#!/usr/bin/env python3
"""spaCy NER extractor with offsets.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model).

Every input DocID appears in the output at least once: a document with entities
contributes one row per entity; a document with none contributes a single row with a
null Start/Stop/Span/Label/LabelRaw (Engine/Model are still set). This lets the
orchestrator record the doc as processed even when nothing was found.

`Engine` is "spacy"; `Model` carries the model name (e.g. en_core_web_sm), so the
output is self-describing with no string-splitting downstream; LexNLP rows carry
"lexnlp" in both columns.

--timeout N is a stall guard (0 = off): each window must complete within N seconds.
nlp.pipe streams batches, so a hung window can't be skipped in-stream; instead, on
the first timeout the pipe is abandoned and every REMAINING window is processed
individually under its own alarm -- timed-out windows are skipped with a stderr note
(DocID + window offset), all other windows complete, and affected docs still appear
in the output (their other rows, or the sentinel). Consequences, by design: after a
timeout event the rest of the run is sequential, and under n_process > 1 a wedged
internal worker may burn one core until process exit. SIGALRM is best-effort around
long native ops. spaCy's runtime is ~linear in window length, so with windowing +
--max-chars a timeout should be a near-impossible event; the guard exists so a
multi-day corpus pass can never hang silently.

--max-chars N truncates every document to its first N characters BEFORE extraction
(0 = off). Truncation preserves the offset contract: the prefix is unchanged, so all
emitted offsets remain valid against the full original text.

Documents still longer than nlp.max_length are processed in WINDOWS: split at
whitespace near the limit with a small overlap, NER per window, entity offsets
shifted back to document-absolute, duplicates from the overlap region deduped on
(Start, Stop, LabelRaw). Normal-length docs take the single-window path unchanged.

Offsets are 0-based, half-open, code-point indices: text[Start:Stop] == Span.
`Label` is the shared cross-engine vocabulary (LABEL_MAP); `LabelRaw` keeps the native
spaCy tag. --label filters on the unified labels. Aligned schema with extract_lexnlp.py.

--device {auto,cpu,cuda,mps}: GPU acceleration via spacy.require_gpu() -- CuPy on CUDA,
Metal/MPS on Apple Silicon. NOTE: on Apple, MPS only accelerates the transformer (trf);
the CNN models (sm/md/lg) stay on CPU. On any GPU, n_process is forced to 1 (a single
device can't be shared across worker processes); use n_process only for CPU models.
"""
import argparse
import os
import signal
import sys
from pathlib import Path

import pandas as pd
import pyarrow.dataset as pads
import spacy
from tqdm import tqdm

LABEL_MAP = {
    "ORG": "ORG",
    "PERSON": "PERSON",
    "GPE": "GPE", "LOC": "GPE",
    "DATE": "DATE",
    "MONEY": "MONEY",
    "PERCENT": "PERCENT",
    "QUANTITY": "AMOUNT",
}
COLUMNS = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]
WINDOW_OVERLAP = 1_000   # chars shared by adjacent windows so boundary entities survive


class _WindowTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _WindowTimeout()


def resolve_inputs(paths):
    """A single file, a folder, or several of each -> a flat list of parquet files."""
    files = []
    for p in paths:
        pth = Path(p)
        files += sorted(str(f) for f in pth.rglob("*.parquet")) if pth.is_dir() else [str(pth)]
    if not files:
        raise SystemExit("no parquet files found in the given path(s)")
    return files


def truncate_texts(texts, max_chars):
    """Cut every text to its first max_chars characters (0/None = off).
    Returns (texts, n_truncated)."""
    if not max_chars or max_chars <= 0:
        return texts, 0
    n_trunc = sum(1 for t in texts if len(t) > max_chars)
    if n_trunc:
        texts = [t[:max_chars] if len(t) > max_chars else t for t in texts]
    return texts, n_trunc


def windows(text, max_len, overlap=WINDOW_OVERLAP):
    """Yield (offset, chunk) covering `text` in pieces of at most max_len chars.

    Short texts yield a single (0, text) -- the unwindowed fast path. Long texts are
    split at the last whitespace before the limit (so tokens aren't cut) when one
    exists in the second half of the window; adjacent windows share `overlap` chars,
    and offsets are window starts in document coordinates.
    """
    n = len(text)
    if n <= max_len:
        yield 0, text
        return
    start = 0
    while start < n:
        end = min(start + max_len, n)
        if end < n:
            ws = text.rfind(" ", start, end)
            if ws > start + max_len // 2:
                end = ws
        yield start, text[start:end]
        if end >= n:
            break
        start = end - overlap


def setup_device(requested):
    """requested: auto|cpu|cuda|mps. Activates the GPU (if any) and returns the active device.

    Must run before any pipeline is loaded. CuPy backs CUDA; Thinc routes the torch
    transformer to MPS on Apple Silicon. Falls back to CPU under 'auto'.
    """
    if requested == "cpu":
        return "cpu"

    # CUDA (CuPy): accelerates both CNN and transformer layers.
    if requested in ("auto", "cuda"):
        try:
            import cupy  # noqa: F401  (only present in a CUDA-enabled install)
            if spacy.require_gpu():
                return "cuda"
        except Exception:
            if requested == "cuda":
                raise SystemExit("CUDA requested but unavailable (install a CUDA build, e.g. pip install 'spacy[cuda12x]').")

    # MPS (Apple Silicon): accelerates the torch transformer only, not the CNN ops.
    if requested in ("auto", "mps"):
        os.environ.setdefault("PYTORCH_ENABLE_MPS_FALLBACK", "1")  # unsupported ops fall back to CPU
        try:
            import torch
            if torch.backends.mps.is_available() and spacy.require_gpu():
                return "mps"
        except Exception:
            if requested == "mps":
                raise SystemExit("MPS requested but unavailable (need torch>=1.13 with MPS on macOS 12.3+).")

    return "cpu"


def load_model(model):
    nlp = spacy.load(model)
    for pipe in ("parser", "lemmatizer", "tagger", "attribute_ruler", "morphologizer"):
        if pipe in nlp.pipe_names:
            nlp.disable_pipe(pipe)
    nlp.max_length = 5_000_000
    return nlp


def iter_docs(nlp, items, batch_size, n_process, timeout, pbar):
    """Yield (doc, ctx) for every item in `items`, guarding each yield with SIGALRM.

    timeout <= 0: plain nlp.pipe. Otherwise: stream through nlp.pipe with an alarm
    around each next(); on the FIRST timeout, abandon the stream (its internal state
    is unrecoverable) and process every remaining item individually under its own
    alarm, yielding (None, ctx) for windows that time out so the caller can skip them.
    nlp.pipe preserves input order, so the resume index is exact.
    """
    if timeout <= 0:
        for doc, ctx in nlp.pipe(items, as_tuples=True, batch_size=batch_size, n_process=n_process):
            pbar.update(1)
            yield doc, ctx
        return

    signal.signal(signal.SIGALRM, _alarm_handler)
    idx = 0
    piped = nlp.pipe(items, as_tuples=True, batch_size=batch_size, n_process=n_process)
    fallback = False

    while idx < len(items):
        if not fallback:
            try:
                signal.alarm(timeout)
                try:
                    doc, ctx = next(piped)
                finally:
                    signal.alarm(0)
            except StopIteration:
                return
            except _WindowTimeout:
                print(f"[timeout] no window finished in {timeout}s -> "
                      f"sequential fallback for the remaining {len(items) - idx} window(s)",
                      file=sys.stderr)
                fallback = True
                continue
            pbar.update(1)
            yield doc, ctx
            idx += 1
        else:
            chunk, ctx = items[idx]
            doc = None
            try:
                signal.alarm(timeout)
                try:
                    doc = nlp(chunk)
                finally:
                    signal.alarm(0)
            except _WindowTimeout:
                docid, off = ctx
                print(f"[timeout] {docid} window@{off}: > {timeout}s, skipped", file=sys.stderr)
            pbar.update(1)
            yield doc, ctx
            idx += 1


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--model", default="en_core_web_sm", help="spaCy model name or path")
    ap.add_argument("--label", nargs="+", default=None,
                    help="unified labels to keep (default: all). E.g. ORG GPE DATE")
    ap.add_argument("--max-chars", type=int, default=0,
                    help="truncate each document to its first N characters (0 = off)")
    ap.add_argument("--timeout", type=int, default=600,
                    help="per-window stall guard in seconds (0 = off)")
    ap.add_argument("--device", default="auto", choices=["auto", "cpu", "cuda", "mps"],
                    help="GPU device for inference (auto tries cuda -> mps -> cpu)")
    ap.add_argument("--batch-size", type=int, default=64)
    ap.add_argument("--n-process", type=int, default=1, help="CPU workers for nlp.pipe (ignored on GPU)")
    ap.add_argument("--no-progress", action="store_true", help="disable the progress bar")
    args = ap.parse_args()

    device = setup_device(args.device)                 # before load_model, per spaCy docs
    n_process = args.n_process
    if device != "cpu" and n_process != 1:
        print(f"[device={device}] forcing n_process=1 (a GPU can't be shared across workers)", file=sys.stderr)
        n_process = 1

    df = (pads.dataset(resolve_inputs(args.inputs), format="parquet")
              .to_table(columns=[args.id_col, args.text_col])
              .to_pandas())
    texts = df[args.text_col].fillna("").astype(str).tolist()
    docids = df[args.id_col].tolist()

    texts, n_trunc = truncate_texts(texts, args.max_chars)
    if n_trunc:
        print(f"{n_trunc} doc(s) truncated to {args.max_chars} chars", file=sys.stderr)

    keep = set(args.label) if args.label else None
    nlp = load_model(args.model)
    engine = "spacy"
    model = Path(args.model).name                      # e.g. en_core_web_sm (path -> base name)

    # One (window_text, (docid, offset)) item per window; normal docs = one window.
    items = []
    n_windowed = 0
    for docid, text in zip(docids, texts):
        wins = list(windows(text, nlp.max_length))
        if len(wins) > 1:
            n_windowed += 1
        items.extend((chunk, (docid, off)) for off, chunk in wins)
    if n_windowed:
        print(f"{n_windowed} doc(s) exceed max_length -> windowed "
              f"(overlap={WINDOW_OVERLAP})", file=sys.stderr)

    pbar = tqdm(total=len(items), unit="win",
                desc=f"spaCy:{args.model} [{device}] (n_process={n_process})",
                file=sys.stderr, disable=args.no_progress)

    # Collect per doc: offsets shifted to document-absolute; overlap dupes dropped.
    # A None doc = timed-out window: record a marker for that doc (re-run later),
    # don't drop the doc silently.
    rows_by_doc = {docid: [] for docid in docids}
    seen = {}
    timed_out = set()
    for doc, (docid, off) in iter_docs(nlp, items, args.batch_size, n_process, args.timeout, pbar):
        if doc is None:
            timed_out.add(docid)
            continue
        doc_rows = rows_by_doc[docid]
        doc_seen = seen.setdefault(docid, set())
        for ent in doc.ents:
            label = LABEL_MAP.get(ent.label_, ent.label_)
            if keep and label not in keep:
                continue
            key = (off + ent.start_char, off + ent.end_char, ent.label_)
            if key in doc_seen:                         # duplicate from the overlap region
                continue
            doc_seen.add(key)
            doc_rows.append((docid, off + ent.start_char, off + ent.end_char,
                             ent.text, label, ent.label_, engine, model))
    pbar.close()

    rows = []
    for docid in docids:
        doc_rows = rows_by_doc[docid]
        rows.extend(doc_rows)                           # real candidates, if any
        if docid in timed_out:                          # window(s) skipped -> marker (any rows or not)
            rows.append((docid, None, None, None, None, "timeout:window", engine, model))
        elif not doc_rows:                              # genuine no-hit -> sentinel
            rows.append((docid, None, None, None, None, None, engine, model))

    out = pd.DataFrame(rows, columns=COLUMNS)
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")   # real nulls, not NaN
    n_cand = int(out["Start"].notna().sum())
    out.to_parquet(args.output, index=False)
    print(f"{len(df)} doc(s) -> {n_cand} candidate(s)  [spacy:{model}, device={device}, n_process={n_process}]")


if __name__ == "__main__":
    main()
