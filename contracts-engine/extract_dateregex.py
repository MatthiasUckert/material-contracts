#!/usr/bin/env python3
"""Regex date extractor with offsets -- the paper's eight patterns, ported.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model).

Ports the original paper pipeline's date regexes (03-NamedEntities.R,
get_date_regex) into the harmonised candidate schema: Engine = "paper",
Model = MODEL (a constant tied to this pattern set -- revising the patterns
means bumping it), Label = "DATE", LabelRaw = the pattern name (ISO, Slash,
Text, ...). Differences vs the paper, by design: runs on TextRaw (not the
uppercased/de-punctuated TextMod), so offsets index the canonical text and the
punctuation-bearing patterns (European, Text) behave as written;
case-insensitive throughout; emits offset spans instead of a count table;
normalisation/cleaning is NOT done here (adjudicator's job downstream).

Matching is per pattern (for LabelRaw provenance); overlapping spans are
resolved by LONGEST SPAN WINS, pattern-table order breaking ties (so Text
beats MonthYear where both fire on the same text).

Every input DocID appears in the output at least once: a doc with matches
contributes one row per kept match; a doc with none contributes a single
null-span sentinel row (Engine/Model still set), so the orchestrator can record
the doc as processed.

--max-chars N truncates every document to its first N characters BEFORE
extraction (0 = off). Offsets are 0-based, half-open, code-point indices:
text[Start:Stop] == Span. Aligned schema/CLI with extract_spacy.py /
extract_lexnlp.py.
"""
import argparse
import os
import signal
import re
import sys
from multiprocessing import Pool
from pathlib import Path

import pandas as pd
import pyarrow.dataset as pads
from tqdm import tqdm

ENGINE = "paper"
MODEL = "dateregex-v1"   # identifies THIS pattern set; bump on any pattern change
COLUMNS = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]

MONTHS = [
    "January", "February", "March", "April", "May", "June", "July", "August",
    "September", "October", "November", "December",
    "Jan", "Feb", "Mar", "Apr", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
]
_M = "|".join(MONTHS)

# The paper's eight patterns (03-NamedEntities.R, get_date_regex), in priority
# order: ties on equal-length overlaps go to the earlier entry.
PATTERNS = [
    ("ISO",        r"\b\d{4}-\d{2}-\d{2}\b"),
    ("ISOShort",   r"\b\d{2}-\d{2}-\d{4}\b"),
    ("Slash",      r"\b\d{1,2}/\d{1,2}/\d{4}\b"),
    ("SlashShort", r"\b\d{1,2}/\d{1,2}/\d{2}\b"),
    ("European",   r"\b\d{1,2}\.\d{1,2}\.\d{4}\b"),
    ("Text",       r"\b(?:" + _M + r")\s+\d{1,2}(?:st|nd|rd|th)?(?:[,\s]+|\s+)\d{4}\b"),
    ("YearFirst",  r"\b\d{4}/\d{1,2}/\d{1,2}\b"),
    ("MonthYear",  r"\b(?:" + _M + r")\s+\d{4}\b"),
]
COMPILED = [(name, re.compile(pat, re.IGNORECASE)) for name, pat in PATTERNS]


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
    Returns (texts, n_truncated). Non-strings pass through untouched."""
    if not max_chars or max_chars <= 0:
        return texts, 0
    n_trunc = sum(1 for t in texts if isinstance(t, str) and len(t) > max_chars)
    if n_trunc:
        texts = [t[:max_chars] if isinstance(t, str) and len(t) > max_chars else t
                 for t in texts]
    return texts, n_trunc


_TIMEOUT = 0          # per-document seconds, set in each worker by _init_worker


class _ExtractorTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _ExtractorTimeout()


def _init_worker(timeout):
    """Pool initializer: set the per-document cap and install the SIGALRM handler.

    Every pattern is compiled at import, so a worker started under spawn recompiles them rather
    than inheriting them. Nothing is set in main() and read in a worker.
    """
    global _TIMEOUT
    signal.signal(signal.SIGALRM, _alarm_handler)
    _TIMEOUT = timeout


def extract_one(args):
    """Rows for one document: per-pattern matches, overlaps resolved by longest
    span first (pattern order breaking ties), output sorted by Start. Always
    returns >=1 row: a null-span sentinel if nothing matched.

    Takes a (docid, text) TUPLE rather than two arguments, because imap_unordered passes one
    item per call. The signature matches extract_moneyregex.py and extract_redaction.py.

    A document that exceeds the cap emits a timeout marker rather than raising, so one
    pathological document cannot end the pass. The greedy overlap resolution is quadratic in
    candidate count in the worst case, which is where the cap earns its place: a table of dates
    can produce tens of thousands of candidates in a single document.
    """
    docid, text = args
    rows = []
    if isinstance(text, str) and text.strip():
      try:
        if _TIMEOUT > 0:
            signal.alarm(_TIMEOUT)
        cands = []
        for prio, (name, rx) in enumerate(COMPILED):
            for m in rx.finditer(text):
                cands.append((m.start(), m.end(), name, prio))
        # longest first, then pattern priority, then position; greedy keep
        cands.sort(key=lambda c: (-(c[1] - c[0]), c[3], c[0]))
        kept = []
        for s, e, name, _ in cands:
            if all(e <= ks or s >= ke for ks, ke, _ in kept):
                kept.append((s, e, name))
        kept.sort()
        rows = [(docid, s, e, text[s:e], "DATE", name, ENGINE, MODEL)
                for s, e, name in kept]
      except _ExtractorTimeout:
        print(f"[timeout] {docid}: > {_TIMEOUT}s, skipped", file=sys.stderr)
        return [(docid, None, None, None, None, "timeout:dateregex", ENGINE, MODEL)]
      finally:
        if _TIMEOUT > 0:
            signal.alarm(0)
    if not rows:
        rows.append((docid, None, None, None, None, None, ENGINE, MODEL))
    return rows


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--label", nargs="+", default=["DATE"],
                    help="unified labels to extract; this engine supports: DATE")
    ap.add_argument("--max-chars", type=int, default=0,
                    help="truncate each document to its first N characters (0 = off)")
    ap.add_argument("--timeout", type=int, default=0,
                    help="per-document cap in seconds (0 = off)")
    ap.add_argument("--n-process", type=int, default=1, help="worker processes (<=0 = all cores)")
    ap.add_argument("--chunk-size", type=int, default=64, help="docs per task when parallelising")
    ap.add_argument("--no-progress", action="store_true", help="disable the progress bar")
    args = ap.parse_args()

    df = (pads.dataset(resolve_inputs(args.inputs), format="parquet")
              .to_table(columns=[args.id_col, args.text_col])
              .to_pandas())
    texts = df[args.text_col].tolist()
    texts, n_trunc = truncate_texts(texts, args.max_chars)
    if n_trunc:
        print(f"{n_trunc} doc(s) truncated to {args.max_chars} chars", file=sys.stderr)

    rows = []
    if "DATE" in args.label:
        items = list(zip(df[args.id_col].tolist(), texts))
        nproc = args.n_process if args.n_process > 0 else (os.cpu_count() or 1)
        desc = f"{ENGINE}:{MODEL} (n_process={nproc})"
        timeout = max(0, args.timeout)

        if nproc == 1:
            _init_worker(timeout)
            for it in tqdm(items, total=len(items), unit="doc", desc=desc,
                           file=sys.stderr, disable=args.no_progress):
                rows.extend(extract_one(it))
        else:
            with Pool(processes=nproc, initializer=_init_worker, initargs=(timeout,)) as pool:
                for r in tqdm(pool.imap_unordered(extract_one, items, chunksize=args.chunk_size),
                              total=len(items), unit="doc", desc=desc,
                              file=sys.stderr, disable=args.no_progress):
                    rows.extend(r)
    else:
        print(f"no date extractor for {args.label}; writing sentinels only", file=sys.stderr)
        rows = [(docid, None, None, None, None, None, ENGINE, MODEL)
                for docid in df[args.id_col].tolist()]

    out = pd.DataFrame(rows, columns=COLUMNS)
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
    n_cand = int(out["Start"].notna().sum())
    out.to_parquet(args.output, index=False)
    print(f"{len(df)} doc(s) -> {n_cand} candidate(s)  [{ENGINE}:{MODEL}]")


if __name__ == "__main__":
    main()
