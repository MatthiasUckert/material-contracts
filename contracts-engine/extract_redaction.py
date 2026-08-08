#!/usr/bin/env python3
"""Redaction-indicator extractor with offsets -- the paper's bracket classes, ported.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model).

Stamps Engine = "paper", Model = MODEL. Label is always "REDACT"; LabelRaw carries the
class, which is what downstream aggregation pivots on.

WHY THIS IS AN EXTRACTOR AND NOT A COUNT
The published redaction analysis counts bracketed indicators per document. That answers
how much was withheld and cannot answer what was withheld, because a count carries no
position. Emitting the indicators as spans with offsets puts them in the same coordinate
system as every other candidate, so "how far is this money span from the nearest
redaction" becomes a window function rather than a new pass over the text.

That matters for two questions. It is the only external evidence money admits: a marker
sits where a commercially material amount used to be, so a cue that precedes "[***]" in
one contract and "$5,000,000" in another is locating the same slot. And it turns "how
many redactions" into "redactions of what" -- adjacent to an amount, to a party name, or
to neither.

CLASSES
Four, following the published classification so the numbers reconcile, plus one addition.

  RedactSymbol   A bracket holding only whitespace and asterisks: [***], [ * * * ], [*].
  RedactExplicit A bracket naming confidential treatment: CONFIDENTIAL, REDACT, CTR.
  OmitExplicit   A bracket recording deletion: INTENTIONALLY, OMITTED, DELETE.
  OmitSymbol     A bracket holding only bullet or ellipsis characters, or three dots.
  RedactBare     An unbracketed run of three or more asterisks. NOT in the published
                 method; separated so it can be measured and then kept or dropped on
                 evidence. Filter it with LabelRaw <> 'RedactBare' if it floods.

Precedence follows the original: a bracket reading "[CONFIDENTIAL PORTION OMITTED]"
classifies as RedactExplicit rather than OmitExplicit, because confidential treatment is
the more specific claim. Brackets matching nothing -- [1.4], [borrower] -- are not
emitted; a label called REDACT should mean redaction.

WHY THE TEXT IS NOT NORMALISED FIRST
The published version collapses whitespace and upper-cases before matching, which is
why "[ * * * ]" reduces to "[***]" and classifies cleanly. Both operations change string
length, so any offset taken afterwards indexes a string that no longer exists. Here the
match is taken against the RAW text and only the matched substring is normalised, for
classification. The bracket pattern therefore has to tolerate interior whitespace and
line breaks itself.

--max-chars N truncates every document to its first N characters BEFORE extraction
(0 = off). --timeout N caps each document (seconds; 0 = off); on timeout the document is
skipped and emits a marker row (LabelRaw = "timeout:redaction", null span).

Offsets are 0-based, half-open, code-point indices: text[Start:Stop] == Span. Aligned
schema/CLI with extract_spacy.py / extract_lexnlp.py / extract_dateregex.py.
"""
import argparse
import os
import re
import signal
import sys
from multiprocessing import Pool
from pathlib import Path

import pandas as pd
import pyarrow.dataset as pads
from tqdm import tqdm

ENGINE = "paper"
MODEL = "redaction-v1"   # identifies THIS class set; bump on any rule change
LABEL = "REDACT"
COLUMNS = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]

# A bracket holding no other bracket, bounded so a stray "[" cannot swallow a paragraph.
# Newlines are allowed inside because the raw text is not re-wrapped before matching and
# "[CONFIDENTIAL TREATMENT\nREQUESTED]" is ordinary in a converted filing.
RX_BRACKET = re.compile(r"\[[^\[\]]{0,80}\]", re.DOTALL)

# Three or more asterisks standing alone. The guards keep it off footnote markers and off
# the interior of a bracket already matched above.
RX_BARE = re.compile(r"(?<![*\w])\*{3,}(?![*\w])")

# Bullet and ellipsis characters used as redaction fill, including the cp1252 strays that
# survive conversion. Compiled as a class so the match is on the characters themselves
# rather than on an escaped rendering of them.
BULLETS = "\u25cf\u2022\u2026\u00b7\u2219\u0095\u0097\u0086\u2010\u2043"
RX_BULLETS_ONLY = re.compile(r"^[" + re.escape(BULLETS) + r"\s]+$")

RX_CONF = re.compile(r"CONFIDENTIAL|REDACT|CTR")
RX_OMIT = re.compile(r"INTENTIONALLY|OMITTED|DELETE")
RX_STARS_ONLY = re.compile(r"^[*\s]+$")
RX_DOTS = re.compile(r"\.{3}")

_TIMEOUT = 0


class _ExtractorTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _ExtractorTimeout()


def _init_worker(timeout):
    """Pool initializer: set the per-document cap and install the SIGALRM handler.

    Every pattern in this module is compiled at import, so a worker started under spawn
    recompiles them rather than inheriting them. Nothing is set in main() and read in a
    worker, which is the failure mode that silently produced zero candidates elsewhere.
    """
    global _TIMEOUT
    signal.signal(signal.SIGALRM, _alarm_handler)
    _TIMEOUT = timeout


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


def classify(span):
    """Which redaction class a bracketed span belongs to, or None if it is not one.

    Classification runs on a normalised COPY of the matched substring -- upper-cased with
    whitespace removed -- so that "[ * * * ]" and "[***]" reach the same verdict while the
    offsets continue to index the untouched original.
    """
    norm = re.sub(r"\s+", "", span).upper()
    inner = norm[1:-1] if len(norm) >= 2 else ""
    if not inner:
        return None
    if RX_CONF.search(inner):
        return "RedactExplicit"
    if RX_STARS_ONLY.match(inner):
        return "RedactSymbol"
    if RX_OMIT.search(inner):
        return "OmitExplicit"
    if RX_BULLETS_ONLY.match(inner) or RX_DOTS.search(inner):
        return "OmitSymbol"
    return None


def find_indicators(text):
    """Every redaction indicator in one text, as (start, stop, span, class).

    Brackets are taken first and their spans recorded, so a bare asterisk run sitting
    inside one is not emitted twice under two different classes.
    """
    out = []
    covered = []
    for m in RX_BRACKET.finditer(text):
        klass = classify(m.group(0))
        covered.append((m.start(), m.end()))
        if klass is not None:
            out.append((m.start(), m.end(), m.group(0), klass))
    for m in RX_BARE.finditer(text):
        if any(a <= m.start() and m.end() <= b for a, b in covered):
            continue
        out.append((m.start(), m.end(), m.group(0), "RedactBare"))
    out.sort(key=lambda r: r[0])
    return out


def extract_one(args):
    """Rows for one document. Always returns >=1 row: a null-span sentinel if nothing is
    found (including blank text). The whole document is capped at _TIMEOUT seconds if set;
    on timeout it emits a marker row so the orchestrator can record the status."""
    docid, text = args
    rows = []
    if isinstance(text, str) and text.strip():
        try:
            if _TIMEOUT > 0:
                signal.alarm(_TIMEOUT)
            try:
                hits = find_indicators(text)
            finally:
                if _TIMEOUT > 0:
                    signal.alarm(0)
        except _ExtractorTimeout:
            print(f"[timeout] {docid}: redaction > {_TIMEOUT}s, skipped", file=sys.stderr)
            return [(docid, None, None, None, None, "timeout:redaction", ENGINE, MODEL)]
        except Exception:                  # one bad document must not kill the run
            hits = []
        rows = [(docid, s, e, sp, LABEL, kl, ENGINE, MODEL) for s, e, sp, kl in hits]
    if not rows:
        rows.append((docid, None, None, None, None, None, ENGINE, MODEL))
    return rows


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--label", nargs="+", default=[LABEL],
                    help=f"unified labels to extract; this engine supports: {LABEL}")
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

    if LABEL not in args.label:
        print(f"no redaction extractor for {args.label}; writing sentinels only", file=sys.stderr)
        rows = [(docid, None, None, None, None, None, ENGINE, MODEL)
                for docid in df[args.id_col].tolist()]
        out = pd.DataFrame(rows, columns=COLUMNS)
        out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
        out.to_parquet(args.output, index=False)
        print(f"{len(df)} doc(s) -> 0 candidate(s)  [{ENGINE}:{MODEL}]")
        return

    texts = df[args.text_col].tolist()
    texts, n_trunc = truncate_texts(texts, args.max_chars)
    if n_trunc:
        print(f"{n_trunc} doc(s) truncated to {args.max_chars} chars", file=sys.stderr)
    items = list(zip(df[args.id_col].tolist(), texts))

    nproc = args.n_process if args.n_process > 0 else (os.cpu_count() or 1)
    desc = f"{ENGINE}:{MODEL} (n_process={nproc})"
    timeout = max(0, args.timeout)

    rows = []
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

    out = pd.DataFrame(rows, columns=COLUMNS)
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
    n_cand = int(out["Start"].notna().sum())
    mix = out["LabelRaw"].value_counts().to_dict()
    out.to_parquet(args.output, index=False)
    print(f"{len(df)} doc(s) -> {n_cand} candidate(s)  [{ENGINE}:{MODEL}]")
    if mix:
        print("  " + "  ".join(f"{k}={v}" for k, v in sorted(mix.items())), file=sys.stderr)


if __name__ == "__main__":
    main()
