#!/usr/bin/env python3
"""LexNLP DATE probe: the core candidate contract, plus the parsed date the extractor throws away.

WHY THIS EXISTS, AND WHY IT MATTERS MORE HERE THAN ANYWHERE ELSE
extract_lexnlp.py keeps ann.coords and text[start:stop] and writes the constant "date" into
LabelRaw. DateAnnotation carries two further fields and one of them is the whole point of the
extractor:

    DateValue  the PARSED date -- a real calendar date, not a string
    Score      LexNLP's own confidence in that parse

Every other engine in this family emits a string that something downstream has to parse. LexNLP has
already done it, correctly handling "the 9th day of January, 2014" and "March 3, 2011" alike, and
the result is discarded at the seam. Nothing downstream can recover it in general: parsing
"1/2/2020" needs a convention, and parsing "the ninth day of January" needs a grammar.

THIS IS THE FIELD THE EXPIRY PROBLEM NEEDS. 04B reports expiry cues firing thousands of times while
04C yields about twenty documents. A start date and an end date are the same LABEL and different
ROLES, and a role cannot be assigned from a string without knowing what date it denotes. With a
parsed value the question becomes arithmetic: does this date precede the filing date or follow it,
and by how long.

THE OUTPUT CONTRACT
Core, identical to every other extractor, and mandatory:

    DocID  Start  Stop  Span  Label  LabelRaw  Engine  Model

Start and Stop are 0-based half-open code-point offsets, so text[Start:Stop] == Span. LabelRaw is
the engine's own tag, "date", unchanged.

Extras:

    DateValue  ISO-8601 date as a STRING, "YYYY-MM-DD". Written as text on purpose: a date crossing
               a parquet seam through pandas acquires a timezone and a NaT, and neither survives
               into R as the thing that went in. A ten-character string is unambiguous in both
               languages and R parses it with one format.
    Score      float, LexNLP's confidence. Reported rather than filtered on here; whether it earns
               a threshold is a question for the check script, not for the extractor.

A DATE CAN BE FOUND AND NOT PARSED. Where LexNLP locates a date expression but cannot resolve it to
a calendar date, DateValue is null while the span is still emitted. That is a third state beyond
"found" and "not found", and it is left visible rather than dropped.

EVERY INPUT DOCUMENT APPEARS IN THE OUTPUT AT LEAST ONCE, sentinel row where nothing was found.

Run through the image with the entrypoint overridden:

  docker run --rm -v "$PWD/in":/work:ro -v "$PWD/out":/out -v "$PWD/probe":/probe:ro \
    --entrypoint python contracts-lexnlp /probe/probe_lexnlp_date.py /work/sample.parquet \
    --output /out/DATE__lexnlp__lexnlp.parquet --n-process 20 --chunk-size 8 --timeout 240
"""
import argparse
import os
import signal
import sys
from multiprocessing import Pool
from pathlib import Path

import pandas as pd
import pyarrow.dataset as pads
from tqdm import tqdm

import warnings
warnings.filterwarnings("ignore", message="Trying to unpickle estimator")

from lexnlp.extract.en.dates import get_date_annotations

CORE = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]
EXTRA = ["DateValue", "Score"]
COLUMNS = CORE + EXTRA

ENGINE = "lexnlp"
MODEL = "lexnlp"
LABEL = "DATE"
LABEL_RAW = "date"

_TIMEOUT = 0


class _ExtractorTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _ExtractorTimeout()


def _init_worker():
    signal.signal(signal.SIGALRM, _alarm_handler)


def _iso(value):
    """A date as YYYY-MM-DD, or None. Never a datetime, never a timestamp, never a NaT."""
    if value is None:
        return None
    try:
        return value.isoformat()[:10]
    except AttributeError:
        return str(value)[:10] or None


def _row(docid, start, stop, span, label_raw, ann=None):
    if ann is None:
        return (docid, start, stop, span, None if span is None else LABEL, label_raw,
                ENGINE, MODEL, None, None)
    return (docid, start, stop, span, LABEL, label_raw, ENGINE, MODEL,
            _iso(getattr(ann, "date", None)), getattr(ann, "score", None))


def extract_one(args):
    docid, text = args
    if not (isinstance(text, str) and text.strip()):
        return [_row(docid, None, None, None, None)]

    try:
        if _TIMEOUT > 0:
            signal.alarm(_TIMEOUT)
        try:
            anns = list(get_date_annotations(text))
        finally:
            if _TIMEOUT > 0:
                signal.alarm(0)
    except _ExtractorTimeout:
        print("[timeout] " + str(docid) + " > " + str(_TIMEOUT) + "s", file=sys.stderr)
        return [_row(docid, None, None, None, "timeout:date")]
    except Exception as exc:
        print("[error] " + str(docid) + ": " + type(exc).__name__ + " " + str(exc), file=sys.stderr)
        return [_row(docid, None, None, None, "error:" + type(exc).__name__)]

    n = len(text)
    rows = []
    for ann in anns:
        start, stop = ann.coords
        if 0 <= start < stop <= n:
            rows.append(_row(docid, start, stop, text[start:stop], LABEL_RAW, ann))

    return rows if rows else [_row(docid, None, None, None, None)]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+")
    ap.add_argument("--output", required=True)
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--timeout", type=int, default=240)
    ap.add_argument("--n-process", type=int, default=1)
    ap.add_argument("--chunk-size", type=int, default=8)
    ap.add_argument("--no-progress", action="store_true")
    args = ap.parse_args()

    global _TIMEOUT
    _TIMEOUT = max(0, args.timeout)

    files = []
    for p in args.inputs:
        pth = Path(p)
        files += sorted(str(f) for f in pth.rglob("*.parquet")) if pth.is_dir() else [str(pth)]
    if not files:
        raise SystemExit("no parquet files found in the given path(s)")

    df = (pads.dataset(files, format="parquet")
              .to_table(columns=[args.id_col, args.text_col])
              .to_pandas())
    items = list(zip(df[args.id_col].tolist(), df[args.text_col].tolist()))

    nproc = args.n_process if args.n_process > 0 else (os.cpu_count() or 1)
    desc = "lexnlp DATE (n_process=" + str(nproc) + ")"

    rows = []
    if nproc == 1:
        _init_worker()
        for it in tqdm(items, total=len(items), unit="doc", desc=desc,
                       file=sys.stderr, disable=args.no_progress):
            rows.extend(extract_one(it))
    else:
        with Pool(processes=nproc, initializer=_init_worker) as pool:
            for r in tqdm(pool.imap_unordered(extract_one, items, chunksize=args.chunk_size),
                          total=len(items), unit="doc", desc=desc,
                          file=sys.stderr, disable=args.no_progress):
                rows.extend(r)

    out = pd.DataFrame(rows, columns=COLUMNS)
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
    out["DateValue"] = out["DateValue"].astype("string")     # stays text across the seam
    Path(args.output).parent.mkdir(parents=True, exist_ok=True)
    out.to_parquet(args.output, index=False)

    n_cand = int(out["Start"].notna().sum())
    n_parsed = int(out["DateValue"].notna().sum())
    print(str(len(df)) + " doc(s) -> " + str(n_cand) + " candidate(s), "
          + str(n_parsed) + " with a parsed date, "
          + str(len(out) - n_cand) + " sentinel/marker row(s)")


if __name__ == "__main__":
    main()
