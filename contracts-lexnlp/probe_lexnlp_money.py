#!/usr/bin/env python3
"""LexNLP MONEY probe: the core candidate contract, plus the amount and currency it parses.

WHY THIS EXISTS
extract_lexnlp.py keeps ann.coords and text[start:stop] and writes the constant "money" into
LabelRaw. MoneyAnnotation carries two further fields:

    Amount    the parsed figure, a Decimal
    Currency  an ISO code -- LexNLP maps its symbols through CURRENCY_SYMBOL_MAP, so "$" arrives
              as USD and the pound sign as GBP rather than as themselves

THE ISO CODE IS WHY THIS PROBE IS WORTH RUNNING even though this engine finds very little money.
04A measured 2,677 MONEY spans for lexnlp against roughly 46,000 for moneyregex and each spaCy
model -- it is a twentieth of the volume, and the read examples show why: it recognises spelled-out
forms and little else. But it settles the vocabulary. Both rule engines can report USD and EUR
rather than one reporting a dollar sign and the other a word, and that agreement is only checkable
because this engine already speaks it.

THE OUTPUT CONTRACT
Core, identical to every other extractor, and mandatory:

    DocID  Start  Stop  Span  Label  LabelRaw  Engine  Model

Start and Stop are 0-based half-open code-point offsets, so text[Start:Stop] == Span.

Extras:

    Amount    the figure, written as a STRING. A Decimal crossing a parquet seam through pandas
              becomes a float, and a contract value of 9,752,233.001 is exactly the kind of number
              that does not survive that round trip intact. R reads the text and decides its own
              type.
    Currency  ISO 4217 where LexNLP resolved one, null where it did not.

AN AMOUNT CAN BE FOUND AND NOT PARSED, and a currency can be absent while an amount is present --
"payable in the amount of 5,000,000" has a figure and no denomination. Both are emitted as they
come, because a null here is a fact about the text rather than a failure of the extractor.

EVERY INPUT DOCUMENT APPEARS IN THE OUTPUT AT LEAST ONCE, sentinel row where nothing was found.

Run through the image with the entrypoint overridden:

  docker run --rm -v "$PWD/in":/work:ro -v "$PWD/out":/out -v "$PWD/probe":/probe:ro \
    --entrypoint python contracts-lexnlp /probe/probe_lexnlp_money.py /work/sample.parquet \
    --output /out/MONEY__lexnlp__lexnlp.parquet --n-process 20 --chunk-size 8 --timeout 240
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

from lexnlp.extract.en.money import get_money_annotations

CORE = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]
EXTRA = ["Amount", "Currency"]
COLUMNS = CORE + EXTRA

ENGINE = "lexnlp"
MODEL = "lexnlp"
LABEL = "MONEY"
LABEL_RAW = "money"

_TIMEOUT = 0


class _ExtractorTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _ExtractorTimeout()


def _init_worker():
    signal.signal(signal.SIGALRM, _alarm_handler)


def _amount(value):
    """A Decimal as a plain decimal string, or None. Never scientific notation, never a float."""
    if value is None:
        return None
    try:
        return format(value, "f")
    except (ValueError, TypeError):
        return str(value)


def _row(docid, start, stop, span, label_raw, ann=None):
    if ann is None:
        return (docid, start, stop, span, None if span is None else LABEL, label_raw,
                ENGINE, MODEL, None, None)
    return (docid, start, stop, span, LABEL, label_raw, ENGINE, MODEL,
            _amount(getattr(ann, "amount", None)), getattr(ann, "currency", None))


def extract_one(args):
    docid, text = args
    if not (isinstance(text, str) and text.strip()):
        return [_row(docid, None, None, None, None)]

    try:
        if _TIMEOUT > 0:
            signal.alarm(_TIMEOUT)
        try:
            anns = list(get_money_annotations(text))
        finally:
            if _TIMEOUT > 0:
                signal.alarm(0)
    except _ExtractorTimeout:
        print("[timeout] " + str(docid) + " > " + str(_TIMEOUT) + "s", file=sys.stderr)
        return [_row(docid, None, None, None, "timeout:money")]
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
    desc = "lexnlp MONEY (n_process=" + str(nproc) + ")"

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
    out["Amount"] = out["Amount"].astype("string")       # stays text across the seam
    out["Currency"] = out["Currency"].astype("string")
    Path(args.output).parent.mkdir(parents=True, exist_ok=True)
    out.to_parquet(args.output, index=False)

    n_cand = int(out["Start"].notna().sum())
    n_amt = int(out["Amount"].notna().sum())
    print(str(len(df)) + " doc(s) -> " + str(n_cand) + " candidate(s), "
          + str(n_amt) + " with an amount, "
          + str(len(out) - n_cand) + " sentinel/marker row(s)")


if __name__ == "__main__":
    main()
