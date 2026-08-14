#!/usr/bin/env python3
"""LexNLP ORG probe: the core candidate contract, plus everything the annotation carries.

WHY THIS EXISTS
extract_lexnlp.py keeps ann.coords and text[start:stop] and discards the rest of the annotation.
CompanyAnnotation carries six further fields, and two of them are things the R side is currently
reconstructing by hand. This probe emits the SAME CORE every extractor emits, plus those fields as
extra columns, so the two can be compared row for row and the extras inspected on disk.

THE OUTPUT CONTRACT
Core, identical to extract_spacy.py and extract_lexnlp.py, and mandatory:

    DocID     the document identifier, as given
    Start     0-based, half-open, code-point offset
    Stop      likewise; text[Start:Stop] == Span
    Span      the matched text, verbatim
    Label     the shared cross-engine vocabulary -- "ORG" here
    LabelRaw  the engine's OWN label for the row -- "company" here, unchanged
    Engine    "lexnlp"
    Model     "lexnlp"

Extras, optional and engine-specific. An extractor emits what it has; the R orchestrator decides
which of them the target table accepts:

    Name         the company name with the legal form removed
    NameAbbr     the parenthesised abbreviation, e.g. "(NGM)"
    TypeFull     the surface form matched in the text -- "Corporation", "Inc."
    TypeAbbr     the NORMALISED abbreviation -- CORP, LLC, NA, LP
    TypeLabel    the legal category -- Corporation, Company, Partnership, National Association
    Description  Trust Bank, Trust Company, Trust, Bank, Company, Partnership, Agency

CASING IS NOT NORMALISED ON Description. LexNLP normalises the TYPE through company_types.csv but
passes the description through as matched surface text, so "Bank" and "BANK" both occur. That is
left as found here and normalised on the R side, so the parquet stays a faithful record of what the
engine said.

EVERY INPUT DOCUMENT APPEARS IN THE OUTPUT AT LEAST ONCE. A document with no companies contributes
one row with null Start/Stop/Span/Label/LabelRaw and Engine/Model still set. That sentinel is what
lets the orchestrator record the document as processed rather than as never attempted, and it is
the difference between "no hit" and "not run" in the ledger.

Run through the image with the entrypoint overridden:

  docker run --rm -v "$PWD/in":/work:ro -v "$PWD/out":/out -v "$PWD/probe":/probe:ro \
    --entrypoint python contracts-lexnlp /probe/probe_lexnlp_org.py /work/sample.parquet \
    --output /out/ORG__lexnlp__lexnlp.parquet --n-process 20 --chunk-size 8 --timeout 240
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

from lexnlp.extract.en.entities.nltk_maxent import get_company_annotations

CORE = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]
EXTRA = ["Name", "NameAbbr", "TypeFull", "TypeAbbr", "TypeLabel", "Description"]
COLUMNS = CORE + EXTRA

ENGINE = "lexnlp"
MODEL = "lexnlp"
LABEL = "ORG"
LABEL_RAW = "company"

_TIMEOUT = 0


class _ExtractorTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _ExtractorTimeout()


def _init_worker():
    signal.signal(signal.SIGALRM, _alarm_handler)


def _row(docid, start, stop, span, label_raw, ann=None):
    """One output row, core first then extras, with extras null on sentinel and timeout rows."""
    if ann is None:
        return (docid, start, stop, span, None if span is None else LABEL, label_raw,
                ENGINE, MODEL) + (None,) * len(EXTRA)
    return (docid, start, stop, span, LABEL, label_raw, ENGINE, MODEL,
            ann.name, ann.name_abbr, ann.company_type_full, ann.company_type_abbr,
            ann.company_type_label, ann.description)


def extract_one(args):
    """Annotations for one document, or a sentinel, or a timeout marker. Never nothing."""
    docid, text = args
    if not (isinstance(text, str) and text.strip()):
        return [_row(docid, None, None, None, None)]

    try:
        if _TIMEOUT > 0:
            signal.alarm(_TIMEOUT)
        try:
            anns = list(get_company_annotations(text))
        finally:
            if _TIMEOUT > 0:
                signal.alarm(0)
    except _ExtractorTimeout:
        print("[timeout] " + str(docid) + " > " + str(_TIMEOUT) + "s", file=sys.stderr)
        return [_row(docid, None, None, None, "timeout:document")]
    except Exception as exc:
        print("[error] " + str(docid) + ": " + type(exc).__name__, file=sys.stderr)
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
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True)
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--timeout", type=int, default=240, help="per-document cap (0 = off)")
    ap.add_argument("--n-process", type=int, default=1, help="workers (<=0 = all cores)")
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
    desc = "lexnlp ORG (n_process=" + str(nproc) + ")"

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
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")   # real nulls, not NaN
    Path(args.output).parent.mkdir(parents=True, exist_ok=True)
    out.to_parquet(args.output, index=False)

    n_cand = int(out["Start"].notna().sum())
    print(str(len(df)) + " doc(s) -> " + str(n_cand) + " candidate(s), "
          + str(len(out) - n_cand) + " sentinel/marker row(s)")


if __name__ == "__main__":
    main()
