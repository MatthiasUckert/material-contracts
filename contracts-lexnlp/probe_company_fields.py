#!/usr/bin/env python3
"""Probe: what does LexNLP's company extractor actually carry, beyond the span?

WHY THIS EXISTS
extract_lexnlp.py keeps ann.coords and text[start:stop] and discards everything else. The
annotation object carries six further fields, and two of them look like they would replace work the
R side is currently doing by hand:

  name               the company name with the legal form already removed
  name_abbr          the parenthesised abbreviation, e.g. "(NGM)"
  company_type_full  the surface form matched in the text -- "Corporation", "Inc."
  company_type_abbr  the normalised abbreviation -- "Corp", "LLC", "LP"
  company_type_label the legal category -- Corporation, Company, Partnership, National Association
  description        one of: Trust Bank, Trust Company, Trust, Bank, Company, Partnership, Agency

THE COORDS ARE NOT THE NAME. LexNLP matches inside a noun phrase and its coords cover the whole
regex match, which includes a leading article group -- literally "by and between|by and among|
among|between|with|the|and|to|by|an|a|all" -- and a trailing delimiter. `name` is what survives
after LexNLP has stripped the company type, the Borrower / <X> Agent false positives, leading and
trailing "and|&|of", and numeric or date prefixes. So `name` is a cleaner object than the span, and
this probe exists to find out by how much rather than to assume it.

THIS SCRIPT DECIDES NOTHING and writes one parquet in its own directory. It does not touch the
store, the manifest or the production extractor.

Run through the image with the entrypoint overridden:

  docker run --rm -v "$PWD/in":/work:ro -v "$PWD/out":/out -v "$PWD/probe":/probe:ro \
    --entrypoint python contracts-lexnlp /probe/probe_company_fields.py /work/sample.parquet \
    --output /out/company_fields.parquet --n-process 20 --chunk-size 8 --timeout 240
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

# One row per annotation. UpperDoc and UpperSent are not annotation fields -- they are the two
# guards inside LexNLP that can silence a document entirely, recorded per document so the R side can
# size them without reading the LexNLP source.
COLUMNS = ["DocID", "Start", "Stop", "SpanText", "Name", "NameAbbr",
           "TypeFull", "TypeAbbr", "TypeLabel", "Description"]

DOC_COLUMNS = ["DocID", "DocLen", "NAnn", "UpperDoc", "UpperHead", "HasTypeToken"]

_TIMEOUT = 0


class _ExtractorTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _ExtractorTimeout()


def _init_worker():
    signal.signal(signal.SIGALRM, _alarm_handler)


def probe_one(args):
    """Annotations plus the two document-level guards, for one document.

    THE GUARDS ARE THE POINT OF THE SECOND RETURN VALUE. CompanyDetector.get_company_annotations()
    returns immediately when the whole document is uppercase, and skips any sentence that is
    uppercase, because an all-caps string defeats the proper-noun grammar the noun-phrase extractor
    depends on. It also returns immediately when no company type occurs anywhere in the text. All
    three are silent: the document simply produces nothing, and no marker distinguishes that from a
    document containing no companies.

    UpperHead is measured on the first 3,000 characters rather than the whole document, because that
    is the region the party rules read, and an all-caps preamble under a mixed-case body is invisible
    to the document-level guard while still costing every party in it.
    """
    docid, text = args
    rows = []
    if not (isinstance(text, str) and text.strip()):
        return rows, (docid, 0, 0, False, False, False)

    head = text[:3000]
    doc_row = (
        docid,
        len(text),
        0,
        text == text.upper(),
        head == head.upper(),
        False,
    )

    try:
        if _TIMEOUT > 0:
            signal.alarm(_TIMEOUT)
        try:
            anns = list(get_company_annotations(text))
        finally:
            if _TIMEOUT > 0:
                signal.alarm(0)
    except _ExtractorTimeout:
        print(f"[timeout] {docid} > {_TIMEOUT}s, skipped", file=sys.stderr)
        return rows, doc_row
    except Exception as exc:                      # one bad document must not kill the probe
        print(f"[error] {docid}: {type(exc).__name__}", file=sys.stderr)
        return rows, doc_row

    n = len(text)
    for ann in anns:
        start, stop = ann.coords
        if 0 <= start < stop <= n:
            rows.append((
                docid, start, stop, text[start:stop],
                ann.name, ann.name_abbr,
                ann.company_type_full, ann.company_type_abbr, ann.company_type_label,
                ann.description,
            ))

    doc_row = (doc_row[0], doc_row[1], len(rows), doc_row[3], doc_row[4], doc_row[5])
    return rows, doc_row


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True, help="output parquet path (annotations)")
    ap.add_argument("--output-docs", default=None,
                    help="output parquet path (per-document guards); defaults beside --output")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--timeout", type=int, default=240,
                    help="per-document cap in seconds (0 = off)")
    ap.add_argument("--n-process", type=int, default=1, help="worker processes (<=0 = all cores)")
    ap.add_argument("--chunk-size", type=int, default=8, help="docs per task when parallelising")
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
    desc = f"lexnlp company fields (n_process={nproc})"

    rows, doc_rows = [], []
    if nproc == 1:
        _init_worker()
        for it in tqdm(items, total=len(items), unit="doc", desc=desc,
                       file=sys.stderr, disable=args.no_progress):
            r, d = probe_one(it)
            rows.extend(r)
            doc_rows.append(d)
    else:
        with Pool(processes=nproc, initializer=_init_worker) as pool:
            for r, d in tqdm(pool.imap_unordered(probe_one, items, chunksize=args.chunk_size),
                             total=len(items), unit="doc", desc=desc,
                             file=sys.stderr, disable=args.no_progress):
                rows.extend(r)
                doc_rows.append(d)

    out = pd.DataFrame(rows, columns=COLUMNS)
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
    out.to_parquet(args.output, index=False)

    path_docs = args.output_docs or str(Path(args.output).with_name("company_docs.parquet"))
    pd.DataFrame(doc_rows, columns=DOC_COLUMNS).to_parquet(path_docs, index=False)

    n_upper = sum(1 for d in doc_rows if d[3])
    n_uhead = sum(1 for d in doc_rows if d[4])
    print(f"{len(df)} doc(s) -> {len(out)} annotation(s); "
          f"{n_upper} all-uppercase document(s), {n_uhead} all-uppercase head(s)")


if __name__ == "__main__":
    main()
