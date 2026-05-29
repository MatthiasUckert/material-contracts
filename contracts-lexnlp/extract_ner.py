#!/usr/bin/env python3
"""LexNLP candidate-entity extractor.

Input : a parquet file OR a directory of parquet files (one row = one document),
        with an id column and a text column.
Output: parquet with columns (DocID, Label, Entity).
"""
import argparse
import pandas as pd
import pyarrow.dataset as pads
from lexnlp.extract.en.entities.nltk_maxent import (
    get_companies, get_persons, get_geopolitical,
)

EXTRACTORS = {"ORG": get_companies, "PERSON": get_persons, "GPE": get_geopolitical}

def extract_one(docid, text, labels, max_words):
    if not isinstance(text, str) or not text.strip():
        return [{"DocID": docid, "Label": "", "Entity": ""}]
    if max_words:
        text = " ".join(text.split()[:max_words])
    rows = []
    for label, fn in EXTRACTORS.items():
        if labels and label not in labels:
            continue
        src = text.title() if label == "ORG" else text   # title-case helps MaxEnt catch ALL-CAPS orgs
        for item in fn(src):
            entity = " ".join(map(str, item)) if isinstance(item, (list, tuple)) else str(item)
            rows.append({"DocID": docid, "Label": label, "Entity": entity})
    return rows or [{"DocID": docid, "Label": "", "Entity": ""}]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("input_path", help="parquet file or directory of parquet files")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--id-col", default="DocID", help="column holding the document id")
    ap.add_argument("--text-col", default="TextRaw", help="column holding the document text")
    ap.add_argument("--label", nargs="+", default=None, help="subset of ORG PERSON GPE")
    ap.add_argument("--max-words", type=int, default=250)
    args = ap.parse_args()

    df = (pads.dataset(args.input_path, format="parquet")
              .to_table(columns=[args.id_col, args.text_col])
              .to_pandas())

    labels = set(args.label) if args.label else None
    rows = []
    for docid, text in zip(df[args.id_col], df[args.text_col]):
        rows.extend(extract_one(docid, text, labels, args.max_words))

    out = pd.DataFrame(rows, columns=["DocID", "Label", "Entity"])
    out.to_parquet(args.output, index=False)
    print(f"Wrote {len(out)} rows from {len(df)} document(s) -> {args.output}")

if __name__ == "__main__":
    main()
