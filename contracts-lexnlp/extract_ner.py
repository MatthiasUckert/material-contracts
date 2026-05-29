#!/usr/bin/env python3
"""LexNLP candidate-entity extractor: text file(s) -> parquet (DocID, Label, Entity)."""
import argparse, os
import pandas as pd
from lexnlp.extract.en.entities.nltk_maxent import (
    get_companies, get_persons, get_geopolitical,
)

EXTRACTORS = {"ORG": get_companies, "PERSON": get_persons, "GPE": get_geopolitical}

def extract_one(path, labels, max_words):
    with open(path, encoding="utf-8", errors="ignore") as f:
        text = f.read()
    if max_words:
        text = " ".join(text.split()[:max_words])
    docid = os.path.splitext(os.path.basename(path))[0]
    rows = []
    for label, fn in EXTRACTORS.items():
        if labels and label not in labels:
            continue
        src = text.title() if label == "ORG" else text   # title-case catches ALL-CAPS orgs
        for item in fn(src):
            entity = " ".join(map(str, item)) if isinstance(item, (list, tuple)) else str(item)
            rows.append({"DocID": docid, "Label": label, "Entity": entity})
    return rows or [{"DocID": docid, "Label": "", "Entity": ""}]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("input_path", help="text file or directory of .txt files")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--label", nargs="+", default=None, help="subset of ORG PERSON GPE")
    ap.add_argument("--max-words", type=int, default=250)
    args = ap.parse_args()

    if os.path.isdir(args.input_path):
        paths = [os.path.join(args.input_path, f)
                 for f in os.listdir(args.input_path) if f.lower().endswith(".txt")]
    else:
        paths = [args.input_path]

    rows = []
    for p in paths:
        rows.extend(extract_one(p, set(args.label) if args.label else None, args.max_words))
    pd.DataFrame(rows, columns=["DocID", "Label", "Entity"]).to_parquet(args.output, index=False)
    print(f"Wrote {len(rows)} rows from {len(paths)} file(s) -> {args.output}")

if __name__ == "__main__":
    main()
