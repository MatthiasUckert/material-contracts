#!/usr/bin/env python3
"""LexNLP structured-data extractor with offsets.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine).

Maps LexNLP's built-in get_*_annotations extractors into the shared label vocabulary
so output aligns with extract_spacy.py (identical schema). --label selects unified
labels. GPE uses LexNLP's GeoEntityLocator over a baked gazetteer (the LexPredict
geopolitical_divisions.csv), built once per process. PERSON has no annotation API.

All annotation .coords are document-absolute, so text[Start:Stop] == Span.
tqdm bar on stderr; --n-process forks a pool over documents (Linux -> fork, so the
loaded extractors / geo locator are shared copy-on-write).
"""
import argparse
import os
import sys
from multiprocessing import Pool
from pathlib import Path

import pandas as pd
import pyarrow.dataset as pads
from tqdm import tqdm

import warnings
# LexNLP ships sklearn 0.23 pickles loaded under a newer sklearn -> benign version warning.
warnings.filterwarnings("ignore", message="Trying to unpickle estimator")

from lexnlp.extract.en.entities.nltk_maxent import get_company_annotations
from lexnlp.extract.en.dates import get_date_annotations
from lexnlp.extract.en.money import get_money_annotations
from lexnlp.extract.en.amounts import get_amount_annotations
from lexnlp.extract.en.percents import get_percent_annotations
from lexnlp.extract.en.ratios import get_ratio_annotations
from lexnlp.extract.en.durations import get_duration_annotations
from lexnlp.extract.common.geoentity_detector import GeoEntityLocator
from lexnlp.extract.all_locales.languages import LANG_EN
from lexnlp.config.en import geoentities_config
from lexnlp.extract.en.dict_entities import DictionaryEntry, prepare_alias_banlist_dict

COLUMNS = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine"]
GEO_CONFIG_DEFAULT = "/app/geoentities.csv"
# Skip the per-document NLTK text-normalisation in geo matching: much faster on long
# contracts, slightly less robust alias matching -- acceptable for a recall scaffold.
GEO_SIMPLIFIED_NORMALIZATION = True

_SELECTED = []        # [(label, raw, fn)], set in main() before the pool forks
_GEO_LOCATOR = None    # GeoEntityLocator, built once if GPE is selected


def _geo_extractor(text):
    return _GEO_LOCATOR.get_geoentity_annotations(text)


# unified label -> (LabelRaw tag, extractor). PERSON has no offset API in LexNLP.
EXTRACTORS = {
    "ORG":      ("company",   get_company_annotations),
    "DATE":     ("date",      get_date_annotations),
    "MONEY":    ("money",     get_money_annotations),
    "AMOUNT":   ("amount",    get_amount_annotations),
    "PERCENT":  ("percent",   get_percent_annotations),
    "RATIO":    ("ratio",     get_ratio_annotations),
    "DURATION": ("duration",  get_duration_annotations),
    "GPE":      ("geoentity", _geo_extractor),
}


def resolve_inputs(paths):
    """A single file, a folder, or several of each -> a flat list of parquet files."""
    files = []
    for p in paths:
        pth = Path(p)
        files += sorted(str(f) for f in pth.rglob("*.parquet")) if pth.is_dir() else [str(pth)]
    if not files:
        raise SystemExit("no parquet files found in the given path(s)")
    return files


def build_geo_locator(geo_config_path):
    """LexNLP's own GeoEntityLocator, built once (cheap) and reused across documents."""
    if not os.path.exists(geo_config_path):
        raise SystemExit(f"GPE requested but geo config not found: {geo_config_path}")
    geo_cfg = DictionaryEntry.load_entities_from_single_df(pd.read_csv(geo_config_path), "en")
    return GeoEntityLocator(
        LANG_EN.code,
        geo_cfg,
        prepare_alias_banlist_dict(geoentities_config.ALIAS_BLACK_LIST),
        min_alias_len=geoentities_config.MIN_ALIAS_LEN,
        simplified_normalization=GEO_SIMPLIFIED_NORMALIZATION,
    )


def extract_one(args):
    docid, text = args
    if not isinstance(text, str) or not text.strip():
        return []
    n = len(text)
    rows = []
    for label, raw, fn in _SELECTED:
        try:
            anns = list(fn(text))
        except Exception:                       # one bad extractor/doc shouldn't kill the run
            continue
        for ann in anns:
            start, stop = ann.coords
            if 0 <= start < stop <= n:
                rows.append((docid, start, stop, text[start:stop], label, raw, "lexnlp"))
    return rows


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--label", nargs="+", default=["ORG"],
                    help="unified labels to extract; supported: " + ", ".join(EXTRACTORS))
    ap.add_argument("--geo-config", default=GEO_CONFIG_DEFAULT,
                    help="geoentities CSV for GPE (LexPredict single-df format)")
    ap.add_argument("--n-process", type=int, default=1, help="worker processes (<=0 = all cores)")
    ap.add_argument("--chunk-size", type=int, default=8, help="docs per task when parallelising")
    ap.add_argument("--no-progress", action="store_true", help="disable the progress bar")
    args = ap.parse_args()

    global _SELECTED, _GEO_LOCATOR
    _SELECTED = [(lab, *EXTRACTORS[lab]) for lab in args.label if lab in EXTRACTORS]
    if not _SELECTED:
        pd.DataFrame([], columns=COLUMNS).to_parquet(args.output, index=False)
        print(f"no LexNLP extractor for {args.label}; wrote empty output")
        return

    # GPE is dictionary-driven: build the locator once (before the pool forks).
    if any(lab == "GPE" for lab, _, _ in _SELECTED):
        _GEO_LOCATOR = build_geo_locator(args.geo_config)

    nproc = args.n_process if args.n_process > 0 else (os.cpu_count() or 1)
    desc = f"lexnlp {[s[0] for s in _SELECTED]} (n_process={nproc})"

    df = (pads.dataset(resolve_inputs(args.inputs), format="parquet")
              .to_table(columns=[args.id_col, args.text_col])
              .to_pandas())
    items = list(zip(df[args.id_col].tolist(), df[args.text_col].tolist()))

    rows = []
    if nproc == 1:
        for it in tqdm(items, total=len(items), unit="doc", desc=desc,
                       file=sys.stderr, disable=args.no_progress):
            rows.extend(extract_one(it))
    else:
        with Pool(processes=nproc) as pool:
            for r in tqdm(pool.imap(extract_one, items, chunksize=args.chunk_size),
                          total=len(items), unit="doc", desc=desc,
                          file=sys.stderr, disable=args.no_progress):
                rows.extend(r)

    out = pd.DataFrame(rows, columns=COLUMNS)
    out.to_parquet(args.output, index=False)
    print(f"{len(df)} doc(s) -> {len(out)} candidate(s) [lexnlp {[s[0] for s in _SELECTED]}, n_process={nproc}]")


if __name__ == "__main__":
    main()
