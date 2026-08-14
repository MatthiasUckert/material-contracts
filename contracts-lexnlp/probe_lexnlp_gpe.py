#!/usr/bin/env python3
"""LexNLP GPE probe: the core candidate contract, plus everything GeoAnnotation carries.

WHY THIS EXISTS
extract_lexnlp.py keeps ann.coords and text[start:stop] and writes the constant "geoentity" into
LabelRaw. GeoAnnotation carries eleven further fields, and unlike the company case several of them
are RESOLUTION rather than description: the matched surface form is an alias, and the annotation
says which canonical entity it resolved to. That is a different kind of information from a legal
form -- it is the engine's own answer to "which place is this", and nothing downstream can
reconstruct it from the span alone, because the alias-to-entity mapping lives in the container.

THE OUTPUT CONTRACT
Core, identical to every other extractor, and mandatory:

    DocID  Start  Stop  Span  Label  LabelRaw  Engine  Model

Start and Stop are 0-based half-open code-point offsets, so text[Start:Stop] == Span. LabelRaw is
the engine's own tag, "geoentity", unchanged.

Extras, engine-specific and optional:

    Name            the canonical entity name LexNLP resolved to
    NameEn          the English name, where the canonical one is not English
    Alias           the alias actually matched in the text
    EntityCategory  the entity's own class -- Country, Province, and so on
    Iso2 / Iso3     ISO-3166-2 and ISO-3166-3 codes for the resolved entity
    EntityId        the numeric identifier in the geoentity table
    EntityPriority  the table's own tie-break weight where one alias serves several entities
    Source          provenance recorded by the table
    Year            where the table dates the entity

NOTE ON THE ISO COLUMNS. These are the RESOLVED entity's codes, which is a different thing from the
ISO alias columns the extractor drops at LOCATOR BUILD time. Those were dropped because matching on
them fires "IN" against "IN WITNESS WHEREOF" and "NOR" against "neither ... nor"; recording the code
of an entity matched by NAME carries none of that risk and is the cheap way to normalise a place.

EVERY INPUT DOCUMENT APPEARS IN THE OUTPUT AT LEAST ONCE, sentinel row where nothing was found, so
the ledger can tell "no hit" from "not run".

THIS IS THE EXPENSIVE EXTRACTOR. The geoentity pass scans the alias dictionary over the whole text
and 04A measured four timeouts at 240 seconds on the sample; the per-document cap is honoured here
for the same reason.

Run through the image with the entrypoint overridden:

  docker run --rm -v "$PWD/in":/work:ro -v "$PWD/out":/out -v "$PWD/probe":/probe:ro \
    --entrypoint python contracts-lexnlp /probe/probe_lexnlp_gpe.py /work/sample.parquet \
    --output /out/GPE__lexnlp__lexnlp.parquet --n-process 20 --chunk-size 8 --timeout 240
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

from lexnlp.extract.all_locales.languages import LANG_EN
from lexnlp.extract.common.geoentity_detector import GeoEntityLocator
from lexnlp.config.en import geoentities_config
from lexnlp.extract.en.dict_entities import (DictionaryEntry, DictionaryEntryAlias,
                                             prepare_alias_banlist_dict)

CORE = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]
EXTRA = ["Name", "NameEn", "Alias", "EntityCategory", "Iso2", "Iso3",
         "EntityId", "EntityPriority", "Source", "Year"]
COLUMNS = CORE + EXTRA

ENGINE = "lexnlp"
MODEL = "lexnlp"
LABEL = "GPE"
LABEL_RAW = "geoentity"

GEO_CONFIG_DEFAULT = "/app/geoentities.csv"
GEO_MIN_ALIAS_DEFAULT = 4     # backstop only; the ISO columns are dropped outright

_TIMEOUT = 0
_LOCATOR = None


class _ExtractorTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _ExtractorTimeout()


def _init_worker():
    signal.signal(signal.SIGALRM, _alarm_handler)


def build_geo_locator(geo_config_path, min_alias_len=GEO_MIN_ALIAS_DEFAULT, iso_codes=False):
    """A VERBATIM COPY of extract_lexnlp.py's builder. It must stay verbatim.

    The probe exists to record fields the production extractor discards, which is only meaningful
    if the two find the SAME SPANS. Reconstructing the locator from the library's own API instead of
    copying this function is how the first version of this probe came to import a symbol that does
    not exist in LexNLP 2.3.0 -- and had it imported cleanly, a different min_alias_len or a missing
    ban list would have changed the spans silently instead of failing loudly.

    THE ISO CODE COLUMNS ARE DROPPED FROM THE ALIAS SET, AND A LENGTH THRESHOLD IS NOT ENOUGH.
    The two-letter codes give IN, OR, ME, DE, LA and AL, and "IN" matches "IN WITNESS WHEREOF" on
    essentially every document. Raising min_alias_len to 3 removes those and walks into the
    three-letter codes, which are worse because they match case-insensitively: NOR (Norway) was the
    single most frequent span in the sample at 188 hits, from "neither ... nor", and AND (Andorra)
    took 37. So the fix names the alias columns; min_alias_len stays as a backstop at 4.

    Conflict resolving is left at LexNLP's default, so a position matched by two entities yields
    both. That is the honest behaviour for a candidate store: the duplicate is visible to the
    resolution step rather than silently decided here -- and it is why T7 counts duplicates on
    (DocID, Start, Stop, LabelRaw) rather than asserting one span per offset.
    """
    config = pd.read_csv(geo_config_path)
    alias_columns = [
        DictionaryEntryAlias("Entity Name", LANG_EN.code, False),
        DictionaryEntryAlias("Alias", LANG_EN.code, False),
    ]
    if iso_codes:
        alias_columns += [
            DictionaryEntryAlias("ISO-3166-2", LANG_EN.code, True),
            DictionaryEntryAlias("ISO-3166-3", LANG_EN.code, True),
        ]
    entries = DictionaryEntry.load_entities_from_single_df(
        config, LANG_EN.code, alias_columns=alias_columns)
    ban = prepare_alias_banlist_dict(geoentities_config.ALIAS_BLACK_LIST)
    return GeoEntityLocator(
        LANG_EN.code,
        entries,
        ban,
        text_languages=[LANG_EN.code],   # English aliases only: "Island" is German for Iceland
        min_alias_len=min_alias_len,
    )


def _row(docid, start, stop, span, label_raw, ann=None):
    if ann is None:
        return (docid, start, stop, span, None if span is None else LABEL, label_raw,
                ENGINE, MODEL) + (None,) * len(EXTRA)
    return (
        docid, start, stop, span, LABEL, label_raw, ENGINE, MODEL,
        getattr(ann, "name", None), getattr(ann, "name_en", None), getattr(ann, "alias", None),
        getattr(ann, "entity_category", None), getattr(ann, "iso_3166_2", None),
        getattr(ann, "iso_3166_3", None), getattr(ann, "entity_id", None),
        getattr(ann, "entity_priority", None), getattr(ann, "source", None),
        getattr(ann, "year", None),
    )


def extract_one(args):
    docid, text = args
    if not (isinstance(text, str) and text.strip()):
        return [_row(docid, None, None, None, None)]

    try:
        if _TIMEOUT > 0:
            signal.alarm(_TIMEOUT)
        try:
            anns = list(_LOCATOR.get_geoentity_annotations(text))
        finally:
            if _TIMEOUT > 0:
                signal.alarm(0)
    except _ExtractorTimeout:
        print("[timeout] " + str(docid) + " > " + str(_TIMEOUT) + "s", file=sys.stderr)
        return [_row(docid, None, None, None, "timeout:geoentity")]
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
    ap.add_argument("--geo-config", default=GEO_CONFIG_DEFAULT)
    ap.add_argument("--geo-min-alias-len", type=int, default=GEO_MIN_ALIAS_DEFAULT)
    ap.add_argument("--geo-iso-codes", action="store_true",
                    help="keep the ISO alias columns; see build_geo_locator for why not")
    ap.add_argument("--timeout", type=int, default=240)
    ap.add_argument("--n-process", type=int, default=1)
    ap.add_argument("--chunk-size", type=int, default=8)
    ap.add_argument("--no-progress", action="store_true")
    args = ap.parse_args()

    global _TIMEOUT, _LOCATOR
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

    # Built BEFORE the pool so the workers inherit it copy-on-write rather than each paying to
    # construct a dictionary of tens of thousands of aliases.
    _LOCATOR = build_geo_locator(args.geo_config, args.geo_min_alias_len, args.geo_iso_codes)
    n_alias = sum(len(e.aliases) for e in _LOCATOR.geo_config_list)
    print("geo locator: " + str(len(_LOCATOR.geo_config_list)) + " entities, "
          + str(n_alias) + " aliases, min_alias_len=" + str(args.geo_min_alias_len)
          + ", iso_codes=" + str(args.geo_iso_codes), file=sys.stderr)

    nproc = args.n_process if args.n_process > 0 else (os.cpu_count() or 1)
    desc = "lexnlp GPE (n_process=" + str(nproc) + ")"

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
    Path(args.output).parent.mkdir(parents=True, exist_ok=True)
    out.to_parquet(args.output, index=False)

    n_cand = int(out["Start"].notna().sum())
    print(str(len(df)) + " doc(s) -> " + str(n_cand) + " candidate(s), "
          + str(len(out) - n_cand) + " sentinel/marker row(s)")


if __name__ == "__main__":
    main()
