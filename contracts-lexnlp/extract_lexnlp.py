#!/usr/bin/env python3
"""LexNLP structured-data extractor with offsets.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model).

Every input DocID appears in the output at least once: a document with matches
contributes one row per annotation; a document with none (including blank text)
contributes a single row with a null Start/Stop/Span/Label/LabelRaw (Engine/Model are
still set). This lets the orchestrator record the doc as processed even when nothing
matched.

`Engine` and `Model` are both "lexnlp" (LexNLP has no model variants), keeping the
schema aligned with extract_spacy.py with no string-splitting downstream.

--timeout N caps every extractor on every document at N seconds (SIGALRM; 0 = off).
LexNLP's maxent NER / date grammar have catastrophic-runtime corners on pathological
documents; on timeout the extractor is skipped for that doc (stderr note with the
DocID), the doc keeps its other rows or falls back to the sentinel, and the run
continues -- the doc is still recorded as processed.

--max-chars N truncates every document to its first N characters BEFORE extraction
(0 = off, the default). Truncation preserves the offset contract: the prefix is
unchanged, so all emitted offsets remain valid against the full original text.

Maps LexNLP's built-in get_*_annotations extractors into the shared label vocabulary
so output aligns with extract_spacy.py (identical schema). --label selects unified
labels. GPE is ENABLED but expensive (see build_geo_locator); geography candidates are
    LexNLP geoentities, the gazetteer and spaCy:
the EXTRACTORS entry and --geo-config remain, but build_geo_locator raises until it
is re-implemented. PERSON has no annotation API.

All annotation .coords are document-absolute, so text[Start:Stop] == Span.
tqdm bar on stderr; --n-process forks a pool over documents (Linux -> fork, so the
loaded extractors / geo locator are shared copy-on-write); results are consumed
unordered so one slow document doesn't freeze the progress bar.
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
# LexNLP ships sklearn 0.23 pickles loaded under a newer sklearn -> benign version warning.
warnings.filterwarnings("ignore", message="Trying to unpickle estimator")

from lexnlp.extract.all_locales.languages import LANG_EN
from lexnlp.extract.common.geoentity_detector import GeoEntityLocator
from lexnlp.config.en import geoentities_config
from lexnlp.extract.en.dict_entities import (DictionaryEntry, DictionaryEntryAlias,
                                             prepare_alias_banlist_dict)
from lexnlp.extract.en.entities.nltk_maxent import get_company_annotations
from lexnlp.extract.en.dates import get_date_annotations
from lexnlp.extract.en.money import get_money_annotations
from lexnlp.extract.en.amounts import get_amount_annotations
from lexnlp.extract.en.percents import get_percent_annotations
from lexnlp.extract.en.ratios import get_ratio_annotations
from lexnlp.extract.en.durations import get_duration_annotations

COLUMNS = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]
GEO_CONFIG_DEFAULT = "/app/geoentities.csv"   # LexPredict single-df format, baked into the image
GEO_MIN_ALIAS_DEFAULT = 4                     # backstop only; the ISO columns are dropped outright

_SELECTED = []        # [(label, raw, fn)], set in main() before the pool forks
_GEO_LOCATOR = None    # GeoEntityLocator, built once in main() before the pool forks
_TIMEOUT = 0           # per-extractor-per-doc seconds, set in main() before the fork


class _ExtractorTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _ExtractorTimeout()


def _init_worker():
    """Pool initializer: install the SIGALRM handler in each worker process."""
    signal.signal(signal.SIGALRM, _alarm_handler)


def _geo_extractor(text):
    return _GEO_LOCATOR.get_geoentity_annotations(text)


def build_geo_locator(geo_config_path, min_alias_len=GEO_MIN_ALIAS_DEFAULT, iso_codes=False):
    """Build the LexNLP GeoEntityLocator over the LexPredict geoentities CSV.

    THIS IS NOT THE SLOW PATH, THOUGH IT WAS DOCUMENTED AS ONE. An earlier note here claimed the
    geoentity pass cost ~5 s/doc and was the throughput bottleneck; measured on a 300-document
    random draw at n_process=24 it runs at 64 doc/s, against 4.9 doc/s for ORG, DATE and MONEY
    together. Geoentities are about 6% of this extractor's corpus bill. The expensive work is the
    maxent company NER and the date grammar.

    THE ISO CODE COLUMNS ARE DROPPED FROM THE ALIAS SET, AND A LENGTH THRESHOLD IS NOT ENOUGH.
    LexNLP's default aliases are Entity Name, Alias, ISO-3166-2 and ISO-3166-3, and both code
    columns are ordinary English words in a contract. The two-letter codes give IN, OR, ME, DE,
    LA and AL -- "IN" matches "IN WITNESS WHEREOF" on essentially every document. Raising
    min_alias_len to 3 removes those and walks into the three-letter codes, which are worse
    because they match case-insensitively: measured on twenty sample contracts, NOR (Norway)
    was the single most frequent span at 188 hits, from "neither ... nor", and AND (Andorra)
    took 37. The set also contains PER, CAN, ARE, ARM, FIN, TON and MAR, and "per annum" alone
    would poison the label.

    So the fix is to name the alias columns rather than to filter by length. Entity Name and
    Alias are kept; both ISO columns go. min_alias_len stays as a backstop against a short entry
    in the Alias column, not as the primary guard.

    The cost is postal abbreviations -- "New York, NY 10167" contributes only "New York". That is
    acceptable because the gazetteer engine already recovers a two-letter state code where a ZIP
    follows it, which is a constraint this dictionary cannot express, and duplicating the
    capability badly is worse than not duplicating it.

    Conflict resolving is left at LexNLP's default of "none", so a position matched by two
    entities yields both. That is the honest behaviour for a candidate store: the duplicate is
    visible to the resolution step rather than silently decided here.

    The locator is built once in the parent and inherited by the pool through fork; rebuilding it
    per worker would repeat the dictionary normalisation for every process.
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


def write_output(rows, path):
    """Build the aligned DataFrame and write parquet, with real nulls on the offsets.

    Start/Stop are cast to nullable Int64 so no-hit rows store SQL NULL rather than the
    float64 NaN pandas would otherwise coerce a mixed int/None column into. The string
    columns keep object dtype, where None already serialises to a parquet null.
    """
    out = pd.DataFrame(rows, columns=COLUMNS)
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
    out.to_parquet(path, index=False)
    return out


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


def extract_one(args):
    """Rows for one document. Always returns >=1 row: a null-span sentinel if no match
    (including blank text). Each extractor is capped at _TIMEOUT seconds (if set); a
    timed-out extractor is skipped for this doc and emits a marker row
    (LabelRaw = "timeout:<raw>", null span) so the orchestrator can mark the doc's
    status as timeout and re-run it later; the doc's other extractors still run."""
    docid, text = args
    rows = []
    timed_out = []
    if isinstance(text, str) and text.strip():
        n = len(text)
        for label, raw, fn in _SELECTED:
            try:
                if _TIMEOUT > 0:
                    signal.alarm(_TIMEOUT)
                try:
                    anns = list(fn(text))
                finally:
                    if _TIMEOUT > 0:
                        signal.alarm(0)
            except _ExtractorTimeout:
                print(f"[timeout] {docid}: {raw} > {_TIMEOUT}s, skipped", file=sys.stderr)
                timed_out.append(raw)
                continue
            except Exception:                       # one bad extractor/doc shouldn't kill the run
                continue
            for ann in anns:
                start, stop = ann.coords
                if 0 <= start < stop <= n:
                    rows.append((docid, start, stop, text[start:stop], label, raw, "lexnlp", "lexnlp"))
    # one marker row per timed-out extractor (null span; survives into the store
    # only long enough for ner_db_append to read Status, then dropped)
    for raw in timed_out:
        rows.append((docid, None, None, None, None, f"timeout:{raw}", "lexnlp", "lexnlp"))
    if not rows:                                    # genuine no-hit -> sentinel
        rows.append((docid, None, None, None, None, None, "lexnlp", "lexnlp"))
    return rows


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--label", nargs="+", default=["ORG"],
                    help="unified labels to extract; supported: " + ", ".join(EXTRACTORS))
    ap.add_argument("--max-chars", type=int, default=0,
                    help="truncate each document to its first N characters (0 = off)")
    ap.add_argument("--timeout", type=int, default=60,
                    help="per-extractor-per-document cap in seconds (0 = off)")
    ap.add_argument("--geo-config", default=GEO_CONFIG_DEFAULT,
                    help="geoentities CSV for GPE (LexPredict single-df format)")
    ap.add_argument("--geo-min-alias-len", type=int, default=GEO_MIN_ALIAS_DEFAULT,
                    help="shortest geo alias matched; a backstop, not the primary guard")
    ap.add_argument("--geo-iso-codes", action="store_true",
                    help="also match ISO-3166 codes as places; NOR, AND, PER and CAN are English "
                         "words and this is off for that reason")
    ap.add_argument("--n-process", type=int, default=1, help="worker processes (<=0 = all cores)")
    ap.add_argument("--chunk-size", type=int, default=8, help="docs per task when parallelising")
    ap.add_argument("--no-progress", action="store_true", help="disable the progress bar")
    args = ap.parse_args()

    global _SELECTED, _GEO_LOCATOR, _TIMEOUT
    _SELECTED = [(lab, *EXTRACTORS[lab]) for lab in args.label if lab in EXTRACTORS]
    _TIMEOUT = max(0, args.timeout)
    if not _SELECTED:
        write_output([], args.output)
        print(f"no LexNLP extractor for {args.label}; wrote empty output")
        return

    # GPE is dictionary-driven: build the locator once (before the pool forks).
    if any(lab == "GPE" for lab, _, _ in _SELECTED):
        _GEO_LOCATOR = build_geo_locator(args.geo_config, args.geo_min_alias_len,
                                         args.geo_iso_codes)
        n_alias = sum(len(e.aliases) for e in _GEO_LOCATOR.geo_config_list)
        print(f"geo locator: {len(_GEO_LOCATOR.geo_config_list)} entities, {n_alias} aliases, "
              f"min_alias_len={args.geo_min_alias_len}, iso_codes={args.geo_iso_codes}",
              file=sys.stderr)

    nproc = args.n_process if args.n_process > 0 else (os.cpu_count() or 1)
    desc = f"lexnlp {[s[0] for s in _SELECTED]} (n_process={nproc})"

    df = (pads.dataset(resolve_inputs(args.inputs), format="parquet")
              .to_table(columns=[args.id_col, args.text_col])
              .to_pandas())
    texts = df[args.text_col].tolist()
    texts, n_trunc = truncate_texts(texts, args.max_chars)
    if n_trunc:
        print(f"{n_trunc} doc(s) truncated to {args.max_chars} chars", file=sys.stderr)
    items = list(zip(df[args.id_col].tolist(), texts))

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

    out = write_output(rows, args.output)
    n_cand = int(out["Start"].notna().sum())
    print(f"{len(df)} doc(s) -> {n_cand} candidate(s) [lexnlp {[s[0] for s in _SELECTED]}, n_process={nproc}]")


if __name__ == "__main__":
    main()
