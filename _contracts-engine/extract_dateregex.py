#!/usr/bin/env python3
"""Regex date extractor with offsets -- the paper's eight patterns, ported, and parsed.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model, DateValue).

Ports the original paper pipeline's date regexes (03-NamedEntities.R,
get_date_regex) into the harmonised candidate schema: Engine = "paper",
Model = MODEL (a constant tied to this pattern set -- revising the patterns
means bumping it), Label = "DATE", LabelRaw = the pattern name (ISO, Slash,
Text, ...). Differences vs the paper, by design: runs on TextRaw (not the
uppercased/de-punctuated TextMod), so offsets index the canonical text and the
punctuation-bearing patterns (European, Text) behave as written;
case-insensitive throughout; emits offset spans instead of a count table.

DateValue: THE PARSED DATE, ISO-8601 AS A STRING
Each pattern determines exactly one reading of its own matched text, so the format lives in the
pattern table beside the regex it belongs to rather than being reconstructed downstream. That
placement is the point: adding a ninth pattern without a format is then a visible omission in one
table instead of a silent null appearing in another language.

WHY THIS ENGINE CANNOT INVENT A DATE. Every pattern requires an explicit year, so there is no
partial match to complete and no reference date to complete it from. That is a real difference from
a grammar-based extractor, which will happily resolve "May 1 of each year" by supplying a year of
its own -- and if that year comes from the clock, the same document yields different dates on
different days. Nothing here can do that: no year in the text, no match.

    ISO           2011-03-03          ISOShort    03-03-2011
    Slash         3/3/2011            SlashShort  3/3/11
    European      3.3.2011            YearFirst   2011/03/03
    Text          March 3, 2011       MonthYear   March 2011
    DayMonthLong  the 9th day of January, 2014    DayMonth   3rd March 2011

v2 ADDED THE DAY-FIRST FAMILY, WHICH v1 DID NOT MATCH AT ALL. "the 9th day of January, 2014" and
"this 20th day of December, 2005" are how a contract preamble states its own signing date, and v1
returned nothing for either -- the form the paper most wants was the form it could not see. Worse,
v1's MonthYear fired on the TAIL of "3rd March 2011", kept "March 2011" and would now resolve it to
the first of the month: a silently wrong day carrying a LabelRaw that gives no hint anything was
dropped. Both new patterns outrank MonthYear by span length, so the longer reading wins.

DAY-FIRST AND MONTH-FIRST SHARE ONE PARSE, which is why all three textual patterns carry the order
"Mdy". The month is identified by NAME rather than by position, so once it is removed the remaining
numbers are day-then-year in both readings. That is the whole reason a textual date carries none of
the ambiguity a numeric one does.

Also widened in v2: "Sept" joins the month list (it was absent, so every "Sept 3, 2011" was lost),
abbreviations may carry a full stop ("Mar. 3, 2011"), MonthYear admits a comma ("March, 2011"), and
ISO admits single-digit components ("2011-3-3").

TWO ASSUMPTIONS, BOTH NAMED HERE RATHER THAN LEFT TO A READER
  MONTH-FIRST. Slash, SlashShort and ISOShort are month-day-year, because these are US filings.
  "10/1/1999" is therefore 1 October, not 10 January. European is day-first, which is what its name
  has always meant. The two orders differ only where both components are 12 or under; measured on a
  sample of twenty-five contracts that was ONE span, because contracts overwhelmingly write dates
  out in words -- Text and MonthYear were 171 of 175 matches.
  TWO-DIGIT YEARS pivot at 69: 00-68 read as 2000-2068, 69-99 as 1969-1999. EDGAR begins in 1993
  and the pivot is the C standard's, so the rule is safe for this corpus and stated for any other.

PRECISION IS NOT A COLUMN. MonthYear resolves to the first of the month, so its DateValue looks
like a day-precision date and is not one. LabelRaw already says which pattern matched, and
duplicating that into a second column invites the two to disagree. CONSUMERS MUST READ LabelRaw:
LabelRaw == "MonthYear" means the day in DateValue is a placeholder.

An unparseable match keeps its span and gets a null DateValue -- "February 30, 2011" and
"13/45/2020" both match a pattern and denote no date. That is a third state beyond found and not
found, and dropping those rows would report a precision the patterns did not achieve.

Matching is per pattern (for LabelRaw provenance); overlapping spans are
resolved by LONGEST SPAN WINS, pattern-table order breaking ties (so Text
beats MonthYear where both fire on the same text).

Every input DocID appears in the output at least once: a doc with matches
contributes one row per kept match; a doc with none contributes a single
null-span sentinel row (Engine/Model still set), so the orchestrator can record
the doc as processed.

--max-chars N truncates every document to its first N characters BEFORE
extraction (0 = off). Offsets are 0-based, half-open, code-point indices:
text[Start:Stop] == Span. Aligned schema/CLI with extract_spacy.py /
extract_lexnlp.py.
"""
import argparse
import datetime
import os
import signal
import re
import sys
from multiprocessing import Pool
from pathlib import Path

import pandas as pd
import pyarrow.dataset as pads
from tqdm import tqdm

ENGINE = "paper"
MODEL = "dateregex-v2"   # identifies THIS pattern set; bump on any pattern change
CORE = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]
EXTRA = ["DateValue"]
COLUMNS = CORE + EXTRA

# ORDER IS LOAD-BEARING. Python's alternation is leftmost-first, not longest-first, so a shorter
# name placed earlier wins and leaves a letter behind: with "Sep" before "Sept", "Sept 3, 2011"
# matches "Sep", then the pattern demands whitespace and finds "t", and the whole date is lost.
# Full names precede abbreviations, and "Sept" precedes "Sep", for that reason alone.
MONTHS = [
    "January", "February", "March", "April", "May", "June", "July", "August",
    "September", "October", "November", "December",
    "Sept", "Jan", "Feb", "Mar", "Apr", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
]
_M = "|".join(MONTHS)
_MD = r"(?:" + _M + r")\.?"     # a month name, with the abbreviation's optional full stop

# Month name -> number, built from the same list the patterns are built from, so the two cannot
# drift. Keyed on the first three letters uppercased, which collapses "September" and "Sep" onto
# one entry and makes the lookup independent of the system locale -- strptime's %B reads LC_TIME
# and would parse these differently on a machine set to German.
MONTH_NUM = {}
for _i, _name in enumerate(MONTHS[:12], start=1):
    MONTH_NUM[_name[:3].upper()] = _i

_RE_ORDINAL = re.compile(r"(?<=\d)(st|nd|rd|th)", re.IGNORECASE)
_RE_MONTH = re.compile(_M, re.IGNORECASE)
_RE_DIGITS = re.compile(r"\d+")

# The paper's eight patterns (03-NamedEntities.R, get_date_regex), in priority
# order: ties on equal-length overlaps go to the earlier entry. The third field is the ORDER of the
# numeric components as they appear in the matched text, and it is what turns a span into a date.
#   ymd / mdy / dmy   three numbers, read in that order
#   Mdy / My          a month NAME, then the remaining numbers in that order
_ORD = r"(?:st|nd|rd|th)?"

PATTERNS = [
    ("ISO",           r"\b\d{4}-\d{1,2}-\d{1,2}\b",                        "ymd"),
    ("ISOShort",      r"\b\d{2}-\d{2}-\d{4}\b",                          "mdy"),
    ("Slash",         r"\b\d{1,2}/\d{1,2}/\d{4}\b",                      "mdy"),
    ("SlashShort",    r"\b\d{1,2}/\d{1,2}/\d{2}\b",                      "mdy"),
    ("European",      r"\b\d{1,2}\.\d{1,2}\.\d{4}\b",                    "dmy"),
    ("DayMonthLong",  r"\b\d{1,2}" + _ORD + r"\s+day\s+of\s+" + _MD + r",?\s+\d{4}\b", "Mdy"),
    ("Text",          _MD + r"\s+\d{1,2}" + _ORD + r"(?:[,\s]+|\s+)\d{4}\b",   "Mdy"),
    ("DayMonth",      r"\b\d{1,2}" + _ORD + r"\s+" + _MD + r",?\s+\d{4}\b",  "Mdy"),
    ("YearFirst",     r"\b\d{4}/\d{1,2}/\d{1,2}\b",                      "ymd"),
    ("MonthYear",     _MD + r"\s*,?\s*\d{4}\b",                          "My"),
]
COMPILED = [(name, re.compile(pat, re.IGNORECASE), order) for name, pat, order in PATTERNS]
ORDER_BY_NAME = {name: order for name, _pat, order in PATTERNS}

YEAR_PIVOT = 69          # 00-68 -> 2000s, 69-99 -> 1900s; the C standard's rule


def parse_span(span, order):
    """One matched span and its pattern's component order -> an ISO date string, or None.

    Returns None rather than raising on a match that denotes no date: the patterns admit
    "February 30, 2011" and "13/45/2020" because a regex counts digits and does not know how many
    days April has. datetime.date does the validating.
    """
    if not order:
        return None

    if order in ("Mdy", "My"):
        mon = _RE_MONTH.search(span)
        if mon is None:
            return None
        month = MONTH_NUM.get(mon.group(0)[:3].upper())
        nums = [int(n) for n in _RE_DIGITS.findall(_RE_ORDINAL.sub("", span))]
        if month is None or not nums:
            return None
        if order == "My":
            day, year = 1, nums[-1]          # first of the month; LabelRaw says it is a placeholder
        else:
            if len(nums) < 2:
                return None
            day, year = nums[0], nums[-1]
    else:
        nums = [int(n) for n in _RE_DIGITS.findall(span)]
        if len(nums) < 3:
            return None
        if order == "ymd":
            year, month, day = nums[0], nums[1], nums[2]
        elif order == "mdy":
            month, day, year = nums[0], nums[1], nums[2]
        else:                                 # dmy
            day, month, year = nums[0], nums[1], nums[2]
        if year < 100:                        # two-digit year, pivoted
            year += 2000 if year < YEAR_PIVOT else 1900

    try:
        return datetime.date(year, month, day).isoformat()
    except ValueError:
        return None


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


_TIMEOUT = 0          # per-document seconds, set in each worker by _init_worker


class _ExtractorTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _ExtractorTimeout()


def _init_worker(timeout):
    """Pool initializer: set the per-document cap and install the SIGALRM handler.

    Every pattern is compiled at import, so a worker started under spawn recompiles them rather
    than inheriting them. Nothing is set in main() and read in a worker.
    """
    global _TIMEOUT
    signal.signal(signal.SIGALRM, _alarm_handler)
    _TIMEOUT = timeout


def extract_one(args):
    """Rows for one document: per-pattern matches, overlaps resolved by longest
    span first (pattern order breaking ties), output sorted by Start. Always
    returns >=1 row: a null-span sentinel if nothing matched.

    Takes a (docid, text) TUPLE rather than two arguments, because imap_unordered passes one
    item per call. The signature matches extract_moneyregex.py and extract_redaction.py.

    A document that exceeds the cap emits a timeout marker rather than raising, so one
    pathological document cannot end the pass. The greedy overlap resolution is quadratic in
    candidate count in the worst case, which is where the cap earns its place: a table of dates
    can produce tens of thousands of candidates in a single document.

    Parsing happens INSIDE the cap, after overlap resolution, so it runs once per kept span rather
    than once per candidate and a table of dates cannot pay for it thousands of times over.
    """
    docid, text = args
    rows = []
    if isinstance(text, str) and text.strip():
      try:
        if _TIMEOUT > 0:
            signal.alarm(_TIMEOUT)
        cands = []
        for prio, (name, rx, _order) in enumerate(COMPILED):
            for m in rx.finditer(text):
                cands.append((m.start(), m.end(), name, prio))
        # longest first, then pattern priority, then position; greedy keep
        cands.sort(key=lambda c: (-(c[1] - c[0]), c[3], c[0]))
        kept = []
        for s, e, name, _ in cands:
            if all(e <= ks or s >= ke for ks, ke, _ in kept):
                kept.append((s, e, name))
        kept.sort()
        rows = [(docid, s, e, text[s:e], "DATE", name, ENGINE, MODEL,
                 parse_span(text[s:e], ORDER_BY_NAME.get(name)))
                for s, e, name in kept]
      except _ExtractorTimeout:
        print(f"[timeout] {docid}: > {_TIMEOUT}s, skipped", file=sys.stderr)
        return [(docid, None, None, None, None, "timeout:dateregex", ENGINE, MODEL, None)]
      finally:
        if _TIMEOUT > 0:
            signal.alarm(0)
    if not rows:
        rows.append((docid, None, None, None, None, None, ENGINE, MODEL, None))
    return rows


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--label", nargs="+", default=["DATE"],
                    help="unified labels to extract; this engine supports: DATE")
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
    texts = df[args.text_col].tolist()
    texts, n_trunc = truncate_texts(texts, args.max_chars)
    if n_trunc:
        print(f"{n_trunc} doc(s) truncated to {args.max_chars} chars", file=sys.stderr)

    rows = []
    if "DATE" in args.label:
        items = list(zip(df[args.id_col].tolist(), texts))
        nproc = args.n_process if args.n_process > 0 else (os.cpu_count() or 1)
        desc = f"{ENGINE}:{MODEL} (n_process={nproc})"
        timeout = max(0, args.timeout)

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
    else:
        print(f"no date extractor for {args.label}; writing sentinels only", file=sys.stderr)
        rows = [(docid, None, None, None, None, None, ENGINE, MODEL, None)
                for docid in df[args.id_col].tolist()]

    out = pd.DataFrame(rows, columns=COLUMNS)
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
    out["DateValue"] = out["DateValue"].astype("string")   # stays text across the seam
    n_cand = int(out["Start"].notna().sum())
    n_parsed = int(out["DateValue"].notna().sum())
    out.to_parquet(args.output, index=False)
    print(f"{len(df)} doc(s) -> {n_cand} candidate(s), {n_parsed} parsed  [{ENGINE}:{MODEL}]")


if __name__ == "__main__":
    main()
