#!/usr/bin/env python3
"""Regex date and term extractor with offsets -- the paper's patterns, ported, parsed, and extended.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model, DateValue,
        TermN, TermUnit, TermYears).

Ports the original paper pipeline's date regexes (03-NamedEntities.R, get_date_regex) into the
harmonised candidate schema: Engine = "paper", Model = MODEL (a constant tied to this pattern set --
revising the patterns means bumping it), Label = "DATE" or "TERM", LabelRaw = the pattern name (ISO,
Slash, Text, PeriodOf, Anniversary, ...). Differences vs the paper, by design: runs on TextRaw (not
the uppercased/de-punctuated TextMod), so offsets index the canonical text and the
punctuation-bearing patterns (European, Text) behave as written; case-insensitive throughout; emits
offset spans instead of a count table.

v3 ADDS THE TERM FAMILY, AND IT IS A SECOND LABEL RATHER THAN A SECOND ENGINE
A contract states when it ends in one of two ways. It names a date -- which every pattern below the
DATE table finds -- or it states a DURATION and names no date at all: "for a period of five (5)
years from the Effective Date", "the third anniversary", "shall continue until terminated". The
second is invisible to a date extractor by construction, and on a 4,398-document sample it is the
only thing said about the end in roughly one document in ten.

Terms are emitted with Label = "TERM" and carry TermN, TermUnit and TermYears where DATE carries
DateValue. THE UNIT IS KEPT, NOT ONLY THE CONVERSION: thirty days and one month are within a day of
each other in years and are not the same thing in a contract, and it is the unit that separates a
notice period from a duration. A consumer wanting years has TermYears; one wanting to exclude
anything under a year has TermUnit and does not have to rediscover the distinction.

OVERLAPS ARE RESOLVED WITHIN A LABEL, NOT ACROSS. "for a period of five (5) years from January 1,
2020" is a term AND a date, they overlap, and both are true. Resolving across labels would make the
longer span silently delete the shorter, so each label's candidates compete only with their own.

    DATE
    ISO           2011-03-03          ISOShort    03-03-2011
    Slash         3/3/2011            SlashShort  3/3/11
    European      3.3.2011            YearFirst   2011/03/03
    Text          March 3, 2011       MonthYear   March 2011
    DayMonthLong  the 9th day of January, 2014    DayMonth   3rd March 2011

    TERM
    PeriodOf      for a period of five (5) years  UnitTerm    a three (3)-year term
    ContinueFor   shall continue for ten (10) years          UnitPeriod  a thirty (30) day period
    Anniversary   the third anniversary           OpenEnded   shall continue until terminated

UnitTerm AND UnitPeriod ARE ONE PATTERN SPLIT ON ITS TRAILING NOUN, and the split is worth a
provenance column of its own. Measured over 500 documents before it was made: every one of the
eight commonest matches ended in "period" -- "thirty (30) day period", "30-day period", "ten (10)
day period" -- and the modal value was thirty days. A "three-year TERM" is what a contract calls
its own duration; a "thirty day PERIOD" is a notice, a cure or a payment window. Both are stated
periods and the extractor emits both; which of them is a DURATION is a question for the rule that
reads this, and LabelRaw is what lets that rule decide without re-reading the text.

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

WHY THIS ENGINE CANNOT INVENT A DATE. Every DATE pattern requires a year in the text -- four digits
in nine of them, two in SlashShort -- so there is no partial match to complete and no reference date
to complete it from. That is a real difference from a grammar-based extractor, which will happily
resolve "May 1 of each year" by supplying a year of its own; and if that year comes from the clock,
the same document yields different dates on different days. Measured against LexNLP on the sample:
of its spans carrying no written year, 76% resolve to the future at a median of 11.6 years out, and
80.8% of all dates more than fifteen years out are year-less. Section numbers and exhibit references
-- "Section 3-1", "Exhibit 10-15" -- are what that produces. Nothing here can do it: no year in the
text, no match.

A TERM CANNOT BE INVENTED EITHER, on the same principle: every term pattern requires a number and a
unit in the text, or an explicit ordinal. OpenEnded is the one exception and it is deliberate --
"shall continue until terminated" is a real drafting form stating an unbounded term, and it emits a
span with TermYears NULL rather than nothing at all, so a consumer sees a stated term of unknown
length rather than an absent one.

TWO ASSUMPTIONS, BOTH NAMED HERE RATHER THAN LEFT TO A READER
  MONTH-FIRST. Slash, SlashShort and ISOShort are month-day-year, because these are US filings.
  "10/1/1999" is therefore 1 October, not 10 January. European is day-first, which is what its name
  has always meant. The two orders differ only where both components are 12 or under; measured over
  the sample that is 47.2% of numeric spans, but among the UNAMBIGUOUS ones only 3.0% are day-first,
  so the convention is right and the residual exposure is at most thirty days on a duration reported
  in years.
  TWO-DIGIT YEARS pivot at 69: 00-68 read as 2000-2068, 69-99 as 1969-1999. EDGAR begins in 1993
  and the pivot is the C standard's, so the rule is safe for this corpus and stated for any other.

PRECISION IS NOT A COLUMN. MonthYear resolves to the first of the month, so its DateValue looks
like a day-precision date and is not one. LabelRaw already says which pattern matched, and
duplicating that into a second column invites the two to disagree. CONSUMERS MUST READ LabelRaw:
LabelRaw == "MonthYear" means the day in DateValue is a placeholder, and LabelRaw == "OpenEnded"
means TermYears is absent because the term is unbounded rather than because parsing failed.

An unparseable match keeps its span and gets a null value -- "February 30, 2011" and "13/45/2020"
both match a pattern and denote no date. That is a third state beyond found and not found, and
dropping those rows would report a precision the patterns did not achieve.

Matching is per pattern (for LabelRaw provenance); overlapping spans are resolved WITHIN EACH LABEL
by LONGEST SPAN WINS, pattern-table order breaking ties (so Text beats MonthYear where both fire on
the same text).

Every input DocID appears in the output at least once: a doc with matches contributes one row per
kept match; a doc with none contributes a single null-span sentinel row (Engine/Model still set), so
the orchestrator can record the doc as processed.

--max-chars N truncates every document to its first N characters BEFORE extraction (0 = off).
Offsets are 0-based, half-open, code-point indices: text[Start:Stop] == Span. Aligned schema/CLI
with extract_spacy.py / extract_lexnlp.py.
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


ENGINE = "paper"
MODEL = "dateregex-v3"   # identifies THIS pattern set; bump on any pattern change
CORE = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]
EXTRA = ["DateValue", "TermN", "TermUnit", "TermYears"]
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


# -- TERMS -------------------------------------------------------------------------------------
# A duration stated in words, naming no date. Built from the same shape as the date table: name,
# regex, reading. The reading says how to turn the matched text into a number and a unit.
#   n_unit     a number and a unit, in that order       "five (5) years"
#   ord_year   an ordinal, read as that many years      "the third anniversary"
#   open       an unbounded term, no number at all      "shall continue until terminated"

# Written-out numbers, because a contract writes "five (5) years" about as often as "5 years" and
# about as often as "five years". Where the parenthesised digit is present it wins: it is the
# drafter's own disambiguation of their own sentence.
NUMBER_WORD = {
    "one": 1, "two": 2, "three": 3, "four": 4, "five": 5, "six": 6, "seven": 7, "eight": 8,
    "nine": 9, "ten": 10, "eleven": 11, "twelve": 12, "thirteen": 13, "fourteen": 14,
    "fifteen": 15, "sixteen": 16, "seventeen": 17, "eighteen": 18, "nineteen": 19, "twenty": 20,
    "thirty": 30, "forty": 40, "fifty": 50, "sixty": 60, "ninety": 90,
}
ORDINAL_WORD = {
    "first": 1, "second": 2, "third": 3, "fourth": 4, "fifth": 5, "sixth": 6, "seventh": 7,
    "eighth": 8, "ninth": 9, "tenth": 10, "eleventh": 11, "twelfth": 12, "fifteenth": 15,
    "twentieth": 20,
}
# Longest first, for the same leftmost-first reason the month list is ordered: with "seven" before
# "seventeen", "seventeen" matches "seven" and leaves "teen" behind.
_NUMW = "|".join(sorted(NUMBER_WORD, key=len, reverse=True))
_ORDW = "|".join(sorted(ORDINAL_WORD, key=len, reverse=True))
_NUM = r"(?:" + _NUMW + r"|\d{1,3})"
_ORDN = r"(?:" + _ORDW + r"|\d{1,2}(?:st|nd|rd|th))"

# A unit is kept as written and converted separately; the conversion is nominal, which is why the
# unit travels beside it. A month is a twelfth of a year here and 28 to 31 days in a contract.
UNIT_YEARS = {"day": 1.0 / 365.25, "week": 7.0 / 365.25, "month": 1.0 / 12.0, "year": 1.0}
_UNIT = r"(?:year|month|week|day)"

# The parenthesised digit is an OPTIONAL SECOND READING of the same number, never a separate one.
_NUM_PAREN = _NUM + r"\s*(?:\(\s*(\d{1,3})\s*\))?"

TERM_PATTERNS = [
    ("PeriodOf",    r"\b(?:period|term)\s+of\s+(?:approximately\s+)?" + _NUM_PAREN +
                    r"\s+" + _UNIT + r"s?\b",                                   "n_unit"),
    ("ContinueFor", r"\b(?:continue|continues|remain|remains|be)\s+(?:in\s+"
                    r"(?:full\s+force\s+and\s+)?effect\s+)?for\s+(?:a\s+)?"
                    r"(?:period\s+of\s+)?" + _NUM_PAREN + r"\s+" + _UNIT + r"s?\b", "n_unit"),
    ("UnitTerm",    r"\b" + _NUM_PAREN + r"[\s-]+" + _UNIT + r"\s+term\b",     "n_unit"),
    ("UnitPeriod",  r"\b" + _NUM_PAREN + r"[\s-]+" + _UNIT + r"\s+period\b",   "n_unit"),
    ("Anniversary", r"\b" + _ORDN + r"\s+anniversary\b",                        "ord_year"),
    ("OpenEnded",   r"\b(?:continue|continues|remain|remains)\s+(?:in\s+"
                    r"(?:full\s+force\s+and\s+)?effect\s+)?until\s+terminated\b"
                    r"|\bin\s+perpetuity\b|\bperpetual\s+(?:term|license|licence)\b", "open"),
]
TERM_COMPILED = [(name, re.compile(pat, re.IGNORECASE), kind)
                 for name, pat, kind in TERM_PATTERNS]
TERM_KIND_BY_NAME = {name: kind for name, _pat, kind in TERM_PATTERNS}

_RE_UNIT = re.compile(_UNIT, re.IGNORECASE)
_RE_NUMW = re.compile(_NUMW, re.IGNORECASE)
_RE_ORDW = re.compile(_ORDW, re.IGNORECASE)


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


def parse_term(span, kind):
    """One matched span and its pattern's reading -> (n, unit, years), any of which may be None.

    THE PARENTHESISED DIGIT WINS where the drafter supplied both, because "five (5) years" is one
    number written twice and the digit is their own disambiguation. Where a span carries digits and
    no parenthesis the digits are the number; where it carries only a word, the word is.

    OpenEnded returns a unit and no number: the term is stated and unbounded, which is a different
    fact from a term that failed to parse, and LabelRaw says which.
    """
    if not kind:
        return None, None, None

    if kind == "open":
        return None, "open", None

    if kind == "ord_year":
        w = _RE_ORDW.search(span)
        if w is not None:
            n = ORDINAL_WORD.get(w.group(0).lower())
        else:
            d = _RE_DIGITS.search(span)
            n = int(d.group(0)) if d else None
        if n is None:
            return None, None, None
        return float(n), "year", float(n)

    # n_unit: a number then a unit. The digits inside a parenthesis are preferred; failing that the
    # first bare number, whether written as digits or as a word.
    unit_m = _RE_UNIT.search(span)
    if unit_m is None:
        return None, None, None
    unit = unit_m.group(0).lower()

    n = None
    paren = re.search(r"\(\s*(\d{1,3})\s*\)", span)
    if paren is not None:
        n = int(paren.group(1))
    else:
        head = span[:unit_m.start()]
        d = _RE_DIGITS.search(head)
        if d is not None:
            n = int(d.group(0))
        else:
            w = _RE_NUMW.search(head)
            if w is not None:
                n = NUMBER_WORD.get(w.group(0).lower())
    if n is None or n <= 0:
        return None, unit, None

    return float(n), unit, round(float(n) * UNIT_YEARS[unit], 4)
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


def _keep_longest(text, compiled):
    """Candidates from one pattern table, overlaps resolved by longest span first.

    Pattern-table order breaks ties, then position. Greedy: a kept span blocks anything overlapping
    it. Quadratic in candidate count in the worst case, which is where the per-document cap earns
    its place -- a table of dates can produce tens of thousands of candidates in one document.
    """
    cands = []
    for prio, (name, rx, _kind) in enumerate(compiled):
        for m in rx.finditer(text):
            cands.append((m.start(), m.end(), name, prio))
    cands.sort(key=lambda c: (-(c[1] - c[0]), c[3], c[0]))
    kept = []
    for s, e, name, _ in cands:
        if all(e <= ks or s >= ke for ks, ke, _ in kept):
            kept.append((s, e, name))
    kept.sort()
    return kept


def extract_one(args):
    """Rows for one document: dates and terms, each resolved within its own label.

    OVERLAPS ARE RESOLVED WITHIN A LABEL, NOT ACROSS. "for a period of five (5) years from
    January 1, 2020" is a term AND a date, they overlap, and both are true; a single greedy pass
    would let the longer span delete the shorter and lose one of them.

    Takes a (docid, text) TUPLE rather than two arguments, because imap_unordered passes one item
    per call. The signature matches extract_moneyregex.py and extract_redaction.py.

    A document that exceeds the cap emits a timeout marker rather than raising, so one pathological
    document cannot end the pass. Parsing happens INSIDE the cap, after overlap resolution, so it
    runs once per kept span rather than once per candidate.
    """
    docid, text = args
    rows = []
    if isinstance(text, str) and text.strip():
      try:
        if _TIMEOUT > 0:
            signal.alarm(_TIMEOUT)

        for s, e, name in _keep_longest(text, COMPILED):
            rows.append((docid, s, e, text[s:e], "DATE", name, ENGINE, MODEL,
                         parse_span(text[s:e], ORDER_BY_NAME.get(name)),
                         None, None, None))

        for s, e, name in _keep_longest(text, TERM_COMPILED):
            n, unit, years = parse_term(text[s:e], TERM_KIND_BY_NAME.get(name))
            rows.append((docid, s, e, text[s:e], "TERM", name, ENGINE, MODEL,
                         None, n, unit, years))

        rows.sort(key=lambda r: (r[4], r[1]))
      except _ExtractorTimeout:
        print(f"[timeout] {docid}: > {_TIMEOUT}s, skipped", file=sys.stderr)
        return [(docid, None, None, None, None, "timeout:dateregex", ENGINE, MODEL,
                 None, None, None, None)]
      finally:
        if _TIMEOUT > 0:
            signal.alarm(0)
    if not rows:
        rows.append((docid, None, None, None, None, None, ENGINE, MODEL,
                     None, None, None, None))
    return rows


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--label", nargs="+", default=["DATE", "TERM"],
                    help="unified labels to extract; this engine supports: DATE TERM")
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

    want = set(l.upper() for l in args.label)
    rows = []
    if want & {"DATE", "TERM"}:
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

        # A label the caller did not ask for is dropped AFTER extraction rather than skipped during
        # it: both tables run over the same text in one pass, so filtering here costs nothing and
        # keeps one code path rather than two.
        if want != {"DATE", "TERM"}:
            rows = [r for r in rows if r[4] is None or r[4] in want]
    else:
        print(f"no date extractor for {args.label}; writing sentinels only", file=sys.stderr)
        rows = [(docid, None, None, None, None, None, ENGINE, MODEL, None, None, None, None)
                for docid in df[args.id_col].tolist()]

    # _NER.R creates the output directory before calling, so the pipeline never needs this. A
    # manual run does, and failing after the extraction rather than before it wastes the whole pass.
    outdir = os.path.dirname(os.path.abspath(args.output))
    if outdir:
        os.makedirs(outdir, exist_ok=True)

    out = pd.DataFrame(rows, columns=COLUMNS)
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
    out["DateValue"] = out["DateValue"].astype("string")   # stays text across the seam
    out["TermUnit"] = out["TermUnit"].astype("string")
    out[["TermN", "TermYears"]] = out[["TermN", "TermYears"]].astype("Float64")

    n_date = int((out["Label"] == "DATE").sum())
    n_parsed = int(out["DateValue"].notna().sum())
    n_term = int((out["Label"] == "TERM").sum())
    n_termv = int(out["TermYears"].notna().sum())
    out.to_parquet(args.output, index=False)
    print(f"{len(df)} doc(s) -> {n_date} date(s), {n_parsed} parsed; "
          f"{n_term} term(s), {n_termv} valued  [{ENGINE}:{MODEL}]")


if __name__ == "__main__":
    main()
