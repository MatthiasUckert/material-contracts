"""Dates and stated terms, by pattern, with offsets.

Emits two labels. DATE is the paper's original pattern set, ported and parsed. TERM is a duration
stated in words with no date attached, which a date extractor cannot see by construction.

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

WHY TERM IS A SECOND LABEL AND NOT A SECOND ENGINE
A contract states when it ends in one of two ways: it names a date, or it states a DURATION and
names no date at all. On a 4,398-document sample the second is the only thing said about the end in
roughly one document in ten, and 1,103 documents (25.1%) have no future date but a stated term of a
year or more. End-statement coverage goes from 37.7% to 62.8%. Both tables run over the same text
in one pass, so the second label is free.

WHY THIS ENGINE CANNOT INVENT A DATE
Every DATE pattern requires a year in the text -- four digits in nine of them, two in SlashShort --
so there is no partial match to complete and no reference date to complete it from. That is a real
difference from a grammar-based extractor, which will resolve "May 1 of each year" by supplying a
year of its own; and if that year comes from the clock, the same document yields different dates on
different days. Measured against LexNLP on the sample: of its spans carrying no written year, 76%
resolve to the future at a median of 11.6 years out, and 80.8% of all dates more than fifteen years
out are year-less. Section numbers and exhibit references -- "Section 3-1", "Exhibit 10-15" -- are
what that produces.

A TERM CANNOT BE INVENTED EITHER: every term pattern requires a number and a unit in the text, or an
explicit ordinal. OpenEnded is the single exception and it is deliberate -- "shall continue until
terminated" is a real drafting form stating an unbounded term, and it emits a span with TermYears
null rather than nothing at all, so a consumer sees a stated term of unknown length rather than an
absent one.

CONSUMERS MUST READ LabelRaw. It is provenance, not decoration. LabelRaw == "MonthYear" means the
day in DateValue is a placeholder, because the pattern matched a month and a year and the first of
the month is a convention. LabelRaw == "OpenEnded" means TermYears is absent because the term is
unbounded rather than because parsing failed. Precision is deliberately not a separate column:
duplicating what LabelRaw already says invites the two to disagree.

UnitTerm AND UnitPeriod ARE ONE PATTERN SPLIT ON ITS TRAILING NOUN, and the split is worth a
provenance value of its own. Measured over 500 documents before it was made: every one of the eight
commonest matches ended in "period" -- "thirty (30) day period", "30-day period" -- and the modal
value was thirty days. A "three-year TERM" is what a contract calls its own duration; a "thirty day
PERIOD" is a notice, a cure or a payment window. Both are stated periods and both are emitted;
which of them counts as a duration is a question for the rule that reads this, and LabelRaw is what
lets that rule decide without re-reading the text.

TWO ASSUMPTIONS, NAMED HERE RATHER THAN LEFT TO A READER
  MONTH-FIRST. Slash, SlashShort and ISOShort are month-day-year, because these are US filings.
  "10/1/1999" is therefore 1 October, not 10 January. European is day-first, which is what its name
  has always meant. The two orders differ only where both components are 12 or under; measured over
  the sample that is 47.2% of numeric spans, but among the UNAMBIGUOUS ones only 3.0% are
  day-first, so the convention is right and the residual exposure is at most thirty days on a
  duration reported in years.
  TWO-DIGIT YEARS pivot at 69: 00-68 read as 2000-2068, 69-99 as 1969-1999. EDGAR begins in 1993
  and the pivot is the C standard's, so the rule is safe for this corpus and stated for any other.

An unparseable match keeps its span and gets a null value -- "February 30, 2011" and "13/45/2020"
both match a pattern and denote no date. That is a third state beyond found and not found, and
dropping those rows would report a precision the patterns did not achieve.
"""
from __future__ import annotations

import datetime
import re
import sys

from . import _io

MODEL = "dateregex-v3"     # THE version. The filename carries none; see _io.spec_hash().
NAME = "dateregex"
LABELS = ("DATE", "TERM")
EXTRAS = ("DateValue", "TermN", "TermUnit", "TermYears")

DEFAULT_TIMEOUT = 0
DEFAULT_CHUNK_SIZE = 64


# 1. Dates ---------------------------------------------------------------------------------------

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
_MD = r"(?:" + _M + r")\.?"      # a month name, with the abbreviation's optional full stop

# Month name -> number, built from the same list the patterns are built from, so the two cannot
# drift. Keyed on the first three letters uppercased, which collapses "September" and "Sep" onto one
# entry and makes the lookup independent of the system locale -- strptime's %B reads LC_TIME and
# would parse these differently on a machine set to German.
MONTH_NUM = {name[:3].upper(): i for i, name in enumerate(MONTHS[:12], start=1)}

_RE_ORDINAL = re.compile(r"(?<=\d)(st|nd|rd|th)", re.IGNORECASE)
_RE_MONTH = re.compile(_M, re.IGNORECASE)
_RE_DIGITS = re.compile(r"\d+")
_ORD = r"(?:st|nd|rd|th)?"

# The paper's patterns, in priority order: ties on equal-length overlaps go to the earlier entry.
# The third field is the ORDER of the numeric components as they appear in the matched text, and it
# is what turns a span into a date.
#   ymd / mdy / dmy   three numbers, read in that order
#   Mdy / My          a month NAME, then the remaining numbers in that order
#
# DAY-FIRST AND MONTH-FIRST SHARE ONE PARSE, which is why all three textual patterns carry "Mdy".
# The month is identified by NAME rather than by position, so once it is removed the remaining
# numbers are day-then-year in both readings. That is the whole reason a textual date carries none
# of the ambiguity a numeric one does.
#
# DayMonthLong and DayMonth both outrank MonthYear by span length. Before they existed, MonthYear
# fired on the TAIL of "3rd March 2011", kept "March 2011", and resolved it to the first of the
# month: a silently wrong day carrying a LabelRaw that gave no hint anything had been dropped.
PATTERNS = [
    ("ISO",          r"\b\d{4}-\d{1,2}-\d{1,2}\b",                                     "ymd"),
    ("ISOShort",     r"\b\d{2}-\d{2}-\d{4}\b",                                         "mdy"),
    ("Slash",        r"\b\d{1,2}/\d{1,2}/\d{4}\b",                                     "mdy"),
    ("SlashShort",   r"\b\d{1,2}/\d{1,2}/\d{2}\b",                                     "mdy"),
    ("European",     r"\b\d{1,2}\.\d{1,2}\.\d{4}\b",                                   "dmy"),
    ("DayMonthLong", r"\b\d{1,2}" + _ORD + r"\s+day\s+of\s+" + _MD + r",?\s+\d{4}\b",   "Mdy"),
    ("Text",         _MD + r"\s+\d{1,2}" + _ORD + r"(?:[,\s]+|\s+)\d{4}\b",             "Mdy"),
    ("DayMonth",     r"\b\d{1,2}" + _ORD + r"\s+" + _MD + r",?\s+\d{4}\b",              "Mdy"),
    ("YearFirst",    r"\b\d{4}/\d{1,2}/\d{1,2}\b",                                     "ymd"),
    ("MonthYear",    _MD + r"\s*,?\s*\d{4}\b",                                          "My"),
]
COMPILED = [(name, re.compile(pat, re.IGNORECASE), order) for name, pat, order in PATTERNS]
ORDER_BY_NAME = {name: order for name, _pat, order in PATTERNS}

YEAR_PIVOT = 69          # 00-68 -> 2000s, 69-99 -> 1900s; the C standard's rule


# 2. Terms ---------------------------------------------------------------------------------------

# Written-out numbers, because a contract writes "five (5) years" about as often as "5 years" and
# about as often as "five years".
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

# THE UNIT IS KEPT, NOT ONLY THE CONVERSION. Thirty days and one month are within a day of each
# other in years and are not the same thing in a contract, and it is the unit that separates a
# notice period from a duration. A consumer wanting years has TermYears; one wanting to exclude
# anything under a year has TermUnit and does not have to rediscover the distinction. The
# conversion is nominal -- a month is a twelfth of a year here and 28 to 31 days in a contract.
UNIT_YEARS = {"day": 1.0 / 365.25, "week": 7.0 / 365.25, "month": 1.0 / 12.0, "year": 1.0}
_UNIT = r"(?:year|month|week|day)"

# The parenthesised digit is an OPTIONAL SECOND READING of the same number, never a separate one.
_NUM_PAREN = _NUM + r"\s*(?:\(\s*(\d{1,3})\s*\))?"

TERM_PATTERNS = [
    ("PeriodOf",    r"\b(?:period|term)\s+of\s+(?:approximately\s+)?" + _NUM_PAREN +
                    r"\s+" + _UNIT + r"s?\b",                                        "n_unit"),
    ("ContinueFor", r"\b(?:continue|continues|remain|remains|be)\s+(?:in\s+"
                    r"(?:full\s+force\s+and\s+)?effect\s+)?for\s+(?:a\s+)?"
                    r"(?:period\s+of\s+)?" + _NUM_PAREN + r"\s+" + _UNIT + r"s?\b",   "n_unit"),
    ("UnitTerm",    r"\b" + _NUM_PAREN + r"[\s-]+" + _UNIT + r"\s+term\b",            "n_unit"),
    ("UnitPeriod",  r"\b" + _NUM_PAREN + r"[\s-]+" + _UNIT + r"\s+period\b",          "n_unit"),
    ("Anniversary", r"\b" + _ORDN + r"\s+anniversary\b",                              "ord_year"),
    ("OpenEnded",   r"\b(?:continue|continues|remain|remains)\s+(?:in\s+"
                    r"(?:full\s+force\s+and\s+)?effect\s+)?until\s+terminated\b"
                    r"|\bin\s+perpetuity\b|\bperpetual\s+(?:term|license|licence)\b", "open"),
]
TERM_COMPILED = [(name, re.compile(pat, re.IGNORECASE), kind) for name, pat, kind in TERM_PATTERNS]
TERM_KIND_BY_NAME = {name: kind for name, _pat, kind in TERM_PATTERNS}

_RE_UNIT = re.compile(_UNIT, re.IGNORECASE)
_RE_NUMW = re.compile(_NUMW, re.IGNORECASE)
_RE_ORDW = re.compile(_ORDW, re.IGNORECASE)
_RE_PAREN_NUM = re.compile(r"\(\s*(\d{1,3})\s*\)")


# 3. Identity ------------------------------------------------------------------------------------

# EVERYTHING THAT CHANGES OUTPUT, AND NOTHING THAT DOES NOT. Edit a pattern and the hash moves;
# rewrite a docstring and it does not. A moved hash under an unchanged MODEL is an abort, because
# it means an existing store's dateregex-v3 rows were produced by rules that no longer exist.
SPEC = {
    "months": MONTHS,
    "patterns": PATTERNS,
    "year_pivot": YEAR_PIVOT,
    "term_patterns": TERM_PATTERNS,
    "number_word": NUMBER_WORD,
    "ordinal_word": ORDINAL_WORD,
    "unit_years": UNIT_YEARS,
}

EMIT = _io.Emitter(MODEL, EXTRAS)


# 4. Parsing -------------------------------------------------------------------------------------

def parse_span(span, order):
    """One matched span and its pattern's component order -> an ISO date string, or None.

    Returns None rather than raising on a match that denotes no date: the patterns admit
    "February 30, 2011" and "13/45/2020" because a regex counts digits and does not know how many
    days April has. datetime.date does the validating.

    :param span: the matched text.
    :param order: one of ymd, mdy, dmy, Mdy, My.
    :return: ISO date string, or None where the match denotes no date.
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
            day, year = 1, nums[-1]      # first of the month; LabelRaw says it is a placeholder
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
        else:                            # dmy
            day, month, year = nums[0], nums[1], nums[2]
        if year < 100:                   # two-digit year, pivoted
            year += 2000 if year < YEAR_PIVOT else 1900

    try:
        return datetime.date(year, month, day).isoformat()
    except ValueError:
        return None


def parse_term(span, kind):
    """One matched span and its pattern's reading -> (n, unit, years), any of which may be None.

    THE PARENTHESISED DIGIT WINS where the drafter supplied both, because "five (5) years" is one
    number written twice and the digit is their own disambiguation of their own sentence. Where a
    span carries digits and no parenthesis the digits are the number; where it carries only a word,
    the word is.

    OpenEnded returns a unit and no number: the term is stated and unbounded, which is a different
    fact from a term that failed to parse, and LabelRaw says which.

    :param span: the matched text.
    :param kind: one of n_unit, ord_year, open.
    :return: (TermN, TermUnit, TermYears).
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

    # n_unit: a number then a unit.
    unit_m = _RE_UNIT.search(span)
    if unit_m is None:
        return None, None, None
    unit = unit_m.group(0).lower()

    n = None
    paren = _RE_PAREN_NUM.search(span)
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


# 5. Extraction ----------------------------------------------------------------------------------

WANT = frozenset(LABELS)     # set inside each worker by init_worker


def init_worker(timeout, want):
    """Pool initialiser: the per-document cap and the requested label set.

    Both are set in the WORKER, never in main() and read here. Under spawn a worker inherits no
    module state, so anything set in the parent is absent in the child -- and spawn is the default
    on macOS, which is the machine this runs on.
    """
    global WANT
    _io.install_alarm(timeout)
    WANT = frozenset(want)


def extract_one(args):
    """Rows for one document: dates and terms, each resolved within its own label.

    Takes a (docid, text) TUPLE rather than two arguments, because imap_unordered passes one item
    per call.

    A document over the cap emits a timeout marker rather than raising, so one pathological file
    cannot end a corpus pass. Parsing happens INSIDE the cap and AFTER overlap resolution, so it
    runs once per kept span rather than once per candidate.
    """
    docid, text = args
    rows = []
    if isinstance(text, str) and text.strip():
        try:
            _io.start_alarm()

            if "DATE" in WANT:
                for s, e, name in _io.keep_longest(text, COMPILED):
                    span = text[s:e]
                    rows.append(EMIT.row(
                        docid, s, e, span, "DATE", name,
                        (parse_span(span, ORDER_BY_NAME.get(name)), None, None, None)
                    ))

            if "TERM" in WANT:
                for s, e, name in _io.keep_longest(text, TERM_COMPILED):
                    span = text[s:e]
                    n, unit, years = parse_term(span, TERM_KIND_BY_NAME.get(name))
                    rows.append(EMIT.row(
                        docid, s, e, span, "TERM", name, (None, n, unit, years)
                    ))

            rows.sort(key=lambda r: (r[4], r[1]))
        except _io.ExtractorTimeout:
            print(f"[timeout] {docid}: {NAME} > {_io.TIMEOUT}s, skipped", file=sys.stderr)
            return [EMIT.timeout(docid, NAME)]
        except Exception as exc:            # one bad document must not end a corpus pass
            print(f"[error] {docid}: {NAME}: {exc}", file=sys.stderr)
            return [EMIT.error(docid, NAME)]
        finally:
            _io.cancel_alarm()

    if not rows:
        rows.append(EMIT.sentinel(docid))
    return rows


def run(inputs, out_dir, labels=LABELS, id_col="DocID", text_col="TextRaw", max_chars=0,
        timeout=DEFAULT_TIMEOUT, n_process=1, chunk_size=DEFAULT_CHUNK_SIZE, no_progress=False):
    """One pass of this extractor over a document set.

    A label the caller did not ask for is skipped during extraction rather than filtered after it.
    Both tables run over the same text, so the saving is small -- but the ledger records what was
    REQUESTED, and an extractor that emits a label nobody asked for writes rows the ledger has no
    entry for.

    :return: the path written.
    """
    want = [l.upper() for l in labels if l.upper() in LABELS]
    ids, texts = _io.read_documents(inputs, id_col, text_col, max_chars)

    if not want:
        print(f"no {NAME} label in {labels}; writing sentinels only", file=sys.stderr)
        rows = [EMIT.sentinel(i) for i in ids]
    else:
        rows = _io.run_pool(
            items=list(zip(ids, texts)),
            extract_one=extract_one,
            initializer=init_worker,
            initargs=(max(0, timeout), tuple(want)),
            n_process=n_process,
            chunk_size=chunk_size,
            desc=f"{_io.ENGINE}:{MODEL}",
            no_progress=no_progress,
        )

    frame = _io.finalize(rows, ids, EMIT)
    path = _io.write_output(frame, out_dir, MODEL, casts={
        "DateValue": "string",      # stays text across the seam; R parses it
        "TermUnit": "string",
        "TermN": "Float64",
        "TermYears": "Float64",
    })

    n_date = int((frame["Label"] == "DATE").sum())
    n_parsed = int(frame["DateValue"].notna().sum())
    n_term = int((frame["Label"] == "TERM").sum())
    n_termv = int(frame["TermYears"].notna().sum())
    print(f"{len(ids)} doc(s) -> {n_date} date(s), {n_parsed} parsed; "
          f"{n_term} term(s), {n_termv} valued  [{_io.ENGINE}:{MODEL}] -> {path}")
    return path


def main(argv=None):
    ap = _io.parser(f"matcon-extract {NAME}", __doc__.splitlines()[0],
                    default_timeout=DEFAULT_TIMEOUT, default_chunk_size=DEFAULT_CHUNK_SIZE)
    ap.add_argument("--label", nargs="+", default=list(LABELS),
                    help=f"labels to extract; this engine supports: {' '.join(LABELS)}")
    ap.add_argument("--version", action="version", version=_io.version_line(MODEL, SPEC))
    a = ap.parse_args(argv)
    run(a.inputs, a.out_dir, a.label, a.id_col, a.text_col, a.max_chars, a.timeout,
        a.n_process, a.chunk_size, a.no_progress)


if __name__ == "__main__":
    main()
