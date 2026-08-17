"""What dateregex must match, what it must decline, and what each match must parse to.

Ported from test_dateregex_v3.py. Two tables, and the second matters as much as the first: a regex
is judged as much by what it declines, and every NEGATIVE is a real contract phrase that a looser
pattern would swallow.

The v2 regression lives in test_regression.py, because it needs a baseline artifact rather than a
fixture.
"""
from __future__ import annotations

import pytest

from matcon_extract import dateregex as dr

#: (text, [(Label, LabelRaw, Span, value)]). Value is DateValue for DATE and TermYears for TERM.
CASES = [
    # --- terms: the number-and-unit family --------------------------------------------------
    ("for a period of five (5) years from the Effective Date",
     [("TERM", "PeriodOf", "period of five (5) years", 5.0)]),
    ("for a period of three years",
     [("TERM", "PeriodOf", "period of three years", 3.0)]),
    ("a period of twelve (12) months",
     [("TERM", "PeriodOf", "period of twelve (12) months", 1.0)]),
    ("a period of thirty (30) days",
     [("TERM", "PeriodOf", "period of thirty (30) days", 0.0821)]),
    ("for a period of seventeen (17) months",
     [("TERM", "PeriodOf", "period of seventeen (17) months", 1.4167)]),
    ("period of approximately two years",
     [("TERM", "PeriodOf", "period of approximately two years", 2.0)]),
    ("the initial term of two (2) years",
     [("TERM", "PeriodOf", "term of two (2) years", 2.0)]),

    # THE PARENTHESISED DIGIT WINS. "five (5)" is one number written twice and the digit is the
    # drafter's own disambiguation; a disagreement between the two is theirs, not ours.
    ("a period of five (7) years",
     [("TERM", "PeriodOf", "period of five (7) years", 7.0)]),

    ("This Agreement shall continue for ten (10) years",
     [("TERM", "ContinueFor", "continue for ten (10) years", 10.0)]),
    ("shall remain in full force and effect for five years",
     [("TERM", "ContinueFor", "remain in full force and effect for five years", 5.0)]),

    # UNITTERM AND UNITPERIOD ARE ONE PATTERN SPLIT ON ITS TRAILING NOUN. Over 500 documents every
    # one of the eight commonest matches of the unsplit pattern ended in "period" and the modal
    # value was thirty days: a "three-year TERM" is a duration and a "thirty day PERIOD" is a
    # notice window, and only LabelRaw can tell the rule downstream which it read.
    ("a three (3)-year term",
     [("TERM", "UnitTerm", "three (3)-year term", 3.0)]),
    ("a five year period",
     [("TERM", "UnitPeriod", "five year period", 5.0)]),
    ("each three-year period",
     [("TERM", "UnitPeriod", "three-year period", 3.0)]),
    ("within a thirty (30) day period",
     [("TERM", "UnitPeriod", "thirty (30) day period", 0.0821)]),
    ("a 12-month period",
     [("TERM", "UnitPeriod", "12-month period", 1.0)]),

    # --- terms: the anniversary family ------------------------------------------------------
    ("prior to the third anniversary of the Effective Date",
     [("TERM", "Anniversary", "third anniversary", 3.0)]),
    ("on or before the 5th anniversary",
     [("TERM", "Anniversary", "5th anniversary", 5.0)]),
    ("the twelfth anniversary",
     [("TERM", "Anniversary", "twelfth anniversary", 12.0)]),

    # THE FIRST ANNIVERSARY IS THE ONE TO WATCH. It parses, correctly, as a one-year term -- and in
    # an employment agreement it is as often a vesting date or a notice milestone as it is the
    # contract's end. The extractor's job is to find it; deciding whether it is a DURATION belongs
    # to the rule that reads this, and this case exists so the ambiguity is on the record here.
    ("the first anniversary of the date hereof",
     [("TERM", "Anniversary", "first anniversary", 1.0)]),

    # --- terms: unbounded -------------------------------------------------------------------
    # OpenEnded carries a unit and no number: a term that is stated and unbounded is a different
    # fact from one that failed to parse, and LabelRaw is what says which.
    ("shall continue until terminated by either party",
     [("TERM", "OpenEnded", "continue until terminated", None)]),
    ("remains in effect until terminated in accordance with Section 9",
     [("TERM", "OpenEnded", "remains in effect until terminated", None)]),
    ("the license is granted in perpetuity",
     [("TERM", "OpenEnded", "in perpetuity", None)]),
    ("a perpetual license to use the Marks",
     [("TERM", "OpenEnded", "perpetual license", None)]),

    # --- dates: v2's family, which v3 must not touch -----------------------------------------
    ("dated as of January 1, 2020",
     [("DATE", "Text", "January 1, 2020", "2020-01-01")]),
    ("the 9th day of January, 2014",
     [("DATE", "DayMonthLong", "9th day of January, 2014", "2014-01-09")]),
    ("3/3/2011",       [("DATE", "Slash", "3/3/2011", "2011-03-03")]),
    ("3/3/11",         [("DATE", "SlashShort", "3/3/11", "2011-03-03")]),
    ("2011-03-03",     [("DATE", "ISO", "2011-03-03", "2011-03-03")]),
    ("2011/03/03",     [("DATE", "YearFirst", "2011/03/03", "2011-03-03")]),
    ("3.3.2011",       [("DATE", "European", "3.3.2011", "2011-03-03")]),
    ("3rd March 2011", [("DATE", "DayMonth", "3rd March 2011", "2011-03-03")]),
    ("Sept 3, 2011",   [("DATE", "Text", "Sept 3, 2011", "2011-09-03")]),
    ("March 2011",     [("DATE", "MonthYear", "March 2011", "2011-03-01")]),

    # A MATCH THAT DENOTES NO DATE KEEPS ITS SPAN. Found-and-unparseable is a third state beyond
    # found and not found, and dropping it would report a precision the pattern did not achieve.
    ("February 30, 2011", [("DATE", "Text", "February 30, 2011", None)]),

    # --- both labels on overlapping text ------------------------------------------------------
    # THE CROSS-LABEL CASE, and the reason overlaps resolve within a label rather than across. The
    # date sits inside the term's sentence; both are true; a single greedy pass would keep the
    # longer and silently delete the other.
    ("for a period of five (5) years from January 1, 2020",
     [("DATE", "Text", "January 1, 2020", "2020-01-01"),
      ("TERM", "PeriodOf", "period of five (5) years", 5.0)]),
]

#: Every entry is a real contract phrase that a looser pattern would swallow.
NEGATIVES = [
    "for a period of time to be agreed",            # no number, no unit
    "for a period of five (5) business days",       # a unit the table does not carry
    "a period of not less than three (3) years",    # a floor, not a term
    "over a period of 12 consecutive months",       # an interval of observation
    "during the period of employment",              # no number
    "term of this Agreement",                       # a reference, not a length
    "within thirty (30) days of notice",            # a notice period with no term lead
    "the Company shall pay $30,000",                # a number and no unit
    "Section 10-15 governs the foregoing",          # a section reference, not a date
    "Exhibit 3-1 attached hereto",                  # an exhibit reference
    "the parties met in 2011",                      # a bare year is not a date
]


def extract(text):
    """One text through the extractor, reduced to what the cases assert.

    Calls extract_one() directly rather than run(), so a case is a pure function of the patterns
    and does not touch parquet.
    """
    dr.init_worker(0, dr.LABELS)
    rows = dr.extract_one(("T", text))
    out = []
    for r in rows:
        if r[1] is None:                       # sentinel
            continue
        label, label_raw, span = r[4], r[5], r[3]
        value = r[8] if label == "DATE" else r[11]     # DateValue / TermYears
        out.append((label, label_raw, span, value))
    return sorted(out)


@pytest.mark.parametrize("text,expected", CASES, ids=[c[0][:44] for c in CASES])
def test_case(text, expected):
    assert extract(text) == sorted(expected)


@pytest.mark.parametrize("text", NEGATIVES)
def test_negative(text):
    assert extract(text) == [], f"{text!r} should match nothing"


def test_span_is_a_substring_of_the_input():
    """The offset contract, at the level of one phrase rather than one corpus."""
    for text, _ in CASES:
        dr.init_worker(0, dr.LABELS)
        for r in dr.extract_one(("T", text)):
            if r[1] is None:
                continue
            assert text[r[1]:r[2]] == r[3]


def test_open_ended_carries_a_unit_and_no_number():
    """OpenEnded means unbounded, not unparsed, and TermUnit is what says so."""
    dr.init_worker(0, dr.LABELS)
    rows = [r for r in dr.extract_one(("T", "shall continue until terminated"))
            if r[5] == "OpenEnded"]
    assert len(rows) == 1
    assert rows[0][10] == "open"      # TermUnit
    assert rows[0][9] is None         # TermN
    assert rows[0][11] is None        # TermYears


def test_month_year_resolves_to_the_first_and_says_so():
    """MonthYear's day is a placeholder. LabelRaw is the only thing that reveals it."""
    dr.init_worker(0, dr.LABELS)
    rows = [r for r in dr.extract_one(("T", "March 2011")) if r[4] == "DATE"]
    assert len(rows) == 1
    assert rows[0][5] == "MonthYear"
    assert rows[0][8] == "2011-03-01"


def test_two_digit_years_pivot_at_69():
    assert dr.parse_span("1/1/68", "mdy") == "2068-01-01"
    assert dr.parse_span("1/1/69", "mdy") == "1969-01-01"


def test_no_date_is_invented_without_a_written_year():
    """The whole argument for this engine.

    A grammar-based extractor resolves "May 1 of each year" by supplying a year of its own; if that
    year comes from the clock, the same document yields different dates on different days. Nothing
    here can: no year in the text, no match.
    """
    for text in ("May 1 of each year", "the first day of each month", "on or about March 1"):
        assert [r for r in extract(text) if r[0] == "DATE"] == []
