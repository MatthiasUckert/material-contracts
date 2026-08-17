"""What moneyregex must match, what it must decline, and what each match must parse to.

Written from the cases moneyregex-v6's own comments record as measured -- each version bump names
the span that caused it, and every one of those is a case here. The extractor had no test file; the
evidence was in the comments and is now executable.

The NEGATIVES matter as much as the cases. Requiring a figure beside a currency marker is what
separates an amount from a denomination, and it is why this arm returns fewer spans than the
transformer while covering more of the cases that carry a number.
"""
from __future__ import annotations

import re
from decimal import Decimal

import pytest

from matcon_extract import _io, moneyregex as mr

#: (text, [(LabelRaw, Span, Amount, Currency)]). Amount is the string that crosses the seam.
CASES = [
    # --- the symbol family --------------------------------------------------------------------
    ("$5,000,000",  [("symbol_amount", "$5,000,000", "5000000", "USD")]),
    ("US$10,000",   [("symbol_amount", "US$10,000", "10000", "USD")]),
    ("U.S.$10,000", [("symbol_amount", "U.S.$10,000", "10000", "USD")]),

    # PREFIXED SIGNS RESOLVE TO THEIR OWN CODE. Only the unmarked dollar falls through to USD,
    # which in an SEC filing is the domestic one -- an assumption, and it reaches nothing else.
    ("R$1.500.000", [("symbol_amount", "R$1.500.000", "1500000", "BRL")]),
    ("C$25,000",    [("symbol_amount", "C$25,000", "25000", "CAD")]),
    ("HK$1,000",    [("symbol_amount", "HK$1,000", "1000", "HKD")]),
    ("\u00a35,000", [("symbol_amount", "\u00a35,000", "5000", "GBP")]),

    # WHITESPACE BETWEEN MARKER AND FIGURE IS ROUTINE in converted filings. A single optional space
    # misses "$  5,000" outright.
    ("$  5,000", [("symbol_amount", "$  5,000", "5000", "USD")]),

    # --- the scale word, which must be inside the span ------------------------------------------
    # "$30 million" parses to thirty without it: six orders of magnitude, and thirty is a perfectly
    # plausible contract value, so nothing downstream would flag it.
    ("$30 million",    [("symbol_scaled", "$30 million", "30000000", "USD")]),
    ("$25.0 million",  [("symbol_scaled", "$25.0 million", "25000000", "USD")]),
    ("$5mm",           [("symbol_scaled", "$5mm", "5000000", "USD")]),

    # THE SCALE WORD WAS ONCE ONLY ATTACHED TO THE DOLLAR SIGN, so "NOK 23 million" matched
    # iso_amount, kept "NOK 23" and parsed to twenty-three -- at the BOTTOM of the distribution,
    # where a magnitude check looks for errors at the top.
    ("NOK 23 million", [("iso_scaled", "NOK 23 million", "23000000", "NOK")]),
    ("EUR 5.5 mill",   [("iso_scaled", "EUR 5.5 mill", "5500000", "EUR")]),

    # --- ISO codes and spelt names --------------------------------------------------------------
    # A symbol-only arm returns no money AT ALL for a contract with a foreign counterparty.
    ("EUR 11,848,000", [("iso_amount", "EUR 11,848,000", "11848000", "EUR")]),
    ("USD9,750,000",   [("iso_amount", "USD9,750,000", "9750000", "USD")]),
    ("RMB10,000,000",  [("iso_amount", "RMB10,000,000", "10000000", "CNY")]),
    ("Euro 6 million", [("name_scaled", "Euro 6 million", "6000000", "EUR")]),

    # --- redactions, which are why this arm exists ----------------------------------------------
    # A denomination with the figure withheld. Returning zero, or dropping the row, would erase
    # exactly the observation that matters: the amounts a filer withholds are systematically the
    # commercially material ones.
    ("$[***]", [("symbol_redact", "$[***]", None, "USD")]),
    ("$**",    [("symbol_redact", "$**", None, "USD")]),
    ("$TBD",   [("symbol_redact", "$TBD", None, "USD")]),
    ("$____",  [("symbol_redact", "$____", None, "USD")]),
    ("a minimum market price of $ per share", [("symbol_bare", "$", None, "USD")]),

    # --- conversion damage ----------------------------------------------------------------------
    # A euro amount whose symbol became a capital E. Grouped thousands are REQUIRED: without them
    # "E12" matches an exhibit number and half the alphabet soup in a filing header.
    ("E185,000,000", [("euro_letter", "E185,000,000", "185000000", "EUR")]),

    # --- words ----------------------------------------------------------------------------------
    ("5 million Dollars",  [("word_scaled", "5 million Dollars", "5000000", "USD")]),
    ("5,000,000 Dollars",  [("amount_word", "5,000,000 Dollars", "5000000", "USD")]),
    # The n/100 tail is how a contract writes cents in a words-only figure. Dropping it would round
    # every one of them down.
    ("SIXTY-ONE THOUSAND NINETY AND 90/100 Dollars",
     [("words_only", "SIXTY-ONE THOUSAND NINETY AND 90/100 Dollars", "61090.9", "USD")]),

    # --- the separator rule ---------------------------------------------------------------------
    # THE SUB-CENT CASE. Read as European grouping "0.0001" becomes 1 -- a per-share price of a
    # tenth of a cent recorded as one dollar, a factor of a thousand with no symptom. "par value
    # $0.0001 per share" is boilerplate in these filings.
    ("par value $0.0001 per share", [("symbol_amount", "$0.0001", "0.0001", "USD")]),
    ("$1.500,00",  [("symbol_amount", "$1.500,00", "1500", "USD")]),
    ("$1.234.567", [("symbol_amount", "$1.234.567", "1234567", "USD")]),
]

#: Real contract text a looser pattern would swallow.
NEGATIVES = [
    "14.13 Requirements of Law",        # a section number; the largest false-positive family
    "Section 10.25 of the Agreement",   # likewise
    "payable in U.S. Dollars",          # a denomination, not an amount
    "amounts stated in Dollars",        # likewise
    "the Dollar has weakened",          # likewise
    "Exhibit 10.15 attached hereto",    # an exhibit reference
    "a distance of 5 mm",               # no currency marker, so the scale word cannot fire
    "the parties met in 2011",          # a bare number
]


def extract(text):
    """One text through the extractor, reduced to what the cases assert."""
    mr.init_worker(0)
    out = []
    for r in mr.extract_one(("T", text)):
        if r[1] is None:                       # sentinel
            continue
        out.append((r[5], r[3], r[8], r[9]))   # LabelRaw, Span, Amount, Currency
    return sorted(out, key=lambda x: (x[1], x[0]))


@pytest.mark.parametrize("text,expected", CASES, ids=[c[0][:44] for c in CASES])
def test_case(text, expected):
    assert extract(text) == sorted(expected, key=lambda x: (x[1], x[0]))


@pytest.mark.parametrize("text", NEGATIVES)
def test_negative(text):
    assert extract(text) == [], f"{text!r} should match nothing"


def test_a_denomination_without_a_figure_is_not_an_amount():
    """The transformer tags "U.S. Dollars", "Dollars" and "the Dollar" as money roughly seven
    hundred times across the sample, and none of them is an amount. Requiring a figure is what
    separates the two."""
    for text in ("U.S. Dollars", "Dollars", "the Dollar", "in Euros"):
        assert extract(text) == []


def test_a_redacted_amount_keeps_its_currency_and_has_no_number():
    """A currency with a null amount is a THIRD STATE, not a failure. Returning zero would put a
    withheld figure at the bottom of every distribution it enters."""
    for text in ("$[***]", "$**", "$TBD"):
        rows = extract(text)
        assert len(rows) == 1
        assert rows[0][2] is None       # Amount
        assert rows[0][3] == "USD"      # Currency


def test_the_amount_crosses_the_seam_as_text():
    """A Decimal becomes a float in pandas, and a contract value of 9,752,233.001 is exactly the
    kind of number that does not survive that intact."""
    rows = extract("$9,752,233.001")
    assert rows[0][2] == "9752233.001"
    assert isinstance(rows[0][2], str)


def test_separator_conventions():
    """The regex cannot decide this and must not try; the parser decides per span.

    Compared as Decimal against a Decimal built from a string. Comparing against a float would
    reintroduce exactly the precision loss the string seam exists to prevent.
    """
    assert mr.num_from_text("1,234,567.89") == Decimal("1234567.89")
    assert mr.num_from_text("1.500,00") == Decimal("1500.00")
    assert mr.num_from_text("1.234.567") == Decimal("1234567")
    assert mr.num_from_text("0.0001") == Decimal("0.0001")
    # One comma with a three-digit tail is grouping.
    assert mr.num_from_text("5,000") == Decimal("5000")


def test_the_decimal_comma_branch_is_unreachable_through_num():
    r"""A DEAD BRANCH, asserted so it stays visible.

    num_from_text()'s docstring says one comma with a tail that is not three digits reads as a
    DECIMAL comma. NUM cannot produce that: its comma alternative requires \d{3} after every
    comma, and its European alternatives all carry a dot as well, which sends the span down the
    both-separators branch instead. So "5,25" is matched by the plain \d+ alternative as "5" and
    the comma is never seen.

    This produces no wrong answer -- a bare "5,25" beside a currency marker does not occur in the
    corpus -- but it is a rule stated in prose that the code cannot reach. Recorded rather than
    fixed: changing NUM would move spans in an existing store to serve a case that never arises.
    """
    assert mr.num_from_text("5,25") == Decimal("5")


def test_words_parse_with_their_cents():
    assert mr.num_from_words("SIXTY-ONE THOUSAND NINETY AND 90/100") == Decimal("61090.90")
    assert mr.num_from_words("two million") == Decimal("2000000")
    assert mr.num_from_words("five hundred thousand") == Decimal("500000")


def test_overlaps_resolve_leftmost_not_longest():
    """The rule this label uses, asserted so the port cannot drift back to the other one.

    moneyregex-v6's docstring said longest-first and its code was leftmost-first. The code is what
    an existing store was built with, so the code is what is preserved.
    """
    compiled = [("short", re.compile(r"AAA"), None),
                ("long", re.compile(r"AA BBBBBBBBBB"), None)]
    text = "xAAA BBBBBBBBBB"
    assert [n for _s, _e, n in _io.keep_leftmost(text, compiled)] == ["short"]
    assert [n for _s, _e, n in _io.keep_longest(text, compiled)] == ["long"]
