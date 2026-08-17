"""What redaction must classify, what it must exclude, and what the exclusions remove.

Written from the cases redaction-v1's comments record as observed in reading sessions. The
extractor had no test file; the evidence was in the comments and is now executable.

THE EXCLUSIONS GET AS MUCH ATTENTION AS THE CLASSES. Page filler alone was roughly one marker in
six, which is the margin by which a redaction count built without it overstates itself -- so a test
that only checked what the classes catch would miss the largest correction the extractor makes.
"""
from __future__ import annotations

import pytest

from matcon_extract import redaction as rd

#: (text, [(LabelRaw, Span)])
CASES = [
    # --- RedactSymbol: a bracket holding only whitespace and asterisks -------------------------
    ("the fee is [***] per unit",      [("RedactSymbol", "[***]")]),
    ("the fee is [ * * * ] per unit",  [("RedactSymbol", "[ * * * ]")]),
    ("the fee is [*] per unit",        [("RedactSymbol", "[*]")]),

    # --- RedactExplicit: a bracket naming confidential treatment --------------------------------
    ("[CONFIDENTIAL TREATMENT REQUESTED]", [("RedactExplicit", "[CONFIDENTIAL TREATMENT REQUESTED]")]),
    ("[REDACTED]",                         [("RedactExplicit", "[REDACTED]")]),
    ("[CTR]",                              [("RedactExplicit", "[CTR]")]),

    # PRECEDENCE. "[CONFIDENTIAL PORTION OMITTED]" carries both vocabularies and classifies as the
    # more specific claim, which is confidential treatment rather than plain deletion.
    ("[CONFIDENTIAL PORTION OMITTED]", [("RedactExplicit", "[CONFIDENTIAL PORTION OMITTED]")]),

    # --- OmitExplicit: a bracket recording deletion ---------------------------------------------
    ("[INTENTIONALLY OMITTED]", [("OmitExplicit", "[INTENTIONALLY OMITTED]")]),
    ("[OMITTED]",               [("OmitExplicit", "[OMITTED]")]),

    # --- OmitSymbol: bullets, ellipsis characters, or three dots --------------------------------
    ("[\u2022\u2022\u2022]", [("OmitSymbol", "[\u2022\u2022\u2022]")]),
    ("[\u25cf\u25cf]",       [("OmitSymbol", "[\u25cf\u25cf]")]),
    ("[...]",                [("OmitSymbol", "[...]")]),

    # --- RedactBlank: a bracket holding only underscores ---------------------------------------
    # v1 MISSED THIS ENTIRELY. 33 spans in a 300-document sample, and moneyregex already treats the
    # same form as a redaction -- its symbol_redact matches "$____" via _{2,}. Two extractors
    # disagreeing about whether an underscore run marks a removal was a gap, not a judgement call.
    ("the price is [___] per unit",         [("RedactBlank", "[___]")]),
    ("the price is [__] per unit",          [("RedactBlank", "[__]")]),
    ("the price is [_____________] here",   [("RedactBlank", "[_____________]")]),

    # --- RedactBare: an unbracketed asterisk run, NOT in the published method --------------------
    # Kept separate so it can be measured and then retained or dropped on evidence.
    ("the amount of *** was withheld", [("RedactBare", "***")]),

    # A LINE BREAK INSIDE THE BRACKET is ordinary in a converted filing, which is why the pattern
    # runs with DOTALL and the text is never re-wrapped before matching.
    ("[CONFIDENTIAL TREATMENT\nREQUESTED]",
     [("RedactExplicit", "[CONFIDENTIAL TREATMENT\nREQUESTED]")]),
]

#: Brackets that are not redactions. A label called REDACT should mean redaction.
NEGATIVES = [
    "[1.4]",                                        # a clause number
    "[borrower]",                                   # a defined term
    "[Company]",                                    # likewise
    "see [Exhibit A]",                              # a cross-reference
    "[Remainder of page intentionally left blank]",  # page filler; conceals nothing
    "[SIGNATURE PAGE FOLLOWS]",                     # likewise
    "[NAME]",                                       # an unfilled template FIELD, not a removal
    "[DATE]",                                       # likewise
    "[ILLEGIBLE]",                                  # a conversion artifact; 238 in 300 documents
]


def extract(text):
    """One text through the extractor, reduced to (LabelRaw, Span)."""
    rd.init_worker(0)
    rows, _dropped = rd.extract_one(("T", text))
    return sorted((r[5], r[3]) for r in rows if r[1] is not None)


def dropped(text):
    rd.init_worker(0)
    _rows, d = rd.extract_one(("T", text))
    return {k: v for k, v in d.items() if v}


@pytest.mark.parametrize("text,expected", CASES, ids=[c[0][:44] for c in CASES])
def test_case(text, expected):
    assert extract(text) == sorted(expected)


@pytest.mark.parametrize("text", NEGATIVES)
def test_negative(text):
    assert extract(text) == [], f"{text!r} should classify as nothing"


# --- The three exclusions, each measured ---------------------------------------------------------

def test_page_filler_is_excluded_and_counted():
    """Roughly one marker in six, and it conceals nothing whatever.

    "[Remainder of page intentionally left blank]" sits before the execution clause of a great many
    contracts. It carries the word INTENTIONALLY, so without the filler test it classifies as an
    omission -- and a redaction count built that way overstates itself by that margin.
    """
    text = "[Remainder of page intentionally left blank]"
    assert extract(text) == []
    assert dropped(text) == {"filler": 1}


def test_a_genuine_omission_survives_the_filler_test():
    """The filler rule keys on LEFTBLANK, not on the word the two forms share."""
    assert extract("[INTENTIONALLY OMITTED]") == [("OmitExplicit", "[INTENTIONALLY OMITTED]")]


def test_the_opening_legend_is_excluded_and_counted():
    """A filing explains its convention by QUOTING the marker.

    "...such excluded information is indicated by [***]" describes a marker rather than standing
    where content was removed. Recognised by the verb to its left, not by the bracket.
    """
    text = "such excluded information is indicated by [***]"
    assert extract(text) == []
    assert dropped(text) == {"legend": 1}


def test_a_marker_in_running_text_is_not_a_legend():
    assert extract("the fee is [***] per unit") == [("RedactSymbol", "[***]")]


def test_a_drawn_rule_is_excluded_and_counted():
    """A row of asterisks bordering a seal occupies its whole LINE. A redaction never does.

    The test is the line rather than the length, because a genuine "[*****]" is legitimate and a
    five-asterisk divider is not.
    """
    text = "above\n*****\nbelow"
    assert extract(text) == []
    assert dropped(text) == {"line_rule": 1}


def test_an_asterisk_run_inside_a_sentence_survives():
    assert extract("the amount of *** was withheld") == [("RedactBare", "***")]


# --- Structural ----------------------------------------------------------------------------------

def test_ctr_does_not_match_inside_a_word():
    """WHITESPACE IS COLLAPSED, NOT REMOVED, and this is why.

    Removing it destroys word boundaries, so \\bCTR\\b matches inside eleCTRonically and
    "[electronically]" -- an ordinary bracketed word -- classifies as a confidential-treatment
    marker.
    """
    assert extract("[electronically]") == []
    assert extract("[ELECTRONICALLY]") == []


def test_a_bare_run_inside_a_bracket_is_not_emitted_twice():
    """Brackets are taken first and their extents recorded. Without that, "[***]" yields both a
    RedactSymbol and a RedactBare at overlapping offsets under two different classes."""
    rows = extract("the fee is [***] per unit")
    assert len(rows) == 1
    assert rows[0][0] == "RedactSymbol"


def test_offsets_index_the_raw_text():
    """The published version upper-cases and collapses whitespace BEFORE matching. Both change
    string length, so an offset taken afterwards indexes a string that no longer exists. Only the
    matched substring is normalised here, and only for classification."""
    text = "the fee is [ * * * ] per unit"
    rd.init_worker(0)
    rows, _ = rd.extract_one(("T", text))
    for r in rows:
        if r[1] is None:
            continue
        assert text[r[1]:r[2]] == r[3]


def test_the_class_set_has_not_grown_silently():
    """CLASSES is what a consumer pivots on. A new one appearing without a version bump would
    change the shape of every downstream aggregation."""
    assert rd.CLASSES == ("RedactExplicit", "RedactSymbol", "RedactBlank", "OmitExplicit",
                          "OmitSymbol", "RedactBare")


def test_filler_is_counted_apart_from_an_ordinary_bracket():
    """v1 POOLED THEM. Both paths returned None from classify(), so page filler -- which the rule
    RECOGNISES and chooses to drop -- was counted with "[1.4]" and "[Company]", which were never
    redactions. On the sample that bucket held 17,052 brackets and no reader could tell what it
    was made of. The docstring claims filler is roughly one marker in six; nothing could check it.
    """
    assert dropped("[SIGNATURE PAGE FOLLOWS]") == {"filler": 1}
    assert dropped("[1.4]") == {"not_a_redaction": 1}
    assert dropped("[ILLEGIBLE]") == {"not_a_redaction": 1}


def test_a_template_field_is_not_a_removal():
    """THE LINE v2 DRAWS, stated because a referee could poke at it. An underscore blank marks a
    REMOVAL and is emitted; a named placeholder marks a FIELD TO COMPLETE and is not. Both are the
    unfilled-template idiom, and only one of them says something was taken out."""
    assert extract("Attention: [___]") == [("RedactBlank", "[___]")]
    assert extract("Attention: [NAME]") == []


def test_every_class_is_reachable():
    """A class nobody can produce is a class that should not be documented."""
    produced = set()
    for text, _ in CASES:
        produced.update(lr for lr, _span in extract(text))
    assert produced == set(rd.CLASSES)
