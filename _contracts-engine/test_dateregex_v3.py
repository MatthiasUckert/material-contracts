#!/usr/bin/env python3
"""Tests for extract_dateregex_v3.py -- the pattern table, the parsers and the invariants.

Lives beside the extractors in contracts-engine/ and defaults to its siblings, so it can be run
from the project root without repeating their paths. THE ENGINE VENV, not `uv run`: contracts-engine
is its own uv project, `uv run` from the repository root resolves the ROOT environment and finds no
pandas, and _NER.R calls contracts-engine/.venv/bin/python directly. Matching that is what makes a
test passing here mean the pipeline will run it.

    contracts-engine/.venv/bin/python contracts-engine/test_dateregex_v3.py

    contracts-engine/.venv/bin/python contracts-engine/test_dateregex_v3.py \\
        --sample 2_output/04A-EntityExtract/sample_text.parquet --n-docs 500

A bare filename in --engine or --v2 resolves NEXT TO THIS SCRIPT rather than in the working
directory; anything with a separator in it is taken as given. That is the difference between a test
that runs from the project root and one that only runs from the folder it lives in.

FOUR THINGS, AND THEY ANSWER DIFFERENT QUESTIONS.

CASES asserts what each pattern should match and what it should parse to. It is the regression
suite: a pattern edit that breaks one of these is visible immediately, and a pattern edit that
SHOULD change one of these means editing the expectation deliberately rather than discovering later
that a number moved.

NEGATIVES asserts what must NOT match. A regex is judged as much by what it declines, and the
entries here are the near misses that a looser pattern would swallow -- "period of time",
"period of five business days", "period of not less than three (3) years". Every one of them is a
real contract phrase and none is a stated term.

REGRESSION compares v3's DATE rows against v2's on the same text. v3 adds a label and changes how
overlaps resolve; neither should move a single date. If this fails, the cross-label change leaked
into the date family and that is a defect rather than a design choice.

SURVEY runs over real documents and prints the commonest span each pattern matched. It asserts
nothing. It is how a false positive is FOUND rather than confirmed: the cases above test the
phrases someone thought of, and the survey shows the phrases the corpus actually contains. A
pattern whose top spans are all one boilerplate sentence is a pattern measuring that sentence.

Exit code is 0 only if CASES, NEGATIVES, INVARIANTS and (when asked for) REGRESSION all pass.
"""
import argparse
import importlib.util
import sys
from collections import Counter, defaultdict
from pathlib import Path


# -- What each pattern should match, and what it should parse to ---------------------------------
# (text, [(Label, LabelRaw, Span, Value)]) where Value is DateValue for DATE and TermYears for
# TERM. An empty list means the text should produce nothing at all.
#
# ONE EXPECTATION PER ROW OF THE TABLE, in the order the extractor emits them (DATE before TERM,
# each sorted by position), so a test failure names the row rather than the set.

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
    ("3/3/2011",   [("DATE", "Slash", "3/3/2011", "2011-03-03")]),
    ("3/3/11",     [("DATE", "SlashShort", "3/3/11", "2011-03-03")]),
    ("2011-03-03", [("DATE", "ISO", "2011-03-03", "2011-03-03")]),
    ("2011/03/03", [("DATE", "YearFirst", "2011/03/03", "2011-03-03")]),
    ("3.3.2011",   [("DATE", "European", "3.3.2011", "2011-03-03")]),
    ("3rd March 2011", [("DATE", "DayMonth", "3rd March 2011", "2011-03-03")]),
    ("Sept 3, 2011",   [("DATE", "Text", "Sept 3, 2011", "2011-09-03")]),
    ("March 2011", [("DATE", "MonthYear", "March 2011", "2011-03-01")]),

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


# -- What must NOT match -------------------------------------------------------------------------
# Every entry is a real contract phrase that a looser pattern would swallow. A regex is judged as
# much by what it declines, and these are the near misses.

NEGATIVES = [
    "for a period of time to be agreed",              # no number, no unit
    "for a period of five (5) business days",         # a unit the table does not carry
    "a period of not less than three (3) years",      # a floor, not a term
    "over a period of 12 consecutive months",         # an interval of observation
    "during the period of employment",                # no number
    "term of this Agreement",                         # a reference, not a length
    "within thirty (30) days of notice",              # a notice period with no term lead
    "the Company shall pay $30,000",                  # a number and no unit
    "Section 10-15 governs the foregoing",            # a section reference, not a date
    "Exhibit 3-1 attached hereto",                    # an exhibit reference
    "the parties met in 2011",                        # a bare year is not a date
]


# -- Invariants that must hold on ANY input ------------------------------------------------------

def check_invariants(rows, text_by_doc, name):
    """Structural facts that hold whatever the patterns are. Returns a list of failure strings.

    These are the checks worth running on REAL text rather than on fixtures: a fixture exercises
    the phrase someone thought of, and an invariant fails on the document nobody did.
    """
    bad = []
    seen_docs = set()
    per_label = defaultdict(list)

    for r in rows:
        doc, start, stop, span, label, raw = r[0], r[1], r[2], r[3], r[4], r[5]
        date_v, term_n, term_u, term_y = r[8], r[9], r[10], r[11]
        seen_docs.add(doc)

        if start is None:                       # the sentinel row for a document with no match
            if span is not None or label is not None:
                bad.append(f"{name}: sentinel row for {doc} carries a span or a label")
            continue

        # THE OFFSET DISCIPLINE. text[Start:Stop] must BE the span; anything else means the offsets
        # index something other than the text they were measured on.
        src = text_by_doc.get(doc)
        if src is not None and src[start:stop] != span:
            bad.append(f"{name}: {doc} @{start}: text slice != Span "
                       f"({src[start:stop]!r} vs {span!r})")

        if label not in ("DATE", "TERM"):
            bad.append(f"{name}: {doc} @{start}: unexpected Label {label!r}")
        per_label[(doc, label)].append((start, stop))

        if label == "DATE" and (term_n is not None or term_u is not None or term_y is not None):
            bad.append(f"{name}: {doc} @{start}: DATE row carries term columns")
        if label == "TERM" and date_v is not None:
            bad.append(f"{name}: {doc} @{start}: TERM row carries a DateValue")

        # A TERM WITH A YEAR VALUE MUST HAVE A NUMBER AND A UNIT, and the open-ended form must have
        # neither a number nor a value -- that is what distinguishes "unbounded" from "unparsed".
        if label == "TERM":
            if raw == "OpenEnded":
                if term_u != "open" or term_n is not None or term_y is not None:
                    bad.append(f"{name}: {doc} @{start}: OpenEnded row is not (None, 'open', None)")
            elif term_y is not None and (term_n is None or term_u is None):
                bad.append(f"{name}: {doc} @{start}: TermYears without TermN or TermUnit")

    # WITHIN a label, no two spans may overlap; ACROSS labels they may and must be allowed to.
    for (doc, label), spans in per_label.items():
        spans.sort()
        for (s1, e1), (s2, e2) in zip(spans, spans[1:]):
            if s2 < e1:
                bad.append(f"{name}: {doc}: overlapping {label} spans {s1}-{e1} and {s2}-{e2}")

    missing = set(text_by_doc) - seen_docs
    if missing:
        bad.append(f"{name}: {len(missing)} document(s) produced no row at all")
    return bad


# -- Harness -------------------------------------------------------------------------------------

def beside(path):
    """A bare filename resolves next to THIS script; anything with a separator is taken as given."""
    p = Path(path)
    return str(p if p.parent != Path(".") else Path(__file__).resolve().parent / p.name)


def load_engine(path):
    """Import an extractor by path, so a test can hold two versions at once."""
    path = beside(path)
    if not Path(path).exists():
        raise SystemExit(f"no extractor at {path}")
    spec = importlib.util.spec_from_file_location(f"eng_{abs(hash(path))}", path)
    mod = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = mod
    spec.loader.exec_module(mod)
    mod._init_worker(0)
    return mod


def observed(mod, text):
    """The rows one text produces, reduced to what the cases assert."""
    out = []
    for r in mod.extract_one(("t", text)):
        if r[1] is None:
            continue
        value = r[8] if r[4] == "DATE" else r[11]
        out.append((r[4], r[5], r[3], value))
    return out


def run_cases(mod):
    fails = []
    for text, want in CASES:
        got = observed(mod, text)
        if got != want:
            fails.append(f"  {text!r}\n     want {want}\n     got  {got}")
    return fails


def run_negatives(mod):
    fails = []
    for text in NEGATIVES:
        got = observed(mod, text)
        if got:
            fails.append(f"  {text!r}\n     want nothing\n     got  {got}")
    return fails


def run_regression(new, old, texts):
    """v3's DATE rows against v2's, on the same text.

    v3 adds a label and changes how overlaps resolve. Neither should move a single date, and this
    is the check that says so rather than the assumption that says so.
    """
    fails = []
    for i, text in enumerate(texts):
        a = [(r[1], r[2], r[3], r[5], r[8]) for r in new.extract_one(("t", text))
             if r[1] is not None and r[4] == "DATE"]
        b = [(r[1], r[2], r[3], r[5], r[8]) for r in old.extract_one(("t", text))
             if r[1] is not None]
        if a != b:
            fails.append(f"  text #{i}: v3 DATE rows differ from v2\n     v3 {a}\n     v2 {b}")
    return fails


def survey(mod, texts_by_doc, top_n):
    """The commonest span each pattern matched, over real documents. Asserts nothing.

    THIS IS HOW A FALSE POSITIVE IS FOUND. The cases above test the phrases someone thought of;
    this shows the phrases the corpus contains. A pattern whose top spans are all one boilerplate
    sentence is a pattern measuring that sentence.
    """
    spans = defaultdict(Counter)
    values = defaultdict(Counter)
    docs = defaultdict(set)
    rows = []
    for doc, text in texts_by_doc.items():
        rs = mod.extract_one((doc, text))
        rows.extend(rs)
        for r in rs:
            if r[1] is None:
                continue
            key = (r[4], r[5])
            spans[key][" ".join(r[3].split()).lower()] += 1
            docs[key].add(doc)
            if r[4] == "TERM" and r[11] is not None:
                values[key][r[11]] += 1

    print(f"\n{'=' * 96}\nSURVEY over {len(texts_by_doc)} document(s)\n{'=' * 96}")
    for key in sorted(spans, key=lambda k: (-sum(spans[k].values()), k)):
        label, raw = key
        n = sum(spans[key].values())
        print(f"\n{label:5s} {raw:14s} {n:7,d} span(s) in {len(docs[key]):6,d} doc(s)")
        for span, c in spans[key].most_common(top_n):
            print(f"        {c:6,d}  {span[:70]!r}")
        if values[key]:
            head = ", ".join(f"{v:g}y x{c}" for v, c in values[key].most_common(6))
            print(f"        values: {head}")
    return rows


def load_texts(path, id_col, text_col, n):
    import pyarrow.dataset as pads
    tbl = (pads.dataset(path, format="parquet").to_table(columns=[id_col, text_col]).to_pandas())
    if n > 0:
        tbl = tbl.head(n)
    return {d: t for d, t in zip(tbl[id_col], tbl[text_col]) if isinstance(t, str)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--engine", default="extract_dateregex_v3.py",
                    help="extractor under test; a bare name resolves beside this script")
    ap.add_argument("--v2", default="extract_dateregex.py",
                    help="previous version, for the DATE regression; '' to skip")
    ap.add_argument("--sample", default=None,
                    help="parquet of real documents, for the survey and the sample invariants")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--n-docs", type=int, default=500, help="documents surveyed (0 = all)")
    ap.add_argument("--top", type=int, default=8, help="spans listed per pattern in the survey")
    args = ap.parse_args()

    mod = load_engine(args.engine)
    print(f"engine: {mod.ENGINE}:{mod.MODEL}")
    ok = True

    fails = run_cases(mod)
    print(f"\nCASES      {len(CASES) - len(fails):3d}/{len(CASES):3d} passed")
    for f in fails:
        print(f)
    ok &= not fails

    fails = run_negatives(mod)
    print(f"NEGATIVES  {len(NEGATIVES) - len(fails):3d}/{len(NEGATIVES):3d} passed")
    for f in fails:
        print(f)
    ok &= not fails

    fixtures = {f"case{i}": t for i, (t, _w) in enumerate(CASES)}
    fixture_rows = [r for d, t in fixtures.items() for r in mod.extract_one((d, t))]
    bad = check_invariants(fixture_rows, fixtures, "fixtures")
    print(f"INVARIANTS {'passed' if not bad else str(len(bad)) + ' FAILED'} (fixtures)")
    for b in bad:
        print(f"  {b}")
    ok &= not bad

    if args.v2 and Path(beside(args.v2)).exists():
        old = load_engine(args.v2)
        texts = [t for t, _w in CASES] + NEGATIVES
        fails = run_regression(mod, old, texts)
        print(f"REGRESSION {len(texts) - len(fails):3d}/{len(texts):3d} texts match"
              f"  (vs {old.MODEL})")
        for f in fails:
            print(f)
        ok &= not fails

    if args.sample:
        texts = load_texts(args.sample, args.id_col, args.text_col, args.n_docs)
        rows = survey(mod, texts, args.top)
        bad = check_invariants(rows, texts, "sample")
        print(f"\nINVARIANTS {'passed' if not bad else str(len(bad)) + ' FAILED'} (sample)")
        for b in bad[:20]:
            print(f"  {b}")
        ok &= not bad

    print(f"\n{'ALL PASSED' if ok else 'FAILURES ABOVE'}")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
