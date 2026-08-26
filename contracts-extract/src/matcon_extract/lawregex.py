"""Governing-law clauses, as spans, with offsets.

Emits one label. `LabelRaw` carries which cue opened the clause, which is what downstream
classification pivots on.

WHY THIS IS AN EXTRACTOR AND NOT A RULE
Governing law was found backwards. 04B2's geo_law() took every place name the gazetteer resolved to
a State or Country, read the 120 characters BEFORE it, and asked whether that window carried cue
language. Two things follow from that shape and neither is good.

  IT IS PROXIMITY, NOT CONTAINMENT. "...governed by New York law. Delaware corporations shall..."
  puts Delaware within 120 characters of a cue, and Delaware is not the jurisdiction. Asking
  instead whether a place sits INSIDE a governing-law clause cannot make that mistake, and it
  disposes of the window parameter rather than tuning it.

  IT NEEDS THE TEXT, TWICE. Once for the window, and once more for the self-referential arm, which
  uppercased every document whole to test for two phrase lists. At corpus scale that is hours of
  reading, which is why 04D runs with Cues = FALSE -- and with the cues off, the governing-law
  columns DO NOT APPEAR AT ALL. Not zero, not missing: absent. It is the largest hole in the
  released entity variables and it is a consequence of where the rule lives, not of the rule.

WHAT THIS EMITS, AND WHAT IT DELIBERATELY DOES NOT
A LAW span covers the clause. It carries NO EXTRAS. The jurisdiction is not resolved here.

That is the whole design. gazetteer.py already resolves place names -- through a three-tier
admission gate, a 181,810-row lookup and an NParent ambiguity count -- and every one of those spans
is already in the store. Resolving a second Delaware inside this module would be a worse copy of
that work and would leave two answers to reconcile. So the jurisdiction arrives from a JOIN:

    LAW  doc01  [ 24 .. 96 ]  "governed by ... State of Delaware"  GovernedBy
    GPE  doc01  [ 88 .. 96 ]  "Delaware"                           US State
                    ^^^^^^ inside the LAW span

and the rule downstream is three cases, all answerable from the store with no text at all:

    a LAW span with a GPE span inside it        -> named, jurisdiction is that GPE's GeoUnit
    a LAW span whose LabelRaw is SelfReferential -> self-referential
    no LAW span                                  -> none found

THE SELF-REFERENTIAL FORM IS A CLASS, NOT AN ABSENCE
"governed by the laws of the State in which the Premises are located" specifies the law perfectly
well and names no place. 04B2 kept it as its own outcome for exactly that reason -- recording
nothing would count a real contract term as a coverage gap, and it is 11.9% of leases. Here it is a
LabelRaw like any other, so it is distinguished by the PRESENCE of a span rather than by a scan of
the document, and geo_law()'s whole-document uppercase disappears.

CUES OPEN THE CLAUSE; THE REACH CLOSES IT
Each pattern is a cue phrase followed by a bounded run that stops at the first sentence boundary or
at REACH characters, whichever comes first. Contract sentences are long, so neither bound alone is
enough: a pure sentence boundary swallows the following clause when the drafter used semicolons
sparingly, and a pure character count truncates "the laws of the Commonwealth of Massachusetts" when
the cue sat early.

REACH IS DECLARED, NOT CHOSEN. 120 is a starting value and belongs in SPEC so that changing it moves
the model's identity. What it should be is a question for the 4,398-document sample, where this
module's answers can be set beside geo_law()'s -- the same way ChunkSize came from a measured sweep
rather than from taste. Until that runs, treat the number as provisional and the comparison as the
next piece of work.

OVERLAPS RESOLVE LONGEST-FIRST
Two cues can open at nearly the same place -- "governing law" immediately followed by "governed by
and construed in accordance with" -- and they describe one clause, not two. keep_longest() is the
right resolver here for the same reason dateregex uses it: these are alternative readings of the
same text, so the longest reading wins outright. moneyregex's keep_leftmost() would be wrong,
because a governing-law clause is not a figure read in document order.

THE CUE LISTS COME FROM 04B2 UNCHANGED
Seven opening cues and four self-referential phrases, ported verbatim from .geo_law_cue and
.geo_law_self so that a difference between this module's output and geo_law()'s is a difference of
MECHANISM rather than of vocabulary. Widening them is a separate decision to be argued on evidence.
"""
from __future__ import annotations

import re
import sys

from . import _io

MODEL = "lawregex-v1"       # THE version. The filename carries none; see _io.spec_hash().
NAME = "lawregex"
LABELS = ("LAW",)
EXTRAS = ()

DEFAULT_TIMEOUT = 0
DEFAULT_CHUNK_SIZE = 64


# 1. The clause ---------------------------------------------------------------------------------

#: How far past the cue a clause may run. See "REACH IS DECLARED, NOT CHOSEN" above: this is a
#: starting value to be settled against the sample, and it sits in SPEC so a change moves the model.
REACH = 120

#: What stops a clause early. A sentence boundary, a semicolon, or a newline -- whichever the
#: drafter reached for. The character class is what bounds the run; REACH bounds it if none occurs.
STOP = r"[^.;\n]"

#: The opening cues, in the order they are tried. Ported verbatim from 04B2's .geo_law_cue.
#:
#: ORDER IS NOT PRECEDENCE. keep_longest() decides between overlapping matches, so a longer cue does
#: not need to be listed first to win. The order is the order 04B2 wrote them, kept so the two lists
#: can be read side by side.
CUES = (
    ("GovernedByConstrued", r"governed\s+by\s+and\s+construed"),
    ("GovernedBy",          r"governed\s+by"),
    ("LawsOfState",         r"laws\s+of\s+the\s+state\s+of"),
    ("LawsOfCommonwealth",  r"laws\s+of\s+the\s+commonwealth\s+of"),
    ("ConstruedAccordance", r"construed\s+in\s+accordance\s+with\s+the\s+laws"),
    ("GoverningLaw",        r"governing\s+law"),
    ("SubmitJurisdiction",  r"submit\s+to\s+the\s+jurisdiction\s+of"),
)

#: The self-referential phrases. Ported verbatim from 04B2's .geo_law_self.
#:
#: A clause carrying one of these SPECIFIES the governing law and names no place. It is recognised
#: inside an already-matched clause rather than as a cue of its own, because "the State in which"
#: is not a governing-law cue on its own -- it appears in indemnity and notice provisions too.
SELF = (
    r"state\s+in\s+which",
    r"jurisdiction\s+in\s+which",
    r"state\s+where\s+the",
    r"laws\s+of\s+the\s+jurisdiction\s+in\s+which",
)

#: One compiled pattern per cue: the cue, then a bounded run to a sentence boundary or REACH.
#:
#: THE RUN TAKES AS MUCH AS IT CAN UP TO THE CEILING and stops at the first character STOP excludes,
#: which is what makes a full stop bind tighter than the character count without a second pass.
#:
#: THE THIRD ELEMENT IS THE READING, and keep_longest() requires it: its tables are
#: (name, compiled, reading) triples because dateregex needs to know HOW to parse what a pattern
#: matched. Nothing is parsed here, so it is None -- present because the resolver's signature is
#: shared, not because this module has a use for it.
PATTERNS = tuple(
    (name, re.compile(cue + r"(?:" + STOP + r"){0," + str(REACH) + r"}", re.I), None)
    for name, cue in CUES
)

RX_SELF = re.compile("|".join(SELF), re.I)


# THE CONTEXT WIDTH IS PART OF THIS EXTRACTOR'S IDENTITY. Every rule that opens a document does so
# for the same reason -- the characters around a span -- so storing them at extraction is what turns
# three text-reading rules into three column reads. It sits in SPEC because a store holding rows cut
# at 160 beside rows cut at 0 under one model tag would give a rule full context from some spans and
# none from others, with nothing to say which. Changing the width therefore moves the hash and
# clears the store: expensive, and correct.
SPEC = {
    "cue": _io.DEFAULT_CUE,
    "cues": CUES,
    "self": SELF,
    "reach": REACH,
    "stop": STOP,
}

# THE EMITTER IS BUILT AT IMPORT AND THE WIDTHS ARE MODULE STATE, NOT PER-DOCUMENT STATE. A worker's
# globals die with the process, so anything set per document on this object would be lost across a
# pool boundary -- which is why row() takes the text explicitly rather than the emitter holding it.
EMIT = _io.Emitter(MODEL, EXTRAS, _io.DEFAULT_CUE, _io.DEFAULT_CUE)


# 2. Finding ------------------------------------------------------------------------------------

def classify(span):
    """Which class a matched clause belongs to, given the cue that opened it.

    The cue name is the class UNLESS the clause turns out to be self-referential, which outranks it:
    "governed by the laws of the State in which the Premises are located" opens on GovernedBy and is
    not a named jurisdiction. Downstream reads LabelRaw alone, so the distinction has to live there
    rather than in a second column.

    :param span: the matched clause.
    :return: "SelfReferential" where the clause names no place by construction, else None so the
        caller keeps the cue name.
    """
    return "SelfReferential" if RX_SELF.search(span) else None


def find_clauses(text):
    """Every governing-law clause in one text.

    Every cue is matched, then the family's own resolver decides between overlaps -- so a clause
    that two cues both open is emitted once. keep_longest() is the right rule here for dateregex's
    reason: these are alternative readings of the same text, not figures read in document order.

    THE TRAILING TRIM HAPPENS AFTER RESOLUTION, NOT BEFORE. The bounded run can end on
    whitespace,
    and a span ending in a space would put Stop one past the clause. Trimming first would mean
    keep_longest() resolving overlaps on different offsets from the ones emitted, so the two must
    stay in this order.

    :param text: the document.
    :return: list of (start, stop, span, class) sorted by position.
    """
    out = []
    for s, e, name in _io.keep_longest(text, PATTERNS):
        span = text[s:e].rstrip()
        if not span:
            continue
        out.append((s, s + len(span), span, classify(span) or name))
    return out


# 3. The pass ------------------------------------------------------------------------------------

def init_worker(timeout, want=None):
    """Per-worker setup.

    `want` is accepted and ignored so every module's initialiser has one signature.
    """
    _io.install_alarm(timeout)


def extract_one(args):
    """Rows for one document."""
    docid, text = args
    rows = []

    if isinstance(text, str) and text.strip():
        try:
            _io.start_alarm()
            for s, e, span, klass in find_clauses(text):
                rows.append(EMIT.row(docid, s, e, span, "LAW", klass, None, text))
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
    path = _io.write_output(frame, out_dir, MODEL)

    n_hit = int(frame["Start"].notna().sum())
    n_doc = int(frame.loc[frame["Start"].notna(), "DocID"].nunique())
    print(f"{len(ids)} doc(s) -> {n_hit} clause(s) in {n_doc} doc(s)  "
          f"[{_io.ENGINE}:{MODEL}] -> {path}")
    mix = frame["LabelRaw"].value_counts().to_dict()
    if mix:
        print("  " + "  ".join(f"{k}={v}" for k, v in sorted(mix.items())), file=sys.stderr)

    # HOW MANY CLAUSES NAME NO PLACE BY CONSTRUCTION. Printed beside the mix because it is the one
    # number that says whether the self-referential form is a rounding error or a real share, and
    # 04B2 put it at 11.9% of leases.
    n_self = int((frame["LabelRaw"] == "SelfReferential").sum())
    if n_hit:
        print(f"  self-referential: {n_self} of {n_hit} clause(s) "
              f"({100.0 * n_self / n_hit:.1f}%)", file=sys.stderr)
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
