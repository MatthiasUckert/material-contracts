"""Redaction indicators, by bracket class, with offsets.

Emits one label. `LabelRaw` carries the class, which is what downstream aggregation pivots on.

WHY THIS IS AN EXTRACTOR AND NOT A COUNT
The published redaction analysis counts bracketed indicators per document. That answers how much was
withheld and cannot answer WHAT was withheld, because a count carries no position. Emitting the
indicators as spans puts them in the same coordinate system as every other candidate, so "how far is
this money span from the nearest redaction" becomes a window function rather than a new pass over
the text.

That matters for two questions. It is the only external evidence money admits: a marker sits where a
commercially material amount used to be, so a cue preceding "[***]" in one contract and "$5,000,000"
in another is locating the same slot. And it turns "how many redactions" into "redactions of what" --
adjacent to an amount, to a party name, or to neither.

CLASSES
Four following the published classification so the numbers reconcile, plus one addition.

  RedactExplicit  A bracket naming confidential treatment: CONFIDENTIAL, REDACT, CTR.
  RedactSymbol    A bracket holding only whitespace and asterisks: [***], [ * * * ], [*].
  RedactBlank     A bracket holding only whitespace and underscores: [___], [__].
  OmitExplicit    A bracket recording deletion: INTENTIONALLY, OMITTED, DELETE.
  OmitSymbol      A bracket holding only bullet or ellipsis characters, or three dots.
  RedactBare      An unbracketed run of three or more asterisks. NOT in the published method;
                  separated so it can be measured and then kept or dropped on evidence. Filter it
                  with LabelRaw <> 'RedactBare' if it floods.

Precedence follows the original: "[CONFIDENTIAL PORTION OMITTED]" classifies as RedactExplicit
rather than OmitExplicit, because confidential treatment is the more specific claim. Brackets
matching nothing -- [1.4], [borrower] -- are not emitted; a label called REDACT should mean
redaction.

WHY THE TEXT IS NOT NORMALISED FIRST
The published version collapses whitespace and upper-cases before matching, which is why "[ * * * ]"
reduces to "[***]" and classifies cleanly. Both operations change string LENGTH, so any offset taken
afterwards indexes a string that no longer exists. Here the match is taken against the RAW text and
only the matched substring is normalised, for classification. The bracket pattern therefore has to
tolerate interior whitespace and line breaks itself.

TWO PASSES, NOT A PATTERN TABLE
Brackets are taken first and their extents recorded; a bare asterisk run inside one is then skipped
rather than emitted twice under two classes. Neither of the family's overlap resolvers applies --
this is containment exclusion, and it is why this module does not call keep_longest() or
keep_leftmost().

THE EXCLUSIONS ARE COUNTED, AND THE COUNTS ARE REPORTED
Three things are dropped, each named by a reading session's account of what an earlier version was
picking up: page filler, which conceals nothing; a marker quoted in a filing's opening legend, which
describes the convention rather than marking a gap; and a rule of asterisks drawn as a border, which
is typography.

  A NOTE ON THE PORT. redaction-v1 computed these counts, returned them from find_indicators(), and
  then discarded them at the call site -- so the argument its own docstring makes, that an exclusion
  nobody can see is indistinguishable from a pattern that never matched, was true of the code
  itself. They are now summed across the run and printed. No span changes, so the model tag does
  not move.
"""
from __future__ import annotations

import re
import sys

from . import _io

MODEL = "redaction-v2"      # THE version. The filename carries none; see _io.spec_hash().
NAME = "redaction"
LABELS = ("REDACT",)
EXTRAS = ()

DEFAULT_TIMEOUT = 0
DEFAULT_CHUNK_SIZE = 64


# 1. What counts as a candidate ------------------------------------------------------------------

# A bracket holding no other bracket, bounded so a stray "[" cannot swallow a paragraph. Newlines
# are allowed inside because the raw text is not re-wrapped before matching and
# "[CONFIDENTIAL TREATMENT\nREQUESTED]" is ordinary in a converted filing.
P_BRACKET = r"\[[^\[\]]{0,80}\]"

# Three or more asterisks standing alone. The guards keep it off footnote markers and off the
# interior of a bracket already matched above.
P_BARE = r"(?<![*\w])\*{3,}(?![*\w])"

# Bullet and ellipsis characters used as redaction fill, including the cp1252 strays that survive
# conversion. Held as a class so the match is on the characters themselves rather than on an
# escaped rendering of them.
BULLETS = "\u25cf\u2022\u2026\u00b7\u2219\u0095\u0097\u0086\u2010\u2043"

P_CONF = r"CONFIDENTIAL|REDACT|\bCTR\b"
P_OMIT = r"INTENTIONALLY|OMITTED|DELETE"
P_STARS_ONLY = r"^[*\s]+$"

# AN UNDERSCORE FILL IS A REDACTION AND redaction-v1 MISSED IT. 33 spans in a 300-document sample,
# and moneyregex already treats exactly this form as one -- its symbol_redact matches "$____" via
# _{2,}. Two extractors disagreeing about whether an underscore run marks a removal is a gap, not a
# judgement call.
#
# ITS OWN CLASS RATHER THAN FOLDED INTO RedactSymbol, because the two are different drafting acts.
# "[***]" is a POSITIVE mark meaning something was here. "[___]" is a blank line, and a blank line
# in these filings is also the unfilled-template idiom that produces "[NAME]", "[DATE]" and
# "[ADDRESS]". Folding it would hide a family that still needs testing inside one that is settled.
#
# THE LINE THIS DRAWS, stated because a referee could poke at it: an underscore blank marks a
# REMOVAL, a named placeholder marks a FIELD TO COMPLETE. "[___]" is emitted and "[NAME]" is not.
P_BLANK_ONLY = r"^[_\s]+$"
P_DOTS = r"\.{3}"

# PAGE FILLER. "[Remainder of page intentionally left blank]" sits immediately before the execution
# clause of a great many contracts and conceals nothing whatever, but it carries the word
# INTENTIONALLY and so classified as an omission. A reading session put it at roughly one marker in
# six, which is the margin by which a redaction count built this way overstates itself. Tested
# before the omission branch, and separated from a genuine "[INTENTIONALLY OMITTED]" by LEFTBLANK
# rather than by the shared word.
P_FILLER = r"LEFT\s*BLANK|PAGE\s*FOLLOWS|SIGNATURE\s*PAGE"

# THE OPENING LEGEND. A filing explains the convention by quoting the marker: "...such excluded
# information is indicated by [***]". That occurrence DESCRIBES a marker rather than standing where
# content was removed, so it is a false site. Recognised by the verb immediately to its left rather
# than by anything about the bracket itself.
P_LEGEND = r"(INDICATED|DENOTED|MARKED|REPRESENTED|REPLACED|SHOWN)\s+BY\s*$"
LEGEND_LOOKBACK = 80

# A DRAWN RULE. A row of asterisks bordering a notary seal, or underlining a signature line,
# occupies its whole line. A redaction never does: it stands inside a sentence. The test is the
# LINE, not the length, because a genuine "[*****]" is legitimate and a five-asterisk divider is
# not.
P_LINE_RULE = r"^[*\s_-]+$"

RX_BRACKET = re.compile(P_BRACKET, re.DOTALL)
RX_BARE = re.compile(P_BARE)
RX_BULLETS_ONLY = re.compile(r"^[" + re.escape(BULLETS) + r"\s]+$")
RX_CONF = re.compile(P_CONF)
RX_OMIT = re.compile(P_OMIT)
RX_STARS_ONLY = re.compile(P_STARS_ONLY)
RX_BLANK_ONLY = re.compile(P_BLANK_ONLY)
RX_DOTS = re.compile(P_DOTS)
RX_FILLER = re.compile(P_FILLER)
RX_LEGEND = re.compile(P_LEGEND, re.IGNORECASE)
RX_LINE_RULE = re.compile(P_LINE_RULE)

#: The classes this extractor can emit, in precedence order. Named so a consumer can enumerate them
#: without parsing the source, and so the test suite can assert the set has not silently grown.
CLASSES = ("RedactExplicit", "RedactSymbol", "RedactBlank", "OmitExplicit", "OmitSymbol",
           "RedactBare")

#: What each exclusion removed. Reported at the end of a run.
#
# FILLER IS SEPARATE FROM not_a_redaction, and in v1 it was not. Both paths returned None from
# classify(), so page filler -- which the rule RECOGNISES and chooses to drop -- was pooled with
# ordinary brackets like "[1.4]" that were never redactions. The docstring claims filler is roughly
# one marker in six and nothing could check it. Diagnostic only: no span changes.
DROP_KEYS = ("filler", "legend", "line_rule", "not_a_redaction")


# 2. Identity ------------------------------------------------------------------------------------


# THE CONTEXT WIDTH IS PART OF THIS EXTRACTOR'S IDENTITY. Every rule that opens a document does so
# for the same reason -- the characters around a span -- so storing them at extraction is what turns
# three text-reading rules into three column reads. It sits in SPEC because a store holding rows cut
# at 160 beside rows cut at 0 under one model tag would give a rule full context from some spans and
# none from others, with nothing to say which. Changing the width therefore moves the hash and
# clears the store: expensive, and correct.
SPEC = {
    "cue": _io.DEFAULT_CUE,
    "bracket": P_BRACKET,
    "bare": P_BARE,
    "bullets": BULLETS,
    "conf": P_CONF,
    "omit": P_OMIT,
    "stars_only": P_STARS_ONLY,
    "blank_only": P_BLANK_ONLY,
    "dots": P_DOTS,
    "filler": P_FILLER,
    "legend": P_LEGEND,
    "legend_lookback": LEGEND_LOOKBACK,
    "line_rule": P_LINE_RULE,
    "classes": CLASSES,
}

# THE EMITTER IS BUILT AT IMPORT AND THE WIDTHS ARE MODULE STATE, NOT PER-DOCUMENT STATE. A worker's
# globals die with the process, so anything set per document on this object would be lost across a
# pool boundary -- which is why row() takes the text explicitly rather than the emitter holding it.
EMIT = _io.Emitter(MODEL, EXTRAS, _io.DEFAULT_CUE, _io.DEFAULT_CUE)


# 3. Classification ------------------------------------------------------------------------------

def classify(span):
    """Which redaction class a bracketed span belongs to, or None if it is not one.

    Classification runs on a normalised COPY of the matched substring, so that "[ * * * ]" and
    "[***]" reach the same verdict while the offsets continue to index the untouched original.

    WHITESPACE IS COLLAPSED, NOT REMOVED. Removing it destroys word boundaries, and the abbreviation
    CTR then matches inside eleCTRonically -- so "[electronically]", an ordinary bracketed word,
    classifies as a confidential-treatment marker. The published version strips whitespace and tests
    the same three strings, so it carries this too.

    :param span: the matched bracket, including both brackets.
    :return: (class, reason). Exactly one is set: a class where the bracket IS a redaction, a
        reason from DROP_KEYS where it is not. Returning the reason is what lets the run report
        why a bracket was dropped rather than only that it was.
    """
    norm = re.sub(r"\s+", " ", span).upper().strip()
    inner = norm[1:-1] if len(norm) >= 2 else ""
    if not inner:
        return None, "not_a_redaction"
    if RX_FILLER.search(inner):
        return None, "filler"
    if RX_CONF.search(inner):
        return "RedactExplicit", None
    if RX_STARS_ONLY.match(inner):
        return "RedactSymbol", None
    if RX_BLANK_ONLY.match(inner):
        return "RedactBlank", None
    if RX_OMIT.search(inner):
        return "OmitExplicit", None
    if RX_BULLETS_ONLY.match(inner) or RX_DOTS.search(inner):
        return "OmitSymbol", None
    return None, "not_a_redaction"


def is_legend(text, start):
    """True when the marker is being described rather than standing in for removed content.

    :param text: the whole document; the verb to the LEFT is what decides.
    :param start: offset of the bracket.
    """
    left = text[max(0, start - LEGEND_LOOKBACK):start]
    return RX_LEGEND.search(left.rstrip()) is not None


def is_line_rule(text, start, stop):
    """True when the match occupies its entire line, i.e. is a drawn rule rather than a gap."""
    a = text.rfind("\n", 0, start) + 1
    b = text.find("\n", stop)
    line = text[a:(b if b >= 0 else len(text))]
    return RX_LINE_RULE.match(line) is not None


def find_indicators(text):
    """Every redaction indicator in one text, with what was excluded alongside.

    Brackets are taken first and their extents recorded, so a bare asterisk run sitting inside one
    is not emitted twice under two different classes.

    :param text: the document.
    :return: (hits, dropped) where hits is a list of (start, stop, span, class) sorted by position
        and dropped counts what each exclusion removed.
    """
    out = []
    covered = []
    dropped = dict.fromkeys(DROP_KEYS, 0)

    for m in RX_BRACKET.finditer(text):
        covered.append((m.start(), m.end()))
        klass, reason = classify(m.group(0))
        if klass is None:
            dropped[reason] += 1
            continue
        if klass == "RedactSymbol" and is_legend(text, m.start()):
            dropped["legend"] += 1
            continue
        out.append((m.start(), m.end(), m.group(0), klass))

    for m in RX_BARE.finditer(text):
        if any(a <= m.start() and m.end() <= b for a, b in covered):
            continue
        if is_line_rule(text, m.start(), m.end()):
            dropped["line_rule"] += 1
            continue
        out.append((m.start(), m.end(), m.group(0), "RedactBare"))

    out.sort(key=lambda r: r[0])
    return out, dropped


# 4. Extraction ----------------------------------------------------------------------------------

def init_worker(timeout, want=LABELS):
    """Pool initialiser: the per-document cap.

    `want` is accepted and ignored so every module's initialiser has one signature.
    """
    _io.install_alarm(timeout)


def extract_one(args):
    """(rows, dropped) for one document -- PAIRED, because the exclusion counts must survive.

    A worker's globals die with the process, so a per-document count cannot be accumulated in module
    state; it has to come back with the rows. run_pool(paired=True) sums them.
    """
    docid, text = args
    rows = []
    dropped = dict.fromkeys(DROP_KEYS, 0)

    if isinstance(text, str) and text.strip():
        try:
            _io.start_alarm()
            hits, dropped = find_indicators(text)
            for s, e, span, klass in hits:
                rows.append(EMIT.row(docid, s, e, span, "REDACT", klass, None, text))
        except _io.ExtractorTimeout:
            print(f"[timeout] {docid}: {NAME} > {_io.TIMEOUT}s, skipped", file=sys.stderr)
            return [EMIT.timeout(docid, NAME)], dict.fromkeys(DROP_KEYS, 0)
        except Exception as exc:            # one bad document must not end a corpus pass
            print(f"[error] {docid}: {NAME}: {exc}", file=sys.stderr)
            return [EMIT.error(docid, NAME)], dict.fromkeys(DROP_KEYS, 0)
        finally:
            _io.cancel_alarm()

    if not rows:
        rows.append(EMIT.sentinel(docid))
    return rows, dropped


def run(inputs, out_dir, labels=LABELS, id_col="DocID", text_col="TextRaw", max_chars=0,
        timeout=DEFAULT_TIMEOUT, n_process=1, chunk_size=DEFAULT_CHUNK_SIZE, no_progress=False):
    """One pass of this extractor over a document set.

    :return: the path written.
    """
    want = [l.upper() for l in labels if l.upper() in LABELS]
    ids, texts = _io.read_documents(inputs, id_col, text_col, max_chars)

    if not want:
        print(f"no {NAME} label in {labels}; writing sentinels only", file=sys.stderr)
        rows, dropped = [EMIT.sentinel(i) for i in ids], {}
    else:
        rows, dropped = _io.run_pool(
            items=list(zip(ids, texts)),
            extract_one=extract_one,
            initializer=init_worker,
            initargs=(max(0, timeout), tuple(want)),
            n_process=n_process,
            chunk_size=chunk_size,
            desc=f"{_io.ENGINE}:{MODEL}",
            no_progress=no_progress,
            paired=True,
        )

    frame = _io.finalize(rows, ids, EMIT)
    path = _io.write_output(frame, out_dir, MODEL)

    n_hit = int(frame["Start"].notna().sum())
    n_doc = int(frame.loc[frame["Start"].notna(), "DocID"].nunique())
    print(f"{len(ids)} doc(s) -> {n_hit} indicator(s) in {n_doc} doc(s)  "
          f"[{_io.ENGINE}:{MODEL}] -> {path}")
    mix = frame["LabelRaw"].value_counts().to_dict()
    if mix:
        print("  " + "  ".join(f"{k}={v}" for k, v in sorted(mix.items())), file=sys.stderr)
    if dropped:
        # WHAT WAS EXCLUDED, ALWAYS PRINTED. A reader who cannot see how much an exclusion removed
        # cannot tell a working rule from a pattern that never fired.
        print("  excluded: " + "  ".join(f"{k}={v}" for k, v in sorted(dropped.items())),
              file=sys.stderr)
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
