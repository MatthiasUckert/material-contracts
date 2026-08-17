"""Monetary amounts, by pattern, with offsets.

Emits one label. `LabelRaw` carries the FORM matched, which is what makes the arm auditable: an
amount found by its currency symbol and one inferred from spelled-out words are different kinds of
evidence and should not be pooled without saying so.

WHY A RULE ARM FOR MONEY
Every other label was settled by measurement. Money never was, because EDGAR records no contract
value and so no anchor could rank an engine; the transformer went into the policy as a declared
default rather than a measured choice, and it is the only reason a transformer is in the pipeline
at all.

The suspicion this arm tests is that money in contracts is a bounded grammar rather than a
recognition problem. The evidence was already sitting in the resolver, which filtered the
transformer's spans to those carrying a currency marker or a thousands separator before using any
of them -- a pattern was doing the discriminating and the model was proposing candidates for it to
rescue.

THREE THINGS A PATTERN DOES BETTER, ALL NAMED BY A READING SESSION
  Currency other than dollars. "E185,000,000" is a euro amount whose symbol did not survive
  conversion, and a model trained on general English reads the capital E as a letter. A character
  class does not have that problem.
  Redacted amounts. "a minimum market price of $ per share" and "$[***]" have no number for a model
  to tag, so the transformer finds nothing -- and these are the cases that matter most, because the
  figures withheld are systematically the commercially material ones.
  Section numbers. "14.13 Requirements of Law" is the largest false-positive family in the label.
  Requiring a currency marker excludes them by construction rather than afterwards.

WHAT IT WILL MISS
An amount written in words with no currency word after it, and an amount whose currency is
established a paragraph earlier and never repeated. The first looks rare -- word forms almost always
sit beside the digit form in parentheses -- and the second is a relation, not a pattern. Neither is
claimed to be handled.

WHAT IT DELIBERATELY DOES NOT MATCH
A currency name with no figure attached. The transformer tags "U.S. Dollars", "Dollars" and "the
Dollar" as money roughly seven hundred times across the sample, and none of them is an amount:
"payable in U.S. Dollars" states a denomination. Requiring a figure is what separates the two, and
it is why this arm returns fewer spans than the transformer while covering more of the cases that
carry a number.

WHY Amount CAN BE None WHILE Currency IS SET
Three forms match a REDACTED figure. They carry a denomination and no number, and they are among
the reasons this arm exists: the amounts a filer withholds are systematically the material ones.
Returning zero, or dropping the row, would erase exactly the observation that matters.

OVERLAPS RESOLVE LEFTMOST-FIRST HERE, NOT LONGEST-FIRST
Money expressions are read in document order: a figure that has already begun is not superseded by
a later, longer expression that happens to swallow it. Dates use the other rule, because competing
date patterns are alternative readings of the SAME text and the longest reading wins outright.

  A NOTE ON THE PORT. moneyregex-v6's own docstring said "resolved longest-first"; its code sorted
  on (start, -length) and swept left to right, which is leftmost-first. The two disagree whenever a
  longer span begins inside a shorter earlier one. The CODE is preserved exactly, because changing
  it would move spans in an existing store, and the discrepancy is recorded here rather than
  quietly resolved in either direction.
"""
from __future__ import annotations

import re
import sys
from decimal import Decimal, InvalidOperation

from . import _io

MODEL = "moneyregex-v6"     # THE version. The filename carries none; see _io.spec_hash().
NAME = "moneyregex"
LABELS = ("MONEY",)
EXTRAS = ("Amount", "Currency")

DEFAULT_TIMEOUT = 0
DEFAULT_CHUNK_SIZE = 64


# 1. The pieces a pattern is built from ----------------------------------------------------------

# Symbols that survive HTML-to-text conversion, and the national prefixes that precede them. R$ and
# US$ both end in the dollar sign, so the prefix is optional rather than enumerated twice. \u0080 is
# the euro sign mangled by a cp1252 round trip, which is how it survives conversion in a fair number
# of these filings: 42 spans across 24 documents that a proper euro class never sees.
CUR_SYM = "[$\u00a3\u20ac\u00a5\u0080]"
# U.S.$10,000,000 occurs with the stops in place; a prefix without them misses it.
PREFIX = r"(?:U\.?S\.?|R|C|A|HK|S|NZ)?"

# ISO codes and spelt currency names. Contracts with a foreign counterparty write "EUR 11,848,000"
# and "RMB10,000,000" and nothing else, so a symbol-only arm returns no money at all for them --
# the gap is small in total volume and total for the documents it affects.
ISO = r"(?:USD|EUR|GBP|CHF|JPY|CAD|AUD|CNY|RMB|HKD|SGD|NZD|SEK|NOK|DKK)"
# Names that PRECEDE their figure. Dollar and pound are deliberately absent: English writes the
# number first ("5,000,000 Dollars", handled by amount_word), and a name-then-number rule on
# "dollar" matched a loan-servicing data dictionary 123 times across two documents -- field specs
# reading "No commas(,) or dollar signs", not amounts. Euro, renminbi and yen do occur this way.
CUR_NAME = r"(?:euros?|renminbi|yen)"

# SEPARATORS MUST BE CONSISTENT. Allowing either as a group separator lets a comma-grouped figure
# run past its own decimal point into the next number: "$9,752,233.001.857" was captured whole, 71
# times, and parses to nothing. Commas group with a dot decimal, dots group with a comma decimal,
# and the two are never mixed.
#
# THE EUROPEAN ALTERNATIVE WAS EATING SUB-CENT FIGURES. As written in v4 it was
# \d{1,3}(?:\.\d{3})+, which matches "0.000" inside "0.0001" -- one dot group, three digits, done --
# so the trailing digit was dropped from the SPAN and a par value of $0.0001 was stored as $0.000
# and parsed to zero. "par value $0.0001 per share" is boilerplate in these filings, and nothing
# downstream could have caught it: the span is well-formed, the offsets round-trip, and zero is a
# number. European grouping is now admitted only where it is UNAMBIGUOUS -- either a comma decimal
# follows it ("1.500,00") or there are two groups or more ("1.234.567").
NUM = (
    r"\d{1,3}(?:,\d{3})+(?:\.\d+)?"      # 1,234,567.89   US grouping
    r"|\d{1,3}(?:\.\d{3})+,\d+"          # 1.500,00       European, comma decimal
    r"|\d{1,3}(?:\.\d{3}){2,}"           # 1.234.567      European, two groups or more
    r"|\d+\.\d+|\d+"                     # plain
)

# Numerals as words. "and" is NOT in this list: a pattern admitting it would match "and Dollars" in
# ordinary running text, which occurs constantly and is not an amount.
NUMWORD = (
    r"(?:one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|"
    r"fifteen|sixteen|seventeen|eighteen|nineteen|twenty|thirty|forty|fifty|sixty|seventy|"
    r"eighty|ninety|hundred|thousand|million|billion|trillion)"
)
CUR_WORD = r"(?:dollars?|euros?|pounds?)"

# THE SCALE WORD WAS ONCE ONLY ATTACHED TO THE DOLLAR SIGN. symbol_scaled handled "$30 million" and
# nothing else did, so "NOK 23 million" matched iso_amount, kept "NOK 23" and parsed to twenty-three
# -- six orders of magnitude, at the BOTTOM of the distribution where a magnitude check looks for
# errors at the top. Found by reading the five non-USD spans in a sample of twenty-five; four were
# wrong.
#
# ORDER IS LOAD-BEARING. Python's alternation is leftmost-first, so "million" must precede "mill" or
# the longer word is cut short and the "ion" left behind. "mill" is in the list because Scandinavian
# and continental filings abbreviate that way -- "NOK 23.0 mill" -- and "mm" because finance writes
# "$5mm". Both are safe only because a currency marker is required first: a bare "5 mm" is a
# millimetre and matches nothing here.
SCALE_WORD = r"(?:million|billion|trillion|thousand|mill|mm)"


# 2. The forms -----------------------------------------------------------------------------------
# The third field is the READING -- how to turn this form's text into an amount. It sits beside the
# pattern for the same reason the date table's does: each form determines exactly one reading of its
# own text, and a parser written elsewhere has to guess which one fired.
#   figure   a numeral, optionally scaled
#   words    a spelled-out amount
#   redact   a denomination with the figure withheld; no number, deliberately

PATTERNS = [
    # $5,000,000  US$10,000  R$1.500.000  $0.001
    ("symbol_amount", rf"{PREFIX}{CUR_SYM}\s{{0,3}}(?:{NUM})", "figure"),
    # $30 million. The figure alone parses to thirty, so the scale word has to be inside the span
    # or the amount is out by six orders of magnitude.
    ("symbol_scaled", rf"{PREFIX}{CUR_SYM}\s{{0,3}}(?:{NUM})\s{{0,3}}{SCALE_WORD}\b", "figure"),
    # NOK 23 million, EUR 5.5 mill -- the same trap, for a code rather than a sign
    ("iso_scaled", rf"(?<![A-Za-z]){ISO}\s{{0,3}}(?:{NUM})\s{{0,3}}{SCALE_WORD}\b", "figure"),
    # EUR 11,848,000  USD9,750,000  RMB10,000,000
    ("iso_amount", rf"(?<![A-Za-z]){ISO}\s{{0,3}}(?:{NUM})", "figure"),
    # Euro 6 million -- the name spelt out, with a scale word after the figure
    ("name_scaled", rf"(?<![A-Za-z]){CUR_NAME}\s{{0,3}}(?:{NUM})\s{{0,3}}{SCALE_WORD}\b", "figure"),
    # Euro 6.667.856,00 -- the name spelt out, and European separators with it
    ("name_amount", rf"(?<![A-Za-z]){CUR_NAME}\s{{0,3}}(?:{NUM})", "figure"),
    # $[***]  $**  $TBD  $____ -- the amount was withheld and the symbol survived. Bare asterisks
    # are included because a redaction is not always bracketed: "$**" occurs 43 times. The bracket
    # content excludes BOTH brackets; excluding only the closing one lets an unclosed "$[" run
    # forward to the next "]" belonging to a different marker, which produced a span of twenty-two
    # characters of prose between two redactions.
    ("symbol_redact",
     rf"{PREFIX}{CUR_SYM}\s{{0,3}}(?:\[[^\[\]\n]{{0,40}}\]|\*{{1,6}}|TBD|_{{2,}})", "redact"),
    # A table cell truncated at conversion leaves "$[" with no closing bracket, 69 times across 34
    # documents. The content is restricted to whitespace and asterisks rather than made optional: an
    # unbounded run to the next space swallowed forty characters of ordinary prose in testing.
    ("symbol_redact_open", rf"{PREFIX}{CUR_SYM}\s{{0,3}}\[[\s*]{{0,6}}", "redact"),
    # "a minimum market price of $ per share" -- withheld with nothing at all left behind
    ("symbol_bare", rf"{PREFIX}{CUR_SYM}(?=\s+per\b)", "redact"),
    # A euro amount whose symbol became a capital E in conversion. Grouped thousands are REQUIRED:
    # without them "E12" matches an exhibit number, a clause label and half the alphabet soup in a
    # filing header.
    ("euro_letter", r"(?<![A-Za-z])E\s?\d{1,3}(?:,\d{3})+(?:\.\d+)?", "figure"),
    # 5 million Dollars -- the scale word sits BETWEEN the figure and the currency, so amount_word
    # cannot reach it: that pattern requires the two to be adjacent.
    ("word_scaled", rf"(?:{NUM})\s{{0,3}}{SCALE_WORD}\s+{CUR_WORD}\b", "figure"),
    # 5,000,000 Dollars
    ("amount_word", rf"(?:{NUM})\s+{CUR_WORD}\b", "figure"),
    # SIXTY-ONE THOUSAND NINETY AND 90/100 Dollars
    ("words_only",
     rf"(?<![A-Za-z])(?:{NUMWORD}[-\s]+)+(?:(?:and|[a-z]+)[-\s]+)*"
     rf"(?:\d{{1,2}}/100\s+)?{CUR_WORD}\b", "words"),
]
COMPILED = [(name, re.compile(pat, re.IGNORECASE), reading) for name, pat, reading in PATTERNS]
READING_BY_NAME = {name: reading for name, _pat, reading in PATTERNS}


# 3. Resolving a denomination --------------------------------------------------------------------

# Prefix before a dollar sign -> ISO. Bare "$" falls through to USD.
#
# BARE "$" IS READ AS USD, and that is an assumption rather than a fact. Prefixed forms are honoured
# -- US$, R$, C$, A$, HK$, S$, NZ$ each resolve to their own code -- so it reaches only the unmarked
# sign, which in an SEC filing is the domestic dollar.
PREFIX_ISO = {
    "US": "USD", "U.S.": "USD", "U.S": "USD", "US.": "USD",
    "R": "BRL", "C": "CAD", "A": "AUD", "HK": "HKD", "S": "SGD", "NZ": "NZD",
}
SYMBOL_ISO = {
    "$": "USD",
    "\u00a3": "GBP",
    "\u20ac": "EUR",
    "\u00a5": "JPY",
    "\u0080": "EUR",        # the euro sign mangled by a cp1252 round trip
}
NAME_ISO = {
    "euro": "EUR", "euros": "EUR", "renminbi": "CNY", "yen": "JPY",
    "dollar": "USD", "dollars": "USD", "pound": "GBP", "pounds": "GBP",
}
# Keyed on the same words SCALE_WORD is built from, so a word can never be matched and then not
# applied. "mill" and "mm" both mean million wherever a currency marker precedes them.
SCALE = {"thousand": 1000, "million": 10 ** 6, "billion": 10 ** 9, "trillion": 10 ** 12,
         "mill": 10 ** 6, "mm": 10 ** 6}

WORD_VAL = {
    "one": 1, "two": 2, "three": 3, "four": 4, "five": 5, "six": 6, "seven": 7, "eight": 8,
    "nine": 9, "ten": 10, "eleven": 11, "twelve": 12, "thirteen": 13, "fourteen": 14,
    "fifteen": 15, "sixteen": 16, "seventeen": 17, "eighteen": 18, "nineteen": 19,
    "twenty": 20, "thirty": 30, "forty": 40, "fifty": 50, "sixty": 60, "seventy": 70,
    "eighty": 80, "ninety": 90,
}

_RE_ISO = re.compile(r"(?<![A-Za-z])(USD|EUR|GBP|CHF|JPY|CAD|AUD|CNY|RMB|HKD|SGD|NZD|SEK|NOK|DKK)",
                     re.IGNORECASE)
_RE_SYM = re.compile(r"([$\u00a3\u20ac\u00a5\u0080])")
_RE_PREFIX = re.compile(r"(U\.?S\.?|R|C|A|HK|S|NZ)?[$\u00a3\u20ac\u00a5\u0080]", re.IGNORECASE)
_RE_NAME = re.compile(r"(?<![A-Za-z])(euros?|renminbi|yen|dollars?|pounds?)(?![A-Za-z])",
                      re.IGNORECASE)
_RE_NUM = re.compile(NUM)      # THE SAME pattern the extractor matched with, never a second copy
_RE_SCALE = re.compile(r"(?<![A-Za-z])" + SCALE_WORD + r"(?![A-Za-z])", re.IGNORECASE)
_RE_FRACTION = re.compile(r"(\d{1,2})/100")


# 4. Identity ------------------------------------------------------------------------------------

# EVERYTHING THAT CHANGES OUTPUT, INCLUDING THE PARSE TABLES. A currency map is not decoration: move
# BRL off R$ and the same span means something else. Edit any of these and the hash moves; rewrite a
# docstring and it does not.
SPEC = {
    "cur_sym": CUR_SYM,
    "prefix": PREFIX,
    "iso": ISO,
    "cur_name": CUR_NAME,
    "num": NUM,
    "numword": NUMWORD,
    "cur_word": CUR_WORD,
    "scale_word": SCALE_WORD,
    "patterns": PATTERNS,
    "prefix_iso": PREFIX_ISO,
    "symbol_iso": SYMBOL_ISO,
    "name_iso": NAME_ISO,
    "scale": SCALE,
    "word_val": WORD_VAL,
}

EMIT = _io.Emitter(MODEL, EXTRAS)


# 5. Parsing -------------------------------------------------------------------------------------

def num_from_text(txt):
    """A figure as a Decimal, with the separator convention decided per span.

    THE REGEX CANNOT DECIDE THIS AND MUST NOT TRY. Its European alternative
    \\d{1,3}(?:\\.\\d{3})+ matches "0.001" -- and read as grouping that becomes 1, so a per-share
    price of a tenth of a cent is recorded as one dollar, a factor of a thousand with no symptom.
    Per-share prices in that form are common in these filings.

    The rule, in order:
      both separators present   the LAST one is the decimal point
      two or more commas        grouping
      exactly one comma         grouping if the tail is exactly three digits, else a decimal comma
      two or more dots          European grouping
      one dot or none           already a plain decimal

    The remaining ambiguity is a single dot with three digits after it and nothing else: "1.500" is
    one and a half in a US filing and fifteen hundred in a European one. It is read as a decimal,
    because this corpus is SEC filings; the same string in another corpus would need the other rule.

    :param txt: the matched span.
    :return: Decimal, or None where the span carries no readable figure.
    """
    m = _RE_NUM.search(txt)
    if m is None:
        return None
    raw = m.group(0)
    n_dot, n_com = raw.count("."), raw.count(",")

    if n_com and n_dot:
        if raw.rfind(",") > raw.rfind("."):
            raw = raw.replace(".", "").replace(",", ".")
        else:
            raw = raw.replace(",", "")
    elif n_com >= 2:
        raw = raw.replace(",", "")
    elif n_com == 1:
        raw = raw.replace(",", "") if re.fullmatch(r"\d{1,3},\d{3}", raw) else raw.replace(",", ".")
    elif n_dot >= 2:
        raw = raw.replace(".", "")

    try:
        return Decimal(raw)
    except InvalidOperation:
        return None


def num_from_words(txt):
    """A spelled-out amount as a Decimal.

    "SIXTY-ONE THOUSAND NINETY AND 90/100" -> 61090.90. The n/100 tail is how a contract writes
    cents in a words-only figure, and dropping it would round every one of them down.

    :param txt: the matched span.
    :return: Decimal, or None where no numeral word was present.
    """
    total, current = 0, 0
    seen = False
    for tok in re.split(r"[-\s]+", txt.lower()):
        tok = tok.strip(",.")
        if tok in WORD_VAL:
            current += WORD_VAL[tok]
            seen = True
        elif tok == "hundred":
            current = (current or 1) * 100
            seen = True
        elif tok in SCALE:
            total += (current or 1) * SCALE[tok]
            current = 0
            seen = True
    if not seen:
        return None
    value = Decimal(total + current)
    frac = _RE_FRACTION.search(txt)
    if frac is not None:
        value += Decimal(frac.group(1)) / Decimal(100)
    return value


def currency_of(span, form):
    """The ISO code a span denominates, or None.

    :param span: the matched text.
    :param form: the LabelRaw of the pattern that matched; euro_letter cannot be read off the text.
    :return: ISO 4217 code, or None.
    """
    iso = _RE_ISO.search(span)
    if iso is not None:
        code = iso.group(1).upper()
        return "CNY" if code == "RMB" else code

    if form == "euro_letter":
        return "EUR"

    sym = _RE_SYM.search(span)
    if sym is not None:
        if sym.group(1) == "$":
            pre = _RE_PREFIX.match(span[:sym.end()])
            key = (pre.group(1) or "").upper().replace(".", "") if pre else ""
            if key:
                return PREFIX_ISO.get(key) or PREFIX_ISO.get(key + ".") or "USD"
            return "USD"        # the unmarked dollar, in an SEC filing, is the domestic one
        return SYMBOL_ISO.get(sym.group(1))

    name = _RE_NAME.search(span)
    if name is not None:
        return NAME_ISO.get(name.group(1).lower())
    return None


def parse_money(span, form):
    """One matched span and its form -> (amount as a string or None, ISO code or None).

    THE AMOUNT CROSSES THE SEAM AS TEXT. A Decimal becomes a float in pandas, and a contract value
    of 9,752,233.001 is exactly the kind of number that does not survive that intact.

    THE SCALE WORD MUST BE APPLIED. "$30 million" parses to thirty without it -- six orders of
    magnitude, and thirty is a perfectly plausible contract value, so nothing downstream would
    flag it.

    :param span: the matched text.
    :param form: the pattern name, which selects the reading.
    :return: (Amount, Currency), either of which may be None.
    """
    cur = currency_of(span, form)
    reading = READING_BY_NAME.get(form)

    if reading == "redact":
        return None, cur                  # withheld: a currency, deliberately no number

    if reading == "words":
        val = num_from_words(span)
    else:
        val = num_from_text(span)
        if val is not None:
            scale = _RE_SCALE.search(span)
            if scale is not None:
                val = val * SCALE[scale.group(0).lower()]

    if val is None:
        return None, cur
    return format(val.normalize(), "f"), cur


# 6. Extraction ----------------------------------------------------------------------------------

def init_worker(timeout, want=LABELS):
    """Pool initialiser: the per-document cap.

    Set in the WORKER, never in main() and read here. Under spawn a worker inherits no module state,
    and spawn is the default on macOS, which is the machine this runs on. `want` is accepted and
    ignored so every module's initialiser has one signature.
    """
    _io.install_alarm(timeout)


def extract_one(args):
    """Rows for one document. Always returns at least one: a sentinel where nothing was found.

    Parsing happens INSIDE the cap and AFTER overlap resolution, so it runs once per kept span
    rather than once per candidate.
    """
    docid, text = args
    rows = []
    if isinstance(text, str) and text.strip():
        try:
            _io.start_alarm()
            for s, e, form in _io.keep_leftmost(text, COMPILED):
                span = text[s:e]
                amount, currency = parse_money(span, form)
                rows.append(EMIT.row(docid, s, e, span, "MONEY", form, (amount, currency)))
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
    path = _io.write_output(frame, out_dir, MODEL, casts={
        "Amount": "string",         # text across the seam; a Decimal would become a lossy float
        "Currency": "string",
    })

    n_cand = int(frame["Start"].notna().sum())
    n_amt = int(frame["Amount"].notna().sum())
    n_cur = int(frame["Currency"].notna().sum())
    print(f"{len(ids)} doc(s) -> {n_cand} candidate(s), {n_amt} with an amount, "
          f"{n_cur} with a currency  [{_io.ENGINE}:{MODEL}] -> {path}")
    mix = frame["LabelRaw"].value_counts().to_dict()
    if mix:
        print("  " + "  ".join(f"{k}={v}" for k, v in sorted(mix.items())), file=sys.stderr)
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
