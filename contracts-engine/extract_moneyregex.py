#!/usr/bin/env python3
"""Regex money extractor with offsets -- a rule arm for the one label that had none.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model,
        Amount, Currency).

Stamps Engine = "paper", Model = MODEL. Label is always "MONEY"; LabelRaw carries the
form matched, which is what makes the arm auditable: an amount found by its currency
symbol and one inferred from words are different kinds of evidence and should not be
pooled without saying so.

WHY A RULE ARM FOR MONEY
Every other label was settled by measurement. Money never was, because EDGAR records no
contract value and so no anchor could rank an engine, and the transformer went into the
policy as a declared default rather than a measured choice. It is also the only reason a
transformer is in the pipeline at all.

The suspicion this arm tests is that money in contracts is a bounded grammar rather than a
recognition problem. The evidence for that was already sitting in the resolver, which
filters the transformer's spans to those carrying a currency marker or a thousands
separator before using any of them -- a pattern was doing the discriminating and the model
was proposing candidates for it to rescue.

THREE THINGS A PATTERN SHOULD DO BETTER, ALL NAMED BY A READING SESSION
Currency other than dollars. "E185,000,000" is a euro amount whose symbol did not survive
conversion, and a model trained on general English reads the capital E as a letter. A
character class does not have that problem.

Redacted amounts. "a minimum market price of $ per share" and "$[***]" have no number for
a model to tag, so the transformer finds nothing -- and these are the cases that matter
most, because the figures withheld are systematically the commercially material ones.

Section numbers. "14.13 Requirements of Law" is the largest false-positive family in the
label. Requiring a currency marker excludes them by construction rather than afterwards.

WHAT IT WILL MISS
An amount written in words with no currency word after it, and an amount whose currency is
established a paragraph earlier and never repeated. The first looks rare -- a reading
session observed that word forms almost always sit beside the digit form in parentheses --
and the second is a relation, not a pattern. Neither is claimed to be handled.

WHAT IT DELIBERATELY DOES NOT MATCH
A currency name with no figure attached. The transformer tags "U.S. Dollars", "Dollars"
and "the Dollar" as money roughly seven hundred times across the sample, and none of them
is an amount -- "payable in U.S. Dollars" states a denomination. Requiring a figure is what
separates the two, and it is why this arm returns fewer spans than the transformer while
covering more of the cases that carry a number.

THIS IS A CANDIDATE FOR MEASUREMENT, NOT A REPLACEMENT
Swapping one declared engine for another is not an improvement in evidence. What this arm
makes possible is a comparison: cross-engine agreement, the shape checks in 04B, and how
many amounts adjacent to a redaction marker each engine recovers. The last is countable
and is where a difference is expected.

--max-chars N truncates every document to its first N characters BEFORE extraction
(0 = off). --timeout N caps each document (seconds; 0 = off); on timeout the document is
skipped and emits a marker row (LabelRaw = "timeout:moneyregex", null span).

Offsets are 0-based, half-open, code-point indices: text[Start:Stop] == Span. Aligned
schema/CLI with extract_spacy.py / extract_lexnlp.py / extract_dateregex.py.
"""
import argparse
import os
import re
import signal
import sys
from decimal import Decimal, InvalidOperation
from multiprocessing import Pool
from pathlib import Path

import pandas as pd
import pyarrow.dataset as pads
from tqdm import tqdm

ENGINE = "paper"
MODEL = "moneyregex-v6"   # identifies THIS pattern set; bump on any rule change
LABEL = "MONEY"
CORE = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]
EXTRA = ["Amount", "Currency"]
COLUMNS = CORE + EXTRA

# Symbols that survive HTML-to-text conversion, and the national prefixes that precede them.
# R$ and US$ both end in the dollar sign, so the prefix is optional rather than enumerated twice.
# \u0080 is the euro sign mangled by a cp1252 round trip, which is how it survives conversion in a
# fair number of these filings: 42 spans across 24 documents that a proper euro class never sees.
CUR_SYM = "[$\u00a3\u20ac\u00a5\u0080]"
# U.S.$10,000,000 occurs with the stops in place; a prefix without them misses it.
PREFIX = r"(?:U\.?S\.?|R|C|A|HK|S|NZ)?"

# Whitespace between a currency marker and its figure is routine in converted filings, so the gap
# is up to three characters rather than one. A single optional space misses "$  5,000" outright,
# and 579 of the 2,125 currency-adjacent redaction sites carry whitespace before the marker.
#
# ISO codes and spelt currency names, both with and without a space before the figure. Contracts
# with a foreign counterparty write "EUR 11,848,000" and "RMB10,000,000" and nothing else, so a
# symbol-only arm returns no money at all for them -- the gap is small in total volume and total
# for the documents it affects.
ISO = r"(?:USD|EUR|GBP|CHF|JPY|CAD|AUD|CNY|RMB|HKD|SGD|NZD|SEK|NOK|DKK)"
# Names that precede their figure. Dollar and pound are deliberately absent: English writes the
# number first ("5,000,000 Dollars", handled by amount_word), and a name-then-number rule on
# "dollar" matched a loan-servicing data dictionary 123 times across two documents -- field specs
# reading "No commas(,) or dollar signs", not amounts. Euro, renminbi and yen do occur this way.
CUR_NAME = r"(?:euros?|renminbi|yen)"

# Separators must be CONSISTENT. Allowing either as a group separator lets a comma-grouped figure
# run past its own decimal point into the next number: "$9,752,233.001.857" was captured whole,
# 71 times, and parses to nothing. Commas group with a dot decimal, dots group with a comma
# decimal, and the two are never mixed.
# v5: THE EUROPEAN ALTERNATIVE WAS EATING SUB-CENT FIGURES. As written in v4 it was
# \d{1,3}(?:\.\d{3})+(?:,\d+)?, which matches "0.000" inside "0.0001" -- one dot group, three
# digits, done -- so the trailing digit was dropped from the SPAN and a par value of $0.0001 was
# stored as $0.000 and parsed to zero. "par value $0.0001 per share" is boilerplate in these
# filings, and nothing downstream could have caught it: the span is well-formed, the offsets
# round-trip, and zero is a number.
#
# European grouping is now admitted only where it is UNAMBIGUOUS -- either a comma decimal follows
# it ("1.500,00") or there are two groups or more ("1.234.567"). A single dot group with nothing
# after it falls through to the plain decimal, so "1.500" reads as one and a half. That is the US
# reading and it is the right default for SEC filings; the same string in a European corpus would
# need the other rule, which is why it is stated here rather than left to the regex.
NUM = (
    r"\d{1,3}(?:,\d{3})+(?:\.\d+)?"      # 1,234,567.89   US grouping
    r"|\d{1,3}(?:\.\d{3})+,\d+"           # 1.500,00       European, comma decimal
    r"|\d{1,3}(?:\.\d{3}){2,}"            # 1.234.567      European, two groups or more
    r"|\d+\.\d+|\d+"                      # plain
)

# Numerals as words. "and" is NOT in this list: a pattern admitting it would match "and Dollars"
# in ordinary running text, which occurs constantly and is not an amount.
NUMWORD = (
    r"(?:one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|"
    r"fifteen|sixteen|seventeen|eighteen|nineteen|twenty|thirty|forty|fifty|sixty|seventy|"
    r"eighty|ninety|hundred|thousand|million|billion|trillion)"
)
CUR_WORD = r"(?:dollars?|euros?|pounds?)"

# v6: THE SCALE WORD WAS ONLY EVER ATTACHED TO THE DOLLAR SIGN. symbol_scaled handled "$30 million"
# and nothing else did, so "NOK 23 million" matched iso_amount, kept "NOK 23" and parsed to
# twenty-three -- six orders of magnitude, at the BOTTOM of the distribution where a magnitude
# check looks for errors at the top. Found by reading the five non-USD spans in a sample of
# twenty-five; four of them were wrong.
#
# ORDER IS LOAD-BEARING AGAIN. Python's alternation is leftmost-first, so "million" must precede
# "mill" or the longer word is cut short and the "ion" left behind. "mill" is in the list because
# Scandinavian and continental filings abbreviate that way -- "NOK 23.0 mill" -- and "mm" because
# finance writes "$5mm". Both are safe only because a currency marker is required first: a bare
# "5 mm" is a millimetre and matches nothing here.
SCALE_WORD = r"(?:million|billion|trillion|thousand|mill|mm)"

# Order matters only for reporting; overlaps are resolved by length in find_amounts().
PATTERNS = [
    # $5,000,000  US$10,000  R$1.500.000  EUR185,000,000  $0.001
    ("symbol_amount", rf"{PREFIX}{CUR_SYM}\s{{0,3}}(?:{NUM})"),
    # $30 million, $25.0 million. The figure alone parses to thirty, so the scale word has to be
    # inside the span or the amount is out by six orders of magnitude. Listed before the plain
    # symbol form because overlaps resolve longest-first and this one must win.
    ("symbol_scaled", rf"{PREFIX}{CUR_SYM}\s{{0,3}}(?:{NUM})\s{{0,3}}{SCALE_WORD}\b"),
    # NOK 23 million, EUR 5.5 mill -- the same trap as symbol_scaled, for a code rather than a sign
    ("iso_scaled", rf"(?<![A-Za-z]){ISO}\s{{0,3}}(?:{NUM})\s{{0,3}}{SCALE_WORD}\b"),
    # EUR 11,848,000  USD9,750,000  RMB10,000,000
    ("iso_amount", rf"(?<![A-Za-z]){ISO}\s{{0,3}}(?:{NUM})"),
    # Euro 6 million -- the name spelt out, with a scale word after the figure
    ("name_scaled", rf"(?<![A-Za-z]){CUR_NAME}\s{{0,3}}(?:{NUM})\s{{0,3}}{SCALE_WORD}\b"),
    # Euro 6.667.856,00 -- the name spelt out, and European separators with it
    ("name_amount", rf"(?<![A-Za-z]){CUR_NAME}\s{{0,3}}(?:{NUM})"),
    # $[***]  $**  $TBD  $____  -- the amount was withheld and the symbol survived. Bare asterisks
    # are included because a redaction is not always bracketed: "$**" occurs 43 times.
    # The bracket content excludes BOTH brackets. Excluding only the closing one lets an unclosed
    # "$[" run forward to the next "]" belonging to a different marker, which in testing produced
    # a span of twenty-two characters of prose between two redactions.
    ("symbol_redact",
     rf"{PREFIX}{CUR_SYM}\s{{0,3}}(?:\[[^\[\]\n]{{0,40}}\]|\*{{1,6}}|TBD|_{{2,}})"),
    # A table cell truncated at conversion leaves "$[" with no closing bracket, 69 times across 34
    # documents. The content is restricted to whitespace and asterisks rather than made optional:
    # an unbounded run to the next space swallowed forty characters of ordinary prose in testing.
    # Listed after the closed form so that longest-first still prefers "$[***]" over "$[***".
    ("symbol_redact_open", rf"{PREFIX}{CUR_SYM}\s{{0,3}}\[[\s*]{{0,6}}"),
    # "a minimum market price of $ per share" -- withheld with nothing at all left behind
    ("symbol_bare", rf"{PREFIX}{CUR_SYM}(?=\s+per\b)"),
    # A euro amount whose symbol became a capital E in conversion. Grouped thousands are required:
    # without them "E12" matches an exhibit number, a clause label and half the alphabet soup in a
    # filing header.
    ("euro_letter", r"(?<![A-Za-z])E\s?\d{1,3}(?:,\d{3})+(?:\.\d+)?"),
    # 5 million Dollars -- the scale word sits BETWEEN the figure and the currency, so amount_word
    # cannot reach it: that pattern requires the two to be adjacent.
    ("word_scaled", rf"(?:{NUM})\s{{0,3}}{SCALE_WORD}\s+{CUR_WORD}\b"),
    # 5,000,000 Dollars
    ("amount_word", rf"(?:{NUM})\s+{CUR_WORD}\b"),
    # SIXTY-ONE THOUSAND NINETY AND 90/100 Dollars
    ("words_only",
     rf"(?<![A-Za-z])(?:{NUMWORD}[-\s]+)+(?:(?:and|[a-z]+)[-\s]+)*"
     rf"(?:\d{{1,2}}/100\s+)?{CUR_WORD}\b"),
]
RX = [(name, re.compile(pat, re.IGNORECASE)) for name, pat in PATTERNS]


# ---------------------------------------------------------------------------------------------
# Parsing. THE FORMAT LIVES BESIDE THE PATTERN IT BELONGS TO, for the same reason the date
# extractor's does: each of the ten forms determines exactly one reading of its own text, and a
# parser written somewhere else has to guess which one fired. _RE_NUM below is compiled from NUM
# itself rather than restated, so the figure the parser reads is by construction the figure the
# extractor matched.
#
# WHY Amount CAN BE None WHILE Currency IS SET. Three forms match a REDACTED figure -- "$[***]",
# "$**", "a minimum market price of $ per share". They carry a denomination and no number, and they
# are among the reasons this arm exists at all: the amounts a filer withholds are systematically
# the commercially material ones. Returning zero, or dropping the row, would erase exactly the
# observation that matters.
#
# THE SCALE WORD MUST BE APPLIED. "$30 million" parses to thirty without it -- six orders of
# magnitude, and thirty is a perfectly plausible contract value, so nothing downstream would flag it.
#
# BARE "$" IS READ AS USD. That is an assumption, not a fact. Prefixed forms are honoured -- US$,
# R$, C$, A$, HK$, S$, NZ$ each resolve to their own code -- so it reaches only the unmarked sign,
# which in an SEC filing is the domestic dollar.


# Prefix before a dollar sign -> ISO. Bare "$" falls through to USD.
PREFIX_ISO = {
    "US": "USD", "U.S.": "USD", "U.S": "USD", "US.": "USD",
    "R": "BRL", "C": "CAD", "A": "AUD", "HK": "HKD", "S": "SGD", "NZ": "NZD",
}
SYMBOL_ISO = {
    "$": "USD",
    "\u00a3": "GBP",
    "\u20ac": "EUR",
    "\u00a5": "JPY",
    "\u0080": "EUR",     # the euro sign mangled by a cp1252 round trip
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
_RE_NUM = re.compile(NUM)   # THE SAME pattern the extractor matched with, never a second copy
_RE_SCALE = re.compile(r"(?<![A-Za-z])" + SCALE_WORD + r"(?![A-Za-z])", re.IGNORECASE)
_RE_FRACTION = re.compile(r"(\d{1,2})/100")

REDACT_FORMS = {"symbol_redact", "symbol_redact_open", "symbol_bare"}


def _num_from_text(txt):
    """A figure as a Decimal, with the separator convention decided per span.

    THE REGEX CANNOT DECIDE THIS AND MUST NOT TRY. Its European alternative,
    \\d{1,3}(?:\\.\\d{3})+, matches "0.001" -- and read as grouping that becomes 1, so a per-share
    price of a tenth of a cent is recorded as one dollar, a factor of a thousand with no symptom.
    Per-share prices in that form are common in these filings.

    The rule, in order:
      both separators present  the LAST one is the decimal point
      two or more commas       grouping
      exactly one comma        grouping if the tail is exactly three digits, else a decimal comma
      two or more dots         European grouping
      one dot or none          already a plain decimal

    The remaining ambiguity is a single dot with three digits after it and nothing else: "1.500" is
    one and a half in a US filing and fifteen hundred in a European one. It is read as a decimal,
    because this corpus is SEC filings; the same string in another corpus would need the other rule.
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


def _num_from_words(txt):
    """A spelled-out amount as a Decimal. 'SIXTY-ONE THOUSAND NINETY AND 90/100' -> 61090.90."""
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
    """The ISO code a span denominates, or None."""
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
            return "USD"           # the unmarked dollar, in an SEC filing, is the domestic one
        return SYMBOL_ISO.get(sym.group(1))

    name = _RE_NAME.search(span)
    if name is not None:
        return NAME_ISO.get(name.group(1).lower())
    return None


def parse_money(span, form):
    """One matched span and its pattern name -> (amount as a string or None, ISO code or None)."""
    cur = currency_of(span, form)

    if form in REDACT_FORMS:
        return None, cur                  # withheld: a currency, deliberately no number

    if form == "words_only":
        val = _num_from_words(span)
    else:
        val = _num_from_text(span)
        if val is not None:
            scale = _RE_SCALE.search(span)
            if scale is not None:
                val = val * SCALE[scale.group(0).lower()]

    if val is None:
        return None, cur
    return format(val.normalize(), "f"), cur

_TIMEOUT = 0


class _ExtractorTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _ExtractorTimeout()


def _init_worker(timeout):
    """Pool initializer: set the per-document cap and install the SIGALRM handler.

    Every pattern is compiled at import, so a worker started under spawn recompiles them
    rather than inheriting them. Nothing is set in main() and read in a worker.
    """
    global _TIMEOUT
    signal.signal(signal.SIGALRM, _alarm_handler)
    _TIMEOUT = timeout


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


def find_amounts(text):
    """Every monetary expression in one text, as (start, stop, span, form).

    Several patterns describe the same expression -- "5,000,000 Dollars" satisfies both the
    digits-then-word form and, from a later offset, the words-only form -- so matches are
    resolved longest-first and non-overlapping. Taking the shorter one would truncate the
    amount and leave a fragment that parses to the wrong number.
    """
    hits = []
    for name, rx in RX:
        for m in rx.finditer(text):
            if m.group(0).strip():
                hits.append((m.start(), m.end(), m.group(0), name))
    hits.sort(key=lambda h: (h[0], -(h[1] - h[0])))

    out = []
    last = -1
    for s, e, span, name in hits:
        if s >= last:
            out.append((s, e, span, name))
            last = e
    return out


def extract_one(args):
    """Rows for one document. Always returns >=1 row: a null-span sentinel if nothing is
    found (including blank text). The whole document is capped at _TIMEOUT seconds if set;
    on timeout it emits a marker row so the orchestrator can record the status."""
    docid, text = args
    rows = []
    if isinstance(text, str) and text.strip():
        try:
            if _TIMEOUT > 0:
                signal.alarm(_TIMEOUT)
            try:
                hits = find_amounts(text)
            finally:
                if _TIMEOUT > 0:
                    signal.alarm(0)
        except _ExtractorTimeout:
            print(f"[timeout] {docid}: moneyregex > {_TIMEOUT}s, skipped", file=sys.stderr)
            return [(docid, None, None, None, None, "timeout:moneyregex", ENGINE, MODEL,
                 None, None)]
        except Exception:                  # one bad document must not kill the run
            hits = []
        rows = []
        for s_, e_, sp_, form_ in hits:
            amount_, cur_ = parse_money(sp_, form_)
            rows.append((docid, s_, e_, sp_, LABEL, form_, ENGINE, MODEL, amount_, cur_))
    if not rows:
        rows.append((docid, None, None, None, None, None, ENGINE, MODEL, None, None))
    return rows


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--label", nargs="+", default=[LABEL],
                    help=f"unified labels to extract; this engine supports: {LABEL}")
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

    if LABEL not in args.label:
        print(f"no money extractor for {args.label}; writing sentinels only", file=sys.stderr)
        rows = [(docid, None, None, None, None, None, ENGINE, MODEL, None, None)
                for docid in df[args.id_col].tolist()]
        out = pd.DataFrame(rows, columns=COLUMNS)
        out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
        out[["Amount", "Currency"]] = out[["Amount", "Currency"]].astype("string")
        out.to_parquet(args.output, index=False)
        print(f"{len(df)} doc(s) -> 0 candidate(s)  [{ENGINE}:{MODEL}]")
        return

    texts = df[args.text_col].tolist()
    texts, n_trunc = truncate_texts(texts, args.max_chars)
    if n_trunc:
        print(f"{n_trunc} doc(s) truncated to {args.max_chars} chars", file=sys.stderr)
    items = list(zip(df[args.id_col].tolist(), texts))

    nproc = args.n_process if args.n_process > 0 else (os.cpu_count() or 1)
    desc = f"{ENGINE}:{MODEL} (n_process={nproc})"
    timeout = max(0, args.timeout)

    rows = []
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

    out = pd.DataFrame(rows, columns=COLUMNS)
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
    # Text across the seam, deliberately. A Decimal becomes a float in pandas, and a contract value
    # of 9,752,233.001 is exactly the kind of number that does not survive that intact.
    out[["Amount", "Currency"]] = out[["Amount", "Currency"]].astype("string")
    n_cand = int(out["Start"].notna().sum())
    n_amt = int(out["Amount"].notna().sum())
    mix = out["LabelRaw"].value_counts().to_dict()
    out.to_parquet(args.output, index=False)
    print(f"{len(df)} doc(s) -> {n_cand} candidate(s), {n_amt} with an amount  [{ENGINE}:{MODEL}]")
    if mix:
        print("  " + "  ".join(f"{k}={v}" for k, v in sorted(mix.items())), file=sys.stderr)


if __name__ == "__main__":
    main()
