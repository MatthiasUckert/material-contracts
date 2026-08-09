#!/usr/bin/env python3
"""Regex money extractor with offsets -- a rule arm for the one label that had none.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model).

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
from multiprocessing import Pool
from pathlib import Path

import pandas as pd
import pyarrow.dataset as pads
from tqdm import tqdm

ENGINE = "paper"
MODEL = "moneyregex-v2"   # identifies THIS pattern set; bump on any rule change
LABEL = "MONEY"
COLUMNS = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]

# Symbols that survive HTML-to-text conversion, and the national prefixes that precede them.
# R$ and US$ both end in the dollar sign, so the prefix is optional rather than enumerated twice.
# \u0080 is the euro sign mangled by a cp1252 round trip, which is how it survives conversion in a
# fair number of these filings: 42 spans across 24 documents that a proper euro class never sees.
CUR_SYM = "[$\u00a3\u20ac\u00a5\u0080]"
PREFIX = r"(?:US|R|C|A|HK|S|NZ)?"

# Whitespace between a currency marker and its figure is routine in converted filings, so the gap
# is up to three characters rather than one. A single optional space misses "$  5,000" outright,
# and 579 of the 2,125 currency-adjacent redaction sites carry whitespace before the marker.
#
# ISO codes and spelt currency names, both with and without a space before the figure. Contracts
# with a foreign counterparty write "EUR 11,848,000" and "RMB10,000,000" and nothing else, so a
# symbol-only arm returns no money at all for them -- the gap is small in total volume and total
# for the documents it affects.
ISO = r"(?:USD|EUR|GBP|CHF|JPY|CAD|AUD|CNY|RMB|HKD|SGD|NZD|SEK|NOK|DKK)"
CUR_NAME = r"(?:euros?|dollars?|pounds?|renminbi|yen)"

# Separators must be CONSISTENT. Allowing either as a group separator lets a comma-grouped figure
# run past its own decimal point into the next number: "$9,752,233.001.857" was captured whole,
# 71 times, and parses to nothing. Commas group with a dot decimal, dots group with a comma
# decimal, and the two are never mixed.
NUM = (
    r"\d{1,3}(?:,\d{3})+(?:\.\d+)?"     # 1,234,567.89   US convention
    r"|\d{1,3}(?:\.\d{3})+(?:,\d+)?"     # 1.234.567,89   European convention
    r"|\d+\.\d+|\d+"
)

# Numerals as words. "and" is NOT in this list: a pattern admitting it would match "and Dollars"
# in ordinary running text, which occurs constantly and is not an amount.
NUMWORD = (
    r"(?:one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|thirteen|fourteen|"
    r"fifteen|sixteen|seventeen|eighteen|nineteen|twenty|thirty|forty|fifty|sixty|seventy|"
    r"eighty|ninety|hundred|thousand|million|billion|trillion)"
)
CUR_WORD = r"(?:dollars?|euros?|pounds?)"

# Order matters only for reporting; overlaps are resolved by length in find_amounts().
PATTERNS = [
    # $5,000,000  US$10,000  R$1.500.000  EUR185,000,000  $0.001
    ("symbol_amount", rf"{PREFIX}{CUR_SYM}\s{{0,3}}(?:{NUM})"),
    # EUR 11,848,000  USD9,750,000  RMB10,000,000
    ("iso_amount", rf"(?<![A-Za-z]){ISO}\s{{0,3}}(?:{NUM})"),
    # Euro 6.667.856,00 -- the name spelt out, and European separators with it
    ("name_amount", rf"(?<![A-Za-z]){CUR_NAME}\s{{0,3}}(?:{NUM})"),
    # $[***]  $**  $TBD  $____  -- the amount was withheld and the symbol survived. Bare asterisks
    # are included because a redaction is not always bracketed: "$**" occurs 43 times.
    ("symbol_redact", rf"{PREFIX}{CUR_SYM}\s{{0,3}}(?:\[[^\]]{{0,40}}\]|\*{{1,6}}|TBD|_{{2,}})"),
    # "a minimum market price of $ per share" -- withheld with nothing at all left behind
    ("symbol_bare", rf"{PREFIX}{CUR_SYM}(?=\s+per\b)"),
    # A euro amount whose symbol became a capital E in conversion. Grouped thousands are required:
    # without them "E12" matches an exhibit number, a clause label and half the alphabet soup in a
    # filing header.
    ("euro_letter", r"(?<![A-Za-z])E\s?\d{1,3}(?:,\d{3})+(?:\.\d+)?"),
    # 5,000,000 Dollars
    ("amount_word", rf"(?:{NUM})\s+{CUR_WORD}\b"),
    # SIXTY-ONE THOUSAND NINETY AND 90/100 Dollars
    ("words_only",
     rf"(?<![A-Za-z])(?:{NUMWORD}[-\s]+)+(?:(?:and|[a-z]+)[-\s]+)*"
     rf"(?:\d{{1,2}}/100\s+)?{CUR_WORD}\b"),
]
RX = [(name, re.compile(pat, re.IGNORECASE)) for name, pat in PATTERNS]

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
            return [(docid, None, None, None, None, "timeout:moneyregex", ENGINE, MODEL)]
        except Exception:                  # one bad document must not kill the run
            hits = []
        rows = [(docid, s, e, sp, LABEL, form, ENGINE, MODEL) for s, e, sp, form in hits]
    if not rows:
        rows.append((docid, None, None, None, None, None, ENGINE, MODEL))
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
        rows = [(docid, None, None, None, None, None, ENGINE, MODEL)
                for docid in df[args.id_col].tolist()]
        out = pd.DataFrame(rows, columns=COLUMNS)
        out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
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
    n_cand = int(out["Start"].notna().sum())
    mix = out["LabelRaw"].value_counts().to_dict()
    out.to_parquet(args.output, index=False)
    print(f"{len(df)} doc(s) -> {n_cand} candidate(s)  [{ENGINE}:{MODEL}]")
    if mix:
        print("  " + "  ".join(f"{k}={v}" for k, v in sorted(mix.items())), file=sys.stderr)


if __name__ == "__main__":
    main()
