#!/usr/bin/env python3
"""Gazetteer place extractor with offsets -- the paper's geographic lookup, gated.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model).

Stamps Engine = "paper", Model = MODEL. Label is always "GPE"; LabelRaw carries the
resolved GeoClass ("US State", "Country", "US County", "US Populated Place"), which is
what downstream aggregation pivots on. Coordinates and ISO3 are NOT emitted -- the
candidate schema has no room for them and they are recoverable by joining the uppercased
Span back to the lookup.

WHY THIS IS NOT A PLAIN DICTIONARY MATCH
The lookup holds 181,810 place names, and 31,925 of them are also English dictionary
words (the IsWord flag, built by testing each name against SCOWL). Matching them all
produces nonsense: "Enterprise", "Superior", "Eagle", "Mobile", "Reading" and "Bath" are
all US populated places. The original approach dropped every name flagged IsWord, which
removes the noise and, with it, 40 of the 50 US states -- including Delaware, California
and Texas, since single-word state names are all in the dictionary. The ten states that
survive are the multi-word ones (New York, North Carolina, Rhode Island and so on), so a
state-level geography built that way is a sample of state names by orthography.

This extractor keeps those names and gates them on context instead. A place name is
emitted when it is either self-evidencing or corroborated by a nearby anchor:

  ANCHOR      US State, or Country outside AMBIGUOUS_COUNTRIES. Emitted unconditionally.
              A US state name in a US commercial contract is a state; the handful of
              country names that are also ordinary nouns or given names are listed out
              and demoted rather than trusted.
  DISTINCTIVE Any other place with IsWord == 0. Emitted when an anchor occurs within
              --state-window characters. Loose, because the name itself carries most of
              the evidence.
  WORD-LIKE   Any other place with IsWord == 1. Emitted when an anchor occurs within
              --word-window characters. Strict, because the name carries none.

Distance is the gap between the nearest edges of the two spans, so "Palo Alto,
California" scores 2. A document with no anchor at all emits only its anchors, which is
to say nothing: a city named with no jurisdiction anywhere near it is not evidence about
where a contracting party sits, and that is the distinction the geography variable has to
support.

MATCHING
Names are matched as token n-grams, not as substrings, so "READING" inside "PROOFREADING"
cannot fire and no word-boundary regex is needed. Tokens are ASCII letter runs (the
lookup is ASCII-only by construction); each token is uppercased for the dictionary probe
while its offsets come from the original text, so casing never moves an offset. Matching
is longest-first and left-to-right without overlap, so "NEW YORK" wins over "YORK" and
consumes it.

Where one name belongs to several classes -- 1,701 do -- the class is resolved by
precedence US State > Country > US County > US Populated Place. "New York" is therefore a
state rather than one of the eight populated places sharing the name, and "Georgia" is a
state rather than a country. Both are the right reading in a US filing.

--max-chars N truncates every document to its first N characters BEFORE extraction
(0 = off). --timeout N caps each document (seconds; 0 = off); on timeout the document is
skipped and emits a marker row (LabelRaw = "timeout:gazetteer", null span) so the
orchestrator records Status = 'timeout' and can re-run it.

Offsets are 0-based, half-open, code-point indices: text[Start:Stop] == Span. Aligned
schema/CLI with extract_spacy.py / extract_lexnlp.py / extract_dateregex.py.
"""
import argparse
import bisect
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
MODEL = "gazetteer-v1"   # identifies THIS gate + lookup pairing; bump on any rule change
COLUMNS = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]

# Class precedence when one name carries several -- 1,701 names do. Lower index wins.
# Populated place outranks county deliberately: filings give addresses, and an address
# names a city. "Palo Alto" is a city in California and also a county in Iowa; "Mobile"
# is a city in Alabama and also the county around it. Reading both as cities is right far
# more often than not, and the county reading survives where the text says so, because
# "County of X" leaves X matching on its own.
CLASS_RANK = ["US State", "Country", "US Populated Place", "US County"]

# Classes that need no corroboration.
ANCHOR_CLASSES = {"US State", "Country"}

# Country names that are also ordinary English nouns or common given names. Left in the
# lookup but demoted out of the anchor set, so they must be corroborated like any other
# word-like name. Without this, every "turkey" and every person called Jordan is a
# geopolitical entity. EDITORIAL: this list is a judgement call and is meant to be read
# and argued with, not treated as settled.
# Names that also resolve to a US State are deliberately absent: "Georgia" is ambiguous
# between a state and a country, but both readings are geographic and precedence already
# picks the state, which is the right reading in a US filing.
AMBIGUOUS_COUNTRIES = {
    "CHAD", "GUINEA", "JERSEY", "JORDAN", "MALI", "TOGO", "TURKEY",
}

# Separators permitted between a dictionary-word place and the anchor that licenses it.
SEPARATOR_RX = re.compile(r"[\s,.;:()\[\]-]*")

# Single-token street-type suffixes. Each is a real populated place somewhere, and each is
# overwhelmingly an address component in a filing: "1 Chase Plaza, New York" would
# otherwise license Plaza on the comma alone. Dropped from the index entirely, which is
# safe for multi-word names -- "Overland Park" is a two-token entry and is untouched.
# EDITORIAL: like AMBIGUOUS_COUNTRIES, this list is meant to be read and argued with.
ADDRESS_WORDS = {
    "AVENUE", "BOULEVARD", "CIRCLE", "COURT", "DRIVE", "HIGHWAY", "LANE", "PARKWAY",
    "PLAZA", "ROAD", "STREET", "TERRACE", "TURNPIKE",
}

# Token = a run of ASCII letters, optionally carrying internal apostrophes. The lookup is
# ASCII-only (it was filtered on stri_enc_isascii when built), so anything outside this
# class cannot match and is skipped without loss.
TOKEN_RX = re.compile(r"[A-Za-z]+(?:'[A-Za-z]+)*")

# Set in main() before the pool forks.
_NAME_INFO = {}     # tuple(tokens) -> (GeoClass, IsWord)
_FIRST_LENS = {}    # first token -> tuple of n-gram lengths, longest first
_STATE_WINDOW = 200
_WORD_WINDOW = 40
_TIMEOUT = 0


class _ExtractorTimeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _ExtractorTimeout()


def _init_worker(lookup, state_window, word_window, timeout):
    """Pool initializer: build the index and install the SIGALRM handler in this process.

    The index is built per worker rather than built once and inherited. Inheriting works
    only under the fork start method; macOS and Windows default to spawn, where a worker
    re-imports this module and begins with empty globals. A fork-only design therefore
    matches nothing, raises nothing, and writes a sentinel for every document -- and the
    ledger records that as a clean no-hit, which is never re-run. Rebuilding costs a few
    seconds per worker, in parallel, once per invocation.

    The emptiness check converts the same failure into a crash if it ever recurs by
    another route: an extractor that finds nothing must fail loudly, not quietly.
    """
    global _NAME_INFO, _FIRST_LENS, _STATE_WINDOW, _WORD_WINDOW, _TIMEOUT
    signal.signal(signal.SIGALRM, _alarm_handler)
    _STATE_WINDOW = state_window
    _WORD_WINDOW = word_window
    _TIMEOUT = timeout
    _NAME_INFO, _FIRST_LENS = build_index(lookup)
    _demote_ambiguous()
    if not _NAME_INFO:
        raise SystemExit(f"gazetteer index is empty after loading {lookup}")


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


def build_index(lookup_path):
    """Collapse the lookup parquet into the two dictionaries the scanner needs.

    Returns (name_info, first_lens) where name_info maps a token tuple to its resolved
    (GeoClass, IsWord) and first_lens maps a first token to the n-gram lengths worth
    probing at that position, longest first. The second dictionary is what keeps the scan
    cheap: most tokens in a contract begin no place name at all, so they cost one failed
    dictionary probe rather than eleven.
    """
    df = pd.read_parquet(lookup_path, columns=["GeoName", "GeoClass", "IsWord"])
    rank = {c: i for i, c in enumerate(CLASS_RANK)}
    df = df.assign(Rank=df["GeoClass"].map(rank))
    if df["Rank"].isna().any():
        bad = sorted(set(df.loc[df["Rank"].isna(), "GeoClass"]))
        raise SystemExit(f"lookup carries unranked GeoClass values: {bad}")

    # One row per name: the most specific class, and IsWord set if any row flags it.
    df = (df.sort_values("Rank")
            .groupby("GeoName", as_index=False)
            .agg(GeoClass=("GeoClass", "first"), IsWord=("IsWord", "max")))

    name_info = {}
    first_lens = {}
    for name, klass, is_word in df.itertuples(index=False):
        toks = tuple(name.split())
        if not toks:
            continue
        name_info[toks] = (klass, int(is_word))
        first_lens.setdefault(toks[0], set()).add(len(toks))
    first_lens = {k: tuple(sorted(v, reverse=True)) for k, v in first_lens.items()}
    return name_info, first_lens


def scan(text):
    """Every non-overlapping gazetteer match in one text, longest-first, left to right.

    Returns a list of (start, stop, geo_class, is_word). Offsets index `text` directly.
    """
    toks = [(m.group(0).upper(), m.start(), m.end(), m.group(0)[0].isupper())
            for m in TOKEN_RX.finditer(text)]
    n = len(toks)
    out = []
    i = 0
    while i < n:
        lens = _FIRST_LENS.get(toks[i][0]) if toks[i][3] else None
        if lens is not None:
            matched = 0
            for L in lens:                       # longest first
                if i + L > n:
                    continue
                info = _NAME_INFO.get(tuple(toks[j][0] for j in range(i, i + L)))
                if info is not None:
                    out.append((toks[i][1], toks[i + L - 1][2], info[0], info[1]))
                    matched = L
                    break
            if matched:
                i += matched                     # consume the whole match
                continue
        i += 1
    return out


def gate(matches, text):
    """Keep the anchors, plus the gated matches that an anchor corroborates.

    Anchors are the self-evidencing classes; every other match must have one within a
    window whose width depends on whether the name is also a dictionary word. Distance is
    the gap between the nearest edges of the two spans, so an adjacent anchor scores 0 and
    the measure does not punish long place names.
    """
    anchors = [(s, e) for s, e, k, _ in matches if k in ANCHOR_CLASSES]
    if not anchors:
        return []

    anchor_spans = set(anchors)
    kept = []
    for s, e, klass, is_word in matches:
        if (s, e) in anchor_spans and klass in ANCHOR_CLASSES:
            kept.append((s, e, klass))
        elif is_word:
            if _adjacent_anchor(e, anchors, text):
                kept.append((s, e, klass))
        elif _nearest_gap(s, e, anchors) <= _STATE_WINDOW:
            kept.append((s, e, klass))
    kept.sort()
    return kept


def _adjacent_anchor(stop, anchors, text):
    """True when an anchor follows within --word-window characters and nothing but
    separators lies between.

    Plain proximity cannot gate a dictionary word, because an address block is dense with
    anchors and licenses every token in it: in "400 Hamilton Avenue, Palo Alto,
    California", the state sits within forty characters of Hamilton, Avenue and Palo Alto
    alike. What distinguishes the city is that it abuts the state with only a comma
    between them, which is how US addresses are written. Requiring a separator-only gap
    therefore keeps "Mobile, Alabama" and drops the street it stands on.

    Anchors are position-sorted, so the search bisects to the first candidate rather than
    walking from the start. Scanning linearly here is quadratic in the number of matches,
    which is invisible on a normal contract and fatal on the multi-megabyte outliers the
    corpus contains.
    """
    idx = bisect.bisect_left(anchors, (stop, -1))   # first anchor starting at or after stop
    for j in range(idx, len(anchors)):
        a_s = anchors[j][0]
        if a_s - stop > _WORD_WINDOW:
            break
        if SEPARATOR_RX.fullmatch(text[stop:a_s]):
            return True
    return False


def _nearest_gap(start, stop, anchors):
    """Smallest edge-to-edge gap between [start, stop) and any anchor span (0 if they
    touch or overlap). Anchors are sorted, so the search is a bisect plus two probes."""
    idx = bisect.bisect_left(anchors, (start, stop))
    best = None
    for j in (idx - 1, idx, idx + 1):
        if 0 <= j < len(anchors):
            a_s, a_e = anchors[j]
            if a_s < stop and start < a_e:      # overlapping spans are zero distance
                gap = 0
            else:
                gap = a_s - stop if a_s >= stop else start - a_e
            if best is None or gap < best:
                best = gap
    return best if best is not None else 10 ** 9


def extract_one(args):
    """Rows for one document. Always returns >=1 row: a null-span sentinel if nothing
    survives the gate (including blank text). The whole document is capped at _TIMEOUT
    seconds if set; on timeout it emits a marker row so the orchestrator can record the
    status and re-run it later."""
    docid, text = args
    rows = []
    if isinstance(text, str) and text.strip():
        try:
            if _TIMEOUT > 0:
                signal.alarm(_TIMEOUT)
            try:
                kept = gate(scan(text), text)
            finally:
                if _TIMEOUT > 0:
                    signal.alarm(0)
        except _ExtractorTimeout:
            print(f"[timeout] {docid}: gazetteer > {_TIMEOUT}s, skipped", file=sys.stderr)
            return [(docid, None, None, None, None, "timeout:gazetteer", ENGINE, MODEL)]
        except Exception:                  # one bad document must not kill the run
            kept = []
        rows = [(docid, s, e, text[s:e], "GPE", klass, ENGINE, MODEL) for s, e, klass in kept]
    if not rows:
        rows.append((docid, None, None, None, None, None, ENGINE, MODEL))
    return rows


def _demote_ambiguous():
    """Drop the street-type suffixes and move ambiguous country names out of the anchor set.

    They stay matchable, but as word-like names needing corroboration, by relabelling
    them to the class they also hold as a US place where they have one and forcing the
    IsWord flag otherwise. Done once after the index is built so the scanner itself stays
    free of special cases.
    """
    for name in ADDRESS_WORDS:
        _NAME_INFO.pop((name,), None)

    for name in AMBIGUOUS_COUNTRIES:
        key = tuple(name.split())
        info = _NAME_INFO.get(key)
        if info is None:
            continue
        klass, _ = info
        if klass != "Country":          # a US State reading is never demoted
            continue
        _NAME_INFO[key] = ("US Populated Place", 1)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--label", nargs="+", default=["GPE"],
                    help="unified labels to extract; this engine supports: GPE")
    ap.add_argument("--lookup", required=True, help="geo_lookup.parquet")
    ap.add_argument("--state-window", type=int, default=200,
                    help="chars within which an anchor licenses a distinctive name")
    ap.add_argument("--word-window", type=int, default=40,
                    help="chars within which an anchor licenses a dictionary-word name")
    ap.add_argument("--max-chars", type=int, default=0,
                    help="truncate each document to its first N characters (0 = off)")
    ap.add_argument("--timeout", type=int, default=120,
                    help="per-document cap in seconds (0 = off)")
    ap.add_argument("--n-process", type=int, default=1, help="worker processes (<=0 = all cores)")
    ap.add_argument("--chunk-size", type=int, default=8, help="docs per task when parallelising")
    ap.add_argument("--no-progress", action="store_true", help="disable the progress bar")
    args = ap.parse_args()

    init_args = (args.lookup, max(0, args.state_window), max(0, args.word_window),
                 max(0, args.timeout))

    df = (pads.dataset(resolve_inputs(args.inputs), format="parquet")
              .to_table(columns=[args.id_col, args.text_col])
              .to_pandas())

    if "GPE" not in args.label:
        print(f"no gazetteer extractor for {args.label}; writing sentinels only", file=sys.stderr)
        rows = [(docid, None, None, None, None, None, ENGINE, MODEL)
                for docid in df[args.id_col].tolist()]
        out = pd.DataFrame(rows, columns=COLUMNS)
        out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
        out.to_parquet(args.output, index=False)
        print(f"{len(df)} doc(s) -> 0 candidate(s)  [{ENGINE}:{MODEL}]")
        return

    # Cheap up-front validation so a bad lookup path fails here rather than inside every
    # worker at once. The index itself is built per worker; see _init_worker.
    n_names = len(pd.read_parquet(args.lookup, columns=["GeoName"]))
    print(f"gazetteer: {n_names} lookup row(s), state-window={init_args[1]}, "
          f"word-window={init_args[2]}", file=sys.stderr)

    texts = df[args.text_col].tolist()
    texts, n_trunc = truncate_texts(texts, args.max_chars)
    if n_trunc:
        print(f"{n_trunc} doc(s) truncated to {args.max_chars} chars", file=sys.stderr)
    items = list(zip(df[args.id_col].tolist(), texts))

    nproc = args.n_process if args.n_process > 0 else (os.cpu_count() or 1)
    desc = f"{ENGINE}:{MODEL} (n_process={nproc})"

    rows = []
    if nproc == 1:
        _init_worker(*init_args)
        for it in tqdm(items, total=len(items), unit="doc", desc=desc,
                       file=sys.stderr, disable=args.no_progress):
            rows.extend(extract_one(it))
    else:
        with Pool(processes=nproc, initializer=_init_worker, initargs=init_args) as pool:
            for r in tqdm(pool.imap_unordered(extract_one, items, chunksize=args.chunk_size),
                          total=len(items), unit="doc", desc=desc,
                          file=sys.stderr, disable=args.no_progress):
                rows.extend(r)

    out = pd.DataFrame(rows, columns=COLUMNS)
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
    n_cand = int(out["Start"].notna().sum())
    out.to_parquet(args.output, index=False)
    print(f"{len(df)} doc(s) -> {n_cand} candidate(s)  [{ENGINE}:{MODEL}]")


if __name__ == "__main__":
    main()
