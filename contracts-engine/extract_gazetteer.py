#!/usr/bin/env python3
"""Hierarchical gazetteer GPE extractor with offsets -- the paper's USGS+countries
place-name lookup, ported with PROXIMITY-based structural disambiguation.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model).

Matches a place-name dictionary (geo_lookup.parquet: Country / US State / US County /
US Populated Place) against each document via spaCy's PhraseMatcher (case-insensitive
via attr=LOWER, so it matches TextRaw directly -- offsets stay valid against the
canonical text), then applies a HIERARCHY + PROXIMITY GATE:
  Country, US State  -- anchors: always kept (closed curated sets)
  US County, US Place -- gated: kept iff one of the name's parent states is matched
                         WITHIN A CHARACTER WINDOW of this match in the same doc.
The window depends on whether the name is also a common English word (IsWord):
  IsWord == 0 (distinctive, e.g. CHARLOTTESVILLE)  -> loose window (--state-window)
  IsWord == 1 (collision,  e.g. MOBILE, READING)   -> strict window (--word-window)
A doc-global gate failed empirically: contracts always name some state, so every
place in that state passed -- function-word towns (ALL, MAY, SECTION) flooded the
output (98% of place hits were IsWord collisions). Requiring the parent state to be
*near* the match restores the link: "Mobile, Alabama" survives (Alabama adjacent);
"mobile workforce" does not (no nearby state). Tight window for collisions rejects
prose noise while rescuing genuine "Place, State" references that lexical exclusion
would have dropped.

NOTE: the default windows are UNTESTED starting values -- sweep on the sample and
tune (--state-window / --word-window) before trusting at scale.

Engine = "paper", Model = "gazetteer-v1", Label = "GPE", LabelRaw = the GeoClass.
Overlaps resolved longest-span-first. Every input DocID appears at least once (null-
span sentinel if nothing survives; marker row on timeout). --timeout caps matching per
doc. --max-chars truncates before matching. Offsets are 0-based, half-open, code-point:
text[Start:Stop] == Span. Aligned schema/CLI with the other extractors.

macOS uses spawn (not fork): workers rebuild the matcher in their initializer.
"""
import argparse
import bisect
import os
import signal
import sys
from collections import defaultdict
from multiprocessing import Pool
from pathlib import Path

import pandas as pd
import pyarrow.dataset as pads
import spacy
from spacy.matcher import PhraseMatcher
from tqdm import tqdm

ENGINE = "paper"
MODEL = "gazetteer-v1"
COLUMNS = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]
ANCHOR_CLASSES = {"Country", "US State"}
GATED_CLASSES = {"US County", "US Populated Place"}
LOOKUP_DEFAULT = "/app/geo_lookup.parquet"   # overridden by --lookup

_NLP = None
_MATCHER = None
_NAME_META = None     # lower(name) -> {classes, states, isword}
_TIMEOUT = 0
_STATE_WINDOW = 200   # loose window (chars) for distinctive gated names
_WORD_WINDOW = 40     # strict window (chars) for common-word gated names


class _Timeout(Exception):
    pass


def _alarm_handler(signum, frame):
    raise _Timeout()


def resolve_inputs(paths):
    files = []
    for p in paths:
        pth = Path(p)
        files += sorted(str(f) for f in pth.rglob("*.parquet")) if pth.is_dir() else [str(pth)]
    if not files:
        raise SystemExit("no parquet files found in the given path(s)")
    return files


def truncate_texts(texts, max_chars):
    if not max_chars or max_chars <= 0:
        return texts, 0
    n = sum(1 for t in texts if isinstance(t, str) and len(t) > max_chars)
    if n:
        texts = [t[:max_chars] if isinstance(t, str) and len(t) > max_chars else t for t in texts]
    return texts, n


def build_matcher(lookup_path):
    """Blank-English PhraseMatcher over all distinct names + a name->meta map.

    Every name is matched (anchors AND gated, incl. common words) -- unlike the
    lexical-exclusion approach, the proximity gate decides at match time. meta keeps
    `isword` so pass 2 picks the loose vs strict window for gated names.

    _NAME_META[lower(name)] = {classes: set, states: set(parent states, UPPER),
                               isword: 0/1}. Returns (nlp, matcher, name_meta).
    """
    if not os.path.exists(lookup_path):
        raise SystemExit(f"gazetteer lookup not found: {lookup_path}")
    lk = pd.read_parquet(lookup_path, columns=["GeoName", "GeoClass", "ParentState", "IsWord"])

    name_meta = defaultdict(lambda: {"classes": set(), "states": set(), "isword": 0})
    for geoname, geoclass, parent, isword in zip(
            lk["GeoName"], lk["GeoClass"], lk["ParentState"], lk["IsWord"]):
        key = str(geoname).lower()
        m = name_meta[key]
        m["classes"].add(geoclass)
        m["isword"] = max(m["isword"], int(isword))   # any class flags it a word -> word
        if geoclass == "US State":
            m["states"].add(str(geoname))              # state anchors its own presence
        elif isinstance(parent, str) and parent:
            m["states"].add(parent)                    # gated name -> parent state(s)

    nlp = spacy.blank("en")
    nlp.max_length = 5_000_000
    matcher = PhraseMatcher(nlp.vocab, attr="LOWER")
    patterns = list(nlp.tokenizer.pipe(name_meta.keys()))
    matcher.add("GEO", patterns)
    return nlp, matcher, dict(name_meta)


def _init_worker(lookup_path, timeout, state_window, word_window):
    """Spawn-safe: install alarm handler + build matcher in this worker process."""
    global _NLP, _MATCHER, _NAME_META, _TIMEOUT, _STATE_WINDOW, _WORD_WINDOW
    signal.signal(signal.SIGALRM, _alarm_handler)
    _TIMEOUT = timeout
    _STATE_WINDOW = state_window
    _WORD_WINDOW = word_window
    _NLP, _MATCHER, _NAME_META = build_matcher(lookup_path)


def _resolve_overlaps(cands):
    """cands: list of (start, stop, geoclass). Longest-span-first greedy keep."""
    cands.sort(key=lambda c: (-(c[1] - c[0]), c[0]))
    kept = []
    for s, e, gc in cands:
        if all(e <= ks or s >= ke for ks, ke, _ in kept):
            kept.append((s, e, gc))
    kept.sort()
    return kept


def _near_state(match_mid, parent_states, state_positions, window):
    """True iff any parent state of this name has an occurrence whose midpoint is
    within `window` chars of match_mid. state_positions: {STATE -> sorted [mids]}."""
    for st in parent_states:
        mids = state_positions.get(st)
        if not mids:
            continue
        i = bisect.bisect_left(mids, match_mid)
        for j in (i - 1, i):                       # nearest on each side
            if 0 <= j < len(mids) and abs(mids[j] - match_mid) <= window:
                return True
    return False


def extract_one(args):
    """Rows for one document: PhraseMatch, hierarchy+proximity gate, overlap-resolve.
    >=1 row always (sentinel if nothing survives; marker on timeout)."""
    docid, text = args
    if not (isinstance(text, str) and text.strip()):
        return [(docid, None, None, None, None, None, ENGINE, MODEL)]

    try:
        if _TIMEOUT > 0:
            signal.alarm(_TIMEOUT)
        try:
            doc = _NLP.make_doc(text)
            matches = _MATCHER(doc)
        finally:
            if _TIMEOUT > 0:
                signal.alarm(0)
    except _Timeout:
        print(f"[timeout] {docid}: gazetteer > {_TIMEOUT}s, skipped", file=sys.stderr)
        return [(docid, None, None, None, None, "timeout:gazetteer", ENGINE, MODEL)]

    if not matches:
        return [(docid, None, None, None, None, None, ENGINE, MODEL)]

    # Pass 1: collect matches (span + meta + midpoint) and record WHERE each state
    # name occurs (state -> sorted midpoints), for the proximity test in pass 2.
    raw = []                                   # (start, stop, meta, mid)
    state_positions = defaultdict(list)
    for _, tok_s, tok_e in matches:
        span = doc[tok_s:tok_e]
        meta = _NAME_META.get(span.text.lower())
        if meta is None:
            continue
        s, e = span.start_char, span.end_char
        mid = (s + e) // 2
        raw.append((s, e, meta, mid))
        if "US State" in meta["classes"]:
            for st in meta["states"]:
                state_positions[st].append(mid)
    for st in state_positions:
        state_positions[st].sort()

    # Pass 2: gate. Anchors always; gated kept iff a parent state is within the
    # window (loose for distinctive names, strict for common-word collisions).
    cands = []
    for s, e, meta, mid in raw:
        classes = meta["classes"]
        chosen = None
        if classes & ANCHOR_CLASSES:
            chosen = "Country" if "Country" in classes else "US State"
        elif classes & GATED_CLASSES:
            window = _WORD_WINDOW if meta["isword"] else _STATE_WINDOW
            if _near_state(mid, meta["states"], state_positions, window):
                chosen = "US County" if "US County" in classes else "US Populated Place"
        if chosen is not None:
            cands.append((s, e, chosen))

    if not cands:
        return [(docid, None, None, None, None, None, ENGINE, MODEL)]

    kept = _resolve_overlaps(cands)
    return [(docid, s, e, text[s:e], "GPE", gc, ENGINE, MODEL) for s, e, gc in kept]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True)
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--label", nargs="+", default=["GPE"], help="this engine supports: GPE")
    ap.add_argument("--lookup", default=LOOKUP_DEFAULT, help="geo_lookup.parquet path")
    ap.add_argument("--state-window", type=int, default=200,
                    help="char window for distinctive (non-word) gated names")
    ap.add_argument("--word-window", type=int, default=40,
                    help="char window for common-word gated names (strict)")
    ap.add_argument("--max-chars", type=int, default=0)
    ap.add_argument("--timeout", type=int, default=120, help="per-doc cap, seconds (0 = off)")
    ap.add_argument("--n-process", type=int, default=1)
    ap.add_argument("--chunk-size", type=int, default=8)
    ap.add_argument("--no-progress", action="store_true")
    args = ap.parse_args()

    global _NLP, _MATCHER, _NAME_META, _TIMEOUT, _STATE_WINDOW, _WORD_WINDOW
    _TIMEOUT = max(0, args.timeout)
    _STATE_WINDOW = args.state_window
    _WORD_WINDOW = args.word_window

    if "GPE" not in args.label:
        df = (pads.dataset(resolve_inputs(args.inputs), format="parquet")
                  .to_table(columns=[args.id_col]).to_pandas())
        out = pd.DataFrame(
            [(d, None, None, None, None, None, ENGINE, MODEL) for d in df[args.id_col]],
            columns=COLUMNS)
        out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
        out.to_parquet(args.output, index=False)
        print(f"GPE not requested; wrote {len(out)} sentinels", file=sys.stderr)
        return

    df = (pads.dataset(resolve_inputs(args.inputs), format="parquet")
              .to_table(columns=[args.id_col, args.text_col]).to_pandas())
    texts = df[args.text_col].tolist()
    texts, n_trunc = truncate_texts(texts, args.max_chars)
    if n_trunc:
        print(f"{n_trunc} doc(s) truncated to {args.max_chars} chars", file=sys.stderr)
    items = list(zip(df[args.id_col].tolist(), texts))

    nproc = args.n_process if args.n_process > 0 else (os.cpu_count() or 1)
    desc = f"{ENGINE}:{MODEL} (n_process={nproc})"

    rows = []
    if nproc == 1:
        signal.signal(signal.SIGALRM, _alarm_handler)
        _NLP, _MATCHER, _NAME_META = build_matcher(args.lookup)
        print(f"matcher built: {len(_NAME_META)} distinct names "
              f"(state_window={_STATE_WINDOW}, word_window={_WORD_WINDOW})", file=sys.stderr)
        for it in tqdm(items, total=len(items), unit="doc", desc=desc,
                       file=sys.stderr, disable=args.no_progress):
            rows.extend(extract_one(it))
    else:
        with Pool(processes=nproc, initializer=_init_worker,
                  initargs=(args.lookup, _TIMEOUT, _STATE_WINDOW, _WORD_WINDOW)) as pool:
            for r in tqdm(pool.imap_unordered(extract_one, items, chunksize=args.chunk_size),
                          total=len(items), unit="doc", desc=desc,
                          file=sys.stderr, disable=args.no_progress):
                rows.extend(r)

    out = pd.DataFrame(rows, columns=COLUMNS)
    out[["Start", "Stop"]] = out[["Start", "Stop"]].astype("Int64")
    n_cand = int(out["Start"].notna().sum())
    out.to_parquet(args.output, index=False)
    print(f"{len(df)} doc(s) -> {n_cand} candidate(s) "
          f"[{ENGINE}:{MODEL}, sw={_STATE_WINDOW}, ww={_WORD_WINDOW}, n_process={nproc}]")


if __name__ == "__main__":
    main()
