"""Place names, by gated gazetteer lookup, with offsets.

Emits one label. `LabelRaw` carries the resolved `GeoClass` -- "US State", "Country", "US County",
"US Populated Place" -- which is what downstream aggregation pivots on.

WHY THIS IS NOT A PLAIN DICTIONARY MATCH
The lookup holds 181,810 place names and 31,925 of them are also English dictionary words (the
`IsWord` flag, built by testing each name against SCOWL). Matching them all produces nonsense:
Enterprise, Superior, Eagle, Mobile, Reading and Bath are all US populated places. The original
approach dropped every name flagged `IsWord`, which removes the noise and, with it, 40 of the 50 US
states -- including Delaware, California and Texas, since single-word state names are all in the
dictionary. The ten that survive are the multi-word ones, so a state-level geography built that way
is a sample of state names by orthography.

This extractor keeps those names and gates them on context instead:

  ANCHOR       US State, or Country outside AMBIGUOUS_COUNTRIES. Emitted unconditionally. A US
               state name in a US commercial contract is a state; the handful of country names that
               are also ordinary nouns or given names are listed out and demoted.
  DISTINCTIVE  Any other place with IsWord == 0. Emitted when an anchor occurs within STATE_WINDOW
               characters. Loose, because the name itself carries most of the evidence.
  WORD-LIKE    Any other place with IsWord == 1. Emitted when an anchor follows within WORD_WINDOW
               characters AND nothing but separators lies between.

A document with no anchor at all emits nothing: a city named with no jurisdiction anywhere near it
is not evidence about where a contracting party sits, and that is the distinction the geography
variable has to support.

MATCHING
Names are matched as token n-grams, not substrings, so READING inside PROOFREADING cannot fire and
no word-boundary regex is needed. Tokens are ASCII letter runs; each is uppercased for the
dictionary probe while its offsets come from the original text, so casing never moves an offset.
Matching is longest-first and left to right without overlap, so NEW YORK wins over YORK and consumes
it.

Where one name belongs to several classes -- 1,701 do -- the class resolves by precedence
US State > Country > US Populated Place > US County. New York is a state rather than one of the
eight populated places sharing the name, and Georgia is a state rather than a country. Both are the
right reading in a US filing.

WHAT v2 ADDS, AND WHY
v1 emitted core columns only, so `matcon` GPE rows had every declared extra null while LexNLP's were
filled -- the exact asymmetry per-label extras exist to remove. 04B2 recovered the rest by rejoining
the lookup in R on the uppercased span, which can disagree with what the extractor decided:
`_demote_ambiguous()` rewrites the class of ambiguous country names and drops street-type suffixes
AT INDEX-BUILD TIME, and a join against the raw lookup gets the un-demoted class.

  GeoKey    the lookup key that matched, so a consumer stops re-deriving the normalisation
  IsWord    the name is also a dictionary word, so it needed corroboration
  NParent   how many distinct parents the name has. A resolver needs to know WASHINGTON is
            ambiguous before it picks one, and this is the cheapest way to say so
  Iso2      where the winning class resolves it: all 50 states, 81% of populated places, 95% of
            counties, and NEVER a country
  Iso3      where the winning class resolves it: every country, and USA for every US entity
  MatchKind anchor or corroborated -- the extractor's own gating decision, invisible until now

RESOLUTION IS SCOPED TO THE WINNING CLASS, AND THAT IS NOT A DETAIL. Collapsing a name across all
its rows and taking the single distinct value produces `FRANCE -> Iso2 = US-ID`: there is a town
called France in Idaho, the country carries no Iso2 at all, so the town's is the only non-null value
in the group. Only 11 of 50 states resolve an Iso2 that way, because most state names are also towns
elsewhere. Scoped to the winning class, all 50 do and France resolves ISO3 = FRA with Iso2 null.

WHAT IS NOT EMITTED. `ParentCounty`. `build_index()` collapses the lookup to one row per name, so at
match time the extractor has a name and a class and no parent at all -- and SPRINGFIELD is a
populated place in 33 states. Emitting one of them would be inventing a value, which is the thing
this package exists not to do. `NParent` is what tells a consumer why the column is absent.

THE WINDOWS ARE CONSTANTS, NOT FLAGS. They change output, so leaving them as runtime arguments meant
`gazetteer-v1` named a rule set that two runs could disagree about. They are in SPEC, and changing
one is a version bump.

THE LOOKUP IS PART OF THE VERSION. Its content hash folds into SPEC, because rebuilding
`geo_lookup.parquet` moves the spans under an unchanged model tag otherwise. The last rebuild was
safe only by accident: it added columns, and the scanner reads a handful.
"""
from __future__ import annotations

import bisect
import re
import sys
from pathlib import Path

import pandas as pd

from . import _io

MODEL = "gazetteer-v2"      # THE version. The filename carries none; see _io.spec_hash().
NAME = "gazetteer"
LABELS = ("GPE",)
EXTRAS = ("GeoKey", "IsWord", "NParent", "Iso2", "Iso3", "MatchKind")

DEFAULT_TIMEOUT = 120       # this extractor's per-document cost is orders above the regex passes
DEFAULT_CHUNK_SIZE = 8

#: Ships with the package. Overridable for a rebuild, not for a run.
DEFAULT_LOOKUP = Path(__file__).parent / "data" / "geo_lookup.parquet"


# 1. The rules -----------------------------------------------------------------------------------

# Class precedence when one name carries several. Lower index wins. Populated place outranks county
# DELIBERATELY: filings give addresses, and an address names a city. "Palo Alto" is a city in
# California and also a county in Iowa; "Mobile" is a city in Alabama and also the county around it.
# Reading both as cities is right far more often than not, and the county reading survives where the
# text says so, because "County of X" leaves X matching on its own.
CLASS_RANK = ("US State", "Country", "US Populated Place", "US County")

#: Classes that need no corroboration.
ANCHOR_CLASSES = frozenset({"US State", "Country"})

# Country names that are also ordinary English nouns or common given names. Left in the lookup but
# demoted out of the anchor set, so they must be corroborated like any other word-like name. Without
# this, every "turkey" and every person called Jordan is a geopolitical entity.
# EDITORIAL: a judgement call, meant to be read and argued with rather than treated as settled.
# Names that also resolve to a US State are deliberately absent: "Georgia" is ambiguous between a
# state and a country, but both readings are geographic and precedence already picks the state.
AMBIGUOUS_COUNTRIES = ("CHAD", "GUINEA", "JERSEY", "JORDAN", "MALI", "TOGO", "TURKEY")

# Single-token street-type suffixes. Each is a real populated place somewhere, and each is
# overwhelmingly an address component in a filing: "1 Chase Plaza, New York" would otherwise license
# Plaza on the comma alone. Dropped from the index entirely, which is safe for multi-word names --
# "Overland Park" is a two-token entry and is untouched.
# EDITORIAL: like AMBIGUOUS_COUNTRIES, meant to be read and argued with.
ADDRESS_WORDS = ("AVENUE", "BOULEVARD", "CIRCLE", "COURT", "DRIVE", "HIGHWAY", "LANE", "PARKWAY",
                 "PLAZA", "ROAD", "STREET", "TERRACE", "TURNPIKE")

#: Separators permitted between a dictionary-word place and the anchor that licenses it.
P_SEPARATOR = r"[\s,.;:()\[\]-]*"

#: A run of ASCII letters, optionally carrying internal apostrophes. The lookup is ASCII-only by
#: construction, so anything outside this class cannot match and is skipped without loss.
P_TOKEN = r"[A-Za-z]+(?:'[A-Za-z]+)*"

STATE_WINDOW = 200      # distinctive names: an anchor anywhere within this many characters
WORD_WINDOW = 40        # dictionary words: an anchor this close AND only separators between

SEPARATOR_RX = re.compile(P_SEPARATOR)
TOKEN_RX = re.compile(P_TOKEN)


# 2. Identity ------------------------------------------------------------------------------------

def spec(lookup_path=DEFAULT_LOOKUP):
    """The constants that determine this extractor's output, including its data.

    THE LOOKUP IS PART OF THE VERSION. A gazetteer's spans are a function of its dictionary as much
    as of its gate, so a SPEC that named only the rules would let a rebuilt lookup change the output
    under an unchanged model tag.

    :param lookup_path: the geo lookup this run will read.
    :return: dict suitable for _io.spec_hash().
    """
    out = {
        "class_rank": CLASS_RANK,
        "anchor_classes": sorted(ANCHOR_CLASSES),
        "ambiguous_countries": AMBIGUOUS_COUNTRIES,
        "address_words": ADDRESS_WORDS,
        "separator": P_SEPARATOR,
        "token": P_TOKEN,
        "state_window": STATE_WINDOW,
        "word_window": WORD_WINDOW,
        "extras": EXTRAS,
        # THE CONTEXT WIDTH IS PART OF THIS EXTRACTOR'S IDENTITY, for the same reason the lookup is.
        # A store holding rows cut at 160 beside rows cut at 0 under one model tag would give a rule
        # full context from some spans and none from others, with nothing to say which.
        "cue": _io.DEFAULT_CUE,
    }
    path = Path(lookup_path)
    out["lookup"] = _io.file_hash(path) if path.exists() else "absent"
    return out


SPEC = spec()
# THE EMITTER IS BUILT AT IMPORT AND THE WIDTHS ARE MODULE STATE, NOT PER-DOCUMENT STATE. A worker's
# globals die with the process, so anything set per document on this object would be lost across a
# pool boundary -- which is why row() takes the text explicitly rather than the emitter holding it.
EMIT = _io.Emitter(MODEL, EXTRAS, _io.DEFAULT_CUE, _io.DEFAULT_CUE)


# 3. The index -----------------------------------------------------------------------------------

NAME_INFO = {}      # tuple(tokens) -> (GeoClass, IsWord, GeoKey, NParent, Iso2, Iso3)
FIRST_LENS = {}     # first token -> tuple of n-gram lengths, longest first


def build_index(lookup_path):
    """Collapse the lookup parquet into the two dictionaries the scanner needs.

    ONE ROW PER NAME, and the collapse is where a naive version goes wrong. Taking the single
    distinct value of a column across ALL a name's rows produces FRANCE -> Iso2 = US-ID: there is a
    town called France in Idaho, the country carries no Iso2, and the town's is the only non-null
    value in the group. Resolution is therefore scoped to the rows of the WINNING class, after
    precedence has been applied.

    A column resolves only where the winning class holds exactly one distinct non-null value.
    Anything ambiguous is null, and `NParent` says why.

    :param lookup_path: geo_lookup.parquet.
    :return: (name_info, first_lens). first_lens maps a first token to the n-gram lengths worth
        probing there, longest first -- most tokens in a contract begin no place name at all, so
        they cost one failed dictionary probe rather than eleven.
    """
    cols = ["GeoName", "GeoKey", "GeoClass", "IsWord", "NParent", "Iso2", "ISO3"]
    df = pd.read_parquet(lookup_path, columns=cols)

    rank = {c: i for i, c in enumerate(CLASS_RANK)}
    df = df.assign(Rank=df["GeoClass"].map(rank))
    if df["Rank"].isna().any():
        bad = sorted(set(df.loc[df["Rank"].isna(), "GeoClass"]))
        raise SystemExit(f"lookup carries unranked GeoClass values: {bad}")

    # Keep only the winning class's rows before resolving anything from them.
    best = df.groupby("GeoName")["Rank"].transform("min")
    df = df[df["Rank"] == best]

    # VECTORISED, NOT groupby.apply(). A per-group Python callable over 115,856 groups took 43
    # seconds, and this index is rebuilt in EVERY worker because spawn inherits no module state --
    # so it was 43 seconds of startup on every run. nunique() plus first() expresses the same rule
    # ("resolvable means exactly one distinct non-null value") in C, and first() already skips
    # nulls.
    grouped = df.groupby("GeoName", as_index=True, sort=False)
    agg = pd.DataFrame({
        "GeoClass": grouped["GeoClass"].first(),
        "GeoKey": grouped["GeoKey"].first(),
        "IsWord": grouped["IsWord"].max(),
        "NParent": grouped["NParent"].max(),
        "Iso2": grouped["Iso2"].first().where(grouped["Iso2"].nunique(dropna=True) == 1),
        "Iso3": grouped["ISO3"].first().where(grouped["ISO3"].nunique(dropna=True) == 1),
    })

    name_info, first_lens = {}, {}
    names = agg.index.to_numpy()
    klass = agg["GeoClass"].to_numpy()
    gkey = agg["GeoKey"].to_numpy()
    isw = agg["IsWord"].fillna(0).astype("int64").to_numpy()
    npar = agg["NParent"].to_numpy()
    iso2 = agg["Iso2"].to_numpy()
    iso3 = agg["Iso3"].to_numpy()

    for i in range(len(names)):
        name = str(names[i])
        toks = tuple(name.split())
        if not toks:
            continue
        name_info[toks] = (
            klass[i],
            int(isw[i]),
            gkey[i] if isinstance(gkey[i], str) else name,
            int(npar[i]) if pd.notna(npar[i]) else None,
            iso2[i] if isinstance(iso2[i], str) else None,
            iso3[i] if isinstance(iso3[i], str) else None,
        )
        first_lens.setdefault(toks[0], set()).add(len(toks))
    first_lens = {k: tuple(sorted(v, reverse=True)) for k, v in first_lens.items()}
    return name_info, first_lens


def demote_ambiguous(name_info):
    """Drop street-type suffixes and move ambiguous country names out of the anchor set.

    They stay matchable, but as word-like names needing corroboration: relabelled to the class they
    also hold as a US place where they have one, and forced word-like otherwise. Done ONCE after the
    index is built so the scanner itself stays free of special cases -- which is also why a
    consumer rejoining the raw lookup in another language gets a different answer, and why v2 emits
    the resolved columns rather than leaving that join to be redone.
    """
    for word in ADDRESS_WORDS:
        name_info.pop((word,), None)

    for country in AMBIGUOUS_COUNTRIES:
        key = tuple(country.split())
        info = name_info.get(key)
        if info is None:
            continue
        if info[0] != "Country":            # a US State reading is never demoted
            continue
        name_info[key] = ("US Populated Place", 1) + info[2:]
    return name_info


# 4. Matching ------------------------------------------------------------------------------------

def scan(text):
    """Every non-overlapping gazetteer match in one text, longest-first, left to right.

    :return: list of (start, stop, info) where info is the NAME_INFO tuple. Offsets index `text`.
    """
    toks = [(m.group(0).upper(), m.start(), m.end(), m.group(0)[0].isupper())
            for m in TOKEN_RX.finditer(text)]
    n = len(toks)
    out = []
    i = 0
    while i < n:
        lens = FIRST_LENS.get(toks[i][0]) if toks[i][3] else None
        if lens is not None:
            matched = 0
            for L in lens:                                  # longest first
                if i + L > n:
                    continue
                info = NAME_INFO.get(tuple(toks[j][0] for j in range(i, i + L)))
                if info is not None:
                    out.append((toks[i][1], toks[i + L - 1][2], info))
                    matched = L
                    break
            if matched:
                i += matched                                # consume the whole match
                continue
        i += 1
    return out


def gate(matches, text):
    """Keep the anchors, plus the gated matches that an anchor corroborates.

    Distance is the gap between the nearest EDGES of the two spans, so an adjacent anchor scores 0
    and the measure does not punish long place names.

    :return: list of (start, stop, info, match_kind) sorted by position.
    """
    anchors = [(s, e) for s, e, info in matches if info[0] in ANCHOR_CLASSES]
    if not anchors:
        return []

    anchor_spans = set(anchors)
    kept = []
    for s, e, info in matches:
        klass, is_word = info[0], info[1]
        if (s, e) in anchor_spans and klass in ANCHOR_CLASSES:
            kept.append((s, e, info, "anchor"))
        elif is_word:
            if _adjacent_anchor(e, anchors, text):
                kept.append((s, e, info, "corroborated"))
        elif _nearest_gap(s, e, anchors) <= STATE_WINDOW:
            kept.append((s, e, info, "corroborated"))
    kept.sort(key=lambda r: (r[0], r[1]))
    return kept


def _adjacent_anchor(stop, anchors, text):
    """True when an anchor follows within WORD_WINDOW characters and only separators lie between.

    PLAIN PROXIMITY CANNOT GATE A DICTIONARY WORD, because an address block is dense with anchors
    and licenses every token in it: in "400 Hamilton Avenue, Palo Alto, California" the state sits
    within forty characters of Hamilton, Avenue and Palo Alto alike. What distinguishes the city is
    that it ABUTS the state with only a comma between them, which is how US addresses are written.
    Requiring a separator-only gap keeps "Mobile, Alabama" and drops the street it stands on.

    Anchors are position-sorted, so the search bisects to the first candidate rather than walking
    from the start. Scanning linearly is quadratic in the number of matches -- invisible on a normal
    contract and fatal on the multi-megabyte outliers the corpus contains.
    """
    idx = bisect.bisect_left(anchors, (stop, -1))
    for j in range(idx, len(anchors)):
        a_s = anchors[j][0]
        if a_s - stop > WORD_WINDOW:
            break
        if SEPARATOR_RX.fullmatch(text[stop:a_s]):
            return True
    return False


def _nearest_gap(start, stop, anchors):
    """Smallest edge-to-edge gap between [start, stop) and any anchor (0 if they touch or overlap).

    Anchors are sorted, so the search is a bisect plus two probes.
    """
    idx = bisect.bisect_left(anchors, (start, stop))
    best = None
    for j in (idx - 1, idx, idx + 1):
        if 0 <= j < len(anchors):
            a_s, a_e = anchors[j]
            if a_s < stop and start < a_e:          # overlapping spans are zero distance
                gap = 0
            else:
                gap = a_s - stop if a_s >= stop else start - a_e
            if best is None or gap < best:
                best = gap
    return best if best is not None else 10 ** 9


# 5. Extraction ----------------------------------------------------------------------------------

def init_worker(timeout, want=LABELS, lookup_path=None):
    """Pool initialiser: the per-document cap and the 116k-entry index.

    THE INDEX IS BUILT IN THE WORKER, not in main() and inherited. Under spawn a worker inherits no
    module state, and spawn is the default on macOS -- which is the machine this runs on. Building
    it per worker costs a few seconds once and is why this extractor takes a larger chunk size.
    """
    global NAME_INFO, FIRST_LENS
    _io.install_alarm(timeout)
    NAME_INFO, FIRST_LENS = build_index(lookup_path or DEFAULT_LOOKUP)
    demote_ambiguous(NAME_INFO)


def extract_one(args):
    """Rows for one document. Always returns at least one: a sentinel where nothing survived."""
    docid, text = args
    rows = []
    if isinstance(text, str) and text.strip():
        try:
            _io.start_alarm()
            for s, e, info, kind in gate(scan(text), text):
                klass, is_word, geo_key, n_parent, iso2, iso3 = info
                rows.append(EMIT.row(
                    docid, s, e, text[s:e], "GPE", klass,
                    (geo_key, is_word, n_parent, iso2, iso3, kind), text
                ))
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
        timeout=DEFAULT_TIMEOUT, n_process=1, chunk_size=DEFAULT_CHUNK_SIZE, no_progress=False,
        lookup_path=None):
    """One pass of this extractor over a document set.

    :param lookup_path: override the packaged geo lookup. For a rebuild, not for a run: a lookup
        that is not the packaged one produces a different SPEC hash, which is the point.
    :return: the path written.
    """
    lookup = Path(lookup_path or DEFAULT_LOOKUP)
    if not lookup.exists():
        raise SystemExit(f"geo lookup not found at {lookup}")

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
            initargs=(max(0, timeout), tuple(want), str(lookup)),
            n_process=n_process,
            chunk_size=chunk_size,
            desc=f"{_io.ENGINE}:{MODEL}",
            no_progress=no_progress,
        )

    frame = _io.finalize(rows, ids, EMIT)
    path = _io.write_output(frame, out_dir, MODEL, casts={
        "GeoKey": "string", "Iso2": "string", "Iso3": "string", "MatchKind": "string",
        "IsWord": "Int64", "NParent": "Int64",
    })

    n_hit = int(frame["Start"].notna().sum())
    n_doc = int(frame.loc[frame["Start"].notna(), "DocID"].nunique())
    n_anchor = int((frame["MatchKind"] == "anchor").sum())
    print(f"{len(ids)} doc(s) -> {n_hit} place(s) in {n_doc} doc(s), {n_anchor} anchor(s)  "
          f"[{_io.ENGINE}:{MODEL}] -> {path}")
    mix = frame["LabelRaw"].value_counts().to_dict()
    if mix:
        print("  " + "  ".join(f"{k}={v}" for k, v in sorted(mix.items())), file=sys.stderr)
    return path


def main(argv=None):
    ap = _io.parser(f"matcon-extract {NAME}", __doc__.splitlines()[0],
                    default_timeout=DEFAULT_TIMEOUT, default_chunk_size=DEFAULT_CHUNK_SIZE)
    ap.add_argument("--label", nargs="+", default=list(LABELS),
                    help=f"labels to extract; this engine supports: {' '.join(LABELS)}")
    ap.add_argument("--lookup", default=None,
                    help="override the packaged geo_lookup.parquet (changes the SPEC hash)")
    ap.add_argument("--version", action="version", version=_io.version_line(MODEL, SPEC))
    a = ap.parse_args(argv)
    run(a.inputs, a.out_dir, a.label, a.id_col, a.text_col, a.max_chars, a.timeout,
        a.n_process, a.chunk_size, a.no_progress, a.lookup)


if __name__ == "__main__":
    main()
