"""Shared machinery: the offset contract, the row shape, and the seam to parquet.

WHAT BELONGS HERE, AND WHAT DOES NOT
This module holds the things where two implementations diverging is a CORRECTNESS bug rather than
a slow one. Each extractor owns its own patterns, its own worker initialiser and its own
parallelism, because those differ for real reasons and the regex passes are fast enough that
sharing them buys nothing. What is shared is the shape of what comes out.

Four things in particular:

  THE OFFSET CONTRACT. Every span is 0-based, half-open, over CODE POINTS, and
  text[Start:Stop] == Span exactly. Python string slicing is code-point based, so this holds by
  construction here; it is the R side that has to use stringi::stri_sub and never substr. The
  contract is asserted in one place (verify_offsets) and tested once for every extractor.

  SENTINEL ROWS. A document that was processed and matched nothing still appears, carrying engine
  and model and nulling everything else. Without it a document that was processed is
  indistinguishable from one that was never seen, and the orchestrator's ledger cannot tell the
  difference either. Four extractors each building their own sentinel is four chances to get this
  wrong.

  TIMEOUT ROWS. A document over its cap emits a marker rather than raising, so one pathological
  file cannot end a corpus pass. The marker carries "timeout:<name>" in LabelRaw, which is what R
  reads to set Status.

  THE SPEC HASH. MODEL is the version and the filename is not; the hash is what makes that
  enforceable rather than a rule people remember. See spec_hash().

OVERLAP RESOLUTION lives here too, in keep_longest(), because "overlaps resolve WITHIN a label,
not across" is an invariant of the whole family rather than of one extractor. A term and a date can
legitimately share text and both be true.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import signal
import sys
from pathlib import Path

import pandas as pd
import pyarrow.dataset as pads
from tqdm import tqdm

#: The columns every extractor emits, in this order, before its own extras.
CORE = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model"]

#: Stamped into every row. The provenance axis: which tool found this span, not which suite it
#: was shipped in. Third-party engines stamp their own name ("lexnlp", "spacy"); everything in
#: this package is "matcon".
ENGINE = "matcon"


# 1. Identity and versioning -------------------------------------------------------------------

def spec_hash(spec):
    """A stable 12-character fingerprint of the constants that determine an extractor's output.

    WHY THIS EXISTS. The rule is that MODEL is the version and the filename is not. That rule was
    already broken twice by discipline alone -- extract_moneyregex.py carried moneyregex-v6, six
    versions deep in a file whose name recorded none of them, and dateregex acquired a second file
    for the same extractor. The hash makes the rule checkable: MODEL unchanged with the hash moved
    means somebody edited a pattern without bumping the version, and that is an abort rather than a
    silent change of meaning in an existing store.

    Hashed over the SPEC dict rather than the file, so comments and refactors do not trigger it and
    a pattern edit always does. Serialisation is canonical JSON with sorted keys; tuples serialise
    as lists, which is deterministic, and floats via repr, which is stable in Python 3.

    :param spec: dict of the constants that change output. Patterns, vocabularies, pivots,
        windows -- and, where an extractor reads one, a content hash of its lookup file.
    :return: the first 12 hex characters of the SHA-256.
    """
    blob = json.dumps(spec, sort_keys=True, default=str, ensure_ascii=True)
    return hashlib.sha256(blob.encode("ascii")).hexdigest()[:12]


def file_hash(path):
    """A content hash of a data file, for folding into an extractor's SPEC.

    An extractor that reads a lookup has a version that means nothing unless the lookup is part of
    it: rebuild geo_lookup.parquet and the spans move under an unchanged model tag. The last such
    rebuild was safe only by accident -- it added columns, and the scanner reads three.

    :param path: file to hash.
    :return: the first 12 hex characters of the SHA-256 of its bytes.
    """
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()[:12]


def version_line(model, spec):
    """The one line --version prints: engine, model, spec hash."""
    return f"{ENGINE} {model} {spec_hash(spec)}"


# 2. The row shape -----------------------------------------------------------------------------

class Emitter:
    """Builds every row one extractor emits, in one column order.

    An extractor declares its identity and its extra columns once, at import, and thereafter
    cannot emit a row of the wrong width or with the engine and model unstamped. The sentinel and
    timeout rows come from the same object as the ordinary ones, which is the point: they are the
    rows most easily got wrong and least likely to be noticed.
    """

    def __init__(self, model, extras=()):
        """
        :param model: the MODEL constant -- the version tag stamped into every row.
        :param extras: this extractor's extra column names, in output order.
        """
        self.model = model
        self.extras = tuple(extras)
        self.columns = CORE + list(self.extras)
        self._nulls = (None,) * len(self.extras)

    def row(self, docid, start, stop, span, label, label_raw, extras=None):
        """One found span.

        :param extras: tuple in the declared extras order, or None for all-null. Length is
            asserted, because a short tuple would silently shift every later column.
        """
        if extras is None:
            extras = self._nulls
        elif len(extras) != len(self.extras):
            raise ValueError(
                f"{self.model}: {len(extras)} extra(s) supplied, {len(self.extras)} declared"
            )
        return (docid, start, stop, span, label, label_raw, ENGINE, self.model) + tuple(extras)

    def sentinel(self, docid):
        """A document that was processed and matched nothing.

        NOT the same as an absent document, and the difference is the whole reason this exists.
        """
        return (docid, None, None, None, None, None, ENGINE, self.model) + self._nulls

    def timeout(self, docid, name):
        """A document cut off by the per-document cap. R reads LabelRaw to set Status."""
        return (docid, None, None, None, None, f"timeout:{name}", ENGINE, self.model) + self._nulls

    def error(self, docid, name):
        """A document that raised something other than a timeout.

        DISTINGUISHABLE FROM A SENTINEL, WHICH IS THE ENTIRE POINT. moneyregex-v6 caught every
        exception and fell through to a bare sentinel, so a document whose extraction CRASHED was
        recorded as one that was processed and matched nothing. Both produce a null span, both
        satisfy the ledger, and nothing anywhere could tell them apart -- so a pattern failing on
        a class of documents would look exactly like that class having no amounts in it.

        One pathological document must not end a corpus pass, so the exception is still swallowed.
        It is simply no longer swallowed silently.
        """
        return (docid, None, None, None, None, f"error:{name}", ENGINE, self.model) + self._nulls


# 3. Overlaps ----------------------------------------------------------------------------------

def keep_longest(text, compiled):
    """Candidates from one pattern table, overlaps resolved by longest span first.

    WITHIN A LABEL, NEVER ACROSS. "for a period of five (5) years from January 1, 2020" is a term
    AND a date, they overlap, and both are true; one greedy pass over both tables would let the
    longer span delete the shorter and lose one of them. Each label's candidates compete only with
    their own, so an extractor emitting two labels calls this twice.

    Ties on equal length go to the earlier entry in the pattern table, then to position, which is
    what makes table order a deliberate priority rather than an accident of authorship.

    Quadratic in candidate count in the worst case, which is where the per-document cap earns its
    place: a table of dates can produce tens of thousands of candidates in one document.

    :param text: the document text; offsets index it directly.
    :param compiled: list of (name, compiled_regex, reading) in priority order.
    :return: list of (start, stop, name), sorted by position.
    """
    cands = []
    for prio, (name, rx, _reading) in enumerate(compiled):
        for m in rx.finditer(text):
            cands.append((m.start(), m.end(), name, prio))
    cands.sort(key=lambda c: (-(c[1] - c[0]), c[3], c[0]))
    kept = []
    for s, e, name, _ in cands:
        if all(e <= ks or s >= ke for ks, ke, _ in kept):
            kept.append((s, e, name))
    kept.sort()
    return kept


def keep_leftmost(text, compiled):
    """Candidates from one pattern table, resolved LEFTMOST first and longest at each position.

    A SECOND RULE, NOT A VARIANT OF THE FIRST, and the two genuinely disagree. Given a short span
    at 0-5 and a long one at 3-20, keep_longest() returns the long one and this returns the short.
    Which is right depends on the label: dates compete as alternative readings of the same text, so
    the longest reading wins outright; money expressions are read in document order, and a figure
    that has already begun is not superseded by a later, longer expression that happens to swallow
    it.

    Preserved from moneyregex-v6 exactly. Its own docstring described the rule as longest-first,
    which the code was not -- see the note in moneyregex.py. Changing it would change spans in an
    existing store, so the behaviour is kept and the discrepancy is recorded rather than resolved.

    Ties on equal start and equal length fall to pattern-table order, because Python's sort is
    stable.

    :param text: unused; present so the two resolvers are interchangeable at the call site.
    :param compiled: list of (name, compiled_regex, reading) in priority order.
    :return: list of (start, stop, name), sorted by position.
    """
    hits = []
    for name, rx, _reading in compiled:
        for m in rx.finditer(text):
            if m.group(0).strip():
                hits.append((m.start(), m.end(), name))
    hits.sort(key=lambda h: (h[0], -(h[1] - h[0])))
    out, last = [], -1
    for s, e, name in hits:
        if s >= last:
            out.append((s, e, name))
            last = e
    return out


# 4. The timeout cap ---------------------------------------------------------------------------

TIMEOUT = 0     # per-document seconds; set inside each worker, never in main()


class ExtractorTimeout(Exception):
    """Raised by the SIGALRM handler when one document exceeds its cap."""


def _alarm_handler(signum, frame):
    raise ExtractorTimeout()


def install_alarm(timeout):
    """Set this worker's per-document cap and install the handler.

    Called from a Pool initializer, never from main(). Under spawn a worker does not inherit
    module state, so anything set in the parent and read in a child is a bug that appears only on
    macOS -- which is the machine this runs on.
    """
    global TIMEOUT
    signal.signal(signal.SIGALRM, _alarm_handler)
    TIMEOUT = timeout


def start_alarm():
    if TIMEOUT > 0:
        signal.alarm(TIMEOUT)


def cancel_alarm():
    if TIMEOUT > 0:
        signal.alarm(0)


# 5. Input and output --------------------------------------------------------------------------

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

    :return: (texts, n_truncated). Non-strings pass through untouched.
    """
    if not max_chars or max_chars <= 0:
        return texts, 0
    n_trunc = sum(1 for t in texts if isinstance(t, str) and len(t) > max_chars)
    if n_trunc:
        texts = [t[:max_chars] if isinstance(t, str) and len(t) > max_chars else t for t in texts]
    return texts, n_trunc


def read_documents(inputs, id_col, text_col, max_chars=0):
    """Read the documents one pass will run over.

    :return: (ids, texts). Reads exactly two columns, so a wide input costs nothing extra.
    """
    df = (pads.dataset(resolve_inputs(inputs), format="parquet")
              .to_table(columns=[id_col, text_col])
              .to_pandas())
    ids = df[id_col].tolist()
    texts, n_trunc = truncate_texts(df[text_col].tolist(), max_chars)
    if n_trunc:
        print(f"{n_trunc} doc(s) truncated to {max_chars} chars", file=sys.stderr)
    return ids, texts


def run_pool(items, extract_one, initializer, initargs, n_process, chunk_size, desc,
             no_progress=False, paired=False):
    """Run one extractor over every document, in parallel or not.

    The loop is here rather than in each module because the WORKER-STATE rule is easy to get wrong
    and the same everywhere: the timeout and any index are set by the initialiser inside the
    worker. The initialiser itself is the module's own, which is why it is an argument -- the
    gazetteer builds a 185k-entry index and the regex extractors set an integer.

    n_process == 1 runs in-process, which is what makes a debugger usable.

    PAIRED MODE exists because a per-document count cannot be accumulated in a module global: a
    worker's globals die with the process. An extractor that measures what it EXCLUDED -- and an
    exclusion nobody can see is indistinguishable from a pattern that never matched -- must
    therefore return those counts beside its rows, and they are summed here.

    :param paired: False, extract_one returns rows. True, it returns (rows, dict-of-counts).
    :return: rows, or (rows, summed counts) when paired.
    """
    nproc = n_process if n_process > 0 else (os.cpu_count() or 1)
    rows = []
    tally = {}

    def take(result):
        if not paired:
            rows.extend(result)
            return
        got, counts = result
        rows.extend(got)
        for k, v in counts.items():
            tally[k] = tally.get(k, 0) + v

    if nproc == 1:
        initializer(*initargs)
        for it in tqdm(items, total=len(items), unit="doc", desc=f"{desc} (n_process=1)",
                       file=sys.stderr, disable=no_progress):
            take(extract_one(it))
    else:
        from multiprocessing import Pool
        with Pool(processes=nproc, initializer=initializer, initargs=initargs) as pool:
            for r in tqdm(pool.imap_unordered(extract_one, items, chunksize=chunk_size),
                          total=len(items), unit="doc", desc=f"{desc} (n_process={nproc})",
                          file=sys.stderr, disable=no_progress):
                take(r)
    return (rows, tally) if paired else rows


def finalize(rows, ids, emitter):
    """Guarantee every input document appears in the output, then hand back a frame.

    EVERY DocID APPEARS AT LEAST ONCE. An extractor that returns nothing for a document, or a
    parallel run that loses one, would otherwise produce a store where the document looks
    unprocessed and gets re-extracted on every later pass. Cheap to assert, expensive to discover.
    """
    seen = {r[0] for r in rows}
    missing = [i for i in ids if i not in seen]
    if missing:
        print(f"{len(missing)} doc(s) produced no row at all; adding sentinels", file=sys.stderr)
        rows = rows + [emitter.sentinel(i) for i in missing]
    return pd.DataFrame(rows, columns=emitter.columns)


def write_output(frame, out_dir, model, casts=None):
    """Write one model's parquet, named so the store can read its identity off the filename.

    ONE PARQUET PER MODEL, NEVER PER LABEL SET. R's ner_db_append() asserts that a staging file
    carries exactly one (Engine, Model) pair, because an extractor writing the wrong tag produces
    a store that looks complete and attributes spans to a method that never saw the document. An
    entry point asked for two labels belonging to two models therefore writes two files.

    :param casts: dict of column -> pandas dtype, applied before writing so the seam carries types
        rather than leaving the first consumer to guess.
    :return: the path written.
    """
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    path = out_dir / f"{ENGINE}__{model}.parquet"
    frame[["Start", "Stop"]] = frame[["Start", "Stop"]].astype("Int64")
    for col, dtype in (casts or {}).items():
        frame[col] = frame[col].astype(dtype)
    frame.to_parquet(path, index=False)
    return path


# 6. Checks ------------------------------------------------------------------------------------

def verify_offsets(frame, texts_by_id):
    """Assert the offset contract on every non-sentinel row.

    text[Start:Stop] == Span, exactly, over code points. Used by the tests and available to a
    caller who wants the guarantee at run time.

    :return: list of (DocID, Start, Stop, expected, actual) for every row that fails. Empty is a
        pass.
    """
    bad = []
    for docid, start, stop, span in zip(frame["DocID"], frame["Start"], frame["Stop"],
                                        frame["Span"]):
        if pd.isna(start):
            continue
        text = texts_by_id.get(docid)
        if text is None:
            bad.append((docid, start, stop, None, span))
            continue
        cut = text[int(start):int(stop)]
        if cut != span:
            bad.append((docid, int(start), int(stop), cut, span))
    return bad


# 7. The shared command line -------------------------------------------------------------------

def add_common_arguments(ap, default_timeout=0, default_chunk_size=64):
    """The flags every extractor takes, so their names and meanings cannot drift.

    Defaults that genuinely differ by extractor -- the gazetteer needs a cap and a small chunk
    because its per-document cost is orders of magnitude higher -- are parameters here rather than
    per-module argparse calls, so the divergence is visible in one place instead of buried in four.
    """
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--out-dir", required=True, help="destination directory for the parquet")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--max-chars", type=int, default=0,
                    help="truncate each document to its first N characters (0 = off)")
    ap.add_argument("--timeout", type=int, default=default_timeout,
                    help="per-document cap in seconds (0 = off)")
    ap.add_argument("--n-process", type=int, default=1, help="worker processes (<=0 = all cores)")
    ap.add_argument("--chunk-size", type=int, default=default_chunk_size,
                    help="docs per task when parallelising")
    ap.add_argument("--no-progress", action="store_true", help="disable the progress bar")
    return ap


def parser(prog, description, **kw):
    """An argparse parser carrying the common flags."""
    ap = argparse.ArgumentParser(prog=prog, description=description)
    return add_common_arguments(ap, **kw)
