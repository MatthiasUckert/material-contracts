"""The entry point: one command, labels select the extractors.

    python -m matcon_extract sample.parquet --out-dir out/ --label DATE TERM MONEY

ONE PARQUET PER MODEL, NEVER ONE PER RUN. The store asserts that a staging file carries exactly one
(Engine, Model) pair, because an extractor writing the wrong tag produces a store that looks
complete and attributes spans to a method that never saw the document. Asking for DATE and MONEY
therefore writes two files, named <engine>__<model>.parquet so the identity is readable off the
filename before anything opens it.

THE SAME ENTRY POINT SERVES BOTH CALLERS. R's orchestrator invokes this, not the modules directly.
Two invocation paths drift, and a drifted second path is how a store ends up holding rows produced
by a command nobody can reconstruct.

Labels are resolved to modules through LABEL_OWNER, so a caller names what it wants rather than
which script produces it. Where one module owns several requested labels it runs once and emits
them together, which is why "--label DATE TERM" is one pass over the text and not two.
"""
from __future__ import annotations

import argparse
import importlib
import json
import sys

from . import LABEL_OWNER, __version__, _io


def _load(module_name):
    return importlib.import_module(f".{module_name}", package="matcon_extract")


def _available():
    """The modules actually present in this installation.

    LABEL_OWNER declares the full label map; during the port not every module exists yet, and a
    partial install is a legitimate state rather than a broken one. Distinguishing declared from
    available is what lets the default label set mean "everything this build can do" while an
    EXPLICIT request for an absent label still fails, and fails saying so.
    """
    return {m for m in set(LABEL_OWNER.values())
            if importlib.util.find_spec(f"matcon_extract.{m}") is not None}


def _default_labels():
    have = _available()
    return sorted(l for l, m in LABEL_OWNER.items() if m in have)


def _plan(labels):
    """Requested labels -> [(module, [labels it owns])], in a fixed order.

    Fixed rather than input order so two runs asking for the same labels in different orders write
    the same files and the goldens compare.
    """
    unknown = [l for l in labels if l not in LABEL_OWNER]
    if unknown:
        raise SystemExit(f"unknown label(s): {' '.join(unknown)}; "
                         f"supported: {' '.join(sorted(LABEL_OWNER))}")
    have = _available()
    absent = sorted({l for l in labels if LABEL_OWNER[l] not in have})
    if absent:
        raise SystemExit(
            f"label(s) {' '.join(absent)} need "
            f"{' '.join(sorted({LABEL_OWNER[l] for l in absent}))}, which is not installed"
        )
    by_module = {}
    for label in sorted(set(labels)):
        by_module.setdefault(LABEL_OWNER[label], []).append(label)
    return sorted(by_module.items())


def _versions():
    """Every extractor's identity line: engine, model, spec hash.

    Printed rather than returned because this is what a runbook records to prove which rules
    produced a store, and what a check compares against a pinned value.
    """
    lines = []
    have = _available()
    for module_name in sorted(set(LABEL_OWNER.values())):
        if module_name not in have:
            lines.append(f"{module_name}: not installed")
            continue
        mod = _load(module_name)
        lookup = getattr(mod, "DEFAULT_LOOKUP", None)
        if lookup is not None and not lookup.exists():
            lines.append(f"{_io.ENGINE} {mod.MODEL} data-absent ({lookup})")
            continue
        lines.append(_io.version_line(mod.MODEL, mod.SPEC))
    return "\n".join(lines)


def describe():
    """Every installed extractor's identity, as JSON on stdout.

    WHY R NEEDS THIS. The ledger is keyed on (DocID, Engine, Model, Label), so the orchestrator must
    know the model tag BEFORE a pass in order to ask what still needs doing. But the model is
    stamped by the extractor and never typed in R -- that is the rule that keeps a version string
    out of the runbooks. Something has to bridge the two, and a self-description is cheaper and
    safer than a second declaration of the same facts on the R side.

    A second declaration is precisely the defect this replaces: the label set used to be declared in
    R, keyed on the extractor STEM rather than the model, and dateregex-v3 emitting a second label
    made that declaration silently wrong. Nothing errored -- the request simply asked for one label,
    the other was filtered out inside Python, and the ledger recorded success.

    Emits, per module: engine, model, module, labels, spec_hash, and whether a data dependency is
    missing. A caller can therefore verify a pinned version, discover the labels, and refuse to
    start a pass whose rules have changed under an unchanged tag -- all before reading a document.
    """
    out = []
    have = _available()
    for module_name in sorted(set(LABEL_OWNER.values())):
        if module_name not in have:
            out.append({"module": module_name, "available": False})
            continue
        mod = _load(module_name)
        lookup = getattr(mod, "DEFAULT_LOOKUP", None)
        ready = lookup is None or lookup.exists()
        out.append({
            "module": module_name,
            "available": True,
            "ready": ready,
            "engine": _io.ENGINE,
            "model": mod.MODEL,
            "labels": list(mod.LABELS),
            "extras": list(mod.EXTRAS),
            # A HASH IS A PROMISE that two runs printing the same value produce the same output.
            # An extractor whose data dependency is absent can produce no output at all, so it gets
            # no hash rather than one that merely looks valid.
            "spec_hash": _io.spec_hash(mod.SPEC) if ready else None,
            "default_timeout": mod.DEFAULT_TIMEOUT,
            "default_chunk_size": mod.DEFAULT_CHUNK_SIZE,
        })
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(
        prog="matcon-extract",
        description="Deterministic entity extraction from contract text.",
    )
    ap.add_argument("inputs", nargs="*", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--out-dir", help="destination directory; one parquet per model")
    ap.add_argument("--label", nargs="+", default=None,
                    help=f"labels to extract; declared: {' '.join(sorted(LABEL_OWNER))}. "
                         "Default is every label this installation can produce.")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--max-chars", type=int, default=0,
                    help="truncate each document to its first N characters (0 = off)")
    ap.add_argument("--timeout", type=int, default=None,
                    help="per-document cap in seconds; default is each extractor's own")
    ap.add_argument("--n-process", type=int, default=1, help="worker processes (<=0 = all cores)")
    ap.add_argument("--chunk-size", type=int, default=None,
                    help="docs per task; default is each extractor's own")
    ap.add_argument("--no-progress", action="store_true", help="disable the progress bar")
    ap.add_argument("--version", action="store_true",
                    help="print every extractor's engine, model and spec hash, then exit")
    ap.add_argument("--describe", action="store_true",
                    help="print every extractor's identity as JSON, then exit")
    a = ap.parse_args(argv)

    if a.describe:
        print(json.dumps(describe(), indent=2))
        return 0

    if a.version:
        print(f"matcon-extract {__version__}")
        print(_versions())
        return 0

    if not a.inputs or not a.out_dir:
        ap.error("inputs and --out-dir are required")

    labels = _default_labels() if a.label is None else [l.upper() for l in a.label]
    if not labels:
        raise SystemExit("no extractor modules are installed")
    written = []
    for module_name, owned in _plan(labels):
        mod = _load(module_name)
        # DEFAULTS ARE THE EXTRACTOR'S OWN UNLESS OVERRIDDEN. The gazetteer needs a cap and a small
        # chunk because its per-document cost is orders of magnitude above the regex passes; a
        # single shared default would either leave it unguarded or throttle the other three.
        written.append(mod.run(
            inputs=a.inputs,
            out_dir=a.out_dir,
            labels=owned,
            id_col=a.id_col,
            text_col=a.text_col,
            max_chars=a.max_chars,
            timeout=mod.DEFAULT_TIMEOUT if a.timeout is None else a.timeout,
            n_process=a.n_process,
            chunk_size=mod.DEFAULT_CHUNK_SIZE if a.chunk_size is None else a.chunk_size,
            no_progress=a.no_progress,
        ))

    print(f"\n{len(written)} file(s) written to {a.out_dir}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
