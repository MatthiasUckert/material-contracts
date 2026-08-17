"""The invariants every extractor must satisfy, tested once for all of them.

These are not tests of what the patterns find. They are tests of the properties the whole family
rests on, and each of them corresponds to a failure that has already happened or that would be
invisible if it did.
"""
from __future__ import annotations

import ast
import importlib
import inspect
from pathlib import Path

import pandas as pd
import pytest

from matcon_extract import LABEL_OWNER, _io

FIXTURE = Path(__file__).parent / "data" / "sample.parquet"

#: Modules that exist today. Grows as the port proceeds; a missing one is skipped, not failed, so
#: the suite is green at every intermediate commit rather than only at the end.
MODULES = sorted({m for m in set(LABEL_OWNER.values())
                  if importlib.util.find_spec(f"matcon_extract.{m}") is not None})

#: The pinned identity of every ported extractor. A moved hash under an unchanged MODEL means a
#: rule was edited without bumping the version, which would silently change what an existing
#: store's rows mean. Update this line ONLY together with MODEL.
PINNED = {
    "dateregex": ("dateregex-v3", "28bc2896850d"),
    "moneyregex": ("moneyregex-v6", "7c0340fde451"),
    "redaction": ("redaction-v2", "cc331c463645"),
    # The gazetteer's hash includes a content hash of geo_lookup.parquet, so it is pinned only
    # where that file is present. A machine without the 6.3 MB lookup skips it rather than failing.
    "gazetteer": ("gazetteer-v2", "54c2236f0e3a"),
}


def _frame(outputs, name):
    if name not in outputs:
        pytest.skip(f"{name} produced no output (data dependency absent)")
    return outputs[name]


@pytest.fixture(scope="session")
def docs():
    df = pd.read_parquet(FIXTURE)
    return dict(zip(df["DocID"], df["TextRaw"]))


@pytest.fixture(scope="session")
def outputs(tmp_path_factory):
    """Run every ported extractor once, over the fixture, and hand back the frames."""
    out = tmp_path_factory.mktemp("out")
    frames = {}
    for name in MODULES:
        mod = importlib.import_module(f"matcon_extract.{name}")
        lookup = getattr(mod, "DEFAULT_LOOKUP", None)
        if lookup is not None and not lookup.exists():
            continue                       # data-dependent module, data not present
        path = mod.run(inputs=[str(FIXTURE)], out_dir=str(out), no_progress=True)
        frames[name] = pd.read_parquet(path)
    return frames


# 1. The offset contract -------------------------------------------------------------------------

@pytest.mark.parametrize("name", MODULES)
def test_offsets_are_exact(name, outputs, docs):
    """text[Start:Stop] == Span, over code points, on every non-sentinel row.

    The one contract the whole family rests on. It is why no extractor is ever pointed at a
    normalised or reconstructed copy of a document, and why the R side must use stringi::stri_sub
    and never substr -- base substr misaligns about ninety-nine per cent of spans on this corpus.
    """
    assert _io.verify_offsets(_frame(outputs, name), docs) == []


@pytest.mark.parametrize("name", MODULES)
def test_offsets_are_ordered_and_bounded(name, outputs, docs):
    """Start < Stop, both inside the text. A zero-width or reversed span is a pattern bug."""
    frame = _frame(outputs, name)
    real = frame[frame["Start"].notna()]
    assert (real["Start"] < real["Stop"]).all()
    assert (real["Start"] >= 0).all()
    for docid, stop in zip(real["DocID"], real["Stop"]):
        assert int(stop) <= len(docs[docid])


# 2. The row shape -------------------------------------------------------------------------------

@pytest.mark.parametrize("name", MODULES)
def test_every_document_appears(name, outputs, docs):
    """A document processed and matched nothing is NOT the same as a document never seen.

    Without this a store cannot tell them apart, and the orchestrator re-extracts the second
    forever while believing the first is done.
    """
    assert set(_frame(outputs, name)["DocID"]) == set(docs)


@pytest.mark.parametrize("name", MODULES)
def test_identity_is_stamped_on_every_row(name, outputs):
    """Including sentinels and timeout markers, which is exactly where it gets dropped."""
    mod = importlib.import_module(f"matcon_extract.{name}")
    frame = _frame(outputs, name)
    assert set(frame["Engine"]) == {_io.ENGINE}
    assert set(frame["Model"]) == {mod.MODEL}


@pytest.mark.parametrize("name", MODULES)
def test_columns_are_core_plus_declared_extras(name, outputs):
    mod = importlib.import_module(f"matcon_extract.{name}")
    assert list(_frame(outputs, name).columns) == _io.CORE + list(mod.EXTRAS)


@pytest.mark.parametrize("name", MODULES)
def test_sentinel_rows_carry_nothing_but_identity(name, outputs):
    """A null Start must mean a null everything, or a consumer filtering on Start keeps junk."""
    frame = _frame(outputs, name)
    sent = frame[frame["Start"].isna()]
    for col in ("Stop", "Span", "Label"):
        assert sent[col].isna().all()


@pytest.mark.parametrize("name", MODULES)
def test_labels_are_within_the_declared_set(name, outputs):
    mod = importlib.import_module(f"matcon_extract.{name}")
    found = set(_frame(outputs, name)["Label"].dropna())
    assert found <= set(mod.LABELS)


# 3. Overlap resolution --------------------------------------------------------------------------

@pytest.mark.parametrize("name", MODULES)
def test_spans_do_not_overlap_within_a_label(name, outputs):
    """Overlaps resolve WITHIN a label, never across.

    A term and a date can legitimately share text and both be true, so the guarantee is per label.
    Two spans of the SAME label overlapping means keep_longest() let one through.
    """
    frame = _frame(outputs, name).dropna(subset=["Start"])
    for (_doc, _label), grp in frame.groupby(["DocID", "Label"]):
        spans = sorted(zip(grp["Start"], grp["Stop"]))
        for (_s1, e1), (s2, _e2) in zip(spans, spans[1:]):
            assert e1 <= s2


# 4. Version hygiene -----------------------------------------------------------------------------

@pytest.mark.parametrize("name", MODULES)
def test_spec_hash_is_pinned(name):
    """A moved hash under an unchanged MODEL means a rule was edited without a version bump.

    This is the structural replacement for a discipline that already failed twice:
    extract_moneyregex.py reached its sixth version inside a filename recording none of them, and
    dateregex acquired a second file for the same extractor.
    """
    mod = importlib.import_module(f"matcon_extract.{name}")
    if name not in PINNED:
        pytest.skip(f"{name} not pinned yet")
    if name == "gazetteer" and not mod.DEFAULT_LOOKUP.exists():
        pytest.skip("geo lookup absent; the gazetteer hash includes it")
    model, digest = PINNED[name]
    assert mod.MODEL == model
    assert _io.spec_hash(mod.SPEC) == digest, (
        f"{name}: SPEC changed under MODEL {mod.MODEL}. Bump MODEL and update PINNED."
    )


@pytest.mark.parametrize("name", MODULES)
def test_spec_hash_is_stable_across_calls(name):
    mod = importlib.import_module(f"matcon_extract.{name}")
    assert _io.spec_hash(mod.SPEC) == _io.spec_hash(mod.SPEC)


@pytest.mark.parametrize("name", MODULES)
def test_no_constant_is_defined_twice(name):
    """One definition of each identity constant per module.

    extract_dateregex_v3.py carried a SECOND ENGINE, MODEL, COLUMNS, MONTHS, PATTERNS and
    parse_span(), all of them the superseded v2 copy. Python shadows the earlier binding, so the
    file behaved correctly and 33 passing assertions never saw it. A duplicate that changes
    behaviour and a duplicate that does not are the same defect; only one of them announces itself.
    """
    mod = importlib.import_module(f"matcon_extract.{name}")
    tree = ast.parse(Path(inspect.getfile(mod)).read_text())
    seen, dupes = set(), []
    for node in tree.body:
        names = []
        if isinstance(node, ast.Assign):
            names = [t.id for t in node.targets if isinstance(t, ast.Name)]
        elif isinstance(node, ast.AnnAssign) and isinstance(node.target, ast.Name):
            names = [node.target.id]
        elif isinstance(node, (ast.FunctionDef, ast.ClassDef)):
            names = [node.name]
        for n in names:
            if n in seen:
                dupes.append(n)
            seen.add(n)
    assert dupes == [], f"{name}: defined more than once at module level: {sorted(set(dupes))}"


# 5. The label contract --------------------------------------------------------------------------

def test_every_label_has_exactly_one_owner():
    """Two modules claiming one label would make the output depend on plan order."""
    assert len(LABEL_OWNER) == len(set(LABEL_OWNER))


@pytest.mark.parametrize("name", MODULES)
def test_module_declares_the_labels_it_is_given(name):
    mod = importlib.import_module(f"matcon_extract.{name}")
    owned = {l for l, m in LABEL_OWNER.items() if m == name}
    assert owned == set(mod.LABELS)


@pytest.mark.parametrize("name", MODULES)
def test_requesting_one_label_suppresses_the_other(name, tmp_path):
    """A label nobody asked for must not appear.

    The ledger records what was REQUESTED. Rows for a label outside that set have no ledger entry,
    so they are invisible to every completeness check that follows.
    """
    mod = importlib.import_module(f"matcon_extract.{name}")
    if len(mod.LABELS) < 2:
        pytest.skip("single-label extractor")
    first = mod.LABELS[0]
    path = mod.run(inputs=[str(FIXTURE)], out_dir=str(tmp_path), labels=[first], no_progress=True)
    found = set(pd.read_parquet(path)["Label"].dropna())
    assert found <= {first}
