"""Does the label split move a single date?

THE QUESTION. v3 adds TERM to an extractor that emitted only DATE, and it changes how overlaps
resolve -- within a label rather than across -- because "for a period of five (5) years from
January 1, 2020" is a term AND a date and one greedy pass would let the longer span delete the
shorter. That change is supposed to be INVISIBLE to the date family. This compares span for span,
and a single difference is a defect rather than a design choice.

AGAINST A PARQUET, NOT AGAINST A SECOND SCRIPT. The old check ran two extractor files to compare
two versions, which is exactly the convention that produced a second file for one extractor. There
is now one file per extractor and the previous version's OUTPUT is the baseline: cheaper, it is
what the store's model column is for, and it keeps working when v4 arrives.

Freeze the baseline once:

    2_output/_Probe/Check-NER-DATE-RegexV3/out/DATE__paper__dateregex-v2.parquet

and point MATCON_BASELINE at it, or drop it in tests/data/. Absent, every test here skips, so the
suite stays green on a machine that has the package and not the corpus.
"""
from __future__ import annotations

import os
from pathlib import Path

import pandas as pd
import pytest

from matcon_extract import dateregex as dr

#: The v2 output to compare against, and the text it was produced from.
BASELINE = os.environ.get("MATCON_BASELINE")
CORPUS = os.environ.get("MATCON_CORPUS")

#: Columns that must agree exactly. Engine and Model are excluded because they are SUPPOSED to
#: differ -- that is the whole point of the rename and the version bump.
COMPARED = ["DocID", "Start", "Stop", "Span", "LabelRaw", "DateValue"]

pytestmark = pytest.mark.skipif(
    not (BASELINE and CORPUS and Path(BASELINE).exists() and Path(CORPUS).exists()),
    reason="set MATCON_BASELINE and MATCON_CORPUS to run the v2 regression",
)


def _dates(frame):
    """DATE rows only, normalised so two frames can be compared as sets of rows."""
    out = frame[(frame["Label"] == "DATE") & frame["Start"].notna()].copy()
    out["Start"] = out["Start"].astype("int64")
    out["Stop"] = out["Stop"].astype("int64")
    out["DateValue"] = out["DateValue"].astype("string")
    return (out[COMPARED]
            .sort_values(COMPARED, kind="stable")
            .reset_index(drop=True))


@pytest.fixture(scope="module")
def frames(tmp_path_factory):
    old = pd.read_parquet(BASELINE)
    # Fail on the WRONG FILE rather than on a KeyError three frames deep. Pointing MATCON_BASELINE
    # at the corpus instead of the v2 output is the obvious mistake and deserves one clear line.
    missing = [c for c in ("Label", "Start", "Span", "LabelRaw", "DateValue") if c not in old]
    if missing:
        pytest.fail(f"{BASELINE} is not a dateregex output; missing {' '.join(missing)}")
    if set(old["Model"].dropna()) != {"dateregex-v2"}:
        pytest.fail(f"{BASELINE} stamps Model {sorted(set(old['Model'].dropna()))}, "
                    "expected dateregex-v2")
    path = dr.run(inputs=[CORPUS], out_dir=str(tmp_path_factory.mktemp("reg")),
                  n_process=max(1, (os.cpu_count() or 2) - 1), no_progress=True)
    return _dates(old), _dates(pd.read_parquet(path))


def test_date_spans_are_identical(frames):
    """Every offset, every LabelRaw, every parsed value, unchanged.

    A failure here means the term table stole text from the date table, which is precisely what
    resolving overlaps within a label is supposed to prevent.
    """
    old, new = frames
    assert len(old) == len(new), f"v2 has {len(old)} date spans, v3 has {len(new)}"
    pd.testing.assert_frame_equal(old, new, check_dtype=False)


def test_the_same_documents_carry_dates(frames):
    """A document gaining or losing its date family entirely is a different failure and a worse
    one: it survives a row count that happens to match."""
    old, new = frames
    assert set(old["DocID"]) == set(new["DocID"])


def test_v3_adds_terms(frames, tmp_path):
    """The regression proves nothing was lost. This proves something was gained.

    A run that changed no dates AND found no terms would pass every test above while having done
    nothing at all.
    """
    path = dr.run(inputs=[CORPUS], out_dir=str(tmp_path), labels=["TERM"], no_progress=True)
    terms = pd.read_parquet(path)
    terms = terms[terms["Start"].notna()]
    assert len(terms) > 0
    assert terms["TermYears"].notna().any()
    print(f"\n  {len(terms):,} term span(s) in {terms['DocID'].nunique():,} document(s)")
