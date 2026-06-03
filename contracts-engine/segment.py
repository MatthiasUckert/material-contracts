#!/usr/bin/env python3
"""Segment stage (MasterDoc §12) — paragraph/clause segmentation as character offsets.

Input : a parquet file OR a directory of parquet files, one row = one document,
        with an id column and a text column (TextRaw).
Output: two parquet files
          --out-segments : (DocID, SegmentID, Start, Stop, NChar, SegHash)
          --out-anchors  : (DocID, TextLen, TextSha256, SegVersion)

Offset convention (locked): 0-based, half-open [Start, Stop), Unicode code-point
indices into TextRaw -- NOT byte offsets. Python slices natively; in R use
substr(x, Start + 1, Stop). The segment text is never stored: rehydrate by slicing
TextRaw on demand, so it can never drift from source. The anchor sidecar lets any
consumer assert it is slicing the same TextRaw segmentation ran on.
"""
import argparse
import hashlib
import re

import pandas as pd
import pyarrow.dataset as pads

SEG_VERSION = "1"  # bump when the segmentation logic changes

# One tunable separator regex per mode (no spaCy/ML in this stage).
SEPARATORS = {
    "blank_line":     re.compile(r"\n\s*\n"),  # >=1 fully blank line -> paragraph break
    "single_newline": re.compile(r"\r?\n"),    # every line is its own unit
}


def _sha256(s: str) -> str:
    return hashlib.sha256(s.encode("utf-8")).hexdigest()


def segment_one(text: str, sep: re.Pattern):
    """Return [(Start, Stop), ...] for each non-empty, whitespace-trimmed segment.

    Offsets are code-point indices into `text`, 0-based, half-open. Internal
    single newlines (line wraps) are kept inside a segment; only leading/trailing
    whitespace is trimmed, and all-whitespace chunks are dropped.
    """
    spans, pos = [], 0
    for m in sep.finditer(text):
        spans.append((pos, m.start()))
        pos = m.end()
    spans.append((pos, len(text)))

    out = []
    for s, e in spans:
        chunk = text[s:e]
        lead = len(chunk) - len(chunk.lstrip())
        trail = len(chunk) - len(chunk.rstrip())
        s2, e2 = s + lead, e - trail
        if e2 > s2:                       # drop empty / whitespace-only chunks
            out.append((s2, e2))
    return out


def main():
    ap = argparse.ArgumentParser(
        description="Paragraph segmentation -> offsets table + anchor sidecar.")
    ap.add_argument("input_path",
                    help="parquet file or directory of parquet files (one row = one document)")
    ap.add_argument("--out-segments", required=True, help="output parquet: segment offsets table")
    ap.add_argument("--out-anchors", required=True, help="output parquet: per-doc anchor sidecar")
    ap.add_argument("--id-col", default="DocID", help="column holding the document id")
    ap.add_argument("--text-col", default="TextRaw", help="column holding the document text")
    ap.add_argument("--mode", default="blank_line", choices=list(SEPARATORS),
                    help="paragraph boundary rule (default: blank_line)")
    args = ap.parse_args()

    sep = SEPARATORS[args.mode]
    seg_version = f"{SEG_VERSION}:{args.mode}"

    df = (pads.dataset(args.input_path, format="parquet")
              .to_table(columns=[args.id_col, args.text_col])
              .to_pandas())

    seg_rows, anchor_rows = [], []
    for docid, text in zip(df[args.id_col], df[args.text_col]):
        if not isinstance(text, str):
            text = ""
        anchor_rows.append({
            "DocID": docid,
            "TextLen": len(text),
            "TextSha256": _sha256(text),
            "SegVersion": seg_version,
        })
        for seg_id, (start, stop) in enumerate(segment_one(text, sep)):
            seg_rows.append({
                "DocID": docid,
                "SegmentID": seg_id,
                "Start": start,
                "Stop": stop,
                "NChar": stop - start,
                "SegHash": _sha256(text[start:stop]),
            })

    segments = pd.DataFrame(
        seg_rows, columns=["DocID", "SegmentID", "Start", "Stop", "NChar", "SegHash"])
    anchors = pd.DataFrame(
        anchor_rows, columns=["DocID", "TextLen", "TextSha256", "SegVersion"])

    segments.to_parquet(args.out_segments, index=False)
    anchors.to_parquet(args.out_anchors, index=False)
    print(f"{len(df)} doc(s) -> {len(segments)} segment(s)  [mode={args.mode}]")
    print(f"  segments -> {args.out_segments}")
    print(f"  anchors  -> {args.out_anchors}")


if __name__ == "__main__":
    main()
