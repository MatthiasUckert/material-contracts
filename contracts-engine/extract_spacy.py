#!/usr/bin/env python3
"""spaCy NER extractor with offsets.

Input : one or more parquet paths (files and/or folders), one row = one document.
Output: one parquet (DocID, Start, Stop, Span, Label, LabelRaw, Engine).

Offsets are 0-based, half-open, code-point indices: text[Start:Stop] == Span.
`Label` is the shared cross-engine vocabulary (LABEL_MAP); `LabelRaw` keeps the native
spaCy tag. --label filters on the unified labels. Aligned schema with extract_lexnlp.py.

--device {auto,cpu,cuda,mps}: GPU acceleration via spacy.require_gpu() -- CuPy on CUDA,
Metal/MPS on Apple Silicon. NOTE: on Apple, MPS only accelerates the transformer (trf);
the CNN models (sm/md/lg) stay on CPU. On any GPU, n_process is forced to 1 (a single
device can't be shared across worker processes); use n_process only for CPU models.
"""
import argparse
import os
import sys
from pathlib import Path

import pandas as pd
import pyarrow.dataset as pads
import spacy
from tqdm import tqdm

LABEL_MAP = {
    "ORG": "ORG",
    "PERSON": "PERSON",
    "GPE": "GPE", "LOC": "GPE",
    "DATE": "DATE",
    "MONEY": "MONEY",
    "PERCENT": "PERCENT",
    "QUANTITY": "AMOUNT",
}
COLUMNS = ["DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine"]


def resolve_inputs(paths):
    """A single file, a folder, or several of each -> a flat list of parquet files."""
    files = []
    for p in paths:
        pth = Path(p)
        files += sorted(str(f) for f in pth.rglob("*.parquet")) if pth.is_dir() else [str(pth)]
    if not files:
        raise SystemExit("no parquet files found in the given path(s)")
    return files


def setup_device(requested):
    """requested: auto|cpu|cuda|mps. Activates the GPU (if any) and returns the active device.

    Must run before any pipeline is loaded. CuPy backs CUDA; Thinc routes the torch
    transformer to MPS on Apple Silicon. Falls back to CPU under 'auto'.
    """
    if requested == "cpu":
        return "cpu"

    # CUDA (CuPy): accelerates both CNN and transformer layers.
    if requested in ("auto", "cuda"):
        try:
            import cupy  # noqa: F401  (only present in a CUDA-enabled install)
            if spacy.require_gpu():
                return "cuda"
        except Exception:
            if requested == "cuda":
                raise SystemExit("CUDA requested but unavailable (install a CUDA build, e.g. pip install 'spacy[cuda12x]').")

    # MPS (Apple Silicon): accelerates the torch transformer only, not the CNN ops.
    if requested in ("auto", "mps"):
        os.environ.setdefault("PYTORCH_ENABLE_MPS_FALLBACK", "1")  # unsupported ops fall back to CPU
        try:
            import torch
            if torch.backends.mps.is_available() and spacy.require_gpu():
                return "mps"
        except Exception:
            if requested == "mps":
                raise SystemExit("MPS requested but unavailable (need torch>=1.13 with MPS on macOS 12.3+).")

    return "cpu"


def load_model(model):
    nlp = spacy.load(model)
    for pipe in ("parser", "lemmatizer", "tagger", "attribute_ruler", "morphologizer"):
        if pipe in nlp.pipe_names:
            nlp.disable_pipe(pipe)
    nlp.max_length = 5_000_000
    return nlp


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("inputs", nargs="+", help="parquet file(s) and/or folder(s)")
    ap.add_argument("--output", required=True, help="output parquet path")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--text-col", default="TextRaw")
    ap.add_argument("--model", default="en_core_web_sm", help="spaCy model name or path")
    ap.add_argument("--label", nargs="+", default=None,
                    help="unified labels to keep (default: all). E.g. ORG GPE DATE")
    ap.add_argument("--device", default="auto", choices=["auto", "cpu", "cuda", "mps"],
                    help="GPU device for inference (auto tries cuda -> mps -> cpu)")
    ap.add_argument("--batch-size", type=int, default=64)
    ap.add_argument("--n-process", type=int, default=1, help="CPU workers for nlp.pipe (ignored on GPU)")
    ap.add_argument("--no-progress", action="store_true", help="disable the progress bar")
    args = ap.parse_args()

    device = setup_device(args.device)                 # before load_model, per spaCy docs
    n_process = args.n_process
    if device != "cpu" and n_process != 1:
        print(f"[device={device}] forcing n_process=1 (a GPU can't be shared across workers)", file=sys.stderr)
        n_process = 1

    df = (pads.dataset(resolve_inputs(args.inputs), format="parquet")
              .to_table(columns=[args.id_col, args.text_col])
              .to_pandas())
    texts = df[args.text_col].fillna("").astype(str).tolist()

    keep = set(args.label) if args.label else None
    nlp = load_model(args.model)

    piped = nlp.pipe(texts, batch_size=args.batch_size, n_process=n_process)
    docs = tqdm(piped, total=len(texts), unit="doc",
                desc=f"spaCy:{args.model} [{device}] (n_process={n_process})",
                file=sys.stderr, disable=args.no_progress)

    rows = []
    for docid, doc in zip(df[args.id_col], docs):
        for ent in doc.ents:
            label = LABEL_MAP.get(ent.label_, ent.label_)
            if keep and label not in keep:
                continue
            rows.append((docid, ent.start_char, ent.end_char, ent.text,
                         label, ent.label_, "spacy"))

    pd.DataFrame(rows, columns=COLUMNS).to_parquet(args.output, index=False)
    print(f"{len(df)} doc(s) -> {len(rows)} candidate(s)  [spacy:{args.model}, device={device}, n_process={n_process}]")


if __name__ == "__main__":
    main()
