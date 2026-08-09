"""classify_apply.py -- label documents with a model saved by classify_train.py --fit-final.

WHY THIS IS A SEPARATE SCRIPT
classify_train.py owns training. It reads a labelled parquet, deals folds, fits, and writes metrics;
every one of those steps assumes a label column exists. Inference assumes the opposite -- the whole
point is documents nobody has labelled -- so bolting a mode onto the trainer would mean guarding
every step with "unless we are only predicting", and the guards would outnumber the work.

What this file must match exactly is the trainer's ENCODING, because a model is only as reproducible
as the tokenisation it was fitted under:
  * the same tokenizer, loaded from the model directory rather than by name, so a checkpoint carries
    its own vocabulary;
  * truncation at the same max_len, which is a validated hyperparameter and not a memory setting;
  * padding="max_length" rather than dynamic padding, which keeps tensor shapes constant across
    batches and avoids the MPS kernel recompiles that dominate wall clock on Apple silicon.

The label mapping is not passed in. classify_train.py hands id2label and label2id to
from_pretrained, so they are written into the checkpoint's config.json and travel with the weights.
Reconstructing them here from a labelled sample would be a second source of truth, and the two would
eventually disagree about which integer means which category -- silently, since the outputs would
still be valid category names.
"""

from __future__ import annotations

import argparse
import json
import time
from pathlib import Path

import pandas as pd
import torch
from transformers import AutoModelForSequenceClassification, AutoTokenizer


def pick_device() -> str:
    """Same order as the trainer, so inference runs where training ran."""
    if torch.backends.mps.is_available():
        return "mps"
    if torch.cuda.is_available():
        return "cuda"
    return "cpu"


def main() -> None:
    ap = argparse.ArgumentParser(description="Label documents with a saved classifier.")
    ap.add_argument("--data", required=True, help="parquet with DocID and the text column")
    ap.add_argument("--model-dir", required=True, help="directory written by --fit-final")
    ap.add_argument("--out", required=True, help="parquet to write")
    ap.add_argument("--text-col", default="Text")
    ap.add_argument("--id-col", default="DocID")
    ap.add_argument("--max-len", type=int, default=256)
    ap.add_argument("--batch-size", type=int, default=32)
    ap.add_argument("--save-probs", dest="save_probs", action="store_true", default=False,
                    help="write one column per category as well as the top-1 label")
    ap.add_argument("--verbose", action="store_true", default=False)
    args = ap.parse_args()

    def say(msg: str) -> None:
        if args.verbose:
            print(msg, flush=True)

    model_dir = Path(args.model_dir)
    if not model_dir.is_dir():
        raise SystemExit(f"[error ] no model directory at {model_dir}")

    df = pd.read_parquet(args.data)
    for col in (args.id_col, args.text_col):
        if col not in df.columns:
            raise SystemExit(f"[error ] column {col!r} not in {args.data}")
    if len(df) == 0:
        raise SystemExit("[error ] nothing to classify")

    device = pick_device()
    tok = AutoTokenizer.from_pretrained(model_dir)
    model = AutoModelForSequenceClassification.from_pretrained(model_dir)
    model.to(device)
    model.eval()

    # The checkpoint carries its own mapping; keys arrive as strings or ints depending on how the
    # config was serialised, so both are normalised here rather than trusted.
    id2label = {int(k): v for k, v in model.config.id2label.items()}
    labels_sorted = [id2label[i] for i in sorted(id2label)]
    say(f"[model ] {model_dir.name} | {len(labels_sorted)} classes | device={device} | "
        f"max_len={args.max_len}")

    texts = df[args.text_col].fillna("").astype(str).tolist()
    n = len(texts)
    top_idx: list[int] = []
    top_prob: list[float] = []
    all_probs: list[torch.Tensor] = []

    t0 = time.time()
    with torch.inference_mode():
        for start in range(0, n, args.batch_size):
            batch = texts[start:start + args.batch_size]
            # Encoded per batch, not once for the whole file: a corpus chunk encoded in one go holds
            # every token id in memory at the same time for no gain in speed.
            enc = tok(batch, truncation=True, max_length=args.max_len,
                      padding="max_length", return_tensors="pt")
            enc = {k: v.to(device) for k, v in enc.items()}
            logits = model(**enc).logits
            probs = torch.softmax(logits, dim=-1).detach().to("cpu")
            best = probs.argmax(dim=-1)
            top_idx.extend(best.tolist())
            top_prob.extend(probs.gather(1, best.unsqueeze(1)).squeeze(1).tolist())
            if args.save_probs:
                all_probs.append(probs)
            if args.verbose and (start // args.batch_size) % 50 == 0:
                say(f"[infer ] {min(start + args.batch_size, n)}/{n}")

    out = pd.DataFrame({
        args.id_col: df[args.id_col].tolist(),
        "PredLabel": [id2label[i] for i in top_idx],
        "Top1Prob": top_prob,
    })
    if args.save_probs:
        wide = pd.DataFrame(torch.cat(all_probs).numpy(), columns=labels_sorted)
        out = pd.concat([out.reset_index(drop=True), wide.add_prefix("P_")], axis=1)

    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    out.to_parquet(args.out, index=False)

    secs = time.time() - t0
    say(f"[done  ] {n} documents in {secs:.1f}s ({n / max(secs, 1e-9):.1f}/s) -> {args.out}")

    # A one-line receipt beside the output, so a chunk can be traced to the checkpoint that produced
    # it without opening the parquet or trusting the caller's own record.
    meta = {
        "model_dir": str(model_dir),
        "n_docs": int(n),
        "max_len": int(args.max_len),
        "batch_size": int(args.batch_size),
        "device": device,
        "seconds": round(secs, 2),
        "classes": labels_sorted,
    }
    Path(args.out).with_suffix(".json").write_text(json.dumps(meta, indent=2))


if __name__ == "__main__":
    main()
