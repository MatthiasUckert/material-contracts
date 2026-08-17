#!/usr/bin/env python
"""BERT classifier for the material-contracts sample (fold-based, per-run folders).

Consumes prepared parquet [DocID, Text, ClassBroad, ClassDetailed,
ClassDetailed2, AmendType, LabelRound, Fold]. Trains one sequence-classification
model on one label column (--label-col: any column, e.g. ClassDetailed,
ClassBroad, AmendType) using one text column, holding out one fold
(--test-fold). Rows with NA in the chosen label are dropped (lets AmendType,
with some NA, train cleanly). Optional inverse-frequency class weighting. Writes
a self-describing run folder. Loop --test-fold 1..k from R for CV.
"""

import os
os.environ.setdefault("PYTORCH_ENABLE_MPS_FALLBACK", "1")
os.environ.setdefault("TOKENIZERS_PARALLELISM", "false")

import argparse
import contextlib
import json
import subprocess
import sys
import time
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd
import torch
import transformers
from sklearn.metrics import accuracy_score, f1_score, precision_recall_fscore_support
from transformers import (
    AutoModelForSequenceClassification,
    AutoTokenizer,
    Trainer,
    TrainingArguments,
    set_seed,
)

import logging


class _DropNewWeightsWarning(logging.Filter):
    """Suppress only the 'newly initialized classifier head' notice from
    from_pretrained; all other transformers warnings still propagate."""
    _NEEDLES = (
        "were not initialized from the model checkpoint",
        "You should probably TRAIN this model",
    )

    def filter(self, record):
        msg = record.getMessage()
        return not any(n in msg for n in self._NEEDLES)


logging.getLogger("transformers.modeling_utils").addFilter(_DropNewWeightsWarning())


def pick_device():
    if torch.backends.mps.is_available():
        return "mps"
    if torch.cuda.is_available():
        return "cuda"
    return "cpu"


def fmt_num(x):
    xf = float(x)
    return str(int(xf)) if xf.is_integer() else f"{xf:g}"


def git_commit():
    try:
        return subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            capture_output=True, text=True, check=True
        ).stdout.strip()
    except Exception:
        return None


class RunLogger:
    """Per-run logging: detail to run.log, only key lines to the real console.

    All the per-fold chatter (device, label/data summary, the transformers epoch
    logs, library warnings) goes to <run_dir>/run.log. The console -- which is the
    R console during a serial sweep -- receives only the start line and the final
    acc/f1 line, so a 320-run sweep stays a readable progress strip. The console
    stream is captured at construction so summary lines survive the fd-level
    redirection used around training.
    """

    def __init__(self, log_path, console):
        self._fh = open(log_path, "w", buffering=1)   # line-buffered
        self._console = console

    @property
    def file(self):
        return self._fh

    def log(self, msg=""):
        """File only."""
        self._fh.write(f"{msg}\n")
        self._fh.flush()

    def say(self, msg=""):
        """Console + file."""
        self._console.write(f"{msg}\n")
        self._console.flush()
        self.log(msg)

    def close(self):
        try:
            self._fh.flush()
            self._fh.close()
        except Exception:
            pass


@contextlib.contextmanager
def redirect_fds(target):
    """Point stdout/stderr (fd 1/2) at an open file for the duration.

    Operates at the file-descriptor level via os.dup2 so that transformers
    logging, tqdm, and any C-level writes all land in the log regardless of which
    stream reference they captured at import time -- the failure mode a plain
    contextlib.redirect_stdout cannot cover. Original fds are restored on exit,
    so a traceback from a failed run still reaches the console.
    """
    sys.stdout.flush()
    sys.stderr.flush()
    saved_out, saved_err = os.dup(1), os.dup(2)
    tfd = target.fileno()
    os.dup2(tfd, 1)
    os.dup2(tfd, 2)
    try:
        yield
    finally:
        sys.stdout.flush()
        sys.stderr.flush()
        os.dup2(saved_out, 1)
        os.dup2(saved_err, 2)
        os.close(saved_out)
        os.close(saved_err)


class TextDataset(torch.utils.data.Dataset):
    def __init__(self, encodings, labels):
        self.encodings = encodings
        self.labels = labels

    def __len__(self):
        return len(self.labels)

    def __getitem__(self, idx):
        item = {k: v[idx] for k, v in self.encodings.items()}
        item["labels"] = self.labels[idx]
        return item


class WeightedTrainer(Trainer):
    """Trainer with a fixed class-weight vector in the cross-entropy loss."""
    def __init__(self, class_weights=None, **kwargs):
        super().__init__(**kwargs)
        self.class_weights = class_weights

    def compute_loss(self, model, inputs, return_outputs=False, **kwargs):
        labels = inputs.pop("labels")
        outputs = model(**inputs)
        logits = outputs.logits
        weight = None if self.class_weights is None else self.class_weights.to(logits.device)
        loss_fct = torch.nn.CrossEntropyLoss(weight=weight)
        loss = loss_fct(logits.view(-1, model.config.num_labels), labels.view(-1))
        return (loss, outputs) if return_outputs else loss


def smoke_subset(df, label_col, n_per_class):
    parts = [g.sample(min(len(g), n_per_class), random_state=0)
             for _, g in df.groupby(label_col)]
    return pd.concat(parts).reset_index(drop=True)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", required=True, help="prepared parquet")
    ap.add_argument("--label-col", default="ClassDetailed")
    ap.add_argument("--text-col", default="Text")
    ap.add_argument("--fold-col", default="Fold")
    ap.add_argument("--test-fold", type=int, default=1)
    ap.add_argument("--model", default="roberta-base")
    ap.add_argument("--max-len", type=int, default=512)
    ap.add_argument("--epochs", type=float, default=6.0)
    ap.add_argument("--batch-size", type=int, default=32)
    ap.add_argument("--lr", type=float, default=2e-5)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--class-weights", dest="class_weights", action="store_true", default=False)
    ap.add_argument("--runs-root", default="2_output/03-Classification/runs")
    ap.add_argument("--save-model", dest="save_model", action="store_true", default=True)
    ap.add_argument("--no-save-model", dest="save_model", action="store_false")
    ap.add_argument("--save-probs", dest="save_probs", action="store_true", default=True)
    ap.add_argument("--no-save-probs", dest="save_probs", action="store_false")
    ap.add_argument("--overwrite", action="store_true")
    ap.add_argument("--smoke", action="store_true")
    ap.add_argument("--fit-final", dest="fit_final", action="store_true", default=False,
                    help="train on ALL rows (no held-out fold), skip evaluation, and "
                         "force-save the model -- produces the deployable model.")
    ap.add_argument("--verbose", action="store_true", default=False,
                    help="stream training output (epoch logs, progress bar) to the "
                         "console; default captures it into run.log only.")
    args = ap.parse_args()

    set_seed(args.seed)
    device = pick_device()
    if args.fit_final:
        args.save_model = True            # the whole point of this mode

    model_slug = args.model.replace("/", "-")
    wtag = 1 if args.class_weights else 0
    config_name = (
        f"{args.label_col}__{model_slug}__T{args.text_col}_"
        f"L{args.max_len}_E{fmt_num(args.epochs)}_B{args.batch_size}_"
        f"LR{args.lr:g}_W{wtag}_S{args.seed}"
    )
    run_name = f"{config_name}_F{args.test_fold}"
    run_token = f"bert:{model_slug}:{args.label_col}:T{args.text_col}:W{wtag}:F{args.test_fold}"
    if args.fit_final:
        run_name = f"{config_name}__FINAL"
        run_token = f"bert:{model_slug}:{args.label_col}:T{args.text_col}:W{wtag}:FINAL"

    runs_root = Path(args.runs_root)
    if args.smoke:
        runs_root = runs_root / "_smoke"
    run_dir = runs_root / run_name

    # final mode is marked done by its config.json (written last); CV mode by its
    # metrics_overall.parquet.
    done_marker = run_dir / ("config.json" if args.fit_final else "metrics_overall.parquet")
    if done_marker.exists() and not args.overwrite and not args.smoke:
        print(f"[skip  ] run exists: {run_dir} (use --overwrite to redo)")
        return
    run_dir.mkdir(parents=True, exist_ok=True)
    log = RunLogger(run_dir / "run.log", console=sys.stdout)

    df = pd.read_parquet(args.data)
    missing = {"DocID", args.text_col, args.label_col, args.fold_col} - set(df.columns)
    if missing:
        raise SystemExit(f"prepared data missing columns: {sorted(missing)}")

    # drop rows with no label for this task (e.g. NA AmendType); no-op for the
    # class columns, which have no NA.
    df = df[df[args.label_col].notna()].copy()

    if args.fit_final:
        # train on everything; there is no held-out fold in this mode.
        train_df = df.copy()
        test_df = df.iloc[0:0].copy()
    else:
        train_df = df[df[args.fold_col] != args.test_fold].copy()
        test_df = df[df[args.fold_col] == args.test_fold].copy()
    if args.smoke:
        train_df = smoke_subset(train_df, args.label_col, 8)
        if not args.fit_final:
            test_df = smoke_subset(test_df, args.label_col, 4)
        args.epochs = 1.0

    labels_sorted = sorted(train_df[args.label_col].unique().tolist())
    lab2id = {lab: i for i, lab in enumerate(labels_sorted)}
    id2lab = {i: lab for lab, i in lab2id.items()}
    if not args.fit_final:
        test_df = test_df[test_df[args.label_col].isin(lab2id)].copy()

    fold_disp = "ALL" if args.fit_final else args.test_fold
    log.say(f"[run   ] {run_name}")
    log.log(f"[device] {device}")
    log.log(f"[label ] {args.label_col} | text={args.text_col} | classes={len(labels_sorted)} | "
            f"weights={bool(args.class_weights)} | fold={fold_disp}")
    log.log(f"[data  ] train={len(train_df)} test={len(test_df)}")

    started = time.time()
    started_iso = datetime.now(timezone.utc).isoformat(timespec="seconds")

    # Heavy compute. By default we silence the console and capture everything
    # (transformers epoch logs, library warnings, progress output) into run.log at
    # the fd level. In --verbose mode we skip the redirect so it streams live to the
    # console instead (run.log then keeps only the structured lines; per-epoch
    # metrics are always in train_log.parquet).
    train_ctx = contextlib.nullcontext() if args.verbose else redirect_fds(log.file)
    with train_ctx:
        tok = AutoTokenizer.from_pretrained(args.model)

        def encode(texts):
            # static max_length padding keeps shapes constant, which avoids MPS
            # kernel recompiles across batches.
            return tok(texts, truncation=True, max_length=args.max_len,
                       padding="max_length", return_tensors="pt")

        train_txt = train_df[args.text_col].fillna("").astype(str).tolist()
        train_enc = encode(train_txt)
        train_y = torch.tensor([lab2id[x] for x in train_df[args.label_col]])
        if not args.fit_final:
            test_txt = test_df[args.text_col].fillna("").astype(str).tolist()
            test_enc = encode(test_txt)
            test_y = torch.tensor([lab2id[x] for x in test_df[args.label_col]])

        model = AutoModelForSequenceClassification.from_pretrained(
            args.model, num_labels=len(labels_sorted), id2label=id2lab, label2id=lab2id
        )

        targs = TrainingArguments(
            output_dir=str(run_dir / "hf"),
            num_train_epochs=args.epochs,
            per_device_train_batch_size=args.batch_size,
            per_device_eval_batch_size=args.batch_size,
            learning_rate=args.lr,
            seed=args.seed,
            save_strategy="no",
            logging_strategy="epoch",
            disable_tqdm=not args.verbose,   # show the live progress bar when verbose
            report_to="none",
            dataloader_pin_memory=False,
            fp16=False,
            bf16=False,
        )

        if args.class_weights:
            counts = train_df[args.label_col].value_counts().reindex(labels_sorted).to_numpy()
            cw = counts.sum() / (len(counts) * counts)            # sklearn "balanced", mean ~1
            class_weights = torch.tensor(cw, dtype=torch.float)
            trainer = WeightedTrainer(class_weights=class_weights, model=model, args=targs,
                                      train_dataset=TextDataset(train_enc, train_y))
        else:
            trainer = Trainer(model=model, args=targs,
                              train_dataset=TextDataset(train_enc, train_y))

        trainer.train()

        if not args.fit_final:
            pred = trainer.predict(TextDataset(test_enc, test_y))
            probs = torch.softmax(torch.tensor(pred.predictions), dim=-1).numpy()
            pred_ids = probs.argmax(axis=-1)
            scores = probs.max(axis=-1)
            true_ids = test_y.numpy()

    ended_iso = datetime.now(timezone.utc).isoformat(timespec="seconds")
    duration = round(time.time() - started, 1)
    acc = f1_macro = f1_weight = None

    if not args.fit_final:
        pd.DataFrame({
            "ConfigName": config_name, "Run": run_token,
            "DocID": test_df["DocID"].to_numpy(),
            "TrueLabel": [id2lab[i] for i in true_ids],
            "PredLabel": [id2lab[i] for i in pred_ids],
            "Score": scores, "Fold": args.test_fold,
        }).to_parquet(run_dir / "predictions.parquet", index=False)

        if args.save_probs:
            wide = pd.DataFrame(probs, columns=labels_sorted)
            wide.insert(0, "DocID", test_df["DocID"].to_numpy())
            wide.insert(0, "ConfigName", config_name)
            wide.melt(id_vars=["ConfigName", "DocID"], var_name="Class", value_name="Prob") \
                .to_parquet(run_dir / "probabilities.parquet", index=False)

        acc = float(accuracy_score(true_ids, pred_ids))
        f1_macro = float(f1_score(true_ids, pred_ids, average="macro"))
        f1_weight = float(f1_score(true_ids, pred_ids, average="weighted"))
        pd.DataFrame([{
            "ConfigName": config_name, "Run": run_token, "RunName": run_name, "Model": args.model,
            "LabelCol": args.label_col, "TextCol": args.text_col, "ClassWeights": bool(args.class_weights),
            "TestFold": args.test_fold, "MaxLen": args.max_len, "Epochs": args.epochs,
            "BatchSize": args.batch_size, "LR": args.lr, "Seed": args.seed, "Device": device,
            "nTrain": len(train_df), "nTest": len(test_df), "nClasses": len(labels_sorted),
            "Accuracy": acc, "F1_macro": f1_macro, "F1_weighted": f1_weight,
            "DurationSec": duration, "Smoke": bool(args.smoke),
        }]).to_parquet(run_dir / "metrics_overall.parquet", index=False)

        p, r, f, s = precision_recall_fscore_support(
            true_ids, pred_ids, labels=list(range(len(labels_sorted))), zero_division=0
        )
        pd.DataFrame({
            "ConfigName": config_name, "Run": run_token, "Label": labels_sorted,
            "Precision": p, "Recall": r, "F1": f, "Support": s.astype(int),
        }).to_parquet(run_dir / "metrics_perclass.parquet", index=False)

    try:
        pd.DataFrame(trainer.state.log_history).to_parquet(
            run_dir / "train_log.parquet", index=False
        )
    except Exception as e:
        log.log(f"[warn  ] could not write train_log: {e}")

    if args.save_model:
        model.save_pretrained(run_dir / "model")
        tok.save_pretrained(run_dir / "model")

    manifest = {
        "config_name": config_name, "run_name": run_name, "run_token": run_token,
        "fit_final": bool(args.fit_final),
        "smoke": bool(args.smoke), "model": args.model, "label_col": args.label_col,
        "text_col": args.text_col, "class_weights": bool(args.class_weights),
        "test_fold": (None if args.fit_final else args.test_fold),
        "max_len": args.max_len, "epochs": args.epochs,
        "batch_size": args.batch_size, "lr": args.lr, "seed": args.seed, "device": device,
        "n_classes": len(labels_sorted), "labels": labels_sorted, "label2id": lab2id,
        "data_path": str(args.data), "n_train": len(train_df), "n_test": len(test_df),
        "accuracy": acc, "f1_macro": f1_macro, "f1_weighted": f1_weight,
        "duration_sec": duration, "started_at": started_iso, "ended_at": ended_iso,
        "saved_model": bool(args.save_model),
        "versions": {"python": sys.version.split()[0],
                     "torch": torch.__version__,
                     "transformers": transformers.__version__},
        "git_commit": git_commit(),
    }
    (run_dir / "config.json").write_text(json.dumps(manifest, indent=2))

    if args.fit_final:
        log.say(f"[final ] saved deployable model on {len(train_df)} docs to {run_dir / 'model'} ({duration}s)")
    else:
        log.say(f"[done  ] acc={acc:.3f} f1_macro={f1_macro:.3f} f1_weighted={f1_weight:.3f} ({duration}s)")
    log.log(f"[write ] {run_dir}")
    log.close()


if __name__ == "__main__":
    main()
