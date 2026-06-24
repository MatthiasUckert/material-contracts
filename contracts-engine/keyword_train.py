#!/usr/bin/env python
"""Keyword classifier for the material-contracts sample (fold-based, per-run folders).

Structural twin of classify_train.py: same prepared parquet, same frozen folds,
same self-describing run folder, same skip-if-exists. Instead of fine-tuning a
transformer it induces a per-class lexicon from the TRAIN folds and scores the
held-out fold by precision-weighted keyword matching (the single-label port of
Storm van Lier's hybrid regex stream).

Method, per fold (fit on the other k-1 folds, predict the held-out one):
  1. Mine candidate terms per class on the training rows of the chosen field:
     CountVectorizer (n-grams, min_df / max_df), then rank terms per class by
     chi-square (one-vs-rest, on binary presence) UNION TF-IDF mean-difference;
     keep the top-K of each measure.
  2. Weight each (term, class) by its TRAINING precision: of the training docs the
     term fires in, the fraction whose label is that class (Storm Eq. 1). A
     non-discriminative term ("AGREEMENT") earns a low weight automatically.
  3. Score the held-out fold: score(doc, c) = sum of the fired terms' weights for
     class c. Predict argmax over classes; ABSTAIN (PredLabel "(none)") when no
     term fires. Weights are fit once on train and reused on the held-out fold,
     so no held-out information leaks into the lexicon.

Text source (--source):
  docdesc  : mine + score on the filer title (--desc-col, default DocDesc).
  text     : mine + score on the body (--text-col, default Text).
  combined : mine both fields, fuse score = (--alpha) * docdesc + text, argmax.

Amendment / any asymmetric binary task (--positive-class):
  "Original" is defined by the ABSENCE of amendment language, so argmax-with-
  abstain does not fit. With --positive-class set, only that class's lexicon is
  mined, and the rule is: predict the positive class if its lexicon fires
  (score > 0), else the other label. Binary, exhaustive, no abstention.

Writes the SAME run-folder schema as classify_train.py (predictions.parquet,
probabilities.parquet, metrics_overall.parquet, metrics_perclass.parquet,
config.json) PLUS lexicon.parquet (the mined terms and their precision weights --
the interpretable payoff and the seed for an optional human-prune pass). The R
side (clf_* overview layer) scores it head-to-head with BERT on the IDENTICAL
folds. Loop --test-fold 1..k from R for CV.
"""

import os
os.environ.setdefault("TOKENIZERS_PARALLELISM", "false")

import argparse
import json
import subprocess
import sys
import time
import warnings
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd
import sklearn
from scipy.sparse import csr_matrix
from sklearn.feature_extraction.text import (
    CountVectorizer, TfidfTransformer, ENGLISH_STOP_WORDS,
)
from sklearn.feature_selection import chi2

# A custom stop_words list paired with a custom token_pattern triggers a benign
# sklearn UserWarning ("Your stop_words may be inconsistent..."); silence only that.
warnings.filterwarnings("ignore", message=".*stop_words may be inconsistent.*")

NONE_LABEL = "(none)"   # sentinel PredLabel for an abstention (multi-class only)

# SEC / EDGAR filing boilerplate that survives the alpha token filter but carries
# no class signal: exhibit references, file-extension scraps, formatting words.
# Numeric exhibit tokens ("10.1", "ex-10") are already dropped by the alpha-only
# token pattern; this catches the alpha residue ("exhibit", "txt", "form", ...).
# Deliberately EXCLUDES amend/restated (those are the amendment-task signal).
DOMAIN_STOPWORDS = frozenset({
    "exhibit", "exhibits", "ex", "txt", "htm", "html", "pdf", "doc", "docx",
    "page", "pages", "dated", "form", "forms", "schedule", "schedules",
    "annex", "appendix", "registrant", "filed", "filing",
})

# Short tag for the stopword regime, stamped into ConfigName so configs are
# distinct on disk and the regime shows up as a leaderboard axis.
SW_TAG = {"none": "none", "english": "en", "domain": "dom", "english_domain": "endom"}


def resolve_stopwords(choice):
    """Map the --stopwords choice to what CountVectorizer expects."""
    if choice == "none":
        return None
    if choice == "english":
        return "english"
    if choice == "domain":
        return sorted(DOMAIN_STOPWORDS)
    if choice == "english_domain":
        return sorted(ENGLISH_STOP_WORDS | DOMAIN_STOPWORDS)
    raise ValueError(f"unknown stopwords choice: {choice}")


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


def smoke_subset(df, label_col, n_per_class):
    parts = [g.sample(min(len(g), n_per_class), random_state=0)
             for _, g in df.groupby(label_col)]
    return pd.concat(parts).reset_index(drop=True)


# Lexicon induction ------------------------------------------------------

def fit_lexicon(train_texts, train_labels, classes, topk, ngram_range,
                min_df, max_df, stopwords, token_pattern):
    """Mine a per-class lexicon on one field of the training rows.

    Returns (vectorizer, weight_matrix, lexicon_rows):
      vectorizer    : fitted CountVectorizer, to transform held-out texts.
      weight_matrix : dense (n_vocab x n_classes) of per-(term, class) precision
                      weights, columns aligned to ``classes``.
      lexicon_rows  : list of dicts (Class, Term, Measure, Weight, HitsTrain,
                      HitsPosTrain) for the lexicon.parquet artifact.
    On an empty vocabulary (everything pruned) returns a zero weight matrix so
    the caller scores every doc as no-match.
    """
    n_classes = len(classes)
    cls_to_idx = {c: i for i, c in enumerate(classes)}

    cv = CountVectorizer(
        lowercase=True, ngram_range=ngram_range, min_df=min_df, max_df=max_df,
        stop_words=stopwords, token_pattern=token_pattern,
    )
    try:
        counts = cv.fit_transform(train_texts)
    except ValueError:
        # empty vocabulary after pruning -> no signal from this field
        return None, np.zeros((0, n_classes), dtype=np.float32), []

    vocab = np.array(cv.get_feature_names_out())
    present = (counts > 0).astype(np.int8)                 # binary presence
    tfidf = TfidfTransformer().fit_transform(counts)       # for the mean-diff
    total_hits = np.asarray(present.sum(axis=0)).ravel()   # per-term, all docs

    W = np.zeros((len(vocab), n_classes), dtype=np.float32)
    rows = []
    labels_arr = np.asarray(train_labels)

    for c in classes:
        y = (labels_arr == c)
        if y.sum() == 0:
            continue                                       # class absent in train

        # chi-square on binary presence (one-vs-rest), only terms seen in positives
        chi_scores, _ = chi2(present, y.astype(np.int8))
        chi_scores = np.nan_to_num(np.asarray(chi_scores), nan=0.0,
                                   posinf=0.0, neginf=0.0)
        pos_hits = np.asarray(present[y].sum(axis=0)).ravel()
        chi_scores = np.where(pos_hits > 0, chi_scores, 0.0)
        chi_order = np.argsort(-chi_scores, kind="stable")[:topk]
        chi_terms = {int(j) for j in chi_order if chi_scores[j] > 0}

        # TF-IDF mean-difference (with vs without the class)
        pos_mean = np.asarray(tfidf[y].mean(axis=0)).ravel()
        neg_mean = np.asarray(tfidf[~y].mean(axis=0)).ravel()
        diff = pos_mean - neg_mean
        diff_order = np.argsort(-diff, kind="stable")[:topk]
        tfidf_terms = {int(j) for j in diff_order if diff[j] > 0}

        c_idx = cls_to_idx[c]
        for j in (chi_terms | tfidf_terms):
            weight_ = float(pos_hits[j] / max(int(total_hits[j]), 1))
            W[j, c_idx] = weight_
            measure_ = ("both" if (j in chi_terms and j in tfidf_terms)
                        else "chi2" if j in chi_terms else "tfidf")
            rows.append({
                "Class": c, "Term": vocab[j], "Measure": measure_,
                "Weight": round(weight_, 4),
                "HitsTrain": int(total_hits[j]), "HitsPosTrain": int(pos_hits[j]),
            })

    return cv, W, rows


def score_field(texts, vectorizer, W):
    """Precision-weighted (n_docs x n_classes) score block for one field."""
    if vectorizer is None or W.shape[0] == 0:
        return np.zeros((len(texts), W.shape[1]), dtype=np.float32)
    present_ = (vectorizer.transform(texts) > 0).astype(np.float32)
    return np.asarray(present_ @ csr_matrix(W).toarray(), dtype=np.float32)


# Scoring (abstention-aware) ---------------------------------------------

def score_predictions(true_labels, pred_labels, class_list):
    """Overall + per-class metrics, averaged over the real classes only.

    Abstentions (PredLabel == NONE_LABEL) are handled naturally: they match no
    real class, so they are a false negative for the doc's true class and never
    a false positive. NONE_LABEL is never one of ``class_list``, so it is excluded
    from the macro average. Accuracy counts an abstention as incorrect.
    """
    true_ = np.asarray(true_labels)
    pred_ = np.asarray(pred_labels)
    acc_ = float(np.mean(pred_ == true_)) if len(true_) else 0.0

    per_class_ = []
    for c in class_list:
        tp_ = int(np.sum((pred_ == c) & (true_ == c)))
        fp_ = int(np.sum((pred_ == c) & (true_ != c)))
        fn_ = int(np.sum((true_ == c) & (pred_ != c)))
        support_ = int(np.sum(true_ == c))
        prec_ = tp_ / (tp_ + fp_) if (tp_ + fp_) else 0.0
        rec_ = tp_ / (tp_ + fn_) if (tp_ + fn_) else 0.0
        f1_ = 2 * prec_ * rec_ / (prec_ + rec_) if (prec_ + rec_) else 0.0
        per_class_.append({"Label": c, "Precision": prec_, "Recall": rec_,
                           "F1": f1_, "Support": support_})

    f1s_ = np.array([r["F1"] for r in per_class_], dtype=float)
    sup_ = np.array([r["Support"] for r in per_class_], dtype=float)
    macro_ = float(f1s_.mean()) if len(f1s_) else 0.0
    weighted_ = float((f1s_ * sup_).sum() / sup_.sum()) if sup_.sum() else 0.0
    return acc_, macro_, weighted_, per_class_


class _Tee:
    """Write to several streams at once (stdout/stderr + a per-run log file).

    Used to mirror the run narrative into run_dir/run.log while the parallel
    sweep discards the console. Flushes on every write so a crash still leaves
    both the narrative and the traceback on disk.
    """

    def __init__(self, *streams):
        self._streams = streams

    def write(self, data):
        for s in self._streams:
            s.write(data)
            s.flush()

    def flush(self):
        for s in self._streams:
            s.flush()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--data", required=True, help="prepared parquet (with DocDesc)")
    ap.add_argument("--label-col", default="ClassDetailed")
    ap.add_argument("--source", default="text", choices=["docdesc", "text", "combined"])
    ap.add_argument("--text-col", default="Text")
    ap.add_argument("--desc-col", default="DocDesc")
    ap.add_argument("--fold-col", default="Fold")
    ap.add_argument("--test-fold", type=int, default=1)
    ap.add_argument("--positive-class", default=None,
                    help="binary asymmetric mode (e.g. Amended); mine only this class")
    ap.add_argument("--alpha", type=float, default=2.0, help="title weight (combined only)")
    ap.add_argument("--topk", type=int, default=25, help="top terms per measure per class")
    ap.add_argument("--ngram-min", type=int, default=1)
    ap.add_argument("--ngram-max", type=int, default=3)
    ap.add_argument("--min-df", type=int, default=2)
    ap.add_argument("--max-df", type=float, default=0.5)
    ap.add_argument("--stopwords", default="english_domain",
                    choices=["none", "english", "domain", "english_domain"],
                    help="english_domain (default) drops English function words AND "
                         "SEC/exhibit boilerplate; content-word n-grams (research "
                         "development, amended restated) survive")
    ap.add_argument("--min-token-len", type=int, default=3,
                    help="shortest alpha token kept; raise to 4 to drop more short noise")
    ap.add_argument("--seed", type=int, default=42, help="stamped for parity; mining is deterministic")
    ap.add_argument("--runs-root", default="2_output/03b-KeywordClass/runs")
    ap.add_argument("--save-probs", dest="save_probs", action="store_true", default=True)
    ap.add_argument("--no-save-probs", dest="save_probs", action="store_false")
    ap.add_argument("--overwrite", action="store_true")
    ap.add_argument("--smoke", action="store_true")
    args = ap.parse_args()

    ngram_range = (args.ngram_min, args.ngram_max)
    stopwords = resolve_stopwords(args.stopwords)
    token_pattern = rf"(?u)\b[a-zA-Z]{{{args.min_token_len},}}\b"
    binary = args.positive_class is not None

    config_name = (
        f"{args.label_col}__keyword-{args.source}__"
        f"K{args.topk}_N{args.ngram_max}_A{fmt_num(args.alpha)}_"
        f"SW{SW_TAG[args.stopwords]}_TL{args.min_token_len}_S{args.seed}"
    )
    run_name = f"{config_name}_F{args.test_fold}"
    run_token = (f"keyword:{args.source}:{args.label_col}:"
                 f"K{args.topk}:SW{SW_TAG[args.stopwords]}:F{args.test_fold}")

    runs_root = Path(args.runs_root)
    if args.smoke:
        runs_root = runs_root / "_smoke"
    run_dir = runs_root / run_name

    done_marker = run_dir / "metrics_overall.parquet"
    if done_marker.exists() and not args.overwrite and not args.smoke:
        print(f"[skip  ] run exists: {run_dir} (use --overwrite to redo)")
        return
    run_dir.mkdir(parents=True, exist_ok=True)

    # Tee stdout + stderr to run_dir/run.log: the orchestrator discards the
    # console for the parallel sweep, so this keeps a per-run forensic trail
    # (narrative on success, traceback on failure) next to the artifacts.
    _logf = open(run_dir / "run.log", "w", encoding="utf-8")
    _orig_out, _orig_err = sys.stdout, sys.stderr
    sys.stdout = _Tee(_orig_out, _logf)
    sys.stderr = _Tee(_orig_err, _logf)

    df = pd.read_parquet(args.data)
    need_ = {"DocID", args.label_col, args.fold_col}
    if args.source in ("text", "combined"):
        need_.add(args.text_col)
    if args.source in ("docdesc", "combined"):
        need_.add(args.desc_col)
    missing = need_ - set(df.columns)
    if missing:
        raise SystemExit(f"prepared data missing columns: {sorted(missing)}")

    # drop rows with no label for this task (e.g. NA AmendType); no-op for the
    # class columns, which have no NA.
    df = df[df[args.label_col].notna()].copy()

    train_df = df[df[args.fold_col] != args.test_fold].copy()
    test_df = df[df[args.fold_col] == args.test_fold].copy()
    min_df = args.min_df
    max_df = args.max_df
    if args.smoke:
        train_df = smoke_subset(train_df, args.label_col, 40)
        test_df = smoke_subset(test_df, args.label_col, 10)
        min_df, max_df = 1, 1.0          # tiny corpus: do not prune to empty

    # class set and the negative label for binary mode
    if binary:
        classes = [args.positive_class]
        others_ = sorted(x for x in train_df[args.label_col].unique()
                         if x != args.positive_class)
        if not others_:
            raise SystemExit("positive-class mode needs a second label in train")
        negative_class = others_[0]
        score_classes = sorted(train_df[args.label_col].unique().tolist())
    else:
        classes = sorted(train_df[args.label_col].unique().tolist())
        negative_class = None
        score_classes = classes
        test_df = test_df[test_df[args.label_col].isin(set(classes))].copy()

    print(f"[run   ] {run_name}")
    print(f"[label ] {args.label_col} | source={args.source} | classes={len(classes)} | "
          f"positive={args.positive_class} | test_fold={args.test_fold}")
    print(f"[data  ] train={len(train_df)} test={len(test_df)}")

    started = time.time()
    started_iso = datetime.now(timezone.utc).isoformat(timespec="seconds")

    def field_texts(frame, which):
        col_ = args.desc_col if which == "docdesc" else args.text_col
        return frame[col_].fillna("").astype(str).tolist()

    # fit per-field lexicons on train, score the held-out fold
    fields_ = ["docdesc", "text"] if args.source == "combined" else [args.source]
    weights_ = {"docdesc": args.alpha, "text": 1.0}
    test_scores = np.zeros((len(test_df), len(classes)), dtype=np.float32)
    lex_rows_ = []
    for fld_ in fields_:
        print(f"[mine  ] field={fld_}")
        vec_, W_, rows_ = fit_lexicon(
            field_texts(train_df, fld_), train_df[args.label_col].tolist(),
            classes, args.topk, ngram_range, min_df, max_df, stopwords, token_pattern)
        for r in rows_:
            r["Zone"] = fld_
        lex_rows_.extend(rows_)
        print(f"[score ] field={fld_} terms={len(rows_)}")
        test_scores += weights_[fld_] * score_field(field_texts(test_df, fld_), vec_, W_)

    # decision rule
    if binary:
        pos_raw = test_scores[:, 0]
        norm_ = float(pos_raw.max()) if pos_raw.size and pos_raw.max() > 0 else 1.0
        pos01 = np.clip(pos_raw / norm_, 0.0, 1.0)
        pred_labels = np.where(pos_raw > 0, args.positive_class, negative_class)
        win_score = pos01
        prob_classes = [negative_class, args.positive_class]
        prob_matrix = np.column_stack([1.0 - pos01, pos01])
    else:
        norm_ = float(test_scores.max()) if test_scores.size and test_scores.max() > 0 else 1.0
        scores01 = np.clip(test_scores / norm_, 0.0, 1.0)
        row_max = test_scores.max(axis=1)
        arg_ = test_scores.argmax(axis=1)
        pred_labels = np.array([classes[i] for i in arg_], dtype=object)
        pred_labels[row_max <= 0] = NONE_LABEL          # abstain on no match
        win_score = scores01.max(axis=1)
        win_score[row_max <= 0] = 0.0
        prob_classes = classes
        prob_matrix = scores01

    true_labels = test_df[args.label_col].to_numpy()
    coverage = float(np.mean(pred_labels != NONE_LABEL))

    ended_iso = datetime.now(timezone.utc).isoformat(timespec="seconds")
    duration = round(time.time() - started, 1)

    # predictions.parquet (same schema as the BERT trainer)
    pd.DataFrame({
        "ConfigName": config_name, "Run": run_token,
        "DocID": test_df["DocID"].to_numpy(),
        "TrueLabel": true_labels, "PredLabel": pred_labels,
        "Score": win_score, "Fold": args.test_fold,
    }).to_parquet(run_dir / "predictions.parquet", index=False)

    if args.save_probs:
        wide = pd.DataFrame(prob_matrix, columns=prob_classes)
        wide.insert(0, "DocID", test_df["DocID"].to_numpy())
        wide.insert(0, "ConfigName", config_name)
        wide.melt(id_vars=["ConfigName", "DocID"], var_name="Class", value_name="Prob") \
            .to_parquet(run_dir / "probabilities.parquet", index=False)

    # metrics (abstention-aware), over the real classes only
    acc, f1_macro, f1_weight, per_class = score_predictions(
        true_labels, pred_labels, score_classes)

    pd.DataFrame([{
        "ConfigName": config_name, "Run": run_token, "RunName": run_name,
        "Model": f"keyword-{args.source}", "LabelCol": args.label_col,
        "TextCol": args.source, "ClassWeights": False, "TestFold": args.test_fold,
        "MaxLen": np.nan, "Epochs": np.nan, "BatchSize": np.nan, "LR": np.nan,
        "Seed": args.seed, "Device": "cpu",
        "nTrain": len(train_df), "nTest": len(test_df), "nClasses": len(score_classes),
        "Accuracy": acc, "F1_macro": f1_macro, "F1_weighted": f1_weight,
        "DurationSec": duration, "Smoke": bool(args.smoke),
        # keyword-specific axes (NA on the BERT rows when bound together in R)
        "Source": args.source, "TopK": args.topk, "NgramMax": args.ngram_max,
        "MinDf": args.min_df, "MaxDf": args.max_df, "Alpha": args.alpha,
        "Stopwords": args.stopwords, "MinTokenLen": args.min_token_len,
        "PositiveClass": args.positive_class, "Coverage": coverage,
    }]).to_parquet(run_dir / "metrics_overall.parquet", index=False)

    pd.DataFrame([{**r, "ConfigName": config_name, "Run": run_token}
                  for r in per_class]).to_parquet(
        run_dir / "metrics_perclass.parquet", index=False)

    # lexicon.parquet -- the interpretable payoff and the human-prune seed
    if lex_rows_:
        pd.DataFrame([{**r, "ConfigName": config_name, "Run": run_token}
                      for r in lex_rows_]).to_parquet(
            run_dir / "lexicon.parquet", index=False)

    manifest = {
        "config_name": config_name, "run_name": run_name, "run_token": run_token,
        "smoke": bool(args.smoke), "model": f"keyword-{args.source}",
        "label_col": args.label_col, "source": args.source,
        "text_col": args.text_col, "desc_col": args.desc_col,
        "positive_class": args.positive_class, "negative_class": negative_class,
        "test_fold": args.test_fold, "alpha": args.alpha, "topk": args.topk,
        "ngram_range": list(ngram_range), "min_df": args.min_df, "max_df": args.max_df,
        "stopwords": args.stopwords, "min_token_len": args.min_token_len,
        "n_stopwords": (0 if stopwords is None else
                        len(ENGLISH_STOP_WORDS) if stopwords == "english"
                        else len(stopwords)),
        "seed": args.seed, "device": "cpu",
        "n_classes": len(score_classes), "classes": score_classes,
        "data_path": str(args.data), "n_train": len(train_df), "n_test": len(test_df),
        "accuracy": acc, "f1_macro": f1_macro, "f1_weighted": f1_weight,
        "coverage": coverage, "n_terms": len(lex_rows_),
        "duration_sec": duration, "started_at": started_iso, "ended_at": ended_iso,
        "versions": {"python": sys.version.split()[0],
                     "numpy": np.__version__,
                     "pandas": pd.__version__,
                     "sklearn": sklearn.__version__},
        "git_commit": git_commit(),
    }
    (run_dir / "config.json").write_text(json.dumps(manifest, indent=2))

    cov_msg = "" if binary else f" coverage={coverage:.3f}"
    print(f"[done  ] acc={acc:.3f} f1_macro={f1_macro:.3f} "
          f"f1_weighted={f1_weight:.3f}{cov_msg} ({duration}s)")
    print(f"[write ] {run_dir}")

    sys.stdout, sys.stderr = _orig_out, _orig_err
    _logf.close()


if __name__ == "__main__":
    main()
