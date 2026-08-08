#!/usr/bin/env python
"""Keyword statistics miner for the material-contracts sample (fold-based).

WHAT CHANGED FROM THE PREVIOUS VERSION
This script no longer classifies anything. It emits STATISTICS; R makes every
decision. The reason is cost asymmetry: mining is expensive (tokenise 4.4k
contracts, build an n-gram vocabulary), selection is cheap (filter a table,
walk a sorted list). Baking the selection floors into the engine meant every
floor combination needed a re-mine. Splitting the seam collapses the mining
grid to ~200 runs and makes the selection sweep interactive in R.

Consequences of the split:
  - No --positive-class. The amendment rule ("predict Amended if any Amended
    term fires, else Original") is a decision, so it lives in R. This engine is
    task-agnostic: it mines whatever label column it is handed.
  - No topk. Candidate depth is bounded by --max-candidates purely to keep the
    artifacts small; the real term budget is the greedy marginal-reach rule in R.
  - No chi-square, no TF-IDF mean-difference. Both are replaced by Power (below),
    which is directional by construction. sklearn's chi2 is UNSIGNED, so the old
    ranking admitted terms that were strongly associated with the ABSENCE of the
    class -- harmless for the classifier (their precision weight was ~0) but
    fatal for a published keyword table, which is now the deliverable.

POWER: the ranking statistic
For a candidate term t and class c, on the TRAINING folds:
    HitsPos = training docs of class c in which t fires
    HitsTot = training docs of ANY class in which t fires
    Precision = HitsPos / HitsTot          (of the docs t fires in, how many are c)
    Power = Wilson 95% LOWER confidence bound on Precision
Raw precision cannot tell 2/2 from 190/200 -- it calls them 1.00 and 0.95 and
ranks the fluke first. Power calls them 0.34 and 0.91. It is monotone in both
precision and evidence, bounded in [0, 1], and reads as "we are 95% confident
that a hit means this class at least this often". It is the sort key of the
published table and the confidence dial at scoring time.

DIRECTIONAL GUARD
A (term, class) pair is kept only when Precision > prior(c), i.e. seeing the term
raises the posterior for c above its base rate. One line, and it removes the
anti-association leak by construction.

TRUNCATION
--nwords truncates the body to the first N whitespace words (0 = full document),
applied identically to the training and held-out rows. 256 / 512 are the
like-for-like comparison with BERT's token windows (legal English runs ~1.35
WordPiece per word, so these are conservative); 1024 / 2048 / full are windows
BERT structurally cannot read.

MANUAL TERM LISTS (--terms-file)
With --terms-file the engine skips mining entirely and computes the same
statistics and incidence for exactly the (Class, Term) pairs supplied. This is
how a hand-curated list is evaluated on identical folds, and it works for terms
that never survived mining (or never appeared in the corpus -- those are
reported as unmatchable rather than silently dropped).

OUTPUTS (per mine run folder)
  termstats.parquet       MineName, Run, Zone, Class, Term, HitsPos, HitsTot,
                          NClass, NTrain, Prior, Precision, Reach, Power
  incidence_train.parquet Class, Term, DocIdx   (class docs only -- all the
                          greedy marginal-reach rule needs)
  incidence_test.parquet  Term, DocIdx          (held-out fold; absent for F0)
  docs_train.parquet      DocIdx, DocID, Label
  docs_test.parquet       DocIdx, DocID, Label  (absent for F0)
  mine.json               manifest

--test-fold 0 means "no held-out fold": mine on ALL labelled rows. That is the
all-data run whose lexicon becomes the published table, mirroring the BERT
convention of CV for the estimate and an all-data refit for the artifact.
"""

import os
os.environ.setdefault("TOKENIZERS_PARALLELISM", "false")

import argparse
import hashlib
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
from sklearn.feature_extraction.text import CountVectorizer, ENGLISH_STOP_WORDS

# A custom token_pattern paired with a stop_words list triggers a benign sklearn
# UserWarning; we pass stop_words=None, but keep the filter for safety.
warnings.filterwarnings("ignore", message=".*stop_words may be inconsistent.*")

Z_95 = 1.959963984540054      # two-sided 95% normal quantile

# SEC / EDGAR filing boilerplate that survives the alpha token filter but carries
# no class signal. Deliberately EXCLUDES amend/restated: those are the amendment
# task's entire signal.
DOMAIN_STOPWORDS = frozenset({
    "exhibit", "exhibits", "txt", "htm", "html", "pdf", "doc", "docx",
    "page", "pages", "dated", "form", "forms", "schedule", "schedules",
    "annex", "appendix", "registrant", "filed", "filing",
})

# Stamped into the mine name so regimes are distinct on disk and become a
# leaderboard axis rather than a silent setting.
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


def truncate_words(texts, n_words):
    """First n_words whitespace words of each text (0 = full document)."""
    if not n_words:
        return texts
    return [" ".join(t.split()[:n_words]) for t in texts]


def wilson_lower(k, n, z=Z_95):
    """Wilson score LOWER bound for a binomial proportion k / n.

    Vectorised, safe at n = 0 (returns 0). This is Power: it shrinks toward 0
    when the evidence is thin, so a 2/2 term cannot outrank a 190/200 term.
    """
    k_ = np.asarray(k, dtype=np.float64)
    n_ = np.asarray(n, dtype=np.float64)
    out_ = np.zeros(np.broadcast(k_, n_).shape, dtype=np.float64)
    ok_ = n_ > 0
    if not np.any(ok_):
        return out_
    p_ = np.divide(k_, n_, out=np.zeros_like(out_), where=ok_)
    z2_ = z * z
    denom_ = 1.0 + z2_ / np.where(ok_, n_, 1.0)
    centre_ = p_ + z2_ / (2.0 * np.where(ok_, n_, 1.0))
    margin_ = z * np.sqrt(
        p_ * (1.0 - p_) / np.where(ok_, n_, 1.0)
        + z2_ / (4.0 * np.where(ok_, n_, 1.0) ** 2)
    )
    out_ = np.where(ok_, (centre_ - margin_) / denom_, 0.0)
    return np.clip(out_, 0.0, 1.0)


# Candidate statistics ----------------------------------------------------

def build_vectorizer(ngram_range, min_df, max_df, min_token_len, stopwords=None,
                     vocabulary=None):
    """CountVectorizer with the project's alpha-only token pattern.

    Numbers and punctuation never become keywords: exhibit numbers, dates and
    dollar amounts are entity-extraction territory (the NER track), not contract
    -type signal, and they would dominate an n-gram vocabulary.
    """
    return CountVectorizer(
        lowercase=True,
        ngram_range=ngram_range,
        min_df=min_df if vocabulary is None else 1,
        max_df=max_df if vocabulary is None else 1.0,
        stop_words=stopwords if vocabulary is None else None,
        token_pattern=rf"(?u)\b[a-zA-Z]{{{min_token_len},}}\b",
        vocabulary=vocabulary,
    )


def term_statistics(present, vocab, labels, classes, n_train,
                    power_floor, max_candidates, pair_filter=None):
    """Per-(term, class) statistics from a binary presence matrix.

    present        : csr (n_docs x n_vocab), binary.
    pair_filter    : optional dict {class: set(term)} restricting which pairs are
                     evaluated (manual-list mode). None = evaluate every term
                     against every class (mining mode).

    Returns (stats_df, kept_index) where kept_index maps each surviving class to
    the vocabulary column indices of its candidate terms.
    """
    total_hits_ = np.asarray(present.sum(axis=0)).ravel().astype(np.int64)
    labels_ = np.asarray(labels)
    frames_ = []
    kept_ = {}

    for c_ in classes:
        y_ = labels_ == c_
        n_class_ = int(y_.sum())
        if n_class_ == 0:
            continue
        prior_ = n_class_ / float(n_train)
        pos_hits_ = np.asarray(present[y_].sum(axis=0)).ravel().astype(np.int64)

        idx_ = np.flatnonzero(pos_hits_ > 0)
        if pair_filter is not None:
            allowed_ = pair_filter.get(c_, set())
            idx_ = np.array(
                [j for j in range(len(vocab)) if vocab[j] in allowed_],
                dtype=np.int64,
            )
            if idx_.size == 0:
                continue

        prec_ = pos_hits_[idx_] / np.maximum(total_hits_[idx_], 1)
        power_ = wilson_lower(pos_hits_[idx_], total_hits_[idx_])

        if pair_filter is None:
            # directional guard + evidence floor; both are cheap and both exist
            # to protect the published table rather than the classifier
            keep_ = (prec_ > prior_) & (power_ >= power_floor)
            idx_, prec_, power_ = idx_[keep_], prec_[keep_], power_[keep_]
            if idx_.size == 0:
                continue
            if idx_.size > max_candidates:
                order_ = np.argsort(-power_, kind="stable")[:max_candidates]
                idx_, prec_, power_ = idx_[order_], prec_[order_], power_[order_]

        kept_[c_] = idx_
        frames_.append(pd.DataFrame({
            "Class": c_,
            "Term": vocab[idx_],
            "HitsPos": pos_hits_[idx_],
            "HitsTot": total_hits_[idx_],
            "NClass": n_class_,
            "NTrain": int(n_train),
            "Prior": prior_,
            "Precision": prec_,
            "Reach": pos_hits_[idx_] / float(n_class_),
            "Power": power_,
        }))

    stats_ = (pd.concat(frames_, ignore_index=True) if frames_
              else pd.DataFrame(columns=["Class", "Term", "HitsPos", "HitsTot",
                                         "NClass", "NTrain", "Prior",
                                         "Precision", "Reach", "Power"]))
    return stats_, kept_


def class_incidence(present, vocab, labels, kept_index):
    """Long (Class, Term, DocIdx) for candidate terms, class documents only.

    The greedy marginal-reach rule asks "which documents OF THIS CLASS does this
    term catch that the accepted terms missed", so incidence outside the class is
    never needed. Restricting it here is what keeps these files small.
    DocIdx indexes the training frame (0-based), matching docs_train.parquet.
    """
    labels_ = np.asarray(labels)
    frames_ = []
    for c_, idx_ in kept_index.items():
        rows_ = np.flatnonzero(labels_ == c_)
        if rows_.size == 0 or idx_.size == 0:
            continue
        sub_ = present[rows_][:, idx_].tocoo()
        if sub_.nnz == 0:
            continue
        frames_.append(pd.DataFrame({
            "Class": c_,
            "Term": vocab[idx_][sub_.col],
            "DocIdx": rows_[sub_.row].astype(np.int32),
        }))
    if not frames_:
        return pd.DataFrame(columns=["Class", "Term", "DocIdx"])
    return pd.concat(frames_, ignore_index=True)


def test_incidence(present_test, vocab, kept_index):
    """Long (Term, DocIdx) over the union of candidate terms, held-out fold."""
    union_ = np.unique(np.concatenate([v for v in kept_index.values()])) \
        if kept_index else np.array([], dtype=np.int64)
    if union_.size == 0:
        return pd.DataFrame(columns=["Term", "DocIdx"])
    sub_ = present_test[:, union_].tocoo()
    if sub_.nnz == 0:
        return pd.DataFrame(columns=["Term", "DocIdx"])
    return pd.DataFrame({
        "Term": vocab[union_][sub_.col],
        "DocIdx": sub_.row.astype(np.int32),
    })


class _Tee:
    """Write to several streams at once (stdout/stderr + a per-run log file)."""

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
    ap.add_argument("--source", default="text", choices=["text", "docdesc"])
    ap.add_argument("--text-col", default="Text")
    ap.add_argument("--desc-col", default="DocDesc")
    ap.add_argument("--fold-col", default="Fold")
    ap.add_argument("--test-fold", type=int, default=1,
                    help="held-out fold; 0 mines ALL labelled rows (no test set)")
    ap.add_argument("--nwords", type=int, default=0,
                    help="truncate body to first N whitespace words (0 = full)")
    ap.add_argument("--ngram-max", type=int, default=3)
    ap.add_argument("--min-df", type=int, default=3)
    ap.add_argument("--max-df", type=float, default=0.5)
    ap.add_argument("--min-token-len", type=int, default=3)
    ap.add_argument("--stopwords", default="none",
                    choices=["none", "english", "domain", "english_domain"],
                    help="none keeps function-word n-grams (best for macro-F1); "
                         "english_domain yields content-word terms (best for a "
                         "human-readable published table)")
    ap.add_argument("--power-floor", type=float, default=0.30,
                    help="permissive pre-filter; the real floors are swept in R")
    ap.add_argument("--max-candidates", type=int, default=300,
                    help="candidate cap per class; bounds artifact size only")
    ap.add_argument("--terms-file", default=None,
                    help="parquet/csv of (Class, Term): score a manual list instead of mining")
    ap.add_argument("--seed", type=int, default=42,
                    help="stamped for parity; mining is deterministic")
    ap.add_argument("--runs-root", default="2_output/03C-ClassifyTrainKeyword/mines")
    ap.add_argument("--overwrite", action="store_true")
    ap.add_argument("--smoke", action="store_true")
    args = ap.parse_args()

    manual = args.terms_file is not None
    ngram_range = (1, args.ngram_max)
    stopwords = resolve_stopwords(args.stopwords)

    # A manual run is identified by the LIST it scores, not merely by the task and window. Without
    # this, two different term lists for the same task write to the same directory, the second run
    # finds the first one's output and skips, and the second list is silently reported using the
    # first list's statistics. Nothing errors and nothing looks wrong.
    terms_df = None
    terms_hash = None
    if manual:
        tf_ = Path(args.terms_file)
        terms_df = (pd.read_parquet(tf_) if tf_.suffix == ".parquet" else pd.read_csv(tf_))
        miss_ = {"Class", "Term"} - set(terms_df.columns)
        if miss_:
            raise SystemExit(f"terms file missing columns: {sorted(miss_)}")
        terms_df["Term"] = terms_df["Term"].astype(str).str.lower().str.strip()
        terms_df = terms_df.drop_duplicates(subset=["Class", "Term"])
        payload_ = "\n".join(sorted(f"{c}\t{t}" for c, t in zip(terms_df["Class"], terms_df["Term"])))
        terms_hash = hashlib.sha1(payload_.encode("utf-8")).hexdigest()[:10]

    tag_w_ = "full" if args.nwords == 0 else str(args.nwords)
    if manual:
        # The regime belongs in the name for the same reason the list hash does: it changes how the
        # supplied terms are tokenised and therefore what statistics they receive. Two runs of one
        # list under different regimes are different runs.
        mine_name = (f"{args.label_col}__kwmanual-{args.source}__"
                     f"W{tag_w_}_SW{SW_TAG[args.stopwords]}_{terms_hash}")
    else:
        mine_name = (
            f"{args.label_col}__kwmine-{args.source}__"
            f"W{tag_w_}_N{args.ngram_max}_D{args.min_df}_TL{args.min_token_len}_"
            f"SW{SW_TAG[args.stopwords]}"
        )
    run_name = f"{mine_name}_F{args.test_fold}"
    run_token = f"kwmine:{args.source}:{args.label_col}:W{tag_w_}:F{args.test_fold}"

    runs_root = Path(args.runs_root)
    if args.smoke:
        runs_root = runs_root / "_smoke"
    run_dir = runs_root / run_name

    done_marker = run_dir / "termstats.parquet"
    if done_marker.exists() and not args.overwrite and not args.smoke:
        print(f"[skip  ] mine exists: {run_dir} (use --overwrite to redo)")
        return
    run_dir.mkdir(parents=True, exist_ok=True)

    _logf = open(run_dir / "run.log", "w", encoding="utf-8")
    _orig_out, _orig_err = sys.stdout, sys.stderr
    sys.stdout = _Tee(_orig_out, _logf)
    sys.stderr = _Tee(_orig_err, _logf)

    df = pd.read_parquet(args.data)
    field_col = args.text_col if args.source == "text" else args.desc_col
    need_ = {"DocID", args.label_col, args.fold_col, field_col}
    missing = need_ - set(df.columns)
    if missing:
        raise SystemExit(f"prepared data missing columns: {sorted(missing)}")

    df = df[df[args.label_col].notna()].copy()

    # test-fold 0 = all-data mine: everything trains, nothing is held out
    all_data = args.test_fold == 0
    if all_data:
        train_df = df.copy()
        test_df = df.iloc[0:0].copy()
    else:
        train_df = df[df[args.fold_col] != args.test_fold].copy()
        test_df = df[df[args.fold_col] == args.test_fold].copy()

    min_df, max_df = args.min_df, args.max_df
    if args.smoke:
        train_df = smoke_subset(train_df, args.label_col, 40)
        if len(test_df):
            test_df = smoke_subset(test_df, args.label_col, 10)
        min_df, max_df = 1, 1.0

    classes = sorted(train_df[args.label_col].unique().tolist())
    if len(test_df):
        test_df = test_df[test_df[args.label_col].isin(set(classes))].copy()

    print(f"[run   ] {run_name}")
    print(f"[label ] {args.label_col} | source={args.source} | classes={len(classes)} | "
          f"nwords={tag_w_} | sw={args.stopwords} | test_fold={args.test_fold}{' (ALL-DATA)' if all_data else ''}")
    print(f"[data  ] train={len(train_df)} test={len(test_df)}")

    started = time.time()
    started_iso = datetime.now(timezone.utc).isoformat(timespec="seconds")

    def field_texts(frame):
        raw_ = frame[field_col].fillna("").astype(str).tolist()
        return truncate_words(raw_, args.nwords) if args.source == "text" else raw_

    train_texts = field_texts(train_df)
    test_texts = field_texts(test_df)

    # vocabulary: mined, or fixed to the supplied manual list
    pair_filter = None
    unmatchable = []
    if manual:
        # The supplied terms must be expressed the way THIS run tokenises, or they cannot fire.
        # Under a stopword regime the miner forms n-grams after removal, so "the corporation and the
        # borrower" yields the bigram "corporation borrower"; a list written in either form has to be
        # folded to that canonical shape before it becomes a vocabulary entry. Doing this makes a
        # mined list and a hand-written list matchable under one regime, which is what a union of the
        # two requires.
        probe_ = build_vectorizer((1, 1), 1, 1.0, args.min_token_len, stopwords=stopwords)
        tokenise_ = probe_.build_analyzer()

        canon_ = {}
        unmatchable = []
        for raw_ in sorted(set(terms_df["Term"])):
            toks_ = tokenise_(raw_)
            if not toks_:
                unmatchable.append(raw_)
            else:
                canon_[raw_] = " ".join(toks_)
        if unmatchable:
            print(f"[warn  ] {len(unmatchable)} supplied term(s) survive tokenisation as nothing "
                  f"and can never fire: {unmatchable[:10]}")

        terms_df = terms_df[terms_df["Term"].isin(canon_)].copy()
        terms_df["TermRaw"] = terms_df["Term"]
        terms_df["Term"] = terms_df["Term"].map(canon_)
        n_folded_ = int((terms_df["Term"] != terms_df["TermRaw"]).sum())
        if n_folded_:
            print(f"[terms ] {n_folded_} term(s) folded to their tokenised form")
        terms_df = terms_df.drop_duplicates(subset=["Class", "Term"])

        vocab_list = sorted(terms_df["Term"].unique().tolist())
        max_n_ = max(len(t.split()) for t in vocab_list) if vocab_list else 1
        ngram_range = (1, max(1, max_n_))
        cv = build_vectorizer(ngram_range, min_df, max_df, args.min_token_len,
                              stopwords=stopwords, vocabulary=vocab_list)
        pair_filter = (terms_df.groupby("Class")["Term"]
                       .apply(lambda s: set(s.tolist())).to_dict())
        counts = cv.transform(train_texts)
    else:
        cv = build_vectorizer(ngram_range, min_df, max_df, args.min_token_len,
                              stopwords=stopwords)
        try:
            counts = cv.fit_transform(train_texts)
        except ValueError:
            counts = None

    if counts is None:
        print("[warn  ] empty vocabulary after pruning -- writing empty artifacts")
        vocab = np.array([], dtype=object)
        stats_df = pd.DataFrame(columns=["Class", "Term", "HitsPos", "HitsTot",
                                         "NClass", "NTrain", "Prior",
                                         "Precision", "Reach", "Power"])
        kept_index, inc_train, inc_test = {}, \
            pd.DataFrame(columns=["Class", "Term", "DocIdx"]), \
            pd.DataFrame(columns=["Term", "DocIdx"])
    else:
        vocab = np.array(cv.get_feature_names_out())
        present = (counts > 0).astype(np.int8).tocsr()
        print(f"[vocab ] terms={len(vocab):,}")

        stats_df, kept_index = term_statistics(
            present, vocab, train_df[args.label_col].tolist(), classes,
            len(train_df), args.power_floor, args.max_candidates,
            pair_filter=pair_filter,
        )
        inc_train = class_incidence(present, vocab,
                                    train_df[args.label_col].tolist(), kept_index)
        if len(test_df):
            present_test = (cv.transform(test_texts) > 0).astype(np.int8).tocsr()
            inc_test = test_incidence(present_test, vocab, kept_index)
        else:
            inc_test = pd.DataFrame(columns=["Term", "DocIdx"])

    n_cand_ = len(stats_df)
    print(f"[cand  ] (term, class) pairs kept={n_cand_:,} | "
          f"inc_train={len(inc_train):,} rows | inc_test={len(inc_test):,} rows")

    duration = round(time.time() - started, 1)
    ended_iso = datetime.now(timezone.utc).isoformat(timespec="seconds")

    stats_df.insert(0, "Zone", args.source)
    stats_df.insert(0, "Run", run_token)
    stats_df.insert(0, "MineName", mine_name)
    stats_df.to_parquet(run_dir / "termstats.parquet", index=False)
    inc_train.to_parquet(run_dir / "incidence_train.parquet", index=False)

    pd.DataFrame({
        "DocIdx": np.arange(len(train_df), dtype=np.int32),
        "DocID": train_df["DocID"].to_numpy(),
        "Label": train_df[args.label_col].to_numpy(),
    }).to_parquet(run_dir / "docs_train.parquet", index=False)

    if len(test_df):
        inc_test.to_parquet(run_dir / "incidence_test.parquet", index=False)
        pd.DataFrame({
            "DocIdx": np.arange(len(test_df), dtype=np.int32),
            "DocID": test_df["DocID"].to_numpy(),
            "Label": test_df[args.label_col].to_numpy(),
        }).to_parquet(run_dir / "docs_test.parquet", index=False)

    manifest = {
        "mine_name": mine_name, "run_name": run_name, "run_token": run_token,
        "mode": "manual" if manual else "mine",
        "smoke": bool(args.smoke), "all_data": bool(all_data),
        "label_col": args.label_col, "source": args.source,
        "field_col": field_col, "nwords": args.nwords,
        "test_fold": args.test_fold, "ngram_range": list(ngram_range),
        "min_df": args.min_df, "max_df": args.max_df,
        "min_token_len": args.min_token_len, "stopwords": args.stopwords,
        "n_stopwords": (0 if stopwords is None else
                        len(ENGLISH_STOP_WORDS) if stopwords == "english"
                        else len(stopwords)),
        "power_floor": args.power_floor, "max_candidates": args.max_candidates,
        "terms_file": (None if args.terms_file is None else str(Path(args.terms_file).resolve())),
        "terms_hash": terms_hash,
        "n_terms_supplied": (0 if terms_df is None else int(len(terms_df))),
        "unmatchable_terms": unmatchable,
        "seed": args.seed, "n_classes": len(classes), "classes": classes,
        "data_path": str(args.data),
        "n_train": len(train_df), "n_test": len(test_df),
        "n_vocab": int(len(vocab)), "n_candidates": int(n_cand_),
        "duration_sec": duration,
        "started_at": started_iso, "ended_at": ended_iso,
        "versions": {"python": sys.version.split()[0],
                     "numpy": np.__version__,
                     "pandas": pd.__version__,
                     "sklearn": sklearn.__version__},
        "git_commit": git_commit(),
    }
    (run_dir / "mine.json").write_text(json.dumps(manifest, indent=2))

    print(f"[done  ] candidates={n_cand_:,} ({duration}s)")
    print(f"[write ] {run_dir}")

    sys.stdout, sys.stderr = _orig_out, _orig_err
    _logf.close()


if __name__ == "__main__":
    main()
