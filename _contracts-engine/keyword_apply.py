"""Apply a published keyword lexicon to documents, and emit which terms occurred.

WHAT THIS DOES, AND WHAT IT DELIBERATELY DOES NOT
-------------------------------------------------
It answers one question: for each document, which terms of this lexicon occur in it. It does not
know what a class is, what a threshold is, or how a label is chosen. Those belong to the decision
rule, which lives once on the R side and is called identically by the stage that measures a table's
precision and the stage that applies it to the corpus.

The seam is here because this is the boundary of what only Python can do. The tokenisation is
sklearn's analyzer, and reproducing it elsewhere is what put a hand-written substring search in front
of a lexicon mined under stopword removal -- the table committed on 36% of the corpus where it had
published 52%, and nothing anywhere reported a fault. Emitting finished predictions instead would
move a second rule into this file and recreate the same problem one layer up.

USAGE
    python keyword_apply.py --lexicon table.parquet --data docs.parquet --out hits.parquet \
        --stopwords english_domain --min-token-len 3 --nwords 256

The lexicon needs a Term column; anything else it carries is ignored here and used by the decision
rule. The document table needs DocID and a text column.
"""

import argparse
import sys
import time

import pandas as pd

from keyword_text import (
    STOPWORD_CHOICES,
    canonical_terms,
    incidence,
    resolve_stopwords,
    truncate_words,
)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--lexicon", required=True, help="published table parquet, with a Term column")
    ap.add_argument("--data", required=True, help="documents parquet")
    ap.add_argument("--out", required=True, help="destination parquet: DocID, Term")
    ap.add_argument("--doc-col", default="DocID")
    ap.add_argument("--text-col", default="Text")
    ap.add_argument("--desc-col", default="DocDesc")
    ap.add_argument("--source", default="text", choices=["text", "docdesc"],
                    help="which field the table was mined against")
    ap.add_argument("--nwords", type=int, default=0,
                    help="truncate to the first N whitespace words; 0 = whole document. MUST match "
                         "the window the table was mined at: its precision was measured there")
    ap.add_argument("--stopwords", default="english_domain", choices=list(STOPWORD_CHOICES),
                    help="MUST match the regime the table was mined under; it decides which n-grams "
                         "can form at all")
    ap.add_argument("--min-token-len", type=int, default=3)
    args = ap.parse_args()

    t0_ = time.time()

    lex_ = pd.read_parquet(args.lexicon)
    if "Term" not in lex_.columns:
        sys.exit(f"[error ] {args.lexicon} has no Term column")

    docs_ = pd.read_parquet(args.data)
    for col_ in (args.doc_col,):
        if col_ not in docs_.columns:
            sys.exit(f"[error ] {args.data} has no {col_} column")

    text_col_ = args.text_col if args.source == "text" else args.desc_col
    if text_col_ not in docs_.columns:
        sys.exit(f"[error ] {args.data} has no {text_col_} column")

    stops_ = resolve_stopwords(args.stopwords)
    raw_ = docs_[text_col_].fillna("").astype(str).tolist()
    texts_ = truncate_words(raw_, args.nwords) if args.source == "text" else raw_

    # The lexicon is folded through the same analyzer before it becomes a vocabulary. A table mined
    # under this regime is already canonical and folds to itself; a hand-written list is not, and
    # folding is what lets the two be used interchangeably.
    map_, unmatchable_ = canonical_terms(
        terms=lex_["Term"].astype(str).tolist(),
        stopwords=stops_,
        min_token_len=args.min_token_len,
    )
    if unmatchable_:
        print(f"[warn  ] {len(unmatchable_)} term(s) survive tokenisation as nothing and can never "
              f"fire: {unmatchable_[:10]}")

    vocab_ = sorted(set(map_.values()))
    if not vocab_:
        sys.exit("[error ] no term in this lexicon survives tokenisation; nothing can be applied")

    hits_ = incidence(
        texts=texts_,
        doc_ids=docs_[args.doc_col].tolist(),
        vocabulary=vocab_,
        stopwords=stops_,
        min_token_len=args.min_token_len,
    )

    # Reported back in the lexicon's own spelling, so the decision rule can join on the column it
    # already has rather than learning about canonicalisation.
    back_ = {}
    for raw_term_, canon_ in map_.items():
        back_.setdefault(canon_, raw_term_)
    if len(hits_):
        hits_ = hits_.assign(Term=hits_["Term"].map(back_))

    hits_.to_parquet(args.out, index=False)

    n_docs_ = docs_[args.doc_col].nunique()
    n_hit_ = hits_["DocID"].nunique() if len(hits_) else 0
    print(f"[apply ] {len(vocab_):,} terms | {n_docs_:,} documents | {len(hits_):,} hits on "
          f"{n_hit_:,} documents ({n_hit_ / max(n_docs_, 1):.1%}) | {time.time() - t0_:.1f}s")
    print(f"[write ] {args.out}")


if __name__ == "__main__":
    main()
