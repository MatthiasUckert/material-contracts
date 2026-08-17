"""Tokenisation shared by keyword mining and keyword application.

WHY THIS FILE EXISTS
--------------------
A keyword table's published precision is a property of a PAIR: the lexicon, and the rule that decides
whether a term occurs in a document. Mine under one rule and apply under another and the number
measured is not a number about the system that runs -- the terms still fire, just on different
documents, and nothing in the output says so.

The rule lived twice: here, in sklearn's analyzer, and again as a hand-written substring search on the
R side. This module is the single copy. keyword_train.py mines with it and keyword_apply.py applies
with it, so the pair cannot come apart.

WHAT THE RULE IS
----------------
Lowercase, alphabetic tokens of at least min_token_len characters, stopwords removed, then n-grams
formed over what survives. The order matters and is the whole subtlety: removal happens BEFORE
n-gram formation, so "the corporation and the borrower" yields the bigram "corporation borrower".
A term mined that way cannot be found by looking for it literally.

Numbers and punctuation never become keywords. Exhibit numbers, dates and dollar amounts are
entity-extraction territory and would dominate an n-gram vocabulary if admitted here.
"""

import warnings

import numpy as np
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer, ENGLISH_STOP_WORDS

# A custom token_pattern paired with a stop_words list triggers a benign sklearn UserWarning about
# the two possibly disagreeing. They do not: both are defined here, together, for that reason.
warnings.filterwarnings("ignore", message=".*stop_words may be inconsistent.*")

# SEC / EDGAR filing boilerplate that survives the alpha token filter but carries no class signal.
# Deliberately EXCLUDES amend/restated: those are the amendment task's entire signal.
DOMAIN_STOPWORDS = frozenset({
    "exhibit", "exhibits", "txt", "htm", "html", "pdf", "doc", "docx",
    "page", "pages", "dated", "form", "forms", "schedule", "schedules",
    "annex", "appendix", "registrant", "filed", "filing",
})

# Stamped into the mine name so regimes are distinct on disk and become a leaderboard axis rather
# than a silent setting.
SW_TAG = {"none": "none", "english": "en", "domain": "dom", "english_domain": "endom"}

STOPWORD_CHOICES = ("none", "english", "domain", "english_domain")


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


def truncate_words(texts, n_words):
    """First n_words whitespace words of each text (0 = full document)."""
    if not n_words:
        return texts
    return [" ".join(t.split()[:n_words]) for t in texts]


def build_vectorizer(ngram_range, min_token_len, stopwords=None, vocabulary=None,
                     min_df=1, max_df=1.0):
    """CountVectorizer under the project's tokenisation.

    STOPWORDS ARE HONOURED WHETHER OR NOT A VOCABULARY IS SUPPLIED, which is the correction this
    module exists to carry. Dropping them when a vocabulary is fixed changes the analyzer rather than
    the vocabulary: tokens are no longer removed, so n-grams no longer close over the gaps they left,
    and a term mined as "corporation borrower" is then searched for as two literally adjacent words.
    It scores zero against the very document it was mined from, silently, and the table appears to
    have lost its precision when what it lost was its tokenisation.
    """
    return CountVectorizer(
        lowercase=True,
        ngram_range=ngram_range,
        min_df=min_df if vocabulary is None else 1,
        max_df=max_df if vocabulary is None else 1.0,
        stop_words=stopwords,
        token_pattern=rf"(?u)\b[a-zA-Z]{{{min_token_len},}}\b",
        vocabulary=vocabulary,
    )


def canonical_terms(terms, stopwords, min_token_len):
    """Fold each term to the form this tokenisation would produce.

    A term has to be expressed the way the analyzer expresses it or it can never fire. Under a
    stopword regime "the corporation and the borrower" is stored as "corporation borrower", and a
    lexicon written either way has to be folded to that shape before it becomes a vocabulary entry.

    Returns (mapping, unmatchable): terms surviving tokenisation as nothing are reported rather than
    silently dropped, because a term that can never fire is a table promising coverage it has not got.
    """
    probe_ = build_vectorizer((1, 1), min_token_len, stopwords=stopwords)
    tok_ = probe_.build_analyzer()

    mapping_, unmatchable_ = {}, []
    for raw_ in sorted(set(terms)):
        toks_ = tok_(raw_)
        if toks_:
            mapping_[raw_] = " ".join(toks_)
        else:
            unmatchable_.append(raw_)
    return mapping_, unmatchable_


def ngram_max(vocabulary):
    """Longest term in the vocabulary, in words.

    DERIVED, never configured. Matching needs n-grams only as long as the longest term actually
    present, and the lexicon states that. A pinned value can fall out of step with the file it
    describes, and the failure is silent in the worst direction: a trigram lexicon matched at
    bigram-max loses its longest and most precise terms with no error anywhere.
    """
    if len(vocabulary) == 0:
        return 1
    return max(1, max(len(t.split()) for t in vocabulary))


def incidence(texts, doc_ids, vocabulary, stopwords, min_token_len):
    """Long (DocID, Term) for every lexicon term occurring in each document.

    Presence, not count: the decision rule downstream asks whether a term occurred, and a term
    occurring three times in one contract is not three times the evidence.
    """
    vocab_ = sorted(set(vocabulary))
    if len(vocab_) == 0 or len(texts) == 0:
        return pd.DataFrame({"DocID": pd.Series(dtype="object"),
                             "Term": pd.Series(dtype="object")})

    cv_ = build_vectorizer(
        ngram_range=(1, ngram_max(vocab_)),
        min_token_len=min_token_len,
        stopwords=stopwords,
        vocabulary=vocab_,
    )
    present_ = (cv_.transform(texts) > 0).tocoo()
    if present_.nnz == 0:
        return pd.DataFrame({"DocID": pd.Series(dtype="object"),
                             "Term": pd.Series(dtype="object")})

    ids_ = np.asarray(doc_ids, dtype=object)
    names_ = np.asarray(cv_.get_feature_names_out(), dtype=object)
    return pd.DataFrame({
        "DocID": ids_[present_.row],
        "Term": names_[present_.col],
    })
