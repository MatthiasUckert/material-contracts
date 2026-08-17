# contracts-classify

Contract classification, both arms. The 03 family's Python side.

| Script | Arm | Called from |
|:--|:--|:--|
| `classify_train.py` | transformer fine-tuning | `03B-ClassifyTrainBERT` |
| `classify_apply.py` | transformer application | `03F-ClassifyApply` |
| `keyword_train.py` | keyword mining | `03C-ClassifyTrainKeyword` |
| `keyword_text.py` | tokenisation shared by the keyword scripts | -- |
| `keyword_apply.py` | keyword application | `03F-ClassifyApply` |

```bash
uv venv --python 3.12
uv pip install torch transformers scikit-learn numpy pandas pyarrow

.venv/bin/python classify_train.py --help
.venv/bin/python keyword_train.py  --help
```

## Why both arms share one environment

The keyword arm needs numpy, pandas and scikit-learn, and no torch. On dependency weight alone it
would be its own folder -- that is the argument that separated `contracts-extract`.

It is not, because **03F applies both arms to the corpus in one pass.** Splitting them would give
that document two interpreter paths where it has one, and the benefit is a publication that has not
happened. When `matcon-classify` is actually published, the keyword arm moves to its own folder;
that is a fifteen-minute change and there is no reason to pay for it early.

The folder is named for the CONCERN rather than the library, which is what it shares with
`contracts-extract`: two of ours named for what they do, two third-party wrappers named for what
they wrap.

## What is publishable here, and what is not

**The keyword miner is a contribution.** Precision-first term selection on Wilson lower bounds,
per-fold mining, filer-diversity floors. The output is a term list you can read, which is the point:
a classifier you can inspect rather than probe.

**The transformer scripts are not.** Standard fine-tuning against a public checkpoint.

## Dependency note

`scikit-learn` is declared here and was declared **nowhere before** -- not in
`contracts-engine/pyproject.toml`, not in its lock. Three of these five scripts import it.

## What the two arms established

**Crowned configuration:** legal-bert, 6 epochs, unweighted, LR 2e-5, 512 tokens. Macro-F1 0.882 /
accuracy 0.916 on ClassDetailed; 0.862 / 0.903 on AmendType. Deployed as an all-data refit.

**Routing does not improve on BERT.** The oracle ceiling equals BERT-everywhere at 0.882 -- keyword
confidence correlates with BERT confidence, so keyword gates fire on the documents BERT already gets
right. A clean null result, and the reason the keyword arm is reported rather than deployed.
