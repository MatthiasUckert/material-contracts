# contracts-spacy

spaCy NER over contract text, emitting the shared candidate schema.

```bash
uv venv --python 3.12
uv pip install -r <(python3 -c "import tomllib;print('\n'.join(tomllib.load(open('pyproject.toml','rb'))['project']['dependencies']))")
.venv/bin/python -m spacy download en_core_web_lg

.venv/bin/python extract_spacy.py docs.parquet --output out.parquet --model en_core_web_lg
```

## Why this is its own folder

`spacy[transformers]` pulls torch and a gigabyte of models. Nothing light can share an environment
with it, which is the whole reason the suites are split by dependency weight rather than by what
they extract.

## Why it is NOT built on matcon-extract

The other extractors are pattern sweeps over one document at a time and share `matcon_extract._io`.
This one is not: it streams through `nlp.pipe`, windows documents past `nlp.max_length` and shifts
offsets back to document-absolute, chooses a device, and guards stalls at the PIPE level rather than
per document. Forcing it into a shape built for regex extractors would be a rewrite of verified
code that has run corpus-scale, for no gain.

The cost is accepted and named: `resolve_inputs` and `truncate_texts` are duplicated here. They are
twenty lines and they do not change.

## What it must still satisfy

The schema is shared even though the implementation is not.

- Offsets are 0-based, half-open, over **code points**: `text[Start:Stop] == Span` exactly
- Every input `DocID` appears at least once; a document with no entities gets a sentinel row
- `Engine` is `spacy`; `Model` carries the model name, so a row is self-describing
- `Label` is the cross-engine vocabulary (`LABEL_MAP`), `LabelRaw` keeps the native spaCy tag

## Note on devices

`--device mps` accelerates the transformer model only. The CNN models (sm/md/lg) stay on CPU on
Apple silicon. On any GPU `n_process` is forced to 1, because one device cannot be shared across
worker processes.
