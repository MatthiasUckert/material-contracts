# The extraction image

`contracts-lexnlp-17a366b025fe.tar.gz` is the Docker image that extracted the organisation candidates
of the database: LexNLP 2.3.0 in Python 3.8, with the NLTK data it needs baked in, so it runs offline.
The rules that turn candidates into the published parties are part of the pipeline, not of the image.

## Loading it

```bash
docker load -i contracts-lexnlp-17a366b025fe.tar.gz     # restores the image as contracts-lexnlp:latest
```

**The image is built for 64-bit ARM** (`linux/arm64`, native on Apple Silicon). On an Intel or AMD
machine Docker runs it under emulation, which works but is several times slower. To run natively
there, rebuild it from the `Dockerfile` in the pipeline repository (folder `contracts-lexnlp/`, at
commit `e3f97216ef96d58d6001faf23bc2e5017eed5b7a`) and pin its packages to `requirements.lock.txt`
here; a rebuild is the same software, not the same image.

## Running it

The image reads parquet files with a `DocID` and a `TextRaw` column, which is what `text/` holds, and
writes one parquet file with one row per match:

```bash
docker run --rm -v "$PWD":/work contracts-lexnlp:latest \
  exhibit10_2012.parquet \
  --output lexnlp_org_2012.parquet \
  --label ORG \
  --n-process 8
```

| Option | Default | Meaning |
|:--|:--|:--|
| inputs | | one or more parquet files or folders, relative to the mounted directory |
| `--output` | | the parquet file to write |
| `--label` | `ORG` | what to extract: `ORG`, `DATE`, `GPE`, `MONEY`, `AMOUNT`, `PERCENT`, `RATIO`, `DURATION` |
| `--id-col`, `--text-col` | `DocID`, `TextRaw` | the input columns |
| `--timeout` | 60 | seconds per extractor and document; past it, the document keeps its other rows |
| `--max-chars` | 0 (off) | read only the first N characters of each document |
| `--n-process` | 1 | worker processes; 0 or less uses all cores |

The output has `DocID`, `Start`, `Stop`, `Span`, `Label`, `LabelRaw`, `Engine` and `Model`, plus
columns specific to each label (for `ORG`: `Name`, `NameAbbr`, `TypeFull`, `TypeAbbr`, `TypeLabel`,
`Description`). Offsets follow the same rule as the published spans: `TextRaw[Start:Stop] == Span`. A
document without any match appears once, with empty offsets, so every input document is accounted
for.

The database used the image for `ORG` only. Its places, dates, law clauses and redaction markers come
from the pipeline's own extractors. `text/` holds exactly the text the image read.

## Licence

LexNLP is licensed under the GNU Affero General Public License v3.0 (see `NOTICE.md`). The image
contains it and is distributed under that licence.
