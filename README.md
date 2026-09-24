# material-contracts

The working repository of "The analysis of material contracts: Use of SEC contractual data in accounting
research" (Grosskopf, Sehn and Uckert). It holds the pipeline that builds the material-contracts database from
SEC EDGAR: the R and Quarto runbooks for every stage, the Python classifiers and extractors they call, and the
scripts that build every table and figure of the paper.

This is the code as we run it, not a packaged replication. It runs against a local copy of EDGAR (about 1.9
million retrieved documents) that is not in the repository. What we publish for reuse is described at
<https://matthiasuckert.github.io/matcon-data-and-methods/>: the data package (the database, the entity spans,
the documents, the fine-tuned classifiers and the extraction image) with a documentation site and runnable
examples on a small sample. Online Appendix B of the paper describes the pipeline stage by stage, Appendix F the
data package.

## Layout

| Folder | What it holds |
|:--|:--|
| `1_code/` | The pipeline: one `.R` function library plus one `.qmd` runbook per script, numbered in the order they run |
| `1_code/_Commons/` | Functions shared by several scripts (initialisation, entity helpers, tables, plots) |
| `1_code/_Publish/` | The public schema of the data package and the usage guides shipped with the models and the image |
| `1_code/_Templates/` | LaTeX templates for the manuscript and online-appendix fragments the pipeline writes |
| `1_code/_Tests/` | Probes and one-off checks, kept as evidence for statements in the runbooks |
| `contracts-classify/` | Python: fine-tuning and applying the transformer classifiers; the keyword table |
| `contracts-extract/` | Python package `matcon_extract`: regex and gazetteer extractors (dates, terms, places, law, redactions) |
| `contracts-lexnlp/` | The Docker image and script that run LexNLP for organisation candidates |
| `contracts-spacy/` | Python: the spaCy run on the labelled sample, used for the comparison in Online Appendix D only |
| `renv.lock`, `renv/` | The R environment, pinned |

The two R packages the pipeline uses have repositories of their own: [rGetEDGAR](https://github.com/MatthiasUckert/rGetEDGAR)
(retrieval and parsing of EDGAR filings, tag v0.1.0) and [rLabelDocs](https://github.com/MatthiasUckert/rLabelDocs)
(the Shiny application the labelled sample was produced with, tag v0.1.1).

## The scripts and the stages of the paper

| Scripts | Stage (Online Appendix B) |
|:--|:--|
| `01A`-`01E` | Acquisition: EDGAR index, documents, metadata, 8-K items, confidential treatment orders |
| `02A`-`02B` | Database: Compustat link and the register of unique attachments |
| `03A`-`03F` | Classification: sample preparation, transformer, keyword table, LLM arm, orchestration, application |
| `04A`-`04D` | Entity extraction: extraction, the rules per entity kind, corpus run, application |
| `05A`-`05B` | Text searches and the 8-K item taxonomy |
| `10` | Release: export of the database |
| `30`, `50` | The paper's tables and figures, and the numbers the manuscript cites |
| `40A`-`40B` | Publication: the code and the data package |

Each runbook states its purpose, inputs, construction, validation and outputs in its own text. A script reads what
the scripts before it produced under `2_output/<script>/` (not in the repository) and writes its own result there.

## Environments

- R: `renv::restore()` from `renv.lock` (R 4.5). Quarto renders the runbooks from `1_code/` (see `1_code/_quarto.yml`).
- Python: each of `contracts-classify/`, `contracts-extract/` and `contracts-spacy/` is a `uv` project with its own
  `pyproject.toml` and `uv.lock` (`uv sync` inside the folder). `contracts-lexnlp/` is built as a Docker image from
  its `Dockerfile` and `requirements.lock.txt`; the built image is part of the data package.
- Compustat (via WRDS) is used in `02A` and is not published. The site says which steps need it.

## Licence

MIT (`LICENSE`), except `contracts-lexnlp/`, which runs LexNLP (AGPL-3.0) and is published under the AGPL-3.0 as
well (`contracts-lexnlp/NOTICE.md`).
