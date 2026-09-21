# SITE-UPDATE -- adapt the site to the final data package v1.0.0

The data package is final and on Drive: five parts, 124 files, 49.5 GB. The site was built against an earlier export
and reads it everywhere. This brief is the complete delta. Work through the priorities in order; stop after P2.
`CLAUDE.md` still holds: branch, no push, no invented links or figures, render often.

## What the package is now

| Part | Holds |
|:--|:--|
| `core/` | `Contracts`, `Summaries`, `Places`, `TermDocs`, `CtoOrders`, `Labels`, `KeywordTerms` (parquet), `ContractIndex.csv.gz`, one `<Table>_Codebook.csv` each |
| `spans/` | `org_mentions`, `places_geo`, `law_clauses`, `date_spans`, `term_spans`, `redact_spans`, `Spans_Codebook.csv` |
| `text/` | `<type>/<type>_<year>.parquet` for exhibit10, 8k, 8ka, cto; `Text_Codebook.csv` |
| `models/` | six archives, `models_index.csv`, `deployed.parquet`, `README.md` (usage guide) |
| `lexnlp/` | the image, `requirements.lock.txt`, `NOTICE.md`, `README.md` (usage guide) |

The codebooks are the public documentation of every column; render them, do not rewrite them.

## The sample: old path -> new path

| Old | New |
|:--|:--|
| `sample/contracts.parquet` | `sample/core/Contracts.parquet` |
| `sample/summaries.parquet` | `sample/core/Summaries.parquet` |
| `sample/places.parquet` | `sample/core/Places.parquet` |
| `sample/termdocs.parquet` | `sample/core/TermDocs.parquet` |
| `sample/cto_orders.parquet` | `sample/core/CtoOrders.parquet` |
| `sample/labels.parquet` | `sample/core/Labels.parquet` |
| (none) | `sample/core/KeywordTerms.parquet` |
| `sample/text.parquet` | `sample/text/exhibit10/exhibit10_sample.parquet` |
| `sample/spans/<stem>.parquet` | unchanged, except `money_spans.parquet` is gone |
| `sample/codebooks/<Table>_Codebook.csv` | `sample/core/<Table>_Codebook.csv`, `sample/spans/Spans_Codebook.csv`, `sample/text/Text_Codebook.csv` |
| `sample/package_numbers.csv` | unchanged path; new keys `rows.labels`, `rows.keywordterms`; `package.files` now counts the manifest (124) |

The sample now mirrors the package layout, so "change one path" means: replace `sample/` by the package root.
It also holds 3 rejected attachments (`Removed`) and contracts with several registrant copies (`PrimaryFiler`),
so both can be shown at work.

## What left the package, and what came in

- **Compustat is gone:** `gvkey`, `datadate`, `cyear`, `fyear`, `fqtr`. `EstiSample` and the `SampleStep*` columns
  stay. The match to Compustat is rebuilt from `CIK` and `DateFiled`.
- **Amounts are gone entirely:** `spans/money_spans.parquet`; in `Contracts` `nUniAmountNaive`, `nUniAmountUSD`,
  `nUniAmountOther`, `MoneyMaxUSD`, `MoneyMedUSD`, `MoneyMaxOther`, `MoneyMedOther`, `nRedactMoney`; in
  `redact_spans` `RedactedEntity`, `RedactedRef`, `RedactedText`. Say once that amounts were extracted but are not
  used by the paper and not published; do not mention them otherwise.
- **New in `Contracts`: `ClassRollup`,** the paper's broad category (the roll-up of `Class`). `ClassBroad` comes
  from an additional model trained on the broad labels; `HierConsistent` is TRUE exactly where the two agree.
- **New in `ContractIndex`: `AccessionNumber`** and `ClassRollup`.
- **Labels:** `core/Labels.parquet` (the 4,398 labelled contracts as trained on, with `Fold`). There is no
  `ClassificationLabels.csv` any more.
- **Keyword terms:** `core/KeywordTerms.parquet`, one file, the window in use for each task (256 words), with a
  `Task` column. There is no `keyword_tables/` folder and no catalogue any more.
- **Text is published exactly as the models and extractors read it.** Some HTML documents carry quotation marks,
  apostrophes and dashes as control characters (U+0080-U+009F); the package README gives the repair (R and Python).

## P1 -- the site must render and be right

1. **Every read of the sample** uses the new paths (`_common.R`, `data.qmd`, `methods.qmd`, `index.qmd`).
2. **The offset demonstration** in `data.qmd` and **"From spans to variables"** in `methods.qmd` use
   `money_spans`. Move both to dates: `date_spans` for the offset demo, and for the rule example a few lines of dplyr
   over `date_spans`/`term_spans` compared with `DurationYears` or `TermYears` in `Contracts`.
3. **`reproduce.qmd`:** the Compustat paragraph says the identifiers are published. They are not; rewrite it (match
   rebuilt from `CIK` and `DateFiled`; regressions need WRDS).
4. **Roll-up:** replace the map built from the labels in `methods.qmd` by the column `ClassRollup`.
5. **Renames:** `ClassificationLabels` -> `Labels`; `keyword_tables/` and its catalogue -> `KeywordTerms` (one
   file, `Task` column, 256-word window).
6. **Codebooks:** render all of them from their new places, including `Labels`, `KeywordTerms`, `ContractIndex`,
   `Spans` and `Text`.
7. **Known issues** (replace the page's list with these): Columbia read as Colombia (heavy over-count of Colombia in
   places and country counts); a country under several spellings, count on `GeoCountryIso`; `DurationYears` missing
   above 30 years, other date columns uncapped; `ClassBroad` is an additional model, the paper uses `ClassRollup`;
   `LawStart` and `PlaceStart` stored as whole-numbered doubles; one `org_mentions` row without a span for contracts
   without any organisation; no document-level gold set for the entities; the text as read (control characters,
   with the repair). Plus a short "not in the package": Compustat, amounts.

## P2 -- the model guides

`_reference/models-README.md` and `_reference/lexnlp-README.md` are the guides published in the package. The
classification and extraction sections of `methods.qmd` follow them: which model is the paper's
(`ClassDetailed_L256`; the published labels come from the 256-token models for all three tasks, although
cross-validation selected the 512-token model for the broad task), how to load one and reproduce the published
columns, the three rules (input `TextRaw`, the context length in the archive's name, the label mapping from the
archive), and for LexNLP: `docker load`, the ARM64 note, the run command, the output. `models_index.csv` now has
`LabelledPublished`, `Selected` and `MacroF1`.

## Links

The parts are not shared yet. `data/parts.csv` keeps `Status = planned` and empty links, `_variables.yml` keeps
`TODO-PACKAGE-LINK`; the author fills them in. Do not add a sixth part: replication is not published.

## Done means

The site renders from the new `sample/` alone; no page mentions money columns, the Compustat identifiers,
`ClassificationLabels`, `keyword_tables`, `sample/codebooks` or a replication part; the roll-up comes from
`ClassRollup`; the only TODOs left are links; `SITE-REPORT.md` says what changed.
