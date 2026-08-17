# matcon-extract

Deterministic entity extraction from contract text. Four pattern-and-lookup extractors that emit
character-offset spans into one schema.

```
pip install matcon-extract
python -m matcon_extract contracts.parquet --out-dir out/ --label DATE TERM MONEY
```

## What makes it different

**It cannot supply a value the text does not contain.** Every date requires a written year; every
term requires a number and a unit. A grammar-based extractor will resolve "May 1 of each year" by
supplying a year of its own, and where that year comes from the clock, the same document yields
different dates on different days. Measured against one such extractor on 4,398 SEC contracts: of
its spans carrying no written year, 76% resolve to the future at a median of 11.6 years out, and
80.8% of all dates more than fifteen years out are year-less. Section numbers and exhibit
references are what that produces.

**It installs in seconds.** pandas, pyarrow, tqdm. No models, no torch, no download.

## Labels

| Label | Extractor | Extras |
|:--|:--|:--|
| `DATE` | `dateregex` | `DateValue` |
| `TERM` | `dateregex` | `TermN`, `TermUnit`, `TermYears` |
| `MONEY` | `moneyregex` | `Amount`, `Currency` |
| `GPE` | `gazetteer` | `GeoKey`, `IsWord`, `NParent`, `Iso2`, `MatchKind` |
| `REDACT` | `redaction` | -- |

## The offset contract

Every span is 0-based, half-open, over **code points**, and `text[Start:Stop] == Span` exactly.
Consumers in languages that index by byte must slice by code point -- in R that means
`stringi::stri_sub`, never `substr`.

## Reading the output

One parquet per model, named `matcon__<model>.parquet`. Core columns are `DocID`, `Start`, `Stop`,
`Span`, `Label`, `LabelRaw`, `Engine`, `Model`, followed by that label's extras.

**`LabelRaw` is provenance and must be read.** `MonthYear` means the day in `DateValue` is a
placeholder. `OpenEnded` means `TermYears` is absent because the term is unbounded, not because
parsing failed. A match that denotes no date keeps its span and gets a null value: found-and-
unparseable is a third state beyond found and not found.

**A document that matched nothing still appears**, as a single row with the engine and model set
and everything else null. That is what distinguishes a document that was processed from one that
was never seen.

## Versions

`--version` prints `engine model spec-hash` for every extractor. The model tag is the version; the
filename is not. The hash covers the constants that determine output, so a changed pattern under an
unchanged model tag is detectable rather than silent.

## License

MIT.
