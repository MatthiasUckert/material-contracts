---
title: "Final Exhibits -- The Manuscript's Tables and Figures, from the Release (31)"
author: "Matthias Uckert"
date: "`r format(Sys.Date(), '%Y-%m-%d')`"
---

```{r}
#| label: setup
#| include: false
#| purl: false

here::i_am("1_code/_Commons/_Initialize.R")
knitr::opts_knit$set(root.dir = here::here())

# message = TRUE is required: cli writes through the message stream under knitr, so suppressing
# messages would discard this document's entire output. comment = "" removes the "##" prefix from
# console blocks so they can be copied verbatim. Figure geometry is NOT set here -- it is declared
# once in _quarto.yml.
knitr::opts_chunk$set(
  root.dir = here::here(),
  message  = TRUE,
  warning  = FALSE,
  comment  = ""
)

options(cli.num_colors = 1, cli.width = 120, mc.table_mode = "console")
```

```{r}
#| label: load-libraries
#| echo: false

# THE COMMONS FIRST. _Plots.R rebuilds its level registry empty every time it is sourced, so every
# library that registers a vocabulary must follow it.
.path_libs <- purrr::map_chr(
  c("_Initialize", "_Utils", "_NER", "_Plots", "_Tables", "_Entity"),
  \(.s) here::here("1_code", "_Commons", paste0(.s, ".R"))
)
purrr::walk(.x = .path_libs, .f = \(.p) source(file = .p, encoding = "UTF-8"))

# THREE UPSTREAM LIBRARIES, IN THIS ORDER. 03A owns the class vocabulary; 10 owns exp_cache_hit() and
# the column-by-column report; 30 owns every read function, every vocabulary object and every
# exhibit tibble this document draws from. 30 registers the paper's class order and the form families
# when sourced, which is why it comes after _Plots and before this document's own library.
.path_libs <- c(
  .path_libs,
  purrr::map_chr(
    c("03A-ClassifyPrepare", "10-ExportData", "30-Descriptives"),
    \(.s) init_create_script_fun(.dir_here = here::here(), .name_script = .s)
  )
)
purrr::walk(.x = utils::tail(.path_libs, 3L), .f = \(.p) source(file = .p, encoding = "UTF-8"))

.name_script <- "31-FinalExhibits"

cat("Main Directory: ")
(.dir_main <- init_create_script_dir(.dir_here = here::here(), .name_script = .name_script))

cat("Function File:  ")
(.path_fun <- init_create_script_fun(.dir_here = here::here(), .name_script = .name_script))

source(file = .path_fun, encoding = "UTF-8")
```

# Purpose

This document produces the exhibits of the manuscript in the form the manuscript prints them: one
view per table or figure, on the sample the paper names, with the decided styling, and with the
figure note written from the same tibble that drew the figure. `30-Descriptives` is the lab -- every
exhibit on every sample, reconciled against both manuscripts -- and stays so; this document is the
paper's copy of it.

## One source, and it is hers

Every input is the one `30` reads, read through `30`'s own functions: the release `10-ExportData`
deploys to her `MatContractPipeline` folder and the outputs of her `101` and `103` do-files. The
conversions of her `.dta` files are `30`'s artifacts under `30`'s cache and are read from there; this
document does not rebuild them unless told to on the page. What this buys is that a number here, a
number in `30` and a number in one of her tables come from the same bytes, and a difference between
them is a difference in definition rather than in data.

## One release, and it is dated

The paper is written on the 9 September release, the first to carry the corrected state and country
counts. Every parquet in the release is checked against that date before anything is read, and a
render on an older file aborts rather than printing numbers the paper would have to retract.

## One exhibit at a time

Each exhibit is added behind the samples as one block: the decision it implements, the sample it is
drawn on, the `30` tibble it reads or the one it computes, its final plot or table function, and the
note. The Overview at the end holds every figure once, in the order the manuscript prints them.
This version holds the readiness checks only; the first exhibit follows the first decision.

# Configuration

```{r}
#| label: configure

.lP <- list(
  # WHERE HER FOLDER IS. The Dropbox root on this machine; ~/Downloads/Pipeline is a copy with the same
  # layout and works as a drop-in while Dropbox is offline.
  Root = "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
  Params = list(
    # THE RELEASE THE PAPER IS WRITTEN ON. Every .parquet under Input must be at least this new.
    ReleaseMin = "2026-09-09",
    # THE FOUR DEFINITIONAL SWITCHES, 30's defaults: 103's redaction rule with one marker enough, the
    # cascade duration that reproduces Table 3, the adjusted word count the text prints, and the
    # recital parties. Each is settled or overturned in the exhibit that depends on it.
    Redaction  = "symbolexplicit",
    RedactMin  = 1L,
    Duration   = "cascade",
    Words      = "adjusted",
    Parties    = "recital"
  )
)

.lP$Input <- list(
  FilContracts     = fs::path(.lP$Root, "100_Data_Export", "Contracts.parquet"),
  FilSummaries     = fs::path(.lP$Root, "100_Data_Export", "Summaries.parquet"),
  FilPlaces        = fs::path(.lP$Root, "100_Data_Export", "Places.parquet"),
  FilTermDocs      = fs::path(.lP$Root, "100_Data_Export", "TermDocs.parquet"),
  FilCtoOrders     = fs::path(.lP$Root, "100_Data_Export", "CtoOrders.parquet"),
  DtaQuarter       = fs::path(.lP$Root, "103_Variable_Creation", "103_Output",
                              "quarter_dictionary_COMPUSTAT_variables.dta"),
  DtaAkContract    = fs::path(.lP$Root, "103_Variable_Creation", "103_Output",
                              "full_dictionary_contract_level_AK.dta"),
  DtaConcentration = fs::path(.lP$Root, "101_Data_prep", "101_Output", "concentration.dta")
)

# 30'S CACHE, READ NOT WRITTEN. The parquet conversions of her .dta files are 30's artifacts; the
# paths are resolved from 30's script directory so that they cannot drift from where 30 writes them.
.dir_30 <- init_create_script_dir(.dir_here = here::here(), .name_script = "30-Descriptives")

.lP$Cache <- list(
  CacheQuarter       = utils_file_path(.dir_30, "Cache", "Quarter.parquet"),
  CacheAkContract    = utils_file_path(.dir_30, "Cache", "AkContract.parquet"),
  CacheConcentration = utils_file_path(.dir_30, "Cache", "Concentration.parquet")
)

# WHERE THE MANUSCRIPT'S COPIES GO. One folder per artifact kind; the file names follow the
# manuscript's once they are known.
.lP$Output <- list(
  DirFigures = utils_file_path(.dir_main, "Output", "Figures"),
  DirTables  = utils_file_path(.dir_main, "Output", "Tables"),
  DirNotes   = utils_file_path(.dir_main, "Output", "Notes"),
  DirData    = utils_file_path(.dir_main, "Output", "Data")
)

purrr::walk(.x = unlist(.lP$Output), .f = fs::dir_create)
```

# Input

## The release, dated

```{r}
#| label: input-vintage

tab_vintage <- fin_check_vintage(
  .inputs   = unlist(.lP$Input),        # every path above
  .min_date = .lP$Params$ReleaseMin,    # the release the paper is written on
  .strict   = FALSE                     # older parquet warns; TRUE aborts, for the manuscript render
)
```

## Her conversions, from 30's cache

```{r}
#| label: input-cache

fin_check_cache(
  .path_cache = .lP$Cache$CacheQuarter,      # 30's conversion of her quarter panel
  .path_dta   = .lP$Input$DtaQuarter,
  .cols       = .des_quarter_cols,           # 30's column map, so a rebuild here equals 30's
  .build      = FALSE                        # TRUE converts into 30's cache from here
)

fin_check_cache(
  .path_cache = .lP$Cache$CacheAkContract,
  .path_dta   = .lP$Input$DtaAkContract,
  .cols       = .des_ak_contract_cols,
  .build      = FALSE
)

fin_check_cache(
  .path_cache = .lP$Cache$CacheConcentration,
  .path_dta   = .lP$Input$DtaConcentration,
  .cols       = NULL,                        # small; every column under its Stata name
  .build      = FALSE
)
```

## The tables

Every table `30` reads, read the same way, switches as in Configuration. Nothing is filtered here;
the samples are named in the next section.

```{r}
#| label: input-read

tab_contracts <- des_read_contracts(
  .path       = .lP$Input$FilContracts,
  .redaction  = .lP$Params$Redaction,   # which marker kinds count
  .redact_min = .lP$Params$RedactMin,   # how many of them
  .duration   = .lP$Params$Duration,    # cascade or naive
  .words      = .lP$Params$Words,       # adjusted or raw
  .parties    = .lP$Params$Parties      # recital, counterparty or spellings
)

stopifnot(setequal(setdiff(unique(tab_contracts$Class), NA), .des_class_levels))

tab_summaries     <- des_read_summaries(.path = .lP$Input$FilSummaries, .tab_contracts = tab_contracts)
tab_quarter       <- des_read_quarter(.path = .lP$Cache$CacheQuarter)
tab_concentration <- des_read_concentration(.path = .lP$Cache$CacheConcentration)
tab_ak            <- des_read_ak_contract(.path = .lP$Cache$CacheAkContract)
tab_places        <- des_read_places(.path = .lP$Input$FilPlaces)
tab_term_docs     <- des_read_term_docs(.path = .lP$Input$FilTermDocs)
tab_cto_orders    <- des_read_cto_orders(.path = .lP$Input$FilCtoOrders)

# HER FLAGS ON THE CONTRACT ROW, as 30 joins them: her sample flag and redaction indicators, so that
# any exhibit can be reconciled against her side row by row.
exp_check_collide(.a = names(tab_contracts), .b = names(tab_ak), .by = "DocID", .what = "the reconciliation join")
tab_contracts <- dplyr::left_join(tab_contracts, tab_ak, by = dplyr::join_by(DocID), relationship = "one-to-one")

fin_report_tables(.tabs = list(
  Contracts     = tab_contracts,
  Summaries     = tab_summaries,
  Quarter       = tab_quarter,
  Concentration = tab_concentration,
  Places        = tab_places,
  TermDocs      = tab_term_docs,
  CtoOrders     = tab_cto_orders
))
```

# Samples

The same membership columns `30` defines, written out again because they are the sample definition
of every exhibit that follows and a reader of this document should not have to open `30` to find
them. The filters are `30`'s to the letter; the invariant check below is what guards the copy.

```{r}
#| label: samples-define

tab_contracts <- tab_contracts |>
  dplyr::mutate(
    # S00: every row of the release, ladder step 1 included.
    S00_Edgar       = TRUE,
    # S0: every row at ladder steps 2-6, every copy. The paper's full sample.
    S0_Universe     = .data$SampleStepCode >= 2L,
    # S1: one row per attachment, whatever its ladder step.
    S1_Unique       = .data$PrimaryFiler == 1L,
    # S2: the descriptive sample. Inside the window, well-formed, one copy per attachment.
    S2_Descriptive  = .data$Keep,
    # S2s: S2 from filers more than two years past their first contract (104_SPAC.do's rule).
    S2s_Seasoned    = .data$Keep & .data$IsSeasoned == 1L,
    # S3: S2 with a matched Compustat quarter; from 2005 for delay, from 2008 for redaction.
    S3_Matched      = .data$Keep & .data$Matched,
    S3a_Matched2005 = .data$Keep & .data$Matched & .data$Year >= 2005L,
    S3b_Matched2008 = .data$Keep & .data$Matched & .data$Year >= 2008L,
    # S6: the redaction window on the descriptive sample, matched or not.
    S6_Redaction    = .data$Keep & .data$Year >= 2008L
  )

# THE CONTRACT-LEVEL SAMPLES, in ladder order. 30 defines this vector in its runbook, not its library,
# so it is written here again; the invariant check below is what guards the copy.
.des_contract_samples <- c(
  "S00_Edgar", "S0_Universe", "S1_Unique", "S2_Descriptive", "S2s_Seasoned", "S3_Matched", "S3a_Matched2005",
  "S3b_Matched2008", "S6_Redaction"
)

# THE TWO SAMPLES THAT ARE NOT CONTRACT TABLES.
lst_other <- list(
  # S7: single-agreement announcements on 8-Ks carrying at most one Exhibit 10, attached or not.
  S7_Summaries = dplyr::filter(tab_summaries, .data$SumIsSingle == 1L, .data$nExhibits <= 1L),
  # S7_All: every announcement 01D recovered, the restriction lifted.
  S7_All       = tab_summaries,
  # S5: her firm-quarter panel inside the window.
  S5_Quarter   = dplyr::filter(tab_quarter, dplyr::between(.data$cyear, 2001L, 2024L))
)

tab_samples <- des_table_samples(.tab = tab_contracts, .samples = .des_contract_samples, .others = lst_other)
```

# Validation

Two checks, each stated before it is read. **The samples nest as the ladder says**: S00 holds S0 and
S1, both hold S2, and S2 holds its subsets and the redaction window; a row outside its parent is a
definition that drifted from `30`'s. **S2 is the paper's number**: 1,136,095, the revision's unique
contracts, with no tolerance because there is no rounding.

```{r}
#| label: validation-samples

tab_counts <- fin_check_samples(
  .tab     = tab_contracts,
  .samples = .des_contract_samples,   # 30's contract-level sample names, ladder order
  .ref     = .des_reference           # the revision's Table 2 Panel A
)
```

# Overview

```{r}
#| label: overview-samples

tbl_out(.tab = tab_samples, .title = "The named samples every exhibit cites")

tibble::tibble(
  Parameter = names(.lP$Params),
  Value     = as.character(unlist(.lP$Params))
) |>
  tbl_out(.title = "The choices this render made")
```

# Next

The exhibits, one block each in the order the manuscript prints them, starting with Table 2. Each
block lands between Samples and Validation as it is decided; Deployment and the manuscript-facing
manifest follow once the first figure exists.
