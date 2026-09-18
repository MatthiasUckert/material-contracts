# 31-FinalExhibits: every table, figure and cited tibble of the paper, in one store ----------------------------------------
#
# WHAT THIS FILE DOES
# The one store of the paper's exhibits: the tables and figures of the manuscript, the online appendix and the
# response memo, and the data-only tibbles 40-Numbers reads its numbers from. Each is written under its name --
# Data/, Tables/, Notes/ and Figures/ under this document's Output -- and no other document is to write there.
#
# WHERE ITS PARTS CAME FROM (moved unchanged on 16 September 2026)
# Sections 0-22 are 30-FinalExhibits' library: the release readers, the prepared tables, the samples, the
# manuscript's exhibits, the manifest and the deployment. Its six full classification tables are registered here
# as ...Full, because the appendix's cuts of them keep the plain names. Sections 23-27 are the builders
# 40-Numbers ran: the two data-only tibbles with their build helper (num_), then the appendix chapters' exhibits,
# chapter by chapter (oaa_, oab_, oac_, oad_). They keep their prefixes until the clean-up, write in the same
# layout under this document's Output, and rebuild only where an input or this library is newer than their files.
# Section 28 shows what those builders write and checks the move against the files 30 and 40 wrote. Section 29
# holds the exhibits first built here, section 30 the firm-fundamentals layouts under choice. Figures are written
# as PNG only (decided 17 September): Overleaf reads the png, and no pdf device is involved.
#
# PREPARED ONCE, READ LAZILY EVER AFTER
# Every input is read through fin_read_*() once, given its derived and membership columns, and
# written under Output/Prepared as one parquet per table. Each prepared table is keyed on what it was
# built from -- the release file or .dta conversion, this library (a reader change must rebuild),
# and for Contracts a hash of the definitional switches -- through the exp_cache_hit() rule every
# cache in the monorepo obeys, with a .rerun switch per table. A prepared table is therefore never
# stale and never rebuilt for nothing; a second render costs a few file_info() calls. Everything after
# it opens the prepared files with arrow::open_dataset() and registers them on one DuckDB connection,
# so an exhibit pulls the columns and rows it needs and nothing else sits in memory.
#
# WHAT IS READ. The Dropbox release; Ann-Kristin's 101/103 outputs, converted under this document's own Cache and
# keyed on their size; and the pipeline stage outputs the classification and appendix exhibits need: the run
# folders of 03A-03D, 03B's crowned classification, 01A's index mirror and landing pages, and 04A's span stores.
# Nothing downstream of the release is read, except by the migration check of section 28.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation; lines at most 125 columns.

if (FALSE) {
  .inputs   <- unlist(.lP$Input)
  .min_date <- .lP$Params$ReleaseMin
  .dir      <- .lP$Output$DirPrepared
}


# 0. The release: readers, vocabulary, references, and the exhibit data carried over from the old 30 ------------------
# 30-Descriptives was the lab: every exhibit on every sample, reconciled against the two manuscripts.
# It was retired on 10 September 2026 once every decision it produced lived here, and this document
# took its number the next day. What this section
# holds is what this document still used from it, verbatim except for the prefix: the readers of the
# release and of her .dta files (fin_read_*, fin_dta_to_parquet), the vocabulary the figures agree
# on (class order, form families, filing groups, industries, regimes), the reference numbers the
# checks are made against, and the data functions behind seven figures and the sample ladder
# (fin_data_f*, fin_bins_f07, fin_table_ladder, fin_quarter_samples). The f-numbers are the
# revision's figure numbers and are kept as identifiers only; the exhibits themselves are named.

.fin_class_levels <- c(
  "Business Structure: Investment and Merger",
  "Business Structure: Peer Agreements",
  "Employment: Compensation",
  "Employment: Legal",
  "Financial Instruments: Credit",
  "Financial Instruments: Equity",
  "Leases",
  "Licenses",
  "Purchases and Sales: Assets",
  "Customer / Supplier",
  "R&D",
  "Other"
)

.fin_class_short <- c(
  "(1) M&A", "(2) Peer Agreements", "(3) Empl. - Compensation", "(4) Empl. - Legal", "(5) Credit",
  "(6) Equity", "(7) Leases", "(8) Licenses", "(9) Assets", "(10) Customer/Supplier", "(11) R&D",
  "(12) Other"
)

# Form families, amendment forms folded into their base, and the three filing groups the paper uses
# throughout: ad-hoc (8-K and the registration statements), yearly (10-K, 20-F), quarterly (10-Q).
.fin_form_levels <- c("8-K", "10-Q", "10-K", "S-1", "S-4", "F-1", "F-4", "20-F")

.fin_form_group <- c(
  "8-K" = "Ad-hoc", "S-1" = "Ad-hoc", "S-4" = "Ad-hoc", "F-1" = "Ad-hoc", "F-4" = "Ad-hoc",
  "10-K" = "Yearly", "20-F" = "Yearly",
  "10-Q" = "Quarterly"
)

.fin_form_foreign <- c("F-1", "F-4", "20-F")

# Fama-French 12, in the display order 101 assigns to ff_ind. FfInd in the quarter panel is an integer
# 1..12 in exactly this order.
.fin_ff12_levels <- c(
  "Consumer NonDurables", "Consumer Durables", "Manufacturing", "Energy", "Chemicals",
  "Business Equipment", "Telecommunication", "Utilities", "Wholesale & Retail", "Healthcare",
  "Finance", "Other"
)

plot_register_levels(
  .key     = "ClassPaper",
  .levels  = .fin_class_levels,
  .short   = .fin_class_short,
  .colours = NULL
)

plot_register_levels(
  .key     = "FormFamily",
  .levels  = .fin_form_levels,
  .short   = NULL,
  .colours = plot_pal_cat(length(.fin_form_levels))
)

plot_register_levels(
  .key     = "FilingGroup",
  .levels  = c("Ad-hoc", "Yearly", "Quarterly"),
  .short   = NULL,
  .colours = plot_pal_seq(3L)
)

plot_register_levels(
  .key     = "FilingGroup4",
  .levels  = c("8-K", "Registration", "Yearly", "Quarterly"),
  .short   = NULL,
  .colours = plot_pal_seq(4L)
)

# 05A's term families, in its order. The vocabulary is 05A's; this document only draws from its hits.
.fin_family_levels <- c("Pandemic", "Disruption", "RateReform", "Regulation", "Boilerplate")

plot_register_levels(
  .key     = "FfInd",
  .levels  = .fin_ff12_levels,
  .short   = NULL,
  .colours = NULL
)

# The named samples, in ladder order, with the one-line description each caption and table uses. The
# filters themselves are written in the runbook; this is only what they are called.
.fin_sample_labels <- c(
  S00_Edgar        = "every row of the release, the 2001-2024 window not yet applied",
  S0_Universe      = "every copy at ladder steps 2-6: the paper's full sample",
  S1_Unique        = "one copy per attachment, whatever its ladder step",
  S2_Descriptive   = "the descriptive sample: inside the window, well-formed, one copy per attachment",
  S2s_Seasoned     = "the descriptive sample from filers more than two years past their first contract",
  S3_Matched       = "the descriptive sample with a matched Compustat quarter",
  S3a_Matched2005  = "the matched sample from 2005, the delay tables' start year",
  S3b_Matched2008  = "the matched sample from 2008, the redaction tables' start year",
  S6_Redaction     = "the descriptive sample from 2008, when orders become observable",
  S7_Summaries     = "single-agreement Item 1.01 announcements on 8-Ks carrying at most one exhibit",
  S7_All           = "every Item 1.01 announcement 01D recovered, the restriction lifted",
  S5_Quarter       = "her firm-quarter panel inside the window",
  T4_Regression    = "quarter panel with the filing-decision controls present, ever-filers only",
  T4_Trimmed       = "the regression sample less firms whose filing never varies, overall or within a FAST period",
  S3_AK            = "one arbitrary contract per firm-year, as 104 draws Figure 7"
)

# The reference values the pipeline is checked against: the manuscript's Table 2 Panel A as printed
# in revision 1. A number here is a prediction, stated before the ladder is counted.
# THE NUMBERS THE LADDER IS CHECKED AGAINST are the resubmission's, on the release of 10 September
# 2026, after the copies without readable text were folded into the malformatted rung at the export.
# The revision printed 31,289 / 269,283 / 1,136,095 / 26,915; the column keeps its name because the
# checks read it.
.fin_reference <- tibble::tribble(
  ~Quantity,            ~Revision,
  "Full sample",        1436667L,
  "Malformatted",       32029L,
  "Multiple filer",     269278L,
  "Unique contracts",   1135360L,
  "Firms full sample",  43123L,
  "Firms unique",       26914L
)

# What is read from Contracts.parquet. Named here rather than at the call site because the file has
# 123 columns and the export's dictionary names each one; this list is the subset the descriptives
# need, and a column not in it is not loaded.
.fin_contract_cols <- c(
  # keys and filing facts
  "DocID", "HashDocument", "HashIndex", "CIK", "CompanyName", "FormType", "DateFiled", "DocDesc",
  # ladder and copies
  "SampleStepCode", "SampleStepDesc", "DescSample", "EstiSample", "PrimaryFiler", "MultFiler", "Removed",
  "RemClass",
  # Compustat keys
  "gvkey", "datadate", "cyear", "fyear", "fqtr",
  # text size
  "nWords", "nWordsAdj", "nNums",
  # classification
  "Class", "ClassBroad", "AmendType", "BertClassDetailedProb", "ClassDetailedFlag",
  # parties and places
  "nUniSpellingsNaive", "nUniRegistrant", "nUniCofiler", "nUniCounterparty", "HasNoParty",
  "nUniStateNaive", "nUniCountryNaive", "nUniStateCounterparty", "nUniCountryCounterparty",
  # dates
  "DateStart", "DateEnd", "DurationYears", "DurationSource", "DurationDropped", "NaiveYears",
  # orders and markers
  "HasCto", "nCtoOrders", "CtoIsExtension",
  "nRedactExplicit", "nRedactSymbol", "nRedactBlank", "nOmitExplicit", "nOmitSymbol", "nRedactBare",
  "nRedactMoney",
  # the parent 8-K
  "nItems", "nItemsVoluntary", "ReportsItem101", "SumWords", "SumAttached"
)

# The quarter panel: 103's Stata names on the left, the names this document uses on the right. Only
# these columns are converted; the .dta carries some three hundred and the rest are Compustat raw.
.fin_quarter_cols <- c(
  gvkey                 = "gvkey",
  fyear                 = "fyearq",
  fqtr                  = "fqtr",
  cik                   = "cik",
  datadate              = "datadate",
  cyear                 = "year_fe",
  Filed                 = "filing",
  nContractsQ           = "contract_per_quarter",
  nPeriodicQ            = "contracts_periodic",
  nCurrentQ             = "contracts_current",
  nContractsY           = "contract_per_year",
  FfInd                 = "ff_ind",
  FfIndAdj              = "ff_ind_adj",
  Sic4                  = "sic4",
  State                 = "state",
  AtqWin                = "atq_win",
  LogMve                = "lmve",
  OcfWin                = "oancfq_win",
  AcqNeg                = "acquisition_d_n",
  AcqPos                = "acquisition_d_p",
  LeverageWin           = "leverage_win",
  HhiSic3Win            = "hhi_win",
  HhiSic2Win            = "sic2_hhi_win",
  ChgSalesWin           = "change_sales_win",
  ChgSalesQ4Win         = "q4_change_sales_win",
  GoodwillImp           = "dummy_goodwill_imp",
  Loss                  = "loss",
  RoaWin                = "roa_win",
  EqIssue               = "eq_issue_dummy",
  DebtIssue             = "debt_issue_dummy",
  MbWin                 = "MB_win",
  Fluidity              = "prodmktfluid",
  PostFast              = "post_fast",
  LitRisk               = "LitRisk",
  nContractsTotal       = "total_contract"
)

# Ann-Kristin's own contract-level derivations, read back for reconciliation only: her sample flag,
# her class code, and her two redaction indicators. Everything else on that file is either in
# Contracts.parquet already or in the quarter panel.
.fin_ak_contract_cols <- c(
  DocID          = "DocID",
  AkKeep         = "Keep",
  AkClassCode    = "class_predicted",
  AkRedacted     = "redacted",
  AkRedactedText = "redacted_text",
  AkIsAmend      = "isAmend",
  AkYear         = "year"
)

#' Convert one of Ann-Kristin's .dta files to parquet, once
#'
#' THE .DTA IS THE SOURCE AND THE PARQUET IS A CACHE OF IT. The cache is used only while it is newer
#' than the .dta, through the same modification-time rule 10-ExportData applies to its own caches, so
#' a re-run of her 103 on Dropbox rebuilds it here without anyone remembering to.
#'
#' COLUMNS ARE SELECTED AT READ TIME, NOT AFTER. Her contract-level file is five gigabytes as .dta and
#' most of it is Contracts.parquet again; reading the whole thing to keep seven columns is the
#' difference between a minute and twenty. The requested names are checked against the header first,
#' so a column she has renamed is reported by name rather than as a haven error.
#'
#' LABELS ARE DROPPED. Stata value labels arrive as haven_labelled vectors, which arrow refuses to
#' write. Zapping them keeps the underlying codes, which is what 103 computes on.
#'
#' @param .path_dta Character. The .dta on her side.
#' @param .path_out Character. The parquet this writes.
#' @param .cols Named character vector or NULL. Names are what the column is called here, values what it
#'   is called in the .dta. NULL converts every column under its Stata name.
#' @param .rerun Logical. TRUE reconverts regardless of timestamps.
#' @return Invisibly, the parquet path.
fin_dta_to_parquet <- function(.path_dta, .path_out, .cols = NULL, .rerun = FALSE) {
  if (FALSE) {
    .path_dta <- .lP$Input$DtaQuarter
    .path_out <- .lP$Cache$CacheQuarter
    .cols     <- .fin_quarter_cols
    .rerun    <- FALSE
  }

  task_ <- fs::path_file(.path_dta)

  if (!fs::file_exists(.path_dta)) {
    cli::cli_abort("{(task_)}: not found at {.file {(.path_dta)}}. Is the Dropbox root in .lP right?")
  }

  if (exp_cache_hit(.task = task_, .path_out = .path_out, .paths_in = .path_dta, .rerun = .rerun)) {
    cli::cli_alert_info("{(task_)}: parquet cache is newer than the .dta and is used as it stands.")
    return(invisible(.path_out))
  }

  header_ <- names(haven::read_dta(file = .path_dta, n_max = 0L))

  if (is.null(.cols)) {
    select_ <- purrr::set_names(header_, header_)
  } else {
    missing_ <- .cols[!.cols %in% header_]
    if (length(missing_) > 0L) {
      cli::cli_abort(c(
        "{(task_)}: {length(missing_)} requested column{?s} not in the .dta.",
        "x" = paste(paste0(names(missing_), " <- ", missing_), collapse = ", ")
      ))
    }
    select_ <- .cols
  }

  cli::cli_alert_info("{(task_)}: reading {length(select_)} of {length(header_)} columns from the .dta.")
  t0_ <- Sys.time()

  out_ <- haven::read_dta(file = .path_dta, col_select = dplyr::all_of(unname(select_))) |>
    dplyr::mutate(dplyr::across(dplyr::where(haven::is.labelled), haven::zap_labels)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), \(.x) dplyr::na_if(.x, ""))) |>
    dplyr::rename(dplyr::all_of(select_))

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)

  cli::cli_alert_success(
    "{(task_)}: {format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns, \\
     {round(as.numeric(difftime(Sys.time(), t0_, units = 'mins')), 1)} min."
  )
  invisible(.path_out)
}

#' The contract table with the paper's derived columns and its definitional choices made once
#'
#' Reads the columns in .fin_contract_cols and adds what every exhibit needs: the calendar year, the
#' form family and filing group, the delayed indicator, the sample flags, the class as a factor in
#' the paper's order, and the four measures the specification leaves as switches -- redaction,
#' duration, words and parties. The switches are arguments so the runbook states them and the
#' Robustness section can flip them one at a time.
#'
#' MARKERS ARE MISSING, NOT ZERO, on four contracts in five. The six marker columns belong to the
#' Redactions block, whose marker is nRedactExplicit; where 04D found no marker at all the block is
#' absent and every count is NA. A mean over them without coalescing reports the marker rate among
#' contracts that have markers, which is 26 percent and means nothing. 103 handles this with
#' !missing(); here every marker count is coalesced to zero at read time, once.
#'
#' REDACTED CHANGES MEANING AT THE FAST ACT, and the two halves are kept apart as well as combined.
#' HasCto is the SEC granting confidential treatment, observable from 2008 and gone after 2019;
#' nRedact is markers in the text, observable throughout. Redacted is the paper's variable: the order
#' up to 2018, the markers from 2019. RedactedCto and RedactedText are the two identifications on their
#' own, which is what Figure 10 plots.
#'
#' @param .path Character. Contracts.parquet.
#' @param .redaction Character. Which marker kinds count: "symbolexplicit" (103's nRedact: Symbol +
#'   Explicit), "bracketed" (the five bracketed kinds, 20's default), "withheld" (the four Redact
#'   kinds), "money" (markers matched to a withheld price).
#' @param .redact_min Integer. Markers needed for a contract to count as redacted by text. 103 uses 1.
#' @param .duration Character. "cascade" for DurationYears (revision Table 3), "naive" for NaiveYears.
#' @param .words Character. "adjusted" for nWordsAdj (the 8,825 in the text), "raw" for nWords.
#' @param .parties Character. "recital" for registrant + co-filers + counterparties, "counterparty"
#'   for nUniCounterparty alone, "spellings" for the naive rung.
#' @return Tibble, one row per registrant copy, every ladder step.
fin_read_contracts <- function(.path, .redaction = "symbolexplicit", .redact_min = 1L, .duration = "cascade",
                               .words = "adjusted", .parties = "recital") {
  if (FALSE) {
    .path       <- .lP$Input$FilContracts
    .redaction  <- "symbolexplicit"
    .redact_min <- 1L
    .duration   <- "cascade"
    .words      <- "adjusted"
    .parties    <- "recital"
  }
  .redaction <- match.arg(.redaction, c("symbolexplicit", "bracketed", "withheld", "money"))
  .duration  <- match.arg(.duration,  c("cascade", "naive"))
  .words     <- match.arg(.words,     c("adjusted", "raw"))
  .parties   <- match.arg(.parties,   c("recital", "counterparty", "spellings"))

  cli::cli_alert_info("Reading {length(.fin_contract_cols)} columns from {.file {fs::path_file(.path)}}")

  have_ <- names(arrow::open_dataset(.path))
  miss_ <- setdiff(.fin_contract_cols, have_)
  if (length(miss_) > 0L) {
    cli::cli_abort("Contracts.parquet lacks {length(miss_)} expected column{?s}: {miss_}.")
  }

  tab_ <- arrow::open_dataset(.path) |>
    dplyr::select(dplyr::all_of(.fin_contract_cols)) |>
    dplyr::collect()

  marker_ <- c("nRedactExplicit", "nRedactSymbol", "nRedactBlank", "nOmitExplicit", "nOmitSymbol",
               "nRedactBare", "nRedactMoney")

  out_ <- tab_ |>
    dplyr::mutate(dplyr::across(dplyr::all_of(marker_), \(.x) dplyr::coalesce(.x, 0L))) |>
    dplyr::mutate(
      Year        = as.integer(lubridate::year(.data$DateFiled)),
      FormFamily  = stringi::stri_replace_first_regex(.data$FormType, "/A$", ""),
      IsAmendForm = as.integer(stringi::stri_detect_regex(.data$FormType, "/A$")),
      FilingGroup = unname(.fin_form_group[.data$FormFamily]),
      IsForeign   = as.integer(.data$FormFamily %in% .fin_form_foreign),
      Delayed     = as.integer(.data$FormFamily %in% c("10-K", "10-Q", "20-F")),
      Is8K        = as.integer(.data$FormFamily == "8-K"),
      IsRegStmt   = as.integer(.data$FormFamily %in% c("S-1", "S-4", "F-1", "F-4")),
      Keep        = .data$DescSample == 1L & .data$PrimaryFiler == 1L,
      Matched     = !is.na(.data$gvkey),
      ClassCode   = match(.data$Class, .fin_class_levels),
      ClassPaper  = factor(.data$Class, levels = .fin_class_levels),
      IsAmend     = as.integer(.data$AmendType == "Amended"),
      PostFast    = as.integer(.data$DateFiled > as.Date("2019-04-02")),
      nRedact     = switch(.redaction,
        symbolexplicit = .data$nRedactSymbol + .data$nRedactExplicit,
        bracketed      = .data$nRedactSymbol + .data$nRedactExplicit + .data$nRedactBlank +
          .data$nOmitExplicit + .data$nOmitSymbol,
        withheld       = .data$nRedactSymbol + .data$nRedactExplicit + .data$nRedactBlank + .data$nRedactBare,
        money          = .data$nRedactMoney
      ),
      HasMarkers  = as.integer(.data$nRedact >= .redact_min),
      RedactedCto = dplyr::if_else(.data$Year >= 2008L & .data$Year <= 2018L, .data$HasCto, NA_integer_),
      Redacted    = dplyr::case_when(
        .data$Year < 2008L  ~ NA_integer_,
        .data$Year <= 2018L ~ .data$HasCto,
        .default            = as.integer(.data$HasCto == 1L | .data$HasMarkers == 1L)
      ),
      RedactedText = dplyr::if_else(.data$Year >= 2008L,
                                    as.integer(.data$HasCto == 1L | .data$HasMarkers == 1L), NA_integer_),
      DurationYrs = if (.duration == "cascade") .data$DurationYears else .data$NaiveYears,
      Words       = if (.words == "adjusted") .data$nWordsAdj else .data$nWords,
      Parties     = switch(.parties,
        recital      = .data$nUniRegistrant + .data$nUniCofiler + .data$nUniCounterparty,
        counterparty = .data$nUniCounterparty,
        spellings    = .data$nUniSpellingsNaive
      ),
      Countries   = .data$nUniCountryNaive
    ) |>
    # THE FILER'S FIRST CONTRACT, for the seasoned-filer sample: 104_SPAC.do takes the earliest filing
    # date per CIK over its Keep rows and calls a contract seasoned when it comes more than two years
    # later. Computed over the same rows here. A firm already filing in 2001 is left-censored.
    dplyr::mutate(
      EntryDate  = suppressWarnings(min(.data$DateFiled[.data$Keep], na.rm = TRUE)),
      .by = "CIK"
    ) |>
    dplyr::mutate(
      EntryDate  = dplyr::if_else(is.infinite(as.numeric(.data$EntryDate)), as.Date(NA), .data$EntryDate),
      IsSeasoned = as.integer(!is.na(.data$EntryDate) &
                                as.numeric(.data$DateFiled - .data$EntryDate) / 365.25 > 2)
    )

  unknown_ <- setdiff(unique(stats::na.omit(out_$FormFamily)), .fin_form_levels)
  if (length(unknown_) > 0L) cli::cli_abort("Unregistered form famil{?y/ies}: {unknown_}.")

  cli::cli_alert_success(
    "{format(nrow(out_), big.mark = ',')} rows; {format(sum(out_$Keep), big.mark = ',')} descriptive-sample \\
     primary copies; {sum(is.na(out_$ClassCode) & out_$Keep)} of them unlabelled."
  )
  out_
}

#' The Item 1.01 announcements, attached or not
#'
#' 10's second release: one row per 8-K whose Item 1.01 narrative was recovered. Carries no ladder;
#' the paper's restriction to single-agreement announcements is SumIsSingle, and the restriction to
#' 8-Ks carrying at most one Exhibit 10 needs the contract table, which is why the count is joined here.
#'
#' @param .path Character. Summaries.parquet.
#' @param .tab_contracts Tibble from `fin_read_contracts()`, for the exhibits per filing.
#' @return Tibble, one row per announcement, with nExhibits and Year added.
fin_read_summaries <- function(.path, .tab_contracts) {
  if (FALSE) {
    .path          <- .lP$Input$FilSummaries
    .tab_contracts <- tab_contracts
  }

  ex_ <- .tab_contracts |>
    dplyr::filter(.data$DescSample == 1L) |>
    dplyr::count(.data$HashIndex, name = "nExhibits")

  out_ <- arrow::read_parquet(.path) |>
    dplyr::left_join(ex_, by = dplyr::join_by(HashIndex)) |>
    dplyr::mutate(
      nExhibits = dplyr::coalesce(.data$nExhibits, 0L),
      Year      = as.integer(lubridate::year(.data$DateFiled)),
      OnTime    = as.integer(.data$SumLagDays <= 6L)   # four business days is at most six calendar days
    )

  cli::cli_alert_success(
    "{format(nrow(out_), big.mark = ',')} announcements, {format(sum(out_$SumAttached == 1L), big.mark = ',')} \\
     with an exhibit attached"
  )
  out_
}

#' The firm-quarter panel as 103 built it
#'
#' One row per gvkey-quarter for every firm that filed at least one contract, non-filing quarters
#' included, with the winsorised variables the regressions use. Read from the parquet conversion,
#' under the names in .fin_quarter_cols. Nothing is recomputed: Table 4 has to show the same clipped
#' values her regressions control for.
#'
#' @param .path Character. The converted parquet.
#' @return Tibble, one row per gvkey x fyear x fqtr.
fin_read_quarter <- function(.path) {
  if (FALSE) .path <- .lP$Cache$CacheQuarter

  out_ <- arrow::read_parquet(.path) |>
    dplyr::mutate(
      gvkey    = stringi::stri_pad_left(as.character(.data$gvkey), width = 6L, pad = "0"),
      cik      = stringi::stri_pad_left(as.character(.data$cik), width = 10L, pad = "0"),
      fyear    = as.integer(.data$fyear),
      fqtr     = as.integer(.data$fqtr),
      Filed    = as.integer(dplyr::coalesce(.data$Filed, 0)),
      # 103 sums the counts on the contract rows before merging in the non-filing quarters, so the
      # split counts are missing where the total is zero. A non-filing quarter filed nothing by either
      # channel; zero is the measurement.
      dplyr::across(c("nContractsQ", "nPeriodicQ", "nCurrentQ", "nContractsY"), \(.x) dplyr::coalesce(.x, 0)),
      # 101 codes the industries 1..12 in .fin_ff12_levels' order; anything else, including the 0 it
      # assigns before the ranges, becomes NA rather than a thirteenth industry.
      FfIndLab = factor(as.integer(.data$FfInd), levels = seq_along(.fin_ff12_levels), labels = .fin_ff12_levels),
      YQ       = .data$fyear + (.data$fqtr - 1L) / 4
    )

  dup_ <- out_ |> dplyr::count(.data$gvkey, .data$fyear, .data$fqtr) |> dplyr::filter(.data$n > 1L)
  if (nrow(dup_) > 0L) cli::cli_abort("{nrow(dup_)} duplicated firm-quarter{?s} in the quarter panel.")

  cli::cli_alert_success(
    "{format(nrow(out_), big.mark = ',')} firm-quarters, {format(dplyr::n_distinct(out_$gvkey), big.mark = ',')} \\
     firms, {format(sum(out_$Filed), big.mark = ',')} filing quarters"
  )
  out_
}

#' Segment-sales concentration and diversity, one row per gvkey-year, as 101 computed it
#'
#' @param .path Character. The converted parquet of concentration.dta.
#' @return Tibble: gvkey, fyear, and whatever 101 named the measures, padded and typed.
fin_read_concentration <- function(.path) {
  if (FALSE) .path <- .lP$Cache$CacheConcentration

  out_ <- arrow::read_parquet(.path) |>
    dplyr::mutate(
      gvkey = stringi::stri_pad_left(as.character(.data$gvkey), width = 6L, pad = "0"),
      fyear = as.integer(.data$fyear)
    ) |>
    dplyr::distinct(.data$gvkey, .data$fyear, .keep_all = TRUE)

  cli::cli_alert_success("{format(nrow(out_), big.mark = ',')} firm-years of segment concentration")
  out_
}

#' Ann-Kristin's contract-level derivations, for reconciliation
#'
#' Seven columns off her five-gigabyte file: her sample flag, class code, amendment flag and the two
#' redaction indicators. Joined to the contract table on DocID so that every difference between her
#' Keep and PrimaryFiler, and between her redacted and this file's Redacted, can be counted rather
#' than argued about.
#'
#' @param .path Character. The converted parquet.
#' @return Tibble keyed on DocID.
fin_read_ak_contract <- function(.path) {
  if (FALSE) .path <- .lP$Cache$CacheAkContract

  out_ <- arrow::read_parquet(.path) |>
    dplyr::mutate(
      AkKeep  = as.integer(dplyr::coalesce(.data$AkKeep, 0)),
      AkYear  = as.integer(.data$AkYear)
    ) |>
    dplyr::distinct(.data$DocID, .keep_all = TRUE)

  cli::cli_alert_success("{format(nrow(out_), big.mark = ',')} rows of her contract-level flags")
  out_
}

#' The Places release: every place a contract names, with the party it belongs to
#'
#' One row per contract, place, party role and law-clause flag, at primary-copy grain. Read whole:
#' nine and a half million rows of short strings, which is what Figure 6 draws from.
#'
#' @param .path Character. Places.parquet.
#' @return Tibble as released, GeoLevel and PartyRole as character.
fin_read_places <- function(.path) {
  if (FALSE) .path <- .lP$Input$FilPlaces
  out_ <- arrow::read_parquet(.path) |>
    dplyr::mutate(InLawClause = as.integer(.data$InLawClause), nMentions = as.integer(.data$nMentions))
  cli::cli_alert_success(
    "{format(nrow(out_), big.mark = ',')} place rows on {format(dplyr::n_distinct(out_$DocID), big.mark = ',')} \\
     contracts; {format(sum(out_$InLawClause == 1L), big.mark = ',')} inside a law clause"
  )
  out_
}

#' The TermDocs release: 05A's hits per contract and family, cut to the release
#'
#' @param .path Character. TermDocs.parquet.
#' @return Tibble: DocID, Family (character, in .fin_family_levels), nHits, nTerms, HasTerm, FirstPos,
#'   PerKWords.
fin_read_term_docs <- function(.path) {
  if (FALSE) .path <- .lP$Input$FilTermDocs
  out_ <- arrow::read_parquet(.path) |>
    dplyr::select(-dplyr::any_of("HashDocument")) |>
    dplyr::mutate(Family = as.character(.data$Family), HasTerm = as.integer(.data$HasTerm), nHits = as.integer(.data$nHits))
  unknown_ <- setdiff(unique(out_$Family), .fin_family_levels)
  if (length(unknown_) > 0L) cli::cli_abort("TermDocs carries {?a family/families} this document does not know: {unknown_}.")
  cli::cli_alert_success(
    "{format(dplyr::n_distinct(out_$DocID), big.mark = ',')} scanned contracts, {dplyr::n_distinct(out_$Family)} \\
     families; {format(sum(out_$HasTerm[out_$Family == 'Pandemic']), big.mark = ',')} name a pandemic term"
  )
  out_
}

#' The CtoOrders release: every order reference 01E parsed, linked or not
#'
#' @param .path Character. CtoOrders.parquet.
#' @return Tibble as released, plus OrderYear and Linked.
fin_read_cto_orders <- function(.path) {
  if (FALSE) .path <- .lP$Input$FilCtoOrders
  out_ <- arrow::read_parquet(.path) |>
    dplyr::mutate(
      OrderDate   = as.Date(.data$OrderDate),
      OrderYear   = as.integer(lubridate::year(.data$OrderDate)),
      IsExtension = as.integer(.data$IsExtension),
      Linked      = !is.na(.data$DocID)
    )
  cli::cli_alert_success(
    "{format(dplyr::n_distinct(out_$OrderDocID), big.mark = ',')} orders, {format(nrow(out_), big.mark = ',')} \\
     references, {format(sum(out_$Linked), big.mark = ',')} linked to a contract"
  )
  out_
}

#' The sample ladder as Table 2 Panel A prints it
#'
#' Counted from the contract table's own ladder columns. Full sample is every row at steps 2-6 --
#' step 1, outside the window, is the one rung the paper never mentions -- malformed is step 2,
#' multiple filer is every non-primary copy on the descriptive rungs, and unique contracts is what
#' remains. Firms are distinct CIKs at each rung; the co-filer loss is CIKs that appear only on
#' non-primary copies.
#'
#' @param .tab Tibble from `fin_read_contracts()`.
#' @return Tibble: Quantity, Contracts, Firms, in the table's row order.
fin_table_ladder <- function(.tab) {
  if (FALSE) .tab <- tab_contracts

  win_  <- dplyr::filter(.tab, .data$SampleStepCode >= 2L)
  desc_ <- dplyr::filter(win_, .data$SampleStepCode >= 3L)
  uniq_ <- dplyr::filter(desc_, .data$PrimaryFiler == 1L)

  tibble::tibble(
    Quantity  = c("Full sample", "Malformatted", "Multiple filer", "Unique contracts"),
    Contracts = c(nrow(win_), -sum(win_$SampleStepCode == 2L), -sum(desc_$PrimaryFiler == 0L), nrow(uniq_)),
    Firms     = c(
      dplyr::n_distinct(win_$CIK),
      -(dplyr::n_distinct(win_$CIK) - dplyr::n_distinct(desc_$CIK)),
      -(dplyr::n_distinct(desc_$CIK) - dplyr::n_distinct(uniq_$CIK)),
      dplyr::n_distinct(uniq_$CIK)
    )
  )
}

#' The ladder against the manuscript's numbers
#'
#' Stated before it is read: every quantity in .fin_reference reproduces exactly, or the release on
#' Dropbox is not the one the revision's Table 2 was built from. A miss is not rounded away; it aborts.
#'
#' @param .tab_ladder Tibble from `fin_table_ladder()`.
#' @param .ref Tibble. The reference values.
#' @return Invisibly, the comparison table.
fin_check_reference <- function(.tab_ladder, .ref = .fin_reference) {
  if (FALSE) {
    .tab_ladder <- tab_ladder
    .ref        <- .fin_reference
  }

  got_ <- tibble::tibble(
    Quantity = c(.tab_ladder$Quantity, "Firms full sample", "Firms unique"),
    Pipeline = c(abs(.tab_ladder$Contracts), .tab_ladder$Firms[1L], .tab_ladder$Firms[4L])
  )

  cmp_ <- dplyr::inner_join(.ref, got_, by = dplyr::join_by(Quantity)) |>
    dplyr::mutate(Diff = .data$Pipeline - .data$Revision)

  tbl_out(.tab = cmp_, .title = "Table 2 Panel A: revision 1 against this release")

  if (any(cmp_$Diff != 0L)) {
    cli::cli_abort("Table 2 Panel A does not reproduce: {sum(cmp_$Diff != 0L)} quantit{?y/ies} differ.")
  }
  cli::cli_alert_success("every Table 2 Panel A quantity reproduces exactly")
  invisible(cmp_)
}

#' Pre-format a tibble's numeric columns for the console
#'
#' tbl_out() prints a non-integer numeric with three decimals whatever is asked of it, by design: the
#' caller who wants something else formats to character first. This is that formatting, once: every
#' numeric column to the decimals given, counts to none, missing to nothing.
#'
#' @param .tab Tibble.
#' @param .digits Integer. Decimals for numeric columns.
#' @param .counts Character. Columns printed as whole numbers whatever .digits says.
#' @return The tibble with those columns as character.
fin_fmt <- function(.tab, .digits = 2L, .counts = c("N", "Rows", "Contracts", "Firms", "Year", "Revision")) {
  if (FALSE) {
    .tab    <- tibble::tibble(Year = 2001L, Share = 0.12345, N = 12L)
    .digits <- 2L
    .counts <- "N"
  }
  .tab |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      \(.x) {
        d_ <- if (dplyr::cur_column() %in% .counts && all(.x == round(.x), na.rm = TRUE)) 0L else .digits
        out_ <- if (dplyr::cur_column() == "Year") as.character(.x) else tbl_num(.x, .digits = d_)
        dplyr::if_else(is.na(.x), NA_character_, out_)
      }
    ))
}

#' One exhibit's slice for one sample
#'
#' @param .tab_data The exhibit tibble.
#' @param .sample Character. The Sample value wanted.
#' @return The rows carrying it; aborts if there are none, because a missing tab should not be blank.
fin_slice <- function(.tab_data, .sample) {
  if (FALSE) {
    .tab_data <- tab_f01
    .sample   <- "S2_Descriptive"
  }
  out_ <- dplyr::filter(.tab_data, .data$Sample == .sample)
  if (nrow(out_) == 0L) cli::cli_abort("No rows for sample {.val {(.sample)}}; known: {unique(.tab_data$Sample)}.")
  out_
}

# What revision 1 prints above each bar. A prediction for S2; the other samples have no reference.
.fin_reference_f01 <- tibble::tribble(
  ~FormFamily, ~Revision,
  "8-K",  38.5,
  "10-Q", 22.1,
  "10-K", 21.1,
  "S-1",  13.1,
  "S-4",   2.3,
  "F-1",   2.2,
  "F-4",   0.4,
  "20-F",  0.3
)

#' Contracts by form family and amendment status, with the form's share of the sample
#'
#' Counts, not means, so the tibble carries everything the figure, its labels and the report need.
#' An unlabelled contract -- 03F could not classify it, or it is malformed and never reached 03F --
#' keeps its bar and is a third segment, so that a sample which includes the malformed rows shows
#' them rather than silently shrinking.
#'
#' @param .tab One of the named samples: a contract tibble from `fin_read_contracts()`.
#' @param .sample Character. The sample's name, carried on every row.
#' @return Tibble: Sample, Region, FormFamily, Amend, N, NForm, Share (of the sample), ShareAmend
#'   (of the form).
fin_data_f01 <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample <- "S2_Descriptive"
  }

  .tab |>
    dplyr::mutate(
      Region = dplyr::if_else(.data$IsForeign == 1L, "Foreign filings", "US filings"),
      Amend  = dplyr::coalesce(.data$AmendType, "Unlabelled")
    ) |>
    dplyr::count(.data$Region, .data$FormFamily, .data$Amend, name = "N") |>
    dplyr::mutate(NForm = sum(.data$N), .by = "FormFamily") |>
    dplyr::mutate(
      Sample     = .sample,
      Share      = .data$NForm / sum(.data$N),
      ShareAmend = .data$N / .data$NForm
    ) |>
    dplyr::arrange(match(.data$FormFamily, .fin_form_levels), .data$Amend)
}

#' Figure 1 in numbers: each form's share of each sample, beside the revision's label
#'
#' @param .tab_data Tibble from `fin_data_f01()`, every sample.
#' @param .ref Tibble. The revision's labels for S2.
#' @return Invisibly, the wide table printed.
fin_report_f01 <- function(.tab_data, .ref = .fin_reference_f01) {
  if (FALSE) {
    .tab_data <- tab_f01
    .ref      <- .fin_reference_f01
  }

  wide_ <- .tab_data |>
    dplyr::distinct(.data$Sample, .data$FormFamily, .data$Share) |>
    dplyr::mutate(Share = round(100 * .data$Share, 1)) |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Share") |>
    dplyr::left_join(.ref, by = dplyr::join_by(FormFamily)) |>
    dplyr::arrange(match(.data$FormFamily, .fin_form_levels))

  tbl_out(
    .tab = fin_fmt(wide_, 1L),
    .title  = "Figure 1: share of contracts by form, percent, one column per sample",
    .notes = c(Revision = "The label printed above the bar in revision 1, which was drawn on S2.")
  )

  amend_ <- .tab_data |>
    dplyr::filter(.data$Amend == "Amended") |>
    dplyr::distinct(.data$Sample, .data$FormFamily, .data$ShareAmend) |>
    dplyr::mutate(ShareAmend = round(100 * .data$ShareAmend, 1)) |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "ShareAmend") |>
    dplyr::arrange(match(.data$FormFamily, .fin_form_levels))

  tbl_out(
    .tab = fin_fmt(amend_, 1L),
    .title  = "Figure 1: amendments as a share of each form, percent",
    .notes = c(FormFamily = "Read S-1 here: the registration statement is where amendments cluster, and
                             the text's 'about 20% amendments' is a corpus average of very different forms.")
  )

  if ("S2_Descriptive" %in% names(wide_)) {
    off_ <- abs(wide_$S2_Descriptive - wide_$Revision)
    if (all(off_ <= 0.05, na.rm = TRUE)) {
      cli::cli_alert_success("every S2 share matches the revision's label to the printed decimal")
    } else {
      cli::cli_alert_warning("{sum(off_ > 0.05, na.rm = TRUE)} S2 share{?s} differ from the revision's labels")
    }
  }
  invisible(wide_)
}

.fin_regime_years <- c(Reform2004 = 2004 + 235 / 366, Fast2019 = 2019 + 92 / 365)

#' Lines over years, one per group, optionally faceted, with the regime years marked
#'
#' The shape behind Figures 5, 10 and F1 and the line versions of Figure 4. Colour comes from the
#' registry where a key is given; otherwise from the categorical palette, which caps at eight, so a
#' twelve-line figure must facet rather than colour.
#'
#' @param .tab Tibble, one row per x per group (per facet).
#' @param .x Character. The year column.
#' @param .y Character. The value column.
#' @param .group Character or NULL. Column giving one line per level; NULL draws one line.
#' @param .facet Character or NULL. Column to facet on.
#' @param .key Character or NULL. Registration key ordering and colouring .group.
#' @param .key_facet Character or NULL. Registration key ordering .facet, with short labels.
#' @param .pct Logical. Percent axis.
#' @param .ncol Integer. Facet columns.
#' @param .free_y Logical. Free y scales across facets.
#' @param .regimes Logical. Dashed verticals at the 2004 reform and the FAST Act.
#' @param .ylab Character or NULL. Y-axis title.
#' @param .points Logical. Mark each observation; FALSE for series dense enough that markers smear.
#' @return A ggplot.
fin_shape_lines <- function(.tab, .x, .y, .group = NULL, .facet = NULL, .key = NULL, .key_facet = NULL,
                            .pct = FALSE, .ncol = 4L, .free_y = FALSE, .regimes = TRUE, .ylab = NULL, .points = TRUE,
                            .step = NULL) {
  if (FALSE) {
    .tab       <- tab_f10 |> dplyr::filter(.data$Sample == "S2_Descriptive")
    .x         <- "Year"
    .y         <- "Share"
    .group     <- "Series"
    .facet     <- NULL
    .key       <- "RedactSeries"
    .key_facet <- NULL
    .pct       <- TRUE
    .ncol      <- 4L
    .free_y    <- FALSE
    .regimes   <- TRUE
    .ylab      <- "Share of contracts"
  }

  dat_ <- .tab |>
    dplyr::mutate(
      PlotX = as.numeric(.data[[.x]]),
      PlotY = as.numeric(.data[[.y]]),
      PlotG = if (is.null(.group)) factor("all") else
        if (is.null(.key)) factor(.data[[.group]]) else plot_factor(.data[[.group]], .key = .key, .short = TRUE),
      PlotF = if (is.null(.facet)) factor("all") else
        if (is.null(.key_facet)) factor(.data[[.facet]]) else
          plot_factor(.data[[.facet]], .key = .key_facet, .short = TRUE)
    )

  p_ <- ggplot2::ggplot(dat_, ggplot2::aes(x = .data$PlotX, y = .data$PlotY, colour = .data$PlotG, group = .data$PlotG)) +
    ggplot2::geom_line(linewidth = 0.6)
  if (.points) p_ <- p_ + ggplot2::geom_point(size = 1.1)

  # A regime line outside the data's years would stretch the axis to it; only those inside are drawn.
  regimes_ <- .fin_regime_years[.fin_regime_years >= min(dat_$PlotX) - 0.5 & .fin_regime_years <= max(dat_$PlotX) + 0.5]
  if (.regimes && length(regimes_) > 0L) {
    p_ <- p_ + ggplot2::geom_vline(
      xintercept = unname(regimes_), linetype = "dashed", colour = .plot_ref, linewidth = .plot_line * 2
    )
  }

  if (!is.null(.facet)) {
    p_ <- p_ + ggplot2::facet_wrap(ggplot2::vars(.data$PlotF), ncol = .ncol, scales = if (.free_y) "free_y" else "fixed")
  }
  # Panels three or more abreast have room for a label every eight years, not every four.
  step_ <- if (!is.null(.step)) .step else if (!is.null(.facet) && .ncol >= 3L) 8 else 4

  colour_ <- if (is.null(.group)) {
    ggplot2::scale_colour_manual(values = c(all = .plot_ink), guide = "none")
  } else if (!is.null(.key) && !is.null(plot_entry(.key)$Colours)) {
    plot_scale_colour_key(.key = .key, .short = TRUE, name = NULL)
  } else {
    plot_scale_colour_cat(name = NULL)
  }

  p_ +
    colour_ +
    (if (.pct) plot_scale_y_pct(.accuracy = 1, .expand = c(0.02, 0.05)) else
       ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0.02, 0.05)))) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(step_)) +
    ggplot2::labs(x = NULL, y = .ylab) +
    plot_theme(.grid = "y", .legend = if (is.null(.group)) "none" else "bottom")
}

#' Shares stacked to one per year, with an optional count panel above and an optional line over
#'
#' The shape behind Figures 2 and 4. The paper draws the count as a line on a second axis over the
#' bars; two scales on one panel are arbitrary relative to each other, so the count gets its own
#' short panel above and shares the year axis. A line drawn over the bars must be a share on the
#' same 0-1 scale, which is what the amendment share is.
#'
#' @param .tab Tibble, one row per year per fill level, with the level's count.
#' @param .year Character. The year column.
#' @param .n Character. The count column; shares are computed within year.
#' @param .fill Character. The segment column.
#' @param .key Character or NULL. Registration key ordering .fill; colours come from the sequential
#'   ramp, darkest first, because up to twelve segments exceed the categorical palette.
#' @param .line Tibble or NULL. Year and a share column, drawn as a black line over the bars.
#' @param .line_y Character. The share column in .line.
#' @param .line_name Character. Legend label for the line.
#' @param .count Logical. Draw the count panel above.
#' @param .regimes Logical. Dashed verticals at the regime years.
#' @return A patchwork of two ggplots, or one ggplot when .count is FALSE.
fin_shape_share_stack <- function(.tab, .year, .n, .fill, .key = NULL, .line = NULL, .line_y = "Share",
                                  .line_name = "Share", .count = TRUE, .regimes = TRUE) {
  if (FALSE) {
    .tab       <- tab_f02 |> dplyr::filter(.data$Sample == "S2_Descriptive")
    .year      <- "Year"
    .n         <- "N"
    .fill      <- "FilingGroup"
    .key       <- "FilingGroup"
    .line      <- NULL
    .line_y    <- "Share"
    .line_name <- "Share"
    .count     <- TRUE
    .regimes   <- TRUE
  }

  dat_ <- .tab |>
    dplyr::mutate(
      PlotYear = as.numeric(.data[[.year]]),
      PlotN    = as.numeric(.data[[.n]]),
      PlotFill = if (is.null(.key)) factor(.data[[.fill]]) else plot_factor(.data[[.fill]], .key = .key, .short = TRUE)
    ) |>
    dplyr::mutate(PlotShare = .data$PlotN / sum(.data$PlotN), .by = "PlotYear")

  # Up to six segments take the ramp in order. Past that, adjacent segments on one ramp are too close
  # to tell apart, so the ramp is dealt out interleaved -- light, dark, light, dark -- which keeps a
  # monotone legend impossible but every boundary visible. The legend still lists the levels in order.
  n_fill_ <- nlevels(dat_$PlotFill)
  ramp_   <- plot_pal_seq(n_fill_)
  if (n_fill_ > 6L) {
    half_ <- ceiling(n_fill_ / 2)
    idx_  <- as.vector(rbind(seq_len(half_), half_ + seq_len(half_)))[seq_len(n_fill_)]
    ramp_ <- ramp_[idx_]
  }
  fills_ <- purrr::set_names(ramp_, levels(dat_$PlotFill))

  vline_ <- if (.regimes) {
    ggplot2::geom_vline(xintercept = unname(.fin_regime_years), linetype = "dashed", colour = "black",
                        linewidth = .plot_line * 2)
  } else {
    NULL
  }

  bars_ <- ggplot2::ggplot(dat_, ggplot2::aes(x = .data$PlotYear, y = .data$PlotShare, fill = .data$PlotFill)) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE), width = 0.85, colour = "white",
                      linewidth = .plot_line) +
    vline_ +
    ggplot2::scale_fill_manual(values = fills_, name = NULL) +
    plot_scale_y_pct(.accuracy = 1, .expand = c(0, 0)) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2), expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::labs(x = NULL, y = "Share of contracts") +
    plot_theme(.grid = "none", .legend = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = if (n_fill_ > 6L) 3L else 1L, byrow = TRUE))

  if (!is.null(.line)) {
    line_ <- .line |> dplyr::mutate(PlotYear = as.numeric(.data[[.year]]), PlotLine = as.numeric(.data[[.line_y]]))
    bars_ <- bars_ +
      ggplot2::geom_line(data = line_, mapping = ggplot2::aes(x = .data$PlotYear, y = .data$PlotLine, linetype = .line_name),
                         inherit.aes = FALSE, colour = "black", linewidth = 0.7) +
      ggplot2::scale_linetype_manual(values = purrr::set_names("solid", .line_name), name = NULL)
  }

  if (!.count) return(bars_)

  tot_ <- dat_ |> dplyr::summarise(Total = sum(.data$PlotN), .by = "PlotYear")
  count_ <- ggplot2::ggplot(tot_, ggplot2::aes(x = .data$PlotYear, y = .data$Total / 1000)) +
    ggplot2::geom_line(colour = .plot_ink, linewidth = 0.7) +
    ggplot2::geom_point(colour = .plot_ink, size = 1.1) +
    vline_ +
    ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0.05, 0.1)),
                                limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2), expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::labs(x = NULL, y = "Contracts (000)") +
    plot_theme(.grid = "y", .legend = "none") +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())

  patchwork::wrap_plots(count_, bars_, ncol = 1L, heights = c(1, 3))
}

#' Contracts per year and form family
#'
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Year, FormFamily, FilingGroup, FilingGroup4, N.
fin_data_f02 <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample <- "S2_Descriptive"
  }
  .tab |>
    dplyr::count(.data$Year, .data$FormFamily, name = "N") |>
    dplyr::mutate(
      Sample       = .sample,
      FilingGroup  = unname(.fin_form_group[.data$FormFamily]),
      FilingGroup4 = dplyr::case_when(
        .data$FormFamily == "8-K"                              ~ "8-K",
        .data$FormFamily %in% c("S-1", "S-4", "F-1", "F-4")    ~ "Registration",
        .default                                               = .data$FilingGroup
      )
    ) |>
    dplyr::arrange(.data$Year, match(.data$FormFamily, .fin_form_levels))
}

#' Figure 2, three groups as the paper draws it
#' @param .tab_data One sample's rows from `fin_data_f02()`.
#' @return A patchwork.
fin_plot_f02 <- function(.tab_data) {
  if (FALSE) .tab_data <- fin_slice(tab_f02, "S2_Descriptive")
  .tab_data |>
    dplyr::summarise(N = sum(.data$N), .by = c("Year", "FilingGroup")) |>
    fin_shape_share_stack(.year = "Year", .n = "N", .fill = "FilingGroup", .key = "FilingGroup", .count = TRUE)
}

#' Figure 2 in numbers: yearly totals and the four group shares, per sample
#' @param .tab_data Every sample from `fin_data_f02()`.
#' @return Invisibly, the long table.
fin_report_f02 <- function(.tab_data) {
  if (FALSE) .tab_data <- tab_f02

  long_ <- .tab_data |>
    dplyr::summarise(N = sum(.data$N), .by = c("Sample", "Year", "FilingGroup4")) |>
    dplyr::mutate(Total = sum(.data$N), Share = .data$N / sum(.data$N), .by = c("Sample", "Year"))

  tot_ <- long_ |>
    dplyr::distinct(.data$Sample, .data$Year, .data$Total) |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Total")
  tbl_out(.tab = fin_fmt(tot_, 0L), .title = "Figure 2: contracts per year, one column per sample",
          .notes = c(Year = "2021 is the year the text calls 80 thousand; read it here."))

  s2_ <- long_ |>
    dplyr::filter(.data$Sample == "S2_Descriptive") |>
    dplyr::mutate(Share = round(100 * .data$Share, 1)) |>
    dplyr::select("Year", "FilingGroup4", "Share") |>
    tidyr::pivot_wider(names_from = "FilingGroup4", values_from = "Share")
  tbl_out(.tab = fin_fmt(s2_, 1L), .title = "Figure 2: group shares by year on S2, percent, four groups",
          .notes = c(Registration = "S-1, S-4, F-1 and F-4, which the paper folds into ad-hoc. Read 2020-2021."))
  invisible(long_)
}

.fin_reference_f03 <- tibble::tribble(
  ~Class,                                        ~Revision,
  "Business Structure: Investment and Merger",   3.91,
  "Business Structure: Peer Agreements",         1.35,
  "Employment: Compensation",                   20.17,
  "Employment: Legal",                          20.90,
  "Financial Instruments: Credit",              21.73,
  "Financial Instruments: Equity",              11.54,
  "Leases",                                      3.35,
  "Licenses",                                    1.89,
  "Purchases and Sales: Assets",                 3.15,
  "Customer / Supplier",                         7.48,
  "R&D",                                         1.06,
  "Other",                                       3.42
)

#' Contracts by type: count, share, and per filing firm-year
#'
#' Panel B's grain is CIK x year x class, kept where the count is positive; the mean over those
#' firm-years is the paper's "contracts per firm per year" among firms filing that type. Unlabelled
#' contracts are counted in the share denominator and reported as their own row, not dropped.
#'
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Class, N, Share, nFirmYears, PerFirmYear.
fin_data_f03 <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample <- "S2_Descriptive"
  }
  a_ <- .tab |>
    dplyr::mutate(Class = dplyr::coalesce(.data$Class, "Unlabelled")) |>
    dplyr::count(.data$Class, name = "N") |>
    dplyr::mutate(Share = .data$N / sum(.data$N))

  b_ <- .tab |>
    dplyr::filter(!is.na(.data$Class)) |>
    dplyr::count(.data$CIK, .data$Year, .data$Class, name = "nFY") |>
    dplyr::summarise(nFirmYears = dplyr::n(), PerFirmYear = mean(.data$nFY), .by = "Class")

  a_ |>
    dplyr::left_join(b_, by = dplyr::join_by(Class)) |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::arrange(match(.data$Class, c(.fin_class_levels, "Unlabelled")))
}

#' Figure 3 in numbers: class shares per sample beside the revision's labels
#' @param .tab_data Every sample from `fin_data_f03()`.
#' @return Invisibly, the wide table.
fin_report_f03 <- function(.tab_data, .ref = .fin_reference_f03) {
  if (FALSE) {
    .tab_data <- tab_f03
    .ref      <- .fin_reference_f03
  }
  wide_ <- .tab_data |>
    dplyr::mutate(Share = round(100 * .data$Share, 2)) |>
    dplyr::select("Sample", "Class", "Share") |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Share") |>
    dplyr::left_join(.ref, by = dplyr::join_by(Class)) |>
    dplyr::mutate(Class = dplyr::coalesce(.fin_class_short[match(.data$Class, .fin_class_levels)], .data$Class))
  tbl_out(.tab = fin_fmt(wide_, 2L), .title = "Figure 3 Panel A: share of contracts by type, percent, per sample",
          .notes = c(Revision = "The label at the end of each bar in revision 1, drawn on S2."))

  pf_ <- .tab_data |>
    dplyr::filter(.data$Class != "Unlabelled") |>
    dplyr::select("Sample", "Class", "PerFirmYear") |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "PerFirmYear") |>
    dplyr::mutate(Class = .fin_class_short[match(.data$Class, .fin_class_levels)])
  tbl_out(.tab = fin_fmt(pf_, 2L), .title = "Figure 3 Panel B: contracts per firm-year of the type, per sample",
          .notes = c(Class = "Mean over CIK-years with at least one contract of the type."))
  invisible(wide_)
}

#' Contracts per year and type, with the year's amendment share
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Year, Class, N, Share, ShareAmend (of the year, repeated on its rows).
fin_data_f04 <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample <- "S2_Descriptive"
  }
  amend_ <- .tab |>
    dplyr::filter(!is.na(.data$IsAmend)) |>
    dplyr::summarise(ShareAmend = mean(.data$IsAmend), .by = "Year")

  .tab |>
    dplyr::filter(!is.na(.data$Class)) |>
    dplyr::count(.data$Year, .data$Class, name = "N") |>
    dplyr::mutate(Share = .data$N / sum(.data$N), .by = "Year") |>
    dplyr::left_join(amend_, by = dplyr::join_by(Year)) |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::arrange(.data$Year, match(.data$Class, .fin_class_levels))
}

#' Figure 4: stacked type shares by year with the amendment share over them
#' @param .tab_data One sample's rows from `fin_data_f04()`.
#' @return A ggplot.
fin_plot_f04 <- function(.tab_data) {
  if (FALSE) .tab_data <- fin_slice(tab_f04, "S2_Descriptive")
  line_ <- dplyr::distinct(.tab_data, .data$Year, .data$ShareAmend)
  fin_shape_share_stack(
    .tab = .tab_data, .year = "Year", .n = "N", .fill = "Class", .key = "ClassPaper",
    .line = line_, .line_y = "ShareAmend", .line_name = "Share amended", .count = FALSE
  )
}

plot_register_levels(
  .key     = "RedactSeries",
  .levels  = c("CtoOnly", "TextOnly", "Union"),
  .short   = c("CTO identification", "Marker identification", "CTO or markers (paper)"),
  .colours = c("#60a3d9", "#B7791F", "#002147")
)

.fin_reference_f10 <- tibble::tribble(
  ~Year,  ~Series,    ~Revision,
  2008L,  "CtoOnly",  2.2,
  2018L,  "CtoOnly",  3.8,
  2019L,  "Union",    6.4,
  2024L,  "Union",    11.5
)

#' Share redacted per year under the three identifications
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Year, Series, N, Share.
fin_data_f10 <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S6_Redaction, , drop = FALSE]
    .sample <- "S6_Redaction"
  }
  .tab |>
    dplyr::filter(.data$Year >= 2008L) |>
    dplyr::summarise(
      N        = dplyr::n(),
      CtoOnly  = dplyr::if_else(dplyr::first(.data$Year) <= 2018L, mean(.data$HasCto), NA_real_),
      TextOnly = mean(.data$HasMarkers),
      Union    = mean(.data$RedactedText),
      .by = "Year"
    ) |>
    tidyr::pivot_longer(c("CtoOnly", "TextOnly", "Union"), names_to = "Series", values_to = "Share") |>
    dplyr::filter(!is.na(.data$Share)) |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::arrange(.data$Series, .data$Year)
}

#' Figure 10 in numbers: the union series per sample beside the revision's readings
#' @param .tab_data Every sample from `fin_data_f10()`.
#' @return Invisibly, the table.
fin_report_f10 <- function(.tab_data, .ref = .fin_reference_f10) {
  if (FALSE) {
    .tab_data <- tab_f10
    .ref      <- .fin_reference_f10
  }
  wide_ <- .tab_data |>
    dplyr::filter(.data$Series != "TextOnly") |>
    dplyr::mutate(Share = round(100 * .data$Share, 2)) |>
    dplyr::select("Sample", "Year", "Series", "Share") |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Share") |>
    dplyr::left_join(.ref, by = dplyr::join_by(Year, Series)) |>
    dplyr::arrange(.data$Series, .data$Year)
  tbl_out(.tab = fin_fmt(wide_, 2L),
          .title = "Figure 10: share redacted per year, percent, CTO and union series per sample",
          .notes = c(Revision = "Read off the revision's figure. The union is the paper's RegEx line."))
  invisible(wide_)
}

.fin_control_sets <- list(
  Quarter = c("LogMve", "OcfWin", "AcqNeg", "AcqPos", "LeverageWin", "GoodwillImp", "Loss", "RoaWin"),
  # Her 105's $var_set for Table 4: the filing-decision controls plus the Herfindahl and the one-quarter
  # sales change. The sales lead is what costs the quarters, and the paper's 462,572 needs it.
  Filing  = c("LogMve", "OcfWin", "AcqNeg", "AcqPos", "LeverageWin", "HhiSic3Win", "ChgSalesWin", "GoodwillImp",
              "Loss", "RoaWin"),
  Full    = c("LogMve", "OcfWin", "AcqNeg", "AcqPos", "LeverageWin", "GoodwillImp", "Loss", "EqIssue", "DebtIssue",
              "RoaWin", "ChgSalesQ4Win", "MbWin")
)

#' Contract rows joined to their firm-quarter's variables
#'
#' A many-to-one join on gvkey, fiscal year and quarter, which is how 103 carries the Compustat block
#' onto contracts. Only the columns an exhibit needs travel; the contract table already holds the
#' keys.
#'
#' @param .tab A contract table, or one sample of it.
#' @param .quarter The quarter panel from `fin_read_quarter()`.
#' @param .cols Character. Quarter-panel columns to attach.
#' @return The contract rows with those columns, rows without a panel quarter kept with NA.
fin_join_quarter <- function(.tab, .quarter, .cols) {
  if (FALSE) {
    .tab     <- tab_contracts[tab_contracts$S3_Matched, , drop = FALSE]
    .quarter <- tab_quarter
    .cols    <- c("FfInd", .fin_control_sets$Full)
  }
  miss_ <- setdiff(.cols, names(.quarter))
  if (length(miss_) > 0L) cli::cli_abort("The quarter panel lacks {miss_}.")
  q_ <- .quarter |>
    dplyr::select("gvkey", "fyear", "fqtr", dplyr::all_of(setdiff(.cols, c("gvkey", "fyear", "fqtr")))) |>
    dplyr::distinct(.data$gvkey, .data$fyear, .data$fqtr, .keep_all = TRUE)
  dplyr::left_join(.tab, q_, by = dplyr::join_by(gvkey, fyear, fqtr), relationship = "many-to-one")
}

#' The two Table 4 samples as membership columns on the quarter panel
#'
#' Her 105, step for step: the controls, the fiscal quarter, the industry and the state must be
#' present; a state in which filing never varies is dropped (105 hard-codes it as state 36 "because
#' it predicts filed perfectly" -- here the rule, not the code, so it holds on any release); firms that
#' never file inside what is left are dropped; and the trimmed sample also drops firms whose filing
#' never varies, overall or within either FAST period. 105 groups firms by CIK, not gvkey.
#'
#' @param .quarter The quarter panel inside the window.
#' @param .controls Character. Controls that must be present.
#' @param .firm Character. The firm identifier the trims group by: "cik" as in 105, or "gvkey".
#' @param .drop_states Logical. Drop states without variation in filing, as 105 does.
#' @return The panel with T4_Regression, T4_TrimmedAll (overall variation only) and T4_Trimmed added.
fin_quarter_samples <- function(.quarter, .controls = .fin_control_sets$Filing, .firm = c("cik", "gvkey"),
                                .drop_states = TRUE) {
  if (FALSE) {
    .quarter     <- lst_other$S5_Quarter
    .controls    <- .fin_control_sets$Filing
    .firm        <- "cik"
    .drop_states <- TRUE
  }
  .firm <- match.arg(.firm)

  out_ <- .quarter |>
    dplyr::mutate(
      Complete = rowSums(is.na(dplyr::pick(dplyr::all_of(.controls)))) == 0L &
        !is.na(.data$State) & !is.na(.data$fyear) & !is.na(.data$fqtr) & !is.na(.data$FfIndLab)
    )
  if (.drop_states) {
    out_ <- out_ |>
      dplyr::mutate(
        StateVaries = dplyr::n_distinct(.data$Filed[.data$Complete]) > 1L,
        .by = "State"
      ) |>
      dplyr::mutate(Complete = .data$Complete & .data$StateVaries) |>
      dplyr::select(-"StateVaries")
  }
  out_ |>
    dplyr::mutate(
      EverFiles     = any(.data$Filed[.data$Complete] == 1L),
      T4_Regression = .data$Complete & .data$EverFiles,
      .by = dplyr::all_of(.firm)
    ) |>
    dplyr::mutate(
      MeanAll  = mean(.data$Filed[.data$T4_Regression]),
      MeanPre  = mean(.data$Filed[.data$T4_Regression & .data$PostFast == 0L]),
      MeanPost = mean(.data$Filed[.data$T4_Regression & .data$PostFast == 1L]),
      .by = dplyr::all_of(.firm)
    ) |>
    dplyr::mutate(
      # Overall variation only -- the trim an earlier 105 applied -- kept as its own column so the
      # ladder can show where the revision's count sits between the two.
      T4_TrimmedAll = .data$T4_Regression & !is.nan(.data$MeanAll) & .data$MeanAll > 0 & .data$MeanAll < 1,
      T4_Trimmed    = .data$T4_TrimmedAll &
        (is.nan(.data$MeanPre) | (.data$MeanPre > 0 & .data$MeanPre < 1)) &
        (is.nan(.data$MeanPost) | (.data$MeanPost > 0 & .data$MeanPost < 1))
    ) |>
    dplyr::select(-"EverFiles", -"MeanAll", -"MeanPre", -"MeanPost")
}

.fin_reference_f08 <- tibble::tribble(
  ~Industry,              ~Revision,
  "Consumer NonDurables", 1.30,
  "Utilities",            1.57,
  "Healthcare",           1.58,
  "Finance",              1.11
)

#' Share redacted per industry x type x period
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @param .quarter The quarter panel, for FfInd.
#' @return Tibble: Sample, Period, Industry, Class, N, Share.
fin_data_f09 <- function(.tab, .sample, .quarter) {
  if (FALSE) {
    .tab     <- tab_contracts[tab_contracts$S3b_Matched2008, , drop = FALSE]
    .sample  <- "S3b_Matched2008"
    .quarter <- tab_quarter
  }
  fin_join_quarter(.tab = .tab, .quarter = .quarter, .cols = "FfIndLab") |>
    dplyr::filter(.data$Year >= 2008L, !is.na(.data$Redacted), !is.na(.data$Class), !is.na(.data$FfIndLab)) |>
    dplyr::mutate(Period = dplyr::if_else(.data$PostFast == 1L, "Post-FAST", "Pre-FAST")) |>
    dplyr::summarise(N = dplyr::n(), Share = mean(.data$Redacted), .by = c("Period", "FfIndLab", "Class")) |>
    dplyr::rename(Industry = "FfIndLab") |>
    dplyr::mutate(Industry = as.character(.data$Industry), Sample = .sample) |>
    dplyr::arrange(.data$Period, match(.data$Industry, .fin_ff12_levels), match(.data$Class, .fin_class_levels)) |>
    dplyr::relocate("Sample")
}

#' Figure 9 in numbers: the industry means per period, and the cells that carry fewer than 30 contracts
#' @param .tab_data Every sample from `fin_data_f09()`.
#' @return Invisibly, the industry table.
fin_report_f09 <- function(.tab_data) {
  if (FALSE) .tab_data <- tab_f09
  ind_ <- .tab_data |>
    dplyr::summarise(Share = sum(.data$Share * .data$N) / sum(.data$N), nThin = sum(.data$N < 30L),
                     .by = c("Sample", "Period", "Industry")) |>
    dplyr::mutate(Col = paste0(.data$Sample, "_", .data$Period)) |>
    dplyr::select("Industry", "Col", "Share") |>
    tidyr::pivot_wider(names_from = "Col", values_from = "Share") |>
    dplyr::arrange(match(.data$Industry, .fin_ff12_levels))
  tbl_out(.tab = fin_fmt(ind_, 3L), .title = "Figure 9: share redacted by industry and period, per sample",
          .notes = c(Industry = "Contract-weighted over types. Her heat map had rows that barely differed; these differ."))
  invisible(ind_)
}

#' Firm-years with their country count and diversity, under one grain rule
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @param .conc Tibble from `fin_read_concentration()`.
#' @param .grain Character. "mean" over the firm's contracts, or "first" contract, as 104 does.
#' @return Tibble: Sample, Grain, gvkey, fyear, Countries, Diversity.
fin_data_f07 <- function(.tab, .sample, .conc, .grain = "mean") {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S3_Matched, , drop = FALSE]
    .sample <- "S3_Matched"
    .conc   <- tab_concentration
    .grain  <- "mean"
  }
  .grain <- match.arg(.grain, c("mean", "first"))
  fy_ <- .tab |>
    dplyr::filter(!is.na(.data$gvkey), !is.na(.data$Countries)) |>
    dplyr::arrange(.data$gvkey, .data$fyear, .data$DocID)
  fy_ <- if (.grain == "mean") {
    dplyr::summarise(fy_, Countries = mean(.data$Countries), .by = c("gvkey", "fyear"))
  } else {
    dplyr::distinct(fy_, .data$gvkey, .data$fyear, .keep_all = TRUE) |> dplyr::select("gvkey", "fyear", "Countries")
  }
  fy_ |>
    dplyr::inner_join(dplyr::select(.conc, "gvkey", "fyear", Diversity = "diversity"), by = dplyr::join_by(gvkey, fyear)) |>
    dplyr::filter(!is.na(.data$Diversity)) |>
    dplyr::mutate(Sample = .sample, Grain = .grain) |>
    dplyr::relocate("Sample", "Grain")
}

#' The binscatter: bin means, the OLS line, the slope and its t
#' @param .tab_data One sample's rows from `fin_data_f07()`.
#' @param .bins Integer. Equal-count bins of diversity.
#' @param .trim Character. "p99" clips both at the 99th percentile; "drop" removes the top percentile of both.
#' @return List: Bins (tibble), Fit (tibble: Slope, TStat, N).
fin_bins_f07 <- function(.tab_data, .bins = 50L, .trim = "p99") {
  if (FALSE) {
    .tab_data <- fin_slice(tab_f07, "S3_Matched")
    .bins     <- 50L
    .trim     <- "p99"
  }
  .trim <- match.arg(.trim, c("p99", "drop"))
  d_ <- .tab_data
  if (.trim == "p99") {
    d_ <- d_ |>
      dplyr::mutate(
        Countries = pmin(.data$Countries, stats::quantile(.data$Countries, 0.99, names = FALSE)),
        Diversity = pmin(.data$Diversity, stats::quantile(.data$Diversity, 0.99, names = FALSE))
      )
  } else {
    d_ <- d_ |>
      dplyr::filter(.data$Countries < stats::quantile(.data$Countries, 0.99, names = FALSE),
                    .data$Diversity < stats::quantile(.data$Diversity, 0.99, names = FALSE))
  }
  fit_ <- stats::lm(Countries ~ Diversity, data = d_)
  co_  <- summary(fit_)$coefficients
  bins_ <- d_ |>
    dplyr::mutate(Bin = dplyr::ntile(.data$Diversity, .bins)) |>
    dplyr::summarise(Diversity = mean(.data$Diversity), Countries = mean(.data$Countries), N = dplyr::n(), .by = "Bin")
  list(
    Bins = bins_,
    Fit  = tibble::tibble(Slope = co_["Diversity", "Estimate"], TStat = co_["Diversity", "t value"], N = nrow(d_),
                          Intercept = co_["(Intercept)", "Estimate"])
  )
}

plot_register_levels(
  .key     = "TermFamily",
  .levels  = .fin_family_levels,
  .short   = c("Pandemic", "Disruption", "Rate reform", "Regulation", "Boilerplate"),
  .colours = NULL
)


# 1. The release on disk ----------------------------------------------------------------------------------------------------

#' Check every release file against the release the paper is written on
#'
#' The 9 September release carries the export_GPE() null fix; every state and country count before it
#' is one too high on a fifth of contracts. A render on an older Contracts or Places file prints
#' numbers the paper then has to retract, so the vintage is checked before anything is read. Her .dta
#' outputs are exempt from the date but reported beside the release so that a 103 run older than the
#' release it should have read is visible on the same line. Whether an older release file aborts or
#' warns is the .strict switch: FALSE while the deck is being built, TRUE for the render that goes
#' into the manuscript.
#'
#' @param .inputs Named character. Every path in .lP$Input.
#' @param .min_date Date or character. The release date the paper is written on.
#' @param .strict Logical. TRUE aborts on an older release file; FALSE warns and names it. Only the
#'   files carrying geography counts (Contracts, Places) moved with the fix, so an older Summaries or
#'   TermDocs is a vintage note, not a wrong number.
#' @return Invisibly, the vintage table (Input, File, Exists, Modified, IsRelease, Current).
fin_check_vintage <- function(.inputs, .min_date, .strict = FALSE) {
  if (FALSE) {
    .inputs   <- unlist(.lP$Input)
    .min_date <- .lP$Params$ReleaseMin
    .strict   <- FALSE
  }
  min_ <- as.Date(.min_date)

  out_ <- tibble::tibble(
    Input     = names(.inputs),
    File      = fs::path_file(unname(.inputs)),
    Exists    = fs::file_exists(unname(.inputs)),
    Modified  = as.Date(fs::file_info(unname(.inputs))$modification_time),
    IsRelease = stringi::stri_endswith_fixed(unname(.inputs), ".parquet")
  ) |>
    dplyr::mutate(Current = .data$Exists & .data$Modified >= min_)

  missing_ <- out_$File[!out_$Exists]
  if (length(missing_) > 0L) {
    cli::cli_abort("{length(missing_)} input{?s} not found: {missing_}. Is the Dropbox root in .lP right?")
  }

  stale_ <- out_$File[out_$IsRelease & !out_$Current]
  if (length(stale_) > 0L) {
    msg_ <- c(
      "{length(stale_)} release file{?s} predate{?s/} the {as.character(min_)} release: {stale_}.",
      "i" = "Deploy the current 10-ExportData release to Dropbox before the final render."
    )
    if (.strict) cli::cli_abort(msg_) else cli::cli_warn(msg_)
  }

  older_ <- out_$File[!out_$IsRelease & !out_$Current]
  if (length(older_) > 0L) {
    cli::cli_alert_warning(
      "{length(older_)} of her output{?s} {?is/are} older than the release: {older_}. \\
       Table 2 Panel B, Table 4 and the redaction reconciliation close only after her 102/103 rerun."
    )
  }

  tbl_out(
    .tab   = dplyr::mutate(out_, Modified = as.character(.data$Modified)),
    .title = "Every input, dated against the release the paper is written on",
    .notes = c(Current = "FALSE on a .dta is a vintage gap; FALSE on a .parquet warns, or aborts under .strict.")
  )
  invisible(out_)
}

#' Convert one of her .dta files to parquet under this document's own cache, when it has changed
#'
#' STAND-ALONE. This document reads nothing another document rendered: the conversion is its own
#' artifact under its own Cache, made with 30's fin_dta_to_parquet() (the code is shared; the file is
#' not), and rebuilt when the .dta changed. Changed means a newer modification time AND a different
#' size: Dropbox re-syncs and re-hydrates files without changing a byte, and an mtime alone would
#' rebuild a two-gigabyte conversion on every render. The size is recorded in a sidecar beside the
#' parquet at every build; a rerun of 103 changes it.
#'
#' @param .path_dta Character. Her .dta.
#' @param .path_cache Character. The parquet under this document's Cache.
#' @param .cols Named character. The column map 30 uses for this file, or NULL for every column.
#' @param .rerun Logical. TRUE reconverts regardless.
#' @return Invisibly, the cache path.
fin_convert_dta <- function(.path_dta, .path_cache, .cols = NULL, .rerun = FALSE) {
  if (FALSE) {
    .path_dta   <- .lP$Input$DtaQuarter
    .path_cache <- .lP$Cache$CacheQuarter
    .cols       <- .fin_quarter_cols
    .rerun      <- FALSE
  }
  task_ <- fs::path_file(.path_dta)
  if (!fs::file_exists(.path_dta)) cli::cli_abort("{(task_)}: not found at {.file {(.path_dta)}}.")
  side_     <- paste0(.path_cache, ".src")
  size_now_ <- as.character(as.numeric(fs::file_size(.path_dta)))
  hit_ <- !.rerun && fs::file_exists(.path_cache) && fs::file_exists(side_) &&
    identical(readLines(side_, n = 1L, warn = FALSE), size_now_)
  if (hit_) {
    cli::cli_alert_success("{(task_)}: parquet cache matches the .dta's size and is read as it stands.")
    return(invisible(.path_cache))
  }
  fin_dta_to_parquet(
    .path_dta = .path_dta,
    .path_out = .path_cache,
    .cols     = .cols,
    .rerun    = TRUE
  )
  writeLines(c(size_now_, as.character(fs::file_info(.path_dta)$modification_time)), side_)
  invisible(.path_cache)
}


# 2. Prepare: every input to disk, keyed on what it was built from ----------------------------------------------------
# One function per table. Each reads through its fin_read_*(), adds what every exhibit needs from that
# table -- membership columns above all -- and writes one parquet under Output/Prepared. Each begins
# with fin_prepared_hit(): current on disk means return the path and read nothing.

# THE PREPARED TABLES, in the order they are built (Summaries needs Contracts), and the contract-level
# membership columns in ladder order.
.fin_tables <- c("Contracts", "Summaries", "Quarter", "Concentration", "Places", "TermDocs", "CtoOrders")

.fin_contract_samples <- c(
  "S00_Edgar", "S0_Universe", "S1_Unique", "S2_Descriptive", "S2s_Seasoned", "S3_Matched", "S3a_Matched2005",
  "S3b_Matched2008", "S6_Redaction"
)

#' Is a prepared table current on disk?
#'
#' Current means: the parquet exists, is newer than every input it was built from and than this
#' library, and -- where the table depends on the definitional switches -- carries the same hash of
#' them in its sidecar. Anything else rebuilds. .rerun = TRUE rebuilds regardless.
#'
#' @param .name Character. Table name, the file stem.
#' @param .dir Character. Output/Prepared.
#' @param .paths_in Character. Files the table was built from.
#' @param .params List or NULL. The switches whose hash the sidecar must match.
#' @param .rerun Logical. TRUE forces a rebuild.
#' @return Logical. TRUE if the prepared table can be read as it stands.
fin_prepared_hit <- function(.name, .dir, .paths_in, .params = NULL, .rerun = FALSE) {
  if (FALSE) {
    .name     <- "Contracts"
    .dir      <- .lP$Output$DirPrepared
    .paths_in <- c(.lP$Input$FilContracts, .lP$Cache$CacheAkContract)
    .params   <- .lP$Params
    .rerun    <- FALSE
  }
  path_ <- fs::path(.dir, paste0(.name, ".parquet"))
  lib_  <- init_create_script_fun(.dir_here = here::here(), .name_script = "31-FinalExhibits")
  hit_  <- exp_cache_hit(
    .task     = .name,
    .path_out = path_,
    .paths_in = c(.paths_in, lib_),
    .rerun    = .rerun
  )
  if (!hit_) return(FALSE)

  if (!is.null(.params)) {
    side_ <- fs::path(.dir, paste0(.name, ".hash"))
    have_ <- if (fs::file_exists(side_)) readLines(side_, n = 1L, warn = FALSE) else ""
    if (!identical(have_, rlang::hash(.params))) {
      cli::cli_alert_warning("{(.name)}: rebuilding, the definitional switches changed since it was prepared.")
      return(FALSE)
    }
  }
  cli::cli_alert_info("{(.name)}: prepared table is current and is read as it stands.")
  TRUE
}

#' Write a prepared table, its switch hash beside it, and say what was written
#'
#' @param .tab Tibble.
#' @param .name Character. Table name; the file stem and the DuckDB table name.
#' @param .dir Character. Output/Prepared.
#' @param .params List or NULL. The switches the table was built under; hashed into a sidecar.
#' @return Invisibly, the path written.
fin_write_prepared <- function(.tab, .name, .dir, .params = NULL) {
  if (FALSE) {
    .tab    <- tab_contracts
    .name   <- "Contracts"
    .dir    <- .lP$Output$DirPrepared
    .params <- .lP$Params
  }
  if (nrow(.tab) == 0L) cli::cli_abort("{(.name)}: nothing to write; the reader returned no rows.")
  path_ <- fs::path(.dir, paste0(.name, ".parquet"))
  fs::dir_create(.dir)
  arrow::write_parquet(.tab, path_)
  if (!is.null(.params)) writeLines(rlang::hash(.params), fs::path(.dir, paste0(.name, ".hash")))
  cli::cli_alert_success(
    "{(.name)}: {format(nrow(.tab), big.mark = ',')} rows, {ncol(.tab)} columns, \\
     {round(as.numeric(fs::file_size(path_)) / 1e9, 2)} GB on disk"
  )
  invisible(path_)
}

#' The contract-level samples as membership columns
#'
#' The sample definitions, one logical column each, in ladder order:
#'   S00_Edgar        every row of the release, ladder step 1 included
#'   S0_Universe      every copy at ladder steps 2-6: the paper's full sample
#'   S1_Unique        one copy per attachment, whatever its ladder step (PrimaryFiler)
#'   S2_Descriptive   the descriptive sample: inside the window, well-formed, one copy per attachment (Keep)
#'   S2s_Seasoned     S2 from filers more than two years past their first contract (104_SPAC.do's rule)
#'   S3_Matched       S2 with a matched Compustat quarter
#'   S3a_Matched2005  S3 from 2005, the delay tables' start year
#'   S3b_Matched2008  S3 from 2008, the redaction tables' start year
#'   S6_Redaction     S2 from 2008, when orders become observable
#' A membership column costs one byte per row; an exhibit is drawn on a sample by filtering on the
#' column of that name, in DuckDB or in R, and the same figure on two samples is the same function
#' called twice.
#'
#' @param .tab Tibble from `fin_read_contracts()`.
#' @return The tibble with the nine columns added.
fin_add_samples <- function(.tab) {
  if (FALSE) .tab <- fin_read_contracts(.path = .lP$Input$FilContracts)
  .tab |>
    dplyr::mutate(
      S00_Edgar       = TRUE,
      S0_Universe     = .data$SampleStepCode >= 2L,
      S1_Unique       = .data$PrimaryFiler == 1L,
      S2_Descriptive  = .data$Keep,
      S2s_Seasoned    = .data$Keep & .data$IsSeasoned == 1L,
      S3_Matched      = .data$Keep & .data$Matched,
      S3a_Matched2005 = .data$Keep & .data$Matched & .data$Year >= 2005L,
      S3b_Matched2008 = .data$Keep & .data$Matched & .data$Year >= 2008L,
      S6_Redaction    = .data$Keep & .data$Year >= 2008L
    )
}

#' Prepare Contracts: the release read as 30 reads it, her flags joined, the samples added
#'
#' @param .path Character. Contracts.parquet in the release.
#' @param .path_ak Character. 30's conversion of her contract-level file.
#' @param .params List. The definitional switches: Redaction, RedactMin, Duration, Words, Parties.
#' @param .dir Character. Output/Prepared.
#' @param .rerun Logical. TRUE rebuilds regardless of what is on disk.
#' @return Invisibly, the path written.
fin_prepare_contracts <- function(.path, .path_ak, .params, .dir, .rerun = FALSE) {
  if (FALSE) {
    .path    <- .lP$Input$FilContracts
    .path_ak <- .lP$Cache$CacheAkContract
    .params  <- .lP$Params
    .dir     <- .lP$Output$DirPrepared
    .rerun   <- FALSE
  }
  hit_ <- fin_prepared_hit(
    .name     = "Contracts",
    .dir      = .dir,
    .paths_in = c(.path, .path_ak),
    .params   = .params,
    .rerun    = .rerun
  )
  if (hit_) {
    return(invisible(fs::path(.dir, "Contracts.parquet")))
  }
  tab_ <- fin_read_contracts(
    .path       = .path,
    .redaction  = .params$Redaction,
    .redact_min = .params$RedactMin,
    .duration   = .params$Duration,
    .words      = .params$Words,
    .parties    = .params$Parties
  )
  unknown_ <- setdiff(unique(stats::na.omit(tab_$Class)), .fin_class_levels)
  if (length(unknown_) > 0L) cli::cli_abort("Contracts carries {?a class/classes} this document does not know: {unknown_}.")

  ak_ <- fin_read_ak_contract(.path = .path_ak)
  exp_check_collide(
    .a    = names(tab_),
    .b    = names(ak_),
    .by   = "DocID",
    .what = "the reconciliation join"
  )

  tab_ |>
    dplyr::left_join(ak_, by = dplyr::join_by(DocID), relationship = "one-to-one") |>
    fin_add_samples() |>
    fin_write_prepared(
      .name   = "Contracts",
      .dir    = .dir,
      .params = .params
    )
}

#' Prepare Summaries: the announcements with the exhibit count and the two announcement samples
#'
#' The paper's Table 7 sample is single-agreement Item 1.01 announcements on 8-Ks carrying at most one
#' Exhibit 10; the exhibit count comes from the prepared contract table, read lazily for the two
#' columns that count. S7_All is every announcement, the restriction lifted.
#'
#' @param .path Character. Summaries.parquet in the release.
#' @param .path_contracts Character. The prepared Contracts.parquet.
#' @param .dir Character. Output/Prepared.
#' @param .rerun Logical. TRUE rebuilds regardless of what is on disk.
#' @return Invisibly, the path written.
fin_prepare_summaries <- function(.path, .path_contracts, .dir, .rerun = FALSE) {
  if (FALSE) {
    .path           <- .lP$Input$FilSummaries
    .path_contracts <- fs::path(.lP$Output$DirPrepared, "Contracts.parquet")
    .dir            <- .lP$Output$DirPrepared
    .rerun          <- FALSE
  }
  hit_ <- fin_prepared_hit(
    .name     = "Summaries",
    .dir      = .dir,
    .paths_in = c(.path, .path_contracts),
    .rerun    = .rerun
  )
  if (hit_) {
    return(invisible(fs::path(.dir, "Summaries.parquet")))
  }
  con_ <- arrow::open_dataset(.path_contracts) |>
    dplyr::select("HashIndex", "DescSample") |>
    dplyr::collect()

  fin_read_summaries(
    .path          = .path,
    .tab_contracts = con_
  ) |>
    dplyr::mutate(
      S7_Summaries = .data$SumIsSingle == 1L & .data$nExhibits <= 1L,
      S7_All       = TRUE
    ) |>
    fin_write_prepared(
      .name = "Summaries",
      .dir  = .dir
    )
}

#' Prepare Quarter: her firm-quarter panel with the window sample
#'
#' 103 leaves 1997-2026 on the file and every do-file drops year_fe outside 2001-2024 at the point of
#' use; here S5_Quarter is that window as a column. The regression-sample rungs Table 4 counts are
#' built where Table 4 is built, because they depend on the control set.
#'
#' @param .path_cache Character. 30's conversion of the quarter panel.
#' @param .dir Character. Output/Prepared.
#' @param .rerun Logical. TRUE rebuilds regardless of what is on disk.
#' @return Invisibly, the path written.
fin_prepare_quarter <- function(.path_cache, .dir, .rerun = FALSE) {
  if (FALSE) {
    .path_cache <- .lP$Cache$CacheQuarter
    .dir        <- .lP$Output$DirPrepared
    .rerun  <- FALSE
  }
  hit_ <- fin_prepared_hit(
    .name     = "Quarter",
    .dir      = .dir,
    .paths_in = .path_cache,
    .rerun    = .rerun
  )
  if (hit_) {
    return(invisible(fs::path(.dir, "Quarter.parquet")))
  }
  fin_read_quarter(.path = .path_cache) |>
    dplyr::mutate(S5_Quarter = dplyr::between(.data$cyear, 2001L, 2024L)) |>
    fin_write_prepared(
      .name = "Quarter",
      .dir  = .dir
    )
}

#' Prepare Concentration: 101's segment concentration and diversity, one row per gvkey-year
#' @param .path_cache Character. 30's conversion of concentration.dta.
#' @param .dir Character. Output/Prepared.
#' @param .rerun Logical. TRUE rebuilds regardless of what is on disk.
#' @return Invisibly, the path written.
fin_prepare_concentration <- function(.path_cache, .dir, .rerun = FALSE) {
  if (FALSE) {
    .path_cache <- .lP$Cache$CacheConcentration
    .dir        <- .lP$Output$DirPrepared
    .rerun  <- FALSE
  }
  hit_ <- fin_prepared_hit(
    .name     = "Concentration",
    .dir      = .dir,
    .paths_in = .path_cache,
    .rerun    = .rerun
  )
  if (hit_) {
    return(invisible(fs::path(.dir, "Concentration.parquet")))
  }
  fin_read_concentration(.path = .path_cache) |>
    fin_write_prepared(
      .name = "Concentration",
      .dir  = .dir
    )
}

#' Prepare Places: every place a contract names, with its party role and law-clause flag
#' @param .path Character. Places.parquet in the release.
#' @param .dir Character. Output/Prepared.
#' @param .rerun Logical. TRUE rebuilds regardless of what is on disk.
#' @return Invisibly, the path written.
fin_prepare_places <- function(.path, .dir, .rerun = FALSE) {
  if (FALSE) {
    .path   <- .lP$Input$FilPlaces
    .dir    <- .lP$Output$DirPrepared
    .rerun  <- FALSE
  }
  hit_ <- fin_prepared_hit(
    .name     = "Places",
    .dir      = .dir,
    .paths_in = .path,
    .rerun    = .rerun
  )
  if (hit_) {
    return(invisible(fs::path(.dir, "Places.parquet")))
  }
  fin_read_places(.path = .path) |>
    fin_write_prepared(
      .name = "Places",
      .dir  = .dir
    )
}

#' Prepare TermDocs: 05A's hits per contract and family
#' @param .path Character. TermDocs.parquet in the release.
#' @param .dir Character. Output/Prepared.
#' @param .rerun Logical. TRUE rebuilds regardless of what is on disk.
#' @return Invisibly, the path written.
fin_prepare_term_docs <- function(.path, .dir, .rerun = FALSE) {
  if (FALSE) {
    .path   <- .lP$Input$FilTermDocs
    .dir    <- .lP$Output$DirPrepared
    .rerun  <- FALSE
  }
  hit_ <- fin_prepared_hit(
    .name     = "TermDocs",
    .dir      = .dir,
    .paths_in = .path,
    .rerun    = .rerun
  )
  if (hit_) {
    return(invisible(fs::path(.dir, "TermDocs.parquet")))
  }
  fin_read_term_docs(.path = .path) |>
    fin_write_prepared(
      .name = "TermDocs",
      .dir  = .dir
    )
}

#' Prepare CtoOrders: every order reference 01E parsed, linked or not
#' @param .path Character. CtoOrders.parquet in the release.
#' @param .dir Character. Output/Prepared.
#' @param .rerun Logical. TRUE rebuilds regardless of what is on disk.
#' @return Invisibly, the path written.
fin_prepare_cto_orders <- function(.path, .dir, .rerun = FALSE) {
  if (FALSE) {
    .path   <- .lP$Input$FilCtoOrders
    .dir    <- .lP$Output$DirPrepared
    .rerun  <- FALSE
  }
  hit_ <- fin_prepared_hit(
    .name     = "CtoOrders",
    .dir      = .dir,
    .paths_in = .path,
    .rerun    = .rerun
  )
  if (hit_) {
    return(invisible(fs::path(.dir, "CtoOrders.parquet")))
  }
  fin_read_cto_orders(.path = .path) |>
    fin_write_prepared(
      .name = "CtoOrders",
      .dir  = .dir
    )
}


# 3. Open and register: the prepared tables, lazily ---------------------------------------------------------------------

#' Open every prepared parquet as an Arrow dataset
#'
#' Nothing is read: an Arrow dataset is a handle on the file, and dplyr verbs on it are pushed down
#' until collect(). The list is named by table, so `lst_ds$Contracts` is the contract table.
#'
#' @param .dir Character. Output/Prepared.
#' @param .tables Character. The table names expected; a missing file aborts.
#' @return Named list of Arrow datasets.
fin_open_prepared <- function(.dir, .tables) {
  if (FALSE) {
    .dir    <- .lP$Output$DirPrepared
    .tables <- .fin_tables
  }
  paths_ <- fs::path(.dir, paste0(.tables, ".parquet"))
  miss_  <- .tables[!fs::file_exists(paths_)]
  if (length(miss_) > 0L) cli::cli_abort("Prepared table{?s} not on disk: {miss_}. Run the prepare chunk first.")
  purrr::map(purrr::set_names(paths_, .tables), \(.p) arrow::open_dataset(.p))
}

#' Register every prepared table on one DuckDB connection under its table name
#'
#' A VIEW ON THE PARQUET, NOT AN ARROW REGISTRATION. DuckDB can scan a registered Arrow dataset, but
#' filter pushdown into that scan is partial -- an IN list is "not supported yet" and aborts the
#' query -- and it is slower than DuckDB's own parquet reader. Each prepared table is therefore a
#' view over read_parquet() on its file, so `DBI::dbGetQuery(con, "... FROM Contracts ...")` and
#' `dplyr::tbl(con, "Contracts")` see the same rows the Arrow dataset does, with every predicate
#' pushed down. A view is replaced on re-registration.
#'
#' @param .con A DuckDB connection.
#' @param .datasets Named list from `fin_open_prepared()`; each dataset's file is what the view reads.
#' @return Invisibly, the table names registered.
fin_register_prepared <- function(.con, .datasets) {
  if (FALSE) {
    .con      <- con
    .datasets <- lst_ds
  }
  purrr::iwalk(.datasets, \(.ds, .n) {
    files_ <- .ds$files
    if (length(files_) != 1L) cli::cli_abort("{(.n)}: expected one parquet file, found {length(files_)}.")
    DBI::dbExecute(
      conn      = .con,
      statement = sprintf("CREATE OR REPLACE VIEW \"%s\" AS SELECT * FROM read_parquet('%s')", .n, files_)
    )
  })
  cli::cli_alert_success("{length(.datasets)} table{?s} registered on DuckDB as parquet views: {names(.datasets)}")
  invisible(names(.datasets))
}

#' One line per prepared table: rows, columns, size on disk
#'
#' @param .datasets Named list from `fin_open_prepared()`.
#' @return Invisibly, the summary table.
fin_report_prepared <- function(.datasets) {
  if (FALSE) .datasets <- lst_ds
  out_ <- purrr::imap(.datasets, \(.ds, .n) tibble::tibble(
    Table   = .n,
    Rows    = nrow(.ds),
    Columns = ncol(.ds),
    SizeGB  = round(sum(as.numeric(fs::file_size(.ds$files))) / 1e9, 2)
  )) |>
    purrr::list_rbind()
  tbl_out(
    .tab   = out_,
    .title = "The prepared tables, opened lazily",
    .notes = c(Rows = "Counted from the parquet metadata; nothing is in memory until an exhibit collects.")
  )
  invisible(out_)
}


# 4. Samples ----------------------------------------------------------------------------------------------------------------

#' The named samples, counted on the prepared tables in DuckDB
#'
#' Rows, distinct contracts and distinct firms per contract-level sample from one query over
#' Contracts; the announcement and quarter samples from their own tables. Labels are 30's.
#'
#' @param .con A DuckDB connection with the prepared tables registered.
#' @param .samples Character. The contract-level membership columns.
#' @return Tibble: Sample, What, Rows, Contracts, Firms.
fin_table_samples <- function(.con, .samples) {
  if (FALSE) {
    .con     <- con
    .samples <- .fin_contract_samples
  }
  sel_ <- purrr::map_chr(.samples, \(.s) sprintf(
    paste0("COUNT(*) FILTER (WHERE \"%s\") AS \"Rows_%s\", ",
           "COUNT(DISTINCT HashDocument) FILTER (WHERE \"%s\") AS \"Contracts_%s\", ",
           "COUNT(DISTINCT CIK) FILTER (WHERE \"%s\") AS \"Firms_%s\""),
    .s, .s, .s, .s, .s, .s
  ))
  wide_ <- DBI::dbGetQuery(.con, paste0("SELECT ", paste(sel_, collapse = ", "), " FROM Contracts"))
  con_ <- tibble::tibble(
    Sample    = .samples,
    Rows      = as.integer(unlist(wide_[paste0("Rows_", .samples)])),
    Contracts = as.integer(unlist(wide_[paste0("Contracts_", .samples)])),
    Firms     = as.integer(unlist(wide_[paste0("Firms_", .samples)]))
  )

  sum_ <- DBI::dbGetQuery(.con, paste(
    "SELECT COUNT(*) FILTER (WHERE S7_Summaries) AS S7_Summaries, COUNT(*) FILTER (WHERE S7_All) AS S7_All",
    "FROM Summaries"
  ))
  qtr_ <- DBI::dbGetQuery(.con, paste(
    "SELECT COUNT(*) FILTER (WHERE S5_Quarter) AS Rows, COUNT(DISTINCT gvkey) FILTER (WHERE S5_Quarter) AS Firms",
    "FROM Quarter"
  ))
  oth_ <- tibble::tibble(
    Sample    = c("S7_Summaries", "S7_All", "S5_Quarter"),
    Rows      = as.integer(c(sum_$S7_Summaries, sum_$S7_All, qtr_$Rows)),
    Contracts = NA_integer_,
    Firms     = as.integer(c(NA, NA, qtr_$Firms))
  )

  dplyr::bind_rows(con_, oth_) |>
    dplyr::mutate(What = unname(.fin_sample_labels[.data$Sample]), .after = "Sample")
}

#' The sample columns nest as the ladder says, and the paper's sample is the paper's number
#'
#' Three things, each stated before it is counted. Every membership column is non-empty. The
#' contract-level samples nest: S00 contains S0 and S1, both contain S2, S2 contains its subsets and S6.
#' S2 counts exactly the revision's unique contracts, which 30 already checks through the ladder;
#' it is checked again here on the prepared columns, because the columns are what every exhibit
#' filters on. Only the membership columns are collected: nine bytes per row.
#'
#' @param .ds The prepared Contracts dataset.
#' @param .samples Character. The contract-level membership column names.
#' @param .ref Tibble. 30's .fin_reference; the "Unique contracts" row is read.
#' @return Invisibly, a tibble of sample counts.
fin_check_samples <- function(.ds, .samples, .ref = .fin_reference) {
  if (FALSE) {
    .ds      <- lst_ds$Contracts
    .samples <- .fin_contract_samples
    .ref     <- .fin_reference
  }
  miss_ <- setdiff(.samples, names(.ds))
  if (length(miss_) > 0L) cli::cli_abort("No membership column{?s} for sample{?s} {miss_}.")
  tab_ <- .ds |>
    dplyr::select(dplyr::all_of(.samples)) |>
    dplyr::collect()

  counts_ <- tibble::tibble(Sample = .samples, N = purrr::map_int(.samples, \(.s) sum(tab_[[.s]], na.rm = TRUE)))
  empty_  <- counts_$Sample[counts_$N == 0L]
  if (length(empty_) > 0L) cli::cli_abort("Empty sample{?s}: {empty_}.")

  # NESTING. A parent-child pair is listed once; a child row outside its parent is a definition error.
  # S1 is not inside S0: a primary copy at ladder step 1 is outside the window but still one per
  # attachment. Both sit inside S00, and S2 sits inside both.
  nest_ <- tibble::tribble(
    ~Parent,          ~Child,
    "S00_Edgar",      "S0_Universe",
    "S00_Edgar",      "S1_Unique",
    "S0_Universe",    "S2_Descriptive",
    "S1_Unique",      "S2_Descriptive",
    "S2_Descriptive", "S2s_Seasoned",
    "S2_Descriptive", "S3_Matched",
    "S3_Matched",     "S3a_Matched2005",
    "S3_Matched",     "S3b_Matched2008",
    "S2_Descriptive", "S6_Redaction"
  ) |>
    dplyr::filter(.data$Parent %in% .samples, .data$Child %in% .samples) |>
    dplyr::mutate(Outside = purrr::map2_int(.data$Parent, .data$Child, \(.p, .c) sum(tab_[[.c]] & !tab_[[.p]])))
  broken_ <- nest_[nest_$Outside > 0L, , drop = FALSE]
  if (nrow(broken_) > 0L) {
    what_ <- paste0(broken_$Child, " not in ", broken_$Parent, " (", broken_$Outside, ")")
    cli::cli_abort("Sample{?s} outside {?its/their} parent: {what_}.")
  }

  n_s2_  <- counts_$N[counts_$Sample == "S2_Descriptive"]
  n_ref_ <- .ref$Revision[.ref$Quantity == "Unique contracts"]
  if (length(n_s2_) == 1L && n_s2_ != n_ref_) {
    cli::cli_abort("S2 counts {format(n_s2_, big.mark = ',')} against the revision's {format(n_ref_, big.mark = ',')}.")
  }

  cli::cli_alert_success(
    "{length(.samples)} contract-level samples, all non-empty, nested as the ladder says; \\
     S2 = {format(n_s2_, big.mark = ',')} as the revision prints."
  )
  invisible(counts_)
}


# 5. Exhibits ---------------------------------------------------------------------------------------------------------------
# One block per exhibit, in the order the manuscript prints them: fin_data_<id>() reading from the
# prepared tables, fin_plot_<id>() or fin_tex_<id>() for the manuscript version, fin_note_<id>() for
# the figure note written from the tibble. Added one at a time.

# THE MANUSCRIPT'S TABLES ARE NAMED, NOT NUMBERED. Numbers move with every reorder; a file called
# Categories.tex does not. Each table function writes four things under one stem: the tibble
# (Data/<Name>.parquet), the tabular (Tables/<Name>.tex, booktabs, no caption -- the manuscript's
# table environment supplies it), the note (Notes/<Name>.tex, one paragraph), and the rendered html
# into the runbook. The chunk that calls it needs `results: asis`.

#' Escape the characters LaTeX reads as commands
#' @param .x Character.
#' @return Character, safe inside a tabular cell.
fin_tex_escape <- function(.x) {
  if (FALSE) .x <- c("R&D", "50% of $1")
  out_ <- gsub("\\", "\\textbackslash{}", .x, fixed = TRUE)
  for (.ch in c("&", "%", "$", "#", "_")) out_ <- gsub(.ch, paste0("\\", .ch), out_, fixed = TRUE)
  out_
}

#' Finish a tabular fragment for the manuscript and write it
#'
#' ONE FRAME FOR EVERY TABLE, AND IT IS HERS. Ann-Kristin's fragments set the row height at 1.5, rule
#' the table with a double line on top and a single one below, and give every column a fixed
#' width -- the label column wide, the number columns 20 mm each and right-aligned -- so that a
#' table has the same shape whatever its numbers. Every fragment this document writes is rebuilt
#' into that frame here, and the widths are decided by what fits:
#'
#'   1. FIXED WIDTHS when they fit: number columns at .col_mm, narrowed down to .col_min if the
#'      label column would otherwise fall under .label_min; the label column takes the rest.
#'   2. NATURAL WIDTH when fixed columns would squeeze the label below .label_min: the tabular
#'      sizes itself from its content, rules and stretch as in 1.
#'   3. SCALED when the natural width would exceed the text block anyway -- more than .max_natural
#'      number columns -- with resizebox from graphicx.
#'
#' The type size is not set here: the manuscript's table environment sets it around the input.
#' Nothing beyond booktabs and graphicx is needed in the preamble. The text width is taken as
#' .text_mm for the arithmetic only; the fragment itself uses \\textwidth.
#'
#' @param .lines Character. The fragment as the writer built it: an optional \\begingroup\\size
#'   line, \\begin{tabular}{spec} ... \\end{tabular}, an optional \\endgroup.
#' @param .path Character. The .tex written.
#' @param .stretch Numeric. \\arraystretch; 1.5 is hers.
#' @param .col_mm,.col_min Numeric. Target and minimum width of a number column, millimetres.
#' @param .label_min Numeric. Minimum width of the label column, millimetres.
#' @param .max_natural Integer. Number columns beyond which the table is scaled.
#' @param .text_mm Numeric. The text width assumed for the arithmetic, millimetres.
#' @return Invisibly, the path.
# -- 5.1 The frame every tabular is written in --------------------------------------------------------------------
# THE PAPER'S FRAME (17 September, from Ann-Kristin's note): row spacing 1.2, a tabular* that fills \columnwidth,
# \hline rules only, headers centred, and no size command in the file -- the float sets \footnotesize. Nothing is
# scaled: a table that would run past the text block has its column padding cut first, and it is reported when even
# the tightest padding does not bring it inside. The widths below are the character widths of the manuscript's font
# at \footnotesize, measured in the interact class; a table's width is estimated from them before it is written,
# and the estimate runs one to four percent under the truth, which the margin covers.
.fin_tex_target <- 408      # \columnwidth of the manuscript, in points
.fin_tex_margin <- 0.04     # the safety margin on the estimate, which runs up to four percent under
.fin_tex_pads   <- c(6, 4, 3, 2)   # \tabcolsep in points, from LaTeX's default down
.fin_tex_lab_min <- 55      # the narrowest a wrapped column may become, in points
.fin_tex_mm      <- 2.845   # points per millimetre, where a specification names one
.fin_tex_em     <- 8.50     # 1em at \footnotesize, in points

# Character widths in points, upright.
.fin_tex_pt <- c(
  "0" = 4.25, "1" = 4.25, "2" = 4.25, "3" = 4.25, "4" = 4.25, "5" = 4.25, "6" = 4.25, "7" = 4.25, "8" = 4.25,
  "9" = 4.25, "," = 2.36, "." = 2.36, "%" = 7.08, "(" = 3.31, ")" = 3.31, "/" = 4.25, "-" = 2.83, "+" = 6.61,
  " " = 2.83, "a" = 4.25, "b" = 4.72, "c" = 3.78, "d" = 4.72, "e" = 3.78, "f" = 2.60, "g" = 4.25, "h" = 4.72,
  "i" = 2.36, "j" = 2.60, "k" = 4.49, "l" = 2.36, "m" = 7.08, "n" = 4.72, "o" = 4.25, "p" = 4.72, "q" = 4.49,
  "r" = 3.31, "s" = 3.35, "t" = 3.31, "u" = 4.72, "v" = 4.49, "w" = 6.14, "x" = 4.49, "y" = 4.49, "z" = 3.78,
  "A" = 6.37, "B" = 6.02, "C" = 6.14, "D" = 6.49, "E" = 5.78, "F" = 5.54, "G" = 6.67, "H" = 6.37, "I" = 3.06,
  "J" = 4.36, "K" = 6.60, "L" = 5.31, "M" = 7.78, "N" = 6.37, "O" = 6.61, "P" = 5.78, "Q" = 6.61, "R" = 6.25,
  "S" = 4.72, "T" = 6.14, "U" = 6.37, "V" = 6.37, "W" = 8.73, "X" = 6.37, "Y" = 6.37, "Z" = 5.19
)

# The same, bold: category and total rows are set in bold.
.fin_tex_pt_bold <- c(
  "0" = 4.90, "1" = 4.90, "2" = 4.90, "3" = 4.90, "4" = 4.90, "5" = 4.90, "6" = 4.90, "7" = 4.90, "8" = 4.90,
  "9" = 4.90, "," = 2.72, "." = 2.72, "%" = 8.17, "(" = 3.81, ")" = 3.81, "/" = 4.90, "-" = 3.27, "+" = 7.62,
  " " = 3.27, "a" = 4.76, "b" = 5.44, "c" = 4.36, "d" = 5.44, "e" = 4.49, "f" = 3.94, "g" = 5.04, "h" = 5.44,
  "i" = 2.72, "j" = 2.99, "k" = 5.17, "l" = 2.72, "m" = 8.17, "n" = 5.44, "o" = 4.90, "p" = 5.44, "q" = 5.17,
  "r" = 4.05, "s" = 3.87, "t" = 3.81, "u" = 5.44, "v" = 5.31, "w" = 7.21, "x" = 5.17, "y" = 5.31, "z" = 4.36,
  "A" = 7.38, "B" = 6.96, "C" = 7.08, "D" = 7.50, "E" = 6.42, "F" = 6.15, "G" = 7.70, "H" = 7.64, "I" = 3.67,
  "J" = 5.05, "K" = 7.65, "L" = 5.88, "M" = 9.27, "N" = 7.64, "O" = 7.36, "P" = 6.68, "Q" = 7.36, "R" = 7.32,
  "S" = 5.44, "T" = 6.82, "U" = 7.51, "V" = 7.51, "W" = 10.24, "X" = 7.38, "Y" = 7.62, "Z" = 5.99
)

.fin_tex_pt_other      <- mean(.fin_tex_pt[letters])        # what an unlisted character is taken to be
.fin_tex_pt_other_bold <- mean(.fin_tex_pt_bold[letters])

#' The width one cell asks for, in points
#'
#' The cell's LaTeX is reduced to the characters that print: \\textbf{} sets the bold metrics, \\hspace{1em} adds an
#' em, escapes become their character, and every other macro is dropped.
#'
#' @param .cell Character. One cell of a row, as it stands in the fragment.
#' @param .longest Logical. TRUE returns the widest word in the cell rather than the width of all of it.
#' @return Numeric. The width in points.
fin_tex_cell_pt <- function(.cell, .longest = FALSE) {
  if (FALSE) {
    .cell    <- "\\hspace{1em}Customer / Supplier"
    .longest <- FALSE
  }
  s_     <- trimws(.cell)
  bold_  <- grepl("\\\\textbf\\{", s_)
  n_em_  <- length(gregexpr("\\\\hspace\\{1em\\}", s_)[[1L]][gregexpr("\\\\hspace\\{1em\\}", s_)[[1L]] > 0L])
  s_     <- gsub("\\\\hspace\\{1em\\}", "", s_)
  s_     <- gsub("\\\\(%|&|\\$|#|_)", "\\1", s_)
  s_     <- gsub("\\\\[a-zA-Z]+\\*?", "", s_)
  s_     <- gsub("[{}]", "", s_)
  chars_ <- strsplit(s_, "")[[1L]]
  if (length(chars_) == 0L) return(n_em_ * .fin_tex_em)
  tab_   <- if (bold_) .fin_tex_pt_bold else .fin_tex_pt
  w_     <- unname(tab_[chars_])
  w_[is.na(w_)] <- if (bold_) .fin_tex_pt_other_bold else .fin_tex_pt_other
  if (!.longest) return(sum(w_) + n_em_ * .fin_tex_em)
  # THE WIDEST WORD is what a column can never be narrower than: a word does not break across lines.
  runs_ <- split(w_, cumsum(chars_ == " "))
  max(vapply(runs_, sum, numeric(1L))) + n_em_ * .fin_tex_em
}

#' The width a tabular asks for, in points
#'
#' Every column is as wide as its widest cell; a \\multicolumn wider than the columns it spans pushes them apart.
#' The padding is LaTeX's, twice per column.
#'
#' @param .rows Character. The body rows of the tabular, rules included; rules are skipped.
#' @param .ncol Integer. Columns in the tabular, the label column counted.
#' @param .pad Numeric. \\tabcolsep in points.
#' @param .fixed Numeric or NULL. The width a specification fixes for a column, NA where the column is free.
#' @param .columns Logical. TRUE returns the column widths instead of their sum with the padding.
#' @param .longest Logical. TRUE measures the widest word of a cell rather than the whole cell.
#' @return Numeric. The width in points.
fin_tex_est_width <- function(.rows, .ncol, .pad, .fixed = NULL, .columns = FALSE, .longest = FALSE) {
  if (FALSE) {
    .rows    <- c("a & 1 \\\\", "b & 2 \\\\")
    .ncol    <- 2L
    .pad     <- 6
    .fixed   <- NULL
    .columns <- FALSE
    .longest <- FALSE
  }
  cols_  <- rep(0, .ncol)
  spans_ <- list()
  for (row_ in .rows) {
    if (grepl("^\\\\\\\\(hline|midrule|toprule|bottomrule|cmidrule|addlinespace)", trimws(row_))) next
    cells_ <- strsplit(sub("\\\\\\\\\\s*$", "", row_), "(?<!\\\\)&", perl = TRUE)[[1L]]
    j_ <- 1L
    for (cell_ in cells_) {
      mc_ <- regmatches(cell_, regexec("^\\s*\\\\multicolumn\\{([0-9]+)\\}\\{[^}]*\\}\\{(.*)\\}\\s*$", cell_))[[1L]]
      if (length(mc_) == 3L) {
        spans_[[length(spans_) + 1L]] <- list(
          J = j_,
          K = as.integer(mc_[2L]),
          W = fin_tex_cell_pt(.cell = mc_[3L], .longest = .longest)
        )
        j_ <- j_ + as.integer(mc_[2L])
      } else {
        if (j_ <= .ncol) cols_[j_] <- max(cols_[j_], fin_tex_cell_pt(.cell = cell_, .longest = .longest))
        j_ <- j_ + 1L
      }
    }
  }
  for (s_ in spans_) {
    idx_  <- seq.int(s_$J, min(s_$J + s_$K - 1L, .ncol))
    have_ <- sum(cols_[idx_]) + (s_$K - 1L) * 2 * .pad
    if (s_$W > have_) cols_[idx_] <- cols_[idx_] + (s_$W - have_) / s_$K
  }
  # A FIXED COLUMN IS AS WIDE AS IT SAYS, whatever its cells hold: what does not fit wraps inside it.
  if (!is.null(.fixed)) cols_[!is.na(.fixed)] <- .fixed[!is.na(.fixed)]
  if (.columns) return(cols_)
  sum(cols_) + 2 * .pad * .ncol
}

#' A tabular put into the paper's frame, with the padding that brings it inside the text block
#'
#' Rules become \\hline, the rules under a spanning header go, every header cell is centred, and the tabular becomes
#' a tabular* that fills \\columnwidth. The padding is the widest of .fin_tex_pads the table fits in. A table that
#' fits in none is scaled as a last resort and named in a warning: it has to lose columns or be split.
#'
#' @param .lines Character. One tabular, rules and all, as a fragment builder writes it.
#' @param .name Character. The stem, for the warning.
#' @return Character. The framed fragment.
fin_tex_fit <- function(.lines, .name = "table") {
  if (FALSE) {
    .lines <- c("\\begin{tabular}{l r}", "\\toprule", " & N \\\\", "\\midrule", "a & 1 \\\\",
                "\\bottomrule", "\\end{tabular}")
    .name  <- "Test"
  }
  body_ <- .lines[!grepl("^\\\\begingroup\\\\[a-z]+$", .lines) & !grepl("^\\\\endgroup$", .lines)]
  i_ <- grep("^\\\\begin\\{tabular\\}\\{", body_)
  j_ <- grep("^\\\\end\\{tabular\\}$", body_)
  if (length(i_) != 1L || length(j_) != 1L) cli::cli_abort("The fragment for {(.name)} is not one tabular.")
  spec_  <- sub("^\\\\begin\\{tabular\\}\\{(.*)\\}$", "\\1", body_[i_])
  # THE COLUMNS, as the specification gives them: l, c and r take the width of their widest cell, while a p column
  # has its width written into it and wraps what does not fit, which is how a column of prose stays narrow.
  toks_  <- regmatches(spec_, gregexpr(">\\{[^}]*\\}p\\{[^}]*\\}|p\\{[^}]*\\}|[lcr]", spec_))[[1L]]
  n_     <- length(toks_)
  fixed_ <- vapply(toks_, \(.t) {
    if (!grepl("^(>\\{[^}]*\\})?p\\{", .t)) return(NA_real_)
    share_ <- suppressWarnings(as.numeric(sub("^.*?([0-9.]+)\\\\(text|column)width.*$", "\\1", .t)))
    mm_    <- suppressWarnings(as.numeric(sub("^.*?([0-9.]+)mm.*$", "\\1", .t)))
    cm_    <- suppressWarnings(as.numeric(sub("^.*?([0-9.]+)cm.*$", "\\1", .t)))
    pt_ <- if (!is.na(share_)) share_ * .fin_tex_target else if (!is.na(cm_)) cm_ * 10 * .fin_tex_mm else
      if (!is.na(mm_)) mm_ * .fin_tex_mm else NA_real_
    pt_
  }, numeric(1L))
  # THE RULES ARE \hline, AS HER TABLES HAVE THEM, and the rules under a spanning header go.
  body_ <- sub("^\\\\toprule$", "\\\\hline\\\\hline", body_)
  body_ <- sub("^\\\\midrule$", "\\\\hline", body_)
  body_ <- sub("^\\\\bottomrule$", "\\\\hline", body_)
  drop_ <- grepl("^\\\\cmidrule", body_)
  body_ <- body_[!drop_]
  i_ <- grep("^\\\\begin\\{tabular\\}\\{", body_)
  j_ <- grep("^\\\\end\\{tabular\\}$", body_)
  # THE HEADER IS CENTRED over its column, which is where the header rows are: between the double rule and the
  # first single rule under it.
  top_  <- grep("^\\\\hline\\\\hline$", body_)
  mid_  <- grep("^\\\\hline$", body_)
  head_ <- if (length(top_) == 1L && any(mid_ > top_)) seq.int(top_ + 1L, min(mid_[mid_ > top_]) - 1L) else integer()
  for (k_ in head_) {
    cells_ <- strsplit(sub("\\\\\\\\\\s*$", "", body_[k_]), "(?<!\\\\)&", perl = TRUE)[[1L]]
    cells_ <- vapply(cells_, \(.c) {
      if (trimws(.c) == "" || grepl("^\\s*\\\\multicolumn", .c)) .c else paste0("\\multicolumn{1}{c}{", trimws(.c), "}")
    }, character(1L))
    body_[k_] <- paste0(paste(cells_, collapse = " & "), " \\\\")
  }
  rows_ <- body_[seq.int(i_ + 1L, j_ - 1L)]
  cols_ <- fin_tex_est_width(.rows = rows_, .ncol = n_, .pad = 0, .fixed = fixed_, .columns = TRUE)
  words_ <- fin_tex_est_width(.rows = rows_, .ncol = n_, .pad = 0, .columns = TRUE, .longest = TRUE)
  est_  <- vapply(.fin_tex_pads, \(.p) sum(cols_) + 2 * .p * n_, numeric(1L))
  fits_ <- which(est_ * (1 + .fin_tex_margin) <= .fin_tex_target)
  # COLUMNS OF PROSE WRAP BEFORE ANYTHING IS SCALED: the number columns keep the width they need, and what is left
  # is shared between the free text columns in proportion to the width each would have taken.
  wraps_ <- rep(NA_real_, n_)
  # A TEXT COLUMN MAY WRAP, whether the specification names its width or leaves it free; a number column never
  # does, so a figure is not broken across lines.
  free_  <- which((is.na(fixed_) & toks_ == "l") | (!is.na(fixed_) & !grepl("raggedleft|centering", toks_)))
  if (length(fits_) == 0L && length(free_) > 0L) {
    for (k_ in seq_along(.fin_tex_pads)) {
      room_ <- .fin_tex_target - sum(cols_[-free_]) * (1 + .fin_tex_margin) - 2 * .fin_tex_pads[[k_]] * n_
      if (room_ <= 0) next
      # A COLUMN NARROWER THAN THE FLOOR KEEPS THE WIDTH IT ASKS FOR; the wide ones share what is left, so a
      # narrow column is not starved by a proportional share.
      # EVERY COLUMN KEEPS ITS WIDEST WORD, or its whole width where that is narrower; what is left over goes to
      # the columns that would still like to be wider, in proportion to what they are missing.
      nat_  <- cols_[free_]
      min_  <- pmin(nat_, pmax(.fin_tex_lab_min, words_[free_]))
      if (sum(min_) <= room_) {
        want_  <- min_
        gap_   <- nat_ - min_
        extra_ <- room_ - sum(min_)
        if (extra_ > 0 && sum(gap_) > 0) want_ <- min_ + pmin(gap_, extra_ * gap_ / sum(gap_))
        wraps_[free_] <- round(want_, 1)
        fits_ <- k_
        break
      }
    }
  }
  pad_  <- if (length(fits_) > 0L) .fin_tex_pads[[min(fits_)]] else utils::tail(.fin_tex_pads, 1L)
  open_ <- c("\\begingroup",
             if (pad_ != 6) paste0("\\setlength{\\tabcolsep}{", pad_, "pt}"),
             "\\renewcommand*{\\arraystretch}{1.2}")
  for (k_ in which(!is.na(wraps_))) {
    toks_[[k_]] <- paste0(">{\\raggedright\\arraybackslash}p{", wraps_[[k_]], "pt}")
  }
  if (length(fits_) == 0L) {
    cli::cli_alert_warning(paste0(
      "{(.name)}: about {round(min(est_))} pt wide against {(.fin_tex_target)} pt of text block, even at the ",
      "tightest padding; scaled to fit. It has to lose columns or be split."
    ))
    # WHAT THE FRAME READ, so a table that scales unexpectedly can be traced without opening the fragment.
    cli::cli_alert_info(paste0(
      "{(.name)}: columns {paste(round(cols_), collapse = \' + \')} pt, of which ",
      "{sum(!is.na(fixed_))} of {n_} declared in the specification."
    ))
    body_[i_] <- paste0("\\begin{tabular}{", spec_, "}")
    out_ <- c(open_, "\\resizebox{\\columnwidth}{!}{%", body_, "}", "\\endgroup")
  } else {
    body_[i_] <- paste0("\\begin{tabular*}{\\columnwidth}{", toks_[1L], "@{\\extracolsep{\\fill}}",
                        paste(toks_[-1L], collapse = ""), "}")
    body_[j_] <- "\\end{tabular*}"
    out_ <- c(open_, body_, "\\endgroup")
  }
  out_
}

#' A tibble of cells put into the paper's frame, for the builders that hand over cells rather than lines
#'
#' The specification only says which columns are text and which are numbers; the widths are the frame's business. A
#' column named Panel is not a column: it opens an italic row across the table wherever its value changes, as the
#' appendix's own frame has it, and never reaches the cells. A long table is left to that frame, which breaks it
#' across pages.
#'
#' @param .tab Tibble. Character cells, in table order; a Panel column opens the groups.
#' @param .header Character. One header cell per column, the Panel column aside.
#' @param .spec Character or NULL. The old column specification; a raggedleft or centering column is a number.
#' @param .long Logical. TRUE hands the table to the appendix frame, which sets it as a longtable.
#' @param .name Character. The stem, for the warning.
#' @return Character. The framed fragment.
fin_tex_frame <- function(.tab, .header, .spec = NULL, .long = FALSE, .name = "table") {
  if (FALSE) {
    .tab    <- tibble::tibble(Row = "a", N = "1")
    .header <- c("", "N")
    .spec   <- NULL
    .long   <- FALSE
    .name   <- "Test"
  }
  if (isTRUE(.long)) {
    return(oa_frame_table(
      .tab    = .tab,
      .header = .header,
      .spec   = .spec,
      .long   = TRUE
    ))
  }
  panel_ <- if ("Panel" %in% names(.tab)) .tab$Panel else rep(NA_character_, nrow(.tab))
  cells_ <- dplyr::select(.tab, -dplyr::any_of("Panel"))
  if (length(.header) != ncol(cells_) || (!is.null(.spec) && length(.spec) != ncol(cells_))) {
    cli::cli_abort("{(.name)}: the specification, the header and the cells disagree on the number of columns.")
  }
  # A NUMBER COLUMN IS r; A COLUMN OF PROSE KEEPS THE WIDTH ITS SPECIFICATION GIVES IT, so running text wraps
  # inside it instead of stretching the table.
  toks_ <- if (is.null(.spec)) {
    c("l", rep("r", ncol(cells_) - 1L))
  } else {
    ifelse(grepl("raggedleft|centering", .spec), "r", .spec)
  }
  rows_ <- purrr::map_chr(seq_len(nrow(cells_)), \(.i) {
    paste0(paste(unlist(cells_[.i, ]), collapse = " & "), " \\\\")
  })
  # THE PANEL OPENS A ROW OF ITS OWN wherever it changes, spanning every column.
  open_ <- !is.na(panel_) & (seq_along(panel_) == 1L | panel_ != dplyr::lag(panel_, default = ""))
  body_ <- unlist(purrr::map(seq_along(rows_), \(.i) {
    if (open_[.i]) {
      c(paste0("\\multicolumn{", ncol(cells_), "}{l}{\\textit{", panel_[.i], "}} \\\\"), rows_[.i])
    } else {
      rows_[.i]
    }
  }))
  fin_tex_fit(
    .lines = c(
      paste0("\\begin{tabular}{", paste(toks_, collapse = " "), "}"),
      "\\toprule",
      paste0(paste(.header, collapse = " & "), " \\\\"),
      "\\midrule",
      body_,
      "\\bottomrule",
      "\\end{tabular}"
    ),
    .name  = .name
  )
}

#' Write a tabular into the paper's frame
#'
#' The frame is fin_tex_fit()'s; this writes it to disk under the exhibit's stem.
#'
#' @param .lines Character. One tabular, rules and all.
#' @param .path Path. Where the fragment goes.
#' @return Invisibly, the path.
fin_tex_write <- function(.lines, .path) {
  if (FALSE) {
    .lines <- c("\\begin{tabular}{l r}", "\\toprule", "a & 1 \\\\", "\\bottomrule", "\\end{tabular}")
    .path  <- fs::path(.lP$Output$DirTables, "Test.tex")
  }
  out_ <- fin_tex_fit(
    .lines = .lines,
    .name  = fs::path_ext_remove(fs::path_file(.path))
  )
  fs::dir_create(fs::path_dir(.path))
  writeLines(out_, .path)
  invisible(.path)
}

#' Write a booktabs tabular from a character tibble
#'
#' Cells are written as given -- the caller has already formatted numbers and blanked repeated
#' labels -- and escaped here. The alignment string is the caller's, because a description column
#' needs p{} and a numeric column r, and no rule derives that from the data.
#'
#' @param .tab Tibble; every column character.
#' @param .path Character. The .tex written.
#' @param .align Character. The tabular column specification, e.g. "p{3cm} p{3cm} p{9cm}".
#' @param .space_before Integer. Row numbers that get \\addlinespace above them.
#' @return Invisibly, the path.
fin_tex_tabular <- function(.tab, .path, .align, .space_before = integer(0)) {
  if (FALSE) {
    .tab          <- tibble::tibble(A = c("x", "", "y"), B = c("1", "2", "3"))
    .path         <- fs::path(.lP$Output$DirTables, "Test.tex")
    .align        <- "l r"
    .space_before <- 3L
  }
  cells_ <- .tab |>
    dplyr::mutate(dplyr::across(dplyr::everything(), \(.x) fin_tex_escape(as.character(.x)))) |>
    as.matrix()
  head_ <- paste(fin_tex_escape(names(.tab)), collapse = " & ")
  body_ <- apply(cells_, 1L, paste, collapse = " & ")
  body_ <- paste0(dplyr::if_else(seq_along(body_) %in% .space_before, "\\addlinespace\n", ""), body_, " \\\\")
  lines_ <- c(
    paste0("\\begin{tabular}{", .align, "}"),
    "\\toprule",
    paste0(head_, " \\\\"),
    "\\midrule",
    body_,
    "\\bottomrule",
    "\\end{tabular}"
  )
  fs::dir_create(fs::path_dir(.path))
  fin_tex_write(
    .lines = lines_,
    .path  = .path
  )
  invisible(.path)
}

#' Write an exhibit's note as one LaTeX paragraph
#' @param .text Character. The note, one string; sentences may be given as a vector and are joined.
#' @param .path Character. The .tex written.
#' @return Invisibly, the path.
fin_note_write <- function(.text, .path) {
  if (FALSE) {
    .text <- c("First sentence.", "Second sentence.")
    .path <- fs::path(.lP$Output$DirNotes, "Test.tex")
  }
  fs::dir_create(fs::path_dir(.path))
  writeLines(paste(.text, collapse = " "), .path)
  invisible(.path)
}

#' Write an exhibit's tibble under its name
#' @param .tab Tibble.
#' @param .name Character. The stem.
#' @param .dir Character. Output/Data.
#' @return Invisibly, the path.
fin_save_data <- function(.tab, .name, .dir) {
  if (FALSE) {
    .tab  <- tibble::tibble(A = 1L)
    .name <- "Test"
    .dir  <- .lP$Output$DirData
  }
  fs::dir_create(.dir)
  path_ <- fs::path(.dir, paste0(.name, ".parquet"))
  arrow::write_parquet(.tab, path_)
  invisible(path_)
}

#' Render a manuscript table into the runbook as html
#'
#' The commons' rendered path, forced: whatever mc.table_mode the document runs on, a manuscript
#' table is shown as a real table, never as a console block. Requires `results: asis` on the chunk.
#'
#' @param .tab Tibble to show.
#' @param .title Character. The caption; the table's name, not a number.
#' @param .notes Character or NULL. Unnamed notes go under the table as a general note.
#' @param .full_width Logical. TRUE for tables with a prose column.
#' @return Invisibly, .tab.
fin_show_table <- function(.tab, .title, .notes = NULL, .full_width = FALSE) {
  if (FALSE) {
    .tab        <- tibble::tibble(A = "x", B = "y")
    .title      <- "Test"
    .notes      <- "A note."
    .full_width <- TRUE
  }
  tbl_out(
    .tab        = .tab,
    .title      = .title,
    .notes      = .notes,
    .full_width = .full_width,
    .mode       = "kable"
  )
}


# 6. Table: contract categories --------------------------------------------------------------------------------------------

#' The contract categories: level 1, level 2 and the definition of each, as the manuscript prints them
#'
#' Hand-written, because it is the taxonomy's definition and not a computation. Class is 30's label
#' for the same category, so the table joins to every exhibit tibble that carries one, and Code is
#' the paper's number for it. Rows are in the revision's taxonomic order; the paper's numbered order
#' is a sort on Code.
#'
#' @return Tibble: Level1, Level2, Description, Class, Code.
fin_tab_categories <- function() {
  out_ <- tibble::tribble(
    ~Level1,                 ~Level2,               ~Class,                                        ~Code,
    "Financial Instruments", "Credit",              "Financial Instruments: Credit",               5L,
    "Financial Instruments", "Equity",              "Financial Instruments: Equity",               6L,
    "Employment",            "Compensation",        "Employment: Compensation",                    3L,
    "Employment",            "Legal",               "Employment: Legal",                           4L,
    "Purchases and Sales",   "Assets",              "Purchases and Sales: Assets",                 9L,
    "Purchases and Sales",   "R&D",                 "R&D",                                         11L,
    "Purchases and Sales",   "Customer / Supplier", "Customer / Supplier",                         10L,
    "Licenses",              "",                    "Licenses",                                    8L,
    "Leases",                "",                    "Leases",                                      7L,
    "Business Structure",    "Peer Agreements",     "Business Structure: Peer Agreements",         2L,
    "Business Structure",    "M&A",                 "Business Structure: Investment and Merger",   1L,
    "Other",                 "",                    "Other",                                       12L
  )
  desc_ <- c(
    paste("Credit and debt contracts filed by the firm, including credit and loan agreements, waivers,",
          "loan amendments, letter agreements, guarantees, and promissory notes."),
    paste("Equity-related agreements, such as warrant, subscription, stock purchase, and registration rights",
          "agreements, and agreements governing common or preferred stock and shareholder voting rights."),
    paste("Employee and executive compensation agreements, including stock option and incentive plans, bonus",
          "plans, deferred compensation, and defined contribution plans."),
    paste("Employment contracts and related agreements, including offer letters, retention, severance,",
          "non-compete, separation, and change-in-control agreements."),
    paste("Contracts for the purchase or sale of tangible or intangible assets (excluding inventory and",
          "services), such as asset purchase and receivables sale agreements."),
    "Research, development, and related consulting agreements.",
    paste("Agreements for the purchase or sale of inventory or operational services, such as supply,",
          "distribution, manufacturing, procurement, vendor, and services agreements."),
    "License and royalty agreements for intellectual property, technology, or products.",
    "All lease agreements and related participation agreements.",
    paste("Collaborative agreements between firms, such as joint ventures, alliances, strategic partnering,",
          "collaboration, co-branding, and limited partnership agreements."),
    paste("Contracts related to mergers, acquisitions, or other business combinations, including agreements",
          "that stem from a merger, such as tax matters agreements."),
    "Legal agreements not captured by other categories, such as settlements, indemnities, or custom arrangements."
  )
  out_ <- dplyr::mutate(out_, Description = desc_, .after = "Level2")

  unknown_ <- setdiff(out_$Class, .fin_class_levels)
  if (length(unknown_) > 0L) cli::cli_abort("Categories table names {?a class/classes} 30 does not register: {unknown_}.")
  if (!setequal(out_$Code, 1:12)) cli::cli_abort("Categories table does not number the twelve classes once each.")
  out_
}

#' The categories table: built, saved, written as tex and note, shown
#'
#' The one call the runbook makes. Level 1 is printed on its first row only, as the manuscript does;
#' under the paper's order the groups stay contiguous, so the blanking holds either way.
#'
#' @param .order Character. "taxonomic" as the revision prints it, or "paper" for the numbered order.
#' @param .dirs List. .lP$Output: DirData, DirTables, DirNotes are used.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the tibble.
fin_table_categories <- function(.order = c("taxonomic", "paper"), .dirs, .name = "Categories") {
  if (FALSE) {
    .order <- "taxonomic"
    .dirs  <- .lP$Output
    .name  <- "Categories"
  }
  .order <- match.arg(.order)
  tab_ <- fin_tab_categories()
  if (.order == "paper") tab_ <- dplyr::arrange(tab_, .data$Code)
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )

  # LEVEL 1 ONCE PER GROUP. A repeated label is blanked below its first row; the rows that open a
  # group get a little air above them in the tex.
  shown_ <- tab_ |>
    dplyr::mutate(
      First  = .data$Level1 != dplyr::lag(.data$Level1, default = ""),
      Level1 = dplyr::if_else(.data$First, .data$Level1, "")
    )
  first_ <- which(shown_$First)[-1L]
  shown_ <- dplyr::select(shown_, "Category level 1" = "Level1", "Category level 2" = "Level2", "Description")

  fin_tex_tabular(
    .tab          = shown_,
    .path         = fs::path(.dirs$DirTables, paste0(.name, ".tex")),
    .align        = "p{3.4cm} p{3.4cm} p{9.6cm}",
    .space_before = first_
  )

  note_ <- c(
    "This table defines the twelve contract categories.",
    "The categorization builds on Verrecchia and Weber (2006) and incorporates the schemes used in",
    "Boone et al. (2016), Bao et al. (2022), Thompson et al. (2023), and Bao et al. (2025).",
    "Categories are mutually exclusive; contracts are classified according to their primary obligation."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )

  fin_show_table(
    .tab        = shown_,
    .title      = "Contract categories",
    .notes      = paste(note_, collapse = " "),
    .full_width = TRUE
  )
  invisible(tab_)
}


# 7. Table: sample selection ---------------------------------------------------------------------------------------------
# Panel A is the descriptive ladder, counted from the release's own ladder columns, and reproduces
# the revision to the row. Panel B is the estimation ladder as 106 builds its samples: every
# registrant copy at steps 2-6, less the malformed, less the CIKs Compustat does not cover, less the
# rows whose fiscal quarter is missing a control (one rung in the manuscript, several rules in the
# code), then the two start years and the firm-level sample from 105.

# THE CONTROL SETS THAT DEFINE "MISSING DATA". Resubmission is what her 105 and 106 estimate on in
# September 2026 -- the earlier Full set plus product-market fluidity, which is what shrinks the sample.
# Revision is the set that reproduced the revision's Table 4 means; it is kept as the switch that
# shows how much of the difference is the control set and how much the panel vintage.
.fin_controls <- list(
  Resubmission = c("Loss", "ChgSalesQ4Win", "Fluidity", "LogMve", "OcfWin", "AcqNeg", "AcqPos", "LeverageWin",
                   "GoodwillImp", "EqIssue", "DebtIssue", "RoaWin", "MbWin"),
  Revision     = .fin_control_sets$Filing
)

#' Panel B: the estimation ladder, contracts, quarters and firms at each rung
#'
#' Rows are counted as 106 counts them: registrant copies, firms as distinct CIKs, quarters as
#' distinct gvkey-fiscal-quarters among the rows that have one. The quarter's completeness is
#' fin_quarter_samples()' Complete -- the controls, the fiscal quarter, the industry and the state
#' present, and the state one in which filing varies -- joined onto the contract rows, which is what
#' 106's markout plus its state rule amount to. The firm-level row is T4_Trimmed on the same controls.
#'
#' @param .con Tibble. Contract rows: SampleStepCode, PrimaryFiler, CIK, gvkey, fyear, fqtr, Year.
#' @param .qtr Tibble. The quarter panel inside the window, every column fin_quarter_samples() needs.
#' @param .controls Character. The control set.
#' @return Tibble: Rung, Contracts, Primary, Quarters, Firms; Contracts and Primary NA on the
#'   firm-level row.
fin_tab_sample_b <- function(.con, .qtr, .controls) {
  if (FALSE) {
    .con      <- lst_ds$Contracts |>
      dplyr::select("SampleStepCode", "PrimaryFiler", "CIK", "gvkey", "fyear", "fqtr", "Year") |>
      dplyr::collect()
    .qtr      <- lst_ds$Quarter |> dplyr::filter(.data$S5_Quarter) |> dplyr::collect()
    .controls <- .fin_controls$Resubmission
  }
  qs_ <- fin_quarter_samples(
    .quarter     = .qtr,
    .controls    = .controls,
    .firm        = "cik",
    .drop_states = TRUE
  )
  ok_ <- qs_ |>
    dplyr::filter(.data$Complete) |>
    dplyr::distinct(.data$gvkey, .data$fyear, .data$fqtr) |>
    dplyr::mutate(Complete = TRUE)

  rows_ <- .con |>
    dplyr::filter(.data$SampleStepCode >= 2L) |>
    dplyr::left_join(
      ok_,
      by           = dplyr::join_by(gvkey, fyear, fqtr),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(Complete = dplyr::coalesce(.data$Complete, FALSE))

  rung_ <- function(.d, .name) {
    q_ <- dplyr::filter(.d, !is.na(.data$gvkey))
    tibble::tibble(
      Rung      = .name,
      Contracts = nrow(.d),
      Primary   = sum(.d$PrimaryFiler == 1L),
      Quarters  = if (nrow(q_) == 0L) NA_integer_ else nrow(dplyr::distinct(q_, .data$gvkey, .data$fyear, .data$fqtr)),
      Firms     = dplyr::n_distinct(.d$CIK)
    )
  }
  s1_ <- rows_
  s2_ <- dplyr::filter(s1_, .data$SampleStepCode >= 3L)
  s3_ <- dplyr::filter(s2_, .data$SampleStepCode >= 4L)
  s4_ <- dplyr::filter(s3_, .data$SampleStepCode == 6L, .data$Complete)

  firm_ <- qs_[qs_$T4_Trimmed, , drop = FALSE]

  dplyr::bind_rows(
    rung_(s1_, "Full sample"),
    rung_(s2_, "Less malformatted documents"),
    rung_(s3_, "Less missing Compustat match"),
    rung_(s4_, "Less missing data: contract-level sample"),
    rung_(dplyr::filter(s4_, .data$Year >= 2005L), "from 2005"),
    rung_(dplyr::filter(s4_, .data$Year >= 2008L), "from 2008"),
    tibble::tibble(
      Rung      = "Firm-level sample (incl. non-filing quarters)",
      Contracts = NA_integer_,
      Primary   = NA_integer_,
      Quarters  = nrow(firm_),
      Firms     = dplyr::n_distinct(firm_$cik)
    )
  )
}

#' Both panels as the manuscript prints them: one row per line, exclusions as negative differences
#'
#' @param .ladder_a Tibble from fin_table_ladder().
#' @param .ladder_b Tibble from fin_tab_sample_b().
#' @return Tibble: Panel, Row, Kind (total / exclusion / subsample / firm), Contracts, Quarters, Firms,
#'   Primary (Panel B only).
fin_tab_sample_shape <- function(.ladder_a, .ladder_b) {
  if (FALSE) {
    .ladder_a <- fin_table_ladder(.tab = con_)
    .ladder_b <- fin_tab_sample_b(
      .con      = con_,
      .qtr      = qtr_,
      .controls = .fin_controls$Resubmission
    )
  }
  a_ <- tibble::tibble(
    Panel     = "Panel A: Unique contracts",
    Row       = c("Full sample", "Malformatted documents", "Multiple filer", "Unique contracts"),
    Kind      = c("total", "exclusion", "exclusion", "total"),
    Contracts = .ladder_a$Contracts,
    Quarters  = NA_integer_,
    Firms     = .ladder_a$Firms,
    Primary   = NA_integer_
  )
  b_ <- .ladder_b
  diff_ <- function(.x, .i) .x[.i] - .x[.i - 1L]
  b_rows_ <- tibble::tibble(
    Panel     = "Panel B: Estimation samples",
    Row       = c("Full sample", "Malformatted documents", "Missing Compustat match", "Missing data",
                  "Contract-level sample", "from 2005", "from 2008", "Firm-level sample (incl. non-filing quarters)"),
    Kind      = c("total", "exclusion", "exclusion", "exclusion", "total", "subsample", "subsample", "firm"),
    Contracts = c(b_$Contracts[1L], diff_(b_$Contracts, 2L), diff_(b_$Contracts, 3L), diff_(b_$Contracts, 4L),
                  b_$Contracts[4L], b_$Contracts[5L], b_$Contracts[6L], NA_integer_),
    Quarters  = c(NA_integer_, NA_integer_, NA_integer_, diff_(b_$Quarters, 4L),
                  b_$Quarters[4L], b_$Quarters[5L], b_$Quarters[6L], b_$Quarters[7L]),
    Firms     = c(b_$Firms[1L], diff_(b_$Firms, 2L), diff_(b_$Firms, 3L), diff_(b_$Firms, 4L),
                  b_$Firms[4L], b_$Firms[5L], b_$Firms[6L], b_$Firms[7L]),
    Primary   = c(b_$Primary[1L], diff_(b_$Primary, 2L), diff_(b_$Primary, 3L), diff_(b_$Primary, 4L),
                  b_$Primary[4L], b_$Primary[5L], b_$Primary[6L], NA_integer_)
  )
  dplyr::bind_rows(a_, b_rows_)
}

#' Format a count column for the page: thousands separated, missing blank
#' @param .x Numeric.
#' @return Character.
fin_fmt_count <- function(.x) {
  if (FALSE) .x <- c(1436667L, -31289L, NA)
  dplyr::if_else(is.na(.x), "", formatC(.x, format = "d", big.mark = ","))
}

#' Write the sample-selection tabular: panel headers, indented exclusions, bold totals
#' @param .tab Tibble from fin_tab_sample_shape().
#' @param .path Character. The .tex written.
#' @return Invisibly, the path.
fin_tex_sample <- function(.tab, .path) {
  if (FALSE) {
    .tab  <- fin_tab_sample_shape(
      .ladder_a = ladder_a_,
      .ladder_b = ladder_b_
    )
    .path <- fs::path(.lP$Output$DirTables, "SampleSelection.tex")
  }
  cell_ <- function(.label, .c, .q, .f, .kind) {
    lab_ <- fin_tex_escape(.label)
    lab_ <- switch(.kind,
      total     = paste0("\\textbf{", lab_, "}"),
      exclusion = paste0("\\hspace{1em}", lab_),
      subsample = paste0("\\hspace{0.5em}", lab_),
      firm      = paste0("\\textit{", lab_, "}")
    )
    num_ <- c(.c, .q, .f)
    if (.kind == "total") num_ <- dplyr::if_else(nzchar(num_), paste0("\\textbf{", num_, "}"), num_)
    if (.kind == "firm")  num_ <- dplyr::if_else(nzchar(num_), paste0("\\textit{", num_, "}"), num_)
    paste0(paste(c(lab_, num_), collapse = " & "), " \\\\")
  }
  body_ <- character(0)
  for (p_ in unique(.tab$Panel)) {
    rows_ <- .tab[.tab$Panel == p_, , drop = FALSE]
    if (length(body_) > 0L) body_ <- c(body_, "\\addlinespace[1.5ex]")
    body_ <- c(body_, paste0("\\multicolumn{4}{l}{\\textbf{", fin_tex_escape(p_), "}} \\\\"))
    body_ <- c(body_, purrr::pmap_chr(
      list(rows_$Row, fin_fmt_count(rows_$Contracts), fin_fmt_count(rows_$Quarters), fin_fmt_count(rows_$Firms),
           rows_$Kind),
      cell_
    ))
  }
  lines_ <- c(
    "\\begin{tabular}{l r r r}",
    "\\toprule",
    " & Contracts & Quarters & Firms \\\\",
    "\\midrule",
    body_,
    "\\bottomrule",
    "\\end{tabular}"
  )
  fs::dir_create(fs::path_dir(.path))
  fin_tex_write(
    .lines = lines_,
    .path  = .path
  )
  invisible(.path)
}

#' Write a kable into the runbook with its footnote repaired and left-aligned
#'
#' The commons' repair -- a real colspan on the footnote row, no row rules -- plus text-align: left
#' on that row: a spanning cell inherits no alignment from the column it starts in and is otherwise
#' centred. Emitted inside the same raw-html fence tbl_out() uses; the chunk needs `results: asis`.
#'
#' @param .kable A kable with its footnote attached.
#' @param .ncol Integer. Columns the footnote spans.
#' @return Invisibly, NULL.
fin_emit_kable <- function(.kable, .ncol) {
  if (FALSE) {
    .kable <- knitr::kable(utils::head(iris), format = "html") |> kableExtra::footnote(general = "a note")
    .ncol  <- 5L
  }
  k_ <- tbl_fix_footnote(
    .kable = .kable,
    .ncol  = .ncol
  )
  txt_ <- gsub('style="padding: 0; border: 0;"', 'style="padding: 0; border: 0; text-align: left;"', as.character(k_),
               fixed = TRUE)
  txt_ <- gsub("style='padding: 0; border: 0;'", "style='padding: 0; border: 0; text-align: left;'", txt_, fixed = TRUE)
  cat("\n```{=html}\n", txt_, "\n```\n\n", sep = "")
  invisible(NULL)
}

#' Show a panelled count table in the runbook: panel rows spanning, exclusions indented, totals bold
#'
#' Built directly on kableExtra because the commons' path has no panel grouping. Emitted inside the
#' same raw-html fence tbl_out() uses; the chunk needs `results: asis`.
#'
#' @param .tab Tibble from fin_tab_sample_shape().
#' @param .title Character. Caption.
#' @param .note Character. General note under the table.
#' @return Invisibly, .tab.
fin_show_sample <- function(.tab, .title, .note) {
  if (FALSE) {
    .tab   <- fin_tab_sample_shape(
      .ladder_a = ladder_a_,
      .ladder_b = ladder_b_
    )
    .title <- "Sample selection"
    .note  <- "A note."
  }
  shown_ <- tibble::tibble(
    ` `       = .tab$Row,
    Contracts = fin_fmt_count(.tab$Contracts),
    Quarters  = fin_fmt_count(.tab$Quarters),
    Firms     = fin_fmt_count(.tab$Firms)
  )
  idx_ <- purrr::map(unique(.tab$Panel), \(.p) which(.tab$Panel == .p))
  k_ <- shown_ |>
    knitr::kable(
      format   = "html",
      escape   = TRUE,
      caption  = .title,
      align    = "lrrr",
      booktabs = TRUE
    ) |>
    kableExtra::kable_styling(
      full_width        = FALSE,
      position          = "left",
      bootstrap_options = c("hover", "condensed")
    ) |>
    kableExtra::row_spec(which(.tab$Kind == "total"), bold = TRUE) |>
    kableExtra::row_spec(which(.tab$Kind == "firm"), italic = TRUE) |>
    kableExtra::add_indent(which(.tab$Kind %in% c("exclusion", "subsample"))) |>
    kableExtra::footnote(
      general           = .note,
      general_title     = "Notes:",
      footnote_as_chunk = TRUE,
      threeparttable    = TRUE
    )
  # PANEL HEADERS SPAN THE TABLE AND SIT LEFT. A spanning cell inherits no alignment from the column
  # it starts in, so without the css it is centred, which is what the manuscript does not do.
  for (i_ in seq_along(idx_)) {
    k_ <- kableExtra::pack_rows(
      kable_input   = k_,
      group_label   = unique(.tab$Panel)[i_],
      start_row     = min(idx_[[i_]]),
      end_row       = max(idx_[[i_]]),
      label_row_css = "text-align: left; border-bottom: 1px solid;"
    )
  }
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(shown_)
  )
  invisible(.tab)
}

#' The sample-selection table: built from the prepared tables, saved, written as tex and note, shown
#'
#' Panel A is exact against the revision and aborts otherwise (30's check, re-run here on the
#' prepared columns). Panel B is the resubmission's ladder on the control set given; the revision's
#' numbers are not reproduced by design, because the control set and the panel have both moved.
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .ds_quarter The prepared Quarter dataset.
#' @param .controls Character. The control set that defines "missing data"; .fin_controls names two.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the shaped tibble.
fin_table_sample <- function(.ds_contracts, .ds_quarter, .controls = .fin_controls$Resubmission, .dirs,
                             .name = "SampleSelection") {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .ds_quarter   <- lst_ds$Quarter
    .controls     <- .fin_controls$Resubmission
    .dirs         <- .lP$Output
    .name         <- "SampleSelection"
  }
  con_ <- .ds_contracts |>
    dplyr::select("SampleStepCode", "PrimaryFiler", "CIK", "gvkey", "fyear", "fqtr", "Year") |>
    dplyr::collect()
  miss_ <- setdiff(.controls, names(.ds_quarter))
  if (length(miss_) > 0L) cli::cli_abort("The quarter panel lacks control{?s} {miss_}.")
  qtr_ <- .ds_quarter |>
    dplyr::filter(.data$S5_Quarter) |>
    dplyr::select("gvkey", "fyear", "fqtr", "cik", "Filed", "State", "FfIndLab", "PostFast", dplyr::all_of(.controls)) |>
    dplyr::collect()

  ladder_a_ <- fin_table_ladder(.tab = con_)
  fin_check_reference(
    .tab_ladder = ladder_a_,
    .ref        = .fin_reference
  )
  ladder_b_ <- fin_tab_sample_b(
    .con      = con_,
    .qtr      = qtr_,
    .controls = .controls
  )
  tab_ <- fin_tab_sample_shape(
    .ladder_a = ladder_a_,
    .ladder_b = ladder_b_
  )
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  fin_tex_sample(
    .tab  = tab_,
    .path = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  note_ <- c(
    "This table illustrates the sample selection. The columns show the number of excluded and remaining",
    "material contracts, firm-quarters, and firms after each sample selection step. Panel A shows the",
    "descriptive sample, while Panel B shows the estimation sample. The contract-level sample contains all",
    "contracts that can be merged to the Compustat database and for which the regression variables are",
    "available in the respective quarter. The firm-level sample further includes all non-filing quarters",
    "with complete regression variables of firms that filed at least one contract during the sample period,",
    "and excludes firms whose filing does not vary within a FAST Act period."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  fin_show_sample(
    .tab   = tab_,
    .title = "Sample selection",
    .note  = paste(note_, collapse = " ")
  )
  cli::cli_alert_info(
    "Panel B on {length(.controls)} controls; contracts are registrant copies, primary copies ride in the data \\
     file ({format(ladder_b_$Primary[4L], big.mark = ',')} at the contract-level rung)."
  )
  invisible(tab_)
}


# 8. Table: content characteristics by category ---------------------------------------------------------------------------
# Words, duration, parties, countries and states by category, on the unique-contract sample: N once,
# mean and SD per measure. Two arms from the same rows: naive is what the extractor proposed,
# rule-based is what the rules made of it, and the tables differ in exactly those definitions so a
# reader sees where the N and the level move. Rows follow the Categories table: super-category rows
# carry the pooled statistics, sub-categories sit indented beneath, Total last.

# THE ROLES A RULE-BASED PARTY CAN HAVE, and the release column that counts each. The recital roles
# are the default role set; parties, countries and states all use the same set so the three columns
# say one thing.
.fin_party_roles <- c(
  registrant   = "nUniRegistrant",
  cofiler      = "nUniCofiler",
  counterparty = "nUniCounterparty",
  signatory    = "nUniSignatory",
  other        = "nUniOther"
)
.fin_roles_recital <- c("registrant", "cofiler", "counterparty")

# THE TWO ARMS, column by column. The rule-based parties, countries and states are not release
# columns: parties are the sum of the role columns chosen, countries and states are counted from
# Places for places attached to a party of one of those roles.
.fin_content_arms <- list(
  naive = c(Words = "nWords",    Duration = "NaiveYears",    Parties = "nUniSpellingsNaive",
            Countries = "nUniCountryNaive", States = "nUniStateNaive"),
  rule  = c(Words = "nWordsAdj", Duration = "DurationYears", Parties = "PartiesRole",
            Countries = "CountriesRole",    States = "StatesRole")
)

#' Distinct countries and states attached to a party of the roles given, per contract, from Places
#'
#' A place counts when it sits outside a governing-law clause and the party it is attached to has
#' one of the roles. Contracts with no such place get no row; the caller coalesces to zero, because
#' a contract the extractor scanned and found nothing in has zero countries, not a missing value.
#'
#' @param .con A DuckDB connection with Places registered.
#' @param .roles Character. Party roles, lower case as Places carries them.
#' @return Tibble: DocID, CountriesRole, StatesRole.
fin_places_by_role <- function(.con, .roles = .fin_roles_recital) {
  if (FALSE) {
    .con   <- con
    .roles <- .fin_roles_recital
  }
  bad_ <- setdiff(.roles, names(.fin_party_roles))
  if (length(bad_) > 0L) cli::cli_abort("Unknown party role{?s}: {bad_}.")
  in_ <- paste0("'", .roles, "'", collapse = ", ")
  DBI::dbGetQuery(.con, paste0(
    "SELECT DocID, ",
    "  COUNT(DISTINCT GeoCountryIso) FILTER (WHERE GeoCountryIso IS NOT NULL) AS CountriesRole, ",
    "  COUNT(DISTINCT GeoState) FILTER (WHERE GeoState IS NOT NULL) AS StatesRole ",
    "FROM Places ",
    "WHERE InLawClause = 0 AND PartyRole IN (", in_, ") ",
    "GROUP BY DocID"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(c("CountriesRole", "StatesRole"), as.integer))
}

#' The five measures on one arm, for the unique-contract sample, with the category and its parent
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .con A DuckDB connection with Places registered (rule-based countries and states).
#' @param .arm Character. "naive" or "rule".
#' @param .roles Character. The party roles the rule-based parties, countries and states count.
#' @return Tibble, one row per S2 contract: DocID, Year, Class, Level1, Level2, Words, Duration, Parties,
#'   Countries, States. Class, Level1 and Level2 are NA on unlabelled contracts.
fin_content_rows <- function(.ds_contracts, .con, .arm = c("rule", "naive"), .roles = .fin_roles_recital) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .con          <- con
    .arm          <- "rule"
    .roles        <- .fin_roles_recital
  }
  .arm <- match.arg(.arm)
  map_ <- .fin_content_arms[[.arm]]
  cols_ <- c("DocID", "Year", "Class", "nWords", "nWordsAdj", "NaiveYears", "DurationYears", "nUniSpellingsNaive",
             "nUniRegistrant", "nUniCofiler", "nUniCounterparty", "nUniCountryNaive", "nUniStateNaive")
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S2_Descriptive) |>
    dplyr::select(dplyr::all_of(cols_)) |>
    dplyr::collect()
  if (.arm == "rule") {
    role_cols_ <- unname(.fin_party_roles[.roles])
    miss_ <- setdiff(role_cols_, names(rows_))
    if (length(miss_) > 0L) cli::cli_abort("Role column{?s} not in the prepared table: {miss_}.")
    rows_ <- rows_ |>
      dplyr::mutate(PartiesRole = rowSums(dplyr::pick(dplyr::all_of(role_cols_)))) |>
      dplyr::left_join(
        fin_places_by_role(
          .con   = .con,
          .roles = .roles
        ),
        by           = dplyr::join_by(DocID),
        relationship = "one-to-one"
      ) |>
      dplyr::mutate(dplyr::across(c("CountriesRole", "StatesRole"), \(.x) dplyr::coalesce(.x, 0L)))
  }
  cat_ <- fin_tab_categories() |>
    dplyr::select("Class", "Level1", "Level2")
  rows_ |>
    dplyr::left_join(
      cat_,
      by           = dplyr::join_by(Class),
      relationship = "many-to-one"
    ) |>
    dplyr::transmute(
      DocID     = .data$DocID,
      Year      = .data$Year,
      Class     = .data$Class,
      Level1    = .data$Level1,
      Level2    = .data$Level2,
      Words     = as.numeric(.data[[map_[["Words"]]]]),
      Duration  = as.numeric(.data[[map_[["Duration"]]]]),
      Parties   = as.numeric(.data[[map_[["Parties"]]]]),
      Countries = as.numeric(.data[[map_[["Countries"]]]]),
      States    = as.numeric(.data[[map_[["States"]]]])
    )
}

#' N, mean and SD of the five measures over a set of rows
#' @param .rows Tibble from fin_content_rows(), or a subset.
#' @return One-row tibble: N, then <Measure>Mean, <Measure>SD for Words, Dur, Part, Ctry, State, plus
#'   DurN (end established) and PartN, CtryN, StateN (at least one found).
fin_content_stats <- function(.rows) {
  if (FALSE) {
    .rows <- fin_content_rows(
      .ds_contracts = lst_ds$Contracts,
      .con          = con,
      .arm          = "rule"
    )
  }
  d_ <- .rows$Duration[!is.na(.rows$Duration)]
  tibble::tibble(
    N         = nrow(.rows),
    WordsMean = mean(.rows$Words, na.rm = TRUE),
    WordsSD   = stats::sd(.rows$Words, na.rm = TRUE),
    DurN      = length(d_),
    DurMean   = mean(d_),
    DurSD     = stats::sd(d_),
    PartMean  = mean(.rows$Parties, na.rm = TRUE),
    PartSD    = stats::sd(.rows$Parties, na.rm = TRUE),
    CtryMean  = mean(.rows$Countries, na.rm = TRUE),
    CtrySD    = stats::sd(.rows$Countries, na.rm = TRUE),
    StateMean = mean(.rows$States, na.rm = TRUE),
    StateSD   = stats::sd(.rows$States, na.rm = TRUE),
    # WHERE THE EXTRACTION FOUND SOMETHING. Counts are zero, not missing, where nothing was found;
    # these are what the observation contrast compares across arms.
    PartN     = sum(.rows$Parties > 0, na.rm = TRUE),
    CtryN     = sum(.rows$Countries > 0, na.rm = TRUE),
    StateN    = sum(.rows$States > 0, na.rm = TRUE)
  )
}

#' Rows in the Categories order for any per-contract table: super-categories pooled, sub-categories, Total
#'
#' The one row builder every category table uses, so the content tables and the parties table cannot
#' drift in structure. .stats is called on the pooled rows of each super-category, on each
#' sub-category, and on every row for Total.
#'
#' @param .rows Tibble with Class and Level1, one row per contract.
#' @param .stats Function of one tibble returning a one-row tibble.
#' @return Tibble: Row (label), Kind (super / sub / total), Level1, Class, then .stats' columns.
fin_tab_by_category <- function(.rows, .stats) {
  if (FALSE) {
    .rows  <- fin_content_rows(
      .ds_contracts = lst_ds$Contracts,
      .con          = con,
      .arm          = "rule"
    )
    .stats <- fin_content_stats
  }
  cat_ <- fin_tab_categories()
  out_ <- list()
  for (l1_ in unique(cat_$Level1)) {
    sub_ <- cat_[cat_$Level1 == l1_, , drop = FALSE]
    out_[[length(out_) + 1L]] <- dplyr::bind_cols(
      tibble::tibble(Row = l1_, Kind = "super", Level1 = l1_, Class = NA_character_),
      .stats(.rows[.rows$Level1 %in% l1_, , drop = FALSE])
    )
    if (nrow(sub_) > 1L) {
      for (i_ in seq_len(nrow(sub_))) {
        out_[[length(out_) + 1L]] <- dplyr::bind_cols(
          tibble::tibble(Row = sub_$Level2[i_], Kind = "sub", Level1 = l1_, Class = sub_$Class[i_]),
          .stats(.rows[.rows$Class %in% sub_$Class[i_], , drop = FALSE])
        )
      }
    }
  }
  out_[[length(out_) + 1L]] <- dplyr::bind_cols(
    tibble::tibble(Row = "Total", Kind = "total", Level1 = NA_character_, Class = NA_character_),
    .stats(.rows)
  )
  purrr::list_rbind(out_)
}

#' Contracts that carry a category: the sub rows plus the super rows that have no sub rows
#' @param .tab Tibble from fin_tab_by_category(), with Kind, Level1 and N.
#' @return Integer.
fin_n_classed <- function(.tab) {
  if (FALSE) .tab <- tab_content_rule
  single_ <- .tab$Kind == "super" & !.tab$Level1 %in% .tab$Level1[.tab$Kind == "sub"]
  as.integer(sum(.tab$N[.tab$Kind == "sub" | single_]))
}

#' Format a numeric column for the page: fixed decimals, thousands separated, missing blank
#' @param .x Numeric.
#' @param .d Integer. Decimals.
#' @return Character.
fin_fmt_num <- function(.x, .d) {
  if (FALSE) {
    .x <- c(8825.4, NA)
    .d <- 0L
  }
  dplyr::if_else(is.na(.x), "", formatC(.x, format = "f", digits = .d, big.mark = ","))
}

#' A category table's body as one tex line per row: bold super and total rows, indented sub rows
#'
#' @param .cells Tibble of character columns, the label first.
#' @param .kind Character. "super", "sub" or "total" per row.
#' @return Character, one tabular line per row, with a rule before Total.
fin_tex_category_body <- function(.cells, .kind) {
  if (FALSE) {
    .cells <- tibble::tibble(Row = c("A", "a1", "Total"), N = c("2", "1", "2"))
    .kind  <- c("super", "sub", "total")
  }
  line_ <- function(.i) {
    lab_ <- fin_tex_escape(.cells[[1L]][.i])
    lab_ <- switch(.kind[.i],
      super = paste0("\\textbf{", lab_, "}"),
      sub   = paste0("\\hspace{1em}", lab_),
      total = paste0("\\textbf{", lab_, "}")
    )
    num_ <- unlist(.cells[.i, -1L])
    if (.kind[.i] != "sub") num_ <- dplyr::if_else(nzchar(num_), paste0("\\textbf{", num_, "}"), num_)
    paste0(paste(c(lab_, num_), collapse = " & "), " \\\\")
  }
  body_ <- purrr::map_chr(seq_len(nrow(.cells)), line_)
  append(body_, "\\midrule", after = which(.kind == "total") - 1L)
}

#' A category table as html, with a spanning header, into the runbook
#'
#' @param .cells Tibble of character columns, the label first; names are the second header row.
#' @param .kind Character. "super", "sub" or "total" per row.
#' @param .groups Named integer. The spanning header, as add_header_above() takes it.
#' @param .title Character. Caption.
#' @param .note Character. General note.
#' @param .font_size Numeric. Point size.
#' @return Invisibly, NULL.
fin_show_category <- function(.cells, .kind, .groups, .title, .note, .font_size = 12) {
  if (FALSE) {
    .cells     <- tibble::tibble(` ` = c("A", "a1", "Total"), N = c("2", "1", "2"))
    .kind      <- c("super", "sub", "total")
    .groups    <- c(" " = 1, "Contracts" = 1)
    .title     <- "Test"
    .note      <- "A note."
    .font_size <- 12
  }
  k_ <- .cells |>
    knitr::kable(
      format   = "html",
      escape   = TRUE,
      caption  = .title,
      align    = paste0("l", strrep("r", ncol(.cells) - 1L)),
      booktabs = TRUE
    ) |>
    kableExtra::kable_styling(
      full_width        = FALSE,
      position          = "left",
      bootstrap_options = c("hover", "condensed"),
      font_size         = .font_size
    ) |>
    kableExtra::column_spec(
      column    = 1L,
      extra_css = "white-space: nowrap;"
    ) |>
    kableExtra::add_header_above(.groups) |>
    kableExtra::row_spec(which(.kind %in% c("super", "total")), bold = TRUE) |>
    kableExtra::row_spec(which(.kind == "total") - 1L, extra_css = "border-bottom: 1px solid #333;") |>
    kableExtra::add_indent(which(.kind == "sub")) |>
    kableExtra::footnote(
      general           = .note,
      general_title     = "Notes:",
      footnote_as_chunk = TRUE,
      threeparttable    = TRUE
    )
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(.cells)
  )
  invisible(NULL)
}

#' The unit the words columns are printed in: the header and the note's wording
#'
#' @param .scale Numeric. The divisor of the word counts: 1, or 1000 for thousands.
#' @return A list: Header (the spanning header's text) and Note (the unit in words, or NA for plain counts).
fin_words_unit <- function(.scale) {
  if (FALSE) .scale <- 1000
  if (.scale == 1) return(list(Header = "Words", Note = NA_character_))
  list(
    Header = paste0("Words (in ", format(.scale, big.mark = ",", scientific = FALSE), "s)"),
    Note   = if (.scale == 1000) "thousands" else paste0("units of ", format(.scale, big.mark = ",", scientific = FALSE))
  )
}

#' The content cells for the page: N as a count, every mean and SD with the decimals given
#'
#' Words are divided by .words_scale before they are formatted, so two decimals in thousands read as
#' 14.31 rather than 14,311. The duration N is not a column: it rides in the tibble and the note, so
#' the table stays on one line per row.
#'
#' @param .tab Tibble from fin_tab_by_category(.stats = fin_content_stats).
#' @param .digits Named integer. Decimals for Mean and SD.
#' @param .words_scale Numeric. The divisor of the word counts (1000: thousands).
#' @return Tibble of character columns in table order, Row first.
fin_content_cells <- function(.tab, .digits = c(Mean = 2L, SD = 2L), .words_scale = 1000) {
  if (FALSE) {
    .tab         <- tab_content_rule
    .digits      <- c(Mean = 2L, SD = 2L)
    .words_scale <- 1000
  }
  m_ <- .digits[["Mean"]]
  s_ <- .digits[["SD"]]
  tibble::tibble(
    Row       = .tab$Row,
    N         = fin_fmt_count(.tab$N),
    WordsMean = fin_fmt_num(.tab$WordsMean / .words_scale, m_),
    WordsSD   = fin_fmt_num(.tab$WordsSD / .words_scale,   s_),
    DurMean   = fin_fmt_num(.tab$DurMean,   m_),
    DurSD     = fin_fmt_num(.tab$DurSD,     s_),
    PartMean  = fin_fmt_num(.tab$PartMean,  m_),
    PartSD    = fin_fmt_num(.tab$PartSD,    s_),
    CtryMean  = fin_fmt_num(.tab$CtryMean,  m_),
    CtrySD    = fin_fmt_num(.tab$CtrySD,    s_),
    StateMean = fin_fmt_num(.tab$StateMean, m_),
    StateSD   = fin_fmt_num(.tab$StateSD,   s_)
  )
}

#' Write the content tabular: spanning headers, super rows bold, sub rows indented, Total set apart
#'
#' The tabular is wrapped in a size group so the fragment sets its own type size wherever the
#' manuscript includes it.
#'
#' @param .tab Tibble from fin_tab_by_category(.stats = fin_content_stats).
#' @param .path Character. The .tex written.
#' @param .digits Named integer. Passed to fin_content_cells().
#' @param .words_scale Numeric. Passed to fin_content_cells(); the header names the unit.
#' @param .size Character. A LaTeX size command without the backslash: "small", "footnotesize".
#' @return Invisibly, the path.
fin_tex_content <- function(.tab, .path, .digits = c(Mean = 2L, SD = 2L), .words_scale = 1000, .size = "small") {
  if (FALSE) {
    .tab         <- tab_content_rule
    .path        <- fs::path(.lP$Output$DirTables, "ContentRule.tex")
    .digits      <- c(Mean = 2L, SD = 2L)
    .words_scale <- 1000
    .size        <- "small"
  }
  cells_ <- fin_content_cells(
    .tab         = .tab,
    .digits      = .digits,
    .words_scale = .words_scale
  )
  body_ <- fin_tex_category_body(
    .cells = cells_,
    .kind  = .tab$Kind
  )
  lines_ <- c(
    paste0("\\begingroup\\", .size),
    "\\begin{tabular}{l r rr rr rr rr rr}",
    "\\toprule",
    paste0(" & Contracts & \\multicolumn{2}{c}{", fin_words_unit(.scale = .words_scale)$Header,
           "} & \\multicolumn{2}{c}{Duration (years)} &"),
    "   \\multicolumn{2}{c}{Parties} & \\multicolumn{2}{c}{Countries} & \\multicolumn{2}{c}{States} \\\\",
    paste("\\cmidrule(lr){2-2} \\cmidrule(lr){3-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-8}",
          "\\cmidrule(lr){9-10} \\cmidrule(lr){11-12}"),
    " & N & Mean & SD & Mean & SD & Mean & SD & Mean & SD & Mean & SD \\\\",
    "\\midrule",
    body_,
    "\\bottomrule",
    "\\end{tabular}",
    "\\endgroup"
  )
  fs::dir_create(fs::path_dir(.path))
  fin_tex_write(
    .lines = lines_,
    .path  = .path
  )
  invisible(.path)
}

#' The content table on one arm: built from the prepared tables, checked, saved, written, shown
#'
#' THE N IS CHECKED, NOT ASSUMED. The note states the sample and its size; the function prints the
#' size it was written on beside the size counted here and the sum of the category rows, and warns
#' if the first two differ. The category rows sum to less than the sample because contracts the
#' classifier left unlabelled are in the sample and in the Total row but in no category row.
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .con A DuckDB connection with Places registered.
#' @param .arm Character. "rule" or "naive".
#' @param .roles Character. The party roles the rule-based parties, countries and states count.
#' @param .dirs List. .lP$Output.
#' @param .name Character or NULL. The stem every file takes; NULL derives it from the arm.
#' @param .digits Named integer. Decimals for Mean and SD.
#' @param .words_scale Numeric. The divisor of the word counts (1000: thousands), named in the header and the note.
#' @param .ref Tibble. 30's .fin_reference; the sample size the note assumes.
#' @return Invisibly, the table tibble with Arm and Sample columns.
fin_table_content <- function(.ds_contracts, .con, .arm = c("rule", "naive"), .roles = .fin_roles_recital, .dirs,
                              .name = NULL, .digits = c(Mean = 2L, SD = 2L), .words_scale = 1000,
                              .ref = .fin_reference) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .con          <- con
    .arm          <- "rule"
    .roles        <- .fin_roles_recital
    .dirs         <- .lP$Output
    .name         <- NULL
    .digits       <- c(Mean = 2L, SD = 2L)
    .words_scale  <- 1000
    .ref          <- .fin_reference
  }
  .arm  <- match.arg(.arm)
  name_ <- if (is.null(.name)) paste0("Content", if (.arm == "rule") "Rule" else "Naive") else .name

  rows_ <- fin_content_rows(
    .ds_contracts = .ds_contracts,
    .con          = .con,
    .arm          = .arm,
    .roles        = .roles
  )
  tab_ <- fin_tab_by_category(
    .rows  = rows_,
    .stats = fin_content_stats
  ) |>
    dplyr::mutate(Arm = .arm, Sample = "S2_Descriptive", .before = 1L)

  # ASSUMED AGAINST ACTUAL. The note is written from the count here, never from memory; the
  # revision's number is printed beside it so a moved release is seen, not discovered.
  n_actual_  <- nrow(rows_)
  n_assumed_ <- .ref$Revision[.ref$Quantity == "Unique contracts"]
  n_classed_ <- fin_n_classed(.tab = tab_)
  n_unlab_   <- n_actual_ - n_classed_
  if (n_actual_ != n_assumed_) {
    cli::cli_alert_warning(
      "Unique contracts: {format(n_actual_, big.mark = ',')} on this release against the revision's \\
       {format(n_assumed_, big.mark = ',')}."
    )
  } else {
    cli::cli_alert_success("Unique contracts: {format(n_actual_, big.mark = ',')}, as the revision prints.")
  }
  cli::cli_alert_info(
    "Category rows sum to {format(n_classed_, big.mark = ',')}; {format(n_unlab_, big.mark = ',')} unlabelled \\
     contracts are in the Total row only."
  )
  tot_   <- tab_[tab_$Kind == "total", , drop = FALSE]
  pct_   <- function(.n) formatC(100 * .n / n_actual_, format = "f", digits = 1)

  fin_save_data(
    .tab  = tab_,
    .name = name_,
    .dir  = .dirs$DirData
  )
  fin_tex_content(
    .tab         = tab_,
    .path        = fs::path(.dirs$DirTables, paste0(name_, ".tex")),
    .digits      = .digits,
    .words_scale = .words_scale
  )
  unit_ <- fin_words_unit(.scale = .words_scale)
  # THE LAST SENTENCE OF THE NOTE says what the cells are rounded to, and the words' unit where there is one.
  spell_ <- c("no decimals", "one decimal", "two decimals", "three decimals")
  dig_txt_ <- if (.digits[["Mean"]] == .digits[["SD"]]) {
    paste0("means and standard deviations carry ", spell_[.digits[["Mean"]] + 1L], ".")
  } else {
    paste0("means carry ", spell_[.digits[["Mean"]] + 1L], " and standard deviations ",
           spell_[.digits[["SD"]] + 1L], ".")
  }
  last_ <- if (is.na(unit_$Note)) dig_txt_ else paste0("Words are in ", unit_$Note, "; ", dig_txt_)
  last_ <- paste0(toupper(stringi::stri_sub(last_, 1L, 1L)), stringi::stri_sub(last_, 2L))
  roles_txt_ <- paste(.roles, collapse = ", ")
  arm_note_ <- if (.arm == "rule") {
    c("Words exclude the filing header. Duration is the difference in years between the contract's start",
      "date and its end date as established by a cascade of rules (a stated term, an open-ended clause, a",
      "cue-word date, or the farthest future date), capped at 30 years; it is undefined where no end could",
      paste0("be established. Parties are the distinct organisations matched to a party of the roles ",
             roles_txt_, ". Countries and states are the distinct countries and U.S. states attached to one"),
      "of those parties outside a governing-law clause.")
  } else {
    c("Words are counted on the flattened text including the filing header. Duration is the difference",
      "in years between the contract's start date and the farthest date after the filing, uncapped; it is",
      "undefined where the contract names no future date. Parties are the distinct organisation names the",
      "extractor proposed before grouping. Countries and states are the distinct countries and U.S. states",
      "named anywhere outside a governing-law clause, attached to a party or not.")
  }
  note_ <- c(
    "This table shows the number of words, the contract duration, the number of contract parties and the",
    "number of countries and U.S. states mentioned, by contract category, on the unique-contract sample",
    paste0("(N = ", format(n_actual_, big.mark = ","), "); ", format(n_unlab_, big.mark = ","),
           " contracts without a category label enter the Total row only."),
    "Category rows report the pooled statistics of their sub-categories.",
    arm_note_,
    paste0("Duration is defined on ", format(tot_$DurN, big.mark = ","),
           " contracts; the number per category is reported in the replication data."),
    "A contract in which no party, country or state was extracted counts as zero rather than missing;",
    paste0("at least one is extracted on ", pct_(tot_$PartN), " percent of contracts for parties, ",
           pct_(tot_$CtryN), " percent for countries and ", pct_(tot_$StateN), " percent for states."),
    "States are U.S. states, so foreign parties contribute none.",
    last_
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(name_, ".tex"))
  )
  cells_ <- fin_content_cells(
    .tab         = tab_,
    .digits      = .digits,
    .words_scale = .words_scale
  )
  names(cells_) <- c(" ", "N", rep(c("Mean", "SD"), 5L))
  groups_ <- c(1, 1, 2, 2, 2, 2, 2)
  names(groups_) <- c(" ", "Contracts", unit_$Header, "Duration (years)", "Parties", "Countries", "States")
  fin_show_category(
    .cells  = cells_,
    .kind   = tab_$Kind,
    .groups = groups_,
    .title  = paste0("Content characteristics by category (", if (.arm == "rule") "rule-based" else "naive", ")"),
    .note   = paste(note_, collapse = " ")
  )
  invisible(tab_)
}


# 9. Table: observation contrast between the two content arms -----------------------------------------------------------
# The two content tables share every row; this one puts their N's side by side. Contracts once; then
# for duration, parties, countries and states the number of contracts on which the measure is
# defined (an end established) or found something (at least one), naive against rule-based, and the
# difference. Words are defined on every contract on both arms and have no block.

#' The contrast rows: per category, naive N, rule-based N and their difference for four measures
#' @param .tab_rule Tibble from fin_table_content(.arm = "rule").
#' @param .tab_naive Tibble from fin_table_content(.arm = "naive").
#' @return Tibble: Row, Kind, N, then <Measure>Naive, <Measure>Rule, <Measure>Delta for Dur, Part, Ctry, State.
fin_tab_contrast <- function(.tab_rule, .tab_naive) {
  if (FALSE) {
    .tab_rule  <- tab_content_rule
    .tab_naive <- tab_content_naive
  }
  if (!identical(.tab_rule$Row, .tab_naive$Row)) cli::cli_abort("The two content tables do not share their rows.")
  if (!identical(.tab_rule$N, .tab_naive$N)) cli::cli_abort("The two content tables differ in N; same S2 expected.")
  tibble::tibble(
    Row        = .tab_rule$Row,
    Kind       = .tab_rule$Kind,
    N          = .tab_rule$N,
    DurNaive   = .tab_naive$DurN,
    DurRule    = .tab_rule$DurN,
    DurDelta   = .tab_rule$DurN - .tab_naive$DurN,
    PartNaive  = .tab_naive$PartN,
    PartRule   = .tab_rule$PartN,
    PartDelta  = .tab_rule$PartN - .tab_naive$PartN,
    CtryNaive  = .tab_naive$CtryN,
    CtryRule   = .tab_rule$CtryN,
    CtryDelta  = .tab_rule$CtryN - .tab_naive$CtryN,
    StateNaive = .tab_naive$StateN,
    StateRule  = .tab_rule$StateN,
    StateDelta = .tab_rule$StateN - .tab_naive$StateN
  )
}

#' The contrast cells for the page: counts, deltas signed
#' @param .tab Tibble from fin_tab_contrast().
#' @return Tibble of character columns, Row first.
fin_contrast_cells <- function(.tab) {
  if (FALSE) {
    .tab <- fin_tab_contrast(
      .tab_rule  = tab_content_rule,
      .tab_naive = tab_content_naive
    )
  }
  d_ <- function(.x) dplyr::if_else(is.na(.x), "", formatC(.x, format = "d", big.mark = ",", flag = "+"))
  tibble::tibble(
    Row        = .tab$Row,
    N          = fin_fmt_count(.tab$N),
    DurNaive   = fin_fmt_count(.tab$DurNaive),
    DurRule    = fin_fmt_count(.tab$DurRule),
    DurDelta   = d_(.tab$DurDelta),
    PartNaive  = fin_fmt_count(.tab$PartNaive),
    PartRule   = fin_fmt_count(.tab$PartRule),
    PartDelta  = d_(.tab$PartDelta),
    CtryNaive  = fin_fmt_count(.tab$CtryNaive),
    CtryRule   = fin_fmt_count(.tab$CtryRule),
    CtryDelta  = d_(.tab$CtryDelta),
    StateNaive = fin_fmt_count(.tab$StateNaive),
    StateRule  = fin_fmt_count(.tab$StateRule),
    StateDelta = d_(.tab$StateDelta)
  )
}

#' Write the contrast tabular
#' @param .tab Tibble from fin_tab_contrast().
#' @param .path Character. The .tex written.
#' @param .size Character. LaTeX size command without the backslash.
#' @return Invisibly, the path.
fin_tex_contrast <- function(.tab, .path, .size = "footnotesize") {
  if (FALSE) {
    .tab  <- fin_tab_contrast(
      .tab_rule  = tab_content_rule,
      .tab_naive = tab_content_naive
    )
    .path <- fs::path(.lP$Output$DirTables, "ContentContrast.tex")
    .size <- "footnotesize"
  }
  cells_ <- fin_contrast_cells(.tab = .tab)
  body_  <- fin_tex_category_body(
    .cells = cells_,
    .kind  = .tab$Kind
  )
  lines_ <- c(
    paste0("\\begingroup\\", .size),
    "\\begin{tabular}{l r rrr rrr rrr rrr}",
    "\\toprule",
    " & Contracts & \\multicolumn{3}{c}{Duration defined} & \\multicolumn{3}{c}{Parties found} &",
    "   \\multicolumn{3}{c}{Countries found} & \\multicolumn{3}{c}{States found} \\\\",
    "\\cmidrule(lr){2-2} \\cmidrule(lr){3-5} \\cmidrule(lr){6-8} \\cmidrule(lr){9-11} \\cmidrule(lr){12-14}",
    " & N & Naive & Rule & Diff. & Naive & Rule & Diff. & Naive & Rule & Diff. & Naive & Rule & Diff. \\\\",
    "\\midrule",
    body_,
    "\\bottomrule",
    "\\end{tabular}",
    "\\endgroup"
  )
  fs::dir_create(fs::path_dir(.path))
  fin_tex_write(
    .lines = lines_,
    .path  = .path
  )
  invisible(.path)
}

#' The observation contrast: built from the two content tables, saved, written, shown
#'
#' @param .tab_rule Tibble returned by fin_table_content(.arm = "rule").
#' @param .tab_naive Tibble returned by fin_table_content(.arm = "naive").
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the contrast tibble.
fin_table_content_contrast <- function(.tab_rule, .tab_naive, .dirs, .name = "ContentContrast") {
  if (FALSE) {
    .tab_rule  <- tab_content_rule
    .tab_naive <- tab_content_naive
    .dirs      <- .lP$Output
    .name      <- "ContentContrast"
  }
  tab_ <- fin_tab_contrast(
    .tab_rule  = .tab_rule,
    .tab_naive = .tab_naive
  ) |>
    dplyr::mutate(Sample = "S2_Descriptive", .before = 1L)
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  fin_tex_contrast(
    .tab  = tab_,
    .path = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  tot_ <- tab_[tab_$Kind == "total", , drop = FALSE]
  note_ <- c(
    "This table contrasts, by contract category and on the unique-contract sample",
    paste0("(N = ", format(tot_$N, big.mark = ","), "), the number of contracts on which each content measure"),
    "is available under the naive extraction and under the rule-based extraction. Duration is defined",
    "where an end date could be established: under the naive rule, where the contract names a date after",
    "the filing; under the rule-based cascade, where a stated term, an open-ended clause, a cue-word date",
    "or a future date establishes one. Parties, countries and states are found where at least one was",
    "extracted: under the naive rule, any organisation name, or any country or state mention outside a",
    "governing-law clause; under the rule-based extraction, a party in the recital, or a country or state",
    "attached to one. Diff. is rule-based less naive. Category rows report the pooled counts of their",
    paste0("sub-categories; ", format(tot_$N - fin_n_classed(.tab = .tab_rule), big.mark = ","),
           " contracts without a category label enter the Total row only.")
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  cells_ <- fin_contrast_cells(.tab = tab_)
  names(cells_) <- c(" ", "N", rep(c("Naive", "Rule", "Diff."), 4L))
  fin_show_category(
    .cells  = cells_,
    .kind   = tab_$Kind,
    .groups = c(" " = 1, "Contracts" = 1, "Duration defined" = 3, "Parties found" = 3, "Countries found" = 3,
                "States found" = 3),
    .title  = "Observations by content measure, naive against rule-based",
    .note   = paste(note_, collapse = " ")
  )
  cli::cli_alert_info(
    "Total: duration {format(tot_$DurNaive, big.mark = ',')} -> {format(tot_$DurRule, big.mark = ',')}; parties \\
     {format(tot_$PartNaive, big.mark = ',')} -> {format(tot_$PartRule, big.mark = ',')}; countries \\
     {format(tot_$CtryNaive, big.mark = ',')} -> {format(tot_$CtryRule, big.mark = ',')}; states \\
     {format(tot_$StateNaive, big.mark = ',')} -> {format(tot_$StateRule, big.mark = ',')}."
  )
  invisible(tab_)
}


# 10. Table: parties by role, the granular view --------------------------------------------------------------------------
# For the authors, not the manuscript: where the party count comes from. Per category, the naive
# spellings, then the mean parties per role after grouping -- registrant, co-filer, counterparty,
# signatory, other -- their sum, and two shares: contracts in which no organisation was found at all,
# and contracts in which the registrant was matched. Two decimals throughout.

#' The party columns per S2 contract, including the two roles the prepared table does not carry
#'
#' nUniSignatory and nUniOther are not in 30's column list, so they are read from the release itself
#' by DocID; the rest come from the prepared table.
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .path_release Character. Contracts.parquet in the release, for the two extra columns.
#' @return Tibble, one row per S2 contract: DocID, Class, Level1, Level2, Spellings, the five role
#'   counts under their role names, HasNoParty.
fin_parties_rows <- function(.ds_contracts, .path_release) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .path_release <- .lP$Input$FilContracts
  }
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S2_Descriptive) |>
    dplyr::select("DocID", "Class", "nUniSpellingsNaive", "nUniRegistrant", "nUniCofiler", "nUniCounterparty",
                  "HasNoParty") |>
    dplyr::collect()
  extra_ <- arrow::open_dataset(.path_release) |>
    dplyr::select("DocID", "nUniSignatory", "nUniOther") |>
    dplyr::collect()
  cat_ <- fin_tab_categories() |>
    dplyr::select("Class", "Level1", "Level2")
  rows_ |>
    dplyr::left_join(
      extra_,
      by           = dplyr::join_by(DocID),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      cat_,
      by           = dplyr::join_by(Class),
      relationship = "many-to-one"
    ) |>
    dplyr::transmute(
      DocID        = .data$DocID,
      Class        = .data$Class,
      Level1       = .data$Level1,
      Level2       = .data$Level2,
      Spellings    = as.numeric(.data$nUniSpellingsNaive),
      registrant   = as.numeric(.data$nUniRegistrant),
      cofiler      = as.numeric(.data$nUniCofiler),
      counterparty = as.numeric(.data$nUniCounterparty),
      signatory    = as.numeric(dplyr::coalesce(.data$nUniSignatory, 0L)),
      other        = as.numeric(dplyr::coalesce(.data$nUniOther, 0L)),
      HasNoParty   = as.integer(dplyr::coalesce(.data$HasNoParty, 0L))
    )
}

#' Means of the party columns over a set of rows
#' @param .rows Tibble from fin_parties_rows(), or a subset.
#' @return One-row tibble: N, Spellings, Registrant, Cofiler, Counterparty, Signatory, Other, Grouped,
#'   ShareNoParty, ShareRegistrant.
fin_parties_stats <- function(.rows) {
  if (FALSE) {
    .rows <- fin_parties_rows(
      .ds_contracts = lst_ds$Contracts,
      .path_release = .lP$Input$FilContracts
    )
  }
  tibble::tibble(
    N               = nrow(.rows),
    Spellings       = mean(.rows$Spellings, na.rm = TRUE),
    Registrant      = mean(.rows$registrant),
    Cofiler         = mean(.rows$cofiler),
    Counterparty    = mean(.rows$counterparty),
    Signatory       = mean(.rows$signatory),
    Other           = mean(.rows$other),
    Grouped         = mean(.rows$registrant + .rows$cofiler + .rows$counterparty + .rows$signatory + .rows$other),
    ShareNoParty    = 100 * mean(.rows$HasNoParty == 1L),
    ShareRegistrant = 100 * mean(.rows$registrant > 0)
  )
}

#' The parties cells for the page: two decimals, shares in percent
#' @param .tab Tibble from fin_tab_by_category(.stats = fin_parties_stats).
#' @return Tibble of character columns, Row first.
fin_parties_cells <- function(.tab) {
  if (FALSE) .tab <- tab_parties
  tibble::tibble(
    Row             = .tab$Row,
    N               = fin_fmt_count(.tab$N),
    Spellings       = fin_fmt_num(.tab$Spellings, 2L),
    Registrant      = fin_fmt_num(.tab$Registrant, 2L),
    Cofiler         = fin_fmt_num(.tab$Cofiler, 2L),
    Counterparty    = fin_fmt_num(.tab$Counterparty, 2L),
    Signatory       = fin_fmt_num(.tab$Signatory, 2L),
    Other           = fin_fmt_num(.tab$Other, 2L),
    Grouped         = fin_fmt_num(.tab$Grouped, 2L),
    ShareNoParty    = fin_fmt_num(.tab$ShareNoParty, 1L),
    ShareRegistrant = fin_fmt_num(.tab$ShareRegistrant, 1L)
  )
}

#' The parties-by-role table: built, saved, written, shown
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .path_release Character. Contracts.parquet in the release.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the table tibble.
fin_table_parties <- function(.ds_contracts, .path_release, .dirs, .name = "PartiesDetail") {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .path_release <- .lP$Input$FilContracts
    .dirs         <- .lP$Output
    .name         <- "PartiesDetail"
  }
  rows_ <- fin_parties_rows(
    .ds_contracts = .ds_contracts,
    .path_release = .path_release
  )
  tab_ <- fin_tab_by_category(
    .rows  = rows_,
    .stats = fin_parties_stats
  ) |>
    dplyr::mutate(Sample = "S2_Descriptive", .before = 1L)
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  cells_ <- fin_parties_cells(.tab = tab_)
  body_  <- fin_tex_category_body(
    .cells = cells_,
    .kind  = tab_$Kind
  )
  lines_ <- c(
    "\\begingroup\\footnotesize",
    "\\begin{tabular}{l r r rrrrr r rr}",
    "\\toprule",
    " & Contracts & Naive & \\multicolumn{6}{c}{Parties after grouping, by role} & \\multicolumn{2}{c}{Share (\\%)} \\\\",
    "\\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-9} \\cmidrule(lr){10-11}",
    " & N & Spellings & Registrant & Co-filer & Counterparty & Signatory & Other & All & No party & Registrant matched \\\\",
    "\\midrule",
    body_,
    "\\bottomrule",
    "\\end{tabular}",
    "\\endgroup"
  )
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  tot_  <- tab_[tab_$Kind == "total", , drop = FALSE]
  note_ <- c(
    "For the authors. Mean parties per contract by category on the unique-contract sample",
    paste0("(N = ", format(tot_$N, big.mark = ","), "). Spellings are the distinct organisation names the"),
    "extractor proposed before grouping. The role columns are the distinct parties after grouping,",
    "by the role the rules assigned: matched to the filing registrant, to another registrant of the same",
    "filing, a party in the recital window that is not the registrant, a party appearing only in the",
    "final tenth of the document, or a party named outside both. All is their sum. No party is the share",
    "of contracts in which no organisation was found at all; Registrant matched is the share in which the",
    "registrant was identified in the text. The recital parties of the content tables are the first three",
    "roles."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_) <- c(" ", "N", "Spellings", "Registrant", "Co-filer", "Counterparty", "Signatory", "Other", "All",
                     "No party", "Registrant matched")
  fin_show_category(
    .cells  = cells_,
    .kind   = tab_$Kind,
    .groups = c(" " = 1, "Contracts" = 1, "Naive" = 1, "Parties after grouping, by role" = 6, "Share (%)" = 2),
    .title  = "Parties by role, the granular view",
    .note   = paste(note_, collapse = " ")
  )
  cli::cli_alert_info(
    "Total: {fin_fmt_num(tot_$Spellings, 2L)} spellings -> {fin_fmt_num(tot_$Grouped, 2L)} grouped parties, \\
     of which {fin_fmt_num(tot_$Registrant + tot_$Cofiler + tot_$Counterparty, 2L)} in the recital; no party on \\
     {fin_fmt_num(tot_$ShareNoParty, 1L)} percent, registrant matched on {fin_fmt_num(tot_$ShareRegistrant, 1L)}."
  )
  invisible(tab_)
}


# 11. Table: money amounts, the granular view ---------------------------------------------------------------------------
# For the authors. Per category: how often the extractor found an amount and how many, on the
# naive count and after the zero and par-value filter, in USD and in other currencies; and the
# largest and the median USD amount as MEDIANS across contracts. Medians, because the release
# documents a defect in the amounts themselves: flattened HTML tables glued adjacent cells into one
# figure on roughly 1,500 contracts, and a mean of maxima is that defect and nothing else.

#' The money columns per S2 contract, read from the release by DocID
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .path_release Character. Contracts.parquet in the release.
#' @return Tibble, one row per S2 contract: DocID, Class, Level1, Level2 and the seven money columns.
fin_money_rows <- function(.ds_contracts, .path_release) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .path_release <- .lP$Input$FilContracts
  }
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S2_Descriptive) |>
    dplyr::select("DocID", "Class") |>
    dplyr::collect()
  money_cols_ <- c("nUniAmountNaive", "nUniAmountUSD", "nUniAmountOther", "MoneyMaxUSD", "MoneyMedUSD",
                   "MoneyMaxOther", "MoneyMedOther")
  have_ <- names(arrow::open_dataset(.path_release))
  miss_ <- setdiff(money_cols_, have_)
  if (length(miss_) > 0L) cli::cli_abort("The release lacks money column{?s} {miss_}.")
  extra_ <- arrow::open_dataset(.path_release) |>
    dplyr::select("DocID", dplyr::all_of(money_cols_)) |>
    dplyr::collect()
  cat_ <- fin_tab_categories() |>
    dplyr::select("Class", "Level1", "Level2")
  rows_ |>
    dplyr::left_join(
      extra_,
      by           = dplyr::join_by(DocID),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      cat_,
      by           = dplyr::join_by(Class),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(dplyr::across(c("nUniAmountNaive", "nUniAmountUSD", "nUniAmountOther"),
                                \(.x) as.numeric(dplyr::coalesce(.x, 0L))))
}

#' Coverage, counts and medians of the money columns over a set of rows
#' @param .rows Tibble from fin_money_rows(), or a subset.
#' @return One-row tibble: N, NaiveShare, NaiveMean, UsdShare, UsdMean, OtherMean, MaxN, MaxMedian, MedN,
#'   MedMedian (amounts in USD millions).
fin_money_stats <- function(.rows) {
  if (FALSE) {
    .rows <- fin_money_rows(
      .ds_contracts = lst_ds$Contracts,
      .path_release = .lP$Input$FilContracts
    )
  }
  max_ <- .rows$MoneyMaxUSD[!is.na(.rows$MoneyMaxUSD)]
  med_ <- .rows$MoneyMedUSD[!is.na(.rows$MoneyMedUSD)]
  tibble::tibble(
    N          = nrow(.rows),
    NaiveShare = 100 * mean(.rows$nUniAmountNaive > 0),
    NaiveMean  = mean(.rows$nUniAmountNaive),
    UsdShare   = 100 * mean(.rows$nUniAmountUSD > 0),
    UsdMean    = mean(.rows$nUniAmountUSD),
    OtherMean  = mean(.rows$nUniAmountOther),
    MaxN       = length(max_),
    MaxMedian  = if (length(max_) == 0L) NA_real_ else stats::median(max_) / 1e6,
    MedN       = length(med_),
    MedMedian  = if (length(med_) == 0L) NA_real_ else stats::median(med_) / 1e6
  )
}

#' The money cells for the page
#' @param .tab Tibble from fin_tab_by_category(.stats = fin_money_stats).
#' @return Tibble of character columns, Row first.
fin_money_cells <- function(.tab) {
  if (FALSE) .tab <- tab_money
  tibble::tibble(
    Row        = .tab$Row,
    N          = fin_fmt_count(.tab$N),
    NaiveShare = fin_fmt_num(.tab$NaiveShare, 1L),
    NaiveMean  = fin_fmt_num(.tab$NaiveMean, 2L),
    UsdShare   = fin_fmt_num(.tab$UsdShare, 1L),
    UsdMean    = fin_fmt_num(.tab$UsdMean, 2L),
    OtherMean  = fin_fmt_num(.tab$OtherMean, 2L),
    MaxN       = fin_fmt_count(.tab$MaxN),
    MaxMedian  = fin_fmt_num(.tab$MaxMedian, 2L),
    MedN       = fin_fmt_count(.tab$MedN),
    MedMedian  = fin_fmt_num(.tab$MedMedian, 2L)
  )
}

#' The money table: built, saved, written, shown
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .path_release Character. Contracts.parquet in the release.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the table tibble.
fin_table_money <- function(.ds_contracts, .path_release, .dirs, .name = "MoneyDetail") {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .path_release <- .lP$Input$FilContracts
    .dirs         <- .lP$Output
    .name         <- "MoneyDetail"
  }
  rows_ <- fin_money_rows(
    .ds_contracts = .ds_contracts,
    .path_release = .path_release
  )
  tab_ <- fin_tab_by_category(
    .rows  = rows_,
    .stats = fin_money_stats
  ) |>
    dplyr::mutate(Sample = "S2_Descriptive", .before = 1L)
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  cells_ <- fin_money_cells(.tab = tab_)
  body_  <- fin_tex_category_body(
    .cells = cells_,
    .kind  = tab_$Kind
  )
  lines_ <- c(
    "\\begingroup\\footnotesize",
    "\\begin{tabular}{l r rr rr r rr rr}",
    "\\toprule",
    " & Contracts & \\multicolumn{2}{c}{Naive amounts} & \\multicolumn{2}{c}{USD, filtered} & Other &",
    "   \\multicolumn{2}{c}{Largest USD} & \\multicolumn{2}{c}{Median USD} \\\\",
    paste("\\cmidrule(lr){2-2} \\cmidrule(lr){3-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-7}",
          "\\cmidrule(lr){8-9} \\cmidrule(lr){10-11}"),
    " & N & Found (\\%) & Mean & Found (\\%) & Mean & Mean & N & Median (m) & N & Median (m) \\\\",
    "\\midrule",
    body_,
    "\\bottomrule",
    "\\end{tabular}",
    "\\endgroup"
  )
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  tot_  <- tab_[tab_$Kind == "total", , drop = FALSE]
  note_ <- c(
    "For the authors. Monetary amounts by category on the unique-contract sample",
    paste0("(N = ", format(tot_$N, big.mark = ","), "). Naive amounts are the distinct parsed, non-withheld"),
    "amounts in any currency before the zero and par-value filter; USD, filtered are the distinct USD",
    "amounts after it; Other are the distinct non-USD amounts after it, pooled across currencies without",
    "conversion, so only their count is meaningful. Found is the share of contracts with at least one",
    "such amount; Mean is the mean count over all contracts, zero where none. Largest and median USD are",
    "the contract's largest and median USD amount, reported as the median across contracts in USD",
    "millions on the N contracts where a usable USD figure exists. Medians rather than means because",
    "flattened HTML tables glued adjacent cells into one figure on roughly 1,500 contracts, and a mean of",
    "maxima measures that defect."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_) <- c(" ", "N", "Found (%)", "Mean", "Found (%)", "Mean", "Mean", "N", "Median (m)", "N", "Median (m)")
  fin_show_category(
    .cells  = cells_,
    .kind   = tab_$Kind,
    .groups = c(" " = 1, "Contracts" = 1, "Naive amounts" = 2, "USD, filtered" = 2, "Other" = 1, "Largest USD" = 2,
                "Median USD" = 2),
    .title  = "Monetary amounts by category, the granular view",
    .note   = paste(note_, collapse = " ")
  )
  cli::cli_alert_info(
    "Total: an amount on {fin_fmt_num(tot_$NaiveShare, 1L)} percent naive, {fin_fmt_num(tot_$UsdShare, 1L)} percent \\
     USD after the filter; median largest USD amount {fin_fmt_num(tot_$MaxMedian, 2L)} m on \\
     {format(tot_$MaxN, big.mark = ',')} contracts."
  )
  invisible(tab_)
}


# 12. Table: redactions, extensive and intensive margin ------------------------------------------------------------------
# For the authors, on the redaction window (from 2008, when orders become observable). Extensive
# margin: the share of contracts with at least one marker, by kind of marker, and the share under a
# confidential treatment order on the pre-FAST part. Intensive margin: among marked contracts, how
# many markers, how many carry five or more -- the threshold question -- and how many prices were
# withheld.

#' The marker and order columns per S6 contract
#' @param .ds_contracts The prepared Contracts dataset.
#' @return Tibble, one row per S6 contract: DocID, Class, Level1, Level2, Year, the seven marker
#'   counts, HasCto.
fin_redact_rows <- function(.ds_contracts) {
  if (FALSE) .ds_contracts <- lst_ds$Contracts
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S6_Redaction) |>
    dplyr::select("DocID", "Class", "Year", "nRedactExplicit", "nRedactSymbol", "nRedactBlank", "nOmitExplicit",
                  "nOmitSymbol", "nRedactBare", "nRedactMoney", "HasCto") |>
    dplyr::collect()
  cat_ <- fin_tab_categories() |>
    dplyr::select("Class", "Level1", "Level2")
  rows_ |>
    dplyr::left_join(
      cat_,
      by           = dplyr::join_by(Class),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      HasCto  = as.integer(dplyr::coalesce(.data$HasCto, 0L)),
      Paper   = .data$nRedactSymbol + .data$nRedactExplicit,   # the manuscript's marker measure
      Omit    = .data$nOmitExplicit + .data$nOmitSymbol
    )
}

#' Extensive and intensive margins of the markers over a set of rows
#' @param .rows Tibble from fin_redact_rows(), or a subset.
#' @return One-row tibble: N, PaperShare, BlankShare, BareShare, OmitShare, CtoN, CtoShare, MarkedN,
#'   MarkersMean, Ge5Share, MoneyN, MoneyMean.
fin_redact_stats <- function(.rows) {
  if (FALSE) .rows <- fin_redact_rows(.ds_contracts = lst_ds$Contracts)
  pre_    <- .rows[.rows$Year <= 2018L, , drop = FALSE]
  marked_ <- .rows[.rows$Paper > 0, , drop = FALSE]
  money_  <- .rows[.rows$nRedactMoney > 0, , drop = FALSE]
  tibble::tibble(
    N           = nrow(.rows),
    PaperShare  = 100 * mean(.rows$Paper > 0),
    BlankShare  = 100 * mean(.rows$nRedactBlank > 0),
    BareShare   = 100 * mean(.rows$nRedactBare > 0),
    OmitShare   = 100 * mean(.rows$Omit > 0),
    CtoN        = nrow(pre_),
    CtoShare    = if (nrow(pre_) == 0L) NA_real_ else 100 * mean(pre_$HasCto == 1L),
    MarkedN     = nrow(marked_),
    MarkersMean = if (nrow(marked_) == 0L) NA_real_ else mean(marked_$Paper),
    Ge5Share    = if (nrow(marked_) == 0L) NA_real_ else 100 * mean(marked_$Paper >= 5),
    MoneyN      = nrow(money_),
    MoneyMean   = if (nrow(money_) == 0L) NA_real_ else mean(money_$nRedactMoney)
  )
}

#' The redaction cells for the page
#' @param .tab Tibble from fin_tab_by_category(.stats = fin_redact_stats).
#' @return Tibble of character columns, Row first.
fin_redact_cells <- function(.tab) {
  if (FALSE) .tab <- tab_redact
  tibble::tibble(
    Row         = .tab$Row,
    N           = fin_fmt_count(.tab$N),
    PaperShare  = fin_fmt_num(.tab$PaperShare, 1L),
    BlankShare  = fin_fmt_num(.tab$BlankShare, 1L),
    BareShare   = fin_fmt_num(.tab$BareShare, 1L),
    OmitShare   = fin_fmt_num(.tab$OmitShare, 1L),
    CtoN        = fin_fmt_count(.tab$CtoN),
    CtoShare    = fin_fmt_num(.tab$CtoShare, 1L),
    MarkedN     = fin_fmt_count(.tab$MarkedN),
    MarkersMean = fin_fmt_num(.tab$MarkersMean, 1L),
    Ge5Share    = fin_fmt_num(.tab$Ge5Share, 1L),
    MoneyN      = fin_fmt_count(.tab$MoneyN),
    MoneyMean   = fin_fmt_num(.tab$MoneyMean, 1L)
  )
}

#' The redaction table: built, saved, written, shown
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the table tibble.
fin_table_redactions <- function(.ds_contracts, .dirs, .name = "RedactionsDetail") {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .dirs         <- .lP$Output
    .name         <- "RedactionsDetail"
  }
  rows_ <- fin_redact_rows(.ds_contracts = .ds_contracts)
  tab_ <- fin_tab_by_category(
    .rows  = rows_,
    .stats = fin_redact_stats
  ) |>
    dplyr::mutate(Sample = "S6_Redaction", .before = 1L)
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  # THE PAGE SHOWS SIX COLUMNS (decided 10 Sep): the paper's marker measure on the extensive margin,
  # the order share, and the intensive margin among marked contracts. Blank, bare and omitted markers
  # and the price-withholding count stay in the data file.
  cells_ <- fin_redact_cells(.tab = tab_) |>
    dplyr::select("Row", "N", "PaperShare", "CtoN", "CtoShare", "MarkedN", "MarkersMean", "Ge5Share")
  body_  <- fin_tex_category_body(
    .cells = cells_,
    .kind  = tab_$Kind
  )
  lines_ <- c(
    "\\begingroup\\small",
    "\\begin{tabular}{l r r rr rrr}",
    "\\toprule",
    paste(" & Contracts & Marked (\\%) & \\multicolumn{2}{c}{Order, 2008-18} &",
          "\\multicolumn{3}{c}{Among contracts with markers} \\\\"),
    "\\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-8}",
    " & N & Redaction & N & \\% & N & Markers & $\\geq$5 (\\%) \\\\",
    "\\midrule",
    body_,
    "\\bottomrule",
    "\\end{tabular}",
    "\\endgroup"
  )
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  tot_  <- tab_[tab_$Kind == "total", , drop = FALSE]
  note_ <- c(
    "For the authors. Redaction markers by category on the unique-contract sample from 2008, when",
    paste0("confidential treatment orders become observable (N = ", format(tot_$N, big.mark = ","), ")."),
    "Two identifications sit side by side. Marked is the text: the share of contracts whose text carries",
    "at least one redaction marker, a bracketed placeholder naming confidential treatment or holding a",
    "symbol, which is the manuscript's measure. Order is the SEC: the share of contracts filed 2008-2018",
    "covered by a granted confidential treatment order, on the N contracts of those years; a contract",
    "can be in either, both or neither. The last block is the intensive margin of the text side, among",
    "the N contracts with at least one marker: the mean number of markers per contract and the share of",
    "marked contracts carrying five or more, which is the column the marker-threshold question needs."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_) <- c(" ", "N", "Redaction", "N", "%", "N", "Markers", ">= 5 (%)")
  fin_show_category(
    .cells  = cells_,
    .kind   = tab_$Kind,
    .groups = c(" " = 1, "Contracts" = 1, "Marked (%)" = 1, "Order, 2008-18" = 2, "Among contracts with markers" = 3),
    .title  = "Redactions by category: extensive and intensive margin",
    .note   = paste(note_, collapse = " ")
  )
  cli::cli_alert_info(
    "Total: {fin_fmt_num(tot_$PaperShare, 1L)} percent marked; order on {fin_fmt_num(tot_$CtoShare, 1L)} percent of \\
     2008-18 contracts; among marked, {fin_fmt_num(tot_$MarkersMean, 1L)} markers and \\
     {fin_fmt_num(tot_$Ge5Share, 1L)} percent with five or more."
  )
  invisible(tab_)
}


# 13. Open issues: probes that keep the evidence on the page -----------------------------------------------------------
# Each function here is read-only and cheap, and exists so that an issue under discussion with
# Ann-Kristin is a table in the render rather than a number remembered from a console.

#' The unlabelled contracts in S2: how many, where in the ladder, what they are
#'
#' Every one of them has zero words: the exhibit is a placeholder for a PDF or an image, and no
#' classifier, extractor or word count ever saw text. They pass 02B's malformed test because that
#' test is about format, not about whether any text survived flattening. The table says so with
#' counts, by ladder step and form, and prints the ten most common titles.
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .path_release Character. Contracts.parquet in the release, for nChars.
#' @return Invisibly, a list: Summary (one row), BySample (step x form), Titles (top ten).
fin_issue_unlabelled <- function(.ds_contracts, .path_release) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .path_release <- .lP$Input$FilContracts
  }
  un_ <- .ds_contracts |>
    dplyr::filter(.data$S2_Descriptive, is.na(.data$Class)) |>
    dplyr::select("DocID", "FormType", "Year", "DocDesc", "nWords", "SampleStepDesc", "HasNoParty") |>
    dplyr::collect()
  chars_ <- arrow::open_dataset(.path_release) |>
    dplyr::filter(.data$DocID %in% un_$DocID) |>
    dplyr::select("DocID", "nChars") |>
    dplyr::collect()
  un_ <- dplyr::left_join(
    un_,
    chars_,
    by           = dplyr::join_by(DocID),
    relationship = "one-to-one"
  )
  summary_ <- tibble::tibble(
    Unlabelled  = nrow(un_),
    ZeroWords   = sum(un_$nWords == 0L),
    MedianChars = stats::median(un_$nChars),
    MaxChars    = max(un_$nChars),
    NoParty     = sum(un_$HasNoParty == 1L, na.rm = TRUE),
    PdfInTitle  = sum(stringi::stri_detect_regex(dplyr::coalesce(un_$DocDesc, ""), "PDF|IMAGE|GRAPHIC|PAPER"))
  )
  by_ <- un_ |>
    dplyr::mutate(Form = stringi::stri_replace_first_regex(.data$FormType, "/A$", "")) |>
    dplyr::count(.data$SampleStepDesc, .data$Form, name = "N") |>
    tidyr::pivot_wider(names_from = "Form", values_from = "N", values_fill = 0L) |>
    dplyr::rename(Step = "SampleStepDesc")
  titles_ <- un_ |>
    dplyr::count(.data$DocDesc, name = "N", sort = TRUE) |>
    utils::head(10L) |>
    dplyr::mutate(DocDesc = dplyr::coalesce(.data$DocDesc, "(no description)"))

  tbl_out(
    .tab   = summary_,
    .title = "Unlabelled contracts in the unique-contract sample",
    .notes = c(ZeroWords = "Equal to Unlabelled: no text reached the classifier.",
               PdfInTitle = "Titles naming a PDF, image, graphic or paper exhibit.")
  )
  tbl_out(
    .tab   = by_,
    .title = "The same contracts by ladder step and form family"
  )
  tbl_out(
    .tab   = titles_,
    .title = "Their ten most common exhibit descriptions"
  )
  invisible(list(Summary = summary_, BySample = by_, Titles = titles_))
}


# 14. Table: classifier confidence by category ---------------------------------------------------------------------------
# The BERT probability behind the crowned detailed class, by category: N, mean, SD and quartiles;
# beside them, how often the keyword arm agreed with it. The 735 zero-word contracts carry no
# probability and are not in this table (see Open Issues).

#' The confidence columns per labelled S2 contract
#' @param .ds_contracts The prepared Contracts dataset.
#' @return Tibble, one row per labelled S2 contract: DocID, Class, Level1, Level2, Prob, Flag.
fin_confidence_rows <- function(.ds_contracts) {
  if (FALSE) .ds_contracts <- lst_ds$Contracts
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S2_Descriptive, !is.na(.data$Class)) |>
    dplyr::select("DocID", "Class", Prob = "BertClassDetailedProb", Flag = "ClassDetailedFlag") |>
    dplyr::collect()
  cat_ <- fin_tab_categories() |>
    dplyr::select("Class", "Level1", "Level2")
  rows_ |>
    dplyr::left_join(
      cat_,
      by           = dplyr::join_by(Class),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(Prob = as.numeric(.data$Prob), Flag = as.character(.data$Flag))
}

#' Distribution of the crowned probability and the keyword agreement over a set of rows
#' @param .rows Tibble from fin_confidence_rows(), or a subset.
#' @return One-row tibble: N, Mean, SD, P25, P50, P75, ShareConfirmed, ShareContradicted.
fin_confidence_stats <- function(.rows) {
  if (FALSE) .rows <- fin_confidence_rows(.ds_contracts = lst_ds$Contracts)
  p_ <- .rows$Prob[!is.na(.rows$Prob)]
  q_ <- if (length(p_) == 0L) rep(NA_real_, 3L) else unname(stats::quantile(p_, c(0.25, 0.5, 0.75)))
  tibble::tibble(
    N                 = nrow(.rows),
    Mean              = if (length(p_) == 0L) NA_real_ else mean(p_),
    SD                = if (length(p_) < 2L) NA_real_ else stats::sd(p_),
    P25               = q_[[1L]],
    P50               = q_[[2L]],
    P75               = q_[[3L]],
    ShareConfirmed    = 100 * mean(.rows$Flag %in% "confirmed"),
    ShareContradicted = 100 * mean(.rows$Flag %in% "contradicted")
  )
}

#' The confidence cells for the page: probabilities to three decimals, shares to one
#' @param .tab Tibble from fin_tab_by_category(.stats = fin_confidence_stats).
#' @return Tibble of character columns, Row first.
fin_confidence_cells <- function(.tab) {
  if (FALSE) .tab <- tab_confidence
  tibble::tibble(
    Row               = .tab$Row,
    N                 = fin_fmt_count(.tab$N),
    Mean              = fin_fmt_num(.tab$Mean, 3L),
    SD                = fin_fmt_num(.tab$SD, 3L),
    P25               = fin_fmt_num(.tab$P25, 3L),
    P50               = fin_fmt_num(.tab$P50, 3L),
    P75               = fin_fmt_num(.tab$P75, 3L),
    ShareConfirmed    = fin_fmt_num(.tab$ShareConfirmed, 1L),
    ShareContradicted = fin_fmt_num(.tab$ShareContradicted, 1L)
  )
}

#' The classifier-confidence table: built, saved, written, shown
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the table tibble.
fin_table_confidence <- function(.ds_contracts, .dirs, .name = "ClassConfidence") {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .dirs         <- .lP$Output
    .name         <- "ClassConfidence"
  }
  rows_ <- fin_confidence_rows(.ds_contracts = .ds_contracts)
  flags_ <- setdiff(unique(rows_$Flag), c("confirmed", "contradicted", NA))
  if (length(flags_) > 0L) {
    cli::cli_alert_info("ClassDetailedFlag also takes {flags_}; those rows count in neither share.")
  }
  tab_ <- fin_tab_by_category(
    .rows  = rows_,
    .stats = fin_confidence_stats
  ) |>
    dplyr::mutate(Sample = "S2_Descriptive, labelled", .before = 1L)
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  cells_ <- fin_confidence_cells(.tab = tab_)
  body_  <- fin_tex_category_body(
    .cells = cells_,
    .kind  = tab_$Kind
  )
  lines_ <- c(
    "\\begingroup\\small",
    "\\begin{tabular}{l r rrrrr rr}",
    "\\toprule",
    " & Contracts & \\multicolumn{5}{c}{Probability of the assigned category} & \\multicolumn{2}{c}{Keyword arm (\\%)} \\\\",
    "\\cmidrule(lr){2-2} \\cmidrule(lr){3-7} \\cmidrule(lr){8-9}",
    " & N & Mean & SD & P25 & Median & P75 & Confirms & Contradicts \\\\",
    "\\midrule",
    body_,
    "\\bottomrule",
    "\\end{tabular}",
    "\\endgroup"
  )
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  tot_  <- tab_[tab_$Kind == "total", , drop = FALSE]
  note_ <- c(
    "This table shows the classifier's confidence in the assigned category, by category, on the",
    paste0("labelled unique-contract sample (N = ", format(tot_$N, big.mark = ","), "; contracts without"),
    "readable text carry no label and are excluded). The probability is the fine-tuned transformer's",
    "softmax probability of the category it assigned. Confirms and Contradicts are the shares of",
    "contracts on which the keyword-based classifier fired and agreed, respectively disagreed, with the",
    "transformer's category; on the remainder it did not fire. Category rows report the pooled",
    "statistics of their sub-categories."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_) <- c(" ", "N", "Mean", "SD", "P25", "Median", "P75", "Confirms", "Contradicts")
  fin_show_category(
    .cells  = cells_,
    .kind   = tab_$Kind,
    .groups = c(" " = 1, "Contracts" = 1, "Probability of the assigned category" = 5, "Keyword arm (%)" = 2),
    .title  = "Classifier confidence by category",
    .note   = paste(note_, collapse = " ")
  )
  cli::cli_alert_info(
    "Total: mean probability {fin_fmt_num(tot_$Mean, 3L)}, median {fin_fmt_num(tot_$P50, 3L)}; keyword arm confirms \\
     {fin_fmt_num(tot_$ShareConfirmed, 1L)} percent, contradicts {fin_fmt_num(tot_$ShareContradicted, 1L)} percent."
  )
  invisible(tab_)
}


# 15. Tables: Item 1.01 summaries -----------------------------------------------------------------------------------------
# Three tables on the announcement sample: the definitions of the nine summary measures, the
# replication of the revision's comparison (delayed against attached, Welch t), and the same nine
# measures as outcomes of a regression on the delay indicator with year effects, firm effects, and
# the resubmission's controls. Delayed means the 8-K carried no Exhibit 10 -- the contract came
# later, in a periodic filing -- attached means it rode on the 8-K.

# THE NINE MEASURES, in table order, with the label each row carries and the definition the
# variables table prints. Words, Fog, boilerplate, lag and compliance are on their own scale; the
# four counts are per thousand words.
.fin_summary_measures <- tibble::tibble(
  Measure = c("Words", "Fog", "Uncertainty", "Numbers", "Dollars", "Percents", "Boilerplate", "LagDays", "OnTime"),
  Label   = c("Number of words", "Fog index", "Uncertainty (per 1,000 words)", "Numbers (per 1,000 words)",
              "Dollar figures (per 1,000 words)", "Percentages (per 1,000 words)", "Boilerplate share",
              "Announcement lag (days)", "Within four business days"),
  Short   = c("Words", "Fog", "Uncert.", "Numbers", "Dollars", "Percents", "Boiler.", "Lag", "On time"),
  Definition = c(
    "Words in the Item 1.01 narrative after its heading is removed.",
    paste("Gunning Fog index of the narrative: 0.4 times the sum of words per sentence and the percentage",
          "of words with three or more syllables."),
    "Words in the Loughran-McDonald uncertainty list, per thousand words.",
    "Numeric tokens, per thousand words.",
    "Dollar amounts, per thousand words.",
    "Percentages, per thousand words.",
    paste("Share of the narrative's four-word phrases that appear in the Item 1.01 narratives of at least",
          "one in a hundred distinct filers."),
    paste("Calendar days from the agreement date the narrative states to the filing date of the 8-K,",
          "winsorised at the first and ninety-ninth percentile of the sample."),
    paste("Indicator equal to one where the announcement lag is at most six calendar days, the longest span",
          "four business days can cover.")
  )
)

#' The variables table for the summary measures: built, saved, written, shown
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the tibble.
fin_table_summary_variables <- function(.dirs, .name = "SummaryVariables") {
  if (FALSE) {
    .dirs <- .lP$Output
    .name <- "SummaryVariables"
  }
  tab_ <- dplyr::select(.fin_summary_measures, Variable = "Label", "Definition")
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  fin_tex_tabular(
    .tab   = tab_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex")),
    .align = "p{4.4cm} p{12cm}"
  )
  note_ <- c(
    "This table defines the measures of the Item 1.01 narrative used in the summary tables. An",
    "announcement is delayed where the 8-K carries no Exhibit 10, so that the contract itself is filed",
    "later with a periodic report, and attached where the contract rides on the 8-K."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  fin_show_table(
    .tab        = tab_,
    .title      = "Summary measures",
    .notes      = paste(note_, collapse = " "),
    .full_width = TRUE
  )
  invisible(tab_)
}

#' The announcement sample as rows: the nine measures, the delay indicator, the keys
#'
#' THE LAG IS WINSORISED, the on-time indicator is not. A misread agreement date makes a lag of
#' minus three hundred days or of three thousand, and a handful of those move a mean by more than
#' the difference between the groups; the lag is clipped at the sample's first and ninety-ninth
#' percentile. The indicator is computed from the raw lag upstream and needs no clipping.
#'
#' @param .ds_summaries The prepared Summaries dataset.
#' @param .sample Character. The membership column: S7_Summaries or S7_All.
#' @param .winsor Numeric of length two, or NULL. The probabilities the lag is clipped at.
#' @return Tibble, one row per announcement: HashIndex, CIK, DateFiled, Year, Delayed, and the nine
#'   measures under their Measure names, plus LagRaw.
fin_summary_rows <- function(.ds_summaries, .sample = "S7_Summaries", .winsor = c(0.01, 0.99)) {
  if (FALSE) {
    .ds_summaries <- lst_ds$Summaries
    .sample       <- "S7_Summaries"
    .winsor       <- c(0.01, 0.99)
  }
  out_ <- .ds_summaries |>
    dplyr::filter(.data[[.sample]]) |>
    dplyr::select("HashIndex", "CIK", "DateFiled", "SumAttached", "SumWords", "SumFog", "SumUncertain", "SumNumbers",
                  "SumDollars", "SumPercents", "SumBoiler", "SumLagDays", "OnTime") |>
    dplyr::collect() |>
    dplyr::transmute(
      HashIndex   = .data$HashIndex,
      CIK         = stringi::stri_pad_left(as.character(.data$CIK), width = 10L, pad = "0"),
      DateFiled   = as.Date(.data$DateFiled),
      Year        = as.integer(lubridate::year(.data$DateFiled)),
      Delayed     = as.integer(.data$SumAttached == 0L),
      Words       = as.numeric(.data$SumWords),
      Fog         = as.numeric(.data$SumFog),
      Uncertainty = 1000 * .data$SumUncertain / .data$SumWords,
      Numbers     = 1000 * .data$SumNumbers / .data$SumWords,
      Dollars     = 1000 * .data$SumDollars / .data$SumWords,
      Percents    = 1000 * .data$SumPercents / .data$SumWords,
      Boilerplate = as.numeric(.data$SumBoiler),
      LagRaw      = as.numeric(.data$SumLagDays),
      OnTime      = as.numeric(.data$OnTime)
    )
  if (is.null(.winsor)) {
    out_ <- dplyr::mutate(out_, LagDays = .data$LagRaw)
  } else {
    q_ <- stats::quantile(out_$LagRaw, probs = .winsor, na.rm = TRUE, names = FALSE)
    out_ <- dplyr::mutate(out_, LagDays = pmin(pmax(.data$LagRaw, q_[[1L]]), q_[[2L]]))
  }
  dplyr::relocate(out_, "LagDays", .before = "OnTime")
}

#' The comparison: N and mean by group, the difference and its Welch t, per measure
#' @param .rows Tibble from fin_summary_rows().
#' @param .measures Character. Which measures, in table order.
#' @return Tibble: Measure, NDelayed, NAttached, MeanDelayed, MeanAttached, Diff, TStat, PValue.
fin_summary_compare <- function(.rows, .measures = .fin_summary_measures$Measure) {
  if (FALSE) {
    .rows     <- fin_summary_rows(.ds_summaries = lst_ds$Summaries)
    .measures <- .fin_summary_measures$Measure
  }
  one_ <- function(.m) {
    v_ <- .rows[[.m]]
    ok_ <- !is.na(v_) & is.finite(v_)
    d_ <- v_[ok_ & .rows$Delayed == 1L]
    a_ <- v_[ok_ & .rows$Delayed == 0L]
    t_ <- stats::t.test(d_, a_)
    tibble::tibble(
      Measure      = .m,
      NDelayed     = length(d_),
      NAttached    = length(a_),
      MeanDelayed  = mean(d_),
      MeanAttached = mean(a_),
      Diff         = mean(d_) - mean(a_),
      TStat        = unname(t_$statistic),
      PValue       = t_$p.value
    )
  }
  purrr::map(.measures, one_) |>
    purrr::list_rbind()
}

#' Significance stars from a p-value
#' @param .p Numeric.
#' @return Character: "***", "**", "*" or "".
fin_stars <- function(.p) {
  if (FALSE) .p <- c(0.001, 0.03, 0.08, 0.5)
  dplyr::case_when(is.na(.p) ~ "", .p < 0.01 ~ "***", .p < 0.05 ~ "**", .p < 0.10 ~ "*", .default = "")
}

#' The comparison table: built, saved, written, shown
#' @param .ds_summaries The prepared Summaries dataset.
#' @param .sample Character. S7_Summaries (the paper's) or S7_All.
#' @param .measures Character. Which measures, in row order; the default is all nine.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the comparison tibble.
fin_table_summaries <- function(.ds_summaries, .sample = "S7_Summaries", .measures = .fin_summary_measures$Measure,
                                .dirs, .name = "Summaries") {
  if (FALSE) {
    .ds_summaries <- lst_ds$Summaries
    .sample       <- "S7_Summaries"
    .measures     <- .fin_summary_measures$Measure
    .dirs         <- .lP$Output
    .name         <- "Summaries"
  }
  rows_ <- fin_summary_rows(
    .ds_summaries = .ds_summaries,
    .sample       = .sample
  )
  tab_ <- fin_summary_compare(
    .rows     = rows_,
    .measures = .measures
  ) |>
    dplyr::mutate(Sample = .sample, .before = 1L)
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  cells_ <- tibble::tibble(
    Variable     = unname(.fin_summary_measures$Label[match(tab_$Measure, .fin_summary_measures$Measure)]),
    NDelayed     = fin_fmt_count(tab_$NDelayed),
    NAttached    = fin_fmt_count(tab_$NAttached),
    MeanDelayed  = fin_fmt_num(tab_$MeanDelayed, 3L),
    MeanAttached = fin_fmt_num(tab_$MeanAttached, 3L),
    Diff         = paste0(fin_fmt_num(tab_$Diff, 3L), fin_stars(tab_$PValue)),
    TStat        = fin_fmt_num(tab_$TStat, 3L)
  )
  tex_cells_ <- dplyr::mutate(cells_, Diff = paste0(fin_fmt_num(tab_$Diff, 3L), "$^{", fin_stars(tab_$PValue), "}$"))
  body_ <- purrr::map_chr(seq_len(nrow(tex_cells_)), \(.i) {
    row_ <- unlist(tex_cells_[.i, ])
    row_[1L] <- fin_tex_escape(row_[1L])
    paste0(paste(row_, collapse = " & "), " \\\\")
  })
  lines_ <- c(
    "\\begingroup\\small",
    "\\begin{tabular}{l rr rr r r}",
    "\\toprule",
    " & \\multicolumn{2}{c}{Obs.} & \\multicolumn{2}{c}{Mean} & Difference & \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    "Variable & Delayed & Attached & Delayed & Attached & (Delayed - Attached) & t-stat \\\\",
    "\\midrule",
    body_,
    "\\bottomrule",
    "\\end{tabular}",
    "\\endgroup"
  )
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  n_d_ <- sum(rows_$Delayed == 1L)
  n_a_ <- sum(rows_$Delayed == 0L)
  note_ <- c(
    "This table compares the Item 1.01 narratives of delayed and attached announcements on the",
    "announcement sample: Item 1.01 8-Ks announcing a single agreement and carrying at most one",
    paste0("Exhibit 10 (N = ", format(n_d_ + n_a_, big.mark = ","), "; ", format(n_d_, big.mark = ","),
           " delayed, ", format(n_a_, big.mark = ","), " attached)."),
    if ("LagDays" %in% .measures) {
      paste("The measures are defined in the summary measures table; the announcement lag is winsorised",
            "at the first and ninety-ninth percentile.")
    } else {
      "The measures are defined in the summary measures table."
    },
    "Difference is the delayed mean less the attached mean; t-stat is Welch's t. ***, ** and * denote",
    "significance at the 1, 5 and 10 percent level."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_) <- c("Variable", "Delayed", "Attached", "Delayed", "Attached", "Difference", "t-stat")
  k_ <- cells_ |>
    knitr::kable(
      format   = "html",
      escape   = TRUE,
      caption  = "Item 1.01 summaries, delayed against attached",
      align    = "lrrrrrr",
      booktabs = TRUE
    ) |>
    kableExtra::kable_styling(
      full_width        = FALSE,
      position          = "left",
      bootstrap_options = c("hover", "condensed")
    ) |>
    kableExtra::add_header_above(c(" " = 1, "Obs." = 2, "Mean" = 2, "Delayed - Attached" = 2)) |>
    kableExtra::footnote(
      general           = paste(note_, collapse = " "),
      general_title     = "Notes:",
      footnote_as_chunk = TRUE,
      threeparttable    = TRUE
    )
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(cells_)
  )
  cli::cli_alert_info(
    "{format(n_d_, big.mark = ',')} delayed against {format(n_a_, big.mark = ',')} attached; \\
     words {fin_fmt_num(tab_$MeanDelayed[1L], 1L)} / {fin_fmt_num(tab_$MeanAttached[1L], 1L)} \\
     (revision 537.3 / 461.0 on 86,056 / 61,349)."
  )
  invisible(tab_)
}

#' Match each announcement to the firm's first fiscal quarter ending on or after the filing date
#'
#' Delayed announcements carry no contract row and hence no Compustat key; attached ones do, but the
#' two groups must be matched by the same rule, so both go through this one: the same CIK, the
#' first quarter end on or after the filing, and at most .window days later, which is 102's window.
#'
#' @param .con A DuckDB connection with Summaries and Quarter registered.
#' @param .sample Character. The announcement membership column.
#' @param .window Integer. Maximum days from the filing to the quarter end.
#' @return Tibble: HashIndex, gvkey, fyear, fqtr.
fin_summary_quarters <- function(.con, .sample = "S7_Summaries", .window = 100L) {
  if (FALSE) {
    .con    <- con
    .sample <- "S7_Summaries"
    .window <- 100L
  }
  DBI::dbGetQuery(.con, paste0(
    "WITH s AS (SELECT HashIndex, LPAD(CAST(CIK AS VARCHAR), 10, '0') AS cik, CAST(DateFiled AS DATE) AS filed ",
    "           FROM Summaries WHERE \"", .sample, "\"), ",
    "     j AS (SELECT s.HashIndex, q.gvkey, q.fyear, q.fqtr, ",
    "                  ROW_NUMBER() OVER (PARTITION BY s.HashIndex ORDER BY CAST(q.datadate AS DATE)) AS rn ",
    "           FROM s JOIN Quarter q ON q.cik = s.cik AND q.S5_Quarter ",
    "             AND CAST(q.datadate AS DATE) >= s.filed ",
    "             AND DATEDIFF('day', s.filed, CAST(q.datadate AS DATE)) <= ", .window, ") ",
    "SELECT HashIndex, gvkey, fyear, fqtr FROM j WHERE rn = 1"
  )) |>
    tibble::as_tibble()
}

#' One outcome, one specification, estimated
#' @param .rows Tibble with the outcome, Delayed, Year, CIK and any controls.
#' @param .outcome Character. The outcome column.
#' @param .spec Integer. 1 year effects; 2 year and firm effects; 3 as 2 with controls.
#' @param .controls Character. Control columns, used by spec 3.
#' @return One-row tibble: Outcome, Spec, Coef, TStat, PValue, N, R2Within.
fin_summary_fit <- function(.rows, .outcome, .spec, .controls) {
  if (FALSE) {
    .rows     <- rows_
    .outcome  <- "Words"
    .spec     <- 2L
    .controls <- .fin_controls$Resubmission
  }
  d_ <- .rows[is.finite(.rows[[.outcome]]), , drop = FALSE]
  if (.spec == 3L) d_ <- d_[stats::complete.cases(d_[, .controls, drop = FALSE]), , drop = FALSE]
  rhs_ <- if (.spec == 3L) paste(c("Delayed", .controls), collapse = " + ") else "Delayed"
  fe_  <- if (.spec == 1L) "Year" else "Year + CIK"
  f_   <- stats::as.formula(paste0(.outcome, " ~ ", rhs_, " | ", fe_))
  m_ <- fixest::feols(
    fml      = f_,
    data     = d_,
    cluster  = ~CIK,
    fixef.rm = "singleton",
    notes    = FALSE                 # the singleton note, once per fit, is not a finding
  )
  ct_ <- fixest::coeftable(m_)
  tibble::tibble(
    Outcome  = .outcome,
    Spec     = .spec,
    Coef     = unname(ct_["Delayed", "Estimate"]),
    TStat    = unname(ct_["Delayed", "t value"]),
    PValue   = unname(ct_["Delayed", "Pr(>|t|)"]),
    N        = stats::nobs(m_),
    R2Within = unname(fixest::r2(m_, type = "wr2"))
  )
}

#' The regression table: nine outcomes across, three specifications down
#'
#' @param .ds_summaries The prepared Summaries dataset.
#' @param .ds_quarter The prepared Quarter dataset, for the controls.
#' @param .con A DuckDB connection with Summaries and Quarter registered, for the quarter match.
#' @param .sample Character. The announcement membership column.
#' @param .measures Character. Which outcomes, in column order; the default is all nine.
#' @param .controls Character. The control set for the third specification.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the long tibble of estimates.
fin_table_summaries_regression <- function(.ds_summaries, .ds_quarter, .con, .sample = "S7_Summaries",
                                           .measures = .fin_summary_measures$Measure,
                                           .controls = .fin_controls$Resubmission, .dirs,
                                           .name = "SummariesRegression") {
  if (FALSE) {
    .ds_summaries <- lst_ds$Summaries
    .ds_quarter   <- lst_ds$Quarter
    .con          <- con
    .sample       <- "S7_Summaries"
    .measures     <- .fin_summary_measures$Measure
    .controls     <- .fin_controls$Resubmission
    .dirs         <- .lP$Output
    .name         <- "SummariesRegression"
  }
  rows_ <- fin_summary_rows(
    .ds_summaries = .ds_summaries,
    .sample       = .sample
  )
  keys_ <- fin_summary_quarters(
    .con    = .con,
    .sample = .sample
  )
  ctrl_ <- .ds_quarter |>
    dplyr::select("gvkey", "fyear", "fqtr", dplyr::all_of(.controls)) |>
    dplyr::collect() |>
    dplyr::distinct(.data$gvkey, .data$fyear, .data$fqtr, .keep_all = TRUE)
  rows_ <- rows_ |>
    dplyr::left_join(
      keys_,
      by           = dplyr::join_by(HashIndex),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      ctrl_,
      by           = dplyr::join_by(gvkey, fyear, fqtr),
      relationship = "many-to-one"
    )
  n_matched_ <- sum(!is.na(rows_$gvkey))
  cli::cli_alert_info(
    "{format(nrow(rows_), big.mark = ',')} announcements; {format(n_matched_, big.mark = ',')} matched to a fiscal \\
     quarter; {format(sum(stats::complete.cases(rows_[, .controls])), big.mark = ',')} with every control."
  )

  grid_ <- tidyr::expand_grid(Outcome = .measures, Spec = 1:3)
  est_ <- purrr::map2(grid_$Outcome, grid_$Spec, \(.o, .s) fin_summary_fit(
    .rows     = rows_,
    .outcome  = .o,
    .spec     = .s,
    .controls = .controls
  )) |>
    purrr::list_rbind() |>
    dplyr::mutate(Sample = .sample, .before = 1L)
  fin_save_data(
    .tab  = est_,
    .name = .name,
    .dir  = .dirs$DirData
  )

  # THE PAGE: one block per specification, the outcomes across. Each block is the coefficient with
  # stars, its t in parentheses, the effects and controls lines, N and the within R-squared.
  short_ <- .fin_summary_measures$Short[match(.measures, .fin_summary_measures$Measure)]
  block_ <- function(.s, .tex) {
    e_ <- est_[est_$Spec == .s, , drop = FALSE]
    e_ <- e_[match(.measures, e_$Outcome), , drop = FALSE]
    star_ <- if (.tex) paste0("$^{", fin_stars(e_$PValue), "}$") else fin_stars(e_$PValue)
    yes_ <- function(.x) if (.x) "Yes" else "No"
    tibble::tibble(
      Row = c("Delayed", "", "Year effects", "Firm effects", "Controls", "N", "Within R2"),
      !!!purrr::set_names(purrr::map(seq_len(nrow(e_)), \(.i) c(
        paste0(fin_fmt_num(e_$Coef[.i], 3L), star_[.i]),
        paste0("(", fin_fmt_num(e_$TStat[.i], 2L), ")"),
        "Yes", yes_(.s >= 2L), yes_(.s == 3L),
        fin_fmt_count(e_$N[.i]),
        fin_fmt_num(e_$R2Within[.i], 3L)
      )), short_)
    )
  }
  heads_ <- c("Year effects", "Year and firm effects", "Year and firm effects, controls")
  body_  <- character(0)
  html_  <- list()
  for (s_ in 1:3) {
    b_ <- block_(.s = s_, .tex = TRUE)
    if (s_ > 1L) body_ <- c(body_, "\\addlinespace[1.5ex]")
    body_ <- c(body_, paste0("\\multicolumn{", ncol(b_), "}{l}{\\textit{(", s_, ") ", heads_[s_], "}} \\\\"))
    body_ <- c(body_, purrr::map_chr(seq_len(nrow(b_)), \(.i) paste0(paste(unlist(b_[.i, ]), collapse = " & "), " \\\\")))
    html_[[s_]] <- block_(.s = s_, .tex = FALSE)
  }
  lines_ <- c(
    "\\begingroup\\footnotesize",
    paste0("\\begin{tabular}{l ", strrep("r", length(short_)), "}"),
    "\\toprule",
    paste0(" & ", paste(fin_tex_escape(short_), collapse = " & "), " \\\\"),
    paste0(" & ", paste(paste0("(", seq_along(short_), ")"), collapse = " & "), " \\\\"),
    "\\midrule",
    body_,
    "\\bottomrule",
    "\\end{tabular}",
    "\\endgroup"
  )
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )

  note_ <- c(
    "This table regresses each summary measure on an indicator for a delayed announcement, on the",
    paste0("announcement sample (N = ", format(nrow(rows_), big.mark = ","), " in the first two specifications;"),
    "the third keeps the announcements matched to a Compustat fiscal quarter with every control present).",
    "The measures are defined in the summary measures table and are the columns; the specifications are",
    "the blocks: year effects; year and firm effects, firms with a single announcement dropped; and year",
    "and firm effects with the firm-quarter controls of the estimation tables, matched to the first",
    "fiscal quarter ending on or after the filing and at most one hundred days later.",
    if ("LagDays" %in% .measures) {
      "The announcement lag is winsorised at the first and ninety-ninth percentile."
    } else {
      NULL
    },
    "Standard errors are clustered by firm; t-statistics in parentheses. ***, ** and * denote",
    "significance at the 1, 5 and 10 percent level."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )

  shown_ <- purrr::list_rbind(html_)
  names(shown_)[1L] <- " "
  k_ <- shown_ |>
    knitr::kable(
      format   = "html",
      escape   = TRUE,
      caption  = "Item 1.01 summaries, delayed announcements in a regression",
      align    = paste0("l", strrep("r", length(short_))),
      booktabs = TRUE
    ) |>
    kableExtra::kable_styling(
      full_width        = FALSE,
      position          = "left",
      bootstrap_options = c("hover", "condensed"),
      font_size         = 12
    ) |>
    kableExtra::footnote(
      general           = paste(note_, collapse = " "),
      general_title     = "Notes:",
      footnote_as_chunk = TRUE,
      threeparttable    = TRUE
    )
  for (s_ in 1:3) {
    k_ <- kableExtra::pack_rows(
      kable_input   = k_,
      group_label   = paste0("(", s_, ") ", heads_[s_]),
      start_row     = (s_ - 1L) * 7L + 1L,
      end_row       = s_ * 7L,
      label_row_css = "text-align: left; border-bottom: 1px solid;"
    )
  }
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(shown_)
  )
  invisible(est_)
}


# 16. Tables: the announcement sample, before the tests --------------------------------------------------------------------
# Two overviews that precede the summary tables: how the announcement sample is selected from every
# Item 1.01 8-K the pipeline recovered, and what the announcement lag looks like before it is
# winsorised. Both are the documentation of a choice, on the page, with the N's.

#' The announcement ladder: every recovered Item 1.01 8-K down to the sample, by attachment
#'
#' The published filter reads the number of agreements from the number of distinct "On <Month> <D>,
#' <YYYY>" openings in the narrative: exactly one keeps the filing. It is a proxy, and the ladder
#' shows what it costs: the filings where no such date was read, the filings with two or more --
#' concurrent agreements, or one agreement whose narrative repeats a date -- and, on the filing side,
#' the 8-Ks carrying more than one Exhibit 10.
#'
#' @param .ds_summaries The prepared Summaries dataset.
#' @return Tibble: Rung, Kind (total / exclusion), All, Delayed, Attached.
fin_tab_summary_ladder <- function(.ds_summaries) {
  if (FALSE) .ds_summaries <- lst_ds$Summaries
  s_ <- .ds_summaries |>
    dplyr::filter(.data$S7_All) |>
    dplyr::select("SumAttached", "SumDates", "nExhibits") |>
    dplyr::collect() |>
    dplyr::mutate(
      Delayed = .data$SumAttached == 0L,
      NoDate  = is.na(.data$SumDates) | .data$SumDates == 0L,
      Multi   = !.data$NoDate & .data$SumDates >= 2L,
      Single  = !.data$NoDate & .data$SumDates == 1L,
      Exhib2  = .data$Single & .data$nExhibits > 1L,
      Keep    = .data$Single & .data$nExhibits <= 1L
    )
  n_ <- function(.f) c(sum(.f), sum(.f & s_$Delayed), sum(.f & !s_$Delayed))
  rung_ <- function(.name, .kind, .n) tibble::tibble(Rung = .name, Kind = .kind, All = .n[1L], Delayed = .n[2L],
                                                     Attached = .n[3L])
  dplyr::bind_rows(
    rung_("Item 1.01 announcements recovered", "total", n_(rep(TRUE, nrow(s_)))),
    rung_("No agreement date read in the narrative", "exclusion", -n_(s_$NoDate)),
    rung_("Two or more agreement dates", "exclusion", -n_(s_$Multi)),
    rung_("More than one Exhibit 10 on the 8-K", "exclusion", -n_(s_$Exhib2)),
    rung_("Announcement sample", "total", n_(s_$Keep))
  )
}

#' The announcement ladder: built, saved, written, shown
#' @param .ds_summaries The prepared Summaries dataset.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the ladder tibble.
fin_table_summary_sample <- function(.ds_summaries, .dirs, .name = "SummarySample") {
  if (FALSE) {
    .ds_summaries <- lst_ds$Summaries
    .dirs         <- .lP$Output
    .name         <- "SummarySample"
  }
  tab_ <- fin_tab_summary_ladder(.ds_summaries = .ds_summaries)
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  cells_ <- tibble::tibble(
    Rung     = tab_$Rung,
    All      = fin_fmt_count(tab_$All),
    Delayed  = fin_fmt_count(tab_$Delayed),
    Attached = fin_fmt_count(tab_$Attached)
  )
  body_ <- purrr::map_chr(seq_len(nrow(cells_)), \(.i) {
    lab_ <- fin_tex_escape(cells_$Rung[.i])
    num_ <- unlist(cells_[.i, -1L])
    if (tab_$Kind[.i] == "total") {
      lab_ <- paste0("\\textbf{", lab_, "}")
      num_ <- paste0("\\textbf{", num_, "}")
    } else {
      lab_ <- paste0("\\hspace{1em}", lab_)
    }
    paste0(paste(c(lab_, num_), collapse = " & "), " \\\\")
  })
  lines_ <- c(
    "\\begin{tabular}{l r r r}",
    "\\toprule",
    " & All & Delayed & Attached \\\\",
    "\\midrule",
    body_,
    "\\bottomrule",
    "\\end{tabular}"
  )
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  note_ <- c(
    "This table shows how the announcement sample is selected. The starting point is every 8-K whose",
    "Item 1.01 narrative the pipeline recovered. The published restriction to filings announcing a single",
    "agreement is implemented by counting the distinct agreement-date openings of the form 'On March 3,",
    "2015' in the narrative and keeping the filings with exactly one; filings in which no such date is",
    "read and filings naming two or more -- concurrent agreements, or one agreement whose narrative",
    "repeats a date -- are excluded. Filings carrying more than one Exhibit 10 are excluded on the same",
    "grounds. Delayed announcements carry no Exhibit 10; attached ones carry one."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_)[1L] <- " "
  k_ <- cells_ |>
    knitr::kable(
      format   = "html",
      escape   = TRUE,
      caption  = "The announcement sample",
      align    = "lrrr",
      booktabs = TRUE
    ) |>
    kableExtra::kable_styling(
      full_width        = FALSE,
      position          = "left",
      bootstrap_options = c("hover", "condensed")
    ) |>
    kableExtra::row_spec(which(tab_$Kind == "total"), bold = TRUE) |>
    kableExtra::add_indent(which(tab_$Kind == "exclusion")) |>
    kableExtra::footnote(
      general           = paste(note_, collapse = " "),
      general_title     = "Notes:",
      footnote_as_chunk = TRUE,
      threeparttable    = TRUE
    )
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(cells_)
  )
  invisible(tab_)
}

#' The announcement lag before winsorising: its tails, by attachment
#'
#' The lag is the filing date less the agreement date the narrative opens with, and a misread date
#' puts a few announcements hundreds of days off in either direction. This is the evidence for
#' clipping it, with the clip points the tables use.
#'
#' @param .ds_summaries The prepared Summaries dataset.
#' @param .sample Character. The membership column.
#' @param .winsor Numeric of length two. The probabilities the tables clip at.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the distribution tibble.
fin_table_summary_lag <- function(.ds_summaries, .sample = "S7_Summaries", .winsor = c(0.01, 0.99), .dirs,
                                  .name = "SummaryLag") {
  if (FALSE) {
    .ds_summaries <- lst_ds$Summaries
    .sample       <- "S7_Summaries"
    .winsor       <- c(0.01, 0.99)
    .dirs         <- .lP$Output
    .name         <- "SummaryLag"
  }
  rows_ <- fin_summary_rows(
    .ds_summaries = .ds_summaries,
    .sample       = .sample,
    .winsor       = NULL
  )
  one_ <- function(.d, .name) {
    v_ <- .d$LagRaw[!is.na(.d$LagRaw)]
    q_ <- stats::quantile(v_, c(0, 0.01, 0.05, 0.5, 0.95, 0.99, 1), names = FALSE)
    tibble::tibble(
      Group    = .name,
      N        = length(v_),
      Negative = 100 * mean(v_ < 0),
      Min      = q_[[1L]],
      P1       = q_[[2L]],
      P5       = q_[[3L]],
      Median   = q_[[4L]],
      P95      = q_[[5L]],
      P99      = q_[[6L]],
      Max      = q_[[7L]],
      Mean     = mean(v_),
      MeanWin  = mean(pmin(pmax(v_, stats::quantile(rows_$LagRaw, .winsor[[1L]], na.rm = TRUE)),
                           stats::quantile(rows_$LagRaw, .winsor[[2L]], na.rm = TRUE)))
    )
  }
  tab_ <- dplyr::bind_rows(
    one_(rows_, "All"),
    one_(rows_[rows_$Delayed == 1L, , drop = FALSE], "Delayed"),
    one_(rows_[rows_$Delayed == 0L, , drop = FALSE], "Attached")
  ) |>
    dplyr::mutate(Sample = .sample, .before = 1L)
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  shown_ <- tab_ |>
    dplyr::transmute(
      Group    = .data$Group,
      N        = fin_fmt_count(.data$N),
      `Negative (%)` = fin_fmt_num(.data$Negative, 1L),
      Min      = fin_fmt_num(.data$Min, 0L),
      P1       = fin_fmt_num(.data$P1, 0L),
      P5       = fin_fmt_num(.data$P5, 0L),
      Median   = fin_fmt_num(.data$Median, 0L),
      P95      = fin_fmt_num(.data$P95, 0L),
      P99      = fin_fmt_num(.data$P99, 0L),
      Max      = fin_fmt_num(.data$Max, 0L),
      Mean     = fin_fmt_num(.data$Mean, 2L),
      `Mean, winsorised` = fin_fmt_num(.data$MeanWin, 2L)
    )
  note_ <- paste(
    "Calendar days from the agreement date the narrative opens with to the filing date, on the",
    "announcement sample, before winsorising. A negative lag is a filing dated before the agreement",
    "date it announces, which is a misread date. The last column clips at the sample's",
    paste0(100 * .winsor[[1L]], "th and ", 100 * .winsor[[2L]], "th percentile, as the tables do.")
  )
  fin_tex_tabular(
    .tab   = shown_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex")),
    .align = paste0("l ", strrep("r", ncol(shown_) - 1L))
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  k_ <- shown_ |>
    knitr::kable(
      format   = "html",
      escape   = TRUE,
      caption  = "The announcement lag before winsorising",
      align    = paste0("l", strrep("r", ncol(shown_) - 1L)),
      booktabs = TRUE
    ) |>
    kableExtra::kable_styling(
      full_width        = FALSE,
      position          = "left",
      bootstrap_options = c("hover", "condensed")
    ) |>
    kableExtra::footnote(
      general           = note_,
      general_title     = "Notes:",
      footnote_as_chunk = TRUE,
      threeparttable    = TRUE
    )
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(shown_)
  )
  cli::cli_alert_info(
    "Lag on all announcements: min {fin_fmt_num(tab_$Min[1L], 0L)}, P1 {fin_fmt_num(tab_$P1[1L], 0L)}, median \\
     {fin_fmt_num(tab_$Median[1L], 0L)}, P99 {fin_fmt_num(tab_$P99[1L], 0L)}, max {fin_fmt_num(tab_$Max[1L], 0L)}; \\
     {fin_fmt_num(tab_$Negative[1L], 1L)} percent negative."
  )
  invisible(tab_)
}


# 17. Prepare: the classification panel ---------------------------------------------------------------------------------
# Nothing about the classifiers is in the release; it lives in the run folders of 03B, 03C and 03D
# and in the crown records those stages wrote. This step builds what 03E builds at render -- one row
# per labelled document and task, the truth beside every engine's out-of-fold prediction and score,
# NA where an engine declined -- and writes it beside the other prepared tables, keyed on the run
# folders and the crown records. Every classification table below is a group-by on it.

.fin_class_tasks  <- c("ClassDetailed", "ClassBroad", "AmendType")
.fin_class_tables <- c("ClassSample", "ClassPanel", "ClassCrowned", "ClassSweep")

#' Which configuration stands for each engine and task, as 03E assembles it
#'
#' The transformer at every context length 03B deployed and cross-validated on all folds; the
#' keyword table the catalogue marks as default; the best fully cross-validated LLM configuration
#' where that stage ran. Ships marks the transformer length 03B crowned.
#'
#' @param .dir_bert,.dir_kw,.dir_llm Character. The three training stages' output directories.
#' @return Tibble: Task, Engine, ConfigName, Ships.
fin_class_crowned <- function(.dir_bert, .dir_kw, .dir_llm) {
  if (FALSE) {
    .dir_bert <- init_create_script_dir(.dir_here = here::here(), .name_script = "03B-ClassifyTrainBERT")
    .dir_kw   <- init_create_script_dir(.dir_here = here::here(), .name_script = "03C-ClassifyTrainKeyword")
    .dir_llm  <- init_create_script_dir(.dir_here = here::here(), .name_script = "03D-ClassifyLLM")
  }
  bert_ <- clf_load_overall(.runs_roots = fs::path(.dir_bert, "runs")) |>
    dplyr::filter(!.data$Smoke, .data$ConfigName %in% orch_final_configs(.dir_bert = .dir_bert)) |>
    dplyr::summarise(
      LabelCol = dplyr::first(.data$LabelCol),
      MaxLen   = as.integer(dplyr::first(.data$MaxLen)),
      nFolds   = dplyr::n_distinct(.data$TestFold),
      .by = "ConfigName"
    ) |>
    dplyr::filter(.data$nFolds >= 5L) |>
    dplyr::transmute(
      Task       = .data$LabelCol,
      Engine     = paste0("Bert", .data$MaxLen),
      ConfigName = .data$ConfigName
    )
  crown_ <- arrow::read_parquet(fs::path(.dir_bert, "model_final", "deployed.parquet")) |>
    dplyr::transmute(
      Task     = .data$LabelCol,
      Baseline = paste0("Bert", as.integer(.data$MaxLen))
    )
  kw_ <- arrow::read_parquet(fs::path(.dir_kw, "table", "catalogue.parquet")) |>
    dplyr::filter(.data$Default) |>
    dplyr::transmute(
      Task       = .data$Task,
      Engine     = "Kw",
      ConfigName = .data$ConfigName
    )
  llm_ <- if (fs::dir_exists(fs::path(.dir_llm, "runs"))) {
    clf_load_overall(.runs_roots = fs::path(.dir_llm, "runs")) |>
      dplyr::filter(!.data$Smoke) |>
      dplyr::summarise(
        MacroF1  = mean(.data$F1_macro),
        nFolds   = dplyr::n_distinct(.data$TestFold),
        LabelCol = dplyr::first(.data$LabelCol),
        .by = "ConfigName"
      ) |>
      dplyr::filter(.data$nFolds >= 5L) |>
      dplyr::slice_max(
        .data$MacroF1,
        n         = 1L,
        by        = "LabelCol",
        with_ties = FALSE
      ) |>
      dplyr::transmute(
        Task       = .data$LabelCol,
        Engine     = "Llm",
        ConfigName = .data$ConfigName
      )
  } else {
    tibble::tibble(
      Task       = character(),
      Engine     = character(),
      ConfigName = character()
    )
  }
  dplyr::bind_rows(bert_, llm_, kw_) |>
    dplyr::filter(.data$Task %in% .fin_class_tasks) |>
    dplyr::left_join(
      crown_,
      by = dplyr::join_by(Task)
    ) |>
    dplyr::mutate(
      Ships    = .data$Engine == .data$Baseline,
      Baseline = NULL
    ) |>
    dplyr::arrange(.data$Task, .data$Engine)
}

#' Prepare the four classification tables
#'
#' ClassSample: the labelled documents without their text, with a word count. ClassPanel: one row
#' per task and document, the truth and every engine's out-of-fold prediction and score, wide by
#' engine, NA where an engine declined; the keyword arm on AmendType is silenced outside Amended as
#' 03E does. ClassCrowned: which configuration stands for each engine. ClassSweep: 03B's per-fold
#' metrics for every configuration, the model-selection evidence.
#'
#' @param .dir_prep,.dir_bert,.dir_kw,.dir_llm Character. The 03A-03D output directories.
#' @param .dir Character. Output/Prepared.
#' @param .rerun Logical. TRUE rebuilds regardless of what is on disk.
#' @return Invisibly, the paths written.
fin_prepare_classification <- function(.dir_prep, .dir_bert, .dir_kw, .dir_llm, .dir, .rerun = FALSE) {
  if (FALSE) {
    .dir_prep <- init_create_script_dir(.dir_here = here::here(), .name_script = "03A-ClassifyPrepare")
    .dir_bert <- init_create_script_dir(.dir_here = here::here(), .name_script = "03B-ClassifyTrainBERT")
    .dir_kw   <- init_create_script_dir(.dir_here = here::here(), .name_script = "03C-ClassifyTrainKeyword")
    .dir_llm  <- init_create_script_dir(.dir_here = here::here(), .name_script = "03D-ClassifyLLM")
    .dir      <- .lP$Output$DirPrepared
    .rerun    <- FALSE
  }
  path_prep_ <- fs::path(.dir_prep, "prepared.parquet")
  runs_ <- fs::path(c(.dir_bert, .dir_kw, .dir_llm), "runs")
  runs_ <- runs_[fs::dir_exists(runs_)]
  ins_  <- c(path_prep_, fs::path(.dir_bert, "model_final", "deployed.parquet"),
             fs::path(.dir_kw, "table", "catalogue.parquet"), runs_)
  hit_ <- fin_prepared_hit(
    .name     = "ClassPanel",
    .dir      = .dir,
    .paths_in = ins_,
    .rerun    = .rerun
  )
  if (hit_ && all(fs::file_exists(fs::path(.dir, paste0(.fin_class_tables, ".parquet"))))) {
    return(invisible(fs::path(.dir, paste0(.fin_class_tables, ".parquet"))))
  }
  if (!fs::file_exists(path_prep_)) cli::cli_abort("03A has not run: {.file {path_prep_}} is missing.")

  prep_ <- arrow::read_parquet(path_prep_)
  sample_ <- prep_ |>
    dplyr::mutate(nWords = stringi::stri_count_words(.data$Text)) |>
    dplyr::select(-dplyr::any_of(c("Text")))
  crowned_ <- fin_class_crowned(
    .dir_bert = .dir_bert,
    .dir_kw   = .dir_kw,
    .dir_llm  = .dir_llm
  )
  panel_ <- purrr::map(.fin_class_tasks, \(.t) {
    cfg_ <- crowned_[crowned_$Task == .t, c("Engine", "ConfigName"), drop = FALSE]
    if (nrow(cfg_) == 0L) return(NULL)
    long_ <- orch_read(
      .runs_roots = runs_,
      .configs    = cfg_
    )
    orch_panel(
      .long        = long_,
      .commit_only = if (.t == "AmendType") "Amended" else NULL
    ) |>
      dplyr::mutate(
        Task    = .t,
        .before = 1L
      )
  }) |>
    purrr::list_rbind()
  sweep_ <- clf_load_overall(.runs_roots = fs::path(.dir_bert, "runs")) |>
    dplyr::filter(!.data$Smoke)

  fin_write_prepared(
    .tab  = sample_,
    .name = "ClassSample",
    .dir  = .dir
  )
  fin_write_prepared(
    .tab  = panel_,
    .name = "ClassPanel",
    .dir  = .dir
  )
  fin_write_prepared(
    .tab  = crowned_,
    .name = "ClassCrowned",
    .dir  = .dir
  )
  fin_write_prepared(
    .tab  = sweep_,
    .name = "ClassSweep",
    .dir  = .dir
  )
  invisible(fs::path(.dir, paste0(.fin_class_tables, ".parquet")))
}


# 18. Tables: classification --------------------------------------------------------------------------------------------
# All of them for the authors until the manuscript's set is chosen. Every score is out-of-fold:
# five folds dealt once in 03A, each document predicted once by a model that never saw it. Where a
# table is by category the rows follow the Categories table, the super rows scoring the detailed
# predictions rolled up to their parent.

#' Per-label scores of one engine's predictions against the truth, with the totals
#'
#' One-vs-rest per label: support, how many were predicted as it, coverage (share of its documents
#' the engine committed on), precision, recall over every document, recall over the committed ones,
#' F1, and lenient recall where a second valid label is supplied. The total row is macro over labels
#' for precision, recall and F1, and overall for coverage and accuracy.
#'
#' THE UNCERTAINTY IS THE SPREAD ACROSS FOLDS. The pooled score is one number, but five fold models
#' produced it; with .fold given, every metric is also computed within each fold and its standard
#' deviation across folds is returned in a column suffixed SD. On the Total row that is the SD of
#' the fold-level overall score, which is what a reader comparing two engines wants to see.
#'
#' @param .truth Character. True labels.
#' @param .pred Character. Predicted labels, NA where the engine declined.
#' @param .truth2 Character or NULL. Second valid label, NA where none.
#' @param .labels Character. The labels to score, in order.
#' @param .fold Integer or NULL. The fold each document was held out in; NULL returns no SD columns.
#' @return Tibble: Label, Support, Predicted, Coverage, Accuracy, Precision, Recall, RecallSel, F1,
#'   RecallLenient, with a final "Total" row; plus <metric>SD columns when .fold is given.
fin_class_metrics <- function(.truth, .pred, .truth2 = NULL, .labels = sort(unique(.truth)), .fold = NULL) {
  if (FALSE) {
    .truth  <- c("A", "A", "B", "B")
    .pred   <- c("A", "B", "B", NA)
    .truth2 <- c(NA, "B", NA, NA)
    .labels <- c("A", "B")
    .fold   <- c(1L, 2L, 1L, 2L)
  }
  if (!is.null(.fold)) {
    pooled_ <- fin_class_metrics(
      .truth  = .truth,
      .pred   = .pred,
      .truth2 = .truth2,
      .labels = .labels,
      .fold   = NULL
    )
    per_ <- purrr::map(sort(unique(.fold)), \(.f) {
      in_ <- .fold == .f
      fin_class_metrics(
        .truth  = .truth[in_],
        .pred   = .pred[in_],
        .truth2 = if (is.null(.truth2)) NULL else .truth2[in_],
        .labels = .labels,
        .fold   = NULL
      ) |>
        dplyr::mutate(Fold = .f)
    }) |>
      purrr::list_rbind()
    metrics_ <- c("Coverage", "Accuracy", "Precision", "Recall", "RecallSel", "F1", "RecallLenient")
    sd_ <- per_ |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(metrics_), \(.x) stats::sd(.x, na.rm = TRUE), .names = "{.col}SD"),
        .by = "Label"
      )
    return(dplyr::left_join(
      pooled_,
      sd_,
      by           = dplyr::join_by(Label),
      relationship = "one-to-one"
    ))
  }
  n_    <- length(.truth)
  hit_  <- dplyr::coalesce(.pred == .truth, FALSE)
  len_  <- if (is.null(.truth2)) hit_ else hit_ | dplyr::coalesce(.pred == .truth2, FALSE)
  rows_ <- purrr::map(.labels, \(.k) {
    is_ <- .truth == .k
    said_ <- dplyr::coalesce(.pred == .k, FALSE)
    tp_ <- sum(is_ & said_)
    com_ <- sum(is_ & !is.na(.pred))
    tibble::tibble(
      Label         = .k,
      Support       = sum(is_),
      Predicted     = sum(said_),
      Coverage      = if (sum(is_) > 0L) com_ / sum(is_) else NA_real_,
      Accuracy      = (tp_ + sum(!is_ & !said_)) / n_,
      Precision     = if (sum(said_) > 0L) tp_ / sum(said_) else NA_real_,
      Recall        = if (sum(is_) > 0L) tp_ / sum(is_) else NA_real_,
      RecallSel     = if (com_ > 0L) tp_ / com_ else NA_real_,
      RecallLenient = if (sum(is_) > 0L) sum(is_ & len_) / sum(is_) else NA_real_
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(F1 = dplyr::if_else(
      is.finite(.data$Precision) & is.finite(.data$Recall) & (.data$Precision + .data$Recall) > 0,
      2 * .data$Precision * .data$Recall / (.data$Precision + .data$Recall), 0
    ))
  tot_ <- tibble::tibble(
    Label         = "Total",
    Support       = n_,
    Predicted     = sum(!is.na(.pred)),
    Coverage      = mean(!is.na(.pred)),
    Accuracy      = mean(hit_),
    Precision     = mean(rows_$Precision, na.rm = TRUE),
    Recall        = mean(rows_$Recall, na.rm = TRUE),
    RecallSel     = mean(rows_$RecallSel, na.rm = TRUE),
    RecallLenient = mean(len_),
    F1            = mean(rows_$F1)
  )
  dplyr::bind_rows(rows_, tot_)
}

#' Lay per-label scores out in the Categories order: super rows from the broad scores, sub rows from the detailed
#'
#' @param .det Tibble from fin_class_metrics() on detailed labels (Label = 30's class names).
#' @param .broad Tibble from fin_class_metrics() on the same predictions rolled up (Label = Level1).
#' @return Tibble: Row, Kind, Level1, Class, then the score columns; Total from .det.
fin_class_layout <- function(.det, .broad) {
  if (FALSE) {
    .det   <- det_
    .broad <- broad_
  }
  cat_ <- fin_tab_categories()
  out_ <- list()
  for (l1_ in unique(cat_$Level1)) {
    sub_ <- cat_[cat_$Level1 == l1_, , drop = FALSE]
    b_   <- .broad[.broad$Label == l1_, , drop = FALSE]
    out_[[length(out_) + 1L]] <- dplyr::bind_cols(
      tibble::tibble(Row = l1_, Kind = "super", Level1 = l1_, Class = NA_character_),
      dplyr::select(b_, -"Label")
    )
    if (nrow(sub_) > 1L) {
      for (i_ in seq_len(nrow(sub_))) {
        d_ <- .det[.det$Label == sub_$Class[i_], , drop = FALSE]
        out_[[length(out_) + 1L]] <- dplyr::bind_cols(
          tibble::tibble(Row = sub_$Level2[i_], Kind = "sub", Level1 = l1_, Class = sub_$Class[i_]),
          dplyr::select(d_, -"Label")
        )
      }
    }
  }
  out_[[length(out_) + 1L]] <- dplyr::bind_cols(
    tibble::tibble(Row = "Total", Kind = "total", Level1 = NA_character_, Class = NA_character_),
    dplyr::select(.det[.det$Label == "Total", , drop = FALSE], -"Label")
  )
  purrr::list_rbind(out_)
}

#' The detailed panel with the second label and the parent of truth and prediction
#' @param .ds_panel The prepared ClassPanel dataset.
#' @param .ds_sample The prepared ClassSample dataset.
#' @param .engine Character. The engine column to read.
#' @return Tibble: DocID, Fold, Truth, Truth2, Pred, Score, TruthBroad, PredBroad.
fin_class_detailed <- function(.ds_panel, .ds_sample, .engine) {
  if (FALSE) {
    .ds_panel  <- lst_ds_class$ClassPanel
    .ds_sample <- lst_ds_class$ClassSample
    .engine    <- "Bert256"
  }
  cols_ <- c("DocID", "Fold", "Truth", .engine, paste0(.engine, "Score"))
  p_ <- .ds_panel |>
    dplyr::filter(.data$Task == "ClassDetailed") |>
    dplyr::select(dplyr::all_of(cols_)) |>
    dplyr::collect()
  names(p_) <- c("DocID", "Fold", "Truth", "Pred", "Score")
  s_ <- .ds_sample |>
    dplyr::select(
      "DocID",
      Truth2 = "ClassDetailed2"
    ) |>
    dplyr::collect()
  map_ <- fin_tab_categories() |> dplyr::select("Class", "Level1")
  p_ |>
    dplyr::left_join(
      s_,
      by           = dplyr::join_by(DocID),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      dplyr::rename(map_, Truth = "Class", TruthBroad = "Level1"),
      by = dplyr::join_by(Truth)
    ) |>
    dplyr::left_join(
      dplyr::rename(map_, Pred = "Class", PredBroad = "Level1"),
      by = dplyr::join_by(Pred)
    )
}

#' Which transformer length ships for a task
#' @param .ds_crowned The prepared ClassCrowned dataset.
#' @param .task Character.
#' @return Character, e.g. "Bert256".
fin_class_ships <- function(.ds_crowned, .task) {
  if (FALSE) {
    .ds_crowned <- lst_ds_class$ClassCrowned
    .task       <- "ClassDetailed"
  }
  c_ <- .ds_crowned |>
    dplyr::filter(.data$Task == .task, .data$Ships) |>
    dplyr::collect()
  if (nrow(c_) != 1L) cli::cli_abort("{nrow(c_)} shipping configurations for {(.task)}; expected one.")
  c_$Engine
}

# -- 18.1 The labelled sample -----------------------------------------------------------------------------------------

#' The labelled sample by category, with the amendment label as a second block
#' @param .ds_sample The prepared ClassSample dataset.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem.
#' @return Invisibly, the tibble.
fin_table_class_sample <- function(.ds_sample, .dirs, .name = "ClassLabelledFull") {
  if (FALSE) {
    .ds_sample <- lst_ds_class$ClassSample
    .dirs      <- .lP$Output
    .name      <- "ClassLabelledFull"
  }
  s_ <- dplyr::collect(.ds_sample)
  cat_ <- fin_tab_categories() |> dplyr::select("Class", "Level1", "Level2")
  rows_ <- s_ |>
    dplyr::left_join(
      dplyr::rename(cat_, ClassDetailed = "Class"),
      by = dplyr::join_by(ClassDetailed)
    ) |>
    dplyr::rename(Class = "ClassDetailed")
  stats_ <- function(.r) tibble::tibble(
    N       = nrow(.r),
    Share   = 100 * nrow(.r) / nrow(rows_),
    Second  = sum(!is.na(.r$ClassDetailed2)),
    Round1  = 100 * mean(.r$LabelRound == "Round1"),
    Words   = stats::median(.r$nWords)
  )
  tab_ <- fin_tab_by_category(
    .rows  = rows_,
    .stats = stats_
  )
  am_ <- purrr::map(c("Original", "Amended"), \(.a) dplyr::bind_cols(
    tibble::tibble(Row = .a, Kind = "sub", Level1 = "Amendment", Class = NA_character_),
    stats_(rows_[rows_$AmendType == .a, , drop = FALSE])
  )) |>
    purrr::list_rbind()
  tab_ <- dplyr::bind_rows(
    tab_[tab_$Kind != "total", , drop = FALSE],
    tibble::tibble(Row = "Amendment label", Kind = "super", Level1 = "Amendment", Class = NA_character_,
                   N = nrow(rows_), Share = 100, Second = NA_integer_, Round1 = NA_real_, Words = NA_real_),
    am_,
    tab_[tab_$Kind == "total", , drop = FALSE]
  )
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  cells_ <- tibble::tibble(
    Row    = tab_$Row,
    N      = fin_fmt_count(tab_$N),
    Share  = fin_fmt_num(tab_$Share, 1L),
    Second = fin_fmt_count(tab_$Second),
    Round1 = fin_fmt_num(tab_$Round1, 1L),
    Words  = fin_fmt_count(round(tab_$Words))
  )
  body_ <- fin_tex_category_body(
    .cells = cells_,
    .kind  = tab_$Kind
  )
  lines_ <- c(
    "\\begingroup\\small", "\\begin{tabular}{l r r r r r}", "\\toprule",
    " & N & Share (\\%) & Second label & Round 1 (\\%) & Median words \\\\", "\\midrule", body_, "\\bottomrule",
    "\\end{tabular}", "\\endgroup"
  )
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  note_ <- c(
    paste0("The hand-labelled training sample (N = ", format(nrow(rows_), big.mark = ","), " documents with"),
    "readable text), by category and by amendment label. Second label counts documents carrying a second",
    "valid category, which training never uses and lenient scoring credits. Round 1 is the share labelled",
    "in the first labelling round. Folds for cross-validation are dealt once on this sample, stratified",
    "on the detailed category."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_) <- c(" ", "N", "Share (%)", "Second label", "Round 1 (%)", "Median words")
  fin_show_category(
    .cells  = cells_,
    .kind   = tab_$Kind,
    .groups = c(" " = 1, "Labelled documents" = 5),
    .title  = "The labelled sample",
    .note   = paste(note_, collapse = " ")
  )
  invisible(tab_)
}

# -- 18.2 The transformer, out of fold ------------------------------------------------------------------------------------

#' A scores table by category, written and shown: the shared body of the transformer and keyword tables
#' @param .tab Tibble from fin_class_layout().
#' @param .cols Named character. Score columns to show, names are the headers; numeric shown to 3 decimals.
#' @param .name,.dirs,.title,.note As elsewhere.
#' @return Invisibly, .tab.
fin_class_show <- function(.tab, .cols, .name, .dirs, .title, .note) {
  if (FALSE) {
    .tab   <- tab_
    .cols  <- c(N = "Support", Precision = "Precision", Recall = "Recall", F1 = "F1")
    .name  <- "ClassTransformerFull"
    .dirs  <- .lP$Output
    .title <- "Transformer"
    .note  <- "A note."
  }
  fin_save_data(
    .tab  = .tab,
    .name = .name,
    .dir  = .dirs$DirData
  )
  # THE POOLED SCORE ALONE. The tibble keeps each metric's across-fold SD (the columns suffixed SD) for the
  # data file; the page prints the pooled score only, which keeps the table narrow (decided 17 September).
  # Counts and shares stay as they are.
  cells_ <- tibble::tibble(Row = .tab$Row)
  for (i_ in seq_along(.cols)) {
    col_ <- .cols[[i_]]
    v_   <- .tab[[col_]]
    cells_[[names(.cols)[i_]]] <- if (col_ %in% c("Support", "Predicted")) {
      fin_fmt_count(v_)
    } else if (grepl("\\(%\\)", names(.cols)[i_])) {
      fin_fmt_num(100 * v_, 1L)
    } else {
      dplyr::if_else(is.na(v_), "", fin_fmt_num(v_, 3L))
    }
  }
  body_ <- fin_tex_category_body(
    .cells = cells_,
    .kind  = .tab$Kind
  )
  lines_ <- c(
    "\\begingroup\\small",
    paste0("\\begin{tabular}{l ", strrep("r", length(.cols)), "}"),
    "\\toprule",
    paste0(" & ", paste(fin_tex_escape(names(.cols)), collapse = " & "), " \\\\"),
    "\\midrule", body_, "\\bottomrule", "\\end{tabular}", "\\endgroup"
  )
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  fin_note_write(
    .text = .note,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_)[1L] <- " "
  fin_show_category(
    .cells  = cells_,
    .kind   = .tab$Kind,
    .groups = c(" " = 1, "Out-of-fold scores" = length(.cols)),
    .title  = .title,
    .note   = paste(.note, collapse = " ")
  )
  invisible(.tab)
}

#' The transformer by category: the shipping model's out-of-fold scores, super rows rolled up
#' @param .ds_panel,.ds_sample,.ds_crowned The prepared classification datasets.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem.
#' @return Invisibly, the tibble.
fin_table_class_transformer <- function(.ds_panel, .ds_sample, .ds_crowned, .dirs, .name = "ClassTransformerFull") {
  if (FALSE) {
    .ds_panel   <- lst_ds_class$ClassPanel
    .ds_sample  <- lst_ds_class$ClassSample
    .ds_crowned <- lst_ds_class$ClassCrowned
    .dirs       <- .lP$Output
    .name       <- "ClassTransformerFull"
  }
  eng_ <- fin_class_ships(
    .ds_crowned = .ds_crowned,
    .task       = "ClassDetailed"
  )
  d_ <- fin_class_detailed(
    .ds_panel  = .ds_panel,
    .ds_sample = .ds_sample,
    .engine    = eng_
  )
  det_   <- fin_class_metrics(
    .truth  = d_$Truth,
    .pred   = d_$Pred,
    .truth2 = d_$Truth2,
    .labels = .fin_class_levels,
    .fold   = d_$Fold
  )
  broad_ <- fin_class_metrics(
    .truth  = d_$TruthBroad,
    .pred   = d_$PredBroad,
    .labels = unique(fin_tab_categories()$Level1),
    .fold   = d_$Fold
  )
  tab_ <- fin_class_layout(
    .det   = det_,
    .broad = broad_
  ) |>
    dplyr::mutate(
      Engine  = eng_,
      .before = 1L
    )
  tot_ <- tab_[tab_$Kind == "total", , drop = FALSE]
  bt_  <- broad_[broad_$Label == "Total", , drop = FALSE]
  note_ <- c(
    paste0("Out-of-fold scores of the transformer that ships (", eng_, ": context length ",
           sub("^Bert", "", eng_), " tokens) on the labelled sample (N = ", format(tot_$Support, big.mark = ","), ")."),
    "Each document is predicted once by the fold model that did not see it. Sub-category rows score the",
    "detailed prediction against the detailed label; super-category rows score the same predictions",
    "rolled up to their parent, so an error inside a parent does not count there. Accuracy per row is",
    "one-vs-rest; precision, recall and F1 are per category, and the Total row reports overall accuracy",
    "with macro precision, recall and F1. Lenient recall credits a prediction that equals the document's",
    "second valid category.",
    paste0("Overall: accuracy ", fin_fmt_num(tot_$Accuracy, 3L), " strict, ", fin_fmt_num(tot_$RecallLenient, 3L),
           " lenient; macro-F1 ", fin_fmt_num(tot_$F1, 3L), "; at the broad level accuracy ",
           fin_fmt_num(bt_$Accuracy, 3L), " and macro-F1 ", fin_fmt_num(bt_$F1, 3L), ".")
  )
  fin_class_show(
    .tab   = tab_,
    .cols  = c(N = "Support", Predicted = "Predicted", Accuracy = "Accuracy", Precision = "Precision",
               Recall = "Recall", F1 = "F1", `Lenient recall` = "RecallLenient"),
    .name  = .name,
    .dirs  = .dirs,
    .title = paste0("Transformer, out of fold (", eng_, ")"),
    .note  = note_
  )
  cli::cli_alert_info(
    "{(eng_)}: accuracy {fin_fmt_num(tot_$Accuracy, 3L)}, lenient {fin_fmt_num(tot_$RecallLenient, 3L)}, macro-F1 \\
     {fin_fmt_num(tot_$F1, 3L)}; broad accuracy {fin_fmt_num(bt_$Accuracy, 3L)}."
  )
  invisible(tab_)
}

# -- 18.3 The amendment task ------------------------------------------------------------------------------------------------

#' The amendment classifier: Original against Amended, out of fold
#' @param .ds_panel,.ds_crowned The prepared classification datasets.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem.
#' @return Invisibly, the tibble.
fin_table_class_amendment <- function(.ds_panel, .ds_crowned, .dirs, .name = "ClassAmendmentFull") {
  if (FALSE) {
    .ds_panel   <- lst_ds_class$ClassPanel
    .ds_crowned <- lst_ds_class$ClassCrowned
    .dirs       <- .lP$Output
    .name       <- "ClassAmendmentFull"
  }
  eng_ <- fin_class_ships(
    .ds_crowned = .ds_crowned,
    .task       = "AmendType"
  )
  p_ <- .ds_panel |>
    dplyr::filter(.data$Task == "AmendType") |>
    dplyr::select("DocID", "Fold", "Truth", dplyr::all_of(eng_)) |>
    dplyr::collect()
  m_ <- fin_class_metrics(
    .truth  = p_$Truth,
    .pred   = p_[[eng_]],
    .labels = c("Original", "Amended"),
    .fold   = p_$Fold
  ) |>
    dplyr::mutate(
      Engine  = eng_,
      .before = 1L
    )
  fin_save_data(
    .tab  = m_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  # THE POOLED SCORE ALONE; the fold SDs stay in the data file (decided 17 September).
  fmt3_ <- function(.v) dplyr::if_else(is.na(.v), "", fin_fmt_num(.v, 3L))
  cells_ <- tibble::tibble(
    Label     = m_$Label,
    N         = fin_fmt_count(m_$Support),
    Predicted = fin_fmt_count(m_$Predicted),
    Accuracy  = fmt3_(m_$Accuracy),
    Precision = fmt3_(m_$Precision),
    Recall    = fmt3_(m_$Recall),
    F1        = fmt3_(m_$F1)
  )
  kind_ <- c("sub", "sub", "total")
  body_ <- fin_tex_category_body(
    .cells = cells_,
    .kind  = kind_
  )
  body_ <- gsub("\\\\hspace\\{1em\\}", "", body_)
  lines_ <- c("\\begingroup\\small", "\\begin{tabular}{l r r r r r r}", "\\toprule",
              " & N & Predicted & Accuracy & Precision & Recall & F1 \\\\", "\\midrule", body_, "\\bottomrule",
              "\\end{tabular}", "\\endgroup")
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  tot_ <- m_[m_$Label == "Total", , drop = FALSE]
  note_ <- c(
    paste0("Out-of-fold scores of the amendment classifier that ships (", eng_, ") on the labelled sample"),
    paste0("(N = ", format(tot_$Support, big.mark = ","), "). Accuracy per row is one-vs-rest; the Total row"),
    "reports overall accuracy with macro precision, recall and F1."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_)[1L] <- " "
  fin_show_category(
    .cells  = cells_,
    .kind   = c("super", "super", "total"),
    .groups = c(" " = 1, "Out-of-fold scores" = 6),
    .title  = paste0("Amendment classifier, out of fold (", eng_, ")"),
    .note   = paste(note_, collapse = " ")
  )
  invisible(m_)
}

# -- 18.4 The keyword arm ---------------------------------------------------------------------------------------------------

#' The keyword arm by category: coverage, precision where it fired, recall over everything
#' @param .ds_panel,.ds_sample The prepared classification datasets.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem.
#' @return Invisibly, the tibble.
fin_table_class_keyword <- function(.ds_panel, .ds_sample, .dirs, .name = "ClassKeywordFull") {
  if (FALSE) {
    .ds_panel  <- lst_ds_class$ClassPanel
    .ds_sample <- lst_ds_class$ClassSample
    .dirs      <- .lP$Output
    .name      <- "ClassKeywordFull"
  }
  if (!"Kw" %in% names(.ds_panel)) cli::cli_abort("The panel carries no keyword arm.")
  d_ <- fin_class_detailed(
    .ds_panel  = .ds_panel,
    .ds_sample = .ds_sample,
    .engine    = "Kw"
  )
  det_   <- fin_class_metrics(
    .truth  = d_$Truth,
    .pred   = d_$Pred,
    .truth2 = d_$Truth2,
    .labels = .fin_class_levels,
    .fold   = d_$Fold
  )
  broad_ <- fin_class_metrics(
    .truth  = d_$TruthBroad,
    .pred   = d_$PredBroad,
    .labels = unique(fin_tab_categories()$Level1),
    .fold   = d_$Fold
  )
  tab_ <- fin_class_layout(
    .det   = det_,
    .broad = broad_
  ) |>
    dplyr::mutate(
      Engine  = "Kw",
      .before = 1L
    )
  tot_ <- tab_[tab_$Kind == "total", , drop = FALSE]
  note_ <- c(
    paste0("Out-of-fold scores of the keyword table on the labelled sample (N = ",
           format(tot_$Support, big.mark = ","), "). The keyword arm commits only where a term it trusts"),
    "fires and declines the rest, so two recalls are reported: over every document, where a declined",
    "document is a miss, and over the committed ones. Coverage is the share of a category's documents",
    "the arm committed on; precision is computed over what it predicted. Super-category rows score the",
    "predictions rolled up to their parent. The Total row reports overall coverage and accuracy with",
    "macro precision, recall and F1.",
    paste0("Overall: coverage ", fin_fmt_num(tot_$Coverage, 3L), ", accuracy ", fin_fmt_num(tot_$Accuracy, 3L),
           " over all documents and ", fin_fmt_num(tot_$RecallSel, 3L), " where it committed; macro-F1 ",
           fin_fmt_num(tot_$F1, 3L), ".")
  )
  fin_class_show(
    .tab   = tab_,
    .cols  = c(N = "Support", `Coverage (%)` = "Coverage", Predicted = "Predicted", Precision = "Precision",
               Recall = "Recall", `Recall, committed` = "RecallSel", F1 = "F1"),
    .name  = .name,
    .dirs  = .dirs,
    .title = "Keyword arm, out of fold",
    .note  = note_
  )
  cli::cli_alert_info(
    "Keyword arm: coverage {fin_fmt_num(tot_$Coverage, 3L)}, accuracy {fin_fmt_num(tot_$Accuracy, 3L)} \\
     ({fin_fmt_num(tot_$RecallSel, 3L)} where committed), macro-F1 {fin_fmt_num(tot_$F1, 3L)}."
  )
  invisible(tab_)
}

# -- 18.5 The arms compared ------------------------------------------------------------------------------------------------

#' Every engine on every task, and the routing ceiling on the detailed task
#' @param .ds_panel,.ds_crowned The prepared classification datasets.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem.
#' @return Invisibly, a list: Arms, Ceiling.
fin_table_class_arms <- function(.ds_panel, .ds_crowned, .dirs, .name = "ClassArmsFull") {
  if (FALSE) {
    .ds_panel   <- lst_ds_class$ClassPanel
    .ds_crowned <- lst_ds_class$ClassCrowned
    .dirs       <- .lP$Output
    .name       <- "ClassArmsFull"
  }
  crowned_ <- dplyr::collect(.ds_crowned)
  panel_   <- dplyr::collect(.ds_panel)
  arms_ <- purrr::map(.fin_class_tasks, \(.t) {
    p_ <- panel_[panel_$Task == .t, , drop = FALSE]
    if (nrow(p_) == 0L) return(NULL)
    engs_ <- crowned_$Engine[crowned_$Task == .t]
    engs_ <- engs_[engs_ %in% names(p_)]
    purrr::map(engs_, \(.e) {
      m_ <- fin_class_metrics(
        .truth = p_$Truth,
        .pred  = p_[[.e]],
        .fold  = p_$Fold
      )
      t_ <- m_[m_$Label == "Total", , drop = FALSE]
      tibble::tibble(
        Task        = .t,
        Engine      = .e,
        Ships       = isTRUE(crowned_$Ships[crowned_$Task == .t & crowned_$Engine == .e]),
        N           = t_$Support,
        Coverage    = t_$Coverage,
        Accuracy    = t_$Accuracy,
        AccuracySD  = t_$AccuracySD,
        AccuracySel = if (t_$Predicted > 0L) sum(dplyr::coalesce(p_[[.e]] == p_$Truth, FALSE)) / t_$Predicted else NA_real_,
        MacroF1     = t_$F1,
        MacroF1SD   = t_$F1SD,
        WeightedF1  = sum(m_$F1[m_$Label != "Total"] * m_$Support[m_$Label != "Total"]) / t_$Support
      )
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
  det_ <- panel_[panel_$Task == "ClassDetailed", , drop = FALSE]
  order_ <- c(fin_class_ships(.ds_crowned = .ds_crowned, .task = "ClassDetailed"),
              setdiff(arms_$Engine[arms_$Task == "ClassDetailed"], fin_class_ships(.ds_crowned = .ds_crowned,
                                                                                    .task = "ClassDetailed")))
  ceil_ <- orch_ceiling(
    .tab   = det_,
    .order = order_
  )
  fin_save_data(
    .tab  = arms_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  fin_save_data(
    .tab  = ceil_,
    .name = paste0(.name, "Ceiling"),
    .dir  = .dirs$DirData
  )

  task_lab_ <- c(ClassDetailed = "Detailed (12)", ClassBroad = "Broad (7)", AmendType = "Amendment (2)")
  cells_ <- tibble::tibble(
    Task        = unname(task_lab_[arms_$Task]),
    Engine      = paste0(arms_$Engine, dplyr::if_else(arms_$Ships, " (ships)", "")),
    N           = fin_fmt_count(arms_$N),
    Coverage    = fin_fmt_num(arms_$Coverage, 3L),
    Accuracy    = fin_fmt_num(arms_$Accuracy, 3L),
    AccuracySel = fin_fmt_num(arms_$AccuracySel, 3L),
    MacroF1     = fin_fmt_num(arms_$MacroF1, 3L),
    WeightedF1  = fin_fmt_num(arms_$WeightedF1, 3L)
  )
  ceil_cells_ <- tibble::tibble(
    Task     = "Detailed, ceiling",
    Engine   = ceil_$Engines,
    N        = fin_fmt_count(ceil_$nCorrect),
    Coverage = "", Accuracy = fin_fmt_num(ceil_$Accuracy, 3L), AccuracySel = "",
    MacroF1  = "", WeightedF1 = paste0("+", fin_fmt_num(ceil_$Marginal, 3L))
  )
  all_cells_ <- dplyr::bind_rows(cells_, ceil_cells_)
  body_ <- purrr::map_chr(seq_len(nrow(all_cells_)), \(.i) {
    r_ <- unlist(all_cells_[.i, ])
    r_[1:2] <- fin_tex_escape(r_[1:2])
    paste0(paste(r_, collapse = " & "), " \\\\")
  })
  body_ <- append(body_, "\\midrule", after = nrow(cells_))
  lines_ <- c("\\begingroup\\small", "\\begin{tabular}{l l r r r r r r}", "\\toprule",
              "Task & Engine & N & Coverage & Accuracy & Accuracy, committed & Macro-F1 & Weighted F1 \\\\",
              "\\midrule", body_, "\\bottomrule", "\\end{tabular}", "\\endgroup")
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  note_ <- c(
    "Every engine on every task, out of fold on the labelled sample. Coverage is the share of documents",
    "the engine committed on; accuracy is over every document, a declined document counting as a miss,",
    "and accuracy committed is over the committed ones. Macro-F1 weights every category equally,",
    "weighted F1 by support. Ships marks the transformer length the training stage crowned. The ceiling",
    "rows take the shipping transformer and add the other engines in turn, counting a document correct",
    "if any engine in the set got it right: the accuracy a perfect router could reach, and what each",
    "engine adds at most; the last column is that marginal gain."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(all_cells_) <- c("Task", "Engine", "N", "Coverage", "Accuracy", "Accuracy, committed", "Macro-F1", "Weighted F1")
  k_ <- all_cells_ |>
    knitr::kable(format = "html", escape = TRUE, caption = "The arms compared, out of fold", align = "llrrrrrr",
                 booktabs = TRUE) |>
    kableExtra::kable_styling(
      full_width        = FALSE,
      position          = "left",
      bootstrap_options = c("hover", "condensed")
    ) |>
    kableExtra::row_spec(
      nrow(cells_),
      extra_css = "border-bottom: 1px solid #333;"
    ) |>
    kableExtra::footnote(general = paste(note_, collapse = " "), general_title = "Notes:", footnote_as_chunk = TRUE,
                         threeparttable = TRUE)
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(all_cells_)
  )
  invisible(list(Arms = arms_, Ceiling = ceil_))
}

# -- 18.6 Model selection ---------------------------------------------------------------------------------------------------

#' The sweep: every transformer configuration by task and context length, the crown marked
#' @param .ds_sweep,.ds_crowned The prepared classification datasets.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem.
#' @return Invisibly, the tibble.
fin_table_class_sweep <- function(.ds_sweep, .ds_crowned, .dirs, .name = "ClassSweep") {
  if (FALSE) {
    .ds_sweep   <- lst_ds_class$ClassSweep
    .ds_crowned <- lst_ds_class$ClassCrowned
    .dirs       <- .lP$Output
    .name       <- "ClassSweep"
  }
  sw_ <- dplyr::collect(.ds_sweep)
  crowned_ <- dplyr::collect(.ds_crowned)
  grp_ <- intersect(c("LabelCol", "ConfigName", "Model", "MaxLen"), names(sw_))
  tab_ <- sw_ |>
    dplyr::summarise(
      Folds      = dplyr::n_distinct(.data$TestFold),
      Accuracy   = mean(.data$Accuracy),
      AccuracySD = stats::sd(.data$Accuracy),
      MacroF1    = mean(.data$F1_macro),
      MacroF1SD  = stats::sd(.data$F1_macro),
      WeightedF1 = mean(.data$F1_weighted),
      .by = dplyr::all_of(grp_)
    ) |>
    dplyr::mutate(
      Task   = .data$LabelCol,
      Crown  = .data$ConfigName %in% crowned_$ConfigName[crowned_$Ships],
      Deployed = .data$ConfigName %in% crowned_$ConfigName
    ) |>
    dplyr::arrange(match(.data$Task, .fin_class_tasks), dplyr::desc(.data$MacroF1))
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  task_lab_ <- c(ClassDetailed = "Detailed", ClassBroad = "Broad", AmendType = "Amendment")
  cells_ <- tibble::tibble(
    Task       = unname(task_lab_[tab_$Task]),
    Model      = if ("Model" %in% names(tab_)) as.character(tab_$Model) else "",
    MaxLen     = if ("MaxLen" %in% names(tab_)) fin_fmt_count(as.integer(tab_$MaxLen)) else "",
    Folds      = fin_fmt_count(tab_$Folds),
    Accuracy   = fin_fmt_num(tab_$Accuracy, 3L),
    MacroF1    = fin_fmt_num(tab_$MacroF1, 3L),
    WeightedF1 = fin_fmt_num(tab_$WeightedF1, 3L),
    Status     = dplyr::case_when(
      tab_$Crown ~ "ships",
      tab_$Deployed ~ "deployed",
      .default = ""
    )
  )
  body_ <- purrr::map_chr(seq_len(nrow(cells_)), \(.i) {
    r_ <- unlist(cells_[.i, ])
    r_[1:2] <- fin_tex_escape(r_[1:2])
    paste0(paste(r_, collapse = " & "), " \\\\")
  })
  lines_ <- c("\\begingroup\\footnotesize", "\\begin{tabular}{l l r r r r r l}", "\\toprule",
              "Task & Model & Context & Folds & Accuracy & Macro-F1 & Weighted F1 & \\\\", "\\midrule", body_,
              "\\bottomrule", "\\end{tabular}", "\\endgroup")
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  note_ <- c(
    "Every transformer configuration cross-validated, by task: the mean across the five folds of",
    "out-of-fold accuracy and macro-F1. Selection is on macro-F1; deployed marks the",
    "configurations refitted on the full sample at each context length, ships the one the paper uses."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_) <- c("Task", "Model", "Context", "Folds", "Accuracy", "Macro-F1", "Weighted F1", " ")
  k_ <- cells_ |>
    knitr::kable(format = "html", escape = TRUE, caption = "Model selection: the sweep", align = "llrrrrrl",
                 booktabs = TRUE) |>
    kableExtra::kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("hover", "condensed"),
                              font_size = 12) |>
    kableExtra::row_spec(
      which(tab_$Crown),
      bold = TRUE
    ) |>
    kableExtra::footnote(general = paste(note_, collapse = " "), general_title = "Notes:", footnote_as_chunk = TRUE,
                         threeparttable = TRUE)
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(cells_)
  )
  invisible(tab_)
}

# -- 18.7 The confusion matrix ----------------------------------------------------------------------------------------------

#' The detailed confusion matrix of the shipping transformer, in the Categories order
#' @param .ds_panel,.ds_sample,.ds_crowned The prepared classification datasets.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem.
#' @return Invisibly, the count matrix as a tibble (rows true, columns predicted).
fin_table_class_confusion <- function(.ds_panel, .ds_sample, .ds_crowned, .dirs, .name = "ClassConfusionFull") {
  if (FALSE) {
    .ds_panel   <- lst_ds_class$ClassPanel
    .ds_sample  <- lst_ds_class$ClassSample
    .ds_crowned <- lst_ds_class$ClassCrowned
    .dirs       <- .lP$Output
    .name       <- "ClassConfusionFull"
  }
  eng_ <- fin_class_ships(
    .ds_crowned = .ds_crowned,
    .task       = "ClassDetailed"
  )
  d_ <- fin_class_detailed(
    .ds_panel  = .ds_panel,
    .ds_sample = .ds_sample,
    .engine    = eng_
  )
  cat_ <- fin_tab_categories()
  lab_ <- dplyr::if_else(nzchar(cat_$Level2), cat_$Level2, cat_$Level1)
  t_ <- factor(d_$Truth, levels = cat_$Class, labels = lab_)
  p_ <- factor(d_$Pred,  levels = cat_$Class, labels = lab_)
  m_ <- table(True = t_, Predicted = p_, useNA = "no")
  tab_ <- tibble::as_tibble(
    as.data.frame.matrix(m_),
    rownames = "True"
  ) |>
    dplyr::mutate(
      Engine  = eng_,
      .before = 1L
    )
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  cells_ <- tibble::as_tibble(
    as.data.frame.matrix(m_),
    rownames = "True"
  ) |>
    dplyr::mutate(dplyr::across(-"True", \(.x) fin_fmt_count(as.integer(.x))))
  body_ <- purrr::map_chr(seq_len(nrow(cells_)), \(.i) {
    r_ <- unlist(cells_[.i, ])
    r_[1L] <- fin_tex_escape(r_[1L])
    paste0(paste(r_, collapse = " & "), " \\\\")
  })
  head_ <- paste0(" & ", paste(paste0("\\rotatebox{90}{", fin_tex_escape(lab_), "}"), collapse = " & "), " \\\\")
  lines_ <- c("\\begingroup\\footnotesize", paste0("\\begin{tabular}{l ", strrep("r", length(lab_)), "}"), "\\toprule",
              head_, "\\midrule", body_, "\\bottomrule", "\\end{tabular}", "\\endgroup")
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  note_ <- c(
    paste0("Confusion matrix of the shipping transformer (", eng_, ") on the labelled sample, out of fold:"),
    "rows are the true category, columns the predicted one, cells the number of documents. Categories",
    "are in the taxonomic order of the categories table, so an error that stays inside a parent sits",
    "next to the diagonal and one that crosses a parent sits further from it."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_)[1L] <- "True \\ Predicted"
  k_ <- cells_ |>
    knitr::kable(format = "html", escape = TRUE, caption = paste0("Confusion matrix, out of fold (", eng_, ")"),
                 align = paste0("l", strrep("r", length(lab_))), booktabs = TRUE) |>
    kableExtra::kable_styling(full_width = FALSE, position = "left", bootstrap_options = c("hover", "condensed"),
                              font_size = 11) |>
    kableExtra::column_spec(
      column    = 1L,
      extra_css = "white-space: nowrap;"
    ) |>
    kableExtra::footnote(general = paste(note_, collapse = " "), general_title = "Notes:", footnote_as_chunk = TRUE,
                         threeparttable = TRUE)
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(cells_)
  )
  invisible(tab_)
}


# 19. Figures --------------------------------------------------------------------------------------------------------------
# One function per figure, the same contract as the tables: the tibble behind the figure goes to
# Data/<Name>.parquet, the figure to Figures/<Name>.png through plot_save(), the note to
# Notes/<Name>.tex, and the figure is printed into the runbook. Where 30 already computes and draws
# the exhibit in the decided form, its fin_data_*() and fin_plot_*() are reused; a figure whose
# manuscript form differs gets a fin_plot_*() here.

#' Save a figure's three artifacts and print it
#' @param .plot A ggplot.
#' @param .data Tibble. The exhibit tibble behind it.
#' @param .note Character. The note, one string or sentences.
#' @param .name Character. The stem every file takes.
#' @param .dirs List. .lP$Output.
#' @param .height Numeric. Figure height in inches, from plot_height().
#' @param .width Numeric. Figure width in inches.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_save <- function(.plot, .data, .note, .name, .dirs, .height, .width = 7.5) {
  if (FALSE) {
    .plot   <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
    .data   <- tibble::tibble(x = 1)
    .note   <- "A note."
    .name   <- "Test"
    .dirs   <- .lP$Output
    .height <- plot_height(8L)
    .width  <- 7.5
  }
  fin_save_data(
    .tab  = .data,
    .name = .name,
    .dir  = .dirs$DirData
  )
  # PRINT FIRST, SAVE SECOND, AND PUT THE DEVICE BACK. Under knitr the figure is recorded on the device
  # that is current when it is printed. ggsave() opens a file device and closes it again, and when
  # the Cairo device fails to open -- which plot_save() retries around -- the close can land on the
  # device that was current before, which is knitr's. Printing before any file device is touched
  # records the figure first; restoring the device afterwards keeps the next chunk's figure safe too.
  print(.plot)
  dev_ <- grDevices::dev.cur()
  files_ <- plot_save(
    .plot    = .plot,
    .name    = .name,
    .dir     = .dirs$DirFigures,
    .height  = .height,
    .width   = .width,
    .formats = "png"            # Overleaf reads the png; no pdf is written (decided 17 September)
  )
  if (dev_ != 1L && dev_ %in% grDevices::dev.list() && grDevices::dev.cur() != dev_) grDevices::dev.set(dev_)
  fin_note_write(
    .text = .note,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  # THE NOTE UNDER THE FIGURE, as prose. Written inside pandoc's raw-html fence, as tbl_out() does, so
  # the paragraph is a note paragraph and not a console block; the chunk needs `results: asis`.
  cat("\n```{=html}\n<p class=\"table-note\"><em>Notes:</em> ", paste(.note, collapse = " "), "</p>\n```\n\n", sep = "")
  invisible(list(Plot = .plot, Data = .data, Files = files_))
}

# -- 19.1 Filing types ------------------------------------------------------------------------------------------------------

#' The filing-types figure in the manuscript's palette
#'
#' 30's figure, redrawn in the blue ramp the other manuscript figures use rather than the binary
#' greys the design layer registers for the amendment task: dark for original, light for amended,
#' as the revision printed it. Contracts without an amendment label keep their white segment in the
#' bar so the heights still sum to the sample, but leave the legend: they are the zero-word
#' contracts of the open ladder issue, invisible at their size, and a legend key for an invisible
#' segment is a question the note answers instead.
#'
#' @param .tab_data Tibble from fin_data_f01() for one sample.
#' @return A ggplot.
fin_plot_filing_types <- function(.tab_data) {
  if (FALSE) .tab_data <- fin_data_f01(.tab = rows_, .sample = "S2_Descriptive")
  blues_ <- plot_pal_seq(3L)
  fill_  <- c(Original = blues_[[3L]], Amended = blues_[[1L]], Unlabelled = "#FFFFFF")
  dat_ <- .tab_data |>
    dplyr::mutate(
      PlotForm = factor(.data$FormFamily, levels = .fin_form_levels),
      PlotFill = factor(.data$Amend, levels = names(fill_)),
      Region   = factor(.data$Region, levels = c("US filings", "Foreign filings"))
    )
  lab_ <- dat_ |>
    dplyr::distinct(.data$Region, .data$PlotForm, .data$NForm, .data$Share) |>
    dplyr::mutate(Label = scales::label_percent(accuracy = 0.1)(.data$Share))
  dat_ |>
    ggplot2::ggplot(ggplot2::aes(x = .data$PlotForm, y = .data$N, fill = .data$PlotFill)) +
    ggplot2::geom_col(
      position  = ggplot2::position_stack(reverse = TRUE),
      width     = 0.7,
      colour    = .plot_ref,
      linewidth = .plot_line
    ) +
    ggplot2::geom_text(
      data        = lab_,
      mapping     = ggplot2::aes(x = .data$PlotForm, y = .data$NForm, label = .data$Label),
      inherit.aes = FALSE,
      vjust       = -0.4,
      size        = .plot_base / ggplot2::.pt * 0.85,
      family      = .plot_font
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$Region), scales = "free_x", space = "free_x") +
    ggplot2::scale_fill_manual(values = fill_, breaks = c("Original", "Amended"), name = NULL) +
    plot_scale_y_count(.expand = c(0, 0.10)) +
    ggplot2::labs(x = NULL, y = "Contracts") +
    plot_theme(.grid = "y", .legend = "bottom")
}

#' The distribution of filing types: contracts by form, US and foreign, original and amended
#'
#' 30's data on the unique-contract sample, drawn by fin_plot_filing_types(); the revision's labels
#' are checked against the render through 30's own report.
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @param .ref Tibble. 30's reference labels for the figure.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_filing_types <- function(.ds_contracts, .dirs, .name = "FilingTypes", .ref = .fin_reference_f01) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .dirs         <- .lP$Output
    .name         <- "FilingTypes"
    .ref          <- .fin_reference_f01
  }
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S2_Descriptive) |>
    dplyr::select("IsForeign", "FormFamily", "AmendType") |>
    dplyr::collect()
  dat_ <- fin_data_f01(
    .tab    = rows_,
    .sample = "S2_Descriptive"
  )
  fin_report_f01(
    .tab_data = dat_,
    .ref      = .ref
  )
  n_ <- nrow(rows_)
  unlab_ <- sum(dat_$N[dat_$Amend == "Unlabelled"])
  note_ <- c(
    "This figure shows the distribution of material contracts across filing types, divided into filings",
    "of U.S. firms (left) and of foreign firms (right), on the unique-contract sample",
    paste0("(N = ", format(n_, big.mark = ","), "). Each bar is split into original agreements and amendments;"),
    "the label above a bar is the form's share of the sample. Amendment forms (e.g. 8-K/A) are counted",
    "with their base form.",
    if (unlab_ > 0L) {
      paste0(format(unlab_, big.mark = ","), " contracts without readable text carry no amendment label;",
             " they count in the bars and the shares but not in the split.")
    } else {
      NULL
    }
  )
  fin_figure_save(
    .plot   = fin_plot_filing_types(.tab_data = dat_),
    .data   = dat_,
    .note   = note_,
    .name   = .name,
    .dirs   = .dirs,
    .height = plot_height(8L)
  )
}

# -- 19.2 Filings over time -------------------------------------------------------------------------------------------------

# THE REGIME LINES, with the label each carries on the page.
.fin_regime_labels <- c(Reform2004 = "Form 8-K reform (Aug 2004)", Fast2019 = "FAST Act (Apr 2019)")

#' Filings over time in the manuscript's form: stacked shares, the yearly total as a line on a second axis
#'
#' The revision draws the total over the bars on a second axis, and the manuscript keeps that form;
#' the second axis is scaled so the largest year sits just under the top of the bars. The two regime
#' dates are dashed and labelled on the page. Both axes say their unit.
#'
#' @param .tab_data Tibble from fin_report_f02() for one sample: Year, FilingGroup4, N, Total, Share.
#' @param .line_colour Character. The total line's colour; a mid grey reads on the blue ramp.
#' @return A ggplot.
fin_plot_filings_time <- function(.tab_data, .line_colour = "#4D4D4D") {
  if (FALSE) {
    .tab_data    <- long_
    .line_colour <- "#4D4D4D"
  }
  dat_ <- .tab_data |>
    dplyr::mutate(
      PlotYear = as.numeric(.data$Year),
      PlotFill = plot_factor(.data$FilingGroup4, .key = "FilingGroup4", .short = TRUE)
    )
  fills_ <- purrr::set_names(plot_pal_seq(nlevels(dat_$PlotFill)), levels(dat_$PlotFill))
  tot_ <- dat_ |>
    dplyr::distinct(.data$PlotYear, .data$Total) |>
    dplyr::mutate(Thousand = .data$Total / 1000)
  scale_ <- max(tot_$Thousand) / 0.95            # the peak year sits at 95 percent of the bar height
  line_name_ <- "Contracts per year (right axis)"
  reg_ <- tibble::tibble(
    X     = unname(.fin_regime_years),
    Label = unname(.fin_regime_labels[names(.fin_regime_years)])
  )
  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$PlotYear, y = .data$Share, fill = .data$PlotFill)) +
    ggplot2::geom_col(
      position  = ggplot2::position_stack(reverse = TRUE),
      width     = 0.85,
      colour    = "white",
      linewidth = .plot_line
    ) +
    ggplot2::geom_vline(
      xintercept = reg_$X,
      linetype   = "dashed",
      colour     = "black",
      linewidth  = .plot_line * 2
    ) +
    ggplot2::geom_text(
      data        = reg_,
      mapping     = ggplot2::aes(x = .data$X, y = 1.02, label = .data$Label),
      inherit.aes = FALSE,
      hjust       = -0.05,
      vjust       = 0,
      size        = .plot_base / ggplot2::.pt * 0.8,
      family      = .plot_font
    ) +
    # THE LINE WEARS A HALO. One colour cannot separate from both the palest and the darkest bar, so
    # a wider white line sits under the grey one and the points carry a white rim.
    ggplot2::geom_line(
      data        = tot_,
      mapping     = ggplot2::aes(x = .data$PlotYear, y = .data$Thousand / scale_),
      inherit.aes = FALSE,
      colour      = "white",
      linewidth   = 1.1
    ) +
    ggplot2::geom_line(
      data        = tot_,
      mapping     = ggplot2::aes(x = .data$PlotYear, y = .data$Thousand / scale_, linetype = line_name_),
      inherit.aes = FALSE,
      colour      = .line_colour,
      linewidth   = 0.6
    ) +
    ggplot2::geom_point(
      data        = tot_,
      mapping     = ggplot2::aes(x = .data$PlotYear, y = .data$Thousand / scale_),
      inherit.aes = FALSE,
      colour      = .line_colour,
      size        = 1.0
    ) +
    ggplot2::scale_fill_manual(values = fills_, name = NULL) +
    ggplot2::scale_linetype_manual(
      values = purrr::set_names("solid", line_name_),
      name   = NULL,
      guide  = ggplot2::guide_legend(override.aes = list(colour = .line_colour, linewidth = 0.6))
    ) +
    ggplot2::scale_y_continuous(
      labels   = scales::label_percent(accuracy = 1),
      expand   = ggplot2::expansion(mult = c(0, 0.08)),
      sec.axis = ggplot2::sec_axis(~ . * scale_, name = "Contracts per year (in 1,000s)",
                                   labels = scales::label_comma())
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2), expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::labs(x = NULL, y = "Share of contracts (in %)") +
    plot_theme(.grid = "none", .legend = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1), linetype = ggplot2::guide_legend(order = 2))
}

#' Filings over time: each year's contracts by filing group, shares stacked, the year's total above
#'
#' 30's four-group data: registration statements stand on their own rather than inside ad-hoc, which
#' is what lets the figure show the 2021 spike for what it is. Drawn in the manuscript's form by
#' fin_plot_filings_time(). 30's report prints the yearly totals and the group shares, which is where
#' the manuscript's "80 thousand" is corrected.
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_filings_time <- function(.ds_contracts, .dirs, .name = "FilingsTime") {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .dirs         <- .lP$Output
    .name         <- "FilingsTime"
  }
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S2_Descriptive) |>
    dplyr::select("Year", "FormFamily") |>
    dplyr::collect()
  dat_ <- fin_data_f02(
    .tab    = rows_,
    .sample = "S2_Descriptive"
  )
  long_ <- fin_report_f02(.tab_data = dat_)
  reg_ <- long_ |>
    dplyr::filter(
      .data$FilingGroup4 == "Registration",
      .data$Year %in% c(2020L, 2021L, 2022L)
    )
  peak_ <- long_ |>
    dplyr::distinct(.data$Year, .data$Total) |>
    dplyr::slice_max(.data$Total, n = 1L)
  note_ <- c(
    "This figure shows the distribution of filed contracts over filing groups and time on the",
    paste0("unique-contract sample (N = ", format(nrow(rows_), big.mark = ","), "). The bars give each year's"),
    "contracts by the group of the filing they arrived in, as shares summing to one: current reports",
    "(8-K), registration statements (S-1, S-4, F-1, F-4), yearly reports (10-K, 20-F) and quarterly",
    "reports (10-Q); the line gives the year's number of contracts in thousands on the right axis. Amendment forms",
    "are counted with their base form. The dashed lines mark the 2004 amendments to Form 8-K and the",
    "2019 FAST Act.",
    paste0("The registration share is ", fin_fmt_num(100 * reg_$Share[reg_$Year == 2020L], 1L), " percent in 2020, ",
           fin_fmt_num(100 * reg_$Share[reg_$Year == 2021L], 1L), " percent in 2021 and ",
           fin_fmt_num(100 * reg_$Share[reg_$Year == 2022L], 1L), " percent in 2022; the peak year is ",
           peak_$Year, " with ", format(peak_$Total, big.mark = ","), " contracts.")
  )
  fin_figure_save(
    .plot   = fin_plot_filings_time(.tab_data = long_),
    .data   = long_,
    .note   = note_,
    .name   = .name,
    .dirs   = .dirs,
    .height = plot_height(8L)
  )
}

# -- 19.3 Seasoned filers: the 2021 spike is new registrants ----------------------------------------------------------------

#' Contracts per year and the registration share, all filers against seasoned filers
#'
#' A seasoned filer is one more than two years past its first contract in the sample, 104_SPAC.do's
#' rule, carried on the contract table as IsSeasoned. If the 2021 rise were accelerated filing by
#' existing firms it would show among seasoned filers; if it is newly registering firms filing the
#' contracts of their two pre-registration years, it will not. Two panels, one tibble.
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @return Tibble: Year, Filers (All / Seasoned), N, NReg, ShareReg.
fin_data_seasoned <- function(.ds_contracts) {
  if (FALSE) .ds_contracts <- lst_ds$Contracts
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S2_Descriptive) |>
    dplyr::select("Year", "IsRegStmt", "IsSeasoned") |>
    dplyr::collect()
  one_ <- function(.r, .lab) {
    .r |>
      dplyr::summarise(
        N        = dplyr::n(),
        NReg     = sum(.data$IsRegStmt == 1L),
        .by = "Year"
      ) |>
      dplyr::mutate(Filers = .lab, ShareReg = .data$NReg / .data$N)
  }
  dplyr::bind_rows(
    one_(rows_, "All filers"),
    one_(rows_[rows_$IsSeasoned == 1L, , drop = FALSE], "Seasoned filers")
  ) |>
    dplyr::relocate("Filers") |>
    dplyr::arrange(.data$Filers, .data$Year)
}

#' Two panels: contracts per year and the registration share, all against seasoned
#' @param .tab_data Tibble from fin_data_seasoned().
#' @return A patchwork.
fin_plot_seasoned <- function(.tab_data) {
  if (FALSE) .tab_data <- fin_data_seasoned(.ds_contracts = lst_ds$Contracts)
  dat_ <- .tab_data |>
    dplyr::mutate(
      Thousand = .data$N / 1000,
      Filers   = factor(.data$Filers, levels = c("All filers", "Seasoned filers"))
    )
  p1_ <- fin_shape_lines(
    .tab   = dat_,
    .x     = "Year",
    .y     = "Thousand",
    .group = "Filers",
    .pct   = FALSE,
    .ylab  = "Contracts (thousands)"
  ) + ggplot2::ggtitle("Panel A. Contracts per year")
  p2_ <- fin_shape_lines(
    .tab   = dat_,
    .x     = "Year",
    .y     = "ShareReg",
    .group = "Filers",
    .pct   = TRUE,
    .ylab  = "Share on registration statements"
  ) + ggplot2::ggtitle("Panel B. Registration statements")
  patchwork::wrap_plots(p1_, p2_, ncol = 1L, guides = "collect") &
    ggplot2::theme(legend.position = "bottom")
}

#' The seasoned-filer figure: built, saved, shown
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_seasoned <- function(.ds_contracts, .dirs, .name = "FilingsSeasoned") {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .dirs         <- .lP$Output
    .name         <- "FilingsSeasoned"
  }
  dat_ <- fin_data_seasoned(.ds_contracts = .ds_contracts)
  g_ <- function(.f, .y, .col) dat_[[.col]][dat_$Filers == .f & dat_$Year == .y]
  tbl_out(
    .tab   = dat_ |>
      dplyr::filter(.data$Year %in% 2018:2023) |>
      dplyr::mutate(ShareReg = 100 * .data$ShareReg) |>
      fin_fmt(1L),
    .title = "Contracts per year and the registration share, 2018-2023, all against seasoned filers"
  )
  note_ <- c(
    "This figure contrasts all filers with seasoned filers -- firms more than two years past their",
    "first contract in the sample -- on the unique-contract sample. Panel A shows the number of",
    "contracts per year, Panel B the share filed with a registration statement (S-1, S-4, F-1, F-4).",
    paste0("Seasoned filers file ", format(g_("Seasoned filers", 2021L, "N"), big.mark = ","),
           " contracts in 2021 against ", format(g_("Seasoned filers", 2020L, "N"), big.mark = ","),
           " in 2020, while all filers rise from ", format(g_("All filers", 2020L, "N"), big.mark = ","),
           " to ", format(g_("All filers", 2021L, "N"), big.mark = ","),
           "; the registration share among all filers reaches ",
           fin_fmt_num(100 * g_("All filers", 2021L, "ShareReg"), 1L), " percent in 2021 against ",
           fin_fmt_num(100 * g_("Seasoned filers", 2021L, "ShareReg"), 1L), " percent among seasoned filers."),
    "The 2021 rise is therefore newly registering firms filing the contracts of their pre-registration",
    "years, not accelerated filing by existing firms. Firms already filing in 2001 are left-censored",
    "and count as seasoned from 2003."
  )
  fin_figure_save(
    .plot   = fin_plot_seasoned(.tab_data = dat_),
    .data   = dat_,
    .note   = note_,
    .name   = .name,
    .dirs   = .dirs,
    .height = plot_height(12L)
  )
}

# -- 19.4 Contract types ----------------------------------------------------------------------------------------------------

#' The category rows a horizontal bar figure draws: super-categories as header rows, sub-categories indented
#'
#' The figure twin of the tables' layout: a super-category with sub-categories is a bold label on an
#' empty row with its sub-categories indented beneath; a single-level category is one bold row that
#' carries its own bar. Reversed so the first category sits at the top of a horizontal chart.
#'
#' @return Tibble: Row (axis label, indented for sub rows), Kind (super / sub / single), Class (NA on
#'   header rows), with Row a factor in drawing order.
fin_category_axis <- function() {
  cat_ <- fin_tab_categories()
  out_ <- list()
  for (l1_ in unique(cat_$Level1)) {
    sub_ <- cat_[cat_$Level1 == l1_, , drop = FALSE]
    if (nrow(sub_) == 1L) {
      out_[[length(out_) + 1L]] <- tibble::tibble(Row = l1_, Kind = "single", Class = sub_$Class)
    } else {
      out_[[length(out_) + 1L]] <- tibble::tibble(Row = l1_, Kind = "super", Class = NA_character_)
      out_[[length(out_) + 1L]] <- tibble::tibble(Row = paste0("    ", sub_$Level2), Kind = "sub", Class = sub_$Class)
    }
  }
  purrr::list_rbind(out_) |>
    dplyr::mutate(Row = factor(.data$Row, levels = rev(.data$Row)))
}

#' Contract types, two panels: count with the share, and contracts per firm-year, super-categories as header rows
#'
#' Each panel carries its letter above it, as the text cites them ("Panel A", "Panel B").
#' @param .tab_data Tibble from fin_data_f03() for one sample.
#' @return A patchwork.
fin_plot_contract_types <- function(.tab_data) {
  if (FALSE) .tab_data <- dat_
  axis_ <- fin_category_axis()
  dat_ <- axis_ |>
    dplyr::left_join(
      dplyr::filter(.tab_data, .data$Class != "Unlabelled"),
      by = dplyr::join_by(Class)
    )
  # HEADER ROWS ARE BOLD. Faces are set per axis label, which ggplot2 accepts for element_text() with
  # a warning it does not need to give; the levels run bottom-up, so the vector is reversed to match.
  faces_ <- rev(dplyr::if_else(axis_$Kind == "sub", "plain", "bold"))
  theme_ <- ggplot2::theme(axis.text.y = ggplot2::element_text(face = faces_, hjust = 0))
  # THE PANEL TITLES start at the left edge of each panel's plot, so A's sits above the category labels.
  title_ <- ggplot2::theme(
    plot.title          = ggplot2::element_text(
      family = .plot_font,
      size   = .plot_base,
      face   = "bold",
      hjust  = 0,
      margin = ggplot2::margin(b = 6)
    ),
    plot.title.position = "plot"
  )
  a_ <- ggplot2::ggplot(dat_, ggplot2::aes(y = .data$Row, x = .data$N / 1000)) +
    ggplot2::geom_col(fill = .plot_ink, width = 0.7, na.rm = TRUE) +
    ggplot2::geom_text(
      data    = dat_[!is.na(dat_$N), , drop = FALSE],
      mapping = ggplot2::aes(label = scales::label_percent(accuracy = 0.1)(.data$Share)),
      hjust   = -0.15,
      size    = .plot_base / ggplot2::.pt * 0.8,
      family  = .plot_font
    ) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    plot_scale_x_count(.expand = c(0, 0.25)) +
    ggplot2::labs(
      title = "Panel A: Number of contracts and share of the sample",
      x     = "Contracts (in 1,000s)",
      y     = NULL
    ) +
    plot_theme(.grid = "x", .legend = "none") +
    suppressWarnings(theme_) +
    title_
  b_ <- ggplot2::ggplot(dat_, ggplot2::aes(y = .data$Row, x = .data$PerFirmYear)) +
    ggplot2::geom_col(fill = .plot_ref, width = 0.7, na.rm = TRUE) +
    ggplot2::geom_text(
      data    = dat_[!is.na(dat_$N), , drop = FALSE],
      mapping = ggplot2::aes(label = formatC(.data$PerFirmYear, format = "f", digits = 2)),
      hjust   = -0.15,
      size    = .plot_base / ggplot2::.pt * 0.8,
      family  = .plot_font
    ) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.25))) +
    ggplot2::labs(
      title = "Panel B: Contracts per firm-year",
      x     = "Contracts per firm-year",
      y     = NULL
    ) +
    plot_theme(.grid = "x", .legend = "none") +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank()) +
    title_
  patchwork::wrap_plots(a_, b_, ncol = 2L, widths = c(1.6, 1))
}

#' The contract-types figure: built from 30's data, checked against the revision, saved, shown
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @param .ref Tibble. 30's reference shares for the figure.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_contract_types <- function(.ds_contracts, .dirs, .name = "ContractTypes", .ref = .fin_reference_f03) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .dirs         <- .lP$Output
    .name         <- "ContractTypes"
    .ref          <- .fin_reference_f03
  }
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S2_Descriptive) |>
    dplyr::select("Class", "CIK", "Year") |>
    dplyr::collect()
  dat_ <- fin_data_f03(
    .tab    = rows_,
    .sample = "S2_Descriptive"
  )
  fin_report_f03(
    .tab_data = dat_,
    .ref      = .ref
  )
  n_     <- nrow(rows_)
  unlab_ <- sum(dat_$N[dat_$Class == "Unlabelled"])
  note_ <- c(
    "This figure shows the distribution of contracts over categories on the unique-contract sample",
    paste0("(N = ", format(n_, big.mark = ","), "). Panel A gives each category's number of contracts, with its share"),
    "of the sample as the label; Panel B the average number of contracts per firm and year in that",
    "category, over the firm-years in which the firm filed at least one contract of it. Categories are",
    "grouped by their super-category as in the categories table.",
    if (unlab_ > 0L) {
      paste0(format(unlab_, big.mark = ","), " contracts without readable text carry no category and are",
             " counted in the shares' denominator only.")
    } else {
      NULL
    }
  )
  fin_figure_save(
    .plot   = fin_plot_contract_types(.tab_data = dat_),
    .data   = dat_,
    .note   = note_,
    .name   = .name,
    .dirs   = .dirs,
    .height = plot_height(12L)
  )
}

# -- 19.5 Contract types over time: alternatives ----------------------------------------------------------------------------
# Four forms of one tibble, each its own sub-header in the runbook until one is chosen. The tibble is
# fin_data_f04(): year x category counts and shares, with the year's amendment share beside
# them. Every form uses the Categories order and super-categories; the twelve on one stack (the
# revision's form) is drawn only as the reference the others are judged against.

#' Year x category shares joined to the Categories table, with the super-category shares beside
#' @param .tab_data Tibble from fin_data_f04() for one sample.
#' @return Tibble: Year, Class, Level1, Level2, Bar, Panel, N, Share, ShareAmend, NSuper, ShareSuper.
fin_types_time_rows <- function(.tab_data) {
  if (FALSE) .tab_data <- dat_
  cat_ <- fin_tab_categories() |>
    dplyr::mutate(
      Bar   = dplyr::if_else(nzchar(.data$Level2), .data$Level2, .data$Level1),
      Panel = dplyr::if_else(nzchar(.data$Level2), paste0(.data$Level1, ": ", .data$Level2), .data$Level1)
    ) |>
    dplyr::select("Class", "Level1", "Level2", "Bar", "Panel")
  .tab_data |>
    dplyr::inner_join(
      cat_,
      by = dplyr::join_by(Class)
    ) |>
    dplyr::mutate(
      NSuper     = sum(.data$N),
      ShareSuper = sum(.data$Share),
      .by = c("Year", "Level1")
    ) |>
    dplyr::mutate(
      Level1 = factor(.data$Level1, levels = unique(cat_$Level1)),
      Class  = factor(.data$Class, levels = cat_$Class),
      Bar    = factor(.data$Bar, levels = cat_$Bar),
      Panel  = factor(.data$Panel, levels = cat_$Panel)
    )
}

#' Alternative A: shares stacked by super-category, the amendment share as a line over them
#' @param .rows Tibble from fin_types_time_rows().
#' @return A ggplot.
fin_plot_types_time_a <- function(.rows) {
  if (FALSE) .rows <- rows_
  sup_ <- .rows |>
    dplyr::distinct(.data$Year, .data$Level1, .data$NSuper, .data$ShareAmend) |>
    dplyr::rename(N = "NSuper")
  line_ <- dplyr::distinct(sup_, .data$Year, .data$ShareAmend)
  fin_shape_share_stack(
    .tab       = sup_,                    # Level1 is a factor in the Categories order, kept as such
    .year      = "Year",
    .n         = "N",
    .fill      = "Level1",
    .key       = NULL,
    .line      = line_,
    .line_y    = "ShareAmend",
    .line_name = "Share amended",
    .count     = FALSE
  ) +
    ggplot2::labs(y = "Share of contracts (in %)") +
    ggplot2::guides(
      fill     = ggplot2::guide_legend(order = 1, nrow = 2L, byrow = TRUE),
      linetype = ggplot2::guide_legend(order = 2)
    )
}

#' Lines over years in small multiples, one y scale for every panel, years that do not collide
#'
#' The shape behind alternatives B and C. Every panel shares the y axis so the eye compares levels
#' across panels, not just shapes; the year breaks are fixed at 2004, 2012 and 2020 so a label never
#' lands on a panel edge. Lines are named at their right end where .labels_end is set, which is the
#' legend of a panel with two or three lines.
#'
#' @param .tab Tibble with Year, Share and the facet column.
#' @param .facet Character. The panel column, a factor in drawing order.
#' @param .group Character or NULL. The line column within a panel; NULL draws one line per panel.
#' @param .colour Character or NULL. A column keyed to at most three colours; NULL draws in ink.
#' @param .labels_end Logical. Name each line at its right end.
#' @param .ncol Integer. Panels per row.
#' @return A ggplot.
fin_lines_panels <- function(.tab, .facet, .group = NULL, .colour = NULL, .labels_end = FALSE, .ncol = 4L) {
  if (FALSE) {
    .tab        <- rows_
    .facet      <- "Panel"
    .group      <- NULL
    .colour     <- NULL
    .labels_end <- FALSE
    .ncol       <- 4L
  }
  dat_ <- .tab |>
    dplyr::mutate(
      PlotG = if (is.null(.group)) "all" else as.character(.data[[.group]]),
      PlotC = if (is.null(.colour)) "all" else as.character(.data[[.colour]])
    )
  cols_ <- if (is.null(.colour)) c(all = .plot_ink) else purrr::set_names(plot_pal_cat(3L), c("1", "2", "3"))
  regimes_ <- .fin_regime_years[.fin_regime_years >= min(dat_$Year) - 0.5 & .fin_regime_years <= max(dat_$Year) + 0.5]
  p_ <- ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$Share, colour = .data$PlotC, group = .data$PlotG)) +
    ggplot2::geom_vline(
      xintercept = unname(regimes_),
      linetype   = "dashed",
      colour     = .plot_ref,
      linewidth  = .plot_line * 2
    ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 0.9)
  if (.labels_end) {
    p_ <- p_ + ggplot2::geom_text(
      data        = dat_ |> dplyr::filter(.data$Year == max(.data$Year)),
      mapping     = ggplot2::aes(x = .data$Year + 0.5, y = .data$Share, label = .data$PlotG, colour = .data$PlotC),
      inherit.aes = FALSE,
      hjust       = 0,
      size        = .plot_base / ggplot2::.pt * 0.7,
      family      = .plot_font,
      show.legend = FALSE
    )
  }
  p_ +
    ggplot2::facet_wrap(ggplot2::vars(.data[[.facet]]), ncol = .ncol) +
    ggplot2::scale_colour_manual(values = cols_, guide = "none") +
    plot_scale_y_pct(.accuracy = 1, .expand = c(0.02, 0.05)) +
    ggplot2::scale_x_continuous(
      breaks = c(2004, 2012, 2020),
      expand = ggplot2::expansion(mult = c(0.03, if (.labels_end) 0.45 else 0.03))
    ) +
    ggplot2::labs(x = NULL, y = "Share of contracts (in %)") +
    plot_theme(.grid = "y", .legend = "none") +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(8, "pt"), strip.text = ggplot2::element_text(lineheight = 0.9))
}

#' Alternative B: one panel per category, the twelve in the Categories order, one y scale
#' @param .rows Tibble from fin_types_time_rows().
#' @return A ggplot.
fin_plot_types_time_b <- function(.rows) {
  if (FALSE) .rows <- rows_
  dat_ <- .rows |>
    dplyr::mutate(Panel2 = factor(gsub(": ", "\n", as.character(.data$Panel), fixed = TRUE),
                                  levels = gsub(": ", "\n", levels(.data$Panel), fixed = TRUE)))
  fin_lines_panels(
    .tab   = dat_,
    .facet = "Panel2",
    .ncol  = 4L
  )
}

#' Alternative C: one panel per super-category, its sub-categories as lines inside, one y scale
#'
#' Colour separates lines within a panel, so it is keyed on the sub-category's position in its
#' super-category -- three colours at most -- and the line is named at its right end.
#'
#' @param .rows Tibble from fin_types_time_rows().
#' @return A ggplot.
fin_plot_types_time_c <- function(.rows) {
  if (FALSE) .rows <- rows_
  dat_ <- .rows |>
    dplyr::mutate(Pos = match(.data$Class, levels(.data$Class))) |>
    dplyr::mutate(
      Rank = as.character(.data$Pos - min(.data$Pos) + 1L),
      .by  = "Level1"
    )
  fin_lines_panels(
    .tab        = dat_,
    .facet      = "Level1",
    .group      = "Bar",
    .colour     = "Rank",
    .labels_end = TRUE,
    .ncol       = 3L
  )
}

#' Alternative D: the twelve on one stack, the revision's form, drawn by 30 as the reference
#' @param .tab_data Tibble from fin_data_f04() for one sample.
#' @return A ggplot.
fin_plot_types_time_d <- function(.tab_data) {
  if (FALSE) .tab_data <- dat_
  fin_plot_f04(.tab_data = .tab_data)
}

#' Contract types over time, one alternative: built from 30's data, saved under its own stem, shown
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .form Character. "a" super-category stack; "b" twelve small multiples; "c" one panel per
#'   super-category with sub-category lines; "d" the revision's twelve-stack.
#' @param .dirs List. .lP$Output.
#' @param .name Character or NULL. The stem; NULL derives TypesTime<Form>.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_types_time <- function(.ds_contracts, .form = c("a", "b", "c", "d"), .dirs, .name = NULL) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .form         <- "a"
    .dirs         <- .lP$Output
    .name         <- NULL
  }
  .form <- match.arg(.form)
  name_ <- if (is.null(.name)) paste0("TypesTime", toupper(.form)) else .name
  rows0_ <- .ds_contracts |>
    dplyr::filter(.data$S2_Descriptive) |>
    dplyr::select("Year", "Class", "IsAmend") |>
    dplyr::collect()
  dat_  <- fin_data_f04(
    .tab    = rows0_,
    .sample = "S2_Descriptive"
  )
  rows_ <- fin_types_time_rows(.tab_data = dat_)
  n_    <- nrow(rows0_)
  am_   <- dplyr::distinct(dat_, .data$Year, .data$ShareAmend)
  base_ <- c(
    paste0("This figure shows each year's contracts by category, as shares of the year's contracts, on the",
           " unique-contract sample (N = ", format(n_, big.mark = ","), "; contracts without readable text carry"),
    "no category and are excluded from the shares). Categories and super-categories are those of the",
    "categories table."
  )
  form_note_ <- switch(.form,
    a = c("Shares are stacked by super-category; the line gives the share of the year's contracts that are",
          paste0("amendments to existing contracts, between ", fin_fmt_num(100 * min(am_$ShareAmend), 0L), " and ",
                 fin_fmt_num(100 * max(am_$ShareAmend), 0L), " percent in every year.")),
    b = "One panel per category, all panels on one scale.",
    c = "One panel per super-category, its sub-categories as lines, all panels on one scale.",
    d = "Shares are stacked by category; the line gives the share of the year's contracts that are amendments."
  )
  plot_ <- switch(.form,
    a = fin_plot_types_time_a(.rows = rows_),
    b = fin_plot_types_time_b(.rows = rows_),
    c = fin_plot_types_time_c(.rows = rows_),
    d = fin_plot_types_time_d(.tab_data = dat_)
  )
  fin_figure_save(
    .plot   = plot_,
    .data   = rows_,
    .note   = c(base_, form_note_),
    .name   = name_,
    .dirs   = .dirs,
    .height = switch(.form, a = plot_height(8L), b = plot_height(18L), c = plot_height(12L), d = plot_height(12L))
  )
}

# -- 19.6 Content characteristics over time ---------------------------------------------------------------------------------

# THE MEASURES THE FIGURE DRAWS, in panel order, with the panel title each takes. The rule-based arm
# of the content table, so a value here and a value there are the same definition.
.fin_content_panels <- c(
  Duration  = "Duration (years)",
  Countries = "Countries mentioned",
  Parties   = "Parties",
  Words     = "Words (in 1,000s)",
  States    = "U.S. states mentioned"
)

#' Mean, SD and quartiles per year of the content measures, on the rule-based arm
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .con A DuckDB connection with Places registered.
#' @param .measures Character. Which of the five, in panel order.
#' @return Tibble: Year, Measure, N, Mean, SD, P25, P75.
fin_data_content_time <- function(.ds_contracts, .con, .measures = names(.fin_content_panels)[1:4]) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .con          <- con
    .measures     <- names(.fin_content_panels)[1:4]
  }
  rows_ <- fin_content_rows(
    .ds_contracts = .ds_contracts,
    .con          = .con,
    .arm          = "rule"
  ) |>
    dplyr::mutate(Words = .data$Words / 1000)
  rows_ |>
    dplyr::select("Year", dplyr::all_of(.measures)) |>
    tidyr::pivot_longer(-"Year", names_to = "Measure", values_to = "Value") |>
    dplyr::filter(!is.na(.data$Value)) |>
    dplyr::summarise(
      N    = dplyr::n(),
      Mean = mean(.data$Value),
      SD   = stats::sd(.data$Value),
      P25  = stats::quantile(.data$Value, 0.25, names = FALSE),
      P75  = stats::quantile(.data$Value, 0.75, names = FALSE),
      .by = c("Year", "Measure")
    ) |>
    dplyr::mutate(Measure = factor(.data$Measure, levels = .measures)) |>
    dplyr::arrange(.data$Measure, .data$Year)
}

#' The content measures over time: the mean as a line, a band around it
#' @param .tab_data Tibble from fin_data_content_time().
#' @param .band Character. "sd" mean +/- one SD, clipped at zero; "iqr" the quartiles; "ci" the 95 percent
#'   confidence interval of the mean; "none" the mean alone.
#' @param .ncol Integer. Panels per row.
#' @return A ggplot.
fin_plot_content_time <- function(.tab_data, .band = c("sd", "iqr", "ci", "none"), .ncol = 2L) {
  if (FALSE) {
    .tab_data <- dat_
    .band     <- "sd"
    .ncol     <- 2L
  }
  .band <- match.arg(.band)
  dat_ <- .tab_data |>
    dplyr::mutate(
      Se    = .data$SD / sqrt(.data$N),
      Lo    = switch(.band,
        sd   = pmax(.data$Mean - .data$SD, 0),
        iqr  = .data$P25,
        ci   = .data$Mean - 1.96 * .data$Se,
        none = .data$Mean
      ),
      Hi    = switch(.band,
        sd   = .data$Mean + .data$SD,
        iqr  = .data$P75,
        ci   = .data$Mean + 1.96 * .data$Se,
        none = .data$Mean
      ),
      Panel = factor(unname(.fin_content_panels[as.character(.data$Measure)]),
                     levels = unname(.fin_content_panels[levels(.data$Measure)]))
    )
  band_name_ <- switch(.band, sd = "Mean +/- 1 SD", iqr = "Interquartile range", ci = "95% confidence interval",
                       none = NULL)
  regimes_ <- .fin_regime_years[.fin_regime_years >= min(dat_$Year) - 0.5 & .fin_regime_years <= max(dat_$Year) + 0.5]
  p_ <- ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year))
  if (.band != "none") {
    p_ <- p_ + ggplot2::geom_ribbon(
      mapping = ggplot2::aes(ymin = .data$Lo, ymax = .data$Hi, fill = band_name_),
      alpha   = 0.25,
      colour  = NA
    )
  }
  p_ <- p_ +
    ggplot2::geom_vline(
      xintercept = unname(regimes_),
      linetype   = "dashed",
      colour     = .plot_ref,
      linewidth  = .plot_line * 2
    ) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = .data$Mean, colour = "Mean"), linewidth = 0.7) +
    ggplot2::geom_point(mapping = ggplot2::aes(y = .data$Mean, colour = "Mean"), size = 1.0) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Panel), ncol = .ncol, scales = "free_y") +
    ggplot2::scale_colour_manual(values = c(Mean = .plot_ink), name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::scale_x_continuous(breaks = c(2004, 2012, 2020), expand = ggplot2::expansion(mult = 0.03)) +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
  if (.band != "none") {
    p_ <- p_ +
      ggplot2::scale_fill_manual(values = purrr::set_names(.plot_ink, band_name_), name = NULL) +
      ggplot2::guides(colour = ggplot2::guide_legend(order = 1), fill = ggplot2::guide_legend(order = 2))
  }
  p_
}

#' The content measures as an index: each mean relative to its first year, one panel, one line each
#'
#' Four measures on four scales cannot share a panel; four indices can, and the index is what the
#' text says -- words up, duration down -- on one axis.
#'
#' @param .tab_data Tibble from fin_data_content_time().
#' @param .base Integer. The year set to 100.
#' @return A ggplot.
fin_plot_content_index <- function(.tab_data, .base = 2001L) {
  if (FALSE) {
    .tab_data <- dat_
    .base     <- 2001L
  }
  dat_ <- .tab_data |>
    dplyr::mutate(
      Index = 100 * .data$Mean / .data$Mean[.data$Year == .base],
      .by   = "Measure"
    ) |>
    dplyr::mutate(Panel = factor(unname(.fin_content_panels[as.character(.data$Measure)]),
                                 levels = unname(.fin_content_panels[levels(.data$Measure)])))
  cols_ <- purrr::set_names(plot_pal_cat(nlevels(dat_$Panel)), levels(dat_$Panel))
  regimes_ <- .fin_regime_years[.fin_regime_years >= min(dat_$Year) - 0.5 & .fin_regime_years <= max(dat_$Year) + 0.5]
  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$Index, colour = .data$Panel, group = .data$Panel)) +
    ggplot2::geom_hline(yintercept = 100, colour = .plot_ref, linewidth = .plot_line * 2) +
    ggplot2::geom_vline(
      xintercept = unname(regimes_),
      linetype   = "dashed",
      colour     = .plot_ref,
      linewidth  = .plot_line * 2
    ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.0) +
    ggplot2::scale_colour_manual(values = cols_, name = NULL) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.05)) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(4), expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::labs(x = NULL, y = paste0("Mean per year, ", .base, " = 100")) +
    plot_theme(.grid = "y", .legend = "bottom")
}

#' The content-over-time figure in one form: built on the rule-based arm, saved under its own stem, shown
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .con A DuckDB connection with Places registered.
#' @param .form Character. "sd", "iqr", "ci", "none" for the mean with that band; "index" for the one-panel index.
#' @param .measures Character. Which of the five measures, in panel order.
#' @param .dirs List. .lP$Output.
#' @param .name Character or NULL. The stem; NULL derives ContentTime<Form>.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_content_time <- function(.ds_contracts, .con, .form = c("sd", "iqr", "ci", "none", "index"),
                                    .measures = names(.fin_content_panels)[1:4], .dirs, .name = NULL) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .con          <- con
    .form         <- "sd"
    .measures     <- names(.fin_content_panels)[1:4]
    .dirs         <- .lP$Output
    .name         <- NULL
  }
  .form <- match.arg(.form)
  name_ <- if (is.null(.name)) paste0("ContentTime", switch(.form, sd = "SD", iqr = "IQR", ci = "CI", none = "Mean",
                                                             index = "Index")) else .name
  dat_ <- fin_data_content_time(
    .ds_contracts = .ds_contracts,
    .con          = .con,
    .measures     = .measures
  )
  if (.form == "sd") {
    tbl_out(
      .tab   = dat_ |>
        dplyr::filter(.data$Year %in% c(2001L, 2010L, 2019L, 2024L)) |>
        dplyr::mutate(Measure = as.character(.data$Measure)) |>
        fin_fmt(2L),
      .title = "Content measures per year: four years, the panel behind every form of the figure"
    )
  }
  n_ <- .ds_contracts |>
    dplyr::filter(.data$S2_Descriptive) |>
    dplyr::count() |>
    dplyr::collect() |>
    dplyr::pull("n")
  defs_ <- c(
    "Duration is the rule-based contract duration in years, defined where an end date could be",
    "established; countries are the distinct countries attached to a recital party outside a governing-law",
    "clause; parties are the recital parties -- registrant, co-filers and counterparties; words exclude the",
    "filing header and are in thousands. The measures are those of the content table. The dashed lines",
    "mark the 2004 amendments to Form 8-K and the 2019 FAST Act."
  )
  head_ <- paste0("This figure shows the development of the contract content measures over the sample period on",
                  " the unique-contract sample (N = ", format(n_, big.mark = ","), "): ")
  clip_ <- .form == "sd" && any(dat_$Mean - dat_$SD < 0)
  note_ <- switch(.form,
    sd    = c(paste0(head_, "the mean per year as a line and, shaded, the mean plus and minus one standard deviation",
                     " of that year's contracts."),
              if (clip_) "Where the mean less one standard deviation is negative the band is cut at zero." else NULL,
              defs_),
    iqr   = c(paste0(head_, "the mean per year as a line and, shaded, the interquartile range of that year's contracts."),
              defs_),
    ci    = c(paste0(head_, "the mean per year as a line and, shaded, its 95 percent confidence interval."), defs_),
    none  = c(paste0(head_, "the mean per year."), defs_),
    index = c(paste0(head_, "each measure's mean per year relative to its mean in 2001, set to 100, so that the four",
                     " measures share one axis."), defs_)
  )
  plot_ <- if (.form == "index") {
    fin_plot_content_index(.tab_data = dat_, .base = min(dat_$Year))
  } else {
    fin_plot_content_time(.tab_data = dat_, .band = .form, .ncol = 2L)
  }
  fin_figure_save(
    .plot   = plot_,
    .data   = dplyr::mutate(dat_, Form = .form),
    .note   = note_,
    .name   = name_,
    .dirs   = .dirs,
    .height = if (.form == "index") plot_height(8L) else plot_height(12L)
  )
}

# -- 19.7 Redactions over time ----------------------------------------------------------------------------------------------

#' Redactions over time in the manuscript's form: two identifications, the FAST Act marked
#'
#' The CTO identification is the share of contracts covered by a granted confidential treatment
#' order, drawn through 2019, the year orders end with the FAST Act; the RegEx identification is the
#' paper's measure, a granted order or at least one redaction marker in the text, drawn throughout.
#' The marker-only line 30 adds is off by default and on for an appendix version.
#'
#' @param .tab_data Tibble from fin_data_f10() for one sample.
#' @param .markers Logical. Draw the marker-only series as a third line.
#' @return A ggplot.
fin_plot_redactions_time <- function(.tab_data, .markers = FALSE) {
  if (FALSE) {
    .tab_data <- dat_
    .markers  <- FALSE
  }
  labs_ <- c(CtoOnly = "CTO identification", Union = "RegEx identification", TextOnly = "Marker identification")
  keep_ <- if (.markers) names(labs_) else c("CtoOnly", "Union")
  blues_ <- plot_pal_seq(3L)
  cols_  <- c(`CTO identification` = blues_[[1L]], `RegEx identification` = blues_[[3L]],
              `Marker identification` = blues_[[2L]])
  dat_ <- .tab_data |>
    dplyr::filter(.data$Series %in% keep_) |>
    dplyr::mutate(Series = factor(unname(labs_[.data$Series]), levels = unname(labs_[keep_])))
  # THE LINE SITS ON THE 2019 POINT. The axis counts whole years, and the two identifications meet in 2019, the year
  # orders end; the April date would put the line between two points (decided 17 September).
  reg_ <- tibble::tibble(
    X     = 2019,
    Label = unname(.fin_regime_labels["Fast2019"])
  )
  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$Share, colour = .data$Series, group = .data$Series)) +
    ggplot2::geom_vline(
      xintercept = reg_$X,
      linetype   = "dashed",
      colour     = "black",
      linewidth  = .plot_line * 2
    ) +
    ggplot2::geom_text(
      data        = reg_,
      mapping     = ggplot2::aes(x = .data$X, y = Inf, label = .data$Label),
      inherit.aes = FALSE,
      hjust       = 1.05,
      vjust       = 1.5,
      size        = .plot_base / ggplot2::.pt * 0.8,
      family      = .plot_font
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = cols_[levels(dat_$Series)], name = NULL) +
    plot_scale_y_pct(
      .accuracy = 1,
      .expand   = c(0.02, 0.08),
      .breaks   = scales::breaks_width(0.02)   # whole-percent ticks: a 2.5-percent step printed as 2 and 8
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2), expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::labs(x = NULL, y = "Share of contracts with redactions (in %)") +
    plot_theme(.grid = "y", .legend = "bottom")
}

#' The redactions-over-time figure: built from 30's data, checked against the revision, saved, shown
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .markers Logical. Draw the marker-only series too.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @param .ref Tibble. 30's reference readings of the revision's figure.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_redactions_time <- function(.ds_contracts, .markers = FALSE, .dirs, .name = "RedactionsTime",
                                       .ref = .fin_reference_f10) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .markers      <- FALSE
    .dirs         <- .lP$Output
    .name         <- "RedactionsTime"
    .ref          <- .fin_reference_f10
  }
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S6_Redaction) |>
    dplyr::select("Year", "HasCto", "HasMarkers", "Redacted", "RedactedText") |>
    dplyr::collect()
  dat_ <- fin_data_f10(
    .tab    = rows_,
    .sample = "S6_Redaction"
  )
  fin_report_f10(
    .tab_data = dat_,
    .ref      = .ref
  )
  # THE CTO SERIES RUNS INTO 2019, AND ITS LAST POINT IS THE SWITCH. The revision's light line is
  # 103's `redacted`: the order through 2018, the markers from 2019, so its 2019 value is the paper's
  # measure at the year the definition changes, and it lands on the RegEx line. Orders alone cover
  # about one percent of 2019 -- three months of orders over a year of contracts -- which is not what
  # the figure means to show. The 2019 point is therefore the paper's Redacted, and the note says so.
  cto19_ <- rows_ |>
    dplyr::filter(.data$Year == 2019L) |>
    dplyr::summarise(N = dplyr::n(), Share = mean(.data$Redacted)) |>
    dplyr::mutate(Year = 2019L, Series = "CtoOnly", Sample = "S6_Redaction")
  dat_ <- dplyr::bind_rows(dat_, cto19_) |>
    dplyr::arrange(.data$Series, .data$Year)
  pre_  <- mean(rows_$HasCto[rows_$Year <= 2018L])
  post_ <- mean(rows_$RedactedText[rows_$Year > 2018L])
  note_ <- c(
    "This figure shows the share of contracts with redactions per year on the unique-contract sample",
    paste0("from 2008, when confidential treatment orders become observable (N = ", format(nrow(rows_), big.mark = ","),
           ")."),
    "The CTO identification counts a contract as redacted where the SEC granted a confidential treatment",
    "order for it; it ends with 2019, the year the FAST Act replaced the order with self-executing",
    "redaction, and its 2019 point is the paper's redaction measure at that switch.",
    "The RegEx identification counts a contract as redacted where it is covered by a granted",
    "order or its text carries at least one redaction marker -- a bracketed placeholder naming",
    "confidential treatment or holding a symbol -- which is the measure the paper uses throughout.",
    if (.markers) "The marker identification counts the markers alone." else NULL,
    paste0("On average ", fin_fmt_num(100 * pre_, 1L), " percent of contracts filed 2008-2018 carry an order and ",
           fin_fmt_num(100 * post_, 1L), " percent of those filed after the FAST Act are redacted under the paper's",
           " measure."),
    "The dashed line marks the FAST Act."
  )
  fin_figure_save(
    .plot   = fin_plot_redactions_time(.tab_data = dat_, .markers = .markers),
    .data   = dat_,
    .note   = note_,
    .name   = .name,
    .dirs   = .dirs,
    .height = plot_height(5L)
  )
}

# -- 19.8 Maps --------------------------------------------------------------------------------------------------------------
# Places attached to a party, on the unique-contract sample: where the counterparties are, and where
# the registrant's own footprint is. Two forms. The ranked map fills every area and numbers the ten
# largest, with the names and counts in a table beside it rather than on the area. The flow map
# draws a curve from the registrant's place to the counterparty's, which is the contracting
# relation the referee asked about. Both read the prepared Places through DuckDB.

# THE TWO GAZETTEER ARTEFACTS, suppressed at the map until 04B2 is fixed: "Columbia" resolves to
# Colombia and "Island" to Iceland, at 6.8 and 2.5 percent of contracts. Stated in every note.
.fin_map_suppress <- c("COL", "ISL")

#' Country names by ISO alpha-3, from the table the world map is drawn from
#' @return Tibble: Area (ISO alpha-3), Name.
fin_country_names <- function() {
  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(.data$iso_a3)) |>
    dplyr::transmute(Area = .data$iso_a3, Name = .data$name) |>
    dplyr::distinct(.data$Area, .keep_all = TRUE) |>
    tibble::as_tibble()
}

#' Contracts per state or country for places attached to the roles given, on the unique-contract sample
#'
#' @param .con A DuckDB connection with Contracts and Places registered.
#' @param .panel Character. "State" (U.S. states) or "Country".
#' @param .roles Character. Party roles whose attached places count.
#' @param .suppress Character. ISO alpha-3 codes dropped from the country panel.
#' @return Tibble: Area, Name, nContracts, Share (of the unique-contract sample), Rank.
fin_places_area <- function(.con, .panel = c("State", "Country"), .roles = "counterparty", .suppress = .fin_map_suppress) {
  if (FALSE) {
    .con      <- con
    .panel    <- "State"
    .roles    <- "counterparty"
    .suppress <- .fin_map_suppress
  }
  .panel <- match.arg(.panel)
  in_ <- paste0("'", .roles, "'", collapse = ", ")
  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM Contracts WHERE S2_Descriptive")$N
  sql_ <- if (.panel == "State") {
    paste0(
      "SELECT p.GeoState AS Area, p.GeoState AS Name, COUNT(DISTINCT p.DocID) AS nContracts ",
      "FROM Places p JOIN Contracts c ON c.DocID = p.DocID ",
      "WHERE c.S2_Descriptive AND p.InLawClause = 0 AND p.PartyRole IN (", in_, ") ",
      "  AND p.GeoCountryIso = 'USA' AND p.GeoState IS NOT NULL ",
      "GROUP BY p.GeoState"
    )
  } else {
    paste0(
      "SELECT p.GeoCountryIso AS Area, COUNT(DISTINCT p.DocID) AS nContracts ",
      "FROM Places p JOIN Contracts c ON c.DocID = p.DocID ",
      "WHERE c.S2_Descriptive AND p.InLawClause = 0 AND p.PartyRole IN (", in_, ") ",
      "  AND p.GeoCountryIso IS NOT NULL ",
      "GROUP BY p.GeoCountryIso"
    )
  }
  out_ <- DBI::dbGetQuery(.con, sql_) |>
    tibble::as_tibble()
  if (.panel == "Country") {
    out_ <- out_ |>
      dplyr::left_join(
        fin_country_names(),
        by = dplyr::join_by(Area)
      ) |>
      dplyr::mutate(Name = dplyr::coalesce(.data$Name, .data$Area))
  }
  out_ |>
    dplyr::filter(!(.panel == "Country" & .data$Area %in% .suppress)) |>
    dplyr::mutate(
      nContracts = as.integer(.data$nContracts),
      Name       = if (.panel == "State") stringi::stri_trans_totitle(.data$Name) else .data$Name,
      Share      = .data$nContracts / n_
    ) |>
    dplyr::arrange(dplyr::desc(.data$nContracts), .data$Area) |>  # ties by area code: ranks stay put
    dplyr::mutate(Rank = dplyr::row_number())
}

#' Contracts by registrant place and counterparty place, on the unique-contract sample
#'
#' One row per contract, registrant place and counterparty place, counted as distinct contracts per
#' pair. The state panel keeps U.S. states on both ends; the country panel keeps a U.S. registrant
#' and any counterparty country, which is the flow from the filer to the rest of the world.
#'
#' @param .con A DuckDB connection with Contracts and Places registered.
#' @param .panel Character. "State" or "Country".
#' @param .suppress Character. ISO alpha-3 codes dropped from the country panel.
#' @return Tibble: From, To, nContracts, with a Within attribute giving the same-place count on the
#'   state panel.
fin_places_flows <- function(.con, .panel = c("State", "Country"), .suppress = .fin_map_suppress) {
  if (FALSE) {
    .con      <- con
    .panel    <- "State"
    .suppress <- .fin_map_suppress
  }
  .panel <- match.arg(.panel)
  col_ <- if (.panel == "State") "GeoState" else "GeoCountryIso"
  where_ <- if (.panel == "State") {
    "r.GeoCountryIso = 'USA' AND r.GeoState IS NOT NULL AND k.GeoCountryIso = 'USA' AND k.GeoState IS NOT NULL"
  } else {
    "r.GeoCountryIso = 'USA' AND k.GeoCountryIso IS NOT NULL AND k.GeoCountryIso <> 'USA'"
  }
  sql_ <- paste0(
    "WITH s AS (SELECT DocID FROM Contracts WHERE S2_Descriptive), ",
    "     r AS (SELECT p.DocID, p.GeoState, p.GeoCountryIso FROM Places p JOIN s ON s.DocID = p.DocID ",
    "           WHERE p.InLawClause = 0 AND p.PartyRole = 'registrant'), ",
    "     k AS (SELECT p.DocID, p.GeoState, p.GeoCountryIso FROM Places p JOIN s ON s.DocID = p.DocID ",
    "           WHERE p.InLawClause = 0 AND p.PartyRole = 'counterparty') ",
    "SELECT r.", col_, " AS \"From\", k.", col_, " AS \"To\", COUNT(DISTINCT r.DocID) AS nContracts ",
    "FROM r JOIN k ON k.DocID = r.DocID ",
    "WHERE ", where_, " ",
    "GROUP BY r.", col_, ", k.", col_
  )
  out_ <- DBI::dbGetQuery(.con, sql_) |>
    tibble::as_tibble() |>
    dplyr::mutate(nContracts = as.integer(.data$nContracts)) |>
    dplyr::filter(!(.panel == "Country" & .data$To %in% .suppress)) |>
    dplyr::arrange(dplyr::desc(.data$nContracts), .data$From, .data$To)  # ties by the two places
  within_ <- sum(out_$nContracts[out_$From == out_$To])
  out_ <- dplyr::filter(out_, .data$From != .data$To)
  attr(out_, "Within") <- within_
  out_
}

#' A ranked map: every area filled, the ten largest numbered; the names and counts in the note, an inset or a table
#'
#' @param .tab Tibble from fin_places_area().
#' @param .panel Character. "State" or "Country".
#' @param .top Integer. How many areas are numbered and listed.
#' @param .counts Character. Where the ten names and counts go: "note" leaves the map alone at full
#'   width (the note carries them); "inset" sets the list inside the map, bottom left; "table" puts it
#'   beside the map.
#' @return A ggplot or a patchwork.
fin_plot_map_ranked <- function(.tab, .panel = c("State", "Country"), .top = 10L, .counts = c("inset", "note", "table")) {
  if (FALSE) {
    .tab    <- fin_places_area(.con = con, .panel = "State")
    .panel  <- "State"
    .top    <- 10L
    .counts <- "inset"
  }
  .panel  <- match.arg(.panel)
  .counts <- match.arg(.counts)
  d_ <- dplyr::select(.tab, "Area", N = "nContracts")
  map_ <- if (.panel == "State") {
    plot_map_usa(.tab = d_, .val = "N", .log = TRUE, .name = "Contracts", .label_n = 0L)
  } else {
    plot_map_world(.tab = d_, .val = "N", .log = TRUE, .name = "Contracts", .label_n = 0L)
  }
  shape_ <- if (.panel == "State") plot_shape_usa() else plot_shape_world()
  top_ <- .tab |>
    dplyr::slice_head(n = .top) |>
    dplyr::mutate(Key = if (.panel == "State") stringi::stri_trans_toupper(.data$Area) else .data$Area)
  pts_ <- shape_ |>
    dplyr::inner_join(dplyr::select(top_, Area = "Key", "Rank"), by = dplyr::join_by(Area))
  pts_ <- suppressWarnings(sf::st_point_on_surface(pts_))
  # THE LEGEND LIES UNDER THE MAP. The commons stand it beside the map for the label collisions a
  # short horizontal bar produces; with the K and M labels and a bar the width of the map's middle
  # third, the four labels have room, and the map takes the full width of its panel.
  # THE RANK SITS IN A CIRCLE: a white disc with a thin ink rim under an ink number reads on the palest
  # and the darkest fill alike, and on a state too small to hold a number.
  map_ <- map_ +
    ggplot2::geom_sf(
      data        = pts_,
      inherit.aes = FALSE,
      shape       = 21,
      fill        = "#FFFFFF",
      colour      = .plot_ink,
      size        = 4.2,
      stroke      = 0.5
    ) +
    ggplot2::geom_sf_text(
      data        = pts_,
      mapping     = ggplot2::aes(label = .data$Rank),
      inherit.aes = FALSE,
      size        = .plot_base / ggplot2::.pt * 0.65,
      fontface    = "bold",
      colour      = .plot_ink,
      family      = .plot_font
    ) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(
      title.position = "left",
      title.vjust    = 0.9,
      barwidth       = ggplot2::unit(5, "cm"),
      barheight      = ggplot2::unit(0.3, "cm")
    )) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title    = ggplot2::element_text(family = .plot_font, size = .plot_base - 1)
    )
  if (.counts == "note") return(map_)
  # THE LIST OF TEN, drawn as text so that map and list share one figure file: beside the map, or
  # set into its empty corner. The inset is a third of the figure wide, so it takes smaller type and
  # short names; the side table has the room for the full ones.
  short_ <- function(.x) {
    x_ <- dplyr::case_match(.x, "United States of America" ~ "United States", .default = .x)
    dplyr::if_else(nchar(x_) > 18L, paste0(stringi::stri_sub(x_, 1L, 16L), "..."), x_)
  }
  size_ <- if (.counts == "inset") 0.62 else 0.8
  rows_ <- top_ |>
    dplyr::mutate(
      Y     = -.data$Rank,
      Label = paste0(.data$Rank, ". ", if (.counts == "inset") short_(.data$Name) else .data$Name),
      Value = paste0(format(.data$nContracts, big.mark = ","), " (", formatC(100 * .data$Share, format = "f",
                                                                            digits = 1), "%)")
    )
  head_ <- if (.counts == "inset") "Contracts (share of sample)" else "Contracts naming the place (share of sample)"
  tab_ <- ggplot2::ggplot(rows_) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(x = 0, y = .data$Y, label = .data$Label),
      hjust   = 0,
      size    = .plot_base / ggplot2::.pt * size_,
      family  = .plot_font
    ) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(x = 1, y = .data$Y, label = .data$Value),
      hjust   = 1,
      size    = .plot_base / ggplot2::.pt * size_,
      family  = .plot_font
    ) +
    ggplot2::annotate("text", x = 0, y = 0, label = head_, hjust = 0, size = .plot_base / ggplot2::.pt * size_,
                      fontface = "bold", family = .plot_font) +
    ggplot2::scale_x_continuous(limits = c(-0.02, 1.02), expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_continuous(limits = c(-.top - 0.6, 0.6), expand = ggplot2::expansion(0)) +
    ggplot2::theme_void()
  if (.counts == "table") {
    return(patchwork::wrap_plots(map_, tab_, ncol = 2L, widths = c(2.6, 1)))
  }
  inset_ <- tab_ +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#FFFFFFD9", colour = "#BFBFBF", linewidth = 0.3),
      plot.margin     = ggplot2::margin(4, 6, 4, 6)
    )
  box_ <- if (.panel == "State") c(-0.06, 0.00, 0.30, 0.40) else c(-0.04, 0.04, 0.31, 0.52)
  map_ + patchwork::inset_element(
    p      = inset_,
    left   = box_[[1L]],
    bottom = box_[[2L]],
    right  = box_[[3L]],
    top    = box_[[4L]]
  )
}

#' A flow map: curves from the registrant's place to the counterparty's, the largest flows drawn
#'
#' Coordinates are the shapes' points on surface, projected to the map's own coordinate system, so
#' the curves are drawn in the space the map is drawn in. Width scales with contracts; the largest
#' destinations are named.
#'
#' @param .flows Tibble from fin_places_flows().
#' @param .panel Character. "State" or "Country".
#' @param .top Integer. How many flows are drawn.
#' @param .label Integer. How many destinations are named.
#' @return A ggplot.
fin_plot_map_flows <- function(.flows, .panel = c("State", "Country"), .top = 25L, .label = 10L) {
  if (FALSE) {
    .flows <- fin_places_flows(.con = con, .panel = "State")
    .panel <- "State"
    .top   <- 25L
    .label <- 10L
  }
  .panel <- match.arg(.panel)
  shape_ <- if (.panel == "State") plot_shape_usa() else plot_shape_world()
  crs_   <- if (.panel == "State") 5070L else 4326L
  pts_ <- suppressWarnings(sf::st_point_on_surface(shape_)) |>
    sf::st_transform(crs_)
  xy_ <- sf::st_coordinates(pts_)
  cen_ <- tibble::tibble(Area = pts_$Area, X = xy_[, 1L], Y = xy_[, 2L])
  key_ <- function(.x) if (.panel == "State") stringi::stri_trans_toupper(.x) else .x
  top_ <- .flows |>
    dplyr::slice_head(n = .top) |>
    dplyr::mutate(FromKey = key_(.data$From), ToKey = key_(.data$To)) |>
    dplyr::inner_join(dplyr::rename(cen_, FromKey = "Area", X1 = "X", Y1 = "Y"), by = dplyr::join_by(FromKey)) |>
    dplyr::inner_join(dplyr::rename(cen_, ToKey = "Area", X2 = "X", Y2 = "Y"), by = dplyr::join_by(ToKey))
  lab_ <- top_ |>
    dplyr::summarise(N = sum(.data$nContracts), X = dplyr::first(.data$X2), Y = dplyr::first(.data$Y2), .by = "To") |>
    dplyr::slice_max(.data$N, n = .label, with_ties = FALSE) |>
    dplyr::mutate(Label = if (.panel == "State") {
      stringi::stri_trans_totitle(.data$To)
    } else {
      dplyr::coalesce(fin_country_names()$Name[match(.data$To, fin_country_names()$Area)], .data$To)
    })
  p_ <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data      = shape_,
      fill      = "#F5F5F5",
      colour    = "#BFBFBF",              # borders visible on the pale fill, at world scale too
      linewidth = 0.15
    ) +
    ggplot2::geom_curve(
      data      = top_,
      mapping   = ggplot2::aes(x = .data$X1, y = .data$Y1, xend = .data$X2, yend = .data$Y2, linewidth = .data$nContracts),
      curvature = 0.25,
      colour    = .plot_ink,
      alpha     = 0.55,
      lineend   = "round"
    ) +
    ggplot2::geom_point(
      data    = dplyr::distinct(top_, .data$X2, .data$Y2),
      mapping = ggplot2::aes(x = .data$X2, y = .data$Y2),
      colour  = .plot_ink,
      size    = 1.2
    ) +
    ggplot2::geom_text(
      data    = lab_,
      mapping = ggplot2::aes(x = .data$X, y = .data$Y, label = .data$Label),
      size    = .plot_base / ggplot2::.pt * 0.7,
      family  = .plot_font,
      vjust   = -0.8
    ) +
    ggplot2::scale_linewidth_continuous(range = c(0.3, 3), labels = scales::label_comma(), name = "Contracts") +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "none", .legend = "right") +
    ggplot2::theme(
      axis.text        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      axis.line        = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = NA)
    )
  if (.panel == "State") {
    p_ + ggplot2::coord_sf(crs = crs_, datum = NA, expand = FALSE)
  } else {
    p_ + ggplot2::coord_sf(crs = crs_, ylim = c(-58, 84), datum = NA, expand = FALSE)
  }
}

#' A ranked map figure: built from Places, saved, shown
#' @param .con A DuckDB connection with Contracts and Places registered.
#' @param .panel Character. "State" or "Country".
#' @param .roles Character. Party roles whose attached places count.
#' @param .top Integer. Areas numbered and listed.
#' @param .counts Character. "note", "inset" or "table": where the ten names and counts appear.
#' @param .suppress Character. Country codes dropped.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_map <- function(.con, .panel = c("State", "Country"), .roles = "counterparty", .top = 10L,
                           .counts = c("inset", "note", "table"), .suppress = .fin_map_suppress, .dirs, .name) {
  if (FALSE) {
    .con      <- con
    .panel    <- "State"
    .roles    <- "counterparty"
    .top      <- 10L
    .counts   <- "inset"
    .suppress <- .fin_map_suppress
    .dirs     <- .lP$Output
    .name     <- "MapStatesCounterparty"
  }
  .panel  <- match.arg(.panel)
  .counts <- match.arg(.counts)
  tab_ <- fin_places_area(
    .con      = .con,
    .panel    = .panel,
    .roles    = .roles,
    .suppress = .suppress
  )
  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM Contracts WHERE S2_Descriptive")$N
  roles_txt_ <- if (identical(.roles, "counterparty")) "a counterparty" else
    paste0("a party of the roles ", paste(.roles, collapse = ", "))
  note_ <- c(
    paste0("This figure shows, for each ", if (.panel == "State") "U.S. state" else "country", ", the number of"),
    paste0("contracts in which the ", if (.panel == "State") "state" else "country", " is attached to ", roles_txt_,
           " outside a governing-law clause, on the unique-contract sample (N = ", format(n_, big.mark = ","), ")."),
    "A contract naming a place several times is counted once; the fill is on a logarithmic scale.",
    paste0("The ", .top, " largest are numbered on the map: ",
           paste0(tab_$Rank[seq_len(.top)], " ", tab_$Name[seq_len(.top)], " (",
                  format(tab_$nContracts[seq_len(.top)], big.mark = ","), ", ",
                  formatC(100 * tab_$Share[seq_len(.top)], format = "f", digits = 1), "%)", collapse = "; "),
           "."),
    if (.panel == "Country" && length(.suppress) > 0L) {
      paste0(paste(.suppress, collapse = " and "), " are omitted: the gazetteer resolves the words Columbia and",
             " Island to them.")
    } else {
      NULL
    }
  )
  fin_figure_save(
    .plot   = fin_plot_map_ranked(.tab = tab_, .panel = .panel, .top = .top, .counts = .counts),
    .data   = dplyr::mutate(tab_, Panel = .panel, Roles = paste(.roles, collapse = "+")),
    .note   = note_,
    .name   = .name,
    .dirs   = .dirs,
    .height = plot_height(8L),
    .width  = if (.counts == "table") 10 else 8
  )
}

#' A flow map figure: built from Places, saved, shown
#' @param .con A DuckDB connection with Contracts and Places registered.
#' @param .panel Character. "State" or "Country".
#' @param .top Integer. Flows drawn.
#' @param .suppress Character. Country codes dropped.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_flows <- function(.con, .panel = c("State", "Country"), .top = 25L, .suppress = .fin_map_suppress, .dirs,
                             .name) {
  if (FALSE) {
    .con      <- con
    .panel    <- "State"
    .top      <- 25L
    .suppress <- .fin_map_suppress
    .dirs     <- .lP$Output
    .name     <- "FlowsStates"
  }
  .panel <- match.arg(.panel)
  fl_ <- fin_places_flows(
    .con      = .con,
    .panel    = .panel,
    .suppress = .suppress
  )
  within_ <- attr(fl_, "Within")
  tbl_out(
    .tab   = dplyr::slice_head(fl_, n = 15L),
    .title = paste0("The fifteen largest ", tolower(.panel), " flows, registrant to counterparty")
  )
  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM Contracts WHERE S2_Descriptive")$N
  note_ <- c(
    paste0("This figure draws, on the unique-contract sample (N = ", format(n_, big.mark = ","), "), the ",
           .top, " largest flows from the ", if (.panel == "State") "state" else "country",
           " attached to the registrant to the ", if (.panel == "State") "state" else "country",
           " attached to a counterparty, both outside a governing-law clause; the width of a curve is the number",
           " of contracts, and the largest destinations are named."),
    if (.panel == "State") {
      paste0("Contracts whose registrant and counterparty are attached to the same state (",
             format(within_, big.mark = ","), ") are not drawn.")
    } else {
      paste0("Only flows from a U.S. registrant to a foreign counterparty are drawn; ",
             paste(.suppress, collapse = " and "), " are omitted because the gazetteer resolves the words",
             " Columbia and Island to them.")
    }
  )
  fin_figure_save(
    .plot   = fin_plot_map_flows(.flows = fl_, .panel = .panel, .top = .top),
    .data   = dplyr::mutate(fl_, Panel = .panel),
    .note   = note_,
    .name   = .name,
    .dirs   = .dirs,
    .height = plot_height(8L)
  )
}

# -- 19.9 Geographic diversity ----------------------------------------------------------------------------------------------

#' The diversity binscatter drawn as the manuscript prints it
#' @param .bins Tibble from fin_bins_f07()$Bins.
#' @param .fit Tibble from fin_bins_f07()$Fit.
#' @param .ylab Character. The y label.
#' @return A ggplot.
fin_plot_diversity <- function(.bins, .fit, .ylab = "Countries mentioned per contract") {
  if (FALSE) {
    .bins <- b_$Bins
    .fit  <- b_$Fit
    .ylab <- "Countries mentioned per contract"
  }
  ggplot2::ggplot(.bins, ggplot2::aes(x = .data$Diversity, y = .data$Countries)) +
    ggplot2::geom_abline(
      intercept = .fit$Intercept,
      slope     = .fit$Slope,
      colour    = .plot_ink,
      linewidth = 0.6
    ) +
    ggplot2::geom_point(colour = .plot_ref, size = 1.8) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::label_number(accuracy = 0.1)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
    ggplot2::labs(x = "Segment geographical diversity", y = .ylab) +
    plot_theme(.grid = "y", .legend = "none")
}

#' The geographic-diversity figure: her grain and her trim, from 30's functions, saved, shown
#'
#' HER CODE, STEP FOR STEP. One contract per firm and fiscal year (the first by DocID; 104 takes the
#' first in Stata's order, which is not reproducible), merged to 101's concentration file; the top
#' percentile of diversity and of the country count dropped, not clipped; 85 equal-count bins of
#' diversity; the line from an OLS on the underlying rows. The manuscript's note says "trim at the
#' 99th percentile and 50 bins", which is not what the code does; the note written here says what is
#' done. .grain = "mean" and the p99 clip are one call away and stay in 30.
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .ds_concentration The prepared Concentration dataset.
#' @param .grain Character. "first" as 104 does; "mean" for the firm-year mean of the country count.
#' @param .bins Integer. Equal-count bins.
#' @param .trim Character. "drop" as 104 does; "p99" clips both variables instead.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_diversity <- function(.ds_contracts, .ds_concentration, .grain = c("first", "mean"), .bins = 85L,
                                 .trim = c("drop", "p99"), .dirs, .name = "Diversity") {
  if (FALSE) {
    .ds_contracts     <- lst_ds$Contracts
    .ds_concentration <- lst_ds$Concentration
    .grain            <- "first"
    .bins             <- 85L
    .trim             <- "drop"
    .dirs             <- .lP$Output
    .name             <- "Diversity"
  }
  .grain <- match.arg(.grain)
  .trim  <- match.arg(.trim)
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S3_Matched) |>
    dplyr::select("DocID", "gvkey", "fyear", "Countries") |>
    dplyr::collect()
  conc_ <- dplyr::collect(.ds_concentration)
  dat_ <- fin_data_f07(
    .tab    = rows_,
    .sample = "S3_Matched",
    .conc   = conc_,
    .grain  = .grain
  )
  b_ <- fin_bins_f07(
    .tab_data = dat_,
    .bins     = .bins,
    .trim     = .trim
  )
  tbl_out(
    .tab   = fin_fmt(b_$Fit, 2L),
    .title = "The line behind the figure: slope, t and N (revision: 1.2, t 33)",
    .notes = c(N = "Firm-years after the trim; the regression is on these rows, the dots are their bin means.")
  )
  zero_ <- mean(dat_$Diversity == 0)
  note_ <- c(
    "This figure shows a scatter plot of the number of countries mentioned in a firm's contracts against",
    "the geographical diversity of its segment sales, one minus the sum of squared segment sales shares",
    "(Bushman et al., 2004; Godigbe et al., 2024), by firm and fiscal year on the unique-contract sample",
    "matched to Compustat.",
    if (.grain == "first") {
      "Each firm-year contributes the country count of one of its contracts."
    } else {
      "Each firm-year contributes the mean country count of its contracts."
    },
    if (.trim == "drop") {
      "Firm-years in the top percentile of either variable are dropped."
    } else {
      "Both variables are winsorised at the 99th percentile."
    },
    paste0("The dots are the means of ", .bins, " equal-count bins of diversity (N = ",
           format(b_$Fit$N, big.mark = ","), " firm-years; ", fin_fmt_num(100 * zero_, 0L),
           " percent report a single geographic segment and sit at zero). The line is the OLS fit on the"),
    paste0("underlying firm-years: coefficient ", fin_fmt_num(b_$Fit$Slope, 2L), ", t-statistic ",
           fin_fmt_num(b_$Fit$TStat, 1L), ".")
  )
  fin_figure_save(
    .plot   = fin_plot_diversity(.bins = b_$Bins, .fit = b_$Fit),
    .data   = dplyr::mutate(b_$Bins, Grain = .grain, Trim = .trim, Slope = b_$Fit$Slope, TStat = b_$Fit$TStat,
                            NFit = b_$Fit$N),
    .note   = note_,
    .name   = .name,
    .dirs   = .dirs,
    .height = plot_height(8L)
  )
}

# -- 19.10 Filing metrics by firm fundamentals ------------------------------------------------------------------------------

#' Panel A's data: contracts per firm-quarter for the smallest and largest size quartile, by channel
#'
#' 104 step for step, on the full quarter panel inside the window -- the sample restrictions above
#' the figure in 104 are commented out. Size quartiles of winsorised total assets within fiscal
#' year; fiscal quarters 2001q2 to 2024q1; the mean per quarter and quartile is the quartile's
#' contracts divided by its distinct CIKs, non-filing quarters included. Periodic is 10-K, 10-Q and
#' 20-F; current is every other form, registration statements included, which is what 103 calls
#' contracts_current and 104 labels "8-K".
#'
#' @param .ds_quarter The prepared Quarter dataset.
#' @return Tibble: fyear, fqtr, YQ, Quartile, Size (Small / Large), nFirms, Periodic, Current, All.
fin_data_size_channel <- function(.ds_quarter) {
  if (FALSE) .ds_quarter <- lst_ds$Quarter
  q_ <- .ds_quarter |>
    dplyr::filter(.data$S5_Quarter) |>
    dplyr::select("cik", "fyear", "fqtr", "AtqWin", "nContractsQ", "nPeriodicQ", "nCurrentQ") |>
    dplyr::collect() |>
    dplyr::filter(!is.na(.data$AtqWin), !is.na(.data$fyear), !is.na(.data$fqtr)) |>
    dplyr::mutate(Quartile = dplyr::ntile(.data$AtqWin, 4L), .by = "fyear") |>
    dplyr::mutate(YQ = .data$fyear + (.data$fqtr - 1L) / 4) |>
    dplyr::filter(.data$YQ >= 2001.25, .data$YQ <= 2024)          # 2001q2 to 2024q1, as 104 windows it
  q_ |>
    dplyr::filter(.data$Quartile %in% c(1L, 4L)) |>
    dplyr::summarise(
      nFirms   = dplyr::n_distinct(.data$cik),
      Periodic = sum(.data$nPeriodicQ) / dplyr::n_distinct(.data$cik),
      Current  = sum(.data$nCurrentQ) / dplyr::n_distinct(.data$cik),
      All      = sum(.data$nContractsQ) / dplyr::n_distinct(.data$cik),
      .by = c("fyear", "fqtr", "YQ", "Quartile")
    ) |>
    dplyr::mutate(Size = dplyr::if_else(.data$Quartile == 1L, "Small", "Large")) |>
    dplyr::arrange(.data$Quartile, .data$YQ)
}

#' Panel B's data: mean contracts per firm-quarter by Fama-French industry
#' @param .ds_quarter The prepared Quarter dataset.
#' @return Tibble: FfInd (factor in display order), nQuarters, Mean.
fin_data_industry <- function(.ds_quarter) {
  if (FALSE) .ds_quarter <- lst_ds$Quarter
  .ds_quarter |>
    dplyr::filter(.data$S5_Quarter) |>
    dplyr::select("FfIndLab", "nContractsQ") |>
    dplyr::collect() |>
    dplyr::filter(!is.na(.data$FfIndLab)) |>
    dplyr::summarise(nQuarters = dplyr::n(), Mean = mean(.data$nContractsQ), .by = "FfIndLab") |>
    dplyr::mutate(FfInd = factor(as.character(.data$FfIndLab), levels = .fin_ff12_levels)) |>
    dplyr::select("FfInd", "nQuarters", "Mean") |>
    dplyr::arrange(.data$FfInd)
}

#' The figure: Panel A as two channel panels with small and large firms, Panel B the industry bars
#' @param .size Tibble from fin_data_size_channel().
#' @param .industry Tibble from fin_data_industry().
#' @return A patchwork.
fin_plot_firm_fundamentals <- function(.size, .industry) {
  if (FALSE) {
    .size     <- fin_data_size_channel(.ds_quarter = lst_ds$Quarter)
    .industry <- fin_data_industry(.ds_quarter = lst_ds$Quarter)
  }
  blues_ <- plot_pal_seq(3L)
  cols_  <- c(Small = blues_[[2L]], Large = blues_[[3L]])
  long_ <- .size |>
    tidyr::pivot_longer(c("Periodic", "Current"), names_to = "Channel", values_to = "Mean") |>
    dplyr::mutate(
      Channel = factor(dplyr::if_else(.data$Channel == "Periodic", "Periodic filings", "Current reports"),
                       levels = c("Periodic filings", "Current reports")),
      Size    = factor(.data$Size, levels = c("Small", "Large"))
    )
  a_ <- long_ |>
    ggplot2::ggplot(ggplot2::aes(x = .data$YQ, y = .data$Mean, colour = .data$Size, linetype = .data$Size)) +
    ggplot2::geom_line(linewidth = 0.55) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Channel), ncol = 2L) +
    ggplot2::scale_colour_manual(values = cols_, name = NULL) +
    ggplot2::scale_linetype_manual(values = c(Small = "solid", Large = "dashed"), name = NULL) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.1),
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_x_continuous(breaks = seq(2004, 2024, 4), expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::labs(x = NULL, y = "Contracts per firm-quarter", title = "A. Firm size") +
    plot_theme(.grid = "y", .legend = "bottom") +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(10, "pt"))
  b_ <- ggplot2::ggplot(.industry, ggplot2::aes(y = forcats::fct_rev(.data$FfInd), x = .data$Mean)) +
    ggplot2::geom_col(
      fill      = blues_[[1L]],
      colour    = .plot_ref,
      linewidth = .plot_line,
      width     = 0.8
    ) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = formatC(.data$Mean, format = "f", digits = 2)),
      hjust   = -0.15,
      size    = .plot_base / ggplot2::.pt * 0.75,
      family  = .plot_font
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ggplot2::labs(x = "Contracts per firm-quarter", y = NULL, title = "B. Industry") +
    plot_theme(.grid = "x", .legend = "none")
  patchwork::wrap_plots(a_, b_, ncol = 2L, widths = c(1.7, 1)) &
    ggplot2::theme(plot.title = ggplot2::element_text(family = .plot_font, size = .plot_base, face = "bold"))
}

#' The firm-fundamentals figure: built on the quarter panel as 104 builds it, saved, shown
#' @param .ds_quarter The prepared Quarter dataset.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @param .ref Tibble. 30's reference readings of the revision's Panel B.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_firm_fundamentals <- function(.ds_quarter, .dirs, .name = "FirmFundamentals", .ref = .fin_reference_f08) {
  if (FALSE) {
    .ds_quarter <- lst_ds$Quarter
    .dirs       <- .lP$Output
    .name       <- "FirmFundamentals"
    .ref        <- .fin_reference_f08
  }
  size_ <- fin_data_size_channel(.ds_quarter = .ds_quarter)
  ind_  <- fin_data_industry(.ds_quarter = .ds_quarter)
  tbl_out(
    .tab   = ind_ |>
      dplyr::mutate(FfInd = as.character(.data$FfInd)) |>
      dplyr::left_join(
        dplyr::select(.ref, FfInd = "Industry", "Revision"),
        by = dplyr::join_by(FfInd)
      ) |>
      fin_fmt(2L),
    .title = "Panel B: contracts per firm-quarter by industry, beside the revision's bars"
  )
  n_q_ <- .ds_quarter |>
    dplyr::filter(.data$S5_Quarter) |>
    dplyr::count() |>
    dplyr::collect() |>
    dplyr::pull("n")
  note_ <- c(
    "This figure shows filing activity by firm characteristics on the firm-quarter panel: every fiscal",
    paste0("quarter between 2001 and 2024 of every firm that filed at least one contract in the sample (",
           format(n_q_, big.mark = ","), " firm-quarters), non-filing quarters included."),
    "Panel A gives the mean number of contracts per firm-quarter for firms in the smallest and the",
    "largest quartile of winsorised total assets, quartiles defined within each fiscal year, separately",
    "for periodic filings (10-K, 10-Q, 20-F) and current reports (every other form, registration",
    "statements included), by fiscal quarter from 2001q2 to 2024q1.",
    "Panel B gives the mean number of contracts per firm-quarter by Fama-French 12 industry."
  )
  fin_figure_save(
    .plot   = fin_plot_firm_fundamentals(.size = size_, .industry = ind_),
    .data   = dplyr::bind_rows(
      dplyr::mutate(size_, Panel = "A"),
      dplyr::mutate(ind_, Panel = "B", FfInd = as.character(.data$FfInd))
    ),
    .note   = note_,
    .name   = .name,
    .dirs   = .dirs,
    .height = plot_height(8L),
    .width  = 10
  )
}

# -- 19.11 Redactions by industry and category ------------------------------------------------------------------------------

#' The heat map: industries down, categories across under their super-category, one row per FAST period
#'
#' The cell is the share of contracts redacted among the contracts of that industry and category
#' in that period, which is the quantity the referee asked for. A cell under .min_n contracts, and a
#' cell without any, is grey with a dash: a share on a handful of contracts is not reported.
#'
#' @param .tab_data Tibble from fin_data_f09() for one sample, one period (or "All").
#' @param .min_n Integer. Cells with fewer contracts are grey with a dash.
#' @param .limit Numeric. Top of the fill scale; one value across the three periods keeps them comparable.
#' @param .accuracy Numeric. The precision of a cell's label, as scales::label_percent() takes it.
#' @param .strips Character. "top" heads each super-category's columns with its name; "bottom" sets the name under
#'   the category labels. Either way the name is text, without a box.
#' @return A ggplot.
fin_plot_redactions_heat <- function(.tab_data, .min_n = 30L, .limit = 1, .accuracy = 0.1, .strips = c("top", "bottom")) {
  if (FALSE) {
    .tab_data <- dat_
    .min_n    <- 30L
    .limit    <- 1
    .accuracy <- 0.1
    .strips   <- "top"
  }
  .strips <- match.arg(.strips)
  if (length(unique(.tab_data$Period)) != 1L) cli::cli_abort("One period per figure; the rows carry several.")
  cat_ <- fin_tab_categories() |>
    dplyr::mutate(Bar = dplyr::if_else(nzchar(.data$Level2), .data$Level2, .data$Level1)) |>
    dplyr::select("Class", "Level1", "Bar")
  # EVERY CELL IS DRAWN. An industry that filed no contract of a category in the period has no row,
  # and a missing row is a hole in the grid that reads as background; completed to N = 0, it is a
  # grey cell with a dash, which is what "no contracts" looks like. A thin cell, under .min_n
  # contracts, is set the same way: its share is not reported, so no cell is left without a label.
  dat_ <- .tab_data |>
    dplyr::select("Industry", "Class", "N", "Share") |>
    tidyr::complete(Industry = .fin_ff12_levels, Class = cat_$Class, fill = list(N = 0L, Share = NA_real_)) |>
    dplyr::inner_join(
      cat_,
      by = dplyr::join_by(Class)
    ) |>
    dplyr::mutate(
      Level1   = factor(.data$Level1, levels = unique(cat_$Level1)),
      Bar      = factor(.data$Bar, levels = cat_$Bar),
      Industry = factor(.data$Industry, levels = rev(.fin_ff12_levels)),
      Share    = dplyr::if_else(.data$N >= .min_n, .data$Share, NA_real_),   # thin and empty cells: not reported
      Label    = dplyr::if_else(
        is.na(.data$Share),
        "-",
        scales::label_percent(accuracy = .accuracy)(.data$Share)
      ),
      Dark     = dplyr::coalesce(.data$Share > 0.4 * .limit, FALSE)
    )
  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Bar, y = .data$Industry, fill = .data$Share)) +
    ggplot2::geom_tile(colour = "#FFFFFF", linewidth = 0.5) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = .data$Label, colour = .data$Dark),
      size    = .plot_base / ggplot2::.pt * 0.8,
      family  = .plot_font
    ) +
    ggplot2::facet_grid(
      cols   = ggplot2::vars(.data$Level1),
      scales = "free_x",
      space  = "free_x",
      switch = if (.strips == "bottom") "x" else NULL   # "top": the super-category heads its columns
    ) +
    ggplot2::scale_fill_gradient(
      low    = "#DCE6F1",
      high   = "#002147",
      limits   = c(0, .limit),
      oob      = scales::oob_squish,
      na.value = "#F2F2F2",
      labels = scales::label_percent(accuracy = 1),
      name   = "Share redacted"
    ) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "#FFFFFF", `FALSE` = .plot_ink), guide = "none") +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "none", .legend = "right") +
    ggplot2::theme(
      axis.text.x      = ggplot2::element_text(angle = 40, hjust = 1),
      axis.ticks       = ggplot2::element_blank(),
      panel.spacing.x  = ggplot2::unit(3, "pt"),
      strip.placement  = "outside",
      strip.background = ggplot2::element_blank(),                   # the super-category as text, no box
      strip.text.x     = ggplot2::element_text(size = .plot_base * 0.8, face = "bold"),
      legend.title     = ggplot2::element_text(family = .plot_font, size = .plot_base - 1)
    )
}

#' The redactions heat map for one period: built from 30's within-cell shares, saved, shown
#'
#' Three figures from one tibble: the whole window from 2008, the years before the FAST Act and the
#' years after it. The pooled cell is the contract-weighted mean of the two periods' cells. All three
#' share one fill scale so a shade means the same thing on each page.
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .ds_quarter The prepared Quarter dataset, for the industry.
#' @param .period Character. "all", "pre" or "post".
#' @param .min_n Integer. Cells with fewer contracts are grey with a dash.
#' @param .limit Numeric. Top of the fill scale, shared across the three.
#' @param .accuracy Numeric. The precision of a cell's label, as scales::label_percent() takes it.
#' @param .strips Character. Where the super-category names go; see fin_plot_redactions_heat().
#' @param .dirs List. .lP$Output.
#' @param .name Character or NULL. The stem; NULL derives RedactionsIndustry<Period>.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_redactions_heat <- function(.ds_contracts, .ds_quarter, .period = c("all", "pre", "post"), .min_n = 30L,
                                       .limit = 0.8, .accuracy = 0.1, .strips = "top", .dirs, .name = NULL) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .ds_quarter   <- lst_ds$Quarter
    .period       <- "all"
    .min_n        <- 30L
    .limit        <- 0.8
    .accuracy     <- 0.1
    .strips       <- "top"
    .dirs         <- .lP$Output
    .name         <- NULL
  }
  .period <- match.arg(.period)
  name_ <- if (is.null(.name)) paste0("RedactionsIndustry", c(all = "All", pre = "Pre", post = "Post")[[.period]]) else
    .name
  rows_ <- .ds_contracts |>
    dplyr::filter(.data$S3b_Matched2008) |>
    dplyr::select("DocID", "gvkey", "fyear", "fqtr", "Year", "Redacted", "Class", "PostFast") |>
    dplyr::collect()
  qtr_ <- .ds_quarter |>
    dplyr::select("gvkey", "fyear", "fqtr", "FfIndLab") |>
    dplyr::collect()
  dat_ <- fin_data_f09(
    .tab     = rows_,
    .sample  = "S3b_Matched2008",
    .quarter = qtr_
  )
  if (.period == "all") fin_report_f09(.tab_data = dat_)
  one_ <- switch(.period,
    all  = dat_ |>
      dplyr::summarise(
        Share = sum(.data$Share * .data$N) / sum(.data$N),
        N     = sum(.data$N),
        .by   = c("Sample", "Industry", "Class")
      ) |>
      dplyr::mutate(Period = "All"),
    pre  = dplyr::filter(dat_, .data$Period == "Pre-FAST"),
    post = dplyr::filter(dat_, .data$Period == "Post-FAST")
  )
  n_     <- sum(one_$N)
  thin_  <- sum(one_$N < .min_n)
  empty_ <- 12L * 12L - nrow(one_)
  digits_ <- min(2L, max(0L, as.integer(round(-log10(.accuracy)))))
  when_ <- switch(.period,
    all  = "from 2008",
    pre  = "from 2008 to the FAST Act (April 2019)",
    post = "after the FAST Act (April 2019)"
  )
  note_ <- c(
    "This figure shows the share of contracts redacted within each industry and contract category, on",
    paste0("the unique-contract sample matched to a Compustat quarter, contracts filed ", when_, " (N = ",
           format(n_, big.mark = ","), " contracts with an industry, a category and a redaction measure)."),
    "A contract is redacted where it is covered by a granted confidential treatment order (through 2018)",
    "or its text carries a redaction marker (from 2019). Industries are the Fama-French 12 sectors of",
    "the filing firm; categories are grouped by super-category as in the categories table. The fill runs",
    paste0("from 0 to ", fin_fmt_num(100 * .limit, 0L), " percent on every version of this figure."),
    paste0("Shares are rounded to ", c("whole percent", "one decimal", "two decimals")[digits_ + 1L], "."),
    paste0("Cells with fewer than ", .min_n, " contracts are grey with a dash: ", thin_, " with some contracts and ",
           empty_, " with none, of 144.")
  )
  fin_figure_save(
    .plot   = fin_plot_redactions_heat(
      .tab_data = one_,
      .min_n    = .min_n,
      .limit    = .limit,
      .accuracy = .accuracy,
      .strips   = .strips
    ),
    .data   = one_,
    .note   = note_,
    .name   = name_,
    .dirs   = .dirs,
    .height = plot_height(12L),
    .width  = 10
  )
}


# 20. Manifest -----------------------------------------------------------------------------------------------------------
# Every exhibit this document writes, by the name its files take: what it is, the sample it is drawn
# on, and which files exist under Output/. The registry names the stems; the disk says what is
# there. Where is empty until the manuscript / online appendix / memo split is decided.

.fin_registry <- tibble::tribble(
  ~Stem, ~Kind, ~Sample, ~Description,
  "Categories", "Table", "--",
  "The twelve categories and their definitions",
  "SampleSelection", "Table", "S0 to S2; estimation ladder",
  "Sample selection, Panel A unique contracts, Panel B estimation",
  "SampleAttrition", "Table", "matched copies; S5_Quarter ever-filers",
  "Rows lost per regression requirement, both grains (authors)",
  "FluidityMissing", "Table", "S5_Quarter ever-filers",
  "Firm-quarters without fluidity, by loss, size, year (authors)",
  "ContentRule", "Table", "S2_Descriptive",
  "Words, duration, parties, countries, states by category, rule-based",
  "ContentNaive", "Table", "S2_Descriptive",
  "The same on the naive extraction",
  "ContentContrast", "Table", "S2_Descriptive",
  "N per measure, naive against rule-based",
  "PartiesDetail", "Table", "S2_Descriptive",
  "Parties by role (authors)",
  "ClassConfidence", "Table", "S2_Descriptive, labelled",
  "Transformer probability of the assigned category",
  "MoneyDetail", "Table", "S2_Descriptive",
  "Monetary amounts (authors)",
  "RedactionsDetail", "Table", "S6_Redaction",
  "Redaction markers, extensive and intensive margin (authors)",
  "SummaryVariables", "Table", "--",
  "Definitions of the Item 1.01 summary measures",
  "SummarySample", "Table", "S7_All to S7_Summaries",
  "The announcement ladder",
  "SummaryLag", "Table", "S7_Summaries",
  "Announcement lag before winsorising (authors)",
  "Summaries", "Table", "S7_Summaries",
  "Item 1.01 summaries, delayed against attached",
  "SummariesRegression", "Table", "S7_Summaries",
  "The delay indicator in a regression, nine outcomes",
  "SummariesText", "Table", "S7_Summaries",
  "Delayed against attached, the seven text measures",
  "SummariesTextRegression", "Table", "S7_Summaries",
  "The delay indicator in a regression, seven outcomes",
  "ClassLabelledFull", "Table", "labelled sample (03A)",
  "The labelled sample by category",
  "ClassTransformerFull", "Table", "labelled sample, out of fold",
  "Transformer scores by category",
  "ClassAmendmentFull", "Table", "labelled sample, out of fold",
  "Amendment classifier scores",
  "ClassKeywordFull", "Table", "labelled sample, out of fold",
  "Keyword arm scores by category",
  "ClassArmsFull", "Table", "labelled sample, out of fold",
  "Every engine on every task, routing ceiling",
  "ClassArmsFullCeiling", "Data", "labelled sample, out of fold",
  "The routing ceiling beside the arms, as its own tibble",
  "ClassSweep", "Table", "labelled sample, out of fold",
  "Model selection: the sweep",
  "ClassConfusionFull", "Table", "labelled sample, out of fold",
  "Confusion matrix, detailed",
  "FilingTypes", "Figure", "S2_Descriptive",
  "Distribution of filing types",
  "FilingsTime", "Figure", "S2_Descriptive",
  "Filings over time, four groups, total on the right axis",
  "FilingsSeasoned", "Figure", "S2_Descriptive, S2s_Seasoned",
  "All against seasoned filers: the 2021 spike",
  "ContractTypes", "Figure", "S2_Descriptive",
  "Contract types: count and share, per firm-year",
  "TypesTimeA", "Figure", "S2_Descriptive",
  "Types over time, stacked by super-category",
  "TypesTimeB", "Figure", "S2_Descriptive",
  "Types over time, twelve small multiples",
  "TypesTimeC", "Figure", "S2_Descriptive",
  "Types over time, one panel per super-category",
  "TypesTimeD", "Figure", "S2_Descriptive",
  "Types over time, the revision's twelve-stack (reference)",
  "ContentTimeSD", "Figure", "S2_Descriptive",
  "Content measures over time, mean +/- 1 SD",
  "ContentTimeIQR", "Figure", "S2_Descriptive",
  "Content measures over time, interquartile range",
  "ContentTimeCI", "Figure", "S2_Descriptive",
  "Content measures over time, 95% CI of the mean",
  "ContentTimeIndex", "Figure", "S2_Descriptive",
  "Content measures over time, index 2001 = 100",
  "RedactionsTime", "Figure", "S6_Redaction",
  "Redactions over time, two identifications",
  "MapStatesCounterparty", "Figure", "S2_Descriptive",
  "States attached to a counterparty, ranked",
  "MapStatesRecital", "Figure", "S2_Descriptive",
  "States attached to a recital party, ranked",
  "MapCountriesCounterparty", "Figure", "S2_Descriptive",
  "Countries attached to a counterparty, ranked",
  "MapCountriesRecital", "Figure", "S2_Descriptive",
  "Countries attached to a recital party, ranked",
  "FlowsStates", "Figure", "S2_Descriptive",
  "Registrant state to counterparty state",
  "FlowsCountries", "Figure", "S2_Descriptive",
  "U.S. registrant to counterparty country",
  "Diversity", "Figure", "S3_Matched, one contract per firm-year",
  "Countries mentioned against segment diversity",
  "FirmFundamentals", "Figure", "S5_Quarter",
  "Contracts per firm-quarter by size and channel, and by industry",
  "RedactionsIndustryAll", "Figure", "S3b_Matched2008",
  "Redaction share by industry and category, from 2008",
  "RedactionsIndustryPre", "Figure", "S3b_Matched2008, pre-FAST",
  "Redaction share by industry and category, before the FAST Act",
  "RedactionsIndustryPost", "Figure", "S3b_Matched2008, post-FAST",
  "Redaction share by industry and category, after the FAST Act"
)

# 31: THE EXHIBITS MOVED FROM 40, and what every stem needs beyond the four columns above: its topic -- the section of
# the runbook that builds it -- and its origin, the document whose output the move is checked against, under the
# name the stem had there. The six full classification tables are ...Full here; the cuts keep the plain names.
.fin_registry_moved <- tibble::tribble(
  ~Stem, ~Kind, ~Sample, ~Description,
  "ExhibitRequired", "Table", "--",
  "Row (10) of the Item 601 exhibit table, with what the database covers",
  "FormDescriptions", "Table", "--",
  "The forms that carry material contracts and the CFR sections that establish them",
  "FormCoverage", "Table", "master index; release",
  "Filings per form type, and what the database holds of them",
  "ExhibitFiles", "Table", "document lists, 1998-2001",
  "Exhibit 10 attachments available as separate files, by quarter",
  "TextCounts", "Data", "release",
  "Counts the text cites that no table prints",
  "PipelineStages", "Table", "--",
  "The pipeline, stage by stage",
  "PackageVersions", "Data", "--",
  "The installed versions of the published packages",
  "FilingsWithinYear", "Figure", "release",
  "When in the year contracts are filed",
  "SeasonedFilers", "Figure", "release; master index; landing pages",
  "All filers against seasoned filers, three panels",
  "CategoryTitles", "Table", "S2_Descriptive",
  "What filers call their contracts, by category",
  "ClassLabelled", "Table", "labelled sample (03A)",
  "The labelled sample by category, the appendix's cut",
  "ClassDeployed", "Table", "labelled sample, out of fold",
  "The configurations that left the sweep",
  "ClassTransformer", "Table", "labelled sample, out of fold",
  "Transformer scores by category, the appendix's cut",
  "ClassConfusion", "Table", "labelled sample, out of fold",
  "Confusion matrix, the appendix's cut",
  "ClassAmendment", "Table", "labelled sample, out of fold",
  "Amendment classifier scores, the appendix's cut",
  "ClassKeyword", "Table", "labelled sample, out of fold",
  "Keyword table scores by category, the appendix's cut",
  "ClassArms", "Table", "labelled sample, out of fold",
  "Every engine on every task and the ceiling, the appendix's cut",
  "ClassSecondPairs", "Table", "labelled sample, two labels",
  "Primary against second label",
  "ClassSecond", "Table", "labelled sample, two labels",
  "The model's two choices against the two labels",
  "EntityCoverage", "Table", "labelled sample (04A)",
  "What each extractor found on the labelled sample",
  "EntityPositions", "Figure", "labelled sample (04A)",
  "Where in the contract each extractor's candidates fall",
  "EntityAgreement", "Figure", "labelled sample (04A)",
  "How far the extractors agree about what is there",
  "DurationRungs", "Table", "S2_Descriptive",
  "The source of each contract's end date",
  "ContentValues", "Table", "S2_Descriptive",
  "Naive and rule-based content values by category",
  "CtoLinkage", "Table", "orders; release",
  "From the order to the contract it covers",
  "CtoAhci", "Data", "orders, new grants through 2019",
  "Orders and exhibits on the forms Ahci (2025) covers",
  "CtoMarkers", "Data", "release, 2008-2018",
  "Orders against text markers"
)

# 31: EXHIBITS FIRST BUILT HERE, with no origin to check the move against.
.fin_registry_new <- tibble::tribble(
  ~Stem, ~Kind, ~Sample, ~Description,
  "ContentReadings", "Table", "S2_Descriptive",
  "Naive against rule-based reading per measure: share with a value and value, totals",
  "FirmFundamentalsA", "Figure", "S5_Quarter",
  "Firm fundamentals under choice: panels stacked, Panel A by fiscal quarter",
  "FirmFundamentalsB", "Figure", "S5_Quarter",
  "Firm fundamentals under choice: panels stacked, Panel A as fiscal-year means",
  "FirmFundamentalsC", "Figure", "S5_Quarter",
  "Firm fundamentals under choice: panels stacked, Panel A as trailing four-quarter means",
  "FirmSize", "Figure", "S5_Quarter",
  "Firm fundamentals under choice, split: size quartile and channel, fiscal-year means",
  "FirmIndustry", "Figure", "S5_Quarter",
  "Firm fundamentals under choice, split: contracts per firm-quarter by industry"
)

.fin_renamed <- c(
  ClassLabelledFull    = "ClassLabelled",
  ClassTransformerFull = "ClassTransformer",
  ClassAmendmentFull   = "ClassAmendment",
  ClassKeywordFull     = "ClassKeyword",
  ClassArmsFull        = "ClassArms",
  ClassArmsFullCeiling = "ClassArmsCeiling",
  ClassConfusionFull   = "ClassConfusion"
)

# THE TOPICS, in the runbook's order. The paper's own order is still moving, so the runbook follows the subject;
# where an exhibit is printed is the manifest's Where column, decided apart from this.
.fin_topic_of <- list(
  Sources        = c("ExhibitRequired", "FormDescriptions", "FormCoverage", "ExhibitFiles", "TextCounts",
                     "PipelineStages", "PackageVersions"),
  Samples        = c("SampleSelection", "SampleAttrition", "FluidityMissing"),
  Filing         = c("FilingTypes", "FilingsTime", "FilingsWithinYear", "FilingsSeasoned", "SeasonedFilers",
                     "FirmFundamentals", "FirmFundamentalsA", "FirmFundamentalsB", "FirmFundamentalsC", "FirmSize",
                     "FirmIndustry"),
  Types          = c("Categories", "CategoryTitles", "ContractTypes", "TypesTimeA", "TypesTimeB", "TypesTimeC",
                     "TypesTimeD", "ClassConfidence"),
  Classification = c("ClassLabelledFull", "ClassTransformerFull", "ClassAmendmentFull", "ClassKeywordFull",
                     "ClassArmsFull", "ClassArmsFullCeiling", "ClassSweep", "ClassConfusionFull", "ClassLabelled",
                     "ClassDeployed", "ClassTransformer", "ClassConfusion", "ClassAmendment", "ClassKeyword",
                     "ClassArms", "ClassSecondPairs", "ClassSecond"),
  Content        = c("EntityCoverage", "EntityPositions", "EntityAgreement", "ContentRule", "ContentNaive",
                     "ContentContrast", "PartiesDetail", "MoneyDetail", "ContentValues", "DurationRungs",
                     "ContentReadings", "ContentTimeSD", "ContentTimeIQR", "ContentTimeCI", "ContentTimeIndex"),
  Geography      = c("MapStatesCounterparty", "MapStatesRecital", "MapCountriesCounterparty",
                     "MapCountriesRecital", "FlowsStates", "FlowsCountries", "Diversity"),
  Redactions     = c("CtoLinkage", "CtoAhci", "CtoMarkers", "RedactionsDetail", "RedactionsTime",
                     "RedactionsIndustryAll", "RedactionsIndustryPre", "RedactionsIndustryPost"),
  Announcements  = c("SummarySample", "SummaryLag", "SummaryVariables", "Summaries", "SummariesText",
                     "SummariesRegression", "SummariesTextRegression")
)

.fin_registry <- dplyr::bind_rows(
  .fin_registry,
  .fin_registry_moved,
  .fin_registry_new
) |>
  dplyr::left_join(
    tibble::tibble(
      Stem  = unlist(.fin_topic_of, use.names = FALSE),
      Topic = rep(names(.fin_topic_of), lengths(.fin_topic_of))
    ),
    by = "Stem"
  ) |>
  dplyr::mutate(
    Origin     = dplyr::case_when(
      .data$Stem %in% .fin_registry_moved$Stem ~ "40",
      .data$Stem %in% .fin_registry_new$Stem   ~ "31",
      .default                                 = "30"
    ),
    OriginStem = dplyr::coalesce(unname(.fin_renamed[.data$Stem]), .data$Stem)
  )

if (anyNA(.fin_registry$Topic) || anyDuplicated(.fin_registry$Stem) > 0L) {
  cli::cli_abort("The exhibit registry needs exactly one row and one topic per stem.")
}

#' The manifest: every registered stem with the files it has on disk, written as csv and shown
#'
#' The registry says what an exhibit is and what it is drawn on; the disk says which files exist.
#' A registered stem with no files is a chunk that did not run; a file with no registered stem is an
#' exhibit nobody catalogued. Both are printed, because both are mistakes.
#'
#' @param .dirs List. .lP$Output.
#' @param .registry Tibble. The exhibit registry.
#' @param .path Character. Where the csv goes.
#' @return Invisibly, the manifest tibble.
fin_manifest <- function(.dirs, .registry = .fin_registry, .path) {
  if (FALSE) {
    .dirs     <- .lP$Output
    .registry <- .fin_registry
    .path     <- utils_file_path(.dir_main, "Output", "Manifest.csv")
  }
  # THE FILE NAMES, relative to Output/, as they are on disk: the manuscript's \input and
  # \includegraphics take exactly these strings.
  rel_ <- function(.dir, .ext) {
    p_ <- fs::path(.dir, paste0(.registry$Stem, ".", .ext))
    dplyr::if_else(fs::file_exists(p_), paste0(fs::path_file(.dir), "/", fs::path_file(p_)), NA_character_)
  }
  out_ <- .registry |>
    dplyr::mutate(
      Where   = "",
      Data    = rel_(.dirs$DirData, "parquet"),
      Tex     = rel_(.dirs$DirTables, "tex"),
      Pdf     = rel_(.dirs$DirFigures, "pdf"),
      Png     = rel_(.dirs$DirFigures, "png"),
      Note    = rel_(.dirs$DirNotes, "tex")
    ) |>
    dplyr::mutate(
      Files = purrr::pmap_chr(list(.data$Tex, .data$Pdf, .data$Png, .data$Note, .data$Data), \(...) {
        paste(stats::na.omit(c(...)), collapse = ", ")
      })
    )
  missing_ <- out_$Stem[is.na(out_$Data) & is.na(out_$Tex) & is.na(out_$Png)]
  if (length(missing_) > 0L) cli::cli_alert_warning("Registered without files on disk: {missing_}.")
  on_disk_ <- c(
    fs::path_ext_remove(fs::path_file(fs::dir_ls(.dirs$DirTables, glob = "*.tex"))),
    fs::path_ext_remove(fs::path_file(fs::dir_ls(.dirs$DirFigures, glob = "*.png")))
  )
  stray_ <- setdiff(unique(on_disk_), .registry$Stem)
  if (length(stray_) > 0L) cli::cli_alert_warning("On disk but not registered: {stray_}.")
  readr::write_csv(out_, .path)
  shown_ <- dplyr::select(out_, "Topic", "Stem", "Kind", "Where", "Sample", "Description", "Files")
  k_ <- shown_ |>
    knitr::kable(
      format   = "html",
      escape   = TRUE,
      caption  = "Every exhibit this document writes",
      align    = "lllllll",
      booktabs = TRUE
    ) |>
    kableExtra::kable_styling(
      full_width        = TRUE,
      position          = "left",
      bootstrap_options = c("hover", "condensed"),
      font_size         = 12
    ) |>
    kableExtra::footnote(
      general           = paste("Files are relative to Output/: the tabular or the figure, then the note and the data.",
                                "Where is the manuscript / online appendix / memo split, empty until decided."),
      general_title     = "Notes:",
      footnote_as_chunk = TRUE,
      threeparttable    = TRUE
    )
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(shown_)
  )
  invisible(out_)
}


# 21. Tables: what each variable costs, and who is missing fluidity -----------------------------------------------------
# Two diagnostics under the sample-selection table (decided 10 Sep). The attrition table is a
# marginal ladder: with every regression requirement present as the reference, how many rows each
# requirement removes on its own, at the contract grain and at the firm-quarter grain. A sequential
# ladder would depend on the order the requirements are listed in; a marginal one does not. The
# fluidity table asks who the firm-quarters without product-market fluidity are.

#' The firm-quarter panel with one missingness flag per regression requirement
#'
#' Requirements as 105 and 106 impose them: the fiscal quarter, the industry, the state, a state
#' in which filing varies (106's state 36), and each control. Ever-filers only, inside the window.
#'
#' @param .ds_quarter The prepared Quarter dataset.
#' @param .controls Character. The control set.
#' @return Tibble: gvkey, fyear, fqtr, cik, Filed, Loss, AtqWin, and one logical Miss_<name> per requirement.
fin_quarter_requirements <- function(.ds_quarter, .controls) {
  if (FALSE) {
    .ds_quarter <- lst_ds$Quarter
    .controls   <- .fin_controls$Resubmission
  }
  q_ <- .ds_quarter |>
    dplyr::filter(.data$S5_Quarter) |>
    dplyr::select("gvkey", "fyear", "fqtr", "cik", "Filed", "State", "FfIndLab", "AtqWin", "Loss",
                  dplyr::all_of(.controls)) |>
    dplyr::collect() |>
    dplyr::mutate(EverFiles = any(.data$Filed == 1L), .by = "gvkey") |>
    dplyr::filter(.data$EverFiles)
  # A STATE IN WHICH FILING NEVER VARIES cannot carry a state effect; 106 drops state 36 by hand,
  # 30 finds them by the rule. Same rule here.
  flat_ <- q_ |>
    dplyr::filter(!is.na(.data$State)) |>
    dplyr::summarise(Flat = dplyr::n_distinct(.data$Filed) == 1L, .by = "State") |>
    dplyr::filter(.data$Flat) |>
    dplyr::pull("State")
  out_ <- q_ |>
    dplyr::mutate(
      `Miss_Fiscal quarter`   = is.na(.data$fyear) | is.na(.data$fqtr),
      `Miss_Industry`         = is.na(.data$FfIndLab),
      `Miss_State`            = is.na(.data$State),
      `Miss_State, no variation` = !is.na(.data$State) & .data$State %in% flat_
    )
  for (c_ in .controls) out_[[paste0("Miss_", c_)]] <- is.na(out_[[c_]])
  out_
}

#' The attrition table: rows lost per requirement, contract grain and firm-quarter grain
#'
#' @param .ds_contracts The prepared Contracts dataset.
#' @param .ds_quarter The prepared Quarter dataset.
#' @param .controls Character. The control set.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the table tibble.
fin_table_attrition <- function(.ds_contracts, .ds_quarter, .controls = .fin_controls$Resubmission, .dirs,
                                .name = "SampleAttrition") {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .ds_quarter   <- lst_ds$Quarter
    .controls     <- .fin_controls$Resubmission
    .dirs         <- .lP$Output
    .name         <- "SampleAttrition"
  }
  q_ <- fin_quarter_requirements(
    .ds_quarter = .ds_quarter,
    .controls   = .controls
  )
  req_ <- sub("^Miss_", "", grep("^Miss_", names(q_), value = TRUE))
  # THE CONTRACT GRAIN: registrant copies at ladder steps 2-6 with a matched quarter, each carrying
  # its quarter's flags. Copies without a matched quarter are their own row.
  con_ <- .ds_contracts |>
    dplyr::filter(.data$SampleStepCode >= 2L) |>
    dplyr::select("DocID", "SampleStepCode", "gvkey", "fyear", "fqtr") |>
    dplyr::collect()
  n_con_ <- nrow(con_)
  con_m_ <- con_ |>
    dplyr::filter(.data$SampleStepCode == 6L) |>
    dplyr::left_join(
      dplyr::select(q_, "gvkey", "fyear", "fqtr", dplyr::starts_with("Miss_")),
      by           = dplyr::join_by(gvkey, fyear, fqtr),
      relationship = "many-to-one"
    )
  con_m_ <- con_m_ |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Miss_"), \(.x) dplyr::coalesce(.x, TRUE)))
  one_ <- function(.d, .r) {
    m_ <- .d[[paste0("Miss_", .r)]]
    tibble::tibble(Requirement = .r, Lost = sum(m_), Share = mean(m_))
  }
  any_ <- function(.d) rowSums(dplyr::select(.d, dplyr::starts_with("Miss_"))) > 0
  c_rows_ <- purrr::map(req_, \(.r) one_(con_m_, .r)) |>
    purrr::list_rbind() |>
    dplyr::rename(LostC = "Lost", ShareC = "Share")
  f_rows_ <- purrr::map(req_, \(.r) one_(q_, .r)) |>
    purrr::list_rbind() |>
    dplyr::rename(LostF = "Lost", ShareF = "Share")
  tab_ <- dplyr::inner_join(
    c_rows_,
    f_rows_,
    by = dplyr::join_by(Requirement)
  ) |>
    dplyr::mutate(Kind = "requirement")
  head_ <- tibble::tibble(
    Requirement = c("Rows before the requirements (matched quarter)", "Rows lost to any requirement",
                    "Rows with every requirement present"),
    LostC  = c(nrow(con_m_), sum(any_(con_m_)), sum(!any_(con_m_))),
    ShareC = c(1, mean(any_(con_m_)), mean(!any_(con_m_))),
    LostF  = c(nrow(q_), sum(any_(q_)), sum(!any_(q_))),
    ShareF = c(1, mean(any_(q_)), mean(!any_(q_))),
    Kind   = c("total", "total", "total")
  )
  tab_ <- dplyr::bind_rows(head_[1L, ], tab_, head_[2:3, ]) |>
    dplyr::mutate(Controls = paste(.controls, collapse = "+"), .before = 1L)
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  cells_ <- tibble::tibble(
    Requirement = tab_$Requirement,
    LostC       = fin_fmt_count(tab_$LostC),
    ShareC      = fin_fmt_num(100 * tab_$ShareC, 1L),
    LostF       = fin_fmt_count(tab_$LostF),
    ShareF      = fin_fmt_num(100 * tab_$ShareF, 1L)
  )
  body_ <- purrr::map_chr(seq_len(nrow(cells_)), \(.i) {
    r_ <- unlist(cells_[.i, ])
    r_[1L] <- fin_tex_escape(r_[1L])
    if (tab_$Kind[.i] == "total") r_ <- paste0("\\textbf{", r_, "}") else r_[1L] <- paste0("\\hspace{1em}", r_[1L])
    paste0(paste(r_, collapse = " & "), " \\\\")
  })
  body_ <- append(body_, "\\midrule", after = nrow(cells_) - 2L)
  lines_ <- c(
    "\\begingroup\\small", "\\begin{tabular}{l rr rr}", "\\toprule",
    " & \\multicolumn{2}{c}{Contracts} & \\multicolumn{2}{c}{Firm-quarters} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    " & Lost & \\% & Lost & \\% \\\\", "\\midrule", body_, "\\bottomrule", "\\end{tabular}", "\\endgroup"
  )
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  note_ <- c(
    "This table shows what each regression requirement costs. The first row is the starting point: at",
    "the contract grain, every registrant copy at ladder steps 2 to 6 matched to a Compustat fiscal",
    paste0("quarter (", format(nrow(con_m_), big.mark = ","), " of ", format(n_con_, big.mark = ","), " copies); at"),
    "the firm-quarter grain, every fiscal quarter between 2001 and 2024 of a firm that filed at least one",
    "contract. Each requirement row gives the rows that lack that requirement alone, so the rows do not",
    "add up: a row missing two variables is counted twice. The last two rows give the rows lost to any",
    "requirement and the rows that pass all of them, which is the estimation sample. State, no variation",
    "marks quarters in a state in which filing never varies, which a state effect cannot use."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_) <- c(" ", "Lost", "%", "Lost", "%")
  k_ <- cells_ |>
    knitr::kable(
      format   = "html",
      escape   = TRUE,
      caption  = "What each requirement costs",
      align    = "lrrrr",
      booktabs = TRUE
    ) |>
    kableExtra::kable_styling(
      full_width        = FALSE,
      position          = "left",
      bootstrap_options = c("hover", "condensed")
    ) |>
    kableExtra::add_header_above(c(" " = 1, "Contracts" = 2, "Firm-quarters" = 2)) |>
    kableExtra::row_spec(which(tab_$Kind == "total"), bold = TRUE) |>
    kableExtra::add_indent(which(tab_$Kind == "requirement")) |>
    kableExtra::footnote(
      general           = paste(note_, collapse = " "),
      general_title     = "Notes:",
      footnote_as_chunk = TRUE,
      threeparttable    = TRUE
    )
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(cells_)
  )
  worst_ <- tab_[tab_$Kind == "requirement", , drop = FALSE] |> dplyr::slice_max(.data$LostF, n = 1L)
  cli::cli_alert_info(
    "Costliest requirement: {(worst_$Requirement)}, {format(worst_$LostF, big.mark = ',')} firm-quarters \\
     ({fin_fmt_num(100 * worst_$ShareF, 1L)} percent); every requirement present on \\
     {format(head_$LostF[3L], big.mark = ',')} firm-quarters."
  )
  invisible(tab_)
}

#' Who is missing product-market fluidity: by loss, by size, by year
#'
#' @param .ds_quarter The prepared Quarter dataset.
#' @param .dirs List. .lP$Output.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the table tibble.
fin_table_fluidity <- function(.ds_quarter, .dirs, .name = "FluidityMissing") {
  if (FALSE) {
    .ds_quarter <- lst_ds$Quarter
    .dirs       <- .lP$Output
    .name       <- "FluidityMissing"
  }
  q_ <- fin_quarter_requirements(
    .ds_quarter = .ds_quarter,
    .controls   = "Fluidity"
  ) |>
    dplyr::mutate(
      Missing  = .data$Miss_Fluidity,
      LossLab  = dplyr::case_when(
        .data$Loss == 1L ~ "Loss firm",
        .data$Loss == 0L ~ "Profit firm",
        .default         = "Loss unknown"
      ),
      Quartile = dplyr::ntile(.data$AtqWin, 4L),
      .by      = "fyear"
    ) |>
    dplyr::mutate(SizeLab = dplyr::if_else(is.na(.data$Quartile), "Size unknown", paste0("Size quartile ", .data$Quartile)))
  # THE SHARE BEFORE THE COUNT: inside summarise() a later line sees the earlier line's result, so
  # the mean of the flag must be taken before the flag's name is reused for its sum.
  one_ <- function(.d, .col, .panel) {
    .d |>
      dplyr::summarise(
        N        = dplyr::n(),
        Share    = mean(.data$Missing),
        Filed    = mean(.data$Filed == 1L),
        Missing  = sum(.data$Missing),
        .by      = dplyr::all_of(.col)
      ) |>
      dplyr::rename(Level = dplyr::all_of(.col)) |>
      dplyr::mutate(Level = as.character(.data$Level), Panel = .panel) |>
      dplyr::arrange(.data$Level)
  }
  tab_ <- dplyr::bind_rows(
    tibble::tibble(Panel = "All", Level = "All firm-quarters", N = nrow(q_), Missing = sum(q_$Missing),
                   Share = mean(q_$Missing), Filed = mean(q_$Filed == 1L)),
    one_(q_, "LossLab", "By loss"),
    one_(q_, "SizeLab", "By size"),
    one_(dplyr::filter(q_, .data$fyear >= 2001L), "fyear", "By fiscal year")
  ) |>
    dplyr::select("Panel", "Level", "N", "Missing", "Share", "Filed")
  # THE TEST THE OTHER WAY ROUND: among the quarters without fluidity, how many are loss firms, against
  # the quarters with it. This is the sentence the note makes.
  flip_ <- q_ |>
    dplyr::summarise(
      N     = dplyr::n(),
      Loss  = mean(.data$Loss == 1L, na.rm = TRUE),
      Filed = mean(.data$Filed == 1L),
      .by   = "Missing"
    )
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )
  cells_ <- tibble::tibble(
    Level   = tab_$Level,
    N       = fin_fmt_count(tab_$N),
    Missing = fin_fmt_count(tab_$Missing),
    Share   = fin_fmt_num(100 * tab_$Share, 1L),
    Filed   = fin_fmt_num(100 * tab_$Filed, 1L)
  )
  loss_ <- tab_[tab_$Panel == "By loss", , drop = FALSE]
  miss_ <- flip_[flip_$Missing, , drop = FALSE]
  pres_ <- flip_[!flip_$Missing, , drop = FALSE]
  note_ <- c(
    "This table shows which firm-quarters lack product-market fluidity (Hoberg, Phillips and Prabhala,",
    "2014), on every fiscal quarter between 2001 and 2024 of a firm that filed at least one contract.",
    "Each row is a group of firm-quarters: their number, how many of them have no fluidity value and",
    "the share that is, and the share in which the firm filed a contract. Loss is a negative quarterly",
    "income; size quartiles are of winsorised total assets within fiscal year.",
    if (all(c("Loss firm", "Profit firm") %in% loss_$Level)) {
      paste0("Fluidity is missing on ", fin_fmt_num(100 * loss_$Share[loss_$Level == "Loss firm"], 1L),
             " percent of loss-firm quarters and ", fin_fmt_num(100 * loss_$Share[loss_$Level == "Profit firm"], 1L),
             " percent of profit-firm quarters; read the other way, ", fin_fmt_num(100 * miss_$Loss, 1L),
             " percent of the quarters without fluidity are loss firms against ", fin_fmt_num(100 * pres_$Loss, 1L),
             " percent of those with it, and the quarters without fluidity file a contract in ",
             fin_fmt_num(100 * miss_$Filed, 1L), " percent of cases against ", fin_fmt_num(100 * pres_$Filed, 1L),
             " percent.")
    } else {
      NULL
    }
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  body_ <- character(0)
  for (p_ in unique(tab_$Panel)) {
    rows_ <- cells_[tab_$Panel == p_, , drop = FALSE]
    if (length(body_) > 0L) body_ <- c(body_, "\\addlinespace[1ex]")
    body_ <- c(body_, paste0("\\multicolumn{5}{l}{\\textit{", fin_tex_escape(p_), "}} \\\\"))
    body_ <- c(body_, purrr::map_chr(seq_len(nrow(rows_)), \(.i) {
      r_ <- unlist(rows_[.i, ])
      r_[1L] <- paste0("\\hspace{1em}", fin_tex_escape(r_[1L]))
      paste0(paste(r_, collapse = " & "), " \\\\")
    }))
  }
  lines_ <- c(
    "\\begingroup\\small", "\\begin{tabular}{l r r r r}", "\\toprule",
    " & Firm-quarters & Without fluidity & Without fluidity (\\%) & Filed a contract (\\%) \\\\",
    "\\midrule", body_, "\\bottomrule", "\\end{tabular}", "\\endgroup"
  )
  fs::dir_create(.dirs$DirTables)
  fin_tex_write(
    .lines = lines_,
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )
  names(cells_) <- c(" ", "Firm-quarters", "Without fluidity", "Without fluidity (%)", "Filed a contract (%)")
  k_ <- cells_ |>
    knitr::kable(
      format   = "html",
      escape   = TRUE,
      caption  = "Which firm-quarters lack product-market fluidity",
      align    = "lrrrr",
      booktabs = TRUE
    ) |>
    kableExtra::kable_styling(
      full_width        = FALSE,
      position          = "left",
      bootstrap_options = c("hover", "condensed")
    ) |>
    kableExtra::footnote(
      general           = paste(note_, collapse = " "),
      general_title     = "Notes:",
      footnote_as_chunk = TRUE,
      threeparttable    = TRUE
    )
  for (p_ in unique(tab_$Panel)) {
    idx_ <- which(tab_$Panel == p_)
    k_ <- kableExtra::pack_rows(
      kable_input   = k_,
      group_label   = p_,
      start_row     = min(idx_),
      end_row       = max(idx_),
      label_row_css = "text-align: left; border-bottom: 1px solid;"
    )
  }
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(cells_)
  )
  cli::cli_alert_info(
    "Fluidity missing on {fin_fmt_num(100 * mean(q_$Missing), 1L)} percent of firm-quarters; \\
     {fin_fmt_num(100 * miss_$Loss, 1L)} percent of those are loss firms against {fin_fmt_num(100 * pres_$Loss, 1L)} \\
     percent where fluidity is present."
  )
  invisible(tab_)
}


# 22. Deployment to the paper folder -------------------------------------------------------------------------------------
# The manuscript reads from one folder on Dropbox: 200-Paper_figures/current/, with Figures/,
# Tables/, Notes/, the manifest and the rendered html. Every deployment first moves the previous
# current/ to _archive/<timestamp>/, as the data export does, so nothing is ever overwritten. The
# html of the render that is running cannot be copied from inside it -- Quarto writes it last --
# so the chunk copies the html that exists and records its time, and fin_deploy_html() copies the
# fresh one from the console once the render is done.

#' Archive the previous deployment and copy the exhibits into current/
#'
#' @param .dirs List. .lP$Output.
#' @param .dir_paper Character. The 200-Paper_figures folder.
#' @param .path_html Character. The rendered html, copied if it exists.
#' @param .manifest Tibble. The manifest returned by fin_manifest(); refuses to deploy an incomplete set.
#' @param .release Character. Params$ReleaseMin, recorded in VERSION.txt.
#' @param .run Logical. FALSE prints what would happen and touches nothing.
#' @return Invisibly, the current/ path.
fin_deploy_paper <- function(.dirs, .dir_paper, .path_html, .manifest, .release, .run = FALSE) {
  if (FALSE) {
    .dirs      <- .lP$Output
    .dir_paper <- .lP$Paper
    .path_html <- fs::path(.dir_main, "31-FinalExhibits.html")
    .manifest  <- tab_manifest
    .release   <- .lP$Params$ReleaseMin
    .run       <- FALSE
  }
  # COMPLETE MEANS SOME FILE EXISTS. An author-only table writes a parquet and no tabular, which is
  # not a chunk that failed to run; a stem with nothing at all is.
  incomplete_ <- .manifest$Stem[is.na(.manifest$Tex) & is.na(.manifest$Png) & is.na(.manifest$Data)]
  if (length(incomplete_) > 0L) {
    msg_ <- "{length(incomplete_)} registered exhibit{?s} without any file: {incomplete_}."
    if (.run) cli::cli_abort(c("Not deploying.", "x" = msg_)) else cli::cli_alert_warning(msg_)
  }
  cur_  <- fs::path(.dir_paper, "current")
  arch_ <- fs::path(.dir_paper, "_archive", format(Sys.time(), "%Y-%m-%d_%H%M"))
  srcs_ <- c(Figures = .dirs$DirFigures, Tables = .dirs$DirTables, Notes = .dirs$DirNotes)
  # ONLY PNGS LEAVE THE FIGURES FOLDER: a pdf left there by an earlier render is not deployed.
  files_ <- function(.d, .n) {
    if (.n == "Figures") fs::dir_ls(.d, type = "file", glob = "*.png") else fs::dir_ls(.d, type = "file")
  }
  n_files_ <- sum(purrr::imap_int(srcs_, \(.d, .n) length(files_(.d = .d, .n = .n))))
  html_ok_ <- fs::file_exists(.path_html)
  if (!.run) {
    cli::cli_alert_info(
      "Dry run: would archive {.path {cur_}} to {.path {arch_}} and copy {n_files_} files plus the manifest\\
       {if (html_ok_) 'and the html' else '(no html found)'} into it. Set .run = TRUE to deploy."
    )
    return(invisible(cur_))
  }
  if (fs::dir_exists(cur_)) {
    fs::dir_create(fs::path_dir(arch_))
    fs::file_move(cur_, arch_)
    cli::cli_alert_success("Archived the previous deployment to {.path {arch_}}.")
  }
  fs::dir_create(cur_)
  purrr::iwalk(srcs_, \(.d, .n) {
    fs::dir_create(fs::path(cur_, .n))
    fs::file_copy(files_(.d = .d, .n = .n), fs::path(cur_, .n), overwrite = TRUE)
  })
  fs::file_copy(fs::path(fs::path_dir(.dirs$DirFigures), "Manifest.csv"), cur_, overwrite = TRUE)
  html_line_ <- if (html_ok_) {
    fs::file_copy(.path_html, cur_, overwrite = TRUE)
    paste0("html: ", fs::path_file(.path_html), " from ",
           format(fs::file_info(.path_html)$modification_time, "%Y-%m-%d %H:%M"),
           " (the render before this one; run fin_deploy_html() after the render to refresh)")
  } else {
    "html: none found; run fin_deploy_html() after the render"
  }
  writeLines(
    c(
      paste0("Deployed: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
      paste0("Release the exhibits are drawn on: ", .release),
      paste0("Exhibits: ", n_files_, " files under Figures/, Tables/, Notes/; Manifest.csv"),
      html_line_
    ),
    fs::path(cur_, "VERSION.txt")
  )
  cli::cli_alert_success("Deployed {n_files_} files and the manifest to {.path {cur_}}.")
  invisible(cur_)
}

#' Copy the freshly rendered html into current/, from the console once the render is done
#' @param .path_html Character. The rendered html.
#' @param .dir_paper Character. The 200-Paper_figures folder.
#' @return Invisibly, the copied path.
fin_deploy_html <- function(.path_html, .dir_paper) {
  if (FALSE) {
    .path_html <- fs::path(.dir_main, "31-FinalExhibits.html")
    .dir_paper <- .lP$Paper
  }
  cur_ <- fs::path(.dir_paper, "current")
  if (!fs::dir_exists(cur_)) cli::cli_abort("No current deployment at {.path {cur_}}; deploy the exhibits first.")
  if (!fs::file_exists(.path_html)) cli::cli_abort("No rendered html at {.path {.path_html}}.")
  out_ <- fs::file_copy(.path_html, cur_, overwrite = TRUE)
  ver_ <- fs::path(cur_, "VERSION.txt")
  lines_ <- if (fs::file_exists(ver_)) readLines(ver_, warn = FALSE) else character(0)
  lines_ <- c(lines_[!startsWith(lines_, "html:")],
              paste0("html: ", fs::path_file(.path_html), " from ",
                     format(fs::file_info(.path_html)$modification_time, "%Y-%m-%d %H:%M")))
  writeLines(lines_, ver_)
  cli::cli_alert_success("Copied the render to {.path {out_}}.")
  invisible(out_)
}


# 23. Moved from 40-Numbers: the two tibbles no exhibit holds, and the build helper ----------------------------------------

#' Orders and references on the forms and in the period Ahci (2025) covers
#'
#' The manuscript's footnote compares our order and exhibit counts with Ahci (2025), whose sample is drawn from
#' 10-K, 10-Q and 8-K filings and ends with the FAST Act. The release's CtoOrders is one row per exhibit reference
#' an order makes, with the source form the reference names and the order's own filing date, so both restrictions
#' are filters; the totals of the linkage funnel (chapter A's CtoLinkage) are the unrestricted counts the sentence
#' before the footnote cites.
#'
#' Three counting rules per cell, because the comparison depends on them: every order (Kind "total"), orders that
#' grant including extensions of an earlier grant (Kind "granting", the export's HasCto rule), and grants that are
#' not extensions (Kind "granting-new", the rule the earlier text described as Ahci's). An order is counted where
#' at least one of its references survives the filters, a reference where it names an exhibit number.
#'
#' @param .path_orders Character. The release's CtoOrders.parquet.
#' @param .forms Character. The form types kept; amendments (form/A) are kept with their form.
#' @param .to Integer. The last order year of the restricted period.
#' @return Tibble: Row (form set), Period, Kind, Orders, References, Linked.
num_data_cto_ahci <- function(.path_orders, .forms = c("10-K", "10-Q", "8-K"), .to = 2019L) {
  if (FALSE) {
    .path_orders <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "CtoOrders.parquet"
    )
    .forms <- c("10-K", "10-Q", "8-K")
    .to    <- 2019L
  }
  ds_   <- arrow::open_dataset(sources = .path_orders)
  need_ <- c("OrderDocID", "OrderDate", "Status", "IsExtension", "ExhibitNo", "SourceForm", "DocID")
  miss_ <- setdiff(need_, names(ds_))
  if (length(miss_) > 0L) {
    cli::cli_abort("CtoOrders lacks {.val {miss_}}; the release has {.val {names(ds_)}}.")
  }
  ref_ <- ds_ |>
    dplyr::select(dplyr::all_of(need_)) |>
    dplyr::collect() |>
    dplyr::mutate(
      FormBase = sub("/A$", "", dplyr::coalesce(.data$SourceForm, "")),
      OnForm   = .data$FormBase %in% .forms,
      InPeriod = as.integer(format(as.Date(.data$OrderDate), "%Y")) <= .to,
      HasRef   = !is.na(.data$ExhibitNo),
      Linked   = !is.na(.data$DocID),
      Grants   = dplyr::coalesce(.data$Status == "GRANTING", FALSE),
      NewGrant = .data$Grants & dplyr::coalesce(as.integer(.data$IsExtension), 0L) == 0L
    )
  count_ <- function(.d, .row, .period, .kind) {
    tibble::tibble(
      Row        = .row,
      Period     = .period,
      Kind       = .kind,
      Orders     = dplyr::n_distinct(.d$OrderDocID),
      References = sum(.d$HasRef),
      Linked     = sum(.d$Linked)
    )
  }
  cell_ <- function(.d, .row, .period) {
    dplyr::bind_rows(
      count_(.d = .d, .row = .row, .period = .period, .kind = "total"),
      count_(.d = dplyr::filter(.d, .data$Grants), .row = .row, .period = .period, .kind = "granting"),
      count_(.d = dplyr::filter(.d, .data$NewGrant), .row = .row, .period = .period, .kind = "granting-new")
    )
  }
  to_ <- paste0("through ", .to)
  dplyr::bind_rows(
    cell_(.d = ref_, .row = "All forms", .period = "all years"),
    cell_(.d = dplyr::filter(ref_, .data$InPeriod), .row = "All forms", .period = to_),
    cell_(.d = dplyr::filter(ref_, .data$OnForm), .row = "10-K, 10-Q and 8-K", .period = "all years"),
    cell_(.d = dplyr::filter(ref_, .data$OnForm, .data$InPeriod), .row = "10-K, 10-Q and 8-K", .period = to_)
  )
}

#' Orders against text markers, before the FAST Act
#'
#' The paper identifies redactions through orders until 2018 and through bracketed markers in the text from 2019.
#' Both indicators exist for every year, so the years in which the order was mandatory are a test of the text
#' rule: among unique contracts of 2008-2018 covered by a granted order, how many also carry at least one marker,
#' and among those that carry a marker, how many are covered by an order. The first share is the one the manuscript
#' cites; the second says how much the text rule finds beyond the orders.
#'
#' The marker definition is the published one: symbol and explicit markers, the two kinds that sit above base rate
#' (see the redaction notes in the project). The threshold is an argument, so the five-marker robustness switch is
#' one call away.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .from,.to Integer. The years of the comparison, 2008 to 2018 by default.
#' @param .min_markers Integer. Markers a contract needs to count as redacted by text; 1 is the paper's rule.
#' @return Tibble: Row, N, Share -- the two-by-two and its two conditional shares.
num_data_cto_markers <- function(.path_contracts, .from = 2008L, .to = 2018L, .min_markers = 1L) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .from        <- 2008L
    .to          <- 2018L
    .min_markers <- 1L
  }
  ds_   <- arrow::open_dataset(sources = .path_contracts)
  need_ <- c("DocID", "DateFiled", "PrimaryFiler", "DescSample", "HasCto", "nRedactSymbol", "nRedactExplicit")
  miss_ <- setdiff(need_, names(ds_))
  if (length(miss_) > 0L) {
    cli::cli_abort("Contracts lacks {.val {miss_}}; the release has {.val {names(ds_)}}.")
  }
  tab_ <- ds_ |>
    dplyr::select(dplyr::all_of(need_)) |>
    dplyr::filter(.data$PrimaryFiler == 1L, .data$DescSample == 1L) |>
    dplyr::collect() |>
    dplyr::mutate(
      Year    = as.integer(format(as.Date(.data$DateFiled), "%Y")),
      Cto     = dplyr::coalesce(as.integer(.data$HasCto), 0L) == 1L,
      Markers = dplyr::coalesce(.data$nRedactSymbol, 0L) + dplyr::coalesce(.data$nRedactExplicit, 0L),
      Text    = .data$Markers >= .min_markers
    ) |>
    dplyr::filter(.data$Year >= .from, .data$Year <= .to)
  n_cto_  <- sum(tab_$Cto)
  n_text_ <- sum(tab_$Text)
  n_both_ <- sum(tab_$Cto & tab_$Text)
  tibble::tibble(
    Row   = c("Unique contracts in the window", "Covered by a granted order", "With a text marker", "Both",
              "Order only", "Marker only", "Neither",
              "Share of ordered contracts with a marker", "Share of marked contracts with an order"),
    N     = c(nrow(tab_), n_cto_, n_text_, n_both_, n_cto_ - n_both_, n_text_ - n_both_,
              nrow(tab_) - n_cto_ - n_text_ + n_both_, NA_integer_, NA_integer_),
    Share = c(rep(NA_real_, 7L),
              if (n_cto_ > 0L) n_both_ / n_cto_ else NA_real_,
              if (n_text_ > 0L) n_both_ / n_text_ else NA_real_)
  )
}

#' Build one of this pair's tibbles, where its inputs or this library are newer than the parquet
#'
#' The layout is the chapters': Data/<Name>.parquet under this pair's output directory, which is what
#' oa_read_exhibit() and the registry read. The build test is the appendix's, oa_build_needed(), so a changed
#' release or a changed builder rebuilds and nothing else does.
#'
#' @param .name Character. The tibble's name, and the file stem.
#' @param .fun Function. The builder; called with no arguments (wrap the paths in a closure).
#' @param .inputs Character. The files the tibble depends on, this library among them.
#' @param .dir_own Character. This pair's output directory.
#' @param .force Logical. TRUE rebuilds regardless.
#' @return Invisibly, "built" or "up to date".
num_build <- function(.name, .fun, .inputs, .dir_own, .force) {
  if (FALSE) {
    .name    <- "CtoAhci"
    .fun     <- \() num_data_cto_ahci(.path_orders = .path_orders)
    .inputs  <- c(.path_orders, .path_lib)
    .dir_own <- here::here("2_output", "31-FinalExhibits", "Output")
    .force   <- FALSE
  }
  out_ <- fs::path(.dir_own, "Data", paste0(.name, ".parquet"))
  if (!oa_build_needed(.outputs = out_, .inputs = .inputs, .force = .force)) return(invisible("up to date"))
  fs::dir_create(fs::path_dir(out_))
  arrow::write_parquet(
    x    = .fun(),
    sink = out_
  )
  invisible("built")
}


# 24. Moved from 40-Numbers: Appendix A, filing requirements and distributions (oaa_) --------------------------------------

#
# 40-OnlineAppendix-A.R -- Appendix A, SEC filing requirements and distributions: the exhibits this chapter builds
#
#
# 40-OnlineAppendix-A.qmd holds the chapter's text; _Commons/_OnlineAppendix.R holds everything the chapters share.
# This library holds only the builders of the exhibits Appendix A writes itself, in 30's layout under this chapter's
# output directory: the coverage table, the within-year figure, the confidential-treatment linkage funnel, and the
# counts the text cites that no table prints. Two exhibits of the response memo are built here too, because their
# data is this chapter's -- the exhibit-files table behind the 2001 start, and the seasoned-filer figure -- and the
# memo pair sets them from here.
#
# THE PREFIX IS oaa_: online appendix, chapter A. Functions are compute (a tibble, no printing), plot (a ggplot) or
# build (writes the exhibit's files where its inputs are newer, returns the status invisibly).


# -- 24.1 The sample, as 30 reads it ---------------------------------------------------------------------------------------

#' The unique contracts of the descriptive sample, as 30 selects them
#'
#' The one reader of the release every build here shares, so all of them count exactly the paper's sample: a
#' contract is kept where DescSample is 1 and it is the primary copy of its attachment -- 30's Keep. The flags are
#' compared after the collect, where an integer and a logical flag compare alike; in Arrow they need not.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .cols Character. The columns to keep besides the flags.
#' @return Tibble: .cols, plus Date and Year, for 2001-2024.
oaa_read_sample <- function(.path_contracts, .cols) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .cols <- c("FormType", "DateFiled")
  }
  arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::all_of(unique(c(.cols, "DateFiled", "PrimaryFiler", "DescSample")))) |>
    dplyr::collect() |>
    dplyr::filter(
      dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      dplyr::coalesce(as.integer(.data$DescSample), 0L) == 1L
    ) |>
    dplyr::mutate(
      Date = as.Date(.data$DateFiled),
      Year = as.integer(format(.data$Date, "%Y"))
    ) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::select(-"PrimaryFiler", -"DescSample")
}

# -- 24.2 Coverage: every form that carries material contracts, and what the database holds of it --------------------------

#' The coverage groups: every EDGAR form type that carries material contracts, and whether the database covers it
#'
#' One row per form type. Panel A lists the forms the database draws on, Panel B the forms outside its scope. The
#' table states what the database and the master index hold: filings for every form, and contracts only for the
#' forms the database downloaded -- the contracts of forms outside the scope were never downloaded, so they are not
#' counted and not estimated. Form 20-F sits in Panel A: the database holds the contracts its filers number as
#' Exhibit 10, while the form's own instructions list material contracts as Exhibit 4, which the note says.
#'
#' @return Tibble: FormType, Panel, Group.
oaa_coverage_forms <- function() {
  a_ <- "In the database"
  b_ <- "Outside it"
  tibble::tribble(
    ~FormType,   ~Panel, ~Group,
    "10-K",      a_,     "Annual reports",
    "10-K/A",    a_,     "Annual reports",
    "10-Q",      a_,     "Quarterly reports",
    "10-Q/A",    a_,     "Quarterly reports",
    "8-K",       a_,     "Current reports",
    "8-K/A",     a_,     "Current reports",
    "S-1",       a_,     "Registration statements",
    "S-1/A",     a_,     "Registration statements",
    "S-4",       a_,     "Registration statements",
    "S-4/A",     a_,     "Registration statements",
    "F-1",       a_,     "Registration statements",
    "F-1/A",     a_,     "Registration statements",
    "F-4",       a_,     "Registration statements",
    "F-4/A",     a_,     "Registration statements",
    "20-F",      a_,     "Foreign annual reports",
    "20-F/A",    a_,     "Foreign annual reports",
    "10QSB",     b_,     "Small business, periodic",
    "10QSB/A",   b_,     "Small business, periodic",
    "10KSB",     b_,     "Small business, periodic",
    "10KSB/A",   b_,     "Small business, periodic",
    "10KSB40",   b_,     "Small business, periodic",
    "10KSB40/A", b_,     "Small business, periodic",
    "SB-2",      b_,     "Small business, registration",
    "SB-2/A",    b_,     "Small business, registration",
    "SB-1",      b_,     "Small business, registration",
    "SB-1/A",    b_,     "Small business, registration",
    "10SB12G",   b_,     "Small business, registration",
    "10SB12G/A", b_,     "Small business, registration",
    "10SB12B",   b_,     "Small business, registration",
    "10SB12B/A", b_,     "Small business, registration",
    "10-K405",   b_,     "10-K variants",
    "10-K405/A", b_,     "10-K variants",
    "10KT405",   b_,     "10-K variants",
    "10-KT",     b_,     "10-K variants",
    "10-KT/A",   b_,     "10-K variants",
    "10-12G",    b_,     "Exchange Act registrations",
    "10-12G/A",  b_,     "Exchange Act registrations",
    "10-12B",    b_,     "Exchange Act registrations",
    "10-12B/A",  b_,     "Exchange Act registrations",
    "S-11",      b_,     "Real-estate registrations",
    "S-11/A",    b_,     "Real-estate registrations"
  )
}

#' The coverage table's data: filings per group, and the contracts the database holds
#'
#' @param .dir_master Character. The master index's parquet directory.
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Panel, Group, Forms, Years, Filings, Contracts, Basis, Kind (group, total or design); one row per
#'   group, a total per panel, and one row for the forms out by design. Contracts is missing outside the database.
oaa_data_coverage <- function(.dir_master, .path_contracts) {
  if (FALSE) {
    .dir_master <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  forms_ <- oaa_coverage_forms()
  # FILINGS per form type, 2001-2024, from the master index
  fil_ <- arrow::open_dataset(sources = .dir_master) |>
    dplyr::filter(.data$FormType %in% forms_$FormType) |>
    dplyr::select("FormType", "DateFiled") |>
    dplyr::collect() |>
    dplyr::mutate(Year = as.integer(format(as.Date(.data$DateFiled), "%Y"))) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::summarise(Filings = dplyr::n(), First = min(.data$Year), Last = max(.data$Year), .by = "FormType")
  # CONTRACTS the database holds: unique contracts of the descriptive sample, per form type
  held_ <- oaa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = "FormType"
  ) |>
    dplyr::count(.data$FormType, name = "Held")
  # THE FORMS OF A GROUP, in the order the list above gives them, amendments folded into one mention
  label_ <- function(.f) {
    orig_ <- .f[!grepl("/A$", .f)]
    paste0(paste(orig_, collapse = ", "), if (any(grepl("/A$", .f))) " (with /A)" else "")
  }
  out_ <- forms_ |>
    dplyr::left_join(fil_, by = "FormType") |>
    dplyr::left_join(held_, by = "FormType") |>
    dplyr::mutate(Filings = dplyr::coalesce(.data$Filings, 0L)) |>
    dplyr::summarise(
      Forms     = label_(.f = .data$FormType),
      Years     = paste0(min(.data$First, na.rm = TRUE), "-", max(.data$Last, na.rm = TRUE)),
      Filings   = sum(.data$Filings),
      Contracts = if (dplyr::first(.data$Panel) == "In the database") sum(.data$Held, na.rm = TRUE) else NA_real_,
      .by       = c("Panel", "Group")
    ) |>
    dplyr::mutate(
      Contracts = as.numeric(.data$Contracts),
      Basis     = dplyr::case_when(
        .data$Group == "Foreign annual reports" ~ "The release; Exhibit 10 only",
        .data$Panel == "In the database"        ~ "The release",
        .default                                = "Not downloaded"
      ),
      Kind = "group"
    )
  totals_ <- out_ |>
    dplyr::summarise(Filings = sum(.data$Filings), Contracts = sum(.data$Contracts), .by = "Panel") |>
    dplyr::mutate(Group = "Total", Forms = "", Years = "", Basis = "", Kind = "total")
  design_ <- tibble::tibble(
    Panel = "Out by design", Group = "Asset-backed issuers; foreign current reports",
    Forms = "SF-1, SF-3, 10-D; 6-K", Years = "", Filings = NA_integer_, Contracts = NA_real_,
    Basis = "Not operating firms; no exhibit numbering", Kind = "design"
  )
  dplyr::bind_rows(
    dplyr::filter(out_, .data$Panel == "In the database"),
    dplyr::filter(totals_, .data$Panel == "In the database"),
    dplyr::filter(out_, .data$Panel == "Outside it"),
    dplyr::filter(totals_, .data$Panel == "Outside it"),
    design_
  )
}

#' Build the coverage table
#'
#' @param .dir_master Character. The master index's parquet directory.
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_coverage <- function(.dir_master, .path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_master <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "FormCoverage"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.dir_master, .path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_coverage(
    .dir_master     = .dir_master,
    .path_contracts = .path_contracts
  )
  num_ <- function(.x) dplyr::if_else(is.na(.x), "", format(.x, big.mark = ",", trim = TRUE, scientific = FALSE))
  cells_ <- tab_ |>
    dplyr::transmute(
      Panel     = .data$Panel,
      Group     = oa_tex_escape(.x = .data$Group),
      Forms     = oa_tex_escape(.x = .data$Forms),
      Years     = gsub("-", "--", .data$Years, fixed = TRUE),
      Filings   = num_(.x = .data$Filings),
      Contracts = dplyr::if_else(.data$Panel == "Outside it", "--", num_(.x = .data$Contracts)),
      Basis     = oa_tex_escape(.x = .data$Basis)
    )
  tot_ <- tab_$Kind == "total"
  cells_[tot_, c("Group", "Filings", "Contracts")] <- lapply(cells_[tot_, c("Group", "Filings", "Contracts")],
                                                             \(.x) paste0("\\textbf{", .x, "}"))
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("Group", "EDGAR form types", "Years", "Filings", "Contracts", "Basis"),
      .spec   = c(oa_col_text(.share = 0.19), oa_col_text(.share = 0.21), "l",
                  oa_col_num(.mm = 16), oa_col_num(.mm = 16), oa_col_text(.share = 0.15))
    ),
    .note    = paste(
      "Filings are counted in EDGAR's master index, 2001-2024; contracts are the unique contracts of the descriptive",
      "sample in the release. The forms outside the database were not downloaded, so their contracts are not",
      "counted. Small business issuers reported under Regulation S-B, whose Item 601 required material contracts",
      "as Exhibit 10 in the same way as Regulation S-K; the SB forms were phased out after SEC Release 33-8876 took",
      "effect in February 2008. Form 20-F lists material contracts as Exhibit 4 (Instruction 4(a) of its",
      "Instructions as to Exhibits); the database holds the 20-F contracts that filers number as Exhibit 10."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

# -- 24.3 Within the year: when contracts are filed ------------------------------------------------------------------------

#' The within-year figure's data: the share of each report type's contracts filed on each calendar day
#'
#' Unique contracts of the descriptive sample (30's Keep), 2001-2024. For every report type and year, the share of
#' that year's contracts filed on each day; the figure shows the mean over years, so every year counts equally.
#' February 29 is folded into February 28, so all years share one calendar; days without a filing count as zero.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Group, Day (a date of 2025, standing for the calendar day), Share (percent).
oaa_data_within_year <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  groups_ <- tibble::tribble(
    ~FormType, ~Group,
    "8-K",     "Current reports (8-K)",
    "8-K/A",   "Current reports (8-K)",
    "10-K",    "Annual reports (10-K)",
    "10-K/A",  "Annual reports (10-K)",
    "10-Q",    "Quarterly reports (10-Q)",
    "10-Q/A",  "Quarterly reports (10-Q)"
  )
  con_ <- oaa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = "FormType"
  ) |>
    dplyr::inner_join(groups_, by = "FormType") |>
    dplyr::mutate(MD = sub("^02-29$", "02-28", format(.data$Date, "%m-%d")))
  days_ <- format(seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"), "%m-%d")
  grid_ <- tidyr::expand_grid(Group = unique(groups_$Group), Year = 2001L:2024L, MD = days_)
  con_ |>
    dplyr::count(.data$Group, .data$Year, .data$MD, name = "N") |>
    dplyr::right_join(grid_, by = c("Group", "Year", "MD")) |>
    dplyr::mutate(N = dplyr::coalesce(.data$N, 0L)) |>
    dplyr::mutate(Share = 100 * .data$N / sum(.data$N), .by = c("Group", "Year")) |>
    dplyr::filter(!is.na(.data$Share)) |>
    dplyr::summarise(Share = mean(.data$Share), .by = c("Group", "MD")) |>
    dplyr::mutate(
      Day   = as.Date(paste0("2025-", .data$MD)),
      Group = factor(.data$Group, levels = unique(groups_$Group))
    ) |>
    dplyr::select("Group", "Day", "Share") |>
    dplyr::arrange(.data$Group, .data$Day)
}

#' The within-year figure: one panel per report type, one scale, the filing windows shaded
#'
#' @param .tab Tibble from oaa_data_within_year().
#' @return A ggplot.
oaa_plot_within_year <- function(.tab) {
  if (FALSE) .tab <- oaa_data_within_year(.path_contracts = "Contracts.parquet")
  win_ <- tibble::tibble(
    Start = as.Date(c("2025-01-01", "2025-04-01", "2025-07-01", "2025-10-01")),
    End   = as.Date(c("2025-03-31", "2025-05-15", "2025-08-14", "2025-11-14")),
    Label = c("10-K window", "10-Q window", "10-Q window", "10-Q window"),
    Group = factor(levels(.tab$Group)[1L], levels = levels(.tab$Group))
  )
  top_ <- max(.tab$Share) * 1.08
  ggplot2::ggplot(.tab, ggplot2::aes(x = .data$Day, y = .data$Share)) +
    ggplot2::geom_rect(
      data        = dplyr::select(win_, -"Group"),
      mapping     = ggplot2::aes(xmin = .data$Start, xmax = .data$End, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill        = "#EDEDED"
    ) +
    ggplot2::geom_text(
      data        = win_,
      mapping     = ggplot2::aes(x = .data$Start + (.data$End - .data$Start) / 2, y = top_, label = .data$Label),
      inherit.aes = FALSE,
      size        = 3.1,
      colour      = "#595959",
      family      = .plot_font,
      vjust       = 1
    ) +
    ggplot2::geom_line(colour = plot_pal_cat(.n = 1L), linewidth = 0.35) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Group), ncol = 1L) +
    ggplot2::scale_x_date(
      breaks = seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month"),
      labels = \(.d) format(.d, "%b"),
      expand = c(0.005, 0.005)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, top_), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Share of the year's contracts filed on the day, in %") +
    plot_theme(.grid = "y", .legend = "none") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

#' Build the within-year figure
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_within_year <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own        <- here::here("2_output", "31-FinalExhibits", "Output")
    .force          <- FALSE
    .path_lib       <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "FilingsWithinYear"
  outs_ <- c(fs::path(.dir_own, "Figures", paste0(name_, ".png")),
             fs::path(.dir_own, c("Notes", "Data"), paste0(name_, c(".tex", ".parquet"))))
  ins_  <- c(.path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_within_year(.path_contracts = .path_contracts)
  oa_write_exhibit(
    .name    = name_,
    .lines   = NULL,
    .note    = paste(
      "Unique contracts, 2001-2024, filed with current reports (8-K), annual reports (10-K) and quarterly reports",
      "(10-Q), each with its amendments. For each report type and year, the share of that year's contracts filed on",
      "each calendar day; the lines show the mean over years. Shaded are the filing windows of December year-end",
      "filers: 90 days after the fiscal year's end for the 10-K and 45 days after each quarter's end for the 10-Q.",
      "Within them, the deadlines of 60, 75 and 90 days (10-K) and 40 and 45 days (10-Q) differ by filer status."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  plot_save(
    .plot    = oaa_plot_within_year(.tab = tab_),
    .name    = name_,
    .dir     = fs::path(.dir_own, "Figures"),
    .height  = 5.2,
    .formats = "png"            # no pdf (decided 17 September)
  )
  invisible("built")
}

# -- 24.4 Confidential treatment orders: from the order to the contract it covers ------------------------------------------

#' The linkage funnel's data: every reference an order makes, by where it ends
#'
#' The release's CtoOrders.parquet holds one row per reference an order makes to an exhibit, and LinkStatus records
#' where each reference ended: linked to the attachment it names, or failed at one of nine named points in the chain
#' from the order's text to the database. The codes are numbered in chain order, which is the order the table keeps.
#' A linked reference is then classified by what the order does -- grants, extends, denies, revokes -- because the
#' paper's redaction measure counts grants and their extensions and not denials. The last row is the release's own
#' flag on the unique contracts of the sample, so the funnel ends where the paper's variable begins.
#'
#' @param .path_orders Character. The release's CtoOrders.parquet.
#' @param .path_contracts Character. The release's Contracts.parquet, for the last row.
#' @return Tibble: Panel, Step, Code, N, Share (of references), Kind (total / item / result).
oaa_data_cto <- function(.path_orders, .path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_orders <- fs::path(fs::path_dir(.path_contracts), "CtoOrders.parquet")
  }
  ref_ <- arrow::open_dataset(sources = .path_orders) |>
    dplyr::select(dplyr::any_of(c("OrderDocID", "LinkStatus", "Status", "IsExtension"))) |>
    dplyr::collect()
  n_ref_ <- nrow(ref_)
  # THE FAILURE POINTS, in chain order, in words. The code is the release's own; the words are the table's.
  steps_ <- tibble::tribble(
    ~Code,       ~Step,
    "A-linked",  "Linked to the attachment it names",
    "1-",        "No exhibit reference could be parsed from the order",
    "2-",        "The reference is not to an Exhibit 10",
    "3-",        "The order does not name the filing the exhibit came with",
    "4-",        "The named filing is not in EDGAR's index",
    "5-",        "The named filing matches several filings",
    "6-",        "No exhibit of the named filing was downloaded (a form outside the frame)",
    "7-",        "The exhibit number matches several attachments of the filing",
    "8-",        "The exhibit number is lettered and cannot be matched",
    "9-",        "The exhibit number is not among the attachments downloaded"
  )
  code_ <- dplyr::case_when(
    ref_$LinkStatus == "A-linked" ~ "A-linked",
    .default = paste0(stringi::stri_sub(ref_$LinkStatus, 1L, 1L), "-")
  )
  unknown_ <- setdiff(unique(code_), steps_$Code)
  if (length(unknown_) > 0L) cli::cli_abort("LinkStatus code{?s} the funnel does not name: {.val {unknown_}}.")
  by_code_ <- tibble::tibble(Code = code_) |>
    dplyr::count(.data$Code, name = "N")
  panel_a_ <- steps_ |>
    dplyr::left_join(by_code_, by = "Code") |>
    dplyr::mutate(N = dplyr::coalesce(.data$N, 0L), Kind = "item")
  linked_ <- ref_[ref_$LinkStatus == "A-linked", , drop = FALSE]
  ext_    <- dplyr::coalesce(as.integer(linked_$IsExtension), 0L) == 1L
  st_     <- linked_$Status
  panel_b_ <- tibble::tibble(
    Code = c("", "", "", "", ""),
    Step = c("Grants confidential treatment", "Extends an earlier grant", "Denies the request, in whole or in part",
             "Revokes an earlier grant", "Order text not parsed, status unknown"),
    N    = c(
      sum(st_ == "GRANTING" & !ext_, na.rm = TRUE),
      sum(st_ == "GRANTING" & ext_, na.rm = TRUE),
      sum(st_ == "DENYING", na.rm = TRUE),
      sum(st_ == "REVOKING", na.rm = TRUE),
      sum(is.na(st_))
    ),
    Kind = "item"
  )
  # THE LAST ROW: the release's flag, on the unique contracts of the descriptive sample
  con_ <- oaa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = "HasCto"
  )
  n_cto_ <- sum(dplyr::coalesce(as.integer(con_$HasCto), 0L) == 1L)
  dplyr::bind_rows(
    tibble::tibble(Panel = "A. References, by where they end", Code = "", Step = "Orders in the release",
                   N = dplyr::n_distinct(ref_$OrderDocID), Kind = "total"),
    tibble::tibble(Panel = "A. References, by where they end", Code = "", Step = "References to exhibits they make",
                   N = n_ref_, Kind = "total"),
    dplyr::mutate(panel_a_, Panel = "A. References, by where they end"),
    tibble::tibble(Panel = "B. Linked references, by what the order does", Code = "", Step = "Linked references",
                   N = nrow(linked_), Kind = "total"),
    dplyr::mutate(panel_b_, Panel = "B. Linked references, by what the order does"),
    tibble::tibble(Panel = "C. In the sample", Code = "",
                   Step = "Unique contracts of the descriptive sample with a grant or an extension",
                   N = n_cto_, Kind = "result")
  ) |>
    dplyr::mutate(Share = dplyr::if_else(.data$Panel == "C. In the sample", NA_real_, .data$N / n_ref_)) |>
    dplyr::select("Panel", "Step", "Code", "N", "Share", "Kind")
}

#' Build the linkage funnel
#'
#' @param .path_orders Character. The release's CtoOrders.parquet.
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_cto <- function(.path_orders, .path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_orders <- fs::path(fs::path_dir(.path_contracts), "CtoOrders.parquet")
    .dir_own     <- here::here("2_output", "31-FinalExhibits", "Output")
    .force       <- FALSE
    .path_lib    <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "CtoLinkage"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.path_orders, .path_contracts, .path_lib)
  if (!fs::file_exists(.path_orders)) return(invisible("waiting for CtoOrders.parquet in the release"))
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_cto(
    .path_orders    = .path_orders,
    .path_contracts = .path_contracts
  )
  num_ <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)
  pct_ <- function(.x) dplyr::if_else(is.na(.x), "", formatC(100 * .x, format = "f", digits = 1L))
  cells_ <- tab_ |>
    dplyr::transmute(
      Panel = .data$Panel,
      Step  = dplyr::if_else(.data$Kind == "item", paste0("\\hspace{1em}", oa_tex_escape(.x = .data$Step)),
                             oa_tex_escape(.x = .data$Step)),
      N     = num_(.x = .data$N),
      Share = pct_(.x = .data$Share)
    )
  tot_ <- tab_$Kind %in% c("total", "result")
  cells_[tot_, c("Step", "N")] <- lapply(cells_[tot_, c("Step", "N")], \(.x) paste0("\\textbf{", .x, "}"))
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("", "N", "\\% of references"),
      .spec   = c(oa_col_text(.share = 0.66), oa_col_num(.mm = 18), oa_col_num(.mm = 22))
    ),
    .note    = paste(
      "Every confidential treatment order EDGAR lists (form type CT ORDER, from May 2008) is parsed for the exhibits",
      "it covers; an order identifies an exhibit by the filing it came with and its exhibit number, and each such",
      "reference is followed to the attachment in the database. Panel A counts references by where they end, in",
      "the order the chain is followed. Panel B classifies the linked references by what the order does. Panel C",
      "counts the unique contracts of the descriptive sample the release flags as covered by a grant or an",
      "extension of a grant; denials and revocations are not counted, following Ahci (2025). Shares are of all",
      "references."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# -- 24.5 The counts the text cites, which no table prints -----------------------------------------------------------------

#' The counts the text of Appendix A cites, which no table prints
#'
#' Passages of the text name numbers that belong to no exhibit: how many rows the release holds and how many of
#' them are unique attachments, how many of those the sample keeps, how many predate 2001, and what each quality
#' rule flags. They are computed here on the release, written as a tibble like any other exhibit's data, and read by
#' the numbers spec; nothing is printed.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Key, Value.
oaa_data_counts <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  all_ <- arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::any_of(c("DocID", "DateFiled", "PrimaryFiler", "DescSample", "SampleStepCode", "Removed",
                                  "RemClass", "nWords"))) |>
    dplyr::collect() |>
    dplyr::mutate(
      Primary = dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      Sample  = dplyr::coalesce(as.integer(.data$DescSample), 0L) == 1L,
      Flagged = dplyr::coalesce(as.integer(.data$Removed), 0L) == 1L,
      Year    = as.integer(format(as.Date(.data$DateFiled), "%Y"))
    )
  uni_ <- dplyr::filter(all_, .data$Primary)
  # THE RULES: RemClass names the rule that flagged a document, prefixed 1-, 2- or 3-; a flagged document without a
  # rule is a placeholder that carries no text (the PDF and image references folded into the malformatted rung).
  rule_ <- uni_ |>
    dplyr::filter(.data$Flagged, !is.na(.data$RemClass)) |>
    dplyr::count(.data$RemClass, name = "N")
  pick_ <- function(.prefix) {
    row_ <- rule_[startsWith(rule_$RemClass, .prefix), , drop = FALSE]
    if (nrow(row_) == 0L) return(0)
    sum(row_$N)
  }
  n_flag_ <- sum(uni_$Flagged)
  n_rule_ <- pick_(.prefix = "1-") + pick_(.prefix = "2-") + pick_(.prefix = "3-")
  tibble::tibble(
    Key = c("DocsAll", "DocsUnique", "DocsSample", "DocsPre2001", "DocsFlagged", "RuleShort", "RuleStopwords",
            "RuleNumeric", "Placeholders", "YearFirst", "YearLast"),
    Value = c(
      nrow(all_),
      nrow(uni_),
      sum(uni_$Sample),
      sum(uni_$Year < 2001L, na.rm = TRUE),
      n_flag_,
      pick_(.prefix = "1-"),
      pick_(.prefix = "2-"),
      pick_(.prefix = "3-"),
      max(n_flag_ - n_rule_, 0),
      min(uni_$Year[uni_$Sample], na.rm = TRUE),
      max(uni_$Year[uni_$Sample], na.rm = TRUE)
    )
  )
}

#' Build the counts the text cites
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_counts <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "TextCounts"
  outs_ <- fs::path(.dir_own, "Data", paste0(name_, ".parquet"))
  ins_  <- c(.path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  fs::dir_create(fs::path(.dir_own, "Data"))
  arrow::write_parquet(
    x    = oaa_data_counts(.path_contracts = .path_contracts),
    sink = outs_
  )
  invisible("built")
}


# -- 24.6 For the response memo: the exhibit files behind the 2001 start, and the seasoned filers --------------------------
# Built here because their data is this chapter's; registered under Section "M", so the runbook shows them and the
# fragment leaves them out. The memo pair sets them from this chapter's output directory.

#' The exhibit-files table's data: per quarter, the Exhibit 10s EDGAR's index lists, and those that are files
#'
#' EDGAR's index page lists the documents of a filing back to 1993, read from the tags of the complete submission,
#' but only a filing submitted through the modernised system disseminates each document as a file of its own. In
#' 01B's links table the difference is one column: a listed exhibit that is a file carries a Document name, and one
#' that is not carries NA and an address ending at the filer's folder. The table counts both, by quarter, across the
#' years in which the change happened.
#'
#' @param .dir_links Character. 01A's mirrored links table, the directory of one parquet per quarter.
#' @param .from Numeric. The first year-quarter, as 01A writes it (1998.1).
#' @param .to Numeric. The last year-quarter, inclusive.
#' @return Tibble: YearQuarter, Quarter (as text), Listed, WithFile, ShareFile.
oaa_data_files <- function(.dir_links, .from, .to) {
  if (FALSE) {
    .dir_links <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$DocLinks$DirMain$Links
    .from <- 1998.1
    .to   <- 2001.2
  }
  arrow::open_dataset(sources = .dir_links) |>
    dplyr::filter(.data$YearQuarter >= .from, .data$YearQuarter <= .to) |>
    dplyr::select("YearQuarter", "Type", "Document") |>
    dplyr::collect() |>
    dplyr::filter(grepl("^EX-?10", toupper(.data$Type))) |>
    dplyr::summarise(
      Listed   = dplyr::n(),
      WithFile = sum(!is.na(.data$Document)),
      .by      = "YearQuarter"
    ) |>
    dplyr::mutate(
      ShareFile = .data$WithFile / .data$Listed,
      Quarter   = paste0(floor(.data$YearQuarter), " Q", round(10 * (.data$YearQuarter - floor(.data$YearQuarter))))
    ) |>
    dplyr::arrange(.data$YearQuarter) |>
    dplyr::select("YearQuarter", "Quarter", "Listed", "WithFile", "ShareFile")
}

#' Build the exhibit-files table
#'
#' @param .dir_links Character. 01A's mirrored links table.
#' @param .from Numeric. The first year-quarter.
#' @param .to Numeric. The last year-quarter.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_files <- function(.dir_links, .from, .to, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_links <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$DocLinks$DirMain$Links
    .from     <- 1998.1
    .to       <- 2001.2
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "ExhibitFiles"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.dir_links, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_files(
    .dir_links = .dir_links,
    .from      = .from,
    .to        = .to
  )
  num_ <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)
  cells_ <- tab_ |>
    dplyr::transmute(
      Quarter   = .data$Quarter,
      Listed    = num_(.x = .data$Listed),
      WithFile  = num_(.x = .data$WithFile),
      ShareFile = formatC(100 * .data$ShareFile, format = "f", digits = 1L)
    )
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("Quarter", "Exhibit 10s listed", "Of which files", "\\%"),
      .spec   = c("l", oa_col_num(.mm = 24), oa_col_num(.mm = 22), oa_col_num(.mm = 14))
    ),
    .note    = paste(
      "From the document lists of EDGAR's filing index pages, mirrored for every filing of the eight forms the",
      "database covers, 1998 Q1 to 2001 Q2. Listed counts the attachments whose exhibit type begins EX-10; of which",
      "files counts those that carry a file name and address of their own on the index page. An exhibit listed",
      "without a file exists only inside the filing's complete submission text file."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' The seasoned-filer figure's data: four groups of filers, per year
#'
#' All filers; seasoned filers by 30's rule -- more than two years (2 x 365.25 days) past the CIK's first contract in
#' the sample, which leaves firms already filing in 2001 unseasoned until 2003; seasoned filers by the CIK's first
#' EDGAR filing of any form, from the master index, which starts in 1993 and does not date a firm by the filings
#' under study; and the contracts of blank-check registrants, the filings whose SIC code is 6770, from 01A's landing
#' pages. For each group and year: contracts, those filed with a registration statement (S-1, S-4, F-1, F-4 and
#' their amendments), and the equity contracts. The label is the release's Class, the crowned engine's detailed
#' label under a name that does not depend on the engine, which is also what 30 reads.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .path_landing Character. 01A's LandingPageAll.parquet, for the SIC code of each filing.
#' @param .dir_master Character. The master index's parquet directory.
#' @return Tibble: Year, Filers, N, NReg, ShareReg, NEquity, ShareEquity.
oaa_data_seasoned <- function(.path_contracts, .path_landing, .dir_master) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_landing <- here::here("2_output", "01A-EdgarIndex", "Output", "LandingPageAll.parquet")
    .dir_master   <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
  }
  pad_ <- function(.x) sprintf("%010.0f", as.numeric(.x))
  con_ <- oaa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = c("CIK", "HashIndex", "FormType", "Class")
  ) |>
    dplyr::mutate(
      CIK      = pad_(.x = .data$CIK),
      IsReg    = sub("/A$", "", .data$FormType) %in% c("S-1", "S-4", "F-1", "F-4"),
      IsEquity = dplyr::coalesce(.data$Class == "Financial Instruments: Equity", FALSE)
    )
  if (!any(con_$IsEquity)) {
    cli::cli_abort("No contract's {.field Class} is {.val Financial Instruments: Equity}; the release's labels differ.")
  }
  # 30'S RULE: the first contract of the CIK in the sample
  con_ <- con_ |>
    dplyr::mutate(Entry = min(.data$Date), .by = "CIK") |>
    dplyr::mutate(Seasoned = as.numeric(.data$Date - .data$Entry) / 365.25 > 2)
  # THE CHECK: the first EDGAR filing of the CIK, any form, from the master index
  first_ <- arrow::open_dataset(sources = .dir_master) |>
    dplyr::select("CIK", "DateFiled") |>
    dplyr::mutate(Date = as.Date(.data$DateFiled)) |>
    dplyr::group_by(.data$CIK) |>
    dplyr::summarise(First = min(.data$Date, na.rm = TRUE)) |>
    dplyr::collect() |>
    dplyr::mutate(CIK = pad_(.x = .data$CIK)) |>
    dplyr::summarise(First = min(.data$First), .by = "CIK")
  con_ <- con_ |>
    dplyr::left_join(first_, by = "CIK") |>
    dplyr::mutate(SeasonedEdgar = dplyr::coalesce(as.numeric(.data$Date - .data$First) / 365.25 > 2, FALSE))
  # BLANK-CHECK REGISTRANTS: the filing's SIC code
  sic_ <- tibble::as_tibble(arrow::read_parquet(file = .path_landing, col_select = c("HashIndex", "SIC"))) |>
    dplyr::distinct(.data$HashIndex, .keep_all = TRUE)
  con_ <- con_ |>
    dplyr::left_join(sic_, by = "HashIndex") |>
    dplyr::mutate(BlankCheck = dplyr::coalesce(trimws(as.character(.data$SIC)) == "6770", FALSE))
  one_ <- function(.rows, .label) {
    .rows |>
      dplyr::summarise(
        N       = dplyr::n(),
        NReg    = sum(.data$IsReg),
        NEquity = sum(.data$IsEquity),
        .by     = "Year"
      ) |>
      tidyr::complete(Year = 2001L:2024L, fill = list(N = 0L, NReg = 0L, NEquity = 0L)) |>
      dplyr::mutate(Filers = .label)
  }
  dplyr::bind_rows(
    one_(.rows = con_,                                   .label = "All filers"),
    one_(.rows = dplyr::filter(con_, .data$Seasoned),      .label = "Seasoned filers"),
    one_(.rows = dplyr::filter(con_, .data$SeasonedEdgar), .label = "Seasoned, by first EDGAR filing"),
    one_(.rows = dplyr::filter(con_, .data$BlankCheck),    .label = "Blank-check registrants (SIC 6770)")
  ) |>
    dplyr::mutate(
      ShareReg    = dplyr::if_else(.data$N > 0L, .data$NReg / .data$N, NA_real_),
      ShareEquity = dplyr::if_else(.data$N > 0L, .data$NEquity / .data$N, NA_real_)
    ) |>
    dplyr::select("Year", "Filers", "N", "NReg", "ShareReg", "NEquity", "ShareEquity") |>
    dplyr::arrange(.data$Filers, .data$Year)
}

#' The seasoned-filer figure: contracts, registration share and equity share, all filers against seasoned filers
#'
#' @param .tab Tibble from oaa_data_seasoned().
#' @return A ggplot.
oaa_plot_seasoned <- function(.tab) {
  if (FALSE) .tab <- oaa_data_seasoned(.path_contracts = "C", .path_landing = "L", .dir_master = "M")
  lv_ <- c("All filers", "Seasoned filers", "Seasoned, by first EDGAR filing", "Blank-check registrants (SIC 6770)")
  pa_ <- c("A. Contracts per year, in 1,000s", "B. Filed with a registration statement, in %",
           "C. Equity contracts, in %")
  two_ <- c("All filers", "Seasoned filers")
  dat_ <- dplyr::bind_rows(
    dplyr::transmute(.tab, .data$Year, .data$Filers, Value = .data$N / 1000, Panel = pa_[1L]),
    dplyr::transmute(dplyr::filter(.tab, .data$Filers %in% two_), .data$Year, .data$Filers,
                     Value = 100 * .data$ShareReg, Panel = pa_[2L]),
    dplyr::transmute(dplyr::filter(.tab, .data$Filers %in% two_), .data$Year, .data$Filers,
                     Value = 100 * .data$ShareEquity, Panel = pa_[3L])
  ) |>
    dplyr::filter(!is.na(.data$Value)) |>
    dplyr::mutate(
      Filers = factor(.data$Filers, levels = lv_),
      Panel  = factor(.data$Panel, levels = pa_)
    )
  cat_ <- plot_pal_cat(.n = 3L)
  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$Value, colour = .data$Filers,
                                     linetype = .data$Filers)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 0.9) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Panel), ncol = 1L, scales = "free_y") +
    ggplot2::scale_colour_manual(values = stats::setNames(cat_[c(1L, 2L, 2L, 3L)], lv_), drop = FALSE) +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "solid", "dashed", "solid"), lv_), drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq(2002L, 2024L, 2L), expand = c(0.01, 0.01)) +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL, linetype = NULL) +
    plot_theme(.grid = "y", .legend = "bottom") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0)) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2L), linetype = ggplot2::guide_legend(nrow = 2L))
}

#' Build the seasoned-filer figure
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .path_landing Character. 01A's LandingPageAll.parquet.
#' @param .dir_master Character. The master index's parquet directory.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_seasoned <- function(.path_contracts, .path_landing, .dir_master, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_landing <- here::here("2_output", "01A-EdgarIndex", "Output", "LandingPageAll.parquet")
    .dir_master   <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
    .dir_own      <- here::here("2_output", "31-FinalExhibits", "Output")
    .force        <- FALSE
    .path_lib     <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "SeasonedFilers"
  outs_ <- c(fs::path(.dir_own, "Figures", paste0(name_, ".png")),
             fs::path(.dir_own, c("Notes", "Data"), paste0(name_, c(".tex", ".parquet"))))
  ins_  <- c(.path_contracts, .path_landing, .dir_master, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_seasoned(
    .path_contracts = .path_contracts,
    .path_landing   = .path_landing,
    .dir_master     = .dir_master
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = NULL,
    .note    = paste(
      "Unique contracts of the descriptive sample, 2001-2024. A seasoned filer is one more than two years past its",
      "first contract in the sample, so that firms already filing in 2001 count as seasoned from 2003; the dashed",
      "line dates a filer by its first EDGAR filing of any form instead, from EDGAR's master index, which starts in",
      "1993. Blank-check registrants are the filings under SIC code 6770. Registration statements are Forms S-1,",
      "S-4, F-1 and F-4 with their amendments; equity contracts are those classified as Financial Instruments:",
      "Equity."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  plot_save(
    .plot    = oaa_plot_seasoned(.tab = tab_),
    .name    = name_,
    .dir     = fs::path(.dir_own, "Figures"),
    .height  = 7.2,
    .formats = "png"            # no pdf (decided 17 September)
  )
  invisible("built")
}


# -- 24.7 The exhibit table: row (10) of 17 C.F.R. 229.601(a), with what the database covers -------------------------------

#' Row (10) of the exhibit table, one line per form, with the database's coverage beside it
#'
#' The exhibit table in Item 601(a) lists, for every form, which exhibits must be filed with it; row (10) is the
#' material contracts. The content is the regulation's and is kept here as a tibble so that the table is an exhibit
#' like any other. Required is the X of the table; Covered is whether the database downloads Exhibit 10 from the
#' form. Read from the eCFR on 14 Sep 2026; the two footnotes of the table (S-4 and F-4, 8-K) are stated in the note.
#'
#' @return Tibble: Form, Act, Required, Covered, Note.
oaa_data_exhibit_table <- function() {
  tibble::tribble(
    ~Form,    ~Act,             ~Required, ~Covered, ~Note,
    "S-1",    "Securities Act", TRUE,      TRUE,     "",
    "S-3",    "Securities Act", FALSE,     FALSE,    "",
    "SF-1",   "Securities Act", TRUE,      FALSE,    "asset-backed issuers",
    "SF-3",   "Securities Act", TRUE,      FALSE,    "asset-backed issuers",
    "S-4",    "Securities Act", TRUE,      TRUE,     "footnote 1 of the table",
    "S-8",    "Securities Act", FALSE,     FALSE,    "",
    "S-11",   "Securities Act", TRUE,      FALSE,    "real-estate companies",
    "F-1",    "Securities Act", TRUE,      TRUE,     "",
    "F-3",    "Securities Act", FALSE,     FALSE,    "",
    "F-4",    "Securities Act", TRUE,      TRUE,     "footnote 1 of the table",
    "10",     "Exchange Act",   TRUE,      FALSE,    "registration under the Exchange Act",
    "8-K",    "Exchange Act",   FALSE,     TRUE,     "footnote 2 of the table; Item 1.01 announces the contract",
    "10-D",   "Exchange Act",   TRUE,      FALSE,    "asset-backed issuers",
    "10-Q",   "Exchange Act",   TRUE,      TRUE,     "",
    "10-K",   "Exchange Act",   TRUE,      TRUE,     "",
    "ABS-EE", "Exchange Act",   FALSE,     FALSE,    "asset-backed issuers"
  )
}

#' Build the exhibit table
#'
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library, the table's only input.
#' @return Invisibly, the build's status.
oaa_build_exhibit_table <- function(.dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "ExhibitRequired"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = .path_lib, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_exhibit_table()
  cells_ <- tibble::tibble(
    Panel    = paste(tab_$Act, "forms"),
    Form     = oa_tex_escape(.x = tab_$Form),
    Required = dplyr::if_else(tab_$Required, "X", "--"),
    Covered  = dplyr::if_else(tab_$Covered, "X", "--"),
    Note     = dplyr::if_else(nzchar(tab_$Note), oa_tex_escape(.x = tab_$Note), "--")
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("Form", "Exhibit 10 required", "In the database", "Note"),
      .spec   = c(oa_col_text(.share = 0.12), oa_col_num(.mm = 30), oa_col_num(.mm = 26), oa_col_text(.share = 0.40))
    ),
    .note    = paste(
      "Row (10), material contracts, of the exhibit table in Item 601(a) of Regulation S-K (17 C.F.R. 229.601(a)),",
      "read on September 14, 2026: an X marks a form with which a material contract must be filed as Exhibit 10.",
      "The table's footnote 1 exempts a company from providing the exhibit on Form S-4 or F-4 where it has elected",
      "to provide information at the level of Form S-3 or F-3 and that form would not require it; footnote 2 limits",
      "Form 8-K exhibits to those relevant to the subject matter of the report. In the database marks the forms from",
      "which the database downloads Exhibit 10; the 8-K is covered although the table does not require the contract",
      "there, because filers attach it to the announcement under Item 1.01 (Section A.1)."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# -- 24.8 What the forms are: the CFR sections that establish them ---------------------------------------------------------

#' The sixteen forms of the exhibit table, each with the CFR section that establishes it and its purpose
#'
#' Securities Act forms are established in 17 C.F.R. Part 239 and Exchange Act forms in Part 249; each section's
#' heading states what the form is for, and that heading is the description printed. Kept here as a tibble so that
#' the table is an exhibit like any other; the runbook's source register links every section.
#'
#' @return Tibble: Form, Act, Section, Description, Url.
oaa_data_form_descriptions <- function() {
  ecfr_ <- function(.part, .section) sprintf("https://www.ecfr.gov/current/title-17/chapter-II/part-%s/section-%s",
                                             .part, .section)
  tibble::tribble(
    ~Form,    ~Act,             ~Section,    ~Description,
    "S-1",    "Securities Act", "239.11",    "Registration statement under the Securities Act of 1933; the general form",
    "S-3",    "Securities Act", "239.13",    "Registration statement for specified transactions by certain issuers (shelf registration)",
    "SF-1",   "Securities Act", "239.44",    "Registration statement under the Securities Act of 1933 for offerings of asset-backed securities",
    "SF-3",   "Securities Act", "239.45",    "Registration statement for offerings of asset-backed securities offered pursuant to certain types of transactions (shelf)",
    "S-4",    "Securities Act", "239.25",    "Registration of securities issued in business combination transactions",
    "S-8",    "Securities Act", "239.16b",   "Registration of securities to be offered to employees pursuant to employee benefit plans",
    "S-11",   "Securities Act", "239.18",    "Registration of securities of certain real estate companies",
    "F-1",    "Securities Act", "239.31",    "Registration statement for securities of certain foreign private issuers",
    "F-3",    "Securities Act", "239.33",    "Registration statement for specified transactions by certain foreign private issuers",
    "F-4",    "Securities Act", "239.34",    "Registration statement for securities of certain foreign private issuers issued in certain business combination transactions",
    "10",     "Exchange Act",   "249.210",
    "General form for registration of securities pursuant to section 12(b) or (g) of the Exchange Act",
    "8-K",    "Exchange Act",   "249.308",   "Current report",
    "10-D",   "Exchange Act",   "249.312",   "Asset-backed issuer distribution report",
    "10-Q",   "Exchange Act",   "249.308a",  "Quarterly report",
    "10-K",   "Exchange Act",   "249.310",   "Annual report",
    "ABS-EE", "Exchange Act",   "249.1401",  "Asset-backed securities: submission of asset data file and related documents"
  ) |>
    dplyr::mutate(
      Part = dplyr::if_else(.data$Act == "Securities Act", "239", "249"),
      Url  = ecfr_(.part = .data$Part, .section = .data$Section)
    ) |>
    dplyr::select("Form", "Act", "Section", "Description", "Url")
}

#' Build the form-descriptions table
#'
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library, the table's only input.
#' @return Invisibly, the build's status.
oaa_build_form_descriptions <- function(.dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "FormDescriptions"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = .path_lib, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_form_descriptions()
  cells_ <- tibble::tibble(
    Panel       = paste(tab_$Act, "forms"),
    Form        = oa_tex_escape(.x = tab_$Form),
    Section     = paste0("17 C.F.R. ", tab_$Section),
    Description = oa_tex_escape(.x = tab_$Description)
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("Form", "Established in", "What the form is for"),
      .spec   = c(oa_col_text(.share = 0.10), oa_col_text(.share = 0.20), oa_col_text(.share = 0.62))
    ),
    .note    = paste(
      "The forms of the exhibit table, each with the section of the Code of Federal Regulations that establishes it",
      "-- Part 239 for forms under the Securities Act of 1933, Part 249 for forms under the Securities Exchange Act",
      "of 1934 -- and the purpose that section's heading states. Amendments to a form are filed under the same form",
      "type with the suffix /A."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# 25. Moved from 40-Numbers: Appendix B, software (oab_) -------------------------------------------------------------------

#
# 40-OnlineAppendix-B.R -- Appendix B, the software: the exhibits this chapter builds
#
#
# 40-OnlineAppendix-B.qmd holds the chapter's text; _Commons/_OnlineAppendix.R holds everything the chapters share.
# This library holds the two things Appendix B writes itself: the pipeline table, whose content is text rather than
# data and is kept here as a tibble so that it deploys through the same machinery as every other exhibit, and the
# versions of the two packages, read from the installed packages so that the text cannot name a version that is not
# the one the database was built with.
#
# THE PREFIX IS oab_: online appendix, chapter B.


# -- 25.1 The pipeline, stage by stage -------------------------------------------------------------------------------------

#' The pipeline table's content: six stages, what each does, what it produces, where it is described
#'
#' Written as a tibble rather than as prose in the text so that the table is an exhibit like any other -- built,
#' numbered, deployed and read through \\oaown -- and so that its wording is in one place. It names no script: the
#' stages are the ones a reader needs to place the chapters, and the code's own numbering is not part of the paper.
#'
#' @return Tibble: Stage, Does, Produces, Where.
oab_data_stages <- function() {
  tibble::tribble(
    ~Stage, ~Does, ~Produces, ~Where,
    "Acquisition",
    paste("Reads EDGAR's index, visits every selected filing, downloads its documents and converts them",
          "to text"),
    "Every Exhibit 10, Item 1.01 current report and confidential treatment order, as text",
    "A.1; rGetEDGAR (B.2)",
    "Database",
    paste("Identifies unique attachments, flags text that failed conversion, links orders and",
          "announcements to contracts"),
    "One row per contract, with its filing, its copies and its links",
    "A.4",
    "Labelling",
    "Two annotators assign each contract of a stratified sample a category and an amendment flag",
    "The labelled sample",
    "C.1; rLabelDocs (B.3)",
    "Classification",
    paste("Fine-tunes a transformer and derives a keyword table on the labelled sample, out of fold, and",
          "applies both to the corpus"),
    "A category, its probability and a keyword label per contract",
    "C; the classifier (B.4)",
    "Entity extraction",
    paste("Finds partners, places, dates, amounts and redaction markers as spans, and turns spans into",
          "variables by rule"),
    "The content variables per contract",
    "D; the extractors (B.4)",
    "Release",
    "Exports the contract rows, the linked orders and announcements, and the codebook",
    "The published files",
    "F"
  )
}

#' Build the pipeline table
#'
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library, the table's only input.
#' @return Invisibly, the build's status.
oab_build_stages <- function(.dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "PipelineStages"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = .path_lib, .force = .force)) return(invisible("up to date"))
  tab_ <- oab_data_stages()
  cells_ <- tab_ |>
    dplyr::transmute(
      Stage    = paste0("\\textbf{", oa_tex_escape(.x = .data$Stage), "}"),
      Does     = oa_tex_escape(.x = .data$Does),
      Produces = oa_tex_escape(.x = .data$Produces),
      Where    = oa_tex_escape(.x = .data$Where)
    )
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("Stage", "What it does", "What it produces", "Described in"),
      .spec   = c(oa_col_text(.share = 0.14), oa_col_text(.share = 0.40), oa_col_text(.share = 0.28),
                  oa_col_text(.share = 0.14))
    ),
    .note    = paste(
      "The six stages of the pipeline, in the order they run. Each stage reads what the previous one produced and",
      "writes its own result, so a stage can be rerun without repeating the ones before it. The two R packages and",
      "the classification and extraction code described in this appendix are the software behind the stages named",
      "beside them; the remaining stages are scripts of the pipeline itself."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# -- 25.2 The package versions, read from the packages ---------------------------------------------------------------------

#' The versions of the two packages, as installed
#'
#' The text names the versions the database was built with. Read from the installed packages rather than typed, so
#' the sentence follows an upgrade; a package that is not installed resolves to NA and the number reports it.
#'
#' @return Tibble: Key, Value -- rGetEDGAR and rLabelDocs, as "0.1.0".
oab_data_versions <- function() {
  ver_ <- function(.pkg) {
    if (!requireNamespace(.pkg, quietly = TRUE)) return(NA_character_)
    as.character(utils::packageVersion(.pkg))
  }
  tibble::tibble(
    Key   = c("rGetEDGAR", "rLabelDocs"),
    Value = c(ver_(.pkg = "rGetEDGAR"), ver_(.pkg = "rLabelDocs"))
  )
}

#' Build the versions tibble
#'
#' Rebuilt on every render, since an installed package can change without any file of the pipeline changing.
#'
#' @param .dir_own Character. This chapter's output directory.
#' @return Invisibly, the build's status.
oab_build_versions <- function(.dir_own) {
  if (FALSE) .dir_own <- here::here("2_output", "31-FinalExhibits", "Output")
  fs::dir_create(fs::path(.dir_own, "Data"))
  arrow::write_parquet(
    x    = oab_data_versions(),
    sink = fs::path(.dir_own, "Data", "PackageVersions.parquet")
  )
  invisible("built")
}


# 26. Moved from 40-Numbers: Appendix C, classification (oac_) -------------------------------------------------------------
# 31: THE CUTS READ THE FULL TABLES. In one output folder a cut and the table it reads cannot share a name, so
# the full tables are ...Full (ClassLabelledFull, ClassArmsFullCeiling, ...) and each cut reads its ...Full
# tibble and writes under the plain name the appendix inputs. ClassSweep is read as it is; it has no cut of its own.
#

#
# 40-OnlineAppendix-C.R -- Appendix C, the classification approach: the exhibits this chapter builds
#
#
# 40-OnlineAppendix-C.qmd holds the chapter's text; _Commons/_OnlineAppendix.R holds everything the chapters share.
# Every table of the chapter is written here, in the appendix's own cut: most from the data tibbles 30 writes beside
# its own tables (the labelled sample, the sweep, the scores by category, the confusion matrix, the amendment flag,
# the keyword table, the arms and the ceiling), with fewer columns, no fold uncertainty and numbered categories;
# two from other sources -- the descriptions filers give their contracts, from the release, and the second-label
# table, from 03B's out-of-fold classification. 30 stays the manuscript's writer; nothing of 30 is changed or rerun.
#
# THE CATEGORY NUMBERS follow the order of the paper's categories table, which is the order 30 prints, so an error
# that stays inside a parent sits next to the diagonal of the confusion matrix. One vector, .oac_categories, holds
# it; every table reads its numbers from there.
#
# THE PREFIX IS oac_: online appendix, chapter C.


# -- 26.1 The sample, as 30 reads it ---------------------------------------------------------------------------------------

#' The unique contracts of the descriptive sample, as 30 selects them
#'
#' The one reader of the release every build here shares, so all of them count exactly the paper's sample: a
#' contract is kept where DescSample is 1 and it is the primary copy of its attachment -- 30's Keep. The flags are
#' compared after the collect, where an integer and a logical flag compare alike; in Arrow they need not.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .cols Character. The columns to keep besides the flags.
#' @return Tibble: .cols, plus Date and Year, for 2001-2024.
oac_read_sample <- function(.path_contracts, .cols) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .cols <- c("FormType", "DateFiled")
  }
  arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::all_of(unique(c(.cols, "DateFiled", "PrimaryFiler", "DescSample")))) |>
    dplyr::collect() |>
    dplyr::filter(
      dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      dplyr::coalesce(as.integer(.data$DescSample), 0L) == 1L
    ) |>
    dplyr::mutate(
      Date = as.Date(.data$DateFiled),
      Year = as.integer(format(.data$Date, "%Y"))
    ) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::select(-"PrimaryFiler", -"DescSample")
}

# -- 26.2 What filers call their contracts, per category -------------------------------------------------------------------

#' The most frequent descriptions filers give their contracts, per category
#'
#' The filer's own description is the only human-written label an attachment carries, so it shows what a category
#' holds in the words of the people who file. Free text: filers write what they like, and many write nothing beyond
#' the exhibit number. Three rules make the descriptions comparable. A leading exhibit number is stripped, so that
#' "10.1 Credit Agreement" counts with "Credit Agreement". A description that carries no words beyond an exhibit
#' number or a form name is dropped as uninformative, and the share it accounts for is reported per category.
#' Descriptions are grouped case- and punctuation-insensitively, and the group is printed in its most frequent
#' original spelling.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .n Integer. Descriptions per category.
#' @return Tibble: Class, Kind, Rank, Description, N, Share (of the category's contracts that carry a description),
#'   plus nClass, nNamed and pNamed per category.
oac_data_titles <- function(.path_contracts, .n) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .n <- 10L
  }
  # A LEADING EXHIBIT NUMBER, in the spellings filers use: EX-10.1, EXHIBIT 10.23, 10.1, (10.1), 10.1 -
  # It must carry a decimal point, or the word EX or EXHIBIT: a bare number is part of the title -- a year in
  # "2020 Equity Incentive Plan", a count in "3 Year Supply Agreement" -- and stripping it would corrupt the text.
  .re_number <- paste0(
    "(?i)^[\\s(\\[]*(",
    "ex(hibit)?[\\s.-]*\\d{1,3}([.(][a-z0-9]+\\)?)*",   # EX-10.1, EXHIBIT 10, EX 10.23(a), Ex. 10(a)
    "|\\d{1,3}([.(][a-z0-9]+\\)?)+",                    # 10.1, 10(a), 10.23a
    ")[\\s)\\]:.,-]*"
  )
  # NOTHING BUT A WORD FOR THE EXHIBIT ITSELF, or a form name: no description at all
  .re_unnamed <- paste0(
    "(?i)^(ex|exhibit|exhibits|document|attachment|annex|appendix|material contract[s]?|agreement|contract|",
    "8-k|10-k|10-q|20-f|s-1|s-4|f-1|f-4)$"
  )
  con_ <- oac_read_sample(
    .path_contracts = .path_contracts,
    .cols           = c("Class", "DocDesc")
  ) |>
    dplyr::filter(!is.na(.data$Class))
  if (nrow(con_) == 0L) cli::cli_abort("No contract carries a {.field Class}; the release's labels differ.")
  clean_ <- con_ |>
    dplyr::mutate(
      Desc = trimws(dplyr::coalesce(.data$DocDesc, "")),
      Desc = stringi::stri_replace_first_regex(.data$Desc, .re_number, ""),
      Desc = trimws(stringi::stri_replace_all_regex(.data$Desc, "\\s+", " ")),
      # UNINFORMATIVE: nothing left, or nothing but a form name or a word for the exhibit itself
      Named = nzchar(.data$Desc) & !stringi::stri_detect_regex(.data$Desc, .re_unnamed),
      Key = toupper(stringi::stri_replace_all_regex(.data$Desc, "[^[:alnum:] ]", " ")),
      Key = trimws(stringi::stri_replace_all_regex(.data$Key, "\\s+", " "))
    )
  per_class_ <- clean_ |>
    dplyr::summarise(nClass = dplyr::n(), pNamed = mean(.data$Named), .by = "Class")
  # A READABLE SPELLING. Filers most often write in capitals, and a table of capitals is hard to read and says
  # nothing the lower-case spelling does not. The most frequent spelling that is not all capitals is printed where
  # there is one; otherwise the capitals are set in title case, with the short words that belong inside a title
  # left lower-case.
  pretty_ <- function(.x) {
    small_ <- c("a", "an", "and", "as", "at", "by", "for", "from", "in", "of", "on", "or", "the", "to", "with")
    words_ <- strsplit(tolower(.x), " ", fixed = TRUE)[[1L]]
    up_    <- paste0(toupper(substring(words_, 1L, 1L)), substring(words_, 2L))
    out_   <- ifelse(words_ %in% small_ & seq_along(words_) > 1L, words_, up_)
    paste(out_, collapse = " ")
  }
  n_named_ <- clean_ |>
    dplyr::filter(.data$Named) |>
    dplyr::summarise(nNamed = dplyr::n(), .by = "Class")
  top_ <- clean_ |>
    dplyr::filter(.data$Named) |>
    dplyr::count(.data$Class, .data$Key, .data$Desc, name = "nSpelling") |>
    dplyr::arrange(dplyr::desc(.data$nSpelling)) |>
    dplyr::summarise(
      Description = {
        mixed_ <- .data$Desc[.data$Desc != toupper(.data$Desc)]
        if (length(mixed_) > 0L) mixed_[[1L]] else pretty_(.x = .data$Desc[[1L]])
      },
      N           = sum(.data$nSpelling),
      .by         = c("Class", "Key")
    ) |>
    dplyr::arrange(.data$Class, dplyr::desc(.data$N)) |>
    dplyr::slice_head(n = .n, by = "Class") |>
    dplyr::mutate(Rank = dplyr::row_number(), .by = "Class") |>
    dplyr::left_join(per_class_, by = "Class") |>
    dplyr::left_join(n_named_, by = "Class") |>
    dplyr::mutate(Share = .data$N / .data$nNamed, Kind = "title") |>
    dplyr::select("Class", "Kind", "Rank", "Description", "N", "Share", "nClass", "nNamed", "pNamed")
  dplyr::arrange(top_, .data$Class, .data$Rank)
}

#' Build the table of filer descriptions by category, one row per category
#'
#' The reviewer asked for example titles; a list of ten per category runs to a page and a half, and the same content
#' fits twelve rows: the category, its most frequent descriptions on one line with their counts, and the share of
#' its contracts that carry a description at all.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .n Integer. Descriptions per category.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oac_build_titles <- function(.path_contracts, .n, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .n        <- 5L
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "CategoryTitles"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oac_data_titles(
    .path_contracts = .path_contracts,
    .n              = .n
  )
  num_ <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)
  rows_ <- tab_ |>
    dplyr::arrange(.data$Class, .data$Rank) |>
    dplyr::summarise(
      Descriptions = paste0(oa_tex_escape(.x = .data$Description), " (", num_(.x = .data$N), ")", collapse = "; "),
      pNamed       = dplyr::first(.data$pNamed),
      .by          = "Class"
    ) |>
    dplyr::mutate(Order = oac_number(.name = .data$Class)) |>
    dplyr::arrange(.data$Order)
  cells_ <- tibble::tibble(
    Category     = oac_label(.class = rows_$Class),
    Descriptions = rows_$Descriptions,
    Named        = formatC(100 * rows_$pNamed, format = "f", digits = 0L)
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("Category", paste("The", .n, "most frequent descriptions (contracts)"), "\\% described"),
      .spec   = c(oa_col_text(.share = 0.22), oa_col_text(.share = 0.62), oa_col_num(.mm = 18))
    ),
    .note    = paste(
      "The", .n, "most frequent descriptions filers give their contracts, by category, over the unique contracts of",
      "the descriptive sample; the category is the classifier's label. A description is the filer's own free text,",
      "the only human-written label an attachment carries. A leading exhibit number is stripped before counting, and",
      "a description that says nothing beyond an exhibit or form name is treated as no description; the last column",
      "is the share of a category's contracts that carry one. Descriptions are grouped without regard to case and",
      "punctuation and printed in the most frequent spelling that is not in capitals."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# -- 26.3 The chapter's cut of 30's classification tables ------------------------------------------------------------------
# 30 writes the manuscript's version of each table and, beside it, the data tibble it printed. The appendix prints
# the same numbers with fewer columns, no fold uncertainty and numbered categories. Each builder reads 30's tibble,
# writes the appendix's table under this chapter's directory, and keeps the tibble it read as its own data, so the
# numbers in the text resolve from the same file whichever copy is found first.

# THE CATEGORIES, in the order of the paper's categories table. Class is the label 30's tibbles carry.
.oac_categories <- tibble::tibble(
  Short  = c("Credit", "Equity", "Compensation", "Legal", "Assets", "R&D", "Customer / Supplier", "Licenses",
             "Leases", "Peer Agreements", "M&A", "Other"),
  Number = 1:12
)

#' A category's number, from any of the names it goes by
#'
#' 30's tibbles carry the short name in Row and a qualified name in Class ("Employment: Compensation"); the release
#' carries the qualified name, and M&A's qualified name is "Investment and Merger". The number is looked up on the
#' short name, taken as the part after the parent where there is one, with that alias.
#'
#' @param .name Character. Short or qualified category names.
#' @return Integer, NA where the name is not one of the twelve.
oac_number <- function(.name) {
  if (FALSE) .name <- c("Employment: Compensation", "R&D", "Business Structure: Investment and Merger")
  short_ <- sub("^.*: ", "", as.character(.name))
  short_[short_ %in% c("Investment and Merger", "Investment & Merger", "Mergers and Acquisitions")] <- "M&A"
  match(short_, .oac_categories$Short)
}

#' A category's printed label: its number and its short name, escaped for LaTeX
#'
#' @param .class Character. Category names, short or qualified.
#' @return Character, "(3) Compensation"; NA where the class is not one of the twelve.
oac_label <- function(.class) {
  if (FALSE) .class <- c("Employment: Compensation", "R&D")
  i_ <- oac_number(.name = .class)
  dplyr::if_else(is.na(i_), NA_character_,
                 paste0("(", .oac_categories$Number[i_], ") ", oa_tex_escape(.x = .oac_categories$Short[i_])))
}

#' 30's data tibble for one exhibit, or an abort that names the file
#'
#' @param .dir_data Character. 30's Output/Data.
#' @param .name Character. The exhibit's stem.
#' @return Tibble.
oac_read_30 <- function(.dir_data, .name) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "31-FinalExhibits", "Output", "Data")
    .name     <- "ClassLabelled"
  }
  p_ <- fs::path(.dir_data, paste0(.name, ".parquet"))
  if (!fs::file_exists(p_)) cli::cli_abort("{.file {p_}} is missing: the full tables are built before their cuts.")
  arrow::read_parquet(p_)
}

#' The row labels of a category table: numbered leaves, bold parents, bold total
#'
#' 30's category tibbles carry Row (the printed name) and Kind (super, sub, total). A sub row is a leaf under its
#' parent, indented and numbered; a super row whose name is one of the twelve -- Licenses, Leases, Other -- is a leaf
#' of its own and is numbered in bold; any other super row is a parent, in bold without a number.
#'
#' @param .tab Tibble with Row and Kind.
#' @return Character, one label per row, escaped.
oac_row_labels <- function(.tab) {
  if (FALSE) .tab <- tibble::tibble(Row = c("Employment", "Compensation", "Licenses", "Total"),
                                    Kind = c("super", "sub", "super", "total"))
  lab_ <- oac_label(.class = .tab$Row)
  dplyr::case_when(
    .tab$Kind == "total"              ~ paste0("\\textbf{", oa_tex_escape(.x = .tab$Row), "}"),
    !is.na(lab_) & .tab$Kind == "sub" ~ paste0("\\hspace{1em}", lab_),
    !is.na(lab_)                      ~ paste0("\\textbf{", lab_, "}"),
    .default                          = paste0("\\textbf{", oa_tex_escape(.x = .tab$Row), "}")
  )
}

#' Whether a build of one of 30's tables is due
#'
#' @param .dir_own,.name,.path_30,.path_lib,.force As in the builders.
#' @return Logical.
oac_due <- function(.dir_own, .name, .path_30, .path_lib, .force) {
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(.name, c(".tex", ".tex", ".parquet")))
  oa_build_needed(.outputs = outs_, .inputs = c(.path_30, .path_lib), .force = .force)
}

oac_fmt3 <- function(.x) dplyr::if_else(is.na(.x), "--", formatC(.x, format = "f", digits = 3L))
oac_fmtn <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)

#' Build the labelled-sample table: N, share and second labels per category
#'
#' @param .dir_data Character. 30's Output/Data.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oac_build_labelled <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "31-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "ClassLabelled"
  p30_  <- fs::path(.dir_data, paste0(name_, "Full.parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = paste0(name_, "Full"))
  # THE AMENDMENT BLOCK -- Original / Amended under an "Amendment label" heading -- is a second table's worth of
  # rows; the appendix states the two counts in the text and prints the categories alone.
  cat_ <- tab_ |>
    dplyr::filter(is.na(.data$Level1) | .data$Level1 != "Amendment")
  cells_ <- tibble::tibble(
    Row    = oac_row_labels(.tab = cat_),
    N      = oac_fmtn(.x = cat_$N),
    Share  = formatC(cat_$Share, format = "f", digits = 1L),  # 30 stores the share as a percent
    Second = oac_fmtn(.x = dplyr::coalesce(cat_$Second, 0L))
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("", "N", "\\%", "Second label"),
      .spec   = c(oa_col_text(.share = 0.52), oa_col_num(.mm = 18), oa_col_num(.mm = 14), oa_col_num(.mm = 24))
    ),
    .note    = paste(
      "The labelled sample: contracts with readable text and a label, by category. Parent rows sum their",
      "sub-categories. Second label counts the contracts that carry a second valid category, recorded beside the",
      "primary and never used in training. The numbers in parentheses are the categories' numbers throughout this",
      "appendix, in the order of the paper's categories table."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the deployed-models table from the sweep: what was crowned and what was deployed, per task
#'
#' The sweep itself is stated in the text -- the grid, how many configurations, how the winner was chosen. The table
#' shows only the configurations that left the sweep: the one that ships per task and the ones deployed beside it.
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_deployed <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "31-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "ClassDeployed"
  p30_  <- fs::path(.dir_data, "ClassSweep.parquet")
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = "ClassSweep") |>
    dplyr::mutate(Crown = dplyr::coalesce(as.logical(.data$Crown), FALSE),
                  Deployed = dplyr::coalesce(as.logical(.data$Deployed), FALSE)) |>
    dplyr::filter(.data$Crown | .data$Deployed) |>
    dplyr::mutate(
      TaskOrder = match(.data$LabelCol, c("ClassDetailed", "ClassBroad", "AmendType")),
      Status    = dplyr::if_else(.data$Crown, "ships", "deployed")
    ) |>
    dplyr::arrange(.data$TaskOrder, dplyr::desc(.data$Crown), .data$MaxLen)
  cells_ <- tibble::tibble(
    Task     = oa_tex_escape(.x = as.character(tab_$Task)),
    Model    = oa_tex_escape(.x = sub("^.*/", "", as.character(tab_$Model))),
    Context  = as.character(tab_$MaxLen),
    Accuracy = oac_fmt3(.x = tab_$Accuracy),
    MacroF1  = oac_fmt3(.x = tab_$MacroF1),
    Status   = tab_$Status
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("Task", "Encoder", "Context", "Accuracy", "Macro-F1", ""),
      .spec   = c(oa_col_text(.share = 0.20), oa_col_text(.share = 0.30), oa_col_num(.mm = 16), oa_col_num(.mm = 20),
                  oa_col_num(.mm = 20), "l")
    ),
    .note    = paste(
      "The configurations that left the sweep: per task, the one crowned by the highest mean out-of-fold macro-F1,",
      "which labels the corpus (ships), and the ones refitted beside it (deployed). Context is the number of tokens",
      "read from the start of a contract. Accuracy and macro-F1 are pooled out of fold over the five folds."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build a scores-by-category table in the appendix's cut, from one of 30's engine tibbles
#'
#' Serves the transformer and the keyword table, which share 30's layout. No fold uncertainty; the columns the
#' text reads: support, and per category precision, recall and F1, with lenient recall for the transformer and
#' coverage for the keyword table.
#'
#' @param .name Character. "ClassTransformer" or "ClassKeyword".
#' @param .columns Character. The score columns to print, from 30's tibble.
#' @param .header Character. Their printed headers.
#' @param .note Character. The table's note.
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_scores <- function(.name, .columns, .header, .note, .dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .name     <- "ClassTransformer"
    .columns  <- c("Precision", "Recall", "F1", "RecallLenient")
    .header   <- c("Precision", "Recall", "F1", "Lenient recall")
    .note     <- "..."
    .dir_data <- here::here("2_output", "31-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  p30_ <- fs::path(.dir_data, paste0(.name, "Full.parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = .name, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = paste0(.name, "Full"))
  miss_ <- setdiff(.columns, names(tab_))
  if (length(miss_) > 0L) cli::cli_abort("30's {.val {(.name)}} lacks {.field {miss_}}.")
  cells_ <- dplyr::bind_cols(
    tibble::tibble(Row = oac_row_labels(.tab = tab_), N = oac_fmtn(.x = tab_$Support)),
    tab_ |>
      dplyr::select(dplyr::all_of(.columns)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(.x) oac_fmt3(.x = .x)))
  )
  oa_write_exhibit(
    .name    = .name,
    .lines   = fin_tex_frame(
      .name   = .name,
      .tab    = cells_,
      .header = c("", "N", .header),
      .spec   = c(oa_col_text(.share = 0.34), oa_col_num(.mm = 16), rep(oa_col_num(.mm = 18), length(.columns)))
    ),
    .note    = .note,
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the confusion matrix with numbered categories on both axes
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_confusion <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "31-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "ClassConfusion"
  p30_  <- fs::path(.dir_data, paste0(name_, "Full.parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = paste0(name_, "Full"))
  cols_ <- .oac_categories$Short
  miss_ <- setdiff(cols_, names(tab_))
  if (length(miss_) > 0L) cli::cli_abort("30's confusion matrix lacks the column{?s} {.val {miss_}}.")
  tab_ <- tab_[match(cols_, tab_$True), , drop = FALSE]
  # A CONTRACT COUNTS ONCE, in the row of its true category and the column of its prediction, so the matrix is not
  # symmetric: (1, 2) counts credit contracts predicted as equity, (2, 1) equity contracts predicted as credit. Rows
  # sum to a category's contracts, columns to how often the model chose it. The diagonal -- the contracts classified
  # correctly -- is bold; any other zero is a blank, so the errors stand out, as in the second-label table.
  k_   <- length(cols_)
  m_   <- as.matrix(tab_[, cols_])
  v_   <- as.vector(m_)
  chr_ <- matrix(dplyr::if_else(v_ == 0, "", oac_fmtn(.x = v_)), nrow = k_)
  diag(chr_) <- paste0("\\textbf{", oac_fmtn(.x = diag(m_)), "}")
  cells_ <- dplyr::bind_cols(
    tibble::tibble(True = oac_label(.class = tab_$True)),
    tibble::as_tibble(chr_, .name_repair = \(.x) cols_)
  )
  lines_ <- fin_tex_frame(
    .name   = name_,
    .tab    = cells_,
    .header = c("True category", paste0("(", .oac_categories$Number, ")")),
    .spec   = c(oa_col_text(.share = 0.22), rep(oa_col_num(.mm = 9), k_))
  )
  # THE PREDICTION NAMES THE COLUMNS, in a spanning row above their numbers.
  lines_ <- append(
    x      = lines_,
    values = c(
      paste0(" & \\multicolumn{", k_, "}{c}{Predicted category} \\\\"),
      paste0("\\cmidrule(lr){2-", k_ + 1L, "}")
    ),
    after  = which(lines_ == "\\hline\\hline")[1L]
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = lines_,
    .note    = paste(
      "Confusion matrix of the transformer that ships on the labelled sample, out of fold: rows are the true category,",
      "columns the predicted one, numbered as in the labelled-sample table; cells are numbers of contracts, blank where",
      "zero, and the diagonal, in bold, counts the contracts classified correctly. Each contract counts once, in the",
      "row of its true category and the column of its prediction, so the table is not symmetric: a row sums to the",
      "category's contracts, a column to how often the model predicted the category. Categories are in taxonomic",
      "order, so an error that stays inside a parent sits next to the diagonal and one that crosses a parent sits",
      "further from it."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the amendment table: the two labels and the total, without fold uncertainty
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_amendment <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "31-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "ClassAmendment"
  p30_  <- fs::path(.dir_data, paste0(name_, "Full.parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = paste0(name_, "Full"))
  tot_ <- tab_$Label == "Total"
  cells_ <- tibble::tibble(
    Label     = dplyr::if_else(tot_, paste0("\\textbf{", oa_tex_escape(.x = tab_$Label), "}"),
                               oa_tex_escape(.x = tab_$Label)),
    N         = oac_fmtn(.x = tab_$Support),
    Predicted = oac_fmtn(.x = tab_$Predicted),
    Precision = oac_fmt3(.x = tab_$Precision),
    Recall    = oac_fmt3(.x = tab_$Recall),
    F1        = oac_fmt3(.x = tab_$F1)
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("", "N", "Predicted", "Precision", "Recall", "F1"),
      .spec   = c(oa_col_text(.share = 0.30), rep(oa_col_num(.mm = 18), 5L))
    ),
    .note    = paste(
      "Out-of-fold scores of the amendment classifier that ships on the labelled sample. N is the number of",
      "contracts with the label, Predicted the number the classifier assigned it. The Total row reports overall",
      "accuracy in the precision column and macro recall and macro-F1 beside it."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the arms table: every engine on every task, and the ceiling, without fold uncertainty
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_arms <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "31-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "ClassArms"
  p30_  <- fs::path(.dir_data, c("ClassArmsFull.parquet", "ClassArmsFullCeiling.parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  arms_ <- oac_read_30(.dir_data = .dir_data, .name = "ClassArmsFull")
  ceil_ <- oac_read_30(.dir_data = .dir_data, .name = "ClassArmsFullCeiling")
  task_ <- c(ClassDetailed = "Detailed (12)", ClassBroad = "Broad (7)", AmendType = "Amendment (2)")
  eng_  <- c(Bert256 = "Transformer, 256 tokens", Bert512 = "Transformer, 512 tokens", Kw = "Keyword table",
             Llm = "Language model")
  arms_ <- arms_ |>
    dplyr::mutate(TaskOrder = match(.data$Task, names(task_)), EngOrder = match(.data$Engine, names(eng_))) |>
    dplyr::arrange(.data$TaskOrder, .data$EngOrder)
  a_ <- tibble::tibble(
    Panel    = unname(dplyr::coalesce(task_[arms_$Task], arms_$Task)),
    Engine   = paste0(oa_tex_escape(.x = unname(dplyr::coalesce(eng_[arms_$Engine], arms_$Engine))),
                      dplyr::if_else(dplyr::coalesce(as.logical(arms_$Ships), FALSE), " (ships)", "")),
    Coverage = oac_fmt3(.x = arms_$Coverage),
    Accuracy = oac_fmt3(.x = arms_$Accuracy),
    AccSel   = oac_fmt3(.x = arms_$AccuracySel),
    MacroF1  = oac_fmt3(.x = arms_$MacroF1)
  )
  c_ <- tibble::tibble(
    Panel    = "Detailed, ceiling of perfect routing",
    Engine   = oa_tex_escape(.x = as.character(ceil_$Engines)),
    Coverage = "",
    Accuracy = oac_fmt3(.x = ceil_$Accuracy),
    AccSel   = "",
    MacroF1  = paste0("+", oac_fmt3(.x = ceil_$Marginal))
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = dplyr::bind_rows(a_, c_),
      .header = c("", "Coverage", "Accuracy", "Accuracy where committed", "Macro-F1"),
      .spec   = c(oa_col_text(.share = 0.36), oa_col_num(.mm = 18), oa_col_num(.mm = 18), oa_col_num(.mm = 30),
                  oa_col_num(.mm = 18))
    ),
    .note    = paste(
      "Every engine on every task, out of fold on the same five folds. Coverage is the share of contracts an engine",
      "labels; the transformer and the language model label every contract, the keyword table abstains where no",
      "term fires. Accuracy is over all contracts, an abstention counting as wrong; accuracy where committed is over",
      "the contracts the engine labelled. The last rows are the ceiling: the accuracy perfect routing would reach on",
      "the detailed task, taking for every contract whichever of the engines named is right, which needs the true",
      "label and cannot be run; the last column is what each added engine gains over the ones before it."
    ),
    .data    = dplyr::bind_rows(dplyr::mutate(arms_, Block = "arms"), dplyr::mutate(ceil_, Block = "ceiling")),
    .dir_own = .dir_own
  )
  invisible("built")
}


# -- 26.3 The second label against the runner-up ---------------------------------------------------------------------------

#' The second-label data: every dual-labelled document with the model's two choices beside its two labels
#'
#' 03A records a second valid category for the minority of labelled documents that fit two, reviewed by hand so that
#' the primary is the intended one; training never uses it. 03B's classification file carries, for every labelled
#' document, the model's first and second choice with their probabilities, out of fold, and the second label beside
#' them. Two tables are built from it: the matrix of primary against second label, which shows where the annotators
#' saw two answers, and, per primary category, how often the model's first choice is the primary and its runner-up
#' the second label. The margin between the model's two choices, on documents with two labels and with one, is kept
#' in the data for the text.
#'
#' @param .path_class Character. 03B's crowned_classification.parquet for the detailed task.
#' @return Tibble: DocID, Primary, Second, Top1, Top2, Top1Prob, Margin, Dual.
oac_data_second <- function(.path_class) {
  if (FALSE) {
    .path_class <- here::here("2_output", "03B-ClassifyTrainBERT", "classification", "crowned_classification.parquet")
  }
  tab_ <- arrow::read_parquet(.path_class) |>
    dplyr::select(dplyr::any_of(c("DocID", "TrueLabel", "Top1Class", "Top1Prob", "Top2Class", "Margin",
                                  "ClassDetailed2")))
  need_ <- setdiff(c("TrueLabel", "Top1Class", "Top2Class", "Top1Prob", "Margin", "ClassDetailed2"), names(tab_))
  if (length(need_) > 0L) cli::cli_abort("The classification file lacks {.field {need_}}.")
  tab_ |>
    dplyr::transmute(
      DocID    = .data$DocID,
      Primary  = as.character(.data$TrueLabel),
      Second   = as.character(.data$ClassDetailed2),
      Top1     = as.character(.data$Top1Class),
      Top2     = as.character(.data$Top2Class),
      Top1Prob = .data$Top1Prob,
      Margin   = .data$Margin,
      Dual     = !is.na(.data$Second) & nzchar(.data$Second)
    )
}

#' The per-category hits and the margins, from the second-label data
#'
#' @param .tab Tibble from oac_data_second().
#' @return Tibble: Row (a category or Total), Kind, N (documents with two labels), Hit1, Hit2, Both, plus the
#'   two margin rows (Kind "margin": N is the group's size, Value its mean margin).
oac_second_summary <- function(.tab) {
  if (FALSE) .tab <- oac_data_second(.path_class = here::here("2_output", "03B-ClassifyTrainBERT", "classification",
                                                                 "crowned_classification.parquet"))
  dual_ <- dplyr::filter(.tab, .data$Dual) |>
    dplyr::mutate(
      Hit1 = .data$Top1 == .data$Primary,
      Hit2 = .data$Top2 == .data$Second,
      Both = .data$Hit1 & .data$Hit2,
      Order = oac_number(.name = .data$Primary)
    )
  by_ <- dual_ |>
    dplyr::summarise(N = dplyr::n(), Hit1 = sum(.data$Hit1), Hit2 = sum(.data$Hit2), Both = sum(.data$Both),
                     .by = c("Primary", "Order")) |>
    dplyr::arrange(.data$Order) |>
    dplyr::transmute(Row = .data$Primary, Kind = "category", N = .data$N, Hit1 = .data$Hit1, Hit2 = .data$Hit2,
                     Both = .data$Both, Value = NA_real_)
  tot_ <- tibble::tibble(Row = "Total", Kind = "total", N = nrow(dual_), Hit1 = sum(dual_$Hit1),
                         Hit2 = sum(dual_$Hit2), Both = sum(dual_$Both), Value = NA_real_)
  mar_ <- .tab |>
    dplyr::summarise(N = dplyr::n(), Value = mean(.data$Margin, na.rm = TRUE), .by = "Dual") |>
    dplyr::transmute(Row = dplyr::if_else(.data$Dual, "Margin, two labels", "Margin, one label"), Kind = "margin",
                     N = .data$N, Hit1 = NA_integer_, Hit2 = NA_integer_, Both = NA_integer_, Value = .data$Value)
  dplyr::bind_rows(by_, tot_, mar_)
}

#' Build the two second-label tables: the matrix of label pairs, and the model's hits per category
#'
#' @param .path_class Character. 03B's crowned_classification.parquet.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oac_build_second <- function(.path_class, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_class <- here::here("2_output", "03B-ClassifyTrainBERT", "classification", "crowned_classification.parquet")
    .dir_own    <- here::here("2_output", "31-FinalExhibits", "Output")
    .force      <- FALSE
    .path_lib   <- here::here("1_code", "31-FinalExhibits.R")
  }
  names_ <- c("ClassSecondPairs", "ClassSecond")
  outs_  <- unlist(purrr::map(names_, \(.n) fs::path(.dir_own, c("Tables", "Notes", "Data"),
                                                       paste0(.n, c(".tex", ".tex", ".parquet")))))
  if (!fs::file_exists(.path_class)) return(invisible("waiting for 03B's crowned_classification.parquet"))
  if (!oa_build_needed(.outputs = outs_, .inputs = c(.path_class, .path_lib), .force = .force)) {
    return(invisible("up to date"))
  }
  tab_  <- oac_data_second(.path_class = .path_class)
  dual_ <- dplyr::filter(tab_, .data$Dual)
  # THE MATRIX: primary label down, second label across, both numbered; a zero prints as a blank so the pairs stand out
  k_ <- nrow(.oac_categories)
  m_ <- matrix(0L, nrow = k_, ncol = k_)
  i_ <- oac_number(.name = dual_$Primary)
  j_ <- oac_number(.name = dual_$Second)
  if (anyNA(i_) || anyNA(j_)) cli::cli_abort("A second-label category is not one of the twelve.")
  for (r_ in seq_along(i_)) m_[i_[r_], j_[r_]] <- m_[i_[r_], j_[r_]] + 1L
  pairs_ <- tibble::as_tibble(m_, .name_repair = \(.x) .oac_categories$Short) |>
    dplyr::mutate(Primary = .oac_categories$Short, .before = 1L)
  # A CONTRACT COUNTS ONCE, in the row of its primary label and the column of its second, so the matrix is not
  # symmetric: (2, 1) counts equity contracts whose second label is credit, (1, 2) credit contracts whose second
  # label is equity. The diagonal is empty by construction -- a second label differs from the primary -- and
  # prints as a dash; any other zero is a blank, so the pairs stand out.
  v_   <- as.vector(m_)
  chr_ <- matrix(dplyr::if_else(v_ == 0L, "", oac_fmtn(.x = v_)), nrow = k_)
  diag(chr_) <- "--"
  cells_p_ <- dplyr::bind_cols(
    tibble::tibble(Primary = oac_label(.class = pairs_$Primary)),
    tibble::as_tibble(chr_, .name_repair = \(.x) .oac_categories$Short)
  )
  lines_p_ <- fin_tex_frame(
    .name   = names_[1L],
    .tab    = cells_p_,
    .header = c("Primary label", paste0("(", .oac_categories$Number, ")")),
    .spec   = c(oa_col_text(.share = 0.22), rep(oa_col_num(.mm = 9), k_))
  )
  # THE SECOND LABEL NAMES THE COLUMNS, in a spanning row above their numbers.
  lines_p_ <- append(
    x      = lines_p_,
    values = c(
      paste0(" & \\multicolumn{", k_, "}{c}{Second label} \\\\"),
      paste0("\\cmidrule(lr){2-", k_ + 1L, "}")
    ),
    after  = which(lines_p_ == "\\hline\\hline")[1L]
  )
  oa_write_exhibit(
    .name    = names_[1L],
    .lines   = lines_p_,
    .note    = paste(
      "The", oac_fmtn(.x = nrow(dual_)), "labelled contracts that carry a second valid category: rows are the",
      "primary label, columns the second, numbered as in the labelled-sample table; cells are numbers of contracts,",
      "blank where zero. Each contract counts once, in the row of its primary label and the column of its second, so",
      "the table is not symmetric: (2, 1) counts equity contracts whose second label is credit, (1, 2) credit",
      "contracts whose second label is equity, and a row sums to the second-label count of its category in the",
      "labelled-sample table. Every such contract was reviewed and the intended primary recorded, so the direction is",
      "informative. The diagonal is empty by construction, since a second label differs from the primary."
    ),
    .data    = pairs_,
    .dir_own = .dir_own
  )
  # THE HITS: per primary category, the model's first choice against the primary and its runner-up against the second
  sum_ <- oac_second_summary(.tab = tab_)
  rows_ <- dplyr::filter(sum_, .data$Kind %in% c("category", "total"))
  pct_ <- function(.n, .d) paste0(oac_fmtn(.x = .n), " (", formatC(100 * .n / .d, format = "f", digits = 0L), ")")
  cells_h_ <- tibble::tibble(
    Row  = dplyr::if_else(rows_$Kind == "total", paste0("\\textbf{", rows_$Row, "}"),
                          dplyr::coalesce(oac_label(.class = rows_$Row), oa_tex_escape(.x = rows_$Row))),
    N    = oac_fmtn(.x = rows_$N),
    Hit1 = pct_(.n = rows_$Hit1, .d = rows_$N),
    Hit2 = pct_(.n = rows_$Hit2, .d = rows_$N),
    Both = pct_(.n = rows_$Both, .d = rows_$N)
  )
  oa_write_exhibit(
    .name    = names_[2L],
    .lines   = fin_tex_frame(
      .name   = names_[2L],
      .tab    = cells_h_,
      .header = c("Primary label", "Two labels", "First choice is the primary (\\%)",
                  "Runner-up is the second label (\\%)", "Both (\\%)"),
      .spec   = c(oa_col_text(.share = 0.26), oa_col_num(.mm = 18), oa_col_num(.mm = 30), oa_col_num(.mm = 32),
                  oa_col_num(.mm = 20))
    ),
    .note    = paste(
      "The contracts with two labels, by primary label, against the detailed transformer's first and second choice",
      "out of fold: how many the model's first choice labels with the primary, how many its runner-up labels with",
      "the second label, and how many both. Shares are of the row's contracts."
    ),
    .data    = sum_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# 27. Moved from 40-Numbers: Appendix D, entity extraction (oad_) ----------------------------------------------------------

#
# 40-OnlineAppendix-D.R -- Appendix D, entity extraction and the rules: the exhibits this chapter builds
#
#
# 40-OnlineAppendix-D.qmd holds the chapter's text; _Commons/_OnlineAppendix.R holds everything the chapters share.
# This library builds the chapter's own exhibits: what each extractor found on the labelled sample, counted from the
# three span stores 04A wrote; where in a contract each extractor's candidates fall, and how far the extractors agree,
# computed from the same stores with 04A's definitions; where each contract's end date comes from, and what the content
# variables are worth under the naive and the rule-based reading, both from the release. The naive-against-rule
# coverage by category is 30's.
#
# THE PREFIX IS oad_: online appendix, chapter D.


# -- 27.1 The sample, as 30 reads it ---------------------------------------------------------------------------------------

#' The unique contracts of the descriptive sample, as 30 selects them
#'
#' The one reader of the release every build here shares, so all of them count exactly the paper's sample: a
#' contract is kept where DescSample is 1 and it is the primary copy of its attachment -- 30's Keep. The flags are
#' compared after the collect, where an integer and a logical flag compare alike; in Arrow they need not.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .cols Character. The columns to keep besides the flags.
#' @return Tibble: .cols, plus Date and Year, for 2001-2024.
oad_read_sample <- function(.path_contracts, .cols) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .cols <- c("FormType", "DateFiled")
  }
  arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::all_of(unique(c(.cols, "DateFiled", "PrimaryFiler", "DescSample")))) |>
    dplyr::collect() |>
    dplyr::filter(
      dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      dplyr::coalesce(as.integer(.data$DescSample), 0L) == 1L
    ) |>
    dplyr::mutate(
      Date = as.Date(.data$DateFiled),
      Year = as.integer(format(.data$Date, "%Y"))
    ) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::select(-"PrimaryFiler", -"DescSample")
}

# -- 27.2 What each extractor found on the labelled sample -----------------------------------------------------------------

#' Spans and coverage per family, model and entity, from 04A's stores
#'
#' 04A writes one DuckDB file per family under its Output/Store, holding one table per entity, every row a span with
#' its document and its offsets; spaCy's tables also carry the model. The stores are opened read-only and counted:
#' spans, and the documents in which the family found at least one, over the documents of the sample. Coverage is
#' not a quality measure -- an extractor that tags every capitalised word reaches complete coverage -- and the text
#' says so; what it establishes is what each extractor attempts and how much it proposes.
#'
#' @param .dir_store Character. 04A's Output, holding matcon.duckdb, spacy.duckdb and lexnlp.duckdb.
#' @param .path_sample Character. 04A's sample_text.parquet, the documents every store indexes.
#' @return Tibble: Family, Model, Entity, Spans, Docs, Coverage, over nDocs documents (as an attribute-free column).
oad_data_coverage <- function(.dir_store, .path_sample) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
  }
  n_docs_ <- nrow(arrow::read_parquet(.path_sample, col_select = "DocID"))
  meta_   <- c("ledger", "manifest", "bench", "failures", "corpus_index", "sample")
  read_family_ <- function(.family) {
    p_ <- fs::path(.dir_store, paste0(.family, ".duckdb"))
    if (!fs::file_exists(p_)) cli::cli_abort("04A's store {.file {p_}} does not exist.")
    con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(p_), read_only = TRUE)
    on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
    tabs_ <- setdiff(DBI::dbListTables(con_), meta_)
    purrr::map_dfr(tabs_, \(.t) {
      cols_ <- DBI::dbListFields(con_, .t)
      if (!"DocID" %in% cols_) return(NULL)
      by_model_ <- "Model" %in% cols_
      sql_ <- if (by_model_) {
        sprintf("SELECT Model, COUNT(*) AS Spans, COUNT(DISTINCT DocID) AS Docs FROM \"%s\" GROUP BY Model", .t)
      } else {
        sprintf("SELECT '%s' AS Model, COUNT(*) AS Spans, COUNT(DISTINCT DocID) AS Docs FROM \"%s\"", .family, .t)
      }
      DBI::dbGetQuery(con_, sql_) |>
        tibble::as_tibble() |>
        dplyr::mutate(Family = .family, Entity = toupper(.t), .before = 1L)
    })
  }
  purrr::map_dfr(c("lexnlp", "spacy", "matcon"), read_family_) |>
    dplyr::mutate(
      Spans    = as.integer(.data$Spans),
      Docs     = as.integer(.data$Docs),
      Coverage = .data$Docs / n_docs_,
      nDocs    = n_docs_
    ) |>
    dplyr::arrange(.data$Family, .data$Model, .data$Entity)
}

#' Build the extractor-coverage table
#'
#' Rows are the entities the chapter discusses, in the order it discusses them; columns are the three families, spaCy
#' represented by the transformer model 04A ran. A dash marks an entity a family does not attempt.
#'
#' @param .dir_store Character. 04A's Output.
#' @param .path_sample Character. 04A's sample_text.parquet.
#' @param .spacy_model Character. The spaCy model to print, "en_core_web_trf".
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_coverage <- function(.dir_store, .path_sample, .spacy_model, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
    .spacy_model <- "en_core_web_trf"
    .dir_own     <- here::here("2_output", "31-FinalExhibits", "Output")
    .force       <- FALSE
    .path_lib    <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "EntityCoverage"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(fs::path(.dir_store, paste0(c("lexnlp", "spacy", "matcon"), ".duckdb")), .path_lib)
  if (!all(fs::file_exists(ins_[1:3]))) return(invisible("waiting for 04A's stores"))
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oad_data_coverage(
    .dir_store   = .dir_store,
    .path_sample = .path_sample
  )
  ent_ <- tibble::tribble(
    ~Entity,  ~Label,
    "ORG",    "Organisations",
    "PERSON", "Persons",
    "GPE",    "Places",
    "DATE",   "Dates",
    "TERM",   "Stated periods",
    "MONEY",  "Monetary amounts",
    "REDACT", "Redaction markers",
    "LAW",    "Governing-law clauses"
  )
  fam_ <- c("lexnlp", "spacy", "matcon")
  pick_ <- tab_ |>
    dplyr::filter(.data$Family != "spacy" | .data$Model == .spacy_model) |>
    dplyr::summarise(Spans = sum(.data$Spans), Docs = max(.data$Docs), Coverage = max(.data$Coverage),
                     .by = c("Family", "Entity"))
  cell_ <- function(.f, .e) {
    r_ <- pick_[pick_$Family == .f & pick_$Entity == .e, , drop = FALSE]
    if (nrow(r_) == 0L) return(c("--", "--"))
    c(format(r_$Spans, big.mark = ",", trim = TRUE), formatC(100 * r_$Coverage, format = "f", digits = 0L))
  }
  cells_ <- purrr::map_dfr(seq_len(nrow(ent_)), \(.i) {
    v_ <- unlist(purrr::map(fam_, \(.f) cell_(.f = .f, .e = ent_$Entity[.i])))
    tibble::tibble(Entity = ent_$Label[.i], L1 = v_[1], L2 = v_[2], S1 = v_[3], S2 = v_[4], M1 = v_[5], M2 = v_[6])
  })
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("", "LexNLP", "\\%", "spaCy", "\\%", "Patterns", "\\%"),
      .spec   = c(oa_col_text(.share = 0.28), rep(oa_col_num(.mm = 17), 6L))
    ),
    .note    = paste(
      "What each extractor proposed on the", format(tab_$nDocs[1], big.mark = ","), "contracts of the labelled",
      "sample: LexNLP, spaCy (its transformer model) and the pattern and gazetteer extractors written for the",
      "database (Patterns). Under each, the number of text spans proposed and the share of contracts",
      "in which the extractor found at least one. A dash marks an entity the extractor does not attempt. Coverage is",
      "not a quality measure: an extractor that tags every capitalised word reaches complete coverage."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# -- 27.3 Where each contract's end date comes from ------------------------------------------------------------------------

#' Where each contract's end date comes from, and what the two durations look like
#'
#' The cascade answers from the first of four sources present, so the rungs partition the sample: a stated term, an
#' open-ended clause, which establishes that there is no end date, a future date beside a termination cue, and the
#' farthest future date. The naive measure uses the last of these alone. Both durations run from the same start, so
#' they differ only in the end they take, and the quartiles below are over the contracts each measure is defined on.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Rung, Label, N, Share, plus the quartiles of the rule-based and the naive duration.
oad_data_duration <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  con_ <- oad_read_sample(
    .path_contracts = .path_contracts,
    .cols           = c("DurationSource", "DurationDropped", "DurationYears", "NaiveYears")
  )
  rungs_ <- tibble::tribble(
    ~Rung,     ~Label,
    "term",    "A stated term",
    "open",    "An open-ended clause",
    "cue",     "A future date beside a termination cue",
    "maxdate", "The farthest future date",
    "none",    "No end established"
  )
  q_ <- function(.x, .p) {
    x_ <- .x[!is.na(.x)]
    if (length(x_) == 0L) return(NA_real_)
    unname(stats::quantile(x_, probs = .p, type = 7L))
  }
  n_all_ <- nrow(con_)
  by_rung_ <- con_ |>
    dplyr::mutate(Rung = dplyr::coalesce(.data$DurationSource, "none")) |>
    dplyr::summarise(
      N       = dplyr::n(),
      Defined = sum(!is.na(.data$DurationYears)),
      Q1      = q_(.x = .data$DurationYears, .p = 0.25),
      Med     = q_(.x = .data$DurationYears, .p = 0.50),
      Q3      = q_(.x = .data$DurationYears, .p = 0.75),
      .by     = "Rung"
    )
  out_ <- rungs_ |>
    dplyr::left_join(by_rung_, by = "Rung") |>
    dplyr::mutate(dplyr::across(c("N", "Defined"), \(.x) dplyr::coalesce(.x, 0L))) |>
    dplyr::mutate(Kind = "rung")
  # THE TWO MEASURES, each over the contracts it is defined on, so the comparison is of what each one yields
  totals_ <- tibble::tibble(
    Rung    = c("all-rule", "all-naive"),
    Label   = c("Rule-based duration", "Naive duration"),
    N       = c(n_all_, n_all_),
    Defined = c(sum(!is.na(con_$DurationYears)), sum(!is.na(con_$NaiveYears))),
    Q1      = c(q_(.x = con_$DurationYears, .p = 0.25), q_(.x = con_$NaiveYears, .p = 0.25)),
    Med     = c(q_(.x = con_$DurationYears, .p = 0.50), q_(.x = con_$NaiveYears, .p = 0.50)),
    Q3      = c(q_(.x = con_$DurationYears, .p = 0.75), q_(.x = con_$NaiveYears, .p = 0.75)),
    Kind    = "measure"
  )
  # A COMPLETE ACCOUNT OF WHAT IS MISSING. The panel is built over the contracts that have no duration, so its rows
  # sum to exactly that number; a contract whose reason the release does not record is a row of its own rather than
  # a silent remainder.
  dropped_ <- con_ |>
    dplyr::filter(is.na(.data$DurationYears)) |>
    dplyr::mutate(Reason = dplyr::coalesce(.data$DurationDropped, "unrecorded")) |>
    dplyr::mutate(Reason = dplyr::if_else(.data$Reason == "kept", "unrecorded", .data$Reason)) |>
    dplyr::count(.data$Reason, name = "N") |>
    dplyr::transmute(
      Rung    = paste0("dropped-", .data$Reason),
      Label   = dplyr::case_match(
        .data$Reason,
        "capped"     ~ "Longer than thirty years, dropped",
        "negative"   ~ "The end precedes the start, dropped",
        "no end"     ~ "No end date established",
        "unrecorded" ~ "No reason recorded",
        .default     = .data$Reason
      ),
      N       = .data$N,
      Defined = 0L,
      Q1      = NA_real_,
      Med     = NA_real_,
      Q3      = NA_real_,
      Kind    = "dropped"
    ) |>
    dplyr::arrange(dplyr::desc(.data$N))
  dplyr::bind_rows(out_, dropped_, totals_) |>
    dplyr::mutate(Share = .data$N / n_all_, Contracts = n_all_) |>
    dplyr::select("Rung", "Label", "Kind", "N", "Share", "Defined", "Q1", "Med", "Q3", "Contracts")
}

#' Build the table of duration sources
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_duration <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "DurationRungs"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = c(.path_contracts, .path_lib), .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oad_data_duration(.path_contracts = .path_contracts)
  num_ <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)
  yrs_ <- function(.x) dplyr::if_else(is.na(.x), "--", formatC(.x, format = "f", digits = 1L))
  panel_ <- c(rung = "Which rung answered", dropped = "Why a duration is missing",
              measure = "The two measures, over the contracts each is defined on")
  cells_ <- tab_ |>
    dplyr::transmute(
      Panel   = unname(panel_[.data$Kind]),
      Label   = oa_tex_escape(.x = .data$Label),
      N       = num_(.x = .data$N),
      Share   = paste0(formatC(100 * .data$Share, format = "f", digits = 1L), "\\%"),
      Defined = dplyr::if_else(.data$Kind == "dropped", "--", num_(.x = .data$Defined)),
      Q1      = yrs_(.x = .data$Q1),
      Med     = yrs_(.x = .data$Med),
      Q3      = yrs_(.x = .data$Q3)
    )
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("Source", "Contracts", "Share", "Defined", "25th", "Median", "75th"),
      .spec   = c(oa_col_text(.share = 0.25), oa_col_num(.mm = 16), oa_col_num(.mm = 12),
                  oa_col_num(.mm = 15), oa_col_num(.mm = 11), oa_col_num(.mm = 13), oa_col_num(.mm = 11))
    ),
    .note    = paste(
      "The unique contracts of the descriptive sample. The cascade takes the first source present, so the rungs",
      "partition the sample: a stated term; an open-ended clause, which establishes that the contract has no end",
      "date and therefore no duration; a future date within a termination cue's reach; and the farthest future date.",
      "Defined counts the contracts of the row for which a duration could be computed, and the quartiles are in",
      "years, over those contracts. Shares are of all contracts throughout, so the second panel accounts for every",
      "contract that lacks a duration and the first for every contract.",
      "The naive duration takes the farthest future date for every contract, and both",
      "measures run from the same start date, so they differ only in the end they take."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' The counts the text of Appendix A cites, which no table prints
#'
#' Three passages name numbers that belong to no exhibit: what each quality rule flags, what the confidential
#' treatment orders cover, and how many Item 1.01 announcements the sample keeps. They are computed here, written as
#' a tibble like any other exhibit's data, and read by the numbers spec; nothing is printed.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .path_orders Character. The release's CtoOrders.parquet, one row per reference an order makes.
#' @return Tibble: Key, Value.


# -- 27.4 Where candidates fall, and how far the extractors agree: computed from 04A's stores ------------------------------
# 04A reports positions, contrast, consensus and pairwise agreement in its runbook and saves none of them. They are
# recomputed here from the same stores, with the same definitions: every span binned by its position in its document;
# contrast as the share in the first and last decile over the share through the middle deciles; overlapping spans of
# one entity in one document merged into a mention, whatever produced them; consensus as the number of producers that
# found each mention; agreement as the Jaccard index over mentions, and exact agreement as the share of mentions both
# found whose boundaries coincide.

#' Every span of the entities compared, from the three stores, as one table in DuckDB
#'
#' Opens the three stores read-only in one connection, checks that each entity table carries a document key and
#' half-open offsets, and unions the spans of the entities asked for into a temporary table on the connection:
#' Producer, Entity, DocID, Start, Stop. spaCy is represented by one model. The connection is returned so that the
#' callers can run their SQL on the table; they close it.
#'
#' @param .dir_store Character. 04A's Output, holding matcon.duckdb, spacy.duckdb and lexnlp.duckdb.
#' @param .spacy_model Character. The spaCy model to include.
#' @param .entities Character. The entity tables to read, upper case.
#' @return A DBI connection holding the temporary table "spans".
oad_spans_open <- function(.dir_store, .spacy_model, .entities) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .spacy_model <- "en_core_web_trf"
    .entities    <- c("ORG", "PERSON", "GPE", "LAW", "DATE", "TERM", "MONEY", "REDACT")
  }
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  fam_ <- c("lexnlp", "spacy", "matcon")
  for (f_ in fam_) {
    p_ <- fs::path(.dir_store, paste0(f_, ".duckdb"))
    if (!fs::file_exists(p_)) {
      DBI::dbDisconnect(con_, shutdown = TRUE)
      cli::cli_abort("04A's store {.file {p_}} does not exist.")
    }
    DBI::dbExecute(con_, sprintf("ATTACH '%s' AS %s (READ_ONLY)", as.character(p_), f_))
  }
  parts_ <- character(0)
  for (f_ in fam_) {
    tabs_ <- DBI::dbGetQuery(con_, sprintf(
      "SELECT table_name FROM information_schema.tables WHERE table_catalog = '%s' AND table_schema = 'main'", f_
    ))$table_name
    for (e_ in .entities) {
      t_ <- tolower(e_)
      if (!t_ %in% tabs_) next
      cols_ <- DBI::dbListFields(con_, DBI::Id(catalog = f_, schema = "main", table = t_))
      need_ <- setdiff(c("DocID", "Start", "Stop"), cols_)
      if (length(need_) > 0L) {
        DBI::dbDisconnect(con_, shutdown = TRUE)
        cli::cli_abort("{f_}.{t_} lacks {.field {need_}}; it has {.field {cols_}}.")
      }
      prod_  <- if (f_ == "spacy") paste0("spacy:", sub("^en_core_web_", "", .spacy_model)) else f_
      where_ <- if (f_ == "spacy" && "Model" %in% cols_) sprintf(" WHERE Model = '%s'", .spacy_model) else ""
      parts_ <- c(parts_, sprintf(
        "SELECT '%s' AS Producer, '%s' AS Entity, DocID, CAST(Start AS BIGINT) AS Start, CAST(Stop AS BIGINT) AS Stop
         FROM %s.main.%s%s", prod_, e_, f_, t_, where_
      ))
    }
  }
  if (length(parts_) == 0L) {
    DBI::dbDisconnect(con_, shutdown = TRUE)
    cli::cli_abort("None of the entity tables asked for exists in the three stores.")
  }
  DBI::dbExecute(con_, paste("CREATE TEMP TABLE spans AS", paste(parts_, collapse = " UNION ALL ")))
  con_
}

#' Positions and contrast: where in its document each candidate falls, per producer and entity
#'
#' @param .con A connection from oad_spans_open().
#' @param .path_sample Character. 04A's sample_text.parquet, for the length of every document.
#' @param .bins Integer. Bins per document, a multiple of ten.
#' @return List of two tibbles: positions (Producer, Entity, Bin, N, Share) and contrast (Producer, Entity,
#'   MidShare, EndShare, Contrast).
oad_data_positions <- function(.con, .path_sample, .bins = 30L) {
  if (FALSE) {
    .con         <- oad_spans_open(here::here("2_output", "04A-EntityExtract", "Output"), "en_core_web_trf", "ORG")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
    .bins        <- 30L
  }
  # THE TEXT EVERY OFFSET INDEXES: DocID and TextRaw. Its length in code points is what the offsets are relative to.
  len_ <- arrow::open_dataset(.path_sample) |>
    dplyr::select("DocID", "TextRaw") |>
    dplyr::collect() |>
    dplyr::transmute(DocID = .data$DocID, Length = nchar(.data$TextRaw, type = "chars"))
  DBI::dbWriteTable(.con, "doclen", as.data.frame(len_), temporary = TRUE, overwrite = TRUE)
  pos_ <- DBI::dbGetQuery(.con, sprintf(
    "SELECT s.Producer, s.Entity,
            LEAST(%d, 1 + CAST(FLOOR(%d * s.Start / GREATEST(d.Length, 1)) AS INTEGER)) AS Bin,
            COUNT(*) AS N
     FROM spans s JOIN doclen d USING (DocID)
     GROUP BY 1, 2, 3", .bins, .bins
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(N = as.integer(.data$N)) |>
    tidyr::complete(tidyr::nesting(Producer, Entity), Bin = seq_len(.bins), fill = list(N = 0L)) |>
    dplyr::mutate(Share = .data$N / sum(.data$N), .by = c("Producer", "Entity")) |>
    dplyr::arrange(.data$Producer, .data$Entity, .data$Bin)
  # CONTRAST: the first and last decile against the middle, the second and ninth deciles left out as shoulders
  dec_ <- .bins / 10L
  con_ <- pos_ |>
    dplyr::mutate(Zone = dplyr::case_when(
      .data$Bin <= dec_ | .data$Bin > .bins - dec_ ~ "end",
      .data$Bin <= 2L * dec_ | .data$Bin > .bins - 2L * dec_ ~ "shoulder",
      .default = "mid"
    )) |>
    dplyr::filter(.data$Zone != "shoulder") |>
    dplyr::summarise(Share = mean(.data$Share), .by = c("Producer", "Entity", "Zone")) |>
    tidyr::pivot_wider(names_from = "Zone", values_from = "Share") |>
    dplyr::transmute(Producer = .data$Producer, Entity = .data$Entity, MidShare = .data$mid, EndShare = .data$end,
                     Contrast = .data$end / .data$mid)
  list(positions = pos_, contrast = con_)
}

#' Consensus and pairwise agreement over mentions
#'
#' Overlapping spans of one entity in one document, from any producer, are merged into a mention by gaps and
#' islands; a mention's producers are whoever contributed a span to it. Consensus counts mentions by how many
#' producers found them. Pairwise agreement, for every pair of producers that attempt an entity, is the Jaccard
#' index over mentions -- both over either -- and exact agreement is the share of mentions both found on which
#' their outermost boundaries coincide.
#'
#' @param .con A connection from oad_spans_open().
#' @return List of two tibbles: consensus (Entity, NProducer, NMentions, Share, Eligible) and pairwise (Entity,
#'   ProducerA, ProducerB, Both, Exact, MentionsA, MentionsB, Jaccard, ExactShare).
oad_data_agreement <- function(.con) {
  if (FALSE) .con <- oad_spans_open(here::here("2_output", "04A-EntityExtract", "Output"), "en_core_web_trf", "GPE")
  DBI::dbExecute(.con, "
    CREATE OR REPLACE TEMP TABLE mentions AS
    WITH o AS (
      SELECT *, MAX(Stop) OVER (PARTITION BY DocID, Entity ORDER BY Start, Stop
                                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS PrevMax
      FROM spans
    ),
    g AS (
      SELECT *, CASE WHEN PrevMax IS NULL OR Start >= PrevMax THEN 1 ELSE 0 END AS NewGroup FROM o
    ),
    m AS (
      SELECT *, SUM(NewGroup) OVER (PARTITION BY DocID, Entity ORDER BY Start, Stop
                                    ROWS UNBOUNDED PRECEDING) AS MentionID
      FROM g
    )
    SELECT DocID, Entity, MentionID, Producer, MIN(Start) AS Start, MAX(Stop) AS Stop
    FROM m GROUP BY 1, 2, 3, 4
  ")
  cons_ <- DBI::dbGetQuery(.con, "
    WITH per AS (SELECT Entity, DocID, MentionID, COUNT(DISTINCT Producer) AS NProducer
                 FROM mentions GROUP BY 1, 2, 3),
         elig AS (SELECT Entity, COUNT(DISTINCT Producer) AS Eligible FROM spans GROUP BY 1)
    SELECT p.Entity, p.NProducer, COUNT(*) AS NMentions, e.Eligible
    FROM per p JOIN elig e USING (Entity) GROUP BY 1, 2, 4 ORDER BY 1, 2
  ") |>
    tibble::as_tibble() |>
    dplyr::mutate(NMentions = as.integer(.data$NMentions), Eligible = as.integer(.data$Eligible)) |>
    dplyr::mutate(Share = .data$NMentions / sum(.data$NMentions), .by = "Entity")
  pair_ <- DBI::dbGetQuery(.con, "
    WITH prods AS (SELECT DISTINCT Entity, Producer FROM spans),
         pairs AS (SELECT a.Entity, a.Producer AS A, b.Producer AS B
                   FROM prods a JOIN prods b ON a.Entity = b.Entity AND a.Producer < b.Producer),
         cnt AS (SELECT Entity, Producer, COUNT(*) AS N FROM mentions GROUP BY 1, 2),
         overlap AS (SELECT x.Entity, x.Producer AS A, y.Producer AS B,
                         COUNT(*) AS Both,
                         SUM(CASE WHEN x.Start = y.Start AND x.Stop = y.Stop THEN 1 ELSE 0 END) AS Exact
                  FROM mentions x JOIN mentions y
                    ON x.Entity = y.Entity AND x.DocID = y.DocID AND x.MentionID = y.MentionID
                   AND x.Producer < y.Producer
                  GROUP BY 1, 2, 3)
    SELECT p.Entity, p.A AS ProducerA, p.B AS ProducerB,
           COALESCE(b.Both, 0) AS Both, COALESCE(b.Exact, 0) AS Exact,
           ca.N AS MentionsA, cb.N AS MentionsB
    FROM pairs p
    LEFT JOIN overlap b ON b.Entity = p.Entity AND b.A = p.A AND b.B = p.B
    JOIN cnt ca ON ca.Entity = p.Entity AND ca.Producer = p.A
    JOIN cnt cb ON cb.Entity = p.Entity AND cb.Producer = p.B
    ORDER BY 1, 2, 3
  ") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(c("Both", "Exact", "MentionsA", "MentionsB"), as.integer),
      Jaccard    = .data$Both / (.data$MentionsA + .data$MentionsB - .data$Both),
      ExactShare = dplyr::if_else(.data$Both > 0L, .data$Exact / .data$Both, NA_real_)
    )
  list(consensus = cons_, pairwise = pair_)
}

#' The positions figure: share of candidates by position in the document, one panel per entity, one line per producer
#'
#' @param .tab Tibble: Producer, Entity, Bin, Share.
#' @return A ggplot.
oad_plot_positions <- function(.tab) {
  if (FALSE) {
    .tab <- tibble::tibble(Producer = "lexnlp", Entity = "ORG", Bin = 1:30, Share = rep(1 / 30, 30))
  }
  ent_ <- c("ORG", "PERSON", "GPE", "LAW", "DATE", "TERM", "MONEY", "REDACT")
  lab_ <- c(ORG = "Organisations", PERSON = "Persons", GPE = "Places", LAW = "Governing law", DATE = "Dates",
            TERM = "Stated periods", MONEY = "Amounts", REDACT = "Redaction markers")
  prod_ <- c("lexnlp", "spacy:trf", "matcon")
  plab_ <- c(lexnlp = "LexNLP", `spacy:trf` = "spaCy", matcon = "Patterns")
  tab_ <- .tab |>
    dplyr::filter(.data$Entity %in% ent_, .data$Producer %in% prod_) |>
    dplyr::mutate(
      Entity   = factor(unname(lab_[.data$Entity]), levels = unname(lab_)),
      Producer = factor(unname(plab_[.data$Producer]), levels = unname(plab_)),
      Position = (.data$Bin - 0.5) / max(.data$Bin)
    )
  ggplot2::ggplot(tab_, ggplot2::aes(x = .data$Position, y = .data$Share, colour = .data$Producer)) +
    ggplot2::geom_line(linewidth = 0.45) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Entity), ncol = 4L, scales = "free_y") +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks = c(0.25, 0.5, 0.75)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_colour_manual(values = plot_pal_cat(.n = 3L), drop = FALSE) +
    ggplot2::labs(x = "Position in the contract", y = "Share of the producer's candidates", colour = NULL) +
    plot_theme(.grid = "y", .legend = "bottom") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

#' The agreement figure: pairwise agreement over mentions, one panel per entity two producers reach
#'
#' @param .tab Tibble: Entity, ProducerA, ProducerB, Jaccard.
#' @return A ggplot.
oad_plot_agreement <- function(.tab) {
  if (FALSE) .tab <- tibble::tibble(Entity = "GPE", ProducerA = "lexnlp", ProducerB = "matcon", Jaccard = 0.66)
  plab_ <- c(lexnlp = "LexNLP", `spacy:trf` = "spaCy", matcon = "Patterns")
  lab_  <- c(ORG = "Organisations", GPE = "Places", DATE = "Dates", MONEY = "Amounts")
  lvl_  <- unname(plab_)
  pairs_ <- .tab |>
    dplyr::filter(.data$Entity %in% names(lab_)) |>
    dplyr::transmute(Entity = .data$Entity, A = unname(plab_[.data$ProducerA]), B = unname(plab_[.data$ProducerB]),
                     Jaccard = .data$Jaccard)
  diag_ <- pairs_ |>
    dplyr::select("Entity", "A", "B") |>
    tidyr::pivot_longer(cols = c("A", "B"), values_to = "P") |>
    dplyr::distinct(.data$Entity, .data$P) |>
    dplyr::transmute(Entity = .data$Entity, A = .data$P, B = .data$P, Jaccard = 1)
  cells_ <- dplyr::bind_rows(pairs_, diag_) |>
    dplyr::mutate(
      A      = factor(.data$A, levels = lvl_),
      B      = factor(.data$B, levels = rev(lvl_)),
      Entity = factor(unname(lab_[.data$Entity]), levels = unname(lab_)),
      Label  = formatC(.data$Jaccard, format = "f", digits = 2L),
      Dark   = .data$Jaccard > 0.5
    )
  ggplot2::ggplot(cells_, ggplot2::aes(x = .data$A, y = .data$B, fill = .data$Jaccard)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Label, colour = .data$Dark), family = .plot_font, size = 3.2,
                       show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    ggplot2::scale_fill_gradient(low = "#EEF0F4", high = plot_pal_cat(.n = 1L), limits = c(0, 1)) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Entity), ncol = 2L, scales = "free") +
    ggplot2::labs(x = NULL, y = NULL, fill = "Agreement") +
    plot_theme(.grid = "none", .legend = "right") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Build the positions and the agreement figures together, from one pass over the stores
#'
#' One connection, one spans table, both computations; written as two exhibits. The build is keyed on the three
#' stores and this library.
#'
#' @param .dir_store Character. 04A's Output, holding the three stores.
#' @param .path_sample Character. 04A's sample_text.parquet.
#' @param .spacy_model Character. The spaCy model compared.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_alignment <- function(.dir_store, .path_sample, .spacy_model, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
    .spacy_model <- "en_core_web_trf"
    .dir_own     <- here::here("2_output", "31-FinalExhibits", "Output")
    .force       <- FALSE
    .path_lib    <- here::here("1_code", "31-FinalExhibits.R")
  }
  names_ <- c("EntityPositions", "EntityAgreement")
  outs_  <- unlist(purrr::map(names_, \(.n) c(
    fs::path(.dir_own, "Figures", paste0(.n, ".png")),
    fs::path(.dir_own, c("Notes", "Data"), paste0(.n, c(".tex", ".parquet")))
  )))
  ins_   <- c(fs::path(.dir_store, paste0(c("lexnlp", "spacy", "matcon"), ".duckdb")), .path_sample, .path_lib)
  if (!all(fs::file_exists(ins_[1:4]))) return(invisible("waiting for 04A's stores"))
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  con_ <- oad_spans_open(
    .dir_store   = .dir_store,
    .spacy_model = .spacy_model,
    .entities    = c("ORG", "PERSON", "GPE", "LAW", "DATE", "TERM", "MONEY", "REDACT")
  )
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
  pos_ <- oad_data_positions(.con = con_, .path_sample = .path_sample)
  agr_ <- oad_data_agreement(.con = con_)
  oa_write_exhibit(
    .name    = names_[1L],
    .lines   = NULL,
    .note    = paste(
      "Every candidate span each extractor proposed on the labelled sample, by its position in the contract: the",
      "contract is divided into thirty bins of equal length, and each line is the share of the producer's candidates",
      "for that entity that fall in each bin. LexNLP, spaCy (its transformer model) and the pattern and gazetteer",
      "extractors written for the database (Patterns); an extractor absent from a panel does not attempt that",
      "entity. A producer that spreads an entity evenly through the text draws a flat line."
    ),
    .data    = dplyr::left_join(pos_$positions, pos_$contrast, by = c("Producer", "Entity")),
    .dir_own = .dir_own
  )
  plot_save(
    .plot    = oad_plot_positions(.tab = pos_$positions),
    .name    = names_[1L],
    .dir     = fs::path(.dir_own, "Figures"),
    .height  = 4.6,
    .formats = "png"            # no pdf (decided 17 September)
  )
  oa_write_exhibit(
    .name    = names_[2L],
    .lines   = NULL,
    .note    = paste(
      "Agreement between extractors on the labelled sample, for the entities two of them attempt. Overlapping spans",
      "of one entity within a contract are merged into a mention -- a place in the text where something was found",
      "-- and agreement is the Jaccard index over mentions: the share of mentions both producers found among those",
      "either found. It measures agreement about what is there; agreement about where a mention ends is a separate",
      "quantity and is not shown. The diagonal is a producer against itself."
    ),
    .data    = dplyr::bind_rows(dplyr::mutate(agr_$pairwise, Block = "pairwise"),
                                dplyr::mutate(agr_$consensus, Block = "consensus")),
    .dir_own = .dir_own
  )
  plot_save(
    .plot    = oad_plot_agreement(.tab = agr_$pairwise),
    .name    = names_[2L],
    .dir     = fs::path(.dir_own, "Figures"),
    .height  = 4.4,
    .formats = "png"            # no pdf (decided 17 September)
  )
  invisible("built")
}


# -- 27.5 What the variables are worth under the naive and the rule-based reading ------------------------------------------

#' The values companion's data: by category, the typical value of each measure under both readings
#'
#' 30's contrast table counts on how many contracts each measure is defined; this one reports what it is worth on
#' them. Duration: the median years under the naive end (the farthest future date) and under the cascade. Parties:
#' the mean number of distinct organisation spellings (naive) and of registrants, co-registrants and counterparties
#' (rule). Countries and states: the mean number under the naive count (any mention outside a governing-law clause)
#' and attached to the registrant and to the counterparties by the 200-character rule, the two reported apart
#' because a country attached to both is one country. Amounts: the mean number of distinct figures read (naive)
#' and kept after the zero and par-value filters, in U.S. dollars.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Row, Kind, Level1, Class, N, and one column per measure and reading.
oad_data_values <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  cols_ <- c("Class", "DurationYears", "NaiveYears", "nUniSpellingsNaive", "nUniRegistrant",
             "nUniCofiler", "nUniCounterparty", "nUniCountryNaive", "nUniCountryRegistrant",
             "nUniCountryCounterparty", "nUniStateNaive", "nUniStateRegistrant", "nUniStateCounterparty",
             "nUniAmountNaive", "nUniAmountUSD")
  con_ <- oad_read_sample(
    .path_contracts = .path_contracts,
    .cols           = cols_
  ) |>
    dplyr::mutate(
      # THE TWO LEVELS, from the release's Class: "Employment: Compensation" is parent and sub; "Licenses" is both;
      # two Purchases-and-Sales sub-categories are released without their parent and are put back under it.
      Level2 = dplyr::if_else(grepl(": ", .data$Class, fixed = TRUE), sub("^.*: ", "", .data$Class), .data$Class),
      Level2 = dplyr::if_else(.data$Level2 == "Investment and Merger", "M&A", .data$Level2),
      Level1 = dplyr::case_when(
        grepl(": ", .data$Class, fixed = TRUE)            ~ sub(": .*$", "", .data$Class),
        .data$Class %in% c("R&D", "Customer / Supplier") ~ "Purchases and Sales",
        .default                                          = .data$Class
      ),
      PartiesRule = dplyr::coalesce(.data$nUniRegistrant, 0L) + dplyr::coalesce(.data$nUniCofiler, 0L) +
        dplyr::coalesce(.data$nUniCounterparty, 0L)
    )
  stat_ <- function(.d) {
    tibble::tibble(
      N            = nrow(.d),
      DurNaive     = stats::median(.d$NaiveYears, na.rm = TRUE),
      DurRule      = stats::median(.d$DurationYears, na.rm = TRUE),
      PartNaive    = mean(.d$nUniSpellingsNaive, na.rm = TRUE),
      PartRule     = mean(.d$PartiesRule, na.rm = TRUE),
      CtryNaive    = mean(.d$nUniCountryNaive, na.rm = TRUE),
      CtryReg      = mean(.d$nUniCountryRegistrant, na.rm = TRUE),
      CtryCpty     = mean(.d$nUniCountryCounterparty, na.rm = TRUE),
      StateNaive   = mean(.d$nUniStateNaive, na.rm = TRUE),
      StateReg     = mean(.d$nUniStateRegistrant, na.rm = TRUE),
      StateCpty    = mean(.d$nUniStateCounterparty, na.rm = TRUE),
      AmtNaive     = mean(.d$nUniAmountNaive, na.rm = TRUE),
      AmtRule      = mean(.d$nUniAmountUSD, na.rm = TRUE)
    )
  }
  lab_ <- dplyr::filter(con_, !is.na(.data$Class))
  # THE ROWS: parents with their sub-categories, leaf parents alone, then the total, in taxonomic order
  l1_ <- c("Financial Instruments", "Employment", "Purchases and Sales", "Licenses", "Leases", "Business Structure",
           "Other")
  rows_ <- purrr::map_dfr(l1_, \(.p) {
    d1_ <- dplyr::filter(lab_, .data$Level1 == .p)
    if (nrow(d1_) == 0L) return(NULL)
    order_ <- c("Credit", "Equity", "Compensation", "Legal", "Assets", "R&D", "Customer / Supplier",
                "Peer Agreements", "M&A")
    subs_ <- unique(d1_$Level2[!is.na(d1_$Level2) & d1_$Level2 != .p])
    subs_ <- subs_[order(match(subs_, order_))]
    top_ <- dplyr::bind_cols(tibble::tibble(Row = .p, Kind = "super", Level1 = .p, Class = NA_character_),
                             stat_(.d = d1_))
    if (length(subs_) == 0L) return(top_)
    sub_ <- purrr::map_dfr(subs_, \(.s) {
      d2_ <- dplyr::filter(d1_, .data$Level2 == .s)
      dplyr::bind_cols(tibble::tibble(Row = .s, Kind = "sub", Level1 = .p, Class = d2_$Class[1]), stat_(.d = d2_))
    })
    dplyr::bind_rows(top_, sub_)
  })
  dplyr::bind_rows(
    rows_,
    dplyr::bind_cols(tibble::tibble(Row = "Total", Kind = "total", Level1 = NA_character_, Class = NA_character_),
                     stat_(.d = con_))
  )
}

#' Build the values companion
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_values <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "31-FinalExhibits", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "31-FinalExhibits.R")
  }
  name_ <- "ContentValues"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = c(.path_contracts, .path_lib), .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oad_data_values(.path_contracts = .path_contracts)
  f1_ <- function(.x) formatC(.x, format = "f", digits = 1L)
  f2_ <- function(.x) formatC(.x, format = "f", digits = 2L)
  lab_ <- dplyr::case_when(
    tab_$Kind == "total" ~ paste0("\\textbf{", oa_tex_escape(.x = tab_$Row), "}"),
    tab_$Kind == "sub"   ~ paste0("\\hspace{1em}", oa_tex_escape(.x = tab_$Row)),
    .default             = paste0("\\textbf{", oa_tex_escape(.x = tab_$Row), "}")
  )
  cells_ <- tibble::tibble(
    Row        = lab_,
    DurNaive   = f1_(.x = tab_$DurNaive),
    DurRule    = f1_(.x = tab_$DurRule),
    PartNaive  = f2_(.x = tab_$PartNaive),
    PartRule   = f2_(.x = tab_$PartRule),
    CtryNaive  = f2_(.x = tab_$CtryNaive),
    CtryReg    = f2_(.x = tab_$CtryReg),
    CtryCpty   = f2_(.x = tab_$CtryCpty),
    StateNaive = f2_(.x = tab_$StateNaive),
    StateReg   = f2_(.x = tab_$StateReg),
    StateCpty  = f2_(.x = tab_$StateCpty),
    AmtNaive   = f2_(.x = tab_$AmtNaive),
    AmtRule    = f2_(.x = tab_$AmtRule)
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = fin_tex_frame(
      .name   = name_,
      .tab    = cells_,
      .header = c("", "Naive", "Rule", "Naive", "Rule", "Naive", "Reg.", "Cpty.", "Naive", "Reg.", "Cpty.",
                  "Naive", "Rule"),
      .spec   = c(oa_col_text(.share = 0.20), rep(oa_col_num(.mm = 10), 12L))
    ),
    .note    = paste(
      "The companion to the coverage contrast: what each measure is worth, by category, on the unique contracts of",
      "the descriptive sample, under the naive reading of the spans and under the rule. Duration (years) is the",
      "median over the contracts on which each reading defines it: the naive end is the farthest future date, the",
      "rule's end the first source present of a stated term, an open-ended clause, a cued date and the farthest",
      "date, dropped above thirty years. Parties is the mean number per contract of distinct organisation spellings",
      "(naive) and of registrants, co-registrants and counterparties (rule). Countries and states are the mean",
      "number per contract of distinct mentions outside a governing-law clause (naive) and of those attached to",
      "the registrant (Reg.) and to the counterparties (Cpty.) by the 200-character rule, reported apart because a",
      "place attached to both is one place. Amounts is the mean number of distinct figures read with a currency",
      "marker (naive) and kept in U.S. dollars after the zero and par-value filters (rule). The column groups are,",
      "from left to right, duration, parties, countries, states and amounts."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# 28. The store: showing what the moved builders wrote, and checking the move ------------------------------------------
# The builders moved from 40 write files and return a status; they print nothing. fin_show_built() shows what they
# wrote -- the figure, the table as html, or the head of a data-only tibble, each with its note -- so this runbook
# holds every exhibit. fin_check_migration() compares this document's files with the ones 30 and 40 wrote, stem by
# stem, while those two still write theirs; it reads their output directories and goes when 31 moves back to 30.

#' Show the exhibits a moved builder wrote to disk
#'
#' A figure is embedded from its png, a table is converted from its tex by pandoc as the appendix chapters convert
#' theirs, and a stem with neither shows the head of its tibble. The note follows, with LaTeX's escapes undone. The
#' chunk that calls it needs `results: asis`.
#'
#' @param .names Character. The stems to show, in order.
#' @param .dirs List. .lP$Output.
#' @param .rows Integer. Rows shown of a data-only tibble.
#' @return Invisibly, a tibble: Stem, Shown (figure, table, data or missing).
fin_show_built <- function(.names, .dirs, .rows = 10L) {
  if (FALSE) {
    .names <- c("FilingsWithinYear", "CtoLinkage", "CtoAhci")
    .dirs  <- .lP$Output
    .rows  <- 10L
  }
  pandoc_ <- oa_pandoc_bin()
  shown_ <- purrr::map_chr(.names, \(.n) {
    png_  <- fs::path(.dirs$DirFigures, paste0(.n, ".png"))
    tex_  <- fs::path(.dirs$DirTables, paste0(.n, ".tex"))
    note_ <- fs::path(.dirs$DirNotes, paste0(.n, ".tex"))
    data_ <- fs::path(.dirs$DirData, paste0(.n, ".parquet"))
    cat(
      "\n\n#### ",
      .n,
      "\n\n",
      sep = ""
    )
    kind_ <- "missing"
    if (fs::file_exists(png_)) {
      cat(
        "\n```{=html}\n<img src=\"",
        xfun::base64_uri(png_),
        "\" style=\"width:100%\" alt=\"",
        .n,
        "\">\n```\n\n",
        sep = ""
      )
      kind_ <- "figure"
    } else if (fs::file_exists(tex_)) {
      html_ <- oa_table_html(
        .path_tex = tex_,
        .pandoc   = pandoc_
      )$Html
      if (is.na(html_)) {
        cli::cli_alert_warning("{(.n)}: pandoc could not convert the table; the tibble is shown instead.")
        if (fs::file_exists(data_)) {
          fin_show_table(
            .tab   = utils::head(arrow::read_parquet(data_), .rows),
            .title = .n
          )
        }
      } else {
        cat(
          "\n```{=html}\n",
          html_,
          "\n```\n\n",
          sep = ""
        )
      }
      kind_ <- "table"
    } else if (fs::file_exists(data_)) {
      fin_show_table(
        .tab   = utils::head(arrow::read_parquet(data_), .rows),
        .title = .n
      )
      kind_ <- "data"
    }
    if (fs::file_exists(note_)) {
      text_ <- gsub("\\\\([%&$#_])", "\\1", oa_note_text(.path = note_))
      cat(
        "\n```{=html}\n<p class=\"table-note\"><em>Notes:</em> ",
        text_,
        "</p>\n```\n\n",
        sep = ""
      )
    }
    if (kind_ == "missing") cli::cli_alert_warning("{(.n)}: nothing under Output/ to show.")
    kind_
  })
  invisible(tibble::tibble(Stem = .names, Shown = shown_))
}

#' Compare this document's exhibit files with the ones 30 and 40 wrote
#'
#' Stem by stem, from the registry's Origin and OriginStem. A tibble is "same" (identical), "equal" (within numerical
#' tolerance), "reordered" (the same rows in another order: a query without a full ORDER BY returns tied rows in any
#' order) or "differs", with Detail saying how. A tabular and a note are compared by their bytes. A png is "redrawn"
#' where its bytes differ but its dimensions and size agree: the rasterizer's antialiasing is not reproducible byte
#' for byte, and the figure's content is its tibble and its note. "--" is a file neither side has; "new" marks a
#' stem first built in 31, with nothing to compare against. A reference
#' written from an older release differs for that reason alone, so the check reports and never stops the render.
#' The chunk that calls it needs `results: asis`.
#'
#' @param .registry Tibble. The exhibit registry, with Topic, Origin and OriginStem.
#' @param .dirs List. .lP$Output.
#' @param .refs Named character. The output directory of each origin, named "30" and "40"; origin "31" has none.
#' @param .tolerance Numeric. The relative difference in png file size still read as a redraw.
#' @return Invisibly, a tibble: Topic, Stem, Origin, Data, Tex, Note, Png, Detail.
fin_check_migration <- function(.registry = .fin_registry, .dirs, .refs, .tolerance = 0.05) {
  if (FALSE) {
    .registry  <- .fin_registry
    .dirs      <- .lP$Output
    .refs      <- .lP$Migration
    .tolerance <- 0.05
  }
  sides_ <- function(.a, .b) {
    dplyr::case_when(
      !fs::file_exists(.a) & !fs::file_exists(.b) ~ "--",
      !fs::file_exists(.a)                         ~ "missing in 31",
      !fs::file_exists(.b)                         ~ "missing in ref",
      .default                                     = NA_character_
    )
  }
  same_file_ <- function(.a, .b) {
    out_ <- unname(sides_(.a, .b))
    if (!is.na(out_)) return(out_)
    if (identical(unname(tools::md5sum(.a)), unname(tools::md5sum(.b)))) "same" else "differs"
  }
  same_png_ <- function(.a, .b) {
    out_ <- same_file_(.a, .b)
    if (out_ != "differs") return(out_)
    dims_  <- function(.p) readBin(con = .p, what = "raw", n = 24L)[17:24]   # width and height from the IHDR chunk
    ratio_ <- as.numeric(fs::file_size(.a)) / as.numeric(fs::file_size(.b))
    if (identical(dims_(.a), dims_(.b)) && abs(ratio_ - 1) < .tolerance) "redrawn" else "differs"
  }
  sort_rows_ <- function(.d) {
    keys_ <- .d[vapply(.d, is.atomic, logical(1))]
    if (ncol(keys_) == 0L) return(.d)
    out_ <- .d[do.call(order, unname(as.list(keys_))), , drop = FALSE]
    rownames(out_) <- NULL
    out_
  }
  same_data_ <- function(.a, .b) {
    out_ <- unname(sides_(.a, .b))
    if (!is.na(out_)) return(c(out_, ""))
    a_ <- as.data.frame(arrow::read_parquet(.a))
    b_ <- as.data.frame(arrow::read_parquet(.b))
    if (identical(a_, b_)) return(c("same", ""))
    if (!identical(names(a_), names(b_))) {
      return(c("differs", paste0(
        "columns only in 31: ", paste(setdiff(names(a_), names(b_)), collapse = " "),
        "; only in the reference: ", paste(setdiff(names(b_), names(a_)), collapse = " ")
      )))
    }
    if (nrow(a_) != nrow(b_)) {
      return(c("differs", paste0("rows: ", nrow(a_), " in 31, ", nrow(b_), " in the reference")))
    }
    if (isTRUE(all.equal(a_, b_, check.attributes = FALSE))) return(c("equal", "within numerical tolerance"))
    a_ <- sort_rows_(a_)
    b_ <- sort_rows_(b_)
    if (isTRUE(all.equal(a_, b_, check.attributes = FALSE))) return(c("reordered", "the same rows in another order"))
    cols_ <- names(a_)[!purrr::map2_lgl(a_, b_, \(.x, .y) isTRUE(all.equal(.x, .y, check.attributes = FALSE)))]
    c("differs", paste("values in", paste(utils::head(cols_, 5L), collapse = ", ")))
  }
  reg_ <- .registry |>
    dplyr::mutate(
      RefDir  = unname(.refs[.data$Origin]),
      RefStem = dplyr::coalesce(.data$OriginStem, .data$Stem)
    )
  cmp_ <- purrr::pmap(
    .l = list(reg_$Stem, reg_$RefDir, reg_$RefStem),
    .f = \(.s, .r, .rs) {
      if (is.na(.r)) {
        return(tibble::tibble(Data = "new", Tex = "new", Note = "new", Png = "new", Detail = "first built in 31"))
      }
      data_ <- same_data_(
        fs::path(.dirs$DirData, paste0(.s, ".parquet")),
        fs::path(.r, "Data", paste0(.rs, ".parquet"))
      )
      tibble::tibble(
        Data   = data_[1L],
        Tex    = same_file_(
          fs::path(.dirs$DirTables, paste0(.s, ".tex")),
          fs::path(.r, "Tables", paste0(.rs, ".tex"))
        ),
        Note   = same_file_(
          fs::path(.dirs$DirNotes, paste0(.s, ".tex")),
          fs::path(.r, "Notes", paste0(.rs, ".tex"))
        ),
        Png    = same_png_(
          fs::path(.dirs$DirFigures, paste0(.s, ".png")),
          fs::path(.r, "Figures", paste0(.rs, ".png"))
        ),
        Detail = data_[2L]
      )
    }
  ) |>
    purrr::list_rbind()
  out_ <- dplyr::bind_cols(
    dplyr::select(reg_, "Topic", "Stem", "Origin"),
    cmp_
  )
  cells_ <- unlist(dplyr::select(out_, "Data", "Tex", "Note", "Png"), use.names = FALSE)
  n_off_     <- sum(cells_ %in% c("differs", "missing in 31", "missing in ref"))
  n_redrawn_ <- sum(cells_ == "redrawn")
  n_ordered_ <- sum(cells_ == "reordered")
  n_new_     <- sum(out_$Data == "new")
  if (n_off_ == 0L) {
    cli::cli_alert_success("Every file of every stem matches its origin.")
  } else {
    cli::cli_alert_warning("{n_off_} file{?s} differ{?s/} from {?its/their} origin or {?is/are} missing on one side.")
  }
  cli::cli_alert_info("{n_redrawn_} png{?s} redrawn; {n_ordered_} tibble{?s} with the same rows in another order.")
  cli::cli_alert_info("{n_new_} stem{?s} first built in 31, with nothing to compare against.")
  fin_show_table(
    .tab   = out_,
    .title = "31 against the files 30 and 40 wrote"
  )
  invisible(out_)
}


# 29. Table: the two readings of the content variables, totals -----------------------------------------------------
# First built in 31 (16 September 2026) to replace, in the online appendix, the observation contrast and the values
# table with one compact table: a row per measure, the share of contracts on which it takes a value and what it is
# worth, under the naive and the rule-based reading. It computes nothing new: it reads the content, money and values
# tibbles this document wrote, and checks the means that two of those builders compute separately against each other.

#' The two readings of the content variables, totals only: the share with a value and the value, per measure
#'
#' Duration is a median over the contracts on which a reading defines it; the other measures are means over all
#' contracts, zero where nothing was extracted. The rule-based countries and states are the paper's content table's --
#' places attached to any recital party -- and the rows beneath them split that count by the registrant and the
#' counterparties. The chunk that calls it needs `results: asis`.
#'
#' @param .dirs List. .lP$Output; the four tibbles are read from DirData.
#' @param .name Character. The stem every file takes.
#' @return Invisibly, the table tibble: Key, Row, Kind, Statistic, NaiveShare, NaiveValue, RuleShare, RuleValue, N.
fin_table_content_readings <- function(.dirs, .name = "ContentReadings") {
  if (FALSE) {
    .dirs <- .lP$Output
    .name <- "ContentReadings"
  }
  total_ <- function(.stem) {
    p_ <- fs::path(.dirs$DirData, paste0(.stem, ".parquet"))
    if (!fs::file_exists(p_)) cli::cli_abort("{.file {p_}} is missing; its table is built earlier in this topic.")
    tab_ <- arrow::read_parquet(p_)
    tab_[tab_$Kind == "total", , drop = FALSE]
  }
  rul_ <- total_(.stem = "ContentRule")
  nai_ <- total_(.stem = "ContentNaive")
  mon_ <- total_(.stem = "MoneyDetail")
  val_ <- total_(.stem = "ContentValues")
  ns_  <- c(ContentRule = rul_$N, ContentNaive = nai_$N, MoneyDetail = mon_$N, ContentValues = val_$N)
  if (length(unique(ns_)) != 1L) {
    cli::cli_abort("The four tibbles are not on one sample: {paste(names(ns_), ns_, collapse = ', ')}.")
  }
  n_ <- unname(ns_[[1L]])

  # COMPUTED TWICE, CHECKED HERE. The values table recomputes from the release what the content and money tables
  # compute from the prepared contracts. Amounts are left out: the values table drops the contracts on which no
  # amount was read, while the paper counts those as zero, which is the money table's mean and the one printed.
  twice_ <- tibble::tibble(
    Quantity = c("Parties, naive", "Parties, rule-based", "Countries, naive", "States, naive"),
    Content  = c(nai_$PartMean, rul_$PartMean, nai_$CtryMean, nai_$StateMean),
    Values   = c(val_$PartNaive, val_$PartRule, val_$CtryNaive, val_$StateNaive)
  ) |>
    dplyr::mutate(Agree = abs(.data$Content - .data$Values) < 1e-9)
  if (all(twice_$Agree)) {
    cli::cli_alert_success("The means both the content and the values builders compute agree.")
  } else {
    cli::cli_alert_warning("The content and the values builders disagree on: {twice_$Quantity[!twice_$Agree]}.")
    tbl_out(
      .tab   = twice_,
      .title = "Means computed twice"
    )
  }

  pct_ <- function(.k) 100 * .k / n_
  tab_ <- tibble::tibble(
    Key        = c("Duration", "Parties", "Countries", "CountriesRegistrant", "CountriesCounterparty", "States",
                   "StatesRegistrant", "StatesCounterparty", "Amounts"),
    Row        = c("Duration, median years", "Parties, mean per contract", "Countries, mean per contract",
                   "attached to the registrant", "attached to a counterparty", "States, mean per contract",
                   "attached to the registrant", "attached to a counterparty", "Amounts, mean per contract"),
    Kind       = c("measure", "measure", "measure", "split", "split", "measure", "split", "split", "measure"),
    Statistic  = c("median", rep("mean", 8L)),
    NaiveShare = c(pct_(nai_$DurN), pct_(nai_$PartN), pct_(nai_$CtryN), NA, NA, pct_(nai_$StateN), NA, NA,
                   mon_$NaiveShare),
    NaiveValue = c(val_$DurNaive, nai_$PartMean, nai_$CtryMean, NA, NA, nai_$StateMean, NA, NA, mon_$NaiveMean),
    RuleShare  = c(pct_(rul_$DurN), pct_(rul_$PartN), pct_(rul_$CtryN), NA, NA, pct_(rul_$StateN), NA, NA,
                   mon_$UsdShare),
    RuleValue  = c(val_$DurRule, rul_$PartMean, rul_$CtryMean, val_$CtryReg, val_$CtryCpty, rul_$StateMean,
                   val_$StateReg, val_$StateCpty, mon_$UsdMean),
    N          = n_
  )
  fin_save_data(
    .tab  = tab_,
    .name = .name,
    .dir  = .dirs$DirData
  )

  # THE CELLS: shares with one decimal, values with two, a dash where a reading has no such quantity.
  fmt_ <- function(.x, .d) dplyr::if_else(is.na(.x), "--", formatC(.x, format = "f", digits = .d))
  cells_ <- tibble::tibble(
    Row        = tab_$Row,
    NaiveShare = fmt_(tab_$NaiveShare, 1L),
    NaiveValue = fmt_(tab_$NaiveValue, 2L),
    RuleShare  = fmt_(tab_$RuleShare, 1L),
    RuleValue  = fmt_(tab_$RuleValue, 2L)
  )
  body_ <- purrr::map_chr(seq_len(nrow(cells_)), \(.i) {
    lab_ <- fin_tex_escape(cells_$Row[.i])
    if (tab_$Kind[.i] == "split") lab_ <- paste0("\\hspace{1em}", lab_)
    paste0(paste(c(lab_, unlist(cells_[.i, -1L])), collapse = " & "), " \\\\")
  })
  fin_tex_write(
    .lines = c(
      "\\begin{tabular}{l r r r r}",
      "\\toprule",
      " & \\multicolumn{2}{c}{Naive reading} & \\multicolumn{2}{c}{Rule-based reading} \\\\",
      "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
      " & With a value (\\%) & Value & With a value (\\%) & Value \\\\",
      "\\midrule",
      body_,
      "\\bottomrule",
      "\\end{tabular}"
    ),
    .path  = fs::path(.dirs$DirTables, paste0(.name, ".tex"))
  )

  note_ <- c(
    "This table sets the naive reading of the extracted spans against the rule-based reading on the unique-contract",
    paste0("sample (N = ", format(n_, big.mark = ","), "): for each content measure, the share of contracts on which it"),
    "takes a value and what it is worth. Duration is the median, in years, over the contracts on which a reading",
    "defines it: the naive end is the farthest date after the filing; the rule-based end is the first of a stated",
    "term, an open-ended clause, a cue-word date and the farthest future date, and durations above 30 years are",
    "dropped. The other measures are means over all contracts, a contract without a value counting as zero.",
    "Parties are the distinct organization names the extractor proposed (naive) and the registrants, co-registrants",
    "and counterparties they are grouped into (rule-based). Countries and states are the distinct places named",
    "outside a governing-law clause (naive) and those attached to a registrant, co-registrant or counterparty",
    "(rule-based), as in the content table of the paper; the rows beneath split the rule-based places into those",
    "attached to the registrant and those attached to the counterparties, which overlap and leave out the",
    "co-registrants, so they do not add up to the row above. Amounts are the distinct figures read with a currency",
    "marker (naive) and those kept in U.S. dollars after the zero and par-value filters (rule-based). A dash marks a",
    "cell that is not defined."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )

  shown_ <- dplyr::mutate(cells_, dplyr::across(-"Row", \(.x) dplyr::if_else(.x == "--", "-", .x)))
  names(shown_) <- c(" ", "With a value (%)", "Value", "With a value (%)", "Value")
  k_ <- shown_ |>
    knitr::kable(
      format   = "html",
      escape   = TRUE,
      caption  = "The two readings of the content variables, totals",
      align    = "lrrrr",
      booktabs = TRUE
    ) |>
    kableExtra::kable_styling(
      full_width        = FALSE,
      position          = "left",
      bootstrap_options = c("hover", "condensed"),
      font_size         = 12
    ) |>
    kableExtra::add_header_above(c(" " = 1, "Naive reading" = 2, "Rule-based reading" = 2)) |>
    kableExtra::add_indent(which(tab_$Kind == "split")) |>
    kableExtra::footnote(
      general           = paste(note_, collapse = " "),
      general_title     = "Notes:",
      footnote_as_chunk = TRUE,
      threeparttable    = TRUE
    )
  fin_emit_kable(
    .kable = k_,
    .ncol  = ncol(shown_)
  )
  invisible(tab_)
}


# 30. Figure: firm fundamentals, the layouts under choice ----------------------------------------------------------
# First built in 31 (16 September 2026). The figure 30 builds is ten inches wide; set at the manuscript's text width
# of 5.65 inches it prints 11-point type at about six points. These forms use the standard 7.5-inch canvas: three
# stack the two panels and differ only in Panel A's time axis, two split the figure in two. The data are 30's
# (fin_data_size_channel(), fin_data_industry()); only the smoothing of Panel A and the layout are new.

#' Panel A's data on the time axis a form plots
#'
#' The saw-tooth of the periodic series is the 10-K quarter: contracts attached to the annual report arrive once a
#' year. "year" averages the four quarters of each complete fiscal year (2002 to 2023; the window's first and last
#' fiscal years are partial); "roll4" is a trailing mean over four fiscal quarters, from the fourth quarter in.
#'
#' @param .size Tibble from fin_data_size_channel().
#' @param .smooth Character. "quarter", "year" or "roll4".
#' @return Tibble: X (the plotted time), Size, Periodic, Current.
fin_data_size_smooth <- function(.size, .smooth = c("quarter", "year", "roll4")) {
  if (FALSE) {
    .size   <- fin_data_size_channel(.ds_quarter = lst_ds$Quarter)
    .smooth <- "year"
  }
  .smooth <- match.arg(.smooth)
  roll_ <- function(.x) {
    as.numeric(stats::filter(
      x      = .x,
      filter = rep(0.25, 4L),
      sides  = 1L
    ))
  }
  out_ <- switch(
    .smooth,
    quarter = dplyr::mutate(.size, X = .data$YQ),
    year    = .size |>
      dplyr::filter(dplyr::n() == 4L, .by = c("fyear", "Size")) |>          # complete fiscal years only
      dplyr::summarise(
        Periodic = mean(.data$Periodic),
        Current  = mean(.data$Current),
        .by      = c("fyear", "Size")
      ) |>
      dplyr::mutate(X = .data$fyear),
    roll4   = .size |>
      dplyr::arrange(.data$Size, .data$YQ) |>
      dplyr::mutate(
        Periodic = roll_(.x = .data$Periodic),
        Current  = roll_(.x = .data$Current),
        .by      = "Size"
      ) |>
      dplyr::filter(!is.na(.data$Periodic)) |>
      dplyr::mutate(X = .data$YQ)
  )
  dplyr::select(out_, "X", "Size", "Periodic", "Current")
}

#' The firm-fundamentals figure in one of the layouts under choice
#'
#' @param .size Tibble from fin_data_size_smooth().
#' @param .industry Tibble from fin_data_industry().
#' @param .panels Character. "stack" (Panel A above Panel B), "size" (A alone) or "industry" (B alone).
#' @param .smooth Character. Panel A's time axis, for its label: "quarter", "year" or "roll4".
#' @return A ggplot, or a patchwork for "stack".
fin_plot_firm_forms <- function(.size, .industry, .panels = c("stack", "size", "industry"), .smooth = "quarter") {
  if (FALSE) {
    .size     <- fin_data_size_smooth(.size = fin_data_size_channel(.ds_quarter = lst_ds$Quarter), .smooth = "year")
    .industry <- fin_data_industry(.ds_quarter = lst_ds$Quarter)
    .panels   <- "stack"
    .smooth   <- "year"
  }
  .panels <- match.arg(.panels)
  stack_  <- .panels == "stack"
  blues_  <- plot_pal_seq(3L)
  cols_   <- c(Small = blues_[[2L]], Large = blues_[[3L]])
  title_  <- ggplot2::theme(
    plot.title          = ggplot2::element_text(
      family = .plot_font,
      size   = .plot_base,
      face   = "bold",
      hjust  = 0,
      margin = ggplot2::margin(b = 6)
    ),
    plot.title.position = "plot"
  )
  long_ <- .size |>
    tidyr::pivot_longer(c("Periodic", "Current"), names_to = "Channel", values_to = "Mean") |>
    dplyr::mutate(
      Channel = factor(dplyr::if_else(.data$Channel == "Periodic", "Periodic filings", "Current reports"),
                       levels = c("Periodic filings", "Current reports")),
      Size    = factor(.data$Size, levels = c("Small", "Large"))
    )
  a_ <- long_ |>
    ggplot2::ggplot(ggplot2::aes(x = .data$X, y = .data$Mean, colour = .data$Size, linetype = .data$Size)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Channel), ncol = 2L) +
    ggplot2::scale_colour_manual(values = cols_, name = NULL) +
    ggplot2::scale_linetype_manual(values = c(Small = "solid", Large = "dashed"), name = NULL) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(accuracy = 0.1),
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::scale_x_continuous(breaks = seq(2004, 2024, 4), expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::labs(
      title = if (stack_) "Panel A: Firm size" else NULL,
      x     = switch(.smooth, quarter = NULL, year = "Fiscal year", roll4 = "Fiscal quarter, mean of the last four"),
      y     = "Contracts per firm-quarter"
    ) +
    plot_theme(.grid = "y", .legend = if (stack_) "top" else "bottom") +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(14, "pt")) +
    title_
  b_ <- ggplot2::ggplot(.industry, ggplot2::aes(y = forcats::fct_rev(.data$FfInd), x = .data$Mean)) +
    ggplot2::geom_col(
      fill      = blues_[[1L]],
      colour    = .plot_ref,
      linewidth = .plot_line,
      width     = 0.7
    ) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = formatC(.data$Mean, format = "f", digits = 2)),
      hjust   = -0.15,
      size    = .plot_base / ggplot2::.pt * 0.9,
      family  = .plot_font
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(
      title = if (stack_) "Panel B: Industry" else NULL,
      x     = "Contracts per firm-quarter",
      y     = NULL
    ) +
    plot_theme(.grid = "x", .legend = "none") +
    title_
  switch(
    .panels,
    # EACH PANEL KEEPS ITS OWN MARGINS. Stacked, patchwork lines the two plot areas up, and Panel B's industry
    # names are wide enough that the alignment pushes Panel A's axis title away from its own axis. Wrapped as
    # elements, the panels are laid out side by side without that alignment: the title sits by the axis it names.
    stack    = patchwork::wrap_plots(
      patchwork::wrap_elements(full = a_),
      patchwork::wrap_elements(full = b_),
      ncol    = 1L,
      heights = c(1, 1.1)
    ),
    size     = a_,
    industry = b_
  )
}

#' One firm-fundamentals form, built, saved and shown
#'
#' "a" to "c" stack Panel A above Panel B on the standard canvas, with Panel A by fiscal quarter, as fiscal-year
#' means or as trailing four-quarter means; "size" and "industry" are the two halves of a split, the first on
#' fiscal-year means. The chunk that calls it needs `results: asis`.
#'
#' @param .ds_quarter The prepared Quarter dataset.
#' @param .form Character. "a", "b", "c", "size" or "industry".
#' @param .dirs List. .lP$Output.
#' @param .name Character or NULL. The stem; NULL takes the form's own.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_firm_forms <- function(.ds_quarter, .form = c("a", "b", "c", "size", "industry"), .dirs, .name = NULL) {
  if (FALSE) {
    .ds_quarter <- lst_ds$Quarter
    .form       <- "b"
    .dirs       <- .lP$Output
    .name       <- NULL
  }
  .form <- match.arg(.form)
  spec_ <- switch(
    .form,
    a        = list(Panels = "stack", Smooth = "quarter", Stem = "FirmFundamentalsA", Rows = 18L),
    b        = list(Panels = "stack", Smooth = "year", Stem = "FirmFundamentalsB", Rows = 18L),
    c        = list(Panels = "stack", Smooth = "roll4", Stem = "FirmFundamentalsC", Rows = 18L),
    size     = list(Panels = "size", Smooth = "year", Stem = "FirmSize", Rows = 8L),
    industry = list(Panels = "industry", Smooth = "year", Stem = "FirmIndustry", Rows = 12L)
  )
  name_ <- if (is.null(.name)) spec_$Stem else .name
  size_ <- fin_data_size_smooth(
    .size   = fin_data_size_channel(.ds_quarter = .ds_quarter),
    .smooth = spec_$Smooth
  )
  ind_ <- fin_data_industry(.ds_quarter = .ds_quarter)
  n_q_ <- .ds_quarter |>
    dplyr::filter(.data$S5_Quarter) |>
    dplyr::count() |>
    dplyr::collect() |>
    dplyr::pull("n")
  when_ <- switch(
    spec_$Smooth,
    quarter = "by fiscal quarter from 2001q2 to 2024q1.",
    year    = "averaged over the four quarters of each complete fiscal year from 2002 to 2023.",
    roll4   = "as a trailing mean over four fiscal quarters from 2002q1 to 2024q1."
  )
  size_txt_ <- paste(
    "the mean number of contracts per firm-quarter for firms in the smallest and the largest quartile of winsorized",
    "total assets, quartiles defined within each fiscal year, separately for periodic filings (10-K, 10-Q, 20-F) and",
    "current reports (every other form, registration statements included),",
    when_
  )
  ind_txt_ <- "the mean number of contracts per firm-quarter by Fama-French 12 industry."
  lead_ <- paste0(
    "This figure shows filing activity by firm characteristics on the firm-quarter panel: every fiscal quarter ",
    "between 2001 and 2024 of every firm that filed at least one contract in the sample (",
    format(n_q_, big.mark = ","), " firm-quarters), non-filing quarters included."
  )
  note_ <- switch(
    spec_$Panels,
    stack    = c(lead_, paste("Panel A gives", size_txt_), paste("Panel B gives", ind_txt_)),
    size     = c(lead_, paste("It gives", size_txt_)),
    industry = c(lead_, paste("It gives", ind_txt_))
  )
  ind_chr_ <- dplyr::mutate(ind_, FfInd = as.character(.data$FfInd))
  data_ <- switch(
    spec_$Panels,
    stack    = dplyr::bind_rows(
      dplyr::mutate(size_, Panel = "A"),
      dplyr::mutate(ind_chr_, Panel = "B")
    ),
    size     = size_,
    industry = ind_chr_
  )
  fin_figure_save(
    .plot   = fin_plot_firm_forms(
      .size     = size_,
      .industry = ind_,
      .panels   = spec_$Panels,
      .smooth   = spec_$Smooth
    ),
    .data   = data_,
    .note   = note_,
    .name   = name_,
    .dirs   = .dirs,
    .height = plot_height(spec_$Rows)
  )
}
