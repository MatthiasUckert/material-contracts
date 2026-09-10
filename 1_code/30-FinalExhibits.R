# 31-FinalExhibits: the manuscript's exhibits in their final form, from the released files ---------------------------------
#
# WHAT THIS FILE DOES
# The paper's exhibits: one view per exhibit, on its manuscript sample, in the form the manuscript
# prints, with the figure note written from the tibble that drew it. It reads the Dropbox release
# and Ann-Kristin's 101/103 outputs through its own readers, so a number here and a number in one
# of her tables come from the same bytes.
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
# STAND-ALONE. This document reads the Dropbox folder and the classification stages' run folders,
# and nothing that another document rendered. Her .dta files are converted under this document's
# own Cache, keyed on their size. 30-Descriptives, the lab this grew out of, was retired on 10
# September 2026; what this document used from it is section 0 of this library.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation; lines at most 125 columns.

if (FALSE) {
  .inputs   <- unlist(.lP$Input)
  .min_date <- .lP$Params$ReleaseMin
  .dir      <- .lP$Output$DirPrepared
}


# 0. The release: readers, vocabulary, references, and the exhibit data carried over from 30 --------------------------
# 30-Descriptives was the lab: every exhibit on every sample, reconciled against the two manuscripts.
# It was retired on 10 September 2026 once every decision it produced lived here. What this section
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
  writeLines(lines_, .path)
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
  writeLines(lines_, .path)
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

#' The content cells for the page: N and means with no decimals, SDs with two
#'
#' The duration N is not a column: it rides in the tibble and the note, so the table stays on one
#' line per row.
#'
#' @param .tab Tibble from fin_tab_by_category(.stats = fin_content_stats).
#' @param .digits Named integer. Decimals for Mean and SD.
#' @return Tibble of character columns in table order, Row first.
fin_content_cells <- function(.tab, .digits = c(Mean = 0L, SD = 2L)) {
  if (FALSE) {
    .tab    <- tab_content_rule
    .digits <- c(Mean = 0L, SD = 2L)
  }
  m_ <- .digits[["Mean"]]
  s_ <- .digits[["SD"]]
  tibble::tibble(
    Row       = .tab$Row,
    N         = fin_fmt_count(.tab$N),
    WordsMean = fin_fmt_num(.tab$WordsMean, m_),
    WordsSD   = fin_fmt_num(.tab$WordsSD,   s_),
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
#' @param .size Character. A LaTeX size command without the backslash: "small", "footnotesize".
#' @return Invisibly, the path.
fin_tex_content <- function(.tab, .path, .digits = c(Mean = 0L, SD = 2L), .size = "small") {
  if (FALSE) {
    .tab    <- tab_content_rule
    .path   <- fs::path(.lP$Output$DirTables, "ContentRule.tex")
    .digits <- c(Mean = 0L, SD = 2L)
    .size   <- "small"
  }
  cells_ <- fin_content_cells(
    .tab    = .tab,
    .digits = .digits
  )
  body_ <- fin_tex_category_body(
    .cells = cells_,
    .kind  = .tab$Kind
  )
  lines_ <- c(
    paste0("\\begingroup\\", .size),
    "\\begin{tabular}{l r rr rr rr rr rr}",
    "\\toprule",
    " & Contracts & \\multicolumn{2}{c}{Words} & \\multicolumn{2}{c}{Duration (years)} &",
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
  writeLines(lines_, .path)
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
#' @param .ref Tibble. 30's .fin_reference; the sample size the note assumes.
#' @return Invisibly, the table tibble with Arm and Sample columns.
fin_table_content <- function(.ds_contracts, .con, .arm = c("rule", "naive"), .roles = .fin_roles_recital, .dirs,
                              .name = NULL, .digits = c(Mean = 0L, SD = 2L), .ref = .fin_reference) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .con          <- con
    .arm          <- "rule"
    .roles        <- .fin_roles_recital
    .dirs         <- .lP$Output
    .name         <- NULL
    .digits       <- c(Mean = 0L, SD = 2L)
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
    .tab    = tab_,
    .path   = fs::path(.dirs$DirTables, paste0(name_, ".tex")),
    .digits = .digits
  )
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
    "Means are rounded to whole numbers; standard deviations to two decimals."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(name_, ".tex"))
  )
  cells_ <- fin_content_cells(
    .tab    = tab_,
    .digits = .digits
  )
  names(cells_) <- c(" ", "N", rep(c("Mean", "SD"), 5L))
  fin_show_category(
    .cells  = cells_,
    .kind   = tab_$Kind,
    .groups = c(" " = 1, "Contracts" = 1, "Words" = 2, "Duration (years)" = 2, "Parties" = 2, "Countries" = 2,
                "States" = 2),
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
  writeLines(lines_, .path)
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))

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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
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
fin_table_class_sample <- function(.ds_sample, .dirs, .name = "ClassLabelled") {
  if (FALSE) {
    .ds_sample <- lst_ds_class$ClassSample
    .dirs      <- .lP$Output
    .name      <- "ClassLabelled"
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
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
    .name  <- "ClassTransformer"
    .dirs  <- .lP$Output
    .title <- "Transformer"
    .note  <- "A note."
  }
  fin_save_data(
    .tab  = .tab,
    .name = .name,
    .dir  = .dirs$DirData
  )
  # A SCORE WITH ITS SPREAD. Where the tibble carries the across-fold SD of a metric, the cell reads
  # "pooled (sd)", as a regression table would; counts and shares stay as they are.
  cells_ <- tibble::tibble(Row = .tab$Row)
  for (i_ in seq_along(.cols)) {
    col_ <- .cols[[i_]]
    v_   <- .tab[[col_]]
    sd_  <- .tab[[paste0(col_, "SD")]]
    cells_[[names(.cols)[i_]]] <- if (col_ %in% c("Support", "Predicted")) {
      fin_fmt_count(v_)
    } else if (grepl("\\(%\\)", names(.cols)[i_])) {
      fin_fmt_num(100 * v_, 1L)
    } else if (is.null(sd_)) {
      fin_fmt_num(v_, 3L)
    } else {
      dplyr::if_else(is.na(v_), "", paste0(fin_fmt_num(v_, 3L), " (", fin_fmt_num(sd_, 3L), ")"))
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
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
fin_table_class_transformer <- function(.ds_panel, .ds_sample, .ds_crowned, .dirs, .name = "ClassTransformer") {
  if (FALSE) {
    .ds_panel   <- lst_ds_class$ClassPanel
    .ds_sample  <- lst_ds_class$ClassSample
    .ds_crowned <- lst_ds_class$ClassCrowned
    .dirs       <- .lP$Output
    .name       <- "ClassTransformer"
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
    "second valid category. In parentheses, the standard deviation of the score across the five folds,",
    "each computed on that fold's held-out documents.",
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
fin_table_class_amendment <- function(.ds_panel, .ds_crowned, .dirs, .name = "ClassAmendment") {
  if (FALSE) {
    .ds_panel   <- lst_ds_class$ClassPanel
    .ds_crowned <- lst_ds_class$ClassCrowned
    .dirs       <- .lP$Output
    .name       <- "ClassAmendment"
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
  sdf_ <- function(.v, .s) dplyr::if_else(is.na(.v), "", paste0(fin_fmt_num(.v, 3L), " (", fin_fmt_num(.s, 3L), ")"))
  cells_ <- tibble::tibble(
    Label     = m_$Label,
    N         = fin_fmt_count(m_$Support),
    Predicted = fin_fmt_count(m_$Predicted),
    Accuracy  = sdf_(m_$Accuracy, m_$AccuracySD),
    Precision = sdf_(m_$Precision, m_$PrecisionSD),
    Recall    = sdf_(m_$Recall, m_$RecallSD),
    F1        = sdf_(m_$F1, m_$F1SD)
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
  tot_ <- m_[m_$Label == "Total", , drop = FALSE]
  note_ <- c(
    paste0("Out-of-fold scores of the amendment classifier that ships (", eng_, ") on the labelled sample"),
    paste0("(N = ", format(tot_$Support, big.mark = ","), "). Accuracy per row is one-vs-rest; the Total row"),
    "reports overall accuracy with macro precision, recall and F1. In parentheses, the standard deviation",
    "across the five folds."
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
fin_table_class_keyword <- function(.ds_panel, .ds_sample, .dirs, .name = "ClassKeyword") {
  if (FALSE) {
    .ds_panel  <- lst_ds_class$ClassPanel
    .ds_sample <- lst_ds_class$ClassSample
    .dirs      <- .lP$Output
    .name      <- "ClassKeyword"
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
    "macro precision, recall and F1. In parentheses, the standard deviation across the five folds.",
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
fin_table_class_arms <- function(.ds_panel, .ds_crowned, .dirs, .name = "ClassArms") {
  if (FALSE) {
    .ds_panel   <- lst_ds_class$ClassPanel
    .ds_crowned <- lst_ds_class$ClassCrowned
    .dirs       <- .lP$Output
    .name       <- "ClassArms"
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
    Accuracy    = paste0(fin_fmt_num(arms_$Accuracy, 3L), " (", fin_fmt_num(arms_$AccuracySD, 3L), ")"),
    AccuracySel = fin_fmt_num(arms_$AccuracySel, 3L),
    MacroF1     = paste0(fin_fmt_num(arms_$MacroF1, 3L), " (", fin_fmt_num(arms_$MacroF1SD, 3L), ")"),
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
  note_ <- c(
    "Every engine on every task, out of fold on the labelled sample. Coverage is the share of documents",
    "the engine committed on; accuracy is over every document, a declined document counting as a miss,",
    "and accuracy committed is over the committed ones; in parentheses, the standard deviation across",
    "the five folds. Macro-F1 weights every category equally,",
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
    Accuracy   = paste0(fin_fmt_num(tab_$Accuracy, 3L), " (", fin_fmt_num(tab_$AccuracySD, 3L), ")"),
    MacroF1    = paste0(fin_fmt_num(tab_$MacroF1, 3L), " (", fin_fmt_num(tab_$MacroF1SD, 3L), ")"),
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
              "Task & Model & Context & Folds & Accuracy (SD) & Macro-F1 (SD) & Weighted F1 & \\\\", "\\midrule", body_,
              "\\bottomrule", "\\end{tabular}", "\\endgroup")
  fs::dir_create(.dirs$DirTables)
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
  note_ <- c(
    "Every transformer configuration cross-validated, by task: the mean and standard deviation across",
    "the five folds of out-of-fold accuracy and macro-F1. Selection is on macro-F1; deployed marks the",
    "configurations refitted on the full sample at each context length, ships the one the paper uses."
  )
  fin_note_write(
    .text = note_,
    .path = fs::path(.dirs$DirNotes, paste0(.name, ".tex"))
  )
  names(cells_) <- c("Task", "Model", "Context", "Folds", "Accuracy (SD)", "Macro-F1 (SD)", "Weighted F1", " ")
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
fin_table_class_confusion <- function(.ds_panel, .ds_sample, .ds_crowned, .dirs, .name = "ClassConfusion") {
  if (FALSE) {
    .ds_panel   <- lst_ds_class$ClassPanel
    .ds_sample  <- lst_ds_class$ClassSample
    .ds_crowned <- lst_ds_class$ClassCrowned
    .dirs       <- .lP$Output
    .name       <- "ClassConfusion"
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
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
# Data/<Name>.parquet, the figure to Figures/<Name>.pdf and .png through plot_save(), the note to
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
    .plot   = .plot,
    .name   = .name,
    .dir    = .dirs$DirFigures,
    .height = .height,
    .width  = .width
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
    ggplot2::labs(x = "Contracts (in 1,000s)", y = NULL) +
    plot_theme(.grid = "x", .legend = "none") +
    suppressWarnings(theme_)
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
    ggplot2::labs(x = "Contracts per firm-year", y = NULL) +
    plot_theme(.grid = "x", .legend = "none") +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
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
  reg_ <- tibble::tibble(
    X     = unname(.fin_regime_years["Fast2019"]),
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
    plot_scale_y_pct(.accuracy = 1, .expand = c(0.02, 0.08)) +
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
    dplyr::arrange(dplyr::desc(.data$nContracts)) |>
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
    dplyr::arrange(dplyr::desc(.data$nContracts))
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
#' in that period, which is the quantity the referee asked for. Cells under .min_n contracts keep
#' their fill and lose their label.
#'
#' @param .tab_data Tibble from fin_data_f09() for one sample, one period (or "All").
#' @param .min_n Integer. Cells with fewer contracts are not labelled.
#' @param .limit Numeric. Top of the fill scale; one value across the three periods keeps them comparable.
#' @return A ggplot.
fin_plot_redactions_heat <- function(.tab_data, .min_n = 30L, .limit = 1) {
  if (FALSE) {
    .tab_data <- dat_
    .min_n    <- 30L
    .limit    <- 1
  }
  if (length(unique(.tab_data$Period)) != 1L) cli::cli_abort("One period per figure; the rows carry several.")
  cat_ <- fin_tab_categories() |>
    dplyr::mutate(Bar = dplyr::if_else(nzchar(.data$Level2), .data$Level2, .data$Level1)) |>
    dplyr::select("Class", "Level1", "Bar")
  # EVERY CELL IS DRAWN. An industry that filed no contract of a category in the period has no row,
  # and a missing row is a hole in the grid that reads as background; completed to N = 0, it is a
  # grey cell with a dash, which is what "no contracts" looks like. Thin cells keep their fill and
  # lose their label.
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
      Label    = dplyr::case_when(
        .data$N == 0L       ~ "-",
        .data$N >= .min_n   ~ scales::label_percent(accuracy = 1)(.data$Share),
        .default            = ""
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
      switch = "x"                        # the super-category strip sits below the category labels
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
#' @param .min_n Integer. Cells with fewer contracts are not labelled.
#' @param .limit Numeric. Top of the fill scale, shared across the three.
#' @param .dirs List. .lP$Output.
#' @param .name Character or NULL. The stem; NULL derives RedactionsIndustry<Period>.
#' @return Invisibly, a list: Plot, Data, Files.
fin_figure_redactions_heat <- function(.ds_contracts, .ds_quarter, .period = c("all", "pre", "post"), .min_n = 30L,
                                       .limit = 0.8, .dirs, .name = NULL) {
  if (FALSE) {
    .ds_contracts <- lst_ds$Contracts
    .ds_quarter   <- lst_ds$Quarter
    .period       <- "all"
    .min_n        <- 30L
    .limit        <- 0.8
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
    paste0("Cells with fewer than ", .min_n, " contracts (", thin_, " of 144) are shaded but not labelled;"),
    paste0("cells with no contracts (", empty_, ") are grey with a dash.")
  )
  fin_figure_save(
    .plot   = fin_plot_redactions_heat(.tab_data = one_, .min_n = .min_n, .limit = .limit),
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
  "ClassLabelled", "Table", "labelled sample (03A)",
  "The labelled sample by category",
  "ClassTransformer", "Table", "labelled sample, out of fold",
  "Transformer scores by category",
  "ClassAmendment", "Table", "labelled sample, out of fold",
  "Amendment classifier scores",
  "ClassKeyword", "Table", "labelled sample, out of fold",
  "Keyword arm scores by category",
  "ClassArms", "Table", "labelled sample, out of fold",
  "Every engine on every task, routing ceiling",
  "ClassSweep", "Table", "labelled sample, out of fold",
  "Model selection: the sweep",
  "ClassConfusion", "Table", "labelled sample, out of fold",
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
  missing_ <- out_$Stem[is.na(out_$Data) & is.na(out_$Tex) & is.na(out_$Pdf)]
  if (length(missing_) > 0L) cli::cli_alert_warning("Registered without files on disk: {missing_}.")
  on_disk_ <- c(
    fs::path_ext_remove(fs::path_file(fs::dir_ls(.dirs$DirTables, glob = "*.tex"))),
    fs::path_ext_remove(fs::path_file(fs::dir_ls(.dirs$DirFigures, glob = "*.pdf")))
  )
  stray_ <- setdiff(unique(on_disk_), .registry$Stem)
  if (length(stray_) > 0L) cli::cli_alert_warning("On disk but not registered: {stray_}.")
  readr::write_csv(out_, .path)
  shown_ <- dplyr::select(out_, "Stem", "Kind", "Where", "Sample", "Description", "Files")
  k_ <- shown_ |>
    knitr::kable(
      format   = "html",
      escape   = TRUE,
      caption  = "Every exhibit this document writes",
      align    = "llllll",
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
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
  writeLines(lines_, fs::path(.dirs$DirTables, paste0(.name, ".tex")))
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
  incomplete_ <- .manifest$Stem[is.na(.manifest$Tex) & is.na(.manifest$Pdf) & is.na(.manifest$Data)]
  if (length(incomplete_) > 0L) {
    msg_ <- "{length(incomplete_)} registered exhibit{?s} without any file: {incomplete_}."
    if (.run) cli::cli_abort(c("Not deploying.", "x" = msg_)) else cli::cli_alert_warning(msg_)
  }
  cur_  <- fs::path(.dir_paper, "current")
  arch_ <- fs::path(.dir_paper, "_archive", format(Sys.time(), "%Y-%m-%d_%H%M"))
  srcs_ <- c(Figures = .dirs$DirFigures, Tables = .dirs$DirTables, Notes = .dirs$DirNotes)
  n_files_ <- sum(purrr::map_int(srcs_, \(.d) length(fs::dir_ls(.d, type = "file"))))
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
    fs::file_copy(fs::dir_ls(.d, type = "file"), fs::path(cur_, .n), overwrite = TRUE)
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
