# 30-Descriptives: the paper's descriptive tables and figures, from the released files ---------------------------------
#
# WHAT THIS FILE DOES
# Reads the release Ann-Kristin's Stata analysis reads -- Contracts.parquet, Summaries.parquet, and the
# firm-quarter panel her 103 do-file builds from Compustat -- and turns them into the sample tables,
# figures and descriptive tables of the paper. Nothing here estimates a regression; those stay in
# Stata (106) and in 20-RunRegression.
#
# ONE DATA SOURCE, AND IT IS HERS
# Every input sits under her MatContractPipeline folder. Where she has a .dta, it is converted once to
# parquet under this script's Cache and read from there; the conversion is keyed on the .dta's
# modification time, so a re-export on her side rebuilds it and nothing else. The point is that a
# number in this document and a number in her tables come from the same bytes.
#
# ONE EXHIBIT AT A TIME
# Sections 1-5 read, derive, name the samples and validate. Section 6 is the contract every exhibit
# follows; each later section is one exhibit -- a compute function, a plot or tex function and a
# report function -- added in the order the specification lists them and drawn on every sample the
# runbook hands it.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation; lines at most 125 columns.

if (FALSE) {
  .path     <- .lP$Input$FilContracts
  .path_dta <- .lP$Input$DtaQuarter
  .path_out <- .lP$Cache$CacheQuarter
}


# 0. Vocabulary -------------------------------------------------------------------------------------------------------------
# The twelve classes in the order the manuscript numbers them, which is also 103's class_predicted and
# 20's .reg_class_levels. 03A registers the same set under "ClassDetailed" in taxonomic order; this
# file registers the paper's order under its own key so neither library has to change for the other.
# Which order the paper finally uses is Q1 of the specification; changing it is one edit here.

.des_class_levels <- c(
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

.des_class_short <- c(
  "(1) M&A", "(2) Peer Agreements", "(3) Empl. - Compensation", "(4) Empl. - Legal", "(5) Credit",
  "(6) Equity", "(7) Leases", "(8) Licenses", "(9) Assets", "(10) Customer/Supplier", "(11) R&D",
  "(12) Other"
)

# Form families, amendment forms folded into their base, and the three filing groups the paper uses
# throughout: ad-hoc (8-K and the registration statements), yearly (10-K, 20-F), quarterly (10-Q).
.des_form_levels <- c("8-K", "10-Q", "10-K", "S-1", "S-4", "F-1", "F-4", "20-F")

.des_form_group <- c(
  "8-K" = "Ad-hoc", "S-1" = "Ad-hoc", "S-4" = "Ad-hoc", "F-1" = "Ad-hoc", "F-4" = "Ad-hoc",
  "10-K" = "Yearly", "20-F" = "Yearly",
  "10-Q" = "Quarterly"
)

.des_form_foreign <- c("F-1", "F-4", "20-F")

# Fama-French 12, in the display order 101 assigns to ff_ind. FfInd in the quarter panel is an integer
# 1..12 in exactly this order.
.des_ff12_levels <- c(
  "Consumer NonDurables", "Consumer Durables", "Manufacturing", "Energy", "Chemicals",
  "Business Equipment", "Telecommunication", "Utilities", "Wholesale & Retail", "Healthcare",
  "Finance", "Other"
)

plot_register_levels(
  .key     = "ClassPaper",
  .levels  = .des_class_levels,
  .short   = .des_class_short,
  .colours = NULL
)

# The same twelve without their numbers, for axes where forty characters do not fit.
plot_register_levels(
  .key     = "ClassBare",
  .levels  = .des_class_levels,
  .short   = stringi::stri_replace_first_regex(.des_class_short, "^\\([0-9]+\\) ", ""),
  .colours = NULL
)

plot_register_levels(
  .key     = "FormFamily",
  .levels  = .des_form_levels,
  .short   = NULL,
  .colours = plot_pal_cat(length(.des_form_levels))
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
.des_family_levels <- c("Pandemic", "Disruption", "RateReform", "Regulation", "Boilerplate")

plot_register_levels(
  .key     = "FfInd",
  .levels  = .des_ff12_levels,
  .short   = NULL,
  .colours = NULL
)

# The named samples, in ladder order, with the one-line description each caption and table uses. The
# filters themselves are written in the runbook; this is only what they are called.
.des_sample_labels <- c(
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
.des_reference <- tibble::tribble(
  ~Quantity,            ~Revision,
  "Full sample",        1436667L,
  "Malformatted",       31289L,
  "Multiple filer",     269283L,
  "Unique contracts",   1136095L,
  "Firms full sample",  43123L,
  "Firms unique",       26915L
)

# What is read from Contracts.parquet. Named here rather than at the call site because the file has
# 123 columns and the export's dictionary names each one; this list is the subset the descriptives
# need, and a column not in it is not loaded.
.des_contract_cols <- c(
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
.des_quarter_cols <- c(
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
.des_ak_contract_cols <- c(
  DocID          = "DocID",
  AkKeep         = "Keep",
  AkClassCode    = "class_predicted",
  AkRedacted     = "redacted",
  AkRedactedText = "redacted_text",
  AkIsAmend      = "isAmend",
  AkYear         = "year"
)


# 1. Conversion -------------------------------------------------------------------------------------------------------------

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
des_convert_dta <- function(.path_dta, .path_out, .cols = NULL, .rerun = FALSE) {
  if (FALSE) {
    .path_dta <- .lP$Input$DtaQuarter
    .path_out <- .lP$Cache$CacheQuarter
    .cols     <- .des_quarter_cols
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


# 2. Reading ----------------------------------------------------------------------------------------------------------------

#' The contract table with the paper's derived columns and its definitional choices made once
#'
#' Reads the columns in .des_contract_cols and adds what every exhibit needs: the calendar year, the
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
des_read_contracts <- function(.path, .redaction = "symbolexplicit", .redact_min = 1L, .duration = "cascade",
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

  cli::cli_alert_info("Reading {length(.des_contract_cols)} columns from {.file {fs::path_file(.path)}}")

  have_ <- names(arrow::open_dataset(.path))
  miss_ <- setdiff(.des_contract_cols, have_)
  if (length(miss_) > 0L) {
    cli::cli_abort("Contracts.parquet lacks {length(miss_)} expected column{?s}: {miss_}.")
  }

  tab_ <- arrow::open_dataset(.path) |>
    dplyr::select(dplyr::all_of(.des_contract_cols)) |>
    dplyr::collect()

  marker_ <- c("nRedactExplicit", "nRedactSymbol", "nRedactBlank", "nOmitExplicit", "nOmitSymbol",
               "nRedactBare", "nRedactMoney")

  out_ <- tab_ |>
    dplyr::mutate(dplyr::across(dplyr::all_of(marker_), \(.x) dplyr::coalesce(.x, 0L))) |>
    dplyr::mutate(
      Year        = as.integer(lubridate::year(.data$DateFiled)),
      FormFamily  = stringi::stri_replace_first_regex(.data$FormType, "/A$", ""),
      IsAmendForm = as.integer(stringi::stri_detect_regex(.data$FormType, "/A$")),
      FilingGroup = unname(.des_form_group[.data$FormFamily]),
      IsForeign   = as.integer(.data$FormFamily %in% .des_form_foreign),
      Delayed     = as.integer(.data$FormFamily %in% c("10-K", "10-Q", "20-F")),
      Is8K        = as.integer(.data$FormFamily == "8-K"),
      IsRegStmt   = as.integer(.data$FormFamily %in% c("S-1", "S-4", "F-1", "F-4")),
      Keep        = .data$DescSample == 1L & .data$PrimaryFiler == 1L,
      Matched     = !is.na(.data$gvkey),
      ClassCode   = match(.data$Class, .des_class_levels),
      ClassPaper  = factor(.data$Class, levels = .des_class_levels),
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

  unknown_ <- setdiff(unique(stats::na.omit(out_$FormFamily)), .des_form_levels)
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
#' @param .tab_contracts Tibble from `des_read_contracts()`, for the exhibits per filing.
#' @return Tibble, one row per announcement, with nExhibits and Year added.
des_read_summaries <- function(.path, .tab_contracts) {
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
#' under the names in .des_quarter_cols. Nothing is recomputed: Table 4 has to show the same clipped
#' values her regressions control for.
#'
#' @param .path Character. The converted parquet.
#' @return Tibble, one row per gvkey x fyear x fqtr.
des_read_quarter <- function(.path) {
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
      # 101 codes the industries 1..12 in .des_ff12_levels' order; anything else, including the 0 it
      # assigns before the ranges, becomes NA rather than a thirteenth industry.
      FfIndLab = factor(as.integer(.data$FfInd), levels = seq_along(.des_ff12_levels), labels = .des_ff12_levels),
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
des_read_concentration <- function(.path) {
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
des_read_ak_contract <- function(.path) {
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
des_read_places <- function(.path) {
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
#' @return Tibble: DocID, Family (character, in .des_family_levels), nHits, nTerms, HasTerm, FirstPos,
#'   PerKWords.
des_read_term_docs <- function(.path) {
  if (FALSE) .path <- .lP$Input$FilTermDocs
  out_ <- arrow::read_parquet(.path) |>
    dplyr::select(-dplyr::any_of("HashDocument")) |>
    dplyr::mutate(Family = as.character(.data$Family), HasTerm = as.integer(.data$HasTerm), nHits = as.integer(.data$nHits))
  unknown_ <- setdiff(unique(out_$Family), .des_family_levels)
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
des_read_cto_orders <- function(.path) {
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

# 3. Samples ----------------------------------------------------------------------------------------------------------------

#' The sample ladder as Table 2 Panel A prints it
#'
#' Counted from the contract table's own ladder columns. Full sample is every row at steps 2-6 --
#' step 1, outside the window, is the one rung the paper never mentions -- malformed is step 2,
#' multiple filer is every non-primary copy on the descriptive rungs, and unique contracts is what
#' remains. Firms are distinct CIKs at each rung; the co-filer loss is CIKs that appear only on
#' non-primary copies.
#'
#' @param .tab Tibble from `des_read_contracts()`.
#' @return Tibble: Quantity, Contracts, Firms, in the table's row order.
des_table_ladder <- function(.tab) {
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

#' Where every row of the file sits, by ladder step and copy
#'
#' @param .tab Tibble from `des_read_contracts()`.
#' @return Tibble: SampleStepDesc, rows, primary copies, distinct attachments, distinct CIKs.
des_table_steps <- function(.tab) {
  if (FALSE) .tab <- tab_contracts

  .tab |>
    dplyr::summarise(
      nRows     = dplyr::n(),
      nPrimary  = sum(.data$PrimaryFiler),
      nDocs     = dplyr::n_distinct(.data$HashDocument),
      nCIK      = dplyr::n_distinct(.data$CIK),
      nMatched  = sum(!is.na(.data$gvkey)),
      .by = c("SampleStepCode", "SampleStepDesc")
    ) |>
    dplyr::arrange(.data$SampleStepCode)
}

#' The named samples every exhibit draws on, with their sizes
#'
#' Contract-level samples are membership columns on the contract table; the announcement and
#' quarter samples are their own tables and are passed as a named list beside it.
#'
#' @param .tab The contract table carrying the membership columns.
#' @param .samples Character. Membership column names, in ladder order.
#' @param .others Named list of tibbles. Samples that are not contract tables.
#' @return Tibble: Sample, What, Rows, Contracts (distinct attachments), Firms.
des_table_samples <- function(.tab, .samples, .others = list()) {
  if (FALSE) {
    .tab     <- tab_contracts
    .samples <- .des_contract_samples
    .others  <- lst_other
  }

  con_ <- purrr::map(.samples, \(.s) {
    in_ <- .tab[[.s]]
    tibble::tibble(
      Sample    = .s,
      What      = unname(.des_sample_labels[.s]),
      Rows      = sum(in_),
      Contracts = dplyr::n_distinct(.tab$HashDocument[in_]),
      Firms     = dplyr::n_distinct(.tab$CIK[in_])
    )
  }) |>
    purrr::list_rbind()

  oth_ <- purrr::imap(.others, \(.o, .n) {
    tibble::tibble(
      Sample    = .n,
      What      = unname(.des_sample_labels[.n]),
      Rows      = nrow(.o),
      Contracts = NA_integer_,
      Firms     = if ("CIK" %in% names(.o)) dplyr::n_distinct(.o$CIK) else
        if ("gvkey" %in% names(.o)) dplyr::n_distinct(.o$gvkey) else NA_integer_
    )
  }) |>
    purrr::list_rbind()

  dplyr::bind_rows(con_, oth_)
}


# 4. Validation -------------------------------------------------------------------------------------------------------------

#' The ladder against the manuscript's numbers
#'
#' Stated before it is read: every quantity in .des_reference reproduces exactly, or the release on
#' Dropbox is not the one the revision's Table 2 was built from. A miss is not rounded away; it aborts.
#'
#' @param .tab_ladder Tibble from `des_table_ladder()`.
#' @param .ref Tibble. The reference values.
#' @return Invisibly, the comparison table.
des_check_reference <- function(.tab_ladder, .ref = .des_reference) {
  if (FALSE) {
    .tab_ladder <- tab_ladder
    .ref        <- .des_reference
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

#' Her Keep against PrimaryFiler
#'
#' Both keep one copy per attachment and both give 1,136,095, but they need not keep the SAME copy.
#' Where they differ, the contract is attributed to a different registrant, and possibly a different
#' form, in her figures than here. The count is what Q19 of the specification turns on.
#'
#' @param .tab Tibble from `des_read_contracts()` joined to `des_read_ak_contract()`.
#' @return Invisibly, a one-row tibble of the counts.
des_check_copies <- function(.tab) {
  if (FALSE) .tab <- tab_contracts

  desc_ <- dplyr::filter(.tab, .data$DescSample == 1L)

  out_ <- tibble::tibble(
    nKeepHere   = sum(desc_$PrimaryFiler == 1L),
    nKeepHers   = sum(desc_$AkKeep == 1L, na.rm = TRUE),
    nBoth       = sum(desc_$PrimaryFiler == 1L & desc_$AkKeep == 1L, na.rm = TRUE),
    nHereOnly   = sum(desc_$PrimaryFiler == 1L & desc_$AkKeep != 1L, na.rm = TRUE),
    nHersOnly   = sum(desc_$PrimaryFiler == 0L & desc_$AkKeep == 1L, na.rm = TRUE)
  )

  tbl_out(.tab = out_, .title = "One copy per attachment: PrimaryFiler against her first-row rule")

  # WHETHER THE DIFFERENT COPY IS A DIFFERENT CONTRACT. The two copies of one attachment are joined
  # on HashDocument and their filing facts compared. A copy pair on the same form from the same
  # registrant is a distinction without a difference; one on a 10-K here and an 8-K there moves the
  # contract between filing groups in every figure that splits on them.
  pair_ <- desc_ |>
    dplyr::filter(.data$PrimaryFiler == 1L, dplyr::coalesce(.data$AkKeep, 0L) != 1L) |>
    dplyr::select("HashDocument", HereCIK = "CIK", HereForm = "FormFamily", HereYear = "Year") |>
    dplyr::inner_join(
      desc_ |>
        dplyr::filter(.data$PrimaryFiler == 0L, dplyr::coalesce(.data$AkKeep, 0L) == 1L) |>
        dplyr::select("HashDocument", HersCIK = "CIK", HersForm = "FormFamily", HersYear = "Year"),
      by = dplyr::join_by(HashDocument)
    )

  diff_ <- tibble::tibble(
    nPairs        = nrow(pair_),
    nDiffCIK      = sum(pair_$HereCIK != pair_$HersCIK),
    nDiffForm     = sum(pair_$HereForm != pair_$HersForm),
    nDiffGroup    = sum(unname(.des_form_group[pair_$HereForm]) != unname(.des_form_group[pair_$HersForm])),
    nDiffYear     = sum(pair_$HereYear != pair_$HersYear)
  )
  tbl_out(
    .tab   = diff_,
    .title = "What differs between the two copies",
    .notes = c(nDiffGroup = "Copies on different filing groups: these contracts sit in a different bar of
                             Figure 2 on her side than here. A count near zero closes Q19.")
  )
  invisible(list(Counts = out_, Differences = diff_))
}

#' Her redaction indicators against this file's
#'
#' With .redaction = "symbolexplicit" and .redact_min = 1 the two must agree on every row from 2008,
#' because that is 103's rule. A disagreement is a definition drift, and it says which side moved.
#'
#' THE ONE DISAGREEMENT THAT IS NOT A DRIFT is a release gap. HasCto changed between exports when 01E
#' relinked the orders, and her .dta files are built from whichever release she last ran 102 on. A
#' disagreement confined to rows where the two text flags agree is that gap and nothing else, and the
#' warning says so, with both dates, rather than leaving the reader to suspect the definition.
#'
#' @param .tab Tibble from `des_read_contracts()` joined to `des_read_ak_contract()`.
#' @param .path_release Character. Contracts.parquet, for its modification date.
#' @param .path_hers Character. Her contract-level .dta, for its modification date.
#' @return Invisibly, the cross-tabulation.
des_check_redaction <- function(.tab, .path_release, .path_hers) {
  if (FALSE) {
    .tab          <- tab_contracts
    .path_release <- .lP$Input$FilContracts
    .path_hers    <- .lP$Input$DtaAkContract
  }

  # BEFORE 2019 THE PAPER'S FLAG IS THE ORDER, so a disagreement there is HasCto and only HasCto. From
  # 2019 it is the markers, so a disagreement there is the marker rule. Era is the column that tells
  # the two apart, which is why the cross-tabulation carries it.
  out_ <- .tab |>
    dplyr::filter(.data$Keep, .data$Year >= 2008L, !is.na(.data$AkRedacted)) |>
    dplyr::mutate(Era = dplyr::if_else(.data$Year <= 2018L, "2008-2018", "2019-")) |>
    dplyr::count(.data$Era, .data$Redacted, .data$AkRedacted, .data$RedactedText, .data$AkRedactedText, name = "n") |>
    dplyr::arrange(.data$Era, dplyr::desc(.data$n))

  tbl_out(.tab = out_, .title = "Redacted and RedactedText, this file against 103, by era")

  off_  <- dplyr::filter(out_, .data$Redacted != .data$AkRedacted | .data$RedactedText != .data$AkRedactedText)
  post_ <- dplyr::filter(off_, .data$Era == "2019-")

  if (nrow(off_) == 0L) {
    cli::cli_alert_success("every contract from 2008 carries the same redaction flags as 103")
    return(invisible(out_))
  }

  date_ <- function(.p) format(as.Date(fs::file_info(.p)$modification_time), "%d %b %Y")
  cli::cli_alert_warning(
    "{format(sum(off_$n), big.mark = ',')} contracts disagree with 103's redaction flags, \\
     {format(sum(post_$n), big.mark = ',')} of them from 2019."
  )
  cli::cli_alert_info(
    "Contracts.parquet is dated {date_(.path_release)}, her contract file {date_(.path_hers)}. A disagreement \\
     before 2019 is HasCto moving between the two releases; one from 2019 is the marker rule itself."
  )
  invisible(out_)
}

#' The two files agree where they overlap
#'
#' Every attached announcement in Summaries has a contract in the descriptive sample on the same
#' HashIndex, save the attachments the ladder placed outside the window or among the malformed. The
#' number that does not is printed, because a large one means the two files come from different
#' renders of 01D.
#'
#' @param .tab_summaries Tibble from `des_read_summaries()`.
#' @return Invisibly, the counts.
des_check_summaries <- function(.tab_summaries) {
  if (FALSE) .tab_summaries <- tab_summaries

  att_ <- dplyr::filter(.tab_summaries, .data$SumAttached == 1L)
  out_ <- tibble::tibble(
    nAttached      = nrow(att_),
    nWithExhibit   = sum(att_$nExhibits > 0L),
    nWithoutInDesc = sum(att_$nExhibits == 0L),
    nDelayed       = sum(.tab_summaries$SumAttached == 0L),
    nSingle        = sum(.tab_summaries$SumIsSingle == 1L)
  )
  tbl_out(.tab = out_, .title = "Announcements against the descriptive sample")
  invisible(out_)
}


# 5. Reporting --------------------------------------------------------------------------------------------------------------

#' Every input, where it is, and whether it is there
#'
#' @param .inputs Named character vector of paths.
#' @return Invisibly, the table printed.
des_report_inputs <- function(.inputs) {
  if (FALSE) .inputs <- unlist(.lP$Input)

  out_ <- tibble::tibble(
    Input    = names(.inputs),
    File     = fs::path_file(unname(.inputs)),
    Exists   = fs::file_exists(unname(.inputs)),
    SizeGB   = round(as.numeric(fs::file_size(unname(.inputs))) / 1e9, 2),
    Modified = as.character(as.Date(fs::file_info(unname(.inputs))$modification_time))
  )
  tbl_out(
    .tab   = out_,
    .title = "Inputs on Ann-Kristin's side",
    .notes = c(Modified = "A .dta newer than its parquet cache is reconverted on the next render.")
  )
  invisible(out_)
}

#' The ladder, the steps and the samples, printed
#'
#' @param .tab_ladder Tibble from `des_table_ladder()`.
#' @param .tab_steps Tibble from `des_table_steps()`.
#' @param .tab_samples Tibble from `des_table_samples()`.
#' @return Invisibly, NULL.
des_report_samples <- function(.tab_ladder, .tab_steps, .tab_samples) {
  if (FALSE) {
    .tab_ladder  <- tab_ladder
    .tab_steps   <- tab_steps
    .tab_samples <- tab_samples
  }

  tbl_out(.tab = .tab_ladder, .title = "Table 2 Panel A, from the ladder columns")
  tbl_out(
    .tab   = .tab_steps,
    .title = "Every row of the file, by ladder step",
    .notes = c(nPrimary = "Primary copies are the attachment grain; rows minus primary is the co-filer fan-out.")
  )
  tbl_out(
    .tab   = .tab_samples,
    .title = "The named samples",
    .notes = c(Sample = "The codes every exhibit in the specification cites.")
  )
  invisible(NULL)
}


# 6. Exhibit helpers --------------------------------------------------------------------------------------------------------
# Every exhibit is three functions with one contract. des_data_<id>(.tab, .sample) filters nothing
# it is not told to, computes, and returns a tibble carrying a Sample column; des_plot_<id>(.tab_data)
# takes ONLY that tibble and returns a ggplot; des_report_<id>(.tab_data) prints the headline numbers
# beside whatever the manuscript printed. The tibble is the artifact: it is what goes to disk, and it
# is enough to redraw the figure without the data it came from.
#
# THE SAMPLE IS AN ARGUMENT, NOT A FILTER INSIDE THE FUNCTION. The runbook defines its samples once,
# as membership columns on the contract table, and hands each exhibit the ones it should be drawn on;
# the same figure on eight samples is the same function called eight times, and the differences
# between the tabs are the ladder.

#' Run one exhibit's compute function over several named samples
#'
#' A SAMPLE IS A LOGICAL COLUMN, NOT A COPY. Eight samples of a 1.4-million-row table held as eight
#' tibbles is eight times the memory for no information; the runbook writes each sample as one
#' membership column on the contract table, and this filters on the column named. The filters stay
#' on the page, in the chunk that creates the columns.
#'
#' @param .tab The contract table carrying the membership columns.
#' @param .samples Character. Membership column names, in the order the tabs should take.
#' @param .fun The exhibit's des_data_* function, taking .tab and .sample.
#' @param ... Further arguments passed to .fun.
#' @return One tibble, the per-sample results row-bound, Sample first.
des_over_samples <- function(.tab, .samples, .fun, ...) {
  if (FALSE) {
    .tab     <- tab_contracts
    .samples <- .des_contract_samples
    .fun     <- des_data_f01
  }
  miss_ <- setdiff(.samples, names(.tab))
  if (length(miss_) > 0L) cli::cli_abort("No membership column{?s} for sample{?s} {miss_}.")

  purrr::map(.samples, \(.s) .fun(.tab = .tab[.tab[[.s]], , drop = FALSE], .sample = .s, ...)) |>
    purrr::list_rbind() |>
    dplyr::relocate("Sample")
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
des_fmt <- function(.tab, .digits = 2L, .counts = c("N", "Rows", "Contracts", "Firms", "Year", "Revision")) {
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

#' Write an exhibit's tibble to Output/Data
#'
#' One parquet per exhibit, every sample inside it under its Sample column, so a reader gets the
#' whole comparison from one file.
#'
#' @param .tab_data The exhibit tibble.
#' @param .name Character. Exhibit id, the file stem.
#' @param .dir Character. Output/Data.
#' @return Invisibly, the path written.
des_save_data <- function(.tab_data, .name, .dir) {
  if (FALSE) {
    .tab_data <- tab_f01
    .name     <- "F01"
    .dir      <- .lP$Output$DirData
  }
  fs::dir_create(.dir)
  path_ <- fs::path(.dir, paste0(.name, ".parquet"))
  arrow::write_parquet(.tab_data, path_)
  cli::cli_alert_success("{(.name)}: {format(nrow(.tab_data), big.mark = ',')} rows to {.file {fs::path_file(path_)}}")
  invisible(path_)
}

#' Draw and write an exhibit once per sample
#'
#' The figure files the manuscript picks from: <id>_<sample>.pdf and .png, one pair per sample, each
#' drawn by the exhibit's own plot function from the sample's slice of the tibble.
#'
#' @param .tab_data The exhibit tibble, carrying Sample.
#' @param .fun The exhibit's des_plot_* function, taking .tab_data.
#' @param .name Character. Exhibit id.
#' @param .dir Character. Output/Figures.
#' @param .height Numeric. Height in inches, from plot_height().
#' @return Invisibly, a character vector of the paths written.
des_save_figures <- function(.tab_data, .fun, .name, .dir, .height) {
  if (FALSE) {
    .tab_data <- tab_f01
    .fun      <- des_plot_f01
    .name     <- "F01"
    .dir      <- .lP$Output$DirFigures
    .height   <- plot_height(8L)
  }
  samples_ <- unique(.tab_data$Sample)
  out_ <- purrr::map(samples_, \(.s) {
    plot_save(
      .plot   = .fun(.tab_data = dplyr::filter(.tab_data, .data$Sample == .s)),
      .name   = paste0(.name, "_", .s),
      .dir    = .dir,
      .height = .height
    )
  }) |>
    unlist()
  cli::cli_alert_success("{(.name)}: {length(samples_)} sample{?s} drawn, {length(out_)} files written")
  invisible(out_)
}

#' One exhibit's slice for one sample
#'
#' @param .tab_data The exhibit tibble.
#' @param .sample Character. The Sample value wanted.
#' @return The rows carrying it; aborts if there are none, because a missing tab should not be blank.
des_slice <- function(.tab_data, .sample) {
  if (FALSE) {
    .tab_data <- tab_f01
    .sample   <- "S2_Descriptive"
  }
  out_ <- dplyr::filter(.tab_data, .data$Sample == .sample)
  if (nrow(out_) == 0L) cli::cli_abort("No rows for sample {.val {(.sample)}}; known: {unique(.tab_data$Sample)}.")
  out_
}


#' Is this code running inside a render
#'
#' knit_child() CALLED FROM THE CONSOLE WRITES INTO THE PROJECT ROOT. Outside a render knitr falls
#' back to its own defaults, and its default fig.path is "figure/" relative to the working directory
#' -- which is how a figure/ folder appeared beside 1_code. Inside a Quarto render the path is the
#' document's own figure-html directory and nothing lands in the root. So the tabset builders knit
#' only under a render; run as a chunk in the editor they draw each sample to the device instead,
#' which is what an interactive run wants anyway.
#'
#' @return Logical.
des_is_knitting <- function() isTRUE(getOption("knitr.in.progress"))


#' A tabset of one exhibit's figure, one tab per sample, written as a knitted child
#'
#' EIGHT SAMPLES TIMES FIFTEEN EXHIBITS IS NOT A THING TO HAND-WRITE. The runbook calls this once per
#' exhibit from a chunk with output: asis; the child it knits holds one labelled figure chunk per
#' sample, so the rendered document is the same as if the chunks had been typed -- labelled figure
#' files, cross-referenceable, captioned -- without the typing. The chunk label is fig-<id>-<sample>
#' in lower case, which is the file name the figure gets under the render's figure directory.
#'
#' THE CHILD IS EVALUATED WHERE IT IS CALLED, so it sees the exhibit tibble and the plot function
#' by the names the caller holds them under; both are passed by name rather than by value for that
#' reason, and knit_child() is told to use the caller's environment.
#'
#' THE CHILD NAMES ITS OWN DEVICE AND WIDTH. Inside a render both come from _quarto.yml; run from the
#' console, knit_child() falls back to knitr's defaults, and the base png device does not know the
#' house font and fails with "invalid font type" on the first label. ragg_png and the project's fixed
#' width are therefore restated in every child chunk, so the figure is the same one whether the chunk
#' is rendered or stepped through. The embedded preview is rasterised at 110 dpi: the manuscript files
#' are written at 300 by plot_save(), and seventy previews at 200 made the document twenty megabytes.
#'
#' SEVERAL FIGURES IN ONE TAB. Where an exhibit is two figures on the page -- Figure 6's states and
#' countries -- .fun_name, .id, .height and .caption are vectors of the same length, and every tab
#' holds one chunk per figure, each with its own label, file and caption. Nothing is patched together.
#'
#' @param .tab_name Character. The name of the exhibit tibble in the calling environment.
#' @param .fun_name Character. The name of the exhibit's plot function; several for several figures.
#' @param .id Character. Exhibit id, lower case, for the chunk labels: "f01"; one per function.
#' @param .height Numeric. Figure height in inches, from plot_height(); one per function or one for all.
#' @param .caption Character. What the figure shows; the sample's own description is appended. One
#'   per function or one for all.
#' @param .samples Character or NULL. Which samples to draw, in tab order; NULL draws every sample in
#'   the tibble, in the order of .des_sample_labels.
#' @return Invisibly, the child markdown. Its knitted form is written to the document as a side
#'   effect, which is why the calling chunk must carry output: asis.
des_tabs_figure <- function(.tab_name, .fun_name, .id, .height, .caption, .samples = NULL) {
  if (FALSE) {
    .tab_name <- "tab_f01"
    .fun_name <- "des_plot_f01"
    .id       <- "f01"
    .height   <- plot_height(8L)
    .caption  <- "Contracts by form, original and amended, share of the sample above each bar."
    .samples  <- NULL
  }
  n_fig_ <- length(.fun_name)
  if (length(.id) != n_fig_) cli::cli_abort("des_tabs_figure(): .id must name one id per plot function.")
  .height  <- rep_len(.height, n_fig_)
  .caption <- rep_len(.caption, n_fig_)

  env_  <- parent.frame()
  tab_  <- get(.tab_name, envir = env_)
  have_ <- unique(tab_$Sample)
  want_ <- if (is.null(.samples)) names(.des_sample_labels)[names(.des_sample_labels) %in% have_] else .samples
  miss_ <- setdiff(want_, have_)
  if (length(miss_) > 0L) cli::cli_abort("{(.id[1L])}: no rows for sample{?s} {miss_}.")

  # INTERACTIVE: one plot per sample and figure to the device, no child, nothing written anywhere.
  if (!des_is_knitting()) {
    cli::cli_alert_info("{(.id[1L])}: not rendering; drawing {length(want_) * n_fig_} figure{?s} to the device.")
    for (s_ in want_) for (k_ in seq_len(n_fig_)) print(get(.fun_name[k_], envir = env_)(.tab_data = des_slice(tab_, s_)))
    return(invisible(NULL))
  }

  one_ <- function(.s) {
    chunks_ <- purrr::map_chr(seq_len(n_fig_), \(.k) {
      label_ <- paste0("fig-", .id[.k], "-", tolower(gsub("_", "-", .s)))
      cap_   <- paste0(.caption[.k], " Sample ", gsub("_", " ", .s), ": ", .des_sample_labels[[.s]], ".")
      paste0(
        "```{r ", label_, ", dev=\"ragg_png\", dpi=110, fig.width=7.5, fig.height=", format(.height[.k], nsmall = 1),
        ", fig.cap=\"", cap_, "\"}\n",
        .fun_name[.k], "(.tab_data = des_slice(", .tab_name, ", \"", .s, "\"))\n",
        "```\n"
      )
    })
    paste0("### ", gsub("_", " ", .s), "\n\n", paste(chunks_, collapse = "\n"))
  }

  child_ <- paste0(
    "::: {.panel-tabset}\n\n",
    paste(purrr::map_chr(want_, one_), collapse = "\n"),
    "\n:::\n"
  )

  cat(knitr::knit_child(text = child_, envir = env_, quiet = TRUE), sep = "\n")
  invisible(child_)
}


# 7. F01: distribution of filing types --------------------------------------------------------------------------------------
# The paper's Figure 1: contracts by the form they arrived in, US forms and foreign forms side by
# side, each bar split into original agreements and amendments, with the form's share of the sample
# printed above it.

# What revision 1 prints above each bar. A prediction for S2; the other samples have no reference.
.des_reference_f01 <- tibble::tribble(
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
#' @param .tab One of the named samples: a contract tibble from `des_read_contracts()`.
#' @param .sample Character. The sample's name, carried on every row.
#' @return Tibble: Sample, Region, FormFamily, Amend, N, NForm, Share (of the sample), ShareAmend
#'   (of the form).
des_data_f01 <- function(.tab, .sample) {
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
    dplyr::arrange(match(.data$FormFamily, .des_form_levels), .data$Amend)
}

#' Figure 1: stacked bars by form, US and foreign panels, share labels above the bars
#'
#' Built directly rather than through plot_bar_stacked(), which draws horizontal ranked bars: this
#' figure is vertical, faceted, and labelled with a share that is not the bar's own height. The two
#' panels share the count axis and take their width from their number of forms, so a bar is the same
#' width on both sides and the foreign panel does not stretch three forms across half the page.
#'
#' @param .tab_data One sample's rows from `des_data_f01()`.
#' @return A ggplot.
des_plot_f01 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f01, "S2_Descriptive")

  # The two registered greys, and white for the unlabelled segment: a third grey would sit between
  # them and be read as one of them. Hairline borders on every segment keep white visible.
  fill_ <- c(plot_entry("AmendType")$Colours, Unlabelled = "#FFFFFF")

  dat_ <- .tab_data |>
    dplyr::mutate(
      PlotForm = factor(.data$FormFamily, levels = .des_form_levels),
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
      data    = lab_,
      mapping = ggplot2::aes(x = .data$PlotForm, y = .data$NForm, label = .data$Label),
      inherit.aes = FALSE,
      vjust   = -0.4,
      size    = .plot_base / ggplot2::.pt * 0.85,
      family  = .plot_font
    ) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$Region), scales = "free_x", space = "free_x") +
    ggplot2::scale_fill_manual(values = fill_, breaks = names(fill_)[names(fill_) %in% dat_$Amend], name = NULL) +
    plot_scale_y_count(.expand = c(0, 0.10)) +
    ggplot2::labs(x = NULL, y = "Contracts") +
    plot_theme(.grid = "y", .legend = "bottom")
}

#' Figure 1 in numbers: each form's share of each sample, beside the revision's label
#'
#' @param .tab_data Tibble from `des_data_f01()`, every sample.
#' @param .ref Tibble. The revision's labels for S2.
#' @return Invisibly, the wide table printed.
des_report_f01 <- function(.tab_data, .ref = .des_reference_f01) {
  if (FALSE) {
    .tab_data <- tab_f01
    .ref      <- .des_reference_f01
  }

  wide_ <- .tab_data |>
    dplyr::distinct(.data$Sample, .data$FormFamily, .data$Share) |>
    dplyr::mutate(Share = round(100 * .data$Share, 1)) |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Share") |>
    dplyr::left_join(.ref, by = dplyr::join_by(FormFamily)) |>
    dplyr::arrange(match(.data$FormFamily, .des_form_levels))

  tbl_out(
    .tab = des_fmt(wide_, 1L),
    .title  = "Figure 1: share of contracts by form, percent, one column per sample",
    .notes = c(Revision = "The label printed above the bar in revision 1, which was drawn on S2.")
  )

  amend_ <- .tab_data |>
    dplyr::filter(.data$Amend == "Amended") |>
    dplyr::distinct(.data$Sample, .data$FormFamily, .data$ShareAmend) |>
    dplyr::mutate(ShareAmend = round(100 * .data$ShareAmend, 1)) |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "ShareAmend") |>
    dplyr::arrange(match(.data$FormFamily, .des_form_levels))

  tbl_out(
    .tab = des_fmt(amend_, 1L),
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


# 8. Shared shapes ----------------------------------------------------------------------------------------------------------
# Two figure shapes several exhibits share, kept here rather than in _Plots.R until a second script
# needs them. Both take a tidy tibble and column names, add no title, and return a themed ggplot.

# Regime lines every time series carries: the 8-K item reform and the FAST Act.
.des_regime_dates <- c(Reform2004 = as.Date("2004-08-23"), Fast2019 = as.Date("2019-04-02"))
.des_regime_years <- c(Reform2004 = 2004 + 235 / 366, Fast2019 = 2019 + 92 / 365)

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
des_shape_lines <- function(.tab, .x, .y, .group = NULL, .facet = NULL, .key = NULL, .key_facet = NULL,
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
  regimes_ <- .des_regime_years[.des_regime_years >= min(dat_$PlotX) - 0.5 & .des_regime_years <= max(dat_$PlotX) + 0.5]
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
des_shape_share_stack <- function(.tab, .year, .n, .fill, .key = NULL, .line = NULL, .line_y = "Share",
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
    ggplot2::geom_vline(xintercept = unname(.des_regime_years), linetype = "dashed", colour = "black",
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

#' A descriptive table with one row per group and Total: N, mean, SD and three quantiles
#'
#' @param .tab Tibble.
#' @param .value Character. The column summarised.
#' @param .group Character. The grouping column.
#' @return Tibble: Group, N, Mean, SD, P25, P50, P75, with a Total row last.
des_stats_by <- function(.tab, .value, .group) {
  if (FALSE) {
    .tab   <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .value <- "Words"
    .group <- "ClassPaper"
  }
  one_ <- function(.d, .g) {
    v_ <- .d[[.value]]
    tibble::tibble(
      Group = .g,
      N     = sum(!is.na(v_)),
      Mean  = mean(v_, na.rm = TRUE),
      SD    = stats::sd(v_, na.rm = TRUE),
      P25   = stats::quantile(v_, 0.25, na.rm = TRUE, names = FALSE),
      P50   = stats::median(v_, na.rm = TRUE),
      P75   = stats::quantile(v_, 0.75, na.rm = TRUE, names = FALSE)
    )
  }
  groups_ <- .tab |> dplyr::filter(!is.na(.data[[.group]])) |> dplyr::group_split(.data[[.group]])
  by_ <- purrr::map(groups_, \(.d) one_(.d, as.character(.d[[.group]][1L]))) |> purrr::list_rbind()
  dplyr::bind_rows(by_, one_(.tab, "Total"))
}

#' Write a tibble as a booktabs tabular
#'
#' The manuscript's tables are LaTeX; this writes the body the paper includes, numbers formatted as
#' the column's magnitude wants, and nothing else. Caption and notes belong to the manuscript.
#'
#' @param .tab Tibble. Character first column, numeric others.
#' @param .path Character. Destination .tex.
#' @param .digits Named integer or single integer. Decimals per numeric column, or for all.
#' @return Invisibly, the path.
des_tex_table <- function(.tab, .path, .digits = 2L) {
  if (FALSE) {
    .tab    <- tab_t03 |> dplyr::filter(.data$Sample == "S2_Descriptive", .data$Panel == "Words")
    .path   <- fs::path(.lP$Output$DirTables, "T03_S2_Descriptive_Words.tex")
    .digits <- c(N = 0L, Mean = 0L, SD = 0L, P25 = 0L, P50 = 0L, P75 = 0L)
  }
  num_ <- names(.tab)[purrr::map_lgl(.tab, is.numeric)]
  dig_ <- if (length(.digits) == 1L && is.null(names(.digits))) {
    purrr::set_names(rep(.digits, length(num_)), num_)
  } else {
    .digits
  }

  fmt_ <- .tab |>
    dplyr::mutate(dplyr::across(dplyr::all_of(num_), \(.x) {
      d_ <- dig_[[dplyr::cur_column()]]
      dplyr::if_else(is.na(.x), "", formatC(.x, format = "f", digits = d_, big.mark = ","))
    })) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), \(.x) gsub("&", "\\\\&", .x, fixed = TRUE)))

  align_ <- paste0("l", strrep("r", ncol(.tab) - 1L))
  head_  <- paste(names(.tab), collapse = " & ")
  body_  <- apply(as.matrix(fmt_), 1L, paste, collapse = " & ")

  lines_ <- c(
    paste0("\\begin{tabular}{", align_, "}"),
    "\\toprule",
    paste0(head_, " \\\\"),
    "\\midrule",
    paste0(body_, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  )
  fs::dir_create(fs::path_dir(.path))
  writeLines(lines_, .path)
  invisible(.path)
}

#' A tabset of one exhibit's table, one tab per sample, written as a knitted child
#'
#' The table twin of des_tabs_figure(): the child chunks print rather than plot, through the
#' exhibit's own print function, which takes one sample's rows.
#'
#' @param .tab_name Character. The exhibit tibble, by name.
#' @param .fun_name Character. The exhibit's print function, by name, taking .tab_data.
#' @param .id Character. Exhibit id for the chunk labels.
#' @param .samples Character or NULL. Which samples, in tab order; NULL is every one present.
#' @return Invisibly, the child markdown.
des_tabs_table <- function(.tab_name, .fun_name, .id, .samples = NULL) {
  if (FALSE) {
    .tab_name <- "tab_t03"
    .fun_name <- "des_print_t03"
    .id       <- "t03"
    .samples  <- NULL
  }
  env_  <- parent.frame()
  tab_  <- get(.tab_name, envir = env_)
  have_ <- unique(tab_$Sample)
  want_ <- if (is.null(.samples)) names(.des_sample_labels)[names(.des_sample_labels) %in% have_] else .samples

  # INTERACTIVE: one table per sample to the console, no child.
  if (!des_is_knitting()) {
    fun_ <- get(.fun_name, envir = env_)
    cli::cli_alert_info("{(.id)}: not rendering; printing {length(want_)} sample{?s}.")
    for (s_ in want_) fun_(.tab_data = des_slice(tab_, s_))
    return(invisible(NULL))
  }

  one_ <- function(.s) {
    label_ <- paste0("tbl-", .id, "-", tolower(gsub("_", "-", .s)))
    paste0(
      "### ", gsub("_", " ", .s), "\n\n",
      "```{r ", label_, ", message=TRUE, comment=\"\"}\n",
      .fun_name, "(.tab_data = des_slice(", .tab_name, ", \"", .s, "\"))\n",
      "```\n"
    )
  }
  child_ <- paste0("::: {.panel-tabset}\n\n", paste(purrr::map_chr(want_, one_), collapse = "\n"), "\n:::\n")
  cat(knitr::knit_child(text = child_, envir = env_, quiet = TRUE), sep = "\n")
  invisible(child_)
}


# 9. F02: filings over time -------------------------------------------------------------------------------------------------
# The paper's Figure 2: each year's contracts split into the three filing groups, shares stacked to
# one, and the year's total as a line. Kept at year x form family so both groupings -- the paper's
# three and the four that separate registration statements from 8-Ks -- derive from one tibble.

.des_reference_f02 <- tibble::tribble(
  ~Year,  ~Quantity,        ~Revision,
  2021L,  "Contracts",      59969,     # read off the figure; the text still says 80 thousand
  2003L,  "Ad-hoc share",   0.30,      # "around one third" before the reform
  2006L,  "Ad-hoc share",   0.62       # "around 60%" after it
)

#' Contracts per year and form family
#'
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Year, FormFamily, FilingGroup, FilingGroup4, N.
des_data_f02 <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample <- "S2_Descriptive"
  }
  .tab |>
    dplyr::count(.data$Year, .data$FormFamily, name = "N") |>
    dplyr::mutate(
      Sample       = .sample,
      FilingGroup  = unname(.des_form_group[.data$FormFamily]),
      FilingGroup4 = dplyr::case_when(
        .data$FormFamily == "8-K"                              ~ "8-K",
        .data$FormFamily %in% c("S-1", "S-4", "F-1", "F-4")    ~ "Registration",
        .default                                               = .data$FilingGroup
      )
    ) |>
    dplyr::arrange(.data$Year, match(.data$FormFamily, .des_form_levels))
}

#' Figure 2, three groups as the paper draws it
#' @param .tab_data One sample's rows from `des_data_f02()`.
#' @return A patchwork.
des_plot_f02 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f02, "S2_Descriptive")
  .tab_data |>
    dplyr::summarise(N = sum(.data$N), .by = c("Year", "FilingGroup")) |>
    des_shape_share_stack(.year = "Year", .n = "N", .fill = "FilingGroup", .key = "FilingGroup", .count = TRUE)
}

#' Figure 2, four groups: registration statements on their own
#' @param .tab_data One sample's rows from `des_data_f02()`.
#' @return A patchwork.
des_plot_f02b <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f02, "S2_Descriptive")
  .tab_data |>
    dplyr::summarise(N = sum(.data$N), .by = c("Year", "FilingGroup4")) |>
    des_shape_share_stack(.year = "Year", .n = "N", .fill = "FilingGroup4", .key = "FilingGroup4", .count = TRUE)
}

#' Figure 2 in numbers: yearly totals and the four group shares, per sample
#' @param .tab_data Every sample from `des_data_f02()`.
#' @return Invisibly, the long table.
des_report_f02 <- function(.tab_data) {
  if (FALSE) .tab_data <- tab_f02

  long_ <- .tab_data |>
    dplyr::summarise(N = sum(.data$N), .by = c("Sample", "Year", "FilingGroup4")) |>
    dplyr::mutate(Total = sum(.data$N), Share = .data$N / sum(.data$N), .by = c("Sample", "Year"))

  tot_ <- long_ |>
    dplyr::distinct(.data$Sample, .data$Year, .data$Total) |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Total")
  tbl_out(.tab = des_fmt(tot_, 0L), .title = "Figure 2: contracts per year, one column per sample",
          .notes = c(Year = "2021 is the year the text calls 80 thousand; read it here."))

  s2_ <- long_ |>
    dplyr::filter(.data$Sample == "S2_Descriptive") |>
    dplyr::mutate(Share = round(100 * .data$Share, 1)) |>
    dplyr::select("Year", "FilingGroup4", "Share") |>
    tidyr::pivot_wider(names_from = "FilingGroup4", values_from = "Share")
  tbl_out(.tab = des_fmt(s2_, 1L), .title = "Figure 2: group shares by year on S2, percent, four groups",
          .notes = c(Registration = "S-1, S-4, F-1 and F-4, which the paper folds into ad-hoc. Read 2020-2021."))
  invisible(long_)
}


# 10. F03: contract types ---------------------------------------------------------------------------------------------------
# The paper's Figure 3: Panel A the count and share of each type, Panel B contracts per firm per
# year of that type, over the firm-years in which the firm filed at least one of it.

.des_reference_f03 <- tibble::tribble(
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
des_data_f03 <- function(.tab, .sample) {
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
    dplyr::arrange(match(.data$Class, c(.des_class_levels, "Unlabelled")))
}

#' Figure 3: two horizontal panels, count with share labels and contracts per firm-year
#' @param .tab_data One sample's rows from `des_data_f03()`.
#' @return A patchwork.
des_plot_f03 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f03, "S2_Descriptive")

  dat_ <- .tab_data |>
    dplyr::filter(.data$Class != "Unlabelled") |>
    dplyr::mutate(PlotClass = plot_factor(.data$Class, .key = "ClassPaper", .short = TRUE, .rev = TRUE))

  a_ <- ggplot2::ggplot(dat_, ggplot2::aes(y = .data$PlotClass, x = .data$N / 1000)) +
    ggplot2::geom_col(fill = .plot_ink, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = scales::label_percent(accuracy = 0.1)(.data$Share)),
                       hjust = -0.15, size = .plot_base / ggplot2::.pt * 0.8, family = .plot_font) +
    plot_scale_x_count(.expand = c(0, 0.25)) +
    ggplot2::labs(x = "Contracts (000)", y = NULL) +
    plot_theme(.grid = "x", .legend = "none")

  b_ <- ggplot2::ggplot(dat_, ggplot2::aes(y = .data$PlotClass, x = .data$PerFirmYear)) +
    ggplot2::geom_col(fill = .plot_ref, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = formatC(.data$PerFirmYear, format = "f", digits = 2)),
                       hjust = -0.15, size = .plot_base / ggplot2::.pt * 0.8, family = .plot_font) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.25))) +
    ggplot2::labs(x = "Contracts per firm-year", y = NULL) +
    plot_theme(.grid = "x", .legend = "none") +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

  patchwork::wrap_plots(a_, b_, ncol = 2L, widths = c(1.6, 1))
}

#' Figure 3 in numbers: class shares per sample beside the revision's labels
#' @param .tab_data Every sample from `des_data_f03()`.
#' @return Invisibly, the wide table.
des_report_f03 <- function(.tab_data, .ref = .des_reference_f03) {
  if (FALSE) {
    .tab_data <- tab_f03
    .ref      <- .des_reference_f03
  }
  wide_ <- .tab_data |>
    dplyr::mutate(Share = round(100 * .data$Share, 2)) |>
    dplyr::select("Sample", "Class", "Share") |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Share") |>
    dplyr::left_join(.ref, by = dplyr::join_by(Class)) |>
    dplyr::mutate(Class = dplyr::coalesce(.des_class_short[match(.data$Class, .des_class_levels)], .data$Class))
  tbl_out(.tab = des_fmt(wide_, 2L), .title = "Figure 3 Panel A: share of contracts by type, percent, per sample",
          .notes = c(Revision = "The label at the end of each bar in revision 1, drawn on S2."))

  pf_ <- .tab_data |>
    dplyr::filter(.data$Class != "Unlabelled") |>
    dplyr::select("Sample", "Class", "PerFirmYear") |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "PerFirmYear") |>
    dplyr::mutate(Class = .des_class_short[match(.data$Class, .des_class_levels)])
  tbl_out(.tab = des_fmt(pf_, 2L), .title = "Figure 3 Panel B: contracts per firm-year of the type, per sample",
          .notes = c(Class = "Mean over CIK-years with at least one contract of the type."))
  invisible(wide_)
}


# 11. F04: contract types over time -----------------------------------------------------------------------------------------
# The paper's Figure 4: each year's contracts by type, shares stacked to one, with the amendment
# share as a line; and its Online Appendix C line versions, which the same tibble draws as small
# multiples.

#' Contracts per year and type, with the year's amendment share
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Year, Class, N, Share, ShareAmend (of the year, repeated on its rows).
des_data_f04 <- function(.tab, .sample) {
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
    dplyr::arrange(.data$Year, match(.data$Class, .des_class_levels))
}

#' Figure 4: stacked type shares by year with the amendment share over them
#' @param .tab_data One sample's rows from `des_data_f04()`.
#' @return A ggplot.
des_plot_f04 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f04, "S2_Descriptive")
  line_ <- dplyr::distinct(.tab_data, .data$Year, .data$ShareAmend)
  des_shape_share_stack(
    .tab = .tab_data, .year = "Year", .n = "N", .fill = "Class", .key = "ClassPaper",
    .line = line_, .line_y = "ShareAmend", .line_name = "Share amended", .count = FALSE
  )
}

#' Online Appendix C: the same shares as twelve small multiples
#' @param .tab_data One sample's rows from `des_data_f04()`.
#' @return A ggplot.
des_plot_f04b <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f04, "S2_Descriptive")
  des_shape_lines(.tab = .tab_data, .x = "Year", .y = "Share", .group = NULL, .facet = "Class",
                  .key_facet = "ClassPaper", .pct = TRUE, .ncol = 4L, .free_y = FALSE, .ylab = "Share of contracts")
}

#' Figure 4 in numbers: amendment share by year per sample, and the equity share
#' @param .tab_data Every sample from `des_data_f04()`.
#' @return Invisibly, the amendment table.
des_report_f04 <- function(.tab_data) {
  if (FALSE) .tab_data <- tab_f04
  am_ <- .tab_data |>
    dplyr::distinct(.data$Sample, .data$Year, .data$ShareAmend) |>
    dplyr::mutate(ShareAmend = round(100 * .data$ShareAmend, 1)) |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "ShareAmend")
  tbl_out(.tab = des_fmt(am_, 1L), .title = "Figure 4: amendments as a share of the year's contracts, percent, per sample",
          .notes = c(Year = "The text says about 20 percent; the figure has said about 30 since the reclassification."))

  eq_ <- .tab_data |>
    dplyr::filter(.data$Class == "Financial Instruments: Equity") |>
    dplyr::mutate(Share = round(100 * .data$Share, 1)) |>
    dplyr::select("Sample", "Year", "Share") |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Share")
  tbl_out(.tab = des_fmt(eq_, 1L), .title = "Figure 4: the equity share by year, percent, per sample",
          .notes = c(Year = "2020-2021 against S2s_Seasoned is Online Appendix Figure C2's argument."))
  invisible(am_)
}


# 12. F05: content characteristics over time --------------------------------------------------------------------------------
# The paper's Figure 5: mean and median per year of duration, countries, parties and words.

.des_reference_f05 <- tibble::tribble(
  ~Measure,    ~Quantity, ~Revision,
  "Words",     "mean",    8825,
  "Duration",  "mean",    2.79,
  "Parties",   "mean",    2.94,    # old pipeline; the text still prints it
  "Countries", "mean",    1.9      # old pipeline; the text still prints it
)

#' Mean and median of the four content measures by year
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Year, Measure, N, Mean, Median.
des_data_f05 <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample <- "S2_Descriptive"
  }
  .tab |>
    dplyr::select("Year", Duration = "DurationYrs", "Countries", "Parties", "Words") |>
    tidyr::pivot_longer(-"Year", names_to = "Measure", values_to = "Value") |>
    dplyr::filter(!is.na(.data$Value)) |>
    dplyr::summarise(N = dplyr::n(), Mean = mean(.data$Value), Median = stats::median(.data$Value),
                     .by = c("Year", "Measure")) |>
    dplyr::mutate(
      Sample  = .sample,
      Measure = factor(.data$Measure, levels = c("Duration", "Countries", "Parties", "Words"))
    ) |>
    dplyr::arrange(.data$Measure, .data$Year)
}

#' Figure 5: four panels, mean and median lines
#' @param .tab_data One sample's rows from `des_data_f05()`.
#' @return A ggplot.
des_plot_f05 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f05, "S2_Descriptive")
  long_ <- .tab_data |>
    dplyr::mutate(Words = .data$Measure == "Words") |>
    tidyr::pivot_longer(c("Mean", "Median"), names_to = "Statistic", values_to = "Value") |>
    dplyr::mutate(
      Value   = dplyr::if_else(.data$Words, .data$Value / 1000, .data$Value),
      Measure = factor(dplyr::case_when(
        .data$Measure == "Duration"  ~ "Duration (years)",
        .data$Measure == "Countries" ~ "Countries mentioned",
        .data$Measure == "Parties"   ~ "Parties",
        .default                     = "Words (000)"
      ), levels = c("Duration (years)", "Countries mentioned", "Parties", "Words (000)"))
    )
  des_shape_lines(.tab = long_, .x = "Year", .y = "Value", .group = "Statistic", .facet = "Measure",
                  .pct = FALSE, .ncol = 2L, .free_y = TRUE, .ylab = NULL)
}

#' Figure 5 in numbers: sample-period means and medians per measure and sample
#' @param .tab_data Every sample from `des_data_f05()`.
#' @return Invisibly, the table.
des_report_f05 <- function(.tab_data, .ref = .des_reference_f05) {
  if (FALSE) {
    .tab_data <- tab_f05
    .ref      <- .des_reference_f05
  }
  # The whole-period mean is the N-weighted mean of the yearly means; the median is not recoverable
  # from yearly medians and is reported from the years' median of medians, labelled as such.
  wide_ <- .tab_data |>
    dplyr::summarise(Mean = sum(.data$Mean * .data$N) / sum(.data$N), .by = c("Sample", "Measure")) |>
    dplyr::mutate(Mean = round(.data$Mean, 2)) |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Mean") |>
    dplyr::left_join(dplyr::select(.ref, "Measure", "Revision"), by = dplyr::join_by(Measure)) |>
    dplyr::mutate(Measure = as.character(.data$Measure))
  tbl_out(.tab = des_fmt(wide_, 2L), .title = "Figure 5: sample-period means per measure and sample",
          .notes = c(Revision = "What the text prints. Parties and countries are old-pipeline numbers."))
  invisible(wide_)
}


# 13. T03: length and duration by type --------------------------------------------------------------------------------------
# The paper's Table 3: words and duration by type, N, mean, SD and quartiles, Total last; plus the
# panel referee 2 asked for, the redaction share by type before and after the FAST Act.

.des_reference_t03 <- tibble::tribble(
  ~Panel,      ~Quantity,  ~Revision,
  "Words",     "Mean",     8831,
  "Words",     "P50",      3848,
  "Duration",  "Mean",     2.79,
  "Duration",  "SD",       3.28,
  "Duration",  "P50",      1.50,
  "Duration",  "N",        779440
)

#' Words and duration by type, and the redaction share by type and FAST period
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @return Tibble, long: Sample, Panel, Group, N, Mean, SD, P25, P50, P75; Panel "Redaction" carries
#'   Mean = share and N only, with Group suffixed by period.
des_data_t03 <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample <- "S2_Descriptive"
  }
  w_ <- des_stats_by(.tab = .tab, .value = "Words",       .group = "ClassPaper") |> dplyr::mutate(Panel = "Words")
  d_ <- des_stats_by(.tab = .tab, .value = "DurationYrs", .group = "ClassPaper") |> dplyr::mutate(Panel = "Duration")

  r_ <- .tab |>
    dplyr::filter(!is.na(.data$Redacted), !is.na(.data$ClassPaper)) |>
    dplyr::mutate(Period = dplyr::if_else(.data$PostFast == 1L, "Post-FAST", "Pre-FAST")) |>
    dplyr::summarise(N = dplyr::n(), Mean = mean(.data$Redacted), .by = c("ClassPaper", "Period")) |>
    dplyr::bind_rows(
      .tab |>
        dplyr::filter(!is.na(.data$Redacted)) |>
        dplyr::mutate(Period = dplyr::if_else(.data$PostFast == 1L, "Post-FAST", "Pre-FAST")) |>
        dplyr::summarise(N = dplyr::n(), Mean = mean(.data$Redacted), ClassPaper = factor("Total"), .by = "Period")
    ) |>
    dplyr::mutate(Panel = "Redaction", Group = as.character(.data$ClassPaper)) |>
    dplyr::select("Panel", "Group", "Period", "N", "Mean")

  dplyr::bind_rows(w_, d_) |>
    dplyr::bind_rows(r_) |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::relocate("Sample", "Panel", "Group")
}

#' Table 3 printed: the three panels for one sample
#' @param .tab_data One sample's rows from `des_data_t03()`.
#' @return Invisibly, NULL.
des_print_t03 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_t03, "S2_Descriptive")
  short_ <- function(.g) dplyr::coalesce(.des_class_short[match(.g, .des_class_levels)], .g)

  w_ <- .tab_data |> dplyr::filter(.data$Panel == "Words") |>
    dplyr::transmute(Type = short_(.data$Group), .data$N, .data$Mean, .data$SD, .data$P25, .data$P50, .data$P75)
  tbl_out(.tab = des_fmt(w_, 0L), .title = "Table 3 Panel A: words per contract by type")

  d_ <- .tab_data |> dplyr::filter(.data$Panel == "Duration") |>
    dplyr::transmute(Type = short_(.data$Group), .data$N, .data$Mean, .data$SD, .data$P25, .data$P50, .data$P75)
  tbl_out(.tab = des_fmt(d_, 2L), .title = "Table 3 Panel B: duration in years by type")

  r_ <- .tab_data |> dplyr::filter(.data$Panel == "Redaction") |>
    dplyr::mutate(Share = round(100 * .data$Mean, 1)) |>
    dplyr::select("Group", "Period", "N", "Share") |>
    tidyr::pivot_wider(names_from = "Period", values_from = c("N", "Share")) |>
    dplyr::mutate(Type = short_(.data$Group)) |>
    dplyr::arrange(match(.data$Group, c(.des_class_levels, "Total"))) |>
    dplyr::select("Type", dplyr::any_of(c("N_Pre-FAST", "Share_Pre-FAST", "N_Post-FAST", "Share_Post-FAST")))
  tbl_out(.tab = des_fmt(r_, 1L),
          .title = "Table 3 Panel C: share redacted by type, before and after the FAST Act, percent",
          .notes = c(Type = "Redacted is the order before April 2019 and the markers after it; rows from 2008 only."))
  invisible(NULL)
}

#' Table 3 to .tex, three files per sample
#' @param .tab_data One sample's rows from `des_data_t03()`.
#' @param .sample Character. The sample, for the file names.
#' @param .dir Character. Output/Tables.
#' @return Invisibly, the paths.
des_tex_t03 <- function(.tab_data, .sample, .dir) {
  if (FALSE) {
    .tab_data <- des_slice(tab_t03, "S2_Descriptive")
    .sample   <- "S2_Descriptive"
    .dir      <- .lP$Output$DirTables
  }
  short_ <- function(.g) dplyr::coalesce(.des_class_short[match(.g, .des_class_levels)], .g)
  stats_ <- function(.p) .tab_data |> dplyr::filter(.data$Panel == .p) |>
    dplyr::transmute(Type = short_(.data$Group), .data$N, .data$Mean, .data$SD, .data$P25, .data$P50, .data$P75)

  p1_ <- des_tex_table(stats_("Words"), fs::path(.dir, paste0("T03_", .sample, "_Words.tex")),
                       .digits = c(N = 0L, Mean = 0L, SD = 0L, P25 = 0L, P50 = 0L, P75 = 0L))
  p2_ <- des_tex_table(stats_("Duration"), fs::path(.dir, paste0("T03_", .sample, "_Duration.tex")),
                       .digits = c(N = 0L, Mean = 2L, SD = 2L, P25 = 2L, P50 = 2L, P75 = 2L))
  r_ <- .tab_data |> dplyr::filter(.data$Panel == "Redaction") |>
    dplyr::mutate(Share = 100 * .data$Mean) |>
    dplyr::select("Group", "Period", "N", "Share") |>
    tidyr::pivot_wider(names_from = "Period", values_from = c("N", "Share")) |>
    dplyr::arrange(match(.data$Group, c(.des_class_levels, "Total"))) |>
    dplyr::transmute(Type = short_(.data$Group), .data[["N_Pre-FAST"]], .data[["Share_Pre-FAST"]],
                     .data[["N_Post-FAST"]], .data[["Share_Post-FAST"]])
  names(r_) <- c("Type", "N pre", "Share pre", "N post", "Share post")
  p3_ <- des_tex_table(r_, fs::path(.dir, paste0("T03_", .sample, "_Redaction.tex")),
                       .digits = c("N pre" = 0L, "Share pre" = 1L, "N post" = 0L, "Share post" = 1L))
  invisible(c(p1_, p2_, p3_))
}

#' Table 3 in numbers: the Total rows per sample beside the revision
#' @param .tab_data Every sample from `des_data_t03()`.
#' @return Invisibly, the table.
des_report_t03 <- function(.tab_data, .ref = .des_reference_t03) {
  if (FALSE) {
    .tab_data <- tab_t03
    .ref      <- .des_reference_t03
  }
  tot_ <- .tab_data |>
    dplyr::filter(.data$Group == "Total", .data$Panel != "Redaction") |>
    dplyr::select("Sample", "Panel", "N", "Mean", "SD", "P50") |>
    tidyr::pivot_longer(c("N", "Mean", "SD", "P50"), names_to = "Quantity", values_to = "Value") |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Value") |>
    dplyr::left_join(.ref, by = dplyr::join_by(Panel, Quantity))
  tbl_out(.tab = des_fmt(tot_, 2L), .title = "Table 3: the Total rows per sample, beside revision 1",
          .notes = c(Revision = "Words 8,831 is the table; the text prints 8,825, which is nWordsAdj on S2."))
  invisible(tot_)
}


# 14. F10: redactions over time ---------------------------------------------------------------------------------------------
# The paper's Figure 10: the share of contracts redacted per year, by the order and by the text, from
# 2008. Three series: the order alone (to 2018), the markers alone, and the paper's union.

plot_register_levels(
  .key     = "RedactSeries",
  .levels  = c("CtoOnly", "TextOnly", "Union"),
  .short   = c("CTO identification", "Marker identification", "CTO or markers (paper)"),
  .colours = c("#60a3d9", "#B7791F", "#002147")
)

.des_reference_f10 <- tibble::tribble(
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
des_data_f10 <- function(.tab, .sample) {
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

#' Figure 10: three lines from 2008 with the FAST Act marked
#' @param .tab_data One sample's rows from `des_data_f10()`.
#' @return A ggplot.
des_plot_f10 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f10, "S6_Redaction")
  des_shape_lines(.tab = .tab_data, .x = "Year", .y = "Share", .group = "Series", .key = "RedactSeries",
                  .pct = TRUE, .regimes = TRUE, .ylab = "Share of contracts redacted")
}

#' Figure 10 in numbers: the union series per sample beside the revision's readings
#' @param .tab_data Every sample from `des_data_f10()`.
#' @return Invisibly, the table.
des_report_f10 <- function(.tab_data, .ref = .des_reference_f10) {
  if (FALSE) {
    .tab_data <- tab_f10
    .ref      <- .des_reference_f10
  }
  wide_ <- .tab_data |>
    dplyr::filter(.data$Series != "TextOnly") |>
    dplyr::mutate(Share = round(100 * .data$Share, 2)) |>
    dplyr::select("Sample", "Year", "Series", "Share") |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Share") |>
    dplyr::left_join(.ref, by = dplyr::join_by(Year, Series)) |>
    dplyr::arrange(.data$Series, .data$Year)
  tbl_out(.tab = des_fmt(wide_, 2L),
          .title = "Figure 10: share redacted per year, percent, CTO and union series per sample",
          .notes = c(Revision = "Read off the revision's figure. The union is the paper's RegEx line."))
  invisible(wide_)
}


# 15. FF1: redaction share by type over time --------------------------------------------------------------------------------
# Appendix Figure F1: the paper's redacted share per year and type, twelve lines in the manuscript,
# twelve small multiples here.

#' Share redacted per year and type
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Year, Class, N, Share.
des_data_ff1 <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S6_Redaction, , drop = FALSE]
    .sample <- "S6_Redaction"
  }
  .tab |>
    dplyr::filter(.data$Year >= 2008L, !is.na(.data$Redacted), !is.na(.data$Class)) |>
    dplyr::summarise(N = dplyr::n(), Share = mean(.data$Redacted), .by = c("Year", "Class")) |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::arrange(match(.data$Class, .des_class_levels), .data$Year)
}

#' Appendix F1: twelve small multiples, shared y
#' @param .tab_data One sample's rows from `des_data_ff1()`.
#' @return A ggplot.
des_plot_ff1 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_ff1, "S6_Redaction")
  des_shape_lines(.tab = .tab_data, .x = "Year", .y = "Share", .group = NULL, .facet = "Class",
                  .key_facet = "ClassPaper", .pct = TRUE, .ncol = 4L, .free_y = FALSE, .ylab = "Share of contracts redacted")
}

#' Appendix F1 in numbers: pre- and post-FAST share by type, per sample
#' @param .tab_data Every sample from `des_data_ff1()`.
#' @return Invisibly, the table.
des_report_ff1 <- function(.tab_data) {
  if (FALSE) .tab_data <- tab_ff1
  wide_ <- .tab_data |>
    dplyr::mutate(Period = dplyr::if_else(.data$Year >= 2019L, "Post", "Pre")) |>
    dplyr::summarise(Share = sum(.data$Share * .data$N) / sum(.data$N), .by = c("Sample", "Class", "Period")) |>
    dplyr::mutate(Share = round(100 * .data$Share, 1), Col = paste0(.data$Sample, "_", .data$Period)) |>
    dplyr::select("Class", "Col", "Share") |>
    tidyr::pivot_wider(names_from = "Col", values_from = "Share") |>
    dplyr::mutate(Class = .des_class_short[match(.data$Class, .des_class_levels)])
  tbl_out(.tab = des_fmt(wide_, 1L),
          .title = "Appendix F1: share redacted by type, pre (2008-18) and post (2019-) FAST, percent",
          .notes = c(Class = "Contract-weighted over years. R&D and Licenses are the classes the text should name."))
  invisible(wide_)
}


# 16. T07: Item 1.01 summaries, delayed against attached --------------------------------------------------------------------
# The paper's Table 7 on the release's own summaries, with the two measures the export added --
# boilerplate share and announcement lag -- and the four-business-day compliance share the planned
# Table 5 needs. Welch t on the difference, delayed less attached, as the paper signs it.

.des_reference_t07 <- tibble::tribble(
  ~Measure,        ~Group,     ~Revision,
  "Words",         "Delayed",  537.281,
  "Words",         "Attached", 461.006,
  "Fog",           "Delayed",  24.034,
  "Fog",           "Attached", 24.591,
  "Uncertainty",   "Delayed",  6.370,
  "Uncertainty",   "Attached", 5.121,
  "Numbers",       "Delayed",  38.460,
  "Numbers",       "Attached", 40.053,
  "Dollars",       "Delayed",  8.061,
  "Dollars",       "Attached", 8.217,
  "Percents",      "Delayed",  3.858,
  "Percents",      "Attached", 3.994
)

.des_t07_measures <- c(
  Words = "Words", Fog = "Gunning Fog", Uncertainty = "Uncertainty (per 1,000 words)",
  Numbers = "Numbers (per 1,000 words)", Dollars = "Dollar figures (per 1,000 words)",
  Percents = "Percentages (per 1,000 words)", Boilerplate = "Boilerplate share",
  LagDays = "Announcement lag (calendar days)", OnTime = "Within four business days"
)

#' The nine summary measures by attachment status, with the difference and its t
#'
#' @param .tab One sample of the announcement table, from `des_read_summaries()`.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Measure, then for Delayed and Attached N and Mean, Diff, TStat, PValue.
des_data_t07 <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- lst_other$S7_Summaries
    .sample <- "S7_Summaries"
  }
  long_ <- .tab |>
    dplyr::transmute(
      Group       = dplyr::if_else(.data$SumAttached == 1L, "Attached", "Delayed"),
      Words       = as.numeric(.data$SumWords),
      Fog         = .data$SumFog,
      Uncertainty = 1000 * .data$SumUncertain / .data$SumWords,
      Numbers     = 1000 * .data$SumNumbers / .data$SumWords,
      Dollars     = 1000 * .data$SumDollars / .data$SumWords,
      Percents    = 1000 * .data$SumPercents / .data$SumWords,
      Boilerplate = .data$SumBoiler,
      LagDays     = as.numeric(.data$SumLagDays),
      OnTime      = as.numeric(.data$OnTime)
    ) |>
    tidyr::pivot_longer(-"Group", names_to = "Measure", values_to = "Value") |>
    dplyr::filter(!is.na(.data$Value), is.finite(.data$Value))

  one_ <- function(.d) {
    d_ <- .d$Value[.d$Group == "Delayed"]
    a_ <- .d$Value[.d$Group == "Attached"]
    t_ <- if (length(d_) > 1L && length(a_) > 1L) stats::t.test(d_, a_) else NULL
    tibble::tibble(
      NDelayed     = length(d_),
      NAttached    = length(a_),
      MeanDelayed  = mean(d_),
      MeanAttached = mean(a_),
      Diff         = mean(d_) - mean(a_),
      TStat        = if (is.null(t_)) NA_real_ else unname(t_$statistic),
      PValue       = if (is.null(t_)) NA_real_ else t_$p.value
    )
  }

  long_ |>
    dplyr::group_split(.data$Measure) |>
    purrr::map(\(.d) dplyr::mutate(one_(.d), Measure = .d$Measure[1L])) |>
    purrr::list_rbind() |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::arrange(match(.data$Measure, names(.des_t07_measures))) |>
    dplyr::relocate("Sample", "Measure")
}

#' Table 7 printed for one sample
#' @param .tab_data One sample's rows from `des_data_t07()`.
#' @return Invisibly, NULL.
des_print_t07 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_t07, "S7_Summaries")
  out_ <- .tab_data |>
    dplyr::transmute(
      Measure = unname(.des_t07_measures[.data$Measure]),
      .data$NDelayed, .data$NAttached, .data$MeanDelayed, .data$MeanAttached, .data$Diff, .data$TStat,
      Stars = dplyr::case_when(
        .data$PValue < 0.01 ~ "***", .data$PValue < 0.05 ~ "**", .data$PValue < 0.10 ~ "*", .default = ""
      )
    )
  tbl_out(.tab = des_fmt(out_, 3L, .counts = c("NDelayed", "NAttached")),
          .title = "Table 7: Item 1.01 summaries, delayed against attached",
          .notes = c(Diff = "Delayed less attached, as the paper signs it. Welch t.",
                     Measure = "The lag is signed and heavy-tailed: a filing dated before its agreement is negative."))
  invisible(NULL)
}

#' Table 7 to .tex
#' @param .tab_data One sample's rows.
#' @param .sample Character.
#' @param .dir Character. Output/Tables.
#' @return Invisibly, the path.
des_tex_t07 <- function(.tab_data, .sample, .dir) {
  if (FALSE) {
    .tab_data <- des_slice(tab_t07, "S7_Summaries")
    .sample   <- "S7_Summaries"
    .dir      <- .lP$Output$DirTables
  }
  out_ <- .tab_data |>
    dplyr::transmute(
      Measure = unname(.des_t07_measures[.data$Measure]),
      `N delayed` = .data$NDelayed, `N attached` = .data$NAttached,
      `Mean delayed` = .data$MeanDelayed, `Mean attached` = .data$MeanAttached,
      Difference = .data$Diff, `t` = .data$TStat
    )
  des_tex_table(out_, fs::path(.dir, paste0("T07_", .sample, ".tex")),
                .digits = c(`N delayed` = 0L, `N attached` = 0L, `Mean delayed` = 3L, `Mean attached` = 3L,
                            Difference = 3L, t = 2L))
}

#' Table 7 in numbers: the means per sample beside the revision
#' @param .tab_data Every sample from `des_data_t07()`.
#' @return Invisibly, the table.
des_report_t07 <- function(.tab_data, .ref = .des_reference_t07) {
  if (FALSE) {
    .tab_data <- tab_t07
    .ref      <- .des_reference_t07
  }
  long_ <- .tab_data |>
    dplyr::select("Sample", "Measure", Delayed = "MeanDelayed", Attached = "MeanAttached") |>
    tidyr::pivot_longer(c("Delayed", "Attached"), names_to = "Group", values_to = "Mean") |>
    dplyr::mutate(Col = paste0(.data$Sample, "_", .data$Group)) |>
    dplyr::select("Measure", "Group", "Col", "Mean") |>
    tidyr::pivot_wider(names_from = "Col", values_from = "Mean") |>
    dplyr::left_join(.ref, by = dplyr::join_by(Measure, Group)) |>
    dplyr::arrange(match(.data$Measure, names(.des_t07_measures)), .data$Group)
  tbl_out(.tab = des_fmt(long_, 3L), .title = "Table 7: means per sample and group, beside revision 1",
          .notes = c(Revision = "The revision's Table 7 is the old pipeline's; N and Fog differ by construction."))
  invisible(long_)
}

#' Compliance by year: the share of announcements within four business days, by attachment
#' @param .tab One sample of the announcement table.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Year, Group, N, Share, MedianLag.
des_data_t07b <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- lst_other$S7_Summaries
    .sample <- "S7_Summaries"
  }
  .tab |>
    dplyr::filter(!is.na(.data$SumLagDays)) |>
    dplyr::mutate(Group = dplyr::if_else(.data$SumAttached == 1L, "Attached", "Delayed")) |>
    dplyr::summarise(N = dplyr::n(), Share = mean(.data$OnTime), MedianLag = stats::median(.data$SumLagDays),
                     .by = c("Year", "Group")) |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::arrange(.data$Group, .data$Year)
}

#' Compliance figure: share on time per year, attached and delayed
#' @param .tab_data One sample's rows from `des_data_t07b()`.
#' @return A ggplot.
des_plot_t07b <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_t07b, "S7_Summaries")
  des_shape_lines(.tab = .tab_data, .x = "Year", .y = "Share", .group = "Group", .key = NULL, .pct = TRUE,
                  .regimes = TRUE, .ylab = "Announced within four business days")
}


# 17. OA-A1: filings within the calendar year -------------------------------------------------------------------------------
# The Online Appendix figure: each day of the year's share of the group's contracts, averaged over
# years, with the reporting seasons shaded. Day of year is one-based; 29 February pools with 28.

.des_seasons <- tibble::tribble(
  ~Season,      ~Start, ~Stop,
  "Annual",     46L,    91L,
  "Quarterly",  106L,   136L,
  "Quarterly",  197L,   227L,
  "Quarterly",  289L,   319L
)

#' Share of each group's yearly contracts filed on each day of the year, averaged over years
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, FilingGroup, Day, Share (percent of the group's year, mean over years).
des_data_fa1 <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample <- "S2_Descriptive"
  }
  .tab |>
    dplyr::filter(!is.na(.data$FilingGroup)) |>
    dplyr::mutate(
      Day = as.integer(lubridate::yday(.data$DateFiled)),
      Day = dplyr::if_else(lubridate::leap_year(.data$DateFiled) & .data$Day >= 60L, .data$Day - 1L, .data$Day)
    ) |>
    dplyr::count(.data$Year, .data$FilingGroup, .data$Day, name = "N") |>
    tidyr::complete(.data$Year, .data$FilingGroup, Day = 1:365, fill = list(N = 0L)) |>
    dplyr::mutate(ShareYear = .data$N / sum(.data$N), .by = c("Year", "FilingGroup")) |>
    dplyr::summarise(Share = mean(.data$ShareYear), .by = c("FilingGroup", "Day")) |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::arrange(.data$FilingGroup, .data$Day)
}

#' The within-year figure: three lines over the day of year with the seasons shaded
#' @param .tab_data One sample's rows from `des_data_fa1()`.
#' @return A ggplot.
des_plot_fa1 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_fa1, "S2_Descriptive")
  dat_ <- .tab_data |> dplyr::mutate(PlotG = plot_factor(.data$FilingGroup, .key = "FilingGroup"))
  months_ <- c(32, 91, 152, 213, 274, 335)
  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = .des_seasons,
      mapping = ggplot2::aes(xmin = .data$Start, xmax = .data$Stop, ymin = -Inf, ymax = Inf, alpha = .data$Season),
      fill = .plot_ref
    ) +
    ggplot2::scale_alpha_manual(
      values = c(Annual = 0.35, Quarterly = 0.18), name = NULL,
      labels = c(Annual = "Annual reporting season", Quarterly = "Quarterly reporting season")
    ) +
    ggplot2::geom_line(data = dat_, mapping = ggplot2::aes(x = .data$Day, y = .data$Share, colour = .data$PlotG),
                       linewidth = 0.5) +
    plot_scale_colour_key(.key = "FilingGroup", name = NULL) +
    ggplot2::scale_x_continuous(breaks = months_, labels = c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec"),
                                expand = ggplot2::expansion(mult = 0.01)) +
    plot_scale_y_pct(.accuracy = 1, .expand = c(0, 0.05)) +
    ggplot2::labs(x = NULL, y = "Share of the group's yearly contracts") +
    plot_theme(.grid = "y", .legend = "bottom")
}

#' The within-year figure in numbers: the share filed inside each season, per group and sample
#' @param .tab_data Every sample from `des_data_fa1()`.
#' @return Invisibly, the table.
des_report_fa1 <- function(.tab_data) {
  if (FALSE) .tab_data <- tab_fa1
  in_ <- function(.d) purrr::map_lgl(.d, \(.x) any(.x >= .des_seasons$Start & .x <= .des_seasons$Stop))
  out_ <- .tab_data |>
    dplyr::mutate(InSeason = in_(.data$Day)) |>
    dplyr::summarise(ShareInSeason = 100 * sum(.data$Share[.data$InSeason]), .by = c("Sample", "FilingGroup")) |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "ShareInSeason")
  tbl_out(.tab = des_fmt(out_, 1L),
          .title = "Within the year: percent of each group's contracts filed in a reporting season",
          .notes = c(FilingGroup = "The seasons cover 128 of 365 days; a group filing uniformly would show 35."))
  invisible(out_)
}


# 18. T01: agreement titles by type -----------------------------------------------------------------------------------------
# Referee 2 asked for representative agreement titles per category. The filer's own description of
# the exhibit is the one human-written label a contract carries; normalised, the most frequent ones
# per class are that table, and the classifier's agreement with the keyword lexicon is beside it.

#' Normalise a filer's exhibit description to a title
#'
#' Upper case, the exhibit designation and any date or number removed, amendment prefixes collapsed to
#' one token, punctuation and runs of space squeezed. Aggressive by design: the point is to count
#' titles, and "Amendment No. 3 to the Credit Agreement dated..." is a credit agreement amendment.
#'
#' @param .x Character vector.
#' @return Character vector.
des_title_norm <- function(.x) {
  if (FALSE) .x <- c("EX-10.1 Amendment No. 3 to Credit Agreement, dated as of March 1, 2019", NA)
  y_ <- stringi::stri_trans_toupper(.x)
  y_ <- stringi::stri_replace_all_regex(y_, "\\bEX(HIBIT)?[ -]?10[.\\-]?[0-9A-Z()]*\\b", " ")
  y_ <- stringi::stri_replace_all_regex(
    y_, "\\b(FIRST|SECOND|THIRD|FOURTH|FIFTH|SIXTH|SEVENTH|EIGHTH|NINTH|TENTH)\\b", " "
  )
  y_ <- stringi::stri_replace_all_regex(y_, "\\bAMENDMENT\\s*(NO\\.?|NUMBER)?\\s*[0-9]*\\s*(TO|OF)\\b", "AMENDMENT TO")
  y_ <- stringi::stri_replace_all_regex(y_, "\\bDATED\\b.*$", " ")
  y_ <- stringi::stri_replace_all_regex(y_, "\\b(AS OF|EFFECTIVE)\\b.*$", " ")
  y_ <- stringi::stri_replace_all_regex(
    y_, "\\b(JANUARY|FEBRUARY|MARCH|APRIL|MAY|JUNE|JULY|AUGUST|SEPTEMBER|OCTOBER|NOVEMBER|DECEMBER)\\b.*$", " "
  )
  y_ <- stringi::stri_replace_all_regex(y_, "[0-9]+", " ")
  y_ <- stringi::stri_replace_all_regex(y_, "[^A-Z&/ ]+", " ")
  y_ <- stringi::stri_replace_all_regex(y_, "\\b(THE|A|AN|OF|AND|BY|BETWEEN|AMONG|WITH)\\b", " ")
  y_ <- stringi::stri_replace_all_regex(y_, "\\s+", " ")
  y_ <- stringi::stri_trim_both(y_)
  dplyr::na_if(y_, "")
}

#' The most frequent titles per class, and the lexicon's agreement with the classifier
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @param .top Integer. Titles kept per class.
#' @return Tibble: Sample, Class, Rank, Title, N, ShareOfClass, nClass, ShareConfirmed.
des_data_t01 <- function(.tab, .sample, .top = 8L) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample <- "S2_Descriptive"
    .top    <- 8L
  }
  base_ <- .tab |>
    dplyr::filter(!is.na(.data$Class)) |>
    dplyr::mutate(Title = des_title_norm(.data$DocDesc))

  agree_ <- base_ |>
    dplyr::summarise(
      nClass         = dplyr::n(),
      ShareConfirmed = mean(.data$ClassDetailedFlag == "confirmed", na.rm = TRUE),
      ShareChecked   = mean(.data$ClassDetailedFlag != "unchecked", na.rm = TRUE),
      .by = "Class"
    )

  # Descriptions that name the slot rather than the agreement -- "Exhibit", "Material Contracts",
  # "Agreement" -- are titles of nothing and are left out of the ranking, not of the denominator.
  empty_ <- c("EXHIBIT", "EXHIBITS", "MATERIAL CONTRACT", "MATERIAL CONTRACTS", "AGREEMENT", "AMENDMENT",
              "AMENDMENT TO", "DOCUMENT", "CONTRACT", "AGREEMENTS", "FORM", "EX")

  base_ |>
    dplyr::filter(!is.na(.data$Title), !.data$Title %in% empty_) |>
    dplyr::count(.data$Class, .data$Title, name = "N") |>
    dplyr::mutate(ShareOfClass = .data$N / sum(.data$N), .by = "Class") |>
    dplyr::arrange(.data$Class, dplyr::desc(.data$N)) |>
    dplyr::mutate(Rank = dplyr::row_number(), .by = "Class") |>
    dplyr::filter(.data$Rank <= .top) |>
    dplyr::left_join(agree_, by = dplyr::join_by(Class)) |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::arrange(match(.data$Class, .des_class_levels), .data$Rank) |>
    dplyr::relocate("Sample", "Class", "Rank", "Title")
}

#' The titles table printed for one sample
#' @param .tab_data One sample's rows from `des_data_t01()`.
#' @return Invisibly, NULL.
des_print_t01 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_t01, "S2_Descriptive")
  out_ <- .tab_data |>
    dplyr::transmute(
      Type  = dplyr::if_else(.data$Rank == 1L, .des_class_short[match(.data$Class, .des_class_levels)], ""),
      .data$Rank,
      Title = stringi::stri_sub(stringi::stri_trans_totitle(.data$Title), 1L, 60L),
      .data$N,
      Share = 100 * .data$ShareOfClass
    )
  tbl_out(.tab = des_fmt(out_, 1L, .counts = c("N", "Rank")),
          .title = "Table 1: the most frequent exhibit titles per type, share of the type's contracts")
  agree_ <- .tab_data |>
    dplyr::distinct(.data$Class, .data$nClass, .data$ShareConfirmed, .data$ShareChecked) |>
    dplyr::transmute(Type = .des_class_short[match(.data$Class, .des_class_levels)], N = .data$nClass,
                     Confirmed = 100 * .data$ShareConfirmed, Checked = 100 * .data$ShareChecked)
  tbl_out(.tab = des_fmt(agree_, 1L),
          .title = "Table 1: the keyword lexicon against the classifier, percent of the type",
          .notes = c(Confirmed = "Contracts where the lexicon fired and agreed; Checked is where it fired at all."))
  invisible(NULL)
}

#' The titles table to .tex
#' @param .tab_data One sample's rows.
#' @param .sample Character.
#' @param .dir Character.
#' @return Invisibly, the path.
des_tex_t01 <- function(.tab_data, .sample, .dir) {
  if (FALSE) {
    .tab_data <- des_slice(tab_t01, "S2_Descriptive")
    .sample   <- "S2_Descriptive"
    .dir      <- .lP$Output$DirTables
  }
  out_ <- .tab_data |>
    dplyr::transmute(
      Type  = dplyr::if_else(.data$Rank == 1L, .des_class_short[match(.data$Class, .des_class_levels)], ""),
      Title = stringi::stri_trans_totitle(.data$Title),
      N     = .data$N,
      Share = 100 * .data$ShareOfClass
    )
  des_tex_table(out_, fs::path(.dir, paste0("T01_", .sample, ".tex")), .digits = c(N = 0L, Share = 1L))
}


# 19. CTO: the confidential-treatment counts, contract side -----------------------------------------------------------------
# What the release can say about the orders: how many contracts carry one, how many orders each, how
# many are extensions, how many are matched to Compustat, and how the orders sit against the markers.
# The order-side counts -- orders issued, exhibits named, exhibits unlinked -- need 01E's own table and
# wait for the export to carry it.

#' Order coverage by year and in total
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Year (with "Total"), N, nCto, ShareCto, nExtension, nMultiOrder, nMatched,
#'   nCtoWithMarkers, nCtoNoMarkers, nMarkersNoCto.
des_data_cto <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S6_Redaction, , drop = FALSE]
    .sample <- "S6_Redaction"
  }
  one_ <- function(.d, .y) {
    tibble::tibble(
      Year            = .y,
      N               = nrow(.d),
      nCto            = sum(.d$HasCto == 1L),
      ShareCto        = mean(.d$HasCto == 1L),
      nExtension      = sum(.d$CtoIsExtension == 1L),
      nMultiOrder     = sum(.d$nCtoOrders > 1L),
      nMatched        = sum(.d$HasCto == 1L & .d$Matched),
      nCtoWithMarkers = sum(.d$HasCto == 1L & .d$HasMarkers == 1L),
      nCtoNoMarkers   = sum(.d$HasCto == 1L & .d$HasMarkers == 0L),
      nMarkersNoCto   = sum(.d$HasCto == 0L & .d$HasMarkers == 1L)
    )
  }
  by_ <- .tab |>
    dplyr::filter(.data$Year >= 2008L) |>
    dplyr::group_split(.data$Year) |>
    purrr::map(\(.d) one_(.d, as.character(.d$Year[1L]))) |>
    purrr::list_rbind()
  dplyr::bind_rows(by_, one_(dplyr::filter(.tab, .data$Year >= 2008L), "Total")) |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::relocate("Sample")
}

#' The order counts printed, per sample, Total row and the 2008-2018 window
#' @param .tab_data Every sample from `des_data_cto()`.
#' @return Invisibly, the table.
des_report_cto <- function(.tab_data) {
  if (FALSE) .tab_data <- tab_cto
  tot_ <- .tab_data |>
    dplyr::filter(.data$Year == "Total") |>
    dplyr::select(-"Year")
  tbl_out(.tab = des_fmt(tot_, 3L, .counts = c("N", "nCto", "nExtension", "nMultiOrder", "nMatched", "nCtoWithMarkers",
                                                "nCtoNoMarkers", "nMarkersNoCto")),
          .title = "Confidential treatment from 2008: contracts covered, per sample",
          .notes = c(
            nCtoNoMarkers = "Covered by an order yet carrying no marker in the text: the linkage-versus-text gap.",
            nMarkersNoCto = "Markers without an order: the textual identification the old paper footnoted."
          ))
  yr_ <- .tab_data |>
    dplyr::filter(.data$Sample == "S6_Redaction", .data$Year != "Total") |>
    dplyr::select("Year", "N", "nCto", "ShareCto", "nExtension", "nCtoWithMarkers", "nCtoNoMarkers", "nMarkersNoCto")
  tbl_out(.tab = des_fmt(yr_, 3L, .counts = c("N", "nCto", "nExtension", "nCtoWithMarkers", "nCtoNoMarkers",
                                               "nMarkersNoCto")),
          .title = "Confidential treatment by year on S6")
  invisible(tot_)
}


# 20. LOSS: where the ladder loses contracts --------------------------------------------------------------------------------
# Two figures from her 104 that neither paper version shows: the share of each year's contracts lost
# to format and to Compustat coverage, and the Compustat loss by form. Ladder figures, so drawn once
# on the universe rather than per sample.

#' Sample losses by year and by form
#' @param .tab The contract table; the function takes steps 2-6 itself.
#' @param .sample Character. Its name; "S0_Universe" by construction.
#' @return Tibble, long: Sample, Panel ("Year" or "Form"), Key, N, nMalformed, nUnmatched, ShareMalformed,
#'   ShareUnmatched.
des_data_loss <- function(.tab, .sample = "S0_Universe") {
  if (FALSE) {
    .tab    <- tab_contracts
    .sample <- "S0_Universe"
  }
  base_ <- .tab |>
    dplyr::filter(.data$SampleStepCode >= 2L, .data$PrimaryFiler == 1L) |>
    dplyr::mutate(Malformed = .data$SampleStepCode == 2L, Unmatched = .data$SampleStepCode %in% 3:5)
  one_ <- function(.d, .panel, .key) {
    .d |>
      dplyr::summarise(
        N              = dplyr::n(),
        nMalformed     = sum(.data$Malformed),
        nUnmatched     = sum(.data$Unmatched),
        ShareMalformed = mean(.data$Malformed),
        ShareUnmatched = mean(.data$Unmatched),
        .by = dplyr::all_of(.key)
      ) |>
      dplyr::rename(Key = dplyr::all_of(.key)) |>
      dplyr::mutate(Panel = .panel, Key = as.character(.data$Key))
  }
  dplyr::bind_rows(one_(base_, "Year", "Year"), one_(base_, "Form", "FormFamily")) |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::relocate("Sample", "Panel", "Key")
}

#' The loss figure: two panels, by year and by form
#' @param .tab_data The tibble from `des_data_loss()`.
#' @return A patchwork.
des_plot_loss <- function(.tab_data) {
  if (FALSE) .tab_data <- tab_loss
  yr_ <- .tab_data |>
    dplyr::filter(.data$Panel == "Year") |>
    dplyr::mutate(Year = as.integer(.data$Key)) |>
    dplyr::select("Year", Malformed = "ShareMalformed", `No Compustat quarter` = "ShareUnmatched") |>
    tidyr::pivot_longer(-"Year", names_to = "Loss", values_to = "Share")
  a_ <- des_shape_lines(.tab = yr_, .x = "Year", .y = "Share", .group = "Loss", .pct = TRUE, .regimes = FALSE,
                        .ylab = "Share of the year's attachments")
  fm_ <- .tab_data |>
    dplyr::filter(.data$Panel == "Form") |>
    dplyr::mutate(PlotForm = factor(.data$Key, levels = rev(.des_form_levels)))
  b_ <- ggplot2::ggplot(fm_, ggplot2::aes(y = .data$PlotForm, x = .data$nUnmatched / 1000)) +
    ggplot2::geom_col(fill = .plot_ink, width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = scales::label_percent(accuracy = 0.1)(.data$ShareUnmatched)),
                       hjust = -0.15, size = .plot_base / ggplot2::.pt * 0.8, family = .plot_font) +
    plot_scale_x_count(.expand = c(0, 0.3)) +
    ggplot2::labs(x = "Attachments without a Compustat quarter (000)", y = NULL) +
    plot_theme(.grid = "x", .legend = "none")
  patchwork::wrap_plots(a_, b_, ncol = 2L, widths = c(1.4, 1))
}

#' The loss figure in numbers
#' @param .tab_data The tibble from `des_data_loss()`.
#' @return Invisibly, the form panel.
des_report_loss <- function(.tab_data) {
  if (FALSE) .tab_data <- tab_loss
  fm_ <- .tab_data |>
    dplyr::filter(.data$Panel == "Form") |>
    dplyr::arrange(match(.data$Key, .des_form_levels)) |>
    dplyr::transmute(Form = .data$Key, .data$N, .data$nMalformed, .data$nUnmatched,
                     PctMalformed = 100 * .data$ShareMalformed, PctUnmatched = 100 * .data$ShareUnmatched)
  tbl_out(.tab = des_fmt(fm_, 1L, .counts = c("N", "nMalformed", "nUnmatched")),
          .title = "Where the ladder loses attachments, by form",
          .notes = c(
            PctUnmatched = "S-4 and F-4 lose most: merger registrants are often not yet, or no longer, Compustat firms."
          ))
  invisible(fm_)
}


# 21. The firm-quarter side: control sets and the join ----------------------------------------------------------------------
# The variables Table 4 shows and the estimation ladder counts. The names are this document's, mapped
# from 103's in .des_quarter_cols; the sets are the paper's, as 20-RunRegression states them.

.des_controls <- list(
  Quarter = c("LogMve", "OcfWin", "AcqNeg", "AcqPos", "LeverageWin", "GoodwillImp", "Loss", "RoaWin"),
  # Her 105's $var_set for Table 4: the filing-decision controls plus the Herfindahl and the one-quarter
  # sales change. The sales lead is what costs the quarters, and the paper's 462,572 needs it.
  Filing  = c("LogMve", "OcfWin", "AcqNeg", "AcqPos", "LeverageWin", "HhiSic3Win", "ChgSalesWin", "GoodwillImp",
              "Loss", "RoaWin"),
  Full    = c("LogMve", "OcfWin", "AcqNeg", "AcqPos", "LeverageWin", "GoodwillImp", "Loss", "EqIssue", "DebtIssue",
              "RoaWin", "ChgSalesQ4Win", "MbWin")
)

.des_var_labels <- c(
  nContractsQ   = "Contracts per quarter",
  LogMve        = "Logarithmic market value",
  OcfWin        = "Operating cash flow",
  AcqNeg        = "Divestment dummy",
  AcqPos        = "Investment dummy",
  LeverageWin   = "Leverage",
  HhiSic3Win    = "Herfindahl-Hirschman index",
  ChgSalesWin   = "Future economic performance",
  ChgSalesQ4Win = "Future economic performance (4q)",
  GoodwillImp   = "Goodwill impairment dummy",
  Loss          = "Loss dummy",
  EqIssue       = "Equity issue",
  DebtIssue     = "Debt issue",
  RoaWin        = "Return on assets",
  MbWin         = "Market-to-book",
  Fluidity      = "Product market fluidity"
)

#' Contract rows joined to their firm-quarter's variables
#'
#' A many-to-one join on gvkey, fiscal year and quarter, which is how 103 carries the Compustat block
#' onto contracts. Only the columns an exhibit needs travel; the contract table already holds the
#' keys.
#'
#' @param .tab A contract table, or one sample of it.
#' @param .quarter The quarter panel from `des_read_quarter()`.
#' @param .cols Character. Quarter-panel columns to attach.
#' @return The contract rows with those columns, rows without a panel quarter kept with NA.
des_join_quarter <- function(.tab, .quarter, .cols) {
  if (FALSE) {
    .tab     <- tab_contracts[tab_contracts$S3_Matched, , drop = FALSE]
    .quarter <- tab_quarter
    .cols    <- c("FfInd", .des_controls$Full)
  }
  miss_ <- setdiff(.cols, names(.quarter))
  if (length(miss_) > 0L) cli::cli_abort("The quarter panel lacks {miss_}.")
  q_ <- .quarter |>
    dplyr::select("gvkey", "fyear", "fqtr", dplyr::all_of(setdiff(.cols, c("gvkey", "fyear", "fqtr")))) |>
    dplyr::distinct(.data$gvkey, .data$fyear, .data$fqtr, .keep_all = TRUE)
  dplyr::left_join(.tab, q_, by = dplyr::join_by(gvkey, fyear, fqtr), relationship = "many-to-one")
}


# 22. T02B: the estimation ladder -------------------------------------------------------------------------------------------
# Table 2 Panel B: every copy at steps 2-6, less the malformed, less the CIKs Compustat does not
# cover, less the copies with no fiscal quarter, less the rows missing a control -- with the two
# start years and the firm-level row. Copies rather than attachments, because that is how the panel
# was printed; the primary-copy count sits beside it so both readings are on the page.

#' The estimation ladder, contracts, quarters and firms at each rung
#' @param .tab The contract table.
#' @param .quarter The quarter panel.
#' @param .controls Character. The control set that defines "missing data".
#' @return Tibble: Rung, Contracts, Primary, Quarters, Firms.
des_table_ladder_b <- function(.tab, .quarter, .controls = .des_controls$Full) {
  if (FALSE) {
    .tab      <- tab_contracts
    .quarter  <- tab_quarter
    .controls <- .des_controls$Full
  }
  ok_ <- .quarter |>
    dplyr::transmute(.data$gvkey, .data$fyear, .data$fqtr,
                     Complete = rowSums(is.na(dplyr::pick(dplyr::all_of(.controls)))) == 0L)

  j_ <- .tab |>
    dplyr::filter(.data$SampleStepCode >= 2L) |>
    dplyr::select("SampleStepCode", "PrimaryFiler", "CIK", "gvkey", "fyear", "fqtr", "Year") |>
    dplyr::left_join(ok_, by = dplyr::join_by(gvkey, fyear, fqtr), relationship = "many-to-one") |>
    dplyr::mutate(Complete = dplyr::coalesce(.data$Complete, FALSE))

  rung_ <- function(.d, .name) {
    tibble::tibble(
      Rung      = .name,
      Contracts = nrow(.d),
      Primary   = sum(.d$PrimaryFiler == 1L),
      Quarters  = if (all(is.na(.d$gvkey))) NA_integer_ else
        nrow(dplyr::distinct(dplyr::filter(.d, !is.na(.data$gvkey)), .data$gvkey, .data$fyear, .data$fqtr)),
      Firms     = if (all(is.na(.d$gvkey))) dplyr::n_distinct(.d$CIK) else dplyr::n_distinct(.d$gvkey)
    )
  }

  s1_ <- j_
  s2_ <- dplyr::filter(s1_, .data$SampleStepCode >= 3L)
  s3_ <- dplyr::filter(s2_, .data$SampleStepCode >= 4L)
  s4_ <- dplyr::filter(s3_, .data$SampleStepCode == 6L)
  s5_ <- dplyr::filter(s4_, .data$Complete)

  # The firm-level rows are Table 4's samples, built the way 105 builds them, on the control set this
  # ladder is defined on -- so the two printed ladders bracket the paper's count from both sides.
  firm_ <- .quarter |>
    dplyr::filter(dplyr::between(.data$cyear, 2001L, 2024L)) |>
    des_quarter_samples(.controls = .controls)
  firm_row_ <- function(.flag, .name) {
    d_ <- firm_[firm_[[.flag]], , drop = FALSE]
    tibble::tibble(Rung = .name, Contracts = NA_integer_, Primary = NA_integer_, Quarters = nrow(d_),
                   Firms = dplyr::n_distinct(d_$cik))
  }

  dplyr::bind_rows(
    rung_(s1_, "Full sample (steps 2-6, every copy)"),
    rung_(s2_, "Less malformatted"),
    rung_(s3_, "Less CIK not in Compustat"),
    rung_(s4_, "Less no fiscal quarter matched"),
    rung_(s5_, "Less missing controls: contract-level sample"),
    rung_(dplyr::filter(s5_, .data$Year >= 2005L), "  from 2005"),
    rung_(dplyr::filter(s5_, .data$Year >= 2008L), "  from 2008"),
    firm_row_("T4_Regression", "Firm-level sample (these controls, state, ever-filers; CIK firms)"),
    firm_row_("T4_TrimmedAll", "  less firms whose filing never varies overall"),
    firm_row_("T4_Trimmed", "  less firms whose filing never varies within a FAST period")
  )
}

#' Table 2 Panel B printed for both control sets
#' @param .tab The contract table.
#' @param .quarter The quarter panel.
#' @return Invisibly, a named list of the two ladders.
des_report_ladder_b <- function(.tab, .quarter) {
  if (FALSE) {
    .tab     <- tab_contracts
    .quarter <- tab_quarter
  }
  full_ <- des_table_ladder_b(.tab = .tab, .quarter = .quarter, .controls = .des_controls$Full)
  fil_  <- des_table_ladder_b(.tab = .tab, .quarter = .quarter, .controls = .des_controls$Filing)
  quar_ <- des_table_ladder_b(.tab = .tab, .quarter = .quarter, .controls = .des_controls$Quarter)
  tbl_out(.tab = des_fmt(full_, 0L, .counts = c("Contracts", "Primary", "Quarters", "Firms")),
          .title = "Table 2 Panel B: the estimation ladder, missing data on the full control set",
          .notes = c(Primary = "One copy per attachment; Contracts counts every registrant copy, as the paper did."))
  tbl_out(.tab = des_fmt(fil_, 0L, .counts = c("Contracts", "Primary", "Quarters", "Firms")),
          .title = "Table 2 Panel B: the same ladder on her 105 control set (Table 4's)")
  tbl_out(.tab = des_fmt(quar_, 0L, .counts = c("Contracts", "Primary", "Quarters", "Firms")),
          .title = "Table 2 Panel B: the same ladder, missing data on the filing-decision control set")
  invisible(list(Full = full_, Filing = fil_, Quarter = quar_))
}


# 23. T04: filing against non-filing firm-quarters --------------------------------------------------------------------------
# The paper's Table 4 on her quarter panel: every variable by whether the quarter had a contract,
# with the Welch t on the difference. Two samples: the regression sample -- controls complete,
# ever-filers -- and that sample after her firm-effect trimming, which drops firms whose filing never
# varies, overall or within either FAST period.

.des_reference_t04 <- tibble::tribble(
  ~Variable,      ~Group,        ~Revision,
  "nContractsQ",  "Filing",      3.08,
  "LogMve",       "Non-filing",  5.32,
  "LogMve",       "Filing",      6.09,
  "Loss",         "Non-filing",  0.39,
  "Loss",         "Filing",      0.43,
  "LeverageWin",  "Non-filing",  0.39,
  "LeverageWin",  "Filing",      0.36
)

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
des_quarter_samples <- function(.quarter, .controls = .des_controls$Filing, .firm = c("cik", "gvkey"),
                                .drop_states = TRUE) {
  if (FALSE) {
    .quarter     <- lst_other$S5_Quarter
    .controls    <- .des_controls$Filing
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

#' Table 4: the variables by filing status, with the difference and its t
#' @param .tab One sample of the quarter panel.
#' @param .sample Character. Its name.
#' @param .vars Character. Variables, in table order.
#' @return Tibble: Sample, Variable, Group, N, Mean, SD, P25, P50, P75, plus Diff, TStat, PValue on the
#'   Filing rows.
des_data_t04 <- function(.tab, .sample, .vars = c("nContractsQ", .des_controls$Full, "HhiSic3Win", "ChgSalesWin",
                                                     "Fluidity")) {
  if (FALSE) {
    .tab    <- tab_q[tab_q$T4_Regression, , drop = FALSE]
    .sample <- "T4_Regression"
    .vars   <- c("nContractsQ", .des_controls$Full, "Fluidity")
  }
  vars_ <- intersect(.vars, names(.tab))
  one_ <- function(.v) {
    x_ <- .tab[[.v]]
    f_ <- .tab$Filed == 1L
    st_ <- function(.x) tibble::tibble(
      N = sum(!is.na(.x)), Mean = mean(.x, na.rm = TRUE), SD = stats::sd(.x, na.rm = TRUE),
      P25 = stats::quantile(.x, 0.25, na.rm = TRUE, names = FALSE), P50 = stats::median(.x, na.rm = TRUE),
      P75 = stats::quantile(.x, 0.75, na.rm = TRUE, names = FALSE)
    )
    tstat_ <- NA_real_
    pval_  <- NA_real_
    nf_ <- st_(x_[!f_]) |> dplyr::mutate(Group = "Non-filing")
    fi_ <- st_(x_[f_])  |> dplyr::mutate(Group = "Filing")
    t_  <- if (sum(!is.na(x_[f_])) > 1L && sum(!is.na(x_[!f_])) > 1L && stats::sd(x_, na.rm = TRUE) > 0) {
      stats::t.test(x_[f_], x_[!f_])
    } else {
      NULL
    }
    if (!is.null(t_)) {
      tstat_ <- unname(t_$statistic)
      pval_  <- t_$p.value
    }
    dplyr::bind_rows(nf_, fi_) |>
      dplyr::mutate(
        Variable = .v,
        Diff     = dplyr::if_else(.data$Group == "Filing", fi_$Mean - nf_$Mean, NA_real_),
        TStat    = dplyr::if_else(.data$Group == "Filing", tstat_, NA_real_),
        PValue   = dplyr::if_else(.data$Group == "Filing", pval_, NA_real_)
      )
  }
  purrr::map(vars_, one_) |>
    purrr::list_rbind() |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::relocate("Sample", "Variable", "Group")
}

#' Table 4 printed for one sample
#' @param .tab_data One sample's rows from `des_data_t04()`.
#' @return Invisibly, NULL.
des_print_t04 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_t04, "T4_Regression")
  out_ <- .tab_data |>
    dplyr::mutate(
      Variable = dplyr::coalesce(.des_var_labels[.data$Variable], .data$Variable),
      Stars    = dplyr::case_when(
        is.na(.data$PValue) ~ "", .data$PValue < 0.01 ~ "***", .data$PValue < 0.05 ~ "**", .data$PValue < 0.10 ~ "*",
        .default = ""
      )
    ) |>
    dplyr::select("Variable", "Group", "N", "Mean", "SD", "P25", "P50", "P75", "Diff", "TStat", "Stars")
  tbl_out(.tab = des_fmt(out_, 2L), .title = "Table 4: firm-quarters with and without a contract",
          .notes = c(Diff = "Filing less non-filing, Welch t. Variables as 103 winsorised them."))
  invisible(NULL)
}

#' Table 4 to .tex, the paper's layout: one row per variable, groups across
#' @param .tab_data One sample's rows.
#' @param .sample Character.
#' @param .dir Character.
#' @return Invisibly, the path.
des_tex_t04 <- function(.tab_data, .sample, .dir) {
  if (FALSE) {
    .tab_data <- des_slice(tab_t04, "T4_Regression")
    .sample   <- "T4_Regression"
    .dir      <- .lP$Output$DirTables
  }
  wide_ <- .tab_data |>
    dplyr::mutate(Variable = dplyr::coalesce(.des_var_labels[.data$Variable], .data$Variable),
                  G = dplyr::if_else(.data$Group == "Filing", "F", "NF")) |>
    dplyr::select("Variable", "G", "N", "Mean", "SD", "P50", "Diff", "TStat") |>
    tidyr::pivot_wider(names_from = "G", values_from = c("N", "Mean", "SD", "P50", "Diff", "TStat")) |>
    dplyr::transmute(.data$Variable, `N non-filing` = .data$N_NF, `N filing` = .data$N_F,
                     `Mean non-filing` = .data$Mean_NF, `Mean filing` = .data$Mean_F, Difference = .data$Diff_F,
                     t = .data$TStat_F, `SD non-filing` = .data$SD_NF, `SD filing` = .data$SD_F,
                     `Median non-filing` = .data$P50_NF, `Median filing` = .data$P50_F)
  des_tex_table(wide_, fs::path(.dir, paste0("T04_", .sample, ".tex")),
                .digits = c(`N non-filing` = 0L, `N filing` = 0L, `Mean non-filing` = 2L, `Mean filing` = 2L,
                            Difference = 2L, t = 2L, `SD non-filing` = 2L, `SD filing` = 2L,
                            `Median non-filing` = 2L, `Median filing` = 2L))
}

#' Table 4 in numbers: the means per sample and group beside the revision
#' @param .tab_data Every sample from `des_data_t04()`.
#' @return Invisibly, the table.
des_report_t04 <- function(.tab_data, .ref = .des_reference_t04) {
  if (FALSE) {
    .tab_data <- tab_t04
    .ref      <- .des_reference_t04
  }
  out_ <- .tab_data |>
    dplyr::mutate(Col = paste0(.data$Sample, "_", .data$Group)) |>
    dplyr::select("Variable", "Group", "Col", "Mean") |>
    tidyr::pivot_wider(names_from = "Col", values_from = "Mean") |>
    dplyr::left_join(.ref, by = dplyr::join_by(Variable, Group)) |>
    dplyr::mutate(Variable = dplyr::coalesce(.des_var_labels[.data$Variable], .data$Variable))
  tbl_out(.tab = des_fmt(out_, 2L), .title = "Table 4: means per sample and group, beside revision 1")
  n_ <- .tab_data |>
    dplyr::filter(.data$Variable == "nContractsQ") |>
    dplyr::select("Sample", "Group", "N") |>
    tidyr::pivot_wider(names_from = "Group", values_from = "N")
  tbl_out(.tab = des_fmt(n_, 0L, .counts = c("Filing", "Non-filing")),
          .title = "Table 4: firm-quarters per sample",
          .notes = c(Sample = "Revision 1 prints 251,699 / 210,873 in the table and 248,001 / 207,826 in its note."))
  invisible(out_)
}


# 24. F08: filing metrics by firm fundamentals ------------------------------------------------------------------------------
# Panel A: contracts per firm-quarter for the smallest and largest size quartile, by channel, over
# fiscal quarters. Panel B: contracts per firm-quarter by industry. The channel counts come from the
# contract table rather than 103's two, so that registration statements are a channel of their own.

.des_reference_f08 <- tibble::tribble(
  ~Industry,              ~Revision,
  "Consumer NonDurables", 1.30,
  "Utilities",            1.57,
  "Healthcare",           1.58,
  "Finance",              1.11
)

#' Contract counts per firm-quarter by channel, from the contract table
#' @param .tab The contract table.
#' @param .sample Character. The membership column whose rows are counted.
#' @return Tibble: gvkey, fyear, fqtr, n8K, nPeriodic, nRegistration, nAll.
des_channel_counts <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- tab_contracts
    .sample <- "S3_Matched"
  }
  .tab[.tab[[.sample]] & !is.na(.tab$gvkey), , drop = FALSE] |>
    dplyr::summarise(
      n8K           = sum(.data$Is8K),
      nPeriodic     = sum(.data$Delayed),
      nRegistration = sum(.data$IsRegStmt),
      nAll          = dplyr::n(),
      .by = c("gvkey", "fyear", "fqtr")
    )
}

#' Panel A: mean contracts per firm-quarter by size quartile, channel and fiscal quarter
#' @param .tab One sample of the quarter panel, inside the window.
#' @param .sample Character. Its name.
#' @param .counts Tibble from `des_channel_counts()`.
#' @return Tibble: Sample, YQ, fyear, fqtr, Quartile, Channel, nFirms, Mean.
des_data_f08a <- function(.tab, .sample, .counts) {
  if (FALSE) {
    .tab    <- lst_other$S5_Quarter
    .sample <- "S5_Quarter"
    .counts <- des_channel_counts(tab_contracts, "S3_Matched")
  }
  .tab |>
    dplyr::filter(!is.na(.data$AtqWin)) |>
    dplyr::mutate(Quartile = dplyr::ntile(.data$AtqWin, 4L), .by = "fyear") |>
    dplyr::left_join(.counts, by = dplyr::join_by(gvkey, fyear, fqtr)) |>
    dplyr::mutate(dplyr::across(c("n8K", "nPeriodic", "nRegistration", "nAll"), \(.x) dplyr::coalesce(.x, 0L))) |>
    dplyr::summarise(
      nFirms       = dplyr::n_distinct(.data$gvkey),
      `8-K`        = mean(.data$n8K),
      Periodic     = mean(.data$nPeriodic),
      Registration = mean(.data$nRegistration),
      All          = mean(.data$nAll),
      .by = c("YQ", "fyear", "fqtr", "Quartile")
    ) |>
    tidyr::pivot_longer(c("8-K", "Periodic", "Registration", "All"), names_to = "Channel", values_to = "Mean") |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::arrange(.data$Channel, .data$Quartile, .data$YQ) |>
    dplyr::relocate("Sample")
}

#' Panel A drawn: small and large firms, one panel per channel
#' @param .tab_data One sample's rows from `des_data_f08a()`.
#' @return A ggplot.
des_plot_f08a <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f08a, "S5_Quarter")
  dat_ <- .tab_data |>
    dplyr::filter(.data$Quartile %in% c(1L, 4L), .data$Channel != "All") |>
    dplyr::mutate(Size = factor(dplyr::if_else(.data$Quartile == 1L, "Small (Q1)", "Large (Q4)"),
                                levels = c("Small (Q1)", "Large (Q4)")),
                  Channel = factor(.data$Channel, levels = c("Periodic", "8-K", "Registration")))
  des_shape_lines(.tab = dat_, .x = "YQ", .y = "Mean", .group = "Size", .facet = "Channel", .pct = FALSE, .ncol = 3L,
                  .free_y = FALSE, .regimes = TRUE, .ylab = "Contracts per firm-quarter", .points = FALSE)
}

#' Panel B: mean contracts per firm-quarter by industry
#' @param .tab One sample of the quarter panel.
#' @param .sample Character. Its name.
#' @return Tibble: Sample, Industry, nQuarters, nFirms, Mean, MeanFiling.
des_data_f08b <- function(.tab, .sample) {
  if (FALSE) {
    .tab    <- lst_other$S5_Quarter
    .sample <- "S5_Quarter"
  }
  .tab |>
    dplyr::filter(!is.na(.data$FfIndLab)) |>
    dplyr::summarise(
      nQuarters  = dplyr::n(),
      nFirms     = dplyr::n_distinct(.data$gvkey),
      Mean       = mean(.data$nContractsQ),
      MeanFiling = mean(.data$nContractsQ[.data$Filed == 1L]),
      .by = "FfIndLab"
    ) |>
    dplyr::rename(Industry = "FfIndLab") |>
    dplyr::mutate(Industry = as.character(.data$Industry), Sample = .sample) |>
    dplyr::arrange(match(.data$Industry, .des_ff12_levels)) |>
    dplyr::relocate("Sample")
}

#' Panel B drawn
#' @param .tab_data One sample's rows from `des_data_f08b()`.
#' @return A ggplot.
des_plot_f08b <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f08b, "S5_Quarter")
  plot_bar_ranked(.tab = .tab_data, .cat = "Industry", .val = "Mean", .key = "FfInd", .label = TRUE, .accuracy = 0.01,
                  .pct = FALSE) +
    ggplot2::labs(x = "Contracts per firm-quarter")
}

#' Figure 8 in numbers
#' @param .tab_a Every sample from `des_data_f08a()`.
#' @param .tab_b Every sample from `des_data_f08b()`.
#' @return Invisibly, the industry table.
des_report_f08 <- function(.tab_a, .tab_b, .ref = .des_reference_f08) {
  if (FALSE) {
    .tab_a <- tab_f08a
    .tab_b <- tab_f08b
    .ref   <- .des_reference_f08
  }
  a_ <- .tab_a |>
    dplyr::filter(.data$Quartile %in% c(1L, 4L)) |>
    dplyr::mutate(Period = dplyr::case_when(
      .data$fyear >= 2019L ~ "2019-", .data$fyear >= 2005L ~ "2005-18", .default = "2001-04"
    )) |>
    dplyr::summarise(Mean = mean(.data$Mean), .by = c("Sample", "Channel", "Quartile", "Period")) |>
    dplyr::mutate(Col = paste0("Q", .data$Quartile, "_", .data$Period)) |>
    dplyr::select("Sample", "Channel", "Col", "Mean") |>
    tidyr::pivot_wider(names_from = "Col", values_from = "Mean")
  tbl_out(.tab = des_fmt(a_, 2L),
          .title = "Figure 8 Panel A: contracts per firm-quarter, smallest and largest quartile, by period",
          .notes = c(Channel = "Registration statements are the channel her figure folded into current reports."))
  b_ <- .tab_b |>
    dplyr::select("Sample", "Industry", "Mean") |>
    tidyr::pivot_wider(names_from = "Sample", values_from = "Mean") |>
    dplyr::left_join(.ref, by = dplyr::join_by(Industry))
  tbl_out(.tab = des_fmt(b_, 2L), .title = "Figure 8 Panel B: contracts per firm-quarter by industry, beside the revision")
  invisible(b_)
}


# 25. F09: redaction by industry and type -----------------------------------------------------------------------------------
# The share redacted within each industry x type cell, on the matched contracts from 2008, before
# and after the FAST Act. Her version averaged year x type shares with industry weights, so its rows
# barely differed; this is the mean over the cell's own contracts, with the cell's N kept so thin
# cells can be greyed rather than read.

#' Share redacted per industry x type x period
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @param .quarter The quarter panel, for FfInd.
#' @return Tibble: Sample, Period, Industry, Class, N, Share.
des_data_f09 <- function(.tab, .sample, .quarter) {
  if (FALSE) {
    .tab     <- tab_contracts[tab_contracts$S3b_Matched2008, , drop = FALSE]
    .sample  <- "S3b_Matched2008"
    .quarter <- tab_quarter
  }
  des_join_quarter(.tab = .tab, .quarter = .quarter, .cols = "FfIndLab") |>
    dplyr::filter(.data$Year >= 2008L, !is.na(.data$Redacted), !is.na(.data$Class), !is.na(.data$FfIndLab)) |>
    dplyr::mutate(Period = dplyr::if_else(.data$PostFast == 1L, "Post-FAST", "Pre-FAST")) |>
    dplyr::summarise(N = dplyr::n(), Share = mean(.data$Redacted), .by = c("Period", "FfIndLab", "Class")) |>
    dplyr::rename(Industry = "FfIndLab") |>
    dplyr::mutate(Industry = as.character(.data$Industry), Sample = .sample) |>
    dplyr::arrange(.data$Period, match(.data$Industry, .des_ff12_levels), match(.data$Class, .des_class_levels)) |>
    dplyr::relocate("Sample")
}

#' The heat map, one panel per period, cells under the minimum N left unlabelled
#' @param .tab_data One sample's rows from `des_data_f09()`.
#' @param .min_n Integer. Cells with fewer contracts are drawn but not labelled.
#' @return A patchwork.
des_plot_f09 <- function(.tab_data, .min_n = 30L) {
  if (FALSE) {
    .tab_data <- des_slice(tab_f09, "S3b_Matched2008")
    .min_n    <- 30L
  }
  one_ <- function(.p) {
    # The label is the share in percentage points, printed as a number: plot_heatmap() formats a
    # separate label column as a count, so the share is scaled before it goes in.
    d_ <- .tab_data |>
      dplyr::filter(.data$Period == .p) |>
      dplyr::mutate(Label = dplyr::if_else(.data$N >= .min_n, 100 * .data$Share, NA_real_))
    plot_heatmap(.tab = d_, .x = "Class", .y = "Industry", .fill = "Share", .key_x = "ClassBare", .key_y = "FfInd",
                 .short = TRUE, .label = TRUE, .pct = TRUE, .accuracy = 1, .angle = 40, .limits = c(0, 1),
                 .cell_label = "Label") +
      ggplot2::labs(title = .p) +
      ggplot2::theme(plot.title = ggplot2::element_text(family = .plot_font, face = "bold", hjust = 0.5,
                                                        size = .plot_base))
  }
  patchwork::wrap_plots(one_("Pre-FAST"), one_("Post-FAST"), ncol = 2L)
}

#' Figure 9 in numbers: the industry means per period, and the cells that carry fewer than 30 contracts
#' @param .tab_data Every sample from `des_data_f09()`.
#' @return Invisibly, the industry table.
des_report_f09 <- function(.tab_data) {
  if (FALSE) .tab_data <- tab_f09
  ind_ <- .tab_data |>
    dplyr::summarise(Share = sum(.data$Share * .data$N) / sum(.data$N), nThin = sum(.data$N < 30L),
                     .by = c("Sample", "Period", "Industry")) |>
    dplyr::mutate(Col = paste0(.data$Sample, "_", .data$Period)) |>
    dplyr::select("Industry", "Col", "Share") |>
    tidyr::pivot_wider(names_from = "Col", values_from = "Share") |>
    dplyr::arrange(match(.data$Industry, .des_ff12_levels))
  tbl_out(.tab = des_fmt(ind_, 3L), .title = "Figure 9: share redacted by industry and period, per sample",
          .notes = c(Industry = "Contract-weighted over types. Her heat map had rows that barely differed; these differ."))
  invisible(ind_)
}


# 26. F07: geographic diversity ---------------------------------------------------------------------------------------------
# Countries mentioned against segment-sales diversity, at firm-year grain. Base: the firm-year mean
# over the firm's contracts, both variables trimmed at the 99th percentile, fifty equal-count bins.
# AK: one contract per firm-year, the top percentile of both dropped, 85 bins -- what 104 draws.

.des_reference_f07 <- tibble::tribble(
  ~Quantity,   ~Revision,
  "Slope",     1.2,
  "TStat",     33
)

#' Firm-years with their country count and diversity, under one grain rule
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @param .conc Tibble from `des_read_concentration()`.
#' @param .grain Character. "mean" over the firm's contracts, or "first" contract, as 104 does.
#' @return Tibble: Sample, Grain, gvkey, fyear, Countries, Diversity.
des_data_f07 <- function(.tab, .sample, .conc, .grain = "mean") {
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
#' @param .tab_data One sample's rows from `des_data_f07()`.
#' @param .bins Integer. Equal-count bins of diversity.
#' @param .trim Character. "p99" clips both at the 99th percentile; "drop" removes the top percentile of both.
#' @return List: Bins (tibble), Fit (tibble: Slope, TStat, N).
des_bins_f07 <- function(.tab_data, .bins = 50L, .trim = "p99") {
  if (FALSE) {
    .tab_data <- des_slice(tab_f07, "S3_Matched")
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

#' Figure 7 drawn
#' @param .tab_data One sample's rows from `des_data_f07()`.
#' @return A ggplot.
des_plot_f07 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f07, "S3_Matched")
  ak_ <- unique(.tab_data$Grain) == "first"
  b_  <- des_bins_f07(.tab_data, .bins = if (ak_) 85L else 50L, .trim = if (ak_) "drop" else "p99")
  ggplot2::ggplot(b_$Bins, ggplot2::aes(x = .data$Diversity, y = .data$Countries)) +
    ggplot2::geom_abline(intercept = b_$Fit$Intercept, slope = b_$Fit$Slope, colour = .plot_ref, linewidth = 0.6) +
    ggplot2::geom_point(colour = .plot_ink, size = 1.6) +
    ggplot2::annotate(
      "text", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5, family = .plot_font, size = .plot_base / ggplot2::.pt,
      label = sprintf("slope %.2f, t = %.1f, N = %s", b_$Fit$Slope, b_$Fit$TStat, format(b_$Fit$N, big.mark = ","))
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(x = "Segment geographical diversity", y = "Countries mentioned per contract") +
    plot_theme(.grid = "both", .legend = "none")
}

#' Figure 7 in numbers: slope and t per sample and grain
#' @param .tab_data Every sample from `des_data_f07()`.
#' @return Invisibly, the table.
des_report_f07 <- function(.tab_data, .ref = .des_reference_f07) {
  if (FALSE) {
    .tab_data <- tab_f07
    .ref      <- .des_reference_f07
  }
  out_ <- .tab_data |>
    dplyr::group_split(.data$Sample) |>
    purrr::map(\(.d) {
      ak_ <- .d$Grain[1L] == "first"
      f_  <- des_bins_f07(.d, .bins = if (ak_) 85L else 50L, .trim = if (ak_) "drop" else "p99")$Fit
      dplyr::mutate(f_, Sample = .d$Sample[1L], Grain = .d$Grain[1L])
    }) |>
    purrr::list_rbind() |>
    dplyr::select("Sample", "Grain", "N", "Slope", "TStat")
  tbl_out(.tab = des_fmt(out_, 2L), .title = "Figure 7: slope of countries on diversity, per sample and grain",
          .notes = c(Grain = "Revision 1 prints slope 1.2, t 33, on one contract per firm-year."))
  invisible(out_)
}


# 27. F06: geographic mentions, two maps ------------------------------------------------------------------------------------
# The paper's Figure 6: where the contracts point. Panel A is the US states, Panel B the countries,
# each filled by the number of contracts naming the place. Four views of the same file: every
# mention outside a law clause, as the revision drew it; the mentions attached to a counterparty,
# which is referee 1's question -- are these contracting locations? -- answered by construction; the
# mentions attached to the registrant, which is the filer's own footprint; and the law-clause places,
# which are jurisdictions and are drawn apart so that the other three cannot be read as them.

.des_place_views <- c(
  Any          = "every place outside a governing-law clause, attached to a party or not",
  Counterparty = "places attached to a counterparty: contracting locations",
  Registrant   = "places attached to the registrant: the filer's own footprint",
  Law          = "places named inside a governing-law clause: jurisdictions"
)

#' Contracts per state and per country, per view
#'
#' ONE ROW PER SAMPLE, PANEL, VIEW AND AREA. The count is distinct contracts naming the place under
#' the view, which is what a map of prevalence wants; the mention count rides beside it for a map of
#' intensity. The Places release is at primary-copy grain, so a sample that carries co-filer copies
#' contributes its primary copies here and nothing else.
#'
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @param .places The Places release from `des_read_places()`.
#' @return Tibble: Sample, Panel ("State" or "Country"), View, Area, nContracts, nMentions, nBase,
#'   Share -- nBase being the sample's contracts with any place under the view, Share the ratio.
des_data_f06 <- function(.tab, .sample, .places) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample <- "S2_Descriptive"
    .places <- tab_places
  }
  plc_ <- .places |>
    dplyr::semi_join(dplyr::distinct(.tab, .data$DocID), by = dplyr::join_by(DocID))

  view_ <- function(.d, .view) {
    keep_ <- switch(
      .view,
      Any          = .d$InLawClause == 0L,
      Counterparty = .d$InLawClause == 0L & .d$PartyRole == "counterparty",
      Registrant   = .d$InLawClause == 0L & .d$PartyRole == "registrant",
      Law          = .d$InLawClause == 1L
    )
    .d[keep_, , drop = FALSE] |> dplyr::mutate(View = .view)
  }
  long_ <- purrr::map(names(.des_place_views), \(.v) view_(plc_, .v)) |> purrr::list_rbind()

  one_ <- function(.d, .panel, .col) {
    d_ <- dplyr::filter(.d, !is.na(.data[[.col]]))
    if (.panel == "State") d_ <- dplyr::filter(d_, .data$GeoCountryIso == "USA")
    base_ <- d_ |> dplyr::summarise(nBase = dplyr::n_distinct(.data$DocID), .by = "View")
    d_ |>
      dplyr::summarise(
        nContracts = dplyr::n_distinct(.data$DocID),
        nMentions  = sum(.data$nMentions),
        .by        = c("View", dplyr::all_of(.col))
      ) |>
      dplyr::rename(Area = dplyr::all_of(.col)) |>
      dplyr::left_join(base_, by = dplyr::join_by(View)) |>
      dplyr::mutate(Panel = .panel, Share = .data$nContracts / .data$nBase)
  }

  dplyr::bind_rows(one_(long_, "State", "GeoState"), one_(long_, "Country", "GeoCountryIso")) |>
    dplyr::mutate(Sample = .sample) |>
    dplyr::select("Sample", "Panel", "View", "Area", "nContracts", "nMentions", "nBase", "Share") |>
    dplyr::arrange(.data$Panel, match(.data$View, names(.des_place_views)), dplyr::desc(.data$nContracts))
}

#' Figure 6, one panel: the states or the countries, for the one view the rows carry
#'
#' TWO FIGURES, NOT ONE. The paper's Panel A and Panel B are separate figures on the page, and a
#' patchwork of two maps shares one height and one legend row badly. Each panel is its own plot
#' function, and the view is chosen by the rows handed in: the runbook slices tab_f06 by View once,
#' so the same two functions draw all four views.
#'
#' @param .tab_data One sample's rows from `des_data_f06()`, one View.
#' @param .panel Character. "State" or "Country".
#' @return A ggplot.
des_shape_f06 <- function(.tab_data, .panel) {
  if (FALSE) {
    .tab_data <- des_slice(tab_f06_any, "S2_Descriptive")
    .panel    <- "State"
  }
  views_ <- unique(.tab_data$View)
  if (length(views_) != 1L) cli::cli_abort("Figure 6 draws one view at a time; the rows carry {length(views_)}.")
  d_ <- .tab_data |>
    dplyr::filter(.data$Panel == .panel) |>
    dplyr::select("Area", N = "nContracts")
  if (nrow(d_) == 0L) d_ <- tibble::tibble(Area = character(0), N = numeric(0))
  if (.panel == "State") {
    plot_map_usa(.tab = d_, .val = "N", .log = TRUE, .name = "Contracts", .label_n = 8L)
  } else {
    plot_map_world(.tab = d_, .val = "N", .log = TRUE, .name = "Contracts", .label_n = 8L)
  }
}

#' Figure 6 Panel A, the states, and Panel B, the countries
#' @param .tab_data One sample's rows from `des_data_f06()`, one View.
#' @return A ggplot.
des_plot_f06a <- function(.tab_data) des_shape_f06(.tab_data = .tab_data, .panel = "State")
des_plot_f06b <- function(.tab_data) des_shape_f06(.tab_data = .tab_data, .panel = "Country")

#' The leading places per view on the paper's sample, printed
#' @param .tab_data Every sample from `des_data_f06()`.
#' @param .sample Character. The sample to print.
#' @param .top Integer. Places per panel and view.
#' @return Invisibly, the printed table.
des_report_f06 <- function(.tab_data, .sample = "S2_Descriptive", .top = 8L) {
  if (FALSE) {
    .tab_data <- tab_f06
    .sample   <- "S2_Descriptive"
    .top      <- 8L
  }
  d_ <- .tab_data |>
    dplyr::filter(.data$Sample == .sample) |>
    dplyr::slice_max(.data$nContracts, n = .top, by = c("Panel", "View"), with_ties = FALSE) |>
    dplyr::mutate(Rank = dplyr::row_number(), .by = c("Panel", "View")) |>
    dplyr::mutate(Cell = paste0(.data$Area, " (", tbl_num(100 * .data$Share, .digits = 1L), "%)")) |>
    dplyr::select("Panel", "View", "Rank", "Cell") |>
    tidyr::pivot_wider(names_from = "View", values_from = "Cell")
  base_ <- .tab_data |>
    dplyr::filter(.data$Sample == .sample) |>
    dplyr::distinct(.data$Panel, .data$View, .data$nBase) |>
    dplyr::mutate(Cell = tbl_num(.data$nBase, .digits = 0L), Rank = 0L) |>
    dplyr::select("Panel", "View", "Rank", "Cell") |>
    tidyr::pivot_wider(names_from = "View", values_from = "Cell")
  tbl_out(.tab = dplyr::bind_rows(base_, d_) |> dplyr::arrange(.data$Panel, .data$Rank),
          .title = paste0("Figure 6: the leading places on ", .sample,
                          ", share of the contracts with any place in the view"),
          .notes = c(Rank = "Row 0 is the base: contracts in the sample naming at least one place under the view."))
  invisible(d_)
}


# 28. F11: the pandemic terms -----------------------------------------------------------------------------------------------
# Referee 2 asked for a basic vocabulary search for Covid-19-related terms. 05A ran it, with four
# control families -- disruption, rate reform, regulation, boilerplate -- so that a rise in one word
# list can be told from a rise in words. This draws the one panel the paper needs from the released
# hits: the share of each quarter's contracts naming a term of each family, and the quarter's count
# split by whether the pandemic family hit.

plot_register_levels(
  .key     = "TermFamily",
  .levels  = .des_family_levels,
  .short   = c("Pandemic", "Disruption", "Rate reform", "Regulation", "Boilerplate"),
  .colours = NULL
)

#' Hit shares per period and family, at year and at quarter grain
#'
#' THE DENOMINATOR IS THE CONTRACTS 05A SCANNED, not the contracts that hit: TermDocs carries every
#' scanned contract five times, once per family, with zeros, and a contract in the sample the scan
#' never reached is absent rather than a zero.
#'
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @param .terms The TermDocs release from `des_read_term_docs()`.
#' @param .from_quarter Integer. First year of the quarterly grain.
#' @return Tibble: Sample, Grain ("Year" or "Quarter"), Period (numeric: the year, or year + (q-1)/4),
#'   Family, nDocs, nWith, Share, nHits.
des_data_f11 <- function(.tab, .sample, .terms, .from_quarter = 2018L) {
  if (FALSE) {
    .tab          <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample       <- "S2_Descriptive"
    .terms        <- tab_term_docs
    .from_quarter <- 2018L
  }
  j_ <- .tab |>
    dplyr::select("DocID", "Year", "DateFiled") |>
    dplyr::inner_join(.terms, by = dplyr::join_by(DocID), relationship = "one-to-many") |>
    dplyr::mutate(Quarter = .data$Year + (lubridate::quarter(.data$DateFiled) - 1L) / 4)

  one_ <- function(.d, .grain, .col) {
    .d |>
      dplyr::summarise(
        nDocs  = dplyr::n(),
        nWith  = sum(.data$HasTerm == 1L),
        nHits  = sum(.data$nHits),
        .by    = c(dplyr::all_of(.col), "Family")
      ) |>
      dplyr::rename(Period = dplyr::all_of(.col)) |>
      dplyr::mutate(Grain = .grain, Share = .data$nWith / .data$nDocs)
  }
  dplyr::bind_rows(
    one_(j_, "Year", "Year"),
    one_(dplyr::filter(j_, .data$Year >= .from_quarter), "Quarter", "Quarter")
  ) |>
    dplyr::mutate(Sample = .sample, Family = factor(.data$Family, levels = .des_family_levels)) |>
    dplyr::select("Sample", "Grain", "Period", "Family", "nDocs", "nWith", "Share", "nHits") |>
    dplyr::arrange(.data$Grain, .data$Period, .data$Family)
}

#' Figure 11: family shares per quarter above, the quarter's contracts split by the pandemic hit below
#' @param .tab_data One sample's rows from `des_data_f11()`.
#' @return A patchwork.
des_plot_f11 <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f11, "S2_Descriptive")
  q_ <- dplyr::filter(.tab_data, .data$Grain == "Quarter")
  top_ <- des_shape_lines(
    .tab = q_, .x = "Period", .y = "Share", .group = "Family", .key = "TermFamily", .pct = TRUE,
    .regimes = FALSE, .ylab = "Share of contracts naming a term", .points = FALSE, .step = 1
  ) +
    ggplot2::geom_vline(xintercept = 2020, linetype = "dashed", colour = .plot_ref, linewidth = .plot_line * 2)

  bot_ <- q_ |>
    dplyr::filter(.data$Family == "Pandemic") |>
    dplyr::transmute(.data$Period, Mentions = .data$nWith, Silent = .data$nDocs - .data$nWith) |>
    tidyr::pivot_longer(c("Mentions", "Silent"), names_to = "Kind", values_to = "N") |>
    dplyr::mutate(Kind = factor(.data$Kind, levels = c("Silent", "Mentions"),
                                labels = c("No pandemic term", "Names a pandemic term")))
  bars_ <- ggplot2::ggplot(bot_, ggplot2::aes(x = .data$Period, y = .data$N / 1000, fill = .data$Kind)) +
    ggplot2::geom_col(width = 0.22, colour = "white", linewidth = .plot_line) +
    ggplot2::scale_fill_manual(values = purrr::set_names(plot_pal_seq(2L), levels(bot_$Kind)), name = NULL) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(1)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::geom_vline(xintercept = 2020, linetype = "dashed", colour = .plot_ref, linewidth = .plot_line * 2) +
    ggplot2::labs(x = NULL, y = "Contracts per quarter (000)") +
    plot_theme(.grid = "y", .legend = "bottom")

  patchwork::wrap_plots(top_, bars_, ncol = 1L, heights = c(1.4, 1))
}

#' The pandemic share by year and by filing group: which channel carried the rise
#' @param .tab One sample of the contract table.
#' @param .sample Character. Its name.
#' @param .terms The TermDocs release.
#' @param .from Integer. First year.
#' @return Tibble: Sample, Year, FilingGroup4, nDocs, nWith, Share.
des_data_f11b <- function(.tab, .sample, .terms, .from = 2016L) {
  if (FALSE) {
    .tab    <- tab_contracts[tab_contracts$S2_Descriptive, , drop = FALSE]
    .sample <- "S2_Descriptive"
    .terms  <- tab_term_docs
    .from   <- 2016L
  }
  .tab |>
    dplyr::filter(.data$Year >= .from) |>
    dplyr::select("DocID", "Year", "FormFamily") |>
    dplyr::inner_join(dplyr::filter(.terms, .data$Family == "Pandemic"), by = dplyr::join_by(DocID),
                      relationship = "one-to-one") |>
    dplyr::mutate(FilingGroup4 = des_group4(.data$FormFamily)) |>
    dplyr::summarise(nDocs = dplyr::n(), nWith = sum(.data$HasTerm == 1L), .by = c("Year", "FilingGroup4")) |>
    dplyr::mutate(Sample = .sample, Share = .data$nWith / .data$nDocs) |>
    dplyr::select("Sample", "Year", "FilingGroup4", "nDocs", "nWith", "Share") |>
    dplyr::arrange(.data$Year, .data$FilingGroup4)
}

#' Figure 11b: the pandemic share per year, one line per filing group
#' @param .tab_data One sample's rows from `des_data_f11b()`.
#' @return A ggplot.
des_plot_f11b <- function(.tab_data) {
  if (FALSE) .tab_data <- des_slice(tab_f11b, "S2_Descriptive")
  des_shape_lines(
    .tab = .tab_data, .x = "Year", .y = "Share", .group = "FilingGroup4", .key = "FilingGroup4", .pct = TRUE,
    .regimes = FALSE, .ylab = "Share naming a pandemic term", .step = 1
  )
}

#' The four filing groups from the form family, as Figure 2's four-group view draws them
#' @param .form Character. FormFamily values.
#' @return Character: 8-K, Registration, Yearly or Quarterly.
des_group4 <- function(.form) {
  if (FALSE) .form <- c("8-K", "S-1", "10-K")
  grp_ <- unname(.des_form_group[.form])
  dplyr::case_when(
    .form == "8-K"                           ~ "8-K",
    .form %in% c("S-1", "S-4", "F-1", "F-4") ~ "Registration",
    .default                                 = grp_
  )
}

#' The pandemic shares by year on the paper's sample, printed
#' @param .tab_data Every sample from `des_data_f11()`.
#' @param .sample Character. The sample to print.
#' @return Invisibly, the year table.
des_report_f11 <- function(.tab_data, .sample = "S2_Descriptive") {
  if (FALSE) {
    .tab_data <- tab_f11
    .sample   <- "S2_Descriptive"
  }
  yr_ <- .tab_data |>
    dplyr::filter(.data$Sample == .sample, .data$Grain == "Year", .data$Period >= 2016) |>
    dplyr::mutate(Cell = 100 * .data$Share, Year = as.integer(.data$Period)) |>
    dplyr::select("Year", "Family", "Cell") |>
    tidyr::pivot_wider(names_from = "Family", values_from = "Cell")
  n_ <- .tab_data |>
    dplyr::filter(.data$Sample == .sample, .data$Grain == "Year", .data$Period >= 2016, .data$Family == "Pandemic") |>
    dplyr::transmute(Year = as.integer(.data$Period), .data$nDocs)
  tbl_out(.tab = des_fmt(dplyr::left_join(n_, yr_, by = dplyr::join_by(Year)), 1L, .counts = c("Year", "nDocs")),
          .title = paste0("Pandemic and control families by year on ", .sample, ", percent of scanned contracts"),
          .notes = c(nDocs = "Contracts 05A scanned; a contract the scan never reached is absent, not a zero."))
  invisible(yr_)
}


# 29. CTO, the order side ---------------------------------------------------------------------------------------------------
# What the SEC issued, from 01E's references: orders per year, what they did, how many exhibits each
# named, how many of those resolved to a contract at all and how many to one in the paper's sample.
# The contract side, in section 19, counts covered contracts; this counts the paperwork behind them,
# which is what section 3.5 of the manuscript describes.

# What the old text printed: 10,360 orders, 19,844 exhibits named, 3,623 unmatched, 16,221 linked.
.des_reference_cto_orders <- tibble::tribble(
  ~Quantity,   ~Revision,
  "Orders",    10360,
  "Exhibits",  19844,
  "Linked",    16221,
  "Unlinked",   3623
)

#' Orders and references by year and in total
#' @param .orders The CtoOrders release from `des_read_cto_orders()`.
#' @param .tab The contract table; the descriptive sample's DocIDs say what "in sample" means.
#' @return Tibble: Year (with "Total"), Orders, Granting, Denying, Extensions, Exhibits, Linked,
#'   InSample, Unlinked, Contracts.
des_data_cto_orders <- function(.orders, .tab) {
  if (FALSE) {
    .orders <- tab_cto_orders
    .tab    <- tab_contracts
  }
  keep_ <- .tab$DocID[.tab$Keep]
  one_ <- function(.d, .y) {
    tibble::tibble(
      Year       = .y,
      Orders     = dplyr::n_distinct(.d$OrderDocID),
      Granting   = dplyr::n_distinct(.d$OrderDocID[.d$Status %in% "GRANTING"]),
      Denying    = dplyr::n_distinct(.d$OrderDocID[.d$Status %in% c("DENYING", "REVOKING")]),
      Extensions = dplyr::n_distinct(.d$OrderDocID[.d$IsExtension == 1L]),
      Exhibits   = sum(!is.na(.d$ExhibitNo)),
      Linked     = sum(!is.na(.d$DocID)),
      InSample   = sum(.d$DocID %in% keep_),
      Unlinked   = sum(!is.na(.d$ExhibitNo) & is.na(.d$DocID)),
      Contracts  = dplyr::n_distinct(.d$DocID[!is.na(.d$DocID)])
    )
  }
  by_ <- .orders |>
    dplyr::group_split(.data$OrderYear) |>
    purrr::map(\(.d) one_(.d, as.character(.d$OrderYear[1L]))) |>
    purrr::list_rbind()
  dplyr::bind_rows(by_, one_(.orders, "Total"))
}

#' The order-side counts printed, beside the old text's numbers
#' @param .tab_data From `des_data_cto_orders()`.
#' @param .ref The reference tibble.
#' @return Invisibly, the table.
des_report_cto_orders <- function(.tab_data, .ref = .des_reference_cto_orders) {
  if (FALSE) {
    .tab_data <- tab_cto_orders_year
    .ref      <- .des_reference_cto_orders
  }
  cnt_ <- setdiff(names(.tab_data), "Year")
  tbl_out(.tab = des_fmt(.tab_data, 0L, .counts = cnt_),
          .title = "Confidential-treatment orders by year of the order: what the SEC issued",
          .notes = c(
            Exhibits = "References with an exhibit number parsed; an order can name several.",
            Linked   = "References resolved to a contract in the release; InSample is those in the descriptive sample.",
            Unlinked = "References with a number and no contract: the chain broke at one of 01E's numbered steps."
          ))
  tot_ <- dplyr::filter(.tab_data, .data$Year == "Total")
  cmp_ <- .ref |>
    dplyr::mutate(Pipeline = c(tot_$Orders, tot_$Exhibits, tot_$Linked, tot_$Unlinked)[match(.data$Quantity, .ref$Quantity)])
  tbl_out(.tab = des_fmt(cmp_, 0L, .counts = c("Revision", "Pipeline")),
          .title = "Section 3.5: the old text's counts against 01E's table",
          .notes = c(Revision = "The old pipeline's numbers; 01E re-parsed every order, so all four move."))
  invisible(.tab_data)
}


# 30. Manifest and reconciliation -------------------------------------------------------------------------------------------

#' Every file this render wrote, by exhibit and sample
#' @param .dirs Named character vector: Data, Figures, Tables.
#' @param .path Character. Where to write the manifest.
#' @return Invisibly, the manifest tibble.
des_manifest <- function(.dirs, .path) {
  if (FALSE) {
    .dirs <- c(Data = .lP$Output$DirData, Figures = .lP$Output$DirFigures, Tables = .lP$Output$DirTables)
    .path <- fs::path(.dir_main, "Output", "Manifest.csv")
  }
  out_ <- purrr::imap(.dirs, \(.d, .kind) {
    f_ <- fs::dir_ls(.d, type = "file")
    tibble::tibble(Kind = .kind, File = fs::path_file(f_), SizeKB = round(as.numeric(fs::file_size(f_)) / 1024, 1))
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      Exhibit = stringi::stri_extract_first_regex(.data$File, "^[A-Za-z0-9]+"),
      Sample  = stringi::stri_extract_first_regex(.data$File, "(?<=_)S[0-9A-Za-z]+_[A-Za-z0-9]+|(?<=_)T4_[A-Za-z]+")
    ) |>
    dplyr::relocate("Exhibit", "Sample", "Kind", "File", "SizeKB") |>
    dplyr::arrange(.data$Exhibit, .data$Kind, .data$File)
  readr::write_csv(out_, .path)
  by_ <- out_ |>
    dplyr::count(.data$Kind, Ext = fs::path_ext(.data$File)) |>
    dplyr::mutate(Label = paste0(.data$n, " ", .data$Ext)) |>
    dplyr::summarise(Label = paste(.data$Label, collapse = ", "), .by = "Kind") |>
    dplyr::mutate(Label = paste0(.data$Kind, ": ", .data$Label))
  cli::cli_alert_success(
    "Manifest: {nrow(out_)} files under {length(.dirs)} directories, written to {.file {fs::path_file(.path)}}"
  )
  cli::cli_alert_info("{paste(by_$Label, collapse = '; ')}")
  invisible(out_)
}

#' Every number the manuscript prints beside what this render gives on the paper's own sample
#'
#' The one table to send. Each exhibit contributed its reference tibble; this joins the pipeline
#' value from the exhibit tibbles the runbook holds and prints the difference.
#'
#' @param .env Environment holding the exhibit tibbles (the runbook's).
#' @return Invisibly, the table.
des_table_reconcile <- function(.env = parent.frame()) {
  if (FALSE) .env <- globalenv()
  g_ <- function(.n) if (exists(.n, envir = .env)) get(.n, envir = .env) else NULL
  rows_ <- list()

  if (!is.null(t_ <- g_("tab_ladder"))) {
    rows_$T2 <- tibble::tibble(Exhibit = "T2A", Quantity = t_$Quantity, Pipeline = abs(t_$Contracts)) |>
      dplyr::left_join(.des_reference, by = dplyr::join_by(Quantity))
  }
  if (!is.null(t_ <- g_("tab_f01"))) {
    rows_$F1 <- t_ |> dplyr::filter(.data$Sample == "S2_Descriptive") |>
      dplyr::distinct(.data$FormFamily, .data$Share) |>
      dplyr::transmute(Exhibit = "F1", Quantity = paste0("Share ", .data$FormFamily), Pipeline = 100 * .data$Share) |>
      dplyr::left_join(dplyr::mutate(.des_reference_f01, Quantity = paste0("Share ", .data$FormFamily)) |>
                         dplyr::select("Quantity", "Revision"), by = dplyr::join_by(Quantity))
  }
  if (!is.null(t_ <- g_("tab_f03"))) {
    rows_$F3 <- t_ |> dplyr::filter(.data$Sample == "S2_Descriptive", .data$Class != "Unlabelled") |>
      dplyr::transmute(Exhibit = "F3", Quantity = paste0("Share ", .des_class_short[match(.data$Class, .des_class_levels)]),
                       Pipeline = 100 * .data$Share, Class = .data$Class) |>
      dplyr::left_join(.des_reference_f03, by = dplyr::join_by(Class)) |> dplyr::select(-"Class")
  }
  if (!is.null(t_ <- g_("tab_t03"))) {
    rows_$T3 <- t_ |> dplyr::filter(.data$Sample == "S2_Descriptive", .data$Group == "Total", .data$Panel != "Redaction") |>
      dplyr::select("Panel", "N", "Mean", "SD", "P50") |>
      tidyr::pivot_longer(-"Panel", names_to = "Quantity", values_to = "Pipeline") |>
      dplyr::inner_join(.des_reference_t03, by = dplyr::join_by(Panel, Quantity)) |>
      dplyr::transmute(Exhibit = "T3", Quantity = paste(.data$Panel, .data$Quantity), .data$Pipeline, .data$Revision)
  }
  if (!is.null(t_ <- g_("tab_f10"))) {
    rows_$F10 <- t_ |> dplyr::filter(.data$Sample == "S6_Redaction") |>
      dplyr::inner_join(.des_reference_f10, by = dplyr::join_by(Year, Series)) |>
      dplyr::transmute(Exhibit = "F10", Quantity = paste(.data$Series, .data$Year), Pipeline = 100 * .data$Share,
                       .data$Revision)
  }
  if (!is.null(t_ <- g_("tab_t07"))) {
    rows_$T7 <- t_ |> dplyr::filter(.data$Sample == "S7_Summaries") |>
      dplyr::select("Measure", Delayed = "MeanDelayed", Attached = "MeanAttached") |>
      tidyr::pivot_longer(-"Measure", names_to = "Group", values_to = "Pipeline") |>
      dplyr::inner_join(.des_reference_t07, by = dplyr::join_by(Measure, Group)) |>
      dplyr::transmute(Exhibit = "T7", Quantity = paste(.data$Measure, .data$Group), .data$Pipeline, .data$Revision)
  }

  if (!is.null(t_ <- g_("tab_cto_orders_year"))) {
    tot_ <- dplyr::filter(t_, .data$Year == "Total")
    rows_$CTO <- .des_reference_cto_orders |>
      dplyr::mutate(
        Exhibit  = "3.5",
        Pipeline = c(Orders = tot_$Orders, Exhibits = tot_$Exhibits, Linked = tot_$Linked,
                     Unlinked = tot_$Unlinked)[.data$Quantity]
      ) |>
      dplyr::select("Exhibit", "Quantity", "Pipeline", "Revision")
  }

  out_ <- purrr::list_rbind(rows_) |>
    dplyr::mutate(Diff = .data$Pipeline - .data$Revision,
                  RelPct = dplyr::if_else(.data$Revision != 0, 100 * .data$Diff / .data$Revision, NA_real_))
  tbl_out(.tab = des_fmt(out_, 2L, .counts = c("Pipeline", "Revision")),
          .title = "Reconciliation: revision 1 against this render, on the paper's own sample",
          .notes = c(
            RelPct = "Percent of the revision's value. Old-pipeline numbers (T7, parties, countries) are expected to differ."
          ))
  invisible(out_)
}
