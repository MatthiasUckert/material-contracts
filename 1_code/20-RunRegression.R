# 20-RunRegression.R ------------------------------------------------------------------------------
#
# The library behind 20-RunRegression.qmd. Reads Contracts.parquet and three external inputs, builds
# the two analysis panels the paper estimates on, and fits the ten regression tables.
#
# WHY THIS EXISTS. The submitted tables were produced by seven Stata do-files that assembled the
# contract table out of eight pipeline releases. Those eight are now one file, and the assembly they
# did is gone. What is left is the analysis proper: three external inputs, a firm-quarter panel, a
# contract panel, and ten specifications. That is small enough to be one library, and stating it in
# the repo's own language makes it reviewable beside everything upstream of it.
#
# Every function carries the `reg_` prefix. Compute functions return tibbles and print nothing;
# report functions print and return invisibly.

# 0. Vocabulary ------------------------------------------------------------------------------------

# The twelve classes, with Other last because it is the omitted category in every table that
# enters contract type. Identical to .clf_class_detailed in 03A; restated here so this file can be
# read without 03A, and checked against it in the runbook.
.reg_class_levels <- c(
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

# SHORT LEVELS FOR ESTIMATION. fixest names an interaction `Class::Leases:Loss` and splits on the
# colon to render it, so a level such as `Employment: Compensation` breaks the split and the row
# vanishes from the table. The factor carries these instead; the long labels return through the
# dictionary at display time and are what the data are checked against.
.reg_class_short <- c("MnA", "Peer", "EmpComp", "EmpLegal", "Credit", "Equity", "Leases", "Licenses",
                      "Assets", "CustSupp", "RnD", "Other")

# The control sets, as the paper names them. Quarter is the filing-decision set; Contract adds the
# four that only make sense once a contract exists. Full is the long list every contract-level
# table past Table E1 uses, in the paper's order.
.reg_controls <- list(
  Quarter  = c("LogMve", "OcfWin", "AcqNeg", "AcqPos", "LeverageWin", "GoodwillImp", "Loss", "RoaWin"),
  Contract = c("LogMve", "OcfWin", "AcqNeg", "AcqPos", "LeverageWin", "GoodwillImp", "Loss",
               "EqIssue", "DebtIssue", "RoaWin", "HhiSic3Win", "ChgSalesWin"),
  Full     = c("LogMve", "OcfWin", "AcqNeg", "AcqPos", "LeverageWin", "GoodwillImp", "Loss",
               "EqIssue", "DebtIssue", "RoaWin", "ChgSalesQ4Win", "MbWin")
)

# Printed names, so the tex output reads like the paper rather than like the code.
.reg_dict <- c(
  Filed             = "Filed",
  Delayed           = "Delayed filing",
  Redacted          = "Redacted",
  HasVoluntaryItem  = "Bundled",
  nItemsVoluntary   = "Voluntary items",
  LogMve            = "Size",
  OcfWin            = "Operating cash flow",
  AcqNeg            = "Acquisition (neg.)",
  AcqPos            = "Acquisition (pos.)",
  LeverageWin       = "Leverage",
  GoodwillImp       = "Goodwill impairment",
  Loss              = "Loss",
  RoaWin            = "ROA",
  EqIssue           = "Equity issuance",
  DebtIssue         = "Debt issuance",
  HhiSic3Win        = "HHI (SIC3)",
  HhiSic2Win        = "HHI (SIC2)",
  ChgSalesWin       = "Sales growth",
  ChgSalesQ4Win     = "Sales growth (4q)",
  MbWin             = "Market-to-book",
  Fluidity          = "Product market fluidity",
  ShareDelayedPre   = "Share delayed pre-2004",
  Post2004          = "Post",
  Did               = "Post x Share delayed",
  Trend             = "Trend",
  Trend2            = "Trend sq.",
  gvkey             = "Firm",
  cyear             = "Year",
  fqtr              = "Quarter",
  FfInd             = "Industry",
  State             = "State",
  Class             = "Contract type"
)

# Class labels for the tex output: the short estimation level back to the paper's name.
.reg_dict_class <- stats::setNames(
  paste0("Class: ", .reg_class_levels),
  paste0("Class::", .reg_class_short)
)

# 1. Helpers ---------------------------------------------------------------------------------------

#' Winsorise the way Stata's `winsor, p(.01)` does
#'
#' Stata's `_pctile` computes the p-th percentile as the average of the two order statistics either
#' side of n * p when n * p is an integer, and the next order statistic when it is not. That is
#' `quantile(type = 2)`; the default `type = 7` interpolates and gives a different tail value on
#' every variable, which would show up as a coefficient difference nobody could trace.
#'
#' @param .x Numeric vector. Missing values are ignored in the percentiles and stay missing.
#' @param .p Numeric. Tail probability, applied symmetrically.
#' @return Numeric vector of the same length, clipped at the p and 1 - p percentiles.
reg_winsor <- function(.x, .p = 0.01) {
  if (FALSE) {
    .x <- c(rnorm(1000), 50, -50, NA)
    .p <- 0.01
  }
  if (all(is.na(.x))) return(.x)
  q_ <- stats::quantile(.x, probs = c(.p, 1 - .p), na.rm = TRUE, type = 2L, names = FALSE)
  pmin(pmax(.x, q_[1L]), q_[2L])
}

#' Gap-aware lag on an integer time index
#'
#' `tsset id yq` then `l.x` returns the value one period back only when that period is present;
#' a firm missing the previous quarter gets a missing lag, not the value from two quarters ago.
#' `dplyr::lag()` returns the previous row regardless, which is wrong on an unbalanced panel.
#' Use inside `mutate(.by = gvkey)` after arranging on the index.
#'
#' @param .x Vector to shift.
#' @param .t Integer vector. The time index, one unit per period, unique within the group.
#' @param .k Integer. Periods back; negative values lead.
#' @return Vector of the same length as .x.
reg_lag <- function(.x, .t, .k = 1L) {
  if (FALSE) {
    .x <- c(10, 20, 40)
    .t <- c(1L, 2L, 4L)
    .k <- 1L
  }
  .x[match(.t - .k, .t)]
}

#' Stata's quarterly index: quarters since 1960q1
#'
#' Any integer index with unit steps would do for `reg_lag()`; this one is chosen so a value can
#' be compared against Stata output directly.
#'
#' @param .fyear Integer. Fiscal year.
#' @param .fqtr Integer. Fiscal quarter, 1 to 4.
#' @return Integer.
reg_yq <- function(.fyear, .fqtr) {
  if (FALSE) {
    .fyear <- 2004L
    .fqtr  <- 3L
  }
  (as.integer(.fyear) - 1960L) * 4L + as.integer(.fqtr) - 1L
}

#' Fama-French 12 industries from a four-digit SIC code
#'
#' The ranges are the standard Fama-French definition. The paper reports them in a different
#' order -- Finance first, then Business Equipment -- which `reg_ff12_adj()` applies.
#'
#' @param .sic Integer vector. Four-digit SIC.
#' @return Integer vector, 1 to 12, NA where .sic is.
reg_ff12 <- function(.sic) {
  if (FALSE) .sic <- c(2011L, 3711L, 7372L, 6021L, 9999L, NA)

  in_ <- function(.lo, .hi) !is.na(.sic) & .sic >= .lo & .sic <= .hi
  out_ <- ifelse(is.na(.sic), NA_integer_, 12L)
  out_[in_(100, 999) | in_(2000, 2399) | in_(2700, 2749) | in_(2770, 2799) | in_(3100, 3199) |
         in_(3940, 3989)] <- 1L
  out_[in_(2500, 2519) | in_(2590, 2599) | in_(3630, 3659) | in_(3710, 3711) | in_(3714, 3714) |
         in_(3716, 3716) | in_(3750, 3751) | in_(3792, 3792) | in_(3900, 3939) | in_(3990, 3999)] <- 2L
  out_[in_(2520, 2589) | in_(2600, 2699) | in_(2750, 2769) | in_(3000, 3099) | in_(3200, 3569) |
         in_(3580, 3629) | in_(3700, 3709) | in_(3712, 3713) | in_(3715, 3715) | in_(3717, 3749) |
         in_(3752, 3791) | in_(3793, 3799) | in_(3830, 3839) | in_(3860, 3899)] <- 3L
  out_[in_(1200, 1399) | in_(2900, 2999)] <- 4L
  out_[in_(2800, 2829) | in_(2840, 2899)] <- 5L
  out_[in_(3570, 3579) | in_(3660, 3692) | in_(3694, 3699) | in_(3810, 3829) | in_(7370, 7379)] <- 6L
  out_[in_(4800, 4899)] <- 7L
  out_[in_(4900, 4949)] <- 8L
  out_[in_(5000, 5999) | in_(7200, 7299) | in_(7600, 7699)] <- 9L
  out_[in_(2830, 2839) | in_(3693, 3693) | in_(3840, 3859) | in_(8000, 8099)] <- 10L
  out_[in_(6000, 6999)] <- 11L
  out_
}

#' The paper's display order for the twelve industries
#'
#' @param .ff Integer vector from `reg_ff12()`.
#' @return Integer vector, relabelled.
reg_ff12_adj <- function(.ff) {
  if (FALSE) .ff <- 1:12
  map_ <- c(`11` = 1L, `6` = 2L, `10` = 3L, `12` = 4L, `9` = 5L, `3` = 6L,
            `4` = 7L, `1` = 8L, `8` = 9L, `7` = 10L, `5` = 11L, `2` = 12L)
  unname(map_[as.character(.ff)])
}

# 2. External inputs ---------------------------------------------------------------------------------

#' Compustat quarterly fundamentals, as the paper prepares them
#'
#' One row per gvkey-quarter. The dedup order follows the do-file exactly (cik-datadate, then
#' gvkey-fyearq-fqtr, first row kept) so the surviving row is the same one. The three forward-sales
#' leads are built here because they need the full panel, before anything is dropped.
#'
#' @param .path Character. The WRDS extract, a .dta.
#' @return Tibble, one row per gvkey x fyear x fqtr, with sic1-4, FfInd, FfIndAdj and YQ added.
reg_read_compustat <- function(.path) {
  if (FALSE) .path <- .lP$Input$FilCompustat

  cli::cli_alert_info("Reading Compustat quarterly from {.file {basename(.path)}}")
  raw_ <- haven::read_dta(.path) |>
    dplyr::mutate(dplyr::across(dplyr::where(haven::is.labelled), haven::zap_labels))

  keep_ <- c("gvkey", "cik", "datadate", "fyearq", "fqtr", "fyr", "conm", "rdq",
             "actq", "ancq", "apq", "aqaq", "aqpq", "atq", "ceqq", "cheq", "chq", "cshoq", "dlttq",
             "doq", "dpq", "gdwliaq", "gdwlq", "ltq", "niq", "oiadpq", "rcaq", "saleq", "xidoq",
             "xrdq", "capxy", "oancfy", "dvpspq", "dvpsxq", "mkvaltq", "prccq", "city", "state",
             "sic", "ipodate", "cusip", "tic", "loc", "prstkcy", "sstky", "dltisy", "dltry",
             "ibq", "dlcq")

  out_ <- raw_ |>
    dplyr::filter(!is.na(.data$cik), .data$cik != "", !is.na(.data$fqtr), !is.na(.data$fyearq)) |>
    dplyr::distinct(.data$cik, .data$datadate, .keep_all = TRUE) |>
    dplyr::distinct(.data$gvkey, .data$fyearq, .data$fqtr, .keep_all = TRUE) |>
    dplyr::select(dplyr::any_of(keep_)) |>
    dplyr::rename(fyear = "fyearq") |>
    dplyr::mutate(
      gvkey    = stringi::stri_pad_left(as.character(.data$gvkey), width = 6L, pad = "0"),
      cik      = stringi::stri_pad_left(as.character(.data$cik),   width = 10L, pad = "0"),
      datadate = as.Date(.data$datadate),
      YQ       = reg_yq(.data$fyear, .data$fqtr),
      Sic4     = suppressWarnings(as.integer(.data$sic)),
      Sic3     = .data$Sic4 %/% 10L,
      Sic2     = .data$Sic4 %/% 100L,
      FfInd12  = reg_ff12(.data$Sic4),
      FfInd    = reg_ff12_adj(.data$FfInd12)
    ) |>
    dplyr::arrange(.data$gvkey, .data$YQ) |>
    dplyr::mutate(
      SalesLead2 = reg_lag(.data$saleq, .data$YQ, -2L),
      SalesLead3 = reg_lag(.data$saleq, .data$YQ, -3L),
      SalesLead4 = reg_lag(.data$saleq, .data$YQ, -4L),
      .by = "gvkey"
    )

  cli::cli_alert_success("{format(nrow(out_), big.mark = ',')} firm-quarters, {dplyr::n_distinct(out_$gvkey)} firms")
  out_
}

#' Segment sales concentration, one row per gvkey-year
#'
#' A sales Herfindahl over business segments; diversity is one minus it. Segment rows are kept only
#' where the segment date is the source date, which is the do-file's rule for the most recent
#' restatement.
#'
#' @param .path Character. The Compustat segments extract, a .dta.
#' @return Tibble: gvkey, fyear, Concentration, Diversity, nSegments.
reg_read_segments <- function(.path) {
  if (FALSE) .path <- .lP$Input$FilSegments

  cli::cli_alert_info("Reading segments from {.file {basename(.path)}}")
  out_ <- haven::read_dta(.path) |>
    dplyr::mutate(dplyr::across(dplyr::where(haven::is.labelled), haven::zap_labels)) |>
    dplyr::filter(.data$stype != "OPSEG", !is.na(.data$sales), .data$sales >= 0,
                  .data$datadate == .data$srcdate) |>
    dplyr::distinct(.data$gvkey, .data$snms, .data$sales, .data$srcdate, .keep_all = TRUE) |>
    dplyr::mutate(
      gvkey      = stringi::stri_pad_left(as.character(.data$gvkey), width = 6L, pad = "0"),
      datadate   = as.Date(.data$datadate),
      TotalSales = sum(.data$sales),
      nSegments  = dplyr::n(),
      .by = c("gvkey", "datadate")
    ) |>
    dplyr::mutate(ShareSq = (.data$sales / .data$TotalSales)^2) |>
    dplyr::summarise(
      Concentration = sum(.data$ShareSq, na.rm = TRUE),
      nSegments     = dplyr::first(.data$nSegments),
      .by = c("gvkey", "datadate")
    ) |>
    dplyr::mutate(fyear = as.integer(lubridate::year(.data$datadate)), Diversity = 1 - .data$Concentration) |>
    dplyr::distinct(.data$gvkey, .data$fyear, .keep_all = TRUE) |>
    dplyr::select("gvkey", "fyear", "Concentration", "Diversity", "nSegments")

  cli::cli_alert_success("{format(nrow(out_), big.mark = ',')} firm-years")
  out_
}

#' Hoberg-Phillips product market fluidity, one row per gvkey-year
#'
#' @param .path Character. The tab-delimited download.
#' @return Tibble: gvkey, fyear, Fluidity.
reg_read_fluidity <- function(.path) {
  if (FALSE) .path <- .lP$Input$FilFluidity

  cli::cli_alert_info("Reading fluidity from {.file {basename(.path)}}")
  out_ <- readr::read_tsv(.path, show_col_types = FALSE) |>
    dplyr::rename(fyear = "year", Fluidity = "prodmktfluid") |>
    dplyr::mutate(gvkey = stringi::stri_pad_left(as.character(.data$gvkey), width = 6L, pad = "0"),
                  fyear = as.integer(.data$fyear)) |>
    dplyr::distinct(.data$gvkey, .data$fyear, .keep_all = TRUE) |>
    dplyr::select("gvkey", "fyear", "Fluidity")

  cli::cli_alert_success("{format(nrow(out_), big.mark = ',')} firm-years")
  out_
}

# 3. Contracts ---------------------------------------------------------------------------------------

#' The contract table, with the paper's derived variables and its three definitional choices made
#'
#' Reads 10-ExportData's release and adds what the analysis needs on top of it. The three choices
#' -- which duration, which redaction count, whether a filing with no item structure counts zero
#' items or none -- are arguments, so the runbook states them and the defaults reproduce the
#' submitted tables.
#'
#' Ladder step 1 is dropped as the paper drops it. Step 2 stays, because the descriptive sample
#' (`DescSample`) is a column and each table filters on it where the paper did.
#'
#' @param .path Character. Contracts.parquet.
#' @param .duration Character. "naive" for NaiveYears, "cascade" for DurationYears.
#' @param .redaction Character. "bracketed", "withheld" or "money".
#' @param .items Character. "zero" coalesces a missing item count to 0; "missing" leaves it.
#' @param .voluntary Character. "era" is `nItemsVoluntary`, which counts 2.02 / 7.01 / 8.01 after
#'   23 August 2004 and 12 / 9 / 5 before it. "dotted" counts only the three post-reform codes, which
#'   is zero on every pre-reform filing by construction -- the submitted measure, kept so Table 8 can
#'   be shown both ways.
#' @return Tibble, one row per attachment copy.
reg_read_contracts <- function(.path, .duration = "naive", .redaction = "bracketed", .items = "zero",
                               .voluntary = "era") {
  if (FALSE) {
    .path      <- .lP$Input$FilContracts
    .duration  <- "naive"
    .redaction <- "bracketed"
    .items     <- "zero"
    .voluntary <- "era"
  }
  .duration  <- match.arg(.duration,  c("naive", "cascade"))
  .redaction <- match.arg(.redaction, c("bracketed", "withheld", "money"))
  .items     <- match.arg(.items,     c("zero", "missing"))
  .voluntary <- match.arg(.voluntary, c("era", "dotted"))

  cli::cli_alert_info("Reading contracts from {.file {basename(.path)}}")
  tab_ <- arrow::open_dataset(.path) |>
    dplyr::filter(.data$SampleStepCode != 1L) |>
    dplyr::collect()

  out_ <- tab_ |>
    dplyr::mutate(
      Year        = as.integer(lubridate::year(.data$DateFiled)),
      YQ          = reg_yq(.data$fyear, .data$fqtr),
      Keep        = .data$DescSample == 1L & .data$PrimaryFiler == 1L,
      Matched     = !is.na(.data$gvkey),
      # The paper's "delayed": disclosed in a periodic report rather than a current one.
      Delayed     = as.integer(.data$FormType %in% c("10-K", "10-K/A", "10-Q", "10-Q/A", "20-F", "20-F/A")),
      Is8K        = .data$FormType %in% c("8-K", "8-K/A"),
      ClassLabel  = .data$Class,
      ClassCode   = match(.data$Class, .reg_class_levels),
      Class       = factor(.reg_class_short[.data$ClassCode], levels = .reg_class_short),
      IsAmend     = as.integer(.data$AmendType == "Amended"),
      PostFast    = as.integer(.data$DateFiled > as.Date("2019-04-02")),
      DurationYrs = if (.duration == "naive") .data$NaiveYears else .data$DurationYears,
      nRedact     = switch(.redaction,
        bracketed = .data$nRedactExplicit + .data$nRedactSymbol + .data$nRedactBlank +
          .data$nOmitExplicit + .data$nOmitSymbol,
        withheld  = .data$nRedactExplicit + .data$nRedactSymbol + .data$nRedactBlank + .data$nRedactBare,
        money     = .data$nRedactMoney
      ),
      # Pre-FAST redaction required an order; post-FAST it is self-executing and shows as markers.
      Redacted    = dplyr::case_when(
        .data$Year < 2008 ~ NA_integer_,
        .data$HasCto == 1L ~ 1L,
        .data$Year > 2018 & !is.na(.data$nRedact) & .data$nRedact > 0L ~ 1L,
        .default = 0L
      ),
      nItemsVoluntary = if (.voluntary == "era") .data$nItemsVoluntary else
        .data$ReportsItem202 + .data$ReportsItem701 + .data$ReportsItem801,
      HasVoluntaryItem = as.integer(.data$nItemsVoluntary > 0L),
      nParties    = .data$nUniSpellingsNaive,
      nPlaces     = .data$nUniStateNaive + .data$nUniCountryNaive
    )

  if (.items == "zero") {
    out_ <- out_ |>
      dplyr::mutate(
        nItems           = dplyr::coalesce(.data$nItems, 0L),
        nItemsVoluntary  = dplyr::coalesce(.data$nItemsVoluntary, 0L),
        HasVoluntaryItem = dplyr::coalesce(.data$HasVoluntaryItem, 0L)
      )
  }

  cli::cli_alert_success(
    "{format(nrow(out_), big.mark = ',')} rows; {format(sum(out_$Keep), big.mark = ',')} descriptive-sample primary copies"
  )
  out_
}

# 4. Firm-quarter panel ----------------------------------------------------------------------------------

#' The rows that count as a contract, stated once
#'
#' Both panels must agree on which attachment copies exist, or a contract can sit on a firm-quarter
#' whose count says none was filed. The quarter panel counts these rows; the contract panel carries
#' exactly these rows; the validation that the second is a subset of the first then holds by
#' construction rather than by luck.
#'
#' @param .contracts Tibble from `reg_read_contracts()`.
#' @param .primary Logical. TRUE keeps primary registrant copies only.
#' @return The subset: matched to a firm-quarter, not malformed, and primary if asked.
reg_rows_counted <- function(.contracts, .primary = FALSE) {
  if (FALSE) {
    .contracts <- contracts
    .primary   <- FALSE
  }
  out_ <- dplyr::filter(.contracts, .data$Matched, .data$SampleStepCode != 2L)
  if (.primary) out_ <- dplyr::filter(out_, .data$PrimaryFiler == 1L)
  out_
}

#' Contract counts per firm-quarter, joined onto the full Compustat panel
#'
#' The one place a non-event exists. Every Compustat firm-quarter appears; the ones with no
#' contract carry zero counts; firms that never file leave at the end, as the paper drops them.
#'
#' Counts are grouped on gvkey throughout. The do-file grouped the total on gvkey and the
#' periodic-report and per-class counts on cik; within a quarter the two partitions coincide
#' wherever cik maps to one gvkey, which the interval match guarantees, so this is a tidy-up
#' rather than a change.
#'
#' @param .contracts Tibble from `reg_read_contracts()`.
#' @param .compustat Tibble from `reg_read_compustat()`.
#' @param .fluidity Tibble from `reg_read_fluidity()`.
#' @param .primary Logical. TRUE counts primary registrant copies only; FALSE counts every row,
#'   which is what the submitted tables did.
#' @return Tibble, one row per gvkey x fyear x fqtr, all Compustat columns plus the counts.
reg_build_quarter <- function(.contracts, .compustat, .fluidity, .primary = FALSE) {
  if (FALSE) {
    .contracts <- reg_read_contracts(.lP$Input$FilContracts)
    .compustat <- reg_read_compustat(.lP$Input$FilCompustat)
    .fluidity  <- reg_read_fluidity(.lP$Input$FilFluidity)
    .primary   <- FALSE
  }

  counts_ <- reg_rows_counted(.contracts, .primary = .primary) |>
    dplyr::summarise(
      nContractsQ   = dplyr::n(),
      nDelayedQ     = sum(.data$Delayed, na.rm = TRUE),
      DateFiledQ    = min(.data$DateFiled),
      dplyr::across(
        .cols  = "ClassCode",
        .fns   = purrr::set_names(
          purrr::map(1:12, \(.k) \(.x) sum(.x == .k, na.rm = TRUE)),
          paste0("nClass", 1:12, "Q")
        ),
        .names = "{.fn}"
      ),
      .by = c("gvkey", "fyear", "fqtr")
    )

  out_ <- .compustat |>
    dplyr::left_join(counts_, by = c("gvkey", "fyear", "fqtr")) |>
    dplyr::left_join(.fluidity, by = c("gvkey", "fyear")) |>
    dplyr::mutate(
      dplyr::across(c("nContractsQ", "nDelayedQ", dplyr::starts_with("nClass")), \(.x) dplyr::coalesce(.x, 0L)),
      Filed      = as.integer(.data$nContractsQ > 0L),
      cyear      = as.integer(lubridate::year(.data$datadate)),
      PostFast   = as.integer(.data$datadate > as.Date("2019-04-02")),
      ShareDelayedQ = dplyr::if_else(.data$nContractsQ > 0L, .data$nDelayedQ / .data$nContractsQ, NA_real_)
    ) |>
    dplyr::mutate(
      nContractsY = sum(.data$nContractsQ),
      FiledY      = as.integer(.data$nContractsY > 0L),
      .by = c("gvkey", "fyear")
    ) |>
    dplyr::mutate(nContractsTotal = sum(.data$nContractsQ), .by = "gvkey")

  cli::cli_alert_success(
    "{format(nrow(out_), big.mark = ',')} firm-quarters, {format(sum(out_$Filed), big.mark = ',')} with a contract"
  )
  out_
}

#' The accounting variables, as Table 4 defines them
#'
#' Winsorising is at 1/99 over the whole panel, once, before any sample is drawn -- which is the
#' do-file's order and the one that keeps every table on the same clipped values. Lags respect
#' gaps. The Herfindahls are computed on the full Compustat universe, not on filers, because a
#' concentration measure over filers alone measures who files.
#'
#' ONE DEPARTURE. The do-file's quarterly operating cash flow was `d.oancfy` with a Q1 correction
#' that, as written, replaced the variable with itself. Q1 here is the year-to-date figure, which
#' is what a first quarter's quarterly flow is. The equity-issuance block in the same file handles
#' the identical case correctly, so this is the treatment the author intended.
#'
#' @param .quarter Tibble from `reg_build_quarter()`.
#' @return The same tibble with the variable block added and never-filers removed.
reg_add_firm_vars <- function(.quarter) {
  if (FALSE) .quarter <- reg_build_quarter(contracts, compustat, fluidity)

  win_ <- c("prccq", "cshoq", "mkvaltq", "niq", "ceqq", "xrdq", "atq", "saleq", "oiadpq", "oancfy",
            "capxy", "ltq", "aqaq", "xidoq", "prstkcy", "sstky", "dltisy", "dltry", "ibq", "dlttq",
            "dlcq", "nContractsQ")

  out_ <- .quarter |>
    dplyr::mutate(dplyr::across(dplyr::all_of(win_), reg_winsor, .names = "{.col}Win")) |>
    dplyr::mutate(saleq = dplyr::if_else(.data$saleq < 0, NA_real_, .data$saleq)) |>
    # Herfindahls on the full universe, before any lag or filter can thin it.
    dplyr::mutate(
      Sic3Sales = sum(.data$saleq, na.rm = TRUE),
      .by = c("Sic3", "fyear", "fqtr")
    ) |>
    dplyr::mutate(Sic2Sales = sum(.data$saleq, na.rm = TRUE), .by = c("Sic2", "fyear", "fqtr")) |>
    dplyr::mutate(
      HhiSic3 = sum((.data$saleq / .data$Sic3Sales)^2, na.rm = TRUE),
      .by = c("Sic3", "fyear", "fqtr")
    ) |>
    dplyr::mutate(
      HhiSic2 = sum((.data$saleq / .data$Sic2Sales)^2, na.rm = TRUE),
      .by = c("Sic2", "fyear", "fqtr")
    ) |>
    dplyr::arrange(.data$gvkey, .data$YQ) |>
    dplyr::mutate(
      LagAtq       = reg_lag(.data$atq,    .data$YQ),
      LagAtqWin    = reg_lag(.data$atqWin, .data$YQ),
      LeadSales    = reg_lag(.data$saleq,  .data$YQ, -1L),
      LagOancfy    = reg_lag(.data$oancfy, .data$YQ),
      LagSstkyWin  = reg_lag(.data$sstkyWin,   .data$YQ),
      LagPrstkyWin = reg_lag(.data$prstkcyWin, .data$YQ),
      .by = "gvkey"
    ) |>
    dplyr::mutate(
      # Size and value
      Mve          = .data$prccqWin * .data$cshoqWin,
      LogMve       = log(.data$Mve),
      Mb           = (.data$prccq * .data$cshoq) / .data$ceqq,
      RnD          = .data$xrdq / .data$atq,
      # Growth, three horizons plus the one-quarter version
      ChgSales     = (.data$LeadSales  / .data$atq) - (.data$saleq / .data$LagAtq),
      ChgSalesQ2   = (.data$SalesLead2 / .data$atq) - (.data$saleq / .data$LagAtq),
      ChgSalesQ3   = (.data$SalesLead3 / .data$atq) - (.data$saleq / .data$LagAtq),
      ChgSalesQ4   = (.data$SalesLead4 / .data$atq) - (.data$saleq / .data$LagAtq),
      # Quarterly operating cash flow from the year-to-date figure
      Ocf          = dplyr::if_else(.data$fqtr == 1L, .data$oancfy, .data$oancfy - .data$LagOancfy),
      # Leverage on lagged assets, with missing debt read as none
      TotalDebt    = dplyr::coalesce(.data$dlttq, 0) + dplyr::coalesce(.data$dlcq, 0),
      Leverage     = .data$TotalDebt / .data$LagAtq,
      # Indicators
      AcqNeg       = as.integer(!is.na(.data$aqaqWin) & .data$aqaqWin < 0),
      AcqPos       = as.integer(!is.na(.data$aqaqWin) & .data$aqaqWin > 0),
      Roa          = .data$ibq / .data$LagAtq,
      Loss         = as.integer(!is.na(.data$niq) & .data$niq < 0),
      GoodwillImp  = as.integer(!is.na(.data$gdwliaq) & .data$gdwliaq < 0),
      # Issuance, from year-to-date flows differenced within the year
      SstkyQ       = dplyr::if_else(.data$fqtr == 1L, dplyr::coalesce(.data$sstkyWin, 0),
                                    dplyr::coalesce(.data$sstkyWin, 0) - dplyr::coalesce(.data$LagSstkyWin, 0)),
      PrstkyQ      = dplyr::if_else(.data$fqtr == 1L, dplyr::coalesce(.data$prstkcyWin, 0),
                                    dplyr::coalesce(.data$prstkcyWin, 0) - dplyr::coalesce(.data$LagPrstkyWin, 0)),
      EqIssueRaw   = (.data$SstkyQ - .data$PrstkyQ) / .data$LagAtqWin,
      EqIssue      = as.integer(!is.na(.data$EqIssueRaw) & .data$EqIssueRaw >= 0.05)
    ) |>
    dplyr::arrange(.data$gvkey, .data$YQ) |>
    dplyr::mutate(LagTotalDebt = reg_lag(.data$TotalDebt, .data$YQ), .by = "gvkey") |>
    dplyr::mutate(
      DebtChg      = (.data$TotalDebt - .data$LagTotalDebt) / .data$LagTotalDebt,
      DebtIssue    = dplyr::case_when(
        !is.na(.data$DebtChg) ~ as.integer(.data$DebtChg >= 0.05),
        .data$TotalDebt == 0  ~ 0L,
        .default = NA_integer_
      ),
      # Second-pass winsorising of the constructed ratios
      MbWin        = reg_winsor(.data$Mb),
      RnDWin       = reg_winsor(.data$RnD),
      ChgSalesWin  = reg_winsor(.data$ChgSales),
      ChgSalesQ2Win = reg_winsor(.data$ChgSalesQ2),
      ChgSalesQ3Win = reg_winsor(.data$ChgSalesQ3),
      ChgSalesQ4Win = reg_winsor(.data$ChgSalesQ4),
      OcfWin       = reg_winsor(.data$Ocf),
      HhiSic3Win   = reg_winsor(.data$HhiSic3),
      HhiSic2Win   = reg_winsor(.data$HhiSic2),
      LeverageWin  = reg_winsor(.data$Leverage),
      RoaWin       = reg_winsor(.data$Roa),
      State        = factor(.data$state)
    ) |>
    dplyr::filter(.data$nContractsTotal > 0L)

  cli::cli_alert_success(
    "{format(nrow(out_), big.mark = ',')} firm-quarters for {dplyr::n_distinct(out_$gvkey)} firms that filed at least once"
  )
  out_
}

# 5. Contract panel ------------------------------------------------------------------------------------

#' The contract panel: one row per attachment copy, carrying its firm-quarter's variables
#'
#' A many-to-one join on the quarter keys. Nothing is computed here that the quarter panel does
#' not already carry, so a contract-level regression and a firm-quarter regression control for
#' the same numbers.
#'
#' @param .contracts Tibble from `reg_read_contracts()`.
#' @param .quarter Tibble from `reg_add_firm_vars()`.
#' @param .primary Logical. Must match what `reg_build_quarter()` was given.
#' @return Tibble, one row per counted contract copy that matched a firm-quarter in the panel.
reg_build_contract <- function(.contracts, .quarter, .primary = FALSE) {
  if (FALSE) {
    .contracts <- reg_read_contracts(.lP$Input$FilContracts)
    .quarter   <- reg_add_firm_vars(reg_build_quarter(.contracts, compustat, fluidity))
    .primary   <- FALSE
  }

  firm_ <- .quarter |>
    dplyr::select(-dplyr::any_of(c("cik", "datadate", "cyear", "PostFast", "YQ", "DateFiledQ")))

  out_ <- reg_rows_counted(.contracts, .primary = .primary) |>
    dplyr::inner_join(firm_, by = c("gvkey", "fyear", "fqtr"), relationship = "many-to-one")

  cli::cli_alert_success(
    "{format(nrow(out_), big.mark = ',')} contract copies joined to {dplyr::n_distinct(out_$gvkey)} firms"
  )
  out_
}

# 6. Samples --------------------------------------------------------------------------------------------

#' The filing-decision sample: firm-quarters 2001-2024 with every control present
#'
#' Firms that never file within the window leave, as do rows missing any control or fixed effect.
#'
#' THE SAME-SAMPLE FILTER. The submitted Table 7 estimated its pooled columns on the sample its
#' conditional-logit columns needed: firms whose outcome never varies -- always-filers and
#' never-filers -- were dropped before both, and again within each FAST period. That keeps every
#' column on one sample and removes the largest firms from the pooled model, which is where its
#' leverage coefficient comes from. TRUE reproduces it; FALSE is the pooled population, and lets
#' fixest and survival drop non-varying strata where they actually matter.
#'
#' @param .quarter Tibble from `reg_add_firm_vars()`.
#' @param .years Integer length two. Inclusive calendar-year window on datadate.
#' @param .same_sample Logical. Drop firms with no within-variation, overall and within FAST period.
#' @return Tibble.
reg_sample_quarter <- function(.quarter, .years = c(2001L, 2024L), .same_sample = FALSE) {
  if (FALSE) {
    .quarter     <- quarter
    .years       <- c(2001L, 2024L)
    .same_sample <- FALSE
  }
  need_ <- c("Filed", .reg_controls$Quarter, "fyear", "fqtr", "FfInd", "State")

  out_ <- .quarter |>
    dplyr::filter(.data$cyear >= .years[1L], .data$cyear <= .years[2L]) |>
    tidyr::drop_na(dplyr::all_of(need_)) |>
    dplyr::mutate(EverFiled = max(.data$Filed), .by = "gvkey") |>
    dplyr::filter(.data$EverFiled == 1L) |>
    dplyr::select(-"EverFiled")

  if (.same_sample) {
    out_ <- out_ |>
      dplyr::mutate(MeanAll = mean(.data$Filed), .by = "gvkey") |>
      dplyr::mutate(MeanFast = mean(.data$Filed), .by = c("gvkey", "PostFast")) |>
      dplyr::filter(.data$MeanAll > 0, .data$MeanAll < 1, .data$MeanFast > 0, .data$MeanFast < 1) |>
      dplyr::select(-"MeanAll", -"MeanFast")
  }

  n_filed_ <- format(sum(out_$Filed), big.mark = ",")
  cli::cli_alert_success(
    "{format(nrow(out_), big.mark = ',')} firm-quarters, {dplyr::n_distinct(out_$gvkey)} firms, {n_filed_} filing"
  )
  out_
}

#' The contract-level sample: matched, well-formed, with every control present
#'
#' @param .contract Tibble from `reg_build_contract()`.
#' @param .min_year Integer. Earliest filing year kept. The delay and contract-type tables start
#'   in 2005, redaction in 2008, bundling in 2004.
#' @param .controls Character. Which control set must be complete.
#' @param .primary Logical. TRUE keeps primary registrant copies only. The submitted tables ran on
#'   every copy, so a contract two registrants filed entered twice with the same text and two
#'   different firm-quarters; FALSE reproduces that, TRUE is the defensible sample.
#' @return Tibble.
reg_sample_contract <- function(.contract, .min_year = 2005L, .controls = .reg_controls$Full,
                                .primary = FALSE) {
  if (FALSE) {
    .contract <- contract
    .min_year <- 2005L
    .controls <- .reg_controls$Full
    .primary  <- FALSE
  }
  need_ <- unique(c(.controls, "fyear", "fqtr", "FfInd", "State", "Class"))

  out_ <- .contract |>
    dplyr::filter(.data$cyear >= 2001L, .data$cyear <= 2024L, .data$Year >= .min_year,
                  .data$DescSample == 1L) |>
    tidyr::drop_na(dplyr::all_of(need_))
  if (.primary) out_ <- dplyr::filter(out_, .data$PrimaryFiler == 1L)

  cli::cli_alert_success(
    "{format(nrow(out_), big.mark = ',')} contracts from {dplyr::n_distinct(out_$gvkey)} firms, filed {(.min_year)} onward"
  )
  out_
}

# 7. Estimation ------------------------------------------------------------------------------------------

#' One specification, under one of three estimators
#'
#' `logit` is the paper's pooled logit: fixed effects entered as dummies, which fixest absorbs.
#' `lpm` is the paper's `reghdfe` column: a linear model with the firm effect absorbed too.
#' `clogit` is the paper's conditional logit, through `survival::clogit()` with exact conditional
#' likelihood -- the same estimator, and slow enough on 1.4 million rows that it is opt-in.
#'
#' A logit with the firm effect absorbed by fixest is NOT the same as `clogit`. It maximises the
#' unconditional likelihood with the firm intercepts concentrated out, which is the
#' incidental-parameters-biased estimator conditional logit exists to avoid. That is why `lpm`
#' rather than `feglm` stands beside `clogit` here: the linear firm-effect column is what the
#' paper actually reports, and it needs no defence.
#'
#' Standard errors cluster on the firm in every case.
#'
#' @param .data Tibble.
#' @param .dv Character. The outcome column.
#' @param .rhs Character vector. Regressors; a factor is entered through `i()` automatically.
#' @param .fe Character vector. Fixed effects to absorb (logit, lpm) or enter as dummies (clogit).
#' @param .estimator Character. "logit", "lpm" or "clogit".
#' @param .firm_fe Logical. Absorb the firm effect (lpm) or stratify on it (clogit). Ignored for logit.
#' @param .subset Logical vector or NULL. Row filter applied before fitting.
#' @param .cluster Character. Cluster variable.
#' @return A fitted model object.
reg_fit <- function(.data, .dv, .rhs, .fe = c("cyear", "fqtr"), .estimator = "logit",
                    .firm_fe = FALSE, .subset = NULL, .cluster = "gvkey") {
  if (FALSE) {
    .data      <- sample_quarter
    .dv        <- "Filed"
    .rhs       <- .reg_controls$Quarter
    .fe        <- c("cyear", "fqtr", "FfInd", "State")
    .estimator <- "logit"
    .firm_fe   <- FALSE
    .subset    <- NULL
    .cluster   <- "gvkey"
  }
  .estimator <- match.arg(.estimator, c("logit", "lpm", "clogit"))
  dat_ <- if (is.null(.subset)) .data else .data[.subset, , drop = FALSE]

  # Factors enter through i() so fixest labels the levels. THE REFERENCE IS THE LAST LEVEL, stated
  # explicitly: fixest's own default is the first, which would make every class coefficient
  # relative to Investment and Merger rather than to Other. An i() the caller wrote out is passed
  # through untouched, so an interaction can carry its own ref.
  rhs_ <- purrr::map_chr(.rhs, \(.v) {
    if (!is.null(dat_[[.v]]) && is.factor(dat_[[.v]])) {
      paste0("i(", .v, ", ref = \"", utils::tail(levels(dat_[[.v]]), 1L), "\")")
    } else {
      .v
    }
  })
  rhs_ <- paste(rhs_, collapse = " + ")

  if (.estimator == "clogit") {
    fe_  <- paste(paste0("factor(", .fe, ")"), collapse = " + ")
    fml_ <- stats::as.formula(paste0(.dv, " ~ ", rhs_, " + ", fe_, " + strata(", .cluster, ")"))
    return(survival::clogit(fml_, data = dat_, method = "exact"))
  }

  fe_all_ <- if (.estimator == "lpm" && .firm_fe) c(.fe, .cluster) else .fe
  fml_    <- stats::as.formula(paste0(.dv, " ~ ", rhs_, " | ", paste(fe_all_, collapse = " + ")))
  cl_     <- stats::as.formula(paste0("~", .cluster))

  fit_ <- if (.estimator == "logit") {
    fixest::feglm(fml_, data = dat_, family = stats::binomial(), cluster = cl_, notes = FALSE)
  } else {
    fixest::feols(fml_, data = dat_, cluster = cl_, notes = FALSE)
  }

  # notes = FALSE keeps the singleton-removal chatter out of the render, but it also silences the
  # one note that matters. A regressor dropped for collinearity is a specification error, and it
  # is reported here so it cannot hide behind a clean table.
  if (length(fit_$collin.var) > 0L) {
    cli::cli_alert_warning("{(.dv)}: removed for collinearity: {paste(fit_$collin.var, collapse = ', ')}")
  }
  fit_
}

#' The paper's three-way split: full sample, pre-FAST, post-FAST
#'
#' @param .data Tibble carrying PostFast.
#' @param ... Passed to `reg_fit()`.
#' @return Named list of three models.
reg_fit_fast <- function(.data, ...) {
  if (FALSE) .data <- sample_contract
  list(
    Full = reg_fit(.data, ...),
    Pre  = reg_fit(.data, .subset = .data$PostFast == 0L, ...),
    Post = reg_fit(.data, .subset = .data$PostFast == 1L, ...)
  )
}

#' Write a model list to a tex file and return the etable
#'
#' @param .models Named list of fitted models.
#' @param .path Character. Destination .tex.
#' @param .title Character. Table title.
#' @param .keep Character or NULL. Regex on coefficient names to keep; NULL keeps all.
#' @return The etable, invisibly.
reg_tex <- function(.models, .path, .title, .keep = NULL) {
  if (FALSE) {
    .models <- list(Full = fit_a, Pre = fit_b)
    .path   <- fs::path(.lP$Output$DirTables, "filed.tex")
    .title  <- "Filing decision"
    .keep   <- NULL
  }
  fs::dir_create(fs::path_dir(.path))
  tab_ <- fixest::etable(
    .models,
    dict     = c(.reg_dict, .reg_dict_class),
    keep     = .keep,
    signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10),
    digits   = 3L,
    fitstat  = ~ n + pr2 + r2,
    title    = .title,
    tex      = TRUE,
    file     = .path,
    replace  = TRUE
  )
  cli::cli_alert_success("wrote {.file {fs::path_file(.path)}}")
  invisible(tab_)
}

# 8. Tables ----------------------------------------------------------------------------------------------

#' Table 7: the filing decision at firm-quarter grain
#'
#' Six columns: pooled logit and firm-effect specification, each on the full window and the two
#' FAST sub-periods. Firm effects are the LPM by default; pass "clogit" to add the exact match.
#'
#' @param .sample Tibble from `reg_sample_quarter()`.
#' @param .estimators Character. Any of "logit", "lpm", "clogit"; the first is always pooled.
#' @return Named list of models.
reg_table_filed <- function(.sample, .estimators = c("logit", "lpm")) {
  if (FALSE) {
    .sample     <- sample_quarter
    .estimators <- c("logit", "lpm")
  }
  fe_pool_ <- c("cyear", "fqtr", "FfInd", "State")
  fe_firm_ <- c("cyear", "fqtr")
  out_ <- list()
  if ("logit" %in% .estimators) {
    out_ <- c(out_, stats::setNames(
      reg_fit_fast(.sample, .dv = "Filed", .rhs = .reg_controls$Quarter, .fe = fe_pool_, .estimator = "logit"),
      c("Logit", "Logit pre", "Logit post")
    ))
  }
  if ("lpm" %in% .estimators) {
    out_ <- c(out_, stats::setNames(
      reg_fit_fast(.sample, .dv = "Filed", .rhs = .reg_controls$Quarter, .fe = fe_firm_,
                   .estimator = "lpm", .firm_fe = TRUE),
      c("FE LPM", "FE LPM pre", "FE LPM post")
    ))
  }
  if ("clogit" %in% .estimators) {
    out_ <- c(out_, stats::setNames(
      reg_fit_fast(.sample, .dv = "Filed", .rhs = .reg_controls$Quarter, .fe = fe_firm_, .estimator = "clogit"),
      c("Clogit", "Clogit pre", "Clogit post")
    ))
  }
  out_
}

#' Table E1: which contract types are disclosed late
#'
#' Delayed on the eleven type dummies (Other omitted) and the quarter controls. Pooled logit only,
#' as in the paper.
#'
#' @param .sample Tibble from `reg_sample_contract()` with .min_year = 2005.
#' @return Named list of three models.
reg_table_contract_type <- function(.sample) {
  if (FALSE) .sample <- sample_contract
  stats::setNames(
    reg_fit_fast(.sample, .dv = "Delayed", .rhs = c("Class", .reg_controls$Contract),
                 .fe = c("cyear", "fqtr", "FfInd"), .estimator = "logit"),
    c("Full", "Pre-FAST", "Post-FAST")
  )
}

#' Tables 9 and 10: delayed filing and redaction, with competition entered two ways
#'
#' The same shape for both outcomes. `Fluidity` is the paper's primary competition measure and
#' `HhiSic2Win` its robustness alternative; each gets the three-way FAST split, pooled and with
#' firm effects.
#'
#' @param .sample Tibble from `reg_sample_contract()`.
#' @param .dv Character. "Delayed" or "Redacted".
#' @param .competition Character. "Fluidity" or "HhiSic2Win".
#' @param .estimators Character. As in `reg_table_filed()`.
#' @return Named list of models.
reg_table_outcome <- function(.sample, .dv, .competition = "Fluidity", .estimators = c("logit", "lpm")) {
  if (FALSE) {
    .sample      <- sample_contract
    .dv          <- "Delayed"
    .competition <- "Fluidity"
    .estimators  <- c("logit", "lpm")
  }
  rhs_     <- c(.reg_controls$Full, .competition)
  fe_pool_ <- c("cyear", "fqtr", "FfInd", "Class")
  fe_firm_ <- c("cyear", "fqtr", "Class")
  dat_     <- tidyr::drop_na(.sample, dplyr::all_of(c(.dv, .competition)))
  out_ <- list()
  if ("logit" %in% .estimators) {
    out_ <- c(out_, stats::setNames(
      reg_fit_fast(dat_, .dv = .dv, .rhs = rhs_, .fe = fe_pool_, .estimator = "logit"),
      c("Logit", "Logit pre", "Logit post")
    ))
  }
  if ("lpm" %in% .estimators) {
    out_ <- c(out_, stats::setNames(
      reg_fit_fast(dat_, .dv = .dv, .rhs = rhs_, .fe = fe_firm_, .estimator = "lpm", .firm_fe = TRUE),
      c("FE LPM", "FE LPM pre", "FE LPM post")
    ))
  }
  if ("clogit" %in% .estimators) {
    out_ <- c(out_, stats::setNames(
      reg_fit_fast(dat_, .dv = .dv, .rhs = rhs_, .fe = fe_firm_, .estimator = "clogit"),
      c("Clogit", "Clogit pre", "Clogit post")
    ))
  }
  out_
}

#' Appendix D1: bundling of contracts with voluntary 8-K items
#'
#' 8-K filings from 2004 only, since the item is a property of the parent 8-K. Panel A regresses
#' the bundling indicator and the count on controls; Panel B interacts contract type with Loss.
#'
#' @param .sample Tibble from `reg_sample_contract()` with .min_year = 2004.
#' @param .estimators Character. As in `reg_table_filed()`.
#' @return Named list of models.
reg_table_bundling <- function(.sample, .estimators = c("logit", "lpm")) {
  if (FALSE) {
    .sample     <- sample_contract
    .estimators <- c("logit", "lpm")
  }
  dat_ <- .sample |>
    dplyr::filter(.data$Is8K, .data$cyear >= 2004L) |>
    tidyr::drop_na("Fluidity")
  rhs_ <- c(.reg_controls$Full, "Fluidity")
  fe_  <- c("cyear", "fqtr", "Class")

  out_ <- list(
    `A: Count FE LPM` = reg_fit(dat_, .dv = "nItemsVoluntary", .rhs = rhs_, .fe = fe_, .estimator = "lpm", .firm_fe = TRUE),
    `A: Bundled FE LPM` = reg_fit(dat_, .dv = "HasVoluntaryItem", .rhs = rhs_, .fe = fe_, .estimator = "lpm",
                                  .firm_fe = TRUE),
    # The interaction names its reference. Without one, i() emits all twelve Class x Loss columns,
    # which sum to the Loss main effect already in rhs_: exact collinearity.
    `B: Count x Loss` = reg_fit(dat_, .dv = "nItemsVoluntary",
                                .rhs = c("i(Class, Loss, ref = \"Other\")", "Class", rhs_),
                                .fe = c("cyear", "fqtr"), .estimator = "lpm", .firm_fe = TRUE)
  )
  if ("clogit" %in% .estimators) {
    out_[["A: Bundled clogit"]] <- reg_fit(dat_, .dv = "HasVoluntaryItem", .rhs = rhs_, .fe = fe_, .estimator = "clogit")
  }
  out_
}

#' Table 8: the 2004 reform as a continuous difference-in-differences
#'
#' Treatment intensity is the firm's pre-reform share of contracts disclosed in periodic reports;
#' the outcome is the number of voluntary items on the parent 8-K. One row per firm-quarter,
#' fiscal years before 2008, with a quadratic trend in calendar months.
#'
#' Grouped on gvkey throughout. The do-file computed the intensity from a cik-grouped numerator
#' and a gvkey-grouped denominator, and its dependent variable was whichever contract row
#' survived a `duplicates drop` rather than the average the table notes describe. Both are
#' replaced: the intensity is a within-gvkey share, and the outcome is the firm-quarter mean.
#'
#' @param .contract Tibble from `reg_build_contract()`.
#' @return Named list of three models: pooled, industry effects, firm effects.
reg_table_did <- function(.contract) {
  if (FALSE) .contract <- contract

  reform_ <- as.Date("2004-08-23")
  dat_ <- .contract |>
    dplyr::filter(.data$Matched, .data$SampleStepCode != 2L, .data$fyear < 2008L) |>
    dplyr::mutate(
      Pre2004 = .data$DateFiled < reform_,
      ShareDelayedPre = mean(.data$Delayed[.data$Pre2004]),
      .by = "gvkey"
    ) |>
    dplyr::summarise(
      nItemsVoluntary = mean(.data$nItemsVoluntary, na.rm = TRUE),
      DateFiled       = min(.data$DateFiled),
      datadate        = dplyr::first(.data$datadate),
      dplyr::across(dplyr::all_of(c(.reg_controls$Full, "Fluidity", "FfInd", "ShareDelayedPre")), dplyr::first),
      .by = c("gvkey", "fyear", "fqtr")
    ) |>
    tidyr::drop_na(dplyr::all_of(c("ShareDelayedPre", .reg_controls$Full, "Fluidity"))) |>
    dplyr::mutate(
      Post2004 = as.integer(.data$datadate >= reform_),
      Did      = .data$Post2004 * .data$ShareDelayedPre,
      Month    = lubridate::year(.data$DateFiled) * 12L + lubridate::month(.data$DateFiled)
    ) |>
    dplyr::mutate(
      Trend  = 0.01 * (.data$Month - min(.data$Month) + 1L),
      Trend2 = .data$Trend^2
    )

  base_ <- c("ShareDelayedPre", "Post2004", "Did", "Trend", "Trend2", .reg_controls$Full, "Fluidity")
  list(
    Pooled   = fixest::feols(stats::as.formula(paste("nItemsVoluntary ~", paste(base_, collapse = " + "))),
                             data = dat_, cluster = ~gvkey, notes = FALSE),
    Industry = fixest::feols(stats::as.formula(paste("nItemsVoluntary ~", paste(base_, collapse = " + "), "| FfInd")),
                             data = dat_, cluster = ~gvkey, notes = FALSE),
    Firm     = fixest::feols(stats::as.formula(paste("nItemsVoluntary ~",
                                                     paste(setdiff(base_, "ShareDelayedPre"), collapse = " + "),
                                                     "| gvkey")),
                             data = dat_, cluster = ~gvkey, notes = FALSE)
  )
}

# 9. Reports ---------------------------------------------------------------------------------------------

#' What the two panels look like, in the numbers a reader checks first
#'
#' @param .quarter Tibble from `reg_add_firm_vars()`.
#' @param .contract Tibble from `reg_build_contract()`.
#' @return Invisibly, a one-row tibble of the counts printed.
reg_report_panels <- function(.quarter, .contract) {
  if (FALSE) {
    .quarter  <- quarter
    .contract <- contract
  }
  tab_ <- tibble::tibble(
    Panel = c("Firm-quarter", "Contract"),
    Rows  = c(nrow(.quarter), nrow(.contract)),
    Firms = c(dplyr::n_distinct(.quarter$gvkey), dplyr::n_distinct(.contract$gvkey)),
    Years = c(paste(range(.quarter$fyear), collapse = "-"), paste(range(.contract$fyear), collapse = "-")),
    Event = c(sum(.quarter$Filed), sum(.contract$Keep))
  )
  tbl_out(tab_, .title = "The two panels",
          .notes = c(Event = "Firm-quarters with a contract; contracts in the descriptive sample as primary copies."))
  invisible(tab_)
}

#' The control variables, described the way Table 4 describes them
#'
#' @param .quarter Tibble from `reg_sample_quarter()`.
#' @param .vars Character. Which columns.
#' @return Invisibly, the descriptive table.
reg_report_controls <- function(.quarter, .vars = .reg_controls$Quarter) {
  if (FALSE) {
    .quarter <- sample_quarter
    .vars    <- .reg_controls$Quarter
  }
  tab_ <- .quarter |>
    dplyr::select(dplyr::all_of(.vars)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Variable", values_to = "Value") |>
    dplyr::summarise(
      N    = sum(!is.na(.data$Value)),
      Mean = mean(.data$Value, na.rm = TRUE),
      SD   = stats::sd(.data$Value, na.rm = TRUE),
      P25  = stats::quantile(.data$Value, 0.25, na.rm = TRUE, names = FALSE),
      P50  = stats::median(.data$Value, na.rm = TRUE),
      P75  = stats::quantile(.data$Value, 0.75, na.rm = TRUE, names = FALSE),
      .by = "Variable"
    ) |>
    dplyr::mutate(Variable = factor(.data$Variable, levels = .vars)) |>
    dplyr::arrange(.data$Variable) |>
    dplyr::mutate(Variable = unname(.reg_dict[as.character(.data$Variable)]))
  tbl_out(tab_, .title = "Controls", .digits = 3L)
  invisible(tab_)
}

#' Print a model list as a console table of coefficients and t-statistics
#'
#' @param .models Named list of fitted models.
#' @param .title Character.
#' @param .keep Character or NULL. Regex on coefficient names.
#' @return Invisibly, the etable as a data frame.
reg_report_models <- function(.models, .title, .keep = NULL) {
  if (FALSE) {
    .models <- tab_filed
    .title  <- "Filing decision"
    .keep   <- NULL
  }
  tab_ <- fixest::etable(.models, dict = c(.reg_dict, .reg_dict_class), keep = .keep,
                         signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), digits = 3L,
                         fitstat = ~ n + pr2 + r2)
  cli::cli_h3(.title)
  print(tab_)
  invisible(tab_)
}
