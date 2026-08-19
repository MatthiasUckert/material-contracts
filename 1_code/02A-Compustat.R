# 02A-Compustat: acquire Compustat fundamentals and turn quarters into date ranges ---------------------------------------
#
# WHAT THIS FILE DOES
# Downloads the annual and quarterly fundamentals from WRDS and converts each firm-quarter into a
# date interval, so that a filing date can be matched to the quarter it falls in.
#
# IT IS AN ACQUISITION SCRIPT AND NOTHING ELSE
# It does not know what a contract is. Separating it from the register that consumes it means the
# register can be rebuilt without carrying the WRDS machinery, and a second external source -- CRSP,
# say -- becomes another script here rather than another section of an already long one.
#
# THE DOWNLOADS ARE GATED, THE CHECKS ARE NOT
# Both WRDS calls sit behind .lP$Param$Acquire, which defaults to FALSE, exactly as the EDGAR
# downloads do in 01A and 01B. Credentials are read inside the gated chunk and nowhere else, so a
# missing key file is an error only when a connection is actually wanted.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation.

if (FALSE) {
  .path_quarter <- .lP$Compustat$QuarterData
  .path_out     <- .lP$Output$QuarterRange
  .tab_range    <- tab_range
}


# 1. Dates -------------------------------------------------------------------------------------------------------------

#' Add years or quarters to a date, rolling back rather than overflowing
#'
#' Adding three months to 31 May gives 31 August, but adding three months to 30 November gives
#' 30 February, which does not exist. Base arithmetic rolls that forward into March and silently
#' moves the observation into the next quarter; rolling back to the last valid day of the intended
#' month keeps it where it belongs.
#'
#' @param .date Date vector.
#' @param .type Character. "Years" or "Quarters".
#' @param .n Integer number of periods; negative subtracts.
#' @return Date vector.
cmp_add_periods <- function(.date, .type = c("Years", "Quarters"), .n = 1L) {
  if (FALSE) {
    .date <- as.Date("2015-11-30")
    .type <- "Quarters"
    .n    <- 1L
  }

  type_ <- match.arg(.type)
  if (identical(type_, "Years")) {
    lubridate::add_with_rollback(.date, lubridate::years(.n))
  } else {
    lubridate::add_with_rollback(.date, base::months(.n * 3L))
  }
}


# 2. The Compustat matching frame --------------------------------------------------------------------------------------

#' Turn Compustat quarters into date ranges a filing date can fall inside
#'
#' A filing carries a date; Compustat carries fiscal quarters. Matching them means turning each
#' quarter into an interval and asking which one the filing date falls in. That is only well defined
#' if a firm has one observation per quarter, which Compustat does not guarantee.
#'
#' FOUR DEDUPLICATIONS, IN ORDER, EACH KEEPING THE LARGEST FIRM. Compustat carries the same quarter
#' under more than one identifier pairing: a CIK mapped to two gvkeys after a restructuring, one
#' gvkey reported under two CIKs, and the same fiscal quarter filed twice. Sorting on total assets
#' before each deduplication keeps the observation describing the larger entity, which is the parent
#' rather than a subsidiary shell in the cases where they differ.
#'
#' THE INTERVAL IS CAPPED AT ONE QUARTER. Each quarter runs to the start of the next, but a firm
#' that stops reporting leaves an interval running to its next appearance, which can be years later.
#' A filing in that gap would then be matched to a quarter it has no business being matched to, so
#' any interval longer than one quarter is truncated to one.
#'
#' @param .path_quarter Path to the Compustat quarterly parquet.
#' @param .path_out Destination parquet path.
#' @return A tibble: CIK, gvkey, datadate, fyear, fqtr, atq, DateStart, DateStop, dQtrs.
cmp_quarter_range <- function(.path_quarter, .path_out) {
  if (FALSE) {
    .path_quarter <- .lP$Compustat$QuarterData
    .path_out     <- .lP$Compustat$QuarterRange
  }

  out_ <- arrow::open_dataset(sources = .path_quarter) |>
    dplyr::select(CIK = "cik", "gvkey", "datadate", fyear = "fyearq", "fqtr", "atq") |>
    dplyr::filter(
      !is.na(.data$CIK), !is.na(.data$gvkey), !is.na(.data$datadate),
      !is.na(.data$fyear), !is.na(.data$fqtr)
    ) |>
    dplyr::collect() |>
    dplyr::arrange(.data$CIK, .data$gvkey, .data$datadate, dplyr::desc(.data$atq)) |>
    dplyr::distinct(.data$CIK, .data$gvkey, .data$datadate, .keep_all = TRUE) |>
    dplyr::arrange(.data$CIK, .data$datadate, dplyr::desc(.data$atq)) |>
    dplyr::distinct(.data$CIK, .data$datadate, .keep_all = TRUE) |>
    dplyr::arrange(.data$gvkey, .data$datadate, dplyr::desc(.data$atq)) |>
    dplyr::distinct(.data$gvkey, .data$datadate, .keep_all = TRUE) |>
    dplyr::arrange(.data$gvkey, .data$fyear, .data$fqtr, dplyr::desc(.data$atq)) |>
    dplyr::distinct(.data$gvkey, .data$fyear, .data$fqtr, .keep_all = TRUE) |>
    dplyr::arrange(.data$gvkey, .data$fyear, .data$fqtr, .data$datadate) |>
    dplyr::mutate(
      DateStart = .data$datadate,
      DateStop  = dplyr::lead(.data$datadate),
      .by       = c("gvkey", "fyear", "fqtr")
    ) |>
    dplyr::mutate(
      DateStop = dplyr::if_else(
        is.na(.data$DateStop), cmp_add_periods(.data$DateStart, "Quarters", 1L), .data$DateStop
      ),
      dQtrs = (lubridate::year(.data$DateStop) * 4L + lubridate::quarter(.data$DateStop)) -
        (lubridate::year(.data$DateStart) * 4L + lubridate::quarter(.data$DateStart)),
      DateStop = dplyr::if_else(
        .data$dQtrs > 1L, cmp_add_periods(.data$DateStart, "Quarters", 1L), .data$DateStop
      )
    )

  arrow::write_parquet(out_, .path_out)
  out_
}

#' Compustat coverage, reported before anything is joined to it
#'
#' @param .tab_range Output of cmp_quarter_range().
#' @return A one-row tibble.
cmp_coverage_summary <- function(.tab_range) {
  if (FALSE) .tab_range <- tab_range

  tibble::tibble(
    nRows      = nrow(.tab_range),
    nFirms     = dplyr::n_distinct(.tab_range$gvkey),
    nCIKs      = dplyr::n_distinct(.tab_range$CIK),
    YearFirst  = min(lubridate::year(.tab_range$DateStart), na.rm = TRUE),
    YearLast   = max(lubridate::year(.tab_range$DateStart), na.rm = TRUE),
    ShareCapped = mean(.tab_range$dQtrs > 1L, na.rm = TRUE)
  )
}


# 4. Reports -----------------------------------------------------------------------------------------------------------

#' Every report in this document, in order
#'
#' @param .tab_range Output of cmp_quarter_range().
#' @return Invisibly NULL.
cmp_report_all <- function(.tab_range) {
  if (FALSE) .tab_range <- tab_range

  tbl_head("Compustat coverage")
  tbl_out(
    .tab    = cmp_coverage_summary(.tab_range = .tab_range),
    .title  = NULL,
    .pct    = "ShareCapped",
    .digits = 1L,
    .notes  = c(ShareCapped = "Intervals truncated to one quarter because the firm stopped reporting.")
  )

  tbl_head("Firm-quarters per year")
  tbl_out(
    .tab = .tab_range |>
      dplyr::mutate(Year = lubridate::year(.data$DateStart)) |>
      dplyr::summarise(nQuarters = dplyr::n(), nFirms = dplyr::n_distinct(.data$gvkey), .by = "Year") |>
      dplyr::arrange(.data$Year) |>
      dplyr::filter(.data$Year >= 1990L),
    .title = NULL,
    .n     = 40L
  )

  invisible(NULL)
}
