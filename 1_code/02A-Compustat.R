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
# WHERE THINGS ARE WRITTEN
# QuarterRange.parquet is what 02B reads, so it goes in Output/. The two downloads are neither an
# output nor a cache: they are a local copy of a licensed external archive, they cannot be rebuilt
# without WRDS credentials, and nothing downstream opens them. They sit at the script root for the
# same reason 01A's GetEDGAR/ tree does.
#
# WHAT IS RECOMPUTED, AND WHAT IS NOT
# cmp_quarter_range() takes .rerun, defaulting to FALSE, and fingerprints the quarterly download
# before deciding. Four sorted deduplications over the whole of Compustat is the only real work here,
# and it cannot produce a different answer while that file stands still. The result is also the
# published artifact, so a stale rewrite would move its modification time and make any cache 02B
# later keys on it miss for nothing.
#
# FIGURES ARE DEFINED HERE AND WRITTEN NOWHERE. The document displays what cmp_plot_coverage()
# returns; the consolidated release script writes the files the manuscript needs.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation.

if (FALSE) {
  .path_quarter <- .lP$Download$QuarterData
  .path_out     <- .lP$Output$QuarterRange
  .path_stamp   <- .lP$Cache$RangeStamp
  .tab_range    <- tab_range
  .rerun        <- FALSE
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
#' THE INTERVAL IS ONE QUARTER FROM THE FISCAL QUARTER END, ALWAYS. datadate is the date the quarter
#' closed, and the filing reporting that quarter arrives in the months after it, so [datadate,
#' datadate + 1 quarter) is the window a filing for that quarter falls in.
#'
#' THE ALTERNATIVE WAS CONSIDERED AND IS WORSE, BUT NOT PERFECT EITHER. Running each interval to the
#' start of the firm's next observation fails in two ways. A firm that stops reporting leaves an
#' interval running to its next appearance years later, and a filing anywhere in that gap is matched
#' to a quarter it has no business being matched to. A firm reporting irregularly produces intervals
#' that overlap, and a filing then matches several quarters at once.
#'
#' The fixed window removes the first failure entirely -- an interval cannot stretch -- and bounds
#' the second rather than eliminating it. Where a firm reports two quarters less than three calendar
#' months apart, which happens on a stub quarter after a fiscal year-end change and on 52/53-week
#' calendars, the windows still overlap. The document measures how often: 1,820 firm-quarters of
#' 1,676,449, or about one in a thousand, and 02B counts the filings that actually land in one.
#'
#' What the fixed window gives up is the few days before an unusually early next filing. That is the
#' smallest of the three losses and the only bounded one.
#'
#' THE COLLECTED TABLE IS SORTED BEFORE ANY DEDUPLICATION, AND THAT IS A CORRECTNESS FIX RATHER THAN
#' TIDINESS. An Arrow scan makes no promise about the order in which record batches are returned, and
#' distinct(.keep_all = TRUE) keeps the FIRST occurrence. Where two rows tie on the sort key of a
#' deduplication -- same firm, same quarter, same total assets -- which of them survived was decided
#' by whatever order Arrow happened to produce, so the published artifact was not a deterministic
#' function of its input. Sorting once on all six selected columns fixes it without touching the
#' rule: dplyr::arrange() is stable, so every later sort preserves this order among its own ties, and
#' the only cases affected are ones that were previously arbitrary.
#'
#' CACHED ON THE OUTPUT ITSELF. The four deduplications are the only real work in this document and
#' they are a pure function of the quarterly download. The result is also what 02B reads, so
#' rewriting it on every render would move its modification time and make any cache keyed on it miss
#' for nothing -- the failure 01C produced for five documents downstream.
#'
#' @param .path_quarter Path to the Compustat quarterly parquet.
#' @param .path_out Destination parquet path; the published artifact.
#' @param .path_stamp Parquet under Cache/ holding the fingerprint .path_out was built under.
#' @param .rerun Logical. TRUE rebuilds regardless of the fingerprint.
#' @return A tibble: CIK, gvkey, datadate, fyear, fqtr, atq, DateStart, DateStop.
cmp_quarter_range <- function(.path_quarter, .path_out, .path_stamp, .rerun = FALSE) {
  if (FALSE) {
    .path_quarter <- .lP$Download$QuarterData
    .path_out     <- .lP$Output$QuarterRange
    .path_stamp   <- .lP$Cache$RangeStamp
    .rerun        <- FALSE
  }

  stamp_ <- utils_dir_stamp(.dirs = .path_quarter)

  fresh_ <- !.rerun &&
    fs::file_exists(.path_out) &&
    identical(utils_stamp_read(.path = .path_stamp), stamp_)

  if (fresh_) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "Fundamentals unchanged: {format(nrow(out_), big.mark = ',')} firm-quarters read from the published file."
    )
    return(out_)
  }

  if (!.rerun && fs::file_exists(.path_stamp)) {
    cli::cli_alert_warning("The quarterly download has moved; the matching frame is being rebuilt.")
  }

  out_ <- arrow::open_dataset(sources = .path_quarter) |>
    dplyr::select(CIK = "cik", "gvkey", "datadate", fyear = "fyearq", "fqtr", "atq") |>
    dplyr::filter(
      !is.na(.data$CIK), !is.na(.data$gvkey), !is.na(.data$datadate),
      !is.na(.data$fyear), !is.na(.data$fqtr)
    ) |>
    dplyr::collect() |>
    # Every selected column, so a tie anywhere below is broken the same way on every run.
    dplyr::arrange(
      .data$CIK, .data$gvkey, .data$datadate, .data$fyear, .data$fqtr, .data$atq
    ) |>
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
      DateStop  = cmp_add_periods(.date = .data$datadate, .type = "Quarters", .n = 1L)
    )

  arrow::write_parquet(out_, .path_out)
  arrow::write_parquet(tibble::tibble(Stamp = stamp_), .path_stamp)
  cli::cli_alert_success(
    "Matching frame rebuilt: {format(nrow(out_), big.mark = ',')} firm-quarters written."
  )

  out_
}

#' Compustat coverage, reported before anything is joined to it
#'
#' @param .tab_range Output of cmp_quarter_range().
#' @return A one-row tibble.
cmp_coverage_summary <- function(.tab_range) {
  if (FALSE) .tab_range <- tab_range

  tibble::tibble(
    nRows     = nrow(.tab_range),
    nFirms    = dplyr::n_distinct(.tab_range$gvkey),
    nCIKs     = dplyr::n_distinct(.tab_range$CIK),
    YearFirst = min(lubridate::year(.tab_range$DateStart), na.rm = TRUE),
    YearLast  = max(lubridate::year(.tab_range$DateStart), na.rm = TRUE)
  )
}


#' Firm-quarters and distinct firms per calendar year
#'
#' PULLED OUT OF THE REPORTER, where it was computed inline. It is the coverage result rather than
#' formatting, the figure plots the same numbers, and the report is called twice -- so derived in
#' place it was three traversals producing three answers that must agree by construction.
#'
#' @param .tab_range Output of cmp_quarter_range().
#' @param .year_min Integer. Earliest year to report; Compustat runs back further than any sample.
#' @return A tibble: Year, nQuarters, nFirms.
cmp_quarters_by_year <- function(.tab_range, .year_min = 1990L) {
  if (FALSE) {
    .tab_range <- tab_range
    .year_min  <- 1990L
  }

  .tab_range |>
    dplyr::mutate(Year = lubridate::year(.data$DateStart)) |>
    dplyr::summarise(nQuarters = dplyr::n(), nFirms = dplyr::n_distinct(.data$gvkey), .by = "Year") |>
    dplyr::filter(.data$Year >= .year_min) |>
    dplyr::arrange(.data$Year)
}


# 4. Reports -----------------------------------------------------------------------------------------------------------

#' Every report in this document, in order
#'
#' TAKES THE TWO SUMMARIES, DOES NOT COMPUTE THEM. Shown once in Results and again in the Overview.
#'
#' @param .tab_cov Output of cmp_coverage_summary().
#' @param .tab_year Output of cmp_quarters_by_year().
#' @return Invisibly NULL.
cmp_report_all <- function(.tab_cov, .tab_year) {
  if (FALSE) {
    .tab_cov  <- tab_coverage
    .tab_year <- tab_by_year
  }

  tbl_head("Compustat coverage")
  tbl_out(
    .tab   = .tab_cov,
    .title = NULL,
    .notes = c(YearLast = "Compustat runs past the acquisition frame; the sample ladder in 02B closes it.")
  )

  tbl_head("Firm-quarters per year")
  tbl_out(
    .tab   = .tab_year,
    .title = NULL,
    .n     = 40L
  )

  invisible(NULL)
}


# 5. Figures -----------------------------------------------------------------------------------------------------------
# DEFINED HERE, WRITTEN NOWHERE. The document displays what this returns and the consolidated release
# script writes the file the manuscript needs.

#' Firm-quarters in the matching frame, per calendar year
#'
#' IN THE LIBRARY RATHER THAN THE CHUNK. It was built inline in the Overview, which put a ggplot
#' specification in a runbook and left the release script with nothing to call to reproduce it. Every
#' other figure in the pipeline is a named function for exactly that reason.
#'
#' @param .tab Output of cmp_quarters_by_year().
#' @return A ggplot object.
cmp_plot_coverage <- function(.tab) {
  if (FALSE) .tab <- tab_by_year

  ggplot2::ggplot(.tab, ggplot2::aes(x = .data$Year, y = .data$nQuarters)) +
    ggplot2::geom_col(fill = plot_pal_seq(.n = 1L)) +
    plot_scale_y_count() +
    ggplot2::labs(x = NULL, y = "Firm-quarters") +
    plot_theme(.grid = "y")
}
