# 02-SelectSample: merge the corpus to Compustat and define the samples ------------------------------------------------
#
# WHAT THIS FILE DOES
# 01C produced one metadata table describing every document acquired. This joins it to Compustat
# fundamentals on the filing date, applies the sample ladder, and writes one table per group.
#
# IT IS A SIBLING, NOT AN ANCESTOR
# Nothing in 03 or 04 reads what this produces. Classification and entity extraction work from
# 01C's metadata directly; this branch exists because the empirical analysis needs financial
# covariates and the text pipelines do not. The number tells you nothing about the other.
#
# THE ACQUISITION SWITCH
# The two WRDS downloads sit behind .lP$Param$Acquire, which defaults to FALSE, exactly as the EDGAR
# downloads do in 01A and 01B. Credentials are read inside the gated chunk and nowhere else, so a
# missing key file is an error only when a connection is actually wanted.
#
# THREE GROUPS, THREE SAMPLES
# Contracts, 8-K reports and CT orders are merged and reported separately throughout. Exhibit 10 is
# most of the corpus, so a pooled attrition rate would be the Exhibit 10 rate with the other two
# averaged into invisibility.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation.

if (FALSE) {
  .path_quarter <- .lP$Compustat$QuarterData
  .path_landing <- .lP$Input$LandingPage
  .path_meta    <- .lP$Input$MetaData
  .tab_range    <- tab_range
  .group        <- "Exhibit10"
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
smp_add_periods <- function(.date, .type = c("Years", "Quarters"), .n = 1L) {
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
smp_compustat_range <- function(.path_quarter, .path_out) {
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
        is.na(.data$DateStop), smp_add_periods(.data$DateStart, "Quarters", 1L), .data$DateStop
      ),
      dQtrs = (lubridate::year(.data$DateStop) * 4L + lubridate::quarter(.data$DateStop)) -
        (lubridate::year(.data$DateStart) * 4L + lubridate::quarter(.data$DateStart)),
      DateStop = dplyr::if_else(
        .data$dQtrs > 1L, smp_add_periods(.data$DateStart, "Quarters", 1L), .data$DateStop
      )
    )

  arrow::write_parquet(out_, .path_out)
  out_
}

#' Compustat coverage, reported before anything is joined to it
#'
#' @param .tab_range Output of smp_compustat_range().
#' @return A one-row tibble.
smp_compustat_summary <- function(.tab_range) {
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


# 3. Filing items ------------------------------------------------------------------------------------------------------

#' The 8-K item list for each filing, where it is unambiguous
#'
#' The landing table holds one row per filing and registrant, so a filing's item list appears once
#' per registrant. Where every registrant agrees the row is kept; where they do not the filing is
#' dropped rather than one version being chosen, because there is no basis for choosing and the
#' alternative is a silent coin flip on a variable used for interpretation.
#'
#' @param .path_landing Path to 01C's restricted landing table.
#' @return A tibble: HashIndex, Items.
smp_filing_items <- function(.path_landing) {
  if (FALSE) .path_landing <- .lP$Input$LandingPage

  arrow::open_dataset(sources = .path_landing) |>
    dplyr::select("HashIndex", "Items") |>
    dplyr::filter(!is.na(.data$Items)) |>
    dplyr::collect() |>
    dplyr::mutate(Items = gsub("\n", "|", .data$Items)) |>
    dplyr::distinct() |>
    dplyr::filter(dplyr::n() == 1L, .by = "HashIndex")
}


# 4. Merging -----------------------------------------------------------------------------------------------------------

#' Join one group of documents to Compustat on the filing date
#'
#' The join is an interval join: a document matches the Compustat quarter whose window contains its
#' filing date. It is a left join, so documents with no match survive and are attributed by the
#' sample ladder rather than disappearing here.
#'
#' THE DATE COLUMN IS RENAMED ON THE WAY IN. The interval join has to name the column literally, and
#' naming it once here rather than in three places is what stops the three groups drifting apart.
#'
#' @param .path_meta Path to 01C's FullMetaData.
#' @param .group Character. "Exhibit10", "8-K" or "CTO".
#' @param .tab_range Output of smp_compustat_range().
#' @param .tab_items Output of smp_filing_items().
#' @param .date_col Character. Name of the filing-date column in FullMetaData.
#' @return A tibble of merged rows, one or more per document.
smp_merge_group <- function(.path_meta, .group, .tab_range, .tab_items, .date_col = "DateFiled") {
  if (FALSE) {
    .path_meta <- .lP$Input$MetaData
    .group     <- "Exhibit10"
    .tab_range <- tab_range
    .tab_items <- tab_items
    .date_col  <- "DateFiled"
  }

  pat_ <- switch(.group, "Exhibit10" = "^Exhibit10$", "CTO" = "^CTO$", "8-K" = "^8-K")

  arrow::open_dataset(sources = .path_meta) |>
    dplyr::filter(grepl(pat_, .data$DocTypeMod)) |>
    dplyr::collect() |>
    dplyr::rename(DateFiled = dplyr::all_of(.date_col)) |>
    dplyr::left_join(
      y  = .tab_range,
      by = dplyr::join_by("CIK", "DateFiled" >= "DateStart", "DateFiled" < "DateStop")
    ) |>
    dplyr::left_join(.tab_items, by = dplyr::join_by("HashIndex"))
}

#' How many documents matched more than one Compustat quarter
#'
#' The interval join can return more than one row for a document if two of a firm's quarter windows
#' overlap. Those documents are dropped, and this counts them first: an exclusion that is applied
#' without being counted is indistinguishable from a document that was never selected.
#'
#' @param .tab Output of smp_merge_group().
#' @return A one-row tibble: nRows, nDocs, nMultiMatched, ShareMultiMatched.
smp_multimatch_summary <- function(.tab) {
  if (FALSE) .tab <- merged_ex10

  cnt_ <- .tab |>
    dplyr::summarise(nMatch = dplyr::n(), .by = "DocID")

  tibble::tibble(
    nRows             = nrow(.tab),
    nDocs             = nrow(cnt_),
    nMultiMatched     = sum(cnt_$nMatch > 1L),
    ShareMultiMatched = sum(cnt_$nMatch > 1L) / nrow(cnt_)
  )
}


# 5. The sample ladder -------------------------------------------------------------------------------------------------

#' Derive the sample flags from the ladder step
#'
#' The ladder itself is written in the document, because the thresholds and their order are the
#' argument. This is the mechanical part: the step number extracted from its own label, and the two
#' sample indicators derived from it.
#'
#' THE DESCRIPTIVE SAMPLE DOES NOT REQUIRE A COMPUSTAT MATCH. It is every well-formatted document
#' filed inside the window, which is what the corpus can describe. The estimation sample is the
#' subset that also matched, which is what a regression can use. Reporting only the second would
#' understate the corpus by whatever share of filers Compustat does not cover.
#'
#' @param .tab A merged table carrying SampleStepDesc.
#' @return The same table with SampleStepCode, DescSample and EstiSample added and moved to front.
smp_sample_flags <- function(.tab) {
  if (FALSE) .tab <- merged_ex10

  .tab |>
    dplyr::mutate(
      SampleStepCode = as.integer(stringi::stri_extract_first_regex(.data$SampleStepDesc, "\\d+")),
      DescSample     = as.integer(.data$SampleStepCode >= 3L),
      EstiSample     = as.integer(.data$SampleStepCode == 5L)
    ) |>
    dplyr::relocate("SampleStepCode", "SampleStepDesc", "DescSample", "EstiSample")
}

#' Drop the columns that were only needed for the join
#'
#' DocExt IS TESTED FOR, NOT EXTRACTED BY tools::file_ext(). That helper builds a new string for
#' every element through substring() and allocates again through ifelse(), so it passes three times
#' over a million URLs. stringi extracts in one pass; the empty string rather than NA on no match
#' keeps the column's type and meaning identical to what it replaces.
#'
#' @param .tab A merged table carrying the sample flags.
#' @return The same table, tidied.
smp_tidy_sample <- function(.tab) {
  if (FALSE) .tab <- merged_ex10

  .tab |>
    dplyr::select(-dplyr::any_of(c("atq", "DateStart", "DateStop", "dQtrs"))) |>
    dplyr::mutate(cyear = lubridate::year(.data$datadate), .before = "fyear") |>
    dplyr::mutate(
      DocExt = dplyr::coalesce(
        stringi::stri_extract_last_regex(.data$UrlDocument, "(?<=\\.)[[:alnum:]]+$"), ""
      ),
      .after = "UrlDocument"
    )
}


# 6. The selection table -----------------------------------------------------------------------------------------------

#' The sample selection table, as it appears in the paper
#'
#' One row per ladder step, showing what each step removes. Steps that remove documents are shown
#' negative, because the table is read as a subtraction from the universe down to the final sample.
#'
#' FIRM COUNTS ARE SHOWN ONLY ON THE FINAL ROW. A firm count on an intermediate step would be the
#' number of firms among the documents removed at that step, which is not the number of firms lost:
#' a firm with a hundred documents loses one and remains in the sample. Leaving those cells empty is
#' more honest than filling them with a number that invites the wrong reading, and that includes the
#' descriptive-sample subtotal: summing empty cells gives zero, and zero firms is a claim, whereas
#' empty is the absence of one.
#'
#' EVERY STEP APPEARS, INCLUDING THE ONES THAT REMOVED NOTHING. A step absent from the table because
#' it caught no document reads as an omission rather than as a zero, and the three groups would then
#' have tables of different heights that cannot be set side by side. The four exclusion steps are
#' filled in at zero where they did not fire.
#'
#' @param .tab A merged table carrying the sample flags.
#' @return A tibble, one row per step plus a descriptive-sample subtotal.
smp_sample_table <- function(.tab) {
  if (FALSE) .tab <- merged_ex10

  tmp_ <- dplyr::bind_rows(dplyr::mutate(.tab, SampleStepDesc = "00-SEC EDGAR Universe"), .tab) |>
    dplyr::mutate(
      FirmQtr  = paste0(.data$gvkey, .data$fyear, .data$fqtr),
      FirmYear = paste0(.data$gvkey, .data$fyear)
    ) |>
    dplyr::summarise(
      nFilesAll  = dplyr::n(),
      nFilesUni  = dplyr::n_distinct(.data$HashDocument),
      nFirmQtrs  = dplyr::n_distinct(.data$FirmQtr),
      nFirmYears = dplyr::n_distinct(.data$FirmYear),
      nFirms     = dplyr::n_distinct(.data$gvkey),
      .by        = "SampleStepDesc"
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = c("nFirmQtrs", "nFirmYears", "nFirms"),
      .fns  = \(.x) dplyr::if_else(startsWith(.data$SampleStepDesc, "05"), .x, NA_integer_)
    )) |>
    dplyr::mutate(dplyr::across(
      .cols = -"SampleStepDesc",
      .fns  = \(.x) dplyr::if_else(grepl("Less", .data$SampleStepDesc), -.x, .x)
    )) |>
    dplyr::bind_rows(tibble::tibble(
      SampleStepDesc = c(
        "01-Less: Outside 2001 - 2024", "02-Less: Malformatted Documents",
        "03-Less: CIK not in Compustat", "04-Less: No Compustat Match on Filing Date"
      ),
      nFilesAll = 0L, nFilesUni = 0L
    )) |>
    dplyr::distinct(.data$SampleStepDesc, .keep_all = TRUE) |>
    dplyr::arrange(.data$SampleStepDesc)

  # The subtotal sums documents and leaves the firm counts undefined: they are NA on every row it
  # sums, and summing NA to zero would assert that no firm is in the descriptive sample.
  sub_ <- dplyr::filter(tmp_, grepl("^0[012]", .data$SampleStepDesc)) |>
    dplyr::summarise(dplyr::across(c("nFilesAll", "nFilesUni"), \(.x) sum(.x, na.rm = TRUE))) |>
    dplyr::mutate(
      SampleStepDesc = "Descriptive Sample",
      nFirmQtrs = NA_integer_, nFirmYears = NA_integer_, nFirms = NA_integer_
    )

  dplyr::bind_rows(
    dplyr::filter(tmp_, grepl("^0[012]", .data$SampleStepDesc)),
    sub_,
    dplyr::filter(tmp_, grepl("^0[345]", .data$SampleStepDesc))
  ) |>
    dplyr::mutate(SampleStepDesc = gsub("^\\d+-", "", .data$SampleStepDesc)) |>
    dplyr::relocate("SampleStepDesc")
}


# 7. Reports -----------------------------------------------------------------------------------------------------------

#' Report one group's sample selection
#'
#' @param .tab A merged table carrying the sample flags.
#' @param .group Character. The group name, for the heading.
#' @return The selection table, invisibly.
smp_report_selection <- function(.tab, .group) {
  if (FALSE) {
    .tab   <- merged_ex10
    .group <- "Exhibit10"
  }

  sel_ <- smp_sample_table(.tab = .tab)

  tbl_head("Sample selection: {(.group)}")
  tbl_out(
    .tab   = sel_,
    .title = NULL,
    .notes = c(
      SampleStepDesc = "The universe row is 01C's corpus less the documents dropped for matching more \\
                        than one Compustat quarter; that count is reported above the ladder.",
      nFilesUni      = "Distinct attachments; documents fetched under several registrants count once.",
      nFirms         = "Shown only on the final row: a firm losing one document of many is not lost."
    )
  )

  invisible(sel_)
}

#' Every report in this document, in order
#'
#' @param .lst_samples Named list of the three merged tables.
#' @param .tab_range Output of smp_compustat_range().
#' @return Invisibly NULL.
smp_report_all <- function(.lst_samples, .tab_range) {
  if (FALSE) {
    .lst_samples <- lst_samples
    .tab_range   <- tab_range
  }

  tbl_head("Compustat coverage")
  tbl_out(.tab = smp_compustat_summary(.tab_range = .tab_range), .pct = "ShareCapped", .digits = 1L)

  purrr::iwalk(.lst_samples, \(.t, .g) smp_report_selection(.tab = .t, .group = .g))

  tbl_head("Contracts by form type")
  tbl_out(.tab = smp_form_counts(.tab_overview = smp_filing_overview(.lst_samples = .lst_samples)))

  invisible(NULL)
}


# 8. The filing overview -----------------------------------------------------------------------------------------------

#' Stack the three samples into one long table, at two levels of uniqueness
#'
#' Every count this document reports downstream is a slice of this table, and building it once is
#' what stops the slices disagreeing. It carries each group twice under each sample definition:
#' once per document, and once per attachment.
#'
#' TWO LEVELS, BECAUSE BOTH ARE THE ANSWER TO A DIFFERENT QUESTION. How many contract documents were
#' filed is a question about disclosure activity; how many distinct contracts exist is a question
#' about the corpus. They differ by the sixteen per cent of documents that are one attachment fetched
#' under several registrants, and quoting either without saying which is what makes the two
#' irreconcilable later.
#'
#' THE ATTACHMENT-LEVEL ROW IS CHOSEN DETERMINISTICALLY. Reducing to one row per attachment means
#' picking which registrant's row survives, and picking the first in whatever order the table happens
#' to be in makes the choice depend on how the data was read. Ordering on DocID first fixes it. The
#' counts are unaffected either way -- FormType is constant within an attachment -- but the
#' covariates carried alongside are not.
#'
#' @param .lst_samples Named list of the three sample tables.
#' @return A long tibble with sType and uType identifying the slice.
smp_filing_overview <- function(.lst_samples) {
  if (FALSE) .lst_samples <- lst_samples

  lab_ <- c(Exhibit10 = "EX10", Filing08K = "08Ks", FilingCTO = "CTOs")

  all_ <- purrr::imap(
    .x = .lst_samples,
    .f = function(.t, .g) {
      dplyr::bind_rows(
        dplyr::mutate(dplyr::filter(.t, .data$DescSample == 1L),
                      sType = paste0(lab_[[.g]], ": Descriptive Sample")),
        dplyr::mutate(dplyr::filter(.t, .data$EstiSample == 1L),
                      sType = paste0(lab_[[.g]], ": Estimation Sample"))
      )
    }
  ) |>
    dplyr::bind_rows()

  dplyr::bind_rows(
    dplyr::mutate(all_, uType = "All Contracts"),
    all_ |>
      dplyr::arrange(.data$sType, .data$HashDocument, .data$DocID) |>
      dplyr::distinct(.data$sType, .data$HashDocument, .keep_all = TRUE) |>
      dplyr::mutate(uType = "Unique Contracts")
  ) |>
    dplyr::select(dplyr::any_of(c(
      "DescSample", "EstiSample", "sType", "uType", "DocID", "HashDocument",
      "gvkey", "datadate", "cyear", "fyear", "fqtr", "YQ", "FormType", "nWords", "nWordsAdj"
    )))
}

#' Contract counts by form type, sample and uniqueness level
#'
#' The wide table the manuscript reports. Form types run across the columns because that is how a
#' reader compares them; the four rows are the sample definitions.
#'
#' @param .tab_overview Output of smp_filing_overview().
#' @return A wide tibble: SampleType, UniqueType, one column per form type.
smp_form_counts <- function(.tab_overview) {
  if (FALSE) .tab_overview <- tab_overview

  .tab_overview |>
    dplyr::filter(startsWith(.data$sType, "EX10")) |>
    dplyr::summarise(n = dplyr::n(), .by = c("sType", "uType", "FormType")) |>
    dplyr::mutate(FormType = gsub("-", "", .data$FormType)) |>
    tidyr::pivot_wider(names_from = "FormType", values_from = "n", values_fill = 0L) |>
    dplyr::rename(SampleType = "sType", UniqueType = "uType") |>
    dplyr::arrange(.data$SampleType, .data$UniqueType)
}


# 9. Figures -----------------------------------------------------------------------------------------------------------

#' Which SEC forms the contracts were attached to, original against amended
#'
#' @param .tab The Exhibit 10 sample.
#' @return A tibble: FormType, AmendType, nDocs.
smp_form_composition <- function(.tab) {
  if (FALSE) .tab <- dplyr::filter(tab_overview, .data$sType == "EX10: Estimation Sample")

  .tab |>
    dplyr::select("DocID", "HashDocument", "FormType") |>
    dplyr::mutate(
      AmendType = dplyr::if_else(endsWith(.data$FormType, "/A"), "Amended", "Original"),
      FormType  = gsub("/A$", "", .data$FormType)
    ) |>
    dplyr::summarise(nDocs = dplyr::n(), .by = c("FormType", "AmendType")) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' Contracts by form type and amendment status
#'
#' @param .tab Output of smp_form_composition().
#' @return A ggplot object.
smp_plot_forms <- function(.tab) {
  if (FALSE) .tab <- smp_form_composition(lst_samples[["Exhibit10"]])

  plot_bar_stacked(
    .tab  = .tab,
    .cat  = "FormType",
    .val  = "nDocs",
    .fill = "AmendType"
  )
}
