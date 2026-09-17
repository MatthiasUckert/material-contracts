# 02B-Register: one row per document, every decision recorded ------------------------------------------------------------
#
# WHAT THIS FILE DOES
# Joins the corpus to Compustat, applies the sample ladder, and attaches what 01D and 01E found. The
# result is one table describing every document the pipeline holds.
#
# NOTHING IS REMOVED, EVER
# The register has exactly as many rows as 01C published. A document outside the window, one that
# failed a quality rule, one whose filer is not in Compustat, one that matched two fiscal quarters --
# each is marked and kept. Completeness is then true by construction rather than by arithmetic, and
# every downstream script filters the same table rather than re-deriving a subset.
#
# That is a change from the arrangement it replaces, which deleted the documents matching more than
# one Compustat quarter. Three hundred and thirty-two of them, which is nothing; but they were
# deleted outside the ladder, so no table in the paper accounted for them.
#
# NO ABSOLUTE PATHS
# A path written on one machine is wrong on every other. The register carries the type, quarter and
# identifier that determine it, and utils_doc_path() rebuilds it at read time. That is what lets the
# table be archived and used by someone who did not run the pipeline.
#
# ONE TABLE, SORTED BY GROUP
# Contracts, 8-K reports and CT orders are one file rather than three. Parquet keeps per-row-group
# statistics, so sorting by group means a reader asking only for 8-K documents skips the contract row
# groups without reading them. One artifact, and the column sets cannot drift apart.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation.

if (FALSE) {
  .path_meta <- .lP$Input$MetaData
  .tab_range <- tab_range
  .tab       <- tab_register
}


# 1. Merging -----------------------------------------------------------------------------------------------------------

#' Join the corpus to Compustat on the filing date
#'
#' An interval join: a document matches the Compustat quarter whose window contains its filing date.
#' Left, so a document with no match survives and is attributed by the ladder rather than disappearing
#' here.
#'
#' A DOCUMENT MATCHING TWO QUARTERS HAS NO QUARTER. The join can return several rows where two of a
#' firm's windows overlap, and something has to collapse them. Deleting the document, as an earlier
#' version did, removes it from every count without recording that it existed. Setting the Compustat
#' fields to missing and keeping one row says what is true: the filing date does not determine a
#' fiscal quarter for this firm, so no quarter is assigned.
#'
#' THE INTERVAL JOIN RUNS ON THREE COLUMNS, NOT ON FORTY. Only DocID, CIK and DateFiled decide which
#' quarter a document falls in, and a non-equi join carrying the whole of the consolidated metadata
#' through it moves 1.77 million rows of text statistics and filer flags for nothing. The match is
#' computed narrow and joined back on DocID.
#'
#' CACHED, AND THE CACHE IS THE NARROW TABLE. Six columns per document rather than forty-five, so it
#' is a few tens of megabytes rather than most of a gigabyte, and it is what the expensive step
#' actually produces. The consolidated metadata is read either way, because the ladder below needs
#' every column of it.
#'
#' @param .path_meta Path to 01C's consolidated metadata.
#' @param .tab_range Output of 02A's cmp_quarter_range().
#' @param .stamp Character. Fingerprint of what determines the match, from utils_dir_stamp().
#' @param .path_cache Parquet holding the match and the fingerprint it was built under.
#' @param .rerun Logical. TRUE rematches regardless of the fingerprint.
#' @return A tibble, one row per document, with the Compustat columns and nQuarters.
reg_merge_compustat <- function(.path_meta, .tab_range, .stamp, .path_cache, .rerun = FALSE) {
  if (FALSE) {
    .path_meta  <- .lP$Input$MetaData
    .tab_range  <- tab_range
    .stamp      <- stamp_merge
    .path_cache <- .lP$Cache$QuarterMatch
    .rerun      <- FALSE
  }

  cols_ <- c("gvkey", "datadate", "fyear", "fqtr")
  meta_ <- dplyr::collect(arrow::open_dataset(sources = .path_meta))

  fresh_ <- !.rerun && identical(utils_stamp_read(.path = .path_cache), .stamp)

  if (fresh_) {
    link_ <- dplyr::select(arrow::read_parquet(file = .path_cache), -"Stamp")
    cli::cli_alert_info(
      "Corpus and matching frame unchanged: the quarter match was read from cache."
    )
    return(dplyr::left_join(meta_, link_, by = dplyr::join_by("DocID")))
  }

  if (!.rerun && fs::file_exists(.path_cache)) {
    cli::cli_alert_warning("The corpus or the matching frame has moved; the quarter match is being rebuilt.")
  }

  link_ <- meta_ |>
    dplyr::select("DocID", "CIK", "DateFiled") |>
    dplyr::left_join(
      y  = dplyr::select(.tab_range, "CIK", dplyr::all_of(cols_), "DateStart", "DateStop"),
      by = dplyr::join_by("CIK", "DateFiled" >= "DateStart", "DateFiled" < "DateStop")
    ) |>
    dplyr::mutate(nQuarters = dplyr::n(), .by = "DocID") |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(cols_),
        .fns  = \(.x) dplyr::if_else(.data$nQuarters > 1L, NA, .x)
      )
    ) |>
    # Selected before the deduplication rather than after it. Once the Compustat fields of an
    # ambiguous document are missing, its several rows differ only in the window bounds, and
    # dropping those first makes the surviving row identical whichever one distinct() keeps.
    dplyr::select("DocID", dplyr::all_of(cols_), "nQuarters") |>
    dplyr::distinct() |>
    dplyr::arrange(.data$DocID)

  arrow::write_parquet(dplyr::mutate(link_, Stamp = .stamp), .path_cache)
  cli::cli_alert_success(
    "Quarter match rebuilt: {format(nrow(link_), big.mark = ',')} documents, cached."
  )

  dplyr::left_join(meta_, link_, by = dplyr::join_by("DocID"))
}


# 2. The sample flags ----------------------------------------------------------------------------------------------------

#' Derive the sample flags from the ladder step
#'
#' The ladder itself is written in the document, because the thresholds and their order are the
#' argument. This is the mechanical part: the step number read from its own label, and the two sample
#' indicators derived from it.
#'
#' THE DESCRIPTIVE SAMPLE DOES NOT REQUIRE A COMPUSTAT MATCH. It is every well-formatted document
#' filed inside the window, which is what the corpus can describe. The estimation sample is the subset
#' that also matched, which is what a regression can use. Reporting only the second would understate
#' the corpus by whatever share of filers Compustat does not cover.
#'
#' @param .tab A merged table carrying SampleStepDesc.
#' @param .step_final Integer. The ladder step denoting the final sample.
#' @return The same table with SampleStepCode, DescSample and EstiSample added.
reg_sample_flags <- function(.tab, .step_final = 6L) {
  if (FALSE) {
    .tab        <- tab_merged
    .step_final <- 6L
  }

  .tab |>
    dplyr::mutate(
      SampleStepCode = as.integer(stringi::stri_extract_first_regex(.data$SampleStepDesc, "\\d+")),
      DescSample     = as.integer(.data$SampleStepCode >= 3L),
      EstiSample     = as.integer(.data$SampleStepCode == .step_final)
    )
}


# 3. Attaching what the other scripts found ------------------------------------------------------------------------------

#' Attach the Item 1.01 extraction outcome from 01D
#'
#' A FLAG, NOT THE TEXT. Roughly a quarter of a million summaries is on the order of a gigabyte, and
#' putting that in the register would make the one table nobody can load. The register says whether a
#' summary exists; Item101.parquet holds it, one join away on DocID.
#'
#' HasSummary IS ABOUT THIS PIPELINE, NOT ABOUT THE FILING. It says 01D recovered an Item 1.01
#' narrative from THIS DOCUMENT, which is a property of our extractor. Whether the parent filing
#' REPORTS Item 1.01 is a different fact living in 01D's FilingItems tables, and it used to share
#' this column's name -- HasItem101 -- which meant one name carried two meanings that disagree
#' wherever both are defined: a filing can report the item while the extraction fails.
#'
#' It is zero rather than missing for a document that was never a candidate. A candidate is an 8-K
#' whose filing reports the item, so a contract having no summary is not an absence of information --
#' it is the wrong kind of document to have one.
#'
#' THE FILING'S ITEM LIST IS NO LONGER ATTACHED HERE. It was, as a pipe-joined string, and it was
#' missing on every row that is not an Item 1.01 candidate -- which is every contract. A filing-level
#' fact reaching documents through a document-level join is empty exactly where it would be useful,
#' and 10 now joins 01D's per-filing table on HashIndex instead, where it lands on every row.
#'
#' @param .tab The register under construction.
#' @param .path_item Path to 01D's Item101.parquet.
#' @return .tab with HasSummary and Item101Outcome added.
reg_attach_item101 <- function(.tab, .path_item) {
  if (FALSE) {
    .tab       <- tab_merged
    .path_item <- .lP$Input$Item101
  }

  itm_ <- arrow::open_dataset(sources = .path_item) |>
    dplyr::select("DocID", Item101Outcome = "Outcome") |>
    dplyr::collect()

  ok_ <- c("extracted", "ambiguous-longest")

  .tab |>
    dplyr::left_join(itm_, by = dplyr::join_by("DocID")) |>
    dplyr::mutate(
      HasSummary = dplyr::case_when(
        is.na(.data$Item101Outcome)   ~ 0L,
        .data$Item101Outcome %in% ok_ ~ 1L,
        .default                      = 0L
      )
    )
}

#' Derive the columns that are properties of other columns
#'
#' DocExt is the file extension of the document's URL, and cyear the calendar year of the Compustat
#' observation. Neither is stored upstream because neither is a measurement: both are a restatement
#' of something already present, and the place to restate it is once, here, rather than in whichever
#' downstream script happens to need it first.
#'
#' The extension is read with stringi rather than tools::file_ext(), which builds a new string per
#' element and allocates again through ifelse(), passing three times over a million URLs.
#'
#' @param .tab The register under construction.
#' @return .tab with DocExt and cyear added.
reg_derive <- function(.tab) {
  if (FALSE) .tab <- tab_merged

  .tab |>
    dplyr::mutate(
      DocExt = dplyr::coalesce(
        stringi::stri_extract_last_regex(.data$UrlDocument, "(?<=\\.)[[:alnum:]]+$"), ""
      ),
      cyear = lubridate::year(.data$datadate)
    )
}


# 4. Shaping the register ------------------------------------------------------------------------------------------------

#' Put the register in its final shape
#'
#' SORTED BY GROUP, THEN DOCUMENT. Parquet keeps minimum and maximum values per row group, so a
#' reader filtering on group skips the row groups that cannot contain a match rather than reading and
#' discarding them. Sorting is what makes one file as cheap to slice as three separate ones.
#'
#' THE MACHINE PATH IS DROPPED, THE PUBLIC URL IS NOT. DocType and YQ determine the path together
#' with DocID, and utils_doc_path() rebuilds it; an absolute path is correct on one machine and wrong
#' on every other, which is what stops an archived table being usable. UrlDocument is a different
#' thing entirely -- the document's address on EDGAR, identical everywhere and the only way a reader
#' of the archive can fetch the original -- so it stays.
#'
#' @param .tab The register under construction.
#' @return The register, ordered and with its columns in a stated order.
reg_shape <- function(.tab) {
  if (FALSE) .tab <- tab_merged

  .tab |>
    dplyr::select(-dplyr::any_of(c("DocPath", "Path", "nQuarters"))) |>
    dplyr::relocate(dplyr::any_of(c(
      # identity
      "DocID", "HashDocument", "HashIndex", "CIK", "Group",
      # where the document is, without saying where this machine keeps it
      "DocType", "YQ",
      # what it is
      "DocTypeRaw", "DocTypeMod", "FormType", "DocExt", "DateFiled", "UrlDocument",
      # what it contains
      "nWords", "nWordsAdj", "nChars", "nNums", "pStopShort",
      # whether it is usable
      "Removed", "RemClass",
      # whether it is a repeat
      "nCIK", "MultFiler", "FilerCopiesAgree", "PrimaryFiler",
      # who filed it
      "gvkey", "datadate", "cyear", "fyear", "fqtr",
      # which sample it is in
      "SampleStepCode", "SampleStepDesc", "DescSample", "EstiSample",
      # whether this document's own Item 1.01 summary was recovered
      "HasSummary", "Item101Outcome"
    ))) |>
    dplyr::arrange(.data$Group, .data$DocID)
}


# 4b. Deployment ---------------------------------------------------------------------------------------------------------

#' Write the register, if what produced it has moved
#'
#' THE REASON IS NOT DISK TIME. Five documents downstream -- 03A, 03F, 04A, 04C and any analysis --
#' read this file, and a fingerprint downstream is only as stable as the modification time of the
#' file it points at. Rewriting it unconditionally moves that timestamp on every render of this
#' document, so every cache keyed on it misses although the bytes are identical. 01C produced
#' exactly that failure for five documents before it was guarded.
#'
#' THE REGISTER IS BUILT EITHER WAY. Only the write is guarded, so every check below runs on the
#' object in memory and a skipped write leaves nothing unverified.
#'
#' @param .tab The register.
#' @param .path_out Destination parquet path.
#' @param .stamp Character. Fingerprint of what determines it, from utils_dir_stamp().
#' @param .path_stamp Parquet under Cache/ holding the fingerprint it was last written under.
#' @param .rerun Logical. TRUE writes regardless of the fingerprint.
#' @return .tab, invisibly.
reg_write_register <- function(.tab, .path_out, .stamp, .path_stamp, .rerun = FALSE) {
  if (FALSE) {
    .tab        <- tab_register
    .path_out   <- .lP$Output$Documents
    .stamp      <- stamp_register
    .path_stamp <- .lP$Cache$OutputStamp
    .rerun      <- FALSE
  }

  fresh_ <- !.rerun &&
    fs::file_exists(.path_out) &&
    identical(utils_stamp_read(.path = .path_stamp), .stamp)

  if (fresh_) {
    cli::cli_alert_info("Inputs unchanged: {fs::path_file(.path_out)} was left as it stands.")
    return(invisible(.tab))
  }

  arrow::write_parquet(.tab, .path_out)
  arrow::write_parquet(tibble::tibble(Stamp = .stamp), .path_stamp)
  cli::cli_alert_success(
    "Written: {format(nrow(.tab), big.mark = ',')} rows, {ncol(.tab)} columns."
  )

  invisible(.tab)
}


# 5. Reports -------------------------------------------------------------------------------------------------------------

#' The sample selection table, as it appears in the paper
#'
#' One row per ladder step, showing what each step removes. Steps that remove documents are shown
#' negative, because the table is read as a subtraction from the universe down to the final sample.
#'
#' FIRM COUNTS ARE SHOWN ONLY ON THE FINAL ROW. A firm count on an intermediate step would be the
#' number of firms among the documents removed at that step, which is not the number of firms lost: a
#' firm with a hundred documents loses one and remains in the sample. Leaving those cells empty is
#' more honest than filling them with a number that invites the wrong reading, and that includes the
#' descriptive-sample subtotal, where summing empty cells would give zero and zero firms is a claim.
#'
#' EVERY STEP APPEARS, INCLUDING THE ONES THAT REMOVED NOTHING. A step absent because it caught no
#' document reads as an omission rather than as a zero, and the three groups would then have tables of
#' different heights that cannot be set side by side.
#'
#' @param .tab The register.
#' @param .group Character. Which group to report, or NULL for all of them pooled.
#' @return A tibble, one row per step plus a descriptive-sample subtotal.
reg_sample_table <- function(.tab, .group = NULL) {
  if (FALSE) {
    .tab   <- tab_register
    .group <- "Exhibit10"
  }

  dat_ <- if (is.null(.group)) .tab else dplyr::filter(.tab, .data$Group == .group)

  steps_ <- sort(unique(.tab$SampleStepDesc))
  less_  <- steps_[grepl("Less", steps_)]

  tmp_ <- dplyr::bind_rows(dplyr::mutate(dat_, SampleStepDesc = "00-SEC EDGAR Universe"), dat_) |>
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
      .fns  = \(.x) dplyr::if_else(grepl("Final", .data$SampleStepDesc), .x, NA_integer_)
    )) |>
    dplyr::mutate(dplyr::across(
      .cols = -"SampleStepDesc",
      .fns  = \(.x) dplyr::if_else(grepl("Less", .data$SampleStepDesc), -.x, .x)
    )) |>
    dplyr::bind_rows(tibble::tibble(SampleStepDesc = less_, nFilesAll = 0L, nFilesUni = 0L)) |>
    dplyr::distinct(.data$SampleStepDesc, .keep_all = TRUE) |>
    dplyr::arrange(.data$SampleStepDesc)

  sub_ <- tmp_ |>
    dplyr::filter(grepl("^0[012]", .data$SampleStepDesc)) |>
    dplyr::summarise(dplyr::across(c("nFilesAll", "nFilesUni"), \(.x) sum(.x, na.rm = TRUE))) |>
    dplyr::mutate(
      SampleStepDesc = "Descriptive Sample",
      nFirmQtrs = NA_integer_, nFirmYears = NA_integer_, nFirms = NA_integer_
    )

  dplyr::bind_rows(
    dplyr::filter(tmp_, grepl("^0[012]", .data$SampleStepDesc)),
    sub_,
    dplyr::filter(tmp_, grepl("^0[3-9]", .data$SampleStepDesc))
  ) |>
    dplyr::mutate(SampleStepDesc = gsub("^\\d+-", "", .data$SampleStepDesc)) |>
    dplyr::relocate("SampleStepDesc")
}

#' What the register contains, by group
#'
#' @param .tab The register.
#' @return A tibble, one row per group.
reg_group_summary <- function(.tab) {
  if (FALSE) .tab <- tab_register

  .tab |>
    dplyr::summarise(
      nDocs        = dplyr::n(),
      nAttachments = dplyr::n_distinct(.data$HashDocument),
      nFilings     = dplyr::n_distinct(.data$HashIndex),
      nRemoved     = sum(.data$Removed),
      nDesc        = sum(.data$DescSample),
      nEsti        = sum(.data$EstiSample),
      nSummary     = sum(.data$HasSummary),
      .by          = "Group"
    ) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' Every report in this document, in order
#'
#' TAKES THE SUMMARIES, DOES NOT COMPUTE THEM. This block is shown twice, in Results and again in the
#' Overview, and the sample table is derived once per group -- so computed in place it was eight
#' traversals of the register producing answers that must agree by construction.
#'
#' @param .tab_grp Output of reg_group_summary().
#' @param .tab_samples Named list of reg_sample_table() results, one per group.
#' @return Invisibly NULL.
reg_report_all <- function(.tab_grp, .tab_samples) {
  if (FALSE) {
    .tab_grp     <- tab_group_summary
    .tab_samples <- lst_sample_tables
  }

  tbl_head("What the register contains")
  tbl_out(
    .tab   = .tab_grp,
    .title = NULL,
    .notes = c(
      nAttachments = "Distinct attachments; a document fetched under several registrants counts once.",
      nSummary     = "Only 8-K documents are candidates, so zero elsewhere is the right kind of zero."
    )
  )

  purrr::iwalk(
    .x = .tab_samples,
    .f = function(.tab, .g) {
      tbl_head("Sample selection: {(.g)}")
      tbl_out(
        .tab   = .tab,
        .title = NULL,
        .notes = c(
          nFilesUni = "Distinct attachments; documents fetched under several registrants count once.",
          nFirms    = "Shown only on the final row: a firm losing one document of many is not lost."
        )
      )
    }
  )

  invisible(NULL)
}


# 6. Figures -------------------------------------------------------------------------------------------------------------
# DEFINED HERE, WRITTEN NOWHERE. The document displays what this returns and the consolidated release
# script writes the file the manuscript needs -- as it does the per-group sample-selection tables,
# which reg_sample_table() produces and this script no longer writes to disk.

#' Documents per year, by what they are in
#'
#' @param .tab The register, restricted to one group.
#' @return A ggplot object.
reg_plot_samples <- function(.tab) {
  if (FALSE) .tab <- dplyr::filter(tab_register, .data$Group == "Exhibit10")

  dat_ <- .tab |>
    dplyr::mutate(
      Year   = as.integer(format(.data$DateFiled, "%Y")),
      Status = dplyr::case_when(
        .data$EstiSample == 1L ~ "Estimation sample",
        .data$DescSample == 1L ~ "Descriptive only",
        .default               = "Excluded"
      )
    ) |>
    dplyr::summarise(nDocs = dplyr::n(), .by = c("Year", "Status")) |>
    dplyr::filter(.data$Year >= 1996L)

  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$nDocs, fill = .data$Status)) +
    ggplot2::geom_col() +
    plot_scale_fill_cat() +
    plot_scale_y_count() +
    ggplot2::labs(x = NULL, y = "Documents", fill = NULL) +
    plot_theme(.grid = "y")
}
