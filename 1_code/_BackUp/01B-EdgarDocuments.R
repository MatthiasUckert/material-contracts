# 01B-EdgarDocuments: fetch the documents themselves, and index them on disk -------------------------------------------
#
# WHAT THIS FILE DOES
# 01A mirrored the landing pages, which say what each filing contains. 01B decides which of those
# attachments are worth retrieving, fetches and parses them, and writes FilePaths.parquet -- the
# DocID to Path index every later script uses to locate a document's text on disk.
#
# WHAT IS NOT HERE
# Nothing is measured about the documents. Word counts, stopword ratios, parse-error rates and the
# quality rules that drop a document from the study are 01C. The division is by cost and by failure
# mode: this script is network-bound and must survive interruption; 01C is local, cheap, and re-runs
# from scratch in minutes.
#
# WHERE THE DOCUMENTS LAND
# rGetEDGAR resolves link tables and downloaded documents from a single root, so the documents are
# written into the mirror 01A established rather than into this script's own output directory.
# Duplicating three hundred megabytes of link tables to obtain a second root would buy nothing.
# Ownership stays unambiguous: 01A writes MasterIndex/ and DocLinks/, 01B writes DocumentData/, and
# no artifact has two writers. This script's own directory holds the index.
#
# THE PARSED TREE IS THE ONLY COPY
# Documents are fetched with .keep_orig = FALSE, which discards the original bytes once parsing
# succeeds. There is no local source to re-parse from, and re-deriving the tree means one request
# per document at ten per second. That is why acquisition is gated and why nothing here deletes.
#
# PARALLELISM
# Documents are fetched through rGetEDGAR, which parallelises internally with furrr. Parallel work
# written for this pipeline uses mirai; the two coexist because the download loop belongs to the
# package rather than to this script.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation.

if (FALSE) {
  .path_links   <- .lP$Input$Links
  .path_landing <- .lP$Input$LandingPageAll
  .dir_parsed   <- .lP$Edgar$DocumentData$Parsed
  .path_out     <- .lP$Output$FilePaths
  .mod          <- "Exhibit 10"
  .item         <- "1.01"
}


# 1. Vocabulary --------------------------------------------------------------------------------------------------------

#' Raw exhibit-type strings for one modal document type
#'
#' Filers do not write exhibit types consistently. "EX-10.1", "EX-10", "EXHIBIT 10.23" and dozens of
#' further spellings all denote the same thing, and matching them with a pattern is how a study ends
#' up silently excluding whichever spellings the pattern's author did not think of. The exclusion is
#' invisible, because the omitted documents never enter any count.
#'
#' rGetEDGAR ships a lookup enumerating the observed raw strings against a modal type, and this
#' reads it. The mapping is then versioned with the package rather than with this project, so a
#' spelling discovered later arrives as an update instead of as a permanent gap.
#'
#' @param .mod Character vector. Modal document types, e.g. "Exhibit 10", "CTO", c("8-K", "8-K/A").
#' @return Character vector of raw Type strings mapping to those modal types.
edg_doc_types <- function(.mod) {
  if (FALSE) .mod <- "Exhibit 10"

  tab_ <- rGetEDGAR::Table_DocTypesRaw
  out_ <- tab_$DocTypeRaw[tab_$DocTypeMod %in% .mod]

  if (length(out_) == 0L) cli::cli_abort("No raw types map to {(.mod)}.")
  out_
}

#' Link-table rows for a set of raw exhibit types, with the file extension resolved
#'
#' EXTENSIONLESS URLS ARE DROPPED. A minority of link rows carry a URL with no file extension. These
#' are not documents; they are directory stubs and malformed references that resolve to nothing, and
#' requesting one costs a round trip against the SEC's rate limit for a guaranteed failure. Dropping
#' them here rather than letting the download fail keeps the error count meaningful: what remains in
#' the failure statistics is documents that should have parsed and did not.
#'
#' FOUR COLUMNS ARE COLLECTED, NOT THIRTY MILLION ROWS OF EVERYTHING. The link tables hold tens of
#' millions of rows across a dozen columns, and this function needs the identifier, the filing, the
#' type and the URL. Collecting the rest costs memory proportional to the corpus for data nothing
#' reads, and the URL itself is dropped again as soon as it has been tested.
#'
#' THE EXTENSION IS TESTED, NOT EXTRACTED. tools::file_ext() builds a new string for every element
#' through substring() and then allocates again through ifelse(), so it passes three times over a
#' vector of millions of URLs to answer a yes-or-no question. stringi tests the pattern in one pass
#' and allocates nothing, and no caller needs the extension itself.
#'
#' @param .path_links Directory of mirrored document-link tables.
#' @param .types Character vector of raw Type strings; see edg_doc_types().
#' @param .hash_index Optional character vector of HashIndex values to restrict to. NULL keeps all.
#' @return A tibble: DocID, HashIndex, Type.
edg_links_with_ext <- function(.path_links, .types, .hash_index = NULL) {
  if (FALSE) {
    .path_links <- .lP$Input$Links
    .types      <- edg_doc_types(.mod = "Exhibit 10")
    .hash_index <- NULL
  }

  arr_ <- arrow::open_dataset(sources = .path_links) |>
    dplyr::filter(.data$Type %in% .types)

  if (!is.null(.hash_index)) {
    arr_ <- dplyr::filter(arr_, .data$HashIndex %in% .hash_index)
  }

  arr_ |>
    dplyr::select("DocID", "HashIndex", "Type", "UrlDocument") |>
    dplyr::collect() |>
    dplyr::filter(stringi::stri_detect_regex(.data$UrlDocument, "\\.[[:alnum:]]+$")) |>
    dplyr::select("DocID", "HashIndex", "Type")
}

#' Filings whose landing page reports a given 8-K item
#'
#' MATCHED AS A FIXED STRING, NOT A PATTERN. Items arrive as a newline-separated list of codes such
#' as "1.01\n2.03\n9.01". Written as a regular expression, "1.01" carries a wildcard in the middle
#' and would also match "1101" or "1a01". No 8-K item code has that shape, so the two agree on this
#' corpus, and the document checks that rather than asserting it. A match rule that is accidentally
#' loose is worth not carrying forward even where it happens to be harmless.
#'
#' THE MATCH RUNS IN ARROW, NOT IN R. The landing table holds twenty-one million rows, and pulling
#' its Items column into R to test a substring means materialising every one of them to keep the
#' small minority that are 8-K item lists. Arrow applies the test during the scan and returns only
#' the filings that pass, which is a few hundred thousand short strings rather than a column.
#'
#' @param .path_landing Path to the unfiltered landing table written by 01A.
#' @param .item Character. Item code, e.g. "1.01".
#' @param .fixed Logical. TRUE matches the code literally; FALSE treats it as a pattern, which is
#'   used only to quantify the difference between the two.
#' @return Character vector of HashIndex values.
edg_hash_with_item <- function(.path_landing, .item, .fixed = TRUE) {
  if (FALSE) {
    .path_landing <- .lP$Input$LandingPageAll
    .item         <- "1.01"
    .fixed        <- TRUE
  }

  fix_ <- isTRUE(.fixed)

  arrow::open_dataset(sources = .path_landing) |>
    dplyr::filter(grepl(.item, .data$Items, fixed = fix_)) |>
    dplyr::distinct(.data$HashIndex) |>
    dplyr::collect() |>
    dplyr::pull(.data$HashIndex)
}


# 2. Outstanding work --------------------------------------------------------------------------------------------------

#' Subtract what is already on disk from what was selected
#'
#' The whole idempotence of this stage. The parsed tree is the ledger: a DocID with a file under it
#' has been retrieved, and nothing else needs to be remembered. No separate manifest is kept, because
#' a manifest can disagree with the directory it describes and the directory is the thing that
#' matters.
#'
#' THE DISK LISTING IS PASSED IN, NOT TAKEN. Walking a tree of this size costs about a minute, and
#' the document needs the same answer three times: here, to build the download queue, and again in
#' the overview. Listing once and passing the result is the difference between one walk and three,
#' and it also guarantees the three agree, which re-listing does not.
#'
#' @param .tab_sel Tibble of selected link rows, carrying DocID and Group.
#' @param .ids_disk Character vector of DocID values present in the parsed tree.
#' @return A tibble: Group, nSelected, nRetrieved, nOutstanding, ShareRetrieved.
edg_outstanding <- function(.tab_sel, .ids_disk) {
  if (FALSE) {
    .tab_sel  <- tab_selected
    .ids_disk <- vec_on_disk
  }

  have_ <- .ids_disk

  .tab_sel |>
    dplyr::summarise(
      nSelected    = dplyr::n(),
      nRetrieved   = sum(.data$DocID %in% have_),
      nOutstanding = sum(!.data$DocID %in% have_),
      .by          = "Group"
    ) |>
    dplyr::mutate(ShareRetrieved = .data$nRetrieved / .data$nSelected)
}

#' Report selection against what is on disk
#'
#' @param .tab_sel Tibble of selected link rows, carrying DocID and Group.
#' @param .ids_disk Character vector of DocID values present in the parsed tree.
#' @param .acquire Logical. The acquisition switch, reported alongside the backlog so that an
#'   incomplete corpus and a disabled download are never read as the same thing.
#' @return The outstanding tibble, invisibly.
edg_report_selection <- function(.tab_sel, .ids_disk, .acquire) {
  if (FALSE) {
    .tab_sel  <- tab_selected
    .ids_disk <- vec_on_disk
    .acquire  <- FALSE
  }

  out_ <- edg_outstanding(.tab_sel = .tab_sel, .ids_disk = .ids_disk)

  tbl_head("Selected against retrieved")
  tbl_out(
    .tab    = out_,
    .title  = NULL,
    .pct    = "ShareRetrieved",
    .digits = 1L,
    .notes  = c(
      nOutstanding = "Selected but not on disk. A stable nonzero value is dead links, not an interrupted run."
    )
  )

  n_ <- sum(out_$nOutstanding)
  if (n_ == 0L) {
    tbl_note("Every selected document is on disk.")
  } else if (isTRUE(.acquire)) {
    tbl_note("{n_} document{?s} outstanding; acquisition is ON, so this render will fetch them.")
  } else {
    tbl_note(
      "{n_} document{?s} outstanding and acquisition is OFF, so none were fetched. Set \\
       .lP$Param$Acquire to TRUE to close the gap.",
      .type = "warn"
    )
  }

  invisible(out_)
}


# 3. The on-disk index -------------------------------------------------------------------------------------------------

#' Build the DocID to Path index
#'
#' Every later script needs to turn a DocID into a file path, and this is the one place that mapping
#' is produced. Two independently constructed indexes of a corpus this size can disagree and nothing
#' would report it, which is why the mapping has a single writer.
#'
#' REBUILT ONLY WHEN SOMETHING WAS FETCHED. Walking the tree is minutes of work and its result cannot
#' change unless a document was added. The rebuild is therefore keyed on the acquisition switch
#' rather than on the outstanding count, which is a proxy that reaches zero for two different
#' reasons -- nothing left to fetch, or nothing left that can be fetched -- and cannot distinguish
#' them. The switch states directly whether this render was permitted to add anything.
#'
#' @param .dir_parsed Root of the parsed-document tree.
#' @param .path_out Destination parquet path.
#' @param .rerun Logical. TRUE re-walks the tree; FALSE reuses the file if it exists.
#' @return A tibble: DocID, DocType, YQ, Path.
edg_index_documents <- function(.dir_parsed, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .dir_parsed <- .lP$Edgar$DocumentData$Parsed
    .path_out   <- .lP$Output$FilePaths
    .rerun      <- FALSE
  }

  utils_list_project_files(
    .dir_data = .dir_parsed,
    .path_out = .path_out,
    .rerun    = .rerun
  )
}

#' Composition of the retrieved corpus
#'
#' @param .tab_idx The DocID to Path index.
#' @return A tibble: DocType, nDocs, nYQ, Share.
edg_corpus_composition <- function(.tab_idx) {
  if (FALSE) .tab_idx <- arrow::read_parquet(.lP$Output$FilePaths)

  .tab_idx |>
    dplyr::summarise(
      nDocs = dplyr::n(),
      nYQ   = dplyr::n_distinct(.data$YQ),
      .by   = "DocType"
    ) |>
    dplyr::mutate(Share = .data$nDocs / sum(.data$nDocs)) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' Retrieved documents per year-quarter and type
#'
#' @param .tab_idx The DocID to Path index.
#' @return A tibble: YQ, DocType, nDocs.
edg_corpus_by_quarter <- function(.tab_idx) {
  if (FALSE) .tab_idx <- arrow::read_parquet(.lP$Output$FilePaths)

  .tab_idx |>
    dplyr::summarise(nDocs = dplyr::n(), .by = c("YQ", "DocType")) |>
    dplyr::arrange(.data$YQ, .data$DocType)
}

#' Report the retrieved corpus
#'
#' @param .tab_idx The DocID to Path index.
#' @return The composition tibble, invisibly.
edg_report_documents <- function(.tab_idx) {
  if (FALSE) .tab_idx <- arrow::read_parquet(.lP$Output$FilePaths)

  cmp_ <- edg_corpus_composition(.tab_idx = .tab_idx)

  tbl_head("Retrieved corpus")
  tbl_out(
    .tab    = cmp_,
    .title  = NULL,
    .pct    = "Share",
    .digits = 1L,
    .notes  = c(
      nYQ = "Quarter coverage differs by type because the forms carrying each began at different dates."
    )
  )

  invisible(cmp_)
}

#' Every report in this document, in order
#'
#' The block to copy out when the corpus needs checking without re-rendering.
#'
#' @param .lp The configuration list.
#' @param .tab_sel Tibble of selected link rows.
#' @param .tab_idx The DocID to Path index.
#' @param .ids_disk Character vector of DocID values present in the parsed tree.
#' @return Invisibly NULL.
edg_report_all_docs <- function(.lp, .tab_sel, .tab_idx, .ids_disk) {
  if (FALSE) {
    .lp       <- .lP
    .tab_sel  <- tab_selected
    .tab_idx  <- arrow::read_parquet(.lP$Output$FilePaths)
    .ids_disk <- vec_on_disk
  }

  edg_report_selection(
    .tab_sel  = .tab_sel,
    .ids_disk = .ids_disk,
    .acquire  = .lp$Param$Acquire
  )
  edg_report_documents(.tab_idx = .tab_idx)

  invisible(NULL)
}


# 4. Figures -----------------------------------------------------------------------------------------------------------

#' Retrieved documents per year, stacked by type
#'
#' @param .tab Output of edg_corpus_by_quarter().
#' @return A ggplot object.
edg_plot_corpus <- function(.tab) {
  if (FALSE) .tab <- edg_corpus_by_quarter(arrow::read_parquet(.lP$Output$FilePaths))

  dat_ <- .tab |>
    dplyr::mutate(Year = as.integer(stringi::stri_sub(.data$YQ, 1L, 4L))) |>
    dplyr::summarise(nDocs = sum(.data$nDocs), .by = c("Year", "DocType"))

  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$nDocs, fill = .data$DocType)) +
    ggplot2::geom_col() +
    plot_scale_fill_cat() +
    plot_scale_y_count() +
    ggplot2::labs(x = NULL, y = "Documents", fill = NULL) +
    plot_theme(.grid = "y")
}
