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
# THIS SCRIPT WRITES ONLY INSIDE ITS OWN OUTPUT DIRECTORY
# rGetEDGAR resolves link tables and downloaded documents from a single root: it reads
# <root>/DocLinks/ and writes <root>/DocumentData/. Handing it 01A's root would put a million
# documents inside 01A's directory, which is not this script's to write in.
#
# So 01B keeps its own root and stages the link tables into it first. The staged copy is a cache
# derived from 01A's output, refreshed when 01A's differs, and it exists because the package's API
# requires links and documents to share a parent -- not because two scripts need the same data.
# The alternative, writing into a directory another script owns, trades a regenerable duplicate for
# an ownership rule, which is the wrong way round: the duplicate can be rebuilt from one command
# and the rule cannot be reinstated once the pipeline is built around breaking it.
#
# THE PARSED TREE IS THE ONLY COPY
# Documents are fetched with .keep_orig = FALSE, which discards the original bytes once parsing
# succeeds. There is no local source to re-parse from, and re-deriving the tree means one request
# per document at ten per second. That is why acquisition is gated and why nothing here deletes.
#
# WHERE THINGS ARE WRITTEN
# Anything a later script reads goes in Output/. Anything that exists only to make a re-render cheap
# goes in Cache/. The mirror root is neither and is named on its own, because it holds both kinds of
# thing at once: DocLinks/ is a clone of 01A's that one command rebuilds, and DocumentData/Parsed/
# is the corpus. A heading meaning "safe to delete" over 1.46 million irreplaceable documents is a
# heading somebody will eventually act on.
#
# THE INDEX IS STAMPED AGAINST THE TREE
# edg_index_documents() takes .rerun, defaulting to FALSE, and fingerprints the parsed tree with
# utils_tree_stamp() before deciding whether to walk it. Walking 1.46 million files costs about a
# minute; the fingerprint costs about four hundred stat calls. That is what lets the same call serve
# twice -- once before the download to say what is on disk, once after to pick up what arrived --
# and cost nothing the second time when nothing arrived.
#
# This replaces keying the rebuild on the acquisition switch. The switch says whether this render was
# PERMITTED to add documents, not whether it did, so a permitted render that fetched nothing paid a
# full re-walk for no reason.
#
# FIGURES ARE DEFINED HERE AND WRITTEN NOWHERE. The document displays what edg_plot_corpus() returns;
# the consolidated release script writes the files the manuscript needs.
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
  .path_stamp   <- .lP$Cache$TreeStamp
  .mod          <- "Exhibit 10"
  .item         <- "1.01"
  .rerun        <- FALSE
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


# 1b. Staging ----------------------------------------------------------------------------------------------------------

#' Stage 01A's link tables into this script's own mirror root
#'
#' rGetEDGAR reads link tables and writes documents under one root, so downloading into this
#' script's directory requires the links to be there too. This clones them, and nothing else: the
#' master index is not staged because nothing here reads it.
#'
#' A CACHE, NOT A SECOND SOURCE. 01A remains the sole writer of the link tables. What sits here is a
#' copy refreshed whenever the original differs, and deleting it costs one re-run of this chunk.
#' Naming it a cache rather than an input is the honest description: no decision in this pipeline is
#' ever made from this copy that would not be made identically from 01A's.
#'
#' REFRESHED BY NAME AND SIZE, NOT BY TIMESTAMP. A clone shares data blocks with its source, so its
#' modification time says nothing useful about whether the contents still agree. Comparing the file
#' names and byte sizes answers the question directly and costs one directory listing of each side.
#'
#' On a copy-on-write filesystem the clone is instantaneous and consumes no additional space.
#' Elsewhere it is a real copy of a few gigabytes, made once.
#'
#' @param .dir_src Directory of 01A's mirrored link tables.
#' @param .dir_dst Directory of this script's staged copy.
#' @param .quiet Logical. Suppress per-file progress.
#' @return A one-row tibble: nSource, nStaged, nCopied, Bytes.
edg_stage_links <- function(.dir_src, .dir_dst, .quiet = FALSE) {
  if (FALSE) {
    .dir_src <- .lP$Input$Links
    .dir_dst <- .lP$Edgar$DocLinks$DirMain$Links
    .quiet   <- FALSE
  }

  fs::dir_create(.dir_dst)

  src_ <- fs::dir_info(.dir_src, type = "file") |>
    dplyr::transmute(Name = fs::path_file(.data$path), Path = .data$path, Size = as.numeric(.data$size))
  dst_ <- fs::dir_info(.dir_dst, type = "file") |>
    dplyr::transmute(Name = fs::path_file(.data$path), SizeDst = as.numeric(.data$size))

  todo_ <- src_ |>
    dplyr::left_join(dst_, by = dplyr::join_by("Name")) |>
    dplyr::filter(is.na(.data$SizeDst) | .data$SizeDst != .data$Size)

  if (nrow(todo_) > 0L) {
    if (!.quiet) cli::cli_alert_info("Staging {nrow(todo_)} link table{?s} into this script's mirror.")
    purrr::walk(
      .x = seq_len(nrow(todo_)),
      .f = function(.i) {
        dst_f_ <- fs::path(.dir_dst, todo_$Name[.i])
        if (fs::file_exists(dst_f_)) fs::file_delete(dst_f_)
        st_ <- system2("cp", c("-c", shQuote(todo_$Path[.i]), shQuote(dst_f_)))
        if (!identical(st_, 0L)) cli::cli_abort("Staging failed for {todo_$Name[.i]}.")
      }
    )
  } else if (!.quiet) {
    cli::cli_alert_info("Link tables already staged.")
  }

  tibble::tibble(
    nSource = nrow(src_),
    nStaged = length(fs::dir_ls(.dir_dst, type = "file")),
    nCopied = nrow(todo_),
    Bytes   = sum(src_$Size)
  )
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
#' TAKES THE OUTSTANDING TABLE, DOES NOT COMPUTE IT. The document shows this twice, once in Selection
#' and once in the Overview, and each computation tests membership of a million and a half
#' identifiers. Computing it once in the document and passing it here also guarantees the two
#' statements agree, which recomputation does not.
#'
#' @param .tab_out Output of edg_outstanding().
#' @param .acquire Logical. The acquisition switch, reported alongside the backlog so that an
#'   incomplete corpus and a disabled download are never read as the same thing.
#' @return .tab_out, invisibly.
edg_report_selection <- function(.tab_out, .acquire) {
  if (FALSE) {
    .tab_out <- tab_outstanding
    .acquire <- FALSE
  }

  tbl_head("Selected against retrieved")
  tbl_out(
    .tab    = .tab_out,
    .title  = NULL,
    .pct    = "ShareRetrieved",
    .digits = 1L,
    .notes  = c(
      nOutstanding = "Selected but not on disk. A stable nonzero value is dead links, not an interrupted run."
    )
  )

  n_ <- sum(.tab_out$nOutstanding)
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

  invisible(.tab_out)
}


# 3. The on-disk index -------------------------------------------------------------------------------------------------

#' Build the DocID to Path index
#'
#' Every later script needs to turn a DocID into a file path, and this is the one place that mapping
#' is produced. Two independently constructed indexes of a corpus this size can disagree and nothing
#' would report it, which is why the mapping has a single writer.
#'
#' THE INDEX IS THE TREE READ INTO A TABLE, NOT A SECOND RECORD OF IT. A manifest that is written
#' once and trusted thereafter can drift from the directory it describes, and the directory is the
#' thing that matters. utils_tree_stamp() closes that gap: the index is rebuilt whenever the number
#' of DocType/YQ directories or any of their modification times has moved, which is whenever a
#' document was added or removed. Walking 1.46 million files costs about a minute and the fingerprint
#' costs about four hundred stat calls, so keeping the two in agreement is three orders of magnitude
#' cheaper than the walk it replaces.
#'
#' WHICH IS WHY THIS IS CALLED TWICE. The document needs to know what is on disk before the download,
#' to build the queue, and again afterwards, to pick up whatever arrived. The identical call serves
#' both: the fingerprint has moved exactly when the second answer differs from the first, and costs
#' nothing when it has not.
#'
#' THIS REPLACES KEYING THE REBUILD ON THE ACQUISITION SWITCH. The switch states whether a render was
#' permitted to add documents, not whether it did, so a permitted render that fetched nothing -- the
#' common case once the corpus is complete -- paid a full re-walk to produce the file it already had.
#'
#' @param .dir_parsed Root of the parsed-document tree.
#' @param .path_out Destination parquet path; the published artifact.
#' @param .path_stamp Parquet under Cache/ holding the fingerprint .path_out was last built under.
#' @param .rerun Logical. TRUE re-walks the tree regardless of the fingerprint. Use it where a
#'   document may have been rewritten in place, which the fingerprint cannot see.
#' @return A tibble: DocID, DocType, YQ, Path.
edg_index_documents <- function(.dir_parsed, .path_out, .path_stamp, .rerun = FALSE) {
  if (FALSE) {
    .dir_parsed <- .lP$Edgar$DocumentData$Parsed
    .path_out   <- .lP$Output$FilePaths
    .path_stamp <- .lP$Cache$TreeStamp
    .rerun      <- FALSE
  }

  stamp_ <- utils_tree_stamp(.dirs = .dir_parsed, .depth = 2L)

  fresh_ <- !.rerun &&
    fs::file_exists(.path_out) &&
    identical(utils_stamp_read(.path = .path_stamp), stamp_)

  if (fresh_) {
    out_  <- arrow::read_parquet(file = .path_out)
    n_    <- nrow(out_)
    # THE ORDER MATTERS. cli takes its plural quantity from the LAST interpolation before the {?}
    # marker, and format() hands it a length-one string, which reads as one. qty() states the
    # number without printing anything, so it has to sit after the formatted count, not before it.
    cli::cli_alert_info(
      "Tree unchanged: {format(n_, big.mark = ',')} {cli::qty(n_)}document{?s}, index read from cache."
    )
    return(out_)
  }

  if (!.rerun && fs::file_exists(.path_stamp)) {
    cli::cli_alert_warning("The parsed tree has moved; the index is being rebuilt.")
  }

  # .rerun = TRUE unconditionally: the fingerprint above has already made the decision, and letting
  # the helper make it again on its own weaker test would reuse a file this function just judged
  # stale.
  out_ <- utils_list_project_files(
    .dir_data = .dir_parsed,
    .path_out = .path_out,
    .rerun    = TRUE
  )

  arrow::write_parquet(tibble::tibble(Stamp = stamp_), .path_stamp)
  n_ <- nrow(out_)
  cli::cli_alert_success("Index rebuilt: {format(n_, big.mark = ',')} {cli::qty(n_)}document{?s}.")

  out_
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
#' TAKES THE COMPOSITION TABLE, DOES NOT COMPUTE IT. Shown once in Results and again in the Overview,
#' and the group-by runs over the whole index each time.
#'
#' @param .tab_cmp Output of edg_corpus_composition().
#' @return .tab_cmp, invisibly.
edg_report_documents <- function(.tab_cmp) {
  if (FALSE) .tab_cmp <- tab_composition

  tbl_head("Retrieved corpus")
  tbl_out(
    .tab    = .tab_cmp,
    .title  = NULL,
    .pct    = "Share",
    .digits = 1L,
    .notes  = c(
      nYQ = "Quarter coverage differs by type because the forms carrying each began at different dates."
    )
  )

  invisible(.tab_cmp)
}

#' Every report in this document, in order
#'
#' The block to copy out when the corpus needs checking without re-rendering.
#'
#' @param .tab_out Output of edg_outstanding().
#' @param .tab_cmp Output of edg_corpus_composition().
#' @param .acquire Logical. The acquisition switch.
#' @return Invisibly NULL.
edg_report_all_docs <- function(.tab_out, .tab_cmp, .acquire) {
  if (FALSE) {
    .tab_out <- tab_outstanding
    .tab_cmp <- tab_composition
    .acquire <- FALSE
  }

  edg_report_selection(.tab_out = .tab_out, .acquire = .acquire)
  edg_report_documents(.tab_cmp = .tab_cmp)

  invisible(NULL)
}


# 4. Figures -----------------------------------------------------------------------------------------------------------
# DEFINED HERE, WRITTEN NOWHERE. The document displays what this returns and the consolidated release
# script writes the files the manuscript needs.

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
