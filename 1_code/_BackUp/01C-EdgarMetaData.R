# 01C-EdgarMetaData: measure the documents, apply the quality rules, consolidate one table -----------------------------
#
# WHAT THIS FILE DOES
# 01B put 1.8 million parsed documents on disk and indexed them. 01C is the first script that opens
# them. It measures each one, decides which are too damaged to analyse, restricts the upstream
# metadata to the filings that survived, and joins everything into FullMetaData.parquet -- the one
# table 02, 03 and 04 read.
#
# ENTIRELY LOCAL, ENTIRELY RE-RUNNABLE
# Nothing here touches the network, so nothing here is gated. Every step inspects its own output and
# does only what is missing. This is the document in the family where "everything runs" holds
# without qualification, and it is also where every number a referee will ask about is produced.
#
# FLAGGED, NOT DELETED
# Documents failing the quality rules are marked with Removed = 1 and kept in the table. A pipeline
# that drops them cannot report its own attrition, and the count of what was excluded and why is a
# result rather than an implementation detail.
#
# MEASUREMENT IS THE EXPENSIVE PART
# Three passes over the corpus, each reading every document once: parse errors, text statistics, and
# stopword counts. Each writes a cache and skips if it is present. First run is hours; every later
# one is seconds.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation.

if (FALSE) {
  .tab_idx    <- tab_index
  .path_doc   <- tab_index$Path[1]
  .path_stop  <- .lP$Cache$StopWords
  .path_out   <- .lP$Output$FullMetaData
  .workers    <- 10L
}


# 1. Per-document measurement ------------------------------------------------------------------------------------------

#' Parse status of one document
#'
#' rGetEDGAR records a parse failure inside the document's own parquet file rather than by omitting
#' it, so a failure is a readable row rather than an absence. That is what makes the failure rate
#' computable at all: a document that was never written cannot be distinguished from one that was
#' never selected.
#'
#' @param .path_doc Path to one parsed document.
#' @return A one-row tibble: DocID, DocExt, ErrParse, MsgParse.
edg_doc_error <- function(.path_doc) {
  if (FALSE) .path_doc <- tab_index$Path[1]

  arrow::read_parquet(
    file     = .path_doc,
    col_select = dplyr::all_of(c("DocID", "DocExt", "ErrParse", "MsgParse")),
    mmap     = FALSE
  )
}

#' Text and format statistics for one document
#'
#' The measurements that the quality rules are applied to, plus the SEC header fields that identify
#' what the filer said the attachment was.
#'
#' WORD COUNTS INCLUDE THE HEADER. Every parsed document begins with the SEC's own <TYPE>,
#' <SEQUENCE>, <FILENAME> and <DESCRIPTION> block, which contributes words that are not contract
#' text. A near-empty attachment therefore does not measure as empty, and a rule keyed on the raw
#' count would keep it. The header fields are extracted here so that their word count can be
#' subtracted downstream.
#'
#' @param .path_doc Path to one parsed document.
#' @return A one-row tibble with counts, format flags and the five header fields.
edg_doc_stats <- function(.path_doc) {
  if (FALSE) .path_doc <- tab_index$Path[1]

  tab_ <- arrow::read_parquet(
    file       = .path_doc,
    col_select = dplyr::all_of(c("DocID", "HTML", "TextRaw"))
  ) |>
    dplyr::mutate(HTML = stringi::stri_replace_all_regex(.data$HTML, "([[:blank:]]|[[:space:]])+", " "))

  cnt_ <- tab_ |>
    dplyr::summarise(
      nWords = stringi::stri_count_words(.data$TextRaw),
      nChars = nchar(.data$TextRaw),
      nNums  = stringi::stri_count_regex(.data$TextRaw, "\\b\\d+\\b"),
      .by    = "DocID"
    )

  inf_ <- tab_ |>
    dplyr::mutate(HTML = toupper(.data$HTML)) |>
    dplyr::summarise(
      isHTML   = as.integer(grepl("<\\s?HEAD\\s?>", .data$HTML)),
      isIXBRL  = as.integer(stringi::stri_detect_regex(.data$HTML, "<\\s?IX\\s?:|US-GAAAP\\s?:")),
      nImgs    = stringi::stri_count_regex(.data$HTML, "<\\s?IMG"),
      FilTitle = trimws(stringi::stri_extract_first_regex(.data$HTML, "(?<=<\\s?TITLE\\s?>).+?(?=<)")),
      FilType  = trimws(stringi::stri_extract_first_regex(.data$HTML, "(?<=<\\s?TYPE\\s?>).+?(?=<)")),
      FilSeq   = trimws(stringi::stri_extract_first_regex(.data$HTML, "(?<=<\\s?SEQUENCE\\s?>).+?(?=<)")),
      FilName  = trimws(stringi::stri_extract_first_regex(.data$HTML, "(?<=<\\s?FILENAME\\s?>).+?(?=<)")),
      FilDesc  = trimws(stringi::stri_extract_first_regex(.data$HTML, "(?<=<\\s?DESCRIPTION\\s?>).+?(?=<)")),
      .by      = "DocID"
    ) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Fil"), \(.x) gsub("<.+?>", "", .x)))

  dplyr::left_join(cnt_, inf_, by = dplyr::join_by("DocID"))
}

#' Stopword counts for one document
#'
#' The share of a document's tokens that are common English function words. Ordinary prose runs high
#' on this measure; a document that runs low is usually not prose at all but a table, an exhibit
#' index, or the debris of a failed conversion. It is the one diagnostic that catches a document
#' which parsed without error and is still unusable.
#'
#' Both Loughran-McDonald lists are counted. The short list is the discriminating one; the long list
#' is retained because the two disagree on documents that are borderline, and that disagreement is
#' informative about the threshold.
#'
#' @param .path_doc Path to one parsed document.
#' @param .path_stop Path to the prepared stopword dictionary.
#' @return A one-row tibble: DocID, nToken, nStopShort, pStopShort, nStopLong, pStopLong.
edg_doc_stopwords <- function(.path_doc, .path_stop) {
  if (FALSE) {
    .path_doc  <- tab_index$Path[1]
    .path_stop <- .lP$Cache$StopWords
  }

  rex_ <- arrow::read_parquet(.path_stop) |>
    dplyr::mutate(RegEx = paste0("\\b", .data$StopWord, "\\b")) |>
    dplyr::summarise(RegEx = paste(.data$RegEx, collapse = "|"), .by = "Dictionary")
  lst_ <- split(rex_$RegEx, rex_$Dictionary)

  arrow::read_parquet(file = .path_doc, col_select = dplyr::all_of(c("DocID", "TextMod"))) |>
    dplyr::mutate(
      nToken     = stringi::stri_count_fixed(.data$TextMod, " ") + 1L,
      nStopShort = stringi::stri_count_regex(.data$TextMod, lst_[["L&M StopWords (Short)"]]),
      pStopShort = .data$nStopShort / .data$nToken,
      nStopLong  = stringi::stri_count_regex(.data$TextMod, lst_[["L&M StopWords (Long)"]]),
      pStopLong  = .data$nStopLong / .data$nToken,
      TextMod    = NULL
    )
}


# 2. Running a measurement over the corpus -----------------------------------------------------------------------------

#' Apply a per-document measurement across the corpus in parallel
#'
#' One function for all three passes, because they differ only in what they compute per file. Sharing
#' the traversal means the three cannot drift apart in how they chunk, how they handle a failure, or
#' which documents they cover.
#'
#' WORK IS SENT IN CHUNKS, NOT PER DOCUMENT. Dispatching one file per call would spend more time in
#' serialisation than in measurement across a corpus this size. Each worker receives a slice of the
#' path list and returns one bound table.
#'
#' SKIP IF DONE. The output file is the ledger. Present means finished, so a completed pass costs one
#' parquet read and the document can run it unconditionally.
#'
#' @param .tab_idx The DocID to Path index from 01B.
#' @param .path_out Destination parquet path for this pass.
#' @param .fun A function of one path, returning a one-row tibble.
#' @param .workers Integer. Parallel workers; also the number of chunks.
#' @param .label Character. Name of the pass, for progress reporting.
#' @return The measurement tibble.
edg_measure_corpus <- function(.tab_idx, .path_out, .fun, .workers = 10L, .label = "measure") {
  if (FALSE) {
    .tab_idx  <- tab_index
    .path_out <- .lP$Cache$DocErrors
    .fun      <- edg_doc_error
    .workers  <- 10L
    .label    <- "parse errors"
  }

  if (fs::file_exists(.path_out)) {
    cli::cli_alert_info("{(.label)}: already complete, reading from cache.")
    return(arrow::read_parquet(.path_out))
  }

  cli::cli_alert_info("{(.label)}: measuring {nrow(.tab_idx)} documents on {.workers} workers.")

  mirai::daemons(.workers)
  on.exit(mirai::daemons(0L), add = TRUE)

  paths_  <- unname(.tab_idx$Path)
  chunks_ <- split(paths_, cut(seq_along(paths_), .workers, labels = FALSE))

  out_ <- mirai::mirai_map(
    .x = chunks_,
    .f = function(.chunk, .f) dplyr::bind_rows(lapply(.chunk, .f)),
    .f = .fun
  )[.progress] |>
    dplyr::bind_rows()

  arrow::write_parquet(out_, .path_out)
  out_
}

#' Build the stopword dictionary
#'
#' The dictionary is standardised with the same function that standardised the document text, so a
#' word is spelled identically on both sides of the comparison. Standardising only one side is how a
#' stopword count comes out systematically low without anything appearing to be wrong.
#'
#' @param .path_short Path to the short Loughran-McDonald stopword list.
#' @param .path_long Path to the long Loughran-McDonald stopword list.
#' @param .path_out Destination parquet path.
#' @return A tibble: Dictionary, StopWord.
edg_stopword_dict <- function(.path_short, .path_long, .path_out) {
  if (FALSE) {
    .path_short <- .lP$Input$StopShort
    .path_long  <- .lP$Input$StopLong
    .path_out   <- .lP$Cache$StopWords
  }

  out_ <- dplyr::bind_rows(
    tibble::tibble(
      Dictionary = "L&M StopWords (Short)",
      StopWord   = unique(rGetEDGAR::standardize_text(readLines(.path_short, warn = FALSE)))
    ),
    tibble::tibble(
      Dictionary = "L&M StopWords (Long)",
      StopWord   = unique(rGetEDGAR::standardize_text(readLines(.path_long, warn = FALSE)))
    )
  )

  arrow::write_parquet(out_, .path_out)
  out_
}


# 3. Consolidating the statistics --------------------------------------------------------------------------------------

#' Join the three measurement passes into one document-statistics table
#'
#' THE ADJUSTED WORD COUNT IS THE POINT OF THIS FUNCTION. Every parsed document carries the SEC's own
#' header, and its words are counted along with the contract's. nWordsAdj subtracts them, and it is
#' what the emptiness rule is applied to: without the subtraction, an attachment containing nothing
#' but a header measures as a document with content.
#'
#' pNums is capped at one. The numerator counts numeric tokens with one pattern and the denominator
#' counts words with another, and on documents that are almost entirely digits the two disagree
#' enough to produce a share above unity. Capping is honest here because the quantity is only ever
#' used against a threshold well below the cap.
#'
#' @param .tab_stats Output of the text-statistics pass.
#' @param .tab_stop Output of the stopword pass.
#' @return A tibble of per-document statistics, percentages on a 0-100 scale.
edg_doc_statistics <- function(.tab_stats, .tab_stop) {
  if (FALSE) {
    .tab_stats <- tab_stats_raw
    .tab_stop  <- tab_stop_raw
  }

  .tab_stats |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Fil"), \(.x) dplyr::if_else(is.na(.x), "", .x)),
      FilText = paste(.data$FilTitle, .data$FilType, .data$FilSeq, .data$FilName, .data$FilDesc)
    ) |>
    dplyr::select("DocID", "nWords", "nChars", "nNums", "nImgs", "FilText") |>
    dplyr::left_join(
      y  = dplyr::select(.tab_stop, "DocID", "pStopLong", "pStopShort"),
      by = dplyr::join_by("DocID")
    ) |>
    dplyr::mutate(
      nWordsFile = stringi::stri_count_words(.data$FilText),
      nWordsAdj  = pmax(.data$nWords - .data$nWordsFile, 0L),
      pNums      = pmin(.data$nNums / .data$nWords, 1),
      dplyr::across(dplyr::starts_with("p"), \(.x) .x * 100),
      FilText    = NULL
    ) |>
    dplyr::relocate(c("pStopShort", "pStopLong"), .after = "pNums")
}


# 4. The quality rules -------------------------------------------------------------------------------------------------

#' Documents too damaged to analyse
#'
#' Three rules, each catching a different failure of conversion rather than a property of the
#' contract. They are deliberately blunt: the threshold in each case sits far from anything a real
#' agreement produces, so the rule removes wreckage without adjudicating content.
#'
#'   1. TOO SHORT. Ten words or fewer after the header is subtracted. An agreement cannot be stated
#'      in ten words; what measures this way is a stub, a link, or a conversion that produced almost
#'      nothing.
#'
#'   2. NOT PROSE. Under twenty per cent stopwords, among documents under a hundred words. Ordinary
#'      English runs far above that. The length condition matters: a long document with few stopwords
#'      is an exhibit schedule, which is legitimate content, whereas a short one is debris.
#'
#'   3. MOSTLY DIGITS. Three quarters or more of the tokens numeric. That is a financial table, not
#'      an agreement.
#'
#' RemClass records which rule fired first, in that order, so a document caught by two is attributed
#' to the more fundamental. The counts per rule are reported, because a rule that fires on almost
#' nothing and one that fires on a tenth of the corpus warrant different scrutiny.
#'
#' @param .tab_stats Output of edg_doc_statistics().
#' @param .tab_idx The DocID to Path index, for the document type.
#' @return A tibble of the documents to flag, one row each, with RemClass.
edg_flag_removals <- function(.tab_stats, .tab_idx) {
  if (FALSE) {
    .tab_stats <- tab_docstats
    .tab_idx   <- tab_index
  }

  .tab_stats |>
    dplyr::mutate(
      RemWords = as.integer(.data$nWordsAdj <= 10),
      RemStop  = as.integer(.data$pStopShort <= 20 & .data$nWordsAdj <= 100),
      RemNums  = as.integer(.data$pNums >= 75),
      RemDoc   = pmax(.data$RemWords, .data$RemStop, .data$RemNums)
    ) |>
    dplyr::filter(.data$RemDoc == 1L) |>
    dplyr::mutate(
      RemClass = dplyr::case_when(
        .data$RemWords == 1L ~ "1-Too short: 10 words or fewer after the header",
        .data$RemStop  == 1L ~ "2-Not prose: under 20 pct stopwords in a document under 100 words",
        .data$RemNums  == 1L ~ "3-Mostly digits: 75 pct or more of tokens numeric"
      )
    ) |>
    dplyr::left_join(
      y  = dplyr::select(.tab_idx, "DocID", "DocType"),
      by = dplyr::join_by("DocID")
    ) |>
    dplyr::mutate(DocType = gsub("A$", "", .data$DocType)) |>
    dplyr::relocate("DocType", .after = "DocID")
}


# 5. Reports -----------------------------------------------------------------------------------------------------------

#' Parse failures by file type and message
#'
#' @param .tab_err Output of the parse-error pass.
#' @return A tibble: DocExt, MsgParse, nDocs, Share.
edg_error_summary <- function(.tab_err) {
  if (FALSE) .tab_err <- tab_errors

  .tab_err |>
    dplyr::filter(.data$ErrParse == 1L) |>
    dplyr::summarise(nDocs = dplyr::n(), .by = c("DocExt", "MsgParse")) |>
    dplyr::mutate(Share = .data$nDocs / sum(.data$nDocs)) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' Report parse failures
#'
#' @param .tab_err Output of the parse-error pass.
#' @return The summary tibble, invisibly.
edg_report_errors <- function(.tab_err) {
  if (FALSE) .tab_err <- tab_errors

  sum_ <- edg_error_summary(.tab_err = .tab_err)
  n_   <- sum(.tab_err$ErrParse)

  tbl_head("Parse failures")
  tbl_out(.tab = sum_, .title = NULL, .pct = "Share", .digits = 1L, .n = 15L)
  tbl_note(
    "{n_} of {nrow(.tab_err)} documents failed to parse. PDF conversion is the bulk of it and \\
     is expected; a failure concentrated in one file type that previously converted is not."
  )

  invisible(sum_)
}

#' Removals by rule and document type
#'
#' @param .tab_rem Output of edg_flag_removals().
#' @param .tab_idx The DocID to Path index, for the denominator.
#' @return A tibble: RemClass, nDocs, Share.
edg_removal_summary <- function(.tab_rem, .tab_idx) {
  if (FALSE) {
    .tab_rem <- tab_removed
    .tab_idx <- tab_index
  }

  .tab_rem |>
    dplyr::summarise(nDocs = dplyr::n(), .by = "RemClass") |>
    dplyr::mutate(ShareOfCorpus = .data$nDocs / nrow(.tab_idx)) |>
    dplyr::arrange(.data$RemClass)
}

#' Report the quality rules
#'
#' @param .tab_rem Output of edg_flag_removals().
#' @param .tab_idx The DocID to Path index.
#' @return The removal summary, invisibly.
edg_report_removals <- function(.tab_rem, .tab_idx) {
  if (FALSE) {
    .tab_rem <- tab_removed
    .tab_idx <- tab_index
  }

  sum_ <- edg_removal_summary(.tab_rem = .tab_rem, .tab_idx = .tab_idx)

  tbl_head("Documents flagged for removal")
  tbl_out(
    .tab    = sum_,
    .title  = NULL,
    .pct    = "ShareOfCorpus",
    .digits = 1L,
    .notes  = c(RemClass = "Rules are applied in order; a document caught by two is attributed to the first.")
  )

  tbl_head("Flagged by document type")
  tbl_out(
    .tab = .tab_rem |>
      dplyr::summarise(nDocs = dplyr::n(), .by = c("DocType", "RemClass")) |>
      tidyr::pivot_wider(names_from = "RemClass", values_from = "nDocs", values_fill = 0L),
    .title = NULL
  )

  invisible(sum_)
}

#' Multi-filer agreement check
#'
#' The same agreement is filed by every party to it, so one document can appear under several CIKs.
#' Those copies should be byte-identical, and where they are not the joint filing is not in fact the
#' same document. This reports the share of jointly-filed documents whose copies agree on both size
#' and character count.
#'
#' @param .tab_meta The consolidated metadata table.
#' @return A one-row tibble: nJoint, ShareAgreeing.
edg_multifiler_check <- function(.tab_meta) {
  if (FALSE) .tab_meta <- tab_metadata

  .tab_meta |>
    dplyr::filter(.data$MultFiler == 1L) |>
    dplyr::summarise(
      nSizes = dplyr::n_distinct(.data$DocSize),
      nChars = dplyr::n_distinct(.data$nChars),
      .by    = "HashDocument"
    ) |>
    dplyr::summarise(
      nJoint        = dplyr::n(),
      ShareAgreeing = mean(pmin(.data$nSizes, .data$nChars) == 1L)
    )
}

#' Every report in this document, in order
#'
#' The block to copy out when the metadata needs checking without re-rendering.
#'
#' @param .tab_err Output of the parse-error pass.
#' @param .tab_rem Output of edg_flag_removals().
#' @param .tab_idx The DocID to Path index.
#' @param .tab_meta The consolidated metadata table.
#' @return Invisibly NULL.
edg_report_all_meta <- function(.tab_err, .tab_rem, .tab_idx, .tab_meta) {
  if (FALSE) {
    .tab_err  <- tab_errors
    .tab_rem  <- tab_removed
    .tab_idx  <- tab_index
    .tab_meta <- tab_metadata
  }

  edg_report_errors(.tab_err = .tab_err)
  edg_report_removals(.tab_rem = .tab_rem, .tab_idx = .tab_idx)

  tbl_head("Jointly filed documents")
  tbl_out(.tab = edg_multifiler_check(.tab_meta = .tab_meta), .pct = "ShareAgreeing", .digits = 1L)

  invisible(NULL)
}


# 6. Figures -----------------------------------------------------------------------------------------------------------

#' Flagged documents by rule
#'
#' @param .tab Output of edg_removal_summary().
#' @return A ggplot object.
edg_plot_removals <- function(.tab) {
  if (FALSE) .tab <- edg_removal_summary(tab_removed, tab_index)

  plot_bar_ranked(
    .tab   = .tab,
    .cat   = "RemClass",
    .val   = "nDocs",
    .label = TRUE
  )
}
