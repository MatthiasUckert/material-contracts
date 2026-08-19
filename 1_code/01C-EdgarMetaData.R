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


# 1. Vocabulary --------------------------------------------------------------------------------------------------------

#' Fold document types into the three groups the study is about
#'
#' The corpus holds four document types, but it answers three questions. Exhibit 10 attachments are
#' the contracts; 8-K and 8-K/A are one thing, the filer's own report of entering into an agreement,
#' and differ only in whether the report was amended; CT orders are the confidential-treatment
#' record.
#'
#' EVERY RATE IN THIS DOCUMENT IS REPORTED BY GROUP. A parse-failure or exclusion rate pooled over
#' all three describes none of them: Exhibit 10 is eighty-three per cent of the corpus, so a pooled
#' figure is the Exhibit 10 figure with the other two averaged into invisibility, and a problem
#' confined to CT orders would have to be forty times worse than one in contracts before it moved
#' the pooled number by the same amount.
#'
#' @param .doc_type Character vector of document types as the index records them.
#' @return Character vector: "Exhibit10", "8-K" or "CTO".
edg_doc_group <- function(.doc_type) {
  if (FALSE) .doc_type <- tab_index$DocType

  dplyr::case_when(
    grepl("^Exhibit10", .doc_type) ~ "Exhibit10",
    grepl("^CTO", .doc_type)       ~ "CTO",
    .default                       = "8-K"
  )
}


# 2. Per-document measurement ------------------------------------------------------------------------------------------

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


# 3. Running a measurement over the corpus -----------------------------------------------------------------------------

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
#' A DAEMON IS A FRESH R SESSION. It has the installed packages and nothing else: no sourced project
#' functions, no configuration list, none of the objects the calling session holds. Two consequences
#' shape this signature.
#'
#' First, .sources names the files each worker must source before it can do anything. A measurement
#' function that calls another function defined in the same library will not find it otherwise, and
#' the failure arrives per chunk rather than up front.
#'
#' Second, extra arguments are passed as .args rather than captured in a closure. Writing
#' \(.p) f(.p, .lP$Param$X) reads more naturally, but the closure's environment is the calling
#' session's global environment, which is not sent; in the worker .lP does not exist. A list of plain
#' values is data, and data crosses.
#'
#' @param .tab_idx A table with a Path column naming the documents to measure.
#' @param .path_out Destination parquet path for this pass.
#' @param .fun A function whose first argument is a path, returning a one-row tibble.
#' @param .args Named list of further arguments to .fun. Must be plain data.
#' @param .sources Character vector of R files each worker sources before measuring.
#' @param .workers Integer. Parallel workers; also the number of chunks.
#' @param .label Character. Name of the pass, for progress reporting.
#' @param .rows Character. "one" if the measurement returns exactly one row per document, "many" if
#'   it can return several. The check differs: one row in one row out, against every document
#'   appearing at least once. A pass that returns several rows checked as though it returned one
#'   would fail on correct output; a pass that returns one checked as though it returned many would
#'   pass on a truncated result.
#' @return The measurement tibble.
edg_measure_corpus <- function(.tab_idx, .path_out, .fun, .args = list(), .sources = character(0),
                               .workers = 24L, .label = "measure", .rows = c("one", "many")) {
  if (FALSE) {
    .tab_idx  <- tab_index
    .path_out <- .lP$Cache$DocErrors
    .fun      <- edg_doc_error
    .args     <- list()
    .sources  <- .path_fun
    .workers  <- 24L
    .label    <- "parse errors"
    .rows     <- "one"
  }

  rows_ <- match.arg(.rows)

  if (fs::file_exists(.path_out)) {
    cli::cli_alert_info("{(.label)}: already complete, reading from cache.")
    return(arrow::read_parquet(.path_out))
  }

  cli::cli_alert_info("{(.label)}: measuring {nrow(.tab_idx)} documents on {(.workers)} workers.")

  mirai::daemons(.workers)
  on.exit(mirai::daemons(0L), add = TRUE)

  if (length(.sources) > 0L) {
    mirai::everywhere(
      .expr = for (p in .paths) source(p, encoding = "UTF-8"),
      .args = list(.paths = as.character(.sources))
    )
  }

  paths_  <- unname(.tab_idx$Path)
  chunks_ <- split(paths_, cut(seq_along(paths_), .workers, labels = FALSE))

  # CONSTANTS GO IN .args, NOT IN THE DOTS. mirai_map vectorises over its dots the way pmap does, so
  # a function passed there is zipped alongside the chunks rather than held fixed, and the map
  # silently collapses to the length of the shortest input. .args is the constant channel.
  #
  # The worker's argument is also named .fn rather than .f, because mirai_map takes .f itself and a
  # second one would be matched to the same formal.
  out_ <- mirai::mirai_map(
    .x    = chunks_,
    .f    = function(.chunk, .fn, .fargs) {
      dplyr::bind_rows(lapply(.chunk, function(.p) do.call(.fn, c(list(.p), .fargs))))
    },
    .args = list(.fn = .fun, .fargs = .args)
  )[.progress] |>
    dplyr::bind_rows()

  # CHECKED BEFORE IT IS WRITTEN. One document in, one row out is the contract of every measurement
  # function here, and a result of another shape means the map did not do what was asked. Writing
  # first and checking later caches the wrong answer, and a cache is believed on every later run.
  n_seen_ <- if (identical(rows_, "one")) nrow(out_) else dplyr::n_distinct(out_$DocID)

  if (n_seen_ != nrow(.tab_idx)) {
    cli::cli_abort(
      "{(.label)}: expected {nrow(.tab_idx)} documents, accounted for {n_seen_}. Nothing was cached."
    )
  }

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


# 4. Consolidating the statistics --------------------------------------------------------------------------------------

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


# 5. The quality rules -------------------------------------------------------------------------------------------------

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


# 6. Repeated documents ------------------------------------------------------------------------------------------------

#' Flag documents fetched more than once because a filing named several registrants
#'
#' A filing can name many registrants -- a merger registration lists every guarantor subsidiary --
#' and its attachments are listed under each of them. Acquisition follows that listing, so one
#' attachment becomes one file per registrant. HashDocument identifies the attachment and DocID
#' identifies the registrant's copy of it, which makes the grouping exact rather than inferred: no
#' content comparison is needed to know that two DocIDs point at the same URL.
#'
#' THE COPIES ARE VERIFIED, NOT ASSUMED. Comparing the reported file size proves nothing, because
#' the size comes from the same landing page that produced the URL. What tests the files is what was
#' measured after parsing them: character, word and numeric-token counts. Where all three agree
#' across a group, the copies are identical for every purpose downstream.
#'
#' FILING DATE IS NOT A TIEBREAK HERE. Every copy of one attachment belongs to the same filing and
#' therefore carries the same date. What distinguishes them is the registrant, so the primary is the
#' lowest DocID -- which orders by CIK -- among copies that passed the quality rules. Preferring a
#' surviving copy matters: a failed conversion under one registrant would otherwise discard an
#' attachment that reads perfectly under another.
#'
#' FLAGGED, NOT DEDUPLICATED. Whether to analyse one copy or all of them is a downstream decision and
#' may differ by task: classification wants one, entity extraction may want each, because the
#' registrant-side metadata differs even where the text does not.
#'
#' @param .tab The consolidated metadata table, carrying HashDocument, DocID, Removed and the counts.
#' @return The same table with FilerCopiesAgree and PrimaryFiler added.
edg_flag_filer_copies <- function(.tab) {
  if (FALSE) .tab <- tab_metadata

  .tab |>
    dplyr::mutate(
      FilerCopiesAgree = as.integer(
        dplyr::n_distinct(.data$nChars) == 1L &
          dplyr::n_distinct(.data$nWords) == 1L &
          dplyr::n_distinct(.data$nNums) == 1L
      ),
      PrimaryFiler = as.integer(
        dplyr::row_number(dplyr::pick("Removed", "DocID")) == 1L
      ),
      .by = "HashDocument"
    )
}

#' How much of the corpus is one attachment fetched under several registrants
#'
#' The number that any count of documents has to be read against. A corpus statistic quoted per
#' document overstates the number of distinct attachments by whatever this reports, and the overstate
#' is not uniform: it concentrates in registration statements, which carry the most co-registrants.
#'
#' REPORTED BY GROUP. Registration statements carry the co-registrants, so the redundancy is
#' concentrated in the contracts and a pooled figure would attribute it to all three.
#'
#' @param .tab The consolidated metadata table.
#' @return A tibble, one row per group.
edg_filer_summary <- function(.tab) {
  if (FALSE) .tab <- tab_metadata

  .tab |>
    dplyr::mutate(Group = edg_doc_group(.doc_type = .data$DocTypeMod)) |>
    dplyr::summarise(
      nFilers = dplyr::n(),
      Agree   = min(.data$FilerCopiesAgree),
      .by     = c("Group", "HashDocument")
    ) |>
    dplyr::summarise(
      nDocuments    = sum(.data$nFilers),
      nAttachments  = dplyr::n(),
      nMultiGroups  = sum(.data$nFilers > 1L),
      nGroupsDiffer = sum(.data$Agree == 0L),
      .by           = "Group"
    ) |>
    dplyr::mutate(
      nRedundant     = .data$nDocuments - .data$nAttachments,
      ShareRedundant = .data$nRedundant / .data$nDocuments
    ) |>
    dplyr::select("Group", "nDocuments", "nAttachments", "nRedundant", "ShareRedundant",
                  "nMultiGroups", "nGroupsDiffer") |>
    dplyr::arrange(dplyr::desc(.data$nDocuments))
}

#' Attachments whose copies disagree
#'
#' The count alone is not actionable. A group that disagrees means two requests for one URL returned
#' different content, and which attachment it was, how far the copies diverge and under which
#' registrants they were filed are the things that decide whether it matters.
#'
#' @param .tab The consolidated metadata table.
#' @return A tibble, one row per disagreeing attachment.
edg_filer_disagreements <- function(.tab) {
  if (FALSE) .tab <- tab_metadata

  .tab |>
    dplyr::filter(.data$FilerCopiesAgree == 0L) |>
    dplyr::summarise(
      nFilers   = dplyr::n(),
      nDistinct = dplyr::n_distinct(.data$nChars),
      MinChars  = min(.data$nChars),
      MaxChars  = max(.data$nChars),
      .by       = "HashDocument"
    ) |>
    dplyr::mutate(SpreadChars = .data$MaxChars - .data$MinChars) |>
    dplyr::arrange(dplyr::desc(.data$SpreadChars))
}

#' Distribution of registrants per attachment
#'
#' @param .tab The consolidated metadata table.
#' @return A tibble: nFilers, nAttachments, nDocuments.
edg_filer_distribution <- function(.tab) {
  if (FALSE) .tab <- tab_metadata

  .tab |>
    dplyr::mutate(Group = edg_doc_group(.doc_type = .data$DocTypeMod)) |>
    dplyr::summarise(nFilers = dplyr::n(), .by = c("Group", "HashDocument")) |>
    dplyr::summarise(nAttachments = dplyr::n(), .by = c("Group", "nFilers")) |>
    dplyr::mutate(nDocuments = .data$nFilers * .data$nAttachments) |>
    dplyr::arrange(.data$Group, .data$nFilers)
}

#' Report repeated documents
#'
#' @param .tab The consolidated metadata table.
#' @return The summary tibble, invisibly.
edg_report_filer_copies <- function(.tab) {
  if (FALSE) .tab <- tab_metadata

  sum_ <- edg_filer_summary(.tab = .tab)

  tbl_head("Attachments fetched under several registrants")
  tbl_out(
    .tab    = sum_,
    .title  = NULL,
    .pct    = "ShareRedundant",
    .digits = 1L,
    .notes  = c(
      nAttachments  = "Distinct attachments; the denominator for any claim about corpus size.",
      nGroupsDiffer = "Groups whose copies disagree on length. Should be zero: one URL, one document."
    )
  )

  tbl_head("Registrants per attachment, by group")
  tbl_out(
    .tab   = edg_filer_distribution(.tab = .tab) |> dplyr::filter(.data$nFilers <= 8L),
    .title = NULL,
    .notes = c(nFilers = "Truncated at eight; the tail runs much further on registration statements.")
  )

  invisible(sum_)
}


# 7. Reports -----------------------------------------------------------------------------------------------------------

#' Parse failures by file type and message
#'
#' @param .tab_err Output of the parse-error pass.
#' @param .tab_idx The DocID to Path index, which supplies the group.
#' @return A tibble: Group, nDocs, nFailed, ShareFailed.
edg_error_summary <- function(.tab_err, .tab_idx) {
  if (FALSE) {
    .tab_err <- tab_errors
    .tab_idx <- tab_index
  }

  .tab_idx |>
    dplyr::select("DocID", "DocType") |>
    dplyr::mutate(Group = edg_doc_group(.doc_type = .data$DocType)) |>
    dplyr::left_join(dplyr::select(.tab_err, "DocID", "ErrParse"), by = dplyr::join_by("DocID")) |>
    dplyr::summarise(
      nDocs   = dplyr::n(),
      nFailed = sum(.data$ErrParse == 1L, na.rm = TRUE),
      .by     = "Group"
    ) |>
    dplyr::mutate(ShareFailed = .data$nFailed / .data$nDocs) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' What the parse failures were
#'
#' @param .tab_err Output of the parse-error pass.
#' @return A tibble: DocExt, MsgParse, nDocs.
edg_error_messages <- function(.tab_err) {
  if (FALSE) .tab_err <- tab_errors

  .tab_err |>
    dplyr::filter(.data$ErrParse == 1L) |>
    dplyr::summarise(nDocs = dplyr::n(), .by = c("DocExt", "MsgParse")) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' Report parse failures
#'
#' @param .tab_err Output of the parse-error pass.
#' @param .tab_idx The DocID to Path index, which supplies the group.
#' @return The summary tibble, invisibly.
edg_report_errors <- function(.tab_err, .tab_idx) {
  if (FALSE) {
    .tab_err <- tab_errors
    .tab_idx <- tab_index
  }

  sum_ <- edg_error_summary(.tab_err = .tab_err, .tab_idx = .tab_idx)

  tbl_head("Parse failures by group")
  tbl_out(
    .tab    = sum_,
    .title  = NULL,
    .pct    = "ShareFailed",
    .digits = 3L,
    .notes  = c(ShareFailed = "Of that group's own documents, not of the corpus.")
  )

  tbl_head("What the failures were")
  tbl_out(.tab = edg_error_messages(.tab_err = .tab_err), .title = NULL, .n = 15L)
  tbl_note("PDF conversion is the bulk of it and is expected; a failure concentrated in a file type \\
            that previously converted is not.")

  invisible(sum_)
}

#' Removals by rule and document type
#'
#' EACH GROUP AGAINST ITS OWN DENOMINATOR. An exclusion rate divided by the size of the whole corpus
#' says how much of the corpus a rule removed, which is not the question. The question is what share
#' of contracts, of 8-K reports, and of CT orders each rule removes, and those are three numbers.
#'
#' @param .tab_rem Output of edg_flag_removals().
#' @param .tab_idx The DocID to Path index, which supplies the per-group denominator.
#' @return A tibble: Group, RemClass, nDocs, nGroup, ShareOfGroup.
edg_removal_summary <- function(.tab_rem, .tab_idx) {
  if (FALSE) {
    .tab_rem <- tab_removed
    .tab_idx <- tab_index
  }

  den_ <- .tab_idx |>
    dplyr::mutate(Group = edg_doc_group(.doc_type = .data$DocType)) |>
    dplyr::summarise(nGroup = dplyr::n(), .by = "Group")

  .tab_rem |>
    dplyr::mutate(Group = edg_doc_group(.doc_type = .data$DocType)) |>
    dplyr::summarise(nDocs = dplyr::n(), .by = c("Group", "RemClass")) |>
    dplyr::left_join(den_, by = dplyr::join_by("Group")) |>
    dplyr::mutate(ShareOfGroup = .data$nDocs / .data$nGroup) |>
    dplyr::arrange(.data$Group, .data$RemClass)
}

#' Documents surviving the quality rules, by group
#'
#' @param .tab_rem Output of edg_flag_removals().
#' @param .tab_idx The DocID to Path index.
#' @return A tibble: Group, nDocs, nFlagged, nKept, ShareKept.
edg_survival_summary <- function(.tab_rem, .tab_idx) {
  if (FALSE) {
    .tab_rem <- tab_removed
    .tab_idx <- tab_index
  }

  rem_ <- .tab_rem$DocID

  .tab_idx |>
    dplyr::mutate(Group = edg_doc_group(.doc_type = .data$DocType)) |>
    dplyr::summarise(
      nDocs    = dplyr::n(),
      nFlagged = sum(.data$DocID %in% rem_),
      .by      = "Group"
    ) |>
    dplyr::mutate(
      nKept     = .data$nDocs - .data$nFlagged,
      ShareKept = .data$nKept / .data$nDocs
    ) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
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

  tbl_head("What survives, by group")
  tbl_out(
    .tab    = edg_survival_summary(.tab_rem = .tab_rem, .tab_idx = .tab_idx),
    .title  = NULL,
    .pct    = "ShareKept",
    .digits = 1L
  )

  tbl_head("Which rule fired, by group")
  tbl_out(
    .tab    = sum_,
    .title  = NULL,
    .pct    = "ShareOfGroup",
    .digits = 2L,
    .notes  = c(
      RemClass     = "Rules are applied in order; a document caught by two is attributed to the first.",
      ShareOfGroup = "Of that group's own documents. A group absent from a row had no document caught."
    )
  )

  invisible(sum_)
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

  edg_report_errors(.tab_err = .tab_err, .tab_idx = .tab_idx)
  edg_report_removals(.tab_rem = .tab_rem, .tab_idx = .tab_idx)
  edg_report_filer_copies(.tab = .tab_meta)

  invisible(NULL)
}


# 8. Figures -----------------------------------------------------------------------------------------------------------

#' Flagged documents by rule and group
#'
#' Stacked rather than ranked, because the summary now carries a row per group and a ranked bar
#' would draw each rule three times without saying which group each bar belonged to.
#'
#' @param .tab Output of edg_removal_summary().
#' @return A ggplot object.
edg_plot_removals <- function(.tab) {
  if (FALSE) .tab <- edg_removal_summary(tab_removed, tab_index)

  plot_bar_stacked(
    .tab  = dplyr::mutate(.tab, RemRule = stringi::stri_sub(.data$RemClass, 1L, 1L)),
    .cat  = "RemRule",
    .val  = "nDocs",
    .fill = "Group"
  )
}
