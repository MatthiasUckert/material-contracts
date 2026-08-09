# 04D-EntityApply: apply the settled entity policy to the corpus ----
#
# WHAT THIS FILE DOES, IN ONE PARAGRAPH
# Every decision has been made. 04A extracted, 04B measured which engine to keep and which window to
# read it under, 04C turned spans into variables and checked them against facts EDGAR recorded. This
# stage does none of that again. It reads extraction_policy.parquet and rules_kept.parquet and
# applies them to a corpus that carries no labels.
#
# WHY THE APPLY STAGE HAS ALMOST NO DECISIONS OF ITS OWN
# An apply stage free to choose its own engine or its own window is one whose output depends on
# which version of it last ran, and a released dataset cannot be defended on those terms. The engine
# per label, the character budget from each end, and the rules are all read from artifacts. What is
# left here is bookkeeping: index the tree, slice the text, run the extractors, resolve, write.
#
# THE WINDOW IS TWO SLICES, NOT ONE TRUNCATION
# --max-chars truncates a prefix, and 04B found that geography needs the end of a contract as much
# as the beginning: a governing-law clause sits in the final fifth, and a head-only window costs
# twenty-six points of document-level recall on places. So a label with a tail budget is extracted
# twice, from a head slice and a tail slice, and the tail's offsets are shifted back onto the full
# document before anything reads them. Splicing the two into one string would have been less code
# and would have made every offset index a text that exists nowhere.
#
# THE ANCHORS TRAVEL WITH THE RELEASE
# The filer's name and its filing date are EDGAR facts, available for every contract in the corpus
# and not only for the labelled sample. So the consistency checks 04C ran as diagnostics run here as
# well, and every released row carries whether the filer appeared among its resolved parties and
# whether its contract date precedes its filing. A user can condition on those without re-running
# anything, which is a different offer from a single headline number.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_policy <- .lP$Input$Policy
  .dir_corpus  <- .lP$Input$DirCorpus
  .chunk       <- dplyr::slice_head(tab_index, n = 200L)
}


# Null-coalescing. Base R carries this from 4.4.0; defining it keeps the script off a version
# dependency, and 03C-CoWorkKeywords.R does the same for the same reason.
`%||%` <- function(.x, .y) if (is.null(.x)) .y else .x


# 1. The corpus index -----------------------------------------------------
# The labelled sample fits in memory and the corpus does not. What is held throughout is one row per
# document with its path and the EDGAR facts; text is read one chunk at a time and discarded.

#' One row per corpus document, with its path and the facts the release is checked against
#'
#' The anchor columns are taken here rather than at resolution because they are cheap, they are the
#' same facts 04B measured against, and carrying them per document means the consistency flags can
#' be written into the release rather than computed once as a diagnostic.
#'
#' @param .dir_corpus Root of the parsed-contract tree.
#' @param .path_meta EDGAR document metadata parquet.
#' @param .path_landing EDGAR landing-page parquet supplying addresses, or NULL.
#' @param .path_cache Where the file index is cached; walking a million paths is not repeated.
#' @param .rerun TRUE re-walks the tree.
#' @param .limit Integer to rehearse on a random draw, NULL for the corpus.
#' @param .seed Sampling seed, so a limited run draws the same documents each time.
#' @return Tibble: DocID, Path, and the anchor columns that resolved.
ent_corpus_index <- function(.dir_corpus, .path_meta, .path_landing = NULL, .path_cache,
                             .rerun = FALSE, .limit = NULL, .seed = 42L) {
  if (FALSE) {
    .dir_corpus   <- .lP$Input$DirCorpus
    .path_meta    <- .lP$Input$MetaData
    .path_landing <- .lP$Input$LandingPage
    .path_cache   <- .lP$Cache$CorpusFiles
    .rerun        <- FALSE
    .limit        <- 2000L
    .seed         <- 42L
  }
  if (!fs::dir_exists(.dir_corpus)) cli::cli_abort("No corpus tree at {(.dir_corpus)}")

  idx_ <- utils_list_project_files(
    .dir_data = .dir_corpus,
    .path_out = .path_cache,
    .rerun    = .rerun
  ) |>
    dplyr::select(DocID, Path) |>
    dplyr::mutate(Path = unname(.data$Path))
  cli::cli_alert_info("Corpus index: {nrow(idx_)} document{?s}")

  want_ <- c("CIK", "CompanyName", "DateFiled", "HashIndex")
  avail_ <- arrow::open_dataset(sources = .path_meta)$schema$names
  idx_ <- idx_ |>
    dplyr::left_join(
      arrow::open_dataset(sources = .path_meta) |>
        dplyr::select(dplyr::all_of(c("DocID", intersect(want_, avail_)))) |>
        dplyr::collect() |>
        dplyr::distinct(.data$DocID, .keep_all = TRUE),
      by = dplyr::join_by(DocID)
    )

  if (!is.null(.path_landing) && fs::file_exists(.path_landing) && "HashIndex" %in% names(idx_)) {
    land_ <- arrow::open_dataset(sources = .path_landing)$schema$names
    take_ <- intersect(c("BusinessAddress", "MailingAddress"), land_)
    if (length(take_) > 0L && "HashIndex" %in% land_) {
      idx_ <- idx_ |>
        dplyr::left_join(
          arrow::open_dataset(sources = .path_landing) |>
            dplyr::select(dplyr::all_of(c("HashIndex", take_))) |>
            dplyr::collect() |>
            dplyr::distinct(.data$HashIndex, .keep_all = TRUE),
          by = dplyr::join_by(HashIndex)
        )
    }
  }

  if (!is.null(.limit) && .limit < nrow(idx_)) {
    idx_ <- withr::with_seed(.seed, dplyr::slice_sample(idx_, n = .limit))
    cli::cli_alert_warning(
      "Limited to {nrow(idx_)} document{?s}, drawn at random across the whole tree. Output from a \\
       limited run is written to a separate directory so it cannot be mistaken for a corpus pass."
    )
  }
  idx_
}

#' Where a run writes, kept apart when it is a rehearsal
#' @param .dir Release directory.
#' @param .limit The Limit parameter; NULL means a corpus pass.
#' @return A path.
ent_output_dir <- function(.dir, .limit = NULL) {
  if (FALSE) {
    .dir   <- .lP$Output$Vars
    .limit <- 2000L
  }
  if (is.null(.limit)) .dir else paste0(.dir, "_preview")
}

#' Read the text for one chunk; it enters memory here and leaves when the chunk is written
#' @param .chunk Rows of the index.
#' @return .chunk with a Text column, unreadable documents dropped.
ent_read_chunk <- function(.chunk) {
  if (FALSE) .chunk <- dplyr::slice_head(tab_index, n = 100L)

  out_ <- .chunk |> dplyr::mutate(Text = purrr::map_chr(.data$Path, clf_read_text))
  n_bad_ <- sum(is.na(out_$Text) | !nzchar(dplyr::coalesce(out_$Text, "")))
  if (n_bad_ > 0L) {
    cli::cli_alert_warning("{n_bad_} document{?s} unreadable or empty; dropped from this chunk.")
  }
  out_ |> dplyr::filter(!is.na(.data$Text), nzchar(.data$Text))
}


# 2. Extraction over two slices -------------------------------------------

#' The distinct extractor runs a policy implies
#'
#' Several labels can share an engine under different windows -- LexNLP serves organisations with no
#' tail and dates with one -- so running once per label would extract the same head twice. Grouping
#' by engine and slice runs each combination once and keeps only the labels that asked for it.
#'
#' @param .policy Tibble from ent_read_policy().
#' @return Tibble: Combo, Slice, Chars, Labels (a list column).
ent_slice_plan <- function(.policy) {
  if (FALSE) .policy <- tab_policy

  head_ <- .policy |>
    dplyr::transmute(Combo, Slice = "head", Chars = as.integer(.data$CapChars), Label)
  tail_ <- .policy |>
    dplyr::filter(.data$TailChars > 0L) |>
    dplyr::transmute(Combo, Slice = "tail", Chars = as.integer(.data$TailChars), Label)

  dplyr::bind_rows(head_, tail_) |>
    dplyr::summarise(Labels = list(sort(unique(.data$Label))), .by = c(Combo, Slice, Chars)) |>
    dplyr::arrange(.data$Combo, .data$Slice)
}

#' Write the slim text parquet one extractor run reads
#'
#' The tail slice records the offset it starts at. Every span the extractor returns for it indexes
#' the slice, and adding the shift back is what keeps a corpus offset meaning the same thing as a
#' sample offset. A document shorter than the budget is its own slice and the shift is zero.
#'
#' @param .docs Chunk with DocID and Text.
#' @param .slice "head" or "tail".
#' @param .chars Character budget.
#' @param .path Destination parquet.
#' @return Tibble: DocID, OffsetShift.
ent_write_slice <- function(.docs, .slice, .chars, .path) {
  if (FALSE) {
    .docs  <- docs_
    .slice <- "tail"
    .chars <- 5000L
    .path  <- fs::file_temp(ext = "parquet")
  }

  len_ <- stringi::stri_length(.docs$Text)
  if (identical(.slice, "head")) {
    txt_   <- stringi::stri_sub(.docs$Text, 1L, pmin(len_, .chars))
    shift_ <- rep(0L, nrow(.docs))
  } else {
    shift_ <- pmax(0L, len_ - .chars)
    txt_   <- stringi::stri_sub(.docs$Text, shift_ + 1L, len_)
  }
  fs::dir_create(fs::path_dir(.path))
  arrow::write_parquet(tibble::tibble(DocID = .docs$DocID, TextRaw = txt_), .path)
  tibble::tibble(DocID = .docs$DocID, OffsetShift = as.integer(shift_))
}

#' Dispatch one combo to its extractor
#'
#' The same wrappers 04A ran, so the corpus is extracted by the code the measurement was made on.
#' A combo with no branch aborts rather than returning nothing, because an engine silently absent
#' from a corpus pass is a column of missing variables nobody can explain afterwards.
#'
#' @param .combo Engine token, as in the policy.
#' @param .input,.output Parquet paths.
#' @param .labels Labels to request.
#' @param .max_chars Truncation, or NULL; the slice is already cut, so this is normally NULL.
#' @param .n_process,.batch_size,.timeout Per-combo throughput knobs.
#' @param .device spaCy device string.
#' @param .quiet Passed through.
#' @return Invisibly .output.
ent_run_engine <- function(.combo, .input, .output, .labels, .max_chars = NULL,
                           .n_process = 16L, .batch_size = 64L, .timeout = 0L,
                           .device = "auto", .quiet = TRUE) {
  if (FALSE) {
    .combo  <- "lexnlp"
    .input  <- fs::file_temp(ext = "parquet")
    .output <- fs::file_temp(ext = "parquet")
    .labels <- c("ORG", "DATE")
  }

  parts_ <- strsplit(.combo, ":", fixed = TRUE)[[1]]
  engine_ <- parts_[1]
  model_  <- if (length(parts_) > 1L) paste(parts_[-1], collapse = ":") else engine_

  if (engine_ == "spacy") {
    ner_spacy(.inputs = .input, .output = .output, .labels = .labels, .max_chars = .max_chars,
              .model = model_, .device = if (grepl("trf", model_, fixed = TRUE)) .device else "cpu",
              .batch_size = .batch_size, .n_process = if (grepl("trf", model_, fixed = TRUE)) 1L
              else .n_process, .timeout = .timeout, .quiet = .quiet)
  } else if (engine_ == "lexnlp") {
    ner_lexnlp(.inputs = .input, .output = .output, .labels = .labels, .max_chars = .max_chars,
               .chunk_size = .batch_size, .n_process = .n_process, .timeout = .timeout,
               .quiet = .quiet)
  } else if (engine_ == "paper" && model_ == "dateregex-v1") {
    ner_dateregex(.inputs = .input, .output = .output, .labels = .labels,
                  .max_chars = .max_chars, .quiet = .quiet)
  } else if (engine_ == "paper" && model_ == "gazetteer-v1") {
    ner_gazetteer(.inputs = .input, .output = .output, .labels = .labels, .max_chars = .max_chars,
                  .n_process = .n_process, .chunk_size = .batch_size, .timeout = .timeout,
                  .quiet = .quiet)
  } else if (engine_ == "paper" && model_ == "redaction-v1") {
    ner_redaction(.inputs = .input, .output = .output, .labels = .labels, .max_chars = .max_chars,
                  .n_process = .n_process, .chunk_size = .batch_size, .timeout = .timeout,
                  .quiet = .quiet)
  } else {
    cli::cli_abort("No extractor for combo {.val {(.combo)}}.")
  }
  invisible(.output)
}

#' Extract one chunk under the whole policy and return candidates on full-document offsets
#'
#' @param .docs Chunk with DocID and Text.
#' @param .plan Tibble from ent_slice_plan().
#' @param .dir_work Scratch directory for the slices and the extractor output.
#' @param .knobs Named list of per-combo throughput settings.
#' @param .device spaCy device string.
#' @return Tibble: DocID, Label, Start, Stop, Span, LabelRaw, Engine, Model.
ent_extract_chunk <- function(.docs, .plan, .dir_work, .knobs = list(), .device = "auto") {
  if (FALSE) {
    .docs     <- docs_
    .plan     <- tab_plan
    .dir_work <- fs::path(.dir_main, "Work")
    .knobs    <- .KNOBS
    .device   <- "auto"
  }

  fs::dir_create(.dir_work)
  out_ <- purrr::pmap(.plan, function(Combo, Slice, Chars, Labels) {
    tag_  <- paste0(gsub("[^A-Za-z0-9]", "-", Combo), "_", Slice)
    in_   <- fs::path(.dir_work, paste0("in_", tag_, ".parquet"))
    outp_ <- fs::path(.dir_work, paste0("out_", tag_, ".parquet"))
    if (fs::file_exists(outp_)) fs::file_delete(outp_)

    shift_ <- ent_write_slice(.docs = .docs, .slice = Slice, .chars = Chars, .path = in_)
    knob_  <- .knobs[[Combo]] %||% list()
    ent_run_engine(
      .combo      = Combo,
      .input      = in_,
      .output     = outp_,
      .labels     = Labels,
      .max_chars  = NULL,                       # the slice is already cut
      .n_process  = knob_$n_process  %||% 16L,
      .batch_size = knob_$batch_size %||% 64L,
      .timeout    = knob_$timeout    %||% 0L,
      .device     = .device,
      .quiet      = TRUE
    )
    cand_ <- arrow::read_parquet(outp_) |>
      dplyr::filter(!is.na(.data$Start), .data$Label %in% Labels) |>
      dplyr::left_join(shift_, by = dplyr::join_by(DocID)) |>
      dplyr::mutate(
        Start = as.integer(.data$Start) + .data$OffsetShift,
        Stop  = as.integer(.data$Stop)  + .data$OffsetShift
      ) |>
      dplyr::select(-OffsetShift)
    fs::file_delete(c(in_, outp_)[fs::file_exists(c(in_, outp_))])
    cand_
  }) |>
    purrr::list_rbind()

  # A short document is its whole head slice and its whole tail slice, so the same span arrives
  # twice. Deduplicating on the span itself is exact; deduplicating on the document is not.
  dplyr::distinct(out_, DocID, Label, Start, Stop, Span, .keep_all = TRUE)
}


# 3. Resolution over a chunk ----------------------------------------------

#' A DuckDB session holding one chunk's candidates in the shape the resolvers expect
#'
#' 04C reads a table called deployed and a table called lens, and gets them by filtering a persistent
#' store through the policy. Here the extraction was already run under the policy, so the candidates
#' ARE the deployed set. Building the same two tables means every resolver below is the code 04C was
#' validated with rather than a corpus variant of it.
#'
#' @param .cands Tibble from ent_extract_chunk().
#' @param .docs Chunk with DocID and Text.
#' @param .path_text Where the chunk's text is written for the resolvers to slice context from.
#' @return A live DBI connection; the caller disconnects.
ent_chunk_session <- function(.cands, .docs, .path_text) {
  if (FALSE) {
    .cands     <- cand_
    .docs      <- docs_
    .path_text <- fs::file_temp(ext = "parquet")
  }

  arrow::write_parquet(tibble::tibble(DocID = .docs$DocID, TextRaw = .docs$Text), .path_text)
  con_ <- DBI::dbConnect(duckdb::duckdb())
  ent_put_table(.con = con_, .name = "deployed", .tab = dplyr::select(
    .cands, DocID, Label, Start, Stop, Span, LabelRaw
  ))
  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TABLE lens AS SELECT DocID, length(TextRaw) AS DocLen ",
    "FROM read_parquet('", as.character(fs::path_abs(.path_text)), "') WHERE length(TextRaw) > 0"
  ))
  con_
}

#' Resolve one chunk into variable rows, using 04C's resolvers unchanged
#'
#' @param .con Session from ent_chunk_session().
#' @param .path_text The chunk's text parquet.
#' @param .rules Tibble of rules kept by 04B.
#' @param .keys Per-document anchor keys for this chunk.
#' @param .formats strptime formats.
#' @param .ctx_max Context width.
#' @param .head_pos Position under which an uncued organisation still reads as a party.
#' @return Tibble: one row per document.
ent_resolve_chunk <- function(.con, .path_text, .rules, .keys, .formats,
                              .ctx_max = 400L, .head_pos = 0.10) {
  if (FALSE) {
    .con       <- con_
    .path_text <- path_text_
    .rules     <- tab_rules
    .keys      <- keys_
    .formats   <- .DATE_FORMATS
  }

  ent_defined_terms(.con = .con, .path_text = .path_text)
  ent_assign_roles(.con = .con, .path_text = .path_text, .rules = .rules, .ctx_max = .ctx_max)

  ent_assemble(
    .keys     = .keys,
    .parties  = ent_resolve_parties(.con = .con, .head_pos = .head_pos),
    .dates    = ent_resolve_dates(.con = .con, .formats = .formats),
    .places   = ent_resolve_places(.con = .con),
    .value    = ent_resolve_value(.con = .con),
    # The published measure needs every date in the document, and at corpus scale "every date in the
    # document" is every date the deployed engines returned -- there is no unfiltered store to fall
    # back on. The column therefore describes the deployed window, and 04C's sample figure is the
    # one that reproduces the published definition on full text.
    .pubdates = ent_published_dates_chunk(.con = .con, .formats = .formats)
  )
}

#' Maximum date per document, from this chunk's candidates
#' @param .con Session with roles built.
#' @param .formats strptime formats.
#' @return Tibble: DocID, MaxDateAny, NDatesFull.
ent_published_dates_chunk <- function(.con, .formats) {
  if (FALSE) {
    .con     <- con_
    .formats <- .DATE_FORMATS
  }

  fmt_ <- paste0("['", paste(.formats, collapse = "','"), "']")
  DBI::dbGetQuery(.con, paste0(
    "WITH d AS (SELECT DISTINCT DocID, Span FROM deployed WHERE Label = 'DATE'), ",
    "p AS (SELECT DocID, CAST(try_strptime(", ent_sql_dateclean("Span"), ", ", fmt_,
    ") AS DATE) AS D FROM d) ",
    "SELECT DocID, max(D) AS MaxDateAny, COUNT(D) AS NDatesFull FROM p ",
    "WHERE D IS NOT NULL GROUP BY DocID"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(NDatesFull = as.integer(.data$NDatesFull))
}


# 4. Bookkeeping ----------------------------------------------------------
# The two timing helpers follow 03F-ClassifyApply.R, which solved the same problem for the
# classifier: a corpus pass is measured in hours, so the rate has to be measured on this machine
# rather than assumed, and a resumed run must keep the timings it already paid for.

#' Record what one chunk cost, beside the output rather than inside it
#' @param .dir Output directory.
#' @param .chunk Chunk index.
#' @param .n_docs Documents resolved.
#' @param .seconds Wall time.
#' @param .n_cands Candidates extracted.
#' @return Invisibly the path written.
ent_write_timing <- function(.dir, .chunk, .n_docs, .seconds, .n_cands) {
  if (FALSE) {
    .dir     <- .dir_out
    .chunk   <- 1L
    .n_docs  <- 500L
    .seconds <- 120
    .n_cands <- 40000L
  }
  dir_ <- fs::path(.dir, "_timing")
  fs::dir_create(dir_)
  path_ <- fs::path(dir_, sprintf("timing_%04d.parquet", as.integer(.chunk)))
  arrow::write_parquet(
    tibble::tibble(
      Chunk = as.integer(.chunk), nDocs = as.integer(.n_docs),
      nCands = as.integer(.n_cands), Seconds = as.numeric(.seconds), WrittenAt = Sys.time()
    ),
    path_
  )
  invisible(path_)
}

#' Measured throughput, and what a full pass would cost at that rate
#' @param .dir Output directory.
#' @param .n_corpus Documents a full pass would cover.
#' @return Invisibly the timing tibble.
ent_report_throughput <- function(.dir, .n_corpus) {
  if (FALSE) {
    .dir      <- .dir_out
    .n_corpus <- nrow(tab_index)
  }
  dir_ <- fs::path(.dir, "_timing")
  if (!fs::dir_exists(dir_)) {
    cli::cli_alert_info("Nothing timed yet; run at least one chunk to measure this machine.")
    return(invisible(NULL))
  }
  tab_ <- fs::dir_ls(dir_, glob = "*.parquet") |>
    purrr::map(arrow::read_parquet) |>
    purrr::list_rbind()
  if (nrow(tab_) == 0L) return(invisible(NULL))

  rate_ <- sum(tab_$nDocs) / sum(tab_$Seconds)
  cli::cli_h2("Measured throughput")
  clf_say_table(
    .tab = tibble::tibble(
      Chunks      = nrow(tab_),
      Documents   = sum(tab_$nDocs),
      Candidates  = sum(tab_$nCands),
      Minutes     = round(sum(tab_$Seconds) / 60, 1),
      DocsPerSec  = round(rate_, 1),
      CandsPerDoc = round(sum(tab_$nCands) / sum(tab_$nDocs), 1),
      FullPassHrs = round(.n_corpus / rate_ / 3600, 1)
    )
  )
  cli::cli_alert_info(
    "Extraction and resolution are timed together, because that is what a pass costs. \\
     CandsPerDoc times the corpus is what 04E's consumer has to store."
  )
  invisible(tab_)
}


# 5. Report ---------------------------------------------------------------

#' What the pass produced and how far it agrees with the EDGAR facts
#' @param .tab Assembled variable rows.
#' @return Invisibly .tab.
ent_report_apply <- function(.tab) {
  if (FALSE) .tab <- tab_vars

  cli::cli_h2("Released variables")
  clf_say_table(
    .tab = tibble::tibble(
      Item = c("Documents", "With >=2 parties", "With a contract start", "With a contract end",
               "With an amount", "With a redaction marker", "With a party address"),
      N = c(nrow(.tab),
            sum(.tab$NParties >= 2L, na.rm = TRUE),
            sum(!is.na(.tab$ContractStart)),
            sum(!is.na(.tab$ContractEnd)),
            sum(!is.na(.tab$MaxAmount)),
            sum(dplyr::coalesce(.tab$NRedact, 0L) > 0L),
            sum(dplyr::coalesce(.tab$NPartyAddress, 0L) > 0L))
    ) |>
      dplyr::mutate(Share = clf_pct(.data$N / nrow(.tab)))
  )

  cli::cli_h2("Consistency flags carried by the release")
  clf_say_table(
    .tab = tibble::tibble(
      Check = c("Party set contains the filer", "Contract start at or before the filing date"),
      N     = c(sum(.tab$HasFilerParty, na.rm = TRUE),
                sum(.tab$ContractStart <= .tab$DateFiled, na.rm = TRUE)),
      Of    = c(sum(!is.na(.tab$HasFilerParty)),
                sum(!is.na(.tab$ContractStart) & !is.na(.tab$DateFiled)))
    ) |>
      dplyr::mutate(Share = clf_pct(.data$N / .data$Of))
  )
  cli::cli_alert_info(
    "These are computed for every released row, not on a sample: the filer's name and its filing \\
     date are EDGAR facts available corpus-wide, so a user can condition on them without re-running \\
     anything."
  )
  invisible(.tab)
}
