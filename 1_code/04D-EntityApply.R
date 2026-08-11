# 04D-EntityApply: extract every engine over the whole corpus into one store ------------------------------------------
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


# 1. The corpus index ------------------------------------------------------------------------------------------------
# Where the documents are and what EDGAR knows about them. Walked once and cached: listing
# 1.46 million files is not something to repeat on every render.


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

  # THE CORPUS SIZE TRAVELS WITH THE INDEX, and it has to, because the moment a limit is applied the
  # index stops knowing how big the corpus is and nrow() silently becomes the rehearsal size. The
  # throughput projection is computed from that number, so a rehearsal projected onto itself and
  # reported the corpus pass as taking a tenth of an hour when the true figure was eighty.
  #
  # A rehearsal is the ONLY time the projection is wanted, which is exactly when nrow() is wrong.
  n_corpus_ <- nrow(idx_)

  # Total size on disk, taken BEFORE the limit for the same reason as the count: it is the corpus
  # fingerprint, and a rehearsal that fingerprinted its own ten thousand documents would abort the
  # release run with "the tree has changed" when nothing had. One stat call per file, once per
  # render, which is seconds against a pass measured in days.
  bytes_corpus_ <- sum(as.numeric(fs::file_size(idx_$Path)), na.rm = TRUE)

  if (!is.null(.limit) && .limit < nrow(idx_)) {
    idx_ <- withr::with_seed(.seed, dplyr::slice_sample(idx_, n = .limit))
    cli::cli_alert_warning(
      "Limited to {nrow(idx_)} document{?s}, drawn at random across the whole tree. This writes to \\
       the SAME store as a full run and its work counts towards it: the ledger records each \\
       document against each engine, so setting Limit to NULL continues rather than restarting."
    )
  }
  attr(idx_, "NCorpus")     <- as.integer(n_corpus_)
  attr(idx_, "BytesCorpus") <- bytes_corpus_
  idx_
}


#' How many documents the corpus holds, whatever the index was limited to
#'
#' The accessor exists so that a call site cannot reach for nrow() by mistake. Under a rehearsal
#' nrow() is the rehearsal size, and every quantity scaled by it -- the throughput projection above
#' all -- comes out wrong by whatever factor the limit imposed, silently and in the reassuring
#' direction.
#'
#' @param .index Tibble from ent_corpus_index().
#' @param .what Which quantity: the document count or the total size on disk.
#' @return Numeric. The corpus figure, before any limit.
ent_corpus_n <- function(.index, .what = c("docs", "bytes")) {
  if (FALSE) {
    .index <- tab_index
    .what  <- "docs"
  }
  .what <- match.arg(.what)
  key_  <- if (.what == "docs") "NCorpus" else "BytesCorpus"

  n_ <- attr(.index, key_)
  if (is.null(n_)) {
    cli::cli_abort(c(
      "Index carries no corpus {(.what)} figure.",
      "i" = "It must come from ent_corpus_index(), which records both before limiting."
    ))
  }
  n_
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


# 2. The plan --------------------------------------------------------------------------------------------------------
# Which engines run and which labels each is asked for, checked against the dispatch before
# any work starts.

#' Which engine runs, and for which labels
#'
#' Replaces the head-and-tail slice plan the previous version built. Every label reads full text, so
#' there is nothing to slice: what the policy still determines is which engines run and which labels
#' each is asked for. Several labels can share an engine -- LexNLP serves organisations and dates --
#' so grouping by engine runs each once rather than once per label.
#'
#' @param .policy Tibble from ent_read_policy().
#' @return Tibble: Combo and Labels, a list column.
ent_engine_plan <- function(.policy) {
  if (FALSE) .policy <- tab_policy

  cap_ <- unique(.policy$CapChars)
  tail_ <- unique(.policy$TailChars)
  if (!all(tail_ == 0L)) {
    cli::cli_abort(c(
      "The policy declares a tail for {.policy$Label[.policy$TailChars > 0L]}.",
      "i" = "This document extracts full text only; a tail needs the slice design in _BackUp.",
      "x" = "Ignoring it would extract a prefix and report success."
    ))
  }

  .policy |>
    dplyr::summarise(Labels = list(sort(unique(.data$Label))), .by = Combo) |>
    dplyr::arrange(.data$Combo)
}


#' The labels argument ner_run() expects, keyed by combination
#'
#' ner_run() takes labels either as a vector applying to everything or as a list keyed on the
#' combination token. The second is what a mixed engine set needs: asking the gazetteer for DATE
#' would return nothing and asking LexNLP for GPE would return the geoentity pass this project
#' excludes by policy.
#'
#' @param .plan Tibble from ent_engine_plan().
#' @return Named list of character vectors.
ent_plan_labels <- function(.plan) {
  if (FALSE) .plan <- tab_plan
  purrr::set_names(.plan$Labels, .plan$Combo)
}

#'
#' ner_run() dispatches on the combination token through a chain of branches, so one it does not
#' cover surfaces as an abort from inside the extraction loop -- after the index is built and the
#' first chunks have run. On a corpus pass that is hours in. Naming the supported set once, here,
#' lets the plan be checked before any work starts.
#'
#' @return Character vector of supported engine and model-stem tokens.
ent_engine_supported <- function() {
  c("spacy", "lexnlp", "paper:dateregex", "paper:gazetteer", "paper:redaction", "paper:moneyregex")
}


#' Fail before the pass rather than during it
#'
#' Checks every combination in the plan against the dispatch. This exists because the check it
#' performs was missing: money moved from the transformer to the regex arm in 04B, the policy and
#' the throughput knobs were updated, and the extractor dispatch was not -- so the plan named an
#' engine nothing could run and a rehearsal aborted three combinations into the first chunk.
#'
#' Also warns on a combination with no throughput knobs. That is not fatal -- the defaults apply --
#' but on this engine set the defaults are wrong often enough to be worth seeing: the transformer
#' cannot share a device, LexNLP stalls without a timeout.
#'
#' @param .plan Tibble from ent_engine_plan().
#' @param .knobs Named list of per-combination throughput settings.
#' @return Invisibly the plan, unchanged.
ent_check_plan <- function(.plan, .knobs = list()) {
  if (FALSE) {
    .plan  <- tab_plan
    .knobs <- .KNOBS
  }

  stem_ <- function(.x) {
    eng_ <- sub(":.*$", "", .x)
    mod_ <- sub("-v[0-9]+$", "", sub("^[^:]*:", "", .x))
    dplyr::if_else(grepl(":", .x, fixed = TRUE), paste0(eng_, ":", mod_), eng_)
  }

  bad_ <- setdiff(stem_(unique(.plan$Combo)), ent_engine_supported())
  if (length(bad_) > 0L) {
    cli::cli_abort(c(
      "The policy names {length(bad_)} engine{?s} this document cannot run: {bad_}.",
      "i" = "Dispatch covers: {ent_engine_supported()}.",
      "x" = "Left to the extraction loop this would abort part-way through a corpus pass."
    ))
  }

  noknob_ <- setdiff(unique(.plan$Combo), names(.knobs))
  if (length(noknob_) > 0L) {
    cli::cli_alert_warning(
      "No throughput settings for {noknob_}; the defaults apply, which are rarely right."
    )
  }
  cli::cli_alert_success("Plan checked: {dplyr::n_distinct(.plan$Combo)} engine{?s} dispatchable.")
  invisible(.plan)
}


# 3. The store fingerprint -------------------------------------------------------------------------------------------
# The one guard that cannot be recovered after the fact. See ent_corpus_manifest().


#' What the corpus store was built against
#'
#' THE OFFSET CONTRACT, AND IT IS DIFFERENT FROM 04A'S. The sample freezes one canonical text to
#' disk and every offset indexes that file. A corpus of 1.46 million documents cannot be frozen that
#' way -- the text alone runs to tens of gigabytes -- so the contract here is the reading function
#' instead: every offset indexes clf_read_text(Path), which is deterministic and is the same
#' function 04A derived its canonical text with.
#'
#' That holds only while the parsed tree holds. Regenerate the corpus and every offset in the store
#' silently indexes a different string: is.na() catches nothing, spans rehydrate as plausible
#' nonsense, and no number changes visibly. This fingerprint is what makes that loud.
#'
#' Size on disk rather than character count, because counting characters means reading 1.46 million
#' files and the fingerprint would then cost more than the thing it guards. A regenerated tree
#' changes its byte total; a tree that has not been touched does not.
#'
#' @param .index Tibble from ent_corpus_index(), unlimited.
#' @param .run Character. Combination tokens this store is built with.
#' @param .labels Named list from ent_plan_labels().
#' @return One-row tibble: NDocs, TotalBytes, Run, Labels, CreatedAt.
ent_corpus_manifest <- function(.index, .run, .labels) {
  if (FALSE) {
    .index  <- tab_index
    .run    <- tab_plan$Combo
    .labels <- lst_labels
  }

  # BOTH FIGURES DESCRIBE THE CORPUS, NOT THE INDEX IN HAND. Under a rehearsal the index holds the
  # rehearsal, so computing either from it would write a fingerprint of ten thousand documents and
  # then abort the release run against it -- reporting a regenerated tree when nothing had changed,
  # which is the one message here a reader would act on immediately and wrongly.
  tibble::tibble(
    NDocs      = ent_corpus_n(.index, .what = "docs"),
    TotalBytes = ent_corpus_n(.index, .what = "bytes"),
    Run        = paste(sort(.run), collapse = " | "),
    Labels     = paste(sort(unique(unlist(.labels))), collapse = ","),
    CreatedAt  = Sys.time()
  )
}


#' Compare the corpus fingerprint against the one the store was built under
#'
#' Same shape as 04A's manifest check and for the same reason: the DuckDB ledger records that a
#' document was processed by an engine and nothing else. It cannot tell that the document's text has
#' changed underneath it, so a rebuilt corpus produces a store that reports itself complete and
#' holds offsets into a string that no longer exists.
#'
#' A fingerprint mismatch is fatal here rather than advisory. On the sample a stale store wastes a
#' render; on the corpus it produces a released dataset whose spans point at the wrong characters.
#'
#' The engine inventory is not a fingerprint and is reported rather than enforced: adding an engine
#' is the ordinary incremental case, and the ledger handles it correctly.
#'
#' @param .path_manifest Where the manifest is written.
#' @param .manifest One-row tibble from ent_corpus_manifest().
#' @return Invisibly, a tibble of the comparison.
ent_corpus_manifest_sync <- function(.path_manifest, .manifest) {
  if (FALSE) {
    .path_manifest <- .lP$Store$Manifest
    .manifest      <- ent_corpus_manifest(tab_index, tab_plan$Combo, lst_labels)
  }

  show_ <- function(.x) {
    if (is.numeric(.x)) format(.x, scientific = FALSE, trim = TRUE) else as.character(.x)
  }
  keys_ <- c("NDocs", "TotalBytes", "Labels", "Run")
  kind_ <- c("fingerprint", "fingerprint", "fingerprint", "inventory")
  cur_  <- purrr::map_chr(keys_, \(.k) show_(.manifest[[.k]]))

  if (!fs::file_exists(.path_manifest)) {
    fs::dir_create(fs::path_dir(.path_manifest))
    arrow::write_parquet(.manifest, .path_manifest)
    out_ <- tibble::tibble(Field = keys_, Kind = kind_, Stored = "(new)", Current = cur_,
                           Match = TRUE, Note = "store created")
    tbl_say(.tab = out_, .title = "Corpus fingerprint")
    return(invisible(out_))
  }

  old_ <- arrow::read_parquet(.path_manifest)
  out_ <- tibble::tibble(
    Field  = keys_,
    Kind   = kind_,
    Stored = purrr::map_chr(keys_, \(.k) show_(old_[[.k]])),
    Current = cur_
  ) |>
    dplyr::mutate(Same = .data$Stored == .data$Current)

  split_ <- function(.x) if (is.na(.x)) character(0) else trimws(strsplit(.x, "|", fixed = TRUE)[[1]])
  was_   <- split_(out_$Stored[out_$Field == "Run"])
  now_   <- split_(out_$Current[out_$Field == "Run"])

  out_ <- out_ |>
    dplyr::mutate(
      Match = dplyr::if_else(.data$Kind == "inventory", TRUE, .data$Same),
      Note  = dplyr::case_when(
        .data$Kind == "inventory" & identical(was_, now_) ~ "unchanged",
        .data$Kind == "inventory" ~ paste0(length(now_), " asked for, ", length(was_), " stored"),
        .data$Same ~ "",
        TRUE ~ "TREE HAS CHANGED -- every offset in the store is suspect"
      ),
      Stored  = dplyr::if_else(.data$Kind == "inventory", paste0(length(was_), " combos"), .data$Stored),
      Current = dplyr::if_else(.data$Kind == "inventory", paste0(length(now_), " combos"), .data$Current)
    ) |>
    dplyr::select(Field, Kind, Stored, Current, Match, Note)

  tbl_say(.tab = out_, .title = "Corpus fingerprint")

  bad_ <- out_ |> dplyr::filter(.data$Kind == "fingerprint", !.data$Match)
  if (nrow(bad_) > 0L) {
    cli::cli_abort(c(
      "The corpus does not match what this store was built against: {bad_$Field}.",
      "x" = "Offsets in the store index text that has since been regenerated.",
      "i" = "Move the store aside and rebuild, or restore the tree it was built from."
    ))
  }
  if (!identical(was_, now_)) arrow::write_parquet(.manifest, .path_manifest)
  invisible(out_)
}


# 4. Extraction ------------------------------------------------------------------------------------------------------

#' Extract one chunk of the corpus into the shared store
#'
#' The chunk exists to bound memory, not to bound work: the text of five hundred documents is held
#' in R for as long as the extractors need it and then dropped. Resumption is NOT by chunk. It is
#' the store's own ledger, which records each document against each engine, so an interrupted run
#' resumes at the document it stopped on rather than at the start of its chunk, and a re-run of a
#' finished chunk costs one query.
#'
#' ner_run() does the extraction, and using it rather than a corpus-specific implementation is the
#' point: it is the function that built the sample store in 04A, so the two stores are populated by
#' one piece of code and differ only in what was pointed at them.
#'
#' @param .chunk Rows of the corpus index.
#' @param .db_path The corpus candidate store.
#' @param .run Character. Combination tokens to run.
#' @param .labels Named list from ent_plan_labels().
#' @param .dir_work Scratch directory; the chunk's text parquet is written and deleted here.
#' @param .knobs Named list of per-combination throughput settings.
#' @param .device Passed to the spaCy extractor.
#' @return Invisibly, a one-row tibble: documents read and seconds taken.
ent_extract_corpus_chunk <- function(.chunk, .db_path, .run, .labels, .dir_work,
                                     .knobs = list(), .device = "auto") {
  if (FALSE) {
    .chunk    <- dplyr::slice_head(tab_index, n = 50L)
    .db_path  <- .lP$Store$NerDB
    .run      <- tab_plan$Combo
    .labels   <- lst_labels
    .dir_work <- .lP$Work$Dir
    .knobs    <- .KNOBS
    .device   <- "auto"
  }

  t0_    <- Sys.time()
  docs_  <- ent_read_chunk(.chunk = .chunk)
  if (nrow(docs_) == 0L) {
    return(invisible(tibble::tibble(nDocs = 0L, Seconds = 0)))
  }

  fs::dir_create(.dir_work)
  path_ <- fs::file_temp(pattern = "corpus_", tmp_dir = .dir_work, ext = "parquet")
  arrow::write_parquet(tibble::tibble(DocID = docs_$DocID, TextRaw = docs_$Text), path_)
  on.exit(if (fs::file_exists(path_)) fs::file_delete(path_), add = TRUE)

  knob_ <- function(.k) {
    v_ <- purrr::map(.knobs, .k) |> purrr::compact()
    if (length(v_) == 0L) NULL else v_
  }

  ner_run(
    .inputs        = path_,
    .db_path       = .db_path,
    .run           = .run,
    .labels        = .labels,
    .max_chars     = NULL,        # full text; the window is a resolution-time filter in 04E
    .retry_timeout = FALSE,
    .id_col        = "DocID",
    .text_col      = "TextRaw",
    .docs_per_run  = NULL,        # the chunk IS the slice; ner_run must not re-chunk it
    .device        = .device,
    .n_process     = knob_("n_process")  %||% 16L,
    .batch_size    = knob_("batch_size") %||% 64L,
    .timeout       = knob_("timeout")    %||% 0L,
    .keep_staging  = FALSE,
    .quiet         = TRUE
  )

  invisible(tibble::tibble(
    nDocs   = nrow(docs_),
    Seconds = as.numeric(difftime(Sys.time(), t0_, units = "secs"))
  ))
}


# 5. Bookkeeping and report ------------------------------------------------------------------------------------------

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
ent_report_throughput <- function(.dir, .n_corpus, .n_cands = NULL) {
  if (FALSE) {
    .dir      <- .dir_store
    .n_corpus <- ent_corpus_n(tab_index)
    .n_cands  <- sum(.ov$ledger$Candidates)
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
  tbl_say(
    .tab = tibble::tibble(
      Chunks      = nrow(tab_),
      Documents   = sum(tab_$nDocs),
      Minutes     = round(sum(tab_$Seconds) / 60, 1),
      DocsPerSec  = round(rate_, 1),
      CorpusDocs  = as.integer(.n_corpus),
      FullPassHrs = round(.n_corpus / rate_ / 3600, 1),
      FullPassDay = round(.n_corpus / rate_ / 3600 / 24, 1),
      # Candidate figures come from the STORE and only when it has been read. The timing log cannot
      # supply them: a chunk the ledger had already seen does no work, so its candidate count is
      # zero and the per-document rate would fall by however much of the run was resumed.
      Candidates  = if (is.null(.n_cands)) NULL else as.integer(.n_cands),
      CandsPerDoc = if (is.null(.n_cands)) NULL else round(.n_cands / sum(tab_$nDocs), 1),
      CorpusMCand = if (is.null(.n_cands)) NULL else {
        round(.n_cands / sum(tab_$nDocs) * .n_corpus / 1e6, 1)
      }
    )
  )
  cli::cli_alert_info(
    "Extraction only; resolution is 04E and costs a fraction of this. CorpusDocs is the tree \\
     before any limit, so the projection means the same thing whether this run was a rehearsal or \\
     the release."
  )
  if (sum(tab_$nDocs) < .n_corpus) {
    cli::cli_alert_warning(
      "Projected from {sum(tab_$nDocs)} of {(.n_corpus)} documents. The draw is random across the \\
       whole tree, so it is unbiased on document COUNT -- but cost follows length, and 04A found \\
       the corpus tail longer than the labelled sample's. Read this as a floor."
    )
  }
  invisible(tab_)
}


