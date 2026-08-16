# 04C-EntityCorpus: run the chosen engines over the whole Exhibit-10 corpus -------------------------------------------
#
# WHAT THIS FILE DOES
# 04A extracted nine engines over 4,398 labelled contracts and produced the evidence for choosing
# between them. This file runs the chosen ones over ~1.46 million documents and writes their spans
# to a store with the same schema. It resolves nothing and selects nothing.
#
# WHY IT CAN RUN BEFORE THE RULES ARE SETTLED
# Because extraction is rule-independent. Every candidate carries its offsets into the canonical
# text, every engine reads the whole document, and every window a rule might impose is a WHERE
# clause over what is already stored. A rule decided in six weeks costs a query; a rule decided
# before extraction and then changed costs the pass. That asymmetry is the whole argument for
# running this now and arguing about 04B in parallel.
#
# THE ENGINE SET IS DECLARED HERE, NOT READ FROM A POLICY FILE
# The previous version read extraction_policy.parquet, written by a 04B that measured engines and
# crowned them. Engine evidence now lives in 04A and the choice is an argument made in prose, so the
# set is written out in the runbook where a reader can see it beside the reasoning. One fewer
# artifact, one fewer thing to be stale.
#
# THE LEDGER IS WHAT MAKES A TWO-DAY PASS SURVIVABLE
# Resumption is not a checkpoint file. ner_run() records a document against an engine when its rows
# are written, so a pass killed at any point resumes by asking the store what is missing. That also
# means ADDING AN ENGINE LATER IS INCREMENTAL: spaCy is absent from this set, so persons are not
# extracted, and adding the transformer afterwards costs one transformer pass and touches nothing
# already written.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .dir_corpus <- .lP$Input$DirCorpus
  .db_path    <- .lP$Store$NerDB
  .plan       <- tab_plan
}


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
#' performs was missing: money moved from the transformer to the regex arm, the engine set and the
#' throughput knobs were updated, and the extractor dispatch was not -- so the plan named an engine
#' nothing could run and a rehearsal aborted three combinations into the first chunk.
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
      "The plan names {length(bad_)} engine{?s} this document cannot run: {bad_}.",
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
  cli::cli_alert_success("Plan checked: {nrow(.plan)} engine{?s} dispatchable.")
  invisible(.plan)
}


# 3. The store fingerprint -------------------------------------------------------------------------------------------
# The one guard that cannot be recovered after the fact. See ent_corpus_manifest().


#' The declared plan as a per-combination label table
#'
#' The same shape 04A's ent_labels_resolved() produces, so the manifest comparison and the
#' relabelling clear are one mechanism across both documents rather than two that drift.
#'
#' @param .plan Tibble with Combo and a Labels list column.
#' @return Tibble: Combo, Labels -- comma-joined and sorted, so it compares as a string.
ent_plan_resolved <- function(.plan) {
  if (FALSE) .plan <- tab_plan

  tibble::tibble(
    Combo  = .plan$Combo,
    Labels = purrr::map_chr(.plan$Labels, \(.x) paste(sort(.x), collapse = ","))
  ) |>
    dplyr::arrange(.data$Combo)
}


#' Fingerprint a named label list as one comparable string
#'
#' @param .labels Named list of character vectors, keyed on the combination token.
#' @return Character scalar: "combo=labels | combo=labels".
ent_labels_string <- function(.labels) {
  if (FALSE) .labels <- lst_labels

  if (is.null(.labels) || length(.labels) == 0L) return(NA_character_)
  keys_ <- sort(names(.labels))
  paste0(keys_, "=", purrr::map_chr(keys_, \(.k) paste(sort(.labels[[.k]]), collapse = ",")),
         collapse = " | ")
}


#' What the store already holds, before anything is run
#'
#' THE FIRST THING A READER OF A TWO-DAY PASS NEEDS, and its absence caused real confusion: a second
#' render found every document already extracted, so the loop had nothing to do and printed nothing,
#' which is indistinguishable from a loop that is broken. Silence is a bad way to say "finished".
#'
#' Reports per engine because the ledger is per engine, and a document counts as complete only when
#' every declared engine has seen it -- adding an engine to the plan makes every document in the
#' store incomplete again, correctly, and this is where that becomes visible rather than surprising.
#'
#' The remaining-time estimate uses the rate already measured in the timing log rather than a
#' constant, so it sharpens as the pass proceeds and says so when there is nothing to go on yet.
#'
#' @param .db_path The corpus candidate store.
#' @param .n_corpus Integer. Documents in the corpus, from ent_corpus_n().
#' @param .run Character. Combination tokens the plan declares.
#' @param .dir Directory holding the _timing log, or NULL to skip the estimate.
#' @return Invisibly, a tibble of the per-engine figures.
ent_store_status <- function(.db_path, .n_corpus, .run, .dir = NULL) {
  if (FALSE) {
    .db_path  <- .lP$Store$NerDB
    .n_corpus <- ent_corpus_n(tab_index)
    .run      <- tab_plan$Combo
    .dir      <- .dir_store
  }

  cli::cli_h2("Store status before this run")

  if (!fs::file_exists(.db_path)) {
    cli::cli_alert_info("No store yet. All {(.n_corpus)} corpus document{?s} are to be extracted.")
    return(invisible(tibble::tibble()))
  }

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  combo_ <- "CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END"
  per_ <- DBI::dbGetQuery(con_, paste0(
    "SELECT ", combo_, " AS Combo, COUNT(DISTINCT DocID) AS DocsDone, ",
    "  SUM(CASE WHEN Status = 'timeout' THEN 1 ELSE 0 END) AS Timeouts FROM runs GROUP BY 1"
  )) |>
    tibble::as_tibble()
  cand_ <- DBI::dbGetQuery(con_, paste0(
    "SELECT ", combo_, " AS Combo, COUNT(*) AS Candidates FROM candidates GROUP BY 1"
  )) |>
    tibble::as_tibble()

  in_ <- paste0("('", paste(.run, collapse = "','"), "')")
  done_all_ <- DBI::dbGetQuery(con_, paste0(
    "SELECT COUNT(*) AS N FROM (SELECT DocID FROM runs WHERE ", combo_, " IN ", in_,
    " GROUP BY DocID HAVING COUNT(DISTINCT ", combo_, ") >= ", length(.run), ")"
  ))$N

  out_ <- tibble::tibble(Combo = sort(unique(c(.run, per_$Combo)))) |>
    dplyr::left_join(per_, by = "Combo") |>
    dplyr::left_join(cand_, by = "Combo") |>
    dplyr::mutate(
      Declared   = .data$Combo %in% .run,
      DocsDone   = dplyr::coalesce(as.integer(.data$DocsDone), 0L),
      Timeouts   = dplyr::coalesce(as.integer(.data$Timeouts), 0L),
      Candidates = dplyr::coalesce(as.integer(.data$Candidates), 0L),
      PctCorpus  = .data$DocsDone / .n_corpus
    ) |>
    dplyr::select(Combo, Declared, DocsDone, PctCorpus, Timeouts, Candidates)

  tbl_say(.tab = dplyr::mutate(out_, PctCorpus = tbl_pct(.data$PctCorpus, 2L)))

  pending_ <- .n_corpus - done_all_
  cli::cli_alert_info(
    "{done_all_} of {(.n_corpus)} document{?s} complete on all {length(.run)} declared engine{?s}; \\
     {pending_} pending."
  )

  # The estimate uses what has actually been measured here rather than a constant.
  if (!is.null(.dir) && pending_ > 0L) {
    logs_ <- fs::dir_ls(fs::path(.dir, "_timing"), glob = "*.parquet", fail = FALSE)
    if (length(logs_) > 0L) {
      tim_ <- purrr::map(logs_, arrow::read_parquet) |> purrr::list_rbind()
      new_ <- if ("nNew" %in% names(tim_)) sum(tim_$nNew) else sum(tim_$nDocs)
      if (new_ > 0L && sum(tim_$Seconds) > 0) {
        rate_ <- new_ / sum(tim_$Seconds)
        cli::cli_alert_info(
          "At the {round(rate_, 1)} doc/s measured so far: {round(pending_ / rate_ / 3600, 1)}h \\
           remaining, finishing about {format(Sys.time() + pending_ / rate_, '%a %d %b %H:%M')}."
        )
      }
    } else {
      cli::cli_alert_info("No timings yet; the first chunk will produce an estimate.")
    }
  }
  if (any(!out_$Declared)) {
    cli::cli_alert_warning(
      "The store holds {out_$Combo[!out_$Declared]}, which this run does not declare. Those rows \\
       are left alone; they neither count towards completeness nor get extended."
    )
  }
  invisible(out_)
}

#' The documents that still need extracting
#'
#' ASKED ONCE, UP FRONT, RATHER THAN ONCE PER CHUNK. ner_run() already consults the ledger and skips
#' what it has seen, so a resumed run was correct without this -- but it was correct expensively.
#' Every chunk first read its documents' TEXT off disk and wrote a parquet, and only then discovered
#' there was nothing to extract. On a corpus re-render that is 1.46 million file reads to establish
#' that the work is already done.
#'
#' Asking here instead makes the remaining work the thing that gets chunked, which has three
#' consequences beyond the saving. Progress is against a denominator that means something. The
#' throughput rate stops being distorted by chunks that did nothing. And the document reports what
#' is left before it starts, which is what a reader of a two-day run wants to know first.
#'
#' PENDING IS PER DOCUMENT, NOT PER COMBINATION, because the text is read once and handed to every
#' engine. A document one engine has not seen must be read whatever the other four have done. The
#' ledger row is what counts as seen, whatever its status: a timeout was tried, and ner_run() only
#' revisits one when asked with .retry_timeout.
#'
#' @param .db_path The corpus candidate store. Absent means everything is pending.
#' @param .index Tibble from ent_corpus_index().
#' @param .run Character. Combination tokens the plan declares.
#' @return .index filtered to pending documents, with its corpus attributes preserved.
ent_pending <- function(.db_path, .index, .run, .labels) {
  if (FALSE) {
    .db_path <- .lP$Store$NerDB
    .index   <- tab_index
    .run     <- tab_plan$Combo
    .labels  <- lst_labels
  }

  # ATTRIBUTES DO NOT SURVIVE A FILTER. NCorpus and BytesCorpus are carried on the index and dplyr
  # drops them, so the fingerprint and the projection would both silently lose their denominator.
  keep_ <- attributes(.index)[c("NCorpus", "BytesCorpus")]
  restore_ <- function(.t) {
    attr(.t, "NCorpus")     <- keep_$NCorpus
    attr(.t, "BytesCorpus") <- keep_$BytesCorpus
    .t
  }

  if (!fs::file_exists(.db_path)) {
    cli::cli_alert_info("No store yet: all {nrow(.index)} document{?s} pending.")
    return(restore_(.index))
  }

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  # COMPLETE MEANS EVERY DECLARED COMBINATION AND LABEL, not every combination. The ledger is keyed
  # on the label as well, so a document that has been through LexNLP for three of its four labels
  # is not done -- and counting combinations alone would call it done and never extract the fourth.
  # The expected pair count is the sum over combinations of the labels each was asked for.
  n_pairs_ <- sum(purrr::map_int(.labels[.run], length))
  in_ <- paste0("('", paste(.run, collapse = "','"), "')")
  done_ <- DBI::dbGetQuery(con_, paste0(
    "SELECT DocID FROM ( ",
    "  SELECT DocID, COUNT(*) AS NPairs ",
    "  FROM (SELECT DISTINCT DocID, Engine, Model, Label FROM runs ",
    "        WHERE (CASE WHEN Engine = Model THEN Engine ",
    "               ELSE Engine || ':' || Model END) IN ", in_, ") ",
    "  GROUP BY DocID) ",
    "WHERE NPairs >= ", n_pairs_
  ))$DocID

  out_ <- dplyr::filter(.index, !.data$DocID %in% done_)
  n_done_ <- nrow(.index) - nrow(out_)
  if (n_done_ > 0L) {
    cli::cli_alert_info(
      "{n_done_} document{?s} already complete in the store; {nrow(out_)} pending."
    )
  } else {
    cli::cli_alert_info("Nothing in the store yet for this set: {nrow(out_)} pending.")
  }
  restore_(out_)
}

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
    # PER ENGINE, not a flattened union. A union cannot distinguish "LexNLP was asked for GPE" from
    # "the gazetteer was asked for GPE", so adding a label to one engine would leave the fingerprint
    # unchanged and the store would report itself complete under a set it was never built with.
    # That is exactly how PERSON went missing from the sample store for a full pass.
    Labels     = ent_labels_string(.labels = .labels),
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

#' A running account of a pass measured in days
#'
#' WRITTEN THREE WAYS, AND NOT THROUGH cli, WHICH IS THE POINT. cli emits a condition through
#' message(), and anything that handles messages holds them: knitr buffers a chunk's messages until
#' the chunk ends, so a loop running for two days shows nothing at all until it is over. That is the
#' opposite of what a progress report is for, and it is why three earlier attempts at this -- a
#' purrr bar, a cli bar, cli alert lines -- all produced silence.
#'
#' cat() to the console bypasses the condition system entirely and flush.console() forces it out
#' immediately, which covers the interactive case. The LOG FILE covers everything else: it is a
#' plain text file appended one line per chunk, so progress can be watched with `tail -f` from
#' another terminal, read after RStudio has been closed, or checked on a machine that is running
#' the pass headless. On a two-day run that is not a convenience, it is the only way to know the
#' thing is alive without touching the session.
#'
#' sprintf rather than glue or cli interpolation: no dot-literal rules, no evaluation environment to
#' get wrong, no dependency on a package's formatting decisions in the one function whose job is to
#' still work when other things are not.
#'
#' @param .n_chunks Integer. Chunks in this pass.
#' @param .n_docs Integer. Documents still to extract.
#' @param .path_log Character or NULL. Plain text log appended one line per chunk.
#' @return An environment to hand to ent_progress_step().
ent_progress_new <- function(.n_chunks, .n_docs, .path_log = NULL) {
  if (FALSE) {
    .n_chunks <- length(chunks_)
    .n_docs   <- nrow(tab_todo)
    .path_log <- .lP$Store$Progress
  }

  e_ <- new.env(parent = emptyenv())
  e_$NChunks <- as.integer(.n_chunks)
  e_$NDocs   <- as.integer(.n_docs)
  e_$Chunk   <- 0L
  e_$Docs    <- 0L
  e_$Seconds <- 0
  e_$Log     <- .path_log

  head_ <- sprintf(
    "== %s | %d chunk(s), %d document(s) to extract ==",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), e_$NChunks, e_$NDocs
  )
  cat(head_, "\n", sep = "", file = stdout())
  utils::flush.console()
  if (!is.null(.path_log)) {
    fs::dir_create(fs::path_dir(.path_log))
    cat(head_, "\n", sep = "", file = .path_log, append = TRUE)
    cat("Watch it with: tail -f ", as.character(.path_log), "\n", sep = "", file = stdout())
    utils::flush.console()
  }
  e_
}

#' Report one chunk, and what it implies for the rest
#'
#' The rate is cumulative rather than per chunk, so a single slow chunk -- a run of long documents,
#' a LexNLP stall -- moves the estimate rather than dominating it.
#'
#' @param .progress Environment from ent_progress_new().
#' @param .n_new Documents this chunk actually extracted.
#' @param .seconds Wall-clock seconds the chunk took.
#' @return Invisibly, the environment.
ent_progress_step <- function(.progress, .n_new, .seconds) {
  if (FALSE) {
    .progress <- prog_
    .n_new    <- 2000L
    .seconds  <- 264
  }

  .progress$Chunk   <- .progress$Chunk + 1L
  # Coerced and guarded. A zero-length increment turns the accumulator into integer(0) and every
  # arithmetic downstream inherits it silently, so the failure surfaces at whichever comparison
  # touches it first rather than where the value came from.
  n_new_ <- as.integer(.n_new)
  if (length(n_new_) != 1L || is.na(n_new_)) n_new_ <- 0L
  .progress$Docs    <- .progress$Docs + n_new_
  .progress$Seconds <- .progress$Seconds + as.numeric(.seconds)

  rate_  <- .progress$Docs / max(.progress$Seconds, 1e-9)
  left_  <- max(.progress$NDocs - .progress$Docs, 0L)
  eta_h_ <- if (rate_ > 0) left_ / rate_ / 3600 else NA_real_
  eta_at_ <- if (is.finite(eta_h_)) format(Sys.time() + left_ / rate_, "%a %d %b %H:%M") else "?"

  line_ <- sprintf(
    "[%s] chunk %d/%d | %s doc in %ds | %.1f doc/s | %s left | %.1fh | ETA %s",
    format(Sys.time(), "%H:%M:%S"),
    .progress$Chunk, .progress$NChunks,
    format(n_new_, big.mark = ","), round(as.numeric(.seconds)),
    rate_, format(left_, big.mark = ","), eta_h_, eta_at_
  )

  cat(line_, "\n", sep = "", file = stdout())
  utils::flush.console()
  if (!is.null(.progress$Log)) cat(line_, "\n", sep = "", file = .progress$Log, append = TRUE)
  invisible(.progress)
}

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
    # THE SAME COLUMNS AS THE NORMAL RETURN, and the omission of nNew here cost a crash at the very
    # end of the corpus pass. Every document in the final chunk was unreadable, this branch fired,
    # the caller read out_$nNew as NULL, as.integer(NULL) gave integer(0), and the progress
    # accumulator became zero-length -- which surfaced three lines later as "argument is of length
    # zero" from a comparison that had nothing to do with the cause.
    #
    # A function with two exits owes them the same shape.
    cli::cli_alert_warning(
      "Every document in this chunk was unreadable or empty; nothing to extract. These stay \\
       pending on every render, because a document that cannot be read cannot be recorded as done."
    )
    return(invisible(tibble::tibble(
      nDocs   = 0L,
      nNew    = 0L,
      Seconds = as.numeric(difftime(Sys.time(), t0_, units = "secs"))
    )))
  }

  fs::dir_create(.dir_work)
  path_ <- fs::file_temp(pattern = "corpus_", tmp_dir = .dir_work, ext = "parquet")
  arrow::write_parquet(tibble::tibble(DocID = docs_$DocID, TextRaw = docs_$Text), path_)
  on.exit(if (fs::file_exists(path_)) fs::file_delete(path_), add = TRUE)

  knob_ <- function(.k) {
    v_ <- purrr::map(.knobs, .k) |> purrr::compact()
    if (length(v_) == 0L) NULL else v_
  }

  run_ <- ner_run(
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
    # THE EXTRACTORS' OWN BARS ARE OFF, and this is what makes the loop's bar readable. Each Python
    # extractor writes a tqdm bar to stderr; five engines per chunk over hundreds of chunks is five
    # bars per chunk, all writing carriage returns to the same line cli is drawing the pass on. The
    # inner bars report one extractor on 2,000 documents, which is the wrong unit anyway -- what a
    # reader of a two-day run needs is how far through the corpus it is.
    # The live bar stays ON. It rides the subprocess streams, which inherit the terminal and never
    # reach the rendered document, so it costs nothing there and is the only way to watch a pass
    # that runs for a day. The cli chatter stays OFF: that goes through the message stream, and at
    # 730 chunks by five engines it is three and a half thousand lines of "done in 4.2s".
    .no_progress   = FALSE,
    .quiet         = TRUE
  )

  # DOCUMENTS IN THE CHUNK AND DOCUMENTS ACTUALLY EXTRACTED ARE DIFFERENT NUMBERS, and conflating
  # them corrupts the projection in the flattering direction. A chunk the ledger already covers
  # returns in seconds while still reporting its full document count, so a rate taken as
  # nDocs / Seconds over a partly resumed run overstates throughput by whatever share was resumed --
  # a rehearsal continued from an earlier one reported nine documents a second where the true figure
  # was seven and a half, and forty-three hours where it was fifty-four.
  #
  # nNew is the largest number of documents any single engine had to process. Not the sum, because
  # five engines over the same document is one document's worth of reading; not the minimum, because
  # an engine that had already finished says nothing about the four that had not.
  n_new_ <- if (is.null(run_) || nrow(run_) == 0L) 0L else max(as.integer(run_$Docs), 0L)

  invisible(tibble::tibble(
    nDocs   = nrow(docs_),
    nNew    = n_new_,
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
ent_write_timing <- function(.dir, .chunk, .n_docs, .n_new, .seconds) {
  if (FALSE) {
    .dir     <- .dir_store
    .chunk   <- 1L
    .n_docs  <- 2000L
    .n_new   <- 2000L
    .seconds <- 233
  }
  dir_ <- fs::path(.dir, "_timing")
  fs::dir_create(dir_)
  path_ <- fs::path(dir_, sprintf("timing_%04d.parquet", as.integer(.chunk)))
  arrow::write_parquet(
    tibble::tibble(
      Chunk = as.integer(.chunk), nDocs = as.integer(.n_docs), nNew = as.integer(.n_new),
      Seconds = as.numeric(.seconds), WrittenAt = Sys.time()
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

  # Per document of NEW work. See ent_extract_corpus_chunk(): a resumed chunk returns its full
  # document count in a handful of seconds, so nDocs here would flatter the projection by whatever
  # share of the run was already in the ledger.
  n_new_  <- if ("nNew" %in% names(tab_)) sum(tab_$nNew) else sum(tab_$nDocs)
  n_seen_ <- sum(tab_$nDocs)
  if (n_new_ == 0L) {
    cli::cli_alert_info("Every chunk was already in the ledger; nothing to time.")
    return(invisible(tab_))
  }
  rate_ <- n_new_ / sum(tab_$Seconds)
  cli::cli_h2("Measured throughput")
  tbl_say(
    .tab = tibble::tibble(
      Chunks      = nrow(tab_),
      Documents   = n_seen_,
      Extracted   = n_new_,
      Minutes     = round(sum(tab_$Seconds) / 60, 1),
      DocsPerSec  = round(rate_, 1),
      CorpusDocs  = as.integer(.n_corpus),
      FullPassHrs = round(.n_corpus / rate_ / 3600, 1),
      FullPassDay = round(.n_corpus / rate_ / 3600 / 24, 1),
      # Candidate figures come from the STORE and only when it has been read. The timing log cannot
      # supply them: a chunk the ledger had already seen does no work, so its candidate count is
      # zero and the per-document rate would fall by however much of the run was resumed.
      Candidates  = if (is.null(.n_cands)) NULL else as.integer(.n_cands),
      CandsPerDoc = if (is.null(.n_cands)) NULL else round(.n_cands / n_seen_, 1),
      CorpusMCand = if (is.null(.n_cands)) NULL else {
        round(.n_cands / n_seen_ * .n_corpus / 1e6, 1)
      }
    )
  )
  cli::cli_alert_info(
    "Extraction only; resolution is 04E and costs a fraction of this. CorpusDocs is the tree \\
     before any limit, so the projection means the same thing whether this run was a rehearsal or \\
     the release."
  )
  if (n_new_ < n_seen_) {
    cli::cli_alert_info(
      "{n_seen_ - n_new_} document{?s} in these chunks were already in the ledger. The rate is per \\
       document EXTRACTED, so the projection is unaffected by how much was resumed."
    )
  }
  if (n_seen_ < .n_corpus) {
    cli::cli_alert_warning(
      "Projected from {n_new_} newly extracted of {(.n_corpus)} documents. The draw is random across the \\
       whole tree, so it is unbiased on document COUNT -- but cost follows length, and 04A found \\
       the corpus tail longer than the labelled sample's. Read this as a floor."
    )
  }
  invisible(tab_)
}


