# 03F-ClassifyApply: labelling the corpus (app_*) ------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# Every decision has been made. 03B fitted a transformer per task and per context length and crowned
# one of each; 03C published a keyword table per task and marked one; 03E measured what combining
# them would buy and found the answer was nothing. This file makes none of those decisions again. It
# reads what those stages deployed and runs it over the corpus.
#
# THE STORE IS THE DESIGN
# A pass over a million documents does not complete in one sitting and must not start from zero when
# it fails at ninety per cent. Everything lands in one DuckDB, scoped by a run key, and three
# questions can be asked of it at any moment: what is done, what is missing, and what has changed
# since the rows already there were written. The third is the one that is easy to leave out and the
# one that silently corrupts a released dataset -- a table holding two context lengths under one
# name looks exactly like a table holding one.
#
# THE RUN KEY IS THE RECIPE, NOT THE ARTIFACT
# It hashes the task, the configuration name and the context length. Those determine the weights
# given the fixed seed upstream, so a byte-identical refit does not throw away three hours of work.
# The artifact path and its fingerprint are RECORDED rather than keyed, so a checkpoint swapped by
# hand surfaces in the drift report instead of being silently honoured or silently discarded.
#
# ONE KEY PER TASK, NOT ONE PER PASS
# Tasks finish independently. Keying the whole pass would mean a crown moving on one task discards
# the two that were already done.
#
# WHERE THE ENGINE BOUNDARY SITS, AND WHY IT SITS THERE
# The transformer emits finished predictions, because a class is what it was trained to produce. The
# keyword engine does NOT: it emits which terms occurred, and the decision rule that turns terms into
# a class lives once, in R, and is called identically by the stage that measured the table's
# precision and by this one. A second decision rule in a second language is what once put a
# hand-written substring search in front of a lexicon mined under stopword removal -- the table
# committed on 36% of the corpus where it had published 52%, and nothing anywhere reported a fault.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; if (FALSE) dev blocks; pure ASCII; parenthesised cli interpolation.

if (FALSE) {
  .con  <- app_connect(.path = .lP$Store$Path)
  .task <- "ClassDetailed"
}


# 1. The store -----------------------------------------------------------------------------------------------------------
# Six tables in one file. The corpus index lives here rather than in a parquet cache, which is what
# makes "which documents still need labelling" a single anti-join instead of a set operation across
# two storage layers.

#' Open the label store, creating its schema on first use
#'
#' Every table is created if absent and left alone otherwise, so opening an existing store is not a
#' destructive act and the same call serves the first run and the hundredth.
#'
#' @param .path Character. Path to the DuckDB file.
#' @param .quiet Logical. Silence the engine's own progress bar.
#' @return A DBI connection.
app_connect <- function(.path, .quiet = TRUE) {
  if (FALSE) {
    .path  <- .lP$Store$Path
    .quiet <- TRUE
  }
  fs::dir_create(fs::path_dir(.path))
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = .path)

  # THE ENGINE DRAWS ITS OWN PROGRESS BAR, and it draws it over ours. Any query long enough to
  # deserve one -- the anti-join over a million rows that decides what is still outstanding -- writes
  # a bar to the terminal with carriage returns, and the elapsed-and-ETA line the pass emits is
  # overwritten by it. What survives in a log is a row of blank space where a progress line should be,
  # which reads as a run that has gone quiet.
  #
  # Two spellings, because the setting was renamed between DuckDB versions and this file should not
  # care which one is installed. A version recognising neither is left with its bar rather than
  # refused a connection: a cosmetic setting must not be able to stop the store from opening.
  if (isTRUE(.quiet)) {
    for (.sql in c("SET enable_progress_bar = false", "PRAGMA disable_progress_bar")) {
      try(DBI::dbExecute(con_, .sql), silent = TRUE)
    }
  }

  # The corpus table carries the register's own columns rather than a directory listing's. Classify
  # is the deduplication and the population filter resolved once, at load, so every later query is a
  # filter rather than a re-derivation; InPopulation records membership for the copy itself, which
  # differs from Classify wherever an attachment's primary sits outside the population.
  DBI::dbExecute(con_, "CREATE TABLE IF NOT EXISTS corpus (
      DocID VARCHAR PRIMARY KEY, HashDocument VARCHAR, Path VARCHAR, DocType VARCHAR, YQ VARCHAR,
      PrimaryFiler BOOLEAN, FilerCopiesAgree BOOLEAN,
      DescSample BOOLEAN, EstiSample BOOLEAN, InPopulation BOOLEAN, Classify BOOLEAN)")

  # One row per run key. This is what makes drift reportable: the configuration behind rows already
  # written is recorded beside them, so a changed setting can be named rather than merely detected.
  DBI::dbExecute(con_, "CREATE TABLE IF NOT EXISTS runs (
      RunKey VARCHAR PRIMARY KEY, Engine VARCHAR, Task VARCHAR, ConfigName VARCHAR,
      MaxLen INTEGER, NWords INTEGER, Tau DOUBLE, Artifact VARCHAR, Fingerprint VARCHAR,
      StartedAt TIMESTAMP, UpdatedAt TIMESTAMP)")

  DBI::dbExecute(con_, "CREATE TABLE IF NOT EXISTS bert_labels (
      RunKey VARCHAR, DocID VARCHAR, Top1Class VARCHAR, Top1Prob DOUBLE,
      Top2Class VARCHAR, Top2Prob DOUBLE)")

  # The evidence trail, and the reason the decision rule can be revised without re-reading a million
  # documents: a changed threshold is a re-run of kw_decide() over these rows rather than a re-run of
  # the lexicon over the corpus.
  DBI::dbExecute(con_, "CREATE TABLE IF NOT EXISTS keyword_hits (
      RunKey VARCHAR, DocID VARCHAR, Term VARCHAR, Class VARCHAR, Power DOUBLE)")

  DBI::dbExecute(con_, "CREATE TABLE IF NOT EXISTS keyword_labels (
      RunKey VARCHAR, DocID VARCHAR, Top1Class VARCHAR, Top1Prob DOUBLE,
      Top2Class VARCHAR, Top2Prob DOUBLE, TopTerm VARCHAR)")

  # Timing runs, kept apart from the labels they never write. A benchmark exists to be comparable
  # across renders, so it is keyed on the parameters it varied and skipped once measured; and it
  # exists to price combinations that will never ship, so its predictions are discarded rather than
  # stored. Writing them would half-populate a run key nobody asked for and report it as partial.
  DBI::dbExecute(con_, "CREATE TABLE IF NOT EXISTS benchmarks (
      BenchKey VARCHAR PRIMARY KEY, Engine VARCHAR, Task VARCHAR, ConfigName VARCHAR,
      MaxLen INTEGER, BatchSize INTEGER, nDocs INTEGER, Seconds DOUBLE, DocsPerSecond DOUBLE,
      MeasuredAt TIMESTAMP)")

  # RETRYABLE BY CONSTRUCTION. A failure is recorded with its reason and excluded from the current
  # pass, and the next pass tries again: a file missing because a mount hiccuped must not be
  # blacklisted forever, and a file that is genuinely empty simply reappears in the report, which is
  # honest rather than tidy.
  DBI::dbExecute(con_, "CREATE TABLE IF NOT EXISTS failures (
      RunKey VARCHAR, DocID VARCHAR, Reason VARCHAR, SeenAt TIMESTAMP)")

  con_
}

#' The key identifying one engine, one task, one configuration
#'
#' Hashes the recipe rather than the artifact. Two runs of the same configuration share a key and
#' therefore resume each other; a different context length is a different key and cannot pool with
#' what came before.
#'
#' @param .engine Character. "bert" or "keyword".
#' @param .task Character. Label column.
#' @param .config Character. Configuration name written upstream.
#' @param .window Integer. Context length for the transformer, truncation window for the lexicon.
#' @return Character. A twelve-character key.
app_run_key <- function(.engine, .task, .config, .window) {
  if (FALSE) {
    .engine <- "bert"
    .task   <- "ClassDetailed"
    .config <- "ClassDetailed__nlpaueb-legal-bert-base-uncased__TText_L256_E6_B32_LR2e-05_W1_S42"
    .window <- 256L
  }
  substr(digest::digest(paste(.engine, .task, .config, as.integer(.window), sep = "|"),
                        algo = "xxhash64"), 1L, 12L)
}

#' A cheap fingerprint of the artifact behind a run
#'
#' Size and modification time of every file in the directory, hashed. Not part of the key, because a
#' refit that reproduces the same weights should not invalidate a finished pass. Recorded, because a
#' checkpoint replaced by hand should not pass unnoticed either.
#'
#' @param .path Character. File or directory.
#' @return Character, or NA where the path is absent.
app_fingerprint <- function(.path) {
  if (FALSE) .path <- "2_output/03B/model_final/x__FINAL/model"
  if (!fs::file_exists(.path) && !fs::dir_exists(.path)) return(NA_character_)
  fils_ <- if (fs::dir_exists(.path)) fs::dir_ls(.path, recurse = TRUE, type = "file") else .path
  if (length(fils_) == 0L) return(NA_character_)
  info_ <- fs::file_info(fils_)
  substr(digest::digest(paste(fs::path_file(fils_), info_$size, info_$modification_time,
                              collapse = "|"), algo = "xxhash64"), 1L, 12L)
}

#' Record, or update, the configuration behind a run key
#'
#' Written before any label, so an interrupted pass still leaves behind a statement of what it was
#' doing. The upsert is what lets a resumed pass refresh the fingerprint without losing the start
#' time.
#'
#' @param .con Connection.
#' @param .spec One-row tibble carrying RunKey, Engine, Task, ConfigName and the window columns.
#' @return Invisibly .spec.
app_run_register <- function(.con, .spec) {
  if (FALSE) {
    .con  <- con
    .spec <- spec_bert[1, ]
  }
  now_ <- Sys.time()
  old_ <- DBI::dbGetQuery(.con, "SELECT RunKey, StartedAt FROM runs WHERE RunKey = ?",
                          params = list(.spec$RunKey))
  row_ <- tibble::tibble(
    RunKey      = .spec$RunKey,
    Engine      = .spec$Engine,
    Task        = .spec$Task,
    ConfigName  = .spec$ConfigName,
    MaxLen      = if ("MaxLen" %in% names(.spec)) as.integer(.spec$MaxLen) else NA_integer_,
    NWords      = if ("NWords" %in% names(.spec)) as.integer(.spec$NWords) else NA_integer_,
    Tau         = if ("Tau" %in% names(.spec)) as.numeric(.spec$Tau) else NA_real_,
    Artifact    = .spec$Artifact,
    Fingerprint = app_fingerprint(.path = .spec$Artifact),
    StartedAt   = if (nrow(old_) == 1L) old_$StartedAt[[1]] else now_,
    UpdatedAt   = now_
  )
  DBI::dbExecute(.con, "DELETE FROM runs WHERE RunKey = ?", params = list(.spec$RunKey))
  DBI::dbAppendTable(.con, "runs", row_)
  invisible(.spec)
}

#' Load the corpus index into the store, walking the tree only once
#'
#' A million paths is a slow walk and an unchanging one between renders, so it is done when the table
#' is empty and skipped otherwise. `.reload` forces it, which is what a changed register needs.
#'
#' @param .con Connection.
#' @param .dir_corpus Character. Root of the parsed-contract tree.
#' @param .path_cache Character. Parquet the walk is cached to.
#' @param .reload Logical. Rebuild from the register even if the table holds rows.
#' @return Invisibly the number of ATTACHMENTS this population will classify -- not the index
#'   size, which counts every registrant copy. It is the denominator every caller reads against.
app_corpus_load <- function(.con, .path_register, .dir_mirror, .population = "all",
                            .doc_type = "Exhibit10", .reload = FALSE) {
  if (FALSE) {
    .con           <- con
    .path_register <- .lP$Input$Register
    .dir_mirror    <- .lP$Param$DirMirror
    .population    <- "all"
    .reload        <- FALSE
  }
  .population <- match.arg(.population, c("all", "descriptive", "estimation"))

  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS n FROM corpus")$n[[1]]
  if (n_ > 0L && !.reload) {
    cli::cli_alert_info("Corpus index already loaded: {n_} document{?s}.")
    return(invisible(n_))
  }
  if (!fs::file_exists(.path_register)) cli::cli_abort("No register at {.path {(.path_register)}}.")

  # THE REGISTER IS THE GATE, NOT A DIRECTORY WALK. A walk makes a second, independent statement
  # about what is in the corpus, and the two can disagree without either reporting it. The register
  # holds every document with its ladder step attached, so membership and population come from one
  # place. It stores no path: DocTypeMod, YQ and DocID determine one, and utils_doc_path() rebuilds
  # it. Verified across all four document types before this was written.
  reg_ <- arrow::open_dataset(sources = .path_register) |>
    dplyr::select(
      "DocID", "HashDocument", "DocTypeMod", "YQ", "CIK",
      "Removed", "DescSample", "EstiSample",
      "MultFiler", "nCIK", "PrimaryFiler", "FilerCopiesAgree"
    ) |>
    dplyr::filter(.data$DocTypeMod == .doc_type) |>
    dplyr::collect()

  # WHICH COPIES THE POPULATION WANTS, AND WHICH ATTACHMENT ANSWERS FOR THEM.
  #
  # PrimaryFiler is chosen GLOBALLY -- the lowest DocID among copies that passed the quality rules --
  # while EstiSample depends on a per-CIK Compustat match. The two criteria therefore disagree: an
  # attachment can have its primary copy outside the estimation sample and another copy inside it.
  # Measured before this was written: 4,432 such attachments for the estimation sample, and none at
  # all for the descriptive one, which is exactly where the mechanism predicts them -- the descriptive
  # rules are per document, the Compustat match is per registrant.
  #
  # So the rule is: classify the primary of every attachment with AT LEAST ONE copy in the
  # population. Restricting to primaries that are themselves in it would leave those 4,432
  # unlabelled, silently. It also makes the store population-independent, so a pass run for one
  # population is reused verbatim by any other.
  in_pop_ <- switch(
    .population,
    all         = rep(TRUE, nrow(reg_)),
    descriptive = as.logical(reg_$DescSample),
    estimation  = as.logical(reg_$EstiSample)
  )
  in_pop_ <- !is.na(in_pop_) & in_pop_
  hash_want_ <- unique(reg_$HashDocument[in_pop_])

  reg_ <- reg_ |>
    dplyr::mutate(InPopulation = in_pop_) |>
    dplyr::mutate(
      # The row the engine actually reads. One per attachment, by construction: 01C makes exactly one
      # copy primary and that was verified across all 1,189,805 attachments before this was written.
      Classify = as.logical(.data$PrimaryFiler) & .data$HashDocument %in% hash_want_
    ) |>
    dplyr::arrange(.data$DocID)

  DBI::dbExecute(.con, "DELETE FROM corpus")
  DBI::dbAppendTable(
    .con, "corpus",
    reg_ |>
      dplyr::transmute(
        DocID, HashDocument,
        DocType = .data$DocTypeMod,
        YQ      = as.character(.data$YQ),
        Path    = as.character(utils_doc_path(
          .dir_mirror = .dir_mirror,
          .doc_type   = .data$DocTypeMod,
          .yq         = .data$YQ,          # utils_doc_path normalises the register's double form
          .doc_id     = .data$DocID
        )),
        PrimaryFiler     = as.logical(.data$PrimaryFiler),
        FilerCopiesAgree = as.logical(.data$FilerCopiesAgree),
        DescSample       = as.logical(.data$DescSample),
        EstiSample       = as.logical(.data$EstiSample),
        InPopulation     = .data$InPopulation,
        Classify         = .data$Classify
      )
  )

  n_copy_     <- nrow(reg_)
  n_attach_   <- dplyr::n_distinct(reg_$HashDocument)
  n_classify_ <- sum(reg_$Classify)

  cli::cli_alert_success(
    "Corpus index loaded from the register: {format(n_copy_, big.mark = ',')} \\
     {cli::qty(n_copy_)}cop{?y/ies} of {format(n_attach_, big.mark = ',')} \\
     {cli::qty(n_attach_)}attachment{?s}."
  )
  # Attachments against attachments. Reporting the count as a share of COPIES read as a selection when
  # it is not: population "all" wants every attachment, and 81% of copies is the deduplication rate
  # wearing the language of a filter.
  cli::cli_alert_info(
    "Population {.val {(.population)}} wants {format(n_classify_, big.mark = ',')} of them \\
     ({round(100 * n_classify_ / n_attach_)}%). That count -- attachments, not copies -- is the \\
     denominator every progress and cost figure below is read against; the release fans back out to \\
     all {format(n_copy_, big.mark = ',')} copies."
  )
  invisible(n_classify_)
}

#' Documents this run key still has to label
#'
#' The anti-join the store exists for. A document already labelled is excluded; one recorded as
#' failed is excluded from THIS pass and returns to the queue on the next, which is what makes a
#' transient read failure transient.
#'
#' @param .con Connection.
#' @param .run_key Character.
#' @param .table Character. Label table to check against.
#' @param .limit Integer or NULL. Cap for a rehearsal; NULL takes everything outstanding.
#' @return Tibble: DocID, Path.
app_pending <- function(.con, .run_key, .table = "bert_labels", .limit = NULL) {
  if (FALSE) {
    .con     <- con
    .run_key <- "a1b2c3d4e5f6"
    .table   <- "bert_labels"
    .limit   <- 2000L
  }
  # `c.Classify` is the deduplication and the population filter at once: one row per attachment, and
  # only attachments the chosen population wants. ORDER BY DocID is not cosmetic -- it fixes chunk
  # composition, which is what makes a pass reproducible rather than merely repeatable.
  sql_ <- paste0(
    "SELECT c.DocID, c.Path FROM corpus c ",
    "LEFT JOIN (SELECT DISTINCT DocID FROM ", .table, " WHERE RunKey = ?) d ON d.DocID = c.DocID ",
    "LEFT JOIN (SELECT DISTINCT DocID FROM failures WHERE RunKey = ?) f ON f.DocID = c.DocID ",
    "WHERE c.Classify AND d.DocID IS NULL AND f.DocID IS NULL ORDER BY c.DocID"
  )
  if (!is.null(.limit)) sql_ <- paste0(sql_, " LIMIT ", as.integer(.limit))
  tibble::as_tibble(DBI::dbGetQuery(.con, sql_, params = list(.run_key, .run_key)))
}

#' What is done, what is missing, and what has changed
#'
#' Three questions, one table. The third needs the runs table: a key with no rows is indistinguishable
#' from a key that was never started unless the previous configuration for the same task is on record
#' beside it, so drift is reported by naming the field that moved rather than by the absence of rows.
#'
#' @param .con Connection.
#' @param .specs Tibble of the runs this document intends, one row per engine and task.
#' @return Tibble: Engine, Task, RunKey, nDone, nFailed, nPending, Status, Note.
#' What the store actually covers, measured against every population
#'
#' NO POPULATION TAG IS STORED, DELIBERATELY. A tag records what a run INTENDED; this measures what it
#' achieved, and the two diverge exactly when it matters -- an interrupted pass over the whole corpus
#' would carry the tag "all" and be wrong about itself. Coverage cannot lie and adds no state to go
#' stale.
#'
#' Reported per population and per task, on ATTACHMENTS rather than copies, because attachments are
#' what the engine reads and copies are what the release fans out to.
#'
#' @param .con Store connection.
#' @param .specs Run specifications, as built by the document.
#' @return Invisibly a tibble: one row per population and task.
app_coverage <- function(.con, .specs) {
  if (FALSE) {
    .con   <- con
    .specs <- specs
  }
  app_require_con(.con = .con, .what = "report coverage")

  pops_ <- c(all = "TRUE", descriptive = "DescSample", estimation = "EstiSample")

  out_ <- purrr::imap(pops_, function(.where, .pop) {
    # Attachments the population wants: one row per HashDocument with at least one copy in it.
    want_ <- DBI::dbGetQuery(.con, paste0(
      "SELECT COUNT(DISTINCT c.HashDocument) AS n FROM corpus c WHERE ", .where
    ))$n[[1]]

    purrr::map(seq_len(nrow(.specs)), function(.i) {
      sp_  <- .specs[.i, ]
      tbl_ <- if (sp_$Engine == "bert") "bert_labels" else "keyword_labels"
      got_ <- DBI::dbGetQuery(.con, paste0(
        "SELECT COUNT(DISTINCT c.HashDocument) AS n FROM corpus c ",
        "INNER JOIN (SELECT DISTINCT DocID FROM ", tbl_, " WHERE RunKey = ?) l ON l.DocID = c.DocID ",
        "WHERE ", .where
      ), params = list(sp_$RunKey))$n[[1]]
      tibble::tibble(
        Population = .pop, Engine = sp_$Engine, Task = sp_$Task,
        Wanted = want_, Labelled = got_, Share = got_ / want_
      )
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()

  cli::cli_h2("What this store covers")
  out_ |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_out(.title = "Attachments labelled, by population")
  cli::cli_text("")
  cli::cli_alert_info(
    "Read this rather than a filename. A pass interrupted part way through the whole corpus and a \\
     pass run deliberately over one population hold the same documents and differ only in intent; \\
     coverage distinguishes them and a label would not."
  )
  invisible(out_)
}

app_status <- function(.con, .specs) {
  if (FALSE) {
    .con   <- con
    .specs <- dplyr::bind_rows(spec_bert, spec_kw)
  }
  # WHERE Classify, because that is what app_pending() serves. Counting every row in `corpus` would
  # make nPending the copies count while nDone is the attachments count, so a finished pass would sit
  # at 273,134 pending forever and never report itself complete.
  n_corpus_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS n FROM corpus WHERE Classify")$n[[1]]
  prev_     <- tibble::as_tibble(DBI::dbGetQuery(.con, "SELECT * FROM runs"))

  purrr::map(seq_len(nrow(.specs)), function(.i) {
    s_    <- .specs[.i, ]
    tab_  <- if (identical(s_$Engine, "bert")) "bert_labels" else "keyword_labels"
    done_ <- DBI::dbGetQuery(
      .con, paste0("SELECT COUNT(DISTINCT DocID) AS n FROM ", tab_, " WHERE RunKey = ?"),
      params = list(s_$RunKey))$n[[1]]
    fail_ <- DBI::dbGetQuery(
      .con, "SELECT COUNT(DISTINCT DocID) AS n FROM failures WHERE RunKey = ?",
      params = list(s_$RunKey))$n[[1]]

    # A run for the same engine and task under a DIFFERENT key is the previous configuration -- unless
    # the caller intends to keep both, which is what running two context lengths side by side is.
    # Excluding every key in the current specification is what tells a deliberate second run from a
    # changed setting: the first is in the plan, the second is not.
    old_ <- prev_ |>
      dplyr::filter(.data$Engine == s_$Engine, .data$Task == s_$Task,
                    !.data$RunKey %in% .specs$RunKey)
    note_ <- if (nrow(old_) == 0L) {
      ""
    } else {
      purrr::map_chr(seq_len(nrow(old_)), function(.j) {
        o_    <- old_[.j, ]
        diff_ <- c(
          if (!identical(o_$ConfigName, s_$ConfigName)) "configuration",
          if (!isTRUE(o_$MaxLen == s_$MaxLen) && !all(is.na(c(o_$MaxLen, s_$MaxLen)))) {
            paste0("MaxLen ", o_$MaxLen, " -> ", s_$MaxLen)
          },
          if (!isTRUE(o_$NWords == s_$NWords) && !all(is.na(c(o_$NWords, s_$NWords)))) {
            paste0("NWords ", o_$NWords, " -> ", s_$NWords)
          }
        )
        paste0("supersedes ", o_$RunKey, " (", paste(diff_, collapse = ", "), ")")
      }) |>
        paste(collapse = "; ")
    }

    fp_now_ <- app_fingerprint(.path = s_$Artifact)
    fp_old_ <- prev_$Fingerprint[prev_$RunKey == s_$RunKey]
    if (length(fp_old_) == 1L && !is.na(fp_old_) && !identical(fp_old_, fp_now_)) {
      note_ <- paste(c(note_, "ARTIFACT CHANGED under the same key"), collapse = "; ")
    }

    tibble::tibble(
      Engine = s_$Engine, Task = s_$Task, RunKey = s_$RunKey,
      # as.integer, because DBI returns these as doubles and the table formatter then renders a count
      # of zero as "0.000" beside a count of 1,189,805 -- two formats for one kind of quantity.
      nDone    = as.integer(done_),
      nFailed  = as.integer(fail_),
      nPending = as.integer(n_corpus_ - done_ - fail_),
      Status = dplyr::case_when(done_ == 0L                     ~ "not started",
                                done_ + fail_ >= n_corpus_      ~ "complete",
                                TRUE                            ~ "partial"),
      Note = note_
    )
  }) |>
    purrr::list_rbind()
}

#' Report the store's state before anything is run
#' @param .tab Output of app_status().
#' @param .n_corpus Integer. Attachments this population classifies, as returned by
#'   `app_corpus_load()`. NOT the index size: the index holds one row per registrant copy.
#' @return Invisibly .tab.
app_report_status <- function(.tab, .n_corpus) {
  if (FALSE) {
    .tab      <- status
    .n_corpus <- 1.1e6
  }
  tbl_head("The store, before this run")
  tbl_out(
    .tab    = .tab,
    .title  = "The store, before this run",
    .groups = c(" " = 3, "Documents" = 3, " " = 2),
    .notes  = c(
      nPending = paste("Corpus minus done minus failed, for THIS run key. A failure is excluded from",
                       "this pass and returns to the queue on the next, so a transient read error",
                       "does not blacklist a document."),
      Note     = paste("Names the configuration a key supersedes, where one exists. An artifact that",
                       "changed under an UNCHANGED key is the dangerous case and is called out: the",
                       "rows already written came from different weights.")
    )
  )
  drift_ <- .tab |> dplyr::filter(nzchar(.data$Note))
  if (nrow(drift_) > 0L) {
    tbl_note(
      "{nrow(drift_)} run{?s} differ{?s/} from what is already in the store. Rows under a superseded \\
       key are not deleted -- they are simply not read, since every query is scoped by key -- so \\
       nothing is lost and nothing is mixed.",
      .type = "warn"
    )
  }
  tbl_note(
    "{format(.n_corpus, big.mark = ',')} {cli::qty(.n_corpus)}attachment{?s} to classify under this \\
     population. The index holds more -- one row per registrant copy -- and the release fans out to \\
     those; nothing below counts them."
  )
  invisible(.tab)
}


# 2. Reading text --------------------------------------------------------------------------------------------------------

#' Read the text of one chunk of documents
#'
#' Text is read and discarded chunk by chunk rather than held: a million contracts do not fit in
#' memory and nothing below needs them twice. A document whose parquet is missing or empty comes back
#' with NA text and is recorded as a failure rather than stopping the pass.
#'
#' @param .docs Tibble with DocID and Path.
#' @return .docs with a Text column.
app_read_text <- function(.docs, .workers = 1L) {
  if (FALSE) {
    .docs    <- pending[1:10, ]
    .workers <- 24L
  }
  if (nrow(.docs) == 0L) return(dplyr::mutate(.docs, Text = character(0)))

  seq_read_ <- function(.d) dplyr::mutate(.d, Text = purrr::map_chr(.data$Path, clf_read_text))

  # Below the threshold the dispatch costs more than the read saves, and the last chunk of a pass is
  # routinely short.
  if (.workers <= 1L || nrow(.docs) < 500L) return(seq_read_(.docs))

  # ONE BATCH PER WORKER, not one task per document. 5,000 individual tasks would spend more time in
  # dispatch than in arrow. Batches are contiguous and split() orders numeric groups numerically, so
  # unlist() reassembles the original order -- which matters, because the caller pairs Text back onto
  # .docs positionally.
  n_    <- nrow(.docs)
  grp_  <- ceiling(seq_len(n_) / ceiling(n_ / .workers))
  bats_ <- split(.docs$Path, grp_)

  # mirai does NOT throw when a task fails: it RETURNS the error as a miraiError, which is a character
  # scalar carrying a class. So a failed batch arrives looking like one text, and 24 failed batches
  # unlist to a character vector of 24 -- which is exactly how an earlier version reported "24 texts
  # for 5000 documents" while discarding the 24 messages that said why. They are read here instead.
  txt_ <- tryCatch(
    {
      out_ <- mirai::mirai_map(
        .x = bats_,
        .f = function(paths) vapply(paths, clf_read_text, character(1), USE.NAMES = FALSE)
      )[]

      err_ <- which(vapply(out_, \(.r) inherits(.r, "miraiError"), logical(1)))
      if (length(err_) > 0L) {
        cli::cli_alert_danger(
          "{length(err_)} of {length(out_)} batch{?es} failed in the daemons. First message:"
        )
        cli::cli_text("  {as.character(out_[[err_[[1]]]])}")
        NULL
      } else {
        unlist(out_, use.names = FALSE)
      }
    },
    error = function(e) {
      cli::cli_alert_warning(
        "Parallel read failed ({conditionMessage(e)}); falling back to a sequential read for this \\
         chunk. The result is identical, only slower."
      )
      NULL
    }
  )

  # A SHORT RESULT IS A FAILURE, NOT A PARTIAL SUCCESS. Pairing a short Text vector onto .docs would
  # recycle it and label documents with other documents' text, which is worse than being slow and
  # would not announce itself.
  if (is.null(txt_) || length(txt_) != n_) {
    if (!is.null(txt_)) {
      cli::cli_alert_warning(
        "Parallel read returned {length(txt_)} text{?s} for {n_} document{?s}; reading sequentially. \\
         A count matching the BATCH count rather than the document count means the tasks failed and \\
         returned their error messages -- see the line above for the first of them."
      )
    }
    return(seq_read_(.docs))
  }
  dplyr::mutate(.docs, Text = txt_)
}

#' Split a queue into chunks of a given size
#' @param .docs Tibble.
#' @param .size Integer. Documents per chunk.
#' @return List of tibbles.
app_chunks <- function(.docs, .size) {
  if (FALSE) {
    .docs <- pending
    .size <- 5000L
  }
  if (nrow(.docs) == 0L) return(list())
  split(.docs, ceiling(seq_len(nrow(.docs)) / .size)) |> unname()
}


# 3. The transformer -----------------------------------------------------------------------------------------------------

#' The model 03B deployed for one task, at a chosen context length
#'
#' Discovered from the deployment directory rather than rebuilt from a naming convention. 03B fits one
#' model per task AND per context length and names each directory for the configuration it holds, so
#' the set of deployable models is a directory listing joined to the metrics that crowned them.
#'
#' `.max_len` NULL takes the length 03B crowned for the task; an explicit value takes that length
#' instead. The choice belongs to the caller because it is a cost decision -- the longer window is a
#' second full pass over the corpus -- and 03E measures what it buys.
#'
#' @param .dir_bert Character. Output root of the training stage.
#' @param .tab_overall Per-fold metrics from clf_load_overall(), supplying MaxLen and the score.
#' @param .crown Tibble from deployed.parquet, naming what 03B crowned per task.
#' @param .task Character. Label column.
#' @param .max_len Integer or NULL.
#' @return One-row tibble: Task, ConfigName, MaxLen, Artifact, MacroF1, Crowned.
app_pick_model <- function(.dir_bert, .tab_overall, .crown, .task, .max_len = NULL) {
  if (FALSE) {
    .dir_bert    <- .dir_bert
    .tab_overall <- tab_overall
    .crown       <- bert_crown
    .task        <- "ClassDetailed"
    .max_len     <- 256L
  }
  root_ <- fs::path(.dir_bert, "model_final")
  dirs_ <- if (fs::dir_exists(root_)) {
    fs::dir_ls(root_, type = "directory", glob = "*__FINAL")
  } else {
    character(0)
  }
  dirs_ <- dirs_[fs::dir_exists(fs::path(dirs_, "model"))]
  if (length(dirs_) == 0L) cli::cli_abort("No deployed models under {.path {as.character(root_)}}.")

  found_ <- tibble::tibble(
    ConfigName = sub("__FINAL$", "", fs::path_file(dirs_)),
    Artifact   = as.character(fs::path(dirs_, "model"))
  )

  meta_ <- .tab_overall |>
    dplyr::filter(!.data$Smoke, .data$LabelCol == .task) |>
    dplyr::summarise(MaxLen = as.integer(dplyr::first(.data$MaxLen)),
                     MacroF1 = mean(.data$F1_macro), .by = ConfigName)

  cand_ <- found_ |>
    dplyr::inner_join(meta_, by = dplyr::join_by(ConfigName)) |>
    dplyr::mutate(Crowned = .data$ConfigName %in% .crown$ConfigName[.crown$LabelCol == .task])
  if (nrow(cand_) == 0L) cli::cli_abort("No deployed model for {(.task)}.")

  out_ <- if (is.null(.max_len)) {
    cand_ |> dplyr::filter(.data$Crowned)
  } else {
    cand_ |> dplyr::filter(.data$MaxLen == as.integer(.max_len))
  }
  if (nrow(out_) == 0L) {
    cli::cli_abort(c(
      "No deployed model for {(.task)} at the requested context length.",
      "i" = "Available: {paste(cand_$MaxLen, collapse = ', ')}"
    ))
  }
  out_ |>
    dplyr::slice_max(.data$MacroF1, n = 1L, with_ties = FALSE) |>
    dplyr::transmute(Task = .task, ConfigName, MaxLen, Artifact, MacroF1, Crowned)
}

#' Label one chunk with the transformer
#'
#' Shells out to classify_apply.py, the inference companion to the trainer, so the deployed model is
#' applied by the same code the folds were run through. The context length is passed explicitly and is
#' never allowed to default: a model applied at a length it was not fitted under encodes its input
#' differently from its training data, produces perfectly ordinary labels, and says so nowhere.
#'
#' @param .docs Tibble with DocID and Text.
#' @param .artifact Character. Model directory.
#' @param .max_len Integer. Context length the model was fitted at.
#' @param .python,.script Interpreter and classify_apply.py.
#' @param .batch_size Integer.
#' @param .out_dir Character. Scratch directory for the exchange parquets.
#' @return Tibble: DocID, Top1Class, Top1Prob, Top2Class, Top2Prob.
app_bert_chunk <- function(.docs, .artifact, .max_len, .python, .script, .batch_size = 32L,
                           .out_dir = fs::path(tempdir(), "app-bert")) {
  if (FALSE) {
    .docs       <- dplyr::slice_head(pending, n = 100L)
    .artifact   <- pick$Artifact
    .max_len    <- pick$MaxLen
    .batch_size <- 32L
  }
  if (!fs::file_exists(.script)) {
    cli::cli_abort(c("No inference script at {.path {(.script)}}.",
                     "i" = "Expected the companion to classify_train.py."))
  }
  fs::dir_create(.out_dir)
  in_  <- fs::path(.out_dir, "input.parquet")
  out_ <- fs::path(.out_dir, "pred.parquet")
  arrow::write_parquet(.docs |> dplyr::select(DocID, Text), in_)

  res_ <- processx::run(
    command = .python,
    args    = c(.script,
                "--data", in_, "--model-dir", .artifact, "--out", out_,
                "--text-col", "Text", "--id-col", "DocID",
                "--max-len", as.character(as.integer(.max_len)),
                "--batch-size", as.character(as.integer(.batch_size)),
                "--save-probs"),
    error_on_status = FALSE, echo = FALSE
  )
  if (res_$status != 0L || !fs::file_exists(out_)) {
    cli::cli_abort(c("classify_apply.py failed.", "i" = utils::tail(strsplit(res_$stderr, "\n")[[1]], 3)))
  }

  pred_ <- arrow::read_parquet(out_)
  fs::file_delete(c(in_, out_))
  app_top2(.pred = pred_)
}

#' Reduce a probability matrix to the top two classes
#'
#' The full vector is not stored. It costs little and cannot be recovered without re-running
#' inference, but the questions this dataset is for are answered by the label and the runner-up: how
#' confident, and what was the alternative. Storing twelve columns per document to answer two
#' questions is a decision that should be made deliberately rather than by default.
#'
#' @param .pred Output of classify_apply.py, with P_<Class> columns.
#' @return Tibble: DocID, Top1Class, Top1Prob, Top2Class, Top2Prob.
app_top2 <- function(.pred) {
  if (FALSE) .pred <- arrow::read_parquet("pred.parquet")
  pcols_ <- grep("^P_", names(.pred), value = TRUE)
  if (length(pcols_) == 0L) {
    cli::cli_abort("No P_<Class> columns; classify_apply.py must be called with --save-probs.")
  }
  .pred |>
    dplyr::select(DocID, dplyr::all_of(pcols_)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(pcols_), names_to = "Class", values_to = "Prob") |>
    dplyr::mutate(Class = sub("^P_", "", .data$Class)) |>
    dplyr::slice_max(.data$Prob, n = 2L, by = DocID, with_ties = FALSE) |>
    dplyr::mutate(Rank = dplyr::row_number(dplyr::desc(.data$Prob)), .by = DocID) |>
    tidyr::pivot_wider(id_cols = DocID, names_from = "Rank",
                       values_from = c("Class", "Prob"), names_sep = "") |>
    dplyr::transmute(DocID, Top1Class = .data$Class1, Top1Prob = .data$Prob1,
                     Top2Class = .data$Class2, Top2Prob = .data$Prob2)
}


# 4. The keyword table ---------------------------------------------------------------------------------------------------
# Two steps, and the split is deliberate. keyword_apply.py answers which terms occur, because the
# tokenisation is sklearn's analyzer and reproducing it elsewhere is what once put a hand-written
# substring search in front of a mined lexicon. kw_decide() turns terms into a class, because that
# rule has to be the same one 03C measured the table's precision with.

#' The lexicon 03C published for one task, at a chosen truncation window
#'
#' @param .dir_kw Character. Output root of the keyword stage.
#' @param .catalogue The published catalogue.
#' @param .task Character. Label column.
#' @param .n_words Integer or NULL. NULL takes the window 03C marked as default.
#' @return One-row tibble: Task, ConfigName, NWords, Tau, Source, Stopwords, NgramMax, MinTokenLen,
#'   Mode, PositiveClass, Artifact.
app_pick_lexicon <- function(.dir_kw, .catalogue, .task, .n_words = NULL) {
  if (FALSE) {
    .dir_kw    <- .dir_kw
    .catalogue <- kw_cat
    .task      <- "ClassDetailed"
    .n_words   <- NULL
  }
  mine_ <- .catalogue |> dplyr::filter(.data$Task == .task)
  if (nrow(mine_) == 0L) cli::cli_abort("The catalogue lists no table for {(.task)}.")

  out_ <- if (is.null(.n_words)) mine_ |> dplyr::filter(Default) else {
    mine_ |> dplyr::filter(.data$NWords == as.integer(.n_words))
  }
  if (nrow(out_) != 1L) {
    cli::cli_abort(c("No single published table for {(.task)} at the requested window.",
                     "i" = "Available: {paste(mine_$NWords, collapse = ', ')}"))
  }
  out_ |>
    dplyr::transmute(
      Task, ConfigName, NWords = as.integer(NWords), Tau, Source, Stopwords,
      NgramMax = as.integer(NgramMax), MinTokenLen = as.integer(MinTokenLen), Mode, PositiveClass,
      Artifact = as.character(fs::path(.dir_kw, "table",
                                       paste0(kw_table_stem(Task, NWords), ".parquet")))
    )
}

#' Which terms fired, and what the decision rule made of them
#'
#' Returns both, because both are worth keeping: the hits are the evidence trail, and they are what
#' lets a revised threshold be applied by re-running the decision rule over stored rows rather than
#' the lexicon over the corpus.
#'
#' @param .docs Tibble with DocID and Text.
#' @param .spec One row of app_pick_lexicon().
#' @param .classes Character vector of the task's categories.
#' @param .python,.script Interpreter and keyword_apply.py.
#' @return List: Hits (DocID, Term, Class, Power) and Labels (DocID, Top1Class, ...).
app_keyword_chunk <- function(.docs, .spec, .classes, .python, .script) {
  if (FALSE) {
    .docs    <- dplyr::slice_head(pending, n = 100L)
    .spec    <- pick_kw
    .classes <- sort(unique(tab_prep$ClassDetailed))
  }
  lex_ <- arrow::read_parquet(.spec$Artifact)

  hits_ <- kw_hits_engine(
    .lexicon       = lex_,
    .docs          = .docs |> dplyr::mutate(DocDesc = NA_character_),
    .source        = .spec$Source,
    .n_words       = .spec$NWords,
    .stopwords     = .spec$Stopwords,
    .min_token_len = .spec$MinTokenLen,
    .python        = .python,
    .script        = .script
  )

  # The decision rule, called exactly as 03C calls it when it measures the table's precision. A second
  # rule here would be a second answer to the same question, and the published precision would then
  # describe a system nobody runs.
  dec_ <- kw_decide(
    .hits           = hits_,
    .docs           = .docs |> dplyr::select(DocID),
    .classes        = .classes,
    .tau            = .spec$Tau,
    .mode           = .spec$Mode,
    .positive_class = if (is.na(.spec$PositiveClass)) NULL else .spec$PositiveClass,
    .probs          = FALSE
  )

  # Class and Power are guaranteed rather than selected optionally: the hits table has a fixed schema
  # and an append missing a column fails at the write, several chunks after the cause.
  for (.c in c("Class", "Power")) {
    if (!.c %in% names(hits_)) hits_[[.c]] <- if (identical(.c, "Power")) NA_real_ else NA_character_
  }

  list(
    Hits = hits_ |>
      dplyr::select(DocID, Term, Class, Power),
    Labels = dec_$Pred |>
      dplyr::transmute(DocID, Top1Class = .data$PredLabel, Top1Prob = .data$Score,
                       Top2Class = .data$Pred2, Top2Prob = .data$Score2, TopTerm)
  )
}


#' A duration a human can read at a glance
#'
#' Seconds up to a minute and a half, then minutes, then hours. A pass over this corpus runs for hours
#' and a progress line reporting 20,847 seconds makes a reader do arithmetic while they are trying to
#' decide whether to wait.
#'
#' @param .secs Numeric. Seconds.
#' @return Character.
app_duration <- function(.secs) {
  if (FALSE) .secs <- 20847
  if (!is.finite(.secs) || .secs < 0) return("--")
  if (.secs <   90) return(sprintf("%.0fs", .secs))
  if (.secs < 5400) return(sprintf("%.1fm", .secs / 60))
  sprintf("%.1fh", .secs / 3600)
}

#' Key for one timing measurement
#'
#' Everything that changes the number is in the key, so a grid already measured is skipped and a grid
#' widened by one setting measures only the setting that was added.
#'
#' @param .engine,.task,.config Character.
#' @param .max_len,.batch,.n_docs Integer.
#' @return Character.
app_bench_key <- function(.engine, .task, .config, .max_len, .batch, .n_docs) {
  if (FALSE) {
    .engine  <- "bert"
    .task    <- "ClassDetailed"
    .config  <- "cfgA"
    .max_len <- 256L
    .batch   <- 32L
    .n_docs  <- 2000L
  }
  substr(digest::digest(paste(.engine, .task, .config, as.integer(.max_len), as.integer(.batch),
                              as.integer(.n_docs), sep = "|"), algo = "xxhash64"), 1L, 12L)
}

#' A fixed sample of documents to time against
#'
#' The SAME documents every time, ordered by identifier. A benchmark comparing two context lengths on
#' two different samples measures the samples as much as the lengths, and the difference this grid
#' exists to detect is a factor of two at most.
#'
#' Drawn from the corpus rather than from what is outstanding, so a benchmark run after a pass has
#' begun times the same work as one run before it.
#'
#' @param .con Connection.
#' @param .n Integer. Documents to draw.
#' @return Tibble: DocID, Path, Text.
app_bench_docs <- function(.con, .n) {
  if (FALSE) {
    .con <- con
    .n   <- 2000L
  }
  docs_ <- DBI::dbGetQuery(
    .con, paste0("SELECT DocID, Path FROM corpus ORDER BY DocID LIMIT ", as.integer(.n))
  ) |>
    tibble::as_tibble() |>
    app_read_text()
  out_ <- docs_ |> dplyr::filter(!is.na(.data$Text), nzchar(trimws(.data$Text)))
  if (nrow(out_) == 0L) cli::cli_abort("No readable text in the first {(.n)} corpus documents.")
  if (nrow(out_) < nrow(docs_)) {
    cli::cli_alert_info(
      "{nrow(docs_) - nrow(out_)} of {nrow(docs_)} sample document{?s} had no text; timing the rest."
    )
  }
  out_
}

#' Time one engine configuration, without writing a single label
#'
#' The predictions are produced and thrown away. That is what makes the grid free to price a context
#' length nobody intends to deploy, and what keeps a timing run from consuming the queue the real pass
#' is going to work through.
#'
#' Already-measured combinations are skipped, so this chunk costs minutes on the first render and
#' seconds afterwards -- which is what lets it run on every render rather than hiding behind a switch.
#'
#' @param .con Connection.
#' @param .docs Output of app_bench_docs().
#' @param .grid Tibble: Engine, Task, ConfigName, Artifact, MaxLen, BatchSize.
#' @param .python,.script Interpreter and classify_apply.py.
#' @param .chunk_size Integer. Documents per engine invocation, so the model-load cost is timed as it
#'   would be paid.
#' @return Tibble of every row in the grid, measured or recalled.
app_benchmark <- function(.con, .docs, .grid, .python, .script, .chunk_size = 5000L) {
  if (FALSE) {
    .con        <- con
    .docs       <- bench_docs
    .grid       <- grid_bench
    .chunk_size <- 5000L
  }
  have_ <- tibble::as_tibble(DBI::dbGetQuery(.con, "SELECT * FROM benchmarks"))

  purrr::map(seq_len(nrow(.grid)), function(.i) {
    g_   <- .grid[.i, ]
    key_ <- app_bench_key(g_$Engine, g_$Task, g_$ConfigName, g_$MaxLen, g_$BatchSize, nrow(.docs))
    hit_ <- have_ |> dplyr::filter(.data$BenchKey == key_)
    if (nrow(hit_) == 1L) {
      return(hit_ |> dplyr::mutate(Measured = "recalled"))
    }

    cli::cli_alert_info(
      "Timing {(g_$Engine)} / {(g_$Task)} at L{g_$MaxLen}, batch {g_$BatchSize} \\
       on {nrow(.docs)} document{?s}."
    )
    t0_ <- Sys.time()
    purrr::walk(app_chunks(.docs = .docs, .size = .chunk_size), function(.ch) {
      invisible(app_bert_chunk(
        .docs = .ch, .artifact = g_$Artifact, .max_len = g_$MaxLen,
        .python = .python, .script = .script, .batch_size = g_$BatchSize
      ))
    })
    secs_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))

    row_ <- tibble::tibble(
      BenchKey = key_, Engine = g_$Engine, Task = g_$Task, ConfigName = g_$ConfigName,
      MaxLen = as.integer(g_$MaxLen), BatchSize = as.integer(g_$BatchSize),
      nDocs = nrow(.docs), Seconds = secs_, DocsPerSecond = nrow(.docs) / max(secs_, 1e-9),
      MeasuredAt = Sys.time()
    )
    DBI::dbAppendTable(.con, "benchmarks", row_)
    row_ |> dplyr::mutate(Measured = "new")
  }) |>
    purrr::list_rbind()
}

#' Report the timing grid and what each setting would cost over the corpus
#' @param .tab Output of app_benchmark().
#' @param .n_corpus Integer. Attachments this population classifies, as returned by
#'   `app_corpus_load()`. NOT the index size: the index holds one row per registrant copy.
#' @return Invisibly .tab.
app_report_benchmark <- function(.tab, .n_corpus) {
  if (FALSE) {
    .tab      <- bench
    .n_corpus <- 1.1e6
  }
  out_ <- .tab |>
    dplyr::mutate(HoursPerTask = .n_corpus / .data$DocsPerSecond / 3600) |>
    dplyr::arrange(.data$Task, .data$MaxLen, .data$BatchSize)

  tbl_head("What each setting would cost over the corpus")
  tbl_out(
    .tab    = out_ |> dplyr::select(Task, MaxLen, BatchSize, nDocs, Seconds, DocsPerSecond,
                                    HoursPerTask, Measured),
    .title  = "What each setting would cost over the corpus",
    .groups = c(" " = 3, "Measured on the sample" = 3, " " = 2),
    .digits = 1L,
    .notes  = c(
      DocsPerSecond = paste("Over the whole run including reading and writing, not inference alone,",
                            "because that is what the wall clock is made of."),
      HoursPerTask  = paste("This rate projected to every ATTACHMENT this population classifies, for",
                            "ONE task -- not to every registrant copy, which deduplication removes.",
                            "Three",
                            "tasks cost three times this unless they share a pass."),
      Measured      = paste("A row reading recalled was measured on an earlier render and read back",
                            "rather than run again. Nothing here writes a label, so re-running costs",
                            "time and changes nothing.")
    )
  )

  wide_ <- out_ |>
    dplyr::summarise(Hours = mean(.data$HoursPerTask), .by = MaxLen) |>
    dplyr::arrange(.data$MaxLen)
  if (nrow(wide_) == 2L) {
    tbl_note(
      "The longer window costs {sprintf('%.1fx', wide_$Hours[2] / wide_$Hours[1])} the shorter one \\
       per task -- {sprintf('%.1f', wide_$Hours[1])} hours against \\
       {sprintf('%.1f', wide_$Hours[2])}. Read that against what 03E measured the length to be worth \\
       on the labelled sample: the question is not which window is better but whether the better one \\
       is worth the difference."
    )
  }
  invisible(out_)
}


# 5. The pass ------------------------------------------------------------------------------------------------------------

#' Label the outstanding documents for one run, chunk by chunk
#'
#' Every chunk is written before the next is read, which is what makes an interrupted pass resumable
#' rather than merely restartable. A chunk that fails as a whole is recorded document by document and
#' the pass continues: one unreadable file must not cost a million labelled ones.
#'
#' @param .con Connection.
#' @param .spec One row of the run specification.
#' @param .label_fn Function of one chunk, returning a list with Labels and optionally Hits.
#' @param .chunk_size Integer. Documents per call to the engine.
#' @param .limit Integer or NULL. Cap the queue, for a rehearsal.
#' @param .report_every Integer. Chunks between progress lines.
#' @param .workers Integer. Daemons for the corpus READ only; the engine call stays sequential.
#'   1 reads sequentially and starts nothing. The first and last chunks always
#'   report, so a short run is never silent and a long one confirms early that it is alive. Note that
#'   this counts CHUNKS, so the cadence in minutes moves with .chunk_size: at five thousand documents
#'   a chunk every fifth chunk is roughly every six minutes, and at five hundred it is roughly every
#'   forty seconds.
#' @return Tibble: nDocs, nChunks, Seconds, DocsPerSecond.
app_pass <- function(.con, .spec, .label_fn, .chunk_size = 5000L, .limit = NULL,
                     .report_every = 5L, .workers = 1L) {
  if (FALSE) {
    .con        <- con
    .spec       <- spec_bert[1, ]
    .label_fn   <- function(.d) list(Labels = app_bert_chunk(.d, "x", 256L, "python3", "s.py"))
    .chunk_size   <- 5000L
    .limit        <- 2000L
    .report_every <- 5L
  }
  tab_ <- if (identical(.spec$Engine, "bert")) "bert_labels" else "keyword_labels"
  app_run_register(.con = .con, .spec = .spec)

  # DAEMONS ARE STARTED ONCE PER PASS, not once per chunk. Startup is about a second each, and a full
  # pass is a couple of hundred chunks, so starting them inside the loop would spend more on daemons
  # than the parallel read saves.
  #
  # ONLY THE READ IS PARALLEL. The engines stay sequential and deliberately so: the transformer holds
  # the GPU, which is one resource, and several processes contending for it is slower than one using
  # it. Reading is file I/O and scales; that is why this is where the workers go.
  par_ok_ <- FALSE
  if (.workers > 1L) {
    mirai::daemons(.workers)
    on.exit(mirai::daemons(0L), add = TRUE)

    # A DAEMON IS A FRESH R SESSION AND HOLDS NONE OF THIS ONE'S FUNCTIONS. An earlier version sent
    # clf_read_text() as a serialized closure and assumed it would carry; it did not, and every task
    # returned a miraiError instead of text. The reader is sourced into each daemon instead, which is
    # also what keeps ONE implementation of it: the corpus is read by the same function that read the
    # labelled sample, not by a copy that happens to agree today.
    #
    # The whole chain travels because 03A's library is not inert -- it calls plot_register_levels()
    # at load time, so _Plots.R has to be in scope before it loads.
    par_ok_ <- tryCatch({
      mirai::everywhere(
        {
          for (.f in c(file.path("_Commons", "_Initialize.R"), file.path("_Commons", "_Utils.R"),
                       file.path("_Commons", "_Plots.R"),      file.path("_Commons", "_Tables.R"),
                       "03A-ClassifyPrepare.R")) {
            source(file.path(.code, .f), encoding = "UTF-8")
          }
        },
        .code = here::here("1_code")
      )
      TRUE
    }, error = function(e) {
      cli::cli_alert_warning(
        "Could not prepare the daemons ({conditionMessage(e)}); this pass reads sequentially."
      )
      FALSE
    })

    if (par_ok_) {
      cli::cli_alert_success("{(.workers)} daemon{?s} up and carrying the reader.")
    }
  }

  queue_ <- app_pending(.con = .con, .run_key = .spec$RunKey, .table = tab_, .limit = .limit)
  if (nrow(queue_) == 0L) {
    cli::cli_alert_info("{(.spec$Engine)} / {(.spec$Task)}: nothing outstanding.")
    return(tibble::tibble(nDocs = 0L, nChunks = 0L, Seconds = 0, DocsPerSecond = NA_real_))
  }

  chunks_ <- app_chunks(.docs = queue_, .size = .chunk_size)
  cli::cli_alert_info(
    "{(.spec$Engine)} / {(.spec$Task)}: {nrow(queue_)} document{?s} in {length(chunks_)} chunk{?s}."
  )
  t0_   <- Sys.time()
  done_ <- 0L

  # A plain loop rather than a walk, because the progress line needs a running count and an
  # accumulator reaching out of a closure is a worse way to say the same thing.
  for (.i in seq_along(chunks_)) {
    ch_    <- app_read_text(.docs = chunks_[[.i]], .workers = if (par_ok_) .workers else 1L)
    done_  <- done_ + nrow(chunks_[[.i]])
    bad_ <- ch_ |> dplyr::filter(is.na(.data$Text) | !nzchar(trimws(.data$Text)))
    ok_  <- ch_ |> dplyr::filter(!is.na(.data$Text), nzchar(trimws(.data$Text)))

    if (nrow(bad_) > 0L) {
      DBI::dbAppendTable(.con, "failures", tibble::tibble(
        RunKey = .spec$RunKey, DocID = bad_$DocID, Reason = "no text", SeenAt = Sys.time()))
    }
    if (nrow(ok_) > 0L) {
      res_ <- tryCatch(.label_fn(ok_), error = function(e) {
        cli::cli_alert_danger("Chunk {(.i)} failed: {conditionMessage(e)}")
        NULL
      })
      if (is.null(res_)) {
        DBI::dbAppendTable(.con, "failures", tibble::tibble(
          RunKey = .spec$RunKey, DocID = ok_$DocID, Reason = "engine error", SeenAt = Sys.time()))
      } else {
        DBI::dbAppendTable(.con, tab_,
                           res_$Labels |> dplyr::mutate(RunKey = .spec$RunKey, .before = 1))
        if (!is.null(res_$Hits) && nrow(res_$Hits) > 0L) {
          DBI::dbAppendTable(.con, "keyword_hits",
                             res_$Hits |> dplyr::mutate(RunKey = .spec$RunKey, .before = 1))
        }
      }
    }

    # PROGRESS IS REPORTED WHATEVER HAPPENED TO THE CHUNK, including one that failed entirely: a run
    # that goes quiet is indistinguishable from a run that has hung, and the difference matters at
    # three in the morning.
    #
    # The rate is cumulative rather than per-chunk, so it self-corrects. The first chunk on Apple
    # silicon pays for kernel compilation and never pays again, and an estimate built on it alone
    # would promise hours that never materialise; averaged over everything done so far it settles
    # within a few chunks.
    if (.i %% .report_every == 0L || .i == 1L || .i == length(chunks_)) {
      now_     <- Sys.time()
      elapsed_ <- as.numeric(difftime(now_, t0_, units = "secs"))
      rate_    <- done_ / max(elapsed_, 1e-9)
      left_    <- nrow(queue_) - done_
      eta_     <- left_ / max(rate_, 1e-9)
      cli::cli_alert_info(
        "  chunk {(.i)}/{length(chunks_)} | {done_}/{nrow(queue_)} docs | \\
         {app_duration(elapsed_)} elapsed | {round(rate_)}/s | \\
         {if (left_ == 0L) 'done' else paste0('ETA ', app_duration(eta_), ' (~', \\
          format(now_ + eta_, '%H:%M'), ')')}"
      )
    }
  }

  secs_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))
  app_run_register(.con = .con, .spec = .spec)
  tibble::tibble(nDocs = nrow(queue_), nChunks = length(chunks_), Seconds = secs_,
                 DocsPerSecond = nrow(queue_) / max(secs_, 1e-9))
}

#' Report a set of pass timings and what they imply for the whole corpus
#' @param .tab Timings, bound across runs, carrying Engine and Task.
#' @param .n_corpus Integer. Attachments this population classifies, as returned by
#'   `app_corpus_load()`. NOT the index size: the index holds one row per registrant copy.
#' @return Invisibly .tab.
app_report_timing <- function(.tab, .n_corpus) {
  if (FALSE) {
    .tab      <- timings
    .n_corpus <- 1.1e6
  }
  tbl_head("Throughput, and what a full pass would cost")
  tbl_out(
    .tab = .tab |>
      dplyr::mutate(FullPassHours = .n_corpus / .data$DocsPerSecond / 3600),
    .title  = "Throughput, and what a full pass would cost",
    .digits = 1L,
    .notes  = c(
      DocsPerSecond = paste("Measured over the whole pass including reading and writing, not over",
                            "inference alone, because that is what the wall clock is made of."),
      FullPassHours = paste("This rate projected to every ATTACHMENT this population classifies,",
                            "not to every registrant copy. On a short",
                            "rehearsal it is pessimistic: the first chunk pays for kernel",
                            "compilation on Apple silicon and never recurs.")
    )
  )
  invisible(.tab)
}


# 6. The released file ---------------------------------------------------------------------------------------------------
# The store is an archive: long, keyed by run, holding every hit and every timing. This is the
# deliverable: one row per document, one column per thing a reader needs, in the shape a merge wants.
#
# BOTH ENGINES SHIP, AND THEIR SCORES ARE NAMED APART. The transformer's is a softmax probability over
# classes; the keyword table's is Power, a Wilson lower bound on the firing term's training precision.
# Both live in [0, 1] and neither is the other, so calling both of them Prob would put two
# incomparable quantities in adjacent columns under one name and invite the comparison 03E ruled out.
#
# ABSTENTION BECOMES NA. The decision rule emits a sentinel where no term fired, and also where the
# top two classes tied -- a deliberate refusal to break it. Carried into the release as a category, it
# would grow a phantom class holding half the corpus in every tabulation.
#
# ONLY DOCUMENTS WITH EVERY TRANSFORMER LABEL. A file holding a detailed label and a missing broad one
# is a file where a cross-tabulation silently drops rows and reports a number computed on a subset
# while a reader believes they are looking at the corpus. What is left out is counted rather than
# hidden, which is the same information without the trap.

#' Stop with a sentence rather than a stack trace when the store is shut
#'
#' A closed connection surfaces from the engine as "Invalid connection" with a context of
#' `rapi_prepare`, twenty frames below the call that caused it, and says nothing about which call that
#' was. Every function here reads the store, and the only way to arrive with a shut one is to have
#' disconnected too early, so the check names that.
#'
#' @param .con Connection.
#' @param .what Character. What was being attempted, for the message.
#' @return Invisibly TRUE, or aborts.
#' @keywords internal
app_require_con <- function(.con, .what = "read the store") {
  if (FALSE) {
    .con  <- con
    .what <- "build the release"
  }
  if (!DBI::dbIsValid(.con)) {
    cli::cli_abort(c(
      "The store is closed, so it is not possible to {(.what)}.",
      "i" = "The disconnect chunk runs last for exactly this reason. If a section was added below \
             it, move it above."
    ))
  }
  invisible(TRUE)
}

#' The released table: one row per document, both engines side by side
#'
#' @param .con Connection.
#' @param .specs Run specification, filtered to the runs this file is built from.
#' @param .tab_prep Prepared sample, supplying the detailed-to-broad mapping for the consistency flag.
#' @param .tasks Character vector of tasks, in the order their column blocks should appear.
#' @param .none Character. Abstention sentinel the decision rule emits, converted to NA.
#' @return Tibble, one row per document carrying every task's block.
app_release <- function(.con, .specs, .tab_prep, .tasks, .none = "(none)") {
  if (FALSE) {
    .con      <- con
    .specs    <- dplyr::filter(specs, is.na(MaxLen) | MaxLen == 256L)
    .tab_prep <- tab_prep
    .tasks    <- .lP$Param$Tasks
  }
  app_require_con(.con = .con, .what = "build the release")

  # Joined in R rather than in one long pivot query. The frames are a few million rows of short
  # strings and the machine has room for them; a generated pivot across six run keys would be faster
  # and unreadable, and this is the code that decides what a released dataset looks like.
  blocks_ <- purrr::map(.tasks, function(.t) {
    kb_ <- .specs |> dplyr::filter(.data$Engine == "bert",    .data$Task == .t)
    kk_ <- .specs |> dplyr::filter(.data$Engine == "keyword", .data$Task == .t)
    if (nrow(kb_) != 1L) cli::cli_abort("Expected exactly one transformer run for {(.t)}.")

    bert_ <- DBI::dbGetQuery(
      .con, "SELECT DocID, Top1Class, Top1Prob, Top2Class, Top2Prob
             FROM bert_labels WHERE RunKey = ?", params = list(kb_$RunKey)
    ) |>
      tibble::as_tibble() |>
      stats::setNames(c("DocID", paste0("Bert", .t), paste0("Bert", .t, "Prob"),
                        paste0("Bert", .t, "2"), paste0("Bert", .t, "2Prob")))

    if (nrow(kk_) != 1L) return(bert_)

    kw_ <- DBI::dbGetQuery(
      .con, "SELECT DocID, Top1Class, Top1Prob, TopTerm
             FROM keyword_labels WHERE RunKey = ?", params = list(kk_$RunKey)
    ) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        # The sentinel and a tied top pair are both refusals, and both arrive here as the sentinel.
        Top1Class = dplyr::if_else(.data$Top1Class == .none, NA_character_, .data$Top1Class),
        Top1Prob  = dplyr::if_else(is.na(.data$Top1Class), NA_real_, .data$Top1Prob),
        TopTerm   = dplyr::if_else(is.na(.data$Top1Class), NA_character_, .data$TopTerm)
      ) |>
      stats::setNames(c("DocID", paste0("Kw", .t), paste0("Kw", .t, "Power"),
                        paste0("Kw", .t, "Term")))

    bert_ |>
      dplyr::left_join(kw_, by = dplyr::join_by(DocID)) |>
      dplyr::mutate(
        !!paste0(.t, "Flag") := dplyr::case_when(
          is.na(.data[[paste0("Kw", .t)]])                      ~ "unchecked",
          .data[[paste0("Kw", .t)]] == .data[[paste0("Bert", .t)]] ~ "confirmed",
          TRUE                                                  ~ "contradicted"
        )
      )
  })

  # INNER joins across tasks, which is what makes every row complete. A document reached by the
  # detailed pass and not yet by the broad one leaves rather than arriving with a hole.
  out_ <- purrr::reduce(blocks_, \(.a, .b) dplyr::inner_join(.a, .b, by = dplyr::join_by(DocID)))

  # THE FAN-OUT. Everything above is keyed on the attachment that was actually classified -- one row
  # per HashDocument, the primary copy. A filing naming several registrants lists the same attachment
  # under each of them, so the released file carries one row per REGISTRANT COPY and merges onto
  # filer-level data directly. Roughly one document in six is a repeat of this kind.
  #
  # 01C settled which way this goes: "classification wants one, entity extraction may want each,
  # because the registrant-side metadata differs even where the text does not." The store holds the
  # one; the release fans out to the each.
  #
  # FilerCopiesAgree GATES THE FAN-OUT. The flag records whether an attachment's copies match on all
  # three length measures. Where they do not, the text under one registrant is not the text under
  # another and the label was computed from only one of them, so the copies keep the label and are
  # marked rather than passed off as equivalent. 01C found one attachment of 1.49 million in this
  # state and all eight of its copies were already flagged Removed, so this costs nothing today and
  # is not therefore free to leave unsaid.
  copies_ <- DBI::dbGetQuery(
    .con,
    "SELECT DocID, HashDocument, PrimaryFiler, FilerCopiesAgree, DescSample, EstiSample
     FROM corpus ORDER BY DocID"
  ) |>
    tibble::as_tibble()

  key_ <- copies_ |>
    dplyr::filter(as.logical(.data$PrimaryFiler)) |>
    dplyr::select("HashDocument", Primary = "DocID")

  n_attach_ <- nrow(out_)
  out_ <- out_ |>
    dplyr::inner_join(y = key_, by = dplyr::join_by(DocID == Primary)) |>
    dplyr::select(-"DocID") |>
    # One row per registrant copy. INNER, so a copy with no classified attachment does not arrive
    # carrying empty label columns.
    dplyr::inner_join(y = copies_, by = dplyr::join_by(HashDocument), relationship = "one-to-many") |>
    dplyr::relocate("DocID", "HashDocument", "PrimaryFiler", "FilerCopiesAgree",
                    "DescSample", "EstiSample") |>
    dplyr::arrange(.data$DocID)

  n_disagree_ <- sum(!as.logical(out_$FilerCopiesAgree), na.rm = TRUE)
  cli::cli_alert_info(
    "Fanned out {format(n_attach_, big.mark = ',')} {cli::qty(n_attach_)}classified attachment{?s} \\
     to {format(nrow(out_), big.mark = ',')} {cli::qty(nrow(out_))}registrant cop{?y/ies}."
  )
  if (n_disagree_ > 0L) {
    cli::cli_alert_warning(
      "{format(n_disagree_, big.mark = ',')} {cli::qty(n_disagree_)}cop{?y/ies} belong{?s/} to an \\
       attachment whose copies do NOT agree on length. \\
       They carry the label computed from the primary copy and are marked by {.field FilerCopiesAgree}."
    )
  }

  if (all(c("ClassDetailed", "ClassBroad") %in% .tasks)) {
    map_ <- .tab_prep |>
      dplyr::filter(!is.na(.data$ClassDetailed), !is.na(.data$ClassBroad)) |>
      dplyr::distinct(BertClassDetailed = .data$ClassDetailed, Parent = .data$ClassBroad)
    out_ <- out_ |>
      dplyr::left_join(map_, by = dplyr::join_by(BertClassDetailed)) |>
      dplyr::mutate(HierConsistent = .data$Parent == .data$BertClassBroad, Parent = NULL)
  }
  out_ |> dplyr::arrange(.data$DocID)
}

#' Write the release and the manifest that makes it reproducible
#'
#' Two files, and the second is not optional. Labels without a record of what produced them are
#' unreproducible: in a year nobody will know which checkpoint, at which context length, wrote a
#' given column, and the file itself cannot say.
#'
#' A release built while a run is still going gets `_partial` in its name. It is a well-formed file
#' either way -- every column populated, merges without a murmur -- and nothing inside it says it
#' covers two per cent of the corpus. The stamp is what stops a partial file being mistaken for a
#' finished one six months later.
#'
#' @param .tab Output of app_release().
#' @param .con Connection, for the run records.
#' @param .specs The runs this release was built from.
#' @param .dir Character. Destination directory.
#' @param .stem Character. File stem; the context length and any partial stamp are appended.
#' @param .max_len Integer or NULL. Stamped into the name, because two context lengths produce two
#'   releases and a reader must be able to tell them apart without opening either.
#' @param .n_corpus Integer. Attachments this population classifies, for the completeness
#'   report. NOT the index size, which counts every registrant copy the release fans out to.
#' @return Invisibly a one-row tibble naming both files.
app_write_release <- function(.tab, .con, .specs, .dir, .stem = "contract_labels",
                              .max_len = NULL, .n_corpus = NA_integer_) {
  if (FALSE) {
    .tab      <- release
    .con      <- con
    .specs    <- specs_256
    .dir      <- .lP$Output$Release
    .max_len  <- 256L
    .n_corpus <- n_corpus
  }
  app_require_con(.con = .con, .what = "write the release manifest")
  fs::dir_create(.dir)
  status_  <- app_status(.con = .con, .specs = .specs)
  partial_ <- any(status_$Status != "complete")

  name_ <- paste0(.stem,
                  if (!is.null(.max_len)) paste0("_L", as.integer(.max_len)) else "",
                  if (partial_) "_partial" else "")
  path_ <- fs::path(.dir, paste0(name_, ".parquet"))
  man_  <- fs::path(.dir, paste0(name_, "_manifest.parquet"))

  arrow::write_parquet(.tab, path_)

  runs_ <- DBI::dbGetQuery(.con, "SELECT * FROM runs") |>
    tibble::as_tibble() |>
    dplyr::filter(.data$RunKey %in% .specs$RunKey) |>
    dplyr::left_join(status_ |> dplyr::select(RunKey, nDone, nFailed, Status),
                     by = dplyr::join_by(RunKey)) |>
    dplyr::mutate(
      Release   = name_,
      # STAMPED IN UTC, because the two timestamps beside it are. StartedAt and UpdatedAt round-trip
      # through the store, which normalises to UTC; Sys.time() written straight out carries local wall
      # clock with no zone attached. Subtracting one from the other then gives a duration quietly
      # wrong by the offset -- two hours here -- and nothing in the file says so.
      WrittenAt = as.POSIXct(Sys.time(), tz = "UTC"),
      # Counts arrive from the engine as int64 and land in R as numeric. A manifest reporting 28985.0
      # documents is not wrong, it is just not a count.
      nDone     = as.integer(.data$nDone),
      nFailed   = as.integer(.data$nFailed),
      nReleased = nrow(.tab),
      # The denominator, so the file describes itself. A year from now 28,985 out of 1,462,939 is
      # immediately legible where 28,985 on its own is a number needing a second document.
      nCorpus   = as.integer(.n_corpus)
    )
  arrow::write_parquet(runs_, man_)

  tbl_head("The released file")
  tibble::tibble(
    File = c(fs::path_file(path_), fs::path_file(man_)),
    Rows = c(nrow(.tab), nrow(runs_)),
    Cols = c(ncol(.tab), ncol(runs_)),
    MB   = round(as.numeric(fs::file_size(c(path_, man_))) / 1024^2, 1)
  ) |>
    tbl_out(.title = "The released file")

  if (is.finite(.n_corpus)) {
    tbl_note(
      "{nrow(.tab)} of {(.n_corpus)} corpus document{?s} carry every transformer label and are in this \\
       file; {(.n_corpus - nrow(.tab))} are not, because at least one pass has not reached them or \\
       could not read them."
    )
  }
  if (partial_) {
    tbl_note(
      "At least one run is still incomplete, so this release is stamped {.val {name_}}. It is a \\
       well-formed file and it does not cover the corpus.",
      .type = "warn"
    )
  }
  tbl_note(
    "The manifest names the configuration, the context length and the artifact fingerprint behind \\
     every column. It is the half of the release that makes the other half reproducible, and it \\
     travels with it."
  )
  invisible(tibble::tibble(Labels = as.character(path_), Manifest = as.character(man_)))
}

#' What the released columns mean
#'
#' Written from the table rather than typed, so a column added upstream cannot go undocumented.
#'
#' @param .tab Output of app_release().
#' @return Tibble: Column, Meaning.
app_release_schema <- function(.tab) {
  if (FALSE) .tab <- release
  tibble::tibble(Column = names(.tab)) |>
    dplyr::mutate(
      Meaning = dplyr::case_when(
        .data$Column == "DocID"                    ~ "Document identifier; the merge key.",
        .data$Column == "HierConsistent"           ~ "Does the detailed label's parent equal the broad label?",
        grepl("Flag$", .data$Column)               ~ "confirmed / contradicted / unchecked by the lexicon.",
        grepl("^Kw.*Term$", .data$Column)          ~ "The term that fired; NA where the lexicon was silent.",
        grepl("^Kw.*Power$", .data$Column)         ~ paste("Wilson lower bound on that term's",
                                                            "training precision. NOT a probability."),
        grepl("^Kw", .data$Column)                 ~ "Keyword label; NA where the lexicon was silent or tied.",
        grepl("^Bert.*2Prob$", .data$Column)       ~ "Softmax probability of the runner-up class.",
        grepl("^Bert.*2$", .data$Column)           ~ "Runner-up class.",
        grepl("^Bert.*Prob$", .data$Column)        ~ "Softmax probability of the released label.",
        grepl("^Bert", .data$Column)               ~ "The released label.",
        TRUE                                       ~ ""
      )
    )
}

# 7. Results -------------------------------------------------------------------------------------------------------------

#' What each run produced, read back from the store
#' @param .con Connection.
#' @param .specs The run specification.
#' @return Tibble: Engine, Task, nLabelled, Coverage, MeanTop1, nFailed.
#' Category shares across the labelled corpus
#'
#' Drawn at the registered taxonomic order and colours, so this panel and 03B's read against each
#' other bar for bar. Computed in the store rather than pulled into R: the corpus is over a million
#' rows and a count is a query.
#'
#' @param .con Store connection.
#' @param .spec One run specification, single row.
#' @param .key Registered level key for order and colour.
#' @return A ggplot.
app_plot_distribution <- function(.con, .spec, .key = "ClassDetailed") {
  if (FALSE) {
    .con  <- con
    .spec <- dplyr::filter(specs, Engine == "bert", Task == "ClassDetailed")
  }
  app_require_con(.con = .con, .what = "plot the distribution")
  if (nrow(.spec) != 1L) cli::cli_abort("Expected exactly one run specification.")

  tab_ <- DBI::dbGetQuery(
    .con, "SELECT Top1Class AS Label, COUNT(*) AS n FROM bert_labels WHERE RunKey = ?
           GROUP BY Top1Class", params = list(.spec$RunKey)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(Share = .data$n / sum(.data$n))

  plot_bar_ranked(
    .tab  = tab_, .cat = "Label", .val = "Share",
    .key  = .key,      # registered taxonomic order and colours
    .pct  = TRUE,      # shares, not counts
    .desc = FALSE      # taxonomic order, not by size: a panel that reorders cannot be compared
  )
}

#' How confident the transformer is across the corpus
#'
#' Binned in the store rather than collected: a million probabilities do not need to travel to be
#' histogrammed. The bin width is stated rather than chosen by a default, because the distribution is
#' massed against one and a default bin count hides exactly the tail that matters.
#'
#' @param .con Store connection.
#' @param .spec One run specification, single row.
#' @param .binwidth Probability bin.
#' @return A ggplot.
app_plot_confidence <- function(.con, .spec, .binwidth = 0.01) {
  if (FALSE) {
    .con  <- con
    .spec <- dplyr::filter(specs, Engine == "bert", Task == "ClassDetailed")
  }
  app_require_con(.con = .con, .what = "plot confidence")
  if (nrow(.spec) != 1L) cli::cli_abort("Expected exactly one run specification.")

  tab_ <- DBI::dbGetQuery(
    .con, paste0(
      "SELECT FLOOR(Top1Prob / ", .binwidth, ") * ", .binwidth, " AS Bin, COUNT(*) AS n ",
      "FROM bert_labels WHERE RunKey = ? GROUP BY Bin ORDER BY Bin"
    ), params = list(.spec$RunKey)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(Share = .data$n / sum(.data$n))

  # A ranked bar over probability bins would sort the axis by height, which is exactly wrong for a
  # distribution. Drawn directly, with the bins left in their own order.
  ggplot2::ggplot(tab_, ggplot2::aes(x = .data$Bin, y = .data$Share)) +
    ggplot2::geom_col(fill = .plot_ink, width = .binwidth * 0.9) +
    plot_scale_y_pct() +
    ggplot2::labs(x = "Top-class probability", y = "Share of the corpus") +
    plot_theme(.grid = "y")
}

#' Where the keyword table confirms, contradicts, or never looked
#'
#' The three-way flag the release carries, by category. `unchecked` is the keyword arm's abstention:
#' a property of the lexicon rather than of the document, so a tall unchecked bar marks a category the
#' table never reached rather than a document that was hard.
#'
#' @param .con Store connection.
#' @param .specs Run specifications; the transformer and keyword runs for `.task` are selected here.
#' @param .task Task to draw.
#' @param .key Registered level key for order and colour.
#' @return A ggplot, or NULL where the task has no keyword run.
app_plot_agreement <- function(.con, .specs, .task = "ClassDetailed", .key = "ClassDetailed") {
  if (FALSE) {
    .con   <- con
    .specs <- specs
    .task  <- "ClassDetailed"
  }
  app_require_con(.con = .con, .what = "plot agreement")

  kb_ <- .specs |> dplyr::filter(.data$Engine == "bert",    .data$Task == .task)
  kk_ <- .specs |> dplyr::filter(.data$Engine == "keyword", .data$Task == .task)
  if (nrow(kb_) != 1L) cli::cli_abort("Expected exactly one transformer run for {(.task)}.")
  if (nrow(kk_) != 1L) {
    cli::cli_alert_info("No keyword run for {(.task)}; nothing to compare.")
    return(invisible(NULL))
  }

  tab_ <- DBI::dbGetQuery(
    .con,
    "SELECT b.Top1Class AS Label,
            CASE WHEN k.Top1Class IS NULL      THEN 'unchecked'
                 WHEN k.Top1Class = b.Top1Class THEN 'confirmed'
                 ELSE 'contradicted' END       AS Agreement,
            COUNT(*) AS n
       FROM bert_labels b
       LEFT JOIN (SELECT DocID, Top1Class FROM keyword_labels WHERE RunKey = ?) k
         ON k.DocID = b.DocID
      WHERE b.RunKey = ?
      GROUP BY Label, Agreement",
    params = list(kk_$RunKey, kb_$RunKey)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(Share = .data$n / sum(.data$n), .by = "Label")

  plot_bar_stacked(
    .tab   = tab_, .cat = "Label", .val = "Share", .fill = "Agreement",
    .key   = .key,     # registered taxonomic order and colours
    .share = TRUE,     # each category sums to one, so the unchecked share is directly readable
    .desc  = FALSE     # taxonomic order
  )
}

app_summary <- function(.con, .specs) {
  if (FALSE) {
    .con   <- con
    .specs <- dplyr::bind_rows(spec_bert, spec_kw)
  }
  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS n FROM corpus")$n[[1]]
  purrr::map(seq_len(nrow(.specs)), function(.i) {
    s_   <- .specs[.i, ]
    tab_ <- if (identical(s_$Engine, "bert")) "bert_labels" else "keyword_labels"
    q_   <- DBI::dbGetQuery(.con, paste0(
      "SELECT COUNT(DISTINCT DocID) AS nLab, AVG(Top1Prob) AS MeanTop1 FROM ", tab_,
      " WHERE RunKey = ? AND Top1Class IS NOT NULL"), params = list(s_$RunKey))
    f_ <- DBI::dbGetQuery(.con, "SELECT COUNT(DISTINCT DocID) AS n FROM failures WHERE RunKey = ?",
                          params = list(s_$RunKey))$n[[1]]
    tibble::tibble(Engine = s_$Engine, Task = s_$Task, nLabelled = q_$nLab[[1]],
                   Coverage = q_$nLab[[1]] / max(n_, 1L), MeanTop1 = q_$MeanTop1[[1]], nFailed = f_)
  }) |>
    purrr::list_rbind()
}

#' How the corpus distributes across the categories of one run
#' @param .con Connection.
#' @param .spec One row of the run specification.
#' @return Tibble: Class, nDocs, Share, MeanTop1.
app_distribution <- function(.con, .spec) {
  if (FALSE) {
    .con  <- con
    .spec <- spec_bert[1, ]
  }
  tab_ <- if (identical(.spec$Engine, "bert")) "bert_labels" else "keyword_labels"
  DBI::dbGetQuery(.con, paste0(
    "SELECT Top1Class AS Class, COUNT(*) AS nDocs, AVG(Top1Prob) AS MeanTop1 FROM ", tab_,
    " WHERE RunKey = ? GROUP BY Top1Class ORDER BY nDocs DESC"), params = list(.spec$RunKey)) |>
    tibble::as_tibble() |>
    dplyr::mutate(Share = .data$nDocs / sum(.data$nDocs), .after = nDocs)
}

#' Why documents failed, and how many
#' @param .con Connection.
#' @return Tibble: RunKey, Reason, nDocs.
app_failures <- function(.con) {
  if (FALSE) .con <- con
  DBI::dbGetQuery(.con, "SELECT RunKey, Reason, COUNT(DISTINCT DocID) AS nDocs
                         FROM failures GROUP BY RunKey, Reason ORDER BY nDocs DESC") |>
    tibble::as_tibble()
}
