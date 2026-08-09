# 03B-ClassifyTrainBERT: the BERT trainer wrapper (bert_*) ----
# Re-homes the proven BERT pipeline into the 03A-consuming layout. This file is
# ONLY the trainer wrapper: it deals fold ids to the contracts-engine Python
# trainer (classify_train.py) across the CLI + parquet seam and lets the engine
# write the self-describing run folders. Everything else -- prep, scoring,
# leaderboards, pooling, plots, the disk cache -- lives in 03A-ClassifyPrepare.R,
# which the 03B runbook sources, so the splits and the scoring layer never drift.
#
# Division of labour (unchanged seam):
#   - R stamps config and dispatches one fold per call; never reads model internals.
#   - Python trains, evaluates out-of-fold, and writes predictions.parquet,
#     probabilities.parquet, metrics_overall.parquet, metrics_perclass.parquet,
#     train_log.parquet, config.json, and run.log into runs/<ConfigName>_F<fold>.
#   - Idempotency is the engine's: a run whose metrics_overall.parquet exists is
#     skipped (use .overwrite to force). So a sweep is safely re-runnable and a
#     warm runs tree costs nothing but a process spawn per fold.
#
# Console contract: the engine streams its per-fold detail (device, the epoch
# logs, library warnings) into the run's run.log and keeps stdout to a [run] /
# [done] pair. bert_train captures that stdout and emits exactly ONE cli line per
# run -- success with acc / macro-F1, or a cached-skip note -- so a 320-run serial
# sweep reads as a tidy progress strip. stderr is inherited, so a Python traceback
# still surfaces immediately.
#
# Serial by design: training is MPS-bound on the M3 Ultra; concurrent jobs contend
# for the one GPU, so bert_sweep walks the grid serially (no mirai). This is the
# one structural difference from 03C's kw_sweep, which is CPU-bound and parallel.
#
# Tasks all reuse this one harness via --label-col:
#   ClassDetailed (12) / ClassBroad (7) / AmendType (Original vs Amended). The
#   amendment NA-label-drop already lives in the trainer, so AmendType trains clean.
#
# House style: native pipe; explicit package::function; dot-prefixed args;
# underscore-suffixed locals; if (FALSE) dev blocks; cli/fs/here; pure ASCII;
# {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_data <- .lP$Input$Prepared
  .runs_root <- .lP$Runs$Bert
  .python    <- .lP$Engine$Python
  .script    <- .lP$Engine$Script
}


# Train one fold ----------------------------------------------------------

#' Train one fold by invoking the contracts-engine Python trainer
#'
#' Shells out to classify_train.py for a single held-out fold, then reports one
#' cli line. The engine skips a run whose metrics already exist, so calling this
#' on a warm runs tree is a cheap no-op that prints a "skip" note.
#'
#' @param .path_data Prepared parquet path (from 03A).
#' @param .label_col "ClassDetailed", "ClassBroad", or "AmendType".
#' @param .test_fold Integer. Fold held out for testing.
#' @param .text_col Text column to train on (default "Text"; BERT ignores titles).
#' @param .class_weights Logical. Inverse-frequency class weights.
#' @param .model HuggingFace model id (default legal-bert, the crowned model).
#' @param .max_len,.epochs,.batch_size,.lr,.seed Hyperparameters.
#' @param .runs_root Directory under which the run folder is written.
#' @param .python,.script Paths to the venv python and the trainer script.
#' @param .save_model,.overwrite,.smoke Flags. .save_model defaults FALSE -- the
#'   sweep does not persist weights; deployment (03E) saves the final model.
#' @param .verbose Logical. Stream the engine's live training output (epoch logs,
#'   progress bar) to the console instead of capturing it to one cli line; the
#'   per-fold detail still records to run.log either way.
#' @return Invisibly "done", "skip", or "ran" (unparsed).
bert_train <- function(.path_data,
                       .label_col = c("ClassDetailed", "ClassBroad", "AmendType"),
                       .test_fold = 1L,
                       .text_col = "Text",
                       .class_weights = FALSE,
                       .model = "nlpaueb/legal-bert-base-uncased",
                       .max_len = 512L,
                       .epochs = 6,
                       .batch_size = 32L,
                       .lr = 2e-5,
                       .seed = 42L,
                       .runs_root = here::here("2_output", "03B-ClassifyTrainBERT", "runs"),
                       .python = here::here("contracts-engine", ".venv", "bin", "python"),
                       .script = here::here("contracts-engine", "classify_train.py"),
                       .save_model = FALSE,
                       .overwrite = FALSE,
                       .smoke = FALSE,
                       .verbose = FALSE) {
  if (FALSE) {
    .path_data     <- .lP$Input$Prepared
    .label_col     <- "ClassDetailed"
    .test_fold     <- 1L
    .runs_root     <- .lP$Runs$Bert
    .smoke         <- TRUE
  }

  .label_col <- match.arg(.label_col)
  if (!fs::file_exists(.python)) cli::cli_abort("Python not found at {(.python)}")
  if (!fs::file_exists(.script)) cli::cli_abort("Trainer not found at {(.script)}")

  args_ <- c(
    .script,
    "--data", .path_data,
    "--label-col", .label_col,
    "--text-col", .text_col,
    "--test-fold", as.character(.test_fold),
    "--model", .model,
    "--max-len", as.character(.max_len),
    "--epochs", as.character(.epochs),
    "--batch-size", as.character(.batch_size),
    "--lr", as.character(.lr),
    "--seed", as.character(.seed),
    "--runs-root", .runs_root
  )
  if (.class_weights) args_ <- c(args_, "--class-weights")
  if (!.save_model)   args_ <- c(args_, "--no-save-model")
  if (.overwrite)     args_ <- c(args_, "--overwrite")
  if (.smoke)         args_ <- c(args_, "--smoke")
  if (.verbose)       args_ <- c(args_, "--verbose")

  # Verbose: inherit the streams so the engine's [run] / live epochs / [done] flow
  # to the console as they happen. No capture, no parsed summary line (the engine
  # already prints its metrics live).
  if (.verbose) {
    status_ <- system2(.python, args = args_, stdout = "", stderr = "")
    if (!identical(status_, 0L)) {
      cli::cli_abort("Python trainer failed (exit {status_}) for {(.label_col)} fold {(.test_fold)}")
    }
    return(invisible("done"))
  }

  # Quiet (default): capture the engine's terse stdout so the console shows exactly
  # one cli line per run; stderr is inherited so a Python traceback still reaches
  # the console. The full per-fold detail lives in <run_dir>/run.log.
  out_ <- suppressWarnings(system2(.python, args = args_, stdout = TRUE, stderr = ""))
  status_ <- attr(out_, "status")
  if (!is.null(status_) && status_ != 0L) {
    cli::cli_abort("Python trainer failed (exit {status_}) for {(.label_col)} fold {(.test_fold)}")
  }

  tag_ <- sprintf(
    "%s/%s L%d E%s W%d F%d",
    .label_col, fs::path_file(.model), as.integer(.max_len),
    as.character(.epochs), as.integer(.class_weights), as.integer(.test_fold)
  )

  if (any(grepl("^\\[skip", out_))) {
    cli::cli_alert_info("skip {tag_} (cached)")
    return(invisible("skip"))
  }

  done_ <- grep("^\\[done", out_, value = TRUE)
  if (length(done_) == 1L) {
    acc_ <- stringr::str_match(done_, "acc=([0-9.]+)")[, 2]
    f1_  <- stringr::str_match(done_, "f1_macro=([0-9.]+)")[, 2]
    sec_ <- stringr::str_match(done_, "\\(([0-9.]+)s\\)")[, 2]
    cli::cli_alert_success("done {tag_}  acc={acc_} macroF1={f1_} ({sec_}s)")
    return(invisible("done"))
  }

  # Unexpected (engine produced no parseable [done]); surface what it said.
  cli::cli_alert_warning("ran {tag_} -- no [done] line parsed; see run.log")
  cli::cli_verbatim(out_)
  invisible("ran")
}


# Cross-validate one configuration ----------------------------------------

#' Run k-fold CV for one configuration (loops bert_train over folds)
#'
#' @param .path_data Prepared parquet path.
#' @param .label_col "ClassDetailed", "ClassBroad", or "AmendType".
#' @param .folds Integer vector of fold ids to hold out.
#' @param ... Passed through to bert_train (.model, .lr, .max_len, .epochs, ...).
#' @return Invisible NULL.
bert_cv <- function(.path_data, .label_col = "ClassDetailed", .folds = 1:5, ...) {
  if (FALSE) {
    .path_data <- .lP$Input$Prepared
    .label_col <- "ClassDetailed"
    .folds     <- 1:5
  }
  cli::cli_h2("BERT CV -- {(.label_col)}, folds {paste(.folds, collapse = ', ')}")
  purrr::walk(.folds, \(.f) bert_train(.path_data, .label_col = .label_col, .test_fold = .f, ...))
  invisible(NULL)
}


# Sweep a grid (serial) ---------------------------------------------------

#' Stable run key for matching grid rows against finished runs
#'
#' Builds the same string from a grid row and from a finished run's recorded
#' config, so a set membership test answers "is this run already on disk?". The
#' formatting is applied identically on both sides (so it never has to agree with
#' the engine's own ConfigName string -- only with itself), which sidesteps any
#' float-format drift between R and Python. Vectorised: every argument may be a
#' column.
#'
#' @param .label_col,.model,.text_col,.max_len,.epochs,.batch_size,.lr,.class_weights,.seed,.fold
#'   The ten axes that uniquely identify a run (scalars recycle).
#' @return Character vector of keys.
#' @keywords internal
bert_run_key <- function(.label_col, .model, .text_col, .max_len, .epochs,
                         .batch_size, .lr, .class_weights, .seed, .fold) {
  paste(
    .label_col,
    .model,
    .text_col,
    as.integer(.max_len),
    formatC(.epochs, format = "g", digits = 8),
    as.integer(.batch_size),
    formatC(.lr, format = "g", digits = 8),
    as.integer(as.logical(.class_weights)),
    as.integer(.seed),
    as.integer(.fold),
    sep = "|"
  )
}

#' Keys of all finished (non-smoke) runs under one or more runs roots
#'
#' Reads each run's metrics_overall.parquet (one tiny row) and rebuilds its key
#' via bert_run_key. Tolerant: returns character(0) when no runs exist yet, so the
#' first sweep simply reports everything as to-do.
#'
#' @param .runs_roots Character vector of one or more runs directories.
#' @return Character vector of finished-run keys.
#' @keywords internal
bert_done_keys <- function(.runs_roots) {
  if (FALSE) .runs_roots <- .lP$Runs$Bert
  cols_ <- c("LabelCol", "Model", "TextCol", "MaxLen", "Epochs",
             "BatchSize", "LR", "ClassWeights", "Seed", "TestFold")
  paths_ <- .runs_roots |>
    purrr::map(\(.r) if (fs::dir_exists(.r)) {
      fs::dir_ls(.r, recurse = TRUE, glob = "*metrics_overall.parquet")
    } else {
      character(0)
    }) |>
    purrr::list_c()
  paths_ <- paths_[!grepl("_smoke", paths_)]
  if (length(paths_) == 0L) return(character(0))

  purrr::map(paths_, \(.p) arrow::read_parquet(.p, col_select = dplyr::all_of(cols_))) |>
    purrr::list_rbind() |>
    (\(.d) bert_run_key(.d$LabelCol, .d$Model, .d$TextCol, .d$MaxLen, .d$Epochs,
                        .d$BatchSize, .d$LR, .d$ClassWeights, .d$Seed, .d$TestFold))()
}

#' Run a serial BERT sweep over a configuration grid
#'
#' Walks the grid one run at a time (MPS-bound; no parallelism) and dispatches
#' each row to bert_train. Before running it scans the runs tree, reports how many
#' grid rows are already done versus still to run (overall and per task), then
#' trains only the missing ones behind a progress bar. The grid must carry the
#' columns model, label_col, lr, max_len, epochs, class_weights, fold; text, batch
#' size and seed are held fixed across the grid via the function arguments.
#'
#' Done-detection is advisory: even a missed match is caught at runtime by the
#' engine's own skip-if-exists, so the sweep can never duplicate a finished run.
#' With .overwrite = TRUE the scan is bypassed and every row is (re)trained.
#'
#' @param .path_data Prepared parquet path.
#' @param .grid Tibble with columns model, label_col, lr, max_len, epochs,
#'   class_weights, fold (one row per run).
#' @param .runs_root Directory under which run folders are written / scanned.
#' @param .text_col Text column (held fixed across the grid).
#' @param .batch_size Batch size (held fixed across the grid).
#' @param .seed RNG seed (held fixed across the grid; must match bert_train).
#' @param .overwrite Logical. Retrain every row, ignoring what is already on disk.
#' @param .verbose Logical. Stream each run's live training output (passed to
#'   bert_train); note it interleaves with the progress bar, so it is mainly for
#'   debugging a single run rather than a full sweep.
#' @param ... Passed through to bert_train (.python, .script, ...).
#' @return Invisibly the grid tagged with its Done status.
bert_sweep <- function(.path_data, .grid,
                       .runs_root = here::here("2_output", "03B-ClassifyTrainBERT", "runs"),
                       .text_col = "Text",
                       .batch_size = 32L,
                       .seed = 42L,
                       .overwrite = FALSE,
                       .verbose = FALSE,
                       ...) {
  if (FALSE) {
    .path_data <- .lP$Input$Prepared
    .grid      <- tidyr::expand_grid(
      model = "nlpaueb/legal-bert-base-uncased", label_col = "ClassDetailed",
      lr = 2e-5, max_len = 512L, epochs = 6, class_weights = FALSE, fold = 1:5
    )
    .runs_root <- .lP$Runs$Bert
    .overwrite <- FALSE
  }

  need_ <- c("model", "label_col", "lr", "max_len", "epochs", "class_weights", "fold")
  miss_ <- setdiff(need_, names(.grid))
  if (length(miss_) > 0L) cli::cli_abort("Grid missing columns: {miss_}")

  # Tag each grid row done / to-do by matching its key against the runs tree.
  grid_ <- .grid |>
    dplyr::mutate(
      RowKey = bert_run_key(.data$label_col, .data$model, .text_col, .data$max_len,
                            .data$epochs, .batch_size, .data$lr, .data$class_weights,
                            .seed, .data$fold)
    )
  done_keys_ <- if (.overwrite) character(0) else bert_done_keys(.runs_root)
  grid_ <- grid_ |> dplyr::mutate(Done = .data$RowKey %in% done_keys_)

  n_all_  <- nrow(grid_)
  n_done_ <- sum(grid_$Done)
  n_todo_ <- n_all_ - n_done_

  # Pre-flight: what is already done, what still needs to run.
  # Note nDone / nToRun are deliberately NOT named Done: dplyr evaluates
  # summarise() expressions sequentially, so a column named Done would shadow the
  # logical Done column for every later expression in the same call.
  cli::cli_h2("BERT sweep -- {n_all_} run(s), serial{if (.overwrite) ' (overwrite)' else ''}")
  cli::cli_alert_info("Already done: {n_done_}  |  To run: {n_todo_}")
  grid_ |>
    dplyr::summarise(All = dplyr::n(), nDone = sum(.data$Done), nToRun = sum(!.data$Done),
                     .by = label_col) |>
    dplyr::arrange(.data$label_col) |>
    purrr::pwalk(\(label_col, All, nDone, nToRun)
      cli::cli_alert_info("{label_col}: {nDone}/{All} done, {nToRun} to run"))

  if (n_todo_ == 0L) {
    cli::cli_alert_success("Nothing to do -- all {n_all_} run(s) already on disk.")
    return(invisible(grid_))
  }

  # Train only the missing rows. No cli progress bar here: each run takes several
  # minutes, so a bar would tick once per fold and its ETA would be undefined for
  # the whole of the first run. An explicit status line before each fit is more
  # useful -- it names what is training now and, once one fit has completed,
  # projects a wall-clock finish time from the running mean.
  todo_ <- grid_ |> dplyr::filter(!.data$Done) |> dplyr::select(dplyr::all_of(need_))
  started_ <- Sys.time()

  fmt_dur_ <- function(.sec) {
    if (!is.finite(.sec) || .sec < 0) return("--")
    h_ <- floor(.sec / 3600)
    m_ <- floor((.sec %% 3600) / 60)
    if (h_ > 0) sprintf("%dh%02dm", h_, m_) else sprintf("%dm", m_)
  }

  for (i_ in seq_len(n_todo_)) {
    row_ <- todo_[i_, ]

    elapsed_ <- as.numeric(difftime(Sys.time(), started_, units = "secs"))
    eta_ <- if (i_ == 1L) {
      "ETA --"
    } else {
      per_ <- elapsed_ / (i_ - 1L)
      rem_ <- per_ * (n_todo_ - i_ + 1L)
      paste0("ETA ", fmt_dur_(rem_), ", done ~", format(Sys.time() + rem_, "%H:%M"))
    }
    tag_ <- sprintf("%s L%d E%s LR%s W%d fold %d",
                    row_$label_col, as.integer(row_$max_len),
                    formatC(row_$epochs, format = "g"),
                    formatC(row_$lr, format = "g"),
                    as.integer(as.logical(row_$class_weights)),
                    as.integer(row_$fold))
    cli::cli_alert_info(
      "[{i_}/{n_todo_}] {tag_} | elapsed {fmt_dur_(elapsed_)} | {eta_}"
    )

    bert_train(
      .path_data     = .path_data,
      .label_col     = row_$label_col,
      .model         = row_$model,
      .lr            = row_$lr,
      .max_len       = as.integer(row_$max_len),
      .epochs        = row_$epochs,
      .class_weights = row_$class_weights,
      .test_fold     = as.integer(row_$fold),
      .text_col      = .text_col,
      .batch_size    = .batch_size,
      .seed          = .seed,
      .runs_root     = .runs_root,
      .overwrite     = .overwrite,
      .verbose       = .verbose,
      ...
    )
  }

  total_ <- as.numeric(difftime(Sys.time(), started_, units = "secs"))
  cli::cli_alert_success(
    "Sweep complete -- {n_todo_} trained, {n_done_} already cached. Total {fmt_dur_(total_)}."
  )
  invisible(grid_)
}


# Deploy: crowned config + final all-data fit -----------------------------

#' Read the crowned (top macro-F1) configuration's hyperparameters
#'
#' Picks the leaderboard's top config for one task and returns its hyperparameters
#' as a list -- the recipe to refit on all data. Reads them back from the runs'
#' recorded metrics (any fold row carries the full config), so the deployed model
#' is guaranteed to match the configuration that was actually crowned by CV.
#'
#' @param .tab_overall Output of clf_load_overall().
#' @param .label_col Task to crown ("ClassDetailed", "ClassBroad", "AmendType").
#' @param .max_len Integer or NULL. Restrict the search to runs at this context length, so a
#'   configuration can be selected under a deployment constraint rather than absolutely. NULL ranks
#'   across every context length.
#' @return Named list: config_name, label_col, model, text_col, max_len, epochs,
#'   lr, class_weights, seed.
bert_crowned_config <- function(.tab_overall, .label_col = "ClassDetailed", .max_len = NULL) {
  if (FALSE) {
    .tab_overall <- clf_load_overall(.runs_roots = .lP$Runs$Bert)
    .label_col   <- "ClassDetailed"
    .max_len     <- NULL
  }
  cand_ <- .tab_overall |> dplyr::filter(.data$LabelCol == .label_col)
  if (!is.null(.max_len)) {
    cand_ <- cand_ |> dplyr::filter(as.integer(.data$MaxLen) == as.integer(.max_len))
    if (nrow(cand_) == 0L) {
      cli::cli_abort("No {(.label_col)} runs at max_len {(.max_len)}.")
    }
  }
  best_ <- clf_leaderboard(cand_) |> dplyr::slice(1) |> dplyr::pull(.data$ConfigName)
  row_  <- .tab_overall |> dplyr::filter(.data$ConfigName == best_) |> dplyr::slice(1)
  list(
    config_name   = best_,
    label_col     = row_$LabelCol,
    model         = row_$Model,
    text_col      = row_$TextCol,
    max_len       = as.integer(row_$MaxLen),
    epochs        = row_$Epochs,
    lr            = row_$LR,
    class_weights = as.logical(row_$ClassWeights),
    seed          = as.integer(row_$Seed)
  )
}

#' Fit the crowned configuration on ALL labelled docs and save the model
#'
#' Invokes the trainer in --fit-final mode: no held-out fold, no evaluation, just
#' one model trained on every labelled document and persisted (weights, tokenizer,
#' label map). This is the deployable artifact 03E / downstream apply loads. The
#' CV estimate already certified the configuration, so re-introducing a hold-out
#' here would only throw data away.
#'
#' @param .path_data Prepared parquet path (from 03A).
#' @param .config Crowned-config list from bert_crowned_config().
#' @param .model_dir Directory to write the final model under (one subfolder
#'   <config_name>__FINAL/model).
#' @param .batch_size Batch size.
#' @param .overwrite Logical. Refit even if a final model already exists.
#' @param .verbose Logical. Stream the live training output to the console.
#' @param .python,.script Paths to the venv python and the trainer script.
#' @return Invisibly the path to the saved model directory.
bert_fit_final <- function(.path_data, .config,
                           .model_dir = here::here("2_output", "03B-ClassifyTrainBERT", "model_final"),
                           .batch_size = 32L,
                           .overwrite = FALSE,
                           .verbose = FALSE,
                           .python = here::here("contracts-engine", ".venv", "bin", "python"),
                           .script = here::here("contracts-engine", "classify_train.py")) {
  if (FALSE) {
    .path_data <- .lP$Input$Prepared
    .config    <- bert_crowned_config(tab_overall, "ClassDetailed")
    .model_dir <- .lP$Output$ModelFinal
  }
  if (!fs::file_exists(.python)) cli::cli_abort("Python not found at {(.python)}")
  if (!fs::file_exists(.script)) cli::cli_abort("Trainer not found at {(.script)}")

  args_ <- c(
    .script,
    "--data", .path_data,
    "--label-col", .config$label_col,
    "--text-col", .config$text_col,
    "--model", .config$model,
    "--max-len", as.character(.config$max_len),
    "--epochs", as.character(.config$epochs),
    "--lr", as.character(.config$lr),
    "--batch-size", as.character(.batch_size),
    "--seed", as.character(.config$seed),
    "--runs-root", .model_dir,
    "--fit-final"
  )
  if (isTRUE(.config$class_weights)) args_ <- c(args_, "--class-weights")
  if (.overwrite)                    args_ <- c(args_, "--overwrite")
  if (.verbose)                      args_ <- c(args_, "--verbose")

  cli::cli_alert_info("Fitting crowned config on ALL labelled docs: {(.config$config_name)}")
  model_path_ <- fs::path(.model_dir, paste0(.config$config_name, "__FINAL"), "model")

  if (.verbose) {
    status_ <- system2(.python, args = args_, stdout = "", stderr = "")
    if (!identical(status_, 0L)) cli::cli_abort("fit-final failed (exit {status_})")
    cli::cli_alert_success("Saved deployable model: {model_path_}")
    return(invisible(model_path_))
  }

  out_ <- suppressWarnings(system2(.python, args = args_, stdout = TRUE, stderr = ""))
  status_ <- attr(out_, "status")
  if (!is.null(status_) && status_ != 0L) cli::cli_abort("fit-final failed (exit {status_})")

  if (any(grepl("^\\[skip", out_))) {
    cli::cli_alert_info("final model already on disk: {model_path_}")
  } else {
    cli::cli_alert_success("Saved deployable model: {model_path_}")
  }
  invisible(model_path_)
}


# Saved classification (per-doc predictions + full probability vector) -----

#' Pool out-of-fold class probabilities for one configuration (long form)
#'
#' Sibling of clf_pool_predictions for the engine's probabilities.parquet: one row
#' per (DocID, Class) carrying the softmax probability from the fold model that did
#' not train on that doc.
#'
#' @param .runs_roots Character vector of one or more runs directories.
#' @param .config_name ConfigName string.
#' @return Long tibble: ConfigName, DocID, Class, Prob.
bert_pool_probabilities <- function(.runs_roots, .config_name) {
  paths_ <- .runs_roots |>
    purrr::map(\(.r) fs::dir_ls(.r, recurse = TRUE, glob = "*probabilities.parquet")) |>
    purrr::list_c()
  paths_ <- paths_[!grepl("_smoke", paths_)]
  purrr::map(paths_, arrow::read_parquet) |>
    purrr::list_rbind() |>
    dplyr::filter(.data$ConfigName == .config_name)
}

#' Build the per-document classification table for one configuration
#'
#' Joins the pooled out-of-fold predictions to the pooled probability vectors and
#' derives the confidence signals: the top-1 class and probability, the runner-up
#' (top-2) class and probability, their Margin (top1 - top2), and whether the
#' prediction was Correct. The full per-class probability is carried as one P_<Class>
#' column each, so downstream can recompute anything. Optionally attaches
#' ClassDetailed2 / LabelRound from the prepared sample.
#'
#' @param .runs_roots Character vector of one or more runs directories.
#' @param .config_name ConfigName string (e.g. the crowned detailed config).
#' @param .tab_prep Optional prepared sample; if given, ClassDetailed2 and
#'   LabelRound are joined on.
#' @return Tibble: DocID, Fold, TrueLabel, PredLabel, Top1Class, Top1Prob,
#'   Top2Class, Top2Prob, Margin, Correct, [ClassDetailed2, LabelRound], P_<Class>...
bert_classification <- function(.runs_roots, .config_name, .tab_prep = NULL) {
  if (FALSE) {
    .runs_roots  <- .lP$Runs$Bert
    .config_name <- best_det_
    .tab_prep    <- tab_prep
  }
  preds_ <- clf_pool_predictions(.runs_roots, .config_name) |>
    dplyr::select(DocID, Fold, TrueLabel, PredLabel, Score)
  probs_ <- bert_pool_probabilities(.runs_roots, .config_name)

  # top-1 / top-2 per doc
  top_ <- probs_ |>
    dplyr::arrange(.data$DocID, dplyr::desc(.data$Prob)) |>
    dplyr::mutate(Rank = dplyr::row_number(), .by = DocID) |>
    dplyr::filter(.data$Rank <= 2L) |>
    dplyr::select(DocID, Rank, Class, Prob) |>
    tidyr::pivot_wider(names_from = "Rank", values_from = c("Class", "Prob"), names_sep = "")

  # full per-class probability, one P_<Class> column each
  probs_wide_ <- probs_ |>
    tidyr::pivot_wider(id_cols = DocID, names_from = "Class", values_from = "Prob",
                       names_prefix = "P_")

  out_ <- preds_ |>
    dplyr::left_join(top_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      Top1Class = .data$Class1,
      Top1Prob  = .data$Prob1,
      Top2Class = .data$Class2,
      Top2Prob  = .data$Prob2,
      Margin    = .data$Prob1 - .data$Prob2,
      Correct   = .data$PredLabel == .data$TrueLabel
    ) |>
    dplyr::select(DocID, Fold, TrueLabel, PredLabel,
                  Top1Class, Top1Prob, Top2Class, Top2Prob, Margin, Correct)

  if (!is.null(.tab_prep)) {
    out_ <- out_ |>
      dplyr::left_join(
        .tab_prep |> dplyr::select(DocID, ClassDetailed2, LabelRound),
        by = dplyr::join_by(DocID)
      )
  }

  out_ |> dplyr::left_join(probs_wide_, by = dplyr::join_by(DocID))
}

#' Save the per-document classification to disk (parquet, optional csv / dta)
#'
#' Parquet is the canonical output. For the Stata hand-off, .dta = TRUE writes a
#' copy with the readable P_<Class> probability columns renamed P01..Pnn (Stata
#' forbids spaces / colons / slashes in variable names) plus a codebook CSV mapping
#' the safe names back to the class labels.
#'
#' @param .tab Per-doc classification from bert_classification().
#' @param .dir Output directory (created if needed).
#' @param .stem File stem (default "crowned_classification").
#' @param .csv Logical. Also write a CSV.
#' @param .dta Logical. Also write a Stata .dta with sanitised names + codebook.
#' @return Invisibly the output directory.
bert_save_classification <- function(.tab, .dir, .stem = "crowned_classification",
                                     .csv = TRUE, .dta = FALSE) {
  fs::dir_create(.dir)
  pq_ <- fs::path(.dir, paste0(.stem, ".parquet"))
  arrow::write_parquet(.tab, pq_)
  cli::cli_alert_success("Wrote {pq_} ({nrow(.tab)} docs, {ncol(.tab)} cols)")

  if (.csv) {
    csv_ <- fs::path(.dir, paste0(.stem, ".csv"))
    readr::write_csv(.tab, csv_)
    cli::cli_alert_success("Wrote {csv_}")
  }

  if (.dta) {
    pcols_ <- grep("^P_", names(.tab), value = TRUE)
    safe_  <- sprintf("P%02d", seq_along(pcols_))
    dta_tab_ <- .tab |>
      dplyr::rename_with(\(.n) safe_[match(.n, pcols_)], dplyr::all_of(pcols_))
    dta_  <- fs::path(.dir, paste0(.stem, ".dta"))
    code_ <- fs::path(.dir, paste0(.stem, "_codebook.csv"))
    haven::write_dta(dta_tab_, dta_)
    readr::write_csv(tibble::tibble(Variable = safe_, Class = sub("^P_", "", pcols_)), code_)
    cli::cli_alert_success("Wrote {dta_} (+ codebook; P_* renamed P01..)")
  }

  invisible(.dir)
}


# Confidence / calibration overviews --------------------------------------
# All read the per-doc classification table (bert_classification). "Is BERT's 80%
# confidence right 80% of the time?" is calibration; "first vs second guess" is the
# margin; "at what confidence is it right" is the risk-coverage (selective) curve.

#' Reliability table: confidence bin vs empirical accuracy
#'
#' Bins the top-1 probability into equal-width bins and reports, per bin, the mean
#' predicted confidence and the empirical accuracy. A perfectly calibrated model
#' has Accuracy == MeanConf in every bin (Gap == 0); fine-tuned transformers are
#' usually over-confident (Gap < 0 in the high bins).
#'
#' @param .tab_class Per-doc classification (needs Top1Prob, Correct).
#' @param .n_bins Integer. Number of equal-width confidence bins.
#' @return Tibble: Bin, NDocs, MeanConf, Accuracy, Gap.
bert_calibration <- function(.tab_class, .n_bins = 10L) {
  brks_ <- seq(0, 1, length.out = .n_bins + 1L)
  .tab_class |>
    dplyr::mutate(Bin = cut(.data$Top1Prob, breaks = brks_, include.lowest = TRUE)) |>
    dplyr::summarise(
      NDocs    = dplyr::n(),
      MeanConf = mean(.data$Top1Prob),
      Accuracy = mean(.data$Correct),
      .by = Bin
    ) |>
    dplyr::arrange(.data$Bin) |>
    dplyr::mutate(Gap = .data$Accuracy - .data$MeanConf)
}

#' Calibration summary scalars (ECE, MCE, multiclass Brier)
#'
#' ECE is the doc-weighted mean absolute gap between confidence and accuracy across
#' bins; MCE the worst bin's gap; Brier the mean squared error of the full
#' probability vector against the one-hot truth (lower is better on all three).
#'
#' @param .tab_class Per-doc classification (needs Top1Prob, Correct, TrueLabel,
#'   and the P_<Class> probability columns).
#' @param .n_bins Integer. Bins for ECE / MCE.
#' @return One-row tibble: NBins, ECE, MCE, Brier, MeanConf, Accuracy.
bert_calibration_metrics <- function(.tab_class, .n_bins = 10L) {
  bins_ <- bert_calibration(.tab_class, .n_bins)
  n_    <- nrow(.tab_class)
  ece_  <- sum(bins_$NDocs / n_ * abs(bins_$Accuracy - bins_$MeanConf))
  mce_  <- max(abs(bins_$Accuracy - bins_$MeanConf))

  pcols_   <- grep("^P_", names(.tab_class), value = TRUE)
  classes_ <- sub("^P_", "", pcols_)
  prob_m_  <- as.matrix(.tab_class[pcols_])
  onehot_  <- outer(.tab_class$TrueLabel, classes_, `==`) * 1
  brier_   <- mean(rowSums((prob_m_ - onehot_)^2))

  tibble::tibble(
    NBins    = .n_bins,
    ECE      = ece_,
    MCE      = mce_,
    Brier    = brier_,
    MeanConf = mean(.tab_class$Top1Prob),
    Accuracy = mean(.tab_class$Correct)
  )
}

#' Reliability diagram (confidence vs accuracy; dashed = perfect calibration)
#' @param .tab_class Per-doc classification.
#' @param .n_bins Integer. Confidence bins.
#' @return A ggplot.
bert_plot_reliability <- function(.tab_class, .n_bins = 10L) {
  bins_ <- bert_calibration(.tab_class, .n_bins)
  p_ <- bins_ |>
    ggplot2::ggplot(ggplot2::aes(x = MeanConf, y = Accuracy)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(size = NDocs)) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_size_continuous(guide = "none") +
    ggplot2::labs(x = "Mean predicted confidence", y = "Empirical accuracy")
  clf_apply_theme(p_)
}

#' Decision margin (top-1 minus top-2) summarised by outcome
#'
#' A large margin means BERT was decisive; a small one means it was torn between
#' two classes. If correct predictions carry a clearly larger margin than incorrect
#' ones, the margin is a usable confidence signal (for routing / a reject option).
#'
#' @param .tab_class Per-doc classification (needs Margin, Top1Prob, Correct).
#' @return Tibble: Correct, NDocs, MeanMargin, MedianMargin, MeanTop1.
bert_margin_summary <- function(.tab_class) {
  .tab_class |>
    dplyr::summarise(
      NDocs        = dplyr::n(),
      MeanMargin   = mean(.data$Margin),
      MedianMargin = stats::median(.data$Margin),
      MeanTop1     = mean(.data$Top1Prob),
      .by = Correct
    ) |>
    dplyr::arrange(dplyr::desc(.data$Correct))
}

#' Margin distribution for correct vs incorrect predictions
#' @param .tab_class Per-doc classification.
#' @return A ggplot.
bert_plot_margin <- function(.tab_class) {
  p_ <- .tab_class |>
    dplyr::mutate(Outcome = dplyr::if_else(.data$Correct, "Correct", "Incorrect")) |>
    ggplot2::ggplot(ggplot2::aes(x = Margin, fill = Outcome)) +
    ggplot2::geom_density(alpha = 0.5, color = NA) +
    ggplot2::scale_fill_manual(values = c(Correct = "grey30", Incorrect = "grey75")) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Top-1 minus top-2 probability (margin)", y = "Density", fill = NULL)
  clf_apply_theme(p_)
}

#' Risk-coverage: accuracy on the kept subset at rising confidence floors
#'
#' For each confidence threshold, keep only docs whose top-1 probability clears it
#' and report Coverage (share kept) and Accuracy on that subset. This is the BERT
#' twin of 03C's selective-classification curve: it says how accurately the model
#' can label a confident fraction, the basis for a reject option or routing.
#'
#' @param .tab_class Per-doc classification (needs Top1Prob, Correct).
#' @param .thresholds Numeric vector of confidence floors.
#' @return Tibble: Threshold, Coverage, NKept, Accuracy.
bert_confidence_accuracy <- function(.tab_class, .thresholds = seq(0, 0.95, by = 0.05)) {
  n_ <- nrow(.tab_class)
  purrr::map(.thresholds, function(t_) {
    kept_ <- .tab_class |> dplyr::filter(.data$Top1Prob >= t_)
    tibble::tibble(
      Threshold = t_,
      Coverage  = nrow(kept_) / n_,
      NKept     = nrow(kept_),
      Accuracy  = if (nrow(kept_) == 0L) NA_real_ else mean(kept_$Correct)
    )
  }) |>
    purrr::list_rbind()
}

#' Risk-coverage curve (accuracy on kept vs coverage)
#' @param .tab_class Per-doc classification.
#' @param .thresholds Numeric vector of confidence floors.
#' @return A ggplot.
bert_plot_risk_coverage <- function(.tab_class, .thresholds = seq(0, 0.95, by = 0.05)) {
  rc_ <- bert_confidence_accuracy(.tab_class, .thresholds)
  p_ <- rc_ |>
    ggplot2::ggplot(ggplot2::aes(x = Coverage, y = Accuracy)) +
    ggplot2::geom_line(linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(color = Threshold), size = 1.8) +
    ggplot2::scale_color_gradient(low = "grey75", high = "grey15") +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Coverage (share of docs kept)", y = "Accuracy on kept", color = "Min conf")
  clf_apply_theme(p_)
}


# Grid construction ---------------------------------------------------------

#' Summarise a sweep grid, and project how long it will take
#'
#' Prints the shape of a sweep -- configurations, folds and runs per arm -- before any GPU time is
#' spent on it. Where completed runs are supplied, it also projects the wall-clock cost of the runs
#' still outstanding, which is the number that actually decides whether a sweep is worth starting.
#'
#' The projection fits run duration on the product of epochs and context length, which is what
#' compute scales with, plus a fixed per-run overhead for tokenisation, model loading and evaluation.
#' Both terms are estimated from runs already on disk, so the estimate is this machine's throughput
#' rather than a generic figure.
#'
#' @param .grid Sweep grid as assembled in the runbook.
#' @param .by Character vector of grid columns to group the summary by. Defaults to task and context
#'   length, which are what drive both the interest and the cost of a sweep.
#' @param .tab_overall Bound per-fold metrics from clf_load_overall(), or NULL. Supplying it enables
#'   the runtime projection.
#' @param .runs_root Directory holding the run folders; used to decide which rows are already done.
#' @param .text_col,.batch_size,.seed The constants that enter a run key. These must match the values
#'   passed to bert_sweep(), or the report will describe a different sweep from the one that runs.
#' @return Invisibly a per-arm summary tibble.
bert_report_grid <- function(.grid, .tab_overall = NULL,
                             .by         = c("label_col", "max_len"),
                             .runs_root  = here::here("2_output", "03B-ClassifyTrainBERT", "runs"),
                             .text_col   = "Text",
                             .batch_size = 32L,
                             .seed       = 42L) {
  if (FALSE) {
    .grid        <- grid_all
    .tab_overall <- clf_load_overall(.runs_roots = .lP$Runs$Bert)
    .by          <- c("label_col", "max_len")
    .runs_root   <- .lP$Runs$Bert
    .text_col    <- "Text"
    .batch_size  <- 32L
    .seed        <- 42L
  }
  fmt_dur_ <- function(.sec) {
    if (!is.finite(.sec) || .sec <= 0) return("--")
    d_ <- floor(.sec / 86400)
    h_ <- floor((.sec %% 86400) / 3600)
    m_ <- floor((.sec %% 3600) / 60)
    if (d_ > 0) sprintf("%dd %02dh", d_, h_) else if (h_ > 0) sprintf("%dh %02dm", h_, m_) else sprintf("%dm", m_)
  }

  grid_ <- .grid |>
    dplyr::mutate(Units = .data$epochs * .data$max_len)   # compute scales with this product

  # Mark rows already on disk. This reuses bert_run_key() and bert_done_keys(), the same matching
  # bert_sweep() itself performs, rather than reconstructing it with a join: a second, independent
  # rule would eventually disagree with the first, and the report would then describe a sweep other
  # than the one about to run. It also sidesteps joining on floating-point learning rates.
  done_keys_ <- bert_done_keys(.runs_root)   # one scan of the runs tree, not one per row

  grid_ <- grid_ |>
    dplyr::mutate(
      RowKey = bert_run_key(
        .label_col = .data$label_col, .model = .data$model, .text_col = .text_col,
        .max_len = .data$max_len, .epochs = .data$epochs, .batch_size = .batch_size,
        .lr = .data$lr, .class_weights = .data$class_weights, .seed = .seed, .fold = .data$fold
      ),
      # The same key with the fold zeroed identifies a configuration rather than a run. Counting
      # distinct configurations this way avoids pick(), which cannot select grouping columns and so
      # breaks whenever .by names one of the hyperparameters.
      ConfigKey = bert_run_key(
        .label_col = .data$label_col, .model = .data$model, .text_col = .text_col,
        .max_len = .data$max_len, .epochs = .data$epochs, .batch_size = .batch_size,
        .lr = .data$lr, .class_weights = .data$class_weights, .seed = .seed, .fold = 0L
      ),
      Done = .data$RowKey %in% done_keys_
    )

  out_ <- grid_ |>
    dplyr::summarise(
      Configs = dplyr::n_distinct(.data$ConfigKey),
      Folds   = dplyr::n_distinct(.data$fold),
      Runs    = dplyr::n(),
      nDone   = sum(.data$Done),
      nToRun  = sum(!.data$Done),
      Units   = sum(.data$Units[!.data$Done]),
      .by = dplyr::all_of(.by)
    ) |>
    dplyr::arrange(dplyr::desc(.data$nToRun))

  # Fit duration on observed runs: intercept is fixed overhead, slope is seconds per unit of
  # epochs x tokens. Needs at least two distinct workloads to separate the two terms.
  fit_ <- NULL
  if (!is.null(.tab_overall) && "DurationSec" %in% names(.tab_overall)) {
    obs_ <- .tab_overall |>
      dplyr::filter(!.data$Smoke, !is.na(.data$DurationSec)) |>
      dplyr::transmute(Sec = .data$DurationSec, Units = .data$Epochs * as.integer(.data$MaxLen))
    if (nrow(obs_) >= 3L && dplyr::n_distinct(obs_$Units) >= 2L) {
      fit_ <- stats::lm(Sec ~ Units, data = obs_)
    }
  }

  cli::cli_h2("Sweep grid")
  show_ <- out_ |> dplyr::select(dplyr::all_of(.by), Configs, Folds, Runs, nDone, nToRun)
  if (!is.null(fit_)) {
    show_ <- show_ |>
      dplyr::mutate(
        Est = purrr::map2_chr(out_$nToRun, out_$Units, \(.n, .u) {
          if (.n == 0L) "--" else fmt_dur_(.n * stats::coef(fit_)[[1]] + .u * stats::coef(fit_)[[2]])
        })
      )
  }
  clf_say_table(.tab = show_)
  cli::cli_text("")

  cli::cli_alert_info(
    "{sum(out_$Runs)} run{?s} defined, {sum(out_$nDone)} already on disk, {sum(out_$nToRun)} outstanding."
  )
  if (!is.null(fit_)) {
    total_ <- sum(out_$nToRun) * stats::coef(fit_)[[1]] + sum(out_$Units) * stats::coef(fit_)[[2]]
    cli::cli_alert_info(
      "Projected remaining time {.strong {fmt_dur_(total_)}}, from {stats::nobs(fit_)} completed run{?s}: \
       {round(stats::coef(fit_)[[1]])}s fixed overhead plus \
       {signif(stats::coef(fit_)[[2]], 3)}s per epoch-token unit."
    )
  } else {
    cli::cli_alert_info("Supply .tab_overall to project the remaining wall-clock time.")
  }
  invisible(out_)
}

# Configuration selection ---------------------------------------------------

#' Name of the best-scoring configuration for one task
#'
#' Thin wrapper over clf_leaderboard() that returns only the winning ConfigName. Exists so the
#' runbook does not have to repeat a filter / rank / extract pipeline once per task.
#'
#' @param .tab_overall Bound per-fold metrics, from clf_load_overall().
#' @param .label_col Task to rank: "ClassDetailed", "ClassBroad" or "AmendType".
#' @return Character scalar: the ConfigName ranked first by mean macro-F1.
bert_best_config <- function(.tab_overall, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab_overall <- clf_load_overall(.runs_roots = .lP$Runs$Bert)
    .label_col   <- "ClassDetailed"
  }
  .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col) |>
    clf_leaderboard() |>
    dplyr::slice(1) |>
    dplyr::pull(.data$ConfigName)
}

#' Out-of-fold predictions for the best configuration of one task
#'
#' Combines configuration selection and prediction pooling, which are always performed together.
#' Each document is predicted exactly once, by the fold model that did not see it in training.
#'
#' @param .tab_overall Bound per-fold metrics, from clf_load_overall().
#' @param .runs_roots Directory (or directories) holding the run folders.
#' @param .label_col Task to pool: "ClassDetailed", "ClassBroad" or "AmendType".
#' @return Tibble of pooled out-of-fold predictions: DocID, TrueLabel, PredLabel, ConfigName, Fold.
bert_best_predictions <- function(.tab_overall, .runs_roots, .label_col = "ClassDetailed") {
  clf_pool_predictions(
    .runs_roots  = .runs_roots,
    .config_name = bert_best_config(.tab_overall = .tab_overall, .label_col = .label_col)
  )
}


# Taxonomy consistency ------------------------------------------------------

#' Agreement between the two contract-type models across taxonomy levels
#'
#' The broad taxonomy is a strict roll-up of the detailed one: every detailed category has exactly
#' one broad parent. Two independently trained models therefore make a checkable joint claim. Where
#' the detailed model's prediction maps to a different broad category than the broad model predicts
#' directly, at least one of them is wrong, and a released dataset carrying both would contain an
#' internal contradiction.
#'
#' This is a property of the models, not of the labels: the labels are consistent by construction.
#'
#' @param .tab_pred_detailed Pooled out-of-fold predictions from the detailed model.
#' @param .tab_pred_broad Pooled out-of-fold predictions from the broad model.
#' @param .tab_prep Prepared sample, supplying the detailed-to-broad mapping.
#' @return Tibble, one row per document: DocID, TrueBroad, PredDetailed, PredDetailedRolled,
#'   PredBroad, Agree, RolledCorrect, BroadCorrect.
bert_hierarchy_agreement <- function(.tab_pred_detailed, .tab_pred_broad, .tab_prep) {
  if (FALSE) {
    .tab_pred_detailed <- pred_det
    .tab_pred_broad    <- pred_broad
    .tab_prep          <- tab_prep
  }
  map_ <- .tab_prep |> dplyr::distinct(ClassDetailed, ClassBroad)

  .tab_pred_detailed |>
    dplyr::select(DocID, PredDetailed = PredLabel) |>
    dplyr::left_join(
      y  = map_ |> dplyr::rename(PredDetailed = ClassDetailed, PredDetailedRolled = ClassBroad),
      by = dplyr::join_by(PredDetailed)
    ) |>
    dplyr::inner_join(
      y  = .tab_pred_broad |> dplyr::select(DocID, TrueBroad = TrueLabel, PredBroad = PredLabel),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::mutate(
      Agree         = .data$PredDetailedRolled == .data$PredBroad,
      RolledCorrect = .data$PredDetailedRolled == .data$TrueBroad,
      BroadCorrect  = .data$PredBroad == .data$TrueBroad
    )
}

#' Report cross-level agreement and adjudicate the disagreements
#'
#' @param .tab_agree Output of bert_hierarchy_agreement().
#' @param .n Integer. Number of disagreeing category pairs to list.
#' @return Invisibly the disagreement pair table.
bert_report_hierarchy <- function(.tab_agree, .n = 10L) {
  n_    <- nrow(.tab_agree)
  dis_  <- .tab_agree |> dplyr::filter(!.data$Agree)

  cli::cli_h2("Taxonomy consistency -- detailed rolled up vs broad direct")
  clf_say_table(
    .tab = tibble::tibble(
      Outcome = c("Agree", "Disagree"),
      Docs    = c(n_ - nrow(dis_), nrow(dis_)),
      Share   = clf_pct(c((n_ - nrow(dis_)) / n_, nrow(dis_) / n_))
    )
  )

  if (nrow(dis_) == 0L) {
    cli::cli_alert_success("The two models never contradict each other.")
    return(invisible(NULL))
  }

  # Where they disagree, which one was right? This decides which to publish.
  cli::cli_text("")
  clf_say_table(
    .tab = tibble::tibble(
      Verdict = c("Rolled-up detailed correct", "Broad direct correct", "Both wrong"),
      Docs    = c(
        sum(dis_$RolledCorrect),
        sum(dis_$BroadCorrect),
        sum(!dis_$RolledCorrect & !dis_$BroadCorrect)
      )
    ) |>
      dplyr::mutate(Share = clf_pct(.data$Docs / nrow(dis_))),
    .title = "Adjudicating the disagreements"
  )

  pairs_ <- dis_ |>
    dplyr::count(RolledUpTo = .data$PredDetailedRolled, BroadSaid = .data$PredBroad, name = "Docs") |>
    dplyr::arrange(dplyr::desc(.data$Docs))

  cli::cli_text("")
  clf_say_table(
    .tab   = utils::head(pairs_, .n),
    .title = paste0("Where they part company (top ", min(.n, nrow(pairs_)), " of ", nrow(pairs_), ")")
  )
  cli::cli_text("")
  cli::cli_alert_info(
    "Publishing both columns from separate models would put these documents in two categories at \\
     once. Deriving the broad label by roll-up makes that impossible by construction."
  )
  invisible(pairs_)
}


# Cross-task overview -------------------------------------------------------

#' Headline result for all three tasks, side by side
#'
#' Collects the winning configuration and its cross-validated scores for each task into one table.
#' This is the summary a reader needs; the per-task sections above supply the detail behind it.
#'
#' @param .tab_overall Bound per-fold metrics, from clf_load_overall().
#' @param .runs_roots Directory (or directories) holding the run folders.
#' @param .label_cols Character vector of tasks to include.
#' @return Invisibly the summary tibble.
bert_report_summary <- function(.tab_overall, .runs_roots,
                                .label_cols = c("ClassDetailed", "ClassBroad", "AmendType")) {
  if (FALSE) {
    .tab_overall <- clf_load_overall(.runs_roots = .lP$Runs$Bert)
    .runs_roots  <- .lP$Runs$Bert
    .label_cols  <- c("ClassDetailed", "ClassBroad", "AmendType")
  }
  out_ <- purrr::map(.label_cols, \(.task) {
    cfg_  <- bert_crowned_config(.tab_overall = .tab_overall, .label_col = .task)
    pred_ <- clf_pool_predictions(.runs_roots = .runs_roots, .config_name = cfg_$config_name)
    per_  <- clf_perclass(.tab_pred = pred_)
    sco_  <- clf_scores(.tab_pred = pred_)
    tibble::tibble(
      Task         = .task,
      Classes      = nrow(per_),
      Docs         = sco_$N,
      Encoder      = sub("^.*/", "", cfg_$model),
      MaxLen       = as.integer(cfg_$max_len),
      Epochs       = as.integer(cfg_$epochs),
      LR           = formatC(cfg_$lr, format = "g"),
      Weights      = as.integer(isTRUE(cfg_$class_weights)),
      Accuracy     = round(sco_$Accuracy, 3),
      MacroF1      = round(sco_$F1_macro, 3),
      WeakestClass = paste0(per_$Label[[which.min(per_$F1)]], " (", round(min(per_$F1), 3), ")")
    )
  }) |>
    purrr::list_rbind()

  cli::cli_h2("Headline results")
  clf_say_table(.tab = out_)
  cli::cli_text("")
  cli::cli_alert_info(
    "All figures are pooled out-of-fold: every document is scored by a model that did not train on it."
  )
  invisible(out_)
}

#' Per-class scores for all three tasks in one table
#'
#' @param .tab_overall Bound per-fold metrics, from clf_load_overall().
#' @param .runs_roots Directory (or directories) holding the run folders.
#' @param .label_cols Character vector of tasks to include.
#' @return Invisibly the stacked per-class tibble.
bert_report_perclass_all <- function(.tab_overall, .runs_roots,
                                     .label_cols = c("ClassDetailed", "ClassBroad", "AmendType")) {
  out_ <- purrr::map(.label_cols, \(.task) {
    clf_pool_predictions(
      .runs_roots  = .runs_roots,
      .config_name = bert_best_config(.tab_overall = .tab_overall, .label_col = .task)
    ) |>
      clf_perclass() |>
      dplyr::mutate(Task = .task, .before = 1)
  }) |>
    purrr::list_rbind()

  cli::cli_h2("Per-class scores, all tasks")
  out_ |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))) |>
    clf_say_table()
  cli::cli_text("")
  cli::cli_alert_info(
    "Macro-F1 is the unweighted mean of the F1 column within each task, so the smallest categories \\
     carry the same weight as the largest."
  )
  invisible(out_)
}


# Deploy: fit every task at every context length ----------------------------

#' Refit the best configuration of each task at each context length
#'
#' Deployment is not a single choice. Context length trades accuracy against inference cost, and on a
#' corpus of any size that trade-off is decided by whoever runs the classifier, not by whoever trained
#' it. Fitting the best configuration at each length -- rather than only at the length that won
#' overall -- means the decision can be made later, with the cost figures in hand, without retraining.
#'
#' Within each task and length, the configuration is the one cross-validation selected under that
#' constraint; every other hyperparameter is still chosen on the evidence.
#'
#' @param .path_data Prepared sample written by the sample-construction script.
#' @param .tab_overall Bound per-fold metrics, from clf_load_overall().
#' @param .model_dir Destination directory for deployable models.
#' @param .label_cols Character vector of tasks to fit.
#' @param .max_lens Integer vector of context lengths to fit at.
#' @param .batch_size Integer. Training batch size.
#' @param .overwrite Logical. Refit even where a model for that configuration already exists.
#' @param .verbose Logical. Stream per-epoch training loss to the console.
#' @return Invisibly a tibble of the configurations fitted: Task, MaxLen, ConfigName, MacroF1.
bert_fit_final_all <- function(.path_data, .tab_overall, .model_dir,
                               .label_cols = c("ClassDetailed", "ClassBroad", "AmendType"),
                               .max_lens   = c(256L, 512L),
                               .batch_size = 32L, .overwrite = FALSE, .verbose = TRUE) {
  if (FALSE) {
    .path_data   <- .lP$Input$Prepared
    .tab_overall <- clf_load_overall(.runs_roots = .lP$Runs$Bert)
    .model_dir   <- .lP$Output$ModelFinal
    .label_cols  <- c("ClassDetailed", "ClassBroad", "AmendType")
    .max_lens    <- c(256L, 512L)
  }
  plan_ <- tidyr::expand_grid(Task = .label_cols, MaxLen = as.integer(.max_lens))

  cli::cli_h2("Deploying {nrow(plan_)} model{?s}: {length(.label_cols)} task{?s} x {length(.max_lens)} context length{?s}")

  out_ <- purrr::pmap(plan_, \(Task, MaxLen) {
    cfg_ <- bert_crowned_config(.tab_overall = .tab_overall, .label_col = Task, .max_len = MaxLen)
    board_ <- .tab_overall |>
      dplyr::filter(.data$LabelCol == Task) |>
      clf_leaderboard() |>
      dplyr::filter(.data$ConfigName == cfg_$config_name)

    bert_fit_final(
      .path_data  = .path_data,
      .config     = cfg_,
      .model_dir  = .model_dir,
      .batch_size = .batch_size,
      .overwrite  = .overwrite,
      .verbose    = .verbose
    )

    tibble::tibble(
      Task    = Task,
      MaxLen  = MaxLen,
      Config  = clf_config_label(.config_name = cfg_$config_name),
      MacroF1 = round(board_$F1macro_mean[[1]], 3)
    )
  }) |>
    purrr::list_rbind()

  cli::cli_text("")
  clf_say_table(.tab = out_, .title = "Deployed models")
  cli::cli_text("")
  cli::cli_alert_info(
    "MacroF1 is the cross-validated score of that configuration, so the accuracy cost of the shorter \\
     context length is readable directly from this table."
  )
  invisible(out_)
}
