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
  .python <- .lP$Engine$Python
  .script <- .lP$Engine$Script
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
                       .smoke = FALSE) {
  if (FALSE) {
    .path_data <- .lP$Input$Prepared
    .label_col <- "ClassDetailed"
    .test_fold <- 1L
    .runs_root <- .lP$Runs$Bert
    .smoke <- TRUE
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
  if (!.save_model) args_ <- c(args_, "--no-save-model")
  if (.overwrite) args_ <- c(args_, "--overwrite")
  if (.smoke) args_ <- c(args_, "--smoke")

  # Capture the engine's terse stdout so the console shows exactly one cli line
  # per run; stderr is inherited so a Python traceback still reaches the console.
  # The full per-fold detail lives in <run_dir>/run.log.
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
    f1_ <- stringr::str_match(done_, "f1_macro=([0-9.]+)")[, 2]
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
    .folds <- 1:5
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
  cols_ <- c(
    "LabelCol", "Model", "TextCol", "MaxLen", "Epochs",
    "BatchSize", "LR", "ClassWeights", "Seed", "TestFold"
  )
  paths_ <- .runs_roots |>
    purrr::map(\(.r) if (fs::dir_exists(.r)) {
      fs::dir_ls(.r, recurse = TRUE, glob = "*metrics_overall.parquet")
    } else {
      character(0)
    }) |>
    purrr::list_c()
  paths_ <- paths_[!grepl("_smoke", paths_)]
  if (length(paths_) == 0L) {
    return(character(0))
  }

  purrr::map(paths_, \(.p) arrow::read_parquet(.p, col_select = dplyr::all_of(cols_))) |>
    purrr::list_rbind() |>
    (\(.d) bert_run_key(
      .d$LabelCol, .d$Model, .d$TextCol, .d$MaxLen, .d$Epochs,
      .d$BatchSize, .d$LR, .d$ClassWeights, .d$Seed, .d$TestFold
    ))()
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
#' @param ... Passed through to bert_train (.python, .script, ...).
#' @return Invisibly the grid tagged with its Done status.
bert_sweep <- function(.path_data, .grid,
                       .runs_root = here::here("2_output", "03B-ClassifyTrainBERT", "runs"),
                       .text_col = "Text",
                       .batch_size = 32L,
                       .seed = 42L,
                       .overwrite = FALSE,
                       ...) {
  if (FALSE) {
    .path_data <- .lP$Input$Prepared
    .grid <- tidyr::expand_grid(
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
      RowKey = bert_run_key(
        .data$label_col, .data$model, .text_col, .data$max_len,
        .data$epochs, .batch_size, .data$lr, .data$class_weights,
        .seed, .data$fold
      )
    )
  done_keys_ <- if (.overwrite) character(0) else bert_done_keys(.runs_root)
  grid_ <- grid_ |> dplyr::mutate(Done = .data$RowKey %in% done_keys_)

  n_all_ <- nrow(grid_)
  n_done_ <- sum(grid_$Done)
  n_todo_ <- n_all_ - n_done_

  # Pre-flight: what is already done, what still needs to run.
  cli::cli_h2("BERT sweep -- {n_all_} run(s), serial{if (.overwrite) ' (overwrite)' else ''}")
  cli::cli_alert_info("Already done: {n_done_}  |  To run: {n_todo_}")
  grid_ |>
    dplyr::summarise(
      All = dplyr::n(), Done = sum(.data$Done), ToRun = sum(!.data$Done),
      .by = label_col
    ) |>
    dplyr::arrange(.data$label_col) |>
    purrr::pwalk(\(label_col, All, Done, ToRun)
    cli::cli_alert_info("{label_col}: {Done}/{All} done, {ToRun} to run"))

  if (n_todo_ == 0L) {
    cli::cli_alert_success("Nothing to do -- all {n_all_} run(s) already on disk.")
    return(invisible(grid_))
  }

  # Train only the missing rows, behind a progress bar. A for-loop (not pwalk)
  # keeps the progress bar in the same frame as its updates -- the reliable cli
  # idiom; the Python training dwarfs any loop overhead.
  todo_ <- grid_ |>
    dplyr::filter(!.data$Done) |>
    dplyr::select(dplyr::all_of(need_))
  cli::cli_progress_bar(
    format = paste0(
      "{cli::pb_spin} Training {cli::pb_current}/{cli::pb_total} ",
      "{cli::pb_bar} {cli::pb_percent} | elapsed {cli::pb_elapsed} | ETA {cli::pb_eta}"
    ),
    total = n_todo_, clear = FALSE
  )
  for (i_ in seq_len(n_todo_)) {
    row_ <- todo_[i_, ]
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
      ...
    )
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  cli::cli_alert_success("Sweep complete -- {n_todo_} trained, {n_done_} already cached.")
  invisible(grid_)
}
