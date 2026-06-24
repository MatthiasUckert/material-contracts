# 03-Classification: sample prep, training wrapper, and overview helpers ----
# Engine-agnostic seam: R deals folds and reads text -> prepared parquet;
# Python (contracts-engine/classify_train.py) trains one fold and writes a
# self-describing run folder; R aggregates the run folders for the overview.
#
# Terminology (settled):
#   ClassBroad     = 7-class broad taxonomy (source column Level1)
#   ClassDetailed  = 12-class detailed taxonomy (source column DocClassFinal1)
#   ClassDetailed2 = the second detailed label for dual-class docs (DocClassFinal2,
#                    NA for the ~97% single-class docs). Used for LENIENT scoring
#                    only; training stays single-label on ClassDetailed.
#   AmendType      = amendment task label: Original vs Amended (NA for 75 docs,
#                    dropped by the trainer for that task).
#   LabelRound     = label-source round: Round1 = automated (was S1_fallback),
#                    Round2 = manual / gold (was S2). Round2 are the trustworthy ones.
# Sample: full combined set (Round1 + Round2) by default. DocDesc is NOT used
# here; it is reserved for a later keyword / regex approach.

if (FALSE) {
  .tab_input <- fils_class_sample
  .path_labels <- .lP$Input$ClassificationSample
  .path_data <- .lP$Output$Prepared
  .runs_root <- .lP$Output$RunsDir
}


# Sample preparation ------------------------------------------------------

#' Read one parsed document's full text from its per-document parquet
#'
#' Returns NA on any failure (missing file/column/empty) so the caller can
#' tally misses instead of aborting the whole run.
#'
#' @param .path Character. Path to the per-document parquet.
#' @return Character scalar of document text, or NA.
clf_read_text <- function(.path) {
  tab_ <- tryCatch(arrow::read_parquet(.path), error = function(e) NULL)
  if (is.null(tab_) || !"TextRaw" %in% names(tab_)) {
    return(NA_character_)
  }
  txt_ <- tab_[["TextRaw"]]
  if (length(txt_) == 0L) {
    return(NA_character_)
  }
  paste(txt_, collapse = "\n")
}

#' Build the prepared classification sample with frozen stratified folds
#'
#' Consumes the pre-pathed labelled df as the spine. By default keeps all rounds
#' (the full combined sample); pass .round = "Round2" to restrict to gold labels.
#' Renames source columns to ClassBroad (Level1) and ClassDetailed
#' (DocClassFinal1), carries the dual-class second label (ClassDetailed2) and the
#' amendment label (AmendType), recodes the label source to LabelRound, reads
#' text, and deals a deterministic stratified k-fold assignment (stratified on
#' ClassDetailed; reused for ClassBroad and AmendType so all tasks share folds).
#'
#' @param .tab_input Tibble. Must contain DocID, Path, Level1, DocClassFinal1.
#'   DocClassFinal2, AmendType, Provenance are used if present (NA-filled if not).
#' @param .path_labels Path to label parquet (used only to join Provenance if absent).
#' @param .round Character or NULL. Keep only this round; NULL keeps all.
#' @param .k Integer. Number of folds.
#' @param .seed Integer. RNG seed.
#' @return Tibble: DocID, Text, ClassBroad, ClassDetailed, ClassDetailed2,
#'   AmendType, LabelRound, Fold.
clf_prepare_sample <- function(.tab_input, .path_labels = NULL,
                               .round = NULL, .k = 5L, .seed = 42L) {
  need_ <- c("DocID", "Path", "Level1", "DocClassFinal1")
  miss_cols_ <- setdiff(need_, names(.tab_input))
  if (length(miss_cols_) > 0L) cli::cli_abort("Input missing columns: {miss_cols_}")

  round_lab_ <- if (is.null(.round)) "all rounds" else .round

  # NA-fill optional columns so the select/transmute is stable
  if (!"DocClassFinal2" %in% names(.tab_input)) .tab_input <- .tab_input |> dplyr::mutate(DocClassFinal2 = NA_character_)
  if (!"AmendType" %in% names(.tab_input)) .tab_input <- .tab_input |> dplyr::mutate(AmendType = NA_character_)

  tab_ <- .tab_input |>
    dplyr::select(DocID, Path,
      ClassBroad = Level1, ClassDetailed = DocClassFinal1,
      ClassDetailed2 = DocClassFinal2, AmendType,
      dplyr::any_of("Provenance")
    )

  if (!"Provenance" %in% names(tab_)) {
    if (is.null(.path_labels)) {
      cli::cli_abort("No Provenance column and no labels path to join it from.")
    }
    prov_ <- arrow::read_parquet(.path_labels) |> dplyr::select(DocID, Provenance)
    tab_ <- tab_ |> dplyr::left_join(prov_, by = dplyr::join_by(DocID))
  }

  # recode label source: S1_fallback -> Round1 (automated), S2 -> Round2 (manual)
  tab_ <- tab_ |>
    dplyr::mutate(
      LabelRound = dplyr::case_when(
        .data$Provenance == "S1_fallback" ~ "Round1",
        .data$Provenance == "S2" ~ "Round2",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(-dplyr::any_of("Provenance"))

  if (!is.null(.round)) {
    tab_ <- tab_ |> dplyr::filter(.data$LabelRound == .round)
  }

  tab_ <- tab_ |> dplyr::filter(!is.na(.data$ClassBroad), !is.na(.data$ClassDetailed))
  cli::cli_alert_info("{round_lab_}: {nrow(tab_)} docs after label filter")

  tab_ <- tab_ |> dplyr::filter(fs::file_exists(.data$Path))
  cli::cli_alert_info("Reading {nrow(tab_)} document texts ...")
  tab_ <- tab_ |> dplyr::mutate(Text = purrr::map_chr(.data$Path, clf_read_text, .progress = TRUE))

  ready_ <- tab_ |> dplyr::filter(!is.na(.data$Text), trimws(.data$Text) != "")
  drop_txt_ <- nrow(tab_) - nrow(ready_)
  if (drop_txt_ > 0L) cli::cli_alert_warning("Dropped {drop_txt_} docs with empty/unreadable text")

  thin_ <- ready_ |>
    dplyr::count(.data$ClassDetailed) |>
    dplyr::filter(.data$n < .k)
  if (nrow(thin_) > 0L) {
    cli::cli_alert_warning("Classes with < {(.k)} docs (a fold may lack them): {paste(thin_$ClassDetailed, collapse = ', ')}")
  }

  set.seed(.seed)
  out_ <- ready_ |>
    dplyr::arrange(.data$ClassDetailed, .data$DocID) |>
    dplyr::group_by(.data$ClassDetailed) |>
    dplyr::mutate(Fold = ((sample(dplyr::n()) - 1L) %% .k) + 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      DocID, Text, ClassBroad, ClassDetailed, ClassDetailed2,
      AmendType, LabelRound, Fold
    )

  cli::cli_alert_success("Prepared {nrow(out_)} docs across {(.k)} folds")
  out_
}

#' Write the prepared sample to parquet
#' @param .tab Prepared tibble from clf_prepare_sample().
#' @param .path_out Output parquet path.
clf_write_prepared <- function(.tab, .path_out) {
  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(.tab, .path_out)
  cli::cli_alert_success("Wrote {(.path_out)}")
  invisible(.path_out)
}

#' Per-fold class counts for one granularity (eyeball check)
#' @param .tab Prepared tibble.
#' @param .level One of "ClassDetailed", "ClassBroad", "AmendType".
clf_fold_overview <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType")) {
  .level <- match.arg(.level)
  .tab |>
    dplyr::filter(!is.na(.data[[.level]])) |>
    dplyr::count(Label = .data[[.level]], .data$Fold) |>
    tidyr::pivot_wider(
      names_from = "Fold", values_from = "n",
      values_fill = 0L, names_prefix = "F"
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(Total = sum(dplyr::c_across(dplyr::starts_with("F")))) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(.data$Total))
}


# Training wrapper (shells out to the Python trainer) ---------------------

#' Train one fold by invoking the contracts-engine Python trainer
#'
#' @param .path_data Prepared parquet path.
#' @param .label_col "ClassDetailed", "ClassBroad", or "AmendType".
#' @param .test_fold Integer. Fold held out for testing.
#' @param .text_col Text column to train on (default "Text").
#' @param .class_weights Logical. Use inverse-frequency class weights.
#' @param .model HuggingFace model id.
#' @param .max_len,.epochs,.batch_size,.lr,.seed Hyperparameters.
#' @param .runs_root Directory under which run folders are written.
#' @param .python,.script Paths to the venv python and the trainer script.
#' @param .save_model,.overwrite,.smoke Flags.
#' @return Invisible exit status.
clf_train <- function(.path_data,
                      .label_col = c("ClassDetailed", "ClassBroad", "AmendType"),
                      .test_fold = 1L,
                      .text_col = "Text",
                      .class_weights = FALSE,
                      .model = "roberta-base",
                      .max_len = 512L,
                      .epochs = 6,
                      .batch_size = 32L,
                      .lr = 2e-5,
                      .seed = 42L,
                      .runs_root = here::here("2_output", "03-Classification", "runs"),
                      .python = here::here("contracts-engine", ".venv", "bin", "python"),
                      .script = here::here("contracts-engine", "classify_train.py"),
                      .save_model = FALSE,
                      .overwrite = FALSE,
                      .smoke = FALSE) {
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

  cli::cli_alert_info("Training {(.label_col)}/{(.text_col)} fold {(.test_fold)} (W={(.class_weights)}, {(.model)}, L{(.max_len)}, E{(.epochs)}) ...")
  status_ <- system2(.python, args = args_, stdout = "", stderr = "")
  if (!identical(status_, 0L)) cli::cli_abort("Python trainer failed (exit {status_})")
  invisible(status_)
}

#' Run k-fold CV for one configuration (loops clf_train over folds)
#' @param .path_data Prepared parquet path.
#' @param .label_col "ClassDetailed", "ClassBroad", or "AmendType".
#' @param .folds Integer vector of fold ids to hold out.
#' @param ... Passed through to clf_train (.model, .lr, .max_len, .epochs, ...).
clf_cv <- function(.path_data, .label_col = "ClassDetailed", .folds = 1:5, ...) {
  purrr::walk(.folds, function(f_) {
    clf_train(.path_data, .label_col = .label_col, .test_fold = f_, ...)
  })
  invisible(NULL)
}


# Overview: load + summarise ----------------------------------------------

#' Bind all per-fold overall-metrics rows from the runs tree
#' @param .runs_root Runs directory.
clf_load_overall <- function(.runs_root) {
  paths_ <- fs::dir_ls(.runs_root, recurse = TRUE, glob = "*metrics_overall.parquet")
  paths_ <- paths_[!grepl("_smoke", paths_)]
  if (length(paths_) == 0L) cli::cli_abort("No metrics_overall.parquet under {(.runs_root)}")
  purrr::map(paths_, arrow::read_parquet) |> purrr::list_rbind()
}

#' Leaderboard: mean / sd across folds, one row per configuration (numeric)
#' @param .tab_overall Output of clf_load_overall().
clf_leaderboard <- function(.tab_overall) {
  .tab_overall |>
    dplyr::filter(!.data$Smoke) |>
    dplyr::summarise(
      nFolds = dplyr::n(),
      Acc_mean = mean(.data$Accuracy), Acc_sd = sd(.data$Accuracy),
      F1macro_mean = mean(.data$F1_macro), F1macro_sd = sd(.data$F1_macro),
      F1weight_mean = mean(.data$F1_weighted), F1weight_sd = sd(.data$F1_weighted),
      .by = c(ConfigName, Model, LabelCol, TextCol, ClassWeights, MaxLen, Epochs, BatchSize, LR, Seed)
    ) |>
    dplyr::arrange(dplyr::desc(.data$F1macro_mean))
}

#' Leaderboard, formatted for reading (top .n configs, mean +/- sd as strings)
#' @param .tab_overall Output of clf_load_overall().
#' @param .label_col Optional character. Restrict to one task (e.g. "ClassDetailed").
#' @param .n Integer. Rows to show.
clf_leaderboard_show <- function(.tab_overall, .label_col = NULL, .n = 20L) {
  tab_ <- if (is.null(.label_col)) .tab_overall else dplyr::filter(.tab_overall, .data$LabelCol == .label_col)
  clf_leaderboard(tab_) |>
    dplyr::mutate(
      Rank        = dplyr::row_number(),
      Accuracy    = sprintf("%.3f +/- %.3f", .data$Acc_mean, .data$Acc_sd),
      F1_macro    = sprintf("%.3f +/- %.3f", .data$F1macro_mean, .data$F1macro_sd),
      F1_weighted = sprintf("%.3f +/- %.3f", .data$F1weight_mean, .data$F1weight_sd)
    ) |>
    dplyr::select(
      Rank, Model, LabelCol, MaxLen, Epochs, LR, ClassWeights,
      nFolds, Accuracy, F1_macro, F1_weighted
    ) |>
    head(.n)
}

#' Marginal effect of one sweep axis on macro-F1 (all else averaged over)
#'
#' Optionally restrict to one task first via .label_col (recommended, since
#' tasks have different difficulty). The grid is balanced, so this is a fair
#' marginal mean rather than a controlled contrast.
#'
#' @param .tab_overall Output of clf_load_overall().
#' @param .axis Character column to group by ("Model","MaxLen","Epochs",
#'   "ClassWeights","LR","LabelCol").
#' @param .label_col Optional character. Restrict to one task first.
clf_effect <- function(.tab_overall, .axis, .label_col = NULL) {
  tab_ <- if (is.null(.label_col)) .tab_overall else dplyr::filter(.tab_overall, .data$LabelCol == .label_col)
  tab_ |>
    dplyr::filter(!.data$Smoke) |>
    dplyr::summarise(
      nRuns = dplyr::n(),
      F1macro_mean = mean(.data$F1_macro),
      F1macro_sd = sd(.data$F1_macro),
      Acc_mean = mean(.data$Accuracy),
      .by = dplyr::all_of(.axis)
    ) |>
    dplyr::arrange(dplyr::desc(.data$F1macro_mean))
}


# Overview: pooled out-of-fold predictions --------------------------------

#' Pool out-of-fold predictions for one configuration across all folds
#' @param .runs_root Runs directory.
#' @param .config_name ConfigName string (from the leaderboard).
clf_pool_predictions <- function(.runs_root, .config_name) {
  paths_ <- fs::dir_ls(.runs_root, recurse = TRUE, glob = "*predictions.parquet")
  paths_ <- paths_[!grepl("_smoke", paths_)]
  purrr::map(paths_, arrow::read_parquet) |>
    purrr::list_rbind() |>
    dplyr::filter(.data$ConfigName == .config_name)
}

#' Attach the dual-class second label to pooled predictions (for lenient scoring)
#' @param .tab_pred Pooled predictions (must contain DocID).
#' @param .tab_prep Prepared sample (must contain DocID, ClassDetailed2).
clf_add_second_label <- function(.tab_pred, .tab_prep) {
  .tab_pred |>
    dplyr::inner_join(
      .tab_prep |> dplyr::select(DocID, ClassDetailed2),
      by = dplyr::join_by(DocID)
    )
}

#' Attach LabelRound to pooled predictions (for a round-sliced robustness check)
#' @param .tab_pred Pooled predictions (must contain DocID).
#' @param .tab_prep Prepared sample (must contain DocID, LabelRound).
clf_add_labelround <- function(.tab_pred, .tab_prep) {
  .tab_pred |>
    dplyr::inner_join(
      .tab_prep |> dplyr::select(DocID, LabelRound),
      by = dplyr::join_by(DocID)
    )
}

#' Internal: per-row correctness vector (strict or lenient)
#'
#' Strict: prediction matches the primary label. Lenient: prediction matches
#' EITHER the primary (TrueLabel) or the dual-class second label (ClassDetailed2,
#' when present). Lenient requires ClassDetailed2 in the table.
#' @keywords internal
clf_correct_vec <- function(.tab_pred, .lenient) {
  if (.lenient && "ClassDetailed2" %in% names(.tab_pred)) {
    (.tab_pred$PredLabel == .tab_pred$TrueLabel) |
      (!is.na(.tab_pred$ClassDetailed2) & .tab_pred$PredLabel == .tab_pred$ClassDetailed2)
  } else {
    .tab_pred$PredLabel == .tab_pred$TrueLabel
  }
}

#' Per-class precision / recall / F1 / support from pooled predictions
#'
#' Lenient scoring (for dual-class docs): a prediction is accepted if it matches
#' the primary OR the second label. Precision(c) is over docs predicted c;
#' recall(c) is over docs whose PRIMARY label is c (each doc in exactly one
#' recall bucket), with a dual-class doc credited as recalled for its primary if
#' the model predicts either valid label. Strict scoring ignores the second
#' label and reduces to the standard definition.
#'
#' @param .tab_pred Pooled predictions (DocID, TrueLabel, PredLabel, [ClassDetailed2]).
#' @param .lenient Logical. Lenient (either-label) scoring.
clf_perclass <- function(.tab_pred, .lenient = FALSE) {
  if (.lenient && !"ClassDetailed2" %in% names(.tab_pred)) {
    cli::cli_alert_warning("Lenient requested but no ClassDetailed2 column; using strict. Join via clf_add_second_label().")
    .lenient <- FALSE
  }
  correct_ <- clf_correct_vec(.tab_pred, .lenient)
  pred_ <- .tab_pred$PredLabel
  true_ <- .tab_pred$TrueLabel
  classes_ <- sort(unique(c(true_, pred_)))
  purrr::map(classes_, function(c_) {
    pp_ <- sum(pred_ == c_)
    ap_ <- sum(true_ == c_)
    tp_prec_ <- sum(pred_ == c_ & correct_)
    tp_rec_ <- sum(true_ == c_ & correct_)
    prec_ <- if (pp_ == 0L) NA_real_ else tp_prec_ / pp_
    rec_ <- if (ap_ == 0L) NA_real_ else tp_rec_ / ap_
    f1_ <- if (is.na(prec_) || is.na(rec_) || (prec_ + rec_) == 0) 0 else 2 * prec_ * rec_ / (prec_ + rec_)
    tibble::tibble(Label = c_, Precision = prec_, Recall = rec_, F1 = f1_, Support = ap_)
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(dplyr::desc(.data$Support))
}

#' Headline scores (accuracy, macro-F1, weighted-F1) from pooled predictions
#' @param .tab_pred Pooled predictions.
#' @param .lenient Logical. Lenient (either-label) scoring; needs ClassDetailed2.
clf_scores <- function(.tab_pred, .lenient = FALSE) {
  if (.lenient && !"ClassDetailed2" %in% names(.tab_pred)) {
    cli::cli_alert_warning("Lenient requested but no ClassDetailed2 column; using strict. Join via clf_add_second_label().")
    .lenient <- FALSE
  }
  pc_ <- clf_perclass(.tab_pred, .lenient = .lenient)
  correct_ <- clf_correct_vec(.tab_pred, .lenient)
  tibble::tibble(
    Scoring     = if (.lenient) "lenient" else "strict",
    Accuracy    = mean(correct_),
    F1_macro    = mean(pc_$F1),
    F1_weighted = sum(pc_$F1 * pc_$Support) / sum(pc_$Support),
    N           = nrow(.tab_pred)
  )
}

#' Confusion matrix (rows = true, cols = predicted) from pooled predictions
#' @param .tab_pred Pooled predictions.
clf_confusion <- function(.tab_pred) {
  .tab_pred |>
    dplyr::count(.data$TrueLabel, .data$PredLabel) |>
    tidyr::pivot_wider(names_from = "PredLabel", values_from = "n", values_fill = 0L) |>
    dplyr::arrange(.data$TrueLabel)
}

#' Roll detailed-level predictions up to the broad taxonomy
#'
#' Maps both true and predicted ClassDetailed labels to their ClassBroad parent
#' (mapping derived from the prepared sample) so the result can be scored at the
#' broad level and compared against a directly-trained ClassBroad model.
#'
#' @param .tab_pred Pooled ClassDetailed predictions (DocID, TrueLabel, PredLabel).
#' @param .tab_prep Prepared sample (must contain ClassDetailed, ClassBroad).
#' @return Predictions tibble with TrueLabel / PredLabel at the broad level.
clf_rollup_to_broad <- function(.tab_pred, .tab_prep) {
  map_ <- .tab_prep |> dplyr::distinct(ClassDetailed, ClassBroad)
  .tab_pred |>
    dplyr::left_join(
      map_ |> dplyr::rename(TrueLabel = ClassDetailed, TrueBroad = ClassBroad),
      by = dplyr::join_by(TrueLabel)
    ) |>
    dplyr::left_join(
      map_ |> dplyr::rename(PredLabel = ClassDetailed, PredBroad = ClassBroad),
      by = dplyr::join_by(PredLabel)
    ) |>
    dplyr::transmute(DocID, TrueLabel = TrueBroad, PredLabel = PredBroad)
}
