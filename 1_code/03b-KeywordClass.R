# 03b-KeywordClass: keyword diagnostics, training wrapper, and scoring ----
# Same engine-agnostic seam as the BERT track: R deals folds, reads text, and
# attaches DocDesc -> prepared parquet; Python (contracts-engine/keyword_train.py)
# mines a per-class lexicon on the TRAIN folds and scores the held-out fold,
# writing the SAME run-folder schema as classify_train.py; R reuses the clf_*
# overview layer to score keyword head-to-head with BERT on the IDENTICAL folds.
#
# Reused from 03-Classification.R (source it alongside this file):
#   clf_prepare_sample  -- deals the frozen folds (SAME seed/k -> identical folds)
#   clf_load_overall    -- binds metrics_overall across runs (BERT + keyword)
#   clf_pool_predictions-- pools out-of-fold predictions for one config
#   clf_leaderboard*, clf_effect, clf_fold_overview, clf_add_second_label
# Keyword-specific (this file): kw_* below. Scoring is abstention-aware because a
# keyword prediction can be "(none)" (no term fired); BERT always predicts.
#
# Terminology matches the BERT track: ClassBroad / ClassDetailed / ClassDetailed2
# / AmendType / LabelRound. The keyword method adds:
#   Source         = which field(s): "docdesc" (filer title), "text" (body),
#                    "combined" (alpha * docdesc + text).
#   Coverage       = share of docs the method predicts (1 - abstention rate).
#   "(none)"       = sentinel PredLabel for an abstention.

if (FALSE) {
  .tab_input   <- fils_class_sample
  .path_labels <- .lP$Input$ClassificationSample
  .path_data   <- .lP$Output$Prepared
  .runs_root   <- .lP$Output$RunsDir
}


# Step 0 diagnostics ------------------------------------------------------

#' DocDesc coverage (run before modelling)
#'
#' How much of the sample carries a non-empty filer title. If below 100%, a
#' docdesc-only model must abstain on the gaps, which Coverage will then report.
#'
#' @param .tab Prepared sample (must contain DocDesc, ClassDetailed).
#' @return Tibble: overall and per-ClassDetailed non-empty-DocDesc share.
kw_diag_desc_coverage <- function(.tab) {
  if (FALSE) {
    .tab <- arrow::read_parquet(.lP$Output$Prepared)
  }
  has_desc_ <- function(.x) !is.na(.x) & trimws(.x) != ""

  overall_ <- .tab |>
    dplyr::summarise(
      Level    = "ALL",
      N        = dplyr::n(),
      HasDesc  = sum(has_desc_(.data$DocDesc)),
      Coverage = mean(has_desc_(.data$DocDesc))
    )

  by_class_ <- .tab |>
    dplyr::summarise(
      N        = dplyr::n(),
      HasDesc  = sum(has_desc_(.data$DocDesc)),
      Coverage = mean(has_desc_(.data$DocDesc)),
      .by      = ClassDetailed
    ) |>
    dplyr::rename(Level = ClassDetailed) |>
    dplyr::arrange(.data$Coverage)

  cli::cli_alert_info("DocDesc non-empty: {scales::percent(overall_$Coverage, 0.1)} of {overall_$N} docs")
  dplyr::bind_rows(overall_, by_class_)
}

#' Header-leakage probe: does the body restate the title verbatim?
#'
#' If DocDesc (or DocName) appears verbatim near the top of TextRaw, then mining
#' Text partly relearns DocDesc and the docdesc-vs-text contrast is muddied. High
#' leakage argues for stripping a header zone (the analog of Storm stripping the
#' Kadaster "De bewaarder." registration prefix) before mining the body.
#'
#' stri_sub (not base substr) is used for the head slice: base substr indexes
#' bytes, stri_sub indexes code points, so multibyte titles slice correctly.
#'
#' @param .tab Prepared sample (DocID, Text, DocDesc; DocName optional).
#' @param .n_chars Head window of Text to test, in characters.
#' @return Tibble: leak share for DocDesc and (if present) DocName.
kw_diag_header_leak <- function(.tab, .n_chars = 300L) {
  if (FALSE) {
    .tab     <- arrow::read_parquet(.lP$Output$Prepared)
    .n_chars <- 300L
  }
  head_ <- stringi::stri_sub(.tab$Text, 1L, .n_chars) |> stringi::stri_trans_tolower()

  leak_share_ <- function(.field) {
    field_ <- stringi::stri_trans_tolower(.field)
    ok_    <- !is.na(field_) & trimws(field_) != "" & !is.na(head_)
    if (!any(ok_)) return(NA_real_)
    mean(stringi::stri_detect_fixed(head_[ok_], field_[ok_]))
  }

  out_ <- tibble::tibble(
    Field    = "DocDesc",
    NChars   = .n_chars,
    LeakShare = leak_share_(.tab$DocDesc)
  )
  if ("DocName" %in% names(.tab)) {
    out_ <- dplyr::bind_rows(out_, tibble::tibble(
      Field = "DocName", NChars = .n_chars, LeakShare = leak_share_(.tab$DocName)
    ))
  }
  cli::cli_alert_info("Head-{(.n_chars)}-char verbatim title match: DocDesc {scales::percent(out_$LeakShare[[1]], 0.1)}")
  out_
}


# Prepared sample (attach DocDesc to the frozen-fold spine) ---------------

#' Write the keyword prepared parquet: BERT folds + DocDesc / DocName
#'
#' Takes the output of clf_prepare_sample (which carries the frozen Fold and the
#' label columns but DROPS DocDesc) and re-attaches DocDesc / DocName from the
#' metadata-joined input, so the keyword engine can mine the title field. Because
#' the folds come from clf_prepare_sample with the same seed/k, they are byte-for-
#' byte the BERT folds -- the two tracks are scored on identical splits.
#'
#' @param .tab_prep Output of clf_prepare_sample (DocID, Text, ClassBroad,
#'   ClassDetailed, ClassDetailed2, AmendType, LabelRound, Fold).
#' @param .tab_meta Metadata-joined input (must contain DocID, DocDesc; DocName
#'   used if present).
#' @param .path_out Output parquet path.
#' @return Invisible path written.
kw_write_prepared <- function(.tab_prep, .tab_meta, .path_out) {
  if (FALSE) {
    .tab_prep <- clf_prepare_sample(fils_class_sample, .lP$Input$ClassificationSample)
    .tab_meta <- fils_class_sample
    .path_out <- .lP$Output$Prepared
  }
  meta_ <- .tab_meta |>
    dplyr::select(DocID, dplyr::any_of(c("DocDesc", "DocName"))) |>
    dplyr::distinct(DocID, .keep_all = TRUE)

  if (!"DocDesc" %in% names(meta_)) cli::cli_abort("Metadata input has no DocDesc column")

  out_ <- .tab_prep |>
    dplyr::left_join(meta_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("DocDesc", "DocName")),
                                ~ dplyr::coalesce(.x, "")))

  n_missing_ <- sum(out_$DocDesc == "")
  if (n_missing_ > 0L) cli::cli_alert_warning("{n_missing_} docs have empty DocDesc (docdesc model will abstain on these)")

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)
  cli::cli_alert_success("Wrote {(.path_out)} ({nrow(out_)} docs, with DocDesc)")
  invisible(.path_out)
}


# Training wrapper (shells out to the Python keyword engine) --------------

#' Build the keyword-trainer command for one (config x fold)
#'
#' Pure: validates args and returns the python + argument vector, with NO side
#' effects (no shelling out, no cli). Both kw_train (sequential) and kw_sweep
#' (parallel) call this so they construct identical commands; the parallel path
#' needs a side-effect-free builder it can run inside fork workers.
#'
#' @param .path_data Prepared parquet path (with DocDesc).
#' @param .label_col "ClassDetailed", "ClassBroad", or "AmendType".
#' @param .source "docdesc", "text", or "combined".
#' @param .test_fold Integer. Fold held out for testing.
#' @param .text_col,.desc_col Body and title column names.
#' @param .positive_class Character or NULL. Binary asymmetric mode (e.g.
#'   "Amended"): mine only this class, predict it if its lexicon fires else the
#'   other label. NULL gives multi-class argmax with abstain-on-no-match.
#' @param .alpha Title weight for .source = "combined".
#' @param .topk,.ngram_max,.min_df,.max_df Mining hyperparameters.
#' @param .stopwords "none", "english", "domain", or "english_domain" (English
#'   function words + SEC/exhibit boilerplate; the lessons-learned default).
#' @param .min_token_len Shortest alpha token kept (3 default; 4 drops more noise).
#' @param .seed Stamped for parity (mining is deterministic).
#' @param .runs_root Directory under which run folders are written.
#' @param .python,.script Paths to the venv python and the keyword trainer.
#' @param .save_probs,.overwrite,.smoke Flags.
#' @return List(python = <path>, args = <character vector>).
kw_command <- function(.path_data,
                       .label_col = c("ClassDetailed", "ClassBroad", "AmendType"),
                       .source = c("text", "docdesc", "combined"),
                       .test_fold = 1L,
                       .text_col = "Text",
                       .desc_col = "DocDesc",
                       .positive_class = NULL,
                       .alpha = 2.0,
                       .topk = 25L,
                       .ngram_max = 3L,
                       .min_df = 2L,
                       .max_df = 0.5,
                       .stopwords = c("english_domain", "none", "english", "domain"),
                       .min_token_len = 3L,
                       .seed = 42L,
                       .runs_root = here::here("2_output", "03b-KeywordClass", "runs"),
                       .python = here::here("contracts-engine", ".venv", "bin", "python"),
                       .script = here::here("contracts-engine", "keyword_train.py"),
                       .save_probs = TRUE,
                       .overwrite = FALSE,
                       .smoke = FALSE) {

  .label_col <- match.arg(.label_col)
  .source    <- match.arg(.source)
  .stopwords <- match.arg(.stopwords)

  args_ <- c(
    .script,
    "--data", .path_data,
    "--label-col", .label_col,
    "--source", .source,
    "--text-col", .text_col,
    "--desc-col", .desc_col,
    "--test-fold", as.character(.test_fold),
    "--alpha", as.character(.alpha),
    "--topk", as.character(.topk),
    "--ngram-max", as.character(.ngram_max),
    "--min-df", as.character(.min_df),
    "--max-df", as.character(.max_df),
    "--stopwords", .stopwords,
    "--min-token-len", as.character(.min_token_len),
    "--seed", as.character(.seed),
    "--runs-root", .runs_root
  )
  if (!is.null(.positive_class)) args_ <- c(args_, "--positive-class", .positive_class)
  if (!.save_probs)             args_ <- c(args_, "--no-save-probs")
  if (.overwrite)              args_ <- c(args_, "--overwrite")
  if (.smoke)                  args_ <- c(args_, "--smoke")

  list(python = .python, args = args_)
}

#' Mine + score one fold by invoking the contracts-engine keyword trainer
#'
#' Sequential single-run wrapper for manual checks (smoke, one real fold). For
#' sweeping a grid in parallel use kw_sweep. One invocation = one (config x fold);
#' skip-if-exists is handled engine-side. Runs default into the 03b runs root.
#'
#' @inheritParams kw_command
#' @return Invisible exit status.
kw_train <- function(.path_data, ...,
                     .python = here::here("contracts-engine", ".venv", "bin", "python"),
                     .script = here::here("contracts-engine", "keyword_train.py")) {
  if (!fs::file_exists(.python)) cli::cli_abort("Python not found at {(.python)}")
  if (!fs::file_exists(.script)) cli::cli_abort("Trainer not found at {(.script)}")

  cmd_ <- kw_command(.path_data, ..., .python = .python, .script = .script)
  status_ <- system2(cmd_$python, args = cmd_$args, stdout = "", stderr = "")
  if (!identical(status_, 0L)) cli::cli_abort("Python keyword trainer failed (exit {status_})")
  invisible(status_)
}

#' Run a grid of keyword (config x fold) runs in parallel
#'
#' The runs are independent and CPU-bound (each shells out to its own Python
#' process), so they parallelise trivially. parallel::mclapply forks one worker
#' per run, capped at .workers concurrent -- fork-based, no extra packages, and it
#' will not contend with BERT, which is GPU-bound. Each worker only calls
#' system2 on a command pre-built in the main process, so no per-worker state or
#' sourcing is needed. (mc.preschedule = FALSE load-balances dynamically, which
#' matters here: docdesc runs finish in well under a second while text/combined
#' runs dominate.)
#'
#' The grid must have columns label_col, source, fold; optional columns
#' positive_class, stopwords, min_token_len, topk, ngram_max, alpha override the
#' defaults per row (missing columns fall back to the kw_command defaults).
#'
#' @param .grid Tibble of runs (see above).
#' @param .path_data Prepared parquet path.
#' @param .runs_root Runs directory (default the 03b runs root).
#' @param .python,.script Paths to the venv python and the keyword trainer.
#' @param .workers Concurrent workers. Default leaves 2 cores free; lower it if
#'   text/combined runs spike memory (each builds an n-gram vocabulary in RAM).
#' @param .overwrite Re-run cells already on disk.
#' @return Invisible list of per-run exit statuses.
kw_sweep <- function(.grid, .path_data,
                     .runs_root = here::here("2_output", "03b-KeywordClass", "runs"),
                     .python = here::here("contracts-engine", ".venv", "bin", "python"),
                     .script = here::here("contracts-engine", "keyword_train.py"),
                     .workers = max(1L, parallel::detectCores() - 2L),
                     .overwrite = FALSE) {
  if (FALSE) {
    .grid      <- tidyr::expand_grid(source = "text", label_col = "ClassDetailed", fold = 1:5)
    .path_data <- .lP$Output$Prepared
    .runs_root <- .lP$Output$RunsDir
    .workers   <- 8L
  }
  if (!fs::file_exists(.python)) cli::cli_abort("Python not found at {(.python)}")
  if (!fs::file_exists(.script)) cli::cli_abort("Trainer not found at {(.script)}")

  need_ <- c("label_col", "source", "fold")
  miss_ <- setdiff(need_, names(.grid))
  if (length(miss_) > 0L) cli::cli_abort("Grid missing columns: {miss_}")

  has_ <- function(.col) .col %in% names(.grid)
  cmds_ <- purrr::map(seq_len(nrow(.grid)), function(i_) {
    kw_command(
      .path_data      = .path_data,
      .label_col      = .grid$label_col[[i_]],
      .source         = .grid$source[[i_]],
      .test_fold      = .grid$fold[[i_]],
      .positive_class = if (has_("positive_class")) .grid$positive_class[[i_]] else NULL,
      .stopwords      = if (has_("stopwords"))      .grid$stopwords[[i_]]      else "english_domain",
      .min_token_len  = if (has_("min_token_len"))  .grid$min_token_len[[i_]]  else 3L,
      .topk           = if (has_("topk"))           .grid$topk[[i_]]           else 25L,
      .ngram_max      = if (has_("ngram_max"))      .grid$ngram_max[[i_]]      else 3L,
      .alpha          = if (has_("alpha"))          .grid$alpha[[i_]]          else 2.0,
      .runs_root      = .runs_root,
      .python         = .python,
      .script         = .script,
      .overwrite      = .overwrite
    )
  })

  cli::cli_alert_info("Dispatching {length(cmds_)} keyword runs across {(.workers)} workers ...")
  t0_ <- Sys.time()
  res_ <- parallel::mclapply(
    cmds_,
    function(cmd_) system2(cmd_$python, args = cmd_$args, stdout = FALSE, stderr = FALSE),
    mc.cores = .workers, mc.preschedule = FALSE
  )
  ok_ <- purrr::map_lgl(res_, function(s_) identical(as.integer(s_), 0L))
  mins_ <- round(as.numeric(difftime(Sys.time(), t0_, units = "mins")), 1)
  if (any(!ok_)) cli::cli_alert_warning("{sum(!ok_)} of {length(cmds_)} runs returned non-zero -- inspect, then re-run (skip-if-exists makes re-runs cheap)")
  cli::cli_alert_success("Sweep done: {sum(ok_)}/{length(cmds_)} runs OK in {mins_} min")
  invisible(res_)
}

#' Run k-fold CV for one keyword configuration (sequential; manual checks)
#'
#' For sweeping a grid, prefer kw_sweep (parallel). This loops kw_train over folds.
#'
#' @param .path_data Prepared parquet path.
#' @param .label_col "ClassDetailed", "ClassBroad", or "AmendType".
#' @param .source "docdesc", "text", or "combined".
#' @param .folds Integer vector of fold ids to hold out.
#' @param ... Passed through to kw_train (.positive_class, .stopwords, .topk, ...).
kw_cv <- function(.path_data, .label_col = "ClassDetailed", .source = "text",
                  .folds = 1:5, ...) {
  purrr::walk(.folds, function(f_) {
    kw_train(.path_data, .label_col = .label_col, .source = .source,
             .test_fold = f_, ...)
  })
  invisible(NULL)
}


# Overview: abstention-aware scoring (siblings of clf_*) ------------------
# These mirror clf_perclass / clf_scores / clf_confusion but exclude the "(none)"
# abstention sentinel from the class set and the macro average. The precision /
# recall arithmetic is otherwise the SAME as clf_*: an abstained doc is a false
# negative for its true class (PredLabel != true) and a false positive for
# nothing (PredLabel == "(none)"), so abstaining costs recall and protects
# precision -- the selective-prediction semantics we want.

#' Per-class precision / recall / F1 / support from pooled keyword predictions
#'
#' @param .tab_pred Pooled predictions (DocID, TrueLabel, PredLabel; optional
#'   ClassDetailed2 for lenient dual-class scoring via clf_add_second_label).
#' @param .lenient Logical. Accept either the primary or the dual-class second label.
kw_perclass <- function(.tab_pred, .lenient = FALSE) {
  if (FALSE) {
    .tab_pred <- clf_pool_predictions(.lP$Output$RunsDir, best_kw_)
    .lenient  <- FALSE
  }
  if (.lenient && !"ClassDetailed2" %in% names(.tab_pred)) {
    cli::cli_alert_warning("Lenient requested but no ClassDetailed2 column; using strict. Join via clf_add_second_label().")
    .lenient <- FALSE
  }
  correct_ <- if (.lenient && "ClassDetailed2" %in% names(.tab_pred)) {
    (.tab_pred$PredLabel == .tab_pred$TrueLabel) |
      (!is.na(.tab_pred$ClassDetailed2) & .tab_pred$PredLabel == .tab_pred$ClassDetailed2)
  } else {
    .tab_pred$PredLabel == .tab_pred$TrueLabel
  }

  pred_ <- .tab_pred$PredLabel
  true_ <- .tab_pred$TrueLabel
  classes_ <- setdiff(sort(unique(c(true_, pred_))), "(none)")   # drop abstain sentinel
  purrr::map(classes_, function(c_) {
    pp_      <- sum(pred_ == c_)
    ap_      <- sum(true_ == c_)
    tp_prec_ <- sum(pred_ == c_ & correct_)
    tp_rec_  <- sum(true_ == c_ & correct_)
    prec_ <- if (pp_ == 0L) NA_real_ else tp_prec_ / pp_
    rec_  <- if (ap_ == 0L) NA_real_ else tp_rec_ / ap_
    f1_   <- if (is.na(prec_) || is.na(rec_) || (prec_ + rec_) == 0) 0 else 2 * prec_ * rec_ / (prec_ + rec_)
    tibble::tibble(Label = c_, Precision = prec_, Recall = rec_, F1 = f1_, Support = ap_)
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(dplyr::desc(.data$Support))
}

#' Headline scores (accuracy, macro-F1, weighted-F1, coverage) from pooled keyword predictions
#'
#' Coverage is the share of docs the method predicted (1 - abstention rate);
#' accuracy counts an abstention as incorrect.
#'
#' @param .tab_pred Pooled predictions.
#' @param .lenient Logical. Lenient (either-label) scoring; needs ClassDetailed2.
kw_scores <- function(.tab_pred, .lenient = FALSE) {
  if (.lenient && !"ClassDetailed2" %in% names(.tab_pred)) {
    cli::cli_alert_warning("Lenient requested but no ClassDetailed2 column; using strict. Join via clf_add_second_label().")
    .lenient <- FALSE
  }
  pc_ <- kw_perclass(.tab_pred, .lenient = .lenient)
  correct_ <- if (.lenient && "ClassDetailed2" %in% names(.tab_pred)) {
    (.tab_pred$PredLabel == .tab_pred$TrueLabel) |
      (!is.na(.tab_pred$ClassDetailed2) & .tab_pred$PredLabel == .tab_pred$ClassDetailed2)
  } else {
    .tab_pred$PredLabel == .tab_pred$TrueLabel
  }
  tibble::tibble(
    Scoring     = if (.lenient) "lenient" else "strict",
    Accuracy    = mean(correct_),
    F1_macro    = mean(pc_$F1),
    F1_weighted = sum(pc_$F1 * pc_$Support) / sum(pc_$Support),
    Coverage    = mean(.tab_pred$PredLabel != "(none)"),
    N           = nrow(.tab_pred)
  )
}

#' Confusion matrix (rows = true, cols = predicted) from pooled keyword predictions
#'
#' The "(none)" column, when present, shows which true classes the method
#' abstained on -- useful for seeing where the lexicon has no signal.
#'
#' @param .tab_pred Pooled predictions.
kw_confusion <- function(.tab_pred) {
  .tab_pred |>
    dplyr::count(.data$TrueLabel, .data$PredLabel) |>
    tidyr::pivot_wider(names_from = "PredLabel", values_from = "n", values_fill = 0L) |>
    dplyr::arrange(.data$TrueLabel)
}


# Overview: keyword leaderboard (surfaces Source + Coverage) --------------

#' Leaderboard over keyword configs: mean / sd across folds (numeric)
#'
#' Filters clf_load_overall() output to keyword rows (those carrying a Source),
#' so BERT runs in the same root are excluded. One row per configuration.
#'
#' @param .tab_overall Output of clf_load_overall().
kw_leaderboard <- function(.tab_overall) {
  .tab_overall |>
    dplyr::filter(!is.na(.data$Source), !.data$Smoke) |>
    dplyr::summarise(
      nFolds        = dplyr::n(),
      Acc_mean      = mean(.data$Accuracy),    Acc_sd      = sd(.data$Accuracy),
      F1macro_mean  = mean(.data$F1_macro),    F1macro_sd  = sd(.data$F1_macro),
      F1weight_mean = mean(.data$F1_weighted), F1weight_sd = sd(.data$F1_weighted),
      Cov_mean      = mean(.data$Coverage),
      .by = c(ConfigName, Model, LabelCol, Source, TopK, NgramMax, Alpha,
              Stopwords, MinTokenLen, PositiveClass, Seed)
    ) |>
    dplyr::arrange(dplyr::desc(.data$F1macro_mean))
}

#' Keyword leaderboard, formatted for reading (top .n configs)
#'
#' @param .tab_overall Output of clf_load_overall().
#' @param .label_col Optional character. Restrict to one task (e.g. "ClassDetailed").
#' @param .n Integer. Rows to show.
kw_leaderboard_show <- function(.tab_overall, .label_col = NULL, .n = 20L) {
  tab_ <- if (is.null(.label_col)) .tab_overall else dplyr::filter(.tab_overall, .data$LabelCol == .label_col)
  kw_leaderboard(tab_) |>
    dplyr::mutate(
      Rank        = dplyr::row_number(),
      Coverage    = sprintf("%.3f", .data$Cov_mean),
      Accuracy    = sprintf("%.3f +/- %.3f", .data$Acc_mean, .data$Acc_sd),
      F1_macro    = sprintf("%.3f +/- %.3f", .data$F1macro_mean, .data$F1macro_sd),
      F1_weighted = sprintf("%.3f +/- %.3f", .data$F1weight_mean, .data$F1weight_sd)
    ) |>
    dplyr::select(Rank, LabelCol, Source, Stopwords, MinTokenLen, TopK, NgramMax, Alpha,
                  nFolds, Coverage, Accuracy, F1_macro, F1_weighted) |>
    head(.n)
}


# Inspect a config's mined lexicon (the interpretable payoff) -------------

#' Pool and summarise the mined lexicon for one keyword configuration
#'
#' Binds lexicon.parquet across the config's folds and reports, per (Zone, Class,
#' Term), the mean training precision weight and the number of folds the term was
#' mined in (its stability). High-weight, all-fold terms are the trustworthy
#' signals -- and the natural seed list for an optional human-prune pass.
#'
#' @param .runs_root Runs directory.
#' @param .config_name ConfigName string (from kw_leaderboard).
#' @param .min_folds Integer. Keep only terms mined in at least this many folds.
#' @param .top_per_class Integer or NULL. Keep only the top terms per class by weight.
kw_load_lexicon <- function(.runs_root, .config_name, .min_folds = 1L, .top_per_class = NULL) {
  if (FALSE) {
    .runs_root     <- .lP$Output$RunsDir
    .config_name   <- best_kw_det_
    .min_folds     <- 3L
    .top_per_class <- 15L
  }
  paths_ <- fs::dir_ls(.runs_root, recurse = TRUE, glob = "*lexicon.parquet")
  paths_ <- paths_[!grepl("_smoke", paths_)]
  if (length(paths_) == 0L) cli::cli_abort("No lexicon.parquet under {(.runs_root)}")

  lex_ <- purrr::map(paths_, arrow::read_parquet) |>
    purrr::list_rbind() |>
    dplyr::filter(.data$ConfigName == .config_name)
  if (nrow(lex_) == 0L) cli::cli_abort("No lexicon rows for config {(.config_name)}")

  out_ <- lex_ |>
    dplyr::summarise(
      Folds        = dplyr::n_distinct(.data$Run),
      WeightMean   = mean(.data$Weight),
      Measure      = paste(sort(unique(.data$Measure)), collapse = "/"),
      HitsPosMean  = mean(.data$HitsPosTrain),
      .by = c(Zone, Class, Term)
    ) |>
    dplyr::filter(.data$Folds >= .min_folds) |>
    dplyr::arrange(.data$Zone, .data$Class, dplyr::desc(.data$WeightMean))

  if (!is.null(.top_per_class)) {
    out_ <- out_ |>
      dplyr::slice_max(.data$WeightMean, n = .top_per_class, by = c(Zone, Class), with_ties = FALSE)
  }
  out_
}
