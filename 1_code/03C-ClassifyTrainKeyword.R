# 03C-ClassifyTrainKeyword: keyword sweep (parallel) + mined lexicon ----
# The keyword classifier: a transparent, lexical counterpart to BERT (03B) on the
# IDENTICAL frozen folds. R builds per-(config x fold) commands; Python
# (contracts-engine/keyword_train.py) mines a per-class lexicon on the TRAIN folds
# and scores the held-out fold, writing the SAME run-folder schema as the BERT
# engine (predictions / metrics_overall / metrics_perclass / config.json) plus
# lexicon.parquet. R scores keyword head-to-head with BERT through the shared
# 03A layer, on byte-for-byte the same splits.
#
# Sources 03A (source it alongside this file) for everything common:
#   clf_prepare_sample / clf_write_prepared -- the single prepared.parquet (which
#       already carries DocDesc, so there is NO keyword-specific prep step here)
#   clf_perclass / clf_scores / clf_confusion -- the unified abstention-aware
#       scoring layer (.none = "(none)" by default); keyword was the reason it is
#       abstention-aware, so the old kw_perclass / kw_scores / kw_confusion are gone
#   clf_load_overall / clf_pool_predictions / clf_effect / clf_add_second_label
#
# Keyword-specific (this file): diagnostics, the trainer command + runners, the
# axis-broken-out leaderboard, and the lexicon inspector. The method adds:
#   Source   = which field(s): "text" (body), "docdesc" (filer title),
#              "combined" (alpha * docdesc + text).
#   Coverage = share of docs the method predicts (1 - abstention rate).
#   "(none)" = sentinel PredLabel for an abstention (no term fired).
#
# Parallelism: the sweep dispatches independent, CPU-bound shell-outs across mirai
# daemons (scoped to the sweep, torn down on exit). Logging: each run tees its
# narrative to run_dir/run.log; the console stays quiet and shows only formatted
# performance + run stats; the master structured log is clf_load_overall() binding
# the per-run metrics_overall rows -- no shared file the parallel workers race on.
#
# House style: native pipe; explicit package::function; dot-prefixed args;
# underscore-suffixed locals; .data$ for existing columns, bare CamelCase for new;
# if (FALSE) dev blocks; cli/fs/here; pure ASCII; {(.arg)} parens in cli.

if (FALSE) {
  .path_data <- .lP$Input$Prepared
  .runs_root <- .lP$Output$RunsDir
  .grid      <- tidyr::expand_grid(label_col = "ClassDetailed", source = c("text", "docdesc", "combined"),
                                   stopwords = "none", fold = 1:5)
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
    .tab <- arrow::read_parquet(.lP$Input$Prepared)
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
#' If DocDesc (or DocName) appears verbatim near the top of Text, then mining
#' Text partly relearns DocDesc and the docdesc-vs-text contrast is muddied. High
#' leakage argues for stripping a header zone before mining the body.
#'
#' stri_sub (not base substr) is used for the head slice: base substr indexes
#' bytes, stri_sub indexes code points, so multibyte titles slice correctly.
#'
#' @param .tab Prepared sample (DocID, Text, DocDesc; DocName optional).
#' @param .n_chars Head window of Text to test, in characters.
#' @return Tibble: leak share for DocDesc and (if present) DocName.
kw_diag_header_leak <- function(.tab, .n_chars = 300L) {
  if (FALSE) {
    .tab     <- arrow::read_parquet(.lP$Input$Prepared)
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
    Field     = "DocDesc",
    NChars    = .n_chars,
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


# Training command + single run -------------------------------------------

#' Build the keyword-trainer command for one (config x fold)
#'
#' Pure: validates args and returns the python + argument vector, with NO side
#' effects. Both kw_train (sequential) and kw_sweep (parallel) call this so they
#' construct identical commands; the parallel path needs a side-effect-free builder
#' it can run inside daemons.
#'
#' The .stopwords default is "none": legal boilerplate function-word n-grams
#' ("the consultant shall") are discriminative verbal fingerprints, not noise, so
#' removing them does not help (and "none" won the sweep over "english_domain").
#'
#' @param .path_data Prepared parquet path (carries DocDesc).
#' @param .label_col "ClassDetailed", "ClassBroad", or "AmendType".
#' @param .source "text", "docdesc", or "combined".
#' @param .test_fold Integer. Fold held out for testing.
#' @param .text_col,.desc_col Body and title column names.
#' @param .positive_class Character or NULL. Binary asymmetric mode (mine only this
#'   class, predict it if its lexicon fires else the other label). NULL gives
#'   multi-class argmax with abstain-on-no-match.
#' @param .alpha Title weight for .source = "combined".
#' @param .topk,.ngram_max,.min_df,.max_df Mining hyperparameters.
#' @param .stopwords "none" (default), "english", "domain", or "english_domain".
#' @param .min_token_len Shortest alpha token kept.
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
                       .stopwords = c("none", "english_domain", "english", "domain"),
                       .min_token_len = 3L,
                       .seed = 42L,
                       .runs_root = here::here("2_output", "03C-ClassifyTrainKeyword", "runs"),
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
#' skip-if-exists is handled engine-side. The Python narrative is shown on the
#' console (one run, so no spam) and is also captured to run_dir/run.log.
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

#' Run k-fold CV for one keyword configuration (sequential; manual checks)
#'
#' For sweeping a grid, prefer kw_sweep (parallel). This loops kw_train over folds.
#'
#' @param .path_data Prepared parquet path.
#' @param .label_col "ClassDetailed", "ClassBroad", or "AmendType".
#' @param .source "text", "docdesc", or "combined".
#' @param .folds Integer vector of fold ids to hold out.
#' @param ... Passed through to kw_train (.stopwords, .topk, .runs_root, ...).
kw_cv <- function(.path_data, .label_col = "ClassDetailed", .source = "text",
                  .folds = 1:5, ...) {
  purrr::walk(.folds, function(f_) {
    kw_train(.path_data, .label_col = .label_col, .source = .source,
             .test_fold = f_, ...)
  })
  invisible(NULL)
}


# Parallel sweep (mirai) --------------------------------------------------

#' Run a grid of keyword (config x fold) cells in parallel via mirai
#'
#' The cells are independent and CPU-bound (each shells out to its own Python
#' process), so they parallelise trivially. mirai daemons are spun up for the
#' sweep and torn down on exit, so workers are not left pinned and will not
#' contend with a GPU/MPS-bound BERT run. Each daemon only runs system2 on a
#' command pre-built in the main process (absolute paths throughout), so no
#' per-daemon state or sourcing is needed. A failed cell is captured (not fatal);
#' the summary reports it and re-running is cheap (the engine skips finished cells).
#'
#' For many sweeps back-to-back in one session, hoist mirai::daemons() to the
#' caller and delete the on.exit teardown so daemons are reused.
#'
#' The grid must have columns label_col, source, fold; optional columns
#' positive_class, stopwords, min_token_len, topk, ngram_max, alpha override the
#' defaults per row (missing columns fall back to the kw_command defaults).
#'
#' @param .grid Tibble of cells (see above).
#' @param .path_data Prepared parquet path.
#' @param .runs_root Runs directory (default the 03C runs root).
#' @param .python,.script Paths to the venv python and the keyword trainer.
#' @param .workers Concurrent daemons. Default leaves 2 cores free.
#' @param .overwrite Re-run cells already on disk.
#' @return Invisible tibble of performance for the freshly-run cells (by macro-F1).
kw_sweep <- function(.grid, .path_data,
                     .runs_root = here::here("2_output", "03C-ClassifyTrainKeyword", "runs"),
                     .python = here::here("contracts-engine", ".venv", "bin", "python"),
                     .script = here::here("contracts-engine", "keyword_train.py"),
                     .workers = max(1L, parallel::detectCores() - 2L),
                     .overwrite = FALSE) {
  if (FALSE) {
    .grid      <- tidyr::expand_grid(label_col = "ClassDetailed", source = "text",
                                     stopwords = "none", fold = 1:5)
    .path_data <- .lP$Input$Prepared
    .runs_root <- .lP$Output$RunsDir
    .workers   <- 8L
    .overwrite <- FALSE
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
      .stopwords      = if (has_("stopwords"))      .grid$stopwords[[i_]]      else "none",
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

  cli::cli_alert_info("Dispatching {length(cmds_)} keyword cells across {(.workers)} mirai daemons ...")
  t0_ <- Sys.time()

  mirai::daemons(.workers)
  on.exit(mirai::daemons(0L), add = TRUE)

  # mirai_map dispatches one task per command; system2 is base (available on the
  # daemon), cmd is the mapped element. ms_[.progress] collects with a progress
  # bar (use ms_[] if an older mirai lacks the .progress signal).
  ms_  <- mirai::mirai_map(cmds_, \(cmd) system2(cmd$python, cmd$args, stdout = FALSE, stderr = FALSE))
  res_ <- ms_[.progress]

  mins_ <- round(as.numeric(difftime(Sys.time(), t0_, units = "mins")), 1)
  ok_   <- purrr::map_lgl(res_, function(s_) {
    !inherits(s_, "miraiError") && identical(as.integer(s_), 0L)
  })
  if (any(!ok_)) {
    cli::cli_alert_warning("{sum(!ok_)} of {length(cmds_)} cells failed -- see each run.log; re-run is cheap (engine skips finished cells)")
  }
  cli::cli_alert_success("Sweep done: {sum(ok_)}/{length(cmds_)} OK in {mins_} min")

  # Tidy performance for the freshly written cells. The mtime gate drops cells
  # that were skipped (their metrics predate this sweep); those results are still
  # on disk and reachable via clf_load_overall().
  paths_ <- fs::dir_ls(.runs_root, recurse = TRUE, glob = "*metrics_overall.parquet")
  paths_ <- paths_[!grepl("_smoke", paths_)]
  fresh_ <- paths_[file.mtime(paths_) >= t0_]
  perf_  <- if (length(fresh_) > 0L) {
    purrr::map(fresh_, arrow::read_parquet) |>
      purrr::list_rbind() |>
      dplyr::select(RunName, Source, Stopwords, TopK, Accuracy, F1_macro, Coverage, DurationSec) |>
      dplyr::arrange(dplyr::desc(.data$F1_macro))
  } else {
    tibble::tibble()
  }
  if (nrow(perf_) > 0L) {
    cli::cli_h3("Fresh cells (by macro-F1)")
    print(perf_, n = nrow(perf_))
  }
  invisible(perf_)
}


# Overview: keyword leaderboard (surfaces Source + Coverage) --------------

#' Leaderboard over keyword configs: mean / sd across folds (numeric)
#'
#' Filters clf_load_overall() output to keyword rows (those carrying a Source), so
#' BERT runs in a combined load are excluded. One row per configuration, with the
#' keyword axes broken out as columns (Source, Stopwords, TopK, ...) -- the
#' analyst's view, complementary to clf_leaderboard (ConfigName-keyed, cross-method)
#' and clf_effect (marginal effect of one axis).
#'
#' @param .tab_overall Output of clf_load_overall().
#' @return One row per keyword configuration, descending by mean macro-F1.
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
#' @return Formatted tibble.
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
#' @return Tibble: Zone, Class, Term, Folds, WeightMean, Measure, HitsPosMean.
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
      Folds       = dplyr::n_distinct(.data$Run),
      WeightMean  = mean(.data$Weight),
      Measure     = paste(sort(unique(.data$Measure)), collapse = "/"),
      HitsPosMean = mean(.data$HitsPosTrain),
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

# Selective classification: high-precision subset ------------------------
# The per-class numbers showed high precision but low recall on the hard classes:
# the lexicon is usually right when it commits, and pays mostly for being forced to
# guess (argmax) on ambiguous documents. Gating on the confidence Score turns that
# into a deliberate choice -- classify only what the lexicon is sure about, defer
# the rest. These read pooled predictions only (no re-mining, no engine change) and
# reuse 03A's abstention-aware scoring. This is the keyword arm of 03D routing.

#' Apply a confidence gate to pooled predictions (selective classification)
#'
#' Relabels low-confidence predictions to the abstention sentinel, so the shared
#' scoring layer treats them as deferred. .threshold is either a single cutoff for
#' every prediction, or a NAMED vector of per-class cutoffs (names = predicted
#' labels, as kw_calibrate returns). A prediction is kept when its Score is at least
#' the cutoff for its predicted class; otherwise it abstains. A class whose cutoff
#' is NA (target precision unreachable) abstains entirely.
#'
#' @param .tab_pred Pooled predictions (DocID, TrueLabel, PredLabel, Score).
#' @param .threshold Numeric scalar, or named numeric vector keyed by predicted label.
#' @param .none Character. Abstention sentinel (default "(none)").
#' @return .tab_pred with low-confidence PredLabel set to .none (other columns intact).
kw_gate <- function(.tab_pred, .threshold, .none = "(none)") {
  if (FALSE) {
    .tab_pred  <- pred_kw
    .threshold <- 0.5
    .none      <- "(none)"
  }
  has_names_ <- !is.null(names(.threshold))
  cut_ <- if (has_names_) {
    thr_ <- .threshold[.tab_pred$PredLabel]    # per-class lookup by predicted label
    thr_[is.na(thr_)] <- Inf                   # unreachable class -> always abstain
    unname(thr_)
  } else {
    rep(.threshold[1], nrow(.tab_pred))
  }
  keep_ <- .tab_pred$PredLabel != .none & .tab_pred$Score >= cut_
  .tab_pred |>
    dplyr::mutate(PredLabel = dplyr::if_else(keep_, .data$PredLabel, .none))
}

#' Precision-coverage curve for a keyword config (global confidence gate)
#'
#' Sweeps a single Score cutoff over .grid; at each cutoff the gated predictions are
#' scored on the COVERED subset. SelAccuracy is micro-accuracy among predicted docs
#' (the selective classifier's precision) and rises as the cutoff tightens; Coverage
#' is the predicted share and falls; MacroF1 is the abstention-aware mean per-class
#' F1 (eventually falls as recall is sacrificed). The accuracy-rejection tradeoff in
#' one table.
#'
#' @param .tab_pred Pooled predictions (DocID, TrueLabel, PredLabel, Score).
#' @param .grid Numeric vector of Score cutoffs to sweep.
#' @param .none Character. Abstention sentinel (default "(none)").
#' @return Tibble: Threshold, NPred, Coverage, SelAccuracy, MacroF1 (one row per cutoff).
kw_precision_coverage <- function(.tab_pred,
                                  .grid = seq(0, 0.9, by = 0.05),
                                  .none = "(none)") {
  if (FALSE) {
    .tab_pred <- pred_kw
    .grid     <- seq(0, 0.9, by = 0.05)
    .none     <- "(none)"
  }
  n_ <- nrow(.tab_pred)
  purrr::map(.grid, function(t_) {
    g_       <- kw_gate(.tab_pred, t_, .none = .none)
    covered_ <- g_$PredLabel != .none
    n_pred_  <- sum(covered_)
    sel_acc_ <- if (n_pred_ == 0L) NA_real_ else
      mean(g_$PredLabel[covered_] == g_$TrueLabel[covered_])
    macro_   <- if (n_pred_ == 0L) NA_real_ else mean(clf_perclass(g_, .none = .none)$F1)
    tibble::tibble(
      Threshold   = t_,
      NPred       = n_pred_,
      Coverage    = n_pred_ / n_,
      SelAccuracy = sel_acc_,
      MacroF1     = macro_
    )
  }) |>
    purrr::list_rbind()
}

#' Per-class confidence thresholds for a target precision
#'
#' For each predicted class, finds the LOWEST Score cutoff (the most permissive,
#' keeping the most documents) at which precision among the kept predictions reaches
#' .target_precision, requiring at least .min_keep kept. This is the per-class
#' operating point for a high-precision selective classifier; the returned Threshold
#' column feeds kw_gate via a named vector. Precision is strict (predicted == primary
#' true label). Classes that cannot reach the target get Threshold NA (kw_gate then
#' abstains on them entirely). Realised Precision / Recall are reported at the chosen
#' cutoff, so ties at the boundary show honestly rather than being assumed exact.
#'
#' @param .tab_pred Pooled predictions (DocID, TrueLabel, PredLabel, Score).
#' @param .target_precision Numeric in (0, 1]. Precision to clear per class (default 0.95).
#' @param .min_keep Integer. Minimum kept predictions for a class to qualify (default 5).
#' @param .none Character. Abstention sentinel to exclude (default "(none)").
#' @return Tibble per predicted class: Label, Threshold, Precision, Recall, NKeep,
#'   NPredBase, KeepShare, Support (descending by Support).
kw_calibrate <- function(.tab_pred, .target_precision = 0.95,
                         .min_keep = 5L, .none = "(none)") {
  if (FALSE) {
    .tab_pred         <- pred_kw
    .target_precision <- 0.95
    .min_keep         <- 5L
    .none             <- "(none)"
  }
  classes_ <- setdiff(sort(unique(.tab_pred$PredLabel)), .none)
  supp_    <- .tab_pred |> dplyr::count(.data$TrueLabel, name = "Support")

  purrr::map(classes_, function(c_) {
    pc_ <- .tab_pred |>
      dplyr::filter(.data$PredLabel == c_) |>
      dplyr::mutate(Correct = .data$TrueLabel == c_) |>
      dplyr::arrange(dplyr::desc(.data$Score))
    n_base_  <- nrow(pc_)
    support_ <- supp_$Support[supp_$TrueLabel == c_]
    support_ <- if (length(support_) == 0L) 0L else support_

    # cumulative precision reading from the highest-score prediction downward;
    # the deepest cut (max index) still on target maximises coverage
    cum_prec_ <- cumsum(pc_$Correct) / seq_len(n_base_)
    ok_       <- which(cum_prec_ >= .target_precision & seq_len(n_base_) >= .min_keep)
    k_        <- if (length(ok_) == 0L) NA_integer_ else max(ok_)

    if (is.na(k_)) {
      tibble::tibble(Label = c_, Threshold = NA_real_, Precision = NA_real_,
                     Recall = 0, NKeep = 0L, NPredBase = n_base_,
                     KeepShare = 0, Support = support_)
    } else {
      thr_    <- pc_$Score[k_]
      kept_   <- pc_$Score >= thr_                 # realise the cut (ties included)
      n_keep_ <- sum(kept_)
      tibble::tibble(
        Label     = c_,
        Threshold = thr_,
        Precision = mean(pc_$Correct[kept_]),
        Recall    = if (support_ == 0L) NA_real_ else sum(pc_$Correct[kept_]) / support_,
        NKeep     = n_keep_,
        NPredBase = n_base_,
        KeepShare = n_keep_ / n_base_,
        Support   = support_
      )
    }
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(dplyr::desc(.data$Support))
}

#' One-line summary of a gated (selective) prediction set
#'
#' What a high-precision layer actually delivers: how much of the corpus it
#' classifies (Coverage) and how accurate it is on that covered subset
#' (SelAccuracy). Unlike clf_scores -- which counts an abstention as incorrect --
#' these describe the classified slice on its own terms.
#'
#' @param .tab_pred Gated pooled predictions (from kw_gate).
#' @param .none Character. Abstention sentinel (default "(none)").
#' @return One-row tibble: Coverage, SelAccuracy, NClassified, NTotal.
kw_selective_summary <- function(.tab_pred, .none = "(none)") {
  if (FALSE) {
    .tab_pred <- kw_gate(pred_kw, 0.5)
    .none     <- "(none)"
  }
  covered_ <- .tab_pred$PredLabel != .none
  tibble::tibble(
    Coverage    = mean(covered_),
    SelAccuracy = if (any(covered_)) mean(.tab_pred$PredLabel[covered_] == .tab_pred$TrueLabel[covered_]) else NA_real_,
    NClassified = sum(covered_),
    NTotal      = length(covered_)
  )
}

#' Plot the precision-coverage (accuracy-rejection) curve
#'
#' Selective accuracy against coverage, one point per swept cutoff. Reading
#' right-to-left shows the precision bought by abstaining on more documents. House
#' theme via clf_apply_theme.
#'
#' @param .curve Output of kw_precision_coverage.
#' @return A ggplot.
kw_plot_precision_coverage <- function(.curve) {
  if (FALSE) {
    .curve <- kw_precision_coverage(pred_kw)
  }
  p_ <- .curve |>
    dplyr::filter(!is.na(.data$SelAccuracy)) |>
    ggplot2::ggplot(ggplot2::aes(x = Coverage, y = SelAccuracy)) +
    ggplot2::geom_line(linewidth = 0.4, color = "grey30") +
    ggplot2::geom_point(size = 1.6, color = "grey20") +
    ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = "Coverage (share of documents classified)",
                  y = "Selective accuracy (precision on classified)")
  clf_apply_theme(p_)
}
