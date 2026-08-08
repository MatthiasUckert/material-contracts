# 03A-ClassifyPrepare: dataset creation + shared "overall" tooling ----
# The common foundation for the whole classification pipeline. Everything in this
# file is METHOD-AGNOSTIC: it builds the one prepared sample (with frozen folds)
# that BERT (03B) and keyword (03C) both consume, and it provides the single
# scoring / leaderboard / plotting layer that every method (BERT, keyword, the
# 03D router, the 03E deployment) reports through. No training wrappers live here:
#   - bert_* (the BERT trainer wrapper) lives in 03B-ClassifyTrainBERT.R
#   - kw_*   (the keyword miner wrapper) lives in 03C-ClassifyTrainKeyword.R
# Those source THIS file for prep + scoring, so the splits never drift.
#
# Two consolidations vs the old 03-Classification.R / 03b-KeywordClass.R pair:
#   1. ONE prepared parquet. clf_prepare_sample now carries DocDesc / DocName, so
#      there is a single prepared.parquet for both tracks (BERT ignores the title
#      column; keyword mines it). The old kw_write_prepared re-attach is gone.
#   2. ONE scoring layer. clf_perclass / clf_scores are abstention-aware via a
#      .none sentinel argument: with no abstention (BERT) they reduce EXACTLY to
#      the old behaviour; with abstention (keyword "(none)") they drop the
#      sentinel from the class set and report Coverage. The old kw_perclass /
#      kw_scores duplicates are folded in here.
#
# Terminology (settled -- see the state document):
#   ClassBroad     = 7-class broad taxonomy (source column Level1)
#   ClassDetailed  = 12-class detailed taxonomy (source column DocClassFinal1)
#   ClassDetailed2 = dual-class second label (DocClassFinal2, NA for single-class
#                    docs); LENIENT scoring only, training stays single-label
#   AmendType      = amendment task label: Original vs Amended (NA for some docs)
#   LabelRound     = label source: Round1 = automated, Round2 = manual / gold
#   DocDesc        = filer title; DocName = filename. Keyword-track inputs.
#
# House style: native pipe; explicit package::function; dot-prefixed args;
# underscore-suffixed locals; .data$ for existing columns in dplyr verbs, bare
# CamelCase for new columns; if (FALSE) dev blocks; cli/fs/here; pure ASCII;
# stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .tab_input   <- fils_class_sample
  .path_labels <- .lP$Input$ClassificationSample
  .path_data   <- .lP$Output$Prepared
  .runs_roots  <- c(.lP$Runs$Bert, .lP$Runs$Kw)
}


# Disk cache --------------------------------------------------------------
# Lightweight disk memoisation for the report docs. Replaces per-chunk eval
# guards: a chunk always runs, but the costly read / scoring behind it happens
# once. clf_cache() returns the stored value when present and only evaluates its
# expression on a miss (or when overwriting), so a warm cache is free. Flip
# everything at once with options(clf.cache.overwrite = TRUE) -- e.g. after a fresh
# sweep -- or force a single key with .overwrite = TRUE.

#' Compute-once disk cache for a report artifact
#'
#' Returns the cached value for .key when present (and .overwrite is FALSE);
#' otherwise evaluates .expr, stores it, and returns it. .expr is a lazily-evaluated
#' argument -- on a cache hit it is never forced, so the computation does not run.
#' This is the substitute for scattering `eval: !expr runs_exist_` across chunks:
#' the chunk runs unconditionally, the disk read happens at most once, and an
#' existing result is never clobbered unless asked.
#'
#' @param .key Character. Cache name; sanitised into a file name.
#' @param .expr Expression evaluated only on a miss (untouched on a hit).
#' @param .overwrite Logical. Recompute and overwrite even if cached. Defaults to
#'   getOption("clf.cache.overwrite", FALSE).
#' @param .dir Cache directory (created if needed).
#' @return The cached or freshly-computed value.
clf_cache <- function(.key, .expr,
                      .overwrite = getOption("clf.cache.overwrite", FALSE),
                      .dir = here::here("2_output", "_cache")) {
  if (FALSE) {
    .key       <- "kw_overall"
    .expr      <- clf_load_overall(.lP$Output$RunsDir)
    .overwrite <- FALSE
    .dir       <- here::here("2_output", "_cache")
  }
  fs::dir_create(.dir)
  safe_ <- gsub("[^A-Za-z0-9_.-]", "_", .key)
  path_ <- fs::path(.dir, paste0(safe_, ".rds"))
  if (!.overwrite && fs::file_exists(path_)) {
    cli::cli_alert_info("cache hit: {(.key)}")
    return(readRDS(path_))
  }
  val_ <- .expr                       # forces the promise -- compute now
  saveRDS(val_, path_)
  act_ <- if (.overwrite) "overwrite" else "write"
  cli::cli_alert_success("cache {act_}: {(.key)}")
  val_
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
  if (is.null(tab_) || !"TextRaw" %in% names(tab_)) return(NA_character_)
  txt_ <- tab_[["TextRaw"]]
  if (length(txt_) == 0L) return(NA_character_)
  paste(txt_, collapse = "\n")
}

#' Build the single prepared classification sample with frozen stratified folds
#'
#' Consumes the pre-pathed labelled df as the spine and emits the ONE prepared
#' table both tracks read. Renames source columns to ClassBroad (Level1) and
#' ClassDetailed (DocClassFinal1); carries the dual-class second label
#' (ClassDetailed2), the amendment label (AmendType), and -- new vs the old BERT
#' prep -- DocDesc / DocName for the keyword track (empty string when absent).
#' Recodes the label source to LabelRound, reads text, and deals a deterministic
#' stratified k-fold assignment (stratified on ClassDetailed, reused for
#' ClassBroad and AmendType so all tasks share folds). By default keeps all rounds
#' (the full combined sample); pass .round = "Round2" to restrict to gold labels.
#'
#' @param .tab_input Tibble. Must contain DocID, Path, Level1, DocClassFinal1.
#'   DocClassFinal2, AmendType, Provenance, DocDesc, DocName are used if present.
#' @param .path_labels Path to label parquet (used only to join Provenance if absent).
#' @param .round Character or NULL. Keep only this round; NULL keeps all.
#' @param .k Integer. Number of folds.
#' @param .seed Integer. RNG seed.
#' @return Tibble: DocID, Text, DocDesc, DocName, ClassBroad, ClassDetailed,
#'   ClassDetailed2, AmendType, LabelRound, Fold.
clf_prepare_sample <- function(.tab_input, .path_labels = NULL,
                               .round = NULL, .k = 5L, .seed = 42L) {
  if (FALSE) {
    .tab_input   <- fils_class_sample
    .path_labels <- .lP$Input$ClassificationSample
    .round       <- NULL
    .k           <- 5L
    .seed        <- 42L
  }

  need_ <- c("DocID", "Path", "Level1", "DocClassFinal1")
  miss_cols_ <- setdiff(need_, names(.tab_input))
  if (length(miss_cols_) > 0L) cli::cli_abort("Input missing columns: {miss_cols_}")

  round_lab_ <- if (is.null(.round)) "all rounds" else .round

  # NA-fill optional columns so the select is stable whether or not they exist
  if (!"DocClassFinal2" %in% names(.tab_input)) .tab_input <- .tab_input |> dplyr::mutate(DocClassFinal2 = NA_character_)
  if (!"AmendType"      %in% names(.tab_input)) .tab_input <- .tab_input |> dplyr::mutate(AmendType = NA_character_)

  tab_ <- .tab_input |>
    dplyr::select(DocID, Path,
                  ClassBroad = Level1, ClassDetailed = DocClassFinal1,
                  ClassDetailed2 = DocClassFinal2, AmendType,
                  dplyr::any_of(c("Provenance", "DocDesc", "DocName")))

  # carry the keyword-track fields; create empty if the metadata join was absent
  if (!"DocDesc" %in% names(tab_)) tab_ <- tab_ |> dplyr::mutate(DocDesc = NA_character_)
  if (!"DocName" %in% names(tab_)) tab_ <- tab_ |> dplyr::mutate(DocName = NA_character_)

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
        .data$Provenance == "S2"          ~ "Round2",
        TRUE                               ~ NA_character_
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

  # title coverage is informative for the keyword track (docdesc-only abstains
  # where the title is empty); report it once here.
  ready_ <- ready_ |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("DocDesc", "DocName")), ~ dplyr::coalesce(.x, "")))
  n_no_desc_ <- sum(ready_$DocDesc == "")
  if (n_no_desc_ > 0L) cli::cli_alert_info("{n_no_desc_} docs have empty DocDesc (keyword docdesc model abstains on these)")

  thin_ <- ready_ |> dplyr::count(.data$ClassDetailed) |> dplyr::filter(.data$n < .k)
  if (nrow(thin_) > 0L) {
    cli::cli_alert_warning("Classes with < {(.k)} docs (a fold may lack them): {paste(thin_$ClassDetailed, collapse = ', ')}")
  }

  set.seed(.seed)
  out_ <- ready_ |>
    dplyr::arrange(.data$ClassDetailed, .data$DocID) |>
    dplyr::group_by(.data$ClassDetailed) |>
    dplyr::mutate(Fold = ((sample(dplyr::n()) - 1L) %% .k) + 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(DocID, Text, DocDesc, DocName, ClassBroad, ClassDetailed,
                     ClassDetailed2, AmendType, LabelRound, Fold)

  cli::cli_alert_success("Prepared {nrow(out_)} docs across {(.k)} folds")
  out_
}

#' Write the prepared sample to parquet
#' @param .tab Prepared tibble from clf_prepare_sample().
#' @param .path_out Output parquet path.
#' @return Invisible path written.
clf_write_prepared <- function(.tab, .path_out) {
  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(.tab, .path_out)
  cli::cli_alert_success("Wrote {(.path_out)} ({nrow(.tab)} docs)")
  invisible(.path_out)
}

#' Per-fold class counts for one granularity (eyeball the stratification)
#' @param .tab Prepared tibble.
#' @param .level One of "ClassDetailed", "ClassBroad", "AmendType".
#' @return Tibble: Label, one column per fold, Total.
clf_fold_overview <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType")) {
  .level <- match.arg(.level)
  .tab |>
    dplyr::filter(!is.na(.data[[.level]])) |>
    dplyr::count(Label = .data[[.level]], .data$Fold) |>
    tidyr::pivot_wider(names_from = "Fold", values_from = "n",
                       values_fill = 0L, names_prefix = "F") |>
    dplyr::rowwise() |>
    dplyr::mutate(Total = sum(dplyr::c_across(dplyr::starts_with("F")))) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(.data$Total))
}


# Sample description (publication tables) ---------------------------------

#' Headline composition of the labelled sample (one row, publication summary)
#'
#' The "here is our data resource" line: total docs, the Round1 / Round2 split,
#' how many carry a dual-class second label, and the amendment-task breakdown.
#'
#' @param .tab Prepared tibble.
#' @return One-row tibble.
clf_sample_composition <- function(.tab) {
  if (FALSE) .tab <- arrow::read_parquet(.lP$Output$Prepared)
  tibble::tibble(
    N          = nrow(.tab),
    nRound1    = sum(.tab$LabelRound == "Round1", na.rm = TRUE),
    nRound2    = sum(.tab$LabelRound == "Round2", na.rm = TRUE),
    nDualClass = sum(!is.na(.tab$ClassDetailed2)),
    nOriginal  = sum(.tab$AmendType == "Original", na.rm = TRUE),
    nAmended   = sum(.tab$AmendType == "Amended", na.rm = TRUE),
    nAmendNA   = sum(is.na(.tab$AmendType))
  )
}

#' Class distribution at one granularity (count + share, sorted)
#'
#' Works for ClassDetailed / ClassBroad (no NAs by construction) and for the
#' amendment task AmendType (which does carry NAs). With .include_na = TRUE the
#' NA docs become an explicit "(unlabeled)" row -- the honest way to show the
#' AmendType breakdown, where the unlabeled share is the part the amendment
#' classifier never sees.
#'
#' @param .tab Prepared tibble.
#' @param .level "ClassDetailed", "ClassBroad", or "AmendType".
#' @param .include_na Logical. Keep NA as an "(unlabeled)" row (default FALSE).
#' @return Tibble: Class, N, Pct (descending by N).
clf_class_distribution <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType"),
                                   .include_na = FALSE) {
  .level <- match.arg(.level)
  tab_ <- if (.include_na) {
    .tab |> dplyr::mutate(dplyr::across(dplyr::all_of(.level),
                                        ~ dplyr::coalesce(as.character(.x), "(unlabeled)")))
  } else {
    .tab |> dplyr::filter(!is.na(.data[[.level]]))
  }
  tab_ |>
    dplyr::count(Class = .data[[.level]], name = "N") |>
    dplyr::mutate(Pct = .data$N / sum(.data$N)) |>
    dplyr::arrange(dplyr::desc(.data$N))
}

#' Document-length distribution (the max_len rationale)
#'
#' Contracts are long; at 512 tokens the model sees only the opening pages. The
#' word thresholds (~200w ~ 256 tokens, ~400w ~ 512 tokens) quantify how much
#' tail is truncated and motivate the 256-vs-512 sweep axis.
#'
#' @param .tab Prepared tibble.
#' @param .text_col Column to measure (default "Text").
#' @return One-row tibble of length summaries.
clf_length_summary <- function(.tab, .text_col = "Text") {
  n_ <- stringi::stri_count_words(.tab[[.text_col]])
  tibble::tibble(
    N           = length(n_),
    Median      = stats::median(n_),
    P90         = stats::quantile(n_, 0.90, names = FALSE),
    P99         = stats::quantile(n_, 0.99, names = FALSE),
    PctOver200w = mean(n_ > 200),
    PctOver400w = mean(n_ > 400)
  )
}

#' Missingness audit across all prepared columns
#'
#' Per-column NA count / share, plus an empty-string count for character columns
#' (DocDesc / DocName are coalesced to "" in prep, so an absent title shows up as
#' empty, not NA). Two kinds of NA live in this table and the distinction is
#' editorial, not mechanical:
#'   - STRUCTURAL: ClassDetailed2 is NA for every single-class doc (the large
#'     majority). That is the definition of a single-class doc, not missing data.
#'   - GENUINE: AmendType is NA for docs never labelled for the amendment task;
#'     that subset is simply excluded when the amendment classifier trains.
#' ClassBroad / ClassDetailed / Text / Fold are NA-free by construction (the prep
#' filters or assigns them), so a nonzero count there signals an upstream problem.
#'
#' @param .tab Prepared tibble.
#' @return Tibble: Column, nNA, PctNA, nEmpty (descending by nNA).
clf_na_overview <- function(.tab) {
  if (FALSE) .tab <- arrow::read_parquet(.lP$Output$Prepared)
  n_ <- nrow(.tab)
  na_ <- .tab |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Column", values_to = "nNA")
  empty_ <- .tab |>
    dplyr::summarise(dplyr::across(dplyr::where(is.character), ~ sum(.x == "", na.rm = TRUE))) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Column", values_to = "nEmpty")
  na_ |>
    dplyr::left_join(empty_, by = dplyr::join_by(Column)) |>
    dplyr::mutate(
      nEmpty = dplyr::coalesce(.data$nEmpty, 0L),
      PctNA  = .data$nNA / n_
    ) |>
    dplyr::select(Column, nNA, PctNA, nEmpty) |>
    dplyr::arrange(dplyr::desc(.data$nNA), dplyr::desc(.data$nEmpty))
}

#' Dual-class headline: count, share, and Round split
#'
#' Dual-class docs carry a second valid label (ClassDetailed2). Automated Round1
#' can only emit a single label, so dual labels are confined to Round2 (manual /
#' gold) -- the Round split below confirms it. The primary of the two is assigned
#' by a manual review pass, not by list order; we train single-label on that
#' primary and keep the secondary for lenient robustness scoring and description.
#'
#' @param .tab Prepared tibble.
#' @return One-row tibble: nDual, PctDual, nDualRound1, nDualRound2.
clf_dual_class_overview <- function(.tab) {
  dual_ <- .tab |> dplyr::filter(!is.na(.data$ClassDetailed2))
  tibble::tibble(
    nDual       = nrow(dual_),
    PctDual     = nrow(dual_) / nrow(.tab),
    nDualRound1 = sum(dual_$LabelRound == "Round1", na.rm = TRUE),
    nDualRound2 = sum(dual_$LabelRound == "Round2", na.rm = TRUE)
  )
}

#' Co-occurring class pairs among dual-class docs (directional)
#'
#' Every dual-classified document has been through a manual review pass that
#' assigns which of the two categories is the primary, so the order carries
#' information and is reported as it stands. (It previously did not: the pair was
#' sorted alphabetically because the two labels were treated as co-equal. That is
#' no longer the case, and collapsing the direction would discard the review.)
#' The result shows which contract types are taxonomically adjacent and, where one
#' direction dominates a pair, which category the reviewer consistently led with.
#'
#' @param .tab Prepared tibble.
#' @return Tibble: ClassDetailed, ClassDetailed2, N (descending by N).
clf_dual_class_pairs <- function(.tab) {
  if (FALSE) .tab <- arrow::read_parquet(.lP$Output$Prepared)
  .tab |>
    dplyr::filter(!is.na(.data$ClassDetailed2)) |>
    dplyr::count(.data$ClassDetailed, .data$ClassDetailed2, name = "N") |>
    dplyr::arrange(dplyr::desc(.data$N))
}


# Overview: load + leaderboard --------------------------------------------

#' Bind all per-fold overall-metrics rows from one or more runs trees
#'
#' Accepts a vector of runs roots so a single call can pool BERT (03B) and
#' keyword (03C) runs for a head-to-head. Coverage is written only by the keyword
#' engine; BERT always predicts, so its effective coverage is 1 -- this
#' materialises a uniform Coverage column so the leaderboard and any downstream
#' bind never have to branch on method.
#'
#' @param .runs_roots Character vector of one or more runs directories.
#' @return Tibble of per-(config x fold) overall metrics.
clf_load_overall <- function(.runs_roots) {
  if (FALSE) .runs_roots <- c(.lP$Runs$Bert, .lP$Runs$Kw)
  paths_ <- .runs_roots |>
    purrr::map(\(.r) fs::dir_ls(.r, recurse = TRUE, glob = "*metrics_overall.parquet")) |>
    purrr::list_c()
  paths_ <- paths_[!grepl("_smoke", paths_)]
  if (length(paths_) == 0L) cli::cli_abort("No metrics_overall.parquet under {(.runs_roots)}")

  out_ <- purrr::map(paths_, arrow::read_parquet) |> purrr::list_rbind()
  if (!"Coverage" %in% names(out_)) {
    out_ <- out_ |> dplyr::mutate(Coverage = 1)
  } else {
    out_ <- out_ |> dplyr::mutate(Coverage = dplyr::coalesce(.data$Coverage, 1))
  }
  out_
}

#' Leaderboard: mean / sd across folds, one row per configuration (numeric)
#'
#' Keyed on ConfigName, which already encodes every axis as a string, so the same
#' function ranks BERT and keyword configs without knowing their (different) axes.
#'
#' @param .tab_overall Output of clf_load_overall().
#' @return One row per ConfigName, descending by mean macro-F1.
clf_leaderboard <- function(.tab_overall) {
  .tab_overall |>
    dplyr::filter(!.data$Smoke) |>
    dplyr::summarise(
      nFolds        = dplyr::n(),
      Acc_mean      = mean(.data$Accuracy),    Acc_sd      = sd(.data$Accuracy),
      F1macro_mean  = mean(.data$F1_macro),    F1macro_sd  = sd(.data$F1_macro),
      F1weight_mean = mean(.data$F1_weighted), F1weight_sd = sd(.data$F1_weighted),
      Cov_mean      = mean(.data$Coverage),
      .by = c(ConfigName, Model, LabelCol, TextCol, Seed)
    ) |>
    dplyr::arrange(dplyr::desc(.data$F1macro_mean))
}

#' Leaderboard, formatted for reading (top .n configs, mean +/- sd as strings)
#'
#' @param .tab_overall Output of clf_load_overall().
#' @param .label_col Optional character. Restrict to one task (e.g. "ClassDetailed").
#' @param .n Integer. Rows to show.
#' @return Formatted tibble.
clf_leaderboard_show <- function(.tab_overall, .label_col = NULL, .n = 20L) {
  tab_ <- if (is.null(.label_col)) .tab_overall else dplyr::filter(.tab_overall, .data$LabelCol == .label_col)
  clf_leaderboard(tab_) |>
    dplyr::mutate(
      Rank        = dplyr::row_number(),
      Coverage    = sprintf("%.3f", .data$Cov_mean),
      Accuracy    = sprintf("%.3f +/- %.3f", .data$Acc_mean, .data$Acc_sd),
      F1_macro    = sprintf("%.3f +/- %.3f", .data$F1macro_mean, .data$F1macro_sd),
      F1_weighted = sprintf("%.3f +/- %.3f", .data$F1weight_mean, .data$F1weight_sd)
    ) |>
    dplyr::select(Rank, ConfigName, Model, LabelCol, nFolds, Coverage,
                  Accuracy, F1_macro, F1_weighted) |>
    head(.n)
}

#' Marginal effect of one sweep axis on macro-F1 (all else averaged over)
#'
#' Generic: groups by whatever column you name (Model, MaxLen, Epochs, LR,
#' ClassWeights for BERT; Source, Stopwords, TopK for keyword). Rows where the
#' axis is NA (e.g. MaxLen on keyword rows in a pooled table) are dropped first.
#' Optionally restrict to one task via .label_col (recommended, since tasks differ
#' in difficulty). The grid is balanced, so this is a fair marginal mean.
#'
#' @param .tab_overall Output of clf_load_overall().
#' @param .axis Character column name to group by.
#' @param .label_col Optional character. Restrict to one task first.
#' @return Tibble of marginal means, descending by macro-F1.
clf_effect <- function(.tab_overall, .axis, .label_col = NULL) {
  tab_ <- if (is.null(.label_col)) .tab_overall else dplyr::filter(.tab_overall, .data$LabelCol == .label_col)
  tab_ |>
    dplyr::filter(!.data$Smoke, !is.na(.data[[.axis]])) |>
    dplyr::summarise(
      nRuns        = dplyr::n(),
      F1macro_mean = mean(.data$F1_macro),
      F1macro_sd   = sd(.data$F1_macro),
      Acc_mean     = mean(.data$Accuracy),
      .by = dplyr::all_of(.axis)
    ) |>
    dplyr::arrange(dplyr::desc(.data$F1macro_mean))
}


# Pooled out-of-fold predictions ------------------------------------------

#' Pool out-of-fold predictions for one configuration across all folds / roots
#'
#' Each doc is predicted once, by a model that did not train on it -- the honest
#' per-class metric from CV. Accepts a vector of roots so a router (03D) can pull
#' a BERT config and a keyword config from their separate trees.
#'
#' @param .runs_roots Character vector of one or more runs directories.
#' @param .config_name ConfigName string (from the leaderboard).
#' @return Pooled predictions tibble (DocID, TrueLabel, PredLabel, Score, ...).
clf_pool_predictions <- function(.runs_roots, .config_name) {
  paths_ <- .runs_roots |>
    purrr::map(\(.r) fs::dir_ls(.r, recurse = TRUE, glob = "*predictions.parquet")) |>
    purrr::list_c()
  paths_ <- paths_[!grepl("_smoke", paths_)]
  purrr::map(paths_, arrow::read_parquet) |>
    purrr::list_rbind() |>
    dplyr::filter(.data$ConfigName == .config_name)
}

#' Attach the dual-class second label to pooled predictions (for lenient scoring)
#' @param .tab_pred Pooled predictions (must contain DocID).
#' @param .tab_prep Prepared sample (must contain DocID, ClassDetailed2).
#' @return .tab_pred with ClassDetailed2 joined on.
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
#' @return .tab_pred with LabelRound joined on.
clf_add_labelround <- function(.tab_pred, .tab_prep) {
  .tab_pred |>
    dplyr::inner_join(
      .tab_prep |> dplyr::select(DocID, LabelRound),
      by = dplyr::join_by(DocID)
    )
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


# Unified scoring (abstention-aware) --------------------------------------
# One scoring layer for every method. The .none argument names the abstention
# sentinel: BERT never abstains, so "(none)" is absent from its labels and the
# setdiff / coverage terms are no-ops (numbers identical to the old clf_*).
# Keyword can abstain ("(none)"); the sentinel is dropped from the class set and
# the macro average, and Coverage reports the predicted share. An abstained doc
# is a false negative for its true class and a false positive for nothing -- so
# abstaining costs recall and protects precision, the semantics we want.

#' Internal: per-row correctness vector (strict or lenient)
#'
#' Strict: prediction matches the primary label. Lenient: prediction matches
#' EITHER the primary (TrueLabel) or the dual-class second label (ClassDetailed2,
#' when present).
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
#' Precision(c) is over docs predicted c; recall(c) is over docs whose PRIMARY
#' label is c (each doc in exactly one recall bucket). Lenient scoring credits a
#' dual-class doc as correct if the model predicts either of its two valid labels.
#'
#' @param .tab_pred Pooled predictions (DocID, TrueLabel, PredLabel; optional
#'   ClassDetailed2 for lenient scoring via clf_add_second_label).
#' @param .lenient Logical. Accept either the primary or the dual-class second label.
#' @param .none Character. Abstention sentinel to exclude from the class set
#'   (default "(none)"; harmless when no row carries it).
#' @return Tibble: Label, Precision, Recall, F1, Support (descending by Support).
clf_perclass <- function(.tab_pred, .lenient = FALSE, .none = "(none)") {
  if (FALSE) {
    .tab_pred <- clf_pool_predictions(c(.lP$Runs$Bert, .lP$Runs$Kw), best_)
    .lenient  <- FALSE
    .none     <- "(none)"
  }
  if (.lenient && !"ClassDetailed2" %in% names(.tab_pred)) {
    cli::cli_alert_warning("Lenient requested but no ClassDetailed2 column; using strict. Join via clf_add_second_label().")
    .lenient <- FALSE
  }
  correct_ <- clf_correct_vec(.tab_pred, .lenient)
  pred_ <- .tab_pred$PredLabel
  true_ <- .tab_pred$TrueLabel
  classes_ <- setdiff(sort(unique(c(true_, pred_))), .none)   # drop abstain sentinel
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

#' Headline scores (accuracy, macro-F1, weighted-F1, coverage) from pooled predictions
#'
#' Coverage is the share of docs the method predicted (1 - abstention rate); it is
#' 1 for any method that always predicts (e.g. BERT). Accuracy counts an
#' abstention as incorrect.
#'
#' @param .tab_pred Pooled predictions.
#' @param .lenient Logical. Lenient (either-label) scoring; needs ClassDetailed2.
#' @param .none Character. Abstention sentinel (default "(none)").
#' @return One-row tibble: Scoring, Accuracy, F1_macro, F1_weighted, Coverage, N.
clf_scores <- function(.tab_pred, .lenient = FALSE, .none = "(none)") {
  if (.lenient && !"ClassDetailed2" %in% names(.tab_pred)) {
    cli::cli_alert_warning("Lenient requested but no ClassDetailed2 column; using strict. Join via clf_add_second_label().")
    .lenient <- FALSE
  }
  pc_ <- clf_perclass(.tab_pred, .lenient = .lenient, .none = .none)
  correct_ <- clf_correct_vec(.tab_pred, .lenient)
  tibble::tibble(
    Scoring     = if (.lenient) "lenient" else "strict",
    Accuracy    = mean(correct_),
    F1_macro    = mean(pc_$F1),
    F1_weighted = sum(pc_$F1 * pc_$Support) / sum(pc_$Support),
    Coverage    = mean(.tab_pred$PredLabel != .none),
    N           = nrow(.tab_pred)
  )
}

#' Confusion matrix (rows = true, cols = predicted) from pooled predictions
#'
#' A "(none)" column, when present, shows which true classes the method abstained
#' on -- useful for seeing where a keyword lexicon has no signal.
#'
#' @param .tab_pred Pooled predictions.
#' @return Wide tibble: TrueLabel plus one column per predicted label.
clf_confusion <- function(.tab_pred) {
  .tab_pred |>
    dplyr::count(.data$TrueLabel, .data$PredLabel) |>
    tidyr::pivot_wider(names_from = "PredLabel", values_from = "n", values_fill = 0L) |>
    dplyr::arrange(.data$TrueLabel)
}


# Publication plots -------------------------------------------------------
# ggplot helpers returning publication-ready objects (house theme: Times, classic,
# thin black axes, no grid, legend bottom). Method-agnostic: 03B / 03C / 03D / 03E
# all call these. The results plots populate once the sweeps land; the data plots
# work immediately. Mirrors plot_add_theme() from the shared plot utils -- swap
# that in if you prefer to keep the theme in one place.

#' Apply the house publication theme to a ggplot
#' @param .plot A ggplot object.
#' @return The plot with the house theme applied.
clf_apply_theme <- function(.plot) {
  .plot +
    ggplot2::theme_classic(base_size = 12, base_family = "Times New Roman", base_line_size = 0.2) +
    ggplot2::theme(
      text             = ggplot2::element_text(family = "Times New Roman"),
      axis.title       = ggplot2::element_text(face = "plain", size = 11),
      axis.text        = ggplot2::element_text(color = "black"),
      axis.line        = ggplot2::element_line(linewidth = 0.2, color = "black"),
      axis.ticks       = ggplot2::element_line(linewidth = 0.2, color = "black"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5, size = 12),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_blank(),
      legend.key.size  = ggplot2::unit(0.5, "cm")
    )
}

#' Bar chart of the sample's class distribution at one granularity
#' @param .tab Prepared tibble.
#' @param .level "ClassDetailed", "ClassBroad", or "AmendType".
#' @param .include_na Logical. Keep NA as an "(unlabeled)" bar (default FALSE).
#' @return A ggplot.
clf_plot_class_distribution <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType"),
                                        .include_na = FALSE) {
  .level <- match.arg(.level)
  dat_ <- clf_class_distribution(.tab, .level, .include_na = .include_na)
  p_ <- dat_ |>
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(Class, N), y = N)) +
    ggplot2::geom_col(fill = "grey30", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(N)), hjust = -0.15, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = NULL, y = "Documents")
  clf_apply_theme(p_)
}

#' Horizontal bars of per-class F1 from pooled predictions
#' @param .tab_pred Pooled predictions.
#' @param .lenient Logical. Lenient scoring (needs ClassDetailed2).
#' @param .none Character. Abstention sentinel.
#' @return A ggplot.
clf_plot_perclass <- function(.tab_pred, .lenient = FALSE, .none = "(none)") {
  pc_ <- clf_perclass(.tab_pred, .lenient = .lenient, .none = .none)
  p_ <- pc_ |>
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(Label, F1), y = F1)) +
    ggplot2::geom_col(fill = "grey30", width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", F1)), hjust = -0.2, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, 1.08), expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::labs(x = NULL, y = "F1")
  clf_apply_theme(p_)
}

#' Confusion heatmap (rows = true, cols = predicted)
#' @param .tab_pred Pooled predictions.
#' @param .normalize Logical. Row-normalise to within-true-class shares.
#' @return A ggplot.
clf_plot_confusion <- function(.tab_pred, .normalize = TRUE) {
  long_ <- .tab_pred |> dplyr::count(.data$TrueLabel, .data$PredLabel)
  long_ <- if (.normalize) {
    long_ |> dplyr::mutate(Value = .data$n / sum(.data$n), .by = TrueLabel)
  } else {
    long_ |> dplyr::mutate(Value = .data$n)
  }
  p_ <- long_ |>
    ggplot2::ggplot(ggplot2::aes(x = PredLabel, y = TrueLabel, fill = Value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient(low = "white", high = "grey20") +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(x = "Predicted", y = "True", fill = if (.normalize) "Share" else "N")
  clf_apply_theme(p_) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Leaderboard dot-and-error-bar plot (mean +/- sd macro-F1 per config)
#' @param .tab_overall Output of clf_load_overall().
#' @param .label_col Optional character. Restrict to one task.
#' @param .n Integer. Top configs to show.
#' @return A ggplot.
clf_plot_leaderboard <- function(.tab_overall, .label_col = NULL, .n = 15L) {
  lb_ <- clf_leaderboard(
    if (is.null(.label_col)) .tab_overall else dplyr::filter(.tab_overall, .data$LabelCol == .label_col)
  ) |> head(.n)
  p_ <- lb_ |>
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(ConfigName, F1macro_mean), y = F1macro_mean)) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = F1macro_mean - F1macro_sd, ymax = F1macro_mean + F1macro_sd),
      width = 0.25, linewidth = 0.3
    ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Macro-F1 (mean +/- sd across folds)")
  clf_apply_theme(p_)
}

#' Per-class strict-vs-lenient F1 (dumbbell; the dual-class cost)
#'
#' Requires ClassDetailed2 joined on (clf_add_second_label). The gap between the
#' strict and lenient point for a class is what dual-class ambiguity costs it --
#' concentrated on the rare classes that appear as second labels.
#'
#' @param .tab_pred Pooled predictions with ClassDetailed2 attached.
#' @param .none Character. Abstention sentinel.
#' @return A ggplot.
clf_plot_strict_lenient <- function(.tab_pred, .none = "(none)") {
  if (!"ClassDetailed2" %in% names(.tab_pred)) {
    cli::cli_abort("Need ClassDetailed2; join via clf_add_second_label().")
  }
  s_ <- clf_perclass(.tab_pred, .lenient = FALSE, .none = .none) |> dplyr::select(Label, Strict = F1)
  l_ <- clf_perclass(.tab_pred, .lenient = TRUE,  .none = .none) |> dplyr::select(Label, Lenient = F1)
  dat_ <- dplyr::inner_join(s_, l_, by = dplyr::join_by(Label)) |>
    dplyr::mutate(Label = forcats::fct_reorder(Label, Lenient))

  p_ <- dat_ |>
    ggplot2::ggplot(ggplot2::aes(y = Label)) +
    ggplot2::geom_segment(ggplot2::aes(x = Strict, xend = Lenient, y = Label, yend = Label),
                          color = "grey70", linewidth = 0.5) +
    ggplot2::geom_point(ggplot2::aes(x = Strict, color = "Strict"), size = 2) +
    ggplot2::geom_point(ggplot2::aes(x = Lenient, color = "Lenient"), size = 2) +
    ggplot2::scale_color_manual(values = c(Strict = "grey60", Lenient = "black")) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "F1", y = NULL, color = NULL)
  clf_apply_theme(p_)
}
