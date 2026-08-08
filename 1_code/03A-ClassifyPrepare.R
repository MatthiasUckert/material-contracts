# 03A-ClassifyPrepare: build the training sample + shared tooling ----
#
# WHAT THIS FILE DOES, IN ONE PARAGRAPH
# We have ~4.4k contracts that a human has labelled. 03A turns that label list
# into ONE table -- prepared.parquet -- that every downstream method reads. That
# table holds, per document: the text, the labels for all three tasks, and a fold
# number. Nothing here trains anything. The whole job is (a) decide which
# documents are eligible, (b) attach their text, (c) deal the folds once so every
# method is scored on identical splits.
#
# THE THREE TASKS (all predicted from the same Text column)
#   ClassDetailed  12-class contract taxonomy   <- the headline task
#   ClassBroad      7-class roll-up of the above
#   AmendType       2 classes: Original vs Amended
# All three share the SAME folds, dealt stratified on ClassDetailed. That is why
# fold assignment lives here and not in the trainers: if each task dealt its own
# folds, cross-task comparisons would be meaningless.
#
# SINGLE-LABEL, ALWAYS
# A minority of documents carry a second valid category (ClassDetailed2). We
# train on the primary only. The second label is never a training target; it is
# used solely for lenient scoring, which asks "would this prediction have been
# accepted as the other valid answer?".
#
# WHAT IS *NOT* HERE
# The trainers: bert_* is 03B, kw_* is 03C, the router is 03D. They source this
# file so the folds and the scoring functions never drift.
#
# House style: native pipe; explicit package::function; dot-prefixed args;
# underscore-suffixed locals; .data$ for existing columns in dplyr verbs, bare
# CamelCase for new columns; if (FALSE) dev blocks; cli/fs/here; pure ASCII;
# {(.arg)} parens in cli interpolation.

if (FALSE) {
  .tab_input  <- fils_class_sample
  .path_data  <- .lP$Output$Prepared
  .runs_roots <- c(.lP$Runs$Bert, .lP$Runs$Kw)
}


# 1. Console reporting helpers --------------------------------------------
# Everything 03A reports goes through these, so the console output is one
# consistent, fixed-width, copy-pasteable block rather than a scatter of tables.

#' Render a tibble as aligned fixed-width character lines
#'
#' Numeric columns are right-aligned and comma-grouped; character columns are
#' left-aligned and NA renders as "-". Anything needing custom formatting
#' (percentages, ratios) should be pre-formatted to character by the caller.
#'
#' @param .tab Tibble to render.
#' @param .indent Integer. Leading spaces.
#' @return Character vector, one element per line (header first).
clf_fmt_table <- function(.tab, .indent = 2L) {
  if (FALSE) {
    .tab    <- tibble::tibble(Class = c("Leases", "R&D"), N = c(303L, 83L))
    .indent <- 2L
  }
  pad_ <- strrep(" ", .indent)
  if (nrow(.tab) == 0L) return(paste0(pad_, "(none)"))

  is_num_ <- purrr::map_lgl(.tab, is.numeric)
  cells_ <- purrr::map2(.tab, is_num_, \(.col, .num) {
    if (!.num) return(tidyr::replace_na(as.character(.col), "-"))
    # A count that arrives as a double -- and most do, since n() / total, sum() and nrow() all
    # produce one -- would otherwise print as 874.000, which reads as a measurement carrying three
    # significant decimals rather than as a tally. Whole-valued columns holding anything above one
    # are therefore formatted as counts. The upper-bound test is what protects proportions: a
    # coverage column that happens to be exactly 1 everywhere stays a decimal, because collapsing it
    # would hide that it is a share.
    whole_ <- all(.col == round(.col), na.rm = TRUE) && any(abs(.col) > 1, na.rm = TRUE)
    if (is.integer(.col) || whole_) {
      format(.col, big.mark = ",", trim = TRUE, scientific = FALSE)
    } else {
      formatC(.col, format = "f", digits = 3)
    }
  })

  head_ <- names(.tab)
  wid_  <- purrr::map2_int(cells_, head_, \(.c, .h) max(nchar(.c), nchar(.h)))
  side_ <- dplyr::if_else(is_num_, "left", "right")

  row_ <- function(.vals) {
    paste0(pad_, paste(
      purrr::pmap_chr(list(.vals, wid_, side_),
                      \(.v, .w, .s) stringr::str_pad(.v, .w, side = .s)),
      collapse = "  "
    ))
  }

  body_ <- purrr::map_chr(seq_len(nrow(.tab)),
                          \(.i) row_(purrr::map_chr(cells_, \(.c) .c[[.i]])))
  c(row_(head_), body_)
}

#' Print a tibble to the console as an aligned block
#'
#' @param .tab Tibble to print.
#' @param .title Optional heading printed above the block.
#' @return Invisibly .tab, so this can sit mid-pipe.
clf_say_table <- function(.tab, .title = NULL) {
  if (!is.null(.title)) cli::cli_h3(.title)
  cli::cli_verbatim(clf_fmt_table(.tab))
  invisible(.tab)
}

#' Format a proportion as a percentage string
#' @param .x Numeric vector in [0, 1].
#' @param .digits Integer. Decimal places.
#' @return Character vector.
clf_pct <- function(.x, .digits = 1L) {
  paste0(formatC(100 * .x, format = "f", digits = .digits), "%")
}


# 1a. Naming: short labels for models and configurations ---------------------
# Run identifiers are built to be unique and machine-parseable, which makes them roughly eighty
# characters long. That is fine on disk and unusable on a figure axis or in a console table, where a
# long identifier pushes every number that matters off the visible width. These two functions
# compress an identifier to the parts that actually vary within a study.

#' Short display name for a pre-trained model
#'
#' Drops the organisation prefix and the size/casing suffix that every checkpoint in a family shares.
#' Accepts either the hub form ("nlpaueb/legal-bert-base-uncased") or the slugged form used inside run
#' identifiers ("nlpaueb-legal-bert-base-uncased").
#'
#' @param .model Character vector of model identifiers.
#' @return Character vector of short names, e.g. "legal-bert", "roberta", "longformer".
clf_model_short <- function(.model) {
  if (FALSE) .model <- c("nlpaueb/legal-bert-base-uncased", "roberta-base")
  out_ <- sub("^.*/", "", .model)                                   # hub form: drop the organisation
  out_ <- sub("^(nlpaueb|allenai|google|facebook|microsoft)-", "", out_)   # slugged form: same job
  out_ <- sub("-base-uncased$|-base-cased$|-base-4096$|-base$", "", out_)  # shared family suffixes
  out_
}

#' Short display label for a run configuration
#'
#' Compresses a run identifier to the axes a sweep actually varies: model, context length, epochs,
#' learning rate and class weighting. Batch size, text column and seed are held constant across the
#' study and carry no information, so they are dropped. The task is dropped by default because
#' figures and tables are already produced per task.
#'
#' @param .config_name Character vector of run identifiers.
#' @param .keep_task Logical. Prefix the label with the task name.
#' @return Character vector, e.g. "legal-bert L256 E6 LR2e-05 W1".
clf_config_label <- function(.config_name, .keep_task = FALSE) {
  if (FALSE) {
    .config_name <- "ClassDetailed__nlpaueb-legal-bert-base-uncased__TText_L256_E6_B32_LR2e-05_W1_S42"
    .keep_task   <- FALSE
  }
  parts_ <- stringr::str_split_fixed(.config_name, stringr::fixed("__"), 3)
  spec_  <- parts_[, 3]

  out_ <- paste0(
    clf_model_short(parts_[, 2]),
    " L",  stringr::str_match(spec_, "_L(\\d+)")[, 2],
    " E",  stringr::str_match(spec_, "_E([0-9.]+)")[, 2],
    " LR", stringr::str_match(spec_, "_LR([0-9.e+-]+?)_W")[, 2],
    " W",  stringr::str_match(spec_, "_W([01])")[, 2]
  )
  if (.keep_task) paste0(parts_[, 1], ": ", out_) else out_
}


# 2. Disk cache -----------------------------------------------------------
# Lightweight memoisation for expensive report artifacts. A chunk always runs,
# but the costly computation behind it happens once.
#
# WARNING: the key is a NAME, not a hash of the data behind it. If the label
# spine changes, a warm cache will serve pre-change results with no error. Delete
# 2_output/_cache/ whenever the sample changes.

#' Compute-once disk cache for a report artifact
#'
#' Returns the cached value for .key when present (and .overwrite is FALSE);
#' otherwise evaluates .expr, stores it, and returns it. .expr is lazily
#' evaluated -- on a cache hit it is never forced, so the computation does not
#' run. Flip everything at once with options(clf.cache.overwrite = TRUE).
#'
#' @param .key Character. Cache name; sanitised into a file name.
#' @param .expr Expression evaluated only on a miss (untouched on a hit).
#' @param .overwrite Logical. Recompute and overwrite even if cached.
#' @param .dir Cache directory (created if needed).
#' @return The cached or freshly-computed value.
clf_cache <- function(.key, .expr,
                      .overwrite = getOption("clf.cache.overwrite", FALSE),
                      .dir = here::here("2_output", "_cache")) {
  if (FALSE) {
    .key       <- "kw_overall"
    .expr      <- clf_load_overall(.lP$Runs$Kw)
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
  val_ <- .expr
  saveRDS(val_, path_)
  cli::cli_alert_success("cache {if (.overwrite) 'overwrite' else 'write'}: {(.key)}")
  val_
}


# 3. Sample construction --------------------------------------------------

#' Read one parsed document's full text from its per-document parquet
#'
#' Returns NA on any failure (missing file / column / empty) so the caller can
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

#' Build the prepared training sample, reporting every document that drops out
#'
#' This is the only place a document can leave the sample, and it narrates each
#' departure. Four gates, in order:
#'
#'   1. HAS A CLASS LABEL. Unresolved documents (no Schema-2 label and no
#'      mappable Schema-1 label) carry NA and cannot train anything.
#'   2. HAS A FILE PATH. The label list is joined to the parsed-contract tree by
#'      DocID; a document with no matching file cannot supply text.
#'   3. FILE EXISTS ON DISK. Guards against a stale path cache.
#'   4. TEXT IS NON-EMPTY. A parquet that reads but yields nothing is useless.
#'
#' What survives gets ClassBroad / ClassDetailed / ClassDetailed2 / AmendType,
#' a LabelRound recode (S1_fallback -> Round1 automated, S2 -> Round2 manual),
#' DocDesc / DocName for the keyword track, and a deterministic stratified fold.
#' The intake cascade is attached as attr(out, "Intake") for clf_report_intake().
#'
#' @param .tab_input Tibble. Needs DocID, Path, Level1, DocClassFinal1;
#'   DocClassFinal2, AmendType, Provenance, DocDesc, DocName used if present.
#' @param .round Character or NULL. Keep only this LabelRound; NULL keeps all.
#' @param .k Integer. Number of folds.
#' @param .seed Integer. RNG seed for the fold deal.
#' @return Tibble: DocID, Text, DocDesc, DocName, ClassBroad, ClassDetailed,
#'   ClassDetailed2, AmendType, LabelRound, Fold. Carries attr "Intake".
clf_prepare_sample <- function(.tab_input, .round = NULL, .k = 5L, .seed = 42L) {
  if (FALSE) {
    .tab_input <- fils_class_sample
    .round     <- NULL
    .k         <- 5L
    .seed      <- 42L
  }

  need_ <- c("DocID", "Path", "Level1", "DocClassFinal1")
  miss_ <- setdiff(need_, names(.tab_input))
  if (length(miss_) > 0L) cli::cli_abort("Input missing columns: {miss_}")

  cli::cli_h2("Building the training sample")

  # Optional columns: materialise as NA so the select below is stable.
  for (col_ in c("DocClassFinal2", "AmendType", "DocDesc", "DocName")) {
    if (!col_ %in% names(.tab_input)) {
      .tab_input[[col_]] <- NA_character_
    }
  }
  if (!"Provenance" %in% names(.tab_input)) {
    cli::cli_abort("Input has no Provenance column -- cannot derive LabelRound.")
  }

  tab_ <- .tab_input |>
    dplyr::select(DocID, Path,
                  ClassBroad = Level1, ClassDetailed = DocClassFinal1,
                  ClassDetailed2 = DocClassFinal2, AmendType, Provenance,
                  DocDesc, DocName) |>
    dplyr::mutate(
      LabelRound = dplyr::case_when(
        .data$Provenance == "S1_fallback" ~ "Round1",
        .data$Provenance == "S2"          ~ "Round2",
        TRUE                              ~ NA_character_
      )
    )

  # Gate 0: optional round restriction (not a data-quality drop).
  n_read_ <- nrow(tab_)
  if (!is.null(.round)) {
    tab_ <- tab_ |> dplyr::filter(.data$LabelRound == .round)
    cli::cli_alert_info("Restricted to {(.round)}: {nrow(tab_)} of {n_read_} rows")
  }
  n_start_ <- nrow(tab_)

  # Gate 1: a usable class label.
  tab_ <- tab_ |> dplyr::filter(!is.na(.data$ClassBroad), !is.na(.data$ClassDetailed))
  n_lab_ <- nrow(tab_)

  # Gate 2: a path from the contract-file join.
  tab_ <- tab_ |> dplyr::filter(!is.na(.data$Path))
  n_path_ <- nrow(tab_)

  # Gate 3: that path resolves on disk.
  tab_ <- tab_ |> dplyr::filter(fs::file_exists(.data$Path))
  n_disk_ <- nrow(tab_)

  # Gate 4: the file yields text.
  cli::cli_alert_info("Reading {n_disk_} document texts ...")
  tab_ <- tab_ |>
    dplyr::mutate(Text = purrr::map_chr(.data$Path, clf_read_text, .progress = TRUE)) |>
    dplyr::filter(!is.na(.data$Text), trimws(.data$Text) != "")
  n_text_ <- nrow(tab_)

  if (n_text_ == 0L) cli::cli_abort("No documents survived intake -- nothing to prepare.")

  intake_ <- tibble::tribble(
    ~Stage,                      ~Docs,
    "Label rows in",             n_start_,
    "Has a class label",         n_lab_,
    "Has a contract file path",  n_path_,
    "File exists on disk",       n_disk_,
    "Text is non-empty",         n_text_
  ) |>
    dplyr::mutate(Dropped = dplyr::lag(.data$Docs, default = n_start_) - .data$Docs)

  # Deal the folds. Stratified on ClassDetailed and reused by every task, so the
  # rarest detailed class is the binding constraint on how thin a fold can get.
  thin_ <- tab_ |> dplyr::count(.data$ClassDetailed) |> dplyr::filter(.data$n < .k)
  if (nrow(thin_) > 0L) {
    cli::cli_alert_warning(
      "Classes with fewer than {(.k)} docs (a fold may lack them): {paste(thin_$ClassDetailed, collapse = ', ')}"
    )
  }

  set.seed(.seed)
  out_ <- tab_ |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("DocDesc", "DocName")), ~ dplyr::coalesce(.x, ""))) |>
    dplyr::arrange(.data$ClassDetailed, .data$DocID) |>
    dplyr::group_by(.data$ClassDetailed) |>
    dplyr::mutate(Fold = ((sample(dplyr::n()) - 1L) %% .k) + 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(DocID, Text, DocDesc, DocName, ClassBroad, ClassDetailed,
                     ClassDetailed2, AmendType, LabelRound, Fold)

  attr(out_, "Intake") <- intake_
  cli::cli_alert_success("Training sample: {nrow(out_)} docs across {(.k)} folds")
  out_
}

#' Write the prepared sample to parquet
#'
#' Note the "Intake" attribute does not survive the parquet round-trip; it is a
#' session artifact for the 03A report only.
#'
#' @param .tab Prepared tibble from clf_prepare_sample().
#' @param .path_out Output parquet path.
#' @return Invisible path written.
clf_write_prepared <- function(.tab, .path_out) {
  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(.tab, .path_out)
  cli::cli_alert_success("Wrote {(.path_out)} ({nrow(.tab)} docs)")
  invisible(.path_out)
}


# 4. Report: what goes into training --------------------------------------
# Each clf_report_* prints one block and returns its tibble invisibly, so the
# numbers stay available for further work. clf_report_sample() runs them all.

#' Intake cascade: how the label list became the training sample
#' @param .tab Prepared tibble (must carry attr "Intake").
#' @return Invisibly the intake tibble.
clf_report_intake <- function(.tab) {
  intake_ <- attr(.tab, "Intake")
  if (is.null(intake_)) {
    cli::cli_alert_warning("No intake record (attribute lost -- was this read back from parquet?)")
    return(invisible(NULL))
  }
  cli::cli_h2("1. Intake: which documents made it in")
  intake_ |>
    dplyr::mutate(
      Kept = clf_pct(.data$Docs / max(.data$Docs)),
      Dropped = as.integer(.data$Dropped)
    ) |>
    clf_say_table()
  cli::cli_text("")
  cli::cli_alert_info(
    "Every drop is accounted for above. A nonzero drop at {.strong File exists on disk} \\
     or {.strong Has a contract file path} is a join / path problem, not a labelling one."
  )
  invisible(intake_)
}

#' The three tasks: what each one actually trains on
#' @param .tab Prepared tibble.
#' @return Invisibly the task summary tibble.
clf_report_tasks <- function(.tab) {
  if (FALSE) .tab <- arrow::read_parquet(.lP$Output$Prepared)

  one_ <- function(.name, .col) {
    v_ <- .tab[[.col]]
    ok_ <- v_[!is.na(v_)]
    tab_n_ <- sort(table(ok_))
    tibble::tibble(
      Task          = .name,
      Column        = .col,
      Docs          = length(ok_),
      Unlabelled    = sum(is.na(v_)),
      Classes       = length(tab_n_),
      SmallestClass = paste0(names(tab_n_)[[1]], " (",
                             format(tab_n_[[1]], big.mark = ",", trim = TRUE), ")")
    )
  }

  out_ <- dplyr::bind_rows(
    one_("Detailed",  "ClassDetailed"),
    one_("Broad",     "ClassBroad"),
    one_("Amendment", "AmendType")
  )

  cli::cli_h2("2. The three tasks")
  clf_say_table(out_)
  cli::cli_text("")
  cli::cli_bullets(c(
    "*" = "All three are predicted from the same {.strong Text} column.",
    "*" = "All three share the same folds, dealt stratified on {.strong ClassDetailed}.",
    "*" = "{.strong Unlabelled} docs are dropped by the trainer for that task only.",
    "*" = "The smallest class drives macro-F1, which weights every class equally."
  ))
  invisible(out_)
}

#' Per-class distribution for one task, split by label round
#' @param .tab Prepared tibble.
#' @param .level One of ClassDetailed, ClassBroad, AmendType.
#' @return Invisibly the distribution tibble.
clf_report_classes <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType")) {
  .level <- match.arg(.level)
  n_ <- sum(!is.na(.tab[[.level]]))

  out_ <- .tab |>
    dplyr::filter(!is.na(.data[[.level]])) |>
    dplyr::count(Class = .data[[.level]], .data$LabelRound) |>
    tidyr::pivot_wider(names_from = "LabelRound", values_from = "n", values_fill = 0L) |>
    dplyr::mutate(N = as.integer(rowSums(dplyr::across(dplyr::where(is.numeric))))) |>
    dplyr::arrange(dplyr::desc(.data$N)) |>
    dplyr::mutate(Pct = clf_pct(.data$N / n_)) |>
    dplyr::relocate(Class, N, Pct, dplyr::any_of(c("Round1", "Round2")))

  cli::cli_h2("3. Class distribution -- {(.level)}")
  clf_say_table(out_)
  invisible(out_)
}

#' Class counts and shares for one task (tibble form; feeds the plot layer)
#' @param .tab Prepared tibble.
#' @param .level One of ClassDetailed, ClassBroad, AmendType.
#' @param .include_na Logical. Fold NA into an explicit "(unlabeled)" class.
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

#' Dual-class documents: headline count and the primary -> secondary pairs
#' @param .tab Prepared tibble.
#' @return Invisibly the pairs tibble.
clf_report_duals <- function(.tab, .n = 12L) {
  dual_ <- .tab |> dplyr::filter(!is.na(.data$ClassDetailed2))

  cli::cli_h2("4. Dual-class documents")
  cli::cli_alert_info(
    "{nrow(dual_)} of {nrow(.tab)} docs ({clf_pct(nrow(dual_) / nrow(.tab))}) carry a second valid category."
  )
  if (nrow(dual_) == 0L) return(invisible(NULL))

  by_round_ <- dual_ |> dplyr::count(.data$LabelRound, name = "Docs")
  clf_say_table(by_round_, "By label round")
  cli::cli_text("")
  cli::cli_alert_info(
    "Round1 is automated and emits one label, so duals should be Round2-only."
  )

  pairs_ <- dual_ |>
    dplyr::count(Primary = .data$ClassDetailed, Secondary = .data$ClassDetailed2, name = "Docs") |>
    dplyr::arrange(dplyr::desc(.data$Docs))

  clf_say_table(utils::head(pairs_, .n),
                paste0("Primary -> secondary (top ", min(.n, nrow(pairs_)), " of ", nrow(pairs_), ")"))
  cli::cli_text("")
  cli::cli_bullets(c(
    "*" = "{.strong Primary} is the training target. {.strong Secondary} is never trained on.",
    "*" = "The primary was assigned by manual review, so the direction is meaningful.",
    "*" = "Lenient scoring accepts either; the strict-lenient gap is the ambiguity cost."
  ))
  invisible(pairs_)
}

#' Fold balance for one task
#' @param .tab Prepared tibble.
#' @param .level One of ClassDetailed, ClassBroad, AmendType.
#' @return Invisibly the per-fold tibble.
clf_report_folds <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType")) {
  .level <- match.arg(.level)
  out_ <- clf_fold_overview(.tab, .level)
  cli::cli_h2("5. Fold balance -- {(.level)}")
  clf_say_table(out_)
  cli::cli_text("")
  cli::cli_alert_info(
    "Each fold is held out once. A class thin enough to vanish from a fold makes \\
     that fold's per-class recall undefined -- watch the smallest rows."
  )
  invisible(out_)
}

#' Per-fold counts for one task (tibble form; used by 03B as well)
#' @param .tab Prepared tibble.
#' @param .level One of ClassDetailed, ClassBroad, AmendType.
#' @return Tibble: Label, one column per fold, Total.
clf_fold_overview <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType")) {
  .level <- match.arg(.level)
  .tab |>
    dplyr::filter(!is.na(.data[[.level]])) |>
    dplyr::count(Label = .data[[.level]], .data$Fold) |>
    tidyr::pivot_wider(names_from = "Fold", names_prefix = "Fold", values_from = "n",
                       values_fill = 0L) |>
    dplyr::mutate(Total = as.integer(rowSums(dplyr::across(dplyr::starts_with("Fold"))))) |>
    dplyr::arrange(dplyr::desc(.data$Total))
}

#' Document length against the transformer context windows
#' @param .tab Prepared tibble.
#' @return Invisibly the summary tibble.
clf_report_length <- function(.tab) {
  words_ <- stringi::stri_count_regex(.tab$Text, "\\S+")
  out_ <- tibble::tibble(
    Statistic = c("Min", "25th pct", "Median", "Mean", "75th pct", "Max"),
    Words     = as.integer(round(c(min(words_), stats::quantile(words_, 0.25),
                                   stats::median(words_), mean(words_),
                                   stats::quantile(words_, 0.75), max(words_))))
  )
  cli::cli_h2("6. Document length")
  clf_say_table(out_)
  cli::cli_text("")
  over_ <- tibble::tibble(
    Window       = c("256 tokens (~200 words)", "512 tokens (~400 words)"),
    DocsOver     = c(sum(words_ > 200), sum(words_ > 400)),
    ShareOver    = clf_pct(c(mean(words_ > 200), mean(words_ > 400)))
  )
  clf_say_table(over_, "Documents exceeding the context window")
  cli::cli_text("")
  cli::cli_alert_info(
    "Most contracts overflow both windows, so the model reads the opening pages \\
     only. That is the bet the pipeline rests on: contract type is legible from \\
     the title and preamble."
  )

  # The short tail is the one that can hurt: a doc with a handful of words passed
  # the non-empty gate but carries no signal, and trains on noise.
  thin_ <- c(10L, 50L, 100L, 200L)
  short_ <- tibble::tibble(
    Under = paste0("< ", thin_, " words"),
    Docs  = purrr::map_int(thin_, \(.n) sum(words_ < .n)),
    Share = clf_pct(purrr::map_dbl(thin_, \(.n) mean(words_ < .n)))
  )
  clf_say_table(short_, "Short documents (parsing artifacts)")
  cli::cli_text("")
  cli::cli_alert_info(
    "These passed the non-empty gate but may carry no usable signal. A handful is \\
     noise to tolerate; hundreds would justify a minimum-length gate."
  )
  invisible(out_)
}

#' Missingness audit across the prepared columns
#' @param .tab Prepared tibble.
#' @return Invisibly the missingness tibble.
clf_report_missing <- function(.tab) {
  n_ <- nrow(.tab)
  na_ <- .tab |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Column", values_to = "nNA")
  empty_ <- .tab |>
    dplyr::summarise(dplyr::across(dplyr::where(is.character), ~ sum(.x == "", na.rm = TRUE))) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Column", values_to = "nEmpty")

  out_ <- na_ |>
    dplyr::left_join(empty_, by = dplyr::join_by(Column)) |>
    dplyr::mutate(
      nEmpty = dplyr::coalesce(.data$nEmpty, 0L),
      PctNA  = clf_pct(.data$nNA / n_)
    ) |>
    dplyr::select(Column, nNA, PctNA, nEmpty) |>
    dplyr::arrange(dplyr::desc(.data$nNA), dplyr::desc(.data$nEmpty))

  cli::cli_h2("7. Missing data")
  clf_say_table(out_)
  cli::cli_text("")
  cli::cli_bullets(c(
    "v" = "{.strong ClassDetailed2} NA is expected -- it means 'single-class doc'.",
    "v" = "{.strong DocDesc} nEmpty is expected -- docs with no filer title; the keyword docdesc model abstains on these.",
    "x" = "Anything else nonzero is a bug. ClassBroad / ClassDetailed / AmendType / Text / Fold should all be zero."
  ))
  invisible(out_)
}

#' The whole 03A report in one call
#'
#' Prints, in order: intake cascade, the three tasks, class distributions, dual
#' labels, fold balance, document length, missingness. This is the block to copy
#' out of the console when something needs checking.
#'
#' @param .tab Prepared tibble from clf_prepare_sample().
#' @return Invisibly .tab.
clf_report_sample <- function(.tab) {
  if (FALSE) .tab <- tab_prep
  cli::cli_h1("03A -- what goes into training")
  clf_report_intake(.tab)
  clf_report_tasks(.tab)
  clf_report_classes(.tab, "ClassDetailed")
  clf_report_classes(.tab, "ClassBroad")
  clf_report_classes(.tab, "AmendType")
  clf_report_duals(.tab)
  clf_report_folds(.tab, "ClassDetailed")
  clf_report_length(.tab)
  clf_report_missing(.tab)
  cli::cli_rule()
  invisible(.tab)
}


# 5. Report: results across runs ------------------------------------------
# Console printers for the 03B / 03C / 03D results sections. The computation
# lives in section 6 below; these only format it.

#' Leaderboard as a compact console table, one column per swept axis
#'
#' clf_leaderboard_show() carries ConfigName, which is ~80 characters and makes
#' the table unreadable in a console. This decomposes it back into the axes that
#' actually varied -- model, max_len, epochs, LR, class weights -- so the winning
#' recipe is legible at a glance. Axes constant across the whole leaderboard are
#' dropped and reported once underneath, since a column of identical values is
#' noise.
#'
#' @param .tab_overall Output of clf_load_overall().
#' @param .label_col Task to rank ("ClassDetailed", "ClassBroad", "AmendType").
#' @param .n Integer. Rows to show.
#' @return Invisibly the compact tibble.
clf_report_leaderboard <- function(.tab_overall, .label_col = "ClassDetailed", .n = 15L) {
  if (FALSE) {
    .tab_overall <- clf_load_overall(.lP$Runs$Bert)
    .label_col   <- "ClassDetailed"
    .n           <- 15L
  }
  axes_ <- .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col, !.data$Smoke) |>
    dplyr::summarise(
      Model   = dplyr::first(.data$Model),
      MaxLen  = dplyr::first(.data$MaxLen),
      Epochs  = dplyr::first(.data$Epochs),
      LR      = dplyr::first(.data$LR),
      Weights = dplyr::first(as.integer(as.logical(.data$ClassWeights))),
      .by = ConfigName
    )

  board_ <- .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col) |>
    clf_leaderboard() |>
    dplyr::select(ConfigName, nFolds, F1macro_mean, F1macro_sd, Acc_mean, Acc_sd) |>
    dplyr::left_join(axes_, by = dplyr::join_by(ConfigName)) |>
    dplyr::mutate(
      Rank     = dplyr::row_number(),
      Model    = clf_model_short(.model = .data$Model),
      LR       = formatC(.data$LR, format = "g"),
      Epochs   = as.integer(.data$Epochs),
      MaxLen   = as.integer(.data$MaxLen),
      MacroF1  = sprintf("%.3f +/- %.3f", .data$F1macro_mean, .data$F1macro_sd),
      Accuracy = sprintf("%.3f +/- %.3f", .data$Acc_mean, .data$Acc_sd)
    ) |>
    dplyr::select(Rank, Model, MaxLen, Epochs, LR, Weights, nFolds, MacroF1, Accuracy) |>
    head(.n)

  const_ <- board_ |>
    dplyr::select(Model, MaxLen, Epochs, LR, Weights) |>
    purrr::map_lgl(\(.c) dplyr::n_distinct(.c) == 1L)
  fixed_ <- names(const_)[const_]

  cli::cli_h2("Leaderboard -- {(.label_col)} (top {nrow(board_)})")
  board_ |> dplyr::select(-dplyr::all_of(fixed_)) |> clf_say_table()
  if (length(fixed_) > 0L) {
    held_ <- purrr::map_chr(fixed_, \(.a) paste0(.a, "=", board_[[.a]][[1]]))
    cli::cli_text("")
    cli::cli_alert_info("Constant across every row shown: {paste(held_, collapse = ', ')}")
  }
  cli::cli_text("")
  cli::cli_alert_info("Weights: 1 = class-weighted loss, 0 = unweighted.")
  invisible(board_)
}

#' Marginal effect of each swept axis on macro-F1, as one console table
#'
#' @param .tab_overall Output of clf_load_overall().
#' @param .label_col Task to analyse.
#' @param .axes Character vector of axis column names.
#' @return Invisibly the stacked effect tibble.
clf_report_effects <- function(.tab_overall, .label_col = "ClassDetailed",
                               .axes = c("Model", "MaxLen", "Epochs", "ClassWeights", "LR")) {
  present_ <- .axes[.axes %in% names(.tab_overall)]
  out_ <- purrr::map(present_, \(.axis) {
    clf_effect(.tab_overall, .axis, .label_col = .label_col) |>
      dplyr::rename(Level = 1) |>
      dplyr::mutate(Axis = .axis, Level = as.character(Level)) |>
      dplyr::relocate(Axis)
  }) |>
    purrr::list_rbind()

  cli::cli_h2("Marginal effect of each axis -- {(.label_col)}")
  out_ |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))) |>
    clf_say_table()
  cli::cli_text("")
  cli::cli_alert_info(
    "Each row averages over every other axis on the balanced grid. A gap smaller \\
     than the fold-to-fold sd on the leaderboard is not a real effect."
  )
  invisible(out_)
}

#' Headline scores as a console table
#' @param .tab_pred Pooled predictions.
#' @param .title Heading.
#' @return Invisibly the scores tibble.
clf_report_scores <- function(.tab_pred, .title = "Headline scores") {
  out_ <- clf_scores(.tab_pred)
  cli::cli_h2("{(.title)}")
  out_ |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))) |>
    clf_say_table()
  invisible(out_)
}

#' Per-class precision / recall / F1 as a console table
#' @param .tab_pred Pooled predictions.
#' @param .title Heading.
#' @return Invisibly the per-class tibble.
clf_report_perclass <- function(.tab_pred, .title = "Per-class scores") {
  out_ <- clf_perclass(.tab_pred)
  cli::cli_h2("{(.title)}")
  out_ |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))) |>
    clf_say_table()
  cli::cli_text("")
  cli::cli_alert_info(
    "Macro-F1 is the unweighted mean of the F1 column, so the thinnest classes \\
     move it as much as the largest ones."
  )
  invisible(out_)
}


# 6. Overview: load + leaderboard -----------------------------------------

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
    dplyr::mutate(Config = clf_config_label(.config_name = .data$ConfigName)) |>
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(Config, F1macro_mean), y = F1macro_mean)) +
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
