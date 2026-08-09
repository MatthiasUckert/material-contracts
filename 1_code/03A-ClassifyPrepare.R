# 03A-ClassifyPrepare: build the training sample, and the layer every method reports through --------------------------
#
# WHAT THIS FILE DOES
# Roughly 4.4k contracts carry a hand-assigned label. 03A turns that label list into ONE table --
# prepared.parquet -- holding, per document, its text, its labels for all three tasks, and a fold
# number. Nothing is trained here. The job is three decisions: which documents are eligible, what
# their labels are, and which fold each belongs to.
#
# THE THREE TASKS (all predicted from the same Text column)
#   ClassDetailed  12-class contract taxonomy      the headline task
#   ClassBroad      7-class roll-up of the above
#   AmendType       2 classes: Original vs Amended
# All three share the SAME folds, dealt stratified on ClassDetailed. Fold assignment lives here and
# not in the trainers because a comparison between methods scored on different splits measures the
# splits, not the methods.
#
# SINGLE-LABEL, ALWAYS
# A minority of documents carry a second valid category (ClassDetailed2). Training uses the primary
# only. The second label is never a target; it exists so lenient scoring can ask whether a prediction
# counted as wrong was in fact the document's other valid answer.
#
# WHAT ELSE LIVES HERE, AND WHY
# Three things beyond sample construction, each shared by every downstream method:
#   * The TAXONOMY VOCABULARY (section 1). Registered with the figure design layer, so a category
#     occupies the same position and carries the same short label in every figure of every document.
#   * The SCORING LAYER (sections 7-8). One implementation of per-class metrics, abstention handling
#     and confusion, so the transformer, the keyword miner, the LLM and the router are compared by
#     identical code rather than by four similar implementations that disagree at the third decimal.
#   * The RESULTS REPORTERS (sections 5-6). Leaderboards and marginal effects, likewise shared.
#
# WHAT IS NOT HERE
# The trainers: bert_* is 03B, kw_* is 03C, llm_* is 03D, the router is 03E, application is 03F.
# The look of any figure or table: that is _Commons/_Plots.R and _Commons/_Tables.R, sourced ahead
# of this file. Nothing below sets a colour, a font or a height.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation.

if (FALSE) {
  .tab_input  <- fils_class_sample
  .path_data  <- .lP$Output$Prepared
  .runs_roots <- c(.lP$Runs$Bert, .lP$Runs$Kw)
}


# 1. Vocabulary ------------------------------------------------------------------------------------------------------
# The canonical order and display labels for the three tasks, registered with the design layer at
# source time so every figure downstream inherits them without restating anything.
#
# ORDER IS TAXONOMIC, NOT BY SIZE. Ordering categories by frequency would put a different category
# first in every figure, which is precisely what makes two panels incomparable. Taxonomic order also
# earns its keep analytically: siblings sit adjacent, so in a confusion matrix the mass that stays
# inside a broad parent lands next to the diagonal and the mass that crosses one is visible as a jump
# away from it. That distinction is the central empirical question about this taxonomy. "Other" is
# last everywhere, because a residual category ranked among substantive ones invites reading it as
# one.
#
# SHORT LABELS EXIST BECAUSE OF WIDTH. The longest category name is forty-one characters. Figure
# width is fixed, so a label that does not fit is shortened rather than accommodated. The full names
# remain the values in the data and in every table; the short forms are display only.

.clf_class_detailed <- c(
  "Financial Instruments: Credit",
  "Financial Instruments: Equity",
  "Purchases and Sales: Assets",
  "Customer / Supplier",
  "R&D",
  "Employment: Compensation",
  "Employment: Legal",
  "Business Structure: Peer Agreements",
  "Business Structure: Investment and Merger",
  "Leases",
  "Licenses",
  "Other"
)

.clf_class_detailed_short <- c(
  "Fin: Credit",
  "Fin: Equity",
  "P&S: Assets",
  "Cust./Supplier",
  "R&D",
  "Empl: Compensation",
  "Empl: Legal",
  "Bus: Peer Agr.",
  "Bus: Inv. & Merger",
  "Leases",
  "Licenses",
  "Other"
)

.clf_class_broad <- c(
  "Financial Instruments",
  "Purchases and Sales",
  "Employment",
  "Business Structure",
  "Leases",
  "Licenses",
  "Other"
)

.clf_class_broad_short <- c(
  "Fin. Instruments",
  "Purch. & Sales",
  "Employment",
  "Bus. Structure",
  "Leases",
  "Licenses",
  "Other"
)

# Twelve categories exceed what any categorical palette separates, so no colours are registered for
# the detailed taxonomy: position identifies a category there, and a twelve-colour legend would add
# an encoding carrying no information. The broad taxonomy takes the categorical palette. The
# amendment task takes the two greys the descriptives have always used for it.
plot_register_levels(
  .key     = "ClassDetailed",
  .levels  = .clf_class_detailed,
  .short   = .clf_class_detailed_short,
  .colours = NULL
)

plot_register_levels(
  .key     = "ClassBroad",
  .levels  = .clf_class_broad,
  .short   = .clf_class_broad_short,
  .colours = plot_pal_cat(length(.clf_class_broad))
)

plot_register_levels(
  .key     = "AmendType",
  .levels  = c("Original", "Amended"),
  .short   = NULL,
  .colours = plot_pal_bin()
)

#' Check the registered vocabulary against the labels actually present
#'
#' The registry aborts on an unregistered value, which is the behaviour wanted at draw time but a
#' late and confusing place to discover that the label spine gained a category. This runs the check
#' early and reports both directions: values in the data that nothing knows about, and registered
#' categories that no document carries. The second is not an error -- a rare category can legitimately
#' be absent from a subsample -- but a registered category with zero documents in the full sample is
#' a taxonomy that has drifted from its data.
#'
#' @param .tab Prepared tibble.
#' @return Invisibly a tibble: one row per task, with counts of each kind of mismatch.
clf_report_vocabulary <- function(.tab) {
  if (FALSE) .tab <- tab_prep

  one_ <- function(.key) {
    have_    <- setdiff(unique(as.character(.tab[[.key]])), NA_character_)
    known_   <- plot_levels(.key)
    unknown_ <- setdiff(have_, known_)
    unused_  <- setdiff(known_, have_)
    tibble::tibble(
      Task       = .key,
      Registered = length(known_),
      InSample   = length(have_),
      Unknown    = length(unknown_),
      Unused     = length(unused_),
      Detail     = paste(c(
        if (length(unknown_) > 0L) paste0("unknown: ", paste(unknown_, collapse = "; ")),
        if (length(unused_) > 0L)  paste0("unused: ",  paste(unused_,  collapse = "; "))
      ), collapse = " | ")
    )
  }

  out_ <- purrr::map(c("ClassDetailed", "ClassBroad", "AmendType"), one_) |> purrr::list_rbind()

  cli::cli_h2("Vocabulary against the sample")
  tbl_say(out_)
  cli::cli_text("")
  if (sum(out_$Unknown) > 0L) {
    cli::cli_alert_danger(
      "A category in the data is not registered. Every figure keyed on that task will abort until it \\
       is added to the vocabulary block at the top of this file."
    )
  } else {
    cli::cli_alert_success("Every label in the sample is registered.")
  }
  invisible(out_)
}


# 2. Disk cache ------------------------------------------------------------------------------------------------------
# Memoisation for expensive report artifacts. A chunk always runs, but the cost behind it is paid
# once.
#
# WARNING: the key is a NAME, not a hash of the data behind it. If the label spine changes, a warm
# cache serves pre-change results with no error and no visible difference from a correct run. Delete
# 2_output/_cache/ whenever the sample changes.

#' Compute-once disk cache for a report artifact
#'
#' Returns the cached value for .key when present and .overwrite is FALSE; otherwise evaluates .expr,
#' stores it, and returns it. .expr is lazily evaluated, so on a hit the computation never runs. Flip
#' every cache at once with options(clf.cache.overwrite = TRUE).
#'
#' @param .key Character. Cache name, sanitised into a file name.
#' @param .expr Expression evaluated only on a miss, untouched on a hit.
#' @param .overwrite Logical. Recompute and overwrite even when cached.
#' @param .dir Character. Cache directory, created if absent.
#' @return The cached or freshly computed value.
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


# 3. Sample construction ---------------------------------------------------------------------------------------------

#' Read one parsed document's full text from its per-document parquet
#'
#' Returns NA on any failure -- missing file, missing column, empty content -- so the caller can tally
#' misses at a reported gate instead of aborting a run of several thousand reads on one bad file.
#'
#' @param .path Character. Path to the per-document parquet.
#' @return Character scalar of document text, or NA.
clf_read_text <- function(.path) {
  if (FALSE) .path <- fils_contract$Path[[1]]
  tab_ <- tryCatch(arrow::read_parquet(.path), error = function(e) NULL)
  if (is.null(tab_) || !"TextRaw" %in% names(tab_)) return(NA_character_)
  txt_ <- tab_[["TextRaw"]]
  if (length(txt_) == 0L) return(NA_character_)
  paste(txt_, collapse = "\n")
}

#' Build the prepared training sample, reporting every document that drops out
#'
#' This is the only place a document can leave the sample, and it narrates each departure. Four
#' gates, in order:
#'
#'   1. HAS A CLASS LABEL. Unresolved documents carry NA and cannot train anything.
#'   2. HAS A FILE PATH. The label list joins to the parsed-contract tree on DocID; no match means no
#'      text.
#'   3. FILE EXISTS ON DISK. Guards against a stale path cache.
#'   4. TEXT IS NON-EMPTY. A parquet that reads but yields nothing is useless.
#'
#' Survivors receive ClassBroad / ClassDetailed / ClassDetailed2 / AmendType, a LabelRound recode,
#' DocDesc and DocName for the keyword track, and a deterministic stratified fold. The intake cascade
#' is attached as attr(out, "Intake").
#'
#' @param .tab_input Tibble. Requires DocID, Path, Level1, DocClassFinal1; DocClassFinal2, AmendType,
#'   Provenance, DocDesc and DocName are used when present.
#' @param .round Character or NULL. Keep only this LabelRound; NULL keeps all.
#' @param .k Integer. Number of folds.
#' @param .seed Integer. RNG seed for the fold deal.
#' @return Tibble: DocID, Text, DocDesc, DocName, ClassBroad, ClassDetailed, ClassDetailed2,
#'   AmendType, LabelRound, Fold. Carries attr "Intake".
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

  # Optional columns: materialise as NA so the select below is stable regardless of what the export
  # happened to carry.
  for (col_ in c("DocClassFinal2", "AmendType", "DocDesc", "DocName")) {
    if (!col_ %in% names(.tab_input)) .tab_input[[col_]] <- NA_character_
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

  # Gate 0: optional round restriction. Not a data-quality drop, so it is reported separately.
  n_read_ <- nrow(tab_)
  if (!is.null(.round)) {
    tab_ <- tab_ |> dplyr::filter(.data$LabelRound == .round)
    cli::cli_alert_info("Restricted to {(.round)}: {nrow(tab_)} of {n_read_} rows")
  }
  n_start_ <- nrow(tab_)

  # Gate 1: a usable class label.
  tab_   <- tab_ |> dplyr::filter(!is.na(.data$ClassBroad), !is.na(.data$ClassDetailed))
  n_lab_ <- nrow(tab_)

  # Gate 2: a path from the contract-file join.
  tab_    <- tab_ |> dplyr::filter(!is.na(.data$Path))
  n_path_ <- nrow(tab_)

  # Gate 3: that path resolves on disk.
  tab_    <- tab_ |> dplyr::filter(fs::file_exists(.data$Path))
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

  # Deal the folds. Stratified on ClassDetailed and reused by every task, so the rarest detailed
  # category is the binding constraint on how thin a fold can become.
  thin_ <- tab_ |> dplyr::count(.data$ClassDetailed) |> dplyr::filter(.data$n < .k)
  if (nrow(thin_) > 0L) {
    cli::cli_alert_warning(
      "Categories with fewer than {(.k)} docs (a fold may lack them): \\
       {paste(thin_$ClassDetailed, collapse = ', ')}"
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
#' The "Intake" attribute does not survive the parquet round trip; it is a session artifact for the
#' 03A report only, which is why clf_report_intake() degrades gracefully when it is absent.
#'
#' @param .tab Prepared tibble from clf_prepare_sample().
#' @param .path_out Character. Output parquet path.
#' @return Invisibly the path written.
clf_write_prepared <- function(.tab, .path_out) {
  if (FALSE) {
    .tab      <- tab_prep
    .path_out <- .lP$Output$Prepared
  }
  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(.tab, .path_out)
  cli::cli_alert_success("Wrote {(.path_out)} ({nrow(.tab)} docs)")
  invisible(.path_out)
}


# 4. Report: what goes into training -----------------------------------------------------------------------------------
# Each clf_report_* prints one block and returns its tibble invisibly, so the numbers stay available
# after they have been displayed. clf_report_sample() runs them all.

#' Intake cascade: how the label list became the training sample
#'
#' @param .tab Prepared tibble, which must carry attr "Intake".
#' @return Invisibly the intake tibble, or NULL when the attribute is absent.
clf_report_intake <- function(.tab) {
  if (FALSE) .tab <- tab_prep
  intake_ <- attr(.tab, "Intake")
  if (is.null(intake_)) {
    cli::cli_alert_warning("No intake record (attribute lost -- was this read back from parquet?)")
    return(invisible(NULL))
  }
  cli::cli_h2("1. Intake: which documents made it in")
  intake_ |>
    dplyr::mutate(
      Kept    = tbl_pct(.data$Docs / max(.data$Docs)),
      Dropped = as.integer(.data$Dropped)
    ) |>
    tbl_say()
  cli::cli_text("")
  cli::cli_alert_info(
    "Every drop is accounted for above. A nonzero drop at {.strong File exists on disk} or \\
     {.strong Has a contract file path} is a join or path problem, not a labelling one."
  )
  invisible(intake_)
}

#' The three tasks: what each one actually trains on
#'
#' @param .tab Prepared tibble.
#' @return Invisibly the task summary tibble.
clf_report_tasks <- function(.tab) {
  if (FALSE) .tab <- arrow::read_parquet(.lP$Output$Prepared)

  one_ <- function(.name, .col) {
    v_     <- .tab[[.col]]
    ok_    <- v_[!is.na(v_)]
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
  tbl_say(out_)
  cli::cli_text("")
  cli::cli_bullets(c(
    "*" = "All three are predicted from the same {.strong Text} column.",
    "*" = "All three share the same folds, dealt stratified on {.strong ClassDetailed}.",
    "*" = "{.strong Unlabelled} docs are dropped by the trainer for that task only.",
    "*" = "The smallest category drives macro-F1, which weights every category equally."
  ))
  invisible(out_)
}

#' Per-category distribution for one task, split by label round
#'
#' @param .tab Prepared tibble.
#' @param .level Character. One of ClassDetailed, ClassBroad, AmendType.
#' @return Invisibly the distribution tibble.
clf_report_classes <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType")) {
  if (FALSE) {
    .tab   <- tab_prep
    .level <- "ClassDetailed"
  }
  .level <- match.arg(.level)
  n_ <- sum(!is.na(.tab[[.level]]))

  out_ <- .tab |>
    dplyr::filter(!is.na(.data[[.level]])) |>
    dplyr::count(Class = .data[[.level]], .data$LabelRound) |>
    tidyr::pivot_wider(names_from = "LabelRound", values_from = "n", values_fill = 0L) |>
    dplyr::mutate(N = as.integer(rowSums(dplyr::across(dplyr::where(is.numeric))))) |>
    dplyr::arrange(dplyr::desc(.data$N)) |>
    dplyr::mutate(Pct = tbl_pct(.data$N / n_)) |>
    dplyr::relocate(Class, N, Pct, dplyr::any_of(c("Round1", "Round2")))

  cli::cli_h2("3. Category distribution -- {(.level)}")
  tbl_say(out_)
  invisible(out_)
}

#' Category counts and shares for one task
#'
#' The tibble form behind the distribution figures. Kept separate from the reporter so the numbers
#' can be plotted and printed from one computation rather than two that could diverge.
#'
#' @param .tab Prepared tibble.
#' @param .level Character. One of ClassDetailed, ClassBroad, AmendType.
#' @param .include_na Logical. Fold NA into an explicit "(unlabeled)" category.
#' @return Tibble: Class, N, Pct, descending by N.
clf_class_distribution <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType"),
                                   .include_na = FALSE) {
  if (FALSE) {
    .tab        <- tab_prep
    .level      <- "ClassDetailed"
    .include_na <- FALSE
  }
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

#' Category counts by label round, in long form
#'
#' Feeds the composition figure. Round1 is the automated carry-forward and Round2 the manual pass, so
#' the split shows which categories the current taxonomy actually required new labelling work for.
#'
#' @param .tab Prepared tibble.
#' @param .level Character. One of ClassDetailed, ClassBroad, AmendType.
#' @return Tibble: Class, LabelRound, N.
clf_class_by_round <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType")) {
  if (FALSE) {
    .tab   <- tab_prep
    .level <- "ClassDetailed"
  }
  .level <- match.arg(.level)
  .tab |>
    dplyr::filter(!is.na(.data[[.level]]), !is.na(.data$LabelRound)) |>
    dplyr::count(Class = .data[[.level]], .data$LabelRound, name = "N")
}

#' Dual-class documents: headline count and the primary-to-secondary pairs
#'
#' @param .tab Prepared tibble.
#' @param .n Integer. Pairs to show.
#' @return Invisibly the pairs tibble, or NULL when no document is dual-classified.
clf_report_duals <- function(.tab, .n = 12L) {
  if (FALSE) {
    .tab <- tab_prep
    .n   <- 12L
  }
  dual_ <- .tab |> dplyr::filter(!is.na(.data$ClassDetailed2))

  cli::cli_h2("4. Dual-class documents")
  cli::cli_alert_info(
    "{nrow(dual_)} of {nrow(.tab)} docs ({tbl_pct(nrow(dual_) / nrow(.tab))}) carry a second valid category."
  )
  if (nrow(dual_) == 0L) return(invisible(NULL))

  by_round_ <- dual_ |> dplyr::count(.data$LabelRound, name = "Docs")
  tbl_say(by_round_, "By label round")
  cli::cli_text("")
  cli::cli_alert_info("Round1 is automated and emits one label, so duals should be Round2 only.")

  pairs_ <- dual_ |>
    dplyr::count(Primary = .data$ClassDetailed, Secondary = .data$ClassDetailed2, name = "Docs") |>
    dplyr::arrange(dplyr::desc(.data$Docs))

  tbl_say(utils::head(pairs_, .n),
          paste0("Primary to secondary (top ", min(.n, nrow(pairs_)), " of ", nrow(pairs_), ")"))
  cli::cli_text("")
  cli::cli_bullets(c(
    "*" = "{.strong Primary} is the training target. {.strong Secondary} is never trained on.",
    "*" = "The primary was assigned by manual review, so the direction is meaningful.",
    "*" = "Lenient scoring accepts either; the strict-lenient gap is the ambiguity cost."
  ))
  invisible(pairs_)
}

#' Fold balance for one task
#'
#' @param .tab Prepared tibble.
#' @param .level Character. One of ClassDetailed, ClassBroad, AmendType.
#' @return Invisibly the per-fold tibble.
clf_report_folds <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType")) {
  if (FALSE) {
    .tab   <- tab_prep
    .level <- "ClassDetailed"
  }
  .level <- match.arg(.level)
  out_ <- clf_fold_overview(.tab, .level)
  cli::cli_h2("5. Fold balance -- {(.level)}")
  tbl_say(out_)
  cli::cli_text("")
  cli::cli_alert_info(
    "Each fold is held out once. A category thin enough to vanish from a fold makes that fold's \\
     per-category recall undefined -- watch the smallest rows."
  )
  invisible(out_)
}

#' Per-fold counts for one task
#'
#' @param .tab Prepared tibble.
#' @param .level Character. One of ClassDetailed, ClassBroad, AmendType.
#' @return Tibble: Label, one column per fold, Total.
clf_fold_overview <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType")) {
  if (FALSE) {
    .tab   <- tab_prep
    .level <- "ClassDetailed"
  }
  .level <- match.arg(.level)
  .tab |>
    dplyr::filter(!is.na(.data[[.level]])) |>
    dplyr::count(Label = .data[[.level]], .data$Fold) |>
    tidyr::pivot_wider(names_from = "Fold", names_prefix = "Fold", values_from = "n",
                       values_fill = 0L) |>
    dplyr::mutate(Total = as.integer(rowSums(dplyr::across(dplyr::starts_with("Fold"))))) |>
    dplyr::arrange(dplyr::desc(.data$Total))
}

#' Per-fold counts in long form, as a share of each category
#'
#' Feeds the fold-balance figure. Shares rather than counts, because the question a reader has is
#' whether a category is spread evenly across folds, and at counts that question is invisible: a
#' large category's fold-to-fold variation dwarfs a small category's entire presence.
#'
#' @param .tab Prepared tibble.
#' @param .level Character. One of ClassDetailed, ClassBroad, AmendType.
#' @return Tibble: Class, Fold, N, Share.
clf_fold_shares <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType")) {
  if (FALSE) {
    .tab   <- tab_prep
    .level <- "ClassDetailed"
  }
  .level <- match.arg(.level)
  .tab |>
    dplyr::filter(!is.na(.data[[.level]])) |>
    dplyr::count(Class = .data[[.level]], Fold = .data$Fold, name = "N") |>
    tidyr::complete(Class, Fold, fill = list(N = 0L)) |>
    dplyr::mutate(Share = .data$N / sum(.data$N), .by = Class) |>
    dplyr::mutate(Fold = paste0("Fold ", .data$Fold))
}

#' Document length against the transformer context windows
#'
#' @param .tab Prepared tibble.
#' @return Invisibly the length summary tibble.
clf_report_length <- function(.tab) {
  if (FALSE) .tab <- tab_prep
  words_ <- stringi::stri_count_regex(.tab$Text, "\\S+")
  out_ <- tibble::tibble(
    Statistic = c("Min", "25th pct", "Median", "Mean", "75th pct", "Max"),
    Words     = as.integer(round(c(min(words_), stats::quantile(words_, 0.25),
                                   stats::median(words_), mean(words_),
                                   stats::quantile(words_, 0.75), max(words_))))
  )
  cli::cli_h2("6. Document length")
  tbl_say(out_)
  cli::cli_text("")

  over_ <- tibble::tibble(
    Window    = c("256 tokens (~200 words)", "512 tokens (~400 words)"),
    DocsOver  = c(sum(words_ > 200), sum(words_ > 400)),
    ShareOver = tbl_pct(c(mean(words_ > 200), mean(words_ > 400)))
  )
  tbl_say(over_, "Documents exceeding the context window")
  cli::cli_text("")
  cli::cli_alert_info(
    "Most contracts overflow both windows, so the model reads the opening pages only. That is the \\
     premise the pipeline rests on: contract type is legible from the title and preamble."
  )

  # The short tail is the one that can hurt: a document with a handful of words passed the non-empty
  # gate but carries no signal, and trains on noise.
  thin_  <- c(10L, 50L, 100L, 200L)
  short_ <- tibble::tibble(
    Under = paste0("< ", thin_, " words"),
    Docs  = purrr::map_int(thin_, \(.n) sum(words_ < .n)),
    Share = tbl_pct(purrr::map_dbl(thin_, \(.n) mean(words_ < .n)))
  )
  tbl_say(short_, "Short documents (parsing artifacts)")
  cli::cli_text("")
  cli::cli_alert_info(
    "These passed the non-empty gate but may carry no usable signal. A handful is noise to tolerate; \\
     hundreds would justify a minimum-length gate."
  )
  invisible(out_)
}

#' Missingness audit across the prepared columns
#'
#' @param .tab Prepared tibble.
#' @return Invisibly the missingness tibble.
clf_report_missing <- function(.tab) {
  if (FALSE) .tab <- tab_prep
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
      PctNA  = tbl_pct(.data$nNA / n_)
    ) |>
    dplyr::select(Column, nNA, PctNA, nEmpty) |>
    dplyr::arrange(dplyr::desc(.data$nNA), dplyr::desc(.data$nEmpty))

  cli::cli_h2("7. Missing data")
  tbl_say(out_)
  cli::cli_text("")
  cli::cli_bullets(c(
    "v" = "{.strong ClassDetailed2} NA is expected -- it means a single-category document.",
    "v" = "{.strong DocDesc} nEmpty is expected -- no filer title; the keyword docdesc model abstains.",
    "x" = "Anything else nonzero is a bug. ClassBroad, ClassDetailed, AmendType, Text and Fold should be zero."
  ))
  invisible(out_)
}

#' The whole 03A report in one call
#'
#' Prints, in order: intake cascade, the three tasks, category distributions, dual labels, fold
#' balance, document length, missingness. This is the block to copy out of the console when something
#' needs checking.
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


# 5. Report: results across runs ---------------------------------------------------------------------------------------
# Console printers for the results sections of 03B, 03C, 03D and 03E. The computation lives in
# section 6; these only format it.

#' Leaderboard as a compact console table, one column per swept axis
#'
#' A ConfigName is roughly eighty characters and makes a console table unreadable. This decomposes it
#' back into the axes that actually varied, so the winning recipe is legible at a glance. Axes
#' constant across the whole leaderboard are dropped and reported once underneath, since a column of
#' identical values is noise.
#'
#' @param .tab_overall Output of clf_load_overall().
#' @param .label_col Character. Task to rank.
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
    utils::head(.n)

  const_ <- board_ |>
    dplyr::select(Model, MaxLen, Epochs, LR, Weights) |>
    purrr::map_lgl(\(.c) dplyr::n_distinct(.c) == 1L)
  fixed_ <- names(const_)[const_]

  cli::cli_h2("Leaderboard -- {(.label_col)} (top {nrow(board_)})")
  board_ |> dplyr::select(-dplyr::all_of(fixed_)) |> tbl_say()
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
#' @param .label_col Character. Task to analyse.
#' @param .axes Character vector of axis column names.
#' @return Invisibly the stacked effect tibble.
clf_report_effects <- function(.tab_overall, .label_col = "ClassDetailed",
                               .axes = c("Model", "MaxLen", "Epochs", "ClassWeights", "LR")) {
  if (FALSE) {
    .tab_overall <- clf_load_overall(.lP$Runs$Bert)
    .label_col   <- "ClassDetailed"
    .axes        <- c("Model", "MaxLen", "Epochs", "ClassWeights", "LR")
  }
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
    tbl_say()
  cli::cli_text("")
  cli::cli_alert_info(
    "Each row averages over every other axis on the balanced grid. A gap smaller than the \\
     fold-to-fold sd on the leaderboard is not a real effect."
  )
  invisible(out_)
}

#' Headline scores as a console table
#'
#' @param .tab_pred Pooled predictions.
#' @param .title Character. Heading.
#' @return Invisibly the scores tibble.
clf_report_scores <- function(.tab_pred, .title = "Headline scores") {
  if (FALSE) {
    .tab_pred <- pred_det
    .title    <- "Headline scores"
  }
  out_ <- clf_scores(.tab_pred)
  cli::cli_h2("{(.title)}")
  out_ |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))) |>
    tbl_say()
  invisible(out_)
}

#' Per-category precision, recall and F1 as a console table
#'
#' @param .tab_pred Pooled predictions.
#' @param .title Character. Heading.
#' @return Invisibly the per-category tibble.
clf_report_perclass <- function(.tab_pred, .title = "Per-category scores") {
  if (FALSE) {
    .tab_pred <- pred_det
    .title    <- "Per-category scores"
  }
  out_ <- clf_perclass(.tab_pred)
  cli::cli_h2("{(.title)}")
  out_ |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3))) |>
    tbl_say()
  cli::cli_text("")
  cli::cli_alert_info(
    "Macro-F1 is the unweighted mean of the F1 column, so the thinnest categories move it as much \\
     as the largest ones."
  )
  invisible(out_)
}


# 6. Naming and loading ------------------------------------------------------------------------------------------------
# Run identifiers are built to be unique and machine-parseable, which makes them roughly eighty
# characters long. That is fine on disk and unusable on a figure axis or in a console table. These
# two functions compress an identifier to the parts that actually vary within a study. They sit
# alongside the level registry rather than inside it because a run identifier is derived, not looked
# up: the sweep can add a configuration at any time and no registration should be needed for it.

#' Short display name for a pre-trained model
#'
#' Drops the organisation prefix and the size or casing suffix that every checkpoint in a family
#' shares. Accepts either the hub form ("nlpaueb/legal-bert-base-uncased") or the slugged form used
#' inside run identifiers ("nlpaueb-legal-bert-base-uncased").
#'
#' @param .model Character vector of model identifiers.
#' @return Character vector of short names, e.g. "legal-bert", "roberta", "longformer".
clf_model_short <- function(.model) {
  if (FALSE) .model <- c("nlpaueb/legal-bert-base-uncased", "roberta-base")
  out_ <- sub("^.*/", "", .model)                                          # hub form: drop the organisation
  out_ <- sub("^(nlpaueb|allenai|google|facebook|microsoft)-", "", out_)    # slugged form: same job
  out_ <- sub("-base-uncased$|-base-cased$|-base-4096$|-base$", "", out_)   # shared family suffixes
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

#' Bind all per-fold overall-metrics rows from one or more runs trees
#'
#' Accepts a vector of runs roots so a single call can pool transformer and keyword runs for a
#' head-to-head. Coverage is written only by methods that can abstain; a method that always predicts
#' has an effective coverage of one, so the column is materialised uniformly here and no downstream
#' bind has to branch on method.
#'
#' @param .runs_roots Character vector of one or more runs directories.
#' @return Tibble of per-configuration-per-fold overall metrics.
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

#' Leaderboard: mean and sd across folds, one row per configuration
#'
#' Keyed on ConfigName, which already encodes every axis as a string, so the same function ranks
#' transformer and keyword configurations without knowing that their axes differ.
#'
#' @param .tab_overall Output of clf_load_overall().
#' @return One row per ConfigName, descending by mean macro-F1.
clf_leaderboard <- function(.tab_overall) {
  if (FALSE) .tab_overall <- clf_load_overall(.lP$Runs$Bert)
  .tab_overall |>
    dplyr::filter(!.data$Smoke) |>
    dplyr::summarise(
      nFolds        = dplyr::n(),
      Acc_mean      = mean(.data$Accuracy),    Acc_sd      = stats::sd(.data$Accuracy),
      F1macro_mean  = mean(.data$F1_macro),    F1macro_sd  = stats::sd(.data$F1_macro),
      F1weight_mean = mean(.data$F1_weighted), F1weight_sd = stats::sd(.data$F1_weighted),
      Cov_mean      = mean(.data$Coverage),
      .by = c(ConfigName, Model, LabelCol, TextCol, Seed)
    ) |>
    dplyr::arrange(dplyr::desc(.data$F1macro_mean))
}

#' Leaderboard formatted for reading
#'
#' @param .tab_overall Output of clf_load_overall().
#' @param .label_col Character or NULL. Restrict to one task.
#' @param .n Integer. Rows to show.
#' @return Formatted tibble.
clf_leaderboard_show <- function(.tab_overall, .label_col = NULL, .n = 20L) {
  if (FALSE) {
    .tab_overall <- clf_load_overall(.lP$Runs$Bert)
    .label_col   <- "ClassDetailed"
    .n           <- 20L
  }
  tab_ <- if (is.null(.label_col)) .tab_overall else {
    dplyr::filter(.tab_overall, .data$LabelCol == .label_col)
  }
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
    utils::head(.n)
}

#' Marginal effect of one sweep axis on macro-F1, averaging over all others
#'
#' Generic in the axis, so it serves the transformer sweep (model, length, epochs, LR, weighting) and
#' the keyword sweep (source, stopwords, top-k) without change. Rows where the axis is NA -- a length
#' column on a keyword row in a pooled table -- are dropped first. The grid is balanced, so the
#' result is a fair marginal mean.
#'
#' @param .tab_overall Output of clf_load_overall().
#' @param .axis Character. Column name to group by.
#' @param .label_col Character or NULL. Restrict to one task first, which is recommended, since tasks
#'   differ in difficulty and pooling them averages that difference into the axis.
#' @return Tibble of marginal means, descending by macro-F1.
clf_effect <- function(.tab_overall, .axis, .label_col = NULL) {
  if (FALSE) {
    .tab_overall <- clf_load_overall(.lP$Runs$Bert)
    .axis        <- "Model"
    .label_col   <- "ClassDetailed"
  }
  tab_ <- if (is.null(.label_col)) .tab_overall else {
    dplyr::filter(.tab_overall, .data$LabelCol == .label_col)
  }
  tab_ |>
    dplyr::filter(!.data$Smoke, !is.na(.data[[.axis]])) |>
    dplyr::summarise(
      nRuns        = dplyr::n(),
      F1macro_mean = mean(.data$F1_macro),
      F1macro_sd   = stats::sd(.data$F1_macro),
      Acc_mean     = mean(.data$Accuracy),
      .by = dplyr::all_of(.axis)
    ) |>
    dplyr::arrange(dplyr::desc(.data$F1macro_mean))
}


# 7. Pooled out-of-fold predictions -------------------------------------------------------------------------------------

#' Pool out-of-fold predictions for one configuration across all folds and roots
#'
#' Each document is predicted once, by a model that did not train on it, which is the honest
#' per-category metric cross-validation can give. Accepts a vector of roots so the router can pull a
#' transformer configuration and a keyword configuration from their separate trees.
#'
#' @param .runs_roots Character vector of one or more runs directories.
#' @param .config_name Character. ConfigName from the leaderboard.
#' @return Pooled predictions tibble: DocID, TrueLabel, PredLabel, Score and further columns.
clf_pool_predictions <- function(.runs_roots, .config_name) {
  if (FALSE) {
    .runs_roots  <- c(.lP$Runs$Bert, .lP$Runs$Kw)
    .config_name <- best_
  }
  paths_ <- .runs_roots |>
    purrr::map(\(.r) fs::dir_ls(.r, recurse = TRUE, glob = "*predictions.parquet")) |>
    purrr::list_c()
  paths_ <- paths_[!grepl("_smoke", paths_)]
  purrr::map(paths_, arrow::read_parquet) |>
    purrr::list_rbind() |>
    dplyr::filter(.data$ConfigName == .config_name)
}

#' Attach the dual-class second label to pooled predictions
#'
#' @param .tab_pred Pooled predictions, which must contain DocID.
#' @param .tab_prep Prepared sample, which must contain DocID and ClassDetailed2.
#' @return .tab_pred with ClassDetailed2 joined on.
clf_add_second_label <- function(.tab_pred, .tab_prep) {
  if (FALSE) {
    .tab_pred <- pred_det
    .tab_prep <- tab_prep
  }
  .tab_pred |>
    dplyr::inner_join(
      .tab_prep |> dplyr::select(DocID, ClassDetailed2),
      by = dplyr::join_by(DocID)
    )
}

#' Attach LabelRound to pooled predictions
#'
#' @param .tab_pred Pooled predictions, which must contain DocID.
#' @param .tab_prep Prepared sample, which must contain DocID and LabelRound.
#' @return .tab_pred with LabelRound joined on.
clf_add_labelround <- function(.tab_pred, .tab_prep) {
  if (FALSE) {
    .tab_pred <- pred_det
    .tab_prep <- tab_prep
  }
  .tab_pred |>
    dplyr::inner_join(
      .tab_prep |> dplyr::select(DocID, LabelRound),
      by = dplyr::join_by(DocID)
    )
}

#' Roll detailed predictions up to the broad taxonomy
#'
#' Maps both true and predicted detailed labels to their broad parent, using the mapping the prepared
#' sample itself carries, so the result can be scored at the broad level. Deriving the broad result
#' rather than estimating it separately is what keeps the two internally consistent: a broad score
#' computed independently could contradict the detailed score it is supposedly a roll-up of.
#'
#' @param .tab_pred Pooled detailed predictions: DocID, TrueLabel, PredLabel.
#' @param .tab_prep Prepared sample, which must contain ClassDetailed and ClassBroad.
#' @return Predictions tibble with TrueLabel and PredLabel at the broad level.
clf_rollup_to_broad <- function(.tab_pred, .tab_prep) {
  if (FALSE) {
    .tab_pred <- pred_det
    .tab_prep <- tab_prep
  }
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


# 8. Unified scoring ---------------------------------------------------------------------------------------------------
# One scoring layer for every method. The .none argument names the abstention sentinel. A method that
# always predicts never carries it, so the setdiff and coverage terms are no-ops and its numbers are
# unaffected. A method that can abstain has the sentinel dropped from the class set and from the
# macro average, and Coverage reports the share it committed on. An abstained document is a false
# negative for its true category and a false positive for nothing, so abstaining costs recall and
# protects precision -- which is the semantics a precision-first keyword arm needs.

#' Per-row correctness, strict or lenient
#'
#' Strict: the prediction matches the primary label. Lenient: it matches either the primary or the
#' dual-class second label where one exists.
#'
#' @param .tab_pred Pooled predictions.
#' @param .lenient Logical. Accept the second label as correct.
#' @return Logical vector, one element per row.
#' @keywords internal
clf_correct_vec <- function(.tab_pred, .lenient) {
  if (FALSE) {
    .tab_pred <- pred_det
    .lenient  <- FALSE
  }
  if (.lenient && "ClassDetailed2" %in% names(.tab_pred)) {
    (.tab_pred$PredLabel == .tab_pred$TrueLabel) |
      (!is.na(.tab_pred$ClassDetailed2) & .tab_pred$PredLabel == .tab_pred$ClassDetailed2)
  } else {
    .tab_pred$PredLabel == .tab_pred$TrueLabel
  }
}

#' Per-category precision, recall, F1 and support from pooled predictions
#'
#' Precision for a category is over documents predicted as it; recall is over documents whose PRIMARY
#' label is it, so each document falls in exactly one recall bucket. Lenient scoring credits a
#' dual-class document as correct if the model predicts either of its two valid labels.
#'
#' @param .tab_pred Pooled predictions: DocID, TrueLabel, PredLabel, and optionally ClassDetailed2
#'   for lenient scoring, joined via clf_add_second_label().
#' @param .lenient Logical. Accept either the primary or the second label.
#' @param .none Character. Abstention sentinel, excluded from the class set. Harmless when no row
#'   carries it.
#' @return Tibble: Label, Precision, Recall, F1, Support, descending by Support.
clf_perclass <- function(.tab_pred, .lenient = FALSE, .none = "(none)") {
  if (FALSE) {
    .tab_pred <- clf_pool_predictions(c(.lP$Runs$Bert, .lP$Runs$Kw), best_)
    .lenient  <- FALSE
    .none     <- "(none)"
  }
  if (.lenient && !"ClassDetailed2" %in% names(.tab_pred)) {
    cli::cli_alert_warning(
      "Lenient requested but no ClassDetailed2 column; scoring strictly. Join via clf_add_second_label()."
    )
    .lenient <- FALSE
  }
  correct_ <- clf_correct_vec(.tab_pred, .lenient)
  pred_    <- .tab_pred$PredLabel
  true_    <- .tab_pred$TrueLabel
  classes_ <- setdiff(sort(unique(c(true_, pred_))), .none)
  n_       <- length(classes_)
  if (n_ == 0L) {
    return(tibble::tibble(Label = character(), Precision = numeric(), Recall = numeric(),
                          F1 = numeric(), Support = integer()))
  }

  # Counted with tabulate over factors rather than by looping the categories. The loop is the obvious
  # way to write this and reads more directly, but the routing search calls this function once per
  # policy per fold -- tens of thousands of times in one render -- and the per-category tibble it
  # built each time round dominated that cost. This does the same four counts in four vectorised
  # passes and assembles one tibble.
  #
  # Values outside the class set -- the abstention sentinel above all -- become NA under factor() and
  # are dropped by tabulate, which is what the loop's equality tests did too: a document the method
  # declined is a positive for no category and a negative for the one it truly belongs to.
  pred_f_ <- factor(pred_, levels = classes_)
  true_f_ <- factor(true_, levels = classes_)

  pp_ <- tabulate(pred_f_, nbins = n_)               # predicted as each category
  ap_ <- tabulate(true_f_, nbins = n_)               # truly each category
  tp_prec_ <- tabulate(pred_f_[correct_], nbins = n_)
  tp_rec_  <- tabulate(true_f_[correct_], nbins = n_)

  prec_ <- ifelse(pp_ == 0L, NA_real_, tp_prec_ / pp_)
  rec_  <- ifelse(ap_ == 0L, NA_real_, tp_rec_ / ap_)
  # A category the method never predicts has undefined precision, and one absent from the sample has
  # undefined recall; both score zero F1 rather than propagating NA into the macro average, which is
  # the same convention the per-category loop applied.
  f1_   <- ifelse(
    is.na(prec_) | is.na(rec_) | (prec_ + rec_) == 0,
    0,
    2 * prec_ * rec_ / (prec_ + rec_)
  )

  tibble::tibble(
    Label     = classes_,
    Precision = prec_,
    Recall    = rec_,
    F1        = f1_,
    Support   = as.integer(ap_)
  ) |>
    dplyr::arrange(dplyr::desc(.data$Support))
}

#' Headline scores from pooled predictions
#'
#' Coverage is the share of documents the method committed on, so it is one for any method that
#' always predicts. Accuracy counts an abstention as incorrect.
#'
#' @param .tab_pred Pooled predictions.
#' @param .lenient Logical. Lenient scoring; requires ClassDetailed2.
#' @param .none Character. Abstention sentinel.
#' @return One-row tibble: Scoring, Accuracy, F1_macro, F1_weighted, Coverage, N.
clf_scores <- function(.tab_pred, .lenient = FALSE, .none = "(none)") {
  if (FALSE) {
    .tab_pred <- pred_det
    .lenient  <- FALSE
    .none     <- "(none)"
  }
  if (.lenient && !"ClassDetailed2" %in% names(.tab_pred)) {
    cli::cli_alert_warning(
      "Lenient requested but no ClassDetailed2 column; scoring strictly. Join via clf_add_second_label()."
    )
    .lenient <- FALSE
  }
  pc_      <- clf_perclass(.tab_pred, .lenient = .lenient, .none = .none)
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

#' Confusion matrix from pooled predictions, wide
#'
#' A "(none)" column, when present, shows which true categories the method abstained on, which is
#' where a keyword lexicon has no signal.
#'
#' @param .tab_pred Pooled predictions.
#' @return Wide tibble: TrueLabel plus one column per predicted label.
clf_confusion <- function(.tab_pred) {
  if (FALSE) .tab_pred <- pred_det
  .tab_pred |>
    dplyr::count(.data$TrueLabel, .data$PredLabel) |>
    tidyr::pivot_wider(names_from = "PredLabel", values_from = "n", values_fill = 0L) |>
    dplyr::arrange(.data$TrueLabel)
}

#' Confusion matrix in long form, optionally row-normalised
#'
#' The shape the heatmap primitive consumes. Row normalisation is the default because the question a
#' confusion matrix answers is where a category's documents went, and at raw counts that question is
#' unanswerable for every category except the largest.
#'
#' @param .tab_pred Pooled predictions.
#' @param .normalize Logical. Express each cell as a share of its true category.
#' @return Tibble: TrueLabel, PredLabel, N, Value.
clf_confusion_long <- function(.tab_pred, .normalize = TRUE) {
  if (FALSE) {
    .tab_pred  <- pred_det
    .normalize <- TRUE
  }
  out_ <- .tab_pred |> dplyr::count(.data$TrueLabel, .data$PredLabel, name = "N")
  if (.normalize) {
    out_ |> dplyr::mutate(Value = .data$N / sum(.data$N), .by = TrueLabel)
  } else {
    out_ |> dplyr::mutate(Value = as.numeric(.data$N))
  }
}


# 9. Publication figures -------------------------------------------------------------------------------------------------
# Every figure the classification family shares. The look comes entirely from _Commons/_Plots.R: no
# colour, font, size or height is set below. What these functions own is the mapping from a scored
# tibble to a figure shape, which is the part that needs to know what the numbers mean.
#
# All of them key on the registered vocabulary, so a category sits in the same position here, in
# 03B's confusion matrices, and in 03E's routed per-category scores. That is the whole point: a
# reader comparing two panels should be comparing the numbers, not re-reading two sets of axis
# labels to establish that the rows correspond.

#' Category distribution for one task
#'
#' @param .tab Prepared tibble.
#' @param .level Character. One of ClassDetailed, ClassBroad, AmendType.
#' @param .include_na Logical. Keep unlabelled documents as an explicit category.
#' @return A ggplot.
clf_plot_class_distribution <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType"),
                                        .include_na = FALSE) {
  if (FALSE) {
    .tab        <- tab_prep
    .level      <- "ClassDetailed"
    .include_na <- FALSE
  }
  .level <- match.arg(.level)
  clf_class_distribution(.tab, .level, .include_na = .include_na) |>
    plot_bar_ranked(
      .cat   = "Class",
      .val   = "N",
      .key   = .level,
      .short = TRUE,
      .label = TRUE
    ) +
    ggplot2::labs(x = "Documents")
}

#' Category composition by labelling round
#'
#' Shows which categories the current taxonomy required new labelling work for, rather than carrying
#' a mapped label forward. A category that is almost entirely second-round is one the earlier
#' taxonomy had no equivalent of.
#'
#' @param .tab Prepared tibble.
#' @param .level Character. One of ClassDetailed, ClassBroad, AmendType.
#' @param .share Logical. Normalise each category to its own composition.
#' @return A ggplot.
clf_plot_round_composition <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType"),
                                       .share = TRUE) {
  if (FALSE) {
    .tab   <- tab_prep
    .level <- "ClassDetailed"
    .share <- TRUE
  }
  .level <- match.arg(.level)
  clf_class_by_round(.tab, .level) |>
    plot_bar_stacked(
      .cat   = "Class",
      .val   = "N",
      .fill  = "LabelRound",
      .key   = .level,
      .short = TRUE,
      .share = .share
    ) +
    ggplot2::labs(x = if (.share) "Share of category" else "Documents")
}

#' Fold balance as a heatmap of within-category shares
#'
#' Each row sums to one across the folds, so an even deal shows as a uniform row. The fill scale is
#' anchored at twice the expected share rather than left to auto-scale, which puts a perfectly even
#' deal at the exact middle of the ramp. That anchoring is the whole point of the figure: stratified
#' folds differ from each other by well under a percentage point, and a scale fitted to that spread
#' would render rounding noise as strong visual structure and invite the conclusion the figure exists
#' to rule out. On this scale a category missing from a fold reads as white and nothing else does.
#'
#' @param .tab Prepared tibble.
#' @param .level Character. One of ClassDetailed, ClassBroad, AmendType.
#' @return A ggplot.
clf_plot_fold_balance <- function(.tab, .level = c("ClassDetailed", "ClassBroad", "AmendType")) {
  if (FALSE) {
    .tab   <- tab_prep
    .level <- "ClassDetailed"
  }
  .level <- match.arg(.level)
  dat_ <- clf_fold_shares(.tab, .level)
  k_   <- dplyr::n_distinct(dat_$Fold)

  dat_ |>
    plot_heatmap(
      .x      = "Fold",
      .y      = "Class",
      .fill   = "Share",
      .key_y  = .level,
      .short  = TRUE,
      .pct    = TRUE,
      .angle  = 0,
      .limits = c(0, 2 / k_)
    )
}

#' Per-category F1 from pooled predictions
#'
#' @param .tab_pred Pooled predictions.
#' @param .key Character. Registered vocabulary ordering the categories.
#' @param .lenient Logical. Lenient scoring; requires ClassDetailed2.
#' @param .none Character. Abstention sentinel.
#' @return A ggplot.
clf_plot_perclass <- function(.tab_pred, .key = "ClassDetailed", .lenient = FALSE,
                              .none = "(none)") {
  if (FALSE) {
    .tab_pred <- pred_det
    .key      <- "ClassDetailed"
    .lenient  <- FALSE
    .none     <- "(none)"
  }
  clf_perclass(.tab_pred, .lenient = .lenient, .none = .none) |>
    plot_bar_ranked(
      .cat      = "Label",
      .val      = "F1",
      .key      = .key,
      .short    = TRUE,
      .accuracy = 0.01,
      .limits   = c(0, 1)
    ) +
    ggplot2::labs(x = "F1")
}

#' Confusion heatmap, rows true and columns predicted
#'
#' @param .tab_pred Pooled predictions.
#' @param .key Character. Registered vocabulary ordering both axes.
#' @param .normalize Logical. Row-normalise to within-category shares.
#' @param .label Logical. Print the value in each cell.
#' @param .none Character. Abstention sentinel. It is a predicted label but not a category, so it is
#'   never registered and is passed to the figure as an extra level instead.
#' @param .limits Numeric length-two or NULL. Fix the fill scale. Pass c(0, 1) whenever more than one
#'   confusion matrix is shown, or each panel scales to its own maximum and three matrices that look
#'   alike are shaded on three different scales.
#' @param .label_min Numeric or NULL. Suppress the label on cells below this. A confusion matrix is
#'   mostly zeros, and a hundred and forty-four cells reading "0%" bury the ones that carry the
#'   result.
#' @param .angle Numeric. Rotation of the predicted-category axis labels.
#' @return A ggplot.
clf_plot_confusion <- function(.tab_pred, .key = "ClassDetailed", .normalize = TRUE,
                               .label = TRUE, .none = "(none)", .limits = NULL,
                               .label_min = NULL, .angle = 40) {
  if (FALSE) {
    .tab_pred  <- pred_det
    .key       <- "ClassDetailed"
    .normalize <- TRUE
    .label     <- TRUE
    .none      <- "(none)"
    .limits    <- c(0, 1)
    .label_min <- 0.005
    .angle     <- 40
  }
  clf_confusion_long(.tab_pred, .normalize = .normalize) |>
    plot_heatmap(
      .x         = "PredLabel",
      .y         = "TrueLabel",
      .fill      = "Value",
      .key_x     = .key,
      .key_y     = .key,
      .short     = TRUE,
      .label     = .label,
      .pct       = .normalize,
      .extra     = .none,
      .limits    = .limits,
      .label_min = .label_min,
      .angle     = .angle
    ) +
    ggplot2::labs(x = "Predicted", y = "True")
}

#' Leaderboard: mean macro-F1 by configuration with a fold-level interval
#'
#' Ordered by the estimate rather than by a registered vocabulary, because configurations are not a
#' fixed set: the sweep can add one at any time and ranking is what the figure is for.
#'
#' @param .tab_overall Output of clf_load_overall().
#' @param .label_col Character or NULL. Restrict to one task.
#' @param .n Integer. Configurations to show.
#' @return A ggplot.
clf_plot_leaderboard <- function(.tab_overall, .label_col = NULL, .n = 15L) {
  if (FALSE) {
    .tab_overall <- clf_load_overall(.lP$Runs$Bert)
    .label_col   <- "ClassDetailed"
    .n           <- 15L
  }
  tab_ <- if (is.null(.label_col)) .tab_overall else {
    dplyr::filter(.tab_overall, .data$LabelCol == .label_col)
  }
  clf_leaderboard(tab_) |>
    utils::head(.n) |>
    dplyr::mutate(
      Config = clf_config_label(.config_name = .data$ConfigName),
      Lo     = .data$F1macro_mean - .data$F1macro_sd,
      Hi     = .data$F1macro_mean + .data$F1macro_sd
    ) |>
    plot_points_ci(.cat = "Config", .val = "F1macro_mean", .lo = "Lo", .hi = "Hi") +
    ggplot2::labs(x = "Macro-F1 (mean +/- one fold-level sd)")
}

#' Per-category strict against lenient F1
#'
#' The gap for a category is what dual-class ambiguity costs it, and it concentrates on the rare
#' categories that appear as second labels. A large gap is a statement about the taxonomy, not about
#' the model.
#'
#' @param .tab_pred Pooled predictions with ClassDetailed2 attached via clf_add_second_label().
#' @param .key Character. Registered vocabulary ordering the categories.
#' @param .none Character. Abstention sentinel.
#' @return A ggplot.
clf_plot_strict_lenient <- function(.tab_pred, .key = "ClassDetailed", .none = "(none)") {
  if (FALSE) {
    .tab_pred <- clf_add_second_label(pred_det, tab_prep)
    .key      <- "ClassDetailed"
    .none     <- "(none)"
  }
  if (!"ClassDetailed2" %in% names(.tab_pred)) {
    cli::cli_abort("Need ClassDetailed2; join it via clf_add_second_label().")
  }
  strict_  <- clf_perclass(.tab_pred, .lenient = FALSE, .none = .none) |>
    dplyr::select(Label, Strict = F1)
  lenient_ <- clf_perclass(.tab_pred, .lenient = TRUE, .none = .none) |>
    dplyr::select(Label, Lenient = F1)

  dat_ <- dplyr::inner_join(strict_, lenient_, by = dplyr::join_by(Label)) |>
    dplyr::mutate(Label = plot_factor(.data$Label, .key = .key, .short = TRUE, .rev = TRUE))

  dat_ |>
    ggplot2::ggplot(ggplot2::aes(y = .data$Label)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$Strict, xend = .data$Lenient, y = .data$Label, yend = .data$Label),
      linewidth = 0.5, colour = .plot_ref
    ) +
    ggplot2::geom_point(ggplot2::aes(x = .data$Strict, colour = "Strict"), size = 1.9) +
    ggplot2::geom_point(ggplot2::aes(x = .data$Lenient, colour = "Lenient"), size = 1.9) +
    plot_scale_colour_cat(name = NULL, breaks = c("Strict", "Lenient")) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "F1", y = NULL) +
    plot_theme(.grid = "x")
}


# 10. Compatibility ----------------------------------------------------------------------------------------------------
# The console table helpers now live in _Commons/_Tables.R and the theme in _Commons/_Plots.R, where
# every family can reach them without sourcing a classification script. These aliases keep the old
# names working while the remaining documents are migrated one at a time; they are removed once no
# document calls them. Each is a plain rebinding, so behaviour is identical either way.

clf_say_table   <- tbl_say
clf_fmt_table   <- tbl_fmt
clf_pct         <- tbl_pct
clf_apply_theme <- plot_apply_theme
