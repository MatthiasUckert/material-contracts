# 03E-ClassifyOrchestrate: comparing the arms on one table (orch_*) ----------------------------------------------------
#
# WHAT THIS FILE DOES
# 03B, 03C and 03D each answered "how good is my engine". None of them can answer "does combining
# them help", because none can see what the others said about any particular document. This file puts
# all three on one table -- one row per document, one column per engine -- and tells one story in
# four steps.
#
#   0. The transformer is fitted at two context lengths. Which one ships, and what does the other
#      one give? The length is that engine's cost knob, so both are on the table.
#   1. How good is each engine on its own, category by category?
#   2. What would PERFECT routing give us? It needs the truth and nobody can run it, which is the
#      point: it is the absolute ceiling on every rule at once.
#   3. Can a clue we can actually see find the documents worth rerouting? First clue: the transformer
#      is unsure.
#   4. Second clue: the transformer's own two models contradict each other across the taxonomy.
#
# NOTHING IS RE-RUN HERE. Every number comes from the predictions.parquet files the training stages
# already wrote. No checkpoint is loaded, no lexicon is applied, no prompt is sent.
#
# THE PREDICTIONS ARE ALREADY OUT OF FOLD
# Each training stage cross-validated over five folds, so every label on this table came from a model
# that never saw that document. Any accuracy computed here is honest without further machinery. What
# is NOT protected is the choice of which threshold to quote: sections 3 and 4 read the labels to
# decide where a swap would have helped. With a handful of fixed thresholds rather than a search that
# is a caveat to state, not a design to build around, and the text states it.
#
# ABSTENTION IS NA, NOT A LABEL
# The keyword engine emits a sentinel where no term fired. It is converted to NA on read, so silence
# is missing rather than a category. That distinction runs through every metric below: an abstention
# is a miss for recall over the whole corpus and is excluded from precision, and both numbers are
# reported side by side rather than one standing in for the other.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; if (FALSE) dev blocks; pure ASCII; parenthesised cli interpolation.

if (FALSE) {
  .runs_roots <- c(.lP$Runs$Bert, .lP$Runs$Kw, .lP$Runs$Llm)
  .label_col  <- "ClassDetailed"
}

# The abstention sentinel the keyword and language-model arms emit.
ORCH_NONE <- "(none)"

# One vocabulary this document owns, registered with the design layer the way 03A registers the
# taxonomy. A transformer engine is named for its context length, because the length is the knob that
# distinguishes two otherwise identical models and a reader comparing them needs it on the axis rather
# than in a caption. The reading order is transformers by ascending length, then the cheap arm, then
# the expensive one -- the order the ceiling ladder adds them in, so tables and figures scan the same
# way.
#
# The lengths are listed rather than discovered because the registry is built when this file is
# sourced, before any panel exists. A new context length upstream needs one entry here; until it has
# one, orch_engines() reports it as unregistered rather than dropping it silently.
.orch_engines <- c("Bert256", "Bert512", "Kw", "Llm")

plot_register_levels(
  .key     = "Engine",
  .levels  = .orch_engines,
  .short   = NULL,
  .colours = plot_pal_seq(length(.orch_engines))
)

# 1. Reading what the training stages wrote ----------------------------------------------------------------------------

#' Pooled predictions for a named set of configurations, read in one pass
#'
#' The shared pooling helper globs every prediction file and then filters to one configuration, so
#' calling it once per engine per task re-reads the whole run tree nine times. This reads it once and
#' keeps every wanted configuration, which at this many runs is the difference between seconds and
#' minutes.
#'
#' @param .runs_roots Character vector of runs directories.
#' @param .configs Tibble with Engine and ConfigName, naming what to keep.
#' @return Long tibble: Engine, DocID, Fold, TrueLabel, Pred, Score.
orch_read <- function(.runs_roots, .configs) {
  if (FALSE) {
    .runs_roots <- c(.lP$Runs$Bert, .lP$Runs$Kw)
    .configs    <- crowned_det
  }
  paths_ <- .runs_roots |>
    purrr::map(\(.r) if (fs::dir_exists(.r)) {
      fs::dir_ls(.r, recurse = TRUE, glob = "*predictions.parquet")
    } else {
      character(0)
    }) |>
    purrr::list_c()
  paths_ <- paths_[!grepl("_smoke", paths_)]
  if (length(paths_) == 0L) cli::cli_abort("No predictions.parquet under {(.runs_roots)}")

  want_ <- unique(.configs$ConfigName)
  out_  <- purrr::map(paths_, function(.p) {
    tab_ <- arrow::read_parquet(.p)
    tab_[tab_$ConfigName %in% want_, , drop = FALSE]
  }) |>
    purrr::list_rbind()

  miss_ <- setdiff(want_, unique(out_$ConfigName))
  if (length(miss_) > 0L) {
    cli::cli_abort(c(
      "No predictions found for {length(miss_)} crowned configuration{?s}.",
      "i" = "Missing: {miss_}",
      "i" = "A crowned configuration with no run folder means the training stage deployed a model \\
             it never cross-validated, which is an upstream problem rather than one to work around."
    ))
  }

  out_ |>
    dplyr::inner_join(.configs, by = dplyr::join_by(ConfigName)) |>
    dplyr::transmute(
      Engine, DocID,
      Fold  = as.integer(.data$Fold),
      TrueLabel,
      Pred  = .data$PredLabel,
      Score = dplyr::coalesce(as.numeric(.data$Score), NA_real_)
    )
}

#' Configuration names with a fitted transformer on disk
#'
#' Discovered rather than constructed. 03B fits one model per task AND per context length under
#' model_final/, and names each directory for the configuration it holds, so the set of deployable
#' transformers is a directory listing. Rebuilding those names from a convention is the round trip
#' that once let a manifest point at a configuration nobody had fitted, and it finds every length the
#' trainer deployed rather than only the one it crowned.
#'
#' A directory holding no model subdirectory is skipped: a deployment interrupted part way leaves the
#' folder behind, and a configuration admitted on the strength of an empty folder is one the panel
#' could not be built from.
#'
#' @param .dir_bert Output root of the training stage.
#' @return Character vector of configuration names, empty where nothing has been deployed.
orch_final_configs <- function(.dir_bert) {
  if (FALSE) .dir_bert <- .dir_bert
  root_ <- fs::path(.dir_bert, "model_final")
  if (!fs::dir_exists(root_)) return(character(0))
  dirs_ <- fs::dir_ls(root_, type = "directory", glob = "*__FINAL")
  dirs_ <- dirs_[fs::dir_exists(fs::path(dirs_, "model"))]
  sub("__FINAL$", "", fs::path_file(dirs_))
}

#' The panel: one row per document, one column pair per engine
#'
#' The single table everything below is a group-by on. Wide rather than long, because every question
#' in this document compares engines WITHIN a document -- who agreed, who was right, what a swap would
#' have shipped -- and each of those is a row-wise expression on a wide frame and a join on a long one.
#'
#' `.commit_only` exists for the binary task. The keyword engine mines the positive class only and
#' labels everything else `Original`, which is the absence of evidence wearing the name of a class.
#' Left alone the arm looks like it covers the corpus and agrees with the transformer almost
#' everywhere, and every table below reports a coverage it does not have. Naming the labels that count
#' as commitments turns silence back into NA.
#'
#' @param .long Output of orch_read().
#' @param .commit_only Character vector of labels a keyword arm may commit to, or NULL for all.
#' @param .none Character. Abstention sentinel, converted to NA.
#' @return Tibble: DocID, Fold, Truth, then Pred/Score columns per engine (Bert, BertScore, ...).
orch_panel <- function(.long, .commit_only = NULL, .none = ORCH_NONE) {
  if (FALSE) {
    .long        <- long_det
    .commit_only <- NULL
    .none        <- ORCH_NONE
  }
  dat_ <- .long |>
    dplyr::mutate(
      Pred = dplyr::if_else(.data$Pred == .none, NA_character_, .data$Pred),
      Pred = if (is.null(.commit_only)) {
        .data$Pred
      } else {
        dplyr::if_else(.data$Engine == "Kw" & !.data$Pred %in% .commit_only, NA_character_,
                       .data$Pred)
      }
    )

  # Truth is carried once rather than per engine. Every engine scored the same documents against the
  # same labels, so a mismatch here is a join fault and should abort rather than be averaged over.
  truth_ <- dat_ |> dplyr::distinct(DocID, Fold, Truth = .data$TrueLabel)
  if (nrow(truth_) != dplyr::n_distinct(dat_$DocID)) {
    cli::cli_abort("A document carries two different true labels; the run folders disagree.")
  }

  wide_ <- dat_ |>
    dplyr::select(Engine, DocID, Pred, Score) |>
    tidyr::pivot_wider(names_from = "Engine", values_from = c("Pred", "Score"),
                       names_glue = "{Engine}{.value}")
  names(wide_) <- sub("Pred$", "", names(wide_))

  truth_ |>
    dplyr::left_join(wide_, by = dplyr::join_by(DocID)) |>
    dplyr::relocate(DocID, Fold, Truth)
}

#' Which engines are present in a panel
#'
#' Read off the columns rather than assumed, so a task the generative stage has not covered yields a
#' narrower analysis instead of an error.
#'
#' @param .tab Output of orch_panel().
#' @return Character vector of engine names, in the registered reading order.
orch_engines <- function(.tab) {
  if (FALSE) .tab <- panel_det
  cand_ <- setdiff(names(.tab), c("DocID", "Fold", "Truth"))
  cand_ <- cand_[!grepl("Score$", cand_)]

  # Transformers sorted by context length NUMERICALLY. Sorted as text, a hypothetical Bert1024 would
  # fall between Bert100 and Bert256, and the ladder would add the widest window in the middle.
  bert_ <- cand_[grepl("^Bert", cand_)]
  bert_ <- bert_[order(suppressWarnings(as.integer(sub("^Bert", "", bert_))))]
  out_  <- c(bert_, intersect(c("Kw", "Llm"), cand_))

  new_ <- setdiff(out_, .orch_engines)
  if (length(new_) > 0L) {
    cli::cli_alert_warning(
      "{length(new_)} engine{?s} ({new_}) {?is/are} not in the registered vocabulary, so {?it/they} \\
       will take a default colour. Add {?it/them} to .orch_engines to fix the palette."
    )
  }
  out_
}


# 2. Each engine on its own --------------------------------------------------------------------------------------------
# The reference tables. Two of the three engines abstain, so every metric here has to say what it does
# with an abstention, and the answer is different for each.
#
# PRECISION EXCLUDES ABSTENTIONS. An arm that declines to answer has not made a wrong prediction, and
# counting silence against precision would make a cautious arm look inaccurate rather than quiet.
#
# RECALL HAS TWO HONEST FORMS AND BOTH ARE REPORTED. Over the whole class, an abstention is a miss:
# the document was in that category and the arm did not find it. Among the documents the arm committed
# to, it is excluded. The first is what matters for a corpus pass and is comparable with the
# transformer; the second describes the arm on its own terms. Reporting only the second is how a
# keyword table with 26% coverage comes to look competitive with a model that answers everywhere.
#
# ACCURACY IS ONE-VS-REST AT THE CATEGORY LEVEL and is dominated by true negatives -- a rare category
# reads near-perfect while the arm misses almost every document in it. It is reported because it is
# asked for and it is read with that caveat, never on its own.

#' Per-category scores for one engine
#'
#' @param .tab Output of orch_panel().
#' @param .engine Character. Column holding that engine's prediction.
#' @return Tibble: Category, Support, nPredicted, Coverage, Accuracy, Precision, Recall, RecallSel,
#'   F1, with a final macro-averaged row across categories.
orch_perclass <- function(.tab, .engine = "Bert256") {
  if (FALSE) {
    .tab    <- panel_det
    .engine <- "Kw"
  }
  pred_ <- .tab[[.engine]]
  true_ <- .tab$Truth
  n_    <- length(true_)

  out_ <- purrr::map(sort(unique(true_)), function(.k) {
    is_k_   <- true_ == .k
    said_k_ <- dplyr::coalesce(pred_ == .k, FALSE)
    tp_     <- sum(is_k_ & said_k_)
    comm_k_ <- sum(is_k_ & !is.na(pred_))
    tibble::tibble(
      Category   = .k,
      Support    = sum(is_k_),
      nPredicted = sum(said_k_),
      Coverage   = comm_k_ / sum(is_k_),
      # One-vs-rest: a correct rejection counts, which is why a rare category reads high here whatever
      # the arm did with the documents actually in it.
      Accuracy   = (tp_ + sum(!is_k_ & !said_k_)) / n_,
      Precision  = if (sum(said_k_) > 0L) tp_ / sum(said_k_) else NA_real_,
      Recall     = tp_ / sum(is_k_),
      RecallSel  = if (comm_k_ > 0L) tp_ / comm_k_ else NA_real_
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      F1 = dplyr::if_else(
        is.finite(.data$Precision) & (.data$Precision + .data$Recall) > 0,
        2 * .data$Precision * .data$Recall / (.data$Precision + .data$Recall),
        0
      )
    )

  # The summary row mixes two kinds of average deliberately. Precision, Recall and F1 are MACRO --
  # every category counts once, which is the measure this study reports because the categories differ
  # in size by an order of magnitude and a weighted figure would report the largest of them. Coverage
  # and Accuracy are OVERALL, computed across documents, because a macro average of one-vs-rest
  # accuracies is not a quantity anybody wants. The report says which is which.
  dplyr::bind_rows(
    out_,
    tibble::tibble(
      Category   = "All categories",
      Support    = n_,
      nPredicted = sum(!is.na(pred_)),
      Coverage   = mean(!is.na(pred_)),
      Accuracy   = mean(dplyr::coalesce(pred_ == true_, FALSE)),
      Precision  = mean(out_$Precision, na.rm = TRUE),
      Recall     = mean(out_$Recall),
      RecallSel  = mean(out_$RecallSel, na.rm = TRUE),
      F1         = mean(out_$F1)
    )
  )
}

#' Report one engine's per-category table
#' @param .tab Output of orch_perclass().
#' @param .engine Character. Named in the heading.
#' @param .label_col Task, for the heading.
#' @return Invisibly .tab.
orch_report_perclass <- function(.tab, .engine = "Bert256", .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab       <- perclass_det_bert
    .engine    <- "Bert256"
    .label_col <- "ClassDetailed"
  }
  tbl_head("{(.engine)} by category -- {(.label_col)}")
  tbl_out(
    .tab    = .tab,                                                     # numeric; formatting is below
    .title  = paste0(.engine, " by category -- ", .label_col),          # caption when rendered
    .groups = c(" " = 1, "Size" = 2, "Behaviour" = 2, "Quality" = 4),   # what the columns are for
    .pct    = c("Coverage", "Accuracy", "Precision", "Recall", "RecallSel"),
    .notes  = c(
      Coverage  = paste("The share of the category the engine answered on at all. Support is",
                        "documents truly in the category; nPredicted is documents this engine",
                        "assigned to it."),
      Accuracy  = paste("One-vs-rest and dominated by correct rejections. A rare category reads",
                        "near-perfect while the engine misses most of its documents; read Precision",
                        "and Recall instead."),
      Recall    = paste("Counts an abstention as a miss, so it is comparable across engines. F1 uses",
                        "this recall, which is why an abstaining arm shows high precision and low F1",
                        "-- the honest picture of a partial classifier."),
      RecallSel = paste("Measured only where the engine committed, so it describes the arm on its own",
                        "terms and is not comparable with an engine that answers everywhere."),
      # Left unnamed: this one is about the table rather than about a column, so it carries no
      # marker. An unnamed element of a partly-named vector takes "" as its name, which is what
      # tbl_grouped() reads as "attach to nothing, and do not warn about it".
      paste("In the final row Precision, Recall, RecallSel and F1 are macro averages --",
                        "every category counts once -- while Coverage and Accuracy are computed over",
                        "all documents. For an abstaining arm that row therefore shows high precision",
                        "beside low accuracy, and both are correct.")
    ),
    .summary_row = nrow(.tab)   # the macro row, set apart from the categories
  )
  invisible(.tab)
}

#' Headline scores for every engine side by side
#'
#' Micro and macro answer different questions and the gap between them is the finding. Micro weights
#' every document equally and therefore reports the largest categories; macro weights every category
#' equally and reports the problem this study is actually solving, where the smallest class has under
#' forty documents.
#'
#' @param .tab Output of orch_panel().
#' @return Tibble: Engine, nDocs, Coverage, Accuracy, AccuracySel, MacroF1, MicroF1, WeightedF1.
orch_headline <- function(.tab) {
  if (FALSE) .tab <- panel_det
  purrr::map(orch_engines(.tab = .tab), function(.e) {
    pc_   <- orch_perclass(.tab = .tab, .engine = .e) |> dplyr::filter(.data$Category != "All categories")
    pred_ <- .tab[[.e]]
    hit_  <- dplyr::coalesce(pred_ == .tab$Truth, FALSE)
    tp_   <- sum(hit_)
    # Micro precision divides by what the engine actually predicted; micro recall by every document.
    # For an arm that answers everywhere the two coincide and micro-F1 equals accuracy; for an
    # abstaining arm they part company, which is exactly what the column is for.
    mp_   <- if (sum(!is.na(pred_)) > 0L) tp_ / sum(!is.na(pred_)) else NA_real_
    mr_   <- tp_ / nrow(.tab)
    tibble::tibble(
      Engine      = .e,
      nDocs       = nrow(.tab),
      Coverage    = mean(!is.na(pred_)),
      Accuracy    = mean(hit_),
      AccuracySel = mean(pred_ == .tab$Truth, na.rm = TRUE),
      MacroF1     = mean(pc_$F1),
      MicroF1     = if (is.finite(mp_) && (mp_ + mr_) > 0) 2 * mp_ * mr_ / (mp_ + mr_) else NA_real_,
      WeightedF1  = sum(pc_$F1 * pc_$Support) / sum(pc_$Support)
    )
  }) |>
    purrr::list_rbind()
}

#' Report the headline table
#' @param .tab Output of orch_headline().
#' @param .label_col Task, for the heading.
#' @return Invisibly .tab.
orch_report_headline <- function(.tab, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab       <- headline_det
    .label_col <- "ClassDetailed"
  }
  tbl_head("Every engine side by side -- {(.label_col)}")
  tbl_out(
    .tab    = .tab,                                                  # numeric
    .title  = paste0("Every engine side by side -- ", .label_col),   # caption when rendered
    .groups = c(" " = 2, "Reach" = 1, "Accuracy" = 2, "F1" = 3),     # what the columns are for
    .pct    = c("Coverage", "Accuracy", "AccuracySel"),
    .notes  = c(
      Accuracy    = paste("Counts an abstention as a miss. For an engine at full coverage this and",
                          "AccuracySel are the same number."),
      AccuracySel = "Measured only where the engine committed.",
      MacroF1     = paste("Weights every category equally and is this study's headline, because the",
                          "categories differ in size by an order of magnitude."),
      WeightedF1  = "Weights by category size and therefore reports the largest categories."
    ),
    .summary_row = NULL   # every row is an engine; none of them is a summary of the others
  )
  invisible(.tab)
}


# 3. The two context lengths -------------------------------------------------------------------------------------------
# 03B fits the transformer at two truncation lengths and crowns one per task. Both are on the panel,
# because the length is that engine's cost knob: a second full pass over 1.1 million documents is not
# free, and a reader deciding whether to pay for it needs the two side by side rather than one of them
# and a note saying the other was close.
#
# The comparison is the same shape as every other question in this document. Where the two agree there
# is nothing to choose between them. Where they part company, one of them is wrong, and which one is
# right more often IS the case for a length -- an aggregate difference of two tenths of a point can
# sit on top of hundreds of documents changing hands in both directions.

#' The two transformer windows, agreeing and disagreeing
#'
#' Three rows rather than two aggregate numbers. The aggregate hides the trade: two windows within a
#' fraction of a point of each other can disagree on hundreds of documents, and a reader choosing
#' between them wants to know whether the difference is a handful of documents or a wash across many.
#'
#' Accuracy columns are named for the engines they describe, so a table read out of context still says
#' which length it is about.
#'
#' @param .tab Output of orch_panel().
#' @param .a,.b Character. The two transformer engines to compare.
#' @return Tibble: Case, nDocs, Share, Acc<.a>, Acc<.b>, EitherRight.
orch_windows <- function(.tab, .a, .b) {
  if (FALSE) {
    .tab <- panel_det
    .a   <- "Bert256"
    .b   <- "Bert512"
  }
  miss_ <- setdiff(c(.a, .b), names(.tab))
  if (length(miss_) > 0L) cli::cli_abort("Engine{?s} {miss_} {?is/are} not on the panel.")

  n_   <- nrow(.tab)
  dat_ <- .tab |>
    dplyr::mutate(
      A    = .data[[.a]],
      B    = .data[[.b]],
      Case = dplyr::if_else(.data$A == .data$B, "Both windows agree", "They disagree")
    )

  rows_ <- dplyr::bind_rows(
    dat_ |> dplyr::summarise(nDocs = dplyr::n(),
                             AccA = mean(.data$A == .data$Truth),
                             AccB = mean(.data$B == .data$Truth),
                             Either = mean(.data$A == .data$Truth | .data$B == .data$Truth),
                             .by = Case),
    dat_ |> dplyr::summarise(Case = "All documents", nDocs = dplyr::n(),
                             AccA = mean(.data$A == .data$Truth),
                             AccB = mean(.data$B == .data$Truth),
                             Either = mean(.data$A == .data$Truth | .data$B == .data$Truth))
  ) |>
    dplyr::mutate(Share = .data$nDocs / n_) |>
    dplyr::arrange(match(.data$Case, c("Both windows agree", "They disagree", "All documents"))) |>
    dplyr::select(Case, nDocs, Share, AccA, AccB, EitherRight = Either)

  names(rows_)[names(rows_) == "AccA"] <- paste0("Acc", .a)
  names(rows_)[names(rows_) == "AccB"] <- paste0("Acc", .b)
  rows_
}

#' Report the window comparison with the reading that decides whether to pay for the longer one
#' @param .tab Output of orch_windows().
#' @param .a,.b Character. The two engines, so the notes name them.
#' @param .crowned Character. The engine 03B deployed for this task.
#' @param .label_col Task, for the heading.
#' @return Invisibly .tab.
orch_report_windows <- function(.tab, .a, .b, .crowned, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab       <- windows_det
    .a         <- "Bert256"
    .b         <- "Bert512"
    .crowned   <- "Bert256"
    .label_col <- "ClassDetailed"
  }
  tbl_head("The two context lengths -- {(.label_col)}")
  tbl_out(
    .tab    = .tab,
    .title  = paste0("The two context lengths -- ", .label_col),
    .groups = c(" " = 3, "Accuracy" = 3),
    .pct    = c("Share", paste0("Acc", c(.a, .b)), "EitherRight"),
    .notes  = c(
      EitherRight = paste("The share of documents at least one window got right. On the disagreement",
                          "row it is the ceiling a perfect choice between the two would reach, and it",
                          "is not obtainable: knowing which to believe requires the label."),
      paste0("03B deployed ", .crowned, " for this task. This document reads that decision rather",
             " than remaking it; the table says what the other length would have given.")
    ),
    .summary_row = nrow(.tab)
  )

  dis_ <- .tab |> dplyr::filter(.data$Case == "They disagree")
  if (nrow(dis_) == 1L) {
    a_ <- dis_[[paste0("Acc", .a)]]
    b_ <- dis_[[paste0("Acc", .b)]]
    tbl_note(
      "The two windows part company on {dis_$nDocs} document{?s} ({tbl_pct(dis_$Share)}). There \\
       {(.a)} is right {tbl_pct(a_)} of the time against {(.b)} at {tbl_pct(b_)}, and one of them \\
       is right on {tbl_pct(dis_$EitherRight)}."
    )
    tbl_note(
      "Read that against the All documents row. A small aggregate gap sitting on top of a large \\
       disagreement means the two windows are trading documents rather than one dominating, and the \\
       cheaper one is the sensible default."
    )
  }
  invisible(.tab)
}


# 4. Perfect routing: the absolute ceiling -----------------------------------------------------------------------------
# A router can only relabel a document to something an engine already said. Taking the correct answer
# wherever any engine supplied one is therefore an upper bound on every rule at once, at every
# threshold and in every order.
#
# It uses the truth to choose and nobody can run it. Its job is to separate two readings of a null
# that no search can tell apart: nothing to find, or something to find and no way to see it. Where the
# ceiling sits at the transformer's own accuracy, everything below can only confirm it.

#' Accuracy a perfect router would reach, as engines are added
#'
#' Cumulative rather than one number, because the marginal row is what answers the deployment
#' question. An engine adding nothing to the ceiling cannot be routed to profitably by any rule: there
#' is no document it gets right that the engines before it did not.
#'
#' @param .tab Output of orch_panel().
#' @param .order Character vector of engines, in the order they should be added. The first is the
#'   baseline the ladder starts from.
#' @return Tibble: Step, Added, Engines, nCorrect, Accuracy, Gain, Marginal.
orch_ceiling <- function(.tab, .order) {
  if (FALSE) {
    .tab   <- panel_det
    .order <- c("Bert256", "Bert512", "Kw", "Llm")
  }
  hit_ <- purrr::map(.order, \(.e) dplyr::coalesce(.tab[[.e]] == .tab$Truth, FALSE))
  names(hit_) <- .order

  purrr::map(seq_along(.order), function(.i) {
    any_ <- Reduce(`|`, hit_[seq_len(.i)])
    tibble::tibble(
      Step     = .i - 1L,
      Added    = if (.i == 1L) paste0(.order[[1]], " alone") else .order[[.i]],
      Engines  = paste(.order[seq_len(.i)], collapse = " or "),
      nCorrect = sum(any_),
      Accuracy = mean(any_)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      Gain     = .data$Accuracy - .data$Accuracy[1],
      Marginal = .data$Accuracy - dplyr::lag(.data$Accuracy, default = .data$Accuracy[1])
    )
}

#' Report the ceiling with the reading it forces
#' @param .tab Output of orch_ceiling().
#' @param .label_col Task, for the heading.
#' @return Invisibly .tab.
orch_report_ceiling <- function(.tab, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab       <- ceil_det
    .label_col <- "ClassDetailed"
  }
  tbl_head("Perfect routing: the absolute ceiling -- {(.label_col)}")
  tbl_out(
    .tab    = .tab,                                                        # numeric
    .title  = paste0("Perfect routing: the absolute ceiling -- ", .label_col),
    .groups = c(" " = 3, "What a perfect router reaches" = 4),             # what the columns are for
    .pct    = c("Accuracy", "Gain", "Marginal"),
    .notes  = c(
      Gain     = "Against the first engine in the ladder, which is the one that ships today.",
      Marginal = paste("What this engine adds OVER the ones before it. An engine at zero cannot be",
                       "routed to profitably by any rule: there is no document it gets right that",
                       "the engines before it did not.")
    ),
    .summary_row = NULL   # a ladder, not a table with a total
  )

  top_ <- utils::tail(.tab, 1)
  tbl_note(
    "This uses the truth to choose and nobody can obtain it. With every engine available a perfect \\
     router reaches {tbl_pct(top_$Accuracy)}, {tbl_pct(top_$Gain)} above the transformer alone. \\
     Every rule in the two sections below is bounded by that number."
  )
  dead_ <- .tab |> dplyr::filter(.data$Step > 0L, .data$Marginal <= 0)
  if (nrow(dead_) > 0L) {
    tbl_note(
      "{nrow(dead_)} engine{?s} add nothing to the ceiling ({dead_$Added}). No rule at any threshold \\
       can profit from {?it/them}: there is no document {?it/they} get{?s/} right that the engines \\
       before {?it/them} did not.",
      .type = "warn"
    )
  }
  invisible(.tab)
}


# 5. Does a swap help on a chosen subset? ------------------------------------------------------------------------------
# The engine of sections 5 and 6, written once and called twice. Both ask the same question of
# different subsets: take the documents a clue points at, hand them to another arm, and see what
# happens.
#
# TWO ACCURACIES, BOTH NEEDED. Subset accuracy answers "did the swap help those documents"; overall
# accuracy answers "was it worth doing". A swap can lift the subset and still lose overall if it
# touches enough documents, and one number alone cannot show that.
#
# FIXED AND BROKE ARE THE HONEST PAIR. A swap repairing nine documents and breaking ninety-eight has a
# positive-sounding repair count. The two counts together are the only view that distinguishes a rule
# that found structure from one that traded it away.

#' What swapping in one arm would do on a chosen set of documents
#'
#' Outside the subset the transformer's label is kept, so the overall figure is what a deployment
#' applying this rule and nothing else would have shipped. Inside the subset the arm's label is taken
#' where it committed; where it abstained the transformer's label stands, because every document must
#' leave with one.
#'
#' @param .tab Output of orch_panel().
#' @param .engine Character. The arm swapped in.
#' @param .subset Logical vector over rows of .tab. The documents the clue points at.
#' @param .baseline Character. The engine whose labels ship today and which the swap displaces. Named
#'   rather than assumed, because two transformer windows are on the panel and which of them ships is
#'   a decision 03B made per task, not a property of the column ordering.
#' @param .label Character. What to call this subset in the output.
#' @return One-row tibble: Subset, n, ShareOfCorpus, BaseAcc, ArmCoverage, ArmAcc, Swapped, Fixed,
#'   Broke, SubsetAccAfter, OverallAccAfter, DeltaOverall.
orch_swap <- function(.tab, .engine, .subset, .baseline = "Bert256", .label = "subset") {
  if (FALSE) {
    .tab      <- panel_det
    .engine   <- "Kw"
    .subset   <- panel_det$Bert256Score < 0.9
    .baseline <- "Bert256"
    .label    <- "P < 0.900"
  }
  if (!.baseline %in% names(.tab)) cli::cli_abort("Baseline engine {(.baseline)} is not on the panel.")
  .subset <- dplyr::coalesce(.subset, FALSE)
  arm_    <- .tab[[.engine]]
  base_   <- .tab[[.baseline]]
  truth_  <- .tab$Truth

  # The rule itself, in one line: inside the subset take the arm where it spoke, otherwise keep the
  # transformer.
  new_    <- dplyr::if_else(.subset & !is.na(arm_), arm_, base_)
  moved_  <- new_ != base_
  in_     <- .subset

  tibble::tibble(
    Subset         = .label,
    n              = sum(in_),
    ShareOfCorpus  = mean(in_),
    BaseAcc        = if (sum(in_) > 0L) mean(base_[in_] == truth_[in_]) else NA_real_,
    ArmCoverage    = if (sum(in_) > 0L) mean(!is.na(arm_[in_])) else NA_real_,
    ArmAcc         = if (sum(in_ & !is.na(arm_)) > 0L) {
      mean(arm_[in_ & !is.na(arm_)] == truth_[in_ & !is.na(arm_)])
    } else {
      NA_real_
    },
    Swapped        = sum(moved_),
    Fixed          = sum(moved_ & new_  == truth_),
    Broke          = sum(moved_ & base_ == truth_),
    SubsetAccAfter = if (sum(in_) > 0L) mean(new_[in_] == truth_[in_]) else NA_real_,
    OverallAccAfter = mean(new_ == truth_),
    DeltaOverall   = mean(new_ == truth_) - mean(base_ == truth_)
  )
}

#' Report a set of swap results
#' @param .tab Rows produced by orch_swap(), bound together.
#' @param .engine Character. Named in the heading.
#' @param .label_col Task, for the heading.
#' @return Invisibly .tab.
orch_report_swap <- function(.tab, .engine = "Kw", .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab       <- resolve_det_kw
    .engine    <- "Kw"
    .label_col <- "ClassDetailed"
  }
  tbl_head("Resolving with {(.engine)} -- {(.label_col)}")

  # The spanning header is built from the columns actually present rather than written out, because
  # the caller prepends an Engine column when several arms are stacked into one table and a
  # hard-coded header would then be one column short.
  lead_ <- if ("Engine" %in% names(.tab)) 2L else 1L

  tbl_out(
    .tab    = .tab |>
      # A signed change in percentage points is not a share, so it is formatted here rather than
      # handed to .pct, which would render -0.015 as "-1.5%" and lose the sign convention the column
      # exists to carry.
      dplyr::mutate(DeltaOverall = sprintf("%+.2f pp", 100 * .data$DeltaOverall)),
    .title  = paste0("Resolving with ", .engine, " -- ", .label_col),
    .groups = c(" " = lead_, "Subset" = 2, "On those documents" = 3, "What the swap did" = 3,
                "Result" = 3),
    .pct    = c("ShareOfCorpus", "BaseAcc", "ArmCoverage", "ArmAcc", "SubsetAccAfter",
                "OverallAccAfter"),
    .notes  = c(
      ArmAcc          = paste("Measured on the same documents as BaseAcc and directly comparable to",
                              "it, but computed only where the arm committed; ArmCoverage says how",
                              "much of the subset that was."),
      Broke           = paste("Documents the transformer had right and the swap relabelled wrong.",
                              "Read it beside Fixed: a rule repairing nine and breaking ninety-eight",
                              "has a positive-sounding repair count."),
      OverallAccAfter = paste("Over the whole corpus, keeping the transformer outside the subset. A",
                              "swap can lift the subset and still lose overall if it touches enough",
                              "documents.")
    ),
    .summary_row = NULL   # every row is a threshold; none summarises the others
  )

  win_ <- .tab |> dplyr::filter(.data$DeltaOverall > 0)
  if (nrow(win_) == 0L) {
    tbl_note(
      "No subset where handing the documents to {(.engine)} improves the corpus. Fixed against Broke \\
       says why: the arm repairs some documents and breaks more."
    )
  } else {
    best_ <- win_ |> dplyr::slice_max(.data$DeltaOverall, n = 1L, with_ties = FALSE)
    tbl_note(
      "{nrow(win_)} subset{?s} improve the corpus; the best is {best_$Subset} at \\
       {sprintf('%+.2f pp', 100 * best_$DeltaOverall)}, repairing {best_$Fixed} and breaking \\
       {best_$Broke}."
    )
    tbl_note(
      "The subset was chosen by reading the labels, so this is where a rule might go rather than \\
       evidence that one works.",
      .type = "warn"
    )
  }
  invisible(.tab)
}


# 6. Clue one: the transformer is unsure -------------------------------------------------------------------------------
# The first clue anyone would reach for, and the one that is free: the transformer already emits a
# probability, and 03B showed it separates errors.
#
# THE THRESHOLDS LOOK HIGH BECAUSE THE DISTRIBUTION IS. The softmax is massed against one, so a floor
# at 0.90 selects a few per cent of the corpus rather than a tenth of it. The grid runs up to 0.99 for
# that reason: a conventional-looking 0.90 would test almost nothing and report a null earned by the
# size of the subset rather than by anything about the arms.

#' Swap in one arm on each low-confidence subset in turn
#'
#' @param .tab Output of orch_panel().
#' @param .engine Character. The arm swapped in.
#' @param .baseline Character. The engine that ships; its probability defines the subsets.
#' @param .thresholds Numeric vector. A document is in the subset where the baseline's score is below
#'   the value.
#' @return Rows of orch_swap(), one per threshold.
orch_resolve_conf <- function(.tab, .engine, .baseline = "Bert256",
                              .thresholds = c(0.900, 0.925, 0.950, 0.960, 0.970, 0.980, 0.990)) {
  if (FALSE) {
    .tab        <- panel_det
    .engine     <- "Kw"
    .baseline   <- "Bert256"
    .thresholds <- c(0.900, 0.950, 0.990)
  }
  score_ <- paste0(.baseline, "Score")
  if (!score_ %in% names(.tab)) cli::cli_abort("No score column for baseline {(.baseline)}.")

  purrr::map(.thresholds, function(.t) {
    orch_swap(
      .tab      = .tab,
      .engine   = .engine,
      .subset   = .tab[[score_]] < .t,
      .baseline = .baseline,
      .label    = sprintf("P < %.3f", .t)
    )
  }) |>
    purrr::list_rbind()
}


# 7. Clue two: the transformer contradicts itself across the taxonomy --------------------------------------------------
# A second clue, independent of the first and of the keyword arm entirely. Two transformers were
# trained on the same folds: one predicts the detailed category, the other the broad one. Roll the
# detailed prediction up to its parent and the two can be compared. Where they disagree, one of them
# is wrong about the same document.
#
# THIS IS NOT A TAUTOLOGY, THOUGH IT LOOKS LIKE ONE. 03B publishes the broad label by rolling up the
# detailed prediction, so the RELEASED output is consistent by construction. The broad MODEL still
# exists, still made its own prediction, and still disagrees -- and that disagreement is visible on an
# unlabelled filing, which is what makes it usable as a clue.
#
# It also reaches documents the keyword clue cannot: the lexicon is silent on roughly half the corpus
# and the broad model answers on all of it.

#' Mark where the detailed prediction and the broad prediction disagree
#'
#' @param .panel_det Panel for the detailed task.
#' @param .panel_broad Panel for the broad task.
#' @param .tab_prep Prepared sample, supplying the detailed-to-broad mapping.
#' @param .base_det Character. The detailed engine under test.
#' @param .base_broad Character. The broad engine supplying the second opinion. It need not be the
#'   same context length as the detailed one: 03B crowns per task, and on this study it crowned
#'   different lengths for the two levels.
#' @return .panel_det with BroadPred, DetailedParent and HierOK attached.
orch_add_hierarchy <- function(.panel_det, .panel_broad, .tab_prep,
                               .base_det = "Bert256", .base_broad = "Bert256") {
  if (FALSE) {
    .panel_det   <- panels$ClassDetailed
    .panel_broad <- panels$ClassBroad
    .tab_prep    <- tab_prep
    .base_det    <- "Bert256"
    .base_broad  <- "Bert512"
  }
  map_ <- .tab_prep |>
    dplyr::filter(!is.na(.data$ClassDetailed), !is.na(.data$ClassBroad)) |>
    dplyr::distinct(Child = .data$ClassDetailed, Parent = .data$ClassBroad)

  out_ <- .panel_det |>
    dplyr::mutate(BaseDet = .data[[.base_det]]) |>
    dplyr::left_join(map_ |> dplyr::rename(BaseDet = Child, DetailedParent = Parent),
                     by = dplyr::join_by(BaseDet)) |>
    dplyr::left_join(
      .panel_broad |> dplyr::transmute(DocID, BroadPred = .data[[.base_broad]]),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::mutate(HierOK = .data$DetailedParent == .data$BroadPred)

  n_miss_ <- sum(is.na(out_$HierOK))
  if (n_miss_ > 0L) {
    cli::cli_alert_warning(
      "{n_miss_} document{?s} have no hierarchy verdict -- a predicted category absent from the \\
       mapping, or absent from the broad panel. They are treated as consistent rather than dropped."
    )
  }
  out_ |> dplyr::mutate(HierOK = dplyr::coalesce(.data$HierOK, TRUE))
}

#' How reliable the detailed label is where the two models agree and where they do not
#' @param .tab Output of orch_add_hierarchy(), which carries BaseDet.
#' @return Tibble: Verdict, nDocs, Share, BaseAcc.
orch_hierarchy_split <- function(.tab) {
  if (FALSE) .tab <- panel_hier
  n_ <- nrow(.tab)
  .tab |>
    dplyr::mutate(Verdict = dplyr::if_else(.data$HierOK, "Broad model agrees",
                                           "Broad model disagrees")) |>
    dplyr::summarise(
      nDocs   = dplyr::n(),
      BaseAcc = mean(.data$BaseDet == .data$Truth),
      .by = Verdict
    ) |>
    dplyr::mutate(Share = .data$nDocs / n_) |>
    dplyr::arrange(dplyr::desc(.data$BaseAcc)) |>
    dplyr::select(Verdict, nDocs, Share, BaseAcc)
}

#' Report the hierarchy split
#' @param .tab Output of orch_hierarchy_split().
#' @return Invisibly .tab.
orch_report_hierarchy_split <- function(.tab) {
  if (FALSE) .tab <- hier_split
  tbl_head("Where the two transformers contradict each other")
  tbl_out(
    .tab    = .tab,                                            # numeric
    .title  = "Where the two transformers contradict each other",
    .groups = NULL,                                            # four columns need no spanning header
    .pct    = c("Share", "BaseAcc"),
    .notes  = c(
      Verdict = paste("Computed from two predictions and no labels, so a corpus pass can recompute",
                      "it for every filing. Unlike the lexicon it reaches every document rather than",
                      "the half a term fired on."),
      BaseAcc = "Accuracy of the DETAILED label, which is the one under test."
    ),
    .summary_row = NULL
  )
  invisible(.tab)
}

#' Swap in one arm on the documents where the two transformers disagree
#'
#' @param .tab Output of orch_add_hierarchy().
#' @param .engine Character. The arm swapped in.
#' @param .baseline Character. The engine that ships on the detailed task.
#' @return One row of orch_swap().
orch_resolve_hier <- function(.tab, .engine, .baseline = "Bert256") {
  if (FALSE) {
    .tab      <- panel_hier
    .engine   <- "Kw"
    .baseline <- "Bert256"
  }
  orch_swap(
    .tab      = .tab,
    .engine   = .engine,
    .subset   = !.tab$HierOK,
    .baseline = .baseline,
    .label    = "Broad model disagrees"
  )
}


# 8. Figures -----------------------------------------------------------------------------------------------------------
# The look comes entirely from _Commons/_Plots.R. What these own is the mapping from a result to a
# figure shape.

#' Per-category F1 for every engine
#'
#' Grouped rather than faceted, because the comparison is within a category and across engines, and a
#' facet puts the bars a reader is comparing on different panels.
#'
#' @param .tabs Named list of orch_perclass() outputs, one per engine.
#' @param .key Character. Registered vocabulary ordering the categories.
#' @return A ggplot.
orch_plot_perclass <- function(.tabs, .key = "ClassDetailed") {
  if (FALSE) {
    .tabs <- perclass_det
    .key  <- "ClassDetailed"
  }
  purrr::imap(.tabs, \(.t, .e) dplyr::mutate(.t, Engine = .e)) |>
    purrr::list_rbind() |>
    dplyr::filter(.data$Category != "All categories") |>
    dplyr::mutate(
      Category = plot_factor(.data$Category, .key = .key, .rev = TRUE),
      Engine   = plot_factor(.data$Engine, .key = "Engine")
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$F1, y = .data$Category, fill = .data$Engine)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75,
                      colour = .plot_ink, linewidth = 0.2) +
    plot_scale_fill_key(.key = "Engine") +
    ggplot2::scale_x_continuous(
      limits = c(0, 1), breaks = seq(0, 1, by = 0.25),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(x = "F1 (abstention counts as a miss)", y = NULL) +
    plot_theme(.grid = "x", .legend = "bottom")
}

#' The ceiling as engines are added
#' @param .tab Output of orch_ceiling().
#' @param .base Numeric. The transformer's own accuracy, drawn as the reference line.
#' @return A ggplot.
orch_plot_ceiling <- function(.tab, .base) {
  if (FALSE) {
    .tab  <- ceil_det
    .base <- 0.915
  }
  # The axis is zoomed to the region the steps occupy rather than fixed at zero to one: the whole
  # quantity of interest is a few percentage points and a full axis renders every step as one bar.
  lo_ <- min(.tab$Accuracy) - 0.02
  hi_ <- max(.tab$Accuracy) + 0.02

  .tab |>
    dplyr::mutate(Added = forcats::fct_inorder(plot_wrap(.data$Added, .width = 16L))) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Added, y = .data$Accuracy, group = 1)) +
    ggplot2::geom_hline(yintercept = .base, linetype = 2, linewidth = 0.3, colour = .plot_ref) +
    ggplot2::geom_step(linewidth = 0.5, colour = .plot_ink) +
    ggplot2::geom_point(size = 1.8, colour = .plot_ink) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::label_percent(accuracy = 0.1)(.data$Accuracy)),
      vjust = -0.9, size = (.plot_base - 3) / ggplot2::.pt, family = .plot_font
    ) +
    ggplot2::scale_y_continuous(limits = c(lo_, hi_), labels = scales::label_percent()) +
    ggplot2::labs(x = "Engine added to the oracle", y = "Ceiling accuracy") +
    plot_theme(.grid = "y", .legend = "none")
}

#' Change in corpus accuracy from swapping, against the confidence threshold
#'
#' Plotted as a DELTA rather than a level. The quantity of interest is a fraction of a percentage
#' point against a baseline above ninety, so on an accuracy axis every line is flat and identical.
#' Zero is the transformer, above it is better, below it is worse, and the axis is in percentage
#' points so nothing is exaggerated by the rescaling.
#'
#' @param .tab Rows of orch_swap() carrying an Engine column, across thresholds.
#' @return A ggplot.
orch_plot_resolve <- function(.tab) {
  if (FALSE) .tab <- resolve_det
  .tab |>
    dplyr::mutate(
      Threshold = as.numeric(sub("^P < ", "", .data$Subset)),
      Engine    = plot_factor(.data$Engine, .key = "Engine")
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Threshold, y = 100 * .data$DeltaOverall,
                                 colour = .data$Engine)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3, colour = .plot_ref) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 1.6) +
    plot_scale_colour_key(.key = "Engine") +
    ggplot2::labs(x = "Swap applied where the transformer scores below", y = "Change in corpus accuracy (pp)") +
    plot_theme(.grid = "y", .legend = "bottom")
}
