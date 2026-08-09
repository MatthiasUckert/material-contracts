# 03E-ClassifyOrchestrate: routing among the classification arms (orch_*) ----------------------------------------------
#
# WHAT THIS FILE DOES
# Four arms now label the same documents on the same folds: a transformer, a keyword table, a local
# language model, and whatever derived combinations of those the search constructs. This file asks
# whether committing to one of them conditionally -- routing a document to a second arm when the
# first is unsure -- beats committing to the best single arm outright, and answers it with a
# procedure whose result can be obtained by running the procedure.
#
# THE DISTINCTION THE WHOLE DOCUMENT TURNS ON
# A policy selected on the documents it is then scored on is not an estimate of anything. Selection
# and scoring are therefore nested: a policy is crowned on four folds and scored on the fifth, five
# times over. The in-sample number is still computed and still reported, because a reader will want
# to know the size of the gap, but it is drawn hollow in every figure and labelled as unobtainable.
# Fill in this document means "a number you could actually get".
#
# THE ROUTING NULL IS STRUCTURALLY STRONG
# The gate fires where the second arm is confident, and the second arm is confident where the first
# already is. Cascade gains are therefore elusive by construction rather than by accident, and a
# result showing no gain is a finding about the arms rather than a failure of the search.
#
# WHAT IS NOT HERE
# The arms themselves: 03A owns the folds, the scoring layer and the category vocabulary; 03B, 03C
# and 03D own the transformer, the keyword table and the language model. This document reads their
# run folders and never retrains anything. The look of any figure or table is _Commons/_Plots.R and
# _Commons/_Tables.R; nothing below sets a colour, a font or a height.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; if (FALSE) dev blocks; pure ASCII; parenthesised cli interpolation.

if (FALSE) {
  .runs_roots <- c(.lP$Runs$Bert, .lP$Runs$Kw)
  .label_col  <- "ClassDetailed"
  .tab_prep   <- arrow::read_parquet(.lP$Input$Prepared)
}

# The abstention sentinel, shared with the keyword and language-model arms. An arm that declines emits
# it; the scoring layer drops it from the class set and reports coverage instead.
ORCH_NONE <- "(none)"


# 0. Vocabulary ----------------------------------------------------------------------------------------------------------
# One vocabulary this document owns, registered with the design layer the same way 03A registers the
# taxonomy. Agreement tiers are ordered by how much evidence stands behind a label, strongest first,
# because that is the order a reader scans them in and the order in which their accuracies should
# decline. The order was previously written out at each of the two places the tier is constructed;
# stating it once removes the possibility of the figure and the flag disagreeing about what "sole"
# ranks above.

.orch_tiers <- c("unanimous", "majority", "sole", "split")

plot_register_levels(
  .key     = "AgreementTier",
  .levels  = .orch_tiers,
  .short   = NULL,
  .colours = plot_pal_seq(length(.orch_tiers), .rev = TRUE)
)


# 1. Arm naming --------------------------------------------------------------------------------------------------------
# Run identifiers are eighty characters because they must be unique on disk. A name has a different
# job: it must be readable in a console table and in a policy label, and it must still separate two
# arms that differ. Fixed truncation cannot do both -- an earlier version cut the identifier at a
# fixed field and silently collapsed two genuinely different arms into one, whereupon the better of
# them was kept and the other discarded without a word.

#' Short, unique display names for a set of arms
#'
#' Builds a base name from the method, then adds the SMALLEST set of distinguishing fields needed to
#' make names unique within that base. A single transformer configuration is therefore just
#' "legal-bert"; two keyword lists on the same source become "kw-text:mined" and "kw-text:cowork".
#' Candidate fields are tried in order of how much they mean to a reader, so the disambiguator names
#' the thing that actually differs rather than the first field that happens to vary.
#'
#' @param .tab Tibble with at least Kind, plus any of TermsTag, Stopwords, NWords, NgramMax,
#'   MinReach, MaxTerms, Tau, MaxLen, Epochs, LR, ClassWeights.
#' @return Character vector of arm names, unique by construction.
orch_arm_name <- function(.tab) {
  if (FALSE) .tab <- meta_

  base_ <- dplyr::case_when(
    grepl("^keyword-", .tab$Kind) ~ paste0("kw-", sub("^keyword-", "", .tab$Kind)),
    # LLM run identifiers already strip punctuation from the model tag, so the name is readable as
    # written and only needs the family prefix kept.
    grepl("^llm-", .tab$Kind)     ~ .tab$Kind,
    TRUE                          ~ clf_model_short(.model = .tab$Kind)
  )
  base_ <- sub("^kw-docdesc$", "kw-desc", base_)

  # Ordered by how much a level means to a reader, because the loop below stops as soon as names are
  # unique and therefore names an arm by whichever listed axis separates it first.
  cand_ <- c("TermsTag", "Tier", "Shots", "Guidance", "Stopwords", "NWords", "NgramMax",
             "MinReach", "MaxTerms", "Tau", "NChars", "AllowAbstain", "Think",
             "MaxLen", "Epochs", "LR", "ClassWeights")
  cand_ <- intersect(cand_, names(.tab))
  abbr_ <- c(TermsTag = "", Tier = "", Shots = "S", Guidance = "G", Stopwords = "SW", NWords = "W",
             NgramMax = "N", MinReach = "R", MaxTerms = "M", Tau = "T", NChars = "C",
             AllowAbstain = "A", Think = "R", MaxLen = "L", Epochs = "E", LR = "LR",
             ClassWeights = "CW")

  out_ <- base_
  for (grp_ in unique(base_)) {
    idx_ <- which(base_ == grp_)
    if (length(idx_) <= 1L) next

    tag_  <- rep("", length(idx_))
    used_ <- character(0)
    for (f_ in cand_) {
      if (length(unique(paste0(tag_, "|", seq_along(idx_)))) == length(idx_) &&
          length(unique(tag_)) == length(idx_)) break
      vals_ <- .tab[[f_]][idx_]
      if (dplyr::n_distinct(vals_) <= 1L) next
      used_ <- c(used_, f_)
      tag_  <- paste0(tag_, dplyr::if_else(tag_ == "", "", "_"),
                      abbr_[[f_]], orch_compact(.x = vals_))
      if (length(unique(tag_)) == length(idx_)) break
    }
    # Nothing separated them; fall back to a positional suffix rather than silently colliding.
    if (length(unique(tag_)) < length(idx_)) tag_ <- paste0(tag_, "#", seq_along(idx_))
    out_[idx_] <- paste0(grp_, ":", tag_)
  }
  out_
}

#' Compact a vector of field values into short name tokens
#' @param .x Vector of any type.
#' @return Character vector of short tokens.
orch_compact <- function(.x) {
  if (FALSE) .x <- c(0.05, 0.01)
  if (is.logical(.x)) return(dplyr::if_else(.x, "1", "0"))
  if (is.numeric(.x)) {
    return(sub("\\.?0+$", "", formatC(.x, format = "f", digits = 3, drop0trailing = TRUE)))
  }
  substr(as.character(.x), 1L, 12L)
}


# 2. Arm inventory -----------------------------------------------------------------------------------------------------

#' Every arm's out-of-fold predictions, read from the run tree in one pass
#'
#' The shared pooling helper reads every predictions file under the roots and then keeps the rows of
#' one configuration. Called once that is the cheapest thing that works; called once per arm it reads
#' the whole tree once per arm, and binds several hundred thousand rows only to discard all but a few
#' thousand of them. With a few hundred runs on disk and a handful of arms that is the dominant cost
#' of this document -- minutes of file reading to assemble a table that fits in memory many times
#' over.
#'
#' This reads each file exactly once and filters it as it goes, so the cost is linear in the number of
#' runs rather than in runs times arms, and nothing is ever bound that will not be kept.
#'
#' @param .runs_roots Character vector of run directories.
#' @param .config_names Character vector of ConfigName strings to retain.
#' @return Long tibble of predictions for those configurations only.
orch_predictions <- function(.runs_roots, .config_names) {
  if (FALSE) {
    .runs_roots   <- c(.lP$Runs$Bert, .lP$Runs$Kw)
    .config_names <- res_det$ArmsCV$ConfigName
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

  want_ <- unique(.config_names)
  out_  <- purrr::map(paths_, function(.p) {
    tab_ <- arrow::read_parquet(.p)
    tab_[tab_$ConfigName %in% want_, , drop = FALSE]
  }) |>
    purrr::list_rbind()

  got_  <- unique(out_$ConfigName)
  miss_ <- setdiff(want_, got_)
  if (length(miss_) > 0L) {
    cli::cli_alert_warning("No predictions found for {length(miss_)} configuration{?s}: {miss_}")
  }
  cli::cli_alert_info(
    "Read {length(paths_)} prediction file{?s} once; kept {nrow(out_)} row{?s} across \\
     {length(got_)} configuration{?s}."
  )
  out_
}


#' Inventory the arms available for one task
#'
#' Scans the run folders written by 03B and 03C and returns one row per arm, carrying the folds it
#' covers, the share of documents it commits to, and its precision on the documents it committed to.
#'
#' 03B writes a run folder for every configuration in its sweep, so dozens of transformer
#' configurations exist for one task while 03C writes only its operating point. Inventorying every
#' run would put sixty near-identical transformer arms into a search that is not about
#' hyperparameters. Configurations sharing an arm name are therefore collapsed to the best of them by
#' mean macro-F1, the quantity that crowned a configuration upstream, and `nConfigs` records how many
#' each arm stood for.
#'
#' Selective precision is reported rather than plain accuracy, because an abstaining arm's accuracy
#' over the whole sample confounds how often it is right with how often it declines to answer, and
#' only the first bears on whether its commitments are worth honouring.
#'
#' @param .runs_roots Character vector of runs directories (BERT root, keyword root).
#' @param .label_col Task to inventory.
#' @param .none Character. Abstention sentinel.
#' @return Tibble: Arm, ConfigName, Kind, Source, Origin, TermsFile, nConfigs, nFolds, Folds, nDocs,
#'   Coverage, SelPrecision, Accuracy, MacroF1.
orch_arms <- function(.runs_roots, .label_col = "ClassDetailed", .none = ORCH_NONE) {
  if (FALSE) {
    .runs_roots <- c(.lP$Runs$Bert, .lP$Runs$Kw)
    .label_col  <- "ClassDetailed"
    .none       <- ORCH_NONE
  }
  overall_ <- clf_load_overall(.runs_roots = .runs_roots) |>
    dplyr::filter(.data$LabelCol == .label_col, !.data$Smoke)
  if (nrow(overall_) == 0L) cli::cli_abort("No runs for {(.label_col)} under the given roots.")

  # Only the columns that exist across the engines. Keyword runs carry selection axes, transformer
  # runs hyperparameters, LLM runs prompt axes, and each is NA on the others' rows. Every axis any
  # engine sweeps has to be listed: an axis omitted here is invisible to the namer below, so two
  # configurations differing only on it would collapse to one arm and the lower-scoring one would be
  # dropped without a word. That is how a four-configuration LLM sweep becomes one arm.
  keep_ <- intersect(
    c("Source", "Stopwords", "NWords", "NgramMax", "MinReach", "MaxTerms", "Tau", "Origin",
      "TermsFile", "MaxLen", "Epochs", "LR", "ClassWeights",
      "Tier", "Guidance", "Shots", "NChars", "AllowAbstain", "Think"),
    names(overall_)
  )

  meta_ <- overall_ |>
    dplyr::summarise(
      Kind    = dplyr::first(.data$Model),
      nFolds  = dplyr::n_distinct(.data$TestFold),
      Folds   = paste(sort(unique(.data$TestFold)), collapse = ","),
      MacroF1 = mean(.data$F1_macro),
      dplyr::across(dplyr::all_of(keep_), dplyr::first),
      .by = ConfigName
    ) |>
    dplyr::mutate(
      Source   = if ("Source" %in% keep_) .data$Source else NA_character_,
      Origin   = if ("Origin" %in% keep_) dplyr::coalesce(.data$Origin, "mined") else "mined",
      TermsTag = orch_terms_tag(
        .terms_file = if ("TermsFile" %in% keep_) .data$TermsFile else NA_character_
      )
    )
  meta_$Arm <- orch_arm_name(.tab = meta_)

  # One configuration per arm name, chosen the way every other configuration in this study was.
  # Ties break toward wider fold coverage: an arm on five folds can enter the nested search and an
  # otherwise equal arm on one fold cannot.
  best_ <- meta_ |>
    dplyr::arrange(dplyr::desc(.data$MacroF1), dplyr::desc(.data$nFolds)) |>
    dplyr::mutate(nConfigs = dplyr::n(), .by = Arm) |>
    dplyr::slice(1, .by = Arm)

  n_drop_ <- nrow(meta_) - nrow(best_)
  if (n_drop_ > 0L) {
    cli::cli_alert_info(
      "{(.label_col)}: {nrow(meta_)} configuration{?s} collapsed to {nrow(best_)} arm{?s}; \\
       {n_drop_} lower-scoring sibling{?s} dropped."
    )
  }

  # Coverage and precision for every surviving configuration, from one pass over the run tree rather
  # than one pass per configuration. Reading per configuration is what the shared pooling helper does,
  # and at this many runs it turns a few seconds of file access into minutes of it.
  stats_ <- orch_predictions(.runs_roots = .runs_roots, .config_names = best_$ConfigName) |>
    dplyr::mutate(Hit = .data$PredLabel != .none) |>
    dplyr::summarise(
      nDocs        = dplyr::n(),
      Coverage     = mean(.data$Hit),
      SelPrecision = if (any(.data$Hit)) {
        mean(.data$PredLabel[.data$Hit] == .data$TrueLabel[.data$Hit])
      } else {
        NA_real_
      },
      Accuracy     = mean(.data$PredLabel == .data$TrueLabel),
      .by = ConfigName
    )

  best_ |>
    dplyr::left_join(stats_, by = dplyr::join_by(ConfigName)) |>
    dplyr::select(Arm, ConfigName, Kind, dplyr::any_of(c("Source", "Origin", "TermsFile")),
                  nConfigs, nFolds, Folds, nDocs, Coverage, SelPrecision, Accuracy, MacroF1) |>
    dplyr::arrange(dplyr::desc(.data$Coverage), dplyr::desc(.data$MacroF1))
}

#' Short provenance tag for a term list, or "mined" where none was supplied
#'
#' The stem's first token separates the lists this study uses (cowork, union) without carrying the
#' path, which no console table has room for. The inventory prints the full TermsFile beside it.
#'
#' @param .terms_file Character vector of paths, possibly NA.
#' @return Character vector of tags.
orch_terms_tag <- function(.terms_file) {
  if (FALSE) .terms_file <- c(NA, "/x/cowork_terms_detailed.parquet", "/x/union_terms_detailed.parquet")
  stem_ <- fs::path_ext_remove(fs::path_file(dplyr::coalesce(.terms_file, "mined")))
  dplyr::if_else(is.na(.terms_file), "mined", sub("_.*$", "", stem_))
}

#' Report the arm inventory and say which arms the nested search may use
#'
#' @param .tab Output of orch_arms().
#' @param .n_folds Integer. Folds an arm must cover to enter the nested search.
#' @param .label_col Task, used in the heading.
#' @return Invisibly .tab.
orch_report_arms <- function(.tab, .n_folds = 5L, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab       <- res_det$Arms
    .n_folds   <- 5L
    .label_col <- "ClassDetailed"
  }
  cli::cli_h2("Arms available: {(.label_col)}")
  .tab |>
    dplyr::mutate(
      Coverage     = tbl_pct(.data$Coverage),
      SelPrecision = tbl_pct(.data$SelPrecision),
      MacroF1      = sprintf("%.3f", .data$MacroF1)
    ) |>
    dplyr::select(Arm, dplyr::any_of("Origin"), nConfigs, Folds, nDocs, Coverage, SelPrecision,
                  MacroF1) |>
    tbl_say()

  n_ok_ <- sum(.tab$nFolds >= .n_folds)
  cli::cli_text("")
  cli::cli_alert_info(
    "{n_ok_} of {nrow(.tab)} arm{?s} cover all {(.n_folds)} folds and may enter the nested search; \\
     the rest are held back for the bounded check."
  )
  cli::cli_alert_info(
    "Coverage below 100% means the arm abstains. It cannot change a routed prediction where it \\
     abstains, so coverage caps its influence whatever its precision."
  )
  invisible(.tab)
}


# 3. The routing spine -------------------------------------------------------------------------------------------------

#' Assemble the long routing spine: one row per document and arm
#'
#' Every policy is evaluated against this table, so it is built once. Long rather than wide, because
#' arms are added and removed between tasks and tiers and a wide table would change shape each time.
#'
#' `Score` is deliberately not comparable across arm kinds. For the transformer it is the top-1
#' softmax probability; for a keyword arm it is Power, the Wilson lower bound on the firing term's
#' training precision. A floor is therefore applied within an arm and never used to rank one arm
#' against another, which is why a cascade is an explicit ORDER rather than a score comparison.
#'
#' `.commit_only` exists for the binary task. The keyword engine mines the positive class only and
#' labels everything else "Original", which is the absence of evidence rather than a commitment.
#' Naming the labels that count as commitments turns that arm back into an abstaining one. It applies
#' to keyword arms alone: the transformer has no such asymmetry, and silencing its negative class
#' would leave the cascade with nothing to terminate on.
#'
#' @param .runs_roots Character vector of runs directories.
#' @param .arms Tibble of arms to include (rows of orch_arms()).
#' @param .tab_prep Prepared sample, supplying the carried document columns.
#' @param .commit_only Character vector of labels a keyword arm may commit to, or NULL for all.
#' @param .carry Document-level columns to attach for downstream scoring slices.
#' @param .none Character. Abstention sentinel.
#' @return Tibble: DocID, Fold, TrueLabel, [carried columns], Arm, Pred, Score.
#' The routing spine: every arm's prediction for every document, in long form
#'
#' One row per document per arm, carrying the arm's label, its confidence and whether it committed.
#' This is the table every policy is fitted and scored against, so it is built once per task and never
#' rebuilt.
#'
#' @param .runs_roots Character vector of run directories.
#' @param .arms Arm inventory from orch_arms(), already restricted to arms with full fold coverage.
#' @param .tab_prep Prepared sample, supplying the columns the robustness slices carry.
#' @param .commit_only Character vector or NULL. Restrict a lexical arm to committing on these
#'   categories only.
#' @param .carry Character vector of prepared-sample columns to attach.
#' @param .none Character. Abstention sentinel.
#' @return Long tibble: DocID, Fold, TrueLabel, carried columns, Arm, Pred, Score.
orch_spine <- function(.runs_roots, .arms, .tab_prep, .commit_only = NULL,
                       .carry = c("ClassDetailed2", "LabelRound"), .none = ORCH_NONE) {
  if (FALSE) {
    .runs_roots  <- c(.lP$Runs$Bert, .lP$Runs$Kw)
    .arms        <- res_det$ArmsCV
    .tab_prep    <- tab_prep
    .commit_only <- NULL
    .carry       <- c("ClassDetailed2", "LabelRound")
    .none        <- ORCH_NONE
  }
  # ConfigName is unique within the arm inventory: orch_arms() keeps one configuration per arm name,
  # so this join attaches exactly one arm and one kind to each prediction row.
  key_ <- .arms |> dplyr::select(ConfigName, Arm, Kind)

  out_ <- orch_predictions(.runs_roots = .runs_roots, .config_names = key_$ConfigName) |>
    dplyr::inner_join(key_, by = dplyr::join_by(ConfigName)) |>
    dplyr::transmute(
      DocID,
      Fold  = as.integer(.data$Fold),
      TrueLabel,
      Arm   = .data$Arm,
      Kind  = .data$Kind,
      Pred  = .data$PredLabel,
      Score = dplyr::coalesce(as.numeric(.data$Score), 0)
    )

  if (!is.null(.commit_only)) {
    out_ <- out_ |>
      dplyr::mutate(
        Pred = dplyr::if_else(
          grepl("^keyword-", .data$Kind) & !.data$Pred %in% .commit_only, .none, .data$Pred
        )
      )
  }

  doc_ <- .tab_prep |> dplyr::select(DocID, dplyr::any_of(.carry))

  out_ |>
    dplyr::select(-Kind) |>
    dplyr::left_join(doc_, by = dplyr::join_by(DocID)) |>
    dplyr::relocate(DocID, Fold, TrueLabel, dplyr::any_of(.carry), Arm, Pred, Score)
}

#' Add a derived arm to an existing spine
#'
#' Derived arms are built in R rather than read from a run folder, so they arrive without the
#' document-level columns the spine carries. This attaches them from the rows already present,
#' keeping one code path for scoring however an arm was produced.
#'
#' @param .spine Output of orch_spine().
#' @param .arm_rows Tibble: DocID, Fold, TrueLabel, Arm, Pred, Score.
#' @param .carry Document-level columns to attach.
#' @return The spine with the derived arm bound on.
orch_spine_add <- function(.spine, .arm_rows, .carry = c("ClassDetailed2", "LabelRound")) {
  if (FALSE) {
    .spine    <- spine_det
    .arm_rows <- arm_hier
    .carry    <- c("ClassDetailed2", "LabelRound")
  }
  if (is.null(.arm_rows) || nrow(.arm_rows) == 0L) return(.spine)
  doc_ <- .spine |>
    dplyr::select(DocID, dplyr::any_of(.carry)) |>
    dplyr::distinct()
  dplyr::bind_rows(.spine, .arm_rows |> dplyr::left_join(doc_, by = dplyr::join_by(DocID)))
}

#' Roles read off the spine rather than off the inventory
#'
#' An arm's role depends on what it does after `.commit_only` has been applied and after derived arms
#' have been added, so it is a property of the spine and is computed there. Deriving it twice, once
#' per source, is how the two would come to disagree.
#'
#' @param .spine Output of orch_spine().
#' @param .none Character. Abstention sentinel.
#' @return Tibble: Arm, nDocs, nFolds, Coverage, Accuracy, Role ("terminal" / "gated" / "silent").
orch_roles <- function(.spine, .none = ORCH_NONE) {
  if (FALSE) .spine <- spine_det
  .spine |>
    dplyr::summarise(
      nDocs    = dplyr::n_distinct(.data$DocID),
      nRows    = dplyr::n(),
      nFolds   = dplyr::n_distinct(.data$Fold),
      Coverage = mean(.data$Pred != .none),
      Accuracy = mean(.data$Pred == .data$TrueLabel),
      .by = Arm
    ) |>
    dplyr::mutate(
      Role = dplyr::case_when(
        .data$Coverage >= 1 ~ "terminal",
        .data$Coverage >  0 ~ "gated",
        TRUE                ~ "silent"
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$Coverage), dplyr::desc(.data$Accuracy))
}

#' Report the spine and the roles it implies
#' @param .spine Output of orch_spine().
#' @param .none Character. Abstention sentinel.
#' @return Invisibly the roles tibble.
orch_report_spine <- function(.spine, .none = ORCH_NONE) {
  if (FALSE) .spine <- spine_det
  n_docs_ <- dplyr::n_distinct(.spine$DocID)
  out_ <- orch_roles(.spine = .spine, .none = .none)

  cli::cli_h2("Routing spine")
  out_ |>
    dplyr::mutate(Coverage = tbl_pct(.data$Coverage), Accuracy = tbl_pct(.data$Accuracy)) |>
    dplyr::select(Arm, Role, nRows, nDocs, nFolds, Coverage, Accuracy) |>
    tbl_say()
  cli::cli_text("")
  cli::cli_alert_info(
    "Every arm should reach {n_docs_} documents once. nRows above nDocs means one arm name covers \\
     two configurations; fewer documents than that is a sweep gap upstream."
  )
  if (!any(out_$Role == "terminal")) {
    cli::cli_alert_danger("No terminal arm: a cascade has nothing to fall through to.")
  }
  invisible(out_)
}


# 4. Derived arms ------------------------------------------------------------------------------------------------------
# Arms built in R from what the transformer already predicted. Each encodes a structural claim about
# the taxonomy and is scored by exactly the same code as an arm read off disk, so it wins a fold on
# its merits or it does not.

#' Detailed-to-broad mapping taken from the labelled sample
#' @param .tab_prep Prepared sample (needs ClassDetailed, ClassBroad).
#' @return Tibble: Child, Parent.
orch_hierarchy_map <- function(.tab_prep) {
  if (FALSE) .tab_prep <- tab_prep
  .tab_prep |>
    dplyr::filter(!is.na(.data$ClassDetailed), !is.na(.data$ClassBroad)) |>
    dplyr::distinct(Child = .data$ClassDetailed, Parent = .data$ClassBroad)
}

#' Where detailed errors fall relative to the broad parent
#'
#' Stated before the constrained arm is scored, because it bounds what the constraint can buy. An
#' error keeping the correct broad parent is one the broad prediction was already right about, so
#' constraining the detailed decision cannot touch it. Only errors CROSSING a parent are reachable,
#' and only the subset of those where the broad model is right.
#'
#' @param .tab_pred Pooled detailed predictions (DocID, TrueLabel, PredLabel).
#' @param .tab_prep Prepared sample, supplying the mapping.
#' @return Tibble: Outcome, nDocs, Share.
orch_hierarchy_errors <- function(.tab_pred, .tab_prep) {
  if (FALSE) {
    .tab_pred <- pred_det
    .tab_prep <- tab_prep
  }
  map_ <- orch_hierarchy_map(.tab_prep = .tab_prep)
  n_   <- nrow(.tab_pred)

  .tab_pred |>
    dplyr::left_join(map_ |> dplyr::rename(TrueLabel = Child, TrueParent = Parent),
                     by = dplyr::join_by(TrueLabel)) |>
    dplyr::left_join(map_ |> dplyr::rename(PredLabel = Child, PredParent = Parent),
                     by = dplyr::join_by(PredLabel)) |>
    dplyr::mutate(
      Outcome = dplyr::case_when(
        .data$PredLabel  == .data$TrueLabel  ~ "Correct",
        .data$PredParent == .data$TrueParent ~ "Wrong, same broad parent",
        TRUE                                 ~ "Wrong, different broad parent"
      )
    ) |>
    dplyr::summarise(nDocs = dplyr::n(), Share = dplyr::n() / n_, .by = Outcome) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' Report the hierarchy diagnostic
#' @param .tab Output of orch_hierarchy_errors().
#' @return Invisibly .tab.
orch_report_hierarchy <- function(.tab) {
  if (FALSE) .tab <- hier_err
  cli::cli_h2("Where detailed errors fall")
  .tab |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say()
  cli::cli_text("")
  reach_ <- .tab$Share[.tab$Outcome == "Wrong, different broad parent"]
  reach_ <- if (length(reach_) == 0L) 0 else reach_
  cli::cli_alert_info(
    "Only the crossing row is reachable by a broad-level constraint, and only where the broad model \\
     is right. It bounds the constrained arm's possible gain at {tbl_pct(reach_)} of documents, \\
     against which the broad model's own error rate must be set."
  )
  invisible(.tab)
}

#' Detailed prediction constrained by the broad prediction, as a routable arm
#'
#' Restricts the detailed probability vector to the children of the predicted broad parent and takes
#' the restricted argmax. 03B already guarantees a CONSISTENT published hierarchy by deriving the
#' broad label from the detailed prediction, so what is at stake here is accuracy, not consistency:
#' whether the easier seven-category problem is solved reliably enough to be worth imposing on the
#' harder twelve-category one. It gains where the broad model is right and the unconstrained detailed
#' model would have crossed a parent, and loses wherever the broad model is wrong.
#'
#' @param .class_detailed Per-document detailed classification from bert_classification(), carrying
#'   the P_<Class> probability columns.
#' @param .pred_broad Pooled broad predictions (DocID, PredLabel).
#' @param .tab_prep Prepared sample, supplying the mapping.
#' @param .arm Character. Name to give the resulting arm.
#' @return Tibble in spine form: DocID, Fold, TrueLabel, Arm, Pred, Score.
orch_arm_hierarchical <- function(.class_detailed, .pred_broad, .tab_prep, .arm = "bert-hier") {
  if (FALSE) {
    .class_detailed <- class_det
    .pred_broad     <- pred_broad
    .tab_prep       <- tab_prep
    .arm            <- "bert-hier"
  }
  map_   <- orch_hierarchy_map(.tab_prep = .tab_prep)
  pcols_ <- grep("^P_", names(.class_detailed), value = TRUE)
  if (length(pcols_) == 0L) {
    cli::cli_abort("No P_<Class> columns; pass the table from bert_classification().")
  }

  out_ <- .class_detailed |>
    dplyr::select(DocID, Fold, TrueLabel, dplyr::all_of(pcols_)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(pcols_), names_to = "Child", values_to = "Prob") |>
    dplyr::mutate(Child = sub("^P_", "", .data$Child)) |>
    dplyr::inner_join(map_, by = dplyr::join_by(Child)) |>
    dplyr::inner_join(.pred_broad |> dplyr::select(DocID, BroadPred = .data$PredLabel),
                      by = dplyr::join_by(DocID)) |>
    dplyr::filter(.data$Parent == .data$BroadPred) |>
    dplyr::slice_max(.data$Prob, n = 1L, by = DocID, with_ties = FALSE) |>
    dplyr::transmute(DocID, Fold = as.integer(.data$Fold), TrueLabel,
                     Arm = .arm, Pred = .data$Child, Score = .data$Prob)

  n_miss_ <- dplyr::n_distinct(.class_detailed$DocID) - nrow(out_)
  if (n_miss_ > 0L) {
    cli::cli_alert_warning(
      "{n_miss_} document{?s} lost building {(.arm)} -- a broad prediction with no children in the \\
       mapping. Check the mapping rather than accepting the loss."
    )
  }
  out_
}

#' Detailed prediction rolled up to the broad taxonomy, as a routable arm
#'
#' The mirror of the constrained arm, for the broad task. 03B deploys exactly this route -- the
#' published broad label is the detailed prediction's parent -- so putting it in the broad search
#' makes the deployed route an arm to beat rather than an assumption to inherit.
#'
#' @param .pred_detailed Pooled detailed predictions (DocID, Fold, TrueLabel, PredLabel, Score).
#' @param .tab_prep Prepared sample, supplying the mapping.
#' @param .arm Character. Name to give the resulting arm.
#' @return Tibble in spine form: DocID, Fold, TrueLabel, Arm, Pred, Score.
orch_arm_rollup <- function(.pred_detailed, .tab_prep, .arm = "bert-rollup") {
  if (FALSE) {
    .pred_detailed <- pred_det
    .tab_prep      <- tab_prep
    .arm           <- "bert-rollup"
  }
  map_ <- orch_hierarchy_map(.tab_prep = .tab_prep)

  .pred_detailed |>
    dplyr::left_join(map_ |> dplyr::rename(TrueLabel = Child, TrueParent = Parent),
                     by = dplyr::join_by(TrueLabel)) |>
    dplyr::left_join(map_ |> dplyr::rename(PredLabel = Child, PredParent = Parent),
                     by = dplyr::join_by(PredLabel)) |>
    dplyr::filter(!is.na(.data$TrueParent), !is.na(.data$PredParent)) |>
    dplyr::transmute(
      DocID, Fold = as.integer(.data$Fold), TrueLabel = .data$TrueParent,
      Arm = .arm, Pred = .data$PredParent,
      Score = dplyr::coalesce(as.numeric(.data$Score), 1)
    )
}


# 5. The policy space --------------------------------------------------------------------------------------------------

#' All permutations of a short character vector
#'
#' Written out rather than pulled from a package: cascade depth is capped at a small number, so a
#' recursive enumeration is a few lines and avoids a dependency for one call.
#'
#' @param .x Character vector.
#' @return Matrix, one permutation per row.
orch_permutations <- function(.x) {
  if (FALSE) .x <- c("kw-text", "kw-desc")
  n_ <- length(.x)
  if (n_ <= 1L) return(matrix(.x, nrow = 1L))
  purrr::map(seq_len(n_), function(.i) cbind(.x[.i], orch_permutations(.x = .x[-.i]))) |>
    (\(.l) do.call(rbind, .l))()
}

#' Human-readable policy label
#' @param .order List column of arm-order character vectors.
#' @param .terminal,.floor,.family Vectors of the remaining policy fields.
#' @return Character vector, e.g. "kw-desc > kw-text > legal-bert (P>=0.80, perclass)".
orch_policy_label <- function(.order, .terminal, .floor, .family) {
  if (FALSE) {
    .order    <- list(character(0), c("kw-desc", "kw-text"))
    .terminal <- c("legal-bert", "legal-bert")
    .floor    <- c(0, 0.80)
    .family   <- c("cascade", "perclass")
  }
  chain_ <- purrr::map2_chr(.order, .terminal, function(.o, .t) {
    if (length(.o) == 0L) .t else paste(c(.o, .t), collapse = " > ")
  })
  gate_ <- dplyr::if_else(
    purrr::map_int(.order, length) == 0L,
    "",
    paste0(" (P>=", formatC(.floor, format = "f", digits = 2), ", ", .family, ")")
  )
  paste0(chain_, gate_)
}

#' Stamp identifiers, depth and labels onto an assembled policy table
#' @param .tab Policy rows carrying Family, Terminal, Floor and Order.
#' @return Tibble: PolicyID, Family, Terminal, Floor, Depth, Order, Label.
orch_policy_finalise <- function(.tab) {
  if (FALSE) {
    .tab <- tibble::tibble(Family = "cascade", Terminal = "legal-bert", Floor = 0,
                           Order = list(character(0)))
  }
  .tab |>
    dplyr::mutate(
      Depth    = purrr::map_int(.data$Order, length),
      PolicyID = dplyr::row_number(),
      Label    = orch_policy_label(.order = .data$Order, .terminal = .data$Terminal,
                                   .floor = .data$Floor, .family = .data$Family)
    ) |>
    dplyr::relocate(PolicyID, Family, Terminal, Floor, Depth, Order, Label)
}

#' Enumerate the policies to search over
#'
#' The depth cap limits how hard the sample is searched. Ordered subsets grow factorially while the
#' evidence does not, and two arms deep already covers every substantively different cascade -- one
#' cheap arm first, one second, transformer last.
#'
#' @param .arms_gated Character vector of abstaining arm names.
#' @param .terminals Character vector of always-committing arm names.
#' @param .floors Numeric vector of score floors to cross in.
#' @param .max_depth Integer. Longest cascade of abstaining arms.
#' @param .families Character vector of families ("cascade", "perclass").
#' @return Tibble: PolicyID, Family, Terminal, Floor, Depth, Order, Label.
orch_policy_grid <- function(.arms_gated, .terminals, .floors = c(0, 0.70, 0.80, 0.90),
                             .max_depth = 2L, .families = c("cascade", "perclass")) {
  if (FALSE) {
    .arms_gated <- c("kw-text", "kw-desc")
    .terminals  <- c("legal-bert", "bert-hier")
    .floors     <- c(0, 0.70, 0.80, 0.90)
    .max_depth  <- 2L
    .families   <- c("cascade", "perclass")
  }
  if (length(.terminals) == 0L) cli::cli_abort("A policy needs at least one always-committing arm.")

  # The terminal alone is a policy: it is the incumbent, and it has to be inside the search, or the
  # nested loop can never conclude that routing is not worth doing.
  bare_ <- tibble::tibble(
    Family   = "cascade",
    Terminal = .terminals,
    Floor    = 0,
    Order    = list(character(0))
  )
  if (length(.arms_gated) == 0L) {
    cli::cli_alert_warning("No abstaining arm to gate; the grid holds the terminals alone.")
    return(orch_policy_finalise(.tab = bare_))
  }

  orders_ <- purrr::map(seq_len(min(.max_depth, length(.arms_gated))), function(.d) {
    utils::combn(.arms_gated, .d, simplify = FALSE) |>
      purrr::map(function(.set) {
        perms_ <- orch_permutations(.x = .set)
        purrr::map(seq_len(nrow(perms_)), \(.i) as.character(perms_[.i, ]))
      }) |>
      purrr::list_flatten()
  }) |>
    purrr::list_flatten()

  gated_ <- tidyr::expand_grid(
    Family   = .families,
    Terminal = .terminals,
    Floor    = .floors,
    Order    = orders_
  )
  orch_policy_finalise(.tab = dplyr::bind_rows(bare_, gated_))
}


# 6. Fitting and applying a policy -------------------------------------------------------------------------------------

#' Fit the per-class gate for one policy on the training folds
#'
#' For the "perclass" family, an arm's commitment to a class is honoured only where that arm's
#' precision on the class beat the terminal's accuracy on the SAME documents, measured on the
#' training folds. Comparing against the terminal on the same documents rather than against its
#' global accuracy is the point: an arm commits on easy documents, where the terminal is already
#' close to perfect, so a gate calibrated against the global number would admit arms that lose on
#' every document they actually touch.
#'
#' @param .spine Training rows of the routing spine.
#' @param .policy One row of orch_policy_grid().
#' @param .none Character. Abstention sentinel.
#' @return Tibble: Arm, Class, nDocs, ArmPrecision, TerminalAccuracy, Keep.
#' Cache key for one arm-or-terminal and one score floor
#'
#' Floors are doubles, so the key is formatted rather than pasted: the default conversion of 0.7 and
#' the value stored in the grid must produce the same string or a lookup silently misses and the
#' caller falls back to recomputing, which would be slow and correct rather than fast and correct.
#'
#' @param .a Character. Arm or terminal name.
#' @param .floor Numeric. Score floor.
#' @return Character key.
#' @keywords internal
orch_policy_key <- function(.a, .floor) {
  if (FALSE) {
    .a     <- "legal-bert"
    .floor <- 0.90
  }
  paste0(.a, "||", formatC(.floor, format = "g", digits = 10))
}

#' Precomputed per-class keep decisions, for every terminal and floor at once
#'
#' The keep decision for an arm and a category compares that arm's precision, among the documents it
#' commits on above the floor, against the terminal's accuracy on those same documents. Nothing in
#' that comparison depends on which other arms a policy happens to list, or in what order: it is a
#' function of the arm, the category, the terminal and the floor alone.
#'
#' The unfactored version recomputes it inside every policy, which at a few hundred policies over five
#' folds and three tasks is the same table built thousands of times. Computing it once per terminal
#' and floor, over every arm, and subsetting per policy is exactly equivalent -- filtering rows by arm
#' before or after a per-arm summarise gives the same rows -- and does the work fifty times less
#' often.
#'
#' @param .spine The spine the decision is fitted on.
#' @param .terminals Character vector of terminal arms appearing in the policy grid.
#' @param .floors Numeric vector of score floors appearing in the policy grid.
#' @param .none Character. Abstention sentinel.
#' @return Named list of tibbles, keyed by terminal and floor.
orch_fit_cache <- function(.spine, .terminals, .floors, .none = ORCH_NONE) {
  if (FALSE) {
    .spine     <- dplyr::filter(spine_det, Fold != 1L)
    .terminals <- unique(policies_det$Terminal)
    .floors    <- unique(policies_det$Floor)
    .none      <- ORCH_NONE
  }
  grid_ <- tidyr::expand_grid(Terminal = .terminals, Floor = .floors)

  purrr::pmap(grid_, function(Terminal, Floor) {
    term_ <- .spine |>
      dplyr::filter(.data$Arm == Terminal) |>
      dplyr::select(DocID, TermPred = .data$Pred)

    .spine |>
      dplyr::filter(.data$Pred != .none, .data$Score >= Floor) |>
      dplyr::inner_join(term_, by = dplyr::join_by(DocID)) |>
      # .by is tidyselect and cannot rename, so the grouping column is created before the summarise
      dplyr::mutate(Class = .data$Pred) |>
      dplyr::summarise(
        nDocs            = dplyr::n(),
        ArmPrecision     = mean(.data$Pred == .data$TrueLabel),
        TerminalAccuracy = mean(.data$TermPred == .data$TrueLabel),
        .by = c(Arm, Class)
      ) |>
      dplyr::mutate(Keep = .data$ArmPrecision > .data$TerminalAccuracy) |>
      dplyr::arrange(.data$Arm, dplyr::desc(.data$nDocs))
  }) |>
    purrr::set_names(orch_policy_key(grid_$Terminal, grid_$Floor))
}

#' Precomputed candidate rows, for every arm and floor at once
#'
#' A cascade step asks one question of the spine: which documents did this arm commit on, above this
#' floor, and what did it say. The answer depends on the arm and the floor and on nothing else, so it
#' is shared by every policy that names them. Scanning the whole spine for it inside each policy makes
#' the cost grow with the size of the search rather than with the size of the data.
#'
#' @param .spine The spine the candidates are drawn from.
#' @param .floors Numeric vector of score floors appearing in the policy grid.
#' @param .none Character. Abstention sentinel.
#' @return Named list of two-column tibbles, keyed by arm and floor.
orch_cand_cache <- function(.spine, .floors, .none = ORCH_NONE) {
  if (FALSE) {
    .spine  <- dplyr::filter(spine_det, Fold == 1L)
    .floors <- unique(policies_det$Floor)
    .none   <- ORCH_NONE
  }
  arms_ <- unique(.spine$Arm)
  grid_ <- tidyr::expand_grid(Arm = arms_, Floor = .floors)

  purrr::pmap(grid_, function(Arm, Floor) {
    .spine |>
      dplyr::filter(.data$Arm == !!Arm, .data$Pred != .none, .data$Score >= Floor) |>
      dplyr::select(DocID, ArmPred = .data$Pred)
  }) |>
    purrr::set_names(orch_policy_key(grid_$Arm, grid_$Floor))
}

#' Per-class keep decisions for one policy
#'
#' Which categories an arm is allowed to override the terminal on: those where the arm is more precise
#' than the terminal is accurate, on the documents the arm commits to. Only the perclass family gates
#' this way; a plain cascade lets a committing arm override everywhere.
#'
#' @param .spine The spine the decision is fitted on.
#' @param .policy One row of the policy grid.
#' @param .none Character. Abstention sentinel.
#' @param .cache Optional output of orch_fit_cache(). Supplied, the table is looked up rather than
#'   recomputed; absent, it is computed for this policy alone. The two paths return the same rows.
#' @return Tibble: DocID, Fold, TrueLabel, carried columns, PredLabel, DecidedBy.
#' @return Tibble: Arm, Class, nDocs, ArmPrecision, TerminalAccuracy, Keep.
orch_policy_fit <- function(.spine, .policy, .none = ORCH_NONE, .cache = NULL) {
  if (FALSE) {
    .spine  <- dplyr::filter(spine_det, Fold != 1L)
    .policy <- policies_det[10, ]
    .none   <- ORCH_NONE
    .cache  <- NULL
  }
  order_ <- .policy$Order[[1]]
  empty_ <- tibble::tibble(Arm = character(), Class = character(), nDocs = integer(),
                           ArmPrecision = numeric(), TerminalAccuracy = numeric(), Keep = logical())
  if (.policy$Family != "perclass" || length(order_) == 0L) return(empty_)

  all_ <- if (!is.null(.cache)) {
    .cache[[orch_policy_key(.policy$Terminal, .policy$Floor)]]
  } else {
    orch_fit_cache(
      .spine = .spine, .terminals = .policy$Terminal, .floors = .policy$Floor, .none = .none
    )[[1]]
  }
  if (is.null(all_)) return(empty_)

  all_ |> dplyr::filter(.data$Arm %in% order_)
}

#' Apply a fitted policy to a set of spine rows
#'
#' Walks the cascade in order, taking the first arm that commits a label clearing the floor and, for
#' the per-class family, passing the fitted gate. Anything no gated arm claimed falls through to the
#' terminal, so the output holds exactly one prediction per document.
#'
#' @param .spine Rows of the routing spine to label.
#' @param .policy One row of orch_policy_grid().
#' @param .fit Output of orch_policy_fit() for the same policy.
#' @param .none Character. Abstention sentinel.
#' @param .cache Optional output of orch_cand_cache() built from these same rows. Supplied, each
#'   cascade step is a lookup rather than a scan of the whole spine; absent, it filters as before.
#'   The two paths return identical rows.
#' @return Predictions tibble: DocID, Fold, TrueLabel, PredLabel, DecidedBy (+ carried columns).
orch_policy_apply <- function(.spine, .policy, .fit = NULL, .none = ORCH_NONE, .cache = NULL) {
  if (FALSE) {
    .spine  <- dplyr::filter(spine_det, Fold == 1L)
    .policy <- policies_det[10, ]
    .fit    <- orch_policy_fit(dplyr::filter(spine_det, Fold != 1L), policies_det[10, ])
    .none   <- ORCH_NONE
    .cache  <- NULL
  }
  order_ <- .policy$Order[[1]]

  out_ <- .spine |>
    dplyr::filter(.data$Arm == .policy$Terminal) |>
    dplyr::select(DocID, Fold, TrueLabel, dplyr::any_of(c("ClassDetailed2", "LabelRound")),
                  PredLabel = .data$Pred) |>
    dplyr::mutate(DecidedBy = .policy$Terminal)

  if (nrow(out_) == 0L) cli::cli_abort("Terminal arm {(.policy$Terminal)} absent from these rows.")
  if (length(order_) == 0L) return(out_)

  keep_ <- if (is.null(.fit) || nrow(.fit) == 0L) NULL else dplyr::filter(.fit, .data$Keep)

  # Walk the cascade backwards so each earlier arm overwrites what the later ones decided.
  # Overwriting in reverse priority is equivalent to first-match-wins and needs no accumulator.
  for (arm_ in rev(order_)) {
    cand_ <- if (!is.null(.cache)) {
      .cache[[orch_policy_key(arm_, .policy$Floor)]]
    } else {
      .spine |>
        dplyr::filter(.data$Arm == arm_, .data$Pred != .none, .data$Score >= .policy$Floor) |>
        dplyr::select(DocID, ArmPred = .data$Pred)
    }
    if (is.null(cand_)) next

    if (identical(.policy$Family, "perclass")) {
      allow_ <- if (is.null(keep_)) {
        tibble::tibble(Class = character())
      } else {
        keep_ |> dplyr::filter(.data$Arm == arm_) |> dplyr::select(Class)
      }
      cand_ <- cand_ |> dplyr::semi_join(allow_, by = dplyr::join_by(ArmPred == Class))
    }

    out_ <- out_ |>
      dplyr::left_join(cand_, by = dplyr::join_by(DocID)) |>
      dplyr::mutate(
        DecidedBy = dplyr::if_else(is.na(.data$ArmPred), .data$DecidedBy, arm_),
        PredLabel = dplyr::coalesce(.data$ArmPred, .data$PredLabel),
        ArmPred   = NULL
      )
  }
  out_
}


# 7. Ranking and nested selection --------------------------------------------------------------------------------------

#' Score every policy on one set of spine rows
#' @param .spine Rows to score on.
#' @param .policies Output of orch_policy_grid().
#' @param .spine_fit Optional rows to fit the per-class gate on; defaults to .spine.
#' @param .lenient Logical. Lenient (either-label) scoring.
#' @param .none Character. Abstention sentinel.
#' @return Tibble: PolicyID, Label, Family, Terminal, Floor, Depth, Accuracy, MacroF1, nRouted.
orch_score_policies <- function(.spine, .policies, .spine_fit = NULL, .lenient = FALSE,
                                .none = ORCH_NONE) {
  if (FALSE) {
    .spine     <- spine_det
    .policies  <- policies_det
    .spine_fit <- NULL
    .lenient   <- FALSE
  }
  fit_on_ <- if (is.null(.spine_fit)) .spine else .spine_fit

  # Both tables every policy needs depend only on the terminal, the arm and the floor, never on the
  # ordering. Building them once here is what keeps the cost of the search proportional to the data
  # rather than to the number of orderings the grid happens to enumerate.
  cache_fit_  <- orch_fit_cache(
    .spine = fit_on_, .terminals = unique(.policies$Terminal),
    .floors = unique(.policies$Floor), .none = .none
  )
  cache_cand_ <- orch_cand_cache(
    .spine = .spine, .floors = unique(.policies$Floor), .none = .none
  )

  purrr::map(seq_len(nrow(.policies)), function(.i) {
    pol_  <- .policies[.i, ]
    fit_  <- orch_policy_fit(.spine = fit_on_, .policy = pol_, .none = .none, .cache = cache_fit_)
    pred_ <- orch_policy_apply(.spine = .spine, .policy = pol_, .fit = fit_, .none = .none,
                               .cache = cache_cand_)
    sc_   <- clf_scores(.tab_pred = pred_, .lenient = .lenient, .none = .none)
    tibble::tibble(
      PolicyID = pol_$PolicyID,
      Label    = pol_$Label,
      Family   = pol_$Family,
      Terminal = pol_$Terminal,
      Floor    = pol_$Floor,
      Depth    = pol_$Depth,
      Accuracy = sc_$Accuracy,
      MacroF1  = sc_$F1_macro,
      nRouted  = sum(pred_$DecidedBy != pol_$Terminal)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(dplyr::desc(.data$MacroF1), .data$nRouted, .data$PolicyID)
}

#' Pick one policy from a ranking, within a tolerance
#'
#' A strict argmax crowns whichever rule leads by the fourth decimal, which on a sample of this size
#' is a rounding artifact rather than a preference. Among policies within .tolerance of the best, the
#' one routing the FEWEST documents is taken. The incumbent routes none, so it wins every tie, and
#' routing must earn its place by a margin the sample can measure. This is 03C's precision-tolerance
#' convention applied to macro-F1.
#'
#' @param .rank Output of orch_score_policies().
#' @param .tolerance Macro-F1 band treated as indistinguishable.
#' @return One row of .rank.
orch_pick_policy <- function(.rank, .tolerance = 0.005) {
  if (FALSE) {
    .rank      <- orch_score_policies(spine_det, policies_det)
    .tolerance <- 0.005
  }
  .rank |>
    dplyr::filter(.data$MacroF1 >= max(.data$MacroF1, na.rm = TRUE) - .tolerance) |>
    dplyr::arrange(.data$nRouted, .data$Depth, dplyr::desc(.data$MacroF1), .data$PolicyID) |>
    dplyr::slice(1)
}

#' Nested cross-validated selection of a routing policy
#'
#' For each fold in turn, every policy is fitted and ranked on the other folds, the winner is applied
#' to the held-out fold, and the held-out predictions are pooled. No document contributes to choosing
#' the policy that labels it, so the pooled score estimates what a reader running this selection
#' would obtain rather than what the best rule scored on the data that chose it.
#'
#' @param .spine Routing spine, restricted to arms present on every fold.
#' @param .policies Output of orch_policy_grid().
#' @param .tolerance Macro-F1 band treated as indistinguishable when crowning a fold winner.
#' @param .lenient Logical. Lenient scoring inside the selection.
#' @param .none Character. Abstention sentinel.
#' @return Tibble of pooled held-out predictions plus WinnerID, WinnerLabel, WinnerTerminal.
#' Start worker processes and load this pipeline into each of them
#'
#' The nested loop is five independent problems: crown a policy on four folds, score it on the fifth,
#' and never let the two touch. Nothing crosses between folds, so they can run at once, and on a
#' search of a few thousand policies that is most of the wall clock.
#'
#' Workers are separate R sessions and inherit nothing, so the libraries this pipeline is built from
#' are sourced into each one. The paths are an argument rather than a constant because the runbook
#' already resolves them through the shared helper, and restating a path convention here would be a
#' second place for it to drift.
#'
#' Failure to start workers is reported and the search continues on one core. A slow document is a
#' nuisance; one that silently produced its numbers a different way would be worse.
#'
#' ORDER MATTERS AND SO DOES COMPLETENESS. The libraries have top-level side effects: the sample
#' script registers its taxonomy with the figure layer and aliases the table helpers, both at source
#' time. Sourcing it into a worker that lacks those layers throws part-way through the file, and every
#' function defined below that point silently does not exist. The failure then surfaces much later, as
#' a fold reporting that some function it needs cannot be found. The probe below exists so that it
#' surfaces here instead.
#'
#' @param .sources Character vector of absolute paths to source into each worker, in dependency order.
#' @param .workers Integer or NULL. Worker count; NULL leaves two cores for the session.
#' @param .needs Character vector of functions the fold loop calls in the worker. Their presence is
#'   what the setup is checked against, because a worker that started is not the same as one that can
#'   run the job.
#' @return Invisibly the number of workers started, zero if none.
orch_daemons <- function(.sources, .workers = NULL,
                         .needs = c("orch_score_policies", "orch_pick_policy",
                                    "orch_policy_fit", "orch_policy_apply", "clf_scores")) {
  if (FALSE) {
    .sources <- c(
      here::here("1_code", "_Commons", "_Plots.R"),
      here::here("1_code", "_Commons", "_Tables.R"),
      purrr::map_chr(c("03A-ClassifyPrepare", "03E-ClassifyOrchestrate"),
                     \(.s) init_create_script_fun(here::here(), .s))
    )
    .workers <- NULL
    .needs   <- "orch_score_policies"
  }
  miss_ <- .sources[!fs::file_exists(.sources)]
  if (length(miss_) > 0L) cli::cli_abort("Cannot source into workers, missing: {miss_}")
  if (is.null(.workers)) .workers <- as.integer(max(1L, parallel::detectCores() - 2L))
  if (.workers <= 1L) {
    cli::cli_alert_info("One core available; the fold loop runs serially.")
    return(invisible(0L))
  }

  ok_ <- tryCatch({
    mirai::daemons(.workers)
    mirai::everywhere(
      { for (.p in .srcs) source(.p, encoding = "UTF-8") },
      .args = list(.srcs = as.character(.sources))
    )
    TRUE
  }, error = function(e) {
    cli::cli_alert_warning("Could not start workers ({conditionMessage(e)}); running serially.")
    FALSE
  })

  if (!ok_) {
    try(mirai::daemons(0L), silent = TRUE)
    return(invisible(0L))
  }

  # everywhere() dispatches asynchronously, so a source that failed in a worker does not raise here.
  # Ask the workers directly whether they can do the job, rather than assuming that starting implies
  # readiness.
  probe_ <- tryCatch(
    mirai::mirai_map(
      .x    = seq_len(.workers),
      .f    = function(.i, .needs) {
        gone_ <- .needs[!vapply(.needs, exists, logical(1), mode = "function")]
        if (length(gone_) == 0L) "" else paste(gone_, collapse = ", ")
      },
      .args = list(.needs = .needs)
    )[],
    error = function(e) list(conditionMessage(e))
  )
  bad_ <- unique(unlist(probe_))
  bad_ <- bad_[nzchar(bad_)]

  if (length(bad_) > 0L) {
    cli::cli_alert_danger(
      "Workers started but cannot run the fold loop; missing there: {bad_}."
    )
    cli::cli_alert_warning(
      "This means a library failed to source in the worker, usually because {.arg .sources} is \\
       incomplete or out of dependency order. Falling back to one core; the numbers are unaffected."
    )
    try(mirai::daemons(0L), silent = TRUE)
    return(invisible(0L))
  }

  cli::cli_alert_success("{(.workers)} worker{?s} ready; the fold loop runs in parallel.")
  invisible(as.integer(.workers))
}

#' Are workers currently available
#'
#' @return Logical scalar.
#' @keywords internal
orch_parallel <- function() {
  if (FALSE) NULL
  tryCatch(
    isTRUE(requireNamespace("mirai", quietly = TRUE)) && mirai::status()$connections > 0L,
    error = function(e) FALSE
  )
}

#' One fold of the nested loop: crown on the rest, score on this one
#'
#' Written as a standalone function taking everything it needs, rather than as a closure over the
#' enclosing environment, because it has to run in a worker process that shares nothing with the
#' session. The serial and parallel paths call exactly this, so the two cannot diverge.
#'
#' @param .k Integer. The held-out fold.
#' @param .spine The full routing spine.
#' @param .policies The policy grid.
#' @param .tolerance Numeric. Macro-F1 band treated as a tie.
#' @param .lenient Logical. Lenient scoring.
#' @param .none Character. Abstention sentinel.
#' @return Predictions for the held-out fold, tagged with the winning policy.
orch_fold <- function(.k, .spine, .policies, .tolerance, .lenient, .none) {
  if (FALSE) {
    .k         <- 1L
    .spine     <- spine_det
    .policies  <- policies_det
    .tolerance <- 0.005
    .lenient   <- FALSE
    .none      <- ORCH_NONE
  }
  train_ <- .spine |> dplyr::filter(.data$Fold != .k)
  test_  <- .spine |> dplyr::filter(.data$Fold == .k)

  rank_ <- orch_score_policies(.spine = train_, .policies = .policies, .lenient = .lenient,
                               .none = .none)
  win_  <- .policies |>
    dplyr::filter(.data$PolicyID == orch_pick_policy(.rank = rank_, .tolerance = .tolerance)$PolicyID)

  # The winner is fitted on the training folds and applied to the held-out one, so the two caches are
  # built from different spines. Conflating them is exactly the leak the nesting exists to prevent,
  # which is why they are named apart rather than shared.
  fit_ <- orch_policy_fit(
    .spine = train_, .policy = win_, .none = .none,
    .cache = orch_fit_cache(.spine = train_, .terminals = win_$Terminal,
                            .floors = win_$Floor, .none = .none)
  )

  orch_policy_apply(
    .spine = test_, .policy = win_, .fit = fit_, .none = .none,
    .cache = orch_cand_cache(.spine = test_, .floors = win_$Floor, .none = .none)
  ) |>
    dplyr::mutate(WinnerID = win_$PolicyID, WinnerLabel = win_$Label,
                  WinnerTerminal = win_$Terminal)
}

#' Nested cross-validated selection of a routing policy
#'
#' For each fold in turn, every policy is fitted and ranked on the other folds, the winner is applied
#' to the held-out fold, and the held-out predictions are pooled. No document contributes to choosing
#' the policy that labels it, so the pooled score estimates what a reader running this selection would
#' obtain rather than what the best rule scored on the data that chose it.
#'
#' Folds run on workers when any are available and serially otherwise. Both paths call the same
#' per-fold function, so the result does not depend on which one ran.
#'
#' @param .spine Routing spine, restricted to arms present on every fold.
#' @param .policies Output of orch_policy_grid().
#' @param .tolerance Macro-F1 band treated as indistinguishable when crowning a fold winner.
#' @param .lenient Logical. Lenient scoring inside the selection.
#' @param .none Character. Abstention sentinel.
#' @return Tibble of pooled held-out predictions plus WinnerID, WinnerLabel, WinnerTerminal.
orch_select_nested <- function(.spine, .policies, .tolerance = 0.005, .lenient = FALSE,
                               .none = ORCH_NONE) {
  if (FALSE) {
    .spine     <- spine_det
    .policies  <- policies_det
    .tolerance <- 0.005
    .lenient   <- FALSE
    .none      <- ORCH_NONE
  }
  folds_ <- sort(unique(.spine$Fold))
  par_   <- orch_parallel()
  cli::cli_alert_info(
    "Nested selection: {nrow(.policies)} polic{?y/ies} ranked within each of {length(folds_)} \\
     fold{?s}{if (par_) ', in parallel' else ''}"
  )

  out_ <- if (par_) {
    mirai::mirai_map(
      .x    = folds_,
      .f    = orch_fold,
      .args = list(.spine = .spine, .policies = .policies, .tolerance = .tolerance,
                   .lenient = .lenient, .none = .none)
    )[.progress]
  } else {
    purrr::map(
      folds_, orch_fold,
      .spine = .spine, .policies = .policies, .tolerance = .tolerance,
      .lenient = .lenient, .none = .none, .progress = "folds"
    )
  }

  bad_ <- purrr::map_lgl(out_, \(.r) inherits(.r, "miraiError") || inherits(.r, "errorValue"))
  if (any(bad_)) {
    cli::cli_abort("Fold {folds_[bad_]} failed in a worker: {as.character(out_[bad_][[1]])}")
  }
  purrr::list_rbind(out_)
}

#' Report which policy won in each fold, and what it actually did
#'
#' Two things are reported, because the winner's NAME overstates the instability. A cascade whose
#' floor no document clears is the incumbent under another label, so the column that matters is how
#' many documents the fold's winner actually moved.
#'
#' @param .tab_nested Output of orch_select_nested().
#' @return Invisibly the per-fold winner tibble.
orch_report_stability <- function(.tab_nested) {
  if (FALSE) .tab_nested <- res_det$Nested

  per_fold_ <- .tab_nested |>
    dplyr::summarise(
      nDocs   = dplyr::n(),
      nRouted = sum(.data$DecidedBy != .data$WinnerTerminal),
      Winner  = dplyr::first(.data$WinnerLabel),
      .by = Fold
    ) |>
    dplyr::mutate(Effect = dplyr::if_else(.data$nRouted == 0L, "incumbent", "routed")) |>
    dplyr::arrange(.data$Fold)

  counts_ <- per_fold_ |> dplyr::count(.data$Winner, name = "Folds", sort = TRUE)

  cli::cli_h2("Which policy won, fold by fold")
  per_fold_ |> tbl_say()
  cli::cli_text("")
  counts_ |> tbl_say(.title = "Distinct winners")
  cli::cli_text("")

  n_inert_ <- sum(per_fold_$nRouted == 0L)
  if (n_inert_ > 0L) {
    cli::cli_alert_info(
      "{n_inert_} of {nrow(per_fold_)} fold{?s} routed no documents at all: the crowned cascade was \\
       the incumbent under another name. Count the Effect column, not the winner names."
    )
  }
  if (nrow(counts_) == 1L) {
    cli::cli_alert_info(
      "One policy won every fold, so the ranking reflects the documents rather than the split."
    )
  } else {
    cli::cli_alert_warning(
      "{nrow(counts_)} different policies won across {nrow(per_fold_)} folds. The ranking is not \\
       stable, which means the search is separating rules this sample cannot distinguish."
    )
  }
  invisible(per_fold_)
}


# 8. The headline comparison -------------------------------------------------------------------------------------------

#' Compare the incumbent, the other terminals, the in-sample best and the nested procedure
#'
#' The distance between the last two is the reason this document exists. The in-sample best is what a
#' search selecting and evaluating on the same documents would have reported; the nested estimate is
#' what the procedure delivers. Reporting the second without the third presents selection bias as a
#' gain.
#'
#' @param .spine Routing spine.
#' @param .policies Output of orch_policy_grid().
#' @param .tab_nested Output of orch_select_nested().
#' @param .incumbent Character. Arm treated as the incumbent baseline.
#' @param .lenient Logical. Lenient (either-label) scoring.
#' @param .none Character. Abstention sentinel.
#' @return Tibble: Strategy, Honest, Accuracy, MacroF1, N, DeltaMacroF1, Note.
orch_compare <- function(.spine, .policies, .tab_nested, .incumbent, .lenient = FALSE,
                         .none = ORCH_NONE) {
  if (FALSE) {
    .spine      <- spine_det
    .policies   <- policies_det
    .tab_nested <- nested_det
    .incumbent  <- "legal-bert"
  }
  bare_     <- .policies |> dplyr::filter(.data$Depth == 0L)
  base_pol_ <- bare_ |> dplyr::filter(.data$Terminal == .incumbent) |> dplyr::slice(1)
  if (nrow(base_pol_) == 0L) cli::cli_abort("No bare policy for terminal {(.incumbent)}.")

  base_sc_ <- clf_scores(
    .tab_pred = orch_policy_apply(.spine = .spine, .policy = base_pol_, .none = .none),
    .lenient  = .lenient, .none = .none
  )

  insample_  <- orch_score_policies(.spine = .spine, .policies = .policies, .lenient = .lenient,
                                    .none = .none) |>
    dplyr::slice(1)
  nested_sc_ <- clf_scores(.tab_pred = .tab_nested, .lenient = .lenient, .none = .none)

  others_ <- bare_ |> dplyr::filter(.data$Terminal != .incumbent)
  other_rows_ <- if (nrow(others_) == 0L) NULL else {
    purrr::map(seq_len(nrow(others_)), function(.i) {
      pol_ <- others_[.i, ]
      sc_  <- clf_scores(
        .tab_pred = orch_policy_apply(.spine = .spine, .policy = pol_, .none = .none),
        .lenient  = .lenient, .none = .none
      )
      tibble::tibble(Strategy = pol_$Label, Honest = TRUE, Accuracy = sc_$Accuracy,
                     MacroF1 = sc_$F1_macro, N = sc_$N, Note = "always commits; no routing")
    }) |>
      purrr::list_rbind()
  }

  dplyr::bind_rows(
    tibble::tibble(
      Strategy = paste0(.incumbent, " (incumbent)"), Honest = TRUE,
      Accuracy = base_sc_$Accuracy, MacroF1 = base_sc_$F1_macro, N = base_sc_$N,
      Note = "no selection; the number to beat"
    ),
    other_rows_,
    tibble::tibble(
      Strategy = "Best policy, selected in sample", Honest = FALSE,
      Accuracy = insample_$Accuracy, MacroF1 = insample_$MacroF1, N = base_sc_$N,
      Note = insample_$Label
    ),
    tibble::tibble(
      Strategy = "Nested selection (the procedure)", Honest = TRUE,
      Accuracy = nested_sc_$Accuracy, MacroF1 = nested_sc_$F1_macro, N = nested_sc_$N,
      Note = "policy chosen inside each fold"
    )
  ) |>
    dplyr::mutate(DeltaMacroF1 = .data$MacroF1 - base_sc_$F1_macro) |>
    dplyr::relocate(Strategy, Honest, Accuracy, MacroF1, N, DeltaMacroF1, Note)
}

#' Print the headline comparison with the reading it requires
#' @param .tab Output of orch_compare().
#' @param .title Heading.
#' @return Invisibly .tab.
orch_report_compare <- function(.tab, .title = "Routing against the incumbent") {
  if (FALSE) .tab <- res_det$Compare
  cli::cli_h2("{(.title)}")
  .tab |>
    dplyr::mutate(
      Accuracy     = tbl_pct(.data$Accuracy),
      MacroF1      = sprintf("%.3f", .data$MacroF1),
      DeltaMacroF1 = sprintf("%+.3f", .data$DeltaMacroF1)
    ) |>
    dplyr::select(Strategy, Honest, Accuracy, MacroF1, DeltaMacroF1, Note) |>
    tbl_say()
  cli::cli_text("")

  best_ <- .tab$MacroF1[!.tab$Honest][1]
  inc_  <- .tab$MacroF1[grepl("(incumbent)", .tab$Strategy, fixed = TRUE)][1]
  nest_ <- .tab$MacroF1[.tab$Strategy == "Nested selection (the procedure)"][1]
  cli::cli_alert_info(
    "The in-sample best can only equal or exceed the incumbent, since the incumbent is inside the \\
     search. Here it exceeds it by {sprintf('%+.4f', best_ - inc_)}, and the nested estimate sits \\
     {sprintf('%+.4f', nest_ - inc_)} from it."
  )
  invisible(.tab)
}

#' Where a routed prediction differed from the incumbent, and whether it helped
#'
#' A pooled score can hide a rule that fixes as many documents as it breaks. This is the only view
#' distinguishing a null result from an offsetting one.
#'
#' @param .tab_nested Output of orch_select_nested().
#' @param .spine Routing spine.
#' @param .incumbent Character. Arm treated as the incumbent.
#' @return Tibble: Movement, nDocs, Share.
orch_movement <- function(.tab_nested, .spine, .incumbent) {
  if (FALSE) {
    .tab_nested <- nested_det
    .spine      <- spine_det
    .incumbent  <- "legal-bert"
  }
  inc_ <- .spine |>
    dplyr::filter(.data$Arm == .incumbent) |>
    dplyr::select(DocID, IncPred = .data$Pred)
  n_ <- nrow(.tab_nested)

  .tab_nested |>
    dplyr::inner_join(inc_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      Movement = dplyr::case_when(
        .data$PredLabel == .data$IncPred   ~ "Unchanged",
        .data$PredLabel == .data$TrueLabel ~ "Moved, now correct",
        .data$IncPred   == .data$TrueLabel ~ "Moved, broke a correct label",
        TRUE                               ~ "Moved, wrong either way"
      )
    ) |>
    dplyr::summarise(nDocs = dplyr::n(), Share = dplyr::n() / n_, .by = Movement) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' Report the movement table with the reading it requires
#' @param .tab Output of orch_movement().
#' @return Invisibly .tab.
orch_report_movement <- function(.tab) {
  if (FALSE) .tab <- res_det$Movement
  cli::cli_h2("What routing changed")
  .tab |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say()
  cli::cli_text("")
  fixed_ <- sum(.tab$nDocs[.tab$Movement == "Moved, now correct"])
  broke_ <- sum(.tab$nDocs[.tab$Movement == "Moved, broke a correct label"])
  moved_ <- sum(.tab$nDocs[.tab$Movement != "Unchanged"])
  if (moved_ == 0L) {
    cli::cli_alert_info("Routing moved no documents: the procedure reduced to the incumbent.")
  } else {
    cli::cli_alert_info(
      "Routing moved {moved_} document{?s}, repairing {fixed_} and breaking {broke_}. A small net \\
       figure on top of many moves is an offsetting result, not an inert one."
    )
  }
  invisible(.tab)
}


# 9. Deployment --------------------------------------------------------------------------------------------------------

#' Choose the policy to deploy, fitted on every fold
#'
#' The folds pay for the estimate; the full sample produces the artifact. This mirrors the convention
#' the transformer stage already follows, and the same tolerance applies, so the artifact cannot
#' contradict the document's own conclusion by preferring a fourth-decimal winner.
#'
#' @param .spine Routing spine.
#' @param .policies Output of orch_policy_grid().
#' @param .tolerance Macro-F1 band treated as indistinguishable.
#' @param .deployable Arm names that exist as artifacts. Policies naming anything else are ranked and
#'   reported but not chosen, because a deployment artifact referring to an arm built in memory is
#'   one the apply stage cannot execute. A derived arm that won by a real margin would be a finding
#'   worth building support for, not something to ship silently.
#' @param .lenient Logical. Lenient scoring during selection.
#' @param .none Character. Abstention sentinel.
#' @return List: Policy (one-row tibble), Fit (per-class gate), Rank (full leaderboard).
orch_policy_final <- function(.spine, .policies, .tolerance = 0.005, .deployable = NULL,
                              .lenient = FALSE, .none = ORCH_NONE) {
  if (FALSE) {
    .spine       <- spine_det
    .policies    <- policies_det
    .tolerance   <- 0.005
    .deployable  <- arms_cv_$Arm
  }
  rank_ <- orch_score_policies(.spine = .spine, .policies = .policies, .lenient = .lenient,
                               .none = .none)

  ok_ <- if (is.null(.deployable)) {
    rank_
  } else {
    keep_ <- purrr::map_lgl(rank_$PolicyID, function(.id) {
      pol_ <- .policies |> dplyr::filter(.data$PolicyID == .id)
      all(c(pol_$Order[[1]], pol_$Terminal) %in% .deployable)
    })
    if (!any(keep_)) cli::cli_abort("No policy uses only deployable arms.")
    if (any(!keep_)) {
      cli::cli_alert_info(
        "{sum(!keep_)} polic{?y/ies} excluded from deployment: they name an arm with no artifact."
      )
    }
    rank_[keep_, ]
  }

  pick_ <- orch_pick_policy(.rank = ok_, .tolerance = .tolerance)
  pol_  <- .policies |> dplyr::filter(.data$PolicyID == pick_$PolicyID)
  list(
    Policy = pol_,
    Fit    = orch_policy_fit(.spine = .spine, .policy = pol_, .none = .none),
    Rank   = rank_
  )
}

#' Write the deployment decision 03F consumes
#'
#' 03F applies a model to the full corpus and should make no decisions of its own. Everything it
#' needs is written here as data: the arm order, the floor, the family, the terminal, the
#' configuration behind each arm name, and the per-class gate if there is one.
#'
#' @param .final Output of orch_policy_final().
#' @param .arms Arm inventory, so the ConfigName behind each arm name is recorded.
#' @param .label_col Task the policy applies to.
#' @param .dir Output directory.
#' @param .commit_only Labels the keyword arms were allowed to commit to, recorded for 03E.
#' @param .stem File stem.
#' @return Invisibly the directory.
orch_save_policy <- function(.final, .arms, .label_col, .dir, .commit_only = NULL,
                             .stem = "routing_policy") {
  if (FALSE) {
    .final       <- res_det$Final
    .arms        <- res_det$Arms
    .label_col   <- "ClassDetailed"
    .dir         <- .lP$Output$Policy
    .commit_only <- NULL
    .stem        <- "routing_policy"
  }
  fs::dir_create(.dir)
  pol_   <- .final$Policy
  order_ <- pol_$Order[[1]]
  used_  <- unique(c(order_, pol_$Terminal))

  spec_ <- list(
    label_col   = .label_col,
    written_by  = "03D orch_save_policy",
    written_at  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    family      = pol_$Family,
    terminal    = pol_$Terminal,
    floor       = pol_$Floor,
    order       = I(as.list(order_)), # AsIs: auto_unbox would write a one-arm cascade as a scalar
    commit_only = if (is.null(.commit_only)) NULL else I(as.list(.commit_only)),
    label       = pol_$Label,
    arms        = I(
      .arms |>
        dplyr::filter(.data$Arm %in% used_) |>
        dplyr::select(Arm, ConfigName, Kind) |>
        purrr::transpose()
    )
  )
  path_ <- fs::path(.dir, paste0(.stem, "_", .label_col, ".json"))
  jsonlite::write_json(spec_, path_, auto_unbox = TRUE, pretty = TRUE)

  if (nrow(.final$Fit) > 0L) {
    arrow::write_parquet(.final$Fit,
                         fs::path(.dir, paste0(.stem, "_", .label_col, "_gate.parquet")))
  }
  cli::cli_alert_success("{(.label_col)} policy: {pol_$Label}")
  invisible(.dir)
}


# 10. One task, end to end ---------------------------------------------------------------------------------------------

#' Run the whole routing analysis for one task
#'
#' Inventory, spine, roles, policy grid, nested selection, comparison, movement and deployment
#' selection, in the order the document reports them. Exists so three tasks cost three calls rather
#' than three copies of forty chunks; the rules a referee inspects -- how many folds an arm must
#' cover, which labels count as commitments, how wide the search is -- are named arguments rather
#' than constants buried in the body.
#'
#' @param .runs_roots Character vector of runs directories.
#' @param .label_col Task to run.
#' @param .tab_prep Prepared sample.
#' @param .crowned Character or NULL. The CONFIGURATION the training stage crowned and deployed, which
#'   this stage resolves to its own arm name and takes as the status quo. A configuration name rather
#'   than an arm name because arms are named here, under a convention reconciling four engines; a
#'   second namer upstream would be a second convention. NULL falls back to the most accurate terminal
#'   and says so, which was the source of a three-way disagreement between trainer, stage and
#'   manifest.
#' @param .extra_arms Optional spine-form rows for a derived arm, or NULL.
#' @param .floors,.max_depth,.families Policy-space controls.
#' @param .commit_only Labels a keyword arm may commit to, or NULL for all.
#' @param .n_folds Folds an arm must cover to enter the nested search.
#' @param .tolerance Macro-F1 band treated as indistinguishable.
#' @param .lenient Logical. Lenient scoring inside the selection.
#' @param .carry Document-level columns to attach to the spine.
#' @param .none Character. Abstention sentinel.
#' @return List: LabelCol, Arms, ArmsCV, ArmsHeld, Spine, Roles, Incumbent, Terminals, Gated,
#'   Policies, Nested, Compare, Movement, Derived, Final.
orch_run_task <- function(.runs_roots, .label_col, .tab_prep,
                          .crowned = NULL,
                          .extra_arms = NULL,
                          .floors = c(0, 0.70, 0.80, 0.90),
                          .max_depth = 2L,
                          .families = c("cascade", "perclass"),
                          .commit_only = NULL,
                          .n_folds = 5L,
                          .tolerance = 0.005,
                          .lenient = FALSE,
                          .carry = c("ClassDetailed2", "LabelRound"),
                          .none = ORCH_NONE) {
  if (FALSE) {
    .runs_roots  <- c(.lP$Runs$Bert, .lP$Runs$Kw)
    .label_col   <- "ClassDetailed"
    .tab_prep    <- tab_prep
    .crowned     <- "ClassDetailed__nlpaueb-legal-bert-base-uncased__TText_L256_E6_B32_LR2e-05_W1_S42"
    .extra_arms  <- arm_hier
    .commit_only <- NULL
  }
  cli::cli_h1("Routing: {(.label_col)}")

  arms_      <- orch_arms(.runs_roots = .runs_roots, .label_col = .label_col, .none = .none)
  arms_cv_   <- arms_ |> dplyr::filter(.data$nFolds >= .n_folds)
  arms_held_ <- arms_ |> dplyr::filter(.data$nFolds <  .n_folds)

  spine_ <- orch_spine(
    .runs_roots  = .runs_roots,
    .arms        = arms_cv_,
    .tab_prep    = .tab_prep,
    .commit_only = .commit_only,
    .carry       = .carry,
    .none        = .none
  ) |>
    orch_spine_add(.arm_rows = .extra_arms, .carry = .carry)

  roles_ <- orch_roles(.spine = spine_, .none = .none)

  # A derived arm is one built in memory rather than read from a run folder. It competes in the
  # search on equal terms -- that is the point of building it -- but it is not a thing that exists,
  # and two roles must never fall to it. It cannot be the INCUMBENT, because the incumbent is the
  # status quo a routing rule has to beat and the fallback if none does, and a status quo that has to
  # be reconstructed from two other models each time is not one. And it cannot be DEPLOYED, because
  # the manifest has no way to express "run the broad model, then constrain the detailed one", so an
  # artifact naming it would be one the apply stage cannot execute.
  derived_ <- setdiff(roles_$Arm, arms_cv_$Arm)
  roles_   <- roles_ |> dplyr::mutate(Derived = .data$Arm %in% derived_)

  term_  <- roles_$Arm[roles_$Role == "terminal"]
  gated_ <- roles_$Arm[roles_$Role == "gated"]
  if (length(term_) == 0L) cli::cli_abort("{(.label_col)}: no terminal arm; cannot route.")

  real_term_ <- roles_ |> dplyr::filter(.data$Role == "terminal", !.data$Derived)
  if (nrow(real_term_) == 0L) {
    cli::cli_abort("{(.label_col)}: every terminal arm is derived; nothing deployable to fall back on.")
  }

  # THE INCUMBENT IS GIVEN, NOT CHOSEN HERE. It is the arm the training stage crowned and deployed,
  # supplied by the caller. Ranking the terminals again in this stage -- on accuracy, where the
  # trainer ranked on macro-F1 -- produced a third answer to a question two stages had already
  # answered, and the three disagreed: the manifest pinned a model that was never fitted while naming
  # a terminal that was never pinned. One stage decides, and it is the one that trained the models.
  inc_ <- if (!is.null(.crowned) && length(.crowned) == 1L && !is.na(.crowned)) {
    hit_ <- arms_cv_$Arm[arms_cv_$ConfigName == .crowned]
    if (length(hit_) == 0L) {
      cli::cli_abort(c(
        "{(.label_col)}: the deployed configuration is not in this task's arm inventory.",
        "i" = "Deployed: {.val {(.crowned)}}",
        "i" = "The inventory keeps one configuration per arm name, so a crowned configuration that \\
               lost its own arm to a higher-scoring sibling will not appear. Re-run the trainer's \\
               deployment, or widen the inventory."
      ))
    }
    if (!hit_[[1]] %in% real_term_$Arm) {
      cli::cli_abort(c(
        "{(.label_col)}: the deployed arm {.val {hit_[[1]]}} is not a runnable terminal here.",
        "i" = "Terminals present: {real_term_$Arm}"
      ))
    }
    hit_[[1]]
  } else {
    cli::cli_alert_warning(
      "No deployed configuration supplied for {(.label_col)}; falling back to the most accurate \\
       terminal. Pass {.arg .crowned} so this stage reports the arm that actually ships."
    )
    real_term_ |> dplyr::slice_max(.data$Accuracy, n = 1L, with_ties = FALSE) |> dplyr::pull(Arm)
  }
  if (length(derived_) > 0L) {
    cli::cli_alert_info(
      "{length(derived_)} derived arm{?s} in the search ({derived_}): eligible to win a fold, \
       ineligible to be the incumbent or to be deployed."
    )
  }

  policies_ <- orch_policy_grid(
    .arms_gated = gated_,
    .terminals  = term_,
    .floors     = .floors,
    .max_depth  = .max_depth,
    .families   = .families
  )
  nested_ <- orch_select_nested(.spine = spine_, .policies = policies_, .tolerance = .tolerance,
                                .lenient = .lenient, .none = .none)

  list(
    LabelCol  = .label_col,
    Arms      = arms_,
    ArmsCV    = arms_cv_,
    ArmsHeld  = arms_held_,
    Spine     = spine_,
    Roles     = roles_,
    Incumbent = inc_,
    Terminals = term_,
    Gated     = gated_,
    Policies  = policies_,
    Nested    = nested_,
    Compare   = orch_compare(.spine = spine_, .policies = policies_, .tab_nested = nested_,
                             .incumbent = inc_, .lenient = .lenient, .none = .none),
    Movement  = orch_movement(.tab_nested = nested_, .spine = spine_, .incumbent = inc_),
    Derived   = derived_,
    Final     = orch_policy_final(.spine = spine_, .policies = policies_, .tolerance = .tolerance,
                                  .deployable = arms_cv_$Arm, .lenient = .lenient, .none = .none)
  )
}

#' Every console block for one task, in order
#'
#' The single block to copy out when a routing conclusion needs re-checking.
#'
#' @param .res Output of orch_run_task().
#' @param .n_folds Folds required to enter the nested search, for the arm report.
#' @return Invisibly NULL.
orch_report_all <- function(.res, .n_folds = 5L) {
  if (FALSE) {
    .res     <- res_det
    .n_folds <- 5L
  }
  cli::cli_h1("Routing summary: {(.res$LabelCol)}")
  orch_report_arms(.tab = .res$Arms, .n_folds = .n_folds, .label_col = .res$LabelCol)
  orch_report_spine(.spine = .res$Spine)
  orch_report_stability(.tab_nested = .res$Nested)
  orch_report_compare(.tab = .res$Compare,
                      .title = paste0("Routing against the incumbent: ", .res$LabelCol))
  orch_report_movement(.tab = .res$Movement)
  invisible(NULL)
}

#' One row per task summarising the verdict
#'
#' The table the paper quotes. Everything else in this document supports one of these rows.
#'
#' @param .results List of orch_run_task() outputs.
#' @return Tibble: LabelCol, Incumbent, Deployed, Incumbent macro-F1, Nested macro-F1, Delta, Moved.
orch_verdicts <- function(.results) {
  if (FALSE) .results <- list(res_det, res_broad, res_amend)
  purrr::map(.results, function(.r) {
    cmp_ <- .r$Compare
    tibble::tibble(
      LabelCol      = .r$LabelCol,
      Incumbent     = .r$Incumbent,
      IncumbentF1   = cmp_$MacroF1[grepl("(incumbent)", cmp_$Strategy, fixed = TRUE)][1],
      NestedF1      = cmp_$MacroF1[cmp_$Strategy == "Nested selection (the procedure)"][1],
      DeltaMacroF1  = cmp_$DeltaMacroF1[cmp_$Strategy == "Nested selection (the procedure)"][1],
      Moved         = sum(.r$Movement$nDocs[.r$Movement$Movement != "Unchanged"]),
      Deployed      = .r$Final$Policy$Label
    )
  }) |>
    purrr::list_rbind()
}

#' Report the cross-task verdict table
#' @param .tab Output of orch_verdicts().
#' @return Invisibly .tab.
orch_report_verdicts <- function(.tab) {
  if (FALSE) .tab <- verdicts
  cli::cli_h2("Verdict, all tasks")
  .tab |>
    dplyr::mutate(
      IncumbentF1  = sprintf("%.3f", .data$IncumbentF1),
      NestedF1     = sprintf("%.3f", .data$NestedF1),
      DeltaMacroF1 = sprintf("%+.3f", .data$DeltaMacroF1)
    ) |>
    dplyr::select(LabelCol, Incumbent, IncumbentF1, NestedF1, DeltaMacroF1, Moved, Deployed) |>
    tbl_say()
  cli::cli_text("")
  cli::cli_alert_info(
    "Moved counts documents the nested procedure labelled differently from the incumbent. Zero \\
     there means the procedure reduced to the incumbent on every fold."
  )
  invisible(.tab)
}


# 11. Agreement as a shipped confidence flag ---------------------------------------------------------------------------
# Everything above asks which single label to ship. This section asks a different question, and for a
# reader of the released dataset a more useful one: HOW MUCH TO TRUST each label individually.
#
# The dataset covers far more documents than the 4,398 that carry a hand label, so a user of it
# currently knows one global number and nothing about the document in front of them. A per-document
# flag lets them condition -- restrict to concurring documents for a clean sample, or split on the
# flag as a robustness check -- which is a materially different offer from a single macro-F1.
#
# WHY THIS CAN BE SHIPPED AND "WHERE THE MODEL IS WRONG" CANNOT
# Whether two methods agree is visible on an unlabelled filing: run both, compare. Whether a method
# is WRONG is not, and no amount of care makes it so. That asymmetry is the whole reason the flag is
# built from agreement rather than from error. orch_agreement_pattern() therefore never touches the
# truth column, and is written so a reader can confirm that by looking at it: the reliability of each
# pattern is estimated separately, afterwards, from the labelled sample.
#
# WHICH ARMS MAY VOTE
# Not all of them, and the criterion is independence rather than accuracy. A constrained transformer
# built from the plain transformer's own probability vector is not a second opinion; it agrees with
# its parent by construction, and counting it would manufacture unanimity out of one model. The same
# caution applies to two checkpoints trained on the same folds. The voting set is therefore chosen in
# the document, on stated grounds, and passed in.
#
# THE NULL THIS SECTION HAS TO CLEAR
# The transformer already emits a confidence signal, and 03B already showed it separates errors. If
# agreement adds nothing beyond that probability, the honest recommendation is to ship the
# probability and skip the flag. orch_agreement_vs_prob() is that test, and it is reported next to
# the headline rather than left for a reader to think of.

#' Per-document agreement pattern, computed without reference to the truth
#'
#' Deliberately blind. This function takes the shipped predictions and the arms' predictions and
#' returns how many arms had an opinion and how many of those concurred. It selects no truth column
#' and joins no labelled table, so what it computes is exactly what can be recomputed on a filing
#' that was never labelled -- which is the property that makes the flag shippable at all.
#'
#' An abstaining arm neither agrees nor dissents, so it is excluded from both counts rather than
#' scored as disagreement. Treating silence as dissent would penalise a document for a keyword table
#' that simply had no term for it.
#'
#' @param .spine Routing spine, one row per document and arm.
#' @param .tab_pred Shipped predictions (DocID, PredLabel).
#' @param .arms Character vector of arm names permitted to vote. Chosen on independence grounds by
#'   the caller, since an arm derived from another is not a second opinion.
#' @param .none Character. Abstention sentinel.
#' @return Tibble: DocID, nCommit, nConcur, nDissent, Tier.
orch_agreement_pattern <- function(.spine, .tab_pred, .arms, .none = ORCH_NONE) {
  if (FALSE) {
    .spine    <- res_det$Spine
    .tab_pred <- res_det$Nested
    .arms     <- c("legal-bert", "kw-text")
    .none     <- ORCH_NONE
  }
  ship_ <- .tab_pred |> dplyr::select(DocID, ShipLabel = .data$PredLabel)

  .spine |>
    dplyr::filter(.data$Arm %in% .arms, .data$Pred != .none) |>
    dplyr::select(DocID, Arm, Pred) |>
    dplyr::inner_join(ship_, by = dplyr::join_by(DocID)) |>
    dplyr::summarise(
      nCommit  = dplyr::n(),
      nConcur  = sum(.data$Pred == .data$ShipLabel),
      .by = DocID
    ) |>
    dplyr::mutate(
      nDissent = .data$nCommit - .data$nConcur,
      Tier = dplyr::case_when(
        .data$nCommit <= 1L                     ~ "sole",
        .data$nDissent == 0L                    ~ "unanimous",
        .data$nConcur > .data$nDissent          ~ "majority",
        TRUE                                    ~ "split"
      ),
      Tier = factor(.data$Tier, levels = .orch_tiers)
    ) |>
    # Documents no permitted arm committed on still need a row, or the flag would be missing rather
    # than low and a downstream join would silently drop them.
    dplyr::right_join(ship_ |> dplyr::select(DocID), by = dplyr::join_by(DocID)) |>
    tidyr::replace_na(list(nCommit = 0L, nConcur = 0L, nDissent = 0L)) |>
    dplyr::mutate(Tier = factor(dplyr::coalesce(as.character(.data$Tier), "sole"),
                                levels = .orch_tiers))
}

#' Estimate how reliable each agreement tier is
#'
#' The labelled sample's only job here. The pattern is fixed before any label is consulted, so this
#' is a description of a fixed rule rather than a selection among rules, and needs no nesting: no
#' tier was chosen because it scored well.
#'
#' Coverage matters as much as accuracy. A tier that is 99% right on 4% of the corpus is not a usable
#' flag, and a reader deciding whether to condition on it needs both numbers side by side.
#'
#' @param .pattern Output of orch_agreement_pattern().
#' @param .tab_pred Shipped predictions carrying TrueLabel.
#' @return Tibble: Tier, nDocs, Share, Accuracy, ErrorRate, CumShare, CumAccuracy.
orch_agreement <- function(.pattern, .tab_pred) {
  if (FALSE) {
    .pattern  <- pattern_det
    .tab_pred <- res_det$Nested
  }
  n_ <- nrow(.tab_pred)
  .pattern |>
    dplyr::inner_join(
      .tab_pred |> dplyr::select(DocID, TrueLabel, PredLabel),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::mutate(Correct = .data$PredLabel == .data$TrueLabel) |>
    dplyr::summarise(
      nDocs    = dplyr::n(),
      Share    = dplyr::n() / n_,
      Accuracy = mean(.data$Correct),
      .by = Tier
    ) |>
    dplyr::arrange(.data$Tier) |>
    dplyr::mutate(
      ErrorRate   = 1 - .data$Accuracy,
      # Read down the table: what a user restricting to this tier or better would obtain.
      CumShare    = cumsum(.data$nDocs) / n_,
      CumAccuracy = cumsum(.data$Accuracy * .data$nDocs) / cumsum(.data$nDocs)
    )
}

#' Report the flag with the reading a user of the dataset needs
#' @param .tab Output of orch_agreement().
#' @param .arms Arms that voted, named in the heading so the flag is never read without them.
#' @return Invisibly .tab.
orch_report_agreement <- function(.tab, .arms) {
  if (FALSE) {
    .tab  <- agree_det
    .arms <- c("legal-bert", "kw-text")
  }
  cli::cli_h2("Agreement tiers, voting arms: {toString(.arms)}")
  .tab |>
    dplyr::mutate(
      Share       = tbl_pct(.data$Share),
      Accuracy    = tbl_pct(.data$Accuracy),
      CumShare    = tbl_pct(.data$CumShare),
      CumAccuracy = tbl_pct(.data$CumAccuracy),
      ErrorRate   = NULL
    ) |>
    tbl_say()
  cli::cli_text("")

  una_ <- .tab |> dplyr::filter(.data$Tier == "unanimous")
  if (nrow(una_) == 1L) {
    cli::cli_alert_info(
      "Where every voting arm concurred -- {tbl_pct(una_$Share)} of documents -- the shipped label \\
       is still wrong {tbl_pct(una_$ErrorRate)} of the time. That residual is the flag's ceiling: \\
       independent methods making the SAME mistake is the failure a concurrence flag cannot see."
    )
  }
  cli::cli_alert_info(
    "Read CumAccuracy down the table for what a user restricting to this tier or better obtains. A \\
     flag earns its place only if the top tier is both cleaner and large enough to be worth using."
  )
  invisible(.tab)
}

#' Does agreement add anything the transformer's own probability does not?
#'
#' The null this section has to clear. The transformer already emits a usable confidence signal, and
#' if the tiers stop separating once documents are compared at equal probability, then agreement is
#' a proxy for that probability and the honest recommendation is to ship the simpler number.
#'
#' Bands are equal-count rather than equal-width, because the probability distribution is heavily
#' massed near one and fixed-width bands would put almost every document in the top bin and measure
#' nothing.
#'
#' @param .pattern Output of orch_agreement_pattern().
#' @param .tab_pred Shipped predictions carrying TrueLabel.
#' @param .spine Routing spine, supplying the incumbent's probability.
#' @param .incumbent Arm whose Score is the probability.
#' @param .n_bands Number of equal-count probability bands.
#' @return Tibble: ProbBand, Tier, nDocs, Accuracy.
orch_agreement_vs_prob <- function(.pattern, .tab_pred, .spine, .incumbent, .n_bands = 4L) {
  if (FALSE) {
    .pattern   <- pattern_det
    .tab_pred  <- res_det$Nested
    .spine     <- res_det$Spine
    .incumbent <- res_det$Incumbent
    .n_bands   <- 4L
  }
  prob_ <- .spine |>
    dplyr::filter(.data$Arm == .incumbent) |>
    dplyr::select(DocID, Prob = .data$Score)

  .pattern |>
    dplyr::inner_join(.tab_pred |> dplyr::select(DocID, TrueLabel, PredLabel),
                      by = dplyr::join_by(DocID)) |>
    dplyr::inner_join(prob_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      Correct  = .data$PredLabel == .data$TrueLabel,
      ProbBand = dplyr::ntile(.data$Prob, .n_bands)
    ) |>
    dplyr::summarise(
      nDocs    = dplyr::n(),
      Accuracy = mean(.data$Correct),
      .by = c(ProbBand, Tier)
    ) |>
    dplyr::arrange(.data$ProbBand, .data$Tier)
}

#' Report the cross-tabulation, wide, so tiers can be compared within a band
#' @param .tab Output of orch_agreement_vs_prob().
#' @return Invisibly .tab.
orch_report_agreement_vs_prob <- function(.tab) {
  if (FALSE) .tab <- agree_prob_det
  cli::cli_h2("Agreement within transformer-confidence bands")
  .tab |>
    dplyr::mutate(Cell = paste0(tbl_pct(.data$Accuracy), " (", .data$nDocs, ")")) |>
    dplyr::select(ProbBand, Tier, Cell) |>
    tidyr::pivot_wider(names_from = Tier, values_from = Cell, values_fill = "-") |>
    tbl_say()
  cli::cli_text("")
  cli::cli_alert_info(
    "Band 1 is the least confident quarter, band {max(.tab$ProbBand)} the most. Read ACROSS a row: \\
     tiers still separating at equal probability means agreement carries information the \\
     probability does not, and the flag is worth shipping. Rows that are flat mean it does not."
  )
  invisible(.tab)
}

#' Write the confidence flag definition and its estimated reliability
#'
#' 03F recomputes the tier for every document in the corpus -- it can, since the pattern needs no
#' labels -- and attaches the reliability estimated here. Splitting it this way is what keeps the
#' estimate out of the apply stage: 03F does arithmetic on a rule it was handed, and cannot quietly
#' re-estimate reliability on a corpus that has no labels to estimate it from.
#'
#' @param .agreement Output of orch_agreement().
#' @param .arms Arms permitted to vote, recorded so the flag is reproducible.
#' @param .label_col Task the flag applies to.
#' @param .dir Output directory.
#' @param .stem File stem.
#' @return Invisibly the directory.
orch_save_confidence <- function(.agreement, .arms, .label_col, .dir, .stem = "confidence_flag") {
  if (FALSE) {
    .agreement <- agree_det
    .arms      <- c("legal-bert", "kw-text")
    .label_col <- "ClassDetailed"
    .dir       <- .lP$Output$Policy
  }
  fs::dir_create(.dir)
  arrow::write_parquet(
    .agreement |> dplyr::mutate(LabelCol = .label_col, .before = 1),
    fs::path(.dir, paste0(.stem, "_", .label_col, ".parquet"))
  )
  jsonlite::write_json(
    list(
      label_col   = .label_col,
      written_by  = "03E orch_save_confidence",
      written_at  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      voting_arms = as.list(.arms),
      tiers       = as.list(levels(.agreement$Tier)),
      note        = paste(
        "Tier is recomputed from the voting arms' predictions and needs no labels.",
        "Accuracy is estimated on the labelled sample and must not be re-estimated downstream."
      )
    ),
    fs::path(.dir, paste0(.stem, "_", .label_col, ".json")), auto_unbox = TRUE, pretty = TRUE
  )
  cli::cli_alert_success("Wrote the {(.label_col)} confidence flag over {length(.arms)} voting arms")
  invisible(.dir)
}

#' Accuracy by agreement tier, with the share each tier covers
#'
#' Both numbers in one frame, because either alone misleads: a tier can be almost perfectly accurate
#' and still cover too little of the corpus to be worth conditioning on. Accuracy is the bar height,
#' coverage the bar shading, and the printed label carries the document count so the reader is never
#' asked to judge a share from a shade alone.
#'
#' The accuracy axis is fixed at zero to one rather than zoomed to the tiers. These bars are read
#' against the per-category figures and against the arms' own accuracies, and a zoomed axis would turn
#' a two-point spread into an apparently decisive one.
#'
#' @param .tab Output of orch_agreement().
#' @param .key Character. Registered vocabulary ordering the tiers.
#' @return A ggplot.
orch_plot_agreement <- function(.tab, .key = "AgreementTier") {
  if (FALSE) {
    .tab <- agree_det
    .key <- "AgreementTier"
  }
  .tab |>
    dplyr::mutate(Tier = plot_factor(.data$Tier, .key = .key)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Tier, y = .data$Accuracy)) +
    ggplot2::geom_col(
      ggplot2::aes(alpha = .data$Share),
      width = 0.7, fill = .plot_ink, colour = .plot_ink, linewidth = 0.3
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(scales::label_percent(accuracy = 0.1)(.data$Accuracy),
                                  "\nn = ", scales::label_comma()(.data$nDocs))),
      vjust = -0.35, size = (.plot_base - 3) / ggplot2::.pt, family = .plot_font
    ) +
    ggplot2::scale_alpha_continuous(
      range = c(0.30, 1), limits = c(0, 1), labels = scales::label_percent(), name = "Share of corpus"
    ) +
    # drop = FALSE keeps every registered tier on the axis. A tier no document reached is a finding
    # about the arms -- with four of them committing on a categorical task, no document is decided by
    # a single arm -- and dropping it turns that into a silent absence the reader cannot see.
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1.14), breaks = seq(0, 1, by = 0.25),
      labels = scales::label_percent(), expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(x = NULL, y = "Accuracy of the shipped label") +
    plot_theme(.grid = "y", .legend = "right")
}


# 12. The deployment manifest ------------------------------------------------------------------------------------------
# Everything above concludes; this section writes those conclusions down in a form another process can
# execute. The distinction matters because the artifacts written so far name CONFIGURATIONS, and a
# configuration is not a model: `ClassDetailed__nlpaueb-legal-bert...__L256_E6...` identifies a recipe
# that was cross-validated, not a directory holding weights. A downstream stage handed only that
# string would have to reconstruct the path from a naming convention, and a naming convention shared
# by inference is a naming convention that will eventually be changed in one place and not the other.
#
# So the manifest resolves every arm to a concrete artifact on disk, records the parameters that
# artifact was validated under, and states which arms are on by default. It is the single file 03F
# reads, and the single thing that has to be archived alongside the released labels for anyone to
# reproduce them.
#
# ARTIFACT PATHS COME FROM THE DOCUMENT, NOT FROM A CONVENTION HERE
# 03B names its deployable model `<config>__FINAL/model` and 03C writes `<stem>.parquet`, and this
# file knows neither. The qmd builds the arm-to-artifact table using each upstream stage's own path
# helpers -- the same rule that keeps run roots from drifting -- and passes it in. What this function
# adds is verification: an arm whose artifact is missing is reported rather than written, because a
# manifest promising a model that is not there is worse than no manifest.

#' Estimate the confidence flag under several voting sets at once
#'
#' A tier's reliability is a property of WHO VOTED. Estimating it on three arms and then deploying
#' with two would ship numbers describing a vote that never happened, and nothing downstream could
#' detect the mismatch, because a two-arm pattern computed at deployment looks exactly like a
#' two-arm pattern computed here.
#'
#' So every voting set that might be used is estimated now, and the manifest records which is the
#' default. The generative arm is expensive at corpus scale, so a deployment that leaves it off is a
#' realistic case rather than a hypothetical, and it needs its own reliability table.
#'
#' @param .spine Routing spine.
#' @param .tab_pred Shipped predictions carrying TrueLabel.
#' @param .sets Named list of arm-name vectors, one per voting set.
#' @param .none Character. Abstention sentinel.
#' @return Tibble: SetName, Arms, Tier, nDocs, Share, Accuracy, ErrorRate, CumShare, CumAccuracy.
orch_agreement_sets <- function(.spine, .tab_pred, .sets, .none = ORCH_NONE) {
  if (FALSE) {
    .spine    <- res_det$Spine
    .tab_pred <- res_det$Nested
    .sets     <- list(core = c("legal-bert", "kw-text"))
    .none     <- ORCH_NONE
  }
  purrr::imap(.sets, function(.arms, .name) {
    arms_ <- intersect(.arms, unique(.spine$Arm))
    if (length(arms_) == 0L) {
      cli::cli_alert_warning("Voting set {(.name)} has no arms present in the spine; skipped.")
      return(NULL)
    }
    if (length(arms_) < length(.arms)) {
      cli::cli_alert_warning(
        "Voting set {(.name)} is missing {setdiff(.arms, arms_)}; estimated on what is present."
      )
    }
    pat_ <- orch_agreement_pattern(.spine = .spine, .tab_pred = .tab_pred, .arms = arms_,
                                   .none = .none)
    orch_agreement(.pattern = pat_, .tab_pred = .tab_pred) |>
      dplyr::mutate(SetName = .name, Arms = paste(arms_, collapse = " + "), .before = 1)
  }) |>
    purrr::list_rbind()
}

#' Report every voting set side by side
#'
#' The comparison that decides whether the expensive arm earns its place. A set that separates the
#' corpus no better than a cheaper one is not worth ten hours of inference, and that is a judgement a
#' reader should be able to make from one table rather than by holding two in their head.
#'
#' @param .tab Output of orch_agreement_sets().
#' @param .default Name of the set the manifest will mark as on by default.
#' @return Invisibly .tab.
orch_report_agreement_sets <- function(.tab, .default = NULL) {
  if (FALSE) {
    .tab     <- agree_sets
    .default <- "core"
  }
  cli::cli_h2("Confidence flag under each voting set")
  .tab |>
    dplyr::mutate(
      Share       = tbl_pct(.data$Share),
      Accuracy    = tbl_pct(.data$Accuracy),
      CumShare    = tbl_pct(.data$CumShare),
      CumAccuracy = tbl_pct(.data$CumAccuracy),
      ErrorRate   = NULL
    ) |>
    dplyr::select(SetName, Tier, nDocs, Share, Accuracy, CumShare, CumAccuracy) |>
    tbl_say()
  cli::cli_text("")

  top_ <- .tab |>
    dplyr::filter(.data$Tier == "unanimous") |>
    dplyr::select(SetName, Arms, Share, Accuracy)
  if (nrow(top_) > 0L) {
    top_ |>
      dplyr::mutate(Share = tbl_pct(.data$Share), Accuracy = tbl_pct(.data$Accuracy)) |>
      tbl_say(.title = "Top tier, set by set")
    cli::cli_text("")
    cli::cli_alert_info(
      "A set earns its cost by covering MORE of the corpus at a HIGHER accuracy in this row. A set \\
       that only raises accuracy by quarantining more documents has moved the threshold, not the \\
       information."
    )
  }
  if (!is.null(.default)) {
    cli::cli_alert_info("The manifest will mark {(.default)} as the set applied unless asked otherwise.")
  }
  invisible(.tab)
}

#' Every deployable transformer on disk, with the crowned one marked
#'
#' Discovered rather than constructed. The previous version built a path from a configuration name and
#' trusted it to exist, so a stage that had deployed a different configuration -- the trainer crowns
#' on macro-F1, this stage was ranking terminals on accuracy -- produced a manifest pointing at a
#' model nobody had fitted. Listing what is actually there cannot make that mistake, and it finds
#' every context length the trainer deployed rather than only the one this stage thought to ask for.
#'
#' A run directory is named for the configuration it holds, so the configuration is read back off the
#' directory rather than assumed. Anything that does not join to this task's arm inventory is dropped:
#' a model on disk that this task never evaluated is a leftover, not an arm.
#'
#' @param .arms Arm inventory for one task, from orch_arms().
#' @param .dir_bert Output root of the training stage.
#' @param .default Character. The arm the trainer crowned and deployed for this task.
#' @return Tibble: Arm, Kind, ConfigName, MaxLen, Path, Default, MacroF1.
orch_catalogue_bert <- function(.arms, .dir_bert, .default) {
  if (FALSE) {
    .arms     <- res_det$ArmsCV
    .dir_bert <- .dir_bert
    .default  <- res_det$Incumbent
  }
  root_ <- fs::path(.dir_bert, "model_final")
  if (!fs::dir_exists(root_)) {
    cli::cli_abort(c("No deployed models under {.path {(root_)}}.",
                     "i" = "Run the training stage's deployment section first."))
  }
  dirs_ <- fs::dir_ls(root_, type = "directory", glob = "*__FINAL")
  if (length(dirs_) == 0L) cli::cli_abort("No __FINAL directories under {.path {(root_)}}.")

  found_ <- tibble::tibble(
    ConfigName = sub("__FINAL$", "", fs::path_file(dirs_)),
    Path       = as.character(fs::path(dirs_, "model"))
  ) |>
    dplyr::filter(fs::dir_exists(.data$Path))

  out_ <- .arms |>
    dplyr::filter(!grepl("^keyword-", .data$Kind), !grepl("^llm-", .data$Kind)) |>
    dplyr::select(Arm, ConfigName, MaxLen, MacroF1) |>
    dplyr::inner_join(found_, by = dplyr::join_by(ConfigName)) |>
    dplyr::mutate(Kind = "transformer", MaxLen = as.integer(.data$MaxLen),
                  Default = .data$Arm == .default) |>
    dplyr::arrange(.data$MaxLen)

  if (nrow(out_) == 0L) {
    cli::cli_abort(c(
      "No deployed model joins this task's arm inventory.",
      "i" = "On disk: {utils::head(found_$ConfigName, 2)}",
      "i" = "In the inventory: {utils::head(.arms$ConfigName, 2)}"
    ))
  }
  if (!any(out_$Default)) {
    cli::cli_abort(c(
      "The deployed arm {.val {(.default)}} has no model on disk.",
      "i" = "Deployed here: {out_$Arm}",
      "i" = "A manifest naming an artifact that is absent is one the apply stage cannot execute."
    ))
  }
  dplyr::select(out_, Arm, Kind, ConfigName, MaxLen, Path, Default, MacroF1)
}

#' Every published keyword table on disk, with the marked one flagged
#'
#' The keyword stage publishes a table per truncation window and marks which to apply. Both travel:
#' the window is that arm's cost knob, and a caller with throughput figures may reasonably prefer a
#' cheaper table than the one the evidence marked. Reading the catalogue that stage wrote, rather than
#' rebuilding the choice here, is what stops the two from disagreeing.
#'
#' @param .dir_kw Output root of the keyword stage.
#' @param .label_col Character. Task.
#' @param .catalogue The keyword stage's published catalogue.
#' @return Tibble: Arm, Kind, NWords, Path, Default, Coverage, Realised.
orch_catalogue_keyword <- function(.dir_kw, .label_col, .catalogue) {
  if (FALSE) {
    .dir_kw    <- .dir_kw
    .label_col <- "ClassDetailed"
    .catalogue <- kw_cat
  }
  mine_ <- .catalogue |> dplyr::filter(.data$Task == .label_col)
  if (nrow(mine_) == 0L) {
    cli::cli_abort("The keyword catalogue lists no table for {(.label_col)}.")
  }

  out_ <- mine_ |>
    dplyr::mutate(
      Kind = "keyword",
      Arm  = paste0("kw-text:W",
                    dplyr::if_else(.data$NWords == 0L, "full", as.character(.data$NWords))),
      Path = purrr::map2_chr(.data$Task, .data$NWords, function(.t, .w) {
        as.character(fs::path(.dir_kw, "table", paste0(kw_table_stem(.t, .w), ".parquet")))
      })
    ) |>
    dplyr::filter(fs::file_exists(.data$Path))

  if (nrow(out_) == 0L) {
    cli::cli_abort(c(
      "The catalogue lists tables for {(.label_col)} but none is on disk.",
      "i" = "Expected under {.path {as.character(fs::path(.dir_kw, 'table'))}}."
    ))
  }
  if (!any(out_$Default)) cli::cli_abort("The marked table for {(.label_col)} is not on disk.")
  out_ |> dplyr::select(Arm, Kind, NWords, Path, Default, Coverage, Realised)
}

#' Assemble the deployable catalogue for one task
#'
#' This stage selects nothing. It reads what the training stage crowned and what the keyword stage
#' marked, finds the artifacts behind them, and records every variant beside the default so a later
#' decision can be made with the numbers in hand. Two stages have already answered which model is
#' best; a third opinion here would not be more information, it would be a disagreement.
#'
#' @param .res One task's result from orch_run_task().
#' @param .dir_bert Output root of the training stage.
#' @param .dir_kw Output root of the keyword stage.
#' @param .kw_catalogue The keyword stage's published catalogue.
#' @return Tibble: LabelCol, Arm, Kind, Variant, Path, Default, Score, Measure, MaxLen, NWords.
orch_artifacts <- function(.res, .dir_bert, .dir_kw, .kw_catalogue) {
  if (FALSE) {
    .res          <- res_det
    .dir_bert     <- .dir_bert
    .dir_kw       <- .dir_kw
    .kw_catalogue <- kw_cat
  }
  bert_ <- orch_catalogue_bert(
    .arms = .res$ArmsCV, .dir_bert = .dir_bert, .default = .res$Incumbent
  ) |>
    dplyr::transmute(
      LabelCol = .res$LabelCol, Arm, Kind, Path, Default,
      Variant = paste0("L", .data$MaxLen), MaxLen = .data$MaxLen, NWords = NA_integer_,
      Score = .data$MacroF1, Measure = "macro-F1, cross-validated"
    )

  kw_ <- orch_catalogue_keyword(
    .dir_kw = .dir_kw, .label_col = .res$LabelCol, .catalogue = .kw_catalogue
  ) |>
    dplyr::transmute(
      LabelCol = .res$LabelCol, Arm, Kind, Path, Default,
      Variant = paste0("W", dplyr::if_else(.data$NWords == 0L, "full",
                                           as.character(.data$NWords))),
      MaxLen = NA_integer_, NWords = .data$NWords,
      Score = .data$Coverage, Measure = "coverage at the published precision"
    )

  dplyr::bind_rows(bert_, kw_)
}

#' Print the catalogue this stage is about to pin
#'
#' Read before the manifest is written. Every row ships and every row is applicable; the marked one is
#' what runs unless a caller asks otherwise. Where a variant scores within noise of the default at a
#' fraction of the cost, this table is where that becomes visible.
#'
#' @param .tab Output of orch_artifacts(), bound across tasks.
#' @return Invisibly .tab.
orch_report_catalogue <- function(.tab) {
  if (FALSE) .tab <- tab_artifacts
  cli::cli_h2("Deployable catalogue")
  .tab |>
    dplyr::mutate(
      Default = dplyr::if_else(.data$Default, "<-", ""),
      Score   = sprintf("%.3f", .data$Score)
    ) |>
    dplyr::select(LabelCol, Kind, Variant, Default, Arm, Score, Measure) |>
    tbl_say()
  cli::cli_text("")
  cli::cli_alert_info(
    "The arrow marks what the training and keyword stages crowned. This stage records that choice \\
     rather than making one of its own, and ships every variant beside it so a later caller can \\
     trade accuracy against inference cost with the numbers in front of them."
  )
  cli::cli_alert_info(
    "Score is not comparable across kinds: a transformer row carries cross-validated macro-F1, a \\
     keyword row the share of the corpus its table labels at its published precision."
  )
  invisible(.tab)
}

#' Write the single file 03F reads
#'
#' Pins every decision this document reached, resolved to artifacts that exist. What a downstream
#' stage needs and cannot re-derive: which model file, at which context length, under which routing
#' rule, with which voting set behind the confidence flag, and what each of those scored when it was
#' validated. Recording the scores matters as much as the paths -- a manifest that says what to run
#' but not how well it ran invites deployment of a configuration nobody can defend.
#'
#' Arms are verified rather than trusted. An artifact that is absent is reported and the arm is
#' marked unavailable, so a manifest never promises a model that is not on disk.
#'
#' @param .results List of orch_run_task() outputs, one per task.
#' @param .artifacts Tibble: Arm, Kind, Path, plus any parameters worth pinning as a Params list
#'   column. Built in the document from each upstream stage's own path helpers.
#' @param .agreement Output of orch_agreement_sets().
#' @param .sets Named list of voting sets.
#' @param .default_set Name of the set applied unless a caller asks otherwise.
#' @param .default_kinds Arm KINDS enabled by default at deployment. Kinds rather than arm names,
#'   because a name is derived at run time and would have to be guessed in a configuration block. An
#'   arm pinned but not enabled is available to a caller who asks for it and skipped otherwise.
#' @param .dir Output directory.
#' @param .tab_prep Prepared sample, for the provenance block.
#' @return Invisibly the manifest list.
orch_save_manifest <- function(.results, .artifacts, .agreement, .sets, .default_set,
                               .default_kinds, .dir, .tab_prep) {
  if (FALSE) {
    .results      <- list(res_det, res_broad, res_amend)
    .artifacts    <- tab_artifacts
    .agreement    <- agree_sets
    .sets         <- sets_voting
    .default_set  <- "core"
    .default_kinds <- c("transformer", "keyword")
    .dir          <- .lP$Output$Deploy
    .tab_prep     <- tab_prep
  }
  fs::dir_create(.dir)

  # Everything reaching this point was discovered on disk, so existence is established rather than
  # asserted. What is checked here is the one property the catalogue cannot guarantee: that each task
  # has a marked variant of each kind, since the apply stage runs the marked one.
  found_ <- .artifacts
  gaps_  <- found_ |>
    dplyr::summarise(nDefault = sum(.data$Default), .by = c(LabelCol, Kind)) |>
    dplyr::filter(.data$nDefault != 1L)
  if (nrow(gaps_) > 0L) {
    cli::cli_abort(c(
      "Every task and kind needs exactly one marked variant; {nrow(gaps_)} do{?es/} not.",
      "i" = "{gaps_$LabelCol} / {gaps_$Kind}: {gaps_$nDefault} marked.",
      "i" = "A manifest without a default is one the apply stage cannot execute unattended."
    ))
  }

  thin_ <- found_ |>
    dplyr::filter(.data$Default, .data$Kind %in% .default_kinds) |>
    dplyr::count(LabelCol, name = "nEnabled") |>
    dplyr::filter(.data$nEnabled < 2L)
  if (nrow(thin_) > 0L) {
    cli::cli_alert_warning(
      "{nrow(thin_)} task{?s} deploy a single arm ({thin_$LabelCol}): a concurrence flag needs \\
       somebody to concur with, so those tasks ship a label without one."
    )
  }

  # One record per variant, not per arm. The apply stage runs the marked one unless a caller names
  # another, so both travel and the choice stays open. Params carry what inference needs to reproduce
  # the encoding the artifact was fitted or mined under -- the context length for a transformer, the
  # truncation window for a keyword table. Omitting either lets the apply stage fall back to a default
  # that has nothing to do with how the artifact was built, which is silent and wrong.
  arm_block_ <- function(.tab) {
    purrr::map(seq_len(nrow(.tab)), function(.i) {
      r_ <- .tab[.i, ]
      list(
        arm      = r_$Arm,
        kind     = r_$Kind,
        variant  = r_$Variant,
        artifact = as.character(fs::path_rel(r_$Path, here::here())),
        default  = isTRUE(r_$Default),
        enabled  = r_$Kind %in% .default_kinds,
        scored   = list(value = unname(r_$Score), measure = r_$Measure),
        params   = switch(r_$Kind,
          transformer = list(max_len = unname(r_$MaxLen)),
          keyword     = list(n_words = unname(r_$NWords), source = "text"),
          list()
        )
      )
    })
  }

  tasks_ <- purrr::map(.results, function(.r) {
    pol_  <- .r$Final$Policy
    cmp_  <- .r$Compare
    # Arms belong to the task, not to the study. Each task crowned its own configuration, so a
    # single global list pins the wrong checkpoint for every task but one.
    mine_ <- found_ |> dplyr::filter(.data$LabelCol == .r$LabelCol)
    list(
      label_col = .r$LabelCol,
      incumbent = .r$Incumbent,
      # I() marks this AsIs, which is what stops auto_unbox writing a one-element array as a bare
      # object. Two of the three tasks deploy a single arm, so without it their arms field reads
      # back as the arm itself rather than as a list holding it, and iterating it yields the arm's
      # FIELD NAMES instead of arm records -- quietly, producing no rows rather than an error.
      arms      = I(arm_block_(.tab = mine_)),
      policy    = list(
        family   = pol_$Family,
        terminal = pol_$Terminal,
        floor    = pol_$Floor,
        order    = I(as.list(pol_$Order[[1]])), # AsIs: a one-arm cascade must stay an array
        label    = pol_$Label
      ),
      validated = list(
        estimate     = "nested five-fold selection on the labelled sample",
        macro_f1     = cmp_$MacroF1[cmp_$Strategy == "Nested selection (the procedure)"][1],
        accuracy     = cmp_$Accuracy[cmp_$Strategy == "Nested selection (the procedure)"][1],
        incumbent_f1 = cmp_$MacroF1[grepl("(incumbent)", cmp_$Strategy, fixed = TRUE)][1]
      )
    )
  })
  names(tasks_) <- purrr::map_chr(.results, "LabelCol")

  man_ <- list(
    manifest_version = 1L,
    written_by       = "03E orch_save_manifest",
    written_at       = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    sample           = list(
      n_labelled = nrow(.tab_prep),
      n_folds    = dplyr::n_distinct(.tab_prep$Fold)
    ),
    tasks      = tasks_,
    confidence = list(
      # Estimated on one task. A tier means "the arms concurred", and only a task with more than one
      # deployable arm has anything to concur about.
      label_col   = .results[[1]]$LabelCol,
      sets        = purrr::map(.sets, \(.x) I(as.list(.x))), # AsIs, for the same reason
      default_set = .default_set,
      reliability = "confidence_flag.parquet",
      note        = paste(
        "Tier is recomputed from the voting arms and needs no labels.",
        "Accuracy is estimated here and must not be re-estimated downstream.",
        "A deployment using a different voting set must read that set's reliability row."
      )
    )
  )

  jsonlite::write_json(man_, fs::path(.dir, "manifest.json"), auto_unbox = TRUE, pretty = TRUE,
                       null = "null")
  arrow::write_parquet(.agreement, fs::path(.dir, "confidence_flag.parquet"))

  cli::cli_alert_success(
    "Wrote manifest for {length(tasks_)} task{?s} over {nrow(found_)} variant{?s}; \\
     {sum(found_$Kind %in% .default_kinds)} enabled by default."
  )
  invisible(man_)
}


# 13. Figures ------------------------------------------------------------------------------------------------------------
# The look comes entirely from _Commons/_Plots.R. What these functions own is the mapping from a
# result object to a figure shape.
#
# FILL MEANS OBTAINABLE. Both bar figures below reserve fill for one distinction: whether the number
# could be reproduced by running the procedure. A policy selected and scored on the same documents is
# drawn hollow, because a reader comparing bar lengths would otherwise conclude that routing beat the
# incumbent by a margin nobody can obtain. That is the single misreading this document exists to
# prevent, so it is encoded rather than left to the caption.

#' Macro-F1 by strategy, with the incumbent marked
#'
#' The dashed line is the single arm the routing has to beat. Bars are on a fixed zero-to-one axis so
#' the three tasks can be read against each other and against the arms' own leaderboards; the
#' differences the search is chasing are small, and an axis zoomed to them would make a fourth-decimal
#' gap look like a result.
#'
#' @param .compare Output of orch_compare().
#' @return A ggplot.
orch_plot_compare <- function(.compare) {
  if (FALSE) .compare <- res_det$Compare
  base_ <- .compare$MacroF1[grepl("(incumbent)", .compare$Strategy, fixed = TRUE)][1]

  .compare |>
    dplyr::mutate(
      Strategy = forcats::fct_reorder(.data$Strategy, .data$MacroF1),
      Estimate = dplyr::if_else(.data$Honest, "Obtainable", "Selected in sample")
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$MacroF1, y = .data$Strategy, fill = .data$Estimate)) +
    ggplot2::geom_col(width = 0.7, colour = .plot_ink, linewidth = 0.3) +
    ggplot2::geom_vline(
      xintercept = base_, linetype = 2, linewidth = 0.3, colour = .plot_ref
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", .data$MacroF1)),
      hjust = -0.18, size = (.plot_base - 3) / ggplot2::.pt, family = .plot_font
    ) +
    ggplot2::scale_fill_manual(
      values = c(Obtainable = .plot_ink, `Selected in sample` = "#FFFFFF"), name = NULL
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1.08), breaks = seq(0, 1, by = 0.25),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(x = "Macro-F1 (pooled out-of-fold)", y = NULL) +
    plot_theme(.grid = "none", .legend = "bottom")
}

#' Documents the crowned policy moved, per held-out fold
#'
#' The honest picture of stability, and the figure most likely to contradict the winner names in the
#' tables above. A fold routing nothing ran the incumbent whatever its winning policy was called, so
#' bars rather than names are what a reader should count. Every fold appears even at zero: an absent
#' bar and a zero bar mean opposite things, and only one of them is true here.
#'
#' @param .tab_nested Output of orch_select_nested().
#' @return A ggplot.
orch_plot_stability <- function(.tab_nested) {
  if (FALSE) .tab_nested <- res_det$Nested

  dat_ <- .tab_nested |>
    dplyr::summarise(nRouted = sum(.data$DecidedBy != .data$WinnerTerminal), .by = Fold) |>
    tidyr::complete(Fold = sort(unique(.tab_nested$Fold)), fill = list(nRouted = 0L)) |>
    dplyr::mutate(Fold = factor(.data$Fold))

  # Documents are counted, so the axis takes whole numbers. Left to pick its own breaks a continuous
  # scale lands on fifths, and the all-zero case -- the expected outcome here rather than an anomaly --
  # renders an axis reading 0.00 to 0.15 for a quantity that cannot be fractional. The ceiling is at
  # least one, so a figure where nothing routed still has an axis to be flat against.
  top_  <- max(1L, max(dat_$nRouted, 0L))
  step_ <- max(1L, ceiling(top_ / 4))

  dat_ |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Fold, y = .data$nRouted)) +
    ggplot2::geom_col(width = 0.6, fill = .plot_ink) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::label_comma()(.data$nRouted)),
      vjust = -0.45, size = (.plot_base - 3) / ggplot2::.pt, family = .plot_font
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0L, top_, by = step_),
      limits = c(0, top_ * 1.18),
      labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    # "Documents routed away from the terminal" is thirty-nine characters, and rotated onto a panel
    # 2.4 inches tall it runs off both ends. Width is fixed and height comes from the row count, so a
    # label that does not fit is shortened rather than accommodated; the caption carries what the
    # short form drops.
    ggplot2::labs(x = "Held-out fold", y = "Documents routed") +
    plot_theme(.grid = "y", .legend = "none")
}
