# 03D-ClassifyOrchestrate: routing among classification arms (orch_*) ----
#
# WHAT THIS STAGE IS
# 03B and 03C each produced out-of-fold predictions on the IDENTICAL folds dealt in 03A. This file
# asks, for each of the three tasks, whether a rule routing between them beats the transformer on its
# own -- and answers in a form a referee can accept: the rule is CHOSEN inside the folds, not across
# them.
#
# WHY NESTED SELECTION IS THE WHOLE DESIGN
# Enumerating routing rules on the pooled predictions and reporting the best one selects and
# evaluates on the same documents. With a few dozen candidate rules separated by a fraction of a
# percentage point, the winner of that search is partly a winner by luck. So the rule is ranked on
# four folds and applied to the fifth, rotating. What is reported is the performance of the
# PROCEDURE. The in-sample best is reported beside it, because the gap between them measures the
# selection effect directly.
#
# TOLERANCE, AND WHY IT IS NOT OPTIONAL
# A strict argmax over policies will crown a rule that leads by the fourth decimal on a dozen
# documents. That is not a preference for routing, it is a rounding artifact, and left unchecked it
# makes the fold-level winner change for no reason and writes a deployment artifact that contradicts
# the document's own conclusion. Every ranking here therefore resolves through orch_pick_policy():
# among policies within .tolerance of the best, take the one that ROUTES THE FEWEST DOCUMENTS. The
# incumbent routes none, so it wins every tie by construction, and routing has to earn its place by a
# margin the sample can actually measure. 03C's kw_choose_config() applies the same principle to
# precision; this is that convention carried across.
#
# THE BINARY ASYMMETRY (why amendment needed a decision, not a workaround)
# For AmendType the keyword engine mines the positive class only and assigns "Original" wherever no
# amendment term fired. That label is the ABSENCE of evidence wearing the name of a class: coverage
# reads as 100%, the arm looks like a terminal, and the cascade space collapses to "pick one method".
# 03C states the asymmetry in its own bundle script -- "Original is defined by the ABSENCE of
# amendment language". So .commit_only names the labels that count as commitments: "Amended" for
# amendment, everything for the categorical tasks. The keyword arm then commits where a term fired
# and abstains otherwise, and the routing question becomes the real one: if an amendment term fires,
# call it amended, else ask the transformer. 03B predicts this fails, because "amended and restated"
# titles genuine originals. It is now testable rather than assumed.
#
# ARM AVAILABILITY IS NOT UNIFORM
# Mined keyword arms exist on all five folds. Arms scored from a written term list exist on the
# held-out fold only, because the reading session that produced the list read folds 1-4 WITH their
# labels. They cannot enter a five-fold nested search without contaminating four of the rankings, so
# orch_arms() reports fold availability and the restriction is applied in the qmd where a reader can
# see it.
#
# VOCABULARY (one place; do not drift)
#   Arm        One source of per-document predictions scored on the shared folds. It either COMMITS
#              to a label or ABSTAINS ("(none)").
#   Terminal   An arm that commits on every document, so it can end a cascade.
#   Cascade    An ordered list of abstaining arms, each gated by a score floor, ending in a terminal.
#   Policy     A cascade plus the family governing whether commitments are honoured.
#   Family     "cascade"  -- honour every commitment clearing the floor.
#              "perclass" -- honour a commitment only for classes where that arm beat the terminal on
#                            the TRAINING folds. Deployable, since it uses training labels only.
#
# Everything here is a pure consumer of what 03A, 03B and 03C wrote. No training, no engine spawn, no
# cached reads: the searches are joins over a few thousand rows and complete in seconds.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new; if (FALSE) dev blocks; cli/fs/here;
# pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .runs_roots <- c(.lP$Runs$Bert, .lP$Runs$Kw)
  .label_col  <- "ClassDetailed"
  .tab_prep   <- arrow::read_parquet(.lP$Input$Prepared)
}

# The abstention sentinel. 03C defines the same constant; repeating it means 03D's functions carry a
# working default even when only 03A has been sourced ahead of them.
ORCH_NONE <- "(none)"


# 1. Arm naming ------------------------------------------------------------------------------------
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

  base_ <- dplyr::if_else(
    grepl("^keyword-", .tab$Kind),
    paste0("kw-", sub("^keyword-", "", .tab$Kind)),
    clf_model_short(.model = .tab$Kind)
  )
  base_ <- sub("^kw-docdesc$", "kw-desc", base_)

  cand_ <- c("TermsTag", "Stopwords", "NWords", "NgramMax", "MinReach", "MaxTerms", "Tau",
             "MaxLen", "Epochs", "LR", "ClassWeights")
  cand_ <- intersect(cand_, names(.tab))
  abbr_ <- c(TermsTag = "", Stopwords = "SW", NWords = "W", NgramMax = "N", MinReach = "R",
             MaxTerms = "M", Tau = "T", MaxLen = "L", Epochs = "E", LR = "LR", ClassWeights = "CW")

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


# 2. Arm inventory ---------------------------------------------------------------------------------

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

  # Only the columns that exist across both engines; keyword runs carry selection axes, transformer
  # runs carry hyperparameters, and each is NA on the other's rows.
  keep_ <- intersect(
    c("Source", "Stopwords", "NWords", "NgramMax", "MinReach", "MaxTerms", "Tau", "Origin",
      "TermsFile", "MaxLen", "Epochs", "LR", "ClassWeights"),
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

  stats_ <- purrr::map(best_$ConfigName, function(.cfg) {
    pred_ <- clf_pool_predictions(.runs_roots = .runs_roots, .config_name = .cfg)
    hit_  <- pred_$PredLabel != .none
    tibble::tibble(
      ConfigName   = .cfg,
      nDocs        = nrow(pred_),
      Coverage     = mean(hit_),
      SelPrecision = if (any(hit_)) mean(pred_$PredLabel[hit_] == pred_$TrueLabel[hit_]) else NA_real_,
      Accuracy     = mean(pred_$PredLabel == pred_$TrueLabel)
    )
  }) |>
    purrr::list_rbind()

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
      Coverage     = clf_pct(.data$Coverage),
      SelPrecision = clf_pct(.data$SelPrecision),
      MacroF1      = sprintf("%.3f", .data$MacroF1)
    ) |>
    dplyr::select(Arm, dplyr::any_of("Origin"), nConfigs, Folds, nDocs, Coverage, SelPrecision,
                  MacroF1) |>
    clf_say_table()

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


# 3. The routing spine -----------------------------------------------------------------------------

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
  out_ <- purrr::pmap(
    .l = list(.arms$Arm, .arms$ConfigName, .arms$Kind),
    .f = function(.arm, .cfg, .kind) {
      tab_ <- clf_pool_predictions(.runs_roots = .runs_roots, .config_name = .cfg) |>
        dplyr::transmute(
          DocID,
          Fold  = as.integer(.data$Fold),
          TrueLabel,
          Arm   = .arm,
          Pred  = .data$PredLabel,
          Score = dplyr::coalesce(as.numeric(.data$Score), 0)
        )
      if (!is.null(.commit_only) && grepl("^keyword-", .kind)) {
        tab_ <- tab_ |>
          dplyr::mutate(Pred = dplyr::if_else(.data$Pred %in% .commit_only, .data$Pred, .none))
      }
      tab_
    }
  ) |>
    purrr::list_rbind()

  doc_ <- .tab_prep |> dplyr::select(DocID, dplyr::any_of(.carry))

  out_ |>
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
    dplyr::mutate(Coverage = clf_pct(.data$Coverage), Accuracy = clf_pct(.data$Accuracy)) |>
    dplyr::select(Arm, Role, nRows, nDocs, nFolds, Coverage, Accuracy) |>
    clf_say_table()
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


# 4. Derived arms ----------------------------------------------------------------------------------
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
    dplyr::mutate(Share = clf_pct(.data$Share)) |>
    clf_say_table()
  cli::cli_text("")
  reach_ <- .tab$Share[.tab$Outcome == "Wrong, different broad parent"]
  reach_ <- if (length(reach_) == 0L) 0 else reach_
  cli::cli_alert_info(
    "Only the crossing row is reachable by a broad-level constraint, and only where the broad model \\
     is right. It bounds the constrained arm's possible gain at {clf_pct(reach_)} of documents, \\
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


# 5. The policy space ------------------------------------------------------------------------------

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


# 6. Fitting and applying a policy -----------------------------------------------------------------

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
orch_policy_fit <- function(.spine, .policy, .none = ORCH_NONE) {
  if (FALSE) {
    .spine  <- dplyr::filter(spine_det, Fold != 1L)
    .policy <- policies_det[10, ]
    .none   <- ORCH_NONE
  }
  order_ <- .policy$Order[[1]]
  empty_ <- tibble::tibble(Arm = character(), Class = character(), nDocs = integer(),
                           ArmPrecision = numeric(), TerminalAccuracy = numeric(), Keep = logical())
  if (.policy$Family != "perclass" || length(order_) == 0L) return(empty_)

  term_ <- .spine |>
    dplyr::filter(.data$Arm == .policy$Terminal) |>
    dplyr::select(DocID, TermPred = .data$Pred)

  .spine |>
    dplyr::filter(.data$Arm %in% order_, .data$Pred != .none, .data$Score >= .policy$Floor) |>
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
#' @return Predictions tibble: DocID, Fold, TrueLabel, PredLabel, DecidedBy (+ carried columns).
orch_policy_apply <- function(.spine, .policy, .fit = NULL, .none = ORCH_NONE) {
  if (FALSE) {
    .spine  <- dplyr::filter(spine_det, Fold == 1L)
    .policy <- policies_det[10, ]
    .fit    <- orch_policy_fit(dplyr::filter(spine_det, Fold != 1L), policies_det[10, ])
    .none   <- ORCH_NONE
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
    cand_ <- .spine |>
      dplyr::filter(.data$Arm == arm_, .data$Pred != .none, .data$Score >= .policy$Floor) |>
      dplyr::select(DocID, ArmPred = .data$Pred)

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


# 7. Ranking and nested selection ------------------------------------------------------------------

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

  purrr::map(seq_len(nrow(.policies)), function(.i) {
    pol_  <- .policies[.i, ]
    fit_  <- orch_policy_fit(.spine = fit_on_, .policy = pol_, .none = .none)
    pred_ <- orch_policy_apply(.spine = .spine, .policy = pol_, .fit = fit_, .none = .none)
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
orch_select_nested <- function(.spine, .policies, .tolerance = 0.005, .lenient = FALSE,
                               .none = ORCH_NONE) {
  if (FALSE) {
    .spine     <- spine_det
    .policies  <- policies_det
    .tolerance <- 0.005
  }
  folds_ <- sort(unique(.spine$Fold))
  cli::cli_alert_info(
    "Nested selection: {nrow(.policies)} polic{?y/ies} ranked within each of {length(folds_)} fold{?s}"
  )

  purrr::map(folds_, function(.k) {
    train_ <- .spine |> dplyr::filter(.data$Fold != .k)
    test_  <- .spine |> dplyr::filter(.data$Fold == .k)

    rank_ <- orch_score_policies(.spine = train_, .policies = .policies, .lenient = .lenient,
                                 .none = .none)
    win_  <- .policies |>
      dplyr::filter(.data$PolicyID == orch_pick_policy(.rank = rank_, .tolerance = .tolerance)$PolicyID)
    fit_  <- orch_policy_fit(.spine = train_, .policy = win_, .none = .none)

    orch_policy_apply(.spine = test_, .policy = win_, .fit = fit_, .none = .none) |>
      dplyr::mutate(WinnerID = win_$PolicyID, WinnerLabel = win_$Label,
                    WinnerTerminal = win_$Terminal)
  }, .progress = "folds") |>
    purrr::list_rbind()
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
  per_fold_ |> clf_say_table()
  cli::cli_text("")
  counts_ |> clf_say_table(.title = "Distinct winners")
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


# 8. The headline comparison -----------------------------------------------------------------------

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
      Accuracy     = clf_pct(.data$Accuracy),
      MacroF1      = sprintf("%.3f", .data$MacroF1),
      DeltaMacroF1 = sprintf("%+.3f", .data$DeltaMacroF1)
    ) |>
    dplyr::select(Strategy, Honest, Accuracy, MacroF1, DeltaMacroF1, Note) |>
    clf_say_table()
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
    dplyr::mutate(Share = clf_pct(.data$Share)) |>
    clf_say_table()
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


# 9. Deployment ------------------------------------------------------------------------------------

#' Choose the policy to deploy, fitted on every fold
#'
#' The folds pay for the estimate; the full sample produces the artifact. This mirrors the convention
#' the transformer stage already follows, and the same tolerance applies, so the artifact cannot
#' contradict the document's own conclusion by preferring a fourth-decimal winner.
#'
#' @param .spine Routing spine.
#' @param .policies Output of orch_policy_grid().
#' @param .tolerance Macro-F1 band treated as indistinguishable.
#' @param .lenient Logical. Lenient scoring during selection.
#' @param .none Character. Abstention sentinel.
#' @return List: Policy (one-row tibble), Fit (per-class gate), Rank (full leaderboard).
orch_policy_final <- function(.spine, .policies, .tolerance = 0.005, .lenient = FALSE,
                              .none = ORCH_NONE) {
  if (FALSE) {
    .spine     <- spine_det
    .policies  <- policies_det
    .tolerance <- 0.005
  }
  rank_ <- orch_score_policies(.spine = .spine, .policies = .policies, .lenient = .lenient,
                               .none = .none)
  pick_ <- orch_pick_policy(.rank = rank_, .tolerance = .tolerance)
  pol_  <- .policies |> dplyr::filter(.data$PolicyID == pick_$PolicyID)
  list(
    Policy = pol_,
    Fit    = orch_policy_fit(.spine = .spine, .policy = pol_, .none = .none),
    Rank   = rank_
  )
}

#' Write the deployment decision 03E consumes
#'
#' 03E applies a model to the full corpus and should make no decisions of its own. Everything it
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
    order       = as.list(order_),
    commit_only = if (is.null(.commit_only)) NULL else as.list(.commit_only),
    label       = pol_$Label,
    arms        = .arms |>
      dplyr::filter(.data$Arm %in% used_) |>
      dplyr::select(Arm, ConfigName, Kind) |>
      purrr::transpose()
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


# 10. One task, end to end -------------------------------------------------------------------------

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
#' @param .extra_arms Optional spine-form rows for a derived arm, or NULL.
#' @param .floors,.max_depth,.families Policy-space controls.
#' @param .commit_only Labels a keyword arm may commit to, or NULL for all.
#' @param .n_folds Folds an arm must cover to enter the nested search.
#' @param .tolerance Macro-F1 band treated as indistinguishable.
#' @param .lenient Logical. Lenient scoring inside the selection.
#' @param .carry Document-level columns to attach to the spine.
#' @param .none Character. Abstention sentinel.
#' @return List: LabelCol, Arms, ArmsCV, ArmsHeld, Spine, Roles, Incumbent, Terminals, Gated,
#'   Policies, Nested, Compare, Movement, Final.
orch_run_task <- function(.runs_roots, .label_col, .tab_prep,
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
  term_  <- roles_$Arm[roles_$Role == "terminal"]
  gated_ <- roles_$Arm[roles_$Role == "gated"]
  if (length(term_) == 0L) cli::cli_abort("{(.label_col)}: no terminal arm; cannot route.")
  inc_ <- roles_ |> dplyr::filter(.data$Role == "terminal") |>
    dplyr::slice_max(.data$Accuracy, n = 1L, with_ties = FALSE) |> dplyr::pull(Arm)

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
    Final     = orch_policy_final(.spine = spine_, .policies = policies_, .tolerance = .tolerance,
                                  .lenient = .lenient, .none = .none)
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
    clf_say_table()
  cli::cli_text("")
  cli::cli_alert_info(
    "Moved counts documents the nested procedure labelled differently from the incumbent. Zero \\
     there means the procedure reduced to the incumbent on every fold."
  )
  invisible(.tab)
}


# 11. Figures --------------------------------------------------------------------------------------

#' Macro-F1 by strategy, with the incumbent marked
#'
#' The in-sample bar is drawn hollow because it is not a number anyone can obtain by running the
#' procedure. Filling it like the others would invite the misreading this document exists to prevent.
#'
#' @param .compare Output of orch_compare().
#' @return A ggplot.
orch_plot_compare <- function(.compare) {
  if (FALSE) .compare <- res_det$Compare
  base_ <- .compare$MacroF1[grepl("(incumbent)", .compare$Strategy, fixed = TRUE)][1]
  p_ <- .compare |>
    dplyr::mutate(Strategy = forcats::fct_reorder(.data$Strategy, .data$MacroF1)) |>
    ggplot2::ggplot(ggplot2::aes(x = Strategy, y = MacroF1, fill = Honest)) +
    ggplot2::geom_col(width = 0.7, color = "grey20") +
    ggplot2::geom_hline(yintercept = base_, linetype = "dashed", color = "grey40") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", MacroF1)), hjust = -0.15, size = 3) +
    ggplot2::scale_fill_manual(
      values = c(`TRUE` = "grey30", `FALSE` = "white"),
      labels = c(`TRUE` = "Obtainable", `FALSE` = "Selected in sample")
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1.08), expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Macro-F1 (pooled out-of-fold)", fill = NULL)
  clf_apply_theme(.plot = p_)
}

#' Documents the crowned policy moved, per held-out fold
#'
#' The honest picture of stability. A fold routing nothing ran the incumbent whatever its winner was
#' called, so bars rather than winner names are what a reader should count.
#'
#' @param .tab_nested Output of orch_select_nested().
#' @return A ggplot.
orch_plot_stability <- function(.tab_nested) {
  if (FALSE) .tab_nested <- res_det$Nested
  p_ <- .tab_nested |>
    dplyr::summarise(nRouted = sum(.data$DecidedBy != .data$WinnerTerminal), .by = Fold) |>
    ggplot2::ggplot(ggplot2::aes(x = factor(Fold), y = nRouted)) +
    ggplot2::geom_col(width = 0.6, fill = "grey30", color = "grey20") +
    ggplot2::geom_text(ggplot2::aes(label = nRouted), vjust = -0.5, size = 3) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(x = "Held-out fold", y = "Documents routed away from the terminal")
  clf_apply_theme(.plot = p_)
}
