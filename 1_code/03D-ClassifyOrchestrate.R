# 03D-ClassifyOrchestrate: per-class routing + optional Ollama tiebreak (orch_*) ----
# 03D is the ORCHESTRATOR: it takes the pooled out-of-fold predictions that 03B
# (BERT) and 03C (keyword) already wrote, on the IDENTICAL frozen folds, and asks
# the one question 03D exists to answer -- does routing between the two methods (or
# an LLM tiebreak on the hard cases) beat just running BERT everywhere? Given the
# head-to-head (BERT dominates keyword on every class -- state doc 2.4), the honest
# expectation is "no, or barely", and a clean null IS the finding. So this file is
# built to make that test legible, not to manufacture a win.
#
# Everything here is a PURE CONSUMER of the shared layer. No training, no engine
# spawn, except the optional Ollama tiebreak (which is the only thing that calls a
# process). The inputs are:
#   - 03A: clf_pool_predictions / clf_scores / clf_perclass / clf_apply_theme
#   - 03B: bert_classification (per-doc BERT preds + Top1Prob + Margin),
#          bert_crowned_config (the crowned BERT recipe)
#   - 03C: kw_calibrate / kw_gate (the per-class high-precision keyword gate)
# Source 03A + 03B + 03C alongside this file (the qmd does), then this.
#
# Vocabulary (one place, do not drift):
#   Agreement  = BERT and keyword commit to the SAME label on a doc.
#   Residue    = the disagreement set (incl. keyword abstain) -- the only place a
#                tiebreak can change anything.
#   Oracle     = a DIAGNOSTIC ceiling that peeks at the true label to pick the
#                better method per class. NOT deployable; it bounds what routing
#                could ever buy. If oracle ~ BERT, routing is dead.
#   Tiebreak   = how the residue is resolved: "bert" (default), "keyword", or
#                "ollama" (qwen3:32b adjudicates, header-only, temp 0, JSON,
#                hash-cached).
#
# Deployability is a first-class column on every result, because the cleanest 03D
# story is: here is the incumbent (BERT), here is the unattainable ceiling
# (oracle), and here is what every DEPLOYABLE router actually scores against the
# incumbent. We never quietly report the oracle as if it were shippable.
#
# House style: native pipe; explicit package::function; dot-prefixed args;
# underscore-suffixed locals; .data$ for existing columns, bare CamelCase for new;
# if (FALSE) dev blocks; cli/fs/here; pure ASCII; stringi::stri_sub never substr;
# {(.arg)} parens in cli interpolation.

if (FALSE) {
  .runs_roots <- c(.lP$Runs$Bert, .lP$Runs$Kw)
  .bert_config <- bert_crowned_config(tab_overall, "ClassDetailed")$config_name
  .kw_config   <- best_kw
  .tab_prep    <- arrow::read_parquet(.lP$Input$Prepared)
}


# Assemble: one row per doc, both methods side by side --------------------

#' Assemble the routing table: BERT and keyword predictions per document
#'
#' The spine every router consumes. Pulls the crowned BERT config's per-doc
#' classification (argmax label + Top1Prob + Margin via bert_classification) and
#' the best keyword config's pooled predictions (argmax label + Score, "(none)"
#' where it abstained), and inner-joins them on DocID. Because both tracks ran the
#' IDENTICAL frozen folds, every doc appears once on each side and the join is
#' complete -- a doc missing from one side signals an upstream sweep gap, which is
#' surfaced as a warning rather than silently dropped.
#'
#' Keyword predictions are carried RAW (argmax + Score); gating is a router-level
#' concern (orch_route_selective gates internally). ClassDetailed2 and LabelRound
#' ride along from the prepared sample so downstream lenient / round-sliced scoring
#' works without a re-join.
#'
#' @param .runs_roots Character vector of runs roots (BERT root, keyword root).
#' @param .bert_config ConfigName of the crowned BERT config (from the leaderboard).
#' @param .kw_config ConfigName of the best keyword config.
#' @param .tab_prep Prepared sample (DocID, ClassDetailed2, LabelRound).
#' @param .none Character. Keyword abstention sentinel (default "(none)").
#' @return Tibble: DocID, TrueLabel, Fold, BertPred, BertProb, BertMargin, KwPred,
#'   KwScore, Agree, ClassDetailed2, LabelRound (one row per doc).
orch_assemble <- function(.runs_roots, .bert_config, .kw_config, .tab_prep,
                          .none = "(none)") {
  if (FALSE) {
    .runs_roots  <- c(.lP$Runs$Bert, .lP$Runs$Kw)
    .bert_config <- best_det_
    .kw_config   <- best_kw
    .tab_prep    <- tab_prep
    .none        <- "(none)"
  }

  bert_ <- bert_classification(.runs_roots, .bert_config, .tab_prep = .tab_prep) |>
    dplyr::select(DocID, Fold, TrueLabel,
                  BertPred = PredLabel, BertProb = Top1Prob, BertMargin = Margin,
                  dplyr::any_of(c("ClassDetailed2", "LabelRound")))

  kw_ <- clf_pool_predictions(.runs_roots, .kw_config) |>
    dplyr::select(DocID, KwPred = PredLabel, KwScore = Score)

  n_bert_ <- nrow(bert_)
  n_kw_   <- nrow(kw_)
  out_ <- bert_ |> dplyr::inner_join(kw_, by = dplyr::join_by(DocID))

  if (nrow(out_) < max(n_bert_, n_kw_)) {
    cli::cli_alert_warning(
      "Join kept {nrow(out_)} of BERT {n_bert_} / keyword {n_kw_} docs -- a sweep gap on one side?"
    )
  }

  out_ <- out_ |>
    dplyr::mutate(Agree = .data$KwPred != .none & .data$BertPred == .data$KwPred)

  cli::cli_alert_info(
    "Assembled {nrow(out_)} docs -- agree on {sum(out_$Agree)} ({scales::percent(mean(out_$Agree), 0.1)})"
  )
  out_ |>
    dplyr::relocate(DocID, TrueLabel, Fold, BertPred, BertProb, BertMargin,
                    KwPred, KwScore, Agree)
}


# Per-class head-to-head (the table that motivates -- or kills -- routing) --

#' Per-class F1: BERT vs keyword, with the winner and the gap
#'
#' Scores each method's pooled predictions through 03A's clf_perclass on identical
#' folds, joins per class, and names the winner and the F1 gap. This is the single
#' table 03D rests on: if BERT wins every row by a wide margin (the expected case),
#' routing for accuracy cannot help, and the deployable routers below are predicted
#' to tie BERT. Keyword's strict per-class F1 is reported here (abstain = miss);
#' its high-precision contribution is the SELECTIVE story (orch_route_selective),
#' not this aggregate.
#'
#' @param .runs_roots Character vector of runs roots.
#' @param .bert_config Crowned BERT ConfigName.
#' @param .kw_config Best keyword ConfigName.
#' @param .none Character. Keyword abstention sentinel.
#' @return Tibble: Label, F1_BERT, F1_KW, Gap (BERT - KW), Winner, Support
#'   (descending by Support).
orch_perclass_compare <- function(.runs_roots, .bert_config, .kw_config,
                                  .none = "(none)") {
  if (FALSE) {
    .runs_roots  <- c(.lP$Runs$Bert, .lP$Runs$Kw)
    .bert_config <- best_det_
    .kw_config   <- best_kw
  }
  b_ <- clf_perclass(clf_pool_predictions(.runs_roots, .bert_config), .none = .none) |>
    dplyr::select(Label, F1_BERT = F1, Support)
  k_ <- clf_perclass(clf_pool_predictions(.runs_roots, .kw_config), .none = .none) |>
    dplyr::select(Label, F1_KW = F1)

  b_ |>
    dplyr::left_join(k_, by = dplyr::join_by(Label)) |>
    dplyr::mutate(
      F1_KW  = dplyr::coalesce(.data$F1_KW, 0),
      Gap    = .data$F1_BERT - .data$F1_KW,
      Winner = dplyr::if_else(.data$F1_BERT >= .data$F1_KW, "BERT", "keyword")
    ) |>
    dplyr::select(Label, F1_BERT, F1_KW, Gap, Winner, Support) |>
    dplyr::arrange(dplyr::desc(.data$Support))
}

#' Named method-choice vector from a per-class comparison (for oracle routing)
#'
#' Collapses orch_perclass_compare to a Label -> "bert" / "keyword" lookup that
#' orch_route_oracle consumes. Lower-cased to match the tiebreak vocabulary.
#'
#' @param .compare Output of orch_perclass_compare.
#' @return Named character vector keyed by class label, values "bert" / "keyword".
orch_winner_map <- function(.compare) {
  stats::setNames(tolower(.compare$Winner), .compare$Label)
}


# Routers: each maps the assembled table to a PredLabel, scored by clf_scores ----
# Every router returns a table with DocID, TrueLabel, PredLabel (+ carried
# ClassDetailed2 / LabelRound), so 03A's clf_scores / clf_perclass score them all
# identically. "(none)" survives where a router defers to an abstaining method, and
# clf_scores counts it as a miss / reports Coverage < 1, which is the honest cost.

#' Internal: carry the scoring-relevant columns onto a router's output
#' @keywords internal
orch_carry <- function(.tab, .pred) {
  .tab |>
    dplyr::transmute(
      DocID, TrueLabel, PredLabel = .pred,
      dplyr::across(dplyr::any_of(c("ClassDetailed2", "LabelRound")))
    )
}

#' Router: BERT everywhere (the incumbent baseline)
#' @param .tab Assembled routing table.
#' @return Predictions tibble (DocID, TrueLabel, PredLabel, ...).
orch_route_bert <- function(.tab) {
  orch_carry(.tab, .tab$BertPred)
}

#' Router: keyword everywhere (contrast; abstain = miss)
#' @param .tab Assembled routing table.
#' @return Predictions tibble.
orch_route_keyword <- function(.tab) {
  orch_carry(.tab, .tab$KwPred)
}

#' Router: oracle per-class (DIAGNOSTIC CEILING -- not deployable)
#'
#' For each doc, take the prediction of whichever method WINS that doc's TRUE class
#' (per .winners). This peeks at the label, so it is not a shippable router -- it is
#' the upper bound on what any per-class routing could achieve. Read it as: if even
#' this ceiling barely clears BERT-everywhere, accuracy routing is not worth doing.
#'
#' @param .tab Assembled routing table.
#' @param .winners Named vector (orch_winner_map): class -> "bert" / "keyword".
#' @return Predictions tibble.
orch_route_oracle <- function(.tab, .winners) {
  if (FALSE) {
    .tab     <- tab_route
    .winners <- orch_winner_map(compare_)
  }
  choice_ <- .winners[.tab$TrueLabel]
  choice_[is.na(choice_)] <- "bert"                 # unseen class -> BERT
  pred_ <- dplyr::if_else(choice_ == "keyword", .tab$KwPred, .tab$BertPred)
  orch_carry(.tab, pred_)
}

#' Router: agreement + tiebreak (the deployable router)
#'
#' Where BERT and keyword agree, take the consensus label. Where they disagree (incl.
#' keyword abstain) -- the residue -- resolve by .tiebreak: "bert" keeps BERT (the
#' safe default given BERT dominates), "keyword" keeps keyword (may abstain),
#' "ollama" takes the qwen3 adjudication from .ollama_pred and falls back to BERT on
#' any doc the LLM did not answer.
#'
#' @param .tab Assembled routing table.
#' @param .tiebreak "bert", "keyword", or "ollama".
#' @param .ollama_pred Tibble (DocID, OllamaLabel) from orch_ollama_tiebreak;
#'   required when .tiebreak = "ollama".
#' @return Predictions tibble.
orch_route_agreement <- function(.tab, .tiebreak = c("bert", "keyword", "ollama"),
                                 .ollama_pred = NULL) {
  if (FALSE) {
    .tab         <- tab_route
    .tiebreak    <- "bert"
    .ollama_pred <- NULL
  }
  .tiebreak <- match.arg(.tiebreak)

  # scalar branch on the tiebreak mode (control flow, not a vectorised choice);
  # dplyr::if_else would reject the length-1 condition against vector branches.
  resid_ <- if (.tiebreak == "keyword") .tab$KwPred else .tab$BertPred
  if (.tiebreak == "ollama") {
    if (is.null(.ollama_pred)) cli::cli_abort("tiebreak = 'ollama' needs .ollama_pred (DocID, OllamaLabel).")
    look_ <- stats::setNames(.ollama_pred$OllamaLabel, .ollama_pred$DocID)
    oll_  <- unname(look_[as.character(.tab$DocID)])
    resid_ <- dplyr::coalesce(oll_, .tab$BertPred)     # LLM, else BERT fallback
  }

  pred_ <- dplyr::if_else(.tab$Agree, .tab$BertPred, resid_)
  orch_carry(.tab, pred_)
}

#' Router: selective keyword + BERT backfill (the 03C gate wired in)
#'
#' Keyword owns the documents it labels at its calibrated high-precision operating
#' point (the gated subset where its PredLabel is not "(none)"); BERT backfills
#' everything keyword abstained on. This is the deployable form of "let the cheap,
#' interpretable method take what it is sure about." The gated keyword predictions
#' come from 03C (kw_calibrate -> kw_gate) and are passed in, keeping 03D decoupled
#' from how the gate was built.
#'
#' @param .tab Assembled routing table.
#' @param .kw_gated Gated keyword predictions (DocID, PredLabel) from kw_gate.
#' @param .none Character. Abstention sentinel (default "(none)").
#' @return Predictions tibble.
orch_route_selective <- function(.tab, .kw_gated, .none = "(none)") {
  if (FALSE) {
    .tab      <- tab_route
    .kw_gated <- pred_hi
    .none     <- "(none)"
  }
  gate_ <- stats::setNames(.kw_gated$PredLabel, .kw_gated$DocID)
  kwg_  <- unname(gate_[as.character(.tab$DocID)])
  keep_ <- !is.na(kwg_) & kwg_ != .none
  pred_ <- dplyr::if_else(keep_, kwg_, .tab$BertPred)
  orch_carry(.tab, pred_)
}


# Diagnostics on the residue ----------------------------------------------

#' Agreement summary: how often, how accurate, and who is right when they differ
#'
#' The decision-grade diagnostic. Splits the sample into the agreement set and the
#' residue and reports, for each, the share of docs and the accuracy. Crucially, on
#' the residue it reports how often BERT is right vs how often keyword is right --
#' if BERT is right far more often on the docs they dispute, a tiebreak (Ollama
#' included) has little headroom, because the safe default (keep BERT) is already
#' near-optimal there.
#'
#' @param .tab Assembled routing table.
#' @param .none Character. Abstention sentinel (default "(none)").
#' @return Tibble: Set, NDocs, Share, Accuracy, BertRight, KwRight (one row per set).
orch_agreement_summary <- function(.tab, .none = "(none)") {
  if (FALSE) .tab <- tab_route
  n_ <- nrow(.tab)
  .tab |>
    dplyr::mutate(
      Set       = dplyr::if_else(.data$Agree, "Agree", "Residue"),
      BertHit   = .data$BertPred == .data$TrueLabel,
      KwHit     = .data$KwPred != .none & .data$KwPred == .data$TrueLabel,
      EitherHit = .data$BertHit | .data$KwHit          # is the truth among the two candidates?
    ) |>
    dplyr::summarise(
      NDocs     = dplyr::n(),
      Share     = dplyr::n() / n_,
      EitherAcc = mean(.data$EitherHit),               # ceiling on this set (best of the two)
      BertRight = mean(.data$BertHit),
      KwRight   = mean(.data$KwHit),
      .by = Set
    ) |>
    dplyr::arrange(dplyr::desc(.data$Set))            # Residue last
}

#' The disagreement residue (the only docs a tiebreak can change)
#'
#' Extracts the docs where BERT and keyword differ (incl. keyword abstain), carrying
#' both candidate labels and BERT's confidence. This is the set fed to the Ollama
#' tiebreak, and the set worth eyeballing to see whether the disputes are real
#' (genuinely ambiguous contracts) or keyword noise.
#'
#' @param .tab Assembled routing table.
#' @return Tibble: DocID, TrueLabel, BertPred, BertProb, KwPred, KwScore.
orch_disagreement_set <- function(.tab) {
  .tab |>
    dplyr::filter(!.data$Agree) |>
    dplyr::select(DocID, TrueLabel, BertPred, BertProb, KwPred, KwScore)
}

#' On-subset head-to-head: BERT vs keyword where the keyword gate commits
#'
#' The decisive counterfactual behind the selective router. A selective accuracy of
#' (say) 95% sounds strong only until you ask what BERT scores on the SAME documents
#' the keyword gate kept -- because keyword's confidence and BERT's confidence are
#' correlated (an easy, prototypical contract is easy for both). This partitions the
#' sample into the docs the gate OWNS (it committed a non-abstain label) and the
#' docs BERT BACKFILLS (the gate abstained), and reports each method's accuracy on
#' each partition. The headline is the Keyword-owned row: if BertAcc there exceeds
#' KwAcc, handing those docs to keyword strictly loses, which is why the selective
#' router cannot beat BERT-everywhere. On the backfill partition keyword abstained,
#' so its accuracy is undefined (NA), not zero -- it declined to guess, it did not
#' guess wrong.
#'
#' @param .tab Assembled routing table (DocID, TrueLabel, BertPred).
#' @param .kw_gated Gated keyword predictions (DocID, PredLabel) from kw_gate.
#' @param .none Character. Abstention sentinel (default "(none)").
#' @return Tibble: Owner, NDocs, Share, KwAcc, BertAcc, Gap (BertAcc - KwAcc); the
#'   Keyword-owned and BERT-backfill partitions plus an All row.
orch_subset_compare <- function(.tab, .kw_gated, .none = "(none)") {
  if (FALSE) {
    .tab      <- tab_route
    .kw_gated <- pred_hi
    .none     <- "(none)"
  }
  gate_ <- stats::setNames(.kw_gated$PredLabel, .kw_gated$DocID)
  dat_  <- .tab |>
    dplyr::mutate(
      KwGated = unname(gate_[as.character(.data$DocID)]),
      Owns    = !is.na(.data$KwGated) & .data$KwGated != .none,
      Owner   = dplyr::if_else(.data$Owns, "Keyword-owned", "BERT-backfill"),
      BertHit = .data$BertPred == .data$TrueLabel,
      KwHit   = .data$Owns & .data$KwGated == .data$TrueLabel
    )
  n_ <- nrow(dat_)

  by_ <- dat_ |>
    dplyr::summarise(
      NDocs   = dplyr::n(),
      Share   = dplyr::n() / n_,
      KwAcc   = if (any(.data$Owns)) mean(.data$KwHit) else NA_real_,
      BertAcc = mean(.data$BertHit),
      .by = Owner
    ) |>
    # keyword abstained on the backfill partition: its accuracy there is undefined
    dplyr::mutate(KwAcc = dplyr::if_else(.data$Owner == "BERT-backfill", NA_real_, .data$KwAcc))

  all_ <- dat_ |>
    dplyr::summarise(
      Owner = "All", NDocs = dplyr::n(), Share = 1,
      KwAcc = NA_real_, BertAcc = mean(.data$BertHit)
    )

  dplyr::bind_rows(by_, all_) |>
    dplyr::mutate(Gap = .data$BertAcc - .data$KwAcc) |>
    dplyr::arrange(dplyr::desc(.data$NDocs)) |>
    dplyr::relocate(Owner, NDocs, Share, KwAcc, BertAcc, Gap)
}


# Headline comparison ------------------------------------------------------

#' Score every router and tabulate the delta against BERT-everywhere
#'
#' Builds the standard router family (BERT, keyword, oracle if .winners given,
#' agreement+BERT, agreement+Ollama if .ollama_pred given, selective if .kw_gated
#' given), scores each through 03A's clf_scores, and reports each strategy's
#' Accuracy / MacroF1 / Coverage, its Deployable flag, and DeltaMacroF1 against the
#' BERT baseline. The headline 03D table: a near-zero (or negative) delta on every
#' deployable router is the clean null finding the state doc anticipates.
#'
#' @param .tab Assembled routing table.
#' @param .winners Optional named method-choice vector (enables the oracle ceiling).
#' @param .kw_gated Optional gated keyword predictions (enables the selective router).
#' @param .ollama_pred Optional Ollama adjudications (enables agreement+Ollama).
#' @param .lenient Logical. Lenient (either-label) scoring; needs ClassDetailed2.
#' @param .none Character. Abstention sentinel.
#' @return Tibble: Strategy, Deployable, Accuracy, MacroF1, Coverage, N,
#'   DeltaMacroF1 (descending by MacroF1).
orch_compare <- function(.tab, .winners = NULL, .kw_gated = NULL,
                         .ollama_pred = NULL, .lenient = FALSE, .none = "(none)") {
  if (FALSE) {
    .tab      <- tab_route
    .winners  <- orch_winner_map(compare_)
    .kw_gated <- pred_hi
  }

  routes_ <- list(
    "BERT (everywhere)"     = list(dep = TRUE,  pred = orch_route_bert(.tab)),
    "Keyword (everywhere)"  = list(dep = TRUE,  pred = orch_route_keyword(.tab)),
    "Agreement + BERT"      = list(dep = TRUE,  pred = orch_route_agreement(.tab, "bert"))
  )
  if (!is.null(.winners)) {
    routes_[["Oracle per-class (ceiling)"]] <-
      list(dep = FALSE, pred = orch_route_oracle(.tab, .winners))
  }
  if (!is.null(.kw_gated)) {
    routes_[["Selective KW + BERT backfill"]] <-
      list(dep = TRUE, pred = orch_route_selective(.tab, .kw_gated, .none = .none))
  }
  if (!is.null(.ollama_pred)) {
    routes_[["Agreement + Ollama"]] <-
      list(dep = TRUE, pred = orch_route_agreement(.tab, "ollama", .ollama_pred = .ollama_pred))
  }

  rows_ <- purrr::imap(routes_, function(r_, name_) {
    clf_scores(r_$pred, .lenient = .lenient, .none = .none) |>
      dplyr::transmute(
        Strategy   = name_,
        Deployable = r_$dep,
        Accuracy, MacroF1 = F1_macro, Coverage, N
      )
  }) |>
    purrr::list_rbind()

  base_ <- rows_$MacroF1[rows_$Strategy == "BERT (everywhere)"]
  rows_ |>
    dplyr::mutate(DeltaMacroF1 = .data$MacroF1 - base_) |>
    dplyr::arrange(dplyr::desc(.data$MacroF1))
}

#' Bar chart of macro-F1 per router with the BERT baseline marked
#'
#' Deployable routers are filled dark, the oracle ceiling is shown hollow (it is not
#' a shippable number), and a dashed line marks BERT-everywhere so the eye reads each
#' router as "clears the incumbent or not" at a glance.
#'
#' @param .compare Output of orch_compare.
#' @return A ggplot.
orch_plot_compare <- function(.compare) {
  if (FALSE) .compare <- orch_compare(tab_route, .winners = orch_winner_map(compare_))
  base_ <- .compare$MacroF1[.compare$Strategy == "BERT (everywhere)"]
  p_ <- .compare |>
    dplyr::mutate(Strategy = forcats::fct_reorder(.data$Strategy, .data$MacroF1)) |>
    ggplot2::ggplot(ggplot2::aes(x = Strategy, y = MacroF1, fill = Deployable)) +
    ggplot2::geom_col(width = 0.7, color = "grey20") +
    ggplot2::geom_hline(yintercept = base_, linetype = "dashed", color = "grey40") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", MacroF1)), hjust = -0.15, size = 3) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "grey30", `FALSE` = "white"),
                               labels = c(`TRUE` = "Deployable", `FALSE` = "Ceiling")) +
    ggplot2::scale_y_continuous(limits = c(0, 1.05), expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Macro-F1 (pooled out-of-fold)", fill = NULL)
  clf_apply_theme(p_)
}


# Optional Ollama tiebreak (qwen3:32b on the residue) ---------------------
# The ONLY part of 03D that spawns a process. Adjudicates the disagreement residue
# only: for each disputed doc it shows qwen3 the document header (signal is
# front-loaded, so the opening is enough and far cheaper than the full contract) and
# the two candidate labels, constrains the answer to a JSON object whose label is one
# of those two, runs at temperature 0, and HASH-CACHES every answer so an interrupted
# pass resumes for free (qwen3:32b is slow). Mirrors the NER _Ollama.R contract; if
# you prefer, the call/cache primitives could be hoisted into a shared _Ollama.R.

#' Build the adjudication prompt for one disputed document
#'
#' Minimal, deterministic prompt: the two candidate labels and the header excerpt,
#' asking for a single best label as JSON. "/no_think" suppresses qwen3's reasoning
#' trace so the response is clean JSON (drop it if you want the trace in the log).
#'
#' @param .label_a,.label_b The two candidate labels (BERT, keyword).
#' @param .header Character. Document header excerpt.
#' @return Character scalar prompt.
orch_ollama_prompt <- function(.label_a, .label_b, .header) {
  paste0(
    "You are classifying a corporate material contract into exactly one category.\n",
    "Two classifiers disagree. The only valid answers are:\n",
    "  A) ", .label_a, "\n",
    "  B) ", .label_b, "\n",
    "Read the contract excerpt and choose the single best-fitting category.\n",
    "Respond ONLY with JSON: {\"label\": \"<one of the two exact category strings above>\"}\n",
    "/no_think\n\n",
    "Contract excerpt:\n", .header
  )
}

#' Call local Ollama once, constrained to a two-label JSON answer
#'
#' POSTs to the Ollama /api/chat endpoint with temperature 0 and a JSON-schema
#' format that restricts label to the enum {.label_a, .label_b}, so the model cannot
#' invent a third class. Returns the chosen label, or NA on any parse/transport
#' failure (the caller then falls back to BERT). Pure transport -- caching is the
#' wrapper's job.
#'
#' @param .prompt Character. The adjudication prompt.
#' @param .label_a,.label_b The two permitted labels (the format enum).
#' @param .model Ollama model tag (default "qwen3:32b").
#' @param .url Ollama chat endpoint.
#' @param .temperature Sampling temperature (default 0).
#' @return Character scalar label, or NA_character_.
orch_ollama_call <- function(.prompt, .label_a, .label_b,
                             .model = "qwen3:32b",
                             .url = "http://localhost:11434/api/chat",
                             .temperature = 0) {
  if (FALSE) {
    .prompt  <- orch_ollama_prompt("Leases", "Other", "This Lease Agreement ...")
    .label_a <- "Leases"; .label_b <- "Other"
  }
  body_ <- list(
    model    = .model,
    messages = list(list(role = "user", content = .prompt)),
    stream   = FALSE,
    options  = list(temperature = .temperature),
    format   = list(
      type       = "object",
      properties = list(label = list(type = "string", enum = list(.label_a, .label_b))),
      required   = list("label")
    )
  )
  resp_ <- tryCatch(
    httr2::request(.url) |>
      httr2::req_body_json(body_) |>
      httr2::req_timeout(600) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    error = function(e) {
      cli::cli_alert_warning("Ollama call failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(resp_)) return(NA_character_)

  content_ <- resp_$message$content %||% ""
  # be defensive: extract the JSON object even if a stray trace leaks through
  json_ <- stringi::stri_extract_first_regex(content_, "\\{.*\\}")
  if (is.na(json_)) return(NA_character_)
  parsed_ <- tryCatch(jsonlite::fromJSON(json_), error = function(e) NULL)
  lab_ <- parsed_$label %||% NA_character_
  if (!lab_ %in% c(.label_a, .label_b)) NA_character_ else lab_
}

#' Adjudicate the disagreement residue with Ollama (hash-cached, resumable)
#'
#' For every doc in the residue, builds the header excerpt (stri_sub, code points
#' not bytes) and the two-label prompt, then looks up a per-doc cache keyed by a
#' hash of (DocID, the sorted candidate pair, model, header window, prompt version).
#' On a hit it reuses the stored label; on a miss it calls Ollama and writes the
#' answer. The per-key files mean an interrupted run resumes for free. Returns one
#' adjudicated label per residue doc (NA where the model declined / failed, which
#' the agreement router treats as a BERT fallback).
#'
#' @param .residue Output of orch_disagreement_set (DocID, BertPred, KwPred, ...).
#' @param .tab_prep Prepared sample (DocID, Text) for the header excerpt.
#' @param .cache_dir Directory for per-doc answer caches.
#' @param .model Ollama model tag.
#' @param .url Ollama chat endpoint.
#' @param .header_chars Header window in characters (default 4000 ~ first pages).
#' @param .prompt_version Bump to invalidate the cache when the prompt changes.
#' @param .overwrite Logical. Ignore the cache and re-ask every doc.
#' @return Tibble: DocID, OllamaLabel (one row per residue doc).
orch_ollama_tiebreak <- function(.residue, .tab_prep,
                                 .cache_dir = here::here("2_output", "03D-ClassifyOrchestrate", "_ollama_cache"),
                                 .model = "qwen3:32b",
                                 .url = "http://localhost:11434/api/chat",
                                 .header_chars = 4000L,
                                 .prompt_version = "v1",
                                 .overwrite = FALSE) {
  if (FALSE) {
    .residue   <- orch_disagreement_set(tab_route)
    .tab_prep  <- tab_prep
    .header_chars <- 4000L
  }
  fs::dir_create(.cache_dir)

  text_ <- stats::setNames(.tab_prep$Text, .tab_prep$DocID)
  n_ <- nrow(.residue)
  cli::cli_alert_info("Ollama tiebreak over {n_} disputed doc(s) with {(.model)}")

  cli::cli_progress_bar(
    format = paste0("{cli::pb_spin} Adjudicating {cli::pb_current}/{cli::pb_total} ",
                    "{cli::pb_bar} {cli::pb_percent} | ETA {cli::pb_eta}"),
    total = n_, clear = FALSE
  )
  out_ <- vector("list", n_)
  for (i_ in seq_len(n_)) {
    row_  <- .residue[i_, ]
    cands_ <- sort(c(row_$BertPred, row_$KwPred))     # order-free key
    key_  <- rlang::hash(list(row_$DocID, cands_, .model, .header_chars, .prompt_version))
    path_ <- fs::path(.cache_dir, paste0(key_, ".rds"))

    if (!.overwrite && fs::file_exists(path_)) {
      lab_ <- readRDS(path_)
    } else {
      header_ <- stringi::stri_sub(text_[[as.character(row_$DocID)]] %||% "", 1L, .header_chars)
      prompt_ <- orch_ollama_prompt(row_$BertPred, row_$KwPred, header_)
      lab_    <- orch_ollama_call(prompt_, row_$BertPred, row_$KwPred,
                                  .model = .model, .url = .url)
      saveRDS(lab_, path_)
    }
    out_[[i_]] <- tibble::tibble(DocID = row_$DocID, OllamaLabel = lab_)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  res_ <- purrr::list_rbind(out_)
  n_ok_ <- sum(!is.na(res_$OllamaLabel))
  cli::cli_alert_success("Ollama answered {n_ok_}/{n_} (the rest fall back to BERT)")
  res_
}

# Local null-coalescing helper (avoids a hard rlang dependency at call sites)
`%||%` <- function(.x, .y) if (is.null(.x) || length(.x) == 0L || is.na(.x[1])) .y else .x
