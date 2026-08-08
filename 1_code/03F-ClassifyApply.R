# 03F-ClassifyApply: the shippable classifier (mc_*) ----
#
# WHAT THIS STAGE IS
# One entry point, mc_classify(), that takes documents and returns labels plus a confidence flag.
# Everything it does was decided upstream and validated on folds; this file executes those decisions
# and makes none of its own.
#
# THE RULE THAT MAKES IT SAFE
# Every default is read from the manifest 03E wrote, never written into a function signature. A
# signature default is a second copy of a decision, and a second copy drifts: someone retunes the
# transformer, the manifest updates, and the function keeps applying last quarter's context length
# because nobody remembered it was written down twice. So mc_classify() ships with almost no defaults
# of its own -- it reads them -- and an argument passed explicitly is recorded in the output as a
# DEPARTURE from the validated configuration rather than silently honoured.
#
# That is also what makes the overrides safe to offer. A caller can run a different checkpoint or a
# longer window; they simply cannot do so without it showing up in the result.
#
# WHY THE GENERATIVE ARM IS OFF BY DEFAULT
# It is the most independent vote available and the only arm that saw none of the labelled sample, so
# it is the one that most improves the confidence flag. It is also hours of inference over a corpus
# of this size rather than minutes. Pinned, therefore, and disabled: a caller who wants the
# three-family flag asks for it and pays for it, and a caller who does not gets the two-family flag
# together with reliability numbers estimated on the vote they actually ran. Shipping one set of
# reliabilities for every voting set would be the quiet error here, because a two-arm pattern
# computed at deployment is indistinguishable from a two-arm pattern computed on the labelled sample.
#
# WHAT COMES BACK
# One row per document per task: the label, which arm decided it, that arm's confidence, what every
# arm said independently, the agreement tier, and the reliability estimated for that tier under that
# voting set. A user of the released dataset can therefore condition on trust without re-running
# anything.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new; if (FALSE) dev blocks; cli/fs/here;
# pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_manifest <- utils_file_path(.dir_orch, "deployment", "manifest.json")
  .docs          <- arrow::read_parquet(.lP$Input$Corpus)
}

MC_NONE <- "(none)"


# 1. The manifest ----------------------------------------------------------------------------------

#' Read and validate the deployment manifest
#'
#' Fails loudly and early rather than part way through a corpus. A manifest promising an artifact
#' that is not on disk is the failure worth catching here: discovered mid-run it wastes hours, and
#' discovered never it produces labels from whichever model happened to be lying around.
#'
#' @param .path Path to manifest.json.
#' @return List with the manifest plus a resolved arms tibble.
mc_manifest <- function(.path) {
  if (FALSE) .path <- .lP$Input$Manifest
  if (!fs::file_exists(.path)) {
    cli::cli_abort(c("No manifest at {(.path)}.", "i" = "Run 03E to write one."))
  }
  man_ <- jsonlite::read_json(.path, simplifyVector = FALSE)

  arms_ <- purrr::map(man_$arms, function(.a) {
    tibble::tibble(
      Arm       = .a$arm,
      Kind      = .a$kind,
      Artifact  = as.character(fs::path_abs(.a$artifact, start = here::here())),
      Available = isTRUE(.a$available),
      Enabled   = isTRUE(.a$enabled),
      Params    = list(.a$params)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(OnDisk = fs::file_exists(.data$Artifact) | fs::dir_exists(.data$Artifact))

  gone_ <- arms_ |> dplyr::filter(.data$Available, !.data$OnDisk)
  if (nrow(gone_) > 0L) {
    cli::cli_abort(c(
      "The manifest promises artifacts that are no longer on disk: {gone_$Arm}",
      "i" = "Re-run the stage that wrote them, or re-run 03E to record their current location."
    ))
  }
  # The reliability table sits beside the manifest, so the path it was read from travels with it.
  # Recovering it later from an attribute that may not be there is how a lookup silently reads the
  # wrong file, or none.
  list(Manifest = man_, Arms = arms_, Dir = as.character(fs::path_dir(.path)))
}

#' Print what the manifest commits this run to
#'
#' Read before a corpus pass rather than after. Everything below is a decision someone made once and
#' validated, and the point of showing it is that a run applying the wrong quarter's configuration
#' should be visible in the first screen of output rather than in the released labels.
#'
#' @param .man Output of mc_manifest().
#' @return Invisibly .man.
mc_report_manifest <- function(.man) {
  if (FALSE) .man <- man
  m_ <- .man$Manifest
  cli::cli_h2("Deployment manifest")
  cli::cli_alert_info(
    "Version {m_$manifest_version}, written {m_$written_at} from {m_$sample$n_labelled} labelled \\
     documents over {m_$sample$n_folds} folds."
  )

  purrr::map(m_$tasks, function(.t) {
    tibble::tibble(
      Task      = .t$label_col,
      Policy    = .t$policy$label,
      MacroF1   = .t$validated$macro_f1,
      Accuracy  = .t$validated$accuracy
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(Accuracy = clf_pct(.data$Accuracy), MacroF1 = sprintf("%.3f", .data$MacroF1)) |>
    clf_say_table(.title = "Tasks, and what each scored when validated")

  .man$Arms |>
    dplyr::mutate(Artifact = fs::path_rel(.data$Artifact, here::here())) |>
    dplyr::select(Arm, Kind, Enabled, OnDisk, Artifact) |>
    clf_say_table(.title = "Arms")

  cli::cli_text("")
  cli::cli_alert_info(
    "Default voting set for the confidence flag: {m_$confidence$default_set}. An arm present but \\
     not enabled is available to a caller who asks for it."
  )
  invisible(.man)
}


# 2. The arms, applied -----------------------------------------------------------------------------
# One function per kind, each returning the same three columns, so mc_classify() assembles them
# without knowing which produced what. That is the same contract the run folders enforce upstream,
# carried into inference.

#' Apply the deployed transformer
#'
#' Shells out to the same inference script the sweeps used, in classify mode, against the all-data
#' refit the manifest points at. Batched, because a corpus of this size does not fit in memory and
#' because a failure a million documents in should cost one batch rather than the run.
#'
#' @param .docs Documents (DocID, Text).
#' @param .artifact Path to the saved model directory.
#' @param .params Manifest parameters for this arm (max_len and so on).
#' @param .script Inference script.
#' @param .python Python binary.
#' @param .batch_size Documents per forward pass.
#' @param .out_dir Scratch directory for the parquet seam.
#' @return Tibble: DocID, Pred, Score.
mc_apply_bert <- function(.docs, .artifact, .params, .script, .python, .batch_size = 32L,
                          .out_dir = fs::path(tempdir(), "mc-bert")) {
  if (FALSE) {
    .docs     <- dplyr::slice_head(tab_docs, n = 100L)
    .artifact <- man$Arms$Artifact[man$Arms$Kind == "transformer"]
    .params   <- man$Arms$Params[[1]]
  }
  fs::dir_create(.out_dir)
  in_  <- fs::path(.out_dir, "input.parquet")
  out_ <- fs::path(.out_dir, "pred.parquet")
  arrow::write_parquet(.docs |> dplyr::select(DocID, Text), in_)

  args_ <- c(
    .script,
    "--data", in_,
    "--model-dir", .artifact,
    "--max-len", as.character(.params$max_len %||% 256L),
    "--batch-size", as.character(.batch_size),
    "--out", out_,
    "--classify"
  )
  status_ <- system2(.python, args = args_, stdout = "", stderr = "")
  if (!identical(status_, 0L)) cli::cli_abort("Transformer inference failed (exit {status_})")

  arrow::read_parquet(out_) |>
    dplyr::transmute(DocID, Pred = .data$PredLabel, Score = as.numeric(.data$Top1Prob))
}

#' Apply the published keyword table
#'
#' Pure string matching against the shipped lexicon, so it runs in R and needs no engine. Each term
#' carries the training precision that earned its place; a document takes the class whose best
#' matching term has the highest Power, and abstains where nothing clears the operating threshold.
#'
#' @param .docs Documents (DocID, Text, DocDesc).
#' @param .artifact Path to the lexicon parquet.
#' @param .params Manifest parameters (source, tau).
#' @param .none Abstention sentinel.
#' @return Tibble: DocID, Pred, Score.
mc_apply_keyword <- function(.docs, .artifact, .params, .none = MC_NONE) {
  if (FALSE) {
    .docs     <- dplyr::slice_head(tab_docs, n = 100L)
    .artifact <- man$Arms$Artifact[man$Arms$Kind == "keyword"]
    .params   <- list(source = "text", tau = 0.70)
  }
  lex_ <- arrow::read_parquet(.artifact)
  src_ <- .params$source %||% "text"
  txt_ <- if (src_ == "docdesc") .docs$DocDesc else .docs$Text

  hay_ <- stringi::stri_trans_tolower(dplyr::coalesce(txt_, ""))
  hits_ <- purrr::map(seq_len(nrow(lex_)), function(.i) {
    found_ <- stringi::stri_detect_fixed(hay_, paste0(" ", lex_$Term[[.i]], " "))
    if (!any(found_)) return(NULL)
    tibble::tibble(DocID = .docs$DocID[found_], Class = lex_$Class[[.i]],
                   Power = as.numeric(lex_$Power[[.i]]))
  }) |>
    purrr::list_rbind()

  base_ <- tibble::tibble(DocID = .docs$DocID, Pred = .none, Score = 0)
  if (nrow(hits_) == 0L) return(base_)

  best_ <- hits_ |>
    dplyr::slice_max(.data$Power, n = 1L, by = DocID, with_ties = FALSE) |>
    dplyr::transmute(DocID, Pred = .data$Class, Score = .data$Power)

  base_ |>
    dplyr::rows_update(best_, by = "DocID", unmatched = "ignore")
}

#' Apply the generative arm
#'
#' Off unless asked for, and the reason is cost rather than quality: this is the most independent
#' vote available and hours of inference over a corpus of this size. The prompt is rebuilt from the
#' recipe the manifest pinned, and worked examples are drawn from the whole labelled sample -- which
#' is legitimate at deployment for the same reason the transformer's all-data refit is, since no
#' corpus document is in that sample. The folds certified the recipe; deployment fits it on
#' everything.
#'
#' @param .docs Documents (DocID, Text).
#' @param .params Manifest parameters (model, shots and so on).
#' @param .tab_prep Labelled sample, supplying the deployment example draw.
#' @param .label_col Task.
#' @param .none Abstention sentinel.
#' @param ... Passed to llm_classify() from 03D.
#' @return Tibble: DocID, Pred, Score.
mc_apply_llm <- function(.docs, .params, .tab_prep, .label_col, .none = MC_NONE, ...) {
  if (FALSE) {
    .docs      <- dplyr::slice_head(tab_docs, n = 20L)
    .params    <- list(model = "qwen3:8b", shots = 4L, n_chars = 6000L)
    .tab_prep  <- tab_prep
    .label_col <- "ClassDetailed"
  }
  labels_ <- llm_labels(.tab_prep = .tab_prep, .label_col = .label_col)
  block_  <- llm_render_labels(.labels = labels_, .definitions = NULL, .guidance = "labels")

  ex_ <- if ((.params$shots %||% 0L) > 0L) {
    llm_render_examples(
      .tab = llm_examples(
        .tab_prep  = .tab_prep,
        .label_col = .label_col,
        # Every fold, because at deployment there is no held-out fold to protect: the documents being
        # classified are not in this sample at all.
        .folds     = sort(unique(.tab_prep$Fold)),
        .per_class = .params$shots
      ),
      .label_col = .label_col
    )
  } else {
    NULL
  }

  llm_classify(
    .tab           = .docs |> dplyr::mutate(Fold = 0L, !!.label_col := NA_character_),
    .label_col     = .label_col,
    .labels        = labels_,
    .labels_block  = block_,
    .task_line     = .params$task_line %||% "Classify this contract into exactly one category.",
    .examples      = ex_,
    .model         = .params$model %||% "qwen3:8b",
    .allow_abstain = isTRUE(.params$allow_abstain),
    .n_chars       = .params$n_chars %||% 6000L,
    .think         = isTRUE(.params$think),
    ...
  ) |>
    dplyr::transmute(DocID, Pred = .data$PredLabel, Score = .data$Score)
}


# 3. Routing and the flag --------------------------------------------------------------------------

#' Apply a manifest routing policy to per-arm predictions
#'
#' The deployment counterpart of the orchestrator's policy application, reading its instructions from
#' the manifest instead of a searched grid. Walks the cascade in order, honours a commitment that
#' clears the floor, and falls through to the terminal.
#'
#' @param .arm_pred Long tibble: DocID, Arm, Pred, Score.
#' @param .policy Manifest policy block for one task.
#' @param .none Abstention sentinel.
#' @return Tibble: DocID, Label, DecidedBy, Prob.
mc_route <- function(.arm_pred, .policy, .none = MC_NONE) {
  if (FALSE) {
    .arm_pred <- arm_pred
    .policy   <- man$Manifest$tasks$ClassDetailed$policy
  }
  order_ <- unlist(.policy$order) %||% character(0)

  out_ <- .arm_pred |>
    dplyr::filter(.data$Arm == .policy$terminal) |>
    dplyr::transmute(DocID, Label = .data$Pred, DecidedBy = .policy$terminal,
                     Prob = .data$Score)
  if (nrow(out_) == 0L) cli::cli_abort("Terminal arm {(.policy$terminal)} produced no predictions.")
  if (length(order_) == 0L) return(out_)

  for (arm_ in rev(order_)) {
    cand_ <- .arm_pred |>
      dplyr::filter(.data$Arm == arm_, .data$Pred != .none, .data$Score >= .policy$floor) |>
      dplyr::select(DocID, ArmPred = .data$Pred, ArmScore = .data$Score)
    out_ <- out_ |>
      dplyr::left_join(cand_, by = dplyr::join_by(DocID)) |>
      dplyr::mutate(
        DecidedBy = dplyr::if_else(is.na(.data$ArmPred), .data$DecidedBy, arm_),
        Prob      = dplyr::coalesce(.data$ArmScore, .data$Prob),
        Label     = dplyr::coalesce(.data$ArmPred, .data$Label),
        ArmPred   = NULL, ArmScore = NULL
      )
  }
  out_
}

#' Attach the agreement tier and its estimated reliability
#'
#' The tier is recomputed here from the voting arms, exactly as 03E computed it and using no labels,
#' which is what makes it computable on a corpus at all. Reliability is looked up rather than
#' estimated: a corpus has no truth to estimate it from, and a stage that appeared to derive one
#' would be deriving something else.
#'
#' The lookup is keyed on the voting set as well as the tier. A run that leaves the generative arm
#' off produces two-arm patterns, and those must be read against the two-arm reliabilities -- a
#' two-arm pattern is indistinguishable from a three-arm one once computed, so nothing downstream
#' could catch the mismatch.
#'
#' @param .routed Output of mc_route().
#' @param .arm_pred Long per-arm predictions.
#' @param .arms Arm names that voted.
#' @param .reliability Tier reliability table from the manifest directory.
#' @param .set_name Voting set applied.
#' @param .none Abstention sentinel.
#' @return .routed plus nCommit, nConcur, Tier, Reliability.
mc_flag <- function(.routed, .arm_pred, .arms, .reliability, .set_name, .none = MC_NONE) {
  if (FALSE) {
    .routed      <- routed
    .arm_pred    <- arm_pred
    .arms        <- c("legal-bert", "kw-text")
    .reliability <- rel
    .set_name    <- "core"
  }
  pat_ <- .arm_pred |>
    dplyr::filter(.data$Arm %in% .arms, .data$Pred != .none) |>
    dplyr::inner_join(.routed |> dplyr::select(DocID, Label), by = dplyr::join_by(DocID)) |>
    dplyr::summarise(
      nCommit = dplyr::n(),
      nConcur = sum(.data$Pred == .data$Label),
      .by = DocID
    ) |>
    dplyr::mutate(
      nDissent = .data$nCommit - .data$nConcur,
      Tier = dplyr::case_when(
        .data$nCommit <= 1L            ~ "sole",
        .data$nDissent == 0L           ~ "unanimous",
        .data$nConcur > .data$nDissent ~ "majority",
        TRUE                           ~ "split"
      )
    )

  rel_ <- .reliability |>
    dplyr::filter(.data$SetName == .set_name) |>
    dplyr::transmute(Tier = as.character(.data$Tier), Reliability = .data$Accuracy)

  .routed |>
    dplyr::left_join(pat_ |> dplyr::select(DocID, nCommit, nConcur, Tier),
                     by = dplyr::join_by(DocID)) |>
    tidyr::replace_na(list(nCommit = 0L, nConcur = 0L, Tier = "sole")) |>
    dplyr::left_join(rel_, by = dplyr::join_by(Tier))
}


# 4. The entry point -------------------------------------------------------------------------------

#' Classify documents using the deployed configuration
#'
#' The one function to call. Reads the manifest, runs the enabled arms, routes, attaches the
#' confidence flag, and returns one row per document per task.
#'
#' Every argument that is NULL takes its value from the manifest. Passing one explicitly is allowed
#' and recorded: the returned table carries a Departures attribute naming what was overridden, so a
#' result produced under a non-validated configuration says so rather than looking like any other.
#'
#' @param .docs Documents: DocID, Text, and DocDesc where the keyword arm uses it.
#' @param .manifest Output of mc_manifest().
#' @param .tasks Tasks to label. NULL takes every task the manifest carries.
#' @param .arms Arm names to run. NULL takes the manifest's enabled set.
#' @param .voting_set Voting set for the confidence flag. NULL takes the manifest default.
#' @param .model_dir Override the transformer artifact. NULL uses the manifest's.
#' @param .max_len Override the context length. NULL uses the manifest's.
#' @param .tab_prep Labelled sample, required only when the generative arm runs.
#' @param .python,.script Inference binary and script for the transformer arm.
#' @param .batch_size Documents per forward pass.
#' @param .none Abstention sentinel.
#' @param ... Passed to the generative arm.
#' @return Tibble: DocID, Task, Label, DecidedBy, Prob, per-arm columns, nCommit, nConcur, Tier,
#'   Reliability. Carries a Departures attribute.
mc_classify <- function(.docs, .manifest, .tasks = NULL, .arms = NULL, .voting_set = NULL,
                        .model_dir = NULL, .max_len = NULL, .tab_prep = NULL,
                        .python = NULL, .script = NULL, .batch_size = 32L, .none = MC_NONE, ...) {
  if (FALSE) {
    .docs     <- dplyr::slice_head(tab_docs, n = 100L)
    .manifest <- man
    .tasks    <- "ClassDetailed"
  }
  m_ <- .manifest$Manifest

  # A NULL argument means "use what was validated"; anything else is a departure and is recorded.
  dep_ <- c(
    if (!is.null(.arms))       "arms",
    if (!is.null(.voting_set)) "voting_set",
    if (!is.null(.model_dir))  "model_dir",
    if (!is.null(.max_len))    "max_len"
  )
  if (length(dep_) > 0L) {
    cli::cli_alert_warning(
      "Departing from the validated configuration on: {dep_}. The result carries this in its \\
       Departures attribute; it is not comparable with labels produced under the manifest."
    )
  }

  tasks_ <- .tasks %||% purrr::map_chr(m_$tasks, "label_col")
  set_   <- .voting_set %||% m_$confidence$default_set
  vote_  <- unlist(m_$confidence$sets[[set_]])

  arms_run_ <- .manifest$Arms |>
    dplyr::filter(if (is.null(.arms)) .data$Enabled else .data$Arm %in% .arms) |>
    dplyr::filter(.data$OnDisk)
  if (nrow(arms_run_) == 0L) cli::cli_abort("No arms to run.")

  missing_vote_ <- setdiff(vote_, arms_run_$Arm)
  if (length(missing_vote_) > 0L) {
    cli::cli_abort(c(
      "Voting set {(set_)} needs {missing_vote_}, which this run is not producing.",
      "i" = "Enable {?it/them}, or choose a voting set whose arms are all running."
    ))
  }

  path_rel_ <- fs::path(.manifest$Dir, m_$confidence$reliability)
  if (!fs::file_exists(path_rel_)) cli::cli_abort("No reliability table at {path_rel_}")
  rel_ <- arrow::read_parquet(path_rel_)

  out_ <- purrr::map(tasks_, function(.task) {
    cli::cli_h2("Labelling {.task}: {nrow(.docs)} document{?s}, {nrow(arms_run_)} arm{?s}")

    arm_pred_ <- purrr::map(seq_len(nrow(arms_run_)), function(.i) {
      a_ <- arms_run_[.i, ]
      p_ <- a_$Params[[1]] %||% list()
      res_ <- switch(a_$Kind,
        transformer = mc_apply_bert(
          .docs = .docs, .artifact = .model_dir %||% a_$Artifact,
          .params = utils::modifyList(p_, list(max_len = .max_len %||% p_$max_len)),
          .script = .script, .python = .python, .batch_size = .batch_size
        ),
        keyword = mc_apply_keyword(.docs = .docs, .artifact = a_$Artifact, .params = p_,
                                   .none = .none),
        llm = mc_apply_llm(.docs = .docs, .params = p_, .tab_prep = .tab_prep,
                           .label_col = .task, .none = .none, ...),
        cli::cli_abort("Unknown arm kind {(a_$Kind)}")
      )
      res_ |> dplyr::mutate(Arm = a_$Arm)
    }) |>
      purrr::list_rbind()

    routed_ <- mc_route(.arm_pred = arm_pred_, .policy = m_$tasks[[.task]]$policy, .none = .none)

    mc_flag(
      .routed = routed_, .arm_pred = arm_pred_, .arms = vote_,
      .reliability = rel_, .set_name = set_, .none = .none
    ) |>
      dplyr::left_join(
        arm_pred_ |>
          dplyr::select(DocID, Arm, Pred) |>
          tidyr::pivot_wider(names_from = Arm, values_from = Pred, names_prefix = "Arm_"),
        by = dplyr::join_by(DocID)
      ) |>
      dplyr::mutate(Task = .task, VotingSet = set_, .after = DocID)
  }) |>
    purrr::list_rbind()

  attr(out_, "Departures") <- dep_
  attr(out_, "Manifest")   <- m_$written_at
  out_
}

#' Report what a classification run produced
#' @param .tab Output of mc_classify().
#' @return Invisibly .tab.
mc_report_run <- function(.tab) {
  if (FALSE) .tab <- labelled
  cli::cli_h2("Labelling summary")
  .tab |>
    dplyr::summarise(
      nDocs   = dplyr::n_distinct(.data$DocID),
      Routed  = sum(.data$DecidedBy != dplyr::first(.data$DecidedBy)),
      .by = c(Task, VotingSet)
    ) |>
    clf_say_table()

  .tab |>
    dplyr::summarise(nDocs = dplyr::n(), Reliability = dplyr::first(.data$Reliability),
                     .by = c(Task, Tier)) |>
    dplyr::mutate(Reliability = clf_pct(.data$Reliability)) |>
    dplyr::arrange(.data$Task, .data$Tier) |>
    clf_say_table(.title = "Confidence tiers")

  dep_ <- attr(.tab, "Departures")
  cli::cli_text("")
  if (length(dep_) > 0L) {
    cli::cli_alert_warning("Produced under departures from the manifest: {dep_}.")
  } else {
    cli::cli_alert_success("Produced entirely under the validated configuration.")
  }
  invisible(.tab)
}

# Local null-coalescing helper (base R gained %||% in 4.4; this keeps the file self-contained).
`%||%` <- function(.x, .y) if (is.null(.x)) .y else .x
