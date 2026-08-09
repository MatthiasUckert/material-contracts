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

#' Read a JSON field that should be a list of records, whatever auto_unbox did to it
#'
#' jsonlite writes a one-element array as a bare object under auto_unbox, so a task deploying a
#' single arm has an `arms` field holding the arm itself rather than a list containing it. Iterating
#' that yields the arm's FIELD NAMES instead of arm records, which produces no rows rather than an
#' error -- the failure then surfaces several steps later as a missing column, naming nothing useful.
#'
#' Detected by looking for a signature field: a record has it, a list of records does not.
#'
#' @param .x The parsed field.
#' @param .signature A field name every record carries.
#' @return A list of records, possibly empty.
mc_as_records <- function(.x, .signature) {
  if (FALSE) {
    .x         <- man_$tasks[[1]]$arms
    .signature <- "arm"
  }
  if (is.null(.x)) return(list())
  if (!is.list(.x)) return(list())
  if (.signature %in% names(.x)) list(.x) else .x
}

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

  # Arms are nested inside each task, because each task crowned its own configuration: the detailed
  # taxonomy deploys one checkpoint and the broad taxonomy another. A flat arm list would name one
  # task's model for all of them, and the failure surfaces as a terminal with no predictions rather
  # than as anything recognisable.
  arms_ <- purrr::map(man_$tasks, function(.t) {
    purrr::map(mc_as_records(.x = .t$arms, .signature = "arm"), function(.a) {
      tibble::tibble(
        LabelCol  = .t$label_col %||% NA_character_,
        Arm       = .a$arm %||% NA_character_,
        Kind      = .a$kind %||% NA_character_,
        Artifact  = as.character(fs::path_abs(.a$artifact %||% ".", start = here::here())),
        Available = isTRUE(.a$available),
        Enabled   = isTRUE(.a$enabled),
        Params    = list(.a$params)
      )
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()

  # An empty result here is the one failure that would otherwise travel: every downstream step would
  # report a missing column rather than a missing manifest section. Say what was actually found.
  if (nrow(arms_) == 0L) {
    cli::cli_abort(c(
      "The manifest lists no arms.",
      "i" = "Top-level keys: {names(man_)}",
      "i" = "Keys under the first task: {names(man_$tasks[[1]])}",
      "i" = "03E writes arms inside each task; re-run it if this manifest predates that."
    ))
  }

  arms_ <- arms_ |>
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
    dplyr::select(LabelCol, Arm, Kind, Enabled, OnDisk, Artifact) |>
    clf_say_table(.title = "Arms, per task")

  cli::cli_text("")
  cli::cli_alert_info(
    "Confidence flag estimated on {m_$confidence$label_col}, default voting set \\
     {m_$confidence$default_set}. Other tasks ship a label without a tier: a concurrence flag needs \\
     more than one deployable arm, and only that task has one."
  )
  invisible(.man)
}


# 2. The arms, applied -----------------------------------------------------------------------------
# One function per kind, each returning the same three columns, so mc_classify() assembles them
# without knowing which produced what. That is the same contract the run folders enforce upstream,
# carried into inference.

#' Apply the deployed transformer
#'
#' Shells out to classify_apply.py, the inference companion to the trainer, against the all-data
#' refit the manifest points at. A separate script rather than a mode on classify_train.py: training
#' assumes a label column at every step and inference assumes there is none, so a combined script
#' would be mostly guards. What the two share is the encoding -- same tokenizer, same max_len, same
#' fixed-width padding -- because a model is only as reproducible as the tokenisation it was fitted
#' under.
#'
#' The label mapping is not passed across. The trainer writes id2label into the checkpoint config, so
#' it travels with the weights; supplying it here would be a second source of truth, and the two
#' would eventually disagree about which integer means which category while still emitting valid
#' category names.
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
    "--text-col", "Text",
    "--id-col", "DocID",
    "--max-len", as.character(.params$max_len %||% 256L),
    "--batch-size", as.character(.batch_size)
  )
  args_ <- c(args_, "--out", out_)

  # Exit 127 is the shell failing to find the interpreter, which is a configuration problem rather
  # than an inference one and deserves a different message: a corpus pass that dies four hours in
  # because a path was wrong should say so in the first line, not report a failed forward pass.
  if (!fs::file_exists(.python)) {
    cli::cli_abort(c(
      "No Python interpreter at {(.python)}.",
      "i" = "03B trains through {.path contracts-engine/.venv/bin/python}; point .lP$Engine$Python there."
    ))
  }
  if (!fs::file_exists(.script)) {
    cli::cli_abort(c(
      "No inference script at {(.script)}.",
      "i" = "Expected {.path contracts-engine/classify_apply.py}, the companion to classify_train.py."
    ))
  }

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

  tasks_    <- .tasks %||% purrr::map_chr(m_$tasks, "label_col")
  set_      <- .voting_set %||% m_$confidence$default_set
  vote_     <- unlist(m_$confidence$sets[[set_]])
  flag_task_ <- m_$confidence$label_col %||% tasks_[[1]]

  path_rel_ <- fs::path(.manifest$Dir, m_$confidence$reliability)
  if (!fs::file_exists(path_rel_)) cli::cli_abort("No reliability table at {path_rel_}")
  rel_ <- arrow::read_parquet(path_rel_)

  out_ <- purrr::map(tasks_, function(.task) {
    # Arms are resolved per task, because each task deploys its own checkpoint. Resolving once
    # outside this loop is what put the detailed task's model in front of the broad task's policy.
    arms_run_ <- .manifest$Arms |>
      dplyr::filter(.data$LabelCol == .task, .data$OnDisk) |>
      dplyr::filter(if (is.null(.arms)) .data$Enabled else .data$Arm %in% .arms)
    if (nrow(arms_run_) == 0L) cli::cli_abort("No arms to run for {(.task)}.")

    term_ <- m_$tasks[[.task]]$policy$terminal
    if (!term_ %in% arms_run_$Arm) {
      cli::cli_abort(c(
        "Task {(.task)} deploys terminal {(term_)}, which is not among its runnable arms.",
        "i" = "Runnable: {arms_run_$Arm}",
        "i" = "Re-run 03E so the manifest pins this task's own artifacts."
      ))
    }

    cli::cli_h2("Labelling {(.task)}: {nrow(.docs)} document{?s} through {nrow(arms_run_)} arm{?s}")

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

    # A tier is only meaningful where the reliability behind it was estimated. Attaching one to a
    # task whose flag was never estimated would produce a column that looks exactly like a real one.
    flagged_ <- if (identical(.task, flag_task_) && all(vote_ %in% arm_pred_$Arm)) {
      mc_flag(
        .routed = routed_, .arm_pred = arm_pred_, .arms = vote_,
        .reliability = rel_, .set_name = set_, .none = .none
      )
    } else {
      if (identical(.task, flag_task_)) {
        cli::cli_alert_warning(
          "Voting set {(set_)} needs {length(setdiff(vote_, arm_pred_$Arm))} arm{?s} this run is \\
           not producing; {(.task)} ships without a tier."
        )
      }
      routed_ |>
        dplyr::mutate(nCommit = NA_integer_, nConcur = NA_integer_,
                      Tier = NA_character_, Reliability = NA_real_)
    }

    flagged_ |>
      # Against the policy's terminal, which is known, rather than against whatever arm decided the
      # first row. The two agree only while nothing routes, which is exactly when the column is
      # least interesting and least likely to be checked.
      dplyr::mutate(Routed = .data$DecidedBy != term_) |>
      dplyr::left_join(
        arm_pred_ |>
          dplyr::select(DocID, Arm, Pred) |>
          tidyr::pivot_wider(names_from = Arm, values_from = Pred, names_prefix = "Arm_"),
        by = dplyr::join_by(DocID)
      ) |>
      dplyr::mutate(
        Task = .task,
        VotingSet = dplyr::if_else(identical(.task, flag_task_), set_, NA_character_),
        .after = DocID
      )
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
      nDocs  = dplyr::n_distinct(.data$DocID),
      Routed = if ("Routed" %in% names(.tab)) sum(.data$Routed) else NA_integer_,
      .by = c(Task, VotingSet)
    ) |>
    clf_say_table()

  # A task with no flag has no tier, and printing NA% for it invites reading a missing measurement
  # as a bad one. Those rows are dropped and named instead.
  flagged_ <- .tab |> dplyr::filter(!is.na(.data$Tier))
  if (nrow(flagged_) > 0L) {
    flagged_ |>
      dplyr::summarise(nDocs = dplyr::n(), Reliability = dplyr::first(.data$Reliability),
                       .by = c(Task, Tier)) |>
      dplyr::mutate(Reliability = clf_pct(.data$Reliability)) |>
      dplyr::arrange(.data$Task, .data$Tier) |>
      clf_say_table(.title = "Confidence tiers")
  }
  no_flag_ <- setdiff(unique(.tab$Task), unique(flagged_$Task))
  if (length(no_flag_) > 0L) {
    cli::cli_alert_info(
      "{length(no_flag_)} task{?s} ship without a tier ({no_flag_}): only one deployable arm, so \\
       there is nobody to concur with."
    )
  }

  dep_ <- attr(.tab, "Departures")
  cli::cli_text("")
  if (length(dep_) > 0L) {
    cli::cli_alert_warning("Produced under departures from the manifest: {dep_}.")
  } else {
    cli::cli_alert_success("Produced entirely under the validated configuration.")
  }
  invisible(.tab)
}


# 5. The corpus ------------------------------------------------------------------------------------
# The labelled sample fits in memory; the corpus does not. 1.1 million contracts of full text is
# hundreds of gigabytes, so nothing here loads text for more than one chunk at a time. What IS held
# throughout is the index -- one row per document with its path and filer title -- which is three
# small columns and comfortably resident.
#
# That is the difference between this stage and every stage before it. Upstream, a document's text
# was a column. Here it is a file read on demand and discarded, and any code that treats it as a
# column will exhaust memory somewhere past the first hundred thousand documents.

#' Build the corpus index: one row per document, with its path and filer title
#'
#' Walks the parsed-contract tree once and caches the result, because enumerating a million files
#' takes minutes and the answer changes only when new contracts are parsed. Text is deliberately NOT
#' read here -- the index carries where each document lives, and the chunk loop reads it.
#'
#' A limit draws a seeded random sample rather than the first n rows. The tree is organised by
#' quarter, so the first n documents are the oldest n documents, and a rehearsal on the oldest
#' quarter of EDGAR would say very little about a classifier that has to work across twenty years of
#' drafting conventions.
#'
#' @param .dir_corpus Root of the parsed-contract tree.
#' @param .path_meta Metadata parquet supplying DocDesc.
#' @param .path_cache Where the file index is cached.
#' @param .rerun Logical. Rebuild the index rather than reusing the cache.
#' @param .limit Documents to keep, or NULL for the whole corpus.
#' @param .seed Fixed, so a limited run draws the same documents every time.
#' @param .tab_prep Optional labelled sample; where supplied, adds an InSample flag.
#' @return Tibble: DocID, Path, DocDesc, InSample.
mc_corpus_index <- function(.dir_corpus, .path_meta, .path_cache, .rerun = FALSE, .limit = NULL,
                            .seed = 42L, .tab_prep = NULL) {
  if (FALSE) {
    .dir_corpus <- .lP$Input$DirCorpus
    .path_meta  <- .lP$Input$MetaData
    .path_cache <- .lP$Cache$CorpusFiles
    .limit      <- 2000L
  }
  if (!fs::dir_exists(.dir_corpus)) cli::cli_abort("No corpus tree at {(.dir_corpus)}")

  idx_ <- utils_list_project_files(
    .dir_data = .dir_corpus,  # root of the parsed-contract tree
    .path_out = .path_cache,  # where the index is cached
    .rerun    = .rerun        # FALSE reuses an existing index
  ) |>
    dplyr::select(DocID, Path) |>
    dplyr::mutate(Path = unname(.data$Path))
  cli::cli_alert_info("Corpus index: {nrow(idx_)} document{?s}")

  # DocDesc is the filer's own title. The keyword arm can key on it, and it costs one small join.
  if (fs::file_exists(.path_meta)) {
    idx_ <- idx_ |>
      dplyr::left_join(
        arrow::open_dataset(sources = .path_meta) |>
          dplyr::select(DocID, DocDesc) |>
          dplyr::collect(),
        by = dplyr::join_by(DocID)
      )
  } else {
    cli::cli_alert_warning("No metadata at {(.path_meta)}; DocDesc will be missing.")
    idx_ <- idx_ |> dplyr::mutate(DocDesc = NA_character_)
  }

  # Documents that trained the model are still part of the corpus and still get labels, but a user
  # measuring anything on them would be measuring training accuracy. Flagged, not dropped.
  idx_ <- idx_ |>
    dplyr::mutate(
      InSample = if (is.null(.tab_prep)) FALSE else .data$DocID %in% .tab_prep$DocID
    )

  if (!is.null(.limit) && .limit < nrow(idx_)) {
    idx_ <- withr::with_seed(.seed, dplyr::slice_sample(idx_, n = .limit))
    cli::cli_alert_warning(
      "Limited to {nrow(idx_)} document{?s}, drawn at random across the whole tree. Labels from a \\
       limited run are written to a separate directory so they cannot be mistaken for a corpus pass."
    )
  }
  idx_
}

#' Read the text for one chunk of the index
#'
#' The only place text enters memory, and it leaves again when the chunk is written. Documents whose
#' parquet is missing or empty are dropped and counted rather than aborting a pass measured in hours.
#'
#' @param .chunk Rows of the corpus index.
#' @return The chunk with a Text column, minus unreadable documents.
mc_read_chunk <- function(.chunk) {
  if (FALSE) .chunk <- dplyr::slice_head(tab_index, n = 100L)
  out_ <- .chunk |>
    dplyr::mutate(Text = purrr::map_chr(.data$Path, clf_read_text))
  n_bad_ <- sum(is.na(out_$Text) | !nzchar(dplyr::coalesce(out_$Text, "")))
  if (n_bad_ > 0L) {
    cli::cli_alert_warning("{n_bad_} document{?s} unreadable or empty; dropped from this chunk.")
  }
  out_ |> dplyr::filter(!is.na(.data$Text), nzchar(.data$Text))
}

#' Where labels belong, given whether the run was limited
#'
#' A limited run and a corpus pass are not the same object, and chunk files named alike would let the
#' second resume from the first: chunk one of a two-thousand-document rehearsal holds different
#' documents from chunk one of the full pass, and the skip-if-present rule cannot tell them apart.
#' Separating them physically is the only guarantee that survives a forgotten argument.
#'
#' @param .dir Base labels directory.
#' @param .limit Documents the run was capped at, or NULL.
#' @return Character path.
mc_labels_dir <- function(.dir, .limit = NULL) {
  if (FALSE) {
    .dir   <- .lP$Output$Labels
    .limit <- 2000L
  }
  if (is.null(.limit)) .dir else paste0(.dir, "_preview")
}

# 6. Throughput ------------------------------------------------------------------------------------
# A corpus pass is a commitment measured in hours, and the only honest way to size it is to measure
# the machine rather than reason about it. Each chunk records how long it took beside its labels, so
# a projection is arithmetic over work already done rather than a guess, and a run resumed after an
# interruption keeps the timings it already paid for.

#' Record how long one chunk took, beside its labels
#'
#' A sidecar rather than a column: the duration belongs to the chunk, not to each of its rows, and
#' twenty-five thousand copies of the same number is not a record of anything.
#'
#' @param .dir Labels directory.
#' @param .chunk Chunk index.
#' @param .n_docs Documents labelled in this chunk.
#' @param .seconds Wall clock for the chunk.
#' @param .n_tasks Tasks labelled, since a chunk costs one model pass per task.
#' @return Invisibly the path written.
mc_write_timing <- function(.dir, .chunk, .n_docs, .seconds, .n_tasks) {
  if (FALSE) {
    .dir     <- .dir_labels
    .chunk   <- 1L
    .n_docs  <- 2500L
    .seconds <- 180
    .n_tasks <- 3L
  }
  dir_ <- fs::path(.dir, "_timing")
  fs::dir_create(dir_)
  path_ <- fs::path(dir_, sprintf("timing_%04d.parquet", as.integer(.chunk)))
  arrow::write_parquet(
    tibble::tibble(
      Chunk     = as.integer(.chunk),
      nDocs     = as.integer(.n_docs),
      nTasks    = as.integer(.n_tasks),
      Seconds   = as.numeric(.seconds),
      WrittenAt = Sys.time()
    ),
    path_
  )
  invisible(path_)
}

#' Report measured throughput and project the remaining work
#'
#' The rate is per DOCUMENT-TASK, not per document, because a chunk costs one model pass for each
#' task it labels: quoting a per-document figure from a three-task run would understate a one-task
#' run by a factor of three and overstate nothing, which is the wrong direction for a number someone
#' plans a night around.
#'
#' @param .dir Labels directory.
#' @param .n_corpus Documents in the full corpus.
#' @param .n_tasks Tasks a full pass would label.
#' @return Invisibly the timing tibble, or NULL where nothing has been timed.
mc_report_throughput <- function(.dir, .n_corpus, .n_tasks = 3L) {
  if (FALSE) {
    .dir      <- .dir_labels
    .n_corpus <- nrow(tab_index)
    .n_tasks  <- 3L
  }
  dir_ <- fs::path(.dir, "_timing")
  if (!fs::dir_exists(dir_)) {
    cli::cli_alert_info("Nothing timed yet; run at least one chunk to measure this machine.")
    return(invisible(NULL))
  }
  tab_ <- fs::dir_ls(dir_, glob = "*.parquet") |>
    purrr::map(arrow::read_parquet) |>
    purrr::list_rbind()
  if (nrow(tab_) == 0L) return(invisible(NULL))

  secs_  <- sum(tab_$Seconds)
  units_ <- sum(tab_$nDocs * tab_$nTasks)
  rate_  <- units_ / secs_

  cli::cli_h2("Measured throughput")
  clf_say_table(
    .tab = tibble::tibble(
      Chunks      = nrow(tab_),
      Documents   = sum(tab_$nDocs),
      DocTasks    = units_,
      Minutes     = round(secs_ / 60, 1),
      PerSecond   = round(rate_, 1),
      FullPassHrs = round(.n_corpus * .n_tasks / rate_ / 3600, 1)
    )
  )
  cli::cli_text("")
  cli::cli_alert_info(
    "Rate is per document-task: a chunk costs one model pass for each task it labels, so a \\
     per-document figure from a {max(tab_$nTasks)}-task run would misprice a one-task run."
  )
  invisible(tab_)
}


# 7. Composition -----------------------------------------------------------------------------------
# The corpus does not have to look like the labelled sample, and it will not: the sample was drawn to
# support estimation, with the thin categories deliberately over-represented so they could be learnt
# at all. A gap is therefore expected. What the gap CANNOT distinguish on its own is whether it comes
# from that sampling design or from the model drifting toward frequent categories on documents unlike
# anything it trained on -- and those have very different consequences for the released labels.
#
# The confidence tiers separate them. If documents the arms agreed on look like the labelled sample
# while the ones they did not agree on carry the skew, the drift is on the uncertain documents and
# the flag is already isolating it. If every tier is skewed alike, the difference is composition and
# the labels are fine.

#' Category composition: labelled sample, corpus, and each confidence tier
#'
#' @param .tab_labels Output of a corpus pass.
#' @param .tab_prep Labelled sample.
#' @param .label_col Task to compare.
#' @return Tibble: Label, SampleShare, CorpusShare, one column per tier.
mc_distribution <- function(.tab_labels, .tab_prep, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab_labels <- tab_labels
    .tab_prep   <- tab_prep
    .label_col  <- "ClassDetailed"
  }
  sample_ <- .tab_prep |>
    dplyr::filter(!is.na(.data[[.label_col]])) |>
    dplyr::count(Label = .data[[.label_col]], name = "nSample") |>
    dplyr::mutate(SampleShare = .data$nSample / sum(.data$nSample))

  corp_ <- .tab_labels |>
    dplyr::filter(.data$Task == .label_col) |>
    dplyr::count(Label, name = "nCorpus") |>
    dplyr::mutate(CorpusShare = .data$nCorpus / sum(.data$nCorpus))

  tiers_ <- .tab_labels |>
    dplyr::filter(.data$Task == .label_col, !is.na(.data$Tier)) |>
    dplyr::count(Tier, Label, name = "n") |>
    dplyr::mutate(Share = .data$n / sum(.data$n), .by = Tier) |>
    dplyr::select(Tier, Label, Share) |>
    tidyr::pivot_wider(names_from = Tier, values_from = Share, values_fill = 0)

  sample_ |>
    dplyr::select(Label, SampleShare) |>
    dplyr::full_join(corp_ |> dplyr::select(Label, CorpusShare), by = dplyr::join_by(Label)) |>
    dplyr::full_join(tiers_, by = dplyr::join_by(Label)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(.x) tidyr::replace_na(.x, 0))) |>
    dplyr::arrange(dplyr::desc(.data$CorpusShare))
}

#' Report composition, and the distance of each tier from the labelled sample
#'
#' Total variation distance is the summary: half the sum of absolute share differences, which is the
#' largest share of documents any single reweighting could move. Zero means identical composition,
#' one means disjoint. Read the tiers against each other rather than against any absolute standard --
#' the question is not whether a tier differs from the sample but whether the tiers differ from each
#' other, because only the second is evidence about the model.
#'
#' @param .tab Output of mc_distribution().
#' @return Invisibly the distance tibble.
mc_report_distribution <- function(.tab) {
  if (FALSE) .tab <- dist_det

  cli::cli_h2("Category composition")
  .tab |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), clf_pct)) |>
    clf_say_table()

  cols_ <- setdiff(names(.tab), c("Label", "SampleShare"))
  tvd_ <- purrr::map(cols_, function(.c) {
    tibble::tibble(
      Set = .c,
      TVD = 0.5 * sum(abs(.tab[[.c]] - .tab$SampleShare))
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$TVD)

  cli::cli_text("")
  tvd_ |>
    dplyr::mutate(TVD = sprintf("%.3f", .data$TVD)) |>
    clf_say_table(.title = "Distance from the labelled sample's composition")
  cli::cli_text("")

  agree_ <- tvd_$TVD[tvd_$Set == "unanimous"]
  other_ <- tvd_$TVD[tvd_$Set %in% c("sole", "split")]
  if (length(agree_) == 1L && length(other_) > 0L) {
    if (agree_ < min(other_)) {
      cli::cli_alert_info(
        "Documents the arms agreed on sit closer to the labelled sample than the ones they did not. \\
         That is the signature of drift concentrated on uncertain documents, which the flag is \\
         already isolating -- the skew travels with low confidence rather than with the corpus."
      )
    } else {
      cli::cli_alert_info(
        "Every tier is skewed alike, so the difference is composition rather than model behaviour: \\
         the labelled sample over-represents thin categories by design and the corpus does not. \\
         The labels are unaffected; the reliability estimates carry the sample's composition and \\
         that belongs in the text."
      )
    }
  }
  invisible(tvd_)
}

# Local null-coalescing helper (base R gained %||% in 4.4; this keeps the file self-contained).
`%||%` <- function(.x, .y) if (is.null(.x)) .y else .x
