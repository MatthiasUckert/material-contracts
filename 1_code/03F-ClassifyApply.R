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
# RESTARTING, AND WHY THE OUTPUT DIRECTORY IS NAMED FOR THE MANIFEST
# A corpus pass is chunked and idempotent: a chunk whose output is present is skipped, so a run that
# dies at chunk four hundred resumes rather than restarts. That is only safe while every chunk in a
# directory was produced under the same configuration. Keyed on the directory name alone, a manifest
# rewritten between two runs would leave the first run's chunks in place and the second would adopt
# them -- silently mixing two configurations in one output, with nothing on the rows to say so. The
# directory therefore carries the manifest's identity, and a new manifest means a new directory.
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
# WHAT COMES BACK, AND WHAT DOES NOT
# One row per document per task, carrying what each arm said independently -- its first and second
# choice and the score behind each -- plus the agreement tier and the reliability estimated for that
# tier under that voting set.
#
# THERE IS NO Label COLUMN, and that is the design rather than an omission. Every routing rule the
# orchestration stage searched is a deterministic function of the columns below: a cascade is "take
# the terminal's choice, override where the gated arm committed above the floor", which on a finished
# table is one mutate. Inference over the corpus is hours; applying a rule to a finished table is
# seconds. Deciding the label at inference time therefore buys nothing and forecloses changing one's
# mind, so the label is derived in a later step and this stage ships the evidence for it.
#
# Per-arm scores travel for a second reason: without them one can see that the arms agreed but not
# which was more confident when they disagreed, and that is precisely the case worth inspecting.
#
# WHY ARM COLUMNS ARE NAMED FOR THE KIND
# Bert_, Kw_, Llm_ rather than the arm's own name. An arm name is assigned relative to the inventory
# it was built from and carries a positional suffix among siblings, so the same checkpoint answers to
# different names in two runs whose inventories differ. A column name baked into several million rows
# has to be stable across runs or every script written against the first output breaks silently
# against the second. Exactly one arm of each kind runs, so the kind identifies it without ambiguity,
# and the arm's real name, variant, configuration and validated score are written beside the labels
# as a run record.
#
# THE TWO Score2 COLUMNS ARE NOT THE SAME KIND OF OBJECT
# The transformer's is softmax mass over categories; the keyword table's is a Wilson lower bound on
# training precision, and it is NA where only one class matched at all. The generative arm has no
# runner-up: a constrained schema returns one label. They are named alike because they occupy the
# same slot, not because they are comparable, and no downstream comparison across kinds is valid.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new; if (FALSE) dev blocks; cli/fs/here;
# pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_manifest <- utils_file_path(.dir_orch, "deployment", "manifest.json")
  .docs          <- arrow::read_parquet(.lP$Input$Corpus)
}

MC_NONE <- "(none)"

#' A manifest parameter that has no safe default
#'
#' Some parameters describe the encoding an artifact was built under -- the transformer's context
#' length, the keyword table's truncation window. Substituting a default for a missing one produces
#' output that is wrong in a way nothing downstream can detect, because the result is a perfectly
#' ordinary label. These stop the run instead.
#'
#' @param .x The value read from the manifest.
#' @param .name Parameter name, for the message.
#' @param .kind Arm kind, for the message.
#' @return .x, invisibly unchanged, or an abort.
mc_require <- function(.x, .name, .kind) {
  if (FALSE) {
    .x    <- NULL
    .name <- "max_len"
    .kind <- "transformer"
  }
  if (is.null(.x) || length(.x) != 1L || is.na(.x)) {
    cli::cli_abort(c(
      "The manifest pins no {(.name)} for the {(.kind)} arm.",
      "i" = "This parameter defines the encoding the artifact was built under, so there is no \\
             default that is safe to assume.",
      "i" = "Re-run 03E: its catalogue writes params for every arm it pins."
    ))
  }
  .x
}


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
        LabelCol = .t$label_col %||% NA_character_,
        Arm      = .a$arm %||% NA_character_,
        Kind     = .a$kind %||% NA_character_,
        # The variant is what a caller names to override the default -- L512, W1024, blind -- so it
        # travels beside the arm rather than being parsed back out of it.
        Variant  = .a$variant %||% NA_character_,
        Default  = isTRUE(.a$default),
        Enabled  = isTRUE(.a$enabled),
        Artifact = as.character(fs::path_abs(.a$artifact %||% ".", start = here::here())),
        Score    = .a$scored$value %||% NA_real_,
        Measure  = .a$scored$measure %||% NA_character_,
        Params   = list(.a$params)
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

  # Only the arms this run would actually reach. A variant pinned for a caller who might one day
  # prefer it is not a reason to abort a run that is not using it, but a DEFAULT that has gone
  # missing is: it is what the next chunk would have been labelled with.
  gone_ <- arms_ |> dplyr::filter(.data$Default, .data$Enabled, !.data$OnDisk)
  if (nrow(gone_) > 0L) {
    cli::cli_abort(c(
      "The manifest marks artifacts as default that are no longer on disk: {gone_$Arm}",
      "i" = "Re-run the stage that wrote them, or re-run 03E to record their current location."
    ))
  }
  stray_ <- arms_ |> dplyr::filter(!.data$Default, !.data$OnDisk)
  if (nrow(stray_) > 0L) {
    cli::cli_alert_warning(
      "{nrow(stray_)} pinned variant{?s} {?is/are} not on disk and cannot be selected with \\
       {.arg .prefer}: {stray_$Arm}."
    )
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
    dplyr::mutate(Accuracy = tbl_pct(.data$Accuracy), MacroF1 = sprintf("%.3f", .data$MacroF1)) |>
    tbl_say(.title = "Tasks, and what each scored when validated")

  .man$Arms |>
    dplyr::mutate(
      Default = dplyr::if_else(.data$Default, "<-", ""),
      Enabled = dplyr::if_else(.data$Enabled, "yes", "no"),
      OnDisk  = dplyr::if_else(.data$OnDisk, "yes", "NO"),
      Score   = sprintf("%.3f", .data$Score)
    ) |>
    dplyr::select(LabelCol, Kind, Variant, Default, Enabled, OnDisk, Score, Arm) |>
    tbl_say(.title = "Arms, per task")

  cli::cli_text("")
  cli::cli_alert_info(
    "The arrow marks what runs. Every other row is a variant a caller can select with \\
     {.arg .prefer}, at the score shown; scores are comparable within a kind and not across kinds."
  )

  # A flag the manifest does not pin is not a flag this stage can attach. Saying so here rather than
  # leaving four NA columns to be discovered in the output is the difference between a known gap and
  # an apparent bug.
  if (isTRUE(m_$confidence$shipped)) {
    cli::cli_alert_info(
      "Confidence flag estimated on {m_$confidence$label_col}, default voting set \\
       {m_$confidence$default_set}. Other tasks ship without a tier: a concurrence flag needs more \\
       than one deployable arm, and only that task has one."
    )
  } else {
    cli::cli_alert_warning(
      "This manifest pins NO confidence flag, so Tier, Reliability and VotingSet ship as NA on \\
       every row. The per-arm columns are unaffected and the label a later step derives from them \\
       is unaffected: the flag is a separate estimate, not an input to either."
    )
  }
  invisible(.man)
}


#' Column prefix for an arm kind
#'
#' One place, because the widening below and anything reading the output have to agree, and a second
#' inline mapping is how a reader ends up looking for Bert_Pred1 in a file that spells it
#' transformer_Pred1.
#'
#' @param .kind Character vector of arm kinds.
#' @return Character vector of column prefixes.
mc_kind_prefix <- function(.kind) {
  if (FALSE) .kind <- c("transformer", "keyword", "llm")
  out_ <- c(transformer = "Bert", keyword = "Kw", llm = "Llm")[.kind]
  if (anyNA(out_)) cli::cli_abort("No column prefix for arm kind {.val {unique(.kind[is.na(out_)])}}.")
  unname(out_)
}

#' Identity of the configuration a corpus pass ran under
#'
#' Names the output directory. A corpus pass is resumable because a chunk already on disk is skipped,
#' and that is only sound while every chunk in the directory came from the same manifest. Keyed on
#' the directory name alone, a manifest rewritten between two runs leaves the first run's chunks in
#' place for the second to adopt: two configurations in one output, with nothing on the rows saying
#' so. A new manifest therefore means a new directory, and resuming into the wrong one is impossible
#' rather than merely unlikely.
#'
#' Hashed over the arms and their parameters rather than over the file, so re-rendering the
#' orchestration stage without changing what it pins does not orphan a half-finished pass.
#'
#' @param .man Output of mc_manifest().
#' @return Character scalar, safe as a directory name.
mc_run_key <- function(.man) {
  if (FALSE) .man <- man
  sig_ <- .man$Arms |>
    dplyr::filter(.data$Enabled) |>
    dplyr::arrange(.data$LabelCol, .data$Kind, .data$Arm) |>
    dplyr::transmute(
      .data$LabelCol, .data$Kind, .data$Arm, .data$Variant, .data$Default,
      Params = purrr::map_chr(.data$Params, \(.p) paste(names(.p), unlist(.p), collapse = "|"))
    )
  paste0("run_", substr(rlang::hash(list(sig_, .man$Manifest$confidence$default_set)), 1L, 10L))
}

#' Which arm of each kind runs, for one task
#'
#' The manifest ships every deployable variant and marks one per kind. This resolves the marked one
#' unless a caller names another, and returns exactly one arm per kind so the wide output has one
#' column set per kind and never two.
#'
#' A departure is recorded rather than refused. The catalogue exists so that a caller with throughput
#' figures can trade accuracy against cost -- a shorter context length at the same measured accuracy
#' is a real saving -- and forbidding that would make the catalogue decorative. What it may not do is
#' happen quietly, so the chosen variant travels on the result and a non-default choice is named in
#' the console before a single document is read.
#'
#' @param .arms The manifest arm table from mc_manifest().
#' @param .label_col Task.
#' @param .prefer Named character vector, kind to variant, e.g. c(transformer = "L512"). NULL takes
#'   every default.
#' @param .quiet Logical. TRUE suppresses the departure notice, which is reported once at run level.
#' @return Tibble of the arms to run, one per kind, with a logical Departed column.
mc_arms <- function(.arms, .label_col, .prefer = NULL, .quiet = FALSE) {
  if (FALSE) {
    .arms      <- man$Arms
    .label_col <- "ClassDetailed"
    .prefer    <- c(transformer = "L512")
    .quiet     <- FALSE
  }
  mine_ <- .arms |> dplyr::filter(.data$LabelCol == .label_col, .data$Enabled, .data$OnDisk)
  if (nrow(mine_) == 0L) {
    cli::cli_abort(c(
      "No enabled arm is on disk for {(.label_col)}.",
      "i" = "Pinned for this task: {.arms$Arm[.arms$LabelCol == .label_col]}"
    ))
  }

  out_ <- purrr::map(unique(mine_$Kind), function(.k) {
    cand_ <- mine_ |> dplyr::filter(.data$Kind == .k)
    want_ <- if (!is.null(.prefer) && .k %in% names(.prefer)) .prefer[[.k]] else NA_character_
    row_  <- if (!is.na(want_)) cand_ |> dplyr::filter(.data$Variant == want_) else
             cand_ |> dplyr::filter(.data$Default)
    if (nrow(row_) != 1L) {
      cli::cli_abort(c(
        "{(.label_col)}/{(.k)}: expected one arm, found {nrow(row_)}.",
        "i" = if (!is.na(want_)) "Requested variant {.val {want_}}." else "No variant is marked default.",
        "i" = "Available: {cand_$Variant}"
      ))
    }
    row_ |> dplyr::mutate(Departed = !.data$Default)
  }) |>
    purrr::list_rbind()

  dep_ <- out_ |> dplyr::filter(.data$Departed)
  if (nrow(dep_) > 0L && !.quiet) {
    cli::cli_alert_warning(
      "{(.label_col)}: running {nrow(dep_)} non-default variant{?s} ({dep_$Kind}/{dep_$Variant}). \\
       The output records this; it is not comparable with rows produced under the defaults."
    )
  }
  out_
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
    # From the manifest, never from a fallback. A model applied at a context length it was not
    # fitted under encodes its input differently from its training data, produces perfectly ordinary
    # labels, and says nothing about it anywhere. A missing parameter is a broken manifest, so it
    # stops here rather than defaulting to whatever the shortest window happened to be.
    "--max-len", as.character(mc_require(.params$max_len, "max_len", "transformer")),
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

  # CAPTURED, not echoed. stdout = "" sends the child's output to this console, and a transformers
  # script emits tqdm bars on stderr -- which redraw over a cli progress bar and leave a corpus pass
  # looking like it is producing garbage. Capturing also means the output is available to quote when
  # inference fails, where before it had already scrolled past.
  out_lines_ <- suppressWarnings(
    system2(.python, args = args_, stdout = TRUE, stderr = TRUE)
  )
  status_ <- attr(out_lines_, "status")
  if (!is.null(status_) && !identical(as.integer(status_), 0L)) {
    cli::cli_abort(c(
      "Transformer inference failed (exit {status_}).",
      "i" = "Last lines from the engine:",
      utils::tail(out_lines_, 10L)
    ))
  }

  # Top2 came in with the inference script's switch from argmax to topk. An older parquet has no
  # such columns and still reads: the runner-up is then NA, which is the honest value for "this file
  # does not record one" and is distinguishable from a genuine tie.
  res_ <- arrow::read_parquet(out_)
  res_ |>
    dplyr::transmute(
      DocID,
      Pred1  = .data$PredLabel,
      Score1 = as.numeric(.data$Top1Prob),
      Pred2  = if ("Top2Label" %in% names(res_)) as.character(.data$Top2Label) else NA_character_,
      Score2 = if ("Top2Prob" %in% names(res_)) as.numeric(.data$Top2Prob) else NA_real_
    )
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

  # TRUNCATED TO THE WINDOW THE TABLE WAS MINED UNDER. The published precision is a property of the
  # pair (lexicon, window): a table mined over the first 256 words earns its precision from preamble
  # vocabulary, and run against whole documents it fires on later matches the mining never saw and
  # never priced. The result is a table that quietly does not keep its promise, and the only symptom
  # is a precision nobody measured. A window of zero means the miner read the whole document.
  win_ <- as.integer(mc_require(.params$n_words, "n_words", "keyword"))
  txt_ <- dplyr::coalesce(txt_, "")
  if (win_ > 0L) {
    txt_ <- stringi::stri_replace_all_regex(
      str         = txt_,
      pattern     = paste0("^((?:\\S+\\s+){", win_, "}).*$"),
      replacement = "$1",
      opts_regex  = stringi::stri_opts_regex(dotall = TRUE)
    )
  }

  hay_ <- stringi::stri_trans_tolower(txt_)
  hits_ <- purrr::map(seq_len(nrow(lex_)), function(.i) {
    found_ <- stringi::stri_detect_fixed(hay_, paste0(" ", lex_$Term[[.i]], " "))
    if (!any(found_)) return(NULL)
    tibble::tibble(DocID = .docs$DocID[found_], Class = lex_$Class[[.i]],
                   Power = as.numeric(lex_$Power[[.i]]))
  }) |>
    purrr::list_rbind()

  base_ <- tibble::tibble(DocID = .docs$DocID, Pred1 = .none, Score1 = 0,
                          Pred2 = NA_character_, Score2 = NA_real_)
  if (nrow(hits_) == 0L) return(base_)

  # Ranked by CLASS, not by term. A document matching four terms of one category and one of another
  # has two candidate classes, not five candidate terms, and the runner-up worth recording is the
  # second class. Where only one class matched at all there is no runner-up, and NA says so -- a
  # zero there would read as a class scoring zero.
  by_class_ <- hits_ |>
    dplyr::slice_max(.data$Power, n = 1L, by = c(DocID, Class), with_ties = FALSE) |>
    dplyr::arrange(.data$DocID, dplyr::desc(.data$Power)) |>
    dplyr::mutate(Rank = dplyr::row_number(), .by = DocID) |>
    dplyr::filter(.data$Rank <= 2L)

  best_ <- by_class_ |>
    tidyr::pivot_wider(id_cols = DocID, names_from = "Rank", values_from = c("Class", "Power"),
                       names_sep = "")

  # A chunk in which no document matched two classes produces no rank-2 columns at all, and the
  # schema has to be the same in every chunk or the parquet set cannot be read as one dataset.
  if (!"Class2" %in% names(best_)) best_$Class2 <- NA_character_
  if (!"Power2" %in% names(best_)) best_$Power2 <- NA_real_

  best_ <- best_ |>
    dplyr::transmute(DocID, Pred1 = .data$Class1, Score1 = .data$Power1,
                     Pred2 = .data$Class2, Score2 = .data$Power2)

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
#' A constrained schema returns one label, so there is no runner-up to record. NA rather than a
#' repeat of the first choice or an empty string: the column means "this arm reports no second
#' choice", which is a different statement from "the second choice was nothing".
#'
#' @param ... Passed to llm_classify() from 03D.
#' @return Tibble: DocID, Pred1, Score1, Pred2, Score2.
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
    dplyr::transmute(DocID, Pred1 = .data$PredLabel, Score1 = .data$Score,
                     Pred2 = NA_character_, Score2 = NA_real_)
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
#' NOT CALLED BY THIS STAGE. The label is derived in a later step from the finished table, because
#' every policy in the search is a deterministic function of the per-arm columns shipped here and
#' applying one to a finished table costs seconds against hours of inference. This function is the
#' implementation that step uses, kept beside the arms it reads so the two cannot drift; it expects
#' long per-arm rows (DocID, Arm, Pred1, Score1), which is the shape mc_apply_* returns before the
#' widening below.
mc_route <- function(.arm_pred, .policy, .none = MC_NONE) {
  if (FALSE) {
    .arm_pred <- arm_pred
    .policy   <- man$Manifest$tasks$ClassDetailed$policy
  }
  order_ <- unlist(.policy$order) %||% character(0)

  out_ <- .arm_pred |>
    dplyr::filter(.data$Arm == .policy$terminal) |>
    dplyr::transmute(DocID, Label = .data$Pred1, DecidedBy = .policy$terminal,
                     Prob = .data$Score1)
  if (nrow(out_) == 0L) cli::cli_abort("Terminal arm {(.policy$terminal)} produced no predictions.")
  if (length(order_) == 0L) return(out_)

  for (arm_ in rev(order_)) {
    cand_ <- .arm_pred |>
      dplyr::filter(.data$Arm == arm_, .data$Pred1 != .none, .data$Score1 >= .policy$floor) |>
      dplyr::select(DocID, ArmPred = .data$Pred1, ArmScore = .data$Score1)
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
#' @param .wide One row per document, carrying RefPred: the terminal arm's first choice.
#' @param .arm_pred Long per-arm predictions (DocID, Arm, Pred1, Score1, Pred2, Score2).
#' @param .arms Arm names that voted.
#' @param .reliability Tier reliability table from the manifest directory.
#' @param .set_name Voting set applied.
#' @param .none Abstention sentinel.
#' @return .wide plus nCommit, nConcur, Tier, Reliability.
mc_flag <- function(.wide, .arm_pred, .arms, .reliability, .set_name, .none = MC_NONE) {
  if (FALSE) {
    .wide        <- wide_
    .arm_pred    <- arm_pred_
    .arms        <- c("legal-bert:L256_E6_LR_CW1#8", "kw-text:W256")
    .reliability <- rel_
    .set_name    <- "core"
  }
  # Concurrence is measured against the TERMINAL's first choice rather than against a shipped label,
  # because this stage ships no label. That is the same quantity the estimating stage computed while
  # nothing routed, and it is the only one available before a routing rule has been chosen.
  ref_ <- .wide |> dplyr::select(DocID, Ref = .data$RefPred)

  pat_ <- .arm_pred |>
    dplyr::filter(.data$Arm %in% .arms, .data$Pred1 != .none, !is.na(.data$Pred1)) |>
    dplyr::inner_join(ref_, by = dplyr::join_by(DocID)) |>
    dplyr::summarise(
      nCommit = dplyr::n(),
      nConcur = sum(.data$Pred1 == .data$Ref),
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

  .wide |>
    dplyr::left_join(pat_ |> dplyr::select(DocID, nCommit, nConcur, Tier),
                     by = dplyr::join_by(DocID)) |>
    tidyr::replace_na(list(nCommit = 0L, nConcur = 0L, Tier = "sole")) |>
    dplyr::left_join(rel_, by = dplyr::join_by(Tier))
}

# 4. The entry point -------------------------------------------------------------------------------

#' Classify documents using the deployed configuration
#'
#' The one function to call. Reads the manifest, runs the arm each kind marks as default, attaches
#' the confidence flag where the manifest pins one, and returns one row per document per task.
#'
#' It does not route and it does not label. See the header: the label is a deterministic function of
#' the columns returned here, so deriving it downstream costs seconds and keeps the choice open,
#' where deciding it now would cost a second corpus pass to revisit.
#'
#' Every argument that is NULL takes its value from the manifest. Passing one explicitly is allowed
#' and recorded: the returned table carries a Departures attribute naming what was overridden, so a
#' result produced under a non-validated configuration says so rather than looking like any other.
#'
#' @param .docs Documents: DocID, Text, and DocDesc where the keyword arm uses it.
#' @param .manifest Output of mc_manifest().
#' @param .tasks Tasks to label. NULL takes every task the manifest carries.
#' @param .prefer Named character vector, kind to variant, selecting a non-default artifact.
#' @param .voting_set Voting set for the confidence flag. NULL takes the manifest default.
#' @param .tab_prep Labelled sample, required only when the generative arm runs.
#' @param .in_sample DocIDs of the labelled sample, marking rows the arms were fitted on. NULL
#'   leaves InSample NA rather than asserting a document was unseen.
#' @param .python,.script Inference binary and script for the transformer arm.
#' @param .batch_size Documents per forward pass.
#' @param .none Abstention sentinel.
#' @param .quiet Logical. TRUE suppresses the per-task headers and the departure notice, which are
#'   worth reading once and are noise once per chunk. Aborts are never suppressed: a run that cannot
#'   proceed says so whatever this is set to.
#' @param ... Passed to the generative arm.
#' @return Tibble: DocID, Task, VotingSet, InSample, one Pred1/Score1/Pred2/Score2 set per arm kind,
#'   nCommit, nConcur, Tier, Reliability. No Label: it is derived downstream. Carries a Departures
#'   attribute.
mc_classify <- function(.docs, .manifest, .tasks = NULL, .prefer = NULL, .voting_set = NULL,
                        .tab_prep = NULL, .in_sample = NULL, .python = NULL, .script = NULL,
                        .batch_size = 32L, .none = MC_NONE, .quiet = FALSE, ...) {
  if (FALSE) {
    .docs      <- dplyr::slice_head(tab_docs, n = 100L)
    .manifest  <- man
    .tasks     <- "ClassDetailed"
    .prefer    <- NULL
    .in_sample <- tab_prep$DocID
    .quiet     <- FALSE
  }
  m_ <- .manifest$Manifest

  dep_ <- c(
    if (!is.null(.prefer))     "prefer",
    if (!is.null(.voting_set)) "voting_set"
  )
  # Reported once per call. Under a chunked pass that is once per chunk, which is why the run-level
  # report reads the Departures attribute instead: the fact belongs to the run, not to each chunk.
  if (length(dep_) > 0L && !.quiet) {
    cli::cli_alert_warning(
      "Departing from the validated configuration on: {dep_}. The result carries this in its \\
       Departures attribute; it is not comparable with rows produced under the manifest."
    )
  }

  tasks_ <- .tasks %||% purrr::map_chr(m_$tasks, "label_col")

  # The flag ships only if the manifest pins one. Reading shipped rather than inferring it from an
  # empty set keeps the two possible states apart: a manifest that pins no flag, and a manifest whose
  # flag this run could not compute. Both give NA columns; only the second is a problem.
  shipped_   <- isTRUE(m_$confidence$shipped)
  set_       <- if (shipped_) .voting_set %||% m_$confidence$default_set else NA_character_
  vote_      <- if (shipped_) unlist(m_$confidence$sets[[set_]]) else character(0)
  flag_task_ <- if (shipped_) m_$confidence$label_col %||% tasks_[[1]] else NA_character_

  rel_ <- if (shipped_) {
    path_rel_ <- fs::path(.manifest$Dir, m_$confidence$reliability)
    if (!fs::file_exists(path_rel_)) cli::cli_abort("No reliability table at {path_rel_}")
    arrow::read_parquet(path_rel_)
  } else {
    NULL
  }

  out_ <- purrr::map(tasks_, function(.task) {
    # Arms are resolved per task, because each task crowned its own configuration. Resolving once
    # outside this loop is what put the detailed task's model in front of the broad task's policy.
    arms_run_ <- mc_arms(.arms = .manifest$Arms, .label_col = .task, .prefer = .prefer,
                         .quiet = .quiet)

    term_ <- m_$tasks[[.task]]$policy$terminal
    if (!term_ %in% arms_run_$Arm) {
      cli::cli_abort(c(
        "Task {(.task)} pins terminal {(term_)}, which is not among the arms this run would apply.",
        "i" = "Running: {arms_run_$Arm}",
        "i" = "A terminal absent from the run is a reference nothing can be compared against."
      ))
    }

    if (!.quiet) {
      cli::cli_h2(
        "Labelling {(.task)}: {nrow(.docs)} document{?s} through {nrow(arms_run_)} arm{?s}"
      )
    }

    arm_pred_ <- purrr::map(seq_len(nrow(arms_run_)), function(.i) {
      a_ <- arms_run_[.i, ]
      p_ <- a_$Params[[1]] %||% list()
      res_ <- switch(a_$Kind,
        transformer = mc_apply_bert(
          .docs = .docs, .artifact = a_$Artifact, .params = p_,
          .script = .script, .python = .python, .batch_size = .batch_size
        ),
        keyword = mc_apply_keyword(.docs = .docs, .artifact = a_$Artifact, .params = p_,
                                   .none = .none),
        llm = mc_apply_llm(.docs = .docs, .params = p_, .tab_prep = .tab_prep,
                           .label_col = .task, .none = .none, ...),
        cli::cli_abort("Unknown arm kind {(a_$Kind)}")
      )
      res_ |> dplyr::mutate(Arm = a_$Arm, Kind = a_$Kind)
    }) |>
      purrr::list_rbind()

    # Widened on KIND, not on arm name. See the header: an arm name carries a positional suffix
    # relative to the inventory it was named in, and a column name written into several million rows
    # has to survive a re-render of the stage that produced it.
    wide_ <- arm_pred_ |>
      dplyr::mutate(Col = mc_kind_prefix(.kind = .data$Kind)) |>
      dplyr::select(DocID, Col, Pred1, Score1, Pred2, Score2) |>
      tidyr::pivot_wider(id_cols = DocID, names_from = "Col",
                         values_from = c("Pred1", "Score1", "Pred2", "Score2"),
                         names_glue = "{Col}_{.value}")

    wide_ <- .docs |>
      dplyr::select(DocID) |>
      dplyr::left_join(wide_, by = dplyr::join_by(DocID)) |>
      dplyr::left_join(
        arm_pred_ |> dplyr::filter(.data$Arm == term_) |>
          dplyr::select(DocID, RefPred = .data$Pred1),
        by = dplyr::join_by(DocID)
      )

    flagged_ <- if (shipped_ && identical(.task, flag_task_) && all(vote_ %in% arm_pred_$Arm)) {
      mc_flag(.wide = wide_, .arm_pred = arm_pred_, .arms = vote_,
              .reliability = rel_, .set_name = set_, .none = .none)
    } else {
      if (shipped_ && identical(.task, flag_task_) && !.quiet) {
        cli::cli_alert_warning(
          "Voting set {(set_)} needs {length(setdiff(vote_, arm_pred_$Arm))} arm{?s} this run is \\
           not producing; {(.task)} ships without a tier."
        )
      }
      wide_ |>
        dplyr::mutate(nCommit = NA_integer_, nConcur = NA_integer_,
                      Tier = NA_character_, Reliability = NA_real_)
    }

    # Every column exists on every task whether or not it could be filled, so the corpus reads as one
    # dataset and a later pass that starts shipping the flag changes values rather than schema.
    flagged_ |>
      dplyr::mutate(
        Task      = .task,
        VotingSet = if (identical(.task, flag_task_)) set_ else NA_character_,
        InSample  = if (is.null(.in_sample)) NA else .data$DocID %in% .in_sample,
        .after = DocID
      ) |>
      dplyr::select(-RefPred)
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
  # Commitment per kind, because that is what varies: the transformer always answers and the keyword
  # table answers where its terms fire, so the gap between the two columns IS the keyword arm's
  # coverage on this corpus -- the one number a published-table promise can be checked against.
  pref_ <- intersect(c("Bert", "Kw", "Llm"), sub("_Pred1$", "", grep("_Pred1$", names(.tab), value = TRUE)))
  .tab |>
    dplyr::summarise(
      nDocs = dplyr::n_distinct(.data$DocID),
      dplyr::across(dplyr::all_of(paste0(pref_, "_Pred1")),
                    \(.x) mean(!is.na(.x) & .x != MC_NONE), .names = "{.col}_commit"),
      .by = c(Task, VotingSet)
    ) |>
    dplyr::rename_with(\(.x) sub("_Pred1_commit$", " commits", .x)) |>
    dplyr::mutate(dplyr::across(dplyr::ends_with(" commits"), tbl_pct)) |>
    tbl_say()

  # A task with no flag has no tier, and printing NA% for it invites reading a missing measurement
  # as a bad one. Those rows are dropped and named instead.
  flagged_ <- .tab |> dplyr::filter(!is.na(.data$Tier))
  if (nrow(flagged_) > 0L) {
    flagged_ |>
      dplyr::summarise(nDocs = dplyr::n(), Reliability = dplyr::first(.data$Reliability),
                       .by = c(Task, Tier)) |>
      dplyr::mutate(Reliability = tbl_pct(.data$Reliability)) |>
      dplyr::arrange(.data$Task, .data$Tier) |>
      tbl_say(.title = "Confidence tiers")
  }
  no_flag_ <- setdiff(unique(.tab$Task), unique(flagged_$Task))
  if (length(no_flag_) > 0L) {
    cli::cli_alert_info(
      "{length(no_flag_)} task{?s} ship{?s/} without a tier ({no_flag_}): the manifest pins no \\
       flag, or the arms it votes with are not all running here."
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

  # The size of the TREE, recorded before any cap is applied. Once the index has been sampled its own
  # row count describes the draw, and a throughput projection multiplied by that would price the
  # preview and present it as the corpus pass. Carried as an attribute rather than a column because
  # it is a property of the index, not of each of its million rows.
  n_all_ <- nrow(idx_)

  if (!is.null(.limit) && .limit < nrow(idx_)) {
    idx_ <- withr::with_seed(.seed, dplyr::slice_sample(idx_, n = .limit))
    cli::cli_alert_warning(
      "Limited to {nrow(idx_)} of {n_all_} document{?s}, drawn at random across the whole tree. \\
       Labels from a limited run are written to a separate directory so they cannot be mistaken \\
       for a corpus pass, and the throughput projection still prices the full tree."
    )
  }
  attr(idx_, "nCorpus") <- n_all_
  idx_
}

#' Read the text for one chunk of the index
#'
#' The only place text enters memory, and it leaves again when the chunk is written. Documents whose
#' parquet is missing or empty are dropped rather than aborting a pass measured in days.
#'
#' The count is RETURNED, not printed. A warning per chunk is invisible twice over: once among a
#' thousand progress lines, and again on a resumed render, where the chunks that dropped documents
#' were skipped and say nothing at all. Reconciling indexed against labelled at the end is the only
#' account that survives a restart.
#'
#' @param .chunk Rows of the corpus index.
#' @return The chunk with a Text column, minus unreadable documents, carrying an nDropped attribute.
mc_read_chunk <- function(.chunk) {
  if (FALSE) .chunk <- dplyr::slice_head(tab_index, n = 100L)
  out_ <- .chunk |>
    dplyr::mutate(Text = purrr::map_chr(.data$Path, clf_read_text))
  keep_ <- !is.na(out_$Text) & nzchar(out_$Text)
  out_  <- out_[keep_, ]
  attr(out_, "nDropped") <- sum(!keep_)
  out_
}

# 6. The label store ------------------------------------------------------------------------------
# WHAT IS DONE IS A PROPERTY OF THE DOCUMENTS, NOT OF THE FILENAME THEY LANDED IN.
#
# The previous layout wrote one parquet per chunk and treated the presence of labels_0007.parquet as
# proof that chunk seven was finished. That is only true while "chunk seven" means the same documents
# in every run, and four ordinary things break it without changing a filename: a different chunk size,
# a different sample seed or cap, new contracts shifting every boundary after the insertion point, and
# a process killed mid-write leaving a truncated file that exists. Each failure is silent, and the
# symptom is a coverage table that reconciles because the missing documents are sitting inside
# somebody else's file.
#
# Keying on DocID removes the class of failure rather than detecting it. The work outstanding is an
# anti-join, so chunking happens AFTER the filter and is nothing but a transaction size -- change it
# freely, mid-run if you like. An interrupted insert rolls back and those documents reappear in the
# work list, where a truncated parquet would have reported itself complete.
#
# RunKey scopes it. A document is only labelled under a configuration, so "done" means done under
# this manifest and these arms; two configurations coexist in one table and are compared with a
# query rather than a directory diff. Everything reading the store must filter on it, which is why
# nothing here returns rows without one.
#
# A capped run is now simply a partial corpus pass. It draws from the whole tree, its labels are
# valid under the same configuration, and an uncapped run afterwards picks up what it missed -- so
# the separate preview directory is gone, having existed only to stop chunk numbers colliding.
#
# R owns this file. Python writes parquet and R reads it; the language seam is unchanged.

#' Open the label store, creating its tables if this is the first run
#'
#' @param .path Path to the DuckDB file.
#' @return A DBI connection. The caller disconnects.
mc_db_open <- function(.path) {
  if (FALSE) .path <- .lP$Output$Store
  fs::dir_create(fs::path_dir(.path))
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.path))
  DBI::dbExecute(con_, "CREATE TABLE IF NOT EXISTS timings (
      RunKey    VARCHAR,
      Chunk     INTEGER,
      nDocs     INTEGER,
      nTasks    INTEGER,
      Seconds   DOUBLE,
      WrittenAt TIMESTAMP)")
  con_
}

#' Documents already labelled under one configuration
#'
#' @param .con Connection from mc_db_open().
#' @param .run_key Configuration identity from mc_run_key().
#' @return Character vector of DocIDs, empty where the table does not yet exist.
mc_db_done <- function(.con, .run_key) {
  if (FALSE) {
    .con     <- con
    .run_key <- run_key
  }
  if (!DBI::dbExistsTable(.con, "labels")) return(character())
  DBI::dbGetQuery(
    .con,
    "SELECT DISTINCT DocID FROM labels WHERE RunKey = ?",
    params = list(.run_key)
  )$DocID
}

#' Append one chunk of labels, in a transaction
#'
#' The transaction is the point. A pass killed part-way leaves the store as it was, so the documents
#' in flight reappear in the work list rather than half-appearing in the output.
#'
#' A column set that does not match the table aborts. It means the arms changed -- a generative arm
#' enabled, a kind dropped -- and appending anyway would leave the store holding two shapes under one
#' name, with only the row order to say which was which.
#'
#' @param .con Connection from mc_db_open().
#' @param .tab Labels for one chunk, from mc_classify().
#' @param .run_key Configuration identity.
#' @return Invisibly the number of rows written.
mc_db_append <- function(.con, .tab, .run_key) {
  if (FALSE) {
    .con     <- con
    .tab     <- out_
    .run_key <- run_key
  }
  out_ <- .tab |> dplyr::mutate(RunKey = .run_key, .before = 1L)

  if (!DBI::dbExistsTable(.con, "labels")) {
    DBI::dbWriteTable(.con, "labels", out_)
    return(invisible(nrow(out_)))
  }
  have_ <- DBI::dbListFields(.con, "labels")
  if (!setequal(have_, names(out_))) {
    cli::cli_abort(c(
      "This chunk's columns do not match the label store.",
      "i" = "Only in the store: {setdiff(have_, names(out_))}",
      "i" = "Only in this chunk: {setdiff(names(out_), have_)}",
      "i" = "The arms changed. Start a run under the new configuration rather than mixing shapes."
    ))
  }
  DBI::dbWithTransaction(.con, {
    DBI::dbAppendTable(.con, "labels", out_[have_])
  })
  invisible(nrow(out_))
}

#' Record what one chunk cost
#' @param .con Connection from mc_db_open().
#' @param .run_key Configuration identity.
#' @param .chunk Chunk index within this pass.
#' @param .n_docs Documents labelled.
#' @param .seconds Wall time.
#' @param .n_tasks Tasks labelled; a chunk costs one model pass for each.
#' @return Invisibly NULL.
mc_db_timing <- function(.con, .run_key, .chunk, .n_docs, .seconds, .n_tasks) {
  if (FALSE) {
    .con     <- con
    .run_key <- run_key
    .chunk   <- 1L
    .n_docs  <- 5000L
    .seconds <- 180
    .n_tasks <- 3L
  }
  DBI::dbAppendTable(.con, "timings", tibble::tibble(
    RunKey    = .run_key,
    Chunk     = as.integer(.chunk),
    nDocs     = as.integer(.n_docs),
    nTasks    = as.integer(.n_tasks),
    Seconds   = as.numeric(.seconds),
    WrittenAt = Sys.time()
  ))
  invisible(NULL)
}

#' Read one configuration's labels back
#' @param .con Connection from mc_db_open().
#' @param .run_key Configuration identity.
#' @return Tibble of labels, RunKey dropped.
mc_db_labels <- function(.con, .run_key) {
  if (FALSE) {
    .con     <- con
    .run_key <- run_key
  }
  if (!DBI::dbExistsTable(.con, "labels")) {
    cli::cli_abort("No labels have been written yet: the store holds no table.")
  }
  DBI::dbGetQuery(.con, "SELECT * FROM labels WHERE RunKey = ?", params = list(.run_key)) |>
    tibble::as_tibble() |>
    dplyr::select(-RunKey)
}

#' Export one configuration's labels to parquet
#'
#' The store is a working file, not the deliverable. Two reasons to write parquet beside it: the
#' regression stage reads flat files, and DuckDB's on-disk format has changed across versions before
#' -- a single database holding the only copy of a pass measured in days is a worse artifact than a
#' file anything can open. Seconds against that pass, so it is not a trade.
#'
#' @param .con Connection from mc_db_open().
#' @param .run_key Configuration identity.
#' @param .path Destination parquet.
#' @return Invisibly .path.
mc_db_export <- function(.con, .run_key, .path) {
  if (FALSE) {
    .con     <- con
    .run_key <- run_key
    .path    <- .lP$Output$Export
  }
  fs::dir_create(fs::path_dir(.path))
  DBI::dbExecute(
    .con,
    paste0("COPY (SELECT * EXCLUDE RunKey FROM labels WHERE RunKey = ?) ",
           "TO '", as.character(.path), "' (FORMAT PARQUET)"),
    params = list(.run_key)
  )
  cli::cli_alert_success("Exported to {.path {as.character(.path)}}")
  invisible(.path)
}

#' Label every document in the index that this configuration has not labelled yet
#'
#' THE WORK LIST IS AN ANTI-JOIN, so chunking happens after the filter rather than before it. The
#' progress bar therefore counts only outstanding work by construction, not because a partition was
#' computed carefully, and the chunk size can change between runs -- or mid-run -- without meaning
#' anything, because it identifies nothing.
#'
#' A PROGRESS BAR RATHER THAN A LINE PER CHUNK. A thousand chunks is a thousand lines, which pushes
#' every warning off the top of the log and, in a rendered document, buries the findings under the
#' mechanics. Everything worth keeping is reported once at the end.
#'
#' @param .index Corpus index to label.
#' @param .con Connection from mc_db_open().
#' @param .run_key Configuration identity from mc_run_key().
#' @param .chunk_size Documents per transaction, and therefore how often the progress bar advances.
#'   It identifies nothing, so it can be lowered freely for a livelier bar or raised for fewer
#'   commits; a chunk measured in minutes is a bar that moves in minutes.
#' @param .fn Function taking a chunk with text and returning its labels. It must not print: cli
#'   output interleaved with a progress bar redraws over it. mc_classify() takes .quiet for this.
#' @return Invisibly a tibble: Indexed, Done, Chunks, Written, Dropped, Empty.
mc_corpus_pass <- function(.index, .con, .run_key, .chunk_size, .fn) {
  if (FALSE) {
    .index      <- tab_index
    .con        <- con
    .run_key    <- run_key
    .chunk_size <- 5000L
    .fn         <- function(.docs) mc_classify(.docs = .docs, .manifest = man, .quiet = TRUE)
  }
  done_ <- mc_db_done(.con = .con, .run_key = .run_key)
  todo_ <- .index |> dplyr::filter(!.data$DocID %in% done_)

  cli::cli_alert_info(
    "{length(done_)} of {nrow(.index)} indexed document{?s} already labelled under this \\
     configuration; {nrow(todo_)} to go."
  )
  base_ <- tibble::tibble(
    Indexed = nrow(.index), Done = length(done_), Chunks = 0L, Written = 0L, Dropped = 0L, Empty = 0L
  )
  if (nrow(todo_) == 0L) {
    cli::cli_alert_success("Nothing to do: this configuration has labelled the whole index.")
    return(invisible(base_))
  }
  chunks_ <- split(todo_, ceiling(seq_len(nrow(todo_)) / .chunk_size))

  tally_ <- new.env(parent = emptyenv())
  tally_$written <- 0L
  tally_$dropped <- 0L
  tally_$empty   <- 0L

  # Captured once and closed over. Reaching for the calling frame by depth instead would depend on
  # how many frames purrr puts between that closure and this one, which is purrr's business and not
  # a promise it makes.
  env_ <- rlang::current_env()
  cli::cli_progress_bar(
    name   = "Labelling",
    total  = nrow(todo_),
    format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | {cli::pb_current}/{cli::pb_total} docs | ETA {cli::pb_eta}",
    .envir = env_
  )
  # DRAWN IMMEDIATELY, and this is not cosmetic. cli redraws a bar only when something calls update,
  # and it suppresses the first draw for two seconds after creation. One update per chunk against
  # chunks measured in minutes therefore means the bar is created, never drawn, and first appears
  # when the first chunk finishes -- which looks exactly like a run that has hung.
  cli::cli_progress_update(set = 0L, force = TRUE, .envir = env_)

  # Counted in DOCUMENTS. Chunks are a transaction size and now identify nothing, so a bar measured
  # in them reports a unit the reader did not choose and cannot compare between runs.
  purrr::iwalk(chunks_, function(.chunk, .i) {
    on.exit(cli::cli_progress_update(inc = nrow(.chunk), .envir = env_), add = TRUE)

    # Text enters memory here and leaves when the chunk is committed. Reading the whole corpus first
    # would be simpler to write and impossible to run.
    t0_   <- Sys.time()
    docs_ <- mc_read_chunk(.chunk = .chunk)
    drop_ <- attr(docs_, "nDropped")
    tally_$dropped <- tally_$dropped + if (is.null(drop_)) 0L else drop_
    if (nrow(docs_) == 0L) {
      tally_$empty <- tally_$empty + 1L
      return(invisible(NULL))
    }

    out_ <- .fn(docs_)
    mc_db_append(.con = .con, .tab = out_, .run_key = .run_key)
    mc_db_timing(
      .con     = .con,
      .run_key = .run_key,
      .chunk   = as.integer(.i),
      .n_docs  = dplyr::n_distinct(out_$DocID),
      .seconds = as.numeric(difftime(Sys.time(), t0_, units = "secs")),
      .n_tasks = dplyr::n_distinct(out_$Task)
    )
    tally_$written <- tally_$written + 1L
    invisible(NULL)
  })
  cli::cli_progress_done(.envir = env_)

  out_ <- base_ |>
    dplyr::mutate(Chunks = length(chunks_), Written = tally_$written,
                  Dropped = tally_$dropped, Empty = tally_$empty)
  cli::cli_h2("Corpus pass")
  tbl_say(.tab = out_)
  cli::cli_text("")
  if (tally_$dropped > 0L) {
    cli::cli_alert_warning(
      "{tally_$dropped} document{?s} had no readable text and were dropped. This count covers the \\
       chunks written now; the coverage reconciliation below is the figure that survives a restart."
    )
  }
  invisible(out_)
}

#' Which indexed documents reached the output, and which did not
#'
#' A corpus pass drops documents whose parquet is missing or empty rather than aborting a run
#' measured in days, which is the right trade and leaves a hole nobody is told about: the per-chunk
#' warnings scroll past, and on a resumed render they do not print at all because the chunks that
#' dropped them were skipped. Two numbers in two different tables are then the only evidence, and
#' subtracting one from the other is not a check anybody performs.
#'
#' Reconciled against the LABELLED SAMPLE as well as the index. The sample is the one set of
#' documents whose contents are known, so if it survives at the corpus rate the loss is a property of
#' the archive; if it survives at a different rate, the loss is selective and the released labels are
#' missing a describable kind of document rather than a random slice.
#'
#' @param .tab_index The corpus index this run drew.
#' @param .tab_labels The labels read back from disk.
#' @param .tab_prep Labelled sample, or NULL to reconcile the index alone.
#' @return Tibble: Set, Indexed, Labelled, Missing, Reached.
mc_coverage <- function(.tab_index, .tab_labels, .tab_prep = NULL) {
  if (FALSE) {
    .tab_index  <- tab_index
    .tab_labels <- tab_labels
    .tab_prep   <- tab_prep
  }
  got_ <- unique(.tab_labels$DocID)

  rows_ <- list(
    tibble::tibble(
      Set      = "Corpus index",
      Indexed  = dplyr::n_distinct(.tab_index$DocID),
      Labelled = sum(unique(.tab_index$DocID) %in% got_)
    )
  )
  if (!is.null(.tab_prep)) {
    # Only the sampled documents this run actually drew. Counting the whole sample against a capped
    # index would report the cap as a loss.
    in_idx_ <- intersect(unique(.tab_prep$DocID), unique(.tab_index$DocID))
    rows_ <- c(rows_, list(tibble::tibble(
      Set      = "Labelled sample, within this index",
      Indexed  = length(in_idx_),
      Labelled = sum(in_idx_ %in% got_)
    )))
  }

  purrr::list_rbind(rows_) |>
    dplyr::mutate(
      Missing = .data$Indexed - .data$Labelled,
      Reached = dplyr::if_else(.data$Indexed > 0L, .data$Labelled / .data$Indexed, NA_real_)
    )
}

#' The indexed documents that produced no row
#'
#' Written out rather than counted, because a count says how much is missing and a list says what.
#' The path travels with the identifier so the claim is checkable against the archive instead of
#' being taken on trust.
#'
#' @param .tab_index The corpus index this run drew.
#' @param .tab_labels The labels read back from disk.
#' @return Tibble: DocID, Path, and whatever else the index carried.
mc_missing <- function(.tab_index, .tab_labels) {
  if (FALSE) {
    .tab_index  <- tab_index
    .tab_labels <- tab_labels
  }
  .tab_index |> dplyr::filter(!.data$DocID %in% unique(.tab_labels$DocID))
}

#' Report coverage, and say what a shortfall would mean
#'
#' @param .tab_index The corpus index this run drew.
#' @param .tab_labels The labels read back from disk.
#' @param .tab_prep Labelled sample, or NULL.
#' @return Invisibly the coverage tibble.
mc_report_coverage <- function(.tab_index, .tab_labels, .tab_prep = NULL) {
  if (FALSE) {
    .tab_index  <- tab_index
    .tab_labels <- tab_labels
    .tab_prep   <- tab_prep
  }
  cov_ <- mc_coverage(.tab_index = .tab_index, .tab_labels = .tab_labels, .tab_prep = .tab_prep)

  cli::cli_h2("Coverage: indexed against labelled")
  cov_ |>
    dplyr::mutate(Reached = tbl_pct(.data$Reached)) |>
    tbl_say()

  n_miss_ <- cov_$Missing[[1]]
  cli::cli_text("")
  if (n_miss_ == 0L) {
    cli::cli_alert_success("Every indexed document produced a row.")
    return(invisible(cov_))
  }
  cli::cli_alert_warning(
    "{n_miss_} indexed document{?s} produced no row: the file was missing or held no text."
  )

  # Comparing the two rates is the whole point of carrying the sample through. Equal rates mean the
  # loss is a property of the archive; unequal rates mean it is selective, and a released dataset
  # missing a describable kind of document is a different object from one missing a random slice.
  if (nrow(cov_) > 1L) {
    gap_ <- abs(cov_$Reached[[2]] - cov_$Reached[[1]])
    if (is.finite(gap_) && gap_ > 0.02) {
      cli::cli_alert_danger(
        "The labelled sample reaches the output at {tbl_pct(cov_$Reached[[2]])} against \\
         {tbl_pct(cov_$Reached[[1]])} for the index. The loss is selective, not incidental."
      )
    } else {
      cli::cli_alert_info(
        "The labelled sample reaches the output at the index's rate, so the loss is a property of \\
         the archive rather than of any kind of document."
      )
    }
  }
  invisible(cov_)
}

#' Report measured throughput and project the remaining work
#'
#' The rate is per DOCUMENT-TASK, not per document, because a chunk costs one model pass for each
#' task it labels: quoting a per-document figure from a three-task run would understate a one-task
#' run by a factor of three and overstate nothing, which is the wrong direction for a number someone
#' plans a night around.
#'
#' The projection must be against the WHOLE TREE, never against the index this run happened to hold.
#' A capped run indexes only its own draw, so passing that count back projects the preview and labels
#' it a corpus pass -- understated by exactly the factor the cap imposed, which is the one thing the
#' cap guarantees will be large. The measured RATE is correct either way; only the multiplier is
#' wrong, so this is a reporting fault rather than a measurement one, and correspondingly invisible.
#'
#' @param .con Connection from mc_db_open().
#' @param .run_key Configuration identity.
#' @param .n_corpus Documents in the FULL corpus, not in a limited index.
#' @param .n_tasks Tasks a full pass would label.
#' @param .n_indexed Documents this run indexed, or NULL. Compared against what was timed, so
#'   timings inherited from an earlier run are named rather than quietly averaged in.
#' @return Invisibly the timing tibble, or NULL where nothing has been timed.
mc_report_throughput <- function(.con, .run_key, .n_corpus, .n_tasks = 3L, .n_indexed = NULL) {
  if (FALSE) {
    .con       <- con
    .run_key   <- run_key
    .n_corpus  <- attr(tab_index, "nCorpus")
    .n_tasks   <- 3L
    .n_indexed <- nrow(tab_index)
  }
  tab_ <- DBI::dbGetQuery(
    .con, "SELECT * FROM timings WHERE RunKey = ?", params = list(.run_key)
  ) |>
    tibble::as_tibble()
  if (nrow(tab_) == 0L) {
    cli::cli_alert_info("Nothing timed yet; label at least one chunk to measure this machine.")
    return(invisible(NULL))
  }

  secs_  <- sum(tab_$Seconds)
  units_ <- sum(tab_$nDocs * tab_$nTasks)
  rate_  <- units_ / secs_

  cli::cli_h2("Measured throughput")
  tbl_say(
    .tab = tibble::tibble(
      Chunks      = nrow(tab_),
      Documents   = sum(tab_$nDocs),
      DocTasks    = units_,
      Minutes     = round(secs_ / 60, 1),
      PerSecond   = round(rate_, 1),
      Corpus      = .n_corpus,
      FullPassHrs = round(.n_corpus * .n_tasks / rate_ / 3600, 1)
    )
  )
  cli::cli_text("")
  cli::cli_alert_info(
    "Rate is per document-task: a chunk costs one model pass for each task it labels, so a \\
     per-document figure from a {max(tab_$nTasks)}-task run would misprice a one-task run."
  )
  cli::cli_alert_info(
    "FullPassHrs projects the {(.n_corpus)} documents in the corpus at this rate, whatever this \\
     run was capped at."
  )

  # Timings accumulate in the directory across runs, which is what lets a resumed pass keep the
  # measurements it already paid for. It also means a chunk size changed between runs leaves both
  # regimes in one average, and an average over two regimes describes neither.
  if (dplyr::n_distinct(tab_$nDocs) > 2L) {
    cli::cli_alert_warning(
      "These timings span {dplyr::n_distinct(tab_$nDocs)} chunk sizes, so the rate averages more \\
       than one regime. DELETE FROM timings for this run key to re-measure cleanly; the labels are \\
       a separate table and are not affected."
    )
  }
  if (!is.null(.n_indexed) && sum(tab_$nDocs) > .n_indexed) {
    cli::cli_alert_warning(
      "{sum(tab_$nDocs)} documents are timed here against {(.n_indexed)} in this run's index, so \\
       earlier runs are contributing. The rate stands; the chunk count is not this run's."
    )
  }
  invisible(tab_)
}


# 6b. Corpus statistics --------------------------------------------------------------------------
# Everything here is measured WITHOUT LABELS, which is the only kind of statement available at corpus
# scale and is more informative than it sounds. Two arms trained on different evidence, a taxonomy
# with a known hierarchy, and a runner-up beside every first choice give three independent handles on
# where the released labels are load-bearing and where they are close calls -- none of which requires
# knowing the truth for a single corpus document.
#
# None of it is a substitute for the validated estimate. Agreement is not accuracy: two arms can be
# wrong together, and the keyword arm shares vocabulary with the transformer's first layers. Read
# these as a map of where to look, not as a score.

#' Detailed category to broad parent, read off the labelled sample
#'
#' The hierarchy is a property of the taxonomy, so it is taken from the sample that defines it rather
#' than restated here. Aborts if a detailed category reaches more than one parent: the corpus check
#' below is only meaningful while the map is a function.
#'
#' @param .tab_prep Labelled sample carrying ClassDetailed and ClassBroad.
#' @return Tibble: ClassDetailed, ClassBroad.
mc_taxonomy_map <- function(.tab_prep) {
  if (FALSE) .tab_prep <- tab_prep
  map_ <- .tab_prep |>
    dplyr::distinct(.data$ClassDetailed, .data$ClassBroad) |>
    dplyr::filter(!is.na(.data$ClassDetailed), !is.na(.data$ClassBroad))
  dup_ <- map_ |> dplyr::count(.data$ClassDetailed) |> dplyr::filter(.data$n > 1L)
  if (nrow(dup_) > 0L) {
    cli::cli_abort(c(
      "{nrow(dup_)} detailed categor{?y/ies} reach more than one broad parent: {dup_$ClassDetailed}.",
      "i" = "The hierarchy check below assumes each detailed category has exactly one parent."
    ))
  }
  map_
}

#' What each arm said, per category, for one task
#'
#' ShareKw is computed over the documents the keyword arm COMMITTED to, not over all of them. Divided
#' by the whole corpus it would fall with coverage and read as a composition difference, when what it
#' describes is the composition of the subset the table was willing to speak about.
#'
#' @param .tab Output of a corpus pass.
#' @param .task Task to profile.
#' @param .margin Score gap below which a first choice counts as a close call.
#' @return Tibble: Category, nBert, ShareBert, ShareKw, KwCommit, Agree, MeanScore, CloseCall.
mc_task_profile <- function(.tab, .task, .margin = 0.10) {
  if (FALSE) {
    .tab    <- tab_labels
    .task   <- "ClassDetailed"
    .margin <- 0.10
  }
  d_ <- .tab |> dplyr::filter(.data$Task == .task)
  n_ <- nrow(d_)

  kw_n_ <- sum(d_$Kw_Pred1 != MC_NONE & !is.na(d_$Kw_Pred1))
  kw_   <- d_ |>
    dplyr::filter(.data$Kw_Pred1 != MC_NONE, !is.na(.data$Kw_Pred1)) |>
    dplyr::count(Category = .data$Kw_Pred1, name = "nKw") |>
    dplyr::mutate(ShareKw = .data$nKw / kw_n_)

  # Renamed BEFORE the grouping, not inside it: .by is tidyselect and takes a selection, where
  # group_by() takes expressions. A rename there is silently a different kind of thing.
  d_ |>
    dplyr::rename(Category = "Bert_Pred1") |>
    dplyr::summarise(
      nBert     = dplyr::n(),
      MeanScore = mean(.data$Bert_Score1, na.rm = TRUE),
      CloseCall = mean((.data$Bert_Score1 - .data$Bert_Score2) < .margin, na.rm = TRUE),
      # Agreement only where the keyword arm committed: a document it declined is not one the two
      # disagreed about.
      nBoth     = sum(.data$Kw_Pred1 != MC_NONE & !is.na(.data$Kw_Pred1)),
      Agree     = if (any(.data$Kw_Pred1 != MC_NONE, na.rm = TRUE)) {
        mean((.data$Kw_Pred1 == .data$Category)[.data$Kw_Pred1 != MC_NONE], na.rm = TRUE)
      } else {
        NA_real_
      },
      .by = Category
    ) |>
    dplyr::mutate(ShareBert = .data$nBert / n_, KwCommit = .data$nBoth / .data$nBert) |>
    dplyr::full_join(kw_, by = dplyr::join_by(Category)) |>
    dplyr::arrange(dplyr::desc(.data$ShareBert)) |>
    dplyr::select(Category, nBert, ShareBert, ShareKw, KwCommit, Agree, MeanScore, CloseCall)
}

#' Report one task's per-category profile
#' @param .tab Output of a corpus pass.
#' @param .task Task to profile.
#' @param .margin Close-call threshold.
#' @return Invisibly the profile.
mc_report_task_profile <- function(.tab, .task, .margin = 0.10) {
  if (FALSE) {
    .tab    <- tab_labels
    .task   <- "ClassDetailed"
    .margin <- 0.10
  }
  out_ <- mc_task_profile(.tab = .tab, .task = .task, .margin = .margin)
  cli::cli_h2("{(.task)}: what each arm said, by category")
  out_ |>
    dplyr::mutate(
      dplyr::across(c(ShareBert, ShareKw, KwCommit, Agree, CloseCall), tbl_pct),
      MeanScore = sprintf("%.3f", .data$MeanScore)
    ) |>
    tbl_say()
  cli::cli_text("")
  cli::cli_alert_info(
    "ShareKw is over the documents the keyword arm committed to; KwCommit and Agree are within the \\
     transformer's category. A category with high CloseCall is one the transformer separates weakly \\
     from its runner-up, which is where a released label is worth checking."
  )
  invisible(out_)
}

#' Do the two taxonomies agree with each other on the corpus
#'
#' The broad task is predicted independently of the detailed one, and the taxonomy says which broad
#' parent each detailed category belongs to. Those two facts can be checked against each other on
#' every document without a single label: where the independently-predicted parent differs from the
#' parent of the predicted detailed category, at least one of the two is wrong.
#'
#' It is a lower bound on joint error and an upper bound on nothing -- both can be wrong the same way
#' -- but it is measured on the released documents rather than on 4,398 of them, and it localises
#' disagreement to the categories where the two taxonomies pull apart.
#'
#' @param .tab Output of a corpus pass.
#' @param .map Output of mc_taxonomy_map().
#' @return Tibble: Parent, nDocs, Coherent, plus a Total row.
mc_hierarchy_agreement <- function(.tab, .map) {
  if (FALSE) {
    .tab <- tab_labels
    .map <- mc_taxonomy_map(.tab_prep = tab_prep)
  }
  det_ <- .tab |>
    dplyr::filter(.data$Task == "ClassDetailed") |>
    dplyr::select(DocID, Detailed = .data$Bert_Pred1)
  brd_ <- .tab |>
    dplyr::filter(.data$Task == "ClassBroad") |>
    dplyr::select(DocID, Broad = .data$Bert_Pred1)

  both_ <- det_ |>
    dplyr::inner_join(brd_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(.map, by = dplyr::join_by(Detailed == ClassDetailed)) |>
    dplyr::filter(!is.na(.data$ClassBroad)) |>
    dplyr::mutate(Coherent = .data$ClassBroad == .data$Broad)

  by_ <- both_ |>
    dplyr::summarise(nDocs = dplyr::n(), Coherent = mean(.data$Coherent), .by = ClassBroad) |>
    dplyr::rename(Parent = "ClassBroad") |>
    dplyr::arrange(dplyr::desc(.data$nDocs))

  dplyr::bind_rows(
    by_,
    tibble::tibble(Parent = "All", nDocs = nrow(both_), Coherent = mean(both_$Coherent))
  )
}

#' One row per document, with every task's answer from every arm side by side
#'
#' The corpus output is long in Task, which is right for storage and wrong for any question that
#' crosses tasks. Three tasks labelled the same documents, so the interesting facts -- whether the two
#' taxonomies cohere, whether amendments concentrate anywhere -- live in the join, not in any one
#' task's rows.
#'
#' @param .tab Output of a corpus pass.
#' @return Tibble: DocID and one column per task and arm.
mc_by_document <- function(.tab) {
  if (FALSE) .tab <- tab_labels
  one_ <- function(.task, .prefix) {
    .tab |>
      dplyr::filter(.data$Task == .task) |>
      dplyr::select(DocID, Bert = "Bert_Pred1", Kw = "Kw_Pred1") |>
      dplyr::rename_with(\(.x) paste0(.prefix, .x), -DocID)
  }
  one_("ClassDetailed", "Det") |>
    dplyr::inner_join(one_("ClassBroad", "Brd"), by = dplyr::join_by(DocID)) |>
    dplyr::inner_join(one_("AmendType", "Amd"), by = dplyr::join_by(DocID))
}

#' The taxonomy, filled in with what the corpus pass found
#'
#' Rows are the hierarchy itself -- every detailed category under its broad parent -- so the table is
#' read down the taxonomy rather than down a ranking, and a category the classifier never predicted
#' still occupies its row. An absent category and a zero are different findings, and a table sorted by
#' size cannot show the first.
#'
#' TWO ARMS, TWO DENOMINATORS. The keyword table abstains by design, on the amendment task as well as
#' the detailed one, so nKw counts documents it was willing to speak about and is not a smaller
#' estimate of the same quantity as nBert. The commit share is carried alongside so the gap is
#' attributable rather than mysterious.
#'
#' Incoherent counts documents whose independently-predicted broad class is not this category's
#' parent. It is the disagreement between the two taxonomies localised to where it happens, and it
#' needs no labels: the hierarchy alone says the two predictions cannot both be right.
#'
#' @param .tab Output of a corpus pass.
#' @param .map Output of mc_taxonomy_map().
#' @return Tibble: Broad, Detailed, nBert, nKw, KwCommit, nAmendBert, nAmendKw, Incoherent, Coherent.
mc_hierarchy_table <- function(.tab, .map) {
  if (FALSE) {
    .tab <- tab_labels
    .map <- mc_taxonomy_map(.tab_prep = tab_prep)
  }
  doc_ <- mc_by_document(.tab = .tab)

  bert_ <- doc_ |>
    dplyr::left_join(.map, by = dplyr::join_by(DetBert == ClassDetailed)) |>
    dplyr::summarise(
      nBert      = dplyr::n(),
      nAmendBert = sum(.data$AmdBert != "Original", na.rm = TRUE),
      # The parent the taxonomy assigns against the parent predicted on its own evidence.
      Incoherent = sum(.data$ClassBroad != .data$BrdBert, na.rm = TRUE),
      .by = DetBert
    ) |>
    dplyr::rename(Detailed = "DetBert")

  kw_ <- doc_ |>
    dplyr::filter(.data$DetKw != MC_NONE, !is.na(.data$DetKw)) |>
    dplyr::summarise(
      nKw      = dplyr::n(),
      nAmendKw = sum(.data$AmdKw != "Original" & .data$AmdKw != MC_NONE, na.rm = TRUE),
      .by = DetKw
    ) |>
    dplyr::rename(Detailed = "DetKw")

  # The MAP is the spine, not either arm's output: a category neither arm ever predicted belongs in
  # this table at zero, and joining onto an arm would delete exactly the rows worth noticing.
  .map |>
    dplyr::rename(Broad = "ClassBroad", Detailed = "ClassDetailed") |>
    dplyr::left_join(bert_, by = dplyr::join_by(Detailed)) |>
    dplyr::left_join(kw_, by = dplyr::join_by(Detailed)) |>
    dplyr::mutate(
      dplyr::across(c(nBert, nKw, nAmendBert, nAmendKw, Incoherent), \(.x) dplyr::coalesce(.x, 0L)),
      KwCommit  = dplyr::if_else(.data$nBert > 0L, .data$nKw / .data$nBert, NA_real_),
      Coherent  = dplyr::if_else(.data$nBert > 0L, 1 - .data$Incoherent / .data$nBert, NA_real_),
      AmendBert = dplyr::if_else(.data$nBert > 0L, .data$nAmendBert / .data$nBert, NA_real_)
    ) |>
    dplyr::arrange(.data$Broad, dplyr::desc(.data$nBert))
}

#' Report the taxonomy table, and its roll-up to the broad classes
#' @param .tab Output of a corpus pass.
#' @param .map Output of mc_taxonomy_map().
#' @return Invisibly the detailed table.
mc_report_hierarchy_table <- function(.tab, .map) {
  if (FALSE) {
    .tab <- tab_labels
    .map <- mc_taxonomy_map(.tab_prep = tab_prep)
  }
  out_ <- mc_hierarchy_table(.tab = .tab, .map = .map)

  cli::cli_h2("The taxonomy, as the corpus filled it in")
  out_ |>
    dplyr::mutate(
      dplyr::across(c(KwCommit, AmendBert, Coherent), tbl_pct)
    ) |>
    dplyr::select(Broad, Detailed, nBert, nKw, KwCommit, nAmendBert, nAmendKw, AmendBert,
                  Incoherent, Coherent) |>
    tbl_say()

  cli::cli_h2("Rolled up to the broad classes")
  out_ |>
    dplyr::summarise(
      Detailed   = dplyr::n(),
      nBert      = sum(.data$nBert),
      nKw        = sum(.data$nKw),
      nAmendBert = sum(.data$nAmendBert),
      nAmendKw   = sum(.data$nAmendKw),
      Incoherent = sum(.data$Incoherent),
      .by = Broad
    ) |>
    dplyr::mutate(
      KwCommit  = .data$nKw / .data$nBert,
      AmendBert = .data$nAmendBert / .data$nBert,
      Coherent  = 1 - .data$Incoherent / .data$nBert
    ) |>
    dplyr::arrange(dplyr::desc(.data$nBert)) |>
    dplyr::mutate(dplyr::across(c(KwCommit, AmendBert, Coherent), tbl_pct)) |>
    tbl_say()

  cli::cli_text("")
  cli::cli_alert_info(
    "nKw and nAmendKw count documents the keyword table committed to; KwCommit is that share, so \\
     the two arms' counts are not two estimates of one quantity."
  )
  cli::cli_alert_info(
    "Incoherent counts documents whose independently-predicted broad class is not this row's parent. \\
     At least one of the two predictions is wrong on each; the hierarchy says so without labels."
  )
  invisible(out_)
}

#' Which pairs of categories the model treats as near-substitutes
#'
#' The runner-up is the only statement about alternatives available without labels, and at corpus
#' scale the recurring first-to-second pairs are the taxonomy's real fault lines: two categories the
#' model repeatedly cannot separate are two categories a reader should not treat as cleanly distinct,
#' whichever way any single document went.
#'
#' Weighted by MARGIN as well as by count. A pair that recurs at a margin of 0.9 is the model being
#' certain about many similar documents; the same pair at 0.05 is a coin toss it happens to keep
#' making the same way, and only the second is a fault line.
#'
#' @param .tab Output of a corpus pass.
#' @param .task Task to profile.
#' @param .n Pairs to report.
#' @return Tibble: First, Second, nDocs, Share, MeanMargin, Close.
mc_second_guess <- function(.tab, .task, .n = 12L) {
  if (FALSE) {
    .tab  <- tab_labels
    .task <- "ClassDetailed"
    .n    <- 12L
  }
  d_ <- .tab |>
    dplyr::filter(.data$Task == .task, !is.na(.data$Bert_Pred2))
  if (nrow(d_) == 0L) {
    return(tibble::tibble(First = character(), Second = character(), nDocs = integer(),
                          Share = numeric(), MeanMargin = numeric(), Close = numeric()))
  }
  d_ |>
    dplyr::mutate(Margin = .data$Bert_Score1 - .data$Bert_Score2) |>
    dplyr::rename(First = "Bert_Pred1", Second = "Bert_Pred2") |>
    dplyr::summarise(
      nDocs      = dplyr::n(),
      MeanMargin = mean(.data$Margin, na.rm = TRUE),
      Close      = mean(.data$Margin < 0.10, na.rm = TRUE),
      .by = c(First, Second)
    ) |>
    dplyr::mutate(Share = .data$nDocs / nrow(d_)) |>
    dplyr::arrange(dplyr::desc(.data$nDocs)) |>
    dplyr::slice_head(n = .n) |>
    dplyr::select(First, Second, nDocs, Share, MeanMargin, Close)
}

#' When the arms disagree, is the keyword arm picking the transformer's runner-up
#'
#' A disagreement in which the keyword table names the category the transformer ranked second is a
#' different object from one in which it names something the transformer never considered. The first
#' is two arms splitting a close call; the second is a genuine conflict, and only the second is worth
#' a reader's time.
#'
#' @param .tab Output of a corpus pass.
#' @return Tibble: Task, nDisagree, ShareOfCommitted, IsRunnerUp, MeanMargin.
mc_near_miss <- function(.tab) {
  if (FALSE) .tab <- tab_labels
  .tab |>
    dplyr::filter(.data$Kw_Pred1 != MC_NONE, !is.na(.data$Kw_Pred1)) |>
    dplyr::mutate(Disagree = .data$Kw_Pred1 != .data$Bert_Pred1) |>
    dplyr::summarise(
      nCommitted       = dplyr::n(),
      nDisagree        = sum(.data$Disagree),
      ShareOfCommitted = mean(.data$Disagree),
      IsRunnerUp       = if (any(.data$Disagree)) {
        mean((.data$Kw_Pred1 == .data$Bert_Pred2)[.data$Disagree], na.rm = TRUE)
      } else {
        NA_real_
      },
      MeanMargin = if (any(.data$Disagree)) {
        mean((.data$Bert_Score1 - .data$Bert_Score2)[.data$Disagree], na.rm = TRUE)
      } else {
        NA_real_
      },
      .by = Task
    )
}

#' Amendment status crossed with the detailed category
#'
#' Three tasks labelled the same documents, so they can be read against each other. Whether
#' amendments concentrate in particular contract types is a fact about the corpus that nothing in the
#' per-task tables shows, and it is the kind of thing a user of the released data will want before
#' conditioning on either column.
#'
#' @param .tab Output of a corpus pass.
#' @return Tibble: Category, nDocs, Amended.
mc_amend_profile <- function(.tab) {
  if (FALSE) .tab <- tab_labels
  det_ <- .tab |>
    dplyr::filter(.data$Task == "ClassDetailed") |>
    dplyr::select(DocID, Category = .data$Bert_Pred1)
  amd_ <- .tab |>
    dplyr::filter(.data$Task == "AmendType") |>
    dplyr::select(DocID, Amend = .data$Bert_Pred1)

  det_ |>
    dplyr::inner_join(amd_, by = dplyr::join_by(DocID)) |>
    dplyr::summarise(
      nDocs   = dplyr::n(),
      Amended = mean(.data$Amend != "Original"),
      .by = Category
    ) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
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
#' Composition is taken over the TERMINAL ARM'S FIRST CHOICE, not over a released label, because this
#' stage releases none. That is the right comparison anyway: it is the same quantity the labelled
#' sample was scored on, and it does not move when a routing rule is chosen later.
#'
#' @param .tab_labels Output of a corpus pass.
#' @param .tab_prep Labelled sample.
#' @param .label_col Task to compare.
#' @param .column Prediction column standing for the corpus composition.
#' @return Tibble: Label, SampleShare, CorpusShare, one column per tier.
mc_distribution <- function(.tab_labels, .tab_prep, .label_col = "ClassDetailed",
                            .column = "Bert_Pred1") {
  if (FALSE) {
    .tab_labels <- tab_labels
    .tab_prep   <- tab_prep
    .label_col  <- "ClassDetailed"
    .column     <- "Bert_Pred1"
  }
  sample_ <- .tab_prep |>
    dplyr::filter(!is.na(.data[[.label_col]])) |>
    dplyr::count(Label = .data[[.label_col]], name = "nSample") |>
    dplyr::mutate(SampleShare = .data$nSample / sum(.data$nSample))

  corp_ <- .tab_labels |>
    dplyr::filter(.data$Task == .label_col) |>
    dplyr::count(Label = .data[[.column]], name = "nCorpus") |>
    dplyr::mutate(CorpusShare = .data$nCorpus / sum(.data$nCorpus))

  tiers_ <- .tab_labels |>
    dplyr::filter(.data$Task == .label_col, !is.na(.data$Tier)) |>
    dplyr::count(Tier, Label = .data[[.column]], name = "n") |>
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

#' Every pair of arms, and how often they made the same first choice
#'
#' Agreement is measured only where BOTH arms committed, because a pair cannot disagree about a
#' document one of them declined. Measured over all documents instead, an abstaining arm's agreement
#' would fall as its coverage fell and would read as a quality difference rather than a coverage one.
#'
#' @param .tab Output of a corpus pass.
#' @return Tibble: Task, Pair, nBoth, Agree.
mc_agreement <- function(.tab) {
  if (FALSE) .tab <- tab_labels
  pref_ <- sub("_Pred1$", "", grep("_Pred1$", names(.tab), value = TRUE))
  if (length(pref_) < 2L) {
    return(tibble::tibble(Task = character(), Pair = character(), nBoth = integer(),
                          Agree = numeric()))
  }
  pairs_ <- utils::combn(pref_, 2L, simplify = FALSE)

  purrr::map(pairs_, function(.p) {
    a_ <- paste0(.p[[1]], "_Pred1")
    b_ <- paste0(.p[[2]], "_Pred1")
    .tab |>
      dplyr::mutate(
        Both  = !is.na(.data[[a_]]) & !is.na(.data[[b_]]) &
                .data[[a_]] != MC_NONE & .data[[b_]] != MC_NONE,
        Same  = .data[[a_]] == .data[[b_]]
      ) |>
      dplyr::summarise(
        Pair  = paste(.p, collapse = " vs "),
        nBoth = sum(.data$Both),
        Agree = if (any(.data$Both)) mean(.data$Same[.data$Both]) else NA_real_,
        .by = Task
      )
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$Task, .data$Pair)
}

#' Share of documents each arm committed to
#'
#' @param .tab Output of a corpus pass.
#' @return Tibble: Task, Arm, Commit.
mc_commitment <- function(.tab) {
  if (FALSE) .tab <- tab_labels
  cols_ <- grep("_Pred1$", names(.tab), value = TRUE)
  .tab |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(cols_), \(.x) mean(!is.na(.x) & .x != MC_NONE)),
      .by = Task
    ) |>
    tidyr::pivot_longer(cols = dplyr::all_of(cols_), names_to = "Arm", values_to = "Commit") |>
    dplyr::mutate(Arm = sub("_Pred1$", "", .data$Arm))
}

#' Distribution of the transformer's decision margin, by task
#'
#' The margin is the whole confidence story available without labels. A mass piled at one is a model
#' separating its categories cleanly; weight near zero is documents where a different seed would have
#' produced a different released label, and their share is the honest answer to how much of the
#' corpus is a coin toss.
#'
#' @param .tab Output of a corpus pass.
#' @return A ggplot.
mc_plot_margin <- function(.tab) {
  if (FALSE) .tab <- tab_labels
  .tab |>
    dplyr::filter(!is.na(.data$Bert_Score2)) |>
    dplyr::mutate(Margin = .data$Bert_Score1 - .data$Bert_Score2) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Margin)) +
    ggplot2::geom_histogram(bins = 50L, boundary = 0, fill = .plot_ink) +
    ggplot2::facet_wrap(facets = ggplot2::vars(.data$Task), ncol = 1L, scales = "free_y",
                        drop = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.02))) +
    plot_scale_y_count() +
    ggplot2::labs(x = "First choice minus runner-up", y = "Documents") +
    plot_theme(.grid = "y")
}

#' Where each category's mass goes when it is not first
#'
#' Rows are the first choice, columns the runner-up, shaded by the share of that category's documents
#' -- so it reads like a confusion matrix built without any labels at all. Fixed at zero to one so
#' three tasks can be compared, and every level kept whether or not it has data: a category that never
#' appears as a runner-up is a finding, and dropping it would present that finding as absence.
#'
#' @param .tab Output of a corpus pass.
#' @param .task Task to plot.
#' @return A ggplot.
mc_plot_second_guess <- function(.tab, .task) {
  if (FALSE) {
    .tab  <- tab_labels
    .task <- "ClassDetailed"
  }
  d_ <- .tab |>
    dplyr::filter(.data$Task == .task, !is.na(.data$Bert_Pred2))
  lv_ <- sort(unique(c(d_$Bert_Pred1, d_$Bert_Pred2)))

  d_ |>
    dplyr::rename(First = "Bert_Pred1", Second = "Bert_Pred2") |>
    dplyr::summarise(n = dplyr::n(), .by = c(First, Second)) |>
    dplyr::mutate(Share = .data$n / sum(.data$n), .by = First) |>
    dplyr::mutate(
      First  = factor(.data$First, levels = lv_),
      Second = factor(.data$Second, levels = lv_)
    ) |>
    plot_heatmap(
      .x       = "Second",       # runner-up
      .y       = "First",        # first choice
      .fill    = "Share",        # share of that category's documents
      .pct     = TRUE,           # shares read as percentages
      .limits  = c(0, 1)         # fixed, so three tasks are comparable
    )
}

#' Pairwise agreement across the corpus
#' @param .tab Output of a corpus pass.
#' @return A ggplot.
mc_plot_agreement <- function(.tab) {
  if (FALSE) .tab <- tab_labels
  mc_agreement(.tab = .tab) |>
    dplyr::filter(!is.na(.data$Agree)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Agree, y = .data$Pair)) +
    ggplot2::geom_col(fill = .plot_ink, width = 0.7) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = tbl_pct(.data$Agree)),
      hjust   = -0.18,
      size    = (.plot_base - 3) / ggplot2::.pt,
      family  = .plot_font
    ) +
    ggplot2::facet_wrap(facets = ggplot2::vars(.data$Task), ncol = 1L, drop = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = "Agreement where both arms committed", y = NULL) +
    plot_theme()
}

#' Commitment share across the corpus
#' @param .tab Output of a corpus pass.
#' @return A ggplot.
mc_plot_commitment <- function(.tab) {
  if (FALSE) .tab <- tab_labels
  mc_commitment(.tab = .tab) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Commit, y = .data$Arm)) +
    ggplot2::geom_col(fill = .plot_ink, width = 0.7) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = tbl_pct(.data$Commit)),
      hjust   = -0.18,
      size    = (.plot_base - 3) / ggplot2::.pt,
      family  = .plot_font
    ) +
    ggplot2::facet_wrap(facets = ggplot2::vars(.data$Task), ncol = 1L, drop = FALSE) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = "Share of documents committed to", y = NULL) +
    plot_theme()
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
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), tbl_pct)) |>
    tbl_say()

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
    tbl_say(.title = "Distance from the labelled sample's composition")
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
