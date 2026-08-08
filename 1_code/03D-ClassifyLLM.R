# 03D-ClassifyLLM: zero-shot and few-shot contract classification with a local LLM (llm_*) ----
#
# WHAT THIS STAGE IS
# A third classification arm, estimated on the same documents and the same folds as the transformer
# (03B) and the keyword table (03C), and written into the SAME run-folder schema so the shared
# leaderboard ranks it beside them and the orchestrator can route to it without special-casing.
#
# It exists because a reader of this paper will ask why a fine-tuned encoder was necessary when an
# open-weight instruction model can be pointed at a contract and asked. The answer should be a number
# on identical folds, not an assertion.
#
# THE CONTAMINATION THIS FILE IS BUILT TO AVOID
# A prompt is not a fixed object. It is written by a person, and a person who iterates it against the
# documents the transformer got wrong has fitted the prompt to the labels as surely as gradient
# descent would have -- more efficiently, in fact, because errors are concentrated exactly where the
# taxonomy is ambiguous. Reporting such a prompt's score on the whole sample is leakage, and the
# router downstream cannot detect it: nested selection protects the choice of ROUTING RULE, not the
# provenance of an arm.
#
# So arms are separated by what their author was allowed to see, and the separation is carried in the
# data rather than in a promise:
#
#   Tier    Development                                   Folds       Status
#   blind   Written from the taxonomy alone. No           1-5         enters the nested search
#           prediction, no error list, no document read
#           with its label.
#   tuned   Iterated against folds 1-4, including their   5 only      bounded check
#           errors.
#
# A blind prompt has no training of any kind, so every fold is held out and one pass covers the
# sample. A tuned prompt has seen four folds, so the fifth is its only honest test -- the identical
# discipline 03C applies to the generated keyword list, for the identical reason.
#
# FOLDS FOR A MODEL THAT DOES NOT TRAIN
# A blind arm's per-fold metrics are a partition of one pass, not five independent estimates. They
# are still worth writing: they give the fold-to-fold spread that every comparison in this study is
# expressed in, and they make the run schema identical to the trained arms'. They are NOT a basis for
# selecting among prompts, which is why prompt selection is confined to the tuned tier and reported
# as such.
#
# ABSTENTION IS A SWEPT AXIS, NOT A DETAIL
# A model allowed to answer "unsure" becomes an ABSTAINING arm, which is what the orchestrator's
# cascade is built around: it can commit where it is confident and leave the rest to the transformer.
# A model forced to choose is a terminal arm competing head-on. These are different objects with
# different uses, so both are estimated and the difference is reported.
#
# COST AND IDEMPOTENCE
# One local generation per document is seconds, and the sample is 4,400 documents, so a single cell
# is hours. Every answer is therefore cached on a hash of everything that determines it -- document,
# prompt, model, window, schema -- and a cell already complete is skipped. The first render is long;
# every later one is seconds and reproduces identical output. That is what makes an always-executing
# document affordable here, and it is the same bargain 03B and 03C strike with their sweeps.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new; if (FALSE) dev blocks; cli/fs/here;
# pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .tab_prep  <- arrow::read_parquet(.lP$Input$Prepared)
  .label_col <- "ClassDetailed"
  .model     <- "qwen3:32b"
}

# Shared with 03C and 03D: the abstention sentinel the scoring layer already understands.
LLM_NONE <- "(none)"

# The token the model is told to emit when it declines. Kept distinct from the sentinel so a genuine
# category could never be confused with a refusal, and mapped to the sentinel on the way in.
LLM_UNSURE <- "UNSURE"


# 1. The taxonomy the model is shown -------------------------------------------------------------
# The label set comes from the data rather than from a constant, so a taxonomy revision cannot leave
# the prompt describing categories that no longer exist. Definitions are optional and supplied by the
# document, because what the model was told is exactly the sort of thing a referee wants to read.

#' The label set for one task, in a fixed order
#'
#' Alphabetical rather than by frequency. Ordering by frequency would leak the class prior into the
#' prompt, which is a small amount of training the blind tier is not entitled to.
#'
#' @param .tab_prep Prepared sample from 03A.
#' @param .label_col Task column.
#' @return Character vector of category names.
llm_labels <- function(.tab_prep, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab_prep  <- tab_prep
    .label_col <- "ClassDetailed"
  }
  sort(unique(stats::na.omit(.tab_prep[[.label_col]])))
}

#' Render the category list for a prompt, with definitions where supplied
#'
#' Two guidance levels are meaningful and both are swept. "labels" shows category names only and is
#' the honest floor: it measures what the model knows about contracts with no help. "codebook" adds
#' one line per category and measures what the taxonomy documentation is worth, which is a number
#' worth having, since writing that documentation is the main human cost of adopting this scheme.
#'
#' @param .labels Character vector of category names.
#' @param .definitions Optional tibble (Label, Definition), or NULL.
#' @param .guidance "labels" or "codebook".
#' @return Character scalar, one category per line.
llm_render_labels <- function(.labels, .definitions = NULL, .guidance = c("labels", "codebook")) {
  if (FALSE) {
    .labels      <- llm_labels(tab_prep, "ClassDetailed")
    .definitions <- tab_definitions
    .guidance    <- "codebook"
  }
  .guidance <- match.arg(.guidance)
  if (.guidance == "labels" || is.null(.definitions)) {
    return(paste0("- ", .labels, collapse = "\n"))
  }
  map_ <- stats::setNames(.definitions$Definition, .definitions$Label)
  def_ <- unname(map_[.labels])
  paste0("- ", .labels, dplyr::if_else(is.na(def_), "", paste0(": ", def_)), collapse = "\n")
}


# 2. Prompts ---------------------------------------------------------------------------------------
# A prompt is a swept level, so it is built by a function from named parts rather than pasted at the
# call site. Every part that varies is an argument, and the assembled text is stored with the run, so
# a result can always be traced to the exact words that produced it.

#' Assemble the classification prompt for one document
#'
#' Deliberately plain. Instruction-tuned models respond to elaborate persona framing in ways that are
#' unstable across model families, and this stage compares across families, so anything that helps
#' one and hurts another would be measuring the prompt rather than the model.
#'
#' The excerpt is taken from the opening of the document. 03B established that context length beyond
#' the first few hundred tokens does not improve the transformer, and 03C's window sweep found no
#' signal deeper in the contract either, so a header window is the like-for-like input rather than a
#' concession to cost.
#'
#' @param .text Document text.
#' @param .labels_block Rendered category list from llm_render_labels().
#' @param .task_line One sentence naming what is being decided.
#' @param .examples Optional rendered few-shot block, or NULL.
#' @param .allow_abstain Logical. Offer the model a way to decline.
#' @param .n_chars Header window in characters.
#' @return Character scalar prompt.
llm_prompt <- function(.text, .labels_block, .task_line, .examples = NULL,
                       .allow_abstain = FALSE, .n_chars = 6000L) {
  if (FALSE) {
    .text          <- tab_prep$Text[[1]]
    .labels_block  <- llm_render_labels(llm_labels(tab_prep, "ClassDetailed"))
    .task_line     <- "Classify this contract into exactly one category."
    .examples      <- NULL
    .allow_abstain <- TRUE
    .n_chars       <- 6000L
  }
  # stri_sub, never substr: the corpus is not pure ASCII and byte slicing would cut mid-character.
  excerpt_ <- stringi::stri_sub(.text %||% "", 1L, .n_chars)

  abstain_ <- if (.allow_abstain) {
    paste0("If the excerpt does not let you decide with confidence, answer \"", LLM_UNSURE,
           "\" rather than guessing.")
  } else {
    "You must choose one category even if the excerpt is ambiguous."
  }

  paste0(
    .task_line, "\n\n",
    "Categories:\n", .labels_block, "\n\n",
    abstain_, "\n",
    "Answer only with JSON: {\"label\": \"<one of the exact category strings above>\"}\n",
    if (is.null(.examples)) "" else paste0("\nWorked examples:\n", .examples, "\n"),
    "\nContract excerpt:\n", excerpt_
  )
}

#' Render a few-shot example block from labelled documents
#'
#' Examples are drawn only from folds the tier is entitled to read, which is enforced by the caller
#' rather than here, and the excerpt per example is short: a handful of long examples crowds out the
#' document being classified in the context window and measures truncation instead of guidance.
#'
#' @param .tab Documents to use as examples (Text plus the label column).
#' @param .label_col Task column.
#' @param .n_chars Characters of each example to show.
#' @return Character scalar, or NULL where .tab is empty.
llm_render_examples <- function(.tab, .label_col = "ClassDetailed", .n_chars = 700L) {
  if (FALSE) {
    .tab       <- llm_examples(tab_prep, "ClassDetailed", 1:4, 1L)
    .label_col <- "ClassDetailed"
    .n_chars   <- 700L
  }
  if (is.null(.tab) || nrow(.tab) == 0L) return(NULL)
  paste0(
    "Excerpt: ", stringi::stri_sub(.tab$Text, 1L, .n_chars),
    "\nCategory: ", .tab[[.label_col]],
    collapse = "\n\n"
  )
}

#' Draw few-shot examples, stratified by category, from permitted folds only
#'
#' One example per category by default, which keeps the prompt balanced. Drawing by frequency would
#' hand the model the class prior, and the prior is the single easiest thing to exploit on a sample
#' this imbalanced.
#'
#' @param .tab_prep Prepared sample.
#' @param .label_col Task column.
#' @param .folds Folds the tier may read.
#' @param .per_class Examples per category.
#' @param .seed Fixed so the draw is reproducible.
#' @return Tibble of example documents.
llm_examples <- function(.tab_prep, .label_col = "ClassDetailed", .folds = 1:4, .per_class = 1L,
                         .seed = 42L) {
  if (FALSE) {
    .tab_prep  <- tab_prep
    .label_col <- "ClassDetailed"
    .folds     <- 1:4
    .per_class <- 1L
    .seed      <- 42L
  }
  withr::with_seed(.seed, {
    .tab_prep |>
      dplyr::filter(.data$Fold %in% .folds, !is.na(.data[[.label_col]])) |>
      dplyr::slice_sample(n = .per_class, by = dplyr::all_of(.label_col)) |>
      dplyr::arrange(.data[[.label_col]])
  })
}


# 3. Transport -------------------------------------------------------------------------------------
# One request, one answer, no retries beyond the transport's own. Caching is the caller's job, which
# keeps this function testable without a filesystem and keeps the cache key in one place.

#' Models the local Ollama server currently holds
#'
#' @param .host Ollama host.
#' @param .timeout Seconds before the request is abandoned.
#' @return Character vector of model tags, or NULL where the server is unreachable.
llm_available_models <- function(.host = "http://localhost:11434", .timeout = 20) {
  if (FALSE) .host <- "http://localhost:11434"
  tryCatch(
    httr2::request(.host) |>
      httr2::req_url_path("/api/tags") |>
      httr2::req_timeout(.timeout) |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      (\(.r) purrr::map_chr(.r$models, "model"))(),
    error = function(e) NULL
  )
}

#' Fail before the sweep rather than during it
#'
#' A missing model returns HTTP 404 on every request, which the classifier treats as a declined
#' answer -- so an unpulled tag does not error, it produces a configuration with zero coverage after
#' several thousand futile calls. Checking once, here, converts hours of silent failure into one line
#' naming the command that fixes it. This is the same principle 03A applies by testing its input path
#' at the point the path is declared.
#'
#' @param .models Model tags the sweep intends to use.
#' @param .host Ollama host.
#' @return Invisibly the available model tags.
llm_check_models <- function(.models, .host = "http://localhost:11434") {
  if (FALSE) {
    .models <- c("qwen3:8b", "qwen3:32b")
    .host   <- "http://localhost:11434"
  }
  have_ <- llm_available_models(.host = .host)
  if (is.null(have_)) {
    cli::cli_abort(c(
      "No Ollama server answering at {(.host)}.",
      "i" = "Start it with {.code ollama serve}, then re-run this document."
    ))
  }
  miss_ <- setdiff(.models, have_)
  if (length(miss_) > 0L) {
    cli::cli_abort(c(
      "Model{?s} not present on this server: {miss_}",
      "i" = "Pull {?it/them} with {.code ollama pull {miss_}}, or drop {?it/them} from the grid.",
      "i" = "Available: {have_}"
    ))
  }
  cli::cli_alert_success("All {length(.models)} requested model{?s} present: {(.models)}")
  invisible(have_)
}

#' Draw a preview sample that still contains every category
#'
#' Stratified on the label rather than taken from the top or drawn at random. A preview exists to show
#' what a configuration does, and a uniform draw from an imbalanced sample gives the thinnest
#' categories one or two documents each -- so exactly the categories whose behaviour is in question
#' arrive with per-class scores too noisy to read, or absent entirely.
#'
#' Stratifying on the label rather than on (fold, label) is deliberate. A preview is not an estimate,
#' so fold balance buys nothing, while category coverage is the whole point.
#'
#' @param .tab Documents to sample from.
#' @param .label_col Task column to stratify on.
#' @param .n Documents wanted.
#' @param .seed Fixed, so the same preview is drawn on every render.
#' @return Tibble of at most .n documents.
llm_sample <- function(.tab, .label_col, .n, .seed = 42L) {
  if (FALSE) {
    .tab       <- tab_prep
    .label_col <- "ClassDetailed"
    .n         <- 100L
    .seed      <- 42L
  }
  if (is.null(.n) || .n >= nrow(.tab)) return(.tab)
  per_ <- ceiling(.n / dplyr::n_distinct(.tab[[.label_col]]))

  out_ <- withr::with_seed(.seed, {
    # Two passes, and the row count between them has to be computed in plain R: the n argument of
    # slice_sample is an ordinary value, not a masked expression, so dplyr::n() is unavailable there.
    pool_ <- dplyr::slice_sample(.tab, n = per_, by = dplyr::all_of(.label_col))
    dplyr::slice_sample(pool_, n = min(.n, nrow(pool_)))
  })

  n_all_  <- dplyr::n_distinct(.tab[[.label_col]])
  n_kept_ <- dplyr::n_distinct(out_[[.label_col]])
  cli::cli_alert_info(
    "Preview: {nrow(out_)} of {nrow(.tab)} document{?s}, covering {n_kept_} of {n_all_} categories."
  )
  if (n_kept_ < n_all_) {
    cli::cli_alert_warning(
      "The trim to {(.n)} dropped {n_all_ - n_kept_} categor{?y/ies} entirely. Raise the cap, or \\
       read the per-class table knowing those rows are missing rather than zero."
    )
  }
  out_
}

#' Where a run belongs, given whether it was limited
#'
#' A limited run and a full run of the same configuration are not the same object, and binding them
#' into one leaderboard would average a hundred documents against four thousand without reporting
#' that it had done so. Separating them physically is the only guarantee that cannot be defeated by a
#' forgotten filter, and it follows the rule this project already applies to superseded outputs: a
#' sibling directory is invisible to code that globs a known root, and available to anyone who looks.
#'
#' @param .runs_root Base runs directory.
#' @param .limit Documents the run was capped at, or NULL for a full run.
#' @return Character path.
llm_runs_root <- function(.runs_root, .limit = NULL) {
  if (FALSE) {
    .runs_root <- .lP$Runs$Llm
    .limit     <- 100L
  }
  if (is.null(.limit)) .runs_root else paste0(.runs_root, "_preview")
}

#' JSON schema constraining the answer to the permitted label set
#'
#' The enum is what makes the arm scoreable without post-hoc string matching: the model cannot invent
#' a thirteenth category, so a parse failure is a transport problem rather than a taxonomy problem
#' and the two never have to be disentangled after the fact.
#'
#' @param .labels Permitted category names.
#' @param .allow_abstain Logical. Add the refusal token to the enum.
#' @return A list in Ollama's format schema shape.
llm_schema <- function(.labels, .allow_abstain = FALSE) {
  if (FALSE) {
    .labels        <- llm_labels(tab_prep, "ClassDetailed")
    .allow_abstain <- TRUE
  }
  enum_ <- if (.allow_abstain) c(.labels, LLM_UNSURE) else .labels
  list(
    type       = "object",
    properties = list(label = list(type = "string", enum = as.list(enum_))),
    required   = list("label")
  )
}

#' Call the local model once, constrained to the label enum
#'
#' Temperature zero and a fixed seed, because a classifier that returns a different answer on a second
#' pass cannot be cached, cannot be reproduced, and cannot be compared against a deterministic
#' baseline. Returns NA on any transport or parse failure so the caller can tally misses rather than
#' aborting a run that is hours long.
#'
#' @param .prompt Assembled prompt.
#' @param .labels Permitted category names.
#' @param .model Ollama model tag.
#' @param .allow_abstain Logical. Whether the refusal token is permitted.
#' @param .num_ctx Context window in tokens.
#' @param .think Logical, or NULL to leave the server's default alone. Reasoning-capable models
#'   enable a chain-of-thought pass unless told otherwise, and on a task whose answer is one token
#'   drawn from a fixed enum that trace is the whole cost: hundreds of generated tokens produced and
#'   discarded per document. FALSE turns it off. It is an argument rather than a constant because
#'   whether reasoning helps classification is a fair question, just not one worth paying for before
#'   the baseline exists.
#' @param .host Ollama host.
#' @param .timeout Seconds before the request is abandoned.
#' @return Character scalar label, LLM_UNSURE, or NA_character_.
llm_call <- function(.prompt, .labels, .model = "qwen3:32b", .allow_abstain = FALSE,
                     .num_ctx = 8192L, .think = FALSE, .host = "http://localhost:11434",
                     .timeout = 600) {
  if (FALSE) {
    .prompt        <- "Classify ..."
    .labels        <- llm_labels(tab_prep, "ClassDetailed")
    .model         <- "qwen3:32b"
    .allow_abstain <- TRUE
    .think         <- FALSE
  }
  body_ <- list(
    model    = .model,
    messages = list(list(role = "user", content = .prompt)),
    stream   = FALSE,
    options  = list(temperature = 0, num_ctx = .num_ctx, seed = 1L),
    format   = llm_schema(.labels = .labels, .allow_abstain = .allow_abstain)
  )
  # think is a TOP-LEVEL request field, not an option. Placed inside options it is accepted and
  # ignored, which is the worst available failure: the reasoning pass runs, the wall clock says so,
  # and nothing reports that the setting did nothing. NULL omits the field entirely, which is the
  # escape hatch for a model that rejects it.
  if (!is.null(.think)) body_$think <- .think
  txt_ <- tryCatch(
    httr2::request(.host) |>
      httr2::req_url_path("/api/chat") |>
      httr2::req_body_json(body_, auto_unbox = TRUE) |>
      httr2::req_timeout(.timeout) |>
      # Ollama returns its reason in the BODY, not the status line: a 404 whose body reads
      # "model not found" is a different problem from a 404 at a wrong path, and the status
      # alone cannot tell them apart.
      httr2::req_error(body = function(.resp) httr2::resp_body_string(.resp)) |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      (\(.r) .r$message$content)(),
    error = function(e) {
      cli::cli_alert_warning("Ollama call failed: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(txt_) || !nzchar(txt_)) return(NA_character_)

  # Reasoning models leak a trace even under a format schema; strip it, then take the first object.
  clean_ <- txt_ |>
    stringr::str_remove_all("(?s)<think>.*?</think>") |>
    stringr::str_remove_all("```json|```")
  json_ <- stringi::stri_extract_first_regex(clean_, "\\{.*\\}")
  if (is.na(json_)) return(NA_character_)

  lab_ <- tryCatch(jsonlite::fromJSON(json_)$label, error = function(e) NULL)
  if (is.null(lab_) || length(lab_) != 1L) return(NA_character_)
  if (!lab_ %in% c(.labels, LLM_UNSURE)) return(NA_character_)
  as.character(lab_)
}


# 4. One configuration over a document set ---------------------------------------------------------

#' Canonical configuration name
#'
#' Encodes every axis that varied, so the shared leaderboard -- which keys on this string -- ranks LLM
#' configurations beside transformer and keyword ones without knowing their axes differ. The prompt
#' enters as a hash, because two prompts differing in one clause must not share a name and no readable
#' encoding of a paragraph exists.
#'
#' @param .label_col,.model,.tier,.guidance,.shots,.n_chars,.allow_abstain Swept axes.
#' @param .prompt_hash Short hash of the assembled prompt template.
#' @param .limit Documents the run was capped at, or NULL. A capped run carries the cap in its name,
#'   so its metrics can never be pooled with a full run's under one identifier.
#' @param .think Reasoning regime the run used. In the name for the same reason the cap is: two runs
#'   differing only in whether the model reasoned first are different runs and must not share a
#'   folder.
#' @param .seed Stamped for parity.
#' @return Character scalar.
llm_config_name <- function(.label_col, .model, .tier, .guidance, .shots, .n_chars,
                            .allow_abstain, .prompt_hash, .limit = NULL, .think = FALSE,
                            .seed = 42L) {
  if (FALSE) {
    .label_col     <- "ClassDetailed"
    .model         <- "qwen3:32b"
    .tier          <- "blind"
    .guidance      <- "labels"
    .shots         <- 0L
    .n_chars       <- 6000L
    .allow_abstain <- TRUE
    .prompt_hash   <- "a1b2c3"
    .limit         <- 100L
    .think         <- FALSE
    .seed          <- 42L
  }
  paste0(
    .label_col, "__llm-", gsub("[^A-Za-z0-9]", "", .model), "__",
    toupper(substr(.tier, 1L, 1L)),
    "_G", substr(.guidance, 1L, 3L),
    "_S", .shots,
    "_C", .n_chars,
    "_A", as.integer(.allow_abstain),
    "_R", if (is.null(.think)) "d" else as.integer(.think),
    "_X", .prompt_hash,
    if (is.null(.limit)) "" else paste0("_L", .limit),
    "_S", .seed
  )
}

#' Classify a set of documents under one configuration, cached and resumable
#'
#' Every answer is stored under a hash of everything that determines it, so an interrupted pass
#' resumes for free and a re-render costs a directory listing. The key includes the prompt text
#' itself: change one clause and the cache misses, which is the behaviour a name-keyed cache fails to
#' provide and the reason this project treats name-keyed caches as a trap.
#'
#' @param .tab Documents to classify (DocID, Text, Fold, plus the label column).
#' @param .label_col Task column.
#' @param .labels Permitted category names.
#' @param .labels_block Rendered category list.
#' @param .task_line One sentence naming the decision.
#' @param .examples Rendered few-shot block, or NULL.
#' @param .model Ollama model tag.
#' @param .allow_abstain Logical.
#' @param .n_chars Header window in characters.
#' @param .cache_dir Directory for per-answer caches.
#' @param .think Logical, or NULL. Part of the cache key: an answer produced with a reasoning pass
#'   and one produced without are different answers to the same prompt, so a key omitting it would
#'   serve the old regime's answers after the setting changed and report a speed-up that was really
#'   a cache hit.
#' @param .num_ctx,.host,.timeout Transport settings.
#' @param .overwrite Logical. Ignore the cache and re-ask every document.
#' @param .max_consecutive_fail Abort after this many failures in a row.
#' @return Tibble: DocID, Fold, TrueLabel, PredLabel, Score, Raw.
llm_classify <- function(.tab, .label_col, .labels, .labels_block, .task_line, .examples = NULL,
                         .model = "qwen3:32b", .allow_abstain = FALSE, .n_chars = 6000L,
                         .cache_dir = here::here("2_output", "03D-ClassifyLLM", "cache"),
                         .think = FALSE, .num_ctx = 8192L, .host = "http://localhost:11434",
                         .timeout = 600, .overwrite = FALSE, .max_consecutive_fail = 5L) {
  if (FALSE) {
    .tab          <- dplyr::slice_head(tab_prep, n = 5L)
    .label_col    <- "ClassDetailed"
    .labels       <- llm_labels(tab_prep, "ClassDetailed")
    .labels_block <- llm_render_labels(.labels)
    .task_line    <- "Classify this contract into exactly one category."
    .examples     <- NULL
    .model        <- "qwen3:32b"
  }
  fs::dir_create(.cache_dir)
  n_ <- nrow(.tab)
  if (n_ == 0L) cli::cli_abort("No documents to classify.")
  cli::cli_alert_info("Classifying {n_} document{?s} with {(.model)}")

  cli::cli_progress_bar(
    format = paste0("{cli::pb_spin} {cli::pb_current}/{cli::pb_total} {cli::pb_bar} ",
                    "{cli::pb_percent} | ETA {cli::pb_eta}"),
    total = n_, clear = FALSE
  )
  out_   <- vector("list", n_)
  fail_  <- 0L
  for (i_ in seq_len(n_)) {
    row_    <- .tab[i_, ]
    prompt_ <- llm_prompt(
      .text          = row_$Text,
      .labels_block  = .labels_block,
      .task_line     = .task_line,
      .examples      = .examples,
      .allow_abstain = .allow_abstain,
      .n_chars       = .n_chars
    )
    key_  <- rlang::hash(list(row_$DocID, prompt_, .model, .allow_abstain, .num_ctx, .think))
    path_ <- fs::path(.cache_dir, paste0(key_, ".rds"))

    lab_ <- if (!.overwrite && fs::file_exists(path_)) {
      readRDS(path_)
    } else {
      l_ <- llm_call(
        .prompt        = prompt_,
        .labels        = .labels,
        .model         = .model,
        .allow_abstain = .allow_abstain,
        .num_ctx       = .num_ctx,
        .think         = .think,
        .host          = .host,
        .timeout       = .timeout
      )
      # A failure is NOT cached. Caching one makes a transient outage permanent: the server comes
      # back, the document is never re-asked, and the configuration carries a hole that no later run
      # can fill and nothing reports. Only an answer the model actually gave is worth keeping.
      if (!is.na(l_)) saveRDS(l_, path_)
      l_
    }

    fail_ <- if (is.na(lab_)) fail_ + 1L else 0L
    if (fail_ >= .max_consecutive_fail) {
      cli::cli_progress_done()
      cli::cli_abort(c(
        "{fail_} consecutive failures at document {i_} of {n_}; stopping.",
        "i" = "Check that {(.model)} is pulled and the server is up, then re-run.",
        "i" = "Answers already received are cached, so the pass resumes where it stopped."
      ))
    }

    out_[[i_]] <- tibble::tibble(DocID = row_$DocID, Raw = lab_)
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  res_ <- purrr::list_rbind(out_)
  n_na_ <- sum(is.na(res_$Raw))
  if (n_na_ > 0L) {
    cli::cli_alert_warning(
      "{n_na_} document{?s} returned no usable answer; they are scored as abstentions, which \\
       depresses coverage rather than accuracy."
    )
  }

  .tab |>
    dplyr::select(DocID, Fold, TrueLabel = dplyr::all_of(.label_col)) |>
    dplyr::left_join(res_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      # A refusal, a failure and an unparseable answer are all "no commitment" to the scoring layer.
      PredLabel = dplyr::if_else(is.na(.data$Raw) | .data$Raw == LLM_UNSURE, LLM_NONE, .data$Raw),
      # No usable confidence is available from a constrained single-token answer, so Score is the
      # commitment indicator. A floor above zero in the router therefore does nothing here, which is
      # honest: this arm has no confidence signal to threshold.
      Score = as.numeric(.data$PredLabel != LLM_NONE)
    )
}


# 5. Run folders in the shared schema --------------------------------------------------------------

#' Write one LLM run in the schema 03B and 03C use
#'
#' Writing the same files under the same names is what makes this arm a first-class member of the
#' study rather than a side experiment: clf_load_overall binds it, the leaderboard ranks it, and the
#' orchestrator inventories it with no branch anywhere for "this one is an LLM".
#'
#' @param .runs_root Runs directory.
#' @param .config_name,.run_name Identifiers.
#' @param .pred Per-document predictions from llm_classify().
#' @param .spec Named list of the configuration's axes, carried into config.json.
#' @param .prompt_example One assembled prompt, stored verbatim as provenance.
#' @param .fold Held-out fold this run reports.
#' @param .seconds Wall-clock seconds the pass took.
#' @return Run directory, invisibly.
llm_write_run <- function(.runs_root, .config_name, .run_name, .pred, .spec, .prompt_example,
                          .fold, .seconds) {
  if (FALSE) {
    .runs_root   <- .lP$Output$Runs
    .config_name <- "cfg"
    .run_name    <- "cfg_F1"
    .pred        <- pred_
    .fold        <- 1L
    .seconds     <- 120
  }
  dir_ <- fs::path(.runs_root, .run_name)
  if (nrow(.pred) == 0L) {
    cli::cli_abort(
      "No documents for fold {(.fold)}; a run written from an empty set scores nothing and its \\
       per-class table has no columns for the scoring layer to sort on."
    )
  }
  fs::dir_create(dir_)

  sc_  <- clf_scores(.tab_pred = .pred, .none = LLM_NONE)
  pc_  <- clf_perclass(.tab_pred = .pred, .none = LLM_NONE)
  hit_ <- .pred$PredLabel != LLM_NONE

  .pred |>
    dplyr::mutate(ConfigName = .config_name, Run = .run_name, Fold = as.integer(.data$Fold)) |>
    dplyr::select(ConfigName, Run, DocID, TrueLabel, PredLabel, Score, Fold) |>
    arrow::write_parquet(fs::path(dir_, "predictions.parquet"))

  overall_ <- tibble::tibble(
    ConfigName   = .config_name,
    Run          = .run_name,
    RunName      = .run_name,
    Model        = paste0("llm-", .spec$model),
    LabelCol     = .spec$label_col,
    TextCol      = "Text",
    ClassWeights = FALSE,
    TestFold     = as.integer(.fold),
    MaxLen       = NA_real_,
    Epochs       = NA_real_,
    BatchSize    = NA_real_,
    LR           = NA_real_,
    Seed         = as.integer(.spec$seed),
    Device       = "ollama",
    nTrain       = 0L,                     # nothing is trained; the column exists for the schema
    nTest        = nrow(.pred),
    nClasses     = length(.spec$labels),
    Accuracy     = sc_$Accuracy,
    F1_macro     = sc_$F1_macro,
    F1_weighted  = sc_$F1_weighted,
    DurationSec  = round(.seconds, 2),
    Smoke        = FALSE,
    Source       = "text",
    Tier         = .spec$tier,
    Guidance     = .spec$guidance,
    Shots        = as.integer(.spec$shots),
    NChars       = as.integer(.spec$n_chars),
    AllowAbstain = .spec$allow_abstain,
    Think        = if (is.null(.spec$think)) NA else as.logical(.spec$think),
    PromptHash   = .spec$prompt_hash,
    Limit        = if (is.null(.spec$limit)) NA_integer_ else as.integer(.spec$limit),
    Coverage     = sc_$Coverage,
    SelPrecision = if (any(hit_)) mean(.pred$PredLabel[hit_] == .pred$TrueLabel[hit_]) else NA_real_
  )
  arrow::write_parquet(overall_, fs::path(dir_, "metrics_overall.parquet"))

  pc_ |>
    dplyr::mutate(ConfigName = .config_name, Run = .run_name) |>
    arrow::write_parquet(fs::path(dir_, "metrics_perclass.parquet"))

  jsonlite::write_json(
    c(.spec, list(run_name = .run_name, config_name = .config_name, fold = .fold,
                  authored_by = "03D llm_write_run", prompt_example = .prompt_example)),
    fs::path(dir_, "config.json"), auto_unbox = TRUE, pretty = TRUE
  )
  invisible(dir_)
}


# 6. The sweep -------------------------------------------------------------------------------------

#' Build the configuration grid
#'
#' Tier is not crossed with the rest, because the tiers cover different amounts of the sample and so
#' cost different amounts to run. Everything else is crossed, because the point of a sweep is a
#' balanced comparison and a targeted design cannot deliver one.
#'
#' Three tiers exist, separated by what their prompt was allowed to see:
#'
#'   blind      No examples at all. Nothing about this sample enters the prompt, so every fold is
#'              held out and one pass covers the corpus.
#'   crossfold  Worked examples drawn from the COMPLEMENT of the fold being classified, redrawn for
#'              each fold in turn. No document's label ever appears in the prompt that classifies it,
#'              which is the discipline the transformer's own training follows, so this tier also
#'              earns all five folds -- and unlike a single-fold arm it can enter the router's nested
#'              search and vote on the confidence flag.
#'   tuned      Anything developed with folds one to four in view, including a prompt iterated
#'              against observed errors. The held-out fold is its only honest test.
#'
#' @param .label_cols Tasks to run.
#' @param .models Ollama model tags.
#' @param .guidance Guidance levels ("labels", "codebook").
#' @param .shots Few-shot example counts per category.
#' @param .n_chars Header windows in characters.
#' @param .allow_abstain Logical levels.
#' @param .think Reasoning regimes to cross in. One level by default: the reasoning pass multiplies
#'   generated tokens per document, so sweeping it before a baseline exists spends the whole budget
#'   on a question that is only interesting once the cheap answer is known.
#' @param .tier "blind", "crossfold" or "tuned".
#' @return Tibble, one row per configuration.
llm_grid <- function(.label_cols = "ClassDetailed", .models = "qwen3:32b",
                     .guidance = c("labels", "codebook"), .shots = 0L, .n_chars = 6000L,
                     .allow_abstain = c(FALSE, TRUE), .think = FALSE, .tier = "blind") {
  if (FALSE) {
    .label_cols    <- "ClassDetailed"
    .models        <- c("qwen3:8b", "qwen3:32b")
    .guidance      <- c("labels", "codebook")
    .shots         <- 0L
    .n_chars       <- 6000L
    .allow_abstain <- c(FALSE, TRUE)
    .think         <- FALSE
    .tier          <- "blind"
  }
  tidyr::expand_grid(
    LabelCol     = .label_cols,
    Model        = .models,
    Tier         = .tier,
    Guidance     = .guidance,
    Shots        = as.integer(.shots),
    NChars       = as.integer(.n_chars),
    AllowAbstain = .allow_abstain,
    Think        = .think
  ) |>
    # A few-shot prompt with zero examples is the zero-shot prompt; keep one of them. And a tier
    # defined by its examples with none to show is the blind tier under another name.
    dplyr::filter(
      !(.data$Tier == "blind" & .data$Shots > 0L),
      !(.data$Tier != "blind" & .data$Shots == 0L)
    ) |>
    dplyr::mutate(CellID = dplyr::row_number(), .before = 1L)
}

#' Run one cell of the grid on the folds its tier is entitled to
#'
#' Blind configurations classify every document once and write one run per fold, partitioning that
#' single pass. Tuned configurations classify the holdout fold only, because their prompt was written
#' against the others. Crossfold configurations classify each fold in turn with examples drawn from
#' the complement, so the prompt facing a document never contains that document's own fold -- five
#' passes over a fifth of the corpus each, which costs the same as one blind pass and buys an arm
#' that is honest on every fold.
#'
#' @param .cell One row of llm_grid().
#' @param .tab_prep Prepared sample.
#' @param .definitions Optional definitions tibble for the codebook guidance level.
#' @param .task_lines Named character vector: task column to the sentence describing it.
#' @param .runs_root Runs directory.
#' @param .cache_dir Answer cache directory.
#' @param .folds_full Folds a full-coverage tier (blind, crossfold) covers.
#' @param .fold_holdout Fold a tuned configuration is evaluated on.
#' @param .example_folds Folds a tuned configuration may draw examples from. Crossfold ignores this
#'   and uses the complement of whichever fold it is classifying, which is the whole point of it.
#' @param .overwrite Logical. Re-ask every document.
#' @param .limit Cap on documents classified, or NULL for the whole entitled set. A capped run is
#'   written to a sibling preview root under a name carrying the cap, so it cannot be mistaken for,
#'   or pooled with, a full one.
#' @param .seed Fixed, so the preview draws the same documents on every render.
#' @param ... Transport settings passed to llm_classify().
#' @return Tibble of the runs written, invisibly.
llm_run_cell <- function(.cell, .tab_prep, .definitions = NULL, .task_lines, .runs_root, .cache_dir,
                         .folds_full = 1:5, .fold_holdout = 5L, .example_folds = 1:4,
                         .overwrite = FALSE, .limit = NULL, .seed = 42L, ...) {
  if (FALSE) {
    .cell        <- grid_blind[1, ]
    .tab_prep    <- tab_prep
    .definitions <- NULL
    .task_lines  <- .lP$TaskLines
    .runs_root   <- .lP$Runs$Llm
    .cache_dir   <- .lP$Output$Cache
    .limit       <- 100L
  }
  labels_ <- llm_labels(.tab_prep = .tab_prep, .label_col = .cell$LabelCol)
  block_  <- llm_render_labels(.labels = labels_, .definitions = .definitions,
                               .guidance = .cell$Guidance)

  cross_ <- identical(.cell$Tier, "crossfold")

  # A crossfold cell has no single example block -- it has one per fold, by design. Its examples are
  # therefore built inside the fold loop below, and NULL here.
  ex_tab_ <- if (.cell$Shots > 0L && !cross_) {
    llm_examples(.tab_prep = .tab_prep, .label_col = .cell$LabelCol, .folds = .example_folds,
                 .per_class = .cell$Shots)
  } else {
    NULL
  }
  ex_ <- llm_render_examples(.tab = ex_tab_, .label_col = .cell$LabelCol)

  # The hash covers everything a reader would call "the prompt": the categories as rendered, the
  # instruction, the examples, the window, and whether declining was permitted. The cap is NOT in it,
  # so a preview's answers are reused verbatim by the full run that follows and are never paid for
  # twice.
  #
  # Crossfold hashes the example RECIPE rather than any one rendering, because no single rendering
  # describes it: the block is redrawn per fold, and a name built from fold one's draw would claim to
  # identify a configuration four fifths of which it never saw. Shots and seed determine every draw,
  # so the recipe is the honest identifier. Blind cells pass NULL on both paths and keep the names
  # they already have on disk.
  ex_key_ <- if (cross_) list("crossfold", .cell$Shots, .seed) else ex_
  hash_ <- substr(rlang::hash(list(block_, .task_lines[[.cell$LabelCol]], ex_key_, .cell$NChars,
                                   .cell$AllowAbstain)), 1L, 6L)

  cfg_  <- llm_config_name(
    .label_col     = .cell$LabelCol,
    .model         = .cell$Model,
    .tier          = .cell$Tier,
    .guidance      = .cell$Guidance,
    .shots         = .cell$Shots,
    .n_chars       = .cell$NChars,
    .allow_abstain = .cell$AllowAbstain,
    .prompt_hash   = hash_,
    .limit         = .limit,
    .think         = .cell$Think
  )
  root_ <- llm_runs_root(.runs_root = .runs_root, .limit = .limit)

  folds_ <- if (.cell$Tier == "tuned") .fold_holdout else .folds_full
  docs_  <- .tab_prep |>
    dplyr::filter(.data$Fold %in% folds_, !is.na(.data[[.cell$LabelCol]]))
  if (!is.null(.limit)) {
    docs_ <- llm_sample(.tab = docs_, .label_col = .cell$LabelCol, .n = .limit, .seed = .seed)
  }

  # Idempotence: a cell whose runs are all present is skipped without touching the model. A capped
  # cell may leave a fold empty, so presence is judged on the folds its documents actually occupy.
  want_folds_ <- sort(unique(docs_$Fold))
  want_ <- fs::path(root_, paste0(cfg_, "_F", want_folds_))
  if (!.overwrite && length(want_) > 0L && all(fs::dir_exists(want_))) {
    cli::cli_alert_info("Skipping {cfg_}: {length(want_folds_)} run{?s} already on disk")
    return(invisible(tibble::tibble(ConfigName = cfg_, Fold = want_folds_, Written = FALSE)))
  }

  classify_ <- function(.tab, .examples) {
    llm_classify(
      .tab           = .tab,
      .label_col     = .cell$LabelCol,
      .labels        = labels_,
      .labels_block  = block_,
      .task_line     = .task_lines[[.cell$LabelCol]],
      .examples      = .examples,
      .model         = .cell$Model,
      .allow_abstain = .cell$AllowAbstain,
      .n_chars       = .cell$NChars,
      .cache_dir     = .cache_dir,
      .think         = .cell$Think,
      .overwrite     = .overwrite,
      ...
    )
  }

  t0_ <- Sys.time()
  ex_first_ <- ex_
  if (cross_) {
    # One pass per fold, each shown examples drawn from the other folds only. The examples are
    # redrawn rather than reused because reusing one fold's draw would put that fold's labelled
    # documents into the prompt facing every other fold, which is the leak this tier exists to avoid.
    pred_ <- purrr::map(sort(unique(docs_$Fold)), function(.k) {
      ex_k_ <- llm_render_examples(
        .tab = llm_examples(
          .tab_prep  = .tab_prep,
          .label_col = .cell$LabelCol,
          .folds     = setdiff(folds_, .k),
          .per_class = .cell$Shots,
          .seed      = .seed
        ),
        .label_col = .cell$LabelCol
      )
      if (.k == min(docs_$Fold)) ex_first_ <<- ex_k_
      cli::cli_alert_info("Fold {(.k)}: examples drawn from fold{?s} {setdiff(folds_, .k)}")
      classify_(.tab = dplyr::filter(docs_, .data$Fold == .k), .examples = ex_k_)
    }) |>
      purrr::list_rbind()
  } else {
    pred_ <- classify_(.tab = docs_, .examples = ex_)
  }
  secs_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))

  spec_ <- list(
    label_col = .cell$LabelCol, model = .cell$Model, tier = .cell$Tier,
    guidance = .cell$Guidance, shots = .cell$Shots, n_chars = .cell$NChars,
    allow_abstain = .cell$AllowAbstain, think = .cell$Think, prompt_hash = hash_,
    limit = .limit, seed = .seed, labels = labels_
  )
  # For crossfold this is the first fold's rendering, which is representative rather than complete;
  # the recipe in the spec is what reproduces the rest.
  example_prompt_ <- llm_prompt(
    .text = docs_$Text[[1]], .labels_block = block_, .task_line = .task_lines[[.cell$LabelCol]],
    .examples = ex_first_, .allow_abstain = .cell$AllowAbstain, .n_chars = .cell$NChars
  )

  # Folds are written from the documents actually classified, not from the fold set the tier is
  # entitled to. The two differ whenever the caller caps the run, and a run written for a fold
  # holding no documents produces a per-class table with no columns.
  written_ <- sort(unique(pred_$Fold))
  purrr::walk(written_, function(.f) {
    llm_write_run(
      .runs_root      = root_,
      .config_name    = cfg_,
      .run_name       = paste0(cfg_, "_F", .f),
      .pred           = dplyr::filter(pred_, .data$Fold == .f),
      .spec           = spec_,
      .prompt_example = example_prompt_,
      .fold           = .f,
      .seconds        = secs_ * (sum(pred_$Fold == .f) / nrow(pred_))
    )
  })
  cli::cli_alert_success("{cfg_}: {nrow(pred_)} document{?s}, {length(written_)} run{?s}")
  invisible(tibble::tibble(ConfigName = cfg_, Fold = written_, Written = TRUE))
}

#' Run every cell in a grid
#' @param .grid Output of llm_grid().
#' @param ... Passed to llm_run_cell().
#' @return Tibble of runs written, invisibly.
llm_sweep <- function(.grid, ...) {
  if (FALSE) .grid <- grid_llm
  cli::cli_alert_info("Sweeping {nrow(.grid)} configuration{?s}")
  purrr::map(seq_len(nrow(.grid)), \(.i) llm_run_cell(.cell = .grid[.i, ], ...)) |>
    purrr::list_rbind() |>
    invisible()
}


# 7. Cost, before it is spent ----------------------------------------------------------------------

#' Time one configuration on a handful of documents and project the sweep
#'
#' A local generation is seconds and the sample is thousands of documents, so a sweep is measured in
#' nights. Read this before starting one: the projection is fitted on this machine and this model
#' rather than on a nominal figure, and it is the difference between an overnight run and a week of
#' them.
#'
#' It times classification directly rather than a whole cell. A cell partitions its documents into
#' run folders by fold, which a five-document probe cannot fill, and writing runs is not part of what
#' is being measured anyway. The probe also caches into a temporary directory, so a repeat projection
#' measures the model again rather than reporting the speed of a cache hit.
#'
#' @param .grid Output of llm_grid(); the first row supplies the configuration to time.
#' @param .tab_prep Prepared sample.
#' @param .task_lines Named character vector: task column to the sentence describing it.
#' @param .definitions Optional definitions tibble.
#' @param .n Documents to time.
#' @param .limit Cap the sweep is to be run under, or NULL. The projection is for what will actually
#'   be run, so a capped sweep must be projected against the cap rather than against the corpus.
#' @param .seed Fixed, so the probe draws the same documents each render.
#' @param .host,.num_ctx,.timeout Transport settings.
#' @return Invisibly a one-row projection tibble.
llm_report_cost <- function(.grid, .tab_prep, .task_lines, .definitions = NULL, .n = 5L,
                            .limit = NULL, .seed = 42L, .host = "http://localhost:11434",
                            .num_ctx = 8192L, .timeout = 600) {
  if (FALSE) {
    .grid       <- grid_blind
    .tab_prep   <- tab_prep
    .task_lines <- .lP$TaskLines
    .n          <- 5L
    .limit      <- 100L
  }
  cell_   <- .grid[1, ]
  labels_ <- llm_labels(.tab_prep = .tab_prep, .label_col = cell_$LabelCol)
  block_  <- llm_render_labels(.labels = labels_, .definitions = .definitions,
                               .guidance = cell_$Guidance)

  # Sampled rather than taken from the top: generation time scales with input length, and the first
  # rows of the sample are not a random draw from the length distribution.
  docs_ <- withr::with_seed(.seed, dplyr::slice_sample(.tab_prep, n = .n))

  calls_ <- .grid |>
    dplyr::mutate(
      Docs = dplyr::if_else(.data$Tier == "tuned", round(nrow(.tab_prep) / 5), nrow(.tab_prep)),
      Docs = if (is.null(.limit)) .data$Docs else pmin(.data$Docs, .limit)
    ) |>
    dplyr::pull(Docs) |>
    sum()

  cli::cli_h2("Cost projection")
  cli::cli_alert_info(
    "Timing {(.n)} document{?s} on {cell_$Model} \\
     ({if (isTRUE(cell_$Think)) 'reasoning on' else 'reasoning off'}) to fit a rate for this machine."
  )

  t0_ <- Sys.time()
  probe_ <- llm_classify(
    .tab           = docs_,
    .label_col     = cell_$LabelCol,
    .labels        = labels_,
    .labels_block  = block_,
    .task_line     = .task_lines[[cell_$LabelCol]],
    .examples      = NULL,
    .model         = cell_$Model,
    .allow_abstain = cell_$AllowAbstain,
    .n_chars       = cell_$NChars,
    .cache_dir     = fs::path(tempdir(), paste0("llmcost-", as.integer(Sys.time()))),
    .think         = cell_$Think,
    .num_ctx       = .num_ctx,
    .host          = .host,
    .timeout       = .timeout
  )
  rate_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs")) / .n

  out_ <- tibble::tibble(
    Configs      = nrow(.grid),
    Limit        = if (is.null(.limit)) NA_integer_ else as.integer(.limit),
    Calls        = calls_,
    Answered     = sum(!is.na(probe_$Raw)),
    SecsPerDoc   = round(rate_, 2),
    ProjectedHrs = round(calls_ * rate_ / 3600, 1)
  )
  clf_say_table(.tab = out_)
  cli::cli_text("")
  cli::cli_alert_info(
    "Every answer is cached, so an interrupted sweep resumes and a re-render costs a directory \\
     listing. Set OLLAMA_NUM_PARALLEL above one to overlap requests; the model, not the client, is \\
     the bottleneck."
  )
  invisible(out_)
}


# 8. Reporting -------------------------------------------------------------------------------------

#' Leaderboard across LLM configurations for one task
#'
#' Coverage sits beside accuracy because an abstaining configuration and a forced-choice one are not
#' comparable on accuracy alone: declining protects precision at the cost of recall, and reading only
#' the accuracy column would rank the two by how often they were allowed to stay silent.
#'
#' @param .tab_overall Bound per-fold metrics from clf_load_overall().
#' @param .label_col Task to rank.
#' @param .n Rows to show.
#' @return Invisibly the leaderboard tibble.
llm_report_leaderboard <- function(.tab_overall, .label_col = "ClassDetailed", .n = 20L) {
  if (FALSE) {
    .tab_overall <- tab_overall
    .label_col   <- "ClassDetailed"
    .n           <- 20L
  }
  out_ <- .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col, !.data$Smoke, grepl("^llm-", .data$Model)) |>
    dplyr::summarise(
      nFolds       = dplyr::n(),
      Coverage     = mean(.data$Coverage),
      SelPrecision = mean(.data$SelPrecision, na.rm = TRUE),
      Accuracy     = mean(.data$Accuracy),
      MacroF1      = mean(.data$F1_macro),
      SdMacroF1    = stats::sd(.data$F1_macro),
      .by = dplyr::any_of(c("Model", "Tier", "Guidance", "Shots", "NChars", "AllowAbstain", "Think"))
    ) |>
    # Blocked by tier, not sorted across it. A tuned row is scored on the single fold its prompt never
    # read and a blind row on all five, so one ranking containing both invites reading the gap
    # between them as an effect of the prompt when part of it is the change of measurement.
    dplyr::arrange(.data$Tier, dplyr::desc(.data$MacroF1))

  cli::cli_h2("LLM configurations: {(.label_col)}")
  out_ |>
    dplyr::slice_head(n = .n, by = Tier) |>
    dplyr::mutate(
      Model        = sub("^llm-", "", .data$Model),
      Coverage     = clf_pct(.data$Coverage),
      SelPrecision = clf_pct(.data$SelPrecision),
      Accuracy     = clf_pct(.data$Accuracy),
      MacroF1      = sprintf("%.3f +/- %.3f", .data$MacroF1, .data$SdMacroF1)
    ) |>
    dplyr::select(Tier, Model, Guidance, Shots, Window = NChars, Abstain = AllowAbstain,
                  dplyr::any_of("Think"), Folds = nFolds, Coverage, SelPrecision, Accuracy,
                  MacroF1) |>
    clf_say_table()
  cli::cli_text("")
  if ("Limit" %in% names(.tab_overall) && any(!is.na(.tab_overall$Limit))) {
    cli::cli_alert_warning(
      "These runs were capped at {max(.tab_overall$Limit, na.rm = TRUE)} documents. Every number \\
       here is a preview: read the ordering, not the level, and expect the thin categories to move."
    )
  }

  # Whether the refusal token was ever used is a structural fact about the arm, not a metric, so it
  # is stated rather than left to be read off a coverage column that looks unremarkable at 100%.
  ab_ <- .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col, !.data$Smoke, grepl("^llm-", .data$Model),
                  .data$AllowAbstain)
  if (nrow(ab_) > 0L) {
    cov_ <- mean(ab_$Coverage, na.rm = TRUE)
    if (cov_ >= 1) {
      cli::cli_alert_warning(
        "Every configuration offered the refusal token committed on every document: the model never \\
         declined. This arm cannot be gated into a cascade whatever the prompt says -- it is a \\
         terminal arm competing head-on with the transformer."
      )
    } else {
      cli::cli_alert_info(
        "Configurations offering the refusal token declined on {clf_pct(1 - cov_)} of documents on \\
         average, so the arm can be gated: it commits where it is confident and leaves the rest."
      )
    }
  }

  cli::cli_alert_info(
    "A blind row's fold spread partitions one pass rather than five estimates, so it measures \\
     document heterogeneity, not model variance."
  )
  if (dplyr::n_distinct(out_$Tier) > 1L) {
    cli::cli_alert_warning(
      "Blind and tuned rows are not comparable in this table: the Folds column shows they are \\
       measured on different amounts of the sample. The like-for-like comparison is in Robustness, \\
       where both tiers are scored on the holdout fold alone."
    )
  }
  invisible(out_)
}

#' Marginal effect of one prompt axis, computed within tier
#'
#' Two aggregation decisions, both of which change the answer.
#'
#' The grid is crossed WITHIN a tier and not between tiers: every tuned configuration carries the
#' richest guidance and permits refusal, because a prompt written with the training folds in view
#' would not withhold either. Pooling the tiers therefore turns each axis into a proxy for tier, and
#' the marginal reports the blind-versus-tuned gap under whatever name the axis happens to have. The
#' effect is not subtle -- it can reverse a comparison outright, reporting an advantage for a larger
#' model that holds a greater share of tuned runs when the two are identical within the blind tier.
#'
#' And the mean is taken over CONFIGURATIONS, not over runs. A configuration whose documents happen
#' to reach five folds contributes five rows while one reaching four contributes four, so a run-level
#' mean silently weights configurations by their fold coverage. That is never intended and matters
#' most in a preview, where a capped sample leaves fold coverage uneven.
#'
#' An axis constant within a tier is skipped for that tier rather than printed as a single row: one
#' level is not a comparison, and printing it invites reading a level as an effect.
#'
#' @param .tab_overall Bound per-fold metrics.
#' @param .axis Column to marginalise over.
#' @param .label_col Task.
#' @return Invisibly the effect tibble, or NULL where the axis varies in no tier.
llm_report_effect <- function(.tab_overall, .axis, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab_overall <- tab_overall
    .axis        <- "Guidance"
    .label_col   <- "ClassDetailed"
  }
  dat_ <- .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col, !.data$Smoke, grepl("^llm-", .data$Model),
                  !is.na(.data[[.axis]]))
  if (nrow(dat_) == 0L) {
    cli::cli_alert_info("No LLM runs carry {(.axis)}; nothing to marginalise.")
    return(invisible(NULL))
  }

  # Stage one: one number per configuration, so fold coverage cannot weight the comparison.
  per_cfg_ <- dat_ |>
    dplyr::summarise(
      nFolds   = dplyr::n(),
      Coverage = mean(.data$Coverage),
      MacroF1  = mean(.data$F1_macro),
      .by = dplyr::all_of(c("ConfigName", "Tier", .axis))
    )

  # Stage two: within tier, where the grid is actually crossed.
  out_ <- per_cfg_ |>
    dplyr::summarise(
      nConfigs  = dplyr::n(),
      Coverage  = mean(.data$Coverage),
      MacroF1   = mean(.data$MacroF1),
      SdMacroF1 = stats::sd(.data$MacroF1),
      .by = dplyr::all_of(c("Tier", .axis))
    )

  vary_ <- out_ |>
    dplyr::summarise(nLevels = dplyr::n_distinct(.data[[.axis]]), .by = Tier)
  keep_ <- vary_$Tier[vary_$nLevels > 1L]
  skip_ <- vary_$Tier[vary_$nLevels <= 1L]

  if (length(keep_) == 0L) {
    cli::cli_alert_info(
      "{(.axis)} takes one level in every tier, so there is no comparison to report."
    )
    return(invisible(NULL))
  }

  out_ |>
    dplyr::filter(.data$Tier %in% keep_) |>
    dplyr::arrange(.data$Tier, dplyr::desc(.data$MacroF1)) |>
    dplyr::mutate(
      Coverage = clf_pct(.data$Coverage),
      MacroF1  = sprintf("%.3f +/- %.3f", .data$MacroF1, .data$SdMacroF1),
      SdMacroF1 = NULL
    ) |>
    clf_say_table(.title = paste0("Marginal effect of ", .axis, ", within tier"))

  if (length(skip_) > 0L) {
    cli::cli_alert_info(
      "Not shown for tier{?s} {skip_}: {(.axis)} takes one level there, so the axis is constant \\
       and any number would describe the tier rather than the axis."
    )
  }
  invisible(out_)
}

#' Head-to-head against the transformer and the keyword table on identical folds
#'
#' The table this stage exists to produce, and the one most exposed to a comparison that is not like
#' for like. LLM configurations exist at two tiers covering different amounts of the sample: a blind
#' arm is scored on all five folds, a tuned arm only on the fold its prompt never read. Taking the
#' highest-scoring LLM configuration regardless of tier therefore sets a number computed on a fifth
#' of the sample beside a transformer computed on all of it, and flatters the LLM by construction --
#' the tuned tier scores higher partly because it is the easier measurement, not only because the
#' prompt is better.
#'
#' Only one tier enters, named in the heading, and `nFolds` is shown so any residual mismatch is
#' visible rather than assumed away. The blind tier is the default because it is the arm the
#' orchestrator can actually use.
#'
#' @param .tab_overall Bound per-fold metrics from every runs root.
#' @param .label_col Task.
#' @param .tier LLM tiers admitted to the comparison. Defaults to the two that cover the whole
#'   sample, since those are the only ones measurable against a transformer scored on all of it.
#'   Rows from the other engines carry no tier and are always kept.
#' @return Invisibly the comparison tibble.
llm_report_head_to_head <- function(.tab_overall, .label_col = "ClassDetailed",
                                    .tier = c("blind", "crossfold")) {
  if (FALSE) {
    .tab_overall <- tab_overall
    .label_col   <- "ClassDetailed"
    .tier        <- c("blind", "crossfold")
  }
  dat_ <- .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col, !.data$Smoke) |>
    dplyr::mutate(
      Method = dplyr::case_when(
        grepl("^llm-", .data$Model)     ~ "LLM",
        grepl("^keyword-", .data$Model) ~ "Keyword",
        TRUE                            ~ "Transformer"
      )
    )
  # Tier is NA on transformer and keyword rows, which is what keeps them in.
  if ("Tier" %in% names(dat_)) {
    dat_ <- dat_ |> dplyr::filter(is.na(.data$Tier) | .data$Tier %in% .tier)
  }

  out_ <- dat_ |>
    dplyr::summarise(
      nFolds   = dplyr::n_distinct(.data$TestFold),
      nDocs    = sum(.data$nTest),
      Coverage = mean(.data$Coverage),
      Accuracy = mean(.data$Accuracy),
      MacroF1  = mean(.data$F1_macro),
      .by = c(Method, ConfigName)
    ) |>
    dplyr::slice_max(.data$MacroF1, n = 1L, by = Method, with_ties = FALSE) |>
    dplyr::arrange(dplyr::desc(.data$MacroF1))

  cli::cli_h2("Best of each method: {(.label_col)} (LLM tier{?s}: {(.tier)})")
  out_ |>
    dplyr::mutate(
      Coverage = clf_pct(.data$Coverage),
      Accuracy = clf_pct(.data$Accuracy),
      MacroF1  = sprintf("%.3f", .data$MacroF1)
    ) |>
    dplyr::select(Method, nFolds, nDocs, Coverage, Accuracy, MacroF1, ConfigName) |>
    clf_say_table()
  cli::cli_text("")
  if (dplyr::n_distinct(out_$nFolds) > 1L) {
    cli::cli_alert_warning(
      "Rows here cover different numbers of folds, so the comparison is not like for like. Read \\
       nDocs before reading MacroF1."
    )
  }
  cli::cli_alert_info(
    "An LLM row well below the transformer answers the question this stage was built to answer. A \\
     row close to it makes the arm worth routing to, which the orchestrator decides, not this file."
  )
  invisible(out_)
}

#' Where the LLM and the transformer disagree, and who is right
#'
#' The diagnostic that predicts, before any routing is scored, whether this arm can contribute. If the
#' transformer is right far more often on the documents they dispute, an arm that only commits on
#' those documents has nothing to add.
#'
#' @param .pred_llm Pooled LLM predictions.
#' @param .pred_bert Pooled transformer predictions.
#' @param .none Abstention sentinel.
#' @return Invisibly the summary tibble.
llm_report_disagreement <- function(.pred_llm, .pred_bert, .none = LLM_NONE) {
  if (FALSE) {
    .pred_llm  <- pred_llm
    .pred_bert <- pred_det
  }
  joined_ <- .pred_llm |>
    dplyr::select(DocID, TrueLabel, LlmPred = .data$PredLabel) |>
    dplyr::inner_join(
      .pred_bert |> dplyr::select(DocID, BertPred = .data$PredLabel),
      by = dplyr::join_by(DocID)
    )
  n_ <- nrow(joined_)

  out_ <- joined_ |>
    dplyr::mutate(
      Set = dplyr::case_when(
        .data$LlmPred == .none          ~ "LLM abstained",
        .data$LlmPred == .data$BertPred ~ "Agree",
        TRUE                            ~ "Disagree"
      ),
      LlmRight  = .data$LlmPred == .data$TrueLabel,
      BertRight = .data$BertPred == .data$TrueLabel
    ) |>
    dplyr::summarise(
      nDocs     = dplyr::n(),
      Share     = dplyr::n() / n_,
      LlmRight  = mean(.data$LlmRight),
      BertRight = mean(.data$BertRight),
      .by = Set
    ) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))

  cli::cli_h2("LLM against the transformer, document by document")
  out_ |>
    dplyr::mutate(
      Share     = clf_pct(.data$Share),
      LlmRight  = clf_pct(.data$LlmRight),
      BertRight = clf_pct(.data$BertRight)
    ) |>
    clf_say_table()
  cli::cli_text("")
  cli::cli_alert_info(
    "The Disagree row is the only place routing can change anything. If the transformer is right \\
     there far more often, keeping it is already near-optimal and this arm cannot contribute."
  )
  invisible(out_)
}

#' Every console block for one task, in order
#' @param .tab_overall Bound per-fold metrics.
#' @param .pred_llm Pooled predictions of the leading LLM configuration.
#' @param .pred_bert Pooled transformer predictions.
#' @param .label_col Task.
#' @return Invisibly NULL.
llm_report_all <- function(.tab_overall, .pred_llm, .pred_bert, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab_overall <- tab_overall
    .pred_llm    <- pred_llm
    .pred_bert   <- pred_det
    .label_col   <- "ClassDetailed"
  }
  cli::cli_h1("LLM summary: {(.label_col)}")
  llm_report_leaderboard(.tab_overall = .tab_overall, .label_col = .label_col)
  purrr::walk(c("Model", "Guidance", "AllowAbstain", "Think", "NChars"), function(.a) {
    llm_report_effect(.tab_overall = .tab_overall, .axis = .a, .label_col = .label_col)
  })
  llm_report_head_to_head(.tab_overall = .tab_overall, .label_col = .label_col)
  llm_report_disagreement(.pred_llm = .pred_llm, .pred_bert = .pred_bert)
  invisible(NULL)
}


# 9. Figures ---------------------------------------------------------------------------------------

#' Coverage against selective precision, one point per configuration
#'
#' The plot that separates a cautious configuration from an accurate one. A point high and to the
#' right dominates; a point high and to the left is precise only because it declined most of the
#' corpus, which is a usable property for a routing arm and a poor one for a standalone classifier.
#'
#' @param .tab_overall Bound per-fold metrics.
#' @param .label_col Task.
#' @return A ggplot.
llm_plot_coverage <- function(.tab_overall, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab_overall <- tab_overall
    .label_col   <- "ClassDetailed"
  }
  dat_ <- .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col, !.data$Smoke, grepl("^llm-", .data$Model)) |>
    dplyr::summarise(
      Coverage     = mean(.data$Coverage),
      SelPrecision = mean(.data$SelPrecision, na.rm = TRUE),
      .by = c(Model, Guidance, AllowAbstain)
    ) |>
    dplyr::mutate(Model = sub("^llm-", "", .data$Model))

  p_ <- dat_ |>
    ggplot2::ggplot(ggplot2::aes(x = Coverage, y = SelPrecision, shape = Model, color = Guidance)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_color_grey(start = 0.1, end = 0.6) +
    ggplot2::labs(x = "Coverage", y = "Precision on committed documents")
  clf_apply_theme(.plot = p_)
}

#' Macro-F1 by configuration, blind and tuned distinguished
#'
#' Tuned bars are drawn hollow: their prompt was written against four of the five folds, so their
#' score is not an estimate of what a new prompt would achieve.
#'
#' @param .tab_overall Bound per-fold metrics.
#' @param .label_col Task.
#' @param .n Configurations to show.
#' @return A ggplot.
llm_plot_leaderboard <- function(.tab_overall, .label_col = "ClassDetailed", .n = 12L) {
  if (FALSE) {
    .tab_overall <- tab_overall
    .label_col   <- "ClassDetailed"
    .n           <- 12L
  }
  dat_ <- .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col, !.data$Smoke, grepl("^llm-", .data$Model)) |>
    dplyr::summarise(MacroF1 = mean(.data$F1_macro), .by = c(Model, Tier, Guidance, AllowAbstain)) |>
    dplyr::slice_max(.data$MacroF1, n = .n) |>
    dplyr::mutate(
      Label = paste0(sub("^llm-", "", .data$Model), " ", .data$Guidance,
                     dplyr::if_else(.data$AllowAbstain, " +abstain", "")),
      Blind = .data$Tier == "blind"
    )

  p_ <- dat_ |>
    dplyr::mutate(Label = forcats::fct_reorder(.data$Label, .data$MacroF1)) |>
    ggplot2::ggplot(ggplot2::aes(x = Label, y = MacroF1, fill = Blind)) +
    ggplot2::geom_col(width = 0.7, color = "grey20") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", MacroF1)), hjust = -0.15, size = 3) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "grey30", `FALSE` = "white"),
                               labels = c(`TRUE` = "Blind", `FALSE` = "Tuned on folds 1-4")) +
    ggplot2::scale_y_continuous(limits = c(0, 1.08), expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Macro-F1", fill = NULL)
  clf_apply_theme(.plot = p_)
}

# Local null-coalescing helper (base R gained %||% in 4.4; this keeps the file self-contained).
`%||%` <- function(.x, .y) if (is.null(.x)) .y else .x
