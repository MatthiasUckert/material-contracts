# 03D-ClassifyLLM: a local language model as a third arm (llm_*) -------------------------------------------------------
#
# WHAT THIS FILE DOES
# Classifies contracts by asking a locally hosted language model, over a grid that varies the model,
# how much taxonomy documentation it is shown, how many worked examples it is given, how much of the
# document it reads, and whether it is permitted to decline. Answers are constrained to the label set
# by a response schema, cached per document and configuration, and written into the same run-folder
# schema the transformer and keyword arms use, so one leaderboard ranks all three.
#
# THREE TIERS, AND WHY THEY ARE KEPT APART
#   blind      the prompt was written without reading any labelled document; scores every fold
#   crossfold  worked examples drawn from the folds the model is not being scored on
#   tuned      anything developed with folds one to four in view, scored only on the fifth
# A tuned score is not an estimate of what a new prompt would achieve, and pooling the tiers would
# make every swept axis a proxy for that gap, since a tuned prompt carries the richest settings by
# construction. Every leaderboard and marginal below therefore reports tier explicitly.
#
# ABSTENTION
# Where the schema permits it the model may decline, emitting LLM_NONE. That is the same sentinel the
# keyword arm uses and it is handled by the same abstention-aware scoring layer: declining costs
# recall and protects precision. Coverage is consequently a reported dimension rather than an
# afterthought, and the coverage-against-precision figure is the one that distinguishes a cautious
# configuration from an accurate one.
#
# WHAT IS NOT HERE
# Sample construction, folds, the scoring layer and the category vocabulary: those are 03A, which the
# runbook sources first, so this arm is scored by identical code on identical splits. The transformer
# predictions this arm is measured against come from 03B. The look of any figure or table is
# _Commons/_Plots.R and _Commons/_Tables.R; nothing below sets a colour, a font or a height.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; if (FALSE) dev blocks; pure ASCII; parenthesised cli interpolation.

LLM_NONE <- "(none)"

# The token the model is told to emit when it declines. Kept distinct from the sentinel so a genuine
# category could never be confused with a refusal, and mapped to the sentinel on the way in.
LLM_UNSURE <- "UNSURE"


# 1. The taxonomy the model is shown -----------------------------------------------------------------------------------
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


# 2. Prompts -----------------------------------------------------------------------------------------------------------
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


# 3. Transport ---------------------------------------------------------------------------------------------------------
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
      "{length(miss_)} model{?s} not present on this server: {miss_}",
      "i" = "Pull with {.code ollama pull}, or remove from the grid.",
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


# 4. One configuration over a document set -----------------------------------------------------------------------------

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
#' @param .num_ctx Context window. Also in the name, because a window below what a prompt needs
#'   changes the answer -- the server trims the front of the prompt and the model works from a
#'   contract with no instruction. Two runs at different windows are different runs, and without
#'   this the idempotence check would skip a re-run under a corrected window as already done.
#' @param .seed Stamped for parity.
#' @return Character scalar.
llm_config_name <- function(.label_col, .model, .tier, .guidance, .shots, .n_chars,
                            .allow_abstain, .prompt_hash, .limit = NULL, .think = FALSE,
                            .num_ctx = 8192L, .seed = 42L) {
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
    .num_ctx       <- 8192L
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
    "_K", .num_ctx,
    "_X", .prompt_hash,
    if (is.null(.limit)) "" else paste0("_L", .limit),
    "_S", .seed
  )
}

#' The longest prompt a configuration can produce
#'
#' Exact rather than sampled. The document contributes at most `.n_chars` because the excerpt is
#' truncated there, and every other part is fixed for a configuration, so the ceiling is reached by
#' any document at least that long and can be measured by building one prompt against a maximal
#' filler. Sampling real documents would give the same answer more slowly and occasionally miss it.
#'
#' Characters are converted to tokens by a stated ratio rather than by tokenising, because the
#' tokenizer lives inside the server and this has to run before the first request. Underestimating
#' tokens is the dangerous direction -- it silently truncates -- so the default is deliberately
#' pessimistic for English legal text, where long entity names and citation strings pack more tokens
#' per character than prose.
#'
#' @param .labels_block Rendered category list.
#' @param .task_line The instruction.
#' @param .examples Rendered example block, or NULL.
#' @param .allow_abstain Logical.
#' @param .n_chars Document window in characters.
#' @param .chars_per_token Assumed density. Lower is more conservative.
#' @return Tibble: Chars, EstTokens.
llm_prompt_budget <- function(.labels_block, .task_line, .examples, .allow_abstain, .n_chars,
                              .chars_per_token = 3.2) {
  if (FALSE) {
    .labels_block    <- llm_render_labels(llm_labels(tab_prep, "ClassDetailed"))
    .task_line       <- "Classify this contract into exactly one category."
    .examples        <- NULL
    .allow_abstain   <- FALSE
    .n_chars         <- 6000L
    .chars_per_token <- 3.2
  }
  filler_ <- strrep("x", .n_chars)
  n_ <- nchar(llm_prompt(
    .text          = filler_,
    .labels_block  = .labels_block,
    .task_line     = .task_line,
    .examples      = .examples,
    .allow_abstain = .allow_abstain,
    .n_chars       = .n_chars
  ))
  tibble::tibble(Chars = as.integer(n_), EstTokens = as.integer(ceiling(n_ / .chars_per_token)))
}

#' Context window a prompt budget requires
#'
#' The window is a capacity floor, not a modelling choice: above the point where nothing truncates it
#' changes no answer, and below it the server silently trims the FRONT of the prompt -- which is
#' where the instruction and the category list live, leaving the model a contract and no task. That
#' failure does not error. Under a JSON enum the model still returns a valid category name, just an
#' uninformed one.
#'
#' Rounded up to a power of two above a floor, and both choices are deliberate. The floor costs
#' nothing on hardware with memory to spare and stops a short prompt from getting its own bespoke
#' window. The coarse grid means a small change to the wording does not shift the window, which
#' matters because the window is part of the cache key: a scheme that tracked the budget exactly
#' would invalidate every cached answer whenever a comma moved.
#'
#' @param .tokens Estimated tokens the longest prompt needs.
#' @param .min Floor, which should be the window already in use so valid work stays valid.
#' @param .headroom Multiplier over the estimate.
#' @param .max Ceiling. Above this the configuration is refused rather than truncated.
#' @return Integer context window.
llm_ctx_for <- function(.tokens, .min = 8192L, .headroom = 1.25, .max = 32768L) {
  if (FALSE) {
    .tokens   <- 11500L
    .min      <- 8192L
    .headroom <- 1.25
    .max      <- 32768L
  }
  need_ <- .tokens * .headroom
  ctx_  <- max(.min, 2^ceiling(log2(max(need_, 1))))
  if (ctx_ > .max) {
    cli::cli_abort(c(
      "This configuration needs about {round(need_)} tokens of context, above the {(.max)} ceiling.",
      "i" = "Shorten the document window, shorten the worked examples, or raise the ceiling if the \\
             model supports it."
    ))
  }
  as.integer(ctx_)
}

#' Report what each configuration in a grid will demand of the context window
#'
#' Read before a sweep. A configuration that will not fit fails after its first long document rather
#' than its first document, because prompt length varies with the contract and short ones fit -- so
#' the failure arrives late, looks intermittent, and costs whatever ran before it.
#'
#' @param .grid Output of llm_grid().
#' @param .tab_prep Prepared sample.
#' @param .task_lines Named character vector of instructions.
#' @param .definitions Optional definitions tibble.
#' @param .example_chars Characters shown per worked example.
#' @param .ctx_min,.ctx_max Floor and ceiling for the derived window.
#' @param .seed Fixed, so example draws match the sweep's.
#' @return Invisibly a tibble of budgets.
llm_report_budget <- function(.grid, .tab_prep, .task_lines, .definitions = NULL,
                              .example_chars = 700L, .ctx_min = 8192L, .ctx_max = 32768L,
                              .seed = 42L) {
  if (FALSE) {
    .grid       <- grid_cross
    .tab_prep   <- tab_prep
    .task_lines <- .lP$TaskLines
  }
  out_ <- purrr::map(seq_len(nrow(.grid)), function(.i) {
    cell_   <- .grid[.i, ]
    labels_ <- llm_labels(.tab_prep = .tab_prep, .label_col = cell_$LabelCol)
    block_  <- llm_render_labels(.labels = labels_, .definitions = .definitions,
                                 .guidance = cell_$Guidance)
    ex_ <- if (cell_$Shots > 0L) {
      llm_render_examples(
        .tab = llm_examples(.tab_prep = .tab_prep, .label_col = cell_$LabelCol,
                            .folds = sort(unique(.tab_prep$Fold))[-1], # one fold held out, as in the sweep
                            .per_class = cell_$Shots, .seed = .seed),
        .label_col = cell_$LabelCol, .n_chars = .example_chars
      )
    } else {
      NULL
    }
    b_ <- llm_prompt_budget(
      .labels_block = block_, .task_line = .task_lines[[cell_$LabelCol]], .examples = ex_,
      .allow_abstain = cell_$AllowAbstain, .n_chars = cell_$NChars
    )
    tibble::tibble(
      Model = cell_$Model, Tier = cell_$Tier, Shots = cell_$Shots,
      ExampleChars = if (cell_$Shots > 0L) nchar(ex_ %||% "") else 0L,
      Chars = b_$Chars, EstTokens = b_$EstTokens,
      NumCtx = tryCatch(
        llm_ctx_for(.tokens = b_$EstTokens, .min = .ctx_min, .max = .ctx_max),
        error = function(e) NA_integer_
      )
    )
  }) |>
    purrr::list_rbind()

  cli::cli_h2("Context budget per configuration")
  tbl_say(.tab = out_)
  cli::cli_text("")
  over_ <- out_ |> dplyr::filter(is.na(.data$NumCtx))
  if (nrow(over_) > 0L) {
    cli::cli_alert_danger(
      "{nrow(over_)} configuration{?s} exceed the {(.ctx_max)} ceiling and will not run."
    )
  }
  cli::cli_alert_info(
    "The window is derived per configuration, not fixed: below what a prompt needs the server trims \\
     its FRONT, removing the instruction and the category list while still returning a valid label."
  )
  invisible(out_)
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


# 5. Run folders in the shared schema ----------------------------------------------------------------------------------

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
    NumCtx       = as.integer(.spec$num_ctx %||% NA_integer_),
    EstTokens    = as.integer(.spec$est_tokens %||% NA_integer_),
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


# 6. The sweep ---------------------------------------------------------------------------------------------------------

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
#' @param .example_chars Characters shown per worked example. Forty-eight examples at seven hundred
#'   characters is thirty-six thousand characters of demonstration against a six-thousand-character
#'   document, so this is the lever when the examples come to outweigh the thing being classified.
#' @param .num_ctx Context window, or NULL to derive it from the longest prompt this cell produces.
#' @param .ctx_min,.ctx_max Floor and ceiling for the derived window.
#' @param .limit Cap on documents classified, or NULL for the whole entitled set. A capped run is
#'   written to a sibling preview root under a name carrying the cap, so it cannot be mistaken for,
#'   or pooled with, a full one.
#' @param .seed Fixed, so the preview draws the same documents on every render.
#' @param ... Transport settings passed to llm_classify().
#' @return Tibble of the runs written, invisibly.
llm_run_cell <- function(.cell, .tab_prep, .definitions = NULL, .task_lines, .runs_root, .cache_dir,
                         .folds_full = 1:5, .fold_holdout = 5L, .example_folds = 1:4,
                         .overwrite = FALSE, .limit = NULL, .seed = 42L,
                         .example_chars = 700L, .num_ctx = NULL, .ctx_min = 8192L,
                         .ctx_max = 32768L, ...) {
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
  folds_ <- if (.cell$Tier == "tuned") .fold_holdout else .folds_full

  # Every fold's example block is built here rather than inside the loop below. The context window
  # has to be sized before the first request, a crossfold cell has one block per fold, and the
  # longest of them is what the window must accommodate. Building them is slicing a table; measuring
  # them is what stops a run failing on its first LONG document rather than its first document.
  ex_by_fold_ <- if (cross_ && .cell$Shots > 0L) {
    purrr::set_names(
      purrr::map(folds_, function(.k) {
        llm_render_examples(
          .tab = llm_examples(.tab_prep = .tab_prep, .label_col = .cell$LabelCol,
                              .folds = setdiff(folds_, .k), .per_class = .cell$Shots,
                              .seed = .seed),
          .label_col = .cell$LabelCol, .n_chars = .example_chars
        )
      }),
      as.character(folds_)
    )
  } else {
    NULL
  }

  ex_tab_ <- if (.cell$Shots > 0L && !cross_) {
    llm_examples(.tab_prep = .tab_prep, .label_col = .cell$LabelCol, .folds = .example_folds,
                 .per_class = .cell$Shots, .seed = .seed)
  } else {
    NULL
  }
  ex_ <- llm_render_examples(.tab = ex_tab_, .label_col = .cell$LabelCol,
                             .n_chars = .example_chars)

  # Sized from the longest prompt this cell can produce, over every fold's examples. A fixed window
  # is right for one shot count and wrong for the others, and wrong quietly: below what a prompt
  # needs the server trims its FRONT, taking the instruction and the category list with it.
  budget_ <- purrr::map(c(list(ex_), ex_by_fold_), function(.e) {
    llm_prompt_budget(
      .labels_block  = block_,
      .task_line     = .task_lines[[.cell$LabelCol]],
      .examples      = .e,
      .allow_abstain = .cell$AllowAbstain,
      .n_chars       = .cell$NChars
    )
  }) |>
    purrr::list_rbind()
  need_ <- max(budget_$EstTokens)
  ctx_  <- .num_ctx %||% llm_ctx_for(.tokens = need_, .min = .ctx_min, .max = .ctx_max)
  if (!is.null(.num_ctx) && .num_ctx < need_) {
    cli::cli_alert_warning(
      "Forced window {(.num_ctx)} is below the {need_} tokens this configuration needs; the server \\
       will trim the front of the prompt."
    )
  }
  cli::cli_alert_info("Context: {need_} token{?s} estimated, window {ctx_}.")

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
    .think         = .cell$Think,
    .num_ctx       = ctx_
  )
  root_ <- llm_runs_root(.runs_root = .runs_root, .limit = .limit)

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
      .num_ctx       = ctx_,
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
      # Built above rather than here, because the context window had to be sized before the first
      # request and that needs every fold's block, not just this one's.
      ex_k_ <- ex_by_fold_[[as.character(.k)]]
      if (.k == min(docs_$Fold)) ex_first_ <<- ex_k_
      comp_ <- setdiff(folds_, .k)
      cli::cli_alert_info("Fold {(.k)}: examples from {length(comp_)} other fold{?s}: {comp_}")
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
    limit = .limit, seed = .seed, num_ctx = ctx_, example_chars = .example_chars,
    est_tokens = need_, labels = labels_
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


# 7. Cost, before it is spent ------------------------------------------------------------------------------------------

# The identity of a fitted rate. Everything here changes how long one answer takes: the prompt
# through the categories, the guidance level, the worked examples and the document window; the
# hardware path through the model tag and the context window, which sets the size of the attention
# cache. Nothing else does -- which tier a configuration belongs to and how many documents it faces
# change the bill, not the rate. Keyed on this rather than on a chunk name, for the same reason the
# answer cache is: a name-keyed store serves the previous configuration's number without erroring.
LLM_RATE_KEYS <- c("Task", "Model", "Guidance", "Shots", "NChars", "Abstain", "Think", "NumCtx")

#' The shape of a rate table, with no rows in it
#'
#' Defined once because two callers need an empty one and neither may produce a table with no
#' COLUMNS. An empty tibble is not the same object as a zero-row tibble of the right type: the first
#' fails a join with a message about missing columns, and it appears only once the store exists and a
#' render finds nothing left to measure -- which is to say, never on the render that writes the code
#' and always on the one after.
#'
#' @return Zero-row tibble carrying the rate schema.
llm_rates_empty <- function() {
  tibble::tibble(
    Task = character(), Model = character(), Guidance = character(), Shots = integer(),
    NChars = integer(), Abstain = logical(), Think = logical(), NumCtx = integer(),
    SecsPerDoc = numeric(), Answered = integer(), FittedOn = character(), FittedN = integer()
  )
}

#' Read previously fitted generation rates
#'
#' Missing store, unreadable store and store from an older key set all return the same thing: no
#' known rates. A projection that silently reused a rate fitted under a different key would be worse
#' than one that re-times, and re-timing costs minutes rather than correctness.
#'
#' @param .path Parquet file, or NULL to keep nothing.
#' @param .key_cols Columns identifying a rate.
#' @return Tibble with the key columns plus SecsPerDoc, Answered, FittedOn, FittedN.
llm_rates_read <- function(.path, .key_cols = LLM_RATE_KEYS) {
  if (FALSE) {
    .path     <- .lP$Output$Rates
    .key_cols <- LLM_RATE_KEYS
  }
  if (is.null(.path) || !fs::file_exists(.path)) return(llm_rates_empty())

  out_ <- tryCatch(arrow::read_parquet(.path), error = function(e) NULL)
  if (is.null(out_) || !all(.key_cols %in% names(out_))) {
    cli::cli_alert_warning(
      "The rate store at {(.path)} does not carry the current key, so every shape is re-timed."
    )
    return(llm_rates_empty())
  }
  out_
}

#' Persist fitted generation rates
#'
#' @param .rates Tibble to store.
#' @param .path Parquet file, or NULL to keep nothing.
#' @return Invisibly .rates.
llm_rates_write <- function(.rates, .path) {
  if (FALSE) {
    .rates <- rates_
    .path  <- .lP$Output$Rates
  }
  if (is.null(.path)) return(invisible(.rates))
  fs::dir_create(fs::path_dir(.path))
  arrow::write_parquet(x = .rates, sink = .path)
  invisible(.rates)
}

#' Time a handful of documents per prompt shape and project the sweep
#'
#' A local generation is seconds and the sample is thousands of documents, so a sweep is measured in
#' nights. Read this before starting one: the projection is fitted on this machine and this model
#' rather than on a nominal figure, and it is the difference between an overnight run and a week of
#' them.
#'
#' ONE RATE PER PROMPT SHAPE, NOT ONE RATE FOR THE GRID. With the reasoning pass off the answer is a
#' single enum token, so nearly the whole cost is prefill and prefill is the prompt. The prompt is
#' the category list plus the worked examples, and the example block is categories times shots -- so
#' a four-shot twelve-category prompt runs several times the length of a blind one, and a
#' two-category task is a fraction of a twelve-category one at the same shot count. A single rate
#' fitted on one blind cell and multiplied by every cell's documents understates the long shapes and
#' overstates the short ones simultaneously, and the two errors do not cancel: per task they point in
#' opposite directions. The grid is therefore grouped into the shapes that genuinely differ, each is
#' timed, and each is projected against its own documents.
#'
#' It times classification directly rather than a whole cell. A cell partitions its documents into
#' run folders by fold, which a five-document probe cannot fill, and writing runs is not part of what
#' is being measured anyway. The probe caches into a temporary directory rather than the sweep's, so
#' a rate is always measured against the model rather than against a cache hit.
#'
#' FITTED RATES PERSIST, KEYED ON WHAT DETERMINES THEM. A rate is a property of a machine, a model
#' and a prompt shape, none of which changes between two renders of the same document -- so re-timing
#' every shape on every render buys an identical number for real minutes of inference. The store is
#' keyed on the shape and the derived window, exactly as the answer cache is keyed on the prompt, so
#' a changed configuration misses and is re-timed rather than being served the old configuration's
#' number. `FittedOn` travels with each rate and is printed, because a rate is only as good as the
#' machine it was measured on and a stale one should be visible rather than merely absent. Pass
#' `.refit = TRUE` after a hardware or server change; pass `.rate_store = NULL` to keep nothing.
#'
#' @param .grid Output of llm_grid(), or several bound together.
#' @param .tab_prep Prepared sample.
#' @param .task_lines Named character vector: task column to the sentence describing it.
#' @param .definitions Optional definitions tibble.
#' @param .n Documents to time PER SHAPE. Total probe cost is this times the number of shapes.
#' @param .limit Cap the sweep is to be run under, or NULL. The projection is for what will actually
#'   be run, so a capped sweep must be projected against the cap rather than against the corpus.
#' @param .seed Fixed, so the probe draws the same documents each render.
#' @param .example_chars Characters shown per worked example, as in the sweep.
#' @param .ctx_min,.ctx_max Floor and ceiling for the derived window.
#' @param .host,.num_ctx,.timeout Transport settings. A NULL window is derived per shape exactly as
#'   the sweep derives it, because the window sets the size of the attention cache and therefore the
#'   speed of the thing being measured.
#' @param .rate_store Parquet file holding fitted rates, or NULL to fit everything every time.
#' @param .refit Logical. Re-time every shape and overwrite the store. For a new machine or a
#'   changed server, where the stored numbers are wrong rather than merely old.
#' @return Invisibly a tibble, one row per shape.
llm_report_cost <- function(.grid, .tab_prep, .task_lines, .definitions = NULL, .n = 5L,
                            .limit = NULL, .seed = 42L, .host = "http://localhost:11434",
                            .num_ctx = NULL, .timeout = 600, .example_chars = 700L,
                            .ctx_min = 8192L, .ctx_max = 32768L,
                            .rate_store = NULL, .refit = FALSE) {
  if (FALSE) {
    .grid          <- dplyr::bind_rows(grid_blind, grid_cross)
    .tab_prep      <- tab_prep
    .task_lines    <- .lP$TaskLines
    .definitions   <- tab_definitions
    .n             <- 5L
    .limit         <- NULL
    .seed          <- 42L
    .host          <- .lP$Engine$Host
    .num_ctx       <- NULL
    .timeout       <- .lP$Engine$Timeout
    .example_chars <- 700L
    .ctx_min       <- .lP$Engine$CtxMin
    .ctx_max       <- .lP$Engine$CtxMax
    .rate_store    <- .lP$Output$Rates
    .refit         <- FALSE
  }
  # Tier decides how many documents a cell classifies, not how long each one takes, so it is counted
  # here and left out of the shape below. Two cells at one shot count and different tiers pay the
  # same rate on different amounts of work.
  cells_ <- .grid |>
    dplyr::mutate(
      Docs = dplyr::if_else(.data$Tier == "tuned", round(nrow(.tab_prep) / 5), nrow(.tab_prep)),
      Docs = if (is.null(.limit)) .data$Docs else pmin(.data$Docs, .limit)
    )

  shape_cols_ <- c("LabelCol", "Model", "Guidance", "Shots", "NChars", "AllowAbstain", "Think")
  shapes_ <- cells_ |>
    dplyr::summarise(
      Configs = dplyr::n(),
      Docs    = sum(.data$Docs),
      .by     = dplyr::all_of(shape_cols_)
    )

  docs_ <- withr::with_seed(.seed, dplyr::slice_sample(.tab_prep, n = .n))

  cli::cli_h2("Cost projection")

  # EVERY SHAPE IS RESOLVED WITHOUT INFERENCE FIRST. Building the prompt and deriving the window are
  # pure functions of the configuration and the sample, so they cost nothing and can be done for all
  # shapes; only the RATE needs the model. Resolving first is what makes the store keyable, because
  # the derived window is part of what determines the rate and is not knowable from the grid alone.
  prompts_ <- purrr::map(seq_len(nrow(shapes_)), function(.i) {
    sh_     <- shapes_[.i, ]
    labels_ <- llm_labels(.tab_prep = .tab_prep, .label_col = sh_$LabelCol)
    block_  <- llm_render_labels(.labels = labels_, .definitions = .definitions,
                                 .guidance = sh_$Guidance)
    ex_ <- if (sh_$Shots > 0L) {
      llm_render_examples(
        .tab = llm_examples(.tab_prep = .tab_prep, .label_col = sh_$LabelCol,
                            .folds = sort(unique(.tab_prep$Fold))[-1], # one fold held out, as in the sweep
                            .per_class = sh_$Shots, .seed = .seed),
        .label_col = sh_$LabelCol, .n_chars = .example_chars
      )
    } else {
      NULL
    }
    budget_ <- llm_prompt_budget(
      .labels_block  = block_,
      .task_line     = .task_lines[[sh_$LabelCol]],
      .examples      = ex_,
      .allow_abstain = sh_$AllowAbstain,
      .n_chars       = sh_$NChars
    )
    # Derived as the sweep derives it. A probe run at a smaller window than the sweep will use
    # measures a smaller attention cache and reports a rate the sweep cannot reproduce.
    ctx_ <- .num_ctx %||% llm_ctx_for(.tokens = budget_$EstTokens, .min = .ctx_min, .max = .ctx_max)

    list(
      Shape = tibble::tibble(
        Task      = sh_$LabelCol,
        Model     = sh_$Model,
        Guidance  = sh_$Guidance,
        Shots     = sh_$Shots,
        NChars    = sh_$NChars,
        Abstain   = sh_$AllowAbstain,
        Think     = sh_$Think,
        NumCtx    = ctx_,
        EstTokens = budget_$EstTokens,
        Configs   = sh_$Configs,
        Calls     = sh_$Docs
      ),
      Labels = labels_, Block = block_, Examples = ex_
    )
  })
  shape_tab_ <- purrr::map(prompts_, "Shape") |> purrr::list_rbind()

  known_ <- llm_rates_read(.path = .rate_store, .key_cols = LLM_RATE_KEYS)
  need_  <- if (.refit) {
    shape_tab_
  } else {
    shape_tab_ |> dplyr::anti_join(known_, by = LLM_RATE_KEYS)
  }
  cli::cli_alert_info(
    "{nrow(shape_tab_)} prompt shape{?s}, {nrow(need_)} needing a rate: \\
     {nrow(need_) * .n} generation{?s} before anything else starts."
  )

  fitted_ <- purrr::map(seq_len(nrow(need_)), function(.j) {
    key_ <- need_[.j, ]
    pr_  <- purrr::detect(prompts_, function(.p) {
      identical(.p$Shape[LLM_RATE_KEYS], key_[LLM_RATE_KEYS])
    })
    t0_    <- Sys.time()
    probe_ <- llm_classify(
      .tab           = docs_,
      .label_col     = key_$Task,
      .labels        = pr_$Labels,
      .labels_block  = pr_$Block,
      .task_line     = .task_lines[[key_$Task]],
      .examples      = pr_$Examples,
      .model         = key_$Model,
      .allow_abstain = key_$Abstain,
      .n_chars       = key_$NChars,
      .cache_dir     = fs::path(tempdir(), paste0("llmcost-", as.integer(Sys.time()), "-", .j)),
      .think         = key_$Think,
      .num_ctx       = key_$NumCtx,
      .host          = .host,
      .timeout       = .timeout
    )
    key_[LLM_RATE_KEYS] |>
      dplyr::mutate(
        SecsPerDoc = round(as.numeric(difftime(Sys.time(), t0_, units = "secs")) / .n, 2),
        Answered   = sum(!is.na(probe_$Raw)),
        FittedOn   = as.character(Sys.Date()),
        FittedN    = as.integer(.n)
      )
  }) |>
    purrr::list_rbind()

  # Binding an empty list returns a tibble with no COLUMNS rather than no rows, so the joins below
  # lose the key they join on. The schema is restored rather than inferred from the loop, because a
  # loop that ran zero times has no schema to infer -- and this is the branch a second render takes
  # every time, once the store holds every shape.
  if (nrow(fitted_) == 0L) fitted_ <- llm_rates_empty()

  rates_ <- dplyr::bind_rows(fitted_, dplyr::anti_join(known_, fitted_, by = LLM_RATE_KEYS))
  llm_rates_write(.rates = rates_, .path = .rate_store)

  out_ <- shape_tab_ |>
    dplyr::inner_join(rates_, by = LLM_RATE_KEYS) |>
    dplyr::mutate(Hours = round(.data$Calls * .data$SecsPerDoc / 3600, 1)) |>
    dplyr::select(Task, Model, Shots, Configs, EstTokens, NumCtx, Calls, Answered, SecsPerDoc,
                  FittedOn, Hours)

  out_ |>
    dplyr::arrange(.data$Task, .data$Shots) |>
    tbl_say(.title = "Per prompt shape")
  cli::cli_text("")

  out_ |>
    dplyr::summarise(
      Configs = sum(.data$Configs),
      Calls   = sum(.data$Calls),
      Hours   = round(sum(.data$Hours), 1),
      .by     = Task
    ) |>
    dplyr::arrange(dplyr::desc(.data$Hours)) |>
    tbl_say(.title = "Per task")
  cli::cli_text("")

  cli::cli_alert_info(
    "{round(sum(out_$Hours), 1)} hours for everything above, of which any configuration already on \\
     disk costs nothing: the sweep skips a configuration whose runs exist, and every individual \\
     answer is cached besides. Read the per-task rows to price what is actually outstanding."
  )
  if (nrow(fitted_) == 0L) {
    cli::cli_alert_info(
      "No shape was timed on this render; every rate came from the store. FittedOn says when each \\
       was measured -- re-render with .refit = TRUE after a hardware or server change."
    )
  }
  cli::cli_alert_info(
    "These figures are serial. Set OLLAMA_NUM_PARALLEL above one to overlap requests; the model, \\
     not the client, is the bottleneck."
  )
  invisible(out_)
}


# 8. Reporting ---------------------------------------------------------------------------------------------------------

#' Reconcile the runs on disk against the configurations this render produced
#'
#' The old inventory was a directory listing, which is a statement about the filesystem rather than
#' about this document. A run folder outlives the naming scheme that produced it: add a term to
#' `llm_config_name()` -- the context window, say -- and every existing folder keeps a name the
#' current scheme would never write, while remaining perfectly readable. Nothing errors. The
#' orchestrator then reads one prompt under two names, ranks it as two arms with identical scores,
#' and either double-counts its folds or drops one of them arbitrarily.
#'
#' Reconciled rather than guessed. Both sweeps return the configuration name of every cell they
#' touched, written or skipped, so the set of current names is known exactly and needs no rule about
#' which naming scheme is in force. Anything on disk and outside that set is superseded; anything in
#' the set and not on disk did not complete.
#'
#' A superseded run is REPORTED, NOT DELETED. Deleting is cheap to say and expensive to be wrong
#' about, and these cost nights of inference to produce.
#'
#' @param .swept Bound return values of llm_sweep(), carrying ConfigName.
#' @param .runs_root Runs directory for this render.
#' @return Invisibly a tibble of the runs on disk, with Status.
llm_report_inventory <- function(.swept, .runs_root) {
  if (FALSE) {
    .swept     <- dplyr::bind_rows(swept_blind, swept_cross, swept_tuned)
    .runs_root <- .dir_runs
  }
  disk_ <- fs::dir_ls(.runs_root, type = "directory") |>
    fs::path_file() |>
    tibble::tibble(Run = _) |>
    dplyr::mutate(ConfigName = sub("_F\\d+$", "", .data$Run)) |>
    dplyr::count(ConfigName, name = "Folds")

  current_ <- .swept |> dplyr::distinct(ConfigName) |> dplyr::pull(ConfigName)

  out_ <- disk_ |>
    dplyr::mutate(
      Status = dplyr::if_else(.data$ConfigName %in% current_, "current", "SUPERSEDED")
    ) |>
    dplyr::arrange(.data$Status, .data$ConfigName)

  missing_ <- setdiff(current_, disk_$ConfigName)

  cli::cli_h2("Runs on disk against the configurations this render produced")
  out_ |> tbl_say()
  cli::cli_text("")

  n_old_ <- sum(out_$Status == "SUPERSEDED")
  if (n_old_ > 0L) {
    cli::cli_alert_warning(
      "{n_old_} configuration{?s} on disk {?was/were} not produced by any grid in this render. The \\
       orchestrator reads this directory, so {?it/they} will enter its inventory as {?an arm/arms} \\
       nothing here can account for -- and where the prompt is the same as a current one, as a \\
       duplicate of it carrying identical scores under a second name."
    )
    cli::cli_alert_info(
      "Left in place deliberately. Confirm against the current names above and remove the folders, \\
       or keep them and expect the duplication downstream."
    )
  } else {
    cli::cli_alert_success("Every run on disk was produced by a configuration in this render.")
  }
  if (length(missing_) > 0L) {
    cli::cli_alert_danger(
      "{length(missing_)} configuration{?s} {?was/were} swept but {?has/have} no runs on disk: \\
       {missing_}"
    )
  }
  invisible(out_)
}

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
      Coverage     = tbl_pct(.data$Coverage),
      SelPrecision = tbl_pct(.data$SelPrecision),
      Accuracy     = tbl_pct(.data$Accuracy),
      MacroF1      = sprintf("%.3f +/- %.3f", .data$MacroF1, .data$SdMacroF1)
    ) |>
    dplyr::select(Tier, Model, Guidance, Shots, Window = NChars, Abstain = AllowAbstain,
                  dplyr::any_of("Think"), Folds = nFolds, Coverage, SelPrecision, Accuracy,
                  MacroF1) |>
    tbl_say()
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
        "Configurations offering the refusal token declined on {tbl_pct(1 - cov_)} of documents on \\
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
      Coverage = tbl_pct(.data$Coverage),
      MacroF1  = sprintf("%.3f +/- %.3f", .data$MacroF1, .data$SdMacroF1),
      SdMacroF1 = NULL
    ) |>
    tbl_say(.title = paste0("Marginal effect of ", .axis, ", within tier"))

  if (length(skip_) > 0L) {
    cli::cli_alert_info(
      "Not shown for {length(skip_)} tier{?s} ({skip_}): {(.axis)} takes one level there, so the \\
       axis is constant and any number would describe the tier rather than the axis."
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
      Coverage = tbl_pct(.data$Coverage),
      Accuracy = tbl_pct(.data$Accuracy),
      MacroF1  = sprintf("%.3f", .data$MacroF1)
    ) |>
    dplyr::select(Method, nFolds, nDocs, Coverage, Accuracy, MacroF1, ConfigName) |>
    tbl_say()
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
      Share     = tbl_pct(.data$Share),
      LlmRight  = tbl_pct(.data$LlmRight),
      BertRight = tbl_pct(.data$BertRight)
    ) |>
    tbl_say()
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


# 9. Figures -------------------------------------------------------------------------------------------------------------
# The look comes entirely from _Commons/_Plots.R. What these functions own is the mapping from a swept
# tibble to a figure shape, which is the part that has to know what the numbers mean.
#
# ONE RULE GOVERNS BOTH FIGURES: a configuration is the four-way combination of model, tier, guidance
# and abstention permission, and every one of those four must reach the page. Collapsing any of them
# does not merge two views of one thing, it averages two different things -- and pooling the tier in
# particular contradicts the reason the tiers exist, since a tuned prompt read four of the five folds
# and its score answers a different question from a blind one's.

#' Number of bars the leaderboard will draw
#'
#' Figure height is chosen from the ladder by row count, and a leaderboard's row count is only known
#' once the sweep has run. Deriving it here rather than fixing a number in the chunk option keeps a
#' partially-swept grid from rendering three bars at the height reserved for twelve.
#'
#' @param .tab_overall Bound per-fold metrics from clf_load_overall().
#' @param .label_col Character. Task the leaderboard covers.
#' @param .n Integer. Cap the leaderboard applies.
#' @return Integer, at least one.
llm_n_configs <- function(.tab_overall, .label_col = "ClassDetailed", .n = 12L) {
  if (FALSE) {
    .tab_overall <- tab_overall
    .label_col   <- "ClassDetailed"
    .n           <- 12L
  }
  n_ <- .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col, !.data$Smoke, grepl("^llm-", .data$Model)) |>
    dplyr::distinct(.data$Model, .data$Tier, .data$Guidance, .data$AllowAbstain) |>
    nrow()
  max(1L, min(as.integer(.n), n_))
}

#' Coverage against precision on committed documents
#'
#' The figure that separates a cautious configuration from an accurate one. A point high and to the
#' right dominates. A point high and to the left is precise only because it declined most of the
#' corpus, which is a usable property in a cascade and a poor one standing alone.
#'
#' Every axis of the grid is encoded: tier by shape, prompt design by colour, model by panel. That is
#' three aesthetics for what could be drawn as one cloud of points, and the alternative is worse --
#' averaging a blind configuration together with a tuned one produces a point describing neither, and
#' the reader has no way to see that it happened.
#'
#' Both axes run the full zero to one rather than zooming to the points, because "to the left" is the
#' whole reading and it only means anything against the absolute scale: a cluster occupying the top
#' right corner is itself the result.
#'
#' @param .tab_overall Bound per-fold metrics from clf_load_overall().
#' @param .label_col Character. Task to plot.
#' @return A ggplot.
#' The prompting gradient, one panel per taxonomy
#'
#' The finding this stage carries is not a score, it is a SHAPE: showing the model worked examples
#' drawn from folds it is not being scored on lifts it substantially, and each further example lifts
#' it again with diminishing returns. Read on one taxonomy that shape is a fact about a taxonomy,
#' and a reader is entitled to ask whether twelve fine-grained categories are simply hard to describe
#' in a sentence. Read on three -- twelve categories, seven, and a binary -- it is a fact about
#' prompting.
#'
#' Panels rather than colours because the tasks are not comparable in level: a binary decision starts
#' near a coin flip and a twelve-way one near a twentieth, so plotting them on shared axes would put
#' the eye on the intercepts, which mean nothing across taxonomies, instead of on the slopes, which
#' are the whole point. The vertical scale is free for the same reason and is the one place in this
#' document where it is.
#'
#' Full-coverage tiers only. A tuned configuration is scored on a single fold, so its point would sit
#' on the same line as five-fold estimates while describing a fifth of the sample.
#'
#' @param .tab_overall Bound per-fold metrics.
#' @param .tasks Tasks to panel, in the order they should read.
#' @return A ggplot.
llm_plot_gradient <- function(.tab_overall, .tasks = c("ClassDetailed", "ClassBroad", "AmendType")) {
  if (FALSE) {
    .tab_overall <- tab_overall
    .tasks       <- c("ClassDetailed", "ClassBroad", "AmendType")
  }
  # Summarised before the plot so the axis breaks can be read off what is actually drawn. Taking them
  # from the input instead would let a tier or a task this figure excludes put a tick on the axis.
  dat_ <- .tab_overall |>
    dplyr::filter(
      .data$LabelCol %in% .tasks,
      !.data$Smoke,
      grepl("^llm-", .data$Model),
      .data$Tier %in% c("blind", "crossfold")
    ) |>
    dplyr::summarise(
      MacroF1 = mean(.data$F1_macro),
      SE      = stats::sd(.data$F1_macro) / sqrt(dplyr::n()),
      .by = c(LabelCol, Model, Shots)
    ) |>
    dplyr::mutate(
      Model    = sub("^llm-", "", .data$Model),
      LabelCol = factor(.data$LabelCol, levels = .tasks)
    )

  dat_ |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Shots, y = .data$MacroF1, colour = .data$Model)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$MacroF1 - .data$SE, ymax = .data$MacroF1 + .data$SE,
                   fill = .data$Model),
      alpha = 0.15, colour = NA
    ) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::facet_wrap(ggplot2::vars(LabelCol), scales = "free_y") +
    plot_scale_colour_cat(name = "Model") +
    plot_scale_fill_cat(name = "Model") +
    ggplot2::scale_x_continuous(breaks = sort(unique(dat_$Shots))) +
    ggplot2::labs(x = "Worked examples per category", y = "Macro-F1") +
    plot_theme(.grid = "both", .legend = "bottom")
}

llm_plot_coverage <- function(.tab_overall, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab_overall <- tab_overall
    .label_col   <- "ClassDetailed"
  }
  .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col, !.data$Smoke, grepl("^llm-", .data$Model)) |>
    dplyr::summarise(
      Coverage     = mean(.data$Coverage),
      SelPrecision = mean(.data$SelPrecision, na.rm = TRUE),
      .by = c(Model, Tier, Guidance, AllowAbstain)
    ) |>
    dplyr::mutate(
      Model  = sub("^llm-", "", .data$Model),
      Prompt = paste0(.data$Guidance, dplyr::if_else(.data$AllowAbstain, " +abstain", ""))
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$Coverage, y = .data$SelPrecision,
      colour = .data$Prompt, shape = .data$Tier
    )) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::facet_wrap(ggplot2::vars(Model)) +
    plot_scale_colour_cat(name = "Prompt") +
    ggplot2::scale_x_continuous(limits = c(0, 1), labels = scales::label_percent()) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
    ggplot2::labs(x = "Coverage", y = "Precision on committed documents", shape = "Tier") +
    plot_theme(.grid = "both", .legend = "bottom")
}

#' Macro-F1 by configuration, with the tuned tier drawn hollow
#'
#' The bar label names all four axes of the grid. That is verbose, and the alternative is silent
#' overplotting: two configurations sharing a label share a bar position, and geom_col draws them on
#' top of one another with their value labels superimposed, so three configurations render as one bar
#' carrying three illegible numbers.
#'
#' Fill repeats the tier distinction because it is the one that decides whether a bar can be read at
#' face value. A tuned prompt was written against four of the five folds, so its score is not an
#' estimate of what a new prompt would achieve; drawing it hollow keeps it on the same axis as the
#' honest estimates without inviting the comparison. This is why the figure is not built on the shared
#' ranked-bar primitive, which draws a single ink by design.
#'
#' @param .tab_overall Bound per-fold metrics from clf_load_overall().
#' @param .label_col Character. Task to plot.
#' @param .n Integer. Configurations to show.
#' @return A ggplot.
llm_plot_leaderboard <- function(.tab_overall, .label_col = "ClassDetailed", .n = 12L) {
  if (FALSE) {
    .tab_overall <- tab_overall
    .label_col   <- "ClassDetailed"
    .n           <- 12L
  }
  .tab_overall |>
    dplyr::filter(.data$LabelCol == .label_col, !.data$Smoke, grepl("^llm-", .data$Model)) |>
    dplyr::summarise(MacroF1 = mean(.data$F1_macro), .by = c(Model, Tier, Guidance, AllowAbstain)) |>
    dplyr::slice_max(.data$MacroF1, n = .n, with_ties = FALSE) |>
    dplyr::mutate(
      Label = paste0(
        sub("^llm-", "", .data$Model), " ", .data$Guidance,
        dplyr::if_else(.data$AllowAbstain, " +abstain", ""), " [", .data$Tier, "]"
      ),
      Estimate = dplyr::if_else(.data$Tier == "tuned", "Tuned on folds 1-4", "Blind or cross-fold"),
      Label    = forcats::fct_reorder(.data$Label, .data$MacroF1)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$MacroF1, y = .data$Label, fill = .data$Estimate)) +
    ggplot2::geom_col(width = 0.7, colour = .plot_ink, linewidth = 0.3) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", .data$MacroF1)),
      hjust = -0.18, size = (.plot_base - 3) / ggplot2::.pt, family = .plot_font
    ) +
    ggplot2::scale_fill_manual(
      values = c(`Blind or cross-fold` = .plot_ink, `Tuned on folds 1-4` = "#FFFFFF"),
      name   = NULL
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1.08), breaks = seq(0, 1, by = 0.25),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(x = "Macro-F1", y = NULL) +
    plot_theme(.grid = "none", .legend = "bottom")
}

# Local null-coalescing helper (base R gained %||% in 4.4; this keeps the file self-contained).
`%||%` <- function(.x, .y) if (is.null(.x)) .y else .x
