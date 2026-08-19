# ======================================================================================================================
# _NER.R -- extraction suites, the candidate store, and the benchmark ledger
# ======================================================================================================================
#
# Sourced by 04A before its own library. Three suites produce entity candidates; one store holds
# them; one ledger records what has been done.
#
# THE ARCHITECTURE, IN ONE PARAGRAPH. Each suite is a separate Python environment invoked as a
# subprocess, because their dependencies are mutually incompatible -- LexNLP is pinned to Python 3.8
# and ships in a container, spaCy pulls a gigabyte of models, and matcon-extract needs pandas and
# nothing else. Parquet is the seam: R writes the staged text, Python writes candidate spans, R
# ingests. Python never touches DuckDB and R never parses a document.
#
# THREE FUNCTIONS RATHER THAN ONE DISPATCHER. An earlier design routed every suite through one
# entry point driven by a registry table. It was abandoned because the suites are not uniform:
# spaCy takes a model and a batch size because it streams through nlp.pipe, LexNLP takes neither and
# needs volume mounts, matcon takes neither and resolves modules from the labels. A single signature
# covering all three is mostly arguments that do not apply, and a registry restating what each suite
# can do is a second declaration that can disagree with the first -- which is exactly the defect
# that made this rewrite necessary.
#
# EXTRACTION AND INGEST ARE SEPARATE, and that is not tidiness. The benchmark runs extraction and
# discards the output; if extraction wrote to the store there would be no way to time it without
# polluting what it measures.
#
# THE OFFSET CONTRACT. Every span is 0-based, half-open, over CODE POINTS, and
# text[Start:Stop] == Span exactly. Python emits code-point offsets; R must slice with
# stringi::stri_sub and never base substr, which indexes bytes and misaligns about ninety-nine per
# cent of spans on this corpus. Every check script tests this and it has caught real bugs.


# 1. Suites ------------------------------------------------------------------------------------------------------------

#' Default worker count for every suite
#'
#' Twenty rather than twenty-four, and the reason is the machine. Measured throughput rises 1.60x
#' from eight workers to sixteen and only 1.15x from sixteen to twenty-four, which is the shape of a
#' 28-core M3 Ultra: twenty performance cores and eight efficiency cores. Workers past the twentieth
#' land on efficiency cores running at roughly a third the speed, so they add scheduling pressure
#' and very little work.
#'
#' The transformer ignores this. extract_spacy.py forces n_process to 1 whenever a GPU is active,
#' because one Metal device cannot be shared across worker processes, so the outlier resolves itself
#' rather than needing a special case here.
.ner_n_process <- 20L

#' Default documents per unit of work
#'
#' One value across all three suites. The per-suite variation this replaces was calibrated against
#' extractors that no longer exist: the gazetteer ran at roughly five seconds per document under
#' gazetteer-v1 and needed a chunk of eight, and gazetteer-v2 runs at two thousand documents a
#' second, where a chunk of eight is twenty times more task dispatches than the work justifies.
#'
#' 04A's benchmark section measures this. A deviation earns its place by producing a number, not by
#' being inherited.
.ner_batch_size <- 32L

#' Default per-document cap, in seconds
#'
#' A HANG DETECTOR, NOT A BUDGET. At matcon's throughput a document taking ten minutes is running
#' five orders of magnitude beyond the median and is pathological by definition. LexNLP's geoentity
#' pass averages about five seconds per document and scales with length, so ten minutes is roughly
#' two orders of magnitude of headroom -- generous, deliberately, because a timeout there is not a
#' lost document but a document lost SYSTEMATICALLY IN THE LONGEST AGREEMENTS, which is the worst
#' possible place for a missing-at-random assumption to fail.
.ner_timeout <- 600L

#' Where each suite lives
#'
#' Resolved from the repository root, so a checkout anywhere works and no home directory is written
#' down. Each suite is its own uv project and is invoked through its own interpreter: never `uv run`
#' from the repository root, which resolves the root environment instead of the suite's.
#'
#' @param .suite One of "spacy", "lexnlp", "matcon".
#' @return Path to the suite's folder.
.ner_suite_dir <- function(.suite) {
  if (FALSE) {
    .suite <- "matcon"
  }
  here::here(switch(
    .suite,
    spacy  = "contracts-spacy",
    lexnlp = "contracts-lexnlp",
    matcon = "contracts-extract",
    cli::cli_abort("Unknown suite {(.suite)}.")
  ))
}

#' Which device a spaCy model should run on
#'
#' THE ONLY SURVIVING PER-MODEL TUNING VALUE, and it is not really tuning -- it is a fact about what
#' each pipeline is made of.
#'
#' Activating a GPU forces n_process to 1, because one Metal device cannot be shared across worker
#' processes. For the transformer that is the right trade: the model is a single large matrix
#' operation per batch and the device wins by more than the twenty workers it costs.
#'
#' For the CNN pipelines it is exactly the wrong trade. Their per-document work is small enough that
#' host-device transfer dominates the arithmetic, so activating the GPU buys almost nothing and pays
#' for it by dropping from twenty CPU workers to one. Measured on this sample: en_core_web_lg on
#' MPS with one worker ran at roughly one window per second, which is over an hour for a pass the
#' CPU does in minutes.
#'
#' @param .model spaCy model name.
#' @return "auto" for a transformer pipeline, "cpu" otherwise.
.ner_spacy_device <- function(.model) {
  if (FALSE) {
    .model <- "en_core_web_trf"
  }
  if (stringi::stri_detect_fixed(.model, "trf")) "auto" else "cpu"
}

#' A suite's Python interpreter
#'
#' @param .suite One of "spacy", "matcon". LexNLP has no interpreter on the host; it runs in a
#'   container.
#' @return Path to the suite venv's python.
.ner_python <- function(.suite) {
  if (FALSE) {
    .suite <- "matcon"
  }
  out_ <- fs::path(.ner_suite_dir(.suite), ".venv", "bin", "python")
  if (!fs::file_exists(out_)) {
    cli::cli_abort(c(
      "No interpreter for suite {(.suite)} at {(out_)}.",
      "i" = "cd {(.ner_suite_dir(.suite))} && uv venv --python 3.12 && uv pip install ..."
    ))
  }
  out_
}

#' Run a subprocess, stream its output, and abort on failure
#'
#' Every suite call goes through this. Output streams to the console rather than being captured,
#' because these run for minutes to hours and a progress bar nobody can see is worse than no
#' progress bar. A non-zero exit aborts rather than returning quietly: a suite that failed halfway
#' leaves a partial parquet, and ingesting it would record partial extraction as complete.
#'
#' @param .cmd Executable.
#' @param .args Character vector of arguments.
#' @param .label What to name in the abort message.
#' @return Elapsed seconds, invisibly.
.ner_system <- function(.cmd, .args, .label) {
  if (FALSE) {
    .cmd   <- "echo"
    .args  <- "hello"
    .label <- "demo"
  }
  t0_     <- Sys.time()
  status_ <- system2(.cmd, .args, stdout = "", stderr = "")
  secs_   <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))
  if (!identical(status_, 0L)) {
    cli::cli_abort("{(.label)} exited with status {(status_)} after {round(secs_, 1)}s.")
  }
  invisible(secs_)
}

#' Extract with spaCy
#'
#' One model per call. Vectorising over models was considered and rejected: extract_spacy.py takes
#' --model singular, so four models are four subprocess launches whether the loop sits in R or in
#' Python, and the benchmark needs a wall clock per model rather than one for the set. Looping at
#' the call site keeps a single tested path.
#'
#' @param .path_in Staged parquet carrying .id_col and .text_col.
#' @param .out_dir Directory for the output parquet. Created if absent.
#' @param .model spaCy model name, e.g. "en_core_web_lg".
#' @param .labels Cross-engine labels to keep. NULL keeps everything the model produces.
#' @param .id_col,.text_col Column names in the staged parquet.
#' @param .max_chars Truncate each document to its first N characters. 0 disables.
#' @param .n_process Worker processes. Ignored on GPU, where the script forces 1.
#' @param .batch_size Documents per nlp.pipe batch.
#' @param .timeout Per-document cap in seconds.
#' @param .device One of auto, cpu, cuda, mps. NULL resolves per model -- see .ner_spacy_device().
#' @param .quiet Suppress the progress bar.
#' @return Path to the parquet written, invisibly, with an "elapsed" attribute in seconds.
ner_spacy <- function(.path_in,
                      .out_dir,
                      .model      = "en_core_web_lg",
                      .labels     = NULL,
                      .id_col     = "DocID",
                      .text_col   = "TextRaw",
                      .max_chars  = 0L,
                      .n_process  = .ner_n_process,
                      .batch_size = .ner_batch_size,
                      .timeout    = .ner_timeout,
                      .device     = NULL,
                      .quiet      = FALSE) {
  if (FALSE) {
    .path_in    <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")
    .out_dir    <- here::here("2_output", "04A-EntityExtract", "Stage")
    .model      <- "en_core_web_lg"
    .labels     <- c("ORG", "PERSON", "GPE")
    .id_col     <- "DocID"
    .text_col   <- "TextRaw"
    .max_chars  <- 0L
    .n_process  <- 20L
    .batch_size <- 32L
    .timeout    <- 600L
    .device     <- NULL
    .quiet      <- FALSE
  }

  # NULL means "decide from the model", which is what keeps the rule in one place instead of at
  # every call site. An explicit value still wins, for the benchmark and for debugging.
  device_ <- .device %||% .ner_spacy_device(.model = .model)

  fs::dir_create(.out_dir)
  out_ <- fs::path(.out_dir, paste0("spacy__", .model, ".parquet"))

  args_ <- c(
    fs::path(.ner_suite_dir("spacy"), "extract_spacy.py"),
    .path_in,
    "--output",     out_,
    "--model",      .model,
    "--id-col",     .id_col,
    "--text-col",   .text_col,
    "--max-chars",  .max_chars,
    "--n-process",  .n_process,
    "--batch-size", .batch_size,
    "--timeout",    .timeout,
    "--device",     device_,
    if (!is.null(.labels)) c("--label", .labels),
    if (.quiet) "--no-progress"
  )

  secs_ <- .ner_system(.ner_python("spacy"), as.character(args_), paste0("spacy:", .model))
  structure(out_, elapsed = secs_) |> invisible()
}

#' Extract with LexNLP
#'
#' Runs in a container because LexNLP 2.3.0 is pinned to Python 3.8. The inputs' common parent is
#' mounted read-only at /work and the output directory at /out, so nothing the container writes can
#' reach anything but its own output.
#'
#' NO --platform FLAG. Pinning linux/amd64 breaks image resolution under the containerd image store,
#' and the native arm64 build is 1.93x faster. The architecture warning at runtime is cosmetic.
#'
#' @param .path_in Staged parquet carrying .id_col and .text_col.
#' @param .out_dir Directory for the output parquet. Created if absent.
#' @param .model Ignored; present so all three suites share one signature. LexNLP has one version
#'   and it is the container's.
#' @param .labels Labels to extract. Defaults to everything this suite produces.
#' @param .id_col,.text_col Column names in the staged parquet.
#' @param .max_chars Truncate each document to its first N characters. 0 disables.
#' @param .n_process Worker processes inside the container.
#' @param .batch_size Documents per task.
#' @param .timeout Per-document cap in seconds.
#' @param .image Container image name.
#' @param .quiet Suppress the progress bar.
#' @return Path to the parquet written, invisibly, with an "elapsed" attribute in seconds.
ner_lexnlp <- function(.path_in,
                       .out_dir,
                       .model      = NULL,
                       .labels     = c("ORG", "GPE", "DATE", "MONEY"),
                       .id_col     = "DocID",
                       .text_col   = "TextRaw",
                       .max_chars  = 0L,
                       .n_process  = .ner_n_process,
                       .batch_size = .ner_batch_size,
                       .timeout    = .ner_timeout,
                       .image      = "contracts-lexnlp",
                       .quiet      = FALSE) {
  if (FALSE) {
    .path_in    <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")
    .out_dir    <- here::here("2_output", "04A-EntityExtract", "Stage")
    .model      <- NULL
    .labels     <- c("ORG", "GPE", "DATE", "MONEY")
    .id_col     <- "DocID"
    .text_col   <- "TextRaw"
    .max_chars  <- 0L
    .n_process  <- 20L
    .batch_size <- 32L
    .timeout    <- 600L
    .image      <- "contracts-lexnlp"
    .quiet      <- FALSE
  }

  if (Sys.which("docker") == "") cli::cli_abort("docker not found on PATH.")
  fs::dir_create(.out_dir)

  # THE IMAGE MUST MATCH ITS SOURCES. Editing extract_lexnlp.py without rebuilding leaves a running
  # container that no longer matches the repository, and the extraction it produces is attributed in
  # the store to code that has changed since. The container runs, the parquet is well formed, and
  # the rows are wrong about their own provenance. A warning rather than an abort, because a stale
  # image is still usable if the caller knows.
  ner_lexnlp_check_image(.image = .image, .abort = FALSE)

  in_    <- fs::path_real(.path_in)
  base_  <- fs::path_dir(in_)
  out_   <- fs::path(.out_dir, "lexnlp__lexnlp.parquet")

  args_ <- c(
    "run", "--rm",
    "-v", paste0(as.character(base_), ":/work:ro"),
    "-v", paste0(as.character(fs::path_real(.out_dir)), ":/out"),
    .image,
    fs::path("/work", fs::path_file(in_)),
    "--output",     fs::path("/out", fs::path_file(out_)),
    "--id-col",     .id_col,
    "--text-col",   .text_col,
    "--label",      .labels,
    "--max-chars",  .max_chars,
    "--n-process",  .n_process,
    "--chunk-size", .batch_size,
    "--timeout",    .timeout,
    if (.quiet) "--no-progress"
  )

  secs_ <- .ner_system("docker", as.character(args_), "lexnlp")
  structure(out_, elapsed = secs_) |> invisible()
}

#' Extract with matcon-extract
#'
#' RETURNS SEVERAL PATHS, unlike the other two. Labels map to modules inside the package -- DATE and
#' TERM both come from dateregex, MONEY from moneyregex -- so one call can run several extractors,
#' and each writes its own parquet. One parquet per model is not a convenience: ner_db_append()
#' asserts that a staging file carries exactly one (Engine, Model) pair, because a file carrying two
#' would attribute spans to a method that never saw the document.
#'
#' THE CALLER NEVER NAMES A MODEL. Which version of dateregex runs is whatever is installed, and the
#' extractor stamps it. That is the rule the whole restructuring restored, and ner_matcon_describe()
#' is how R learns the tag without typing it.
#'
#' @param .path_in Staged parquet carrying .id_col and .text_col.
#' @param .out_dir Directory for the output parquets. Created if absent.
#' @param .model Ignored; present so all three suites share one signature. The package versions
#'   itself and reports through --describe.
#' @param .labels Labels to extract. Defaults to everything this suite produces.
#' @param .id_col,.text_col Column names in the staged parquet.
#' @param .max_chars Truncate each document to its first N characters. 0 disables.
#' @param .n_process Worker processes.
#' @param .batch_size Documents per task.
#' @param .timeout Per-document cap in seconds.
#' @param .quiet Suppress the progress bar.
#' @return Character vector of parquet paths, invisibly, with an "elapsed" attribute in seconds.
ner_matcon <- function(.path_in,
                       .out_dir,
                       .model      = NULL,
                       .labels     = c("DATE", "TERM", "MONEY", "REDACT", "GPE"),
                       .id_col     = "DocID",
                       .text_col   = "TextRaw",
                       .max_chars  = 0L,
                       .n_process  = .ner_n_process,
                       .batch_size = .ner_batch_size,
                       .timeout    = .ner_timeout,
                       .quiet      = FALSE) {
  if (FALSE) {
    .path_in    <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")
    .out_dir    <- here::here("2_output", "04A-EntityExtract", "Stage")
    .model      <- NULL
    .labels     <- c("DATE", "TERM", "MONEY", "REDACT", "GPE")
    .id_col     <- "DocID"
    .text_col   <- "TextRaw"
    .max_chars  <- 0L
    .n_process  <- 20L
    .batch_size <- 32L
    .timeout    <- 600L
    .quiet      <- FALSE
  }

  fs::dir_create(.out_dir)

  args_ <- c(
    "-m", "matcon_extract",
    .path_in,
    "--out-dir",    .out_dir,
    "--label",      .labels,
    "--id-col",     .id_col,
    "--text-col",   .text_col,
    "--max-chars",  .max_chars,
    "--n-process",  .n_process,
    "--chunk-size", .batch_size,
    "--timeout",    .timeout,
    if (.quiet) "--no-progress"
  )

  secs_ <- .ner_system(.ner_python("matcon"), as.character(args_), "matcon")

  # The package writes <engine>__<model>.parquet per model, so the paths are discovered rather than
  # predicted -- R does not know which models the requested labels resolved to until it asks.
  want_  <- ner_matcon_describe() |>
    dplyr::filter(purrr::map_lgl(.data$Labels, \(.l) any(.l %in% .labels)))
  out_   <- fs::path(.out_dir, paste0(want_$Engine, "__", want_$Model, ".parquet"))
  miss_  <- out_[!fs::file_exists(out_)]
  if (length(miss_) > 0L) {
    cli::cli_abort("matcon reported success but {length(miss_)} expected file(s) are absent: {(miss_)}")
  }

  structure(as.character(out_), elapsed = secs_) |> invisible()
}
# 2. Description -------------------------------------------------------------------------------------------------------
#
# WHAT THIS SECTION SOLVES. The ledger is keyed on (DocID, Engine, Model, Label), so R must know the
# model tag BEFORE a pass in order to ask what still needs doing. But the model is stamped by the
# extractor and never typed in R -- that is the rule that keeps a version string out of the
# runbooks. Something has to bridge the two.
#
# The previous answer was a table in R naming each extractor's labels, keyed on the extractor STEM.
# It was silently wrong the moment dateregex-v3 emitted a second label: the stem said DATE, the
# request asked for DATE, TERM was filtered out inside Python, and the ledger recorded success.
# Nothing errored, and no check could have caught it, because the table and the code agreed about
# the stem and disagreed about nothing R could see.
#
# So the extractor answers for itself. matcon-extract --describe reports its models, labels and spec
# hashes as JSON; R reads that rather than restating it. There is no second declaration, so there is
# nothing to drift.
#
# THE ASYMMETRY IS DELIBERATE. spaCy and LexNLP do not self-describe, so their label sets are
# declared here. That is honest rather than inconsistent: ours describes itself because we wrote it,
# and a third-party wrapper is described because we did not. Their label vocabularies have not
# changed in the project's life, and a check asserts the declaration matches what actually arrived.


#' What matcon-extract will stamp, asked of the package itself
#'
#' Calls `python -m matcon_extract --describe` and parses the JSON. One subprocess, roughly a second,
#' and it replaces every R-side statement of what matcon can do.
#'
#' `SpecHash` is the fingerprint of the constants that determine an extractor's output -- pattern
#' tables, vocabularies, and for the gazetteer a content hash of its lookup file. A hash that has
#' moved under an unchanged model tag means somebody edited a rule without bumping the version,
#' which would silently change what an existing store's rows mean. Checking it here costs a second
#' and fails before the first document is read; the alternative is discovering it after a slice has
#' already been ingested.
#'
#' `Ready` is FALSE where an extractor's data dependency is absent. The gazetteer without its lookup
#' can produce no output at all, so it reports no hash rather than one that merely looks valid.
#'
#' @return Tibble: Module, Engine, Model, Labels (list), Extras (list), SpecHash, Ready.
#' @examples
#' if (FALSE) ner_matcon_describe()
ner_matcon_describe <- function() {
  if (FALSE) {
    # no arguments
  }

  raw_ <- system2(
    .ner_python("matcon"),
    c("-m", "matcon_extract", "--describe"),
    stdout = TRUE, stderr = FALSE
  )
  if (length(raw_) == 0L) cli::cli_abort("matcon-extract --describe returned nothing.")

  lst_ <- jsonlite::fromJSON(paste(raw_, collapse = "\n"), simplifyVector = FALSE)

  out_ <- lst_ |>
    purrr::keep(\(.d) isTRUE(.d$available)) |>
    purrr::map(\(.d) tibble::tibble(
      Module   = .d$module,
      Engine   = .d$engine,
      Model    = .d$model,
      Labels   = list(unlist(.d$labels)),
      Extras   = list(unlist(.d$extras)),
      SpecHash = .d$spec_hash %||% NA_character_,
      Ready    = isTRUE(.d$ready)
    )) |>
    purrr::list_rbind()

  if (nrow(out_) == 0L) cli::cli_abort("matcon-extract reports no installed extractors.")
  out_
}

#' Which spaCy models are actually installed
#'
#' A MODEL NAME IS NOT A MODEL. Declaring four and having three is not an error anyone notices until
#' the fourth is loaded, which on this pipeline is after the other three have finished -- and the
#' third of them is the slow one. Asking the suite costs one subprocess and about a second.
#'
#' This narrows the asymmetry between our suite and the third-party ones. matcon-extract describes
#' itself completely; spaCy cannot report which labels it will emit, but it can certainly report
#' which models exist, and a declaration that can be checked should be.
#'
#' @return Character vector of installed model names, possibly empty.
ner_spacy_installed <- function() {
  if (FALSE) {
    # no arguments
  }

  raw_ <- suppressWarnings(system2(
    .ner_python("spacy"),
    c("-c", shQuote("import spacy, json; print(json.dumps(sorted(spacy.util.get_installed_models())))")),
    stdout = TRUE, stderr = FALSE
  ))
  if (length(raw_) == 0L || !is.null(attr(raw_, "status"))) {
    cli::cli_warn("Could not ask the spaCy suite which models it has; assuming none.")
    return(character(0))
  }
  as.character(jsonlite::fromJSON(paste(raw_, collapse = "")))
}

#' What spaCy and LexNLP produce, declared because they cannot say
#'
#' `Model` for spaCy is the model NAME, which is genuinely its version: en_core_web_lg and
#' en_core_web_trf are different models, not different runs of one. LexNLP has a single version and
#' it is the container's, so its model tag repeats its engine name.
#'
#' These label sets are checked against what arrives -- see ner_describe_verify(). A declaration
#' nobody checks is how the previous design failed.
#'
#' @param .spacy_models spaCy model names to describe.
#' @return Tibble: Suite, Engine, Model, Labels (list).
ner_declare_third_party <- function(.spacy_models = c("en_core_web_sm", "en_core_web_md",
                                                      "en_core_web_lg", "en_core_web_trf")) {
  if (FALSE) {
    .spacy_models <- c("en_core_web_lg", "en_core_web_trf")
  }

  # spaCy's native tags are mapped to the cross-engine vocabulary inside extract_spacy.py. ORG,
  # PERSON and GPE are the three that survive that mapping and that any consumer here reads.
  #
  # READY IS CHECKED, NOT ASSERTED. A declared model that is not installed fails at load time, which
  # on this pipeline is after every faster combination has already run.
  have_  <- ner_spacy_installed()
  spacy_ <- tibble::tibble(
    Suite  = "spacy",
    Engine = "spacy",
    Model  = .spacy_models,
    Labels = list(c("ORG", "PERSON", "GPE")),
    Ready  = .spacy_models %in% have_
  )

  # GPE is present but disabled by policy in some configurations; it is listed because the engine
  # CAN produce it, and what is actually requested is the caller's choice.
  # The container either exists or docker fails loudly at the first call, so there is nothing here
  # that a check could establish earlier than the run itself.
  lexnlp_ <- tibble::tibble(
    Suite  = "lexnlp",
    Engine = "lexnlp",
    Model  = "lexnlp",
    Labels = list(c("ORG", "GPE", "DATE", "MONEY")),
    Ready  = TRUE
  )

  dplyr::bind_rows(spacy_, lexnlp_)
}

#' Everything all three suites can produce, in one table
#'
#' The ragged grid, written out: not every engine produces every label, and the shape of that
#' raggedness is what 04A's comparison sections filter on. A label with one producer has no
#' cross-engine agreement to report, and saying so once beats emitting empty panels.
#'
#' @param .spacy_models spaCy model names to include.
#' @return Tibble: Suite, Engine, Model, Labels (list), SpecHash, Ready.
ner_describe <- function(.spacy_models = c("en_core_web_sm", "en_core_web_md",
                                           "en_core_web_lg", "en_core_web_trf")) {
  if (FALSE) {
    .spacy_models <- c("en_core_web_lg", "en_core_web_trf")
  }

  matcon_ <- ner_matcon_describe() |>
    dplyr::transmute(
      Suite  = "matcon",
      .data$Engine,
      .data$Model,
      .data$Labels,
      .data$SpecHash,
      .data$Ready
    )

  third_ <- ner_declare_third_party(.spacy_models = .spacy_models) |>
    dplyr::mutate(SpecHash = NA_character_)

  dplyr::bind_rows(matcon_, third_) |>
    dplyr::arrange(.data$Suite, .data$Model)
}

#' What can actually be run, with a loud account of what cannot
#'
#' Ready is FALSE for two reasons and both are recoverable: a spaCy model that is declared but not
#' downloaded, or a matcon extractor whose data dependency is absent. Neither is a reason to stop --
#' the remaining combinations are still worth running and the report still says what was compared --
#' but neither should pass silently either, because a comparison across three models where four were
#' intended is a different result and nothing else would record the difference.
#'
#' @param .describe Output of ner_describe().
#' @return The Ready rows, invisibly, after reporting the others.
ner_runnable <- function(.describe) {
  if (FALSE) {
    .describe <- ner_describe()
  }

  out_ <- dplyr::filter(.describe, .data$Ready)
  bad_ <- dplyr::filter(.describe, !.data$Ready)

  if (nrow(bad_) > 0L) {
    tbl_say(
      .tab   = dplyr::select(bad_, "Suite", "Model"),
      .title = "NOT AVAILABLE -- excluded from every section below"
    )
    spacy_ <- dplyr::filter(bad_, .data$Suite == "spacy")$Model
    if (length(spacy_) > 0L) {
      cli::cli_alert_warning(
        "Install with: contracts-spacy/.venv/bin/python -m spacy download
         {paste(spacy_, collapse = ' && contracts-spacy/.venv/bin/python -m spacy download ')}"
      )
    }
    mat_ <- dplyr::filter(bad_, .data$Suite == "matcon")$Model
    if (length(mat_) > 0L) {
      cli::cli_alert_warning(
        "Data dependency absent for {paste(mat_, collapse = ', ')}; see matcon-extract --version."
      )
    }
  }

  if (nrow(out_) == 0L) cli::cli_abort("No combination is runnable.")
  invisible(out_)
}

#' One row per (Engine, Model, Label): the grid a run actually covers
#'
#' Unnests ner_describe() and keeps only the labels a caller asked for. This is what the ledger is
#' queried against and what the manifest fingerprints, so a document processed for ORG and not for
#' GPE is two rows with two independent outcomes rather than one ambiguous one.
#'
#' @param .describe Output of ner_describe().
#' @param .labels Labels to keep. NULL keeps everything each engine offers.
#' @return Tibble: Suite, Engine, Model, Label.
ner_grid <- function(.describe, .labels = NULL) {
  if (FALSE) {
    .describe <- ner_describe()
    .labels   <- c("ORG", "GPE", "DATE", "TERM", "MONEY", "REDACT")
  }

  out_ <- .describe |>
    dplyr::select("Suite", "Engine", "Model", "Labels") |>
    tidyr::unnest_longer(col = "Labels", values_to = "Label")

  if (!is.null(.labels)) out_ <- dplyr::filter(out_, .data$Label %in% .labels)

  out_ |>
    dplyr::arrange(.data$Suite, .data$Model, .data$Label) |>
    dplyr::distinct()
}

#' How many engines produce each label
#'
#' 04A is built on cross-engine comparison, and after the matcon rewrite two labels have exactly one
#' producer: REDACT and TERM. The agreement, alignment and contrast arms have nothing to say about
#' either, so they are excluded by construction rather than by an empty result.
#'
#' @param .grid Output of ner_grid().
#' @return Tibble: Label, NEngine, NModel, Engines, Comparable.
ner_producers <- function(.grid) {
  if (FALSE) {
    .grid <- ner_grid(.describe = ner_describe())
  }

  .grid |>
    dplyr::summarise(
      NEngine = dplyr::n_distinct(.data$Engine),
      NModel  = dplyr::n_distinct(.data$Model),
      Engines = paste(sort(unique(.data$Engine)), collapse = ", "),
      .by     = "Label"
    ) |>
    dplyr::mutate(Comparable = .data$NEngine > 1L) |>
    dplyr::arrange(dplyr::desc(.data$NEngine), .data$Label)
}

#' Assert that what arrived matches what was declared or described
#'
#' THE CHECK THE PREVIOUS DESIGN LACKED. A declaration nobody verifies is a comment that happens to
#' be executable. Two things are compared:
#'
#' The LABEL SET. Every label present in a staged parquet must appear in the grid for that
#' (Engine, Model). A label outside it has no ledger entry, so it is invisible to every completeness
#' check downstream -- it would be ingested, stored, and then never counted as done.
#'
#' The SPEC HASH, for matcon only. The stamped hash must equal the one recorded when the pass was
#' planned. A difference means the rules changed between planning and running, which on a corpus
#' pass is hours of extraction attributed to code that no longer exists.
#'
#' @param .paths Staged parquet paths to verify.
#' @param .grid Output of ner_grid().
#' @param .describe Output of ner_describe(), for the spec-hash comparison.
#' @return The offending rows, invisibly; empty means a pass.
ner_describe_verify <- function(.paths, .grid, .describe = NULL) {
  if (FALSE) {
    .paths    <- fs::dir_ls(here::here("2_output", "04A-EntityExtract", "Stage"), glob = "*.parquet")
    .grid     <- ner_grid(.describe = ner_describe())
    .describe <- ner_describe()
  }

  seen_ <- .paths |>
    purrr::map(\(.p) {
      arrow::read_parquet(.p, col_select = c("Engine", "Model", "Label")) |>
        dplyr::filter(!is.na(.data$Label)) |>
        dplyr::distinct()
    }) |>
    purrr::list_rbind()

  bad_ <- dplyr::anti_join(seen_, .grid, by = dplyr::join_by(Engine, Model, Label))

  if (nrow(bad_) > 0L) {
    cli::cli_abort(c(
      "{nrow(bad_)} (Engine, Model, Label) combination{?s} arrived that the grid does not contain.",
      "i" = "Rows for a label outside the grid have no ledger entry and are invisible to every
             completeness check downstream.",
      "x" = "{paste(bad_$Engine, bad_$Model, bad_$Label, collapse = ' | ')}"
    ))
  }

  invisible(bad_)
}

#' Report the grid and its raggedness
#'
#' @param .describe Output of ner_describe().
#' @param .labels Labels to keep. NULL keeps everything.
#' @return The grid, invisibly.
ner_report_describe <- function(.describe, .labels = NULL) {
  if (FALSE) {
    .describe <- ner_describe()
    .labels   <- NULL
  }

  grid_ <- ner_grid(.describe = .describe, .labels = .labels)
  prod_ <- ner_producers(.grid = grid_)

  tbl_say(
    .tab   = dplyr::mutate(.describe, Labels = purrr::map_chr(.data$Labels, paste, collapse = ", ")),
    .title = "Suites, models and what each stamps"
  )
  tbl_say(.tab = prod_, .title = "Producers per label")

  single_ <- dplyr::filter(prod_, !.data$Comparable)$Label
  if (length(single_) > 0L) {
    cli::cli_alert_info(
      "Single-producer label{?s}: {paste(single_, collapse = ', ')}. Yield and offset checks apply;
       cross-engine agreement does not and is skipped rather than reported empty."
    )
  }

  invisible(grid_)
}
# 3. The store ---------------------------------------------------------------------------------------------------------
#
# ONE DuckDB, ONE TABLE PER LABEL. The alternative arrangements were considered and rejected for the
# same reason: the labels do not partition by suite. DATE comes from LexNLP and from matcon, GPE from
# both plus spaCy, ORG from LexNLP and spaCy. A store split by suite puts the SAME LABEL in different
# databases, which is the one arrangement that makes engine comparison expensive -- and comparison is
# what 04A exists to do.
#
# WHY PER-LABEL TABLES RATHER THAN ONE FLAT ONE. Extras differ by label and not by engine: a
# LegalForm belongs to ORG whoever found it, an Amount to MONEY. A flat table carries every extra on
# every row and is mostly null; per-label tables carry each extra exactly where it means something.
# Extras are SELECTED rather than required, so an engine that omits one ingests cleanly and reads
# NULL -- which is how spaCy's core-only output and LexNLP's seventeen columns go through one path.
#
# WHERE TWO ENGINES SHARE AN EXTRA THEY SHARE THE COLUMN. matcon's gazetteer and LexNLP both emit
# Iso2 in the US-MN form -- the geo lookup was built that way BECAUSE LexNLP emits it. One column
# means "do these two engines resolve the same place" is a single query rather than a join across
# schemas.


#: Core columns, in this order, on every table. What every consumer reads and what routing depends on.
.store_core <- c("DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model")

#' The per-label schema
#'
#' Each entry names one label's table and the extra columns it carries beyond the core. A label
#' absent from this list cannot be ingested, which is deliberate: a new label is a schema decision
#' and should be made here rather than by whatever parquet arrives first.
#'
#' AMOUNT IS DECIMAL, NOT DOUBLE. moneyregex crosses the seam as text specifically so a contract
#' value survives intact, and casting to a float one layer later would undo that. Measured on the
#' sample, none of 36,647 amounts exceeds fifteen significant digits, so DOUBLE would be lossless
#' today -- but 6,254 carry decimals and 952 are sub-cent, and making the two layers agree costs
#' nothing.
.store_schema <- list(
  org = list(
    Table  = "org",
    Extras = c(LegalForm = "VARCHAR", LegalFormFull = "VARCHAR", Description = "VARCHAR")
  ),
  person = list(
    Table  = "person",
    Extras = character(0)
  ),
  gpe = list(
    Table  = "gpe",
    # Iso2 and Iso3 are SHARED between matcon and LexNLP. The rest split by engine and read NULL for
    # the other, which is what per-label extras are for.
    Extras = c(
      Iso2 = "VARCHAR", Iso3 = "VARCHAR",                    # both engines
      GeoKey = "VARCHAR", IsWord = "INTEGER",                # matcon only
      NParent = "INTEGER", MatchKind = "VARCHAR",            # matcon only
      GeoName = "VARCHAR", GeoAlias = "VARCHAR",             # lexnlp only
      GeoCategory = "VARCHAR", GeoId = "VARCHAR"             # lexnlp only
    )
  ),
  date = list(
    Table  = "date",
    Extras = c(DateValue = "VARCHAR", DateScore = "DOUBLE")
  ),
  term = list(
    Table  = "term",
    Extras = c(TermN = "DOUBLE", TermUnit = "VARCHAR", TermYears = "DOUBLE")
  ),
  money = list(
    Table  = "money",
    Extras = c(Amount = "DECIMAL(28,4)", Currency = "VARCHAR")
  ),
  redact = list(
    Table  = "redact",
    Extras = character(0)
  )
)

#' Which engine fills which extras, for the per-suite views
#'
#' Presentation only: the underlying table stays unified. A reader of gpe_matcon sees six columns
#' that mean something rather than ten of which four are always NULL, and `SELECT ... FROM gpe`
#' still compares every engine in one query.
.store_view_cols <- list(
  gpe = list(
    matcon = c("Iso2", "Iso3", "GeoKey", "IsWord", "NParent", "MatchKind"),
    lexnlp = c("Iso2", "Iso3", "GeoName", "GeoAlias", "GeoCategory", "GeoId"),
    spacy  = character(0)
  )
)

#' Open the candidate store
#'
#' @param .db_path Path to the DuckDB file.
#' @param .read_only Open read-only. A comparison must not be able to modify what it compares against.
#' @return A DBI connection.
ner_db_connect <- function(.db_path, .read_only = FALSE) {
  if (FALSE) {
    .db_path   <- here::here("2_output", "04A-EntityExtract", "Store", "EntityCandidates.duckdb")
    .read_only <- TRUE
  }
  fs::dir_create(fs::path_dir(.db_path))
  DBI::dbConnect(duckdb::duckdb(dbdir = as.character(.db_path), read_only = .read_only))
}

#' Create the store: seven label tables, the ledger, the benchmark log, and the views
#'
#' Idempotent. Every statement is CREATE ... IF NOT EXISTS, so calling it on an existing store is a
#' no-op and calling it on a fresh one builds everything.
#'
#' THE LEDGER IS SEPARATE FROM THE SPANS, and that is the point of it. A document that was processed
#' and matched nothing has no row in any label table, so without a ledger it is indistinguishable
#' from one that was never processed -- and the orchestrator would re-extract it forever while
#' believing it complete.
#'
#' @param .con Connection from ner_db_connect().
#' @return .con, invisibly.
ner_db_init <- function(.con) {
  if (FALSE) {
    .con <- ner_db_connect(.db_path = tempfile(fileext = ".duckdb"))
  }

  core_sql_ <- paste(
    "DocID VARCHAR NOT NULL", "Start BIGINT", "Stop BIGINT", "Span VARCHAR",
    "Label VARCHAR", "LabelRaw VARCHAR", "Engine VARCHAR NOT NULL", "Model VARCHAR NOT NULL",
    sep = ", "
  )

  for (spec_ in .store_schema) {
    extra_sql_ <- if (length(spec_$Extras) == 0L) {
      ""
    } else {
      paste0(", ", paste(names(spec_$Extras), unname(spec_$Extras), collapse = ", "))
    }
    DBI::dbExecute(.con, glue::glue(
      "CREATE TABLE IF NOT EXISTS {spec_$Table} ({core_sql_}{extra_sql_})"
    ))
  }

  # FOUR STATES, NOT TWO. "error" is new and it closes a real gap: an extractor that crashed on a
  # document used to fall through to a bare sentinel, indistinguishable from a clean miss. A pattern
  # failing on a whole class of documents then looked exactly like that class having no matches.
  DBI::dbExecute(.con, "
    CREATE TABLE IF NOT EXISTS runs (
      DocID   VARCHAR NOT NULL,
      Engine  VARCHAR NOT NULL,
      Model   VARCHAR NOT NULL,
      Label   VARCHAR NOT NULL,
      Status  VARCHAR NOT NULL,   -- hit | nohit | timeout | error
      RunAt   TIMESTAMP NOT NULL,
      PRIMARY KEY (DocID, Engine, Model, Label)
    )")

  DBI::dbExecute(.con, "
    CREATE TABLE IF NOT EXISTS bench (
      Suite    VARCHAR NOT NULL,
      Model    VARCHAR NOT NULL,
      Labels   VARCHAR NOT NULL,
      Batch    INTEGER NOT NULL,
      Workers  INTEGER NOT NULL,
      NDoc     INTEGER NOT NULL,
      Machine  VARCHAR NOT NULL,
      Seconds  DOUBLE  NOT NULL,
      DocPerS  DOUBLE  NOT NULL,
      RunAt    TIMESTAMP NOT NULL,
      PRIMARY KEY (Suite, Model, Labels, Batch, Workers, NDoc, Machine)
    )")

  ner_db_views(.con = .con)
  invisible(.con)
}

#' Build the union view and the per-suite views
#'
#' `candidates` unions the seven label tables on the core columns, so "every span this store holds"
#' is one query regardless of how many labels exist.
#'
#' NOTE FOR ANYTHING THAT ENUMERATES TABLES: DBI::dbListTables() returns views alongside tables, and
#' rebuilding a view from a list that already contains it produces a self-referential definition that
#' fails only when queried. Filter on duckdb_views() rather than trusting the table list.
#'
#' @param .con Connection.
#' @return .con, invisibly.
ner_db_views <- function(.con) {
  if (FALSE) {
    .con <- ner_db_connect(.db_path = tempfile(fileext = ".duckdb"))
  }

  sel_ <- purrr::map_chr(
    .store_schema,
    \(.s) glue::glue("SELECT {paste(.store_core, collapse = ', ')} FROM {.s$Table}")
  )
  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE VIEW candidates AS ", paste(sel_, collapse = " UNION ALL ")
  ))

  for (label_ in names(.store_view_cols)) {
    tbl_ <- .store_schema[[label_]]$Table
    for (engine_ in names(.store_view_cols[[label_]])) {
      cols_ <- .store_view_cols[[label_]][[engine_]]
      all_  <- c(.store_core, cols_)
      DBI::dbExecute(.con, glue::glue(
        "CREATE OR REPLACE VIEW {tbl_}_{engine_} AS
         SELECT {paste(all_, collapse = ', ')} FROM {tbl_} WHERE Engine = '{engine_}'"
      ))
    }
  }
  invisible(.con)
}

#' Ingest one staged parquet
#'
#' ONE (Engine, Model) PER FILE, ASSERTED. A staging file carrying two would attribute spans to a
#' method that never saw the document, and nothing downstream could detect it -- the rows are well
#' formed and the ledger is satisfied. This is why matcon writes one parquet per model rather than
#' one per run.
#'
#' Delete-then-insert per (Engine, Model, Label): re-running one combination replaces exactly its own
#' rows and cannot touch another engine's. That is the isolation a per-suite store was proposed to
#' buy, and it is already here.
#'
#' @param .con Connection.
#' @param .path Staged parquet.
#' @param .labels Labels that were REQUESTED. Rows outside this set are refused rather than stored:
#'   the ledger records what was asked for, so a row for an unrequested label has no ledger entry and
#'   is invisible to every completeness check downstream.
#' @return Tibble of rows written per label, invisibly.
ner_db_append <- function(.con, .path, .labels) {
  if (FALSE) {
    .con    <- ner_db_connect(.db_path = tempfile(fileext = ".duckdb"))
    .path   <- here::here("2_output", "04A-EntityExtract", "Stage", "matcon__dateregex-v3.parquet")
    .labels <- c("DATE", "TERM")
  }

  tab_ <- arrow::read_parquet(.path)

  stamp_ <- dplyr::distinct(tab_, .data$Engine, .data$Model)
  if (nrow(stamp_) != 1L) {
    cli::cli_abort(c(
      "{fs::path_file(.path)} carries {nrow(stamp_)} (Engine, Model) pair(s); exactly one is required.",
      "i" = "A file carrying two would attribute spans to a method that never saw the document."
    ))
  }
  engine_ <- stamp_$Engine[[1L]]
  model_  <- stamp_$Model[[1L]]

  seen_ <- setdiff(unique(stats::na.omit(tab_$Label)), .labels)
  if (length(seen_) > 0L) {
    cli::cli_abort(c(
      "{fs::path_file(.path)} carries label{?s} that were not requested: {paste(seen_, collapse = ', ')}.",
      "i" = "The ledger records what was requested; unrequested rows would never be counted as done."
    ))
  }

  out_ <- tibble::tibble(Label = character(0), NRow = integer(0))

  for (label_ in intersect(.labels, names(.store_schema))) {
    spec_ <- .store_schema[[label_]]
    keep_ <- c(.store_core, intersect(names(spec_$Extras), names(tab_)))
    part_ <- tab_ |>
      dplyr::filter(.data$Label == label_) |>
      dplyr::select(dplyr::all_of(keep_))

    DBI::dbExecute(.con, glue::glue(
      "DELETE FROM {spec_$Table} WHERE Engine = '{engine_}' AND Model = '{model_}'"
    ))
    if (nrow(part_) > 0L) DBI::dbAppendTable(.con, spec_$Table, as.data.frame(part_))

    out_ <- dplyr::bind_rows(out_, tibble::tibble(Label = label_, NRow = nrow(part_)))
  }

  ner_db_ledger(.con = .con, .tab = tab_, .engine = engine_, .model = model_, .labels = .labels)
  invisible(out_)
}


# 4. The ledger --------------------------------------------------------------------------------------------------------

#' Record an outcome for every (document, label) a run covered
#'
#' FOUR STATES. A document appears once per requested label with exactly one of:
#'
#'   hit      at least one span for that label
#'   nohit    processed, found nothing -- NOT the same as never processed
#'   timeout  cut off by the per-document cap
#'   error    the extractor raised
#'
#' The last two are read from the sentinel row's LabelRaw, which the Python side stamps as
#' "timeout:<name>" or "error:<name>". Before the error state existed, a crash produced a bare
#' sentinel and was recorded as nohit -- so a pattern failing on a class of documents was
#' indistinguishable from that class having no matches.
#'
#' @param .con Connection.
#' @param .tab The staged parquet, already read.
#' @param .engine,.model The stamp, already verified as unique.
#' @param .labels Labels requested.
#' @return Rows written, invisibly.
ner_db_ledger <- function(.con, .tab, .engine, .model, .labels) {
  if (FALSE) {
    .con    <- ner_db_connect(.db_path = tempfile(fileext = ".duckdb"))
    .tab    <- arrow::read_parquet(here::here("2_output", "04A-EntityExtract", "Stage",
                                              "matcon__dateregex-v3.parquet"))
    .engine <- "matcon"
    .model  <- "dateregex-v3"
    .labels <- c("DATE", "TERM")
  }

  docs_ <- unique(.tab$DocID)

  fail_ <- .tab |>
    dplyr::filter(is.na(.data$Start), !is.na(.data$LabelRaw)) |>
    dplyr::mutate(Fail = dplyr::if_else(
      stringi::stri_startswith_fixed(.data$LabelRaw, "timeout:"), "timeout", "error"
    )) |>
    dplyr::select("DocID", "Fail") |>
    dplyr::distinct()

  hit_ <- .tab |>
    dplyr::filter(!is.na(.data$Start)) |>
    dplyr::distinct(.data$DocID, .data$Label)

  led_ <- tidyr::expand_grid(DocID = docs_, Label = .labels) |>
    dplyr::left_join(dplyr::mutate(hit_, Hit = TRUE), by = dplyr::join_by(DocID, Label)) |>
    dplyr::left_join(fail_, by = dplyr::join_by(DocID)) |>
    dplyr::transmute(
      .data$DocID,
      Engine = .engine,
      Model  = .model,
      .data$Label,
      Status = dplyr::case_when(
        !is.na(.data$Fail) ~ .data$Fail,
        isTRUE(.data$Hit)  ~ "hit",
        .data$Hit %in% TRUE ~ "hit",
        .default = "nohit"
      ),
      RunAt = Sys.time()
    )

  DBI::dbExecute(.con, glue::glue(
    "DELETE FROM runs WHERE Engine = '{.engine}' AND Model = '{.model}'
       AND Label IN ({paste0(\"'\", .labels, \"'\", collapse = ', ')})"
  ))
  DBI::dbAppendTable(.con, "runs", as.data.frame(led_))
  invisible(nrow(led_))
}

#' Which documents a combination has not seen
#'
#' The question every pass asks before it stages anything. Answered from one table for every engine,
#' which is what a store split by suite would have cost.
#'
#' @param .con Connection.
#' @param .doc_ids Candidate documents.
#' @param .engine,.model,.label The combination.
#' @return Character vector of DocIDs with no ledger entry.
ner_db_missing <- function(.con, .doc_ids, .engine, .model, .label) {
  if (FALSE) {
    .con     <- ner_db_connect(.db_path = tempfile(fileext = ".duckdb"))
    .doc_ids <- c("A", "B")
    .engine  <- "matcon"
    .model   <- "dateregex-v3"
    .label   <- "DATE"
  }

  done_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT DISTINCT DocID FROM runs
      WHERE Engine = '{.engine}' AND Model = '{.model}' AND Label = '{.label}'"
  ))$DocID
  setdiff(.doc_ids, done_)
}

#' Remove one combination entirely
#'
#' Spans and ledger together. Removing one without the other leaves a store that reports work done
#' and holds none of it, or holds rows nothing knows about.
#'
#' @param .con Connection.
#' @param .engine,.model The combination.
#' @param .labels Labels to clear. NULL clears all of them.
#' @return Rows removed, invisibly.
ner_db_clear <- function(.con, .engine, .model, .labels = NULL) {
  if (FALSE) {
    .con    <- ner_db_connect(.db_path = tempfile(fileext = ".duckdb"))
    .engine <- "matcon"
    .model  <- "dateregex-v2"
    .labels <- NULL
  }

  n_ <- 0L
  for (label_ in names(.store_schema)) {
    if (!is.null(.labels) && !(toupper(label_) %in% toupper(.labels))) next
    n_ <- n_ + DBI::dbExecute(.con, glue::glue(
      "DELETE FROM {.store_schema[[label_]]$Table} WHERE Engine = '{.engine}' AND Model = '{.model}'"
    ))
  }
  lab_sql_ <- if (is.null(.labels)) "" else
    glue::glue(" AND Label IN ({paste0(\"'\", .labels, \"'\", collapse = ', ')})")
  n_ <- n_ + DBI::dbExecute(.con, glue::glue(
    "DELETE FROM runs WHERE Engine = '{.engine}' AND Model = '{.model}'{lab_sql_}"
  ))
  invisible(n_)
}

#' What the store holds, by combination and label
#'
#' @param .con Connection.
#' @return Tibble: Engine, Model, Label, Status counts, Docs, Spans.
ner_db_summary <- function(.con) {
  if (FALSE) {
    .con <- ner_db_connect(.db_path = tempfile(fileext = ".duckdb"), .read_only = TRUE)
  }

  led_ <- DBI::dbGetQuery(.con, "
    SELECT Engine, Model, Label, Status, COUNT(*) AS N
    FROM runs GROUP BY Engine, Model, Label, Status") |>
    tibble::as_tibble() |>
    tidyr::pivot_wider(names_from = "Status", values_from = "N", values_fill = 0L)

  spans_ <- DBI::dbGetQuery(.con, "
    SELECT Engine, Model, Label, COUNT(*) AS Spans, COUNT(DISTINCT DocID) AS SpanDocs
    FROM candidates WHERE Start IS NOT NULL
    GROUP BY Engine, Model, Label") |>
    tibble::as_tibble()

  led_ |>
    dplyr::left_join(spans_, by = dplyr::join_by(Engine, Model, Label)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(.x) tidyr::replace_na(.x, 0L))) |>
    dplyr::arrange(.data$Engine, .data$Model, .data$Label)
}

#' Warn when the LexNLP image no longer matches its sources
#'
#' Delegates to contracts-lexnlp/image_spec.py, which hashes the Dockerfile, the extractor, the legal
#' form vocabulary and the geo entity table, and compares against the LABEL stamped at build time.
#'
#' A STALE IMAGE FAILS SILENTLY. The container runs, the parquet is well formed, and the rows are
#' wrong about their own provenance -- attributed in the store to code that has changed since.
#'
#' @param .image Image name.
#' @param .abort Abort rather than warn.
#' @return TRUE when current, FALSE otherwise, invisibly.
ner_lexnlp_check_image <- function(.image = "contracts-lexnlp", .abort = FALSE) {
  if (FALSE) {
    .image <- "contracts-lexnlp"
    .abort <- FALSE
  }

  script_ <- fs::path(.ner_suite_dir("lexnlp"), "image_spec.py")
  if (!fs::file_exists(script_)) {
    cli::cli_warn("image_spec.py not found; cannot verify {(.image)}.")
    return(invisible(FALSE))
  }
  # CAPTURED, NOT DISCARDED. An exit code alone cannot distinguish a stale image from an
  # environment where docker is not on PATH -- and an R session started from the GUI routinely has a
  # narrower PATH than a login shell, so the second is the likelier explanation for a check that
  # fails here and passes in a terminal. Reporting the reason costs nothing and stops a false alarm
  # from being read as a real one.
  out_    <- suppressWarnings(system2("python3", c(script_, "--check"),
                                      stdout = TRUE, stderr = TRUE))
  status_ <- attr(out_, "status") %||% 0L
  ok_     <- identical(as.integer(status_), 0L)
  said_   <- paste(out_, collapse = " ")

  if (!ok_) {
    unknown_ <- stringi::stri_detect_fixed(said_, "no image, no docker")
    msg_ <- if (unknown_) c(
      "Could not verify {(.image)}: {(said_)}",
      "i" = "Most often docker is absent from this session's PATH rather than the image being
             stale. Run contracts-lexnlp/image_spec.py --check in a terminal to tell which."
    ) else c(
      "{(.image)} does not match its sources: {(said_)}",
      "i" = "Run contracts-lexnlp/rebuild_lexnlp.sh. Extraction from a stale image is attributed
             in the store to code that has changed since."
    )
    if (.abort) cli::cli_abort(msg_) else cli::cli_warn(msg_)
  }
  invisible(ok_)
}


# 5. Benchmarks --------------------------------------------------------------------------------------------------------
#
# A LEDGER, NOT A SWITCH. Every render reports the whole grid and measures only the cells not already
# stored, so the first render pays the full cost and later ones cost seconds. Nothing is skippable
# and nothing is switched off -- which is the difference between this and an eval: false, and the
# reason the document can carry an expensive measurement without becoming unrenderable.
#
# MACHINE IS IN THE KEY. A timing from a different box is a different measurement, and without it a
# laptop run would silently overwrite the Mac Studio numbers. Re-measuring means deleting rows, which
# is an action rather than a setting.

#' This machine, as a benchmark key
#'
#' @return Short string identifying the host and its core count.
ner_machine <- function() {
  if (FALSE) {
    # no arguments
  }
  paste0(Sys.info()[["sysname"]], "-", Sys.info()[["machine"]], "-", parallel::detectCores(), "c")
}

#' Measure one benchmark cell, or read it from the store
#'
#' Runs the suite, times it, discards the output. Extraction and ingest are separate functions
#' precisely so this can exist: a benchmark that wrote to the store would pollute what it measures.
#'
#' @param .con Connection.
#' @param .suite One of "spacy", "lexnlp", "matcon".
#' @param .path_in Staged parquet to run over.
#' @param .n_doc Documents in that parquet, for the key and the rate.
#' @param .model Model name, or NULL.
#' @param .labels Labels to request.
#' @param .batch Batch size to test.
#' @param .workers Worker count to test.
#' @param .machine Machine key.
#' @return One-row tibble, invisibly.
ner_bench_cell <- function(.con, .suite, .path_in, .n_doc, .model = NULL,
                           .labels, .batch, .workers, .machine = ner_machine()) {
  if (FALSE) {
    .con     <- ner_db_connect(.db_path = tempfile(fileext = ".duckdb"))
    .suite   <- "matcon"
    .path_in <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")
    .n_doc   <- 4398L
    .model   <- NULL
    .labels  <- c("DATE", "TERM")
    .batch   <- 32L
    .workers <- 10L
    .machine <- ner_machine()
  }

  model_key_ <- .model %||% .suite
  lab_key_   <- paste(sort(.labels), collapse = "+")

  hit_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT * FROM bench
      WHERE Suite = '{.suite}' AND Model = '{model_key_}' AND Labels = '{lab_key_}'
        AND Batch = {.batch} AND Workers = {.workers} AND NDoc = {.n_doc}
        AND Machine = '{.machine}'"
  ))
  if (nrow(hit_) == 1L) return(invisible(tibble::as_tibble(hit_)))

  tmp_ <- fs::path(tempdir(), paste0("bench-", model_key_, "-", .batch, "-", .workers))
  fs::dir_create(tmp_)
  on.exit(fs::dir_delete(tmp_), add = TRUE)

  fun_ <- switch(.suite, spacy = ner_spacy, lexnlp = ner_lexnlp, matcon = ner_matcon)
  out_ <- fun_(
    .path_in    = .path_in,
    .out_dir    = tmp_,
    .model      = .model,
    .labels     = .labels,
    .n_process  = .workers,
    .batch_size = .batch,
    .timeout    = .ner_timeout,
    .quiet      = TRUE
  )
  secs_ <- attr(out_, "elapsed")

  row_ <- tibble::tibble(
    Suite = .suite, Model = model_key_, Labels = lab_key_,
    Batch = as.integer(.batch), Workers = as.integer(.workers), NDoc = as.integer(.n_doc),
    Machine = .machine, Seconds = secs_, DocPerS = .n_doc / secs_, RunAt = Sys.time()
  )
  DBI::dbAppendTable(.con, "bench", as.data.frame(row_))
  invisible(row_)
}

#' Run a benchmark grid, measuring only what is missing
#'
#' @param .con Connection.
#' @param .spec Tibble with Suite, Model, Labels (list) -- the combinations to time.
#' @param .path_in Staged parquet.
#' @param .n_doc Documents in it.
#' @param .batches Batch sizes to sweep.
#' @param .workers Worker counts to sweep.
#' @return The full grid for this machine, invisibly.
ner_bench_run <- function(.con, .spec, .path_in, .n_doc,
                          .batches = c(8L, 32L, 128L), .workers = c(10L)) {
  if (FALSE) {
    .con     <- ner_db_connect(.db_path = tempfile(fileext = ".duckdb"))
    .spec    <- ner_describe() |> dplyr::select("Suite", "Model", "Labels")
    .path_in <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")
    .n_doc   <- 4398L
    .batches <- c(8L, 32L, 128L)
    .workers <- c(10L)
  }

  grid_ <- tidyr::expand_grid(
    dplyr::mutate(.spec, .row = dplyr::row_number()),
    Batch   = .batches,
    Workers = .workers
  )

  cli::cli_alert_info("Benchmark grid: {nrow(grid_)} cell{?s}. Cells already stored are not re-run.")

  purrr::pwalk(
    list(grid_$Suite, grid_$Model, grid_$Labels, grid_$Batch, grid_$Workers),
    \(.s, .m, .l, .b, .w) ner_bench_cell(
      .con     = .con,
      .suite   = .s,
      .path_in = .path_in,
      .n_doc   = .n_doc,
      .model   = if (.s == "spacy") .m else NULL,
      .labels  = .l,
      .batch   = .b,
      .workers = .w
    )
  )

  out_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT * FROM bench WHERE Machine = '{ner_machine()}' ORDER BY Suite, Model, Batch, Workers"
  ))
  invisible(tibble::as_tibble(out_))
}
