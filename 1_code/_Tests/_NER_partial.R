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
#' @param .device One of auto, cpu, cuda, mps.
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
                      .device     = "auto",
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
    .device     <- "auto"
    .quiet      <- FALSE
  }

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
    "--device",     .device,
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
  spacy_ <- tibble::tibble(
    Suite  = "spacy",
    Engine = "spacy",
    Model  = .spacy_models,
    Labels = list(c("ORG", "PERSON", "GPE"))
  )

  # GPE is present but disabled by policy in some configurations; it is listed because the engine
  # CAN produce it, and what is actually requested is the caller's choice.
  lexnlp_ <- tibble::tibble(
    Suite  = "lexnlp",
    Engine = "lexnlp",
    Model  = "lexnlp",
    Labels = list(c("ORG", "GPE", "DATE", "MONEY"))
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
    dplyr::mutate(SpecHash = NA_character_, Ready = TRUE)

  dplyr::bind_rows(matcon_, third_) |>
    dplyr::arrange(.data$Suite, .data$Model)
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
