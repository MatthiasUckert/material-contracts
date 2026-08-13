# _NER.R: the extraction seam every entity engine is called through (ner_*) ---------------------------------------------
#
# Six extractors, one orchestrator, one store. Everything in this file is shared: 04A runs the whole
# engine set over the labelled sample to build the evidence, and 04D runs the surviving subset over
# the corpus. Both call the same wrappers, so the corpus is extracted by the code the sample was
# measured with rather than by a second implementation that agrees at the third decimal.
#
# WHAT DOES NOT LIVE HERE. Anything that reads the finished store to describe or judge it belongs to
# the script asking the question. The overview, agreement and per-class profiling functions moved to
# 04A-EntityExtract.R, which is their only caller: a shared file holding one script's analysis makes
# that script's concerns look like everyone's. The engine seam is genuinely shared; its reporting
# was not.
#
# The interactive HTML span viewer moved to _BackUp/_2026-08-11_NER-HtmlExport.R. It is parked, not
# discarded -- it is the tool for reading what an extractor actually did to a document, and it is
# wanted back once there is a session to spend on it.
#
# CROSS-LANGUAGE BOUNDARY. Every extractor is a command line and a parquet file. Neither side holds
# a handle on the other's session, so a failure surfaces as a non-zero exit status rather than a
# corrupted workspace, and either environment can be rebuilt without disturbing the other. Python
# never touches DuckDB; the store is written from R alone.
#
# OFFSETS. Every candidate carries Start and Stop as code-point indices into the canonical text
# written by 04A. That is the one contract the whole family rests on, and it is why no extractor is
# ever pointed at a normalised or reconstructed copy of a document.


# 1. Extractors --------------------------------------------------------------------------------------------------------
# One wrapper per extractor. Each builds a command line, runs it, and returns the parquet it wrote.
# Optional flags are appended only when they differ from the script's own default, so the command
# records the decisions actually taken rather than the ones that happened to be in force.

#' Run the spaCy extractor over parquet input
#'
#' The wrapper exists so that every spaCy flag is set from R and none is left to the script's own
#' defaults: a run whose label set or truncation was decided inside Python is a run whose store
#' cannot be reproduced from the calling document alone. Optional flags are appended only when they
#' differ from the extractor's default, so the command line records the decisions actually taken.
#'
#' The timeout is a per-window stall guard rather than a per-document one. When no window completes
#' inside it the extractor drops to sequential processing and skips the offending window, naming the
#' document and window on stderr, so a multi-day pass cannot hang silently on one pathological file.
#'
#' @param .inputs Character. Parquet file(s) or folder(s); folders are globbed recursively.
#' @param .output Character. Destination parquet for the candidate spans.
#' @param .id_col Character. Document identifier column in the input.
#' @param .text_col Character. Text column the offsets will index.
#' @param .labels Character or NULL. Entity labels to keep; NULL keeps everything the model emits.
#' @param .max_chars Integer or NULL. Truncate each document to its first N characters before
#'   extraction. NULL is no truncation. Documents still over spaCy's max_length are windowed inside
#'   the extractor either way.
#' @param .timeout Integer. Per-window stall guard in seconds; 0 disables it.
#' @param .model Character. spaCy model name.
#' @param .device Character. auto, cpu, cuda or mps. auto sends CNN models to CPU and the
#'   transformer to the available accelerator.
#' @param .batch_size Integer. Documents per spaCy batch.
#' @param .n_process Integer. Worker processes. The transformer occupies one device and must stay
#'   at one.
#' @param .overwrite Logical. TRUE re-runs even when .output exists.
#' @param .no_progress Logical. Suppress the extractor's own progress bar.
#' @param .engine_dir Character. Root of the contracts-engine package.
#' @param .quiet Logical. Suppress the completion message.
#' @return Invisibly, the output path.
ner_spacy <- function(
    .inputs,
    .output,
    .id_col = "DocID",
    .text_col = "TextRaw",
    .labels = c("ORG", "PERSON", "GPE", "DATE", "MONEY"), # NULL -> omit --label -> keep all
    .max_chars = NULL, # NULL -> omit --max-chars -> no truncation
    .timeout = 600L, # per-window stall guard in seconds (0 = off)
    .model = "en_core_web_sm",
    .device = "auto", # auto|cpu|cuda|mps
    .batch_size = 64L,
    .n_process = 1L,
    .overwrite = FALSE,
    .no_progress = FALSE,
    .engine_dir = here::here("contracts-engine"),
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- fil_sample_dirs$Path[20]
    .output <- file.path(.lP$Cache$NerTest, "test_spacy_sm.parquet")
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .labels <- c("ORG", "PERSON", "GPE", "DATE", "MONEY")
    .max_chars <- NULL
    .timeout <- 600L
    .model <- "en_core_web_sm"
    .device <- "cpu"
    .batch_size <- 64L
    .n_process <- 10L
    .overwrite <- FALSE
    .no_progress <- FALSE
    .engine_dir <- here::here("contracts-engine")
    .quiet <- FALSE
  }

  if (fs::file_exists(.output)) {
    if (isTRUE(.overwrite)) {
      fs::file_delete(.output)
    } else {
      if (!.quiet) cli::cli_alert_info("Output exists, skipping: {.path {(.output)}}")
      return(invisible(.output))
    }
  }

  t0_ <- Sys.time()

  python_ <- fs::path(.engine_dir, ".venv", "bin", "python")
  script_ <- fs::path(.engine_dir, "extract_spacy.py")
  if (!fs::file_exists(python_)) cli::cli_abort("No engine venv at {.path {python_}}.")
  if (!fs::file_exists(script_)) cli::cli_abort("Missing {.path {script_}}.")
  fs::dir_create(fs::path_dir(.output))

  args_ <- c(
    script_, fs::path_abs(.inputs),
    "--output", .output,
    "--id-col", .id_col,
    "--text-col", .text_col,
    "--model", .model,
    "--device", .device,
    "--batch-size", .batch_size,
    "--n-process", .n_process,
    "--timeout", as.integer(.timeout)
  )
  if (!is.null(.labels)) args_ <- c(args_, "--label", .labels)
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  if (isTRUE(.no_progress)) args_ <- c(args_, "--no-progress")

  status_ <- system2(python_, args_,
                     stdout = if (.quiet) FALSE else "",
                     stderr = if (.quiet) FALSE else ""
  )
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("extract_spacy.py failed (status {status_}).")

  elapsed_ <- round(as.numeric(difftime(Sys.time(), t0_, units = "secs")), 1)
  if (!.quiet) cli::cli_alert_success("spaCy [{(.model)}] done in {elapsed_}s -> {.path {(.output)}}")
  return(invisible(.output))
}

#' Run the LexNLP extractor inside its container over parquet input
#'
#' LexNLP is pinned to Python 3.8 and cannot share the engine environment, so it runs in a
#' container: the inputs' common base is mounted read-only at /work and the output directory at
#' /out, and host paths are translated into the mount before the command is built.
#'
#' Geography is excluded by policy rather than by capability. LexNLP's geoentity pass is the
#' throughput killer in this engine set, and places are covered by the gazetteer, so paying for it
#' here would buy a second opinion at several times the cost of the first.
#'
#' The timeout caps every extractor on every document because LexNLP's maxent NER and date grammar
#' spin pathologically on a small number of files. On timeout that extractor is skipped for that
#' document, the document still lands in the output carrying its marker row, and the run continues.
#'
#' @param .inputs Character. Parquet file(s) or folder(s); folders are globbed recursively.
#' @param .output Character. Destination parquet for the candidate spans.
#' @param .id_col Character. Document identifier column in the input.
#' @param .text_col Character. Text column the offsets will index.
#' @param .labels Character. Entity labels to keep, within the supported set.
#' @param .max_chars Integer or NULL. Truncate each document to its first N characters. NULL is no
#'   truncation.
#' @param .timeout Integer. Per-extractor-per-document cap in seconds; 0 disables it.
#' @param .geo_config Character. In-container path to the geoentity table. Unused while GPE is
#'   excluded, and kept so that re-enabling it is a one-argument change.
#' @param .chunk_size Integer. Documents per worker chunk.
#' @param .n_process Integer. Worker processes.
#' @param .overwrite Logical. TRUE re-runs even when .output exists.
#' @param .no_progress Logical. Suppress the extractor's own progress bar.
#' @param .image Character. Container image tag.
#' @param .quiet Logical. Suppress the completion message.
#' @return Invisibly, the output path.
ner_lexnlp <- function(
    .inputs,
    .output,
    .id_col = "DocID",
    .text_col = "TextRaw",
    .labels = c("ORG", "DATE", "MONEY"), # full supported set
    .max_chars = NULL, # NULL -> omit --max-chars -> no truncation
    .timeout = 60L, # per-extractor-per-doc cap in seconds (0 = off)
    .geo_config = "/app/geoentities.csv", # in-container path (GPE only)
    .chunk_size = 8L,
    .n_process = 1L,
    .overwrite = FALSE,
    .no_progress = FALSE,
    .image = "contracts-lexnlp",
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- fil_sample_dirs$Path[20]
    .output <- file.path(.lP$Cache$NerTest, "test_lexnlp.parquet")
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .labels <- c("ORG", "DATE", "MONEY")
    .max_chars <- NULL
    .timeout <- 60L
    .geo_config <- "/app/geoentities.csv"
    .chunk_size <- 8L
    .n_process <- 10L
    .overwrite <- FALSE
    .no_progress <- FALSE
    .image <- "contracts-lexnlp"
    .quiet <- FALSE
  }

  if (fs::file_exists(.output)) {
    if (isTRUE(.overwrite)) {
      fs::file_delete(.output)
    } else {
      if (!.quiet) cli::cli_alert_info("Output exists, skipping: {.path {(.output)}}")
      return(invisible(.output))
    }
  }

  t0_ <- Sys.time()

  if (Sys.which("docker") == "") cli::cli_abort("docker not found on PATH.")
  inputs_ <- fs::path_abs(.inputs)
  out_dir_ <- fs::path_dir(.output)
  fs::dir_create(out_dir_)

  base_ <- fs::path_common(inputs_)
  if (!fs::is_dir(base_)) base_ <- fs::path_dir(base_) # single-file case
  rel_ <- fs::path_rel(inputs_, base_)
  cont_in_ <- as.character(fs::path("/work", rel_))
  cont_in_[rel_ == "."] <- "/work"

  args_ <- c(
    "run", "--rm",
    "-v", paste0(as.character(fs::path_real(base_)), ":/work:ro"),
    "-v", paste0(as.character(fs::path_real(out_dir_)), ":/out"),
    .image,
    cont_in_,
    "--output", fs::path("/out", fs::path_file(.output)),
    "--id-col", .id_col,
    "--text-col", .text_col,
    "--label", .labels,
    "--geo-config", .geo_config,
    "--n-process", .n_process,
    "--chunk-size", .chunk_size,
    "--timeout", as.integer(.timeout)
  )
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  if (isTRUE(.no_progress)) args_ <- c(args_, "--no-progress")

  status_ <- system2("docker", args_,
                     stdout = if (.quiet) FALSE else "",
                     stderr = if (.quiet) FALSE else ""
  )
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("LexNLP container failed (status {status_}).")

  elapsed_ <- round(as.numeric(difftime(Sys.time(), t0_, units = "secs")), 1)
  if (!.quiet) cli::cli_alert_success("LexNLP done in {elapsed_}s -> {.path {(.output)}}")
  return(invisible(.output))
}



#' Run the ported date-pattern extractor over parquet input
#'
#' The paper's eight date patterns, ported so that the published measure can be reproduced rather
#' than described. The script stamps Engine "paper" and Model "dateregex-v1"; the model tag names
#' the pattern set, so revising a pattern means bumping MODEL in extract_dateregex.py and not
#' silently changing what an existing store means.
#'
#' Single process and pure regex, so it finishes in minutes. That is why it runs first in the engine
#' order: the positional evidence is available for inspection long before the transformer starts.
#'
#' @param .inputs Character. Parquet file(s) or folder(s); folders are globbed recursively.
#' @param .output Character. Destination parquet for the candidate spans.
#' @param .id_col Character. Document identifier column in the input.
#' @param .text_col Character. Text column the offsets will index.
#' @param .labels Character. Entity labels to keep; DATE is the whole supported set.
#' @param .max_chars Integer or NULL. Truncate each document to its first N characters. NULL is no
#'   truncation.
#' @param .overwrite Logical. TRUE re-runs even when .output exists.
#' @param .no_progress Logical. Suppress the extractor's own progress bar.
#' @param .engine_dir Character. Root of the contracts-engine package.
#' @param .quiet Logical. Suppress the completion message.
#' @return Invisibly, the output path.
ner_dateregex <- function(
    .inputs,
    .output,
    .id_col = "DocID",
    .text_col = "TextRaw",
    .labels = "DATE", # full supported set
    .max_chars = NULL, # NULL -> omit --max-chars -> no truncation
    .overwrite = FALSE,
    .no_progress = FALSE,
    .engine_dir = here::here("contracts-engine"),
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- fil_sample_dirs$Path[20]
    .output <- file.path(.lP$Cache$NerTest, "test_dateregex.parquet")
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .labels <- "DATE"
    .max_chars <- NULL
    .overwrite <- FALSE
    .no_progress <- FALSE
    .engine_dir <- here::here("contracts-engine")
    .quiet <- FALSE
  }

  if (fs::file_exists(.output)) {
    if (isTRUE(.overwrite)) {
      fs::file_delete(.output)
    } else {
      if (!.quiet) cli::cli_alert_info("Output exists, skipping: {.path {(.output)}}")
      return(invisible(.output))
    }
  }

  t0_ <- Sys.time()

  python_ <- fs::path(.engine_dir, ".venv", "bin", "python")
  script_ <- fs::path(.engine_dir, "extract_dateregex.py")
  if (!fs::file_exists(python_)) cli::cli_abort("No engine venv at {.path {python_}}.")
  if (!fs::file_exists(script_)) cli::cli_abort("Missing {.path {script_}}.")
  fs::dir_create(fs::path_dir(.output))

  args_ <- c(
    script_, fs::path_abs(.inputs),
    "--output", .output,
    "--id-col", .id_col,
    "--text-col", .text_col,
    "--label", .labels
  )
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  if (isTRUE(.no_progress)) args_ <- c(args_, "--no-progress")

  status_ <- system2(python_, args_,
                     stdout = if (.quiet) FALSE else "",
                     stderr = if (.quiet) FALSE else ""
  )
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("extract_dateregex.py failed (status {status_}).")

  elapsed_ <- round(as.numeric(difftime(Sys.time(), t0_, units = "secs")), 1)
  if (!.quiet) cli::cli_alert_success("dateregex [dateregex-v1] done in {elapsed_}s -> {.path {(.output)}}")
  return(invisible(.output))
}

#' Run the ported place-name gazetteer over parquet input
#'
#' The paper's USGS-plus-countries lookup, rewritten with a hierarchy and a proximity gate. The
#' rewrite matters: the original excluded any place name that is also an English dictionary word,
#' which removes forty of the fifty US states and leaves a state-level geography consisting of the
#' ten whose names run to two words. This version keeps those names and gates them on context
#' instead, so Delaware and California are recoverable while Reading and Mobile are not admitted
#' without a jurisdiction beside them.
#'
#' Matching runs case-insensitively against the canonical text rather than a normalised copy,
#' because the offsets have to index the string every other engine indexed. The matcher lowercases
#' internally, so case-insensitivity costs nothing in fidelity.
#'
#' @param .inputs Character. Parquet file(s) or folder(s); folders are globbed recursively.
#' @param .output Character. Destination parquet for the candidate spans.
#' @param .id_col Character. Document identifier column in the input.
#' @param .text_col Character. Text column the offsets will index.
#' @param .labels Character. Entity labels to keep; GPE is the whole supported set.
#' @param .lookup Character. Path to the gazetteer parquet.
#' @param .state_window Integer. Characters within which a jurisdiction must appear for a
#'   distinctive place name to be admitted.
#' @param .word_window Integer. The same for a place name that is also an ordinary English word,
#'   which needs a jurisdiction immediately adjacent. This is what separates the city from the
#'   street it stands on.
#' @param .max_chars Integer or NULL. Truncate each document to its first N characters. NULL is no
#'   truncation.
#' @param .timeout Integer. Per-document matching cap in seconds; a marker row is written on
#'   timeout. 0 disables it.
#' @param .n_process Integer. Worker processes.
#' @param .chunk_size Integer. Documents per worker chunk.
#' @param .overwrite Logical. TRUE re-runs even when .output exists.
#' @param .no_progress Logical. Suppress the extractor's own progress bar.
#' @param .engine_dir Character. Root of the contracts-engine package.
#' @param .quiet Logical. Suppress the completion message.
#' @return Invisibly, the output path.
ner_gazetteer <- function(
    .inputs,
    .output,
    .id_col = "DocID",
    .text_col = "TextRaw",
    .labels = "GPE", # full supported set
    .lookup = here::here("contracts-engine", "data", "gazetteer", "geo_lookup.parquet"),
    .state_window = 200L, # loose window (chars) for distinctive gated names
    .word_window = 40L, # strict window (chars) for common-word gated names
    .max_chars = NULL, # NULL -> omit --max-chars -> no truncation
    .timeout = 120L, # per-doc cap in seconds (0 = off)
    .n_process = 1L,
    .chunk_size = 8L,
    .overwrite = FALSE,
    .no_progress = FALSE,
    .engine_dir = here::here("contracts-engine"),
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- fil_sample_dirs$Path[20]
    .output <- file.path(.lP$Cache$NerTest, "test_gazetteer.parquet")
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .labels <- "GPE"
    .lookup <- here::here("contracts-engine", "data", "gazetteer", "geo_lookup.parquet")
    .state_window <- 200L
    .word_window <- 40L
    .max_chars <- NULL
    .timeout <- 120L
    .n_process <- 10L
    .chunk_size <- 8L
    .overwrite <- FALSE
    .no_progress <- FALSE
    .engine_dir <- here::here("contracts-engine")
    .quiet <- FALSE
  }

  if (fs::file_exists(.output)) {
    if (isTRUE(.overwrite)) {
      fs::file_delete(.output)
    } else {
      if (!.quiet) cli::cli_alert_info("Output exists, skipping: {.path {(.output)}}")
      return(invisible(.output))
    }
  }

  t0_ <- Sys.time()

  python_ <- fs::path(.engine_dir, ".venv", "bin", "python")
  script_ <- fs::path(.engine_dir, "extract_gazetteer.py")
  if (!fs::file_exists(python_)) cli::cli_abort("No engine venv at {.path {python_}}.")
  if (!fs::file_exists(script_)) cli::cli_abort("Missing {.path {script_}}.")
  if (!fs::file_exists(.lookup)) cli::cli_abort("Gazetteer lookup not found: {.path {(.lookup)}}.")
  fs::dir_create(fs::path_dir(.output))

  args_ <- c(
    script_, fs::path_abs(.inputs),
    "--output", .output,
    "--id-col", .id_col,
    "--text-col", .text_col,
    "--label", .labels,
    "--lookup", fs::path_abs(.lookup),
    "--state-window", as.integer(.state_window),
    "--word-window", as.integer(.word_window),
    "--n-process", .n_process,
    "--chunk-size", .chunk_size,
    "--timeout", as.integer(.timeout)
  )
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  if (isTRUE(.no_progress)) args_ <- c(args_, "--no-progress")

  status_ <- system2(python_, args_,
                     stdout = if (.quiet) FALSE else "",
                     stderr = if (.quiet) FALSE else ""
  )
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("extract_gazetteer.py failed (status {status_}).")

  elapsed_ <- round(as.numeric(difftime(Sys.time(), t0_, units = "secs")), 1)
  if (!.quiet) cli::cli_alert_success("gazetteer [gazetteer-v1] done in {elapsed_}s -> {.path {(.output)}}")
  return(invisible(.output))
}

#' Resolve inputs to a flat list of parquet files
#'
#' Mirrors what the Python extractors do with the same argument, so a folder passed to R and the
#' same folder passed to the engine resolve to the same file set. An empty result is an error rather
#' than an empty run: a mistyped path that silently processes nothing looks identical to a
#' completed pass in the ledger.
#'
#' @param .inputs Character. Parquet file(s) or folder(s); folders are globbed recursively.
#' @return Character vector of absolute, unique parquet paths.
ner_input_files <- function(.inputs) {
  if (FALSE) {
    .inputs <- fil_sample_dirs$Path[20]
  }

  paths_ <- fs::path_abs(.inputs)
  files_ <- purrr::map(paths_, \(.p) {
    if (fs::is_dir(.p)) fs::dir_ls(.p, recurse = TRUE, glob = "*.parquet") else .p
  }) |>
    unlist() |>
    unique() |>
    as.character()
  if (length(files_) == 0L) cli::cli_abort("No parquet files found in {.arg .inputs}.")
  return(files_)
}

#' Run the redaction-indicator extractor over parquet input
#'
#' Not an entity extractor in the usual sense. It emits the bracketed indicators the published
#' analysis counted as spans under the label REDACT, with LabelRaw carrying the class
#' (RedactSymbol, RedactExplicit, OmitExplicit, OmitSymbol, RedactBare).
#'
#' Emitting them with offsets rather than as a count is the whole point. A count says how much was
#' withheld but not what; putting the indicators in the same coordinate system as the candidates
#' turns "how far is this amount from the nearest redaction" into a window function instead of a
#' second pass over the text. That is the only external evidence money admits, since a marker sits
#' exactly where a commercially material figure used to be.
#'
#' @param .inputs Character. Parquet file(s) or folder(s); folders are globbed recursively.
#' @param .output Character. Destination parquet for the candidate spans.
#' @param .id_col Character. Document identifier column in the input.
#' @param .text_col Character. Text column the offsets will index.
#' @param .labels Character. Entity labels to keep; REDACT is the whole supported set.
#' @param .max_chars Integer or NULL. Truncate each document to its first N characters. NULL is no
#'   truncation, which is what this extractor wants: a cap drops most of the markers.
#' @param .timeout Integer. Per-document cap in seconds; 0 disables it.
#' @param .n_process Integer. Worker processes.
#' @param .chunk_size Integer. Documents per worker chunk.
#' @param .overwrite Logical. TRUE re-runs even when .output exists.
#' @param .no_progress Logical. Suppress the extractor's own progress bar.
#' @param .engine_dir Character. Root of the contracts-engine package.
#' @param .quiet Logical. Suppress the completion message.
#' @return Invisibly, the output path.
ner_redaction <- function(
    .inputs,
    .output,
    .id_col = "DocID",
    .text_col = "TextRaw",
    .labels = "REDACT", # full supported set
    .max_chars = NULL, # NULL -> omit --max-chars -> no truncation
    .timeout = 0L, # per-document cap in seconds; 0 = off
    .n_process = 16L,
    .chunk_size = 64L,
    .overwrite = FALSE,
    .no_progress = FALSE,
    .engine_dir = here::here("contracts-engine"),
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- fil_sample_dirs$Path[20]
    .output <- file.path(.lP$Cache$NerTest, "test_redaction.parquet")
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .labels <- "REDACT"
    .max_chars <- NULL
    .timeout <- 0L
    .n_process <- 16L
    .chunk_size <- 64L
    .overwrite <- FALSE
    .no_progress <- FALSE
    .engine_dir <- here::here("contracts-engine")
    .quiet <- FALSE
  }

  if (fs::file_exists(.output)) {
    if (isTRUE(.overwrite)) {
      fs::file_delete(.output)
    } else {
      if (!.quiet) cli::cli_alert_info("Output exists, skipping: {.path {(.output)}}")
      return(invisible(.output))
    }
  }

  t0_ <- Sys.time()

  python_ <- fs::path(.engine_dir, ".venv", "bin", "python")
  script_ <- fs::path(.engine_dir, "extract_redaction.py")
  if (!fs::file_exists(python_)) cli::cli_abort("No engine venv at {.path {python_}}.")
  if (!fs::file_exists(script_)) cli::cli_abort("Missing {.path {script_}}.")
  fs::dir_create(fs::path_dir(.output))

  args_ <- c(
    script_, fs::path_abs(.inputs),
    "--output", .output,
    "--id-col", .id_col,
    "--text-col", .text_col,
    "--label", .labels,
    "--n-process", as.integer(.n_process),
    "--chunk-size", as.integer(.chunk_size),
    "--timeout", as.integer(.timeout)
  )
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  if (isTRUE(.no_progress)) args_ <- c(args_, "--no-progress")

  status_ <- system2(python_, args_,
                     stdout = if (.quiet) FALSE else "",
                     stderr = if (.quiet) FALSE else ""
  )
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("extract_redaction.py failed (status {status_}).")

  elapsed_ <- round(as.numeric(difftime(Sys.time(), t0_, units = "secs")), 1)
  if (!.quiet) cli::cli_alert_success("redaction [redaction-v1] done in {elapsed_}s -> {.path {(.output)}}")
  return(invisible(.output))
}

#' Run the monetary-pattern extractor over parquet input
#'
#' A rule arm for the one label with no external anchor. Money cannot be ranked on recall, because
#' EDGAR records no contract value, so the choice between engines has to rest on something other
#' than a recall table: agreement with the other family, and whether an engine can propose a span at
#' a site where the figure was withheld.
#'
#' @param .inputs Character. Parquet file(s) or folder(s); folders are globbed recursively.
#' @param .output Character. Destination parquet for the candidate spans.
#' @param .id_col Character. Document identifier column in the input.
#' @param .text_col Character. Text column the offsets will index.
#' @param .labels Character. Entity labels to keep; MONEY is the whole supported set.
#' @param .max_chars Integer or NULL. Truncate each document to its first N characters. NULL is no
#'   truncation.
#' @param .timeout Integer. Per-document cap in seconds; 0 disables it.
#' @param .n_process Integer. Worker processes.
#' @param .chunk_size Integer. Documents per worker chunk.
#' @param .overwrite Logical. TRUE re-runs even when .output exists.
#' @param .no_progress Logical. Suppress the extractor's own progress bar.
#' @param .engine_dir Character. Root of the contracts-engine package.
#' @param .quiet Logical. Suppress the completion message.
#' @return Invisibly, the output path.
ner_moneyregex <- function(
    .inputs,
    .output,
    .id_col = "DocID",
    .text_col = "TextRaw",
    .labels = "MONEY", # full supported set
    .max_chars = NULL, # NULL -> omit --max-chars -> no truncation
    .timeout = 0L, # per-document cap in seconds; 0 = off
    .n_process = 16L,
    .chunk_size = 64L,
    .overwrite = FALSE,
    .no_progress = FALSE,
    .engine_dir = here::here("contracts-engine"),
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- fil_sample_dirs$Path[20]
    .output <- file.path(.lP$Cache$NerTest, "test_moneyregex.parquet")
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .labels <- "MONEY"
    .max_chars <- NULL
    .timeout <- 0L
    .n_process <- 16L
    .chunk_size <- 64L
    .overwrite <- FALSE
    .no_progress <- FALSE
    .engine_dir <- here::here("contracts-engine")
    .quiet <- FALSE
  }

  if (fs::file_exists(.output)) {
    if (isTRUE(.overwrite)) {
      fs::file_delete(.output)
    } else {
      if (!.quiet) cli::cli_alert_info("Output exists, skipping: {.path {(.output)}}")
      return(invisible(.output))
    }
  }

  t0_ <- Sys.time()

  python_ <- fs::path(.engine_dir, ".venv", "bin", "python")
  script_ <- fs::path(.engine_dir, "extract_moneyregex.py")
  if (!fs::file_exists(python_)) cli::cli_abort("No engine venv at {.path {python_}}.")
  if (!fs::file_exists(script_)) cli::cli_abort("Missing {.path {script_}}.")
  fs::dir_create(fs::path_dir(.output))

  args_ <- c(
    script_, fs::path_abs(.inputs),
    "--output", .output,
    "--id-col", .id_col,
    "--text-col", .text_col,
    "--label", .labels,
    "--n-process", as.integer(.n_process),
    "--chunk-size", as.integer(.chunk_size),
    "--timeout", as.integer(.timeout)
  )
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  if (isTRUE(.no_progress)) args_ <- c(args_, "--no-progress")

  status_ <- system2(python_, args_,
                     stdout = if (.quiet) FALSE else "",
                     stderr = if (.quiet) FALSE else ""
  )
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("extract_moneyregex.py failed (status {status_}).")

  elapsed_ <- round(as.numeric(difftime(Sys.time(), t0_, units = "secs")), 1)
  if (!.quiet) cli::cli_alert_success("moneyregex [moneyregex-v4] done in {elapsed_}s -> {.path {(.output)}}")
  return(invisible(.output))
}

#' The labels a combination is asked for
#'
#' THE POLICY LIVES HERE AND NOWHERE ELSE, because two readers need it and they must not drift.
#' ner_run() uses it to build the --label argument; 04A's manifest uses it to fingerprint the store.
#' While the policy was a local inside ner_run(), the manifest could only record that a policy
#' existed -- it wrote the literal string "per-combo policy" -- so editing the policy left the
#' fingerprint unchanged and the store reported itself complete under a label set it had never been
#' built with. PERSON was absent from every spaCy extraction for a full pass that way, with nothing
#' in any document able to show it.
#'
#' The grid this describes is deliberately ragged. spaCy is the only multi-label statistical engine
#' and the only source of PERSON; LexNLP excludes persons and places by policy; the four ported
#' paper extractors are single-label specialists. An engine's absent labels are a property of the
#' engine rather than a failure of the run.
#'
#' Keyed by "engine" and by "engine:model", with engine:model winning, because the paper's models
#' differ from one another.
#'
#' @param .engine Character. Engine token.
#' @param .model Character. Model tag.
#' @param .labels Character, named list or NULL. An explicit override, exactly as ner_run() takes
#'   it; NULL applies the policy.
#' @return Character vector of unified labels.
ner_label_policy <- function(.engine, .model, .labels = NULL) {
  if (FALSE) {
    .engine <- "spacy"
    .model  <- "en_core_web_lg"
    .labels <- NULL
  }

  policy_ <- list(
    "spacy"               = c("ORG", "PERSON", "GPE", "DATE", "MONEY"),
    "lexnlp"              = c("ORG", "DATE", "MONEY"),
    "paper:dateregex-v1"  = "DATE",
    "paper:gazetteer-v1"  = "GPE",
    "paper:redaction-v1"  = "REDACT",
    "paper:moneyregex-v4" = "MONEY"
  )

  key_em_  <- paste0(.engine, ":", .model)
  default_ <- if (!is.null(policy_[[key_em_]])) policy_[[key_em_]] else policy_[[.engine]]
  ner_arg(.labels, .engine, .model, .default = default_)
}

#' Run a set of engine and model combinations into the candidate store
#'
#' The orchestrator every extraction goes through, so that one ledger governs what has been done and
#' no document is processed twice. Combinations are requested as tokens: "lexnlp", "spacy:<model>",
#' "paper:<model>". The paper engine groups the ported extractors under one provenance axis, which
#' is what keeps "whose method is this" answerable after the fact.
#'
#' Idempotent and resumable. Each combination asks the ledger which documents it has not yet seen,
#' processes only those in slices, and appends. An interrupted run repeats only its unfinished
#' slice, which is what makes a multi-hour extraction affordable inside a document that always
#' executes. Staging names carry no chunk index, so a recomputed slice after a crash overwrites its
#' own partial output rather than accumulating beside it.
#'
#' THE LEDGER IS BLIND TO THREE THINGS: which labels were requested, whether the text was truncated
#' first, and how the model was labelled. A store built under one truncation and re-run under
#' another therefore does nothing at all and reports success. 04A closes the label half of this by
#' fingerprinting ner_label_policy() in its manifest and clearing the engines whose set has moved;
#' .max_chars still has to be held fixed for the life of a store by hand.
#'
#' Stall protection differs by engine because the failure modes differ. A spaCy window stall drops
#' to sequential processing and skips the offender; a LexNLP extractor or gazetteer document over
#' its cap is skipped for that document alone. Either way the document lands in the store with a
#' status, and the run continues.
#'
#' @param .inputs Character. Parquet file(s) or folder(s) holding the canonical text.
#' @param .db_path Character. Path to the DuckDB candidate store; created if absent.
#' @param .run Character. Combination tokens to run, cheapest first.
#' @param .labels Character, named list or NULL. NULL applies each engine's own policy. A named
#'   list keys on engine or engine:model.
#' @param .max_chars Integer or NULL. Truncate every document to its first N characters before
#'   extraction. Frozen for the life of the store.
#' @param .retry_timeout Logical. TRUE re-runs documents previously ingested with Status "timeout".
#' @param .id_col Character. Document identifier column in the input.
#' @param .text_col Character. Text column the offsets will index.
#' @param .docs_per_run Integer. Documents per slice; bounds Python-side memory.
#' @param .device Character. Passed to the spaCy extractor.
#' @param .n_process Integer or named list. Worker processes, optionally per combination.
#' @param .batch_size Integer or named list. Batch size, optionally per combination.
#' @param .timeout Integer or named list. Stall guard, optionally per combination.
#' @param .work_dir Character. Where staging parquet is written.
#' @param .keep_staging Logical. TRUE leaves staging files in place after ingest.
#' @param .no_progress Logical. Pass --no-progress to every extractor, suppressing the tqdm bar it
#'   writes to stderr. Leave FALSE where this is the only thing running and the per-engine bar is
#'   the progress display; set TRUE where a CALLER owns the display. The two cannot share a terminal
#'   line: tqdm and cli both write carriage returns, and a corpus loop that shows its own bar over
#'   chunks gets it overwritten by one bar per extractor per chunk.
#' @param .overwrite Logical. Passed to the extractors.
#' @param .quiet Logical. Suppress per-combination messages.
#' @return Invisibly, a tibble of what each combination processed.
ner_run <- function(
    .inputs,
    .db_path,
    .run = c("spacy:en_core_web_sm", "lexnlp", "paper:dateregex-v1", "paper:gazetteer-v1"),
    .labels = NULL, # NULL = per-combo policy; scalar/vector or named (engine / engine:model)
    .max_chars = NULL, # truncate docs to first N chars before extraction (NULL = off)
    .retry_timeout = FALSE, # re-run docs previously ingested as Status = 'timeout'
    .id_col = "DocID",
    .text_col = "TextRaw",
    .docs_per_run = 5000L, # R-side corpus chunking (memory bound; NULL = all at once)
    .device = "auto", # auto|cpu|cuda|mps (spaCy)
    .n_process = 16L, # CPU workers -- scalar or named (engine / engine:model)
    .batch_size = 64L, # spaCy batch / LexNLP+gazetteer chunksize -- scalar or named
    .timeout = list(spacy = 600L, lexnlp = 60L, "paper:gazetteer-v1" = 120L), # stall guard secs, 0=off
    .keep_staging = FALSE,
    .no_progress = FALSE, # suppress the EXTRACTORS' own bars; see the note in the roxygen
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- .lP$Input$SampleContracts
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .run <- c("spacy:en_core_web_sm", "lexnlp", "paper:dateregex-v1", "paper:gazetteer-v1")
    .labels <- NULL
    .max_chars <- NULL
    .retry_timeout <- FALSE
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .docs_per_run <- 5000L
    .device <- "auto"
    .n_process <- list(spacy = 16L, lexnlp = 24L, "paper:gazetteer-v1" = 16L)
    .batch_size <- 64L
    .timeout <- list(spacy = 600L, lexnlp = 60L, "paper:gazetteer-v1" = 120L)
    .keep_staging <- FALSE
    .no_progress <- FALSE
    .quiet <- FALSE
  }


  # Parse .run tokens ("engine" or "engine:model") into the combo grid. Model =
  # the tag stamped into the parquet; ModelArg = what the wrapper receives. Bare
  # "spacy"/"paper" error (both need a model).
  parse_run_ <- function(.tok) {
    parts_ <- strsplit(.tok, ":", fixed = TRUE)[[1]]
    engine_ <- parts_[1]
    if (!engine_ %in% c("spacy", "lexnlp", "paper")) {
      cli::cli_abort("Unknown engine in {.val {(.tok)}}; expected spacy|lexnlp|paper.")
    }
    if (engine_ == "spacy") {
      if (length(parts_) < 2L) {
        cli::cli_abort("spaCy needs a model: {.val {(.tok)}} -> e.g. {.val spacy:en_core_web_sm}")
      }
      model_ <- paste(parts_[-1], collapse = ":") # tolerate ':' in a model path
      tibble::tibble(Engine = "spacy", Model = fs::path_file(model_), ModelArg = model_)
    } else if (engine_ == "lexnlp") {
      tibble::tibble(Engine = "lexnlp", Model = "lexnlp", ModelArg = "lexnlp")
    } else { # paper
      if (length(parts_) < 2L) {
        cli::cli_abort("paper needs a model: {.val {(.tok)}} -> one of dateregex-v1|gazetteer-v1|redaction-v1|moneyregex-v4")
      }
      model_ <- parts_[2]
      if (!model_ %in% c("dateregex-v1", "gazetteer-v1", "redaction-v1", "moneyregex-v4")) {
        cli::cli_abort("Unknown paper model {.val {model_}}; expected dateregex-v1|gazetteer-v1|redaction-v1|moneyregex-v1.")
      }
      tibble::tibble(Engine = "paper", Model = model_, ModelArg = model_)
    }
  }

  combos_ <- purrr::map(.run, parse_run_) |> dplyr::bind_rows()
  if (anyDuplicated(combos_[c("Engine", "Model")])) {
    cli::cli_abort("Duplicate combo(s) in {.arg .run}.")
  }

  files_ <- ner_input_files(.inputs)
  tmp_dir_ <- fs::path(fs::path_dir(.db_path), ".ner_tmp")
  fs::dir_create(tmp_dir_)

  summary_ <- vector("list", nrow(combos_))

  for (i_ in seq_len(nrow(combos_))) {
    engine_ <- combos_$Engine[i_]
    model_ <- combos_$Model[i_]
    key_em_ <- paste0(engine_, ":", model_)

    missing_ <- sort(ner_db_missing(
      .db_path, .inputs, engine_, model_,
      .id_col = .id_col, .retry_timeout = .retry_timeout, .quiet = .quiet
    ))
    if (length(missing_) == 0L) {
      summary_[[i_]] <- tibble::tibble(
        Engine = engine_, Model = model_, Missing = 0L, Docs = 0L, Candidates = 0L
      )
      next
    }

    per_ <- if (is.null(.docs_per_run)) length(missing_) else as.integer(.docs_per_run)
    slices_ <- split(missing_, ceiling(seq_along(missing_) / per_))

    stage_ <- fs::path(tmp_dir_, paste0("staging_", engine_, "_", model_, ".parquet"))
    input_ <- fs::path(tmp_dir_, paste0("input_", engine_, "_", model_, ".parquet"))
    docs_ <- 0L
    cands_ <- 0L

    # Resolve this combo's knobs once (broadcast scalar/vector or engine[:model])
    labels_ <- ner_label_policy(.engine = engine_, .model = model_, .labels = .labels)
    n_process_ <- ner_arg(.n_process, engine_, model_, .default = 16L)
    batch_ <- ner_arg(.batch_size, engine_, model_, .default = 64L)
    timeout_ <- ner_arg(.timeout, engine_, model_, .default = 0L)

    for (s_ in seq_along(slices_)) {
      ids_ <- slices_[[s_]]
      if (!.quiet && length(slices_) > 1L) {
        cli::cli_alert_info("{engine_}/{model_}: slice {s_}/{length(slices_)} ({length(ids_)} doc{?s})")
      }

      if (!fs::file_exists(stage_)) {
        con_tmp_ <- ner_db_connect()
        duckdb::duckdb_register(con_tmp_, "ner_slice_docs", data.frame(DocID = ids_))
        files_sql_ <- paste0("'", files_, "'", collapse = ", ")
        DBI::dbExecute(con_tmp_, paste0(
          "COPY (SELECT \"", .id_col, "\", \"", .text_col, "\" ",
          "FROM read_parquet([", files_sql_, "]) ",
          "WHERE \"", .id_col, "\" IN (SELECT DocID FROM ner_slice_docs)) ",
          "TO '", as.character(fs::path_abs(input_)), "' (FORMAT PARQUET)"
        ))
        DBI::dbDisconnect(con_tmp_, shutdown = TRUE)
      } else if (!.quiet) {
        cli::cli_alert_info("Reusing staging file from interrupted run: {.path {fs::path_file(stage_)}}")
      }

      if (engine_ == "spacy") {
        is_trf_ <- grepl("trf", model_, fixed = TRUE)
        device_ <- if (.device == "auto" && !is_trf_) "cpu" else .device
        ner_spacy(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = labels_, .max_chars = .max_chars, .timeout = timeout_,
          .model = combos_$ModelArg[i_], .device = device_,
          .batch_size = batch_, .n_process = n_process_, .no_progress = .no_progress, .quiet = .quiet
        )
      } else if (engine_ == "lexnlp") {
        ner_lexnlp(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = labels_, .max_chars = .max_chars, .timeout = timeout_,
          .chunk_size = batch_, .n_process = n_process_, .no_progress = .no_progress, .quiet = .quiet
        )
      } else if (engine_ == "paper" && model_ == "dateregex-v1") {
        ner_dateregex(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = labels_, .max_chars = .max_chars, .no_progress = .no_progress, .quiet = .quiet
        )
      } else if (engine_ == "paper" && model_ == "moneyregex-v4") {
        ner_moneyregex(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = labels_, .max_chars = .max_chars, .timeout = timeout_,
          .n_process = n_process_, .chunk_size = batch_, .no_progress = .no_progress, .quiet = .quiet
        )
      } else if (engine_ == "paper" && model_ == "redaction-v1") {
        ner_redaction(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = labels_, .max_chars = .max_chars, .timeout = timeout_,
          .n_process = n_process_, .chunk_size = batch_, .no_progress = .no_progress, .quiet = .quiet
        )
      } else if (engine_ == "paper" && model_ == "gazetteer-v1") {
        ner_gazetteer(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = labels_, .max_chars = .max_chars, .timeout = timeout_,
          .n_process = n_process_, .chunk_size = batch_, .no_progress = .no_progress, .quiet = .quiet
        )
      } else {
        cli::cli_abort("No dispatch for {.val {key_em_}}.")
      }

      # Identity guard: the parquet MUST stamp the combo we dispatched, else abort
      # (catches an extractor's ENGINE/MODEL constants drifting from the token).
      res_ <- ner_db_append(
        .db_path, stage_,
        .expect_engine = engine_, .expect_model = model_,
        .retry_timeout = .retry_timeout, .quiet = .quiet
      )
      docs_ <- docs_ + res_$docs
      cands_ <- cands_ + res_$candidates

      if (isTRUE(.keep_staging)) {
        tag_ <- sprintf("_%03d.parquet", s_)
        fs::file_move(stage_, fs::path_ext_remove(stage_) |> paste0(tag_))
        if (fs::file_exists(input_)) {
          fs::file_move(input_, fs::path_ext_remove(input_) |> paste0(tag_))
        }
      } else {
        del_ <- c(stage_, input_)
        fs::file_delete(del_[fs::file_exists(del_)])
      }
    }

    summary_[[i_]] <- tibble::tibble(
      Engine = engine_, Model = model_,
      Missing = length(missing_), Docs = docs_, Candidates = cands_
    )
  }

  out_ <- dplyr::bind_rows(summary_)
  if (!.quiet) {
    cli::cli_alert_success(
      paste0("ner_run complete: {sum(out_$Candidates)} candidate(s) over {sum(out_$Docs)} ",
             "doc combo(s) ({nrow(out_)} engine/model combo(s)).")
    )
  }
  return(invisible(out_))
}


#' Resolve a possibly per-combination argument for one engine and model
#'
#' Knobs may be given as a scalar applying to everything or as a list keyed on engine or
#' engine:model. Resolving that in one place keeps every extractor wrapper free of the same four
#' lines of lookup, and keeps the precedence rule -- the most specific key wins -- stated once.
#'
#' @param .x Scalar or named list. The argument as supplied by the caller.
#' @param .engine Character. Engine name.
#' @param .model Character. Model name.
#' @param .default Value returned when nothing matches.
#' @return The resolved value.
ner_arg <- function(.x, .engine, .model, .default = NULL) {
  if (is.null(.x)) return(.default)
  if (is.null(names(.x))) return(.x) # unnamed -> broadcast (length 1 or N)
  key_em_ <- paste0(.engine, ":", .model)
  if (key_em_ %in% names(.x)) return(.x[[key_em_]])
  if (.engine %in% names(.x)) return(.x[[.engine]])
  return(.default)
}


# 2. The candidate store -----------------------------------------------------------------------------------------------
# DuckDB holds two tables: `candidates`, one row per span, and `runs`, the ledger recording that a
# document was seen by a combination and with what outcome. The ledger is what makes extraction
# resumable, and its blindness to labels and truncation is why 04A carries a manifest beside it.

#' Open a DuckDB connection with its own progress bar turned off
#'
#' EVERY CONNECTION IN THIS PROJECT GOES THROUGH HERE, and the reason is a display collision rather
#' than anything about the data. DuckDB prints a progress bar for long-running queries by writing
#' carriage returns to the terminal. So does cli, which is what the corpus pass in 04D uses to show
#' how far through it is. Two writers on one line produce a smear that reports neither, and on a
#' pass measured in days the progress display is not a nicety -- it is the only evidence the run is
#' alive.
#'
#' DuckDB's own bar is the one to drop: it reports a single query, cli reports the pass. The setting
#' is applied per connection rather than globally because there is no global to set from R, and it
#' is wrapped in a tolerant call because the two setting names have moved between DuckDB versions
#' and a connection that works is worth more than a bar that is definitely off.
#'
#' @param .db_path Path to the database file, or NULL for an in-memory connection.
#' @param .read_only Logical. Open read-only. DuckDB is single-writer, so anything that only reads
#'   should say so and leave the writer free.
#' @return A live DBI connection. The caller disconnects with dbDisconnect(con, shutdown = TRUE).
ner_db_connect <- function(.db_path = NULL, .read_only = FALSE) {
  if (FALSE) {
    .db_path   <- .lP$Store$NerDB
    .read_only <- TRUE
  }

  # The two raw dbConnect() calls in this project, and they belong here: everything else routes
  # through this function so the settings below cannot be forgotten at a call site.
  con_ <- if (is.null(.db_path)) {
    DBI::dbConnect(duckdb::duckdb())
  } else {
    DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path), read_only = .read_only)
  }
  for (stmt_ in c("SET enable_progress_bar = false", "SET enable_progress_bar_print = false")) {
    try(DBI::dbExecute(con_, stmt_), silent = TRUE)
  }
  con_
}

#' Open the candidate store, creating its schema if absent
#'
#' Safe to call repeatedly: the DDL is idempotent, so a caller never has to know whether the store
#' already exists. The connection is live and the caller disconnects it with
#' DBI::dbDisconnect(con, shutdown = TRUE); DuckDB is single-writer, so holding one open blocks
#' every read-only session.
#'
#' @param .db_path Character. Path to the DuckDB file.
#' @param .quiet Logical. Suppress the creation message.
#' @return A live DBI connection.
ner_db_init <- function(.db_path, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .quiet <- FALSE
  }

  exists_ <- fs::file_exists(.db_path)
  fs::dir_create(fs::path_dir(.db_path))
  con_ <- ner_db_connect(.db_path = .db_path, .read_only = FALSE)

  # candidates = real hits only -- the harmonised Python schema (Engine + Model
  # are parquet columns). Null-span sentinels AND timeout markers are dropped
  # before insert, so every column except LabelRaw is NOT NULL. Start/Stop BIGINT.
  DBI::dbExecute(con_, "
    CREATE TABLE IF NOT EXISTS candidates (
      DocID     VARCHAR  NOT NULL,
      Start     BIGINT   NOT NULL,
      Stop      BIGINT   NOT NULL,
      Span      VARCHAR  NOT NULL,
      Label     VARCHAR  NOT NULL,
      LabelRaw  VARCHAR,
      Engine    VARCHAR  NOT NULL,
      Model     VARCHAR  NOT NULL
    );
  ")

  # runs = the completeness ledger and authoritative skip source. One row per
  # (DocID, Engine, Model) ingested, with Status:
  #   success -- ran, found candidates;        no re-run
  #   nohit   -- ran clean, genuinely nothing;  no re-run
  #   timeout -- >=1 extractor/window skipped;  re-run candidate (.retry_timeout)
  DBI::dbExecute(con_, "
    CREATE TABLE IF NOT EXISTS runs (
      DocID     VARCHAR    NOT NULL,
      Engine    VARCHAR    NOT NULL,
      Model     VARCHAR    NOT NULL,
      Status    VARCHAR    NOT NULL CHECK (Status IN ('success', 'nohit', 'timeout')),
      CreatedAt TIMESTAMP  NOT NULL,
      UNIQUE (DocID, Engine, Model)
    );
  ")

  if (!.quiet) {
    if (exists_) {
      cli::cli_alert_success("NER store OPENED at {.path {(.db_path)}}")
    } else {
      cli::cli_alert_success("NER store CREATED at {.path {(.db_path)}}")
    }
  }
  return(con_)
}

#' Append one extractor's staging parquet to the store
#'
#' Ingest is insert-only by default: a combination already recorded in the ledger is skipped rather
#' than duplicated, because an append that ran twice would double every candidate it wrote and
#' nothing downstream could tell.
#'
#' Marker and sentinel rows are dropped before the candidates insert, so the store holds real spans
#' only and the ledger holds the status. Both tables are written in one transaction; splitting them
#' would allow a store whose candidates and ledger disagree about what has been run.
#'
#' The identity guard is the check that earns its place. An extractor writing the wrong Engine or
#' Model tag produces a store that looks complete and attributes spans to a method that never saw
#' the document, so the expected values are asserted rather than trusted.
#'
#' @param .db_path Character. Path to the DuckDB file.
#' @param .parquet Character. Staging parquet written by an extractor.
#' @param .expect_engine Character or NULL. Assert the Engine tag in the staging file.
#' @param .expect_model Character or NULL. Assert the Model tag.
#' @param .retry_timeout Logical. TRUE deletes prior timeout rows for these documents first.
#' @param .quiet Logical. Suppress the ingest message.
#' @return Invisibly, a list of the row counts written.
ner_db_append <- function(.db_path, .parquet,
                          .expect_engine = NULL, .expect_model = NULL,
                          .retry_timeout = FALSE, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .parquet <- file.path(.lP$Cache$NerTest, "test_lexnlp.parquet")
    .expect_engine <- "lexnlp"
    .expect_model <- "lexnlp"
    .retry_timeout <- FALSE
    .quiet <- FALSE
  }

  if (!fs::file_exists(.parquet)) cli::cli_abort("Staging parquet not found: {.path {(.parquet)}}.")

  con_ <- ner_db_init(.db_path, .quiet = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  # Lazy view on the staging parquet -- DuckDB scans the file, R never holds it
  parquet_ <- as.character(fs::path_abs(.parquet))
  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TEMP VIEW ner_src AS SELECT * FROM read_parquet('", parquet_, "')"
  ))
  src_ <- dplyr::tbl(con_, "ner_src")

  # Identity guard: the parquet must carry exactly one (Engine, Model), and -- if
  # an expectation was passed -- it must equal what ner_run dispatched. Abort
  # loudly on drift rather than misfiling under the stamped identity and looping.
  stamped_ <- src_ |>
    dplyr::distinct(Engine, Model) |>
    dplyr::collect()
  if (nrow(stamped_) != 1L) {
    cli::cli_abort(c(
      "Staging parquet carries {nrow(stamped_)} distinct (Engine, Model) combo(s); expected exactly 1.",
      "i" = "File: {.path {fs::path_file(.parquet)}}"
    ))
  }
  if (!is.null(.expect_engine) && !is.null(.expect_model)) {
    if (!identical(stamped_$Engine[1], .expect_engine) ||
        !identical(stamped_$Model[1], .expect_model)) {
      cli::cli_abort(c(
        "Stamped identity does not match what was dispatched -- the extractor's \\
         ENGINE/MODEL constants have drifted from the {.arg .run} token.",
        "x" = "parquet stamps {.val {stamped_$Engine[1]}} / {.val {stamped_$Model[1]}}",
        "v" = "ner_run expected {.val {(.expect_engine)}} / {.val {(.expect_model)}}",
        "i" = "Fix the extractor's ENGINE/MODEL constants (or the token), then rerun. \\
               Ingesting as-is would misfile the rows and rerun this combo forever."
      ))
    }
  }

  # Per-doc status from the parquet: timeout (marker) > success (real hit) > nohit
  status_ <- src_ |>
    dplyr::group_by(DocID, Engine, Model) |>
    dplyr::summarise(
      HasTimeout = max(dplyr::if_else(!is.na(LabelRaw) & LabelRaw %like% "timeout:%", 1L, 0L), na.rm = TRUE),
      HasHit = max(dplyr::if_else(!is.na(Start), 1L, 0L), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(Status = dplyr::case_when(
      HasTimeout == 1L ~ "timeout",
      HasHit == 1L ~ "success",
      TRUE ~ "nohit"
    )) |>
    dplyr::select(DocID, Engine, Model, Status)

  # Which staged combos are new vs already-present (and their existing status)
  combos_ <- status_ |>
    dplyr::left_join(
      dplyr::tbl(con_, "runs") |> dplyr::select(DocID, Engine, Model, OldStatus = Status),
      by = c("DocID", "Engine", "Model")
    ) |>
    dplyr::collect()

  n_all_ <- nrow(combos_)
  new_ <- combos_ |> dplyr::filter(is.na(OldStatus))
  retry_ <- combos_ |> dplyr::filter(!is.na(OldStatus), OldStatus == "timeout")

  to_ingest_ <- if (isTRUE(.retry_timeout)) {
    dplyr::bind_rows(new_, retry_)
  } else {
    new_
  }

  if (nrow(to_ingest_) == 0L) {
    if (!.quiet) cli::cli_alert_info("All {n_all_} doc combo(s) already in store -- nothing to append.")
    return(invisible(list(docs = 0L, candidates = 0L, retried = 0L)))
  }

  # Register the ingest scope (DocIDs) and the per-doc status to write into runs
  duckdb::duckdb_register(con_, "ner_ingest_docs", data.frame(DocID = to_ingest_$DocID))
  on.exit(duckdb::duckdb_unregister(con_, "ner_ingest_docs"), add = TRUE, after = FALSE)
  duckdb::duckdb_register(con_, "ner_ingest_status", to_ingest_[c("DocID", "Engine", "Model", "Status")])
  on.exit(duckdb::duckdb_unregister(con_, "ner_ingest_status"), add = TRUE, after = FALSE)

  engine_ <- to_ingest_$Engine[1]
  model_ <- to_ingest_$Model[1]
  n_retry_ <- if (isTRUE(.retry_timeout)) nrow(retry_) else 0L

  DBI::dbBegin(con_)
  ok_ <- FALSE
  on.exit(if (!ok_) DBI::dbRollback(con_), add = TRUE, after = FALSE)

  # Retry path: clear the timeout docs from both tables first (scoped to this
  # combo x the staged retry docs)
  if (n_retry_ > 0L) {
    DBI::dbExecute(con_, "
      DELETE FROM candidates
      WHERE Engine = ? AND Model = ?
        AND DocID IN (SELECT DocID FROM ner_ingest_docs)
        AND DocID IN (SELECT DocID FROM runs r WHERE r.Engine = candidates.Engine
                        AND r.Model = candidates.Model AND r.Status = 'timeout')
    ", params = list(engine_, model_))
    DBI::dbExecute(con_, "
      DELETE FROM runs
      WHERE Engine = ? AND Model = ? AND Status = 'timeout'
        AND DocID IN (SELECT DocID FROM ner_ingest_docs)
    ", params = list(engine_, model_))
  }

  # candidates: real hits of the ingest scope only (markers + sentinels excluded)
  n_cand_ <- DBI::dbExecute(con_, "
    INSERT INTO candidates (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model)
    SELECT DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model
    FROM ner_src
    WHERE Start IS NOT NULL
      AND DocID IN (SELECT DocID FROM ner_ingest_docs)
  ")

  # runs: one row per ingested doc combo, with its derived Status
  DBI::dbExecute(con_, "
    INSERT INTO runs (DocID, Engine, Model, Status, CreatedAt)
    SELECT DocID, Engine, Model, Status, now()::TIMESTAMP
    FROM ner_ingest_status
  ")

  DBI::dbCommit(con_)
  ok_ <- TRUE

  if (!.quiet) {
    msg_ <- paste0("Appended {n_cand_} candidate(s) over {nrow(to_ingest_)} of {n_all_} ",
                   "doc combo(s) from {.path {fs::path_file(.parquet)}}")
    if (n_retry_ > 0L) msg_ <- paste0(msg_, " (incl. {n_retry_} timeout retr{?y/ies})")
    cli::cli_alert_success(msg_)
  }
  return(invisible(list(docs = nrow(to_ingest_), candidates = n_cand_, retried = n_retry_)))
}
#' Which documents a combination has not yet been run on
#'
#' The question the resumable design rests on. Asked against the ledger rather than against the
#' candidates table, because a document an engine legitimately found nothing in has no candidates
#' and must not be re-run forever.
#'
#' @param .db_path Character. Path to the DuckDB file.
#' @param .inputs Character. Parquet file(s) or folder(s) holding the canonical text.
#' @param .engine Character. Engine name.
#' @param .model Character. Model name.
#' @param .id_col Character. Document identifier column in the input.
#' @param .retry_timeout Logical. TRUE counts prior timeouts as missing.
#' @param .quiet Logical. Suppress the count message.
#' @return Character vector of document identifiers.
ner_db_missing <- function(.db_path, .inputs, .engine, .model,
                           .id_col = "DocID", .retry_timeout = FALSE, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .inputs <- fil_sample_dirs$Path[20]
    .engine <- "spacy"
    .model <- "en_core_web_sm"
    .id_col <- "DocID"
    .retry_timeout <- FALSE
    .quiet <- FALSE
  }

  files_ <- ner_input_files(.inputs)

  con_ <- ner_db_init(.db_path, .quiet = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  files_sql_ <- paste0("'", files_, "'", collapse = ", ")
  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TEMP VIEW ner_inputs AS ",
    "SELECT \"", .id_col, "\" AS DocID FROM read_parquet([", files_sql_, "])"
  ))

  # Ledger rows that count as "done" for this combo: always success/nohit; when
  # not retrying, timeout counts as done too (so it won't be re-sent).
  done_ <- dplyr::tbl(con_, "runs") |>
    dplyr::filter(Engine == !!.engine, Model == !!.model)
  if (isTRUE(.retry_timeout)) {
    done_ <- done_ |> dplyr::filter(Status != "timeout")
  }

  missing_ <- dplyr::tbl(con_, "ner_inputs") |>
    dplyr::distinct(DocID) |>
    dplyr::anti_join(done_, by = "DocID") |>
    dplyr::pull(DocID)

  n_all_ <- dplyr::tbl(con_, "ner_inputs") |>
    dplyr::summarise(n = dplyr::n_distinct(DocID)) |>
    dplyr::pull(n)

  if (!.quiet) {
    retry_msg_ <- if (isTRUE(.retry_timeout)) " (incl. timeout retries)" else ""
    cli::cli_alert_info(paste0("{(.engine)}/{(.model)}: {length(missing_)} of {n_all_} ",
                               "doc(s) missing{retry_msg_}."))
  }
  return(missing_)
}
#' Time a grid of combinations on a fixed input
#'
#' Cost is one of the two inputs to the deployment policy, and it cannot be read off a completed
#' store: the ledger records that a document was processed, not what it took. Timing has to be
#' measured while it happens, on a set small enough to run every combination over.
#'
#' @param .inputs Character. Parquet file(s) or folder(s) to time against.
#' @param .grid Tibble or list. Combinations to time.
#' @param .id_col Character. Document identifier column in the input.
#' @param .text_col Character. Text column the offsets will index.
#' @return Tibble of per-combination timings.
ner_bench <- function(.inputs, .grid, .id_col = "DocID", .text_col = "TextRaw") {
  if (FALSE) {
    .inputs <- fil_sample_dirs$Path[20]
    .grid <- bench_grid
    .id_col <- "DocID"
    .text_col <- "TextRaw"
  }

  # docs in scope, once (for docs/s)
  files_ <- ner_input_files(.inputs)
  con_tmp_ <- ner_db_connect()
  files_sql_ <- paste0("'", files_, "'", collapse = ", ")
  n_docs_ <- DBI::dbGetQuery(con_tmp_, paste0(
    "SELECT COUNT(DISTINCT \"", .id_col, "\") AS n FROM read_parquet([", files_sql_, "])"
  ))$n
  DBI::dbDisconnect(con_tmp_, shutdown = TRUE)
  cli::cli_alert_info("Benchmark scope: {n_docs_} doc(s), {nrow(.grid)} setting(s)")

  # background RSS sampler: sums RSS (KB) over all extract_spacy.py processes,
  # writes the running peak to a file; killed via its pid file after each run
  rss_file_ <- tempfile(fileext = ".txt")
  pid_file_ <- tempfile(fileext = ".pid")
  sampler_ <- tempfile(fileext = ".sh")
  writeLines(c(
    "#!/bin/bash",
    paste0("echo $$ > ", pid_file_),
    "max=0",
    "while true; do",
    "  cur=$(ps -A -o rss=,command= | grep '[e]xtract_spacy.py' | awk '{s+=$1} END {print s+0}')",
    "  if [ \"$cur\" -gt \"$max\" ]; then max=$cur; fi",
    paste0("  echo $max > ", rss_file_),
    "  sleep 0.5",
    "done"
  ), sampler_)

  results_ <- vector("list", nrow(.grid))

  for (i_ in seq_len(nrow(.grid))) {
    row_ <- .grid[i_, ]
    out_tmp_ <- tempfile(fileext = ".parquet")
    cli::cli_alert_info(
      "[{i_}/{nrow(.grid)}] {row_$Engine}/{row_$Model} device={row_$Device} \\
       n_process={row_$NProcess} batch={row_$BatchSize} chunk={row_$ChunkSize}"
    )

    is_spacy_ <- row_$Engine == "spacy"
    if (is_spacy_) {
      writeLines("0", rss_file_)
      system2("bash", sampler_, stdout = FALSE, stderr = FALSE, wait = FALSE)
    }

    t0_ <- Sys.time()
    ok_ <- tryCatch(
      {
        if (is_spacy_) {
          ner_spacy(
            .inputs = .inputs, .output = out_tmp_,
            .id_col = .id_col, .text_col = .text_col,
            .model = row_$Model, .device = row_$Device,
            .batch_size = row_$BatchSize, .n_process = row_$NProcess,
            .overwrite = TRUE, .no_progress = TRUE, .quiet = TRUE
          )
        } else {
          ner_lexnlp(
            .inputs = .inputs, .output = out_tmp_,
            .id_col = .id_col, .text_col = .text_col,
            .n_process = row_$NProcess, .chunk_size = row_$ChunkSize,
            .overwrite = TRUE, .no_progress = TRUE, .quiet = TRUE
          )
        }
        TRUE
      },
      error = function(e) {
        cli::cli_alert_danger("  FAILED: {conditionMessage(e)}")
        FALSE
      }
    )
    secs_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))

    rss_mb_ <- NA_real_
    if (is_spacy_) {
      if (fs::file_exists(pid_file_)) {
        system2("kill", readLines(pid_file_)[1], stdout = FALSE, stderr = FALSE)
      }
      rss_mb_ <- suppressWarnings(as.numeric(readLines(rss_file_)[1])) / 1024
    }
    if (fs::file_exists(out_tmp_)) fs::file_delete(out_tmp_)

    results_[[i_]] <- dplyr::bind_cols(
      row_,
      tibble::tibble(
        Ok = ok_,
        Secs = round(secs_, 1),
        DocsPerSec = round(n_docs_ / secs_, 2),
        PeakRssMb = round(rss_mb_, 0)
      )
    )
    res_ <- results_[[i_]]
    cli::cli_alert_success("  {res_$Secs}s ({res_$DocsPerSec} docs/s), peak {res_$PeakRssMb} MB")
  }

  dplyr::bind_rows(results_) |>
    dplyr::arrange(Engine, Model, Secs)
}


#' Verify that stored offsets still index the canonical text
#'
#' An offset is an integer into a specific string. If any script disagrees about what a document's
#' text is, every offset in the project is quietly wrong, is.na() catches nothing, and the numbers
#' look entirely normal. This rehydrates spans from the text and compares them to the stored Span,
#' which is the only check that fails loudly when that has happened.
#'
#' Slicing is by code point, not byte. Base substr() on this corpus misaligns about ninety-nine per
#' cent of spans, so stringi::stri_sub() is used throughout.
#'
#' @param .tab Tibble of candidates carrying DocID, Start, Stop and Span.
#' @param .inputs Character. Parquet file(s) or folder(s) holding the canonical text.
#' @param .text_col Character. Text column the offsets index.
#' @return Tibble with the rehydrated span beside the stored one and an agreement flag.
ner_check_offsets <- function(.tab, .inputs, .text_col = "TextRaw") {
  if (FALSE) {
    .tab <- arrow::read_parquet(file.path(.lP$Cache$NerTest, "test_regex.parquet"))
    .inputs <- fil_sample_dirs$Path[20]
    .text_col <- "TextRaw"
  }

  files_ <- ner_input_files(.inputs)
  doc_map_ <- tibble::tibble(
    Path = files_,
    DocID = fs::path_ext_remove(fs::path_file(files_))
  )

  out_ <- .tab |>
    dplyr::filter(!is.na(Start)) |>
    dplyr::mutate(Start = as.integer(Start), Stop = as.integer(Stop)) |>
    dplyr::left_join(doc_map_, by = dplyr::join_by(DocID)) |>
    dplyr::filter(!is.na(Path)) |>
    dplyr::group_by(Path) |>
    dplyr::group_modify(\(.x, .y) {
      text_ <- arrow::read_parquet(.y$Path, col_select = dplyr::all_of(.text_col))[[.text_col]]
      dplyr::mutate(.x, Rehydrated = stringi::stri_sub(text_, Start + 1L, Stop))
    }) |>
    dplyr::ungroup() |>
    dplyr::summarise(N = dplyr::n(), OkShare = mean(Rehydrated == Span))

  if (isTRUE(all.equal(out_$OkShare, 1))) {
    cli::cli_alert_success("Offset round-trip: {out_$N} candidate(s), 100% OK.")
  } else {
    cli::cli_alert_danger("Offset round-trip FAILED: OkShare = {round(out_$OkShare, 4)} over {out_$N} candidate(s).")
  }
  return(invisible(out_))
}

#' Explode a combination-keyed tuning bundle into ner_run() arguments
#'
#' One bundle in the runbook, keyed by combination, is readable; four parallel per-combination lists
#' are not. This turns the first into the second, dropping the knobs a combination omits so they
#' fall through to ner_run()'s own defaults.
#'
#' .tuning is required rather than defaulted. It previously defaulted to an object that is defined
#' nowhere in the project, so a call with no argument failed on lazy evaluation at the point of use
#' rather than at the call site.
#'
#' @param .tuning Named list. One entry per combination token, each a list of knobs.
#' @return A list with .run, .n_process, .batch_size and .timeout, ready for do.call().
ner_run_args <- function(.tuning) {
  pick_ <- function(.knob) purrr::map(.tuning, .knob) |> purrr::compact()
  list(
    .run        = names(.tuning),
    .n_process  = pick_("n_process"),
    .batch_size = pick_("batch_size"),
    .timeout    = pick_("timeout")
  )
}

# 3. Combination tokens, and clearing a scope --------------------------------------------------------------------------

#' Split a combination token into its engine and model
#'
#' The tokens are the vocabulary the whole family speaks in, so parsing them lives in one place. A
#' spaCy or paper token without a model is an error rather than a default, because a silently
#' defaulted model produces a store attributing spans to a method nobody chose.
#'
#' @param .tok Character. One combination token.
#' @return A one-row tibble with Engine and Model.
ner_parse_combo <- function(.tok) {
  if (FALSE) {
    .tok <- "spacy:en_core_web_trf"
  }

  parts_ <- strsplit(.tok, ":", fixed = TRUE)[[1]]
  engine_ <- parts_[1]
  if (!engine_ %in% c("spacy", "lexnlp", "paper")) {
    cli::cli_abort("Unknown engine in {.val {(.tok)}}; expected spacy|lexnlp|paper.")
  }
  if (engine_ == "lexnlp") {
    tibble::tibble(Engine = "lexnlp", Model = "lexnlp")
  } else if (engine_ == "spacy") {
    if (length(parts_) < 2L) cli::cli_abort("spaCy needs a model: {.val {(.tok)}}.")
    tibble::tibble(Engine = "spacy", Model = fs::path_file(paste(parts_[-1], collapse = ":")))
  } else {
    if (length(parts_) < 2L) cli::cli_abort("paper needs a model: {.val {(.tok)}}.")
    tibble::tibble(Engine = "paper", Model = parts_[2])
  }
}


#' Clear a scope from the store so the next run repopulates it
#'
#' The sanctioned lever for anything narrower than a full rebuild. Both tables are cleared in one
#' transaction and never one alone: clearing candidates without the ledger leaves the documents
#' marked done and they are never re-run, and clearing the ledger without the candidates duplicates
#' every span on the next append.
#'
#' Candidates carry no Status, so a status-restricted clear removes candidates for the documents
#' holding that status in the ledger. That mirrors what the append does on a timeout retry.
#'
#' @param .db_path Character. Path to the DuckDB file.
#' @param .run Character. One or more combination tokens.
#' @param .doc_ids Character or NULL. Restrict to these documents.
#' @param .status Character or NULL. Restrict to ledger rows of this status: success, nohit or
#'   timeout.
#' @param .quiet Logical. Suppress the count message.
#' @return Invisibly, a list of the row counts removed.
ner_db_clear <- function(.db_path, .run, .doc_ids = NULL, .status = NULL, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Cache$NerDB
    .run <- "lexnlp"
    .doc_ids <- NULL
    .status <- "timeout"
    .quiet <- FALSE
  }

  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")
  if (!is.null(.status)) {
    .status <- match.arg(.status, c("success", "nohit", "timeout"), several.ok = TRUE)
  }

  combos_ <- purrr::map(.run, ner_parse_combo) |>
    dplyr::bind_rows() |>
    dplyr::distinct()

  con_ <- ner_db_init(.db_path, .quiet = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  # Register the scope predicates as temp tables (no string-building of id lists).
  duckdb::duckdb_register(con_, "ner_clear_combos", as.data.frame(combos_))
  on.exit(duckdb::duckdb_unregister(con_, "ner_clear_combos"), add = TRUE, after = FALSE)

  has_docs_ <- !is.null(.doc_ids)
  if (has_docs_) {
    duckdb::duckdb_register(con_, "ner_clear_docs", data.frame(DocID = unique(.doc_ids)))
    on.exit(duckdb::duckdb_unregister(con_, "ner_clear_docs"), add = TRUE, after = FALSE)
  }
  has_status_ <- !is.null(.status)
  if (has_status_) {
    duckdb::duckdb_register(con_, "ner_clear_status", data.frame(Status = .status))
    on.exit(duckdb::duckdb_unregister(con_, "ner_clear_status"), add = TRUE, after = FALSE)
  }

  combo_pred_ <- "(Engine, Model) IN (SELECT Engine, Model FROM ner_clear_combos)"
  doc_pred_   <- if (has_docs_) " AND DocID IN (SELECT DocID FROM ner_clear_docs)" else ""
  runs_status_pred_ <- if (has_status_) " AND Status IN (SELECT Status FROM ner_clear_status)" else ""
  cand_status_pred_ <- if (has_status_) {
    paste0(
      " AND DocID IN (SELECT DocID FROM runs r ",
      "WHERE r.Engine = candidates.Engine AND r.Model = candidates.Model ",
      "AND r.Status IN (SELECT Status FROM ner_clear_status))"
    )
  } else ""

  DBI::dbBegin(con_)
  ok_ <- FALSE
  on.exit(if (!ok_) DBI::dbRollback(con_), add = TRUE, after = FALSE)

  # candidates first: its Status gate reads runs, so delete it before runs is touched.
  n_cand_ <- DBI::dbExecute(con_, paste0(
    "DELETE FROM candidates WHERE ", combo_pred_, doc_pred_, cand_status_pred_
  ))
  n_runs_ <- DBI::dbExecute(con_, paste0(
    "DELETE FROM runs WHERE ", combo_pred_, doc_pred_, runs_status_pred_
  ))

  DBI::dbCommit(con_)
  ok_ <- TRUE

  status_msg_ <- if (has_status_) paste0(" [status: ", paste(.status, collapse = "/"), "]") else ""
  docs_msg_   <- if (has_docs_) paste0(" [", length(unique(.doc_ids)), " doc(s)]") else ""
  if (!.quiet) {
    cli::cli_alert_success(
      paste0("Cleared {n_runs_} ledger row(s) and {n_cand_} candidate(s) across ",
             "{nrow(combos_)} combo(s){status_msg_}{docs_msg_}. Re-run ner_run() to repopulate.")
    )
  }
  return(invisible(list(runs = n_runs_, candidates = n_cand_, combos = combos_)))
}
