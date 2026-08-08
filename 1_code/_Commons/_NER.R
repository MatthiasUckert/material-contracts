# NER Wrappers ---------------------------------------------------------------------------------------------------------
# Run extract_spacy.py over parquet input(s); returns the output path.
# Every CLI flag is exposed; optional flags are only appended when non-default.
# .max_chars truncates each doc to its first N chars before extraction (NULL = off);
# docs still over spaCy's max_length are windowed inside the extractor.
# .timeout is a per-window stall guard (seconds; 0 = off): if no window completes in
# time, the extractor switches to sequential processing and skips offending windows
# (stderr names DocID + window) -- a multi-day pass can never hang silently.
# If .output already exists it is skipped (cheap guard; the real skip logic is
# the DuckDB ledger) unless .overwrite = TRUE.
ner_spacy <- function(
    .inputs,
    .output,
    .id_col = "DocID",
    .text_col = "TextRaw",
    .labels = c("ORG", "GPE", "DATE", "MONEY"), # NULL -> omit --label -> keep all
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
    .labels <- c("ORG", "GPE", "DATE", "MONEY")
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

# Run extract_lexnlp.py inside the container over parquet input(s); returns the output path.
# Mounts the inputs' common base read-only at /work and the output dir at /out, then
# translates host paths into the mount. Every CLI flag is exposed. GPE is policy-excluded:
# LexNLP's geoentity pass is the throughput killer; geography comes from spaCy + gazetteer.
# .max_chars truncates each doc to its first N chars before extraction (NULL = off).
# .timeout caps every extractor on every document (seconds; 0 = off): LexNLP's maxent
# NER / date grammar can spin pathologically on rare documents; on timeout the
# extractor is skipped for that doc (stderr names the DocID), the doc still lands in
# the output, and the run continues. If .output already exists it is skipped (cheap
# guard; the real skip logic is the DuckDB ledger) unless .overwrite = TRUE.
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



# Run extract_dateregex.py over parquet input(s); returns the output path.
# The paper's eight date patterns, ported. The script stamps Engine = "paper",
# Model = "dateregex-v1" (constants in extract_dateregex.py -- the tag identifies
# the pattern set; revising patterns means bumping MODEL there). Runs in the
# contracts-engine venv; single process (pure regex, fast). If .output already
# exists it is skipped (cheap guard; the real skip logic is the DuckDB ledger)
# unless .overwrite = TRUE.
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

# Run extract_gazetteer.py over parquet input(s); returns the output path.
# The paper's USGS+countries place-name lookup, ported with a hierarchy +
# proximity gate (Engine = "paper", Model = "gazetteer-v1", Label = "GPE",
# LabelRaw = GeoClass). Runs in the contracts-engine venv; PhraseMatcher matches
# case-insensitively against TextRaw (NOT TextMod -- offsets must index the
# canonical text; the matcher lowercases internally). .state_window / .word_window
# are the proximity windows (distinctive vs common-word gated names; sample-tuned
# defaults, sweep later). .timeout caps matching per doc (marker row on timeout).
# If .output exists it is skipped unless .overwrite = TRUE.
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

# Resolve input path(s) -- files and/or folders (folders globbed recursively for
# *.parquet) -- to a flat, unique parquet file list. Mirrors the Python extractors.
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

# Unified NER orchestrator: runs the requested engine x model combos over the
# input parquet(s) and folds everything into the DuckDB store. Per combo: ask the
# ledger what's missing (skip if nothing) -> slice the missing docs by
# .docs_per_run (sorted DocIDs; bounds Python memory) -> per slice: slim filtered
# input (DuckDB COPY, text never in R) -> extractor -> append -> cleanup.
#
# Combos are requested via .run, a vector of tokens:
#   "spacy:<model>"        -- a spaCy model (REQUIRED; bare "spacy" errors)
#   "lexnlp"               -- LexNLP (fixed Model "lexnlp")
#   "paper:dateregex-v1"   -- paper date regexes (Engine "paper", Label DATE)
#   "paper:gazetteer-v1"   -- paper place gazetteer (Engine "paper", Label GPE)
# The "paper" engine groups the paper's own ported extractors (provenance axis
# for the head-to-head: paper vs spacy vs lexnlp), separated by Model. The token
# is the combo's identity everywhere: ner_arg keys, staging names, runs ledger.
# (extract_dateregex.py / extract_gazetteer.py must stamp the matching Engine/
# Model into their parquet -- "paper"/"dateregex-v1" and "paper"/"gazetteer-v1".
# ner_db_append asserts this match and aborts on drift, so a desync fails loud
# instead of re-running the combo forever.)
#
# Crash recovery: staging names are chunk-index-free; after a crash the recomputed
# first slice equals the interrupted one, and the file guard reuses the survivor.
# Device "auto": CNN -> cpu (parallel), *_trf -> auto (Python resolves GPU, one
# process).
#
# Per-combo knobs -- .labels, .n_process, .batch_size, .timeout -- each take a
# scalar/vector (broadcast to all) OR a named list keyed by "engine" and/or
# "engine:model" (most specific wins; see ner_arg). .labels = NULL uses each
# combo's policy default. Knob applicability: n_process = CPU workers (spacy CNN,
# lexnlp, gazetteer; ignored on trf/dateregex); batch_size = spaCy nlp.pipe batch
# / LexNLP + gazetteer pool chunksize (dateregex ignores); timeout = stall guard
# secs, 0 = off (dateregex ignores). Best-practice values documented separately.
#
# Stall protection: spaCy window stall -> sequential fallback, offenders skipped;
# LexNLP extractor over cap / gazetteer doc over cap -> skipped for that doc. Any
# skip -> runs.Status = 'timeout'; .retry_timeout = TRUE re-runs exactly those docs.
#
# NOTE: the ledger is label-, max_chars-, and model-label-blind -- a doc ingested
# under a given label set / .max_chars counts as done for that (Engine, Model).
# Keep .labels and .max_chars FIXED per store.
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
    .quiet <- FALSE
  }
  
  # Per-combo label policy -- fallback when .labels doesn't name a combo. Keyed
  # by "engine" and/or "engine:model"; engine:model wins (paper's two models
  # differ: dateregex -> DATE, gazetteer -> GPE).
  labels_policy_ <- list(
    "spacy"              = c("ORG", "GPE", "DATE", "MONEY"),
    "lexnlp"             = c("ORG", "DATE", "MONEY"),
    "paper:dateregex-v1" = "DATE",
    "paper:gazetteer-v1" = "GPE"
  )
  
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
        cli::cli_abort("paper needs a model: {.val {(.tok)}} -> {.val paper:dateregex-v1} or {.val paper:gazetteer-v1}")
      }
      model_ <- parts_[2]
      if (!model_ %in% c("dateregex-v1", "gazetteer-v1")) {
        cli::cli_abort("Unknown paper model {.val {model_}}; expected dateregex-v1|gazetteer-v1.")
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
    default_labels_ <- if (!is.null(labels_policy_[[key_em_]])) {
      labels_policy_[[key_em_]]
    } else {
      labels_policy_[[engine_]]
    }
    labels_ <- ner_arg(.labels, engine_, model_, .default = default_labels_)
    n_process_ <- ner_arg(.n_process, engine_, model_, .default = 16L)
    batch_ <- ner_arg(.batch_size, engine_, model_, .default = 64L)
    timeout_ <- ner_arg(.timeout, engine_, model_, .default = 0L)
    
    for (s_ in seq_along(slices_)) {
      ids_ <- slices_[[s_]]
      if (!.quiet && length(slices_) > 1L) {
        cli::cli_alert_info("{engine_}/{model_}: slice {s_}/{length(slices_)} ({length(ids_)} doc{?s})")
      }
      
      if (!fs::file_exists(stage_)) {
        con_tmp_ <- DBI::dbConnect(duckdb::duckdb())
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
          .batch_size = batch_, .n_process = n_process_, .quiet = .quiet
        )
      } else if (engine_ == "lexnlp") {
        ner_lexnlp(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = labels_, .max_chars = .max_chars, .timeout = timeout_,
          .chunk_size = batch_, .n_process = n_process_, .quiet = .quiet
        )
      } else if (engine_ == "paper" && model_ == "dateregex-v1") {
        ner_dateregex(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = labels_, .max_chars = .max_chars, .quiet = .quiet
        )
      } else if (engine_ == "paper" && model_ == "gazetteer-v1") {
        ner_gazetteer(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = labels_, .max_chars = .max_chars, .timeout = timeout_,
          .n_process = n_process_, .chunk_size = batch_, .quiet = .quiet
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
      "ner_run complete: {sum(out_$Candidates)} candidate(s) over {sum(out_$Docs)} doc combo(s) ({nrow(out_)} engine/model combo(s))."
    )
  }
  return(invisible(out_))
}


# Resolve a possibly-per-engine / per-model argument for one engine x model combo.
# .x is one of:
#   - unnamed (scalar OR vector) -> broadcast to every combo
#   - named list/vector keyed by "engine" and/or "engine:model" -> most specific
#     wins (an exact "engine:model" key beats a bare "engine" key)
#   - NULL -> .default
ner_arg <- function(.x, .engine, .model, .default = NULL) {
  if (is.null(.x)) return(.default)
  if (is.null(names(.x))) return(.x) # unnamed -> broadcast (length 1 or N)
  key_em_ <- paste0(.engine, ":", .model)
  if (key_em_ %in% names(.x)) return(.x[[key_em_]])
  if (.engine %in% names(.x)) return(.x[[.engine]])
  return(.default)
}


# DuckDB ---------------------------------------------------------------------------------------------------------------
# Open (or create) the NER candidate DuckDB store and ensure its schema exists.
# Returns a live DBI connection; caller disconnects with
# DBI::dbDisconnect(con, shutdown = TRUE). Safe to call repeatedly (idempotent DDL).
ner_db_init <- function(.db_path, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .quiet <- FALSE
  }
  
  exists_ <- fs::file_exists(.db_path)
  fs::dir_create(fs::path_dir(.db_path))
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path))
  
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

# Append one engine staging parquet to the store (creating it if needed). Derives
# everything from the parquet (DocID, Engine, Model columns). Per doc, Status is:
#   timeout  -- a marker row present (LabelRaw LIKE 'timeout:%')
#   success  -- >=1 real candidate (non-null Start)
#   nohit    -- sentinel only
# Marker AND sentinel rows are dropped before the `candidates` insert; only real
# hits land there. runs gets one row per (DocID, Engine, Model) with its Status.
#
# Default: insert-only -- combos already in `runs` are skipped (re-appending the
# same file is a no-op). With .retry_timeout = TRUE, docs currently in `runs` as
# 'timeout' for this combo are first deleted from BOTH tables (scoped to the
# staged docs), then re-inserted with their new status -- the only path that
# deletes. Both inserts (and the retry delete) run in one transaction.
#
# Identity guard: .expect_engine / .expect_model (optional). When set, the
# parquet's stamped (Engine, Model) MUST match -- otherwise the function aborts
# before writing. This catches the silent-infinite-re-run failure mode where an
# extractor's stamped identity drifts from the .run token it was dispatched for
# (e.g. a script still stamping "regex"/"paper-v1" while ner_run queries
# "paper"/"dateregex-v1"): the misfiled rows would never satisfy the ledger
# query, so the combo reruns forever. ner_run always passes the expected pair.
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
    msg_ <- "Appended {n_cand_} candidate(s) over {nrow(to_ingest_)} of {n_all_} doc combo(s) from {.path {fs::path_file(.parquet)}}"
    if (n_retry_ > 0L) msg_ <- paste0(msg_, " (incl. {n_retry_} timeout retr{?y/ies})")
    cli::cli_alert_success(msg_)
  }
  return(invisible(list(docs = nrow(to_ingest_), candidates = n_cand_, retried = n_retry_)))
}
# DocIDs in the input parquet(s) not yet ingested for (.engine, .model).
# A DocID is missing if absent from `runs`, OR -- when .retry_timeout = TRUE --
# present with Status = 'timeout' (an incomplete extraction to retry). DuckDB
# scans only the DocID column; anti-join + status filter against the ledger.
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
    cli::cli_alert_info("{(.engine)}/{(.model)}: {length(missing_)} of {n_all_} doc(s) missing{if (isTRUE(.retry_timeout)) ' (incl. timeout retries)' else ''}.")
  }
  return(missing_)
}
# Brute-force benchmark of the NER wrappers over a settings grid.
# Each grid row runs the wrapper on .inputs with output to a temp file
# (.overwrite = TRUE), recording wall time and -- for spaCy -- peak total RSS
# across all extract_spacy.py worker processes (background ps sampler, 0.5s).
# LexNLP memory is NA (containers run inside the Docker VM; host RSS is
# meaningless). A failing setting (OOM, crash) is recorded with NA and the
# sweep continues -- so the grid can deliberately overshoot the machine.
#
# .grid columns: Engine ("spacy"/"lexnlp"), Model, Device, NProcess, BatchSize,
# ChunkSize (NA where not applicable).
ner_bench <- function(.inputs, .grid, .id_col = "DocID", .text_col = "TextRaw") {
  if (FALSE) {
    .inputs <- fil_sample_dirs$Path[20]
    .grid <- bench_grid
    .id_col <- "DocID"
    .text_col <- "TextRaw"
  }
  
  # docs in scope, once (for docs/s)
  files_ <- ner_input_files(.inputs)
  con_tmp_ <- DBI::dbConnect(duckdb::duckdb())
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
    cli::cli_alert_success("  {results_[[i_]]$Secs}s ({results_[[i_]]$DocsPerSec} docs/s), peak {results_[[i_]]$PeakRssMb} MB")
  }
  
  dplyr::bind_rows(results_) |>
    dplyr::arrange(Engine, Model, Secs)
}


# Offset round-trip gate: rebuild every candidate's span from its [Start, Stop)
# offsets via stringi::stri_sub (code points -- never base substr, see MasterDoc
# §11) against the source documents, and compare to the stored Span. Works for
# ANY generator output (needs only DocID/Start/Stop/Span). .inputs = the source
# parquet(s) the candidates were extracted from (files/folders; DocID = file
# basename). Returns the summary tibble; OkShare must be 1.
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

# Explode a combo-keyed tuning bundle into the ner_run() argument list:
#   list(.run, .n_process, .batch_size, .timeout), each knob a per-combo named list
#   with NULL entries dropped (a knob a combo omits falls to ner_run's default).
# Use directly via do.call(ner_run, c(list(.inputs, .db_path, ...), ner_run_args())).
ner_run_args <- function(.tuning = ner_tuning_default) {
  pick_ <- function(.knob) purrr::map(.tuning, .knob) |> purrr::compact()
  list(
    .run        = names(.tuning),
    .n_process  = pick_("n_process"),
    .batch_size = pick_("batch_size"),
    .timeout    = pick_("timeout")
  )
}


# Overviews ------------------------------------------------------------------------------------------------------------
# NER store overview --------------------------------------------------------------------------------------------------
# Read-only summaries of the candidate store. Returns a list of tibbles:
#   $ledger    -- per engine:model: docs run, by Status, hit rate (from `runs`)
#   $labels    -- per engine:model x Label: candidate + distinct-doc counts
#   $lengths   -- document length summary (needs .inputs)
#   $positions -- per-candidate relative position in [0,1] (needs .inputs):
#                 midpoint (Start+Stop)/2 over length(TextRaw), code points.
# Run AFTER ner_run() finishes -- opens the store read-only (DuckDB single-writer).
# .inputs is the same parquet path you fed ner_run(); omit it to skip the
# length-based pieces. Caveat: with .max_chars truncation, docs longer than the
# cap have candidates only in their first cap chars, so their far-right bins
# under-fill against the full-length denominator -- flag if you want a cap-aware one.
ner_overview <- function(.db_path,
                         .inputs = NULL,
                         .id_col = "DocID",
                         .text_col = "TextRaw",
                         .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Cache$NerDB
    .inputs <- .lP$Input$SampleContracts
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .quiet <- FALSE
  }
  
  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")
  
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
  
  add_combo_ <- function(.df) {
    dplyr::mutate(.df, Combo = dplyr::if_else(Engine == Model, Engine, paste0(Engine, ":", Model)))
  }
  
  runs_ <- dplyr::tbl(con_, "runs")
  cand_ <- dplyr::tbl(con_, "candidates")
  
  # Ledger: docs run per combo, split by Status, with the hit rate.
  ledger_ <- runs_ |>
    dplyr::group_by(Engine, Model) |>
    dplyr::summarise(
      Docs    = dplyr::n(),
      Success = sum(Status == "success", na.rm = TRUE),
      NoHit   = sum(Status == "nohit",   na.rm = TRUE),
      Timeout = sum(Status == "timeout", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect()
  
  cand_combo_ <- cand_ |>
    dplyr::group_by(Engine, Model) |>
    dplyr::summarise(
      Candidates  = dplyr::n(),
      DocsWithHit = dplyr::n_distinct(DocID),
      .groups = "drop"
    ) |>
    dplyr::collect()
  
  ledger_out_ <- ledger_ |>
    dplyr::left_join(cand_combo_, by = c("Engine", "Model")) |>
    dplyr::mutate(
      Candidates    = dplyr::coalesce(Candidates, 0L),
      DocsWithHit   = dplyr::coalesce(DocsWithHit, 0L),
      HitRate       = dplyr::if_else(Docs > 0L, Success / Docs, NA_real_),
      CandPerHitDoc = dplyr::if_else(DocsWithHit > 0L, Candidates / DocsWithHit, NA_real_)
    ) |>
    add_combo_() |>
    dplyr::relocate(Combo) |>
    dplyr::arrange(Engine, Model)
  
  # Label mix per combo.
  labels_out_ <- cand_ |>
    dplyr::group_by(Engine, Model, Label) |>
    dplyr::summarise(Candidates = dplyr::n(), Docs = dplyr::n_distinct(DocID), .groups = "drop") |>
    dplyr::collect() |>
    add_combo_() |>
    dplyr::relocate(Combo) |>
    dplyr::arrange(Engine, Model, dplyr::desc(Candidates))
  
  lengths_out_ <- NULL
  positions_out_ <- NULL
  
  if (!is.null(.inputs)) {
    files_ <- ner_input_files(.inputs)
    files_sql_ <- paste0("'", files_, "'", collapse = ", ")
    DBI::dbExecute(con_, paste0(
      "CREATE OR REPLACE TEMP VIEW ov_lengths AS ",
      "SELECT \"", .id_col, "\" AS DocID, length(\"", .text_col, "\") AS DocLen ",
      "FROM read_parquet([", files_sql_, "])"
    ))
    lens_ <- dplyr::tbl(con_, "ov_lengths")
    
    lengths_out_ <- lens_ |>
      dplyr::collect() |>
      dplyr::summarise(
        NDocs     = dplyr::n(),
        MinLen    = min(DocLen),
        MedianLen = stats::median(DocLen),
        MeanLen   = mean(DocLen),
        MaxLen    = max(DocLen)
      )
    
    positions_out_ <- cand_ |>
      dplyr::inner_join(lens_, by = "DocID") |>
      dplyr::filter(DocLen > 0) |>
      dplyr::transmute(
        Engine, Model, Label,
        Rel = ((Start + Stop) / 2.0) / DocLen
      ) |>
      dplyr::collect() |>
      add_combo_() |>
      dplyr::relocate(Combo)
  }
  
  if (!.quiet) {
    cli::cli_alert_success(
      "Overview: {sum(ledger_out_$Candidates)} candidate(s) over {nrow(ledger_out_)} combo(s){if (is.null(.inputs)) ' (no .inputs -> length/position skipped)' else ''}."
    )
  }
  
  list(
    ledger    = ledger_out_,
    labels    = labels_out_,
    lengths   = lengths_out_,
    positions = positions_out_
  )
}

# Positional histogram: where candidates sit relative to full document length.
# .positions is ner_overview()$positions. Faceted by Label; filled by engine:model
# so you can see whether the engines agree on where a label-type lives (e.g. dates
# clustering at the signature block, parties/ORGs near the top).
ner_plot_positions <- function(.positions, .bins = 30L, .by_combo = TRUE, .free_y = TRUE) {
  if (is.null(.positions) || nrow(.positions) == 0L) {
    cli::cli_abort("No positions to plot (did you pass .inputs to ner_overview()?).")
  }
  map_ <- if (.by_combo) {
    ggplot2::aes(x = Rel, fill = Combo)
  } else {
    ggplot2::aes(x = Rel)
  }
  ggplot2::ggplot(.positions, map_) +
    ggplot2::geom_histogram(bins = .bins, boundary = 0, colour = NA) +
    ggplot2::facet_wrap(~ Label, scales = if (.free_y) "free_y" else "fixed") +
    ggplot2::scale_x_continuous(limits = c(-0.001, 1.001), labels = scales::label_percent()) +
    ggplot2::labs(
      x = "Relative position in document (0% = start, 100% = end)",
      y = "Candidate count",
      fill = "Engine:Model",
      title = "Where NER candidates sit relative to full document length"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

# Cross-engine agreement on the candidate store ----------------------------------------------------------------------
# Cross-engine agreement on the candidate store (DuckDB-side) ---------------------------------------------------------
# Same idea as before -- within each (DocID, Label), merge overlapping spans into a
# "mention" and record which engine:model combos contributed -- but the heavy work
# (interval merge, mention aggregation, pairwise co-occurrence) runs in DuckDB via
# window functions + a self-join; R only collects the small summaries. Returns:
#   $mentions   -- one row per merged mention: bounds, NCands, NCombos, the combo set,
#                  a representative Span (widest candidate). Sorted most-agreed first.
#   $consensus  -- per Label: mentions found by 1, 2, ... combos vs CombosEligible.
#   $pairwise   -- per Label x combo-pair: Both, each combo's count, Jaccard =
#                  Both / (A + B - Both). All eligible pairs listed (0 if never overlap).
# Agreement = ANY overlap (recall-scaffold notion; LLM adjudicates precision).
# Transitive merge can chain dense adjacent spans -- tighten to exact/IoU later if
# needed. Read-only; scratch lives in temp tables (verified OK on a read-only conn).
ner_alignment <- function(.db_path, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Cache$NerDB
    .quiet <- FALSE
  }
  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")
  
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
  
  if (DBI::dbGetQuery(con_, "SELECT COUNT(*) AS n FROM candidates")$n == 0L) {
    cli::cli_abort("No candidates in the store yet.")
  }
  
  # 1) Per-candidate, tagged with a MentionID = merged overlap cluster within
  #    (DocID, Label). Gaps-and-islands: a new cluster starts when Start is at/after
  #    the running max end of all PRIOR spans in the ordered group (half-open spans).
  DBI::dbExecute(con_, "
    CREATE TEMP TABLE ner_align AS
    WITH base AS (
      SELECT DocID, Label,
             CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END AS Combo,
             Start, Stop, Span, (Stop - Start) AS Width
      FROM candidates
    ),
    lagged AS (
      SELECT *, MAX(Stop) OVER (PARTITION BY DocID, Label ORDER BY Start, Stop
                  ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS PrevMaxEnd
      FROM base
    ),
    flagged AS (
      SELECT *, CASE WHEN PrevMaxEnd IS NULL OR Start >= PrevMaxEnd THEN 1 ELSE 0 END AS IsNew
      FROM lagged
    ),
    clustered AS (
      SELECT *, SUM(IsNew) OVER (PARTITION BY DocID, Label ORDER BY Start, Stop
                  ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS Cluster
      FROM flagged
    )
    SELECT DocID, Label, Combo, Start, Stop, Span, Width,
           DocID || '::' || Label || '::' || CAST(Cluster AS VARCHAR) AS MentionID
    FROM clustered
  ")
  
  # 2) One row per mention: bounds, counts, ordered distinct combo set, widest Span.
  DBI::dbExecute(con_, "
    CREATE TEMP TABLE ner_mentions AS
    WITH mc AS (SELECT DISTINCT MentionID, Combo FROM ner_align),
    mc_agg AS (
      SELECT MentionID, COUNT(*) AS NCombos,
             string_agg(Combo, ' | ' ORDER BY Combo) AS Combos
      FROM mc GROUP BY MentionID
    ),
    sp AS (
      SELECT MentionID, any_value(DocID) AS DocID, any_value(Label) AS Label,
             MIN(Start) AS Start, MAX(Stop) AS Stop, COUNT(*) AS NCands,
             arg_max(Span, Width) AS Span
      FROM ner_align GROUP BY MentionID
    )
    SELECT sp.MentionID, sp.DocID, sp.Label, sp.Start, sp.Stop,
           sp.NCands, mc_agg.NCombos, mc_agg.Combos, sp.Span
    FROM sp JOIN mc_agg USING (MentionID)
  ")
  
  mentions_ <- DBI::dbGetQuery(con_, "
    SELECT DocID, Label, Start, Stop, NCands, NCombos, Combos, Span
    FROM ner_mentions ORDER BY NCombos DESC, DocID, Start
  ") |> tibble::as_tibble()
  
  # 3) Consensus: mentions by NCombos per label, vs combos eligible for that label.
  consensus_ <- DBI::dbGetQuery(con_, "
    SELECT Label, NCombos, COUNT(*) AS NMentions FROM ner_mentions GROUP BY Label, NCombos
  ") |>
    dplyr::left_join(
      DBI::dbGetQuery(con_, "
        SELECT Label, COUNT(*) AS CombosEligible
        FROM (SELECT DISTINCT Label, Combo FROM ner_align) GROUP BY Label
      "),
      by = "Label"
    ) |>
    dplyr::group_by(Label) |>
    dplyr::mutate(ShareOfMentions = NMentions / sum(NMentions)) |>
    dplyr::ungroup() |>
    dplyr::arrange(Label, NCombos) |>
    tibble::as_tibble()
  
  # 4) Pairwise: co-occurrence (DB self-join) + per-combo counts; all eligible pairs
  #    assembled in R (tiny) so non-overlapping pairs show Jaccard 0.
  combo_counts_ <- DBI::dbGetQuery(con_, "
    SELECT Label, Combo, COUNT(DISTINCT MentionID) AS Mentions FROM ner_align GROUP BY Label, Combo
  ") |> tibble::as_tibble()
  cooc_ <- DBI::dbGetQuery(con_, "
    WITH m AS (SELECT DISTINCT MentionID, Label, Combo FROM ner_align)
    SELECT a.Label, a.Combo AS ComboA, b.Combo AS ComboB, COUNT(*) AS Both
    FROM m a JOIN m b ON a.MentionID = b.MentionID AND a.Combo < b.Combo
    GROUP BY a.Label, a.Combo, b.Combo
  ") |> tibble::as_tibble()
  
  pairwise_ <- combo_counts_ |>
    dplyr::select(Label, ComboA = Combo) |>
    dplyr::inner_join(dplyr::select(combo_counts_, Label, ComboB = Combo),
                      by = "Label", relationship = "many-to-many") |>
    dplyr::filter(ComboA < ComboB) |>
    dplyr::left_join(cooc_, by = c("Label", "ComboA", "ComboB")) |>
    dplyr::mutate(Both = dplyr::coalesce(Both, 0L)) |>
    dplyr::left_join(dplyr::rename(combo_counts_, N_A = Mentions), by = c("Label", "ComboA" = "Combo")) |>
    dplyr::left_join(dplyr::rename(combo_counts_, N_B = Mentions), by = c("Label", "ComboB" = "Combo")) |>
    dplyr::mutate(Jaccard = Both / (N_A + N_B - Both)) |>
    dplyr::arrange(Label, dplyr::desc(Jaccard))
  
  if (!.quiet) {
    multi_ <- mean(mentions_$NCombos >= 2L)
    cli::cli_alert_success(
      "Alignment: {nrow(mentions_)} mention(s); {scales::percent(multi_, accuracy = 0.1)} found by >= 2 combos."
    )
  }
  
  list(mentions = mentions_, consensus = consensus_, pairwise = pairwise_)
}

# Pairwise agreement heatmap: Jaccard between combos, faceted by Label.
ner_plot_agreement <- function(.pairwise, .digits = 2L) {
  if (is.null(.pairwise) || nrow(.pairwise) == 0L) {
    cli::cli_abort("No pairwise agreement to plot (need a label produced by >= 2 combos).")
  }
  ggplot2::ggplot(.pairwise, ggplot2::aes(x = ComboA, y = ComboB, fill = Jaccard)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = formatC(Jaccard, format = "f", digits = .digits)),
      size = 3
    ) +
    ggplot2::facet_wrap(~ Label, scales = "free") +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1), name = "Jaccard") +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = "Cross-engine agreement (Jaccard on overlapping mentions)"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}


# Highlight / QA rendering functions (html_escape, md_escape, ner_span_class,
# resolve_overlaps, weave_spans, ner_highlight*) were lifted out of this file and
# will be cleaned up in a separate pass. They are not needed for extraction or
# the DuckDB store.


# Per-class profiling + targeted re-run ------------------------------------------------------------------------------
# What the candidate store finds per contract type, plus a surgical store-clear for
# narrow re-runs. Peers of ner_overview / ner_alignment: read-only DuckDB scan,
# DB-side aggregation, only small tibbles cross the boundary.

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


# Surgically clear a scope from the store so the next ner_run() repopulates it
# cleanly. The sanctioned "force re-run" lever for anything narrower than a full
# rebuild (MasterDoc section 15/16 left this deferred). Clears BOTH tables in one
# transaction -- never runs alone, or insert-only append would duplicate candidates.
#
#   .run      one or more engine / engine:model tokens (as ner_run).
#   .doc_ids  optional: restrict to these DocIDs.
#   .status   optional: restrict to ledger rows of this Status (success|nohit|
#             timeout). candidates have no Status, so they are cleared only for the
#             DocIDs carrying that Status in runs (mirrors the append retry delete).
#
# Examples:
#   ner_db_clear(db, "lexnlp")                       # wipe the whole lexnlp combo
#   ner_db_clear(db, "lexnlp", .status = "timeout")  # wipe only the stuck docs
#   ner_db_clear(db, "spacy:en_core_web_trf", .doc_ids = bad_ids)
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
      "Cleared {n_runs_} ledger row(s) and {n_cand_} candidate(s) across {nrow(combos_)} combo(s){status_msg_}{docs_msg_}. Re-run ner_run() to repopulate."
    )
  }
  return(invisible(list(runs = n_runs_, candidates = n_cand_, combos = combos_)))
}


# What the NER layer finds per contract type. Joins the candidate store to a
# DocID -> Class map (Ann-Kristin's classification labels) and reports, per class:
#   $docs        NDocs per class (the per-doc denominator)
#   $coverage    how many ledger docs got a class at all (NRunDocs / NClassed / Pct)
#   $profile     per Class x Combo x Label: Candidates, DocsWithHit, CandPerDoc,
#                PctDocsWithHit  (long, tidy -- the full breakdown)
#   $fingerprint Class x Label wide matrix of CandPerDoc for ONE reference combo
#                (.ref_combo) -- the readable "fingerprint" table
#   $consensus   Class x Label high-confidence (>= .min_combos engines) per-doc rate,
#                ONLY if .mentions (= ner_alignment()$mentions) is supplied. This is
#                engine-agnostic (merged spans), so it avoids the cross-engine
#                double-count -- the most defensible per-type rate for the paper.
#
#   .class_parquet  parquet(s) with at least [.id_col, .class_col].
#   .class_col      label column (default ClassDetailed; swap for ClassBroad etc.).
#   .run            optional combo filter (engine / engine:model tokens); NULL = all.
ner_profile_by_class <- function(.db_path,
                                 .class_parquet,
                                 .class_col = "ClassDetailed",
                                 .id_col = "DocID",
                                 .run = NULL,
                                 .ref_combo = "spacy:en_core_web_trf",
                                 .mentions = NULL,
                                 .min_combos = 2L,
                                 .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Cache$NerDB
    .class_parquet <- .lP$Input$ClassificationSample
    .class_col <- "ClassDetailed"
    .id_col <- "DocID"
    .run <- NULL
    .ref_combo <- "spacy:en_core_web_trf"
    .mentions <- NULL # or .al$mentions from ner_alignment()
    .min_combos <- 2L
    .quiet <- FALSE
  }

  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")

  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  add_combo_ <- function(.df) {
    dplyr::mutate(.df, Combo = dplyr::if_else(Engine == Model, Engine, paste0(Engine, ":", Model)))
  }

  # Class map: DocID -> Class. Validate the column exists, fail loudly with the
  # available names if .class_col is wrong (cheap LIMIT 0 schema probe).
  files_ <- ner_input_files(.class_parquet)
  files_sql_ <- paste0("'", files_, "'", collapse = ", ")
  avail_ <- names(DBI::dbGetQuery(con_, paste0(
    "SELECT * FROM read_parquet([", files_sql_, "]) LIMIT 0"
  )))
  if (!.class_col %in% avail_) {
    cli::cli_abort(c(
      "Class column {.val {(.class_col)}} not found in {.arg .class_parquet}.",
      "i" = "Available columns: {paste(avail_, collapse = ', ')}"
    ))
  }
  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TEMP VIEW ner_class AS ",
    "SELECT \"", .id_col, "\" AS DocID, CAST(\"", .class_col, "\" AS VARCHAR) AS Class ",
    "FROM read_parquet([", files_sql_, "]) ",
    "WHERE \"", .class_col, "\" IS NOT NULL"
  ))
  class_ <- dplyr::tbl(con_, "ner_class")
  class_tbl_ <- dplyr::collect(class_) # small (~n docs); reused for the .mentions join

  runs_ <- dplyr::tbl(con_, "runs")
  cand_ <- dplyr::tbl(con_, "candidates")

  # Optional combo filter (restricts both ledger and candidates).
  if (!is.null(.run)) {
    combos_ <- purrr::map(.run, ner_parse_combo) |>
      dplyr::bind_rows() |>
      dplyr::distinct()
    duckdb::duckdb_register(con_, "ner_prof_combos", as.data.frame(combos_))
    on.exit(duckdb::duckdb_unregister(con_, "ner_prof_combos"), add = TRUE, after = FALSE)
    keep_ <- dplyr::tbl(con_, "ner_prof_combos")
    runs_ <- dplyr::semi_join(runs_, keep_, by = c("Engine", "Model"))
    cand_ <- dplyr::semi_join(cand_, keep_, by = c("Engine", "Model"))
  }

  # Per-class processed-doc count (the denominator). One row per (DocID, Class);
  # combos all cover the same doc set, so this is combo-independent.
  docs_ <- runs_ |>
    dplyr::distinct(DocID) |>
    dplyr::inner_join(class_, by = "DocID") |>
    dplyr::group_by(Class) |>
    dplyr::summarise(NDocs = dplyr::n_distinct(DocID), .groups = "drop") |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(NDocs))

  n_run_docs_ <- runs_ |>
    dplyr::summarise(n = dplyr::n_distinct(DocID)) |>
    dplyr::pull(n)
  n_classed_ <- sum(docs_$NDocs)
  coverage_ <- tibble::tibble(
    NRunDocs   = as.integer(n_run_docs_),
    NClassed   = as.integer(n_classed_),
    PctClassed = if (n_run_docs_ > 0L) n_classed_ / n_run_docs_ else NA_real_
  )

  # Per Class x Combo x Label.
  prof_ <- cand_ |>
    dplyr::inner_join(class_, by = "DocID") |>
    dplyr::group_by(Class, Engine, Model, Label) |>
    dplyr::summarise(
      Candidates  = dplyr::n(),
      DocsWithHit = dplyr::n_distinct(DocID),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::left_join(docs_, by = "Class") |>
    dplyr::mutate(
      CandPerDoc     = Candidates / NDocs,
      PctDocsWithHit = DocsWithHit / NDocs
    ) |>
    add_combo_() |>
    dplyr::relocate(Class, Combo, Engine, Model, Label) |>
    dplyr::arrange(Class, Combo, dplyr::desc(Candidates))

  # Readable fingerprint: one reference combo, Class x Label, CandPerDoc.
  ref_ <- ner_parse_combo(.ref_combo)
  fingerprint_ <- prof_ |>
    dplyr::filter(Engine == ref_$Engine[1], Model == ref_$Model[1]) |>
    dplyr::select(Class, Label, CandPerDoc) |>
    tidyr::pivot_wider(names_from = Label, values_from = CandPerDoc, values_fill = 0) |>
    dplyr::left_join(docs_, by = "Class") |>
    dplyr::relocate(Class, NDocs) |>
    dplyr::arrange(dplyr::desc(NDocs))
  if (nrow(fingerprint_) == 0L && !.quiet) {
    cli::cli_alert_warning("Reference combo {.val {(.ref_combo)}} not present -> empty fingerprint.")
  }

  # High-confidence (>= .min_combos engines agree) per-type rate, from merged
  # mentions. Engine-agnostic, so no double counting across engines.
  consensus_ <- NULL
  if (!is.null(.mentions)) {
    consensus_ <- .mentions |>
      dplyr::filter(NCombos >= .min_combos) |>
      dplyr::inner_join(class_tbl_, by = "DocID") |>
      dplyr::group_by(Class, Label) |>
      dplyr::summarise(
        HiConfMentions = dplyr::n(),
        DocsWithHit    = dplyr::n_distinct(DocID),
        .groups = "drop"
      ) |>
      dplyr::left_join(docs_, by = "Class") |>
      dplyr::mutate(
        HiConfPerDoc   = HiConfMentions / NDocs,
        PctDocsWithHit = DocsWithHit / NDocs
      ) |>
      dplyr::arrange(Class, dplyr::desc(HiConfMentions))
  }

  if (!.quiet) {
    cli::cli_alert_success(
      "Profiled {nrow(docs_)} class(es) over {coverage_$NClassed} classified doc(s) ({scales::label_percent(0.1)(coverage_$PctClassed)} of the ledger){if (is.null(.mentions)) ' (pass .mentions for the consensus view)' else ''}."
    )
    if (!is.na(coverage_$PctClassed) && coverage_$PctClassed < 0.9) {
      cli::cli_alert_warning("{scales::label_percent(0.1)(1 - coverage_$PctClassed)} of ledger docs have no class -- check .class_parquet / .class_col coverage.")
    }
  }

  list(
    docs        = docs_,
    coverage    = coverage_,
    profile     = prof_,
    fingerprint = fingerprint_,
    consensus   = consensus_
  )
}


# Heatmap of the per-class fingerprint: Class (rows) x Label (cols), filled by the
# chosen metric. .profile is ner_profile_by_class()$profile. Pass .combo to pick one
# engine:model (else it facets across combos). .metric: CandPerDoc | PctDocsWithHit.
ner_plot_profile <- function(.profile, .combo = NULL,
                             .metric = c("CandPerDoc", "PctDocsWithHit"),
                             .digits = 1L) {
  if (FALSE) {
    .profile <- .prof$profile
    .combo <- "spacy:en_core_web_trf"
    .metric <- "CandPerDoc"
    .digits <- 1L
  }

  .metric <- match.arg(.metric)
  if (is.null(.profile) || nrow(.profile) == 0L) cli::cli_abort("Empty .profile.")

  df_ <- .profile
  if (!is.null(.combo)) df_ <- dplyr::filter(df_, Combo == .combo)
  if (nrow(df_) == 0L) cli::cli_abort("No rows for combo {.val {(.combo)}}.")

  # Pull the requested metric into a single column for the fill aesthetic.
  df_ <- df_ |> dplyr::mutate(Value = .data[[.metric]])
  is_pct_ <- identical(.metric, "PctDocsWithHit")
  lab_fun_ <- if (is_pct_) scales::label_percent(1) else scales::label_number(accuracy = 10^(-.digits))

  p_ <- ggplot2::ggplot(df_, ggplot2::aes(x = Label, y = Class, fill = Value)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = lab_fun_(Value)), size = 3) +
    ggplot2::scale_fill_viridis_c(option = "mako", direction = -1, labels = lab_fun_) +
    ggplot2::labs(
      x = "Entity label", y = "Contract type",
      fill = if (is_pct_) "Docs w/ hit" else "Per doc",
      title = paste0("NER yield per contract type (", .metric, ")")
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  if (is.null(.combo) && dplyr::n_distinct(df_$Combo) > 1L) {
    p_ <- p_ + ggplot2::facet_wrap(~ Combo)
  }
  p_
}


# HTML export -- one interactive file per doc, every model in it ------------------------------------------------------
# Reads the candidate store directly (all engines), breaks each doc's text at every
# candidate boundary into atomic segments, and tags each segment with the labels and
# engine:model combos covering it. The page has a Model and a Label dropdown that
# live-filter the highlighted body AND the candidate list; a Model x Label count
# matrix sits up top. Spans found by a single model are dimmed when viewing "all
# models" so agreement stands out. Self-contained (inline CSS/JS), read-only store.

html_escape <- function(.x) {
  .x |>
    stringi::stri_replace_all_fixed("&", "&amp;") |>
    stringi::stri_replace_all_fixed("<", "&lt;") |>
    stringi::stri_replace_all_fixed(">", "&gt;") |>
    stringi::stri_replace_all_fixed("\"", "&quot;")
}

ner_html_css <- function() {
  r"---(
:root{--c:#374151;--bg:#e5e7eb;}
body{font-family:ui-sans-serif,system-ui,sans-serif;max-width:1000px;margin:24px auto;padding:0 16px;color:#111827;}
h2,h3{margin:.6em 0 .3em;} .meta{color:#6b7280;font-size:13px;}
code{background:#f3f4f6;padding:1px 4px;border-radius:3px;}
.controls{position:sticky;top:0;background:#fff;padding:10px 0;border-bottom:1px solid #e5e7eb;display:flex;gap:18px;align-items:center;z-index:5;}
.controls select{font-size:14px;padding:3px 6px;}
table.sum{border-collapse:collapse;font-size:13px;margin:8px 0;}
table.sum th,table.sum td{border:1px solid #e5e7eb;padding:3px 8px;text-align:right;}
table.sum th:first-child,table.sum td:first-child{text-align:left;}
table.sum th{background:#f9fafb;}
.doc{white-space:pre-wrap;font-family:ui-monospace,Menlo,monospace;font-size:13px;line-height:1.8;background:#fff;border:1px solid #e5e7eb;border-radius:6px;padding:16px;}
.ner{border-radius:3px;padding:0 1px;cursor:default;}
.ner.off{background:transparent!important;color:inherit!important;box-shadow:none!important;opacity:1!important;}
.ner[data-show="ORG"]{background:#d1fae5;color:#065f46;}
.ner[data-show="DATE"]{background:#e0e7ff;color:#3730a3;}
.ner[data-show="MONEY"]{background:#fef3c7;color:#92400e;}
.ner[data-show="GPE"]{background:#ede9fe;color:#5b21b6;}
.ner.solo{opacity:.45;}
.ner[data-multi]:not(.off){box-shadow:inset 0 -2px 0 rgba(0,0,0,.28);}
details{margin-top:14px;} summary{cursor:pointer;color:#374151;font-size:14px;}
table.cand{border-collapse:collapse;width:100%;font-size:12px;margin-top:8px;}
table.cand th,table.cand td{border:1px solid #e5e7eb;padding:3px 6px;text-align:left;vertical-align:top;}
table.cand th{background:#f9fafb;}
)---"
}

ner_html_js <- function() {
  r"---(
(function(){
  var fM=document.getElementById('fModel'), fL=document.getElementById('fLabel');
  var spans=document.querySelectorAll('.doc .ner');
  var rows=document.querySelectorAll('#cand tbody tr');
  function apply(){
    var m=fM.value, l=fL.value, allM=(m==='');
    spans.forEach(function(el){
      var models=el.dataset.models.split(' ');
      var labels=el.dataset.labels.split(',');
      var on=(m===''||models.indexOf(m)>=0)&&(l===''||labels.indexOf(l)>=0);
      el.classList.toggle('off',!on);
      var lab=(l!==''&&labels.indexOf(l)>=0)?l:labels[0];
      el.setAttribute('data-show',lab);
      el.classList.toggle('solo', on&&allM&&el.dataset.n==='1');
    });
    rows.forEach(function(tr){
      var on=(m===''||tr.dataset.model===m)&&(l===''||tr.dataset.label===l);
      tr.style.display=on?'':'none';
    });
  }
  fM.addEventListener('change',apply); fL.addEventListener('change',apply); apply();
})();
)---"
}

# Weave the text into atomic segments; covered segments become tagged spans.
ner_html_body <- function(.text, .cands) {
  n_ <- stringi::stri_length(.text)
  if (nrow(.cands) == 0L) return(html_escape(.text))
  prio_ <- c("ORG", "GPE", "DATE", "MONEY")
  bounds_ <- sort(unique(c(0L, n_, .cands$Start, .cands$Stop)))
  bounds_ <- bounds_[bounds_ >= 0L & bounds_ <= n_]
  pieces_ <- character(length(bounds_) - 1L)
  for (i_ in seq_len(length(bounds_) - 1L)) {
    a_ <- bounds_[i_]
    b_ <- bounds_[i_ + 1L]
    if (b_ <= a_) next
    seg_ <- stringi::stri_sub(.text, a_ + 1L, b_)
    cov_ <- which(.cands$Start <= a_ & .cands$Stop >= b_)
    if (length(cov_) == 0L) {
      pieces_[i_] <- html_escape(seg_)
      next
    }
    labs_ <- unique(.cands$Label[cov_])
    labs_ <- c(intersect(prio_, labs_), sort(setdiff(labs_, prio_)))
    combos_ <- sort(unique(.cands$Combo[cov_]))
    title_ <- paste0(
      paste(labs_, collapse = "/"), " \u00b7 ", length(combos_),
      if (length(combos_) > 1L) " models: " else " model: ", paste(combos_, collapse = ", ")
    )
    multi_ <- if (length(labs_) > 1L) " data-multi=\"1\"" else ""
    pieces_[i_] <- paste0(
      "<span class=\"ner\"",
      " data-labels=\"", paste(labs_, collapse = ","), "\"",
      " data-models=\"", paste(combos_, collapse = " "), "\"",
      " data-n=\"", length(combos_), "\"",
      " data-show=\"", labs_[1], "\"", multi_,
      " title=\"", html_escape(title_), "\">",
      html_escape(seg_), "</span>"
    )
  }
  paste(pieces_, collapse = "")
}

# Model x Label count matrix (with row/column totals).
ner_html_summary <- function(.cands, .labs, .combos) {
  cnt_ <- dplyr::count(.cands, Combo, Label)
  cell_ <- function(.c, .l) {
    v_ <- cnt_$n[cnt_$Combo == .c & cnt_$Label == .l]
    if (length(v_) == 0L) 0L else v_
  }
  head_ <- paste0("<tr><th>Model</th>", paste0("<th>", .labs, "</th>", collapse = ""), "<th>Total</th></tr>")
  body_ <- vapply(.combos, function(.c) {
    vals_ <- vapply(.labs, function(.l) cell_(.c, .l), integer(1))
    paste0("<tr><td>", html_escape(.c), "</td>",
           paste0("<td>", vals_, "</td>", collapse = ""),
           "<td>", sum(vals_), "</td></tr>")
  }, character(1))
  tot_ <- vapply(.labs, function(.l) sum(cnt_$n[cnt_$Label == .l]), integer(1))
  foot_ <- paste0("<tr><th>Total</th>", paste0("<th>", tot_, "</th>", collapse = ""),
                  "<th>", sum(tot_), "</th></tr>")
  paste0("<table class=\"sum\"><thead>", head_, "</thead><tbody>",
         paste(body_, collapse = ""), "</tbody><tfoot>", foot_, "</tfoot></table>")
}

# Full self-contained HTML for one document.
ner_html_doc <- function(.docid, .text, .cands) {
  prio_ <- c("ORG", "GPE", "DATE", "MONEY")
  labs_ <- unique(.cands$Label)
  labs_ <- c(intersect(prio_, labs_), sort(setdiff(labs_, prio_)))
  combos_ <- sort(unique(.cands$Combo))
  
  body_ <- ner_html_body(.text, .cands)
  summary_ <- ner_html_summary(.cands, labs_, combos_)
  
  model_opts_ <- paste0("<option value=\"", html_escape(combos_), "\">",
                        html_escape(combos_), "</option>", collapse = "")
  label_opts_ <- paste0("<option value=\"", labs_, "\">", labs_, "</option>", collapse = "")
  
  rows_ <- .cands |> dplyr::arrange(Start, Stop) |> dplyr::mutate(Index = dplyr::row_number())
  cand_rows_ <- paste0(
    "<tr data-model=\"", html_escape(rows_$Combo), "\" data-label=\"", rows_$Label, "\">",
    "<td>", rows_$Index, "</td>",
    "<td>", rows_$Start, "-", rows_$Stop, "</td>",
    "<td>", rows_$Label, "</td>",
    "<td>", html_escape(rows_$Combo), "</td>",
    "<td>", html_escape(rows_$Span), "</td></tr>",
    collapse = ""
  )
  
  paste0(
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<title>NER \u2014 ", html_escape(.docid), "</title><style>", ner_html_css(), "</style></head><body>",
    "<h2>NER candidates \u2014 <code>", html_escape(.docid), "</code></h2>",
    "<p class=\"meta\">", nrow(.cands), " candidate(s) \u00b7 ", length(combos_),
    " model(s) \u00b7 spans break at every candidate boundary; a span lists all models that flagged it.</p>",
    summary_,
    "<div class=\"controls\">",
    "<label>Model <select id=\"fModel\"><option value=\"\">All models</option>", model_opts_, "</select></label>",
    "<label>Label <select id=\"fLabel\"><option value=\"\">All labels</option>", label_opts_, "</select></label>",
    "</div>",
    "<div class=\"doc\" id=\"doc\">", body_, "</div>",
    "<details><summary>Candidate list (", nrow(.cands), ")</summary>",
    "<table class=\"cand\" id=\"cand\"><thead><tr><th>#</th><th>Span</th><th>Label</th><th>Model</th><th>Text</th></tr></thead><tbody>",
    cand_rows_, "</tbody></table></details>",
    "<script>", ner_html_js(), "</script></body></html>"
  )
}

# Orchestrator: read candidates + TextRaw, write one HTML per doc (+ optional index).
ner_export_html <- function(.db_path, .inputs, .dir_out,
                            .doc_ids = NULL, .labels = NULL, .run = NULL,
                            .index = TRUE, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Cache$NerDB
    .inputs <- .lP$Input$SampleContracts
    .dir_out <- file.path(.lP$Cache$NerDB |> fs::path_dir(), "Highlight")
    .doc_ids <- NULL
    .labels <- NULL
    .run <- NULL
    .index <- TRUE
    .quiet <- FALSE
  }
  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")
  fs::dir_create(.dir_out)
  
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
  
  q_ <- dplyr::tbl(con_, "candidates")
  if (!is.null(.doc_ids)) q_ <- dplyr::filter(q_, DocID %in% .doc_ids)
  if (!is.null(.labels))  q_ <- dplyr::filter(q_, Label %in% .labels)
  cand_ <- q_ |>
    dplyr::select(DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model) |>
    dplyr::collect() |>
    dplyr::mutate(Combo = dplyr::if_else(Engine == Model, Engine, paste0(Engine, ":", Model)))
  if (!is.null(.run)) cand_ <- dplyr::filter(cand_, Combo %in% .run)
  if (nrow(cand_) == 0L) {
    cli::cli_alert_warning("No candidates for that filter \u2014 nothing to export.")
    return(tibble::tibble())
  }
  
  ids_ <- unique(cand_$DocID)
  texts_ <- arrow::open_dataset(.inputs) |>
    dplyr::filter(DocID %in% ids_) |>
    dplyr::select(DocID, TextRaw) |>
    dplyr::collect()
  
  paths_ <- purrr::map(ids_, function(.d) {
    text_ <- texts_$TextRaw[texts_$DocID == .d]
    if (length(text_) == 0L) {
      cli::cli_alert_warning("No TextRaw for {(.d)}; skipped.")
      return(NULL)
    }
    text_ <- text_[[1]]
    cands_d_ <- dplyr::filter(cand_, DocID == .d)
    
    bad_ <- sum(stringi::stri_sub(text_, cands_d_$Start + 1L, cands_d_$Stop) != cands_d_$Span, na.rm = TRUE)
    if (bad_ > 0L) cli::cli_alert_danger("{(.d)}: {bad_} span(s) fail round-trip \u2014 offsets/encoding suspect.")
    
    safe_id_ <- stringi::stri_replace_all_regex(.d, "[^A-Za-z0-9._-]", "_")
    p_ <- fs::path(.dir_out, paste0(safe_id_, ".html"))
    readr::write_file(ner_html_doc(.d, text_, cands_d_), p_)
    tibble::tibble(DocID = .d, Candidates = nrow(cands_d_), Path = as.character(p_))
  }) |>
    purrr::compact() |>
    purrr::list_rbind()
  
  if (isTRUE(.index) && nrow(paths_) > 0L) {
    links_ <- paste0(
      "<li><a href=\"", fs::path_file(paths_$Path), "\">", html_escape(paths_$DocID),
      "</a> <span class=\"meta\">(", paths_$Candidates, " candidates)</span></li>",
      collapse = ""
    )
    idx_ <- paste0(
      "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>NER export</title>",
      "<style>", ner_html_css(), "</style></head><body><h2>NER export</h2>",
      "<p class=\"meta\">", nrow(paths_), " document(s).</p><ul>", links_, "</ul></body></html>"
    )
    readr::write_file(idx_, fs::path(.dir_out, "index.html"))
  }
  
  if (!.quiet) cli::cli_alert_success("Wrote {nrow(paths_)} HTML file(s) to {.path {(.dir_out)}}.")
  return(paths_)
}