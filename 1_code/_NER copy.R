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



# Highlight / QA rendering ----

html_escape <- function(.x) {
  .x |>
    stringi::stri_replace_all_fixed("&", "&amp;") |>
    stringi::stri_replace_all_fixed("<", "&lt;") |>
    stringi::stri_replace_all_fixed(">", "&gt;") |>
    stringi::stri_replace_all_fixed("\"", "&quot;")
}

md_escape <- function(.x) {
  out_ <- stringi::stri_replace_all_fixed(.x, "\\", "\\\\") # backslash first
  for (ch_ in c("*", "_", "`", "~", "[", "]", "<", ">")) {
    out_ <- stringi::stri_replace_all_fixed(out_, ch_, paste0("\\", ch_))
  }
  out_
}

# CSS class for a span: colour by label, applied solid/dotted/dashed by verdict.
# CSS class for a span: colour by label; verdict (if any) sets solid/dotted/dashed.
ner_span_class <- function(.label, .keep, .has_verdict = TRUE) {
  known_ <- .label %in% c("DATE", "ORG", "MONEY", "GPE")
  base_ <- ifelse(known_, paste0("ner ner-", tolower(.label)), "ner ner-other")
  state_ <- if (!isTRUE(.has_verdict)) {
    rep(" neutral", length(.label)) # no Ollama layer -> just show the tag, coloured
  } else {
    dplyr::case_when(.keep %in% TRUE ~ " kept", .keep %in% FALSE ~ " dropped", TRUE ~ " na")
  }
  paste0(base_, state_)
}

# Longest-span-wins: accept longest first, skip anything overlapping an accepted span.
resolve_overlaps <- function(.spans) {
  if (FALSE) {
    .spans <- dplyr::filter(res_ollama, DocID == res_ollama$DocID[[1]])
  }
  if (nrow(.spans) <= 1L) {
    return(.spans)
  }
  ord_ <- dplyr::arrange(.spans, dplyr::desc(Stop - Start), Start)
  
  starts_ <- integer(0)
  stops_ <- integer(0)
  keep_ <- logical(nrow(ord_))
  for (i_ in seq_len(nrow(ord_))) {
    s_ <- ord_$Start[[i_]]
    e_ <- ord_$Stop[[i_]]
    if (!any(s_ < stops_ & starts_ < e_)) { # [s,e) overlaps [s2,e2) iff s<e2 & s2<e
      keep_[[i_]] <- TRUE
      starts_ <- c(starts_, s_)
      stops_ <- c(stops_, e_)
    }
  }
  ord_ |>
    dplyr::filter(keep_) |>
    dplyr::arrange(Start)
}

# Walk the text, alternating plain runs and marked spans (0-based half-open offsets).
weave_spans <- function(.text, .spans, .render_plain, .render_span) {
  n_ <- stringi::stri_length(.text)
  cursor_ <- 0L
  pieces_ <- character(0)
  for (i_ in seq_len(nrow(.spans))) {
    s_ <- .spans$Start[[i_]]
    e_ <- .spans$Stop[[i_]]
    if (s_ > cursor_) {
      pieces_ <- c(pieces_, .render_plain(stringi::stri_sub(.text, cursor_ + 1L, s_)))
    }
    pieces_ <- c(pieces_, .render_span(stringi::stri_sub(.text, s_ + 1L, e_), .spans[i_, ]))
    cursor_ <- e_
  }
  if (cursor_ < n_) {
    pieces_ <- c(pieces_, .render_plain(stringi::stri_sub(.text, cursor_ + 1L, n_)))
  }
  paste(pieces_, collapse = "")
}

ner_highlight_css <- function() {
  paste(
    "body{font-family:ui-sans-serif,system-ui,sans-serif;max-width:980px;margin:24px auto;padding:0 16px;color:#111827;}",
    "h2,h3{margin:.6em 0 .3em;}",
    ".meta{color:#6b7280;font-size:13px;}",
    "code{background:#f3f4f6;padding:1px 4px;border-radius:3px;}",
    ".legend{margin:12px 0;display:flex;gap:8px;flex-wrap:wrap;}",
    ".doc{white-space:pre-wrap;font-family:ui-monospace,Menlo,monospace;font-size:13px;line-height:1.7;background:#fff;border:1px solid #e5e7eb;border-radius:6px;padding:16px;}",
    ".ner{border-radius:3px;padding:0 2px;}",
    ".ner-date{--c:#3730a3;--bg:#e0e7ff;} .ner-org{--c:#065f46;--bg:#d1fae5;}",
    ".ner-money{--c:#92400e;--bg:#fef3c7;} .ner-gpe{--c:#5b21b6;--bg:#ede9fe;} .ner-other{--c:#374151;--bg:#e5e7eb;}",
    ".neutral{background:var(--bg);color:var(--c);}",
    ".kept{background:var(--bg);color:var(--c);font-weight:600;}",
    ".dropped{color:#9ca3af;border-bottom:2px dotted var(--c);}",
    ".na{outline:1px dashed #9ca3af;}",
    "table{border-collapse:collapse;width:100%;font-size:13px;margin-top:8px;}",
    "th,td{border:1px solid #e5e7eb;padding:4px 8px;text-align:left;vertical-align:top;}",
    "th{background:#f9fafb;}",
    sep = "\n"
  )
}

ner_highlight_html_doc <- function(.docid, .text, .spans_all, .spans_body) {
  has_keep_ <- "Keep" %in% names(.spans_all)
  render_plain_ <- function(.t) html_escape(.t)
  render_span_ <- function(.t, .meta) {
    lab_ <- .meta$Label[[1]]
    keep_ <- if (has_keep_) .meta$Keep[[1]] else NA
    kind_ <- if (has_keep_) .meta$Kind[[1]] else NA_character_
    value_ <- if (has_keep_) .meta$Value[[1]] else NA_character_
    tip_ <- paste0(
      lab_, " \u00b7 ", .meta$Engines[[1]],
      if (has_keep_) paste0(" \u00b7 keep=", keep_) else "",
      if (!is.na(kind_) && nzchar(kind_)) paste0(" \u00b7 ", kind_) else "",
      if (!is.na(value_) && nzchar(value_)) paste0(" \u00b7 ", value_) else ""
    )
    paste0(
      "<span class=\"", ner_span_class(lab_, keep_, has_keep_),
      "\" title=\"", html_escape(tip_), "\">", html_escape(.t), "</span>"
    )
  }
  body_ <- weave_spans(.text, .spans_body, render_plain_, render_span_)
  counts_ <- .spans_all |>
    dplyr::summarise(
      N = dplyr::n(),
      K = if (has_keep_) sum(Keep, na.rm = TRUE) else NA_integer_,
      .by = Label
    ) |>
    dplyr::arrange(Label)
  leg_lab_ <- ifelse(is.na(counts_$K),
                     paste0(counts_$Label, ": ", counts_$N),
                     paste0(counts_$Label, ": ", counts_$N, " (", counts_$K, " kept)")
  )
  legend_ <- paste(paste0(
    "<span class=\"", ner_span_class(counts_$Label, TRUE), "\">",
    html_escape(leg_lab_), "</span>"
  ), collapse = " ")
  keep_col_ <- if (has_keep_) ifelse(is.na(.spans_all$Keep), "", as.character(.spans_all$Keep)) else rep("", nrow(.spans_all))
  kind_col_ <- if (has_keep_) ifelse(is.na(.spans_all$Kind), "", .spans_all$Kind) else rep("", nrow(.spans_all))
  value_col_ <- if (has_keep_) ifelse(is.na(.spans_all$Value), "", .spans_all$Value) else rep("", nrow(.spans_all))
  rows_ <- paste0(
    "<tr><td>", .spans_all$Index,
    "</td><td><span class=\"", ner_span_class(.spans_all$Label, TRUE), "\">", html_escape(.spans_all$Label),
    "</span></td><td>", html_escape(.spans_all$Span),
    "</td><td>", html_escape(.spans_all$Engines),
    "</td><td>", html_escape(keep_col_),
    "</td><td>", html_escape(kind_col_),
    "</td><td>", html_escape(value_col_), "</td></tr>"
  )
  table_ <- paste0(
    "<table><thead><tr><th>#</th><th>Label</th><th>Span</th><th>Engines</th>",
    "<th>Keep</th><th>Kind</th><th>Value</th></tr></thead><tbody>",
    paste(rows_, collapse = ""), "</tbody></table>"
  )
  paste0(
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>NER \u2014 ",
    html_escape(.docid), "</title><style>", ner_highlight_css(), "</style></head><body>",
    "<h2>NER highlight</h2>",
    "<p class=\"meta\">DocID: <code>", html_escape(.docid), "</code> \u00b7 ", nrow(.spans_all),
    " candidates \u00b7 body = longest-span-wins on overlaps; table is complete.</p>",
    "<div class=\"legend\">", legend_, "</div>",
    "<div class=\"doc\">", body_, "</div>",
    "<h3>Verdicts</h3>", table_, "</body></html>"
  )
}

ner_highlight_md_doc <- function(.docid, .text, .spans_all, .spans_body) {
  has_keep_ <- "Keep" %in% names(.spans_all)
  
  render_plain_ <- function(.t) md_escape(.t)
  render_span_ <- function(.t, .meta) {
    keep_ <- if (has_keep_) .meta$Keep[[1]] else NA
    inner_ <- paste0("\u00ab", md_escape(.t), "\u00bb")
    if (isTRUE(keep_)) {
      paste0("**", inner_, "**")
    } else if (isFALSE(keep_)) {
      paste0("~~", inner_, "~~")
    } else {
      inner_
    }
  }
  
  body_ <- weave_spans(.text, .spans_body, render_plain_, render_span_) |>
    stringi::stri_replace_all_fixed("\n", "  \n") # hard breaks keep contract layout
  
  counts_ <- .spans_all |>
    dplyr::summarise(
      N = dplyr::n(),
      K = if (has_keep_) sum(Keep, na.rm = TRUE) else NA_integer_,
      .by = Label
    ) |>
    dplyr::arrange(Label)
  legend_ <- paste(
    ifelse(is.na(counts_$K),
           paste0("**", counts_$Label, "** ", counts_$N),
           paste0("**", counts_$Label, "** ", counts_$N, " (", counts_$K, " kept)")
    ),
    collapse = " \u00b7 "
  )
  
  clean_cell_ <- function(.x) {
    .x |>
      md_escape() |>
      stringi::stri_replace_all_fixed("|", "\\|") |>
      stringi::stri_replace_all_regex("\\s+", " ")
  }
  keep_col_ <- if (has_keep_) ifelse(is.na(.spans_all$Keep), "", as.character(.spans_all$Keep)) else rep("", nrow(.spans_all))
  kind_col_ <- if (has_keep_) ifelse(is.na(.spans_all$Kind), "", .spans_all$Kind) else rep("", nrow(.spans_all))
  value_col_ <- if (has_keep_) ifelse(is.na(.spans_all$Value), "", .spans_all$Value) else rep("", nrow(.spans_all))
  rows_ <- paste0(
    "| ", .spans_all$Index, " | ", .spans_all$Label,
    " | ", clean_cell_(.spans_all$Span), " | ", .spans_all$Engines,
    " | ", keep_col_, " | ", clean_cell_(kind_col_), " | ", clean_cell_(value_col_), " |"
  )
  table_ <- paste(c(
    "| # | Label | Span | Engines | Keep | Kind | Value |",
    "|---|---|---|---|---|---|---|", rows_
  ), collapse = "\n")
  
  paste(
    paste0("# NER highlight \u2014 `", .docid, "`"), "",
    paste0(
      nrow(.spans_all),
      " candidates \u00b7 \u00abspan\u00bb marks every candidate, **bold** = kept, ~~strike~~ = dropped \u00b7 body = longest-span-wins on overlaps; table is complete."
    ),
    "", paste0("**Labels:** ", legend_), "",
    "## Document", "", body_, "",
    "## Verdicts", "", table_, "",
    sep = "\n"
  )
}

# Loop: read TextRaw for the docs in .verdicts, write one HTML + one MD per contract ----
ner_highlight <- function(.verdicts, .inputs, .dir_out, .formats = c("html", "md")) {
  if (FALSE) {
    .verdicts <- res_ollama
    .inputs <- list.files(.lP$Input$SampleContracts, full.names = TRUE)[20]
    .dir_out <- file.path(.lP$Cache$NerTest, "Highlight")
    .formats <- c("html", "md")
  }
  
  fs::dir_create(.dir_out)
  doc_ids_ <- unique(.verdicts$DocID)
  
  texts_ <- arrow::open_dataset(.inputs) |>
    dplyr::filter(DocID %in% doc_ids_) |>
    dplyr::select(DocID, TextRaw) |>
    dplyr::collect()
  
  paths_ <- purrr::map(doc_ids_, \(.d) {
    text_ <- texts_$TextRaw[texts_$DocID == .d]
    if (length(text_) == 0L) {
      cli::cli_alert_warning("No TextRaw for {(.d)}; skipped.")
      return(NULL)
    }
    text_ <- text_[[1]]
    
    spans_all_ <- .verdicts |>
      dplyr::filter(DocID == .d) |>
      dplyr::arrange(Start) |>
      dplyr::mutate(Index = dplyr::row_number())
    
    bad_ <- sum(stringi::stri_sub(text_, spans_all_$Start + 1L, spans_all_$Stop) != spans_all_$Span, na.rm = TRUE)
    if (bad_ > 0L) {
      cli::cli_alert_danger("{(.d)}: {bad_} span(s) fail round-trip \u2014 offsets/encoding suspect.")
    }
    
    spans_body_ <- resolve_overlaps(spans_all_)
    safe_id_ <- stringi::stri_replace_all_regex(.d, "[^A-Za-z0-9._-]", "_")
    
    out_ <- list()
    if ("html" %in% .formats) {
      p_ <- file.path(.dir_out, paste0(safe_id_, "_highlight.html"))
      readr::write_file(ner_highlight_html_doc(.d, text_, spans_all_, spans_body_), p_)
      out_ <- c(out_, list(tibble::tibble(DocID = .d, Format = "html", Path = p_)))
    }
    if ("md" %in% .formats) {
      p_ <- file.path(.dir_out, paste0(safe_id_, "_highlight.md"))
      readr::write_file(ner_highlight_md_doc(.d, text_, spans_all_, spans_body_), p_)
      out_ <- c(out_, list(tibble::tibble(DocID = .d, Format = "md", Path = p_)))
    }
    dplyr::bind_rows(out_)
  }) |>
    purrr::compact() |>
    purrr::list_rbind()
  
  cli::cli_alert_success("Wrote {nrow(paths_)} file(s) to {(.dir_out)}.")
  return(paths_)
}

# Highlight straight from the store (pre-Ollama overview) ----
ner_highlight_store <- function(.db_path, .doc_ids = NULL, .inputs, .dir_out,
                                .run = NULL, .labels = .ollama_labels,
                                .formats = c("html", "md")) {
  if (FALSE) {
    .db_path <- file.path(.lP$Cache$NerExtraction, "SampleNER.duckdb")
    .doc_ids <- NULL # NULL = all docs in store
    .inputs <- list.files(.lP$Input$SampleContracts, full.names = TRUE)[20]
    .dir_out <- file.path(.lP$Cache$NerTest, "Highlight")
    .run <- "lexnlp" # NULL = all engines collapsed; or a ner_run token
    .labels <- .ollama_labels
    .formats <- c("html", "md")
  }
  
  cand_ <- ollama_read_candidates(.db_path, .doc_ids, .labels, .run)
  if (nrow(cand_) == 0L) {
    cli::cli_alert_warning("No candidates in store for that filter \u2014 nothing to render.")
    return(tibble::tibble())
  }
  
  # ner_highlight reads TextRaw, resolves overlaps, and writes the files. No Keep
  # column -> every span renders neutral (the highlighter branches on its absence).
  ner_highlight(
    .verdicts = cand_,
    .inputs   = .inputs,
    .dir_out  = .dir_out,
    .formats  = .formats
  )
}
