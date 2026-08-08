


# Unified NER orchestrator: runs the requested engine x model combos over the
# input parquet(s) and folds everything into the DuckDB store, with all I/O
# handled here. Per combo: ask the ledger what's missing (skip if nothing) ->
# process the missing docs in slices of .docs_per_run (sorted DocIDs; memory
# bound for Python, which holds one slice's texts + results at a time) -> per
# slice: write a slim filtered input (DocID + text; DuckDB COPY, text never
# lands in R) -> run the extractor to a deterministic staging parquet -> append
# to the store -> delete the temp files (or archive them under indexed names
# with .keep_staging).
#
# Crash recovery: the staging name is chunk-index-free on purpose. Appended
# slices leave the missing set, so after a crash the recomputed FIRST slice over
# the sorted DocIDs is exactly the interrupted one -- the surviving staging file
# matches by name and the wrappers' file guard skips re-extraction.
#
# Device policy under "auto": CNN models -> cpu (parallel), *_trf -> auto
# (Python resolves the GPU and forces one process).
#
# Labels are explicit per engine (.labels_spacy / .labels_lexnlp), defaulting to
# the locked policy sets (GPE is LexNLP-excluded: its geoentity pass is the
# throughput killer; geography comes from spaCy + gazetteer). NOTE: the ledger
# is label-blind -- a doc ingested under a narrowed label set counts as done for
# that (Engine, Model); keep the label sets fixed per store.
ner_run_OLD1 <- function(
    .inputs,
    .db_path,
    .engines = c("spacy", "lexnlp"),
    .models = "en_core_web_sm", # spaCy model name(s) or path(s); LexNLP is fixed
    .labels_spacy = c("ORG", "GPE", "DATE", "MONEY"),
    .labels_lexnlp = c("ORG", "DATE", "MONEY"),
    .max_chars = NULL,
    .id_col = "DocID",
    .text_col = "TextRaw",
    .docs_per_run = 10000L, # R-side corpus chunking (memory bound; NULL = all at once)
    .device = "auto", # auto|cpu|cuda|mps (spaCy)
    .batch_size = 64L, # spaCy nlp.pipe batch
    .n_process = 1L, # CPU workers (both engines)
    .chunk_size = 8L, # LexNLP docs per worker task
    .keep_staging = FALSE,
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- .lP$Input$SampleContracts
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .engines <- c("spacy", "lexnlp")
    .models <- "en_core_web_sm"
    .labels_spacy <- c("ORG", "GPE", "DATE", "MONEY")
    .labels_lexnlp <- c("ORG", "DATE", "MONEY")
    .max_chars = NULL
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .docs_per_run <- 10000L
    .device <- "auto"
    .batch_size <- 64L
    .n_process <- 20L
    .chunk_size <- 8L
    .keep_staging <- FALSE
    .quiet <- FALSE
  }
  
  engines_ <- match.arg(.engines, c("spacy", "lexnlp"), several.ok = TRUE)
  
  # Engine x model grid. Model = the tag the extractor stamps into the parquet
  # (basename, in case a model path was given); ModelArg = what ner_spacy gets.
  combos_ <- dplyr::bind_rows(
    if ("spacy" %in% engines_) {
      tibble::tibble(Engine = "spacy", Model = fs::path_file(.models), ModelArg = .models)
    },
    if ("lexnlp" %in% engines_) {
      tibble::tibble(Engine = "lexnlp", Model = "lexnlp", ModelArg = "lexnlp")
    }
  )
  
  files_ <- ner_input_files(.inputs)
  tmp_dir_ <- fs::path(fs::path_dir(.db_path), ".ner_tmp")
  fs::dir_create(tmp_dir_)
  
  summary_ <- vector("list", nrow(combos_))
  
  for (i_ in seq_len(nrow(combos_))) {
    engine_ <- combos_$Engine[i_]
    model_ <- combos_$Model[i_]
    
    # Sorted for deterministic slices -- the basis of the crash-recovery contract
    missing_ <- sort(ner_db_missing(
      .db_path, .inputs, engine_, model_,
      .id_col = .id_col, .quiet = .quiet
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
    
    for (s_ in seq_along(slices_)) {
      ids_ <- slices_[[s_]]
      if (!.quiet && length(slices_) > 1L) {
        cli::cli_alert_info("{engine_}/{model_}: slice {s_}/{length(slices_)} ({length(ids_)} doc{?s})")
      }
      
      # Slim filtered input -- skipped when a staging file survived a crash
      # (extraction is then skipped by the wrappers' file guard anyway).
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
          .labels = .labels_spacy, .max_chars = .max_chars,
          .model = combos_$ModelArg[i_], .device = device_,
          .batch_size = .batch_size, .n_process = .n_process, .quiet = .quiet
        )
      } else {
        ner_lexnlp(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = .labels_lexnlp, .max_chars = .max_chars,
          .chunk_size = .chunk_size, .n_process = .n_process, .quiet = .quiet
        )
      }
      
      res_ <- ner_db_append(.db_path, stage_, .quiet = .quiet)
      docs_ <- docs_ + res_$docs
      cands_ <- cands_ + res_$candidates
      
      # Staging MUST leave the working name before the next slice (the file
      # guard would otherwise re-feed it); .keep_staging archives instead.
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



# Run extract_spacy.py over parquet input(s); returns the output path.
# Every CLI flag is exposed; optional flags are only appended when non-default.
# .max_chars truncates each doc to its first N chars before extraction (NULL = off);
# docs still over spaCy's max_length are windowed inside the extractor.
# If .output already exists it is skipped (cheap guard; the real skip logic is
# the DuckDB ledger) unless .overwrite = TRUE.
ner_spacy_OLD1 <- function(
    .inputs,
    .output,
    .id_col = "DocID",
    .text_col = "TextRaw",
    .labels = c("ORG", "GPE", "DATE", "MONEY"), # NULL -> omit --label -> keep all
    .max_chars = NULL, # NULL -> omit --max-chars -> no truncation
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
    .inputs <- list.files(fil_sample_dirs$Path[20], full.names = TRUE)
    .output <- file.path(.lP$Cache$NerTest, "test_spacy_sm.parquet")
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .labels <- c("ORG", "GPE", "DATE", "MONEY")
    .max_chars <- NULL
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
    "--n-process", .n_process
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
# If .output already exists it is skipped (cheap guard; the real skip logic is the
# DuckDB ledger) unless .overwrite = TRUE.
ner_lexnlp_OLD1 <- function(
    .inputs,
    .output,
    .id_col = "DocID",
    .text_col = "TextRaw",
    .labels = c("ORG", "DATE", "MONEY"), # full supported set
    .max_chars = NULL, # NULL -> omit --max-chars -> no truncation
    .geo_config = "/app/geoentities.csv", # in-container path (GPE only)
    .chunk_size = 8L,
    .n_process = 1L,
    .overwrite = FALSE,
    .no_progress = FALSE,
    .image = "contracts-lexnlp",
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- list.files(fil_sample_dirs$Path[20], full.names = TRUE)
    .output <- file.path(.lP$Cache$NerTest, "test_lexnlp.parquet")
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .labels <- c("ORG", "DATE", "MONEY")
    .max_chars <- NULL
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
    "--chunk-size", .chunk_size
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


# Unified NER orchestrator: runs the requested engine x model combos over the
# input parquet(s) and folds everything into the DuckDB store, with all I/O
# handled here. Per combo: ask the ledger what's missing (skip if nothing) ->
# process the missing docs in slices of .docs_per_run (sorted DocIDs; memory
# bound for Python, which holds one slice's texts + results at a time) -> per
# slice: write a slim filtered input (DocID + text; DuckDB COPY, text never
# lands in R) -> run the extractor to a deterministic staging parquet -> append
# to the store -> delete the temp files (or archive them under indexed names
# with .keep_staging).
#
# Crash recovery: the staging name is chunk-index-free on purpose. Appended
# slices leave the missing set, so after a crash the recomputed FIRST slice over
# the sorted DocIDs is exactly the interrupted one -- the surviving staging file
# matches by name and the wrappers' file guard skips re-extraction.
#
# Engines: spacy (models via .models), lexnlp (fixed), regex (the paper's date
# patterns, ported; fixed Model "paper-v1" -- MUST track the MODEL constant in
# extract_dateregex.py; bump both together). Device policy under "auto": CNN
# models -> cpu (parallel), *_trf -> auto (Python resolves the GPU, one process).
#
# Labels are explicit per engine, defaulting to the locked policy sets. NOTE:
# the ledger is label-blind -- a doc ingested under a narrowed label set or
# .max_chars cap counts as done for that (Engine, Model); keep labels and
# .max_chars fixed per store.
#
# Defaults from the 2026-06 M3-Ultra benchmark (MasterDoc §15): batch 64 (must
# stay <= slice/workers or spaCy under-parallelises), n_process 16, chunk 8,
# trf via "auto" -> MPS.
ner_run_OLD2 <- function(
    .inputs,
    .db_path,
    .engines = c("spacy", "lexnlp", "regex"),
    .models = "en_core_web_sm", # spaCy model name(s) or path(s); lexnlp/regex fixed
    .labels_spacy = c("ORG", "GPE", "DATE", "MONEY"),
    .labels_lexnlp = c("ORG", "DATE", "MONEY"),
    .labels_dateregex = "DATE",
    .max_chars = NULL, # truncate docs to first N chars before extraction (NULL = off)
    .id_col = "DocID",
    .text_col = "TextRaw",
    .docs_per_run = 5000L, # R-side corpus chunking (memory bound; NULL = all at once)
    .device = "auto", # auto|cpu|cuda|mps (spaCy)
    .batch_size = 64L, # spaCy nlp.pipe batch
    .n_process = 16L, # CPU workers (spaCy CNN + LexNLP)
    .chunk_size = 8L, # LexNLP docs per worker task
    .keep_staging = FALSE,
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- .lP$Input$SampleContracts
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .engines <- c("spacy", "lexnlp", "regex")
    .models <- "en_core_web_sm"
    .labels_spacy <- c("ORG", "GPE", "DATE", "MONEY")
    .labels_lexnlp <- c("ORG", "DATE", "MONEY")
    .labels_dateregex <- "DATE"
    .max_chars <- NULL
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .docs_per_run <- 5000L
    .device <- "auto"
    .batch_size <- 64L
    .n_process <- 16L
    .chunk_size <- 8L
    .keep_staging <- FALSE
    .quiet <- FALSE
  }
  
  engines_ <- match.arg(.engines, c("spacy", "lexnlp", "regex"), several.ok = TRUE)
  
  # Engine x model grid. Model = the tag the extractor stamps into the parquet
  # (basename, in case a model path was given); ModelArg = what ner_spacy gets.
  combos_ <- dplyr::bind_rows(
    if ("spacy" %in% engines_) {
      tibble::tibble(Engine = "spacy", Model = fs::path_file(.models), ModelArg = .models)
    },
    if ("lexnlp" %in% engines_) {
      tibble::tibble(Engine = "lexnlp", Model = "lexnlp", ModelArg = "lexnlp")
    },
    if ("regex" %in% engines_) {
      tibble::tibble(Engine = "regex", Model = "paper-v1", ModelArg = "regex")
    }
  )
  
  files_ <- ner_input_files(.inputs)
  tmp_dir_ <- fs::path(fs::path_dir(.db_path), ".ner_tmp")
  fs::dir_create(tmp_dir_)
  
  summary_ <- vector("list", nrow(combos_))
  
  for (i_ in seq_len(nrow(combos_))) {
    engine_ <- combos_$Engine[i_]
    model_ <- combos_$Model[i_]
    
    # Sorted for deterministic slices -- the basis of the crash-recovery contract
    missing_ <- sort(ner_db_missing(
      .db_path, .inputs, engine_, model_,
      .id_col = .id_col, .quiet = .quiet
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
    
    for (s_ in seq_along(slices_)) {
      ids_ <- slices_[[s_]]
      if (!.quiet && length(slices_) > 1L) {
        cli::cli_alert_info("{engine_}/{model_}: slice {s_}/{length(slices_)} ({length(ids_)} doc{?s})")
      }
      
      # Slim filtered input -- skipped when a staging file survived a crash
      # (extraction is then skipped by the wrappers' file guard anyway).
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
          .labels = .labels_spacy, .max_chars = .max_chars,
          .model = combos_$ModelArg[i_], .device = device_,
          .batch_size = .batch_size, .n_process = .n_process, .quiet = .quiet
        )
      } else if (engine_ == "lexnlp") {
        ner_lexnlp(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = .labels_lexnlp, .max_chars = .max_chars,
          .chunk_size = .chunk_size, .n_process = .n_process, .quiet = .quiet
        )
      } else {
        ner_dateregex(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = .labels_dateregex, .max_chars = .max_chars,
          .quiet = .quiet
        )
      }
      
      res_ <- ner_db_append(.db_path, stage_, .quiet = .quiet)
      docs_ <- docs_ + res_$docs
      cands_ <- cands_ + res_$candidates
      
      # Staging MUST leave the working name before the next slice (the file
      # guard would otherwise re-feed it); .keep_staging archives instead.
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

# Open (or create) the NER candidate DuckDB store and ensure its schema exists.
# Returns a live DBI connection; caller disconnects with
# DBI::dbDisconnect(con, shutdown = TRUE). Safe to call repeatedly (idempotent DDL).
ner_db_init_OLD1 <- function(.db_path, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .quiet <- FALSE
  }
  
  exists_ <- fs::file_exists(.db_path)
  fs::dir_create(fs::path_dir(.db_path))
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path))
  
  # candidates = real hits only -- the harmonised Python schema (Engine + Model
  # are parquet columns now). Null-span sentinels are dropped before insert, so
  # every column except LabelRaw is NOT NULL. Start/Stop are BIGINT (parquet int64).
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
  
  # runs = the completeness ledger and the authoritative skip source.
  # One row per (DocID, Engine, Model) ingested -- including docs that found
  # nothing (which candidates can't record). The UNIQUE key is the
  # no-double-append backstop.
  DBI::dbExecute(con_, "
    CREATE TABLE IF NOT EXISTS runs (
      DocID     VARCHAR    NOT NULL,
      Engine    VARCHAR    NOT NULL,
      Model     VARCHAR    NOT NULL,
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


# DocIDs in the input parquet(s) not yet ingested for (.engine, .model).
# Accepts files and/or folders (folders globbed recursively for *.parquet).
# DuckDB scans only the DocID column (column-pruned read_parquet; TextRaw never
# leaves disk) and anti-joins the `runs` ledger. character(0) = scope fully done.
# A store that doesn't exist yet is created empty -> everything is missing.
ner_db_missing_OLD1 <- function(.db_path, .inputs, .engine, .model, .id_col = "DocID", .quiet = FALSE) {
  if (FALSE) {
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .inputs <- fil_sample_dirs$Path[20]
    .engine <- "spacy"
    .model <- "en_core_web_sm"
    .id_col <- "DocID"
    .quiet <- FALSE
  }
  
  # Resolve inputs to a flat parquet file list (mirrors the Python extractors)
  paths_ <- fs::path_abs(.inputs)
  files_ <- purrr::map(paths_, \(.p) {
    if (fs::is_dir(.p)) fs::dir_ls(.p, recurse = TRUE, glob = "*.parquet") else .p
  }) |>
    unlist() |>
    unique() |>
    as.character()
  if (length(files_) == 0L) cli::cli_abort("No parquet files found in {.arg .inputs}.")
  
  con_ <- ner_db_init(.db_path, .quiet = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
  
  # Lazy view on the input parquets (the one SQL string; column-pruned scan)
  files_sql_ <- paste0("'", files_, "'", collapse = ", ")
  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TEMP VIEW ner_inputs AS ",
    "SELECT \"", .id_col, "\" AS DocID FROM read_parquet([", files_sql_, "])"
  ))
  
  missing_ <- dplyr::tbl(con_, "ner_inputs") |>
    dplyr::distinct(DocID) |>
    dplyr::anti_join(
      dplyr::tbl(con_, "runs") |>
        dplyr::filter(Engine == !!.engine, Model == !!.model),
      by = "DocID"
    ) |>
    dplyr::pull(DocID)
  
  n_all_ <- dplyr::tbl(con_, "ner_inputs") |>
    dplyr::summarise(n = dplyr::n_distinct(DocID)) |>
    dplyr::pull(n)
  
  if (!.quiet) {
    cli::cli_alert_info("{(.engine)}/{(.model)}: {length(missing_)} of {n_all_} doc(s) missing.")
  }
  return(missing_)
}


# Append one engine staging parquet to the store (creating the store if needed).
# Everything is derived from the parquet itself (DocID, Engine, Model are columns),
# and only the (DocID, Engine, Model) combos not yet in `runs` are inserted --
# already-ingested combos are skipped, so re-appending the same file is a no-op.
# Null-span sentinels are dropped from `candidates` (their job -- "doc processed"
# -- is carried by `runs`). The parquet is never read into R: DuckDB scans it
# lazily via a temp view (read_parquet -- the one SQL string; dplyr has no verb
# for it), and both inserts compile to INSERT..SELECT inside one transaction.
# Crash -> full rollback. The temp view dies with the connection.
ner_db_append_OLD1 <- function(.db_path, .parquet, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .parquet <- file.path(.lP$Cache$NerTest, "test_spacy_sm.parquet")
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
  
  # New combos = parquet combos anti-joined against the ledger (lazy; collected
  # only for the early return / message -- one row per doc at most)
  new_combos_ <- src_ |>
    dplyr::distinct(DocID, Engine, Model) |>
    dplyr::anti_join(dplyr::tbl(con_, "runs"), by = c("DocID", "Engine", "Model"))
  
  n_new_ <- new_combos_ |>
    dplyr::count() |>
    dplyr::pull(n)
  n_all_ <- src_ |>
    dplyr::summarise(n = dplyr::n_distinct(DocID)) |>
    dplyr::pull(n)
  
  if (n_new_ == 0L) {
    if (!.quiet) cli::cli_alert_info("All {n_all_} doc/engine/model combo(s) already in store -- nothing to append.")
    return(invisible(list(docs = 0L, candidates = 0L)))
  }
  
  # Rows to write, still lazy: real hits of the new combos (candidates), and the
  # new combos stamped server-side with the ingest time (runs)
  cand_new_ <- src_ |>
    dplyr::semi_join(new_combos_, by = c("DocID", "Engine", "Model")) |>
    dplyr::filter(!is.na(Start)) |>
    dplyr::select(DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model)
  runs_new_ <- new_combos_ |>
    dplyr::mutate(CreatedAt = !!dbplyr::sql("now()::TIMESTAMP")) |>
    dplyr::select(DocID, Engine, Model, CreatedAt)
  
  n_cand_ <- cand_new_ |>
    dplyr::count() |>
    dplyr::pull(n)
  
  DBI::dbBegin(con_)
  ok_ <- FALSE
  on.exit(if (!ok_) DBI::dbRollback(con_), add = TRUE, after = FALSE)
  
  dplyr::rows_append(dplyr::tbl(con_, "candidates"), cand_new_, in_place = TRUE)
  dplyr::rows_append(dplyr::tbl(con_, "runs"), runs_new_, in_place = TRUE)
  
  DBI::dbCommit(con_)
  ok_ <- TRUE
  
  if (!.quiet) {
    cli::cli_alert_success(
      "Appended {n_cand_} candidate(s) over {n_new_} of {n_all_} doc combo(s) from {.path {fs::path_file(.parquet)}}"
    )
  }
  return(invisible(list(docs = n_new_, candidates = n_cand_)))
}


# Unified NER orchestrator: runs the requested engine x model combos over the
# input parquet(s) and folds everything into the DuckDB store, with all I/O
# handled here. Per combo: ask the ledger what's missing (skip if nothing) ->
# process the missing docs in slices of .docs_per_run (sorted DocIDs; memory
# bound for Python, which holds one slice's texts + results at a time) -> per
# slice: write a slim filtered input (DocID + text; DuckDB COPY, text never
# lands in R) -> run the extractor to a deterministic staging parquet -> append
# to the store -> delete the temp files (or archive them under indexed names
# with .keep_staging).
#
# Crash recovery: the staging name is chunk-index-free on purpose. Appended
# slices leave the missing set, so after a crash the recomputed FIRST slice over
# the sorted DocIDs is exactly the interrupted one -- the surviving staging file
# matches by name and the wrappers' file guard skips re-extraction.
#
# Engines: spacy (models via .models), lexnlp (fixed), regex (the paper's date
# patterns, ported; fixed Model "paper-v1" -- MUST track the MODEL constant in
# extract_dateregex.py; bump both together). Device policy under "auto": CNN
# models -> cpu (parallel), *_trf -> auto (Python resolves the GPU, one process).
#
# Stall protection: .timeout_spacy guards each spaCy window (on a stall the
# extractor degrades to sequential and skips offenders); .timeout_lexnlp caps
# each LexNLP extractor per doc (offender skipped, doc still recorded). Either
# way the doc lands in the output and the ledger marks it done -- no silent
# hangs, no re-hitting the same pathological doc. dateregex needs no guard
# (simple patterns, no backtracking pathology).
#
# Labels are explicit per engine, defaulting to the locked policy sets. NOTE:
# the ledger is label-blind -- a doc ingested under a narrowed label set or
# .max_chars cap counts as done for that (Engine, Model); keep labels and
# .max_chars fixed per store.
#
# Defaults from the 2026-06 M3-Ultra benchmark (MasterDoc §15): batch 64 (must
# stay <= slice/workers or spaCy under-parallelises), n_process 16, chunk 8,
# trf via "auto" -> MPS.
ner_run_OLD3 <- function(
    .inputs,
    .db_path,
    .engines = c("spacy", "lexnlp", "regex"),
    .models = "en_core_web_sm", # spaCy model name(s) or path(s); lexnlp/regex fixed
    .labels_spacy = c("ORG", "GPE", "DATE", "MONEY"),
    .labels_lexnlp = c("ORG", "DATE", "MONEY"),
    .labels_dateregex = "DATE",
    .max_chars = NULL, # truncate docs to first N chars before extraction (NULL = off)
    .timeout_spacy = 600L, # spaCy per-window stall guard, seconds (0 = off)
    .timeout_lexnlp = 60L, # LexNLP per-extractor-per-doc cap, seconds (0 = off)
    .id_col = "DocID",
    .text_col = "TextRaw",
    .docs_per_run = 5000L, # R-side corpus chunking (memory bound; NULL = all at once)
    .device = "auto", # auto|cpu|cuda|mps (spaCy)
    .batch_size = 64L, # spaCy nlp.pipe batch
    .n_process = 16L, # CPU workers (spaCy CNN + LexNLP)
    .chunk_size = 8L, # LexNLP docs per worker task
    .keep_staging = FALSE,
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- .lP$Input$SampleContracts
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .engines <- c("spacy", "lexnlp", "regex")
    .models <- "en_core_web_sm"
    .labels_spacy <- c("ORG", "GPE", "DATE", "MONEY")
    .labels_lexnlp <- c("ORG", "DATE", "MONEY")
    .labels_dateregex <- "DATE"
    .max_chars <- NULL
    .timeout_spacy <- 600L
    .timeout_lexnlp <- 60L
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .docs_per_run <- 5000L
    .device <- "auto"
    .batch_size <- 64L
    .n_process <- 16L
    .chunk_size <- 8L
    .keep_staging <- FALSE
    .quiet <- FALSE
  }
  
  engines_ <- match.arg(.engines, c("spacy", "lexnlp", "regex"), several.ok = TRUE)
  
  # Engine x model grid. Model = the tag the extractor stamps into the parquet
  # (basename, in case a model path was given); ModelArg = what ner_spacy gets.
  combos_ <- dplyr::bind_rows(
    if ("spacy" %in% engines_) {
      tibble::tibble(Engine = "spacy", Model = fs::path_file(.models), ModelArg = .models)
    },
    if ("lexnlp" %in% engines_) {
      tibble::tibble(Engine = "lexnlp", Model = "lexnlp", ModelArg = "lexnlp")
    },
    if ("regex" %in% engines_) {
      tibble::tibble(Engine = "regex", Model = "paper-v1", ModelArg = "regex")
    }
  )
  
  files_ <- ner_input_files(.inputs)
  tmp_dir_ <- fs::path(fs::path_dir(.db_path), ".ner_tmp")
  fs::dir_create(tmp_dir_)
  
  summary_ <- vector("list", nrow(combos_))
  
  for (i_ in seq_len(nrow(combos_))) {
    engine_ <- combos_$Engine[i_]
    model_ <- combos_$Model[i_]
    
    # Sorted for deterministic slices -- the basis of the crash-recovery contract
    missing_ <- sort(ner_db_missing(
      .db_path, .inputs, engine_, model_,
      .id_col = .id_col, .quiet = .quiet
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
    
    for (s_ in seq_along(slices_)) {
      ids_ <- slices_[[s_]]
      if (!.quiet && length(slices_) > 1L) {
        cli::cli_alert_info("{engine_}/{model_}: slice {s_}/{length(slices_)} ({length(ids_)} doc{?s})")
      }
      
      # Slim filtered input -- skipped when a staging file survived a crash
      # (extraction is then skipped by the wrappers' file guard anyway).
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
          .labels = .labels_spacy, .max_chars = .max_chars,
          .timeout = .timeout_spacy,
          .model = combos_$ModelArg[i_], .device = device_,
          .batch_size = .batch_size, .n_process = .n_process, .quiet = .quiet
        )
      } else if (engine_ == "lexnlp") {
        ner_lexnlp(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = .labels_lexnlp, .max_chars = .max_chars,
          .timeout = .timeout_lexnlp,
          .chunk_size = .chunk_size, .n_process = .n_process, .quiet = .quiet
        )
      } else {
        ner_dateregex(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = .labels_dateregex, .max_chars = .max_chars,
          .quiet = .quiet
        )
      }
      
      res_ <- ner_db_append(.db_path, stage_, .quiet = .quiet)
      docs_ <- docs_ + res_$docs
      cands_ <- cands_ + res_$candidates
      
      # Staging MUST leave the working name before the next slice (the file
      # guard would otherwise re-feed it); .keep_staging archives instead.
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



# Unified NER orchestrator: runs the requested engine x model combos over the
# input parquet(s) and folds everything into the DuckDB store. Per combo: ask the
# ledger what's missing (skip if nothing) -> slice the missing docs by
# .docs_per_run (sorted DocIDs; bounds Python memory) -> per slice: slim filtered
# input (DuckDB COPY, text never in R) -> extractor -> append -> cleanup.
#
# Combos are requested via .run, a vector of tokens:
#   "spacy:<model>"  -- a spaCy model (REQUIRED; bare "spacy" errors)
#   "lexnlp"         -- LexNLP (fixed Model "lexnlp")
#   "regex"          -- paper date patterns (fixed Model "paper-v1"; track the
#                       MODEL constant in extract_dateregex.py)
# The token is the combo's identity everywhere: ner_arg keys, staging names, and
# the runs ledger all speak "engine" / "engine:model".
#
# Crash recovery: staging names are chunk-index-free; after a crash the recomputed
# first slice equals the interrupted one, and the file guard reuses the survivor.
# Device "auto": CNN -> cpu (parallel), *_trf -> auto (Python resolves GPU, one
# process).
#
# Per-combo knobs -- .labels, .n_process, .batch_size, .timeout -- each take a
# scalar/vector (broadcast to all) OR a named list keyed by "engine" and/or
# "engine:model" (most specific wins; see ner_arg). .labels = NULL uses each
# engine's policy default (spacy ORG/GPE/DATE/MONEY; lexnlp ORG/DATE/MONEY -- GPE
# excluded for throughput; regex DATE). Knob meanings: n_process = CPU workers
# (spacy CNN, lexnlp; ignored on trf/regex); batch_size = spaCy nlp.pipe batch /
# LexNLP pool chunksize (regex ignores); timeout = stall guard secs, 0 = off
# (regex ignores). Best-practice values documented separately (MasterDoc §15).
#
# Stall protection: spaCy window stall -> sequential fallback, offenders skipped;
# LexNLP extractor over cap -> skipped for that doc. Any skip -> runs.Status =
# 'timeout'; .retry_timeout = TRUE re-runs exactly those docs.
#
# NOTE: the ledger is label-, max_chars-, and model-label-blind -- a doc ingested
# under a given label set / .max_chars counts as done for that (Engine, Model).
# Keep .labels and .max_chars FIXED per store.
ner_run_OLDV4 <- function(
    .inputs,
    .db_path,
    .run = c("spacy:en_core_web_sm", "lexnlp", "regex"), # combo tokens; see above
    .labels = NULL, # NULL = per-engine policy; scalar/vector or named (engine / engine:model)
    .max_chars = NULL, # truncate docs to first N chars before extraction (NULL = off)
    .retry_timeout = FALSE, # re-run docs previously ingested as Status = 'timeout'
    .id_col = "DocID",
    .text_col = "TextRaw",
    .docs_per_run = 5000L, # R-side corpus chunking (memory bound; NULL = all at once)
    .device = "auto", # auto|cpu|cuda|mps (spaCy)
    .n_process = 16L, # CPU workers -- scalar or named (engine / engine:model)
    .batch_size = 64L, # spaCy batch / LexNLP chunksize -- scalar or named
    .timeout = list(spacy = 600L, lexnlp = 60L), # stall guard secs, 0=off -- scalar or named
    .keep_staging = FALSE,
    .quiet = FALSE
) {
  if (FALSE) {
    .inputs <- .lP$Input$SampleContracts
    .db_path <- file.path(.lP$Cache$NerTest, "test_store.duckdb")
    .run <- c("spacy:en_core_web_sm", "spacy:en_core_web_trf", "lexnlp", "regex")
    .labels <- NULL
    .max_chars <- NULL
    .retry_timeout <- FALSE
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .docs_per_run <- 5000L
    .device <- "auto"
    .n_process <- list(spacy = 16L, lexnlp = 24L)
    .batch_size <- list("spacy:en_core_web_trf" = 128L, spacy = 64L, lexnlp = 8L)
    .timeout <- list(spacy = 600L, lexnlp = 60L)
    .keep_staging <- FALSE
    .quiet <- FALSE
  }
  
  # Per-engine label policy -- fallback when .labels doesn't name a combo.
  labels_policy_ <- list(
    spacy = c("ORG", "GPE", "DATE", "MONEY"),
    lexnlp = c("ORG", "DATE", "MONEY"),
    regex = "DATE"
  )
  
  # Parse .run tokens ("engine" or "engine:model") into the combo grid. Model =
  # the tag stamped into the parquet (basename, in case a model path was given);
  # ModelArg = what the spaCy wrapper receives. Bare "spacy" errors.
  parse_run_ <- function(.tok) {
    parts_ <- strsplit(.tok, ":", fixed = TRUE)[[1]]
    engine_ <- parts_[1]
    if (!engine_ %in% c("spacy", "lexnlp", "regex")) {
      cli::cli_abort("Unknown engine in {.val {(.tok)}}; expected spacy|lexnlp|regex.")
    }
    if (engine_ == "spacy") {
      if (length(parts_) < 2L) {
        cli::cli_abort("spaCy needs a model: {.val {(.tok)}} -> e.g. {.val spacy:en_core_web_sm}")
      }
      model_ <- paste(parts_[-1], collapse = ":") # tolerate ':' in a model path
      tibble::tibble(Engine = "spacy", Model = fs::path_file(model_), ModelArg = model_)
    } else if (engine_ == "lexnlp") {
      tibble::tibble(Engine = "lexnlp", Model = "lexnlp", ModelArg = "lexnlp")
    } else {
      tibble::tibble(Engine = "regex", Model = "paper-v1", ModelArg = "regex")
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
    labels_ <- ner_arg(.labels, engine_, model_, .default = labels_policy_[[engine_]])
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
      } else {
        ner_dateregex(
          .inputs = input_, .output = stage_,
          .id_col = .id_col, .text_col = .text_col,
          .labels = labels_, .max_chars = .max_chars, .quiet = .quiet
        )
      }
      
      res_ <- ner_db_append(.db_path, stage_, .retry_timeout = .retry_timeout, .quiet = .quiet)
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

