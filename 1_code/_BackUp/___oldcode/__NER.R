# Internal helpers (dot-prefixed -> hidden from ls()/autocomplete) ------------

#' Run a child process, streaming its stdout/stderr live; abort on a non-zero exit.
.ner_run_step <- function(.cmd, .args, .label, .quiet = FALSE) {
  if (FALSE) {
    .cmd <- "echo"; .args <- "hi"; .label <- "echo"; .quiet <- FALSE
  }
  status_ <- system2(.cmd, .args,
                     stdout = if (.quiet) FALSE else "",
                     stderr = if (.quiet) FALSE else "")
  if (!identical(as.integer(status_), 0L)) {
    cli::cli_abort("{(.label)} failed (status {status_}).")
  }
  invisible(status_)
}

#' TRUE if .target should be (re)computed; FALSE (with a note) if it already exists.
.ner_should_run <- function(.target, .overwrite = FALSE, .quiet = FALSE) {
  if (fs::file_exists(.target) && !.overwrite) {
    if (!.quiet) cli::cli_alert_info("skip, exists: {.path {fs::path_file(.target)}}")
    return(FALSE)
  }
  TRUE
}

#' Prefixed output path: <engine>[_<model>]_<name> inside .out_dir.
.ner_output_path <- function(.out_dir, .name, .engine, .model = NULL) {
  prefix_ <- if (is.null(.model)) .engine else paste(.engine, fs::path_file(.model), sep = "_")
  fs::path(.out_dir, paste0(prefix_, "_", .name))
}

#' Argument vector for extract_spacy.py.
.ner_spacy_args <- function(.script, .inputs, .output, .id_col, .text_col, .model, .n_process, .labels, .device) {
  c(.script, .inputs, "--output", .output,
    "--id-col", .id_col, "--text-col", .text_col, "--model", .model,
    "--n-process", .n_process, "--device", .device, "--label", .labels)
}

#' `docker run` argument vector for the LexNLP container. Mounts the inputs' common
#' base read-only at /work and the output dir at /out, translating each host input
#' path into the /work mount.
.ner_lexnlp_args <- function(.image, .inputs, .out_dir, .out_name,
                             .id_col, .text_col, .n_process, .labels) {
  if (FALSE) {
    .image <- "contracts-lexnlp"; .inputs <- here::here("0_sample", "2021-1")
    .out_dir <- here::here("2_output", "ner"); .out_name <- "lexnlp_2021-1.parquet"
    .id_col <- "DocID"; .text_col <- "TextRaw"; .n_process <- 1L; .labels <- "ORG"
  }
  base_ <- fs::path_common(.inputs)
  if (!fs::is_dir(base_)) base_ <- fs::path_dir(base_)          # single-file case
  rel_  <- fs::path_rel(.inputs, base_)
  cont_in_ <- as.character(fs::path("/work", rel_))
  cont_in_[rel_ == "."] <- "/work"                             # whole base mounted as input
  
  c("run", "--rm",
    "-v", paste0(as.character(fs::path_real(base_)),    ":/work:ro"),
    "-v", paste0(as.character(fs::path_real(.out_dir)), ":/out"),
    .image,
    cont_in_, "--output", fs::path("/out", .out_name),
    "--id-col", .id_col, "--text-col", .text_col,
    "--n-process", .n_process, "--label", .labels)
}

# Full unified vocabulary (.labels = NULL -> all of these).
.ner_labels_all <- c("ORG", "PERSON", "GPE", "DATE", "MONEY", "AMOUNT", "PERCENT", "RATIO", "DURATION")

# Labels we let LexNLP emit via ner_run. The script also supports GPE (geoentity), but it's
# deliberately omitted here: geography comes from spaCy GPE + the USGS gazetteer, and LexNLP's
# geo pass is slow and redundant. PERSON has no offset API in LexNLP.
.ner_lexnlp_labels <- c("ORG", "DATE", "MONEY", "AMOUNT", "PERCENT", "RATIO", "DURATION")


# Public entry point ---------------------------------------------------------

#' Run NER extraction (spaCy + LexNLP) over contract parquets
#'
#' @description
#' Minimal building block for an R-side orchestrator: runs the selected NER engine(s)
#' over the input parquet(s) and writes one parquet per engine (and, for spaCy, per
#' model), prefixing the engine + model onto the `.output` base name. Both engines map
#' their native types into a shared `Label` vocabulary, and `.labels` selects which of
#' those to extract. Existing outputs are skipped unless `.overwrite = TRUE`.
#'
#' @param .inputs Character vector of parquet paths -- files and/or folders (globbed
#'   recursively for `*.parquet`). One row per document.
#' @param .output Base output path/filename; per-engine files get a prefix, e.g.
#'   `spacy_en_core_web_lg_<name>` and `lexnlp_<name>`.
#' @param .engines Which engines to run; subset of `c("spacy", "lexnlp")`.
#' @param .text_col,.id_col Column names in the inputs (defaults `"TextRaw"`, `"DocID"`).
#' @param .labels Unified labels to extract (one set, requested from both engines). spaCy
#'   covers `ORG/PERSON/GPE/DATE/MONEY/PERCENT/AMOUNT`; LexNLP covers
#'   `ORG/DATE/MONEY/AMOUNT/PERCENT/RATIO/DURATION`. `PERSON` and `GPE` are spaCy-only here
#'   (LexNLP's geo pass is redundant with spaCy + the gazetteer, so it isn't used). `NULL`
#'   (default) = all. LexNLP is skipped if none of `.labels` are ones it does. For different
#'   per-engine sets, call `ner_run()` twice with different `.engines`/`.labels`.
#' @param .model One or more spaCy models (name or path); spaCy is run once per model.
#' @param .n_process CPU worker processes (used for CNN spaCy models and LexNLP; a GPU /
#'   `trf` always runs single-process regardless of this).
#' @param .device spaCy inference device. `"auto"` (default) decides per model: `trf` ->
#'   GPU (CUDA, or Apple MPS) single-process, and CNN models (`sm/md/lg`) -> CPU with
#'   `.n_process`. `"cpu"`/`"cuda"`/`"mps"` force that device for every model. LexNLP is
#'   CPU-only and unaffected.
#' @param .overwrite Re-run and overwrite outputs that already exist (default `FALSE` skips).
#' @param .quiet Suppress progress and child-process output.
#'
#' @return Named character vector of the parquet paths produced (written or already
#'   present), keyed `spacy_<model>` / `lexnlp`. Columns: `DocID, Start, Stop, Span,
#'   Label, LabelRaw, Engine`; offsets are code-point, 0-based, half-open.
#'
#' @examples
#' \dontrun{
#' # one call: CNN models run CPU-parallel, trf runs on the GPU, LexNLP on CPU
#' ner_run(here::here("0_sample", "2021-1"),
#'         here::here("2_output", "ner", "2021-1.parquet"),
#'         .model = c("en_core_web_sm", "en_core_web_md", "en_core_web_lg", "en_core_web_trf"),
#'         .n_process = 10)
#' }
ner_run <- function(.inputs,
                    .output,
                    .engines   = c("spacy", "lexnlp"),
                    .text_col  = "TextRaw",
                    .id_col    = "DocID",
                    .labels    = NULL,
                    .model     = "en_core_web_sm",
                    .n_process = 1L,
                    .device    = "auto",
                    .overwrite = FALSE,
                    .quiet     = FALSE) {
  
  if (FALSE) {
    .inputs <- here::here("0_sample", "2021-1")
    .output <- here::here("2_output", "ner", "2021-1.parquet")
    .engines <- c("spacy", "lexnlp")
    .text_col <- "TextRaw"; .id_col <- "DocID"
    .labels <- NULL
    .model <- c("en_core_web_sm", "en_core_web_lg")
    .n_process <- 1L; .overwrite <- FALSE; .quiet <- FALSE
    .device <- "auto"
  }
  
  .engines <- match.arg(.engines, several.ok = TRUE)
  if (is.null(.labels) || !length(.labels) || all(!nzchar(.labels))) .labels <- .ner_labels_all
  inputs_  <- fs::path_abs(.inputs)
  out_dir_ <- fs::path_dir(.output)
  name_    <- fs::path_file(.output)
  fs::dir_create(out_dir_)
  
  dir_engine_   <- here::here("contracts-engine")
  lexnlp_image_ <- "contracts-lexnlp"
  written_      <- character()
  
  # spaCy: one prefixed file per model. Device + parallelism are chosen per model, so a
  # single call can mix CNN and transformer models. Under "auto", trf runs on GPU
  # (single-process) and the CNN models run on CPU (parallel, with .n_process); an
  # explicit .device is honoured for every model (a GPU still implies single-process).
  if ("spacy" %in% .engines) {
    python_ <- fs::path(dir_engine_, ".venv", "bin", "python")
    script_ <- fs::path(dir_engine_, "extract_spacy.py")
    if (!fs::file_exists(python_)) cli::cli_abort("No engine venv at {.path {python_}}.")
    if (!fs::file_exists(script_)) cli::cli_abort("Missing {.path {script_}}.")
    
    for (model_ in .model) {
      is_trf_ <- grepl("trf", model_, fixed = TRUE)
      device_ <- if (.device == "auto" && !is_trf_) "cpu" else .device
      nproc_  <- if (device_ == "cpu") .n_process else 1L
      target_ <- .ner_output_path(out_dir_, name_, "spacy", model_)
      if (.ner_should_run(target_, .overwrite, .quiet)) {
        if (!.quiet) cli::cli_alert_info("spaCy NER [{model_}] ({device_}, n_process={nproc_}) over {length(inputs_)} input(s)")
        .ner_run_step(python_,
                      .ner_spacy_args(script_, inputs_, target_, .id_col, .text_col, model_, nproc_, .labels, device_),
                      "extract_spacy.py", .quiet)
      }
      written_[paste0("spacy_", fs::path_file(model_))] <- target_
    }
  }
  
  # LexNLP: single prefixed file; the supported subset of .labels (GPE excluded by policy)
  if ("lexnlp" %in% .engines) {
    lex_labels_ <- intersect(.labels, .ner_lexnlp_labels)
    if (length(lex_labels_) == 0) {
      if (!.quiet) cli::cli_alert_info("LexNLP has no extractor for {paste(.labels, collapse = ', ')} -- skipping")
    } else {
      target_ <- .ner_output_path(out_dir_, name_, "lexnlp")
      if (.ner_should_run(target_, .overwrite, .quiet)) {
        if (Sys.which("docker") == "") cli::cli_abort("docker not found on PATH.")
        if (!.quiet) cli::cli_alert_info("LexNLP NER (container) {paste(lex_labels_, collapse = ', ')} over {length(inputs_)} input(s)")
        .ner_run_step("docker",
                      .ner_lexnlp_args(lexnlp_image_, inputs_, out_dir_, fs::path_file(target_),
                                       .id_col, .text_col, .n_process, lex_labels_),
                      "LexNLP container", .quiet)
      }
      written_["lexnlp"] <- target_
    }
  }
  
  if (!.quiet) cli::cli_alert_success("{length(written_)} output(s) in {.path {out_dir_}}")
  written_
}


#' Validate that NER offsets reconstruct Span from each document's text
#'
#' Reads every source document once, rebuilds each candidate's span from its
#' `[Start, Stop)` offsets, and compares against the stored `Span`. Reconstructs
#' two ways: `substr()` (what the pipeline uses) and `stringi::stri_sub()` (strictly
#' code-point based, matching Python's offsets). If a span matches `stri_sub` but not
#' `substr`, the offsets are correct and `substr` is mis-indexing multibyte text --
#' switch the pipeline to `stri_sub()`.
#'
#' @param .ner_path  Path to an NER output parquet (DocID, Start, Stop, Span, ...).
#' @param .doc_files Tibble mapping `DocID` -> `Path` (e.g. `utils_list_files(dir)`).
#' @param .text_col  Name of the text column in the source documents.
#' @return The failing rows (0 rows = every offset round-trips and sits in bounds),
#'   invisibly, after a cli summary.
ner_check_offsets <- function(.ner_path, .doc_files, .text_col = "TextRaw") {
  
  if (FALSE) {
    .ner_path  <- "2_output/03-NamedEntities/Cache/NerExtraction/spacy_en_core_web_trf_NER_2005-2.parquet"
    .doc_files <- utils_list_files(fil_sample_dirs$Path[25])
    .text_col  <- "TextRaw"
  }
  
  cand_ <- arrow::read_parquet(.ner_path) |>
    dplyr::left_join(.doc_files, by = dplyr::join_by(DocID)) |>
    dplyr::filter(!is.na(Path))
  
  out_ <- cand_ |>
    dplyr::group_by(Path) |>
    dplyr::group_modify(\(.x, .y) {
      text_ <- arrow::read_parquet(.y$Path, col_select = dplyr::all_of(.text_col))[[.text_col]]
      dplyr::mutate(.x,
                    NChar    = stringi::stri_length(text_),
                    BySubstr = substr(text_, Start + 1L, Stop),
                    ByStri   = stringi::stri_sub(text_, Start + 1L, Stop))
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(InBounds = Start >= 0L & Stop > Start & Stop <= NChar,
                  NonAscii = !stringi::stri_enc_isascii(Span),
                  OkSubstr = BySubstr == Span,
                  OkStri   = ByStri   == Span)
  
  n_           <- nrow(out_)
  n_nonascii_  <- sum(out_$NonAscii)
  n_bad_       <- sum(!out_$OkStri | !out_$InBounds)
  n_substronly <- sum(out_$OkStri & !out_$OkSubstr)
  
  cli::cli_h3("Offset check: {.file {fs::path_file(.ner_path)}}")
  cli::cli_alert_info("{n_} candidate(s); {n_nonascii_} with non-ASCII span(s).")
  if (n_bad_ == 0L) {
    cli::cli_alert_success("All spans round-trip (stri_sub) and sit in bounds.")
  } else {
    cli::cli_alert_danger("{n_bad_} candidate(s) fail round-trip or bounds.")
  }
  if (n_substronly > 0L) {
    cli::cli_alert_warning(
      "{n_substronly} span(s) match stri_sub but not substr -> switch the pipeline to stringi::stri_sub() (substr mis-indexes multibyte text).")
  }
  
  invisible(dplyr::filter(out_, !(OkSubstr & OkStri & InBounds)))
}


# Purpose: Appendable DuckDB output store for the NER candidate layer ----
#   One DuckDB file, one `candidates` table holding the harmonised candidate
#   schema (DocID, Start, Stop, Span, Label, LabelRaw) plus R-stamped Engine,
#   Model, and run metadata. Skip-if-done lives at (DocID, Engine, Model)
#   granularity.
#
#   Design (MasterDoc V4 §14.1):
#     - The Python extractors keep emitting parquet; this layer never touches
#       them. DuckDB is a pure R-side aggregation layer (duckdb / DBI), appended
#       via INSERT INTO candidates SELECT ... FROM read_parquet(...).
#     - Dedup grain = (DocID, Engine, Model). Label is a *run parameter*, not
#       part of the grain: re-extracting one doc re-emits its whole label set,
#       so putting Label in the key would only invite double-appends. Changing
#       the label set for an existing engine+model is therefore a deliberate
#       clear-and-rerun (ner_store_clear), never an implicit re-extract.
#     - Engine and Model are stamped from the R args (not read from the parquet)
#       so the stored key and the skip-if-done key are guaranteed identical —
#       that invariant is what the whole skip mechanism rests on.
#     - Offsets are 0-based, half-open, code-point indices into TextRaw. Any
#       R-side rehydration of Span from Start/Stop MUST use stringi::stri_sub,
#       never base substr (§12 byte-vs-code-point bug).
#
#   Public API:
#     ner_store_init(.db_path)                          -> open con, ensure schema
#     ner_store_todo(.con, .doc_ids, .engine, .model)   -> DocIDs not yet extracted
#     ner_store_append(.con, .parquet_path, .engine, .model, .run_id)
#     ner_store_clear(.con, .engine, .model, .doc_ids)  -> force-rerun support
#     ner_store_summary(.con)                           -> per (Engine, Model) counts
#     ner_store_close(.con)
#
#   Requires: DBI, duckdb, glue, fs, cli, tibble.


# Connection / schema ----

ner_store_init <- function(.db_path) {
  if (FALSE) {
    .db_path <- here::here("2_output", "03-NamedEntities", "ner_candidates.duckdb")
  }
  
  fs::dir_create(fs::path_dir(.db_path))
  
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = .db_path)
  
  # Explicit column order so the INSERT ... SELECT below is never silently
  # positional. SegmentID is intentionally absent: segmentation (§12) does not
  # feed this layer yet (open sub-decision §10.7) -- add it when it does.
  DBI::dbExecute(
    con_,
    "CREATE TABLE IF NOT EXISTS candidates (
       DocID     VARCHAR,
       Start     BIGINT,
       Stop      BIGINT,
       Span      VARCHAR,
       Label     VARCHAR,
       LabelRaw  VARCHAR,
       Engine    VARCHAR,
       Model     VARCHAR,
       RunId     VARCHAR,
       FetchTime TIMESTAMP
     )"
  )
  
  cli::cli_alert_success("Candidate store ready at {(.db_path)}")
  return(con_)
}

ner_store_close <- function(.con) {
  DBI::dbDisconnect(.con, shutdown = TRUE)
  return(invisible(NULL))
}


# Skip-if-done ----

# Returns the subset of .doc_ids that have NOT yet been extracted by this
# (engine, model). ner_run calls this near the top: skip entirely if empty,
# otherwise hand only the survivors to the extractor.
ner_store_todo <- function(.con, .doc_ids, .engine, .model) {
  if (FALSE) {
    .con     <- ner_store_init(here::here("2_output", "03-NamedEntities", "ner_candidates.duckdb"))
    .doc_ids <- c("0000950123-09-001234", "0000950123-09-005678")
    .engine  <- "spacy"
    .model   <- "en_core_web_trf"
  }
  
  existing_ <- DBI::dbGetQuery(
    .con,
    glue::glue_sql(
      "SELECT DISTINCT DocID
         FROM candidates
        WHERE Engine = {.engine} AND Model = {.model}",
      .con = .con
    )
  )$DocID
  
  todo_ <- setdiff(.doc_ids, existing_)
  
  cli::cli_alert_info(
    "{(.engine)}/{(.model)}: {length(todo_)} of {length(.doc_ids)} doc{?s} to extract \\
     ({length(existing_)} already in store)"
  )
  
  return(todo_)
}


# Append ----

# Transactional append of one extractor's parquet. Engine/Model/RunId/FetchTime
# are stamped from R; the parquet supplies the six harmonised candidate columns.
# A uniqueness guard refuses to append docs already present for this
# (engine, model) so the function is self-defending even if called without a
# prior ner_store_todo() filter.
ner_store_append <- function(.con, .parquet_path, .engine, .model, .run_id = NULL) {
  if (FALSE) {
    .con          <- ner_store_init(here::here("2_output", "03-NamedEntities", "ner_candidates.duckdb"))
    .parquet_path <- here::here("2_output", "03-NamedEntities", "spacy__en_core_web_trf.parquet")
    .engine       <- "spacy"
    .model        <- "en_core_web_trf"
    .run_id       <- NULL
  }
  
  run_id_     <- if (is.null(.run_id)) format(Sys.time(), "%Y%m%dT%H%M%S") else .run_id
  fetch_time_ <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Uniqueness guard against double-append (the skip key is per engine+model).
  incoming_ <- DBI::dbGetQuery(
    .con,
    glue::glue_sql("SELECT DISTINCT DocID FROM read_parquet({.parquet_path})", .con = .con)
  )$DocID
  
  existing_ <- DBI::dbGetQuery(
    .con,
    glue::glue_sql(
      "SELECT DISTINCT DocID FROM candidates WHERE Engine = {.engine} AND Model = {.model}",
      .con = .con
    )
  )$DocID
  
  clash_ <- intersect(incoming_, existing_)
  if (length(clash_) > 0) {
    cli::cli_abort(c(
      "Refusing to append: {length(clash_)} DocID{?s} already in store for {(.engine)}/{(.model)}.",
      "i" = "Pre-filter with ner_store_todo(), or ner_store_clear() to deliberately re-run."
    ))
  }
  
  # Stamp Engine/Model from R (authoritative on the grain); columns named, never
  # positional, so a parquet column-order change can't silently misalign.
  sql_ <- glue::glue_sql(
    "INSERT INTO candidates
       (DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model, RunId, FetchTime)
     SELECT DocID, Start, Stop, Span, Label, LabelRaw,
            {.engine} AS Engine, {.model} AS Model,
            {run_id_} AS RunId, {fetch_time_} AS FetchTime
       FROM read_parquet({.parquet_path})",
    .con = .con
  )
  
  DBI::dbBegin(.con)
  n_ <- tryCatch(
    {
      inserted_ <- DBI::dbExecute(.con, sql_)
      DBI::dbCommit(.con)
      inserted_
    },
    error = function(e) {
      DBI::dbRollback(.con)
      cli::cli_abort("Append failed, rolled back: {conditionMessage(e)}")
    }
  )
  
  cli::cli_alert_success("Appended {n_} candidate row{?s} for {(.engine)}/{(.model)}")
  return(invisible(n_))
}


# Force re-run: clear matching rows ----

# Deletes rows for a given (engine, model) -- optionally restricted to .doc_ids.
# This is the deliberate escape hatch for "I changed the label set": clear, then
# re-run so ner_store_todo() sees those docs as missing again.
ner_store_clear <- function(.con, .engine, .model, .doc_ids = NULL) {
  if (FALSE) {
    .con     <- ner_store_init(here::here("2_output", "03-NamedEntities", "ner_candidates.duckdb"))
    .engine  <- "spacy"
    .model   <- "en_core_web_trf"
    .doc_ids <- NULL
  }
  
  where_ <- glue::glue_sql("Engine = {.engine} AND Model = {.model}", .con = .con)
  if (!is.null(.doc_ids)) {
    where_ <- glue::glue_sql("{where_} AND DocID IN ({.doc_ids*})", .con = .con)
  }
  
  n_ <- DBI::dbExecute(
    .con,
    glue::glue_sql("DELETE FROM candidates WHERE {where_}", .con = .con)
  )
  
  cli::cli_alert_warning("Cleared {n_} row{?s} for {(.engine)}/{(.model)}")
  return(invisible(n_))
}


# Inspection ----

ner_store_summary <- function(.con) {
  if (FALSE) {
    .con <- ner_store_init(here::here("2_output", "03-NamedEntities", "ner_candidates.duckdb"))
  }
  
  out_ <- DBI::dbGetQuery(
    .con,
    "SELECT Engine, Model,
            COUNT(DISTINCT DocID) AS NDocs,
            COUNT(*)              AS NCandidates,
            MIN(FetchTime)        AS FirstRun,
            MAX(FetchTime)        AS LastRun
       FROM candidates
      GROUP BY Engine, Model
      ORDER BY Engine, Model"
  ) |>
    tibble::as_tibble()
  
  return(out_)
}


# Integration sketch -- how ner_run() wires this in (3 call-sites) ----
#
#   con <- ner_store_init(store_path)
#   on.exit(ner_store_close(con), add = TRUE)
#
#   # 1. skip-if-done: which docs still need this engine+model?
#   todo <- ner_store_todo(con, all_doc_ids, engine, model)
#   if (length(todo) == 0L) return(invisible())   # nothing to do
#
#   # 2. write a filtered input parquet (only the missing docs) into 0_scratch/
#   #    and point the existing extractor seam at it -- the extractor stays
#   #    schema-agnostic and never learns about DuckDB or doc filtering.
#   filtered_in <- write_scratch_subset(input_parquet, todo)   # existing helper
#   out_parquet <- run_extractor(filtered_in, engine, model)   # existing seam
#
#   # 3. append the extractor's parquet into the store
#   ner_store_append(con, out_parquet, engine, model)