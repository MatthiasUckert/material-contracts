#' Run NER extraction (spaCy + LexNLP) over contract parquets
#'
#' @description
#' Minimal building block for an R-side orchestrator: runs the selected NER engine(s)
#' over the input parquet(s) and writes one parquet per engine, prefixing the engine
#' (and, for spaCy, the model) onto the `.output` base name. spaCy runs natively in the
#' contracts-engine venv; LexNLP runs in its py3.8 Docker container (inputs mounted
#' read-only, the output directory mounted read-write).
#'
#' @param .inputs Character vector of parquet paths -- files and/or folders (folders
#'   globbed recursively for `*.parquet`). One row per document.
#' @param .output Base output path/filename. Per-engine files are written alongside it
#'   with a prefix, e.g. `spacy_en_core_web_lg_<name>` and `lexnlp_<name>`.
#' @param .engines Which engines to run; subset of `c("spacy", "lexnlp")`.
#' @param .text_col,.id_col Column names in the inputs (defaults `"TextRaw"`, `"DocID"`).
#' @param .model spaCy model name or path (default `"en_core_web_sm"`).
#' @param .n_process spaCy worker processes for `nlp.pipe` (CPU models only; leave at 1 for trf).
#' @param .quiet Suppress progress and child-process output.
#'
#' @return A named character vector of the parquet paths written (by engine). Each file
#'   has columns `DocID, Start, Stop, Span, Label, LabelRaw, Engine`; offsets are 0-based,
#'   half-open, code-point indices into `.text_col` (`text[Start:Stop] == Span`).
#'
#' @examples
#' \dontrun{
#' # writes 2_output/ner/spacy_en_core_web_lg_2021-1.parquet + lexnlp_2021-1.parquet
#' run_ner(here::here("0_sample", "2021-1"),
#'         here::here("2_output", "ner", "2021-1.parquet"),
#'         .model = "en_core_web_lg", .n_process = 10)
#' }
run_ner <- function(.inputs, .output, .engines   = c("spacy", "lexnlp"),
                    .text_col  = "TextRaw",
                    .id_col    = "DocID",
                    .model     = "en_core_web_sm",
                    .n_process = 1L,
                    .quiet     = FALSE) {
  
  if (FALSE) {
    .inputs <- "0_sample/2012-1/"
    .output <- file.path(.lP$Cache$NerExtraction, "NER_2012-1.parquet")
    .engines <- c("spacy", "lexnlp")
    .text_col <- "TextRaw"; .id_col <- "DocID"; .model <- "en_core_web_sm"
    .n_process <- 10L; .quiet <- FALSE
  }
  
  engines_ <- match.arg(.engines, c("spacy", "lexnlp"), several.ok = TRUE)
  inputs_  <- fs::path_abs(.inputs)
  
  # Fixed project layout -- everything lives in this repo, so these never vary.
  dir_engine_   <- here::here("contracts-engine")
  lexnlp_image_ <- "contracts-lexnlp"
  
  out_dir_   <- fs::path_dir(.output)
  name_      <- fs::path_file(.output)
  model_tag_ <- fs::path_file(.model)                 # basename if a path was given
  fs::dir_create(out_dir_)
  
  # internal: run a child process, streaming its stdout/stderr to the console so
  # progress bars show live (capturing would buffer until the process exits).
  run_step_ <- function(.cmd, .args, .label) {
    status_ <- system2(.cmd, .args,
                       stdout = if (.quiet) FALSE else "",
                       stderr = if (.quiet) FALSE else "")
    if (!identical(as.integer(status_), 0L)) {
      cli::cli_abort("{(.label)} failed (status {status_}).")
    }
    invisible(status_)
  }
  
  written_ <- character()
  
  # --- spaCy: native, writes <out_dir>/spacy_<model>_<name> --------------------
  if ("spacy" %in% .engines) {
    python_ <- fs::path(dir_engine_, ".venv", "bin", "python")
    script_ <- fs::path(dir_engine_, "extract_spacy.py")
    if (!fs::file_exists(python_)) cli::cli_abort("No engine venv at {.path {python_}}.")
    if (!fs::file_exists(script_)) cli::cli_abort("Missing {.path {script_}}.")
    
    spacy_out_ <- fs::path(out_dir_, paste0("spacy_", model_tag_, "_", name_))
    if (!.quiet) cli::cli_alert_info("spaCy NER [{(.model)}] over {length(inputs_)} input(s)")
    run_step_(python_,
              c(script_, inputs_, "--output", spacy_out_,
                "--id-col", .id_col, "--text-col", .text_col, "--model", .model,
                "--n-process", .n_process),
              "extract_spacy.py")
    written_["spacy"] <- spacy_out_
  }
  
  # --- LexNLP: container, writes <out_dir>/lexnlp_<name> -----------------------
  if ("lexnlp" %in% .engines) {
    if (Sys.which("docker") == "") cli::cli_abort("docker not found on PATH.")
    
    base_ <- fs::path_common(inputs_)
    if (!fs::is_dir(base_)) base_ <- fs::path_dir(base_)        # single-file case
    rel_  <- fs::path_rel(inputs_, base_)
    cont_in_ <- as.character(fs::path("/work", rel_))
    cont_in_[rel_ == "."] <- "/work"                            # whole base mounted as input
    
    lexnlp_name_ <- paste0("lexnlp_", name_)                    # no model for LexNLP
    if (!.quiet) cli::cli_alert_info("LexNLP NER (container) over {length(inputs_)} input(s)")
    run_step_("docker",
              c("run", "--rm",
                "-v", paste0(as.character(fs::path_real(base_)), ":/work:ro"),
                "-v", paste0(as.character(fs::path_real(out_dir_)), ":/out"),
                lexnlp_image_,
                cont_in_, "--output", fs::path("/out", lexnlp_name_),
                "--id-col", .id_col, "--text-col", .text_col),
              "LexNLP container")
    written_["lexnlp"] <- fs::path(out_dir_, lexnlp_name_)
  }
  
  if (!.quiet) {
    cli::cli_alert_success(
      "wrote {length(written_)} file(s) to {.path {out_dir_}}: {paste(fs::path_file(written_), collapse = ', ')}")
  }
  
  written_
}