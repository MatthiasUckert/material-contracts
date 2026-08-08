# Purpose: Test the NER extractors — exact offsets from spaCy + LexNLP ----
# Renders a sample of HTML docs to clean spaced text (rvest::html_text2), runs both engines
# through the CLI/parquet seam, and verifies every returned offset round-trips:
# str_sub(TextRaw, Start + 1, Stop) == Span. Then prints spans-in-context to eyeball
# whether the offsets land on real entities. Run from the project root, renv active.

# Configuration ----
dir_engine   <- here::here("contracts-engine")
python_bin   <- fs::path(dir_engine, ".venv", "bin", "python")
path_spacy   <- fs::path(dir_engine, "extract_spacy.py")
spacy_model  <- "en_core_web_sm"                   # fast for the test; en_core_web_trf for quality
lexnlp_image <- "contracts-lexnlp"                 # docker image tag (rebuild after editing extract_ner.py)

dir_in  <- here::here("0_sample")
dir_out <- here::here("0_scratch", "ner")
n_docs  <- 25L                                     # small sample to eyeball

# Helper: HTML -> clean spaced text (the render NER reads; NOT TextRaw) ----
render_html <- function(.html) {
  if (FALSE) {
    .html <- "<p>Hello</p><p>World</p>"
  }
  if (is.na(.html) || !nzchar(.html)) {
    return("")
  }
  .html |>
    rvest::read_html(encoding = "UTF-8", options = "RECOVER") |>
    rvest::html_text2()
}

# Helper: run a CLI step; abort loudly on a non-zero exit. ----
run_cli <- function(.cmd, .args, .label) {
  if (FALSE) {
    .cmd <- python_bin; .args <- "--version"; .label <- "test"
  }
  out_    <- system2(.cmd, .args, stdout = TRUE, stderr = TRUE)
  status_ <- attr(out_, "status")
  cat(out_, sep = "\n")
  if (!is.null(status_) && status_ != 0L) {
    cli::cli_abort("{(.label)} failed (status {status_})")
  }
  invisible(out_)
}

# Pre-flight ----
purrr::walk(c(python_bin, path_spacy), \(.p) {
  if (!fs::file_exists(.p)) cli::cli_abort("Missing {.path {(.p)}}.")
})
fs::dir_create(dir_out)

# Build the rendered-text sample ----


set.seed(1)
sample_paths <- sample(list.files(dir_in, full.names = TRUE, recursive = TRUE), size = n_docs)
sample_docs <- arrow::open_dataset(sample_paths, unify_schemas = TRUE) |>
  dplyr::select(DocID, TextRaw) |>
  dplyr::collect()

path_in <- fs::path(dir_out, "in.parquet")
arrow::write_parquet(sample_docs, path_in)

# Run spaCy (engine venv) ----
path_spacy_out <- fs::path(dir_out, "cand_spacy.parquet")
cli::cli_alert_info("spaCy NER on {nrow(sample_docs)} doc(s) [{spacy_model}]")
run_cli(python_bin,
        c(path_spacy, path_in, "--output", path_spacy_out,
          "--text-col", "TextRaw", "--model", spacy_model),
        "spaCy")

# Run LexNLP (py3.8 container; mount the scratch dir so it can read/write in /work) ----
path_lexnlp_out <- fs::path(dir_out, "cand_lexnlp.parquet")
cli::cli_alert_info("LexNLP NER (container)")
run_cli("docker",
        c("run", "--rm", "-v", paste0(dir_out, ":/work"), lexnlp_image,
          "/work/in.parquet", "--output", "/work/cand_lexnlp.parquet",
          "--id-col", "DocID", "--text-col", "TextRaw"),
        "LexNLP")

# Union both engines (same schema) and verify offsets ----
candidates <- dplyr::bind_rows(
  arrow::read_parquet(path_spacy_out),
  arrow::read_parquet(path_lexnlp_out)
)

check <- candidates |>
  dplyr::inner_join(sample_docs, by = "DocID") |>
  dplyr::mutate(
    Rehydrated = stringr::str_sub(TextRaw, Start + 1L, Stop),   # == Python text[Start:Stop]
    OffsetOk   = Rehydrated == Span
  )

# Report ----
cli::cli_h2("NER offset check")
check |>
  dplyr::summarise(
    N = dplyr::n(),
    OffsetOk = sum(OffsetOk),
    PctOk = round(100 * mean(OffsetOk), 1),
    .by = Engine
  ) |>
  print()

if (all(check$OffsetOk)) {
  cli::cli_alert_success("All offsets round-trip — str_sub(Start, Stop) == Span for every candidate.")
} else {
  cli::cli_alert_danger("Some offsets are off — rows where OffsetOk is FALSE:")
  check |>
    dplyr::filter(!OffsetOk) |>
    dplyr::select(Engine, Label, Start, Stop, Span, Rehydrated) |>
    print(n = 20)
}

# Eyeball spans in context — does the offset land on a real entity? ----
check |>
  dplyr::slice_sample(n = min(15L, nrow(check))) |>
  dplyr::mutate(
    Context = stringr::str_sub(
      TextRaw,
      pmax(Start - 25L, 0L) + 1L,
      pmin(Stop + 25L, stringr::str_length(TextRaw))
    )
  ) |>
  dplyr::select(Engine, Label, Span, Context) |>
  print(n = 15, width = Inf)
