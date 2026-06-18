# Run extract_gazetteer.py over parquet input(s); returns the output path.
# The paper's USGS+countries place-name lookup, ported with a hierarchy gate
# (Engine = "paper", Model = "gazetteer-v1", Label = "GPE", LabelRaw = GeoClass).
# Runs in the contracts-engine venv; PhraseMatcher matches case-insensitively
# against TextRaw (NOT TextMod -- offsets must index the canonical text; the
# matcher lowercases internally, so TextRaw is correct and offset-safe).
# .timeout caps matching per doc (marker row on timeout). If .output exists it is
# skipped unless .overwrite = TRUE.
ner_gazetteer <- function(
  .inputs,
  .output,
  .id_col = "DocID",
  .text_col = "TextRaw",
  .labels = "GPE", # full supported set
  .lookup = here::here("contracts-engine", "data", "gazetteer", "geo_lookup.parquet"),
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
  if (!.quiet) cli::cli_alert_success("gazetteer [{MODEL <- 'gazetteer-v1'}] done in {elapsed_}s -> {.path {(.output)}}")
  return(invisible(.output))
}


if (FALSE) {
  source("1_code/_Initialize.R", encoding = "UTF-8")
  source("1_code/_Utils.R", encoding = "UTF-8")
  source("1_code/_NER.R", encoding = "UTF-8")
  
  
  .lP <- list(
    Input = list(
      SampleContracts = "0_sample/"
    ),
    Cache = list(
      NerExtraction = "2_output/03-NamedEntities/Cache/NerExtraction/",
      NerTest       = "2_output/03-NamedEntities/Cache/NerTest/"
    ),
    Output = list()
  )
  
  out_gaz <- ner_gazetteer(
    .inputs = list.files(.lP$Input$SampleContracts, full.names = TRUE)[20],
    .output = file.path(.lP$Cache$NerTest, "test_gazetteer1.parquet"),
    .n_process = 10L
  )
  tab_gaz <- arrow::read_parquet(out_gaz)
  
  ner_check_offsets(tab_gaz, list.files(.lP$Input$SampleContracts, full.names = TRUE)[20]) # 100%
  dplyr::count(tab_gaz, LabelRaw, sort = TRUE)  # class mix; gated classes survived the gate
  
  
  # 1. Are the place hits dominated by short / common-word names?
  tab_gaz |>
    dplyr::filter(LabelRaw == "US Populated Place") |>
    dplyr::mutate(Span = stringi::stri_trans_toupper(Span)) |>
    dplyr::count(Span, sort = TRUE) |>
    print(n = 30)
  
  # 2. Per-doc candidate load — is it a few docs exploding, or uniform?
  tab_gaz |>
    dplyr::filter(!is.na(Start)) |>
    dplyr::count(DocID, name = "NCand") |>
    dplyr::summarise(
      median = median(NCand), p90 = quantile(NCand, .9),
      max = max(NCand), mean = round(mean(NCand), 1)
    )
}
