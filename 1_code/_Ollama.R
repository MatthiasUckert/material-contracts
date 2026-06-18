# Purpose: Quick-and-dirty Ollama adjudication of NER candidates (v0 scaffold) ----
# DB read -> correct document -> offset extraction -> one prompt per candidate.
# Reads the DuckDB candidate store directly (read-only); no dependence on ner_run.
# Reconciliation, SegHash caching/dedup, digest pinning, writeback all come later.

# Config / policy constants ----
.ollama_model <- "qwen3:32b"
.ollama_window <- 160L # context chars each side of a candidate
.ollama_labels <- c("DATE", "GPE", "MONEY", "ORG")
.ollama_host <- "http://localhost:11434"
.ollama_num_ctx <- 8192L # per-candidate prompts are tiny; this is plenty

# Single-verdict JSON schema (Ollama >= 0.5). Pass .schema = NULL for older Ollama;
# the parser below is lenient either way.
ollama_schema <- function() {
  list(
    type = "object",
    properties = list(
      keep  = list(type = "boolean"),
      kind  = list(type = "string"),
      value = list(type = "string")
    ),
    required = list("keep", "kind", "value")
  )
}

# Read candidates from the store ----
ollama_read_candidates <- function(.db_path, .doc_ids = NULL, .labels = .ollama_labels, .run = NULL) {
  if (FALSE) {
    .db_path <- file.path(.lP$Cache$NerExtraction, "SampleNER.duckdb")
    .doc_ids <- NULL
    .labels <- .ollama_labels
    .run <- "lexnlp"
  }

  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = .db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  tbl_ <- dplyr::tbl(con_, "candidates") |>
    dplyr::filter(Label %in% .labels, !is.na(Start)) # sentinels carry null Start

  if (!is.null(.doc_ids)) {
    tbl_ <- dplyr::filter(tbl_, DocID %in% .doc_ids)
  }
  # .run = a single ner_run token: "lexnlp" (engine only) or "spacy:en_core_web_trf".
  if (!is.null(.run)) {
    engine_ <- sub(":.*$", "", .run)
    tbl_ <- dplyr::filter(tbl_, Engine == engine_)
    if (grepl(":", .run)) {
      model_ <- sub("^[^:]*:", "", .run)
      tbl_ <- dplyr::filter(tbl_, Model == model_)
    }
  }

  cand_ <- tbl_ |>
    dplyr::collect() |>
    dplyr::mutate(Start = as.integer(Start), Stop = as.integer(Stop)) |>
    dplyr::summarise(
      Engines = paste(sort(unique(Engine)), collapse = ","),
      NEngine = dplyr::n_distinct(Engine),
      .by = c(DocID, Start, Stop, Span, Label)
    ) |>
    dplyr::arrange(DocID, Start)

  cli::cli_alert_info(
    "Read {nrow(cand_)} distinct candidate spans across {dplyr::n_distinct(cand_$DocID)} contract(s)."
  )
  return(cand_)
}

# Slice marked offset windows from TextRaw ----
ollama_attach_context <- function(.cand, .inputs, .window = .ollama_window) {
  if (FALSE) {
    .cand <- ollama_read_candidates(file.path(.lP$Cache$NerExtraction, "SampleNER.duckdb"))
    .inputs <- list.files(.lP$Input$SampleContracts, full.names = TRUE)[20]
    .window <- .ollama_window
  }

  doc_ids_ <- unique(.cand$DocID)

  texts_ <- arrow::open_dataset(.inputs) |>
    dplyr::filter(DocID %in% doc_ids_) |>
    dplyr::select(DocID, TextRaw) |>
    dplyr::collect()

  miss_ <- setdiff(doc_ids_, texts_$DocID)
  if (length(miss_) > 0L) {
    cli::cli_alert_warning("No TextRaw for {length(miss_)} DocID(s); their candidates are dropped.")
  }

  out_ <- .cand |>
    dplyr::inner_join(texts_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      # Offsets are 0-based half-open code-points (MasterDoc §12); stri_sub is
      # 1-based inclusive -> Start + 1. Clamp left 'from' so a negative is never
      # read as count-from-end; right edge past length clamps to end on its own.
      FromL = pmax(1L, Start + 1L - .window),
      Left = stringi::stri_sub(TextRaw, FromL, Start),
      Mid = stringi::stri_sub(TextRaw, Start + 1L, Stop),
      Right = stringi::stri_sub(TextRaw, Stop + 1L, Stop + .window),
      Marked = paste0(Left, "\u00ab", Mid, "\u00bb", Right),
      DateHint = dplyr::if_else(
        Label == "DATE",
        as.character(suppressWarnings(anytime::anydate(Span))),
        NA_character_
      )
    ) |>
    dplyr::select(-TextRaw, -FromL, -Left, -Right, -Mid)

  cli::cli_alert_success("Sliced context windows for {nrow(out_)} candidate(s).")
  return(out_)
}

# Build a prompt for ONE candidate (label-specific rule) ----
ollama_build_prompt <- function(.label, .span, .marked, .date_hint = NA_character_) {
  if (FALSE) {
    .label <- ctx$Label[[1]]
    .span <- ctx$Span[[1]]
    .marked <- ctx$Marked[[1]]
    .date_hint <- ctx$DateHint[[1]]
  }

  rule_ <- switch(.label,
    DATE = "Decide if this DATE is a contract date. kind in {start, end, signing, other}. value = ISO date YYYY-MM-DD if resolvable, else empty string.",
    MONEY = "Decide if this MONEY value is a contract amount. kind in {total, fee, payment, other}. value = normalised amount, else empty string.",
    GPE = "Decide if this place is the location/domicile of a contracting party (not a mere mention). kind = party label if clear, else empty string. value = normalised place name, else empty string.",
    ORG = "Decide if this organisation is an actual contracting party. kind in {party, agent, guarantor, other}. value = normalised organisation name, else empty string.",
    "Decide if this entity is contractually meaningful. kind = a short type label, value = normalised form or empty string."
  )

  hint_ <- if (!is.na(.date_hint) && nzchar(.date_hint)) {
    paste0(" (date parses as ", .date_hint, ")")
  } else {
    ""
  }

  paste(
    "/no_think",
    "You are auditing one entity candidate extracted from a commercial contract.",
    "The candidate is wrapped in \u00ab \u00bb inside its surrounding context.",
    rule_,
    "Set keep=true only if it is contractually meaningful as described above.",
    "",
    paste0("Candidate (", .label, "): \u00ab", .span, "\u00bb", hint_),
    paste0("Context: ", .marked),
    "",
    "Return ONLY JSON: {\"keep\":bool,\"kind\":string,\"value\":string}. Use empty string where a field does not apply.",
    sep = "\n"
  )
}

# Call Ollama /api/chat directly (version-robust; swap for rollama::query if preferred) ----
ollama_chat <- function(.prompt, .model = .ollama_model, .schema = ollama_schema(),
                        .num_ctx = .ollama_num_ctx, .host = .ollama_host) {
  if (FALSE) {
    .prompt <- ollama_build_prompt(ctx$Label[[1]], ctx$Span[[1]], ctx$Marked[[1]], ctx$DateHint[[1]])
    .model <- .ollama_model
    .schema <- ollama_schema()
    .num_ctx <- .ollama_num_ctx
    .host <- .ollama_host
  }

  body_ <- list(
    model    = .model,
    messages = list(list(role = "user", content = .prompt)),
    stream   = FALSE,
    options  = list(temperature = 0, num_ctx = .num_ctx, seed = 1L) # determinism
  )
  if (!is.null(.schema)) body_$format <- .schema

  resp_ <- httr2::request(.host) |>
    httr2::req_url_path("/api/chat") |>
    httr2::req_body_json(body_, auto_unbox = TRUE) |>
    httr2::req_timeout(300) |>
    httr2::req_perform()

  httr2::resp_body_json(resp_)$message$content
}

# Lenient parse of a single verdict object ----
ollama_parse_one <- function(.txt) {
  if (FALSE) {
    .txt <- "{\"keep\":true,\"kind\":\"start\",\"value\":\"2020-01-01\"}"
  }

  na_ <- list(Keep = NA, Kind = NA_character_, Value = NA_character_, Raw = NA_character_)
  if (length(.txt) == 0L || is.na(.txt) || !nzchar(.txt)) {
    na_$Raw <- if (length(.txt) == 0L) NA_character_ else .txt
    return(na_)
  }

  clean_ <- .txt |>
    stringr::str_remove_all("(?s)<think>.*?</think>") |>
    stringr::str_remove_all("```json|```") |>
    stringr::str_trim()

  parsed_ <- tryCatch(jsonlite::fromJSON(clean_, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(parsed_) || is.null(parsed_[["keep"]])) {
    na_$Raw <- .txt
    return(na_)
  }

  kind_ <- parsed_[["kind"]]
  value_ <- parsed_[["value"]]
  list(
    Keep  = isTRUE(as.logical(parsed_[["keep"]])),
    Kind  = if (is.null(kind_)) NA_character_ else as.character(kind_),
    Value = if (is.null(value_)) NA_character_ else as.character(value_),
    Raw   = NA_character_
  )
}

# One candidate -> verdict (never errors out; failure attaches the raw response) ----
ollama_adjudicate_one <- function(.label, .span, .marked, .date_hint = NA_character_,
                                  .model = .ollama_model, .num_ctx = .ollama_num_ctx,
                                  .host = .ollama_host) {
  if (FALSE) {
    .label <- ctx$Label[[1]]
    .span <- ctx$Span[[1]]
    .marked <- ctx$Marked[[1]]
    .date_hint <- ctx$DateHint[[1]]
  }

  resp_ <- tryCatch(
    ollama_chat(ollama_build_prompt(.label, .span, .marked, .date_hint), .model, ollama_schema(), .num_ctx, .host),
    error = function(e) NA_character_
  )
  ollama_parse_one(resp_)
}

# Top-level loop: one prompt per candidate ----
ollama_adjudicate <- function(.db_path, .inputs, .doc_ids = NULL, .run = NULL,
                              .model = .ollama_model, .window = .ollama_window,
                              .labels = .ollama_labels, .num_ctx = .ollama_num_ctx,
                              .host = .ollama_host) {
  if (FALSE) {
    .db_path <- file.path(.lP$Cache$NerExtraction, "SampleNER.duckdb")
    .inputs <- list.files(.lP$Input$SampleContracts, full.names = TRUE)[20]
    .doc_ids <- NULL # NULL = all docs in store
    .run <- "lexnlp" # NULL = all engines; or a ner_run token
    .model <- .ollama_model
    .window <- .ollama_window
    .labels <- .ollama_labels
    .num_ctx <- .ollama_num_ctx
    .host <- .ollama_host
  }

  cand_ <- ollama_read_candidates(.db_path, .doc_ids, .labels, .run)
  if (nrow(cand_) == 0L) {
    cli::cli_alert_warning("No candidates found \u2014 nothing to adjudicate.")
    return(tibble::tibble())
  }

  ctx_ <- ollama_attach_context(cand_, .inputs, .window) |>
    dplyr::mutate(Index = dplyr::row_number(), .by = DocID)
  if (nrow(ctx_) == 0L) {
    cli::cli_alert_warning("No context windows (TextRaw missing?) \u2014 nothing to adjudicate.")
    return(tibble::tibble())
  }

  cli::cli_alert_info("Adjudicating {nrow(ctx_)} candidate(s) one prompt each with {(.model)}.")

  verdicts_ <- purrr::pmap(
    .l = list(ctx_$Label, ctx_$Span, ctx_$Marked, ctx_$DateHint),
    .f = \(.lab, .spn, .mrk, .hnt) ollama_adjudicate_one(.lab, .spn, .mrk, .hnt, .model, .num_ctx, .host),
    .progress = "Adjudicating candidates"
  ) |>
    dplyr::bind_rows()

  out_ <- dplyr::bind_cols(ctx_, verdicts_)

  cli::cli_alert_success("Done. {sum(out_$Keep, na.rm = TRUE)} of {nrow(out_)} candidates kept.")

  out_ |>
    dplyr::select(
      DocID, Index, Label, Span, Start, Stop, Engines, NEngine,
      DateHint, Keep, Kind, Value, Marked, Raw
    )
}
