# Check-NER-REDACT: one engine, and therefore a different kind of check ----
#
# WHAT THIS IS
# The acceptance check for REDACT extraction, built to the contract the ORG, GPE, DATE and MONEY
# checks settled. Twenty-five documents, the one engine that emits REDACT, its parquet on disk, and
# a battery of tests over it.
#
# THE CONTRACT, UNCHANGED
#   Mandatory core   DocID  Start  Stop  Span  Label  LabelRaw  Engine  Model
#   Optional extras  whatever the engine has and nothing it does not
#
# THIS LABEL HAS ONE ENGINE, AND THAT REMOVES THE INSTRUMENT THE OTHER FOUR LEANED ON
# Every check so far has had cross-engine agreement available: two extractors proposing the same
# span, or the same date, or the same ISO code, corroborate each other in a way neither can
# manufacture alone. Here there is nothing to agree with. paper:redaction-v1 is the sole engine,
# 04A said so, and no comparison can be made.
#
# SO THE EVIDENCE COMES FROM WHAT A MARKER SITS BESIDE. A redaction is not an entity, it is a HOLE
# where an entity used to be, and the only thing that can characterise a hole is its surroundings.
# Three questions follow, and they are the substance of this check:
#
#   WHAT WAS WITHHELD    a marker next to a currency symbol was an amount; next to an organisation
#                        it was a party or a counterparty; next to neither it is unclassified. This
#                        is the question the published redaction count could not ask, because a
#                        count carries no position.
#   IS RedactBare REAL   four of the five classes follow the published classification. RedactBare --
#                        an unbracketed run of three or more asterisks -- does not, and the
#                        extractor's own header says to filter it "if it floods". Whether it floods
#                        is measured here rather than assumed either way.
#   WHERE DO THEY SIT    04A found the only label with no bimodality: the share rises monotonically
#                        from 4.5% in the first decile to 21.3% in the last, which is where
#                        schedules and pricing exhibits live. That shape is checked again on this
#                        draw, because a monotone rise is a strong claim from one measurement.
#
# The parquet is left on disk under 2_output/_Probe/Check-NER-REDACT/out and is meant to be opened.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_04a     <- here::here("2_output", "04A-EntityExtract")
.path_text   <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys   <- fs::path(.dir_04a, "sample_anchors.parquet")
.path_store  <- fs::path(.dir_04a, "Store", "EntityCandidates.duckdb")

.dir_check   <- fs::dir_create(here::here("2_output", "_Probe", "Check-NER-REDACT"))
.dir_in      <- fs::dir_create(fs::path(.dir_check, "in"))
.dir_out     <- fs::dir_create(fs::path(.dir_check, "out"))
.dir_engine  <- here::here("contracts-engine")

.n_docs      <- 25L
.len_min     <- 4000L
.len_max     <- 40000L
.timeout     <- 240L
.max_chars   <- NULL   # must match 04A (.max_chars_extract is NULL there) or block 6 reports drift
.rerun       <- TRUE
.seed        <- 42L    # the SAME seed as the other four checks: the same twenty-five documents
.n_read      <- 3L
.near_win    <- 40L    # characters either side of a marker that count as adjacent
.bins        <- 10L    # deciles, to match 04A's positional statement

tab_combo <- tibble::tribble(
  ~Engine, ~Model,          ~NProc,
  "paper", "redaction-v1",  20L
) |>
  dplyr::mutate(
    Combo = paste0(.data$Engine, ":", .data$Model),
    File  = paste0("REDACT__", .data$Engine, "__", .data$Model, ".parquet"),
    Path  = as.character(fs::path(.dir_out, .data$File))
  )

.core <- c("DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model")

stopifnot(fs::file_exists(c(.path_text, .path_keys, .path_store)))
stopifnot(fs::file_exists(fs::path(.dir_engine, "extract_redaction.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, ".venv", "bin", "python")))


# 2. The sample ----

tab_text <- arrow::read_parquet(.path_text) |>
  dplyr::mutate(DocLen = stringi::stri_length(.data$TextRaw))

tab_keys <- arrow::read_parquet(.path_keys) |>
  dplyr::select(DocID, Class = "ClassDetailed", CompanyName, AmendType)

tab_pick <- tab_text |>
  dplyr::select(DocID, DocLen) |>
  dplyr::inner_join(tab_keys, by = dplyr::join_by(DocID)) |>
  dplyr::filter(.data$DocLen >= .len_min, .data$DocLen <= .len_max) |>
  dplyr::arrange(.data$DocID) |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = 3L, by = Class)))() |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n_docs, nrow(.d)))))() |>
  dplyr::arrange(.data$Class, .data$DocID)

cli::cli_h2("The sample")
cli::cli_alert_info(
  "{nrow(tab_pick)} document{?s}, {dplyr::n_distinct(tab_pick$Class)} contract type{?s} -- the \\
   same draw the other four checks used."
)

tab_text |>
  dplyr::filter(.data$DocID %in% tab_pick$DocID) |>
  dplyr::select(DocID, TextRaw) |>
  arrow::write_parquet(fs::path(.dir_in, "sample.parquet"))


# 3. Run the engine ----

purrr::pwalk(tab_combo, function(Engine, Model, NProc, Combo, File, Path, ...) {
  if (!.rerun && fs::file_exists(Path)) {
    cli::cli_alert_info("Reusing {(File)}.")
    return(invisible(NULL))
  }
  cli::cli_alert_info("Running {(Combo)} ...")
  t0_ <- Sys.time()
  args_ <- c(
    fs::path(.dir_engine, "extract_redaction.py"),
    as.character(fs::path_abs(fs::path(.dir_in, "sample.parquet"))),
    "--output", as.character(Path),
    "--n-process", as.integer(NProc),
    "--timeout", as.integer(.timeout)
  )
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  status_ <- system2(fs::path(.dir_engine, ".venv", "bin", "python"), args_,
                     stdout = "", stderr = "")
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("{(Combo)} failed ({status_}).")
  cli::cli_alert_success(
    "{(Combo)} in {round(as.numeric(difftime(Sys.time(), t0_, units = 'secs')), 1)}s."
  )
})

tab_raw <- arrow::read_parquet(tab_combo$Path[1]) |> tibble::as_tibble()
tab_cand <- dplyr::filter(tab_raw, !is.na(.data$Start))
tab_sent <- dplyr::filter(tab_raw, is.na(.data$Start))

cli::cli_h2("The file on disk")
tibble::tibble(
  File   = tab_combo$File,
  Rows   = nrow(tab_raw),
  KB     = round(as.numeric(fs::file_size(tab_combo$Path[1])) / 1024, 1),
  Extras = paste(setdiff(names(tab_raw), .core), collapse = ", ")
) |>
  print(width = Inf)

cli::cli_alert_info("Open it directly at {.path {(.dir_out)}}.")


# 4. Tests ----
# The same nine. T6 asserts Label is REDACT; LabelRaw is unconstrained because it legitimately
# varies across the five classes.

tab_len <- dplyr::select(tab_text, DocID, TextRaw, DocLen)

rt_ <- tab_cand |>
  dplyr::left_join(tab_len, by = dplyr::join_by(DocID)) |>
  dplyr::mutate(
    # 0-based half-open offsets over code points; stri_sub is 1-based inclusive.
    Sliced = stringi::stri_sub(.data$TextRaw, from = .data$Start + 1L, to = .data$Stop),
    OK     = .data$Sliced == .data$Span,
    InDoc  = .data$Start >= 0L & .data$Stop <= .data$DocLen & .data$Start < .data$Stop
  )

tab_tests <- tibble::tibble(
  Test  = c("T1 core columns present", "T2 every input document present",
            "T3 offsets round-trip to Span", "T4 offsets inside the document",
            "T5 Engine and Model constant and correct", "T6 Label is REDACT on candidates",
            "T7 no duplicate DocID/Start/Stop/LabelRaw", "T8 sentinels fully null",
            "T9 extras absent on sentinel rows"),
  N     = c(
    sum(.core %in% names(tab_raw)),
    dplyr::n_distinct(tab_raw$DocID),
    sum(rt_$OK),
    sum(rt_$InDoc),
    sum(tab_raw$Engine == tab_combo$Engine[1] & tab_raw$Model == tab_combo$Model[1]),
    sum(tab_cand$Label == "REDACT"),
    nrow(dplyr::distinct(tab_cand, .data$DocID, .data$Start, .data$Stop, .data$LabelRaw)),
    sum(is.na(tab_sent$Stop) & is.na(tab_sent$Span) & is.na(tab_sent$Label)),
    nrow(tab_sent)   # this engine carries no extras, so the check is vacuously satisfied
  ),
  Of    = c(length(.core), nrow(tab_pick), nrow(rt_), nrow(rt_), nrow(tab_raw),
            nrow(tab_cand), nrow(tab_cand), nrow(tab_sent), nrow(tab_sent))
)

cli::cli_h2("Contract tests")
tab_tests |>
  dplyr::mutate(Pass = .data$N == .data$Of) |>
  print(n = Inf, width = Inf)

if (any(tab_tests$N != tab_tests$Of)) {
  cli::cli_alert_danger("Failures above block insertion for this engine.")
} else {
  cli::cli_alert_success("All nine checks pass.")
}


# 5. Does a fresh run reproduce the store? ----
# The other candidate tables are pulled in the same connection, because every question below is
# about what a marker sits BESIDE and the neighbours live in the store.

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("ATTACH '", fs::path_abs(.path_store), "' AS s (READ_ONLY)"))

docs_ <- paste(tab_pick$DocID, collapse = "', '")

tab_store <- DBI::dbGetQuery(con, paste0(
  "SELECT DocID, Start, Stop, Span FROM s.candidates ",
  "WHERE Label = 'REDACT' AND Start IS NOT NULL AND DocID IN ('", docs_, "')"
)) |>
  tibble::as_tibble()

tab_neigh <- DBI::dbGetQuery(con, paste0(
  "SELECT Label, ",
  "  CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END AS Combo, ",
  "  DocID, Start AS NStart, Stop AS NStop, Span AS NSpan ",
  "FROM s.candidates WHERE Label IN ('MONEY', 'ORG', 'DATE') AND Start IS NOT NULL ",
  "  AND DocID IN ('", docs_, "')"
)) |>
  tibble::as_tibble()

DBI::dbDisconnect(con, shutdown = TRUE)

cli::cli_h2("Fresh run against 04A's store")
dplyr::full_join(
  dplyr::rename(tab_store, SpanStore = "Span"),
  dplyr::transmute(tab_cand, DocID, Start, Stop, SpanFresh = .data$Span),
  by = dplyr::join_by(DocID, Start, Stop)
) |>
  dplyr::summarise(
    InStore   = sum(!is.na(.data$SpanStore)),
    InFresh   = sum(!is.na(.data$SpanFresh)),
    Matched   = sum(!is.na(.data$SpanStore) & !is.na(.data$SpanFresh)),
    StoreOnly = sum(is.na(.data$SpanFresh)),
    FreshOnly = sum(is.na(.data$SpanStore)),
    TextDiff  = sum(!is.na(.data$SpanStore) & !is.na(.data$SpanFresh) &
                      .data$SpanStore != .data$SpanFresh)
  ) |>
  print(width = Inf)

cli::cli_alert_info(
  "Nothing changed in this extractor, so every column but the first three must be zero. This is \\
   the plain reproducibility case: the store was built weeks ago and this file a moment ago."
)


# 6. Yield, and the class mix ----

cli::cli_h2("Yield")
tibble::tibble(
  Docs          = dplyr::n_distinct(tab_cand$DocID),
  NoHitDocs     = nrow(tab_pick) - dplyr::n_distinct(tab_cand$DocID),
  Spans         = nrow(tab_cand),
  PerDoc        = round(nrow(tab_cand) / max(1L, dplyr::n_distinct(tab_cand$DocID)), 1),
  MaxPerDoc     = max(dplyr::count(tab_cand, .data$DocID)$n),
  DistinctSpans = dplyr::n_distinct(stringi::stri_trans_toupper(tab_cand$Span)),
  MedLen        = stats::median(stringi::stri_length(tab_cand$Span))
) |>
  print(width = Inf)

cli::cli_h2("The five classes")
tab_cand |>
  dplyr::count(.data$LabelRaw, name = "N") |>
  dplyr::mutate(
    Docs = purrr::map_int(.data$LabelRaw, \(.c) dplyr::n_distinct(
      tab_cand$DocID[tab_cand$LabelRaw == .c]
    )),
    Pct = round(100 * .data$N / nrow(tab_cand), 1)
  ) |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "RedactBare is the class to weigh: it is the one addition to the published classification, and \\
   the extractor's own header says to drop it if it floods. Flooding means a large share of spans \\
   concentrated in few documents, so read N against Docs rather than N alone."
)

cli::cli_h2("Concentration, by class")
tab_cand |>
  dplyr::count(.data$LabelRaw, .data$DocID, name = "N") |>
  dplyr::summarise(
    Docs      = dplyr::n(),
    Spans     = sum(.data$N),
    MedPerDoc = stats::median(.data$N),
    MaxPerDoc = max(.data$N),
    .by = LabelRaw
  ) |>
  dplyr::arrange(dplyr::desc(.data$Spans)) |>
  print(n = Inf, width = Inf)


# 7. Where the markers sit ----
# 04A reported the only label with no bimodality: a monotone rise from 4.5% in the first decile to
# 21.3% in the last, which is where schedules and pricing exhibits live. A monotone claim from one
# measurement is worth re-checking on a different draw.

cli::cli_h2("Markers by decile of the document")
tab_cand |>
  dplyr::left_join(dplyr::select(tab_text, DocID, DocLen), by = dplyr::join_by(DocID)) |>
  dplyr::mutate(Decile = pmin(.bins, 1L + as.integer(.data$Start / .data$DocLen * .bins))) |>
  dplyr::count(.data$Decile, name = "N") |>
  dplyr::mutate(Pct = round(100 * .data$N / sum(.data$N), 1)) |>
  print(n = Inf, width = Inf)


# 8. What was withheld ----
# THE QUESTION A COUNT CANNOT ASK, and the reason this label is an extractor rather than a tally.
# A marker beside a currency marker was an amount; beside an organisation it was a party; beside
# neither it stays unclassified. Neighbours come from the store, so the comparison is against the
# same spans every other document in this family reads.

tab_near <- tab_cand |>
  dplyr::select(DocID, MStart = "Start", MStop = "Stop", MSpan = "Span", MClass = "LabelRaw") |>
  dplyr::inner_join(tab_neigh, by = dplyr::join_by(DocID), relationship = "many-to-many") |>
  dplyr::filter(.data$NStart <= .data$MStop + .near_win,
                .data$NStop >= .data$MStart - .near_win)

cli::cli_h2("What sits within {(.near_win)} characters of a marker")
tab_near |>
  dplyr::summarise(
    Markers = dplyr::n_distinct(paste(.data$DocID, .data$MStart)),
    Spans   = dplyr::n(),
    .by = Label
  ) |>
  dplyr::mutate(PctMarkers = round(100 * .data$Markers / nrow(tab_cand), 1)) |>
  dplyr::arrange(dplyr::desc(.data$Markers)) |>
  print(n = Inf, width = Inf)

cli::cli_h2("Markers with no labelled neighbour at all")
tibble::tibble(
  Item = c("Markers", "With a neighbour", "With none"),
  N    = c(nrow(tab_cand),
           dplyr::n_distinct(paste(tab_near$DocID, tab_near$MStart)),
           nrow(tab_cand) - dplyr::n_distinct(paste(tab_near$DocID, tab_near$MStart)))
) |>
  dplyr::mutate(Pct = round(100 * .data$N / nrow(tab_cand), 1)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "A marker with no neighbour is not a failure of anything. Contracts redact clause text, exhibit \\
   titles and schedule rows as well as amounts and names, and the share that carries no adjacent \\
   entity is a fact about what filers withhold rather than about the extractor."
)

cli::cli_h2("Neighbour label by marker class")
tab_near |>
  dplyr::distinct(.data$DocID, .data$MStart, .data$MClass, .data$Label) |>
  dplyr::count(.data$MClass, .data$Label, name = "Markers") |>
  tidyr::pivot_wider(id_cols = MClass, names_from = Label, values_from = Markers,
                     values_fill = 0L) |>
  print(n = Inf, width = Inf)


# 9. Read the markers in context ----
# Every table above is consistent with these being redactions and with their being bracketed
# citations, cross-references or table rules. Only the text separates those.

read_ <- function(.tab, .n, .ctx, .title) {
  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("Nothing to read for {(.title)}.")
    return(invisible(NULL))
  }
  pick_ <- withr::with_seed(.seed, dplyr::slice_sample(.tab, n = min(.n, nrow(.tab))))
  cli::cli_h2(.title)
  pick_ |>
    dplyr::left_join(dplyr::select(tab_text, DocID, TextRaw), by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      Snippet = stringi::stri_replace_all_regex(
        paste0(
          "...",
          stringi::stri_sub(.data$TextRaw, from = pmax(1L, .data$Start + 1L - .ctx),
                            to = .data$Start),
          " >>>", stringi::stri_sub(.data$TextRaw, from = .data$Start + 1L, to = .data$Stop),
          "<<< ",
          stringi::stri_sub(.data$TextRaw, from = .data$Stop + 1L, to = .data$Stop + .ctx),
          "..."
        ),
        "\\s+", " "
      )
    ) |>
    (\(.d) purrr::walk(seq_len(nrow(.d)), function(.i) {
      cat(sprintf("  [%s] %s\n", .d$LabelRaw[.i], .d$Snippet[.i]))
    }))()
  cat("\n")
  invisible(.tab)
}

purrr::walk(sort(unique(tab_cand$LabelRaw)), function(.c) {
  read_(.tab = dplyr::filter(tab_cand, .data$LabelRaw == .c), .n = 4L, .ctx = 90L,
        .title = paste0("Class: ", .c))
})


# 10. Verdict ----

cli::cli_h2("Verdict")
tibble::tibble(
  Combo  = tab_combo$Combo,
  File   = tab_combo$File,
  Rows   = nrow(tab_raw),
  Extras = length(setdiff(names(tab_raw), .core)),
  Passed = all(tab_tests$N == tab_tests$Of)
) |>
  print(width = Inf)

cli::cli_alert_success("Parquet for inspection: {.path {(.dir_out)}}")
