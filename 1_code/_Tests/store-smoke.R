# store-smoke: the per-label store, on a copy ----
#
# WHAT THIS IS
# The test for _Commons/_Store.R before it is pointed at the real store. It works on a COPY of
# 04A's store, so a migration that goes wrong costs a file nobody needed. The original is never
# opened for writing.
#
# WHAT IT ESTABLISHES, in order
#   1. The split moves every row and loses none -- per label, counted before and after.
#   2. `candidates` becomes a VIEW and every existing reader still works, unchanged. This is the
#      claim the whole design rests on: ner_db_missing(), 04A's overview, alignment and contrast
#      layers all read `candidates` and none of them is edited.
#   3. The ledger is untouched, so resumability survives the migration.
#   4. Ingest routes a wide parquet into the right table with its extras, and a core-only parquet
#      into the right table without them -- the LexNLP and spaCy cases.
#   5. A second ingest of the same file is a no-op, because the ledger says so.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII.


# 1. Configuration ----

.path_live <- here::here("2_output", "04A-EntityExtract", "Store", "EntityCandidates.duckdb")
.dir_test  <- fs::dir_create(here::here("2_output", "_Probe", "store-smoke"))
.path_test <- fs::path(.dir_test, "SplitTest.duckdb")
.dir_probe <- here::here("2_output", "_Probe")

.fresh     <- TRUE   # TRUE re-copies the live store, discarding any earlier test run

source(here::here("1_code", "_Commons", "_NER.R"),   encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Store.R"), encoding = "UTF-8")

stopifnot(fs::file_exists(.path_live))


# 2. A copy, never the original ----

# THE WAL MUST GO WITH THE DATABASE. DuckDB writes a sidecar <file>.wal and replays it on open, so
# deleting only the .duckdb leaves the previous session's uncheckpointed transactions to be applied
# on top of the fresh copy. A broken view definition written by an earlier run comes straight back
# that way, and the copy looks pristine.
if (.fresh) {
  purrr::walk(c(.path_test, paste0(.path_test, ".wal")),
              \(.p) if (fs::file_exists(.p)) fs::file_delete(.p))
}
if (!fs::file_exists(.path_test)) {
  fs::file_copy(.path_live, .path_test)
  cli::cli_alert_info(
    "Copied the live store to {.path {(.path_test)}} \\
     ({round(as.numeric(fs::file_size(.path_test)) / 1024^2, 1)} MB)."
  )
}


# 3. Before ----

# EVERY DBI RESULT IS WRAPPED. dbGetQuery() returns a data.frame, whose print method has no n or
# width argument, so print(n = Inf) either truncates silently or errors depending on the value.
db_ <- function(.con, .sql) tibble::as_tibble(DBI::dbGetQuery(.con, .sql))

con <- ner_db_connect(.db_path = .path_test, .read_only = TRUE)
tab_before  <- db_(con, "SELECT Label, COUNT(*) AS Rows FROM candidates GROUP BY Label")
runs_before <- db_(con, "SELECT COUNT(*) AS N FROM runs")$N
DBI::dbDisconnect(con, shutdown = TRUE)

cli::cli_h2("Before the split")
tab_before |>
  dplyr::arrange(dplyr::desc(.data$Rows)) |>
  print(n = Inf, width = Inf)
cli::cli_alert_info("{format(runs_before, big.mark = ',')} ledger row{?s}.")


# 4. Split ----

res_split <- ner_db_split(.db_path = .path_test, .quiet = FALSE)


# 5. After: nothing lost, and candidates is a view ----

con <- ner_db_connect(.db_path = .path_test, .read_only = TRUE)

kind_ <- db_(con, paste0(
  "SELECT table_name, table_type FROM information_schema.tables ORDER BY table_name"
))
tab_after  <- db_(con, "SELECT Label, COUNT(*) AS Rows FROM candidates GROUP BY Label")
runs_after <- db_(con, "SELECT COUNT(*) AS N FROM runs")$N

DBI::dbDisconnect(con, shutdown = TRUE)

cli::cli_h2("Objects in the store")
print(kind_, n = Inf, width = Inf)
cli::cli_alert_info(
  "One BASE TABLE per label plus runs, and exactly one VIEW. A second view, or a label table \\
   missing, is what the checks below are for."
)

cli::cli_h2("Row counts, before against after")
dplyr::full_join(
  dplyr::rename(tab_before, Before = "Rows"),
  dplyr::rename(tab_after, After = "Rows"),
  by = dplyr::join_by(Label)
) |>
  dplyr::mutate(Diff = .data$After - .data$Before) |>
  dplyr::arrange(dplyr::desc(.data$Before)) |>
  print(n = Inf, width = Inf)

tibble::tibble(
  Check = c("candidates is a VIEW", "Total rows unchanged", "Ledger rows unchanged",
            "One table per label present"),
  Pass  = c(
    identical(kind_$table_type[kind_$table_name == "candidates"], "VIEW"),
    sum(tab_before$Rows) == sum(tab_after$Rows),
    runs_before == runs_after,
    all(store_table(tab_before$Label) %in% kind_$table_name)
  )
) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "The second row is the migration's whole obligation. The fourth is what makes the view complete: \\
   a label whose table is missing would vanish from every reader without an error anywhere."
)


# 6. Every existing reader still works, unedited ----
# ner_db_missing() is the one that matters -- resumability is built on it, it queries the ledger and
# the view, and it has not been touched.

tab_text <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")

cli::cli_h2("ner_db_missing() against the split store")
purrr::map(list(
  list(e = "lexnlp", m = "lexnlp",          l = c("ORG", "GPE", "DATE", "MONEY")),
  list(e = "spacy",  m = "en_core_web_trf", l = c("ORG", "PERSON", "GPE", "DATE", "MONEY")),
  list(e = "paper",  m = "redaction-v1",    l = "REDACT")
), function(.x) {
  miss_ <- ner_db_missing(
    .db_path = .path_test, .inputs = tab_text,
    .engine = .x$e, .model = .x$m, .labels = .x$l, .quiet = TRUE
  )
  tibble::tibble(Engine = .x$e, Model = .x$m,
                 Labels = paste(.x$l, collapse = ","), Missing = length(miss_))
}) |>
  purrr::list_rbind() |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "Zero missing for a combination the store already holds is the point: the ledger survived the \\
   migration, so a re-run would extract nothing rather than everything."
)


# 7. Ingest: a wide parquet and a core-only one ----
# The LexNLP case and the spaCy case, from the Check-NER-* output that is already on disk. The
# documents are already in the store, so ingest is a no-op on the ledger -- which is itself the
# test in block 8. Here the store is cleared for those pairs first so the insert has work to do.

files_ <- c(
  wide = fs::path(.dir_probe, "Check-NER-DATE", "out", "DATE__lexnlp__lexnlp.parquet"),
  core = fs::path(.dir_probe, "Check-NER-DATE", "out", "DATE__spacy__en_core_web_trf.parquet")
)

if (all(fs::file_exists(files_))) {
  docs_ <- arrow::read_parquet(files_[["wide"]])$DocID |> unique()

  ner_db_clear(.db_path = .path_test, .run = c("lexnlp", "spacy:en_core_web_trf"),
               .doc_ids = docs_, .labels = "DATE", .quiet = FALSE)

  cli::cli_h2("Ingest: LexNLP DATE (wide -- carries DateValue and Score)")
  ner_db_append(.db_path = .path_test, .parquet = files_[["wide"]], .labels = "DATE",
                .expect_engine = "lexnlp", .expect_model = "lexnlp", .quiet = FALSE)

  cli::cli_h2("Ingest: spaCy DATE (core only)")
  ner_db_append(.db_path = .path_test, .parquet = files_[["core"]], .labels = "DATE",
                .expect_engine = "spacy", .expect_model = "en_core_web_trf", .quiet = FALSE)

  con <- ner_db_connect(.db_path = .path_test, .read_only = TRUE)
  cli::cli_h2("What landed in the date table")
  db_(con, paste0(
    "SELECT Engine, Model, COUNT(*) AS Rows, ",
    "  COUNT(DateValue) AS WithDate, COUNT(DateScore) AS WithScore ",
    "FROM date GROUP BY Engine, Model ORDER BY Rows DESC"
  )) |>
    print(n = Inf, width = Inf)

  cli::cli_h2("Five parsed dates, read back")
  db_(con, paste0(
    "SELECT Span, DateValue, DateScore FROM date WHERE DateValue IS NOT NULL LIMIT 5"
  )) |>
    print(n = Inf, width = Inf)
  DBI::dbDisconnect(con, shutdown = TRUE)

  cli::cli_alert_info(
    "WithDate populated for lexnlp and null for spaCy is the seam working as designed: the same \\
     ingest path takes a wide parquet and a core-only one, and the extras follow the engine that \\
     had them."
  )


  # 8. The same file twice ----

  cli::cli_h2("Ingesting the same file a second time")
  again_ <- ner_db_append(.db_path = .path_test, .parquet = files_[["wide"]], .labels = "DATE",
                          .expect_engine = "lexnlp", .expect_model = "lexnlp", .quiet = FALSE)
  cli::cli_alert_info(
    "{again_$candidates} candidate{?s} written. Anything but zero means the ledger is not blocking \\
     a repeat, and every append would double the rows it wrote."
  )
} else {
  cli::cli_alert_warning(
    "Check-NER-DATE output not found; blocks 7 and 8 skipped. Run Check-NER-DATE.R first."
  )
}

cli::cli_alert_success("Test store left at {.path {(.path_test)}}; the live store was not opened.")
