# probe-publish-schema.R -- READ-ONLY: the facts the public schema and the model guides need ----------------------------
#
# Reads the staged package and a few pipeline files, and writes what the next 50B needs to know but cannot be read off
# the sample: the values every categorical column takes in the full data, the keyword catalogue, what the model
# folders hold, and the LexNLP wrapper as it was when the image was built.
#
# Nothing is listed recursively except the small model folders, so the parsed documents are never walked.
#
# HOW TO RUN: with the material-contracts project open, open this file and click "Source". It changes nothing and
# writes ~/Downloads/probe-publish-schema.txt plus a folder ~/Downloads/probe-files/. Attach the .txt and the folder's
# files in the chat. A few minutes.

dir_pkg   <- here::here("2_output", "50B-PublishData", "Stage", "matcon-data", "v1.0.0")
dir_mod   <- here::here("2_output", "03B-ClassifyTrainBERT", "model_final")
dir_files <- fs::path_expand("~/Downloads/probe-files")
path_rep  <- fs::path_expand("~/Downloads/probe-publish-schema.txt")
fs::dir_create(dir_files)

while (sink.number() > 0L) sink()
sink(file = path_rep, split = TRUE)
cat("probe-publish-schema.R |", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, "SET threads = 8")
q <- function(.sql) tibble::as_tibble(DBI::dbGetQuery(con, .sql))
lit <- function(.x) paste0("'", gsub("'", "''", as.character(.x), fixed = TRUE), "'")
show <- function(.tab, .n = 40L) print(as.data.frame(utils::head(.tab, .n)), row.names = FALSE, right = FALSE)
section <- function(.title, .code) {
  cat("\n########", .title, "########\n")
  tryCatch(.code, error = function(.e) cat("FAILED:", conditionMessage(.e), "\n"))
}
# Values of each named column, with their counts; long free text is cut.
values <- function(.file, .cols, .n = 25L) {
  for (col_ in .cols) {
    cat("\n--", col_, "\n")
    show(q(sprintf(
      "SELECT left(CAST(%s AS VARCHAR), 70) AS Value, count(*) AS N FROM read_parquet(%s) GROUP BY ALL ORDER BY N DESC",
      col_, lit(.file)
    )), .n)
  }
}

section("A. Columns of Contracts that D2 and D15 remove, and anything else about money", {
  cols <- q(sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", lit(fs::path(dir_pkg, "core", "Contracts.parquet"))))
  cat("columns:", nrow(cols), "\n")
  cat("Compustat:", intersect(cols$column_name, c("gvkey", "datadate", "cyear", "fyear", "fqtr")), "\n")
  cat("money-named:", grep("Money|Amount|Dollar|USD", cols$column_name, value = TRUE), "\n")
})

section("B. Categorical values of Contracts", {
  values(
    .file = fs::path(dir_pkg, "core", "Contracts.parquet"),
    .cols = c("RemClass", "SampleStepDesc", "ItemEra", "ClassDetailedFlag", "ClassBroadFlag", "AmendTypeFlag",
              "StartSource", "DurationSource", "DurationDropped", "TermKind", "LawKind", "LawJurisdictionLevel",
              "DocExt", "FormType", "HierConsistent", "Removed", "PrimaryFiler")
  )
  cat("\n-- DateFiled range and rows per year\n")
  show(q(sprintf("SELECT year(DateFiled) AS Year, count(*) AS N FROM read_parquet(%s) GROUP BY ALL ORDER BY 1",
                 lit(fs::path(dir_pkg, "core", "Contracts.parquet")))), 40L)
})

section("C. Categorical values of the other core tables", {
  values(fs::path(dir_pkg, "core", "Places.parquet"), c("GeoLevel", "PartyRole", "InLawClause"))
  cat("\n-- country names per ISO code, where a code has more than one\n")
  show(q(sprintf(
    paste("SELECT GeoCountryIso, list(DISTINCT GeoCountry) AS Names, count(*) AS N FROM read_parquet(%s)",
          "GROUP BY 1 HAVING count(DISTINCT GeoCountry) > 1 ORDER BY N DESC"),
    lit(fs::path(dir_pkg, "core", "Places.parquet"))
  )), 20L)
  values(fs::path(dir_pkg, "core", "Summaries.parquet"), c("FormType", "SumIsSingle"))
  values(fs::path(dir_pkg, "core", "TermDocs.parquet"), c("Family", "HasTerm"))
  values(fs::path(dir_pkg, "core", "CtoOrders.parquet"), c("Status", "LinkStatus", "SourceForm", "IsExtension"))
})

section("D. Categorical values of the span files", {
  s_ <- function(.f) fs::path(dir_pkg, "spans", paste0(.f, ".parquet"))
  values(s_("org_mentions"), c("PartyRole", "MatchKind", "NameFrom", "MergeKind", "IsFirst"))
  values(s_("places_geo"), c("GeoUnit", "GeoLevel", "GeoStateFrom", "Ambiguous", "Attached", "InLawClause"))
  values(s_("law_clauses"), c("LawKind", "JurisdictionLevel"))
  values(s_("date_spans"), c("Side", "CueHit", "Parsed", "HasEndCue"))
  values(s_("term_spans"), c("TermKind", "TermUnit", "PeriodKind", "PeriodCue", "IsOpen"))
  values(s_("redact_spans"), c("Kind", "Bracketed", "Withheld", "RedactedEntity"))
  cat("\n-- org rows without a span\n")
  show(q(sprintf("SELECT PartyRole, count(*) AS N FROM read_parquet(%s) WHERE MentionStart IS NULL GROUP BY ALL",
                 lit(s_("org_mentions")))))
})

section("E. Text files: parse flags and file types", {
  for (slug_ in c("exhibit10", "8k", "8ka", "cto")) {
    cat("\n--", slug_, "\n")
    show(q(sprintf(
      paste("SELECT DocExt, count(*) AS Docs, count(*) FILTER (WHERE ErrParse) AS ErrParse,",
            "count(MsgParse) AS MsgParse FROM read_parquet(%s) GROUP BY ALL ORDER BY 2 DESC"),
      lit(fs::path(dir_pkg, "text", slug_, "*.parquet"))
    )))
  }
  cat("\n-- MsgParse values\n")
  show(q(sprintf(
    paste("SELECT left(MsgParse, 90) AS Msg, count(*) AS N FROM read_parquet(%s)",
          "WHERE MsgParse IS NOT NULL GROUP BY ALL ORDER BY N DESC"),
    lit(fs::path(dir_pkg, "text", "*", "*.parquet"))
  )), 15L)
})

section("F. Labels: columns, rounds, and the detailed-to-broad map", {
  f_ <- fs::path(dir_pkg, "core", "ClassificationLabels.parquet")
  show(q(sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", lit(f_)))[, c("column_name", "column_type")])
  show(q(sprintf("SELECT ClassDetailed, ClassBroad, count(*) AS N FROM read_parquet(%s) GROUP BY ALL ORDER BY 1, 2",
                 lit(f_))))
  show(q(sprintf("SELECT LabelRound, Fold, count(*) AS N FROM read_parquet(%s) GROUP BY ALL ORDER BY 1, 2", lit(f_))))
})

section("G. Keyword tables: the catalogue and one table", {
  dir_kw <- fs::path(dir_pkg, "core", "keyword_tables")
  show(q(sprintf("SELECT * FROM read_parquet(%s)", lit(fs::path(dir_kw, "catalogue.parquet")))), 30L)
  one <- fs::dir_ls(dir_kw, regexp = "keyword_table_detailed_W512\\.parquet$")
  if (length(one) == 1L) {
    show(q(sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", lit(one)))[, c("column_name", "column_type")])
    show(q(sprintf("SELECT * FROM read_parquet(%s) LIMIT 8", lit(one))))
  }
})

section("H. The model folders", {
  for (d_ in fs::dir_ls(dir_mod, type = "directory", regexp = "__FINAL$")) {
    cat("\n--", fs::path_file(d_), "\n")
    info_ <- fs::dir_info(d_, recurse = TRUE, type = "file")
    show(tibble::tibble(File = as.character(fs::path_rel(info_$path, d_)), Bytes = as.numeric(info_$size)), 30L)
  }
  d1 <- fs::dir_ls(dir_mod, type = "directory", regexp = "ClassDetailed.*L256.*__FINAL$")[1]
  cat("\n-- top-level config.json of", fs::path_file(d1), "(home paths shortened)\n")
  cat(gsub("/Users/[^/\"]+/", "~/", readLines(fs::path(d1, "config.json"), warn = FALSE)), sep = "\n")
  cat("\n-- run.log of the same model (home paths shortened)\n")
  cat(gsub("/Users/[^/\"]+/", "~/", readLines(fs::path(d1, "run.log"), warn = FALSE)), sep = "\n")
  hf_ <- fs::path(d1, "model", "config.json")
  if (fs::file_exists(hf_)) {
    cat("\n-- model/config.json\n")
    cat(readLines(hf_, warn = FALSE), sep = "\n")
  }
  cat("\n-- train_log.parquet\n")
  tl_ <- fs::path(d1, "train_log.parquet")
  show(q(sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", lit(tl_)))[, c("column_name", "column_type")])
  show(q(sprintf("SELECT * FROM read_parquet(%s) LIMIT 5", lit(tl_))))
  cat("\n-- deployed.parquet\n")
  show(q(sprintf("SELECT * FROM read_parquet(%s)", lit(fs::path(dir_mod, "deployed.parquet")))))
})

section("I. The LexNLP wrapper at the commit the image was built from", {
  git_show <- function(.spec, .out) {
    system2("git", c("-C", shQuote(here::here()), "show", shQuote(.spec)), stdout = fs::path(dir_files, .out))
    cat(.spec, "->", .out, "|", length(readLines(fs::path(dir_files, .out), warn = FALSE)), "lines\n")
  }
  git_show("e3f9721:contracts-lexnlp/extract_lexnlp.py", "lexnlp-extract_lexnlp.py")
  git_show("e3f9721:contracts-lexnlp/Dockerfile", "lexnlp-Dockerfile")
  git_show("e3f9721:contracts-lexnlp/image_spec.py", "lexnlp-image_spec.py")
  git_show("HEAD:contracts-classify/classify_apply.py", "classify_apply.py")
  git_show("HEAD:contracts-classify/README.md", "contracts-classify-README.md")
})

DBI::dbDisconnect(con, shutdown = TRUE)
cat("\nDone. Nothing was changed. Report:", path_rep, "| files:", dir_files, "\n")
sink()
