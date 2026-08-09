# Where the money regex and the transformer disagree ----
#
# Run after 04A. Nothing here writes anything; it reads the store and prints four blocks.
# The point is to find shape families the regex has no pattern for, which is a different
# question from whether it wins on average.

.path_text  <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")
.path_store <- here::here("2_output", "04A-EntityExtract", "Store", "EntityCandidates.duckdb")
.rgx        <- "paper:moneyregex-v1"
.trf        <- "spacy:en_core_web_trf"

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("ATTACH '", fs::path_abs(.path_store), "' AS s (READ_ONLY)"))
DBI::dbExecute(con, paste0(
  "CREATE OR REPLACE TABLE txt AS SELECT DocID, TextRaw FROM read_parquet('",
  fs::path_abs(.path_text), "')"
))
DBI::dbExecute(con, paste0(
  "CREATE OR REPLACE TABLE m AS SELECT DocID, Start, Stop, Span, ",
  "  CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END AS Combo ",
  "FROM s.candidates WHERE Label = 'MONEY' AND Start IS NOT NULL"
))

# Digits collapse to N so that "$5,000,000" and "$250,000" land in one row. Without it every
# amount is its own shape and the table is a list rather than a diagnosis.
.shape <- "regexp_replace(a.Span, '[0-9]+', 'N', 'g')"

# One engine's spans that the other's do not overlap. Overlap, not equality: the two often
# find the same amount with different boundaries, and counting those as disagreements would
# bury the real gaps.
.only <- paste0(
  "WITH a AS (SELECT * FROM m WHERE Combo = ?), b AS (SELECT * FROM m WHERE Combo = ?) ",
  "SELECT ", .shape, " AS Shape, COUNT(*) AS N, ",
  "       COUNT(DISTINCT a.DocID) AS Docs, any_value(a.Span) AS Example ",
  "FROM a WHERE NOT EXISTS (SELECT 1 FROM b WHERE b.DocID = a.DocID ",
  "                         AND b.Start < a.Stop AND a.Start < b.Stop) ",
  "GROUP BY 1 ORDER BY N DESC LIMIT 30"
)

cli::cli_h2("Found by the transformer, missed by the regex")
DBI::dbGetQuery(con, .only, params = list(.trf, .rgx)) |>
  tibble::as_tibble() |> print(n = 30)

cli::cli_h2("Found by the regex, missed by the transformer")
DBI::dbGetQuery(con, .only, params = list(.rgx, .trf)) |>
  tibble::as_tibble() |> print(n = 30)

# The 41% of currency-adjacent redaction sites the regex does not reach. These are the ones
# worth a pattern, because the site definition already says an amount was there.
cli::cli_h2("Redaction sites after a currency symbol that the regex misses")
DBI::dbGetQuery(con, paste0(
  "WITH site AS ( ",
  "  SELECT r.DocID, r.Start, r.Stop, ",
  "    substring(t.TextRaw, greatest(1, r.Start - 30), least(30, r.Start - 1)) AS Before, ",
  "    substring(t.TextRaw, r.Start + 1, 40) AS After ",
  "  FROM s.candidates r JOIN txt t USING (DocID) ",
  "  WHERE r.Label = 'REDACT' AND r.Start IS NOT NULL ",
  "    AND regexp_matches(substring(t.TextRaw, greatest(1, r.Start - 2), least(3, r.Start)), ",
  "                       '[$\u00a3\u00a5\u20ac]\\s*$')) ",
  "SELECT regexp_replace(s.After, '[0-9]+', 'N', 'g') AS AfterShape, COUNT(*) AS N, ",
  "       any_value(s.Before || ' <<>> ' || s.After) AS Example ",
  "FROM site s WHERE NOT EXISTS (SELECT 1 FROM m WHERE m.Combo = '", .rgx, "' ",
  "  AND m.DocID = s.DocID AND m.Stop >= s.Start - 3 AND m.Start <= s.Stop + 3) ",
  "GROUP BY 1 ORDER BY N DESC LIMIT 25"
)) |>
  tibble::as_tibble() |> print(n = 25)

# A missed shape is only worth a pattern if it is not already noise. Read a handful whole
# before writing anything: a shape family with 400 members is 400 chances to be wrong.
cli::cli_h2("Transformer-only spans in context, a sample")
DBI::dbGetQuery(con, paste0(
  "WITH a AS (SELECT * FROM m WHERE Combo = '", .trf, "') ",
  "SELECT a.Span, ",
  "  substring(t.TextRaw, greatest(1, a.Start - 45), least(45, a.Start)) AS Before, ",
  "  substring(t.TextRaw, a.Stop + 1, 35) AS After ",
  "FROM a JOIN txt t USING (DocID) ",
  "WHERE NOT EXISTS (SELECT 1 FROM m b WHERE b.Combo = '", .rgx, "' ",
  "  AND b.DocID = a.DocID AND b.Start < a.Stop AND a.Start < b.Stop) ",
  "USING SAMPLE 25 ROWS"
)) |>
  tibble::as_tibble() |> print(n = 25)

DBI::dbDisconnect(con, shutdown = TRUE)
