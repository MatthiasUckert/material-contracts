# What the redaction extractor is actually capturing ----
#
# Standalone. Run after 04A. Reads the store and the canonical text, writes nothing, prints
# seven blocks.
#
# REDACT is the one label nobody has looked at directly. It has no anchor, its cues are all
# unmeasured, and the count it feeds is a variable the paper already reports -- so the only
# check available is to look at the output and see whether it is what it claims to be.

.path_text  <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")
.path_anch  <- here::here("2_output", "04A-EntityExtract", "sample_anchors.parquet")
.path_store <- here::here("2_output", "04A-EntityExtract", "Store", "EntityCandidates.duckdb")
.gap        <- 200L   # markers closer than this belong to one redacted region

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("ATTACH '", fs::path_abs(.path_store), "' AS s (READ_ONLY)"))
DBI::dbExecute(con, paste0(
  "CREATE OR REPLACE TABLE txt AS SELECT DocID, TextRaw, length(TextRaw) AS DocLen ",
  "FROM read_parquet('", fs::path_abs(.path_text), "')"
))
DBI::dbExecute(con, paste0(
  "CREATE OR REPLACE TABLE red AS SELECT DocID, Start, Stop, Span, LabelRaw ",
  "FROM s.candidates WHERE Label = 'REDACT' AND Start IS NOT NULL"
))

n_red_ <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS N FROM red")$N
if (n_red_ == 0L) {
  cli::cli_abort(c("No REDACT candidates in the store.",
                   "i" = "Run 04A with paper:redaction-v1 enabled before this."))
}
cli::cli_alert_info("{n_red_} redaction marker{?s} in the store")


# 1. Class mix ------------------------------------------------------------
# RedactBare is the class to watch: it was added outside the published method and a reading
# session suspected it of catching typographic rules rather than removals.

cli::cli_h2("Marker classes")
DBI::dbGetQuery(con, paste0(
  "SELECT LabelRaw AS Class, COUNT(*) AS Markers, COUNT(DISTINCT DocID) AS Docs ",
  "FROM red GROUP BY LabelRaw ORDER BY Markers DESC"
)) |>
  tibble::as_tibble() |>
  dplyr::mutate(PctMarkers = Markers / sum(Markers), PctDocs = Docs / 4398) |>
  print(n = 10)


# 2. Markers against regions ----------------------------------------------
# A table of redacted cells contributes dozens of markers to one withheld thing. Counting
# markers therefore measures how tabular a contract is as much as how much was removed, which
# is why the per-document count is worth reporting both ways.

cli::cli_h2("Markers against distinct regions, per document")
DBI::dbExecute(con, paste0(
  "CREATE OR REPLACE TABLE region AS ",
  "WITH o AS (SELECT DocID, Start, Stop, ",
  "             LAG(Stop) OVER (PARTITION BY DocID ORDER BY Start) AS PrevStop FROM red), ",
  "f AS (SELECT *, CASE WHEN PrevStop IS NULL OR Start - PrevStop > ", as.integer(.gap),
  "        THEN 1 ELSE 0 END AS NewRegion FROM o) ",
  "SELECT DocID, Start, Stop, SUM(NewRegion) OVER (PARTITION BY DocID ORDER BY Start ",
  "  ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS RegionID FROM f"
))
DBI::dbGetQuery(con, paste0(
  "WITH d AS (SELECT DocID, COUNT(*) AS NMarkers, COUNT(DISTINCT RegionID) AS NRegions ",
  "           FROM region GROUP BY DocID) ",
  "SELECT COUNT(*) AS Docs, SUM(NMarkers) AS Markers, SUM(NRegions) AS Regions, ",
  "  median(NMarkers) AS MedMarkers, median(NRegions) AS MedRegions, ",
  "  max(NMarkers) AS MaxMarkers, ",
  "  SUM(CASE WHEN NMarkers >= 100 THEN NMarkers ELSE 0 END) AS MarkersInHeavyDocs, ",
  "  SUM(CASE WHEN NMarkers >= 100 THEN 1 ELSE 0 END) AS HeavyDocs FROM d"
)) |>
  tibble::as_tibble() |> print()

cli::cli_alert_info(
  "MarkersInHeavyDocs is the share of the total contributed by documents with a hundred or \\
   more. If that is most of the count, the variable is measuring tables."
)


# 3. The heaviest documents -----------------------------------------------

cli::cli_h2("Documents contributing the most markers")
DBI::dbGetQuery(con, paste0(
  "WITH d AS (SELECT DocID, COUNT(*) AS NMarkers, COUNT(DISTINCT RegionID) AS NRegions ",
  "           FROM region GROUP BY DocID) ",
  "SELECT d.DocID, d.NMarkers, d.NRegions, t.DocLen ",
  "FROM d JOIN txt t USING (DocID) ORDER BY d.NMarkers DESC LIMIT 10"
)) |>
  tibble::as_tibble() |>
  dplyr::mutate(MarkersPerRegion = round(NMarkers / NRegions, 1)) |>
  print(n = 10)


# 4. What the markers look like, by class ---------------------------------
# The filter runs on the sample, not on the scan, because DuckDB applies USING SAMPLE to the
# table before the WHERE clause -- drawing ten rows from sixty thousand and then filtering
# returns nothing and reads like a clean result.

cli::cli_h2("Markers in context, three per class")
DBI::dbGetQuery(con, paste0(
  "WITH s AS ( ",
  "  SELECT r.LabelRaw, r.Span, ",
  "    substring(t.TextRaw, greatest(1, r.Start - 45), least(45, r.Start)) AS Before, ",
  "    substring(t.TextRaw, r.Stop + 1, 45) AS After, ",
  "    row_number() OVER (PARTITION BY r.LabelRaw ORDER BY hash(r.DocID || CAST(r.Start AS VARCHAR))) AS Rn ",
  "  FROM red r JOIN txt t USING (DocID)) ",
  "SELECT LabelRaw, Span, Before, After FROM s WHERE Rn <= 3 ORDER BY LabelRaw, Rn"
)) |>
  tibble::as_tibble() |> print(n = 30)


# 5. Did the exclusions do anything? --------------------------------------
# Counted from the raw text rather than from the store, so the two can be compared. Page
# filler and the opening legend are the two the extractor drops, and both were sized by a
# reading session rather than measured; this is the measurement.

cli::cli_h2("Every bracket in the corpus, and how it would classify")
DBI::dbGetQuery(con, paste0(
  "WITH b AS (SELECT DocID, unnest(regexp_extract_all(TextRaw, '\\[[^\\[\\]]{0,80}\\]')) AS Ind ",
  "           FROM txt), ",
  "n AS (SELECT DocID, upper(regexp_replace(Ind, '\\s+', ' ', 'g')) AS U FROM b) ",
  "SELECT COUNT(*) AS AllBrackets, ",
  "  SUM(CASE WHEN regexp_matches(U, 'LEFT\\s*BLANK|PAGE\\s*FOLLOWS|SIGNATURE\\s*PAGE') ",
  "      THEN 1 ELSE 0 END) AS PageFillerDropped, ",
  "  SUM(CASE WHEN regexp_matches(U, '^\\[[*\\s]+\\]$') THEN 1 ELSE 0 END) AS SymbolShaped, ",
  "  SUM(CASE WHEN NOT regexp_matches(U, 'LEFT\\s*BLANK|PAGE\\s*FOLLOWS|SIGNATURE\\s*PAGE') ",
  "      AND regexp_matches(U, 'CONFIDENTIAL|REDACT|\\bCTR\\b') THEN 1 ELSE 0 END) AS ExplicitShaped ",
  "FROM n"
)) |>
  tibble::as_tibble() |> print()

cli::cli_alert_info(
  "AllBrackets counts everything in square brackets, most of which is cross-references and \\
   defined terms. The published method classified the same population; PageFillerDropped is \\
   what it counted as an omission and this extractor does not."
)


# 6. Where in the document ------------------------------------------------

cli::cli_h2("Position of markers within the document")
DBI::dbGetQuery(con, paste0(
  "SELECT r.LabelRaw AS Class, COUNT(*) AS Markers, ",
  "  round(median(((r.Start + r.Stop) / 2.0) / t.DocLen), 3) AS MedianPos, ",
  "  round(quantile_cont(((r.Start + r.Stop) / 2.0) / t.DocLen, 0.10), 3) AS P10, ",
  "  round(quantile_cont(((r.Start + r.Stop) / 2.0) / t.DocLen, 0.90), 3) AS P90 ",
  "FROM red r JOIN txt t USING (DocID) WHERE t.DocLen > 0 ",
  "GROUP BY r.LabelRaw ORDER BY Markers DESC"
)) |>
  tibble::as_tibble() |> print(n = 10)


# 7. By contract type -----------------------------------------------------
# Licences and research agreements should lead if the markers are hiding commercial terms:
# royalties and milestones are what gets withheld. If employment agreements lead instead,
# the extractor is finding something else.

if (fs::file_exists(.path_anch)) {
  cli::cli_h2("Redaction by contract type")
  DBI::dbExecute(con, paste0(
    "CREATE OR REPLACE TABLE cls AS SELECT DocID, ClassDetailed FROM read_parquet('",
    fs::path_abs(.path_anch), "')"
  ))
  DBI::dbGetQuery(con, paste0(
    "WITH d AS (SELECT DocID, COUNT(*) AS NMarkers, COUNT(DISTINCT RegionID) AS NRegions ",
    "           FROM region GROUP BY DocID) ",
    "SELECT c.ClassDetailed AS Class, COUNT(*) AS Docs, ",
    "  SUM(CASE WHEN d.DocID IS NOT NULL THEN 1 ELSE 0 END) AS DocsWithRedaction, ",
    "  COALESCE(SUM(d.NRegions), 0) AS Regions ",
    "FROM cls c LEFT JOIN d USING (DocID) GROUP BY c.ClassDetailed ORDER BY Docs DESC"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      PctWithRedaction = DocsWithRedaction / Docs,
      RegionsPerDoc    = round(Regions / Docs, 2)
    ) |>
    print(n = 15)
} else {
  cli::cli_alert_warning("No sample anchors at {(.path_anch)}; skipping the contract-type block.")
}

DBI::dbDisconnect(con, shutdown = TRUE)
