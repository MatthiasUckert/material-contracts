# Stage 1: locate the contracting party ----
#
# WHAT THIS ASKS
# The EDGAR anchor is currently used to ask whether the filer is SOMEWHERE among a document's
# organisation spans. With 80 to 509 ORG candidates per document that question is passed almost
# trivially. This script asks the positional question instead: WHERE is the filer named, and is
# that location regular enough to serve as a coordinate origin for everything else.
#
# Nothing is written except one probe table. The 04A store is attached read-only.
#
# PREDICTIONS, STATED BEFORE LOOKING
#   P1  Anchor positions are bimodal: mass in the opening ~5% and again at the signature block.
#   P2  LexNLP's anchor occurrences are bimodal; the spaCy models are flat throughout.
#   P1 failing is fatal to Stages 2 to 4: it would mean the anchor matches incidental mentions and
#   "first occurrence = preamble" is not a rule.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_04a    <- here::here("2_output", "04A-EntityExtract")
.path_text  <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys  <- fs::path(.dir_04a, "sample_anchors.parquet")
.path_store <- fs::path(.dir_04a, "Store", "EntityCandidates.duckdb")
.dir_out    <- fs::dir_create(here::here("2_output", "_Probe"))

.n_examples <- 20L   # preambles printed at the end
.ctx        <- 220L  # characters either side of the span in those examples
.seed       <- 42L

stopifnot(fs::file_exists(c(.path_text, .path_keys, .path_store)))


# 2. The anchor key ----
# Inlined rather than sourced from 04B, which is being replaced. This is the reference
# implementation now: uppercase, strip punctuation, drop a leading THE, then strip trailing
# corporate suffixes. "The Boeing Company" -> "BOEING". Keys under five characters are dropped,
# because a three-character key matches half the corpus by containment.

norm_company <- function(.x) {
  if (FALSE) .x <- c("ACME HOLDINGS, INC.", "The Boeing Company", "Beta Bank, N.A.")

  suffix_ <- c("INC", "INCORPORATED", "CORP", "CORPORATION", "LLC", "LLP", "LP", "LTD", "LIMITED",
               "PLC", "NV", "BV", "SA", "AG", "GMBH", "CO", "COMPANY", "TRUST", "NA")

  out_ <- .x |>
    stringi::stri_trans_toupper() |>
    stringi::stri_replace_all_regex("[^A-Z0-9 ]", " ") |>
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_trim_both() |>
    stringi::stri_replace_first_regex("^THE ", "")

  purrr::map_chr(out_, function(.s) {
    if (is.na(.s)) return(NA_character_)
    toks_ <- strsplit(.s, " ", fixed = TRUE)[[1]]
    while (length(toks_) > 1L && toks_[length(toks_)] %in% suffix_) toks_ <- toks_[-length(toks_)]
    key_ <- paste(toks_, collapse = " ")
    if (nchar(key_) < 5L) NA_character_ else key_
  })
}

tab_keys <- arrow::read_parquet(.path_keys) |>
  dplyr::transmute(
    DocID,
    Fold,
    Class     = .data$ClassDetailed,
    AmendType,
    AnchorKey = norm_company(.data$CompanyName)
  )

cli::cli_h2("Anchor keys")
cli::cli_alert_info(
  "{sum(!is.na(tab_keys$AnchorKey))} of {nrow(tab_keys)} document{?s} carry a usable company key."
)


# 3. Anchor-matched ORG occurrences ----
# The same normalisation is applied to the span, so span and key are comparable forms. The match is
# bidirectional containment, as 04B had it, but the DIRECTION is recorded rather than collapsed.
# That matters here in a way it did not before: the reverse arm accepts a span whose normalised form
# sits inside the key, so a key of "BOEING CAPITAL SERVICES" admits a bare span "SERVICES". Harmless
# when the question is "did we find the filer"; not harmless when the span is being used to locate
# the preamble.

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("ATTACH '", fs::path_abs(.path_store), "' AS s (READ_ONLY)"))

duckdb::duckdb_register(con, "keys_src", as.data.frame(tab_keys), overwrite = TRUE)
DBI::dbExecute(con, "CREATE OR REPLACE TABLE keys AS SELECT * FROM keys_src")
duckdb::duckdb_unregister(con, "keys_src")

DBI::dbExecute(con, paste0(
  "CREATE OR REPLACE TABLE lens AS SELECT DocID, length(TextRaw) AS DocLen ",
  "FROM read_parquet('", fs::path_abs(.path_text), "') WHERE length(TextRaw) > 0"
))

# Punctuation out, whitespace collapsed, leading THE dropped, then one or more trailing corporate
# suffixes removed in a single pass -- ( X)+$ handles "HOLDINGS INC" and "BANK CO LTD" alike.
.sfx <- paste0("( (INC|INCORPORATED|CORP|CORPORATION|LLC|LLP|LP|LTD|LIMITED|PLC|NV|BV|SA|AG|",
               "GMBH|CO|COMPANY|TRUST|NA))+$")
.key <- paste0(
  "regexp_replace(regexp_replace(trim(regexp_replace(regexp_replace(",
  "upper(c.Span), '[^A-Z0-9 ]', ' ', 'g'), '\\s+', ' ', 'g')), '^THE ', ''), '", .sfx, "', '')"
)

DBI::dbExecute(con, paste0(
  "CREATE OR REPLACE TABLE anchor_org AS ",
  "WITH occ AS ( ",
  "  SELECT DISTINCT ",
  "    CASE WHEN c.Engine = c.Model THEN c.Engine ELSE c.Engine || ':' || c.Model END AS Combo, ",
  "    c.DocID, c.Start, c.Stop, c.Span, ", .key, " AS SpanKey ",
  "  FROM s.candidates c WHERE c.Label = 'ORG' AND c.Start IS NOT NULL), ",
  "hit AS ( ",
  "  SELECT o.*, k.Fold, k.Class, k.AmendType, k.AnchorKey, l.DocLen, ",
  "    CASE WHEN o.SpanKey = k.AnchorKey THEN 'exact' ",
  "         WHEN contains(o.SpanKey, k.AnchorKey) THEN 'forward' ",
  "         ELSE 'reverse' END AS MatchKind ",
  "  FROM occ o JOIN keys k USING (DocID) JOIN lens l USING (DocID) ",
  "  WHERE k.AnchorKey IS NOT NULL AND length(o.SpanKey) > 0 ",
  "    AND (contains(o.SpanKey, k.AnchorKey) OR contains(k.AnchorKey, o.SpanKey))) ",
  "SELECT *, ((Start + Stop) / 2.0) / DocLen AS Pos, ",
  "  row_number() OVER (PARTITION BY Combo, DocID ORDER BY Start) AS Rank, ",
  "  COUNT(*) OVER (PARTITION BY Combo, DocID) AS NOcc ",
  "FROM hit"
))

cli::cli_alert_success(
  "{DBI::dbGetQuery(con, 'SELECT COUNT(*) N FROM anchor_org')$N} anchor-matched ORG occurrence(s)."
)


# 4. Block A: does the origin exist at all ----
# Per engine: of the documents carrying a key, in how many did this engine propose a span matching
# it. This is the recall floor, and it is also the coverage ceiling for every rule built on the
# origin. An engine below it here cannot be the one that locates parties, whatever else it wins.

.n_keyed <- sum(!is.na(tab_keys$AnchorKey))

cli::cli_h2("A. Coverage of the origin, by engine")
DBI::dbGetQuery(con, paste0(
  "SELECT Combo, COUNT(DISTINCT DocID) AS DocsFound, COUNT(*) AS Occurrences, ",
  "  median(NOcc) AS MedOccPerDoc, max(NOcc) AS MaxOccPerDoc ",
  "FROM anchor_org GROUP BY Combo ORDER BY DocsFound DESC"
)) |>
  tibble::as_tibble() |>
  dplyr::mutate(PctDocs = round(100 * .data$DocsFound / .n_keyed, 1), .after = DocsFound) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "Denominator is {(.n_keyed)} document{?s} with a usable key. MedOccPerDoc well above 1 is expected \\
   -- the filer's name recurs -- and is why the table below is per OCCURRENCE."
)


# 5. Block B: P1 -- where in the document ----
# Deciles of relative position. Read the first and last columns: a bimodal shape puts mass in
# decile 1 and again in decile 10. A flat row refutes P1 for that engine.

cli::cli_h2("B. P1 -- position of every anchor occurrence, by decile")
DBI::dbGetQuery(con, paste0(
  "SELECT Combo, least(9, CAST(floor(Pos * 10) AS INTEGER)) AS Decile, COUNT(*) AS N ",
  "FROM anchor_org GROUP BY Combo, Decile"
)) |>
  tibble::as_tibble() |>
  dplyr::mutate(Pct = 100 * .data$N / sum(.data$N), .by = Combo) |>
  dplyr::select(Combo, Decile, Pct) |>
  tidyr::pivot_wider(names_from = Decile, values_from = Pct, names_prefix = "D",
                     values_fill = 0) |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("D"), \(.x) round(.x, 1))) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "Row percentages, so each engine is read against itself. P1 predicts D0 and D9 both elevated."
)


# 6. Block C: P2 -- the first occurrence, which is the candidate origin ----
# Only Rank 1 matters for the rule. If its position is tightly concentrated near zero the origin is
# reliable; a wide spread means the first match is often somewhere other than the preamble.

cli::cli_h2("C. P2 -- position of the FIRST anchor occurrence, by engine")
DBI::dbGetQuery(con, paste0(
  "SELECT Combo, COUNT(*) AS Docs, ",
  "  quantile_cont(Pos, 0.10) AS P10, quantile_cont(Pos, 0.25) AS P25, ",
  "  quantile_cont(Pos, 0.50) AS P50, quantile_cont(Pos, 0.75) AS P75, ",
  "  quantile_cont(Pos, 0.90) AS P90, ",
  "  median(Start) AS MedStartChar, ",
  "  SUM(CASE WHEN Start < 3000 THEN 1 ELSE 0 END) AS Under3k ",
  "FROM anchor_org WHERE Rank = 1 GROUP BY Combo ORDER BY P50"
)) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    dplyr::across(c(P10, P25, P50, P75, P90), \(.x) round(.x, 3)),
    PctUnder3k = round(100 * .data$Under3k / .data$Docs, 1)
  ) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "MedStartChar is the absolute offset, which is what a rule would be written in. PctUnder3k is \\
   the share whose first match sits inside the first 3,000 characters."
)


# 7. Block D: is the match trustworthy as an origin ----
# Two hazards. The reverse arm of the containment admits a span shorter than the key, which is how
# a generic token attaches to a specific filer. And a document whose first match differs by engine
# has no single origin at all.

cli::cli_h2("D. What kind of match is doing the work")
DBI::dbGetQuery(con, paste0(
  "SELECT Combo, MatchKind, COUNT(*) AS N, median(length(SpanKey)) AS MedKeyChars, ",
  "  any_value(Span) AS Example ",
  "FROM anchor_org GROUP BY Combo, MatchKind ORDER BY Combo, N DESC"
)) |>
  tibble::as_tibble() |>
  dplyr::mutate(Pct = round(100 * .data$N / sum(.data$N), 1), .by = Combo) |>
  print(n = Inf, width = Inf)

cli::cli_h2("D. Shortest reverse matches -- the ones most likely to be spurious")
DBI::dbGetQuery(con, paste0(
  "SELECT SpanKey, AnchorKey, COUNT(*) AS N, COUNT(DISTINCT DocID) AS Docs ",
  "FROM anchor_org WHERE MatchKind = 'reverse' ",
  "GROUP BY SpanKey, AnchorKey ORDER BY length(SpanKey) ASC, N DESC LIMIT 20"
)) |>
  tibble::as_tibble() |>
  print(n = Inf, width = Inf)

cli::cli_h2("D. Do the engines agree where the origin is?")
DBI::dbGetQuery(con, paste0(
  "WITH f AS (SELECT DocID, Combo, Start FROM anchor_org WHERE Rank = 1) ",
  "SELECT COUNT(DISTINCT DocID) AS Docs, ",
  "  median(Spread) AS MedSpreadChars, ",
  "  SUM(CASE WHEN Spread = 0 THEN 1 ELSE 0 END) AS Identical, ",
  "  SUM(CASE WHEN Spread <= 500 THEN 1 ELSE 0 END) AS Within500 ",
  "FROM (SELECT DocID, max(Start) - min(Start) AS Spread, COUNT(*) AS NEngines ",
  "      FROM f GROUP BY DocID HAVING COUNT(*) > 1)"
)) |>
  tibble::as_tibble() |>
  print(width = Inf)

cli::cli_alert_info(
  "Spread is the gap between the earliest and latest first-match across engines, per document. A \\
   large median means the origin is engine-dependent and has to be tightened before it is used."
)


# 8. Block E: coverage by contract type and amendment status ----
# Coverage is never pooled. Amendments are predicted to be worse -- they are short and incorporate
# the original by reference -- and reporting them together would hide it.

cli::cli_h2("E. Coverage by contract type")
DBI::dbGetQuery(con, paste0(
  "SELECT Class, COUNT(DISTINCT DocID) AS DocsFound, median(Pos) AS MedPosFirst ",
  "FROM anchor_org WHERE Rank = 1 GROUP BY Class"
)) |>
  tibble::as_tibble() |>
  dplyr::left_join(
    tab_keys |>
      dplyr::filter(!is.na(.data$AnchorKey)) |>
      dplyr::count(.data$Class, name = "DocsKeyed"),
    by = dplyr::join_by(Class)
  ) |>
  dplyr::mutate(
    PctFound    = round(100 * .data$DocsFound / .data$DocsKeyed, 1),
    MedPosFirst = round(.data$MedPosFirst, 3)
  ) |>
  dplyr::arrange(dplyr::desc(.data$DocsKeyed)) |>
  print(n = Inf, width = Inf)

cli::cli_h2("E. Coverage by amendment status")
DBI::dbGetQuery(con, paste0(
  "SELECT AmendType, COUNT(DISTINCT DocID) AS DocsFound, median(Pos) AS MedPosFirst, ",
  "  median(DocLen) AS MedDocLen ",
  "FROM anchor_org WHERE Rank = 1 GROUP BY AmendType"
)) |>
  tibble::as_tibble() |>
  dplyr::left_join(
    tab_keys |>
      dplyr::filter(!is.na(.data$AnchorKey)) |>
      dplyr::count(.data$AmendType, name = "DocsKeyed"),
    by = dplyr::join_by(AmendType)
  ) |>
  dplyr::mutate(
    PctFound    = round(100 * .data$DocsFound / .data$DocsKeyed, 1),
    MedPosFirst = round(.data$MedPosFirst, 3),
    MedDocLen   = as.integer(.data$MedDocLen)
  ) |>
  print(n = Inf, width = Inf)


# 9. Block F: read twenty of them ----
# The block that decides whether any of the above means what it appears to. Every table so far is
# consistent with the first match being a preamble party AND with it being a letterhead, a filing
# footer or a defined term. Only the text says which.

.combo_look <- DBI::dbGetQuery(con, paste0(
  "SELECT Combo, COUNT(DISTINCT DocID) AS N FROM anchor_org WHERE Rank = 1 ",
  "GROUP BY Combo ORDER BY N DESC LIMIT 1"
))$Combo

cli::cli_h2("F. First anchor occurrence in context -- engine {(.combo_look)}")

tab_look <- DBI::dbGetQuery(con, paste0(
  "SELECT DocID, Start, Stop, Span, MatchKind, Pos, NOcc FROM anchor_org ",
  "WHERE Rank = 1 AND Combo = '", .combo_look, "'"
)) |>
  tibble::as_tibble() |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n_examples, nrow(.d)))))()

tab_text <- arrow::read_parquet(.path_text) |>
  dplyr::filter(.data$DocID %in% tab_look$DocID)

tab_look |>
  dplyr::left_join(tab_text, by = dplyr::join_by(DocID)) |>
  dplyr::mutate(
    # 0-based half-open offsets into a code-point index; stri_sub is 1-based inclusive.
    Before = stringi::stri_sub(.data$TextRaw, from = pmax(1L, .data$Start + 1L - .ctx),
                               to = .data$Start),
    Hit    = stringi::stri_sub(.data$TextRaw, from = .data$Start + 1L, to = .data$Stop),
    After  = stringi::stri_sub(.data$TextRaw, from = .data$Stop + 1L, to = .data$Stop + .ctx)
  ) |>
  purrr::pwalk(function(DocID, Start, Span, MatchKind, Pos, NOcc, Before, Hit, After, ...) {
    cli::cli_h3("{DocID} | char {Start} | pos {round(Pos, 3)} | {MatchKind} | {NOcc} occurrence{?s}")
    cat(stringi::stri_replace_all_regex(
      paste0("...", Before, " >>>", Hit, "<<< ", After, "..."), "\\s+", " "
    ), "\n\n")
  })


# 10. Write the occurrence table ----
# One probe artifact, in its own directory, so nothing here can be mistaken for a 04A output.

DBI::dbExecute(con, paste0(
  "COPY (SELECT * FROM anchor_org) TO '",
  fs::path_abs(fs::path(.dir_out, "anchor_org.parquet")), "' (FORMAT PARQUET)"
))
DBI::dbDisconnect(con, shutdown = TRUE)

cli::cli_alert_success("Probe table written to 2_output/_Probe/anchor_org.parquet")
