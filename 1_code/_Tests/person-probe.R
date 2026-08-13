# Person names: is the evidence already in the store? ----
#
# THE QUESTION
# 04A ran with .labels_extract set to NULL, which passes no --label filter to the extractors, so
# spaCy emitted everything in its LABEL_MAP -- ORG, PERSON, GPE, DATE, MONEY, PERCENT and AMOUNT.
# The five-label vocabulary in 04A is registered for FIGURES only; nothing filters the store. If
# that reading is right, person names are already extracted and have simply never been looked at.
#
# WHY IT MATTERS NOW
# Roughly half the sample is not a bilateral agreement between organisations. Employment, indemnity
# and award documents name an individual on the other side, and those two classes alone hold 1,425
# of 4,398 documents. A counterparty rule that only reads ORG spans reports the largest part of the
# sample as single-party and looks like a failure when it is correct.
#
# THE ANCHOR THAT MAKES THIS MEASURABLE
# EDGAR records no counterparty, so there is no external fact about the person in the contract --
# except that the exhibit description very often names them. "EMPLOYMENT AGREEMENT - CHARLES R
# BLAND", "CHANGE OF CONTROL AGREEMENT BETWEEN WEBSTER FINANCIAL AND HARRIET MUNRETT WOLFE". DocDesc
# sits outside the body text in the same way the filer's name does, so the same containment test
# applies, and it lands on exactly the classes where the counterparty is a person.
#
# Reads only. Decides nothing.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_04a    <- here::here("2_output", "04A-EntityExtract")
.path_text  <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys  <- fs::path(.dir_04a, "sample_anchors.parquet")
.path_store <- fs::path(.dir_04a, "Store", "EntityCandidates.duckdb")
.path_party <- here::here("2_output", "04B-EntityMeasure", "contracting_party.parquet")

.min_person <- 8L    # shortest normalised person key admitted; a bare first name matches too much
.n_examples <- 15L
.ctx        <- 200L
.seed       <- 42L

stopifnot(fs::file_exists(c(.path_text, .path_keys, .path_store, .path_party)))

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("ATTACH '", fs::path_abs(.path_store), "' AS s (READ_ONLY)"))


# 2. What is actually in the store ----
# The decisive query. If PERSON appears here, nothing needs re-extracting and the rest of this
# script is analysis rather than a proposal.

cli::cli_h2("A. Every label the store holds, by engine")
DBI::dbGetQuery(con, paste0(
  "SELECT CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END AS Combo, ",
  "  Label, COUNT(*) AS N, COUNT(DISTINCT DocID) AS Docs ",
  "FROM s.candidates WHERE Label IS NOT NULL GROUP BY Combo, Label"
)) |>
  tibble::as_tibble() |>
  dplyr::select(Combo, Label, N) |>
  tidyr::pivot_wider(names_from = Label, values_from = N, values_fill = 0L) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "Any column beyond ORG, GPE, DATE, MONEY and REDACT was extracted and never reported. The \\
   five-label list in 04A is a figure vocabulary, not a filter."
)

.has_person <- DBI::dbGetQuery(
  con, "SELECT COUNT(*) AS N FROM s.candidates WHERE Label = 'PERSON'"
)$N > 0

if (!.has_person) {
  cli::cli_alert_danger("No PERSON rows in the store. The rest of this script has nothing to read.")
  DBI::dbDisconnect(con, shutdown = TRUE)
  stop("PERSON absent; re-extraction required.")
}


# 3. Yield and shape ----
# The same two questions asked of organisations in 04A. Persons per document says whether the label
# is usable at all; the positional profile says whether it behaves like a party mention or like an
# incidental one.

cli::cli_h2("B. Person yield per document, by engine")
DBI::dbGetQuery(con, paste0(
  "WITH per AS ( ",
  "  SELECT CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END AS Combo, ",
  "    DocID, COUNT(*) AS N, COUNT(DISTINCT upper(Span)) AS Distinct ",
  "  FROM s.candidates WHERE Label = 'PERSON' GROUP BY Combo, DocID) ",
  "SELECT Combo, COUNT(*) AS Docs, ",
  "  median(N) AS MedPerDoc, quantile_cont(N, 0.9) AS P90PerDoc, ",
  "  median(Distinct) AS MedDistinct, max(N) AS MaxPerDoc ",
  "FROM per GROUP BY Combo ORDER BY Docs DESC"
)) |>
  tibble::as_tibble() |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "MedDistinct is the number that matters. A contract names a handful of people; a figure in the \\
   hundreds means the extractor is returning something other than parties -- signatories, notice \\
   contacts, or capitalised words it has mistaken for names."
)

DBI::dbExecute(con, paste0(
  "CREATE OR REPLACE TABLE lens AS SELECT DocID, length(TextRaw) AS DocLen ",
  "FROM read_parquet('", fs::path_abs(.path_text), "') WHERE length(TextRaw) > 0"
))

cli::cli_h2("B. Where person names sit, by decile of the document")
DBI::dbGetQuery(con, paste0(
  "SELECT CASE WHEN c.Engine = c.Model THEN c.Engine ELSE c.Engine || ':' || c.Model END AS Combo, ",
  "  least(9, CAST(floor((((c.Start + c.Stop) / 2.0) / l.DocLen) * 10) AS INTEGER)) AS Decile, ",
  "  COUNT(*) AS N ",
  "FROM s.candidates c JOIN lens l USING (DocID) ",
  "WHERE c.Label = 'PERSON' AND c.Start IS NOT NULL GROUP BY Combo, Decile"
)) |>
  tibble::as_tibble() |>
  dplyr::mutate(Pct = round(100 * .data$N / sum(.data$N), 1), .by = Combo) |>
  dplyr::select(Combo, Decile, Pct) |>
  tidyr::pivot_wider(names_from = Decile, values_from = Pct, names_prefix = "D", values_fill = 0) |>
  dplyr::select(Combo, dplyr::num_range("D", 0:9)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "Compare against the organisation profile in 04A. A party named in the preamble and again at the \\
   signature block is bimodal; a flat profile is a document's worth of incidental names."
)


# 4. The exhibit description as an anchor ----
# Same containment test as the company anchor, applied to individuals. The description is normalised
# rather than parsed, because a name does not need to be identified in it -- it only needs to be
# looked for. A minimum length keeps a bare first name from matching a description that happens to
# contain those letters.

tab_keys <- arrow::read_parquet(.path_keys) |>
  dplyr::transmute(
    DocID,
    Class    = .data$ClassDetailed,
    DescNorm = paste(.data$DocDesc, .data$DocName) |>
      stringi::stri_trans_toupper() |>
      stringi::stri_replace_all_regex("[^A-Z ]", " ") |>
      stringi::stri_replace_all_regex("\\s+", " ") |>
      stringi::stri_trim_both()
  )

duckdb::duckdb_register(con, "keys_src", as.data.frame(tab_keys), overwrite = TRUE)
DBI::dbExecute(con, "CREATE OR REPLACE TABLE keys AS SELECT * FROM keys_src")
duckdb::duckdb_unregister(con, "keys_src")

DBI::dbExecute(con, paste0(
  "CREATE OR REPLACE TABLE person_hit AS ",
  "WITH p AS ( ",
  "  SELECT DISTINCT ",
  "    CASE WHEN c.Engine = c.Model THEN c.Engine ELSE c.Engine || ':' || c.Model END AS Combo, ",
  "    c.DocID, c.Start, c.Stop, c.Span, ",
  "    trim(regexp_replace(regexp_replace(upper(c.Span), '[^A-Z ]', ' ', 'g'), ",
  "      '\\s+', ' ', 'g')) AS SpanNorm ",
  "  FROM s.candidates c WHERE c.Label = 'PERSON' AND c.Start IS NOT NULL) ",
  "SELECT p.*, k.Class, l.DocLen, ",
  "  length(p.SpanNorm) AS SpanLen, ",
  "  CASE WHEN length(p.SpanNorm) >= ", as.integer(.min_person),
  "    AND contains(p.SpanNorm, ' ') AND contains(k.DescNorm, p.SpanNorm) ",
  "    THEN TRUE ELSE FALSE END AS InDesc ",
  "FROM p JOIN keys k USING (DocID) JOIN lens l USING (DocID)"
))

cli::cli_h2("C. Do person spans appear in the exhibit description?")
DBI::dbGetQuery(con, paste0(
  "SELECT Combo, COUNT(DISTINCT DocID) AS Docs, ",
  "  COUNT(DISTINCT CASE WHEN InDesc THEN DocID END) AS DocsHit, ",
  "  SUM(CASE WHEN InDesc THEN 1 ELSE 0 END) AS SpansHit, COUNT(*) AS Spans ",
  "FROM person_hit GROUP BY Combo ORDER BY DocsHit DESC"
)) |>
  tibble::as_tibble() |>
  dplyr::mutate(PctDocs = round(100 * .data$DocsHit / .data$Docs, 1), .after = DocsHit) |>
  print(n = Inf, width = Inf)

cli::cli_h2("C. The same, by contract type")
DBI::dbGetQuery(con, paste0(
  "SELECT Class, COUNT(DISTINCT DocID) AS Docs, ",
  "  COUNT(DISTINCT CASE WHEN InDesc THEN DocID END) AS DocsHit ",
  "FROM person_hit GROUP BY Class ORDER BY Docs DESC"
)) |>
  tibble::as_tibble() |>
  dplyr::mutate(PctDocs = round(100 * .data$DocsHit / .data$Docs, 1)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "This is a RECALL FLOOR on individuals and nothing more, exactly as the company anchor is for \\
   organisations. A description that names nobody is not evidence the extractor was wrong; a \\
   description that names someone the extractor never proposed is."
)


# 5. Persons beside the contracting party ----
# The question the counterparty rule turns on: when an individual is the other side, is that
# individual named near the located party.

tab_party <- arrow::read_parquet(.path_party) |>
  dplyr::filter(.data$Status == "located") |>
  dplyr::select(DocID, Class, PartyAt = Start, Party)

duckdb::duckdb_register(con, "party_src", as.data.frame(tab_party), overwrite = TRUE)
DBI::dbExecute(con, "CREATE OR REPLACE TABLE party AS SELECT * FROM party_src")
duckdb::duckdb_unregister(con, "party_src")

cli::cli_h2("D. Distance from the contracting party to the nearest person name")
DBI::dbGetQuery(con, paste0(
  "WITH near AS ( ",
  "  SELECT p.DocID, p.Class, min(abs(h.Start - p.PartyAt)) AS Gap ",
  "  FROM party p JOIN person_hit h USING (DocID) ",
  "  WHERE h.SpanLen >= ", as.integer(.min_person), " AND contains(h.SpanNorm, ' ') ",
  "  GROUP BY p.DocID, p.Class) ",
  "SELECT Class, COUNT(*) AS Docs, median(Gap) AS MedGap, ",
  "  SUM(CASE WHEN Gap <= 300 THEN 1 ELSE 0 END) AS Within300, ",
  "  SUM(CASE WHEN Gap <= 1000 THEN 1 ELSE 0 END) AS Within1000 ",
  "FROM near GROUP BY Class ORDER BY Docs DESC"
)) |>
  tibble::as_tibble() |>
  dplyr::mutate(PctWithin300 = round(100 * .data$Within300 / .data$Docs, 1)) |>
  print(n = Inf, width = Inf)


# 6. Read them ----

.combo_look <- DBI::dbGetQuery(con, paste0(
  "SELECT Combo, COUNT(DISTINCT DocID) AS N FROM person_hit WHERE InDesc ",
  "GROUP BY Combo ORDER BY N DESC LIMIT 1"
))$Combo

cli::cli_h2("E. Person spans that the exhibit description confirms -- engine {(.combo_look)}")

tab_look <- DBI::dbGetQuery(con, paste0(
  "SELECT h.DocID, h.Start, h.Stop, h.Span, h.Class, p.Party ",
  "FROM person_hit h JOIN party p USING (DocID) ",
  "WHERE h.InDesc AND h.Combo = '", .combo_look, "'"
)) |>
  tibble::as_tibble() |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n_examples, nrow(.d)))))()

tab_look |>
  dplyr::left_join(
    arrow::read_parquet(.path_text) |> dplyr::filter(.data$DocID %in% tab_look$DocID),
    by = dplyr::join_by(DocID)
  ) |>
  dplyr::mutate(
    Snippet = stringi::stri_replace_all_regex(
      paste0(
        "...",
        stringi::stri_sub(.data$TextRaw, from = pmax(1L, .data$Start + 1L - .ctx), to = .data$Start),
        " >>>", stringi::stri_sub(.data$TextRaw, from = .data$Start + 1L, to = .data$Stop), "<<< ",
        stringi::stri_sub(.data$TextRaw, from = .data$Stop + 1L, to = .data$Stop + .ctx), "..."
      ),
      "\\s+", " "
    )
  ) |>
  purrr::pwalk(function(Party, Span, Class, Start, Snippet, ...) {
    cli::cli_h3("{Party} | {Class}")
    cat("  person : ", Span, " at char ", Start, "\n", sep = "")
    cat("  context: ", Snippet, "\n\n", sep = "")
  })

cli::cli_alert_info(
  "Read for whether the individual is a PARTY or merely a signatory. An officer signing on behalf \\
   of the company is not the counterparty, and the two look alike until the surrounding words are \\
   read."
)

DBI::dbDisconnect(con, shutdown = TRUE)
