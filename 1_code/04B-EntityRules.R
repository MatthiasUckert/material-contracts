# 04B-EntityRules: the rules that resolve parties, one entity at a time ----------------------------------------------
#
# WHAT THIS FILE DOES
# 04A extracted nine engines over 4,398 hand-classified contracts and ranked none of them, because
# ranking needs labels and there are no entity labels. This file writes the rules. It takes one
# entity at a time, states the rule in the smallest form that could work, applies it, and reports
# what it caught and what it did not. ORG is the first, because every other party quantity is
# measured from where an organisation was found.
#
# THE ONE EXTERNAL FACT
# EDGAR records the registrant's own name, independently of the contract text. That is one side of
# one party, and it is the whole evidential basis: it can establish that an engine found A party,
# never that a span IS a party. Everything below is therefore a recall floor with its failure modes
# reported, not a precision claim.
#
# WHY A WINDOW AT ALL
# LexNLP's organisations are bimodal -- 22.6% of spans in the first tenth of a document, 25.2% in
# the last, and 4.1% in the trough. The middle is body prose naming every company a contract
# mentions; the ends are the preamble and the signature block. A window is the cheapest instrument
# that separates them, and it costs nothing to change later because every candidate already carries
# its offsets.
#
# THE WINDOW IS max(), NOT min()
# An absolute head admits 93.3% of parties overall and 85.4% of the longest length decile. A
# relative head admits 89.7% overall and 58.2% of the SHORTEST decile, because a tenth of a
# 2,473-character amendment is 247 characters and the party's offset runs past that. max() takes
# whichever arm is larger, so the floor binds on short documents and the share binds on long ones.
# min() caps the long documents and scores worse than either arm alone.
#
# NOISE IS EXPECTED AT THIS STAGE. The counts below are deliberately unfiltered: no stoplist, no
# defined-term suppression, no boundary repair beyond one leading connective. What the noise looks
# like is the evidence the next rule is written against.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_anchors <- .lP$Input$Anchors
  .path_text    <- .lP$Input$Text
  .db_path      <- .lP$Input$Store
}


# 1. Vocabulary ------------------------------------------------------------------------------------------------------
# Registered at SOURCE time rather than inside a function, so every figure in this document and any
# later one orders these axes identically. The status order runs best to worst, which is what makes
# a stacked bar readable without consulting the legend.

plot_register_levels(
  .key    = "PartyStatus",
  .levels = c("located", "late", "fragment only", "no match", "no key"),
  .short  = c("located", "late", "fragment", "no match", "no key")
)

plot_register_levels(
  .key    = "Region",
  .levels = c("head", "middle", "tail"),
  .short  = c("head", "middle", "tail")
)


# 2. One reduction, both sides ---------------------------------------------------------------------------------------
# EDGAR writes a name in registration form and a contract writes it in prose. Neither is canonical,
# so both are reduced to a common key before they are compared. The reduction runs twice -- once in
# R over a few thousand company names, once in SQL over millions of spans -- and the two MUST agree,
# which is why they are written beside each other and checked in Validation rather than assumed.
#
# The steps, in order: uppercase, punctuation to space, whitespace collapsed, a leading connective
# dropped, trailing corporate suffixes stripped repeatedly. "The Boeing Company" -> "BOEING";
# "between Precision BioSciences, Inc." -> "PRECISION BIOSCIENCES".

.ent_suffix <- c("INC", "INCORPORATED", "CORP", "CORPORATION", "LLC", "LLP", "LP", "LTD", "LIMITED",
                 "PLC", "NV", "BV", "SA", "AG", "GMBH", "CO", "COMPANY", "TRUST", "NA")

.ent_lead <- c("MADE BY AND BETWEEN", "BY AND BETWEEN", "BY AND AMONG", "AMONGST", "BETWEEN",
               "AMONG", "AND", "WITH", "THIS", "THE", "DATED", "AS OF")

#' Reduce a company name or a candidate span to a comparable key
#'
#' The corporate suffix is the problem this solves. EDGAR records "BOEING CO" where the contract
#' writes "The Boeing Company", and a comparison keeping the suffix misses every such pair while a
#' comparison ignoring case alone still fails on the punctuation.
#'
#' Suffixes are removed repeatedly rather than once, because "BANK CO LTD" carries three. The
#' leading connective is removed because the extractors do not stop cleanly at a name: they return
#' "between Precision BioSciences, Inc." and "AND TEEKAY LNG PARTNERS L.P.", where the location is
#' right and only the boundary is wrong.
#'
#' @param .x Character vector of names or spans as recorded.
#' @param .min Integer. Keys shorter than this become NA. Five for anchor keys, because a
#'   three-character key matches a large share of the corpus by containment and a key that matches
#'   everything locates nothing. Zero for span keys, where the reverse-match floor does that job.
#' @return Character vector of keys, NA below .min characters.
ent_norm_key <- function(.x, .min = 5L) {
  if (FALSE) {
    .x   <- c("ACME HOLDINGS, INC.", "The Boeing Company", "between Precision BioSciences, Inc.")
    .min <- 5L
  }

  lead_ <- paste0("^(", paste(.ent_lead, collapse = "|"), ") ")

  out_ <- .x |>
    stringi::stri_trans_toupper() |>
    stringi::stri_replace_all_regex("[^A-Z0-9 ]", " ") |>
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_trim_both() |>
    stringi::stri_replace_first_regex(lead_, "")

  purrr::map_chr(out_, function(.s) {
    if (is.na(.s)) return(NA_character_)
    toks_ <- strsplit(.s, " ", fixed = TRUE)[[1]]
    while (length(toks_) > 1L && toks_[length(toks_)] %in% .ent_suffix) toks_ <- toks_[-length(toks_)]
    key_ <- paste(toks_, collapse = " ")
    if (nchar(key_) < .min) NA_character_ else key_
  })
}


#' The same reduction, as a SQL expression
#'
#' Written out rather than applied through a registered R callback because it runs over millions of
#' candidate spans, and an R function turns every scan into a single-threaded round trip into the R
#' session. The suffix strip uses "( X)+$" so repeated suffixes go in one pass, which the R version
#' needs a loop for.
#'
#' Built in named stages rather than as one nested expression. The nested form was six calls deep
#' and unreadable, which is how the two implementations drifted apart in the first place.
#'
#' @param .col Character. SQL expression yielding the raw span.
#' @return A SQL expression string producing the key.
ent_sql_key <- function(.col = "Span") {
  if (FALSE) .col <- "c.Span"

  lead_ <- paste0("^(", paste(.ent_lead, collapse = "|"), ") ")
  sfx_  <- paste0("( (", paste(.ent_suffix, collapse = "|"), "))+$")

  x_ <- paste0("upper(", .col, ")")
  x_ <- paste0("regexp_replace(", x_, ", '[^A-Z0-9 ]', ' ', 'g')")
  x_ <- paste0("regexp_replace(", x_, ", '\\s+', ' ', 'g')")
  x_ <- paste0("trim(", x_, ")")
  x_ <- paste0("regexp_replace(", x_, ", '", lead_, "', '')")
  x_ <- paste0("regexp_replace(", x_, ", '", sfx_, "', '')")
  paste0("trim(", x_, ")")
}


#' Per-document anchor keys, one row per contract
#'
#' Reads 04A's sample table rather than reaching back to the EDGAR metadata, so this document
#' depends on one narrow artifact and the sample it describes cannot drift from the sample 04A
#' extracted.
#'
#' @param .path_anchors 04A's sample_anchors.parquet.
#' @return Tibble: DocID, Fold, Class, AmendType, CIK, CompanyName, DateFiled, AnchorKey.
ent_anchor_keys <- function(.path_anchors) {
  if (FALSE) .path_anchors <- .lP$Input$Anchors

  arrow::read_parquet(.path_anchors) |>
    dplyr::transmute(
      DocID,
      Fold,
      Class     = .data$ClassDetailed,
      AmendType,
      CIK,
      CompanyName,
      DateFiled = suppressWarnings(anytime::anydate(as.character(.data$DateFiled))),
      AnchorKey = ent_norm_key(.x = .data$CompanyName, .min = 5L)
    )
}


# 3. The session -----------------------------------------------------------------------------------------------------
# In memory, with 04A's store attached read-only. Temporary tables can then be built freely with no
# possibility of writing to an artifact this document does not own.

#' Copy an R table into DuckDB as a real table rather than leaving it registered
#'
#' A registered data frame is a view backed by the R runtime: every scan crosses back into R,
#' single-threaded, and a join against a multi-million-row table drags the whole join down that
#' path. Materialising costs one copy of a few thousand rows and removes R from the query plan.
#'
#' @param .con Live connection.
#' @param .name Table name to create.
#' @param .tab Tibble or data frame to copy.
#' @return Invisibly .name.
ent_put_table <- function(.con, .name, .tab) {
  if (FALSE) {
    .con  <- con
    .name <- "keys"
    .tab  <- tab_keys
  }
  tmp_ <- paste0(.name, "_src")
  duckdb::duckdb_register(.con, tmp_, as.data.frame(.tab), overwrite = TRUE)
  DBI::dbExecute(.con, paste0("CREATE OR REPLACE TABLE ", .name, " AS SELECT * FROM ", tmp_))
  duckdb::duckdb_unregister(.con, tmp_)
  invisible(.name)
}


#' Open a working session over 04A's candidate store
#'
#' Builds the two tables every query below joins on: the anchor keys, and the document lengths that
#' turn a character offset into a relative position.
#'
#' @param .db_path 04A's candidate store.
#' @param .keys Tibble from ent_anchor_keys().
#' @param .path_text 04A's canonical text parquet.
#' @return A live DBI connection; the caller disconnects.
ent_session <- function(.db_path, .keys, .path_text) {
  if (FALSE) {
    .db_path   <- .lP$Input$Store
    .keys      <- tab_keys
    .path_text <- .lP$Input$Text
  }
  con_ <- ner_db_connect()
  DBI::dbExecute(con_, paste0("ATTACH '", as.character(fs::path_abs(.db_path)), "' AS s (READ_ONLY)"))
  ent_put_table(
    .con  = con_,
    .name = "keys",
    .tab  = dplyr::mutate(.keys, DateFiled = as.character(.data$DateFiled))
  )
  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TABLE lens AS SELECT DocID, length(TextRaw) AS DocLen FROM read_parquet('",
    as.character(fs::path_abs(.path_text)), "') WHERE length(TextRaw) > 0"
  ))
  con_
}


# 4. The candidate set -----------------------------------------------------------------------------------------------
# One table per role, and the roles are genuinely different. The CANDIDATE set is what a rule will
# read, so it wants the selective engine. The ANCHOR set is what establishes whether the filer was
# found at all, which is a recall question, so it reads every engine that emits the label: an engine
# reaching the filer two per cent more often is worth more there than one that is cleaner.

#' Materialise one label's spans for a chosen set of engines
#'
#' DISTINCT on the offsets and the span, because several engines return the identical span at the
#' identical offsets and the combination is carried so corroboration can be counted later.
#'
#' @param .con Session from ent_session().
#' @param .name Table name to create in the session.
#' @param .label Character. The store label, e.g. "ORG".
#' @param .combos Character vector of combination tokens, or NULL for every engine emitting .label.
#' @param .quiet Logical. Suppress the count message.
#' @return Invisibly, the number of rows written.
ent_load_label <- function(.con, .name, .label, .combos = NULL, .quiet = FALSE) {
  if (FALSE) {
    .con    <- con
    .name   <- "org"
    .label  <- "ORG"
    .combos <- "lexnlp"
    .quiet  <- FALSE
  }

  combo_ <- "CASE WHEN c.Engine = c.Model THEN c.Engine ELSE c.Engine || ':' || c.Model END"
  where_ <- paste0("c.Label = '", .label, "' AND c.Start IS NOT NULL")
  if (!is.null(.combos)) {
    where_ <- paste0(where_, " AND ", combo_, " IN ('", paste(.combos, collapse = "', '"), "')")
  }

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE ", .name, " AS ",
    "SELECT DISTINCT ", combo_, " AS Combo, c.DocID, c.Start, c.Stop, c.Span, ",
    ent_sql_key(.col = "c.Span"), " AS SpanKey, l.DocLen ",
    "FROM s.candidates c JOIN lens l USING (DocID) WHERE ", where_
  ))

  n_ <- DBI::dbGetQuery(.con, paste0("SELECT COUNT(*) AS N FROM ", .name))$N
  if (!.quiet) {
    cli::cli_alert_success("{(.name)}: {format(n_, big.mark = ',')} {(.label)} span{?s} loaded.")
  }
  invisible(n_)
}


# 5. Windows ---------------------------------------------------------------------------------------------------------
# The head is where a preamble is; the tail is where a signature block is; the middle is body prose.
# Nothing here uses the anchor, so nothing here can be circular: this is a description of the
# engine, and it would read the same if EDGAR recorded nothing at all.

#' Head and tail boundaries for one parameterisation
#'
#' Head runs from character zero to max(.floor, .share * DocLen); tail runs from
#' DocLen - .share_tail * DocLen to the end. The max() is the whole point: an absolute bound fails
#' the longest documents and a relative bound fails the shortest, and each arm covers the end the
#' other breaks on.
#'
#' On a document short enough for the floor to reach past the tail boundary the two regions overlap.
#' Head wins, and the overlap count is reported rather than silently resolved.
#'
#' @param .con Session with the candidate table built.
#' @param .table Character. Table name to read.
#' @param .head Numeric. Head share of document length.
#' @param .floor Integer. Minimum head width in characters.
#' @param .tail Numeric. Tail share of document length.
#' @return Tibble: one row per span, with Region assigned.
ent_assign_region <- function(.con, .table = "org", .head = 0.10, .floor = 3000L, .tail = 0.20) {
  if (FALSE) {
    .con   <- con
    .table <- "org"
    .head  <- 0.10
    .floor <- 3000L
    .tail  <- 0.20
  }

  DBI::dbGetQuery(.con, paste0(
    "SELECT Combo, DocID, Start, Stop, Span, SpanKey, DocLen, ",
    "  greatest(", as.integer(.floor), ", ", .head, " * DocLen) AS HeadEnd, ",
    "  DocLen - ", .tail, " * DocLen AS TailStart, ",
    "  CASE WHEN Start < greatest(", as.integer(.floor), ", ", .head, " * DocLen) THEN 'head' ",
    "       WHEN Start >= DocLen - ", .tail, " * DocLen THEN 'tail' ",
    "       ELSE 'middle' END AS Region ",
    "FROM ", .table
  )) |>
    tibble::as_tibble()
}


#' Sweep a grid of head and tail cutoffs
#'
#' Two numbers per cell and they answer different questions. PctSpans is where the mass is; PctDocs
#' is whether the region is populated in most contracts or crowded in a few. An engine with
#' MaxPerDoc in the thousands can put a quarter of its spans in the head while reaching only half
#' the documents, and only the second number would show it.
#'
#' MedKeys is the cost side: distinct normalised names admitted per document. A rule reading the
#' region has to survive that many candidates.
#'
#' @param .con Session with the candidate table built.
#' @param .table Character. Table name to read.
#' @param .grid Tibble: Side ("head" or "tail"), Share, Floor.
#' @return Tibble: one row per grid cell, with PctSpans, PctDocs, MedKeys, P90Keys.
ent_window_grid <- function(.con, .table = "org", .grid = NULL) {
  if (FALSE) {
    .con   <- con
    .table <- "org"
    .grid  <- tab_grid
  }

  ent_put_table(.con = .con, .name = "grid_win", .tab = .grid)

  DBI::dbGetQuery(.con, paste0(
    "WITH j AS ( ",
    "  SELECT g.Side, g.Share, g.Floor, o.DocID, o.SpanKey, ",
    "    CASE WHEN g.Side = 'head' ",
    "         THEN o.Start <  greatest(g.Floor, g.Share * o.DocLen) ",
    "         ELSE o.Start >= o.DocLen - greatest(g.Floor, g.Share * o.DocLen) ",
    "    END AS InRegion ",
    "  FROM ", .table, " o CROSS JOIN grid_win g), ",
    "perdoc AS ( ",
    "  SELECT Side, Share, Floor, DocID, COUNT(*) AS NAll, ",
    "    SUM(CASE WHEN InRegion THEN 1 ELSE 0 END) AS NIn, ",
    "    COUNT(DISTINCT CASE WHEN InRegion THEN SpanKey END) AS NKeys ",
    "  FROM j GROUP BY Side, Share, Floor, DocID) ",
    "SELECT Side, Share, Floor, SUM(NAll) AS SpansAll, SUM(NIn) AS SpansIn, ",
    "  COUNT(*) AS DocsAll, SUM(CASE WHEN NIn > 0 THEN 1 ELSE 0 END) AS DocsIn, ",
    "  median(NKeys) AS MedKeys, quantile_cont(NKeys, 0.9) AS P90Keys ",
    "FROM perdoc GROUP BY Side, Share, Floor ORDER BY Side DESC, Share, Floor"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      PctSpans = .data$SpansIn / .data$SpansAll,
      PctDocs  = .data$DocsIn / .data$DocsAll,
      MedKeys  = as.integer(.data$MedKeys),
      P90Keys  = as.integer(.data$P90Keys)
    )
}


#' Span density across the document, pooled and per document
#'
#' The two weightings answer the same question differently and the gap between them is a diagnostic.
#' Pooled shares give a document with three thousand spans three thousand votes; per-document shares
#' give it one. Where the two agree the bimodality is a property of contracts, and where they
#' diverge it is a property of a handful of table-heavy filings.
#'
#' @param .con Session with the candidate table built.
#' @param .table Character. Table name to read.
#' @param .bins Integer. Bins across relative position.
#' @return Tibble: Bin, Pos, Weight ("pooled" or "per document"), Share.
ent_density <- function(.con, .table = "org", .bins = 50L) {
  if (FALSE) {
    .con   <- con
    .table <- "org"
    .bins  <- 50L
  }

  DBI::dbGetQuery(.con, paste0(
    "WITH b AS ( ",
    "  SELECT DocID, least(", as.integer(.bins) - 1L, ", ",
    "    CAST(floor((Start * 1.0 / DocLen) * ", as.integer(.bins), ") AS INTEGER)) AS Bin ",
    "  FROM ", .table, " WHERE DocLen > 0), ",
    "perdoc AS (SELECT DocID, Bin, COUNT(*) AS N FROM b GROUP BY DocID, Bin), ",
    "tot AS (SELECT DocID, SUM(N) AS NDoc FROM perdoc GROUP BY DocID) ",
    "SELECT p.Bin, SUM(p.N) AS NPooled, SUM(p.N * 1.0 / t.NDoc) AS NWeighted ",
    "FROM perdoc p JOIN tot t USING (DocID) GROUP BY p.Bin ORDER BY p.Bin"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      Pos    = (.data$Bin + 0.5) / .bins,
      Pooled = .data$NPooled / sum(.data$NPooled),
      PerDoc = .data$NWeighted / sum(.data$NWeighted)
    ) |>
    tidyr::pivot_longer(
      cols      = c("Pooled", "PerDoc"),
      names_to  = "Weight",
      values_to = "Share"
    ) |>
    dplyr::mutate(
      Weight = factor(.data$Weight, levels = c("Pooled", "PerDoc"),
                      labels = c("pooled over spans", "averaged over documents"))
    ) |>
    dplyr::select(Bin, Pos, Weight, Share)
}


# 6. The anchor ------------------------------------------------------------------------------------------------------
# The filer's own name, matched against the spans. Directional, because the two directions do not
# carry the same evidence: a forward match means the span holds the key plus something else, and a
# reverse match means the span is a FRAGMENT of the recorded name and may be a single character.
# Collapsing them is what let an unguarded reverse arm inflate engine recall.

#' Every span matching the filer's own name
#'
#' Reads the ANCHOR table, which carries every engine, because locating the filer is a recall
#' problem. Usable is recorded rather than filtered so the cost of the reverse floor stays visible.
#'
#' @param .con Session with the anchor candidate table built.
#' @param .table Character. Table name to read.
#' @param .min_reverse Integer. Shortest fragment admitted as a reverse match. The unguarded arm
#'   matched the single character "G" against GEORGIA PACIFIC seventy-three times in one document.
#' @param .quiet Logical. Suppress the count message.
#' @return Invisibly, the number of anchor-matched rows written to the session table anchor_org.
ent_anchor_match <- function(.con, .table = "org_all", .min_reverse = 10L, .quiet = FALSE) {
  if (FALSE) {
    .con         <- con
    .table       <- "org_all"
    .min_reverse <- 10L
    .quiet       <- FALSE
  }

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE anchor_org AS ",
    "SELECT o.Combo, o.DocID, o.Start, o.Stop, o.Span, o.SpanKey, o.DocLen, ",
    "  length(o.SpanKey) AS KeyLen, ",
    "  CASE WHEN o.SpanKey = k.AnchorKey THEN 'exact' ",
    "       WHEN contains(o.SpanKey, k.AnchorKey) THEN 'forward' ",
    "       ELSE 'reverse' END AS MatchKind, ",
    "  CASE WHEN o.SpanKey = k.AnchorKey OR contains(o.SpanKey, k.AnchorKey) ",
    "       OR length(o.SpanKey) >= ", as.integer(.min_reverse), " THEN TRUE ELSE FALSE END AS Usable ",
    "FROM ", .table, " o JOIN keys k USING (DocID) ",
    "WHERE k.AnchorKey IS NOT NULL AND length(o.SpanKey) > 0 ",
    "  AND (contains(o.SpanKey, k.AnchorKey) OR contains(k.AnchorKey, o.SpanKey))"
  ))

  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM anchor_org WHERE Usable")$N
  if (!.quiet) {
    cli::cli_alert_success("{format(n_, big.mark = ',')} usable anchor-matched span{?s}.")
  }
  invisible(n_)
}


#' Anchor recall at every head cutoff in the grid
#'
#' THE COLUMN THAT DECIDES THE WINDOW. Every other number in this document describes the engine;
#' this one says whether a rule reading the head would find the party. The denominator is documents
#' carrying a usable key, and the share of the whole sample is reported beside it so the cost of the
#' key floor stays visible.
#'
#' @param .con Session with anchor_org built.
#' @param .keys Tibble from ent_anchor_keys().
#' @param .grid Tibble: Side, Share, Floor. Only head rows are used.
#' @return Tibble: Share, Floor, DocsIn, PctOfKeyed, PctOfSample.
ent_anchor_grid <- function(.con, .keys, .grid) {
  if (FALSE) {
    .con  <- con
    .keys <- tab_keys
    .grid <- tab_grid
  }

  n_keyed_  <- sum(!is.na(.keys$AnchorKey))
  n_sample_ <- nrow(.keys)

  ent_put_table(.con = .con, .name = "grid_head", .tab = dplyr::filter(.grid, .data$Side == "head"))

  DBI::dbGetQuery(.con, paste0(
    "SELECT g.Share, g.Floor, ",
    "  COUNT(DISTINCT CASE WHEN a.Start < greatest(g.Floor, g.Share * a.DocLen) ",
    "                      THEN a.DocID END) AS DocsIn ",
    "FROM anchor_org a CROSS JOIN grid_head g WHERE a.Usable ",
    "GROUP BY g.Share, g.Floor ORDER BY g.Share, g.Floor"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      PctOfKeyed  = .data$DocsIn / n_keyed_,
      PctOfSample = .data$DocsIn / n_sample_
    )
}


#' One row per document: which span is the contracting party, and how far it can be trusted
#'
#' RUNS FROM THE KEY SIDE, not from the matches. A document where no engine proposed the filer's
#' name is absent from anchor_org entirely, so a join in the other direction would drop it and the
#' coverage figure would be computed over the documents that worked.
#'
#' Candidate ordering is position first and match quality second. Quality-first prefers an exact
#' match at character 8,000 over a forward match at character 200, which is backwards for locating a
#' preamble; measured on this sample it moves roughly a hundred documents into a signature block.
#'
#' @param .con Session with anchor_org built.
#' @param .keys Tibble from ent_anchor_keys().
#' @param .head Numeric. Head share of document length.
#' @param .floor Integer. Minimum head width in characters.
#' @return Tibble: one row per document, with Status, the chosen span and its offsets.
ent_locate_party <- function(.con, .keys, .head = 0.10, .floor = 3000L) {
  if (FALSE) {
    .con   <- con
    .keys  <- tab_keys
    .head  <- 0.10
    .floor <- 3000L
  }

  # RUNS FROM lens, NOT FROM THE MATCHES. A document no engine proposed the filer's name for is
  # absent from anchor_org entirely, so a query rooted there would carry NULL DocLen and drop out of
  # every length-based diagnostic below while still appearing in the status table -- which is how a
  # coverage figure comes to be computed over the documents that worked.
  found_ <- DBI::dbGetQuery(.con, paste0(
    "WITH usable AS ( ",
    "  SELECT DISTINCT DocID, Start, Stop, Span, SpanKey, KeyLen, MatchKind ",
    "  FROM anchor_org WHERE Usable), ",
    "best AS ( ",
    "  SELECT * FROM usable QUALIFY row_number() OVER (PARTITION BY DocID ORDER BY Start, ",
    "    CASE MatchKind WHEN 'exact' THEN 1 WHEN 'forward' THEN 2 ELSE 3 END) = 1), ",
    "tot AS (SELECT DocID, COUNT(*) AS NMatch FROM anchor_org GROUP BY DocID), ",
    "agree AS ( ",
    "  SELECT b.DocID, COUNT(DISTINCT a.Combo) AS NEngine FROM best b JOIN anchor_org a ",
    "    ON a.DocID = b.DocID AND a.Start = b.Start AND a.Stop = b.Stop GROUP BY b.DocID) ",
    "SELECT l.DocID, l.DocLen, ",
    "  greatest(", as.integer(.floor), ", ", .head, " * l.DocLen) AS HeadEnd, ",
    "  coalesce(t.NMatch, 0) AS NMatch, ",
    "  b.Start, b.Stop, b.Span, b.SpanKey, b.KeyLen, b.MatchKind, g.NEngine ",
    "FROM lens l LEFT JOIN tot t USING (DocID) LEFT JOIN best b USING (DocID) ",
    "  LEFT JOIN agree g USING (DocID)"
  )) |>
    tibble::as_tibble()

  # A span whose normalised form begins with a connective is a correct LOCATION with a ragged
  # boundary, which is a different failure from a wrong location and is counted apart rather than
  # pooled with it.
  lead_ <- paste0("^(", paste(.ent_lead, collapse = "|"), ") ")

  .keys |>
    dplyr::left_join(found_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      NMatch = as.integer(dplyr::coalesce(.data$NMatch, 0L)),
      Status = dplyr::case_when(
        is.na(.data$AnchorKey)                            ~ "no key",
        !is.na(.data$Start) & .data$Start < .data$HeadEnd ~ "located",
        !is.na(.data$Start)                               ~ "late",
        .data$NMatch > 0L                                 ~ "fragment only",
        TRUE                                              ~ "no match"
      ),
      Party  = stringi::stri_trim_both(
        stringi::stri_replace_all_regex(dplyr::coalesce(.data$Span, ""), "\\s+", " ")
      ),
      Leaked = !is.na(.data$Span) & stringi::stri_detect_regex(
        stringi::stri_trim_both(stringi::stri_replace_all_regex(
          stringi::stri_replace_all_regex(
            stringi::stri_trans_toupper(dplyr::coalesce(.data$Span, "")), "[^A-Z0-9 ]", " "
          ), "\\s+", " "
        )),
        lead_
      )
    ) |>
    dplyr::relocate(Status, Party, .after = DocID)
}


# 7. Entities in the head --------------------------------------------------------------------------------------------
# The deduplication step, and the reason it exists is visible in 04A's read examples: LexNLP returns
# "Icosavax, Inc.", "Icosavax Inc" and "Icosavax, Inc" as three distinct spans in one contract. They
# are one company. The key built in section 2 collapses them at no cost, because it is the same
# reduction the anchor already uses.

#' Distinct organisations inside the head, one row per document per name
#'
#' The earliest occurrence is kept, because position is what the rules downstream are measured in.
#' NOcc is retained rather than discarded: a name appearing three times in a preamble region is
#' behaving differently from one appearing once, and that will matter when counterparties are
#' separated from incidental mentions.
#'
#' No stoplist and no defined-term suppression. The noise is the evidence for the next rule.
#'
#' @param .con Session with the candidate table built.
#' @param .table Character. Table name to read.
#' @param .head Numeric. Head share of document length.
#' @param .floor Integer. Minimum head width in characters.
#' @param .min_reverse Integer. Shortest fragment admitted when flagging a name as the anchor. The
#'   same floor the anchor match uses, because an unfloored reverse arm would mark a single
#'   character as the filer and the counterparty set is defined by exclusion from this flag.
#' @return Tibble: DocID, SpanKey, Span, Start, Stop, NOcc, IsAnchor.
ent_head_entities <- function(.con, .table = "org", .head = 0.10, .floor = 3000L,
                              .min_reverse = 10L) {
  if (FALSE) {
    .con         <- con
    .table       <- "org"
    .head        <- 0.10
    .floor       <- 3000L
    .min_reverse <- 10L
  }

  # Where several spans share a normalised name and a start offset, the LONGEST is kept. Both are
  # the same entity in the same place; the longer one carries more of the name, and a name short by
  # its suffix is the more common failure than one carrying a neighbour.
  DBI::dbGetQuery(.con, paste0(
    "WITH inhead AS ( ",
    "  SELECT DocID, Start, Stop, Span, SpanKey, DocLen FROM ", .table,
    "  WHERE length(SpanKey) > 0 ",
    "    AND Start < greatest(", as.integer(.floor), ", ", .head, " * DocLen)), ",
    "grp AS ( ",
    "  SELECT DocID, SpanKey, COUNT(*) AS NOcc, min(Start) AS Start FROM inhead ",
    "  GROUP BY DocID, SpanKey) ",
    "SELECT g.DocID, g.SpanKey, g.NOcc, g.Start, i.Stop, i.Span, i.DocLen, ",
    "  CASE WHEN k.AnchorKey IS NOT NULL AND ( ",
    "         g.SpanKey = k.AnchorKey OR contains(g.SpanKey, k.AnchorKey) ",
    "         OR (contains(k.AnchorKey, g.SpanKey) ",
    "             AND length(g.SpanKey) >= ", as.integer(.min_reverse), ") ",
    "       ) THEN TRUE ELSE FALSE END AS IsAnchor ",
    "FROM grp g JOIN inhead i ON i.DocID = g.DocID AND i.SpanKey = g.SpanKey AND i.Start = g.Start ",
    "JOIN keys k ON k.DocID = g.DocID ",
    "QUALIFY row_number() OVER (PARTITION BY g.DocID, g.SpanKey ORDER BY i.Stop DESC) = 1"
  )) |>
    tibble::as_tibble()
}


#' How often each normalised name occurs across documents
#'
#' Document frequency, not term frequency. A name occurring in one contract is a party; a name
#' occurring in a third of them is a defined term the drafting convention supplies -- "COMPANY",
#' "PARTIES", "BOARD". This is the read that would set a stoplist, and it is reported rather than
#' applied, because a threshold chosen before the list is read is a guess.
#'
#' @param .tab Tibble from ent_head_entities().
#' @param .n_docs Integer. Denominator, the documents in the sample.
#' @return Tibble: SpanKey, Docs, PctDocs, Example -- ordered by Docs.
ent_head_terms <- function(.tab, .n_docs) {
  if (FALSE) {
    .tab    <- tab_head
    .n_docs <- nrow(tab_keys)
  }

  .tab |>
    dplyr::summarise(
      Docs    = dplyr::n_distinct(.data$DocID),
      Example = dplyr::first(.data$Span),
      .by = SpanKey
    ) |>
    dplyr::mutate(PctDocs = .data$Docs / .n_docs) |>
    dplyr::arrange(dplyr::desc(.data$Docs))
}


# 8. Reading spans back ----------------------------------------------------------------------------------------------
# Every table above is equally consistent with a chosen span being a preamble party and with it
# being a letterhead, a page footer or a defined term. Only the text distinguishes them, so a fixed
# sample is read on every render. One reader, used by every section, rather than three near-copies.

#' Rehydrate a sample of spans with the text either side
#'
#' Sampled deterministically, so the same documents appear on every render and a change in the
#' examples means a change in the extraction rather than a change in the draw.
#'
#' @param .tab Tibble carrying DocID, Start and Stop.
#' @param .path_text 04A's canonical text parquet.
#' @param .n Integer. Rows drawn.
#' @param .ctx Integer. Characters either side of the span.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: .tab's columns for the drawn rows, plus Snippet.
ent_read_spans <- function(.tab, .path_text, .n = 10L, .ctx = 200L, .seed = 42L) {
  if (FALSE) {
    .tab       <- dplyr::filter(tab_party, .data$Status == "located")
    .path_text <- .lP$Input$Text
    .n         <- 10L
    .ctx       <- 200L
    .seed      <- 42L
  }

  pick_ <- .tab |>
    dplyr::filter(!is.na(.data$Start)) |>
    (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n, nrow(.d)))))()

  if (nrow(pick_) == 0L) return(tibble::tibble())

  pick_ |>
    dplyr::left_join(
      arrow::read_parquet(.path_text) |> dplyr::filter(.data$DocID %in% pick_$DocID),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::mutate(
      # 0-based half-open offsets over code points; stri_sub is 1-based inclusive.
      Snippet = stringi::stri_replace_all_regex(
        paste0(
          "...",
          stringi::stri_sub(.data$TextRaw, from = pmax(1L, .data$Start + 1L - .ctx), to = .data$Start),
          " >>>", stringi::stri_sub(.data$TextRaw, from = .data$Start + 1L, to = .data$Stop), "<<< ",
          stringi::stri_sub(.data$TextRaw, from = .data$Stop + 1L, to = .data$Stop + .ctx),
          "..."
        ),
        "\\s+", " "
      )
    ) |>
    dplyr::select(-TextRaw)
}


#' Every distinct organisation one document carries in its head, read in order
#'
#' The block that says whether the deduplicated count means anything. A list of six names is
#' consistent with six parties and with two parties written four ways, and only the words separate
#' those two readings.
#'
#' @param .tab Tibble from ent_head_entities().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .n Integer. Documents drawn.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: the rows of the drawn documents.
ent_read_head <- function(.tab, .keys, .n = 8L, .seed = 42L) {
  if (FALSE) {
    .tab  <- tab_head
    .keys <- tab_keys
    .n    <- 8L
    .seed <- 42L
  }

  docs_ <- withr::with_seed(.seed, sample(unique(.tab$DocID), size = min(.n, dplyr::n_distinct(.tab$DocID))))

  .tab |>
    dplyr::filter(.data$DocID %in% docs_) |>
    dplyr::left_join(dplyr::select(.keys, DocID, CompanyName, Class), by = dplyr::join_by(DocID)) |>
    dplyr::arrange(.data$DocID, .data$Start)
}


# 9. Report ----------------------------------------------------------------------------------------------------------
# Report functions print through cli and return their tibble invisibly, so every number stays
# available for further work after it has been displayed.

#' How many documents carry a usable anchor key at all
#' @param .tab Tibble from ent_anchor_keys().
#' @return Invisibly .tab.
ent_report_keys <- function(.tab) {
  if (FALSE) .tab <- tab_keys

  cli::cli_h2("Anchor keys")
  tibble::tibble(
    Item = c("Documents", "With a company name", "With a usable key", "Name too short to key"),
    N    = c(nrow(.tab),
             sum(!is.na(.tab$CompanyName)),
             sum(!is.na(.tab$AnchorKey)),
             sum(!is.na(.tab$CompanyName) & is.na(.tab$AnchorKey)))
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$N / nrow(.tab))) |>
    tbl_say(.title = "Key availability")
  cli::cli_alert_info(
    "The last row is the cost of the five-character floor. A shorter key would raise coverage and \\
     locate less: it matches by containment, so it fires wherever those characters occur."
  )
  invisible(.tab)
}


#' Where the mass is and how many documents the region reaches
#' @param .tab Tibble from ent_window_grid().
#' @return Invisibly .tab.
ent_report_windows <- function(.tab) {
  if (FALSE) .tab <- tab_grid_win

  cli::cli_h2("Head and tail cutoffs")
  .tab |>
    dplyr::mutate(
      Share    = tbl_pct(.data$Share),
      PctSpans = tbl_pct(.data$PctSpans),
      PctDocs  = tbl_pct(.data$PctDocs)
    ) |>
    dplyr::select(Side, Share, Floor, PctSpans, PctDocs, MedKeys, P90Keys) |>
    tbl_say(.title = "Share of spans and of documents reached, by cutoff")
  cli::cli_alert_info(
    "PctSpans and PctDocs answer different questions. Mass says where the engine writes; documents \\
     reached says whether the region is populated in most contracts or crowded in a few. A large \\
     gap between them is the signature of a handful of table-heavy filings. The denominator for \\
     PctDocs is {max(.tab$DocsAll)} document{?s} the engine reached at all, NOT the whole sample."
  )
  invisible(.tab)
}


#' Anchor recall at each head cutoff
#' @param .tab Tibble from ent_anchor_grid().
#' @return Invisibly .tab.
ent_report_anchor_grid <- function(.tab) {
  if (FALSE) .tab <- tab_grid_anchor

  cli::cli_h2("Does the head contain the filer?")
  .tab |>
    dplyr::mutate(
      Share       = tbl_pct(.data$Share),
      PctOfKeyed  = tbl_pct(.data$PctOfKeyed),
      PctOfSample = tbl_pct(.data$PctOfSample)
    ) |>
    tbl_say(.title = "Documents whose anchor match falls inside the head")
  cli::cli_alert_info(
    "This is the only column that decides the window. Everything above describes the engine; this \\
     says whether a rule reading the head would find the party. Read it against MedKeys in the \\
     table above: the cutoff to take is where recall stops rising faster than the candidate count."
  )
  invisible(.tab)
}


#' Where the contracting party was found, and where it was not
#' @param .tab Tibble from ent_locate_party().
#' @return Invisibly the status table.
ent_report_party <- function(.tab) {
  if (FALSE) .tab <- tab_party

  out_ <- .tab |>
    dplyr::count(.data$Status, name = "N") |>
    dplyr::mutate(
      Status = plot_factor(.data$Status, .key = "PartyStatus"),
      Share  = .data$N / nrow(.tab)
    ) |>
    dplyr::arrange(.data$Status)

  cli::cli_h2("The contracting party, per document")
  out_ |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "Status")
  cli::cli_alert_info(
    "'located' is the working set. The other four are four different problems: 'no key' is a limit \\
     of the metadata, 'no match' an extraction gap, 'fragment only' a normalisation gap, and 'late' \\
     a fact about how that kind of agreement is drafted."
  )
  invisible(out_)
}


#' What kind of match located the party, and how far the engines corroborate it
#' @param .tab Tibble from ent_locate_party().
#' @return Invisibly the match table.
ent_report_match <- function(.tab) {
  if (FALSE) .tab <- tab_party

  loc_ <- dplyr::filter(.tab, .data$Status == "located")

  out_ <- loc_ |>
    dplyr::summarise(
      N          = dplyr::n(),
      MedKeyLen  = stats::median(.data$KeyLen),
      MedStart   = stats::median(.data$Start),
      MedEngines = stats::median(.data$NEngine),
      PctLeaked  = mean(.data$Leaked),
      .by = MatchKind
    ) |>
    dplyr::arrange(dplyr::desc(.data$N))

  cli::cli_h2("How the party was matched")
  out_ |>
    dplyr::mutate(
      Share     = tbl_pct(.data$N / nrow(loc_)),
      PctLeaked = tbl_pct(.data$PctLeaked),
      dplyr::across(c(N, MedKeyLen, MedStart, MedEngines), as.integer)
    ) |>
    tbl_say(.title = "Located spans by match kind")
  cli::cli_alert_info(
    "MedEngines is corroboration: how many engines proposed the very same offsets. A one on a \\
     reverse match is the row to distrust, because nothing else saw that span and the span is a \\
     fragment of the recorded name rather than the name."
  )
  invisible(out_)
}


#' Raw, distinct-raw and distinct-key counts inside the head
#' @param .head Tibble from ent_head_entities().
#' @param .region Tibble from ent_assign_region().
#' @return Invisibly the count table.
ent_report_dedup <- function(.head, .region) {
  if (FALSE) {
    .head   <- tab_head
    .region <- tab_region
  }

  raw_ <- .region |>
    dplyr::filter(.data$Region == "head") |>
    dplyr::summarise(
      NRaw         = dplyr::n(),
      NDistinctRaw = dplyr::n_distinct(stringi::stri_trans_toupper(.data$Span)),
      .by = DocID
    )

  key_ <- .head |> dplyr::summarise(NDistinctKey = dplyr::n(), .by = DocID)

  out_ <- raw_ |>
    dplyr::left_join(key_, by = dplyr::join_by(DocID)) |>
    dplyr::summarise(
      Docs            = dplyr::n(),
      MedRaw          = stats::median(.data$NRaw),
      MedDistinctRaw  = stats::median(.data$NDistinctRaw),
      MedDistinctKey  = stats::median(.data$NDistinctKey),
      P90DistinctKey  = stats::quantile(.data$NDistinctKey, 0.9, na.rm = TRUE),
      MaxDistinctKey  = max(.data$NDistinctKey, na.rm = TRUE)
    ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))

  cli::cli_h2("Deduplicating the head")
  tbl_say(out_, .title = "Organisations per document inside the head")
  cli::cli_alert_info(
    "MedDistinctRaw counts distinct raw spans; MedDistinctKey counts distinct normalised names. \\
     The gap between them is variant duplication -- 'Icosavax, Inc.', 'Icosavax Inc' and \\
     'Icosavax, Inc' are three spans and one company -- and the key collapses it for free."
  )
  invisible(out_)
}


#' The names that occur in the most documents
#' @param .tab Tibble from ent_head_terms().
#' @param .n Integer. Rows shown.
#' @return Invisibly .tab.
ent_report_terms <- function(.tab, .n = 40L) {
  if (FALSE) {
    .tab <- tab_terms
    .n   <- 40L
  }

  cli::cli_h2("The most common names in the head")
  .tab |>
    dplyr::mutate(PctDocs = tbl_pct(.data$PctDocs)) |>
    tbl_say(.title = "Document frequency of normalised names", .n = .n)
  cli::cli_alert_info(
    "Read this as a stoplist proposal, not as a result. A name in one contract is a party; a name \\
     in a tenth of them is a defined term the drafting convention supplies. Where the list stops \\
     being generic is where a threshold would go."
  )
  invisible(.tab)
}


#' Spans with the text either side, printed for reading
#' @param .tab Tibble from ent_read_spans(), carrying Snippet.
#' @param .cols Character vector of columns printed above each snippet.
#' @param .title Character. Heading.
#' @return Invisibly .tab.
ent_report_read <- function(.tab, .cols = c("CompanyName", "Party", "MatchKind", "Start"),
                            .title = "Located spans in context") {
  if (FALSE) {
    .tab   <- tab_read
    .cols  <- c("CompanyName", "Party", "MatchKind", "Start")
    .title <- "Located spans in context"
  }

  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("No rows to read for {(.title)}.")
    return(invisible(.tab))
  }
  cols_ <- intersect(.cols, names(.tab))

  cli::cli_h2(.title)
  show_ <- .tab |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cols_), as.character)) |>
    dplyr::select(dplyr::all_of(c(cols_, "Snippet")))

  purrr::walk(seq_len(nrow(show_)), function(.i) {
    row_ <- show_[.i, ]
    cli::cli_h3("{row_[[cols_[1]]]}")
    if (length(cols_) > 1L) {
      cat("  ", paste(paste0(cols_[-1], ": ", unlist(row_[cols_[-1]])), collapse = " | "),
          "\n", sep = "")
    }
    cat("  ", row_[["Snippet"]], "\n\n", sep = "")
  })
  cli::cli_alert_info(
    "Read for two things. Whether the span is the filer's name, which is precision on identity; \\
     and whether the surrounding words make it a PARTY, which is lower and is the number that \\
     matters."
  )
  invisible(.tab)
}


#' Every distinct name one document carries, printed document by document
#' @param .tab Tibble from ent_read_head().
#' @return Invisibly .tab.
ent_report_head <- function(.tab) {
  if (FALSE) .tab <- tab_read_head

  cli::cli_h2("Every organisation in the head, one document at a time")
  purrr::walk(unique(.tab$DocID), function(.d) {
    rows_ <- dplyr::filter(.tab, .data$DocID == .d)
    cli::cli_h3("{rows_$CompanyName[1]} | {rows_$Class[1]} | {nrow(rows_)} name{?s}")
    purrr::pwalk(
      dplyr::select(rows_, Start, NOcc, IsAnchor, Span),
      function(Start, NOcc, IsAnchor, Span) {
        cat(sprintf("  %6d  x%-3d %s %s\n", Start, NOcc, if (IsAnchor) "[A]" else "   ", Span))
      }
    )
    cat("\n")
  })
  cli::cli_alert_info(
    "[A] marks a name matching the EDGAR anchor. Read the rest as the counterparty candidate set: \\
     what is left after the anchor is either the other side of the contract or noise, and this is \\
     the block that says which."
  )
  invisible(.tab)
}


#' Located share by contract type
#' @param .tab Tibble from ent_locate_party().
#' @return Invisibly the class table.
ent_report_class <- function(.tab) {
  if (FALSE) .tab <- tab_party

  out_ <- .tab |>
    dplyr::count(.data$Class, .data$Status, name = "N") |>
    tidyr::pivot_wider(names_from = Status, values_from = N, values_fill = 0L) |>
    dplyr::mutate(
      Docs       = as.integer(rowSums(dplyr::pick(dplyr::where(is.numeric)))),
      PctLocated = .data$located / .data$Docs
    ) |>
    dplyr::arrange(dplyr::desc(.data$Docs))

  cli::cli_h2("Coverage by contract type")
  out_ |>
    dplyr::mutate(PctLocated = tbl_pct(.data$PctLocated)) |>
    tbl_say(.title = "Documents by status and contract type")
  cli::cli_alert_info(
    "A class low here is not necessarily a failure of the match. It can be a class whose documents \\
     do not name the filer near the front, which is a fact about the contract rather than the \\
     extraction, and only the read blocks tell the two apart."
  )
  invisible(out_)
}


#' Coverage against document length, which is what the max() window exists for
#' @param .tab Tibble from ent_locate_party().
#' @return Invisibly the length table.
ent_report_length <- function(.tab) {
  if (FALSE) .tab <- tab_party

  out_ <- .tab |>
    dplyr::filter(!is.na(.data$DocLen)) |>
    dplyr::mutate(LenDecile = dplyr::ntile(.data$DocLen, 10L)) |>
    dplyr::summarise(
      Docs       = dplyr::n(),
      MedDocLen  = stats::median(.data$DocLen),
      MedHeadEnd = stats::median(.data$HeadEnd, na.rm = TRUE),
      PctLocated = mean(.data$Status == "located"),
      MedStart   = stats::median(.data$Start[.data$Status == "located"], na.rm = TRUE),
      .by = LenDecile
    ) |>
    dplyr::arrange(.data$LenDecile)

  cli::cli_h2("The window against document length")
  out_ |>
    dplyr::mutate(
      PctLocated = tbl_pct(.data$PctLocated),
      dplyr::across(c(MedDocLen, MedHeadEnd, MedStart), as.integer)
    ) |>
    tbl_say(.title = "Located share by decile of document length")
  cli::cli_alert_info(
    "MedHeadEnd flat in the first deciles and rising in the last is the floor binding on short \\
     documents and the share binding on long ones -- which is exactly what max() is for. A located \\
     share that falls at either end means the arm covering that end is set wrong."
  )
  invisible(out_)
}


#' Every report block in this document, in order
#' @param .keys Tibble from ent_anchor_keys().
#' @param .grid_win Tibble from ent_window_grid().
#' @param .grid_anchor Tibble from ent_anchor_grid().
#' @param .party Tibble from ent_locate_party().
#' @param .head Tibble from ent_head_entities().
#' @param .region Tibble from ent_assign_region().
#' @param .terms Tibble from ent_head_terms().
#' @return Invisibly NULL.
ent_report_all_org <- function(.keys, .grid_win, .grid_anchor, .party, .head, .region, .terms) {
  if (FALSE) {
    .keys        <- tab_keys
    .grid_win    <- tab_grid_win
    .grid_anchor <- tab_grid_anchor
    .party       <- tab_party
    .head        <- tab_head
    .region      <- tab_region
    .terms       <- tab_terms
  }
  ent_report_keys(.tab = .keys)
  ent_report_windows(.tab = .grid_win)
  ent_report_anchor_grid(.tab = .grid_anchor)
  ent_report_party(.tab = .party)
  ent_report_match(.tab = .party)
  ent_report_dedup(.head = .head, .region = .region)
  ent_report_terms(.tab = .terms, .n = 25L)
  ent_report_class(.tab = .party)
  ent_report_length(.tab = .party)
  invisible(NULL)
}


# 10. Figures --------------------------------------------------------------------------------------------------------

#' Span density across the document, both weightings on one panel
#'
#' The vertical rules mark the NOMINAL cutoffs. On a document short enough for the floor to bind the
#' effective head reaches further right than the rule drawn, which is why the console reports the
#' effective width beside this.
#'
#' @param .tab Tibble from ent_density().
#' @param .head Numeric. Nominal head share, drawn as a rule.
#' @param .tail Numeric. Nominal tail share, drawn as a rule.
#' @return A ggplot.
ent_plot_density <- function(.tab, .head = 0.10, .tail = 0.20) {
  if (FALSE) {
    .tab  <- tab_density
    .head <- 0.10
    .tail <- 0.20
  }

  ggplot2::ggplot(.tab, ggplot2::aes(x = .data$Pos, y = .data$Share, colour = .data$Weight)) +
    ggplot2::geom_vline(xintercept = c(.head, 1 - .tail), linewidth = 0.3, linetype = "dashed",
                        colour = "grey40") +
    ggplot2::geom_line(linewidth = 0.5) +
    plot_scale_colour_cat() +
    plot_scale_x_pct(.accuracy = 1) +
    plot_scale_y_pct(.accuracy = 1) +
    ggplot2::labs(x = "Position in document", y = "Share of spans", colour = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}


#' Status composition by contract type
#'
#' Shares rather than counts, because the classes differ in size by an order of magnitude and a
#' count plot would be a picture of the taxonomy instead of the coverage.
#'
#' @param .tab Tibble from ent_locate_party().
#' @param .key_class Character. Registered vocabulary the rows are ordered by.
#' @return A ggplot.
ent_plot_party_status <- function(.tab, .key_class = "ClassDetailed") {
  if (FALSE) {
    .tab       <- tab_party
    .key_class <- "ClassDetailed"
  }
  plot_bar_stacked(
    .tab      = dplyr::count(.tab, .data$Class, .data$Status, name = "N"),
    .cat      = "Class",
    .val      = "N",
    .fill     = "Status",
    .key      = .key_class,
    .key_fill = "PartyStatus",
    .share    = TRUE
  )
}
