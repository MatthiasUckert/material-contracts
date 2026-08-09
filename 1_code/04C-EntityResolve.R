# 04C-EntityResolve: turn spans into the four variables the paper reports ----
#
# WHAT THIS FILE DOES, IN ONE PARAGRAPH
# 04A extracted candidates and described them. 04B measured which engine to keep, which window to
# read, and which of the reading sessions' rules survive contact with the EDGAR anchors. Neither
# produced a variable. This file does: it applies the settled policy to the candidate store,
# assigns a role to each surviving span, and collapses those into one row per contract carrying the
# parties, the contract dates, the party locations and what can be said about value.
#
# IT RESOLVES WHAT 04E WILL PRODUCE, NOT WHAT 04A EXTRACTED
# The store holds eight engines over full text. The corpus pass will run one engine per label over
# a capped window. Resolving the union here would validate a pipeline that is never deployed, so
# the candidate set is filtered through extraction_policy.parquet first and every number below
# describes the deployed configuration.
#
# THREE CONSTRAINTS INHERITED FROM THE MEASUREMENT, NONE OPTIONAL
# Role attaches to an OCCURRENCE, never to a string: a reading session found "New York" serving as
# a party address, as the governing law, as a court venue and inside an exchange name within one
# document, so any design that resolves a place once per document is wrong before it starts.
# Party counts need deduplication, because per-document mention counts are inflated by length and
# per-thousand-character rates are deflated by repetition, and the truth is bracketed between two
# measures that are each wrong in a known direction. And a stop matches a whole span, never a
# substring, or a rule against "company" removes Blue Ridge Real Estate Company.
#
# THE DURATION MEASURE IS THE POINT OF THE DATE SECTION
# The published contract term is the latest date found in a document minus its filing date, and a
# referee observed that it carries a standard deviation of 46.8 years on debt contracts -- an
# impossible figure for an instrument with a mean term under three years, and a symptom of statutory
# references and long-dated maturities entering a maximum. Both measures are computed here, side by
# side, so the difference is shown rather than asserted.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .db_path    <- .lP$Input$Store
  .path_text  <- .lP$Input$Text
  .path_rules <- .lP$Input$Kept
}


# 1. The deployed candidate set -------------------------------------------

#' Read the extraction policy 04B settled
#'
#' @param .path Policy parquet written by 04B.
#' @return Tibble: Label, Combo, CapChars, TailChars, and the recall each implies.
ent_read_policy <- function(.path) {
  if (FALSE) .path <- .lP$Input$Policy

  pol_ <- arrow::read_parquet(.path)
  need_ <- c("Label", "Combo", "CapChars")
  miss_ <- setdiff(need_, names(pol_))
  if (length(miss_) > 0L) cli::cli_abort("Policy is missing: {miss_}")
  if (!"TailChars" %in% names(pol_)) pol_$TailChars <- 0L
  pol_
}

#' Restrict the store to the candidates the corpus pass will actually produce
#'
#' One engine per label, and only the spans falling inside that label's window. Everything
#' downstream reads this table rather than the store, so no later section can quietly resolve a
#' candidate the deployment would never see.
#'
#' The window is applied in characters from each end, because that is what the extractor takes.
#' A tail of zero is the ordinary prefix case; geography is the label that needs a non-zero one,
#' since a governing-law clause sits in the final fifth of a contract and a prefix never reaches it.
#'
#' @param .con Session with the store attached as s.
#' @param .policy Tibble from ent_read_policy().
#' @param .path_text Canonical text parquet, supplying document lengths.
#' @return Invisibly the row count of the table created.
ent_deploy_candidates <- function(.con, .policy, .path_text) {
  if (FALSE) {
    .con       <- con
    .policy    <- tab_policy
    .path_text <- .lP$Input$Text
  }

  ent_put_table(.con = .con, .name = "policy", .tab = dplyr::mutate(
    .policy,
    Engine = sub(":.*$", "", .data$Combo),
    Model  = dplyr::if_else(grepl(":", .data$Combo, fixed = TRUE),
                            sub("^[^:]*:", "", .data$Combo), .data$Combo)
  ))
  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE lens AS SELECT DocID, length(TextRaw) AS DocLen ",
    "FROM read_parquet('", as.character(fs::path_abs(.path_text)), "') WHERE length(TextRaw) > 0"
  ))
  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE deployed AS ",
    "SELECT DISTINCT c.DocID, c.Label, c.Start, c.Stop, c.Span, c.LabelRaw ",
    "FROM s.candidates c ",
    "JOIN policy p ON c.Label = p.Label AND c.Engine = p.Engine AND c.Model = p.Model ",
    "JOIN lens l ON c.DocID = l.DocID ",
    "WHERE c.Start IS NOT NULL ",
    "  AND (c.Stop <= p.CapChars OR c.Start >= l.DocLen - p.TailChars)"
  ))
  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM deployed")$N
  cli::cli_alert_success("Deployed candidate set: {n_} span{?s}")
  invisible(n_)
}


# 2. Roles ----------------------------------------------------------------

#' Terms the document itself defines in parentheses
#'
#' A reading session's strongest observation about organisations: a span that is a single
#' capitalised common noun already introduced as a defined term -- Tenant, Company, Buyer, Holder --
#' is never an organisation name, and a rule dropping those is worth more than the hand-written stop
#' list beside it. The list is per document, which is what makes it stronger: "Company" is a defined
#' term in the contract that defines it and part of a name in Blue Ridge Real Estate Company.
#'
#' Only single words are taken. A multi-word parenthetical is as often a jurisdiction or an aside as
#' a definition, and bounding the pattern to one token keeps a real party name from being captured.
#'
#' @param .con Session.
#' @param .path_text Canonical text parquet.
#' @return Invisibly the row count of the table created.
ent_defined_terms <- function(.con, .path_text) {
  if (FALSE) {
    .con       <- con
    .path_text <- .lP$Input$Text
  }

  # Straight and curly double quotes, built from code points: DuckDB's regex rejects \\u escapes
  # and the house rule keeps the characters out of the source. The straight single quote is
  # deliberately absent -- it would terminate the SQL literal, and contracts do not use it here.
  q_ <- paste0("[", intToUtf8(c(0x22, 0x201C, 0x201D)), "]")
  rx_ <- paste0("\\\\(\\\\s*(?:the\\\\s+)?", q_, "?([A-Z][A-Za-z]{2,28})", q_, "?\\\\s*\\\\)")

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE defterms AS ",
    "SELECT DISTINCT DocID, upper(unnest(regexp_extract_all(TextRaw, '", rx_, "', 1))) AS Term ",
    "FROM read_parquet('", as.character(fs::path_abs(.path_text)), "')"
  ))
  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM defterms")$N
  cli::cli_alert_success("Defined terms: {n_} across the sample")
  invisible(n_)
}

#' Assign a role to each deployed span from the measured rules
#'
#' Cues are matched as literal phrases in a window on the stated side, exactly as 04B scored them.
#' Where several fire with different roles the highest measured lift wins, which is the only
#' non-arbitrary tie-break available: it prefers the rule the anchor showed to be most concentrated
#' on the thing it claims to mark.
#'
#' Stops are applied as whole-span equality and drop the span outright. The defined-term list does
#' the same for organisations. A span no cue reaches keeps a NULL role and is carried rather than
#' discarded, because for parties the preamble position is itself evidence and 04C's party resolver
#' uses it.
#'
#' @param .con Session with deployed built.
#' @param .path_text Canonical text parquet.
#' @param .rules Tibble of rules kept by 04B.
#' @param .ctx_max Widest context materialised, capping the reach of any rule.
#' @return Invisibly the row count of the table created.
ent_assign_roles <- function(.con, .path_text, .rules, .ctx_max = 400L) {
  if (FALSE) {
    .con       <- con
    .path_text <- .lP$Input$Text
    .rules     <- tab_rules
    .ctx_max   <- 400L
  }

  cues_ <- .rules |>
    dplyr::filter(.data$Kind == "cue", !is.na(.data$Role), .data$Role != "") |>
    dplyr::mutate(RuleLift = dplyr::coalesce(.data$Lift, 1)) |>
    dplyr::select(Label, Role, Pattern, Side, Window, RuleLift)
  stops_ <- .rules |>
    dplyr::filter(.data$Kind == "stop") |>
    dplyr::distinct(Label, Pattern)
  ent_put_table(.con = .con, .name = "cues", .tab = cues_)
  ent_put_table(.con = .con, .name = "stops", .tab = stops_)

  norm_ <- paste0("trim(regexp_replace(regexp_replace(upper(d.Span), '[^A-Z0-9 ]', ' ', 'g'), ",
                  "'\\\\s+', ' ', 'g'))")

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE spanctx AS ",
    "SELECT d.DocID, d.Label, d.Start, d.Stop, d.Span, d.LabelRaw, ", norm_, " AS SpanNorm, ",
    "  ((d.Start + d.Stop) / 2.0) / l.DocLen AS Pos, ",
    "  lower(regexp_replace(substring(t.TextRaw, greatest(1, d.Start + 1 - ", as.integer(.ctx_max),
    "), least(", as.integer(.ctx_max), ", d.Start)), '\\\\s+', ' ', 'g')) AS LeftCtx, ",
    "  lower(regexp_replace(substring(t.TextRaw, d.Stop + 1, ", as.integer(.ctx_max),
    "), '\\\\s+', ' ', 'g')) AS RightCtx ",
    "FROM deployed d JOIN read_parquet('", as.character(fs::path_abs(.path_text)),
    "') t USING (DocID) JOIN lens l USING (DocID)"
  ))

  # Drop what the rules say is never the entity, then take the best-supported role for the rest.
  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE roles AS ",
    "WITH kept AS ( ",
    "  SELECT * FROM spanctx sc ",
    "  WHERE NOT EXISTS (SELECT 1 FROM stops st ",
    "                    WHERE st.Label = sc.Label AND sc.SpanNorm = upper(st.Pattern)) ",
    "    AND NOT (sc.Label = 'ORG' AND EXISTS (SELECT 1 FROM defterms dt ",
    "             WHERE dt.DocID = sc.DocID AND dt.Term = sc.SpanNorm))), ",
    "hit AS ( ",
    "  SELECT k.DocID, k.Label, k.Start, k.Stop, c.Role, c.RuleLift ",
    "  FROM kept k JOIN cues c USING (Label) ",
    "  WHERE CASE c.Side ",
    "    WHEN 'left'  THEN contains(right(k.LeftCtx,  c.Window), c.Pattern) ",
    "    WHEN 'right' THEN contains(left(k.RightCtx,  c.Window), c.Pattern) ",
    "    ELSE contains(right(k.LeftCtx, c.Window), c.Pattern) ",
    "         OR contains(left(k.RightCtx, c.Window), c.Pattern) END ",
    "  QUALIFY row_number() OVER (PARTITION BY k.DocID, k.Label, k.Start, k.Stop ",
    "                             ORDER BY c.RuleLift DESC, c.Role) = 1) ",
    "SELECT k.DocID, k.Label, k.Start, k.Stop, k.Span, k.SpanNorm, k.LabelRaw, k.Pos, ",
    "       h.Role, h.RuleLift ",
    "FROM kept k LEFT JOIN hit h USING (DocID, Label, Start, Stop)"
  ))
  n_ <- DBI::dbGetQuery(.con, paste0(
    "SELECT COUNT(*) AS N, SUM(CASE WHEN Role IS NOT NULL THEN 1 ELSE 0 END) AS NRole FROM roles"
  ))
  cli::cli_alert_success("Roles assigned: {n_$NRole} of {n_$N} deployed span{?s} carry one")
  invisible(n_$N)
}


# 3. The four variables ---------------------------------------------------

#' Contracting parties, deduplicated within each document
#'
#' A party is taken as an organisation span carrying the party role, or -- where no cue reached it --
#' one standing in the opening of the document, since the preamble is where parties are named and a
#' cue list will never be complete. Names are collapsed to the key form so that "ACME HOLDINGS,
#' INC." and "Acme Holdings" count once, which is the deduplication the two mention-based measures
#' could not do.
#'
#' @param .con Session with roles built.
#' @param .head_pos Numeric. Relative position under which an uncued organisation counts as a party.
#' @return Tibble: one row per document with NParties and the party list.
ent_resolve_parties <- function(.con, .head_pos = 0.10) {
  if (FALSE) {
    .con      <- con
    .head_pos <- 0.10
  }

  DBI::dbGetQuery(.con, paste0(
    "WITH p AS ( ",
    "  SELECT DocID, SpanNorm FROM roles ",
    "  WHERE Label = 'ORG' AND length(SpanNorm) >= 5 ",
    "    AND (Role = 'party' OR (Role IS NULL AND Pos <= ", .head_pos, "))) ",
    "SELECT DocID, COUNT(DISTINCT SpanNorm) AS NParties, ",
    "       string_agg(DISTINCT SpanNorm, ' | ') AS Parties ",
    "FROM p GROUP BY DocID"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(NParties = as.integer(.data$NParties))
}

#' Contract dates, and the published measure beside them
#'
#' The start is the earliest parseable date carrying a start-like role; the end the latest carrying
#' an expiry role. Both are computed only from dates the roles reach, which is the whole difference
#' from the published measure: that one takes the maximum date in the document, so a statutory
#' reference to an Act of 1933 or a maturity in 2099 enters the term of a two-year contract.
#'
#' DurationPaper reproduces the published definition exactly, on the same documents, so the two can
#' be compared rather than argued about.
#'
#' @param .con Session with roles built.
#' @param .formats strptime formats, as used in 04B.
#' @param .roles_start,.roles_end Role names treated as the beginning and the end of the term.
#' @return Tibble: one row per document with ContractStart, ContractEnd and NDates.
ent_resolve_dates <- function(.con, .formats,
                              .roles_start = c("signing", "effective", "term_start"),
                              .roles_end = "expiry") {
  if (FALSE) {
    .con         <- con
    .formats     <- .DATE_FORMATS
    .roles_start <- c("signing", "effective", "term_start")
    .roles_end   <- "expiry"
  }

  fmt_ <- paste0("['", paste(.formats, collapse = "','"), "']")
  in_ <- function(.x) paste0("('", paste(.x, collapse = "','"), "')")

  DBI::dbGetQuery(.con, paste0(
    "WITH d AS ( ",
    "  SELECT DocID, Role, CAST(try_strptime(", ent_sql_dateclean("Span"), ", ", fmt_,
    ") AS DATE) AS D FROM roles WHERE Label = 'DATE'), ",
    "ok AS (SELECT * FROM d WHERE D IS NOT NULL AND D BETWEEN DATE '1980-01-01' ",
    "                                                     AND DATE '2100-01-01') ",
    "SELECT DocID, ",
    "  min(CASE WHEN Role IN ", in_(.roles_start), " THEN D END) AS ContractStart, ",
    "  max(CASE WHEN Role IN ", in_(.roles_end), " THEN D END) AS ContractEnd, ",
    "  COUNT(*) AS NDates ",
    "FROM ok GROUP BY DocID"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(NDates = as.integer(.data$NDates))
}

#' Reproduce the published term measure, on the terms it was published under
#'
#' The published definition is the latest date ANYWHERE IN THE DOCUMENT minus the filing date. It
#' therefore has to be computed from the full store over full text, not from the deployed candidate
#' set: taking it from a capped window leaves only preamble dates, every one of which precedes the
#' filing, and the measure comes out negative. That is not a corrected version of the published
#' figure, it is a different quantity carrying its name.
#'
#' Computing it here from the unfiltered store is the whole point of the comparison. The right tail
#' is where a statutory reference and a long-dated maturity live, and the right tail is exactly what
#' a window removes.
#'
#' @param .con Session with the store attached as s.
#' @param .formats strptime formats.
#' @return Tibble: DocID, MaxDateAny, NDatesFull.
ent_published_dates <- function(.con, .formats) {
  if (FALSE) {
    .con     <- con
    .formats <- .DATE_FORMATS
  }

  fmt_ <- paste0("['", paste(.formats, collapse = "','"), "']")
  DBI::dbGetQuery(.con, paste0(
    "WITH d AS ( ",
    "  SELECT DISTINCT DocID, Span FROM s.candidates WHERE Label = 'DATE' AND Start IS NOT NULL), ",
    "p AS (SELECT DocID, CAST(try_strptime(", ent_sql_dateclean("Span"), ", ", fmt_,
    ") AS DATE) AS D FROM d) ",
    "SELECT DocID, max(D) AS MaxDateAny, COUNT(D) AS NDatesFull FROM p ",
    "WHERE D IS NOT NULL GROUP BY DocID"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(NDatesFull = as.integer(.data$NDatesFull))
}

#' Coverage of a role under a range of windows, for the roles no anchor can speak to
#'
#' The character grid in 04B chooses a window on how often it recovers the ANCHOR, and the date
#' anchor is a start date. Nothing there measures whether an expiry date survives the same window,
#' and a head-only cap returned one for 22 documents of 4,398 while the same cues fire in their
#' thousands on full text. This reports coverage directly so the window is chosen on the quantity it
#' is meant to preserve.
#'
#' @param .con Session with roles and lens built, and the store attached.
#' @param .label Entity label.
#' @param .roles Roles counted as covered.
#' @param .head,.tail Integer character budgets to evaluate.
#' @return Tibble: Head, Tail, NDocs, PctDocs.
ent_role_coverage <- function(.con, .label, .roles,
                              .head = c(3000L, 5000L, 10000L),
                              .tail = c(0L, 2000L, 5000L, 10000L)) {
  if (FALSE) {
    .con   <- con
    .label <- "DATE"
    .roles <- "expiry"
    .head  <- c(3000L, 5000L, 10000L)
    .tail  <- c(0L, 2000L, 5000L, 10000L)
  }

  ent_put_table(.con = .con, .name = "rgrid",
                .tab = tidyr::expand_grid(Head = as.integer(.head), Tail = as.integer(.tail)))
  in_ <- paste0("('", paste(.roles, collapse = "','"), "')")

  DBI::dbGetQuery(.con, paste0(
    "WITH r AS (SELECT r.DocID, r.Start, r.Stop, l.DocLen FROM roles r JOIN lens l USING (DocID) ",
    "           WHERE r.Label = '", .label, "' AND r.Role IN ", in_, "), ",
    "tot AS (SELECT COUNT(*) AS N FROM lens) ",
    "SELECT g.Head, g.Tail, ",
    "  COUNT(DISTINCT CASE WHEN r.Stop <= g.Head OR r.Start >= r.DocLen - g.Tail ",
    "        THEN r.DocID END) AS NDocs, ",
    "  COUNT(DISTINCT CASE WHEN r.Stop <= g.Head OR r.Start >= r.DocLen - g.Tail ",
    "        THEN r.DocID END) / CAST(any_value(tot.N) AS DOUBLE) AS PctDocs ",
    "FROM rgrid g LEFT JOIN r ON TRUE, tot GROUP BY g.Head, g.Tail ORDER BY g.Head, g.Tail"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(c(Head, Tail, NDocs), as.integer))
}

#' Places, by the role each occurrence carries
#'
#' Reported per occurrence and never per string, because a reading session found the same place name
#' serving four different roles inside a single contract. The per-document summary is therefore a
#' MIX of roles rather than a location, and the share of occurrences that are party addresses is the
#' number a reviewer asked for: whether the countries counted in the published geography are the
#' locations of the contracting entities or mentions of somewhere else.
#'
#' @param .con Session with roles built.
#' @return Tibble: one row per document with counts by role and the party-address list.
ent_resolve_places <- function(.con) {
  if (FALSE) .con <- con

  DBI::dbGetQuery(.con, paste0(
    "SELECT DocID, ",
    "  COUNT(*) AS NPlaces, ",
    "  SUM(CASE WHEN Role = 'party_address' THEN 1 ELSE 0 END) AS NPartyAddress, ",
    "  SUM(CASE WHEN Role = 'governing_law' THEN 1 ELSE 0 END) AS NGoverningLaw, ",
    "  SUM(CASE WHEN Role = 'incorporation' THEN 1 ELSE 0 END) AS NIncorporation, ",
    "  SUM(CASE WHEN Role = 'performance'   THEN 1 ELSE 0 END) AS NPerformance, ",
    "  SUM(CASE WHEN Role = 'incidental'    THEN 1 ELSE 0 END) AS NIncidental, ",
    "  SUM(CASE WHEN Role IS NULL           THEN 1 ELSE 0 END) AS NUnassigned, ",
    "  string_agg(DISTINCT CASE WHEN Role = 'party_address' THEN SpanNorm END, ' | ') ",
    "    AS PartyPlaces ",
    "FROM roles WHERE Label = 'GPE' GROUP BY DocID"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("N"), as.integer))
}

#' What can be said about value, which is less than the other three
#'
#' No anchor exists for a contract's value, so nothing here is validated the way the parties, dates
#' and places are. Two things are still worth carrying. The largest amount in a document is a
#' heuristic a reading session argued for -- par value and per-share prices repeat many times while
#' an aggregate principal amount appears once, and ranking by magnitude separates them almost
#' perfectly. And redaction is a variable in its own right, because the figures withheld are
#' systematically the commercially material ones, which is why they were withheld.
#'
#' The amount is parsed from the span rather than trusted: a money span carrying no currency marker
#' and no thousands separator is a section number, which is the largest false-positive family in the
#' label.
#'
#' REDACTION IS COUNTED IN REGIONS, NOT MARKERS
#' A redacted table contributes one marker per cell, so a marker count measures how tabular a
#' contract is at least as much as how much was withheld. Measured on the sample: 84.8% of 60,547
#' markers come from 106 documents, and a single contract carries 14,689 of them across 193
#' regions. The median document has five markers and three regions. Runs of markers separated by
#' less than .gap characters are therefore collapsed, and the count of regions is what leaves this
#' function. NMarkers is kept beside it because the ratio is itself diagnostic -- a document at
#' seventy markers per region is one redacted schedule, not seventy withheld terms.
#'
#' @param .con Session with roles built.
#' @param .gap Integer. Markers closer than this belong to one redacted region.
#' @return Tibble: one row per document with the money and redaction summaries.
ent_resolve_value <- function(.con, .gap = 200L) {
  if (FALSE) {
    .con <- con
    .gap <- 200L
  }

  # Digits only, after stripping the grouping separators; a span with neither a currency symbol nor
  # a separator is not an amount.
  num_ <- "TRY_CAST(regexp_replace(Span, '[^0-9.]', '', 'g') AS DOUBLE)"
  cur_ <- paste0("regexp_matches(Span, '[", intToUtf8(c(0x24, 0xA3, 0xA5, 0x20AC)), "]')")

  DBI::dbGetQuery(.con, paste0(
    "WITH m AS ( ",
    "  SELECT DocID, ", num_, " AS Amount FROM roles ",
    "  WHERE Label = 'MONEY' AND (", cur_, " OR regexp_matches(Span, '[0-9],[0-9]{3}'))), ",
    # Gaps and islands: a marker opens a new region when the run of blank space before it exceeds
    # the gap, and the running sum of those openings numbers the regions within each document.
    "o AS (SELECT DocID, Start, Stop, LabelRaw, ",
    "        LAG(Stop) OVER (PARTITION BY DocID ORDER BY Start) AS PrevStop ",
    "      FROM roles WHERE Label = 'REDACT'), ",
    "f AS (SELECT *, CASE WHEN PrevStop IS NULL OR Start - PrevStop > ", as.integer(.gap),
    "        THEN 1 ELSE 0 END AS NewRegion FROM o), ",
    "g AS (SELECT *, SUM(NewRegion) OVER (PARTITION BY DocID ORDER BY Start ",
    "        ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS RegionID FROM f), ",
    "r AS (SELECT DocID, COUNT(DISTINCT RegionID) AS NRedact, COUNT(*) AS NRedactMarkers, ",
    "        COUNT(DISTINCT LabelRaw) AS NRedactClasses FROM g GROUP BY DocID) ",
    "SELECT COALESCE(m.DocID, r.DocID) AS DocID, ",
    "  COUNT(m.Amount) AS NAmounts, max(m.Amount) AS MaxAmount, ",
    "  any_value(r.NRedact) AS NRedact, any_value(r.NRedactMarkers) AS NRedactMarkers, ",
    "  any_value(r.NRedactClasses) AS NRedactClasses ",
    "FROM m FULL JOIN r ON m.DocID = r.DocID GROUP BY 1"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(c(NAmounts, NRedact, NRedactMarkers, NRedactClasses), as.integer))
}


# 4. Assembly and validation ----------------------------------------------

#' One row per contract, carrying every variable and the facts to check it against
#'
#' @param .keys Tibble from ent_anchor_keys().
#' @param .parties,.dates,.places,.value The four resolver outputs.
#' @param .pubdates Tibble from ent_published_dates(): the maximum date over the FULL document,
#'   which is what the published term measure is built from and what a windowed set cannot supply.
#' @return Tibble: one row per document.
ent_assemble <- function(.keys, .parties, .dates, .places, .value, .pubdates) {
  if (FALSE) {
    .keys     <- tab_keys
    .parties  <- tab_parties
    .dates    <- tab_dates
    .places   <- tab_places
    .value    <- tab_value
    .pubdates <- tab_pubdates
  }

  .keys |>
    dplyr::select(DocID, Fold, Class, DateFiled, AnchorKey, AnchorText) |>
    dplyr::left_join(.parties, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(.dates,   by = dplyr::join_by(DocID)) |>
    dplyr::left_join(.places,  by = dplyr::join_by(DocID)) |>
    dplyr::left_join(.value,   by = dplyr::join_by(DocID)) |>
    dplyr::left_join(.pubdates, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      NParties      = dplyr::coalesce(.data$NParties, 0L),
      DurationNew   = as.numeric(.data$ContractEnd - .data$ContractStart) / 365.25,
      # The published definition, reproduced exactly: the latest date anywhere in the document,
      # minus the filing date.
      DurationPaper = as.numeric(.data$MaxDateAny - .data$DateFiled) / 365.25,
      FilingDelay   = as.numeric(.data$DateFiled - .data$ContractStart) / 365.25,
      # Containment either way, because the resolved name and the recorded one differ in which
      # corporate form they carry: "ACME HOLDINGS" is the key, "ACME HOLDINGS INC" the span.
      HasFilerParty = purrr::map2_lgl(.data$Parties, .data$AnchorKey, function(.p, .k) {
        if (is.na(.p) || is.na(.k)) return(NA)
        parts_ <- strsplit(.p, " | ", fixed = TRUE)[[1]]
        if (length(parts_) == 0L) return(FALSE)
        any(stringi::stri_detect_fixed(parts_, .k)) ||
          any(stringi::stri_detect_fixed(.k, parts_))
      })
    )
}


# 5. Report ---------------------------------------------------------------

#' What the policy admitted and what the rules did with it
#' @param .n_deployed,.n_roles Integer counts from the build steps.
#' @param .tab Tibble from ent_assemble().
#' @return Invisibly .tab.
ent_report_resolve <- function(.n_deployed, .n_roles, .tab) {
  if (FALSE) {
    .n_deployed <- n_deployed
    .n_roles    <- n_roles
    .tab        <- tab_vars
  }

  cli::cli_h2("Resolution")
  clf_say_table(
    .tab = tibble::tibble(
      Item = c("Deployed spans", "Spans surviving stops", "Documents resolved",
               "Documents with >=2 parties", "Documents with a contract start",
               "Documents with a contract end", "Documents with an amount",
               "Documents with a redaction", "Redacted regions", "Redaction markers"),
      N = c(.n_deployed, .n_roles, nrow(.tab),
            sum(.tab$NParties >= 2L, na.rm = TRUE),
            sum(!is.na(.tab$ContractStart)), sum(!is.na(.tab$ContractEnd)),
            sum(!is.na(.tab$MaxAmount)),
            sum(dplyr::coalesce(.tab$NRedact, 0L) > 0L),
            sum(.tab$NRedact, na.rm = TRUE),
            sum(.tab$NRedactMarkers, na.rm = TRUE))
    )
  )
  cli::cli_alert_info(
    "Redaction is counted in REGIONS. A redacted schedule contributes one marker per cell, so the \\
     marker total is a statement about how tabular the corpus is: on this sample 85% of markers \\
     come from 106 documents, and one contract carries 14,689 of them across 193 regions."
  )
  invisible(.tab)
}

#' The duration measure against the published one
#' @param .tab Tibble from ent_assemble().
#' @return Invisibly the comparison tibble.
ent_report_duration <- function(.tab) {
  if (FALSE) .tab <- tab_vars

  cmp_ <- tibble::tibble(
    Measure = c("Published: max date - filing date", "Role-classified: expiry - start"),
    N       = c(sum(!is.na(.tab$DurationPaper)), sum(!is.na(.tab$DurationNew))),
    Mean    = c(mean(.tab$DurationPaper, na.rm = TRUE), mean(.tab$DurationNew, na.rm = TRUE)),
    SD      = c(stats::sd(.tab$DurationPaper, na.rm = TRUE), stats::sd(.tab$DurationNew, na.rm = TRUE)),
    P50     = c(stats::median(.tab$DurationPaper, na.rm = TRUE),
                stats::median(.tab$DurationNew, na.rm = TRUE)),
    P99     = c(stats::quantile(.tab$DurationPaper, 0.99, na.rm = TRUE),
                stats::quantile(.tab$DurationNew, 0.99, na.rm = TRUE)),
    Max     = c(max(.tab$DurationPaper, na.rm = TRUE), max(.tab$DurationNew, na.rm = TRUE))
  )

  cli::cli_h2("Contract term, in years")
  clf_say_table(.tab = dplyr::mutate(cmp_, dplyr::across(where(is.numeric), \(.x) round(.x, 2))))
  cli::cli_alert_info(
    "The published measure takes the latest date anywhere in the document, so a statutory reference \\
     or a long-dated maturity enters the term of a short contract. Read the standard deviation and \\
     the maximum together: a referee put the first at 46.8 years on debt contracts, which is the \\
     symptom this comparison exists to show."
  )
  invisible(cmp_)
}

#' Whether the places found are the locations of the contracting entities
#' @param .tab Tibble from ent_assemble().
#' @return Invisibly the role-mix tibble.
ent_report_places <- function(.tab) {
  if (FALSE) .tab <- tab_vars

  mix_ <- .tab |>
    dplyr::summarise(dplyr::across(
      c(NPartyAddress, NIncorporation, NGoverningLaw, NPerformance, NIncidental, NUnassigned),
      \(.x) sum(.x, na.rm = TRUE)
    )) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Role", values_to = "N") |>
    dplyr::mutate(Share = .data$N / sum(.data$N)) |>
    dplyr::arrange(dplyr::desc(.data$N))

  cli::cli_h2("Place occurrences by role")
  clf_say_table(.tab = dplyr::mutate(mix_, Share = clf_pct(.data$Share)))
  cli::cli_alert_info(
    "A reviewer asked whether the countries counted in the published geography are the locations of \\
     the contracting entities or mentions of somewhere else. This table is the answer, and it is an \\
     answer about OCCURRENCES: the same place name serves several roles inside one contract, so a \\
     per-document location would be the wrong object."
  )
  invisible(mix_)
}

#' The resolved variables against the facts EDGAR recorded
#' @param .tab Tibble from ent_assemble().
#' @return Invisibly the validation tibble.
ent_report_validation <- function(.tab) {
  if (FALSE) .tab <- tab_vars

  val_ <- tibble::tibble(
    Check = c("Party set contains the filer",
              "Contract start at or before the filing date",
              "Contract end after the contract start",
              "At least two parties"),
    N     = c(sum(.tab$HasFilerParty, na.rm = TRUE),
              sum(.tab$ContractStart <= .tab$DateFiled, na.rm = TRUE),
              sum(.tab$ContractEnd > .tab$ContractStart, na.rm = TRUE),
              sum(.tab$NParties >= 2L, na.rm = TRUE)),
    Of    = c(sum(!is.na(.tab$HasFilerParty)),
              sum(!is.na(.tab$ContractStart) & !is.na(.tab$DateFiled)),
              sum(!is.na(.tab$ContractEnd) & !is.na(.tab$ContractStart)),
              nrow(.tab))
  ) |>
    dplyr::mutate(Share = .data$N / .data$Of)

  cli::cli_h2("Variables against the EDGAR facts")
  clf_say_table(.tab = dplyr::mutate(val_, Share = clf_pct(.data$Share)))
  cli::cli_alert_info(
    "The first row is the one that carries weight: the filer is a party to its own contract, so a \\
     party set that omits it is missing a party that is certainly there. The third is arithmetic \\
     rather than evidence and should be at 100%."
  )
  invisible(val_)
}

#' Every 04C report block, in order
#' @param .n_deployed,.n_roles Integer counts from the build steps.
#' @param .tab Tibble from ent_assemble().
#' @return Invisibly NULL.
ent_report_all_resolve <- function(.n_deployed, .n_roles, .tab) {
  if (FALSE) {
    .n_deployed <- n_deployed
    .n_roles    <- n_roles
    .tab        <- tab_vars
  }
  ent_report_resolve(.n_deployed = .n_deployed, .n_roles = .n_roles, .tab = .tab)
  ent_report_duration(.tab = .tab)
  ent_report_places(.tab = .tab)
  ent_report_validation(.tab = .tab)
  invisible(NULL)
}
