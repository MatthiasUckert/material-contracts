# 04C-EntityResolve: turn spans into the four variables the paper reports --------------------------------------------
#
# WHAT THIS FILE DOES, IN ONE PARAGRAPH
# 04A extracted candidates and described them. 04B measured which engine to keep, which window to
# read, and which of the reading sessions' rules survive contact with the EDGAR anchors. Neither
# produced a variable. This file does: it applies the settled policy to the candidate store,
# assigns a role to each surviving span, and collapses those into one row per contract carrying the
# parties, the contract dates, the party locations and what can be said about value.
#
# IT RESOLVES WHAT 04D WILL PRODUCE, NOT WHAT 04A EXTRACTED
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
# WHERE A ROLE COMES FROM, AND WHY THE TIE-BREAK IS AN ARGUMENT
# Several cues can fire on one span with different roles, so one must win. That choice is a
# methodological one, it is not obviously settled, and it is therefore a named argument with the
# alternatives measured against each other rather than a rule baked into the query. See
# ent_assign_roles() for what each does and ent_compare_tiebreak() for what it changes.
#
# WHAT THIS FILE CANNOT SETTLE
# Nothing here validates that the variables are RIGHT. The anchors confirm that the filer appears
# among the resolved parties and that the contract date precedes its filing, which establishes that
# the extraction finds real things and not that the party count, the term or the value are correct.
# That needs a document-level gold set, and it is the one piece of this family that cannot be built
# from EDGAR facts or a reading session.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .db_path    <- .lP$Input$Store
  .path_text  <- .lP$Input$Text
  .path_rules <- .lP$Input$Kept
}


# 0. Vocabulary ------------------------------------------------------------------------------------------------------
# 03A registers the contract-type taxonomies and 04A the entity vocabularies, and this document
# sources both. Roles are the one axis appearing here that neither registers, and they stay
# unregistered on purpose: they are a per-label vocabulary -- ORG has five, MONEY seven, and they do
# not overlap -- so one flat key would order them arbitrarily and five keys would serve one figure.
# Where a role reaches an axis it carries the order the data gives it.


# 1. The deployed candidate set --------------------------------------------------------------------------------------


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


# 2. Roles -----------------------------------------------------------------------------------------------------------

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
#' @param .min_per_doc Numeric. Abort below this many terms per document. Per document rather than
#'   in total, because the same function runs over the 4,398-document sample in 04C and over chunks
#'   of five hundred in 04D, and an absolute floor calibrated on the first fires on every one of the
#'   second. A sample of contracts defining no terms in parentheses is not a finding, it is a broken
#'   pattern, and the version before this reported exactly that as a success.
#' @return Invisibly the row count of the table created.
ent_defined_terms <- function(.con, .path_text, .min_per_doc = 0.5) {
  if (FALSE) {
    .con         <- con
    .path_text   <- .lP$Input$Text
    .min_per_doc <- 0.5
  }

  # Straight and curly double quotes, built from code points: DuckDB's regex rejects \u escapes
  # and the house rule keeps the characters out of the source. The straight single quote is
  # deliberately absent -- it would terminate the SQL literal, and contracts do not use it here.
  q_ <- paste0("[", intToUtf8(c(0x22, 0x201C, 0x201D)), "]")

  # ESCAPING IS ONE LEVEL, NOT TWO. An earlier version doubled it -- "\\\\(" in the source rather
  # than "\\(" -- which reaches the SQL literal as an escaped backslash followed by a group opener,
  # so the pattern demanded a literal backslash before every parenthesis. It matched nothing, in
  # 4,398 contracts, and reported success with a count of zero. Every other regex in this family
  # uses the single form; this one now matches them.
  rx_ <- paste0("\\(\\s*(?:the\\s+)?", q_, "?([A-Z][A-Za-z]{2,28})", q_, "?\\s*\\)")

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE defterms AS ",
    "SELECT DISTINCT DocID, upper(unnest(regexp_extract_all(TextRaw, '", rx_, "', 1))) AS Term ",
    "FROM read_parquet('", as.character(fs::path_abs(.path_text)), "')"
  ))
  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM defterms")$N

  # A CONTRACT CORPUS WITHOUT PARENTHESISED DEFINED TERMS DOES NOT EXIST. Every agreement opens by
  # defining its parties -- (the "Company"), (the "Purchaser") -- so a low count is a broken pattern
  # rather than a finding, and the only reason the doubled escaping survived is that zero was
  # reported as a success.
  #
  # THE FLOOR IS PER DOCUMENT, NOT ABSOLUTE, and the first version was not. An absolute floor
  # calibrated on the 4,398-document sample fires on every chunk in 04D, where the same function
  # runs over five hundred documents at a time and a perfectly healthy count is two orders of
  # magnitude smaller. A guard that cannot tell a small input from a broken pattern is a guard that
  # gets deleted the first time it is inconvenient.
  #
  # The threshold is far below any plausible true value -- the sample runs about three and a half
  # terms per document -- because it exists to catch a pattern matching NOTHING, not to police the
  # yield.
  n_docs_ <- DBI::dbGetQuery(.con, paste0(
    "SELECT COUNT(*) AS N FROM read_parquet('", as.character(fs::path_abs(.path_text)), "')"
  ))$N
  per_doc_ <- n_ / max(1L, n_docs_)
  if (per_doc_ < .min_per_doc) {
    cli::cli_abort(c(
      "Only {n_} defined term{?s} over {n_docs_} document{?s}, {round(per_doc_, 2)} each.",
      "i" = "Every contract defines its parties in parentheses; the pattern is not matching.",
      "x" = "Check the escaping in {.arg rx_}: one level of backslashes, not two.",
      "i" = "The sample runs about 3.5 per document; the floor is {(.min_per_doc)}."
    ))
  }
  cli::cli_alert_success(
    "Defined terms: {n_} over {n_docs_} document{?s} ({round(per_doc_, 1)} each)"
  )
  invisible(n_)
}

#' Assign a role to each deployed span from the measured rules
#'
#' Cues are matched as literal phrases in a window on the stated side, exactly as 04B scored them.
#' Stops are applied as whole-span equality and drop the span outright; the defined-term list does
#' the same for organisations. A span no cue reaches keeps a NULL role and is carried rather than
#' discarded, because for parties the preamble position is itself evidence and the party resolver
#' uses it.
#'
#' THE TIE-BREAK, WHICH WAS WRONG AND IS THE REASON THE TERM VARIABLE WAS EMPTY.
#'
#' Where several cues fire on one span with different roles, one has to win. The earlier rule was
#' highest measured lift, described as the only non-arbitrary choice available. It is not
#' non-arbitrary; it is systematically biased, and in one direction.
#'
#' 04B judges a cue in a DIRECTION. A `signing` cue is validated by lift ABOVE the base rate,
#' because the anchor is the contract's own date and a signing cue should concentrate on it. An
#' `expiry` cue is validated by lift BELOW it, because an expiry date falls after the filing and a
#' correct expiry cue must therefore AVOID the anchor. So the two groups carry high and low lift by
#' construction, and ranking on raw lift descending means an expiry cue can never win a contested
#' span. It wins only spans no signing cue reaches, which is why the role landed on 22 documents of
#' 4,398 while the same cues fire in their thousands.
#'
#' PROXIMITY REPLACES IT. The nearest matching cue wins. This needs no scale on which a
#' high-direction lift and a low-direction lift are comparable -- and none honestly exists, since
#' the two measure different things -- and it answers the question actually being asked, which is
#' not which cue is best but which cue is ABOUT this span. The rules support it: the seven expiry
#' cues are declared at windows of 15 to 45 characters and six of the seven are left-sided, while
#' the signing set includes block-level phrases declared at 240 -- "sincerely", "ladies and
#' gentlemen". A date in a signature block sits within 35 characters of "shall continue through"
#' and within 240 of "sincerely"; under lift the second won, under proximity the first does.
#'
#' Ties in distance break on the narrower DECLARED window, which is the reading session's own
#' statement of how specific the cue is: a phrase declared at 15 characters asserts more about what
#' abuts it than one declared at 240.
#'
#' .tiebreak is an argument rather than a fixed rule because the procedure is still under
#' discussion. "lift" reproduces the earlier behaviour exactly, which is what makes the change
#' auditable rather than merely asserted.
#'
#' @param .con Session with deployed built.
#' @param .path_text Canonical text parquet.
#' @param .rules Tibble of rules kept by 04B, carrying Pattern, Side, Window, Role and Lift.
#' @param .ctx_max Widest context materialised, capping the reach of any rule.
#' @param .tiebreak How to choose among cues firing on one span with different roles. "proximity"
#'   is nearest-first, then narrowest declared window. "window" is narrowest declared window alone,
#'   which needs no position arithmetic. "lift" is the superseded rule, retained so the difference
#'   it makes can be measured rather than argued.
#' @return Invisibly the row count of the table created.
ent_assign_roles <- function(.con, .path_text, .rules, .ctx_max = 400L,
                             .tiebreak = c("proximity", "window", "lift")) {
  if (FALSE) {
    .con       <- con
    .path_text <- .lP$Input$Text
    .rules     <- tab_rules
    .ctx_max   <- 400L
    .tiebreak  <- "proximity"
  }
  .tiebreak <- match.arg(.tiebreak)

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

  # DISTANCE FROM THE SPAN TO THE CUE, in characters, measured from the right end of the left
  # context and the left end of the right one -- both of which abut the span. strpos() returns the
  # FIRST occurrence, so on the left the arithmetic is inverted: a cue occurring late in the left
  # context is the one closest to the span, and it is the LAST occurrence that matters. Reversing
  # the string turns "last occurrence" into "first occurrence" and the position into the distance.
  #
  # ALIASES: k is the candidate with its context, c is the cue. Getting these the wrong way round is
  # caught by the binder rather than producing a wrong answer, which is the one mercy of SQL.
  #
  # THE SIDE IS RESPECTED rather than both sides always being measured. A left-side cue that also
  # happens to occur to the right of the span has not fired -- the rule says left -- and taking the
  # nearer of the two would let an accidental occurrence on the wrong side win the tie-break with a
  # distance the rule never claimed. Only a two-sided rule takes the nearer of the two.
  dist_left_  <- paste0("CASE WHEN contains(right(k.LeftCtx, c.Window), c.Pattern) ",
                        "THEN strpos(reverse(right(k.LeftCtx, c.Window)), reverse(c.Pattern)) - 1 ",
                        "END")
  dist_right_ <- paste0("CASE WHEN contains(left(k.RightCtx, c.Window), c.Pattern) ",
                        "THEN strpos(left(k.RightCtx, c.Window), c.Pattern) - 1 END")
  dist_ <- paste0(
    "CASE c.Side ",
    "  WHEN 'left'  THEN ", dist_left_, " ",
    "  WHEN 'right' THEN ", dist_right_, " ",
    "  ELSE least(coalesce(", dist_left_, ", 999999), ",
    "             coalesce(", dist_right_, ", 999999)) END"
  )

  order_ <- switch(
    .tiebreak,
    proximity = "ORDER BY Dist ASC, c.Window ASC, c.Role",
    window    = "ORDER BY c.Window ASC, Dist ASC, c.Role",
    lift      = "ORDER BY c.RuleLift DESC, c.Role"
  )

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE roles AS ",
    "WITH kept AS ( ",
    "  SELECT * FROM spanctx sc ",
    "  WHERE NOT EXISTS (SELECT 1 FROM stops st ",
    "                    WHERE st.Label = sc.Label AND sc.SpanNorm = upper(st.Pattern)) ",
    "    AND NOT (sc.Label = 'ORG' AND EXISTS (SELECT 1 FROM defterms dt ",
    "             WHERE dt.DocID = sc.DocID AND dt.Term = sc.SpanNorm))), ",
    "hit AS ( ",
    "  SELECT k.DocID, k.Label, k.Start, k.Stop, c.Role, c.RuleLift, ",
    "         ", dist_, " AS Dist ",
    "  FROM kept k JOIN cues c USING (Label) ",
    "  WHERE CASE c.Side ",
    "    WHEN 'left'  THEN contains(right(k.LeftCtx,  c.Window), c.Pattern) ",
    "    WHEN 'right' THEN contains(left(k.RightCtx,  c.Window), c.Pattern) ",
    "    ELSE contains(right(k.LeftCtx, c.Window), c.Pattern) ",
    "         OR contains(left(k.RightCtx, c.Window), c.Pattern) END ",
    "  QUALIFY row_number() OVER (PARTITION BY k.DocID, k.Label, k.Start, k.Stop ",
    "                             ", order_, ") = 1) ",
    "SELECT k.DocID, k.Label, k.Start, k.Stop, k.Span, k.SpanNorm, k.LabelRaw, k.Pos, ",
    "       h.Role, h.RuleLift, h.Dist ",
    "FROM kept k LEFT JOIN hit h USING (DocID, Label, Start, Stop)"
  ))
  n_ <- DBI::dbGetQuery(.con, paste0(
    "SELECT COUNT(*) AS N, SUM(CASE WHEN Role IS NOT NULL THEN 1 ELSE 0 END) AS NRole FROM roles"
  ))
  cli::cli_alert_success(
    "Roles assigned by {(.tiebreak)}: {n_$NRole} of {n_$N} deployed span{?s} carry one"
  )
  invisible(n_$N)
}

#' What the tie-break rule changes, role by role
#'
#' The change above is a methodological one, so it is measured rather than asserted. Each rule is
#' applied to the same candidate set and the resulting role distributions compared. A rule that
#' moved nothing would not need arguing for; a rule that moves the expiry role from tens of
#' documents to thousands is the finding.
#'
#' Rebuilds the roles table once per rule and leaves it under the rule named LAST, so the caller
#' passes its chosen rule last and nothing downstream depends on a restore executing.
#'
#' @param .con Session with deployed and defterms built.
#' @param .path_text Canonical text parquet.
#' @param .rules Tibble of rules kept by 04B.
#' @param .ctx_max Widest context materialised.
#' @param .tiebreaks Rules to compare, in order; the last one is left in place.
#' @return Tibble: Tiebreak, Label, Role, NSpans, NDocs.
ent_compare_tiebreak <- function(.con, .path_text, .rules, .ctx_max = 400L,
                                 .tiebreaks = c("lift", "window", "proximity")) {
  if (FALSE) {
    .con       <- con
    .path_text <- .lP$Input$Text
    .rules     <- tab_rules
    .ctx_max   <- 400L
    .tiebreaks <- c("lift", "window", "proximity")
  }

  purrr::map(.tiebreaks, function(.tb) {
    ent_assign_roles(
      .con = .con, .path_text = .path_text, .rules = .rules,
      .ctx_max = .ctx_max, .tiebreak = .tb
    )
    DBI::dbGetQuery(.con, paste0(
      "SELECT '", .tb, "' AS Tiebreak, Label, Role, COUNT(*) AS NSpans, ",
      "       COUNT(DISTINCT DocID) AS NDocs ",
      "FROM roles WHERE Role IS NOT NULL GROUP BY Label, Role"
    )) |>
      tibble::as_tibble()
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(dplyr::across(c(NSpans, NDocs), as.integer)) |>
    dplyr::arrange(.data$Label, .data$Role, .data$Tiebreak)
}


# 3. The four variables ----------------------------------------------------------------------------------------------

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


# 4. Coverage ceilings -----------------------------------------------------------------------------------------------
# What a window COULD deliver for a role the anchor cannot speak to, as against what the resolver
# above actually produced. The gap between the two is diagnostic rather than decorative: it
# separates a window problem from a cue problem from a tie-break problem, and until this existed
# the three were indistinguishable.

#' Coverage of a role under a range of windows, for the roles no anchor can speak to
#'
#' The character grid in 04B chooses a window on how often it recovers the ANCHOR, and the date
#' anchor is a start date. Nothing there measures whether an expiry date survives the same window,
#' so this reports coverage directly, and the window can be chosen on the quantity it is meant to
#' preserve.
#'
#' IT PREVIOUSLY COULD NOT ANSWER THAT QUESTION, and the failure was invisible because it produced a
#' flat line that looked like a result. Two independent faults:
#'
#' It read the `roles` table, which descends from `deployed`, which is already filtered to the
#' policy window -- 3,000 characters of head for dates. So the grid's 5,000 and 10,000 head rows
#' queried a table containing nothing past 3,000 and could not differ from the 3,000 row whatever
#' the truth was. Of twelve cells only those at or inside the deployed window could move at all.
#'
#' And it filtered on `Role`, which is assigned by the tie-break. While the tie-break ranked on raw
#' lift it stripped the expiry role systematically, so widening the window admitted spans whose role
#' was then removed by the same mechanism, and the count did not rise. The test measured the window
#' THROUGH the defect it was being used to rule out.
#'
#' Both are fixed here by going back to the source. Candidates come from the unfiltered store rather
#' than from `deployed`, so a wide head has something to find. Roles are recomputed against the
#' rules within this query rather than read from the assignment, so the grid does not depend on the
#' tie-break at all: a span counts when any cue for the role reaches it, which is the widest honest
#' reading and the right one for a coverage ceiling.
#'
#' The result is a CEILING: what a window could deliver, not what the deployed pipeline will. That
#' is the correct quantity for choosing a window, because a cap that cannot clear the ceiling is
#' disqualified whatever the resolver does afterwards.
#'
#' TWO CEILINGS, AND THE DIFFERENCE BETWEEN THEM MATTERS. With .policy left NULL the grid pools
#' every engine in the store, which is the true upper bound on what is recoverable at all. Passed
#' the policy, it restricts to the engines that will actually run -- plural, since the cheap rule
#' arms are unioned in regardless of the ranking -- which is the number comparable to what the
#' resolver produces. The first version pooled every engine and was read against a
#' resolver output produced by one of them, so the gap between the two figures mixed a window
#' effect with an engine effect and could not be attributed.
#'
#' Neither number is comparable to the resolved variable without one further allowance: this counts
#' a document when ANY cue for the role reaches a date, while the role assignment is winner takes
#' all, so a span on which the expiry cue loses its tie-break counts here and not there. That gap is
#' a property of the tie-break rather than of the window, and ent_compare_tiebreak() is where it is
#' measured.
#'
#' @param .con Session with lens built and the store attached as s.
#' @param .path_text Canonical text parquet, supplying the context the cues are matched in.
#' @param .rules Tibble of rules kept by 04B.
#' @param .label Entity label.
#' @param .roles Roles counted as covered.
#' @param .head,.tail Integer character budgets to evaluate.
#' @param .ctx_max Widest context materialised, as 04B scored the rules with.
#' @param .policy Tibble from ent_read_policy(), or NULL. NULL pools every engine in the store.
#' @return Tibble: Head, Tail, NDocs, PctDocs, Engines. PctDocs is over all documents in the sample.
ent_role_coverage <- function(.con, .path_text, .rules, .label, .roles,
                              .head = c(3000L, 5000L, 10000L, 20000L, 100000000L),
                              .tail = c(0L, 2000L, 5000L, 10000L),
                              .ctx_max = 400L, .policy = NULL) {
  if (FALSE) {
    .con       <- con
    .path_text <- .lP$Input$Text
    .rules     <- tab_rules
    .label     <- "DATE"
    .roles     <- "expiry"
    .head      <- c(3000L, 5000L, 10000L, 20000L, 100000000L)
    .tail      <- c(0L, 2000L, 5000L, 10000L)
    .ctx_max   <- 400L
    .policy    <- tab_policy
  }

  cues_ <- .rules |>
    dplyr::filter(
      .data$Kind == "cue", .data$Label == .label, .data$Role %in% .roles,
      !is.na(.data$Pattern)
    ) |>
    dplyr::select(Label, Role, Pattern, Side, Window)
  if (nrow(cues_) == 0L) {
    cli::cli_abort("No kept cues for {.val {(.label)}} role{?s} {(.roles)}.")
  }
  ent_put_table(.con = .con, .name = "rcues", .tab = cues_)
  ent_put_table(.con = .con, .name = "rgrid",
                .tab = tidyr::expand_grid(Head = as.integer(.head), Tail = as.integer(.tail)))

  # From the store UNFILTERED BY WINDOW, so a head wider than the deployed cap has candidates to
  # find. The engine filter is separate and optional; see the note above on the two ceilings.
  eng_ <- ""
  if (!is.null(.policy)) {
    pol_ <- .policy |>
      dplyr::filter(.data$Label == .label) |>
      dplyr::mutate(
        Engine = sub(":.*$", "", .data$Combo),
        Model  = dplyr::if_else(grepl(":", .data$Combo, fixed = TRUE),
                                sub("^[^:]*:", "", .data$Combo), .data$Combo)
      )
    # A label can carry more than one engine: the cheap rule arms are unioned in regardless of the
    # ranking, so restricting to "the" deployed engine would be restricting to whichever row the
    # policy happened to list first. The filter takes the whole deployed set for the label.
    if (nrow(pol_) == 0L) cli::cli_abort("Policy has no row for {.val {(.label)}}.")
    pairs_ <- paste0("(c.Engine = '", pol_$Engine, "' AND c.Model = '", pol_$Model, "')",
                     collapse = " OR ")
    eng_   <- paste0(" AND (", pairs_, ")")
  }

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE rcov AS ",
    "SELECT DISTINCT c.DocID, c.Start, c.Stop, l.DocLen, ",
    "  lower(regexp_replace(substring(t.TextRaw, greatest(1, c.Start + 1 - ", as.integer(.ctx_max),
    "), least(", as.integer(.ctx_max), ", c.Start)), '\\\\s+', ' ', 'g')) AS LeftCtx, ",
    "  lower(regexp_replace(substring(t.TextRaw, c.Stop + 1, ", as.integer(.ctx_max),
    "), '\\\\s+', ' ', 'g')) AS RightCtx ",
    "FROM s.candidates c JOIN lens l USING (DocID) ",
    "JOIN read_parquet('", as.character(fs::path_abs(.path_text)), "') t USING (DocID) ",
    "WHERE c.Label = '", .label, "' AND c.Start IS NOT NULL", eng_
  ))

  DBI::dbGetQuery(.con, paste0(
    "WITH hit AS ( ",
    "  SELECT DISTINCT r.DocID, r.Start, r.Stop, r.DocLen ",
    "  FROM rcov r JOIN rcues q ON TRUE ",
    "  WHERE CASE q.Side ",
    "    WHEN 'left'  THEN contains(right(r.LeftCtx,  q.Window), q.Pattern) ",
    "    WHEN 'right' THEN contains(left(r.RightCtx,  q.Window), q.Pattern) ",
    "    ELSE contains(right(r.LeftCtx, q.Window), q.Pattern) ",
    "         OR contains(left(r.RightCtx, q.Window), q.Pattern) END), ",
    "tot AS (SELECT COUNT(*) AS N FROM lens) ",
    "SELECT g.Head, g.Tail, ",
    "  COUNT(DISTINCT CASE WHEN h.Stop <= g.Head OR h.Start >= h.DocLen - g.Tail ",
    "        THEN h.DocID END) AS NDocs, ",
    "  COUNT(DISTINCT CASE WHEN h.Stop <= g.Head OR h.Start >= h.DocLen - g.Tail ",
    "        THEN h.DocID END) / CAST(any_value(tot.N) AS DOUBLE) AS PctDocs ",
    "FROM rgrid g LEFT JOIN hit h ON TRUE, tot GROUP BY g.Head, g.Tail ORDER BY g.Head, g.Tail"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(c(Head, Tail, NDocs), as.integer),
      Engines = if (is.null(.policy)) "all in store" else "deployed"
    )
}

#' Which engine's candidates the role cues can actually reach
#'
#' THE ENGINE WAS CHOSEN ON A QUANTITY THIS ROLE CANNOT SEE, and that is what this measures. 04B
#' ranks engines on recall against the EDGAR anchor, and for dates the anchor is the contract's own
#' date, which sits in the preamble. An engine excellent at the preamble formula and blind to term
#' clauses scores well and delivers nothing for expiry -- the same structural criticism the window
#' already carries, applied to the engine.
#'
#' Reported over full text, because a window is a second question and mixing the two is what made
#' the first version of the coverage grid unreadable. One row per engine in the store, so the
#' deployed engine can be read against the alternatives rather than against a pooled total that
#' includes it.
#'
#' This does not by itself justify changing an engine. Reaching more dates is recall, and the
#' precision of those reaches is unmeasured here as everywhere else in this family; an engine
#' proposing five times the spans may reach five times the expiry dates and be no better. Read it
#' beside SpansPerAnchor from 04B, which is the cost of that breadth.
#'
#' @param .con Session with lens built and the store attached as s.
#' @param .path_text Canonical text parquet, supplying the context the cues are matched in.
#' @param .rules Tibble of rules kept by 04B.
#' @param .label Entity label.
#' @param .roles Roles counted as covered.
#' @param .ctx_max Widest context materialised, as 04B scored the rules with.
#' @return Tibble: Combo, NSpans, NDocs, PctDocs, one row per engine in the store.
ent_role_by_engine <- function(.con, .path_text, .rules, .label, .roles, .ctx_max = 400L) {
  if (FALSE) {
    .con       <- con
    .path_text <- .lP$Input$Text
    .rules     <- tab_rules
    .label     <- "DATE"
    .roles     <- "expiry"
    .ctx_max   <- 400L
  }

  cues_ <- .rules |>
    dplyr::filter(
      .data$Kind == "cue", .data$Label == .label, .data$Role %in% .roles, !is.na(.data$Pattern)
    ) |>
    dplyr::select(Label, Role, Pattern, Side, Window)
  if (nrow(cues_) == 0L) {
    cli::cli_abort("No kept cues for {.val {(.label)}} role{?s} {(.roles)}.")
  }
  ent_put_table(.con = .con, .name = "ecues", .tab = cues_)

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE ecov AS ",
    "SELECT DISTINCT ",
    "  CASE WHEN c.Engine = c.Model THEN c.Engine ELSE c.Engine || ':' || c.Model END AS Combo, ",
    "  c.DocID, c.Start, c.Stop, ",
    "  lower(regexp_replace(substring(t.TextRaw, greatest(1, c.Start + 1 - ", as.integer(.ctx_max),
    "), least(", as.integer(.ctx_max), ", c.Start)), '\\\\s+', ' ', 'g')) AS LeftCtx, ",
    "  lower(regexp_replace(substring(t.TextRaw, c.Stop + 1, ", as.integer(.ctx_max),
    "), '\\\\s+', ' ', 'g')) AS RightCtx ",
    "FROM s.candidates c ",
    "JOIN read_parquet('", as.character(fs::path_abs(.path_text)), "') t USING (DocID) ",
    "WHERE c.Label = '", .label, "' AND c.Start IS NOT NULL"
  ))

  DBI::dbGetQuery(.con, paste0(
    "WITH hit AS ( ",
    "  SELECT DISTINCT e.Combo, e.DocID, e.Start, e.Stop ",
    "  FROM ecov e JOIN ecues q ON TRUE ",
    "  WHERE CASE q.Side ",
    "    WHEN 'left'  THEN contains(right(e.LeftCtx,  q.Window), q.Pattern) ",
    "    WHEN 'right' THEN contains(left(e.RightCtx,  q.Window), q.Pattern) ",
    "    ELSE contains(right(e.LeftCtx, q.Window), q.Pattern) ",
    "         OR contains(left(e.RightCtx, q.Window), q.Pattern) END), ",
    "tot AS (SELECT COUNT(*) AS N FROM lens) ",
    "SELECT h.Combo, COUNT(*) AS NSpans, COUNT(DISTINCT h.DocID) AS NDocs, ",
    "  COUNT(DISTINCT h.DocID) / CAST(any_value(tot.N) AS DOUBLE) AS PctDocs ",
    "FROM hit h, tot GROUP BY h.Combo ORDER BY NDocs DESC"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(c(NSpans, NDocs), as.integer))
}


# 5. Assembly and validation -----------------------------------------------------------------------------------------

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


# 6. Report ----------------------------------------------------------------------------------------------------------

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
  tbl_say(
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
  tbl_say(.tab = dplyr::mutate(cmp_, dplyr::across(where(is.numeric), \(.x) round(.x, 2))))
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
  tbl_say(.tab = dplyr::mutate(mix_, Share = tbl_pct(.data$Share)))
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
  tbl_say(.tab = dplyr::mutate(val_, Share = tbl_pct(.data$Share)))
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


# 7. Figures ---------------------------------------------------------------------------------------------------------
# Two shapes, both through the shared design layer. They were inline ggplot in the runbook, on
# theme_bw() and at hand-typed heights, which is why they are here now: a figure that will appear in
# the paper should not be defined in the document that happens to display it.

#' The contract term, computed both ways
#'
#' THE COMPARISON IS THE ARGUMENT, so the two measures are drawn on one canvas rather than described
#' in sequence. The published definition takes the latest date anywhere in a document and subtracts
#' the filing date, which admits a statutory reference to an Act of 1933 and a maturity in 2099 into
#' the term of a two-year contract. The role-classified measure can only see dates a cue reached,
#' which is a narrower set and a much smaller one.
#'
#' The axis is truncated, and that is itself the finding rather than a presentational convenience:
#' the published measure runs to thousands of years, so a figure showing its full range would be a
#' single bar at the origin and a horizontal rule. The truncation is stated in the caption and the
#' untruncated moments are in the table above it.
#'
#' Free vertical scales because the two measures differ by two orders of magnitude in count. A
#' shared scale would render the role-classified panel as an empty box, which is a fair description
#' of its coverage and a useless one of its shape.
#'
#' @param .tab Assembled variables from ent_assemble().
#' @param .max_years Numeric. Upper bound of the axis.
#' @param .bins Integer. Histogram bins.
#' @return A ggplot.
ent_plot_duration <- function(.tab, .max_years = 60, .bins = 60L) {
  if (FALSE) {
    .tab       <- tab_vars
    .max_years <- 60
    .bins      <- 60L
  }

  .tab |>
    dplyr::select(DocID, DurationPaper, DurationNew) |>
    tidyr::pivot_longer(-DocID, names_to = "Measure", values_to = "Years") |>
    dplyr::filter(!is.na(.data$Years), .data$Years > -1, .data$Years < .max_years) |>
    dplyr::mutate(Measure = factor(
      .data$Measure,
      levels = c("DurationPaper", "DurationNew"),
      labels = c("Published: latest date less filing", "Role-classified: expiry less start")
    )) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Years)) +
    ggplot2::geom_histogram(bins = .bins, fill = .plot_ink, colour = NA) +
    ggplot2::facet_wrap(~Measure, nrow = 1, scales = "free_y") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.02))) +
    plot_scale_y_count() +
    ggplot2::labs(x = "Contract term (years)", y = "Documents") +
    plot_theme(.grid = "y", .legend = "none")
}

#' What roles the extracted places are serving, by contract type
#'
#' The answer to the question a referee asked of the published geography: whether the countries
#' counted are the locations of the contracting entities or mentions of somewhere else. It is an
#' answer about OCCURRENCES rather than documents, because the same place name serves several roles
#' inside one contract -- a reading session found "New York" as a party address, as the governing
#' law, as a court venue and inside an exchange name in a single document -- so a per-document
#' location would be the wrong object to report.
#'
#' Roles are ordered by total volume rather than alphabetically, and the unassigned bucket is forced
#' last whatever its size. It is the largest category and it is not a finding; leaving it in rank
#' order would put the one uninformative segment at the start of every bar.
#'
#' @param .tab Assembled variables from ent_assemble().
#' @param .key_class Registration key ordering the contract types.
#' @return A ggplot.
ent_plot_place_roles <- function(.tab, .key_class = "ClassDetailed") {
  if (FALSE) {
    .tab       <- tab_vars
    .key_class <- "ClassDetailed"
  }

  long_ <- .tab |>
    dplyr::select(Class, NPartyAddress, NIncorporation, NGoverningLaw, NPerformance,
                  NIncidental, NUnassigned) |>
    tidyr::pivot_longer(-Class, names_to = "Role", values_to = "N") |>
    dplyr::summarise(N = sum(.data$N, na.rm = TRUE), .by = c(Class, Role)) |>
    dplyr::mutate(Share = .data$N / sum(.data$N), .by = Class)

  ord_ <- long_ |>
    dplyr::summarise(Total = sum(.data$N), .by = Role) |>
    dplyr::arrange(dplyr::desc(.data$Total)) |>
    dplyr::pull(Role)
  ord_ <- c(setdiff(ord_, "NUnassigned"), "NUnassigned")

  long_ |>
    dplyr::mutate(
      PlotClass = plot_factor(.data$Class, .key = .key_class, .short = TRUE, .rev = TRUE),
      PlotRole  = factor(.data$Role, levels = ord_, labels = sub("^N", "", ord_))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Share, y = .data$PlotClass, fill = .data$PlotRole)) +
    # reverse = TRUE, because geom_col() stacks the LAST factor level first and would put the
    # unassigned bucket at the start of every bar -- the one segment carrying no information
    # occupying the position a reader's eye lands on. Reversed, the bar reads in legend order and
    # the residual sits at the end where a residual belongs.
    ggplot2::geom_col(width = 0.75, position = ggplot2::position_stack(reverse = TRUE)) +
    plot_scale_x_pct(.accuracy = 1, .expand = c(0, 0), .breaks = scales::breaks_pretty(n = 5)) +
    plot_scale_fill_cat() +
    ggplot2::labs(x = "Share of place occurrences", y = NULL, fill = NULL) +
    plot_theme(.grid = "x", .legend = "bottom")
}
