# 04B-EntityMeasure: score engines and rules against facts EDGAR already recorded --------------------------------------
#
# WHAT THIS FILE DOES, IN ONE PARAGRAPH
# 04A extracted candidates and described them; nothing was measured, because entity labels do not
# exist. This file supplies the missing judge. For every contract EDGAR records the filer's name,
# the filing date and the registered addresses, none of which was derived from the contract text,
# and each of which identifies at least one entity that must be present. That is enough to score an
# engine's recall, to score a proposed rule's precision, and to choose an extraction window --
# without a single annotation.
#
# WHAT THE ANCHOR IS AND WHAT IT IS NOT
# The anchor is one known-true entity per document, not a complete labelling. A span that fails to
# match it is unlabelled, not wrong: most organisations in a contract are counterparties, agents and
# third parties that EDGAR never recorded. So a rule's measured precision is the share of the spans
# it keeps that are the KNOWN entity, which is a lower bound on precision and a proxy that behaves
# well for ranking and badly as a level. Recall against the anchor is the honest number and needs no
# hedging: if an engine never proposes the filer's own name, it did not find it.
#
# ROLES THAT THE ANCHOR CANNOT SPEAK TO
# The filer is a party, so a cue for the `agent` or `regulator` role cannot be confirmed against it
# and would look worthless if it were scored as though it could. Only the roles in .ALIGNED are
# judged, and they are judged in a DIRECTION: for a high-direction role the pass condition is lift
# above the base rate, and for a low-direction role it is lift near zero, which is falsification
# rather than confirmation. The rest are reported on coverage alone and marked unmeasured. A `stop`
# rule is different and is scored universally: it claims a span is never the entity, so any stop
# that kills an anchor is refuted by its own definition, whatever role it carries.
#
# WHICH FOLD, AND WHY IT DIFFERS BY SECTION
# The reading session read folds 1-4, so rules are scored on fold 5 alone. Engine recall involves no
# rules and cannot be contaminated, so it uses all 4,398 documents and is the more precise for it.
# The two are reported separately and never averaged.
#
# WHAT LEAVES THIS FILE
# Four artifacts. anchor_hits is the per-span verdict, read by every section here and by 04D.
# rule_scores is the full record. rules_kept is the survivors, read by 04C and 04D. And
# extraction_policy is the decision: one engine per label, a window, and a Basis saying what kind
# of evidence stands behind each row. 04C and 04D read a decision rather than re-deriving one.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

# Bracketed redaction markers in their symbol form only: "[***]", "[ * * * ]", "[*]". The explicit
# form, "[confidential treatment requested]", shares vocabulary with the money cues themselves, so
# testing one against the other would be circular.
.RX_REDACT_SYM <- "\\[[\\s*]+\\]"

if (FALSE) {
  .db_path      <- .lP$Input$Store
  .path_text    <- .lP$Input$Text
  .path_anchors <- .lP$Input$Anchors
  .path_hits    <- .lP$Output$Hits
}


# 0. Vocabulary ------------------------------------------------------------------------------------------------------
# 04A registers Label and Combo when its library is sourced, and this document sources it. Nothing
# is re-registered here: two registrations of one key would let the two documents order the same
# axis differently, which is the failure a shared registry exists to prevent.
#
# Roles are NOT registered. They are a per-label vocabulary -- ORG has five, MONEY has seven, and
# they do not overlap -- so a single flat key would order them arbitrarily and a per-label key would
# be five registrations serving one figure. Where a role appears on an axis it carries its own
# observed order.


# 1. Anchors ---------------------------------------------------------------------------------------------------------
# One known-true entity per document per label, derived from EDGAR rather than from the contract.

#' Reduce a company name to a matchable key
#'
#' The filer is recorded as a legal name and appears in the contract in whatever form the drafter
#' chose, so "ACME HOLDINGS, INC." has to reach "Acme Holdings". Punctuation goes, trailing legal
#' forms go, a leading article goes. Corporate-form words are stripped only from the END, because
#' "Trust" is a legal form in "Acme Trust" and part of the name in "Trust Bancorp".
#'
#' This is the authoritative version. 04B-CoWorkEntities.R carries its own copy so that the bundle
#' script stays standalone; the two must agree, or the contrast the session read and the scoring
#' applied to its answers are about different sets.
#'
#' @param .x Character vector of company names.
#' @return Character vector of keys, NA where nothing usable survives.
ent_norm_company <- function(.x) {
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

#' Copy an R table into DuckDB as a real table rather than leaving it registered
#'
#' A registered data frame is a view backed by the R runtime: every scan crosses back into R,
#' single-threaded, and a join against a multi-million-row table drags the whole join down that
#' path. Materialising costs one copy of a table that is at most a few hundred thousand rows and
#' removes the R runtime from the query plan entirely.
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

#' The SQL expression that turns a raw date span into something a parser can read
#'
#' Requiring the whole span to be a date discards most of them. The extractors return the date with
#' whatever abuts it -- "FEBRUARY 11, 2009 First Amendment to Agreement", "December 10, 2020Edward",
#' "JUNE 1, 2004 (the" -- and legal drafting adds forms no parser expects: ordinal suffixes, "the
#' 15th day of April, 2007", an abbreviated month carrying a full stop. Measured on the real sample,
#' whole-span matching resolved 6.6% of distinct date strings.
#'
#' So the date is located inside the span and then normalised, rather than the span being handed to
#' a parser whole. On a population shaped like the corpus this moves the resolved share from 20% to
#' 55%, and the strings still failing are the durations, bare numbers and boilerplate phrases that
#' are not dates at all.
#'
#' Two-digit years are deliberately not matched. A span reading 8/26/13 cannot be resolved without
#' a century convention, and inventing one would put spans in the anchor window by assumption.
#'
#' @param .col SQL expression yielding the raw span.
#' @return A SQL expression string.
ent_sql_dateclean <- function(.col = "Span") {
  if (FALSE) .col <- "Span"

  rx_ <- paste0(
    "([A-Za-z]{3,9}\\.?\\s+\\d{1,2}(st|nd|rd|th)?\\s*,?\\s*\\d{4})",     # January 5, 2019
    "|(\\d{1,2}(st|nd|rd|th)?\\s+(day\\s+of\\s+)?[A-Za-z]{3,9}\\.?,?\\s*\\d{4})",  # 15th day of April 2007
    "|(\\d{1,2}-[A-Za-z]{3,9}-\\d{4})",                                    # 13-Mar-2015
    "|(\\d{1,2}[/.-]\\d{1,2}[/.-]\\d{4})",                                 # 8/26/2013
    "|(\\d{4}[/-]\\d{1,2}[/-]\\d{1,2})"                                    # 2015/03/13
  )
  x_ <- paste0("regexp_extract(", .col, ", '", rx_, "', 0)")
  x_ <- paste0("regexp_replace(", x_, ", '(\\d)(st|nd|rd|th)', '\\1', 'gi')")   # 3rd -> 3
  x_ <- paste0("regexp_replace(", x_, ", 'day\\s+of\\s+', '', 'gi')")          # drop "day of"
  x_ <- paste0("regexp_replace(", x_, ", '([A-Za-z]{3})\\.', '\\1', 'g')")      # Oct. -> Oct
  x_ <- paste0("regexp_replace(", x_, ", '\\s*,\\s*', ', ', 'g')")             # tidy commas
  paste0("trim(regexp_replace(", x_, ", '\\s+', ' ', 'g'))")
}

#' The two-letter postal codes EDGAR stores, and the names contracts write
#'
#' @return Tibble: Code, State.
ent_state_codes <- function() {
  if (FALSE) NULL
  tibble::tibble(
    Code = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS",
             "KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC",
             "ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
             "PR","VI","GU"),
    State = c("ALABAMA","ALASKA","ARIZONA","ARKANSAS","CALIFORNIA","COLORADO","CONNECTICUT",
              "DELAWARE","DISTRICT OF COLUMBIA","FLORIDA","GEORGIA","HAWAII","IDAHO","ILLINOIS",
              "INDIANA","IOWA","KANSAS","KENTUCKY","LOUISIANA","MAINE","MARYLAND","MASSACHUSETTS",
              "MICHIGAN","MINNESOTA","MISSISSIPPI","MISSOURI","MONTANA","NEBRASKA","NEVADA",
              "NEW HAMPSHIRE","NEW JERSEY","NEW MEXICO","NEW YORK","NORTH CAROLINA","NORTH DAKOTA",
              "OHIO","OKLAHOMA","OREGON","PENNSYLVANIA","RHODE ISLAND","SOUTH CAROLINA",
              "SOUTH DAKOTA","TENNESSEE","TEXAS","UTAH","VERMONT","VIRGINIA","WASHINGTON",
              "WEST VIRGINIA","WISCONSIN","WYOMING","PUERTO RICO","VIRGIN ISLANDS","GUAM")
  )
}

#' Map bare two-letter codes to state names
#'
#' @param .x Character vector of codes.
#' @param .codes Tibble from ent_state_codes().
#' @return Character vector of names, NA where the code is unknown.
ent_state_name <- function(.x, .codes = ent_state_codes()) {
  if (FALSE) .x <- c("DE", "NY", "ZZ")
  .codes$State[match(stringi::stri_trans_toupper(trimws(.x)), .codes$Code)]
}

#' Append the full state names implied by an EDGAR address
#'
#' EDGAR stores an address as street, city, STATE, ZIP with the state as a two-letter code --
#' "ABBOTT PARK IL 60064", "THE WOODLANDS TX 77380". Contracts write "the State of Illinois". A
#' containment test between the two can therefore only ever fire through the CITY, and every state
#' mention in the contract is invisible to it. Since states are most of what a contract names
#' geographically, that alone would depress the anchor's yield by a large margin.
#'
#' The code is located by the ZIP that follows it rather than as a bare token, because bare
#' two-letter tokens are unsafe here: IN, OR, OK, ME, LA and DE are ordinary words, and ST, FL and
#' PO occur in street lines. A code immediately preceding a five-digit number is unambiguous, and it
#' resolved every address in the sample inspected.
#'
#' @param .x Character vector of upper-cased addresses.
#' @param .codes Tibble from ent_state_codes().
#' @return .x with the implied state names appended.
ent_expand_states <- function(.x, .codes = ent_state_codes()) {
  if (FALSE) .x <- "100 ABBOTT PARK ROAD ABBOTT PARK IL 60064-3500 8479376100"

  hits_ <- stringi::stri_extract_all_regex(.x, "\\b[A-Z]{2}(?=[ ]+[0-9]{5})")
  purrr::map2_chr(.x, hits_, function(.a, .h) {
    if (is.na(.a)) return(NA_character_)
    nm_ <- unique(ent_state_name(stats::na.omit(.h), .codes))
    nm_ <- nm_[!is.na(nm_)]
    if (length(nm_) == 0L) .a else paste(.a, paste(nm_, collapse = " "))
  })
}

#' Per-document anchor keys, one row per contract
#'
#' The state of incorporation is a separate fact from the address and is taken where EDGAR supplies
#' it. Delaware is the commonest place name in these contracts and almost none of these filers sit
#' there physically, so an address-only anchor cannot see it at all. The column is taken by any_of()
#' semantics because which export carries it is an environment fact; where it is absent the
#' incorporation role simply stays unmeasurable, which ent_report_anchors() makes visible.
#'
#' @param .path_anchors 04A's sample anchors parquet.
#' @param .cols_stateinc Candidate names for the state-of-incorporation column.
#' @return Tibble: DocID, Fold, Class, AnchorKey, AnchorText, DateFiled. Carries attr "StateIncCol".
ent_anchor_keys <- function(.path_anchors,
                            .cols_stateinc = c("StateInc", "StateOfIncorporation",
                                               "StateIncorporation", "StateOfIncorp")) {
  if (FALSE) {
    .path_anchors  <- .lP$Input$Anchors
    .cols_stateinc <- c("StateInc", "StateOfIncorporation")
  }

  raw_ <- arrow::read_parquet(.path_anchors)
  inc_ <- intersect(.cols_stateinc, names(raw_))
  inc_name_ <- if (length(inc_) > 0L) ent_state_name(as.character(raw_[[inc_[1]]])) else NA_character_

  out_ <- raw_ |>
    dplyr::transmute(
      DocID, Fold,
      Class      = .data$ClassDetailed,
      DateFiled  = suppressWarnings(anytime::anydate(as.character(.data$DateFiled))),
      AnchorKey  = ent_norm_company(.data$CompanyName),
      AnchorText = stringi::stri_trans_toupper(paste(
        dplyr::coalesce(.data$BusinessAddress, ""),
        dplyr::coalesce(.data$MailingAddress, "")
      ))
    ) |>
    dplyr::mutate(
      AnchorText = ent_expand_states(.x = .data$AnchorText),
      AnchorText = paste(.data$AnchorText, dplyr::coalesce(inc_name_, "")),
      AnchorText = dplyr::if_else(trimws(.data$AnchorText) == "", NA_character_, .data$AnchorText)
    )

  attr(out_, "StateIncCol") <- if (length(inc_) > 0L) inc_[1] else NA_character_
  if (length(inc_) == 0L) {
    cli::cli_alert_warning(
      "No state-of-incorporation column found; the GPE incorporation role stays unmeasurable."
    )
  }
  out_
}

#' Open an in-memory session with the candidate store attached read-only
#'
#' In memory rather than on the store itself, so temporary tables can be built freely without any
#' possibility of writing to an artifact 04A owns.
#'
#' @param .db_path Candidate store.
#' @param .keys Tibble from ent_anchor_keys().
#' @return A live DBI connection; the caller disconnects.
ent_session <- function(.db_path, .keys) {
  if (FALSE) {
    .db_path <- .lP$Input$Store
    .keys    <- tab_keys
  }
  con_ <- ner_db_connect()
  DBI::dbExecute(con_, paste0("ATTACH '", as.character(fs::path_abs(.db_path)),
                              "' AS s (READ_ONLY)"))
  ent_put_table(.con = con_, .name = "keys", .tab = dplyr::mutate(
    .keys, DateFiled = as.character(.data$DateFiled)
  ))
  con_
}

#' Flag every distinct candidate span as anchor-matched or not, and persist it
#'
#' Organisations and places are settled in SQL, where the test is string containment on a normalised
#' form. Dates need a parser, so the DISTINCT span strings are brought into R -- far fewer than the
#' occurrences -- parsed once, and registered back. Money has no anchor and every span is flagged
#' FALSE, which is honest rather than convenient: it says the label is unmeasurable here.
#'
#' The result is written to parquet because three separate analyses read it and 04D wants it too.
#' Recomputing it per analysis would be three passes over millions of rows for one answer.
#'
#' @param .con Session from ent_session().
#' @param .path_out Destination parquet, or NULL to build the in-session table only. The sensitivity
#'   check rebuilds this table under several windows and wants none of them on disk; passing NULL
#'   also means the written artifact can never end up holding a variant it was not configured for.
#' @param .window_days How far before the filing a contract date may sit.
#' @param .formats Character vector of strptime formats tried, in order, against a date span.
#' @return Invisibly the path written, or NULL.
ent_write_anchor_hits <- function(.con, .path_out, .window_days = 730L,
                                  .formats = .DATE_FORMATS) {
  if (FALSE) {
    .con         <- con
    .path_out    <- .lP$Output$Hits
    .window_days <- 730L
    .formats     <- .DATE_FORMATS
  }

  # The normalised span is computed ONCE here and carried, not recomputed inside the CASE. The
  # organisation test reads it twice, either side of an OR, and DuckDB evaluates an inline
  # expression per occurrence: measured on 400k rows, inline is roughly sixty times slower than a
  # materialised column. It is also read again when rule context is built, so this pays twice.
  norm_ <- paste0("trim(regexp_replace(regexp_replace(upper(Span), '[^A-Z0-9 ]', ' ', 'g'), ",
                  "'\\s+', ' ', 'g'))")

  # Dates are parsed here too, and in the database rather than in R. A general-purpose parser has
  # to try a long cascade of heuristics per string and exhausts all of them on every failure, which
  # is most of this input: measured on the real sample it managed 242 strings a second, so the
  # 197,930 distinct date spans would take fourteen minutes. Naming the formats this corpus
  # actually uses turns the same work into a vectorised pass of about a second.
  #
  # The order matters and is a choice rather than a detail. Month-first precedes day-first, so an
  # ambiguous slash date reads as US convention, which is right for an SEC filing; an unambiguous
  # day-first date such as 26/8/2013 still resolves, because 26 cannot be a month.
  fmt_ <- paste0("['", paste(.formats, collapse = "','"), "']")

  cli::cli_alert_info("Collapsing candidates to distinct spans and parsing dates ...")
  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE spans AS ",
    "SELECT DocID, Label, Start, Stop, Span, ", norm_, " AS SpanNorm, ",
    "  CASE WHEN Label = 'DATE' THEN try_strptime(", ent_sql_dateclean("Span"), ", ",
    fmt_, ") END AS SpanDate ",
    "FROM (SELECT DISTINCT DocID, Label, Start, Stop, Span ",
    "      FROM s.candidates WHERE Start IS NOT NULL)"
  ))
  n_ <- DBI::dbGetQuery(.con, paste0(
    "SELECT COUNT(*) AS N, ",
    "  COUNT(DISTINCT CASE WHEN Label = 'DATE' THEN Span END) AS NDate, ",
    "  COUNT(DISTINCT CASE WHEN SpanDate IS NOT NULL THEN Span END) AS NParsed FROM spans"
  ))
  cli::cli_alert_success(
    "{n_$N} distinct span{?s}; {n_$NParsed} of {n_$NDate} distinct date string{?s} resolved."
  )

  cli::cli_alert_info("Flagging anchors ...")
  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE anchor_hits AS ",
    # SpanDate travels with the table. It is what lets the date window be varied in a query rather
    # than by rebuilding anchor_hits, and it saves every consumer re-running the parse.
    "SELECT sp.DocID, sp.Label, sp.Start, sp.Stop, sp.Span, sp.SpanNorm, sp.SpanDate, ",
    "  CASE sp.Label ",
    "    WHEN 'ORG'  THEN COALESCE(k.AnchorKey IS NOT NULL ",
    "                    AND (contains(sp.SpanNorm, k.AnchorKey) ",
    "                         OR contains(k.AnchorKey, sp.SpanNorm)), FALSE) ",
    "    WHEN 'GPE'  THEN COALESCE(k.AnchorText IS NOT NULL AND length(sp.Span) >= 4 ",
    "                    AND contains(k.AnchorText, upper(sp.Span)), FALSE) ",
    "    WHEN 'DATE' THEN COALESCE(sp.SpanDate IS NOT NULL AND k.DateFiled IS NOT NULL ",
    "                    AND CAST(sp.SpanDate AS DATE) <= CAST(k.DateFiled AS DATE) ",
    "                    AND CAST(sp.SpanDate AS DATE) >= CAST(k.DateFiled AS DATE) - ",
    as.integer(.window_days), ", FALSE) ",
    "    ELSE FALSE END AS IsAnchor, ",
    "  k.Fold, k.Class ",
    "FROM spans sp JOIN keys k USING (DocID)"
  ))

  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM anchor_hits")$N
  if (is.null(.path_out)) {
    cli::cli_alert_info("Anchor hits rebuilt in session: {n_} distinct span{?s} (not written)")
    return(invisible(NULL))
  }
  fs::dir_create(fs::path_dir(.path_out))
  DBI::dbExecute(.con, paste0(
    "COPY (SELECT * FROM anchor_hits) TO '", as.character(fs::path_abs(.path_out)),
    "' (FORMAT PARQUET)"
  ))
  cli::cli_alert_success("Anchor hits written: {n_} distinct span{?s}")
  invisible(.path_out)
}

#' Anchor availability and yield, per label
#'
#' Availability is how many documents carry an anchor at all; yield is how many of those have at
#' least one candidate matching it. The gap between them is the interesting quantity: a document
#' with an anchor and no match is one where every engine missed a known-true entity.
#'
#' @param .con Session with anchor_hits built.
#' @return Tibble: Label, DocsWithAnchor, DocsMatched, PctMatched, Spans, AnchorSpans, BaseRate.
ent_anchor_coverage <- function(.con, .window_days = NULL) {
  if (FALSE) {
    .con         <- con
    .window_days <- NULL
  }

  # The date test is re-evaluated here rather than read off anchor_hits when .window_days is given.
  # The sensitivity analysis previously rebuilt the shared anchor_hits table once per window and
  # relied on a later chunk to put it back, which makes every section after that point depend on a
  # restore executing: reorder the document, or fail in between, and the rest of it silently reports
  # against a window nobody chose. Recomputing one column in a query touches nothing.
  isanchor_ <- if (is.null(.window_days)) {
    "ah.IsAnchor"
  } else {
    paste0(
      "CASE WHEN ah.Label <> 'DATE' THEN ah.IsAnchor ELSE COALESCE( ",
      "  ah.SpanDate IS NOT NULL AND k.DateFiled IS NOT NULL ",
      "  AND CAST(ah.SpanDate AS DATE) <= CAST(k.DateFiled AS DATE) ",
      "  AND CAST(ah.SpanDate AS DATE) >= CAST(k.DateFiled AS DATE) - ",
      as.integer(.window_days), ", FALSE) END"
    )
  }
  from_ <- if (is.null(.window_days)) {
    "anchor_hits ah"
  } else {
    "anchor_hits ah JOIN keys k USING (DocID)"
  }

  DBI::dbGetQuery(.con, paste0(
    "WITH avail AS ( ",
    "  SELECT 'ORG' AS Label, COUNT(*) AS N FROM keys WHERE AnchorKey IS NOT NULL ",
    "  UNION ALL SELECT 'GPE', COUNT(*) FROM keys WHERE AnchorText IS NOT NULL ",
    "  UNION ALL SELECT 'DATE', COUNT(*) FROM keys WHERE DateFiled IS NOT NULL ",
    "  UNION ALL SELECT 'MONEY', 0), ",
    "agg AS ( ",
    "  SELECT ah.Label, COUNT(*) AS Spans, ",
    "         SUM(CASE WHEN ", isanchor_, " THEN 1 ELSE 0 END) AS AnchorSpans, ",
    "         COUNT(DISTINCT CASE WHEN ", isanchor_, " THEN ah.DocID END) AS DocsMatched ",
    "  FROM ", from_, " GROUP BY ah.Label) ",
    "SELECT agg.Label, avail.N AS DocsWithAnchor, agg.DocsMatched, ",
    "       agg.Spans, agg.AnchorSpans, ",
    "       CAST(agg.DocsMatched AS DOUBLE) / NULLIF(avail.N, 0) AS PctMatched, ",
    "       CAST(agg.AnchorSpans AS DOUBLE) / NULLIF(agg.Spans, 0) AS BaseRate ",
    "FROM agg LEFT JOIN avail USING (Label) ORDER BY agg.Label"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(c(DocsWithAnchor, DocsMatched, Spans, AnchorSpans), as.integer))
}


# 2. Engine recall floors --------------------------------------------------------------------------------------------
# The result that costs nothing and settles the engine question.

#' Per-engine recall against the known entity, plus the cost of finding it
#'
#' Recall is a document-level share: of the contracts carrying an anchor, in how many did this
#' engine propose at least one span matching it. There is no hedging needed on that number -- an
#' engine that never proposes the filer's own name did not find it.
#'
#' SpansPerAnchor is the companion. An engine returning twenty times as many spans for the same
#' recall is not better, it is louder, and the ratio is exactly the adjudication cost per known
#' entity recovered. Read together they answer whether breadth buys anything.
#'
#' Computed on every document rather than on the held-out fold: no rule enters this, so nothing here
#' can have been taught by the reading session.
#'
#' @param .con Session with anchor_hits built.
#' @return Tibble: Combo, Label, DocsWithAnchor, DocsFound, Recall, Spans, SpansPerAnchor.
ent_engine_recall <- function(.con) {
  if (FALSE) .con <- con

  DBI::dbGetQuery(.con, paste0(
    "WITH ce AS ( ",
    "  SELECT CASE WHEN c.Engine = c.Model THEN c.Engine ELSE c.Engine || ':' || c.Model END ",
    "           AS Combo, ",
    "         c.DocID, c.Label, c.Start, c.Stop FROM s.candidates c WHERE c.Start IS NOT NULL), ",
    "flag AS (SELECT ce.Combo, ce.DocID, ce.Label, ah.IsAnchor ",
    "         FROM ce JOIN anchor_hits ah USING (DocID, Label, Start, Stop)), ",
    "avail AS ( ",
    "  SELECT 'ORG' AS Label, COUNT(*) AS N FROM keys WHERE AnchorKey IS NOT NULL ",
    "  UNION ALL SELECT 'GPE', COUNT(*) FROM keys WHERE AnchorText IS NOT NULL ",
    "  UNION ALL SELECT 'DATE', COUNT(*) FROM keys WHERE DateFiled IS NOT NULL ",
    "  UNION ALL SELECT 'MONEY', 0) ",
    "SELECT f.Combo, f.Label, avail.N AS DocsWithAnchor, ",
    "       COUNT(DISTINCT CASE WHEN f.IsAnchor THEN f.DocID END) AS DocsFound, ",
    "       COUNT(*) AS Spans, ",
    "       SUM(CASE WHEN f.IsAnchor THEN 1 ELSE 0 END) AS AnchorSpans ",
    "FROM flag f LEFT JOIN avail ON f.Label = avail.Label ",
    "GROUP BY f.Combo, f.Label, avail.N ORDER BY f.Label, f.Combo"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(c(DocsWithAnchor, DocsFound, Spans, AnchorSpans), as.integer),
      Recall         = .data$DocsFound / dplyr::na_if(.data$DocsWithAnchor, 0L),
      SpansPerAnchor = .data$Spans / dplyr::na_if(.data$AnchorSpans, 0L)
    )
}


#' Read the extraction policy this family settled on
#'
#' Lives here rather than with its readers because 04B WRITES the artifact, and the design rule is
#' that shared tooling sits in the earliest script that needs it. It was in 04C, which meant 04D
#' had to source the whole resolution library to read one parquet -- and once 04D stopped resolving
#' anything, it stopped sourcing 04C and the function became unreachable.
#'
#' A label may carry more than one row: the cheap rule arms are unioned into the policy regardless
#' of the ranking, so dates run two engines. Consumers deploy every row for a label and deduplicate
#' on the span.
#'
#' @param .path Policy parquet written by this document.
#' @return Tibble: Label, Combo, CapChars, TailChars, and the recall each choice implies.
ent_read_policy <- function(.path) {
  if (FALSE) .path <- .lP$Input$Policy

  pol_ <- arrow::read_parquet(.path)
  need_ <- c("Label", "Combo", "CapChars")
  miss_ <- setdiff(need_, names(pol_))
  if (length(miss_) > 0L) cli::cli_abort("Policy is missing: {miss_}")
  if (!"TailChars" %in% names(pol_)) pol_$TailChars <- 0L
  pol_
}


# 3. Rule scoring ----------------------------------------------------------------------------------------------------

#' Score every proposed rule against the anchor, on the held-out fold
#'
#' Context is materialised once per candidate and every rule is then tested against it, rather than
#' the reverse: the join is a few hundred rules against a few hundred thousand candidates, and doing
#' it the other way would slice the same text hundreds of times.
#'
#' Rules that never fire disappear from a grouped count, so the scores are joined back onto the full
#' rule table. A rule firing zero times is a result -- it is the commonest one -- and a rule silently
#' absent from the output is indistinguishable from a rule nobody proposed.
#'
#' Cues are tested as literal phrases inside a window on the stated side. Stops are tested as WHOLE
#' SPAN equality on the normalised span, never as substrings: applied as substrings, a stop on
#' "company" would kill "Blue Ridge Real Estate Company", which is a party.
#'
#' @param .con Session with anchor_hits built.
#' @param .path_text Canonical text parquet.
#' @param .rules Tibble of compiled rules.
#' @param .fold Fold to score on; the one the reading session never saw.
#' @param .ctx_max Widest context materialised, which caps the window any rule may use.
#' @return Tibble: one row per rule with NFire, NAnchorFire, Precision, Recall, Lift, plus the rule.
ent_rule_scores <- function(.con, .path_text, .rules, .fold = 5L, .ctx_max = 400L) {
  if (FALSE) {
    .con       <- con
    .path_text <- .lP$Input$Text
    .rules     <- tab_rules
    .fold      <- 5L
    .ctx_max   <- 400L
  }

  rules_ <- .rules |>
    dplyr::filter(.data$Kind %in% c("cue", "stop")) |>
    dplyr::mutate(RuleID = dplyr::row_number())
  ent_put_table(.con = .con, .name = "rules", .tab = rules_)

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE ctx AS ",
    "SELECT ah.DocID, ah.Label, ah.Start, ah.Stop, ah.IsAnchor, ah.SpanNorm, ",
    "  lower(regexp_replace(substring(t.TextRaw, greatest(1, ah.Start + 1 - ", as.integer(.ctx_max),
    "), least(", as.integer(.ctx_max), ", ah.Start)), '\\s+', ' ', 'g')) AS LeftCtx, ",
    "  lower(regexp_replace(substring(t.TextRaw, ah.Stop + 1, ", as.integer(.ctx_max),
    "), '\\s+', ' ', 'g')) AS RightCtx ",
    "FROM anchor_hits ah JOIN read_parquet('", as.character(fs::path_abs(.path_text)),
    "') t USING (DocID) WHERE ah.Fold = ", as.integer(.fold)
  ))

  # Derived from the context columns rather than rebuilt from the text: the flag is a property of
  # the window the rules see, so computing it from anything else would let the two drift. The two
  # sides are joined by a pipe rather than a space because the pattern admits only whitespace and
  # asterisks, so a bracket left open at the end of one side cannot close across the join.
  DBI::dbExecute(.con, paste0(
    "ALTER TABLE ctx ADD COLUMN NearRedactSymbol BOOLEAN"
  ))
  DBI::dbExecute(.con, paste0(
    "UPDATE ctx SET NearRedactSymbol = regexp_matches(LeftCtx || ' | ' || RightCtx, '",
    .RX_REDACT_SYM, "')"
  ))

  # What the characters immediately either side of a span imply about it. Read off the ends of the
  # context columns rather than the middle: a currency symbol four characters back is the one the
  # removal left standing, and one four hundred characters back is somebody else's.
  for (col_ in c("ShapeValue BOOLEAN", "ShapeRate BOOLEAN", "ShapePeriod BOOLEAN",
                 "ShapeUnit BOOLEAN")) {
    DBI::dbExecute(.con, paste0("ALTER TABLE ctx ADD COLUMN ", col_))
  }
  DBI::dbExecute(.con, paste0(
    "UPDATE ctx SET ",
    "  ShapeValue  = regexp_matches(right(LeftCtx, 4), '", ent_sql_currency(), "\\s*$'), ",
    "  ShapeRate   = regexp_matches(left(RightCtx, 3), '^\\s*%'), ",
    "  ShapePeriod = regexp_matches(left(RightCtx, 24), ",
    "    '^\\s*((business|calendar)\\s+)?(day|month|year|week)s?'), ",
    "  ShapeUnit   = regexp_matches(left(RightCtx, 24), '^\\s*per\\s+[a-z]')"
  ))

  base_ <- DBI::dbGetQuery(.con, paste0(
    "SELECT Label, COUNT(*) AS NCand, SUM(CASE WHEN IsAnchor THEN 1 ELSE 0 END) AS NAnchor ",
    "FROM ctx GROUP BY Label"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      NCand = as.integer(.data$NCand), NAnchor = as.integer(.data$NAnchor),
      BaseRate = .data$NAnchor / .data$NCand
    )

  fired_ <- DBI::dbGetQuery(.con, paste0(
    "SELECT r.RuleID, COUNT(*) AS NFire, ",
    "       SUM(CASE WHEN c.IsAnchor THEN 1 ELSE 0 END) AS NAnchorFire ",
    "FROM ctx c JOIN rules r USING (Label) ",
    "WHERE CASE ",
    "  WHEN r.Kind = 'stop' THEN c.SpanNorm = upper(r.Pattern) ",
    "  WHEN r.Side = 'left'  THEN contains(right(c.LeftCtx,  r.Window), r.Pattern) ",
    "  WHEN r.Side = 'right' THEN contains(left(c.RightCtx,  r.Window), r.Pattern) ",
    "  ELSE contains(right(c.LeftCtx, r.Window), r.Pattern) ",
    "       OR contains(left(c.RightCtx, r.Window), r.Pattern) END ",
    "GROUP BY r.RuleID"
  )) |>
    tibble::as_tibble()

  rules_ |>
    dplyr::left_join(fired_, by = dplyr::join_by(RuleID)) |>
    dplyr::mutate(
      NFire       = as.integer(dplyr::coalesce(.data$NFire, 0)),
      NAnchorFire = as.integer(dplyr::coalesce(.data$NAnchorFire, 0))
    ) |>
    dplyr::left_join(base_, by = dplyr::join_by(Label)) |>
    dplyr::mutate(
      Precision = .data$NAnchorFire / dplyr::na_if(.data$NFire, 0L),
      Recall    = .data$NAnchorFire / dplyr::na_if(.data$NAnchor, 0L),
      Lift      = .data$Precision / dplyr::na_if(.data$BaseRate, 0),
      Coverage  = .data$NFire / .data$NCand
    )
}

#' Judge each rule in the direction the anchor can actually speak to
#'
#' A first version split roles two ways -- the anchor either represents a role or it does not -- and
#' left 116 cues unjudged. That was too conservative. The anchor is the filer, and the filer is a
#' PARTY, so for some roles the correct behaviour is not high precision against it but the ABSENCE
#' of it. "Act of" should never sit beside the contract's own date; "as administrative agent" should
#' never sit beside the filer's own name; an expiry date falls after the filing while the anchor
#' requires a date at or before it. For those roles a rule that fires on the known entity is firing
#' on the wrong thing, and low lift is the pass condition.
#'
#' That is falsification rather than confirmation, and the verdict says so: a regulator cue scoring
#' near zero has been shown not to fire on parties, which is weaker than being shown correct. It
#' still removes mislabelled rules, which is most of what is wanted from a set nobody has validated.
#'
#' Three roles stay outside the frame in either direction, and deliberately. An affiliate shares the
#' filer's name, so the anchor matches it and high lift means nothing. Governing law is Delaware in a
#' large share of these contracts and so is the state of incorporation, which the anchor may also
#' carry. Money has no anchor at all.
#'
#' @param .tab Tibble from ent_rule_scores().
#' @param .aligned Tibble of Label, Role and Direction ("high" or "low").
#' @param .min_fire Rules firing fewer times than this are not judged on precision.
#' @param .anchor_role Named character vector: the role each label's anchor represents, used to
#'   suggest a reassignment for cues that fire on it under an anti-aligned label.
#' @return .tab with Direction, Measurable, Verdict and SuggestedRole columns.
ent_rule_verdict <- function(.tab, .aligned, .min_fire = 30L,
                             .anchor_role = c(ORG = "party", DATE = "effective",
                                              GPE = "party_address")) {
  if (FALSE) {
    .tab         <- tab_scores
    .aligned     <- .ALIGNED
    .min_fire    <- 30L
    .anchor_role <- c(ORG = "party", DATE = "effective", GPE = "party_address")
  }

  .tab |>
    dplyr::left_join(.aligned, by = dplyr::join_by(Label, Role)) |>
    dplyr::mutate(
      Measurable = !is.na(.data$Direction) | (.data$Kind == "stop" & .data$NAnchor > 0L),
      Verdict = dplyr::case_when(
        .data$NFire == 0L                                     ~ "never fires",
        .data$Kind == "stop" & .data$NAnchor == 0L            ~ "unmeasured: no anchor for label",
        .data$Kind == "stop" & .data$NAnchorFire > 0L         ~ "REFUTED: kills anchors",
        .data$Kind == "stop"                                  ~ "safe",
        is.na(.data$Direction)                                ~ "unmeasured: outside anchor reach",
        .data$NFire < .min_fire                               ~ "too rare to judge",
        .data$Direction == "high" & .data$Lift >= 2           ~ "strong",
        .data$Direction == "high" & .data$Lift >= 1.2         ~ "useful",
        .data$Direction == "high"                             ~ "no better than chance",
        .data$Direction == "low"  & .data$Lift <= 0.5         ~ "consistent: avoids the anchor",
        .data$Direction == "low"  & .data$Lift >= 1.5         ~ "role misassigned: fires on anchor",
        TRUE                                                  ~ "ambiguous"
      ),
      # A low-direction cue with high lift has not been shown wrong. It has been shown not to be
      # SPECIFIC to the role it was given: "located at" was proposed for performance and fires on
      # the filer's own address at five times the base rate, which makes it a party-address cue
      # wearing the wrong label. Discarding it would throw away a measured, strong rule. The
      # suggestion is recorded rather than applied, because whether a cue that co-occurs with the
      # anchor actually LOCATES it is a judgement 04D should make with the number in front of it.
      SuggestedRole = dplyr::if_else(
        .data$Verdict == "role misassigned: fires on anchor",
        unname(.anchor_role[.data$Label]), NA_character_
      )
    )
}


# 4. Window geometry: proportional -----------------------------------------------------------------------------------

#' Does a window belong on a relative or an absolute axis?
#'
#' The sibling of ent_window_chars(), and the two answer different questions. This one measures a
#' window given as a FRACTION of each document; that one measures a window given in characters. The
#' comparison between them is the question a cap cannot answer about itself: the median contract
#' here runs 25,578 characters and the longest runs 10.4 million, so a 3,300-character cap is
#' thirteen percent of the median and three hundredths of a percent of the longest. If the anchor
#' sits at roughly a fixed FRACTION of the document whatever its length, a fixed character cap is
#' badly calibrated for long documents and a proportional one would beat it. Only a fractional grid
#' can show that, which is why this exists alongside the other.
#'
#' THE DENOMINATOR IS DOCUMENTS, NOT MENTIONS, and the two are not interchangeable. A contract that
#' names the filer forty times -- twenty in the preamble and twenty at the signature block -- is
#' recovered by a head-only window: a party has to be found ONCE. Counting mentions, adding a tail
#' records twenty further recovered spans and the tail looks valuable; counting documents it
#' records nothing, correctly, because that document was already recovered. The mention denominator
#' therefore flatters tails systematically, and the tail is exactly what is under dispute.
#'
#' It previously used mentions, in the same document whose prose argued against them.
#'
#' @param .con Session with anchor_hits built.
#' @param .path_text Canonical text parquet, supplying document lengths.
#' @param .head Numeric. Fractions of the document read from the front.
#' @param .tail Numeric. Fractions read from the back; 0 is the plain prefix window.
#' @return Tibble: Label, Head, Tail, DocRecall, SpansKept, Ratio. DocRecall is the share of
#'   documents holding an anchor that keep at least one anchor mention inside the window.
ent_window_grid <- function(.con, .path_text, .head = c(0.02, 0.05, 0.10, 0.20),
                            .tail = c(0, 0.05, 0.10, 0.20)) {
  if (FALSE) {
    .con       <- con
    .path_text <- .lP$Input$Text
    .head      <- c(0.02, 0.05, 0.10, 0.20)
    .tail      <- c(0, 0.05, 0.10, 0.20)
  }

  grid_ <- tidyr::expand_grid(Head = .head, Tail = .tail)
  ent_put_table(.con = .con, .name = "grid", .tab = grid_)

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE pos AS ",
    "SELECT ah.DocID, ah.Label, ah.IsAnchor, ",
    "       ((ah.Start + ah.Stop) / 2.0) / length(t.TextRaw) AS Rel ",
    "FROM anchor_hits ah JOIN read_parquet('", as.character(fs::path_abs(.path_text)),
    "') t USING (DocID) WHERE length(t.TextRaw) > 0"
  ))

  DBI::dbGetQuery(.con, paste0(
    "WITH base AS (SELECT Label, COUNT(DISTINCT DocID) AS NAnchorDocs FROM pos ",
    "              WHERE IsAnchor GROUP BY Label) ",
    "SELECT p.Label, g.Head, g.Tail, any_value(b.NAnchorDocs) AS NAnchorDocs, ",
    "  COUNT(DISTINCT CASE WHEN p.IsAnchor AND (p.Rel <= g.Head OR p.Rel >= 1 - g.Tail) ",
    "        THEN p.DocID END) ",
    "    / CAST(any_value(b.NAnchorDocs) AS DOUBLE) AS DocRecall, ",
    "  SUM(CASE WHEN (p.Rel <= g.Head OR p.Rel >= 1 - g.Tail) THEN 1 ELSE 0 END) ",
    "    / CAST(COUNT(*) AS DOUBLE) AS SpansKept ",
    "FROM pos p CROSS JOIN grid g JOIN base b ON p.Label = b.Label ",
    "GROUP BY p.Label, g.Head, g.Tail ORDER BY p.Label, g.Head, g.Tail"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      NAnchorDocs = as.integer(.data$NAnchorDocs),
      Ratio       = .data$DocRecall / dplyr::na_if(.data$SpansKept, 0)
    )
}


# 5. Section probe ---------------------------------------------------------------------------------------------------

#' Locate known headings by searching for them, not by parsing lines
#'
#' The scan in the bundle script enumerated headings from line structure and returned a table in
#' which every heading occurred in exactly one document -- an impossible result that the reading
#' session reported four times. Two causes, and the fix has to survive both: this corpus carries
#' Windows line endings, which DuckDB's trim() does not strip, so a case filter rejects every line;
#' and HTML-to-text conversion leaves long stretches with no newline at all, so what remains is
#' whatever short line a particular document happens to hold.
#'
#' Searching for a fixed list of headings needs no line structure. It also answers the question that
#' actually matters, which was never "what headings exist" but "does this heading appear, and
#' where": a heading in the final fifth of every contract is a locatable region.
#'
#' @param .path_text Canonical text parquet.
#' @param .headings Character vector of headings to probe, upper case.
#' @return Tibble: Heading, DocFreq, PctDocs, MedianPos, IQRPos.
ent_probe_sections <- function(.path_text, .headings) {
  if (FALSE) {
    .path_text <- .lP$Input$Text
    .headings  <- .HEADINGS
  }
  con_ <- ner_db_connect()
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
  ent_put_table(.con = con_, .name = "probe", .tab = data.frame(Heading = .headings))

  DBI::dbGetQuery(con_, paste0(
    "WITH txt AS (SELECT DocID, upper(TextRaw) AS T, length(TextRaw) AS L ",
    "             FROM read_parquet('", as.character(fs::path_abs(.path_text)), "') ",
    "             WHERE length(TextRaw) > 0), ",
    "hit AS (SELECT p.Heading, txt.DocID, ",
    "               CAST(instr(txt.T, p.Heading) AS DOUBLE) / txt.L AS Rel ",
    "        FROM txt CROSS JOIN probe p WHERE instr(txt.T, p.Heading) > 0), ",
    "tot AS (SELECT COUNT(*) AS N FROM txt) ",
    "SELECT h.Heading, COUNT(*) AS DocFreq, COUNT(*) / any_value(tot.N) AS PctDocs, ",
    "       median(h.Rel) AS MedianPos, ",
    "       quantile_cont(h.Rel, 0.25) AS P25Pos, quantile_cont(h.Rel, 0.75) AS P75Pos ",
    "FROM hit h, tot GROUP BY h.Heading ORDER BY DocFreq DESC"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(DocFreq = as.integer(.data$DocFreq))
}


# 6. Window geometry: characters, and the policy that follows --------------------------------------------------------
# The fractional grid answers a question nobody can act on: --max-chars takes a character count,
# and a character cap is not a fixed fraction of anything -- 3,300 characters is thirteen percent
# of the median contract and three hundredths of a percent of the longest.

#' What a character cap keeps, measured at the level extraction actually needs
#'
#' The earlier grid reported the share of anchor MENTIONS a window retains. That is the wrong
#' denominator for choosing a cap. A party has to be found once, not every time it is named, so
#' what matters is the share of DOCUMENTS in which at least one mention of the known entity
#' survives. The two differ by a lot: on a synthetic sample where three quarters of anchors sit in
#' the opening two thousand characters, a 3,000-character head keeps 75% of documents while keeping
#' only 12% of candidates.
#'
#' Three columns, because a cap is three trade-offs at once. DocRecall is what the cap buys.
#' SpansKept is what 04D then has to resolve. CharsKept is what 04D has to read, and it is the only
#' one that maps to wall-clock time.
#'
#' Restricted to the deployed engine where .policy is given. Pooling every engine overstates what
#' the corpus pass achieves: a document counts as recovered if ANY of eight engines found the anchor
#' there, and only one of them will run.
#'
#' @param .con Session with anchor_hits built.
#' @param .path_text Canonical text parquet.
#' @param .head,.tail Integer vectors of character budgets from the front and the back.
#' @param .policy Tibble of Label and Combo to restrict to, or NULL for all engines pooled.
#' @return Tibble: Label, Head, Tail, NAnchorDocs, DocRecall, SpansKept, CharsKept.
ent_window_chars <- function(.con, .path_text,
                             .head = c(1500L, 3000L, 5000L, 10000L, 20000L),
                             .tail = c(0L, 2000L, 5000L),
                             .policy = NULL) {
  if (FALSE) {
    .con       <- con
    .path_text <- .lP$Input$Text
    .head      <- c(1500L, 3000L, 5000L, 10000L, 20000L)
    .tail      <- c(0L, 2000L, 5000L)
    .policy    <- tab_policy
  }

  # Anchor spans, optionally narrowed to the spans the deployed engine actually proposed.
  if (is.null(.policy)) {
    DBI::dbExecute(.con, "CREATE OR REPLACE TABLE ahwin AS SELECT * FROM anchor_hits")
  } else {
    ent_put_table(.con = .con, .name = "wpolicy", .tab = dplyr::mutate(
      .policy,
      Engine = sub(":.*$", "", .data$Combo),
      Model  = dplyr::if_else(grepl(":", .data$Combo, fixed = TRUE),
                              sub("^[^:]*:", "", .data$Combo), .data$Combo)
    ))
    DBI::dbExecute(.con, paste0(
      "CREATE OR REPLACE TABLE ahwin AS SELECT DISTINCT ah.* FROM anchor_hits ah ",
      "JOIN s.candidates c USING (DocID, Label, Start, Stop) ",
      "JOIN wpolicy p ON c.Label = p.Label AND c.Engine = p.Engine AND c.Model = p.Model"
    ))
  }

  ent_put_table(.con = .con, .name = "cgrid",
                .tab = tidyr::expand_grid(Head = as.integer(.head), Tail = as.integer(.tail)))
  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE lens AS SELECT DocID, length(TextRaw) AS DocLen ",
    "FROM read_parquet('", as.character(fs::path_abs(.path_text)), "') WHERE length(TextRaw) > 0"
  ))

  DBI::dbGetQuery(.con, paste0(
    "WITH pos AS (SELECT ah.Label, ah.DocID, ah.IsAnchor, ah.Start, ah.Stop, l.DocLen ",
    "             FROM ahwin ah JOIN lens l USING (DocID)), ",
    "base AS (SELECT Label, COUNT(DISTINCT DocID) AS NAnchorDocs FROM pos WHERE IsAnchor ",
    "         GROUP BY Label), ",
    "chars AS (SELECT g.Head, g.Tail, ",
    "            SUM(least(g.Head + g.Tail, l.DocLen)) / CAST(SUM(l.DocLen) AS DOUBLE) AS CharsKept ",
    "          FROM lens l CROSS JOIN cgrid g GROUP BY 1, 2) ",
    "SELECT p.Label, g.Head, g.Tail, any_value(b.NAnchorDocs) AS NAnchorDocs, ",
    "  COUNT(DISTINCT CASE WHEN p.IsAnchor AND (p.Stop <= g.Head ",
    "        OR p.Start >= p.DocLen - g.Tail) THEN p.DocID END) ",
    "    / CAST(any_value(b.NAnchorDocs) AS DOUBLE) AS DocRecall, ",
    "  SUM(CASE WHEN (p.Stop <= g.Head OR p.Start >= p.DocLen - g.Tail) THEN 1 ELSE 0 END) ",
    "    / CAST(COUNT(*) AS DOUBLE) AS SpansKept, ",
    "  any_value(c.CharsKept) AS CharsKept ",
    "FROM pos p CROSS JOIN cgrid g JOIN base b ON p.Label = b.Label ",
    "  JOIN chars c ON c.Head = g.Head AND c.Tail = g.Tail ",
    "GROUP BY p.Label, g.Head, g.Tail ORDER BY p.Label, g.Head, g.Tail"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(c(Head, Tail, NAnchorDocs), as.integer))
}

#' Choose one engine per label from the measured recall and cost
#'
#' Stating the rule and letting the code apply it, rather than reading the table and asserting a
#' winner in prose, so the choice is reproducible and its sensitivity to the tolerances is visible.
#'
#' The rule follows the convention already used for keyword configurations elsewhere in this
#' project: take the engines within .recall_tol of the best recall, prefer the cheapest, and where
#' several are within .cost_tol of the cheapest take the most accurate of those. The second
#' tolerance matters here -- on dates two engines sit at 2.7 and 2.8 spans per anchor, a gap far
#' inside any measurement error, and without it the rule would trade three points of recall for a
#' rounding difference.
#'
#' Labels with no anchor get a DECLARED engine rather than no row at all. The first version filtered
#' them out, and 04C then joined its candidate set against a policy with holes in it: money and
#' redaction were dropped silently and the value column came back empty for every contract. A policy
#' is a deployment specification, not a selection result, so it has to name an engine for everything
#' the corpus pass will run -- with Basis recording which choices were measured and which asserted.
#'
#' @param .tab Tibble from ent_engine_recall().
#' @param .defaults Named character vector: engine to declare for labels the anchor cannot rank.
#' @param .recall_tol Recall points below the best an engine may sit and still be considered.
#' @param .cost_tol Relative cost gap treated as a tie.
#' @return Tibble: Label, Combo, Basis, Recall, SpansPerAnchor, NConsidered, Rule.
ent_choose_engines <- function(.tab, .defaults = character(0), .proxy = character(0),
                               .always = tibble::tibble(Label = character(0), Combo = character(0)),
                               .recall_tol = 0.10, .cost_tol = 0.15) {
  if (FALSE) {
    .tab         <- tab_eng
    .defaults    <- .DEFAULT_ENGINE
    .proxy       <- .PROXY_BASIS
    .always      <- .ALWAYS
    .recall_tol  <- 0.10
    .cost_tol    <- 0.15
  }
  # A named vector indexed by a label that is absent returns NA, which is what the coalesce below
  # relies on. An empty character vector indexes to NA the same way, so the default is safe.
  .proxy <- .proxy[unique(.tab$Label)] |> purrr::set_names(unique(.tab$Label))

  measured_ <- .tab |>
    dplyr::filter(!is.na(.data$Recall), .data$SpansPerAnchor > 0) |>
    dplyr::mutate(BestRecall = max(.data$Recall), .by = Label) |>
    dplyr::filter(.data$Recall >= .data$BestRecall - .recall_tol) |>
    dplyr::mutate(
      NConsidered = dplyr::n(),
      MinCost     = min(.data$SpansPerAnchor),
      .by = Label
    ) |>
    dplyr::filter(.data$SpansPerAnchor <= .data$MinCost * (1 + .cost_tol)) |>
    dplyr::slice_max(.data$Recall, n = 1L, by = Label, with_ties = FALSE) |>
    dplyr::transmute(
      Label, Combo, Basis = "measured", Recall, SpansPerAnchor, NConsidered,
      Rule = paste0("within ", round(100 * .recall_tol), " recall points of best, ",
                    "cheapest to within ", round(100 * .cost_tol), "%")
    )

  # THREE KINDS OF EVIDENCE, NOT TWO. The original artifact carried Basis with values "measured" and
  # "declared", which put two quite different situations under one word. A label with no anchor
  # cannot be ranked on recall -- but money is not therefore unevidenced. The redaction-reach
  # measurement asks whether an engine can propose a span at a site where a figure was WITHHELD,
  # which is a capability rather than a tuned score, and it is the case that matters most because a
  # figure is withheld precisely when it is commercially material. That is a measurement, on a
  # proxy, and the artifact should say so rather than filing it beside a bare assertion.
  #
  # 04C and 04D read Basis to know how much weight a row carries. Collapsing the three would let a
  # proxy-measured choice and an unevidenced default look identical downstream.
  need_ <- setdiff(unique(.tab$Label), measured_$Label)
  declared_ <- tibble::tibble(
    Label          = need_,
    Combo          = unname(.defaults[need_]),
    Basis          = unname(dplyr::coalesce(.proxy[need_], "declared")),
    Recall         = NA_real_,
    SpansPerAnchor = NA_real_,
    NConsidered    = NA_integer_,
    Rule           = dplyr::if_else(
      !is.na(.proxy[need_]),
      "no anchor; engine chosen on a measured proxy, not on recall",
      "no anchor and no proxy; engine asserted, not measured"
    )
  )
  gap_ <- declared_$Label[is.na(declared_$Combo)]
  if (length(gap_) > 0L) {
    cli::cli_abort("No default engine declared for {gap_}; 04C would drop the label silently.")
  }

  # THE CHEAP RULE ARMS RUN WHATEVER THE RANKING SAYS, and that is a policy rather than an oversight.
  # The selection above trades recall against cost, which is the right trade for an engine costing
  # hours per corpus pass. It is the wrong trade for one costing minutes: dateregex projects to
  # three tenths of a corpus hour, so excluding it saves nothing measurable and forgoes whatever it
  # finds that the winner does not.
  #
  # It forgoes a great deal. Dates are the case that showed it -- the ranking picked LexNLP over
  # dateregex by 3.4 anchor-recall points, and dateregex reaches an expiry cue in 415 documents
  # against LexNLP's 113. The anchor is the contract's own date, which sits in the preamble, so the
  # criterion that made the choice could not see the difference.
  #
  # These are UNIONS, not replacements. 04C deploys candidates from every engine the policy names
  # for a label and deduplicates on the span, so an engine added here widens the candidate set and
  # cannot narrow it. What it costs is spans to resolve, and for a rule arm at comparable volume to
  # the winner that cost is small and measured in the artifact.
  #
  # A row already chosen on merit is not duplicated: geography, money and redaction each have their
  # rule arm as the ranked winner already.
  always_ <- .always |>
    dplyr::filter(.data$Label %in% unique(.tab$Label)) |>
    dplyr::anti_join(
      dplyr::bind_rows(measured_, declared_) |> dplyr::select(Label, Combo),
      by = dplyr::join_by(Label, Combo)
    ) |>
    dplyr::left_join(
      dplyr::select(.tab, Label, Combo, Recall, SpansPerAnchor),
      by = dplyr::join_by(Label, Combo)
    ) |>
    dplyr::mutate(
      Basis       = "always: cheap rule arm",
      NConsidered = NA_integer_,
      Rule        = "included regardless of rank; a rule arm costs minutes, not hours"
    )

  gap_always_ <- .always |>
    dplyr::filter(.data$Label %in% unique(.tab$Label)) |>
    dplyr::anti_join(dplyr::distinct(.tab, Label, Combo), by = dplyr::join_by(Label, Combo))
  if (nrow(gap_always_) > 0L) {
    cli::cli_abort(c(
      "{.arg .always} names {nrow(gap_always_)} combination{?s} absent from the store.",
      "i" = "{paste(gap_always_$Label, gap_always_$Combo, sep = '/')}",
      "x" = "04C would join its candidate set against an engine that never ran."
    ))
  }

  dplyr::bind_rows(measured_, declared_, always_) |>
    dplyr::arrange(.data$Label, .data$Basis)
}


# 7. Money: the three checks external data supports ------------------------------------------------------------------
# Money has no anchor, so none of the machinery above reaches it. What follows is the whole of the
# external evidence there is: whether a phrase cue co-occurs with a redaction marker, what the
# surviving punctuation around a marker implies about what was removed, and whether an engine can
# propose a span at a withheld site at all. The third is what puts a Basis of "measured: proxy"
# rather than "declared" on the money row of the policy.

#' Do the redaction cues fire where bracketed redaction markers sit?
#'
#' EDGAR records no contract value, so money has no anchor and 64 of its cues cannot be scored. One
#' role is the exception. A redacted amount leaves a bracketed marker in the text -- "[***]",
#' "[ * * * ]", "[*]" -- and that is a different kind of evidence from a phrase in the surrounding
#' sentence. Only the SYMBOL form is used: the explicit form, "[confidential treatment requested]",
#' shares vocabulary with the cues themselves and testing one against the other would be circular.
#'
#' This validates six cues, not sixty-four, and it is worth having for a second reason. The reading
#' session observed that redacted figures are systematically the commercially material ones, so a
#' per-document count of them is a variable rather than only a nuisance, and this is what would
#' establish it.
#'
#' @param .con Session with ctx built by ent_rule_scores().
#' @param .rules Tibble of compiled rules.
#' @return Tibble: Pattern, Side, Window, NFire, NNearSymbol, Precision, BaseRate, Lift.
ent_redaction_check <- function(.con, .rules) {
  if (FALSE) {
    .con   <- con
    .rules <- tab_rules
  }

  red_ <- .rules |>
    dplyr::filter(.data$Label == "MONEY", .data$Kind == "cue", .data$Role == "redacted") |>
    dplyr::mutate(RuleID = dplyr::row_number())
  if (nrow(red_) == 0L) return(red_)
  ent_put_table(.con = .con, .name = "redrules", .tab = red_)

  base_ <- DBI::dbGetQuery(.con, paste0(
    "SELECT COUNT(*) AS NCand, SUM(CASE WHEN NearRedactSymbol THEN 1 ELSE 0 END) AS NNear ",
    "FROM ctx WHERE Label = 'MONEY'"
  ))

  DBI::dbGetQuery(.con, paste0(
    "SELECT r.RuleID, COUNT(*) AS NFire, ",
    "       SUM(CASE WHEN c.NearRedactSymbol THEN 1 ELSE 0 END) AS NNearSymbol ",
    "FROM ctx c JOIN redrules r ON c.Label = r.Label ",
    "WHERE CASE r.Side ",
    "  WHEN 'left'  THEN contains(right(c.LeftCtx,  r.Window), r.Pattern) ",
    "  WHEN 'right' THEN contains(left(c.RightCtx,  r.Window), r.Pattern) ",
    "  ELSE contains(right(c.LeftCtx, r.Window), r.Pattern) ",
    "       OR contains(left(c.RightCtx, r.Window), r.Pattern) END ",
    "GROUP BY r.RuleID"
  )) |>
    tibble::as_tibble() |>
    dplyr::right_join(red_, by = dplyr::join_by(RuleID)) |>
    dplyr::mutate(
      NFire       = as.integer(dplyr::coalesce(.data$NFire, 0)),
      NNearSymbol = as.integer(dplyr::coalesce(.data$NNearSymbol, 0)),
      BaseRate    = base_$NNear / base_$NCand,
      Precision   = .data$NNearSymbol / dplyr::na_if(.data$NFire, 0L),
      Lift        = .data$Precision / dplyr::na_if(.data$BaseRate, 0)
    ) |>
    dplyr::select(Pattern, Side, Window, NFire, NNearSymbol, BaseRate, Precision, Lift) |>
    dplyr::arrange(dplyr::desc(.data$Lift))
}


# 7b. What the redaction markers conceal -----------------------------------------------------------------------------
# The redaction unit asks what was removed, and nothing external records that either. But the
# characters immediately around a marker often survive the removal and identify it: a currency
# symbol left standing before the gap, a percent sign after it, a unit noun following. That is
# evidence of a different kind from a phrase in the surrounding sentence, which is what makes it a
# test of the proposed cues rather than a restatement of them.

#' Currency class, built from code points so the source stays ASCII
#'
#' DuckDB's regular expressions do not accept \u escapes, so the characters have to reach the SQL
#' literally; the house rule keeps them out of the R file. Building the class at run time satisfies
#' both. Dollar covers US$ and R$ as well, since both end in the symbol.
#'
#' @return A SQL character class as a string.
ent_sql_currency <- function() {
  if (FALSE) NULL
  paste0("[", intToUtf8(c(0x24, 0xA3, 0xA5, 0x20AC)), "]")
}

#' Read one or more compiled rule files and tag where each came from
#'
#' Two reading sessions have now proposed rules from different documents. Scoring their union is
#' worth more than scoring either alone, and tagging the source keeps the recurrence question
#' answerable afterwards: a rule both sessions proposed is a property of the corpus, one only a
#' single session proposed may be a property of its draw.
#'
#' @param .paths Named character vector of compiled rule parquets; names become Source.
#' @return Tibble of rules with a Source column, deduplicated on the rule itself.
ent_read_rules <- function(.paths) {
  if (FALSE) .paths <- .lP$Input$Rules

  # A NAMED SESSION THAT IS NOT ON DISK IS A BROKEN CONFIGURATION, NOT A DEGRADED RUN, and this
  # aborts rather than warning because the warning was missed twice. The bundle folder is renamed
  # when a session is superseded, so the path in the configuration goes stale on exactly the
  # occasions when a second session has just produced new rules. Each time, the document rendered,
  # scored half the rule set, and reported a rule count that looked plausible on its own. One
  # warning line in a render that emits several hundred is not a control.
  miss_ <- names(.paths)[!fs::file_exists(.paths)]
  if (length(miss_) > 0L) {
    cli::cli_abort(c(
      "Rule file{?s} missing for session{?s} {miss_}.",
      "i" = "Looked in: {.path {(.paths[miss_])}}",
      "x" = "Scoring the sessions that ARE present would silently halve the rule set.",
      "i" = "Fix the folder name in the configuration, or remove the session from it deliberately."
    ))
  }
  have_ <- .paths

  purrr::imap(have_, \(.p, .nm) dplyr::mutate(arrow::read_parquet(.p), Source = .nm)) |>
    purrr::list_rbind() |>
    dplyr::summarise(
      Source = paste(sort(unique(.data$Source)), collapse = "+"),
      dplyr::across(c(Rationale, Evidence), \(.x) dplyr::first(.x)),
      .by = c(Kind, Label, Role, Pattern, Side, Window)
    )
}

#' Score the redaction cues against the characters the removal left behind
#'
#' The role a session assigned says what it believes was removed. The shape says what the surviving
#' punctuation implies. Where the two agree above the base rate, the cue is finding the kind of gap
#' it claims to. Roles the shape cannot speak to -- a removed party name, a deleted clause body --
#' are reported as unmeasured rather than scored against a signal that does not apply to them.
#'
#' @param .con Session with ctx built by ent_rule_scores().
#' @param .rules Tibble of compiled rules.
#' @param .shape_map Tibble of Role and the Shape column each expects.
#' @return Tibble: Role, Shape, Pattern, Side, Window, NFire, NShape, BaseRate, Precision, Lift.
ent_redaction_shape <- function(.con, .rules, .shape_map) {
  if (FALSE) {
    .con       <- con
    .rules     <- tab_rules
    .shape_map <- .REDACT_SHAPE
  }

  red_ <- .rules |>
    dplyr::filter(.data$Label == "REDACT", .data$Kind == "cue") |>
    dplyr::inner_join(.shape_map, by = dplyr::join_by(Role)) |>
    dplyr::mutate(RuleID = dplyr::row_number())
  if (nrow(red_) == 0L) return(red_)
  ent_put_table(.con = .con, .name = "shaperules", .tab = red_)

  base_ <- DBI::dbGetQuery(.con, paste0(
    "SELECT COUNT(*) AS NCand, ",
    "  SUM(CASE WHEN ShapeValue  THEN 1 ELSE 0 END) AS ShapeValue, ",
    "  SUM(CASE WHEN ShapeRate   THEN 1 ELSE 0 END) AS ShapeRate, ",
    "  SUM(CASE WHEN ShapePeriod THEN 1 ELSE 0 END) AS ShapePeriod, ",
    "  SUM(CASE WHEN ShapeUnit   THEN 1 ELSE 0 END) AS ShapeUnit ",
    "FROM ctx WHERE Label = 'REDACT'"
  )) |>
    tidyr::pivot_longer(-NCand, names_to = "Shape", values_to = "NBase") |>
    dplyr::mutate(BaseRate = .data$NBase / .data$NCand)

  DBI::dbGetQuery(.con, paste0(
    "SELECT r.RuleID, COUNT(*) AS NFire, ",
    "  SUM(CASE r.Shape WHEN 'ShapeValue'  THEN CASE WHEN c.ShapeValue  THEN 1 ELSE 0 END ",
    "                   WHEN 'ShapeRate'   THEN CASE WHEN c.ShapeRate   THEN 1 ELSE 0 END ",
    "                   WHEN 'ShapePeriod' THEN CASE WHEN c.ShapePeriod THEN 1 ELSE 0 END ",
    "                   WHEN 'ShapeUnit'   THEN CASE WHEN c.ShapeUnit   THEN 1 ELSE 0 END ",
    "                   ELSE 0 END) AS NShape ",
    "FROM ctx c JOIN shaperules r ON c.Label = r.Label ",
    "WHERE CASE r.Side ",
    "  WHEN 'left'  THEN contains(right(c.LeftCtx,  r.Window), r.Pattern) ",
    "  WHEN 'right' THEN contains(left(c.RightCtx,  r.Window), r.Pattern) ",
    "  ELSE contains(right(c.LeftCtx, r.Window), r.Pattern) ",
    "       OR contains(left(c.RightCtx, r.Window), r.Pattern) END ",
    "GROUP BY r.RuleID"
  )) |>
    tibble::as_tibble() |>
    dplyr::right_join(red_, by = dplyr::join_by(RuleID)) |>
    dplyr::left_join(dplyr::select(base_, Shape, BaseRate), by = dplyr::join_by(Shape)) |>
    dplyr::mutate(
      NFire     = as.integer(dplyr::coalesce(.data$NFire, 0)),
      NShape    = as.integer(dplyr::coalesce(.data$NShape, 0)),
      Precision = .data$NShape / dplyr::na_if(.data$NFire, 0L),
      Lift      = .data$Precision / dplyr::na_if(.data$BaseRate, 0)
    ) |>
    dplyr::select(Role, Shape, Pattern, Side, Window, NFire, NShape, BaseRate, Precision, Lift) |>
    dplyr::arrange(dplyr::desc(.data$Lift))
}


# 7c. Can an engine find a withheld amount at all? -------------------------------------------------------------------
# Money is the one label with no anchor, so the engine behind it went into the policy as a declared
# default. Cross-engine agreement narrows the question -- the regex arm and the transformer overlap
# at Jaccard 0.77, the highest of any cross-family pair in the store -- but agreement on what both
# find says nothing about what only one of them can.
#
# A redaction marker is where an amount used to be. Whether an engine proposes a span there is a
# CAPABILITY question rather than a tuned metric, and it is the case that matters most, because a
# figure is withheld precisely when it is commercially material.

#' Redaction sites, classified by the characters the removal left behind
#'
#' The class is read off the text either side of the marker and not from any engine's output, so the
#' definition does not presuppose the answer. Two classes carry information:
#'
#'   after_currency  A currency symbol immediately precedes the marker: "$[***]", "a price of $ per
#'                   share". The amount is gone and the symbol is not.
#'   before_unit     A percent sign or a unit noun immediately follows: "[***]% of net sales",
#'                   "[***] days after notice". Nothing marks it as money except what it is measured
#'                   in, so an engine keying on currency cannot reach it either.
#'
#' The second class is the honest half of this test. It favours no engine and bounds what is
#' recoverable at all, which is worth knowing before a redaction-aware money variable is promised.
#'
#' @param .con Session with the store attached as s.
#' @param .path_text Canonical text parquet.
#' @return Invisibly the row count of the table created.
ent_redaction_sites <- function(.con, .path_text) {
  if (FALSE) {
    .con       <- con
    .path_text <- .lP$Input$Text
  }

  cur_ <- ent_sql_currency()
  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE site AS ",
    "SELECT row_number() OVER () AS SiteID, r.DocID, r.Start, r.Stop, ",
    "  CASE WHEN regexp_matches(substring(t.TextRaw, greatest(1, r.Start - 2), ",
    "                           least(3, r.Start)), '", cur_, "\\s*$') THEN 'after_currency' ",
    "       WHEN regexp_matches(substring(t.TextRaw, r.Stop + 1, 14), ",
    "            '^\\s*(%|per\\s+[a-z]|(business |calendar )?(day|month|year|week)s?)') ",
    "         THEN 'before_unit' ",
    "       ELSE 'other' END AS Site ",
    "FROM s.candidates r JOIN read_parquet('", as.character(fs::path_abs(.path_text)),
    "') t USING (DocID) WHERE r.Label = 'REDACT' AND r.Start IS NOT NULL"
  ))
  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM site")$N
  cli::cli_alert_success("Redaction sites classified: {n_}")
  invisible(n_)
}

#' Which money engines propose a span where an amount was removed
#'
#' A site counts as covered when the engine returns a money span overlapping it or within .near
#' characters. Overlap alone would be too strict: an engine may tag the currency symbol without
#' reaching into the bracket, and that is still finding the amount.
#'
#' Read the two site classes differently. On after_currency the regex arm has a pattern written for
#' exactly this shape, so a difference there is expected and the size of it is the result; a
#' transformer has nothing to tag, since the number it would recognise is the thing that was
#' removed. On before_unit neither engine has any reason to fire, and a low figure for both is a
#' statement about the ceiling rather than about either of them.
#'
#' @param .con Session with site built.
#' @param .near Characters either side of a site within which a span counts as covering it.
#' @return Tibble: Combo, Site, NSites, NCovered, PctCovered.
ent_redaction_reach <- function(.con, .near = 3L) {
  if (FALSE) {
    .con  <- con
    .near <- 3L
  }

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE mon AS ",
    "SELECT DocID, Start, Stop, ",
    "  CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END AS Combo ",
    "FROM s.candidates WHERE Label = 'MONEY' AND Start IS NOT NULL"
  ))
  DBI::dbExecute(.con, "CREATE OR REPLACE TABLE engines AS SELECT DISTINCT Combo FROM mon")

  DBI::dbGetQuery(.con, paste0(
    "WITH pair AS ( ",
    "  SELECT DISTINCT s.SiteID, m.Combo FROM site s JOIN mon m ON m.DocID = s.DocID ",
    "    AND m.Stop >= s.Start - ", as.integer(.near), " ",
    "    AND m.Start <= s.Stop + ", as.integer(.near), ") ",
    "SELECT e.Combo, s.Site, COUNT(DISTINCT s.SiteID) AS NSites, ",
    "       COUNT(DISTINCT p.SiteID) AS NCovered ",
    "FROM site s CROSS JOIN engines e ",
    "LEFT JOIN pair p ON p.SiteID = s.SiteID AND p.Combo = e.Combo ",
    "GROUP BY e.Combo, s.Site ORDER BY s.Site, COUNT(DISTINCT p.SiteID) DESC"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      NSites     = as.integer(.data$NSites),
      NCovered   = as.integer(.data$NCovered),
      PctCovered = .data$NCovered / dplyr::na_if(.data$NSites, 0L)
    )
}


# 8. Report ----------------------------------------------------------------------------------------------------------

#' Anchor availability and how often any engine found it
#' @param .tab Tibble from ent_anchor_coverage().
#' @return Invisibly .tab.
ent_report_anchors <- function(.tab) {
  if (FALSE) .tab <- tab_cov

  cli::cli_h2("Anchor availability and yield")
  tbl_say(
    .tab = .tab |>
      dplyr::mutate(PctMatched = tbl_pct(.data$PctMatched), BaseRate = tbl_pct(.data$BaseRate, 2L))
  )
  cli::cli_alert_info(
    "PctMatched is the share of documents where SOME engine proposed the known entity, so it caps \\
     every per-engine recall below. BaseRate is the share of all spans that are the known entity, \\
     which is the number every rule has to beat."
  )
  invisible(.tab)
}

#' Per-engine recall against the known entity
#' @param .tab Tibble from ent_engine_recall().
#' @return Invisibly .tab.
ent_report_engines <- function(.tab) {
  if (FALSE) .tab <- tab_eng

  cli::cli_h2("Engine recall on the known entity, all documents")
  tbl_say(
    .tab = .tab |>
      dplyr::filter(!.data$Label %in% c("MONEY", "REDACT")) |>
      dplyr::mutate(
        Recall         = tbl_pct(.data$Recall),
        SpansPerAnchor = round(.data$SpansPerAnchor, 1)
      ) |>
      dplyr::select(Label, Combo, DocsWithAnchor, DocsFound, Recall, Spans, SpansPerAnchor) |>
      dplyr::arrange(.data$Label, dplyr::desc(.data$DocsFound))
  )
  cli::cli_alert_info(
    "Read the two columns together. An engine returning many times the spans for the same recall \\
     is louder, not better, and SpansPerAnchor is the adjudication cost per known entity recovered."
  )
  cli::cli_alert_warning(
    "MONEY and REDACT are omitted: neither has an anchor -- EDGAR records no contract value and no \\
     redaction ground truth -- so nothing in this table could be computed for either."
  )
  invisible(.tab)
}

#' How the proposed rules fared
#' @param .tab Tibble from ent_rule_verdict().
#' @param .n Rules listed per label in the detail table.
#' @return Invisibly .tab.
ent_report_rules <- function(.tab, .n = 10L) {
  if (FALSE) {
    .tab <- tab_rules_scored
    .n   <- 10L
  }

  cli::cli_h2("Rule verdicts")
  tbl_say(
    .tab = .tab |>
      dplyr::count(.data$Label, .data$Kind, .data$Verdict, name = "N") |>
      tidyr::pivot_wider(names_from = Verdict, values_from = N, values_fill = 0L)
  )

  cli::cli_h2("Strongest cues among the roles the anchor confirms")
  tbl_say(
    .tab = .tab |>
      dplyr::filter(.data$Kind == "cue", .data$NFire > 0L,
                    !is.na(.data$Direction), .data$Direction == "high") |>
      dplyr::slice_max(.data$Lift, n = .n, by = Label, with_ties = FALSE) |>
      dplyr::mutate(
        Precision = tbl_pct(.data$Precision), Recall = tbl_pct(.data$Recall),
        Lift = round(.data$Lift, 2)
      ) |>
      dplyr::select(Label, Role, Pattern, Side, Window, NFire, Precision, Recall, Lift) |>
      dplyr::arrange(.data$Label, dplyr::desc(.data$Lift))
  )

  low_ <- .tab |>
    dplyr::filter(.data$Kind == "cue", !is.na(.data$Direction), .data$Direction == "low",
                  .data$NFire >= 30L)
  if (nrow(low_) > 0L) {
    cli::cli_h2("Roles the anchor judges by absence")
    tbl_say(
      .tab = low_ |>
        dplyr::slice_min(.data$Lift, n = .n, by = Label, with_ties = FALSE) |>
        dplyr::mutate(Precision = tbl_pct(.data$Precision), Lift = round(.data$Lift, 2)) |>
        dplyr::select(Label, Role, Pattern, Side, NFire, Precision, Lift, Verdict) |>
        dplyr::arrange(.data$Label, .data$Lift)
    )
    cli::cli_alert_info(
      "The filer is a party, so an agent, regulator or statutory cue SHOULD avoid it. Low lift here \\
       is the pass condition; it shows the rule is not firing on parties rather than showing it \\
       correct."
    )
  }

  bad_ <- .tab |> dplyr::filter(stringr::str_detect(.data$Verdict, "^REFUTED"))
  if (nrow(bad_) > 0L) {
    cli::cli_h2("Stops the anchor refutes")
    tbl_say(.tab = bad_ |> dplyr::select(Label, Role, Pattern, NFire, NAnchorFire))
    cli::cli_alert_danger(
      "A stop claims its span is never the entity. These killed known entities, so they are wrong \\
       by their own definition and must not reach 04D."
    )
  }

  moved_ <- .tab |> dplyr::filter(!is.na(.data$SuggestedRole))
  if (nrow(moved_) > 0L) {
    cli::cli_h2("Cues whose role the anchor disputes")
    tbl_say(
      .tab = moved_ |>
        dplyr::mutate(Precision = tbl_pct(.data$Precision), Lift = round(.data$Lift, 2)) |>
        dplyr::select(Label, Role, SuggestedRole, Pattern, NFire, Precision, Lift) |>
        dplyr::arrange(dplyr::desc(.data$Lift))
    )
    cli::cli_alert_info(
      "These are not refuted, they are mislabelled. A cue proposed for performance that fires on \\
       the filer's own address is a party-address cue; it is carried forward with the suggestion \\
       recorded, and 04D decides whether co-occurrence amounts to location."
    )
  }
  invisible(.tab)
}

#' What a window of each shape would keep and cost
#' @param .tab Tibble from ent_window_grid().
#' @return Invisibly .tab.
ent_report_window <- function(.tab) {
  if (FALSE) .tab <- tab_window

  cli::cli_h2("Proportional windows: what a fraction of each document keeps")
  tbl_say(
    .tab = .tab |>
      dplyr::filter(.data$Label != "MONEY") |>
      dplyr::mutate(
        Head = tbl_pct(.data$Head, 0L), Tail = tbl_pct(.data$Tail, 0L),
        DocRecall = tbl_pct(.data$DocRecall), SpansKept = tbl_pct(.data$SpansKept),
        Ratio = round(.data$Ratio, 2)
      ) |>
      dplyr::select(Label, Head, Tail, NAnchorDocs, DocRecall, SpansKept, Ratio)
  )
  cli::cli_alert_info(
    "Read this against the character table, not on its own. Both report DOCUMENT recall, so the \\
     two are directly comparable: where a fraction reaches a given recall for a smaller share of \\
     the candidate pool than any character cap does, the cap is on the wrong axis and length is \\
     doing the work."
  )
  invisible(.tab)
}

#' Whether headings can locate a region of a contract
#' @param .tab Tibble from ent_probe_sections().
#' @return Invisibly .tab.
ent_report_sections <- function(.tab) {
  if (FALSE) .tab <- tab_sections

  cli::cli_h2("Heading probe")
  tbl_say(
    .tab = .tab |>
      dplyr::mutate(dplyr::across(c(PctDocs, MedianPos, P25Pos, P75Pos), \(.x) tbl_pct(.x)))
  )
  cli::cli_alert_info(
    "A heading is usable when it appears in most documents AND sits in a narrow band. Frequent but \\
     scattered locates nothing; narrow but rare covers nothing."
  )
  invisible(.tab)
}


#' What a character cap buys and costs
#' @param .tab Tibble from ent_window_chars().
#' @return Invisibly .tab.
ent_report_window_chars <- function(.tab) {
  if (FALSE) .tab <- tab_wchar

  cli::cli_h2("Character caps: document-level recall against cost")
  tbl_say(
    .tab = .tab |>
      dplyr::filter(.data$Label != "MONEY") |>
      dplyr::mutate(dplyr::across(c(DocRecall, SpansKept, CharsKept), \(.x) tbl_pct(.x)))
  )
  cli::cli_alert_info(
    "DocRecall is what the cap buys: the share of documents in which at least one mention of the \\
     known entity survives. A party has to be found once, not every time it is named, so this is \\
     the number a cap should be chosen on. SpansKept is what 04D must resolve; CharsKept is what \\
     04D must read."
  )
  invisible(.tab)
}

#' The chosen engine per label, and the rule that chose it
#' @param .tab Tibble from ent_choose_engines().
#' @return Invisibly .tab.
ent_report_policy <- function(.tab) {
  if (FALSE) .tab <- tab_policy

  cli::cli_h2("Engine policy")
  tbl_say(
    .tab = .tab |>
      dplyr::mutate(
        Recall         = tbl_pct(.data$Recall),
        SpansPerAnchor = round(.data$SpansPerAnchor, 1)
      )
  )
  cli::cli_alert_info(
    "Read the Basis column before the numbers. A measured row was ranked on recall against the \\
     anchor; a proxy row was chosen on evidence of a different kind, named in Rule; a declared row \\
     rests on nothing but the assertion; an always row is a cheap rule arm carried regardless of \\
     rank. Recall is blank where there is no anchor to compute it against, which is not the same \\
     as a recall of zero."
  )
  n_lab_ <- dplyr::n_distinct(.tab$Label)
  if (nrow(.tab) > n_lab_) {
    cli::cli_alert_info(
      "A label with more than one row runs both engines and takes the UNION of their spans, \\
       deduplicated. Adding an engine cannot narrow the candidate set; it costs spans to resolve."
    )
  }
  invisible(.tab)
}

#' Whether the redaction cues coincide with bracketed markers
#' @param .tab Tibble from ent_redaction_check().
#' @return Invisibly .tab.
ent_report_redaction <- function(.tab) {
  if (FALSE) .tab <- tab_redact

  cli::cli_h2("Money: redaction cues against bracketed markers")
  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("No redaction cues in the rule set.")
    return(invisible(.tab))
  }
  tbl_say(
    .tab = .tab |>
      dplyr::mutate(
        dplyr::across(c(BaseRate, Precision), \(.x) tbl_pct(.x)),
        Lift = round(.data$Lift, 2)
      )
  )
  cli::cli_alert_info(
    "The two signals share no vocabulary: one is a phrase near the amount, the other a bracketed \\
     symbol. Lift above 1 means they agree on where redactions are, which is the only external \\
     check money admits. It speaks to this one role and says nothing about the other fifty-eight \\
     cues."
  )
  invisible(.tab)
}

#' Whether the redaction cues agree with the punctuation the removal left behind
#' @param .tab Tibble from ent_redaction_shape().
#' @return Invisibly .tab.
ent_report_redaction_shape <- function(.tab) {
  if (FALSE) .tab <- tab_shape

  cli::cli_h2("Redaction: proposed role against surviving punctuation")
  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("No redaction cues carry a role the shape test can speak to.")
    return(invisible(.tab))
  }
  tbl_say(
    .tab = .tab |>
      dplyr::filter(.data$NFire > 0L) |>
      dplyr::mutate(
        dplyr::across(c(BaseRate, Precision), \(.x) tbl_pct(.x)),
        Lift = round(.data$Lift, 2)
      ) |>
      dplyr::select(Role, Shape, Pattern, Side, NFire, BaseRate, Precision, Lift)
  )
  cli::cli_alert_info(
    "A currency symbol left standing before the gap, a percent sign after it, a unit noun \\
     following: the characters survived the removal and say what kind of thing was there. Lift \\
     above 1 means the cue and the punctuation agree, which is independent evidence because one is \\
     a phrase and the other is not. Roles no shape can speak to -- a removed party name, a deleted \\
     clause -- are absent from this table rather than scored against a signal that misses them."
  )
  invisible(.tab)
}

#' Which money engines reach a withheld amount
#' @param .tab Tibble from ent_redaction_reach().
#' @return Invisibly .tab.
ent_report_redaction_reach <- function(.tab) {
  if (FALSE) .tab <- tab_reach

  cli::cli_h2("Money engines at redaction sites")
  tbl_say(
    .tab = .tab |> dplyr::mutate(PctCovered = tbl_pct(.data$PctCovered))
  )
  cli::cli_alert_info(
    "A redaction marker is where an amount used to be, so this asks whether an engine can find one \\
     at all -- a capability, not a tuned score. On after_currency the symbol survived and the \\
     number did not, which is the case a transformer structurally cannot tag. On before_unit \\
     nothing marks the gap as money except what it is measured in, and a low figure for every \\
     engine is a statement about the ceiling rather than about any of them."
  )
  invisible(.tab)
}

#' Every 04B report block, in order
#' @param .tab_cov Tibble from ent_anchor_coverage().
#' @param .tab_eng Tibble from ent_engine_recall().
#' @param .tab_rules Tibble from ent_rule_verdict().
#' @param .tab_window Tibble from ent_window_grid().
#' @param .tab_sections Tibble from ent_probe_sections().
#' @param .tab_wchar Tibble from ent_window_chars().
#' @param .tab_policy Tibble from ent_choose_engines().
#' @param .tab_redact Tibble from ent_redaction_check().
#' @param .tab_shape Tibble from ent_redaction_shape().
#' @param .tab_reach Tibble from ent_redaction_reach().
#' @return Invisibly NULL.
ent_report_all <- function(.tab_cov, .tab_eng, .tab_rules, .tab_window, .tab_sections,
                           .tab_wchar, .tab_policy, .tab_redact, .tab_shape, .tab_reach) {
  if (FALSE) {
    .tab_cov      <- tab_cov
    .tab_eng      <- tab_eng
    .tab_rules    <- tab_rules_scored
    .tab_window   <- tab_window
    .tab_sections <- tab_sections
    .tab_wchar    <- tab_wchar
    .tab_policy   <- tab_policy
    .tab_redact   <- tab_redact
    .tab_shape    <- tab_shape
    .tab_reach    <- tab_reach
  }
  ent_report_anchors(.tab = .tab_cov)
  ent_report_engines(.tab = .tab_eng)
  ent_report_rules(.tab = .tab_rules, .n = 10L)
  ent_report_redaction(.tab = .tab_redact)
  ent_report_redaction_shape(.tab = .tab_shape)
  ent_report_redaction_reach(.tab = .tab_reach)
  ent_report_window(.tab = .tab_window)
  ent_report_window_chars(.tab = .tab_wchar)
  ent_report_sections(.tab = .tab_sections)
  ent_report_policy(.tab = .tab_policy)
  invisible(NULL)
}


# 9. Figures ---------------------------------------------------------------------------------------------------------
# Three shapes, all through the shared design layer. They were inline ggplot in the runbook, on
# theme_bw() and at heights off the ladder, which is why they are here now: a figure a reader will
# see in the paper should not be defined in the document that happens to display it.

#' Recall against adjudication cost, per engine
#'
#' The figure the engine policy is read off. Up and to the left is better: the same recall for fewer
#' spans returned. An engine sitting far to the right at the same height as one on the left is
#' paying an order of magnitude in adjudication cost for nothing, and that is the whole argument for
#' preferring a narrow engine over a broad one.
#'
#' The horizontal axis is logarithmic because the engines span two orders of magnitude in spans per
#' anchor. On a linear axis every rule extractor collapses onto the origin and the figure shows only
#' that the transformer is expensive, which was never in doubt.
#'
#' MONEY is excluded rather than drawn empty: it has no anchor, so it has no recall, and a panel of
#' points at an undefined height would invite reading an absence as a zero.
#'
#' @param .tab Tibble from ent_engine_recall().
#' @return A ggplot.
ent_plot_engine_recall <- function(.tab) {
  if (FALSE) .tab <- tab_eng

  .tab |>
    dplyr::filter(.data$Label != "MONEY", !is.na(.data$Recall), .data$SpansPerAnchor > 0) |>
    dplyr::mutate(
      PlotLabel = plot_factor(.data$Label, .key = "Label"),
      PlotCombo = plot_factor(.data$Combo, .key = "Combo", .short = TRUE)
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$SpansPerAnchor, y = .data$Recall, colour = .data$PlotCombo
    )) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~PlotLabel, nrow = 1) +
    ggplot2::scale_x_log10() +
    # A LOWER EXPANSION IS REQUIRED HERE, and its absence is not cosmetic: plot_scale_y_pct()
    # defaults to none, which is right for a bar chart whose bars start at zero and wrong for a
    # scatter, where the lowest point then sits exactly on the axis line and renders half cut off.
    # The worst engine in a panel is precisely the point a reader is looking for.
    plot_scale_y_pct(
      .accuracy = 1,
      .expand   = c(0.05, 0.05),
      .breaks   = scales::breaks_pretty(n = 4)
    ) +
    plot_scale_colour_key(.key = "Combo", .short = TRUE) +
    ggplot2::expand_limits(x = 1) +
    ggplot2::labs(
      x = "Spans returned per anchor recovered (log)",
      y = "Recall on the known entity",
      colour = NULL
    ) +
    plot_theme(.grid = "both", .legend = "bottom")
}

#' What each window shape keeps, against what it costs
#'
#' The diagonal is the null: a window keeping documents and candidates in equal proportion has
#' selected nothing. Distance above it is the enrichment, and a tail line sitting above the
#' tail-of-zero line is the reading session's claim -- that confirmed entities cluster at both ends
#' while the bulk of candidates do not -- confirmed.
#'
#' Both axes are document-level, so this figure and the character table report the same quantity
#' against a relative and an absolute axis. That comparison is the point: if a fraction reaches a
#' given recall for a smaller share of the candidate pool than any character cap does, the cap is on
#' the wrong axis and document length is doing the work.
#'
#' @param .tab Tibble from ent_window_grid().
#' @return A ggplot.
ent_plot_window <- function(.tab) {
  if (FALSE) .tab <- tab_window

  dat_ <- .tab |>
    dplyr::filter(.data$Label != "MONEY") |>
    dplyr::mutate(
      PlotLabel = plot_factor(.data$Label, .key = "Label"),
      # ORDERED BY THE NUMBER, NOT BY THE STRING. factor() on a formatted percentage sorts
      # lexically, which puts the legend in the order 0%, 10%, 20%, 5% -- wrong, and wrong in a way
      # that looks like a rendering quirk rather than a bug. The levels come from the sorted numeric
      # values and the labels are formatted afterwards.
      TailPct = factor(
        scales::label_percent(accuracy = 1)(.data$Tail),
        levels = scales::label_percent(accuracy = 1)(sort(unique(.data$Tail)))
      )
    )

  dat_ |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$SpansKept, y = .data$DocRecall,
      colour = .data$TailPct, group = .data$TailPct
    )) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "#B4B4B4") +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::facet_wrap(~PlotLabel, nrow = 1) +
    # BOTH AXES RUN THE FULL UNIT INTERVAL, because the diagonal is the whole argument of the figure
    # and it only means anything when the two axes share a range starting at zero. Auto-scaled, the
    # vertical ran from 31% and the horizontal from 2%, so the null line appeared in one corner and
    # the prose asking a reader to judge distance above it was asking for something not on the page.
    # The clusters lose some spread; the reference they are being read against gains its meaning.
    plot_scale_x_pct(
      .accuracy = 1,
      .expand   = c(0.02, 0.02),
      .breaks   = scales::breaks_pretty(n = 4)
    ) +
    plot_scale_y_pct(
      .accuracy = 1,
      .expand   = c(0.02, 0.02),
      .breaks   = scales::breaks_pretty(n = 4)
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    plot_scale_colour_cat() +
    ggplot2::labs(
      x = "Share of all candidates kept", y = "Share of documents recovered",
      colour = "Tail"
    ) +
    plot_theme(.grid = "both", .legend = "bottom")
}

#' Rule lift against how often the rule fires
#'
#' The useful region is upper right: a rule that concentrates the known entity AND fires often
#' enough to matter. High lift on a handful of candidates is a curiosity; lift near one at any
#' volume is the base rate wearing a phrase.
#'
#' READ THE TWO DIRECTIONS SEPARATELY, which is why they are coloured rather than pooled. For a
#' high-direction role the pass condition is lift above the reference line; for a low-direction role
#' it is lift below it, because the claim being tested is that the cue AVOIDS the anchor. Points
#' from the two groups are not comparable on the vertical axis and a single cloud would imply they
#' were.
#'
#' Both axes are logarithmic. Firing counts run from tens to hundreds of thousands, and lift is a
#' ratio whose interesting range is symmetric about one on a log scale and badly skewed on a linear
#' one.
#'
#' @param .tab Tibble from ent_rule_verdict().
#' @return A ggplot.
ent_plot_rule_lift <- function(.tab) {
  if (FALSE) .tab <- tab_rules_scored

  .tab |>
    dplyr::filter(
      .data$Kind == "cue", .data$Measurable, .data$NFire > 0L,
      !is.na(.data$Lift), .data$Lift > 0, !is.na(.data$Direction)
    ) |>
    dplyr::mutate(
      PlotLabel = plot_factor(.data$Label, .key = "Label"),
      Pass      = dplyr::if_else(
        .data$Direction == "high", "expected above 1", "expected below 1"
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$NFire, y = .data$Lift, colour = .data$Pass)) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2, colour = "#B4B4B4") +
    ggplot2::geom_point(size = 1.6, alpha = 0.8) +
    ggplot2::facet_wrap(~PlotLabel, nrow = 1) +
    ggplot2::scale_x_log10(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    ggplot2::scale_y_log10(expand = ggplot2::expansion(mult = c(0.05, 0.05))) +
    plot_scale_colour_cat() +
    ggplot2::labs(
      x = "Candidates the rule fires on (log)",
      y = "Lift over base rate (log)",
      colour = NULL
    ) +
    plot_theme(.grid = "both", .legend = "bottom")
}
