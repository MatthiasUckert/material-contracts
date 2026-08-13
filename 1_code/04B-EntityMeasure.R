# 04B-EntityMeasure: locate the contracting party in the text -------------------------------------------------------
#
# WHAT THIS FILE DOES, IN ONE PARAGRAPH
# 04A extracted every engine over the sample and declined to rank anything, because ranking needs
# labels and there are no entity labels. This file supplies the one label EDGAR does record: the
# filer's own name. It finds where that name is written in each contract and reports how far that
# location can be trusted. Nothing else is resolved here yet.
#
# WHY LOCATION RATHER THAN MEMBERSHIP
# The obvious use of the anchor is to ask whether the filer appears SOMEWHERE among a document's
# organisation spans. Measured on this sample that question is passed almost trivially: the engines
# return between 80 and 509 organisations per document, so the base rate is a fraction of a percent
# and a rule scored against it is scored against noise. Asking WHERE the filer is named instead
# raises the base rate inside the answered region by two orders of magnitude, and it produces a
# coordinate rather than a boolean -- which is what the counterparty, the party address and the
# signing date all need.
#
# THE MATCH IS DIRECTIONAL AND THE DIRECTION MATTERS
# A span matches the anchor when one normalised form contains the other. Forward containment (the
# span holds the key) is safe: "ACME HOLDINGS, INC." holds "ACME HOLDINGS". Reverse containment
# (the key holds the span) is not, because it has no length floor -- the single character "G" is
# contained in "GEORGIA PACIFIC", and on this sample that one match fired seventy-three times in one
# document. Around half of every spaCy engine's anchor matches are reverse matches with a median
# length of six characters, so an unguarded reverse arm does not merely add noise: it inflates
# apparent recall, and inflates it more for the engines that emit the most fragments.
#
# POSITION IS THE PRIMARY SORT, QUALITY THE TIE-BREAK
# Ordering candidates by match quality first prefers an exact match at character 8,000 over a
# forward match at character 200, which is backwards for locating a preamble. Position leads and
# quality settles ties.
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
# Registered at SOURCE time, not inside a function, so every figure in this document and any later
# one orders the status axis the same way. The order runs best to worst, which is what makes a
# stacked bar readable without consulting the legend.

plot_register_levels(
  .key    = "PartyStatus",
  .levels = c("located", "late", "fragment only", "no match", "no key"),
  .short  = c("located", "late", "fragment", "no match", "no key")
)


# 2. The anchor key --------------------------------------------------------------------------------------------------
# EDGAR writes a filer's name in registration form and a contract writes it in prose. Neither is
# canonical, so both are reduced to a common form before they are compared.

#' Reduce a company name to a comparable key
#'
#' The corporate suffix is the problem this solves. EDGAR records "BOEING CO" where the contract
#' writes "The Boeing Company", and a comparison that keeps the suffix misses every such pair while
#' a comparison that ignores case alone still fails on the punctuation. Stripping to the distinctive
#' part leaves a key both sides agree on.
#'
#' Suffixes are removed repeatedly rather than once, because "BANK CO LTD" carries three. Keys under
#' five characters are dropped rather than shortened: a three-character key matches a large share of
#' the corpus by containment, and a key that matches everything locates nothing.
#'
#' @param .x Character vector of company names as recorded.
#' @return Character vector of keys, NA where the name reduces to fewer than five characters.
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


#' The same reduction, as a SQL expression
#'
#' Written out rather than applied through an R callback because it runs over several million
#' candidate spans, and a registered R function turns every scan into a single-threaded round trip
#' into the R session. The suffix strip uses "( X)+$" so repeated suffixes go in one pass, which the
#' R version needs a loop for.
#'
#' The two must agree. They are kept beside each other for that reason, and ent_report_keys()
#' reports the share of documents where the key form and the span form fail to meet.
#'
#' @param .col Character. SQL expression yielding the raw span.
#' @return A SQL expression string.
ent_sql_orgkey <- function(.col = "Span") {
  if (FALSE) .col <- "c.Span"

  sfx_ <- paste0("( (INC|INCORPORATED|CORP|CORPORATION|LLC|LLP|LP|LTD|LIMITED|PLC|NV|BV|SA|AG|",
                 "GMBH|CO|COMPANY|TRUST|NA))+$")
  paste0(
    "regexp_replace(regexp_replace(trim(regexp_replace(regexp_replace(upper(", .col,
    "), '[^A-Z0-9 ]', ' ', 'g'), '\\s+', ' ', 'g')), '^THE ', ''), '", sfx_, "', '')"
  )
}


#' Per-document anchor keys, one row per contract
#'
#' Reads 04A's sample table rather than reaching back to the metadata, so this document depends on
#' one narrow artifact and the sample it describes cannot drift from the sample 04A extracted.
#'
#' @param .path_anchors 04A's sample anchors parquet.
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
      AnchorKey = ent_norm_company(.data$CompanyName)
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


#' Open a working session over the candidate store
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
  DBI::dbExecute(con_, paste0("ATTACH '", as.character(fs::path_abs(.db_path)),
                              "' AS s (READ_ONLY)"))
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


# 4. Anchor-matched organisations ------------------------------------------------------------------------------------

#' Every organisation span that matches the filer's own name
#'
#' One row per engine per occurrence, because the same name is proposed by several engines at the
#' same offset and which of them proposed it is a separate question from where it sits.
#'
#' MatchKind is recorded rather than collapsed. The three kinds do not carry the same evidence: an
#' exact match needs no defence, a forward match means the span holds the key plus something else,
#' and a reverse match means the span is a FRAGMENT of the recorded name and may be a single
#' character. Collapsing them is what let an unguarded reverse arm inflate engine recall.
#'
#' @param .con Session from ent_session().
#' @param .quiet Suppress the progress message.
#' @return Invisibly, the number of rows written to the in-session anchor_org table.
ent_anchor_org <- function(.con, .quiet = FALSE) {
  if (FALSE) {
    .con   <- con
    .quiet <- FALSE
  }

  key_ <- ent_sql_orgkey(.col = "c.Span")

  DBI::dbExecute(.con, paste0(
    "CREATE OR REPLACE TABLE anchor_org AS ",
    "WITH occ AS ( ",
    "  SELECT DISTINCT ",
    "    CASE WHEN c.Engine = c.Model THEN c.Engine ELSE c.Engine || ':' || c.Model END AS Combo, ",
    "    c.DocID, c.Start, c.Stop, c.Span, ", key_, " AS SpanKey ",
    "  FROM s.candidates c WHERE c.Label = 'ORG' AND c.Start IS NOT NULL) ",
    "SELECT o.Combo, o.DocID, o.Start, o.Stop, o.Span, o.SpanKey, ",
    "  length(o.SpanKey) AS SpanKeyLen, l.DocLen, ",
    "  ((o.Start + o.Stop) / 2.0) / l.DocLen AS Pos, ",
    "  CASE WHEN o.SpanKey = k.AnchorKey THEN 'exact' ",
    "       WHEN contains(o.SpanKey, k.AnchorKey) THEN 'forward' ",
    "       ELSE 'reverse' END AS MatchKind ",
    "FROM occ o JOIN keys k USING (DocID) JOIN lens l USING (DocID) ",
    "WHERE k.AnchorKey IS NOT NULL AND length(o.SpanKey) > 0 ",
    "  AND (contains(o.SpanKey, k.AnchorKey) OR contains(k.AnchorKey, o.SpanKey))"
  ))

  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM anchor_org")$N
  if (!.quiet) cli::cli_alert_success("{n_} anchor-matched organisation span{?s}.")
  invisible(n_)
}


# 5. The contracting party -------------------------------------------------------------------------------------------

#' One row per document: which span is the contracting party, and how far it can be trusted
#'
#' RUNS FROM THE KEY SIDE, not from the matches. A document where no engine proposed the filer's
#' name is absent from anchor_org entirely, so a join in the other direction would drop it and the
#' coverage figure would be computed over the documents that worked. Five statuses are reported and
#' none is discarded.
#'
#' The candidate ordering is position first and match quality second. Quality-first prefers an exact
#' match deep in the document over a forward match in the preamble, which is the wrong preference
#' for a coordinate: measured on this sample it moved roughly a hundred documents out of the located
#' set and into a signature block.
#'
#' NEngine counts how many engines proposed the CHOSEN span, which is a corroboration figure. It is
#' not the same as asking which engine got there first: several engines routinely return the same
#' offsets, so "first" is decided by whatever the sort falls back on and means nothing.
#'
#' @param .con Session with anchor_org built.
#' @param .keys Tibble from ent_anchor_keys().
#' @param .cap Integer. A chosen span beyond this offset is reported as late rather than located.
#' @param .min_reverse Integer. Shortest reverse match admitted as a candidate.
#' @return Tibble: one row per document, with Status, the chosen span and its offsets.
ent_locate_party <- function(.con, .keys, .cap = 3000L, .min_reverse = 10L) {
  if (FALSE) {
    .con         <- con
    .keys        <- tab_keys
    .cap         <- 3000L
    .min_reverse <- 10L
  }

  found_ <- DBI::dbGetQuery(.con, paste0(
    "WITH usable AS ( ",
    "  SELECT DISTINCT DocID, Start, Stop, Span, SpanKey, SpanKeyLen, MatchKind, Pos, DocLen ",
    "  FROM anchor_org ",
    "  WHERE MatchKind <> 'reverse' OR SpanKeyLen >= ", as.integer(.min_reverse), "), ",
    "best AS ( ",
    "  SELECT * FROM usable ",
    "  QUALIFY row_number() OVER (PARTITION BY DocID ORDER BY Start, ",
    "    CASE MatchKind WHEN 'exact' THEN 1 WHEN 'forward' THEN 2 ELSE 3 END) = 1), ",
    "tot AS (SELECT DocID, COUNT(*) AS NMatch FROM anchor_org GROUP BY DocID), ",
    "agree AS ( ",
    "  SELECT b.DocID, COUNT(DISTINCT a.Combo) AS NEngine ",
    "  FROM best b JOIN anchor_org a ",
    "    ON a.DocID = b.DocID AND a.Start = b.Start AND a.Stop = b.Stop ",
    "  GROUP BY b.DocID) ",
    "SELECT t.DocID, t.NMatch, b.Start, b.Stop, b.Span, b.SpanKey, b.SpanKeyLen, ",
    "  b.MatchKind, b.Pos, b.DocLen, g.NEngine ",
    "FROM tot t LEFT JOIN best b USING (DocID) LEFT JOIN agree g USING (DocID)"
  )) |>
    tibble::as_tibble()

  .keys |>
    dplyr::left_join(found_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      NMatch = as.integer(dplyr::coalesce(.data$NMatch, 0L)),
      Status = dplyr::case_when(
        is.na(.data$AnchorKey)                     ~ "no key",
        !is.na(.data$Start) & .data$Start < .cap   ~ "located",
        !is.na(.data$Start)                        ~ "late",
        .data$NMatch > 0L                          ~ "fragment only",
        TRUE                                       ~ "no match"
      ),
      Party = ent_trim_party(.x = .data$Span),
      # A span the engines returned with its neighbours attached is still a correct location. The
      # flag says the name needed work, so the two failure modes -- wrong place, and right place
      # with a ragged boundary -- can be counted apart rather than pooled.
      Leaked = !is.na(.data$Span) &
        .data$Party != stringi::stri_trim_both(
          stringi::stri_replace_all_regex(dplyr::coalesce(.data$Span, ""), "\\s+", " ")
        )
    ) |>
    dplyr::relocate(Status, Party, .after = DocID)
}


#' Strip the connective the engines attach to the front of a party name
#'
#' The extractors do not stop cleanly at a name. They return "between Precision BioSciences, Inc.",
#' "amongAMERICAN CASINO & ENTERTAINMENT PROPERTIES LLC", "the Common Stock of eHealth, Inc." In
#' every one of those the LOCATION is right and only the boundary is wrong, so the leading
#' connective is removed rather than the span rejected.
#'
#' The trailing boundary is deliberately left alone. Spans also split at internal punctuation --
#' "MEDTRONIC" from "MEDTRONIC, INC." -- so a trailing rule has to extend a span as often as cut
#' one, and knowing where the name stops needs the counterparty rule that does not exist yet.
#'
#' @param .x Character vector of raw spans.
#' @return Character vector, whitespace collapsed and the leading connective removed.
ent_trim_party <- function(.x) {
  if (FALSE) .x <- c("between Precision BioSciences, Inc.", "AND TEEKAY LNG PARTNERS L.P.")

  lead_ <- paste0(
    "^\\s*(by and between|by and among|made by and between|among|amongst|between|and|with|",
    "the|this|dated|as of)\\b[\\s,]*"
  )
  .x |>
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_replace_first_regex(
      lead_, "",
      opts_regex = stringi::stri_opts_regex(case_insensitive = TRUE)
    ) |>
    stringi::stri_trim_both()
}


# 6. Reading the spans back ------------------------------------------------------------------------------------------
# Every table above is equally consistent with the chosen span being a preamble party and with it
# being a letterhead, a filing footer or a defined term. Only the text distinguishes them.

#' Rehydrate a sample of located spans with the text either side
#'
#' Sampled deterministically so the same documents appear on every render and a change in the
#' examples means a change in the extraction rather than a change in the draw.
#'
#' @param .tab Tibble from ent_locate_party().
#' @param .path_text 04A's canonical text parquet.
#' @param .n Integer. Documents to draw.
#' @param .ctx Integer. Characters either side of the span.
#' @param .status Character. Which status to draw from.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: CompanyName, Party, MatchKind, Start, NEngine, Class and a Snippet.
ent_party_windows <- function(.tab, .path_text, .n = 20L, .ctx = 200L,
                              .status = "located", .seed = 42L) {
  if (FALSE) {
    .tab       <- tab_party
    .path_text <- .lP$Input$Text
    .n         <- 20L
    .ctx       <- 200L
    .status    <- "located"
    .seed      <- 42L
  }

  pick_ <- .tab |>
    dplyr::filter(.data$Status == .status) |>
    (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n, nrow(.d)))))()

  if (nrow(pick_) == 0L) {
    cli::cli_alert_warning("No document{?s} with status {(.status)} to show.")
    return(tibble::tibble())
  }

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
          stringi::stri_sub(.data$TextRaw, from = pmax(1L, .data$Start + 1L - .ctx),
                            to = .data$Start),
          " >>>",
          stringi::stri_sub(.data$TextRaw, from = .data$Start + 1L, to = .data$Stop),
          "<<< ",
          stringi::stri_sub(.data$TextRaw, from = .data$Stop + 1L, to = .data$Stop + .ctx),
          "..."
        ),
        "\\s+", " "
      )
    ) |>
    dplyr::select(DocID, CompanyName, Party, MatchKind, Start, NEngine, Class, Snippet)
}


# 7. Coverage --------------------------------------------------------------------------------------------------------

#' Status breakdown by contract type
#'
#' Never pooled. A variable present in half of leases and nine tenths of equity agreements is two
#' variables, and the column that matters is which contract types can carry it at all.
#'
#' @param .tab Tibble from ent_locate_party().
#' @return Tibble: Class, Status, N and the within-class share.
ent_party_by_class <- function(.tab) {
  if (FALSE) .tab <- tab_party

  .tab |>
    dplyr::count(.data$Class, .data$Status, name = "N") |>
    dplyr::mutate(Share = .data$N / sum(.data$N), .by = Class)
}


# 8. Report ----------------------------------------------------------------------------------------------------------
# Report functions print through cli and return their tibble invisibly, so every number stays
# available for further work after it has been displayed.

#' How many documents carry a usable key at all
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
    "'located' is the working set. The other four are reported rather than dropped, because a \\
     coverage figure computed over the documents that worked is not a coverage figure. 'no match' \\
     is an extraction gap, 'fragment only' a matching one, and they call for different fixes."
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
      MedKeyLen  = stats::median(.data$SpanKeyLen),
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
    "MedEngines is corroboration: how many of the engines proposed the very same offsets. A value \\
     of one on a reverse match is the row to distrust, because nothing else saw that span and the \\
     span is a fragment of the recorded name rather than the name."
  )
  invisible(out_)
}


#' Located spans read against the company EDGAR recorded
#' @param .tab Tibble from ent_party_windows().
#' @return Invisibly .tab.
ent_report_windows <- function(.tab) {
  if (FALSE) .tab <- tab_windows

  if (nrow(.tab) == 0L) return(invisible(.tab))

  cli::cli_h2("EDGAR's company against the span found for it")
  purrr::pwalk(
    dplyr::select(.tab, CompanyName, Party, MatchKind, Start, NEngine, Snippet),
    function(CompanyName, Party, MatchKind, Start, NEngine, Snippet) {
      cli::cli_h3("{CompanyName}")
      cat("  found  : ", Party, "\n", sep = "")
      cat("  match  : ", MatchKind, " at char ", Start, ", ", NEngine, " engine(s)\n", sep = "")
      cat("  context: ", Snippet, "\n\n", sep = "")
    }
  )
  cli::cli_alert_info(
    "Read for two things. Whether the span is the filer's name, which is precision; and what kind \\
     of document surrounds it, which decides whether a counterparty exists to be found at all."
  )
  invisible(.tab)
}


#' Located share by contract type
#' @param .tab Tibble from ent_party_by_class().
#' @return Invisibly the located rows.
ent_report_class <- function(.tab) {
  if (FALSE) .tab <- tab_class

  out_ <- .tab |>
    dplyr::select(Class, Status, N) |>
    tidyr::pivot_wider(names_from = Status, values_from = N, values_fill = 0L) |>
    dplyr::mutate(
      Docs      = rowSums(dplyr::pick(dplyr::where(is.numeric))) |> as.integer(),
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
     extraction, and the windows above are what tells the two apart."
  )
  invisible(out_)
}


#' Every report block in this document, in order
#' @param .keys Tibble from ent_anchor_keys().
#' @param .party Tibble from ent_locate_party().
#' @param .class Tibble from ent_party_by_class().
#' @return Invisibly NULL.
ent_report_all_party <- function(.keys, .party, .class) {
  if (FALSE) {
    .keys  <- tab_keys
    .party <- tab_party
    .class <- tab_class
  }
  ent_report_keys(.tab = .keys)
  ent_report_party(.tab = .party)
  ent_report_match(.tab = .party)
  ent_report_class(.tab = .class)
  invisible(NULL)
}


# 9. Figures ---------------------------------------------------------------------------------------------------------

#' Status composition by contract type
#'
#' Shares rather than counts, because the classes differ in size by an order of magnitude and a
#' count plot would be a picture of the taxonomy instead of the coverage. The status order runs best
#' to worst from the left, so the located segment is a bar length that can be compared by eye.
#'
#' @param .tab Tibble from ent_party_by_class().
#' @param .key_class Character. Registered vocabulary the rows are ordered by.
#' @return A ggplot.
ent_plot_party_status <- function(.tab, .key_class = "ClassDetailed") {
  if (FALSE) {
    .tab       <- tab_class
    .key_class <- "ClassDetailed"
  }
  plot_bar_stacked(
    .tab      = .tab,
    .cat      = "Class",
    .val      = "N",
    .fill     = "Status",
    .key      = .key_class,
    .key_fill = "PartyStatus",
    .share    = TRUE
  )
}
