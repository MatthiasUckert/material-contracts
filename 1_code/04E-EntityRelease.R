# 04E-EntityRelease: resolve the corpus store into the variables the paper reports -----------------------------------
#
# WHAT THIS FILE DOES, IN ONE PARAGRAPH
# 04D extracted every engine over the whole corpus into one candidate store and resolved nothing.
# This file reads that store, assigns a role to each span from the rules 04B measured, collapses the
# roles into parties, contract dates, party locations and value, and writes one row per contract.
#
# IT IS 04C AT CORPUS SCALE, AND DELIBERATELY THE SAME CODE
# Everything substantive here is sourced from 04C: the resolvers, the role assignment, the
# tie-break, the published-date comparison. What this file adds is chunking and the absence of
# labels. A corpus resolved by a second implementation of 04C would agree with it at the third
# decimal and disagree somewhere that mattered, and nothing would report which.
#
# WHERE THE WINDOW LIVES NOW
# In this document, as a filter over offsets, and that is the whole reason 04D extracts full text.
# A cap applied here can be imposed, revised or withdrawn against evidence that does not exist yet;
# a cap applied at extraction destroys the evidence outside it. The policy still carries CapChars
# and TailChars and this document still honours them -- they are simply no longer irreversible.
#
# WHAT IS STILL OPEN, AND IS NOT PRETENDED OTHERWISE
# The role rules came from reading sessions rather than from measurement. The tie-break between
# competing cues was rewritten once and the rewrite refuted the reason it was made. The expiry cue
# set reaches a date in well under half the corpus even reading every character. None of that is
# settled, and none of it needs re-extracting to change: this document is cheap to re-run.
#
# WHAT NOTHING HERE ESTABLISHES
# That the variables are RIGHT. The anchors confirm the filer appears among the resolved parties and
# that the contract date precedes its filing, which shows the extraction finds real things. It does
# not show that a party count of three is three. That needs a document-level gold set.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .db_path    <- .lP$Input$Store
  .path_rules <- .lP$Input$Kept
}


# 1. Reading the store in chunks -------------------------------------------------------------------------------------
# The store holds every candidate for 1.46 million documents. Resolution is per document, so the
# only thing chunking has to preserve is that a document's spans arrive together.


#' A DuckDB session holding one chunk's candidates in the shape the resolvers expect
#'
#' 04C reads a table called deployed and a table called lens, and gets them by filtering a persistent
#' store through the policy. Here the extraction was already run under the policy, so the candidates
#' ARE the deployed set. Building the same two tables means every resolver below is the code 04C was
#' validated with rather than a corpus variant of it.
#'
#' @param .cands Tibble of candidate spans for this chunk, read from the store 04D wrote.
#' @param .docs Chunk with DocID and Text.
#' @param .path_text Where the chunk's text is written for the resolvers to slice context from.
#' @return A live DBI connection; the caller disconnects.
ent_chunk_session <- function(.cands, .docs, .path_text) {
  if (FALSE) {
    .cands     <- cand_
    .docs      <- docs_
    .path_text <- fs::file_temp(ext = "parquet")
  }

  arrow::write_parquet(tibble::tibble(DocID = .docs$DocID, TextRaw = .docs$Text), .path_text)
  con_ <- DBI::dbConnect(duckdb::duckdb())
  ent_put_table(.con = con_, .name = "deployed", .tab = dplyr::select(
    .cands, DocID, Label, Start, Stop, Span, LabelRaw
  ))
  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TABLE lens AS SELECT DocID, length(TextRaw) AS DocLen ",
    "FROM read_parquet('", as.character(fs::path_abs(.path_text)), "') WHERE length(TextRaw) > 0"
  ))
  con_
}


# 2. Resolution over a chunk -----------------------------------------------------------------------------------------
# Sourced from 04C rather than reimplemented. ent_resolve_chunk() is the corpus wrapper: it builds a
# session over the chunk's candidates, applies 04C's resolvers, and returns one row per document.


#' Resolve one chunk into variable rows, using 04C's resolvers unchanged
#'
#' @param .con Session from ent_chunk_session().
#' @param .path_text The chunk's text parquet.
#' @param .rules Tibble of rules kept by 04B.
#' @param .keys Per-document anchor keys for this chunk.
#' @param .formats strptime formats.
#' @param .ctx_max Context width.
#' @param .head_pos Position under which an uncued organisation still reads as a party.
#' @return Tibble: one row per document.
ent_resolve_chunk <- function(.con, .path_text, .rules, .keys, .formats,
                              .ctx_max = 400L, .head_pos = 0.10) {
  if (FALSE) {
    .con       <- con_
    .path_text <- path_text_
    .rules     <- tab_rules
    .keys      <- keys_
    .formats   <- .DATE_FORMATS
  }

  ent_defined_terms(.con = .con, .path_text = .path_text)
  ent_assign_roles(.con = .con, .path_text = .path_text, .rules = .rules, .ctx_max = .ctx_max)

  ent_assemble(
    .keys     = .keys,
    .parties  = ent_resolve_parties(.con = .con, .head_pos = .head_pos),
    .dates    = ent_resolve_dates(.con = .con, .formats = .formats),
    .places   = ent_resolve_places(.con = .con),
    .value    = ent_resolve_value(.con = .con),
    # The published measure needs every date in the document, and at corpus scale "every date in the
    # document" is every date the deployed engines returned -- there is no unfiltered store to fall
    # back on. The column therefore describes the deployed window, and 04C's sample figure is the
    # one that reproduces the published definition on full text.
    .pubdates = ent_published_dates_chunk(.con = .con, .formats = .formats)
  )
}


#' Maximum date per document, from this chunk's candidates
#' @param .con Session with roles built.
#' @param .formats strptime formats.
#' @return Tibble: DocID, MaxDateAny, NDatesFull.
ent_published_dates_chunk <- function(.con, .formats) {
  if (FALSE) {
    .con     <- con_
    .formats <- .DATE_FORMATS
  }

  fmt_ <- paste0("['", paste(.formats, collapse = "','"), "']")
  DBI::dbGetQuery(.con, paste0(
    "WITH d AS (SELECT DISTINCT DocID, Span FROM deployed WHERE Label = 'DATE'), ",
    "p AS (SELECT DocID, CAST(try_strptime(", ent_sql_dateclean("Span"), ", ", fmt_,
    ") AS DATE) AS D FROM d) ",
    "SELECT DocID, max(D) AS MaxDateAny, COUNT(D) AS NDatesFull FROM p ",
    "WHERE D IS NOT NULL GROUP BY DocID"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(NDatesFull = as.integer(.data$NDatesFull))
}


# 3. Report ----------------------------------------------------------------------------------------------------------


#' What the pass produced and how far it agrees with the EDGAR facts
#' @param .tab Assembled variable rows.
#' @return Invisibly .tab.
ent_report_apply <- function(.tab) {
  if (FALSE) .tab <- tab_vars

  cli::cli_h2("Released variables")
  tbl_say(
    .tab = tibble::tibble(
      Item = c("Documents", "With >=2 parties", "With a contract start", "With a contract end",
               "With an amount", "With a redaction marker", "With a party address"),
      N = c(nrow(.tab),
            sum(.tab$NParties >= 2L, na.rm = TRUE),
            sum(!is.na(.tab$ContractStart)),
            sum(!is.na(.tab$ContractEnd)),
            sum(!is.na(.tab$MaxAmount)),
            sum(dplyr::coalesce(.tab$NRedact, 0L) > 0L),
            sum(dplyr::coalesce(.tab$NPartyAddress, 0L) > 0L))
    ) |>
      dplyr::mutate(Share = tbl_pct(.data$N / nrow(.tab)))
  )

  cli::cli_h2("Consistency flags carried by the release")
  tbl_say(
    .tab = tibble::tibble(
      Check = c("Party set contains the filer", "Contract start at or before the filing date"),
      N     = c(sum(.tab$HasFilerParty, na.rm = TRUE),
                sum(.tab$ContractStart <= .tab$DateFiled, na.rm = TRUE)),
      Of    = c(sum(!is.na(.tab$HasFilerParty)),
                sum(!is.na(.tab$ContractStart) & !is.na(.tab$DateFiled)))
    ) |>
      dplyr::mutate(Share = tbl_pct(.data$N / .data$Of))
  )
  cli::cli_alert_info(
    "These are computed for every released row, not on a sample: the filer's name and its filing \\
     date are EDGAR facts available corpus-wide, so a user can condition on them without re-running \\
     anything."
  )
  invisible(.tab)
}
