# 01E-CtoExhibits: parse confidential-treatment orders and link them to the contracts they cover ------------------------
#
# WHAT THIS FILE DOES
# A confidential-treatment order is a letter from the SEC granting a filer permission to withhold
# parts of an exhibit from public view, for a stated period. It names the filing whose exhibits were
# redacted, the exhibit numbers, and the date each becomes public. This parses those letters and
# links each reference back to the contract it covers.
#
# IT IS A JOIN, NOT A MATCHING PROBLEM
# The letters are a generated form: one template, unchanged from 2008 to 2024, and the text in the
# corpus is clean because 01B parsed it once. Nearly every order names the source form type and the
# source filing date, in one of a handful of wordings, so the link is CIK plus form plus date into
# the master index, then exhibit number into the filing. Two exact hops. Nothing here is fuzzy, and
# where a hop fails the reference is recorded with the reason rather than dropped.
#
# THREE REFERENCE FORMATS, AND THE LATER TWO CARRY MORE
# Most orders list "Exhibit 10.15 through December 31, 2016" and name the source filing once in the
# opening paragraph. A minority name the source filing per exhibit, either inline or as a table, and
# those are the orders covering exhibits from several filings. The per-reference form is strictly
# better data: it removes the ambiguity of one order spanning four filings, which is why the parser
# tries the specific formats before the general one rather than after.
#
# STATUS IS READ FROM THE TITLE, NOT THE BODY
# "ORDER GRANTING CONFIDENTIAL TREATMENT" is the title line and says what the order does. Searching
# the body for "amend" instead finds "to a Form 10-K filed on August 23, 2018, as amended", which
# describes the source filing and not the order, and marks a fifth of plain grants as amendments.
# Denials and revocations are parsed and counted like grants; the export applies the paper's
# definition and keeps only what was granted.
#
# ONE SPELLING ON BOTH SIDES, AND ONE IMPLAUSIBLE DATE
# Exhibit numbers are normalised on the order side exactly as on the filing side, so 10.08 meets 10.8
# and 10.4a meets 10.4A. A release date more than thirty years after the order is a typo in the
# letter -- one exists -- and is set to missing and flagged rather than corrected. The horizon runs
# from the order, not the source filing: a grant extended in 2019 on a 1994 exhibit is ordinary.
#
# WHAT IS RECOMPUTED, AND WHAT IS NOT
# The linkage joins against two tables that dwarf everything else here: every filing EDGAR ever
# published, and every Exhibit 10 in the corpus. Collecting the first alone is the single most
# expensive thing this document does -- the orders themselves are sixteen thousand kilobyte letters.
# Both are a deterministic function of 01A's mirror and 01C's consolidated table, so cto_link_tables()
# takes .rerun, defaulting to FALSE, and fingerprints both before deciding.
#
# The output write is guarded too, and not for its size. 02B reads CtoExhibits.parquet, and a
# fingerprint downstream is only as stable as the modification time of the file it points at.
#
# FIGURES ARE DEFINED HERE AND WRITTEN NOWHERE. The document displays what cto_plot_linkage()
# returns; the consolidated release script writes the files the manuscript needs.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation.

if (FALSE) {
  .path     <- tab_orders$Path[1]
  .text     <- cto_read_order(tab_orders$Path[1])
  .tab_ref  <- tab_refs
  .tab_mst  <- tab_master
  .tab_docs <- tab_exhibits
}


# 1. Vocabulary --------------------------------------------------------------------------------------------------------

#' The pieces of a date and an exhibit number, as the orders write them
#'
#' Written once and reused by all three reference formats, because three copies of "what an exhibit
#' number looks like" is three chances for the formats to disagree about which references exist.
#'
#' The exhibit pattern allows sub-numbering (10.18.1), a trailing letter (10.4A) and a parenthesised
#' suffix (10.26(xxxi)). The last of these cannot be matched to a contract -- the filing side writes
#' those as an unnumbered EX-10 -- but parsing them is what makes them countable.
#'
#' THE DATE TOLERATES A STRAY COMMA. "April, 30, 2009" appears in the letters; the comma is removed
#' again by cto_parse_date(), which is the only place a matched date is turned into one.
#'
#' THE FORM COMES IN THREE SHAPES. Form captures the type after the word "Form"; FormAny is the same
#' shape without a capture, for patterns that need to step over a form on the way to a date; and
#' FormBare is the type standing alone -- "to a 10-Q filed on" -- which is narrower by construction,
#' because without the word "Form" in front of it any capitalised token would qualify.
#'
#' @return Named character vector of pattern fragments.
cto_patterns <- function() {
  form_ <- "[A-Z0-9][A-Z0-9/-]{0,9}"

  c(
    Exhibit  = "([0-9]+(?:\\.[0-9]+)*[A-Za-z]?(?:\\([A-Za-z0-9]+\\))?)",
    Date     = "([A-Za-z]+,?\\s+[0-9]{1,2},\\s*[0-9]{4})",
    Form     = paste0("(", form_, ")"),
    FormAny  = paste0("(?:", form_, ")"),
    FormBare = "((?:[0-9]{1,2}-[A-Z0-9]{1,4}|[A-Z]{1,2}-[0-9]{1,2})(?:/A)?)"
  )
}

#' Turn a matched date into a Date
#'
#' One parser for every date the patterns capture, so the tolerance for a stray comma after the
#' month lives in one place and a date that fails to parse becomes NA in the same way everywhere.
#'
#' @param .x Character vector of dates as the letters write them, or NA.
#' @return Date vector.
cto_parse_date <- function(.x) {
  if (FALSE) .x <- c("December 31, 2016", "April, 30, 2009", NA_character_)

  suppressWarnings(as.Date(
    stringi::stri_replace_first_regex(.x, "^([A-Za-z]+),", "$1"), format = "%B %d, %Y"
  ))
}

#' Read one order, whitespace normalised
#'
#' @param .path Path to one parsed document.
#' @return A one-row tibble: DocID, Text.
cto_read_order <- function(.path) {
  if (FALSE) .path <- tab_orders$Path[1]

  tab_ <- arrow::read_parquet(file = .path, col_select = dplyr::all_of(c("DocID", "TextRaw")))

  tibble::tibble(
    DocID = tab_$DocID[1L],
    Text  = stringi::stri_replace_all_regex(tab_$TextRaw[1L], "\\s+", " ")
  )
}


# 2. Order-level fields ------------------------------------------------------------------------------------------------

#' What the order is, and which filing it concerns
#'
#' STATUS COMES FROM THE TITLE. The line reads "ORDER GRANTING CONFIDENTIAL TREATMENT" or "ORDER
#' DENYING..."; the verb between the two is what the order does. A denial matters out of proportion
#' to its frequency: it means the filer applied to withhold and was refused, and therefore had to
#' disclose.
#'
#' AN EXTENSION IS A SPECIFIC PHRASE. "requesting an extension of a previous grant" identifies one;
#' the bare word "extension" appears in orders that are nothing of the kind. An extension still names
#' the original filing, so it links to the same contract by the same join, and the flag records that
#' an earlier order exists rather than changing how the reference is resolved.
#'
#' THE RULE IS RECORDED BUT DOES NOTHING HERE. Rule 24b-2 is the Exchange Act route and Rule 406 the
#' Securities Act one, which is to say periodic reports against registration statements. It is
#' carried because that distinction may matter to an analysis, not because the parser needs it.
#'
#' THE SOURCE FILING HAS A HANDFUL OF WORDINGS, AND ALL OF THEM ARE READ. "to a Form 10-K filed on"
#' is the template, but the letters also write "to their Forms 10-Q filed on" for two registrants,
#' "to a registration statement on Form S-1 filed on", "to an amended Form 10-K filed on", "to a 10-Q
#' filed on" with no word Form at all, and "to a Form 10-K on" with no word filed. Each of those left
#' an order whose references parsed but had no filing to join to. The form is read first, allowing the
#' article to be absent or possessive and up to five words to sit before "Form", or a bare form type
#' followed by "filed"; a form type carries a digit, and requiring one is what keeps "to a Form filed
#' on" from returning FILED as the type. The date is read by two patterns -- directly after the form
#' with "filed on" optional, and after "filed on" anywhere -- and THE EARLIER MATCH IN THE LETTER
#' WINS. The source filing is named in the application sentence at the top; a footnote further down
#' names a different one -- "refiled with fewer redactions as Exhibit 10.13 to a Form 10-Q filed on
#' May 6, 2010" -- and a fixed precedence between the patterns handed nineteen orders that later
#' date when their source was written "for the fiscal year ended ..., filed on".
#'
#' What stays unnamed is a post-effective amendment cited without a form type, and an order
#' covering two registrants' filings on two dates, where one source filing is the wrong answer
#' rather than a missing one.
#'
#' @param .text Character. One order, whitespace normalised.
#' @return A one-row tibble of order-level fields.
cto_order_fields <- function(.text) {
  if (FALSE) .text <- cto_read_order(tab_orders$Path[1])$Text

  pat_ <- cto_patterns()

  title_ <- stringi::stri_match_first_regex(
    .text, "(?i)ORDER\\s+([A-Z]+(?:\\s+[A-Z]+)*?)\\s+CONFIDENTIAL\\s+TREATMENT"
  )[, 2L]

  rule_ <- stringi::stri_extract_all_regex(.text, "(?i)Rule\\s+(?:24b-2|406)")[[1L]]
  rule_ <- if (all(is.na(rule_))) NA_character_ else {
    paste(sort(unique(toupper(gsub("(?i)Rule\\s+", "", rule_, perl = TRUE)))), collapse = "|")
  }

  # Group 2 is the type after the word Form, group 3 the bare type; at most one of them is set.
  form_ <- stringi::stri_match_first_regex(
    .text,
    paste0(
      "(?i)to\\s+(?:(?:an?|the|their|its)\\s+)?(?:",
      "(?:\\S+\\s+){0,5}?Forms?\\s+(?=[A-Z0-9/-]*[0-9])", pat_[["Form"]],
      "|", pat_[["FormBare"]], "\\s+(?:as\\s+amended\\s+)?filed\\b",
      ")"
    )
  )
  form_ <- dplyr::coalesce(form_[, 2L], form_[, 3L])

  # Two patterns, and the one matching earliest in the letter wins; see the roxygen for why.
  rex_date_ <- c(
    paste0("(?i)Forms?\\s+", pat_[["FormAny"]], "\\s+(?:filed\\s+)?(?:on\\s+)?", pat_[["Date"]]),
    paste0("(?i)filed\\s+(?:on\\s+)?", pat_[["Date"]])
  )
  pos_  <- purrr::map_int(rex_date_, function(.r) stringi::stri_locate_first_regex(.text, .r)[1L, 1L])
  date_ <- if (all(is.na(pos_))) {
    NA_character_
  } else {
    stringi::stri_match_first_regex(.text, rex_date_[[which.min(pos_)]])[, 2L]
  }

  tibble::tibble(
    Status        = toupper(trimws(title_)),
    Rule          = rule_,
    SourceForm    = toupper(trimws(form_)),
    SourceFiledOn = cto_parse_date(.x = date_),
    IsExtension   = as.integer(grepl("(?i)requesting\\s+an?\\s+extension", .text)),
    SourceAmended = as.integer(grepl("(?i)filed[^.]{0,60},\\s*as\\s+amended", .text)),
    nRegistrants  = stringi::stri_count_regex(.text, "(?i)File\\s+Nos?\\."),
    nChars        = nchar(.text)
  )
}


# 3. Exhibit references ------------------------------------------------------------------------------------------------

#' References that name their own source filing, inline
#'
#' "Exhibit 10.20 to Form 8-K/A filed February 14, 2018 through December 27, 2027". Used when one
#' order covers exhibits from more than one filing, which is exactly when the order-level source
#' filing would be wrong.
#'
#' @param .text Character. One order, whitespace normalised.
#' @return A tibble: ExhibitNo, RefForm, RefFiledOn, ReleaseDate.
cto_refs_inline <- function(.text) {
  if (FALSE) .text <- cto_read_order(tab_orders$Path[1])$Text

  pat_ <- cto_patterns()
  rex_ <- paste0(
    "(?i)Exhibits?\\s+", pat_[["Exhibit"]],
    "\\s+to\\s+(?:an?\\s+)?Forms?\\s+", pat_[["Form"]],
    "\\s+filed\\s+(?:on\\s+)?", pat_[["Date"]],
    "\\s+through\\s+", pat_[["Date"]]
  )

  m_ <- stringi::stri_match_all_regex(.text, rex_)[[1L]]
  if (all(is.na(m_))) return(tibble::tibble())

  tibble::tibble(
    ExhibitNo   = m_[, 2L],
    RefForm     = toupper(m_[, 3L]),
    RefFiledOn  = cto_parse_date(.x = m_[, 4L]),
    ReleaseDate = cto_parse_date(.x = m_[, 5L]),
    RefFormat   = "inline"
  )
}

#' References set out as a table
#'
#' The header reads "Exhibit to Form Filed on Confidential Treatment Granted" and each row is a bare
#' number followed by its form, filing date and release date. The word "Exhibit" appears once, in the
#' header, so a pattern anchored on it finds nothing.
#'
#' @param .text Character. One order, whitespace normalised.
#' @return A tibble: ExhibitNo, RefForm, RefFiledOn, ReleaseDate.
cto_refs_table <- function(.text) {
  if (FALSE) .text <- cto_read_order(tab_orders$Path[1])$Text

  if (!grepl("(?i)Exhibit\\s+to\\s+Form\\s+Filed\\s+on", .text)) return(tibble::tibble())

  pat_ <- cto_patterns()
  rex_ <- paste0(
    "(?i)\\b", pat_[["Exhibit"]],
    "\\s+", pat_[["Form"]],
    "\\s+", pat_[["Date"]],
    "\\s+through\\s+", pat_[["Date"]]
  )

  m_ <- stringi::stri_match_all_regex(.text, rex_)[[1L]]
  if (all(is.na(m_))) return(tibble::tibble())

  tibble::tibble(
    ExhibitNo   = m_[, 2L],
    RefForm     = toupper(m_[, 3L]),
    RefFiledOn  = cto_parse_date(.x = m_[, 4L]),
    ReleaseDate = cto_parse_date(.x = m_[, 5L]),
    RefFormat   = "table"
  )
}

#' The listing block, without the application paragraph that precedes it
#'
#' AN ORDER HAS TWO HALVES AND ONLY THE SECOND GRANTS ANYTHING. The first recites what the filer
#' applied for -- "requesting confidential treatment for information it excluded from Exhibit 10.1
#' to a Form 8-K filed on May 14, 2012" -- and the second lists what was granted. Both name exhibits,
#' so a pattern applied to the whole letter reads the application as though it were the grant, and
#' returns a reference with no release date alongside the real one.
#'
#' The two are separated by the sentence that introduces the listing, which every order carries in
#' one wording or another. Where it is absent the whole text is used, because a missing separator is
#' better handled by over-reading than by returning nothing.
#'
#' @param .text Character. One order, whitespace normalised.
#' @return Character. The text from the listing sentence onwards.
cto_listing_block <- function(.text) {
  if (FALSE) .text <- cto_read_order(tab_orders$Path[1])$Text

  pos_ <- stringi::stri_locate_last_regex(
    .text, "(?i)will\\s+not\\s+be\\s+released|for\\s+the\\s+time\\s+periods?\\s+specified"
  )[, 1L]

  if (is.na(pos_)) return(.text)
  stringi::stri_sub(.text, pos_, nchar(.text))
}

#' References that rely on the order-level source filing
#'
#' "Exhibit 10.15 through December 31, 2016" -- the common case. The source filing is whatever the
#' opening paragraph named.
#'
#' THE PLURAL IS ALLOWED AND THE RELEASE DATE IS NOT REQUIRED. Some orders write "Exhibits 10.1
#' through March 31, 2015" even for a single exhibit, and a pattern anchored on the singular fails at
#' the letter s. Orders written after the 2019 amendments often grant treatment with no expiry at all
#' -- "excluded information from the following exhibit will not be released to the public: Exhibit
#' 99.1" -- so requiring a date discards the whole of the recent period.
#'
#' @param .text Character. One order, whitespace normalised.
#' @return A tibble: ExhibitNo, ReleaseDate.
cto_refs_plain <- function(.text) {
  if (FALSE) .text <- cto_read_order(tab_orders$Path[1])$Text

  pat_ <- cto_patterns()
  rex_ <- paste0(
    "(?i)Exhibits?\\s+", pat_[["Exhibit"]],
    "(?:\\s*(?:through|until)\\s+", pat_[["Date"]], ")?"
  )

  m_ <- stringi::stri_match_all_regex(cto_listing_block(.text = .text), rex_)[[1L]]
  if (all(is.na(m_))) return(tibble::tibble())

  tibble::tibble(
    ExhibitNo   = m_[, 2L],
    RefForm     = NA_character_,
    RefFiledOn  = as.Date(NA),
    ReleaseDate = cto_parse_date(.x = m_[, 3L]),
    RefFormat   = "plain"
  )
}


#' References named in a denial or revocation
#'
#' A denial is not a listing block. It says "denied your request for confidential treatment of the
#' information excluded from Exhibit 4.1 to the Form 8-K filed on December 11, 2007" -- exhibit and
#' filing both named, in a sentence, with no release date because nothing was granted.
#'
#' There are ten denials and one revocation in sixteen years; four of their references link, to
#' three contracts. That is too few to support the question they raise -- whether a refused
#' application led to disclosure -- so they are parsed and counted here, and the export keeps only
#' what was granted.
#'
#' @param .text Character. One order, whitespace normalised.
#' @return A tibble: ExhibitNo, RefForm, RefFiledOn, ReleaseDate.
cto_refs_denial <- function(.text) {
  if (FALSE) .text <- cto_read_order(tab_orders$Path[1])$Text

  pat_ <- cto_patterns()
  rex_ <- paste0(
    "(?i)excluded\\s+from\\s+Exhibits?\\s+", pat_[["Exhibit"]],
    "\\s+to\\s+(?:an?|the)\\s+Forms?\\s+", pat_[["Form"]],
    "\\s+filed\\s+(?:on\\s+)?", pat_[["Date"]]
  )

  m_ <- stringi::stri_match_all_regex(.text, rex_)[[1L]]
  if (all(is.na(m_))) return(tibble::tibble())

  tibble::tibble(
    ExhibitNo   = m_[, 2L],
    RefForm     = toupper(m_[, 3L]),
    RefFiledOn  = cto_parse_date(.x = m_[, 4L]),
    ReleaseDate = as.Date(NA),
    RefFormat   = "denial"
  )
}


# 4. One order ---------------------------------------------------------------------------------------------------------

#' Parse one order into its references
#'
#' THE SPECIFIC FORMATS ARE TRIED FIRST. An order whose references name their own filing must not be
#' read by the general pattern, because the general pattern would attach every exhibit to the one
#' filing named in the opening paragraph -- which for these orders is one of several. First match
#' wins, and the format that matched is recorded so the mix can be reported.
#'
#' AN ORDER WITH NO PARSABLE REFERENCE STILL RETURNS A ROW. The order-level fields are known even
#' when the listing block is not, and a missing row is indistinguishable from an order that was never
#' read.
#'
#' THE EXHIBIT NUMBER LEAVES HERE IN THE SPELLING THE JOIN USES. cto_link_tables() normalises the
#' filing side; doing the same here is what lets 10.08 meet 10.8 and 10.4a meet 10.4A. Left raw, those
#' references fell into the category reserved for exhibits incorporated by reference, where a
#' spelling mismatch is invisible.
#'
#' @param .path Path to one parsed document.
#' @return A tibble, one row per reference, or one row with a missing exhibit number.
cto_parse_order <- function(.path) {
  if (FALSE) .path <- tab_orders$Path[1]

  doc_ <- cto_read_order(.path)
  fld_ <- cto_order_fields(.text = doc_$Text[1L])

  # The denial pattern reads a sentence rather than a listing block, and the sentence it reads also
  # appears in the application paragraph of an ordinary grant. Running it only on orders whose title
  # says they denied something is what stops it claiming ninety-five grants it has no business
  # touching.
  deny_ <- !is.na(fld_$Status[1L]) && fld_$Status[1L] %in% c("DENYING", "REVOKING")

  ref_ <- cto_refs_inline(.text = doc_$Text[1L])
  if (nrow(ref_) == 0L) ref_ <- cto_refs_table(.text = doc_$Text[1L])
  if (nrow(ref_) == 0L && isTRUE(deny_)) ref_ <- cto_refs_denial(.text = doc_$Text[1L])
  if (nrow(ref_) == 0L) ref_ <- cto_refs_plain(.text = doc_$Text[1L])

  if (nrow(ref_) == 0L) {
    ref_ <- tibble::tibble(
      ExhibitNo   = NA_character_,
      RefForm     = NA_character_,
      RefFiledOn  = as.Date(NA),
      ReleaseDate = as.Date(NA),
      RefFormat   = "none"
    )
  }

  dplyr::bind_cols(tibble::tibble(DocID = doc_$DocID[1L]), fld_, ref_) |>
    dplyr::mutate(
      ExhibitNo  = cto_exhibit_number(.x = .data$ExhibitNo),
      # A reference that names its own filing overrides the one from the opening paragraph.
      UseForm    = dplyr::coalesce(.data$RefForm, .data$SourceForm),
      UseFiledOn = dplyr::coalesce(.data$RefFiledOn, .data$SourceFiledOn),
      Series     = stringi::stri_extract_first_regex(.data$ExhibitNo, "^[0-9]+")
    )
}


#' Set aside a release date no grant could carry
#'
#' A RELEASE DATE THIRTY YEARS PAST THE ORDER IS A TYPO, NOT A GRANT. One letter in the corpus writes
#' "through August 3, 3036" where its fifteen sibling exhibits say 2026. The date is set to missing
#' and the row flagged; it is not corrected, because which digit was wrong is a guess and a corrected
#' value would be indistinguishable from a parsed one.
#'
#' THE HORIZON RUNS FROM THE ORDER, NOT FROM THE SOURCE FILING. A grant can be extended for decades:
#' an order of 2019 covering an exhibit filed in 1994 through 2024 is ordinary, and measured from the
#' filing it would have been flagged. Measured from the order it is five years, and the typo is a
#' thousand. This is why the guard lives after the join to the order's own filing date rather than
#' inside the parser, which does not know it.
#'
#' @param .tab Parsed references carrying ReleaseDate and the order's DateFiled.
#' @param .years Integer. Years after the order beyond which a release date is set aside.
#' @return .tab with ReleaseImplausible added and ReleaseDate set to NA where it is 1.
cto_guard_release <- function(.tab, .years = 30L) {
  if (FALSE) {
    .tab   <- tab_refs
    .years <- 30L
  }

  .tab |>
    dplyr::mutate(
      ReleaseImplausible = as.integer(
        !is.na(.data$ReleaseDate) & !is.na(.data$DateFiled) &
          .data$ReleaseDate > .data$DateFiled + .years * 365L
      ),
      ReleaseDate = dplyr::if_else(.data$ReleaseImplausible == 1L, as.Date(NA), .data$ReleaseDate)
    )
}


# 4b. What the linkage joins against ------------------------------------------------------------------------------------

#' The two tables every reference is resolved against
#'
#' THE MASTER INDEX IS THE EXPENSIVE ONE, AND IT IS THE FULL ONE. Every filing EDGAR published, not
#' only those we hold documents from: the restricted index is two and a half per cent of it, and
#' joining against that reported half of the linkage failures as filings EDGAR had never heard of
#' when they were indexed all along. Collecting it is minutes and hundreds of megabytes, and its
#' contents cannot change unless the mirror does.
#'
#' THE EXHIBIT TABLE IS BUILT HERE RATHER THAN AFTER THE FILING HOP, because which filings hold
#' exhibits is what breaks a tie between two filings of the same type on the same day.
#'
#' BOTH ARE SORTED. An Arrow scan makes no promise about the order in which record batches come back,
#' so an unsorted collect writes a different file each time from unchanged input. Nothing here hashes
#' these tables, but an artifact that changes between runs cannot be checked against a previous copy.
#'
#' @param .dir_master Directory of 01A's mirrored master index, unrestricted.
#' @param .path_meta 01C's consolidated metadata.
#' @param .paths_out List with exactly the names Master and Exhibits: the two cache destinations.
#'   Checked rather than assumed, because `$` partial-matches on lists and a near-miss returns NULL.
#' @param .path_stamp Parquet holding the fingerprint the two were built under.
#' @param .rerun Logical. TRUE rebuilds regardless of the fingerprint.
#' @return A named list: Master, Exhibits.
cto_link_tables <- function(.dir_master, .path_meta, .paths_out, .path_stamp, .rerun = FALSE) {
  if (FALSE) {
    .dir_master <- .lP$Edgar$MasterIndex$DirParquet
    .path_meta  <- .lP$Input$MetaData
    .paths_out  <- list(Master = .lP$Cache$MasterIndex, Exhibits = .lP$Cache$Exhibits)
    .path_stamp <- .lP$Cache$TargetStamp
    .rerun      <- FALSE
  }

  need_ <- c("Master", "Exhibits")
  if (!all(need_ %in% names(.paths_out))) {
    cli::cli_abort(c(
      "The destination list must carry exactly the names Master and Exhibits.",
      "i" = "Received: {paste(names(.paths_out), collapse = ', ')}."
    ))
  }

  stamp_ <- utils_dir_stamp(.dirs = c(.dir_master, .path_meta))

  fresh_ <- !.rerun &&
    all(fs::file_exists(as.character(unlist(.paths_out[need_])))) &&
    identical(utils_stamp_read(.path = .path_stamp), stamp_)

  if (fresh_) {
    cli::cli_alert_info("Mirror and metadata unchanged: the two link tables were read from cache.")
    return(purrr::map(.paths_out[need_], \(.p) arrow::read_parquet(file = .p)))
  }

  if (!.rerun && fs::file_exists(.path_stamp)) {
    cli::cli_alert_warning("The mirror or the metadata has moved; the link tables are being rebuilt.")
  }

  master_ <- arrow::open_dataset(sources = .dir_master) |>
    dplyr::select("CIK", "FormType", "DateFiled", "HashIndex") |>
    dplyr::collect() |>
    dplyr::distinct() |>
    dplyr::arrange(.data$CIK, .data$FormType, .data$DateFiled, .data$HashIndex)

  exhibits_ <- arrow::open_dataset(sources = .path_meta) |>
    dplyr::filter(grepl("^Exhibit10", .data$DocTypeMod)) |>
    dplyr::select("DocID", "HashIndex", "DocTypeRaw") |>
    dplyr::collect() |>
    dplyr::mutate(ExhibitNo = cto_exhibit_number(.x = .data$DocTypeRaw)) |>
    dplyr::arrange(.data$DocID)

  out_ <- list(Master = master_, Exhibits = exhibits_)

  purrr::walk2(
    .x = out_[need_],
    .y = as.character(unlist(.paths_out[need_])),
    .f = arrow::write_parquet
  )
  arrow::write_parquet(tibble::tibble(Stamp = stamp_), .path_stamp)

  cli::cli_alert_success(
    "Link tables rebuilt: {format(nrow(master_), big.mark = ',')} filings, \\
     {format(nrow(exhibits_), big.mark = ',')} exhibits."
  )

  out_
}


# 4c. Deployment --------------------------------------------------------------------------------------------------------

#' Write the linked references, if what produced them has moved
#'
#' NOT GUARDED FOR ITS SIZE. The file is small. It is guarded because 02B reads it, and a fingerprint
#' downstream is only as stable as the modification time of the file it points at: rewriting this
#' unconditionally would make every cache keyed on it miss on every render of this document, however
#' identical the bytes.
#'
#' @param .tab The linked references, one row per reference.
#' @param .path_out Destination parquet path; the published artifact.
#' @param .stamp Character. The fingerprint of what determined the table, from utils_dir_stamp().
#' @param .path_stamp Parquet holding the fingerprint .path_out was last written under.
#' @param .rerun Logical. TRUE writes regardless of the fingerprint.
#' @return .tab, invisibly.
cto_write_exhibits <- function(.tab, .path_out, .stamp, .path_stamp, .rerun = FALSE) {
  if (FALSE) {
    .tab        <- tab_cto
    .path_out   <- .lP$Output$CtoExhibits
    .stamp      <- stamp_output
    .path_stamp <- .lP$Cache$OutputStamp
    .rerun      <- FALSE
  }

  fresh_ <- !.rerun &&
    fs::file_exists(.path_out) &&
    identical(utils_stamp_read(.path = .path_stamp), .stamp)

  if (fresh_) {
    cli::cli_alert_info("Inputs unchanged: {fs::path_file(.path_out)} was left as it stands.")
    return(invisible(.tab))
  }

  arrow::write_parquet(.tab, .path_out)
  arrow::write_parquet(tibble::tibble(Stamp = .stamp), .path_stamp)
  cli::cli_alert_success("Written: {format(nrow(.tab), big.mark = ',')} references.")

  invisible(.tab)
}


# 5. Linking -----------------------------------------------------------------------------------------------------------

#' Strip an exhibit type down to its number, in a single spelling
#'
#' The filing side writes "EX-10.1"; the order writes "10.1". Removing the prefix puts both on one
#' scale. An unnumbered "EX-10" reduces to "10" and can never match a numbered reference, which is
#' correct: the order named a specific exhibit and the filer did not number one.
#'
#' LEADING ZEROS ARE REMOVED FROM EACH COMPONENT. Filers write 10.01 and 10.1 for the same exhibit,
#' and orders do the same, in both directions -- one order asks for 10.08 of a filing that attached
#' 10.8, another asks for 10.7 of a filing that attached 10.07. Compared as strings those are four
#' different exhibits; compared after normalisation they are two. The normalisation is textual rather
#' than numeric because 10.1.10 has three components and is not a number.
#'
#' APPLIED TO BOTH SIDES. The filing side passes through it in cto_link_tables(), the order side in
#' cto_parse_order(); a normalisation applied to one side only is a comparison of two spellings.
#'
#' @param .x Character vector of exhibit type strings.
#' @return Character vector of bare numbers, zero-padding removed.
cto_exhibit_number <- function(.x) {
  if (FALSE) .x <- c("EX-10.1", "EX-10", "EX-10.18.1", "EX-10.01", "10.08")

  .x |>
    toupper() |>
    stringi::stri_replace_first_regex("^\\s*EX(?:HIBIT)?\\s*[-. ]?\\s*", "") |>
    trimws() |>
    stringi::stri_replace_all_regex("(^|\\.)0+(\\d)", "$1$2")
}

#' Find the filing each reference names
#'
#' FIRST OF TWO HOPS, AND THE AMBIGUITY IS REPORTED RATHER THAN RESOLVED. A filer can lodge two
#' filings of the same type on one day, so CIK plus form plus date does not always identify one
#' filing. Taking the first would produce a link that looks as good as any other and is right half
#' the time; counting the candidates and refusing to choose leaves a number that can be quoted.
#'
#' THE INDEX MUST BE THE UNRESTRICTED ONE. 01C publishes a master index filtered to filings whose
#' documents were retrieved -- two and a half per cent of EDGAR. Joining against it makes every
#' filing we hold no documents from look as though EDGAR had never heard of it, which conflates a
#' gap in the acquisition with a gap in the record. Half of what this script previously called
#' "filing not in the index" was in the index all along.
#'
#' A TOLERANCE IS ALLOWED ON THE DATE, AND ONLY WHERE IT RESOLVES TO ONE FILING. Some orders quote
#' the date the filing was accepted rather than the date the index records, which differs by a day
#' either side. An exact match is preferred; where there is none, a filing within the tolerance is
#' accepted if it is the only one, and the fact that a tolerance was used is recorded so the effect
#' can be measured and switched off.
#'
#' AMBIGUITY IS BROKEN BY WHICH CANDIDATE HOLDS EXHIBITS, WHERE EXACTLY ONE DOES. Using the full
#' index exposes ambiguity the restricted one hid: a filer lodging two filings of a type on one day
#' had only one of them in the restricted index, so the join looked unique when it was not. That is
#' worth exposing, but a reference to an exhibit can only resolve to a filing whose exhibits exist,
#' and where exactly one candidate has any the choice is determined rather than guessed. Where two
#' do, the reference stays ambiguous.
#'
#' CACHED ON ITS RESULT, NOT ON ITS LOOKUP TABLES. Four grouped passes run over the full master index
#' before a single reference is touched -- a distinct, a count, and two grouped filters across some
#' eighteen million rows -- and that is the whole cost of this step; joining forty thousand references
#' to the result is free by comparison. Caching those intermediates would mean half a gigabyte of
#' parquet for tables of fifteen million rows each, where the answer they produce is four megabytes.
#'
#' THE KEY COVERS BOTH SIDES. The two link tables enter through their own cache files, so a rebuilt
#' mirror or metadata invalidates this transitively; the references enter as a hash of their join
#' keys, sorted so that it does not depend on the order Arrow happened to return them in; and the
#' tolerance enters directly, because changing it changes which references resolve.
#'
#' @param .tab_ref Parsed references, carrying CIK, UseForm and UseFiledOn.
#' @param .tab_mst The master index. Pass the unrestricted one.
#' @param .hash_docs Character vector of HashIndex values from which exhibits were acquired.
#' @param .tolerance Integer. Days either side to accept where no exact match exists. 0 disables it.
#' @param .stamp Character. Fingerprint of what determines the result, from utils_dir_stamp().
#' @param .path_cache Parquet holding the linked references and the fingerprint they were built under.
#' @param .rerun Logical. TRUE re-links regardless of the fingerprint.
#' @return .tab_ref with nFilings, HashIndex, DateTolerated and TieBroken added.
cto_link_filing <- function(.tab_ref, .tab_mst, .hash_docs = character(0), .tolerance = 3L,
                            .stamp, .path_cache, .rerun = FALSE) {
  if (FALSE) {
    .tab_ref    <- tab_refs
    .tab_mst    <- tab_master
    .hash_docs  <- unique(tab_exhibits$HashIndex)
    .tolerance  <- 3L
    .stamp      <- stamp_link
    .path_cache <- .lP$Cache$Linked
    .rerun      <- FALSE
  }

  if (!.rerun && identical(utils_stamp_read(.path = .path_cache), .stamp)) {
    out_ <- dplyr::select(arrow::read_parquet(file = .path_cache), -"Stamp")
    cli::cli_alert_info(
      "References and index unchanged: {format(nrow(out_), big.mark = ',')} links read from cache."
    )
    return(out_)
  }

  if (!.rerun && fs::file_exists(.path_cache)) {
    cli::cli_alert_warning("The references, the index or the tolerance has moved; re-linking.")
  }

  key_ <- .tab_mst |>
    dplyr::select("CIK", UseForm = "FormType", UseFiledOn = "DateFiled", "HashIndex") |>
    dplyr::distinct()

  cnt_ <- key_ |>
    dplyr::summarise(nFilings = dplyr::n(), .by = c("CIK", "UseForm", "UseFiledOn"))

  one_ <- key_ |>
    dplyr::filter(dplyr::n() == 1L, .by = c("CIK", "UseForm", "UseFiledOn"))

  # Among several candidates, those from which exhibits were actually acquired. Where exactly one
  # remains, the reference resolves to it; the tie-break is recorded so its effect is measurable.
  tie_ <- key_ |>
    dplyr::filter(dplyr::n() > 1L, .by = c("CIK", "UseForm", "UseFiledOn")) |>
    dplyr::filter(.data$HashIndex %in% .hash_docs) |>
    dplyr::filter(dplyr::n() == 1L, .by = c("CIK", "UseForm", "UseFiledOn")) |>
    dplyr::select("CIK", "UseForm", "UseFiledOn", TieHash = "HashIndex")

  out_ <- .tab_ref |>
    dplyr::left_join(cnt_, by = dplyr::join_by("CIK", "UseForm", "UseFiledOn")) |>
    dplyr::mutate(nFilings = dplyr::coalesce(.data$nFilings, 0L)) |>
    dplyr::left_join(one_, by = dplyr::join_by("CIK", "UseForm", "UseFiledOn")) |>
    dplyr::left_join(tie_, by = dplyr::join_by("CIK", "UseForm", "UseFiledOn")) |>
    dplyr::mutate(
      TieBroken     = as.integer(.data$nFilings > 1L & !is.na(.data$TieHash)),
      HashIndex     = dplyr::if_else(.data$TieBroken == 1L, .data$TieHash, .data$HashIndex),
      nFilings      = dplyr::if_else(.data$TieBroken == 1L, 1L, .data$nFilings),
      TieHash       = NULL,
      DateTolerated = 0L
    )

  if (.tolerance <= 0L) return(cto_cache_linked(.tab = out_, .stamp = .stamp, .path_cache = .path_cache))

  # Only the references with no exact match are retried, and only a unique neighbour is accepted.
  miss_ <- out_ |>
    dplyr::filter(.data$nFilings == 0L, !is.na(.data$UseForm), !is.na(.data$UseFiledOn)) |>
    dplyr::select(-"HashIndex", -"nFilings", -"DateTolerated", -"TieBroken")

  if (nrow(miss_) == 0L) return(cto_cache_linked(.tab = out_, .stamp = .stamp, .path_cache = .path_cache))

  near_ <- miss_ |>
    dplyr::distinct(.data$CIK, .data$UseForm, .data$UseFiledOn) |>
    dplyr::left_join(
      y  = dplyr::select(key_, "CIK", "UseForm", CandDate = "UseFiledOn", CandHash = "HashIndex"),
      by = dplyr::join_by("CIK", "UseForm"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(abs(as.numeric(.data$CandDate - .data$UseFiledOn)) <= .tolerance) |>
    dplyr::summarise(
      nNear    = dplyr::n_distinct(.data$CandHash),
      NearHash = dplyr::first(.data$CandHash),
      .by      = c("CIK", "UseForm", "UseFiledOn")
    ) |>
    dplyr::filter(.data$nNear == 1L) |>
    dplyr::select("CIK", "UseForm", "UseFiledOn", "NearHash")

  out_ |>
    dplyr::left_join(near_, by = dplyr::join_by("CIK", "UseForm", "UseFiledOn")) |>
    dplyr::mutate(
      DateTolerated = as.integer(.data$nFilings == 0L & !is.na(.data$NearHash)),
      HashIndex     = dplyr::if_else(.data$DateTolerated == 1L, .data$NearHash, .data$HashIndex),
      nFilings      = dplyr::if_else(.data$DateTolerated == 1L, 1L, .data$nFilings),
      NearHash      = NULL
    ) |>
    cto_cache_linked(.stamp = .stamp, .path_cache = .path_cache)
}

#' Write the linked references to their cache and return them
#'
#' Split out because cto_link_filing() has three exit points -- tolerance disabled, nothing left to
#' retry, and the full path -- and a cache written at only some of them is a cache that appears to
#' work until the day the input takes another branch.
#'
#' @param .tab The linked references.
#' @param .stamp Character. The fingerprint they were built under.
#' @param .path_cache Destination parquet.
#' @return .tab, unchanged.
cto_cache_linked <- function(.tab, .stamp, .path_cache) {
  if (FALSE) {
    .tab        <- out_
    .stamp      <- stamp_link
    .path_cache <- .lP$Cache$Linked
  }

  arrow::write_parquet(dplyr::mutate(.tab, Stamp = .stamp), .path_cache)
  cli::cli_alert_success("Re-linked: {format(nrow(.tab), big.mark = ',')} references, cached.")

  .tab
}

#' Find the document each reference names, inside its filing
#'
#' SECOND HOP. The exhibit number picks the document within the filing, and the same rule applies:
#' where a filing carries two documents under one exhibit number the reference is left unresolved
#' and counted.
#'
#' A FILING WE HOLD NOTHING FROM IS NOT A FILING MISSING ONE EXHIBIT. Once the join uses the full
#' index, a reference can land on a filing from which no exhibit was ever acquired -- eighty per cent
#' of orders name at least one -- and that is a different statement from a filing whose exhibits we
#' have but which does not include this one. The first is a gap in the acquisition, the second is
#' incorporation by reference: the exhibit index numbers twenty exhibits, eight are attached and the
#' rest point at earlier filings. Both are limits worth quoting and neither is a fault, but only
#' separately.
#'
#' THE RESULT IS SORTED. This is what gets published, and an artifact that changes between runs over
#' unchanged input cannot be checked against a previous copy.
#'
#' @param .tab_ref Output of cto_link_filing().
#' @param .tab_docs Exhibit-10 documents, carrying HashIndex, DocID and a bare exhibit number.
#' @return .tab_ref with nDocs, nInFiling, DocIDContract and LinkStatus added, ordered.
cto_link_document <- function(.tab_ref, .tab_docs) {
  if (FALSE) {
    .tab_ref  <- tab_linked
    .tab_docs <- tab_exhibits
  }

  cnt_ <- .tab_docs |>
    dplyr::summarise(nDocs = dplyr::n(), .by = c("HashIndex", "ExhibitNo"))

  # How many Exhibit 10 documents were acquired from the filing at all, regardless of number.
  any_ <- .tab_docs |>
    dplyr::summarise(nInFiling = dplyr::n(), .by = "HashIndex")

  one_ <- .tab_docs |>
    dplyr::filter(dplyr::n() == 1L, .by = c("HashIndex", "ExhibitNo")) |>
    dplyr::select("HashIndex", "ExhibitNo", DocIDContract = "DocID")

  .tab_ref |>
    dplyr::left_join(cnt_, by = dplyr::join_by("HashIndex", "ExhibitNo")) |>
    dplyr::left_join(one_, by = dplyr::join_by("HashIndex", "ExhibitNo")) |>
    dplyr::left_join(any_, by = dplyr::join_by("HashIndex")) |>
    dplyr::mutate(
      nDocs      = dplyr::coalesce(.data$nDocs, 0L),
      nInFiling  = dplyr::coalesce(.data$nInFiling, 0L),
      # THE RESULT IS TESTED BEFORE THE REASONS. A reference that found its contract is linked
      # whatever the shape of its exhibit number: some filers do write EX-10(a), so a lettered
      # number matches exactly when both sides carry it. Testing the shape first labelled
      # seventy-nine resolved references as unresolvable, which the consistency check caught.
      LinkStatus = dplyr::case_when(
        !is.na(.data$DocIDContract)                    ~ "A-linked",
        is.na(.data$ExhibitNo)                         ~ "1-no reference parsed",
        .data$Series != "10"                           ~ "2-not an Exhibit 10",
        is.na(.data$UseForm) | is.na(.data$UseFiledOn) ~ "3-source filing not named",
        .data$nFilings == 0L                           ~ "4-filing not in EDGAR",
        .data$nFilings > 1L                            ~ "5-filing ambiguous",
        .data$nInFiling == 0L                          ~ "6-no exhibit acquired from that filing",
        .data$nDocs > 1L                               ~ "7-exhibit ambiguous",
        grepl("\\(", .data$ExhibitNo)                  ~ "8-lettered exhibit number",
        .default                                       = "9-exhibit not among those acquired"
      )
    ) |>
    dplyr::arrange(.data$DocID, .data$ExhibitNo)
}


# 6. Reports -----------------------------------------------------------------------------------------------------------

#' Which outcome counts as a resolved reference
#'
#' @return Character vector of status labels.
cto_status_ok <- function() "A-linked"

#' What the orders are
#'
#' @param .tab One row per reference.
#' @return A tibble, one row per order status.
cto_order_summary <- function(.tab) {
  if (FALSE) .tab <- tab_cto

  .tab |>
    dplyr::distinct(.data$DocID, .keep_all = TRUE) |>
    dplyr::summarise(
      nOrders      = dplyr::n(),
      nExtension   = sum(.data$IsExtension == 1L),
      nMultiFiler  = sum(.data$nRegistrants > 1L),
      nSrcAmended  = sum(.data$SourceAmended == 1L),
      .by          = "Status"
    ) |>
    dplyr::arrange(dplyr::desc(.data$nOrders))
}

#' Which reference format each order used
#'
#' @param .tab One row per reference.
#' @return A tibble: RefFormat, nOrders, nRefs, RefsPerOrder.
cto_format_summary <- function(.tab) {
  if (FALSE) .tab <- tab_cto

  .tab |>
    dplyr::summarise(
      nOrders = dplyr::n_distinct(.data$DocID),
      nRefs   = sum(!is.na(.data$ExhibitNo)),
      .by     = "RefFormat"
    ) |>
    dplyr::mutate(RefsPerOrder = round(.data$nRefs / .data$nOrders, 2)) |>
    dplyr::arrange(dplyr::desc(.data$nOrders))
}

#' Where the linkage stands
#'
#' @param .tab One row per reference, linked.
#' @return A tibble: LinkStatus, nRefs, Share.
cto_link_summary <- function(.tab) {
  if (FALSE) .tab <- tab_cto

  .tab |>
    dplyr::summarise(nRefs = dplyr::n(), .by = "LinkStatus") |>
    dplyr::mutate(Share = .data$nRefs / sum(.data$nRefs)) |>
    dplyr::arrange(.data$LinkStatus)
}

#' Linked references by year of the order
#'
#' @param .tab One row per reference, linked, carrying the order's filing date.
#' @return A tibble: Year, nRefs, nLinked, ShareLinked.
cto_link_by_year <- function(.tab) {
  if (FALSE) .tab <- tab_cto

  .tab |>
    dplyr::mutate(Year = as.integer(format(.data$DateFiled, "%Y"))) |>
    dplyr::summarise(
      nRefs   = dplyr::n(),
      nLinked = sum(.data$LinkStatus %in% cto_status_ok()),
      .by     = "Year"
    ) |>
    dplyr::mutate(ShareLinked = .data$nLinked / .data$nRefs) |>
    dplyr::arrange(.data$Year)
}

#' How the filing hop resolved
#'
#' PULLED OUT OF THE REPORTER. It was computed inline inside cto_report_all(), which put a
#' substantive summary -- how many references resolved exactly, how many needed the tie-break, how
#' many needed the date tolerance -- inside a function whose job is to print. A compute function
#' returns it, so the numbers stay available after they have been displayed and the reporter is
#' called twice without deriving them twice.
#'
#' @param .tab The linked references.
#' @return A one-row tibble: nRefs, nExact, nTieBroken, nTolerated, nAmbiguous, nNotInEdgar.
cto_filing_hop <- function(.tab) {
  if (FALSE) .tab <- tab_cto

  .tab |>
    dplyr::filter(!is.na(.data$ExhibitNo)) |>
    dplyr::summarise(
      nRefs        = dplyr::n(),
      nExact       = sum(.data$nFilings == 1L & .data$DateTolerated == 0L & .data$TieBroken == 0L),
      nTieBroken   = sum(.data$TieBroken == 1L),
      nTolerated   = sum(.data$DateTolerated == 1L),
      nAmbiguous   = sum(.data$nFilings > 1L),
      nNotInEdgar  = sum(.data$nFilings == 0L)
    )
}

#' Every report in this document, in order
#'
#' TAKES THE FOUR SUMMARIES, DOES NOT COMPUTE THEM. This block is shown twice, in Results and again
#' in the Overview, and computing them at each call leaves two sets of numbers that must agree by
#' construction and are derived independently.
#'
#' @param .tab_ord Output of cto_order_summary().
#' @param .tab_fmt Output of cto_format_summary().
#' @param .tab_hop Output of cto_filing_hop().
#' @param .tab_lnk Output of cto_link_summary().
#' @return Invisibly NULL.
cto_report_all <- function(.tab_ord, .tab_fmt, .tab_hop, .tab_lnk) {
  if (FALSE) {
    .tab_ord <- tab_order_summary
    .tab_fmt <- tab_format_summary
    .tab_hop <- tab_filing_hop
    .tab_lnk <- tab_link_summary
  }

  tbl_head("What the orders are")
  tbl_out(
    .tab   = .tab_ord,
    .title = NULL,
    .notes = c(
      Status      = "Read from the title line, which says what the order does.",
      nSrcAmended = "The source filing was amended, not the order. A body search for 'amend' \\
                     conflates the two."
    )
  )

  tbl_head("Reference formats")
  tbl_out(
    .tab   = .tab_fmt,
    .title = NULL,
    .notes = c(RefFormat = "inline and table name the source filing per exhibit; plain relies on \\
                            the one named in the opening paragraph.")
  )

  tbl_head("Filing hop")
  tbl_out(
    .tab   = .tab_hop,
    .title = NULL,
    .notes = c(
      nTieBroken = "Several filings matched; exactly one had exhibits, so the choice was determined.",
      nTolerated = "Matched to a filing within three days where no exact date matched."
    )
  )

  tbl_head("Linkage")
  tbl_out(
    .tab    = .tab_lnk,
    .title  = NULL,
    .pct    = "Share",
    .digits = 1L,
    .notes  = c(LinkStatus = "Ordered by how early the chain breaks; every reference is here, none \\
                              was dropped.")
  )

  invisible(NULL)
}


# 7. Figures -----------------------------------------------------------------------------------------------------------
# DEFINED HERE, WRITTEN NOWHERE. The document displays what this returns and the consolidated release
# script writes the files the manuscript needs.

#' Linkage outcome per year
#'
#' @param .tab One row per reference, linked.
#' @return A ggplot object.
cto_plot_linkage <- function(.tab) {
  if (FALSE) .tab <- tab_cto

  dat_ <- .tab |>
    dplyr::mutate(
      Year   = as.integer(format(.data$DateFiled, "%Y")),
      Result = dplyr::if_else(.data$LinkStatus %in% cto_status_ok(), "Linked", "Unresolved")
    ) |>
    dplyr::summarise(nRefs = dplyr::n(), .by = c("Year", "Result"))

  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$nRefs, fill = .data$Result)) +
    ggplot2::geom_col() +
    plot_scale_fill_cat() +
    plot_scale_y_count() +
    ggplot2::labs(x = NULL, y = "Exhibit references", fill = NULL) +
    plot_theme(.grid = "y")
}
