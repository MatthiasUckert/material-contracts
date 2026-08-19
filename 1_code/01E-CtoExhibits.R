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
# corpus is clean because 01B parsed it once. Ninety-six per cent name the source form type and
# ninety-eight the source filing date, so the link is CIK plus form plus date into the master index,
# then exhibit number into the filing. Two exact hops. Nothing here is fuzzy, and where a hop fails
# the reference is recorded with the reason rather than dropped.
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
#' @return Named character vector of pattern fragments.
cto_patterns <- function() {
  c(
    Exhibit = "([0-9]+(?:\\.[0-9]+)*[A-Za-z]?(?:\\([A-Za-z0-9]+\\))?)",
    Date    = "([A-Za-z]+\\s+[0-9]{1,2},\\s*[0-9]{4})",
    Form    = "([A-Z0-9][A-Z0-9/-]{0,9})"
  )
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

  form_ <- stringi::stri_match_first_regex(
    .text, paste0("(?i)to\\s+(?:an?\\s+)?Forms?\\s+", pat_[["Form"]])
  )[, 2L]

  date_ <- stringi::stri_match_first_regex(
    .text, paste0("(?i)filed\\s+(?:on\\s+)?", pat_[["Date"]])
  )[, 2L]

  tibble::tibble(
    Status        = toupper(trimws(title_)),
    Rule          = rule_,
    SourceForm    = toupper(trimws(form_)),
    SourceFiledOn = suppressWarnings(as.Date(date_, format = "%B %d, %Y")),
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
    RefFiledOn  = suppressWarnings(as.Date(m_[, 4L], format = "%B %d, %Y")),
    ReleaseDate = suppressWarnings(as.Date(m_[, 5L], format = "%B %d, %Y")),
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
    RefFiledOn  = suppressWarnings(as.Date(m_[, 4L], format = "%B %d, %Y")),
    ReleaseDate = suppressWarnings(as.Date(m_[, 5L], format = "%B %d, %Y")),
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
    ReleaseDate = suppressWarnings(as.Date(m_[, 3L], format = "%B %d, %Y")),
    RefFormat   = "plain"
  )
}


#' References named in a denial or revocation
#'
#' A denial is not a listing block. It says "denied your request for confidential treatment of the
#' information excluded from Exhibit 4.1 to the Form 8-K filed on December 11, 2007" -- exhibit and
#' filing both named, in a sentence, with no release date because nothing was granted.
#'
#' There are eleven of these in sixteen years, and they matter out of proportion to that: the filer
#' applied to withhold and was refused, so the contract had to be disclosed. Leaving them unparsed
#' discards the one place in this data where non-disclosure was denied rather than chosen.
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
    RefFiledOn  = suppressWarnings(as.Date(m_[, 4L], format = "%B %d, %Y")),
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
      # A reference that names its own filing overrides the one from the opening paragraph.
      UseForm    = dplyr::coalesce(.data$RefForm, .data$SourceForm),
      UseFiledOn = dplyr::coalesce(.data$RefFiledOn, .data$SourceFiledOn),
      Series     = stringi::stri_extract_first_regex(.data$ExhibitNo, "^[0-9]+")
    )
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
#' @param .tab_ref Parsed references, carrying CIK, UseForm and UseFiledOn.
#' @param .tab_mst The master index. Pass the unrestricted one.
#' @param .hash_docs Character vector of HashIndex values from which exhibits were acquired.
#' @param .tolerance Integer. Days either side to accept where no exact match exists. 0 disables it.
#' @return .tab_ref with nFilings, HashIndex, DateTolerated and TieBroken added.
cto_link_filing <- function(.tab_ref, .tab_mst, .hash_docs = character(0), .tolerance = 3L) {
  if (FALSE) {
    .tab_ref   <- tab_refs
    .tab_mst   <- tab_master
    .hash_docs <- unique(tab_exhibits$HashIndex)
    .tolerance <- 3L
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

  if (.tolerance <= 0L) return(out_)

  # Only the references with no exact match are retried, and only a unique neighbour is accepted.
  miss_ <- out_ |>
    dplyr::filter(.data$nFilings == 0L, !is.na(.data$UseForm), !is.na(.data$UseFiledOn)) |>
    dplyr::select(-"HashIndex", -"nFilings", -"DateTolerated", -"TieBroken")

  if (nrow(miss_) == 0L) return(out_)

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
    )
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
#' @param .tab_ref Output of cto_link_filing().
#' @param .tab_docs Exhibit-10 documents, carrying HashIndex, DocID and a bare exhibit number.
#' @return .tab_ref with nDocs, nInFiling, DocIDContract and LinkStatus added.
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
    )
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

#' Every report in this document, in order
#'
#' @param .tab One row per reference, linked.
#' @return Invisibly NULL.
cto_report_all <- function(.tab) {
  if (FALSE) .tab <- tab_cto

  tbl_head("What the orders are")
  tbl_out(
    .tab   = cto_order_summary(.tab = .tab),
    .title = NULL,
    .notes = c(
      Status      = "Read from the title line, which says what the order does.",
      nSrcAmended = "The source filing was amended, not the order. A body search for 'amend' \\
                     conflates the two."
    )
  )

  tbl_head("Reference formats")
  tbl_out(
    .tab   = cto_format_summary(.tab = .tab),
    .title = NULL,
    .notes = c(RefFormat = "inline and table name the source filing per exhibit; plain relies on \\
                            the one named in the opening paragraph.")
  )

  tbl_head("Filing hop")
  tbl_out(
    .tab = .tab |>
      dplyr::filter(!is.na(.data$ExhibitNo)) |>
      dplyr::summarise(
        nRefs        = dplyr::n(),
        nExact       = sum(.data$nFilings == 1L & .data$DateTolerated == 0L & .data$TieBroken == 0L),
        nTieBroken   = sum(.data$TieBroken == 1L),
        nTolerated   = sum(.data$DateTolerated == 1L),
        nAmbiguous   = sum(.data$nFilings > 1L),
        nNotInEdgar  = sum(.data$nFilings == 0L)
      ),
    .title = NULL,
    .notes = c(
      nTieBroken = "Several filings matched; exactly one had exhibits, so the choice was determined.",
      nTolerated = "Matched to a filing within three days where no exact date matched."
    )
  )

  tbl_head("Linkage")
  tbl_out(
    .tab    = cto_link_summary(.tab = .tab),
    .title  = NULL,
    .pct    = "Share",
    .digits = 1L,
    .notes  = c(LinkStatus = "Ordered by how early the chain breaks; every reference is here, none \\
                              was dropped.")
  )

  invisible(NULL)
}


# 7. Figures -----------------------------------------------------------------------------------------------------------

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
