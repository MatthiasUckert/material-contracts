# 04B-EntityRules: the rules that turn candidate spans into variables -- ORG ------------------------------------------
#
# WHAT THIS FILE DOES
# 04A extracted nine engines over 4,398 hand-classified contracts and ranked none of them, because
# ranking needs labels and there are no entity labels. This file writes the rules. It takes one
# entity at a time; ORG is first, because every other party quantity -- the jurisdiction, the
# address, the signing date -- is measured from where an organisation was found.
#
# ONE ENGINE, AND THAT IS THE BINDING CONSTRAINT
# 04C runs five engines over the corpus and none of them is spaCy. For ORG that leaves lexnlp alone,
# which emits 81,467 spans across the sample against spaCy's 5.3 million, so the whole rule runs in R
# over a table that fits in memory and DuckDB is read once.
#
# THE PARTY IS THE ANCHOR, NOT A REGION
# An earlier design read a fixed head of the document and asked whether the filer was inside it.
# Sweeping that head showed the located share rising from 70.6% to 75.1% -- and the LATE share
# falling from 6.5% to 2.0%, with the two summing to 77.1% in every one of twenty cells. That is an
# identity rather than a finding: the match runs on names and cannot depend on a region, so a wider
# head buys no filer and only relabels one that was already found, while the candidate count doubles.
#
# So there is no head. The filer is matched ANYWHERE in the document, the window is centred on WHERE
# IT WAS FOUND, and every document gets its own bounds, stored. The window is the only thing a
# counterparty count depends on, which is also why the window sweep is now cheap: the match is
# computed once and the windows are re-cut over it.
#
# AN ENTITY IS A GROUP OF SPANS, AND ITS OFFSETS COME FROM ONE ROW OF THAT GROUP
# A contract names the same company several times. The mentions are collapsed to one row per
# (document, company), and that row is the EARLIEST mention taken WHOLE -- not a start from one
# mention beside a stop from another, which produces a pair belonging to no mention at all and
# brackets the entire agreement. The occurrence counts and the latest position are group facts and
# are aggregated separately.
#
# NameCore IS THE NAME, THE SPAN IS THE POSITION. lexnlp's spans are ragged -- " BANK, LTD.andWEL",
# ", a signer of the foregoing i" -- while its resolved name on the same row is clean. Measured on
# the sample: median overshoot 5 characters, P90 12, and 99.0% of spans contain their own resolved
# name, against a median gap of 130 characters between adjacent names. Tight enough that the
# geography rule can use Start directly.
#
# NOISE IS EXPECTED AND IS REPORTED AGAINST A BASELINE. No stoplist, no defined-term filter, no
# geography exclusion. What every table carries instead is the UNADJUSTED count -- every distinct
# organisation the contract names anywhere -- beside the rule's count, so the reader sees what the
# rule removed rather than being asked to trust that it removed the right things.
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
# later one orders these axes identically.

plot_register_levels(
  .key    = "PartyStatus",
  .levels = c("matched", "first", "no entity"),
  .short  = c("matched", "first", "none")
)

plot_register_levels(
  .key    = "MatchKind",
  .levels = c("exact", "forward", "family", "reverse"),
  .short  = c("exact", "forward", "family", "reverse")
)

plot_register_levels(
  .key    = "OrgRole",
  .levels = c("filer", "counterparty", "signatory", "other"),
  .short  = c("filer", "counter", "signer", "other")
)

plot_register_levels(
  .key    = "Region",
  .levels = c("head", "middle", "tail"),
  .short  = c("head", "middle", "tail")
)


# 2. One reduction, both sides ---------------------------------------------------------------------------------------
# EDGAR writes a name in registration form and a contract writes it in prose. Both are reduced to a
# common key: strip EDGAR's conformed-name artifacts, uppercase, an ampersand between spaces to AND,
# punctuation to space, whitespace collapsed, a leading connective dropped, trailing corporate
# suffixes stripped. "The Boeing Company" -> "BOEING"; "UGI CORP /PA/" -> "UGI";
# "PETROL OIL & GAS INC" -> "PETROL OIL AND GAS", which is what the contract writes out in full.

.ent_suffix <- c("INC", "INCORPORATED", "CORP", "CORPORATION", "LLC", "LLP", "LP", "LTD", "LIMITED",
                 "PLC", "NV", "BV", "SA", "AG", "GMBH", "CO", "COMPANY", "COMPANIES", "TRUST",
                 "NATASSOC")

.ent_lead <- c("MADE BY AND BETWEEN", "BY AND BETWEEN", "BY AND AMONG", "AMONGST", "BETWEEN",
               "AMONG", "AND", "WITH", "THIS", "THE", "DATED", "AS OF")


#' Remove EDGAR's conformed-name artifacts before the name is reduced
#'
#' EDGAR appends a state-of-incorporation marker to the conformed company name -- "UGI CORP /PA/",
#' "XEROX CORP /NY/", and the backslash variant. The marker is not part of the name, but the
#' reduction turns it into a trailing token that matches nothing in the contract text. A former-name
#' parenthetical is removed for the same reason.
#'
#' @param .x Character vector of EDGAR conformed company names.
#' @return Character vector with the artifacts removed.
ent_strip_conformed <- function(.x) {
  if (FALSE) .x <- c("UGI CORP /PA/", "SMITH BARNEY \\DE\\", "ACME INC (FORMERLY: OLD ACME)")

  .x |>
    stringi::stri_replace_all_regex("\\s*\\((FORMERLY|FKA)[^)]*\\)", "") |>
    stringi::stri_replace_all_regex("[/\\\\][A-Za-z]{2,4}[/\\\\]?\\s*$", "") |>
    stringi::stri_trim_both()
}


#' Reduce a company name or a candidate span to a comparable key
#'
#' The corporate suffix is the problem this solves: EDGAR records "BOEING CO" where the contract
#' writes "The Boeing Company". Suffixes are removed with a repeated group rather than a token loop,
#' because "BANK CO LTD" carries three. The leading connective is removed because the extractors do
#' not stop cleanly at a name.
#'
#' The ampersand is mapped to AND only BETWEEN SPACES, so "PETROL OIL & GAS" and its written-out form
#' agree while "AT&T" is left to the punctuation pass.
#'
#' @param .x Character vector of names or spans as recorded.
#' @param .min Integer. Keys shorter than this become NA. One by default: the floors that matter --
#'   what may be compared at all, and what may be compared by CONTAINMENT -- are rule parameters
#'   applied at match time, where they can be swept.
#' @return Character vector of keys, NA below .min characters.
ent_norm_key <- function(.x, .min = 1L) {
  if (FALSE) {
    .x   <- c("ACME HOLDINGS, INC.", "The Boeing Company", "PETROL OIL & GAS INC")
    .min <- 1L
  }

  lead_ <- paste0("^(", paste(.ent_lead, collapse = "|"), ") ")
  sfx_  <- paste0("( (", paste(.ent_suffix, collapse = "|"), "))+$")

  out_ <- .x |>
    stringi::stri_trans_toupper() |>
    stringi::stri_replace_all_fixed(" & ", " AND ") |>
    stringi::stri_replace_all_regex("[^A-Z0-9 ]", " ") |>
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_trim_both() |>
    stringi::stri_replace_first_regex(lead_, "") |>
    stringi::stri_replace_first_regex(sfx_, "") |>
    stringi::stri_trim_both()

  dplyr::if_else(is.na(out_) | nchar(out_) < .min, NA_character_, out_)
}


#' How many leading tokens two keys share
#'
#' The instrument for the corporate family: "Elephant & Castle Group" against "Elephant & Castle
#' International" share a real corporate prefix and no suffix rule reaches that. LEADING tokens
#' rather than any tokens, because a shared trailing word -- HOLDINGS, PARTNERS, GROUP -- is a
#' drafting convention rather than a relationship.
#'
#' @param .a Character vector of keys.
#' @param .b Character vector of keys, aligned with .a.
#' @return Integer vector: matching tokens from the start, zero where the first differs or either
#'   side is missing.
ent_shared_tokens <- function(.a, .b) {
  if (FALSE) {
    .a <- c("ELEPHANT CASTLE GROUP", "FRANKLIN RESOURCES")
    .b <- c("ELEPHANT CASTLE INTERNATIONAL", "BOEING")
  }

  ta_ <- stringi::stri_split_fixed(.a, " ")
  tb_ <- stringi::stri_split_fixed(.b, " ")

  purrr::map2_int(ta_, tb_, function(.x, .y) {
    n_ <- min(length(.x), length(.y))
    if (n_ == 0L) return(0L)
    eq_ <- .x[seq_len(n_)] == .y[seq_len(n_)]
    if (!isTRUE(eq_[[1L]])) return(0L)
    bad_ <- which(!eq_ | is.na(eq_))
    if (length(bad_) == 0L) as.integer(n_) else as.integer(bad_[[1L]] - 1L)
  })
}


#' Per-document anchor keys, one row per contract
#'
#' Reads 04A's sample table rather than the EDGAR metadata, so this document depends on one narrow
#' artifact and the sample it describes cannot drift from the sample 04A extracted. No length floor
#' is applied; AnchorLen is carried and the rule decides what is long enough for equality and what
#' for containment, which are different questions.
#'
#' @param .path_anchors 04A's sample_anchors.parquet.
#' @return Tibble: DocID, Fold, Class, AmendType, CIK, CompanyName, CompanyClean, DateFiled,
#'   AnchorKey, AnchorTight, AnchorLen, AnchorTok.
ent_anchor_keys <- function(.path_anchors) {
  if (FALSE) .path_anchors <- .lP$Input$Anchors

  arrow::read_parquet(.path_anchors) |>
    dplyr::transmute(
      DocID,
      Fold,
      Class        = .data$ClassDetailed,
      AmendType,
      CIK,
      CompanyName,
      CompanyClean = ent_strip_conformed(.x = .data$CompanyName),
      DateFiled    = suppressWarnings(anytime::anydate(as.character(.data$DateFiled)))
    ) |>
    dplyr::mutate(
      AnchorKey   = ent_norm_key(.x = .data$CompanyClean, .min = 1L),
      AnchorTight = stringi::stri_replace_all_fixed(.data$AnchorKey, " ", ""),
      AnchorLen   = nchar(.data$AnchorKey),
      AnchorTok   = stringi::stri_count_fixed(.data$AnchorKey, " ") + 1L
    )
}


# 3. Input -----------------------------------------------------------------------------------------------------------

#' Document lengths, in code points
#'
#' stringi::stri_length rather than nchar, for the same reason every offset operation in this family
#' uses stri_sub: Python emits code-point offsets and a byte-based length would disagree with them on
#' any document carrying a multibyte character.
#'
#' @param .path_text 04A's canonical text parquet.
#' @return Tibble: DocID, DocLen. Empty documents are dropped.
ent_doc_lens <- function(.path_text) {
  if (FALSE) .path_text <- .lP$Input$Text

  arrow::read_parquet(.path_text) |>
    dplyr::transmute(DocID, DocLen = stringi::stri_length(.data$TextRaw)) |>
    dplyr::filter(.data$DocLen > 0L)
}


#' Load one engine's ORG spans, with both candidate keys and the document length
#'
#' Both keys are built here rather than inside the rule, because the reduction does not depend on any
#' rule parameter and rebuilding it per sweep cell would run the same strings through the same regex
#' to produce the identical answer.
#'
#' @param .db_path 04A's candidate store.
#' @param .lens Tibble from ent_doc_lens().
#' @param .combo Character. Engine combination token, e.g. "lexnlp".
#' @param .quiet Logical. Suppress the count message.
#' @return Tibble: one row per span, with SpanKey, CoreKey and DocLen.
ent_load_org <- function(.db_path, .lens, .combo = "lexnlp", .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Input$Store
    .lens    <- tab_lens
    .combo   <- "lexnlp"
    .quiet   <- FALSE
  }

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  raw_ <- DBI::dbGetQuery(con_, paste0(
    "SELECT DocID, Start, Stop, Span, NameCore, LegalForm, Description ",
    "FROM org ",
    "WHERE Start IS NOT NULL ",
    "  AND (CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END) = '", .combo, "'"
  )) |>
    tibble::as_tibble()

  out_ <- raw_ |>
    dplyr::inner_join(.lens, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      SpanKey = ent_norm_key(.x = .data$Span, .min = 1L),
      CoreKey = ent_norm_key(.x = dplyr::coalesce(.data$NameCore, .data$Span), .min = 1L)
    ) |>
    dplyr::arrange(.data$DocID, .data$Start)

  if (!.quiet) {
    cli::cli_alert_success(
      "{(.combo)}: {format(nrow(out_), big.mark = ',')} ORG span{cli::qty(nrow(out_))}{?s} over \\
       {format(dplyr::n_distinct(out_$DocID), big.mark = ',')} \\
       document{cli::qty(dplyr::n_distinct(out_$DocID))}{?s}."
    )
  }
  out_
}


# 4. Entities and the match ------------------------------------------------------------------------------------------
# Everything in this section is WINDOW-FREE. That is the point of the redesign: the match does not
# depend on where in the document anything sits, so it is computed once and every window is cut over
# the same result.

#' Build one match specification
#'
#' @param .key Character, "core" or "span". Which reduction identifies an entity.
#' @param .min_key Integer. Shortest key admitted for EQUALITY. Two, because "CA, INC." reduces to
#'   "CA" and that is the company's name rather than a degenerate key.
#' @param .min_contain Integer. Shortest key admitted for CONTAINMENT or family. Five, because "CA"
#'   is contained in "CATERPILLAR" and a key matching everything locates nothing.
#' @param .tight Logical. Also compare keys with spaces removed, so UNITED AIRLINES and UNITED AIR
#'   LINES are one company. They are, and nothing else in the reduction reaches that.
#' @param .frag_min Integer. Shortest fragment admitted as a reverse match.
#' @param .frag_share Numeric. Fragment must also cover this share of the anchor key.
#' @param .fam_tokens Integer. Leading tokens two keys must share to count as one corporate family.
#' @param .fam_share Numeric. Those tokens must also be this share of the shorter key.
#' @param .fam_df Numeric. A SINGLE shared leading token is admitted when it opens the name of a
#'   company in at most this share of documents. CHENIERE is distinctive and CHENIERE ENERGY against
#'   CHENIERE CREOLE TRAIL PIPELINE is one corporate family; NATIONAL is not. Zero disables the arm.
#' @param .deep Integer. A matched party found past this offset is flagged rather than rejected: it
#'   is still the right company, but the window around it is a neighbourhood and not a preamble.
#' @return A named list carrying the match specification.
ent_rule <- function(.key = "core", .min_key = 2L, .min_contain = 5L, .tight = TRUE,
                     .frag_min = 10L, .frag_share = 0.6,
                     .fam_tokens = 2L, .fam_share = 0.5, .fam_df = 0.005, .deep = 5000L) {
  if (FALSE) {
    .key         <- "core"
    .min_key     <- 2L
    .min_contain <- 5L
    .tight       <- TRUE
    .frag_min    <- 10L
    .frag_share  <- 0.6
    .fam_tokens  <- 2L
    .fam_share   <- 0.5
    .fam_df      <- 0.005
    .deep        <- 5000L
  }

  if (!.key %in% c("core", "span")) cli::cli_abort("{.arg .key} must be \"core\" or \"span\".")

  list(
    Key       = .key,       MinKey     = as.integer(.min_key),
    MinContain = as.integer(.min_contain), Tight = isTRUE(.tight),
    FragMin   = as.integer(.frag_min),     FragShare = .frag_share,
    FamTokens = as.integer(.fam_tokens),   FamShare  = .fam_share, FamDf = .fam_df,
    Deep      = as.integer(.deep)
  )
}


#' Override fields of a rule, coercing the integer ones
#'
#' @param .rule List from ent_rule().
#' @param .over Named list of field values to replace.
#' @return The rule with .over applied.
ent_rule_set <- function(.rule, .over) {
  if (FALSE) {
    .rule <- ent_rule()
    .over <- list(MinContain = 6L)
  }

  out_ <- .rule
  for (nm_ in names(.over)) out_[[nm_]] <- .over[[nm_]]
  for (nm_ in c("MinKey", "MinContain", "FragMin", "FamTokens", "Deep")) {
    out_[[nm_]] <- as.integer(out_[[nm_]])
  }
  out_$Tight <- isTRUE(out_$Tight)
  out_
}


#' Collapse mentions into entities, one row per document per name
#'
#' lexnlp returns "Icosavax, Inc.", "Icosavax Inc" and "Icosavax, Inc" as three spans and one party.
#'
#' THE OFFSETS COME FROM ONE ROW, SELECTED WHOLE. Start, Stop and Span are taken from the EARLIEST
#' mention by slice_min(), not computed column by column: a summarise() taking Start from the
#' earliest mention and Stop from the longest returns a pair belonging to no mention, running from
#' the preamble to the signature block. NOcc and MaxStart describe the GROUP and are aggregated
#' separately, which is correct -- they are not properties of any single mention.
#'
#' MaxStart is what makes the corroboration signal possible once the tail is clamped past the window:
#' an entity introduced beside the party and mentioned AGAIN at the end of the document was
#' introduced and signed, which no position-of-first-mention column can say.
#'
#' @param .spans Tibble from ent_load_org().
#' @param .rule List from ent_rule(). Supplies only the choice of key.
#' @return Tibble: DocID, Key, KeyTight, Name, Span, Start, Stop, MaxStart, DocLen, NOcc, NTok,
#'   HasForm, SpanWidth, NameLen, Overshoot, SpanHoldsName.
ent_entities <- function(.spans, .rule) {
  if (FALSE) {
    .spans <- tab_spans
    .rule  <- .lP$Params$Rule
  }

  keyed_ <- .spans |>
    dplyr::mutate(Key = if (identical(.rule$Key, "core")) .data$CoreKey else .data$SpanKey) |>
    dplyr::filter(!is.na(.data$Key), nzchar(.data$Key))

  agg_ <- keyed_ |>
    dplyr::summarise(
      NOcc     = dplyr::n(),
      MaxStart = max(.data$Start),
      HasForm  = any(!is.na(.data$LegalForm) | !is.na(.data$Description)),
      .by = c(DocID, Key)
    )

  # with_ties = FALSE so exactly one row survives even where two mentions share an offset, which a
  # duplicate span in the store can produce.
  first_ <- keyed_ |>
    dplyr::slice_min(order_by = .data$Start, n = 1L, by = c(DocID, Key), with_ties = FALSE) |>
    dplyr::select(DocID, Key, Start, Stop, Span, NameCore, DocLen)

  first_ |>
    dplyr::left_join(agg_, by = dplyr::join_by(DocID, Key)) |>
    dplyr::mutate(
      KeyTight      = stringi::stri_replace_all_fixed(.data$Key, " ", ""),
      Name          = dplyr::coalesce(.data$NameCore, .data$Span),
      NTok          = stringi::stri_count_fixed(.data$Key, " ") + 1L,
      SpanWidth     = .data$Stop - .data$Start,
      NameLen       = nchar(.data$Name),
      Overshoot     = .data$SpanWidth - .data$NameLen,
      SpanHoldsName = stringi::stri_detect_fixed(
        stringi::stri_trans_toupper(.data$Span), stringi::stri_trans_toupper(.data$Name)
      )
    ) |>
    dplyr::select(-NameCore) |>
    dplyr::arrange(.data$DocID, .data$Start)
}


#' How often each leading token opens a company name, across documents
#'
#' The gate on the one-token family arm. CHENIERE opens a name in a handful of contracts and
#' CHENIERE ENERGY against CHENIERE CREOLE TRAIL PIPELINE is one corporate family; NATIONAL opens
#' hundreds and shares nothing. Counting DOCUMENTS rather than entities, so a single filing listing
#' twenty affiliates does not make its own prefix look common.
#'
#' @param .ent Tibble from ent_entities().
#' @return Tibble: FirstTok, TokDocs, TokShare.
ent_token_freq <- function(.ent) {
  if (FALSE) .ent <- tab_ent

  n_ <- dplyr::n_distinct(.ent$DocID)

  .ent |>
    dplyr::mutate(FirstTok = stringi::stri_extract_first_regex(.data$Key, "^[A-Z0-9]+")) |>
    dplyr::filter(!is.na(.data$FirstTok)) |>
    dplyr::summarise(TokDocs = dplyr::n_distinct(.data$DocID), .by = FirstTok) |>
    dplyr::mutate(TokShare = .data$TokDocs / n_)
}


#' Everything about the key-to-anchor comparison that no threshold can change
#'
#' @param .ent Tibble from ent_entities().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .freq Tibble from ent_token_freq().
#' @return .ent with the anchor columns, the containment flags, SharedTok, MinTok and TokShare.
ent_match_facts <- function(.ent, .keys, .freq) {
  if (FALSE) {
    .ent  <- tab_ent
    .keys <- tab_keys
    .freq <- tab_freq
  }

  .ent |>
    dplyr::left_join(
      dplyr::select(.keys, DocID, AnchorKey, AnchorTight, AnchorLen, AnchorTok),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::mutate(FirstTok = stringi::stri_extract_first_regex(.data$Key, "^[A-Z0-9]+")) |>
    dplyr::left_join(.freq, by = dplyr::join_by(FirstTok)) |>
    dplyr::mutate(
      TokShare   = dplyr::coalesce(.data$TokShare, 0),
      KeyLen     = nchar(.data$Key),
      KeyTok     = stringi::stri_count_fixed(.data$Key, " ") + 1L,
      IsExact    = !is.na(.data$AnchorKey) & .data$Key == .data$AnchorKey,
      IsTight    = !is.na(.data$AnchorKey) & .data$KeyTight == .data$AnchorTight,
      IsFwd      = !is.na(.data$AnchorKey) &
                   stringi::stri_detect_fixed(.data$Key, .data$AnchorKey),
      IsRev      = !is.na(.data$AnchorKey) &
                   stringi::stri_detect_fixed(.data$AnchorKey, .data$Key),
      SharedTok  = ent_shared_tokens(.a = .data$Key, .b = .data$AnchorKey),
      MinTok     = pmin(.data$KeyTok, .data$AnchorTok)
    )
}


#' Turn the match facts into one match kind, under this rule's thresholds
#'
#' TWO FLOORS, GATING DIFFERENT ARMS. Equality is admitted down to MinKey; containment and family
#' only from MinContain, because "CA" is contained in "CATERPILLAR" and an unfloored containment arm
#' locates the wrong company while reporting success.
#'
#' Precedence is exact, forward, family, reverse -- safest first. Family and reverse can both fire on
#' the same pair and the order settles it rather than leaving it to row order.
#'
#' @param .fact Tibble from ent_match_facts().
#' @param .rule List from ent_rule().
#' @return .fact with CanMatch, CanContain, FragFloor, IsFam and MatchKind added.
ent_match_kind <- function(.fact, .rule) {
  if (FALSE) {
    .fact <- tab_facts
    .rule <- .lP$Params$Rule
  }

  .fact |>
    dplyr::mutate(
      CanMatch   = .data$KeyLen >= .rule$MinKey & .data$AnchorLen >= .rule$MinKey,
      CanContain = .data$KeyLen >= .rule$MinContain & .data$AnchorLen >= .rule$MinContain,
      FragFloor  = pmax(.rule$FragMin, .rule$FragShare * .data$AnchorLen),
      # Two or more shared leading tokens, or exactly one that is distinctive enough to be evidence.
      IsFam      = (.data$SharedTok >= .rule$FamTokens &
                    .data$SharedTok / pmax(.data$MinTok, 1L) >= .rule$FamShare) |
                   (.data$SharedTok == 1L & .rule$FamDf > 0 & .data$TokShare <= .rule$FamDf),
      IsRevOk    = .data$IsRev & .data$KeyLen >= .data$FragFloor,
      MatchKind  = dplyr::case_when(
        is.na(.data$AnchorKey)              ~ NA_character_,
        !.data$CanMatch                     ~ NA_character_,
        .data$IsExact                       ~ "exact",
        .rule$Tight & .data$IsTight         ~ "exact",
        !.data$CanContain                   ~ NA_character_,
        .data$IsFwd                         ~ "forward",
        .data$IsFam                         ~ "family",
        .data$IsRevOk                       ~ "reverse",
        TRUE                                ~ NA_character_
      )
    )
}


#' One row per document: which entity is the contracting party
#'
#' RUNS FROM THE KEY SIDE, not from the matches. A document where lexnlp proposed nothing resembling
#' the filer's name carries no row in the entity table, so a join in the other direction would drop
#' it and the coverage figure would be computed over the documents that worked.
#'
#' THE PARTY IS MATCHED ANYWHERE. There is no region test, so there is no "late": the earliest match
#' in the document wins, with match quality breaking ties at the same position. Where no entity
#' matches, the earliest entity in the document is taken instead and Status records that it was.
#'
#' @param .fact Tibble from ent_match_kind().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .lens Tibble from ent_doc_lens().
#' @param .rule List from ent_rule(). Supplies the deep-anchor flag.
#' @return Tibble: one row per document, with Status, the chosen entity and its offsets.
ent_locate_party <- function(.fact, .keys, .lens, .rule) {
  if (FALSE) {
    .fact <- tab_facts
    .keys <- tab_keys
    .lens <- tab_lens
    .rule <- .lP$Params$Rule
  }

  rank_ <- c(exact = 1L, forward = 2L, family = 3L, reverse = 4L)

  best_ <- .fact |>
    dplyr::filter(!is.na(.data$MatchKind)) |>
    dplyr::mutate(KindRank = unname(rank_[.data$MatchKind])) |>
    dplyr::arrange(.data$DocID, .data$Start, .data$KindRank) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::select(DocID, MatchKey = Key, MatchName = Name, MatchSpan = Span, MatchStart = Start,
                  MatchStop = Stop, MatchKind)

  first_ <- .fact |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::select(DocID, FirstKey = Key, FirstName = Name, FirstSpan = Span, FirstStart = Start,
                  FirstStop = Stop)

  seen_ <- dplyr::summarise(.fact, NEnt = dplyr::n(), .by = DocID)

  .keys |>
    dplyr::left_join(.lens,  by = dplyr::join_by(DocID)) |>
    dplyr::left_join(seen_,  by = dplyr::join_by(DocID)) |>
    dplyr::left_join(best_,  by = dplyr::join_by(DocID)) |>
    dplyr::left_join(first_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      NEnt   = as.integer(dplyr::coalesce(.data$NEnt, 0L)),
      Status = dplyr::case_when(
        !is.na(.data$MatchKind) ~ "matched",
        .data$NEnt > 0L         ~ "first",
        TRUE                    ~ "no entity"
      ),
      PartyKey   = dplyr::if_else(.data$Status == "matched", .data$MatchKey,   .data$FirstKey),
      PartyName  = dplyr::if_else(.data$Status == "matched", .data$MatchName,  .data$FirstName),
      PartySpan  = dplyr::if_else(.data$Status == "matched", .data$MatchSpan,  .data$FirstSpan),
      PartyStart = dplyr::if_else(.data$Status == "matched", .data$MatchStart, .data$FirstStart),
      PartyStop  = dplyr::if_else(.data$Status == "matched", .data$MatchStop,  .data$FirstStop),
      PartyFrac  = .data$PartyStart / .data$DocLen,
      IsDeep     = .data$PartyStart > .rule$Deep,
      Party      = stringi::stri_trim_both(
        stringi::stri_replace_all_regex(dplyr::coalesce(.data$PartyName, ""), "\\s+", " ")
      ),
      # The fallback's own error rate, measurable only where the match succeeded.
      MatchIsFirst = dplyr::if_else(
        .data$Status == "matched", .data$MatchKey == .data$FirstKey, NA
      )
    ) |>
    dplyr::relocate(Status, Party, MatchKind, .after = DocID)
}


# 5. The window ------------------------------------------------------------------------------------------------------
# The ONLY window-dependent step, and therefore the only one a window sweep has to re-run. Each
# document gets its own bounds and they are stored, because a window that cannot be audited per
# document is a parameter rather than a rule.

#' Build one window specification
#'
#' @param .kind Character, "fixed" or "gap". FIXED takes a constant number of characters either side
#'   of the party. GAP grows outward from the party through every name whose distance to its
#'   neighbour is at most .par, and stops at the first larger gap. The gap arm exists because a
#'   syndicated preamble is long BECAUSE it lists twelve lenders, so a constant width truncates the
#'   list -- and scaling on document length is the wrong fix, since the same preamble appears in a
#'   forty-page and a four-hundred-page agreement. Measured on the sample, adjacent names sit a
#'   median of 130 characters apart with a P90 of 2,540.
#' @param .par Numeric. Half-width in characters for "fixed"; maximum admitted gap for "gap".
#'   Inf under "fixed" is the whole document, which is the unadjusted baseline.
#' @param .tail_share Numeric. Tail as a share of document length. Relative rather than flat because
#'   a signature block genuinely scales with the number of parties -- twelve lenders take one
#'   sentence in a preamble and twelve signature blocks at the end.
#' @return A named list carrying the window specification.
ent_window_spec <- function(.kind = "fixed", .par = 2000, .tail_share = 0.10) {
  if (FALSE) {
    .kind       <- "fixed"
    .par        <- 2000
    .tail_share <- 0.10
  }

  if (!.kind %in% c("fixed", "gap")) cli::cli_abort("{.arg .kind} must be \"fixed\" or \"gap\".")

  lab_ <- if (identical(.kind, "fixed")) {
    if (is.infinite(.par)) "whole document" else paste0("+/- ", format(.par, big.mark = ","))
  } else {
    paste0("gap ", format(.par, big.mark = ","))
  }

  list(Kind = .kind, Par = .par, TailShare = .tail_share, Label = lab_)
}


#' Cut the window around the party, and the tail behind it
#'
#' The tail is CLAMPED past the window end, so the two regions cannot overlap. That changes what the
#' tail measures: it can no longer hold a name that also sits beside the party, so a name found there
#' is one the party's neighbourhood did not carry -- "something outside the preamble gets named"
#' rather than "the signature block adds names". Where the window swallows the document, there is no
#' tail and the document is reported as having none rather than dropped.
#'
#' @param .fact Tibble from ent_match_kind().
#' @param .party Tibble from ent_locate_party().
#' @param .spec List from ent_window_spec().
#' @return .fact with WinStart, WinEnd, InWindow, TailStart, HasTail, InTail, RecursInTail,
#'   DistToParty, IsFiler and Role added.
ent_window <- function(.fact, .party, .spec) {
  if (FALSE) {
    .fact  <- tab_facts
    .party <- tab_party
    .spec  <- .lP$Params$Spec
  }

  base_ <- .fact |>
    dplyr::left_join(
      dplyr::select(.party, DocID, PartyKey, PartyStart, Status),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::mutate(
      IsFiler     = !is.na(.data$PartyKey) & .data$Key == .data$PartyKey,
      DistToParty = .data$Start - .data$PartyStart
    )

  if (identical(.spec$Kind, "fixed")) {
    win_ <- base_ |>
      dplyr::mutate(
        WinStart = pmax(0, .data$PartyStart - .spec$Par),
        WinEnd   = pmin(.data$DocLen, .data$PartyStart + .spec$Par),
        InWindow = !is.na(.data$PartyStart) & .data$Start >= .data$WinStart &
                   .data$Start <= .data$WinEnd
      )
  } else {
    # Chain outward from the party. A break falls wherever the distance to the previous name exceeds
    # the admitted gap; the party's own run of unbroken names is the window.
    win_ <- base_ |>
      dplyr::mutate(
        Gap     = .data$Start - dplyr::lag(.data$Start),
        Cluster = cumsum(dplyr::coalesce(.data$Gap, 0) > .spec$Par),
        .by = DocID
      ) |>
      dplyr::mutate(
        PartyCluster = .data$Cluster[which(.data$IsFiler)[1L]],
        .by = DocID
      ) |>
      dplyr::mutate(
        InWindow = !is.na(.data$PartyCluster) & .data$Cluster == .data$PartyCluster
      ) |>
      dplyr::mutate(
        WinStart = suppressWarnings(min(.data$Start[.data$InWindow])),
        WinEnd   = suppressWarnings(max(.data$Stop[.data$InWindow])),
        .by = DocID
      ) |>
      dplyr::mutate(
        WinStart = dplyr::if_else(is.finite(.data$WinStart), .data$WinStart, NA_integer_),
        WinEnd   = dplyr::if_else(is.finite(.data$WinEnd),   .data$WinEnd,   NA_integer_)
      )
  }

  win_ |>
    dplyr::mutate(
      TailStart    = pmax(.data$DocLen * (1 - .spec$TailShare), dplyr::coalesce(.data$WinEnd, 0)),
      HasTail      = .data$TailStart < .data$DocLen,
      InTail       = .data$HasTail & .data$Start >= .data$TailStart & !.data$InWindow,
      RecursInTail = .data$HasTail & .data$MaxStart >= .data$TailStart,
      Role         = dplyr::case_when(
        .data$IsFiler  ~ "filer",
        .data$InWindow ~ "counterparty",
        .data$InTail   ~ "signatory",
        TRUE           ~ "other"
      )
    )
}


#' The counts, one row per document
#'
#' NFull is the UNADJUSTED baseline: every distinct organisation the contract names anywhere. It is
#' carried in every table beside the rule's count, because a reader asked to accept a counterparty
#' count needs to see what the rule removed rather than be told that it removed the right things.
#'
#' @param .roles Tibble from ent_window().
#' @param .party Tibble from ent_locate_party().
#' @return Tibble: one row per document with the class facts, the window bounds and the counts.
ent_counts <- function(.roles, .party) {
  if (FALSE) {
    .roles <- tab_roles
    .party <- tab_party
  }

  cnt_ <- .roles |>
    dplyr::summarise(
      NFull     = dplyr::n_distinct(.data$Key),
      NCounter  = dplyr::n_distinct(.data$Key[.data$Role == "counterparty"]),
      NTailNew  = dplyr::n_distinct(.data$Key[.data$Role == "signatory"]),
      NOther    = dplyr::n_distinct(.data$Key[.data$Role == "other"]),
      NBothEnds = dplyr::n_distinct(
        .data$Key[.data$Role %in% c("filer", "counterparty") & .data$RecursInTail]
      ),
      WinStart  = dplyr::first(.data$WinStart),
      WinEnd    = dplyr::first(.data$WinEnd),
      TailStart = dplyr::first(.data$TailStart),
      HasTail   = dplyr::first(.data$HasTail),
      .by = DocID
    )

  .party |>
    dplyr::select(DocID, Class, AmendType, Status, MatchKind, DocLen, PartyStart, PartyFrac,
                  IsDeep) |>
    dplyr::left_join(cnt_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("N"), \(.x) as.integer(dplyr::coalesce(.x, 0L))),
      WinWidth  = .data$WinEnd - .data$WinStart,
      TailWidth = .data$DocLen - .data$TailStart
    )
}


#' Apply one match rule and one window end to end
#'
#' @param .spans Tibble from ent_load_org().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .lens Tibble from ent_doc_lens().
#' @param .rule List from ent_rule().
#' @param .spec List from ent_window_spec().
#' @return A list: Rule, Spec, Ent, Facts, Party, Roles, Counts.
ent_apply <- function(.spans, .keys, .lens, .rule, .spec) {
  if (FALSE) {
    .spans <- tab_spans
    .keys  <- tab_keys
    .lens  <- tab_lens
    .rule  <- .lP$Params$Rule
    .spec  <- .lP$Params$Spec
  }

  ent_   <- ent_entities(.spans = .spans, .rule = .rule)
  freq_  <- ent_token_freq(.ent = ent_)
  fact_  <- ent_match_kind(
    .fact = ent_match_facts(.ent = ent_, .keys = .keys, .freq = freq_), .rule = .rule
  )
  party_ <- ent_locate_party(.fact = fact_, .keys = .keys, .lens = .lens, .rule = .rule)
  roles_ <- ent_window(.fact = fact_, .party = party_, .spec = .spec)

  list(
    Rule   = .rule,
    Spec   = .spec,
    Ent    = ent_,
    Facts  = fact_,
    Party  = party_,
    Roles  = roles_,
    Counts = ent_counts(.roles = roles_, .party = party_)
  )
}


# 6. Sweeps ----------------------------------------------------------------------------------------------------------
# The window sweep is cheap now, because the match is window-free: it is computed once and every
# window is re-cut over the same facts. The match sweep is the expensive one and moves ONE FACTOR AT
# A TIME around a base rule.

#' Cut every window specification over one match, at document grain
#'
#' @param .fact Tibble from ent_match_kind().
#' @param .party Tibble from ent_locate_party().
#' @param .specs List of lists from ent_window_spec().
#' @param .quiet Logical. Suppress the progress bar.
#' @return Tibble: the count table for every spec, stacked, with Spec and SpecOrder.
ent_sweep_window <- function(.fact, .party, .specs, .quiet = FALSE) {
  if (FALSE) {
    .fact   <- tab_facts
    .party  <- tab_party
    .specs  <- .lP$Params$Specs
    .quiet  <- FALSE
  }

  purrr::imap(.specs, function(.s, .i) {
    ent_counts(.roles = ent_window(.fact = .fact, .party = .party, .spec = .s), .party = .party) |>
      dplyr::mutate(Spec = .s$Label, SpecOrder = as.integer(.i), .before = 1L)
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


#' Sweep the match parameters, one at a time around the base rule
#'
#' @param .spans Tibble from ent_load_org().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .lens Tibble from ent_doc_lens().
#' @param .base List from ent_rule().
#' @param .spec List from ent_window_spec(). Held fixed, so only the match moves.
#' @param .quiet Logical. Suppress the progress bar.
#' @return Tibble: one row per variation, with Factor and Setting naming what changed.
ent_sweep_match <- function(.spans, .keys, .lens, .base, .spec, .quiet = FALSE) {
  if (FALSE) {
    .spans <- tab_spans
    .keys  <- tab_keys
    .lens  <- tab_lens
    .base  <- .lP$Params$Rule
    .spec  <- .lP$Params$Spec
    .quiet <- FALSE
  }

  var_ <- list(
    list(Factor = "base",        Setting = "as chosen",         Over = list()),
    list(Factor = "key",         Setting = "raw span",          Over = list(Key = "span")),
    list(Factor = "equality",    Setting = "floor 5, no split", Over = list(MinKey = 5L)),
    list(Factor = "equality",    Setting = "spaces respected",  Over = list(Tight = FALSE)),
    list(Factor = "containment", Setting = "floor 4",           Over = list(MinContain = 4L)),
    list(Factor = "containment", Setting = "floor 6",           Over = list(MinContain = 6L)),
    list(Factor = "fragment",    Setting = "absolute 10 only",  Over = list(FragShare = 0)),
    list(Factor = "fragment",    Setting = "relative 0.8",      Over = list(FragShare = 0.8)),
    list(Factor = "family",      Setting = "off",               Over = list(FamTokens = 99L,
                                                                            FamDf = 0)),
    list(Factor = "family",      Setting = "two tokens only",   Over = list(FamDf = 0)),
    list(Factor = "family",      Setting = "one token, df 2%",  Over = list(FamDf = 0.02))
  )

  purrr::map(var_, function(.v) {
    rule_ <- ent_rule_set(.rule = .base, .over = .v$Over)
    res_  <- ent_apply(.spans = .spans, .keys = .keys, .lens = .lens, .rule = rule_, .spec = .spec)
    p_    <- res_$Party
    c_    <- res_$Counts
    k_    <- p_$MatchKind[!is.na(p_$MatchKind)]

    tibble::tibble(
      Factor      = .v$Factor,
      Setting     = .v$Setting,
      PctMatched  = mean(p_$Status == "matched"),
      PctFirst    = mean(p_$Status == "first"),
      PctExact    = if (length(k_) == 0L) NA_real_ else mean(k_ == "exact"),
      PctFamily   = if (length(k_) == 0L) NA_real_ else mean(k_ == "family"),
      PctDeep     = mean(p_$IsDeep, na.rm = TRUE),
      PctMatchFirst = mean(p_$MatchIsFirst, na.rm = TRUE),
      MedStart    = stats::median(p_$PartyStart, na.rm = TRUE),
      MeanFull    = mean(c_$NFull),
      MeanCounter = mean(c_$NCounter)
    )
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


# 7. Description -----------------------------------------------------------------------------------------------------

#' Share of organisation spans by relative position, weighted two ways
#'
#' Describes the engine and enters no rule. Span-weighted mass can be produced by a handful of
#' table-heavy filings -- 04A measured one document carrying 3,053 organisation spans -- and
#' document-weighted mass cannot.
#'
#' @param .spans Tibble from ent_load_org().
#' @param .bins Integer. Bins across relative position.
#' @return Tibble: Weight, Pos, Share.
ent_density <- function(.spans, .bins = 50L) {
  if (FALSE) {
    .spans <- tab_spans
    .bins  <- 50L
  }

  base_ <- dplyr::mutate(
    .spans, Bin = pmin(.bins, floor(.bins * .data$Start / .data$DocLen) + 1L)
  )

  spans_ <- base_ |>
    dplyr::summarise(N = dplyr::n(), .by = Bin) |>
    dplyr::mutate(Weight = "spans", Share = .data$N / sum(.data$N))

  docs_ <- base_ |>
    dplyr::mutate(W = 1 / dplyr::n(), .by = DocID) |>
    dplyr::summarise(N = sum(.data$W), .by = Bin) |>
    dplyr::mutate(Weight = "documents", Share = .data$N / sum(.data$N))

  dplyr::bind_rows(spans_, docs_) |>
    dplyr::mutate(Pos = (.data$Bin - 0.5) / .bins) |>
    dplyr::select(Weight, Pos, Share)
}


#' How far a span runs past the name it resolves to
#'
#' Reported for the NEXT entity rather than this one. Nothing in the party rule depends on a tight
#' boundary. The geography rule assigns a place to the nearest organisation, so its scale is the gap
#' between adjacent names, and the overshoot against that gap is what says whether a ragged span can
#' move an assignment.
#'
#' @param .ent Tibble from ent_entities().
#' @return Tibble: one row of quantiles and the share of spans holding their own name.
ent_boundary <- function(.ent) {
  if (FALSE) .ent <- tab_ent

  .ent |>
    dplyr::summarise(
      Entities     = dplyr::n(),
      MedNameLen   = stats::median(.data$NameLen),
      MedSpanWidth = stats::median(.data$SpanWidth),
      MedOvershoot = stats::median(.data$Overshoot),
      P90Overshoot = unname(stats::quantile(.data$Overshoot, 0.9)),
      PctOver20    = mean(.data$Overshoot > 20L),
      PctHoldsName = mean(.data$SpanHoldsName)
    )
}


#' The distances the window is cut from
#'
#' @param .roles Tibble from ent_window().
#' @return Tibble: quantiles of the distance to the party and of the gap between adjacent names.
ent_dist_profile <- function(.roles) {
  if (FALSE) .roles <- tab_roles

  ord_ <- dplyr::arrange(.roles, .data$DocID, .data$Start)

  dist_ <- ord_ |>
    dplyr::filter(!.data$IsFiler, !is.na(.data$DistToParty)) |>
    dplyr::pull(.data$DistToParty) |>
    abs()

  gap_ <- ord_ |>
    dplyr::mutate(Gap = .data$Start - dplyr::lag(.data$Start), .by = DocID) |>
    dplyr::filter(!is.na(.data$Gap)) |>
    dplyr::pull(.data$Gap)

  qs_ <- function(.x, .lab) {
    tibble::tibble(
      Measure = .lab,
      N       = length(.x),
      P10     = unname(stats::quantile(.x, 0.10)),
      Median  = stats::median(.x),
      P90     = unname(stats::quantile(.x, 0.90)),
      Mean    = mean(.x)
    )
  }

  dplyr::bind_rows(
    qs_(.x = dist_, .lab = "Any other name to the party, absolute characters"),
    qs_(.x = gap_,  .lab = "Adjacent names, characters apart")
  )
}


#' How often each name in the window occurs across documents
#'
#' REPORTED AND NOT APPLIED. A name in one contract is a party; a name in a tenth of them is a
#' defined term the drafting convention supplies. But a large bank appears in hundreds of credit
#' agreements as a genuine counterparty, so a frequency cut deletes real parties along with
#' boilerplate. PctFiler is what separates the two cases and is reported beside the frequency.
#'
#' @param .roles Tibble from ent_window().
#' @param .n_docs Integer. Denominator, the documents in the sample.
#' @return Tibble: Key, NTok, Docs, PctDocs, PctFiler, Example -- ordered by Docs.
ent_head_terms <- function(.roles, .n_docs) {
  if (FALSE) {
    .roles  <- tab_roles
    .n_docs <- nrow(tab_keys)
  }

  .roles |>
    dplyr::filter(.data$Role %in% c("filer", "counterparty")) |>
    dplyr::summarise(
      NTok     = dplyr::first(.data$NTok),
      Docs     = dplyr::n_distinct(.data$DocID),
      Example  = dplyr::first(.data$Name),
      PctFiler = mean(.data$Role == "filer"),
      .by = Key
    ) |>
    dplyr::mutate(PctDocs = .data$Docs / .n_docs) |>
    dplyr::arrange(dplyr::desc(.data$Docs))
}


# 8. The two class tables --------------------------------------------------------------------------------------------
# Both carry an ALL row, so a class figure can be read against the sample without arithmetic. Class
# is handled as a character here rather than a registered factor, because "All documents" is not a
# contract type and must not enter the taxonomy.

#' Append an "All documents" row to a per-class summary
#'
#' @param .tab Tibble carrying a Class column.
#' @param .fun Function taking the ungrouped tibble and returning a one-row summary.
#' @param .src Tibble the summary is computed from.
#' @return .tab with the ALL row appended and Class as ordered character.
ent_bind_all <- function(.tab, .fun, .src) {
  if (FALSE) {
    .tab <- tab_where
    .fun <- function(.d) dplyr::summarise(.d, Docs = dplyr::n())
    .src <- tab_counts
  }

  lev_ <- plot_levels("ClassDetailed")

  dplyr::bind_rows(
    dplyr::mutate(.tab, Class = as.character(.data$Class)) |>
      dplyr::arrange(match(.data$Class, lev_)),
    dplyr::mutate(.fun(.src), Class = "All documents", .before = 1L)
  )
}


#' Where the contracting party was found, by contract type
#'
#' @param .counts Tibble from ent_counts().
#' @return Tibble: one row per class plus an ALL row.
ent_table_where <- function(.counts) {
  if (FALSE) .counts <- tab_counts

  f_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs        = dplyr::n(),
      PctMatched  = mean(.data$Status == "matched"),
      PctFirst    = mean(.data$Status == "first"),
      PctNoEntity = mean(.data$Status == "no entity"),
      PctExact    = mean(.data$MatchKind == "exact", na.rm = TRUE),
      MedStart    = stats::median(.data$PartyStart, na.rm = TRUE),
      MedFrac     = stats::median(.data$PartyFrac, na.rm = TRUE),
      PctDeep     = mean(.data$IsDeep, na.rm = TRUE)
    )
  }

  ent_bind_all(.tab = f_(dplyr::group_by(.counts, Class)), .fun = f_, .src = .counts)
}


#' What each window yields, by contract type
#'
#' The unadjusted count sits in the same table, so the rule is read against the thing it replaces
#' rather than on its own.
#'
#' @param .sweep Tibble from ent_sweep_window().
#' @param .status Character. "all", "matched" or "first" -- which documents the row is computed over.
#' @return Tibble: one row per class plus an ALL row, one column per window specification.
ent_table_window <- function(.sweep, .status = "all") {
  if (FALSE) {
    .sweep  <- tab_sweep_window
    .status <- "all"
  }

  src_ <- if (identical(.status, "all")) .sweep else dplyr::filter(.sweep, .data$Status == .status)

  wide_ <- src_ |>
    dplyr::summarise(MeanCounter = mean(.data$NCounter), .by = c(Class, Spec, SpecOrder)) |>
    dplyr::arrange(.data$SpecOrder) |>
    dplyr::select(-SpecOrder) |>
    tidyr::pivot_wider(names_from = Spec, values_from = MeanCounter)

  extra_ <- src_ |>
    dplyr::filter(.data$SpecOrder == max(.data$SpecOrder)) |>
    dplyr::summarise(
      Unadjusted    = mean(.data$NFull),
      MedTailWidth  = stats::median(.data$TailWidth),
      MeanTailNew   = mean(.data$NTailNew),
      PctAnyTailNew = mean(.data$NTailNew > 0L),
      MeanBothEnds  = mean(.data$NBothEnds),
      .by = Class
    )

  per_ <- dplyr::left_join(wide_, extra_, by = dplyr::join_by(Class))

  f_ <- function(.d) {
    w_ <- .d |>
      dplyr::summarise(MeanCounter = mean(.data$NCounter), .by = c(Spec, SpecOrder)) |>
      dplyr::arrange(.data$SpecOrder) |>
      dplyr::select(-SpecOrder) |>
      tidyr::pivot_wider(names_from = Spec, values_from = MeanCounter)
    e_ <- .d |>
      dplyr::filter(.data$SpecOrder == max(.data$SpecOrder)) |>
      dplyr::summarise(
        Unadjusted    = mean(.data$NFull),
        MedTailWidth  = stats::median(.data$TailWidth),
        MeanTailNew   = mean(.data$NTailNew),
        PctAnyTailNew = mean(.data$NTailNew > 0L),
        MeanBothEnds  = mean(.data$NBothEnds)
      )
    dplyr::bind_cols(w_, e_)
  }

  ent_bind_all(.tab = per_, .fun = f_, .src = src_)
}


#' What each window yields, by decile of document length
#'
#' The table that settles whether the window has to scale. The earlier length gradient -- a mean
#' counterparty count of 8.3 in the longest decile against 1.5 in the shortest -- was measured under
#' a window that ITSELF scaled with length, so it could not separate "long contracts name more
#' parties" from "we gave long contracts a wider window". At a fixed width it can.
#'
#' @param .sweep Tibble from ent_sweep_window().
#' @return Tibble: one row per decile, one column per window specification.
ent_table_length <- function(.sweep) {
  if (FALSE) .sweep <- tab_sweep_window

  base_ <- .sweep |>
    dplyr::mutate(Decile = dplyr::ntile(.data$DocLen, 10L), .by = Spec)

  wide_ <- base_ |>
    dplyr::summarise(MeanCounter = mean(.data$NCounter), .by = c(Decile, Spec, SpecOrder)) |>
    dplyr::arrange(.data$SpecOrder) |>
    dplyr::select(-SpecOrder) |>
    tidyr::pivot_wider(names_from = Spec, values_from = MeanCounter)

  extra_ <- base_ |>
    dplyr::filter(.data$SpecOrder == max(.data$SpecOrder)) |>
    dplyr::summarise(
      Docs       = dplyr::n(),
      MedLen     = stats::median(.data$DocLen),
      Unadjusted = mean(.data$NFull),
      .by = Decile
    )

  extra_ |>
    dplyr::left_join(wide_, by = dplyr::join_by(Decile)) |>
    dplyr::arrange(.data$Decile)
}


# 9. Reading and checking --------------------------------------------------------------------------------------------

#' Rehydrate a sample of spans with the text either side
#'
#' Sampled deterministically, so the same documents appear on every render and a change in the
#' examples means a change in the extraction rather than a change in the draw.
#'
#' @param .tab Tibble carrying DocID and the two offset columns.
#' @param .path_text 04A's canonical text parquet.
#' @param .col_start Character. Name of the start-offset column.
#' @param .col_stop Character. Name of the stop-offset column.
#' @param .n Integer. Rows drawn.
#' @param .ctx Integer. Characters either side of the span.
#' @param .seed Integer. Sampling seed.
#' @return .tab's columns for the drawn rows, plus Snippet.
ent_read_spans <- function(.tab, .path_text, .col_start = "PartyStart", .col_stop = "PartyStop",
                           .n = 10L, .ctx = 200L, .seed = 42L) {
  if (FALSE) {
    .tab       <- dplyr::filter(tab_party, .data$Status == "matched")
    .path_text <- .lP$Input$Text
    .col_start <- "PartyStart"
    .col_stop  <- "PartyStop"
    .n         <- 10L
    .ctx       <- 200L
    .seed      <- 42L
  }

  pick_ <- .tab |>
    dplyr::filter(!is.na(.data[[.col_start]])) |>
    (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n, nrow(.d)))))()

  if (nrow(pick_) == 0L) return(tibble::tibble())

  pick_ |>
    dplyr::left_join(
      arrow::read_parquet(.path_text) |> dplyr::filter(.data$DocID %in% pick_$DocID),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::mutate(
      # 0-based half-open offsets over code points; stri_sub is 1-based and inclusive, hence the +1.
      # The left edge is clamped, because a negative "from" counts from the END and would silently
      # return a slice from the wrong part of the document.
      Snippet = stringi::stri_replace_all_regex(
        paste0(
          "...",
          stringi::stri_sub(.data$TextRaw,
                            from = pmax(1L, .data[[.col_start]] + 1L - .ctx),
                            to   = .data[[.col_start]]),
          " >>>",
          stringi::stri_sub(.data$TextRaw,
                            from = .data[[.col_start]] + 1L,
                            to   = .data[[.col_stop]]),
          "<<< ",
          stringi::stri_sub(.data$TextRaw,
                            from = .data[[.col_stop]] + 1L,
                            to   = .data[[.col_stop]] + .ctx),
          "..."
        ),
        "\\s+", " "
      )
    ) |>
    dplyr::select(-TextRaw)
}


#' Cut the text at the stored offsets and compare it to the stored span
#'
#' AN EQUALITY, NOT A CONTAINMENT. Asking whether the span appears somewhere inside the slice is
#' answered YES more easily the wider the slice runs, so a pair of offsets bracketing four pages of
#' contract would pass it every time. The slice must BE the span, which is the only form that can
#' fail, and it is what catches an entity whose offsets came from two different mentions.
#'
#' @param .tab Tibble carrying DocID, the two offset columns and the span column.
#' @param .path_text 04A's canonical text parquet.
#' @param .col_start Character. Name of the start-offset column.
#' @param .col_stop Character. Name of the stop-offset column.
#' @param .col_span Character. Name of the column holding the span as stored.
#' @param .n Integer. Rows drawn.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: DocID, the offsets, Stored, Sliced and Exact.
ent_check_offsets <- function(.tab, .path_text, .col_start = "PartyStart", .col_stop = "PartyStop",
                              .col_span = "PartySpan", .n = 500L, .seed = 42L) {
  if (FALSE) {
    .tab       <- tab_party
    .path_text <- .lP$Input$Text
    .col_start <- "PartyStart"
    .col_stop  <- "PartyStop"
    .col_span  <- "PartySpan"
    .n         <- 500L
    .seed      <- 42L
  }

  pick_ <- .tab |>
    dplyr::filter(!is.na(.data[[.col_start]]), !is.na(.data[[.col_span]])) |>
    (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n, nrow(.d)))))()

  if (nrow(pick_) == 0L) return(tibble::tibble())

  pick_ |>
    dplyr::left_join(
      arrow::read_parquet(.path_text) |> dplyr::filter(.data$DocID %in% pick_$DocID),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::transmute(
      DocID,
      Start  = .data[[.col_start]],
      Stop   = .data[[.col_stop]],
      Stored = .data[[.col_span]],
      Sliced = stringi::stri_sub(.data$TextRaw,
                                 from = .data[[.col_start]] + 1L,
                                 to   = .data[[.col_stop]]),
      Exact  = .data$Sliced == .data$Stored
    )
}


#' Every organisation one document names, in order, with its assigned role
#'
#' The block that says whether the counterparty count means anything. A list of six names is equally
#' consistent with six parties and with two parties written four ways.
#'
#' @param .roles Tibble from ent_window().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .n Integer. Documents drawn.
#' @param .max Integer. Rows shown per document, so one table-heavy filing cannot fill the page.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: the drawn documents' rows, ordered by document and position.
ent_read_entities <- function(.roles, .keys, .n = 6L, .max = 12L, .seed = 42L) {
  if (FALSE) {
    .roles <- tab_roles
    .keys  <- tab_keys
    .n     <- 6L
    .max   <- 12L
    .seed  <- 42L
  }

  if (nrow(.roles) == 0L) return(tibble::tibble())

  docs_ <- withr::with_seed(
    .seed, sample(unique(.roles$DocID), size = min(.n, dplyr::n_distinct(.roles$DocID)))
  )

  .roles |>
    dplyr::filter(.data$DocID %in% docs_) |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::slice_head(n = .max, by = DocID) |>
    dplyr::left_join(dplyr::select(.keys, DocID, CompanyName, Class), by = dplyr::join_by(DocID))
}


# 10. Report ---------------------------------------------------------------------------------------------------------

#' How many documents carry a usable anchor key at all
#' @param .tab Tibble from ent_anchor_keys().
#' @param .rule List from ent_rule().
#' @return Invisibly .tab.
ent_report_keys <- function(.tab, .rule) {
  if (FALSE) {
    .tab  <- tab_keys
    .rule <- .lP$Params$Rule
  }

  cli::cli_h2("Anchor keys")
  tibble::tibble(
    Item = c("Documents in the sample",
             "Company name recorded",
             "Name altered by the conformed strip",
             "Key long enough to compare at all",
             "Key long enough for containment"),
    N    = c(nrow(.tab),
             sum(!is.na(.tab$CompanyName)),
             sum(.tab$CompanyName != .tab$CompanyClean, na.rm = TRUE),
             sum(!is.na(.tab$AnchorKey) & .tab$AnchorLen >= .rule$MinKey),
             sum(!is.na(.tab$AnchorKey) & .tab$AnchorLen >= .rule$MinContain))
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$N / nrow(.tab))) |>
    tbl_say(.title = "Availability of the one external fact")

  cli::cli_alert_info(
    "The gap between the last two rows is what the split floor recovers -- keys like CA and WMIH, \\
     too short for containment because CA sits inside CATERPILLAR, and perfectly usable for equality \\
     because they are the company's name."
  )
  invisible(.tab)
}


#' How wide a span runs past the name it resolves to
#' @param .tab Tibble from ent_boundary().
#' @return Invisibly .tab.
ent_report_boundary <- function(.tab) {
  if (FALSE) .tab <- tab_boundary

  cli::cli_h2("Span boundaries against resolved names")
  .tab |>
    dplyr::mutate(PctOver20 = tbl_pct(.data$PctOver20), PctHoldsName = tbl_pct(.data$PctHoldsName)) |>
    tbl_say(.title = "How far a span runs past its own name")

  cli::cli_alert_info(
    "This block exists for the NEXT entity rather than this one. The party rule reads positions and \\
     is insensitive to a ragged boundary; geography assigns a place to the nearest organisation, so \\
     the overshoot has to be read against the gap between adjacent names reported below."
  )
  invisible(.tab)
}


#' The distances the window is cut from
#' @param .tab Tibble from ent_dist_profile().
#' @return Invisibly .tab.
ent_report_dist <- function(.tab) {
  if (FALSE) .tab <- tab_dist

  cli::cli_h2("Distances between names")
  tbl_say(.tab = .tab, .title = "To the party, and between neighbours")

  cli::cli_alert_info(
    "The first row is what a fixed window cuts. The second is what the GAP window chains through: a \\
     syndicated preamble lists its lenders a few dozen characters apart and then stops, so the P90 \\
     of this row is roughly where a preamble ends and the recitals begin."
  )
  invisible(.tab)
}


#' The party match, and how far the fallback can be trusted
#' @param .tab Tibble from ent_locate_party().
#' @return Invisibly the match table.
ent_report_match <- function(.tab) {
  if (FALSE) .tab <- tab_party

  cli::cli_h2("The contracting party")
  .tab |>
    dplyr::summarise(Docs = dplyr::n(), .by = Status) |>
    dplyr::mutate(Share = tbl_pct(.data$Docs / sum(.data$Docs))) |>
    dplyr::arrange(plot_factor(.data$Status, .key = "PartyStatus")) |>
    tbl_say(.title = "Status, over every document in the sample")

  .tab |>
    dplyr::filter(!is.na(.data$MatchKind)) |>
    dplyr::summarise(
      Docs      = dplyr::n(),
      MedStart  = stats::median(.data$PartyStart),
      MedKeyLen = stats::median(nchar(.data$PartyKey)),
      PctDeep   = tbl_pct(mean(.data$IsDeep)),
      .by = MatchKind
    ) |>
    dplyr::mutate(Share = tbl_pct(.data$Docs / sum(.data$Docs))) |>
    dplyr::arrange(plot_factor(.data$MatchKind, .key = "MatchKind")) |>
    tbl_say(.title = "How the filer was recognised")

  out_ <- tibble::tibble(
    Item = c("Matched documents where the party is also the FIRST name in the document",
             "Documents falling back to the first name",
             "Documents with no organisation at all",
             "Matched parties found deep in the document"),
    N    = c(sum(.tab$MatchIsFirst, na.rm = TRUE),
             sum(.tab$Status == "first"),
             sum(.tab$Status == "no entity"),
             sum(.tab$IsDeep & .tab$Status == "matched", na.rm = TRUE)),
    Of   = c(sum(!is.na(.tab$MatchIsFirst)), nrow(.tab), nrow(.tab),
             sum(.tab$Status == "matched"))
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$N / .data$Of))

  tbl_say(.tab = out_, .title = "The fallback, and the anchors that are not preambles")
  cli::cli_alert_info(
    "The first row IS the fallback's accuracy: the share of documents in which taking the first name \\
     would have given the right answer, measurable only where the match succeeded. The last row is \\
     the window that is a neighbourhood rather than a preamble -- accepted and flagged, because the \\
     company is still the right one."
  )
  invisible(out_)
}


#' Where the party sits, by contract type
#' @param .tab Tibble from ent_table_where().
#' @return Invisibly .tab.
ent_report_where <- function(.tab) {
  if (FALSE) .tab <- tab_where

  cli::cli_h2("Where the party was found, by contract type")
  .tab |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
      MedFrac = tbl_pct(.data$MedFrac)
    ) |>
    tbl_say(.title = "One row per type, and one for the sample")

  cli::cli_alert_info(
    "MedStart is in CHARACTERS and MedFrac is the same quantity as a share of the document. Read the \\
     first: the party's offset is close to constant across a wide range of contract length, so the \\
     fraction moves with length rather than with where the party is, and class correlates with \\
     length."
  )
  invisible(.tab)
}


#' What each window yields, by contract type
#' @param .tab Tibble from ent_table_window().
#' @param .title Character. Which population the table covers.
#' @return Invisibly .tab.
ent_report_window_table <- function(.tab, .title = "All documents") {
  if (FALSE) {
    .tab   <- tab_win_all
    .title <- "All documents"
  }

  cli::cli_h2(paste0("Counterparties by window -- ", .title))
  .tab |>
    dplyr::mutate(PctAnyTailNew = tbl_pct(.data$PctAnyTailNew)) |>
    tbl_say(.title = "Mean counterparties per document, and the unadjusted count beside them")

  cli::cli_alert_info(
    "UNADJUSTED is every distinct organisation the contract names anywhere -- the count this rule \\
     replaces, and the only honest thing to read the window columns against. The published figure it \\
     supersedes is a mean of 2.94 partners per contract, which counted MENTIONS across two engines."
  )
  invisible(.tab)
}


#' What each window yields, by decile of document length
#' @param .tab Tibble from ent_table_length().
#' @return Invisibly .tab.
ent_report_length <- function(.tab) {
  if (FALSE) .tab <- tab_length

  cli::cli_h2("Counterparties by window, by decile of document length")
  tbl_say(.tab = .tab, .title = "Whether the window has to scale")

  cli::cli_alert_info(
    "The question this table settles: a fixed window rising steeply with length means long contracts \\
     genuinely name more parties and a constant width truncates them; a fixed window flat across the \\
     deciles while UNADJUSTED rises means the extra names are body prose the window is right to \\
     exclude. The GAP column is the third possibility -- a per-document width taken from the \\
     clustering rather than from a proxy for it."
  )
  invisible(.tab)
}


#' The match sweep
#' @param .tab Tibble from ent_sweep_match().
#' @return Invisibly .tab.
ent_report_sweep_match <- function(.tab) {
  if (FALSE) .tab <- tab_sweep_match

  cli::cli_h2("Match sweep, one factor at a time around the chosen rule")
  .tab |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "Each row differs from the base row in exactly one parameter")

  cli::cli_alert_info(
    "MeanCounter barely moves across these rows and should not: the match decides WHICH entity \\
     anchors the window, not how many names fall inside it. What to read is PctMatched against \\
     PctDeep -- an arm that raises the first while raising the second is buying documents by \\
     anchoring on a mention that is not a preamble."
  )
  invisible(.tab)
}


#' Document frequency of names in the window
#' @param .tab Tibble from ent_head_terms().
#' @param .n Integer. Rows shown.
#' @return Invisibly .tab.
ent_report_terms <- function(.tab, .n = 25L) {
  if (FALSE) {
    .tab <- tab_terms
    .n   <- 25L
  }

  cli::cli_h2("The most frequent names inside the window")
  .tab |>
    dplyr::mutate(PctDocs = tbl_pct(.data$PctDocs), PctFiler = tbl_pct(.data$PctFiler)) |>
    dplyr::select(Key, NTok, Docs, PctDocs, PctFiler, Example) |>
    tbl_say(.title = "Reported, not applied", .n = .n)

  one_ <- mean(.tab$NTok == 1L)
  cli::cli_alert_info(
    "PctFiler is why no threshold is set here: a name in many documents AND matching the filer in \\
     most of them is a large registrant, one in many and matching in none is boilerplate, and a \\
     frequency cut cannot tell them apart. {tbl_pct(one_)} of these names reduce to a SINGLE TOKEN, \\
     which is where the known noise sits -- WILMINGTON from a trust company, OHIO from \\
     \"an Ohio corporation\" -- measured rather than removed."
  )
  invisible(.tab)
}


#' Print a read block
#' @param .tab Tibble carrying Snippet, from ent_read_spans().
#' @param .cols Character vector of columns shown beside the snippet.
#' @param .title Character. Block title.
#' @return Invisibly .tab.
ent_report_read <- function(.tab, .cols = c("CompanyName", "Party", "MatchKind", "PartyStart"),
                            .title = "Spans in context") {
  if (FALSE) {
    .tab   <- tab_read_matched
    .cols  <- c("CompanyName", "Party", "MatchKind", "PartyStart")
    .title <- "Matched parties in context"
  }

  cli::cli_h2(.title)
  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("Nothing to read: the filter selected no rows carrying an offset.")
    return(invisible(.tab))
  }

  purrr::walk(seq_len(nrow(.tab)), function(.i) {
    row_ <- .tab[.i, ]
    hdr_ <- purrr::map_chr(intersect(.cols, names(row_)), function(.c) {
      paste0(.c, ": ", as.character(row_[[.c]]))
    })
    cli::cli_h3(paste(hdr_, collapse = " | "))
    cli::cli_verbatim(strwrap(row_$Snippet, width = 116, prefix = "  "))
  })
  invisible(.tab)
}


#' Every organisation of a few documents, with its role and the window bounds
#' @param .tab Tibble from ent_read_entities().
#' @param .counts Tibble from ent_counts().
#' @return Invisibly .tab.
ent_report_entities <- function(.tab, .counts) {
  if (FALSE) {
    .tab    <- tab_read_ent
    .counts <- tab_counts
  }

  cli::cli_h2("Whole documents, read in order")
  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("No entities to read.")
    return(invisible(.tab))
  }

  purrr::walk(split(.tab, .tab$DocID), function(.d) {
    id_  <- dplyr::first(.d$DocID)
    row_ <- dplyr::filter(.counts, .data$DocID == id_)
    cli::cli_h3(paste0(id_, " -- ", dplyr::first(.d$CompanyName), " (", dplyr::first(.d$Class), ")"))
    cli::cli_text(
      "Window {format(row_$WinStart, big.mark = ',')}-{format(row_$WinEnd, big.mark = ',')} | \\
       tail from {format(round(row_$TailStart), big.mark = ',')} | \\
       document {format(row_$DocLen, big.mark = ',')} | \\
       unadjusted {row_$NFull}, counterparties {row_$NCounter}"
    )
    .d |>
      dplyr::select(Start, Role, NOcc, NTok, DistToParty, RecursInTail, Name) |>
      tbl_say()
  })

  cli::cli_alert_info(
    "The window bounds are printed per document because they ARE the rule: a counterparty count is \\
     only as good as the bounds it was taken from, and those bounds differ for every contract."
  )
  invisible(.tab)
}


#' Every ORG report in order
#'
#' @param .keys Tibble from ent_anchor_keys().
#' @param .rule List from ent_rule().
#' @param .boundary Tibble from ent_boundary().
#' @param .dist Tibble from ent_dist_profile().
#' @param .party Tibble from ent_locate_party().
#' @param .where Tibble from ent_table_where().
#' @param .win_all Tibble from ent_table_window() over all documents.
#' @param .win_matched Tibble from ent_table_window() over matched documents.
#' @param .win_first Tibble from ent_table_window() over fallback documents.
#' @param .length Tibble from ent_table_length().
#' @param .sweep Tibble from ent_sweep_match().
#' @param .terms Tibble from ent_head_terms().
#' @param .n_terms Integer. Rows of the frequency table shown.
#' @return Invisibly NULL.
ent_report_all_org <- function(.keys, .rule, .boundary, .dist, .party, .where, .win_all,
                               .win_matched, .win_first, .length, .sweep, .terms, .n_terms = 25L) {
  if (FALSE) {
    .keys        <- tab_keys
    .rule        <- .lP$Params$Rule
    .boundary    <- tab_boundary
    .dist        <- tab_dist
    .party       <- tab_party
    .where       <- tab_where
    .win_all     <- tab_win_all
    .win_matched <- tab_win_matched
    .win_first   <- tab_win_first
    .length      <- tab_length
    .sweep       <- tab_sweep_match
    .terms       <- tab_terms
    .n_terms     <- 25L
  }

  ent_report_keys(.tab = .keys, .rule = .rule)
  ent_report_boundary(.tab = .boundary)
  ent_report_dist(.tab = .dist)
  ent_report_match(.tab = .party)
  ent_report_where(.tab = .where)
  ent_report_window_table(.tab = .win_all,     .title = "All documents")
  ent_report_window_table(.tab = .win_matched, .title = "Matched parties only")
  ent_report_window_table(.tab = .win_first,   .title = "Fallback parties only")
  ent_report_length(.tab = .length)
  ent_report_sweep_match(.tab = .sweep)
  ent_report_terms(.tab = .terms, .n = .n_terms)
  invisible(NULL)
}


# 11. Figures --------------------------------------------------------------------------------------------------------
# No titles inside these functions: the caption carries them.

#' Density of organisation spans across relative position
#' @param .tab Tibble from ent_density().
#' @return A ggplot object.
ent_plot_density <- function(.tab) {
  if (FALSE) .tab <- tab_density

  .tab |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Pos, y = .data$Share, colour = .data$Weight)) +
    ggplot2::geom_line(linewidth = 0.7) +
    plot_scale_colour_cat(name = NULL) +
    plot_scale_x_pct(.expand = c(0, 0)) +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}


#' Party status by contract type, as shares within each type
#' @param .tab Tibble from ent_counts().
#' @return A ggplot object.
ent_plot_status <- function(.tab) {
  if (FALSE) .tab <- tab_counts

  .tab |>
    dplyr::summarise(N = dplyr::n(), .by = c(Class, Status)) |>
    plot_bar_stacked(
      .cat      = "Class",
      .val      = "N",
      .fill     = "Status",
      .key      = "ClassDetailed",
      .key_fill = "PartyStatus",
      .short    = FALSE,
      .share    = TRUE
    )
}


#' Mean counterparties by contract type and window specification
#' @param .tab Tibble from ent_sweep_window().
#' @return A ggplot object.
ent_plot_window <- function(.tab) {
  if (FALSE) .tab <- tab_sweep_window

  .tab |>
    dplyr::summarise(MeanCounter = mean(.data$NCounter), .by = c(Class, Spec, SpecOrder)) |>
    dplyr::mutate(Spec = forcats::fct_reorder(.data$Spec, .data$SpecOrder)) |>
    plot_heatmap(
      .x        = "Spec",
      .y        = "Class",
      .fill     = "MeanCounter",
      .key_y    = "ClassDetailed",
      .short    = FALSE,
      .label    = TRUE,
      .pct      = FALSE,
      .accuracy = 0.1
    )
}


#' The rule against the unadjusted count, by contract type
#' @param .tab Tibble from ent_counts().
#' @return A ggplot object.
ent_plot_baseline <- function(.tab) {
  if (FALSE) .tab <- tab_counts

  .tab |>
    dplyr::summarise(
      Removed = mean(.data$NFull) - mean(.data$NCounter),
      Kept    = mean(.data$NCounter),
      .by = Class
    ) |>
    tidyr::pivot_longer(cols = c(Kept, Removed), names_to = "Part", values_to = "N") |>
    plot_bar_stacked(
      .cat      = "Class",
      .val      = "N",
      .fill     = "Part",
      .key      = "ClassDetailed",
      .short    = FALSE,
      .share    = FALSE
    )
}
