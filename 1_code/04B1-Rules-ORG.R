# 04B1-Rules-ORG: the contracting party and its counterparties --------------------------------------------------------
#
# WHAT THIS FILE DOES
# 04A extracted nine engines over 4,398 hand-classified contracts and ranked none of them, because
# ranking needs labels and there are no entity labels. 04B writes the rules, ONE DOCUMENT PER ENTITY.
# This is the first: organisations, because every other party quantity -- the jurisdiction, the
# address, the signing date -- is measured from where an organisation was found.
#
# Shared tooling lives in _Commons/_Entity.R: the name reduction, the store loader, the span reader,
# the offset check and the table shapes. Everything here is a RULE, and rules belong to one entity.
#
# ONE ENGINE, AND THAT IS THE BINDING CONSTRAINT
# 04C runs five engines over the corpus and none of them is spaCy. For ORG that leaves lexnlp alone,
# which emits 81,467 spans against spaCy's 5.3 million, so the whole rule runs in R over a table that
# fits in memory and DuckDB is read once.
#
# THE PARTY IS THE ANCHOR, NOT A REGION
# An earlier design read a fixed head and asked whether the filer was inside it. Sweeping that head
# across twenty settings showed the located share rising from 70.6% to 75.1% and the LATE share
# falling from 6.5% to 2.0%, the two summing to 77.1% in every cell. That is an identity: the match
# runs on names and cannot depend on a region, so a wider head bought no filer and only relabelled
# one already found, while the candidate count doubled. So there is no head. The filer is matched
# ANYWHERE, the window is centred on WHERE IT WAS FOUND, and every document carries its own bounds.
#
# AN ENTITY IS A GROUP OF SPANS, AND ITS OFFSETS COME FROM ONE ROW OF THAT GROUP
# The mentions collapse to one row per (document, company), and that row is the EARLIEST mention
# taken WHOLE. A start from one mention beside a stop from another produces a pair belonging to no
# mention at all and brackets the entire agreement.
#
# WHAT MEASUREMENT SETTLED, AND WHAT IS STILL A ROW IN A SWEEP
# Settled: the window does not scale with length (unadjusted counts rise 13x across length deciles
# while a fixed window rises 2x); a gap window buys nothing over a fixed one (gap 500 and +/- 1,000
# agree to three decimals); the tail is relative rather than flat (a flat 3,000 tail is WIDER on the
# median than a 10% tail and finds a third fewer names, because a relative tail puts its width where
# the signature blocks are); span boundaries are tight (median overshoot 5 characters, 99.0% of spans
# contain their own name); and the deep-family guard is OFF, because reading the ten matches it would
# refuse found six to be the right corporate family.
# Open, and therefore swept: the released width, the tail share, and the fragment rule.
#
# THE POINT IS TO REDUCE NOISE, NOT TO ELIMINATE IT. No stoplist, no defined-term filter. Every table
# carries the UNADJUSTED count -- every distinct organisation the contract names anywhere -- so the
# reader sees what the rule removed rather than being asked to trust that it removed the right things.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_text <- .lP$Input$Text
  .db_path   <- .lP$Input$Store
}


# 1. Vocabulary ------------------------------------------------------------------------------------------------------
# OrgRole is NOT registered here. It describes the Role column of roles_org.parquet, which 04B2 reads
# without sourcing this file, so it lives in _Commons/_Entity.R with the rest of the shared artifact's
# schema. What follows is private to this document.

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


# 2. The match -------------------------------------------------------------------------------------------------------
# WINDOW-FREE, all of it. The match does not depend on where in the document anything sits, so it is
# computed once and every window is cut over the same result.

#' Build one match specification
#'
#' @param .key Character, "core" or "span". Which reduction identifies an entity.
#' @param .min_key Integer. Shortest key admitted for EQUALITY. Two, because "CA, INC." reduces to
#'   "CA" and that is the company's name rather than a degenerate key. Splitting this floor from the
#'   containment floor recovered 152 documents that had no key at all.
#' @param .min_contain Integer. Shortest key admitted for CONTAINMENT or family. Five, because "CA"
#'   sits inside "CATERPILLAR" and a key matching everything locates nothing.
#' @param .tight Logical. Also compare with spaces removed, so UNITED AIRLINES and UNITED AIR LINES
#'   are one company. They are, and nothing else in the reduction reaches that.
#' @param .frag_min Integer. Shortest fragment admitted as a reverse match.
#' @param .frag_share Numeric. Fragment must also cover this share of the anchor key.
#' @param .fam_tokens Integer. Leading tokens two keys must share to count as one corporate family.
#' @param .fam_share Numeric. Those tokens must also be this share of the shorter key.
#' @param .fam_df Numeric. A SINGLE shared leading token is admitted when it opens the name of a
#'   company in at most this share of documents. CHENIERE is evidence and NATIONAL is not.
#' @param .deep Integer. A matched party found past this offset is FLAGGED rather than rejected: it
#'   is still the right company, but the window around it is a neighbourhood and not a preamble.
#' @param .exact_within Numeric. Where the chosen match is NOT exact and an exact match exists this
#'   many characters or fewer further on, take the exact one instead. Position-first was right
#'   against a quality-first rule that would reach character 8,000; it is wrong against an exact
#'   match 450 characters later, which is how "Grant Date, Hubbell" was released as a party name
#'   while the clean "Hubbell" sat further down the same preamble. Zero is pure position-first.
#'
#'   FIVE HUNDRED, from measurement. An exact match sits further on in 17.0% of matched documents.
#'   At 500 the override moves the exact share from 67.8% to 75.7% and leaves the mean counterparty
#'   count at 1.711 -- it changes which entity anchors the window, not how many names fall inside it.
#'   At 2,000 it reaches 78.0% but starts catching pairs 58,000 characters apart, which is a
#'   different part of the document rather than a contaminated span. The fallback's measured accuracy
#'   FALLS under the override, from 71.2% to 64.6%, and that is a correction rather than a cost: the
#'   contaminated span is almost always the first entity in the document, so "the party is also the
#'   first name" was partly true by construction.
#' @param .guard_deep_family Logical. Refuse a ONE-TOKEN family match past .deep. FALSE, because
#'   reading the ten it would refuse found six to be the right corporate family -- Nielsen Holdings
#'   to The Nielsen Company, NewAlliance Bancshares to NewAlliance Bank -- against four bad ones, and
#'   refusing sends all ten to a fallback that is right 71% of the time. Kept as a swept row.
#' @return A named list carrying the match specification.
ent_rule <- function(.key = "core", .min_key = 2L, .min_contain = 5L, .tight = TRUE,
                     .frag_min = 10L, .frag_share = 0.6,
                     .fam_tokens = 2L, .fam_share = 0.5, .fam_df = 0.005,
                     .deep = 5000L, .exact_within = 500, .guard_deep_family = FALSE) {
  if (FALSE) {
    .key               <- "core"
    .min_key           <- 2L
    .min_contain       <- 5L
    .tight             <- TRUE
    .frag_min          <- 10L
    .frag_share        <- 0.6
    .fam_tokens        <- 2L
    .fam_share         <- 0.5
    .fam_df            <- 0.005
    .deep              <- 5000L
    .exact_within      <- 500
    .guard_deep_family <- FALSE
  }

  if (!.key %in% c("core", "span")) cli::cli_abort("{.arg .key} must be \"core\" or \"span\".")

  list(
    Key        = .key,                     MinKey    = as.integer(.min_key),
    MinContain = as.integer(.min_contain), Tight     = isTRUE(.tight),
    FragMin    = as.integer(.frag_min),    FragShare = .frag_share,
    FamTokens  = as.integer(.fam_tokens),  FamShare  = .fam_share, FamDf = .fam_df,
    Deep       = as.integer(.deep),        ExactWithin = .exact_within,
    GuardDeepFamily = isTRUE(.guard_deep_family)
  )
}


#' Override fields of a rule, coercing the integer and logical ones
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
  out_$Tight           <- isTRUE(out_$Tight)
  out_$GuardDeepFamily <- isTRUE(out_$GuardDeepFamily)
  out_
}


#' Collapse mentions into entities, one row per document per name
#'
#' lexnlp returns "Icosavax, Inc.", "Icosavax Inc" and "Icosavax, Inc" as three spans and one party.
#'
#' THE OFFSETS COME FROM ONE ROW, SELECTED WHOLE, by slice_min(). NOcc and MaxStart describe the
#' GROUP and are aggregated separately -- they are not properties of any single mention. MaxStart is
#' what makes corroboration possible once the tail is clamped past the window: an entity introduced
#' beside the party and mentioned AGAIN at the end was introduced and signed.
#'
#' @param .spans Tibble from ent_load_label() with SpanKey and CoreKey added.
#' @param .rule List from ent_rule(). Supplies only the choice of key.
#' @return Tibble: one row per document per key, with the position, the group facts and the boundary
#'   measurements.
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


#' Mark single-token keys that are a token of a longer key in the SAME document
#'
#' lexnlp splits a long name across mentions: "Florida East Coast Railway" appears at character 286
#' and the bare word "Railway" at 580, and both survive the collapse as separate entities. One
#' company, counted twice. "Pier 1 Imports Services Company" splits the same way into "Services
#' Company" and "Imports".
#'
#' WITHIN-DOCUMENT, so this needs no vocabulary and no threshold. A single-token key that appears as
#' a whole token of another key in the same contract is a fragment of it; the same word in a document
#' where no longer name contains it is left alone. Multi-token keys are never marked.
#'
#' Marked and not acted on here: whether a fragment may be a counterparty is a window parameter, and
#' a fragment may still be the PARTY -- "PPL" is the right company for PPL ENERGY SUPPLY LLC.
#'
#' @param .ent Tibble from ent_entities().
#' @return .ent with IsFragment added.
ent_mark_fragments <- function(.ent) {
  if (FALSE) .ent <- tab_ent

  toks_ <- .ent |>
    dplyr::filter(.data$NTok > 1L) |>
    dplyr::mutate(Tok = stringi::stri_split_fixed(.data$Key, " ")) |>
    tidyr::unnest_longer(col = Tok) |>
    dplyr::distinct(DocID, Tok) |>
    dplyr::mutate(InLonger = TRUE)

  .ent |>
    dplyr::left_join(toks_, by = dplyr::join_by(DocID, Key == Tok)) |>
    dplyr::mutate(
      IsFragment = .data$NTok == 1L & dplyr::coalesce(.data$InLonger, FALSE)
    ) |>
    dplyr::select(-InLonger)
}


#' How often each leading token opens a company name, across documents
#'
#' The gate on the one-token family arm. CHENIERE opens a name in a handful of contracts, so CHENIERE
#' ENERGY against CHENIERE CREOLE TRAIL PIPELINE is one corporate family; NATIONAL opens hundreds and
#' shares nothing. Counting DOCUMENTS rather than entities, so one filing listing twenty affiliates
#' does not make its own prefix look common.
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
#' @param .ent Tibble from ent_mark_fragments().
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
      TokShare  = dplyr::coalesce(.data$TokShare, 0),
      KeyLen    = nchar(.data$Key),
      KeyTok    = stringi::stri_count_fixed(.data$Key, " ") + 1L,
      IsExact   = !is.na(.data$AnchorKey) & .data$Key == .data$AnchorKey,
      IsTight   = !is.na(.data$AnchorKey) & .data$KeyTight == .data$AnchorTight,
      IsFwd     = !is.na(.data$AnchorKey) &
                  stringi::stri_detect_fixed(.data$Key, .data$AnchorKey),
      IsRev     = !is.na(.data$AnchorKey) &
                  stringi::stri_detect_fixed(.data$AnchorKey, .data$Key),
      SharedTok = ent_shared_tokens(.a = .data$Key, .b = .data$AnchorKey),
      MinTok    = pmin(.data$KeyTok, .data$AnchorTok)
    )
}


#' Turn the match facts into one match kind, under this rule's thresholds
#'
#' TWO FLOORS, GATING DIFFERENT ARMS. Equality is admitted down to MinKey; containment and family
#' only from MinContain. Precedence is exact, forward, family, reverse -- safest first; family and
#' reverse can both fire on the same pair and the order settles it rather than leaving it to row
#' order.
#'
#' @param .fact Tibble from ent_match_facts().
#' @param .rule List from ent_rule().
#' @return .fact with CanMatch, CanContain, FragFloor, IsFam1, IsFam and MatchKind added.
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
      IsFamN     = .data$SharedTok >= .rule$FamTokens &
                   .data$SharedTok / pmax(.data$MinTok, 1L) >= .rule$FamShare,
      IsFam1     = .data$SharedTok == 1L & .rule$FamDf > 0 & .data$TokShare <= .rule$FamDf &
                   !(.rule$GuardDeepFamily & .data$Start > .rule$Deep),
      IsFam      = .data$IsFamN | .data$IsFam1,
      IsRevOk    = .data$IsRev & .data$KeyLen >= .data$FragFloor,
      MatchKind  = dplyr::case_when(
        is.na(.data$AnchorKey)      ~ NA_character_,
        !.data$CanMatch             ~ NA_character_,
        .data$IsExact               ~ "exact",
        .rule$Tight & .data$IsTight ~ "exact",
        !.data$CanContain           ~ NA_character_,
        .data$IsFwd                 ~ "forward",
        .data$IsFam                 ~ "family",
        .data$IsRevOk               ~ "reverse",
        TRUE                        ~ NA_character_
      )
    )
}


#' One row per document: which entity is the contracting party
#'
#' RUNS FROM THE KEY SIDE, not from the matches. A document where lexnlp proposed nothing resembling
#' the filer's name carries no row in the entity table, so a join in the other direction would drop
#' it and the coverage figure would be computed over the documents that worked.
#'
#' THE PARTY IS MATCHED ANYWHERE. The earliest match wins, with match quality breaking ties at the
#' same position. Where nothing matches, the earliest entity is taken and Status records that it was.
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
                  MatchStop = Stop, MatchKind, MatchTok = SharedTok)

  # The earliest EXACT match, wherever it sits. Carried whether or not the rule acts on it, because
  # the diagnostic is the point: position-first releases "Grant Date, Hubbell" as a party name where
  # LexNLP prepended a document title to the first mention and the clean name follows 450 characters
  # later. How often that happens is a number rather than an anecdote once this column exists.
  exact_ <- .fact |>
    dplyr::filter(.data$MatchKind == "exact") |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::select(DocID, ExactKey = Key, ExactName = Name, ExactSpan = Span, ExactStart = Start,
                  ExactStop = Stop)

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
    dplyr::left_join(exact_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(first_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      NEnt = as.integer(dplyr::coalesce(.data$NEnt, 0L)),
      # Diagnostic first, override second, so the share is reported whatever the rule does with it.
      ExactGap    = .data$ExactStart - .data$MatchStart,
      HasLateExact = !is.na(.data$MatchKind) & .data$MatchKind != "exact" &
                     !is.na(.data$ExactStart) & .data$ExactGap > 0,
      UseExact    = .data$HasLateExact & .rule$ExactWithin > 0 &
                    .data$ExactGap <= .rule$ExactWithin,
      MatchKey    = dplyr::if_else(.data$UseExact, .data$ExactKey,   .data$MatchKey),
      MatchName   = dplyr::if_else(.data$UseExact, .data$ExactName,  .data$MatchName),
      MatchSpan   = dplyr::if_else(.data$UseExact, .data$ExactSpan,  .data$MatchSpan),
      MatchStart  = dplyr::if_else(.data$UseExact, .data$ExactStart, .data$MatchStart),
      MatchStop   = dplyr::if_else(.data$UseExact, .data$ExactStop,  .data$MatchStop),
      MatchKind   = dplyr::if_else(.data$UseExact, "exact",          .data$MatchKind),
      MatchTok    = dplyr::if_else(.data$UseExact, NA_integer_,      .data$MatchTok),
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
      MatchIsFirst = dplyr::if_else(
        .data$Status == "matched", .data$MatchKey == .data$FirstKey, NA
      )
    ) |>
    dplyr::relocate(Status, Party, MatchKind, .after = DocID)
}


# 3. The window ------------------------------------------------------------------------------------------------------
# The ONLY window-dependent step, and therefore the only one a window sweep has to re-run.

#' Build one window specification
#'
#' @param .kind Character, "fixed" or "gap". FIXED takes a constant number of characters either side
#'   of the party. GAP grows outward through every name whose distance to its neighbour is at most
#'   .par. The gap arm is REPORTED rather than chosen: gap 500 and +/- 1,000 returned the same mean
#'   counterparty count to three decimals, and the gap columns track the fixed ones across every
#'   length decile without exceeding them.
#' @param .par Numeric. Half-width in characters for "fixed"; maximum admitted gap for "gap".
#' @param .tail_share Numeric. Tail as a share of document length. RELATIVE rather than flat, and the
#'   evidence is direct: a flat 3,000-character tail is wider on the median than a 10% tail and finds
#'   a third fewer names, because a relative tail puts its width on the long agreements where the
#'   signature blocks with many parties actually are.
#' @param .tail_floor Numeric. Minimum tail width in characters, so a flat tail can be swept against
#'   the relative one by setting .tail_share to zero.
#' @param .drop_fragments Logical. Exclude single-token fragments of a longer name in the same
#'   document from the counterparty count -- the "Railway" in a contract that also names "Florida
#'   East Coast Railway". The party may still be a fragment.
#' @param .label Character or NULL. Overrides the generated label, used where several specifications
#'   differ only in a parameter the label would not otherwise show.
#' @return A named list carrying the window specification.
ent_window_spec <- function(.kind = "fixed", .par = 2000, .tail_share = 0.10, .tail_floor = 0,
                            .drop_fragments = FALSE, .label = NULL) {
  if (FALSE) {
    .kind           <- "fixed"
    .par            <- 2000
    .tail_share     <- 0.10
    .tail_floor     <- 0
    .drop_fragments <- FALSE
    .label          <- NULL
  }

  if (!.kind %in% c("fixed", "gap")) cli::cli_abort("{.arg .kind} must be \"fixed\" or \"gap\".")

  lab_ <- if (!is.null(.label)) {
    .label
  } else if (identical(.kind, "fixed")) {
    paste0("+/- ", format(.par, big.mark = ","))
  } else {
    paste0("gap ", format(.par, big.mark = ","))
  }

  list(Kind = .kind, Par = .par, TailShare = .tail_share, TailFloor = .tail_floor,
       DropFragments = isTRUE(.drop_fragments), Label = lab_)
}


#' Cut the window around the party, and the tail behind it
#'
#' The tail is CLAMPED past the window end, so the two cannot overlap. That changes what it measures:
#' a name found there cannot also sit beside the party, so the tail answers "does anything outside
#' the party's neighbourhood get named" rather than "does the signature block add names". Nothing
#' checks that those names are in a signature block rather than an exhibit list, and that is accepted
#' -- the strategy is to reduce noise, not to eliminate it.
#'
#' @param .fact Tibble from ent_match_kind().
#' @param .party Tibble from ent_locate_party().
#' @param .spec List from ent_window_spec().
#' @return .fact with the window, the tail, DistToParty, IsFiler and Role added.
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
      dplyr::mutate(PartyCluster = .data$Cluster[which(.data$IsFiler)[1L]], .by = DocID) |>
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
      TailWant     = pmax(.spec$TailFloor, .spec$TailShare * .data$DocLen),
      TailStart    = pmax(.data$DocLen - .data$TailWant, dplyr::coalesce(.data$WinEnd, 0)),
      HasTail      = .data$TailStart < .data$DocLen,
      InTail       = .data$HasTail & .data$Start >= .data$TailStart & !.data$InWindow,
      RecursInTail = .data$HasTail & .data$MaxStart >= .data$TailStart,
      Role         = dplyr::case_when(
        .data$IsFiler                                   ~ "filer",
        .data$InWindow & .spec$DropFragments &
          .data$IsFragment                              ~ "fragment",
        .data$InWindow                                  ~ "counterparty",
        .data$InTail                                    ~ "signatory",
        TRUE                                            ~ "other"
      )
    )
}


#' The counts, one row per document
#'
#' NFull is the UNADJUSTED baseline: every distinct organisation the contract names anywhere. It is
#' carried in every table beside the rule's count, because a reader asked to accept a counterparty
#' count needs to see what the rule removed.
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
      NFragment = dplyr::n_distinct(.data$Key[.data$Role == "fragment"]),
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
      HasTail   = dplyr::coalesce(.data$HasTail, FALSE),
      WinWidth  = .data$WinEnd - .data$WinStart,
      TailWidth = .data$DocLen - .data$TailStart
    )
}


#' Apply one match rule and one window end to end
#'
#' @param .spans Tibble from the loader, with SpanKey and CoreKey added.
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

  ent_   <- ent_mark_fragments(.ent = ent_entities(.spans = .spans, .rule = .rule))
  freq_  <- ent_token_freq(.ent = ent_)
  fact_  <- ent_match_kind(
    .fact = ent_match_facts(.ent = ent_, .keys = .keys, .freq = freq_), .rule = .rule
  )
  party_ <- ent_locate_party(.fact = fact_, .keys = .keys, .lens = .lens, .rule = .rule)
  roles_ <- ent_window(.fact = fact_, .party = party_, .spec = .spec)

  list(
    Rule = .rule, Spec = .spec, Ent = ent_, Facts = fact_, Party = party_, Roles = roles_,
    Counts = ent_counts(.roles = roles_, .party = party_)
  )
}


# 4. Sweeps ----------------------------------------------------------------------------------------------------------

#' Cut every window specification over one match, at document grain
#'
#' @param .fact Tibble from ent_match_kind().
#' @param .party Tibble from ent_locate_party().
#' @param .specs List of lists from ent_window_spec().
#' @param .quiet Logical. Suppress the progress bar.
#' @return Tibble: the count table for every spec, stacked, with Spec and SpecOrder.
ent_sweep_window <- function(.fact, .party, .specs, .quiet = FALSE) {
  if (FALSE) {
    .fact  <- tab_facts
    .party <- tab_party
    .specs <- .lP$Params$Specs
    .quiet <- FALSE
  }

  purrr::imap(.specs, function(.s, .i) {
    ent_counts(.roles = ent_window(.fact = .fact, .party = .party, .spec = .s), .party = .party) |>
      dplyr::mutate(Spec = .s$Label, SpecOrder = as.integer(.i), .before = 1L)
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


#' Sweep the match parameters, one at a time around the base rule
#'
#' @param .spans Tibble from the loader.
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
    list(Factor = "family",      Setting = "one token, df 2%",  Over = list(FamDf = 0.02)),
    list(Factor = "family",      Setting = "deep guard on",     Over = list(GuardDeepFamily = TRUE)),
    list(Factor = "ordering",    Setting = "position first",    Over = list(ExactWithin = 0)),
    list(Factor = "ordering",    Setting = "exact within 2,000", Over = list(ExactWithin = 2000))
  )

  purrr::map(var_, function(.v) {
    rule_ <- ent_rule_set(.rule = .base, .over = .v$Over)
    res_  <- ent_apply(.spans = .spans, .keys = .keys, .lens = .lens, .rule = rule_, .spec = .spec)
    p_    <- res_$Party
    c_    <- res_$Counts
    k_    <- p_$MatchKind[!is.na(p_$MatchKind)]

    tibble::tibble(
      Factor        = .v$Factor,
      Setting       = .v$Setting,
      PctMatched    = mean(p_$Status == "matched"),
      PctFirst      = mean(p_$Status == "first"),
      PctExact      = if (length(k_) == 0L) NA_real_ else mean(k_ == "exact"),
      PctFamily     = if (length(k_) == 0L) NA_real_ else mean(k_ == "family"),
      PctDeep       = mean(p_$IsDeep, na.rm = TRUE),
      PctDeepFamily = mean(p_$IsDeep[!is.na(p_$MatchKind) & p_$MatchKind == "family"], na.rm = TRUE),
      PctMatchFirst = mean(p_$MatchIsFirst, na.rm = TRUE),
      PctLateExact  = mean(p_$HasLateExact, na.rm = TRUE),
      MedStart      = stats::median(p_$PartyStart, na.rm = TRUE),
      MeanCounter   = mean(c_$NCounter)
    )
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


# 5. Description -----------------------------------------------------------------------------------------------------

#' How many names sit within reach of the party, at each candidate half-width
#'
#' CONDITIONED ON THE BOUND, which is the only form that informs a width. Reporting the quantiles of
#' the distance over EVERY name in the document instead gives a median in the tens of thousands of
#' characters, because most organisations a long contract names are body prose -- true, and useless
#' for choosing a window.
#'
#' The distribution has no elbow: the count rises steadily with the width and never flattens. So the
#' released width is argued from the NEIGHBOUR GAP instead -- a window has to clear the gap between
#' adjacent names, or it cuts a list of parties in half.
#'
#' @param .roles Tibble from ent_window().
#' @param .bounds Numeric vector of half-widths to report.
#' @return Tibble: one row per bound.
ent_dist_profile <- function(.roles, .bounds = c(500, 1000, 2000, 4000, 10000)) {
  if (FALSE) {
    .roles  <- tab_roles
    .bounds <- c(500, 1000, 2000, 4000, 10000)
  }

  d_ <- .roles |>
    dplyr::filter(!.data$IsFiler, !is.na(.data$DistToParty)) |>
    dplyr::mutate(Abs = abs(.data$DistToParty))

  n_    <- nrow(d_)
  ndoc_ <- dplyr::n_distinct(.roles$DocID)

  purrr::map(.bounds, function(.b) {
    in_ <- d_$Abs[d_$Abs <= .b]
    tibble::tibble(
      Bound      = .b,
      NWithin    = length(in_),
      PctOfNames = length(in_) / max(n_, 1L),
      MeanPerDoc = length(in_) / ndoc_,
      MedWithin  = if (length(in_) == 0L) NA_real_ else stats::median(in_),
      P90Within  = if (length(in_) == 0L) NA_real_ else unname(stats::quantile(in_, 0.9))
    )
  }) |>
    purrr::list_rbind()
}


#' How far apart adjacent names sit, among those near the party
#'
#' THE NUMBER THE RELEASED WIDTH IS ARGUED FROM. A window narrower than the gap between adjacent
#' names cuts a list of parties in half; one wider than the P90 of that gap does not, in nine
#' documents out of ten. Restricted to names within .bound of the party, because across a whole
#' contract the gaps are dominated by body prose.
#'
#' @param .roles Tibble from ent_window().
#' @param .bound Numeric. Only names this close to the party are considered.
#' @return Tibble: one row of quantiles.
ent_gap_profile <- function(.roles, .bound = 4000) {
  if (FALSE) {
    .roles <- tab_roles
    .bound <- 4000
  }

  g_ <- .roles |>
    dplyr::filter(!is.na(.data$DistToParty), abs(.data$DistToParty) <= .bound) |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::mutate(Gap = .data$Start - dplyr::lag(.data$Start), .by = DocID) |>
    dplyr::filter(!is.na(.data$Gap))

  tibble::tibble(
    Bound  = .bound,
    N      = nrow(g_),
    P10    = unname(stats::quantile(g_$Gap, 0.10)),
    Median = stats::median(g_$Gap),
    P90    = unname(stats::quantile(g_$Gap, 0.90)),
    Mean   = mean(g_$Gap)
  )
}


#' How often each name in the window occurs across documents
#'
#' REPORTED AND NOT APPLIED. A name in one contract is a party; a name in a tenth of them is a
#' defined term. But a large bank appears in hundreds of credit agreements as a genuine counterparty,
#' so a frequency cut deletes real parties along with boilerplate. PctFiler separates the two cases
#' and is reported beside the frequency.
#'
#' @param .roles Tibble from ent_window().
#' @param .n_docs Integer. Denominator, the documents in the sample.
#' @return Tibble: Key, NTok, IsFragment, Docs, PctDocs, PctFiler, Example -- ordered by Docs.
ent_window_terms <- function(.roles, .n_docs) {
  if (FALSE) {
    .roles  <- tab_roles
    .n_docs <- nrow(tab_keys)
  }

  .roles |>
    dplyr::filter(.data$Role %in% c("filer", "counterparty", "fragment")) |>
    dplyr::summarise(
      NTok     = dplyr::first(.data$NTok),
      PctFrag  = mean(.data$IsFragment),
      Docs     = dplyr::n_distinct(.data$DocID),
      Example  = dplyr::first(.data$Name),
      PctFiler = mean(.data$Role == "filer"),
      .by = Key
    ) |>
    dplyr::mutate(PctDocs = .data$Docs / .n_docs) |>
    dplyr::arrange(dplyr::desc(.data$Docs))
}


# 6. Tables ----------------------------------------------------------------------------------------------------------

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
#' Window columns only. The tail belongs to a separate table because it is cut by a separate
#' parameter, and mixing the two is how an earlier version reported every tail column as zero: the
#' tail block was computed from the widest window in the sweep, where the window swallows the
#' document and the tail is empty by construction.
#'
#' @param .sweep Tibble from ent_sweep_window().
#' @param .counts Tibble from ent_counts(). Supplies the unadjusted baseline.
#' @param .status Character. "all", "matched" or "first" -- which documents the row covers.
#' @return Tibble: one row per class plus an ALL row, one column per window specification.
ent_table_window <- function(.sweep, .counts, .status = "all") {
  if (FALSE) {
    .sweep  <- tab_sweep_window
    .counts <- tab_counts
    .status <- "all"
  }

  keep_ <- if (identical(.status, "all")) {
    .counts$DocID
  } else {
    .counts$DocID[.counts$Status == .status]
  }

  src_ <- dplyr::filter(.sweep,  .data$DocID %in% keep_)
  cnt_ <- dplyr::filter(.counts, .data$DocID %in% keep_)

  wide_ <- src_ |>
    dplyr::summarise(MeanCounter = mean(.data$NCounter), .by = c(Class, Spec, SpecOrder)) |>
    dplyr::arrange(.data$SpecOrder) |>
    dplyr::select(-SpecOrder) |>
    tidyr::pivot_wider(names_from = Spec, values_from = MeanCounter)

  base_ <- dplyr::summarise(cnt_, Docs = dplyr::n(), Unadjusted = mean(.data$NFull), .by = Class)

  f_ <- function(.d) {
    w_ <- .d |>
      dplyr::summarise(MeanCounter = mean(.data$NCounter), .by = c(Spec, SpecOrder)) |>
      dplyr::arrange(.data$SpecOrder) |>
      dplyr::select(-SpecOrder) |>
      tidyr::pivot_wider(names_from = Spec, values_from = MeanCounter)
    dplyr::bind_cols(tibble::tibble(Docs = nrow(cnt_), Unadjusted = mean(cnt_$NFull)), w_)
  }

  ent_bind_all(
    .tab = dplyr::left_join(base_, wide_, by = dplyr::join_by(Class)), .fun = f_, .src = src_
  )
}


#' What each tail specification yields, by contract type
#'
#' @param .sweep Tibble from ent_sweep_window() over the tail specifications.
#' @return Tibble: one row per class per tail specification, plus ALL rows.
ent_table_tail <- function(.sweep) {
  if (FALSE) .sweep <- tab_sweep_tail

  f_ <- function(.d) {
    .d |>
      dplyr::summarise(
        MedTailWidth  = stats::median(.data$TailWidth, na.rm = TRUE),
        PctNoTail     = mean(!.data$HasTail),
        MeanTailNew   = mean(.data$NTailNew),
        PctAnyTailNew = mean(.data$NTailNew > 0L),
        MeanBothEnds  = mean(.data$NBothEnds),
        .by = c(Spec, SpecOrder)
      ) |>
      dplyr::arrange(.data$SpecOrder) |>
      dplyr::select(-SpecOrder)
  }

  per_ <- .sweep |>
    dplyr::summarise(
      MedTailWidth  = stats::median(.data$TailWidth, na.rm = TRUE),
      PctNoTail     = mean(!.data$HasTail),
      MeanTailNew   = mean(.data$NTailNew),
      PctAnyTailNew = mean(.data$NTailNew > 0L),
      MeanBothEnds  = mean(.data$NBothEnds),
      .by = c(Class, Spec, SpecOrder)
    ) |>
    dplyr::arrange(.data$SpecOrder) |>
    dplyr::select(-SpecOrder)

  ent_bind_all(.tab = per_, .fun = f_, .src = .sweep)
}


#' What each window yields, by decile of document length
#'
#' @param .sweep Tibble from ent_sweep_window().
#' @return Tibble: one row per decile, one column per window specification.
ent_table_length <- function(.sweep) {
  if (FALSE) .sweep <- tab_sweep_window

  base_ <- dplyr::mutate(.sweep, Decile = dplyr::ntile(.data$DocLen, 10L), .by = Spec)

  wide_ <- base_ |>
    dplyr::summarise(MeanCounter = mean(.data$NCounter), .by = c(Decile, Spec, SpecOrder)) |>
    dplyr::arrange(.data$SpecOrder) |>
    dplyr::select(-SpecOrder) |>
    tidyr::pivot_wider(names_from = Spec, values_from = MeanCounter)

  base_ |>
    dplyr::filter(.data$SpecOrder == 1L) |>
    dplyr::summarise(
      Docs       = dplyr::n(),
      MedLen     = stats::median(.data$DocLen),
      Unadjusted = mean(.data$NFull),
      .by = Decile
    ) |>
    dplyr::left_join(wide_, by = dplyr::join_by(Decile)) |>
    dplyr::arrange(.data$Decile)
}


# 7. Reading ---------------------------------------------------------------------------------------------------------

#' Every organisation a few documents name, in order, with its assigned role
#'
#' @param .roles Tibble from ent_window().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .docs Character vector of DocIDs, or NULL to draw at random.
#' @param .n Integer. Documents drawn.
#' @param .max Integer. Rows shown per document, so one table-heavy filing cannot fill the page.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: the drawn documents' rows, ordered by document and position.
ent_read_entities <- function(.roles, .keys, .docs = NULL, .n = 6L, .max = 12L, .seed = 42L) {
  if (FALSE) {
    .roles <- tab_roles
    .keys  <- tab_keys
    .docs  <- NULL
    .n     <- 6L
    .max   <- 12L
    .seed  <- 42L
  }

  if (nrow(.roles) == 0L) return(tibble::tibble())

  pool_ <- if (is.null(.docs)) unique(.roles$DocID) else intersect(.docs, .roles$DocID)
  if (length(pool_) == 0L) return(tibble::tibble())

  docs_ <- withr::with_seed(.seed, sample(pool_, size = min(.n, length(pool_))))

  .roles |>
    dplyr::filter(.data$DocID %in% docs_) |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::slice_head(n = .max, by = DocID) |>
    dplyr::left_join(dplyr::select(.keys, DocID, CompanyName, Class), by = dplyr::join_by(DocID))
}


# 8. Report ----------------------------------------------------------------------------------------------------------

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


#' How many names sit within reach of the party
#' @param .dist Tibble from ent_dist_profile().
#' @param .gap Tibble from ent_gap_profile().
#' @return Invisibly .dist.
ent_report_dist <- function(.dist, .gap) {
  if (FALSE) {
    .dist <- tab_dist
    .gap  <- tab_gap
  }

  cli::cli_h2("Distances inside the party's neighbourhood")
  .dist |>
    dplyr::mutate(PctOfNames = tbl_pct(.data$PctOfNames)) |>
    tbl_say(.title = "Names within each candidate half-width")

  tbl_say(.tab = .gap, .title = "Gap between adjacent names, among those near the party")

  cli::cli_alert_info(
    "The first table has NO ELBOW: the count rises steadily with the width and never flattens, so \\
     the data does not pick a cutoff. The second is what the released width is argued from instead \\
     -- a window narrower than the gap between adjacent names cuts a list of parties in half, and \\
     one clearing the P90 of that gap does not, in nine documents out of ten."
  )
  invisible(.dist)
}


#' The party match, and how far the fallback can be trusted
#' @param .tab Tibble from ent_locate_party().
#' @return Invisibly the summary table.
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

  n_fam1_ <- sum(.tab$MatchKind == "family" & .tab$MatchTok == 1L, na.rm = TRUE)
  out_ <- tibble::tibble(
    Item = c("Matched documents where the party is also the FIRST name",
             "Documents falling back to the first name",
             "Documents with no organisation at all",
             "Matched parties found deep in the document",
             "Family matches resting on a SINGLE shared token",
             "Single-token family matches that are ALSO deep",
             "Non-exact matches with an EXACT match further on",
             "...of those, overridden by the exact-within rule"),
    N    = c(sum(.tab$MatchIsFirst, na.rm = TRUE),
             sum(.tab$Status == "first"),
             sum(.tab$Status == "no entity"),
             sum(.tab$IsDeep & .tab$Status == "matched", na.rm = TRUE),
             n_fam1_,
             sum(.tab$MatchKind == "family" & .tab$MatchTok == 1L & .tab$IsDeep, na.rm = TRUE),
             sum(.tab$HasLateExact, na.rm = TRUE),
             sum(.tab$UseExact, na.rm = TRUE)),
    Of   = c(sum(!is.na(.tab$MatchIsFirst)), nrow(.tab), nrow(.tab),
             sum(.tab$Status == "matched"), sum(.tab$Status == "matched"), max(n_fam1_, 1L),
             sum(.tab$Status == "matched"), max(sum(.tab$HasLateExact, na.rm = TRUE), 1L))
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$N / .data$Of))

  tbl_say(.tab = out_, .title = "The fallback, and the two failures that compound")
  cli::cli_alert_info(
    "The first row IS the fallback's accuracy: the share of documents in which taking the first name \\
     would have given the right answer, measurable only where the match succeeded. The last row is \\
     what the deep guard WOULD refuse and does not: reading those documents found most of them to be \\
     the right corporate family, and refusing sends them to a fallback that is wrong more often."
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
  tbl_say(.tab = .tab, .title = "Mean counterparties per document, against the unadjusted count")

  cli::cli_alert_info(
    "UNADJUSTED is every distinct organisation the contract names anywhere -- the count this rule \\
     replaces, and the only honest thing to read the window columns against. The published figure it \\
     supersedes is a mean of 2.94 partners per contract, which counted MENTIONS across two engines. \\
     The GAP columns are reported rather than chosen: they track the fixed widths without beating \\
     them."
  )
  invisible(.tab)
}


#' What each tail specification yields
#' @param .tab Tibble from ent_table_tail().
#' @return Invisibly .tab.
ent_report_tail <- function(.tab) {
  if (FALSE) .tab <- tab_tail

  cli::cli_h2("The tail, by contract type")
  .tab |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "One block per tail width, the window held at the released value")

  cli::cli_alert_info(
    "The flat block is the control, and it is what settles relative against absolute: it is WIDER on \\
     the median than the 10% block and finds fewer names, because a relative tail puts its width on \\
     the long agreements where the signature blocks with many parties actually are."
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
    "UNADJUSTED rising steeply while the fixed columns stay flat means the extra names in a long \\
     contract are body prose the window is right to exclude, and the width does not need to scale."
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
    "MeanCounter barely moves and should not: the match decides WHICH entity anchors the window, not \\
     how many names fall inside it. Read PctMatched against PctDeepFamily -- an arm that raises the \\
     first while raising the second is buying documents by anchoring on a mention that is not a \\
     preamble."
  )
  invisible(.tab)
}


#' Document frequency of names in the window
#' @param .tab Tibble from ent_window_terms().
#' @param .n Integer. Rows shown.
#' @return Invisibly .tab.
ent_report_terms <- function(.tab, .n = 25L) {
  if (FALSE) {
    .tab <- tab_terms
    .n   <- 25L
  }

  cli::cli_h2("The most frequent names inside the window")
  .tab |>
    dplyr::mutate(
      PctDocs  = tbl_pct(.data$PctDocs),
      PctFiler = tbl_pct(.data$PctFiler),
      PctFrag  = tbl_pct(.data$PctFrag)
    ) |>
    dplyr::select(Key, NTok, PctFrag, Docs, PctDocs, PctFiler, Example) |>
    tbl_say(.title = "Reported, not applied", .n = .n)

  one_ <- mean(.tab$NTok == 1L)
  cli::cli_alert_info(
    "PctFiler is why no frequency threshold is set: a name in many documents AND matching the filer \\
     in most of them is a large registrant, one in many and matching in none is boilerplate, and a \\
     cut cannot tell them apart. {tbl_pct(one_)} of these names are a SINGLE TOKEN; PctFrag is the \\
     share of those occurrences where a LONGER name in the same document contains the token, which \\
     is the fragment rule swept above."
  )
  invisible(.tab)
}


#' Every organisation of a few documents, with its role and the window bounds
#' @param .tab Tibble from ent_read_entities().
#' @param .counts Tibble from ent_counts().
#' @param .title Character. Block title.
#' @return Invisibly .tab.
ent_report_entities <- function(.tab, .counts, .title = "Whole documents, read in order") {
  if (FALSE) {
    .tab    <- tab_read_ent
    .counts <- tab_counts
    .title  <- "Whole documents, read in order"
  }

  cli::cli_h2(.title)
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
      dplyr::select(Start, Role, NOcc, NTok, IsFragment, DistToParty, RecursInTail, Name) |>
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
#' @param .gap Tibble from ent_gap_profile().
#' @param .party Tibble from ent_locate_party().
#' @param .where Tibble from ent_table_where().
#' @param .win_all Tibble from ent_table_window() over all documents.
#' @param .win_matched Tibble from ent_table_window() over matched documents.
#' @param .win_first Tibble from ent_table_window() over fallback documents.
#' @param .tail Tibble from ent_table_tail().
#' @param .length Tibble from ent_table_length().
#' @param .sweep Tibble from ent_sweep_match().
#' @param .terms Tibble from ent_window_terms().
#' @param .n_terms Integer. Rows of the frequency table shown.
#' @return Invisibly NULL.
ent_report_all_org <- function(.keys, .rule, .boundary, .dist, .gap, .party, .where, .win_all,
                               .win_matched, .win_first, .tail, .length, .sweep, .terms,
                               .n_terms = 25L) {
  if (FALSE) {
    .keys        <- tab_keys
    .rule        <- .lP$Params$Rule
    .boundary    <- tab_boundary
    .dist        <- tab_dist
    .gap         <- tab_gap
    .party       <- tab_party
    .where       <- tab_where
    .win_all     <- tab_win_all
    .win_matched <- tab_win_matched
    .win_first   <- tab_win_first
    .tail        <- tab_tail
    .length      <- tab_length
    .sweep       <- tab_sweep_match
    .terms       <- tab_terms
    .n_terms     <- 25L
  }

  ent_report_keys(.tab = .keys, .rule = .rule)
  ent_report_boundary(
    .tab  = .boundary,
    .note = paste("Nothing in the party rule depends on a tight boundary. The geography rule",
                  "assigns a place to the NEAREST organisation, so its scale is the neighbour gap",
                  "reported below, and the overshoot has to be read against that.")
  )
  ent_report_dist(.dist = .dist, .gap = .gap)
  ent_report_match(.tab = .party)
  ent_report_where(.tab = .where)
  ent_report_window_table(.tab = .win_all,     .title = "All documents")
  ent_report_window_table(.tab = .win_matched, .title = "Matched parties only")
  ent_report_window_table(.tab = .win_first,   .title = "Fallback parties only")
  ent_report_tail(.tab = .tail)
  ent_report_length(.tab = .length)
  ent_report_sweep_match(.tab = .sweep)
  ent_report_terms(.tab = .terms, .n = .n_terms)
  invisible(NULL)
}


# 9. Figures ---------------------------------------------------------------------------------------------------------

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


#' The rule against the count it replaces, by contract type
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
