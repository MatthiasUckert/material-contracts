# 04B1-Rules-ORG: the contracting party and its counterparties --------------------------------------------------------
#
# WHAT THIS FILE DOES
# Decides, for every contract in the labelled sample, which organisation is the contracting party and
# which of the others are its counterparties. Function prefix is `ent_`.
#
# Shared tooling lives in _Commons/_Entity.R: the name reduction, the anchor build, the store loader,
# the span reader, the offset check and the table shapes. Everything here is a RULE, and rules belong
# to one entity.
#
# THE RULE IS THREE STEPS AND TWO NUMBERS
#   1. GROUP. Every organisation name in one contract is reduced to a key, and keys naming the same
#      party are merged. Two keys are one party when one is the WORD-PREFIX of the other, or when one
#      is a single word taken UNAMBIGUOUSLY from the other.
#   2. MATCH. A party whose key agrees with the registrant's EDGAR name -- word for word until one of
#      the two runs out -- is the contracting party. Where several agree the earliest wins. Where
#      none does, the first party named is taken and flagged.
#   3. PLACE. Every other party within 2,000 characters of the contracting party is a counterparty;
#      one in the final tenth of the document is a signatory, counted separately; anything else is
#      not counted.
# The numbers are 2,000 and the final tenth. Nothing else in this file enters an answer.
#
# ONE ENGINE, AND THAT IS THE BINDING CONSTRAINT
# 04C ran two families over the corpus and neither is spaCy. For ORG that leaves lexnlp alone, so the
# whole rule runs in R over a table that fits in memory and DuckDB is read once.
#
# 04A's own measurements are why spaCy is not the loss it looks like. On the labelled sample the
# transformer alone emitted 915,716 ORG spans against lexnlp's 81,467 -- eleven times as many. But
# volume is not the argument: the positional contrast is. lexnlp's ORG concentrates at the document
# ends with a contrast of 4.36, the strongest of any producer and entity 04A measured, while spaCy's
# is 0.97, uniform to within rounding. spaCy finds every organisation named anywhere; lexnlp finds
# the ones named where parties are named.
#
# WORD-PREFIX COMPARISON REPLACED FOUR ARMS AND A CORPUS-WIDE COUNT
# The comparison used to have four outcomes -- exact, forward containment, corporate family, reverse
# containment -- with two length floors and a document-frequency gate. All four say the same thing at
# different strengths, and comparing WHOLE WORDS from the start says it once:
#
#   BOEING                    vs  BOEING                    agree, both run out      -> one party
#   BOEING                    vs  BOEING CAPITAL            agree, one runs out      -> one party
#   PENNSYLVANIA POWER LIGHT  vs  PENNSYLVANIA POWER        agree, one runs out      -> one party
#   CHENIERE ENERGY           vs  CHENIERE CREOLE TRAIL     differ at word two       -> separate
#   NATIONAL BANK             vs  NATIONAL SEMICONDUCTOR    differ at word two       -> separate
#
# TWO THINGS FOLLOW, AND THE SECOND IS THE REASON. The length floors go, because they existed for
# SUBSTRING matching -- CA sits inside CATERPILLAR and no floor below five could stop it -- and whole
# words cannot make that mistake. And the document-frequency gate goes: it admitted a SINGLE shared
# leading word when that word opened a company name in at most 0.5% of documents, which required
# counting across the whole corpus. That count was the only quantity in this pipeline whose value
# depended on how the corpus was PARTITIONED, and removing it is what makes this chain chunkable.
#
# WHAT IT COST, MEASURED. The old rule matched 77.1% of documents and this one matches 74.9% -- about
# 97 contracts, which the one-token family arm used to catch and which now fall to the fallback.
# 91.9% of the matches that remain are exact agreements, so the four arms were one arm and a rounding
# error.
#
# GROUPING RUNS BEFORE MATCHING, AND THAT ORDERING IS LOAD-BEARING
# lexnlp splits one name across mentions -- "Florida East Coast Railway" at one offset and the bare
# word "Railway" at another -- and glues neighbouring text onto others: "Grant Date, Hubbell" where
# the heading ran into the party name. Both are one company arriving as two entities.
#
# Grouping first means neither reaches the EDGAR comparison alone. It also means "the first party
# named" in the fallback is the first UNIQUE PARTY rather than the first raw span.
#
# AND IT MADE A PARAMETER DISAPPEAR. The rule used to carry a 500-character reach that preferred a
# clean exact match sitting just behind a contaminated one -- the Hubbell case. Once the contaminated
# span and the clean one are ONE PARTY, there is nothing left for that reach to arbitrate: swept at 0
# and at 500 it produced identical output on every column of every row. It is gone.
#
# THE NAME COMES FROM THE MEMBER THAT MATCHED. A group holds several spellings and the release needs
# one:
#   THE MEMBER THAT MATCHED EDGAR, because for the registrant we know exactly which one did.
#   OTHERWISE THE MEMBER MENTIONED MOST OFTEN, earliest position breaking ties, because a
#   contaminated span is a one-off while the real name recurs.
# Taking the LONGEST would hand "GRANT DATE HUBBELL" the name over "HUBBELL"; taking the EARLIEST
# would do the same, since the glued heading is usually the first mention.
#
# THE PARTY IS THE ANCHOR, NOT A REGION
# An earlier design read a fixed head and asked whether the registrant was inside it. Sweeping that
# head across twenty settings showed the located share rising from 70.6% to 75.1% and the LATE share
# falling from 6.5% to 2.0%, the two summing to 77.1% in every cell. That is an identity: the match
# runs on names and cannot depend on a region, so a wider head bought no party and only relabelled
# one already found. There is no head. The registrant is matched ANYWHERE and the window is centred
# on where it was found.
#
# ONE WINDOW, AND THE ALTERNATIVES ARE SETTLED RATHER THAN OFFERED
# A gap window -- growing outward through every name whose distance to its neighbour is under a
# threshold -- was measured against the fixed one and tracks it: gap 500 against +/- 1,000 agrees to
# within a few hundredths on every contract type. Two rules that agree leave nothing to choose
# between them but simplicity, so the fixed window is the rule and the gap arm is not in this file.
#
# THE TAIL IS RELATIVE AND THAT IS MEASURED. A flat 3,000-character tail is WIDER on the median than
# a 10% tail -- 3,000 against 2,320 on credit agreements -- and finds fewer names, 2.97 against 5.04,
# because a relative tail puts its width on the long agreements where the signature blocks with many
# parties actually are.
#
# ONE FILE, AND EVERY DOCUMENT-LEVEL NUMBER IS A COUNT OVER IT
# The release is one row per contract per party. A contract-level count -- how many counterparties,
# how many signatories, whether the registrant was matched -- is a group-by over that file rather
# than a second file carrying its own copy of the answer. Storing a derived quantity beside its
# source is how a published dataset comes to contradict itself.
#
# A CONTRACT IN WHICH NOTHING WAS FOUND STILL GETS A ROW. lexnlp proposed no organisation at all in
# 116 of 4,398 contracts. Without a sentinel those documents have no rows, so every mean computed
# from the file silently divides by 4,282 -- about 2.6% too high, with nothing to notice. The
# extractors solve this the same way: _io.py emits one null-span row per document that yielded
# nothing, for exactly this reason.
#
# EVERY TABLE CARRIES THE NAIVE COUNT
# NAIVE is every unique party the contract names anywhere, less one for the registrant. It is what
# this rule replaces and the only honest thing to read the counterparty count against. It is NOT a
# released column: it is one subtraction from a count over the release, and computing it where it is
# used rather than storing it is what stops the two disagreeing.
#
# TWO COLUMN NAMES ARE THIS PROJECT'S AND NOT LEXNLP'S, and the translation happens once, in
# ent_load_entity(). NameCore is lexnlp's Name -- renamed because Name means the resolved company for
# ORG and the resolved place for GPE. LegalForm is lexnlp's TypeAbbr -- renamed because it holds the
# string "NA" for a National Association, which R prints identically to a missing value.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_text <- .lP$Input$Text
  .dir_store <- .lP$Input$Store
}


# 1. Vocabulary ------------------------------------------------------------------------------------------------------
# PartyRole is NOT registered here. It orders the role column of the released file, which 04B2 reads
# without sourcing this file, so it lives in _Commons/_Entity.R where both documents can reach it.
# What follows is private to this document.

plot_register_levels(
  .key    = "PartyStatus",
  .levels = c("matched", "first", "no entity"),
  .short  = c("matched", "first", "none")
)

plot_register_levels(
  .key    = "MergeKind",
  .levels = c("alone", "prefix", "fragment"),
  .short  = c("alone", "prefix", "frag")
)


# 2. The rule --------------------------------------------------------------------------------------------------------
# WINDOW-FREE, all of it. Grouping and matching do not depend on where in the document anything sits,
# so they are computed once and every window is cut over the same result.

#' Build one rule specification
#'
#' TWO PARAMETERS, DOWN FROM TWELVE. The ten that went were two length floors, a shared-token count, a
#' shared-token share, a document-frequency gate, a depth threshold and its guard, a fragment length
#' floor, a fragment share, and the exact-match reach. Word-prefix comparison answers what the first
#' nine were approximating without a threshold, and grouping-before-matching made the tenth vacuous.
#'
#' @param .key Character, "core" or "span". Which reduction identifies an entity. CORE is lexnlp's
#'   own resolved name, which is what it emits when it recognises a company rather than a string.
#' @param .merge_fragments Logical. Merge a single-word key into the one longer key that contains it
#'   as a whole word. TRUE. Set FALSE to sweep the grouping itself: with prefix merging alone,
#'   "Railway" stays a separate party from "Florida East Coast Railway".
#'
#'   THE MERGE IS REFUSED WHERE IT IS AMBIGUOUS. A document naming ACME ENERGY and BETA ENERGY does
#'   not let the bare word ENERGY join either, because it belongs to both equally and choosing would
#'   be a guess.
#' @return A named list carrying the rule specification.
ent_rule <- function(.key = "core", .merge_fragments = TRUE) {
  if (FALSE) {
    .key             <- "core"
    .merge_fragments <- TRUE
  }

  if (!.key %in% c("core", "span")) cli::cli_abort("{.arg .key} must be \"core\" or \"span\".")

  list(
    Key            = .key,
    MergeFragments = isTRUE(.merge_fragments),
    Label          = paste0(.key, if (isTRUE(.merge_fragments)) " / prefix+frag" else " / prefix")
  )
}


#' Override fields of a rule, coercing the logical one
#'
#' @param .rule List from ent_rule().
#' @param .over Named list of field values to replace.
#' @return The rule with .over applied.
ent_rule_set <- function(.rule, .over) {
  if (FALSE) {
    .rule <- ent_rule()
    .over <- list(MergeFragments = FALSE)
  }

  out_ <- .rule
  for (nm_ in names(.over)) out_[[nm_]] <- .over[[nm_]]
  out_$MergeFragments <- isTRUE(out_$MergeFragments)
  out_
}


#' Which reduction identifies an entity, under this rule
#'
#' TWO FUNCTIONS PICK THE KEY AND THEY MUST PICK THE SAME ONE. ent_entities() collapses mentions into
#' parties and ent_mentions() maps every mention back to the party it belongs to; a rule chosen twice
#' is a rule that can be chosen differently, and the mention index would then reference parties that
#' do not exist.
#'
#' @param .spans Tibble from the loader with SpanKey and CoreKey added.
#' @param .rule List from ent_rule().
#' @return .spans with Key added, rows carrying no usable key dropped.
ent_key_of <- function(.spans, .rule) {
  if (FALSE) {
    .spans <- tab_spans
    .rule  <- .lP$Params$Rule
  }

  .spans |>
    dplyr::mutate(Key = if (identical(.rule$Key, "core")) .data$CoreKey else .data$SpanKey) |>
    dplyr::filter(!is.na(.data$Key), nzchar(.data$Key))
}


#' Collapse mentions into entities, one row per document per name
#'
#' lexnlp returns "Icosavax, Inc.", "Icosavax Inc" and "Icosavax, Inc" as three spans and one name.
#'
#' THE OFFSETS COME FROM ONE ROW, SELECTED WHOLE, by slice_min(). NOcc and MaxStart describe the
#' GROUP and are aggregated separately -- they are not properties of any single mention. MaxStart is
#' what makes corroboration possible once the tail is clamped past the window: an entity introduced
#' beside the party and mentioned AGAIN at the end was introduced and signed.
#'
#' @param .spans Tibble from the loader with SpanKey and CoreKey added.
#' @param .rule List from ent_rule(). Supplies only the choice of key.
#' @return Tibble: one row per document per key, with the position and the group facts.
ent_entities <- function(.spans, .rule) {
  if (FALSE) {
    .spans <- tab_spans
    .rule  <- .lP$Params$Rule
  }

  keyed_ <- ent_key_of(.spans = .spans, .rule = .rule)

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


#' Which keys in one document name the same party
#'
#' TWO JOINS, NOT A PAIRWISE LOOP, and the difference is what lets this run at corpus scale. Two keys
#' can only be word-prefix related if they share their FIRST word, so prefix merging is a self-join
#' grouped on that word rather than a comparison of every key against every other. Fragment merging
#' is a join of single-word keys onto the unnested words of longer keys. A document naming a hundred
#' organisations would need 4,950 comparisons the other way and needs none here.
#'
#' PREFIX. Two keys agree word for word until one of them runs out: BOEING and BOEING CAPITAL,
#' PENNSYLVANIA POWER LIGHT and PENNSYLVANIA POWER. ent_shared_tokens() counts the leading words two
#' keys share, so the test is that the count reaches the shorter key's length.
#'
#' FRAGMENT. A single-word key appearing as a whole word of exactly one longer key in the same
#' document: RAILWAY inside FLORIDA EAST COAST RAILWAY, HUBBELL inside GRANT DATE HUBBELL. This is
#' lexnlp splitting one name across mentions, and it is the arm the ambiguity guard protects --
#' ENERGY sitting inside both ACME ENERGY and BETA ENERGY joins neither.
#'
#' @param .ent Tibble from ent_entities().
#' @param .rule List from ent_rule(). Supplies whether fragments merge.
#' @return .ent with PartyId and MergeKind added, one PartyId per party per document.
ent_group_parties <- function(.ent, .rule) {
  if (FALSE) {
    .ent  <- tab_ent
    .rule <- .lP$Params$Rule
  }

  base_ <- .ent |>
    dplyr::mutate(FirstTok = stringi::stri_extract_first_regex(.data$Key, "^[^ ]+"))

  # PREFIX PAIRS. Self-join on the first word, then keep the pairs whose shared leading run reaches
  # the shorter key. The inequality on Key drops the self-pair and each pair's mirror image.
  side_ <- dplyr::select(base_, "DocID", "FirstTok", "Key", "NTok")

  pref_ <- side_ |>
    dplyr::inner_join(side_, by = dplyr::join_by(DocID, FirstTok),
                      suffix = c("A", "B"), relationship = "many-to-many") |>
    dplyr::filter(.data$KeyA < .data$KeyB) |>
    dplyr::mutate(Shared = ent_shared_tokens(.a = .data$KeyA, .b = .data$KeyB)) |>
    dplyr::filter(.data$Shared >= pmin(.data$NTokA, .data$NTokB)) |>
    dplyr::select("DocID", "KeyA", "KeyB") |>
    dplyr::mutate(Kind = "prefix")

  frag_ <- tibble::tibble(DocID = character(0), KeyA = character(0), KeyB = character(0),
                          Kind = character(0))

  if (isTRUE(.rule$MergeFragments)) {
    toks_ <- base_ |>
      dplyr::filter(.data$NTok > 1L) |>
      dplyr::mutate(Tok = stringi::stri_split_fixed(.data$Key, " ")) |>
      tidyr::unnest_longer(col = Tok) |>
      dplyr::distinct(.data$DocID, .data$Key, .data$Tok)

    frag_ <- base_ |>
      dplyr::filter(.data$NTok == 1L) |>
      dplyr::select("DocID", Short = "Key") |>
      dplyr::inner_join(toks_, by = dplyr::join_by(DocID, Short == Tok),
                        relationship = "many-to-many") |>
      # AMBIGUOUS MERGES ARE REFUSED, not resolved. A word inside two longer names belongs to both
      # equally and picking one would be a guess dressed as a rule.
      dplyr::filter(dplyr::n() == 1L, .by = c(DocID, Short)) |>
      dplyr::transmute(
        .data$DocID,
        KeyA = pmin(.data$Short, .data$Key),
        KeyB = pmax(.data$Short, .data$Key),
        Kind = "fragment"
      )
  }

  pairs_ <- dplyr::bind_rows(pref_, frag_) |>
    dplyr::distinct(.data$DocID, .data$KeyA, .data$KeyB, .keep_all = TRUE)

  lab_ <- ent_merge_labels(.ent = base_, .pairs = pairs_)

  kind_ <- pairs_ |>
    tidyr::pivot_longer(cols = c("KeyA", "KeyB"), values_to = "Key") |>
    dplyr::summarise(MergeKind = dplyr::first(.data$Kind), .by = c(DocID, Key))

  base_ |>
    dplyr::left_join(lab_,  by = dplyr::join_by(DocID, Key)) |>
    dplyr::left_join(kind_, by = dplyr::join_by(DocID, Key)) |>
    dplyr::mutate(
      PartyId   = dplyr::coalesce(.data$PartyId, .data$Key),
      MergeKind = dplyr::coalesce(.data$MergeKind, "alone")
    ) |>
    dplyr::select(-FirstTok)
}


#' Give every key in a merge graph the same label as everything it reaches
#'
#' LABEL PROPAGATION RATHER THAN A GRAPH PACKAGE. Each key starts labelled with itself and repeatedly
#' takes the smallest label among itself and its neighbours; the process stops when nothing moves.
#' Merge groups inside one contract are two or three keys, so this converges in a handful of passes
#' and adds no dependency.
#'
#' THE ITERATION IS BOUNDED AND THE BOUND ABORTS. A silent stop at the cap would return labels that
#' are nearly right, which is the shape of defect that renders clean and is discovered in a table six
#' weeks later.
#'
#' @param .ent Tibble carrying DocID and Key.
#' @param .pairs Tibble carrying DocID, KeyA and KeyB.
#' @param .max Integer. Passes allowed before the propagation is declared non-convergent.
#' @return Tibble: DocID, Key, PartyId.
ent_merge_labels <- function(.ent, .pairs, .max = 20L) {
  if (FALSE) {
    .ent   <- tab_ent
    .pairs <- tab_pairs
    .max   <- 20L
  }

  lab_ <- dplyr::distinct(.ent, .data$DocID, .data$Key) |>
    dplyr::mutate(PartyId = .data$Key)

  if (nrow(.pairs) == 0L) return(lab_)

  # Both directions, so a label travels either way along an edge.
  edge_ <- dplyr::bind_rows(
    dplyr::select(.pairs, "DocID", Key = "KeyA", Other = "KeyB"),
    dplyr::select(.pairs, "DocID", Key = "KeyB", Other = "KeyA")
  )

  for (i_ in seq_len(.max)) {
    prev_ <- lab_$PartyId

    from_ <- edge_ |>
      dplyr::left_join(dplyr::select(lab_, "DocID", Other = "Key", OtherId = "PartyId"),
                       by = dplyr::join_by(DocID, Other)) |>
      dplyr::summarise(NeighId = min(.data$OtherId, na.rm = TRUE), .by = c(DocID, Key))

    lab_ <- lab_ |>
      dplyr::left_join(from_, by = dplyr::join_by(DocID, Key)) |>
      dplyr::mutate(PartyId = pmin(.data$PartyId, dplyr::coalesce(.data$NeighId, .data$PartyId))) |>
      dplyr::select(-NeighId)

    if (identical(prev_, lab_$PartyId)) return(lab_)
  }

  cli::cli_abort(c(
    "Party labels did not settle in {(.max)} {cli::qty(.max)}pass{?es}.",
    "i" = "A merge group longer than the cap means the pair list is chaining names that are not
           one party."
  ))
}


#' Which parties in each document agree with the registrant's EDGAR name
#'
#' THE COMPARISON IS WORD-PREFIX, applied between each party and the EDGAR name: the two agree word
#' for word until one of them runs out. That is one test where there used to be four, and it needs no
#' length floor -- the floors existed because substring matching put CA inside CATERPILLAR, and whole
#' words cannot.
#'
#' THE MATCH IS TESTED PER KEY AND ANSWERED PER PARTY. A group holds several spellings and any one of
#' them may be the one EDGAR wrote, so every member is compared and the party matches when any member
#' does. The matching member's name is then what the party is CALLED -- see ent_party_names().
#'
#' EXACT IS A COLUMN, NOT AN ARM. Both names running out together is the strongest form of the same
#' agreement, and it is carried because it says how much of the match rate rests on containment
#' rather than on equality. It was 91.9% on the labelled sample.
#'
#' @param .ent Tibble from ent_group_parties().
#' @param .keys Tibble from ent_anchor_keys().
#' @return .ent with AnchorKey, IsMatch and IsExact added.
ent_match_party <- function(.ent, .keys) {
  if (FALSE) {
    .ent  <- tab_grouped
    .keys <- tab_keys
  }

  .ent |>
    dplyr::left_join(
      dplyr::select(.keys, DocID, AnchorKey, AnchorTok),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::mutate(
      Shared  = ent_shared_tokens(.a = .data$Key, .b = .data$AnchorKey),
      IsMatch = !is.na(.data$AnchorKey) &
                .data$Shared >= pmin(.data$NTok, .data$AnchorTok),
      IsExact = .data$IsMatch & .data$NTok == .data$AnchorTok
    )
}


#' One name per party, and the evidence for it
#'
#' A GROUP HOLDS SEVERAL SPELLINGS AND THE RELEASE NEEDS ONE. lexnlp glues neighbouring text onto a
#' mention -- "Grant Date, Hubbell" where a heading ran into the party name -- so the group can carry
#' a contaminated variant beside the clean one.
#'
#' THE MEMBER THAT MATCHED EDGAR NAMES THE PARTY, because for the registrant we know with certainty
#' which spelling EDGAR wrote. Where no member matched there is nothing to match against, so the
#' party takes the name of its MOST MENTIONED member, earliest position breaking ties: a contaminated
#' span is a one-off while the real name recurs.
#'
#' NEITHER LONGEST NOR EARLIEST. Both would hand "GRANT DATE HUBBELL" the name over "HUBBELL" -- the
#' first because it has more words, the second because the glued heading is usually the first
#' mention.
#'
#' @param .ent Tibble from ent_match_party().
#' @return Tibble: one row per document per party, with the chosen name and the group's facts.
ent_party_names <- function(.ent) {
  if (FALSE) .ent <- tab_matched

  .ent |>
    dplyr::arrange(
      .data$DocID, .data$PartyId,
      dplyr::desc(.data$IsExact), dplyr::desc(.data$IsMatch),
      dplyr::desc(.data$NOcc), .data$Start
    ) |>
    dplyr::summarise(
      PartyKey  = dplyr::first(.data$Key),
      PartyName = dplyr::first(.data$Name),
      PartySpan = dplyr::first(.data$Span),
      NameFrom  = dplyr::if_else(dplyr::first(.data$IsMatch), "matched member", "most mentioned"),
      IsMatch   = any(.data$IsMatch),
      IsExact   = any(.data$IsExact),
      NKeys     = dplyr::n(),
      NOcc      = sum(.data$NOcc),
      # NEITHER BRANCH OF if_else IS LAZY, so sorting the merged kinds inside one would run on an
      # empty vector for every party nothing merged into. Taking the minimum of the non-alone kinds
      # and repairing the singletons afterwards is the same answer without the empty case.
      MergeKind = suppressWarnings(min(.data$MergeKind[.data$MergeKind != "alone"])),
      Start     = min(.data$Start),
      Stop      = .data$Stop[which.min(.data$Start)],
      MaxStart  = max(.data$MaxStart),
      DocLen    = dplyr::first(.data$DocLen),
      HasForm   = any(.data$HasForm),
      .by = c(DocID, PartyId)
    ) |>
    dplyr::mutate(
      MergeKind = dplyr::if_else(.data$NKeys > 1L, .data$MergeKind, "alone"),
      MergeKind = dplyr::coalesce(.data$MergeKind, "alone")
    ) |>
    dplyr::arrange(.data$DocID, .data$Start)
}


#' One row per document: which party is the contracting party
#'
#' RUNS FROM THE KEY SIDE, not from the parties. A document where lexnlp proposed nothing resembling
#' the registrant's name carries no party row, so a join in the other direction would drop it and the
#' coverage figure would be computed over the documents that worked.
#'
#' THE EARLIEST MATCH WINS, AND THAT IS THE WHOLE SELECTION. The rule used to prefer an exact
#' agreement sitting within 500 characters of the first match, to rescue a contaminated first
#' mention. Grouping merges the contaminated span into the clean one before this function runs, so
#' the preference had nothing left to arbitrate: at 0 and at 500 it produced identical output on
#' every column of every row of the sweep.
#'
#' WHERE NOTHING MATCHES the first party named is taken and Status records that it was.
#'
#' @param .party Tibble from ent_party_names().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .lens Tibble from ent_doc_lens().
#' @return Tibble: one row per document, with Status, the chosen party and its offsets.
ent_locate_party <- function(.party, .keys, .lens) {
  if (FALSE) {
    .party <- tab_parties
    .keys  <- tab_keys
    .lens  <- tab_lens
  }

  best_ <- .party |>
    dplyr::filter(.data$IsMatch) |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::select(DocID, MatchId = PartyId, MatchName = PartyName, MatchSpan = PartySpan,
                  MatchStart = Start, MatchStop = Stop, MatchExact = IsExact)

  first_ <- .party |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::select(DocID, FirstId = PartyId, FirstName = PartyName, FirstSpan = PartySpan,
                  FirstStart = Start, FirstStop = Stop)

  seen_ <- .party |>
    dplyr::summarise(
      NParties  = dplyr::n(),
      NMerged   = sum(.data$NKeys > 1L),
      NKeysSeen = sum(.data$NKeys),
      .by = DocID
    )

  .keys |>
    dplyr::left_join(.lens,  by = dplyr::join_by(DocID)) |>
    dplyr::left_join(seen_,  by = dplyr::join_by(DocID)) |>
    dplyr::left_join(best_,  by = dplyr::join_by(DocID)) |>
    dplyr::left_join(first_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      dplyr::across(c(NParties, NMerged, NKeysSeen),
                    \(.x) as.integer(dplyr::coalesce(.x, 0L))),
      Status = dplyr::case_when(
        !is.na(.data$MatchId) ~ "matched",
        .data$NParties > 0L   ~ "first",
        .default              = "no entity"
      ),
      PartyId    = dplyr::if_else(.data$Status == "matched", .data$MatchId,    .data$FirstId),
      PartyName  = dplyr::if_else(.data$Status == "matched", .data$MatchName,  .data$FirstName),
      PartySpan  = dplyr::if_else(.data$Status == "matched", .data$MatchSpan,  .data$FirstSpan),
      PartyStart = dplyr::if_else(.data$Status == "matched", .data$MatchStart, .data$FirstStart),
      PartyStop  = dplyr::if_else(.data$Status == "matched", .data$MatchStop,  .data$FirstStop),
      PartyFrac  = .data$PartyStart / .data$DocLen,
      IsExact    = dplyr::coalesce(.data$MatchExact, FALSE),
      Party      = stringi::stri_trim_both(
        stringi::stri_replace_all_regex(dplyr::coalesce(.data$PartyName, ""), "\\s+", " ")
      ),
      MatchIsFirst = dplyr::if_else(
        .data$Status == "matched", .data$MatchId == .data$FirstId, NA
      )
    ) |>
    dplyr::relocate(Status, Party, IsExact, .after = DocID)
}


# 3. The window ------------------------------------------------------------------------------------------------------
# The ONLY position-dependent step, and therefore the only one the width table has to re-run.

#' Build one window specification
#'
#' ONE SHAPE, NOT TWO. A gap window -- growing outward through every name whose distance to its
#' neighbour is under a threshold -- was measured against this one and tracks it to within a few
#' hundredths on every contract type. Two rules that agree leave nothing to choose between them but
#' simplicity, so the gap arm is a settled negative result stated in the prose rather than a branch
#' in this function.
#'
#' @param .par Numeric. Half-width in characters either side of the contracting party. 2,000, which
#'   clears the P90 gap between adjacent names: a window narrower than that gap cuts a list of
#'   parties in half, and the distance distribution itself has no elbow to read a cutoff from.
#' @param .tail_share Numeric. Tail as a share of document length. RELATIVE rather than flat, and the
#'   evidence is direct: a flat 3,000-character tail is wider on the median than a 10% tail -- 3,000
#'   against 2,320 on credit agreements -- and finds fewer names, 2.97 against 5.04, because a
#'   relative tail puts its width on the long agreements where the signature blocks with many parties
#'   actually are.
#' @param .label Character or NULL. Overrides the generated label, used in the width table.
#' @return A named list carrying the window specification.
ent_window_spec <- function(.par = 2000, .tail_share = 0.10, .label = NULL) {
  if (FALSE) {
    .par        <- 2000
    .tail_share <- 0.10
    .label      <- NULL
  }

  list(
    Par       = .par,
    TailShare = .tail_share,
    Label     = if (is.null(.label)) paste0("+/- ", format(.par, big.mark = ",")) else .label
  )
}


#' Cut the window around the contracting party, and the tail behind it
#'
#' The tail is CLAMPED past the window end, so the two cannot overlap. That changes what it measures:
#' a party found there cannot also sit beside the contracting party, so the tail answers "does
#' anything outside the party's neighbourhood get named" rather than "does the signature block add
#' names". Nothing checks that those names are in a signature block rather than an exhibit list, and
#' that is accepted -- the strategy is to reduce noise, not to eliminate it.
#'
#' FOUR ROLES, NOT FIVE. A fragment of a longer name used to reach this function as a party of its
#' own and needed a role to hold it. Grouping merges it into its parent before the window is cut, so
#' there is no longer anything for a fifth role to describe.
#'
#' @param .party_tab Tibble from ent_party_names().
#' @param .party Tibble from ent_locate_party().
#' @param .spec List from ent_window_spec().
#' @return .party_tab with the window, the tail, DistToParty, IsRegistrant and Role added.
ent_window <- function(.party_tab, .party, .spec) {
  if (FALSE) {
    .party_tab <- tab_parties
    .party     <- tab_party
    .spec      <- .lP$Params$Spec
  }

  .party_tab |>
    dplyr::left_join(
      dplyr::select(.party, DocID, ChosenId = PartyId, ChosenStart = PartyStart, Status),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::mutate(
      IsRegistrant = !is.na(.data$ChosenId) & .data$PartyId == .data$ChosenId,
      DistToParty  = .data$Start - .data$ChosenStart,
      WinStart     = pmax(0, .data$ChosenStart - .spec$Par),
      WinEnd       = pmin(.data$DocLen, .data$ChosenStart + .spec$Par),
      InWindow     = !is.na(.data$ChosenStart) & .data$Start >= .data$WinStart &
                     .data$Start <= .data$WinEnd,
      TailStart    = pmax(.data$DocLen - .spec$TailShare * .data$DocLen,
                          dplyr::coalesce(.data$WinEnd, 0)),
      HasTail      = .data$TailStart < .data$DocLen,
      InTail       = .data$HasTail & .data$Start >= .data$TailStart & !.data$InWindow,
      RecursInTail = .data$HasTail & .data$MaxStart >= .data$TailStart,
      Role         = dplyr::case_when(
        .data$IsRegistrant ~ "registrant",
        .data$InWindow     ~ "counterparty",
        .data$InTail       ~ "signatory",
        .default           = "other"
      )
    )
}


#' The counts, one row per document, with the naive baseline beside them
#'
#' NOT WRITTEN. Every column here is a count over the released party file, and this function exists
#' so the report and the figures have them in memory rather than so the release carries a second copy
#' that can disagree with the first.
#'
#' NAIVE IS THE COMPARISON THIS DOCUMENT IS BUILT AROUND. It is every unique party the contract names
#' anywhere, less one for the registrant -- what a reader would count without a rule. Removed is the
#' difference, and it IS what the window did.
#'
#' NAIVE SUBTRACTS ONE WHETHER OR NOT A REGISTRANT WAS FOUND, because it is a baseline rather than a
#' second rule: a reader computing it by hand would subtract the filer without checking whether the
#' filer was identified. Clamped at zero for the documents naming one party or none.
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
      NParties  = dplyr::n_distinct(.data$PartyId),
      NCounter  = dplyr::n_distinct(.data$PartyId[.data$Role == "counterparty"]),
      NTailNew  = dplyr::n_distinct(.data$PartyId[.data$Role == "signatory"]),
      NOther    = dplyr::n_distinct(.data$PartyId[.data$Role == "other"]),
      NMergedIn = dplyr::n_distinct(.data$PartyId[.data$NKeys > 1L]),
      NKeysSeen = sum(.data$NKeys),
      NBothEnds = dplyr::n_distinct(
        .data$PartyId[.data$Role %in% c("registrant", "counterparty") & .data$RecursInTail]
      ),
      WinStart  = dplyr::first(.data$WinStart),
      WinEnd    = dplyr::first(.data$WinEnd),
      TailStart = dplyr::first(.data$TailStart),
      HasTail   = dplyr::first(.data$HasTail),
      .by = DocID
    )

  .party |>
    dplyr::select(DocID, Class, AmendType, Status, IsExact, DocLen, PartyStart, PartyFrac) |>
    dplyr::left_join(cnt_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("N"), \(.x) as.integer(dplyr::coalesce(.x, 0L))),
      HasTail    = dplyr::coalesce(.data$HasTail, FALSE),
      # AT LEAST ONE PARTY BESIDES THE REGISTRANT, anywhere in the document. The coverage figure a
      # reader asks for, and not the same population as either count below: a contract can have no
      # counterparty and still name a signatory.
      HasAnyParty = .data$NParties > 1L,
      NNaive      = as.integer(pmax(.data$NParties - 1L, 0L)),
      NRemoved    = as.integer(.data$NNaive - .data$NCounter),
      NWithTail   = as.integer(.data$NCounter + .data$NTailNew),
      WinWidth    = .data$WinEnd - .data$WinStart,
      TailWidth   = .data$DocLen - .data$TailStart
    )
}


#' Every mention of every party, with its offsets
#'
#' THE POSITIONAL INDEX, AND IT IS NOT THE RELEASE. parties_org.parquet carries one row per party at
#' the offsets of its EARLIEST mention, which is the right shape for a file about parties and the
#' wrong one for anything measuring what sits beside them. A place named next to a party's third
#' mention -- the signature block, where an address is written -- cannot reach that party through a
#' table that only knows where it was first named.
#'
#' 04B2 MEASURED THE COST. The median party has no place within reach of its first mention at any
#' window up to 800 characters, while contracts name places in abundance: 105,829 of them, of which
#' fewer than a fifth attach to anything. Signature blocks are where parties are named WITH
#' addresses, and this index is what lets a later rule reach them.
#'
#' THIN BY DESIGN. Four columns and one row per surviving span. It is an index for 04B2 and 04D
#' rather than an artifact anybody analyses, which is why the party facts stay in the release and are
#' not repeated here.
#'
#' IsFirst MARKS THE MENTION THE RELEASE CARRIES, so a consumer can reproduce the old behaviour
#' exactly by filtering to it -- which is what makes the extra mentions a strictly additive change
#' rather than a different rule.
#'
#' @param .spans Tibble from the loader, with SpanKey and CoreKey added.
#' @param .grouped Tibble from ent_group_parties(). Maps a key to its party.
#' @param .party Tibble from ent_party_names(). Maps a party to its released name.
#' @param .rule List from ent_rule().
#' @return Tibble: DocID, PartyKey, MentionStart, MentionStop, IsFirst.
ent_mentions <- function(.spans, .grouped, .party, .rule) {
  if (FALSE) {
    .spans   <- tab_spans
    .grouped <- res_base$Grouped
    .party   <- tab_parties
    .rule    <- .lP$Params$Rule
  }

  ent_key_of(.spans = .spans, .rule = .rule) |>
    dplyr::inner_join(
      dplyr::distinct(.grouped, .data$DocID, .data$Key, .data$PartyId),
      by = dplyr::join_by(DocID, Key)
    ) |>
    dplyr::inner_join(
      dplyr::distinct(.party, .data$DocID, .data$PartyId, .data$PartyKey),
      by = dplyr::join_by(DocID, PartyId)
    ) |>
    dplyr::transmute(
      .data$DocID,
      .data$PartyKey,
      MentionStart = as.integer(.data$Start),
      MentionStop  = as.integer(.data$Stop)
    ) |>
    dplyr::distinct(.data$DocID, .data$PartyKey, .data$MentionStart, .keep_all = TRUE) |>
    dplyr::arrange(.data$DocID, .data$MentionStart) |>
    dplyr::mutate(
      IsFirst = .data$MentionStart == min(.data$MentionStart),
      .by = c(DocID, PartyKey)
    )
}


#' The release: one row per contract per party
#'
#' THE ONLY FILE THIS DOCUMENT WRITES, and every contract-level quantity is a group-by over it. A
#' second file carrying its own counts would be a derived copy that can disagree with the source: a
#' reader filters the parties, recomputes, gets a different number, and nothing says which is right.
#' The counterparty count is sum(PartyRole == "counterparty"); the naive baseline is one subtraction
#' from a row count; neither is stored.
#'
#' A CONTRACT IN WHICH NOTHING WAS FOUND STILL GETS A ROW. lexnlp proposed no organisation at all in
#' 116 of 4,398 contracts. Without a sentinel those documents have no rows at all, so a mean computed
#' over the file divides by 4,282 -- about 2.6% high, with nothing to notice. The sentinel carries
#' PartyRole "none" and a null name, so every contract appears exactly once at minimum and
#' n_distinct(DocID) is the sample by construction. The extractors solve this the same way, and for
#' the same reason.
#'
#' CLASS AND AMENDTYPE TRAVEL WITH THE PARTIES so the commonest downstream cut -- counterparties by
#' contract type -- needs no join back to the label spine.
#'
#' @param .roles Tibble from ent_window().
#' @param .party Tibble from ent_locate_party(). Supplies every document, including the empty ones.
#' @return Tibble: one row per document per party, plus one sentinel per document with no party.
ent_release_parties <- function(.roles, .party) {
  if (FALSE) {
    .roles <- tab_roles
    .party <- tab_party
  }

  real_ <- .roles |>
    dplyr::left_join(dplyr::select(.party, DocID, Class, AmendType), by = dplyr::join_by(DocID)) |>
    dplyr::transmute(
      .data$DocID,
      .data$Class,
      .data$AmendType,
      PartyName  = .data$PartyName,
      PartyKey   = .data$PartyKey,
      PartyRole  = .data$Role,
      Matched    = .data$IsMatch,
      PartyStart = as.integer(.data$Start),
      PartyStop  = as.integer(.data$Stop),
      NameFrom   = .data$NameFrom,
      NVariants  = as.integer(.data$NKeys),
      NMentions  = as.integer(.data$NOcc)
    )

  none_ <- .party |>
    dplyr::filter(!.data$DocID %in% real_$DocID) |>
    dplyr::transmute(
      .data$DocID,
      .data$Class,
      .data$AmendType,
      PartyName  = NA_character_,
      PartyKey   = NA_character_,
      PartyRole  = "none",
      Matched    = FALSE,
      PartyStart = NA_integer_,
      PartyStop  = NA_integer_,
      NameFrom   = NA_character_,
      NVariants  = NA_integer_,
      NMentions  = NA_integer_
    )

  dplyr::bind_rows(real_, none_) |>
    dplyr::arrange(.data$DocID, .data$PartyStart)
}


#' Apply the rule and the window end to end
#'
#' @param .spans Tibble from the loader, with SpanKey and CoreKey added.
#' @param .keys Tibble from ent_anchor_keys().
#' @param .lens Tibble from ent_doc_lens().
#' @param .rule List from ent_rule().
#' @param .spec List from ent_window_spec().
#' @return A list: Rule, Spec, Ent, Parties, Party, Roles, Counts.
ent_apply <- function(.spans, .keys, .lens, .rule, .spec) {
  if (FALSE) {
    .spans <- tab_spans
    .keys  <- tab_keys
    .lens  <- tab_lens
    .rule  <- .lP$Params$Rule
    .spec  <- .lP$Params$Spec
  }

  ent_    <- ent_entities(.spans = .spans, .rule = .rule)
  group_  <- ent_group_parties(.ent = ent_, .rule = .rule)
  match_  <- ent_match_party(.ent = group_, .keys = .keys)
  party_  <- ent_party_names(.ent = match_)
  chosen_ <- ent_locate_party(.party = party_, .keys = .keys, .lens = .lens)
  roles_  <- ent_window(.party_tab = party_, .party = chosen_, .spec = .spec)

  list(
    Rule = .rule, Spec = .spec, Ent = ent_, Grouped = group_, Parties = party_, Party = chosen_,
    Roles = roles_,
    Counts   = ent_counts(.roles = roles_, .party = chosen_),
    Mentions = ent_mentions(.spans = .spans, .grouped = group_, .party = party_, .rule = .rule)
  )
}


# 4. Evidence for the two numbers ---------------------------------------------------------------------------------------

#' What each candidate half-width would have counted
#'
#' EVIDENCE, NOT A CHOICE. The released rule uses one window; this table exists so a reader who
#' disagrees with 2,000 can see what the alternatives give without opening the code. The rule is
#' window-free, so the grouping and the match are computed once and each width only re-cuts.
#'
#' @param .party_tab Tibble from ent_party_names().
#' @param .party Tibble from ent_locate_party().
#' @param .pars Numeric vector of half-widths to score.
#' @param .tail_share Numeric. Held at the released value throughout.
#' @param .quiet Logical. Suppress the progress bar.
#' @return Tibble: one row per document per width.
ent_sweep_window <- function(.party_tab, .party, .pars = c(500, 1000, 2000, 4000),
                             .tail_share = 0.10, .quiet = FALSE) {
  if (FALSE) {
    .party_tab  <- tab_parties
    .party      <- tab_party
    .pars       <- c(500, 1000, 2000, 4000)
    .tail_share <- 0.10
    .quiet      <- FALSE
  }

  purrr::map(.pars, function(.p) {
    spec_ <- ent_window_spec(.par = .p, .tail_share = .tail_share)
    ent_window(.party_tab = .party_tab, .party = .party, .spec = spec_) |>
      ent_counts(.party = .party) |>
      dplyr::mutate(Spec = spec_$Label, Par = .p, .before = 1L)
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


#' What the grouping is worth, at the released window
#'
#' TWO ROWS, AND THEY ARE THE ONLY DECISION LEFT. Whether single-word fragments merge is the last
#' thing in this rule a reader could disagree with; the comparison itself carries no threshold and
#' the window is one number reported above.
#'
#' @param .spans Tibble from the loader.
#' @param .keys Tibble from ent_anchor_keys().
#' @param .lens Tibble from ent_doc_lens().
#' @param .base List from ent_rule(). The released rule.
#' @param .spec List from ent_window_spec(). Held fixed.
#' @param .quiet Logical. Suppress the progress bar.
#' @return Tibble: one row per rule.
ent_sweep_rule <- function(.spans, .keys, .lens, .base, .spec, .quiet = FALSE) {
  if (FALSE) {
    .spans <- tab_spans
    .keys  <- tab_keys
    .lens  <- tab_lens
    .base  <- .lP$Params$Rule
    .spec  <- .lP$Params$Spec
    .quiet <- FALSE
  }

  rules_ <- list(
    ent_rule_set(.rule = .base, .over = list(MergeFragments = FALSE)),
    .base
  )

  n_ <- dplyr::n_distinct(.keys$DocID)

  purrr::map(rules_, function(.r) {
    cnt_ <- ent_apply(.spans = .spans, .keys = .keys, .lens = .lens,
                      .rule = .r, .spec = .spec)$Counts
    tibble::tibble(
      Rule      = if (.r$MergeFragments) "prefix + fragment" else "prefix only",
      PctMatch  = mean(cnt_$Status == "matched"),
      PctExact  = .ent_share_or_na(.x = cnt_$IsExact[cnt_$Status == "matched"]),
      MeanKeys  = mean(cnt_$NKeysSeen),
      MeanParty = mean(cnt_$NParties),
      MeanNaive = mean(cnt_$NNaive),
      MeanCount = mean(cnt_$NCounter),
      Docs      = n_
    )
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


#' A share, or missing where there is nothing to take a share of
#'
#' mean(logical(0)) is NaN, which tbl_pct renders as "NaN%" and which a reader cannot tell from a
#' computation that went wrong.
#'
#' @param .x Logical vector, possibly empty.
#' @return The mean, or NA_real_ where .x is empty.
.ent_share_or_na <- function(.x) {
  if (FALSE) .x <- logical(0)
  if (length(.x) == 0L) NA_real_ else mean(.x)
}


# 5. Description -----------------------------------------------------------------------------------------------------

#' How many parties sit within each candidate half-width
#'
#' The direct evidence for the released window, and it does NOT settle it: the count rises steadily
#' with the width and never flattens, so the distribution has no elbow to read a cutoff from. The gap
#' profile below is what the width is argued from instead.
#'
#' @param .roles Tibble from ent_window().
#' @param .bounds Numeric vector of half-widths to score.
#' @return Tibble: one row per bound.
ent_dist_profile <- function(.roles, .bounds = c(500, 1000, 2000, 4000, 10000)) {
  if (FALSE) {
    .roles  <- tab_roles
    .bounds <- c(500, 1000, 2000, 4000, 10000)
  }

  src_ <- dplyr::filter(.roles, !.data$IsRegistrant, !is.na(.data$DistToParty))
  n_   <- dplyr::n_distinct(.roles$DocID)

  purrr::map(.bounds, function(.b) {
    tibble::tibble(
      HalfWidth  = .b,
      Names      = sum(abs(src_$DistToParty) <= .b),
      PerDoc     = sum(abs(src_$DistToParty) <= .b) / n_,
      PctOfNames = sum(abs(src_$DistToParty) <= .b) / nrow(src_)
    )
  }) |>
    purrr::list_rbind()
}


#' The gap between adjacent parties near the contracting party
#'
#' THE ARGUMENT FOR THE WIDTH. A window narrower than the distance between adjacent names cuts a list
#' of parties in half; one clearing the P90 of that distance does not, in nine documents out of ten.
#'
#' @param .roles Tibble from ent_window().
#' @param .bound Numeric. Only parties this close to the contracting party enter the profile.
#' @return One-row tibble of quantiles.
ent_gap_profile <- function(.roles, .bound = 4000) {
  if (FALSE) {
    .roles <- tab_roles
    .bound <- 4000
  }

  gaps_ <- .roles |>
    dplyr::filter(!is.na(.data$DistToParty), abs(.data$DistToParty) <= .bound) |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::mutate(Gap = .data$Start - dplyr::lag(.data$Start), .by = DocID) |>
    dplyr::filter(!is.na(.data$Gap))

  if (nrow(gaps_) == 0L) {
    return(tibble::tibble(Gaps = 0L, P50 = NA_real_, P75 = NA_real_, P90 = NA_real_,
                          P95 = NA_real_))
  }

  tibble::tibble(
    Gaps = nrow(gaps_),
    P50  = stats::quantile(gaps_$Gap, 0.50, names = FALSE),
    P75  = stats::quantile(gaps_$Gap, 0.75, names = FALSE),
    P90  = stats::quantile(gaps_$Gap, 0.90, names = FALSE),
    P95  = stats::quantile(gaps_$Gap, 0.95, names = FALSE)
  )
}


#' The commonest counterparty names, and how often each merged
#'
#' READ RATHER THAN FILTERED. There is no stoplist here and none is proposed: a name appearing in many
#' contracts may be a common counterparty rather than noise, and nothing in this document can tell
#' those apart. PctMerged is what the grouping did to each name.
#'
#' @param .roles Tibble from ent_window().
#' @param .n_docs Integer. Documents in the sample, for the share.
#' @return Tibble: one row per name.
ent_window_terms <- function(.roles, .n_docs) {
  if (FALSE) {
    .roles  <- tab_roles
    .n_docs <- nrow(tab_keys)
  }

  .roles |>
    dplyr::filter(.data$Role == "counterparty") |>
    dplyr::summarise(
      Docs      = dplyr::n_distinct(.data$DocID),
      PctMerged = mean(.data$NKeys > 1L),
      MeanKeys  = mean(.data$NKeys),
      .by = PartyKey
    ) |>
    dplyr::mutate(PctDocs = .data$Docs / .n_docs) |>
    dplyr::arrange(dplyr::desc(.data$Docs))
}


# 6. Tables ----------------------------------------------------------------------------------------------------------

#' Where the contracting party was found, by contract type
#' @param .counts Tibble from ent_counts().
#' @return Tibble: one row per type, and one for the sample.
ent_table_where <- function(.counts) {
  if (FALSE) .counts <- tab_counts

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs     = dplyr::n(),
      PctMatch = mean(.data$Status == "matched"),
      PctFirst = mean(.data$Status == "first"),
      PctNone  = mean(.data$Status == "no entity"),
      MedStart = stats::median(.data$PartyStart, na.rm = TRUE),
      MedFrac  = stats::median(.data$PartyFrac, na.rm = TRUE),
      .by = dplyr::any_of("Class")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(.counts), dplyr::desc(.data$Docs)),
    dplyr::mutate(cols_(dplyr::select(.counts, -"Class")), Class = "All", .before = 1L)
  )
}


#' What the rule removed from the naive count, by contract type
#'
#' THE TABLE THIS DOCUMENT EXISTS TO PRODUCE. NAIVE is every unique party the contract names less one
#' for the registrant; COUNT is what the window kept. A reader who disagrees with the window can read
#' the first column and ignore the second, and both are reproducible from the released file.
#'
#' @param .counts Tibble from ent_counts().
#' @return Tibble: one row per type, and one for the sample.
ent_table_naive <- function(.counts) {
  if (FALSE) .counts <- tab_counts

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs       = dplyr::n(),
      MeanNaive  = mean(.data$NNaive),
      MeanCount  = mean(.data$NCounter),
      MedNaive   = stats::median(.data$NNaive),
      MedCount   = stats::median(.data$NCounter),
      MeanRemove = mean(.data$NRemoved),
      PctRemove  = sum(.data$NRemoved) / pmax(sum(.data$NNaive), 1L),
      PctZero    = mean(.data$NCounter == 0L),
      PctAny     = mean(.data$HasAnyParty),
      .by = dplyr::any_of("Class")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(.counts), dplyr::desc(.data$Docs)),
    dplyr::mutate(cols_(dplyr::select(.counts, -"Class")), Class = "All", .before = 1L)
  )
}


#' What the signature block adds, by contract type
#'
#' MeanTailNew is parties named ONLY in the tail -- ones the window never saw -- and it is the number
#' that decides whether the signature block discovers parties or repeats them. It varies twenty-fold
#' by contract type, which is why the two counts are released apart rather than pooled.
#'
#' @param .counts Tibble from ent_counts().
#' @return Tibble: one row per type, and one for the sample.
ent_table_tail <- function(.counts) {
  if (FALSE) .counts <- tab_counts

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs         = dplyr::n(),
      MedTail      = stats::median(.data$TailWidth, na.rm = TRUE),
      MeanCount    = mean(.data$NCounter),
      MeanTailNew  = mean(.data$NTailNew),
      MeanWithTail = mean(.data$NWithTail),
      PctAnyTail   = mean(.data$NTailNew > 0L),
      PctBoth      = mean(.data$NBothEnds > 0L),
      .by = dplyr::any_of("Class")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(.counts), dplyr::desc(.data$Docs)),
    dplyr::mutate(cols_(dplyr::select(.counts, -"Class")), Class = "All", .before = 1L)
  )
}


#' What the grouping did, one row per merge kind
#'
#' @param .roles Tibble from ent_window().
#' @return Tibble: one row per MergeKind, and one for the sample.
ent_table_group <- function(.roles) {
  if (FALSE) .roles <- tab_roles

  body_ <- .roles |>
    dplyr::summarise(
      Parties  = dplyr::n(),
      Keys     = sum(.data$NKeys),
      MeanKeys = mean(.data$NKeys),
      .by = MergeKind
    ) |>
    dplyr::mutate(PctParty = .data$Parties / sum(.data$Parties)) |>
    dplyr::arrange(plot_factor(.data$MergeKind, .key = "MergeKind"))

  dplyr::bind_rows(
    body_,
    tibble::tibble(
      MergeKind = "All",
      Parties   = nrow(.roles),
      Keys      = sum(.roles$NKeys),
      MeanKeys  = mean(.roles$NKeys),
      PctParty  = 1
    )
  )
}


#' What each candidate width would have counted, by contract type
#' @param .sweep Tibble from ent_sweep_window().
#' @param .counts Tibble from ent_counts(). Supplies the naive column.
#' @return Tibble: one row per type, one column per width.
ent_table_window <- function(.sweep, .counts) {
  if (FALSE) {
    .sweep  <- tab_sweep
    .counts <- tab_counts
  }

  .sweep |>
    dplyr::summarise(Mean = mean(.data$NCounter), .by = c(Spec, Class)) |>
    tidyr::pivot_wider(names_from = Spec, values_from = Mean) |>
    dplyr::left_join(
      dplyr::summarise(.counts, Docs = dplyr::n(), Naive = mean(.data$NNaive), .by = Class),
      by = dplyr::join_by(Class)
    ) |>
    dplyr::relocate(Docs, Naive, .after = Class) |>
    dplyr::arrange(dplyr::desc(.data$Docs))
}


#' What each candidate width would have counted, by decile of document length
#' @param .sweep Tibble from ent_sweep_window().
#' @return Tibble: one row per decile, one block per width.
ent_table_length <- function(.sweep) {
  if (FALSE) .sweep <- tab_sweep

  .sweep |>
    dplyr::mutate(Decile = dplyr::ntile(.data$DocLen, 10L)) |>
    dplyr::summarise(
      Docs      = dplyr::n(),
      MedLen    = stats::median(.data$DocLen),
      MeanNaive = mean(.data$NNaive),
      MeanCount = mean(.data$NCounter),
      .by = c(Spec, Decile)
    ) |>
    dplyr::arrange(.data$Spec, .data$Decile)
}


# 7. Reading ---------------------------------------------------------------------------------------------------------

#' Every party a few documents name, in order, with its assigned role
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


#' The documents where grouping merged the most keys, so the merge can be read
#'
#' THE CHECK THE GROUPING NEEDS. A merge that is wrong produces a party carrying two companies' names
#' and nothing about the count would say so. These are the documents where it did the most work.
#'
#' @param .roles Tibble from ent_window().
#' @param .n Integer. Documents drawn.
#' @return Tibble: every merged party of the drawn documents, with its key count.
ent_read_merged <- function(.roles, .n = 8L) {
  if (FALSE) {
    .roles <- tab_roles
    .n     <- 8L
  }

  worst_ <- .roles |>
    dplyr::summarise(Merged = sum(.data$NKeys > 1L), Keys = sum(.data$NKeys), .by = DocID) |>
    dplyr::arrange(dplyr::desc(.data$Merged), dplyr::desc(.data$Keys)) |>
    dplyr::slice_head(n = .n)

  .roles |>
    dplyr::filter(.data$DocID %in% worst_$DocID, .data$NKeys > 1L) |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::select("DocID", "PartyName", "PartyKey", "NKeys", "MergeKind", "Role", "Start")
}


#' A few whole contracts, exactly as the file holds them
#'
#' THE BLOCK A READER OF THE FILE ACTUALLY NEEDS. Everything else in this document describes the
#' release; this shows it. Every row of a handful of documents, in the released columns and the
#' released order, so a reader meeting parties_org.parquet has already seen what one contract looks
#' like inside it.
#'
#' ONE DRAWN DOCUMENT IS ALWAYS A SENTINEL, where the sample holds any. The 116 contracts in which
#' nothing was found are the rows most likely to be mishandled downstream and the least likely to
#' turn up in a random draw, so one is included deliberately rather than left to chance.
#'
#' A SHORT INDEX IS ADDED, and it is not a released column. DocID is forty-four characters and would
#' take a fifth of the console width on every row, which is what the reporting convention warns
#' about: decompose the long identifier and report it once underneath. Doc is what links the two
#' tables the reporter prints.
#'
#' @param .tab Tibble from ent_release_parties().
#' @param .n Integer. Documents drawn, the sentinel among them.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: every released row of the drawn documents, with Doc added for display.
ent_release_sample <- function(.tab, .n = 4L, .seed = 42L) {
  if (FALSE) {
    .tab  <- tab_release
    .n    <- 4L
    .seed <- 42L
  }

  none_ <- unique(.tab$DocID[.tab$PartyRole == "none"])
  some_ <- setdiff(unique(.tab$DocID), none_)

  pick_none_ <- if (length(none_) > 0L) {
    withr::with_seed(.seed, sample(none_, size = 1L))
  } else {
    character(0)
  }
  n_some_ <- max(.n - length(pick_none_), 0L)
  pick_some_ <- withr::with_seed(
    .seed, sample(some_, size = min(n_some_, length(some_)))
  )

  .tab |>
    dplyr::filter(.data$DocID %in% c(pick_some_, pick_none_)) |>
    dplyr::arrange(.data$DocID, .data$PartyStart) |>
    dplyr::mutate(Doc = dplyr::dense_rank(.data$DocID), .before = 1L)
}


#' What every column of the released file means
#'
#' A TABLE RATHER THAN PROSE, AND CHECKED AGAINST THE FILE. A dictionary written as text drifts from
#' the thing it documents and nothing notices; built here and compared to names(), a column added or
#' renamed without a matching entry aborts the render instead of quietly disagreeing with its own
#' documentation. That is the discipline 04A applies to its own store description.
#'
#' @param .tab Tibble from ent_release_parties().
#' @return Tibble: Column, Meaning, OnSentinel.
ent_dictionary_parties <- function(.tab) {
  if (FALSE) .tab <- tab_release

  dict_ <- tibble::tribble(
    ~Column,       ~Meaning,                                                        ~OnSentinel,
    "DocID",       "the contract; joins to the register and every other 04 file",   "present",
    "Class",       "contract type, from 03A's label spine",                         "present",
    "AmendType",   "original or amended, from the same spine",                      "present",
    "PartyName",   "the party's name, as the member that named it wrote it",        "null",
    "PartyKey",    "the reduced key, matching the same party across contracts",     "null",
    "PartyRole",   "registrant, counterparty, signatory, other, or none",           "none",
    "Matched",     "did this party's key agree with the EDGAR filer name",          "FALSE",
    "PartyStart",  "offset of its earliest mention, into 04A's canonical text",     "null",
    "PartyStop",   "offset of the end of that mention",                             "null",
    "NameFrom",    "matched member, or most mentioned -- which spelling named it",  "null",
    "NVariants",   "how many spellings the grouping merged into this party",        "null",
    "NMentions",   "how often the contract names it, across every spelling",        "null"
  )

  # The two failure directions are named separately because they mean different things: a column in
  # the file with no entry is undocumented output, and an entry with no column is a dictionary
  # describing something that no longer exists.
  undoc_  <- setdiff(names(.tab), dict_$Column)
  unseen_ <- setdiff(dict_$Column, names(.tab))
  say_    <- function(.x) if (length(.x) == 0L) "none" else paste(.x, collapse = ", ")

  if (length(undoc_) > 0L || length(unseen_) > 0L) {
    cli::cli_abort(c(
      "The dictionary and the released file disagree.",
      "x" = "In the file and undocumented: {say_(undoc_)}.",
      "x" = "Documented and not in the file: {say_(unseen_)}.",
      "i" = "A dictionary that can drift from its file documents nothing."
    ))
  }

  dict_[match(names(.tab), dict_$Column), ]
}



# 8. Report ----------------------------------------------------------------------------------------------------------

#' How many documents carry a usable anchor key at all
#' @param .tab Tibble from ent_anchor_keys().
#' @return Invisibly .tab.
ent_report_keys <- function(.tab) {
  if (FALSE) .tab <- tab_keys

  cli::cli_h2("The anchor")

  tibble::tibble(
    Item = c("Documents in the sample",
             "Carrying a registered company name",
             "Reduced to a usable key",
             "Key of one word"),
    N    = c(nrow(.tab),
             sum(!is.na(.tab$CompanyName)),
             sum(!is.na(.tab$AnchorKey)),
             sum(!is.na(.tab$AnchorTok) & .tab$AnchorTok == 1L))
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$N / nrow(.tab))) |>
    tbl_say(.title = "What the EDGAR side supplies")

  cli::cli_alert_info(
    "A document with no key can only take a fallback party, so the third row is the ceiling on the \\
     matched share. The fourth is worth watching: a one-word anchor agrees with any contract name \\
     beginning with that word, which is right for BOEING and would be wrong for a common word."
  )
  invisible(.tab)
}


#' What the grouping merged
#' @param .tab Tibble from ent_table_group().
#' @return Invisibly .tab.
ent_report_group <- function(.tab) {
  if (FALSE) .tab <- tab_group

  cli::cli_h2("Grouping names into parties")
  .tab |>
    dplyr::mutate(PctParty = tbl_pct(.data$PctParty), MeanKeys = tbl_num(.data$MeanKeys)) |>
    tbl_say(.title = "One row per merge kind, over every party in the sample")

  cli::cli_alert_info(
    "PREFIX is one name being the beginning of another -- ACME and ACME ENERGY. FRAGMENT is a single \\
     word taken unambiguously from a longer name, which is lexnlp splitting one company across \\
     mentions. ALONE is a party no other name reached. Keys above Parties is the whole of what this \\
     step removed; if the two are equal, nothing merged and every table below is measuring the \\
     window alone."
  )
  invisible(.tab)
}


#' Distances inside the party's neighbourhood
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
    dplyr::mutate(PctOfNames = tbl_pct(.data$PctOfNames), PerDoc = tbl_num(.data$PerDoc)) |>
    tbl_say(.title = "Parties within each candidate half-width")

  tbl_say(.tab = .gap, .title = "Gap between adjacent parties, among those near the contracting one")

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

  out_ <- .tab |>
    dplyr::summarise(Docs = dplyr::n(), .by = Status) |>
    dplyr::mutate(Share = tbl_pct(.data$Docs / sum(.data$Docs))) |>
    dplyr::arrange(plot_factor(.data$Status, .key = "PartyStatus"))

  tbl_say(.tab = out_, .title = "Status, over every document in the sample")

  m_ <- dplyr::filter(.tab, .data$Status == "matched")
  tibble::tibble(
    Item = c("Matched documents",
             "Both names ran out together",
             "The match was also the first party named"),
    N    = c(nrow(m_),
             sum(m_$IsExact),
             sum(dplyr::coalesce(m_$MatchIsFirst, FALSE)))
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$N / pmax(nrow(m_), 1L))) |>
    tbl_say(.title = "What the match looked like where it succeeded")

  cli::cli_alert_info(
    "FIRST is the fallback: no party in the document agreed with the EDGAR name, so the first party \\
     named was taken. The second row above says how much of the match rate rests on equality rather \\
     than on one name containing the other; the third is what the fallback would have got right had \\
     it been used everywhere, and it is partly true by construction because the registrant is \\
     usually named first."
  )
  invisible(out_)
}


#' Where the party was found, by contract type
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
    tbl_say(.title = "Match rate and party position, by contract type")

  cli::cli_alert_info(
    "MedStart is in CHARACTERS and MedFrac is the same quantity as a share of the document. Read the \\
     first: the party's offset is close to constant across a wide range of contract length, so the \\
     fraction moves with length rather than with where the party is, and class correlates with \\
     length."
  )
  invisible(.tab)
}


#' The rule against the naive count
#' @param .tab Tibble from ent_table_naive().
#' @return Invisibly .tab.
ent_report_naive <- function(.tab) {
  if (FALSE) .tab <- tab_naive

  cli::cli_h2("The rule against the naive count")
  .tab |>
    dplyr::mutate(
      dplyr::across(c(MeanNaive, MeanCount, MeanRemove), \(.x) tbl_num(.x)),
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))
    ) |>
    tbl_say(.title = "Counterparties against the naive count, by contract type")

  cli::cli_alert_info(
    "NAIVE is every unique party the contract names anywhere, less one for the registrant -- what a \\
     reader gets without a rule. COUNT is what the window kept. PctRemove is the share of the naive \\
     total the rule discarded: too low and the rule is doing nothing, too high and it is cutting \\
     parties rather than noise. PctZero is the documents left with no counterparty at all, and it \\
     should be read against the contract type rather than on its own -- an employment agreement's \\
     counterparty is a PERSON, which no organisation extractor can find. PctAny is the coverage \\
     figure: at least one party besides the registrant found ANYWHERE, whether the window kept it \\
     or not, and it is the honest answer to how much of the sample this rule reaches."
  )
  invisible(.tab)
}


#' What the signature block adds
#' @param .tab Tibble from ent_table_tail().
#' @return Invisibly .tab.
ent_report_tail <- function(.tab) {
  if (FALSE) .tab <- tab_tail

  cli::cli_h2("What the signature block adds")
  .tab |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Mean"), \(.x) tbl_num(.x)),
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))
    ) |>
    tbl_say(.title = "Signature-block parties, by contract type")

  cli::cli_alert_info(
    "MeanTailNew is parties named ONLY in the tail, which the window never saw. It varies by more \\
     than an order of magnitude across contract types -- a credit agreement names its lending \\
     syndicate in the signature block and an employment agreement names nobody there -- so pooling \\
     the two counts would make one number mean different things by type. They are released apart, \\
     and MeanWithTail is what a study of syndicated lending would use."
  )
  invisible(.tab)
}


#' What each candidate width would have counted
#' @param .tab Tibble from ent_table_window().
#' @return Invisibly .tab.
ent_report_window_table <- function(.tab) {
  if (FALSE) .tab <- tab_win

  cli::cli_h2("The window, by contract type")
  tbl_say(.tab = .tab, .title = "Mean counterparties per document at each candidate half-width")

  cli::cli_alert_info(
    "EVIDENCE, NOT A MENU. The rule releases one width and this table says what the others would \\
     have given. NAIVE is the column every other one is read against; no width can exceed it, and \\
     none of them flattens, which is why the width is argued from the gap between adjacent names \\
     rather than from this table."
  )
  invisible(.tab)
}


#' What each candidate width would have counted, by document length
#' @param .tab Tibble from ent_table_length().
#' @return Invisibly .tab.
ent_report_length <- function(.tab) {
  if (FALSE) .tab <- tab_length

  cli::cli_h2("The window against document length")
  .tab |>
    dplyr::mutate(dplyr::across(c(MeanNaive, MeanCount), \(.x) tbl_num(.x))) |>
    tbl_say(.title = "One row per decile, one block per width")

  cli::cli_alert_info(
    "THE WINDOW DOES NOT SCALE WITH LENGTH, and this is the evidence: the naive count rises about \\
     twentyfold across the deciles while the windowed count roughly doubles. A rule scaling with \\
     length would track the first column, which is what makes a proportional window the wrong \\
     instrument and a fixed one the right one."
  )
  invisible(.tab)
}


#' What the grouping is worth
#' @param .tab Tibble from ent_sweep_rule().
#' @return Invisibly .tab.
ent_report_sweep_rule <- function(.tab) {
  if (FALSE) .tab <- tab_sweep_rule

  cli::cli_h2("The grouping, swept")
  .tab |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
      dplyr::across(dplyr::starts_with("Mean"), \(.x) tbl_num(.x))
    ) |>
    tbl_say(.title = "Two rules, the window held at the released value")

  cli::cli_alert_info(
    "MeanKeys against MeanParty is the grouping: where they differ, names merged. PctMatch should \\
     not move, because a fragment of the registrant's name reduces to the same party either way; \\
     MeanCount should fall, because a counterparty lexnlp split across mentions was being counted \\
     twice."
  )
  invisible(.tab)
}


#' The commonest counterparty names
#' @param .tab Tibble from ent_window_terms().
#' @param .n Integer. Rows shown.
#' @return Invisibly .tab.
ent_report_terms <- function(.tab, .n = 25L) {
  if (FALSE) {
    .tab <- tab_terms
    .n   <- 25L
  }

  cli::cli_h2("The commonest counterparty names")
  .tab |>
    dplyr::slice_head(n = .n) |>
    dplyr::mutate(
      PctDocs   = tbl_pct(.data$PctDocs),
      PctMerged = tbl_pct(.data$PctMerged),
      MeanKeys  = tbl_num(.data$MeanKeys)
    ) |>
    tbl_say(.title = paste0("Top ", .n, " by documents"))

  cli::cli_alert_info(
    "NO STOPLIST, and none is proposed: a name appearing in many contracts may be a common \\
     counterparty rather than noise, and nothing in this document separates those. PctMerged says \\
     how often the grouping had work to do on each name."
  )
  invisible(.tab)
}


#' Every party of a few documents, read in order
#' @param .tab Tibble from ent_read_entities().
#' @param .counts Tibble from ent_counts().
#' @param .title Character. Table title.
#' @return Invisibly .tab.
ent_report_entities <- function(.tab, .counts, .title = "Whole documents, read in order") {
  if (FALSE) {
    .tab    <- tab_read
    .counts <- tab_counts
    .title  <- "Whole documents, read in order"
  }

  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("Nothing to read.")
    return(invisible(.tab))
  }

  cli::cli_h2(.title)

  .tab |>
    dplyr::left_join(dplyr::select(.counts, DocID, NNaive, NCounter), by = dplyr::join_by(DocID)) |>
    dplyr::select("DocID", "CompanyName", "PartyName", "Role", "MergeKind", "NKeys", "Start",
                  "NNaive", "NCounter") |>
    tbl_say(.title = "CompanyName is what EDGAR records; PartyName is what the contract wrote")

  cli::cli_alert_info(
    "Read the ROLE column against the position: a counterparty should sit near the registrant and a \\
     signatory near the end. NKeys above one means the grouping merged something, and those rows are \\
     the ones worth checking by eye."
  )
  invisible(.tab)
}


#' The merges, read rather than trusted
#' @param .tab Tibble from ent_read_merged().
#' @return Invisibly .tab.
ent_report_merged <- function(.tab) {
  if (FALSE) .tab <- tab_merged

  cli::cli_h2("The documents where grouping did the most work")

  if (nrow(.tab) == 0L) {
    cli::cli_alert_info("No document merged a single key, so there is nothing to read here.")
    return(invisible(.tab))
  }

  tbl_say(.tab = .tab, .title = "Every merged party of the documents with the most merges")

  cli::cli_alert_info(
    "A WRONG MERGE PUTS TWO COMPANIES UNDER ONE NAME and no count would reveal it, so this block is \\
     the check. PartyName is the member the naming rule chose; NKeys is how many spellings it stood \\
     for. A row where the name looks like a heading glued to a company is the case the naming rule \\
     exists for, and it should be resolved in favour of the clean name."
  )
  invisible(.tab)
}


#' The rule in one table
#'
#' OVERVIEW REARRANGES WHAT IS ESTABLISHED; IT DOES NOT REPEAT IT. An earlier version called every
#' report function again here, which printed eleven tables twice and made the rendered document
#' impossible to navigate -- a reader scrolling could not tell which half they were in. This is the
#' answer instead: every number the rule produced, in one place, each already argued above.
#'
#' @param .counts Tibble from ent_counts().
#' @param .release Tibble from ent_release_parties().
#' @return Invisibly the table.
ent_report_headline <- function(.counts, .release) {
  if (FALSE) {
    .counts  <- tab_counts
    .release <- tab_release
  }

  cli::cli_h2("The rule in one table")

  out_ <- tibble::tribble(
    ~Item,                                        ~Value,
    "Contracts",                                  format(nrow(.counts), big.mark = ","),
    "Parties found",                              format(sum(.counts$NParties), big.mark = ","),
    "Registrant matched to EDGAR",                tbl_pct(mean(.counts$Status == "matched")),
    "Registrant taken as the first party named",  tbl_pct(mean(.counts$Status == "first")),
    "No organisation found at all",               tbl_pct(mean(.counts$Status == "no entity")),
    "At least one party besides the registrant",  tbl_pct(mean(.counts$HasAnyParty)),
    "Naive counterparties per contract",          tbl_num(mean(.counts$NNaive)),
    "Counterparties per contract, this rule",     tbl_num(mean(.counts$NCounter)),
    "Share of the naive count removed",           tbl_pct(sum(.counts$NRemoved) /
                                                            pmax(sum(.counts$NNaive), 1L)),
    "Signatories per contract, counted apart",    tbl_num(mean(.counts$NTailNew)),
    "Rows in the released file",                  format(nrow(.release), big.mark = ",")
  )

  tbl_say(.tab = out_, .title = "Everything this document decided")

  cli::cli_alert_info(
    "The two counterparty rows are the whole result: a reader counting every party a contract names \\
     and subtracting the registrant gets the first, and this rule gives the second. Every other row \\
     says how far that answer can be trusted."
  )
  invisible(out_)
}


#' What the released file holds, by role
#' @param .tab Tibble from ent_release_parties().
#' @return Invisibly the summary.
ent_report_release <- function(.tab) {
  if (FALSE) .tab <- tab_release

  cli::cli_h2("The released file")

  out_ <- .tab |>
    dplyr::summarise(Rows = dplyr::n(), Docs = dplyr::n_distinct(.data$DocID), .by = PartyRole) |>
    dplyr::mutate(Share = tbl_pct(.data$Rows / sum(.data$Rows))) |>
    dplyr::arrange(plot_factor(.data$PartyRole, .key = "PartyRole"))

  tbl_say(.tab = out_, .title = "One row per contract per party, by role")

  cli::cli_alert_info(
    "OTHER IS THE LARGEST ROLE AND THAT IS THE DESIGN. Those are the parties the window excluded, \\
     kept in the file so a reader can recompute the naive count or re-cut the window from the \\
     release rather than from this render. They are the control group, not discarded data."
  )
  cli::cli_alert_info(
    "NONE is the sentinel: a contract in which lexnlp proposed no organisation at all still gets one \\
     row, so n_distinct(DocID) over this file is the whole sample. Without it every mean computed \\
     downstream would divide by the documents that worked, and nothing would say so."
  )
  invisible(out_)
}


#' A few whole contracts, printed as the file holds them
#'
#' TWO TABLES, AND THE FIRST IS WHY. What EDGAR recorded is not a column of the released file -- it
#' is registrant metadata rather than a fact about a party -- but without it beside the rows a reader
#' meeting Matched = FALSE cannot tell whether the comparison failed or the contract genuinely never
#' names its filer. So the EDGAR name is joined for DISPLAY, into the header table where it belongs:
#' it is constant within a document, and repeating it on every party row would say the same thing
#' four times and cost a quarter of the console width.
#'
#' @param .tab Tibble from ent_release_sample().
#' @param .keys Tibble from ent_anchor_keys(). Supplies CompanyName, for reading only.
#' @return Invisibly .tab.
ent_report_release_sample <- function(.tab, .keys) {
  if (FALSE) {
    .tab  <- tab_sample
    .keys <- tab_keys
  }

  cli::cli_h2("The released file, read")

  .tab |>
    dplyr::distinct(.data$Doc, .data$DocID, .data$Class, .data$AmendType) |>
    dplyr::left_join(dplyr::select(.keys, DocID, CompanyName), by = dplyr::join_by(DocID)) |>
    tbl_say(.title = "The drawn contracts, and what EDGAR records as the filer")

  .tab |>
    dplyr::select("Doc", "PartyName", "PartyKey", "PartyRole", "Matched", "PartyStart",
                  "PartyStop", "NameFrom", "NVariants", "NMentions") |>
    tbl_say(.title = "Every party of those contracts, as the file holds them")

  cli::cli_alert_info(
    "READ THE TWO TOGETHER. Matched says whether a party agreed with the CompanyName above, so a \\
     registrant marked FALSE is the fallback: nothing in that contract resembled the EDGAR name and \\
     the first party found was taken. Doc links the tables; DocID, Class and AmendType are on every \\
     row of the file and are reported once here because they do not vary within a contract."
  )
  cli::cli_alert_info(
    "ONE OF THESE DOCUMENTS IS A SENTINEL, drawn deliberately: it carries the role none and a null \\
     name, and it is the row shape most likely to be dropped by a downstream filter. Anyone writing \\
     a not-missing test on PartyName puts the 116 empty contracts back out of the denominator, \\
     which is the exact failure the sentinel exists to prevent."
  )
  invisible(.tab)
}


#' What the mention index holds
#' @param .tab Tibble from ent_mentions().
#' @param .release Tibble from ent_release_parties().
#' @return Invisibly the summary.
ent_report_mentions <- function(.tab, .release) {
  if (FALSE) {
    .tab     <- tab_mentions
    .release <- tab_release
  }

  cli::cli_h2("The mention index")

  party_ <- dplyr::filter(.release, .data$PartyRole != "none")
  out_ <- tibble::tibble(
    Item = c("Parties in the release",
             "Mentions of them",
             "Parties named more than once",
             "Mentions that are not the first"),
    N    = c(nrow(party_),
             nrow(.tab),
             sum(dplyr::count(.tab, .data$DocID, .data$PartyKey)$n > 1L),
             sum(!.tab$IsFirst))
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$N / pmax(nrow(.tab), 1L)))

  tbl_say(.tab = out_, .title = "One row per mention, four columns")

  cli::cli_alert_info(
    "THE RELEASE KNOWS WHERE A PARTY WAS FIRST NAMED; THIS KNOWS WHERE IT WAS NAMED AT ALL. The last \\
     row is the whole reason the index exists: those are the occurrences a rule measuring context \\
     could not previously reach, and a signature block -- where a party is named WITH an address -- \\
     is almost always among them. Filtering to IsFirst reproduces the old behaviour exactly."
  )
  invisible(out_)
}


#' The column dictionary
#' @param .tab Tibble from ent_dictionary_parties().
#' @return Invisibly .tab.
ent_report_dictionary <- function(.tab) {
  if (FALSE) .tab <- tab_dict

  cli::cli_h2("What every column means")

  tbl_say(.tab = .tab, .title = "Twelve columns, in the order the file carries them")

  cli::cli_alert_info(
    "OnSentinel says what each column holds on a contract where nothing was found. The dictionary is \\
     built from a declared list and compared with the file's own names, so a column added or renamed \\
     without an entry aborts this chunk rather than leaving the documentation quietly wrong."
  )
  invisible(.tab)
}


# 9. Figures ---------------------------------------------------------------------------------------------------------

#' Where organisation spans sit in the document, weighted two ways
#'
#' SPAN-WEIGHTED AND DOCUMENT-WEIGHTED TOGETHER, because they answer different questions. Span mass
#' can be produced by a handful of table-heavy filings; document mass cannot. Where the two agree the
#' shape is a property of contracts, and where they diverge it is a property of a few documents.
#'
#' @param .tab Tibble from ent_density(), carrying Weight, Pos and Share.
#' @return A ggplot.
ent_plot_density <- function(.tab) {
  if (FALSE) .tab <- tab_density

  ggplot2::ggplot(.tab, ggplot2::aes(x = .data$Pos, y = .data$Share, colour = .data$Weight)) +
    ggplot2::geom_line(linewidth = 0.7) +
    plot_scale_colour_cat(name = NULL) +
    plot_scale_x_pct(.accuracy = 1) +
    plot_scale_y_pct(.accuracy = 1) +
    ggplot2::labs(x = "Position in the document", y = "Share of mass") +
    plot_theme(.grid = "y", .legend = "bottom")
}


#' The rule against the naive count, by contract type
#'
#' THE FIGURE THIS DOCUMENT IS FOR. Two bars per class -- what a reader would count without a rule,
#' and what the window kept -- so the gap between them is the rule, read directly.
#'
#' @param .tab Tibble from ent_table_naive().
#' @return A ggplot.
ent_plot_naive <- function(.tab) {
  if (FALSE) .tab <- tab_naive

  .tab |>
    dplyr::filter(.data$Class != "All") |>
    dplyr::select("Class", Naive = "MeanNaive", Rule = "MeanCount") |>
    tidyr::pivot_longer(cols = c("Naive", "Rule"), names_to = "Measure", values_to = "Mean") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Mean,
                                 y = stats::reorder(.data$Class, .data$Mean),
                                 fill = .data$Measure)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
    plot_scale_fill_cat(name = NULL) +
    plot_scale_x_count() +
    ggplot2::labs(x = "Mean counterparties per contract", y = NULL) +
    plot_theme(.grid = "x", .legend = "bottom")
}


#' How each document's party was found
#'
#' plot_bar_stacked() DRAWS THE CATEGORY ON Y AND THE VALUE ON X, and applies its own value scale.
#' Adding a y scale here would put a continuous scale on the contract types. It also computes the
#' share itself through .share, so raw counts go in.
#'
#' @param .tab Tibble from ent_counts().
#' @return A ggplot.
ent_plot_status <- function(.tab) {
  if (FALSE) .tab <- tab_counts

  .tab |>
    dplyr::summarise(N = dplyr::n(), .by = c(Class, Status)) |>
    plot_bar_stacked(
      .cat      = "Class",
      .val      = "N",
      .fill     = "Status",
      .key_fill = "PartyStatus",
      .short    = TRUE,
      .share    = TRUE      # position_fill and a percent axis, both from the helper
    ) +
    ggplot2::labs(x = "Share of documents")
}


#' What the window kept, and what the signature block adds
#'
#' THE TWO RELEASED COUNTS SIDE BY SIDE, because the second is what a reader has to decide whether to
#' add. On employment agreements it is a rounding error; on credit agreements it doubles the answer.
#'
#' @param .tab Tibble from ent_table_tail().
#' @return A ggplot.
ent_plot_tail <- function(.tab) {
  if (FALSE) .tab <- tab_tail

  .tab |>
    dplyr::filter(.data$Class != "All") |>
    dplyr::select("Class", Counterparties = "MeanCount", Signatories = "MeanTailNew") |>
    tidyr::pivot_longer(cols = c("Counterparties", "Signatories"), names_to = "Measure",
                        values_to = "Mean") |>
    plot_bar_stacked(
      .cat   = "Class",
      .val   = "Mean",
      .fill  = "Measure",
      .short = TRUE
    ) +
    ggplot2::labs(x = "Mean counterparties and signatories per contract")
}


#' How many keys each party stood for
#' @param .tab Tibble from ent_window().
#' @return A ggplot.
ent_plot_group <- function(.tab) {
  if (FALSE) .tab <- tab_roles

  .tab |>
    dplyr::summarise(N = dplyr::n(), .by = MergeKind) |>
    dplyr::mutate(Share = .data$N / sum(.data$N)) |>
    plot_bar_ranked(
      .cat      = "MergeKind",
      .val      = "Share",
      .key      = "MergeKind",
      .short    = TRUE,
      .pct      = TRUE,   # the helper owns the value scale; .pct makes it a percent axis
      .accuracy = 1
    ) +
    ggplot2::labs(x = "Share of all parties found, registrant included")
}
