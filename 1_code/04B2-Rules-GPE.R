# 04B2-Rules-GPE: where each party is ------------------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# matcon proposes place names with offsets. This decides which of them describe a party's location,
# which one names the law rather than an address, and what each resolves to.
#
# TWO FILES COME OUT AND BOTH ARE LONG
#   places_geo.parquet   one row per place, INCLUDING the ones the rule refused
#   law_clauses.parquet  one row per governing-law clause
# Neither is a per-party or per-contract table. The collapse to one row per party is a function this
# document defines, scores and checks -- geo_collapse() -- and the export calls it. A collapse stored
# as data is a collapse that cannot be changed without re-releasing; a collapse stored as a query can
# be changed by whoever disagrees with it.
#
# WHY THE REFUSALS ARE IN THE FILE
# Two decisions remove a place: it sits inside a governing-law clause, or it is further than the
# reach from any party mention. Both are recorded as columns rather than as absences, so the file
# shows the rule working rather than only its output -- and so the naive ladder is a filter over one
# file rather than a second computation.
#
# THE NAIVE LADDER, ALL FROM places_geo.parquet
#   NaiveStates    every place, refusals included        n_distinct(State)
#   LawFreeStates  places outside a law clause           the same, filtered on !InLawClause
#   RuleStates     places attached to a counterparty     filtered on Attached and the role
# Each rung is one filter further than the last. The middle one isolates what the governing-law
# exclusion bought, which is the most contestable decision in this document, and it costs nothing.
#
# THE COLLAPSE IS ONE PICK, NOT FOUR
# The previous version chose a city, a county, a country and a state through four independent
# selections, so a party could carry a Delaware state beside a Texas county with nothing in the file
# saying they came from different places. Now the NEAREST attached place wins outright, ties broken
# by the earliest place offset, and State and Country are read off that one place. They cannot
# disagree because there is only one of them.
#
# WHAT NEAREST-WINS MEANS, STATED RATHER THAN DISCOVERED
# "ACME CORPORATION, a Delaware corporation with offices in Austin, Texas" puts Delaware four
# characters after the party name and Austin thirty-nine. So the released state is the INCORPORATION
# state whenever a contract states one, because incorporation language sits immediately after a party
# name and an address does not. That is a choice, it is the simple one, and the dictionary says so.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_lookup <- .lP$Input$Lookup
  .dir_store   <- .lP$Input$Store
}


# 1. Vocabulary --------------------------------------------------------------------------------------------------------
# PartyRole is registered in _Commons/_Entity.R, because 04B1 writes it and this document reads it.
# What follows is private to this document.

plot_register_levels(
  .key    = "GeoLevel",
  .levels = c("Country", "State", "County", "City", "Other"),
  .short  = c("country", "state", "county", "city", "other")
)

plot_register_levels(
  .key    = "GeoStateFrom",
  .levels = c("named", "unique", "named nearby", "unresolved", "none"),
  .short  = c("named", "unique", "nearby", "unresolved", "none")
)

plot_register_levels(
  .key    = "LawSpecified",
  .levels = c("named", "self-referential", "clause without a place", "none found"),
  .short  = c("named", "self-ref", "no place", "none")
)


# 2. The gazetteer -------------------------------------------------------------------------------------------------------

#' Read the two columns the extractor does not emit
#'
#' MOST OF THIS JOIN HAS GONE AWAY, and that is gazetteer-v2 rather than a simplification made here.
#' The extractor now emits GeoKey, NParent, IsWord and the two ISO columns on every row it writes, so
#' the lookup is needed for the two it does not: the state a place sits in and that state's county.
#'
#' READING THE PACKAGE'S OWN DATA FILE IS CROSSING THE CLI SEAM, and it is safe for a specific reason
#' rather than by convention. gazetteer.py's spec() folds a content hash of this exact file into
#' gazetteer-v2's spec hash, so a lookup that changed moves the model's hash and ner_manifest_write()
#' aborts on the next ingest. The seam is crossed and guarded: this document and the store cannot
#' silently disagree about which lookup they used.
#'
#' EVERY COLUMN IS PREFIXED Lk. The gpe table carries its own ISO columns, so an unprefixed join
#' would suffix both sides and every later reference would silently read whichever one dplyr put
#' first.
#'
#' @param .path_lookup The gazetteer's geo_lookup.parquet, inside the matcon-extract package.
#' @return Tibble: GeoKey, LkClass, LkState, LkCounty.
geo_lookup <- function(.path_lookup) {
  if (FALSE) .path_lookup <- .lP$Input$Lookup

  if (!fs::file_exists(.path_lookup)) {
    cli::cli_abort(c(
      "No geo lookup at {.path {(.path_lookup)}}.",
      "i" = "It ships inside matcon-extract, which is where the rebuilder lives too."
    ))
  }

  arrow::read_parquet(.path_lookup) |>
    dplyr::select("GeoKey", "GeoClass", "StateName", "ParentCounty") |>
    dplyr::distinct(.data$GeoKey, .data$GeoClass, .keep_all = TRUE) |>
    dplyr::rename(LkClass = "GeoClass", LkState = "StateName", LkCounty = "ParentCounty")
}


#' Every state a city name exists in, and its county there
#'
#' THE LOOKUP'S OWN AMBIGUITY, KEPT RATHER THAN COLLAPSED. Two thirds of city names exist in several
#' states -- 19,443 of 103,377 -- and geo_lookup() answers that by nulling the parents. This keeps
#' the candidate list instead, so a city can be resolved by asking WHICH OF ITS OWN STATES THE
#' CONTRACT NAMES. That is the same evidence the extractor already uses to admit a word-like name at
#' all, and it is what stops the resolution reaching for a state the city does not exist in.
#'
#' @param .path_lookup The gazetteer's geo_lookup.parquet.
#' @return Tibble: GeoKey, CandState, CandCounty -- one row per city name per state it exists in.
geo_candidates <- function(.path_lookup) {
  if (FALSE) .path_lookup <- .lP$Input$Lookup

  arrow::read_parquet(.path_lookup) |>
    dplyr::filter(stringi::stri_detect_fixed(.data$GeoClass, "Populated Place"),
                  !is.na(.data$StateName)) |>
    dplyr::transmute(
      GeoKey,
      CandState  = stringi::stri_trans_toupper(.data$StateName),
      CandCounty = .data$ParentCounty
    ) |>
    dplyr::distinct()
}


# 3. Resolving a place ---------------------------------------------------------------------------------------------------

#' Put matcon's GPE spans on the shared hierarchy
#'
#' MATCON ARRIVES PART-RESOLVED. gazetteer-v2 emits GeoKey, NParent, IsWord and two ISO columns
#' itself, so only the state and county come from the lookup. Its class is in the core column
#' LabelRaw, which is what the lookup is keyed on beside the name.
#'
#' ITS ISO COLUMNS ARE RENAMED ON READ, in _Entity.R, and the reason is that matcon and LexNLP use
#' the same two names for four different quantities. matcon resolves a country code on EVERY level --
#' USA on a state, a city, a county -- and a subdivision code on everything BUT a country. Read under
#' unrenamed names the two engines disagree silently, and an earlier version of this document did
#' exactly that: it came out right because the country column was read first and matcon's happened to
#' be one, while the subdivision recovery sat dead on every row.
#'
#' LkState, NOT the lookup's ParentState: a state's parent is not itself, so ParentState is NA on
#' every US State row, and reading it once reported 179 states at country level. Iso3 is guarded
#' against the literal string "nan", which an earlier extractor wrote and which reads as a country
#' code rather than as missing.
#'
#' @param .spans Tibble from ent_load_entity() for matcon GPE.
#' @param .lookup Tibble from geo_lookup().
#' @return .spans reduced to the shared columns.
geo_resolve <- function(.spans, .lookup) {
  if (FALSE) {
    .spans  <- tab_raw
    .lookup <- tab_lookup
  }

  .spans |>
    dplyr::mutate(GeoKeyUp = stringi::stri_trans_toupper(.data$GeoKey)) |>
    dplyr::left_join(.lookup, by = dplyr::join_by(GeoKeyUp == GeoKey, LabelRaw == LkClass)) |>
    dplyr::mutate(
      GeoKey     = .data$GeoKeyUp,
      GeoUnit    = stringi::stri_trans_totitle(.data$GeoKeyUp),
      RawClass   = .data$LabelRaw,
      NParentOut = dplyr::coalesce(as.integer(.data$NParent), 1L),
      Ambig      = !is.na(.data$NParent) & .data$NParent > 1L,
      Iso3Out    = dplyr::na_if(.data$CountryIso3, "nan"),
      Iso2Out    = dplyr::if_else(.data$Ambig, NA_character_, dplyr::na_if(.data$SubIso, "nan")),
      StateOut   = dplyr::if_else(.data$Ambig, NA_character_,
                                  stringi::stri_trans_toupper(.data$LkState)),
      CountyOut  = dplyr::if_else(.data$Ambig, NA_character_, .data$LkCounty),
      WordLike   = as.integer(.data$IsWord),
      GeoLevel   = dplyr::case_when(
        .data$RawClass == "Country"                                     ~ "Country",
        .data$RawClass == "US State"                                    ~ "State",
        .data$RawClass == "US County"                                   ~ "County",
        stringi::stri_detect_fixed(dplyr::coalesce(.data$RawClass, ""),
                                   "Populated Place")                   ~ "City",
        .default                                                        = "Other"
      )
    ) |>
    dplyr::select("DocID", "Start", "Stop", "Span", "DocLen", "GeoKey", "GeoUnit", "GeoLevel",
                  County = "CountyOut", State = "StateOut", Iso2 = "Iso2Out", Iso3 = "Iso3Out",
                  "RawClass", NParent = "NParentOut", Ambiguous = "Ambig", "WordLike")
}


#' Give every city a state
#'
#' THREE ARMS, ALL REPORTED. A city whose name exists in one state takes it. A city whose name is
#' ambiguous takes whichever of ITS OWN candidate states the contract names -- nearest to the city,
#' and where two are equidistant the one the contract names most often. A city whose candidates the
#' contract never names keeps no state.
#'
#' THE CONSTRAINT ON THE SECOND ARM IS WHAT MAKES IT SAFE. Taking the nearest state named anywhere
#' would put SPRINGFIELD in Delaware whenever the party is a Delaware corporation; requiring that a
#' SPRINGFIELD exist in that state does not. It uses no new data -- the candidate list is in the
#' lookup already.
#'
#' POSITION FIRST, FREQUENCY SECOND. "Springfield, Illinois" puts the state two characters away, and
#' position settles that outright; frequency first would lose it wherever the contract named another
#' candidate state more often, which for Delaware is most of the corpus. Frequency decides only
#' between equals, and it counts mentions WITHIN THE CONTRACT, so no quantity here depends on how the
#' corpus was partitioned.
#'
#' @param .geo Tibble from geo_resolve().
#' @param .cand Tibble from geo_candidates().
#' @return .geo with State and County filled where they can be, plus GeoStateFrom recording the arm.
geo_city_state <- function(.geo, .cand) {
  if (FALSE) {
    .geo  <- tab_geo
    .cand <- geo_once$Cand
  }

  # Every US state the contract names, with its position and how often it is named.
  states_ <- .geo |>
    dplyr::filter(.data$GeoLevel == "State", !is.na(.data$State)) |>
    dplyr::distinct(.data$DocID, .data$State, .data$Start) |>
    dplyr::rename(NamedState = State, StateAt = Start) |>
    dplyr::mutate(NamedTimes = dplyr::n(), .by = c(DocID, NamedState))

  # Ambiguous cities, crossed with the states they could be in AND the contract names.
  fixed_ <- .geo |>
    dplyr::filter(.data$GeoLevel == "City", .data$Ambiguous) |>
    dplyr::select(DocID, Start, GeoKey) |>
    dplyr::inner_join(.cand, by = dplyr::join_by(GeoKey), relationship = "many-to-many") |>
    dplyr::inner_join(states_, by = dplyr::join_by(DocID, CandState == NamedState),
                      relationship = "many-to-many") |>
    dplyr::mutate(Gap = abs(.data$StateAt - .data$Start)) |>
    # NEAREST, THEN MOST OFTEN NAMED. The second key is what settles a tie that used to fall to row
    # order, which is no rule at all.
    dplyr::arrange(.data$DocID, .data$Start, .data$Gap, dplyr::desc(.data$NamedTimes)) |>
    dplyr::slice_head(n = 1L, by = c(DocID, Start)) |>
    dplyr::select(DocID, Start, FixState = CandState, FixCounty = CandCounty)

  .geo |>
    dplyr::left_join(fixed_, by = dplyr::join_by(DocID, Start)) |>
    dplyr::mutate(
      GeoStateFrom = dplyr::case_when(
        .data$GeoLevel == "State"                ~ "named",
        .data$GeoLevel != "City"                 ~ "none",
        !.data$Ambiguous & !is.na(.data$State)   ~ "unique",
        !is.na(.data$FixState)                   ~ "named nearby",
        .default                                 = "unresolved"
      ),
      State  = dplyr::coalesce(.data$State,  .data$FixState),
      County = dplyr::coalesce(.data$County, .data$FixCounty)
    ) |>
    dplyr::select(-FixState, -FixCounty)
}


#' Give every place a country
#'
#' A state, county or city is in the United States by construction: the gazetteer's classes are US
#' tiers. A country is its own. A subdivision the four levels have no tier for -- Ontario, Guangdong,
#' Scotland -- keeps the ISO code its engine resolved, which is what lets it roll to a country
#' without being reported at a level that does not exist here.
#'
#' THE ISO-3166-2 PREFIX IS FREE COUNTRY INFORMATION, and the first version threw it away. Of the geo
#' dictionary's 437 entities only the 253 countries carry an ISO-3166-3, so Iso3 is NA on every
#' Ontario and Guangdong. Their ISO-3166-2 is a SUBDIVISION code -- CA-AB, CN-13, GB-ENG -- whose
#' prefix is exactly the country. A country's own code has no hyphen and is left alone, which is why
#' the split is guarded rather than applied to every row.
#'
#' @param .geo Tibble from geo_city_state().
#' @return .geo with CountryIso and CountryName added.
geo_country <- function(.geo) {
  if (FALSE) .geo <- tab_geo

  .geo |>
    dplyr::mutate(
      PrefixIso = dplyr::if_else(
        stringi::stri_detect_fixed(dplyr::coalesce(.data$Iso2, ""), "-"),
        stringi::stri_extract_first_regex(.data$Iso2, "^[A-Z]{2}"),
        NA_character_
      ),
      CountryIso = dplyr::case_when(
        .data$GeoLevel %in% c("State", "County", "City") ~ "USA",
        !is.na(.data$Iso3)                               ~ .data$Iso3,
        !is.na(.data$PrefixIso)                          ~ .data$PrefixIso,
        .default                                         = NA_character_
      ),
      # The name is left missing for a recovered subdivision. A two-letter code is not a country
      # name, and inventing one here would put a value in the release that no source produced.
      CountryName = dplyr::case_when(
        .data$GeoLevel %in% c("State", "County", "City") ~ "United States",
        .data$GeoLevel == "Country"                      ~ .data$GeoUnit,
        .default                                         = NA_character_
      )
    ) |>
    dplyr::select(-"PrefixIso")
}


# 4. Governing law -------------------------------------------------------------------------------------------------------

#' Which places sit inside a governing-law clause
#'
#' A NON-EQUI JOIN, AND IT READS NO TEXT. lawregex emits the clause as a span with offsets, so asking
#' whether a place is inside one is a comparison of two integer ranges. The rule it replaces read 120
#' characters before every place and asked whether that window carried cue language, which is
#' proximity rather than containment and which cost an hour of reading at corpus scale.
#'
#' join_by()'s within(x_lower, x_upper, y_lower, y_upper) asks whether the X range fits inside the Y
#' range, so THE PLACE BELONGS ON THE LEFT. Putting the clause there asks whether a 45-character
#' clause fits inside an 8-character place; it does not, and every document comes back empty while
#' the offsets plainly nest. It also cannot be namespaced: join_by() evaluates a small DSL of its own
#' and dplyr exports no within(), so dplyr::within() resolves to base R's and errors.
#'
#' @param .geo Tibble from geo_country().
#' @param .law Tibble of LAW spans from ent_load_entity().
#' @return Tibble: DocID, Start, LawStart, LawKind -- one row per place inside a clause.
geo_law_inside <- function(.geo, .law) {
  if (FALSE) {
    .geo <- tab_geo
    .law <- tab_law
  }

  if (nrow(.law) == 0L) {
    return(tibble::tibble(DocID = character(0), Start = integer(0), LawStart = integer(0),
                          LawKind = character(0)))
  }

  .geo |>
    dplyr::select("DocID", "Start", "Stop") |>
    dplyr::inner_join(
      dplyr::select(.law, DocID, LawStart = "Start", LawStop = "Stop", LawKind = "LabelRaw"),
      by = dplyr::join_by(DocID, within(Start, Stop, LawStart, LawStop))
    ) |>
    dplyr::select("DocID", "Start", "LawStart", "LawKind") |>
    dplyr::distinct(.data$DocID, .data$Start, .keep_all = TRUE)
}


# 5. Attaching a place to a party ----------------------------------------------------------------------------------------

#' Build one attachment specification
#'
#' ONE NUMBER FOR EVERY LEVEL, and it is generous for one of them rather than tuned per level. States
#' attach at a median of five characters -- "a Delaware corporation" -- and cities at seventy-six,
#' because an address is longer. A per-level reach would fit the data better and cost a paragraph to
#' defend; one number is reported here and the registered-address check is what says whether it is
#' too wide.
#'
#' @param .reach Numeric. Furthest a party may be and still claim a place. 200 characters.
#' @param .label Character or NULL. Overrides the generated label, used in the reach table.
#' @return A named list carrying the attachment specification.
geo_spec <- function(.reach = 200, .label = NULL) {
  if (FALSE) {
    .reach <- 200
    .label <- NULL
  }

  list(
    Reach = .reach,
    Label = if (is.null(.label)) paste0(format(.reach, big.mark = ","), " chars") else .label
  )
}


#' Attach every place to the party before it
#'
#' STACK AND FILL, NOT A ROLLING JOIN. Places and parties are interleaved by offset and the party
#' columns carried DOWN, so each place picks up the one immediately preceding it. A rolling join on
#' the closest match returns more than one row wherever two parties tie on position; this returns
#' exactly one row per place by construction, and it is joined back on an explicit row id rather than
#' on order.
#'
#' BACKWARDS ONLY, AND IMMEDIATELY PRECEDING RATHER THAN NEAREST. A preamble names the company and
#' then qualifies it, so the place that describes a party follows it. Sorting puts a mention before a
#' place at the same offset, so a place inside a company name attaches to that company. The nearest
#' mention may be the one AFTER the place, and that one has not been introduced yet.
#'
#' EVERY MENTION, NOT EVERY PARTY, AND THAT IS THE WHOLE OF THE CHANGE. 04B1's release carries a
#' party at the offsets of its FIRST mention, and interleaving that gave a place beside a party's
#' third mention nothing to attach to. Signature blocks are exactly where a party is named WITH an
#' address -- "Address for notices: 123 Main Street, Houston, Texas" -- so the occurrences a
#' first-mention table cannot reach are the ones most likely to carry a place.
#'
#' THE COST OF THE OLD SHAPE WAS MEASURED BEFORE IT WAS CHANGED. The median party had no place within
#' reach of its first mention at any window up to 800 characters, while fewer than a fifth of the
#' places in the corpus attached to anything at all.
#'
#' A PLACE INSIDE A GOVERNING-LAW CLAUSE NEVER REACHES THIS FUNCTION. It is withheld by the caller,
#' because it names the law that applies rather than any party's address, and attaching it would put
#' NEW YORK in the incorporation column of whichever company the clause happened to follow.
#'
#' @param .geo Tibble from geo_country(), already stripped of governing-law places.
#' @param .mentions Tibble read from 04B1's mentions_org.parquet.
#' @param .spec List from geo_spec().
#' @return .geo with PartyKey, MentionStart, MentionIsFirst, DistToParty and Attached added.
geo_attach <- function(.geo, .mentions, .spec) {
  if (FALSE) {
    .geo      <- tab_free
    .mentions <- tab_mentions
    .spec     <- .lP$Params$Spec
  }

  base_ <- dplyr::mutate(.geo, RowId = dplyr::row_number())

  g_ <- base_ |>
    dplyr::transmute(DocID, Pos = .data$Start, IsGeo = TRUE, RowId,
                     PKey = NA_character_, PStart = NA_integer_, PStop = NA_integer_,
                     PFirst = NA)

  o_ <- .mentions |>
    dplyr::transmute(DocID, Pos = .data$MentionStart, IsGeo = FALSE, RowId = NA_integer_,
                     PKey = .data$PartyKey, PStart = .data$MentionStart,
                     PStop = .data$MentionStop, PFirst = .data$IsFirst)

  near_ <- dplyr::bind_rows(o_, g_) |>
    # FALSE sorts before TRUE, so a mention at the same offset counts as preceding.
    dplyr::arrange(.data$DocID, .data$Pos, .data$IsGeo) |>
    dplyr::group_by(.data$DocID) |>
    tidyr::fill(PKey, PStart, PStop, PFirst, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::filter(.data$IsGeo) |>
    dplyr::select(RowId, PartyKey = PKey, MentionStart = PStart, MentionStop = PStop,
                  MentionIsFirst = PFirst)

  base_ |>
    dplyr::left_join(near_, by = dplyr::join_by(RowId)) |>
    dplyr::mutate(
      # Distance between the NEAREST EDGES, so a place immediately following a company name scores a
      # couple of characters rather than the length of the name.
      DistToParty    = .data$Start - .data$MentionStop,
      Attached       = !is.na(.data$DistToParty) & .data$DistToParty >= 0 &
                       .data$DistToParty <= .spec$Reach,
      PartyKey       = dplyr::if_else(.data$Attached, .data$PartyKey,      NA_character_),
      MentionStart   = dplyr::if_else(.data$Attached, .data$MentionStart,  NA_integer_),
      MentionIsFirst = dplyr::if_else(.data$Attached, .data$MentionIsFirst, NA),
      DistToParty    = dplyr::if_else(.data$Attached, .data$DistToParty,   NA_integer_)
    ) |>
    dplyr::select(-MentionStop, -RowId)
}


# 6. The two releases ----------------------------------------------------------------------------------------------------

#' The release: one row per place, refusals included
#'
#' EVERY PLACE THE EXTRACTOR PROPOSED, with what it resolved to and what the rule did with it. A
#' place removed by the governing-law exclusion and a place beyond the reach are both here, marked,
#' rather than absent -- which is what makes both exclusions auditable and what makes the naive
#' ladder a filter over this file rather than a second computation.
#'
#' THE ATTACHMENT COLUMNS ARE NULL ON A REFUSED ROW, and that is not the same as a missing value. A
#' law-clause place was never offered to the attachment, so it has no distance to a party; a place
#' beyond the reach was offered and refused, and Attached says which.
#'
#' @param .places Tibble from geo_country(): every place, resolved.
#' @param .free Tibble from geo_attach(): the places offered to the attachment, with its columns.
#' @param .inside Tibble from geo_law_inside(): the places a clause contains.
#' @return Tibble: one row per place.
geo_release_places <- function(.places, .free, .inside) {
  if (FALSE) {
    .places <- tab_places
    .free   <- tab_attached
    .inside <- tab_inside
  }

  law_ <- dplyr::distinct(.inside, .data$DocID, .data$Start) |>
    dplyr::mutate(InLawClause = TRUE)

  cols_ <- function(.d) {
    dplyr::transmute(
      .d,
      .data$DocID,
      PlaceStart     = as.integer(.data$Start),
      PlaceStop      = as.integer(.data$Stop),
      PlaceText      = .data$Span,
      .data$GeoUnit,
      .data$GeoLevel,
      GeoState       = .data$State,
      GeoCounty      = .data$County,
      GeoCountryIso  = .data$CountryIso,
      GeoCountry     = .data$CountryName,
      .data$GeoStateFrom,
      .data$Ambiguous,
      PartyKey       = if ("PartyKey" %in% names(.d)) .data$PartyKey else NA_character_,
      MentionStart   = if ("MentionStart" %in% names(.d)) {
        as.integer(.data$MentionStart)
      } else {
        NA_integer_
      },
      MentionIsFirst = if ("MentionIsFirst" %in% names(.d)) .data$MentionIsFirst else NA,
      DistToParty    = if ("DistToParty" %in% names(.d)) {
        as.integer(.data$DistToParty)
      } else {
        NA_integer_
      },
      Attached       = if ("Attached" %in% names(.d)) .data$Attached else FALSE
    )
  }

  held_ <- .places |>
    dplyr::semi_join(law_, by = dplyr::join_by(DocID, Start))

  dplyr::bind_rows(cols_(.free), cols_(held_)) |>
    dplyr::left_join(law_, by = dplyr::join_by(DocID, PlaceStart == Start)) |>
    dplyr::mutate(InLawClause = dplyr::coalesce(.data$InLawClause, FALSE)) |>
    dplyr::arrange(.data$DocID, .data$PlaceStart)
}


#' The release: one row per governing-law clause
#'
#' A SEPARATE FILE BECAUSE OF ONE LEVEL. A clause that names no place at all produces no place row,
#' so a jurisdiction carried as a column of a file of places could never express it -- and that level
#' is exactly what distinguishes a contract with no governing-law clause from one whose clause named
#' no jurisdiction. Keeping the clauses is what makes both statable.
#'
#' ONE ROW PER CLAUSE, NOT PER CONTRACT. A contract stating the law twice states it twice; collapsing
#' here would decide which one counts, which is the export's business rather than this file's.
#'
#' A COUNTY OR A CITY IS NOT A JURISDICTION. Contracts choose the law of a state or a country, so a
#' place resolved at a lower tier inside a clause is a mention and the clause keeps a null.
#'
#' @param .law Tibble of LAW spans from ent_load_entity().
#' @param .places Tibble from geo_country().
#' @param .inside Tibble from geo_law_inside().
#' @return Tibble: one row per clause.
geo_release_law <- function(.law, .places, .inside) {
  if (FALSE) {
    .law    <- tab_law
    .places <- tab_places
    .inside <- tab_inside
  }

  place_ <- .inside |>
    dplyr::left_join(
      dplyr::select(.places, DocID, Start, GeoUnit, GeoLevel),
      by = dplyr::join_by(DocID, Start)
    ) |>
    dplyr::filter(.data$GeoLevel %in% c("State", "Country"))

  .law |>
    dplyr::transmute(
      .data$DocID,
      LawStart = as.integer(.data$Start),
      LawStop  = as.integer(.data$Stop),
      LawKind  = .data$LabelRaw
    ) |>
    dplyr::left_join(
      dplyr::transmute(place_, .data$DocID, .data$LawStart, PlaceStart = as.integer(.data$Start),
                       .data$GeoUnit, .data$GeoLevel),
      by = dplyr::join_by(DocID, LawStart), relationship = "one-to-many"
    ) |>
    # A NAMED JURISDICTION OUTRANKS A NULL, then the earliest place inside the clause.
    dplyr::arrange(.data$DocID, .data$LawStart, is.na(.data$GeoUnit), .data$PlaceStart) |>
    dplyr::slice_head(n = 1L, by = c(DocID, LawStart)) |>
    dplyr::transmute(
      .data$DocID, .data$LawStart, .data$LawStop, .data$LawKind,
      Jurisdiction      = .data$GeoUnit,
      JurisdictionLevel = .data$GeoLevel
    ) |>
    dplyr::arrange(.data$DocID, .data$LawStart)
}


#' Apply the rule end to end
#'
#' ONE ENTRY POINT, AND 04D CALLS EXACTLY THIS. The order is a dependency: a place is resolved, then
#' tested for containment in a clause, then -- only if it survived that -- offered to the attachment.
#' A law-clause place is never offered, which is why it carries no distance in the release.
#'
#' @param .spans Tibble from ent_load_entity() for matcon GPE.
#' @param .law Tibble from ent_load_entity() for matcon LAW.
#' @param .mentions Tibble read from 04B1's org_mentions.parquet.
#' @param .geo List: Lookup and Cand.
#' @param .spec List from geo_spec().
#' @return A list: Spec, Places, Inside, Attached, Release, Law.
geo_apply <- function(.spans, .law, .mentions, .geo, .spec) {
  if (FALSE) {
    .spans    <- tab_spans
    .law      <- tab_law
    .mentions <- tab_mentions
    .geo      <- geo_once
    .spec     <- .lP$Params$Spec
  }

  places_ <- .spans |>
    geo_resolve(.lookup = .geo$Lookup) |>
    geo_city_state(.cand = .geo$Cand) |>
    geo_country()

  inside_ <- geo_law_inside(.geo = places_, .law = .law)

  free_ <- places_ |>
    dplyr::anti_join(dplyr::distinct(inside_, .data$DocID, .data$Start),
                     by = dplyr::join_by(DocID, Start)) |>
    geo_attach(.mentions = .mentions, .spec = .spec)

  list(
    Spec     = .spec,
    Places   = places_,
    Inside   = inside_,
    Attached = free_,
    Release  = geo_release_places(.places = places_, .free = free_, .inside = inside_),
    Law      = geo_release_law(.law = .law, .places = places_, .inside = inside_)
  )
}


#' What every column of the place file means
#' @param .tab Tibble from geo_release_places().
#' @return Tibble: Column, Grain, Meaning.
geo_dictionary_places <- function(.tab) {
  if (FALSE) .tab <- tab_release

  dict_ <- tibble::tribble(
    ~Column,          ~Grain,     ~Meaning,
    "DocID",          "document", "the contract; joins to the register and every other 04 file",
    "PlaceStart",     "place",    "offset of the place name, into 04A's canonical text",
    "PlaceStop",      "place",    "offset one past its last character",
    "PlaceText",      "place",    "the surface form, raw: it slices from the two offsets exactly",
    "GeoUnit",        "place",    "the gazetteer's name for what this resolved to",
    "GeoLevel",       "place",    "Country, State, County, City or Other",
    "GeoState",       "place",    "the US state, where the tier or the resolution gives one",
    "GeoCounty",      "place",    "that city's county; kept for reading, not a research variable",
    "GeoCountryIso",  "place",    "ISO code; USA for any US tier",
    "GeoCountry",     "place",    "the country name; null for a recovered subdivision",
    "GeoStateFrom",   "place",    "named, unique, named nearby, unresolved or none",
    "Ambiguous",      "place",    "the name exists under more than one parent in the gazetteer",
    "PartyKey",       "place",    "the party it attached to; null where it attached to none",
    "MentionStart",   "place",    "the mention it reached the party through",
    "MentionIsFirst", "place",    "was that the party's first mention",
    "DistToParty",    "place",    "characters from the mention's end to the place",
    "Attached",       "place",    "did it fall within the reach of a party mention",
    "InLawClause",    "place",    "did it sit inside a governing-law clause, so never offered"
  )

  undoc_  <- setdiff(names(.tab), dict_$Column)
  unseen_ <- setdiff(dict_$Column, names(.tab))
  say_    <- function(.x) if (length(.x) == 0L) "none" else paste(.x, collapse = ", ")

  if (length(undoc_) > 0L || length(unseen_) > 0L) {
    cli::cli_abort(c(
      "The place dictionary and the released file disagree.",
      "x" = "In the file and undocumented: {say_(undoc_)}.",
      "x" = "Documented and not in the file: {say_(unseen_)}.",
      "i" = "A dictionary that can drift from its file documents nothing."
    ))
  }

  dict_[match(names(.tab), dict_$Column), ]
}


#' What every column of the clause file means
#' @param .tab Tibble from geo_release_law().
#' @return Tibble: Column, Grain, Meaning.
geo_dictionary_law <- function(.tab) {
  if (FALSE) .tab <- tab_law_rel

  dict_ <- tibble::tribble(
    ~Column,             ~Grain,   ~Meaning,
    "DocID",             "document", "the contract",
    "LawStart",          "clause",   "offset of the governing-law clause",
    "LawStop",           "clause",   "offset one past its last character",
    "LawKind",           "clause",   "what the extractor called the clause",
    "Jurisdiction",      "clause",   "the state or country inside it; null where it named none",
    "JurisdictionLevel", "clause",   "State or Country; null with the jurisdiction"
  )

  undoc_  <- setdiff(names(.tab), dict_$Column)
  unseen_ <- setdiff(dict_$Column, names(.tab))
  say_    <- function(.x) if (length(.x) == 0L) "none" else paste(.x, collapse = ", ")

  if (length(undoc_) > 0L || length(unseen_) > 0L) {
    cli::cli_abort(c(
      "The clause dictionary and the released file disagree.",
      "x" = "In the file and undocumented: {say_(undoc_)}.",
      "x" = "Documented and not in the file: {say_(unseen_)}."
    ))
  }

  dict_[match(names(.tab), dict_$Column), ]
}


# 7. Reading the release -------------------------------------------------------------------------------------------------
#
# EVERYTHING BELOW READS THE TWO RELEASED FILES AND 04B1'S, AND NOTHING ELSE. That is what makes
# these the functions 04D and the export borrow: a chunk of the corpus release has the same columns
# as the sample's, so the same query produces the corpus number.

#' One row per party, from the place file
#'
#' THE COLLAPSE, DEFINED HERE AND WRITTEN NOWHERE. This document scores it, reports it and checks its
#' shape against a dictionary; the export calls the same function to materialise it. A collapse
#' stored as data cannot be changed without re-releasing, and this one has a decision in it that a
#' reader may well want to change.
#'
#' NEAREST WINS, TIES BY THE EARLIEST PLACE. One sort key and one tie-break, ignoring which mention
#' the place reached the party through. The previous version preferred the earliest MENTION first, so
#' a place two characters from a party's third mention lost to one a hundred and fifty characters
#' from its first. That was defended on the grounds that nothing already published would move, which
#' is not a reason that survives a rewrite.
#'
#' ONE PLACE, SO THE COLUMNS CANNOT DISAGREE. State and Country are read off the same row. Choosing
#' them separately let a party carry a Delaware state beside a Texas county, with nothing in the file
#' saying they came from different places.
#'
#' THE STATE IS THE INCORPORATION STATE WHENEVER A CONTRACT STATES ONE, because "a Delaware
#' corporation" sits immediately after a party name and "with offices in" does not. That follows from
#' nearest-wins rather than from a separate rule, and it is what this variable means.
#'
#' @param .release Tibble or dataset from geo_release_places().
#' @param .party Tibble from ent_party_facts(), one row per party.
#' @return Tibble: one row per party, with the geography columns.
geo_collapse <- function(.release, .party) {
  if (FALSE) {
    .release <- tab_release
    .party   <- tab_party_facts
  }

  pick_ <- .release |>
    dplyr::filter(.data$Attached, !is.na(.data$PartyKey)) |>
    dplyr::arrange(.data$DocID, .data$PartyKey, .data$DistToParty, .data$PlaceStart) |>
    dplyr::slice_head(n = 1L, by = c(DocID, PartyKey)) |>
    dplyr::select("DocID", "PartyKey", "GeoState", "GeoStateFrom", "GeoCountry",
                  "GeoCountryIso", GeoFrom = "GeoUnit", GeoFromLevel = "GeoLevel")

  .party |>
    dplyr::left_join(pick_, by = dplyr::join_by(DocID, PartyKey)) |>
    dplyr::mutate(
      GeoStateFrom = dplyr::coalesce(.data$GeoStateFrom, "none"),
      # A US TIER IMPLIES THE COUNTRY. The place's own resolution already says this; a party placed
      # in Texas with no country would be a gap this rule invented.
      GeoCountry    = dplyr::if_else(
        is.na(.data$GeoCountry) & !is.na(.data$GeoState), "United States", .data$GeoCountry
      ),
      GeoCountryIso = dplyr::if_else(
        is.na(.data$GeoCountryIso) & !is.na(.data$GeoState), "USA", .data$GeoCountryIso
      )
    )
}


#' Does any place attached to the registrant appear in the address EDGAR records
#'
#' ASKED OVER EVERY ATTACHED PLACE, NOT OVER THE COLLAPSED ONE, and the difference is a real gain.
#' The previous version tested the single city the collapse chose, so a registrant whose preamble
#' names one city and whose signature block names the one EDGAR holds came back FALSE. Asking whether
#' ANY of its attached places appears in the address is both simpler and more accurate, and it is why
#' the city no longer needs to survive the collapse.
#'
#' A FLAG AND NOT A FILL. Using EDGAR to supply a missing location would make the variable partly
#' what the contract says and partly what EDGAR says, with nothing downstream able to tell which.
#'
#' THE CEILING IS LOW AND LOW IS NOT FAILURE. Most registrants are incorporated in Delaware and
#' located elsewhere, and EDGAR records the FIRM's address rather than the one written into the
#' contract -- often a subsidiary's, a landlord's or the counterparty's.
#'
#' @param .release Tibble from geo_release_places().
#' @param .party Tibble from ent_party_facts().
#' @param .keys Tibble from ent_anchor_keys(), carrying BusinessAddress.
#' @return Tibble: DocID, PartyKey, EdgarAgrees. NA where there is no address to check against.
.geo_addr_hits <- function(.release, .party, .keys) {
  if (FALSE) {
    .release <- tab_release
    .party   <- tab_party_facts
    .keys    <- tab_keys
  }

  reg_ <- .party |>
    dplyr::filter(.data$PartyRole == "registrant") |>
    dplyr::select("DocID", "PartyKey")

  addr_ <- .keys |>
    dplyr::transmute(
      .data$DocID,
      Addr = stringi::stri_trans_toupper(dplyr::coalesce(.data$BusinessAddress, ""))
    ) |>
    dplyr::filter(nzchar(.data$Addr))

  .release |>
    dplyr::filter(.data$Attached, !is.na(.data$PartyKey), !is.na(.data$GeoUnit)) |>
    dplyr::inner_join(reg_,  by = dplyr::join_by(DocID, PartyKey)) |>
    dplyr::inner_join(addr_, by = dplyr::join_by(DocID)) |>
    dplyr::transmute(
      .data$DocID, .data$PartyKey, .data$PlaceStart, .data$GeoUnit, .data$DistToParty,
      Hit = stringi::stri_detect_fixed(
        .data$Addr, stringi::stri_trans_toupper(.data$GeoUnit)
      )
    )
}


#' Does any place attached to the registrant appear in the address EDGAR records
#'
#' A FLAG AND NOT A RATE, and that is right for a release: an analyst wants to know whether this
#' party's location is corroborated, not what share of its places were. The rate is the sweep's
#' business and lives in .geo_addr_hits().
#'
#' @param .release Tibble from geo_release_places().
#' @param .party Tibble from ent_party_facts().
#' @param .keys Tibble from ent_anchor_keys(), carrying BusinessAddress.
#' @return Tibble: DocID, PartyKey, EdgarAgrees.
geo_edgar_check <- function(.release, .party, .keys) {
  if (FALSE) {
    .release <- tab_release
    .party   <- tab_party_facts
    .keys    <- tab_keys
  }

  if (!"BusinessAddress" %in% names(.keys)) {
    cli::cli_abort(c(
      "The anchor carries no BusinessAddress, so the registered-address check cannot run.",
      "i" = "ent_anchor_keys() joins it from 01C's LandingPage.parquet and skips the join where
             that file is absent."
    ))
  }

  hits_ <- .geo_addr_hits(.release = .release, .party = .party, .keys = .keys)

  spine_ <- .party |>
    dplyr::filter(.data$PartyRole == "registrant") |>
    dplyr::select("DocID", "PartyKey") |>
    dplyr::semi_join(
      dplyr::filter(.keys, nzchar(dplyr::coalesce(.data$BusinessAddress, ""))),
      by = dplyr::join_by(DocID)
    )

  spine_ |>
    dplyr::left_join(
      dplyr::summarise(hits_, EdgarAgrees = any(.data$Hit), .by = c(DocID, PartyKey)),
      by = dplyr::join_by(DocID, PartyKey)
    ) |>
    dplyr::mutate(EdgarAgrees = dplyr::coalesce(.data$EdgarAgrees, FALSE))
}


#' One row per document, from the place file
#'
#' THE NAIVE LADDER AND THE POPULATION, computed from the release rather than from the chain. Each
#' rung is one filter further than the last, so a reader who rejects the governing-law exclusion
#' takes the first and one who rejects the attachment takes the second.
#'
#' COUNTS ON BOTH SIDES. The rule's output is a set of places and the naive alternative is a set of
#' places, so the comparison is like for like: how many distinct states does the contract name, and
#' how many does the rule assign to a counterparty. An earlier design compared a count to a modal
#' assignment, which is not a comparison at all.
#'
#' THE COUNTERPARTIES AND NOT EVERY PARTY. The registrant's own state is in EDGAR and does not need
#' extracting; what this pipeline adds is the geography of the OTHER side of the contract.
#'
#' EVERY DOCUMENT IN .keys GETS A ROW, including the ones where matcon found no place at all. Those
#' carry zeros rather than missing values, because a contract naming no place named no place.
#'
#' @param .release Tibble or dataset from geo_release_places().
#' @param .party Tibble from ent_party_facts().
#' @param .keys Tibble from ent_anchor_keys(). Supplies the population.
#' @return Tibble: one row per document.
geo_doc_facts <- function(.release, .party, .keys) {
  if (FALSE) {
    .release <- tab_release
    .party   <- tab_party_facts
    .keys    <- tab_keys
  }

  role_ <- dplyr::select(.party, "DocID", "PartyKey", "PartyRole")

  base_ <- .release |>
    dplyr::left_join(role_, by = dplyr::join_by(DocID, PartyKey))

  nd_ <- function(.x) dplyr::n_distinct(.x[!is.na(.x)])

  all_ <- base_ |>
    dplyr::summarise(
      NPlaces        = dplyr::n(),
      NaiveStates    = nd_(.data$GeoState),
      NaiveCountries = nd_(.data$GeoCountryIso),
      NInLaw         = sum(.data$InLawClause),
      NAttached      = sum(.data$Attached),
      .by = DocID
    )

  free_ <- base_ |>
    dplyr::filter(!.data$InLawClause) |>
    dplyr::summarise(
      LawFreeStates    = nd_(.data$GeoState),
      LawFreeCountries = nd_(.data$GeoCountryIso),
      .by = DocID
    )

  rule_ <- base_ |>
    dplyr::filter(.data$Attached, .data$PartyRole == "counterparty") |>
    dplyr::summarise(
      RuleStates    = nd_(.data$GeoState),
      RuleCountries = nd_(.data$GeoCountryIso),
      .by = DocID
    )

  dplyr::select(.keys, "DocID") |>
    dplyr::left_join(all_,  by = dplyr::join_by(DocID)) |>
    dplyr::left_join(free_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(rule_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(dplyr::across(
      c(NPlaces, NaiveStates, NaiveCountries, NInLaw, NAttached,
        LawFreeStates, LawFreeCountries, RuleStates, RuleCountries),
      \(.x) as.integer(dplyr::coalesce(.x, 0L))
    ))
}


#' The naive ladder, by contract type
#'
#' THREE RUNGS AND THE STEP BETWEEN EACH PAIR IS ONE DECISION. Naive to law-free is the governing-law
#' exclusion; law-free to rule is the attachment and the counterparty role together. A reader who
#' disagrees with either takes the rung above it and recomputes from the file.
#'
#' @param .doc Tibble from geo_doc_facts(), optionally carrying Class.
#' @return Tibble: one row per type, and one for the sample.
geo_table_naive <- function(.doc) {
  if (FALSE) .doc <- tab_doc

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs         = dplyr::n(),
      MeanPlaces   = mean(.data$NPlaces),
      MeanNaive    = mean(.data$NaiveStates),
      MeanLawFree  = mean(.data$LawFreeStates),
      MeanRule     = mean(.data$RuleStates),
      MeanCountry  = mean(.data$RuleCountries),
      PctAnyPlace  = mean(.data$NPlaces > 0L),
      PctAnyRule   = mean(.data$RuleStates > 0L),
      PctLawCut    = sum(.data$NInLaw) / pmax(sum(.data$NPlaces), 1L),
      .by = dplyr::any_of("Class")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(.doc), dplyr::desc(.data$Docs)),
    dplyr::mutate(cols_(dplyr::select(.doc, -dplyr::any_of("Class"))), Class = "All", .before = 1L)
  )
}


#' What geography each role got, from the collapse
#' @param .tab Tibble from geo_collapse().
#' @return Tibble: one row per role, and one for every party.
geo_table_cover <- function(.tab) {
  if (FALSE) .tab <- tab_collapse

  src_ <- dplyr::filter(.tab, .data$PartyRole != "none")

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Parties    = dplyr::n(),
      PctState   = mean(!is.na(.data$GeoState)),
      PctCountry = mean(!is.na(.data$GeoCountry)),
      PctNonUS   = .geo_share_or_na(
        .x = .data$GeoCountryIso[!is.na(.data$GeoCountryIso)] != "USA"
      ),
      .by = dplyr::any_of("PartyRole")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(src_), plot_factor(.data$PartyRole, .key = "PartyRole")),
    dplyr::mutate(cols_(dplyr::select(src_, -"PartyRole")), PartyRole = "All", .before = 1L)
  )
}


#' What the collapse produces, as a dictionary
#'
#' A CONTRACT ON A FUNCTION RATHER THAN ON A FILE, and that is the stronger claim. Nothing writes
#' this shape here; the export calls geo_collapse() and gets it. Checking the function's output means
#' the export cannot produce a different one.
#'
#' @param .tab Tibble from geo_collapse().
#' @return Tibble: Column, Meaning.
geo_dictionary_parties <- function(.tab) {
  if (FALSE) .tab <- tab_collapse

  dict_ <- tibble::tribble(
    ~Column,         ~Meaning,
    "DocID",         "the contract",
    "PartyKey",      "the reduced key, from 04B1",
    "PartyName",     "the party's name, from 04B1",
    "PartyRole",     "registrant, cofiler, counterparty, signatory, other or none",
    "MatchKind",     "exact, prefix, fallback, cofiler or none",
    "NameFrom",      "which spelling named the party",
    "MergeKind",     "how the grouping merged the spellings",
    "NVariants",     "spellings merged into this party",
    "NMentions",     "how often the contract names it",
    "PartyStart",    "offset of its earliest mention",
    "PartyStop",     "offset of the end of that mention",
    "MaxStart",      "offset of its latest mention",
    "GeoState",      "the US state of the NEAREST attached place; incorporation where stated",
    "GeoStateFrom",  "named, unique, named nearby, unresolved or none",
    "GeoCountry",    "the country of that same place",
    "GeoCountryIso", "that country's ISO code",
    "GeoFrom",       "the place the geography was read off",
    "GeoFromLevel",  "what tier that place was"
  )

  undoc_  <- setdiff(names(.tab), dict_$Column)
  unseen_ <- setdiff(dict_$Column, names(.tab))
  say_    <- function(.x) if (length(.x) == 0L) "none" else paste(.x, collapse = ", ")

  if (length(undoc_) > 0L || length(unseen_) > 0L) {
    cli::cli_abort(c(
      "The collapse dictionary and geo_collapse()'s output disagree.",
      "x" = "In the output and undocumented: {say_(undoc_)}.",
      "x" = "Documented and not in the output: {say_(unseen_)}."
    ))
  }

  dict_[match(names(.tab), dict_$Column), ]
}


#' What the clause file says, one row per outcome
#'
#' LawSpecified IS DERIVED HERE AND NOT STORED. A clause with a jurisdiction is named; a
#' self-referential clause says the law of this agreement; a clause with neither named no place; and
#' a document with no clause row at all found none. The fourth level is why the clauses are a file.
#'
#' @param .law Tibble from geo_release_law().
#' @param .keys Tibble from ent_anchor_keys(). Supplies the population.
#' @return Tibble: one row per outcome.
geo_table_law <- function(.law, .keys) {
  if (FALSE) {
    .law  <- tab_law_rel
    .keys <- tab_keys
  }

  doc_ <- .law |>
    dplyr::mutate(
      Kind = dplyr::case_when(
        !is.na(.data$Jurisdiction)              ~ "named",
        .data$LawKind == "SelfReferential"      ~ "self-referential",
        .default                                = "clause without a place"
      )
    ) |>
    dplyr::arrange(.data$DocID, plot_factor(.data$Kind, .key = "LawSpecified"), .data$LawStart) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::select("DocID", LawSpecified = "Kind")

  dplyr::select(.keys, "DocID") |>
    dplyr::left_join(doc_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(LawSpecified = dplyr::coalesce(.data$LawSpecified, "none found")) |>
    dplyr::summarise(Docs = dplyr::n(), .by = LawSpecified) |>
    dplyr::mutate(Share = .data$Docs / sum(.data$Docs)) |>
    dplyr::arrange(plot_factor(.data$LawSpecified, .key = "LawSpecified"))
}


#' The commonest governing-law jurisdictions
#' @param .law Tibble from geo_release_law().
#' @param .n Integer. Rows returned.
#' @return Tibble: one row per jurisdiction.
geo_table_law_top <- function(.law, .n = 15L) {
  if (FALSE) {
    .law <- tab_law_rel
    .n   <- 15L
  }

  named_ <- dplyr::filter(.law, !is.na(.data$Jurisdiction))
  if (nrow(named_) == 0L) return(tibble::tibble())

  named_ |>
    dplyr::summarise(Clauses = dplyr::n(), .by = c(Jurisdiction, JurisdictionLevel)) |>
    dplyr::mutate(Share = .data$Clauses / nrow(named_)) |>
    dplyr::arrange(dplyr::desc(.data$Clauses)) |>
    dplyr::slice_head(n = .n)
}


#' A few whole contracts, exactly as the place file holds them
#' @param .tab Tibble from geo_release_places().
#' @param .n Integer. Documents drawn.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: every place row of the drawn documents, with Doc added for display.
geo_release_sample <- function(.tab, .n = 4L, .seed = 42L) {
  if (FALSE) {
    .tab  <- tab_release
    .n    <- 4L
    .seed <- 42L
  }

  # ONE DRAWN DOCUMENT CARRIES A REFUSAL, deliberately. A law-clause place and a place beyond the
  # reach are the two rows a reader is most likely to filter away by accident.
  held_ <- unique(.tab$DocID[.tab$InLawClause])
  pick_held_ <- if (length(held_) > 0L) withr::with_seed(.seed, sample(held_, size = 1L)) else
    character(0)
  rest_ <- setdiff(unique(.tab$DocID), pick_held_)
  pick_rest_ <- withr::with_seed(
    .seed, sample(rest_, size = min(max(.n - length(pick_held_), 0L), length(rest_)))
  )

  .tab |>
    dplyr::filter(.data$DocID %in% c(pick_held_, pick_rest_)) |>
    dplyr::arrange(.data$DocID, .data$PlaceStart) |>
    dplyr::mutate(Doc = dplyr::dense_rank(.data$DocID), .before = 1L)
}


# 8. Evidence for the reach ----------------------------------------------------------------------------------------------
#
# THE SWEEP READS THE CHAIN'S INTERMEDIATES AND THAT IS DELIBERATE. It re-attaches at reaches the
# release does not use, so there is no file for it to read. Nothing here has to survive to corpus
# scale.

#' What each candidate reach would have attached
#'
#' EVIDENCE, NOT A CHOICE. The released rule uses one reach; this says what the others would give. The
#' resolution is reach-free, so the places are resolved once and each reach only re-attaches.
#'
#' @param .geo Tibble of places, already stripped of governing-law places.
#' @param .party Tibble read from 04B1's parties_org.parquet.
#' @param .mentions Tibble read from 04B1's mentions_org.parquet.
#' @param .keys Tibble from ent_anchor_keys().
#' @param .reaches Numeric vector of reaches to score.
#' @param .quiet Logical. Suppress the progress bar.
#' @return Tibble: one row per reach.
geo_sweep_reach <- function(.geo, .party, .mentions, .keys, .inside,
                            .reaches = c(100, 200, 400), .quiet = FALSE) {
  if (FALSE) {
    .geo      <- tab_free
    .party    <- tab_party_facts
    .mentions <- tab_mentions
    .keys     <- tab_keys
    .inside   <- tab_inside
    .reaches  <- c(100, 200, 400)
    .quiet    <- FALSE
  }

  n_ <- dplyr::n_distinct(.keys$DocID)

  # SCORED THROUGH THE RELEASE, because there is no per-party table to build any more. Each arm
  # re-attaches at its own reach, assembles the same place file the release uses, and collapses it
  # with the same function -- so an arm is scored by exactly the code that scores the released rule
  # and a difference between arms cannot come from a difference in how they were counted.
  purrr::map(.reaches, function(.r) {
    spec_ <- geo_spec(.reach = .r)
    rel_  <- geo_release_places(
      .places = .geo,
      .free   = geo_attach(.geo = .geo, .mentions = .mentions, .spec = spec_),
      .inside = .inside
    )
    tab_ <- geo_collapse(.release = rel_, .party = .party) |>
      dplyr::filter(.data$PartyRole != "none")

    # TWO EDGAR COLUMNS, AND ONLY THE SECOND CAN DISCIPLINE THE REACH. PctEdgar asks whether ANY
    # attached place of the registrant appears in the recorded address, so widening the reach only
    # adds places and the share can only rise -- a measure that cannot fall cannot say a reach is too
    # wide. PctPlace is the share of ATTACHED PLACES that appear in the address, and it falls as soon
    # as the extra ones are not the company's location. Coverage rises with the reach by
    # construction; this is what has to be read against it.
    edg_  <- geo_edgar_check(.release = rel_, .party = .party, .keys = .keys)
    hits_ <- .geo_addr_hits(.release = rel_, .party = .party, .keys = .keys)

    tibble::tibble(
      Reach    = .r,
      Spec     = spec_$Label,
      PctState = mean(!is.na(tab_$GeoState)),
      PctCount = .geo_share_or_na(
        .x = !is.na(tab_$GeoState[tab_$PartyRole == "counterparty"])
      ),
      PctAny   = mean(!is.na(tab_$GeoState) | !is.na(tab_$GeoCountry)),
      PctEdgar = .geo_share_or_na(.x = edg_$EdgarAgrees),
      PctPlace = .geo_share_or_na(.x = hits_$Hit),
      NPlaceReg = nrow(hits_),
      Docs     = n_
    )
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


#' How far each level of place sits from the party it attached to
#'
#' THE ARGUMENT FOR ONE NUMBER, AND ITS WEAKNESS. A state attaches at a handful of characters and a
#' city at scores of them, so 200 is generous for one and about right for the other. Reporting the
#' distribution per level is what makes that visible rather than buried in a single mean.
#'
#' @param .roles Tibble from geo_attach().
#' @return Tibble: one row per level.
geo_dist_profile <- function(.roles) {
  if (FALSE) .roles <- tab_roles

  .roles |>
    dplyr::filter(.data$Attached) |>
    dplyr::summarise(
      Places = dplyr::n(),
      P25    = stats::quantile(.data$DistToParty, 0.25, names = FALSE),
      P50    = stats::quantile(.data$DistToParty, 0.50, names = FALSE),
      P90    = stats::quantile(.data$DistToParty, 0.90, names = FALSE),
      Max    = max(.data$DistToParty),
      .by = GeoLevel
    ) |>
    dplyr::arrange(plot_factor(.data$GeoLevel, .key = "GeoLevel"))
}


#' A share, or missing where there is nothing to take a share of
#'
#' @param .x Logical vector, possibly empty.
#' @return The mean, or NA_real_ where .x is empty.
.geo_share_or_na <- function(.x) {
  if (FALSE) .x <- logical(0)
  if (length(.x) == 0L) NA_real_ else mean(.x)
}


#' What the gazetteer resolved, one row per level
#' @param .geo Tibble from geo_country().
#' @return Tibble: one row per level, and one for every place.
geo_table_resolve <- function(.geo) {
  if (FALSE) .geo <- tab_places

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Places    = dplyr::n(),
      Docs      = dplyr::n_distinct(.data$DocID),
      PctState  = mean(!is.na(.data$State)),
      PctCounty = mean(!is.na(.data$County)),
      PctCountry = mean(!is.na(.data$CountryIso)),
      PctAmbig  = mean(.data$Ambiguous),
      .by = dplyr::any_of("GeoLevel")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(.geo), plot_factor(.data$GeoLevel, .key = "GeoLevel")),
    dplyr::mutate(cols_(dplyr::select(.geo, -"GeoLevel")), GeoLevel = "All", .before = 1L)
  )
}


#' How every city got its state
#' @param .geo Tibble from geo_country().
#' @return Tibble: one row per arm.
geo_table_city <- function(.geo) {
  if (FALSE) .geo <- tab_places

  .geo |>
    dplyr::filter(.data$GeoLevel == "City") |>
    dplyr::summarise(Cities = dplyr::n(), Docs = dplyr::n_distinct(.data$DocID),
                     .by = GeoStateFrom) |>
    dplyr::mutate(Share = .data$Cities / sum(.data$Cities)) |>
    dplyr::arrange(plot_factor(.data$GeoStateFrom, .key = "GeoStateFrom"))
}


#' What the governing-law exclusion withheld
#' @param .places Tibble from geo_country().
#' @param .inside Tibble from geo_law_inside().
#' @return One-row tibble.
geo_table_exclude <- function(.places, .inside) {
  if (FALSE) {
    .places <- tab_places
    .inside <- tab_inside
  }

  tibble::tibble(
    Item = c("Places found",
             "Inside a governing-law clause",
             "Of those, a state or a country",
             "Documents with at least one"),
    N    = c(nrow(.places),
             nrow(.inside),
             sum(.inside$Start %in% .places$Start[.places$GeoLevel %in% c("State", "Country")]),
             dplyr::n_distinct(.inside$DocID))
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$N / pmax(nrow(.places), 1L)))
}


#' What the gazetteer resolved
#' @param .tab Tibble from geo_table_resolve().
#' @return Invisibly .tab.
geo_report_resolve <- function(.tab) {
  if (FALSE) .tab <- tab_resolve

  cli::cli_h2("What the gazetteer resolved")
  .tab |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "One row per level of place, and one for every place found")

  cli::cli_alert_info(
    "A STATE, COUNTY OR CITY IS IN THE UNITED STATES BY CONSTRUCTION, so PctCountry is 100% on those \\
     three rows and says nothing. Read it on Country and Other: Other is a non-US subdivision -- \\
     Ontario, Guangdong, Scotland -- whose country is recovered from its ISO-3166-2 prefix, because \\
     only 253 of the dictionary's 437 entities carry a three-letter code at all. PctAmbig on the \\
     City row is the population the next table resolves."
  )
  invisible(.tab)
}


#' How every city got its state
#' @param .tab Tibble from geo_table_city().
#' @return Invisibly .tab.
geo_report_city <- function(.tab) {
  if (FALSE) .tab <- tab_city

  cli::cli_h2("How every city got its state")
  .tab |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "One row per arm of the ambiguous-city rule")

  cli::cli_alert_info(
    "UNIQUE is a city name existing in one state. NAMED NEARBY is an ambiguous name resolved to \\
     whichever of ITS OWN states the contract names -- nearest, and where two are equidistant the \\
     one named most often. UNRESOLVED is an ambiguous name whose states the contract never mentions, \\
     and it is left empty on purpose: a fallback would send every unresolved SPRINGFIELD to the same \\
     state, and a constant error reads as a real geographic pattern rather than as noise."
  )
  invisible(.tab)
}


#' What the governing-law exclusion withheld
#' @param .tab Tibble from geo_table_exclude().
#' @return Invisibly .tab.
geo_report_exclude <- function(.tab) {
  if (FALSE) .tab <- tab_exclude

  cli::cli_h2("The governing-law exclusion")
  tbl_say(.tab = .tab, .title = "Places withheld from attachment")

  cli::cli_alert_info(
    "EVERY ONE OF THESE WAS AVAILABLE TO A PARTY BEFORE. A clause falling within reach of a company \\
     name put its jurisdiction in that company's state column, which biased the released geography \\
     toward New York and Delaware -- exactly where a reader would look. The third row is the share \\
     that could have done real damage, because only a state or a country reaches the state column."
  )
  invisible(.tab)
}


#' What the governing-law clause said
#' @param .tab Tibble from geo_table_law().
#' @param .top Tibble from geo_table_law_top().
#' @return Invisibly .tab.
geo_report_law <- function(.tab, .top) {
  if (FALSE) {
    .tab <- tab_law
    .top <- tab_law_top
  }

  cli::cli_h2("The governing law")
  .tab |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "One row per outcome, over every document in the sample")

  .top |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "The commonest jurisdictions, among the documents naming one")

  cli::cli_alert_info(
    "FOUR OUTCOMES AND THE THIRD IS THE ONE TO WATCH. SELF-REFERENTIAL is a clause specifying the \\
     law perfectly well and naming no place -- \"the State in which the Premises are located\" -- so \\
     recording nothing would count a real contract term as a coverage gap. CLAUSE WITHOUT A PLACE is \\
     different: a clause was found and the gazetteer resolved nothing inside it, which is an \\
     extraction gap rather than a drafting fact, and a large share there is a reason to look at the \\
     cue list rather than at the contracts."
  )
  invisible(.tab)
}


#' How far each level of place sits from its party
#' @param .tab Tibble from geo_dist_profile().
#' @return Invisibly .tab.
geo_report_dist <- function(.tab) {
  if (FALSE) .tab <- tab_dist

  cli::cli_h2("How far a place sits from the party it describes")
  tbl_say(.tab = .tab, .title = "Characters between the party's last character and the place's first")

  cli::cli_alert_info(
    "ONE REACH COVERS TWO DIFFERENT DISTANCES. A state follows the name almost immediately -- \\
     \"a Delaware corporation\" -- while a city sits behind an address and further out. 200 is \\
     therefore generous for states and about right for cities, which is a weakness of a single \\
     number and is reported rather than tuned away: a per-level reach would fit better and cost a \\
     paragraph to defend."
  )
  invisible(.tab)
}


#' What each candidate reach would have attached
#' @param .tab Tibble from geo_sweep_reach().
#' @return Invisibly .tab.
geo_report_sweep <- function(.tab) {
  if (FALSE) .tab <- tab_sweep

  cli::cli_h2("The reach, swept")
  .tab |>
    # tbl_pct_safe, not tbl_pct: PctEdgar is a share over the registrants EDGAR carries an address
    # for, and where that is empty "NA%" reads as a computation that went wrong rather than as a
    # measure with nothing to measure.
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct_safe(.x))) |>
    tbl_say(.title = "One row per candidate reach, everything else held")

  cli::cli_alert_info(
    "PctPlace IS THE MEASURE THAT DECIDES THE REACH, and PctEdgar is not. PctEdgar asks whether ANY \\
     attached place of a registrant appears in EDGAR's recorded address, so a wider reach only adds \\
     places and the share can only rise -- it measures coverage wearing a precision label. PctPlace \\
     is the share of the attached PLACES that appear in the address, over NPlaceReg of them, and it \\
     falls as soon as the extra ones are not the company's location."
  )
  cli::cli_alert_info(
    "READ PctPlace AGAINST PctState. Coverage rises with the reach by construction, so a reach is \\
     too wide at the point where the places it adds stop agreeing with EDGAR. Both ceilings are low \\
     and low is not failure: EDGAR records the FIRM's address while a contract often writes a \\
     subsidiary's, a landlord's or the counterparty's."
  )
  invisible(.tab)
}


#' The column dictionary
#' @param .tab Any of this document's dictionary tables.
#' @param .title Character. Heading, since three different dictionaries use this.
#' @return Invisibly .tab.
geo_report_dictionary <- function(.tab, .title = "The columns, in the order the file carries them") {
  if (FALSE) {
    .tab   <- tab_dict
    .title <- "The columns, in the order the file carries them"
  }

  cli::cli_h2(.title)
  tbl_say(
    .tab   = .tab,
    .title = paste0(nrow(.tab), " columns, in the order they appear")
  )

  cli::cli_alert_info(
    "GRAIN SAYS WHERE THE COLUMN BELONGS, and it is the thing a reader most needs from a long file. \\
     A document column repeats across every row of a contract, a party column across every place \\
     that reached that party, and a place column varies row by row. Averaging a party column over \\
     place rows weights it by how many places happened to attach, which is not a quantity anybody \\
     wants. The dictionary is compared with the file's own names, so a column added or renamed \\
     without an entry aborts this chunk rather than leaving the documentation quietly wrong."
  )
  invisible(.tab)
}


# 9. Report --------------------------------------------------------------------------------------------------------------

#' The naive ladder, reported
#' @param .tab Tibble from geo_table_naive().
#' @return Invisibly .tab.
geo_report_naive <- function(.tab) {
  if (FALSE) .tab <- tab_naive

  cli::cli_h2("The rule against the naive ladder")
  .tab |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Mean"), \(.x) tbl_num(.x)),
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))
    ) |>
    tbl_say(.title = "Distinct states per contract, at three rungs")

  cli::cli_alert_info(
    "COUNTS ON BOTH SIDES, so the comparison is like for like. MeanNaive is every distinct state the \\
     contract names anywhere. MeanLawFree drops the ones inside a governing-law clause. MeanRule \\
     counts only the states the rule attached to a COUNTERPARTY -- the registrant's own state is in \\
     EDGAR and does not need extracting."
  )
  cli::cli_alert_info(
    "THE STEP BETWEEN EACH PAIR IS ONE DECISION. Naive to law-free is the exclusion; law-free to \\
     rule is the attachment and the role together."
  )
  cli::cli_alert_info(
    "READ PctLawCut BY CLASS AND NOT IN THE AGGREGATE. The pooled figure is small because a \\
     contract governed by New York law usually names New York somewhere else as well, so removing \\
     the clause rarely removes a DISTINCT STATE from the document -- which is what this ladder \\
     counts. By class the share varies several-fold, and it is largest exactly where a contract \\
     names fewest places and the governing-law state is a large part of the few."
  )
  cli::cli_alert_info(
    "A SMALL GAP HERE IS NOT EVIDENCE THE EXCLUSION IS UNNECESSARY. This ladder counts distinct \\
     states; the exclusion exists to stop a jurisdiction being recorded as a PARTY'S ADDRESS, which \\
     is a question about assignment rather than about counts and which only the collapse can see."
  )
  cli::cli_alert_info(
    "EVERY COLUMN IS A FILTER OVER places_geo.parquet. Nothing here is stored, so a reader who \\
     disagrees with a rung recomputes it rather than taking it."
  )
  invisible(.tab)
}


#' What the two released files hold
#' @param .places Tibble from geo_release_places().
#' @param .law Tibble from geo_release_law().
#' @return Invisibly the summary.
geo_report_release <- function(.places, .law) {
  if (FALSE) {
    .places <- tab_release
    .law    <- tab_law_rel
  }

  cli::cli_h2("The two released files")

  out_ <- tibble::tribble(
    ~File,                   ~Rows,                                 ~Docs,
    "places_geo.parquet",    nrow(.places),                         dplyr::n_distinct(.places$DocID),
    "law_clauses.parquet",   nrow(.law),                            dplyr::n_distinct(.law$DocID)
  ) |>
    dplyr::mutate(dplyr::across(c(Rows, Docs), \(.x) format(.x, big.mark = ",")))

  tbl_say(.tab = out_, .title = "One row per place, and one row per governing-law clause")

  # THE NEW NAMES DO NOT COLLIDE WITH THE SOURCE COLUMNS, and that is not a style choice.
  # summarise() evaluates sequentially, so a column named Attached shadows .data$Attached for every
  # later expression in the same call -- and !34698 is FALSE, which made the refused count zero on a
  # file holding seventy-one thousand of them. It printed as a clean result.
  kept_ <- .places |>
    dplyr::summarise(
      nPlaces   = dplyr::n(),
      nInLaw    = sum(.data$InLawClause),
      nAttached = sum(.data$Attached),
      nRefused  = sum(!.data$Attached & !.data$InLawClause)
    ) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "Outcome", values_to = "Places") |>
    dplyr::mutate(
      Outcome = c("Places found", "Inside a law clause", "Attached to a party",
                  "Offered and beyond the reach"),
      Share   = tbl_pct(.data$Places / max(.data$Places[[1L]], 1L)),
      Places  = format(.data$Places, big.mark = ",")
    )

  tbl_say(.tab = kept_, .title = "What became of every place the extractor proposed")

  cli::cli_alert_info(
    "THE REFUSALS ARE IN THE FILE. A place inside a governing-law clause names the law rather than \\
     an address, and a place beyond the reach belongs to no party -- both are marked rather than \\
     dropped, which is what makes the naive ladder a filter over this file and what lets a reader \\
     see the rule working rather than only its output."
  )
  invisible(out_)
}


#' The collapse, reported
#' @param .tab Tibble from geo_table_cover().
#' @return Invisibly .tab.
geo_report_cover <- function(.tab) {
  if (FALSE) .tab <- tab_cover

  cli::cli_h2("What the collapse gives each party")
  .tab |>
    dplyr::mutate(
      Parties = format(.data$Parties, big.mark = ","),
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct_safe(.x))
    ) |>
    tbl_say(.title = "Coverage by role, from the nearest attached place")

  cli::cli_alert_info(
    "ONE PLACE PER PARTY, SO STATE AND COUNTRY CANNOT DISAGREE. The nearest attached place wins and \\
     both columns are read off it. Choosing them separately is what let an earlier release carry a \\
     Delaware state beside a Texas county."
  )
  cli::cli_alert_info(
    "THE STATE IS THE INCORPORATION STATE WHEREVER A CONTRACT STATES ONE, because incorporation \\
     language sits immediately after a party name and an address does not. That follows from \\
     nearest-wins rather than from a separate rule; anyone wanting location instead should read \\
     GeoFrom and GeoFromLevel, which say which place the geography came from."
  )
  invisible(.tab)
}


#' The place file, read
#' @param .tab Tibble from geo_release_sample().
#' @return Invisibly .tab.
geo_report_release_sample <- function(.tab) {
  if (FALSE) .tab <- tab_sample

  cli::cli_h2("The place file, read")

  .tab |>
    dplyr::select("Doc", "PlaceText", "GeoLevel", "GeoUnit", "GeoState", "GeoStateFrom",
                  "PartyKey", "DistToParty", "Attached", "InLawClause") |>
    tbl_say(.title = "Every place of a few contracts, as the file holds them")

  cli::cli_alert_info(
    "READ Attached AND InLawClause TOGETHER. A row with InLawClause TRUE was never offered to the \\
     attachment, so its null distance is not a missing measurement. A row with Attached FALSE and \\
     InLawClause FALSE was offered and refused: no party mention within reach."
  )
  invisible(.tab)
}


#' The rule in one table
#' @param .doc Tibble from geo_doc_facts().
#' @param .collapse Tibble from geo_collapse().
#' @param .places Tibble from geo_release_places().
#' @return Invisibly the table.
geo_report_headline <- function(.doc, .collapse, .places) {
  if (FALSE) {
    .doc      <- tab_doc
    .collapse <- tab_collapse
    .places   <- tab_release
  }

  cli::cli_h2("The rule in one table")

  party_ <- dplyr::filter(.collapse, .data$PartyRole != "none")
  cnt_   <- dplyr::filter(party_, .data$PartyRole == "counterparty")

  out_ <- tibble::tribble(
    ~Item,                                       ~Value,
    "Contracts",                                 format(nrow(.doc), big.mark = ","),
    "Places found",                              format(nrow(.places), big.mark = ","),
    "Inside a governing-law clause",             tbl_pct(mean(.places$InLawClause)),
    "Attached to a party",                       tbl_pct(mean(.places$Attached)),
    "Contracts naming any place",                tbl_pct(mean(.doc$NPlaces > 0L)),
    "States named per contract, naive",          tbl_num(mean(.doc$NaiveStates)),
    "States named, outside a law clause",        tbl_num(mean(.doc$LawFreeStates)),
    "States the rule gives a counterparty",      tbl_num(mean(.doc$RuleStates)),
    "Parties given a state",                     tbl_pct(mean(!is.na(party_$GeoState))),
    "Counterparties given a state",              tbl_pct_safe(mean(!is.na(cnt_$GeoState))),
    "Parties given a country",                   tbl_pct(mean(!is.na(party_$GeoCountry)))
  )

  tbl_say(.tab = out_, .title = "Everything this document decided")

  cli::cli_alert_info(
    "THE THREE STATE ROWS ARE THE RESULT, and they are a ladder: what the contract names, what \\
     survives the governing-law exclusion, and what the rule assigns to the other side of the \\
     contract. Every other row says how far that answer can be trusted."
  )
  invisible(out_)
}


# 10. Figures ------------------------------------------------------------------------------------------------------------

#' The ladder, by contract type
#' @param .tab Tibble from geo_table_naive().
#' @return A ggplot.
geo_plot_naive <- function(.tab) {
  if (FALSE) .tab <- tab_naive

  .tab |>
    dplyr::filter(.data$Class != "All") |>
    dplyr::select("Class", Naive = "MeanNaive", `Law-free` = "MeanLawFree", Rule = "MeanRule") |>
    tidyr::pivot_longer(cols = -"Class", names_to = "Measure", values_to = "Mean") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Mean,
                                 y = stats::reorder(.data$Class, .data$Mean),
                                 fill = .data$Measure)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
    plot_scale_fill_cat(name = NULL) +
    plot_scale_x_count() +
    ggplot2::labs(x = "Distinct states per contract", y = NULL) +
    plot_theme(.grid = "x", .legend = "bottom")
}


#' Coverage by role, from the collapse
#' @param .tab Tibble from geo_table_cover().
#' @return A ggplot.
geo_plot_cover <- function(.tab) {
  if (FALSE) .tab <- tab_cover

  .tab |>
    dplyr::filter(.data$PartyRole != "All") |>
    dplyr::select("PartyRole", State = "PctState", Country = "PctCountry") |>
    tidyr::pivot_longer(cols = -"PartyRole", names_to = "Measure", values_to = "Share") |>
    plot_bar_stacked(
      .cat   = "PartyRole",
      .val   = "Share",
      .fill  = "Measure",
      .key   = "PartyRole",   # orders the categories by the registered role vocabulary
      .short = TRUE
    ) +
    ggplot2::labs(x = "Share of parties given a place")
}


#' How every city got its state
#' @param .tab Tibble from geo_table_city().
#' @return A ggplot.
geo_plot_city <- function(.tab) {
  if (FALSE) .tab <- tab_city

  plot_bar_ranked(
    .tab      = .tab,
    .cat      = "GeoStateFrom",
    .val      = "Share",
    .key      = "GeoStateFrom",
    .short    = TRUE,
    .pct      = TRUE,
    .accuracy = 1
  ) +
    ggplot2::labs(x = "Share of city mentions")
}


#' What each candidate reach would have attached
#' @param .tab Tibble from geo_sweep_reach().
#' @return A ggplot.
geo_plot_sweep <- function(.tab) {
  if (FALSE) .tab <- tab_sweep

  .tab |>
    dplyr::select("Reach", Placed = "PctAny", Agrees = "PctEdgar") |>
    tidyr::pivot_longer(cols = c("Placed", "Agrees"), names_to = "Measure",
                        values_to = "Share") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Reach, y = .data$Share, colour = .data$Measure)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.8) +
    plot_scale_colour_cat(name = NULL) +
    plot_scale_y_pct(.accuracy = 1) +
    ggplot2::labs(x = "Reach in characters", y = "Share") +
    plot_theme(.grid = "y", .legend = "bottom")
}


#' The commonest governing-law jurisdictions
#' @param .tab Tibble from geo_table_law_top().
#' @return A ggplot.
geo_plot_law <- function(.tab) {
  if (FALSE) .tab <- tab_law_top

  plot_bar_ranked(
    .tab      = .tab,
    .cat      = "Jurisdiction",
    .val      = "Share",
    .pct      = TRUE,
    .accuracy = 1
  ) +
    ggplot2::labs(x = "Share of clauses naming a jurisdiction")
}


