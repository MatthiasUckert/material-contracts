# 04B2-Rules-GPE: where each organisation is -------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# 04B1 found a contracting party in each contract and labelled every other organisation beside it.
# This file answers one question about each of them: WHERE IS IT. Function prefix is `geo_`, so
# nothing here can collide with the `ent_` layer that 03A, 04A and _Entity.R share.
#
# THE QUESTION A REFEREE ASKED
# The published paper reports 1.9 geoentities per contract and a figure of occurrence by continent.
# Referee 1: "it is unclear whether the countries mentioned are the locations of the contracting
# entities... it seems possible that mentioned countries may simply be mentioned in contract
# pertaining to some planned business activity." That is right. The answer is not to find more
# places, it is to attach the ones already found to the organisation they belong to.
#
# THE GRAIN IS ONE ROW PER ORGANISATION, not one per document. A document-level variable is then a
# filter on this table rather than a separate computation, and "which party is this the address of"
# stops being a question the release cannot answer. Every organisation is computed; filer and
# counterparty are a column, exactly as in 04B1.
#
# ONE RULE: THE PLACE NEAREST AN ORGANISATION IS THAT ORGANISATION'S PLACE
# Search backwards, because a preamble names the company and then qualifies it -- "X, a Delaware
# corporation, with offices at 100 Main Street, Springfield, Illinois". Stop at the first
# organisation. Refuse beyond the reach. That is the whole of it.
#
# ONE CUE, AND ONLY BECAUSE PROXIMITY CANNOT REACH IT
# A governing-law clause sits nowhere near a party: 95% of the spans its cue catches are attached to
# no organisation at all. An earlier version also carried incorporation and location cues, which
# fired beside a party that proximity had already caught -- and, because the cue outranked proximity,
# assigned a role to 14,227 spans attached to nothing. Those two families are gone. One signal per
# question.
#
# CITIES ARE AN INPUT, NOT AN OUTPUT
# A city resolves to its state and the state is released; no reported variable is a city. That is the
# only way the location variable can be read against the incorporation variable, because they then
# live on the same tier. Cities stay in the span-level artifact, because they are what validates
# against EDGAR's registered address -- 45.0% at city level against 3.2% at state level, which is the
# separation this whole document rests on.
#
# NOISE IS EXPECTED AND IS NOT ELIMINATED. No place-name stoplist. No check that a city beside a
# party is the party's own rather than its lawyer's. Every table carries the unadjusted count -- every
# distinct place the contract names anywhere -- so a reader sees what the rule removed.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_text   <- .lP$Input$Text
  .path_lookup <- .lP$Input$Lookup
  .path_org    <- .lP$Input$Org
  .dir_store   <- .lP$Input$Store
}


# 1. Vocabulary ------------------------------------------------------------------------------------------------------

plot_register_levels(
  .key    = "GeoLevel",
  .levels = c("Country", "State", "County", "City", "Other"),
  .short  = c("country", "state", "county", "city", "other")
)

plot_register_levels(
  .key    = "GeoHow",
  .levels = c("unique", "nearest named", "unresolved", "not a city"),
  .short  = c("unique", "nearest", "unresolved", "n/a")
)

# THE FAMILY IS THE PRODUCER NOW, and the level is named for it. "gazetteer" was the engine token of
# a flat store that no longer exists; matcon is the family whose gazetteer-v2 model emits these rows,
# and calling it anything else would put a name in the figures that appears nowhere in the store.
plot_register_levels(
  .key    = "GeoEngine",
  .levels = c("lexnlp", "matcon"),
  .short  = c("lexnlp", "matcon")
)


# 2. The one cue -----------------------------------------------------------------------------------------------------
# Governing law only. A clause states which law applies and names a jurisdiction nowhere near a
# party, so no proximity rule can find it; every other role this document assigns comes from
# proximity alone.
#
# The self-referential form is kept as its own outcome. "governed by the laws of the State in which
# the Premises are located" specifies the law perfectly well and names no place, so recording nothing
# would count a real contract term as a coverage gap.

.geo_law_cue <- c("GOVERNED BY AND CONSTRUED", "GOVERNED BY", "LAWS OF THE STATE OF",
                  "LAWS OF THE COMMONWEALTH OF", "CONSTRUED IN ACCORDANCE WITH THE LAWS",
                  "GOVERNING LAW", "SUBMIT TO THE JURISDICTION OF")

.geo_law_self <- c("STATE IN WHICH", "JURISDICTION IN WHICH", "STATE WHERE THE",
                   "LAWS OF THE JURISDICTION IN WHICH")


# 3. Resolution ------------------------------------------------------------------------------------------------------

#' Read the two columns the gazetteer does not emit
#'
#' MOST OF THIS JOIN HAS GONE AWAY, and that is gazetteer-v2 rather than a simplification made here.
#' The extractor now emits GeoKey, NParent, IsWord, Iso2 and Iso3 on every row it writes, so the
#' lookup is needed for the two columns it does not: the state a place sits in and that state's
#' county. Under the flat store none of those arrived and all five were recovered by joining here.
#'
#' READING THE PACKAGE'S OWN DATA FILE IS CROSSING THE CLI SEAM, and it is safe for a specific
#' reason rather than by convention. gazetteer.py's spec() folds a content hash of this exact file
#' into gazetteer-v2's spec hash, so a lookup that changed moves the model's hash and
#' ner_manifest_write() aborts on the next ingest. The seam is crossed and guarded: this document and
#' the store cannot silently disagree about which lookup they used.
#'
#' EVERY COLUMN IS PREFIXED Lk. The gpe table carries its own Iso2 and Iso3, so an unprefixed join
#' would suffix both sides to .x and .y and every later reference would silently read whichever one
#' dplyr happened to put first.
#'
#' @param .path_lookup The gazetteer's geo_lookup.parquet, inside the matcon-extract package.
#' @return Tibble: GeoKey, GeoClass and the two parent columns, prefixed Lk.
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
#' states, and geo_lookup() answers that by nulling the parents. This keeps the candidate list
#' instead, so a city can be resolved by asking WHICH OF ITS CANDIDATE STATES THE CONTRACT NAMES --
#' which is the same evidence the extractor already uses to admit a word-like name at all.
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


#' Put one family's GPE spans on the shared hierarchy
#'
#' TWO PRODUCERS AT DIFFERENT TIERS, not two opinions about one. matcon's gazetteer carries 116,000
#' entries and reaches cities and counties; LexNLP carries 437 and reaches countries and first-level
#' subdivisions and stops. 04A measured the consequence: they agree on 44.5% of mentions and, where
#' they do meet, on boundaries 97.6% of the time. Stacked and marked, never pooled inside a table.
#'
#' MATCON NOW ARRIVES PART-RESOLVED. gazetteer-v2 emits GeoKey, NParent, IsWord and two ISO columns
#' itself, so only the state and county come from the lookup. Its class is still in the core column
#' LabelRaw, which is what the lookup is keyed on beside the name.
#'
#' ITS ISO COLUMNS DO NOT MEAN WHAT LEXNLP'S DO, which is why _Entity.R renames them on read. matcon
#' resolves a country code on EVERY level -- USA on a state, a city, a county -- and a subdivision
#' code on everything BUT a country. LexNLP is the other way round: a country code on countries and a
#' code that may be either elsewhere. Read under one pair of names the two disagree silently, and the
#' first version of this document did exactly that: it came out right because geo_country() reads the
#' country column first and matcon's happened to be one, while the subdivision-prefix recovery sat
#' dead at 0.0% of matcon's rows.
#'
#' LkState, NOT the lookup's ParentState: a state's parent is not itself, so ParentState is NA on
#' every US State row, and reading it once reported 179 states at country level. Iso3 is guarded
#' against the literal string "nan", which an earlier extractor wrote and which reads as a country
#' code rather than as missing.
#'
#' NON-US SUBDIVISIONS -- Ontario, Guangdong, Scotland -- are levelled "Other". They carry no
#' ISO-3166-3 at all, and that is a fact about the dictionary rather than about the extractor: of its
#' 437 entities only the 253 countries have a three-letter code. Their country is recovered in
#' geo_country() from the ISO-3166-2 prefix instead.
#'
#' @param .spans Tibble from ent_load_entity() for one family.
#' @param .lookup Tibble from geo_lookup(). Ignored for lexnlp.
#' @param .family Character, "lexnlp" or "matcon".
#' @return .spans reduced to the shared columns.
geo_resolve <- function(.spans, .lookup, .family) {
  if (FALSE) {
    .spans  <- lst_raw[["matcon"]]
    .lookup <- tab_lookup
    .family <- "matcon"
  }

  out_ <- if (identical(.family, "matcon")) {
    .spans |>
      dplyr::mutate(GeoKeyUp = stringi::stri_trans_toupper(.data$GeoKey)) |>
      dplyr::left_join(.lookup, by = dplyr::join_by(GeoKeyUp == GeoKey, LabelRaw == LkClass)) |>
      dplyr::mutate(
        GeoKey     = .data$GeoKeyUp,
        GeoUnit    = stringi::stri_trans_totitle(.data$GeoKeyUp),
        RawClass   = .data$LabelRaw,
        NParentOut = dplyr::coalesce(as.integer(.data$NParent), 1L),
        Ambig      = !is.na(.data$NParent) & .data$NParent > 1L,
        # matcon's CountryIso3 is a country code on every level -- USA on a state, a city and a
        # county -- and its SubIso is a subdivision code and never a country. The two are mapped
        # onto the shared columns here, which is the only place their meaning is known.
        Iso3Out    = dplyr::na_if(.data$CountryIso3, "nan"),
        Iso2Out    = dplyr::if_else(.data$Ambig, NA_character_,
                                    dplyr::na_if(.data$SubIso, "nan")),
        StateOut   = dplyr::if_else(.data$Ambig, NA_character_,
                                    stringi::stri_trans_toupper(.data$LkState)),
        CountyOut  = dplyr::if_else(.data$Ambig, NA_character_, .data$LkCounty),
        WordLike   = as.integer(.data$IsWord)
      )
  } else {
    .spans |>
      dplyr::mutate(
        GeoKey     = stringi::stri_trans_toupper(dplyr::coalesce(.data$GeoName, .data$Span)),
        GeoUnit    = dplyr::coalesce(.data$GeoName, .data$Span),
        RawClass   = .data$GeoCategory,
        NParentOut = 1L,
        Ambig      = FALSE,
        Iso3Out    = dplyr::na_if(.data$Iso3, "nan"),
        Iso2Out    = dplyr::na_if(.data$Iso2, "nan"),
        StateOut   = dplyr::if_else(.data$GeoCategory == "US States",
                                    stringi::stri_trans_toupper(.data$GeoUnit), NA_character_),
        CountyOut  = NA_character_,
        WordLike   = NA_integer_
      )
  }

  # THE CATEGORY STRINGS ARE LEXNLP'S OWN, verified against the store rather than assumed: Countries,
  # US States, and eight subdivision categories -- Canadian Provinces, Chinese Provinces, UK
  # Countries/Provinces, Mexico States, German States, Australian States, French States, Spanish
  # Autonomies. No Cities and no Counties, because the dictionary has neither. Those two arms below
  # are live for matcon alone.
  out_ |>
    dplyr::mutate(
      GeoLevel = dplyr::case_when(
        .data$RawClass %in% c("Country", "Countries")                  ~ "Country",
        .data$RawClass %in% c("US State", "US States")                 ~ "State",
        .data$RawClass %in% c("US County", "Counties")                 ~ "County",
        stringi::stri_detect_fixed(dplyr::coalesce(.data$RawClass, ""),
                                   "Populated Place")                  ~ "City",
        .data$RawClass %in% c("Cities")                                ~ "City",
        .default                                                       = "Other"
      )
    ) |>
    dplyr::select("DocID", "Start", "Stop", "Span", "DocLen", "GeoKey", "GeoUnit", "GeoLevel",
                  County = "CountyOut", State = "StateOut", Iso2 = "Iso2Out", Iso3 = "Iso3Out",
                  "RawClass", NParent = "NParentOut", Ambiguous = "Ambig", "WordLike")
}


# 4. Resolving a city to a state ---------------------------------------------------------------------------------------

#' Give every city a state
#'
#' THREE OUTCOMES, ALL REPORTED. A city whose name exists in one state takes it. A city whose name is
#' ambiguous takes whichever of its candidate states the CONTRACT ITSELF NAMES, nearest to the city.
#' A city whose candidates the contract never names keeps no state.
#'
#' The constraint on the second arm is what makes it safe. Taking the nearest state span outright
#' would put Springfield in Delaware whenever the party happens to be a Delaware corporation;
#' requiring that a Springfield exist in that state does not. It uses no new data -- the candidate
#' list is in the lookup already -- and it is the same evidence the extractor relies on when it
#' admits a word-like city only where a state anchor sits within forty characters.
#'
#' @param .geo Tibble from geo_resolve(), stacked over engines.
#' @param .cand Tibble from geo_candidates().
#' @return .geo with State and County filled where they can be, plus GeoHow recording which arm.
geo_city_state <- function(.geo, .cand) {
  if (FALSE) {
    .geo  <- tab_geo
    .cand <- tab_cand
  }

  # Every US state the contract names, with its position, from either engine.
  states_ <- .geo |>
    dplyr::filter(.data$GeoLevel == "State", !is.na(.data$State)) |>
    dplyr::distinct(.data$DocID, .data$State, .data$Start) |>
    dplyr::rename(NamedState = State, StateAt = Start)

  # Ambiguous cities, crossed with the states they could be in AND the contract names.
  fixed_ <- .geo |>
    dplyr::filter(.data$GeoLevel == "City", .data$Ambiguous) |>
    dplyr::select(DocID, Start, GeoKey) |>
    dplyr::inner_join(.cand, by = dplyr::join_by(GeoKey), relationship = "many-to-many") |>
    dplyr::inner_join(states_, by = dplyr::join_by(DocID, CandState == NamedState),
                      relationship = "many-to-many") |>
    dplyr::mutate(Gap = abs(.data$StateAt - .data$Start)) |>
    dplyr::arrange(.data$DocID, .data$Start, .data$Gap) |>
    dplyr::slice_head(n = 1L, by = c(DocID, Start)) |>
    dplyr::select(DocID, Start, FixState = CandState, FixCounty = CandCounty)

  .geo |>
    dplyr::left_join(fixed_, by = dplyr::join_by(DocID, Start)) |>
    dplyr::mutate(
      GeoHow = dplyr::case_when(
        .data$GeoLevel != "City"                     ~ "not a city",
        !.data$Ambiguous & !is.na(.data$State)       ~ "unique",
        !is.na(.data$FixState)                       ~ "nearest named",
        TRUE                                         ~ "unresolved"
      ),
      State  = dplyr::coalesce(.data$State,  .data$FixState),
      County = dplyr::coalesce(.data$County, .data$FixCounty)
    ) |>
    dplyr::select(-FixState, -FixCounty)
}


#' Give every place a country
#'
#' THE GAZETTEER'S Iso2 IS A US STATE CODE, NOT A COUNTRY CODE. Reading it as one is why the country
#' variable came back empty for that engine on the first render -- state rows carried it at 99.9% and
#' country rows at 0.0%. ISO alpha-3 is the only country key both engines produce, so it is the join
#' key everywhere below and on the maps.
#'
#' A state, county or city is in the United States by construction: the gazetteer's classes are US
#' tiers and lexnlp's states are US states. A country is its own. A subdivision the four levels have
#' no tier for -- Ontario, Guangdong, Scotland -- keeps the ISO code its engine resolved, which is
#' what lets it roll to a country without being reported at a level that does not exist here.
#'
#' @param .geo Tibble from geo_city_state().
#' @return .geo with CountryIso and CountryName added.
geo_country <- function(.geo) {
  if (FALSE) .geo <- tab_geo

  # THE ISO-3166-2 PREFIX IS FREE COUNTRY INFORMATION, and the first version threw it away. It levels
  # the eight non-US subdivision categories as "Other" and its comment claimed they "keep their ISO
  # code, so they roll to a country" -- they do not. Of the geo dictionary's 437 entities only the
  # 253 countries carry an ISO-3166-3, so Iso3 is NA on every Ontario, Guangdong and Scotland, and
  # CountryIso came out missing for all of them.
  #
  # But their ISO-3166-2 is a SUBDIVISION code -- CA-AB, CN-13, GB-ENG -- whose prefix is exactly the
  # country. Splitting on the hyphen recovers it. A country's own Iso2 has no hyphen and is left
  # alone, which is why the split is guarded rather than applied to every row.
  #
  # IN PRACTICE THIS ARM IS LEXNLP'S. matcon already resolves a country code on every level it emits,
  # so it reaches the second branch and never the third; LexNLP reaches the third on the 3,444 spans
  # its dictionary levels as subdivisions. CountryFrom records which branch each span took, so the
  # split is visible in the render rather than assumed here.
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
      # name, and inventing one here would put a value in the release that no source produced --
      # geo_map_input() draws on CountryIso, which is what the recovery is for.
      CountryName = dplyr::case_when(
        .data$GeoLevel %in% c("State", "County", "City") ~ "United States",
        .data$GeoLevel == "Country"                      ~ .data$GeoUnit,
        .default                                         = NA_character_
      ),
      CountryFrom = dplyr::case_when(
        .data$GeoLevel %in% c("State", "County", "City") ~ "US level",
        !is.na(.data$Iso3)                               ~ "ISO-3166-3",
        !is.na(.data$PrefixIso)                          ~ "ISO-3166-2 prefix",
        .default                                         = "none"
      )
    ) |>
    dplyr::select(-"PrefixIso")
}


# 5. Attachment --------------------------------------------------------------------------------------------------------

#' Build one attachment specification
#'
#' @param .reach Numeric. Furthest an organisation may be and still claim a place. ONE NUMBER for
#'   every level: states attach at a median of five characters and cities at seventy-six, so a
#'   per-level reach would fit the data better and cost a paragraph in the paper to defend. It is
#'   swept as a single alternative row instead, and the registered-address check decides.
#' @param .reach_city Numeric or NULL. When given, cities use this reach and everything else uses
#'   .reach -- the per-level variant, reported rather than adopted.
#' @param .party_only Logical. Attach only to a filer or counterparty, ignoring the organisations
#'   04B1 marked signatory or other. FALSE by default: every organisation gets its geography and the
#'   role is a column, so any later cut is a filter rather than a different rule.
#' @param .label Character or NULL. Overrides the generated label.
#' @return A named list carrying the attachment specification.
geo_spec <- function(.reach = 200, .reach_city = NULL, .party_only = FALSE, .label = NULL) {
  if (FALSE) {
    .reach      <- 200
    .reach_city <- NULL
    .party_only <- FALSE
    .label      <- NULL
  }

  lab_ <- if (!is.null(.label)) {
    .label
  } else if (!is.null(.reach_city)) {
    paste0(format(.reach, big.mark = ","), "/", format(.reach_city, big.mark = ","))
  } else {
    paste0(format(.reach, big.mark = ","), if (.party_only) " party" else "")
  }

  list(Reach = .reach, ReachCity = .reach_city, PartyOnly = isTRUE(.party_only), Label = lab_)
}


#' Attach every place to the organisation before it
#'
#' STACK AND FILL, NOT A ROLLING JOIN. Places and organisations are interleaved by offset and the
#' organisation columns carried DOWN, so each place picks up the one immediately preceding it. A
#' rolling join on the closest match returns more than one row wherever two organisations tie on
#' position; this returns exactly one row per place by construction, and it is joined back on an
#' explicit row id rather than on order.
#'
#' BACKWARDS ONLY. A preamble names the company and then qualifies it, so the place that describes an
#' organisation follows it. Sorting puts an organisation before a place at the same offset, so a
#' place inside a company name attaches to that company.
#'
#' @param .geo Tibble from geo_city_state().
#' @param .org Tibble read from 04B1's roles_org.parquet.
#' @param .spec List from geo_spec().
#' @return .geo with OrgKey, OrgName, OrgRole, OrgStart, DistToOrg and Attached added.
geo_attach <- function(.geo, .org, .spec) {
  if (FALSE) {
    .geo  <- tab_geo
    .org  <- tab_org
    .spec <- .lP$Params$Spec
  }

  org_ <- if (.spec$PartyOnly) {
    dplyr::filter(.org, .data$Role %in% c("filer", "counterparty"))
  } else {
    .org
  }

  base_ <- dplyr::mutate(.geo, RowId = dplyr::row_number())

  g_ <- base_ |>
    dplyr::transmute(DocID, Pos = .data$Start, IsGeo = TRUE, RowId,
                     OKey = NA_character_, OName = NA_character_, ORole = NA_character_,
                     OStart = NA_integer_, OStop = NA_integer_)

  o_ <- org_ |>
    dplyr::transmute(DocID, Pos = .data$Start, IsGeo = FALSE, RowId = NA_integer_,
                     OKey = .data$Key, OName = .data$Name, ORole = .data$Role,
                     OStart = .data$Start, OStop = .data$Stop)

  near_ <- dplyr::bind_rows(o_, g_) |>
    # FALSE sorts before TRUE, so an organisation at the same offset counts as preceding.
    dplyr::arrange(.data$DocID, .data$Pos, .data$IsGeo) |>
    dplyr::group_by(.data$DocID) |>
    tidyr::fill(OKey, OName, ORole, OStart, OStop, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::filter(.data$IsGeo) |>
    dplyr::select(RowId, OrgKey = OKey, OrgName = OName, OrgRole = ORole,
                  OrgStart = OStart, OrgStop = OStop)

  base_ |>
    dplyr::left_join(near_, by = dplyr::join_by(RowId)) |>
    dplyr::mutate(
      # Distance between the NEAREST EDGES, so a place immediately following a company name scores a
      # couple of characters rather than the length of the name.
      DistToOrg = .data$Start - .data$OrgStop,
      Reach     = if (is.null(.spec$ReachCity)) {
        .spec$Reach
      } else {
        dplyr::if_else(.data$GeoLevel == "City", .spec$ReachCity, .spec$Reach)
      },
      Attached  = !is.na(.data$DistToOrg) & .data$DistToOrg >= 0 &
                  .data$DistToOrg <= .data$Reach,
      dplyr::across(c(OrgKey, OrgName, OrgRole),
                    \(.x) dplyr::if_else(.data$Attached, .x, NA_character_)),
      OrgStart  = dplyr::if_else(.data$Attached, .data$OrgStart,  NA_integer_),
      DistToOrg = dplyr::if_else(.data$Attached, .data$DistToOrg, NA_integer_)
    ) |>
    dplyr::select(-OrgStop, -RowId)
}


# 6. Governing law -----------------------------------------------------------------------------------------------------

#' Attach the characters before every span, and the ones after the cue terms
#'
#' Indexed rather than joined. A join would materialise the full text once per span -- nearly two
#' hundred thousand rows times a median contract -- while an index into the text vector copies
#' pointers, because an R character vector holds references into one string cache.
#'
#' @param .geo Tibble carrying DocID and Start.
#' @param .path_text 04A's canonical text parquet.
#' @param .win Integer. Characters read before the span.
#' @return .geo with Before added, uppercased and whitespace-collapsed.
geo_context <- function(.geo, .path_text, .win = 120L) {
  if (FALSE) {
    .geo       <- tab_geo
    .path_text <- .lP$Input$Text
    .win       <- 120L
  }

  txt_ <- arrow::read_parquet(.path_text)
  src_ <- txt_$TextRaw[match(.geo$DocID, txt_$DocID)]

  .geo |>
    dplyr::mutate(
      Before = stringi::stri_trans_toupper(
        stringi::stri_replace_all_regex(
          stringi::stri_sub(src_, from = pmax(1L, .data$Start + 1L - .win), to = .data$Start),
          "\\s+", " "
        )
      )
    )
}


#' The governing law of each contract
#'
#' THE ONLY CUE RULE, and it exists because proximity structurally cannot reach this: a
#' governing-law clause names a jurisdiction nowhere near a party, and 95% of the spans this cue
#' catches are attached to no organisation at all.
#'
#' Three outcomes. A jurisdiction is named. The clause is SELF-REFERENTIAL -- "the laws of the State
#' in which the Premises are located" -- which specifies the law perfectly well and names no place,
#' and recording nothing there would count a real contract term as a coverage gap. Or no clause is
#' found at all.
#'
#' @param .geo Tibble from geo_context(), carrying Before.
#' @param .lens Tibble from ent_doc_lens(). Supplies every document, including those with no clause.
#' @param .path_text 04A's canonical text parquet, for the self-referential scan.
#' @return Tibble: one row per document with GeoGoverningLaw, LawLevel and LawSpecified.
geo_law <- function(.geo, .lens, .path_text) {
  if (FALSE) {
    .geo       <- tab_geo
    .lens      <- tab_lens
    .path_text <- .lP$Input$Text
  }

  hit_ <- function(.txt, .terms) {
    Reduce(`|`, lapply(.terms, function(.t) stringi::stri_detect_fixed(.txt, .t)))
  }

  named_ <- .geo |>
    dplyr::filter(.data$GeoLevel %in% c("State", "Country"), hit_(.data$Before, .geo_law_cue)) |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::select(DocID, GeoGoverningLaw = GeoUnit, LawLevel = GeoLevel)

  txt_  <- arrow::read_parquet(.path_text)
  self_ <- txt_ |>
    dplyr::transmute(
      DocID,
      Body     = stringi::stri_trans_toupper(.data$TextRaw),
      LawSelf  = hit_(.data$Body, .geo_law_cue) & hit_(.data$Body, .geo_law_self)
    ) |>
    dplyr::select(DocID, LawSelf)

  .lens |>
    dplyr::select(DocID) |>
    dplyr::left_join(named_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(self_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      LawSelf      = dplyr::coalesce(.data$LawSelf, FALSE),
      LawSpecified = dplyr::case_when(
        !is.na(.data$GeoGoverningLaw) ~ "named",
        .data$LawSelf                 ~ "self-referential",
        TRUE                          ~ "none found"
      )
    )
}


# 7. The released tables -------------------------------------------------------------------------------------------------

#' Where each organisation is -- one row per document, engine and organisation
#'
#' THE RELEASE. Every organisation 04B1 found, with the geography attached to it and the role it was
#' given. A document-level variable is a filter on this table; "which party is this the address of"
#' is a column rather than a question the file cannot answer.
#'
#' Per family, stacked, so lexnlp and matcon are separate rows for the same organisation and a
#' combined view is a choice the reader makes rather than one this file makes.
#'
#' NGeoNear is what distinguishes "nothing was beside it" from "several were and none resolved",
#' which is the difference between an extraction gap and a resolution gap.
#'
#' @param .roles Tibble from geo_attach().
#' @param .org Tibble read from 04B1's roles_org.parquet.
#' @param .keys Tibble from ent_anchor_keys().
#' @return Tibble: one row per DocID, Combo and organisation.
geo_org_table <- function(.roles, .org, .keys) {
  if (FALSE) {
    .roles <- tab_roles
    .org   <- tab_org
    .keys  <- tab_keys
  }

  att_ <- dplyr::filter(.roles, .data$Attached)

  pick_ <- function(.levels, .col, .name) {
    att_ |>
      dplyr::filter(.data$GeoLevel %in% .levels, !is.na(.data[[.col]])) |>
      dplyr::arrange(.data$DocID, .data$Combo, .data$OrgKey, .data$DistToOrg, .data$Start) |>
      dplyr::slice_head(n = 1L, by = c(DocID, Combo, OrgKey)) |>
      dplyr::select(DocID, Combo, OrgKey, dplyr::all_of(.col)) |>
      dplyr::rename_with(.fn = function(.x) .name, .cols = dplyr::all_of(.col))
  }

  near_ <- att_ |>
    dplyr::summarise(
      NGeoNear = dplyr::n_distinct(.data$GeoUnit),
      MedDist  = stats::median(.data$DistToOrg),
      .by = c(DocID, Combo, OrgKey)
    )

  grid_ <- tidyr::expand_grid(
    dplyr::distinct(.roles, .data$Combo),
    dplyr::select(.org, DocID, OrgKey = Key, OrgName = Name, OrgRole = Role, OrgStart = Start)
  )

  grid_ |>
    dplyr::left_join(near_, by = dplyr::join_by(DocID, Combo, OrgKey)) |>
    dplyr::left_join(pick_(c("State"),   "State",   "GeoIncorporation"),
                     by = dplyr::join_by(DocID, Combo, OrgKey)) |>
    dplyr::left_join(pick_(c("City"),    "State",   "GeoLocation"),
                     by = dplyr::join_by(DocID, Combo, OrgKey)) |>
    dplyr::left_join(pick_(c("City"),    "County",  "GeoCounty"),
                     by = dplyr::join_by(DocID, Combo, OrgKey)) |>
    dplyr::left_join(pick_(c("Country", "Other"), "CountryName", "GeoCountry"),
                     by = dplyr::join_by(DocID, Combo, OrgKey)) |>
    dplyr::left_join(pick_(c("Country", "Other"), "CountryIso", "GeoCountryIso"),
                     by = dplyr::join_by(DocID, Combo, OrgKey)) |>
    dplyr::left_join(dplyr::select(.keys, DocID, Class), by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      NGeoNear = as.integer(dplyr::coalesce(.data$NGeoNear, 0L)),
      HasGeo   = !is.na(.data$GeoIncorporation) | !is.na(.data$GeoLocation) |
                 !is.na(.data$GeoCountry)
    ) |>
    dplyr::relocate(Class, .after = DocID)
}


#' The document-level view, and the unadjusted count beside it
#'
#' A FILTER ON THE ORGANISATION TABLE, not a separate computation. The filer's geography is the
#' filer's row; the counterparty columns are counts over the counterparty rows. Governing law and the
#' unadjusted place count are the only genuinely document-level quantities, because a contract term
#' and a count of everything named anywhere belong to no single party.
#'
#' @param .orgtab Tibble from geo_org_table().
#' @param .roles Tibble from geo_attach().
#' @param .law Tibble from geo_law().
#' @param .keys Tibble from ent_anchor_keys().
#' @return Tibble: one row per document per engine.
geo_doc_table <- function(.orgtab, .roles, .law, .keys) {
  if (FALSE) {
    .orgtab <- tab_org_geo
    .roles  <- tab_roles
    .law    <- tab_law
    .keys   <- tab_keys
  }

  filer_ <- .orgtab |>
    dplyr::filter(.data$OrgRole == "filer") |>
    dplyr::slice_head(n = 1L, by = c(DocID, Combo)) |>
    dplyr::select(DocID, Combo, GeoIncorporation, GeoLocation, GeoCounty, GeoCountry,
                  FilerNear = NGeoNear)

  counter_ <- .orgtab |>
    dplyr::filter(.data$OrgRole == "counterparty") |>
    dplyr::summarise(
      NCounter     = dplyr::n(),
      NCounterGeo  = sum(.data$HasGeo),
      NCounterState = dplyr::n_distinct(.data$GeoLocation[!is.na(.data$GeoLocation)]),
      .by = c(DocID, Combo)
    )

  unadj_ <- .roles |>
    dplyr::summarise(
      NGeo         = dplyr::n_distinct(.data$GeoUnit),
      NGeoAttached = dplyr::n_distinct(.data$GeoUnit[.data$Attached]),
      .by = c(DocID, Combo)
    )

  tidyr::expand_grid(
    dplyr::distinct(.roles, .data$Combo),
    dplyr::select(.keys, DocID, Class, AmendType)
  ) |>
    dplyr::left_join(filer_,   by = dplyr::join_by(DocID, Combo)) |>
    dplyr::left_join(counter_, by = dplyr::join_by(DocID, Combo)) |>
    dplyr::left_join(unadj_,   by = dplyr::join_by(DocID, Combo)) |>
    dplyr::left_join(.law,     by = dplyr::join_by(DocID)) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("N"), \(.x) as.integer(dplyr::coalesce(.x, 0L))))
}


# 8. Validation and sweeps -----------------------------------------------------------------------------------------------

#' Does a place attached to the FILER appear in the address EDGAR records
#'
#' THE ONLY PRECISION MEASURE IN THIS DOCUMENT, and the reason cities are kept as an input. The
#' ceiling is low and low is not failure: most registrants are incorporated in Delaware and located
#' elsewhere, and EDGAR records the FIRM's address rather than the one written into the contract --
#' often a subsidiary's, a landlord's or the counterparty's.
#'
#' Containment on the uppercased address, because EDGAR's address is one free-text field and parsing
#' it into components would be a rule this document does not need.
#'
#' @param .roles Tibble from geo_attach().
#' @param .keys Tibble from ent_anchor_keys().
#' @return Tibble: one row per engine and level.
geo_validate <- function(.roles, .keys) {
  if (FALSE) {
    .roles <- tab_roles
    .keys  <- tab_keys
  }

  if (!"BusinessAddress" %in% names(.keys)) {
    return(tibble::tibble(Combo = character(0), GeoLevel = character(0), Spans = integer(0),
                          NMatch = integer(0), PctMatch = numeric(0)))
  }

  .roles |>
    dplyr::filter(.data$Attached, .data$OrgRole == "filer", !is.na(.data$GeoUnit)) |>
    dplyr::left_join(dplyr::select(.keys, DocID, BusinessAddress), by = dplyr::join_by(DocID)) |>
    dplyr::filter(!is.na(.data$BusinessAddress)) |>
    dplyr::mutate(
      Addr = stringi::stri_trans_toupper(.data$BusinessAddress),
      HitU = stringi::stri_detect_fixed(.data$Addr, stringi::stri_trans_toupper(.data$GeoUnit)),
      HitS = !is.na(.data$State) & stringi::stri_detect_fixed(.data$Addr, .data$State)
    ) |>
    dplyr::summarise(
      Spans      = dplyr::n(),
      PctMatchU  = mean(.data$HitU, na.rm = TRUE),
      PctMatchS  = mean(.data$HitS, na.rm = TRUE),
      .by = c(Combo, GeoLevel)
    ) |>
    dplyr::arrange(.data$Combo, plot_factor(.data$GeoLevel, .key = "GeoLevel"))
}


#' A share, or missing where there is nothing to take a share of
#'
#' mean(logical(0)) is NaN, which tbl_pct renders as "NaN%" and which a reader cannot tell from a
#' computation that went wrong. A level a family never emits has no share, and the honest rendering
#' of that is a dash.
#'
#' @param .x Logical vector, possibly empty.
#' @return The mean, or NA_real_ where .x is empty.
.geo_mean_or_na <- function(.x) {
  if (FALSE) .x <- logical(0)
  if (length(.x) == 0L) NA_real_ else mean(.x)
}


#' Apply every attachment specification, and score each against the registered address
#'
#' The sweep reports the thing a reach can be JUDGED on rather than only the thing it obviously moves.
#' A wider reach always attaches more places; whether the extra ones are right is only visible in the
#' address match.
#'
#' @param .geo Tibble from geo_city_state().
#' @param .org Tibble read from roles_org.parquet.
#' @param .keys Tibble from ent_anchor_keys().
#' @param .specs List of lists from geo_spec().
#' @param .quiet Logical. Suppress the progress bar.
#' @return Tibble: one row per engine per specification.
geo_sweep <- function(.geo, .org, .keys, .specs, .quiet = FALSE) {
  if (FALSE) {
    .geo   <- tab_geo
    .org   <- tab_org
    .keys  <- tab_keys
    .specs <- .lP$Params$Specs
    .quiet <- FALSE
  }

  purrr::imap(.specs, function(.s, .i) {
    r_ <- geo_attach(.geo = .geo, .org = .org, .spec = .s)
    v_ <- geo_validate(.roles = r_, .keys = .keys) |>
      dplyr::filter(.data$GeoLevel == "City") |>
      dplyr::select(Combo, CityAddrMatch = PctMatchU)

    r_ |>
      # A MEAN OVER NOTHING IS NOT ZERO AND IS NOT NaN. LexNLP emits no city at any point, so
      # PctCityAtt is a mean over an empty vector: R returns NaN, tbl_pct renders "NaN%", and a
      # reader has to work out whether that is a failure or an absence. Missing says absence.
      dplyr::summarise(
        Spans       = dplyr::n(),
        PctAttach   = mean(.data$Attached),
        PctToFiler  = mean(dplyr::coalesce(.data$OrgRole, "") == "filer"),
        MedDist     = stats::median(.data$DistToOrg, na.rm = TRUE),
        PctStateAtt = .geo_mean_or_na(.data$Attached[.data$GeoLevel == "State"]),
        PctCityAtt  = .geo_mean_or_na(.data$Attached[.data$GeoLevel == "City"]),
        .by = Combo
      ) |>
      dplyr::left_join(v_, by = dplyr::join_by(Combo)) |>
      dplyr::mutate(Spec = .s$Label, SpecOrder = as.integer(.i), .before = 1L)
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


#' Count places for a map, document-weighted
#'
#' DOCUMENTS, NOT SPANS. One table-heavy filing naming a country four hundred times would otherwise
#' colour that country on its own; a document either mentions a place or it does not.
#'
#' The three scopes are the argument the maps make, and they are the SAME COUNT under three filters
#' rather than three different quantities. UNADJUSTED is every place the contracts name -- the
#' published figure of 1.9 geoentities, and what a referee could not interpret. PARTIES is the places
#' this document attached to a filer or a counterparty. The gap between the two maps IS the rule.
#'
#' @param .roles Tibble from geo_attach(), with the country columns from geo_country().
#' @param .keys Tibble from ent_anchor_keys(). Supplies the contract type.
#' @param .combo Character. Which engine the map is drawn from.
#' @param .scope Character. "all" for every span, "parties" for those attached to a filer or a
#'   counterparty, "counterparty" for counterparties alone.
#' @param .level Character. "country" or "state".
#' @param .by_class Logical. Add the contract type, for small multiples.
#' @return Tibble: one row per area, or per area and class.
geo_map_input <- function(.roles, .keys, .combo = "matcon", .scope = c("all", "parties",
                                                                          "counterparty"),
                          .level = c("country", "state"), .by_class = FALSE) {
  if (FALSE) {
    .roles    <- tab_roles
    .keys     <- tab_keys
    .combo    <- "matcon"
    .scope    <- "all"
    .level    <- "country"
    .by_class <- FALSE
  }

  .scope <- match.arg(.scope)
  .level <- match.arg(.level)

  src_ <- dplyr::filter(.roles, .data$Combo == .combo)

  src_ <- switch(
    .scope,
    all          = src_,
    parties      = dplyr::filter(src_, .data$Attached,
                                 .data$OrgRole %in% c("filer", "counterparty")),
    counterparty = dplyr::filter(src_, .data$Attached, .data$OrgRole == "counterparty")
  )

  src_ <- if (identical(.level, "country")) {
    dplyr::filter(src_, !is.na(.data$CountryIso)) |>
      dplyr::transmute(DocID, Area = .data$CountryIso)
  } else {
    dplyr::filter(src_, !is.na(.data$State)) |>
      dplyr::transmute(DocID, Area = .data$State)
  }

  if (.by_class) {
    src_ <- dplyr::left_join(src_, dplyr::select(.keys, DocID, Class), by = dplyr::join_by(DocID))
    src_ |>
      dplyr::distinct(.data$DocID, .data$Area, .data$Class) |>
      dplyr::summarise(N = dplyr::n(), .by = c(Area, Class))
  } else {
    src_ |>
      dplyr::distinct(.data$DocID, .data$Area) |>
      dplyr::summarise(N = dplyr::n(), .by = Area)
  }
}


#' Which contract types reach outside the United States
#'
#' THE TABLE THAT REPLACED TWELVE WORLD MAPS, and the document had already made the argument against
#' them: its own US map caption says a world map of US contracts is one dark country and a
#' scattering. Twelve of those is twelve dark countries. The question the maps were asked -- which
#' contract types reach abroad -- has a country-by-type answer, and a table gives it in one screen.
#'
#' The construct they were drawn with is also one this project has abandoned twice. A per-class
#' tabset built by knit_child() emptied under Quarto's tabset filter in the 03 pass, and here it
#' failed differently: a plot recorded inside a child is replayed on a device that has not registered
#' the theme's font, and grid raises "invalid font type" the moment a label is drawn.
#'
#' THE UNITED STATES IS EXCLUDED FROM THE COLUMNS AND COUNTED SEPARATELY. Nearly every contract names
#' it, so a column for it would be near-constant and would crowd out the variation the table exists
#' to show. PctForeign is the summary: the share of contracts of that type placing a party anywhere
#' else at all.
#'
#' @param .roles Tibble from geo_attach(), carrying CountryIso and CountryName.
#' @param .keys Tibble from ent_anchor_keys(). Supplies the contract type and the denominator.
#' @param .combo Character. Which family the table is drawn from.
#' @param .scope Character. "all" for every span, "parties" for those attached to a filer or a
#'   counterparty.
#' @param .n Integer. Countries shown as columns, most frequent first.
#' @return Tibble: one row per class plus an ALL row, one column per country.
geo_class_countries <- function(.roles, .keys, .combo = "matcon", .scope = c("parties", "all"),
                                .n = 10L) {
  if (FALSE) {
    .roles <- tab_roles
    .keys  <- tab_keys
    .combo <- "matcon"
    .scope <- "parties"
    .n     <- 10L
  }
  .scope <- match.arg(.scope)

  src_ <- dplyr::filter(.roles, .data$Combo == .combo, !is.na(.data$CountryIso))
  if (identical(.scope, "parties")) {
    src_ <- dplyr::filter(src_, .data$Attached, .data$OrgRole %in% c("filer", "counterparty"))
  }

  # DOCUMENT-WEIGHTED. One table-heavy filing naming a country four hundred times would otherwise
  # carry its own contract type on its own; a document either names a place or it does not.
  doc_ <- src_ |>
    dplyr::distinct(.data$DocID, .data$CountryIso, .data$CountryName) |>
    dplyr::left_join(dplyr::select(.keys, "DocID", "Class"), by = dplyr::join_by(DocID)) |>
    dplyr::mutate(Country = dplyr::coalesce(.data$CountryName, .data$CountryIso))

  top_ <- doc_ |>
    dplyr::filter(.data$CountryIso != "USA") |>
    dplyr::count(.data$Country, name = "N", sort = TRUE) |>
    utils::head(.n) |>
    dplyr::pull(.data$Country)

  denom_ <- dplyr::count(.keys, .data$Class, name = "Docs")

  f_ <- function(.d, .lab) {
    wide_ <- .d |>
      dplyr::filter(.data$Country %in% top_) |>
      dplyr::count(.data$Country, name = "N") |>
      tidyr::pivot_wider(names_from = "Country", values_from = "N")
    n_doc_  <- if (identical(.lab, "All documents")) nrow(.keys) else NA_integer_
    tibble::tibble(Class = .lab, Docs = n_doc_) |>
      dplyr::bind_cols(wide_)
  }

  per_ <- doc_ |>
    dplyr::group_split(.data$Class) |>
    purrr::map(\(.d) f_(.d, as.character(dplyr::first(.d$Class)))) |>
    purrr::list_rbind() |>
    dplyr::select(-"Docs") |>
    dplyr::left_join(dplyr::mutate(denom_, Class = as.character(.data$Class)),
                     by = dplyr::join_by(Class)) |>
    dplyr::relocate("Docs", .after = "Class")

  foreign_ <- doc_ |>
    dplyr::filter(.data$CountryIso != "USA") |>
    dplyr::summarise(NForeign = dplyr::n_distinct(.data$DocID), .by = "Class") |>
    dplyr::mutate(Class = as.character(.data$Class))

  out_ <- per_ |>
    dplyr::left_join(foreign_, by = dplyr::join_by(Class)) |>
    dplyr::mutate(
      PctForeign = dplyr::coalesce(.data$NForeign, 0L) / .data$Docs,
      .after = "Docs"
    ) |>
    dplyr::select(-"NForeign")

  all_ <- f_(doc_, "All documents") |>
    dplyr::mutate(
      PctForeign = dplyr::n_distinct(doc_$DocID[doc_$CountryIso != "USA"]) / nrow(.keys),
      .after = "Docs"
    )

  lev_ <- plot_levels("ClassDetailed")
  dplyr::bind_rows(dplyr::arrange(out_, match(.data$Class, lev_)), all_) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(top_), \(.x) as.integer(dplyr::coalesce(.x, 0L))))
}


#' Print the country-by-type table
#'
#' @param .tab Tibble from geo_class_countries().
#' @param .combo Character, for the title.
#' @return Invisibly .tab.
geo_report_class_countries <- function(.tab, .combo = "matcon") {
  if (FALSE) {
    .tab   <- tab_class_geo
    .combo <- "matcon"
  }

  cli::cli_h2(paste0("Foreign countries by contract type -- ", .combo))
  .tab |>
    dplyr::mutate(PctForeign = tbl_pct(.data$PctForeign)) |>
    tbl_say(.title = "Contracts placing a party in each country, document-weighted")

  cli::cli_alert_info(
    "The United States is excluded from the columns and summarised by PctForeign, because nearly \\
     every contract names it and a near-constant column would crowd out the variation this table \\
     exists to show. Counts are DOCUMENTS: a filing naming a country four hundred times counts once."
  )
  invisible(.tab)
}


# 9. Report --------------------------------------------------------------------------------------------------------------

#' What each engine resolved
#' @param .geo Tibble from geo_city_state().
#' @return Invisibly the summary.
geo_report_resolve <- function(.geo) {
  if (FALSE) .geo <- tab_geo

  cli::cli_h2("Resolution, by engine and level")
  out_ <- .geo |>
    dplyr::summarise(
      Spans     = dplyr::n(),
      Docs      = dplyr::n_distinct(.data$DocID),
      Distinct  = dplyr::n_distinct(.data$GeoUnit),
      PctAmbig  = mean(.data$Ambiguous),
      PctState  = mean(!is.na(.data$State)),
      PctCounty = mean(!is.na(.data$County)),
      PctIso2   = mean(!is.na(.data$Iso2)),
      .by = c(Combo, GeoLevel)
    ) |>
    dplyr::arrange(.data$Combo, plot_factor(.data$GeoLevel, .key = "GeoLevel"))

  out_ |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "Every span placed on one hierarchy")

  .geo |>
    dplyr::filter(.data$GeoLevel == "Other") |>
    dplyr::summarise(Spans = dplyr::n(), .by = c(Combo, RawClass)) |>
    dplyr::arrange(dplyr::desc(.data$Spans)) |>
    tbl_say(.title = "Classes the four levels have no tier for", .n = 10L)

  cli::cli_alert_info(
    "OTHER is a real subdivision -- Ontario, Guangdong, Scotland -- that a US-shaped hierarchy has no \\
     tier for. It keeps its ISO code and rolls to a country rather than being reported at a level \\
     that does not exist here."
  )
  invisible(out_)
}


#' How cities got their state
#' @param .geo Tibble from geo_city_state().
#' @return Invisibly the summary.
geo_report_city <- function(.geo) {
  if (FALSE) .geo <- tab_geo

  cli::cli_h2("Cities resolved to a state")
  out_ <- .geo |>
    dplyr::filter(.data$GeoLevel == "City") |>
    dplyr::summarise(Spans = dplyr::n(), Distinct = dplyr::n_distinct(.data$GeoUnit),
                     .by = c(Combo, GeoHow)) |>
    dplyr::mutate(Share = .data$Spans / sum(.data$Spans), .by = Combo) |>
    dplyr::arrange(.data$Combo, plot_factor(.data$GeoHow, .key = "GeoHow"))

  out_ |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "Which arm gave each city its state")

  cli::cli_alert_info(
    "UNIQUE is a city name that exists in one state. NEAREST NAMED is an ambiguous name resolved to \\
     whichever of its CANDIDATE states the contract itself names -- which is what stops a Springfield \\
     beside a Delaware corporation from becoming Springfield, Delaware. UNRESOLVED keeps no state and \\
     contributes to no variable."
  )
  invisible(out_)
}


#' Where each organisation is
#' @param .orgtab Tibble from geo_org_table().
#' @return Invisibly the summary.
geo_report_org <- function(.orgtab) {
  if (FALSE) .orgtab <- tab_org_geo

  cli::cli_h2("Where each organisation is")
  out_ <- .orgtab |>
    dplyr::summarise(
      Orgs        = dplyr::n(),
      MeanNear    = mean(.data$NGeoNear),
      PctAny      = mean(.data$HasGeo),
      PctIncorp   = mean(!is.na(.data$GeoIncorporation)),
      PctLocation = mean(!is.na(.data$GeoLocation)),
      PctCounty   = mean(!is.na(.data$GeoCounty)),
      PctCountry  = mean(!is.na(.data$GeoCountry)),
      .by = c(Combo, OrgRole)
    ) |>
    dplyr::arrange(.data$Combo, plot_factor(.data$OrgRole, .key = "OrgRole"))

  out_ |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "One row per engine and role, over every organisation 04B1 found")

  cli::cli_alert_info(
    "COVERAGE PER ORGANISATION IS LOWER THAN COVERAGE PER DOCUMENT and that is arithmetic, not a \\
     defect: a document counts as covered when ANY party has a state, an organisation only when IT \\
     does. The filer is named first, in a preamble, with its jurisdiction immediately after; a \\
     counterparty named once in passing often has nothing beside it at all. MeanNear separates \\
     \"nothing was there\" from \"something was there and did not resolve\"."
  )
  invisible(out_)
}


#' The governing law
#' @param .doc Tibble from geo_doc_table().
#' @return Invisibly the summary.
geo_report_law <- function(.doc) {
  if (FALSE) .doc <- tab_doc

  cli::cli_h2("Governing law")
  # Any one engine: the law is a document attribute and identical across them, but taking all rows
  # would count every contract twice.
  one_ <- dplyr::filter(.doc, .data$Combo == dplyr::first(sort(unique(.data$Combo))))

  one_ |>
    dplyr::summarise(Docs = dplyr::n(), .by = LawSpecified) |>
    dplyr::mutate(Share = tbl_pct(.data$Docs / sum(.data$Docs))) |>
    tbl_say(.title = "How each contract specified its law")

  one_ |>
    dplyr::filter(!is.na(.data$GeoGoverningLaw)) |>
    dplyr::summarise(Docs = dplyr::n(), .by = GeoGoverningLaw) |>
    dplyr::arrange(dplyr::desc(.data$Docs)) |>
    dplyr::mutate(Share = tbl_pct(.data$Docs / sum(.data$Docs))) |>
    tbl_say(.title = "Which law, where one was named", .n = 12L)

  one_ |>
    dplyr::summarise(
      Docs    = dplyr::n(),
      PctName = tbl_pct(mean(.data$LawSpecified == "named")),
      PctSelf = tbl_pct(mean(.data$LawSpecified == "self-referential")),
      PctNone = tbl_pct(mean(.data$LawSpecified == "none found")),
      .by = Class
    ) |>
    dplyr::arrange(plot_factor(.data$Class, .key = "ClassDetailed")) |>
    tbl_say(.title = "By contract type")

  cli::cli_alert_info(
    "SELF-REFERENTIAL is a contract term, not a coverage gap. A lease writing \"the laws of the State \\
     in which the Premises are located\" has specified its law exactly, and naming no place is the \\
     point of the clause -- so a class with a low named share and a high self-referential one is \\
     described rather than missed."
  )
  invisible(one_)
}


#' The registered-address validation
#' @param .tab Tibble from geo_validate().
#' @return Invisibly .tab.
geo_report_validate <- function(.tab) {
  if (FALSE) .tab <- tab_validate

  cli::cli_h2("Against the address EDGAR records")
  cli::cli_alert_info(
    "READ THE CEILING FIRST. Most registrants are incorporated in Delaware and located elsewhere, and \\
     EDGAR records the FIRM's address rather than the one written into the contract -- often a \\
     subsidiary's, a landlord's or the counterparty's. A city matching around a half and a state \\
     matching almost never is the shape this check is expected to have, and that GAP is the whole \\
     argument for resolving cities to states rather than reading state spans as locations."
  )

  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("No registered address in the sample table; check skipped.")
    return(invisible(.tab))
  }

  .tab |>
    dplyr::mutate(PctMatchU = tbl_pct(.data$PctMatchU), PctMatchS = tbl_pct(.data$PctMatchS)) |>
    tbl_say(.title = "Places attached to the FILER, against its registered address")
  invisible(.tab)
}


#' What each family's resolved columns actually carry
#'
#' THE CHECK THAT WOULD HAVE CAUGHT MY OWN ERROR. 04A's prose asserted that LexNLP emits no ISO codes
#' -- reasoning from the fact that the codes are dropped as MATCH ALIASES, which is true and does not
#' follow. The codes are properties of the matched entity, not of the alias that matched it, and the
#' store carries ISO-3166-2 on 99.99% of LexNLP's GPE rows. A claim about a column belongs in a table
#' the render produces, not in a paragraph someone reasoned their way to.
#'
#' The category coverage is the same shape of question one level up. geo_resolve() maps a family's
#' raw class onto five levels, and a class string it does not recognise lands silently in "Other". A
#' family whose Other share is large is either genuinely reaching a tier this hierarchy has no name
#' for, or being mapped by a rule that has drifted from what it emits -- and the two look identical
#' until the raw strings are printed beside the count.
#'
#' @param .geo Tibble from geo_resolve(), stacked over families.
#' @return Tibble: one row per family, level and raw class.
geo_coverage <- function(.geo) {
  if (FALSE) .geo <- tab_geo

  .geo |>
    dplyr::summarise(
      Spans     = dplyr::n(),
      PctIso2   = mean(!is.na(.data$Iso2)),
      PctIso3   = mean(!is.na(.data$Iso3)),
      PctState  = mean(!is.na(.data$State)),
      PctCounty = mean(!is.na(.data$County)),
      PctAmbig  = mean(.data$Ambiguous),
      .by = c(Combo, GeoLevel, RawClass)
    ) |>
    dplyr::arrange(.data$Combo, plot_factor(.data$GeoLevel, .key = "GeoLevel"),
                   dplyr::desc(.data$Spans))
}


#' Print the resolved-column coverage, and say what would be wrong if it moved
#'
#' @param .tab Tibble from geo_coverage().
#' @param .country Tibble from geo_country(), for the rollup report.
#' @return Invisibly .tab.
geo_report_coverage <- function(.tab, .country) {
  if (FALSE) {
    .tab     <- tab_cover
    .country <- tab_geo
  }

  cli::cli_h2("What each family resolves, by raw class")
  .tab |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "Every class either family emits, and what it fills")

  cli::cli_alert_info(
    "A COLUMN NAME SHARED ACROSS FAMILIES IS NOT A SHARED QUANTITY. This table is per family for \\
     that reason: matcon resolves a country code on every level and a subdivision code on everything \\
     but a country, while LexNLP is the other way round. They are renamed apart on read and only \\
     mapped together in geo_resolve(), where the mapping is visible."
  )

  other_ <- dplyr::filter(.tab, .data$GeoLevel == "Other")
  if (nrow(other_) > 0L) {
    cli::cli_alert_info(
      "{nrow(other_)} raw class{?es} land in Other: \\
       {paste(unique(other_$RawClass), collapse = ', ')}. These are first-level subdivisions of a \\
       country this hierarchy has no tier for, which is deliberate -- a class arriving here that is \\
       NOT one of those means the level mapping has drifted from what the family emits."
    )
  }

  # THE ROLLUP, REPORTED RATHER THAN ASSERTED. It is the one place a country is inferred rather than
  # read, so how often it fires and from what belongs on the page beside the number it produces.
  roll_ <- .country |>
    dplyr::summarise(Spans = dplyr::n(), .by = c(Combo, CountryFrom)) |>
    dplyr::mutate(Share = tbl_pct(.data$Spans / sum(.data$Spans)), .by = Combo) |>
    dplyr::arrange(.data$Combo, dplyr::desc(.data$Spans))

  tbl_say(.tab = roll_, .title = "Where each span's country came from")
  cli::cli_alert_info(
    "ISO-3166-2 PREFIX is the recovery: of the geo dictionary's 437 entities only the 253 countries \\
     carry a three-letter code, so a non-US subdivision has no ISO-3166-3 and its country is read \\
     from the prefix of its subdivision code instead -- CA-AB is Canada. NONE is a span with no \\
     country at any tier, which is what a state-level or word-like name should show."
  )
  invisible(.tab)
}


#' The attachment sweep
#' @param .tab Tibble from geo_sweep().
#' @return Invisibly .tab.
geo_report_sweep <- function(.tab) {
  if (FALSE) .tab <- tab_sweep

  cli::cli_h2("Attachment sweep")
  .tab |>
    dplyr::select(-SpecOrder) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
                  CityAddrMatch = tbl_pct(.data$CityAddrMatch)) |>
    tbl_say(.title = "One row per engine per specification", .n = 30L)

  cli::cli_alert_info(
    "A wider reach ALWAYS attaches more places, so PctAttach on its own decides nothing. \\
     CityAddrMatch is the column that can fall: it is the share of cities attached to the filer that \\
     appear in its registered address, and a reach buying wrong attachments moves the first column \\
     up and this one down. The 200/400 row is the per-level variant -- reported, and adopted only if \\
     it wins here."
  )
  invisible(.tab)
}


#' The released variables by contract type
#'
#' THE ENGINE IS NAMED, not taken as whichever came first. An earlier version filtered to the first
#' Combo, which is lexnlp -- an engine that emits no cities at all -- and so reported a location
#' variable of exactly zero for every class while the organisation table showed 12.2%. A summary that
#' picks its own engine will pick the wrong one.
#'
#' @param .doc Tibble from geo_doc_table().
#' @param .combo Character. Which engine the table covers.
#' @return Invisibly the summary.
geo_report_class <- function(.doc, .combo = "matcon") {
  if (FALSE) {
    .doc   <- tab_doc
    .combo <- "matcon"
  }

  cli::cli_h2(paste0("The released variables, by contract type -- ", .combo))

  f_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs        = dplyr::n(),
      Unadjusted  = mean(.data$NGeo),
      MeanAttach  = mean(.data$NGeoAttached),
      PctIncorp   = mean(!is.na(.data$GeoIncorporation)),
      PctLocation = mean(!is.na(.data$GeoLocation)),
      PctLaw      = mean(.data$LawSpecified != "none found"),
      MeanCounter = mean(.data$NCounterGeo)
    )
  }

  src_ <- dplyr::filter(.doc, .data$Combo == .combo)

  ent_bind_all(.tab = f_(dplyr::group_by(src_, Class)), .fun = f_, .src = src_) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "One row per type, and one for the sample")

  cli::cli_alert_info(
    "UNADJUSTED is every distinct place the contract names anywhere -- the published figure of 1.9 \\
     geoentities per contract, and the one a referee could not interpret. Everything to its right is \\
     that number with a party attached to it, which is the whole of this document's contribution."
  )
  invisible(src_)
}


#' Every GPE report in order
#'
#' @param .geo Tibble from geo_city_state().
#' @param .orgtab Tibble from geo_org_table().
#' @param .doc Tibble from geo_doc_table().
#' @param .validate Tibble from geo_validate().
#' @param .sweep Tibble from geo_sweep().
#' @return Invisibly NULL.
geo_report_all <- function(.geo, .orgtab, .doc, .validate, .sweep, .combo = "matcon") {
  if (FALSE) {
    .geo      <- tab_geo
    .orgtab   <- tab_org_geo
    .doc      <- tab_doc
    .validate <- tab_validate
    .sweep    <- tab_sweep
  }

  geo_report_resolve(.geo = .geo)
  geo_report_city(.geo = .geo)
  geo_report_org(.orgtab = .orgtab)
  geo_report_validate(.tab = .validate)
  geo_report_sweep(.tab = .sweep)
  geo_report_law(.doc = .doc)
  geo_report_class(.doc = .doc, .combo = .combo)
  invisible(NULL)
}


# 10. Figures ------------------------------------------------------------------------------------------------------------

#' Share of place spans by relative position, per engine
#' @param .tab Tibble from ent_density() computed per engine and stacked.
#' @return A ggplot object.
geo_plot_density <- function(.tab) {
  if (FALSE) .tab <- tab_density

  .tab |>
    dplyr::filter(.data$Weight == "documents") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Pos, y = .data$Share, colour = .data$Combo)) +
    ggplot2::geom_line(linewidth = 0.7) +
    plot_scale_colour_cat(name = NULL) +
    plot_scale_x_pct(.expand = c(0, 0)) +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}


#' Coverage by organisation role, per engine
#' @param .orgtab Tibble from geo_org_table().
#' @return A ggplot object.
geo_plot_role <- function(.orgtab) {
  if (FALSE) .orgtab <- tab_org_geo

  .orgtab |>
    dplyr::summarise(
      Incorporation = sum(!is.na(.data$GeoIncorporation)),
      Location      = sum(!is.na(.data$GeoLocation)),
      Country       = sum(!is.na(.data$GeoCountry)),
      Nothing       = sum(!.data$HasGeo),
      .by = OrgRole
    ) |>
    tidyr::pivot_longer(cols = -OrgRole, names_to = "What", values_to = "N") |>
    plot_bar_stacked(
      .cat   = "OrgRole",
      .val   = "N",
      .fill  = "What",
      .key   = "OrgRole",
      .short = FALSE,
      .share = TRUE
    )
}


#' Distance from a place to the organisation it was attached to
#' @param .tab Tibble from geo_attach().
#' @param .cap Numeric. Distances beyond this are pooled into the last bin.
#' @return A ggplot object.
geo_plot_dist <- function(.tab, .cap = 400) {
  if (FALSE) {
    .tab <- tab_roles
    .cap <- 400
  }

  .tab |>
    dplyr::filter(.data$Attached, !is.na(.data$DistToOrg)) |>
    dplyr::mutate(Dist = pmin(.data$DistToOrg, .cap)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Dist, colour = .data$GeoLevel)) +
    ggplot2::stat_ecdf(linewidth = 0.7) +
    plot_scale_colour_cat(name = NULL) +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}
