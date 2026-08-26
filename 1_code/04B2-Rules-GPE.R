# 04B2-Rules-GPE: where each party is -----------------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# 04B1 found the parties of every contract. This attaches a place to each of them. Function prefix is
# `geo_`.
#
# THE RULE IS THREE TESTS AND ONE NUMBER
#   1. EXCLUDE. A place sitting inside a governing-law clause names the law that applies, not any
#      party's address. It leaves the attachment entirely and becomes a contract variable.
#   2. ATTACH. A place belongs to the party named within 200 characters BEFORE it. A preamble names
#      the company and then qualifies it -- "ACME CORPORATION, a Delaware corporation with offices in
#      Houston, Texas" -- so the place follows the name it describes.
#   3. RESOLVE. State, city, county and country, with the state of an ambiguous city decided by what
#      the contract itself names.
# The number is 200. Nothing else in this file enters an answer.
#
# ONE ENGINE, AND THE REASON IS COVERAGE RATHER THAN QUALITY
# matcon's gazetteer carries 181,810 rows and reaches cities and counties. LexNLP's dictionary
# carries 437 entities and has NO CITIES AND NO COUNTIES at all: it reaches countries and first-level
# subdivisions and stops. Releasing both engines would give every second row a guaranteed-null half
# -- no city, no county, and a state that can never be inferred from a city -- so matcon is the
# release and LexNLP's contribution is reported in 04A, where family comparison belongs.
#
# THE GOVERNING-LAW EXCLUSION IS THE ONE THING THIS FILE GAINED, AND IT REPAIRS A DEFECT
# Attachment used to run over every place with no exception, so in
# "...ACME CORPORATION. This Agreement shall be governed by the laws of the State of New York" a
# clause falling within reach of a company name put NEW YORK in that company's incorporation column.
# 04B2 measured 95% of governing-law spans attaching to no organisation at all, which bounds the
# damage without removing it: the 5% that did attach biased the incorporation variable toward New
# York and Delaware, which is precisely where a reader would look.
#
# lawregex emits the clause as a span, so the fix is containment rather than proximity: a place
# INSIDE a LAW span is the jurisdiction and never an address. That is a non-equi join and it reads no
# text at all.
#
# AND THE SAME JOIN GIVES THE VARIABLE. Asking which place sits inside a clause answers both
# questions at once -- which places to withhold from attachment, and which jurisdiction the contract
# chose. Two outcomes from one join, and the second is a variable accounting research uses directly.
#
# CONTAINMENT BEATS PROXIMITY, AND ONE DOCUMENT IS WHY. In "...governed by the laws of the State of
# New York. Delaware corporations shall deliver..." Delaware begins 47 characters after the cue --
# comfortably inside any proximity window, and not the jurisdiction. The full stop closes the clause
# before Delaware begins, so containment cannot make that mistake.
#
# A HEADING IS A CLAUSE TOO, AND IT OUTRANKS NOTHING. "GOVERNING LAW. This Agreement is made under
# the laws of the State of Texas." is TWO clauses: the heading, closed by its own full stop at zero
# useful characters, and the sentence carrying Texas. Ordering by position alone takes the heading
# and discards the jurisdiction, and a heading followed by the substantive sentence is ordinary
# drafting -- so a clause that found a place outranks one that did not, and position decides only
# between equals.
#
# THE AMBIGUOUS CITY IS THE ONLY REAL DECISION IN THE RESOLUTION
# Two thirds of city names exist in more than one state: SPRINGFIELD sits in 33 of them, and the
# gazetteer has no population, no rank and no primary-place column to order them by. Three arms:
#
#   THE NAME EXISTS IN ONE STATE ONLY               -> that state
#   AMBIGUOUS, AND THE CONTRACT NAMES ONE OF ITS    -> the nearest, ties to the one named most often
#   AMBIGUOUS, AND THE CONTRACT NAMES NONE OF ITS   -> no state
#
# THE CONSTRAINT ON THE SECOND ARM IS WHAT MAKES IT SAFE. Taking the nearest state named ANYWHERE
# would put SPRINGFIELD in Delaware whenever the party happens to be a Delaware corporation, and
# Delaware is incorporation boilerplate in a large share of contracts -- so every Delaware-registered
# filer would drag its cities there and the release would show a concentration that is an artifact of
# the rule. Requiring that a SPRINGFIELD actually EXIST in that state costs one join against a
# candidate list already in the lookup.
#
# POSITION FIRST AND FREQUENCY SECOND, for the same reason. "Springfield, Illinois" puts the state
# two characters away and position settles it outright; frequency first would lose that wherever the
# contract mentioned another candidate state more often. Frequency decides only where nothing is
# adjacent, and it is counted WITHIN THE CONTRACT, so it introduces no quantity whose value depends
# on how the corpus was partitioned.
#
# THE THIRD ARM REFUSES RATHER THAN GUESSES, and that is a decision. A fallback would send every
# unresolved SPRINGFIELD to the same state, and a constant error is not noise: it averages out of
# nothing and reads as a real geographic pattern. GeoStateFrom reports the size of that arm so the
# cost of refusing is a number rather than an argument.
#
# WHAT IS UPSTREAM AND NOT IN THIS FILE
# gazetteer.py admits a place through a three-tier gate before this document sees it: a country or a
# state unconditionally, a distinctive city name only where a state anchor sits within 200
# characters, and a word-like name -- Mobile, Reading, Enterprise -- only within 40. A bare city with
# no state beside it is REFUSED. That is why city coverage looks the way it does, and it is a rule in
# the extractor rather than here.
#
# ONE FILE, EXTENDING 04B1'S
# The release is one row per contract per party, 04B1's twelve columns with geography added to the
# same rows. Single-writer holds: 04B1 owns parties_org.parquet and this document owns
# parties_geo.parquet, which is what goes downstream. A party with no place attached keeps its row
# and carries nulls, so nothing has to be joined back and no count changes.
#
# GOVERNING LAW IS CARRIED, NOT ATTACHED. It is a contract term rather than a party's address, so it
# repeats on every row of a contract exactly as Class and AmendType do, and the dictionary says so.
# Putting it in a party column would assert something the rule never tested.
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


# 2. The gazetteer ------------------------------------------------------------------------------------------------------

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


# 3. Governing law --------------------------------------------------------------------------------------------------

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


#' The governing law of each contract
#'
#' THE SAME JOIN ANSWERS BOTH QUESTIONS. geo_law_inside() says which places to withhold from
#' attachment; this says which of them is the jurisdiction. Two outcomes from one comparison of
#' offsets, and no second copy of the gazetteer's work: the place has already been resolved through
#' the three-tier gate and the 181,810-row lookup, so the jurisdiction is a column rather than a
#' second resolution that could disagree with the first.
#'
#' FOUR OUTCOMES, AND THE FOURTH IS NOT SILENCE. A clause naming a jurisdiction. A SELF-REFERENTIAL
#' clause -- "the laws of the State in which the Premises are located" -- which specifies the law
#' perfectly well and names no place, so recording nothing would count a real contract term as a
#' coverage gap. A clause the gazetteer could not resolve, which is an extraction gap rather than a
#' drafting fact. And no clause at all.
#'
#' A CLAUSE THAT FOUND SOMETHING OUTRANKS ONE THAT DID NOT, and position decides only between equals.
#' "GOVERNING LAW. This Agreement is made under the laws of the State of Texas." is two clauses: the
#' heading, closed by its own full stop at zero useful characters, and the sentence carrying Texas.
#' Ordering by offset alone takes the heading and discards Texas -- a systematic miss, because a
#' heading followed by the substantive sentence is ordinary drafting. is.na() sorts FALSE before
#' TRUE, so the arrange is the whole of the fix.
#'
#' @param .geo Tibble from geo_country().
#' @param .law Tibble of LAW spans from ent_load_entity().
#' @param .inside Tibble from geo_law_inside().
#' @param .keys Tibble from ent_anchor_keys(). Supplies every document, including those with no
#'   clause at all.
#' @return Tibble: one row per document with GoverningLaw, LawLevel and LawSpecified.
geo_law_jurisdiction <- function(.geo, .law, .inside, .keys) {
  if (FALSE) {
    .geo    <- tab_geo
    .law    <- tab_law
    .inside <- tab_inside
    .keys   <- tab_keys
  }

  place_ <- .inside |>
    dplyr::left_join(
      dplyr::select(.geo, DocID, Start, GeoUnit, GeoLevel),
      by = dplyr::join_by(DocID, Start)
    ) |>
    # A COUNTY OR A CITY IS NOT A JURISDICTION. Contracts choose the law of a state or a country, so
    # a place resolved at a lower tier inside a clause is a mention rather than the governing law.
    dplyr::filter(.data$GeoLevel %in% c("State", "Country"))

  rule_ <- .law |>
    dplyr::select(DocID, LawStart = "Start", LawKind = "LabelRaw") |>
    dplyr::left_join(
      dplyr::select(place_, "DocID", "LawStart", "Start", "GeoUnit", "GeoLevel"),
      by = dplyr::join_by(DocID, LawStart), relationship = "one-to-many"
    ) |>
    dplyr::arrange(.data$DocID, is.na(.data$GeoUnit), .data$LawStart, .data$Start) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::transmute(
      .data$DocID,
      GoverningLaw = .data$GeoUnit,
      LawLevel     = .data$GeoLevel,
      LawSpecified = dplyr::case_when(
        !is.na(.data$GeoUnit)              ~ "named",
        .data$LawKind == "SelfReferential" ~ "self-referential",
        .default                           = "clause without a place"
      )
    )

  .keys |>
    dplyr::select("DocID") |>
    dplyr::left_join(rule_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(LawSpecified = dplyr::coalesce(.data$LawSpecified, "none found"))
}


# 4. Attachment -----------------------------------------------------------------------------------------------------

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


#' One place of each kind per party
#'
#' THE SELECTION THE TREE DOES NOT SHOW, and it is a rule of the same standing as the attachment
#' itself. "ACME CORPORATION, a Delaware corporation with offices in Houston, Texas" attaches three
#' places to one party, and the release needs one state, one city, one county and one country. The
#' NEAREST place of each kind wins, position breaking ties -- the same instrument the attachment uses,
#' applied a second time.
#'
#' THE STATE HAS TWO SOURCES AND ONE COLUMN. A state named directly and a state inferred from a city
#' are the same quantity found two ways, so they share a column and GeoStateFrom says which. Splitting
#' them into GeoState and GeoCityState would make a reader join two columns to answer "what state is
#' this party in", and would assert an incorporation-versus-location distinction the rule never
#' tested: "with offices in Texas" names a state directly and is not an incorporation.
#'
#' A DIRECTLY NAMED STATE OUTRANKS AN INFERRED ONE at the same distance, because it is what the
#' contract wrote rather than what the gazetteer worked out.
#'
#' THE EARLIEST MENTION WINS, AND THAT IS WHAT MAKES THE MENTION INDEX PURELY ADDITIVE. A party is
#' now named in several places and each may have something beside it, so the selection needs an order
#' across mentions as well as within one. Taking the earliest means a party that already had a place
#' from its first mention keeps EXACTLY that place: nothing anybody read in the previous release
#' moves, and only parties that had nothing can gain something.
#'
#' It is also the right answer on its own terms. The preamble is where a contract qualifies its
#' parties -- "a Delaware corporation with offices in Houston, Texas" -- and a later mention beside a
#' place is more often a notice address or a schedule than a description of the party.
#'
#' @param .roles Tibble from geo_attach().
#' @param .party Tibble read from 04B1's parties_org.parquet.
#' @return Tibble: one row per document per party, with the geography columns.
geo_party_table <- function(.roles, .party) {
  if (FALSE) {
    .roles <- tab_roles
    .party <- tab_party
  }

  att_ <- dplyr::filter(.roles, .data$Attached)

  pick_ <- function(.levels, .col, .name) {
    att_ |>
      dplyr::filter(.data$GeoLevel %in% .levels, !is.na(.data[[.col]])) |>
      dplyr::arrange(.data$DocID, .data$PartyKey, .data$MentionStart, .data$DistToParty,
                     .data$Start) |>
      dplyr::slice_head(n = 1L, by = c(DocID, PartyKey)) |>
      dplyr::select(DocID, PartyKey, dplyr::all_of(.col)) |>
      dplyr::rename_with(.fn = function(.x) .name, .cols = dplyr::all_of(.col))
  }

  # The state, from either source, with a directly named one preferred at equal distance.
  state_ <- att_ |>
    dplyr::filter(.data$GeoLevel %in% c("State", "City"), !is.na(.data$State)) |>
    dplyr::mutate(FromNamed = .data$GeoStateFrom == "named") |>
    dplyr::arrange(.data$DocID, .data$PartyKey, .data$MentionStart, .data$DistToParty,
                   dplyr::desc(.data$FromNamed), .data$Start) |>
    dplyr::slice_head(n = 1L, by = c(DocID, PartyKey)) |>
    dplyr::select(DocID, PartyKey, GeoState = State, GeoStateFrom, GeoStateAt = MentionIsFirst)

  near_ <- att_ |>
    dplyr::summarise(NGeoNear = dplyr::n_distinct(.data$GeoUnit), .by = c(DocID, PartyKey))

  .party |>
    dplyr::left_join(state_,                                    by = dplyr::join_by(DocID, PartyKey)) |>
    dplyr::left_join(pick_("City",      "GeoUnit",     "GeoCity"),
                     by = dplyr::join_by(DocID, PartyKey)) |>
    dplyr::left_join(pick_("City",      "County",      "GeoCounty"),
                     by = dplyr::join_by(DocID, PartyKey)) |>
    dplyr::left_join(pick_(c("Country", "Other"), "CountryName", "GeoCountry"),
                     by = dplyr::join_by(DocID, PartyKey)) |>
    dplyr::left_join(pick_(c("Country", "Other"), "CountryIso",  "GeoCountryIso"),
                     by = dplyr::join_by(DocID, PartyKey)) |>
    dplyr::left_join(near_,                                     by = dplyr::join_by(DocID, PartyKey)) |>
    dplyr::mutate(
      NGeoNear     = as.integer(dplyr::coalesce(.data$NGeoNear, 0L)),
      GeoStateFrom = dplyr::coalesce(.data$GeoStateFrom, "none"),
      # NOT RELEASED. It says whether the state came from the party's first mention or a later one,
      # which is the measurement that says what the mention index bought and nothing a reader of the
      # file needs. geo_table_gain() is its only consumer.
      GeoStateAt   = dplyr::if_else(is.na(.data$GeoState), NA,
                                    dplyr::coalesce(.data$GeoStateAt, FALSE)),
      # A US TIER IMPLIES THE COUNTRY, and leaving it missing would report a party placed in Texas as
      # having no country. The place's own resolution already says this; the roll-up has to carry it.
      GeoCountry    = dplyr::if_else(
        is.na(.data$GeoCountry) & !is.na(.data$GeoState), "United States", .data$GeoCountry
      ),
      GeoCountryIso = dplyr::if_else(
        is.na(.data$GeoCountryIso) & !is.na(.data$GeoState), "USA", .data$GeoCountryIso
      ),
      HasGeo        = !is.na(.data$GeoState) | !is.na(.data$GeoCountry)
    )
}


#' Does a place attached to the REGISTRANT appear in the address EDGAR records
#'
#' THE ONLY PRECISION MEASURE IN THIS DOCUMENT, and the reason cities are kept as an input. The
#' ceiling is low and low is not failure: most registrants are incorporated in Delaware and located
#' elsewhere, and EDGAR records the FIRM's address rather than the one written into the contract --
#' often a subsidiary's, a landlord's or the counterparty's.
#'
#' A FLAG AND NOT A FILL. Using EDGAR to supply a missing location would make the variable partly
#' what the contract says and partly what EDGAR says, with nothing downstream able to tell which. It
#' is released as a column a researcher can condition on instead.
#'
#' Containment on the uppercased address, because EDGAR's address is one free-text field and parsing
#' it into components would be a rule this document does not need.
#'
#' @param .tab Tibble from geo_party_table().
#' @param .keys Tibble from ent_anchor_keys().
#' @return .tab with EdgarAgrees added, NA on every party that is not the registrant.
geo_edgar_check <- function(.tab, .keys) {
  if (FALSE) {
    .tab  <- tab_geo_party
    .keys <- tab_keys
  }

  # THE ONLY PRECISION MEASURE IN THIS DOCUMENT CANNOT SILENTLY DO NOTHING. Without an address column
  # every check below is NA, and an all-NA precision measure looks identical to one that ran and
  # found nothing. The configure chunk gates the path; this is the second line of defence.
  if (!"BusinessAddress" %in% names(.keys)) {
    cli::cli_abort(c(
      "The anchor carries no BusinessAddress, so the registered-address check cannot run.",
      "i" = "ent_anchor_keys() joins it from 01C's LandingPage.parquet and skips the join where
             that file is absent."
    ))
  }

  .tab |>
    dplyr::left_join(dplyr::select(.keys, DocID, BusinessAddress), by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      Addr        = stringi::stri_trans_toupper(dplyr::coalesce(.data$BusinessAddress, "")),
      EdgarAgrees = dplyr::case_when(
        .data$PartyRole != "registrant"                    ~ NA,
        !nzchar(.data$Addr)                                ~ NA,
        is.na(.data$GeoState) & is.na(.data$GeoCity)       ~ NA,
        .default = stringi::stri_detect_fixed(
          .data$Addr, stringi::stri_trans_toupper(dplyr::coalesce(.data$GeoCity, .data$GeoState))
        )
      )
    ) |>
    dplyr::select(-Addr, -BusinessAddress)
}


#' The release: 04B1's parties, with geography and the contract's governing law
#'
#' ONE FILE, EXTENDING THE ONE 04B1 WROTE. Every row 04B1 released keeps its row here, so a party
#' with no place attached carries nulls rather than disappearing, and no count anybody computed over
#' the party file changes when they move to this one. The sentinel a contract with no organisation
#' carries survives for the same reason.
#'
#' GOVERNING LAW REPEATS ON EVERY ROW, like Class and AmendType, and it is a CONTRACT term rather
#' than a party's address. That is the whole reason places inside a law clause are withheld from
#' attachment: recording the jurisdiction as a column of the contract and never as a column of a
#' party is the distinction the exclusion exists to protect.
#'
#' @param .tab Tibble from geo_edgar_check().
#' @param .law Tibble from geo_law_jurisdiction().
#' @return Tibble: one row per document per party, twenty-one columns.
geo_release_parties <- function(.tab, .law) {
  if (FALSE) {
    .tab <- tab_geo_party
    .law <- tab_law_doc
  }

  .tab |>
    dplyr::left_join(dplyr::select(.law, DocID, GoverningLaw, LawSpecified),
                     by = dplyr::join_by(DocID)) |>
    dplyr::transmute(
      .data$DocID, .data$Class, .data$AmendType,
      .data$PartyName, .data$PartyKey, .data$PartyRole, .data$Matched,
      .data$PartyStart, .data$PartyStop,
      .data$NameFrom, .data$NVariants, .data$NMentions,
      GeoState      = .data$GeoState,
      GeoStateFrom  = .data$GeoStateFrom,
      GeoCity       = .data$GeoCity,
      GeoCounty     = .data$GeoCounty,
      GeoCountry    = .data$GeoCountry,
      GeoCountryIso = .data$GeoCountryIso,
      NGeoNear      = .data$NGeoNear,
      EdgarAgrees   = .data$EdgarAgrees,
      GoverningLaw  = .data$GoverningLaw,
      LawSpecified  = dplyr::coalesce(.data$LawSpecified, "none found")
    ) |>
    dplyr::arrange(.data$DocID, .data$PartyStart)
}


#' Apply the rule end to end
#'
#' @param .spans Tibble from ent_load_entity() for matcon GPE.
#' @param .law Tibble from ent_load_entity() for matcon LAW.
#' @param .party Tibble read from 04B1's parties_org.parquet.
#' @param .mentions Tibble read from 04B1's mentions_org.parquet.
#' @param .keys Tibble from ent_anchor_keys().
#' @param .geo List: Lookup and Cand.
#' @param .spec List from geo_spec().
#' @return A list: Spec, Places, Inside, Law, Roles, Party, Release.
geo_apply <- function(.spans, .law, .party, .mentions, .keys, .geo, .spec) {
  if (FALSE) {
    .spans    <- tab_spans
    .law      <- tab_law
    .party    <- tab_party
    .mentions <- tab_mentions
    .keys     <- tab_keys
    .geo      <- geo_once
    .spec     <- .lP$Params$Spec
  }

  places_ <- .spans |>
    geo_resolve(.lookup = .geo$Lookup) |>
    geo_city_state(.cand = .geo$Cand) |>
    geo_country()

  inside_ <- geo_law_inside(.geo = places_, .law = .law)
  lawdoc_ <- geo_law_jurisdiction(.geo = places_, .law = .law, .inside = inside_, .keys = .keys)

  # THE EXCLUSION, AND IT IS AN ANTI-JOIN RATHER THAN A FILTER ON A FLAG, so a place is withheld by
  # the same offsets the jurisdiction was found by and the two cannot drift apart.
  free_ <- dplyr::anti_join(places_, inside_, by = dplyr::join_by(DocID, Start))

  roles_ <- geo_attach(.geo = free_, .mentions = .mentions, .spec = .spec)
  ptab_  <- geo_party_table(.roles = roles_, .party = .party) |>
    geo_edgar_check(.keys = .keys)

  list(
    Spec    = .spec,
    Places  = places_,
    Inside  = inside_,
    Law     = lawdoc_,
    Roles   = roles_,
    Party   = ptab_,
    Release = geo_release_parties(.tab = ptab_, .law = lawdoc_)
  )
}


# 5. Evidence for the one number ---------------------------------------------------------------------------------------

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
geo_sweep_reach <- function(.geo, .party, .mentions, .keys,
                            .reaches = c(50, 100, 200, 400, 800), .quiet = FALSE) {
  if (FALSE) {
    .geo      <- tab_free
    .party    <- tab_party
    .mentions <- tab_mentions
    .keys     <- tab_keys
    .reaches  <- c(50, 100, 200, 400, 800)
    .quiet    <- FALSE
  }

  n_ <- dplyr::n_distinct(.keys$DocID)

  purrr::map(.reaches, function(.r) {
    spec_ <- geo_spec(.reach = .r)
    tab_  <- geo_attach(.geo = .geo, .mentions = .mentions, .spec = spec_) |>
      geo_party_table(.party = .party) |>
      geo_edgar_check(.keys = .keys)

    reg_ <- dplyr::filter(tab_, .data$PartyRole == "registrant")
    tibble::tibble(
      Reach     = .r,
      Spec      = spec_$Label,
      PctState  = mean(!is.na(tab_$GeoState)),
      PctCity   = mean(!is.na(tab_$GeoCity)),
      PctAny    = mean(tab_$HasGeo),
      MedNear   = stats::median(tab_$NGeoNear),
      PctEdgar  = .geo_share_or_na(.x = reg_$EdgarAgrees[!is.na(reg_$EdgarAgrees)]),
      Docs      = n_
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


# 6. Tables ------------------------------------------------------------------------------------------------------------

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


#' What the governing-law clause said
#' @param .law Tibble from geo_law_jurisdiction().
#' @return Tibble: one row per outcome.
geo_table_law <- function(.law) {
  if (FALSE) .law <- tab_law_doc

  .law |>
    dplyr::summarise(Docs = dplyr::n(), .by = LawSpecified) |>
    dplyr::mutate(Share = .data$Docs / sum(.data$Docs)) |>
    dplyr::arrange(plot_factor(.data$LawSpecified, .key = "LawSpecified"))
}


#' The commonest governing-law jurisdictions
#' @param .law Tibble from geo_law_jurisdiction().
#' @param .n Integer. Rows returned.
#' @return Tibble: one row per jurisdiction.
geo_table_law_top <- function(.law, .n = 15L) {
  if (FALSE) {
    .law <- tab_law_doc
    .n   <- 15L
  }

  named_ <- dplyr::filter(.law, .data$LawSpecified == "named")

  named_ |>
    dplyr::summarise(Docs = dplyr::n(), .by = c(GoverningLaw, LawLevel)) |>
    dplyr::mutate(Share = .data$Docs / nrow(named_)) |>
    dplyr::arrange(dplyr::desc(.data$Docs)) |>
    dplyr::slice_head(n = .n)
}


#' What geography each role got, by contract type
#' @param .tab Tibble from geo_release_parties().
#' @return Tibble: one row per type, and one for the sample.
geo_table_cover <- function(.tab) {
  if (FALSE) .tab <- tab_release

  src_ <- dplyr::filter(.tab, .data$PartyRole != "none")

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Parties    = dplyr::n(),
      PctState   = mean(!is.na(.data$GeoState)),
      PctCity    = mean(!is.na(.data$GeoCity)),
      PctCounty  = mean(!is.na(.data$GeoCounty)),
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


#' What reaching past the first mention bought
#'
#' THE MEASUREMENT THAT SAYS WHETHER THE MENTION INDEX WAS WORTH BUILDING. Every party whose state
#' came from a mention other than its first is a party the previous release left empty, because that
#' release could only see where a party was named first.
#'
#' THE CHANGE IS ADDITIVE BY CONSTRUCTION, so this table has no cost column. The roll-up prefers the
#' earliest mention, which means a party that already had a state keeps exactly that state -- the
#' extra mentions can only fill an empty cell, never overwrite a full one.
#'
#' @param .tab Tibble from geo_party_table(), carrying GeoStateAt.
#' @return Tibble: one row per party role, and one for every party.
geo_table_gain <- function(.tab) {
  if (FALSE) .tab <- tab_geo

  src_ <- dplyr::filter(.tab, .data$PartyRole != "none")

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Parties     = dplyr::n(),
      PctState    = mean(!is.na(.data$GeoState)),
      PctFirst    = mean(dplyr::coalesce(.data$GeoStateAt, FALSE)),
      PctLater    = mean(!is.na(.data$GeoState) & !dplyr::coalesce(.data$GeoStateAt, TRUE)),
      .by = dplyr::any_of("PartyRole")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(src_), plot_factor(.data$PartyRole, .key = "PartyRole")),
    dplyr::mutate(cols_(dplyr::select(src_, -"PartyRole")), PartyRole = "All", .before = 1L)
  )
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


#' A few whole contracts, exactly as the file holds them
#'
#' THE BLOCK A READER OF THE FILE ACTUALLY NEEDS. Everything else describes the release; this shows
#' it. A short index is added for display and is not a released column: DocID is forty-four
#' characters and would take a fifth of the console width on every row.
#'
#' @param .tab Tibble from geo_release_parties().
#' @param .n Integer. Documents drawn.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: every released row of the drawn documents, with Doc added for display.
geo_release_sample <- function(.tab, .n = 4L, .seed = 42L) {
  if (FALSE) {
    .tab  <- tab_release
    .n    <- 4L
    .seed <- 42L
  }

  # Drawn from the documents that actually placed something, because a draw of four from the whole
  # sample would most likely show four rows of nulls and say nothing about the rule.
  pool_ <- unique(.tab$DocID[!is.na(.tab$GeoState) | !is.na(.tab$GeoCountry)])
  if (length(pool_) == 0L) pool_ <- unique(.tab$DocID)

  docs_ <- withr::with_seed(.seed, sample(pool_, size = min(.n, length(pool_))))

  .tab |>
    dplyr::filter(.data$DocID %in% docs_) |>
    dplyr::arrange(.data$DocID, .data$PartyStart) |>
    dplyr::mutate(Doc = dplyr::dense_rank(.data$DocID), .before = 1L)
}


#' What every column of the released file means
#'
#' A TABLE RATHER THAN PROSE, AND CHECKED AGAINST THE FILE. A dictionary written as text drifts from
#' the thing it documents and nothing notices; built here and compared to names(), a column added or
#' renamed without a matching entry aborts the render.
#'
#' @param .tab Tibble from geo_release_parties().
#' @return Tibble: Column, Level, Meaning.
geo_dictionary_parties <- function(.tab) {
  if (FALSE) .tab <- tab_release

  dict_ <- tibble::tribble(
    ~Column,        ~Level,     ~Meaning,
    "DocID",        "contract", "the contract; joins to the register and every other 04 file",
    "Class",        "contract", "contract type, from 03A's label spine",
    "AmendType",    "contract", "original or amended, from the same spine",
    "PartyName",    "party",    "the party's name, as the member that named it wrote it",
    "PartyKey",     "party",    "the reduced key, matching the same party across contracts",
    "PartyRole",    "party",    "registrant, counterparty, signatory, other, or none",
    "Matched",      "party",    "did this party's key agree with the EDGAR filer name",
    "PartyStart",   "party",    "offset of its earliest mention, into 04A's canonical text",
    "PartyStop",    "party",    "offset of the end of that mention",
    "NameFrom",     "party",    "which spelling gave the party its name",
    "NVariants",    "party",    "how many spellings the grouping merged into this party",
    "NMentions",    "party",    "how often the contract names it, across every spelling",
    "GeoState",     "party",    "the US state this party was placed in",
    "GeoStateFrom", "party",    "named, unique, named nearby, unresolved, or none",
    "GeoCity",      "party",    "the city, where one was named beside the party",
    "GeoCounty",    "party",    "that city's county, where the state is known",
    "GeoCountry",   "party",    "the country, United States for any US tier",
    "GeoCountryIso","party",    "that country's ISO-3166-3 code, for joining",
    "NGeoNear",     "party",    "distinct places attached; zero separates absent from unresolved",
    "EdgarAgrees",  "party",    "registrant only: the place appears in EDGAR's recorded address",
    "GoverningLaw", "contract", "the jurisdiction the contract chose; NOT a party's address",
    "LawSpecified", "contract", "named, self-referential, clause without a place, or none found"
  )

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


# 7. Report --------------------------------------------------------------------------------------------------------

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


#' What reaching past the first mention bought
#' @param .tab Tibble from geo_table_gain().
#' @return Invisibly .tab.
geo_report_gain <- function(.tab) {
  if (FALSE) .tab <- tab_gain

  cli::cli_h2("What reaching past the first mention bought")
  .tab |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct_safe(.x))) |>
    tbl_say(.title = "Where each party's state came from, by role")

  cli::cli_alert_info(
    "PctFirst IS WHAT THE PREVIOUS RELEASE COULD REACH and PctLater is what it could not: a party \\
     named again beside an address in a signature block, which a table holding only first mentions \\
     cannot see. PctState is their sum. Nothing here is a trade -- the roll-up prefers the earliest \\
     mention, so a party that already had a state kept exactly that state and only empty cells were \\
     filled."
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
    "PctEdgar IS THE ONLY PRECISION MEASURE HERE, and it is what decides the reach rather than \\
     coverage: a wider reach attaches more places and a falling PctEdgar says the extra ones are \\
     wrong. Its ceiling is low and low is not failure -- EDGAR records the firm's address and the \\
     contract often writes a subsidiary's, a landlord's or the counterparty's."
  )
  invisible(.tab)
}


#' What geography each role got
#' @param .tab Tibble from geo_table_cover().
#' @return Invisibly .tab.
geo_report_cover <- function(.tab) {
  if (FALSE) .tab <- tab_cover

  cli::cli_h2("What geography each role got")
  .tab |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct_safe(.x))) |>
    tbl_say(.title = "One row per party role, and one for every party")

  cli::cli_alert_info(
    "THE REGISTRANT SHOULD BEAT EVERY OTHER ROLE, because the preamble qualifies it first and at \\
     length. A counterparty placed as often as a registrant would mean the attachment is picking up \\
     something other than the qualifying clause. PctNonUS is over the parties that got a country at \\
     all, so it is a share of the placed rather than of the sample."
  )
  invisible(.tab)
}


#' What the released file holds
#' @param .tab Tibble from geo_release_parties().
#' @return Invisibly the summary.
geo_report_release <- function(.tab) {
  if (FALSE) .tab <- tab_release

  cli::cli_h2("The released file")

  out_ <- .tab |>
    dplyr::summarise(
      Rows     = dplyr::n(),
      Docs     = dplyr::n_distinct(.data$DocID),
      PctPlaced = mean(!is.na(.data$GeoState) | !is.na(.data$GeoCountry)),
      .by = PartyRole
    ) |>
    dplyr::mutate(PctPlaced = tbl_pct(.data$PctPlaced),
                  Share     = tbl_pct(.data$Rows / sum(.data$Rows))) |>
    dplyr::arrange(plot_factor(.data$PartyRole, .key = "PartyRole"))

  tbl_say(.tab = out_, .title = "One row per contract per party, by role")

  cli::cli_alert_info(
    "EVERY ROW 04B1 RELEASED KEEPS ITS ROW HERE, the sentinel included, so no count anybody computed \\
     over the party file changes when they move to this one. A party with no place attached carries \\
     nulls rather than disappearing, which is what makes PctPlaced a share of the parties rather \\
     than of the ones that worked."
  )
  invisible(out_)
}


#' A few whole contracts, printed as the file holds them
#' @param .tab Tibble from geo_release_sample().
#' @return Invisibly .tab.
geo_report_release_sample <- function(.tab) {
  if (FALSE) .tab <- tab_sample

  cli::cli_h2("The released file, read")

  .tab |>
    dplyr::distinct(.data$Doc, .data$DocID, .data$Class, .data$GoverningLaw,
                    .data$LawSpecified) |>
    tbl_say(.title = "The drawn contracts, and the governing law each one chose")

  .tab |>
    dplyr::select("Doc", "PartyName", "PartyRole", "GeoState", "GeoStateFrom", "GeoCity",
                  "GeoCounty", "GeoCountry", "NGeoNear", "EdgarAgrees") |>
    tbl_say(.title = "Every party of those contracts, with the place attached to it")

  cli::cli_alert_info(
    "GOVERNING LAW IS IN THE FIRST TABLE AND NOT THE SECOND, and that is the whole point of the \\
     exclusion: it is a term of the contract and never a party's address. It is a column of every \\
     row in the file, reported once here because it does not vary within a contract -- as Class and \\
     AmendType do not."
  )
  invisible(.tab)
}


#' The column dictionary
#' @param .tab Tibble from geo_dictionary_parties().
#' @return Invisibly .tab.
geo_report_dictionary <- function(.tab) {
  if (FALSE) .tab <- tab_dict

  cli::cli_h2("What every column means")
  tbl_say(.tab = .tab, .title = "Twenty-two columns, in the order the file carries them")

  cli::cli_alert_info(
    "LEVEL SAYS WHERE THE COLUMN BELONGS. A contract column repeats on every row of a contract; a \\
     party column varies between them. GoverningLaw is a contract column, which is why no party ever \\
     carries a jurisdiction in its own state field. The dictionary is compared with the file's names, \\
     so a column added or renamed without an entry aborts this chunk."
  )
  invisible(.tab)
}


#' The rule in one table
#' @param .release Tibble from geo_release_parties().
#' @param .law Tibble from geo_law_jurisdiction().
#' @param .inside Tibble from geo_law_inside().
#' @return Invisibly the table.
geo_report_headline <- function(.release, .law, .inside) {
  if (FALSE) {
    .release <- tab_release
    .law     <- tab_law_doc
    .inside  <- tab_inside
  }

  cli::cli_h2("The rule in one table")

  party_ <- dplyr::filter(.release, .data$PartyRole != "none")
  reg_   <- dplyr::filter(.release, .data$PartyRole == "registrant")

  out_ <- tibble::tribble(
    ~Item,                                       ~Value,
    "Contracts",                                 format(dplyr::n_distinct(.release$DocID),
                                                        big.mark = ","),
    "Parties",                                   format(nrow(party_), big.mark = ","),
    "Places withheld as governing law",          format(nrow(.inside), big.mark = ","),
    "Parties given a state",                     tbl_pct(mean(!is.na(party_$GeoState))),
    "Parties given a city",                      tbl_pct(mean(!is.na(party_$GeoCity))),
    "Parties given a country",                   tbl_pct(mean(!is.na(party_$GeoCountry))),
    "Registrants given a state",                 tbl_pct(mean(!is.na(reg_$GeoState))),
    "Registrant place agrees with EDGAR",        tbl_pct_safe(.geo_share_or_na(
                                                   .x = reg_$EdgarAgrees[!is.na(reg_$EdgarAgrees)])),
    "Contracts naming a governing law",          tbl_pct(mean(.law$LawSpecified == "named")),
    "Rows in the released file",                 format(nrow(.release), big.mark = ",")
  )

  tbl_say(.tab = out_, .title = "Everything this document decided")

  cli::cli_alert_info(
    "The three party rows are the result: a place is attached where the contract qualified the name \\
     and left empty where it did not. Every other row says how far that answer can be trusted."
  )
  invisible(out_)
}


# 8. Figures -------------------------------------------------------------------------------------------------------

#' What geography each role got
#' @param .tab Tibble from geo_table_cover().
#' @return A ggplot.
geo_plot_cover <- function(.tab) {
  if (FALSE) .tab <- tab_cover

  .tab |>
    dplyr::filter(.data$PartyRole != "All") |>
    dplyr::select("PartyRole", State = "PctState", City = "PctCity", Country = "PctCountry") |>
    tidyr::pivot_longer(cols = c("State", "City", "Country"), names_to = "Level",
                        values_to = "Share") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Share,
                                 y = plot_factor(.data$PartyRole, .key = "PartyRole",
                                                 .short = TRUE, .rev = TRUE),
                                 fill = .data$Level)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
    plot_scale_fill_cat(name = NULL) +
    plot_scale_x_pct(.accuracy = 1) +
    ggplot2::labs(x = "Share of parties placed", y = NULL) +
    plot_theme(.grid = "x", .legend = "bottom")
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
    .cat      = "GoverningLaw",
    .val      = "Share",
    .pct      = TRUE,
    .accuracy = 1
  ) +
    ggplot2::labs(x = "Share of contracts naming a jurisdiction")
}
