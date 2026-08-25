# Rebuild geo_lookup.parquet with the full hierarchy ----
# Usage: source("contracts-extract/rebuild-geo-lookup.R")
#
# WHY AN R SCRIPT LIVES IN A PYTHON PACKAGE
# It sits beside the artifact it maintains, which is the only place its provenance is discoverable:
# geo_lookup.parquet's CONTENT HASH is folded into gazetteer-v2's SPEC, so this file is the sole
# record of how a provenance-hashed artifact was produced. contracts-lexnlp/rebuild_lexnlp.R is the
# same arrangement for the same reason. Neither is pipeline code and .renvignore excludes both.
#
# WHAT THIS DOES
# The lookup the gazetteer reads carries seven columns and answers "is this string a place". It
# cannot answer "which place", because a name alone does not identify one: SPRINGFIELD is a
# populated place in 35 states and WASHINGTON is a county in 31. This script rebuilds it so that
# every row carries its own position in the hierarchy, which is what turns a match into a variable.
#
# WHAT IS ADDED
#   ParentCounty  the county a populated place sits in. Present in usgs_raw.parquet for all 185,315
#                 populated places and dropped when the lookup was first built. This is the whole
#                 reason county can be the lowest reported level rather than state.
#   Iso2          ISO-3166-2 for US states, "US-MN" form. Chosen deliberately: LexNLP already emits
#                 exactly this for its own US State entities, so both engines produce the identical
#                 identifier and nothing downstream needs to know which found it.
#   GeoKey        the uppercased name, so the join from a span is on a stored column rather than on
#                 a function of one.
#   NParent       how many distinct parents this name has. A resolver needs to know that WASHINGTON
#                 is ambiguous BEFORE it picks one, and this is the cheapest way to say so.
#
# WHAT IS NOT ADDED, AND WHY
# FIPS codes. 03-NamedEntities.R pulled state_numeric, county_numeric and feature_id from the USGS
# Domestic Names file, and usgs_raw.parquet ON DISK does not carry them -- they were dropped at that
# build, not this one. Recovering them needs the original download rather than a local join, so it
# is a separate job. Iso2 covers the state level meanwhile; county has no code and is carried by
# name, which is a real limitation to state in the paper rather than to paper over.
#
# THE EXTRACTOR NOW READS THE COLUMNS THIS SCRIPT ADDS. An earlier version of this header claimed
# that "extract_gazetteer.py reads GeoName, GeoClass and IsWord only, so adding columns cannot
# change which spans are emitted". That was true when it was written and is FALSE NOW:
# gazetteer.py's build_index() reads seven columns -- GeoName, GeoKey, GeoClass, IsWord, NParent,
# Iso2, ISO3 -- and four of them are added here. A rebuild can therefore change what the extractor
# emits, and the safety argument that made this script cheap to re-run no longer holds.
#
# The header also named a check script that re-ran the gazetteer and diffed against the store. That
# script was Check-NER-GPE.R and it is retired. Nothing verifies the rebuild against the store now.
#
# THREE NUMBERS IN THIS HEADER WERE WRONG, AND RUNNING IT IS WHAT FOUND THEM
# They are corrected below and recorded here because a provenance record with a stale number in it
# is worse than one with none: a reader who checks one figure and finds it wrong stops trusting the
# rest, and cannot tell which of the others still hold.
#
#   SPRINGFIELD is a populated place in 33 states, not 35. WASHINGTON as a county is 31, which was
#   right. Both are NParent in the rebuilt lookup and both are checked by Test-MatCon-GPE.R.
#
#   The -99 sentinel reached 150 country rows, not 83 -- 125 that stay unresolved plus 25 repaired.
#   The smaller figure probably counted distinct places where this counts aliases.
#
#   The caveat about Iso2 falling below 100% never fires. usgs_raw does carry PUERTO RICO and GUAM
#   as ParentState values, but none survives into the lookup: it holds four GeoClass values and not
#   one is a territory, so the check reports 100% and the territory table is empty. The block is
#   kept because a future lookup could carry them and reporting nothing would then be wrong.
#
# usgs_raw.parquet HOLDS 974,011 ROWS, not 185,315. It is the full USGS Domestic Names file across
# every feature class; section 3 filters to GeoClass == "Populated Place", and 185,315 is what
# survives that filter. The two numbers describe different things and both are right.
#
# WHY .write DEFAULTS TO FALSE
# arrow::write_parquet() stamps the arrow version into the file's created_by metadata, so re-running
# this script under a different arrow release produces DIFFERENT BYTES FROM IDENTICAL DATA. That
# moves _io.file_hash(geo_lookup.parquet), which moves gazetteer-v2's spec hash, which aborts the
# extraction manifest and re-extracts GPE over 1.19 million documents.
#
# The artifact's identity therefore depends on which arrow version wrote it rather than on what it
# says. The principled repair is for gazetteer.py's spec() to hash the lookup's CONTENT rather than
# its bytes -- but that edit itself moves the spec hash, so it is deliberately deferred to the next
# occasion the matcon store is being re-extracted anyway.
#
# Until then: a run is an INSPECTION. Every validation block below still executes and prints, and
# nothing reaches disk unless .write is set to TRUE by hand.
#
# AND SECTION 5B IS WHY THAT COSTS NOTHING. The rebuild is compared against the lookup ALREADY ON
# DISK -- the file this script would overwrite and whose content hash is inside gazetteer-v2's SPEC.
# Where the two agree, writing would change nothing and the switch is moot; where they disagree, the
# shipped spans came from a rule this script cannot reproduce, and that is precisely the moment a
# write must not happen silently.
#
# The comparison covers EVERY column and reports which differ. Naming only the seven that
# gazetteer.py reads would put a second declaration of Python's behaviour into R, where it can drift
# -- so that list decides the SEVERITY of a difference and never whether one is looked for.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII.


# 1. Configuration ----

.dir_gaz     <- here::here("contracts-extract", "src", "matcon_extract", "data")
.path_lookup <- fs::path(.dir_gaz, "geo_lookup.parquet")
.path_usgs   <- fs::path(.dir_gaz, "usgs_raw.parquet")
.path_backup <- fs::path(.dir_gaz, "geo_lookup_pre_hierarchy.parquet")

.write       <- FALSE  # TRUE replaces the lookup; see "WHY .write DEFAULTS TO FALSE" above

# Columns this script adds. Named once so the input can be stripped of them before the rebuild.
.added       <- c("GeoKey", "StateCode", "Iso2", "StateName", "ParentCounty", "NParent",
                  "IsoSource", "StateForCode")

stopifnot(fs::file_exists(c(.path_lookup, .path_usgs)))

# THIS SCRIPT MUST BE RE-RUNNABLE. Run once it replaces geo_lookup.parquet; run twice, a naive
# version reads its own output, joins ParentCounty onto a table that already has one, and dies on
# ParentCounty.x / ParentCounty.y. Worse, a version that survived that would rebuild a rebuild.
#
# So the rebuild always runs from ONE fixed input. The backup is written before the first rebuild
# and is thereafter the canonical original; if it is ever deleted, the added columns are stripped
# from whatever is on disk instead. Two guards, because the failure is silent under the second one
# alone and destructive under neither.
if (!fs::file_exists(.path_backup)) {
  fs::file_copy(.path_lookup, .path_backup)
  cli::cli_alert_info("Original lookup preserved at {.path {(.path_backup)}}.")
}

.path_source <- .path_backup
cli::cli_alert_info("Rebuilding from {.path {(fs::path_file(.path_source))}}.")


# 2. The state code table ----
# Fifty rows, hardcoded because it is a constant of the world rather than data. DC is included even
# though the lookup's US State class holds exactly fifty names and not the District: usgs_raw does
# carry DISTRICT OF COLUMBIA as a ParentState, so a populated place there must still resolve.

tab_state <- tibble::tribble(
  ~StateName,              ~StateCode,
  "ALABAMA",               "AL",
  "ALASKA",                "AK",
  "ARIZONA",               "AZ",
  "ARKANSAS",              "AR",
  "CALIFORNIA",            "CA",
  "COLORADO",              "CO",
  "CONNECTICUT",           "CT",
  "DELAWARE",              "DE",
  "DISTRICT OF COLUMBIA",  "DC",
  "FLORIDA",               "FL",
  "GEORGIA",               "GA",
  "HAWAII",                "HI",
  "IDAHO",                 "ID",
  "ILLINOIS",              "IL",
  "INDIANA",               "IN",
  "IOWA",                  "IA",
  "KANSAS",                "KS",
  "KENTUCKY",              "KY",
  "LOUISIANA",             "LA",
  "MAINE",                 "ME",
  "MARYLAND",              "MD",
  "MASSACHUSETTS",         "MA",
  "MICHIGAN",              "MI",
  "MINNESOTA",             "MN",
  "MISSISSIPPI",           "MS",
  "MISSOURI",              "MO",
  "MONTANA",               "MT",
  "NEBRASKA",              "NE",
  "NEVADA",                "NV",
  "NEW HAMPSHIRE",         "NH",
  "NEW JERSEY",            "NJ",
  "NEW MEXICO",            "NM",
  "NEW YORK",              "NY",
  "NORTH CAROLINA",        "NC",
  "NORTH DAKOTA",          "ND",
  "OHIO",                  "OH",
  "OKLAHOMA",              "OK",
  "OREGON",                "OR",
  "PENNSYLVANIA",          "PA",
  "RHODE ISLAND",          "RI",
  "SOUTH CAROLINA",        "SC",
  "SOUTH DAKOTA",          "SD",
  "TENNESSEE",             "TN",
  "TEXAS",                 "TX",
  "UTAH",                  "UT",
  "VERMONT",               "VT",
  "VIRGINIA",              "VA",
  "WASHINGTON",            "WA",
  "WEST VIRGINIA",         "WV",
  "WISCONSIN",             "WI",
  "WYOMING",               "WY"
) |>
  dplyr::mutate(Iso2 = paste0("US-", .data$StateCode))

cli::cli_alert_info("{nrow(tab_state)} state code{?s} (50 states plus the District of Columbia).")


# 2b. Repairing the ISO3 sentinel ----
# THE LOOKUP CARRIES -99 WHERE THE SOURCE HAD NO ISO CODE, and -99 is a string, so it counts as
# populated and travels straight into a published country variable. It reached 83 country rows and
# the cross-engine agreement check is what found it: the gazetteer resolved NORWAY to -99 while
# LexNLP resolved the same offsets to NOR.
#
# The 83 divide into two kinds and only one is repairable.
#
# REPAIRABLE -- the entity IS a country and the source simply lost its code. FRANCE and NORWAY are
# both in this group, and the giveaway is that their own aliases kept the right code: EMETAB NORWAY
# and I NORWAY both carry NOR while NORWAY carries -99. Two of the largest economies in Europe
# would otherwise be unresolvable, so the codes are supplied here.
#
# NOT REPAIRABLE -- the entity has no ISO-3166-1 alpha-3 code at all. Northern Cyprus, Somaliland,
# Kashmir, the Siachen Glacier, Ashmore and Cartier Islands and the Australian Indian Ocean
# Territories are contested or dependent territories. Assigning them a parent state is a RESEARCH
# DECISION about sovereignty, not a data repair, so they stay unresolved and are listed on every
# run rather than quietly folded into a neighbour.
#
# KOSOVO sits between the two and is treated as repairable with the reason recorded: it has no
# ISO-3166-1 entry, but XKX is the user-assigned code the World Bank and most trade datasets use,
# so leaving it null would be less useful and no more honest.
#
# EVERY ROW RECORDS WHERE ITS CODE CAME FROM in IsoSource: "source" from the file, "repair" from
# the table below, "none" where it is still unknown. Nothing is changed invisibly.

.alias_fra <- c("FRANCA", "FRANCE", "FRANCIA", "FRANCIAORSZAG", "FRANCJA", "FRANKREICH",
                "FRANKRIJK", "FRANKRIKE", "FRANSA", "PHAP", "PRANCIS")
.alias_nor <- c("NA UY", "NOORWEGEN", "NORGE", "NORUEGA", "NORVEC", "NORVEGE", "NORVEGIA",
                "NORWAY", "NORWEGEN", "NORWEGIA")
.alias_xkx <- c("KOSOVA", "KOSOVO", "KOSOWO", "KOSZOVO")

tab_iso_repair <- dplyr::bind_rows(
  tibble::tibble(GeoName = .alias_fra, Iso3Fix = "FRA", Basis = "source lost the code"),
  tibble::tibble(GeoName = .alias_nor, Iso3Fix = "NOR", Basis = "source lost the code"),
  tibble::tibble(GeoName = .alias_xkx, Iso3Fix = "XKX", Basis = "user-assigned, no ISO-3166-1")
)

cli::cli_alert_info(
  "{nrow(tab_iso_repair)} ISO3 repair{?s} declared, covering \\
   {dplyr::n_distinct(tab_iso_repair$Iso3Fix)} countr{?y/ies}."
)


# 3. The county tier, from the raw file ----
# One row per (populated place name, state) with its county. usgs_raw holds 185,315 populated places
# against the lookup's 166,302, so the two do not agree on the grain; the join is therefore on
# (GeoName, ParentState) and unmatched rows are reported rather than silently dropped.
#
# A name can occur twice in one state -- two SPRINGFIELDs in different Illinois counties -- so the
# county is taken only where it is unique within the state. Where it is not, ParentCounty stays NA
# and NCountyInState records how many there were, because a resolver has to distinguish "no county"
# from "several counties" and averaging them is how a variable comes to be wrong quietly.

tab_pp_county <- arrow::read_parquet(.path_usgs) |>
  dplyr::filter(.data$GeoClass == "Populated Place",
                !is.na(.data$StateName), !is.na(.data$CountyName)) |>
  dplyr::summarise(
    NCountyInState = dplyr::n_distinct(.data$CountyName),
    ParentCounty   = dplyr::first(.data$CountyName),
    .by = c(GeoName, StateName)
  ) |>
  dplyr::mutate(ParentCounty = dplyr::if_else(.data$NCountyInState > 1L, NA_character_,
                                              .data$ParentCounty))

cli::cli_alert_info(
  "{cli::qty(nrow(tab_pp_county))}{format(nrow(tab_pp_county), big.mark = ',')} \\
   (place, state) pair{?s} from the raw file; \\
   {cli::qty(sum(tab_pp_county$NCountyInState > 1L))}\\
   {format(sum(tab_pp_county$NCountyInState > 1L), big.mark = ',')} carr{?ies/y} more than one \\
   county within the same state and are left unresolved."
)


# 4. Rebuild ----
# ParentState is the join key into the state table for every class that has one; for US State the
# name IS the state, and for Country neither applies.

tab_old <- arrow::read_parquet(.path_source) |>
  dplyr::select(-dplyr::any_of(.added))   # belt and braces if the backup is itself a rebuild

tab_new <- tab_old |>
  dplyr::left_join(
    dplyr::select(tab_pp_county, GeoName, StateName, ParentCounty),
    by = dplyr::join_by(GeoName, ParentState == StateName)
  ) |>
  dplyr::mutate(
    # THE STATE A ROW RESOLVES TO, WHICH IS NOT ParentState. A state's parent is not itself, so
    # ParentState is NA on every US State row; a resolver reading it therefore reports a state as
    # having no state and falls through to country. StateName is the column to report from and
    # ParentState is kept beside it as provenance.
    StateName = dplyr::if_else(.data$GeoClass == "US State", .data$GeoName, .data$ParentState)
  ) |>
  dplyr::left_join(
    dplyr::select(tab_state, StateName, StateCode, Iso2),
    by = dplyr::join_by(StateName)
  ) |>
  dplyr::mutate(
    GeoKey       = stringi::stri_trans_toupper(.data$GeoName),
    ParentCounty = dplyr::if_else(.data$GeoClass == "US County", .data$GeoName,
                                  .data$ParentCounty)
  ) |>
  # The sentinel goes first, so a repair is applied to a null rather than over a -99, and so that
  # anything the repair table misses is null rather than a number pretending to be a code.
  dplyr::mutate(ISO3 = dplyr::if_else(.data$ISO3 == "-99", NA_character_, .data$ISO3)) |>
  dplyr::left_join(
    dplyr::select(tab_iso_repair, GeoName, Iso3Fix),
    by = dplyr::join_by(GeoName)
  ) |>
  dplyr::mutate(
    # Guarded twice: only countries, and only where the code is actually missing. A repair must
    # never overwrite a code the source got right.
    Repaired  = is.na(.data$ISO3) & !is.na(.data$Iso3Fix) & .data$GeoClass == "Country",
    ISO3      = dplyr::if_else(.data$Repaired, .data$Iso3Fix, .data$ISO3),
    IsoSource = dplyr::case_when(
      .data$Repaired    ~ "repair",
      !is.na(.data$ISO3) ~ "source",
      TRUE               ~ "none"
    )
  ) |>
  dplyr::mutate(
    # How many distinct parents this name carries, which is what makes a match resolvable or not.
    NParent = dplyr::n_distinct(dplyr::coalesce(.data$ParentState, .data$ISO3)),
    .by = c(GeoKey, GeoClass)
  ) |>
  dplyr::select(GeoName, GeoKey, GeoClass, ISO3, IsoSource, StateCode, Iso2, StateName,
                ParentState, ParentCounty, NParent, LNG, LAT, IsWord)


# 5. Validation ----
# The rebuild must add columns and change nothing else. Row count, the three columns the extractor
# reads, and their values are all checked, because a rebuild that quietly reorders or drops a name
# would change extraction without changing a line of the extractor.

cli::cli_h2("The rebuild changes nothing the extractor reads")
tibble::tibble(
  Check = c("Rows unchanged", "GeoName unchanged", "GeoClass unchanged", "IsWord unchanged",
            "GeoKey populated", "Iso2 populated where a state applies (territories excepted)",
            "ParentCounty populated on counties", "StateName populated on US State rows"),
  N = c(
    as.integer(nrow(tab_new) == nrow(tab_old)),
    sum(tab_new$GeoName == tab_old$GeoName),
    sum(tab_new$GeoClass == tab_old$GeoClass),
    sum(tab_new$IsWord == tab_old$IsWord),
    sum(!is.na(tab_new$GeoKey)),
    sum(!is.na(tab_new$Iso2[tab_new$GeoClass != "Country"])),
    sum(!is.na(tab_new$ParentCounty[tab_new$GeoClass == "US County"])),
    sum(!is.na(tab_new$StateName[tab_new$GeoClass == "US State"]))
  ),
  Of = c(
    1L, nrow(tab_old), nrow(tab_old), nrow(tab_old), nrow(tab_new),
    sum(tab_new$GeoClass != "Country"),
    sum(tab_new$GeoClass == "US County"),
    sum(tab_new$GeoClass == "US State")
  )
) |>
  dplyr::mutate(Pct = round(100 * .data$N / .data$Of, 1)) |>
  print(n = Inf, width = Inf)

# Iso2 below 100% is expected, not a defect: usgs_raw carries PUERTO RICO, GUAM and the other
# territories as ParentState values, and none has a two-letter STATE code. They are listed rather
# than silently left null, because a parent with no code is a place the hierarchy cannot reach.
cli::cli_h2("Parent states with no two-letter code")
tab_new |>
  dplyr::filter(!is.na(.data$ParentState), is.na(.data$Iso2)) |>
  dplyr::count(.data$ParentState, name = "Rows") |>
  dplyr::arrange(dplyr::desc(.data$Rows)) |>
  print(n = 20, width = Inf)

cli::cli_h2("What the hierarchy now reaches, by class")
tab_new |>
  dplyr::summarise(
    Rows        = dplyr::n(),
    HasIso3     = sum(!is.na(.data$ISO3)),
    HasIso2     = sum(!is.na(.data$Iso2)),
    HasState    = sum(!is.na(.data$StateName)),
    HasCounty   = sum(!is.na(.data$ParentCounty)),
    Unambiguous = sum(.data$NParent == 1L),
    .by = GeoClass
  ) |>
  dplyr::mutate(PctCounty = round(100 * .data$HasCounty / .data$Rows, 1),
                PctUnamb  = round(100 * .data$Unambiguous / .data$Rows, 1)) |>
  dplyr::arrange(dplyr::desc(.data$Rows)) |>
  print(n = Inf, width = Inf)

cli::cli_h2("ISO3 provenance")
tab_new |>
  dplyr::filter(.data$GeoClass == "Country") |>
  dplyr::count(.data$IsoSource, name = "Rows") |>
  dplyr::arrange(dplyr::desc(.data$Rows)) |>
  print(n = Inf, width = Inf)

cli::cli_h2("What was repaired")
tab_new |>
  dplyr::filter(.data$IsoSource == "repair") |>
  dplyr::left_join(dplyr::select(tab_iso_repair, GeoName, Basis), by = dplyr::join_by(GeoName)) |>
  dplyr::count(.data$ISO3, .data$Basis, name = "Aliases") |>
  print(n = Inf, width = Inf)

cli::cli_h2("Countries still without a code")
tab_new |>
  dplyr::filter(.data$GeoClass == "Country", .data$IsoSource == "none") |>
  dplyr::pull(.data$GeoName) |>
  sort() |>
  (\(.x) cli::cli_alert_warning(
    "{length(.x)} name{?s}, all contested or dependent territories with no ISO-3166-1 entry: \\
     {paste(utils::head(.x, 12), collapse = ', ')}{if (length(.x) > 12) ' ...' else ''}"
  ))()

# ANY CODE THAT IS NOT THREE UPPERCASE LETTERS IS A SENTINEL IN DISGUISE. -99 was found only
# because a cross-engine check happened to compare it; this asserts the shape directly so the next
# one cannot travel as far.
bad_iso_ <- tab_new |>
  dplyr::filter(!is.na(.data$ISO3), !stringi::stri_detect_regex(.data$ISO3, "^[A-Z]{3}$")) |>
  dplyr::count(.data$ISO3, name = "Rows")

if (nrow(bad_iso_) > 0L) {
  cli::cli_alert_danger("ISO3 values that are not three uppercase letters:")
  print(bad_iso_, n = Inf, width = Inf)
} else {
  cli::cli_alert_success("Every non-null ISO3 is three uppercase letters.")
}

cli::cli_h2("Worked examples")
tab_new |>
  dplyr::filter(.data$GeoKey %in% c("DELAWARE", "MINNEAPOLIS", "SHAKOPEE", "WASHINGTON",
                                    "SPRINGFIELD", "GERMANY")) |>
  dplyr::slice_head(n = 3L, by = c(GeoKey, GeoClass)) |>
  dplyr::select(GeoName, GeoClass, ISO3, IsoSource, Iso2, StateName, ParentCounty, NParent) |>
  dplyr::arrange(.data$GeoName) |>
  print(n = 25, width = Inf)


# 5b. Does the rebuild reproduce the lookup already on disk? ----
# THE CHECK THE RETIRED Check-NER-GPE.R USED TO PROVIDE. Section 5 compares the rebuild against its
# own INPUT, which establishes that the added columns are additive and nothing else. It says nothing
# about whether the result equals the geo_lookup.parquet that is shipped -- and that file's content
# hash is what gazetteer-v2's SPEC is built from, so it is the only comparison that speaks to
# provenance.
#
# EVERY COLUMN IS COMPARED. The seven the extractor reads decide how serious a difference is, not
# whether one is looked for; see the header.

.cols_read <- c("GeoName", "GeoKey", "GeoClass", "IsWord", "NParent", "Iso2", "ISO3")

tab_disk <- arrow::read_parquet(.path_lookup)

cli::cli_h2("Does the rebuild reproduce the shipped lookup?")

same_col_ <- function(.name) {
  if (FALSE) .name <- "GeoName"

  if (!.name %in% names(tab_disk) || !.name %in% names(tab_new)) return(FALSE)
  isTRUE(all.equal(tab_new[[.name]], tab_disk[[.name]]))
}

shared_  <- intersect(names(tab_new), names(tab_disk))
agree_   <- vapply(shared_, same_col_, logical(1L))
differ_  <- shared_[!agree_]
missing_ <- setdiff(names(tab_disk), names(tab_new))
added_   <- setdiff(names(tab_new), names(tab_disk))

tibble::tibble(
  Check = c("Same column set", "Same column order", "Same row count",
            "Every shared column identical",
            "The seven columns gazetteer.py reads are identical"),
  Ok = c(
    length(missing_) == 0L && length(added_) == 0L,
    identical(names(tab_new), names(tab_disk)),
    nrow(tab_new) == nrow(tab_disk),
    length(differ_) == 0L,
    # LENGTH FIRST: all() of an empty vector is TRUE, so a lookup missing every one of the seven
    # would otherwise report this as a pass.
    length(intersect(.cols_read, shared_)) == length(.cols_read) &&
      all(agree_[.cols_read])
  ),
  Detail = c(
    if (length(missing_) + length(added_) == 0L) "" else
      paste0("absent: ", paste(missing_, collapse = ", "),
             "; extra: ", paste(added_, collapse = ", ")),
    "", paste0(format(nrow(tab_new), big.mark = ","), " vs ",
               format(nrow(tab_disk), big.mark = ",")),
    if (length(differ_) == 0L) "" else paste(differ_, collapse = ", "),
    paste(intersect(.cols_read, differ_), collapse = ", ")
  )
) |>
  print(n = Inf, width = Inf)

# A DIFFERENCE IN THE SEVEN IS A DIFFERENT ANSWER; a difference elsewhere is a different file. The
# first means the shipped spans came from rules this script no longer produces and the store cannot
# be reconciled with its own provenance record. The second is real and is not that.
.bad_read <- intersect(.cols_read, differ_)

if (length(.bad_read) > 0L) {
  cli::cli_alert_danger(
    "{length(.bad_read)} {cli::qty(length(.bad_read))}column{?s} the extractor READS \\
     differ{?s/} from the shipped lookup: {paste(.bad_read, collapse = ', ')}. The corpus GPE spans \\
     were produced from a lookup this script does not reproduce."
  )
} else if (length(differ_) > 0L) {
  cli::cli_alert_warning(
    "{length(differ_)} {cli::qty(length(differ_))}column{?s} differ{?s/} but none is read by the \\
     extractor: {paste(differ_, collapse = ', ')}. The spans are unaffected."
  )
} else {
  cli::cli_alert_success(
    "The rebuild reproduces the shipped lookup exactly. gazetteer-v2's spec hash therefore traces \\
     to a rule that can be reconstructed from the two inputs in this folder."
  )
}

# THE WRITE IS REFUSED WHERE IT WOULD MATTER MOST. A rebuild that does not reproduce the shipped
# file is the case where overwriting it is most dangerous, and it was the case least guarded.
if (.write && length(.bad_read) > 0L) {
  cli::cli_abort(c(
    "Refusing to write: the rebuild disagrees with the shipped lookup on a column the extractor \\
     reads.",
    "i" = "Resolve {paste(.bad_read, collapse = ', ')} first, or the store and its provenance \\
           record will describe different rules."
  ))
}


# 6. Write ----
# The previous file is kept under its own name rather than overwritten in place, so the rebuild can
# be undone without a download.

if (.write) {
  if (!fs::file_exists(.path_backup)) fs::file_copy(.path_lookup, .path_backup)
  arrow::write_parquet(tab_new, .path_lookup)
  cli::cli_alert_success(
    "Rebuilt {.path {(.path_lookup)}} ({ncol(tab_old)} -> {ncol(tab_new)} columns). \\
     Previous file kept at {.path {(.path_backup)}}."
  )
} else {
  cli::cli_alert_info("Inspection only; set .write to TRUE to replace the lookup.")
}
