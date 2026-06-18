# ============================================================================
# Gazetteer lookup build -- hierarchical (USGS + world countries)
# ----------------------------------------------------------------------------
# Ports get_usgs_geoloaction + prep_geolocation_lookup (03-NamedEntities.R) into
# ONE lookup parquet for the gazetteer engine (Engine = "paper", Model =
# "gazetteer-v1"). Four classes: Country, US State, US County, US Populated Place.
#
# NO lexical exclusion. Every name is kept; disambiguation is STRUCTURAL and
# happens at MATCH time in the Python matcher, not here:
#   Country    -- always kept (RoW anchor; closed set)
#   US State   -- always kept (US anchor; closed set)
#   US County  -- kept iff its ParentState is also matched in the same document
#   US Place   -- kept iff its ParentState is also matched in the same document
# So "Mobile, Alabama" keeps Mobile (Alabama present); "mobile device" drops it
# (no parent). To support that gate, every gated name carries ParentState.
#
# IsWord is a DIAGNOSTIC column (name is also a common English word) -- NOT a
# filter; it's an extra signal the LLM adjudicator may use downstream.
#
# LNG/LAT are correct midpoints (paper's (min+max/2) parens bug fixed). The
# engine matches Span -> GeoName; metadata (ISO3/LNG/LAT/ParentState) rejoins
# downstream. No store, no Python -- just the dictionary + statistics.
# ============================================================================

dir_gaz_ <- here::here("contracts-engine", "data", "gazetteer")
fs::dir_create(dir_gaz_)
path_usgs_ <- fs::path(dir_gaz_, "usgs_raw.parquet")
path_words_ <- fs::path(dir_gaz_, "exclusion_words.txt")
path_lookup_ <- fs::path(dir_gaz_, "geo_lookup.parquet")

url_usgs_ <- "https://prd-tnm.s3.amazonaws.com/StagedProducts/GeographicNames/DomesticNames/DomesticNames_AllStates_Text.zip"
url_words_ <- "https://raw.githubusercontent.com/dwyl/english-words/master/words_alpha.txt"

# --- 1. USGS Domestic Names (download once, cache raw; keep StateName) -------
if (!fs::file_exists(path_usgs_)) {
  zip_ <- tempfile(fileext = ".zip")
  download.file(url_usgs_, destfile = zip_, mode = "wb")
  dir_un_ <- fs::path(tempdir(), "usgs_unzip")
  fs::dir_create(dir_un_)
  zip::unzip(zip_, exdir = dir_un_)
  
  fs::dir_ls(dir_un_, recurse = TRUE, glob = "*.txt") |>
    purrr::map(\(.f) readr::read_delim(.f, delim = "|", show_col_types = FALSE,
                                       col_types = readr::cols(.default = "c"))) |>
    dplyr::bind_rows() |>
    dplyr::select(
      StateName = state_name, CountyName = county_name,
      GeoName = feature_name, GeoClass = feature_class,
      LNG = prim_long_dec, LAT = prim_lat_dec
    ) |>
    dplyr::mutate(
      dplyr::across(c(LNG, LAT), as.numeric),
      dplyr::across(c(StateName, CountyName, GeoName), standardize_text)
    ) |>
    arrow::write_parquet(path_usgs_)
  cli::cli_alert_success("USGS raw cached -> {.path {fs::path_file(path_usgs_)}}")
} else {
  cli::cli_alert_info("USGS raw exists, reusing -> {.path {fs::path_file(path_usgs_)}}")
}

usgs_ <- arrow::read_parquet(path_usgs_)

# --- 2. US rows, carrying ParentState for the gated classes ------------------
# States: closed anchor set, no parent. Centroid = true midpoint (paper bug fixed).
us_states_ <- usgs_ |>
  dplyr::filter(StateName %in% toupper(state.name), !is.na(StateName)) |>
  dplyr::group_by(GeoName = StateName) |>
  dplyr::summarise(LNG = (min(LNG, na.rm = TRUE) + max(LNG, na.rm = TRUE)) / 2,
                   LAT = (min(LAT, na.rm = TRUE) + max(LAT, na.rm = TRUE)) / 2,
                   .groups = "drop") |>
  dplyr::mutate(ISO3 = "USA", GeoClass = "US State", ParentState = NA_character_)

# County: parent = its state. A county name can span states -> one row per
# (county, state) so the gate can check the right parent; collapse coords.
us_counties_ <- usgs_ |>
  dplyr::filter(!is.na(CountyName), StateName %in% toupper(state.name)) |>
  dplyr::group_by(GeoName = CountyName, ParentState = StateName) |>
  dplyr::summarise(LNG = (min(LNG, na.rm = TRUE) + max(LNG, na.rm = TRUE)) / 2,
                   LAT = (min(LAT, na.rm = TRUE) + max(LAT, na.rm = TRUE)) / 2,
                   .groups = "drop") |>
  dplyr::mutate(ISO3 = "USA", GeoClass = "US County")

# Place: parent = its state. Same (place, state) grain.
us_places_ <- usgs_ |>
  dplyr::filter(GeoClass == "Populated Place", !is.na(GeoName),
                StateName %in% toupper(state.name)) |>
  dplyr::group_by(GeoName, ParentState = StateName) |>
  dplyr::summarise(LNG = mean(LNG, na.rm = TRUE), LAT = mean(LAT, na.rm = TRUE),
                   .groups = "drop") |>
  dplyr::mutate(ISO3 = "USA", GeoClass = "US Populated Place")

# --- 3. World countries (closed anchor set, no parent) -----------------------
countries_cc_ <- countrycode::countryname_dict |>
  dplyr::inner_join(
    dplyr::select(countrycode::codelist, ISO3 = iso3c, country.name.en),
    by = dplyr::join_by(country.name.en)
  ) |>
  tidyr::pivot_longer(dplyr::matches("name"), names_to = NULL, values_to = "GeoName") |>
  dplyr::filter(!is.na(GeoName)) |>
  dplyr::transmute(ISO3, GeoClass = "Country",
                   GeoName = standardize_text(GeoName), LNG = NA_real_, LAT = NA_real_) |>
  dplyr::filter(stringi::stri_enc_isascii(GeoName)) |>
  dplyr::distinct()

countries_ne_ <- tibble::as_tibble(rnaturalearthdata::countries50) |>
  sf::st_drop_geometry() |>
  dplyr::select(ISO3 = iso_a3, LNG = label_x, LAT = label_y, dplyr::matches("name")) |>
  dplyr::select(-dplyr::any_of("name_len")) |>
  tidyr::pivot_longer(dplyr::matches("name"), names_to = NULL, values_to = "GeoName") |>
  dplyr::filter(!is.na(GeoName)) |>
  dplyr::transmute(ISO3, GeoClass = "Country",
                   GeoName = standardize_text(GeoName), LNG, LAT) |>
  dplyr::filter(stringi::stri_enc_isascii(GeoName)) |>
  dplyr::distinct()

countries_ <- dplyr::bind_rows(countries_ne_, countries_cc_) |>
  dplyr::group_by(ISO3) |>
  tidyr::fill(c(LNG, LAT), .direction = "downup") |>
  dplyr::ungroup() |>
  dplyr::distinct(ISO3, GeoClass, GeoName, .keep_all = TRUE) |>
  dplyr::mutate(ParentState = NA_character_)

# --- 4. Combine + base clean -------------------------------------------------
# Dedup grain includes ParentState so (county/place, state) pairs are preserved;
# countries/states (NA parent) dedup on name alone within their class.
lookup_raw_ <- dplyr::bind_rows(us_states_, us_counties_, us_places_, countries_) |>
  dplyr::filter(!is.na(GeoName), !grepl("\\d", GeoName), nchar(GeoName) > 2) |>
  dplyr::distinct(GeoClass, GeoName, ParentState, .keep_all = TRUE) |>
  dplyr::select(GeoName, GeoClass, ISO3, ParentState, LNG, LAT)

# --- 5. Diagnostic IsWord flag (NOT a filter) --------------------------------
if (!fs::file_exists(path_words_)) {
  download.file(url_words_, destfile = path_words_, mode = "wb")
}
words_excl_ <- readr::read_lines(path_words_) |>
  stringi::stri_replace_all_regex("\\r", "") |>
  standardize_text() |>
  unique()
words_excl_ <- words_excl_[nchar(words_excl_) > 0]

lookup_final_ <- lookup_raw_ |>
  dplyr::mutate(IsWord = as.integer(GeoName %in% words_excl_)) |>
  dplyr::arrange(GeoClass, GeoName, ParentState)

arrow::write_parquet(lookup_final_, path_lookup_)

# ============================================================================
# STATISTICS -- audit the hierarchical dictionary before building the matcher
# ============================================================================
cli::cli_h1("Gazetteer lookup statistics (hierarchical)")

cli::cli_h3("Per-class size + how many names are also common words (IsWord)")
lookup_final_ |>
  dplyr::group_by(GeoClass) |>
  dplyr::summarise(
    Rows = dplyr::n(),
    DistinctNames = dplyr::n_distinct(GeoName),
    AlsoWord = sum(IsWord),
    WordShare = round(mean(IsWord), 3),
    Gated = GeoClass[1] %in% c("US County", "US Populated Place"),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(Rows)) |>
  print()

cli::cli_h3("Totals")
cli::cli_alert_info("Rows: {nrow(lookup_final_)}  |  distinct names: {dplyr::n_distinct(lookup_final_$GeoName)}")

cli::cli_h3("Parent coverage for gated classes (every gated row needs a ParentState)")
lookup_final_ |>
  dplyr::filter(GeoClass %in% c("US County", "US Populated Place")) |>
  dplyr::group_by(GeoClass) |>
  dplyr::summarise(N = dplyr::n(),
                   HasParent = sum(!is.na(ParentState)),
                   MissingParent = sum(is.na(ParentState)),
                   .groups = "drop") |>
  print()

cli::cli_h3("Multi-state names (same place/county name across >1 state)")
lookup_final_ |>
  dplyr::filter(GeoClass %in% c("US County", "US Populated Place")) |>
  dplyr::count(GeoClass, GeoName, name = "NStates") |>
  dplyr::filter(NStates > 1) |>
  dplyr::count(GeoClass, name = "MultiStateNames") |>
  print()

cli::cli_h3("Name-length distribution (short names = match-noise risk)")
lookup_final_ |>
  dplyr::mutate(Len = nchar(GeoName)) |>
  dplyr::count(Len) |>
  dplyr::arrange(Len) |>
  print(n = 12)

cli::cli_h3("Common-word names KEPT in closed anchor sets (gate protects them)")
lookup_final_ |>
  dplyr::filter(IsWord == 1L, GeoClass %in% c("Country", "US State")) |>
  dplyr::distinct(GeoClass, GeoName) |>
  dplyr::slice_sample(n = 15) |>
  print(n = 15)

cli::cli_h3("Common-word names in GATED classes (kept in dict, gated at match time)")
lookup_final_ |>
  dplyr::filter(IsWord == 1L, GeoClass %in% c("US County", "US Populated Place")) |>
  dplyr::distinct(GeoClass, GeoName) |>
  dplyr::slice_sample(n = 15) |>
  print(n = 15)

cli::cli_alert_success("Lookup written -> {.path {fs::path_file(path_lookup_)}} ({nrow(lookup_final_)} rows)")