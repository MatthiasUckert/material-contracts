# 1_code/021-PaperOutputs.R ----------------------------------------------------
# Function file for 021-PaperOutputs.qmd.
# Every artifact that appears IN THE PAPER (figures + R-side tables) is produced
# here and nowhere else, and since 2026-08-06 this document also SHIPS them:
# 020-ExportData carries data only.
#
# Design: this file does NOT copy plot code. It SOURCES the existing function
# files (003a, 010) into separate environments and hands back the functions it
# needs - one definition, no drift, and the plot_fire_map() name collision
# between 003a (takes a path) and 010 (takes a data frame) is resolved
# explicitly instead of silently.
#
# Two versions of every sample-dependent artifact:
#   FULL         the geocoded sample, 15,956 firms / 134,717 firm-years
#   EXCL-DAMAGE  minus every firm disclosing wildfire damage in ANY year
#                (paper 3.1), 15,648 firms / 129,717 firm-years
# Fire-level artifacts carry NO suffix - the absence is the signal that the
# sample does not enter them.


# Paths ------------------------------------------------------------------------

#' Which names in pap_paths() are inputs, and which are not
#'
#' The .qmd asserts that its declared `.lstPaths$Input` tree matches
#' `pap_paths()` element by element. That assertion used a hardcoded exclusion
#' list, so every new output path broke it. Declared here instead, next to the
#' paths themselves.
pap_path_kinds <- function() {
  list(
    # Real inputs - must appear in the .qmd's .lstPaths$Input
    inputs  = c("Spine", "Match", "Variables", "VarsSelect", "WildFires",
                "Compustat", "FilterAnnual", "SampleStreet", "Edgar",
                "Store", "StoreBadFiles", "FelixTables"),
    # Resolves to the same file as another input; not declared separately
    aliases = c("WildFiresCln"),
    # Written, not read
    outputs = c("OutputRoot", "FigLocal", "TabLocal", "FigDest", "TabDest",
                "FigArchive", "TabArchive"),
    # Declared for reference, never opened by this document
    unused  = c("FelixDta")
  )
}

#' Subfolder layout, mirrored 1:1 between local and Dropbox
#'
#' Mirrors Felix's own 2_tables structure so a reader moving between his tree
#' and ours does not have to re-learn it.
pap_groups <- function() {
  list(
    figures = c("main", "descriptives", "oa", "splits"),
    tables  = c("descriptives", "main", "oa")
  )
}

#' @param .vintage which 002g vintage the paper describes. V1 is what every
#'   number in the current draft is built on - proven 2026-08-10 by mtime, by the
#'   zero-filled nStatesUni mean (4.957 against 4.96 observed) and by row-level
#'   agreement of 1.0000 against V1's GeoDispersion versus 0.912 against V2's.
#'   Switching the paper to V2 is this one argument.
pap_paths <- function(.dir_dropbox, .vintage = "V1") {
  h_ <- function(...) as.character(here::here(...))

  # FireStata lives BESIDE FireData, like FireTables did. Derived, never
  # hardcoded: the previous FelixTables entry was a literal /Users/... path and
  # could not resolve on the E:/ workstation at all.
  stata_ <- file.path(dirname(.dir_dropbox), "FireStata")
  fig_   <- file.path(stata_, "3A_figures", "Matthias")
  tab_   <- file.path(stata_, "3B_tables",  "Matthias")

  list(
    # Inputs -- -- -- -- --
    Spine        = h_("2_output/003a-PrepareDatasets/DataSets/StaggeredAnnualDiD/6.0-DG-FALSE-FALSE-FALSE.parquet"),
    Match        = h_("2_output/003a-PrepareDatasets/Cache/Matchings/DistanceMatch.parquet"),
    # 020 assembles these now, one panel per vintage. 003a does matching and the
    # 220 spine datasets only.
    Variables    = h_("2_output/020-ExportData/Output",
                      paste0("variables_full_new_", .vintage, ".parquet")),
    VarsSelect   = h_("2_output/020-ExportData/Output",
                      paste0("variables_select_ann_", .vintage, ".parquet")),
    WildFires    = h_("2_output/002b-CleanFires/Output/wildfire_final.parquet"),
    # Same file as WildFires now: 002b moved wildfire_clean to Cache/ and everything
    # downstream reads wildfire_final. Kept as a separate entry so the Figure 2
    # Panel A block does not need editing; both names resolve to the one dataset.
    WildFiresCln = h_("2_output/002b-CleanFires/Output/wildfire_final.parquet"),
    Compustat    = h_("2_output/000a-GetCompustat/Output/CompustatAnnual.parquet"),
    FilterAnnual = h_("2_output/001b-GetSamples/Cache/filter_ann.parquet"),
    SampleStreet = h_("2_output/001b-GetSamples/Output/sample_ann_street.parquet"),
    Edgar        = file.path(.dir_dropbox, "InputData/SEC-Headers/edgar_headers.parquet"),
    # The DuckDB text store. Opened READ ONLY, and only the `reports` table -
    # never `units`, which is 518M rows. Answers "which sample firm-years
    # actually have a filing", which nothing else in the pipeline persists.
    Store         = h_(paste0("2_output/002g-GetDisclosures-", .vintage),
                       "Store", "pfire_text.duckdb"),
    StoreBadFiles = h_(paste0("2_output/002g-GetDisclosures-", .vintage),
                       "Output", "StoreBadFiles.parquet"),
    # Tier 2 (Felix): his current run. Since his 2026-08 reorganisation the CSVs
    # live in descriptives/ main/ oa/ splits/, so every read must recurse.
    FelixTables  = file.path(stata_, "2_tables"),
    FelixDta     = file.path(dirname(.dir_dropbox), "FireFelix", "datasets"),

    # Outputs -- -- -- -- --
    OutputRoot = h_("2_output/021-PaperOutputs/Output"),
    FigLocal   = h_("2_output/021-PaperOutputs/Output/figures"),
    TabLocal   = h_("2_output/021-PaperOutputs/Output/tables"),
    FigDest    = fig_,
    TabDest    = tab_,
    # _archive sits BESIDE the group folders it protects, never inside one, so
    # the mirror can never sweep the archive it just wrote.
    FigArchive = file.path(fig_, "_archive"),
    TabArchive = file.path(tab_, "_archive")
  )
}

#' Create the local and destination folder trees
pap_dirs_init <- function(.paths, .create_dest = TRUE) {
  grp_ <- pap_groups()

  purrr::walk(grp_$figures, ~ fs::dir_create(file.path(.paths$FigLocal, .x)))
  purrr::walk(grp_$tables,  ~ fs::dir_create(file.path(.paths$TabLocal, .x)))

  if (isTRUE(.create_dest)) {
    purrr::walk(grp_$figures, ~ fs::dir_create(file.path(.paths$FigDest, .x)))
    purrr::walk(grp_$tables,  ~ fs::dir_create(file.path(.paths$TabDest, .x)))
    fs::dir_create(.paths$FigArchive)
    fs::dir_create(.paths$TabArchive)
  }

  invisible(NULL)
}


# Variants ---------------------------------------------------------------------

#' The two sample definitions, in the order artifacts should be produced
pap_variants <- function() {
  tibble::tibble(
    variant = c("FULL", "EXCL-DAMAGE"),
    label   = c("Full geocoded sample",
                "Excluding firms that disclose wildfire damage in any year"),
    exclude = c(FALSE, TRUE)
  )
}

#' Firms excluded by the paper's damage rule
#'
#' Paper 3.1: "any firm that discloses wildfire-related impacts, damages, or
#' disruptions in their 10-K filings in ANY year during our sample period". So
#' the rule is firm-level, not firm-year-level, and one damage year removes the
#' firm's whole series. Matches exp_gate_variables()'s F9 count exactly - the
#' two must not be allowed to drift.
#'
#' Note the coverage caveat: s5_assemble() NA-fills every *Bin with 0, so a
#' firm-year with no 10-K carries DamageBin == 0 and survives this rule. The
#' exclusion is therefore conditional on having a filing. pap_report_coverage()
#' quantifies that.
pap_damage_firms <- function(.path_variables) {
  if (FALSE) {
    .path_variables <- .P$Variables
  }

  vars_ <- arrow::open_dataset(.path_variables) |>
    dplyr::select(gvkey, DamageBin) |>
    dplyr::collect()

  ever_ <- vars_ |>
    dplyr::summarise(DamageEver = as.integer(any(DamageBin == 1, na.rm = TRUE)),
                     .by = gvkey)

  out_ <- ever_$gvkey[ever_$DamageEver == 1]

  attr(out_, "n_firms")      <- length(out_)
  attr(out_, "n_firm_years") <- sum(vars_$gvkey %in% out_)
  attr(out_, "n_damage_rows") <- sum(vars_$DamageBin == 1, na.rm = TRUE)
  attr(out_, "n_rows_total")  <- nrow(vars_)

  n_fy_  <- attr(out_, "n_firm_years")
  n_dmg_ <- attr(out_, "n_damage_rows")

  # cli::qty() sets the quantity {?s} agrees with. Without it the plural is
  # taken from the last interpolation, and scales::comma() returns a STRING -
  # so every one of these printed as a singular.
  cli::cli_alert_info(
    "damage rule: {length(out_)} firm{?s} / \\
     {cli::qty(n_fy_)}{scales::comma(n_fy_)} firm-year{?s} \\
     ({round(100 * n_fy_ / nrow(vars_), 2)}%), \\
     from {cli::qty(n_dmg_)}{scales::comma(n_dmg_)} row{?s} carrying an \\
     actual damage disclosure"
  )

  out_
}

#' Restrict a firm-year table to a variant
pap_apply_variant <- function(.tab, .variant, .damage_firms) {
  if (identical(.variant, "FULL")) return(.tab)
  if (!"gvkey" %in% names(.tab)) {
    stop("pap_apply_variant(): no gvkey column - cannot apply the damage rule",
         call. = FALSE)
  }
  dplyr::filter(.tab, !gvkey %in% .damage_firms)
}

#' A slim, variant-filtered copy of the spine, on disk
#'
#' plot_fire_map() and plot_firm_map() in 003a both take a PATH, not a table.
#' Rather than duplicate their code here (the whole point of pap_producers()),
#' write the handful of columns they read to a temporary parquet and hand them
#' that path. Tiny: eight columns of a 134,717-row table.
pap_spine_variant <- function(.path_spine, .variant, .damage_firms, .dir_tmp = tempdir()) {
  if (FALSE) {
    .path_spine   <- .P$Spine
    .variant      <- "EXCL-DAMAGE"
    .damage_firms <- pap_damage_firms(.P$Variables)
    .dir_tmp      <- tempdir()
  }

  cols_ <- c("gvkey", "datadate", "FireID", "FireSizeClass",
             "FireLNG", "FireLAT", "RatioToMid", "FirmLNG", "FirmLAT")

  tab_ <- arrow::read_parquet(.path_spine, col_select = dplyr::all_of(cols_)) |>
    dplyr::ungroup() |>
    pap_apply_variant(.variant, .damage_firms)

  path_ <- file.path(.dir_tmp, paste0("spine_", .variant, ".parquet"))
  arrow::write_parquet(tab_, path_)

  n_row_  <- nrow(tab_)
  n_firm_ <- dplyr::n_distinct(tab_$gvkey)
  n_fire_ <- dplyr::n_distinct(tab_$FireID[tab_$FireID != 0 & !is.na(tab_$FireID)])

  cli::cli_alert_info(
    "{(.variant)} spine slice: {cli::qty(n_row_)}{scales::comma(n_row_)} row{?s}, \\
     {cli::qty(n_firm_)}{scales::comma(n_firm_)} firm{?s}, \\
     {cli::qty(n_fire_)}{scales::comma(n_fire_)} distinct fire{?s}"
  )

  path_
}


# Descriptives, absorbed from 010 ---------------------------------------------
#
# 010-Descriptives.R was retired to _BackUps/ and 021 was the only consumer of
# five of its functions. Rather than leave a live document sourcing a backup
# directory, or add a stray helper file, they live here now.
#
# Renamed on the way in, because 003a defines plot_fire_map() and get_us_map()
# TOO, with a different signature - 003a's fire map takes a PATH, this one takes
# a TABLE. pap_producers() still exposes both, as fire_map_path and
# fire_map_data. get_us_map() was byte-identical in the two files.
#
#   get_sample_selection_table -> pap_sample_selection_table
#   prep_FireSizeClasses       -> pap_fire_size_classes
#   prep_FireStates            -> pap_fire_states
#   plot_fires_by_year         -> pap_fires_by_year
#   plot_fire_development      -> pap_fire_development   (helper of the above)
#   plot_fire_map              -> pap_fire_map_data
#   get_us_map                 -> pap_us_map             (helper of the above)
#
# Converted from magrittr to |> in the move. Checked first: 45 pipes, every one
# targeting a parenthesised call, no dot placeholders, no unprefixed tidyverse
# verbs. `var_summary()` comes from source/00-Utils.R, which the .qmd sources.

pap_sample_selection_table <- function(.path_compustat,
                                      .path_edgar,
                                      .path_compustat_filtered,
                                      .path_sample_street,
                                      .fyear_range = c(1993L, 2022L)) {
  # Funnel stages read straight from the artifacts the pipeline materialises
  # (no per-CIK header scan). Mirrors 001b: Compustat -> EDGAR filers ->
  # firm-years inside the filing window -> geocoded street sample.
  read_ <- function(.path) {
    arrow::open_dataset(.path) |>
      dplyr::select(gvkey, datadate, fyear) |>
      dplyr::filter(dplyr::between(fyear, .fyear_range[1], .fyear_range[2])) |>
      dplyr::collect()
  }

  tab0_ <- read_(.path_compustat)

  gvkeys_edgar_ <- arrow::open_dataset(.path_edgar) |>
    dplyr::distinct(gvkey) |>
    dplyr::collect() |>
    dplyr::pull(gvkey)
  tab1_ <- dplyr::filter(tab0_, gvkey %in% gvkeys_edgar_)

  tab2_ <- read_(.path_compustat_filtered)
  tab3_ <- read_(.path_sample_street)

  tibble::tribble(
    ~Description, ~nObs, ~Firms,
    "Compustat Universe (1993 - 2022)", nrow(tab0_), dplyr::n_distinct(tab0_$gvkey),
    "  Less: Firms without EDGAR filings", nrow(tab1_), dplyr::n_distinct(tab1_$gvkey),
    "  Less: Firm-years outside EDGAR filing window", nrow(tab2_), dplyr::n_distinct(tab2_$gvkey),
    "  Less: No geocoded street address", nrow(tab3_), dplyr::n_distinct(tab3_$gvkey)
  ) |>
    dplyr::mutate(
      nObs = dplyr::if_else(dplyr::row_number() == 1, nObs, c(0L, diff(nObs))),
      Firms = dplyr::if_else(dplyr::row_number() == 1, Firms, c(0L, diff(Firms)))
    ) |>
    janitor::adorn_totals(name = "Final Sample") |>
    tibble::as_tibble()
}


if (FALSE) {
  .path_wildfires <- .lstPaths$Input$WildFiresFinal
}
pap_fire_size_classes <- function(.path_wildfires) {
  tab_fire_fin <- arrow::open_dataset(.path_wildfires) |>
    dplyr::select(FireSizeClass, FireRadius, FireState, FireRadius) |>
    dplyr::collect()
  
  dplyr::bind_rows(
    purrr::map(
      .x = purrr::set_names(1:7, LETTERS[1:7]),
      .f = ~ tab_fire_fin |>
        dplyr::filter(FireSizeClass == LETTERS[.x]) |>
        dplyr::select(FireRadius) |>
        var_summary(),
      .progress = TRUE
    ) |> dplyr::bind_rows(.id = "FireSizeClass"),
    purrr::map(
      .x = purrr::set_names(1:7, LETTERS[1:7]),
      .f = ~ tab_fire_fin |>
        dplyr::filter(FireSizeClass %in% LETTERS[.x:7]) |>
        dplyr::select(FireRadius) |>
        var_summary(),
      .progress = TRUE
    ) |> dplyr::bind_rows(.id = "FireSizeClass") |>
      dplyr::mutate(FireSizeClass = paste0(FireSizeClass, "G"))
  ) |>
    dplyr::select(
      FireSizeClass,
      nFires = nobs,
      MeanRadius = mean,
      MedianRadius = p50,
      Percentile10 = p10,
      Percentile25 = p25,
      Percentile75 = p75,
      percentile90 = p90
    )
}

if (FALSE) {
  .path_wildfires <- .lstPaths$Input$WildFiresFinal
}
pap_fire_states <- function(.path_wildfires) {
  tab_fire_fin <- arrow::open_dataset(.path_wildfires) |>
    dplyr::select(FireSizeClass, FireRadius, FireState, FireRadius) |>
    dplyr::collect()
  
  tab_fire_fin |>
    dplyr::select(FireState, FireRadius) |>
    tidyr::nest(.by = FireState) |>
    dplyr::mutate(
      overview = purrr::map(
        .x = data,
        .f = var_summary,
        .progress = TRUE
      ),
      data = NULL
    ) |>
    tidyr::unnest(overview) |>
    dplyr::select(
      FireState,
      nFires = nobs,
      MeanRadius = mean,
      MedianRadius = p50,
      Percentile10 = p10,
      Percentile25 = p25,
      Percentile75 = p75,
      percentile90 = p90
    ) |>
    tidyr::replace_na(list(FireState = "State not Available")) |>
    dplyr::arrange(-nFires) |>
    dplyr::mutate(Rank = dplyr::dense_rank(-nFires), .before = FireState)
}



pap_fire_development <- function(.tab, .yname) {
  .tab |>
    ggplot2::ggplot(ggplot2::aes(x = Year, y = Value)) +
    ggplot2::geom_line(
      linewidth = .25,
      color = "grey50"
    ) +
    ggplot2::geom_point(
      size = .8,
      color = "darkred"
    ) + 
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Times", size = 10),
      axis.title = ggplot2::element_text(family = "Times", size = 10),
      axis.text = ggplot2::element_text(family = "Times", size = 9),
      title = ggplot2::element_text(family = "Times", size = 12),
      legend.text = ggplot2::element_text(family = "Times", size = 9),
      legend.title = ggplot2::element_blank(),
      legend.position = "top",
      # panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::geom_smooth(color = "darkred", linewidth = .5) +
    ggplot2::labs(
      y = .yname
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(seq(1993, 2017, 4), 2020)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),  # Start at 0, NA means use the data maximum
      breaks = scales::pretty_breaks(),
      labels = scales::comma
    )
}

if (FALSE) {
  .path_wildfires <- .lstPaths$Input$WildFiresFinal
}
pap_fires_by_year <- function(.path_wildfires) {
  tab_fire_fin <- arrow::open_dataset(.path_wildfires) |>
    dplyr::select(FireSizeClass, FireRadius, FireState, FireRadius, FireYear, FireSize) |>
    dplyr::collect()
  
  let <- c(
    purrr::map(1:7, ~ LETTERS[.x]),
    purrr::map(1:6, ~ LETTERS[.x:7])
  ) |> purrr::set_names(c(
    purrr::map_chr(1:7, ~ paste0(LETTERS[.x], LETTERS[.x])),
    purrr::map_chr(1:6, ~ paste0(LETTERS[.x], "G"))
  ))
  
  tab_fire_years <- purrr::map(
    .x = let,
    .f = ~ tab_fire_fin |>
      dplyr::filter(FireYear >= 1993) |>
      dplyr::filter(FireSizeClass %in% .x) |>
      dplyr::select(FireSizeClass, Year = FireYear, FireSize) |>
      dplyr::group_by(Year) |>
      dplyr::summarise(
        nFire = dplyr::n(),
        FireArea = sum(FireSize) / 1e6
      ),
    .progress = TRUE
  ) |>
    dplyr::bind_rows(.id = "FireSizeClass") |>
    tidyr::pivot_longer(c(nFire, FireArea), names_to = "Variable", values_to = "Value") |>
    tidyr::nest(.by = c(FireSizeClass, Variable)) |>
    dplyr::mutate(
      yAxis = dplyr::if_else(Variable == "nFire", "Number of Fires", "Fire Area in Square KiloMeters")
    ) |>
    dplyr::mutate(plot = purrr::map2(
      .x = data,
      .y = yAxis,
      .f = pap_fire_development
    ))
}




pap_us_map <- function() {
  usa_map <- ggplot2::map_data("state")
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = usa_map,
      ggplot2::aes(x = long, y = lat, group = group),
      fill = "white", color = "lightgrey"
    ) +
    ggplot2::coord_fixed(1.3) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::unit(c(0.2, 0, 0, 0), "cm"),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(c(0, 0, 0, 0), "cm")
    )
}


pap_fire_map_data <- function(.tab, .title = NULL, .sample = 500000) {
  # Get unique fire size classes from the data
  unique_classes <- sort(unique(.tab$FireSizeClass))
  
  # Create color palette only for existing classes
  fire_colors <- setNames(
    grDevices::colorRampPalette(c("#ffcdd2", "#b71c1c"))(length(unique_classes)),
    unique_classes
  )
  
  # Create sizes only for existing classes
  fire_sizes <- setNames(
    seq(.5, 2, length.out = length(unique_classes)),
    unique_classes
  )
  
  set.seed(123)
  
  pap_us_map() +
    ggplot2::geom_point(
      data = dplyr::slice_sample(dplyr::filter(.tab, FireLAT <= 50, FireLNG >= -125, FireLAT >= 20), n = .sample),
      ggplot2::aes(
        x = FireLNG,
        y = FireLAT,
        color = FireSizeClass,
        size = FireSizeClass
      )
    ) +
    ggplot2::scale_color_manual(
      values = fire_colors,
      name = "Fire Size Class"
    ) +
    ggplot2::scale_size_manual(
      values = fire_sizes,
      name = "Fire Size Class"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = fire_sizes)),
      size = "none"
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(x, "°"),
      name = "Longitude"
    ) +
    ggplot2::scale_y_continuous(
      labels = function(y) paste0(y, "°"),
      name = "Latitude"
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", color = "white"),
      plot.background = ggplot2::element_rect(fill = "white", color = "white")
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Times", size = 10),
      axis.title = ggplot2::element_text(family = "Times", size = 10),
      axis.text = ggplot2::element_text(family = "Times", size = 9),
      title = ggplot2::element_text(family = "Times", size = 12),
      legend.text = ggplot2::element_text(family = "Times", size = 9),
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(.title) 
  
  
}

# Producers --------------------------------------------------------------------

pap_producers <- function() {
  # parent = globalenv() and 00-Utils sourced into each environment, so the
  # functions borrowed below resolve their helpers no matter where
  # pap_producers() is called from.
  #
  # new.env() takes parent = parent.frame(), i.e. the CALLER's frame, so the
  # lookup chain used to depend on the call site. That was invisible while
  # 010-Descriptives.R called rStats::var_summary - a namespaced call resolves
  # from anywhere. Once that became a bare var_summary (the private packages
  # were retired on 2026-07-31) it had to be findable lexically, and
  # prep_FireSizeClasses() failed with "could not find function var_summary".
  new_env_ <- function() {
    e_ <- new.env(parent = globalenv())

    # 00-Utils and 003a are pre-|> code: hundreds of uses of magrittr's %>%
    # between them, and ZERO bare tidyverse calls (measured 2026-08-06). So the
    # only thing they need from the search path is the pipe itself. (010's
    # functions were absorbed above and converted to |>, so they no longer
    # participate in this.)
    #
    # The setup chunk used to run library(dplyr), which supplied %>% as a side
    # effect nobody had written down - so removing that call to comply with the
    # house prefix convention broke prep_FireSizeClasses() at call time, well
    # after sourcing had appeared to succeed. Injecting the pipe here makes the
    # dependency explicit and confines it to the environments that actually have
    # it, instead of attaching a whole package to the global search path.
    #
    # Delete this line once 003a and 00-Utils migrate to |>.
    assign("%>%", magrittr::`%>%`, envir = e_)

    sys.source(here::here("1_code", "source", "00-Utils.R"), envir = e_)
    e_
  }

  # Only 003a is borrowed now. 010-Descriptives.R was retired to _BackUps/ and
  # its five still-used functions were absorbed into this file above, renamed to
  # pap_* so they do not collide with 003a's plot_fire_map() and get_us_map().
  env_003a_ <- new_env_()
  sys.source(here::here("1_code", "003a-PrepareDatasets.R"), envir = env_003a_)

  list(
    # from 003a
    fire_map_path  = get("plot_fire_map",  envir = env_003a_),  # .path  -> fire map
    firm_map_path  = get("plot_firm_map",  envir = env_003a_),  # .path  -> treat/control map
    dist_to_mid    = get("plot_dist_to_mid", envir = env_003a_),
    event_dem_rep  = get("plot_event_dem_vs_rep", envir = env_003a_),
    event_single   = get("plot_event_single", envir = env_003a_),
    read_ts        = get("read_timeseries_tables", envir = env_003a_),
    # absorbed from 010, defined above in this file
    fire_map_data  = pap_fire_map_data,                         # .tab   -> fire map
    fires_by_year  = pap_fires_by_year,
    fire_sizes     = pap_fire_size_classes,
    fire_states    = pap_fire_states,
    sample_table   = pap_sample_selection_table
  )
}


# Felix / Stata CSV ------------------------------------------------------------

#' Is Felix's tree usable, and which version of it is this?
#'
#' RECURSES. Before 2026-08 his CSVs sat flat in 2_tables/; they now live in
#' descriptives/ main/ oa/ splits/, and the old non-recursive dir_ls() returned
#' zero files - which, because every Tier 2 chunk is wrapped in
#' `if (isTRUE(gate$ok))`, skipped all of Figure 1B, A3 and A4 in silence.
#'
#' Timestamps on that tree are Dropbox re-sync artifacts and cannot be used for
#' staleness, so the gate reports a content fingerprint instead.
pap_felix_ready <- function(.dir, .expect_files = 5L) {
  if (!fs::dir_exists(.dir)) return(list(ok = FALSE, why = "directory not found", n = 0L))

  csv_ <- fs::dir_ls(.dir, glob = "*.csv", recurse = TRUE, type = "file")
  if (length(csv_) < .expect_files) {
    return(list(ok = FALSE,
                why = paste("only", length(csv_), "CSV files (searched recursively)"),
                n = length(csv_)))
  }

  hash_ <- digest::digest(
    paste(sort(fs::path_rel(csv_, .dir)),
          vapply(sort(csv_), function(f) digest::digest(f, algo = "xxhash64", file = TRUE),
                 character(1)),
          collapse = "|"),
    algo = "xxhash64"
  )

  list(ok = TRUE,
       why = paste0(length(csv_), " CSV files, fingerprint ", substr(hash_, 1, 8)),
       n = length(csv_), hash = hash_, files = as.character(csv_))
}

#' Locate one of Felix's CSVs by stem, wherever he has filed it
#'
#' Returns NA_character_ rather than erroring, so a missing source degrades to
#' one skipped figure with a named reason instead of a failed render.
pap_felix_find <- function(.dir, .stem) {
  hits_ <- fs::dir_ls(.dir, glob = paste0("*", .stem, ".csv"),
                      recurse = TRUE, type = "file")
  hits_ <- hits_[fs::path_ext_remove(fs::path_file(hits_)) == .stem]

  if (length(hits_) == 0) {
    cli::cli_alert_warning("Felix CSV not found: {(.stem)}.csv")
    return(NA_character_)
  }
  if (length(hits_) > 1) {
    cli::cli_alert_warning(
      "{(.stem)}.csv found {length(hits_)} times - using {fs::path_rel(hits_[1], .dir)}"
    )
  }
  as.character(hits_[1])
}

# Parser: current Stata CSV export --------------------------------------------
# Layout: row 1 = model ids "(1) (2) ...", row 2 = dependent variable per model,
# then coefficient blocks - a labelled row with estimates, followed by an
# unlabelled row with (standard errors). Cells arrive as ="value".
# Returns tidy: term, model, depvar, estimate, std_err, event_time.

pap_read_stata_csv <- function(.path) {
  # Stata's export writes ragged rows, so readr emits a bare "one or more
  # parsing issues" warning that says nothing about which file or what went
  # wrong. Capture it and report the actual problem table instead - a parse
  # issue in a coefficient block would otherwise be indistinguishable from one
  # in a footer row nobody reads.
  raw_ <- suppressWarnings(readr::read_csv(
    .path, col_names = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  ))

  prob_ <- readr::problems(raw_)
  if (nrow(prob_) > 0) {
    cli::cli_alert_warning(
      "{fs::path_file(.path)}: {nrow(prob_)} parse issue{?s} \\
       (rows {min(prob_$row)}-{max(prob_$row)}); \\
       check these are footer rows, not coefficients"
    )
  }
  cln_ <- raw_ |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub('^=|"', "", .x))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::if_else(.x == "", NA_character_, .x)))

  val_cols_ <- setdiff(names(cln_), "X1")
  models_   <- as.character(cln_[1, val_cols_])
  # Stata wraps long variable names, so a depvar cell can arrive with embedded
  # newlines. Flatten before it becomes a grouping key.
  depvars_  <- trimws(gsub("[\r\n]+", " ", as.character(cln_[2, val_cols_])))

  body_ <- cln_[-c(1, 2), ] |>
    dplyr::mutate(term = X1) |>
    tidyr::fill(term, .direction = "down") |>
    dplyr::filter(!is.na(term)) |>
    dplyr::group_by(term) |>
    dplyr::mutate(kind = dplyr::if_else(dplyr::row_number() == 1, "estimate", "std_err")) |>
    dplyr::ungroup()

  body_ |>
    tidyr::pivot_longer(dplyr::all_of(val_cols_), names_to = "col", values_to = "value") |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(
      model  = models_[match(col, val_cols_)],
      depvar = depvars_[match(col, val_cols_)],
      value  = suppressWarnings(as.numeric(gsub("[()*]", "", value)))
    ) |>
    dplyr::select(term, model, depvar, kind, value) |>
    tidyr::pivot_wider(names_from = kind, values_from = value) |>
    dplyr::mutate(
      event_time = dplyr::case_when(
        grepl("^event_m[0-9]+$", term) ~ -suppressWarnings(as.integer(sub("^event_m", "", term))),
        grepl("^event_p[0-9]+$", term) ~  suppressWarnings(as.integer(sub("^event_p", "", term))),
        grepl("^event_[0-9]+$",  term) ~  suppressWarnings(as.integer(sub("^event_",  "", term))),
        TRUE ~ NA_integer_
      )
    )
}

#' What dependent variables and models does one of Felix's CSVs contain?
#'
#' Call this before plotting anything. His split files are named after the
#' TABLE, not the column: t3_move_democrat.csv carries pStatesUni, MoveZIP and
#' MoveAny in one file.
pap_stata_inventory <- function(.path) {
  pap_read_stata_csv(.path) |>
    dplyr::filter(!is.na(event_time)) |>
    dplyr::summarise(NEventTimes = dplyr::n(), .by = c(model, depvar)) |>
    dplyr::arrange(model)
}

#' Tidy parser output -> the shape both event-plot functions expect
#'
#' `.depvar` and `.model` are NOT optional in practice. The previous version
#' filtered on `!is.na(event_time)` and nothing else, so a multi-depvar file
#' stacked every dependent variable onto one axis - which is what happened to
#' all three panels of Figure A3 on 2026-08-06. The assertion below makes that
#' failure loud instead of plausible-looking.
pap_event_frame <- function(.path, .depvar = NULL, .model = NULL, .ideology = NULL) {
  if (FALSE) {
    .path     <- pap_felix_find(.P$FelixTables, "t3_move_democrat")
    .depvar   <- "pStatesUni"
    .model    <- NULL
    .ideology <- "Democrat"
  }

  tab_ <- pap_read_stata_csv(.path) |>
    dplyr::filter(!is.na(event_time))

  if (!is.null(.depvar)) tab_ <- dplyr::filter(tab_, depvar == .depvar)
  if (!is.null(.model))  tab_ <- dplyr::filter(tab_, model  == .model)

  if (nrow(tab_) == 0) {
    stop("pap_event_frame(): nothing left after filtering ", fs::path_file(.path),
         " - check .depvar / .model against pap_stata_inventory()", call. = FALSE)
  }

  dup_ <- tab_ |>
    dplyr::count(event_time) |>
    dplyr::filter(n > 1)

  if (nrow(dup_) > 0) {
    stop("pap_event_frame(): ", fs::path_file(.path), " still has ",
         nrow(dup_), " duplicated event times after filtering - the file holds ",
         dplyr::n_distinct(tab_$depvar), " dependent variable(s) and ",
         dplyr::n_distinct(tab_$model), " model(s). Pass .depvar / .model.",
         call. = FALSE)
  }

  out_ <- tab_ |>
    dplyr::transmute(
      ivar     = event_time,
      estimate = estimate,
      High     = estimate + stats::qnorm(0.975) * std_err,
      Low      = estimate - stats::qnorm(0.975) * std_err
    )

  if (!is.null(.ideology)) out_$Ideology <- .ideology
  out_
}

# Figure A4 plot: county belief split. Reproduces the styling of the inline
# block in 003a (darkgreen = High Belief, darkred = Low Belief, pre-period
# faded), which never existed as a reusable function.

pap_plot_belief_split <- function(.tab, .ref = c(-6, 10)) {
  d_ <- .tab |>
    dplyr::arrange(ivar) |>
    dplyr::filter(dplyr::between(ivar, .ref[1], .ref[2])) |>
    dplyr::mutate(
      pre_period  = ivar < -1,
      color_group = paste0(Belief, ifelse(pre_period, "_pre", "_post"))
    )

  ggplot2::ggplot(
    d_,
    ggplot2::aes(x = ivar + ifelse(Belief == "High", 0.15, -0.15),
                 y = estimate, color = color_group)
  ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = .3) +
    ggplot2::geom_vline(xintercept = -1, linetype = "dashed", linewidth = .3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = Low, ymax = High), width = .2) +
    ggplot2::geom_point(size = 1) +
    ggplot2::scale_x_continuous(breaks = seq(.ref[1], .ref[2], 1)) +
    ggplot2::scale_color_manual(
      name   = NULL,
      values = c("High_post" = "darkgreen",
                 "High_pre"  = scales::alpha("darkgreen", 0.5),
                 "Low_post"  = "darkred",
                 "Low_pre"   = scales::alpha("darkred", 0.5)),
      breaks = c("High_post", "Low_post"),
      labels = c("High Belief", "Low Belief")
    ) +
    ggplot2::labs(x = "Years relative to Wildfire", y = "Treatment Effect") +
    ggplot2::theme_bw(base_family = "Times") +
    ggplot2::theme(legend.position = "top",
                   panel.grid.minor = ggplot2::element_blank())
}


# Paper tables -----------------------------------------------------------------

# Table 2 - Summary statistics by Treatment / Control / All Firms.
#
# Two fixes carried since 2026-07:
#   1. The original assigned FireRadius to BOTH "Distance to Fire Midpoint" and
#      "Estimated fire radius"; the computed distance was discarded.
#   2. Obs. reported var_summary()'s `nobs`, which is length(value) - the ROW
#      count, NAs included. So the table claimed 134,717 observations of ESG
#      Score while Table 5 col (9) estimates on 22,084. `nnas` is the honest
#      count and is what Felix's Stata version reports, so the two generators
#      now mean the same thing by the same word. NAs are carried alongside so
#      the row count is still recoverable.
pap_table2_summary <- function(.path_spine, .path_vars_select,
                               .variant = "FULL", .damage_firms = NULL) {
  if (FALSE) {
    .path_spine       <- .P$Spine
    .path_vars_select <- .P$VarsSelect
    .variant          <- "FULL"
    .damage_firms     <- pap_damage_firms(.P$Variables)
  }

  spine_ <- arrow::read_parquet(.path_spine) |> dplyr::ungroup()
  vars_  <- arrow::read_parquet(.path_vars_select) |> dplyr::ungroup()
  keys_  <- intersect(intersect(names(spine_), names(vars_)), c("gvkey", "datadate", "fyear"))
  tab_   <- dplyr::left_join(spine_, vars_, by = keys_) |>
    pap_apply_variant(.variant, .damage_firms)

  # GeoDispersion is NOT recomputed here any more. It used to be nStatesUni/50,
  # a 0-1 fraction, while 003a shipped pStatesUni = nStatesUni/51*100, a
  # percent - so Table 2 printed 0.10 for the same variable Table 7 reported in
  # percentage points, on a denominator that was also wrong. Both now come from
  # the one shipped column. Table A1 defines the variable as a percentage, so
  # Table 2 reports the percent form.
  if (!"GeoDispersion" %in% names(tab_)) {
    stop("pap_table2_summary(): GeoDispersion missing from variables_select - ",
         "re-run 003a (prep_variables_select aliases pStatesUni).", call. = FALSE)
  }
  if ("SplitStateIdeology" %in% names(tab_)) tab_$Democrat <- as.integer(tab_$SplitStateIdeology == "DEMOCRAT")

  map_ <- c(
    "ESG Disclosure"                = "ESGBin",
    # Labelled by what each column IS. bBERT95_NetZero thresholds the model's
    # mean confidence and runs against the construct (cor -0.17 with the hit
    # count, +0.77 with confidence), so calling it "NetZero Commitment" in a
    # paper table would misdescribe it. See the block in prep_variables_select().
    "NetZero Commitment (any paragraph)" = "bNetZeroAny",
    "BERT mean confidence >= .95 (legacy)" = "bBERT95_NetZero",
    "ESG Score"                     = "ESGScore",
    "Direct GHG Emission Intensity" = "IntDirectGHG",
    "Scope1 GHG Emission Intensity" = "IntScope1GHG",
    "Emission Score"                = "EmissionScore",
    "EnvPillar Score"               = "EnvPillarScore",
    "Env Provisions"                = "EnvProvisions",
    "GeoDispersion"                 = "GeoDispersion",
    "Move ZIP"                      = "MoveZIP",
    "Move Any"                      = "MoveAny",
    "Number of Fires"               = "FireNum",
    "Democrat"                      = "Democrat",
    "Treated"                       = "Treat",
    "Post"                          = "Post",
    "TimeToTreat"                   = "TimeToTreat",
    "Fire Mentions"                 = "FireBin",
    "Fire Salience Ratio"           = "RatioToMid",
    "Distance to Fire Midpoint"     = "DistToMid",
    "Estimated fire radius"         = "FireRadius"
  )
  miss_ <- names(map_)[!map_ %in% names(tab_)]
  if (length(miss_) > 0) warning("Table 2: columns not found -> ", paste(miss_, collapse = ", "))
  map_ <- map_[map_ %in% names(tab_)]

  sel_ <- tab_ |>
    dplyr::mutate(Group = dplyr::if_else(startsWith(Group, "2"), "Control", "Treatment")) |>
    dplyr::select(Group, dplyr::all_of(unname(map_)))
  names(sel_) <- c("Group", names(map_))

  dplyr::bind_rows(sel_, dplyr::mutate(sel_, Group = "All Firms")) |>
    tidyr::nest(.by = Group) |>
    dplyr::mutate(Overview = purrr::map(data, var_summary), data = NULL) |>
    tidyr::unnest(Overview) |>
    dplyr::select(Group, Variable = var, Obs = nnas, NAs = nas, Rows = nobs,
                  Mean = mean, StdDev = sd, Min = min, Max = max) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                ~ dplyr::if_else(is.infinite(.x) | is.nan(.x), NA_real_, .x))) |>
    # Scientific notation, TODO #7. A column holding both 0.0196 (ESG Disclosure)
    # and 176,946 (Distance to Fire Midpoint) needs so many significant digits in
    # fixed form that format() switches the WHOLE column to exponent notation.
    # Four decimals is more than any summary statistic in this table needs.
    dplyr::mutate(dplyr::across(c(Mean, StdDev, Min, Max), ~ round(.x, 4))) |>
    dplyr::filter(!(Group %in% c("Control", "All Firms") &
                      Variable %in% c("Fire Salience Ratio", "TimeToTreat",
                                      "Estimated fire radius", "Distance to Fire Midpoint")))
}

# Table 1 Panel B - state-level wildfire occurrence and corporate exposure.
#
# Convention (matches the paper): observations and fires are counted in the
# state of the observation; FIRMS are counted once, in their modal HQ state, so
# the firm columns sum to the sample's unique firm count instead of double
# counting relocations.
#
# The Total row now carries IsTotal = TRUE and sorts last. It used to be an
# ordinary row in the CSV, which doubled every downstream aggregate Felix built
# from it (2,330 fires instead of 1,165).
#
# nFires here is a per-state distinct count, so the column sums to MORE than the
# sample's unique fire count: a fire near firms in two states is counted in
# both. UniqueFires on the Total row is the honest figure and is what the note
# under the printed table should quote.
pap_table1b_state <- function(.path_spine, .variant = "FULL", .damage_firms = NULL) {
  if (FALSE) {
    .path_spine   <- .P$Spine
    .variant      <- "FULL"
    .damage_firms <- pap_damage_firms(.P$Variables)
  }

  spine_ <- arrow::read_parquet(.path_spine) |>
    dplyr::ungroup() |>
    pap_apply_variant(.variant, .damage_firms)

  firm_state_ <- spine_ |>
    dplyr::count(gvkey, StateNameFirm, name = "nRows") |>
    dplyr::group_by(gvkey) |>
    dplyr::slice_max(nRows, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(gvkey, HomeState = StateNameFirm)

  treated_firms_ <- spine_ |>
    dplyr::filter(Treat == 1) |>
    dplyr::distinct(gvkey) |>
    dplyr::pull(gvkey)

  by_firm_ <- firm_state_ |>
    dplyr::group_by(State = HomeState) |>
    dplyr::summarise(
      nFirms      = dplyr::n(),
      nTreatFirms = sum(gvkey %in% treated_firms_),
      .groups = "drop"
    )

  by_obs_ <- spine_ |>
    dplyr::group_by(State = StateNameFirm) |>
    dplyr::summarise(
      nFires         = dplyr::n_distinct(FireID[FireID != 0 & !is.na(FireID)]),
      nObs           = dplyr::n(),
      nTreatObs      = sum(Treat == 1, na.rm = TRUE),
      AwarenessRatio = mean(RatioToMid[Treat == 1 & is.finite(RatioToMid)], na.rm = TRUE),
      .groups = "drop"
    )

  out_ <- dplyr::full_join(by_obs_, by_firm_, by = "State") |>
    dplyr::mutate(
      dplyr::across(c(nFirms, nTreatFirms), ~ tidyr::replace_na(.x, 0L)),
      pFires  = round(nFires / sum(nFires) * 100, 2),
      IsTotal = FALSE,
      UniqueFires = NA_integer_
    ) |>
    dplyr::select(IsTotal, State, nFires, pFires, nObs, nFirms,
                  nTreatObs, nTreatFirms, AwarenessRatio, UniqueFires) |>
    dplyr::arrange(dplyr::desc(nFires))

  total_ <- tibble::tibble(
    IsTotal        = TRUE,
    State          = "Total",
    nFires         = sum(out_$nFires),
    pFires         = 100,
    nObs           = nrow(spine_),
    nFirms         = dplyr::n_distinct(spine_$gvkey),
    nTreatObs      = sum(spine_$Treat == 1, na.rm = TRUE),
    nTreatFirms    = length(treated_firms_),
    AwarenessRatio = mean(spine_$RatioToMid[spine_$Treat == 1 & is.finite(spine_$RatioToMid)], na.rm = TRUE),
    # The figure the table note must quote - NOT the sum of the nFires column.
    UniqueFires    = dplyr::n_distinct(spine_$FireID[spine_$FireID != 0 & !is.na(spine_$FireID)])
  )

  dplyr::bind_rows(out_, total_)
}

#' Table 3 - sample construction, with the damage exclusion made explicit
#'
#' The FULL variant ends at the geocoded sample. The EXCL-DAMAGE variant adds
#' the exclusion as its own line, so the funnel in the paper reconciles to the
#' number the regressions actually use instead of being assembled by hand.
pap_table3_sample <- function(.fun_sample_table, .path_compustat, .path_edgar,
                              .path_compustat_filtered, .path_sample_street,
                              .variant = "FULL", .damage_firms = NULL) {
  base_ <- .fun_sample_table(
    .path_compustat          = .path_compustat,
    .path_edgar              = .path_edgar,
    .path_compustat_filtered = .path_compustat_filtered,
    .path_sample_street      = .path_sample_street
  )

  # adorn_totals() appended a "Final Sample" row that sums the column, and it
  # also stamps a "totals" attribute on the frame. Filtering the row off does
  # NOT remove the attribute, so calling adorn_totals() again aborts with
  # "trying to re-add a totals dimension that is already been added". The total
  # is a two-column sum, so compute it with dplyr and drop janitor from this
  # path entirely; the attribute is stripped defensively in case a caller
  # inspects the result.
  strip_ <- function(.tab) {
    attr(.tab, "totals") <- NULL
    tibble::as_tibble(.tab)
  }

  body_ <- strip_(dplyr::filter(base_, Description != "Final Sample"))
  tot_  <- strip_(dplyr::filter(base_, Description == "Final Sample"))

  if (identical(.variant, "FULL")) {
    return(dplyr::bind_rows(body_, dplyr::mutate(tot_, Description = "Geocoded sample")))
  }

  n_firms_ <- attr(.damage_firms, "n_firms")
  n_fy_    <- attr(.damage_firms, "n_firm_years")
  if (is.null(n_firms_) || is.null(n_fy_)) {
    stop("pap_table3_sample(): .damage_firms has no counts - it must come from ",
         "pap_damage_firms(), which attaches them.", call. = FALSE)
  }

  rows_ <- dplyr::bind_rows(
    body_,
    tibble::tibble(
      Description = "  Less: Firms disclosing wildfire damages in 10-Ks",
      nObs        = -as.integer(n_fy_),
      Firms       = -as.integer(n_firms_)
    )
  )

  dplyr::bind_rows(
    rows_,
    tibble::tibble(
      Description = "Final Sample",
      nObs        = sum(rows_$nObs),
      Firms       = sum(rows_$Firms)
    )
  )
}


# Report coverage --------------------------------------------------------------

#' How much of the sample actually has a filing
#'
#' Nothing in the pipeline persists this. s5_coverage_gate() computes the
#' anti-join and returns it invisibly; StoreCoverage.parquet is aggregated to
#' Year x FileType and cannot say WHICH firm-years are missing.
#'
#' The store is not a superset of the sample and the sample is not a superset of
#' the store: store scope is gvkey-in-Compustat plus the date window, the sample
#' additionally requires a geocoded street address. Both directions are
#' reported.
#'
#' Read-only, and only the `reports` table - `units` holds 518M rows and `text`
#' is never projected.
pap_report_coverage <- function(.path_store, .path_spine, .path_bad_files = NULL,
                                .variant = "FULL", .damage_firms = NULL,
                                .form_types = c("10-K", "10-K/A")) {
  if (FALSE) {
    .path_store     <- .P$Store
    .path_spine     <- .P$Spine
    .path_bad_files <- .P$StoreBadFiles
    .variant        <- "FULL"
    .damage_firms   <- pap_damage_firms(.P$Variables)
    .form_types     <- c("10-K", "10-K/A")
  }

  if (!fs::file_exists(.path_store)) {
    cli::cli_alert_warning("no DuckDB store at {(.path_store)} - coverage skipped")
    return(NULL)
  }

  con_ <- try(
    DBI::dbConnect(duckdb::duckdb(), dbdir = .path_store, read_only = TRUE),
    silent = TRUE
  )
  if (inherits(con_, "try-error")) {
    cli::cli_alert_warning(
      "could not open the store read-only (another session may hold it) - coverage skipped"
    )
    return(NULL)
  }
  on.exit(try(DBI::dbDisconnect(con_, shutdown = TRUE), silent = TRUE), add = TRUE)

  meta_ <- try(dplyr::collect(dplyr::tbl(con_, "store_meta")), silent = TRUE)
  if (!inherits(meta_, "try-error") && nrow(meta_) > 0 &&
      isTRUE(meta_$sample_frac[[1]] < 1)) {
    cli::cli_alert_danger(
      "STORE IS A {scales::percent(meta_$sample_frac[[1]])} FIRM SUBSAMPLE - \\
       coverage numbers are NOT the full build"
    )
  }

  docs_ <- dplyr::tbl(con_, "reports") |>
    dplyr::filter(file_type %in% .form_types) |>
    dplyr::select(gvkey, datadate, file_type) |>
    dplyr::collect()

  keys_store_ <- dplyr::distinct(docs_, gvkey, datadate)

  spine_ <- arrow::read_parquet(
    .path_spine,
    col_select = dplyr::all_of(c("gvkey", "datadate", "Treat"))
  ) |>
    dplyr::ungroup() |>
    pap_apply_variant(.variant, .damage_firms)

  sample_ <- spine_ |>
    dplyr::distinct(gvkey, datadate, Treat) |>
    dplyr::mutate(HasDoc = dplyr::if_else(
      paste(gvkey, datadate) %in% paste(keys_store_$gvkey, keys_store_$datadate),
      1L, 0L
    ))

  by_firm_ <- sample_ |>
    dplyr::summarise(NYears = dplyr::n(), NWithDoc = sum(HasDoc), .by = gvkey)

  # Firm-years the store has that the sample does not want: out of scope for the
  # paper (no geocoded street address, or outside the Compustat annual rows).
  orphan_ <- dplyr::anti_join(keys_store_, sample_, by = c("gvkey", "datadate"))

  pct_ <- function(.n, .d) if (.d == 0) NA_real_ else round(100 * .n / .d, 2)

  bad_rows_ <- NA_integer_
  bad_docs_ <- NA_integer_
  if (!is.null(.path_bad_files) && fs::file_exists(.path_bad_files)) {
    bad_ <- arrow::read_parquet(.path_bad_files)
    bad_rows_ <- nrow(bad_)
    bad_docs_ <- sum(bad_$NDocs, na.rm = TRUE)
  }

  n_sample_    <- nrow(sample_)
  n_with_      <- sum(sample_$HasDoc)
  n_without_   <- n_sample_ - n_with_
  n_firms_     <- nrow(by_firm_)
  n_firms_none <- sum(by_firm_$NWithDoc == 0)
  n_tr_        <- sum(sample_$Treat == 1)
  n_ct_        <- sum(sample_$Treat == 0)
  n_tr_with_   <- sum(sample_$HasDoc[sample_$Treat == 1])
  n_ct_with_   <- sum(sample_$HasDoc[sample_$Treat == 0])

  summary_ <- tibble::tribble(
    ~Section, ~Metric, ~Value, ~Share,
    "Store",  "Documents (10-K + 10-K/A)",        nrow(docs_),        NA_real_,
    "Store",  "  of which 10-K",                  sum(docs_$file_type == "10-K"),   NA_real_,
    "Store",  "  of which 10-K/A",                sum(docs_$file_type == "10-K/A"), NA_real_,
    "Store",  "Unique firm-years",                nrow(keys_store_),  NA_real_,
    "Store",  "Unique firms",                     dplyr::n_distinct(docs_$gvkey), NA_real_,
    "Source", "Unusable source files",            bad_rows_,          NA_real_,
    "Source", "Documents lost to unusable files", bad_docs_,          NA_real_,
    "Sample", "Firm-years",                       n_sample_,          100,
    "Sample", "  with >= 1 filing",               n_with_,            pct_(n_with_, n_sample_),
    "Sample", "  with NO filing",                 n_without_,         pct_(n_without_, n_sample_),
    "Sample", "Firms",                            n_firms_,           100,
    "Sample", "  with NO filing in any year",     n_firms_none,       pct_(n_firms_none, n_firms_),
    "Treated",  "Firm-years",                     n_tr_,              100,
    "Treated",  "  with >= 1 filing",             n_tr_with_,         pct_(n_tr_with_, n_tr_),
    "Control",  "Firm-years",                     n_ct_,              100,
    "Control",  "  with >= 1 filing",             n_ct_with_,         pct_(n_ct_with_, n_ct_),
    "Outside",  "Store firm-years not in sample", nrow(orphan_),      NA_real_
  ) |>
    dplyr::mutate(Value = as.numeric(Value), Variant = .variant, .before = 1)

  by_year_ <- sample_ |>
    dplyr::mutate(Year = as.integer(format(datadate, "%Y"))) |>
    dplyr::summarise(
      NFirmYears = dplyr::n(),
      NWithDoc   = sum(HasDoc),
      .by = Year
    ) |>
    dplyr::mutate(
      NNoDoc  = NFirmYears - NWithDoc,
      pWithDoc = round(100 * NWithDoc / NFirmYears, 2),
      Variant  = .variant
    ) |>
    dplyr::arrange(Year)

  cli::cli_alert_info(
    "{(.variant)} coverage: {scales::comma(n_with_)} of \\
     {cli::qty(n_sample_)}{scales::comma(n_sample_)} sample firm-year{?s} have a filing \\
     ({pct_(n_with_, n_sample_)}%); \\
     {cli::qty(n_firms_none)}{scales::comma(n_firms_none)} firm{?s} have none at all"
  )

  # The N4 consequence, spelled out: s5_assemble() NA-fills every *Bin with 0,
  # so each of these firm-years is currently carrying FireBin = 0 and
  # DamageBin = 0 - indistinguishable in the data from "filed and said nothing".
  if (n_without_ > 0) {
    cli::cli_alert_warning(
      "{cli::qty(n_without_)}{scales::comma(n_without_)} firm-year{?s} carry \\
       FireBin/DamageBin == 0 by NA-fill rather than by observation"
    )
  }

  list(Summary = summary_, ByYear = by_year_)
}


# Output helpers ---------------------------------------------------------------

#' Filename stem for an artifact, carrying its variant
#'
#' Both variants are suffixed and no bare filename is ever written: an
#' unsuffixed file would be ambiguous the moment the two exist side by side.
#' Fire-level artifacts pass .variant = NA and stay unsuffixed - the ABSENCE of
#' a suffix is the signal that the sample does not enter them.
pap_label_variant <- function(.label, .variant = NA_character_) {
  if (is.na(.variant) || identical(.variant, "")) return(.label)
  paste0(.label, "_", .variant)
}

# Graphics device: capabilities("cairo") can report TRUE while the runtime DLL
# still fails to load (macOS without XQuartz). So we TRY cairo, verify a file
# actually appeared, and fall back to the base pdf device - which handles
# family = "Times" natively and embeds cleanly in LaTeX.
pap_save_fig <- function(.plot, .label, .dir_fig, .group,
                         .variant = NA_character_, .width = 16, .height = 10) {
  dir_ <- file.path(.dir_fig, .group)
  fs::dir_create(dir_)

  name_ <- pap_label_variant(.label, .variant)
  path_ <- file.path(dir_, paste0(name_, ".pdf"))
  if (fs::file_exists(path_)) fs::file_delete(path_)

  try_dev_ <- function(.device) {
    try(suppressWarnings(ggplot2::ggsave(
      filename = path_, plot = .plot,
      width = .width, height = .height, units = "cm",
      device = .device
    )), silent = TRUE)
    fs::file_exists(path_) && as.numeric(fs::file_size(path_)) > 0
  }

  dev_used_ <- "cairo_pdf"
  ok_ <- try_dev_(grDevices::cairo_pdf)
  if (!ok_) {
    if (fs::file_exists(path_)) fs::file_delete(path_)
    dev_used_ <- "pdf (cairo unavailable at runtime)"
    ok_ <- try_dev_(grDevices::pdf)
  }
  if (!ok_) stop("Figure was NOT written: ", path_, " - both devices failed")

  tibble::tibble(
    label = .label, variant = .variant, kind = "figure", group = .group,
    file = fs::path_file(path_), path = path_,
    kb = round(as.numeric(fs::file_size(path_)) / 1024, 1),
    device = dev_used_
  )
}

pap_save_table <- function(.tab, .label, .dir_tab, .group, .variant = NA_character_) {
  dir_ <- file.path(.dir_tab, .group)
  fs::dir_create(dir_)

  name_ <- pap_label_variant(.label, .variant)
  csv_  <- file.path(dir_, paste0(name_, ".csv"))
  tex_  <- file.path(dir_, paste0(name_, ".tex"))

  readr::write_csv(.tab, csv_)
  writeLines(knitr::kable(.tab, format = "latex", booktabs = TRUE), tex_)

  tibble::tibble(
    label = .label, variant = .variant, kind = "table", group = .group,
    file = fs::path_file(c(csv_, tex_)), path = c(csv_, tex_),
    kb = round(as.numeric(fs::file_size(c(csv_, tex_))) / 1024, 1),
    device = NA_character_
  )
}


# Variant comparison -----------------------------------------------------------

#' What the damage exclusion actually does, on one vintage of the data
#'
#' Every old-vs-AUG26 comparison confounds two changes: the pipeline rebuild and
#' the exclusion. This holds the data constant and varies only the rule, so the
#' exclusion effect is measurable on its own. The treated share of what is
#' dropped is the number a referee will ask for.
pap_variant_delta <- function(.path_spine, .damage_firms) {
  if (FALSE) {
    .path_spine   <- .P$Spine
    .damage_firms <- pap_damage_firms(.P$Variables)
  }

  cols_ <- c("gvkey", "datadate", "Treat", "FireID", "RatioToMid")
  spine_ <- arrow::read_parquet(.path_spine, col_select = dplyr::all_of(cols_)) |>
    dplyr::ungroup()

  one_ <- function(.tab, .variant) {
    tibble::tibble(
      Variant        = .variant,
      FirmYears      = nrow(.tab),
      Firms          = dplyr::n_distinct(.tab$gvkey),
      TreatedObs     = sum(.tab$Treat == 1, na.rm = TRUE),
      ControlObs     = sum(.tab$Treat == 0, na.rm = TRUE),
      TreatedFirms   = dplyr::n_distinct(.tab$gvkey[.tab$Treat == 1]),
      UniqueFires    = dplyr::n_distinct(.tab$FireID[.tab$FireID != 0 & !is.na(.tab$FireID)]),
      MeanRatio      = round(mean(.tab$RatioToMid[.tab$Treat == 1 & is.finite(.tab$RatioToMid)],
                                  na.rm = TRUE), 4)
    )
  }

  full_ <- one_(spine_, "FULL")
  excl_ <- one_(pap_apply_variant(spine_, "EXCL-DAMAGE", .damage_firms), "EXCL-DAMAGE")

  num_ <- setdiff(names(full_), "Variant")
  drop_ <- tibble::tibble(Variant = "Dropped")
  for (n_ in num_) drop_[[n_]] <- full_[[n_]] - excl_[[n_]]

  out_ <- dplyr::bind_rows(full_, excl_, drop_)

  # Treated over-representation: the share of dropped firm-years that are
  # treated, against the treated share of the full sample.
  share_dropped_ <- drop_$TreatedObs / drop_$FirmYears
  share_full_    <- full_$TreatedObs / full_$FirmYears

  attr(out_, "treated_share_dropped") <- share_dropped_
  attr(out_, "treated_share_full")    <- share_full_
  attr(out_, "over_representation")   <- share_dropped_ / share_full_

  cli::cli_alert_info(
    "exclusion removes {scales::comma(drop_$FirmYears)} firm-years; \\
     treated are {round(100 * share_full_, 1)}% of the sample but \\
     {round(100 * share_dropped_, 1)}% of the drop \\
     ({round(share_dropped_ / share_full_, 2)}x)"
  )

  out_
}


# Staleness gate ---------------------------------------------------------------

#' Are the artifacts this run produced newer than the spine they describe?
#'
#' Ported from exp_gate_paper(), deleted from 020-ExportData.R on 2026-08-06
#' when 020 stopped shipping paper artifacts. It gates on the register - the
#' exact paths THIS run wrote - so a hand-drawn artifact like Figure A2 cannot
#' fail it by construction, and anything on disk but unregistered is reported
#' rather than gated.
pap_gate_stale <- function(.reg, .path_spine, .dirs_local) {
  if (nrow(.reg) == 0) {
    cli::cli_alert_danger("register is empty - nothing was produced")
    return(FALSE)
  }

  t_spine_ <- fs::file_info(.path_spine)$modification_time
  own_     <- unique(.reg$path[fs::file_exists(.reg$path)])

  if (length(own_) == 0) {
    cli::cli_alert_danger("register lists no file that exists on disk")
    return(FALSE)
  }

  t_min_ <- min(fs::file_info(own_)$modification_time)
  ok_    <- t_min_ >= t_spine_

  if (ok_) {
    cli::cli_alert_success(
      "all {length(own_)} registered artifact{?s} postdate the spine \\
       ({format(t_spine_, '%Y-%m-%d %H:%M')})"
    )
  } else {
    cli::cli_alert_danger(
      "oldest registered artifact is {format(t_min_, '%Y-%m-%d %H:%M')}, \\
       spine is {format(t_spine_, '%Y-%m-%d %H:%M')} - re-run before shipping"
    )
  }

  on_disk_ <- unlist(purrr::map(
    .dirs_local, ~ as.character(fs::dir_ls(.x, recurse = TRUE, type = "file"))
  ))
  extra_ <- setdiff(on_disk_, c(own_, .reg$path))

  if (length(extra_) > 0) {
    cli::cli_alert_warning(
      "{length(extra_)} local file{?s} not written by this run: \\
       {paste(fs::path_file(extra_), collapse = ', ')}"
    )
  }

  ok_
}


# Archive & mirror -------------------------------------------------------------

#' Copy whatever is at a destination into _archive/<timestamp>/ before touching it
#'
#' Non-destructive by construction: it only reads the destination. The archive
#' folder is a SIBLING of the group folders, never inside one, so it can never
#' be swept by the operation it exists to protect against.
pap_archive_dest <- function(.dir_dest, .dir_archive, .groups, .execute = FALSE,
                             .stamp = format(Sys.time(), "%Y-%m-%d_%H%M%S")) {
  root_ <- file.path(.dir_archive, .stamp)

  fils_ <- unlist(purrr::map(.groups, function(.g) {
    d_ <- file.path(.dir_dest, .g)
    if (!fs::dir_exists(d_)) return(character(0))
    as.character(fs::dir_ls(d_, recurse = TRUE, type = "file"))
  }))

  if (length(fils_) == 0) {
    cli::cli_alert_info("destination is empty - nothing to archive")
    return(tibble::tibble(path = character(), archived = character(), kb = numeric()))
  }

  rel_ <- fs::path_rel(fils_, .dir_dest)
  dst_ <- file.path(root_, rel_)

  if (isTRUE(.execute)) {
    purrr::walk(unique(fs::path_dir(dst_)), fs::dir_create)
    fs::file_copy(fils_, dst_, overwrite = TRUE)
    cli::cli_alert_success("archived {length(fils_)} file{?s} to {(root_)}")
  } else {
    cli::cli_alert_info("DRY RUN - would archive {length(fils_)} file{?s} to {(root_)}")
  }

  tibble::tibble(
    path     = fils_,
    archived = dst_,
    kb       = round(as.numeric(fs::file_size(fils_)) / 1024, 1)
  )
}

#' Copy the local tree onto the destination, without deleting anything
#'
#' exp_sync() in 020 is delete-then-copy. That is right for a folder only the
#' pipeline writes; it is wrong here, because Figure A2 is hand-drawn and lives
#' at the destination only. So this overwrites and never removes - and REPORTS
#' anything at the destination that this run did not produce, because a stale
#' figure Overleaf still points at is exactly how an old number ships.
#'
#' `.prune` deletes those orphans. Safe only after pap_archive_dest() has run
#' with .execute = TRUE, so it defaults to FALSE and prints the list first.
#'
#' `.keep` is the whitelist of destination-only artifacts. Figure A2 is drawn by
#' hand and lives nowhere else, so without this it appears in the orphan list on
#' every single run - which teaches the reader to skim past the orphan list,
#' which is the one thing it must never become. Matched on file name with or
#' without extension. Whitelisted files are reported under their own heading and
#' are never pruned.
pap_mirror <- function(.dir_local, .dir_dest, .groups, .execute = FALSE,
                       .prune = FALSE, .keep = character()) {
  if (FALSE) {
    .dir_local <- .P$FigLocal
    .dir_dest  <- .P$FigDest
    .groups    <- pap_groups()$figures
    .execute   <- FALSE
    .prune     <- FALSE
    .keep      <- "FigA2_Fire_Salience_Ratio.pdf"
  }

  src_ <- unlist(purrr::map(.groups, function(.g) {
    d_ <- file.path(.dir_local, .g)
    if (!fs::dir_exists(d_)) return(character(0))
    as.character(fs::dir_ls(d_, recurse = TRUE, type = "file"))
  }))

  dst_all_ <- unlist(purrr::map(.groups, function(.g) {
    d_ <- file.path(.dir_dest, .g)
    if (!fs::dir_exists(d_)) return(character(0))
    as.character(fs::dir_ls(d_, recurse = TRUE, type = "file"))
  }))

  rel_src_ <- if (length(src_) == 0) character(0) else as.character(fs::path_rel(src_, .dir_local))
  rel_dst_ <- if (length(dst_all_) == 0) character(0) else as.character(fs::path_rel(dst_all_, .dir_dest))
  orphan_  <- dst_all_[!rel_dst_ %in% rel_src_]

  # Split the whitelist off before anything can act on the orphan list.
  is_kept_ <- function(.f) {
    nm_ <- fs::path_file(.f)
    nm_ %in% .keep | fs::path_ext_remove(nm_) %in% fs::path_ext_remove(.keep)
  }
  kept_   <- if (length(orphan_) == 0) character(0) else orphan_[is_kept_(orphan_)]
  orphan_ <- if (length(orphan_) == 0) character(0) else orphan_[!is_kept_(orphan_)]

  if (length(kept_) > 0) {
    cli::cli_alert_info(
      "{length(kept_)} whitelisted destination-only file{?s} left untouched: \\
       {paste(fs::path_file(kept_), collapse = ', ')}"
    )
  }

  # A whitelist entry that matches nothing is a promise the code is not keeping -
  # most likely the hand-made artifact was renamed or deleted at the destination.
  missing_keep_ <- setdiff(
    fs::path_ext_remove(.keep),
    fs::path_ext_remove(fs::path_file(c(kept_, dst_all_)))
  )
  if (length(missing_keep_) > 0) {
    cli::cli_alert_warning(
      "whitelisted but NOT present at the destination: \\
       {paste(missing_keep_, collapse = ', ')}"
    )
  }

  if (length(src_) > 0 && isTRUE(.execute)) {
    tgt_ <- file.path(.dir_dest, rel_src_)
    purrr::walk(unique(fs::path_dir(tgt_)), fs::dir_create)
    fs::file_copy(src_, tgt_, overwrite = TRUE)
    cli::cli_alert_success("mirrored {length(src_)} file{?s} to {(.dir_dest)}")
  } else {
    cli::cli_alert_info(
      "DRY RUN - would mirror {length(src_)} file{?s} to {(.dir_dest)}"
    )
  }

  if (length(orphan_) > 0) {
    cli::cli_alert_warning(
      "{length(orphan_)} file{?s} at the destination were NOT produced by this run:"
    )
    cli::cli_ul(fs::path_file(orphan_))
    if (isTRUE(.prune) && isTRUE(.execute)) {
      fs::file_delete(orphan_)
      cli::cli_alert_success("pruned {length(orphan_)} orphan{?s}")
    } else {
      cli::cli_alert_info("not pruned - set .prune = TRUE once the archive is verified")
    }
  } else {
    cli::cli_alert_success("no orphans at the destination")
  }

  tibble::tibble(
    file   = c(fs::path_file(src_), fs::path_file(orphan_), fs::path_file(kept_)),
    action = c(rep(if (isTRUE(.execute)) "copied" else "would copy", length(src_)),
               rep(if (isTRUE(.prune) && isTRUE(.execute)) "pruned" else "ORPHAN",
                   length(orphan_)),
               rep("kept (whitelisted)", length(kept_)))
  )
}


# Register ---------------------------------------------------------------------

pap_reg_new <- function() {
  tibble::tibble(label = character(), variant = character(), kind = character(),
                 group = character(), file = character(), path = character(),
                 kb = numeric(), device = character())
}

pap_print_reg <- function(.reg) {
  cli::cli_h3("Produced artifacts ({nrow(.reg)} files)")

  for (i in seq_len(nrow(.reg))) {
    lbl_ <- .reg$label[i]
    var_ <- if (is.na(.reg$variant[i])) "sample-independent" else .reg$variant[i]
    fil_ <- .reg$file[i]
    grp_ <- .reg$group[i]
    kb_  <- .reg$kb[i]
    cli::cli_alert_success("{lbl_} [{var_}] -> {grp_}/{fil_} ({kb_} KB)")
  }

  invisible(.reg)
}

#' One row per label: did it produce the variants it should?
#'
#' THREE valid states, not two. The first version assumed anything outside
#' `.expect_both` was sample-independent, so it flagged every Tier 2 figure as a
#' failure - which is as corrosive as a silent pass, because a red mark that is
#' not a fault teaches the reader to skip the check.
#'
#'   both    FULL + EXCL-DAMAGE  - Tier 1, sample-dependent
#'   single  EXCL-DAMAGE only    - Tier 2, until Felix exports a FULL CSV set
#'   none    no suffix           - fire-level, the sample does not enter it
#'
#' A label that is expected and produced NOTHING is reported as MISSING rather
#' than silently absent: Figure 1B and Figure A4 have no source CSV today, and
#' that should stay visible until they do.
pap_reg_check <- function(.reg, .expect_both, .expect_single = character()) {
  got_ <- .reg |>
    dplyr::filter(kind == "figure" | (kind == "table" & grepl("[.]csv$", file))) |>
    dplyr::summarise(Variants = paste(sort(unique(variant)), collapse = " + "),
                     NFiles = dplyr::n(), .by = label)

  none_ <- "none (sample-independent)"

  out_ <- got_ |>
    dplyr::mutate(
      Expected = dplyr::case_when(
        label %in% .expect_both   ~ "EXCL-DAMAGE + FULL",
        label %in% .expect_single ~ "EXCL-DAMAGE",
        TRUE                      ~ none_
      ),
      # sort() drops NA, so a sample-independent label collapses to ""
      Got    = dplyr::if_else(is.na(Variants) | Variants == "", none_, Variants),
      Status = dplyr::if_else(Got == Expected, "OK", "WRONG VARIANTS")
    )

  absent_ <- setdiff(c(.expect_both, .expect_single), got_$label)
  if (length(absent_) > 0) {
    out_ <- dplyr::bind_rows(out_, tibble::tibble(
      label    = absent_,
      Variants = NA_character_,
      NFiles   = 0L,
      Expected = dplyr::if_else(absent_ %in% .expect_both,
                                "EXCL-DAMAGE + FULL", "EXCL-DAMAGE"),
      Got      = "nothing produced",
      Status   = "MISSING"
    ))
  }

  out_ <- dplyr::arrange(out_, Status == "OK", label)

  n_wrong_ <- sum(out_$Status == "WRONG VARIANTS")
  n_miss_  <- sum(out_$Status == "MISSING")

  if (n_wrong_ > 0) {
    cli::cli_alert_danger("{n_wrong_} label{?s} produced the wrong variants:")
    print(dplyr::filter(out_, Status == "WRONG VARIANTS"))
  }
  if (n_miss_ > 0) {
    cli::cli_alert_warning(
      "{n_miss_} expected label{?s} produced nothing: \\
       {paste(out_$label[out_$Status == 'MISSING'], collapse = ', ')}"
    )
  }
  if (n_wrong_ == 0 && n_miss_ == 0) {
    cli::cli_alert_success("every label produced exactly the variants it should")
  }

  out_
}
