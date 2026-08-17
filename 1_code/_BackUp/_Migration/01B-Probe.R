# 01B-Probe: does the selection run, and does it agree with the corpus on disk? -----------------------------------------
#
# WHAT THIS IS
# A cheap standalone check of the three selection arms, run before committing to the twenty minutes
# the full driver takes. Self-contained: it resolves its own paths and depends on nothing left in
# the session.
#
# It answers two questions.
#
#   DOES IT RUN? Each arm collects link rows out of a thirty-two million row dataset. Collecting
#   more columns than are needed, or testing a string in R rather than in Arrow, is enough to take
#   the session down at this scale, and the render is a poor place to discover that.
#
#   DOES IT AGREE? The count each arm selects should match the number of documents of that type on
#   disk, give or take the links the SEC no longer serves. A selection that is larger is fine and
#   expected; one that is SMALLER means documents were retrieved that the present rule would not
#   choose, which is a rule that has drifted from the corpus it produced.
#
# The comparison uses the index written by the previous project, read from the migration stash. That
# is the only thing here that knows a migration is happening, and it is skipped if absent.
#
# Run from the project root:  source("1_code/_Migration/01B-Probe.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)


# 1. Resolve -----------------------------------------------------------------------------------------------------------

source(here::here("1_code", "_Commons", "_Utils.R"), encoding = "UTF-8")
source(here::here("1_code", "01A-EdgarIndex.R"), encoding = "UTF-8")
source(here::here("1_code", "01B-EdgarDocuments.R"), encoding = "UTF-8")

dir_mirror_ <- here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
path_land_  <- here::here("2_output", "01A-EdgarIndex", "LandingPageAll.parquet")
path_ref_   <- here::here("2_output", "_Migration", "01B-EdgarDocuments", "FilePaths_reference.parquet")

stopifnot(
  "01A's mirror is missing"          = fs::dir_exists(dir_mirror_),
  "LandingPageAll.parquet is missing" = fs::file_exists(path_land_)
)

edgar_ <- rGetEDGAR::get_directories(dir_mirror_)
path_links_ <- edgar_$DocLinks$DirMain$Links

say_("== Probe 01B ==")
say_("links : ", path_links_)
say_("files : ", length(fs::dir_ls(path_links_, type = "file")))
say_("rows  : ", fmt_(nrow(arrow::open_dataset(path_links_))))


# 2. The three selection arms ------------------------------------------------------------------------------------------
#
# Timed and sized individually. An arm that is slow but survives is worth knowing about separately
# from one that is fast: this runs on every render.

say_("\n-- Arm 1: Exhibit 10 --")
t1_ <- system.time(
  ex10_ <- edg_links_with_ext(
    .path_links = path_links_,                        # mirrored link tables
    .types      = edg_doc_types(.mod = "Exhibit 10"), # every observed spelling
    .hash_index = NULL                                # NULL keeps all filings
  )
)
say_("  types: ", fmt_(length(edg_doc_types("Exhibit 10"))),
     "   rows: ", fmt_(nrow(ex10_)),
     "   ", round(t1_[["elapsed"]], 1), "s   ", format(object.size(ex10_), units = "auto"))

say_("\n-- Arm 2: CT orders --")
t2_ <- system.time(
  ctos_ <- edg_links_with_ext(
    .path_links = path_links_,                 # mirrored link tables
    .types      = edg_doc_types(.mod = "CTO"), # confidential-treatment orders
    .hash_index = NULL                         # NULL keeps all filings
  )
)
say_("  types: ", fmt_(length(edg_doc_types("CTO"))),
     "   rows: ", fmt_(nrow(ctos_)),
     "   ", round(t2_[["elapsed"]], 1), "s   ", format(object.size(ctos_), units = "auto"))

say_("\n-- Arm 3: 8-K reporting Item 1.01 --")
t3a_ <- system.time(
  hash_ <- edg_hash_with_item(
    .path_landing = path_land_,   # unfiltered landing table from 01A
    .item         = "1.01",       # material definitive agreement
    .fixed        = TRUE          # match literally
  )
)
say_("  filings with the item: ", fmt_(length(hash_)), "   ", round(t3a_[["elapsed"]], 1), "s")

t3b_ <- system.time(
  k08k_ <- edg_links_with_ext(
    .path_links = path_links_,                             # mirrored link tables
    .types      = edg_doc_types(.mod = c("8-K", "8-K/A")), # current reports and amendments
    .hash_index = hash_                                    # only those reporting the item
  )
)
say_("  rows: ", fmt_(nrow(k08k_)), "   ", round(t3b_[["elapsed"]], 1), "s   ",
     format(object.size(k08k_), units = "auto"))

sel_ <- dplyr::bind_rows(
  dplyr::mutate(ex10_, Group = "Exhibit10"),
  dplyr::mutate(ctos_, Group = "CTO"),
  dplyr::mutate(k08k_, Group = "8-K")
)

say_("\n-- Selection total --")
say_("  ", fmt_(nrow(sel_)), " documents   ", format(object.size(sel_), units = "auto"),
     "   ", round(sum(t1_[["elapsed"]], t2_[["elapsed"]], t3a_[["elapsed"]], t3b_[["elapsed"]]), 1), "s")


# 3. Against the corpus on disk ----------------------------------------------------------------------------------------
#
# The index is read rather than the tree walked: the walk takes minutes and the index says the same
# thing. DocType on disk carries an A suffix for amendments, which is folded in so the two sides
# describe the same groups.

if (fs::file_exists(path_ref_)) {
  say_("\n-- Selection against the corpus --")

  disk_ <- arrow::read_parquet(path_ref_) |>
    dplyr::mutate(
      Group = dplyr::case_when(
        grepl("^Exhibit10", .data$DocType) ~ "Exhibit10",
        grepl("^CTO", .data$DocType)       ~ "CTO",
        .default                           = "8-K"
      )
    ) |>
    dplyr::summarise(OnDisk = dplyr::n(), .by = "Group")

  sel_ |>
    dplyr::summarise(Selected = dplyr::n(), .by = "Group") |>
    dplyr::full_join(disk_, by = dplyr::join_by("Group")) |>
    dplyr::mutate(
      Outstanding = .data$Selected - .data$OnDisk,
      Note        = dplyr::if_else(
        .data$Outstanding < 0L,
        "ON DISK BUT NOT SELECTED -- the rule has drifted",
        "selected but not retrieved -- dead links"
      )
    ) |>
    as.data.frame() |>
    print(row.names = FALSE)
} else {
  say_("\n  No reference index in the stash; the corpus comparison was skipped.")
}


# 4. Verdict -----------------------------------------------------------------------------------------------------------

say_("\n-- Verdict --")
say_("  All three arms completed. If Outstanding is positive and small on every group, the")
say_("  selection agrees with the corpus and 01B-Execute.R can run.")
say_("  A negative Outstanding means documents were retrieved that this rule does not select,")
say_("  which is a finding about the rule rather than about the copy.")
