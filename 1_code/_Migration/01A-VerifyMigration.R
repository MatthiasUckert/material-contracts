# 01A-VerifyMigration: does the rewritten 01A reproduce what it replaces? -----------------------------------------------
#
# WHAT THIS IS
# Scaffolding, and the last of it for 01A. The pair itself contains no trace of a migration: it
# validates its own outputs against each other, as any run should, and knows nothing about where its
# seeded inputs came from. This script holds everything that is a question about the PORT rather
# than about the data, and it is deleted once that question is answered.
#
# WHY IT HAS TWO STAGES
# The document overwrites the two things worth comparing against. edg_clean_landing() rewrites
# LandingPageAll.parquet on every render, and edg_parse_landing() would rewrite a quarter's cache
# file if that quarter were not already complete. So the reference copies have to be moved aside
# before the render and compared after it.
#
#   .STAGE <- "before"   move the reference copies out of the way, then render 01A
#   .STAGE <- "after"    compare what the render produced against them
#
# Moved, not deleted, and moved OUTSIDE 01A's output directory: 2_output/01A-EdgarIndex/ should hold
# what 01A produced and nothing else.
#
# WHAT EACH STAGE TESTS
#
#   LandingPageAll.parquet    edg_clean_landing(): the type conversion, the empty-string-to-NA rule,
#                             the column renames, the Error == 0 filter. This is the substantive
#                             transformation, and the one where a rewrite can silently change a
#                             published number.
#
#   One quarter of RawExtract edg_parse_landing(): the parallel parse. Without this the path is
#                             never executed, because every quarter is already complete and the
#                             function correctly does nothing. One quarter is enough to exercise
#                             chunking across workers.
#
# THE COMPARISON IS ORDER-INSENSITIVE, AND HAS TO BE
# Neither table has a unique row key: HashIndex identifies a filing and repeats once per registrant,
# roughly six times on average and several hundred times on a large registration statement. A
# multi-file Arrow scan does not guarantee the order in which it returns record batches either, so
# two runs over identical inputs can hold identical rows in a different order.
#
# Comparing such a table row by row after sorting on HashIndex alone therefore reports differences
# that do not exist: within a filing the registrants line up arbitrarily, and every filer-block
# column appears to disagree. Both tables are instead sorted on the filing AND the filer before
# anything is compared, and the uniqueness of that key is reported rather than assumed -- a key that
# turns out to repeat would make even this comparison unsafe, and that has to be visible.
#
# Run from the project root:  source("1_code/_Migration/01A-VerifyMigration.R")

.STAGE   <- "before"   # "before" moves the reference copies aside; "after" compares against them
.QUARTER <- NULL       # year-quarter to re-parse; NULL picks a median-sized one automatically


# 1. Tools -------------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

dir_01a_   <- here::here("2_output", "01A-EdgarIndex")
dir_raw_   <- fs::path(dir_01a_, "Cache", "RawExtract")
dir_stash_ <- here::here("2_output", "_Migration", "01A-EdgarIndex")

path_landing_ <- fs::path(dir_01a_, "LandingPageAll.parquet")

# The filing plus the filer. HashIndex alone is not a key; adding CIK identifies the registrant, and
# FilmNo separates the rare case of one registrant appearing twice on one filing.
.KEY <- c("HashIndex", "CIK", "FilmNo")

stopifnot("stage must be 'before' or 'after'" = .STAGE %in% c("before", "after"))

say_("== Verify 01A -- stage: ", .STAGE, " ==")


# 2. Which quarter to re-parse -----------------------------------------------------------------------------------------
#
# A median-sized quarter rather than the smallest. The smallest is an early-1990s quarter holding a
# handful of filings, which would split into fewer chunks than there are workers and so would not
# exercise the parallel path at all.

pick_quarter_ <- function() {
  fil_ <- fs::dir_ls(dir_raw_, glob = "*.parquet", type = "file")
  fil_ <- fil_[!grepl("_reference", fil_)]
  if (length(fil_) == 0L) return(NA_character_)
  ord_ <- fil_[order(fs::file_size(fil_))]
  yq_  <- fs::path_ext_remove(fs::path_file(ord_[ceiling(length(ord_) / 2L)]))
  sub("^Landing_", "", yq_)
}


# 3. Stage: before -----------------------------------------------------------------------------------------------------

if (identical(.STAGE, "before")) {
  fs::dir_create(dir_stash_)

  if (length(fs::dir_ls(dir_stash_, type = "file")) > 0L) {
    stop("The stash already holds files. Run the 'after' stage, or clear ", dir_stash_, " by hand.")
  }
  if (!fs::file_exists(path_landing_)) {
    stop("No LandingPageAll.parquet to move aside. Run the seeding script first.")
  }

  yq_ <- if (is.null(.QUARTER)) pick_quarter_() else .QUARTER
  if (is.na(yq_)) stop("No per-quarter cache files found. Run the seeding script first.")

  path_qtr_ <- fs::path(dir_raw_, paste0("Landing_", yq_, ".parquet"))
  if (!fs::file_exists(path_qtr_)) stop("Quarter ", yq_, " not found in the cache.")

  fs::file_move(path_landing_, fs::path(dir_stash_, "LandingPageAll_reference.parquet"))
  fs::file_move(path_qtr_,     fs::path(dir_stash_, paste0("Landing_", yq_, "_reference.parquet")))

  writeLines(yq_, fs::path(dir_stash_, "quarter.txt"))

  say_("\nMoved aside:")
  say_("  LandingPageAll.parquet")
  say_("  Cache/RawExtract/Landing_", yq_, ".parquet")
  say_("\nBoth are now in 2_output/_Migration/01A-EdgarIndex/, outside 01A's own output directory.")
  say_("")
  say_("Now render 01A-EdgarIndex.qmd. It will report quarter ", yq_, " as having work to do and")
  say_("parse it for real, and it will rebuild LandingPageAll.parquet from the full cache.")
  say_("")
  say_("Then set .STAGE <- 'after' and source this script again.")
}


# 4. Stage: after ------------------------------------------------------------------------------------------------------

if (identical(.STAGE, "after")) {
  path_ref_land_ <- fs::path(dir_stash_, "LandingPageAll_reference.parquet")
  path_qtr_txt_  <- fs::path(dir_stash_, "quarter.txt")

  if (!fs::file_exists(path_ref_land_)) stop("No reference copy found. Run the 'before' stage first.")
  if (!fs::file_exists(path_landing_))  stop("No rebuilt LandingPageAll.parquet. Render 01A first.")

  yq_           <- readLines(path_qtr_txt_, warn = FALSE)[1L]
  path_ref_qtr_ <- fs::path(dir_stash_, paste0("Landing_", yq_, "_reference.parquet"))
  path_new_qtr_ <- fs::path(dir_raw_, paste0("Landing_", yq_, ".parquet"))

  #' Compare two tables that hold the same rows in an arbitrary order.
  #'
  #' Both are sorted on the key before anything is compared, and the key's uniqueness is reported
  #' rather than assumed: where it repeats, rows within a tie group align arbitrarily and any
  #' resulting difference is an artifact of the comparison, not of the data.
  compare_ <- function(.a, .b, .key) {
    key_ <- intersect(.key, intersect(names(.a), names(.b)))

    a_ <- dplyr::arrange(.a, dplyr::across(dplyr::all_of(key_)))
    b_ <- dplyr::arrange(.b, dplyr::across(dplyr::all_of(key_)))

    cols_ <- intersect(names(a_), names(b_))
    dif_  <- if (nrow(a_) == nrow(b_)) {
      purrr::map(
        .x = cols_,
        .f = function(.c) {
          x_ <- a_[[.c]]
          y_ <- b_[[.c]]
          tibble::tibble(Column = .c, nDiffer = sum(!(x_ == y_ | (is.na(x_) & is.na(y_))), na.rm = TRUE))
        }
      ) |>
        dplyr::bind_rows() |>
        dplyr::filter(.data$nDiffer > 0L) |>
        dplyr::arrange(dplyr::desc(.data$nDiffer))
    } else {
      tibble::tibble(Column = character(0), nDiffer = integer(0))
    }

    list(
      summary = tibble::tibble(
        RowsReference = nrow(a_),
        RowsRebuilt   = nrow(b_),
        SameColumns   = identical(sort(names(a_)), sort(names(b_))),
        SameTypes     = identical(
          vapply(a_[cols_], \(.x) class(.x)[1L], character(1)),
          vapply(b_[cols_], \(.x) class(.x)[1L], character(1))
        ),
        KeyUniqueRef  = sum(duplicated(a_[key_])) == 0L,
        KeyUniqueNew  = sum(duplicated(b_[key_])) == 0L,
        nColsDiffer   = nrow(dif_)
      ),
      differences = dif_
    )
  }

  say_("\n-- edg_clean_landing(): the full landing table --")
  res_land_ <- compare_(
    .a   = arrow::read_parquet(path_ref_land_),
    .b   = arrow::read_parquet(path_landing_),
    .key = .KEY
  )
  print(as.data.frame(res_land_$summary), row.names = FALSE)
  if (nrow(res_land_$differences) > 0L) {
    say_("\n   columns that differ:")
    print(as.data.frame(res_land_$differences), row.names = FALSE)
  }

  say_("\n-- edg_parse_landing(): quarter ", yq_, " re-parsed in parallel --")
  if (!fs::file_exists(path_new_qtr_)) {
    say_("  Not rebuilt. The render did not re-parse this quarter; check the parse log.")
    res_qtr_ <- NULL
  } else {
    res_qtr_ <- compare_(
      .a   = arrow::read_parquet(path_ref_qtr_),
      .b   = arrow::read_parquet(path_new_qtr_),
      .key = .KEY
    )
    print(as.data.frame(res_qtr_$summary), row.names = FALSE)
    if (nrow(res_qtr_$differences) > 0L) {
      say_("\n   columns that differ:")
      print(as.data.frame(res_qtr_$differences), row.names = FALSE)
    }
  }

  clean_ <- function(.res) {
    is.null(.res) || (
      .res$summary$RowsReference == .res$summary$RowsRebuilt &&
        isTRUE(.res$summary$SameColumns) &&
        isTRUE(.res$summary$SameTypes) &&
        .res$summary$nColsDiffer == 0L
    )
  }

  say_("")
  if (clean_(res_land_) && clean_(res_qtr_)) {
    say_("Both outputs reproduce exactly. 01A is verified.")
    say_("Delete 2_output/_Migration/01A-EdgarIndex/ and this script when the migration closes.")
  } else {
    say_("At least one output differs. The reference copies are intact in the stash.")
    say_("If KeyUnique is FALSE, the key repeats and the row alignment above is not trustworthy.")
  }
}
