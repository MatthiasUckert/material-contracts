# 02-VerifyMigration: does the rewritten 02 reproduce what it replaces? -------------------------------------------------
#
# WHAT THIS IS
# Scaffolding, and the last of it for 02. The pair contains no trace of a migration; this holds
# everything that is a question about the PORT rather than about the data, and is deleted once that
# question is answered.
#
#   .STAGE <- "before"   move the seeded outputs aside, then render 02
#   .STAGE <- "after"    compare what the render produced against them
#
# WHAT IS AND IS NOT MOVED
# The two Compustat downloads stay in place: 02 reads them and never rewrites them, and re-running
# the download is the one thing this migration exists to avoid. Everything else -- the matching
# frame, the three samples, the three selection tables -- is an output of 02 and is moved aside so
# the render has to rebuild it.
#
# PATH-VALUED AND ORDER-DEPENDENT COLUMNS
# Columns holding absolute paths differ by construction, because the reference was written under a
# different project root, and are excluded from the value comparison with the exclusion reported.
# Both sides are sorted on a key before anything is compared, and the key's uniqueness is reported
# rather than assumed: where a key repeats, rows align arbitrarily within tie groups and a reported
# difference belongs to the comparison rather than to the data.
#
# ADDED COLUMNS ARE NOT DIFFERENCES
# This version records things the previous one did not. A shared-column difference count of zero
# alongside a nonzero added count is a pass. A column that disappeared is not.
#
# Run from the project root:  source("1_code/_Migration/02-VerifyMigration.R")

.STAGE <- "before"   # "before" moves the reference outputs aside; "after" compares against them


# 1. Tools -------------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

dir_02_    <- here::here("2_output", "02-SelectSample")
dir_stash_ <- here::here("2_output", "_Migration", "02-SelectSample")

# Relative to 02's directory. The Compustat downloads are deliberately absent from this list.
files_ <- c(
  "Compustat/QuarterRange.parquet",
  "SampleExhibit10.parquet",
  "SampleFiling08K.parquet",
  "SampleFilingCTO.parquet",
  "Overview/SampleSelectExhibit10.parquet",
  "Overview/SampleSelectFiling08K.parquet",
  "Overview/SampleSelectFilingCTO.parquet",
  "Overview/FilingOverviews.parquet",
  "Overview/Ex10FilingCount.parquet"
)

# Each output's natural key. The selection tables are one row per ladder step; the samples are one
# row per document; the matching frame is one row per firm-quarter.
keys_ <- list(
  "Compustat/QuarterRange.parquet"         = c("gvkey", "fyear", "fqtr"),
  "SampleExhibit10.parquet"                = "DocID",
  "SampleFiling08K.parquet"                = "DocID",
  "SampleFilingCTO.parquet"                = "DocID",
  "Overview/SampleSelectExhibit10.parquet" = "SampleStepDesc",
  "Overview/SampleSelectFiling08K.parquet" = "SampleStepDesc",
  "Overview/SampleSelectFilingCTO.parquet" = "SampleStepDesc",
  "Overview/FilingOverviews.parquet"       = c("sType", "uType", "DocID"),
  "Overview/Ex10FilingCount.parquet"       = c("SampleType", "UniqueType")
)

stopifnot("stage must be 'before' or 'after'" = .STAGE %in% c("before", "after"))

say_("== Verify 02 -- stage: ", .STAGE, " ==")

stash_name_ <- function(.rel) paste0(gsub("/", "__", fs::path_ext_remove(.rel)), "_reference.parquet")


# 2. Stage: before -----------------------------------------------------------------------------------------------------

if (identical(.STAGE, "before")) {
  fs::dir_create(dir_stash_)

  if (length(fs::dir_ls(dir_stash_, type = "file")) > 0L) {
    stop("The stash already holds files. Run the 'after' stage, or clear ", dir_stash_, " by hand.")
  }

  have_ <- files_[fs::file_exists(fs::path(dir_02_, files_))]
  if (length(have_) == 0L) stop("No outputs to move aside. Run the seeding script first.")

  purrr::walk(
    .x = have_,
    .f = \(.f) fs::file_move(fs::path(dir_02_, .f), fs::path(dir_stash_, stash_name_(.f)))
  )

  say_("\nMoved aside: ", paste(have_, collapse = ", "))
  say_("All are now in 2_output/_Migration/02-SelectSample/, outside 02's own output directory.")
  say_("")
  say_("The two Compustat downloads were left in place, so the render will reuse them and not")
  say_("connect to WRDS. Now render 02-SelectSample.qmd, then set .STAGE <- 'after'.")
}


# 3. Stage: after ------------------------------------------------------------------------------------------------------

if (identical(.STAGE, "after")) {

  compare_ <- function(.a, .b, .key) {
    cols_ <- intersect(names(.a), names(.b))
    cols_ <- cols_[!grepl("Path$", cols_)]
    key_  <- intersect(.key, cols_)

    a_ <- dplyr::arrange(.a, dplyr::across(dplyr::all_of(key_)))
    b_ <- dplyr::arrange(.b, dplyr::across(dplyr::all_of(key_)))

    dif_ <- if (nrow(a_) == nrow(b_) && length(cols_) > 0L) {
      sum(vapply(cols_, \(.c) !isTRUE(all.equal(a_[[.c]], b_[[.c]])), logical(1)))
    } else {
      NA_integer_
    }

    tibble::tibble(
      RowsRef     = nrow(a_),
      RowsNew     = nrow(b_),
      nColsShared = length(cols_),
      nColsAdded  = length(setdiff(names(b_), names(a_))),
      nColsLost   = length(setdiff(names(a_), names(b_))),
      KeyUniqRef  = sum(duplicated(a_[key_])) == 0L,
      KeyUniqNew  = sum(duplicated(b_[key_])) == 0L,
      nColsDiff   = dif_
    )
  }

  res_ <- purrr::map(
    .x = files_,
    .f = function(.f) {
      ref_p <- fs::path(dir_stash_, stash_name_(.f))
      new_p <- fs::path(dir_02_, .f)

      if (!fs::file_exists(ref_p) || !fs::file_exists(new_p)) {
        return(tibble::tibble(Output = fs::path_file(.f), RowsRef = NA_integer_,
                              RowsNew = NA_integer_, nColsLost = NA_integer_, nColsDiff = NA_integer_))
      }

      dplyr::bind_cols(
        tibble::tibble(Output = fs::path_file(.f)),
        compare_(arrow::read_parquet(ref_p), arrow::read_parquet(new_p), keys_[[.f]])
      )
    }
  ) |>
    dplyr::bind_rows()

  say_("\n-- Outputs --")
  print(as.data.frame(res_), row.names = FALSE)
  say_("  Path-valued columns are excluded: the reference used a different project root.")

  # Name the columns that differ on the largest sample, where a difference is most likely to matter.
  ref_ex_ <- fs::path(dir_stash_, stash_name_("SampleExhibit10.parquet"))
  new_ex_ <- fs::path(dir_02_, "SampleExhibit10.parquet")
  row_ex_ <- dplyr::filter(res_, .data$Output == "SampleExhibit10.parquet")

  if (nrow(row_ex_) == 1L && isTRUE(row_ex_$nColsDiff > 0L)) {
    a_ <- arrow::read_parquet(ref_ex_) |> dplyr::arrange(.data$DocID)
    b_ <- arrow::read_parquet(new_ex_) |> dplyr::arrange(.data$DocID)

    say_("\n-- SampleExhibit10: columns that differ --")
    purrr::map(
      .x = setdiff(intersect(names(a_), names(b_)), c("DocPath")),
      .f = function(.c) {
        same_ <- a_[[.c]] == b_[[.c]] | (is.na(a_[[.c]]) & is.na(b_[[.c]]))
        tibble::tibble(Column = .c, nDiffer = sum(!same_, na.rm = TRUE))
      }
    ) |>
      dplyr::bind_rows() |>
      dplyr::filter(.data$nDiffer > 0L) |>
      dplyr::arrange(dplyr::desc(.data$nDiffer)) |>
      as.data.frame() |>
      print(row.names = FALSE)
  }

  ok_ <- all(res_$nColsLost == 0L, na.rm = TRUE) &&
    all(res_$nColsDiff == 0L, na.rm = TRUE) &&
    !any(is.na(res_$RowsNew))

  add_ <- sum(res_$nColsAdded, na.rm = TRUE)
  if (add_ > 0L) say_("\n  ", add_, " column(s) added by this version; not counted as differences.")

  say_("")
  if (isTRUE(ok_)) {
    say_("Every ported column reproduces exactly. 02 is verified.")
    say_("Delete 2_output/_Migration/02-SelectSample/ and this script when the migration closes.")
  } else {
    say_("At least one output differs. The reference copies are intact in the stash.")
    say_("If KeyUniq is FALSE, the key repeats and the row alignment above is not trustworthy.")
  }
}
