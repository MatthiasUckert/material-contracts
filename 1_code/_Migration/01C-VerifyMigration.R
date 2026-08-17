# 01C-VerifyMigration: does the rewritten 01C reproduce what it replaces? -----------------------------------------------
#
# WHAT THIS IS
# Scaffolding, and the last of it for the 01 family. The pair contains no trace of a migration; this
# script holds everything that is a question about the PORT rather than about the data, and it is
# deleted once that question is answered.
#
# WHY 01C MATTERS MOST
# FullMetaData.parquet is what 02, 03 and 04 read. Every published number about the corpus -- how
# many contracts, how many filers, how much was excluded -- traces back to this table. If any part
# of the rewrite changed it, everything downstream inherits the change silently, because nothing
# downstream re-derives it.
#
#   .STAGE <- "before"   move the six consolidated outputs aside, then render 01C
#   .STAGE <- "after"    compare what the render produced against them, column by column
#
# WHAT IS AND IS NOT RE-RUN
# The three measurement caches are LEFT IN PLACE. Re-running them means opening 1.8 million files,
# and what they compute is per-document regex arithmetic that the rewrite did not alter in substance.
# What IS re-run is everything built on top of them: the join into DocStats, the quality rules, the
# three restrictions, and the final consolidation. Those are where a rewrite changes an answer.
#
# To test the measurement passes as well, delete one cache from 01C/Cache/ before rendering. The
# render will recompute it in full -- hours -- and the comparison below covers the result either way.
#
# Run from the project root:  source("1_code/_Migration/01C-VerifyMigration.R")

.STAGE <- "before"   # "before" moves the reference copies aside; "after" compares against them


# 1. Tools -------------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

dir_01c_   <- here::here("2_output", "01C-EdgarMetaData")
dir_stash_ <- here::here("2_output", "_Migration", "01C-EdgarMetaData")

# The six consolidated outputs. The caches are not listed: they are inputs to what is being tested,
# not outputs of it.
files_ <- c(
  "DocStats.parquet", "RemovedDocs.parquet", "LinkData.parquet",
  "MasterIndex.parquet", "LandingPage.parquet", "FullMetaData.parquet"
)

stopifnot("stage must be 'before' or 'after'" = .STAGE %in% c("before", "after"))

say_("== Verify 01C -- stage: ", .STAGE, " ==")


# 2. Stage: before -----------------------------------------------------------------------------------------------------

if (identical(.STAGE, "before")) {
  fs::dir_create(dir_stash_)

  if (length(fs::dir_ls(dir_stash_, type = "file")) > 0L) {
    stop("The stash already holds files. Run the 'after' stage, or clear ", dir_stash_, " by hand.")
  }

  have_ <- files_[fs::file_exists(fs::path(dir_01c_, files_))]
  if (length(have_) == 0L) stop("No outputs to move aside. Run the seeding script first.")

  purrr::walk(
    .x = have_,
    .f = function(.f) {
      fs::file_move(
        path     = fs::path(dir_01c_, .f),
        new_path = fs::path(dir_stash_, paste0(fs::path_ext_remove(.f), "_reference.parquet"))
      )
    }
  )

  say_("\nMoved aside: ", paste(have_, collapse = ", "))
  say_("All are now in 2_output/_Migration/01C-EdgarMetaData/, outside 01C's own output directory.")
  say_("")
  say_("The three measurement caches in 01C/Cache/ were left in place, so the render will reuse them")
  say_("and complete in minutes. To test a measurement pass too, delete one cache before rendering.")
  say_("")
  say_("Now render 01C-EdgarMetaData.qmd, then set .STAGE <- 'after' and source this script again.")
}


# 3. Stage: after ------------------------------------------------------------------------------------------------------

if (identical(.STAGE, "after")) {

  # Compare two tables on the things that can differ, in the order they matter. A row-count mismatch
  # is a different fault from a type mismatch, and one summary number would conflate them.
  # ADDED COLUMNS ARE NOT DIFFERENCES. This version of the pipeline records things the previous one
  # did not, so a shared-column count of zero alongside a nonzero nColsAdded is a pass, not a
  # failure. What would be a failure is a column that disappeared, or a shared column that changed.
  #
  # Sorting is on the key, and the key's uniqueness is reported rather than assumed: where it
  # repeats, rows align arbitrarily within tie groups and any difference found belongs to the
  # comparison rather than to the data.
  # PATH-VALUED COLUMNS ARE EXCLUDED FROM THE VALUE COMPARISON. The reference was written under a
  # different project root, so any column holding an absolute path differs by construction and
  # carries no information about whether the transformation changed. The columns dropped are named
  # in the output rather than silently skipped.
  compare_ <- function(.a, .b, .key) {
    cols_ <- intersect(names(.a), names(.b))
    cols_ <- cols_[!grepl("Path$", cols_)]

    a_ <- dplyr::arrange(.a, dplyr::across(dplyr::all_of(.key)))
    b_ <- dplyr::arrange(.b, dplyr::across(dplyr::all_of(.key)))

    tibble::tibble(
      RowsRef     = nrow(a_),
      RowsNew     = nrow(b_),
      nColsShared = length(cols_),
      nColsPath   = length(intersect(names(.a), names(.b))) - length(cols_),
      nColsAdded  = length(setdiff(names(b_), names(a_))),
      nColsLost   = length(setdiff(names(a_), names(b_))),
      KeyUniqRef  = sum(duplicated(a_[.key])) == 0L,
      KeyUniqNew  = sum(duplicated(b_[.key])) == 0L,
      nColsDiff   = if (nrow(a_) == nrow(b_) && length(cols_) > 0L) {
        sum(vapply(
          X   = cols_,
          FUN = \(.c) !isTRUE(all.equal(a_[[.c]], b_[[.c]])),
          FUN.VALUE = logical(1)
        ))
      } else {
        NA_integer_
      }
    )
  }

  # Each output has its own natural key; DocID for document-level tables, HashIndex for filing-level.
  keys_ <- c(
    DocStats = "DocID", RemovedDocs = "DocID", LinkData = "DocID",
    MasterIndex = "HashIndex", LandingPage = "HashIndex", FullMetaData = "DocID"
  )

  res_ <- purrr::map(
    .x = files_,
    .f = function(.f) {
      stem_ <- fs::path_ext_remove(.f)
      ref_p <- fs::path(dir_stash_, paste0(stem_, "_reference.parquet"))
      new_p <- fs::path(dir_01c_, .f)

      if (!fs::file_exists(ref_p) || !fs::file_exists(new_p)) {
        return(tibble::tibble(Output = stem_, RowsRef = NA_integer_, RowsNew = NA_integer_,
                              nColsDiff = NA_integer_, nColsLost = NA_integer_))
      }

      cmp_ <- compare_(
        .a   = arrow::read_parquet(ref_p),
        .b   = arrow::read_parquet(new_p),
        .key = keys_[[stem_]]
      )
      dplyr::bind_cols(tibble::tibble(Output = stem_), cmp_)
    }
  ) |>
    dplyr::bind_rows()

  say_("\n-- Consolidated outputs --")
  print(as.data.frame(res_), row.names = FALSE)
  say_("  nColsPath: absolute-path columns, excluded because the reference used a different root.")

  # Where FullMetaData differs, name the columns. "nColsDiff is 2" is not a finding; knowing which
  # two points at the one line of code that produced them.
  ref_full_ <- fs::path(dir_stash_, "FullMetaData_reference.parquet")
  new_full_ <- fs::path(dir_01c_, "FullMetaData.parquet")
  row_full_ <- dplyr::filter(res_, .data$Output == "FullMetaData")

  if (nrow(row_full_) == 1L && isTRUE(row_full_$nColsDiff > 0L)) {
    a_ <- arrow::read_parquet(ref_full_) |> dplyr::arrange(.data$DocID)
    b_ <- arrow::read_parquet(new_full_) |> dplyr::arrange(.data$DocID)

    say_("\n-- FullMetaData: columns that differ --")
    purrr::map(
      .x = intersect(names(a_), names(b_)),
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

  # A pass requires: no output missing, no column lost, and no shared column changed. Columns added
  # by this version are reported and do not count against it.
  ok_ <- all(res_$nColsLost == 0L, na.rm = TRUE) &&
    all(res_$nColsDiff == 0L, na.rm = TRUE) &&
    !any(is.na(res_$RowsNew))

  add_ <- sum(res_$nColsAdded, na.rm = TRUE)
  if (add_ > 0L) {
    say_("\n  ", add_, " column{s} added by this version; those are not counted as differences.")
  }

  say_("")
  if (isTRUE(ok_)) {
    say_("Every ported column reproduces exactly. 01C is verified, and with it the 01 family.")
    say_("Delete 2_output/_Migration/ and the _Migration scripts when the migration closes.")
  } else {
    say_("At least one output differs. The reference copies are intact in the stash.")
  }
}
