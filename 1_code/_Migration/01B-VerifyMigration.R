# 01B-VerifyMigration: does the rewritten 01B reproduce what it replaces? -----------------------------------------------
#
# WHAT THIS IS
# Scaffolding, and the last of it for 01B. The pair contains no trace of a migration; this script
# holds everything that is a question about the PORT rather than about the data, and is deleted once
# that question is answered.
#
#   .STAGE <- "before"   move the reference index aside and force a genuine rebuild, then render 01B
#   .STAGE <- "after"    compare what the render produced against it
#
# THE REBUILD HAS TO BE FORCED
# 01B keys its index rebuild on the acquisition switch, because walking the parsed tree takes minutes
# and the result cannot change unless a document was added. With acquisition off and the index
# already seeded, the render would read the seeded file and prove nothing. Removing it makes
# utils_list_project_files() walk the tree for real, which is the code path under test.
#
# WHAT IS NOT TESTED
# The download. That is one request per document against the SEC, and no version of this check is
# worth that cost. What is tested is everything derived locally: the tree walk, the identifier taken
# from each filename, and the DocType and year-quarter read off the directory structure.
#
# PATHS ARE COMPARED AS FILENAMES
# The reference index was written under a different project root, so its absolute paths cannot match
# and comparing them would report a difference that is not one. What has to match is which documents
# are indexed and how they are classified.
#
# COLUMN NAMES ARE NORMALISED FIRST
# The shared utility that builds this index returns CamelCase column names; the version that wrote
# the reference returned lowercase ones. That rename is deliberate and is reported rather than
# treated as a difference, but it has to be undone before the two can be compared at all. Both sides
# are mapped onto one set of names, and the original names of each are printed so the rename stays
# visible instead of being quietly absorbed.
#
# THE COMPARISON IS ORDER-INSENSITIVE
# Both sides are sorted on the key before anything is compared, and the key's uniqueness is reported
# rather than assumed. A key that turns out to repeat makes row alignment arbitrary within its tie
# groups, and any difference reported then belongs to the comparison rather than to the data -- so
# that has to be visible rather than inferred.
#
# Run from the project root:  source("1_code/_Migration/01B-VerifyMigration.R")

.STAGE <- "before"   # "before" moves the reference copy aside; "after" compares against it


# 1. Tools -------------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

dir_01b_   <- here::here("2_output", "01B-EdgarDocuments")
dir_stash_ <- here::here("2_output", "_Migration", "01B-EdgarDocuments")

path_index_ <- fs::path(dir_01b_, "FilePaths.parquet")
path_ref_   <- fs::path(dir_stash_, "FilePaths_reference.parquet")

.KEY <- "DocID"   # one parsed file per document, so this should be a key; reported, not assumed

stopifnot("stage must be 'before' or 'after'" = .STAGE %in% c("before", "after"))

say_("== Verify 01B -- stage: ", .STAGE, " ==")


# 2. Stage: before -----------------------------------------------------------------------------------------------------

if (identical(.STAGE, "before")) {
  fs::dir_create(dir_stash_)

  if (fs::file_exists(path_ref_)) {
    stop("A reference copy already exists. Run the 'after' stage, or clear ", dir_stash_, " by hand.")
  }
  if (!fs::file_exists(path_index_)) {
    stop("No FilePaths.parquet to move aside. Run the seeding script first.")
  }

  fs::file_move(path_index_, path_ref_)

  say_("\nMoved aside: FilePaths.parquet")
  say_("It is now in 2_output/_Migration/01B-EdgarDocuments/, outside 01B's own output directory.")
  say_("")
  say_("Now render 01B-EdgarDocuments.qmd. With no index present, the parsed tree is walked for real.")
  say_("")
  say_("Then set .STAGE <- 'after' and source this script again.")
}


# 3. Stage: after ------------------------------------------------------------------------------------------------------

if (identical(.STAGE, "after")) {
  if (!fs::file_exists(path_ref_))   stop("No reference copy found. Run the 'before' stage first.")
  if (!fs::file_exists(path_index_)) stop("No rebuilt FilePaths.parquet. Render 01B first.")

  # Column names onto one set, then Path to File: the absolute prefix differs by construction and
  # carries no information, whereas the filename is the identifier the tree walk derived.
  canon_ <- c(docid = "DocID", doctype = "DocType", yq = "YQ", path = "Path")

  prep_ <- function(.path) {
    tab_ <- arrow::read_parquet(.path)

    nm_  <- names(tab_)
    hit_ <- match(tolower(nm_), names(canon_))
    nm_[!is.na(hit_)] <- unname(canon_[hit_[!is.na(hit_)]])
    names(tab_) <- nm_

    if (!"Path" %in% names(tab_)) {
      cli::cli_abort("No path column in {.path}; found {paste(names(tab_), collapse = ', ')}.")
    }

    dplyr::mutate(tab_, File = fs::path_file(.data$Path), Path = NULL)
  }

  compare_ <- function(.a, .b, .key) {
    a_ <- dplyr::arrange(.a, dplyr::across(dplyr::all_of(.key)))
    b_ <- dplyr::arrange(.b, dplyr::across(dplyr::all_of(.key)))

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
        KeyUniqueRef  = sum(duplicated(a_[.key])) == 0L,
        KeyUniqueNew  = sum(duplicated(b_[.key])) == 0L,
        nColsDiffer   = nrow(dif_)
      ),
      differences = dif_,
      ref         = a_,
      new         = b_
    )
  }

  say_("\n-- Column names as written --")
  say_("  reference: ", paste(names(arrow::open_dataset(path_ref_)), collapse = ", "))
  say_("  rebuilt  : ", paste(names(arrow::open_dataset(path_index_)), collapse = ", "))

  say_("\n-- edg_index_documents(): the DocID to Path index --")
  res_ <- compare_(.a = prep_(path_ref_), .b = prep_(path_index_), .key = .KEY)
  print(as.data.frame(res_$summary), row.names = FALSE)

  if (nrow(res_$differences) > 0L) {
    say_("\n   columns that differ:")
    print(as.data.frame(res_$differences), row.names = FALSE)
  }

  # Where the sets differ, name the documents rather than the count. A list of DocIDs present in one
  # and not the other points at a specific subtree; "RowsRebuilt is lower" does not.
  only_ref_ <- setdiff(res_$ref$DocID, res_$new$DocID)
  only_new_ <- setdiff(res_$new$DocID, res_$ref$DocID)

  if (length(only_ref_) > 0L || length(only_new_) > 0L) {
    say_("\n-- Set difference --")
    say_("  in reference only: ", length(only_ref_))
    say_("  in rebuild only:   ", length(only_new_))
    if (length(only_ref_) > 0L) say_("  e.g. ", paste(utils::head(only_ref_, 5L), collapse = ", "))
    if (length(only_new_) > 0L) say_("  e.g. ", paste(utils::head(only_new_, 5L), collapse = ", "))
  }

  ok_ <- res_$summary$RowsReference == res_$summary$RowsRebuilt &&
    isTRUE(res_$summary$SameColumns) &&
    isTRUE(res_$summary$SameTypes) &&
    res_$summary$nColsDiffer == 0L

  say_("")
  if (isTRUE(ok_)) {
    say_("The index reproduces exactly. 01B is verified.")
    say_("Delete 2_output/_Migration/01B-EdgarDocuments/ and this script when the migration closes.")
  } else {
    say_("The index differs. The reference copy is intact in the stash.")
    say_("If KeyUnique is FALSE, the key repeats and the row alignment above is not trustworthy.")
  }
}
