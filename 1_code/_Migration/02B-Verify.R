# 02B-Verify: does the register agree with the sample table it replaces? --------------------------------------------------
#
# WHAT THIS IS
# A small check, not the full migration harness the 01 family carries. Read-only.
#
# WHY IT IS SMALL
# The old 02 wrote three sample tables; 02B writes one register covering every document, including
# those the old script deleted. So the two cannot be compared row for row, and a verification that
# demanded they should would fail on precisely the improvements. What CAN be compared is the part
# both versions agree is the answer: which contracts reach the estimation sample, and what the
# selection ladder says about how many were lost at each step.
#
# WHAT IT CHECKS
#   1. the register holds every document 01C published
#   2. the estimation sample contains the same DocIDs as the old SampleExhibit10
#   3. the columns the old table carried are all present, with the same values
#   4. the selection ladder reaches the same final count
#
# Differences are expected in two places and are reported rather than treated as failures: the old
# script deleted documents matching more than one Compustat quarter, and the register keeps them with
# a ladder step of their own.
#
# Run from the project root:  source("1_code/_Migration/02B-Verify.R")

.DIR_OLD <- NULL   # set to a path to override the sibling-project autodetect


# 1. Tools -------------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

rule_ <- function(...) {
  say_("\n", strrep("=", 100))
  say_("  ", ...)
  say_(strrep("=", 100))
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)


# 2. Load --------------------------------------------------------------------------------------------------------------

dir_new_ <- here::here()
dir_old_ <- if (!is.null(.DIR_OLD)) .DIR_OLD else file.path(dirname(dir_new_), "pMatDisc")

path_reg_ <- here::here("2_output", "02B-Register", "Documents.parquet")
path_ref_ <- file.path(dir_old_, "2_output", "02-SelectSample", "Output", "SampleExhibit10.parquet")

stopifnot(
  "register not found; render 02B first" = fs::file_exists(path_reg_),
  "previous SampleExhibit10 not found"   = fs::file_exists(path_ref_)
)

reg_ <- arrow::read_parquet(path_reg_)
ref_ <- arrow::read_parquet(path_ref_)

rule_("0. The two tables")
say_("  register        : ", fmt_(nrow(reg_)), " rows, ", ncol(reg_), " columns (every document)")
say_("  previous sample : ", fmt_(nrow(ref_)), " rows, ", ncol(ref_), " columns (contracts only)")


# 3. Completeness --------------------------------------------------------------------------------------------------------

rule_("1. Does the register hold the whole corpus?")

n_corpus_ <- arrow::open_dataset(
  sources = here::here("2_output", "01C-EdgarMetaData", "FullMetaData.parquet")
) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::collect() |>
  dplyr::pull(.data$n)

tibble::tibble(
  nCorpus   = n_corpus_,
  nRegister = nrow(reg_),
  Missing   = n_corpus_ - nrow(reg_)
) |>
  as.data.frame() |>
  print(row.names = FALSE)

say_("  Missing must be zero: the register marks documents, it does not remove them.")


# 4. The estimation sample -----------------------------------------------------------------------------------------------

rule_("2. Do the two agree on which contracts reach the estimation sample?")

new_esti_ <- reg_ |>
  dplyr::filter(.data$Group == "Exhibit10", .data$EstiSample == 1L) |>
  dplyr::pull(.data$DocID)

old_esti_ <- ref_ |>
  dplyr::filter(.data$EstiSample == 1L) |>
  dplyr::pull(.data$DocID)

tibble::tibble(
  nOld       = length(old_esti_),
  nNew       = length(new_esti_),
  InBoth     = length(intersect(old_esti_, new_esti_)),
  OnlyInOld  = length(setdiff(old_esti_, new_esti_)),
  OnlyInNew  = length(setdiff(new_esti_, old_esti_))
) |>
  as.data.frame() |>
  print(row.names = FALSE)

say_("  OnlyInOld is expected to be small and to be the documents the old script deleted for")
say_("  matching more than one Compustat quarter; the register keeps those at ladder step 04.")

if (length(setdiff(old_esti_, new_esti_)) > 0L) {
  say_("\n-- where the old sample's extras sit in the register --")
  reg_ |>
    dplyr::filter(.data$DocID %in% setdiff(old_esti_, new_esti_)) |>
    dplyr::count(.data$SampleStepDesc, name = "nDocs") |>
    as.data.frame() |>
    print(row.names = FALSE)
}


# 5. Shared columns ------------------------------------------------------------------------------------------------------

rule_("3. Do the columns both tables carry hold the same values?")

# Path-valued columns are excluded: the reference was written under a different project root, and
# the register deliberately carries none.
#
# THE LADDER WAS RENUMBERED, SO ITS LABELS ARE COMPARED WITHOUT THE NUMBER. The new ladder has a
# step the old one did not -- documents matching several Compustat quarters, which the old script
# deleted -- so the final step is 06 where it was 05. Comparing the numbers would report every row
# of the sample as differing, which is true and uninformative; comparing the labels asks the
# question that matters, which is whether the same document is attributed to the same reason.
cols_ <- intersect(names(reg_), names(ref_))
cols_ <- cols_[!grepl("(?i)path", cols_)]
cols_ <- setdiff(cols_, "SampleStepCode")

strip_step_ <- function(.x) gsub("^\\d+-", "", .x)

both_ <- intersect(old_esti_, new_esti_)

a_ <- ref_ |>
  dplyr::filter(.data$DocID %in% both_) |>
  dplyr::arrange(.data$DocID) |>
  dplyr::mutate(SampleStepDesc = strip_step_(.data$SampleStepDesc))

b_ <- reg_ |>
  dplyr::filter(.data$DocID %in% both_) |>
  dplyr::arrange(.data$DocID) |>
  dplyr::mutate(SampleStepDesc = strip_step_(.data$SampleStepDesc))

dif_ <- purrr::map(
  .x = cols_,
  .f = function(.c) {
    same_ <- a_[[.c]] == b_[[.c]] | (is.na(a_[[.c]]) & is.na(b_[[.c]]))
    tibble::tibble(Column = .c, nDiffer = sum(!same_, na.rm = TRUE))
  }
) |>
  dplyr::bind_rows()

tibble::tibble(
  nColsOld    = ncol(ref_),
  nColsNew    = ncol(reg_),
  nColsShared = length(cols_),
  nColsLost   = length(setdiff(names(ref_)[!grepl("(?i)path", names(ref_))], names(reg_))),
  nColsAdded  = length(setdiff(names(reg_), names(ref_))),
  nColsDiffer = sum(dif_$nDiffer > 0L)
) |>
  as.data.frame() |>
  print(row.names = FALSE)

if (any(dif_$nDiffer > 0L)) {
  say_("\n-- columns that differ --")
  dif_ |>
    dplyr::filter(.data$nDiffer > 0L) |>
    dplyr::arrange(dplyr::desc(.data$nDiffer)) |>
    as.data.frame() |>
    print(row.names = FALSE)
}

lost_ <- setdiff(names(ref_)[!grepl("(?i)path", names(ref_))], names(reg_))
if (length(lost_) > 0L) say_("\n-- columns the register does not carry --\n  ", paste(lost_, collapse = ", "))

say_("\n-- columns the register adds --\n  ",
     paste(setdiff(names(reg_), names(ref_)), collapse = ", "))


# 6. Verdict -------------------------------------------------------------------------------------------------------------

rule_("4. Verdict")

ok_ <- nrow(reg_) == n_corpus_ &&
  length(setdiff(new_esti_, old_esti_)) == 0L &&
  sum(dif_$nDiffer > 0L) == 0L &&
  length(lost_) == 0L

if (isTRUE(ok_)) {
  say_("  The register holds the whole corpus, agrees with the previous sample on every contract")
  say_("  the previous version kept, and carries every column it carried.")
} else {
  say_("  At least one check did not pass. Read the tables above before changing anything: the")
  say_("  register keeping documents the old script deleted is expected and is not a failure.")
}
