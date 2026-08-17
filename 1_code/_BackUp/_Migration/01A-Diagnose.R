# 01A-Diagnose: why do the two landing tables differ? -------------------------------------------------------------------
#
# WHAT THIS IS
# A one-off answer to one question. The verification reported that the rebuilt landing table differs
# from the reference on eight columns, all of them filer-block fields, and nothing else. Three things
# could produce that, they mean entirely different things, and no single number separates them:
#
#   A. PERMUTATION. The rows are the same but ordered differently within a filing. HashIndex repeats
#      about six times per filing, so a positional comparison keyed on it aligns two tables only if
#      their within-filing order happens to match. This would be a fault in the comparison, and 01A
#      would be verified.
#
#   B. STALENESS. The reference holds a different set of rows because it was written once and never
#      rebuilt after later parsing. The rebuild would then be the more correct of the two.
#
#   C. REGRESSION. The same filer, identified by HashIndex and CIK, carries different values in the
#      two tables. That would be a real change in behaviour and the only outcome requiring a fix.
#
# HOW IT DECIDES
# The affected filings are located first, and everything after that works only on those, so the
# expensive comparisons run over a few thousand rows rather than twenty-one million.
#
#   1. Reproduce the positional comparison to find which HashIndex values are implicated.
#   2. Ask whether (HashIndex, CIK) is unique in each table. If it is, it is a usable key and A and
#      C can be told apart directly.
#   3. Compare the two row sets for those filings, order-insensitively. Equal sets means A.
#   4. Where a filer is present in both, compare its values. Any difference means C.
#   5. Report which year-quarters the affected filings come from. Concentration in a few quarters
#      supports B; an even scatter does not.
#
# Run from the project root:  source("1_code/_Migration/01A-Diagnose.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

rule_ <- function(.text) {
  say_("\n", strrep("-", 100))
  say_("  ", .text)
  say_(strrep("-", 100))
}

dir_01a_   <- here::here("2_output", "01A-EdgarIndex")
dir_stash_ <- here::here("2_output", "_Migration", "01A-EdgarIndex")

path_ref_ <- fs::path(dir_stash_, "LandingPageAll_reference.parquet")
path_new_ <- fs::path(dir_01a_, "LandingPageAll.parquet")

stopifnot(
  "reference copy not found" = fs::file_exists(path_ref_),
  "rebuilt table not found"  = fs::file_exists(path_new_)
)


# 1. Load and locate the affected filings ------------------------------------------------------------------------------

rule_("1. Locating the affected filings")

ref_ <- arrow::read_parquet(path_ref_) |> dplyr::arrange(.data$HashIndex)
new_ <- arrow::read_parquet(path_new_) |> dplyr::arrange(.data$HashIndex)

say_("  reference: ", format(nrow(ref_), big.mark = ","), " rows")
say_("  rebuilt  : ", format(nrow(new_), big.mark = ","), " rows")

# The positional comparison the verification made, reproduced here only to find where it fired.
pos_ <- which(!(ref_$CIK == new_$CIK | (is.na(ref_$CIK) & is.na(new_$CIK))))
hash_ <- unique(c(ref_$HashIndex[pos_], new_$HashIndex[pos_]))

say_("  positions where CIK differs: ", format(length(pos_), big.mark = ","))
say_("  filings implicated         : ", format(length(hash_), big.mark = ","))


# 2. Is (HashIndex, CIK) a usable key? ---------------------------------------------------------------------------------

rule_("2. Key uniqueness")

sub_ref_ <- dplyr::filter(ref_, .data$HashIndex %in% hash_)
sub_new_ <- dplyr::filter(new_, .data$HashIndex %in% hash_)

say_("  rows in affected filings, reference: ", format(nrow(sub_ref_), big.mark = ","))
say_("  rows in affected filings, rebuilt  : ", format(nrow(sub_new_), big.mark = ","))

dup_ref_ <- sum(duplicated(sub_ref_[, c("HashIndex", "CIK")]))
dup_new_ <- sum(duplicated(sub_new_[, c("HashIndex", "CIK")]))

say_("  duplicate (HashIndex, CIK), reference: ", dup_ref_)
say_("  duplicate (HashIndex, CIK), rebuilt  : ", dup_new_)
say_(if (dup_ref_ == 0L && dup_new_ == 0L) {
  "  -> the pair is a key, so filers can be matched rather than aligned by position."
} else {
  "  -> the pair repeats; a filer appears twice on the same filing. Reported below."
})


# 3. Are the row sets the same? ----------------------------------------------------------------------------------------
#
# Order-insensitive. If the two hold the same rows, the difference the verification reported was an
# artifact of aligning a non-unique key by position, and nothing about the data changed.

rule_("3. Row sets, ignoring order")

cols_ <- intersect(names(sub_ref_), names(sub_new_))

sig_ <- function(.tab) {
  .tab |>
    dplyr::select(dplyr::all_of(cols_)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    tidyr::unite("Sig", dplyr::everything(), sep = "\u0001", na.rm = FALSE) |>
    dplyr::pull(.data$Sig)
}

sig_ref_ <- sig_(sub_ref_)
sig_new_ <- sig_(sub_new_)

only_ref_ <- setdiff(sig_ref_, sig_new_)
only_new_ <- setdiff(sig_new_, sig_ref_)

print(data.frame(
  Quantity = c("rows, reference", "rows, rebuilt", "in reference only", "in rebuilt only", "common"),
  N        = c(length(sig_ref_), length(sig_new_), length(only_ref_), length(only_new_),
               length(intersect(sig_ref_, sig_new_))),
  row.names = NULL
), row.names = FALSE)

set_equal_ <- length(only_ref_) == 0L && length(only_new_) == 0L


# 4. Do matched filers carry different values? -------------------------------------------------------------------------

rule_("4. Matched filers, value by value")

if (dup_ref_ == 0L && dup_new_ == 0L) {
  cmp_ <- dplyr::inner_join(
    x  = dplyr::select(sub_ref_, dplyr::all_of(cols_)),
    y  = dplyr::select(sub_new_, dplyr::all_of(cols_)),
    by = dplyr::join_by("HashIndex", "CIK"),
    suffix = c(".ref", ".new")
  )

  say_("  filers present in both: ", format(nrow(cmp_), big.mark = ","))

  chk_ <- setdiff(cols_, c("HashIndex", "CIK"))
  dif_ <- purrr::map(
    .x = chk_,
    .f = function(.c) {
      a_ <- cmp_[[paste0(.c, ".ref")]]
      b_ <- cmp_[[paste0(.c, ".new")]]
      tibble::tibble(Column = .c, nDiffer = sum(!(a_ == b_ | (is.na(a_) & is.na(b_))), na.rm = TRUE))
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::filter(.data$nDiffer > 0L) |>
    dplyr::arrange(dplyr::desc(.data$nDiffer))

  if (nrow(dif_) == 0L) {
    say_("  no matched filer differs on any column.")
  } else {
    print(as.data.frame(dif_), row.names = FALSE)
  }
  value_clean_ <- nrow(dif_) == 0L
} else {
  say_("  skipped: (HashIndex, CIK) is not unique, so filers cannot be matched one to one.")
  value_clean_ <- NA
}


# 5. Where do the affected filings come from? --------------------------------------------------------------------------
#
# A reference written once and never rebuilt would be stale for whichever quarters were parsed after
# it was written, so concentration in a few quarters is evidence for that and a scatter is not.

rule_("5. Affected filings by filing year")

sub_ref_ |>
  dplyr::distinct(.data$HashIndex, .keep_all = TRUE) |>
  dplyr::mutate(Year = format(.data$FilingDate, "%Y")) |>
  dplyr::summarise(nFilings = dplyr::n(), .by = "Year") |>
  dplyr::arrange(dplyr::desc(.data$nFilings)) |>
  utils::head(12L) |>
  as.data.frame() |>
  print(row.names = FALSE)


# 6. Verdict -----------------------------------------------------------------------------------------------------------

rule_("6. Verdict")

if (isTRUE(set_equal_) && isTRUE(value_clean_)) {
  say_("  A. PERMUTATION. Both tables hold exactly the same rows, and every matched filer agrees on")
  say_("     every column. The reported difference was the positional comparison aligning a key that")
  say_("     repeats. 01A reproduces its output; the verification script is what needs correcting.")
} else if (!isTRUE(set_equal_) && isTRUE(value_clean_)) {
  say_("  B. STALENESS. The row sets differ but no matched filer disagrees, so the transformation is")
  say_("     unchanged and one table simply holds rows the other does not. Section 5 shows where they")
  say_("     sit. If they are in the rebuilt table only, the reference was written before those")
  say_("     quarters were parsed and the rebuild is the better of the two.")
} else if (isFALSE(value_clean_)) {
  say_("  C. REGRESSION. Filers present in both carry different values. Section 4 names the columns;")
  say_("     that is a real change in behaviour and needs fixing before 01A is accepted.")
} else {
  say_("  INCONCLUSIVE. See sections 2 to 5.")
}
