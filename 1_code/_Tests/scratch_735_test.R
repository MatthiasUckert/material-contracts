# scratch_735_test.R: what folding the unclassified contracts into the malformed rung does ---------------------------------
#
# Reads the current release, applies the rule export_final() will apply, and prints the ladder before
# and after. Nothing is written. Run line by line after the 31 setup chunk (it uses .lP and the
# commons); or set the path by hand.

path_ <- .lP$Input$FilContracts

rel_ <- arrow::open_dataset(path_) |>
  dplyr::select("DocID", "Class", "nWords", "nChars", "CIK", "PrimaryFiler", "SampleStepCode", "SampleStepDesc",
                "DescSample", "EstiSample", "Removed", "RemClass") |>
  dplyr::collect()

# 1. WHO HAS NO CLASS, and is that the same set as "no words"? On every copy, not only the unique ones.
cli::cli_h2("Unclassified rows on the release")
print(table(NoClass = is.na(rel_$Class), ZeroWords = rel_$nWords == 0L, useNA = "ifany"))
print(table(Step = rel_$SampleStepDesc[is.na(rel_$Class)], useNA = "ifany"))
print(table(RemClass = rel_$RemClass[is.na(rel_$Class)], useNA = "ifany"))
print(summary(rel_$nChars[is.na(rel_$Class)]))

# 2. THE RULE. A row with no class inside the window (step 2 or beyond) becomes malformed: it keeps
#    its step-2 label, leaves the descriptive and estimation samples, and says why. Rows at step 1
#    (outside the window) are left where they are: they were never in any sample.
lab2_ <- unique(rel_$SampleStepDesc[rel_$SampleStepCode == 2L])
stopifnot(length(lab2_) == 1L)
new_ <- rel_ |>
  dplyr::mutate(
    Fold           = is.na(.data$Class) & .data$SampleStepCode >= 2L,
    SampleStepCode = dplyr::if_else(.data$Fold, 2L, .data$SampleStepCode),
    SampleStepDesc = dplyr::if_else(.data$Fold, lab2_, .data$SampleStepDesc),
    DescSample     = dplyr::if_else(.data$Fold, 0L, .data$DescSample),
    EstiSample     = dplyr::if_else(.data$Fold, 0L, .data$EstiSample),
    Removed        = dplyr::if_else(.data$Fold, TRUE, as.logical(.data$Removed)),
    RemClass       = dplyr::if_else(.data$Fold & is.na(.data$RemClass), "No readable text", .data$RemClass)
  )
cli::cli_alert_info("{format(sum(new_$Fold), big.mark = ',')} copies folded into the malformed rung; \\
                     {format(sum(new_$Fold & new_$PrimaryFiler == 1L), big.mark = ',')} of them primary copies.")

# 3. THE LADDER, before and after: Table 2 Panel A as 30 counts it.
ladder_ <- function(.d) {
  s0_ <- .d[.d$SampleStepCode >= 2L, , drop = FALSE]
  tibble::tibble(
    Row       = c("Full sample", "Malformatted documents", "Multiple filer", "Unique contracts"),
    Contracts = c(nrow(s0_), -sum(s0_$SampleStepCode == 2L),
                  -sum(s0_$SampleStepCode >= 3L & s0_$PrimaryFiler != 1L),
                  sum(s0_$SampleStepCode >= 3L & s0_$PrimaryFiler == 1L)),
    Firms     = c(dplyr::n_distinct(s0_$CIK),
                  dplyr::n_distinct(s0_$CIK) - dplyr::n_distinct(s0_$CIK[s0_$SampleStepCode >= 3L]),
                  NA_integer_,
                  dplyr::n_distinct(s0_$CIK[s0_$SampleStepCode >= 3L & s0_$PrimaryFiler == 1L]))
  )
}
before_ <- ladder_(rel_)
after_  <- ladder_(new_)
print(dplyr::bind_cols(before_, dplyr::select(after_, ContractsAfter = "Contracts", FirmsAfter = "Firms")))

# 4. WHAT 30 AND 31 WILL SEE: unique contracts, and whether every remaining S2 contract has a class.
s2_after_ <- new_[new_$DescSample == 1L & new_$PrimaryFiler == 1L, , drop = FALSE]
cli::cli_alert_info("Unique contracts after: {format(nrow(s2_after_), big.mark = ',')}; without a class: \\
                     {sum(is.na(s2_after_$Class))} (expected 0).")
