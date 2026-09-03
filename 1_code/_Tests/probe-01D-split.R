# probe-01D-split.R -- does the definition of "attached" explain the word-count reversal?
#
# The published Table 7 called an 8-K "filed" when an Exhibit 10 in the ladder-filtered sample shared
# its HashIndex. 01D calls it attached when any Exhibit 10 in the raw metadata does. This builds both
# splits on the same summaries and reports the word means under each. Writes nothing.
#
# PREDICTION, STATED FIRST. If the split is the cause, the ladder-based split reproduces the paper:
# delayed longer than attached. If the ladder-based split still has attached longer, the cause is
# elsewhere -- the tokeniser, or the span 01D extracts -- and the paper's number is the one to doubt.

here::i_am("1_code/_Commons/_Initialize.R")

item_ <- arrow::open_dataset(here::here("2_output/01D-GetItems/Output/Item101.parquet")) |>
  dplyr::filter(.data$Outcome %in% c("extracted", "ambiguous-longest"), .data$SummaryIsSingle == 1L) |>
  dplyr::select("DocID", "HashIndex", "nWords", "nUncertain", "nNumbers", "HasExhibit10") |>
  dplyr::collect()

reg_ <- arrow::open_dataset(here::here("2_output/02B-Register/Output/Documents.parquet")) |>
  dplyr::filter(.data$Group == "Exhibit10") |>
  dplyr::select("HashIndex", "SampleStepCode") |>
  dplyr::collect()

# The ladder-based split: an Exhibit 10 that survived to the descriptive sample (step 3 on).
desc_ <- reg_ |>
  dplyr::filter(.data$SampleStepCode >= 3L) |>
  dplyr::distinct(.data$HashIndex) |>
  dplyr::pull(.data$HashIndex)

tab_ <- item_ |>
  dplyr::mutate(
    AttachedRaw    = .data$HasExhibit10 == 1L,
    AttachedLadder = .data$HashIndex %in% desc_
  )

cat("\nCrosstab of the two splits\n")
print(table(Raw = tab_$AttachedRaw, Ladder = tab_$AttachedLadder))

for (split_ in c("AttachedRaw", "AttachedLadder")) {
  cat("\n", split_, "\n", sep = "")
  tab_ |>
    dplyr::summarise(
      N         = dplyr::n(),
      Words     = mean(.data$nWords),
      Uncertain = 1000 * sum(.data$nUncertain) / sum(.data$nWords),
      Numbers   = 1000 * sum(.data$nNumbers) / sum(.data$nWords),
      .by = dplyr::all_of(split_)
    ) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(split_))) |>
    print()
}

# The reclassified: attached by the raw split, delayed by the ladder. If these are the long ones,
# that is the whole explanation.
cat("\nAttached raw, delayed by ladder\n")
tab_ |>
  dplyr::filter(.data$AttachedRaw, !.data$AttachedLadder) |>
  dplyr::summarise(N = dplyr::n(), Words = mean(.data$nWords)) |>
  print()
