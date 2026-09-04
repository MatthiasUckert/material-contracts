# probe-redact-old-new.R -- why does the new extractor flag twice as many contracts as the old one?
#
# Same text, near-identical rule, same markers-per-contract where both find markers, twice as many
# contracts with any marker at all. Two candidates from reading the old code: the old pipeline
# dropped any bracket longer than 50 characters, and it matched only against a frozen list of bracket
# strings it had seen when the list was cached. This joins old to new, isolates the contracts only
# the new one flags, and reads what their markers actually are. Writes nothing.
#
# PREDICTIONS, STATED FIRST.
#   If the 50-character cap is the cause, the newly-found markers are long: nchar(MarkText) > 52.
#   If the frozen list is the cause, they are ordinary [***] strings with unusual asterisk counts.
#   If neither, they are something that is not a redaction, and the rule needs tightening.

here::i_am("1_code/_Commons/_Initialize.R")

path_old_   <- "/Users/matthiasuckert/RProjects/Projects/pMatDisc/2_output/06-Redactions/Output/Readactions.parquet"
path_new_   <- here::here("2_output/10-ExportData/Output/Contracts.parquet")
dir_spans_  <- here::here("2_output/04D-EntityApply/Store/REDACT/7b84db9925eb")

old_ <- arrow::read_parquet(path_old_) |>
  dplyr::select("DocID", OldSymbol = "RedactSymbol", OldExplicit = "RedactExplicit")

new_ <- arrow::open_dataset(path_new_) |>
  dplyr::filter(.data$DescSample == 1L, .data$PrimaryFiler == 1L) |>
  dplyr::select("DocID", "DateFiled", "HasCto", "nRedactSymbol", "nRedactExplicit") |>
  dplyr::collect()

j_ <- new_ |>
  dplyr::left_join(old_, by = "DocID") |>
  dplyr::mutate(
    OldSymbol = dplyr::coalesce(.data$OldSymbol, 0L),
    NewSymbol = dplyr::coalesce(.data$nRedactSymbol, 0L),
    InOld     = !is.na(old_$OldSymbol[match(.data$DocID, old_$DocID)])
  )

cat("\n== Coverage: is every new-sample contract in the old file? ==\n")
print(table(InOldFile = j_$InOld))

cat("\n== Symbol: any marker, old against new ==\n")
print(table(Old = j_$OldSymbol > 0, New = j_$NewSymbol > 0))

cat("\n== Where both flag: markers per contract agree? ==\n")
both_ <- dplyr::filter(j_, .data$OldSymbol > 0, .data$NewSymbol > 0)
print(summary(both_$NewSymbol - both_$OldSymbol))
cat("share with identical count:", round(mean(both_$NewSymbol == both_$OldSymbol), 3), "\n")

cat("\n== New-only: how many markers do they have? ==\n")
newonly_ <- dplyr::filter(j_, .data$OldSymbol == 0, .data$NewSymbol > 0)
print(table(cut(newonly_$NewSymbol, c(0, 1, 2, 4, 9, 19, Inf))))
cat("CTO precision, new-only pre-FAST:",
    round(mean(newonly_$HasCto[newonly_$DateFiled < as.Date("2019-04-02")] == 1L), 3), "\n")
cat("CTO precision, both, pre-FAST:   ",
    round(mean(both_$HasCto[both_$DateFiled < as.Date("2019-04-02")] == 1L), 3), "\n")

cat("\n== What the new-only markers actually are ==\n")
files_ <- list.files(dir_spans_, pattern = "^redact_spans\\.parquet$", recursive = TRUE, full.names = TRUE)
ids_   <- newonly_$DocID
spans_ <- arrow::open_dataset(files_) |>
  dplyr::filter(.data$Kind == "RedactSymbol", .data$DocID %in% ids_) |>
  dplyr::select("DocID", "MarkText") |>
  dplyr::collect() |>
  dplyr::mutate(Len = nchar(.data$MarkText), Stars = stringi::stri_count_fixed(.data$MarkText, "*"))

cat("markers read:", nrow(spans_), "on", dplyr::n_distinct(spans_$DocID), "contracts\n")
cat("\nlength of the marker string (old cap was 50):\n")
print(table(cut(spans_$Len, c(0, 5, 10, 20, 50, 52, 80, Inf))))
cat("\nshare over the old cap:", round(mean(spans_$Len > 50), 3), "\n")
cat("\nasterisks per marker:\n")
print(table(cut(spans_$Stars, c(0, 1, 2, 3, 5, 10, 20, 50, Inf))))

cat("\ntwenty most common marker strings among new-only contracts:\n")
spans_ |>
  dplyr::count(.data$MarkText, sort = TRUE) |>
  dplyr::slice_head(n = 20) |>
  dplyr::mutate(MarkText = stringi::stri_sub(.data$MarkText, 1L, 60L)) |>
  print(n = 20)

cat("\n== The reverse: old flags, new does not ==\n")
oldonly_ <- dplyr::filter(j_, .data$OldSymbol > 0, .data$NewSymbol == 0)
cat(nrow(oldonly_), "contracts; old markers on them:\n")
print(summary(oldonly_$OldSymbol))
