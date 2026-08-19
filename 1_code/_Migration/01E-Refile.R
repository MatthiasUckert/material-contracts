# 01E-Refile: is a redacted contract ever filed again? -------------------------------------------------------------------
#
# WHAT THIS IS
# Quick and dirty. Read-only, metadata only, no document is opened. It answers one question before
# anything expensive is built on top of it: does a filer who redacted a contract ever put the same
# contract on EDGAR again?
#
# WHY IT CAN BE CHEAP
# Saying "this is the same contract, filed later" normally means comparing text, which is a pass over
# 1.46 million documents. But 01C already measured every document: characters, words and numeric
# tokens. Two documents agreeing on all three are candidates for being the same text, and two that
# disagree on any of them cannot be. So the candidates come free, and hashing -- if it turns out to
# be worth doing -- runs over a small subset rather than the corpus.
#
# WHAT THIS CANNOT TELL US
# Matching on three counts is necessary, not sufficient: two different contracts of similar length
# will collide. The numbers below are therefore an UPPER BOUND on re-filing. If the upper bound is
# small, the question is settled and no hashing is needed. If it is large, hashing the candidates is
# the next step and it is cheap because the candidate set is small.
#
# AND ONE THING THE MECHANISM SUGGESTS
# Under Rule 24b-2 the complete document goes to the Commission, not to EDGAR. Nothing obliges a
# filer to re-file an unredacted version when the order lapses, so the honest prior is that this is
# rare. That is a prediction, and the point of the script is to check it rather than assert it.
#
# Run from the project root:  source("1_code/_Migration/01E-Refile.R")


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

sub_ <- function(...) {
  say_("\n", strrep("-", 100))
  say_("  ", ...)
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)
pct_ <- function(.x) paste0(format(round(100 * .x, 1), nsmall = 1), "%")

show_ <- function(.tab, .n = 10L) {
  .tab |> utils::head(.n) |> as.data.frame() |> print(row.names = FALSE)
}


# 2. Load --------------------------------------------------------------------------------------------------------------

dir_meta_ <- here::here("2_output", "01C-EdgarMetaData")
dir_cto_  <- here::here("2_output", "01E-CtoExhibits")

cto_ <- arrow::read_parquet(fs::path(dir_cto_, "CtoExhibits.parquet"))

# Every contract, with the three measures that stand in for its text.
con_ <- arrow::open_dataset(sources = fs::path(dir_meta_, "FullMetaData.parquet")) |>
  dplyr::filter(grepl("^Exhibit10", .data$DocTypeMod)) |>
  dplyr::select("DocID", "HashIndex", "HashDocument", "CIK", "DateFiled",
                "nChars", "nWords", "nNums", "Removed") |>
  dplyr::collect()

rule_("0. The corpus")
say_("  contracts        : ", fmt_(nrow(con_)))
say_("  filers           : ", fmt_(dplyr::n_distinct(con_$CIK)))
say_("  linked to an order: ", fmt_(sum(!is.na(cto_$DocIDContract))))


# 3. How often is any contract filed twice? ------------------------------------------------------------------------------
#
# The base rate, and the thing the redaction question has to be read against. Grouped by filer as
# well as by measurements, because two firms filing identical-length documents are not one contract.

rule_("1. Base rate: does the same filer file the same text twice?")

grp_ <- con_ |>
  dplyr::filter(!is.na(.data$nChars), .data$nChars > 0L) |>
  dplyr::summarise(
    nDocs      = dplyr::n(),
    nUrls      = dplyr::n_distinct(.data$HashDocument),
    nFilings   = dplyr::n_distinct(.data$HashIndex),
    FirstFiled = min(.data$DateFiled),
    LastFiled  = max(.data$DateFiled),
    .by = c("CIK", "nChars", "nWords", "nNums")
  )

sub_("Documents sharing filer and all three measures")
tibble::tibble(
  nGroups        = nrow(grp_),
  nSingletons    = sum(grp_$nDocs == 1L),
  nRepeatGroups  = sum(grp_$nDocs > 1L),
  nDocsInRepeats = sum(grp_$nDocs[grp_$nDocs > 1L])
) |>
  as.data.frame() |>
  print(row.names = FALSE)

sub_("Of the repeats, how many span MORE THAN ONE FILING?")
say_("  A group inside one filing is the same attachment under several registrants, which 01C")
say_("  already flags. Only a group spanning filings is a re-filing.")

multi_ <- dplyr::filter(grp_, .data$nDocs > 1L, .data$nFilings > 1L)

tibble::tibble(
  nGroups     = nrow(multi_),
  nDocs       = sum(multi_$nDocs),
  ShareOfCorp = pct_(sum(multi_$nDocs) / nrow(con_)),
  MedianGap   = stats::median(as.numeric(multi_$LastFiled - multi_$FirstFiled)),
  MaxGapYears = round(max(as.numeric(multi_$LastFiled - multi_$FirstFiled)) / 365.25, 1)
) |>
  as.data.frame() |>
  print(row.names = FALSE)

sub_("How far apart are the re-filings?")
multi_ |>
  dplyr::mutate(
    Gap = as.numeric(.data$LastFiled - .data$FirstFiled),
    Band = dplyr::case_when(
      .data$Gap <= 31   ~ "1-within a month",
      .data$Gap <= 365  ~ "2-under a year",
      .data$Gap <= 1095 ~ "3-one to three years",
      .data$Gap <= 2190 ~ "4-three to six years",
      .default          = "5-over six years"
    )
  ) |>
  dplyr::count(.data$Band, name = "nGroups") |>
  dplyr::mutate(Share = pct_(.data$nGroups / sum(.data$nGroups))) |>
  as.data.frame() |>
  print(row.names = FALSE)


# 4. Do CTO-covered contracts get re-filed more? -------------------------------------------------------------------------
#
# The question that matters. If a contract under a confidential-treatment order is no more likely to
# reappear than any other, then nothing is being re-filed when orders lapse, and looking for
# unredacted versions on EDGAR is looking in the wrong place.

rule_("2. Are covered contracts re-filed more often than uncovered ones?")

cov_ <- cto_ |>
  dplyr::filter(!is.na(.data$DocIDContract), !is.na(.data$ReleaseDate)) |>
  dplyr::summarise(ReleaseDate = min(.data$ReleaseDate), .by = "DocIDContract")

# A GROUP OF FIVE HUNDRED "IDENTICAL" DOCUMENTS IS A FORM, NOT A CONTRACT. Where one filer has many
# documents agreeing on all three measures, the measures are not identifying a document -- they are
# identifying a template that filer uses repeatedly. Those groups are excluded and counted, because
# including them would let one boilerplate filer dominate every number below, and because the join
# in section 3 is quadratic in group size.
.CAP <- 20L

big_ <- dplyr::filter(grp_, .data$nDocs > .CAP)
say_("\n  groups larger than ", .CAP, " documents, excluded as templates: ", fmt_(nrow(big_)),
     " covering ", fmt_(sum(big_$nDocs)), " documents (", pct_(sum(big_$nDocs) / nrow(con_)), ")")

multi_ <- dplyr::filter(multi_, .data$nDocs <= .CAP)

rep_ <- multi_ |>
  dplyr::select("CIK", "nChars", "nWords", "nNums") |>
  dplyr::mutate(InRepeatGroup = TRUE)

flag_ <- con_ |>
  dplyr::left_join(rep_, by = dplyr::join_by("CIK", "nChars", "nWords", "nNums")) |>
  dplyr::mutate(
    InRepeatGroup = dplyr::coalesce(.data$InRepeatGroup, FALSE),
    Covered       = .data$DocID %in% cov_$DocIDContract
  )

flag_ |>
  dplyr::summarise(
    nDocs       = dplyr::n(),
    nRefiled    = sum(.data$InRepeatGroup),
    ShareRefile = pct_(mean(.data$InRepeatGroup)),
    .by = "Covered"
  ) |>
  dplyr::arrange(dplyr::desc(.data$Covered)) |>
  as.data.frame() |>
  print(row.names = FALSE)

say_("\n  If the two shares are close, a confidential-treatment order does not make a contract more")
say_("  likely to reappear, and there is no unredacted re-filing to find.")


# 5. Does anything appear AFTER the release date? ------------------------------------------------------------------------
#
# The sharpest version of the question. For a covered contract with a known release date, is there a
# later document by the same filer with the same measurements -- that is, the same text filed again
# once the protection lapsed?

rule_("3. Does a covered contract reappear after its order expires?")

cand_ <- flag_ |>
  dplyr::filter(.data$Covered) |>
  dplyr::left_join(cov_, by = dplyr::join_by("DocID" == "DocIDContract")) |>
  dplyr::filter(!is.na(.data$ReleaseDate)) |>
  dplyr::select("DocID", "CIK", "DateFiled", "ReleaseDate", "nChars", "nWords", "nNums")

say_("  covered contracts with a known release date: ", fmt_(nrow(cand_)))
say_("  of those, release date already passed      : ", fmt_(sum(cand_$ReleaseDate < Sys.Date())))

later_ <- cand_ |>
  dplyr::left_join(
    y = dplyr::select(con_, "CIK", "nChars", "nWords", "nNums",
                      OtherDocID = "DocID", OtherFiled = "DateFiled"),
    by = dplyr::join_by("CIK", "nChars", "nWords", "nNums"),
    relationship = "many-to-many"
  ) |>
  dplyr::filter(.data$OtherDocID != .data$DocID) |>
  dplyr::mutate(
    AfterRelease = .data$OtherFiled > .data$ReleaseDate,
    AfterFiling  = .data$OtherFiled > .data$DateFiled
  )

sub_("Covered contracts with a same-measure document elsewhere")
tibble::tibble(
  nCovered        = nrow(cand_),
  nWithAnyOther   = dplyr::n_distinct(later_$DocID),
  nWithLaterOther = dplyr::n_distinct(later_$DocID[later_$AfterFiling]),
  nAfterRelease   = dplyr::n_distinct(later_$DocID[later_$AfterRelease])
) |>
  dplyr::mutate(ShareAfterRelease = pct_(.data$nAfterRelease / .data$nCovered)) |>
  as.data.frame() |>
  print(row.names = FALSE)

if (any(later_$AfterRelease)) {
  sub_("Examples: covered contract, and a same-measure document filed after the order lapsed")
  later_ |>
    dplyr::filter(.data$AfterRelease) |>
    dplyr::mutate(YearsAfter = round(as.numeric(.data$OtherFiled - .data$ReleaseDate) / 365.25, 1)) |>
    dplyr::select("DocID", "DateFiled", "ReleaseDate", "OtherDocID", "OtherFiled",
                  "YearsAfter", "nChars") |>
    dplyr::arrange(.data$YearsAfter) |>
    show_()
}


# 6. Verdict -------------------------------------------------------------------------------------------------------------

rule_("4. What this decides")

say_("  These counts are an UPPER BOUND: agreeing on three measures is necessary for two documents")
say_("  to be the same text, not sufficient. Two unrelated contracts of the same length collide.")
say_("")
say_("  If section 2 shows covered and uncovered contracts re-filed at similar rates, and section 3")
say_("  finds few appearing after release, then unredacted versions are not being put on EDGAR --")
say_("  which is what Rule 24b-2 implies, since the complete document goes to the Commission.")
say_("")
say_("  If the numbers are large, hashing the candidates is the next step, and it is cheap: the")
say_("  candidate set is a small fraction of the corpus rather than all of it.")
