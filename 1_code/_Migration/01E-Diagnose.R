# 01E-Diagnose: why does a reference fail to reach a contract? -----------------------------------------------------------
#
# WHAT THIS IS
# A read-only investigation of the two largest link failures. Nothing is written and nothing is
# changed.
#
#   5-filing not in the index     2,896 references   the order named a filing we could not find
#   7-exhibit not in the filing   2,286 references   we found the filing but not the exhibit
#
# WHY THESE TWO
# Between them they are sixteen per cent of every exhibit reference in the corpus, which is more than
# the linkage recovers from the whole post-2019 period. More to the point, each label currently
# covers several different situations, and a label that covers several situations cannot be quoted.
#
# THE MAIN SUSPICION, STATED BEFORE IT IS TESTED
# The linkage joins against 01C's master index, which is RESTRICTED to filings whose documents were
# retrieved: 01C filters it on HashIndex before writing. A filing that exists on EDGAR but from which
# nothing was downloaded is therefore absent, and a reference to it is recorded as "filing not in the
# index" when the filing is perfectly well indexed. If that is right, category 5 is largely an
# artefact of which index the join uses, and the honest label for those references is that the
# exhibit was never acquired.
#
# Section 1 tests exactly that, by joining the same references against 01A's UNRESTRICTED index.
#
# Run from the project root:  source("1_code/_Migration/01E-Diagnose.R")

.N_SHOW <- 8L   # examples printed per finding


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

show_ <- function(.tab, .n = .N_SHOW) {
  .tab |> utils::head(.n) |> as.data.frame() |> print(row.names = FALSE)
}


# 2. Load --------------------------------------------------------------------------------------------------------------

purrr::walk(
  .x = c("_Commons/_Utils", "01A-EdgarIndex", "01E-CtoExhibits"),
  .f = \(.s) source(here::here("1_code", paste0(.s, ".R")), encoding = "UTF-8")
)

dir_meta_ <- here::here("2_output", "01C-EdgarMetaData")
dir_cto_  <- here::here("2_output", "01E-CtoExhibits")

cto_ <- arrow::read_parquet(fs::path(dir_cto_, "CtoExhibits.parquet"))

# The RESTRICTED index: what the linkage currently joins against.
rst_ <- arrow::read_parquet(fs::path(dir_meta_, "MasterIndex.parquet")) |>
  dplyr::select("CIK", "FormType", "DateFiled", "HashIndex") |>
  dplyr::distinct()

# The UNRESTRICTED index: every filing 01A mirrored, whether or not anything was downloaded from it.
edg_ <- rGetEDGAR::get_directories(here::here("2_output", "01A-EdgarIndex", "GetEDGAR"))

full_ <- arrow::open_dataset(sources = edg_$MasterIndex$DirParquet) |>
  dplyr::select("CIK", "FormType", "DateFiled", "HashIndex") |>
  dplyr::collect() |>
  dplyr::distinct()

rule_("0. The two indexes")
say_("  restricted (01C, filings whose documents we hold): ", fmt_(nrow(rst_)))
say_("  full       (01A, every filing mirrored)          : ", fmt_(nrow(full_)))
say_("  the restricted index is ", pct_(nrow(rst_) / nrow(full_)), " of the full one")

ex10_ <- arrow::open_dataset(sources = fs::path(dir_meta_, "FullMetaData.parquet")) |>
  dplyr::filter(grepl("^Exhibit10", .data$DocTypeMod)) |>
  dplyr::select("DocID", "HashIndex", "CIK", "DocTypeRaw") |>
  dplyr::collect() |>
  dplyr::mutate(ExhibitNo = cto_exhibit_number(.x = .data$DocTypeRaw))


# 3. Category 5 --------------------------------------------------------------------------------------------------------

rule_("1. Filing not in the index  (", fmt_(sum(cto_$LinkStatus == "5-filing not in the index")), " references)")

f5_ <- cto_ |>
  dplyr::filter(.data$LinkStatus == "5-filing not in the index") |>
  dplyr::select("DocID", "CIK", "DateFiled", "ExhibitNo", "UseForm", "UseFiledOn", "RefFormat")

sub_("Is the named filing in the FULL index, though absent from the restricted one?")

f5_chk_ <- f5_ |>
  dplyr::left_join(
    y  = dplyr::select(full_, "CIK", UseForm = "FormType", UseFiledOn = "DateFiled", FullHash = "HashIndex"),
    by = dplyr::join_by("CIK", "UseForm", "UseFiledOn"),
    relationship = "many-to-many"
  ) |>
  dplyr::summarise(nFullMatches = sum(!is.na(.data$FullHash)), .by = dplyr::all_of(names(f5_)))

f5_chk_ |>
  dplyr::mutate(
    Case = dplyr::case_when(
      .data$nFullMatches == 1L ~ "1-in the FULL index: the restriction hid it",
      .data$nFullMatches > 1L  ~ "2-in the FULL index, but ambiguous",
      .default                 = "3-not in EDGAR as parsed"
    )
  ) |>
  dplyr::count(.data$Case, name = "nRefs") |>
  dplyr::mutate(Share = pct_(.data$nRefs / sum(.data$nRefs))) |>
  as.data.frame() |>
  print(row.names = FALSE)

say_("\n  Case 1 means the label is wrong rather than the data: the filing is indexed, and what we")
say_("  lack is its documents. Those references belong in the exhibit category, not this one.")

sub_("Case 1: which form types, and are they inside the acquisition frame?")
f5_chk_ |>
  dplyr::filter(.data$nFullMatches == 1L) |>
  dplyr::count(.data$UseForm, name = "nRefs", sort = TRUE) |>
  dplyr::mutate(InFrame = .data$UseForm %in% edg_sec_forms()) |>
  show_(12L)

sub_("Case 3: not in EDGAR. Is the filer there at all, and is the form?")
f5_gone_ <- f5_chk_ |>
  dplyr::filter(.data$nFullMatches == 0L) |>
  dplyr::distinct(.data$CIK, .data$UseForm, .data$UseFiledOn)

pairs_ <- full_ |>
  dplyr::select("CIK", UseForm = "FormType") |>
  dplyr::distinct() |>
  dplyr::mutate(FormForFiler = TRUE)

f5_gone_ <- f5_gone_ |>
  dplyr::mutate(FilerInFull = .data$CIK %in% full_$CIK) |>
  dplyr::left_join(pairs_, by = dplyr::join_by("CIK", "UseForm")) |>
  dplyr::mutate(FormForFiler = dplyr::coalesce(.data$FormForFiler, FALSE))

f5_gone_ |>
  dplyr::count(.data$FilerInFull, .data$FormForFiler, name = "nCases") |>
  as.data.frame() |>
  print(row.names = FALSE)

sub_("Where the filer AND the form exist: how far off is the date?")
near_ <- f5_gone_ |>
  dplyr::filter(.data$FormForFiler) |>
  dplyr::left_join(
    y  = dplyr::select(full_, "CIK", UseForm = "FormType", CandDate = "DateFiled"),
    by = dplyr::join_by("CIK", "UseForm"),
    relationship = "many-to-many"
  ) |>
  dplyr::mutate(Days = abs(as.numeric(.data$CandDate - .data$UseFiledOn))) |>
  dplyr::summarise(Nearest = min(.data$Days, na.rm = TRUE), .by = c("CIK", "UseForm", "UseFiledOn"))

if (nrow(near_) > 0L) {
  near_ |>
    dplyr::mutate(
      Band = dplyr::case_when(
        .data$Nearest == 0   ~ "0 days: should have matched",
        .data$Nearest <= 3   ~ "1 to 3 days: a date convention",
        .data$Nearest <= 31  ~ "4 to 31 days",
        .data$Nearest <= 400 ~ "1 to 13 months",
        .default             = "over a year"
      )
    ) |>
    dplyr::count(.data$Band, name = "nCases", sort = TRUE) |>
    as.data.frame() |>
    print(row.names = FALSE)

  sub_("The one-to-three-day cases: would a tolerance recover them?")
  near_ |>
    dplyr::filter(.data$Nearest > 0, .data$Nearest <= 3) |>
    dplyr::arrange(.data$Nearest) |>
    show_()
}


# 4. Category 7 --------------------------------------------------------------------------------------------------------

rule_("2. Exhibit not in the filing  (", fmt_(sum(cto_$LinkStatus == "7-exhibit not in the filing")), " references)")

f7_ <- cto_ |>
  dplyr::filter(.data$LinkStatus == "7-exhibit not in the filing") |>
  dplyr::select("DocID", "CIK", "ExhibitNo", "HashIndex", "UseForm", "UseFiledOn")

by_filing_ <- ex10_ |>
  dplyr::summarise(
    nInFiling = dplyr::n(),
    Present   = paste(sort(unique(.data$ExhibitNo)), collapse = ", "),
    MaxMinor  = suppressWarnings(max(as.numeric(
      stringi::stri_replace_first_regex(unique(.data$ExhibitNo), "^10\\.", "")
    ), na.rm = TRUE)),
    .by = "HashIndex"
  )

f7_chk_ <- f7_ |>
  dplyr::left_join(by_filing_, by = dplyr::join_by("HashIndex")) |>
  dplyr::mutate(
    nInFiling  = dplyr::coalesce(.data$nInFiling, 0L),
    AskedMinor = suppressWarnings(as.numeric(
      stringi::stri_replace_first_regex(.data$ExhibitNo, "^10\\.", "")
    ))
  )

sub_("Hypothesis A: the filing carries no Exhibit 10 at all")
f7_chk_ |>
  dplyr::mutate(Case = dplyr::if_else(.data$nInFiling == 0L, "no Exhibit 10 on disk", "has Exhibit 10s")) |>
  dplyr::count(.data$Case, name = "nRefs") |>
  dplyr::mutate(Share = pct_(.data$nRefs / sum(.data$nRefs))) |>
  as.data.frame() |>
  print(row.names = FALSE)

sub_("Hypothesis B: the number is present but written differently (10.5 against EX-10.05)")
f7_has_ <- f7_chk_ |>
  dplyr::filter(.data$nInFiling > 0L) |>
  dplyr::mutate(
    NumericMatch = purrr::map2_lgl(
      .x = .data$ExhibitNo,
      .y = .data$Present,
      .f = function(.a, .p) {
        want_ <- suppressWarnings(as.numeric(stringi::stri_split_fixed(.a, ".")[[1L]]))
        if (anyNA(want_)) return(FALSE)
        have_ <- stringi::stri_split_regex(.p, ",\\s*")[[1L]]
        any(vapply(
          X   = have_,
          FUN = function(.h) {
            n_ <- suppressWarnings(as.numeric(stringi::stri_split_fixed(.h, ".")[[1L]]))
            length(n_) == length(want_) && !anyNA(n_) && all(n_ == want_)
          },
          FUN.VALUE = logical(1)
        ))
      }
    )
  )

f7_has_ |>
  dplyr::count(.data$NumericMatch, name = "nRefs") |>
  dplyr::mutate(Share = pct_(.data$nRefs / sum(.data$nRefs))) |>
  as.data.frame() |>
  print(row.names = FALSE)
say_("  TRUE means the exhibit IS there and the string comparison missed it: recoverable.")

if (any(f7_has_$NumericMatch)) {
  sub_("The recoverable ones")
  f7_has_ |>
    dplyr::filter(.data$NumericMatch) |>
    dplyr::select(Asked = "ExhibitNo", "Present", "nInFiling") |>
    show_()
}

sub_("Hypothesis C: the order asks for a number above the highest the filing attached")
f7_has_ |>
  dplyr::filter(!.data$NumericMatch, is.finite(.data$MaxMinor), !is.na(.data$AskedMinor)) |>
  dplyr::mutate(
    Case = dplyr::case_when(
      .data$AskedMinor > .data$MaxMinor ~ "asked above the highest number attached",
      .default                          = "asked inside the range but absent"
    )
  ) |>
  dplyr::count(.data$Case, name = "nRefs") |>
  dplyr::mutate(Share = pct_(.data$nRefs / sum(.data$nRefs))) |>
  as.data.frame() |>
  print(row.names = FALSE)
say_("  'Above the highest' is what incorporation by reference looks like from this side: the")
say_("  exhibit index numbers more exhibits than the filing actually attached.")

sub_("Hypothesis D: the exhibit exists for this filer, in a DIFFERENT filing")
by_filer_ <- ex10_ |>
  dplyr::distinct(.data$CIK, .data$ExhibitNo) |>
  dplyr::mutate(ElsewhereForFiler = TRUE)

f7_has_ |>
  dplyr::left_join(by_filer_, by = dplyr::join_by("CIK", "ExhibitNo")) |>
  dplyr::mutate(ElsewhereForFiler = dplyr::coalesce(.data$ElsewhereForFiler, FALSE)) |>
  dplyr::count(.data$NumericMatch, .data$ElsewhereForFiler, name = "nRefs") |>
  dplyr::mutate(Share = pct_(.data$nRefs / sum(.data$nRefs))) |>
  as.data.frame() |>
  print(row.names = FALSE)
say_("  ElsewhereForFiler TRUE with NumericMatch FALSE is the incorporation-by-reference signature:")
say_("  the filer did file that exhibit number, just not in the filing the order names.")

sub_("What the mismatches look like")
f7_has_ |>
  dplyr::filter(!.data$NumericMatch) |>
  dplyr::select(Asked = "ExhibitNo", "Present", "nInFiling", "UseForm") |>
  show_(10L)


# 5. What this decides -------------------------------------------------------------------------------------------------

rule_("3. What this decides")

say_("  Section 1  if most of category 5 sits in the FULL index, the label is wrong rather than the")
say_("             data. Those references should say the exhibit was never acquired, and the join")
say_("             should use the unrestricted index so the two remain distinguishable.")
say_("  Section 1  a cluster at one to three days is a date convention, and a tolerance is defensible.")
say_("  Section 2  hypothesis B is a bug worth fixing. C and D are properties of how filers")
say_("             incorporate exhibits by reference, and are worth reporting rather than chasing.")
