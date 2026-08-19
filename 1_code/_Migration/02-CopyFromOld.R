# 02-CopyFromOld: seed 02's Compustat downloads and reference outputs ---------------------------------------------------
#
# WHAT THIS IS
# Scaffolding, not pipeline. 02-SelectSample.qmd is written as what it is -- a script that downloads
# Compustat and merges it to the corpus. This exists so the download does not have to happen again:
# it places the previous project's Compustat extracts where 02 would have written them, so the
# document renders against real data with its acquisition switch off. It is the only file in the 02
# family that names the previous project, and it is deleted once the migration closes.
#
# WHAT IT COPIES
#   old/Compustat/AnnualData.parquet   -> 02-SelectSample/Compustat/AnnualData.parquet
#   old/Compustat/QuarterData.parquet  -> 02-SelectSample/Compustat/QuarterData.parquet
#   old/Compustat/QuarterRange.parquet -> 02-SelectSample/Compustat/QuarterRange.parquet
#   old/Output/Sample*.parquet         -> 02-SelectSample/Sample*.parquet
#   old/Overview/SampleSelect*.parquet -> 02-SelectSample/Overview/SampleSelect*.parquet
#
# THE TWO KINDS OF FILE HERE ARE NOT THE SAME
# AnnualData and QuarterData are the frozen downloads: 02 reads them and never rewrites them, so
# they stay in place through every run. Everything else is an OUTPUT of 02, seeded only so the
# verification has something to compare against, and moved aside before the render.
#
# NOTE THE FLATTENING. The previous layout nested the sample tables under Output/; this one writes
# them at the top of the script's directory, because a directory named Output inside a directory
# that is entirely output says nothing.
#
# THREE SAFETY PROPERTIES
#
# 1. DRY RUN BY DEFAULT. .APPLY is FALSE. The first run reports the plan and changes nothing.
# 2. IT REFUSES RATHER THAN OVERWRITES, and aborts entirely rather than skipping the offending row.
# 3. IT COPIES, IT DOES NOT MOVE. The previous project is left exactly as it was.
#
# Run from the project root:  source("1_code/_Migration/02-CopyFromOld.R")

.APPLY   <- FALSE    # FALSE reports the plan and changes nothing; TRUE performs the copy
.DIR_OLD <- NULL     # set to a path to override the sibling-project autodetect


# 1. Tools -------------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

size_ <- function(.path) {
  s_ <- file.size(.path)
  if (is.na(s_)) "ABSENT" else format(structure(s_, class = "object_size"), units = "auto")
}


# 2. Resolve both projects ---------------------------------------------------------------------------------------------

dir_new_ <- here::here()
dir_old_ <- if (!is.null(.DIR_OLD)) .DIR_OLD else file.path(dirname(dir_new_), "pMatDisc")
dir_src_ <- file.path(dir_old_, "2_output", "02-SelectSample")
dir_02_  <- file.path("2_output", "02-SelectSample")

say_("== Seed 02 ==")
say_("FROM : ", dir_src_, if (dir.exists(dir_src_)) "   [ok]" else "   [MISSING]")
say_("TO   : ", file.path(dir_new_, dir_02_))
say_("MODE : ", if (isTRUE(.APPLY)) "APPLY -- files will be written" else "DRY RUN -- nothing will be written")

if (!dir.exists(dir_src_)) {
  stop("Previous 02-SelectSample output not found. Set .DIR_OLD at the top of this script.")
}
if (!file.exists(file.path(dir_new_, "2_output", "01C-EdgarMetaData", "FullMetaData.parquet"))) {
  stop("01C's metadata is missing. Run 01C-Execute.R before this.")
}


# 3. The plan ----------------------------------------------------------------------------------------------------------
#
# Kind separates the frozen downloads from the seeded outputs: the first stay put through every run,
# the second are moved aside before the render so that what the render produces can be compared.

plan_ <- tibble::tribble(
  ~Label,        ~Kind,      ~From,                                     ~To,
  "AnnualData",  "download", "Compustat/AnnualData.parquet",            "Compustat/AnnualData.parquet",
  "QuarterData", "download", "Compustat/QuarterData.parquet",           "Compustat/QuarterData.parquet",
  "QuarterRange", "output",  "Compustat/QuarterRange.parquet",          "Compustat/QuarterRange.parquet",
  "Exhibit10",   "output",   "Output/SampleExhibit10.parquet",          "SampleExhibit10.parquet",
  "Filing08K",   "output",   "Output/SampleFiling08K.parquet",          "SampleFiling08K.parquet",
  "FilingCTO",   "output",   "Output/SampleFilingCTO.parquet",          "SampleFilingCTO.parquet",
  "SelExhibit10", "output",  "Overview/SampleSelectExhibit10.parquet",  "Overview/SampleSelectExhibit10.parquet",
  "SelFiling08K", "output",  "Overview/SampleSelectFiling08K.parquet",  "Overview/SampleSelectFiling08K.parquet",
  "SelFilingCTO", "output",  "Overview/SampleSelectFilingCTO.parquet",  "Overview/SampleSelectFilingCTO.parquet",
  "FilingOverview", "output", "Cache/FilingOverviews.parquet",           "Overview/FilingOverviews.parquet",
  "Ex10FormCount", "output",  "Overview/Ex10FilingCount.parquet",        "Overview/Ex10FilingCount.parquet"
) |>
  dplyr::mutate(
    PathFrom  = file.path(dir_src_, .data$From),
    PathTo    = file.path(dir_new_, dir_02_, .data$To),
    SrcExists = file.exists(.data$PathFrom),
    Blocked   = file.exists(.data$PathTo),
    Size      = vapply(.data$PathFrom, size_, character(1))
  )

say_("\n-- Plan --")
plan_ |>
  dplyr::mutate(
    Source      = ifelse(.data$SrcExists, "ok", "MISSING"),
    Destination = ifelse(.data$Blocked, "OCCUPIED", "clear")
  ) |>
  dplyr::select("Label", "Kind", "From", "To", "Size", "Source", "Destination") |>
  as.data.frame() |>
  print(row.names = FALSE)


# 4. Refuse on any collision or missing source -------------------------------------------------------------------------

if (any(!plan_$SrcExists)) {
  say_("\nMissing sources: ", paste(plan_$Label[!plan_$SrcExists], collapse = ", "))
  stop("Aborting: at least one source is missing. Nothing was copied.")
}

if (any(plan_$Blocked)) {
  say_("\nOccupied destinations: ", paste(plan_$Label[plan_$Blocked], collapse = ", "))
  stop("Aborting: at least one destination already exists. Move it aside first. Nothing was copied.")
}


# 5. Execute -----------------------------------------------------------------------------------------------------------

if (!isTRUE(.APPLY)) {
  say_("\nDRY RUN complete. Nothing was written.")
  say_("Set .APPLY <- TRUE at the top of this script and source it again to perform the copy.")
} else {
  say_("\n-- Copying --")

  for (i_ in seq_len(nrow(plan_))) {
    row_ <- plan_[i_, ]
    say_("  ", row_$Label, " (", row_$Size, ") ...")

    fs::dir_create(dirname(row_$PathTo))
    st_ <- system2("cp", c("-c", shQuote(row_$PathFrom), shQuote(row_$PathTo)))
    if (!identical(st_, 0L)) stop("cp failed for ", row_$Label, " with status ", st_)
  }

  # 6. Verify ----------------------------------------------------------------------------------------------------------

  say_("\n-- Verification --")
  ver_ <- plan_ |>
    dplyr::mutate(
      BytesFrom = file.size(.data$PathFrom),
      BytesTo   = file.size(.data$PathTo),
      Match     = ifelse(.data$BytesFrom == .data$BytesTo, "ok", "MISMATCH")
    ) |>
    dplyr::select("Label", "BytesFrom", "BytesTo", "Match")

  print(as.data.frame(ver_), row.names = FALSE)

  if (any(ver_$Match != "ok")) {
    stop("Sizes do not match. The previous project is untouched; investigate before proceeding.")
  }

  say_("\nAll sizes match. The previous project is untouched and remains the fallback.")
  say_("Next: source 02-VerifyMigration.R with .STAGE <- 'before', then render 02.")
}
