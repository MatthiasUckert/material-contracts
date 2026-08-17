# 01C-CopyFromOld: seed the measurement caches and outputs from the previous project ------------------------------------
#
# WHAT THIS IS
# Scaffolding, not pipeline. 01C-EdgarMetaData.qmd is written as what it is -- a script that opens
# every document, measures it, and consolidates the result. This script exists so that the three
# measurement passes do not have to run a second time: it places the previous project's already
# computed caches and outputs exactly where 01C would have written them, so the document renders
# against real data in seconds rather than hours. It is deleted once the migration closes.
#
# WHAT IT COPIES
#   old/Cache/StopWords.parquet              -> 01C/Cache/StopWords.parquet
#   old/DocumentStats/DocumentCheck.parquet  -> 01C/Cache/DocErrors.parquet
#   old/DocumentStats/DocStats.parquet       -> 01C/Cache/DocStats.parquet
#   old/DocumentStats/StopWords.parquet      -> 01C/Cache/DocStop.parquet
#   old/Output/DocStats.parquet              -> 01C/DocStats.parquet
#   old/Output/RemovedDocs.parquet           -> 01C/RemovedDocs.parquet
#   old/Output/LinkData.parquet              -> 01C/LinkData.parquet
#   old/Output/MasterIndex.parquet           -> 01C/MasterIndex.parquet
#   old/Output/LandingPage.parquet           -> 01C/LandingPage.parquet
#   old/Output/FullMetaData.parquet          -> 01C/FullMetaData.parquet
#
# NOTE THE RENAMES. Three of the caches change name on the way across, because 01C names each pass
# after what it measures rather than after where it was stored. DocumentCheck becomes DocErrors,
# and the two files both called StopWords -- the dictionary and the per-document counts -- become
# StopWords and DocStop, which can no longer be confused for one another.
#
# The consolidated outputs are seeded as well as the caches. They are what the verification script
# compares against, and without them the render would have nothing to be checked against.
#
# THREE SAFETY PROPERTIES
#
# 1. DRY RUN BY DEFAULT. .APPLY is FALSE. The first run reports the plan and changes nothing.
# 2. IT REFUSES RATHER THAN OVERWRITES, and aborts entirely rather than skipping the offending row.
# 3. IT COPIES, IT DOES NOT MOVE. The previous project is left exactly as it was.
#
# Run from the project root:  source("1_code/_Migration/01C-CopyFromOld.R")

.APPLY   <- FALSE    # FALSE reports the plan and changes nothing; TRUE performs the copy
.DIR_OLD <- NULL     # set to a path to override the sibling-project autodetect


# 1. Tools -------------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

size_ <- function(.path) {
  s_ <- file.size(.path)
  if (is.na(s_)) "ABSENT" else format(structure(s_, class = "object_size"), units = "auto")
}


# 2. Resolve both projects ---------------------------------------------------------------------------------------------

dir_new_ <- here::here()
dir_old_ <- if (!is.null(.DIR_OLD)) .DIR_OLD else file.path(dirname(dir_new_), "pMatDisc")
dir_src_ <- file.path(dir_old_, "2_output", "01-GetEDGAR")
dir_01c_ <- file.path("2_output", "01C-EdgarMetaData")

say_("== Seed 01C ==")
say_("FROM : ", dir_src_, if (dir.exists(dir_src_)) "   [ok]" else "   [MISSING]")
say_("TO   : ", file.path(dir_new_, dir_01c_))
say_("MODE : ", if (isTRUE(.APPLY)) "APPLY -- files will be written" else "DRY RUN -- nothing will be written")

if (!dir.exists(dir_src_)) {
  stop("Previous 01-GetEDGAR output not found. Set .DIR_OLD at the top of this script.")
}


# 3. The plan ----------------------------------------------------------------------------------------------------------

plan_ <- tibble::tribble(
  ~Label,         ~From,                                        ~To,
  "StopWords",    "Cache/StopWords.parquet",                    file.path("Cache", "StopWords.parquet"),
  "DocErrors",    "DocumentStats/DocumentCheck.parquet",        file.path("Cache", "DocErrors.parquet"),
  "DocStatsRaw",  "DocumentStats/DocStats.parquet",             file.path("Cache", "DocStats.parquet"),
  "DocStopRaw",   "DocumentStats/StopWords.parquet",            file.path("Cache", "DocStop.parquet"),
  "DocStats",     "Output/DocStats.parquet",                    "DocStats.parquet",
  "RemovedDocs",  "Output/RemovedDocs.parquet",                 "RemovedDocs.parquet",
  "LinkData",     "Output/LinkData.parquet",                    "LinkData.parquet",
  "MasterIndex",  "Output/MasterIndex.parquet",                 "MasterIndex.parquet",
  "LandingPage",  "Output/LandingPage.parquet",                 "LandingPage.parquet",
  "FullMetaData", "Output/FullMetaData.parquet",                "FullMetaData.parquet"
) |>
  dplyr::mutate(
    PathFrom  = file.path(dir_src_, .data$From),
    PathTo    = file.path(dir_new_, dir_01c_, .data$To),
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
  dplyr::select("Label", "From", "To", "Size", "Source", "Destination") |>
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
  #
  # Byte sizes rather than file counts, since every item here is a single file. A clone that
  # succeeded produces an identical size; anything else is a truncated write.

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
  say_("Next: source 01C-VerifyMigration.R with .STAGE <- 'before', then render 01C.")
}
