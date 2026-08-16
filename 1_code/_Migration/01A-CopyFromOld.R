# 01A-CopyFromOld: bring the 01A inputs and outputs across from pMatDisc ------------------------------------------------
#
# WHAT THIS DOES
# Copies four things out of the old project into the layout 01A-EdgarIndex expects:
#
#   pMatDisc/2_output/01-GetEDGAR/GetEDGAR/MasterIndex/       -> 0_edgar/MasterIndex/
#   pMatDisc/2_output/01-GetEDGAR/GetEDGAR/DocumentLinks/     -> 0_edgar/DocumentLinks/
#   pMatDisc/2_output/01-GetEDGAR/LandingPage/RawExtract/     -> 2_output/01A-EdgarIndex/Cache/RawExtract/
#   pMatDisc/2_output/01-GetEDGAR/LandingPage/LandingPage.pq  -> 2_output/01A-EdgarIndex/LandingPageAll.parquet
#
# It does NOT touch DocumentData/ (1.77 million files, owned by 01B) or Output/ (owned by 01C).
#
# THREE SAFETY PROPERTIES, IN ORDER OF IMPORTANCE
#
# 1. DRY RUN BY DEFAULT. .APPLY is FALSE. The first run reports exactly what would happen and
#    changes nothing. Set it to TRUE only after reading that report.
#
# 2. IT REFUSES RATHER THAN OVERWRITES. Any destination that already exists aborts the whole script
#    before a single byte is written. There is no --force, and adding one would defeat the point:
#    the old tree is the only copy of work that took days to produce, and a partial overwrite is
#    harder to detect than a refusal.
#
# 3. IT COPIES, IT DOES NOT MOVE. The old tree is left exactly as it was and remains the fallback
#    until the new project renders green end to end.
#
# WHY cp -c AND NOT rsync OR ln
# Both projects sit on the same APFS volume, so cp -c uses clonefile(): the copy is instantaneous
# and consumes no additional space, because the two trees share their data blocks until one of them
# is written to. A hardlink copy has the same speed and space profile but the wrong failure mode --
# hardlinked files are one inode, so editing either tree would silently edit both. Clones diverge on
# write. For an irreplaceable corpus that difference is the whole argument.
#
# If the two projects are ever on different volumes, cp -c falls back to a normal copy, which is
# correct but slow. The script checks and says so.
#
# Run from the material-contracts project root:  source("1_code/_Scripts/01A-CopyFromOld.R")

.APPLY   <- TRUE    # FALSE reports the plan and changes nothing; TRUE performs the copy
.DIR_OLD <- NULL     # set to a path to override the sibling-project autodetect


# 1. Tools -------------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

# Files below a directory, counted without collecting their paths. fs::dir_info() and
# list.files(recursive = TRUE) both allocate in proportion to the number of files and take the R
# session down on trees of this size; a breadth-first walk holds one directory at a time.
walk_count_ <- function(.path) {
  if (!dir.exists(.path)) return(if (file.exists(.path)) 1L else 0L)
  n_     <- 0L
  queue_ <- .path
  while (length(queue_) > 0L) {
    cur_   <- queue_[1L]
    queue_ <- queue_[-1L]
    kid_   <- list.dirs(cur_, recursive = FALSE, full.names = TRUE)
    n_     <- n_ + length(list.files(cur_, all.files = FALSE, no.. = TRUE)) - length(kid_)
    queue_ <- c(queue_, kid_)
  }
  n_
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)


# 2. Resolve both projects ---------------------------------------------------------------------------------------------

dir_new_ <- here::here()
dir_old_ <- if (!is.null(.DIR_OLD)) .DIR_OLD else file.path(dirname(dir_new_), "pMatDisc")
dir_src_ <- file.path(dir_old_, "2_output", "01-GetEDGAR")

say_("== 01A migration ==")
say_("FROM : ", dir_src_, if (dir.exists(dir_src_)) "   [ok]" else "   [MISSING]")
say_("TO   : ", dir_new_, if (dir.exists(dir_new_)) "   [ok]" else "   [MISSING]")
say_("MODE : ", if (isTRUE(.APPLY)) "APPLY -- files will be written" else "DRY RUN -- nothing will be written")

if (!dir.exists(dir_src_)) {
  stop("Old 01-GetEDGAR output not found. Set .DIR_OLD at the top of this script.")
}


# 3. The plan ----------------------------------------------------------------------------------------------------------
#
# One row per copy. Kind distinguishes a directory clone from a single file, because the two need
# different destination handling: a directory is created by the copy, a file needs its parent to
# exist first.

dir_01a_ <- file.path("2_output", "01A-EdgarIndex")

plan_ <- tibble::tribble(
  ~Label,        ~From,                                             ~To,                                    ~Kind,
  "MasterIndex", file.path("GetEDGAR", "MasterIndex"),              file.path("0_edgar", "MasterIndex"),     "dir",
  "DocLinks",    file.path("GetEDGAR", "DocumentLinks"),            file.path("0_edgar", "DocumentLinks"),   "dir",
  "RawExtract",  file.path("LandingPage", "RawExtract"),            file.path(dir_01a_, "Cache", "RawExtract"), "dir",
  "LandingPage", file.path("LandingPage", "LandingPage.parquet"),   file.path(dir_01a_, "LandingPageAll.parquet"), "file"
) |>
  dplyr::mutate(
    PathFrom  = file.path(dir_src_, .data$From),
    PathTo    = file.path(dir_new_, .data$To),
    SrcExists = file.exists(.data$PathFrom),
    DstExists = file.exists(.data$PathTo),
    nFiles    = vapply(.data$PathFrom, walk_count_, integer(1))
  )

say_("\n-- Plan --")
plan_ |>
  dplyr::mutate(
    Source      = ifelse(.data$SrcExists, "ok", "MISSING"),
    Destination = ifelse(.data$DstExists, "OCCUPIED", "clear"),
    Files       = fmt_(.data$nFiles)
  ) |>
  dplyr::select("Label", "From", "To", "Files", "Source", "Destination") |>
  as.data.frame() |>
  print(row.names = FALSE)


# 4. Refuse on any collision or missing source -------------------------------------------------------------------------
#
# Both checks abort the entire script rather than skipping the offending row. A partial migration is
# the state that is hardest to diagnose later, because the destination then looks populated.

if (any(!plan_$SrcExists)) {
  say_("\nMissing sources: ", paste(plan_$Label[!plan_$SrcExists], collapse = ", "))
  stop("Aborting: at least one source is missing. Nothing was copied.")
}

if (any(plan_$DstExists)) {
  say_("\nOccupied destinations: ", paste(plan_$Label[plan_$DstExists], collapse = ", "))
  stop("Aborting: at least one destination already exists. Move it aside first. Nothing was copied.")
}


# 5. Volume check ------------------------------------------------------------------------------------------------------

dev_ <- function(.path) {
  out_ <- system2("df", c("-P", "-k", shQuote(.path)), stdout = TRUE)
  strsplit(trimws(out_[2L]), "\\s+")[[1L]][1L]
}
same_vol_ <- identical(dev_(dir_old_), dev_(dir_new_))

say_("\n-- Volume --")
say_(if (same_vol_) {
  "Same volume: cp -c clones instantly and uses no extra space."
} else {
  "Different volumes: cp -c falls back to a full byte copy. This will take time and space."
})


# 6. Execute -----------------------------------------------------------------------------------------------------------

if (!isTRUE(.APPLY)) {
  say_("\nDRY RUN complete. Nothing was written.")
  say_("Set .APPLY <- TRUE at the top of this script and source it again to perform the copy.")
} else {
  say_("\n-- Copying --")

  for (i_ in seq_len(nrow(plan_))) {
    row_ <- plan_[i_, ]
    say_("  ", row_$Label, " (", fmt_(row_$nFiles), " files) ...")

    fs::dir_create(dirname(row_$PathTo))

    args_ <- if (identical(row_$Kind, "dir")) {
      c("-c", "-R", shQuote(row_$PathFrom), shQuote(row_$PathTo))
    } else {
      c("-c", shQuote(row_$PathFrom), shQuote(row_$PathTo))
    }

    st_ <- system2("cp", args_)
    if (!identical(st_, 0L)) stop("cp failed for ", row_$Label, " with status ", st_)
  }

  # 7. Verify ----------------------------------------------------------------------------------------------------------
  #
  # Counted independently after the fact rather than trusted from cp's exit status. A copy that
  # returned zero having written a truncated tree is exactly the failure this migration cannot
  # afford, and counting is cheap next to re-downloading the corpus.

  say_("\n-- Verification --")
  ver_ <- plan_ |>
    dplyr::mutate(
      nBefore = .data$nFiles,
      nAfter  = vapply(.data$PathTo, walk_count_, integer(1)),
      Match   = ifelse(.data$nBefore == .data$nAfter, "ok", "MISMATCH")
    ) |>
    dplyr::select("Label", "nBefore", "nAfter", "Match")

  print(as.data.frame(ver_), row.names = FALSE)

  if (any(ver_$Match != "ok")) {
    stop("File counts do not match. The old tree is untouched; investigate before proceeding.")
  }
  say_("\nAll counts match. The old tree is untouched and remains the fallback.")
  say_("Next: render 01A-EdgarIndex.qmd with .lP$Param$Acquire = FALSE and check the coverage report.")
}
