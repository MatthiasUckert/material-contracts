# 01A-CopyFromOld: seed 01A's output directory from the previous project ------------------------------------------------
#
# WHAT THIS IS
# Scaffolding, not pipeline. 01A-EdgarIndex.qmd is written as what it is -- a script that acquires
# from EDGAR and parses what it acquired. This script exists solely so that acquisition does not
# have to happen a second time: it places the previous project's already-mirrored files exactly
# where 01A would have written them, so the document renders against real data with its acquisition
# switch off. It is the only file in the 01 family that names the previous project, and it is
# deleted once the migration closes.
#
# WHAT IT COPIES
#   old/GetEDGAR/MasterIndex/        -> 2_output/01A-EdgarIndex/GetEDGAR/MasterIndex/
#   old/GetEDGAR/DocumentLinks/      -> 2_output/01A-EdgarIndex/GetEDGAR/DocumentLinks/
#   old/LandingPage/RawExtract/      -> 2_output/01A-EdgarIndex/Cache/RawExtract/
#   old/LandingPage/LandingPage.pq   -> 2_output/01A-EdgarIndex/LandingPageAll.parquet
#
# It does NOT copy GetEDGAR/DocumentData/ -- 1.77 million files written by 01B, seeded separately.
#
# FOUR SAFETY PROPERTIES
#
# 1. DRY RUN BY DEFAULT. .APPLY is FALSE. The first run reports the plan and changes nothing.
#
# 2. IT REFUSES RATHER THAN OVERWRITES. Any destination holding files aborts the script before
#    anything is written, and aborts entirely rather than skipping the offending row: a partly
#    seeded directory is the state hardest to diagnose later, because it looks populated.
#    An EMPTY destination directory counts as clear, since rendering 01A creates the mirror tree
#    whether or not it has anything to put in it.
#
# 3. IT COPIES, IT DOES NOT MOVE. The previous project is left exactly as it was and remains the
#    fallback until the new one renders end to end.
#
# 4. IT VERIFIES INDEPENDENTLY. Counts are recomputed after cp returns rather than inferred from its
#    exit status. A copy that exits zero having written a truncated tree is the failure this cannot
#    absorb, and counting is cheap beside re-downloading.
#
# WHY cp -c
# Both projects sit on the same APFS volume, so clonefile() makes the copy instantaneous and free:
# the trees share data blocks until one is written to. Hardlinks are equally fast and equally cheap
# but share one inode, so editing either tree would edit both. Clones diverge on write, which for an
# archive that cannot be regenerated is the entire argument.
#
# Run from the project root:  source("1_code/_Migration/01A-CopyFromOld.R")

.APPLY   <- FALSE    # FALSE reports the plan and changes nothing; TRUE performs the copy
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
dir_dst_ <- file.path("2_output", "01A-EdgarIndex")

say_("== Seed 01A ==")
say_("FROM : ", dir_src_, if (dir.exists(dir_src_)) "   [ok]" else "   [MISSING]")
say_("TO   : ", file.path(dir_new_, dir_dst_))
say_("MODE : ", if (isTRUE(.APPLY)) "APPLY -- files will be written" else "DRY RUN -- nothing will be written")

if (!dir.exists(dir_src_)) {
  stop("Previous 01-GetEDGAR output not found. Set .DIR_OLD at the top of this script.")
}


# 3. The plan ----------------------------------------------------------------------------------------------------------
#
# Kind distinguishes a directory clone from a single file: a directory is created by the copy, a
# file needs its parent to exist first.

plan_ <- tibble::tribble(
  ~Label,        ~From,                                            ~To,                                       ~Kind,
  "MasterIndex", file.path("GetEDGAR", "MasterIndex"),
  file.path(dir_dst_, "GetEDGAR", "MasterIndex"),   "dir",
  "DocLinks",    file.path("GetEDGAR", "DocumentLinks"),
  file.path(dir_dst_, "GetEDGAR", "DocumentLinks"), "dir",
  "RawExtract",  file.path("LandingPage", "RawExtract"),
  file.path(dir_dst_, "Cache", "RawExtract"),       "dir",
  "LandingPage", file.path("LandingPage", "LandingPage.parquet"),
  file.path(dir_dst_, "LandingPageAll.parquet"),    "file"
) |>
  dplyr::mutate(
    PathFrom  = file.path(dir_src_, .data$From),
    PathTo    = file.path(dir_new_, .data$To),
    SrcExists = file.exists(.data$PathFrom),
    nFiles    = vapply(.data$PathFrom, walk_count_, integer(1)),
    # An empty destination directory is clear; one holding files is not. See safety property 2.
    nAtDst    = vapply(.data$PathTo, walk_count_, integer(1)),
    Blocked   = .data$nAtDst > 0L
  )

say_("\n-- Plan --")
plan_ |>
  dplyr::mutate(
    Source      = ifelse(.data$SrcExists, "ok", "MISSING"),
    Destination = ifelse(.data$Blocked, "OCCUPIED", "clear"),
    Files       = fmt_(.data$nFiles)
  ) |>
  dplyr::select("Label", "From", "To", "Files", "Source", "Destination") |>
  as.data.frame() |>
  print(row.names = FALSE)


# 4. Refuse on any collision or missing source -------------------------------------------------------------------------

if (any(!plan_$SrcExists)) {
  say_("\nMissing sources: ", paste(plan_$Label[!plan_$SrcExists], collapse = ", "))
  stop("Aborting: at least one source is missing. Nothing was copied.")
}

if (any(plan_$Blocked)) {
  say_("\nOccupied destinations: ", paste(plan_$Label[plan_$Blocked], collapse = ", "))
  stop("Aborting: at least one destination already holds files. Move it aside first. Nothing was copied.")
}


# 5. Volume check ------------------------------------------------------------------------------------------------------

dev_ <- function(.path) {
  out_ <- system2("df", c("-P", "-k", shQuote(.path)), stdout = TRUE)
  strsplit(trimws(out_[2L]), "\\s+")[[1L]][1L]
}

say_("\n-- Volume --")
say_(if (identical(dev_(dir_old_), dev_(dir_new_))) {
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

    # cp -R onto an existing directory nests the source inside it rather than merging, so an empty
    # destination is removed first. recursive = TRUE is required and is safe: the check above already
    # refused anything holding files, but the directory can still contain empty subdirectories --
    # get_directories() creates the whole mirror tree on sight -- and a non-recursive unlink silently
    # fails on those, leaving cp to produce DocumentData/DocumentData.
    if (identical(row_$Kind, "dir") && dir.exists(row_$PathTo)) {
      unlink(row_$PathTo, recursive = TRUE)
    }

    args_ <- if (identical(row_$Kind, "dir")) {
      c("-c", "-R", shQuote(row_$PathFrom), shQuote(row_$PathTo))
    } else {
      c("-c", shQuote(row_$PathFrom), shQuote(row_$PathTo))
    }

    st_ <- system2("cp", args_)
    if (!identical(st_, 0L)) stop("cp failed for ", row_$Label, " with status ", st_)
  }

  # 7. Verify ----------------------------------------------------------------------------------------------------------

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
    stop("File counts do not match. The previous project is untouched; investigate before proceeding.")
  }

  say_("\nAll counts match. The previous project is untouched and remains the fallback.")
  say_("")
  say_("Before rendering, move the seeded landing table aside so the render rebuilds it from the")
  say_("per-quarter cache rather than reusing it. Comparing the two afterwards is what validates")
  say_("the port:")
  say_("")
  say_("  fs::file_move(")
  say_("    here::here('2_output', '01A-EdgarIndex', 'LandingPageAll.parquet'),")
  say_("    here::here('2_output', '01A-EdgarIndex', 'LandingPageAll_seeded.parquet')")
  say_("  )")
}
