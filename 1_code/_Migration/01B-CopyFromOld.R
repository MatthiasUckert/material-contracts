# 01B-CopyFromOld: seed the document corpus from the previous project ---------------------------------------------------
#
# WHAT THIS IS
# Scaffolding, not pipeline. 01B-EdgarDocuments.qmd is written as what it is -- a script that fetches
# documents from EDGAR and indexes them. This script exists solely so that fetching does not have to
# happen a second time: it places the previous project's already-downloaded corpus exactly where 01B
# would have written it, so the document renders against real data with its acquisition switch off.
# It is deleted once the migration closes.
#
# WHAT IT COPIES
#   old/GetEDGAR/DocumentData/    -> 2_output/01B-EdgarDocuments/GetEDGAR/DocumentData/
#   old/Output/FilePaths.parquet  -> 2_output/01B-EdgarDocuments/FilePaths.parquet
#
# DocumentData/ is taken whole rather than just its Parsed/ subtree, because it also holds the parse
# ledger written during acquisition, which records which documents failed and why. 01C reports on
# that, and separating the ledger from the documents it describes leaves both harder to interpret.
#
# Both destinations are inside 01B's own directory. 01B keeps its own mirror root and stages the link
# tables into it, so the corpus it downloads belongs there rather than in the directory of the script
# that produced the links.
#
# THIS IS THE IRREPLACEABLE ONE
# The corpus was fetched with .keep_orig = FALSE, so the original bytes are gone and there is no
# local source to re-parse from. Re-deriving it means one request per document at ten per second:
# days of wall time, assuming every link still resolves, and a measurable share no longer does.
# Everything below is built around not damaging it.
#
# FOUR SAFETY PROPERTIES
#
# 1. DRY RUN BY DEFAULT. .APPLY is FALSE. The first run reports the plan and changes nothing.
#
# 2. IT REFUSES RATHER THAN OVERWRITES, and aborts entirely rather than skipping the offending row:
#    a partly seeded tree is the state hardest to diagnose later, because it looks populated. An
#    EMPTY destination directory counts as clear, since rendering 01A creates the mirror tree
#    including an empty DocumentData/ whether or not anything has been put in it.
#
# 3. IT COPIES, IT DOES NOT MOVE. The previous project is left exactly as it was.
#
# 4. IT VERIFIES INDEPENDENTLY, and counts the Parsed/ subtree separately from the whole. A copy
#    that truncated one branch but happened to total correctly would pass a single aggregate check.
#
# WHY cp -c
# Same APFS volume, so clonefile() makes this instantaneous and free: the trees share data blocks
# until one is written to. Hardlinks are equally fast and cheap but share one inode, so editing
# either tree would edit both. Clones diverge on write, which for a corpus that cannot be
# regenerated is the entire argument.
#
# Run from the project root:  source("1_code/_Migration/01B-CopyFromOld.R")

.APPLY   <- FALSE    # FALSE reports the plan and changes nothing; TRUE performs the copy
.DIR_OLD <- NULL     # set to a path to override the sibling-project autodetect


# 1. Tools -------------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

# Files below a directory, counted without collecting their paths. fs::dir_info() and
# list.files(recursive = TRUE) both allocate in proportion to the number of files and take the R
# session down on a tree of this size; a breadth-first walk holds one directory at a time.
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
dir_01b_ <- file.path("2_output", "01B-EdgarDocuments")
dir_mir_ <- file.path(dir_01b_, "GetEDGAR")

say_("== Seed 01B ==")
say_("FROM : ", dir_src_, if (dir.exists(dir_src_)) "   [ok]" else "   [MISSING]")
say_("TO   : ", file.path(dir_new_, dir_01b_))
say_("MODE : ", if (isTRUE(.APPLY)) "APPLY -- files will be written" else "DRY RUN -- nothing will be written")

if (!dir.exists(dir_src_)) {
  stop("Previous 01-GetEDGAR output not found. Set .DIR_OLD at the top of this script.")
}
if (!dir.exists(file.path(dir_new_, "2_output", "01A-EdgarIndex", "GetEDGAR"))) {
  stop("01A's mirror does not exist yet. Seed and render 01A before seeding 01B.")
}


# 3. The plan ----------------------------------------------------------------------------------------------------------
#
# Counting the source walks roughly 1.8 million files and takes a minute or two. It is done anyway,
# because the count is the baseline the copy is verified against, and a verification with no
# baseline verifies nothing.

say_("\nCounting the source tree -- this takes a minute on 1.8 million files ...")

plan_ <- tibble::tribble(
  ~Label,         ~From,                                     ~To,                                      ~Kind,
  "DocumentData", file.path("GetEDGAR", "DocumentData"),     file.path(dir_mir_, "DocumentData"),      "dir",
  "FilePaths",    file.path("Output", "FilePaths.parquet"),  file.path(dir_01b_, "FilePaths.parquet"), "file"
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

# The parsed subtree, counted separately. It is the part that matters and the part whose loss would
# be least visible in a whole-tree total.
n_parsed_src_ <- walk_count_(file.path(dir_src_, "GetEDGAR", "DocumentData", "Parsed"))

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

say_("\n  of which Parsed/: ", fmt_(n_parsed_src_), " documents")


# 4. Refuse on any collision or missing source -------------------------------------------------------------------------

if (any(!plan_$SrcExists)) {
  say_("\nMissing sources: ", paste(plan_$Label[!plan_$SrcExists], collapse = ", "))
  stop("Aborting: at least one source is missing. Nothing was copied.")
}

if (any(plan_$Blocked)) {
  say_("\nOccupied destinations: ", paste(plan_$Label[plan_$Blocked], collapse = ", "))
  stop("Aborting: at least one destination already holds files. Move it aside first. Nothing was copied.")
}


# 5. Volume and space --------------------------------------------------------------------------------------------------

dev_ <- function(.path) {
  out_ <- system2("df", c("-P", "-k", shQuote(.path)), stdout = TRUE)
  strsplit(trimws(out_[2L]), "\\s+")[[1L]]
}
same_vol_ <- identical(dev_(dir_old_)[1L], dev_(dir_new_)[1L])

say_("\n-- Volume --")
if (same_vol_) {
  say_("Same volume: cp -c clones instantly and uses no extra space.")
} else {
  say_("Different volumes: this will be a full byte copy of ", fmt_(sum(plan_$nFiles)), " files.")
  say_("Free on destination: ", round(as.numeric(dev_(dir_new_)[4L]) / 1048576), " GB")
}


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

  n_parsed_dst_ <- walk_count_(file.path(dir_new_, dir_mir_, "DocumentData", "Parsed"))
  ver_ <- dplyr::bind_rows(ver_, tibble::tibble(
    Label   = "  of which Parsed",
    nBefore = n_parsed_src_,
    nAfter  = n_parsed_dst_,
    Match   = ifelse(n_parsed_src_ == n_parsed_dst_, "ok", "MISMATCH")
  ))

  print(as.data.frame(ver_), row.names = FALSE)

  if (any(ver_$Match != "ok")) {
    stop("File counts do not match. The previous project is untouched; investigate before proceeding.")
  }

  say_("\nAll counts match. The previous project is untouched and remains the fallback.")
  say_("Next: source 01B-VerifyMigration.R with .STAGE <- 'before', then render 01B.")
}
