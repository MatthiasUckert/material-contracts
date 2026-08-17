# 01C-Execute: reset, seed, render and verify 01C in one run ------------------------------------------------------------
#
# WHAT THIS IS
# The migration driver for 01C, and the last of the three. Same shape as 01A's and 01B's.
#
# WHAT IT DOES
#   1. RESET      delete 01C's output directory and its migration stash
#   2. SEED       01C-CopyFromOld.R with .APPLY = TRUE
#   3. BEFORE     01C-VerifyMigration.R with .STAGE = "before"
#   4. RENDER     quarto render 1_code/01C-EdgarMetaData.qmd, output captured to a log
#   5. AFTER      01C-VerifyMigration.R with .STAGE = "after"
#   6. DIGEST     one screen of numbers
#
# THE RESET TOUCHES NOTHING OUTSIDE 01C
# 01C writes only inside its own directory. It reads 01A's link tables, master index and landing
# table, and 01B's document index, and writes none of them.
#
# THE MEASUREMENT CACHES ARE SEEDED, AND THAT IS THE POINT
# The three passes over the corpus open every one of 1.8 million documents and take hours. Seeding
# them means the render exercises everything built on top -- the consolidation, the quality rules,
# the three restrictions, the repeated-document flags -- in minutes. Those are where a rewrite
# changes an answer; the passes themselves are per-document arithmetic that was ported unchanged.
#
# To exercise a pass for real, delete one cache from 01C-EdgarMetaData/Cache/ before rendering. The
# render recomputes it in full, and the comparison covers the result either way.
#
# 01A AND 01B MUST BE DONE FIRST
# The seeding script and the document both stop if their inputs are missing, so ordering is enforced
# rather than remembered.
#
# Run from the project root:  source("1_code/_Migration/01C-Execute.R")

.RESET   <- TRUE     # FALSE skips the delete and resumes; TRUE is the intended mode
.DIR_OLD <- NULL     # set to a path to override the sibling-project autodetect


# 1. Tools -------------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

rule_ <- function(.text) {
  say_("\n", strrep("=", 118))
  say_("  ", .text)
  say_(strrep("=", 118))
}

# Source a script with one or more of its top-level flags overridden. The flag is rewritten in the
# text rather than assigned beforehand, because the script assigns it itself on the way past. The
# rewrite asserts a unique match, so a renamed or moved flag fails loudly.
run_with_ <- function(.path, .set) {
  txt_ <- readLines(.path, warn = FALSE)

  for (nm_ in names(.set)) {
    hit_ <- grep(paste0("^\\", nm_, "[[:space:]]*<-"), txt_)
    if (length(hit_) != 1L) {
      stop("Expected exactly one assignment of ", nm_, " in ", basename(.path), ", found ", length(hit_))
    }
    txt_[hit_] <- paste0(nm_, " <- ", deparse(.set[[nm_]]))
  }

  eval(parse(text = txt_), envir = globalenv())
  invisible(NULL)
}

find_quarto_ <- function() {
  cand_ <- c(
    Sys.which("quarto"),
    Sys.getenv("QUARTO_PATH", ""),
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
    "/usr/local/bin/quarto",
    "/opt/homebrew/bin/quarto"
  )
  cand_ <- cand_[nzchar(cand_) & file.exists(cand_)]
  if (length(cand_) == 0L) stop("Could not locate the quarto binary. Set QUARTO_PATH and re-run.")
  unname(cand_[1L])
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

dir_root_  <- here::here()
dir_mig_   <- file.path(dir_root_, "1_code", "_Migration")
dir_01a_   <- file.path(dir_root_, "2_output", "01A-EdgarIndex")
dir_01b_   <- file.path(dir_root_, "2_output", "01B-EdgarDocuments")
dir_01c_   <- file.path(dir_root_, "2_output", "01C-EdgarMetaData")
dir_stash_ <- file.path(dir_root_, "2_output", "_Migration", "01C-EdgarMetaData")
path_qmd_  <- file.path(dir_root_, "1_code", "01C-EdgarMetaData.qmd")
path_log_  <- file.path(dir_root_, "2_output", "_Migration", "01C-render.log")

t_start_ <- Sys.time()
timing_  <- tibble::tibble(Step = character(0), Seconds = numeric(0))

step_ <- function(.name, .expr) {
  t0_ <- Sys.time()
  force(.expr)
  s_  <- round(as.numeric(difftime(Sys.time(), t0_, units = "secs")), 1)
  timing_ <<- dplyr::bind_rows(timing_, tibble::tibble(Step = .name, Seconds = s_))
  say_("\n[", .name, " took ", s_, "s]")
}


# 2. Reset -------------------------------------------------------------------------------------------------------------

rule_("1. RESET")

if (!dir.exists(file.path(dir_01a_, "GetEDGAR", "MasterIndex"))) {
  stop("01A's mirror is missing. Run 01A-Execute.R before this.")
}
if (!file.exists(file.path(dir_01b_, "FilePaths.parquet"))) {
  stop("01B's document index is missing. Run 01B-Execute.R before this.")
}

if (isTRUE(.RESET)) {
  for (d_ in c(dir_01c_, dir_stash_)) {
    if (dir.exists(d_)) {
      say_("  deleting ", sub(dir_root_, ".", d_, fixed = TRUE))
      unlink(d_, recursive = TRUE)
    } else {
      say_("  absent   ", sub(dir_root_, ".", d_, fixed = TRUE))
    }
  }
  if (file.exists(path_log_)) unlink(path_log_)
  say_("\nClean slate. Nothing outside 01C's own directory was touched.")
} else {
  say_("  .RESET is FALSE -- resuming against whatever is already present.")
}


# 3. Seed --------------------------------------------------------------------------------------------------------------

rule_("2. SEED")

step_("seed", run_with_(
  .path = file.path(dir_mig_, "01C-CopyFromOld.R"),
  .set  = list(.APPLY = TRUE, .DIR_OLD = .DIR_OLD)
))


# 4. Move the reference outputs aside ----------------------------------------------------------------------------------

rule_("3. BEFORE -- move the reference outputs out of the way")

step_("before", run_with_(
  .path = file.path(dir_mig_, "01C-VerifyMigration.R"),
  .set  = list(.STAGE = "before")
))


# 5. Render ------------------------------------------------------------------------------------------------------------

rule_("4. RENDER")

say_("  quarto render 1_code/01C-EdgarMetaData.qmd")
say_("  Output goes to 2_output/_Migration/01C-render.log")
say_("  The three measurement caches are seeded, so this should be minutes rather than hours.")

quarto_ <- find_quarto_()
fs::dir_create(dirname(path_log_))

step_("render", {
  wd_ <- setwd(dir_root_)
  on.exit(setwd(wd_), add = TRUE)
  status_ <<- system2(
    command = quarto_,
    args    = c("render", shQuote(path_qmd_)),
    stdout  = path_log_,
    stderr  = path_log_
  )
})

log_ <- if (file.exists(path_log_)) readLines(path_log_, warn = FALSE) else character(0)

if (!identical(status_, 0L)) {
  say_("\nRENDER FAILED with status ", status_, ". Last 40 lines of the log:\n")
  cat(utils::tail(log_, 40L), sep = "\n")
  stop("Render failed. Nothing further was run; the reference outputs are intact in the stash.")
}

say_("  render ok")


# 6. Compare against the reference outputs -----------------------------------------------------------------------------

rule_("5. AFTER -- compare rebuilt against reference")

step_("after", run_with_(
  .path = file.path(dir_mig_, "01C-VerifyMigration.R"),
  .set  = list(.STAGE = "after")
))


# 7. Digest ------------------------------------------------------------------------------------------------------------
#
# Read from the published table rather than carried out of the render, which ran in its own process.

rule_("6. DIGEST")

source(file.path(dir_root_, "1_code", "_Commons", "_Utils.R"), encoding = "UTF-8")
source(file.path(dir_root_, "1_code", "01C-EdgarMetaData.R"), encoding = "UTF-8")

meta_ <- arrow::read_parquet(file.path(dir_01c_, "FullMetaData.parquet"))
rem_  <- arrow::read_parquet(file.path(dir_01c_, "RemovedDocs.parquet"))
idx_  <- arrow::read_parquet(file.path(dir_01b_, "FilePaths.parquet"))

say_("\n-- FullMetaData --")
print(data.frame(
  Quantity = c("rows", "columns", "distinct DocID", "flagged Removed", "distinct attachments", "primaries"),
  N        = c(nrow(meta_), ncol(meta_), dplyr::n_distinct(meta_$DocID), sum(meta_$Removed),
               dplyr::n_distinct(meta_$HashDocument), sum(meta_$PrimaryFiler)),
  row.names = NULL
), row.names = FALSE)

say_("\n-- Quality rules --")
print(as.data.frame(edg_removal_summary(.tab_rem = rem_, .tab_idx = idx_)), row.names = FALSE)

say_("\n-- Attachments fetched under several registrants --")
print(as.data.frame(edg_filer_summary(.tab = meta_)), row.names = FALSE)

say_("\n-- Registrants per attachment, first ten --")
print(as.data.frame(utils::head(edg_filer_distribution(.tab = meta_), 10L)), row.names = FALSE)

say_("\n-- Columns added since the previous version --")
ref_ <- file.path(dir_stash_, "FullMetaData_reference.parquet")
if (file.exists(ref_)) {
  say_("  ", paste(setdiff(names(meta_), names(arrow::open_dataset(ref_))), collapse = ", "))
} else {
  say_("  (no reference available)")
}

say_("\n-- Artifacts on disk --")
fil_ <- fs::dir_ls(dir_01c_, recurse = FALSE)
print(data.frame(
  Item = sub(paste0(dir_01c_, "/"), "", fil_, fixed = TRUE),
  Size = vapply(fil_, \(.p) {
    if (fs::is_dir(.p)) paste0(length(fs::dir_ls(.p, recurse = TRUE, type = "file")), " files")
    else format(structure(fs::file_size(.p), class = "object_size"), units = "auto")
  }, character(1)),
  row.names = NULL
), row.names = FALSE)

say_("\n-- Timings --")
print(as.data.frame(timing_), row.names = FALSE)
say_("\nTotal: ", round(as.numeric(difftime(Sys.time(), t_start_, units = "mins")), 1), " minutes")

rule_("END")
say_("Paste everything from '5. AFTER' downwards.")
