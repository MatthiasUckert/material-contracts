# 01A-Execute: reset, seed, render and verify 01A in one run ------------------------------------------------------------
#
# WHAT THIS IS
# The migration driver for 01A. It takes the project from nothing to a verified 01A without any
# manual step between, and ends by printing one compact digest -- which is the only thing that needs
# to be read or pasted anywhere.
#
# It is scaffolding like the three scripts it calls, and is deleted with them.
#
# WHAT IT DOES
#   1. RESET      delete 01A's output directory and its migration stash
#   2. SEED       01A-CopyFromOld.R with .APPLY = TRUE
#   3. BEFORE     01A-VerifyMigration.R with .STAGE = "before"
#   4. RENDER     quarto render 1_code/01A-EdgarIndex.qmd, output captured to a log
#   5. AFTER      01A-VerifyMigration.R with .STAGE = "after"
#   6. DIGEST     one screen of numbers
#
# RESET IS DESTRUCTIVE AND DELIBERATE
# Step 1 deletes 2_output/01A-EdgarIndex/ outright. That is safe only because everything in it is
# either a clone of the previous project or reproducible from one, and it is what makes the run
# meaningful: a partially populated directory would let a step appear to succeed by finding output
# an earlier attempt left behind. Nothing outside those two directories is touched.
#
# HOW THE SUB-SCRIPTS ARE DRIVEN
# Each sets its own flag at the top, so a value assigned here would be overwritten when the file is
# sourced. The flag line is therefore rewritten in memory before evaluation, and the rewrite asserts
# that it matched exactly one line: if a script is edited and the flag moves or is renamed, this
# fails loudly rather than silently running with the wrong setting.
#
# Run from the project root:  source("1_code/_Migration/01A-Execute.R")

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
# text rather than assigned beforehand, because the script assigns it itself on the way past.
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

# Quarto ships with RStudio but is not always on the PATH a non-interactive R session inherits.
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
dir_stash_ <- file.path(dir_root_, "2_output", "_Migration", "01A-EdgarIndex")
path_qmd_  <- file.path(dir_root_, "1_code", "01A-EdgarIndex.qmd")
path_log_  <- file.path(dir_root_, "2_output", "_Migration", "01A-render.log")

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

if (isTRUE(.RESET)) {
  for (d_ in c(dir_01a_, dir_stash_)) {
    if (dir.exists(d_)) {
      say_("  deleting ", sub(dir_root_, ".", d_, fixed = TRUE))
      unlink(d_, recursive = TRUE)
    } else {
      say_("  absent   ", sub(dir_root_, ".", d_, fixed = TRUE))
    }
  }
  if (file.exists(path_log_)) unlink(path_log_)
  say_("\nClean slate. Nothing outside these two directories was touched.")
} else {
  say_("  .RESET is FALSE -- resuming against whatever is already present.")
}


# 3. Seed --------------------------------------------------------------------------------------------------------------

rule_("2. SEED")

step_("seed", run_with_(
  .path = file.path(dir_mig_, "01A-CopyFromOld.R"),
  .set  = list(.APPLY = TRUE, .DIR_OLD = .DIR_OLD)
))


# 4. Move the reference copies aside -----------------------------------------------------------------------------------

rule_("3. BEFORE -- move reference copies out of the way")

step_("before", run_with_(
  .path = file.path(dir_mig_, "01A-VerifyMigration.R"),
  .set  = list(.STAGE = "before")
))


# 5. Render ------------------------------------------------------------------------------------------------------------
#
# Captured to a log rather than streamed. The render prints one line per year-quarter and the
# quarter being re-parsed runs for minutes, so the useful content is a handful of lines inside
# several hundred. The digest pulls those out; the whole log stays on disk if more is needed.

rule_("4. RENDER")

say_("  quarto render 1_code/01A-EdgarIndex.qmd")
say_("  Output goes to 2_output/_Migration/01A-render.log")
say_("  The re-parsed quarter takes several minutes. This step is silent while it runs.")

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
  stop("Render failed. Nothing further was run; the reference copies are intact in the stash.")
}

say_("  render ok")


# 6. Compare against the reference copies ------------------------------------------------------------------------------

rule_("5. AFTER -- compare rebuilt against reference")

step_("after", run_with_(
  .path = file.path(dir_mig_, "01A-VerifyMigration.R"),
  .set  = list(.STAGE = "after")
))


# 7. Digest ------------------------------------------------------------------------------------------------------------
#
# Everything below is recomputed from what is on disk rather than carried out of the render, because
# the render ran in its own process. That is the stricter test anyway: it reads the published
# artifact rather than an object that happened to survive in memory.

rule_("6. DIGEST")

source(file.path(dir_root_, "1_code", "_Commons", "_Utils.R"), encoding = "UTF-8")
source(file.path(dir_root_, "1_code", "01A-EdgarIndex.R"), encoding = "UTF-8")

lp_edgar_ <- rGetEDGAR::get_directories(file.path(dir_01a_, "GetEDGAR"))
tab_land_ <- arrow::read_parquet(file.path(dir_01a_, "LandingPageAll.parquet"))

# The re-parsed quarter is not reported here. Quarto captures chunk messages into the document
# rather than onto standard output, so the parse log is not in the render log; the comparison in
# step 5 establishes that the quarter was rebuilt, which is the thing that mattered.

say_("\n-- Frame --")
vec_frame_ <- edg_select_index(
  .dir_master = lp_edgar_$MasterIndex$DirParquet,
  .forms      = edg_sec_forms(),
  .max_year   = 2024L
)
say_("  filings selected: ", fmt_(length(vec_frame_)))

say_("\n-- Grain and frame --")
print(as.data.frame(edg_landing_grain(.tab = tab_land_, .frame = vec_frame_)), row.names = FALSE)

say_("\n-- Form-type composition, top 10 by filings --")
print(as.data.frame(utils::head(edg_landing_forms(.tab = tab_land_), 10L)), row.names = FALSE)

say_("\n-- Coverage by year, first and last five --")
cov_ <- edg_landing_coverage(.tab = tab_land_)
print(as.data.frame(utils::head(cov_, 5L)), row.names = FALSE)
say_("  ...")
print(as.data.frame(utils::tail(cov_, 5L)), row.names = FALSE)

say_("\n-- Artifacts on disk --")
fil_ <- fs::dir_ls(dir_01a_, recurse = FALSE)
print(data.frame(
  Item = sub(paste0(dir_01a_, "/"), "", fil_, fixed = TRUE),
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
