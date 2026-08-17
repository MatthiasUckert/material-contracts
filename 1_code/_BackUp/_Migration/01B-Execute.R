# 01B-Execute: reset, seed, render and verify 01B in one run ------------------------------------------------------------
#
# WHAT THIS IS
# The migration driver for 01B. Same shape as 01A's: it takes the stage from nothing to verified
# without a manual step in between, and ends by printing one compact digest.
#
# WHAT IT DOES
#   1. RESET      delete 01B's output directory, its migration stash, and the corpus branch
#   2. SEED       01B-CopyFromOld.R with .APPLY = TRUE
#   3. BEFORE     01B-VerifyMigration.R with .STAGE = "before"
#   4. RENDER     quarto render 1_code/01B-EdgarDocuments.qmd, output captured to a log
#   5. AFTER      01B-VerifyMigration.R with .STAGE = "after"
#   6. DIGEST     one screen of numbers
#
# WHY THE RESET REACHES INTO 01A'S DIRECTORY
# rGetEDGAR resolves link tables and downloaded documents from a single root, so the corpus lives at
# 01A-EdgarIndex/GetEDGAR/DocumentData -- 01B writes that branch of a tree 01A established. Reseeding
# therefore requires clearing it, and the seeding script refuses to write into a directory holding
# files. Nothing else under 01A is touched: MasterIndex and DocumentLinks belong to 01A, are read
# here and never written, and stay exactly as 01A left them.
#
# THIS RESET IS SLOW AND THE COPY IS NOT
# Deleting roughly 1.8 million files takes minutes. Cloning them back takes seconds, because both
# projects sit on one APFS volume and cp -c shares data blocks rather than moving bytes. The
# asymmetry is worth knowing before assuming the run has hung.
#
# 01A MUST BE DONE FIRST
# The seeding script checks for 01A's mirror and stops if it is absent, so ordering is enforced
# rather than remembered.
#
# Run from the project root:  source("1_code/_Migration/01B-Execute.R")

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
dir_stash_ <- file.path(dir_root_, "2_output", "_Migration", "01B-EdgarDocuments")
dir_docs_  <- file.path(dir_01a_, "GetEDGAR", "DocumentData")
path_qmd_  <- file.path(dir_root_, "1_code", "01B-EdgarDocuments.qmd")
path_log_  <- file.path(dir_root_, "2_output", "_Migration", "01B-render.log")

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

if (isTRUE(.RESET)) {
  for (d_ in c(dir_01b_, dir_stash_, dir_docs_)) {
    if (dir.exists(d_)) {
      say_("  deleting ", sub(dir_root_, ".", d_, fixed = TRUE), " -- this can take minutes")
      unlink(d_, recursive = TRUE)
    } else {
      say_("  absent   ", sub(dir_root_, ".", d_, fixed = TRUE))
    }
  }
  if (file.exists(path_log_)) unlink(path_log_)
  say_("\nClean slate. 01A's MasterIndex and DocumentLinks were not touched.")
} else {
  say_("  .RESET is FALSE -- resuming against whatever is already present.")
}


# 3. Seed --------------------------------------------------------------------------------------------------------------

rule_("2. SEED")

step_("seed", run_with_(
  .path = file.path(dir_mig_, "01B-CopyFromOld.R"),
  .set  = list(.APPLY = TRUE, .DIR_OLD = .DIR_OLD)
))


# 4. Move the reference index aside ------------------------------------------------------------------------------------

rule_("3. BEFORE -- move the reference index out of the way")

step_("before", run_with_(
  .path = file.path(dir_mig_, "01B-VerifyMigration.R"),
  .set  = list(.STAGE = "before")
))


# 5. Render ------------------------------------------------------------------------------------------------------------

rule_("4. RENDER")

say_("  quarto render 1_code/01B-EdgarDocuments.qmd")
say_("  Output goes to 2_output/_Migration/01B-render.log")
say_("  The tree walk runs for real with no index present. This step is silent while it runs.")

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
  stop("Render failed. Nothing further was run; the reference index is intact in the stash.")
}

say_("  render ok")


# 6. Compare against the reference -------------------------------------------------------------------------------------

rule_("5. AFTER -- compare rebuilt against reference")

step_("after", run_with_(
  .path = file.path(dir_mig_, "01B-VerifyMigration.R"),
  .set  = list(.STAGE = "after")
))


# 7. Digest ------------------------------------------------------------------------------------------------------------
#
# Recomputed from disk rather than carried out of the render, which ran in its own process. The
# selection is rebuilt here because it is the substantive decision this stage makes, and reading it
# back off the index would only confirm the index against itself.

rule_("6. DIGEST")

source(file.path(dir_root_, "1_code", "_Commons", "_Utils.R"), encoding = "UTF-8")
source(file.path(dir_root_, "1_code", "01A-EdgarIndex.R"), encoding = "UTF-8")
source(file.path(dir_root_, "1_code", "01B-EdgarDocuments.R"), encoding = "UTF-8")

lp_edgar_ <- rGetEDGAR::get_directories(file.path(dir_01a_, "GetEDGAR"))
path_land_ <- file.path(dir_01a_, "LandingPageAll.parquet")

tab_idx_ <- arrow::read_parquet(file.path(dir_01b_, "FilePaths.parquet"))

say_("\n-- Rebuilding the selection --")
sel_ <- dplyr::bind_rows(
  dplyr::mutate(edg_links_with_ext(lp_edgar_$DocLinks$DirMain$Links, edg_doc_types("Exhibit 10")), Group = "Exhibit10"),
  dplyr::mutate(edg_links_with_ext(lp_edgar_$DocLinks$DirMain$Links, edg_doc_types("CTO")), Group = "CTO"),
  dplyr::mutate(
    edg_links_with_ext(
      .path_links = lp_edgar_$DocLinks$DirMain$Links,
      .types      = edg_doc_types(c("8-K", "8-K/A")),
      .hash_index = edg_hash_with_item(path_land_, "1.01", .fixed = TRUE)
    ),
    Group = "8-K"
  )
)
say_("  selected: ", fmt_(nrow(sel_)), " documents")

say_("\n-- Selected against retrieved --")
print(as.data.frame(edg_outstanding(.tab_sel = sel_, .ids_disk = tab_idx_$DocID)), row.names = FALSE)

say_("\n-- Retrieved corpus --")
print(as.data.frame(edg_corpus_composition(.tab_idx = tab_idx_)), row.names = FALSE)

say_("\n-- Index checks --")
print(data.frame(
  Quantity = c("rows", "distinct DocID", "indexed but not selected"),
  N        = c(nrow(tab_idx_), dplyr::n_distinct(tab_idx_$DocID), sum(!tab_idx_$DocID %in% sel_$DocID)),
  row.names = NULL
), row.names = FALSE)

say_("\n-- Artifacts on disk --")
fil_ <- fs::dir_ls(dir_01b_, recurse = FALSE)
print(data.frame(
  Item = sub(paste0(dir_01b_, "/"), "", fil_, fixed = TRUE),
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
