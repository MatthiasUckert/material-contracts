# 00-RunPair: run a pair's code without rendering it -------------------------------------------------------------------
#
# WHAT THIS IS
# A debugging harness. It extracts the R code from a .qmd and runs it in the current session, so the
# pair executes exactly as it would under Quarto but four minutes faster, with errors landing in the
# console instead of a log file, and with every object left in the global environment afterwards.
#
# WHY PURL RATHER THAN A HAND-WRITTEN COPY
# The obvious harness restates the configuration and calls the functions in order. That creates a
# second definition of the pipeline which drifts from the document within a day, and then debugging
# happens against something the pipeline does not run. knitr::purl() extracts the chunks from the
# document itself: there is nothing to keep in step because there is only one copy.
#
# WHAT IT DOES NOT TEST
# The render. Figure geometry, cross-references, table rendering and anything that depends on knitr
# being active are exercised only by Quarto. This proves the computation; the driver proves the
# document. Use this to find a fault, then re-run the driver to confirm the fix.
#
# THE SETUP CHUNK IS SKIPPED
# It is marked purl: false, because it resolves the script name from the editor or from knitr, and
# neither is available here. The one thing it defines that the rest needs is .name_script, which is
# set below from .PAIR.
#
# WHAT SURVIVES
# Everything. After this returns, the tables the document built are in the global environment under
# their own names, which is the point: a failed step can be inspected where it failed rather than
# reasoned about from a traceback.
#
# Run from the project root:  source("1_code/_Migration/00-RunPair.R")

.PAIR  <- "01C-EdgarMetaData"   # 01A-EdgarIndex, 01B-EdgarDocuments or 01C-EdgarMetaData
.ECHO  <- TRUE                  # FALSE runs quietly; TRUE prints each expression as it evaluates
.KEEP  <- FALSE                 # TRUE leaves the extracted script on disk for line-by-line stepping


# 1. Resolve -----------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

path_qmd_ <- here::here("1_code", paste0(.PAIR, ".qmd"))
stopifnot("no such pair" = fs::file_exists(path_qmd_))

path_out_ <- if (isTRUE(.KEEP)) {
  fs::dir_create(here::here("2_output", "_Migration"))
  here::here("2_output", "_Migration", paste0(.PAIR, "-purled.R"))
} else {
  tempfile(fileext = ".R")
}

say_("== Run ", .PAIR, " without rendering ==")
say_("  source : ", path_qmd_)
say_("  script : ", path_out_)


# 2. Extract -----------------------------------------------------------------------------------------------------------

knitr::purl(input = path_qmd_, output = path_out_, documentation = 0L, quiet = TRUE)

n_lines_ <- length(readLines(path_out_, warn = FALSE))
say_("  ", n_lines_, " lines of R extracted")


# 3. Run ---------------------------------------------------------------------------------------------------------------
#
# .name_script stands in for what the skipped setup chunk would have resolved. knitr.in.progress is
# left unset, so any chunk branching on it takes the interactive path, which is what running here
# means.

.name_script <- .PAIR

options(cli.num_colors = 1, cli.width = 120)

t0_ <- Sys.time()
say_("\n-- Running --\n")

source(file = path_out_, echo = .ECHO, max.deparse.length = 500L, encoding = "UTF-8")

say_("\n-- Done in ", round(as.numeric(difftime(Sys.time(), t0_, units = "mins")), 1), " minutes --")
say_("Objects the document built are in the global environment.")
if (isTRUE(.KEEP)) say_("Extracted script kept at ", path_out_)
