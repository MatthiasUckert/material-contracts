# restore-02-libraries.R -- put the function libraries of 02A and 02B back --------------------------------------------
#
# Since commit 1b19197 (10 Sep) both 1_code/02A-Compustat.R and 1_code/02B-Register.R hold the text of a Final
# Exhibits runbook instead of their functions. The newest versions that still parse are 5488d82 (02A, 19 Aug) and
# 8898545 (02B, 1 Sep). Anything changed in either library between those dates and 10 Sep was never committed in
# working form, so before restoring, this script checks whether the old libraries still fit the current runbooks.
#
# HOW TO RUN (material-contracts project open)
#   1. Source it as it is. DO_RESTORE is FALSE: it only reads, and writes one report,
#      ~/Downloads/restore-02-libraries.txt. Attach that file in the chat.
#   2. Only if the report ends with "SAFE TO RESTORE" (or after we agreed on it): set DO_RESTORE <- TRUE and source
#      it again. It then writes the two files from git -- nothing is committed -- and shows the result.
#
# WHAT IT CHECKS
#   A. which other files commit 1b19197 changed, and what its message says
#   B. every function the current 02A/02B runbooks call without a namespace, against every function defined in the
#      candidate library, the commons, and the other libraries -- a call nothing defines would fail at render
#   C. whether the runbooks changed after the candidate's date, and when 02A/02B last wrote output
#   D. other copies of the two libraries: elsewhere in git history, in the backup folders, and in RStudio's store
#      of open documents, where an unsaved or newer version may survive

DO_RESTORE <- TRUE

candidates <- tibble::tribble(
  ~Library,                  ~Runbook,                    ~Commit,   ~Header,
  "1_code/02A-Compustat.R",  "1_code/02A-Compustat.qmd",  "5488d82", "# 02A-Compustat:",
  "1_code/02B-Register.R",   "1_code/02B-Register.qmd",   "8898545", "# 02B-Register:"
)
broken_commit <- "1b19197"


# 1. Helpers ----------------------------------------------------------------------------------------------------------

rst_git <- function(.args, .stdout = TRUE) {
  suppressWarnings(system2(
    command = "git",
    args    = c("-C", shQuote(here::here()), "-c", "core.quotepath=off", shQuote(.args)),
    stdout  = .stdout,
    stderr  = FALSE
  ))
}

# The R code of a runbook: the lines inside its R chunks, cut out by pattern.
rst_qmd_code <- function(.lines) {
  open_ <- grepl("^\\s*```+\\s*\\{r([\\s,}].*)?$", .lines, perl = TRUE)
  fence_ <- grepl("^\\s*```+\\s*$", .lines, perl = TRUE)
  inside_ <- logical(length(.lines))
  state_ <- FALSE
  for (i_ in seq_along(.lines)) {
    if (!state_ && open_[i_]) {
      state_ <- TRUE
    } else if (state_ && fence_[i_]) {
      state_ <- FALSE
    } else if (state_) {
      inside_[i_] <- TRUE
    }
  }
  .lines[inside_]
}

# Functions assigned at the top level of some code: name <- function(...) or name = function(...).
rst_defined <- function(.code) {
  exprs_ <- tryCatch(parse(text = .code, keep.source = FALSE), error = function(.e) expression())
  out_ <- character(0)
  for (e_ in exprs_) {
    is_assign_ <- is.call(e_) && (identical(e_[[1]], as.name("<-")) || identical(e_[[1]], as.name("=")))
    if (is_assign_ && length(e_) == 3L && is.name(e_[[2]]) &&
        is.call(e_[[3]]) && identical(e_[[3]][[1]], as.name("function"))) {
      out_ <- c(out_, as.character(e_[[2]]))
    }
  }
  unique(out_)
}

# Functions called without a namespace prefix.
rst_called <- function(.code) {
  exprs_ <- tryCatch(parse(text = .code, keep.source = TRUE), error = function(.e) NULL)
  if (is.null(exprs_)) return(character(0))
  pd_ <- utils::getParseData(exprs_)
  pd_ <- pd_[pd_$terminal, ]
  pd_ <- pd_[order(pd_$line1, pd_$col1), ]
  prev_ <- c("", utils::head(pd_$token, -1L))
  calls_ <- pd_$text[pd_$token == "SYMBOL_FUNCTION_CALL" & !prev_ %in% c("NS_GET", "NS_GET_INT", "'$'", "'@'")]
  unique(calls_[!startsWith(calls_, ".")])
}

rst_is_base <- function(.names) {
  pkgs_ <- c("base", "utils", "stats", "tools", "methods", "graphics", "grDevices")
  exported_ <- unique(unlist(lapply(pkgs_, getNamespaceExports)))
  .names %in% exported_ | vapply(.names, exists, logical(1), envir = baseenv(), inherits = FALSE)
}

rst_parses <- function(.code) {
  tryCatch(
    expr = {
      parse(text = .code, keep.source = FALSE)
      TRUE
    },
    error = function(.e) FALSE
  )
}


# 2. Report -----------------------------------------------------------------------------------------------------------

path_report <- fs::path_expand("~/Downloads/restore-02-libraries.txt")
fs::dir_create(fs::path_dir(path_report))
while (sink.number() > 0L) sink()  # a run that stopped half-way leaves its sink open
sink(file = path_report, split = TRUE)
cat("restore-02-libraries.R |", format(Sys.time(), "%Y-%m-%d %H:%M"), "| DO_RESTORE =", DO_RESTORE, "\n")

cat("\n######## A. The commit that broke both files ########\n")
cat(rst_git(c("show", "--stat", "--date=short", "--format=%h %cd %s", broken_commit)), sep = "\n")

# Definitions available besides the candidate: the commons and every other library in the working tree.
others <- c(
  fs::dir_ls(path = here::here("1_code", "_Commons"), glob = "*.R"),
  fs::dir_ls(path = here::here("1_code"), glob = "*.R", recurse = FALSE)
)
others <- others[!fs::path_file(others) %in% fs::path_file(candidates$Library)]
defined_elsewhere <- unique(unlist(lapply(others, \(.f) rst_defined(readLines(.f, warn = FALSE)))))

verdict <- character(0)
for (i in seq_len(nrow(candidates))) {
  cand <- candidates[i, ]
  cat("\n######## B.", cand$Library, "at", cand$Commit, "########\n")
  lib_code <- rst_git(c("show", paste0(cand$Commit, ":", cand$Library)))
  qmd_code <- rst_qmd_code(readLines(here::here(cand$Runbook), warn = FALSE))
  cat("candidate lines:", length(lib_code), "| parses:", rst_parses(lib_code), "\n")

  defined_lib <- rst_defined(lib_code)
  called <- rst_called(qmd_code)
  known <- c(defined_lib, defined_elsewhere, rst_defined(qmd_code))
  missing <- sort(called[!called %in% known & !rst_is_base(called)])
  cat("functions defined by the candidate:", length(defined_lib), "\n")
  cat(paste(sort(defined_lib), collapse = ", "), fill = 118)
  cat("functions the current runbook calls without a namespace:", length(called), "\n")
  cat("of which defined nowhere:", length(missing), "\n")
  if (length(missing) > 0L) cat(paste(missing, collapse = ", "), fill = 118)
  unused <- sort(setdiff(defined_lib, c(called, unlist(lapply(others, \(.f) rst_called(readLines(.f, warn = FALSE)))))))
  cat("candidate functions nothing calls (possibly retired since):", length(unused), "\n")
  if (length(unused) > 0L) cat(paste(unused, collapse = ", "), fill = 118)

  cat("\n######## C.", cand$Runbook, "since the candidate ########\n")
  cand_date <- rst_git(c("show", "-s", "--format=%cI", cand$Commit))
  cat("candidate committed:", cand_date, "\n")
  cat("runbook commits after it:\n")
  cat(rst_git(c("log", "--date=short", "--format=%h %cd %s", paste0(cand$Commit, "..HEAD"), "--", cand$Runbook)),
      sep = "\n")
  qmd_then <- rst_git(c("show", paste0(cand$Commit, ":", cand$Runbook)))
  cat("runbook lines then:", length(qmd_then), "| now:", length(readLines(here::here(cand$Runbook), warn = FALSE)), "\n")
  dir_out <- here::here("2_output", fs::path_ext_remove(fs::path_file(cand$Library)))
  if (fs::dir_exists(dir_out)) {
    info <- fs::dir_info(path = dir_out, recurse = TRUE, type = "file")
    cat("newest output written:", format(max(info$modification_time)), "\n")
  }

  cat("\n######## D. Other copies of", fs::path_file(cand$Library), "########\n")
  hist <- rst_git(c("log", "--all", "--date=short", "--name-only", "--format=@@%h %cd"))
  commit_now <- ""
  hits <- character(0)
  for (h in hist) {
    if (startsWith(h, "@@")) {
      commit_now <- sub("^@@", "", h)
    } else if (nzchar(h) && grepl(fs::path_file(cand$Library), h, fixed = TRUE) && h != cand$Library) {
      hits <- c(hits, paste(commit_now, h))
    }
  }
  cat("in git history under another path:", length(hits), "\n")
  if (length(hits) > 0L) cat(utils::head(hits, 20), sep = "\n")

  roots <- c(here::here("2_output", "_BackUps"), here::here("2_output", "_Migration"), here::here("1_code"))
  roots <- roots[fs::dir_exists(roots)]
  pattern <- paste0(fs::path_ext_remove(fs::path_file(cand$Library)), ".*\\.R$")
  copies <- unlist(lapply(roots, \(.r) fs::dir_ls(path = .r, recurse = 4, type = "file", regexp = pattern, fail = FALSE)))
  copies <- setdiff(copies, here::here(cand$Library))
  cat("files of that name in the backup folders:", length(copies), "\n")
  for (cp in copies) {
    code <- readLines(cp, warn = FALSE)
    cat(sprintf("  %s | %s | %d lines | parses %s | defines %d\n", cp, format(fs::file_info(cp)$modification_time),
                length(code), rst_parses(code), length(rst_defined(code))))
  }

  stores <- c(here::here(".Rproj.user"), fs::path_expand("~/.local/share/rstudio"))
  stores <- stores[fs::dir_exists(stores)]
  store_files <- unlist(lapply(stores, \(.s) fs::dir_ls(path = .s, recurse = TRUE, type = "file", fail = FALSE)))
  store_files <- store_files[fs::file_size(store_files) < fs::as_fs_bytes("20MB")]
  found <- store_files[vapply(store_files, \(.f) {
    lines_ <- tryCatch(readLines(.f, warn = FALSE), error = function(.e) character(0))
    any(grepl(cand$Header, lines_, fixed = TRUE, useBytes = TRUE))
  }, logical(1))]
  cat("RStudio document store entries holding this library:", length(found), "\n")
  for (f in found) cat(sprintf("  %s | %s\n", f, format(fs::file_info(f)$modification_time)))

  ok <- rst_parses(lib_code) && length(missing) == 0L
  verdict <- c(verdict, sprintf("%s <- %s: %s", cand$Library, cand$Commit,
                                if (ok) "fits the current runbook" else "DOES NOT fit the current runbook"))
}

cat("\n######## Verdict ########\n")
cat(verdict, sep = "\n")
all_ok <- all(grepl("fits the current runbook$", verdict))
cat(if (all_ok) "SAFE TO RESTORE" else "NOT SAFE TO RESTORE AS IS -- send this report first", "\n")


# 3. Restore (only with DO_RESTORE <- TRUE) -----------------------------------------------------------------------------

if (DO_RESTORE && !all_ok) {
  cat("\nDO_RESTORE is TRUE, but the verdict is not SAFE: nothing was written.\n")
}
if (DO_RESTORE && all_ok) {
  cat("\n######## Restore ########\n")
  for (i in seq_len(nrow(candidates))) {
    cand <- candidates[i, ]
    target <- here::here(cand$Library)
    rst_git(c("show", paste0(cand$Commit, ":", cand$Library)), .stdout = target)
    code <- readLines(target, warn = FALSE)
    cat(cand$Library, "written from", cand$Commit, "|", length(code), "lines | parses:", rst_parses(code), "\n")
  }
  cat(rst_git(c("diff", "--stat", "--", candidates$Library)), sep = "\n")
  cat("Not committed. Render 02A and 02B to confirm, then commit both files in the Git pane.\n")
}

cat("\nDone. Report:", path_report, "\n")
sink()
