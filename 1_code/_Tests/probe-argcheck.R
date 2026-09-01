# ======================================================================================================================
# PROBE -- do the runbook's calls match the library's signatures?
# ======================================================================================================================
#
# WHAT THIS CATCHES. A chunk calling a function without a required argument, or with an argument the
# function does not have. Both are errors R only raises when the chunk runs, which on a long document
# means finding out twenty minutes in -- and for 10-ExportData, after the caches have been rebuilt.
#
# IT USES R'S OWN MATCHER. match.call() is what R does when it dispatches a call, so this asks the
# same question the render will ask rather than approximating it with a regular expression. That
# matters in two places a text scan gets wrong. Nested calls: in
# exp_report_columns(.tab = exp_table_columns(.tab = x, .path_register = p)) the inner argument
# belongs to the inner function, and a scan reading the outer call's text sees it as the outer's.
# And the pipe: x |> reg_derive() parses as reg_derive(x), so .tab is supplied -- by the time R has
# parsed the chunk there is no pipe left to misread.
#
# WHAT IT DOES NOT CATCH. Anything about the values: a path that does not exist, a column that is not
# there, a type mismatch. Those are the render's business. This is only about the shape of the call.
#
# IT RUNS NOTHING AND WRITES NOTHING. The library is sourced into a throwaway environment; the
# chunks are parsed and never evaluated.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; pure ASCII.

# 1. Functions ---------------------------------------------------------------------------------------------------------

#' The R code in a Quarto document, as one string
#'
#' Chunk options are dropped: they are YAML in a comment and mean nothing to the parser.
#'
#' @param .path Path to a .qmd file.
#' @return Character scalar.
arg_chunk_code <- function(.path) {
  if (FALSE) .path <- "1_code/10-ExportData.qmd"

  lines_ <- readLines(.path, warn = FALSE)
  open_  <- grep("^```\\{r\\}?", lines_)
  close_ <- grep("^```\\s*$", lines_)

  keep_ <- unlist(lapply(open_, function(.o) {
    end_ <- close_[close_ > .o]
    if (length(end_) == 0L) return(integer(0))
    seq_len(0L) |> c(seq(.o + 1L, min(end_) - 1L))
  }))

  code_ <- lines_[keep_]
  paste(code_[!grepl("^#\\|", code_)], collapse = "\n")
}

#' Every call inside an expression, including nested ones
#'
#' @param .expr A call, name or constant.
#' @return A list of calls.
arg_calls <- function(.expr) {
  if (FALSE) .expr <- quote(f(g(1), 2))

  if (!is.call(.expr)) return(list())

  kids_ <- unlist(
    x         = lapply(as.list(.expr), function(.e) if (missing(.e)) list() else arg_calls(.e)),
    recursive = FALSE
  )

  c(list(.expr), kids_)
}

#' Check every call in a runbook against the library it sources
#'
#' @param .path_lib Path to the .R function library.
#' @param .path_qmd Path to the paired .qmd.
#' @param .prefix Character. Only functions whose name starts with this are checked.
#' @return A tibble: Function, Problem, Detail. Empty where everything matches.
arg_check <- function(.path_lib, .path_qmd, .prefix) {
  if (FALSE) {
    .path_lib <- "1_code/10-ExportData.R"
    .path_qmd <- "1_code/10-ExportData.qmd"
    .prefix   <- "exp"
  }

  env_ <- new.env(parent = globalenv())
  suppressWarnings(source(.path_lib, local = env_, encoding = "UTF-8"))

  mine_ <- ls(envir = env_)
  mine_ <- mine_[grepl(paste0("^", .prefix), mine_)]
  mine_ <- mine_[vapply(mine_, function(.n) is.function(get(.n, envir = env_)), logical(1L))]

  calls_ <- unlist(
    x         = lapply(as.list(parse(text = arg_chunk_code(.path = .path_qmd))), arg_calls),
    recursive = FALSE
  )

  out_ <- lapply(calls_, function(.cl) {
    head_ <- .cl[[1L]]
    if (!is.name(head_)) return(NULL)

    nm_ <- as.character(head_)
    if (!nm_ %in% mine_) return(NULL)

    def_ <- get(nm_, envir = env_)

    # AN UNKNOWN ARGUMENT IS WHAT match.call() REFUSES TO MATCH, so the error it raises is the check.
    got_ <- tryCatch(match.call(definition = def_, call = .cl), error = function(.e) .e)
    if (inherits(got_, "error")) {
      return(tibble::tibble(Function = nm_, Problem = "unknown argument",
                            Detail = conditionMessage(got_)))
    }

    # A MISSING ONE IS NOT: match.call() is happy to match a call that would fail on evaluation, so
    # the formals carrying no default are compared against what the matched call supplies.
    fml_  <- formals(def_)
    need_ <- names(fml_)[vapply(fml_, function(.f) identical(.f, quote(expr = )), logical(1L))]
    need_ <- setdiff(need_, "...")
    miss_ <- setdiff(need_, names(got_)[-1L])

    if (length(miss_) == 0L) return(NULL)

    tibble::tibble(Function = nm_, Problem = "required argument not supplied",
                   Detail = paste(miss_, collapse = ", "))
  })

  dplyr::distinct(dplyr::bind_rows(out_))
}

#' Check one pair and say what it found
#'
#' @param .stem Character. Script stem, e.g. "10-ExportData".
#' @param .prefix Character. Function prefix, e.g. "exp".
#' @param .dir Character. Directory holding the pair.
#' @return The problem tibble, invisibly.
arg_report <- function(.stem, .prefix, .dir = "1_code") {
  if (FALSE) {
    .stem   <- "10-ExportData"
    .prefix <- "exp"
  }

  bad_ <- arg_check(
    .path_lib = file.path(.dir, paste0(.stem, ".R")),
    .path_qmd = file.path(.dir, paste0(.stem, ".qmd")),
    .prefix   = .prefix
  )

  if (nrow(bad_) == 0L) {
    cli::cli_alert_success("{(.stem)}: every call matches its signature")
  } else {
    cli::cli_alert_danger("{(.stem)}: {nrow(bad_)} mismatch{?es}")
    print(as.data.frame(bad_), row.names = FALSE, right = FALSE)
  }

  invisible(bad_)
}

# 2. Runner ------------------------------------------------------------------------------------------------------------

#: THE PAIRS THIS CHECKS, and the prefix each library uses. A pair added to the pipeline is a row
#: here; one whose functions carry no shared prefix cannot be checked and should acquire one.
.arg_pairs <- tibble::tribble(
  ~Stem,             ~Prefix,
  "01D-GetItems",    "itm",
  "02B-Register",    "reg",
  "10-ExportData",   "exp"
)

.arg_width_old <- getOption("width")
options(width = 150)

cli::cli_h1("Runbook calls against library signatures")

.arg_dir <- if (requireNamespace("here", quietly = TRUE)) here::here("1_code") else "1_code"

res_arg <- purrr::pmap(
  .l = list(.stem = .arg_pairs$Stem, .prefix = .arg_pairs$Prefix),
  .f = function(.stem, .prefix) {
    if (!file.exists(file.path(.arg_dir, paste0(.stem, ".R")))) {
      cli::cli_alert_warning("{(.stem)}: not found, skipped")
      return(tibble::tibble())
    }
    arg_report(.stem = .stem, .prefix = .prefix, .dir = .arg_dir)
  }
)

n_bad_ <- sum(vapply(res_arg, nrow, integer(1L)))
if (n_bad_ == 0L) {
  cli::cli_alert_success("All {nrow(.arg_pairs)} pair{?s} clean")
} else {
  cli::cli_alert_danger("{n_bad_} mismatch{?es} across {nrow(.arg_pairs)} pair{?s}")
}

options(width = .arg_width_old)
