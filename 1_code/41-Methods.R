# 41-Methods: the manuscript's Section 3, written once and rendered for Overleaf --------------------------------------
#
# WHAT THIS FILE DOES
# The method chapter of the paper is prose that cites numbers. The prose is written in 41-Methods.qmd; the numbers
# are 40-Numbers' register. This library reads that register, cites a key as \pnum{Key} in the LaTeX pass and as its
# value in the html pass, keeps citations, references and cross-references readable in both passes, checks that every
# key the chapter cites exists, and copies the rendered fragment to the paper folder.
#
# WHY \pnum{} AND NOT THE VALUE. The fragment is pasted into Overleaf and edited there by two authors. A value pasted
# into it is a copy that stops being true at the next re-export; a macro is read from Numbers.tex at every compile,
# so the sentence in Overleaf and the table in the appendix cannot disagree, whoever last touched the sentence.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed locals; .data$ for
# existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure ASCII; parenthesised cli interpolation.

if (FALSE) {
  .path_register <- here::here("2_output", "40-Numbers", "Output", "Numbers.csv")
  .path_qmd      <- here::here("1_code", "41-Methods.qmd")
}

# THE CHAPTER'S STATE: the register once loaded, and every key cited while the text renders, so Validation can say
# which keys the chapter used and which did not resolve.
.met_state <- new.env(parent = emptyenv())
.met_state$Register <- NULL
.met_state$Cited    <- character(0)


# 1. The register --------------------------------------------------------------------------------------------------

#' Load 40's register
#'
#' Numbers.csv is one row per key with its value and status; only rows with Status "ok" carry a value. Loaded once
#' per render and kept in the chapter's state.
#'
#' @param .path_register Character. 40-Numbers/Output/Numbers.csv.
#' @return Invisibly, the register tibble.
met_numbers_load <- function(.path_register) {
  if (FALSE) .path_register <- here::here("2_output", "40-Numbers", "Output", "Numbers.csv")
  if (!fs::file_exists(.path_register)) {
    cli::cli_abort("No register at {.file {(.path_register)}}: render 40-Numbers first.")
  }
  reg_ <- readr::read_csv(
    file           = .path_register,
    col_types      = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  need_ <- c("Key", "Section", "Value", "Status", "Note")
  miss_ <- setdiff(need_, names(reg_))
  if (length(miss_) > 0L) cli::cli_abort("The register lacks {.field {miss_}}.")
  .met_state$Register <- reg_
  .met_state$Cited    <- character(0)
  n_ok_ <- sum(reg_$Status == "ok")
  when_ <- format(fs::file_info(.path_register)$modification_time, "%Y-%m-%d %H:%M")
  cli::cli_alert_success("Register loaded: {nrow(reg_)} keys, {n_ok_} resolved, written {when_}.")
  invisible(reg_)
}

#' Cite a number
#'
#' In the LaTeX pass the macro, so Overleaf reads the value from Numbers.tex; in the html pass the value, so the
#' runbook reads as the chapter will. A key the register does not hold aborts the render: a typo is an error, not a
#' blank. A key it holds but could not resolve renders as a bold marker in html, and still as the macro in LaTeX,
#' where it prints as ??Key until 40 resolves it.
#'
#' @param .key Character. The key in 40's registry.
#' @return Character scalar.
met_n <- function(.key) {
  if (FALSE) .key <- "DocsAll"
  reg_ <- .met_state$Register
  if (is.null(reg_)) cli::cli_abort("No register loaded: met_numbers_load() has not run.")
  row_ <- reg_[reg_$Key == .key, , drop = FALSE]
  if (nrow(row_) != 1L) cli::cli_abort("{.val {(.key)}} is not in 40's registry.")
  .met_state$Cited <- c(.met_state$Cited, .key)
  if (isTRUE(knitr::is_latex_output())) return(paste0("\\pnum{", .key, "}"))
  if (identical(row_$Status, "ok")) return(row_$Value)
  paste0("**[?", .key, "]**")
}


# 2. Markup that reads in both passes --------------------------------------------------------------------------------

#' A cross-reference to a section of the paper
#'
#' In LaTeX, Section~\ref{label}; in html, the word and the number the label is known to carry, since the paper is
#' not rendered here. The map is the chapter's own labels plus the ones it points to.
#'
#' @param .label Character. The LaTeX label.
#' @param .word Character. "Section", "Appendix", "Online Appendix", "Table".
#' @param .map Named character. Label to printed number, for the html pass.
#' @return Character scalar.
met_ref <- function(.label, .word = "Section", .map = .met_labels) {
  if (FALSE) {
    .label <- "sec:data"
    .word  <- "Section"
    .map   <- .met_labels
  }
  if (isTRUE(knitr::is_latex_output())) return(paste0(.word, "~\\ref{", .label, "}"))
  num_ <- if (.label %in% names(.map)) .map[[.label]] else paste0("[", .label, "]")
  paste(.word, num_)
}

#' A citation
#'
#' \citet or \citep in LaTeX, resolved against the paper's bibliography in Overleaf; the keys in brackets in html.
#'
#' @param .keys Character. Citation keys, as the paper's .bib has them.
#' @param .mode Character. "t" for \citet, "p" for \citep.
#' @return Character scalar.
met_cite <- function(.keys, .mode = "t") {
  if (FALSE) {
    .keys <- c("Ahci2024", "Boone2016")
    .mode <- "t"
  }
  if (!.mode %in% c("t", "p")) cli::cli_abort("{.arg .mode} is {.val t} or {.val p}.")
  if (isTRUE(knitr::is_latex_output())) return(paste0("\\cite", .mode, "{", paste(.keys, collapse = ","), "}"))
  paste0("[", paste(.keys, collapse = "; "), "]")
}

# THE LABELS THE CHAPTER CARRIES OR POINTS TO, with the numbers they print in the paper as it stands. Used by the html
# pass only; the LaTeX pass leaves resolution to Overleaf.
.met_labels <- c(
  "sec:method"          = "3",
  "sec:data"            = "3.1",
  "sec:classification"  = "3.2",
  "sec:content"         = "3.3",
  "sec:redactions"      = "3.4",
  "sec:announcements"   = "3.5",
  "ch:inst_set"         = "2",
  "fig:contract_ex"     = "A1",
  "sec: contract_sum_ex" = "C",
  "tab:contract_classification" = "1",
  "sec-oa-a"            = "A",
  "sec-oa-b"            = "B",
  "sec-oa-c"            = "C",
  "sec-oa-d"            = "D",
  "sec-oa-a-holds"      = "A.1",
  "sec-oa-b-rgetedgar"  = "B.2",
  "sec-oa-b-rlabeldocs" = "B.3",
  "sec-oa-c-sample"     = "C.1",
  "sec-oa-c-transformer" = "C.2",
  "sec-oa-c-arms"       = "C.3"
)


# 3. Validation ---------------------------------------------------------------------------------------------------------

#' The keys the chapter cited, against the register
#'
#' Every cited key exists by construction (met_n() aborts otherwise); what can still go wrong is a key that exists
#' and did not resolve, which would print as ??Key in the paper. Listed, and with .strict aborting.
#'
#' @param .strict Logical. TRUE aborts on a cited key without a value.
#' @return Invisibly, a tibble: Key, N (citations), Value, Status.
met_report_cited <- function(.strict) {
  if (FALSE) .strict <- FALSE
  reg_ <- .met_state$Register
  if (is.null(reg_)) cli::cli_abort("No register loaded: met_numbers_load() has not run.")
  cited_ <- tibble::tibble(Key = .met_state$Cited) |>
    dplyr::count(.data$Key, name = "N") |>
    dplyr::left_join(
      y  = dplyr::select(reg_, dplyr::all_of(c("Key", "Section", "Value", "Status"))),
      by = dplyr::join_by(Key)
    ) |>
    dplyr::arrange(.data$Section, .data$Key)
  bad_ <- cited_$Key[cited_$Status != "ok"]
  cli::cli_h2("Numbers this chapter cites")
  tbl_say(cited_)
  if (length(bad_) == 0L) {
    cli::cli_alert_success("All {nrow(cited_)} cited keys resolved in 40's last render.")
  } else {
    cli::cli_alert_danger("{length(bad_)} cited key{?s} without a value: {.val {bad_}}.")
    if (isTRUE(.strict)) cli::cli_abort("Cited keys without a value would print as ??Key.")
  }
  invisible(cited_)
}


# 4. Deployment ---------------------------------------------------------------------------------------------------------

#' Copy the rendered fragment to the paper folder as 4-method.tex
#'
#' The LaTeX pass writes the fragment under _rendered/; this copies it where Overleaf's file is kept, archives the
#' version it replaces, and says whether the content changed. The fragment is what goes into Overleaf in place of
#' 4-method.tex; formatting adjustments made there come back into the qmd, never the other way.
#'
#' @param .path_tex Character. The fragment the LaTeX pass wrote.
#' @param .path_qmd Character. This document; a fragment older than it is stale and not deployed.
#' @param .dir_deploy Character. The paper folder for manuscript files.
#' @param .name Character. The file name in the paper folder.
#' @return Invisibly, TRUE where the deployed file changed.
met_deploy <- function(.path_tex, .path_qmd, .dir_deploy, .name = "4-method.tex") {
  if (FALSE) {
    .path_tex   <- here::here("_rendered", "1_code", "41-Methods.tex")
    .path_qmd   <- here::here("1_code", "41-Methods.qmd")
    .dir_deploy <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "200-Paper_figures",
      "Manuscript"
    )
    .name <- "4-method.tex"
  }
  if (!fs::file_exists(.path_tex)) {
    cli::cli_abort("No fragment at {.file {(.path_tex)}}: run the LaTeX pass (quarto render from a terminal).")
  }
  if (fs::file_info(.path_tex)$modification_time < fs::file_info(.path_qmd)$modification_time) {
    cli::cli_abort("The fragment is older than the document: render again before deploying.")
  }
  fs::dir_create(.dir_deploy)
  dest_ <- fs::path(.dir_deploy, .name)
  body_ <- function(.p) {
    l_ <- readLines(.p, warn = FALSE)
    l_[!grepl("^% GENERATED FROM", l_)]
  }
  if (fs::file_exists(dest_) && identical(body_(dest_), body_(.path_tex))) {
    cli::cli_alert_info("{.file {(.name)}} in the paper folder already matches this render.")
    return(invisible(FALSE))
  }
  if (fs::file_exists(dest_)) {
    arch_ <- fs::path(.dir_deploy, "Archive")
    fs::dir_create(arch_)
    stamp_ <- format(fs::file_info(dest_)$modification_time, "%Y%m%d-%H%M%S")
    fs::file_copy(
      path      = dest_,
      new_path  = fs::path(arch_, paste0(fs::path_ext_remove(.name), "-", stamp_, ".tex")),
      overwrite = TRUE
    )
  }
  fs::file_copy(
    path      = .path_tex,
    new_path  = dest_,
    overwrite = TRUE
  )
  cli::cli_alert_success("{.file {(.name)}} copied to {.path {(.dir_deploy)}}: paste it into Overleaf.")
  invisible(TRUE)
}
