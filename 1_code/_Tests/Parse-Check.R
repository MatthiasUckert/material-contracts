# ======================================================================================================================
# Parse-Check.R -- does every R file in the repository parse?
# ======================================================================================================================
#
# WHY THIS EXISTS, AND WHY IT IS ONE LINE OF REAL WORK
# A string literal that closes where the author did not intend is invisible to every mechanical check
# this project runs. Writing \\" inside an R string is an escaped BACKSLASH followed by a TERMINATING
# quote -- \" is the escaped quote -- so one backslash too many ends the literal mid-sentence and the
# remaining prose runs as code.
#
# The bracket checker cannot see it: it models the string exactly as R does, finds the leaked prose
# happens to have balanced parentheses, and reports zero. The undefined-call scan cannot see it. The
# signature scan cannot see it. It surfaces at source() as an "unexpected symbol" pointing at a word
# in the middle of a sentence, which is a long way from the missing backslash that caused it.
#
# R already owns a parser. Approximating one in another language to catch this was tried and produced
# fourteen false positives on valid code -- `if (x) "a" else "b"` closes a string before a letter, and
# so does a trailing comment, and so does a case_when formula. parse() has none of those problems.
#
# WHAT IT DOES NOT DO. It does not run anything, source anything or evaluate anything: parse() reads
# the file and builds expressions without executing them, so a file with a load-time side effect --
# 03A registers a vocabulary, _Entity.R registers OrgRole -- is safe to check.
#
# Usage, before delivering or rendering anything:
#   source(here::here("1_code", "_Tests", "Parse-Check.R"), encoding = "UTF-8")

here::i_am("1_code/_Commons/_Initialize.R")

options(cli.num_colors = 1, cli.width = 120)

.dirs <- c(
  here::here("1_code"),
  here::here("1_code", "_Commons"),
  here::here("1_code", "_Tests"),
  here::here("1_code", "_Scripts")
)

.files <- .dirs[fs::dir_exists(.dirs)] |>
  purrr::map(\(.d) fs::dir_ls(.d, glob = "*.R", type = "file")) |>
  unlist(use.names = FALSE) |>
  sort()

cli::cli_h1("Parse check")
cli::cli_alert_info(
  "{length(.files)} R file{?s} under {paste(fs::path_rel(.dirs[fs::dir_exists(.dirs)],
   start = here::here()), collapse = ', ')}."
)

.res <- purrr::map(.files, function(.f) {
  err_ <- tryCatch({
    parse(file = .f, encoding = "UTF-8")
    NA_character_
  }, error = function(.e) conditionMessage(.e))

  tibble::tibble(
    File = as.character(fs::path_rel(.f, start = here::here())),
    Ok   = is.na(err_),
    Says = err_
  )
}) |>
  purrr::list_rbind()

.bad <- dplyr::filter(.res, !.data$Ok)

if (nrow(.bad) == 0L) {
  cli::cli_alert_success("Every file parses.")
} else {
  # THE MESSAGE IS PRINTED WHOLE, not summarised. parse() names the file, the line, the column and
  # the token it choked on, and every one of those is needed: the token is where the reader looks and
  # the column is where the cause usually is.
  cli::cli_alert_danger("{nrow(.bad)} file{?s} do{?es/} not parse.")
  purrr::walk(seq_len(nrow(.bad)), function(.i) {
    cli::cli_h3(.bad$File[[.i]])
    cli::cli_verbatim(paste("  ", strsplit(.bad$Says[[.i]], "\n")[[1L]]))
  })
}

# A SECOND CHECK ON THE SAME FILES, because a qmd's chunks are R too and a broken string there fails
# at render rather than at source. knitr::purl() extracts them without evaluating anything.
.qmds <- fs::dir_ls(here::here("1_code"), glob = "*.qmd", type = "file") |> sort()

cli::cli_h2("Chunks")
.res_q <- purrr::map(.qmds, function(.f) {
  err_ <- tryCatch({
    tmp_ <- withr::local_tempfile(fileext = ".R")
    knitr::purl(input = .f, output = tmp_, quiet = TRUE, documentation = 0L)
    parse(file = tmp_, encoding = "UTF-8")
    NA_character_
  }, error = function(.e) conditionMessage(.e))

  tibble::tibble(
    File = as.character(fs::path_rel(.f, start = here::here())),
    Ok   = is.na(err_),
    Says = err_
  )
}) |>
  purrr::list_rbind()

.bad_q <- dplyr::filter(.res_q, !.data$Ok)

if (nrow(.bad_q) == 0L) {
  cli::cli_alert_success("Every document's chunks parse.")
} else {
  cli::cli_alert_danger("{nrow(.bad_q)} document{?s} ha{?s/ve} a chunk that does not parse.")
  purrr::walk(seq_len(nrow(.bad_q)), function(.i) {
    cli::cli_h3(.bad_q$File[[.i]])
    cli::cli_verbatim(paste("  ", strsplit(.bad_q$Says[[.i]], "\n")[[1L]]))
  })
}
