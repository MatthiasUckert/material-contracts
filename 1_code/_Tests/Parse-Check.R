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

# ======================================================================================================================
# A THIRD CHECK: does every here::here() name a directory that exists?
# ======================================================================================================================
#
# WHY THIS WAS ADDED, AND WHAT IT WOULD HAVE CAUGHT
# _Scripts/rebuild-geo-lookup.R pointed at here::here("contracts-engine", "data", "gazetteer") for
# months after the Python tree was split into four folders and contracts-engine ceased to exist. It
# parsed. It passed every gate this file runs. It was live tooling -- 04B2 names it in an abort
# message as the way to rebuild geo_lookup.parquet, and matcon-extract hashes that parquet's contents
# into gazetteer-v2's spec -- so the first person to rebuild the gazetteer would have found the
# provenance of a published artifact broken, and found it at the worst possible moment.
#
# A SCRIPT NOBODY SOURCES IS NOT A SCRIPT NOBODY RUNS. Everything under _Scripts/ is run by hand
# rather than sourced by a document, which means no render ever touches it and no error ever surfaces
# until somebody needs it. That is precisely the code a static check is worth most on.
#
# WHY ONLY THE FIRST SEGMENT. here::here("2_output", ...) names a directory a pipeline creates, and
# checking those would fire on every clean checkout. The FIRST segment is different: it is a
# top-level directory of the repository, it is committed, and it either exists or the path is wrong.
# Narrow on purpose -- a check with false positives is a check people learn to skip.

cli::cli_h2("Paths")

# THE PARSER FINDS THESE, NOT A REGULAR EXPRESSION. The first version of this check scanned raw
# lines and matched the here::here() call inside the comment above, reporting a root that was
# missing because the comment was describing it as missing. That is the failure this file exists to
# prevent, one level up: R owns a parser and an approximation of it produces false positives on
# valid code. getParseData() classifies every token, so a string inside a comment is a COMMENT and
# never reaches the check.
#
# WHY only SYMBOL_FUNCTION_CALL named "here": in here::i_am(), "here" is a SYMBOL_PACKAGE and the
# call is i_am, so filtering on the call token picks here::here() and bare here() and nothing else.

.here_roots <- function(.file) {
  pd_ <- tryCatch(
    utils::getParseData(parse(file = .file, encoding = "UTF-8", keep.source = TRUE)),
    error = function(.e) NULL
  )
  if (is.null(pd_) || nrow(pd_) == 0L) return(NULL)

  tok_ <- pd_ |>
    dplyr::filter(.data$terminal) |>
    dplyr::arrange(.data$line1, .data$col1)

  at_ <- which(tok_$token == "SYMBOL_FUNCTION_CALL" & tok_$text == "here")
  if (length(at_) == 0L) return(NULL)

  purrr::map(at_, function(.i) {
    # The opening parenthesis, then the first argument. A call whose first argument is not a string
    # literal -- here::here(.dir) -- is skipped rather than guessed at.
    arg_ <- .i + 2L
    if (arg_ > nrow(tok_) || tok_$token[[arg_]] != "STR_CONST") return(NULL)
    tibble::tibble(
      Line  = tok_$line1[[.i]],
      First = stringi::stri_replace_all_regex(tok_$text[[arg_]], '^"|"$', "")
    )
  }) |>
    purrr::list_rbind()
}

# qmd chunks are checked too, through the same purl the section above uses. Their line numbers refer
# to the extracted chunks rather than to the document, so they are reported as missing rather than
# as a number that would send a reader to the wrong line.
.roots <- purrr::map(.files, function(.f) {
  got_ <- .here_roots(.f)
  if (is.null(got_) || nrow(got_) == 0L) return(NULL)
  dplyr::mutate(got_, File = as.character(fs::path_rel(.f, start = here::here())), .before = 1L)
}) |>
  purrr::list_rbind()

.roots_q <- purrr::map(.qmds, function(.f) {
  got_ <- tryCatch({
    tmp_ <- withr::local_tempfile(fileext = ".R")
    knitr::purl(input = .f, output = tmp_, quiet = TRUE, documentation = 0L)
    .here_roots(tmp_)
  }, error = function(.e) NULL)
  if (is.null(got_) || nrow(got_) == 0L) return(NULL)
  got_ |>
    dplyr::mutate(
      Line = NA_integer_,
      File = as.character(fs::path_rel(.f, start = here::here()))
    ) |>
    dplyr::select("File", "Line", "First")
}) |>
  purrr::list_rbind()

.roots <- dplyr::bind_rows(.roots, .roots_q)

# A FILE THAT DOES NOT PARSE IS INVISIBLE HERE, AND THAT MUST BE SAID RATHER THAN LEFT IMPLICIT.
# getParseData() needs a parse tree, so the files the section above just reported as broken are
# precisely the ones this section cannot examine -- and a file broken enough not to parse is more
# likely, not less, to also carry a path that has moved. Check-NER-ORG.R is exactly that case: it
# named a missing root, and once the check moved onto the parser its parse failure hid the finding.
# A count that silently shrinks when a file breaks is worse than no count.
.unseen <- dplyr::bind_rows(.bad, .bad_q)$File

if (length(.unseen) > 0L) {
  cli::cli_alert_warning(
    "{length(.unseen)} file{?s} could not be examined for paths because {?it does/they do} not \\
     parse: {paste(.unseen, collapse = ', ')}."
  )
}

if (nrow(.roots) == 0L) {
  cli::cli_alert_info("No here::here() literals found.")
} else {
  .roots <- .roots |>
    dplyr::distinct(.data$File, .data$Line, .data$First) |>
    dplyr::mutate(Exists = fs::dir_exists(fs::path(here::here(), .data$First)) |
                    fs::file_exists(fs::path(here::here(), .data$First)))

  .bad_p <- dplyr::filter(.roots, !.data$Exists)

  if (nrow(.bad_p) == 0L) {
    cli::cli_alert_success(
      "Every here::here() root exists ({dplyr::n_distinct(.roots$First)} distinct)."
    )
  } else {
    cli::cli_alert_danger(
      "{nrow(.bad_p)} here::here() {cli::qty(nrow(.bad_p))}call{?s} name{?s/} a root that is not \\
       in the repository."
    )
    .bad_p |>
      dplyr::select("File", "Line", "First") |>
      print(n = Inf)
    cli::cli_alert_info(
      "A root that does not exist is a path that has moved. The file still parses and still passes \\
       every other gate here."
    )
  }
}
