# 50-Numbers: every number the paper and the online appendix cite, from one file --------------------------------------
#
# WHAT THIS FILE DOES
# The manuscript and the appendix chapters cite numbers -- counts of contracts, shares of filings, the accuracy of a
# model, a median duration. Every one of them is a cell of a tibble some exhibit is printed from: 30's tables and
# figures save theirs under 30/Output/Data. This library
# resolves the whole registry over those tibbles, writes them as one LaTeX file (`Numbers.tex`) the Overleaf project
# inputs, and writes the register beside it so a value can be checked without compiling anything.
#
# ONE WRITER. Until this pair existed each appendix chapter wrote its own OA-Numbers-X.tex, and the manuscript typed
# its numbers, so the intro and Section 4 could -- and did -- disagree on the size of the sample. Now a key is unique
# across the paper, resolved once, and cited as \pnum{Key} in the manuscript and \oanum{Key} in the appendix; both
# names read the same table.
#
# WHAT IS COMPUTED HERE. As little as possible. Every tibble the registry reads is built by 30-FinalExhibits and
# read from 30/Output/Data; the one exception is the data package's own report (chapter F below), which 40B writes
# and this pair reads into a small tibble under its own Data/.
#
# THE REGISTRY IS THE DOCUMENT. Each row carries the section of the paper the number appears in and a one-line note
# saying what it is in words, and those two columns are what the register prints beside the value.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed locals; .data$ for
# existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure ASCII; parenthesised cli interpolation.

# 1. The registry: what a row must carry, and how the sections are ordered ---------------------------------------------

#' Check the registry before it is resolved
#'
#' A registry row is Key, Section, Exhibit, Column, Where, Fun, Format, Note. Keys are unique across the paper; a
#' section is one of the paper's sections or an appendix chapter, in the order the register prints; a note is not
#' empty, because an undocumented number is the thing this pair exists to prevent.
#'
#' @param .spec Tibble. The registry.
#' @param .sections Character. The sections in print order.
#' @return Invisibly, .spec in section order.
num_registry_check <- function(.spec, .sections) {
  if (FALSE) {
    .spec     <- tab_numbers_spec
    .sections <- .num_sections
  }
  need_ <- c("Key", "Section", "Exhibit", "Column", "Where", "Fun", "Format", "Note")
  miss_ <- setdiff(need_, names(.spec))
  if (length(miss_) > 0L) cli::cli_abort("The registry lacks {.field {miss_}}.")
  dup_ <- unique(.spec$Key[duplicated(.spec$Key)])
  if (length(dup_) > 0L) cli::cli_abort("Number key{?s} declared twice: {.val {dup_}}.")
  bad_ <- unique(.spec$Section[!.spec$Section %in% .sections])
  if (length(bad_) > 0L) cli::cli_abort("Unknown section{?s} {.val {bad_}}; the order is {.val {(.sections)}}.")
  blank_ <- .spec$Key[is.na(.spec$Note) | !nzchar(trimws(.spec$Note))]
  if (length(blank_) > 0L) cli::cli_abort("No note for {.val {blank_}}: every number says what it is.")
  bad_key_ <- .spec$Key[!grepl("^[A-Za-z][A-Za-z0-9]*$", .spec$Key)]
  if (length(bad_key_) > 0L) {
    cli::cli_abort("Key{?s} {.val {bad_key_}} cannot be a LaTeX macro argument: letters and digits only.")
  }
  invisible(.spec[order(match(.spec$Section, .sections), seq_len(nrow(.spec))), , drop = FALSE])
}


# 2. The numbers file and the register -------------------------------------------------------------------------------

#' The numbers file as lines: \pnum for the paper, \oanum for the appendix, one \pdef per key
#'
#' The paper's preamble inputs this one file. \pdef defines a key, \pnum prints it and prints ??Key in bold where the
#' key is missing, so a typo shows in the PDF; \oadef and \oanum are the same two macros under the names the appendix
#' fragments emit. Every definition carries the section and the exhibit it came from as a trailing comment, so the
#' file is a register on its own. A key that did not resolve is left out and listed in the header, so it prints as
#' ??Key rather than as a stale value.
#'
#' \providecommand rather than \newcommand, so a project that already defines one of the four names keeps its own.
#'
#' @param .tab Tibble from oa_numbers_set(), with Section and Note.
#' @param .doc Character. This pair's stem, named in the header.
#' @return Character vector, one line each.
num_numbers_tex <- function(.tab, .doc) {
  if (FALSE) {
    .tab <- tab_numbers
    .doc <- "50-Numbers"
  }
  ok_  <- .tab[.tab$Status == "ok", , drop = FALSE]
  bad_ <- .tab$Key[.tab$Status != "ok"]
  pad_ <- function(.x, .w) formatC(.x, width = .w, flag = "-")
  w_key_ <- max(nchar(ok_$Key), 1L) + 2L
  w_val_ <- max(nchar(ok_$Value), 1L) + 2L
  defs_ <- paste0(
    pad_(paste0("\\pdef{", ok_$Key, "}"), w_key_ + 7L),
    pad_(paste0("{", ok_$Value, "}"), w_val_),
    " % ", pad_(ok_$Section, 6L), " ", ok_$Exhibit
  )
  # BLANK LINE AND A HEADER BETWEEN SECTIONS, so the file reads by section as the register does.
  sec_ <- ok_$Section
  new_ <- c(TRUE, sec_[-1L] != sec_[-length(sec_)])
  body_ <- unlist(purrr::map(seq_along(defs_), \(.i) {
    if (new_[.i]) c("", paste0("% ---- ", sec_[.i], " ", strrep("-", max(0L, 100L - nchar(sec_[.i])))), defs_[.i])
    else defs_[.i]
  }))
  c(
    paste0("% Numbers.tex -- every number the paper and the online appendix cite, written by ", .doc, " on ",
           format(Sys.time(), "%Y-%m-%d %H:%M"), "."),
    "% Do not edit: a value changed here is overwritten at the next render. Cite a key as \\pnum{Key} in the",
    "% manuscript or \\oanum{Key} in the appendix; an unknown key prints as ??Key in bold.",
    if (length(bad_) > 0L) {
      paste0("% Unresolved at this render, and so printed as ??Key: ", paste(bad_, collapse = ", "), ".")
    },
    "\\makeatletter",
    "\\providecommand{\\pdef}[2]{\\@namedef{pnum@#1}{#2}}",
    "\\providecommand{\\pnum}[1]{\\ifcsname pnum@#1\\endcsname\\csname pnum@#1\\endcsname\\else\\textbf{??#1}\\fi}",
    "\\providecommand{\\oadef}{\\pdef}",
    "\\providecommand{\\oanum}{\\pnum}",
    "\\makeatother",
    body_
  )
}

#' Write the numbers file and the register, on every render
#'
#' Three files under Output/: Numbers.tex for Overleaf, Numbers.csv as the register (Key, Section, Value, Exhibit,
#' Column, Where, Fun, Format, Status, Note), and Numbers-unresolved.txt listing what did not resolve and why, empty
#' when everything did. Written unconditionally, as the chapters' files were: nothing keys a cache on them, and a
#' fresh timestamp is what tells deployment a file is current.
#'
#' @param .tab Tibble from oa_numbers_set(), with Section and Note.
#' @param .dir_own Character. This pair's output directory.
#' @param .doc Character. This pair's stem.
#' @return Invisibly, the three paths.
num_numbers_write <- function(.tab, .dir_own, .doc) {
  if (FALSE) {
    .tab     <- tab_numbers
    .dir_own <- here::here("2_output", "50-Numbers", "Output")
    .doc     <- "50-Numbers"
  }
  fs::dir_create(.dir_own)
  paths_ <- fs::path(.dir_own, c("Numbers.tex", "Numbers.csv", "Numbers-unresolved.txt"))
  writeLines(
    text = num_numbers_tex(
      .tab = .tab,
      .doc = .doc
    ),
    con  = paths_[1L]
  )
  reg_ <- .tab |>
    dplyr::select(dplyr::all_of(c("Key", "Section", "Value", "Exhibit", "Column", "Where", "Fun", "Format",
                                  "Status", "Note")))
  readr::write_csv(
    x    = reg_,
    file = paths_[2L],
    na   = ""
  )
  bad_ <- .tab[.tab$Status != "ok", , drop = FALSE]
  writeLines(
    text = if (nrow(bad_) == 0L) character(0) else paste0(bad_$Key, ": ", bad_$Status, " (", bad_$Exhibit, ")"),
    con  = paths_[3L]
  )
  cli::cli_alert_success(
    "{sum(.tab$Status == 'ok')} of {nrow(.tab)} numbers written to {.file {fs::path_file(paths_[1L])}}; \\
     the register is {.file {fs::path_file(paths_[2L])}}."
  )
  invisible(paths_)
}


# 3. Checks: what cites what ------------------------------------------------------------------------------------------

#' The keys a set of LaTeX or Quarto files cites
#'
#' Scans for \pnum{Key} and \oanum{Key} in the files given -- the manuscript's section files, the appendix fragments,
#' or the qmd sources that emit them. In a qmd the appendix cites through `r oa_n("Key")`, which is also read, so a
#' chapter can be checked before it is rendered.
#'
#' @param .paths Character. Files to scan; missing ones are reported and skipped.
#' @return Tibble: Key, File, N -- how often each key is cited in each file.
num_cited_keys <- function(.paths) {
  if (FALSE) .paths <- here::here("1_code", c("40-OnlineAppendix-A.qmd", "40-OnlineAppendix-C.qmd"))
  have_ <- .paths[fs::file_exists(.paths)]
  gone_ <- .paths[!fs::file_exists(.paths)]
  if (length(gone_) > 0L) cli::cli_alert_warning("Not scanned, not found: {.file {fs::path_file(gone_)}}.")
  if (length(have_) == 0L) return(tibble::tibble(Key = character(0), File = character(0), N = integer(0)))
  # THE CITATION FORMS: \\pnum{Key} and \\oanum{Key} in a fragment; oa_n("Key"), oa_n(.key = "Key") and met_n("Key") in
  # a qmd.
  pat_ <- paste0(
    "\\\\(?:pnum|oanum)\\{([A-Za-z][A-Za-z0-9]*)\\}",
    "|(?:oa_n|met_n)\\((?:\\.key\\s*=\\s*)?\"([A-Za-z][A-Za-z0-9]*)\"\\)"
  )
  empty_ <- tibble::tibble(Key = character(0), File = character(0), N = integer(0))
  out_ <- purrr::map(have_, \(.p) {
    txt_ <- paste(readLines(.p, warn = FALSE), collapse = "\n")
    m_   <- stringi::stri_match_all_regex(txt_, pat_)[[1L]]
    if (is.null(m_) || nrow(m_) == 0L || all(is.na(m_[, 1L]))) return(NULL)
    keys_ <- dplyr::coalesce(m_[, 2L], m_[, 3L])
    tibble::tibble(Key = keys_, File = fs::path_file(.p)) |>
      dplyr::count(.data$Key, .data$File, name = "N")
  }) |>
    purrr::list_rbind()
  if (nrow(out_) == 0L) return(empty_)
  out_
}

#' Every citation resolves, and every key is cited: the two lists that make the registry honest
#'
#' Reported, and with .strict the first list aborts: a key cited that the registry does not hold would print as
#' ??Key in the paper. The second list -- keys defined and cited nowhere -- is information: a number nobody uses
#' is dead weight in the registry, or a chapter not yet written.
#'
#' @param .tab Tibble from oa_numbers_set().
#' @param .cited Tibble from num_cited_keys().
#' @param .strict Logical. TRUE aborts on a cited key the registry lacks.
#' @return Invisibly, a list: Missing (cited, not defined), Unused (defined, not cited).
num_check_citations <- function(.tab, .cited, .strict) {
  if (FALSE) {
    .tab    <- tab_numbers
    .cited  <- tab_cited
    .strict <- FALSE
  }
  missing_ <- .cited |>
    dplyr::filter(!.data$Key %in% .tab$Key) |>
    dplyr::arrange(.data$Key, .data$File)
  unused_ <- .tab$Key[!.tab$Key %in% .cited$Key]
  if (nrow(missing_) == 0L) {
    n_file_ <- dplyr::n_distinct(.cited$File)
    cli::cli_alert_success("Every cited key is in the registry ({nrow(.cited)} citation{?s} in {n_file_} file{?s}).")
  } else {
    cli::cli_alert_danger("{nrow(missing_)} cited key{?s} the registry does not hold:")
    tbl_say(missing_)
    if (isTRUE(.strict)) cli::cli_abort("Cited keys without a definition; they would print as ??Key.")
  }
  if (length(unused_) == 0L) {
    cli::cli_alert_success("Every registered key is cited somewhere.")
  } else {
    cli::cli_alert_info("{length(unused_)} registered key{?s} cited nowhere yet: {.val {unused_}}.")
  }
  invisible(list(Missing = missing_, Unused = unused_))
}

#' Which tibbles the registry reads, and where each was found
#'
#' One row per distinct exhibit named in the registry: the Data/ directory that holds it (the first hit in the
#' order given, as oa_read_exhibit() takes it) or none. A tibble found in two directories is flagged, because
#' which one wins is then a matter of order rather than of design.
#'
#' @param .spec Tibble. The registry.
#' @param .dir_data Character. The Data/ directories, in the order they are searched.
#' @return Tibble: Exhibit, nKeys, Found (the directory's parent stem or "none"), nHits.
num_report_sources <- function(.spec, .dir_data) {
  if (FALSE) {
    .spec     <- tab_numbers_spec
    .dir_data <- .lP$Input$DirData
  }
  ex_ <- .spec |>
    dplyr::count(.data$Exhibit, name = "nKeys") |>
    dplyr::arrange(.data$Exhibit)
  hits_ <- purrr::map(ex_$Exhibit, \(.e) {
    p_ <- fs::path(.dir_data, paste0(.e, ".parquet"))
    p_[fs::file_exists(p_)]
  })
  ex_ <- ex_ |>
    dplyr::mutate(
      nHits = purrr::map_int(hits_, length),
      Found = purrr::map_chr(hits_, \(.h) {
        if (length(.h) == 0L) return("none")
        fs::path_file(fs::path_dir(fs::path_dir(.h[[1L]])))
      })
    )
  dup_ <- ex_$Exhibit[ex_$nHits > 1L]
  if (length(dup_) > 0L) cli::cli_alert_warning("Found in more than one Data/ directory: {.val {dup_}}.")
  none_ <- ex_$Exhibit[ex_$nHits == 0L]
  if (length(none_) > 0L) cli::cli_alert_warning("Not built anywhere: {.val {none_}}.")
  tbl_say(ex_)
  invisible(ex_)
}


# 4. Deployment ---------------------------------------------------------------------------------------------------------

#' Copy the numbers file to the paper folder, archiving the one it replaces
#'
#' The appendix's oa_deploy_tex() does this for a chapter's fragment and exhibits; the numbers file is one file, so
#' this is the same rule in miniature: copy where the content differs, keep the previous version under Archive/
#' with its modification time in the name, say whether Overleaf needs a new upload.
#'
#' @param .path_numbers Character. Output/Numbers.tex as the last render wrote it.
#' @param .dir_deploy Character. The paper folder the appendix fragments deploy to.
#' @return Invisibly, TRUE where the deployed file changed.
num_deploy <- function(.path_numbers, .dir_deploy) {
  if (FALSE) {
    .path_numbers <- here::here("2_output", "50-Numbers", "Output", "Numbers.tex")
    .dir_deploy   <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "200-Paper_figures",
      "OnlineAppendix"
    )
  }
  if (!fs::file_exists(.path_numbers)) cli::cli_abort("No numbers file at {.file {(.path_numbers)}}: render first.")
  fs::dir_create(.dir_deploy)
  dest_ <- fs::path(.dir_deploy, "Numbers.tex")
  # THE HEADER CARRIES THE RENDER TIME, so the comparison skips it: a file that differs only by its timestamp has
  # nothing new for Overleaf.
  body_ <- function(.p) {
    l_ <- readLines(.p, warn = FALSE)
    l_[!startsWith(l_, "% Numbers.tex -- ")]
  }
  same_ <- fs::file_exists(dest_) && identical(body_(dest_), body_(.path_numbers))
  if (same_) {
    cli::cli_alert_info("{.file Numbers.tex} in the paper folder already matches this render; nothing to upload.")
    return(invisible(FALSE))
  }
  if (fs::file_exists(dest_)) {
    arch_ <- fs::path(.dir_deploy, "Archive")
    fs::dir_create(arch_)
    stamp_ <- format(fs::file_info(dest_)$modification_time, "%Y%m%d-%H%M%S")
    fs::file_copy(
      path      = dest_,
      new_path  = fs::path(arch_, paste0("Numbers-", stamp_, ".tex")),
      overwrite = TRUE
    )
  }
  fs::file_copy(
    path      = .path_numbers,
    new_path  = dest_,
    overwrite = TRUE
  )
  cli::cli_alert_success("{.file Numbers.tex} copied to {.path {(.dir_deploy)}}: upload it to Overleaf.")
  invisible(TRUE)
}


# 5. The register, for the chapters --------------------------------------------------------------------------------

#' Load the register into the appendix state, so a chapter's oa_n() reads what 40 resolved
#'
#' The chapters cite numbers through oa_n(), which reads the "Numbers" entry of the appendix state. Until 40 existed
#' each chapter filled it by resolving its own specification; now it is filled from Numbers.csv, so a chapter renders
#' on 40's last render and computes nothing.
#'
#' @param .path_register Character. 50-Numbers/Output/Numbers.csv.
#' @return Invisibly, the register tibble.
num_numbers_load <- function(.path_register) {
  if (FALSE) .path_register <- here::here("2_output", "50-Numbers", "Output", "Numbers.csv")
  if (!fs::file_exists(.path_register)) {
    cli::cli_abort("No register at {.file {(.path_register)}}: render 50-Numbers first.")
  }
  reg_ <- readr::read_csv(
    file           = .path_register,
    col_types      = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  oa_state_put(
    .what  = "Numbers",
    .value = reg_
  )
  when_ <- format(fs::file_info(.path_register)$modification_time, "%Y-%m-%d %H:%M")
  cli::cli_alert_success("Register loaded: {nrow(reg_)} keys, {sum(reg_$Status == 'ok')} resolved, written {when_}.")
  invisible(reg_)
}


# 6. The data package: the numbers 40B reports about what it published -------------------------------------------- --------------------------------------------------------------------

#' The package numbers as a Key/Value tibble
#'
#' 40B writes package_numbers.csv beside the staged sample: one row per figure of the package (files and bytes per
#' folder, rows per table, spans per kind, documents per text type). Every value is read as a number; the version
#' is kept as text under its own key. Byte counts are also given in gigabytes, rounded to one decimal, since that is
#' the unit the appendix prints.
#'
#' @param .path_numbers Character. 40B's package_numbers.csv.
#' @return Tibble: Key, Value (numeric, NA for the version), Text (the version, NA elsewhere).
oaf_data_package <- function(.path_numbers) {
  if (FALSE) {
    .path_numbers <- here::here("2_output", "40B-PublishData", "Stage", "sample", "package_numbers.csv")
  }
  raw_ <- readr::read_csv(
    file           = .path_numbers,
    col_types      = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  need_ <- c("Key", "Value")
  miss_ <- setdiff(need_, names(raw_))
  if (length(miss_) > 0L) cli::cli_abort("package_numbers.csv lacks {.field {miss_}}.")
  nums_ <- raw_ |>
    dplyr::filter(.data$Key != "package.version") |>
    dplyr::mutate(
      Value = as.numeric(.data$Value),
      Text  = NA_character_
    )
  bad_ <- nums_$Key[is.na(nums_$Value)]
  if (length(bad_) > 0L) cli::cli_abort("Non-numeric package number{?s}: {.val {bad_}}.")
  # GIGABYTES BESIDE THE BYTES, one key per byte key, so the appendix cites the unit it prints.
  gb_ <- nums_ |>
    dplyr::filter(grepl("\\.bytes", .data$Key, fixed = FALSE)) |>
    dplyr::mutate(
      Key   = sub(".bytes", ".gb", .data$Key, fixed = TRUE),
      Value = round(.data$Value / 1e9, digits = 1L)
    )
  ver_ <- raw_ |>
    dplyr::filter(.data$Key == "package.version") |>
    dplyr::transmute(
      Key   = .data$Key,
      Text  = .data$Value,   # read before Value is replaced: .data sees columns as they stand in the call
      Value = NA_real_
    ) |>
    dplyr::select("Key", "Value", "Text")
  dplyr::bind_rows(nums_, gb_, ver_)
}

#' Build the package numbers tibble
#'
#' @param .path_numbers Character. 40B's package_numbers.csv.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaf_build_package <- function(.path_numbers, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_numbers <- here::here("2_output", "40B-PublishData", "Stage", "sample", "package_numbers.csv")
    .dir_own      <- here::here("2_output", "50-Numbers", "Output")
    .force        <- FALSE
    .path_lib     <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "PackageNumbers"
  outs_ <- fs::path(.dir_own, "Data", paste0(name_, ".parquet"))
  ins_  <- c(.path_numbers, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  fs::dir_create(fs::path(.dir_own, "Data"))
  arrow::write_parquet(
    x    = oaf_data_package(.path_numbers = .path_numbers),
    sink = outs_
  )
  invisible("built")
}
