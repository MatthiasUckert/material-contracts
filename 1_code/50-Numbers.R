# 50-Numbers: every number the paper and the online appendix cite, from one file --------------------------------------
#
# WHAT THIS FILE DOES
# The manuscript and the appendix chapters cite numbers -- counts of contracts, shares of filings, the accuracy of a
# model, a median duration. Every one of them is a cell of a tibble some exhibit is printed from: 30's tables and
# figures save theirs under 30/Output/Data, the appendix chapters save theirs under their own Data/. This library
# resolves the whole registry over those tibbles, writes them as one LaTeX file (`Numbers.tex`) the Overleaf project
# inputs, and writes the register beside it so a value can be checked without compiling anything.
#
# ONE WRITER. Until this pair existed each appendix chapter wrote its own OA-Numbers-X.tex, and the manuscript typed
# its numbers, so the intro and Section 4 could -- and did -- disagree on the size of the sample. Now a key is unique
# across the paper, resolved once, and cited as \pnum{Key} in the manuscript and \oanum{Key} in the appendix; both
# names read the same table.
#
# WHAT IS COMPUTED HERE. As little as possible. A number the text needs that no exhibit holds is not a scalar computed
# in the registry; it is a small tibble built here, saved under this pair's Data/, and read like any other. Two such
# tibbles today: the orders restricted to the forms Ahci (2025) covers, and the agreement between orders and text
# markers before the FAST Act.
#
# THE REGISTRY IS THE DOCUMENT. Each row carries the section of the paper the number appears in and a one-line note
# saying what it is in words, and those two columns are what the register prints beside the value.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed locals; .data$ for
# existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure ASCII; parenthesised cli interpolation.

if (FALSE) {
  .path_contracts <- fs::path(
    "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
    "100_Data_Export",
    "Contracts.parquet"
  )
  .path_orders <- fs::path(fs::path_dir(.path_contracts), "CtoOrders.parquet")
  .dir_own     <- here::here("2_output", "50-Numbers", "Output")
  .path_lib    <- here::here("1_code", "50-Numbers.R")
}


# 1. Builders: the two tibbles no exhibit holds -----------------------------------------------------------------------

#' Orders and references on the forms and in the period Ahci (2025) covers
#'
#' The manuscript's footnote compares our order and exhibit counts with Ahci (2025), whose sample is drawn from
#' 10-K, 10-Q and 8-K filings and ends with the FAST Act. The release's CtoOrders is one row per exhibit reference
#' an order makes, with the source form the reference names and the order's own filing date, so both restrictions
#' are filters; the totals of the linkage funnel (chapter A's CtoLinkage) are the unrestricted counts the sentence
#' before the footnote cites.
#'
#' Three counting rules per cell, because the comparison depends on them: every order (Kind "total"), orders that
#' grant including extensions of an earlier grant (Kind "granting", the export's HasCto rule), and grants that are
#' not extensions (Kind "granting-new", the rule the earlier text described as Ahci's). An order is counted where
#' at least one of its references survives the filters, a reference where it names an exhibit number.
#'
#' @param .path_orders Character. The release's CtoOrders.parquet.
#' @param .forms Character. The form types kept; amendments (form/A) are kept with their form.
#' @param .to Integer. The last order year of the restricted period.
#' @return Tibble: Row (form set), Period, Kind, Orders, References, Linked.
num_data_cto_ahci <- function(.path_orders, .forms = c("10-K", "10-Q", "8-K"), .to = 2019L) {
  if (FALSE) {
    .path_orders <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "CtoOrders.parquet"
    )
    .forms <- c("10-K", "10-Q", "8-K")
    .to    <- 2019L
  }
  ds_   <- arrow::open_dataset(sources = .path_orders)
  need_ <- c("OrderDocID", "OrderDate", "Status", "IsExtension", "ExhibitNo", "SourceForm", "DocID")
  miss_ <- setdiff(need_, names(ds_))
  if (length(miss_) > 0L) {
    cli::cli_abort("CtoOrders lacks {.val {miss_}}; the release has {.val {names(ds_)}}.")
  }
  ref_ <- ds_ |>
    dplyr::select(dplyr::all_of(need_)) |>
    dplyr::collect() |>
    dplyr::mutate(
      FormBase = sub("/A$", "", dplyr::coalesce(.data$SourceForm, "")),
      OnForm   = .data$FormBase %in% .forms,
      InPeriod = as.integer(format(as.Date(.data$OrderDate), "%Y")) <= .to,
      HasRef   = !is.na(.data$ExhibitNo),
      Linked   = !is.na(.data$DocID),
      Grants   = dplyr::coalesce(.data$Status == "GRANTING", FALSE),
      NewGrant = .data$Grants & dplyr::coalesce(as.integer(.data$IsExtension), 0L) == 0L
    )
  count_ <- function(.d, .row, .period, .kind) {
    tibble::tibble(
      Row        = .row,
      Period     = .period,
      Kind       = .kind,
      Orders     = dplyr::n_distinct(.d$OrderDocID),
      References = sum(.d$HasRef),
      Linked     = sum(.d$Linked)
    )
  }
  cell_ <- function(.d, .row, .period) {
    dplyr::bind_rows(
      count_(.d = .d, .row = .row, .period = .period, .kind = "total"),
      count_(.d = dplyr::filter(.d, .data$Grants), .row = .row, .period = .period, .kind = "granting"),
      count_(.d = dplyr::filter(.d, .data$NewGrant), .row = .row, .period = .period, .kind = "granting-new")
    )
  }
  to_ <- paste0("through ", .to)
  dplyr::bind_rows(
    cell_(.d = ref_, .row = "All forms", .period = "all years"),
    cell_(.d = dplyr::filter(ref_, .data$InPeriod), .row = "All forms", .period = to_),
    cell_(.d = dplyr::filter(ref_, .data$OnForm), .row = "10-K, 10-Q and 8-K", .period = "all years"),
    cell_(.d = dplyr::filter(ref_, .data$OnForm, .data$InPeriod), .row = "10-K, 10-Q and 8-K", .period = to_)
  )
}

#' Orders against text markers, before the FAST Act
#'
#' The paper identifies redactions through orders until 2018 and through bracketed markers in the text from 2019.
#' Both indicators exist for every year, so the years in which the order was mandatory are a test of the text
#' rule: among unique contracts of 2008-2018 covered by a granted order, how many also carry at least one marker,
#' and among those that carry a marker, how many are covered by an order. The first share is the one the manuscript
#' cites; the second says how much the text rule finds beyond the orders.
#'
#' The marker definition is the published one: symbol and explicit markers, the two kinds that sit above base rate
#' (see the redaction notes in the project). The threshold is an argument, so the five-marker robustness switch is
#' one call away.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .from,.to Integer. The years of the comparison, 2008 to 2018 by default.
#' @param .min_markers Integer. Markers a contract needs to count as redacted by text; 1 is the paper's rule.
#' @return Tibble: Row, N, Share -- the two-by-two and its two conditional shares.
num_data_cto_markers <- function(.path_contracts, .from = 2008L, .to = 2018L, .min_markers = 1L) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .from        <- 2008L
    .to          <- 2018L
    .min_markers <- 1L
  }
  ds_   <- arrow::open_dataset(sources = .path_contracts)
  need_ <- c("DocID", "DateFiled", "PrimaryFiler", "DescSample", "HasCto", "nRedactSymbol", "nRedactExplicit")
  miss_ <- setdiff(need_, names(ds_))
  if (length(miss_) > 0L) {
    cli::cli_abort("Contracts lacks {.val {miss_}}; the release has {.val {names(ds_)}}.")
  }
  tab_ <- ds_ |>
    dplyr::select(dplyr::all_of(need_)) |>
    dplyr::filter(.data$PrimaryFiler == 1L, .data$DescSample == 1L) |>
    dplyr::collect() |>
    dplyr::mutate(
      Year    = as.integer(format(as.Date(.data$DateFiled), "%Y")),
      Cto     = dplyr::coalesce(as.integer(.data$HasCto), 0L) == 1L,
      Markers = dplyr::coalesce(.data$nRedactSymbol, 0L) + dplyr::coalesce(.data$nRedactExplicit, 0L),
      Text    = .data$Markers >= .min_markers
    ) |>
    dplyr::filter(.data$Year >= .from, .data$Year <= .to)
  n_cto_  <- sum(tab_$Cto)
  n_text_ <- sum(tab_$Text)
  n_both_ <- sum(tab_$Cto & tab_$Text)
  tibble::tibble(
    Row   = c("Unique contracts in the window", "Covered by a granted order", "With a text marker", "Both",
              "Order only", "Marker only", "Neither",
              "Share of ordered contracts with a marker", "Share of marked contracts with an order"),
    N     = c(nrow(tab_), n_cto_, n_text_, n_both_, n_cto_ - n_both_, n_text_ - n_both_,
              nrow(tab_) - n_cto_ - n_text_ + n_both_, NA_integer_, NA_integer_),
    Share = c(rep(NA_real_, 7L),
              if (n_cto_ > 0L) n_both_ / n_cto_ else NA_real_,
              if (n_text_ > 0L) n_both_ / n_text_ else NA_real_)
  )
}

#' Build one of this pair's tibbles, where its inputs or this library are newer than the parquet
#'
#' The layout is the chapters': Data/<Name>.parquet under this pair's output directory, which is what
#' oa_read_exhibit() and the registry read. The build test is the appendix's, oa_build_needed(), so a changed
#' release or a changed builder rebuilds and nothing else does.
#'
#' @param .name Character. The tibble's name, and the file stem.
#' @param .fun Function. The builder; called with no arguments (wrap the paths in a closure).
#' @param .inputs Character. The files the tibble depends on, this library among them.
#' @param .dir_own Character. This pair's output directory.
#' @param .force Logical. TRUE rebuilds regardless.
#' @return Invisibly, "built" or "up to date".
num_build <- function(.name, .fun, .inputs, .dir_own, .force) {
  if (FALSE) {
    .name    <- "CtoAhci"
    .fun     <- \() num_data_cto_ahci(.path_orders = .path_orders)
    .inputs  <- c(.path_orders, .path_lib)
    .dir_own <- here::here("2_output", "50-Numbers", "Output")
    .force   <- FALSE
  }
  out_ <- fs::path(.dir_own, "Data", paste0(.name, ".parquet"))
  if (!oa_build_needed(.outputs = out_, .inputs = .inputs, .force = .force)) return(invisible("up to date"))
  fs::dir_create(fs::path_dir(out_))
  arrow::write_parquet(
    x    = .fun(),
    sink = out_
  )
  invisible("built")
}


# 2. The registry: what a row must carry, and how the sections are ordered ---------------------------------------------

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


# 3. The numbers file and the register -------------------------------------------------------------------------------

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


# 4. Checks: what cites what ------------------------------------------------------------------------------------------

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


# 5. Deployment ---------------------------------------------------------------------------------------------------------

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


# 6. The register, for the chapters --------------------------------------------------------------------------------

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



# ====================================================================================================================
# Appendix A -- filing requirements and distributions: the builders moved here from 42-OnlineAppendix-A, unchanged; they write into 40's Output/
# ====================================================================================================================

# ======================================================================================================================
# 40-OnlineAppendix-A.R -- Appendix A, SEC filing requirements and distributions: the exhibits this chapter builds
# ======================================================================================================================
#
# 40-OnlineAppendix-A.qmd holds the chapter's text; _Commons/_OnlineAppendix.R holds everything the chapters share.
# This library holds only the builders of the exhibits Appendix A writes itself, in 30's layout under this chapter's
# output directory: the coverage table, the within-year figure, the confidential-treatment linkage funnel, and the
# counts the text cites that no table prints. Two exhibits of the response memo are built here too, because their
# data is this chapter's -- the exhibit-files table behind the 2001 start, and the seasoned-filer figure -- and the
# memo pair sets them from here.
#
# THE PREFIX IS oaa_: online appendix, chapter A. Functions are compute (a tibble, no printing), plot (a ggplot) or
# build (writes the exhibit's files where its inputs are newer, returns the status invisibly).


# 1. The sample, as 30 reads it ---------------------------------------------------------------------------------------

#' The unique contracts of the descriptive sample, as 30 selects them
#'
#' The one reader of the release every build here shares, so all of them count exactly the paper's sample: a
#' contract is kept where DescSample is 1 and it is the primary copy of its attachment -- 30's Keep. The flags are
#' compared after the collect, where an integer and a logical flag compare alike; in Arrow they need not.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .cols Character. The columns to keep besides the flags.
#' @return Tibble: .cols, plus Date and Year, for 2001-2024.
oaa_read_sample <- function(.path_contracts, .cols) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .cols <- c("FormType", "DateFiled")
  }
  arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::all_of(unique(c(.cols, "DateFiled", "PrimaryFiler", "DescSample")))) |>
    dplyr::collect() |>
    dplyr::filter(
      dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      dplyr::coalesce(as.integer(.data$DescSample), 0L) == 1L
    ) |>
    dplyr::mutate(
      Date = as.Date(.data$DateFiled),
      Year = as.integer(format(.data$Date, "%Y"))
    ) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::select(-"PrimaryFiler", -"DescSample")
}

# 2. Coverage: every form that carries material contracts, and what the database holds of it -----------------------

#' The coverage groups: every EDGAR form type that carries material contracts, and whether the database covers it
#'
#' One row per form type. Panel A lists the forms the database draws on, Panel B the forms outside its scope. The
#' table states what the database and the master index hold: filings for every form, and contracts only for the
#' forms the database downloaded -- the contracts of forms outside the scope were never downloaded, so they are not
#' counted and not estimated. Form 20-F sits in Panel A: the database holds the contracts its filers number as
#' Exhibit 10, while the form's own instructions list material contracts as Exhibit 4, which the note says.
#'
#' @return Tibble: FormType, Panel, Group.
oaa_coverage_forms <- function() {
  a_ <- "In the database"
  b_ <- "Outside it"
  tibble::tribble(
    ~FormType,   ~Panel, ~Group,
    "10-K",      a_,     "Annual reports",
    "10-K/A",    a_,     "Annual reports",
    "10-Q",      a_,     "Quarterly reports",
    "10-Q/A",    a_,     "Quarterly reports",
    "8-K",       a_,     "Current reports",
    "8-K/A",     a_,     "Current reports",
    "S-1",       a_,     "Registration statements",
    "S-1/A",     a_,     "Registration statements",
    "S-4",       a_,     "Registration statements",
    "S-4/A",     a_,     "Registration statements",
    "F-1",       a_,     "Registration statements",
    "F-1/A",     a_,     "Registration statements",
    "F-4",       a_,     "Registration statements",
    "F-4/A",     a_,     "Registration statements",
    "20-F",      a_,     "Foreign annual reports",
    "20-F/A",    a_,     "Foreign annual reports",
    "10QSB",     b_,     "Small business, periodic",
    "10QSB/A",   b_,     "Small business, periodic",
    "10KSB",     b_,     "Small business, periodic",
    "10KSB/A",   b_,     "Small business, periodic",
    "10KSB40",   b_,     "Small business, periodic",
    "10KSB40/A", b_,     "Small business, periodic",
    "SB-2",      b_,     "Small business, registration",
    "SB-2/A",    b_,     "Small business, registration",
    "SB-1",      b_,     "Small business, registration",
    "SB-1/A",    b_,     "Small business, registration",
    "10SB12G",   b_,     "Small business, registration",
    "10SB12G/A", b_,     "Small business, registration",
    "10SB12B",   b_,     "Small business, registration",
    "10SB12B/A", b_,     "Small business, registration",
    "10-K405",   b_,     "10-K variants",
    "10-K405/A", b_,     "10-K variants",
    "10KT405",   b_,     "10-K variants",
    "10-KT",     b_,     "10-K variants",
    "10-KT/A",   b_,     "10-K variants",
    "10-12G",    b_,     "Exchange Act registrations",
    "10-12G/A",  b_,     "Exchange Act registrations",
    "10-12B",    b_,     "Exchange Act registrations",
    "10-12B/A",  b_,     "Exchange Act registrations",
    "S-11",      b_,     "Real-estate registrations",
    "S-11/A",    b_,     "Real-estate registrations"
  )
}

#' The coverage table's data: filings per group, and the contracts the database holds
#'
#' @param .dir_master Character. The master index's parquet directory.
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Panel, Group, Forms, Years, Filings, Contracts, Basis, Kind (group, total or design); one row per
#'   group, a total per panel, and one row for the forms out by design. Contracts is missing outside the database.
oaa_data_coverage <- function(.dir_master, .path_contracts) {
  if (FALSE) {
    .dir_master <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  forms_ <- oaa_coverage_forms()
  # FILINGS per form type, 2001-2024, from the master index
  fil_ <- arrow::open_dataset(sources = .dir_master) |>
    dplyr::filter(.data$FormType %in% forms_$FormType) |>
    dplyr::select("FormType", "DateFiled") |>
    dplyr::collect() |>
    dplyr::mutate(Year = as.integer(format(as.Date(.data$DateFiled), "%Y"))) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::summarise(Filings = dplyr::n(), First = min(.data$Year), Last = max(.data$Year), .by = "FormType")
  # CONTRACTS the database holds: unique contracts of the descriptive sample, per form type
  held_ <- oaa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = "FormType"
  ) |>
    dplyr::count(.data$FormType, name = "Held")
  # THE FORMS OF A GROUP, in the order the list above gives them, amendments folded into one mention
  label_ <- function(.f) {
    orig_ <- .f[!grepl("/A$", .f)]
    paste0(paste(orig_, collapse = ", "), if (any(grepl("/A$", .f))) " (with /A)" else "")
  }
  out_ <- forms_ |>
    dplyr::left_join(fil_, by = "FormType") |>
    dplyr::left_join(held_, by = "FormType") |>
    dplyr::mutate(Filings = dplyr::coalesce(.data$Filings, 0L)) |>
    dplyr::summarise(
      Forms     = label_(.f = .data$FormType),
      Years     = paste0(min(.data$First, na.rm = TRUE), "-", max(.data$Last, na.rm = TRUE)),
      Filings   = sum(.data$Filings),
      Contracts = if (dplyr::first(.data$Panel) == "In the database") sum(.data$Held, na.rm = TRUE) else NA_real_,
      .by       = c("Panel", "Group")
    ) |>
    dplyr::mutate(
      Contracts = as.numeric(.data$Contracts),
      Basis     = dplyr::case_when(
        .data$Group == "Foreign annual reports" ~ "The release; Exhibit 10 only",
        .data$Panel == "In the database"        ~ "The release",
        .default                                = "Not downloaded"
      ),
      Kind = "group"
    )
  totals_ <- out_ |>
    dplyr::summarise(Filings = sum(.data$Filings), Contracts = sum(.data$Contracts), .by = "Panel") |>
    dplyr::mutate(Group = "Total", Forms = "", Years = "", Basis = "", Kind = "total")
  design_ <- tibble::tibble(
    Panel = "Out by design", Group = "Asset-backed issuers; foreign current reports",
    Forms = "SF-1, SF-3, 10-D; 6-K", Years = "", Filings = NA_integer_, Contracts = NA_real_,
    Basis = "Not operating firms; no exhibit numbering", Kind = "design"
  )
  dplyr::bind_rows(
    dplyr::filter(out_, .data$Panel == "In the database"),
    dplyr::filter(totals_, .data$Panel == "In the database"),
    dplyr::filter(out_, .data$Panel == "Outside it"),
    dplyr::filter(totals_, .data$Panel == "Outside it"),
    design_
  )
}

#' Build the coverage table
#'
#' @param .dir_master Character. The master index's parquet directory.
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_coverage <- function(.dir_master, .path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_master <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "FormCoverage"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.dir_master, .path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_coverage(
    .dir_master     = .dir_master,
    .path_contracts = .path_contracts
  )
  num_ <- function(.x) dplyr::if_else(is.na(.x), "", format(.x, big.mark = ",", trim = TRUE, scientific = FALSE))
  cells_ <- tab_ |>
    dplyr::transmute(
      Panel     = .data$Panel,
      Group     = oa_tex_escape(.x = .data$Group),
      Forms     = oa_tex_escape(.x = .data$Forms),
      Years     = gsub("-", "--", .data$Years, fixed = TRUE),
      Filings   = num_(.x = .data$Filings),
      Contracts = dplyr::if_else(.data$Panel == "Outside it", "--", num_(.x = .data$Contracts)),
      Basis     = oa_tex_escape(.x = .data$Basis)
    )
  tot_ <- tab_$Kind == "total"
  cells_[tot_, c("Group", "Filings", "Contracts")] <- lapply(cells_[tot_, c("Group", "Filings", "Contracts")],
                                                             \(.x) paste0("\\textbf{", .x, "}"))
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Group", "EDGAR form types", "Years", "Filings", "Contracts", "Basis"),
      .spec   = c(oa_col_text(.share = 0.19), oa_col_text(.share = 0.21), "l",
                  oa_col_num(.mm = 16), oa_col_num(.mm = 16), oa_col_text(.share = 0.15))
    ),
    .note    = paste(
      "Filings are counted in EDGAR's master index, 2001-2024; contracts are the unique contracts of the descriptive",
      "sample in the release. The forms outside the database were not downloaded, so their contracts are not",
      "counted. Small business issuers reported under Regulation S-B, whose Item 601 required material contracts",
      "as Exhibit 10 in the same way as Regulation S-K; the SB forms were phased out after SEC Release 33-8876 took",
      "effect in February 2008. Form 20-F lists material contracts as Exhibit 4 (Instruction 4(a) of its",
      "Instructions as to Exhibits); the database holds the 20-F contracts that filers number as Exhibit 10."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

# 3. Within the year: when contracts are filed ---------------------------------------------------------------------

#' The within-year figure's data: the share of each report type's contracts filed on each calendar day
#'
#' Unique contracts of the descriptive sample (30's Keep), 2001-2024. For every report type and year, the share of
#' that year's contracts filed on each day; the figure shows the mean over years, so every year counts equally.
#' February 29 is folded into February 28, so all years share one calendar; days without a filing count as zero.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Group, Day (a date of 2025, standing for the calendar day), Share (percent).
oaa_data_within_year <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  groups_ <- tibble::tribble(
    ~FormType, ~Group,
    "8-K",     "Current reports (8-K)",
    "8-K/A",   "Current reports (8-K)",
    "10-K",    "Annual reports (10-K)",
    "10-K/A",  "Annual reports (10-K)",
    "10-Q",    "Quarterly reports (10-Q)",
    "10-Q/A",  "Quarterly reports (10-Q)"
  )
  con_ <- oaa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = "FormType"
  ) |>
    dplyr::inner_join(groups_, by = "FormType") |>
    dplyr::mutate(MD = sub("^02-29$", "02-28", format(.data$Date, "%m-%d")))
  days_ <- format(seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"), "%m-%d")
  grid_ <- tidyr::expand_grid(Group = unique(groups_$Group), Year = 2001L:2024L, MD = days_)
  con_ |>
    dplyr::count(.data$Group, .data$Year, .data$MD, name = "N") |>
    dplyr::right_join(grid_, by = c("Group", "Year", "MD")) |>
    dplyr::mutate(N = dplyr::coalesce(.data$N, 0L)) |>
    dplyr::mutate(Share = 100 * .data$N / sum(.data$N), .by = c("Group", "Year")) |>
    dplyr::filter(!is.na(.data$Share)) |>
    dplyr::summarise(Share = mean(.data$Share), .by = c("Group", "MD")) |>
    dplyr::mutate(
      Day   = as.Date(paste0("2025-", .data$MD)),
      Group = factor(.data$Group, levels = unique(groups_$Group))
    ) |>
    dplyr::select("Group", "Day", "Share") |>
    dplyr::arrange(.data$Group, .data$Day)
}

#' The within-year figure: one panel per report type, one scale, the filing windows shaded
#'
#' @param .tab Tibble from oaa_data_within_year().
#' @return A ggplot.
oaa_plot_within_year <- function(.tab) {
  if (FALSE) .tab <- oaa_data_within_year(.path_contracts = "Contracts.parquet")
  win_ <- tibble::tibble(
    Start = as.Date(c("2025-01-01", "2025-04-01", "2025-07-01", "2025-10-01")),
    End   = as.Date(c("2025-03-31", "2025-05-15", "2025-08-14", "2025-11-14")),
    Label = c("10-K window", "10-Q window", "10-Q window", "10-Q window"),
    Group = factor(levels(.tab$Group)[1L], levels = levels(.tab$Group))
  )
  top_ <- max(.tab$Share) * 1.08
  ggplot2::ggplot(.tab, ggplot2::aes(x = .data$Day, y = .data$Share)) +
    ggplot2::geom_rect(
      data        = dplyr::select(win_, -"Group"),
      mapping     = ggplot2::aes(xmin = .data$Start, xmax = .data$End, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill        = "#EDEDED"
    ) +
    ggplot2::geom_text(
      data        = win_,
      mapping     = ggplot2::aes(x = .data$Start + (.data$End - .data$Start) / 2, y = top_, label = .data$Label),
      inherit.aes = FALSE,
      size        = 3.1,
      colour      = "#595959",
      family      = .plot_font,
      vjust       = 1
    ) +
    ggplot2::geom_line(colour = plot_pal_cat(.n = 1L), linewidth = 0.35) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Group), ncol = 1L) +
    ggplot2::scale_x_date(
      breaks = seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "month"),
      labels = \(.d) format(.d, "%b"),
      expand = c(0.005, 0.005)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, top_), expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Share of the year's contracts filed on the day, in %") +
    plot_theme(.grid = "y", .legend = "none") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

#' Build the within-year figure
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_within_year <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own        <- here::here("2_output", "50-Numbers", "Output")
    .force          <- FALSE
    .path_lib       <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "FilingsWithinYear"
  outs_ <- c(fs::path(.dir_own, "Figures", paste0(name_, c(".pdf", ".png"))),
             fs::path(.dir_own, c("Notes", "Data"), paste0(name_, c(".tex", ".parquet"))))
  ins_  <- c(.path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_within_year(.path_contracts = .path_contracts)
  oa_write_exhibit(
    .name    = name_,
    .lines   = NULL,
    .note    = paste(
      "Unique contracts, 2001-2024, filed with current reports (8-K), annual reports (10-K) and quarterly reports",
      "(10-Q), each with its amendments. For each report type and year, the share of that year's contracts filed on",
      "each calendar day; the lines show the mean over years. Shaded are the filing windows of December year-end",
      "filers: 90 days after the fiscal year's end for the 10-K and 45 days after each quarter's end for the 10-Q.",
      "Within them, the deadlines of 60, 75 and 90 days (10-K) and 40 and 45 days (10-Q) differ by filer status."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  plot_save(
    .plot   = oaa_plot_within_year(.tab = tab_),
    .name   = name_,
    .dir    = fs::path(.dir_own, "Figures"),
    .height = 5.2
  )
  invisible("built")
}

# 4. Confidential treatment orders: from the order to the contract it covers ---------------------------------------

#' The linkage funnel's data: every reference an order makes, by where it ends
#'
#' The release's CtoOrders.parquet holds one row per reference an order makes to an exhibit, and LinkStatus records
#' where each reference ended: linked to the attachment it names, or failed at one of nine named points in the chain
#' from the order's text to the database. The codes are numbered in chain order, which is the order the table keeps.
#' A linked reference is then classified by what the order does -- grants, extends, denies, revokes -- because the
#' paper's redaction measure counts grants and their extensions and not denials. The last row is the release's own
#' flag on the unique contracts of the sample, so the funnel ends where the paper's variable begins.
#'
#' @param .path_orders Character. The release's CtoOrders.parquet.
#' @param .path_contracts Character. The release's Contracts.parquet, for the last row.
#' @return Tibble: Panel, Step, Code, N, Share (of references), Kind (total / item / result).
oaa_data_cto <- function(.path_orders, .path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_orders <- fs::path(fs::path_dir(.path_contracts), "CtoOrders.parquet")
  }
  ref_ <- arrow::open_dataset(sources = .path_orders) |>
    dplyr::select(dplyr::any_of(c("OrderDocID", "LinkStatus", "Status", "IsExtension"))) |>
    dplyr::collect()
  n_ref_ <- nrow(ref_)
  # THE FAILURE POINTS, in chain order, in words. The code is the release's own; the words are the table's.
  steps_ <- tibble::tribble(
    ~Code,       ~Step,
    "A-linked",  "Linked to the attachment it names",
    "1-",        "No exhibit reference could be parsed from the order",
    "2-",        "The reference is not to an Exhibit 10",
    "3-",        "The order does not name the filing the exhibit came with",
    "4-",        "The named filing is not in EDGAR's index",
    "5-",        "The named filing matches several filings",
    "6-",        "No exhibit of the named filing was downloaded (a form outside the frame)",
    "7-",        "The exhibit number matches several attachments of the filing",
    "8-",        "The exhibit number is lettered and cannot be matched",
    "9-",        "The exhibit number is not among the attachments downloaded"
  )
  code_ <- dplyr::case_when(
    ref_$LinkStatus == "A-linked" ~ "A-linked",
    .default = paste0(stringi::stri_sub(ref_$LinkStatus, 1L, 1L), "-")
  )
  unknown_ <- setdiff(unique(code_), steps_$Code)
  if (length(unknown_) > 0L) cli::cli_abort("LinkStatus code{?s} the funnel does not name: {.val {unknown_}}.")
  by_code_ <- tibble::tibble(Code = code_) |>
    dplyr::count(.data$Code, name = "N")
  panel_a_ <- steps_ |>
    dplyr::left_join(by_code_, by = "Code") |>
    dplyr::mutate(N = dplyr::coalesce(.data$N, 0L), Kind = "item")
  linked_ <- ref_[ref_$LinkStatus == "A-linked", , drop = FALSE]
  ext_    <- dplyr::coalesce(as.integer(linked_$IsExtension), 0L) == 1L
  st_     <- linked_$Status
  panel_b_ <- tibble::tibble(
    Code = c("", "", "", "", ""),
    Step = c("Grants confidential treatment", "Extends an earlier grant", "Denies the request, in whole or in part",
             "Revokes an earlier grant", "Order text not parsed, status unknown"),
    N    = c(
      sum(st_ == "GRANTING" & !ext_, na.rm = TRUE),
      sum(st_ == "GRANTING" & ext_, na.rm = TRUE),
      sum(st_ == "DENYING", na.rm = TRUE),
      sum(st_ == "REVOKING", na.rm = TRUE),
      sum(is.na(st_))
    ),
    Kind = "item"
  )
  # THE LAST ROW: the release's flag, on the unique contracts of the descriptive sample
  con_ <- oaa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = "HasCto"
  )
  n_cto_ <- sum(dplyr::coalesce(as.integer(con_$HasCto), 0L) == 1L)
  dplyr::bind_rows(
    tibble::tibble(Panel = "A. References, by where they end", Code = "", Step = "Orders in the release",
                   N = dplyr::n_distinct(ref_$OrderDocID), Kind = "total"),
    tibble::tibble(Panel = "A. References, by where they end", Code = "", Step = "References to exhibits they make",
                   N = n_ref_, Kind = "total"),
    dplyr::mutate(panel_a_, Panel = "A. References, by where they end"),
    tibble::tibble(Panel = "B. Linked references, by what the order does", Code = "", Step = "Linked references",
                   N = nrow(linked_), Kind = "total"),
    dplyr::mutate(panel_b_, Panel = "B. Linked references, by what the order does"),
    tibble::tibble(Panel = "C. In the sample", Code = "",
                   Step = "Unique contracts of the descriptive sample with a grant or an extension",
                   N = n_cto_, Kind = "result")
  ) |>
    dplyr::mutate(Share = dplyr::if_else(.data$Panel == "C. In the sample", NA_real_, .data$N / n_ref_)) |>
    dplyr::select("Panel", "Step", "Code", "N", "Share", "Kind")
}

#' Build the linkage funnel
#'
#' @param .path_orders Character. The release's CtoOrders.parquet.
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_cto <- function(.path_orders, .path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_orders <- fs::path(fs::path_dir(.path_contracts), "CtoOrders.parquet")
    .dir_own     <- here::here("2_output", "50-Numbers", "Output")
    .force       <- FALSE
    .path_lib    <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "CtoLinkage"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.path_orders, .path_contracts, .path_lib)
  if (!fs::file_exists(.path_orders)) return(invisible("waiting for CtoOrders.parquet in the release"))
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_cto(
    .path_orders    = .path_orders,
    .path_contracts = .path_contracts
  )
  num_ <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)
  pct_ <- function(.x) dplyr::if_else(is.na(.x), "", formatC(100 * .x, format = "f", digits = 1L))
  cells_ <- tab_ |>
    dplyr::transmute(
      Panel = .data$Panel,
      Step  = dplyr::if_else(.data$Kind == "item", paste0("\\hspace{1em}", oa_tex_escape(.x = .data$Step)),
                             oa_tex_escape(.x = .data$Step)),
      N     = num_(.x = .data$N),
      Share = pct_(.x = .data$Share)
    )
  tot_ <- tab_$Kind %in% c("total", "result")
  cells_[tot_, c("Step", "N")] <- lapply(cells_[tot_, c("Step", "N")], \(.x) paste0("\\textbf{", .x, "}"))
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", "N", "\\% of references"),
      .spec   = c(oa_col_text(.share = 0.66), oa_col_num(.mm = 18), oa_col_num(.mm = 22))
    ),
    .note    = paste(
      "Every confidential treatment order EDGAR lists (form type CT ORDER, from May 2008) is parsed for the exhibits",
      "it covers; an order identifies an exhibit by the filing it came with and its exhibit number, and each such",
      "reference is followed to the attachment in the database. Panel A counts references by where they end, in",
      "the order the chain is followed. Panel B classifies the linked references by what the order does. Panel C",
      "counts the unique contracts of the descriptive sample the release flags as covered by a grant or an",
      "extension of a grant; denials and revocations are not counted, following Ahci (2025). Shares are of all",
      "references."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# 5. The counts the text cites, which no table prints ----------------------------------------------------------------

#' The counts the text of Appendix A cites, which no table prints
#'
#' Passages of the text name numbers that belong to no exhibit: how many rows the release holds and how many of
#' them are unique attachments, how many of those the sample keeps, how many predate 2001, and what each quality
#' rule flags. They are computed here on the release, written as a tibble like any other exhibit's data, and read by
#' the numbers spec; nothing is printed.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Key, Value.
oaa_data_counts <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  all_ <- arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::any_of(c("DocID", "DateFiled", "PrimaryFiler", "DescSample", "SampleStepCode", "Removed",
                                  "RemClass", "nWords"))) |>
    dplyr::collect() |>
    dplyr::mutate(
      Primary = dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      Sample  = dplyr::coalesce(as.integer(.data$DescSample), 0L) == 1L,
      Flagged = dplyr::coalesce(as.integer(.data$Removed), 0L) == 1L,
      Year    = as.integer(format(as.Date(.data$DateFiled), "%Y"))
    )
  uni_ <- dplyr::filter(all_, .data$Primary)
  # THE RULES: RemClass names the rule that flagged a document, prefixed 1-, 2- or 3-; a flagged document without a
  # rule is a placeholder that carries no text (the PDF and image references folded into the malformatted rung).
  rule_ <- uni_ |>
    dplyr::filter(.data$Flagged, !is.na(.data$RemClass)) |>
    dplyr::count(.data$RemClass, name = "N")
  pick_ <- function(.prefix) {
    row_ <- rule_[startsWith(rule_$RemClass, .prefix), , drop = FALSE]
    if (nrow(row_) == 0L) return(0)
    sum(row_$N)
  }
  n_flag_ <- sum(uni_$Flagged)
  n_rule_ <- pick_(.prefix = "1-") + pick_(.prefix = "2-") + pick_(.prefix = "3-")
  # TWO GRAINS. DocsPre2001 and DocsFlagged count primary copies (uni_), which is what Appendix A's text cites.
  # Appendix F describes the released file row by row, so RowsPre2001 and RowsFlagged count every row (all_):
  # RowsPre2001 is what separates the file's row count from the manuscript's 2001-2024 count. DocsSample2008 is the
  # unique-contract sample from 2008, the sample of the redaction figure.
  tibble::tibble(
    Key = c("DocsAll", "DocsUnique", "DocsSample", "DocsPre2001", "DocsFlagged", "RuleShort", "RuleStopwords",
            "RuleNumeric", "Placeholders", "YearFirst", "YearLast", "RowsPre2001", "RowsFlagged",
            "DocsSample2008"),
    Value = c(
      nrow(all_),
      nrow(uni_),
      sum(uni_$Sample),
      sum(uni_$Year < 2001L, na.rm = TRUE),
      n_flag_,
      pick_(.prefix = "1-"),
      pick_(.prefix = "2-"),
      pick_(.prefix = "3-"),
      max(n_flag_ - n_rule_, 0),
      min(uni_$Year[uni_$Sample], na.rm = TRUE),
      max(uni_$Year[uni_$Sample], na.rm = TRUE),
      sum(all_$Year < 2001L, na.rm = TRUE),
      sum(all_$Flagged),
      sum(uni_$Sample & uni_$Year >= 2008L, na.rm = TRUE)
    )
  )
}

#' Build the counts the text cites
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_counts <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "TextCounts"
  outs_ <- fs::path(.dir_own, "Data", paste0(name_, ".parquet"))
  ins_  <- c(.path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  fs::dir_create(fs::path(.dir_own, "Data"))
  arrow::write_parquet(
    x    = oaa_data_counts(.path_contracts = .path_contracts),
    sink = outs_
  )
  invisible("built")
}


# 6. For the response memo: the exhibit files behind the 2001 start, and the seasoned filers -----------------------
# Built here because their data is this chapter's; registered under Section "M", so the runbook shows them and the
# fragment leaves them out. The memo pair sets them from this chapter's output directory.

#' The exhibit-files table's data: per quarter, the Exhibit 10s EDGAR's index lists, and those that are files
#'
#' EDGAR's index page lists the documents of a filing back to 1993, read from the tags of the complete submission,
#' but only a filing submitted through the modernised system disseminates each document as a file of its own. In
#' 01B's links table the difference is one column: a listed exhibit that is a file carries a Document name, and one
#' that is not carries NA and an address ending at the filer's folder. The table counts both, by quarter, across the
#' years in which the change happened.
#'
#' @param .dir_links Character. 01A's mirrored links table, the directory of one parquet per quarter.
#' @param .from Numeric. The first year-quarter, as 01A writes it (1998.1).
#' @param .to Numeric. The last year-quarter, inclusive.
#' @return Tibble: YearQuarter, Quarter (as text), Listed, WithFile, ShareFile.
oaa_data_files <- function(.dir_links, .from, .to) {
  if (FALSE) {
    .dir_links <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$DocLinks$DirMain$Links
    .from <- 1998.1
    .to   <- 2001.2
  }
  arrow::open_dataset(sources = .dir_links) |>
    dplyr::filter(.data$YearQuarter >= .from, .data$YearQuarter <= .to) |>
    dplyr::select("YearQuarter", "Type", "Document") |>
    dplyr::collect() |>
    dplyr::filter(grepl("^EX-?10", toupper(.data$Type))) |>
    dplyr::summarise(
      Listed   = dplyr::n(),
      WithFile = sum(!is.na(.data$Document)),
      .by      = "YearQuarter"
    ) |>
    dplyr::mutate(
      ShareFile = .data$WithFile / .data$Listed,
      Quarter   = paste0(floor(.data$YearQuarter), " Q", round(10 * (.data$YearQuarter - floor(.data$YearQuarter))))
    ) |>
    dplyr::arrange(.data$YearQuarter) |>
    dplyr::select("YearQuarter", "Quarter", "Listed", "WithFile", "ShareFile")
}

#' Build the exhibit-files table
#'
#' @param .dir_links Character. 01A's mirrored links table.
#' @param .from Numeric. The first year-quarter.
#' @param .to Numeric. The last year-quarter.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_files <- function(.dir_links, .from, .to, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_links <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$DocLinks$DirMain$Links
    .from     <- 1998.1
    .to       <- 2001.2
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "ExhibitFiles"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.dir_links, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_files(
    .dir_links = .dir_links,
    .from      = .from,
    .to        = .to
  )
  num_ <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)
  cells_ <- tab_ |>
    dplyr::transmute(
      Quarter   = .data$Quarter,
      Listed    = num_(.x = .data$Listed),
      WithFile  = num_(.x = .data$WithFile),
      ShareFile = formatC(100 * .data$ShareFile, format = "f", digits = 1L)
    )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Quarter", "Exhibit 10s listed", "Of which files", "\\%"),
      .spec   = c("l", oa_col_num(.mm = 24), oa_col_num(.mm = 22), oa_col_num(.mm = 14))
    ),
    .note    = paste(
      "From the document lists of EDGAR's filing index pages, mirrored for every filing of the eight forms the",
      "database covers, 1998 Q1 to 2001 Q2. Listed counts the attachments whose exhibit type begins EX-10; of which",
      "files counts those that carry a file name and address of their own on the index page. An exhibit listed",
      "without a file exists only inside the filing's complete submission text file."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' The seasoned-filer figure's data: four groups of filers, per year
#'
#' All filers; seasoned filers by 30's rule -- more than two years (2 x 365.25 days) past the CIK's first contract in
#' the sample, which leaves firms already filing in 2001 unseasoned until 2003; seasoned filers by the CIK's first
#' EDGAR filing of any form, from the master index, which starts in 1993 and does not date a firm by the filings
#' under study; and the contracts of blank-check registrants, the filings whose SIC code is 6770, from 01A's landing
#' pages. For each group and year: contracts, those filed with a registration statement (S-1, S-4, F-1, F-4 and
#' their amendments), and the equity contracts. The label is the release's Class, the crowned engine's detailed
#' label under a name that does not depend on the engine, which is also what 30 reads.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .path_landing Character. 01A's LandingPageAll.parquet, for the SIC code of each filing.
#' @param .dir_master Character. The master index's parquet directory.
#' @return Tibble: Year, Filers, N, NReg, ShareReg, NEquity, ShareEquity.
oaa_data_seasoned <- function(.path_contracts, .path_landing, .dir_master) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_landing <- here::here("2_output", "01A-EdgarIndex", "Output", "LandingPageAll.parquet")
    .dir_master   <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
  }
  pad_ <- function(.x) sprintf("%010.0f", as.numeric(.x))
  con_ <- oaa_read_sample(
    .path_contracts = .path_contracts,
    .cols           = c("CIK", "HashIndex", "FormType", "Class")
  ) |>
    dplyr::mutate(
      CIK      = pad_(.x = .data$CIK),
      IsReg    = sub("/A$", "", .data$FormType) %in% c("S-1", "S-4", "F-1", "F-4"),
      IsEquity = dplyr::coalesce(.data$Class == "Financial Instruments: Equity", FALSE)
    )
  if (!any(con_$IsEquity)) {
    cli::cli_abort("No contract's {.field Class} is {.val Financial Instruments: Equity}; the release's labels differ.")
  }
  # 30'S RULE: the first contract of the CIK in the sample
  con_ <- con_ |>
    dplyr::mutate(Entry = min(.data$Date), .by = "CIK") |>
    dplyr::mutate(Seasoned = as.numeric(.data$Date - .data$Entry) / 365.25 > 2)
  # THE CHECK: the first EDGAR filing of the CIK, any form, from the master index
  first_ <- arrow::open_dataset(sources = .dir_master) |>
    dplyr::select("CIK", "DateFiled") |>
    dplyr::mutate(Date = as.Date(.data$DateFiled)) |>
    dplyr::group_by(.data$CIK) |>
    dplyr::summarise(First = min(.data$Date, na.rm = TRUE)) |>
    dplyr::collect() |>
    dplyr::mutate(CIK = pad_(.x = .data$CIK)) |>
    dplyr::summarise(First = min(.data$First), .by = "CIK")
  con_ <- con_ |>
    dplyr::left_join(first_, by = "CIK") |>
    dplyr::mutate(SeasonedEdgar = dplyr::coalesce(as.numeric(.data$Date - .data$First) / 365.25 > 2, FALSE))
  # BLANK-CHECK REGISTRANTS: the filing's SIC code
  sic_ <- tibble::as_tibble(arrow::read_parquet(file = .path_landing, col_select = c("HashIndex", "SIC"))) |>
    dplyr::distinct(.data$HashIndex, .keep_all = TRUE)
  con_ <- con_ |>
    dplyr::left_join(sic_, by = "HashIndex") |>
    dplyr::mutate(BlankCheck = dplyr::coalesce(trimws(as.character(.data$SIC)) == "6770", FALSE))
  one_ <- function(.rows, .label) {
    .rows |>
      dplyr::summarise(
        N       = dplyr::n(),
        NReg    = sum(.data$IsReg),
        NEquity = sum(.data$IsEquity),
        .by     = "Year"
      ) |>
      tidyr::complete(Year = 2001L:2024L, fill = list(N = 0L, NReg = 0L, NEquity = 0L)) |>
      dplyr::mutate(Filers = .label)
  }
  dplyr::bind_rows(
    one_(.rows = con_,                                   .label = "All filers"),
    one_(.rows = dplyr::filter(con_, .data$Seasoned),      .label = "Seasoned filers"),
    one_(.rows = dplyr::filter(con_, .data$SeasonedEdgar), .label = "Seasoned, by first EDGAR filing"),
    one_(.rows = dplyr::filter(con_, .data$BlankCheck),    .label = "Blank-check registrants (SIC 6770)")
  ) |>
    dplyr::mutate(
      ShareReg    = dplyr::if_else(.data$N > 0L, .data$NReg / .data$N, NA_real_),
      ShareEquity = dplyr::if_else(.data$N > 0L, .data$NEquity / .data$N, NA_real_)
    ) |>
    dplyr::select("Year", "Filers", "N", "NReg", "ShareReg", "NEquity", "ShareEquity") |>
    dplyr::arrange(.data$Filers, .data$Year)
}

#' The seasoned-filer figure: contracts, registration share and equity share, all filers against seasoned filers
#'
#' @param .tab Tibble from oaa_data_seasoned().
#' @return A ggplot.
oaa_plot_seasoned <- function(.tab) {
  if (FALSE) .tab <- oaa_data_seasoned(.path_contracts = "C", .path_landing = "L", .dir_master = "M")
  lv_ <- c("All filers", "Seasoned filers", "Seasoned, by first EDGAR filing", "Blank-check registrants (SIC 6770)")
  pa_ <- c("A. Contracts per year, in 1,000s", "B. Filed with a registration statement, in %",
           "C. Equity contracts, in %")
  two_ <- c("All filers", "Seasoned filers")
  dat_ <- dplyr::bind_rows(
    dplyr::transmute(.tab, .data$Year, .data$Filers, Value = .data$N / 1000, Panel = pa_[1L]),
    dplyr::transmute(dplyr::filter(.tab, .data$Filers %in% two_), .data$Year, .data$Filers,
                     Value = 100 * .data$ShareReg, Panel = pa_[2L]),
    dplyr::transmute(dplyr::filter(.tab, .data$Filers %in% two_), .data$Year, .data$Filers,
                     Value = 100 * .data$ShareEquity, Panel = pa_[3L])
  ) |>
    dplyr::filter(!is.na(.data$Value)) |>
    dplyr::mutate(
      Filers = factor(.data$Filers, levels = lv_),
      Panel  = factor(.data$Panel, levels = pa_)
    )
  cat_ <- plot_pal_cat(.n = 3L)
  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$Value, colour = .data$Filers,
                                     linetype = .data$Filers)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 0.9) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Panel), ncol = 1L, scales = "free_y") +
    ggplot2::scale_colour_manual(values = stats::setNames(cat_[c(1L, 2L, 2L, 3L)], lv_), drop = FALSE) +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "solid", "dashed", "solid"), lv_), drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = seq(2002L, 2024L, 2L), expand = c(0.01, 0.01)) +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL, linetype = NULL) +
    plot_theme(.grid = "y", .legend = "bottom") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0)) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2L), linetype = ggplot2::guide_legend(nrow = 2L))
}

#' Build the seasoned-filer figure
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .path_landing Character. 01A's LandingPageAll.parquet.
#' @param .dir_master Character. The master index's parquet directory.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oaa_build_seasoned <- function(.path_contracts, .path_landing, .dir_master, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .path_landing <- here::here("2_output", "01A-EdgarIndex", "Output", "LandingPageAll.parquet")
    .dir_master   <- rGetEDGAR::get_directories(
      here::here("2_output", "01A-EdgarIndex", "GetEDGAR")
    )$MasterIndex$DirParquet
    .dir_own      <- here::here("2_output", "50-Numbers", "Output")
    .force        <- FALSE
    .path_lib     <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "SeasonedFilers"
  outs_ <- c(fs::path(.dir_own, "Figures", paste0(name_, c(".pdf", ".png"))),
             fs::path(.dir_own, c("Notes", "Data"), paste0(name_, c(".tex", ".parquet"))))
  ins_  <- c(.path_contracts, .path_landing, .dir_master, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_seasoned(
    .path_contracts = .path_contracts,
    .path_landing   = .path_landing,
    .dir_master     = .dir_master
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = NULL,
    .note    = paste(
      "Unique contracts of the descriptive sample, 2001-2024. A seasoned filer is one more than two years past its",
      "first contract in the sample, so that firms already filing in 2001 count as seasoned from 2003; the dashed",
      "line dates a filer by its first EDGAR filing of any form instead, from EDGAR's master index, which starts in",
      "1993. Blank-check registrants are the filings under SIC code 6770. Registration statements are Forms S-1,",
      "S-4, F-1 and F-4 with their amendments; equity contracts are those classified as Financial Instruments:",
      "Equity."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  plot_save(
    .plot   = oaa_plot_seasoned(.tab = tab_),
    .name   = name_,
    .dir    = fs::path(.dir_own, "Figures"),
    .height = 7.2
  )
  invisible("built")
}


# 7. The exhibit table: row (10) of 17 C.F.R. 229.601(a), with what the database covers ------------------------------

#' Row (10) of the exhibit table, one line per form, with the database's coverage beside it
#'
#' The exhibit table in Item 601(a) lists, for every form, which exhibits must be filed with it; row (10) is the
#' material contracts. The content is the regulation's and is kept here as a tibble so that the table is an exhibit
#' like any other. Required is the X of the table; Covered is whether the database downloads Exhibit 10 from the
#' form. Read from the eCFR on 14 Sep 2026; the two footnotes of the table (S-4 and F-4, 8-K) are stated in the note.
#'
#' @return Tibble: Form, Act, Required, Covered, Note.
oaa_data_exhibit_table <- function() {
  tibble::tribble(
    ~Form,    ~Act,             ~Required, ~Covered, ~Note,
    "S-1",    "Securities Act", TRUE,      TRUE,     "",
    "S-3",    "Securities Act", FALSE,     FALSE,    "",
    "SF-1",   "Securities Act", TRUE,      FALSE,    "asset-backed issuers",
    "SF-3",   "Securities Act", TRUE,      FALSE,    "asset-backed issuers",
    "S-4",    "Securities Act", TRUE,      TRUE,     "footnote 1 of the table",
    "S-8",    "Securities Act", FALSE,     FALSE,    "",
    "S-11",   "Securities Act", TRUE,      FALSE,    "real-estate companies",
    "F-1",    "Securities Act", TRUE,      TRUE,     "",
    "F-3",    "Securities Act", FALSE,     FALSE,    "",
    "F-4",    "Securities Act", TRUE,      TRUE,     "footnote 1 of the table",
    "10",     "Exchange Act",   TRUE,      FALSE,    "registration under the Exchange Act",
    "8-K",    "Exchange Act",   FALSE,     TRUE,     "footnote 2 of the table; Item 1.01 announces the contract",
    "10-D",   "Exchange Act",   TRUE,      FALSE,    "asset-backed issuers",
    "10-Q",   "Exchange Act",   TRUE,      TRUE,     "",
    "10-K",   "Exchange Act",   TRUE,      TRUE,     "",
    "ABS-EE", "Exchange Act",   FALSE,     FALSE,    "asset-backed issuers"
  )
}

#' Build the exhibit table
#'
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library, the table's only input.
#' @return Invisibly, the build's status.
oaa_build_exhibit_table <- function(.dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "ExhibitRequired"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = .path_lib, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_exhibit_table()
  cells_ <- tibble::tibble(
    Panel    = paste(tab_$Act, "forms"),
    Form     = oa_tex_escape(.x = tab_$Form),
    Required = dplyr::if_else(tab_$Required, "X", "--"),
    Covered  = dplyr::if_else(tab_$Covered, "X", "--"),
    Note     = dplyr::if_else(nzchar(tab_$Note), oa_tex_escape(.x = tab_$Note), "--")
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Form", "Exhibit 10 required", "In the database", "Note"),
      .spec   = c(oa_col_text(.share = 0.12), oa_col_num(.mm = 30), oa_col_num(.mm = 26), oa_col_text(.share = 0.40))
    ),
    .note    = paste(
      "Row (10), material contracts, of the exhibit table in Item 601(a) of Regulation S-K (17 C.F.R. 229.601(a)),",
      "read on September 14, 2026: an X marks a form with which a material contract must be filed as Exhibit 10.",
      "The table's footnote 1 exempts a company from providing the exhibit on Form S-4 or F-4 where it has elected",
      "to provide information at the level of Form S-3 or F-3 and that form would not require it; footnote 2 limits",
      "Form 8-K exhibits to those relevant to the subject matter of the report. In the database marks the forms from",
      "which the database downloads Exhibit 10; the 8-K is covered although the table does not require the contract",
      "there, because filers attach it to the announcement under Item 1.01 (Section A.1)."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# 8. What the forms are: the CFR sections that establish them --------------------------------------------------------

#' The sixteen forms of the exhibit table, each with the CFR section that establishes it and its purpose
#'
#' Securities Act forms are established in 17 C.F.R. Part 239 and Exchange Act forms in Part 249; each section's
#' heading states what the form is for, and that heading is the description printed. Kept here as a tibble so that
#' the table is an exhibit like any other; the runbook's source register links every section.
#'
#' @return Tibble: Form, Act, Section, Description, Url.
oaa_data_form_descriptions <- function() {
  ecfr_ <- function(.part, .section) sprintf("https://www.ecfr.gov/current/title-17/chapter-II/part-%s/section-%s",
                                             .part, .section)
  tibble::tribble(
    ~Form,    ~Act,             ~Section,    ~Description,
    "S-1",    "Securities Act", "239.11",    "Registration statement under the Securities Act of 1933; the general form",
    "S-3",    "Securities Act", "239.13",    "Registration statement for specified transactions by certain issuers (shelf registration)",
    "SF-1",   "Securities Act", "239.44",    "Registration statement under the Securities Act of 1933 for offerings of asset-backed securities",
    "SF-3",   "Securities Act", "239.45",    "Registration statement for offerings of asset-backed securities offered pursuant to certain types of transactions (shelf)",
    "S-4",    "Securities Act", "239.25",    "Registration of securities issued in business combination transactions",
    "S-8",    "Securities Act", "239.16b",   "Registration of securities to be offered to employees pursuant to employee benefit plans",
    "S-11",   "Securities Act", "239.18",    "Registration of securities of certain real estate companies",
    "F-1",    "Securities Act", "239.31",    "Registration statement for securities of certain foreign private issuers",
    "F-3",    "Securities Act", "239.33",    "Registration statement for specified transactions by certain foreign private issuers",
    "F-4",    "Securities Act", "239.34",    "Registration statement for securities of certain foreign private issuers issued in certain business combination transactions",
    "10",     "Exchange Act",   "249.210",
    "General form for registration of securities pursuant to section 12(b) or (g) of the Exchange Act",
    "8-K",    "Exchange Act",   "249.308",   "Current report",
    "10-D",   "Exchange Act",   "249.312",   "Asset-backed issuer distribution report",
    "10-Q",   "Exchange Act",   "249.308a",  "Quarterly report",
    "10-K",   "Exchange Act",   "249.310",   "Annual report",
    "ABS-EE", "Exchange Act",   "249.1401",  "Asset-backed securities: submission of asset data file and related documents"
  ) |>
    dplyr::mutate(
      Part = dplyr::if_else(.data$Act == "Securities Act", "239", "249"),
      Url  = ecfr_(.part = .data$Part, .section = .data$Section)
    ) |>
    dplyr::select("Form", "Act", "Section", "Description", "Url")
}

#' Build the form-descriptions table
#'
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library, the table's only input.
#' @return Invisibly, the build's status.
oaa_build_form_descriptions <- function(.dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "FormDescriptions"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = .path_lib, .force = .force)) return(invisible("up to date"))
  tab_ <- oaa_data_form_descriptions()
  cells_ <- tibble::tibble(
    Panel       = paste(tab_$Act, "forms"),
    Form        = oa_tex_escape(.x = tab_$Form),
    Section     = paste0("17 C.F.R. ", tab_$Section),
    Description = oa_tex_escape(.x = tab_$Description)
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Form", "Established in", "What the form is for"),
      .spec   = c(oa_col_text(.share = 0.10), oa_col_text(.share = 0.20), oa_col_text(.share = 0.62))
    ),
    .note    = paste(
      "The forms of the exhibit table, each with the section of the Code of Federal Regulations that establishes it",
      "-- Part 239 for forms under the Securities Act of 1933, Part 249 for forms under the Securities Exchange Act",
      "of 1934 -- and the purpose that section's heading states. Amendments to a form are filed under the same form",
      "type with the suffix /A."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# ====================================================================================================================
# Appendix C -- classification: the builders moved here from 42-OnlineAppendix-C, unchanged; they write into 40's Output/
# ====================================================================================================================

# ======================================================================================================================
# 40-OnlineAppendix-C.R -- Appendix C, the classification approach: the exhibits this chapter builds
# ======================================================================================================================
#
# 40-OnlineAppendix-C.qmd holds the chapter's text; _Commons/_OnlineAppendix.R holds everything the chapters share.
# Every table of the chapter is written here, in the appendix's own cut: most from the data tibbles 30 writes beside
# its own tables (the labeled sample, the sweep, the scores by category, the confusion matrix, the amendment flag,
# the keyword table, the arms and the ceiling), with fewer columns, no fold uncertainty and numbered categories;
# two from other sources -- the descriptions filers give their contracts, from the release, and the second-label
# table, from 03B's out-of-fold classification. 30 stays the manuscript's writer; nothing of 30 is changed or rerun.
#
# THE CATEGORY NUMBERS follow the order of the paper's categories table, which is the order 30 prints, so an error
# that stays inside a parent sits next to the diagonal of the confusion matrix. One vector, .oac_categories, holds
# it; every table reads its numbers from there.
#
# THE PREFIX IS oac_: online appendix, chapter C.


# 1. The sample, as 30 reads it ---------------------------------------------------------------------------------------

#' The unique contracts of the descriptive sample, as 30 selects them
#'
#' The one reader of the release every build here shares, so all of them count exactly the paper's sample: a
#' contract is kept where DescSample is 1 and it is the primary copy of its attachment -- 30's Keep. The flags are
#' compared after the collect, where an integer and a logical flag compare alike; in Arrow they need not.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .cols Character. The columns to keep besides the flags.
#' @return Tibble: .cols, plus Date and Year, for 2001-2024.
oac_read_sample <- function(.path_contracts, .cols) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .cols <- c("FormType", "DateFiled")
  }
  arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::all_of(unique(c(.cols, "DateFiled", "PrimaryFiler", "DescSample")))) |>
    dplyr::collect() |>
    dplyr::filter(
      dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      dplyr::coalesce(as.integer(.data$DescSample), 0L) == 1L
    ) |>
    dplyr::mutate(
      Date = as.Date(.data$DateFiled),
      Year = as.integer(format(.data$Date, "%Y"))
    ) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::select(-"PrimaryFiler", -"DescSample")
}

# 2. What filers call their contracts, per category --------------------------------------------------------------------

#' The most frequent descriptions filers give their contracts, per category
#'
#' The filer's own description is the only human-written label an attachment carries, so it shows what a category
#' holds in the words of the people who file. Free text: filers write what they like, and many write nothing beyond
#' the exhibit number. Three rules make the descriptions comparable. A leading exhibit number is stripped, so that
#' "10.1 Credit Agreement" counts with "Credit Agreement". A description that carries no words beyond an exhibit
#' number or a form name is dropped as uninformative, and the share it accounts for is reported per category.
#' Descriptions are grouped case- and punctuation-insensitively, and the group is printed in its most frequent
#' original spelling.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .n Integer. Descriptions per category.
#' @return Tibble: Class, Kind, Rank, Description, N, Share (of the category's contracts that carry a description),
#'   plus nClass, nNamed and pNamed per category.
oac_data_titles <- function(.path_contracts, .n) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .n <- 10L
  }
  # A LEADING EXHIBIT NUMBER, in the spellings filers use: EX-10.1, EXHIBIT 10.23, 10.1, (10.1), 10.1 -
  # It must carry a decimal point, or the word EX or EXHIBIT: a bare number is part of the title -- a year in
  # "2020 Equity Incentive Plan", a count in "3 Year Supply Agreement" -- and stripping it would corrupt the text.
  .re_number <- paste0(
    "(?i)^[\\s(\\[]*(",
    "ex(hibit)?[\\s.-]*\\d{1,3}([.(][a-z0-9]+\\)?)*",   # EX-10.1, EXHIBIT 10, EX 10.23(a), Ex. 10(a)
    "|\\d{1,3}([.(][a-z0-9]+\\)?)+",                    # 10.1, 10(a), 10.23a
    ")[\\s)\\]:.,-]*"
  )
  # NOTHING BUT A WORD FOR THE EXHIBIT ITSELF, or a form name: no description at all
  .re_unnamed <- paste0(
    "(?i)^(ex|exhibit|exhibits|document|attachment|annex|appendix|material contract[s]?|agreement|contract|",
    "8-k|10-k|10-q|20-f|s-1|s-4|f-1|f-4)$"
  )
  con_ <- oac_read_sample(
    .path_contracts = .path_contracts,
    .cols           = c("Class", "DocDesc")
  ) |>
    dplyr::filter(!is.na(.data$Class))
  if (nrow(con_) == 0L) cli::cli_abort("No contract carries a {.field Class}; the release's labels differ.")
  clean_ <- con_ |>
    dplyr::mutate(
      Desc = trimws(dplyr::coalesce(.data$DocDesc, "")),
      Desc = stringi::stri_replace_first_regex(.data$Desc, .re_number, ""),
      Desc = trimws(stringi::stri_replace_all_regex(.data$Desc, "\\s+", " ")),
      # UNINFORMATIVE: nothing left, or nothing but a form name or a word for the exhibit itself
      Named = nzchar(.data$Desc) & !stringi::stri_detect_regex(.data$Desc, .re_unnamed),
      Key = toupper(stringi::stri_replace_all_regex(.data$Desc, "[^[:alnum:] ]", " ")),
      Key = trimws(stringi::stri_replace_all_regex(.data$Key, "\\s+", " "))
    )
  per_class_ <- clean_ |>
    dplyr::summarise(nClass = dplyr::n(), pNamed = mean(.data$Named), .by = "Class")
  # A READABLE SPELLING. Filers most often write in capitals, and a table of capitals is hard to read and says
  # nothing the lower-case spelling does not. The most frequent spelling that is not all capitals is printed where
  # there is one; otherwise the capitals are set in title case, with the short words that belong inside a title
  # left lower-case.
  pretty_ <- function(.x) {
    small_ <- c("a", "an", "and", "as", "at", "by", "for", "from", "in", "of", "on", "or", "the", "to", "with")
    words_ <- strsplit(tolower(.x), " ", fixed = TRUE)[[1L]]
    up_    <- paste0(toupper(substring(words_, 1L, 1L)), substring(words_, 2L))
    out_   <- ifelse(words_ %in% small_ & seq_along(words_) > 1L, words_, up_)
    paste(out_, collapse = " ")
  }
  n_named_ <- clean_ |>
    dplyr::filter(.data$Named) |>
    dplyr::summarise(nNamed = dplyr::n(), .by = "Class")
  top_ <- clean_ |>
    dplyr::filter(.data$Named) |>
    dplyr::count(.data$Class, .data$Key, .data$Desc, name = "nSpelling") |>
    dplyr::arrange(dplyr::desc(.data$nSpelling)) |>
    dplyr::summarise(
      Description = {
        mixed_ <- .data$Desc[.data$Desc != toupper(.data$Desc)]
        if (length(mixed_) > 0L) mixed_[[1L]] else pretty_(.x = .data$Desc[[1L]])
      },
      N           = sum(.data$nSpelling),
      .by         = c("Class", "Key")
    ) |>
    dplyr::arrange(.data$Class, dplyr::desc(.data$N)) |>
    dplyr::slice_head(n = .n, by = "Class") |>
    dplyr::mutate(Rank = dplyr::row_number(), .by = "Class") |>
    dplyr::left_join(per_class_, by = "Class") |>
    dplyr::left_join(n_named_, by = "Class") |>
    dplyr::mutate(Share = .data$N / .data$nNamed, Kind = "title") |>
    dplyr::select("Class", "Kind", "Rank", "Description", "N", "Share", "nClass", "nNamed", "pNamed")
  dplyr::arrange(top_, .data$Class, .data$Rank)
}

#' Build the table of filer descriptions by category, one row per category
#'
#' The reviewer asked for example titles; a list of ten per category runs to a page and a half, and the same content
#' fits twelve rows: the category, its most frequent descriptions on one line with their counts, and the share of
#' its contracts that carry a description at all.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .n Integer. Descriptions per category.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oac_build_titles <- function(.path_contracts, .n, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .n        <- 5L
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "CategoryTitles"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oac_data_titles(
    .path_contracts = .path_contracts,
    .n              = .n
  )
  num_ <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)
  rows_ <- tab_ |>
    dplyr::arrange(.data$Class, .data$Rank) |>
    dplyr::summarise(
      Descriptions = paste0(oa_tex_escape(.x = .data$Description), " (", num_(.x = .data$N), ")", collapse = "; "),
      pNamed       = dplyr::first(.data$pNamed),
      .by          = "Class"
    ) |>
    dplyr::mutate(Order = oac_number(.name = .data$Class)) |>
    dplyr::arrange(.data$Order)
  cells_ <- tibble::tibble(
    Category     = oac_label(.class = rows_$Class),
    Descriptions = rows_$Descriptions,
    Named        = formatC(100 * rows_$pNamed, format = "f", digits = 0L)
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Category", paste("The", .n, "most frequent descriptions (contracts)"), "\\% described"),
      .spec   = c(oa_col_text(.share = 0.22), oa_col_text(.share = 0.62), oa_col_num(.mm = 18))
    ),
    .note    = paste(
      "The", .n, "most frequent descriptions filers give their contracts, by category, over the unique contracts of",
      "the descriptive sample; the category is the classifier's label. A description is the filer's own free text,",
      "the only human-written label an attachment carries. A leading exhibit number is stripped before counting, and",
      "a description that says nothing beyond an exhibit or form name is treated as no description; the last column",
      "is the share of a category's contracts that carry one. Descriptions are grouped without regard to case and",
      "punctuation and printed in the most frequent spelling that is not in capitals."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# 3. The chapter's cut of 30's classification tables -------------------------------------------------------------------
# 30 writes the manuscript's version of each table and, beside it, the data tibble it printed. The appendix prints
# the same numbers with fewer columns, no fold uncertainty and numbered categories. Each builder reads 30's tibble,
# writes the appendix's table under this chapter's directory, and keeps the tibble it read as its own data, so the
# numbers in the text resolve from the same file whichever copy is found first.

# THE CATEGORIES, in the order of the paper's categories table. Class is the label 30's tibbles carry.
.oac_categories <- tibble::tibble(
  Short  = c("Credit", "Equity", "Compensation", "Legal", "Assets", "R&D", "Customer / Supplier", "Licenses",
             "Leases", "Peer Agreements", "M&A", "Other"),
  Number = 1:12
)

#' A category's number, from any of the names it goes by
#'
#' 30's tibbles carry the short name in Row and a qualified name in Class ("Employment: Compensation"); the release
#' carries the qualified name, and M&A's qualified name is "Investment and Merger". The number is looked up on the
#' short name, taken as the part after the parent where there is one, with that alias.
#'
#' @param .name Character. Short or qualified category names.
#' @return Integer, NA where the name is not one of the twelve.
oac_number <- function(.name) {
  if (FALSE) .name <- c("Employment: Compensation", "R&D", "Business Structure: Investment and Merger")
  short_ <- sub("^.*: ", "", as.character(.name))
  short_[short_ %in% c("Investment and Merger", "Investment & Merger", "Mergers and Acquisitions")] <- "M&A"
  match(short_, .oac_categories$Short)
}

#' A category's printed label: its number and its short name, escaped for LaTeX
#'
#' @param .class Character. Category names, short or qualified.
#' @return Character, "(3) Compensation"; NA where the class is not one of the twelve.
oac_label <- function(.class) {
  if (FALSE) .class <- c("Employment: Compensation", "R&D")
  i_ <- oac_number(.name = .class)
  dplyr::if_else(is.na(i_), NA_character_,
                 paste0("(", .oac_categories$Number[i_], ") ", oa_tex_escape(.x = .oac_categories$Short[i_])))
}

#' 30's data tibble for one exhibit, or an abort that names the file
#'
#' @param .dir_data Character. 30's Output/Data.
#' @param .name Character. The exhibit's stem.
#' @return Tibble.
oac_read_30 <- function(.dir_data, .name) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .name     <- "ClassLabelled"
  }
  p_ <- fs::path(.dir_data, paste0(.name, ".parquet"))
  if (!fs::file_exists(p_)) cli::cli_abort("30 has not written {.file {p_}}; render 30 first.")
  arrow::read_parquet(p_)
}

#' The row labels of a category table: numbered leaves, bold parents, bold total
#'
#' 30's category tibbles carry Row (the printed name) and Kind (super, sub, total). A sub row is a leaf under its
#' parent, indented and numbered; a super row whose name is one of the twelve -- Licenses, Leases, Other -- is a leaf
#' of its own and is numbered in bold; any other super row is a parent, in bold without a number.
#'
#' @param .tab Tibble with Row and Kind.
#' @return Character, one label per row, escaped.
oac_row_labels <- function(.tab) {
  if (FALSE) .tab <- tibble::tibble(Row = c("Employment", "Compensation", "Licenses", "Total"),
                                    Kind = c("super", "sub", "super", "total"))
  lab_ <- oac_label(.class = .tab$Row)
  dplyr::case_when(
    .tab$Kind == "total"              ~ paste0("\\textbf{", oa_tex_escape(.x = .tab$Row), "}"),
    !is.na(lab_) & .tab$Kind == "sub" ~ paste0("\\hspace{1em}", lab_),
    !is.na(lab_)                      ~ paste0("\\textbf{", lab_, "}"),
    .default                          = paste0("\\textbf{", oa_tex_escape(.x = .tab$Row), "}")
  )
}

#' Whether a build of one of 30's tables is due
#'
#' @param .dir_own,.name,.path_30,.path_lib,.force As in the builders.
#' @return Logical.
oac_due <- function(.dir_own, .name, .path_30, .path_lib, .force) {
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(.name, c(".tex", ".tex", ".parquet")))
  oa_build_needed(.outputs = outs_, .inputs = c(.path_30, .path_lib), .force = .force)
}

oac_fmt3 <- function(.x) dplyr::if_else(is.na(.x), "--", formatC(.x, format = "f", digits = 3L))
oac_fmtn <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)

#' Build the labeled-sample table: N, share and second labels per category
#'
#' @param .dir_data Character. 30's Output/Data.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oac_build_labelled <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "ClassLabelled"
  p30_  <- fs::path(.dir_data, paste0(name_, ".parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = name_)
  # THE AMENDMENT BLOCK -- Original / Amended under an "Amendment label" heading -- is a second table's worth of
  # rows; the appendix states the two counts in the text and prints the categories alone.
  cat_ <- tab_ |>
    dplyr::filter(is.na(.data$Level1) | .data$Level1 != "Amendment")
  cells_ <- tibble::tibble(
    Row    = oac_row_labels(.tab = cat_),
    N      = oac_fmtn(.x = cat_$N),
    Share  = formatC(cat_$Share, format = "f", digits = 1L),  # 30 stores the share as a percent
    Second = oac_fmtn(.x = dplyr::coalesce(cat_$Second, 0L))
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", "N", "\\%", "Second label"),
      .spec   = c(oa_col_text(.share = 0.52), oa_col_num(.mm = 18), oa_col_num(.mm = 14), oa_col_num(.mm = 24))
    ),
    .note    = paste(
      "The labeled sample: contracts with readable text and a label, by category. Parent rows sum their",
      "sub-categories. Second label counts the contracts that carry a second valid category, recorded beside the",
      "primary and never used in training. The numbers in parentheses are the categories' numbers throughout this",
      "appendix, in the order of the paper's categories table."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the deployed-models table from the sweep: what was crowned and what was deployed, per task
#'
#' The sweep itself is stated in the text -- the grid, how many configurations, how the winner was chosen. The table
#' shows only the configurations that left the sweep: the one that ships per task and the ones deployed beside it.
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_deployed <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "ClassDeployed"
  p30_  <- fs::path(.dir_data, "ClassSweep.parquet")
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = "ClassSweep") |>
    dplyr::mutate(Crown = dplyr::coalesce(as.logical(.data$Crown), FALSE),
                  Deployed = dplyr::coalesce(as.logical(.data$Deployed), FALSE)) |>
    dplyr::filter(.data$Crown | .data$Deployed) |>
    dplyr::mutate(
      TaskOrder = match(.data$LabelCol, c("ClassDetailed", "ClassBroad", "AmendType")),
      Status    = dplyr::if_else(.data$Crown, "ships", "deployed")
    ) |>
    dplyr::arrange(.data$TaskOrder, dplyr::desc(.data$Crown), .data$MaxLen)
  cells_ <- tibble::tibble(
    Task     = oa_tex_escape(.x = as.character(tab_$Task)),
    Model    = oa_tex_escape(.x = sub("^.*/", "", as.character(tab_$Model))),
    Context  = as.character(tab_$MaxLen),
    Accuracy = oac_fmt3(.x = tab_$Accuracy),
    MacroF1  = oac_fmt3(.x = tab_$MacroF1),
    Status   = tab_$Status
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Task", "Encoder", "Context", "Accuracy", "Macro-F1", ""),
      .spec   = c(oa_col_text(.share = 0.20), oa_col_text(.share = 0.30), oa_col_num(.mm = 16), oa_col_num(.mm = 20),
                  oa_col_num(.mm = 20), "l")
    ),
    .note    = paste(
      "The configurations that left the sweep: per task, the one crowned by the highest mean out-of-fold macro-F1,",
      "which labels the corpus (ships), and the ones refitted beside it (deployed). Context is the number of tokens",
      "read from the start of a contract. Accuracy and macro-F1 are pooled out of fold over the five folds."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build a scores-by-category table in the appendix's cut, from one of 30's engine tibbles
#'
#' Serves the transformer and the keyword table, which share 30's layout. No fold uncertainty; the columns the
#' text reads: support, and per category precision, recall and F1, with lenient recall for the transformer and
#' coverage for the keyword table.
#'
#' @param .name Character. "ClassTransformer" or "ClassKeyword".
#' @param .columns Character. The score columns to print, from 30's tibble.
#' @param .header Character. Their printed headers.
#' @param .note Character. The table's note.
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_scores <- function(.name, .columns, .header, .note, .dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .name     <- "ClassTransformer"
    .columns  <- c("Precision", "Recall", "F1", "RecallLenient")
    .header   <- c("Precision", "Recall", "F1", "Lenient recall")
    .note     <- "..."
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  p30_ <- fs::path(.dir_data, paste0(.name, ".parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = .name, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = .name)
  miss_ <- setdiff(.columns, names(tab_))
  if (length(miss_) > 0L) cli::cli_abort("30's {.val {(.name)}} lacks {.field {miss_}}.")
  cells_ <- dplyr::bind_cols(
    tibble::tibble(Row = oac_row_labels(.tab = tab_), N = oac_fmtn(.x = tab_$Support)),
    tab_ |>
      dplyr::select(dplyr::all_of(.columns)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(.x) oac_fmt3(.x = .x)))
  )
  oa_write_exhibit(
    .name    = .name,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", "N", .header),
      .spec   = c(oa_col_text(.share = 0.34), oa_col_num(.mm = 16), rep(oa_col_num(.mm = 18), length(.columns)))
    ),
    .note    = .note,
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the confusion matrix with numbered categories on both axes
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_confusion <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "ClassConfusion"
  p30_  <- fs::path(.dir_data, paste0(name_, ".parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = name_)
  cols_ <- .oac_categories$Short
  miss_ <- setdiff(cols_, names(tab_))
  if (length(miss_) > 0L) cli::cli_abort("30's confusion matrix lacks the column{?s} {.val {miss_}}.")
  tab_ <- tab_[match(cols_, tab_$True), , drop = FALSE]
  cells_ <- dplyr::bind_cols(
    tibble::tibble(True = oac_label(.class = tab_$True)),
    tab_ |>
      dplyr::select(dplyr::all_of(cols_)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(.x) oac_fmtn(.x = .x)))
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", paste0("(", .oac_categories$Number, ")")),
      .spec   = c(oa_col_text(.share = 0.22), rep(oa_col_num(.mm = 9), 12L))
    ),
    .note    = paste(
      "Confusion matrix of the transformer that ships on the labeled sample, out of fold: rows are the true category,",
      "columns the predicted one, numbered as in the labeled-sample table; cells are numbers of contracts.",
      "Categories are in taxonomic order, so an error that stays inside a parent sits next to the diagonal and one",
      "that crosses a parent sits further from it."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the amendment table: the two labels and the total, without fold uncertainty
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_amendment <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "ClassAmendment"
  p30_  <- fs::path(.dir_data, paste0(name_, ".parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = name_)
  tot_ <- tab_$Label == "Total"
  cells_ <- tibble::tibble(
    Label     = dplyr::if_else(tot_, paste0("\\textbf{", oa_tex_escape(.x = tab_$Label), "}"),
                               oa_tex_escape(.x = tab_$Label)),
    N         = oac_fmtn(.x = tab_$Support),
    Predicted = oac_fmtn(.x = tab_$Predicted),
    Precision = oac_fmt3(.x = tab_$Precision),
    Recall    = oac_fmt3(.x = tab_$Recall),
    F1        = oac_fmt3(.x = tab_$F1)
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", "N", "Predicted", "Precision", "Recall", "F1"),
      .spec   = c(oa_col_text(.share = 0.30), rep(oa_col_num(.mm = 18), 5L))
    ),
    .note    = paste(
      "Out-of-fold scores of the amendment classifier that ships on the labeled sample. N is the number of",
      "contracts with the label, Predicted the number the classifier assigned it. The Total row reports overall",
      "accuracy in the precision column and macro recall and macro-F1 beside it."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the arms table: every engine on every task, and the ceiling, without fold uncertainty
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_arms <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "ClassArms"
  p30_  <- fs::path(.dir_data, c("ClassArms.parquet", "ClassArmsCeiling.parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  arms_ <- oac_read_30(.dir_data = .dir_data, .name = "ClassArms")
  ceil_ <- oac_read_30(.dir_data = .dir_data, .name = "ClassArmsCeiling")
  task_ <- c(ClassDetailed = "Detailed (12)", ClassBroad = "Broad (7)", AmendType = "Amendment (2)")
  eng_  <- c(Bert256 = "Transformer, 256 tokens", Bert512 = "Transformer, 512 tokens", Kw = "Keyword table",
             Llm = "Language model")
  arms_ <- arms_ |>
    dplyr::mutate(TaskOrder = match(.data$Task, names(task_)), EngOrder = match(.data$Engine, names(eng_))) |>
    dplyr::arrange(.data$TaskOrder, .data$EngOrder)
  a_ <- tibble::tibble(
    Panel    = unname(dplyr::coalesce(task_[arms_$Task], arms_$Task)),
    Engine   = paste0(oa_tex_escape(.x = unname(dplyr::coalesce(eng_[arms_$Engine], arms_$Engine))),
                      dplyr::if_else(dplyr::coalesce(as.logical(arms_$Ships), FALSE), " (ships)", "")),
    Coverage = oac_fmt3(.x = arms_$Coverage),
    Accuracy = oac_fmt3(.x = arms_$Accuracy),
    AccSel   = oac_fmt3(.x = arms_$AccuracySel),
    MacroF1  = oac_fmt3(.x = arms_$MacroF1)
  )
  c_ <- tibble::tibble(
    Panel    = "Detailed, ceiling of perfect routing",
    Engine   = oa_tex_escape(.x = as.character(ceil_$Engines)),
    Coverage = "",
    Accuracy = oac_fmt3(.x = ceil_$Accuracy),
    AccSel   = "",
    MacroF1  = paste0("+", oac_fmt3(.x = ceil_$Marginal))
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = dplyr::bind_rows(a_, c_),
      .header = c("", "Coverage", "Accuracy", "Accuracy where committed", "Macro-F1"),
      .spec   = c(oa_col_text(.share = 0.36), oa_col_num(.mm = 18), oa_col_num(.mm = 18), oa_col_num(.mm = 30),
                  oa_col_num(.mm = 18))
    ),
    .note    = paste(
      "Every engine on every task, out of fold on the same five folds. Coverage is the share of contracts an engine",
      "labels; the transformer and the language model label every contract, the keyword table abstains where no",
      "term fires. Accuracy is over all contracts, an abstention counting as wrong; accuracy where committed is over",
      "the contracts the engine labeled. The last rows are the ceiling: the accuracy perfect routing would reach on",
      "the detailed task, taking for every contract whichever of the engines named is right, which needs the true",
      "label and cannot be run; the last column is what each added engine gains over the ones before it."
    ),
    .data    = dplyr::bind_rows(dplyr::mutate(arms_, Block = "arms"), dplyr::mutate(ceil_, Block = "ceiling")),
    .dir_own = .dir_own
  )
  invisible("built")
}


# 3. The second label against the runner-up --------------------------------------------------------------------------

#' The second-label data: every dual-labeled document with the model's two choices beside its two labels
#'
#' 03A records a second valid category for the minority of labeled documents that fit two, reviewed by hand so that
#' the primary is the intended one; training never uses it. 03B's classification file carries, for every labeled
#' document, the model's first and second choice with their probabilities, out of fold, and the second label beside
#' them. Two tables are built from it: the matrix of primary against second label, which shows where the annotators
#' saw two answers, and, per primary category, how often the model's first choice is the primary and its runner-up
#' the second label. The margin between the model's two choices, on documents with two labels and with one, is kept
#' in the data for the text.
#'
#' @param .path_class Character. 03B's crowned_classification.parquet for the detailed task.
#' @return Tibble: DocID, Primary, Second, Top1, Top2, Top1Prob, Margin, Dual.
oac_data_second <- function(.path_class) {
  if (FALSE) {
    .path_class <- here::here("2_output", "03B-ClassifyTrainBERT", "classification", "crowned_classification.parquet")
  }
  tab_ <- arrow::read_parquet(.path_class) |>
    dplyr::select(dplyr::any_of(c("DocID", "TrueLabel", "Top1Class", "Top1Prob", "Top2Class", "Margin",
                                  "ClassDetailed2")))
  need_ <- setdiff(c("TrueLabel", "Top1Class", "Top2Class", "Top1Prob", "Margin", "ClassDetailed2"), names(tab_))
  if (length(need_) > 0L) cli::cli_abort("The classification file lacks {.field {need_}}.")
  tab_ |>
    dplyr::transmute(
      DocID    = .data$DocID,
      Primary  = as.character(.data$TrueLabel),
      Second   = as.character(.data$ClassDetailed2),
      Top1     = as.character(.data$Top1Class),
      Top2     = as.character(.data$Top2Class),
      Top1Prob = .data$Top1Prob,
      Margin   = .data$Margin,
      Dual     = !is.na(.data$Second) & nzchar(.data$Second)
    )
}

#' The per-category hits and the margins, from the second-label data
#'
#' @param .tab Tibble from oac_data_second().
#' @return Tibble: Row (a category or Total), Kind, N (documents with two labels), Hit1, Hit2, Both, plus the
#'   two margin rows (Kind "margin": N is the group's size, Value its mean margin).
oac_second_summary <- function(.tab) {
  if (FALSE) .tab <- oac_data_second(.path_class = here::here("2_output", "03B-ClassifyTrainBERT", "classification",
                                                                 "crowned_classification.parquet"))
  dual_ <- dplyr::filter(.tab, .data$Dual) |>
    dplyr::mutate(
      Hit1 = .data$Top1 == .data$Primary,
      Hit2 = .data$Top2 == .data$Second,
      Both = .data$Hit1 & .data$Hit2,
      Order = oac_number(.name = .data$Primary)
    )
  by_ <- dual_ |>
    dplyr::summarise(N = dplyr::n(), Hit1 = sum(.data$Hit1), Hit2 = sum(.data$Hit2), Both = sum(.data$Both),
                     .by = c("Primary", "Order")) |>
    dplyr::arrange(.data$Order) |>
    dplyr::transmute(Row = .data$Primary, Kind = "category", N = .data$N, Hit1 = .data$Hit1, Hit2 = .data$Hit2,
                     Both = .data$Both, Value = NA_real_)
  tot_ <- tibble::tibble(Row = "Total", Kind = "total", N = nrow(dual_), Hit1 = sum(dual_$Hit1),
                         Hit2 = sum(dual_$Hit2), Both = sum(dual_$Both), Value = NA_real_)
  mar_ <- .tab |>
    dplyr::summarise(N = dplyr::n(), Value = mean(.data$Margin, na.rm = TRUE), .by = "Dual") |>
    dplyr::transmute(Row = dplyr::if_else(.data$Dual, "Margin, two labels", "Margin, one label"), Kind = "margin",
                     N = .data$N, Hit1 = NA_integer_, Hit2 = NA_integer_, Both = NA_integer_, Value = .data$Value)
  dplyr::bind_rows(by_, tot_, mar_)
}

#' Build the two second-label tables: the matrix of label pairs, and the model's hits per category
#'
#' @param .path_class Character. 03B's crowned_classification.parquet.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oac_build_second <- function(.path_class, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_class <- here::here("2_output", "03B-ClassifyTrainBERT", "classification", "crowned_classification.parquet")
    .dir_own    <- here::here("2_output", "50-Numbers", "Output")
    .force      <- FALSE
    .path_lib   <- here::here("1_code", "50-Numbers.R")
  }
  names_ <- c("ClassSecondPairs", "ClassSecond")
  outs_  <- unlist(purrr::map(names_, \(.n) fs::path(.dir_own, c("Tables", "Notes", "Data"),
                                                       paste0(.n, c(".tex", ".tex", ".parquet")))))
  if (!fs::file_exists(.path_class)) return(invisible("waiting for 03B's crowned_classification.parquet"))
  if (!oa_build_needed(.outputs = outs_, .inputs = c(.path_class, .path_lib), .force = .force)) {
    return(invisible("up to date"))
  }
  tab_  <- oac_data_second(.path_class = .path_class)
  dual_ <- dplyr::filter(tab_, .data$Dual)
  # THE MATRIX: primary label down, second label across, both numbered; a zero prints as a blank so the pairs stand out
  k_ <- nrow(.oac_categories)
  m_ <- matrix(0L, nrow = k_, ncol = k_)
  i_ <- oac_number(.name = dual_$Primary)
  j_ <- oac_number(.name = dual_$Second)
  if (anyNA(i_) || anyNA(j_)) cli::cli_abort("A second-label category is not one of the twelve.")
  for (r_ in seq_along(i_)) m_[i_[r_], j_[r_]] <- m_[i_[r_], j_[r_]] + 1L
  pairs_ <- tibble::as_tibble(m_, .name_repair = \(.x) .oac_categories$Short) |>
    dplyr::mutate(Primary = .oac_categories$Short, .before = 1L)
  cells_p_ <- dplyr::bind_cols(
    tibble::tibble(Primary = oac_label(.class = pairs_$Primary)),
    pairs_ |>
      dplyr::select(-"Primary") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(.x) dplyr::if_else(.x == 0L, "", oac_fmtn(.x = .x))))
  )
  oa_write_exhibit(
    .name    = names_[1L],
    .lines   = oa_frame_table(
      .tab    = cells_p_,
      .header = c("Primary label", paste0("(", .oac_categories$Number, ")")),
      .spec   = c(oa_col_text(.share = 0.22), rep(oa_col_num(.mm = 9), k_))
    ),
    .note    = paste(
      "The", oac_fmtn(.x = nrow(dual_)), "labeled contracts that carry a second valid category: rows are the",
      "primary label, columns the second, numbered as in the labeled-sample table; cells are numbers of contracts,",
      "blank where zero. Every such contract was reviewed and the intended primary recorded, so the order of the two",
      "labels is informative."
    ),
    .data    = pairs_,
    .dir_own = .dir_own
  )
  # THE HITS: per primary category, the model's first choice against the primary and its runner-up against the second
  sum_ <- oac_second_summary(.tab = tab_)
  rows_ <- dplyr::filter(sum_, .data$Kind %in% c("category", "total"))
  pct_ <- function(.n, .d) paste0(oac_fmtn(.x = .n), " (", formatC(100 * .n / .d, format = "f", digits = 0L), ")")
  cells_h_ <- tibble::tibble(
    Row  = dplyr::if_else(rows_$Kind == "total", paste0("\\textbf{", rows_$Row, "}"),
                          dplyr::coalesce(oac_label(.class = rows_$Row), oa_tex_escape(.x = rows_$Row))),
    N    = oac_fmtn(.x = rows_$N),
    Hit1 = pct_(.n = rows_$Hit1, .d = rows_$N),
    Hit2 = pct_(.n = rows_$Hit2, .d = rows_$N),
    Both = pct_(.n = rows_$Both, .d = rows_$N)
  )
  oa_write_exhibit(
    .name    = names_[2L],
    .lines   = oa_frame_table(
      .tab    = cells_h_,
      .header = c("Primary label", "Two labels", "First choice is the primary (\\%)",
                  "Runner-up is the second label (\\%)", "Both (\\%)"),
      .spec   = c(oa_col_text(.share = 0.26), oa_col_num(.mm = 18), oa_col_num(.mm = 30), oa_col_num(.mm = 32),
                  oa_col_num(.mm = 20))
    ),
    .note    = paste(
      "The contracts with two labels, by primary label, against the detailed transformer's first and second choice",
      "out of fold: how many the model's first choice labels with the primary, how many its runner-up labels with",
      "the second label, and how many both. Shares are of the row's contracts."
    ),
    .data    = sum_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# ====================================================================================================================
# Appendix D -- entity extraction: the builders moved here from 42-OnlineAppendix-D, unchanged; they write into 40's Output/
# ====================================================================================================================

# ======================================================================================================================
# 40-OnlineAppendix-D.R -- Appendix D, entity extraction and the rules: the exhibits this chapter builds
# ======================================================================================================================
#
# 40-OnlineAppendix-D.qmd holds the chapter's text; _Commons/_OnlineAppendix.R holds everything the chapters share.
# This library builds the chapter's own exhibits: what each extractor found on the labeled sample, counted from the
# three span stores 04A wrote; where in a contract each extractor's candidates fall, and how far the extractors agree,
# computed from the same stores with 04A's definitions; where each contract's end date comes from, and what the content
# variables are worth under the naive and the rule-based reading, both from the release. The naive-against-rule
# coverage by category is 30's.
#
# THE PREFIX IS oad_: online appendix, chapter D.


# 1. The sample, as 30 reads it ---------------------------------------------------------------------------------------

#' The unique contracts of the descriptive sample, as 30 selects them
#'
#' The one reader of the release every build here shares, so all of them count exactly the paper's sample: a
#' contract is kept where DescSample is 1 and it is the primary copy of its attachment -- 30's Keep. The flags are
#' compared after the collect, where an integer and a logical flag compare alike; in Arrow they need not.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .cols Character. The columns to keep besides the flags.
#' @return Tibble: .cols, plus Date and Year, for 2001-2024.
oad_read_sample <- function(.path_contracts, .cols) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .cols <- c("FormType", "DateFiled")
  }
  arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::all_of(unique(c(.cols, "DateFiled", "PrimaryFiler", "DescSample")))) |>
    dplyr::collect() |>
    dplyr::filter(
      dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      dplyr::coalesce(as.integer(.data$DescSample), 0L) == 1L
    ) |>
    dplyr::mutate(
      Date = as.Date(.data$DateFiled),
      Year = as.integer(format(.data$Date, "%Y"))
    ) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::select(-"PrimaryFiler", -"DescSample")
}

# 2. What each extractor found on the labeled sample ------------------------------------------------------------------

#' Spans and coverage per family, model and entity, from 04A's stores
#'
#' 04A writes one DuckDB file per family under its Output/Store, holding one table per entity, every row a span with
#' its document and its offsets; spaCy's tables also carry the model. The stores are opened read-only and counted:
#' spans, and the documents in which the family found at least one, over the documents of the sample. Coverage is
#' not a quality measure -- an extractor that tags every capitalized word reaches complete coverage -- and the text
#' says so; what it establishes is what each extractor attempts and how much it proposes.
#'
#' @param .dir_store Character. 04A's Output, holding matcon.duckdb, spacy.duckdb and lexnlp.duckdb.
#' @param .path_sample Character. 04A's sample_text.parquet, the documents every store indexes.
#' @return Tibble: Family, Model, Entity, Spans, Docs, Coverage, over nDocs documents (as an attribute-free column).
oad_data_coverage <- function(.dir_store, .path_sample) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
  }
  n_docs_ <- nrow(arrow::read_parquet(.path_sample, col_select = "DocID"))
  meta_   <- c("ledger", "manifest", "bench", "failures", "corpus_index", "sample")
  read_family_ <- function(.family) {
    p_ <- fs::path(.dir_store, paste0(.family, ".duckdb"))
    if (!fs::file_exists(p_)) cli::cli_abort("04A's store {.file {p_}} does not exist.")
    con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(p_), read_only = TRUE)
    on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
    tabs_ <- setdiff(DBI::dbListTables(con_), meta_)
    purrr::map_dfr(tabs_, \(.t) {
      cols_ <- DBI::dbListFields(con_, .t)
      if (!"DocID" %in% cols_) return(NULL)
      by_model_ <- "Model" %in% cols_
      sql_ <- if (by_model_) {
        sprintf("SELECT Model, COUNT(*) AS Spans, COUNT(DISTINCT DocID) AS Docs FROM \"%s\" GROUP BY Model", .t)
      } else {
        sprintf("SELECT '%s' AS Model, COUNT(*) AS Spans, COUNT(DISTINCT DocID) AS Docs FROM \"%s\"", .family, .t)
      }
      DBI::dbGetQuery(con_, sql_) |>
        tibble::as_tibble() |>
        dplyr::mutate(Family = .family, Entity = toupper(.t), .before = 1L)
    })
  }
  purrr::map_dfr(c("lexnlp", "spacy", "matcon"), read_family_) |>
    dplyr::mutate(
      Spans    = as.integer(.data$Spans),
      Docs     = as.integer(.data$Docs),
      Coverage = .data$Docs / n_docs_,
      nDocs    = n_docs_
    ) |>
    dplyr::arrange(.data$Family, .data$Model, .data$Entity)
}

#' Build the extractor-coverage table
#'
#' Rows are the entities the chapter discusses, in the order it discusses them; columns are the three families, spaCy
#' represented by the transformer model 04A ran. A dash marks an entity a family does not attempt.
#'
#' @param .dir_store Character. 04A's Output.
#' @param .path_sample Character. 04A's sample_text.parquet.
#' @param .spacy_model Character. The spaCy model to print, "en_core_web_trf".
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_coverage <- function(.dir_store, .path_sample, .spacy_model, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
    .spacy_model <- "en_core_web_trf"
    .dir_own     <- here::here("2_output", "50-Numbers", "Output")
    .force       <- FALSE
    .path_lib    <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "EntityCoverage"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(fs::path(.dir_store, paste0(c("lexnlp", "spacy", "matcon"), ".duckdb")), .path_lib)
  if (!all(fs::file_exists(ins_[1:3]))) return(invisible("waiting for 04A's stores"))
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oad_data_coverage(
    .dir_store   = .dir_store,
    .path_sample = .path_sample
  )
  ent_ <- tibble::tribble(
    ~Entity,  ~Label,
    "ORG",    "Organizations",
    "PERSON", "Persons",
    "GPE",    "Places",
    "DATE",   "Dates",
    "TERM",   "Stated periods",
    "REDACT", "Redaction markers",
    "LAW",    "Governing-law clauses"
  )
  fam_ <- c("lexnlp", "spacy", "matcon")
  pick_ <- tab_ |>
    dplyr::filter(.data$Family != "spacy" | .data$Model == .spacy_model) |>
    dplyr::summarise(Spans = sum(.data$Spans), Docs = max(.data$Docs), Coverage = max(.data$Coverage),
                     .by = c("Family", "Entity"))
  cell_ <- function(.f, .e) {
    r_ <- pick_[pick_$Family == .f & pick_$Entity == .e, , drop = FALSE]
    if (nrow(r_) == 0L) return(c("--", "--"))
    c(format(r_$Spans, big.mark = ",", trim = TRUE), formatC(100 * r_$Coverage, format = "f", digits = 0L))
  }
  cells_ <- purrr::map_dfr(seq_len(nrow(ent_)), \(.i) {
    v_ <- unlist(purrr::map(fam_, \(.f) cell_(.f = .f, .e = ent_$Entity[.i])))
    tibble::tibble(Entity = ent_$Label[.i], L1 = v_[1], L2 = v_[2], S1 = v_[3], S2 = v_[4], M1 = v_[5], M2 = v_[6])
  })
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", "LexNLP", "\\%", "spaCy", "\\%", "Patterns", "\\%"),
      .spec   = c(oa_col_text(.share = 0.28), rep(oa_col_num(.mm = 17), 6L))
    ),
    .note    = paste(
      "What each extractor proposed on the", format(tab_$nDocs[1], big.mark = ","), "contracts of the labeled",
      "sample: LexNLP, spaCy (its transformer model) and the pattern and gazetteer extractors written for the",
      "database (Patterns). Under each, the number of text spans proposed and the share of contracts",
      "in which the extractor found at least one. A dash marks an entity the extractor does not attempt. Coverage is",
      "not a quality measure: an extractor that tags every capitalized word reaches complete coverage."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# 3. Where each contract's end date comes from --------------------------------------------------------------------------

#' Where each contract's end date comes from, and what the two durations look like
#'
#' The cascade answers from the first of four sources present, so the rungs partition the sample: a stated term, an
#' open-ended clause, which establishes that there is no end date, a future date beside a termination cue, and the
#' farthest future date. The naive measure uses the last of these alone. Both durations run from the same start, so
#' they differ only in the end they take, and the quartiles below are over the contracts each measure is defined on.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Rung, Label, N, Share, plus the quartiles of the rule-based and the naive duration.
oad_data_duration <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  con_ <- oad_read_sample(
    .path_contracts = .path_contracts,
    .cols           = c("DurationSource", "DurationDropped", "DurationYears", "NaiveYears")
  )
  rungs_ <- tibble::tribble(
    ~Rung,     ~Label,
    "term",    "A stated term",
    "open",    "An open-ended clause",
    "cue",     "A future date beside a termination cue",
    "maxdate", "The farthest future date",
    "none",    "No end established"
  )
  q_ <- function(.x, .p) {
    x_ <- .x[!is.na(.x)]
    if (length(x_) == 0L) return(NA_real_)
    unname(stats::quantile(x_, probs = .p, type = 7L))
  }
  n_all_ <- nrow(con_)
  by_rung_ <- con_ |>
    dplyr::mutate(Rung = dplyr::coalesce(.data$DurationSource, "none")) |>
    dplyr::summarise(
      N       = dplyr::n(),
      Defined = sum(!is.na(.data$DurationYears)),
      Q1      = q_(.x = .data$DurationYears, .p = 0.25),
      Med     = q_(.x = .data$DurationYears, .p = 0.50),
      Q3      = q_(.x = .data$DurationYears, .p = 0.75),
      .by     = "Rung"
    )
  out_ <- rungs_ |>
    dplyr::left_join(by_rung_, by = "Rung") |>
    dplyr::mutate(dplyr::across(c("N", "Defined"), \(.x) dplyr::coalesce(.x, 0L))) |>
    dplyr::mutate(Kind = "rung")
  # THE TWO MEASURES, each over the contracts it is defined on, so the comparison is of what each one yields
  totals_ <- tibble::tibble(
    Rung    = c("all-rule", "all-naive"),
    Label   = c("Rule-based duration", "Naive duration"),
    N       = c(n_all_, n_all_),
    Defined = c(sum(!is.na(con_$DurationYears)), sum(!is.na(con_$NaiveYears))),
    Q1      = c(q_(.x = con_$DurationYears, .p = 0.25), q_(.x = con_$NaiveYears, .p = 0.25)),
    Med     = c(q_(.x = con_$DurationYears, .p = 0.50), q_(.x = con_$NaiveYears, .p = 0.50)),
    Q3      = c(q_(.x = con_$DurationYears, .p = 0.75), q_(.x = con_$NaiveYears, .p = 0.75)),
    Kind    = "measure"
  )
  # A COMPLETE ACCOUNT OF WHAT IS MISSING. The panel is built over the contracts that have no duration, so its rows
  # sum to exactly that number; a contract whose reason the release does not record is a row of its own rather than
  # a silent remainder.
  dropped_ <- con_ |>
    dplyr::filter(is.na(.data$DurationYears)) |>
    dplyr::mutate(Reason = dplyr::coalesce(.data$DurationDropped, "unrecorded")) |>
    dplyr::mutate(Reason = dplyr::if_else(.data$Reason == "kept", "unrecorded", .data$Reason)) |>
    dplyr::count(.data$Reason, name = "N") |>
    dplyr::transmute(
      Rung    = paste0("dropped-", .data$Reason),
      Label   = dplyr::case_match(
        .data$Reason,
        "capped"     ~ "Longer than thirty years, dropped",
        "negative"   ~ "The end precedes the start, dropped",
        "no end"     ~ "No end date established",
        "unrecorded" ~ "No reason recorded",
        .default     = .data$Reason
      ),
      N       = .data$N,
      Defined = 0L,
      Q1      = NA_real_,
      Med     = NA_real_,
      Q3      = NA_real_,
      Kind    = "dropped"
    ) |>
    dplyr::arrange(dplyr::desc(.data$N))
  dplyr::bind_rows(out_, dropped_, totals_) |>
    dplyr::mutate(Share = .data$N / n_all_, Contracts = n_all_) |>
    dplyr::select("Rung", "Label", "Kind", "N", "Share", "Defined", "Q1", "Med", "Q3", "Contracts")
}

#' Build the table of duration sources
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_duration <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "DurationRungs"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = c(.path_contracts, .path_lib), .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oad_data_duration(.path_contracts = .path_contracts)
  num_ <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)
  yrs_ <- function(.x) dplyr::if_else(is.na(.x), "--", formatC(.x, format = "f", digits = 1L))
  panel_ <- c(rung = "Which rung answered", dropped = "Why a duration is missing",
              measure = "The two measures, over the contracts each is defined on")
  cells_ <- tab_ |>
    dplyr::transmute(
      Panel   = unname(panel_[.data$Kind]),
      Label   = oa_tex_escape(.x = .data$Label),
      N       = num_(.x = .data$N),
      Share   = paste0(formatC(100 * .data$Share, format = "f", digits = 1L), "\\%"),
      Defined = dplyr::if_else(.data$Kind == "dropped", "--", num_(.x = .data$Defined)),
      Q1      = yrs_(.x = .data$Q1),
      Med     = yrs_(.x = .data$Med),
      Q3      = yrs_(.x = .data$Q3)
    )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Source", "Contracts", "Share", "Defined", "25th", "Median", "75th"),
      .spec   = c(oa_col_text(.share = 0.25), oa_col_num(.mm = 16), oa_col_num(.mm = 12),
                  oa_col_num(.mm = 15), oa_col_num(.mm = 11), oa_col_num(.mm = 13), oa_col_num(.mm = 11))
    ),
    .note    = paste(
      "The unique contracts of the descriptive sample. The cascade takes the first source present, so the rungs",
      "partition the sample: a stated term; an open-ended clause, which establishes that the contract has no end",
      "date and therefore no duration; a future date within a termination cue's reach; and the farthest future date.",
      "Defined counts the contracts of the row for which a duration could be computed, and the quartiles are in",
      "years, over those contracts. Shares are of all contracts throughout, so the second panel accounts for every",
      "contract that lacks a duration and the first for every contract.",
      "The naive duration takes the farthest future date for every contract, and both",
      "measures run from the same start date, so they differ only in the end they take."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' The counts the text of Appendix A cites, which no table prints
#'
#' Three passages name numbers that belong to no exhibit: what each quality rule flags, what the confidential
#' treatment orders cover, and how many Item 1.01 announcements the sample keeps. They are computed here, written as
#' a tibble like any other exhibit's data, and read by the numbers spec; nothing is printed.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .path_orders Character. The release's CtoOrders.parquet, one row per reference an order makes.
#' @return Tibble: Key, Value.


# 4. Where candidates fall, and how far the extractors agree: computed from 04A's stores ---------------------------------
# 04A reports positions, contrast, consensus and pairwise agreement in its runbook and saves none of them. They are
# recomputed here from the same stores, with the same definitions: every span binned by its position in its document;
# contrast as the share in the first and last decile over the share through the middle deciles; overlapping spans of
# one entity in one document merged into a mention, whatever produced them; consensus as the number of producers that
# found each mention; agreement as the Jaccard index over mentions, and exact agreement as the share of mentions both
# found whose boundaries coincide.

#' Every span of the entities compared, from the three stores, as one table in DuckDB
#'
#' Opens the three stores read-only in one connection, checks that each entity table carries a document key and
#' half-open offsets, and unions the spans of the entities asked for into a temporary table on the connection:
#' Producer, Entity, DocID, Start, Stop. spaCy is represented by one model. The connection is returned so that the
#' callers can run their SQL on the table; they close it.
#'
#' @param .dir_store Character. 04A's Output, holding matcon.duckdb, spacy.duckdb and lexnlp.duckdb.
#' @param .spacy_model Character. The spaCy model to include.
#' @param .entities Character. The entity tables to read, upper case.
#' @return A DBI connection holding the temporary table "spans".
oad_spans_open <- function(.dir_store, .spacy_model, .entities) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .spacy_model <- "en_core_web_trf"
    .entities    <- c("ORG", "PERSON", "GPE", "LAW", "DATE", "TERM", "MONEY", "REDACT")
  }
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  fam_ <- c("lexnlp", "spacy", "matcon")
  for (f_ in fam_) {
    p_ <- fs::path(.dir_store, paste0(f_, ".duckdb"))
    if (!fs::file_exists(p_)) {
      DBI::dbDisconnect(con_, shutdown = TRUE)
      cli::cli_abort("04A's store {.file {p_}} does not exist.")
    }
    DBI::dbExecute(con_, sprintf("ATTACH '%s' AS %s (READ_ONLY)", as.character(p_), f_))
  }
  parts_ <- character(0)
  for (f_ in fam_) {
    tabs_ <- DBI::dbGetQuery(con_, sprintf(
      "SELECT table_name FROM information_schema.tables WHERE table_catalog = '%s' AND table_schema = 'main'", f_
    ))$table_name
    for (e_ in .entities) {
      t_ <- tolower(e_)
      if (!t_ %in% tabs_) next
      cols_ <- DBI::dbListFields(con_, DBI::Id(catalog = f_, schema = "main", table = t_))
      need_ <- setdiff(c("DocID", "Start", "Stop"), cols_)
      if (length(need_) > 0L) {
        DBI::dbDisconnect(con_, shutdown = TRUE)
        cli::cli_abort("{f_}.{t_} lacks {.field {need_}}; it has {.field {cols_}}.")
      }
      prod_  <- if (f_ == "spacy") paste0("spacy:", sub("^en_core_web_", "", .spacy_model)) else f_
      where_ <- if (f_ == "spacy" && "Model" %in% cols_) sprintf(" WHERE Model = '%s'", .spacy_model) else ""
      parts_ <- c(parts_, sprintf(
        "SELECT '%s' AS Producer, '%s' AS Entity, DocID, CAST(Start AS BIGINT) AS Start, CAST(Stop AS BIGINT) AS Stop
         FROM %s.main.%s%s", prod_, e_, f_, t_, where_
      ))
    }
  }
  if (length(parts_) == 0L) {
    DBI::dbDisconnect(con_, shutdown = TRUE)
    cli::cli_abort("None of the entity tables asked for exists in the three stores.")
  }
  DBI::dbExecute(con_, paste("CREATE TEMP TABLE spans AS", paste(parts_, collapse = " UNION ALL ")))
  con_
}

#' Positions and contrast: where in its document each candidate falls, per producer and entity
#'
#' @param .con A connection from oad_spans_open().
#' @param .path_sample Character. 04A's sample_text.parquet, for the length of every document.
#' @param .bins Integer. Bins per document, a multiple of ten.
#' @return List of two tibbles: positions (Producer, Entity, Bin, N, Share) and contrast (Producer, Entity,
#'   MidShare, EndShare, Contrast).
oad_data_positions <- function(.con, .path_sample, .bins = 30L) {
  if (FALSE) {
    .con         <- oad_spans_open(here::here("2_output", "04A-EntityExtract", "Output"), "en_core_web_trf", "ORG")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
    .bins        <- 30L
  }
  # THE TEXT EVERY OFFSET INDEXES: DocID and TextRaw. Its length in code points is what the offsets are relative to.
  len_ <- arrow::open_dataset(.path_sample) |>
    dplyr::select("DocID", "TextRaw") |>
    dplyr::collect() |>
    dplyr::transmute(DocID = .data$DocID, Length = nchar(.data$TextRaw, type = "chars"))
  DBI::dbWriteTable(.con, "doclen", as.data.frame(len_), temporary = TRUE, overwrite = TRUE)
  pos_ <- DBI::dbGetQuery(.con, sprintf(
    "SELECT s.Producer, s.Entity,
            LEAST(%d, 1 + CAST(FLOOR(%d * s.Start / GREATEST(d.Length, 1)) AS INTEGER)) AS Bin,
            COUNT(*) AS N
     FROM spans s JOIN doclen d USING (DocID)
     GROUP BY 1, 2, 3", .bins, .bins
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(N = as.integer(.data$N)) |>
    tidyr::complete(tidyr::nesting(Producer, Entity), Bin = seq_len(.bins), fill = list(N = 0L)) |>
    dplyr::mutate(Share = .data$N / sum(.data$N), .by = c("Producer", "Entity")) |>
    dplyr::arrange(.data$Producer, .data$Entity, .data$Bin)
  # CONTRAST: the first and last decile against the middle, the second and ninth deciles left out as shoulders
  dec_ <- .bins / 10L
  con_ <- pos_ |>
    dplyr::mutate(Zone = dplyr::case_when(
      .data$Bin <= dec_ | .data$Bin > .bins - dec_ ~ "end",
      .data$Bin <= 2L * dec_ | .data$Bin > .bins - 2L * dec_ ~ "shoulder",
      .default = "mid"
    )) |>
    dplyr::filter(.data$Zone != "shoulder") |>
    dplyr::summarise(Share = mean(.data$Share), .by = c("Producer", "Entity", "Zone")) |>
    tidyr::pivot_wider(names_from = "Zone", values_from = "Share") |>
    dplyr::transmute(Producer = .data$Producer, Entity = .data$Entity, MidShare = .data$mid, EndShare = .data$end,
                     Contrast = .data$end / .data$mid)
  list(positions = pos_, contrast = con_)
}

#' Consensus and pairwise agreement over mentions
#'
#' Overlapping spans of one entity in one document, from any producer, are merged into a mention by gaps and
#' islands; a mention's producers are whoever contributed a span to it. Consensus counts mentions by how many
#' producers found them. Pairwise agreement, for every pair of producers that attempt an entity, is the Jaccard
#' index over mentions -- both over either -- and exact agreement is the share of mentions both found on which
#' their outermost boundaries coincide.
#'
#' @param .con A connection from oad_spans_open().
#' @return List of two tibbles: consensus (Entity, NProducer, NMentions, Share, Eligible) and pairwise (Entity,
#'   ProducerA, ProducerB, Both, Exact, MentionsA, MentionsB, Jaccard, ExactShare).
oad_data_agreement <- function(.con) {
  if (FALSE) .con <- oad_spans_open(here::here("2_output", "04A-EntityExtract", "Output"), "en_core_web_trf", "GPE")
  DBI::dbExecute(.con, "
    CREATE OR REPLACE TEMP TABLE mentions AS
    WITH o AS (
      SELECT *, MAX(Stop) OVER (PARTITION BY DocID, Entity ORDER BY Start, Stop
                                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS PrevMax
      FROM spans
    ),
    g AS (
      SELECT *, CASE WHEN PrevMax IS NULL OR Start >= PrevMax THEN 1 ELSE 0 END AS NewGroup FROM o
    ),
    m AS (
      SELECT *, SUM(NewGroup) OVER (PARTITION BY DocID, Entity ORDER BY Start, Stop
                                    ROWS UNBOUNDED PRECEDING) AS MentionID
      FROM g
    )
    SELECT DocID, Entity, MentionID, Producer, MIN(Start) AS Start, MAX(Stop) AS Stop
    FROM m GROUP BY 1, 2, 3, 4
  ")
  cons_ <- DBI::dbGetQuery(.con, "
    WITH per AS (SELECT Entity, DocID, MentionID, COUNT(DISTINCT Producer) AS NProducer
                 FROM mentions GROUP BY 1, 2, 3),
         elig AS (SELECT Entity, COUNT(DISTINCT Producer) AS Eligible FROM spans GROUP BY 1)
    SELECT p.Entity, p.NProducer, COUNT(*) AS NMentions, e.Eligible
    FROM per p JOIN elig e USING (Entity) GROUP BY 1, 2, 4 ORDER BY 1, 2
  ") |>
    tibble::as_tibble() |>
    dplyr::mutate(NMentions = as.integer(.data$NMentions), Eligible = as.integer(.data$Eligible)) |>
    dplyr::mutate(Share = .data$NMentions / sum(.data$NMentions), .by = "Entity")
  pair_ <- DBI::dbGetQuery(.con, "
    WITH prods AS (SELECT DISTINCT Entity, Producer FROM spans),
         pairs AS (SELECT a.Entity, a.Producer AS A, b.Producer AS B
                   FROM prods a JOIN prods b ON a.Entity = b.Entity AND a.Producer < b.Producer),
         cnt AS (SELECT Entity, Producer, COUNT(*) AS N FROM mentions GROUP BY 1, 2),
         overlap AS (SELECT x.Entity, x.Producer AS A, y.Producer AS B,
                         COUNT(*) AS Both,
                         SUM(CASE WHEN x.Start = y.Start AND x.Stop = y.Stop THEN 1 ELSE 0 END) AS Exact
                  FROM mentions x JOIN mentions y
                    ON x.Entity = y.Entity AND x.DocID = y.DocID AND x.MentionID = y.MentionID
                   AND x.Producer < y.Producer
                  GROUP BY 1, 2, 3)
    SELECT p.Entity, p.A AS ProducerA, p.B AS ProducerB,
           COALESCE(b.Both, 0) AS Both, COALESCE(b.Exact, 0) AS Exact,
           ca.N AS MentionsA, cb.N AS MentionsB
    FROM pairs p
    LEFT JOIN overlap b ON b.Entity = p.Entity AND b.A = p.A AND b.B = p.B
    JOIN cnt ca ON ca.Entity = p.Entity AND ca.Producer = p.A
    JOIN cnt cb ON cb.Entity = p.Entity AND cb.Producer = p.B
    ORDER BY 1, 2, 3
  ") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(c("Both", "Exact", "MentionsA", "MentionsB"), as.integer),
      Jaccard    = .data$Both / (.data$MentionsA + .data$MentionsB - .data$Both),
      ExactShare = dplyr::if_else(.data$Both > 0L, .data$Exact / .data$Both, NA_real_)
    )
  list(consensus = cons_, pairwise = pair_)
}

#' The positions figure: share of candidates by position in the document, one panel per entity, one line per producer
#'
#' @param .tab Tibble: Producer, Entity, Bin, Share.
#' @return A ggplot.
oad_plot_positions <- function(.tab) {
  if (FALSE) {
    .tab <- tibble::tibble(Producer = "lexnlp", Entity = "ORG", Bin = 1:30, Share = rep(1 / 30, 30))
  }
  ent_ <- c("ORG", "PERSON", "GPE", "LAW", "DATE", "TERM", "REDACT")
  lab_ <- c(ORG = "Organizations", PERSON = "Persons", GPE = "Places", LAW = "Governing law", DATE = "Dates",
            TERM = "Stated periods", REDACT = "Redaction markers")
  prod_ <- c("lexnlp", "spacy:trf", "matcon")
  plab_ <- c(lexnlp = "LexNLP", `spacy:trf` = "spaCy", matcon = "Patterns")
  tab_ <- .tab |>
    dplyr::filter(.data$Entity %in% ent_, .data$Producer %in% prod_) |>
    dplyr::mutate(
      Entity   = factor(unname(lab_[.data$Entity]), levels = unname(lab_)),
      Producer = factor(unname(plab_[.data$Producer]), levels = unname(plab_)),
      Position = (.data$Bin - 0.5) / max(.data$Bin)
    )
  ggplot2::ggplot(tab_, ggplot2::aes(x = .data$Position, y = .data$Share, colour = .data$Producer)) +
    ggplot2::geom_line(linewidth = 0.45) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Entity), ncol = 4L, scales = "free_y") +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks = c(0.25, 0.5, 0.75)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_colour_manual(values = plot_pal_cat(.n = 3L), drop = FALSE) +
    ggplot2::labs(x = "Position in the contract", y = "Share of the producer's candidates", colour = NULL) +
    plot_theme(.grid = "y", .legend = "bottom") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

#' The agreement figure: pairwise agreement over mentions, one panel per entity two producers reach
#'
#' @param .tab Tibble: Entity, ProducerA, ProducerB, Jaccard.
#' @return A ggplot.
oad_plot_agreement <- function(.tab) {
  if (FALSE) .tab <- tibble::tibble(Entity = "GPE", ProducerA = "lexnlp", ProducerB = "matcon", Jaccard = 0.66)
  plab_ <- c(lexnlp = "LexNLP", `spacy:trf` = "spaCy", matcon = "Patterns")
  lab_  <- c(ORG = "Organizations", GPE = "Places", DATE = "Dates")
  lvl_  <- unname(plab_)
  pairs_ <- .tab |>
    dplyr::filter(.data$Entity %in% names(lab_)) |>
    dplyr::transmute(Entity = .data$Entity, A = unname(plab_[.data$ProducerA]), B = unname(plab_[.data$ProducerB]),
                     Jaccard = .data$Jaccard) |>
    # THE LOWER TRIANGLE: the row is the later producer, the column the earlier, whichever order the pair arrived
    # in, so no pair lands above the diagonal and leaves its mirror cell empty (spaCy / Patterns, 23 Sep 2026).
    dplyr::mutate(
      Lo = pmin(match(.data$A, lvl_), match(.data$B, lvl_)),
      Hi = pmax(match(.data$A, lvl_), match(.data$B, lvl_)),
      A  = lvl_[.data$Lo],
      B  = lvl_[.data$Hi]
    ) |>
    dplyr::select(-"Lo", -"Hi")
  diag_ <- pairs_ |>
    dplyr::select("Entity", "A", "B") |>
    tidyr::pivot_longer(cols = c("A", "B"), values_to = "P") |>
    dplyr::distinct(.data$Entity, .data$P) |>
    dplyr::transmute(Entity = .data$Entity, A = .data$P, B = .data$P, Jaccard = 1)
  cells_ <- dplyr::bind_rows(pairs_, diag_) |>
    dplyr::mutate(
      A      = factor(.data$A, levels = lvl_),
      B      = factor(.data$B, levels = rev(lvl_)),
      Entity = factor(unname(lab_[.data$Entity]), levels = unname(lab_)),
      Label  = formatC(.data$Jaccard, format = "f", digits = 2L),
      Dark   = .data$Jaccard > 0.5
    )
  ggplot2::ggplot(cells_, ggplot2::aes(x = .data$A, y = .data$B, fill = .data$Jaccard)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Label, colour = .data$Dark), family = .plot_font, size = 3.2,
                       show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    ggplot2::scale_fill_gradient(low = "#EEF0F4", high = plot_pal_cat(.n = 1L), limits = c(0, 1)) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Entity), ncol = 2L, scales = "free") +
    ggplot2::labs(x = NULL, y = NULL, fill = "Agreement") +
    plot_theme(.grid = "none", .legend = "right") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Build the positions and the agreement figures together, from one pass over the stores
#'
#' One connection, one spans table, both computations; written as two exhibits. The build is keyed on the three
#' stores and this library.
#'
#' @param .dir_store Character. 04A's Output, holding the three stores.
#' @param .path_sample Character. 04A's sample_text.parquet.
#' @param .spacy_model Character. The spaCy model compared.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_alignment <- function(.dir_store, .path_sample, .spacy_model, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
    .spacy_model <- "en_core_web_trf"
    .dir_own     <- here::here("2_output", "50-Numbers", "Output")
    .force       <- FALSE
    .path_lib    <- here::here("1_code", "50-Numbers.R")
  }
  names_ <- c("EntityPositions", "EntityAgreement")
  outs_  <- unlist(purrr::map(names_, \(.n) c(
    fs::path(.dir_own, "Figures", paste0(.n, c(".pdf", ".png"))),
    fs::path(.dir_own, c("Notes", "Data"), paste0(.n, c(".tex", ".parquet")))
  )))
  ins_   <- c(fs::path(.dir_store, paste0(c("lexnlp", "spacy", "matcon"), ".duckdb")), .path_sample, .path_lib)
  if (!all(fs::file_exists(ins_[1:4]))) return(invisible("waiting for 04A's stores"))
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  con_ <- oad_spans_open(
    .dir_store   = .dir_store,
    .spacy_model = .spacy_model,
    .entities    = c("ORG", "PERSON", "GPE", "LAW", "DATE", "TERM", "MONEY", "REDACT")
  )
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
  pos_ <- oad_data_positions(.con = con_, .path_sample = .path_sample)
  agr_ <- oad_data_agreement(.con = con_)
  oa_write_exhibit(
    .name    = names_[1L],
    .lines   = NULL,
    .note    = paste(
      "Every candidate span each extractor proposed on the labeled sample, by its position in the contract: the",
      "contract is divided into thirty bins of equal length, and each line is the share of the producer's candidates",
      "for that entity that fall in each bin. LexNLP, spaCy (its transformer model) and the pattern and gazetteer",
      "extractors written for the database (Patterns); an extractor absent from a panel does not attempt that",
      "entity. A producer that spreads an entity evenly through the text draws a flat line."
    ),
    .data    = dplyr::left_join(pos_$positions, pos_$contrast, by = c("Producer", "Entity")),
    .dir_own = .dir_own
  )
  plot_save(
    .plot   = oad_plot_positions(.tab = pos_$positions),
    .name   = names_[1L],
    .dir    = fs::path(.dir_own, "Figures"),
    .height = 4.6
  )
  oa_write_exhibit(
    .name    = names_[2L],
    .lines   = NULL,
    .note    = paste(
      "Agreement between extractors on the labeled sample, for the entities two of them attempt. Overlapping spans",
      "of one entity within a contract are merged into a mention -- a place in the text where something was found",
      "-- and agreement is the Jaccard index over mentions: the share of mentions both producers found among those",
      "either found. It measures agreement about what is there; agreement about where a mention ends is a separate",
      "quantity and is not shown. The diagonal is a producer against itself."
    ),
    .data    = dplyr::bind_rows(dplyr::mutate(agr_$pairwise, Block = "pairwise"),
                                dplyr::mutate(agr_$consensus, Block = "consensus")),
    .dir_own = .dir_own
  )
  plot_save(
    .plot   = oad_plot_agreement(.tab = agr_$pairwise),
    .name   = names_[2L],
    .dir    = fs::path(.dir_own, "Figures"),
    .height = 4.4
  )
  invisible("built")
}


# 5. What the variables are worth under the naive and the rule-based reading --------------------------------------------

#' The values companion's data: by category, the typical value of each measure under both readings
#'
#' 30's contrast table counts on how many contracts each measure is defined; this one reports what it is worth on
#' them. Duration: the median years under the naive end (the farthest future date) and under the cascade. Parties:
#' the mean number of distinct organization spellings (naive) and of registrants, co-registrants and counterparties
#' (rule). Countries and states: the mean number under the naive count (any mention outside a governing-law clause)
#' and attached to the registrant and to the counterparties by the 200-character rule, the two reported apart
#' because a country attached to both is one country.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Row, Kind, Level1, Class, N, and one column per measure and reading.
oad_data_values <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  cols_ <- c("Class", "DurationYears", "NaiveYears", "nUniSpellingsNaive", "nUniRegistrant",
             "nUniCofiler", "nUniCounterparty", "nUniCountryNaive", "nUniCountryRegistrant",
             "nUniCountryCounterparty", "nUniStateNaive", "nUniStateRegistrant", "nUniStateCounterparty")
  con_ <- oad_read_sample(
    .path_contracts = .path_contracts,
    .cols           = cols_
  ) |>
    dplyr::mutate(
      # THE TWO LEVELS, from the release's Class: "Employment: Compensation" is parent and sub; "Licenses" is both;
      # two Purchases-and-Sales sub-categories are released without their parent and are put back under it.
      Level2 = dplyr::if_else(grepl(": ", .data$Class, fixed = TRUE), sub("^.*: ", "", .data$Class), .data$Class),
      Level2 = dplyr::if_else(.data$Level2 == "Investment and Merger", "M&A", .data$Level2),
      Level1 = dplyr::case_when(
        grepl(": ", .data$Class, fixed = TRUE)            ~ sub(": .*$", "", .data$Class),
        .data$Class %in% c("R&D", "Customer / Supplier") ~ "Purchases and Sales",
        .default                                          = .data$Class
      ),
      PartiesRule = dplyr::coalesce(.data$nUniRegistrant, 0L) + dplyr::coalesce(.data$nUniCofiler, 0L) +
        dplyr::coalesce(.data$nUniCounterparty, 0L)
    )
  stat_ <- function(.d) {
    tibble::tibble(
      N            = nrow(.d),
      DurNaive     = stats::median(.d$NaiveYears, na.rm = TRUE),
      DurRule      = stats::median(.d$DurationYears, na.rm = TRUE),
      PartNaive    = mean(.d$nUniSpellingsNaive, na.rm = TRUE),
      PartRule     = mean(.d$PartiesRule, na.rm = TRUE),
      CtryNaive    = mean(.d$nUniCountryNaive, na.rm = TRUE),
      CtryReg      = mean(.d$nUniCountryRegistrant, na.rm = TRUE),
      CtryCpty     = mean(.d$nUniCountryCounterparty, na.rm = TRUE),
      StateNaive   = mean(.d$nUniStateNaive, na.rm = TRUE),
      StateReg     = mean(.d$nUniStateRegistrant, na.rm = TRUE),
      StateCpty    = mean(.d$nUniStateCounterparty, na.rm = TRUE)
    )
  }
  lab_ <- dplyr::filter(con_, !is.na(.data$Class))
  # THE ROWS: parents with their sub-categories, leaf parents alone, then the total, in taxonomic order
  l1_ <- c("Financial Instruments", "Employment", "Purchases and Sales", "Licenses", "Leases", "Business Structure",
           "Other")
  rows_ <- purrr::map_dfr(l1_, \(.p) {
    d1_ <- dplyr::filter(lab_, .data$Level1 == .p)
    if (nrow(d1_) == 0L) return(NULL)
    order_ <- c("Credit", "Equity", "Compensation", "Legal", "Assets", "R&D", "Customer / Supplier",
                "Peer Agreements", "M&A")
    subs_ <- unique(d1_$Level2[!is.na(d1_$Level2) & d1_$Level2 != .p])
    subs_ <- subs_[order(match(subs_, order_))]
    top_ <- dplyr::bind_cols(tibble::tibble(Row = .p, Kind = "super", Level1 = .p, Class = NA_character_),
                             stat_(.d = d1_))
    if (length(subs_) == 0L) return(top_)
    sub_ <- purrr::map_dfr(subs_, \(.s) {
      d2_ <- dplyr::filter(d1_, .data$Level2 == .s)
      dplyr::bind_cols(tibble::tibble(Row = .s, Kind = "sub", Level1 = .p, Class = d2_$Class[1]), stat_(.d = d2_))
    })
    dplyr::bind_rows(top_, sub_)
  })
  dplyr::bind_rows(
    rows_,
    dplyr::bind_cols(tibble::tibble(Row = "Total", Kind = "total", Level1 = NA_character_, Class = NA_character_),
                     stat_(.d = con_))
  )
}

#' Build the values companion
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_values <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "ContentValues"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = c(.path_contracts, .path_lib), .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oad_data_values(.path_contracts = .path_contracts)
  f1_ <- function(.x) formatC(.x, format = "f", digits = 1L)
  f2_ <- function(.x) formatC(.x, format = "f", digits = 2L)
  lab_ <- dplyr::case_when(
    tab_$Kind == "total" ~ paste0("\\textbf{", oa_tex_escape(.x = tab_$Row), "}"),
    tab_$Kind == "sub"   ~ paste0("\\hspace{1em}", oa_tex_escape(.x = tab_$Row)),
    .default             = paste0("\\textbf{", oa_tex_escape(.x = tab_$Row), "}")
  )
  cells_ <- tibble::tibble(
    Row        = lab_,
    DurNaive   = f1_(.x = tab_$DurNaive),
    DurRule    = f1_(.x = tab_$DurRule),
    PartNaive  = f2_(.x = tab_$PartNaive),
    PartRule   = f2_(.x = tab_$PartRule),
    CtryNaive  = f2_(.x = tab_$CtryNaive),
    CtryReg    = f2_(.x = tab_$CtryReg),
    CtryCpty   = f2_(.x = tab_$CtryCpty),
    StateNaive = f2_(.x = tab_$StateNaive),
    StateReg   = f2_(.x = tab_$StateReg),
    StateCpty  = f2_(.x = tab_$StateCpty)
  )
  lines_ <- oa_frame_table(
    .tab    = cells_,
    .header = c("", "Naive", "Rule", "Naive", "Rule", "Naive", "Reg.", "Cpty.", "Naive", "Reg.", "Cpty."),
    .spec   = c(oa_col_text(.share = 0.26), rep(oa_col_num(.mm = 10), 10L))
  )
  # A GROUP ROW ABOVE THE COLUMN HEADERS, so the reader sees which measure a Naive / Rule pair belongs to. The
  # frame writes one header row; the group row is put in above it, right after the double rule.
  group_ <- paste(
    " & \\multicolumn{2}{c}{Duration (years)} & \\multicolumn{2}{c}{Parties} & \\multicolumn{3}{c}{Countries}",
    "& \\multicolumn{3}{c}{States} \\\\"
  )
  top_ <- which(lines_ == "\\hline\\hline")[1L]
  if (is.na(top_)) cli::cli_abort("{name_}: the frame has no double rule to put the group row under.")
  lines_ <- append(lines_, group_, after = top_)
  oa_write_exhibit(
    .name    = name_,
    .lines   = lines_,
    .note    = paste(
      "The companion to the coverage contrast: what each measure is worth, by category, on the unique contracts of",
      "the descriptive sample, under the naive reading of the spans and under the rule. Duration (years) is the",
      "median over the contracts on which each reading defines it: the naive end is the farthest future date, the",
      "rule's end the first source present of a stated term, an open-ended clause, a cued date and the farthest",
      "date, dropped above thirty years. Parties is the mean number per contract of distinct organization spellings",
      "(naive) and of registrants, co-registrants and counterparties (rule). Countries and states are the mean",
      "number per contract of distinct mentions outside a governing-law clause (naive) and of those attached to",
      "the registrant (Reg.) and to the counterparties (Cpty.) by the 200-character rule, reported apart because a",
      "place attached to both is one place. The column groups are, from left to right, duration, parties, countries",
      "and states."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# ====================================================================================================================
# Appendix B -- software: the builders moved here from 42-OnlineAppendix-B, unchanged; they write into 40's Output/
# ====================================================================================================================

# ======================================================================================================================
# 40-OnlineAppendix-B.R -- Appendix B, the software: the exhibits this chapter builds
# ======================================================================================================================
#
# 40-OnlineAppendix-B.qmd holds the chapter's text; _Commons/_OnlineAppendix.R holds everything the chapters share.
# This library holds the two things Appendix B writes itself: the pipeline table, whose content is text rather than
# data and is kept here as a tibble so that it deploys through the same machinery as every other exhibit, and the
# versions of the two packages, read from the installed packages so that the text cannot name a version that is not
# the one the database was built with.
#
# THE PREFIX IS oab_: online appendix, chapter B.


# 1. The pipeline, stage by stage ---------------------------------------------------------------------------------------

#' The pipeline table's content: six stages, what each does, what it produces, where it is described
#'
#' Written as a tibble rather than as prose in the text so that the table is an exhibit like any other -- built,
#' numbered, deployed and read through \\oaown -- and so that its wording is in one place. It names no script: the
#' stages are the ones a reader needs to place the chapters, and the code's own numbering is not part of the paper.
#'
#' @return Tibble: Stage, Does, Produces, Where.
oab_data_stages <- function() {
  tibble::tribble(
    ~Stage, ~Does, ~Produces, ~Where,
    "Acquisition",
    paste("Reads EDGAR's index, visits every selected filing, downloads its documents and converts them",
          "to text"),
    "Every Exhibit 10, Item 1.01 current report and confidential treatment order, as text",
    "A.1; rGetEDGAR (B.2)",
    "Database",
    paste("Identifies unique attachments, flags text that failed conversion, links orders and",
          "announcements to contracts"),
    "One row per contract, with its filing, its copies and its links",
    "A.4",
    "Labelling",
    "Two annotators assign each contract of a stratified sample a category and an amendment flag",
    "The labeled sample",
    "C.1; rLabelDocs (B.3)",
    "Classification",
    paste("Fine-tunes a transformer and derives a keyword table on the labeled sample, out of fold, and",
          "applies both to the corpus"),
    "A category, its probability and a keyword label per contract",
    "C; the classifier (B.4)",
    "Entity extraction",
    paste("Finds partners, places, dates, amounts and redaction markers as spans, and turns spans into",
          "variables by rule"),
    "The content variables per contract",
    "D; the extractors (B.4)",
    "Release",
    "Exports the contract rows, the linked orders and announcements, and the codebook",
    "The published files",
    "F"
  )
}

#' Build the pipeline table
#'
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library, the table's only input.
#' @return Invisibly, the build's status.
oab_build_stages <- function(.dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_own  <- here::here("2_output", "50-Numbers", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "50-Numbers.R")
  }
  name_ <- "PipelineStages"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = .path_lib, .force = .force)) return(invisible("up to date"))
  tab_ <- oab_data_stages()
  cells_ <- tab_ |>
    dplyr::transmute(
      Stage    = paste0("\\textbf{", oa_tex_escape(.x = .data$Stage), "}"),
      Does     = oa_tex_escape(.x = .data$Does),
      Produces = oa_tex_escape(.x = .data$Produces),
      Where    = oa_tex_escape(.x = .data$Where)
    )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Stage", "What it does", "What it produces", "Described in"),
      .spec   = c(oa_col_text(.share = 0.14), oa_col_text(.share = 0.40), oa_col_text(.share = 0.28),
                  oa_col_text(.share = 0.14))
    ),
    .note    = paste(
      "The six stages of the pipeline, in the order they run. Each stage reads what the previous one produced and",
      "writes its own result, so a stage can be rerun without repeating the ones before it. The two R packages and",
      "the classification and extraction code described in this appendix are the software behind the stages named",
      "beside them; the remaining stages are scripts of the pipeline itself."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# 2. The package versions, read from the packages -----------------------------------------------------------------------

#' The versions of the two packages, as installed
#'
#' The text names the versions the database was built with. Read from the installed packages rather than typed, so
#' the sentence follows an upgrade; a package that is not installed resolves to NA and the number reports it.
#'
#' @return Tibble: Key, Value -- rGetEDGAR and rLabelDocs, as "0.1.0".
oab_data_versions <- function() {
  ver_ <- function(.pkg) {
    if (!requireNamespace(.pkg, quietly = TRUE)) return(NA_character_)
    as.character(utils::packageVersion(.pkg))
  }
  tibble::tibble(
    Key   = c("rGetEDGAR", "rLabelDocs"),
    Value = c(ver_(.pkg = "rGetEDGAR"), ver_(.pkg = "rLabelDocs"))
  )
}

#' Build the versions tibble
#'
#' Rebuilt on every render, since an installed package can change without any file of the pipeline changing.
#'
#' @param .dir_own Character. This chapter's output directory.
#' @return Invisibly, the build's status.
oab_build_versions <- function(.dir_own) {
  if (FALSE) .dir_own <- here::here("2_output", "50-Numbers", "Output")
  fs::dir_create(fs::path(.dir_own, "Data"))
  arrow::write_parquet(
    x    = oab_data_versions(),
    sink = fs::path(.dir_own, "Data", "PackageVersions.parquet")
  )
  invisible("built")
}


# ======================================================================================================================
# CHAPTER F. The data package: the numbers 50B reports about what it published
# ======================================================================================================================

# 1. The package numbers, read from 50B's report --------------------------------------------------------------------

#' The package numbers as a Key/Value tibble
#'
#' 50B writes package_numbers.csv beside the staged sample: one row per figure of the package (files and bytes per
#' folder, rows per table, spans per kind, documents per text type). Every value is read as a number; the version
#' is kept as text under its own key. Byte counts are also given in gigabytes, rounded to one decimal, since that is
#' the unit the appendix prints.
#'
#' @param .path_numbers Character. 50B's package_numbers.csv.
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
      Value = NA_real_,
      Text  = .data$Value
    )
  dplyr::bind_rows(nums_, gb_, ver_)
}

#' Build the package numbers tibble
#'
#' @param .path_numbers Character. 50B's package_numbers.csv.
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
