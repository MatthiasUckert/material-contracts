# 01A-EdgarIndex: mirror EDGAR's index and landing pages, and turn the landing HTML into a table -----------------------
#
# WHAT THIS FILE DOES
# EDGAR publishes a quarterly master index of every filing since 1993. For each filing there is a
# landing page listing the documents attached to it. 01A mirrors both locally and parses the landing
# HTML into one table, LandingPageAll.parquet, holding twenty-one metadata fields per filing.
#
# It does not download any document. That is 01B, which uses the links mirrored here to decide what
# to fetch. Splitting the two matters because they fail differently: the index is a few thousand
# files and completes in an afternoon, the documents are 1.8 million and took days.
#
# THE ACQUISITION SWITCH
# Two steps here reach the SEC over the network. Both sit behind .lP$Param$Acquire, which defaults
# to FALSE. The CHECKS never sit behind it: the document always computes what is outstanding and
# always reports it, so a stale mirror is visible in the render rather than invisible. This is the
# difference between a gated step and a skipped one, and the earlier version of this pipeline had
# the second: a single .RERUN flag suppressed the work and the diagnostic together, so a document
# with nothing to say about its own completeness looked identical to one that was complete.
#
# WHAT LIVES WHERE
# The mirror is 0_edgar/, not 2_output/. It is a local copy of somebody else's data, not something
# this script computed, and it is shared with 01B, which writes the third of its three subtrees.
# Each script's 2_output/ directory holds only what that script derived.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation.

if (FALSE) {
  .dir_master  <- .lP$Edgar$MasterIndex$DirParquet
  .path_htmls  <- .lP$Edgar$DocLinks$DirMain$HTMLs
  .path_links  <- .lP$Edgar$DocLinks$DirMain$Links
  .dir_raw     <- .lP$Cache$RawExtract
  .path_out    <- .lP$Output$LandingPageAll
  .forms       <- edg_sec_forms()
  .max_year    <- 2024L
  .workers     <- 10L
}


# 1. Vocabulary --------------------------------------------------------------------------------------------------------

#' The SEC form types that can carry a material contract
#'
#' EDGAR indexes several hundred form types. Only a minority can carry an Exhibit 10, and mirroring
#' the landing page of every filing on EDGAR would multiply the acquisition cost by roughly a factor
#' of ten for filings that cannot contain the object of study.
#'
#' The seventeen below are the periodic reports (10-K, 10-Q, 20-F), the current report that must
#' disclose a material definitive agreement (8-K), the registration statements that attach material
#' agreements as exhibits (S-1, S-4, F-1, F-4), each with its amendment variant, and CT ORDER, which
#' is the confidential-treatment order itself rather than a filing carrying an exhibit.
#'
#' Amendment variants are listed separately rather than matched by prefix. A pattern such as "^8-K"
#' would also capture 8-K12B and 8-K15D5, which are not current reports at all.
#'
#' @return Character vector of seventeen SEC form type strings, exactly as EDGAR writes them.
edg_sec_forms <- function() {
  c(
    "10-K", "10-K/A", "10-Q", "10-Q/A", "8-K", "8-K/A", "20-F", "20-F/A",
    "S-1", "S-1/A", "S-4", "S-4/A", "F-1", "F-1/A", "F-4", "F-4/A", "CT ORDER"
  )
}


# 2. Index selection ---------------------------------------------------------------------------------------------------

#' Which master-index entries need a landing page
#'
#' The master index is the population; this is the frame drawn from it. Restricting to the relevant
#' form types and to filings up to a fixed final year is the entire sample definition at this stage,
#' which is why it is a function of three explicit arguments rather than of anything read from the
#' environment.
#'
#' The year ceiling is a closure rule, not a data limit. EDGAR keeps publishing, so an open-ended
#' frame would grow between renders and no two runs of the pipeline would describe the same corpus.
#'
#' @param .dir_master Directory of master-index parquet files, from the local mirror.
#' @param .forms Character vector of form types to keep; see edg_sec_forms().
#' @param .max_year Integer. Latest year-quarter to include, as a four-digit year.
#' @return Character vector of unique HashIndex values.
edg_select_index <- function(.dir_master, .forms, .max_year) {
  if (FALSE) {
    .dir_master <- .lP$Edgar$MasterIndex$DirParquet
    .forms      <- edg_sec_forms()
    .max_year   <- 2024L
  }

  arrow::open_dataset(sources = .dir_master) |>
    dplyr::filter(.data$FormType %in% .forms) |>
    dplyr::filter(.data$YearQuarter <= .max_year) |>
    dplyr::distinct(.data$HashIndex) |>
    dplyr::collect() |>
    dplyr::pull(.data$HashIndex)
}


# 3. Mirror coverage ---------------------------------------------------------------------------------------------------

#' How much of the selected frame has actually been mirrored
#'
#' The one number that decides whether the acquisition switch needs turning on. It is computed on
#' every render, whether or not anything was acquired, because a document that cannot say how
#' complete its own inputs are is not reporting a result.
#'
#' Landing pages and link tables are counted separately. They are produced by the same request but
#' stored apart, and a gap between them is a different failure from a gap in either: it means pages
#' were fetched whose document lists could not be parsed.
#'
#' @param .dir_master Directory of master-index parquet files.
#' @param .forms Character vector of form types; see edg_sec_forms().
#' @param .max_year Integer. Latest year to include.
#' @param .path_htmls Directory of mirrored landing-page HTML.
#' @param .path_links Directory of mirrored document-link tables.
#' @return A one-row tibble: Selected, Mirrored, Missing, ShareMirrored, nLinkRows.
edg_link_coverage <- function(.dir_master, .forms, .max_year, .path_htmls, .path_links) {
  if (FALSE) {
    .dir_master <- .lP$Edgar$MasterIndex$DirParquet
    .forms      <- edg_sec_forms()
    .max_year   <- 2024L
    .path_htmls <- .lP$Edgar$DocLinks$DirMain$HTMLs
    .path_links <- .lP$Edgar$DocLinks$DirMain$Links
  }

  sel_ <- edg_select_index(.dir_master = .dir_master, .forms = .forms, .max_year = .max_year)

  have_ <- arrow::open_dataset(sources = .path_htmls) |>
    dplyr::distinct(.data$HashIndex) |>
    dplyr::collect() |>
    dplyr::pull(.data$HashIndex)

  miss_ <- length(setdiff(sel_, have_))

  tibble::tibble(
    Selected      = length(sel_),
    Mirrored      = length(sel_) - miss_,
    Missing       = miss_,
    ShareMirrored = (length(sel_) - miss_) / length(sel_),
    nLinkRows     = nrow(arrow::open_dataset(sources = .path_links))
  )
}

#' Report mirror coverage
#'
#' @param .dir_master Directory of master-index parquet files.
#' @param .forms Character vector of form types.
#' @param .max_year Integer. Latest year to include.
#' @param .path_htmls Directory of mirrored landing-page HTML.
#' @param .path_links Directory of mirrored document-link tables.
#' @param .acquire Logical. The value of the acquisition switch, reported alongside the backlog so
#'   that an incomplete mirror and a disabled download are never confused for one another.
#' @return The coverage tibble, invisibly.
edg_report_links <- function(.dir_master, .forms, .max_year, .path_htmls, .path_links, .acquire) {
  if (FALSE) {
    .dir_master <- .lP$Edgar$MasterIndex$DirParquet
    .forms      <- edg_sec_forms()
    .max_year   <- 2024L
    .path_htmls <- .lP$Edgar$DocLinks$DirMain$HTMLs
    .path_links <- .lP$Edgar$DocLinks$DirMain$Links
    .acquire    <- FALSE
  }

  cov_ <- edg_link_coverage(
    .dir_master = .dir_master,
    .forms      = .forms,
    .max_year   = .max_year,
    .path_htmls = .path_htmls,
    .path_links = .path_links
  )

  tbl_head("Mirror coverage")
  tbl_out(
    .tab    = cov_,
    .title  = NULL,
    .pct    = "ShareMirrored",
    .digits = 1L
  )

  n_ <- cov_$Missing
  if (n_ == 0L) {
    tbl_note("The mirror is complete for the selected frame.")
  } else if (isTRUE(.acquire)) {
    tbl_note("{n_} filing{?s} outstanding; acquisition is ON, so this render will fetch them.")
  } else {
    tbl_note(
      "{n_} filing{?s} outstanding and acquisition is OFF, so none were fetched. Set \\
       .lP$Param$Acquire to TRUE to close the gap.",
      .type = "warn"
    )
  }

  invisible(cov_)
}


# 4. Landing-page parsing ----------------------------------------------------------------------------------------------

#' Parse mirrored landing-page HTML into per-quarter tables
#'
#' Idempotent by inspection rather than by switch. Each year-quarter has one output file; the
#' function reads whatever is already in it, parses only the HashIndex values not present, and
#' appends. A completed quarter costs one parquet read. This is what makes it safe for the document
#' to run the step unconditionally on every render, which in turn is what makes the render honest.
#'
#' WORK IS SENT IN CHUNKS, NOT PER DOCUMENT. The obvious parallel map sends one landing page to a
#' worker per call, and each landing page is a full HTML document; the serialisation cost then
#' dominates the parse. Splitting a quarter into one chunk per worker turns tens of thousands of
#' transfers into one per worker, and moves the same bytes once.
#'
#' Parse failures are kept, not dropped. rGetEDGAR marks them with a non-zero Error column, and
#' edg_clean_landing() filters on it. Discarding them here would make the failure rate unobservable,
#' since a page that was never written is indistinguishable from one that was never fetched.
#'
#' @param .path_htmls Directory of mirrored landing-page HTML, one dataset per year-quarter.
#' @param .dir_raw Destination directory for the per-quarter parsed tables.
#' @param .workers Integer. Parallel workers; also the number of chunks each quarter is split into.
#' @param .quiet Logical. Suppress per-quarter progress.
#' @return A tibble with one row per year-quarter: YQ, nExisting, nParsed, nTotal.
edg_parse_landing <- function(.path_htmls, .dir_raw, .workers = 10L, .quiet = FALSE) {
  if (FALSE) {
    .path_htmls <- .lP$Edgar$DocLinks$DirMain$HTMLs
    .dir_raw    <- .lP$Cache$RawExtract
    .workers    <- 10L
    .quiet      <- FALSE
  }

  fs::dir_create(.dir_raw)

  src_ <- utils_list_files(.dirs = .path_htmls, .reg = NULL, .id = "YQ", .rec = FALSE) |>
    dplyr::mutate(YQ = gsub("DocHTMLs_", "", .data$YQ)) |>
    dplyr::arrange(.data$YQ)

  mirai::daemons(.workers)
  on.exit(mirai::daemons(0L), add = TRUE)

  out_ <- purrr::map(
    .x = seq_len(nrow(src_)),
    .f = function(.i) {
      yq_   <- src_$YQ[.i]
      out_f <- fs::path(.dir_raw, paste0("Landing_", yq_, ".parquet"))

      done_ <- if (fs::file_exists(out_f)) arrow::read_parquet(out_f) else tibble::tibble()
      seen_ <- if (nrow(done_) > 0L) done_[["HashIndex"]] else character(0)

      todo_ <- arrow::open_dataset(sources = unname(src_$Path[.i])) |>
        dplyr::filter(!.data$HashIndex %in% seen_) |>
        dplyr::collect()

      if (!.quiet) cli::cli_alert_info("{yq_}: {nrow(done_)} done, {nrow(todo_)} to parse")

      if (nrow(todo_) > 0L) {
        chunks_ <- split(todo_, cut(seq_len(nrow(todo_)), min(.workers, nrow(todo_)), labels = FALSE))
        new_    <- mirai::mirai_map(
          .x = chunks_,
          .f = function(.chunk) {
            dplyr::bind_rows(purrr::map2(.chunk$HashIndex, .chunk$HTML, rGetEDGAR::edgar_parse_landingpage))
          }
        )[] |>
          dplyr::bind_rows()

        arrow::write_parquet(dplyr::bind_rows(done_, new_), out_f)
        rm(new_)
      }

      res_ <- tibble::tibble(
        YQ        = yq_,
        nExisting = nrow(done_),
        nParsed   = nrow(todo_),
        nTotal    = nrow(done_) + nrow(todo_)
      )
      rm(done_, todo_)
      invisible(gc(verbose = FALSE))
      res_
    }
  ) |>
    dplyr::bind_rows()

  out_
}


# 5. Cleaning ----------------------------------------------------------------------------------------------------------

#' Standardise the parsed landing pages into one table
#'
#' Everything arrives from the HTML as character, because that is what the page contains. This
#' function does the type conversion once, centrally, so that no downstream script has to decide
#' whether FilingDate is a date or a string.
#'
#' EMPTY STRINGS BECOME NA. An unpopulated field on a landing page renders as an empty cell, which
#' parses to "" rather than to a missing value. Left alone it counts as a present observation in
#' every summary and joins as a real key, so the conversion happens here and not in the four places
#' that consume the result.
#'
#' Dates are parsed with base as.Date rather than a date library. The values are ISO-8601 as EDGAR
#' writes them, and AcceptedDate carries a time component that as.Date truncates correctly. Anything
#' that does not conform becomes NA and is reported by edg_landing_coverage() rather than silently
#' recoded.
#'
#' The rows kept are those with Error == 0. Parse failures remain in the per-quarter cache, so the
#' failure rate stays computable from the two together.
#'
#' @param .dir_raw Directory of per-quarter parsed tables written by edg_parse_landing().
#' @param .path_out Destination parquet path.
#' @return The cleaned tibble, invisibly; written to .path_out as a side effect.
edg_clean_landing <- function(.dir_raw, .path_out) {
  if (FALSE) {
    .dir_raw  <- .lP$Cache$RawExtract
    .path_out <- .lP$Output$LandingPageAll
  }

  out_ <- arrow::open_dataset(sources = .dir_raw, unify_schemas = TRUE) |>
    dplyr::filter(.data$Error == 0) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::select(
      "HashIndex",
      CIK = "Cik", FYE = "FiscalYearEnd", FormType = "Type",
      "Act", "FileNo", "FilmNo", AccessionNo = "AccessionNumber", "IrsNo",
      AcceptedDate = "Accepted", ReportDate = "PeriodOfReport", "FilingDate",
      StateCode = "StateOfIncorp", SIC = "Sic", SRO = "SrOs", "CurrentReport",
      "MailingAddress", "BusinessAddress", "CompanyName",
      nDocuments = "Documents", "Items"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), trimws),
      dplyr::across(dplyr::where(is.character), \(.x) dplyr::if_else(.x == "", NA_character_, .x)),
      dplyr::across(c("FilingDate", "AcceptedDate", "ReportDate"), as.Date),
      dplyr::across(c("nDocuments", "FYE", "Act"), \(.x) suppressWarnings(as.integer(.x))),
      SRO = dplyr::if_else(.data$SRO == "NONE", NA_character_, .data$SRO)
    )

  arrow::write_parquet(out_, .path_out)
  invisible(out_)
}


# 6. Report: what the landing table contains ---------------------------------------------------------------------------

#' Landing-page coverage by year
#'
#' Two things are worth knowing about this table before anything downstream joins on it: how the
#' filings distribute across the period, and how often the three dates are missing. A year with a
#' collapse in either is an acquisition gap, not a change in filing behaviour.
#'
#' @param .tab The cleaned landing table.
#' @return A tibble with one row per calendar year.
edg_landing_coverage <- function(.tab) {
  if (FALSE) .tab <- arrow::read_parquet(.lP$Output$LandingPageAll)

  .tab |>
    dplyr::mutate(Year = as.integer(format(.data$FilingDate, "%Y"))) |>
    dplyr::summarise(
      nFilings  = dplyr::n(),
      nFilers   = dplyr::n_distinct(.data$CIK),
      ShareNoRD = mean(is.na(.data$ReportDate)),
      ShareNoFD = mean(is.na(.data$FilingDate)),
      .by       = "Year"
    ) |>
    dplyr::arrange(.data$Year)
}

#' Form-type composition of the landing table
#'
#' @param .tab The cleaned landing table.
#' @return A tibble with one row per form type, most frequent first.
edg_landing_forms <- function(.tab) {
  if (FALSE) .tab <- arrow::read_parquet(.lP$Output$LandingPageAll)

  .tab |>
    dplyr::summarise(nFilings = dplyr::n(), .by = "FormType") |>
    dplyr::mutate(Share = .data$nFilings / sum(.data$nFilings)) |>
    dplyr::arrange(dplyr::desc(.data$nFilings))
}

#' Report the landing table
#'
#' @param .tab The cleaned landing table.
#' @return The coverage tibble, invisibly.
edg_report_landing <- function(.tab) {
  if (FALSE) .tab <- arrow::read_parquet(.lP$Output$LandingPageAll)

  cov_ <- edg_landing_coverage(.tab = .tab)

  tbl_head("Landing pages by year")
  tbl_out(
    .tab    = cov_,
    .title  = NULL,
    .pct    = c("ShareNoRD", "ShareNoFD"),
    .digits = 1L,
    .notes  = c(
      ShareNoRD = "Report date is absent by design on forms that cover no period; 8-K is the bulk of it.",
      ShareNoFD = "Filing date should never be missing. A nonzero share here is a parse failure."
    )
  )

  tbl_head("Form-type composition")
  tbl_out(
    .tab    = edg_landing_forms(.tab = .tab),
    .title  = NULL,
    .pct    = "Share",
    .digits = 1L
  )

  invisible(cov_)
}

#' Every report in this document, in order
#'
#' The block to copy out when the mirror needs checking without re-rendering.
#'
#' @param .lp The configuration list.
#' @param .tab The cleaned landing table.
#' @return Invisibly NULL.
edg_report_all <- function(.lp, .tab) {
  if (FALSE) {
    .lp  <- .lP
    .tab <- arrow::read_parquet(.lP$Output$LandingPageAll)
  }

  edg_report_links(
    .dir_master = .lp$Edgar$MasterIndex$DirParquet,
    .forms      = .lp$Param$Forms,
    .max_year   = .lp$Param$MaxYear,
    .path_htmls = .lp$Edgar$DocLinks$DirMain$HTMLs,
    .path_links = .lp$Edgar$DocLinks$DirMain$Links,
    .acquire    = .lp$Param$Acquire
  )
  edg_report_landing(.tab = .tab)

  invisible(NULL)
}


# 7. Figures -----------------------------------------------------------------------------------------------------------

#' Filings mirrored per year
#'
#' @param .tab Output of edg_landing_coverage().
#' @return A ggplot object.
edg_plot_coverage <- function(.tab) {
  if (FALSE) .tab <- edg_landing_coverage(arrow::read_parquet(.lP$Output$LandingPageAll))

  ggplot2::ggplot(.tab, ggplot2::aes(x = .data$Year, y = .data$nFilings)) +
    ggplot2::geom_col(fill = plot_pal_seq(.n = 1L)) +
    plot_scale_y_count() +
    ggplot2::labs(x = NULL, y = "Filings") +
    plot_theme(.grid = "y")
}
