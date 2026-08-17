# 01A-EdgarIndex: mirror EDGAR's index and landing pages, and turn the landing HTML into a table -----------------------
#
# WHAT THIS FILE DOES
# EDGAR publishes a quarterly master index of every filing since 1993. For each filing there is a
# landing page listing the documents attached to it. 01A mirrors both and parses the landing HTML
# into one table, LandingPageAll.parquet, holding twenty-one metadata fields per filing.
#
# It does not download any attachment. That is 01B, which uses the links mirrored here to decide
# what to fetch. The two are separate scripts because they fail differently and at different scales:
# the index is a few thousand files, the attachments are millions.
#
# THE MIRROR
# rGetEDGAR derives its whole directory tree from one root, and this script's root is GetEDGAR/
# inside its own output directory. 01B writes the DocumentData/ branch of that same tree, because
# the package resolves link tables and downloaded documents from a single root and separating them
# would mean duplicating the link tables. Ownership stays unambiguous at the level that matters:
# 01A writes MasterIndex/ and DocLinks/, 01B writes DocumentData/, and no artifact has two writers.
#
# THE ACQUISITION SWITCH
# Two steps reach the SEC over the network. Both sit behind .lP$Param$Acquire, which defaults to
# FALSE. The CHECKS never sit behind it: the document always computes what is outstanding and always
# reports it, so an incomplete mirror is a visible result rather than a silent assumption. Gating a
# step and skipping a step are different things, and only the first is honest.
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
#' the landing page of every filing would multiply the acquisition cost by roughly a factor of ten
#' for filings that cannot contain the object of study.
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
#' surrounding environment.
#'
#' The year ceiling is a closure rule, not a data limit. EDGAR keeps publishing, so an open-ended
#' frame would grow between renders and no two runs of the pipeline would describe the same corpus.
#'
#' @param .dir_master Directory of master-index parquet files.
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

#' How much of the selected frame has been mirrored
#'
#' The one number that decides whether the acquisition switch needs turning on. It is computed on
#' every render, whether or not anything was acquired, because a document that cannot say how
#' complete its own inputs are is not reporting a result.
#'
#' Landing pages and link tables are counted separately. They are produced by the same request but
#' stored apart, and a gap between them is a different failure from a gap in either: it means pages
#' were fetched whose document lists could not be parsed.
#'
#' THE FRAME IS PASSED IN, NOT RECOMPUTED. Selecting it scans the whole master index, and the
#' document needs the same answer three times: to drive acquisition, to report coverage, and again in
#' the overview. Computing it once and passing it is the difference between one scan and three.
#'
#' @param .frame Character vector of HashIndex values in the selected frame.
#' @param .path_htmls Directory of mirrored landing-page HTML.
#' @param .path_links Directory of mirrored document-link tables.
#' @return A one-row tibble: Selected, Mirrored, Missing, ShareMirrored, nLinkRows.
edg_link_coverage <- function(.frame, .path_htmls, .path_links) {
  if (FALSE) {
    .frame      <- vec_hash_index
    .path_htmls <- .lP$Edgar$DocLinks$DirMain$HTMLs
    .path_links <- .lP$Edgar$DocLinks$DirMain$Links
  }

  sel_ <- .frame

  have_ <- arrow::open_dataset(sources = .path_htmls) |>
    dplyr::select("HashIndex") |>
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
#' Takes the coverage tibble rather than computing it, so the overview can restate the same numbers
#' without rescanning the mirror.
#'
#' @param .cov Output of edg_link_coverage().
#' @param .acquire Logical. The value of the acquisition switch, reported alongside the backlog so
#'   that an incomplete mirror and a disabled download are never read as the same thing.
#' @return The coverage tibble, invisibly.
edg_report_links <- function(.cov, .acquire) {
  if (FALSE) {
    .cov     <- tab_coverage
    .acquire <- FALSE
  }

  cov_ <- .cov

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
#' appends. A completed quarter costs one parquet read. That is what makes it safe for the document
#' to run this step unconditionally on every render, which in turn is what makes the render honest:
#' a step cheap enough not to need a switch cannot be left switched off.
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
#' THE SKIP DECISION COSTS TWO COLUMN READS. Both sides are compared on HashIndex alone: the parsed
#' table is read with col_select, and the source dataset contributes only its HashIndex column. The
#' obvious formulation -- read the parsed table whole, and let Arrow filter the source -- makes every
#' completed quarter pull its HTML through a filter to produce nothing, which is most of the runtime
#' of a render that has no work to do.
#'
#' @param .path_htmls Directory of mirrored landing-page HTML, one dataset per year-quarter.
#' @param .dir_raw Destination directory for the per-quarter parsed tables.
#' @param .workers Integer. Parallel workers; also the number of chunks each batch is split into.
#' @param .batch Integer. Landing pages read into memory at once. Bounds peak memory on a large
#'   quarter, where the HTML held plus a copy in every worker is what would exhaust it.
#' @param .quiet Logical. Suppress per-quarter progress.
#' @return A tibble with one row per year-quarter: YQ, nExisting, nParsed, nTotal.
edg_parse_landing <- function(.path_htmls, .dir_raw, .workers = 24L, .batch = 100000L, .quiet = FALSE) {
  if (FALSE) {
    .path_htmls <- .lP$Edgar$DocLinks$DirMain$HTMLs
    .dir_raw    <- .lP$Cache$RawExtract
    .workers    <- 24L
    .batch      <- 100000L
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
      src_f <- unname(src_$Path[.i])
      out_f <- fs::path(.dir_raw, paste0("Landing_", yq_, ".parquet"))

      # The skip decision is made on HashIndex alone, on both sides. Reading the parsed table in
      # full, or letting Arrow filter the source dataset, would touch the HTML column of a quarter
      # that has nothing to do -- and the HTML column is the entire weight of this dataset.
      seen_ <- if (fs::file_exists(out_f)) {
        arrow::read_parquet(file = out_f, col_select = "HashIndex")[["HashIndex"]]
      } else {
        character(0)
      }

      have_ <- arrow::open_dataset(sources = src_f) |>
        dplyr::select("HashIndex") |>
        dplyr::collect() |>
        dplyr::pull(.data$HashIndex)

      todo_ids_ <- setdiff(have_, seen_)
      n_todo_   <- length(todo_ids_)

      if (!.quiet) cli::cli_alert_info("{yq_}: {length(seen_)} done, {n_todo_} to parse")

      if (n_todo_ > 0L) {
        # Read in batches. A quarter can hold millions of landing pages, and holding all of their
        # HTML plus a copy in every worker is the one way this step runs out of memory rather than
        # time. Each batch is read, dispatched, reduced to parsed rows, and dropped.
        batches_ <- split(todo_ids_, ceiling(seq_along(todo_ids_) / .batch))

        new_ <- purrr::map(
          .x = batches_,
          .f = function(.ids) {
            htm_ <- if (length(seen_) == 0L && length(batches_) == 1L) {
              arrow::read_parquet(src_f)
            } else {
              arrow::open_dataset(sources = src_f) |>
                dplyr::filter(.data$HashIndex %in% .ids) |>
                dplyr::collect()
            }

            chunks_ <- split(htm_, cut(seq_len(nrow(htm_)), min(.workers, nrow(htm_)), labels = FALSE))
            rm(htm_)

            res_ <- mirai::mirai_map(
              .x = chunks_,
              .f = function(.chunk) {
                dplyr::bind_rows(purrr::map2(.chunk$HashIndex, .chunk$HTML, rGetEDGAR::edgar_parse_landingpage))
              }
            )[] |>
              dplyr::bind_rows()

            rm(chunks_)
            invisible(gc(verbose = FALSE))
            res_
          }
        ) |>
          dplyr::bind_rows()

        done_ <- if (fs::file_exists(out_f)) arrow::read_parquet(out_f) else tibble::tibble()
        arrow::write_parquet(dplyr::bind_rows(done_, new_), out_f)
        rm(new_, done_)
        invisible(gc(verbose = FALSE))
      }

      tibble::tibble(
        YQ        = yq_,
        nExisting = length(seen_),
        nParsed   = n_todo_,
        nTotal    = length(seen_) + n_todo_
      )
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
#' THE OUTPUT IS SORTED, AND THE SORT KEY IS CHOSEN FOR COMPRESSION. A multi-file Arrow scan does not
#' guarantee the order in which record batches are returned, so two runs over identical inputs can
#' write the same rows in a different order. Nothing downstream depends on order -- every consumer
#' joins on keys -- but an artifact that changes between runs cannot be checked against a previous
#' copy, and a published dataset is exactly where someone will try.
#'
#' FilingDate leads, and that matters more than it looks. Parquet encodes each column chunk with a
#' dictionary and run lengths, which pay off only when values are locally correlated. Sorting first
#' on the filing hash scatters every correlated column and inflates the file by a factor of three and
#' a half; sorting on the date preserves the temporal grouping the data already had, and the filing
#' and filer then break ties deterministically at no cost.
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
    ) |>
    dplyr::arrange(.data$FilingDate, .data$HashIndex, .data$CIK, .data$FilmNo)

  arrow::write_parquet(out_, .path_out)
  invisible(out_)
}


# 6. Report: what the landing table contains ---------------------------------------------------------------------------

#' Landing-page coverage by year
#'
#' ROWS ARE NOT FILINGS. A landing page lists every registrant on the filing, and the parse emits one
#' row per filing-filer pair. A registration statement carrying guarantor subsidiaries can therefore
#' contribute hundreds of rows, and counting rows would overstate the number of filings by a factor
#' that varies with form type. Both quantities are reported, because the ratio between them is itself
#' informative: it is the average number of registrants per filing that year.
#'
#' Missing-date shares are computed over rows rather than filings, which is the right denominator for
#' a parse-quality measure: a field is parsed once per row.
#'
#' @param .tab The cleaned landing table.
#' @return A tibble with one row per calendar year: nRows, nFilings, nFilers, and the two date shares.
edg_landing_coverage <- function(.tab) {
  if (FALSE) .tab <- arrow::read_parquet(.lP$Output$LandingPageAll)

  .tab |>
    dplyr::mutate(Year = as.integer(format(.data$FilingDate, "%Y"))) |>
    dplyr::summarise(
      nRows     = dplyr::n(),
      nFilings  = dplyr::n_distinct(.data$HashIndex),
      nFilers   = dplyr::n_distinct(.data$CIK),
      ShareNoRD = mean(is.na(.data$ReportDate)),
      ShareNoFD = mean(is.na(.data$FilingDate)),
      .by       = "Year"
    ) |>
    dplyr::mutate(FilersPerFiling = .data$nRows / .data$nFilings) |>
    dplyr::arrange(.data$Year)
}

#' The grain of the landing table, against the frame that was selected
#'
#' Three quantities that a reader needs before joining anything to this table, and that no single
#' number conveys.
#'
#' The mirror is broader than the current frame. Landing pages fetched under an earlier or wider
#' selection remain mirrored and are parsed along with everything else, so the table holds filings the
#' present form-type restriction would not select. They are harmless -- downstream selection happens
#' on the link tables, not here -- but the count belongs on the page rather than in a reader's
#' arithmetic.
#'
#' @param .tab The cleaned landing table.
#' @param .frame Character vector of HashIndex values in the selected frame.
#' @return A one-row tibble: nRows, nFilings, RowsPerFiling, InFrame, OutsideFrame.
edg_landing_grain <- function(.tab, .frame) {
  if (FALSE) {
    .tab   <- arrow::read_parquet(.lP$Output$LandingPageAll)
    .frame <- vec_hash_index
  }

  idx_ <- unique(.tab$HashIndex)

  tibble::tibble(
    nRows         = nrow(.tab),
    nFilings      = length(idx_),
    RowsPerFiling = nrow(.tab) / length(idx_),
    InFrame       = sum(idx_ %in% .frame),
    OutsideFrame  = sum(!idx_ %in% .frame)
  )
}

#' Form-type composition of the landing table
#'
#' Reported on both grains. Ranking by rows puts registration statements at the top purely because
#' they carry the most co-registrants, which is a fact about corporate structure rather than about
#' filing volume; ranking by distinct filings answers the question a reader is actually asking.
#'
#' @param .tab The cleaned landing table.
#' @return A tibble with one row per form type, most filings first.
edg_landing_forms <- function(.tab) {
  if (FALSE) .tab <- arrow::read_parquet(.lP$Output$LandingPageAll)

  .tab |>
    dplyr::summarise(nRows = dplyr::n(), nFilings = dplyr::n_distinct(.data$HashIndex), .by = "FormType") |>
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
      nRows           = "One row per filing-filer pair, not per filing.",
      FilersPerFiling = "Registrants per filing. Driven by co-registrants on registration statements.",
      ShareNoRD       = "Report date is absent by design on forms covering no period; 8-K is the bulk of it.",
      ShareNoFD       = "Filing date should never be missing. A nonzero share here is a parse failure."
    )
  )

  tbl_head("Form-type composition")
  tbl_out(
    .tab    = edg_landing_forms(.tab = .tab),
    .title  = NULL,
    .pct    = "Share",
    .digits = 1L,
    .notes  = c(
      nRows = "Rows, so registration statements are inflated by their co-registrants.",
      Share = "A handful of types outside the selected frame appear: the page's own FormType can \\
               disagree with the master index, which is authoritative."
    )
  )

  invisible(cov_)
}

#' Duplicate filers and duplicate rows
#'
#' A filing plus a registrant plus a film number ought to identify one row. It does not everywhere,
#' and the question that follows is whether the repeats are the same row written twice or two rows
#' that genuinely differ. The first is harmless and the second would mean a registrant is recorded
#' inconsistently on its own filing, which anything joining to this table would inherit.
#'
#' Reported rather than fixed. Removing the repeats would change the published row count, and this
#' document's job is to say what the mirror contains, not to decide what it should have contained.
#'
#' @param .tab The cleaned landing table.
#' @param .key Character vector naming the columns that ought to identify a row.
#' @return A one-row tibble: nRows, nKeyDupRows, nKeyDupGroups, nExactDupRows, AllRepeatsIdentical.
edg_landing_duplicates <- function(.tab, .key = c("HashIndex", "CIK", "FilmNo")) {
  if (FALSE) {
    .tab <- arrow::read_parquet(.lP$Output$LandingPageAll)
    .key <- c("HashIndex", "CIK", "FilmNo")
  }

  key_ <- .tab[.key]
  hit_ <- duplicated(key_) | duplicated(key_, fromLast = TRUE)
  sub_ <- .tab[hit_, ]

  n_groups_ <- nrow(dplyr::distinct(sub_[.key]))
  n_exact_  <- nrow(sub_) - nrow(dplyr::distinct(sub_))

  tibble::tibble(
    nRows               = nrow(.tab),
    nKeyDupRows         = nrow(sub_),
    nKeyDupGroups       = n_groups_,
    nExactDupRows       = n_exact_,
    AllRepeatsIdentical = n_exact_ == (nrow(sub_) - n_groups_)
  )
}

#' Every report in this document, in order
#'
#' The block to copy out when the mirror needs checking without re-rendering.
#'
#' @param .cov Output of edg_link_coverage().
#' @param .tab The cleaned landing table.
#' @param .acquire Logical. The value of the acquisition switch.
#' @return Invisibly NULL.
edg_report_all_index <- function(.cov, .tab, .acquire) {
  if (FALSE) {
    .cov     <- tab_coverage
    .tab     <- arrow::read_parquet(.lP$Output$LandingPageAll)
    .acquire <- FALSE
  }

  edg_report_links(.cov = .cov, .acquire = .acquire)
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
