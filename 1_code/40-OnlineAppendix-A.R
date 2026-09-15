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
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-A", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-A.R")
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
    .dir_own        <- here::here("2_output", "40-OnlineAppendix-A", "Output")
    .force          <- FALSE
    .path_lib       <- here::here("1_code", "40-OnlineAppendix-A.R")
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
    .dir_own     <- here::here("2_output", "40-OnlineAppendix-A", "Output")
    .force       <- FALSE
    .path_lib    <- here::here("1_code", "40-OnlineAppendix-A.R")
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
  tibble::tibble(
    Key = c("DocsAll", "DocsUnique", "DocsSample", "DocsPre2001", "DocsFlagged", "RuleShort", "RuleStopwords",
            "RuleNumeric", "Placeholders", "YearFirst", "YearLast"),
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
      max(uni_$Year[uni_$Sample], na.rm = TRUE)
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
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-A", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-A.R")
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
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-A", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-A.R")
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
    .dir_own      <- here::here("2_output", "40-OnlineAppendix-A", "Output")
    .force        <- FALSE
    .path_lib     <- here::here("1_code", "40-OnlineAppendix-A.R")
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
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-A", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-A.R")
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
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-A", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-A.R")
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
