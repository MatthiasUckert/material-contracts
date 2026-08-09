# _Tables.R: the shared table layer ------------------------------------------------------------------------------------
#
# Every table in the monorepo prints through this file. Two audiences, two paths:
#
#   Console  -- tbl_fmt() / tbl_say(). What a runbook reports as it runs, and what gets pasted into
#               discussion. Fixed-width, ASCII, aligned, and it must survive both a terminal and a
#               chat window.
#   Rendered -- tbl_render(). What a reader meets in the knitted document, styled to match
#               _styles.css in html and booktabs in pdf, so the two formats read the same.
#
# Sourced by every runbook immediately after _Plots.R.
#
# The console path is the high-traffic one and was previously carried by a classification script,
# which meant the entity-extraction documents sourced a classification library for the sole purpose
# of printing a table. Moving it here is what lets those documents stop doing that.
#
# Style: native pipe, explicit namespace, dot-prefixed arguments, underscore-suffixed locals, ASCII.


# 1. Scalar formatters ---------------------------------------------------------------------------------------------
# Shared by both paths, so a proportion looks the same in the console block and in the rendered table.

#' Format a proportion as a percentage string
#'
#' @param .x Numeric vector, conventionally in [0, 1].
#' @param .digits Integer. Decimal places.
#' @return Character vector.
tbl_pct <- function(.x, .digits = 1L) {
  if (FALSE) {
    .x      <- c(0.9163, 0.0837)
    .digits <- 1L
  }
  paste0(formatC(100 * .x, format = "f", digits = .digits), "%")
}

#' Format a number with fixed decimals and thousands separators
#'
#' @param .x Numeric vector.
#' @param .digits Integer. Decimal places.
#' @return Character vector.
tbl_num <- function(.x, .digits = 2L) {
  if (FALSE) {
    .x      <- c(1234.5, 7.25)
    .digits <- 2L
  }
  formatC(.x, format = "f", digits = .digits, big.mark = ",")
}


# 2. Console path --------------------------------------------------------------------------------------------------
# Default tibble printing truncates exactly the columns that carry the result, and does so silently.
# Everything reported to the console therefore goes through this formatter instead.

#' Render a tibble as aligned fixed-width character lines
#'
#' Numeric columns are right-aligned and comma-grouped; character columns are left-aligned and NA
#' renders as "-". Anything needing custom formatting -- percentages, ratios -- is pre-formatted to
#' character by the caller, because a formatter that guessed would eventually guess wrong on the one
#' column a reader was checking.
#'
#' @param .tab Tibble to render.
#' @param .indent Integer. Leading spaces.
#' @return Character vector, one element per line, header first.
tbl_fmt <- function(.tab, .indent = 2L) {
  if (FALSE) {
    .tab    <- tibble::tibble(Class = c("Leases", "R&D"), N = c(303L, 83L))
    .indent <- 2L
  }
  pad_ <- strrep(" ", .indent)
  if (nrow(.tab) == 0L) return(paste0(pad_, "(none)"))

  is_num_ <- purrr::map_lgl(.tab, is.numeric)
  cells_  <- purrr::map2(.tab, is_num_, \(.col, .num) {
    if (!.num) return(tidyr::replace_na(as.character(.col), "-"))
    # A count that arrives as a double -- and most do, since n() / total, sum() and nrow() all
    # produce one -- would otherwise print as 874.000, which reads as a measurement carrying three
    # significant decimals rather than as a tally. Whole-valued columns holding anything above one
    # are therefore formatted as counts. The upper-bound test is what protects proportions: a
    # coverage column that happens to be exactly 1 everywhere stays a decimal, because collapsing it
    # would hide that it is a share.
    whole_ <- all(.col == round(.col), na.rm = TRUE) && any(abs(.col) > 1, na.rm = TRUE)
    if (is.integer(.col) || whole_) {
      format(.col, big.mark = ",", trim = TRUE, scientific = FALSE)
    } else {
      formatC(.col, format = "f", digits = 3)
    }
  })

  head_ <- names(.tab)
  wid_  <- purrr::map2_int(cells_, head_, \(.c, .h) max(nchar(.c), nchar(.h)))
  side_ <- dplyr::if_else(is_num_, "left", "right")

  row_ <- function(.vals) {
    paste0(pad_, paste(
      purrr::pmap_chr(list(.vals, wid_, side_), \(.v, .w, .s) stringr::str_pad(.v, .w, side = .s)),
      collapse = "  "
    ))
  }

  body_ <- purrr::map_chr(seq_len(nrow(.tab)), \(.i) row_(purrr::map_chr(cells_, \(.c) .c[[.i]])))
  c(row_(head_), body_)
}

#' Print a tibble to the console as an aligned block
#'
#' Returns its input invisibly, so a report call can sit mid-pipe and the numbers remain available
#' for further work after they have been displayed.
#'
#' @param .tab Tibble to print.
#' @param .title Character or NULL. Heading printed above the block.
#' @param .n Integer or NULL. Print only the first .n rows, noting how many were withheld.
#' @return Invisibly, .tab unchanged and unaffected by .n.
tbl_say <- function(.tab, .title = NULL, .n = NULL) {
  if (FALSE) {
    .tab   <- tibble::tibble(Class = c("Leases", "R&D"), N = c(303L, 83L))
    .title <- "Class distribution"
    .n     <- NULL
  }
  if (!is.null(.title)) cli::cli_h3(.title)
  show_ <- if (is.null(.n)) .tab else utils::head(.tab, .n)
  cli::cli_verbatim(tbl_fmt(show_))
  if (!is.null(.n) && nrow(.tab) > .n) {
    cli::cli_alert_info("{nrow(.tab) - .n} further row{?s} not shown.")
  }
  invisible(.tab)
}


# 3. Rendered path -------------------------------------------------------------------------------------------------
# One wrapper for every kable call, so a runbook carries no per-table formatting. The html look comes
# from _styles.css (horizontal rules only, Times, italic caption above); the pdf look comes from
# booktabs here. Both are set in one place so the formats do not drift apart.

#' Publication-ready table for html and pdf
#'
#' Percent columns are named rather than pre-formatted, so the caller keeps a numeric tibble and the
#' presentation decision stays here.
#'
#' @param .tab Tibble to render.
#' @param .caption Character or NULL. Table caption.
#' @param .digits Integer, scalar or per-column. Decimals for numeric columns.
#' @param .pct Character vector or NULL. Columns to render as percentages.
#' @param .acc Numeric. Rounding accuracy for the .pct columns.
#' @param .n Integer or NULL. Render only the first .n rows.
#' @param .full_width Logical. Stretch to the page width.
#' @param .wide Logical. Scale a many-column table down to fit the pdf page. No effect in html.
#' @return A kableExtra-styled kable, ready to print.
tbl_render <- function(.tab, .caption = NULL, .digits = 3L, .pct = NULL, .acc = 0.1,
                       .n = NULL, .full_width = FALSE, .wide = FALSE) {
  if (FALSE) {
    .tab        <- tibble::tibble(Class = c("Leases", "R&D"), N = c(303L, 83L), Pct = c(0.78, 0.22))
    .caption    <- "Broad taxonomy"
    .digits     <- 3L
    .pct        <- "Pct"
    .acc        <- 0.1
    .n          <- NULL
    .full_width <- FALSE
    .wide       <- FALSE
  }
  tab_ <- .tab
  if (!is.null(.n)) tab_ <- utils::head(tab_, .n)
  if (!is.null(.pct)) {
    tab_ <- tab_ |>
      dplyr::mutate(dplyr::across(dplyr::any_of(.pct), ~ scales::percent(.x, accuracy = .acc)))
  }

  latex_ <- if (.wide) c("hold_position", "scale_down") else "hold_position"

  tab_ |>
    knitr::kable(
      caption     = .caption,
      digits      = .digits,
      booktabs    = TRUE,
      format.args = list(big.mark = ",")
    ) |>
    kableExtra::kable_styling(
      full_width    = .full_width,
      position      = "left",
      latex_options = latex_
    )
}
