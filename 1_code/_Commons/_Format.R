# _Format.R: house formatting helpers for publication-ready tables ----
# Sourced by the report qmds (after _Utils.R). Centralises the look of every
# table so the qmds carry no per-call formatting boilerplate: one fmt_table()
# wraps the caption, rounding, thousands separators, percent columns, and
# booktabs styling, and it works in BOTH html and pdf. The visual rules for html
# tables live in _styles.css (booktabs borders); fmt_table owns the DATA
# presentation plus the pdf/LaTeX booktabs, so the two formats read the same.
#
# Usage (drop-in for knitr::kable):
#   clf_class_distribution(tab_prep, "ClassBroad") |>
#     fmt_table(.caption = "Broad taxonomy", .pct = "Pct")
#
# House style: native pipe; explicit package::function; dot-prefixed args;
# underscore-suffixed locals; pure ASCII. Depends on kableExtra (already used by
# the plot_htmltable helper).

#' Format a numeric vector as a percent string
#' @param .x Numeric proportions (e.g. 0.92).
#' @param .acc Rounding accuracy (default 0.1 -> "92.0%").
#' @return Character vector.
fmt_pct <- function(.x, .acc = 0.1) {
  scales::percent(.x, accuracy = .acc)
}

#' Format a numeric vector with fixed decimals and thousands separators
#' @param .x Numeric.
#' @param .digits Decimal places (default 2).
#' @return Character vector.
fmt_num <- function(.x, .digits = 2L) {
  formatC(.x, format = "f", digits = .digits, big.mark = ",")
}

#' Publication-ready table: one wrapper for every kable call
#'
#' Centralises the house table look -- optional caption, uniform rounding,
#' thousands separators, percent-formatting of named columns, booktabs rules, and
#' a content-width (not full-page) block. Renders in html and pdf: the html visual
#' rules come from _styles.css, the pdf booktabs from kable. Pass columns to show
#' as percent via .pct, so the qmd no longer needs a mutate(scales::percent(...))
#' ahead of the table.
#'
#' @param .tab A data frame / tibble.
#' @param .caption Character or NULL. Table caption.
#' @param .digits Integer (scalar or per-column vector). Decimals for numeric cols.
#' @param .pct Character vector of columns to render as percent, or NULL.
#' @param .acc Percent rounding accuracy for .pct columns (default 0.1).
#' @param .n Integer or NULL. Show only the first .n rows.
#' @param .full_width Logical. Stretch to the page width (default FALSE).
#' @param .wide Logical. For many-column tables, scale the LaTeX/PDF output down
#'   to fit the page width (default FALSE). HTML is unaffected.
#' @return A kableExtra-styled kable, ready to print in html or pdf.
fmt_table <- function(.tab, .caption = NULL, .digits = 3L, .pct = NULL,
                      .acc = 0.1, .n = NULL, .full_width = FALSE, .wide = FALSE) {
  if (FALSE) {
    .tab     <- clf_class_distribution(tab_prep, "ClassBroad")
    .caption <- "Broad taxonomy"
    .pct     <- "Pct"
  }
  tab_ <- .tab
  if (!is.null(.n)) tab_ <- utils::head(tab_, .n)
  if (!is.null(.pct)) {
    tab_ <- tab_ |>
      dplyr::mutate(dplyr::across(dplyr::any_of(.pct), ~ fmt_pct(.x, .acc)))
  }
  latex_opts_ <- if (.wide) c("hold_position", "scale_down") else c("hold_position")
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
      latex_options = latex_opts_
    )
}
