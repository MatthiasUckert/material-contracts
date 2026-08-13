# Addition for _Commons/_Tables.R ---------------------------------------------------------------------------------------
# One wrapper covering the look chosen for the classification runbooks: grouped column headers,
# percentages formatted as percentages, a summary row set apart, and footnotes that span the table.
#
# WHY THE COLSPAN IS PATCHED
# kableExtra writes its footnote row as <td colspan="100%">. The html specification requires colspan
# to be a non-negative integer, so every current browser fails to parse it and falls back to a span of
# one -- which puts the footnote inside the first column and wraps it to that column's width. The
# markup is wrong rather than the styling, so the repair is a substitution on the generated string and
# not a css rule. It is confined to this function; nothing else in the project needs to know.

#' Publication-ready table with grouped headers and spanning footnotes
#'
#' The presentation decisions live here rather than in the runbooks: a caller passes a numeric tibble
#' and names which columns are shares, and the formatting, alignment and footnote markers are applied
#' in one place. A runbook that formatted its own percentages would be a second convention.
#'
#' @param .tab Tibble to render.
#' @param .caption Character or NULL. Table caption.
#' @param .groups Named integer vector or NULL. Spanning header, as add_header_above() takes it, e.g.
#'   c(" " = 1, "Size" = 2, "Quality" = 3). The counts must sum to the column count.
#' @param .pct Character vector or NULL. Columns rendered as percentages.
#' @param .digits Integer. Decimals for the remaining numeric columns.
#' @param .acc Numeric. Rounding accuracy for the percentage columns.
#' @param .notes Named character vector or NULL. Footnotes, named by the column each annotates. The
#'   name is matched against the column names and the marker is attached to that header; a note whose
#'   name matches nothing is still printed, unattached, rather than silently dropped.
#' @param .summary_row Integer or NULL. Row set in bold with a rule above it, conventionally the last.
#' @param .full_width Logical. Stretch to the page width.
#' @return A kableExtra-styled kable, ready to print.
tbl_grouped <- function(.tab, .caption = NULL, .groups = NULL, .pct = NULL, .digits = 3L,
                        .acc = 0.1, .notes = NULL, .summary_row = NULL, .full_width = FALSE) {
  if (FALSE) {
    .tab         <- perclass_det_bert
    .caption     <- "Transformer by category"
    .groups      <- c(" " = 1, "Size" = 2, "Behaviour" = 2, "Quality" = 3)
    .pct         <- c("Coverage", "Accuracy", "Precision", "Recall", "RecallSel")
    .digits      <- 3L
    .acc         <- 0.1
    .notes       <- c(Accuracy = "One-vs-rest and dominated by correct rejections.",
                      Recall   = "Counts an abstention as a miss.")
    .summary_row <- nrow(perclass_det_bert)
    .full_width  <- FALSE
  }
  tab_ <- .tab
  if (!is.null(.pct)) {
    tab_ <- tab_ |>
      dplyr::mutate(dplyr::across(dplyr::any_of(.pct),
                                  \(.x) dplyr::if_else(is.finite(.x),
                                                       scales::percent(.x, accuracy = .acc), "-")))
  }

  # Markers go on the header BEFORE kable sees the frame, so escaping has to be off from here on.
  # Notes are named by column rather than positioned, because a positional list silently annotates the
  # wrong header the first time a column is added.
  head_ <- names(tab_)
  if (!is.null(.notes)) {
    hit_ <- match(names(.notes), head_)
    for (.i in seq_along(.notes)) {
      if (!is.na(hit_[[.i]])) {
        head_[hit_[[.i]]] <- paste0(head_[hit_[[.i]]],
                                    kableExtra::footnote_marker_number(.i, format = "html"))
      }
    }
    miss_ <- names(.notes)[is.na(hit_)]
    if (length(miss_) > 0L) {
      cli::cli_alert_warning(
        "{length(miss_)} footnote{?s} name{?s/} a column that is not in this table ({miss_}); \\
         printed without a marker."
      )
    }
  }
  names(tab_) <- head_

  # Numbers right, text left. Left as a default, kable centres numeric columns of mixed width and the
  # decimal points stop lining up, which is most of what makes a metrics table hard to scan.
  align_ <- dplyr::if_else(purrr::map_lgl(.tab, is.numeric), "r", "l")

  out_ <- tab_ |>
    knitr::kable(
      format      = "html",   # explicit: without it pandoc runs its smart-quote pass over the markers
      escape      = FALSE,    # the header carries html markers
      caption     = .caption,
      digits      = .digits,
      align       = paste(align_, collapse = ""),
      format.args = list(big.mark = ",")
    ) |>
    kableExtra::kable_styling(
      full_width        = .full_width,
      position          = "left",
      bootstrap_options = c("hover", "condensed")
    )

  if (!is.null(.groups)) out_ <- kableExtra::add_header_above(out_, .groups)
  if (!is.null(.summary_row)) {
    out_ <- out_ |>
      kableExtra::row_spec(.summary_row - 1L, extra_css = "border-bottom: 1px solid #333;") |>
      kableExtra::row_spec(.summary_row, bold = TRUE)
  }
  if (!is.null(.notes)) {
    out_ <- kableExtra::footnote(out_, number = unname(.notes), threeparttable = TRUE)
  }
  tbl_fix_colspan(.kable = out_, .ncol = ncol(.tab))
}

#' Repair the footnote row's column span
#'
#' kableExtra emits `colspan="100%"`, which the html specification does not permit -- the attribute
#' takes a non-negative integer. Browsers therefore fail to parse it and fall back to a span of one,
#' so the footnote renders inside the first column and wraps to that column's width. Substituting the
#' real column count is the whole repair.
#'
#' Written as its own function because it applies to any kable carrying a footnote, not only the ones
#' this file builds, and because a reader finding the substitution inline would reasonably wonder what
#' it was working around.
#'
#' @param .kable A kable with a footnote already attached.
#' @param .ncol Integer. Columns the footnote should span.
#' @return The same kable, with the span corrected.
tbl_fix_colspan <- function(.kable, .ncol) {
  if (FALSE) {
    .kable <- knitr::kable(head(iris), format = "html") |> kableExtra::footnote(number = "a note")
    .ncol  <- 5L
  }
  txt_ <- as.character(.kable)
  txt_ <- gsub('colspan="100%"', paste0('colspan="', .ncol, '"'), txt_, fixed = TRUE)
  txt_ <- gsub("colspan='100%'", paste0("colspan='", .ncol, "'"), txt_, fixed = TRUE)
  structure(txt_, format = "html", class = "knitr_kable")
}
