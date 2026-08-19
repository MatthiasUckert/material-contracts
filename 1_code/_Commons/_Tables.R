# _Tables.R: the shared table layer ------------------------------------------------------------------------------------
#
# Every table in the monorepo prints through this file. Two audiences, two paths:
#
#   Console  -- tbl_fmt() / tbl_say(). What a runbook reports as it runs, and what gets pasted into
#               discussion. Fixed-width, ASCII, aligned, and it must survive both a terminal and a
#               chat window.
#   Rendered -- tbl_render() / tbl_grouped(). What a reader meets in the knitted document, styled to
#               match _styles.css in html and booktabs in pdf, so the two formats read the same.
#
# A document does not choose between them per table. tbl_out() takes one specification -- what the
# columns mean, which are shares, which carry a caveat -- and applies whichever path the document set
# through the `mc.table_mode` option. The same call therefore yields a pasteable console block while
# an analysis is being built and a rendered table in the version somebody reads, and the caveats are
# written once rather than once per path.
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
    if (!.num) {
      # A CELL HOLDING A NEWLINE DESTROYS THE COLUMN MODEL. Widths are computed with nchar() and the
      # row is written as one line, so an embedded line break pushes every later cell of that row
      # onto a line of its own with nothing above it naming what it is. It arrives whenever a table
      # carries a condition message, since conditionMessage() keeps the newlines R put there.
      # Whitespace is squished rather than the cell truncated: the content is usually a diagnostic
      # and the informative part is at the front.
      chr_ <- stringi::stri_replace_all_regex(as.character(.col), "\\s+", " ")
      return(tidyr::replace_na(stringi::stri_trim_both(chr_), "-"))
    }
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
#
# tbl_render() is the plain wrapper. tbl_grouped() adds what a metrics table needs -- a spanning
# header, a summary row set apart, and footnotes attached to the columns they annotate -- and
# tbl_out() dispatches between it and the console path. Nothing here is specific to a script family.
#
# RENDERED TABLES NEED `results: asis` ON THE CHUNK. A kable emitted from inside a report function is
# written to standard output, and without that option knitr wraps the markup in a code block and the
# reader sees raw html. tbl_out() checks and names the fix rather than letting it render wrong.

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


#' Which table path this document is using
#'
#' Read from an option rather than passed, so a document sets it once and every table below follows.
#' An unrecognised value falls back to the console path and says so, because the alternative is a
#' document that silently renders half its tables the other way.
#'
#' @param .mode Character or NULL. NULL reads the `mc.table_mode` option, defaulting to "console".
#' @return Either "console" or "kable".
tbl_mode <- function(.mode = NULL) {
  if (FALSE) .mode <- NULL
  mode_ <- if (is.null(.mode)) getOption("mc.table_mode", "console") else .mode
  if (!mode_ %in% c("console", "kable")) {
    cli::cli_alert_warning(
      "Unknown table mode {.val {mode_}}; falling back to {.val console}. Set \\
       {.code options(mc.table_mode = \"kable\")} for rendered tables."
    )
    mode_ <- "console"
  }
  mode_
}

#' Percentages that survive an empty group
#'
#' The scalar formatter pastes a percent sign onto whatever it is given, so a group with nothing to
#' average renders as "NaN%", which reads as a computation that went wrong rather than as a cell that
#' cannot exist. An abstaining classifier has no precision on the documents it abstained from, and
#' that is a property of the question rather than a fault.
#'
#' @param .x Numeric vector, possibly holding NA or NaN.
#' @param .digits Integer. Decimal places.
#' @return Character vector, with non-finite entries rendered as a dash.
tbl_pct_safe <- function(.x, .digits = 1L) {
  if (FALSE) {
    .x      <- c(0.9163, NaN, NA_real_)
    .digits <- 1L
  }
  dplyr::if_else(is.finite(.x), tbl_pct(.x, .digits = .digits), "-")
}

#' Fold a note written across several source lines back into one line
#'
#' NOTES ARE INTERPOLATED, NOT PARSED. tbl_out() emits a console note as `{(.col)}: {(.note)}`, so the
#' note arrives as a value and cli never sees its content as a template. An R line continuation --
#' a trailing backslash before the newline, which every note longer than the 125-column margin needs
#' -- is therefore not a directive to cli but two literal characters, and the rendered page shows a
#' stray backslash followed by the source file's indentation. The kable path shows the same thing in
#' its footnotes. Cleaning here, once, at the only point where both paths converge, is what stops a
#' formatting rule about source files from leaking into published output.
#'
#' Whitespace runs collapse to a single space, which is safe because a note is one sentence of prose
#' about a column. Anything needing a line break is not a note.
#'
#' @param .notes Named character vector or NULL, exactly as a caller wrote it.
#' @return The same vector with continuations folded and whitespace squished, names preserved; NULL
#'   passes through so callers need no guard.
tbl_notes_clean <- function(.notes) {
  if (FALSE) {
    .notes <- c(Share = "A handful of types outside the frame appear: the page's own FormType can \\
                         disagree with the master index.")
  }

  if (is.null(.notes)) return(NULL)

  out_ <- .notes |>
    stringi::stri_replace_all_regex(pattern = "\\\\\\s*\\n", replacement = " ") |>
    stringi::stri_replace_all_regex(pattern = "\\s+", replacement = " ") |>
    stringi::stri_trim_both()

  purrr::set_names(out_, names(.notes))
}

#' One table, rendered the way this document is set to render tables
#'
#' The single call site a report function needs. It takes the full specification -- what the columns
#' mean, which are shares, which carry a caveat -- and applies whichever path the document selected.
#'
#' The caveats travel as `.notes` rather than being printed separately afterwards, which is what makes
#' one specification serve both paths: rendered, they become numbered footnotes attached to the column
#' headers; on the console they become `cli` lines prefixed by the column they refer to. Written twice
#' they would eventually say different things.
#'
#' RENDERED TABLES NEED `results: asis`. A kable emitted from inside a function is written to standard
#' output, and without that chunk option knitr wraps it in a code block and the reader sees raw html.
#' The check below turns a silently mangled document into a named one.
#'
#' @param .tab Tibble to render.
#' @param .title Character or NULL. Heading on the console path, caption on the rendered one.
#' @param .groups Named integer vector or NULL. Spanning header, as add_header_above() takes it, e.g.
#'   c(" " = 1, "Size" = 2, "Quality" = 3). Counts must sum to the column count. Console path ignores
#'   it, having no way to draw one.
#' @param .pct Character vector or NULL. Columns rendered as percentages. The caller passes numbers
#'   and the presentation decision stays here.
#' @param .digits Integer. Decimals for the remaining numeric columns.
#' @param .acc Numeric. Rounding accuracy for the percentage columns.
#' @param .notes Named character vector or NULL. Caveats, named by the column each annotates. A note
#'   naming a column that is absent is still shown, unattached, rather than silently dropped. Notes
#'   pass through tbl_notes_clean() first, so one written across several source lines reads as one
#'   line on both paths.
#' @param .summary_row Integer or NULL. Row set in bold with a rule above it, conventionally the last.
#' @param .n Integer or NULL. Show only the first .n rows.
#' @param .full_width Logical. Stretch to the page width. Rendered path only.
#' @param .mode Character or NULL. Overrides the document setting for this one table.
#' @return Invisibly .tab, so a report function keeps its contract of returning its own numbers.
tbl_out <- function(.tab, .title = NULL, .groups = NULL, .pct = NULL, .digits = 3L, .acc = 0.1,
                    .notes = NULL, .summary_row = NULL, .n = NULL, .full_width = FALSE,
                    .mode = NULL) {
  if (FALSE) {
    .tab         <- tibble::tibble(Class = c("Leases", "R&D"), N = c(303L, 83L), Acc = c(0.98, 0.81))
    .title       <- "Transformer by category"
    .groups      <- c(" " = 1, "Size" = 1, "Quality" = 1)
    .pct         <- "Acc"
    .notes       <- c(Acc = "One-vs-rest and dominated by correct rejections.")
    .summary_row <- 2L
    .mode        <- NULL
  }
  tab_ <- if (is.null(.n)) .tab else utils::head(.tab, .n)

  # Cleaned once, before the paths diverge, so the console block and the rendered footnote cannot
  # disagree about what a note says.
  notes_ <- tbl_notes_clean(.notes = .notes)

  if (identical(tbl_mode(.mode = .mode), "console")) {
    shown_ <- tab_
    if (!is.null(.pct)) {
      shown_ <- shown_ |>
        dplyr::mutate(dplyr::across(dplyr::any_of(.pct), \(.x) tbl_pct_safe(.x, .digits = 1L)))
    }
    tbl_say(.tab = shown_, .title = .title)
    if (!is.null(notes_)) {
      cli::cli_text("")
      purrr::iwalk(notes_, function(.note, .col) {
        if (nzchar(.col)) cli::cli_alert_info("{(.col)}: {(.note)}") else cli::cli_alert_info(.note)
      })
    }
    return(invisible(.tab))
  }

  # A kable written from inside a function goes to standard output, so the chunk must be asis or
  # knitr wraps the markup in a code block. opts_current is empty outside knitr, where the question
  # does not arise.
  res_ <- knitr::opts_current$get("results")
  if (isTRUE(getOption("knitr.in.progress")) && !identical(res_, "asis")) {
    cli::cli_alert_warning(
      "This chunk renders a table but is not {.code results: asis}, so the markup will show as \\
       text. Add {.code #| results: asis} to the chunk."
    )
  }

  html_ <- as.character(tbl_grouped(
    .tab         = tab_,
    .caption     = .title,
    .groups      = .groups,
    .pct         = .pct,
    .digits      = .digits,
    .acc         = .acc,
    .notes       = notes_,
    .summary_row = .summary_row,
    .full_width  = .full_width
  ))

  # WRAPPED IN PANDOC'S RAW-BLOCK FENCE, not written bare. Bare markup emitted under `results: asis`
  # leaves pandoc to infer where the html stops, and it infers from blank lines -- so a table
  # containing one, or a heading following one, lands inside the block instead of outside it. The
  # symptom is not an error: the document renders, and a heading that should have opened a section
  # instead becomes a child of the previous one, so everything after it disappears into a subsection
  # or a tab and the document looks like it stopped early.
  #
  # The fence removes the inference. Pandoc reads the block to the closing backticks and resumes
  # parsing markdown after it, whatever the content held.
  cat("\n```{=html}\n", html_, "\n```\n\n", sep = "")
  invisible(.tab)
}

#' A heading above a table, in whichever form this document is using
#'
#' On the console path a table needs a heading, because a console block carries nothing else naming
#' it. On the rendered path the caption already does that job, so a second copy is noise -- and it
#' arrives as a monospaced block with box-drawing rules, which is exactly what a rendered document
#' should not contain.
#'
#' @param .text Character. cli-style text; interpolation happens in the calling frame.
#' @param .envir Environment interpolation is evaluated in.
#' @return Invisibly NULL.
tbl_head <- function(.text, .envir = parent.frame()) {
  if (FALSE) .text <- "Bert256 by category -- {(.label_col)}"
  if (identical(tbl_mode(), "console")) cli::cli_h2(.text, .envir = .envir)
  invisible(NULL)
}

#' A line of commentary about the table just printed
#'
#' The findings a report function states after its table -- which arm won on the documents that
#' matter, whether anything cleared the baseline -- are prose about the table, not console output. As
#' a `cli` alert they render into a monospaced block that does not wrap, so a sentence longer than the
#' page arrives with a horizontal scroll bar and has to be dragged through to be read. As a paragraph
#' it wraps like the rest of the document.
#'
#' On the console path the alert is what a reader wants, so it stays.
#'
#' Text is folded and interpolated exactly as `cli` would do it, so a caller writes one string and
#' both paths render the same sentence. The fold markers are stripped first: `cli::format_inline()`
#' interpolates but does not unfold, and an unstripped marker reaches the page as a stray backslash.
#'
#' @param .text Character. cli-style text; interpolation happens in the calling frame.
#' @param .type Character. "info" or "warn"; changes the console symbol and the rendered emphasis.
#' @param .envir Environment interpolation is evaluated in.
#' @return Invisibly NULL.
tbl_note <- function(.text, .type = c("info", "warn"), .envir = parent.frame()) {
  if (FALSE) {
    .text  <- "The two windows part company on {n_} document{?s}."
    .type  <- "info"
    .envir <- parent.frame()
  }
  .type <- match.arg(.type)
  if (identical(tbl_mode(), "console")) {
    if (identical(.type, "info")) {
      cli::cli_alert_info(.text, .envir = .envir)
    } else {
      cli::cli_alert_warning(.text, .envir = .envir)
    }
    return(invisible(NULL))
  }

  # Unfold, then squish: a folded cli string usually carries a trailing space before the marker and
  # leading indentation after it, so unfolding alone leaves a double space mid-sentence.
  txt_ <- gsub("\\\\\\s*\n\\s*", " ", .text)
  txt_ <- cli::format_inline(txt_, .envir = .envir)
  txt_ <- gsub("[ \t]+", " ", txt_)
  txt_ <- gsub("&", "&amp;", txt_, fixed = TRUE)
  txt_ <- gsub("<", "&lt;", txt_, fixed = TRUE)
  txt_ <- gsub(">", "&gt;", txt_, fixed = TRUE)
  cls_ <- if (identical(.type, "warn")) "table-note table-note-warn" else "table-note"
  cat("\n```{=html}\n<p class=\"", cls_, "\">", txt_, "</p>\n```\n\n", sep = "")
  invisible(NULL)
}

#' Publication-ready table with grouped headers and spanning footnotes
#'
#' The presentation decisions live here rather than in a runbook: a caller passes a numeric tibble and
#' names which columns are shares, and formatting, alignment and footnote markers are applied in one
#' place. A runbook formatting its own percentages would be a second convention.
#'
#' Called through tbl_out() in normal use. Exposed directly for the case where a document wants a
#' rendered table regardless of its own mode.
#'
#' @param .tab Tibble to render.
#' @param .caption Character or NULL. Table caption.
#' @param .groups Named integer vector or NULL. Spanning header, as add_header_above() takes it.
#' @param .pct Character vector or NULL. Columns rendered as percentages.
#' @param .digits Integer. Decimals for the remaining numeric columns.
#' @param .acc Numeric. Rounding accuracy for the percentage columns.
#' @param .notes Named character vector or NULL. Footnotes, named by the column each annotates. The
#'   name is matched against the column names and the marker attached to that header; a note whose
#'   name matches nothing is still printed, unattached, rather than silently dropped.
#' @param .summary_row Integer or NULL. Row set in bold with a rule above it.
#' @param .full_width Logical. Stretch to the page width.
#' @return A kableExtra-styled kable.
tbl_grouped <- function(.tab, .caption = NULL, .groups = NULL, .pct = NULL, .digits = 3L,
                        .acc = 0.1, .notes = NULL, .summary_row = NULL, .full_width = FALSE) {
  if (FALSE) {
    .tab         <- tibble::tibble(Class = c("Leases", "R&D"), N = c(303L, 83L), Acc = c(0.98, 0.81))
    .caption     <- "Transformer by category"
    .groups      <- c(" " = 1, "Size" = 1, "Quality" = 1)
    .pct         <- "Acc"
    .digits      <- 3L
    .acc         <- 0.1
    .notes       <- c(Acc = "One-vs-rest and dominated by correct rejections.")
    .summary_row <- 2L
    .full_width  <- FALSE
  }
  tab_ <- .tab
  if (!is.null(.pct)) {
    tab_ <- tab_ |>
      dplyr::mutate(dplyr::across(dplyr::any_of(.pct),
                                  \(.x) dplyr::if_else(is.finite(.x),
                                                       scales::percent(.x, accuracy = .acc), "-")))
  }

  # The marker format follows the output format. A marker written as html and rendered to pdf appears
  # in the table as a literal sup tag, which is the failure nobody notices until a referee does.
  # is_latex_output() is FALSE outside knitr, which is the right default for interactive use.
  fmt_ <- if (isTRUE(knitr::is_latex_output())) "latex" else "html"

  # Markers go on the header before kable sees the frame, so escaping is off from here on. Notes are
  # named by column rather than positioned, because a positional list silently annotates the wrong
  # header the first time a column is inserted.
  # Notes split two ways. One naming a column becomes a NUMBERED note with a marker on that header.
  # One left unnamed is about the table rather than about a column and becomes a GENERAL note, with no
  # number -- a numbered note with no marker anywhere is a reference to nothing, and a reader will
  # hunt the table for it.
  head_ <- names(tab_)
  num_  <- character(0)
  gen_  <- character(0)
  if (!is.null(.notes)) {
    nm_    <- names(.notes)
    if (is.null(nm_)) nm_ <- rep("", length(.notes))
    keyed_ <- nzchar(nm_)
    gen_   <- unname(.notes[!keyed_])
    num_   <- .notes[keyed_]

    hit_ <- match(names(num_), head_)
    # Numbered over the KEYED notes only, so the marker on a header and its position in the list are
    # the same number whatever order the caller wrote them in.
    for (.i in seq_along(num_)) {
      if (!is.na(hit_[[.i]])) {
        head_[hit_[[.i]]] <- paste0(head_[hit_[[.i]]],
                                    kableExtra::footnote_marker_number(.i, format = fmt_))
      }
    }
    miss_ <- names(num_)[is.na(hit_)]
    if (length(miss_) > 0L) {
      cli::cli_alert_warning(
        "{length(miss_)} footnote{?s} name{?s/} a column absent from this table ({miss_}); printed \\
         without a marker."
      )
    }
  }
  names(tab_) <- head_

  # Numbers right, text left. Left to itself kable centres numeric columns of mixed width and the
  # decimal points stop lining up, which is most of what makes a metrics table hard to scan.
  align_ <- dplyr::if_else(purrr::map_lgl(.tab, is.numeric), "r", "l")

  out_ <- tab_ |>
    knitr::kable(
      format      = fmt_,     # explicit: without it pandoc runs its smart-quote pass over the markers
      escape      = FALSE,    # the header carries footnote markers
      caption     = .caption,
      digits      = .digits,
      align       = paste(align_, collapse = ""),
      booktabs    = TRUE,     # pdf look; ignored in html
      format.args = list(big.mark = ",")
    ) |>
    kableExtra::kable_styling(
      full_width        = .full_width,
      position          = "left",
      bootstrap_options = c("hover", "condensed"), # html only; ignored under latex
      latex_options     = "hold_position"          # latex only; ignored in html
    )

  if (!is.null(.groups)) {
    # Checked here rather than left to kableExtra, which reports the mismatch from four frames down
    # and names neither the table nor the group that is short. A spanning header is written by hand
    # and goes stale the moment a column is added, so this fires often enough to be worth naming.
    if (sum(.groups) != ncol(.tab)) {
      cli::cli_abort(c(
        "The spanning header covers {sum(.groups)} column{?s} but the table has {ncol(.tab)}.",
        "i" = "Header: {paste0(names(.groups), ' = ', .groups, collapse = ', ')}",
        "i" = "Columns: {names(.tab)}"
      ))
    }
    out_ <- kableExtra::add_header_above(out_, .groups)
  }
  if (!is.null(.summary_row)) {
    # extra_css is html-only and hline_after is latex-only, so each format gets the rule it
    # understands and neither carries markup the other would print literally.
    out_ <- if (identical(fmt_, "latex")) {
      kableExtra::row_spec(out_, .summary_row - 1L, hline_after = TRUE)
    } else {
      kableExtra::row_spec(out_, .summary_row - 1L, extra_css = "border-bottom: 1px solid #333;")
    }
    out_ <- kableExtra::row_spec(out_, .summary_row, bold = TRUE)
  }
  if (length(num_) > 0L || length(gen_) > 0L) {
    out_ <- kableExtra::footnote(
      out_,
      general        = if (length(gen_) > 0L) gen_ else NULL,
      number         = if (length(num_) > 0L) unname(num_) else NULL,
      general_title  = "",                     # the notes speak for themselves; a "Note:" label
      footnote_order = c("number", "general"), # numbered first, since markers point at them
      threeparttable = TRUE
    )
  }
  # The colspan defect is html markup, so the repair applies there and nowhere else.
  if (identical(fmt_, "latex")) out_ else tbl_fix_footnote(.kable = out_, .ncol = ncol(.tab))
}

#' Repair the html footnote block: column span and rules
#'
#' Two defects in the same element, both fixed by substitution on the generated markup because both
#' are markup rather than styling.
#'
#' THE SPAN. kableExtra writes the footnote row as `colspan="100%"`, which the html specification does
#' not permit -- the attribute takes a non-negative integer. Browsers therefore fail to parse it and
#' fall back to a span of one, so the note renders inside the first column and wraps to that column's
#' width. Substituting the real column count is the whole repair, and no css rule reaches it.
#'
#' THE RULES. A footnote row is a table row, so it inherits the horizontal rule the theme draws
#' between rows, and a block of four notes arrives fenced by four lines. The notes are prose about the
#' table rather than more of its data, so they take no rules. The table keeps its own closing rule,
#' which is what separates the two.
#'
#' @param .kable A kable with a footnote already attached.
#' @param .ncol Integer. Columns the footnote should span.
#' @return The same kable, repaired.
tbl_fix_footnote <- function(.kable, .ncol) {
  if (FALSE) {
    .kable <- knitr::kable(utils::head(iris), format = "html") |>
      kableExtra::footnote(number = "a note")
    .ncol  <- 5L
  }
  txt_ <- as.character(.kable)
  txt_ <- gsub('colspan="100%"', paste0('colspan="', .ncol, '"'), txt_, fixed = TRUE)
  txt_ <- gsub("colspan='100%'", paste0("colspan='", .ncol, "'"), txt_, fixed = TRUE)
  # kableExtra emits exactly this style on every footnote cell, so it is the anchor the rule removal
  # attaches to. A footnote block styled some other way is left alone rather than guessed at.
  txt_ <- gsub('style="padding: 0; "', 'style="padding: 0; border: 0;"', txt_, fixed = TRUE)
  txt_ <- gsub("style='padding: 0; '", "style='padding: 0; border: 0;'", txt_, fixed = TRUE)
  structure(txt_, format = "html", class = "knitr_kable")
}
