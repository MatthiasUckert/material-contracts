# _Plots.R: the shared figure design layer -----------------------------------------------------------------------------
#
# Every figure in the monorepo takes its look from this file: fonts, palettes, theme, scales, the
# height ladder, and a small set of recurring figure shapes. Nothing here knows what a contract is.
# Domain scripts keep their own plot functions -- those live next to the data they understand -- and
# call in here for the look, so a restyle is an edit to one file rather than a sweep across six.
#
# Sourced by every runbook immediately after _Utils.R, and therefore before any script's own library.
#
# WHAT BELONGS HERE
#   Anything that decides how a figure looks and is not specific to one analysis: the theme, the
#   palettes, axis formatting, the height ladder, and the four figure shapes that recur often enough
#   that six separate implementations would drift apart.
#
# WHAT DOES NOT
#   The taxonomy, the engine names, the metric definitions. A figure function that has to know the
#   twelve contract categories belongs in the script that owns them. What such a script does instead
#   is register its vocabulary once (section 5), after which every figure drawing that vocabulary
#   gets the same order, the same short labels and the same colours without this file ever learning
#   what the categories mean.
#
# Style: native pipe, explicit namespace, dot-prefixed arguments, underscore-suffixed locals, ASCII.


# 1. Design tokens -------------------------------------------------------------------------------------------------
# The single place any colour or measurement is written down. Everything below reads these; nothing
# below hard-codes a hex value or a point size.

# Times to match the rendered documents (_styles.css) and the manuscript. The family is a system
# font, and on a machine without it ggplot2 substitutes sans without comment -- a difference nobody
# notices on screen and every referee notices in a submitted PDF. The check below is the whole
# defence: it fires once, when this file is sourced, naming what will actually be drawn.
.plot_font <- "Times New Roman"
.plot_base <- 11

if (!.plot_font %in% systemfonts::system_fonts()$family) {
  cli::cli_alert_warning(
    "{.val {(.plot_font)}} is not installed; figures will fall back to the default sans family."
  )
}

# Hairlines. Axis rules, ticks and tile borders all share one weight so a figure reads as one object.
.plot_line <- 0.2

# Ink. A single fill for bars whose identity is already carried by position, and a lighter grey for
# reference lines, null diagonals and incumbent markers that must recede behind the data.
.plot_ink <- "#003b73"
.plot_ref <- "#7A8FA3"

# The blue ramp, re-sorted by luminance. These are the ten colours the project has used since the
# first descriptives; the ordering is the only change. The original declaration order was not
# monotone in lightness, which meant a sequential fill jumped mid-scale and collapsed ambiguously
# when a referee printed the page in greyscale.
.plot_blues <- c(
  "#bfd7ed",   # baby blue
  "#60a3d9",   # blue grotto
  "#4682b4",   # steel blue
  "#007fff",   # azure
  "#0074b7",   # royal blue
  "#0047ab",   # cobalt blue
  "#00416a",   # indigo dye
  "#003b73",   # navy blue
  "#003153",   # prussian blue
  "#002147"    # oxford blue
)

# Categorical slots, fixed order, never cycled. Ordered so that the small-n cases separate as widely
# as possible: two categories get maximum luminance contrast within the house hue, and the warm
# accent enters only at three, where blue alone stops being reliably distinguishable. Past eight,
# colour has stopped carrying the information and the answer is a table or small multiples, not a
# ninth hue -- so plot_pal_cat() refuses rather than inventing one.
.plot_cats <- c(
  "#0074b7",   # royal blue
  "#002147",   # oxford blue
  "#B7791F",   # ochre -- the one warm accent
  "#60a3d9",   # blue grotto
  "#7A8FA3",   # slate
  "#8C4A2F",   # rust
  "#4682b4",   # steel blue
  "#bfd7ed"    # baby blue
)

# Binary. Carried forward unchanged from the descriptives, where these two greys have always meant
# amended against original.
.plot_bins <- c("#888888", "#CFCFCF")

# The height ladder. Width is fixed globally in _quarto.yml and in each runbook's setup chunk; height
# is the only dimension that varies, and it varies with row count rather than with taste. A ladder
# rather than free choice because every figure is read stacked in the Overview, and figures of
# arbitrary differing shapes cannot be compared down the page.
.plot_ladder <- c(Small = 2.4, Medium = 3.4, Large = 4.6, Square = 5.2)


# 2. Palettes ------------------------------------------------------------------------------------------------------
# Three jobs, three functions. Choosing between them is a statement about the data: sequential says
# the categories are ordered, categorical says they are not, binary says there are exactly two and
# they are opposites.

#' Sequential blue ramp of any length
#'
#' For fills that encode magnitude or an ordered category. Interpolates through the house blues in
#' luminance order, so the result reads as a single ramp at any length and survives greyscale.
#'
#' @param .n Integer. Number of colours required.
#' @param .rev Logical. Reverse, so the darkest colour comes first.
#' @return Character vector of .n hex colours, light to dark.
plot_pal_seq <- function(.n, .rev = FALSE) {
  if (FALSE) {
    .n   <- 5L
    .rev <- FALSE
  }
  if (.n < 1L) cli::cli_abort("plot_pal_seq() needs at least one colour; got {(.n)}.")
  out_ <- grDevices::colorRampPalette(.plot_blues)(.n)
  if (.rev) rev(out_) else out_
}

#' Categorical palette, fixed slot order
#'
#' For unordered groups: methods, engines, provenance, taxonomy branches. The order is fixed so that
#' a category keeps its colour when a figure is filtered or re-sorted -- colour follows the entity,
#' never its rank, or two panels of the same document contradict each other.
#'
#' @param .n Integer. Number of categories, at most eight.
#' @return Character vector of .n hex colours.
plot_pal_cat <- function(.n) {
  if (FALSE) .n <- 4L
  if (.n < 1L) cli::cli_abort("plot_pal_cat() needs at least one colour; got {(.n)}.")
  if (.n > length(.plot_cats)) {
    cli::cli_abort(c(
      "plot_pal_cat() caps at {length(.plot_cats)} categories; {(.n)} were requested.",
      "i" = "Beyond eight, colour no longer separates. Fold the tail into a residual group, use small
             multiples, or report a table."
    ))
  }
  .plot_cats[seq_len(.n)]
}

#' The two-category greys
#'
#' @return Character vector of length two, darker first.
plot_pal_bin <- function() {
  .plot_bins
}

#' Greyscale ramp
#'
#' The mono fallback, for a figure destined for a print-only context or one where colour would imply
#' a distinction the data does not support.
#'
#' @param .n Integer. Number of colours required.
#' @param .rev Logical. Reverse, so the darkest colour comes first.
#' @return Character vector of .n hex greys, light to dark.
plot_pal_grey <- function(.n, .rev = FALSE) {
  if (FALSE) {
    .n   <- 4L
    .rev <- FALSE
  }
  if (.n < 1L) cli::cli_abort("plot_pal_grey() needs at least one colour; got {(.n)}.")
  out_ <- grDevices::colorRampPalette(c("#E8E8E8", "#1A1A1A"))(.n)
  if (.rev) rev(out_) else out_
}


# 3. Theme ---------------------------------------------------------------------------------------------------------
# One theme, three switches. The switches exist because a heatmap and a bar chart genuinely want
# different chrome, and without them the alternative is what the project had before: a different
# theme_*() reached for in each file, which is how four visual dialects appeared in one repository.

#' The house ggplot theme
#'
#' Times, classic axes, hairline rules, no panel grid unless asked. Returns a theme object rather
#' than taking a plot, so a caller can layer one further theme() on top -- an angled axis label, a
#' suppressed strip -- without unwrapping anything.
#'
#' @param .base Numeric. Base font size in points.
#' @param .grid Character. Which panel gridlines to draw: "none", "y", "x" or "both".
#' @param .legend Character. Legend placement: "bottom", "right", "top" or "none".
#' @param .title Logical. Reserve space for a plot title. FALSE by default, because the figure
#'   caption carries the title in a rendered document and printing both duplicates it.
#' @return A ggplot2 theme object, to be added to a plot.
plot_theme <- function(.base = .plot_base, .grid = c("none", "y", "x", "both"),
                       .legend = c("bottom", "right", "top", "none"), .title = FALSE) {
  if (FALSE) {
    .base   <- 11
    .grid   <- "none"
    .legend <- "bottom"
    .title  <- FALSE
  }
  .grid   <- match.arg(.grid)
  .legend <- match.arg(.legend)

  grid_y_ <- if (.grid %in% c("y", "both")) {
    ggplot2::element_line(linewidth = .plot_line, colour = "#E4E4E4")
  } else {
    ggplot2::element_blank()
  }
  grid_x_ <- if (.grid %in% c("x", "both")) {
    ggplot2::element_line(linewidth = .plot_line, colour = "#E4E4E4")
  } else {
    ggplot2::element_blank()
  }

  ggplot2::theme_classic(base_size = .base, base_family = .plot_font, base_line_size = .plot_line) +
    ggplot2::theme(
      text               = ggplot2::element_text(family = .plot_font, colour = "black"),
      axis.title         = ggplot2::element_text(family = .plot_font, face = "plain", size = .base),
      axis.text          = ggplot2::element_text(family = .plot_font, colour = "black", size = .base - 1),
      axis.line          = ggplot2::element_line(linewidth = .plot_line, colour = "black"),
      axis.ticks         = ggplot2::element_line(linewidth = .plot_line, colour = "black"),
      axis.ticks.length  = ggplot2::unit(0.15, "cm"),
      panel.grid.major.y = grid_y_,
      panel.grid.major.x = grid_x_,
      panel.grid.minor   = ggplot2::element_blank(),
      panel.border       = ggplot2::element_blank(),
      panel.spacing      = ggplot2::unit(0.4, "cm"),
      strip.background   = ggplot2::element_rect(fill = "white", colour = "black", linewidth = .plot_line),
      strip.text         = ggplot2::element_text(family = .plot_font, face = "bold", size = .base - 1),
      plot.title         = if (.title) {
        ggplot2::element_text(family = .plot_font, face = "bold", hjust = 0.5, size = .base + 1)
      } else {
        ggplot2::element_blank()
      },
      plot.margin        = ggplot2::margin(4, 8, 4, 4),
      legend.position    = .legend,
      legend.title       = ggplot2::element_text(family = .plot_font, size = .base - 1),
      legend.text        = ggplot2::element_text(family = .plot_font, size = .base - 1),
      legend.key.size    = ggplot2::unit(0.45, "cm"),
      legend.margin      = ggplot2::margin(2, 0, 0, 0)
    )
}

#' Apply the house theme to an existing plot
#'
#' The compatibility form, for call sites written against a function that took a plot and returned
#' one. New code should add plot_theme() instead, which composes.
#'
#' @param .plot A ggplot object.
#' @param ... Passed to plot_theme().
#' @return The plot with the house theme applied.
plot_apply_theme <- function(.plot, ...) {
  .plot + plot_theme(...)
}


# 4. Scales --------------------------------------------------------------------------------------------------------
# Fill and colour scales bound to the palettes, plus the axis formatting that would otherwise be
# retyped in every figure. The axis helpers are not cosmetic: the expansion argument is what leaves
# room for a value label at the end of a bar, and getting it wrong clips the label off the panel.

#' Discrete fill from the sequential ramp
#' @param ... Passed to ggplot2::scale_fill_manual().
#' @param .rev Logical. Darkest colour first.
#' @return A ggplot2 scale.
plot_scale_fill_seq <- function(..., .rev = FALSE) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette    = function(.n) plot_pal_seq(.n, .rev = .rev),
    ...
  )
}

#' Discrete fill from the categorical palette
#'
#' Built on scale_fill_manual rather than a palette function, because passing the full slot vector
#' means a category keeps the colour its position earns even when a figure is filtered to a subset.
#' A palette function would re-deal from slot one and repaint the survivors.
#'
#' @param ... Passed to ggplot2::scale_fill_manual().
#' @return A ggplot2 scale.
plot_scale_fill_cat <- function(...) {
  ggplot2::scale_fill_manual(values = .plot_cats, ...)
}

#' Discrete colour from the categorical palette
#' @param ... Passed to ggplot2::scale_colour_manual().
#' @return A ggplot2 scale.
plot_scale_colour_cat <- function(...) {
  ggplot2::scale_colour_manual(values = .plot_cats, ...)
}

#' Continuous fill: white to the darkest house blue
#'
#' For magnitude on a tile or a raster. White at the low end rather than a pale blue, so an empty
#' cell is unmistakably empty and does not read as a small positive value.
#'
#' @param ... Passed to ggplot2::scale_fill_gradient().
#' @return A ggplot2 scale.
plot_scale_fill_grad <- function(...) {
  ggplot2::scale_fill_gradient(low = "#FFFFFF", high = "#002147", ...)
}

#' Continuous colour: white to the darkest house blue
#'
#' The colour twin of plot_scale_fill_grad, for points and lines carrying a magnitude.
#'
#' @param ... Passed to ggplot2::scale_colour_gradient().
#' @return A ggplot2 scale.
plot_scale_colour_grad <- function(...) {
  ggplot2::scale_colour_gradient(low = "#bfd7ed", high = "#002147", ...)
}

#' Percent axis with no wasted expansion
#'
#' Breaks are ggplot2's default unless .breaks is given; see plot_scale_y_pct() for when to give it.
#'
#' @param .accuracy Numeric. Rounding for the tick labels.
#' @param .expand Numeric length-two. Multiplicative expansion, lower then upper.
#' @param .breaks Break function or vector. Defaults to ggplot2's own choice.
#' @return A ggplot2 scale.
plot_scale_x_pct <- function(.accuracy = 1, .expand = c(0, 0.02), .breaks = ggplot2::waiver()) {
  ggplot2::scale_x_continuous(
    labels = scales::label_percent(accuracy = .accuracy),
    breaks = .breaks,
    expand = ggplot2::expansion(mult = .expand)
  )
}

#' Percent axis with no wasted expansion
#'
#' .breaks defaults to ggplot2's own choice, which is scales::breaks_extended() -- an algorithm
#' optimising a trade-off between simplicity and coverage that on a percentage axis will return
#' sequences like 2, 5, 8, 10. No reader would choose that, and it looks unconsidered beside every
#' other axis in the document. Pass scales::breaks_pretty(), which is restricted to multiples of
#' one, two and five, wherever the default comes out badly.
#'
#' IT IS NOT THE DEFAULT, and the reason is a rule rather than a preference: every figure in the 03
#' family already renders through this function, so changing what it returns would silently move
#' tick marks across documents that are finished. Changes to this file are additive, with defaults
#' that preserve existing behaviour, until a deliberate pass re-renders everything together.
#'
#' There is an inconsistency worth knowing about when that pass happens: plot_bar_ranked() already
#' uses pretty breaks on its fixed-limit branch and this function's default on the other, so one
#' figure type can render its axis two ways depending on whether limits were supplied.
#'
#' @param .accuracy Numeric. Rounding for the tick labels.
#' @param .expand Numeric length-two. Multiplicative expansion, lower then upper.
#' @param .breaks Break function or vector. Defaults to ggplot2's own choice.
#' @return A ggplot2 scale.
plot_scale_y_pct <- function(.accuracy = 1, .expand = c(0, 0.02), .breaks = ggplot2::waiver()) {
  ggplot2::scale_y_continuous(
    labels = scales::label_percent(accuracy = .accuracy),
    breaks = .breaks,
    expand = ggplot2::expansion(mult = .expand)
  )
}

#' Count axis, comma-grouped, with headroom for an end-of-bar label
#' @param .expand Numeric length-two. Multiplicative expansion, lower then upper. The upper default
#'   is the room a value label printed past the end of a bar needs.
#' @return A ggplot2 scale.
plot_scale_x_count <- function(.expand = c(0, 0.12)) {
  ggplot2::scale_x_continuous(
    labels = scales::label_comma(),
    expand = ggplot2::expansion(mult = .expand)
  )
}

#' Count axis, comma-grouped, with headroom for an end-of-bar label
#' @param .expand Numeric length-two. Multiplicative expansion, lower then upper.
#' @return A ggplot2 scale.
plot_scale_y_count <- function(.expand = c(0, 0.12)) {
  ggplot2::scale_y_continuous(
    labels = scales::label_comma(),
    expand = ggplot2::expansion(mult = .expand)
  )
}


# 5. Level registry ------------------------------------------------------------------------------------------------
# The mechanism that makes two figures comparable without this file knowing what they show.
#
# A script that owns a vocabulary -- the taxonomy, the set of engines, the routing tiers -- registers
# it once at the top of its own library, naming the canonical order, optional short labels, and
# optional fixed colours. Every figure afterwards calls plot_factor() and receives that order. The
# guarantee is worth stating precisely: a category occupies the same position and carries the same
# colour in every figure of every document, so two panels can be compared by eye rather than by
# reading both sets of axis labels.
#
# The registry is rebuilt empty each time this file is sourced. That is deliberate -- a stale entry
# surviving a re-source is exactly the failure this is meant to prevent -- and it means registration
# must happen in a file sourced after this one, which is where a domain library sits anyway.

.plot_levels <- new.env(parent = emptyenv())

#' Register a canonical level order under a key
#'
#' @param .key Character. Name the vocabulary is retrieved by, conventionally the column name.
#' @param .levels Character vector. The canonical order, as it should appear on an axis.
#' @param .short Character vector or NULL. Display labels, same length and order as .levels. Supply
#'   these wherever the full names run long: an axis label built for uniqueness will consume any
#'   width it is given, and the answer is a shorter label rather than a wider figure.
#' @param .colours Character vector or NULL. Fixed colours, same length and order as .levels.
#'   Defaults to the categorical palette, which caps at eight -- beyond that, pass explicit colours
#'   or accept that these levels are ordered by position only.
#' @return Invisibly, the registered entry as a list.
plot_register_levels <- function(.key, .levels, .short = NULL, .colours = NULL) {
  if (FALSE) {
    .key     <- "AmendType"
    .levels  <- c("Original", "Amended")
    .short   <- NULL
    .colours <- plot_pal_bin()
  }
  if (!is.character(.key) || length(.key) != 1L) cli::cli_abort("{.arg .key} must be one string.")
  if (anyDuplicated(.levels) > 0L) {
    cli::cli_abort("Duplicate level{?s} in {.arg .levels}: {unique(.levels[duplicated(.levels)])}.")
  }
  if (!is.null(.short) && length(.short) != length(.levels)) {
    cli::cli_abort("{.arg .short} has {length(.short)} entr{?y/ies} for {length(.levels)} level{?s}.")
  }
  if (!is.null(.colours) && length(.colours) != length(.levels)) {
    cli::cli_abort("{.arg .colours} has {length(.colours)} entr{?y/ies} for {length(.levels)} level{?s}.")
  }

  cols_ <- .colours
  if (is.null(cols_) && length(.levels) <= length(.plot_cats)) cols_ <- plot_pal_cat(length(.levels))

  entry_ <- list(
    Levels  = as.character(.levels),
    Short   = if (is.null(.short)) as.character(.levels) else as.character(.short),
    Colours = if (is.null(cols_)) NULL else purrr::set_names(cols_, .levels)
  )
  assign(.key, entry_, envir = .plot_levels)
  invisible(entry_)
}

#' Retrieve a registered entry
#'
#' @param .key Character. The registration key.
#' @return A list with Levels, Short and Colours.
plot_entry <- function(.key) {
  if (FALSE) .key <- "AmendType"
  if (!exists(.key, envir = .plot_levels, inherits = FALSE)) {
    known_ <- ls(envir = .plot_levels)
    cli::cli_abort(c(
      "No levels registered under {.val {(.key)}}.",
      "i" = if (length(known_) == 0L) {
        "Nothing is registered yet; the owning library calls plot_register_levels() when sourced."
      } else {
        paste0("Registered: ", paste(known_, collapse = ", "), ".")
      }
    ))
  }
  get(.key, envir = .plot_levels, inherits = FALSE)
}

#' The canonical order registered under a key
#' @param .key Character. The registration key.
#' @return Character vector of levels.
plot_levels <- function(.key) {
  plot_entry(.key)$Levels
}

#' Coerce a column to a factor in its canonical order
#'
#' An unregistered value is an error rather than a silent NA. A dropped category produces a figure
#' that looks entirely normal while omitting a row, which is the failure mode worth being loud about.
#'
#' @param .x Character or factor vector.
#' @param .key Character. The registration key.
#' @param .short Logical. Relabel to the registered short names, keeping the order.
#' @param .extra Character vector or NULL. Levels appended after the registered ones, for sentinels
#'   a vocabulary does not contain -- an abstention marker, an unresolved bucket.
#' @param .rev Logical. Reverse the level order. Useful for a horizontal bar chart, where ggplot
#'   builds the y axis from the bottom up and the first level would otherwise land last.
#' @return A factor.
plot_factor <- function(.x, .key, .short = FALSE, .extra = NULL, .rev = FALSE) {
  if (FALSE) {
    .x     <- c("Amended", "Original", "Amended")
    .key   <- "AmendType"
    .short <- FALSE
    .extra <- NULL
    .rev   <- FALSE
  }
  entry_  <- plot_entry(.key)

  # A sentinel is appended only when the data actually contains it. Registered categories are always
  # kept, because a category the model never predicted should still show as an empty column and that
  # emptiness is a finding. A sentinel is different: it is not a category, and a method that never
  # abstains would otherwise be drawn with a phantom row and column labelled "(none)" that no
  # document could ever occupy.
  seen_extra_ <- intersect(.extra, unique(as.character(.x)))
  levels_ <- c(entry_$Levels, seen_extra_)
  labels_ <- c(if (.short) entry_$Short else entry_$Levels, seen_extra_)

  unknown_ <- setdiff(unique(as.character(.x)), c(levels_, NA_character_))
  if (length(unknown_) > 0L) {
    # The count leads the sentence rather than a bare "Value{?s}". A pluralisation marker takes its
    # quantity from an interpolation, and with none before it and two after, cli cannot tell which
    # of them to count -- so the abort itself fails, and the message naming the real problem is the
    # one thing the reader never sees.
    cli::cli_abort(c(
      "{length(unknown_)} value{?s} not registered under {.val {(.key)}}: {unknown_}.",
      "i" = "Register them, or pass them through {.arg .extra} if they are sentinels."
    ))
  }
  if (.rev) {
    levels_ <- rev(levels_)
    labels_ <- rev(labels_)
  }
  factor(as.character(.x), levels = levels_, labels = labels_)
}

#' Fill scale using a key's registered colours
#' @param .key Character. The registration key.
#' @param .short Logical. Use the registered short names in the legend.
#' @param ... Passed to ggplot2::scale_fill_manual().
#' @return A ggplot2 scale.
plot_scale_fill_key <- function(.key, .short = FALSE, ...) {
  if (FALSE) {
    .key   <- "AmendType"
    .short <- FALSE
  }
  entry_ <- plot_entry(.key)
  if (is.null(entry_$Colours)) {
    cli::cli_abort("No colours registered under {.val {(.key)}}; it has {length(entry_$Levels)} levels.")
  }
  vals_ <- entry_$Colours
  if (.short) names(vals_) <- entry_$Short
  ggplot2::scale_fill_manual(values = vals_, ...)
}

#' Colour scale using a key's registered colours
#' @param .key Character. The registration key.
#' @param .short Logical. Use the registered short names in the legend.
#' @param ... Passed to ggplot2::scale_colour_manual().
#' @return A ggplot2 scale.
plot_scale_colour_key <- function(.key, .short = FALSE, ...) {
  if (FALSE) {
    .key   <- "AmendType"
    .short <- FALSE
  }
  entry_ <- plot_entry(.key)
  if (is.null(entry_$Colours)) {
    cli::cli_abort("No colours registered under {.val {(.key)}}; it has {length(entry_$Levels)} levels.")
  }
  vals_ <- entry_$Colours
  if (.short) names(vals_) <- entry_$Short
  ggplot2::scale_colour_manual(values = vals_, ...)
}


# 6. Geometry ------------------------------------------------------------------------------------------------------
# Height comes from the row count, not from taste. Used as a chunk option, which is why it must be a
# plain function of one number:
#
#   #| fig-height: !expr plot_height(12)

#' Figure height from the content
#'
#' @param .n_rows Integer. Categories on the discrete axis.
#' @param .square Logical. A matrix figure, where height must track width rather than row count.
#' @return Numeric height in inches, taken from the ladder.
plot_height <- function(.n_rows, .square = FALSE) {
  if (FALSE) {
    .n_rows <- 12L
    .square <- FALSE
  }
  if (.square) return(unname(.plot_ladder[["Square"]]))
  if (.n_rows <= 3L)  return(unname(.plot_ladder[["Small"]]))
  if (.n_rows <= 8L)  return(unname(.plot_ladder[["Medium"]]))
  if (.n_rows <= 15L) return(unname(.plot_ladder[["Large"]]))
  # Past fifteen rows the ladder runs out. Derive rather than invent, and cap: a figure taller than
  # nine inches has stopped being one figure and wants splitting or a table.
  min(9.0, 1.2 + 0.28 * .n_rows)
}

#' Wrap long labels across lines
#'
#' The alternative to a wider figure. Width is fixed globally, so a label that does not fit is
#' shortened or wrapped; it is never accommodated by growing the canvas.
#'
#' @param .x Character vector.
#' @param .width Integer. Target characters per line.
#' @return Character vector with newlines inserted.
plot_wrap <- function(.x, .width = 24L) {
  stringr::str_wrap(.x, width = .width)
}


# 7. Figure primitives ---------------------------------------------------------------------------------------------
# Four shapes that recur often enough across the pipeline that separate implementations would drift.
# Each takes a tidy tibble plus column names as strings, returns a themed ggplot, and adds no title:
# the caption in the runbook carries that.
#
# These are a floor, not a ceiling. A figure that needs something else builds it directly and adds
# plot_theme(); the primitives exist for the cases where six scripts would otherwise each write the
# same twenty lines slightly differently.
#
# Note on fill: a ranked bar chart is drawn in a single ink. Position already identifies the
# category, so colouring twelve bars twelve ways adds an encoding that carries no information and
# costs the reader a legend lookup. Where .key is supplied it fixes the ORDER, not the colour.

#' Horizontal ranked bar chart with value labels
#'
#' @param .tab Tibble with one row per category.
#' @param .cat Character. Column holding the category.
#' @param .val Character. Column holding the value.
#' @param .key Character or NULL. Registration key. Supplied, the bars take the canonical order;
#'   NULL, they are ordered by value.
#' @param .short Logical. Use registered short labels. Requires .key.
#' @param .label Logical. Print the value past the end of each bar.
#' @param .accuracy Numeric or NULL. Rounding for the value labels. NULL prints whole numbers with
#'   comma grouping and everything else to two decimals.
#' @param .pct Logical. Treat the value as a proportion: percent axis and percent labels.
#' @param .limits Numeric length-two or NULL. Fix the value axis. Supply this whenever several
#'   figures show the same bounded metric -- per-category F1 across three tasks, say. Left to
#'   auto-scale, each panel fits its own data and three panels that look alike are on three different
#'   axes, which is the specific misreading these figures exist to prevent.
#' @param .extra Character vector or NULL. Levels drawn after the registered ones, for sentinels a
#'   vocabulary does not contain. Requires .key.
#' @param .desc Logical. Largest bar at the top. Ignored when .key is supplied.
#' @return A ggplot.
plot_bar_ranked <- function(.tab, .cat, .val, .key = NULL, .short = FALSE, .label = TRUE,
                            .accuracy = NULL, .pct = FALSE, .limits = NULL, .extra = NULL,
                            .desc = TRUE) {
  if (FALSE) {
    .tab      <- tibble::tibble(Class = c("Leases", "Licenses", "Other"), N = c(303L, 210L, 88L))
    .cat      <- "Class"
    .val      <- "N"
    .key      <- NULL
    .short    <- FALSE
    .label    <- TRUE
    .accuracy <- NULL
    .pct      <- FALSE
    .limits   <- NULL
    .extra    <- NULL
    .desc     <- TRUE
  }
  dat_ <- .tab |>
    dplyr::mutate(
      PlotVal = as.numeric(.data[[.val]]),
      PlotCat = if (is.null(.key)) {
        forcats::fct_reorder(as.character(.data[[.cat]]), .data$PlotVal, .desc = !.desc)
      } else {
        plot_factor(.data[[.cat]], .key = .key, .short = .short, .extra = .extra, .rev = TRUE)
      }
    )

  fmt_ <- if (.pct) {
    scales::label_percent(accuracy = if (is.null(.accuracy)) 0.1 else .accuracy)
  } else if (is.null(.accuracy)) {
    function(.v) dplyr::if_else(.v == round(.v), scales::label_comma()(.v), scales::number(.v, accuracy = 0.01))
  } else {
    scales::label_number(accuracy = .accuracy)
  }

  p_ <- dat_ |>
    ggplot2::ggplot(ggplot2::aes(x = .data$PlotVal, y = .data$PlotCat)) +
    ggplot2::geom_col(fill = .plot_ink, width = 0.7)

  if (.label) {
    p_ <- p_ + ggplot2::geom_text(
      ggplot2::aes(label = fmt_(.data$PlotVal)),
      hjust = -0.18, size = (.plot_base - 3) / ggplot2::.pt, family = .plot_font
    )
  }

  # A fixed limit and the label headroom fight each other: the expansion that leaves room for a value
  # label past the end of a bar would push the axis beyond an explicit upper bound. Where limits are
  # given, the headroom is folded into the upper limit instead, so the label still fits and the axis
  # still matches its sibling panels.
  scale_ <- if (!is.null(.limits)) {
    pad_ <- if (.label) 0.08 * diff(.limits) else 0
    ggplot2::scale_x_continuous(
      limits = c(.limits[[1]], .limits[[2]] + pad_),
      breaks = scales::breaks_pretty(n = 5)(.limits),
      labels = if (.pct) scales::label_percent() else scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0, 0))
    )
  } else if (.pct) {
    plot_scale_x_pct(.expand = c(0, 0.12))
  } else {
    plot_scale_x_count()
  }

  p_ +
    scale_ +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "none", .legend = "none")
}

#' Heatmap of a matrix-shaped tibble
#'
#' Rows run top to bottom in the order given, so a confusion matrix reads with its diagonal from
#' top-left, which is where a reader looks for it.
#'
#' @param .tab Long tibble: one row per cell.
#' @param .x Character. Column mapped to the horizontal axis.
#' @param .y Character. Column mapped to the vertical axis.
#' @param .fill Character. Column holding the cell value.
#' @param .key_x Character or NULL. Registration key ordering the horizontal axis.
#' @param .key_y Character or NULL. Registration key ordering the vertical axis.
#' @param .short Logical. Use registered short labels on both axes.
#' @param .label Logical. Print the value in each cell.
#' @param .pct Logical. Treat the value as a proportion.
#' @param .accuracy Numeric. Rounding for the cell labels.
#' @param .angle Numeric. Rotation of the horizontal axis labels.
#' @param .extra Character vector or NULL. Levels drawn after the registered ones on whichever axes
#'   are keyed, for sentinels a vocabulary does not contain. A method that can abstain predicts one,
#'   and the column showing which true categories it abstained on is exactly what the figure is for.
#' @param .limits Numeric length-two or NULL. Fix the fill scale. Supply this for any quantity with a
#'   meaningful absolute scale -- a share, an accuracy, an agreement rate. Left to auto-scale, the
#'   ramp is stretched across whatever range the data happens to occupy, so a near-constant quantity
#'   renders its rounding noise as strong visual structure, and two panels that should be compared
#'   end up on two different scales while looking alike.
#' @param .label_min Numeric or NULL. Suppress the printed value on cells below this. A sparse matrix
#'   is mostly zeros, and printing every one of them buries the handful of cells that carry the
#'   result under a field of noughts. The cell is still drawn and still shaded; only its label goes.
#' @param .cell_label Character or NULL. Print this column's values in the cells while shading by
#'   .fill. Separating the two is what makes a matrix readable when its columns are on wildly
#'   different scales: shading a raw count that runs 3 to 500 puts every column but the largest at
#'   the white end of the ramp, so the figure shows one column and hides the rest. Shade a
#'   within-column share and print the count, and every column becomes legible without the numbers
#'   changing. NULL prints .fill, which is the ordinary case.
#' @param .drop Which axes discard levels nothing lands on. "none", the default, keeps every
#'   registered level -- correct for a confusion matrix, where a category the classifier never
#'   predicts must still hold its column, because its absence is the result. Name an axis where a
#'   missing level is structural rather than informative: an engine that cannot emit a label has not
#'   disagreed about it, and drawing it as an empty row invites exactly that misreading. It is
#'   per-axis rather than a single flag because the two axes usually differ -- the same figure can
#'   want its entity labels dropped and its contract types kept. Under a facet, dropping happens per
#'   panel only where the facet is declared with free scales.
#' @return A ggplot.
plot_heatmap <- function(.tab, .x, .y, .fill, .key_x = NULL, .key_y = NULL, .short = FALSE,
                         .label = TRUE, .pct = FALSE, .accuracy = 0.01, .angle = 40,
                         .extra = NULL, .limits = NULL, .label_min = NULL,
                         .cell_label = NULL, .drop = c("none", "x", "y", "both")) {
  if (FALSE) {
    .tab      <- tibble::tibble(Pred = c("A", "B"), True = c("A", "A"), Share = c(0.9, 0.1))
    .x        <- "Pred"
    .y        <- "True"
    .fill     <- "Share"
    .key_x    <- NULL
    .key_y    <- NULL
    .short    <- FALSE
    .label    <- TRUE
    .pct      <- TRUE
    .accuracy <- 0.01
    .angle    <- 40
    .extra     <- NULL
    .limits    <- NULL
    .label_min <- NULL
    .cell_label <- NULL
    .drop      <- "none"
  }
  .drop    <- match.arg(.drop)
  drop_x_  <- .drop %in% c("x", "both")
  drop_y_  <- .drop %in% c("y", "both")
  dat_ <- .tab |>
    dplyr::mutate(
      PlotFill = as.numeric(.data[[.fill]]),
      PlotText = if (is.null(.cell_label)) {
        as.numeric(.data[[.fill]])
      } else {
        as.numeric(.data[[.cell_label]])
      },
      PlotX    = if (is.null(.key_x)) factor(as.character(.data[[.x]])) else {
        plot_factor(.data[[.x]], .key = .key_x, .short = .short, .extra = .extra)
      },
      PlotY    = if (is.null(.key_y)) factor(as.character(.data[[.y]])) else {
        plot_factor(.data[[.y]], .key = .key_y, .short = .short, .extra = .extra)
      }
    )

  fmt_ <- if (.pct) {
    scales::label_percent(accuracy = 100 * .accuracy)
  } else {
    scales::label_number(accuracy = .accuracy)
  }

  # Text on a filled tile has to survive both ends of the ramp, so it flips to white on the dark
  # half rather than sitting in one colour that is unreadable somewhere. The threshold is the midpoint
  # of the SCALE, not of the data: with fixed limits the data may occupy a narrow band well away from
  # the middle, and splitting on the data's own midpoint would flip the text inside a uniform block.
  span_ <- if (is.null(.limits)) range(dat_$PlotFill, na.rm = TRUE) else .limits
  mid_  <- mean(span_)

  p_ <- dat_ |>
    ggplot2::ggplot(ggplot2::aes(x = .data$PlotX, y = .data$PlotY, fill = .data$PlotFill)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4)

  if (.label) {
    lab_ <- dat_
    if (!is.null(.label_min)) lab_ <- dplyr::filter(lab_, .data$PlotFill >= .label_min)
    # The printed value formats on its own scale. Where .cell_label is a count and .fill a share,
    # one formatter cannot serve both, and the percentage formatter would render a count of 262 as
    # 26,200%.
    fmt_lab_ <- if (is.null(.cell_label)) fmt_ else scales::label_number(accuracy = .accuracy)
    p_ <- p_ + ggplot2::geom_text(
      data    = lab_,
      mapping = ggplot2::aes(label = fmt_lab_(.data$PlotText), colour = .data$PlotFill > mid_),
      size    = (.plot_base - 3) / ggplot2::.pt, family = .plot_font, show.legend = FALSE
    ) +
      ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "black"), guide = "none")
  }

  p_ +
    plot_scale_fill_grad(
      labels = fmt_,
      name   = NULL,
      limits = .limits,
      # With .cell_label the ramp carries a different quantity from the printed numbers, so a legend
      # would invite reading the cells against the wrong scale.
      guide  = if (is.null(.cell_label)) "colourbar" else "none"
    ) +
    # The default keeps every registered level on the axis even when nothing lands on it. A category
    # a classifier never predicts would otherwise lose its column, and the matrix would quietly stop
    # being square -- hiding the very fact that the category is never chosen. Where an axis is not
    # keyed, its factor carries only observed levels, so this changes nothing.
    #
    # Naming an axis in .drop inverts that for that axis, and does so PER PANEL when the facet uses
    # free scales -- which is the only mechanism that works: dropping levels from the data instead
    # would remove a level from every panel as soon as one panel used it.
    ggplot2::scale_x_discrete(drop = drop_x_) +
    ggplot2::scale_y_discrete(limits = rev, drop = drop_y_) +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "none", .legend = "right") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = .angle, hjust = 1),
      axis.line   = ggplot2::element_blank(),
      axis.ticks  = ggplot2::element_blank()
    )
}

#' Point-and-interval chart, one row per item
#'
#' The leaderboard shape: an estimate per configuration with a spread around it. Drawn horizontally
#' because the item names are long and a vertical version would rotate them.
#'
#' @param .tab Tibble with one row per item.
#' @param .cat Character. Column holding the item name.
#' @param .val Character. Column holding the estimate.
#' @param .lo Character or NULL. Column holding the lower bound.
#' @param .hi Character or NULL. Column holding the upper bound.
#' @param .key Character or NULL. Registration key ordering the items; NULL orders by estimate.
#' @param .ref Numeric or NULL. Vertical reference line, drawn recessive behind the points.
#' @param .limits Numeric length-two or NULL. Fix the value axis. Left NULL the axis zooms to the
#'   estimates, which is usually what a ranking figure wants -- the differences it exists to show are
#'   often smaller than the metric's full range. Supply limits when several panels of the same metric
#'   must be compared against each other rather than read one at a time.
#' @param .desc Logical. Largest estimate at the top. Ignored when .key is supplied.
#' @return A ggplot.
plot_points_ci <- function(.tab, .cat, .val, .lo = NULL, .hi = NULL, .key = NULL,
                           .ref = NULL, .limits = NULL, .desc = TRUE) {
  if (FALSE) {
    .tab  <- tibble::tibble(Config = c("a", "b"), F1 = c(0.88, 0.81), Lo = c(0.85, 0.78),
                            Hi = c(0.91, 0.84))
    .cat  <- "Config"
    .val  <- "F1"
    .lo   <- "Lo"
    .hi   <- "Hi"
    .key    <- NULL
    .ref    <- NULL
    .limits <- NULL
    .desc   <- TRUE
  }
  dat_ <- .tab |>
    dplyr::mutate(
      PlotVal = as.numeric(.data[[.val]]),
      PlotCat = if (is.null(.key)) {
        forcats::fct_reorder(as.character(.data[[.cat]]), .data$PlotVal, .desc = !.desc)
      } else {
        plot_factor(.data[[.cat]], .key = .key, .rev = TRUE)
      }
    )

  p_ <- ggplot2::ggplot(dat_, ggplot2::aes(x = .data$PlotVal, y = .data$PlotCat))
  if (!is.null(.ref)) {
    p_ <- p_ + ggplot2::geom_vline(xintercept = .ref, linetype = 2, linewidth = 0.3, colour = .plot_ref)
  }
  if (!is.null(.lo) && !is.null(.hi)) {
    p_ <- p_ + ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data[[.lo]], xmax = .data[[.hi]]),
      orientation = "y", width = 0.22, linewidth = 0.3, colour = .plot_ink
    )
  }
  p_ <- p_ + ggplot2::geom_point(size = 1.9, colour = .plot_ink)
  # Expansion is set explicitly rather than left to the default. A ranking figure zooms hard, so five
  # percent of a range a few hundredths wide is a few thousandths of headroom, and the outermost
  # interval caps end up flush against the panel edge looking clipped.
  p_ <- p_ + if (is.null(.limits)) {
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.07))
  } else {
    ggplot2::scale_x_continuous(limits = .limits)
  }

  p_ +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "x", .legend = "none")
}

#' Stacked horizontal bar, optionally normalised to shares
#'
#' @param .tab Long tibble: one row per bar-segment combination.
#' @param .cat Character. Column holding the bar identity.
#' @param .val Character. Column holding the segment size.
#' @param .fill Character. Column holding the segment identity.
#' @param .key Character or NULL. Registration key ordering the bars.
#' @param .key_fill Character or NULL. Registration key ordering and colouring the segments; NULL
#'   falls back to the categorical palette in first-seen order.
#' @param .short Logical. Use registered short labels on the bar axis.
#' @param .share Logical. Normalise each bar to sum to one.
#' @param .desc Logical. Largest bar at the top. Ignored when .key is supplied.
#' @return A ggplot.
plot_bar_stacked <- function(.tab, .cat, .val, .fill, .key = NULL, .key_fill = NULL,
                             .short = FALSE, .share = FALSE, .desc = TRUE) {
  if (FALSE) {
    .tab      <- tibble::tibble(Class = c("A", "A", "B", "B"), Type = c("x", "y", "x", "y"),
                                N = c(10L, 4L, 7L, 9L))
    .cat      <- "Class"
    .val      <- "N"
    .fill     <- "Type"
    .key      <- NULL
    .key_fill <- NULL
    .short    <- FALSE
    .share    <- TRUE
    .desc     <- TRUE
  }
  dat_ <- .tab |>
    dplyr::mutate(PlotVal = as.numeric(.data[[.val]])) |>
    dplyr::mutate(BarTotal = sum(.data$PlotVal, na.rm = TRUE), .by = dplyr::all_of(.cat)) |>
    dplyr::mutate(
      PlotCat  = if (is.null(.key)) {
        forcats::fct_reorder(as.character(.data[[.cat]]), .data$BarTotal, .desc = !.desc)
      } else {
        plot_factor(.data[[.cat]], .key = .key, .short = .short, .rev = TRUE)
      },
      PlotFill = if (is.null(.key_fill)) {
        factor(as.character(.data[[.fill]]))
      } else {
        plot_factor(.data[[.fill]], .key = .key_fill)
      }
    )

  p_ <- dat_ |>
    ggplot2::ggplot(ggplot2::aes(x = .data$PlotVal, y = .data$PlotCat, fill = .data$PlotFill)) +
    ggplot2::geom_col(
      position = if (.share) {
        # reverse = TRUE so the first level draws leftmost. Without it ggplot stacks in reverse
        # factor order while the legend lists forward order, and the two disagree on the page.
        ggplot2::position_fill(reverse = TRUE)
      } else {
        ggplot2::position_stack(reverse = TRUE)
      },
      width = 0.7
    )

  scale_ <- if (is.null(.key_fill)) {
    plot_scale_fill_cat(name = NULL)
  } else {
    plot_scale_fill_key(.key = .key_fill, name = NULL)
  }

  p_ +
    scale_ +
    (if (.share) plot_scale_x_pct(.expand = c(0, 0)) else plot_scale_x_count(.expand = c(0, 0.02))) +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "none", .legend = "bottom")
}


# 8. Export --------------------------------------------------------------------------------------------------------
# Figures reach the manuscript as files, not as screenshots of a rendered page. Width is the same
# fixed width the documents use, so a figure looks identical whether a referee meets it in the
# runbook or in the submission.

#' Write a figure to disk in every requested format
#'
#' @param .plot A ggplot object.
#' @param .name Character. File stem, conventionally the chunk label without the "fig-" prefix.
#' @param .dir Character. Destination directory; created if absent.
#' @param .height Numeric. Height in inches, conventionally from plot_height().
#' @param .width Numeric. Width in inches. Fixed by default and should stay that way.
#' @param .formats Character vector. Extensions to write.
#' @param .dpi Numeric. Resolution for raster formats.
#' @return Invisibly, a character vector of the paths written.
plot_save <- function(.plot, .name, .dir, .height, .width = 7.5,
                      .formats = c("pdf", "png"), .dpi = 300) {
  if (FALSE) {
    .plot    <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
    .name    <- "example"
    .dir     <- fs::path_temp()
    .height  <- 3.4
    .width   <- 7.5
    .formats <- c("pdf", "png")
    .dpi     <- 300
  }
  fs::dir_create(.dir)
  # cairo_pdf embeds the system font; the base pdf device would substitute a Type 1 face and lose
  # Times. Cairo is not guaranteed to be compiled in, so fall back rather than fail.
  pdf_dev_ <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf"

  out_ <- purrr::map_chr(.formats, function(.ext) {
    path_ <- fs::path(.dir, paste0(.name, ".", .ext))
    ggplot2::ggsave(
      filename = path_,
      plot     = .plot,
      width    = .width,
      height   = .height,
      units    = "in",
      dpi      = .dpi,
      device   = if (.ext == "pdf") pdf_dev_ else .ext
    )
    as.character(path_)
  })
  invisible(out_)
}


# 9. Report --------------------------------------------------------------------------------------------------------

#' Print the design layer's current state
#'
#' The one block to paste when a figure looks wrong: what is registered, in what order, and what the
#' ladder will return. A key missing from this listing is why a plot_factor() call is failing.
#'
#' @return Invisibly, a tibble with one row per registered key.
plot_report_design <- function() {
  keys_ <- ls(envir = .plot_levels)
  cli::cli_h2("Figure design layer")
  cli::cli_alert_info("Font {.val {(.plot_font)}} at {(.plot_base)}pt; width fixed, height from the ladder.")

  out_ <- if (length(keys_) == 0L) {
    tibble::tibble(Key = character(), NLevels = integer(), Coloured = logical(), Levels = character())
  } else {
    purrr::map(keys_, function(.k) {
      e_ <- plot_entry(.k)
      tibble::tibble(
        Key      = .k,
        NLevels  = length(e_$Levels),
        Coloured = !is.null(e_$Colours),
        Levels   = paste(e_$Short, collapse = ", ")
      )
    }) |>
      purrr::list_rbind()
  }

  if (nrow(out_) == 0L) {
    cli::cli_alert_warning("No vocabularies registered. Every figure will order its own categories.")
  } else {
    cli::cli_verbatim(tbl_fmt(dplyr::mutate(out_, Levels = stringi::stri_sub(.data$Levels, 1L, 70L))))
  }

  cli::cli_alert_info(
    "Ladder: {paste(names(.plot_ladder), unname(.plot_ladder), sep = ' = ', collapse = '; ')}."
  )
  cli::cli_alert_info("A key absent above is why plot_factor() aborts, not a data problem.")
  invisible(out_)
}
