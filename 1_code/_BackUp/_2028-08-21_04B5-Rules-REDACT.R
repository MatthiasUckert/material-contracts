# 04B5-Rules-REDACT: what was withheld ---------------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# Counts redaction markers and asks what each one replaced. Function prefix is `red_`.
#
# THE TWO MARKER KINDS ARE NEVER POOLED
# A bracketed marker -- [***], [REDACTED] -- follows the drafting convention the published
# classification is built on. A bare run of asterisks does not, and one sample document carries
# 14,689 of them: 24% of every redaction span in the sample, from a single filing. Pooling the two
# means one document decides a class contrast, which is exactly what happened on the first look at
# this label, where Employment: Compensation showed a positional contrast of 13.29 against 1 to 5
# everywhere else.
#
# So every count is reported by kind, and every class contrast is computed FOUR WAYS -- span-weighted
# and document-weighted, with bare markers and without. A finding that survives all four is a
# finding; one that appears in only the span-weighted pooled cell is a flood document.
#
# THE RATIO IS THE COMPARABLE QUANTITY
# Contracts run from roughly 4,200 to 17,400 words by class, so a count of markers is partly a count
# of words. Markers per thousand words is what makes an industry or a class comparison mean anything,
# and it is what referee 2 asked for when they wanted redaction intensity by firm-quarter.
#
# WHAT A MARKER REPLACED
# A marker beside a currency symbol hid an amount; beside an organisation, a party name; beside a
# date, a date. That is a proximity question of exactly the kind the geography rule answers, and it
# is the only thing in this document that goes beyond counting. It is reported and not released as a
# variable, because 82% of markers had no labelled entity within forty characters on the earlier
# check and a variable that is missing four times in five is a diagnostic rather than a measure.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_text <- .lP$Input$Text
  .db_path   <- .lP$Input$Store
}


# 1. Vocabulary --------------------------------------------------------------------------------------------------------

plot_register_levels(
  .key    = "RedactKind",
  .levels = c("bracketed", "bare"),
  .short  = c("bracketed", "bare")
)

plot_register_levels(
  .key    = "RedactWeight",
  .levels = c("spans, with bare", "spans, no bare", "documents, with bare", "documents, no bare"),
  .short  = c("span+bare", "span", "doc+bare", "doc")
)

plot_register_levels(
  .key    = "RedactNear",
  .levels = c("money", "org", "date", "gpe", "nothing"),
  .short  = c("money", "org", "date", "gpe", "none")
)


# 2. Input -------------------------------------------------------------------------------------------------------------

#' Load the redaction markers and split them by kind
#'
#' The kind comes from LabelRaw, which is what the extractor called the marker before the store
#' normalised it. Anything the extractor named with "bare" is a run of asterisks; everything else
#' follows the bracketed convention.
#'
#' @param .db_path 04A's candidate store.
#' @param .lens Tibble from ent_doc_lens().
#' @param .combo Character. Engine token.
#' @param .quiet Logical. Suppress the count message.
#' @return Tibble: one row per marker, with Kind.
red_load <- function(.db_path, .lens, .combo = "paper:redaction-v1", .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Input$Store
    .lens    <- tab_lens
    .combo   <- "paper:redaction-v1"
    .quiet   <- FALSE
  }

  ent_load_label(
    .db_path = .db_path, .lens = .lens, .label = "redact", .combo = .combo,
    .extras = character(), .quiet = .quiet
  ) |>
    dplyr::mutate(
      Kind = dplyr::if_else(
        stringi::stri_detect_fixed(stringi::stri_trans_toupper(dplyr::coalesce(.data$LabelRaw, "")),
                                   "BARE"),
        "bare", "bracketed"
      )
    )
}


#' Words per document, for the ratio
#'
#' A count of markers is partly a count of words when contracts run from four thousand to seventeen
#' thousand of them by class. This is the denominator that makes the count comparable.
#'
#' @param .path_text 04A's canonical text parquet.
#' @return Tibble: DocID, NWords.
red_words <- function(.path_text) {
  if (FALSE) .path_text <- .lP$Input$Text

  arrow::read_parquet(.path_text) |>
    dplyr::transmute(DocID, NWords = stringi::stri_count_regex(.data$TextRaw, "\\S+"))
}


# 3. What a marker replaced ---------------------------------------------------------------------------------------------

#' The nearest labelled entity to each marker
#'
#' A marker beside a currency symbol hid an amount; beside an organisation, a party name. The nearest
#' span of any other label within reach is the cheapest statement of that, and it is the same
#' proximity instrument the geography rule uses.
#'
#' REPORTED, NOT RELEASED. On the earlier check 82% of markers had no labelled entity within forty
#' characters, and a variable missing four times in five is a diagnostic rather than a measure. What
#' it can say is which labels sit near markers at all, which is the first thing to know before
#' anyone builds on it.
#'
#' @param .marks Tibble from red_load().
#' @param .db_path 04A's candidate store.
#' @param .lens Tibble from ent_doc_lens().
#' @param .labels Named character vector: label table to engine token.
#' @param .reach Integer. Furthest a labelled span may be and still be the marker's neighbour.
#' @param .quiet Logical. Suppress the loader messages.
#' @return .marks with NearLabel and NearDist added.
red_neighbour <- function(.marks, .db_path, .lens, .labels, .reach = 40L, .quiet = TRUE) {
  if (FALSE) {
    .marks   <- tab_marks
    .db_path <- .lP$Input$Store
    .lens    <- tab_lens
    .labels  <- .lP$Params$Neighbours
    .reach   <- 40L
    .quiet   <- TRUE
  }

  others_ <- purrr::imap(.labels, function(.combo, .label) {
    ent_load_label(
      .db_path = .db_path, .lens = .lens, .label = .label, .combo = .combo,
      .extras = character(), .quiet = .quiet
    ) |>
      dplyr::transmute(DocID, OStart = .data$Start, OStop = .data$Stop, NearLabel = .label)
  }) |>
    purrr::list_rbind()

  # Nearest by either edge, within the reach. One row per marker by construction: the join is
  # filtered to the reach first, so the slice runs over a handful of candidates rather than a
  # document's worth.
  near_ <- .marks |>
    dplyr::select(DocID, Start, Stop) |>
    dplyr::inner_join(others_, by = dplyr::join_by(DocID), relationship = "many-to-many") |>
    dplyr::mutate(
      Dist = dplyr::case_when(
        .data$OStart > .data$Stop  ~ .data$OStart - .data$Stop,
        .data$OStop  < .data$Start ~ .data$Start - .data$OStop,
        TRUE                       ~ 0L
      )
    ) |>
    dplyr::filter(.data$Dist <= .reach) |>
    dplyr::slice_min(order_by = .data$Dist, n = 1L, by = c(DocID, Start), with_ties = FALSE) |>
    dplyr::select(DocID, Start, NearLabel, NearDist = Dist)

  .marks |>
    dplyr::left_join(near_, by = dplyr::join_by(DocID, Start)) |>
    dplyr::mutate(NearLabel = dplyr::coalesce(.data$NearLabel, "nothing"))
}


# 4. The variables -------------------------------------------------------------------------------------------------------

#' Redaction counts per document
#'
#' The two kinds are separate columns, never a total. RedactRatio is markers per thousand words and
#' it uses BRACKETED markers only, because a bare run of asterisks is not the drafting convention the
#' variable is meant to measure -- the bare count sits beside it so a reader can pool them if they
#' disagree.
#'
#' @param .marks Tibble from red_load().
#' @param .words Tibble from red_words().
#' @param .keys Tibble from ent_anchor_keys().
#' @return Tibble: one row per document.
red_counts <- function(.marks, .words, .keys) {
  if (FALSE) {
    .marks <- tab_marks
    .words <- tab_words
    .keys  <- tab_keys
  }

  cnt_ <- .marks |>
    dplyr::summarise(
      NBracketed = sum(.data$Kind == "bracketed"),
      NBare      = sum(.data$Kind == "bare"),
      .by = DocID
    )

  .keys |>
    dplyr::select(DocID, Class, AmendType) |>
    dplyr::left_join(.words, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(cnt_,   by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      dplyr::across(c(NBracketed, NBare), \(.x) as.integer(dplyr::coalesce(.x, 0L))),
      NRedact      = .data$NBracketed + .data$NBare,
      HasRedact    = .data$NBracketed > 0L,
      HasBare      = .data$NBare > 0L,
      RedactRatio  = 1000 * .data$NBracketed / pmax(.data$NWords, 1L),
      RedactRatioAll = 1000 * .data$NRedact / pmax(.data$NWords, 1L)
    )
}


#' The class contrast, computed four ways
#'
#' SPAN-WEIGHTED AND DOCUMENT-WEIGHTED, WITH BARE MARKERS AND WITHOUT. The first look at this label
#' reported Employment: Compensation at 13.29 against 1 to 5 everywhere else, span-weighted and
#' pooled -- and one document in the sample carries 14,689 bare markers, a quarter of every redaction
#' span there is. A finding that survives all four cells is a finding; one that appears in the
#' span-weighted pooled cell alone is that document.
#'
#' @param .marks Tibble from red_load().
#' @param .counts Tibble from red_counts().
#' @return Tibble: one row per class per weighting.
red_contrast <- function(.marks, .counts) {
  if (FALSE) {
    .marks  <- tab_marks
    .counts <- tab_counts
  }

  cls_ <- dplyr::select(.counts, DocID, Class)

  spans_ <- .marks |>
    dplyr::left_join(cls_, by = dplyr::join_by(DocID)) |>
    dplyr::summarise(
      `spans, with bare` = dplyr::n(),
      `spans, no bare`   = sum(.data$Kind == "bracketed"),
      .by = Class
    )

  docs_ <- .counts |>
    dplyr::summarise(
      `documents, with bare` = mean(.data$RedactRatioAll),
      `documents, no bare`   = mean(.data$RedactRatio),
      .by = Class
    )

  dplyr::left_join(spans_, docs_, by = dplyr::join_by(Class)) |>
    tidyr::pivot_longer(cols = -Class, names_to = "Weight", values_to = "Value") |>
    dplyr::mutate(
      Share = .data$Value / sum(.data$Value),
      .by = Weight
    ) |>
    dplyr::arrange(plot_factor(.data$Weight, .key = "RedactWeight"),
                   dplyr::desc(.data$Share))
}


# 5. Report ------------------------------------------------------------------------------------------------------------

#' What was found, and the flood
#' @param .marks Tibble from red_load().
#' @param .counts Tibble from red_counts().
#' @return Invisibly the summary.
red_report_found <- function(.marks, .counts) {
  if (FALSE) {
    .marks  <- tab_marks
    .counts <- tab_counts
  }

  cli::cli_h2("Markers found")
  out_ <- .marks |>
    dplyr::summarise(Spans = dplyr::n(), Docs = dplyr::n_distinct(.data$DocID),
                     MaxPerDoc = max(table(.data$DocID)), .by = Kind) |>
    dplyr::arrange(plot_factor(.data$Kind, .key = "RedactKind"))

  tbl_say(.tab = out_, .title = "By marker kind")

  tibble::tibble(
    Item = c("Documents in the sample",
             "Documents with a bracketed marker",
             "Documents with a bare marker",
             "Markers in the single largest document",
             "...as a share of every marker in the sample"),
    N = c(nrow(.counts),
          sum(.counts$HasRedact),
          sum(.counts$HasBare),
          max(.counts$NRedact),
          NA_integer_)
  ) |>
    dplyr::mutate(
      Share = c(tbl_pct(1), tbl_pct(mean(.counts$HasRedact)), tbl_pct(mean(.counts$HasBare)),
                "", tbl_pct(max(.counts$NRedact) / sum(.counts$NRedact)))
    ) |>
    tbl_say(.title = "The base rate, and the flood")

  cli::cli_alert_info(
    "THE LAST ROW IS WHY THE TWO KINDS NEVER POOL. One filing carries a quarter of every redaction \\
     span in the sample. A class contrast computed span-weighted over pooled kinds is that document's \\
     contrast, not the class's, and the four-way table below is what separates them."
  )
  invisible(out_)
}


#' The class contrast, four ways
#' @param .tab Tibble from red_contrast().
#' @return Invisibly .tab.
red_report_contrast <- function(.tab) {
  if (FALSE) .tab <- tab_contrast

  cli::cli_h2("Redaction by contract type, four weightings")
  .tab |>
    dplyr::mutate(Value = round(.data$Value, 3), Share = tbl_pct(.data$Share)) |>
    tidyr::pivot_wider(names_from = Weight, values_from = c(Value, Share), names_sep = " ") |>
    tbl_say(.title = "A finding that survives all four is a finding")

  cli::cli_alert_info(
    "SPAN, WITH BARE is the weighting that produced the earlier Employment: Compensation figure of \\
     13.29. Read it against DOCUMENTS, NO BARE, which weights every contract equally and counts only \\
     the drafting convention: where the two disagree, one document is doing the talking."
  )
  invisible(.tab)
}


#' What sat beside a marker
#' @param .marks Tibble from red_neighbour().
#' @param .reach Integer. The reach used, for the prose.
#' @return Invisibly the summary.
red_report_neighbour <- function(.marks, .reach = 40L) {
  if (FALSE) {
    .marks <- tab_near
    .reach <- 40L
  }

  cli::cli_h2("What each marker replaced")
  out_ <- .marks |>
    dplyr::summarise(Markers = dplyr::n(),
                     MedDist = stats::median(.data$NearDist, na.rm = TRUE),
                     .by = c(Kind, NearLabel)) |>
    dplyr::mutate(Share = .data$Markers / sum(.data$Markers), .by = Kind) |>
    dplyr::arrange(plot_factor(.data$Kind, .key = "RedactKind"), dplyr::desc(.data$Markers))

  out_ |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = paste0("Nearest labelled span within ", .reach, " characters"), .n = 20L)

  cli::cli_alert_info(
    "REPORTED, NOT RELEASED. On the earlier check 82% of markers had nothing labelled within forty \\
     characters, and a variable missing four times in five is a diagnostic rather than a measure. \\
     What this table can say is which labels sit near markers at all -- and a marker beside a \\
     currency symbol hid an amount, which is the one case where the redaction and the money document \\
     have something to say to each other."
  )
  invisible(out_)
}


#' The base rate against the confidential-treatment match
#' @param .counts Tibble from red_counts().
#' @param .cto Tibble with DocID and a logical, or NULL where the match is not available.
#' @return Invisibly the summary.
red_report_cto <- function(.counts, .cto = NULL) {
  if (FALSE) {
    .counts <- tab_counts
    .cto    <- NULL
  }

  cli::cli_h2("Against the confidential-treatment record")

  if (is.null(.cto)) {
    cli::cli_alert_warning(
      "No confidential-treatment table supplied, so the base rate stands unchecked. \\
       {tbl_pct(mean(.counts$HasRedact))} of documents carry a bracketed marker; if the filings \\
       record a materially different rate, one of the two is wrong and it matters for the intensity \\
       figures a referee asked for."
    )
    return(invisible(NULL))
  }

  out_ <- .counts |>
    dplyr::left_join(.cto, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(HasCto = dplyr::coalesce(.data$HasCto, FALSE)) |>
    dplyr::summarise(Docs = dplyr::n(), .by = c(HasRedact, HasCto)) |>
    dplyr::mutate(Share = .data$Docs / sum(.data$Docs))

  out_ |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "Markers found against confidential treatment recorded")
  invisible(out_)
}


#' Every REDACT report in order
#'
#' @param .marks Tibble from red_load().
#' @param .near Tibble from red_neighbour().
#' @param .counts Tibble from red_counts().
#' @param .contrast Tibble from red_contrast().
#' @param .reach Integer. The neighbour reach, for the prose.
#' @return Invisibly NULL.
red_report_all <- function(.marks, .near, .counts, .contrast, .reach = 40L) {
  if (FALSE) {
    .marks    <- tab_marks
    .near     <- tab_near
    .counts   <- tab_counts
    .contrast <- tab_contrast
    .reach    <- 40L
  }

  red_report_found(.marks = .marks, .counts = .counts)
  red_report_contrast(.tab = .contrast)
  red_report_neighbour(.marks = .near, .reach = .reach)
  red_report_cto(.counts = .counts, .cto = NULL)
  invisible(NULL)
}


# 6. Figures -------------------------------------------------------------------------------------------------------------

#' Redaction intensity by contract type, document-weighted
#' @param .counts Tibble from red_counts().
#' @return A ggplot object.
red_plot_ratio <- function(.counts) {
  if (FALSE) .counts <- tab_counts

  .counts |>
    dplyr::summarise(Ratio = mean(.data$RedactRatio), .by = Class) |>
    plot_bar_ranked(
      .cat      = "Class",
      .val      = "Ratio",
      .key      = "ClassDetailed",
      .short    = FALSE,
      .label    = TRUE,
      .accuracy = 0.01,
      .pct      = FALSE
    )
}


#' Where markers sit in the document
#' @param .marks Tibble from red_load().
#' @param .bins Integer. Bins across relative position.
#' @return A ggplot object.
red_plot_density <- function(.marks, .bins = 50L) {
  if (FALSE) {
    .marks <- tab_marks
    .bins  <- 50L
  }

  .marks |>
    dplyr::group_split(.data$Kind) |>
    purrr::map(function(.d) {
      ent_density(.spans = .d, .bins = .bins) |>
        dplyr::mutate(Kind = dplyr::first(.d$Kind))
    }) |>
    purrr::list_rbind() |>
    dplyr::filter(.data$Weight == "documents") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Pos, y = .data$Share, colour = .data$Kind)) +
    ggplot2::geom_line(linewidth = 0.7) +
    plot_scale_colour_cat(name = NULL) +
    plot_scale_x_pct(.expand = c(0, 0)) +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}
