# 04B4-Rules-MONEY: amounts ---------------------------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# Turns parsed amounts into document-level money variables. Function prefix is `mny_`.
#
# THE LEAST OPINIONATED OF THE FIVE, AND IT SHOULD BE
# There is no external fact to anchor an amount against and no taxonomy that says what a contract's
# value is, so this document does not invent one. It reports what the extractors found, filters the
# two things that are demonstrably not amounts, and puts the numbers beside the contract type so a
# reader can see whether the ordering makes sense.
#
# WHOLE DOCUMENT, NO REGION RULE. Money is genuinely distributed: no engine exceeds a positional
# contrast of 1.85 between the ends and the middle, against 22.6% and 25.2% in the two ends for
# organisations. There is nothing for a window to separate.
#
# CURRENCIES ARE NEVER POOLED. A maximum across currencies is not a quantity. Currency is populated
# on 100% of both money engines' rows -- 42,937 of 42,937 -- so the split costs nothing.
#
# THE REPETITION RATIO IS WHY SUM SURVIVES. A contract restating the same fifty million across five
# clauses sums to two hundred and fifty million, and no filter can tell a restatement from a second
# obligation. So distinct amounts over total spans is reported beside the sum: it says how much of a
# class's total is repetition, which is the caveat the sum needs rather than an argument for dropping
# it.
#
# TWO FILTERS, BOTH ON THINGS THAT ARE NOT AMOUNTS. A zero, and a figure sitting next to "par value"
# -- the store's two commonest money spans are "$ 0" and "$ 0.00", which is boilerplate about share
# denomination rather than anything a party owes. Both are swept, and the unfiltered row stays in
# every table.
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
  .key    = "MoneyBlock",
  .levels = c("USD", "non-USD"),
  .short  = c("USD", "non-USD")
)

plot_register_levels(
  .key    = "MoneyFilter",
  .levels = c("none", "drop zero", "drop zero and par"),
  .short  = c("none", "no zero", "no zero/par")
)


# 2. What is not an amount ---------------------------------------------------------------------------------------------
# Read BEFORE the span, because the phrase precedes the figure: "par value $0.0001 per share",
# "stated value of $0.01". Nothing here is a judgement about whether a figure matters; both families
# are figures that are not amounts anybody owes.

.mny_par_cue <- c("PAR VALUE", "STATED VALUE", "NO PAR", "PAR VALUE OF", "LIQUIDATION PREFERENCE OF")


#' Build one money specification
#'
#' @param .filter Character. "none", "zero" or "par". ZERO drops amounts of exactly zero. PAR also
#'   drops a figure preceded by par-value language.
#' @param .cue_win Integer. Characters read before a span for the par-value cue.
#' @param .label Character or NULL. Overrides the generated label.
#' @return A named list carrying the specification.
mny_spec <- function(.filter = "par", .cue_win = 60L, .label = NULL) {
  if (FALSE) {
    .filter  <- "par"
    .cue_win <- 60L
    .label   <- NULL
  }

  if (!.filter %in% c("none", "zero", "par")) {
    cli::cli_abort("{.arg .filter} must be none, zero or par.")
  }

  lab_ <- if (!is.null(.label)) {
    .label
  } else {
    c(none = "none", zero = "drop zero", par = "drop zero and par")[[.filter]]
  }

  list(Filter = .filter, CueWin = as.integer(.cue_win), Label = lab_)
}


# 3. Input ---------------------------------------------------------------------------------------------------------------

#' Load the money spans from both engines, with the par-value context
#'
#' Amount and Currency arrive resolved from the store. The context is read here rather than in the
#' filter, because it does not depend on any specification and reading the text twice would cost more
#' than carrying sixty characters per span.
#'
#' @param .db_path 04A's candidate store.
#' @param .lens Tibble from ent_doc_lens().
#' @param .path_text 04A's canonical text parquet.
#' @param .combos Named character vector of engine tokens.
#' @param .win Integer. Characters read before a span.
#' @param .quiet Logical. Suppress the count messages.
#' @return Tibble: one row per span, with Combo, Amount, Currency, Block and IsPar.
mny_load <- function(.db_path, .lens, .path_text, .combos, .win = 60L, .quiet = FALSE) {
  if (FALSE) {
    .db_path   <- .lP$Input$Store
    .lens      <- tab_lens
    .path_text <- .lP$Input$Text
    .combos    <- .lP$Params$Combos
    .win       <- 60L
    .quiet     <- FALSE
  }

  raw_ <- purrr::imap(.combos, function(.combo, .engine) {
    ent_load_label(
      .db_path = .db_path, .lens = .lens, .label = "money", .combo = .combo,
      .extras = c("Amount", "Currency"), .quiet = .quiet
    ) |>
      dplyr::mutate(Combo = .engine, .before = 1L)
  }) |>
    purrr::list_rbind()

  txt_ <- arrow::read_parquet(.path_text)
  src_ <- txt_$TextRaw[match(raw_$DocID, txt_$DocID)]

  hit_ <- function(.txt, .terms) {
    Reduce(`|`, lapply(.terms, function(.t) stringi::stri_detect_fixed(.txt, .t)))
  }

  raw_ |>
    dplyr::mutate(
      Before = stringi::stri_trans_toupper(
        stringi::stri_replace_all_regex(
          stringi::stri_sub(src_, from = pmax(1L, .data$Start + 1L - .win), to = .data$Start),
          "\\s+", " "
        )
      ),
      IsPar    = hit_(.data$Before, .mny_par_cue),
      Parsed   = !is.na(.data$Amount) & !is.na(.data$Currency),
      Block    = dplyr::case_when(
        is.na(.data$Currency)     ~ NA_character_,
        .data$Currency == "USD"   ~ "USD",
        TRUE                      ~ "non-USD"
      ),
      IsZero   = !is.na(.data$Amount) & .data$Amount == 0
    )
}


# 4. The variables -------------------------------------------------------------------------------------------------------

#' Apply one specification and aggregate per document, engine and currency block
#'
#' SUM, MAX AND MEAN ALL SURVIVE, with the repetition ratio beside them. Max is the only one that is
#' unambiguous -- the largest figure the contract names -- but a reader wanting a total should have
#' one, and NDistinct over NSpans is the caveat it needs.
#'
#' @param .money Tibble from mny_load().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .spec List from mny_spec().
#' @return Tibble: one row per document, engine and currency block.
mny_aggregate <- function(.money, .keys, .spec) {
  if (FALSE) {
    .money <- tab_money
    .keys  <- tab_keys
    .spec  <- .lP$Params$Spec
  }

  src_ <- dplyr::filter(.money, .data$Parsed)

  src_ <- switch(
    .spec$Filter,
    none = src_,
    zero = dplyr::filter(src_, !.data$IsZero),
    par  = dplyr::filter(src_, !.data$IsZero, !.data$IsPar)
  )

  src_ |>
    dplyr::summarise(
      NSpans    = dplyr::n(),
      NDistinct = dplyr::n_distinct(.data$Amount),
      MoneyMax  = max(.data$Amount),
      MoneySum  = sum(.data$Amount),
      MoneyMean = mean(.data$Amount),
      MoneyMed  = stats::median(.data$Amount),
      .by = c(DocID, Combo, Block)
    ) |>
    dplyr::mutate(RepeatRatio = .data$NDistinct / .data$NSpans) |>
    dplyr::left_join(dplyr::select(.keys, DocID, Class, AmendType), by = dplyr::join_by(DocID)) |>
    dplyr::relocate(Class, AmendType, .after = DocID)
}


#' The document-level release, USD and non-USD side by side
#'
#' Every document gets a row per engine whether or not it names an amount, so a zero is a zero rather
#' than an absence, and the non-USD columns sit beside the USD ones rather than in a separate table
#' nobody joins.
#'
#' @param .agg Tibble from mny_aggregate().
#' @param .keys Tibble from ent_anchor_keys().
#' @return Tibble: one row per document and engine.
mny_doc_table <- function(.agg, .keys) {
  if (FALSE) {
    .agg  <- tab_agg
    .keys <- tab_keys
  }

  wide_ <- .agg |>
    dplyr::select(DocID, Combo, Block, NSpans, NDistinct, MoneyMax, MoneySum) |>
    tidyr::pivot_wider(
      names_from  = Block,
      values_from = c(NSpans, NDistinct, MoneyMax, MoneySum),
      names_sep   = ""
    )

  tidyr::expand_grid(
    dplyr::distinct(.agg, .data$Combo),
    dplyr::select(.keys, DocID, Class, AmendType)
  ) |>
    dplyr::left_join(wide_, by = dplyr::join_by(DocID, Combo)) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("NSpans"), \(.x) as.integer(dplyr::coalesce(.x, 0L))),
      dplyr::across(dplyr::starts_with("NDistinct"), \(.x) as.integer(dplyr::coalesce(.x, 0L))),
      HasMoney = dplyr::coalesce(.data$NSpansUSD, 0L) +
                 dplyr::coalesce(.data$`NSpansnon-USD`, 0L) > 0L
    )
}


#' Reduce one specification to a comparable row
#'
#' @param .agg Tibble from mny_aggregate().
#' @param .spec List from mny_spec().
#' @param .n_docs Integer. Documents in the sample.
#' @return Tibble: one row per engine and currency block.
mny_row <- function(.agg, .spec, .n_docs) {
  if (FALSE) {
    .agg    <- tab_agg
    .spec   <- .lP$Params$Spec
    .n_docs <- nrow(tab_keys)
  }

  .agg |>
    dplyr::summarise(
      Docs        = dplyr::n(),
      PctOfSample = dplyr::n() / .n_docs,
      Spans       = sum(.data$NSpans),
      MeanSpans   = mean(.data$NSpans),
      RepeatRatio = mean(.data$RepeatRatio),
      MedMax      = stats::median(.data$MoneyMax),
      MedSum      = stats::median(.data$MoneySum),
      .by = c(Combo, Block)
    ) |>
    dplyr::mutate(Filter = .spec$Label, .before = 1L)
}


#' Sweep the filters
#'
#' @param .money Tibble from mny_load().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .specs List of lists from mny_spec().
#' @param .quiet Logical. Suppress the progress bar.
#' @return Tibble: one row per filter, engine and currency block.
mny_sweep <- function(.money, .keys, .specs, .quiet = FALSE) {
  if (FALSE) {
    .money <- tab_money
    .keys  <- tab_keys
    .specs <- .lP$Params$Specs
    .quiet <- FALSE
  }

  n_ <- dplyr::n_distinct(.keys$DocID)

  purrr::map(.specs, function(.s) {
    mny_row(.agg = mny_aggregate(.money = .money, .keys = .keys, .spec = .s), .spec = .s,
            .n_docs = n_)
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


# 5. Report ----------------------------------------------------------------------------------------------------------------

#' What each engine found and how much of it parsed
#' @param .money Tibble from mny_load().
#' @return Invisibly the summary.
mny_report_parse <- function(.money) {
  if (FALSE) .money <- tab_money

  cli::cli_h2("Amounts found and amounts parsed")
  out_ <- .money |>
    dplyr::summarise(
      Spans      = dplyr::n(),
      Docs       = dplyr::n_distinct(.data$DocID),
      PctParsed  = mean(.data$Parsed),
      PctZero    = mean(.data$IsZero, na.rm = TRUE),
      PctPar     = mean(.data$IsPar),
      PctUSD     = mean(.data$Block == "USD", na.rm = TRUE),
      .by = Combo
    )

  out_ |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "Every money span, by engine")

  .money |>
    dplyr::filter(.data$Parsed) |>
    dplyr::summarise(Spans = dplyr::n(), .by = c(Combo, Currency)) |>
    dplyr::arrange(dplyr::desc(.data$Spans)) |>
    tbl_say(.title = "Currencies", .n = 12L)

  cli::cli_alert_info(
    "PctZero and PctPar are the two families that are figures rather than amounts. The store's two \\
     commonest money spans are \"$ 0\" and \"$ 0.00\", which is boilerplate about share denomination \\
     -- nothing a party owes -- and both are swept below rather than removed silently."
  )
  invisible(out_)
}


#' The filter sweep
#' @param .tab Tibble from mny_sweep().
#' @return Invisibly .tab.
mny_report_sweep <- function(.tab) {
  if (FALSE) .tab <- tab_sweep

  cli::cli_h2("Filter sweep")
  .tab |>
    dplyr::mutate(
      PctOfSample = tbl_pct(.data$PctOfSample),
      RepeatRatio = round(.data$RepeatRatio, 3),
      MeanSpans   = round(.data$MeanSpans, 1),
      dplyr::across(c(MedMax, MedSum), \(.x) round(.x, 0))
    ) |>
    tbl_say(.title = "One row per filter, engine and currency block", .n = 30L)

  cli::cli_alert_info(
    "REPEATRATIO is distinct amounts over total spans, and it is the caveat the sum needs: a contract \\
     restating the same figure five times sums to five times it, and no filter can tell a \\
     restatement from a second obligation. A ratio near one means the total is a total; near a fifth \\
     means it is mostly the same number counted again."
  )
  invisible(.tab)
}


#' The released variables by contract type
#' @param .agg Tibble from mny_aggregate().
#' @param .combo Character. Which engine the table covers.
#' @param .block Character. Which currency block.
#' @return Invisibly the summary.
mny_report_class <- function(.agg, .combo = "moneyregex", .block = "USD") {
  if (FALSE) {
    .agg   <- tab_agg
    .combo <- "moneyregex"
    .block <- "USD"
  }

  cli::cli_h2(paste0("Amounts by contract type -- ", .combo, ", ", .block))
  src_ <- dplyr::filter(.agg, .data$Combo == .combo, .data$Block == .block)

  f_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs        = dplyr::n(),
      MeanSpans   = mean(.data$NSpans),
      RepeatRatio = mean(.data$RepeatRatio),
      MedMax      = stats::median(.data$MoneyMax),
      MedSum      = stats::median(.data$MoneySum),
      MedMed      = stats::median(.data$MoneyMed)
    )
  }

  ent_bind_all(.tab = f_(dplyr::group_by(src_, Class)), .fun = f_, .src = src_) |>
    dplyr::mutate(
      MeanSpans   = round(.data$MeanSpans, 1),
      RepeatRatio = round(.data$RepeatRatio, 3),
      dplyr::across(c(MedMax, MedSum, MedMed), \(.x) round(.x, 0))
    ) |>
    tbl_say(.title = "One row per type, and one for the sample")

  cli::cli_alert_info(
    "THE SANITY TEST IS THE ORDERING, not the level. Credit agreements and mergers should carry the \\
     largest maxima and compensation agreements the smallest; if they do not, something upstream is \\
     wrong and no filter here will fix it."
  )
  invisible(src_)
}


#' Every MONEY report in order
#'
#' @param .money Tibble from mny_load().
#' @param .sweep Tibble from mny_sweep().
#' @param .agg Tibble from mny_aggregate().
#' @param .combo Character. Which engine the class table covers.
#' @return Invisibly NULL.
mny_report_all <- function(.money, .sweep, .agg, .combo = "moneyregex") {
  if (FALSE) {
    .money <- tab_money
    .sweep <- tab_sweep
    .agg   <- tab_agg
    .combo <- "moneyregex"
  }

  mny_report_parse(.money = .money)
  mny_report_sweep(.tab = .sweep)
  mny_report_class(.agg = .agg, .combo = .combo, .block = "USD")
  mny_report_class(.agg = .agg, .combo = .combo, .block = "non-USD")
  invisible(NULL)
}


# 6. Figures ---------------------------------------------------------------------------------------------------------------

#' Distribution of the largest amount per contract, by type
#' @param .agg Tibble from mny_aggregate().
#' @param .combo Character. Which engine.
#' @param .block Character. Which currency block.
#' @return A ggplot object.
mny_plot_max <- function(.agg, .combo = "moneyregex", .block = "USD") {
  if (FALSE) {
    .agg   <- tab_agg
    .combo <- "moneyregex"
    .block <- "USD"
  }

  .agg |>
    dplyr::filter(.data$Combo == .combo, .data$Block == .block, .data$MoneyMax > 0) |>
    dplyr::summarise(MedMax = stats::median(.data$MoneyMax), .by = Class) |>
    plot_bar_ranked(
      .cat      = "Class",
      .val      = "MedMax",
      .key      = "ClassDetailed",
      .short    = FALSE,
      .label    = TRUE,
      .accuracy = 1,
      .pct      = FALSE
    )
}


#' How much of a contract's money is the same figure repeated
#' @param .agg Tibble from mny_aggregate().
#' @param .combo Character. Which engine.
#' @return A ggplot object.
mny_plot_repeat <- function(.agg, .combo = "moneyregex") {
  if (FALSE) {
    .agg   <- tab_agg
    .combo <- "moneyregex"
  }

  .agg |>
    dplyr::filter(.data$Combo == .combo, .data$NSpans >= 3L) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$RepeatRatio, colour = .data$Block)) +
    ggplot2::stat_ecdf(linewidth = 0.7) +
    plot_scale_colour_cat(name = NULL) +
    plot_scale_x_pct(.expand = c(0, 0)) +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}
