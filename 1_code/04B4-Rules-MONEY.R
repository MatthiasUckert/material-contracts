# 04B4-Rules-MONEY: what a contract is worth -----------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# moneyregex proposes figures with a currency. Two of them are not amounts anybody owes -- a zero and
# a figure with par-value language beside it -- and a third is a price the filer removed. This says
# which is which and leaves the arithmetic to a query.
#
# ONE FILE COMES OUT AND IT IS LONG
# money_spans.parquet is one row per figure, with every filter as a COLUMN rather than as a filter.
# mny_collapse() aggregates it per contract and currency block, and the export calls that; this
# document scores it, reports it and checks its shape.
#
# NOTHING IS DISCARDED, WHICH IS WHY THIS ENTITY WAS THE EASIEST TO MOVE. The filters were already
# computed as flags and then applied one step later, so the long file is what the chain already held
# in memory. Parsed, Withheld, IsZero, IsPar and ParSide are all columns of it.
#
# THE NAIVE LADDER IS THREE FILTERS OVER ONE FILE
#   Parsed                      every figure this document can compute with
#   Parsed & !IsZero            after the zero filter
#   Parsed & !IsZero & !IsPar   the rule
# So NaiveMaxUSD, NaiveSumUSD, NDroppedZero and NDroppedPar stop being stored: each is the same
# aggregate with one filter switched off, and a reader who disagrees with either takes the rung above
# it and recomputes from the file.
#
# A WITHHELD FIGURE IS A THIRD OUTCOME, NOT A PARSE FAILURE. Three moneyregex patterns match a
# currency with no number -- the filer removed the price. They are counted and never summed, because
# an arithmetic including them would have to invent a value, and Withheld keys on the PATTERN NAME
# rather than on a null amount so that a removed price and an unreadable one stay distinguishable.
#
# ParCue SHIPS BESIDE IsPar, for the reason CueHit ships beside HasEndCue in 04B3: storing only the
# boolean would freeze the cue list into the file, and storing the term that fired makes testing a
# shorter list a query. The window stays a specification dial and the sweep is what covers it.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .dir_store <- .lP$Input$Store
}


# 1. Vocabulary ----------------------------------------------------------------------------------------------------------


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

plot_register_levels(
  .key    = "MoneyDrop",
  .levels = c("kept", "zero", "par", "withheld", "unparsed"),
  .short  = c("kept", "zero", "par", "withheld", "unparsed")
)


# 2. What is not an amount -----------------------------------------------------------------------------------------------
# READ BOTH SIDES OF THE FIGURE, because a drafter writes the phrase either way round: "par value
# $0.0001 per share" puts it before and "shares of $0.01 par value per share" puts it after. The old
# rule could only see the first, so the second passed the filter every time.
#
# NOTHING HERE IS A JUDGEMENT ABOUT WHETHER A FIGURE MATTERS. Both families are figures that are not
# amounts anybody owes.

.mny_par_cue <- c("PAR VALUE", "STATED VALUE", "NO PAR", "LIQUIDATION PREFERENCE")

#: The three moneyregex patterns that match a figure the filer removed.
#:
#: NAMED RATHER THAN INFERRED FROM A NULL AMOUNT. An amount is also null when a cast fails, and the
#: two mean opposite things: one is a contract withholding a price, the other is this document unable
#: to read one. Keying on the pattern name separates them, and a name that stops being emitted shows
#: up as a column of zeros rather than as silence.
.mny_redact_form <- c("symbol_redact", "symbol_redact_open", "symbol_bare")


# 3. Input ---------------------------------------------------------------------------------------------------------------

#' Build one money specification
#'
#' TWO DIALS, AND THE SECOND IS THE ONE A READER COULD DISAGREE WITH. The window is a real parameter
#' again: it used to sit in the specification while IsPar was computed once at load time, so a sweep
#' over specifications changed the label and nothing else. Both filters now run where the sweep can
#' reach them.
#'
#' @param .filter Character. "none", "zero" or "par". ZERO drops amounts of exactly zero. PAR also
#'   drops a figure with par-value language beside it.
#' @param .cue_win Integer. Characters read EITHER SIDE of a span for the par-value cue, out of the
#'   160 the store holds. Narrowed here rather than at extraction, so refining the cue list never
#'   means re-extracting.
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

  list(
    Filter = .filter,
    CueWin = as.integer(.cue_win),
    Label  = if (!is.null(.label)) {
      .label
    } else {
      c(none = "none", zero = "drop zero", par = "drop zero and par")[[.filter]]
    }
  )
}


#' Load the money spans, with the cue columns they arrive with
#'
#' AMOUNT IS CAST FROM TEXT. moneyregex writes it as a string so a contract value crosses the seam
#' intact, and the store keeps the string because its schema is the parquet's. as.numeric() here is
#' the first arithmetic anything does with it, which is where the cast belongs -- and a value that
#' will not cast becomes NA and is counted by the parse report rather than silently dropped.
#'
#' NO FILTER RUNS HERE. Both filters depend on a specification, so applying either at load would put
#' them beyond the reach of the sweep -- which is what the old version did with the par cue, leaving a
#' dial that appeared in every swept label and changed nothing.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .lens Tibble: DocID and DocLen.
#' @param .family Character. The family supplying amounts.
#' @param .quiet Logical. Suppress the count message.
#' @return Tibble: one row per span, with Amount, Currency, Block and the two cue columns.
mny_load <- function(.dir_store, .lens, .family = "matcon", .quiet = FALSE) {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .lens      <- tab_lens
    .family    <- "matcon"
    .quiet     <- FALSE
  }

  ent_load_entity(
    .dir_store = .dir_store,
    .family    = .family,
    .entity    = "MONEY",
    .lens      = .lens,
    .extras    = ent_extras(.family, "MONEY"),   # Amount, Currency, and the two cue columns
    .quiet     = .quiet
  ) |>
    dplyr::mutate(
      AmountRaw = as.character(.data$Amount),
      Amount    = suppressWarnings(as.numeric(.data$AmountRaw)),
      # THREE OUTCOMES, NOT TWO. PARSED is a figure this document can compute with. WITHHELD is a
      # figure the CONTRACT removed -- a currency with no number, which moneyregex emits on purpose.
      # Everything else failed to cast, and calling that third thing by the same name as the second
      # is what made the withheld amounts disappear.
      Withheld  = .data$LabelRaw %in% .mny_redact_form & !is.na(.data$Currency),
      Parsed    = !is.na(.data$Amount) & !is.na(.data$Currency),
      Block     = dplyr::case_when(
        is.na(.data$Currency)   ~ NA_character_,
        .data$Currency == "USD" ~ "USD",
        .default                = "non-USD"
      ),
      IsZero    = !is.na(.data$Amount) & .data$Amount == 0
    )
}


# 4. What is not an amount -----------------------------------------------------------------------------------------------

#' Mark the amounts that sit beside par-value language
#'
#' BOTH SIDES, AND THAT IS THE CHANGE. A drafter writes the phrase either way round -- "par value
#' $0.0001 per share" before the figure and "shares of $0.01 par value per share" after it -- and the
#' rule this replaces cut a window ENDING at the span, so the second form was structurally invisible
#' and passed the filter every time.
#'
#' THE WINDOW IS NARROWED FROM THE STORED ONE. matcon keeps 160 characters either side; this reads the
#' innermost .cue_win of them. stri_sub with a negative from() counts back from the end, which is what
#' makes CueBefore's slice the characters NEAREST the amount rather than the start of the window.
#'
#' @param .money Tibble from mny_load().
#' @param .spec List from mny_spec(). Supplies the window.
#' @return .money with Before, After, ParCue, IsPar and ParSide added.
mny_mark_par <- function(.money, .spec) {
  if (FALSE) {
    .money <- tab_money
    .spec  <- .lP$Params$Spec
  }

  if (!all(c("CueBefore", "CueAfter") %in% names(.money))) {
    cli::cli_abort(c(
      "The MONEY spans carry no cue columns, so the par-value filter cannot run.",
      "i" = "matcon stores them at extraction; a store written before that change has to be rebuilt."
    ))
  }

  hit_ <- function(.txt, .terms) {
    Reduce(`|`, lapply(.terms, function(.t) stringi::stri_detect_fixed(.txt, .t)))
  }

  # WHICH PHRASE, NOT WHETHER A PHRASE. Storing only the boolean would freeze the cue list into the
  # released file; storing the term that fired makes testing a shorter list a query over it. First by
  # list order where several match, which is attribution rather than counting.
  which_ <- function(.before, .after, .terms) {
    out_ <- rep(NA_character_, length(.before))
    for (.t in .terms) {
      m_ <- is.na(out_) &
        (stringi::stri_detect_fixed(.before, .t) | stringi::stri_detect_fixed(.after, .t))
      out_[m_] <- .t
    }
    out_
  }

  norm_ <- function(.x) {
    stringi::stri_trans_toupper(stringi::stri_replace_all_regex(dplyr::coalesce(.x, ""), "\\s+", " "))
  }

  .money |>
    dplyr::mutate(
      Before = norm_(stringi::stri_sub(.data$CueBefore, from = -.spec$CueWin)),
      After  = norm_(stringi::stri_sub(.data$CueAfter,  to   =  .spec$CueWin)),
      ParCue = which_(.data$Before, .data$After, .mny_par_cue),
      IsPar  = !is.na(.data$ParCue),
      ParSide = dplyr::case_when(
        !.data$IsPar                          ~ "none",
        hit_(.data$Before, .mny_par_cue) &
          hit_(.data$After, .mny_par_cue)     ~ "both",
        hit_(.data$Before, .mny_par_cue)      ~ "before",
        .default                              = "after"
      )
    )
}


#' A statistic, or missing where there is nothing to compute it over
#'
#' max(numeric(0)) is -Inf with a warning, which renders as a number and is not one. A contract whose
#' every amount the filters removed has no maximum, and the honest rendering of that is a missing
#' value rather than negative infinity.
#'
#' @param .x Numeric vector, possibly empty.
#' @param .f Function taking a numeric vector.
#' @return The statistic, or NA_real_ where .x is empty.
.mny_stat_or_na <- function(.x, .f) {
  if (FALSE) {
    .x <- numeric(0)
    .f <- max
  }
  if (length(.x) == 0L) NA_real_ else .f(.x)
}


# 5. The release ---------------------------------------------------------------------------------------------------------

#' The release: one row per figure, every filter a column
#'
#' NOTHING IS DROPPED HERE. A zero, a par-value figure, a withheld price and a span that would not
#' cast are all in the file, each marked by the column that describes it. That is what makes the
#' naive ladder a filter rather than a second computation, and what lets a reader who disagrees with
#' either filter recompute the aggregate rather than take it.
#'
#' MoneyDrop IS THE FOUR OUTCOMES IN ONE COLUMN, so the commonest cut needs no boolean algebra: kept,
#' zero, par, unparsed. Withheld is separate and deliberately so -- a removed price is not a dropped
#' amount, it is a measurement about the filing.
#'
#' @param .money Tibble from mny_mark_par().
#' @return Tibble: one row per figure.
mny_release_spans <- function(.money) {
  if (FALSE) .money <- tab_marked

  .money |>
    dplyr::transmute(
      .data$DocID,
      MoneyStart = as.integer(.data$Start),
      MoneyStop  = as.integer(.data$Stop),
      MoneyText  = .data$Span,
      Pattern    = .data$LabelRaw,
      .data$AmountRaw,
      .data$Amount,
      .data$Currency,
      .data$Block,
      .data$Parsed,
      .data$Withheld,
      .data$IsZero,
      .data$IsPar,
      .data$ParSide,
      .data$ParCue,
      # WITHHELD OUTRANKS UNPARSED, and the order is the whole of it. A withheld figure has no number
      # to cast, so Parsed is FALSE and it would otherwise land in "unparsed" -- which says this
      # document could not read a figure, when the truth is that the filer removed one. Two opposite
      # findings under one label, and the count was identical to the withheld count because every
      # unparsed row WAS a withheld row.
      MoneyDrop  = dplyr::case_when(
        .data$Withheld ~ "withheld",
        !.data$Parsed  ~ "unparsed",
        .data$IsZero   ~ "zero",
        .data$IsPar    ~ "par",
        .default       = "kept"
      )
    ) |>
    dplyr::arrange(.data$DocID, .data$MoneyStart)
}


#' What every column of the money file means
#' @param .tab Tibble from mny_release_spans().
#' @return Tibble: Column, Grain, Meaning.
mny_dictionary_spans <- function(.tab) {
  if (FALSE) .tab <- tab_release

  dict_ <- tibble::tribble(
    ~Column,      ~Grain,     ~Meaning,
    "DocID",      "document", "the contract",
    "MoneyStart", "figure",   "offset of the figure, into 04A's canonical text",
    "MoneyStop",  "figure",   "offset one past its last character",
    "MoneyText",  "figure",   "the surface form, raw: it slices from the two offsets exactly",
    "Pattern",    "figure",   "which moneyregex pattern matched",
    "AmountRaw",  "figure",   "the number as the extractor wrote it, before casting",
    "Amount",     "figure",   "that number as a double; null where the cast failed or none was there",
    "Currency",   "figure",   "the currency the extractor resolved",
    "Block",      "figure",   "USD or non-USD; the aggregate never mixes them",
    "Parsed",     "figure",   "a figure this document can compute with",
    "Withheld",   "figure",   "a currency with no number -- the filer removed the price",
    "IsZero",     "figure",   "exactly zero; the two commonest money spans are $0 and $0.00",
    "IsPar",      "figure",   "par-value language sits within the window on either side",
    "ParSide",    "figure",   "before, after, both or none -- which side the cue was on",
    "ParCue",     "figure",   "the phrase that fired IsPar, so a shorter list is a query",
    "MoneyDrop",  "figure",   "kept, zero, par, withheld or unparsed -- every outcome, one column"
  )

  undoc_  <- setdiff(names(.tab), dict_$Column)
  unseen_ <- setdiff(dict_$Column, names(.tab))
  say_    <- function(.x) if (length(.x) == 0L) "none" else paste(.x, collapse = ", ")

  if (length(undoc_) > 0L || length(unseen_) > 0L) {
    cli::cli_abort(c(
      "The money dictionary and the released file disagree.",
      "x" = "In the file and undocumented: {say_(undoc_)}.",
      "x" = "Documented and not in the file: {say_(unseen_)}."
    ))
  }

  dict_[match(names(.tab), dict_$Column), ]
}


#' The collapse: one row per contract, from the figure file
#'
#' DEFINED HERE AND WRITTEN NOWHERE. This document scores it, reports it and checks its shape; the
#' export calls it to materialise one row per contract. An aggregate stored as data is an aggregate
#' whose filters cannot be changed without re-releasing, and both of these filters are ones a reader
#' might reasonably reject.
#'
#' THE NAIVE FIGURES ARE THE SAME AGGREGATE WITH ONE FILTER OFF, computed here rather than stored,
#' because the file still holds the rows a stored version would have thrown away.
#'
#' @param .release Tibble or dataset from mny_release_spans().
#' @param .keys Tibble from ent_anchor_keys(). Supplies the population.
#' @param .spec List from mny_spec(). Selects which filters apply.
#' @return Tibble: one row per document in .keys.
mny_collapse <- function(.release, .keys, .spec) {
  if (FALSE) {
    .release <- tab_release
    .keys    <- tab_keys
    .spec    <- .lP$Params$Spec
  }

  # WHICH FILTERS THE SPECIFICATION ASKS FOR. "none" keeps every parsed figure, "zero" drops the
  # zeros, "par" drops both. The rows are still in the file either way.
  keep_ <- .release |>
    dplyr::filter(.data$Parsed) |>
    dplyr::filter(!(.spec$Filter %in% c("zero", "par") & .data$IsZero)) |>
    dplyr::filter(!(.spec$Filter == "par" & .data$IsPar))

  agg_ <- function(.d, .suffix) {
    out_ <- .d |>
      dplyr::summarise(
        N        = dplyr::n(),
        NDistinct = dplyr::n_distinct(.data$Amount),
        Max      = .mny_stat_or_na(.x = .data$Amount, .f = max),
        Sum      = .mny_stat_or_na(.x = .data$Amount, .f = sum),
        Median   = .mny_stat_or_na(.x = .data$Amount, .f = stats::median),
        .by = c(DocID, Block)
      ) |>
      tidyr::pivot_wider(
        names_from  = "Block",
        values_from = c("N", "NDistinct", "Max", "Sum", "Median"),
        names_glue  = paste0("{.value}{Block}", .suffix)
      )

    # A CURRENCY BLOCK THESE DOCUMENTS NEVER NAMED STILL GETS ITS COLUMNS. pivot_wider() names
    # columns after the values it FINDS, so a set of contracts in which none states a non-USD figure
    # produces no non-USD column at all -- and the arithmetic below then fails on a subset while
    # working perfectly on the corpus, because a large enough set always contains one. It is the
    # defect 04B5 already fixed for redaction kinds, and the fix is the same: take the shape from the
    # registered vocabulary, so it is a property of the taxonomy rather than of the input.
    #
    # COUNTS ARE FILLED WITH NA AND NOT ZERO, because the across() below already decides that a
    # missing count means none was found. Two places deciding it is how the two come to disagree.
    blocks_ <- plot_levels(.key = "MoneyBlock")
    count_  <- paste0(rep(c("N", "NDistinct"), each = length(blocks_)), blocks_, .suffix)
    amount_ <- paste0(rep(c("Max", "Sum", "Median"), each = length(blocks_)), blocks_, .suffix)

    for (nm_ in setdiff(count_,  names(out_))) out_[[nm_]] <- rep(NA_integer_, nrow(out_))
    for (nm_ in setdiff(amount_, names(out_))) out_[[nm_]] <- rep(NA_real_, nrow(out_))

    out_
  }

  rule_  <- agg_(.d = keep_, .suffix = "")
  naive_ <- agg_(.d = dplyr::filter(.release, .data$Parsed), .suffix = "Naive")

  counts_ <- .release |>
    dplyr::summarise(
      NAmountsRaw    = sum(.data$Parsed),
      NDroppedZero   = sum(.data$Parsed & .data$IsZero),
      NDroppedPar    = sum(.data$Parsed & !.data$IsZero & .data$IsPar),
      NUnparsed      = sum(!.data$Parsed & !.data$Withheld),
      NWithheldUSD   = sum(.data$Withheld & dplyr::coalesce(.data$Block, "") == "USD"),
      NWithheldOther = sum(.data$Withheld & dplyr::coalesce(.data$Block, "") != "USD"),
      .by = DocID
    )

  .keys |>
    dplyr::select("DocID") |>
    dplyr::left_join(counts_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(rule_,   by = dplyr::join_by(DocID)) |>
    dplyr::left_join(naive_,  by = dplyr::join_by(DocID)) |>
    dplyr::rename(dplyr::any_of(c(
      NAmountsUSD = "NUSD", NAmountsOther = "Nnon-USD",
      NDistinctUSD = "NDistinctUSD", NDistinctOther = "NDistinctnon-USD",
      MoneyMaxUSD = "MaxUSD", MoneyMaxOther = "Maxnon-USD",
      MoneySumUSD = "SumUSD", MoneySumOther = "Sumnon-USD",
      MoneyMedUSD = "MedianUSD", MoneyMedOther = "Mediannon-USD",
      NaiveMaxUSD = "MaxUSDNaive", NaiveMaxOther = "Maxnon-USDNaive",
      NaiveSumUSD = "SumUSDNaive", NaiveSumOther = "Sumnon-USDNaive"
    ))) |>
    # NAMED EXPLICITLY AND NOT BY PREFIX. starts_with("N") also matches NaiveMaxUSD and NaiveSumUSD,
    # which are AMOUNTS rather than counts -- casting those to integer would overflow anything above
    # 2.1 billion and turn a contract with no naive figure from missing into zero. A count is missing
    # because none was found and belongs at zero; an amount is missing because none was named and
    # does not.
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("NAmountsRaw", "NDroppedZero", "NDroppedPar", "NUnparsed",
                        "NWithheldUSD", "NWithheldOther", "NAmountsUSD", "NAmountsOther",
                        "NDistinctUSD", "NDistinctOther")),
        \(.x) as.integer(dplyr::coalesce(.x, 0L))
      ),
      NAmounts = as.integer(dplyr::coalesce(.data$NAmountsUSD, 0L) +
                              dplyr::coalesce(.data$NAmountsOther, 0L))
    ) |>
    dplyr::select(-dplyr::any_of(c("NDistinctUSDNaive", "NDistinctnon-USDNaive",
                                   "NUSDNaive", "Nnon-USDNaive",
                                   "MedianUSDNaive", "Mediannon-USDNaive")))
}


#' Apply the rule end to end
#'
#' ONE ENTRY POINT, AND 04D CALLS EXACTLY THIS. The release comes out; the collapse is returned beside
#' it because this document reports on it, and the export computes it again from the file.
#'
#' @param .money Tibble from mny_load().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .spec List from mny_spec().
#' @return A list: Spec, Marked, Release, Agg.
mny_apply <- function(.money, .keys, .spec) {
  if (FALSE) {
    .money <- tab_money
    .keys  <- tab_keys
    .spec  <- .lP$Params$Spec
  }

  marked_  <- mny_mark_par(.money = .money, .spec = .spec)
  release_ <- mny_release_spans(.money = marked_)

  list(
    Spec    = .spec,
    Marked  = marked_,
    Release = release_,
    Agg     = mny_collapse(.release = release_, .keys = .keys, .spec = .spec)
  )
}


#' The naive ladder, by contract type
#'
#' THREE RUNGS AND THE STEP BETWEEN EACH PAIR IS ONE FILTER. Parsed to no-zero is the zero filter;
#' no-zero to the rule is the par-value filter. Both are computable from the released file, so a
#' reader who rejects either takes the rung above it.
#'
#' @param .doc Tibble from mny_collapse(), optionally carrying Class.
#' @return Tibble: one row per type, and one for the sample.
mny_table_ladder <- function(.doc) {
  if (FALSE) .doc <- tab_agg

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs        = dplyr::n(),
      MeanRaw     = mean(.data$NAmountsRaw),
      MeanKept    = mean(.data$NAmounts),
      PctZero     = sum(.data$NDroppedZero) / pmax(sum(.data$NAmountsRaw), 1L),
      PctPar      = sum(.data$NDroppedPar) / pmax(sum(.data$NAmountsRaw), 1L),
      PctAnyUSD   = mean(!is.na(.data$MoneyMaxUSD)),
      MedMaxUSD   = .mny_stat_or_na(.x = .data$MoneyMaxUSD, .f = stats::median),
      MedNaiveUSD = .mny_stat_or_na(.x = .data$NaiveMaxUSD, .f = stats::median),
      PctWithheld = mean((.data$NWithheldUSD + .data$NWithheldOther) > 0L),
      .by = dplyr::any_of("Class")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(.doc), dplyr::desc(.data$Docs)),
    dplyr::mutate(cols_(dplyr::select(.doc, -dplyr::any_of("Class"))), Class = "All", .before = 1L)
  )
}


#' What became of every figure the extractor proposed
#' @param .release Tibble from mny_release_spans().
#' @return Tibble: one row per outcome.
mny_table_outcome <- function(.release) {
  if (FALSE) .release <- tab_release

  .release |>
    dplyr::summarise(
      Figures  = dplyr::n(),
      MedValue = .mny_stat_or_na(.x = .data$Amount, .f = stats::median),
      .by = MoneyDrop
    ) |>
    dplyr::mutate(Share = .data$Figures / sum(.data$Figures)) |>
    dplyr::arrange(plot_factor(.data$MoneyDrop, .key = "MoneyDrop"))
}


# 7. Evidence ------------------------------------------------------------------------------------------------------------

#' Reduce one specification to a comparable row
#'
#' @param .agg Tibble from mny_collapse(), one wide row per document.
#' @param .spec List from mny_spec().
#' @param .n_docs Integer. Documents in the sample.
#' @return Tibble: one row per currency block.
mny_row <- function(.agg, .spec, .n_docs) {
  if (FALSE) {
    .agg    <- tab_agg
    .spec   <- .lP$Params$Spec
    .n_docs <- nrow(tab_keys)
  }

  # THE COLLAPSE IS WIDE AND THIS ROW IS PER BLOCK, so the two currency blocks are unstacked here
  # rather than carried long through mny_collapse(). The collapse is what the export materialises and
  # a downstream file wants one row per contract; the sweep is a report and wants one row per block.
  # Doing it here keeps the shape decision with the thing that needs the shape.
  block_ <- function(.suffix, .name) {
    n_       <- .agg[[paste0("NAmounts", .suffix)]]
    distinct_ <- .agg[[paste0("NDistinct", .suffix)]]
    max_     <- .agg[[paste0("MoneyMax", .suffix)]]
    sum_     <- .agg[[paste0("MoneySum", .suffix)]]
    has_     <- !is.na(n_) & n_ > 0L

    tibble::tibble(
      Block       = .name,
      Docs        = sum(has_),
      PctOfSample = sum(has_) / .n_docs,
      Spans       = sum(n_[has_]),
      MeanSpans   = .mny_stat_or_na(.x = n_[has_], .f = mean),
      # DISTINCT AMOUNTS OVER TOTAL SPANS, and it is the caveat MoneySum needs: no filter can tell a
      # restatement from a second obligation, so a ratio near zero means the sum counts one figure
      # many times.
      RepeatRatio = .mny_stat_or_na(
        .x = (distinct_[has_] / pmax(n_[has_], 1L)), .f = mean
      ),
      MedMax      = .mny_stat_or_na(.x = max_[has_], .f = stats::median),
      MedSum      = .mny_stat_or_na(.x = sum_[has_], .f = stats::median)
    )
  }

  dplyr::bind_rows(block_(.suffix = "USD", .name = "USD"),
                   block_(.suffix = "Other", .name = "non-USD")) |>
    dplyr::mutate(Filter = .spec$Label, CueWin = .spec$CueWin, .before = 1L)
}


#' Sweep the filters and the window
#'
#' THE UNFILTERED ROW IS FIRST, so what the rule removes is visible rather than asserted. The window
#' rows are what the old specification could not sweep: IsPar was computed once at load time, so
#' every swept label named a width that changed nothing.
#'
#' @param .money Tibble from mny_load().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .specs List from mny_spec().
#' @param .quiet Logical. Suppress the progress bar.
#' @return Tibble: one row per specification and currency block.
mny_sweep <- function(.money, .keys, .specs, .quiet = FALSE) {
  if (FALSE) {
    .money <- tab_money
    .keys  <- tab_keys
    .specs <- .lP$Params$Specs
    .quiet <- FALSE
  }

  n_ <- dplyr::n_distinct(.keys$DocID)

  purrr::map(.specs, function(.s) {
    mny_row(.agg = mny_apply(.money = .money, .keys = .keys, .spec = .s)$Agg,
            .spec = .s, .n_docs = n_)
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


#' Which side of the figure the par language sat on
#'
#' THE MEASUREMENT THAT SAYS WHETHER READING BOTH SIDES WAS WORTH IT. Every amount marked on AFTER
#' alone is one the previous rule could not see, because it cut a window ending at the span. Nothing
#' here is a trade: reading the second side can only mark more, never unmark.
#'
#' @param .marked Tibble from mny_mark_par().
#' @return Tibble: one row per side.
mny_par_side <- function(.marked) {
  if (FALSE) .marked <- tab_marked

  src_ <- dplyr::filter(.marked, .data$Parsed, !.data$IsZero)

  src_ |>
    dplyr::summarise(Spans = dplyr::n(), Docs = dplyr::n_distinct(.data$DocID), .by = ParSide) |>
    dplyr::mutate(Share = .data$Spans / sum(.data$Spans)) |>
    dplyr::arrange(dplyr::desc(.data$Spans))
}


# 8. Tables --------------------------------------------------------------------------------------------------------------

#' What the extractor found and how much of it parsed
#' @param .money Tibble from mny_load().
#' @return Tibble: one row per outcome.
mny_table_parse <- function(.money) {
  if (FALSE) .money <- tab_money

  tibble::tibble(
    Item = c("Spans emitted",
             "Currency resolved",
             "Amount cast to a number",
             "A figure the CONTRACT withheld",
             "Neither: the cast failed"),
    N    = c(nrow(.money),
             sum(!is.na(.money$Currency)),
             sum(.money$Parsed),
             sum(.money$Withheld),
             sum(!.money$Parsed & !.money$Withheld))
  ) |>
    dplyr::mutate(Share = .data$N / pmax(nrow(.money), 1L))
}


#' Why each dropped amount was dropped
#' @param .release Tibble from mny_collapse().
#' @return Tibble: one row per reason.
mny_table_dropped <- function(.release) {
  if (FALSE) .release <- tab_release

  tibble::tibble(
    Reason = c("kept", "zero", "par"),
    Spans  = c(sum(.release$NAmounts),
               sum(.release$NDroppedZero),
               sum(.release$NDroppedPar)),
    Docs   = c(sum(.release$NAmounts > 0L),
               sum(.release$NDroppedZero > 0L),
               sum(.release$NDroppedPar > 0L))
  ) |>
    dplyr::mutate(Share = .data$Spans / pmax(sum(.data$Spans), 1L))
}


#' The money variables by contract type
#'
#' THE ORDERING IS THE ONLY THING THIS TABLE CAN BE READ FOR, and it is the only external check the
#' document has: there is no ground truth for what a contract is worth, but a credit agreement should
#' out-value an employment agreement and a reader can say whether it does.
#'
#' @param .release Tibble from mny_collapse().
#' @return Tibble: one row per type, and one for the sample.
mny_table_class <- function(.release) {
  if (FALSE) .release <- tab_release

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs      = dplyr::n(),
      PctUSD    = mean(!is.na(.data$MoneyMaxUSD)),
      PctOther  = mean(!is.na(.data$MoneyMaxOther)),
      MedMax    = stats::median(.data$MoneyMaxUSD, na.rm = TRUE),
      MedSum    = stats::median(.data$MoneySumUSD, na.rm = TRUE),
      # TWO DEFECTS IN ONE EXPRESSION, AND pmax HID BOTH. Dividing by pmax(NAmounts, 1L) turned a
      # contract with no kept USD amount into 0/1 rather than 0/0, so na.rm had nothing to remove and
      # the median came back as a hard ZERO -- which reads as "this class repeats every figure" and
      # means "the median contract here names no USD figure at all". Licenses and Other reported
      # exactly that, on classes whose PctUSD is under half.
      #
      # And the denominator was the WRONG COUNT: distinct USD amounts over amounts in EVERY currency.
      # Both are fixed by taking the ratio only where a USD amount was kept, over the USD count.
      MedRepeat = stats::median(
        dplyr::if_else(
          .data$NAmountsUSD > 0L, .data$NDistinctUSD / .data$NAmountsUSD, NA_real_
        ),
        na.rm = TRUE
      ),
      # THE REDACTION VARIABLE THAT IS ABOUT MONEY. Reported here rather than only in the release,
      # because whether the withheld prices concentrate in the contract types where commercially
      # material terms live is the whole question, and it is one column away from being answered.
      PctHeld   = mean((.data$NWithheldUSD + .data$NWithheldOther) > 0L),
      .by = dplyr::any_of("Class")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(.release), dplyr::desc(.data$Docs)),
    dplyr::mutate(cols_(dplyr::select(.release, -"Class")), Class = "All", .before = 1L)
  )
}


#' A few whole contracts, exactly as the file holds them
#'
#' DRAWN FROM CONTRACTS THAT NAMED SOMETHING, because a random draw of eight from the whole sample
#' would mostly show rows of nulls and say nothing about the rule. Two of them are drawn from the
#' contracts where a filter actually removed a figure, which is the shape a reader needs to recognise.
#'
#' @param .tab Tibble from mny_collapse().
#' @param .n Integer. Contracts drawn.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: the drawn rows.
mny_release_sample <- function(.tab, .n = 8L, .seed = 42L) {
  if (FALSE) {
    .tab  <- tab_release
    .n    <- 8L
    .seed <- 42L
  }

  cut_  <- .tab$DocID[(.tab$NDroppedZero + .tab$NDroppedPar) > 0L]
  some_ <- setdiff(.tab$DocID[.tab$NAmounts > 0L], cut_)

  pick_cut_  <- if (length(cut_) == 0L) character(0) else {
    withr::with_seed(.seed, sample(cut_, size = min(3L, length(cut_))))
  }
  pick_some_ <- if (length(some_) == 0L) character(0) else {
    withr::with_seed(.seed, sample(some_, size = min(.n - length(pick_cut_), length(some_))))
  }

  .tab |>
    dplyr::filter(.data$DocID %in% c(pick_cut_, pick_some_)) |>
    dplyr::arrange(dplyr::desc(.data$NDroppedZero + .data$NDroppedPar), .data$DocID)
}


# 9. Report --------------------------------------------------------------------------------------------------------------

#' What the extractor found and how much of it parsed
#' @param .tab Tibble from mny_table_parse().
#' @return Invisibly .tab.
mny_report_parse <- function(.tab) {
  if (FALSE) .tab <- tab_parse

  cli::cli_h2("What the extractor supplied")
  .tab |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "Spans emitted, and how many became a usable amount")

  cli::cli_alert_info(
    "THE THIRD AND FOURTH ROWS ARE DIFFERENT THINGS AND USED TO BE ONE. A WITHHELD figure is a \\
     currency with no number, which moneyregex emits on purpose from three patterns -- $[***], $**, \\
     $TBD, and a bare symbol before \"per share\". A FAILED CAST is this document unable to read a \\
     number that is there. Counting them together made every withheld price look like a parse error \\
     and dropped it before the first summarise."
  )
  cli::cli_alert_info(
    "ONLY THE THIRD ROW ENTERS AN ARITHMETIC. The withheld carry no number, so a sum or a maximum \\
     including them would have to invent one; they are counted as an EVENT instead, and released as \\
     NWithheldUSD and NWithheldOther."
  )
  invisible(.tab)
}


#' Which side of the figure the par language sat on
#' @param .tab Tibble from mny_par_side().
#' @return Invisibly .tab.
mny_report_par_side <- function(.tab) {
  if (FALSE) .tab <- tab_side

  cli::cli_h2("Where the par-value language sat")
  .tab |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "One row per side, over every non-zero parsed amount")

  cli::cli_alert_info(
    "AFTER IS WHAT THE PREVIOUS RULE COULD NOT SEE. It cut a window ENDING at the span, so \\
     \"shares of $0.01 par value per share\" -- the phrase written after the figure -- passed the \\
     filter every time. Reading the second side can only mark more and never unmark, so this is a \\
     gain with no cost column: the AFTER and BOTH rows are amounts that were being counted as \\
     contract value and are not."
  )
  invisible(.tab)
}


#' What each filter removed
#' @param .tab Tibble from mny_table_dropped().
#' @return Invisibly .tab.
mny_report_dropped <- function(.tab) {
  if (FALSE) .tab <- tab_drop

  cli::cli_h2("What the two filters removed")
  .tab |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "One row per reason, over every parsed amount")

  cli::cli_alert_info(
    "NEITHER FILTER IS A JUDGEMENT ABOUT WHETHER A FIGURE MATTERS. A zero is not an amount anybody \\
     owes, and neither is a share's par value -- both are boilerplate about denomination. The \\
     unfiltered count stays in the released file, so a reader who disagrees with either can put them \\
     back without re-running anything."
  )
  invisible(.tab)
}


#' The money variables by contract type
#' @param .tab Tibble from mny_table_class().
#' @return Invisibly .tab.
mny_report_class <- function(.tab) {
  if (FALSE) .tab <- tab_class

  cli::cli_h2("What each contract type is worth")
  .tab |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
      MedRepeat = tbl_pct_safe(.data$MedRepeat),
      dplyr::across(c(MedMax, MedSum), \(.x) format(round(.x), big.mark = ","))
    ) |>
    tbl_say(.title = "One row per type, and one for the sample")

  cli::cli_alert_info(
    "THE ORDERING IS THE ONLY EXTERNAL CHECK THIS DOCUMENT HAS. There is no ground truth for what a \\
     contract is worth, but a credit agreement should out-value an employment agreement and a reader \\
     can say whether it does. MedRepeat is distinct USD amounts over kept USD amounts, taken over \\
     the contracts that kept one: a low figure means a contract restating one number many times, \\
     which is the caveat MedSum needs, and a DASH means the median contract of that class names no \\
     USD figure at all."
  )
  cli::cli_alert_info(
    "PctHeld IS THE SHARE THAT NAMED A PRICE AND REMOVED IT -- \"$[***] per unit\" -- and it is the \\
     narrowest redaction measure this pipeline produces. 04B5 counts every [***] a contract carries; \\
     this counts the ones standing where a dollar figure belongs, which is what a question about \\
     withholding COMMERCIALLY MATERIAL terms is actually asking."
  )
  invisible(.tab)
}


#' What each filter and window would have kept
#' @param .tab Tibble from mny_sweep().
#' @return Invisibly .tab.
mny_report_sweep <- function(.tab) {
  if (FALSE) .tab <- tab_sweep

  cli::cli_h2("The filters, swept")
  .tab |>
    dplyr::mutate(
      PctOfSample = tbl_pct(.data$PctOfSample),
      RepeatRatio = tbl_pct(.data$RepeatRatio),
      MeanSpans   = tbl_num(.data$MeanSpans),
      dplyr::across(c(MedMax, MedSum), \(.x) format(round(.x), big.mark = ","))
    ) |>
    tbl_say(.title = "One row per specification and currency block, the unfiltered one first")

  cli::cli_alert_info(
    "THE WINDOW ROWS ARE WHAT THE OLD SPECIFICATION COULD NOT SWEEP. IsPar was computed once when the \\
     spans were loaded, so a swept width appeared in every label and changed nothing -- the same \\
     answer under three different names. Both filters now run where the sweep reaches them, and a \\
     width that changes no number is a finding rather than an artefact."
  )
  invisible(.tab)
}


#' The naive ladder, reported
#' @param .tab Tibble from mny_table_ladder().
#' @return Invisibly .tab.
mny_report_ladder <- function(.tab) {
  if (FALSE) .tab <- tab_ladder

  cli::cli_h2("The rule against the naive ladder")
  .tab |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Mean"), \(.x) tbl_num(.x)),
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
      dplyr::across(dplyr::starts_with("Med"), \(.x) format(round(.x), big.mark = ","))
    ) |>
    tbl_say(.title = "Figures per contract before and after the filters, by contract type")

  cli::cli_alert_info(
    "PctZero AND PctPar ARE THE TWO FILTERS, each as a share of every figure the extractor read. A \\
     zero is not an amount anybody owes and the two commonest money spans in this corpus are $0 and \\
     $0.00. A figure beside par-value language is a share's nominal value, and the cue is read on \\
     BOTH sides because a drafter writes it either way round."
  )
  cli::cli_alert_info(
    "MedMaxUSD AGAINST MedNaiveUSD IS WHAT THE FILTERS COST OR BOUGHT. Both are the largest USD \\
     figure per contract, before and after; a large gap means the filters are removing figures that \\
     were the maximum, which is exactly what par-value language beside a small number cannot cause \\
     and what a zero cannot cause either."
  )
  cli::cli_alert_info(
    "PctWithheld IS NOT A FILTER. It is contracts naming a price the filer removed -- a currency \\
     with no number -- and they are counted and never summed, because an arithmetic including them \\
     would have to invent a value."
  )
  invisible(.tab)
}


#' What became of every figure
#' @param .tab Tibble from mny_table_outcome().
#' @param .release Tibble from mny_release_spans().
#' @return Invisibly .tab.
mny_report_outcome <- function(.tab, .release) {
  if (FALSE) {
    .tab     <- tab_outcome
    .release <- tab_release
  }

  cli::cli_h2("What became of every figure the extractor proposed")
  .tab |>
    dplyr::mutate(
      Figures  = format(.data$Figures, big.mark = ","),
      MedValue = dplyr::if_else(is.finite(.data$MedValue),
                                format(round(.data$MedValue), big.mark = ","), "-"),
      Share    = tbl_pct(.data$Share)
    ) |>
    tbl_say(.title = "One row per outcome, over every figure in the released file")

  unp_ <- sum(.release$MoneyDrop == "unparsed")
  cli::cli_alert_info(
    "EVERY ROW IS IN THE FILE, including the four that no aggregate uses. That is what makes the \\
     ladder a filter rather than a second computation: a reader who wants the zeros back writes one \\
     predicate instead of re-running the rule."
  )
  cli::cli_alert_info(
    "WITHHELD IS NOT UNPARSED, and separating them is the reason both levels exist. A withheld \\
     figure is a currency the filer wrote with the number removed; an unparsed one is a figure this \\
     document could not read. Both carry a null amount, so keying on the moneyregex PATTERN rather \\
     than on the null is what tells two opposite findings apart."
  )
  cli::cli_alert_info(
    "UNPARSED IS {format(unp_, big.mark = ',')}, and a small number here is a finding about the \\
     extractor: moneyregex emits a figure it cannot cast only through the three withheld patterns, \\
     so anything else in this row would be a currency it matched and then failed to read."
  )
  invisible(.tab)
}


#' What the released file holds
#' @param .release Tibble from mny_release_spans().
#' @param .agg Tibble from mny_collapse().
#' @return Invisibly the summary.
mny_report_release <- function(.release, .agg) {
  if (FALSE) {
    .release <- tab_release
    .agg     <- tab_agg
  }

  cli::cli_h2("The released file")

  out_ <- tibble::tibble(
    Item = c("Figures released",
             "Contracts naming any figure",
             "Contracts in the collapse",
             "Contracts with a USD amount after the filters"),
    N    = c(nrow(.release),
             dplyr::n_distinct(.release$DocID),
             nrow(.agg),
             sum(!is.na(.agg$MoneyMaxUSD)))
  ) |>
    dplyr::mutate(N = format(.data$N, big.mark = ","))

  tbl_say(.tab = out_, .title = "money_spans.parquet, and the collapse it supports")

  cli::cli_alert_info(
    "THE COLLAPSE IS NOT WRITTEN. mny_collapse() produces one row per contract and the export calls \\
     it; this document checks its shape against a dictionary so the export cannot produce a \\
     different one. An aggregate stored as data is an aggregate whose filters cannot be changed."
  )
  invisible(out_)
}


#' A few whole contracts, printed as the file holds them
#' @param .tab Tibble from mny_release_sample().
#' @return Invisibly .tab.
mny_report_release_sample <- function(.tab) {
  if (FALSE) .tab <- tab_sample

  cli::cli_h2("The released file, read")

  .tab |>
    dplyr::transmute(
      .data$DocID, .data$Class,
      .data$NAmountsRaw, .data$NAmounts, .data$NDroppedZero, .data$NDroppedPar,
      MaxUSD      = format(round(.data$MoneyMaxUSD), big.mark = ","),
      NaiveMaxUSD = format(round(.data$NaiveMaxUSD), big.mark = ","),
      SumUSD      = format(round(.data$MoneySumUSD), big.mark = ","),
      .data$NDistinctUSD
    ) |>
    tbl_say(.title = "Contracts where a filter bit, then contracts where it did not")

  cli::cli_alert_info(
    "READ MaxUSD AGAINST NaiveMaxUSD ROW BY ROW. Where they are equal the filters removed nothing \\
     that mattered to the maximum; where they differ, the largest figure the contract named was a \\
     par value or a zero. NDistinctUSD against NAmounts is the repetition: a contract naming fifty \\
     million five times has one distinct amount and five spans, and its sum is two hundred and fifty \\
     million."
  )
  invisible(.tab)
}


#' The column dictionary
#' @param .tab Any of this document's dictionary tables.
#' @param .title Character. Heading, since the reporter is shared.
#' @return Invisibly .tab.
mny_report_dictionary <- function(.tab, .title = "The columns, in the order the file carries them") {
  if (FALSE) {
    .tab   <- tab_dict
    .title <- "money_spans.parquet"
  }

  cli::cli_h2(.title)
  tbl_say(.tab = .tab, .title = "Seventeen columns, in the order the file carries them")

  cli::cli_alert_info(
    "THE NAIVE COLUMNS ARE THE ONES THAT MAKE THE REST ARGUABLE. Keeping only what the rule kept \\
     would leave a reader unable to check what it removed; publishing both means the filters are a \\
     choice the file supports rather than one it hides. The dictionary is compared with the file's \\
     own names, so a column added or renamed without an entry aborts this chunk."
  )
  invisible(.tab)
}


#' The rule in one table
#' @param .release Tibble from mny_collapse().
#' @param .side Tibble from mny_par_side().
#' @param .n_spans Integer. Rows in money_spans.parquet, which is the released file --
#'   .release is the collapse, and it has one row per contract rather than per figure.
#' @return Invisibly the table.
mny_report_headline <- function(.release, .side, .n_spans) {
  if (FALSE) {
    .release <- tab_release
    .side    <- tab_side
  }

  cli::cli_h2("The rule in one table")

  # ONE POPULATION FOR BOTH MEDIANS: the contracts the rule kept a USD amount in. Taking each
  # column's own non-missing rows compares a set including the boilerplate-only contracts with one
  # excluding them, and reports the filters as having RAISED contract value.
  keep_ <- !is.na(.release$MoneyMaxUSD)
  usd_  <- .release$MoneyMaxUSD[keep_]
  nai_  <- .release$NaiveMaxUSD[keep_]
  lost_ <- sum(!is.na(.release$NaiveMaxUSD) & is.na(.release$MoneyMaxUSD))
  aft_  <- sum(.side$Spans[.side$ParSide %in% c("after", "both")])

  out_ <- tibble::tribble(
    ~Item,                                     ~Value,
    "Contracts",                               format(nrow(.release), big.mark = ","),
    "Naming a USD amount",                     tbl_pct(mean(!is.na(.release$MoneyMaxUSD))),
    "Amounts parsed",                          format(sum(.release$NAmountsRaw), big.mark = ","),
    "Amounts kept",                            format(sum(.release$NAmounts), big.mark = ","),
    "Dropped as a zero",                       format(sum(.release$NDroppedZero), big.mark = ","),
    "Dropped as a par value",                  format(sum(.release$NDroppedPar), big.mark = ","),
    "Of those, marked only by CueAfter",       format(aft_, big.mark = ","),
    "Prices the CONTRACT withheld",            format(sum(.release$NWithheldUSD) +
                                                        sum(.release$NWithheldOther),
                                                      big.mark = ","),
    "Contracts withholding at least one",      tbl_pct(mean(.release$NWithheldUSD +
                                                              .release$NWithheldOther > 0L)),
    "Median largest USD amount, unfiltered",   format(round(stats::median(nai_)), big.mark = ","),
    "Median largest USD amount, this rule",    format(round(stats::median(usd_)), big.mark = ","),
    "Contracts whose every USD figure went",   format(lost_, big.mark = ","),
    "Figures in the released file",              format(.n_spans, big.mark = ",")
  )

  tbl_say(.tab = out_, .title = "Everything this document decided")

  cli::cli_alert_info(
    "THE TWO MEDIAN ROWS ARE THE RESULT, and both are over the SAME contracts -- the ones the rule \\
     kept a USD amount in. Computing each over its own non-missing rows compares a set including the \\
     contracts whose every figure was boilerplate with one excluding them, and then reports the \\
     filters as having RAISED contract value. The row below it is that excluded population."
  )
  cli::cli_alert_info(
    "The CueAfter row is the amounts the previous rule could not have removed, because it read only \\
     the characters before the figure."
  )
  invisible(out_)
}


# 10. Figures ------------------------------------------------------------------------------------------------------------

#' The rule against the unfiltered count, by contract type
#' @param .tab Tibble from mny_table_ladder().
#' @return A ggplot.
mny_plot_naive <- function(.tab) {
  if (FALSE) .tab <- tab_ladder

  .tab |>
    dplyr::filter(.data$Class != "All") |>
    dplyr::select("Class", Unfiltered = "MeanRaw", Rule = "MeanKept") |>
    tidyr::pivot_longer(cols = c("Unfiltered", "Rule"), names_to = "Measure",
                        values_to = "Amounts") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Amounts,
                                 y = stats::reorder(.data$Class, .data$Amounts),
                                 fill = .data$Measure)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
    plot_scale_fill_cat(name = NULL) +
    plot_scale_x_count() +
    ggplot2::labs(x = "Mean amounts per contract", y = NULL) +
    plot_theme(.grid = "x", .legend = "bottom")
}


#' The largest USD amount, by contract type
#' @param .release Tibble from mny_collapse().
#' @return A ggplot.
mny_plot_max <- function(.release) {
  if (FALSE) .release <- tab_release

  .release |>
    dplyr::filter(!is.na(.data$MoneyMaxUSD), .data$MoneyMaxUSD > 0) |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$MoneyMaxUSD,
      y = stats::reorder(.data$Class, .data$MoneyMaxUSD, FUN = stats::median)
    )) +
    ggplot2::geom_boxplot(outlier.size = 0.5, linewidth = 0.4, fill = plot_pal_seq(1L),
                          colour = plot_pal_grey(1L)) +
    ggplot2::scale_x_log10(labels = scales::label_comma()) +
    ggplot2::labs(x = "Largest USD amount, log scale", y = NULL) +
    plot_theme(.grid = "x")
}


#' Where the par-value language sat
#' @param .tab Tibble from mny_par_side().
#' @return A ggplot.
mny_plot_par_side <- function(.tab) {
  if (FALSE) .tab <- tab_side

  plot_bar_ranked(
    .tab      = dplyr::filter(.tab, .data$ParSide != "none"),
    .cat      = "ParSide",
    .val      = "Spans",
    .accuracy = 1
  ) +
    ggplot2::labs(x = "Amounts marked as a par value")
}


#' How much of a contract's total is repetition
#' @param .release Tibble from mny_collapse().
#' @return A ggplot.
mny_plot_repeat <- function(.release) {
  if (FALSE) .release <- tab_release

  .release |>
    dplyr::filter(.data$NAmounts > 0L) |>
    dplyr::mutate(Ratio = .data$NDistinctUSD / .data$NAmounts) |>
    dplyr::filter(!is.na(.data$Ratio), .data$Ratio > 0) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Ratio)) +
    ggplot2::geom_histogram(bins = 40L, fill = plot_pal_seq(1L), colour = NA) +
    plot_scale_x_pct(.accuracy = 1) +
    plot_scale_y_count() +
    ggplot2::labs(x = "Distinct amounts over amounts kept", y = "Contracts") +
    plot_theme(.grid = "y")
}


