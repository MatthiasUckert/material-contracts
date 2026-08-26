# 04B4-Rules-MONEY: what a contract says it is worth ----------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# Turns parsed amounts into contract-level money variables. Function prefix is `mny_`.
#
# THE LEAST OPINIONATED OF THE FIVE, AND IT SHOULD BE
# There is no external fact to anchor an amount against and no taxonomy that says what a contract's
# value is, so this document does not invent one. It reports what the extractor found, removes the two
# things that are demonstrably not amounts anybody owes, and puts the numbers beside the contract type
# so a reader can see whether the ordering makes sense.
#
# THE RULE IS TWO FILTERS AND NO NUMBER
#   1. A ZERO IS NOT AN AMOUNT. The store's two commonest money spans are "$ 0" and "$ 0.00", which is
#      boilerplate about share denomination rather than anything a party owes.
#   2. A FIGURE BESIDE PAR-VALUE LANGUAGE IS NOT AN AMOUNT EITHER. "par value $0.0001 per share",
#      "stated value of $0.01", "shares of $0.01 par value per share" -- share denomination again, and
#      the commonest way a contract names a number that means nothing about its worth.
# Neither is a judgement about whether a figure MATTERS. Both are figures that are not amounts, and
# the unfiltered count stays in every table so a reader sees what was removed.
#
# ONE ENGINE, AND THE MEASUREMENT IS DECISIVE
# LexNLP's money grammar accepts ONE PHRASING IN SIX. Measured on a fixture and confirmed on the
# sample: of "$9,752,233.001", "$2,500,000", "US$5,000,000", "5,000,000 dollars", "USD 5,000,000" and
# "Five Million Dollars", only the last matches. The consequence at scale is a coverage of 0.163
# against matcon's 0.712 on the corpus pass, and 0.260 against 0.830 on the sample.
#
# AND IT SETTLES A CLAIM THAT WAS IN THE PROSE. An earlier version kept both families on "of 2,541
# mentions the two both found, ZERO share identical offsets, so neither replaces the other". That
# sentence is true and its reading was wrong: zero agreement between a producer that finds five
# phrasings in six and one that finds one is not two engines disagreeing, it is one engine largely
# ABSENT. Releasing both would give every second row a column of nulls, so matcon is the release.
#
# THE PAR CUE IS A COLUMN NOW, AND THIS WAS THE LAST TEXT READ IN THE FAMILY
# mny_load() used to open 04A's canonical text and cut sixty characters before every amount. Every
# matcon span carries CueBefore and CueAfter -- 160 raw characters either side, stored at extraction
# where the document and the offsets were already in one scope -- so the test is a column read.
#
# WITH 04B2 AND 04B3 ALREADY MOVED, NO RULE IN THE 04B FAMILY OPENS A DOCUMENT. 04D's Cues dial
# existed to switch those three reads off at corpus scale, and switching them off is what made the
# governing-law columns vanish from the release. There is nothing left for it to gate.
#
# AND THE WINDOW READS BOTH SIDES, WHICH IT COULD NOT BEFORE. The old roxygen promised "characters
# either side of an amount" and the code cut a window ENDING at the span, so par language written
# AFTER the figure was structurally invisible: "shares of $0.01 par value per share" passed the filter
# every time. Test-MatCon-MONEY.R pinned both halves of that and its doc08 check is written to FLIP
# when a rule reads CueAfter, which is how this change proves itself rather than being asserted.
#
# CURRENCIES ARE NEVER POOLED
# A maximum across currencies is not a quantity. Currency is populated on 100% of matcon's rows, so
# the split costs no coverage at all, and USD and non-USD sit in columns of one row rather than in two
# tables nobody joins.
#
# A WITHHELD FIGURE IS A MEASUREMENT, NOT A PARSE FAILURE
# moneyregex has three patterns for an amount the filer removed -- symbol_redact for "$[***]", "$**",
# "$TBD" and "$____"; symbol_redact_open for an unclosed "$[ ***"; and symbol_bare for a currency
# symbol sitting immediately before "per", as in "a minimum market price of $ per share". All three
# emit a CURRENCY and a NULL AMOUNT, deliberately, and the module says why: "the amounts a filer
# withholds are systematically the material ones. Returning zero, or dropping the row, would erase
# exactly the observation that matters."
#
# THIS DOCUMENT WAS ERASING IT. Parsed required both an amount and a currency, and the aggregation
# began by filtering on it, so every withheld figure was gone before the first summarise and the
# parse table counted it as a failure to cast. It is not a failure. It is a contract naming a price
# and refusing to say what it is.
#
# AND IT IS SHARPER THAN ANYTHING 04B5 CAN OFFER. NRedactSymbol counts "[***]" wherever it falls --
# in a schedule, a definition, a party's name. A symbol_redact span is "[***]" STANDING WHERE A
# DOLLAR FIGURE BELONGS, which is a different and much narrower claim, and it is the one referee 2's
# question about redaction intensity actually points at.
#
# THE REPETITION RATIO IS WHY SUM SURVIVES
# A contract restating the same fifty million across five clauses sums to two hundred and fifty
# million, and no filter can tell a restatement from a second obligation. So distinct amounts over
# total spans is reported beside the sum: it says how much of a total is repetition, which is the
# caveat the sum needs rather than an argument for dropping it.
#
# MAX IS THE ONE UNAMBIGUOUS FIGURE -- the largest amount the contract names -- and it is the column a
# reader wanting one number should take.
#
# EVERY TABLE CARRIES THE UNFILTERED COUNT
# NAIVE is every parsed amount, both filters off: what a reader gets by summing what the extractor
# emitted. It sits beside the rule in every table and in the released file, because the difference
# between the two IS what the filters did.
#
# ONE FILE, ONE ROW PER CONTRACT
# A contract's value belongs to the contract and to no party in it, so this writes its own file. That
# is not the two-file problem 04B1 removed: the counts in that second file were DERIVED from the first
# and could contradict it, while an amount is an independent measurement nothing else computes.
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
  .levels = c("kept", "zero", "par", "unparsed"),
  .short  = c("kept", "zero", "par", "unparsed")
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


# 3. Input ---------------------------------------------------------------------------------------------------------------

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
#' @return .money with Before, After and IsPar added.
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

  norm_ <- function(.x) {
    stringi::stri_trans_toupper(stringi::stri_replace_all_regex(dplyr::coalesce(.x, ""), "\\s+", " "))
  }

  .money |>
    dplyr::mutate(
      Before = norm_(stringi::stri_sub(.data$CueBefore, from = -.spec$CueWin)),
      After  = norm_(stringi::stri_sub(.data$CueAfter,  to   =  .spec$CueWin)),
      IsPar  = hit_(.data$Before, .mny_par_cue) | hit_(.data$After, .mny_par_cue),
      ParSide = dplyr::case_when(
        !.data$IsPar                          ~ "none",
        hit_(.data$Before, .mny_par_cue) &
          hit_(.data$After, .mny_par_cue)     ~ "both",
        hit_(.data$Before, .mny_par_cue)      ~ "before",
        .default                              = "after"
      )
    )
}


# 4. The variables -------------------------------------------------------------------------------------------------------

#' Apply one specification and aggregate per contract and currency block
#'
#' SUM, MAX AND MEDIAN ALL SURVIVE, with the repetition ratio beside them. Max is the only one that is
#' unambiguous -- the largest figure the contract names -- but a reader wanting a total should have
#' one, and distinct amounts over total spans is the caveat it needs.
#'
#' THE NAIVE FIGURES ARE COMPUTED WHATEVER THE SPECIFICATION ASKS FOR, because they are the comparison
#' every table is read against and they cannot be derived later: the filtered rows are gone by then.
#'
#' @param .money Tibble from mny_mark_par().
#' @param .spec List from mny_spec().
#' @return Tibble: one row per document and currency block.
mny_aggregate <- function(.money, .spec) {
  if (FALSE) {
    .money <- tab_marked
    .spec  <- .lP$Params$Spec
  }

  # THE WITHHELD ARE COUNTED FROM THE WHOLE TABLE and never enter a sum, a maximum or a median: they
  # carry no number, so an arithmetic that included them would have to invent one. They are a count
  # of an event -- this contract named a price and removed it -- and that is all they can be.
  held_ <- .money |>
    dplyr::filter(.data$Withheld) |>
    dplyr::summarise(NWithheld = dplyr::n(), .by = c(DocID, Block))

  src_ <- dplyr::filter(.money, .data$Parsed)

  keep_ <- switch(
    .spec$Filter,
    none = rep(TRUE, nrow(src_)),
    zero = !src_$IsZero,
    par  = !src_$IsZero & !src_$IsPar
  )

  src_ |>
    dplyr::mutate(Keep = keep_) |>
    dplyr::summarise(
      NSpans      = sum(.data$Keep),
      NDistinct   = dplyr::n_distinct(.data$Amount[.data$Keep]),
      MoneyMax    = .mny_stat_or_na(.x = .data$Amount[.data$Keep], .f = max),
      MoneySum    = sum(.data$Amount[.data$Keep]),
      MoneyMed    = .mny_stat_or_na(.x = .data$Amount[.data$Keep], .f = stats::median),
      NaiveSpans  = dplyr::n(),
      NaiveMax    = .mny_stat_or_na(.x = .data$Amount, .f = max),
      NaiveSum    = sum(.data$Amount),
      NDropZero   = sum(.data$IsZero),
      NDropPar    = sum(!.data$IsZero & .data$IsPar),
      .by = c(DocID, Block)
    ) |>
    # FULL JOIN, because a contract can withhold every figure it names and parse none of them --
    # "$[***] per share" and nothing else -- and an inner join would drop exactly the document the
    # variable exists to find.
    dplyr::full_join(held_, by = dplyr::join_by(DocID, Block)) |>
    dplyr::mutate(
      dplyr::across(c(NSpans, NDistinct, NaiveSpans, NDropZero, NDropPar, NWithheld),
                    \(.x) as.integer(dplyr::coalesce(.x, 0L))),
      dplyr::across(c(MoneySum, NaiveSum), \(.x) dplyr::coalesce(.x, 0)),
      RepeatRatio = dplyr::if_else(.data$NSpans > 0L, .data$NDistinct / .data$NSpans, NA_real_)
    ) |>
    dplyr::filter(.data$NaiveSpans > 0L | .data$NWithheld > 0L, !is.na(.data$Block))
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


#' The release: one row per contract, USD and non-USD side by side
#'
#' EVERY CONTRACT GETS A ROW, including the ones matcon found no amount in. They carry zero spans and
#' a missing maximum, so a mean over the file divides by the sample rather than by the documents that
#' named a figure.
#'
#' THE TWO BLOCKS ARE COLUMNS OF ONE ROW rather than two rows or two tables. A maximum across
#' currencies is not a quantity, so they can never be pooled -- but a reader wanting the USD figure
#' should not have to filter, and one wanting both should not have to join.
#'
#' NWithheldUSD AND NWithheldOther ARE THE VARIABLE THIS DOCUMENT WAS THROWING AWAY. A currency with
#' no figure: the contract named a price and removed it. It is a narrower and sharper claim than
#' 04B5's NRedactSymbol, which counts "[***]" wherever it falls -- in a schedule, a definition, a
#' party's name -- while this counts it standing where a dollar figure belongs.
#'
#' THEY ENTER NO ARITHMETIC. A withheld figure carries no number, so a sum or a maximum including it
#' would have to invent one. It is a count of an EVENT and nothing else.
#'
#' EVERY QUANTITY IS PER BLOCK, AND NAmounts WAS THE ONE THAT WAS NOT. Maxima, sums, distinct counts,
#' naive figures and withheld prices all split USD from the rest; the kept COUNT was pooled, so a
#' ratio of distinct USD amounts over it mixed a USD numerator with a two-block denominator and
#' understated repetition wherever a contract named a foreign figure. NAmountsUSD and NAmountsOther
#' remove that, and NAmounts stays as their total because a reader wanting one number should not have
#' to add two.
#'
#' THE NAIVE COLUMNS ARE RELEASED, and they are the definition this document argues against. Without
#' them the comparison every table makes cannot be reproduced from the file, because the filtered
#' spans are not in it. Publishing the baseline beside the rule is the same discipline that keeps the
#' excluded parties in 04B1's file.
#'
#' @param .agg Tibble from mny_aggregate().
#' @param .keys Tibble from ent_anchor_keys().
#' @return Tibble: one row per contract, seventeen columns.
mny_release <- function(.agg, .keys) {
  if (FALSE) {
    .agg  <- tab_agg
    .keys <- tab_keys
  }

  wide_ <- .agg |>
    dplyr::select("DocID", "Block", "NSpans", "NDistinct", "MoneyMax", "MoneySum",
                  "NaiveSpans", "NaiveMax", "NaiveSum", "NWithheld") |>
    tidyr::pivot_wider(
      names_from  = Block,
      values_from = c(NSpans, NDistinct, MoneyMax, MoneySum, NaiveSpans, NaiveMax, NaiveSum,
                      NWithheld),
      names_sep   = ""
    )

  drop_ <- .agg |>
    dplyr::summarise(NDropZero = sum(.data$NDropZero), NDropPar = sum(.data$NDropPar),
                     .by = DocID)

  # NAMED EXPLICITLY RATHER THAN BY PREFIX. pivot_wider() emits a column only for a block that
  # appears somewhere in the data, so a sample holding no non-USD amount would produce a file with a
  # different schema from one that does -- and nothing would say so. Declaring the names here means a
  # missing block is a column of zeros rather than an absent column.
  need_ <- c("NSpansUSD", "NSpansnon-USD", "NDistinctUSD", "NDistinctnon-USD",
             "MoneyMaxUSD", "MoneyMaxnon-USD", "MoneySumUSD", "MoneySumnon-USD",
             "NaiveSpansUSD", "NaiveSpansnon-USD", "NaiveMaxUSD", "NaiveMaxnon-USD",
             "NaiveSumUSD", "NaiveSumnon-USD", "NWithheldUSD", "NWithheldnon-USD")
  for (nm_ in setdiff(need_, names(wide_))) wide_[[nm_]] <- NA_real_

  .keys |>
    dplyr::select("DocID", "Class", "AmendType") |>
    dplyr::left_join(wide_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(drop_, by = dplyr::join_by(DocID)) |>
    dplyr::transmute(
      .data$DocID, .data$Class, .data$AmendType,
      NAmounts     = as.integer(dplyr::coalesce(.data$NSpansUSD, 0) +
                                dplyr::coalesce(.data$`NSpansnon-USD`, 0)),
      NAmountsUSD   = as.integer(dplyr::coalesce(.data$NSpansUSD, 0)),
      NAmountsOther = as.integer(dplyr::coalesce(.data$`NSpansnon-USD`, 0)),
      NAmountsRaw  = as.integer(dplyr::coalesce(.data$NaiveSpansUSD, 0) +
                                dplyr::coalesce(.data$`NaiveSpansnon-USD`, 0)),
      NDroppedZero = as.integer(dplyr::coalesce(.data$NDropZero, 0L)),
      NDroppedPar  = as.integer(dplyr::coalesce(.data$NDropPar, 0L)),
      NWithheldUSD   = as.integer(dplyr::coalesce(.data$NWithheldUSD, 0L)),
      NWithheldOther = as.integer(dplyr::coalesce(.data$`NWithheldnon-USD`, 0L)),
      MoneyMaxUSD     = .data$MoneyMaxUSD,
      MoneySumUSD     = .data$MoneySumUSD,
      NDistinctUSD    = as.integer(dplyr::coalesce(.data$NDistinctUSD, 0L)),
      NaiveMaxUSD     = .data$NaiveMaxUSD,
      NaiveSumUSD     = .data$NaiveSumUSD,
      MoneyMaxOther   = .data$`MoneyMaxnon-USD`,
      MoneySumOther   = .data$`MoneySumnon-USD`,
      NDistinctOther  = as.integer(dplyr::coalesce(.data$`NDistinctnon-USD`, 0L)),
      NaiveMaxOther   = .data$`NaiveMaxnon-USD`,
      NaiveSumOther   = .data$`NaiveSumnon-USD`
    ) |>
    dplyr::arrange(.data$DocID)
}


#' Apply one specification end to end
#'
#' @param .money Tibble from mny_load().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .spec List from mny_spec().
#' @return A list: Spec, Marked, Agg, Release.
mny_apply <- function(.money, .keys, .spec) {
  if (FALSE) {
    .money <- tab_money
    .keys  <- tab_keys
    .spec  <- .lP$Params$Spec
  }

  marked_ <- mny_mark_par(.money = .money, .spec = .spec)
  agg_    <- mny_aggregate(.money = marked_, .spec = .spec)

  list(Spec = .spec, Marked = marked_, Agg = agg_,
       Release = mny_release(.agg = agg_, .keys = .keys))
}


# 5. Evidence ------------------------------------------------------------------------------------------------------------

#' Reduce one specification to a comparable row
#'
#' @param .agg Tibble from mny_aggregate().
#' @param .spec List from mny_spec().
#' @param .n_docs Integer. Documents in the sample.
#' @return Tibble: one row per currency block.
mny_row <- function(.agg, .spec, .n_docs) {
  if (FALSE) {
    .agg    <- tab_agg
    .spec   <- .lP$Params$Spec
    .n_docs <- nrow(tab_keys)
  }

  .agg |>
    dplyr::filter(.data$NSpans > 0L) |>
    dplyr::summarise(
      Docs        = dplyr::n(),
      PctOfSample = dplyr::n() / .n_docs,
      Spans       = sum(.data$NSpans),
      MeanSpans   = mean(.data$NSpans),
      RepeatRatio = mean(.data$RepeatRatio, na.rm = TRUE),
      MedMax      = stats::median(.data$MoneyMax, na.rm = TRUE),
      MedSum      = stats::median(.data$MoneySum, na.rm = TRUE),
      .by = Block
    ) |>
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


# 6. Tables --------------------------------------------------------------------------------------------------------------

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


#' What each filter removed, by contract type
#'
#' THE TABLE THIS DOCUMENT EXISTS TO PRODUCE. NAIVE is every parsed amount, both filters off -- what a
#' reader gets by summing what the extractor emitted. RULE is what the two filters left.
#'
#' @param .release Tibble from mny_release().
#' @return Tibble: one row per type, and one for the sample.
mny_table_naive <- function(.release) {
  if (FALSE) .release <- tab_release

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs        = dplyr::n(),
      PctAny      = mean(.data$NAmounts > 0L),
      MeanNaive   = mean(.data$NAmountsRaw),
      MeanRule    = mean(.data$NAmounts),
      PctRemove   = sum(.data$NAmountsRaw - .data$NAmounts) / pmax(sum(.data$NAmountsRaw), 1L),
      # BOTH MEDIANS OVER ONE POPULATION, and getting that wrong made the rule look like it RAISED
      # contract value. A contract whose only figures were zeros or par values has a NaiveMaxUSD --
      # nought, or a hundredth of a dollar -- and no MoneyMaxUSD at all, so a median over each
      # column's own non-missing rows compares a set that includes those with a set that excludes
      # them. The boilerplate-only contracts drag the naive median DOWN, and the filtered median then
      # sits above it: five of thirteen contract types reported exactly that, against a per-contract
      # identity that holds on every single row.
      #
      # The population is the contracts the rule kept a USD amount in. On those, MoneyMaxUSD is at
      # most NaiveMaxUSD by construction, so the medians can only order one way.
      MedMaxNaive = stats::median(.data$NaiveMaxUSD[!is.na(.data$MoneyMaxUSD)], na.rm = TRUE),
      MedMaxRule  = stats::median(.data$MoneyMaxUSD[!is.na(.data$MoneyMaxUSD)], na.rm = TRUE),
      # AND THE OTHER HALF OF THE STORY, which the old table could not show: contracts that named a
      # USD figure and kept none of it. Those are exactly the rows the median has to exclude, so the
      # count belongs beside it rather than nowhere.
      PctAllPar   = mean(!is.na(.data$NaiveMaxUSD) & is.na(.data$MoneyMaxUSD)),
      .by = dplyr::any_of("Class")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(.release), dplyr::desc(.data$Docs)),
    dplyr::mutate(cols_(dplyr::select(.release, -"Class")), Class = "All", .before = 1L)
  )
}


#' Why each dropped amount was dropped
#' @param .release Tibble from mny_release().
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
#' @param .release Tibble from mny_release().
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
#' @param .tab Tibble from mny_release().
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


#' What every column of the released file means
#'
#' A TABLE RATHER THAN PROSE, AND CHECKED AGAINST THE FILE. A column added or renamed without a
#' matching entry aborts the render rather than leaving the documentation quietly wrong.
#'
#' @param .tab Tibble from mny_release().
#' @return Tibble: Column, Meaning.
mny_dictionary <- function(.tab) {
  if (FALSE) .tab <- tab_release

  dict_ <- tibble::tribble(
    ~Column,          ~Meaning,
    "DocID",          "the contract; joins to every other 04 file",
    "Class",          "contract type, from 03A's label spine",
    "AmendType",      "original or amended, from the same spine",
    "NAmounts",       "amounts the rule kept, both currency blocks",
    "NAmountsUSD",    "of those, USD; the denominator a USD ratio needs",
    "NAmountsOther",  "of those, every other currency",
    "NAmountsRaw",    "amounts the extractor parsed, before either filter",
    "NDroppedZero",   "of those, figures of exactly zero",
    "NDroppedPar",    "of those, figures with par-value language beside them",
    "NWithheldUSD",   "USD prices the CONTRACT removed: $[***], $**, $TBD, $ per share",
    "NWithheldOther", "the same in every other currency",
    "MoneyMaxUSD",    "the largest USD amount the rule kept; the one unambiguous figure",
    "MoneySumUSD",    "their total; read against NDistinctUSD, which says how much is repetition",
    "NDistinctUSD",   "distinct kept USD amounts",
    "NaiveMaxUSD",    "the largest USD amount before either filter",
    "NaiveSumUSD",    "their total before either filter",
    "MoneyMaxOther",  "the same maximum for every non-USD currency, never pooled with USD",
    "MoneySumOther",  "their total",
    "NDistinctOther", "distinct kept non-USD amounts",
    "NaiveMaxOther",  "the non-USD maximum before either filter",
    "NaiveSumOther",  "their total before either filter"
  )

  undoc_  <- setdiff(names(.tab), dict_$Column)
  unseen_ <- setdiff(dict_$Column, names(.tab))
  say_    <- function(.x) if (length(.x) == 0L) "none" else paste(.x, collapse = ", ")

  if (length(undoc_) > 0L || length(unseen_) > 0L) {
    cli::cli_abort(c(
      "The dictionary and the released file disagree.",
      "x" = "In the file and undocumented: {say_(undoc_)}.",
      "x" = "Documented and not in the file: {say_(unseen_)}.",
      "i" = "A dictionary that can drift from its file documents nothing."
    ))
  }

  dict_[match(names(.tab), dict_$Column), ]
}


# 7. Report --------------------------------------------------------------------------------------------------------------

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


#' The rule against the unfiltered count
#' @param .tab Tibble from mny_table_naive().
#' @return Invisibly .tab.
mny_report_naive <- function(.tab) {
  if (FALSE) .tab <- tab_naive

  cli::cli_h2("The rule against the unfiltered count")
  .tab |>
    dplyr::mutate(
      dplyr::across(c(MeanNaive, MeanRule), \(.x) tbl_num(.x)),
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
      dplyr::across(dplyr::starts_with("MedMax"), \(.x) format(round(.x), big.mark = ","))
    ) |>
    tbl_say(.title = "Amounts kept against amounts found, by contract type")

  cli::cli_alert_info(
    "PctRemove IS THE SHARE OF PARSED AMOUNTS THE TWO FILTERS DISCARDED: too low and the rule is \\
     doing nothing, too high and it is cutting figures rather than boilerplate. The two MedMax \\
     columns are over the SAME contracts -- the ones that kept a USD amount -- so MedMaxRule can \\
     never exceed MedMaxNaive, and where they differ the largest figure the contract named was a par \\
     value or a zero. PctAllPar is the contracts excluded from both: they named a USD figure and the \\
     filters removed every one of it."
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


#' What the released file holds
#' @param .tab Tibble from mny_release().
#' @return Invisibly the summary.
mny_report_release <- function(.tab) {
  if (FALSE) .tab <- tab_release

  cli::cli_h2("The released file")

  out_ <- tibble::tibble(
    Item = c("Contracts",
             "Naming a USD amount",
             "Naming a non-USD amount",
             "Naming neither",
             "Where a filter removed something"),
    N    = c(nrow(.tab),
             sum(!is.na(.tab$MoneyMaxUSD)),
             sum(!is.na(.tab$MoneyMaxOther)),
             sum(.tab$NAmounts == 0L),
             sum(.tab$NDroppedZero + .tab$NDroppedPar > 0L))
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$N / nrow(.tab)))

  tbl_say(.tab = out_, .title = "One row per contract, by what it named")

  cli::cli_alert_info(
    "EVERY CONTRACT GETS A ROW, including the ones naming no figure at all -- those carry zero spans \\
     and a missing maximum, so a mean over the file divides by the sample rather than by the \\
     documents that named something. The naive columns are released beside the rule so the \\
     comparison every table makes can be reproduced from the file."
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
#' @param .tab Tibble from mny_dictionary().
#' @return Invisibly .tab.
mny_report_dictionary <- function(.tab) {
  if (FALSE) .tab <- tab_dict

  cli::cli_h2("What every column means")
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
#' @param .release Tibble from mny_release().
#' @param .side Tibble from mny_par_side().
#' @return Invisibly the table.
mny_report_headline <- function(.release, .side) {
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
    "Rows in the released file",               format(nrow(.release), big.mark = ",")
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


# 8. Figures -------------------------------------------------------------------------------------------------------------

#' The rule against the unfiltered count, by contract type
#' @param .tab Tibble from mny_table_naive().
#' @return A ggplot.
mny_plot_naive <- function(.tab) {
  if (FALSE) .tab <- tab_naive

  .tab |>
    dplyr::filter(.data$Class != "All") |>
    dplyr::select("Class", Unfiltered = "MeanNaive", Rule = "MeanRule") |>
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
#' @param .release Tibble from mny_release().
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
#' @param .release Tibble from mny_release().
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
