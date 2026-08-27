# 04B3-Rules-DATE: when a contract runs ----------------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# A contract states dates and it states periods. This turns them into a start, an end and a duration:
# the start is the latest date at or before the filing, and the end comes from a four-rung cascade.
#
# TWO FILES COME OUT AND BOTH ARE LONG
#   date_spans.parquet  one row per date span, parsed or not
#   term_spans.parquet  one row per stated period
# Neither is a per-contract table. dte_collapse() runs the cascade over them and the export calls it;
# this document scores it, reports it and checks its shape. A cascade stored as data cannot be
# changed without re-releasing, and this one has four rungs a reader may well want to reorder.
#
# WHAT STOPS BEING STORED AND BECOMES A QUERY
#   DurationSource  which rung the two files support
#   NaiveYears      the last rung alone -- the farthest future date, uncapped
#   The 30-year cap a filter over the released durations rather than a decision baked in
# The last one matters most. The published standard deviation of 40.78 against a stated cap of 30 is
# impossible on [0, 30], and with the cap as a query a reader can compute the distribution at any cap
# or at none, from the file, rather than taking the sentence that says it was applied.
#
# THE PERIOD KIND SHIPS AND THE RULE DOES NOT CHANGE
# A contract states 4.66 periods on average, and the longest is not always the term: 7.0% of released
# durations come from a cure or notice window of thirty days rather than from the life of the
# agreement. Two candidate fixes were measured and both made it worse -- a cue filter swapped thirty
# days for fifteen on half the affected contracts, and a floor needs a number nothing here justifies.
# So the rule is unchanged and PeriodKind is a column: anyone excluding remedy deadlines writes one
# filter over the term file, and the decision does not have to be right today.
#
# PeriodKind HAS GOOD PRECISION AND POOR RECALL, WHICH IS WHY IT IS A DIAGNOSTIC AND NOT A RULE. Where
# it fires it is right -- cure and notice both carry a median of thirty days -- but 68% of periods
# fall through to "other", so a positive filter on "term" would select on a label missing for
# two-thirds of the data. It is released to make the problem countable, not to solve it.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.


# 1. Vocabulary ----------------------------------------------------------------------------------------------------------


# 1. Vocabulary ----------------------------------------------------------------------------------------------------------

plot_register_levels(
  .key    = "DurationSource",
  .levels = c("term", "open", "cue", "maxdate", "none"),
  .short  = c("term", "open", "cue", "maxdate", "none")
)

plot_register_levels(
  .key    = "StartSource",
  .levels = c("signed", "filed"),
  .short  = c("signed", "filed")
)

plot_register_levels(
  .key    = "DurationDropped",
  .levels = c("kept", "negative", "capped", "no end"),
  .short  = c("kept", "negative", "capped", "no end")
)

plot_register_levels(
  .key    = "TermKind",
  .levels = c("UnitTerm", "ContinueFor", "PeriodOf", "UnitPeriod", "Anniversary", "OpenEnded"),
  .short  = c("unit term", "continue", "period of", "unit period", "anniv", "open")
)


plot_register_levels(
  .key    = "PeriodKind",
  .levels = c("term", "survival", "cure", "notice", "other"),
  .short  = c("term", "survival", "cure", "notice", "other")
)


# 2b. What the words beside a period say it is ----------------------------------------------------------------------------
# A DIAGNOSTIC LIST, NOT A RULE. Every period carries 160 characters of stored context either side,
# and these terms say what kind of period the drafter was writing. The list has good precision and
# poor recall -- where it fires the medians are exactly right, and two thirds of periods match
# nothing -- so it is released as a column and no rule conditions on it.

.dte_period_cue <- list(
  survival = "SURVIV",
  cure     = "CURE|REMEDY|DEFAULT",
  notice   = "NOTICE",
  term     = "TERM OF|INITIAL TERM|SHALL CONTINUE|EXPIRE|TERMINAT"
)


# 2. The one cue list ----------------------------------------------------------------------------------------------------
# TEN TERMS AND A HIT RATE FOR EVERY ONE. The list is not defended by argument: the report prints how
# often each term fires and on what share of future dates, and a term firing everywhere without
# marking a termination is a term to drop rather than to explain.
#
# THROUGH and UNTIL are the entries under suspicion -- both are common enough to appear beside any
# date at all -- and the per-term table is where that gets settled rather than asserted.

.dte_end_cue <- c("EXPIR", "TERMINAT", "MATURIT", "UNTIL", "THROUGH", "SHALL END", "ENDS ON",
                  "ENDING", "TERM SHALL", "TERM OF THIS")


# 3. Input ---------------------------------------------------------------------------------------------------------------

#' Build one duration specification
#'
#' TWO DIALS AND A CAP, DOWN FROM NINE. What went: the family and its pooled arm, because one engine
#' is a decision rather than a parameter; RequireYear, because it guards a LexNLP failure mode that
#' matcon cannot have; the head restriction on the start, because region separates the head from the
#' middle and nothing else; the cue window, because the cue is a stored column; and TermKinds and
#' TermFloor, both replaced by taking the longest stated period.
#'
#' @param .start Character, "latest" or "filed". LATEST is the latest date at or before the filing.
#'   FILED uses the filing date for every contract, which is the published definition's start.
#' @param .end Character, "term", "cue" or "any". TERM walks the full cascade. CUE skips the stated
#'   term. ANY takes the farthest future date for every contract -- THE NAIVE BASELINE, and the
#'   definition Table 3 was built with.
#' @param .cap_years Numeric. Durations above this are DROPPED rather than winsorised, because a
#'   duration of exactly the cap that is not one is worse than a missing value. Inf disables it.
#' @param .label Character or NULL. Overrides the generated label.
#' @return A named list carrying the specification.
dte_spec <- function(.start = "latest", .end = "term", .cap_years = 30, .label = NULL) {
  if (FALSE) {
    .start     <- "latest"
    .end       <- "term"
    .cap_years <- 30
    .label     <- NULL
  }

  if (!.start %in% c("latest", "filed")) cli::cli_abort("{.arg .start} must be latest or filed.")
  if (!.end %in% c("term", "cue", "any")) cli::cli_abort("{.arg .end} must be term, cue or any.")

  list(
    Start    = .start,
    End      = .end,
    CapYears = .cap_years,
    Label    = if (!is.null(.label)) {
      .label
    } else {
      paste0(.start, " / ", .end, " / ",
             if (is.infinite(.cap_years)) "uncapped" else paste0(.cap_years, "y"))
    }
  )
}


#' Load the DATE spans, and keep the ones that parsed
#'
#' matcon parses 99.6% of what it emits, so the unparsed remainder is a rounding error rather than a
#' population. That is worth saying because the same is not true of every producer: spaCy emits no
#' date at all, and LexNLP's grammar supplies a year from the clock where the text wrote none.
#'
#' NO CLUSTERING, AND THAT IS A CORRECTION. An earlier version collapsed each document to one row per
#' calendar date, keeping the earliest occurrence. It bought nothing -- the duration is a max() and a
#' min() over VALUES, which do not care how often a value appears -- and it created a defect: the
#' termination cue was then tested at the EARLIEST occurrence, so "shall expire on December 31, 2020"
#' was invisible whenever that date had already appeared in the preamble, which in a contract is the
#' normal case.
#'
#' CueBefore ARRIVES WITH THE SPAN and is not selected away, which is what lets dte_describe() test
#' the cue without opening a document.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .lens Tibble: DocID and DocLen.
#' @param .family Character. The family supplying dates.
#' @param .quiet Logical. Suppress the count message.
#' @return Tibble: one row per span, with DateValue and CueBefore.
dte_load <- function(.dir_store, .lens, .family = "matcon", .quiet = FALSE) {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .lens      <- tab_lens
    .family    <- "matcon"
    .quiet     <- FALSE
  }

  ent_load_entity(
    .dir_store = .dir_store,
    .family    = .family,
    .entity    = "DATE",
    .lens      = .lens,
    .extras    = ent_extras(.family, "DATE"),   # DateValue, and the two cue columns
    .quiet     = .quiet
  ) |>
    dplyr::mutate(
      DateValue = suppressWarnings(anytime::anydate(as.character(.data$DateValue))),
      Parsed    = !is.na(.data$DateValue)
    )
}


#' Load the stated periods, at span grain
#'
#' SPANS AND NOT A COLLAPSE, and that is the change. This function used to pick the longest period,
#' the first beside it and the counts -- so a function named "load" was making this document's second
#' largest decision, invisibly, inside a read. dte_terms_collapse() does the choosing now, over the
#' released file, where a reader can change it.
#'
#' THE PATTERN VOCABULARY IS CHECKED AT THE READ. dateregex names its own patterns and every table
#' orders by them, so a pattern the vocabulary does not name has to stop the render rather than sort
#' silently to the end.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .lens Tibble from ent_doc_lens().
#' @param .quiet Logical. Suppress the count message.
#' @return Tibble: one row per period span, with the cue window the release classifies from.
dte_load_terms <- function(.dir_store, .lens, .quiet = FALSE) {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .lens      <- tab_lens
    .quiet     <- FALSE
  }

  out_ <- ent_load_entity(
    .dir_store = .dir_store,
    .family    = "matcon",                      # the only family emitting a term at all
    .entity    = "TERM",
    .lens      = .lens,
    .extras    = ent_extras("matcon", "TERM"),  # TermN, TermUnit, TermYears
    .quiet     = .quiet
  ) |>
    dplyr::mutate(
      TermKind  = .data$LabelRaw,
      TermYears = as.numeric(.data$TermYears),
      TermN     = as.numeric(.data$TermN),
      IsOpen    = .data$TermKind == "OpenEnded"
    )

  bad_ <- setdiff(unique(out_$TermKind), plot_levels("TermKind"))
  if (length(bad_) > 0L) {
    cli::cli_abort(c(
      "dateregex emits {length(bad_)} TERM pattern{?s} the vocabulary does not name: \\
       {paste(bad_, collapse = ', ')}.",
      "i" = "Register {length(bad_)} name{?s} in TermKind so every table can order them."
    ))
  }

  if (!.quiet) {
    n_doc_ <- dplyr::n_distinct(out_$DocID)
    cli::cli_alert_success(
      "{format(nrow(out_), big.mark = ',')} {cli::qty(nrow(out_))}period span{?s} in \\
       {format(n_doc_, big.mark = ',')} {cli::qty(n_doc_)}document{?s}."
    )
    cli::cli_alert_info(
      "{tbl_num(nrow(out_) / max(n_doc_, 1L))} stated periods per contract that states any. \\
       Choosing the longest is choosing among that many, which is why the choice is a query over \\
       the released file rather than a step inside this read."
    )
  }
  out_
}


# 4. Description ---------------------------------------------------------------------------------------------------------

#' The gap to the filing date, and the termination cue -- per span
#'
#' THE CUE IS A COLUMN READ, AND IT USED TO BE A TEXT READ. This function opened 04A's canonical text
#' and cut 120 characters before every date; CueBefore carries 160 raw characters stored at
#' extraction, where the document and the offsets were already in one scope. The window is narrowed
#' here rather than at extraction so that refining the cue list never means re-extracting.
#'
#' AT SPAN LEVEL RATHER THAN AFTER A COLLAPSE, because a clause states the event and then the date --
#' "shall expire on December 31, 2020" -- and a date written twice must be tested at the occurrence
#' beside the clause rather than at the first one in the document.
#'
#' NORMALISED HERE, NOT IN PYTHON. The column is stored raw so that normalisation stays in R where
#' the matching lives, which makes the comparison exact by construction rather than a test of whether
#' Python's upper() and stringi's agree.
#'
#' @param .dates Tibble from dte_load(), parsed rows only.
#' @param .keys Tibble from ent_anchor_keys(). Supplies DateFiled.
#' @param .win Integer. Characters of the stored cue window this rule reads.
#' @return .dates with GapDays, Side, Before, CueHit and HasEndCue added.
dte_describe <- function(.dates, .keys, .win = 120L) {
  if (FALSE) {
    .dates <- tab_parsed
    .keys  <- tab_keys
    .win   <- 120L
  }

  if (!"CueBefore" %in% names(.dates)) {
    cli::cli_abort(c(
      "The DATE spans carry no CueBefore, so the termination cue cannot be tested.",
      "i" = "matcon stores it at extraction; a store written before that change has to be rebuilt."
    ))
  }

  # WHICH CUE, NOT WHETHER A CUE. The boolean was all this function used to produce, so a reader
  # wanting to know whether THROUGH was carrying the rung had to re-scan the text. Returning the term
  # makes that a group-by over the released file, and the boolean falls out of it -- one pass rather
  # than two, and no way for the two answers to disagree.
  #
  # FIRST BY LIST ORDER where several match. The list is short and the report prints a per-term hit
  # rate independently, so this column is for attribution rather than for counting.
  which_ <- function(.txt, .terms) {
    out_ <- rep(NA_character_, length(.txt))
    for (.t in .terms) {
      hit_ <- is.na(out_) & stringi::stri_detect_fixed(.txt, .t)
      out_[hit_] <- .t
    }
    out_
  }

  .dates |>
    dplyr::left_join(dplyr::select(.keys, DocID, DateFiled), by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      GapDays = as.integer(.data$DateValue - .data$DateFiled),
      Side    = dplyr::case_when(
        is.na(.data$GapDays) ~ NA_character_,
        .data$GapDays < 0L   ~ "before filing",
        .data$GapDays > 0L   ~ "after filing",
        .default             = "same day"
      ),
      # The last .win characters of the stored window, uppercased and collapsed. stri_sub with a
      # negative from() counts back from the end, which is what makes this the characters NEAREST
      # the span rather than the start of the stored window.
      Before = stringi::stri_trans_toupper(
        stringi::stri_replace_all_regex(
          stringi::stri_sub(dplyr::coalesce(.data$CueBefore, ""), from = -.win), "\\s+", " "
        )
      ),
      CueHit    = which_(.data$Before, .dte_end_cue),
      HasEndCue = !is.na(.data$CueHit)
    )
}


# 5. The two releases ----------------------------------------------------------------------------------------------------

#' The release: one row per date span, parsed or not
#'
#' THE UNPARSED ROWS STAY IN THE FILE. A span the parser could not read is a measurement -- it says
#' the extractor found something date-shaped that no rule could use -- and dropping it would make the
#' parse rate uncomputable from the release. Same argument as the refused places in 04B2.
#'
#' THE CUE IS A COLUMN AND SO IS THE TERM THAT FIRED IT. HasEndCue says a termination cue sits in the
#' stored window; CueHit says which one. THROUGH is the entry under suspicion -- top by volume and
#' highest on both share-of-cued and share-of-future, which is the pattern of a common word rather
#' than a cue -- and with the term in the file, testing a shorter cue list is a query rather than a
#' re-run.
#'
#' @param .dates Tibble from dte_describe().
#' @return Tibble: one row per date span.
dte_release_dates <- function(.dates) {
  if (FALSE) .dates <- tab_desc

  .dates |>
    dplyr::transmute(
      .data$DocID,
      DateStart = as.integer(.data$Start),
      DateStop  = as.integer(.data$Stop),
      DateText  = .data$Span,
      .data$DateValue,
      .data$Parsed,
      GapDays   = as.integer(.data$GapDays),
      .data$Side,
      .data$HasEndCue,
      .data$CueHit
    ) |>
    dplyr::arrange(.data$DocID, .data$DateStart)
}


#' The release: one row per stated period
#'
#' EVERY PERIOD, NOT THE LONGEST. A contract states 4.66 of them on average, and which one is the
#' term is a decision rather than a fact. Releasing all of them means the decision lives in
#' dte_collapse() where it can be changed, instead of in a file where it cannot.
#'
#' PeriodKind IS A DIAGNOSTIC. It says what the words beside a period call it, and it exists so that
#' the seven per cent of released durations that are really a remedy deadline can be counted and
#' filtered by anyone who wants to. No rule conditions on it.
#'
#' @param .terms Tibble from dte_release_terms().
#' @return Tibble: one row per period.
dte_release_terms <- function(.terms) {
  if (FALSE) .terms <- tab_terms

  up_ <- stringi::stri_trans_toupper(dplyr::coalesce(.terms$CueBefore, ""))

  kind_ <- rep("other", length(up_))
  cue_  <- rep(NA_character_, length(up_))
  # ORDER IS THE PRIORITY, most specific first: a clause saying a right survives termination carries
  # both TERMINAT and SURVIV, and it is a survival clause.
  for (nm_ in names(.dte_period_cue)) {
    hit_ <- kind_ == "other" & stringi::stri_detect_regex(up_, .dte_period_cue[[nm_]])
    kind_[hit_] <- nm_
    cue_[hit_]  <- stringi::stri_extract_first_regex(up_[hit_], .dte_period_cue[[nm_]])
  }

  .terms |>
    dplyr::transmute(
      .data$DocID,
      TermStart  = as.integer(.data$Start),
      TermStop   = as.integer(.data$Stop),
      TermText   = .data$Span,
      .data$TermKind,
      TermN      = as.numeric(.data$TermN),
      .data$TermUnit,
      TermYears  = as.numeric(.data$TermYears),
      .data$IsOpen,
      PeriodKind = kind_,
      PeriodCue  = cue_
    ) |>
    dplyr::arrange(.data$DocID, .data$TermStart)
}


# 6. The collapse --------------------------------------------------------------------------------------------------------

#' One row per contract per document's periods, from the term file
#'
#' THE COLLAPSE THAT USED TO SIT INSIDE THE LOADER. dte_load_terms() picked the longest period, the
#' first beside it and the counts, so a function named "load" was making the document's second
#' largest decision. It returns spans now and this does the choosing, which is what lets the choice
#' be changed by a query.
#'
#' THE LONGEST, AND THE FIRST BY POSITION BESIDE IT. The second is not used by the rule; it is what
#' the comparison in Selection is measured against, and computing both here means the two answers
#' come from one pass over one table rather than from two that could diverge.
#'
#' @param .terms Tibble from dte_release_terms().
#' @return Tibble: one row per document that stated any period.
dte_terms_collapse <- function(.terms) {
  if (FALSE) .terms <- tab_terms_rel

  open_ <- .terms |>
    dplyr::filter(.data$IsOpen) |>
    dplyr::summarise(OpenKind = dplyr::first(.data$TermKind), .by = DocID)

  closed_ <- dplyr::filter(.terms, !.data$IsOpen, !is.na(.data$TermYears))

  long_ <- closed_ |>
    dplyr::arrange(.data$DocID, dplyr::desc(.data$TermYears), .data$TermStart) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::select("DocID", "TermKind", "TermN", "TermUnit", "TermYears", "TermStart",
                  TermSpan = "TermText", LongestKind = "PeriodKind")

  first_ <- closed_ |>
    dplyr::arrange(.data$DocID, .data$TermStart) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::select("DocID", FirstYears = "TermYears", FirstKind = "TermKind")

  seen_ <- closed_ |>
    dplyr::summarise(
      NTerms   = dplyr::n(),
      MaxYears = max(.data$TermYears),
      MinYears = min(.data$TermYears),
      NRemedy  = sum(.data$PeriodKind %in% c("cure", "notice")),
      .by = DocID
    )

  long_ |>
    dplyr::left_join(first_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(seen_,  by = dplyr::join_by(DocID)) |>
    dplyr::full_join(open_,  by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      NTerms   = as.integer(dplyr::coalesce(.data$NTerms, 0L)),
      NRemedy  = as.integer(dplyr::coalesce(.data$NRemedy, 0L)),
      IsOpen   = !is.na(.data$OpenKind),
      Disagree = !is.na(.data$TermYears) & !is.na(.data$FirstYears) &
                 .data$TermYears != .data$FirstYears
    )
}


#' Start, end and duration -- the cascade, run over the two released files
#'
#' DEFINED HERE AND WRITTEN NOWHERE. This document scores it, reports it and checks its shape; the
#' export calls it to materialise one row per contract. A cascade stored as data is a cascade that
#' cannot be reordered without re-releasing.
#'
#' THE FOUR RUNGS ARE FOUR KINDS OF EVIDENCE, not four attempts at one thing. A stated term is what
#' the contract SAYS about itself. An open-ended clause says it will not end, which is a finding
#' rather than a gap. A cued date is a reading of the words beside a date. The farthest future date
#' is a maximum over dates the contract mentions for reasons it never states, and it is the naive
#' definition kept as the last rung so coverage does not fall to nothing.
#'
#' AN OPEN-ENDED TERM OUTRANKS THE FARTHEST FUTURE DATE AND NOT A STATED ONE. A document saying both
#' "five year term" and "shall continue until terminated" has stated a length, and the length is the
#' answer; a document saying only the second has stated that it HAS no length, which is different
#' from having said nothing.
#'
#' THE NAIVE DURATION IS COMPUTED FOR EVERY DOCUMENT whatever the specification asks for, because it
#' is the comparison every table is read against.
#'
#' THE CAP DROPS RATHER THAN WINSORISES, and the reason is recorded in DurationDropped. A duration of
#' exactly thirty years that is not one is worse than a missing value.
#'
#' @param .dates Tibble from dte_release_dates().
#' @param .terms Tibble from dte_release_terms().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .spec List from dte_spec().
#' @return Tibble: one row per document in .keys.
dte_collapse <- function(.dates, .terms, .keys, .spec) {
  if (FALSE) {
    .dates <- tab_dates_rel
    .terms <- tab_terms_rel
    .keys  <- tab_keys
    .spec  <- .lP$Params$Spec
  }

  terms_ <- dte_terms_collapse(.terms = .terms)
  src_   <- dplyr::filter(.dates, .data$Parsed)

  signed_ <- src_ |>
    dplyr::filter(!is.na(.data$GapDays), .data$GapDays <= 0L) |>
    dplyr::slice_max(order_by = .data$GapDays, n = 1L, by = DocID, with_ties = FALSE) |>
    dplyr::select(DocID, DateSigned = DateValue)

  fut_ <- dplyr::filter(src_, !is.na(.data$GapDays), .data$GapDays > 0L)

  end_any_ <- fut_ |>
    dplyr::slice_max(order_by = .data$DateValue, n = 1L, by = DocID, with_ties = FALSE) |>
    dplyr::select(DocID, EndAny = DateValue)

  end_cue_ <- fut_ |>
    dplyr::filter(.data$HasEndCue) |>
    dplyr::slice_max(order_by = .data$DateValue, n = 1L, by = DocID, with_ties = FALSE) |>
    dplyr::select(DocID, EndCue = DateValue)

  seen_ <- src_ |>
    dplyr::summarise(
      NDates  = dplyr::n_distinct(.data$DateValue),
      NFuture = dplyr::n_distinct(.data$DateValue[.data$GapDays > 0L]),
      NCued   = dplyr::n_distinct(.data$DateValue[.data$HasEndCue & .data$GapDays > 0L]),
      .by = DocID
    )

  .keys |>
    dplyr::select(DocID, DateFiled) |>
    dplyr::left_join(seen_,    by = dplyr::join_by(DocID)) |>
    dplyr::left_join(signed_,  by = dplyr::join_by(DocID)) |>
    dplyr::left_join(end_any_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(end_cue_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(
      dplyr::select(terms_, DocID, TermKind, TermYears, TermN, TermUnit, NTerms, NRemedy,
                    IsOpen, OpenKind, FirstYears, LongestKind, Disagree),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::mutate(
      dplyr::across(c(NDates, NFuture, NCued, NTerms, NRemedy),
                    \(.x) as.integer(dplyr::coalesce(.x, 0L))),
      IsOpen    = dplyr::coalesce(.data$IsOpen, FALSE),
      Disagree  = dplyr::coalesce(.data$Disagree, FALSE),
      DateStart = if (identical(.spec$Start, "filed")) {
        .data$DateFiled
      } else {
        dplyr::coalesce(.data$DateSigned, .data$DateFiled)
      },
      StartSource = dplyr::case_when(
        identical(.spec$Start, "filed") ~ "filed",
        !is.na(.data$DateSigned)        ~ "signed",
        .default                        = "filed"
      ),
      UseTerm   = identical(.spec$End, "term") & !is.na(.data$TermYears),
      IsOpenEnd = !.data$UseTerm & identical(.spec$End, "term") & .data$IsOpen,
      UseCue    = !.data$UseTerm & !.data$IsOpenEnd & .spec$End %in% c("term", "cue") &
                  !is.na(.data$EndCue),
      EndTerm   = .data$DateStart + round(.data$TermYears * 365.25),
      DateEnd   = dplyr::case_when(
        .data$UseTerm   ~ .data$EndTerm,
        .data$IsOpenEnd ~ lubridate::NA_Date_,
        .data$UseCue    ~ .data$EndCue,
        .default        = .data$EndAny
      ),
      DurationSource = dplyr::case_when(
        .data$UseTerm         ~ "term",
        .data$IsOpenEnd       ~ "open",
        .data$UseCue          ~ "cue",
        !is.na(.data$DateEnd) ~ "maxdate",
        .default              = "none"
      ),
      # A stated term that REPLACED a future date the document also carried, rather than filling a
      # gap. Defensible -- a term is what the contract says about itself -- but it is a large silent
      # substitution and this is what makes it countable.
      TermOverrode  = .data$UseTerm & !is.na(.data$EndAny),
      TermFilledGap = .data$UseTerm & is.na(.data$EndAny),
      RawYears      = as.numeric(.data$DateEnd - .data$DateStart) / 365.25,
      NaiveYears    = as.numeric(.data$EndAny - .data$DateStart) / 365.25,
      IsNeg         = !is.na(.data$RawYears) & .data$RawYears < 0,
      IsCapped      = !is.na(.data$RawYears) & .data$RawYears > .spec$CapYears,
      DurationYears = dplyr::if_else(
        !is.na(.data$RawYears) & !.data$IsNeg & !.data$IsCapped, .data$RawYears, NA_real_
      ),
      DurationDropped = dplyr::case_when(
        !is.na(.data$DurationYears) ~ "kept",
        .data$IsNeg                 ~ "negative",
        .data$IsCapped              ~ "capped",
        .default                    = "no end"
      )
    )
}


#' Apply the rule end to end
#'
#' ONE ENTRY POINT, AND 04D CALLS EXACTLY THIS. The two releases come out; the collapse is returned
#' beside them because this document reports on it, and the export computes it again from the files.
#'
#' @param .dates Tibble from dte_describe().
#' @param .terms Tibble from dte_load_terms(), at span grain.
#' @param .keys Tibble from ent_anchor_keys().
#' @param .spec List from dte_spec().
#' @return A list: Spec, Dates, Terms, Duration.
dte_apply <- function(.dates, .terms, .keys, .spec) {
  if (FALSE) {
    .dates <- tab_desc
    .terms <- tab_terms
    .keys  <- tab_keys
    .spec  <- .lP$Params$Spec
  }

  dates_ <- dte_release_dates(.dates = .dates)
  terms_ <- dte_release_terms(.terms = .terms)

  list(
    Spec     = .spec,
    Dates    = dates_,
    Terms    = terms_,
    Duration = dte_collapse(.dates = dates_, .terms = terms_, .keys = .keys, .spec = .spec)
  )
}


#' What every column of the date file means
#' @param .tab Tibble from dte_release_dates().
#' @return Tibble: Column, Grain, Meaning.
dte_dictionary_dates <- function(.tab) {
  if (FALSE) .tab <- tab_dates_rel

  dict_ <- tibble::tribble(
    ~Column,     ~Grain,     ~Meaning,
    "DocID",     "document", "the contract",
    "DateStart", "date",     "offset of the date span, into 04A's canonical text",
    "DateStop",  "date",     "offset one past its last character",
    "DateText",  "date",     "the surface form, raw: it slices from the two offsets exactly",
    "DateValue", "date",     "the parsed date; null where the parser could not read it",
    "Parsed",    "date",     "did the parser read it; an unparsed span is a measurement",
    "GapDays",   "date",     "days from the filing date; negative is before it",
    "Side",      "date",     "before filing, same day, or after filing",
    "HasEndCue", "date",     "a termination cue sits in the 160 characters before the span",
    "CueHit",    "date",     "which cue fired, so a shorter list is a query rather than a re-run"
  )

  undoc_  <- setdiff(names(.tab), dict_$Column)
  unseen_ <- setdiff(dict_$Column, names(.tab))
  say_    <- function(.x) if (length(.x) == 0L) "none" else paste(.x, collapse = ", ")

  if (length(undoc_) > 0L || length(unseen_) > 0L) {
    cli::cli_abort(c(
      "The date dictionary and the released file disagree.",
      "x" = "In the file and undocumented: {say_(undoc_)}.",
      "x" = "Documented and not in the file: {say_(unseen_)}."
    ))
  }

  dict_[match(names(.tab), dict_$Column), ]
}


#' What every column of the term file means
#' @param .tab Tibble from dte_release_terms().
#' @return Tibble: Column, Grain, Meaning.
dte_dictionary_terms <- function(.tab) {
  if (FALSE) .tab <- tab_terms_rel

  dict_ <- tibble::tribble(
    ~Column,      ~Grain,     ~Meaning,
    "DocID",      "document", "the contract",
    "TermStart",  "period",   "offset of the period, into 04A's canonical text",
    "TermStop",   "period",   "offset one past its last character",
    "TermText",   "period",   "the surface form, raw",
    "TermKind",   "period",   "which dateregex pattern matched; a phrasing, not a meaning",
    "TermN",      "period",   "the number the contract wrote",
    "TermUnit",   "period",   "the unit it wrote it in",
    "TermYears",  "period",   "that period in years; null on an open-ended clause",
    "IsOpen",     "period",   "the clause states no length -- a finding, not a gap",
    "PeriodKind", "period",   "term, survival, cure, notice or other -- DIAGNOSTIC, no rule uses it",
    "PeriodCue",  "period",   "the word that fired PeriodKind, so the label is auditable"
  )

  undoc_  <- setdiff(names(.tab), dict_$Column)
  unseen_ <- setdiff(dict_$Column, names(.tab))
  say_    <- function(.x) if (length(.x) == 0L) "none" else paste(.x, collapse = ", ")

  if (length(undoc_) > 0L || length(unseen_) > 0L) {
    cli::cli_abort(c(
      "The term dictionary and the released file disagree.",
      "x" = "In the file and undocumented: {say_(undoc_)}.",
      "x" = "Documented and not in the file: {say_(unseen_)}."
    ))
  }

  dict_[match(names(.tab), dict_$Column), ]
}


#' What the periods are, by kind
#'
#' THE TABLE THAT SAYS WHY PeriodKind SHIPS AND WHY NO RULE USES IT. The medians are the evidence for
#' the cue list: cure and notice both land on thirty days, which is what those words mean. AsLongest
#' is the cost: the share of contracts whose RELEASED duration comes from a period of that kind, and
#' cure plus notice there is the seven per cent this document cannot currently fix.
#'
#' @param .terms Tibble from dte_release_terms().
#' @return Tibble: one row per PeriodKind.
dte_table_period <- function(.terms) {
  if (FALSE) .terms <- tab_terms_rel

  closed_ <- dplyr::filter(.terms, !.data$IsOpen, !is.na(.data$TermYears))
  if (nrow(closed_) == 0L) return(tibble::tibble())

  long_ <- closed_ |>
    dplyr::arrange(.data$DocID, dplyr::desc(.data$TermYears), .data$TermStart) |>
    dplyr::slice_head(n = 1L, by = DocID)

  closed_ |>
    dplyr::summarise(
      Periods  = dplyr::n(),
      MedYears = stats::median(.data$TermYears),
      MedDays  = round(stats::median(.data$TermYears) * 365.25),
      .by = PeriodKind
    ) |>
    dplyr::left_join(
      dplyr::summarise(long_, AsLongest = dplyr::n(), .by = PeriodKind),
      by = dplyr::join_by(PeriodKind)
    ) |>
    dplyr::mutate(
      AsLongest  = as.integer(dplyr::coalesce(.data$AsLongest, 0L)),
      PctPeriod  = .data$Periods / sum(.data$Periods),
      PctLongest = .data$AsLongest / nrow(long_)
    ) |>
    dplyr::arrange(plot_factor(.data$PeriodKind, .key = "PeriodKind"))
}


# 7. Evidence ------------------------------------------------------------------------------------------------------------

#' Reduce one specification to a comparable row
#'
#' SdOverCap IS THE DIAGNOSTIC THIS DOCUMENT WAS BUILT AROUND. On non-negative support under a
#' correctly applied cap the standard deviation cannot exceed half the cap. Table 3 Panel B reports
#' 40.78 against a stated cap of 30, a ratio of 1.36, which no distribution on [0, 30] can produce --
#' so the cap was not applied to what was tabulated, or something was in the wrong unit. Every row
#' below carries the ratio, and it cannot exceed one half.
#'
#' @param .dur Tibble from dte_collapse().
#' @param .spec List from dte_spec().
#' @return One-row tibble.
dte_row <- function(.dur, .spec) {
  if (FALSE) {
    .dur  <- tab_dur
    .spec <- .lP$Params$Spec
  }

  kept_ <- .dur$DurationYears[!is.na(.dur$DurationYears)]

  tibble::tibble(
    Spec      = .spec$Label,
    Start     = .spec$Start,
    End       = .spec$End,
    Cap       = .spec$CapYears,
    Docs      = nrow(.dur),
    PctKept   = mean(!is.na(.dur$DurationYears)),
    PctTerm   = mean(.dur$DurationSource == "term"),
    PctMax    = mean(.dur$DurationSource == "maxdate"),
    Mean      = .dte_stat_or_na(.x = kept_, .f = mean),
    Median    = .dte_stat_or_na(.x = kept_, .f = stats::median),
    Sd        = .dte_stat_or_na(.x = kept_, .f = stats::sd),
    SdOverCap = if (is.infinite(.spec$CapYears) || length(kept_) < 2L) {
      NA_real_
    } else {
      stats::sd(kept_) / .spec$CapYears
    }
  )
}


#' Every specification, over one set of dates and terms
#'
#' THE NAIVE BASELINE IS A ROW HERE, not a separate computation: "filed / any / uncapped" is the
#' published definition exactly, and "latest / any" is the same maximum measured from this document's
#' start. The rule is the row that walks the full cascade.
#'
#' @param .dates Tibble from dte_describe().
#' @param .terms Tibble from dte_release_terms().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .specs List of specifications.
#' @param .quiet Logical. Suppress the progress bar.
#' @return Tibble: one row per specification.
dte_sweep <- function(.dates, .terms, .keys, .specs, .quiet = FALSE) {
  if (FALSE) {
    .dates <- tab_desc
    .terms <- tab_terms
    .keys  <- tab_keys
    .specs <- .lP$Params$Specs
    .quiet <- FALSE
  }

  purrr::map(.specs, function(.s) {
    dte_row(.dur = dte_collapse(.dates = .dates, .terms = .terms, .keys = .keys, .spec = .s),
            .spec = .s)
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


#' A statistic, or missing where there is nothing to compute it over
#'
#' @param .x Numeric vector, possibly empty.
#' @param .f Function taking a numeric vector.
#' @return The statistic, or NA_real_ where .x is too short.
.dte_stat_or_na <- function(.x, .f) {
  if (FALSE) {
    .x <- numeric(0)
    .f <- mean
  }
  if (length(.x) < 2L) NA_real_ else .f(.x)
}


#' How often each termination cue fires, and on what
#'
#' THE CUE LIST IS NOT DEFENDED BY ARGUMENT. A term firing beside a large share of ALL future dates
#' is not marking terminations, it is marking dates -- and THROUGH and UNTIL are the two entries under
#' suspicion, because both are common enough to sit beside anything. PctOfCued against PctOfFuture is
#' what separates a cue from a common word: a useful term is high on the first and low on the second.
#'
#' @param .dates Tibble from dte_describe().
#' @return Tibble: one row per cue term.
dte_cue_hits <- function(.dates) {
  if (FALSE) .dates <- tab_desc

  fut_  <- dplyr::filter(.dates, .data$Parsed, !is.na(.data$GapDays), .data$GapDays > 0L)
  cued_ <- sum(fut_$HasEndCue)

  purrr::map(.dte_end_cue, function(.t) {
    hit_ <- stringi::stri_detect_fixed(fut_$Before, .t)
    tibble::tibble(
      Cue         = .t,
      Spans       = sum(hit_),
      Docs        = dplyr::n_distinct(fut_$DocID[hit_]),
      PctOfFuture = .dte_share_or_na(.x = hit_),
      PctOfCued   = if (cued_ == 0L) NA_real_ else sum(hit_) / cued_
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(dplyr::desc(.data$Spans))
}


#' A share, or missing where there is nothing to take a share of
#'
#' @param .x Logical vector, possibly empty.
#' @return The mean, or NA_real_ where .x is empty.
.dte_share_or_na <- function(.x) {
  if (FALSE) .x <- logical(0)
  if (length(.x) == 0L) NA_real_ else mean(.x)
}


#' The longest stated term against the first one written
#'
#' THE ONE TRADE THIS DOCUMENT MAKES, MEASURED. Taking the longest releases the contract's duration
#' where a cure period was written first; it releases a survival clause where one outlasts the term.
#' Neither can be settled by a count, so this reports how often the two rules disagree and by how
#' much, and the reading block beside it is what says which answer was right.
#'
#' @param .terms Tibble from dte_release_terms().
#' @return Tibble: one row per outcome.
dte_term_compare <- function(.terms) {
  if (FALSE) .terms <- tab_terms

  src_ <- dplyr::filter(.terms, !is.na(.data$TermYears), !is.na(.data$FirstYears))

  tibble::tibble(
    Item = c("Documents stating a closed term",
             "Stating exactly one",
             "Stating more than one",
             "Where the longest is not the first written",
             "Of those, the first was under a year",
             "Of those, the longest is over five years"),
    N    = c(nrow(src_),
             sum(src_$NTerms == 1L),
             sum(src_$NTerms > 1L),
             sum(src_$Disagree),
             sum(src_$Disagree & src_$FirstYears < 1),
             sum(src_$Disagree & src_$TermYears > 5))
  ) |>
    dplyr::mutate(Share = .data$N / pmax(nrow(src_), 1L))
}


# 8. Tables --------------------------------------------------------------------------------------------------------------

#' Which rung each contract's end came from, by contract type
#' @param .dur Tibble from dte_collapse().
#' @return Tibble: one row per type, and one for the sample.
dte_table_source <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs    = dplyr::n(),
      PctTerm = mean(.data$DurationSource == "term"),
      PctOpen = mean(.data$DurationSource == "open"),
      PctCue  = mean(.data$DurationSource == "cue"),
      PctMax  = mean(.data$DurationSource == "maxdate"),
      PctNone = mean(.data$DurationSource == "none"),
      .by = dplyr::any_of("Class")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(.dur), dplyr::desc(.data$Docs)),
    dplyr::mutate(cols_(dplyr::select(.dur, -dplyr::any_of("Class"))), Class = "All", .before = 1L)
  )
}


#' The rule against the naive duration, by contract type
#'
#' THE TABLE THIS DOCUMENT EXISTS TO PRODUCE. NAIVE is the farthest future date the contract mentions,
#' which is the definition Table 3 was built with; RULE is the four-rung cascade. A reader who
#' disagrees with the cascade can read the first column and ignore the second, and both are
#' reproducible from the released file.
#'
#' @param .dur Tibble from dte_collapse().
#' @return Tibble: one row per type, and one for the sample.
dte_table_naive <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs      = dplyr::n(),
      MeanNaive = .dte_stat_or_na(.x = .data$NaiveYears[!is.na(.data$NaiveYears)], .f = mean),
      MeanRule  = .dte_stat_or_na(.x = .data$DurationYears[!is.na(.data$DurationYears)],
                                  .f = mean),
      MedNaive  = .dte_stat_or_na(.x = .data$NaiveYears[!is.na(.data$NaiveYears)],
                                  .f = stats::median),
      MedRule   = .dte_stat_or_na(.x = .data$DurationYears[!is.na(.data$DurationYears)],
                                  .f = stats::median),
      SdNaive   = .dte_stat_or_na(.x = .data$NaiveYears[!is.na(.data$NaiveYears)], .f = stats::sd),
      SdRule    = .dte_stat_or_na(.x = .data$DurationYears[!is.na(.data$DurationYears)],
                                  .f = stats::sd),
      PctSaid   = mean(.data$DurationSource %in% c("term", "open")),
      .by = dplyr::any_of("Class")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(.dur), dplyr::desc(.data$Docs)),
    dplyr::mutate(cols_(dplyr::select(.dur, -dplyr::any_of("Class"))), Class = "All", .before = 1L)
  )
}


#' Why a duration is missing
#' @param .dur Tibble from dte_collapse().
#' @return Tibble: one row per reason.
dte_table_dropped <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  .dur |>
    dplyr::summarise(Docs = dplyr::n(), .by = DurationDropped) |>
    dplyr::mutate(Share = .data$Docs / sum(.data$Docs)) |>
    dplyr::arrange(plot_factor(.data$DurationDropped, .key = "DurationDropped"))
}


#' What the stated terms look like
#' @param .terms Tibble from dte_release_terms().
#' @return Tibble: one row per pattern.
dte_table_terms <- function(.terms) {
  if (FALSE) .terms <- tab_terms

  .terms |>
    dplyr::filter(!is.na(.data$TermYears)) |>
    dplyr::summarise(
      Docs      = dplyr::n(),
      MedYears  = stats::median(.data$TermYears),
      PctUnder1 = mean(.data$TermYears < 1),
      PctOver10 = mean(.data$TermYears > 10),
      MedNTerms = stats::median(.data$NTerms),
      .by = TermKind
    ) |>
    dplyr::mutate(Share = .data$Docs / sum(.data$Docs)) |>
    dplyr::arrange(plot_factor(.data$TermKind, .key = "TermKind"))
}


#' Where the start came from
#' @param .dur Tibble from dte_collapse().
#' @return Tibble: one row per source.
dte_table_start <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  .dur |>
    dplyr::summarise(
      Docs    = dplyr::n(),
      MedGap  = stats::median(as.numeric(.data$DateFiled - .data$DateStart), na.rm = TRUE),
      MeanDur = .dte_stat_or_na(.x = .data$DurationYears[!is.na(.data$DurationYears)], .f = mean),
      .by = StartSource
    ) |>
    dplyr::mutate(Share = .data$Docs / sum(.data$Docs)) |>
    dplyr::arrange(plot_factor(.data$StartSource, .key = "StartSource"))
}


#' A few whole contracts, exactly as the file holds them
#' @param .tab Tibble from dte_collapse().
#' @param .n Integer. Contracts drawn.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: the drawn rows.
dte_release_sample <- function(.tab, .n = 8L, .seed = 42L) {
  if (FALSE) {
    .tab  <- tab_release
    .n    <- 8L
    .seed <- 42L
  }

  # ONE DOCUMENT PER RUNG WHERE THE SAMPLE HOLDS ONE, drawn deliberately. A random draw of eight
  # would show the commonest rung eight times and say nothing about the other three, and the rungs
  # are what a reader of DurationSource needs to recognise.
  pick_ <- purrr::map(plot_levels("DurationSource"), function(.s) {
    pool_ <- .tab$DocID[.tab$DurationSource == .s]
    if (length(pool_) == 0L) return(character(0))
    withr::with_seed(.seed, sample(pool_, size = min(2L, length(pool_))))
  }) |>
    unlist(use.names = FALSE)

  .tab |>
    dplyr::filter(.data$DocID %in% pick_) |>
    dplyr::arrange(plot_factor(.data$DurationSource, .key = "DurationSource"), .data$DocID) |>
    dplyr::slice_head(n = .n)
}


#' The disagreements between the longest term and the first one written
#'
#' READ RATHER THAN COUNTED, because no count can tell a contract's duration from a survival clause.
#' These are the documents where the rule and the alternative give different answers, which is the
#' whole population the choice is made over.
#'
#' @param .terms Tibble from dte_release_terms().
#' @param .n Integer. Documents drawn.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: the drawn documents, with both candidate terms.
dte_read_terms <- function(.terms, .n = 12L, .seed = 42L) {
  if (FALSE) {
    .terms <- tab_terms
    .n     <- 12L
    .seed  <- 42L
  }

  src_ <- dplyr::filter(.terms, .data$Disagree)
  if (nrow(src_) == 0L) return(tibble::tibble())

  src_ |>
    (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n, nrow(.d)))))() |>
    dplyr::arrange(dplyr::desc(.data$TermYears)) |>
    dplyr::select("DocID", "NTerms", Longest = "TermYears", LongestKind = "TermKind",
                  First = "FirstYears", FirstKind = "FirstKind", Span = "TermSpan")
}


# 9. Report --------------------------------------------------------------------------------------------------------------

#' What the dates and the terms supply
#' @param .dates Tibble from dte_load().
#' @param .terms Tibble from dte_release_terms().
#' @param .keys Tibble from ent_anchor_keys().
#' @return Invisibly the summary.
dte_report_input <- function(.dates, .terms, .keys) {
  if (FALSE) {
    .dates <- tab_dates
    .terms <- tab_terms
    .keys  <- tab_keys
  }

  cli::cli_h2("What the extractor supplied")

  n_ <- nrow(.keys)
  out_ <- tibble::tibble(
    Item = c("Documents in the sample",
             "With at least one date",
             "With a date that parsed",
             "With a stated closed term",
             "With an open-ended term"),
    N    = c(n_,
             dplyr::n_distinct(.dates$DocID),
             dplyr::n_distinct(.dates$DocID[.dates$Parsed]),
             dplyr::n_distinct(.terms$DocID[!is.na(.terms$TermYears)]),
             dplyr::n_distinct(.terms$DocID[.terms$IsOpen]))
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$N / n_))

  tbl_say(.tab = out_, .title = "Documents reached, by what the extractor found in them")

  cli::cli_alert_info(
    "matcon parses almost everything it emits, so the gap between the second and third rows is a \\
     rounding error rather than a population. The two term rows are the ceiling on the rungs that \\
     read what the contract SAYS: everything above that share falls to a cue or to the farthest \\
     future date."
  )
  invisible(out_)
}


#' What the stated terms look like
#' @param .tab Tibble from dte_table_terms().
#' @param .cmp Tibble from dte_term_compare().
#' @return Invisibly .tab.
dte_report_terms <- function(.tab, .cmp) {
  if (FALSE) {
    .tab <- tab_term_tab
    .cmp <- tab_term_cmp
  }

  cli::cli_h2("The stated terms")
  .tab |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct_safe(.x)),
      Share = tbl_pct(.data$Share)
    ) |>
    tbl_say(.title = "One row per pattern, over the documents stating a closed term")

  .cmp |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "The longest stated period against the first one written")

  cli::cli_alert_info(
    "PctUnder1 IS THE NUMBER THE RULE EXISTS FOR. A period under a year is a cure window, a notice \\
     period or a warranty, and contracts state several of them beside their actual duration -- all \\
     written \"period of N units\" and all matching the same pattern, so no filter on pattern names \\
     could separate them. Taking the LONGEST does, without a threshold and without deleting the only \\
     stated term of a genuinely short agreement."
  )
  cli::cli_alert_info(
    "The second table is the trade. Where the two rules disagree the longest is right if it is the \\
     contract's duration and wrong if it is a survival clause, and no count can tell those apart -- \\
     which is what the reading block in Robustness is for."
  )
  invisible(.tab)
}


#' How often each termination cue fires
#' @param .tab Tibble from dte_cue_hits().
#' @return Invisibly .tab.
dte_report_cues <- function(.tab) {
  if (FALSE) .tab <- tab_cues

  cli::cli_h2("The termination cues")
  .tab |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct_safe(.x))) |>
    tbl_say(.title = "One row per cue, over every future date")

  cli::cli_alert_info(
    "A USEFUL CUE IS HIGH ON PctOfCued AND LOW ON PctOfFuture. A term firing beside a large share of \\
     ALL future dates is marking dates rather than terminations, and THROUGH and UNTIL are the two \\
     entries under suspicion because both are common enough to sit beside anything. This table is \\
     where that gets settled: a term high on both columns is carrying the cue rung on its own and \\
     should be read before it is trusted."
  )
  invisible(.tab)
}


#' Which rung each contract's end came from
#' @param .tab Tibble from dte_table_source().
#' @return Invisibly .tab.
dte_report_source <- function(.tab) {
  if (FALSE) .tab <- tab_source

  cli::cli_h2("Which rung supplied the end")
  .tab |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "One row per contract type, and one for the sample")

  cli::cli_alert_info(
    "PctTerm AND PctOpen ARE WHAT THE CONTRACT SAID; PctCue is a reading of the words beside a date; \\
     PctMax is a maximum over dates the contract mentions for reasons it never states. Their sum is \\
     coverage and their split is quality, and a reader wanting only what was stated filters \\
     DurationSource rather than recomputing anything."
  )
  invisible(.tab)
}


#' The rule against the naive duration
#' @param .tab Tibble from dte_table_naive().
#' @return Invisibly .tab.
dte_report_naive <- function(.tab) {
  if (FALSE) .tab <- tab_naive

  cli::cli_h2("The rule against the naive duration")
  .tab |>
    dplyr::mutate(
      dplyr::across(c(MeanNaive, MeanRule, MedNaive, MedRule, SdNaive, SdRule),
                    \(.x) tbl_num(.x)),
      PctSaid = tbl_pct(.data$PctSaid)
    ) |>
    tbl_say(.title = "Duration under both definitions, by contract type")

  cli::cli_alert_info(
    "NAIVE is the farthest future date the contract mentions, which is the definition Table 3 was \\
     built with. RULE is the cascade. READ THE TWO STANDARD DEVIATIONS AGAINST EACH OTHER: that is \\
     the referee's question, because a variable capped at 30 years cannot have a standard deviation \\
     above 15 and the published one is 40.78. PctSaid is the share where the end came from something \\
     the contract stated rather than from a maximum."
  )
  invisible(.tab)
}


#' Why a duration is missing, and where the start came from
#' @param .drop Tibble from dte_table_dropped().
#' @param .start Tibble from dte_table_start().
#' @return Invisibly .drop.
dte_report_missing <- function(.drop, .start) {
  if (FALSE) {
    .drop  <- tab_drop
    .start <- tab_start
  }

  cli::cli_h2("What is missing, and why")
  .drop |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "One row per reason a duration is not reported")

  .start |>
    dplyr::mutate(Share = tbl_pct(.data$Share), MeanDur = tbl_num(.data$MeanDur)) |>
    tbl_say(.title = "Where the start came from")

  cli::cli_alert_info(
    "THE CAP DROPS RATHER THAN WINSORISES, so CAPPED is a missing value and not a pile at thirty \\
     years -- a duration of exactly the cap that is not one is worse than nothing. NEGATIVE is an end \\
     before the start, which can only mean the two came from different readings of the document. \\
     FILED is the fallback start: no date preceded the filing, so EDGAR's own date was used, and \\
     that is a date the contract never wrote."
  )
  invisible(.drop)
}


#' Every specification, side by side
#' @param .tab Tibble from dte_sweep().
#' @return Invisibly .tab.
dte_report_sweep <- function(.tab) {
  if (FALSE) .tab <- tab_sweep

  cli::cli_h2("Every definition, side by side")
  .tab |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
      dplyr::across(c(Mean, Median, Sd, SdOverCap), \(.x) tbl_num(.x))
    ) |>
    tbl_say(.title = "One row per definition, the first being the published one")

  cli::cli_alert_info(
    "SdOverCap CANNOT EXCEED ONE HALF on non-negative support under a correctly applied cap, and \\
     Table 3 reports a ratio of 1.36. Any row here above 0.5 means the cap did not reach what is \\
     being summarised. The uncapped rows have no ratio because there is nothing to divide by, which \\
     is itself the point: an uncapped maximum over dates a contract happens to mention has no bound \\
     at all."
  )
  invisible(.tab)
}


#' The periods, by kind
#' @param .tab Tibble from dte_table_period().
#' @return Invisibly .tab.
dte_report_period <- function(.tab) {
  if (FALSE) .tab <- tab_period

  cli::cli_h2("What the stated periods actually are")

  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("No closed period in the sample.")
    return(invisible(.tab))
  }

  .tab |>
    dplyr::mutate(
      Periods  = format(.data$Periods, big.mark = ","),
      MedYears = tbl_num(.data$MedYears),
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))
    ) |>
    tbl_say(.title = "One row per kind, with the share of released durations it produced")

  cli::cli_alert_info(
    "THE MEDIANS ARE THE EVIDENCE FOR THE CUE LIST. Cure and notice both land on thirty days, which \\
     is what those words mean in a contract -- so where this list fires it fires correctly. Two \\
     thirds of periods match nothing and fall to OTHER, which is why it is a diagnostic and not a \\
     rule: a positive filter on TERM would select on a label missing for most of the data."
  )

  bad_ <- sum(.tab$PctLongest[.tab$PeriodKind %in% c("cure", "notice")])
  cli::cli_alert_warning(
    "PctLongest ON CURE AND NOTICE IS {tbl_pct(bad_)} OF RELEASED DURATIONS, and those contracts \\
     have a remedy deadline recorded as the life of the agreement. Two fixes were measured and both \\
     made it worse -- excluding those kinds swapped thirty days for a shorter period on half the \\
     affected contracts. The rule is therefore unchanged and the column ships, so anyone who wants \\
     them gone writes one filter over the term file."
  )
  invisible(.tab)
}


#' What the two released files hold
#' @param .dates Tibble from dte_release_dates().
#' @param .terms Tibble from dte_release_terms().
#' @return Invisibly the summary.
dte_report_release <- function(.dates, .terms) {
  if (FALSE) {
    .dates <- tab_dates_rel
    .terms <- tab_terms_rel
  }

  cli::cli_h2("The two released files")

  out_ <- tibble::tribble(
    ~File,                  ~Rows,         ~Docs,
    "date_spans.parquet",   nrow(.dates),  dplyr::n_distinct(.dates$DocID),
    "term_spans.parquet",   nrow(.terms),  dplyr::n_distinct(.terms$DocID)
  ) |>
    dplyr::mutate(dplyr::across(c(Rows, Docs), \(.x) format(.x, big.mark = ",")))

  tbl_say(.tab = out_, .title = "One row per date span, and one row per stated period")

  cli::cli_alert_info(
    "AN UNPARSED DATE STAYS IN THE FILE. It says the extractor found something date-shaped that no \\
     rule could use, which is a measurement about the parser rather than an absence -- and dropping \\
     it would make the parse rate uncomputable from the release."
  )
  cli::cli_alert_info(
    "EVERY PERIOD STAYS TOO, not the longest. Which one is the term is a decision rather than a \\
     fact, so it belongs in dte_collapse() where a reader can change it, and not in a file where \\
     they cannot."
  )
  invisible(out_)
}


#' A few whole contracts, printed as the file holds them
#' @param .tab Tibble from dte_release_sample().
#' @return Invisibly .tab.
dte_report_release_sample <- function(.tab) {
  if (FALSE) .tab <- tab_sample

  cli::cli_h2("The released file, read")

  .tab |>
    dplyr::select("DocID", "Class", "DateStart", "StartSource", "DateEnd", "DurationSource",
                  "DurationYears", "NaiveYears", "TermYears", "NTerms") |>
    tbl_say(.title = "Two contracts from each rung, in released columns")

  cli::cli_alert_info(
    "READ DurationYears AGAINST NaiveYears ROW BY ROW. Where the rung is TERM they can differ by a \\
     lot, and the difference is a stated duration replacing a maximum over unrelated dates. Where the \\
     rung is MAXDATE they are equal by construction, and those are the contracts for which this rule \\
     has nothing better than the published definition."
  )
  invisible(.tab)
}


#' The disagreements between the two term rules
#' @param .tab Tibble from dte_read_terms().
#' @return Invisibly .tab.
dte_report_read_terms <- function(.tab) {
  if (FALSE) .tab <- tab_read_terms

  cli::cli_h2("Where the longest term is not the first one written")

  if (nrow(.tab) == 0L) {
    cli::cli_alert_info("No document states two closed terms of different lengths.")
    return(invisible(.tab))
  }

  tbl_say(.tab = .tab, .title = "The span the rule chose, beside the one position would have chosen")

  cli::cli_alert_info(
    "THIS IS THE ONE BLOCK A COUNT CANNOT REPLACE. Read the span: \"for a period of five (5) years\" \\
     beside a first term of thirty days is the rule working, and a ten-year confidentiality period \\
     beside a first term of one year is the rule taking a survival clause for a duration. If the \\
     second shape dominates, the answer is a cue on the span rather than a different threshold."
  )
  invisible(.tab)
}


#' The column dictionary
#' @param .tab Any of this document's dictionary tables.
#' @param .title Character. Heading, since three dictionaries share this reporter.
#' @return Invisibly .tab.
dte_report_dictionary <- function(.tab, .title = "The columns, in the order the file carries them") {
  if (FALSE) {
    .tab   <- tab_dict
    .title <- "date_spans.parquet"
  }

  cli::cli_h2(.title)
  tbl_say(.tab = .tab, .title = paste0(nrow(.tab), " columns, in the order they appear"))

  cli::cli_alert_info(
    "GRAIN SAYS WHERE THE COLUMN BELONGS. A document column repeats across every span of a contract \\
     and a span column varies row by row, so averaging the first over the second weights it by how \\
     many spans a contract happened to carry. The dictionary is compared with the file's own names, \\
     so a column added or renamed without an entry aborts this chunk."
  )
  invisible(.tab)
}


#' The rule in one table
#' @param .dur Tibble from dte_collapse().
#' @param .dates Tibble from dte_release_dates().
#' @param .terms Tibble from dte_release_terms().
#' @return Invisibly the table.
dte_report_headline <- function(.dur, .dates, .terms) {
  if (FALSE) {
    .dur   <- tab_dur
    .dates <- tab_dates_rel
    .terms <- tab_terms_rel
  }

  cli::cli_h2("The rule in one table")

  kept_  <- .dur$DurationYears[!is.na(.dur$DurationYears)]
  naive_ <- .dur$NaiveYears[!is.na(.dur$NaiveYears)]

  out_ <- tibble::tribble(
    ~Item,                                      ~Value,
    "Contracts",                                format(nrow(.dur), big.mark = ","),
    "End taken from a stated term",             tbl_pct(mean(.dur$DurationSource == "term")),
    "End taken from an open-ended clause",      tbl_pct(mean(.dur$DurationSource == "open")),
    "End taken from a cued date",               tbl_pct(mean(.dur$DurationSource == "cue")),
    "End taken from the farthest future date",  tbl_pct(mean(.dur$DurationSource == "maxdate")),
    "No end found at all",                      tbl_pct(mean(.dur$DurationSource == "none")),
    "Duration reported",                        tbl_pct(mean(!is.na(.dur$DurationYears))),
    "Median duration, this rule",               tbl_num(.dte_stat_or_na(.x = kept_,
                                                                       .f = stats::median)),
    "Median duration, naive",                   tbl_num(.dte_stat_or_na(.x = naive_,
                                                                       .f = stats::median)),
    "Standard deviation, this rule",            tbl_num(.dte_stat_or_na(.x = kept_, .f = stats::sd)),
    "Standard deviation, naive",                tbl_num(.dte_stat_or_na(.x = naive_,
                                                                       .f = stats::sd)),
    "Date spans released",                      format(nrow(.dates), big.mark = ","),
    "Period spans released",                    format(nrow(.terms), big.mark = ",")
  )

  tbl_say(.tab = out_, .title = "Everything this document decided")

  cli::cli_alert_info(
    "THE TWO STANDARD DEVIATIONS ARE THE ANSWER TO THE REFEREE. A duration capped at thirty years \\
     cannot have one above fifteen, and the published figure is 40.78. The first five rows say where \\
     every contract's end came from, and the first two of those are the only ones the contract \\
     stated itself."
  )
  invisible(out_)
}


# 10. Figures ------------------------------------------------------------------------------------------------------------

#' Which rung supplied the end, by contract type
#' @param .dur Tibble from dte_collapse().
#' @return A ggplot.
dte_plot_source <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  .dur |>
    dplyr::summarise(N = dplyr::n(), .by = c(Class, DurationSource)) |>
    plot_bar_stacked(
      .cat      = "Class",
      .val      = "N",
      .fill     = "DurationSource",
      .key_fill = "DurationSource",
      .short    = TRUE,
      .share    = TRUE
    ) +
    ggplot2::labs(x = "Share of contracts")
}


#' The rule against the naive duration, by contract type
#' @param .tab Tibble from dte_table_naive().
#' @return A ggplot.
dte_plot_naive <- function(.tab) {
  if (FALSE) .tab <- tab_naive

  .tab |>
    dplyr::filter(.data$Class != "All") |>
    dplyr::select("Class", Naive = "MedNaive", Rule = "MedRule") |>
    tidyr::pivot_longer(cols = c("Naive", "Rule"), names_to = "Measure", values_to = "Years") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Years,
                                 y = stats::reorder(.data$Class, .data$Years),
                                 fill = .data$Measure)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
    plot_scale_fill_cat(name = NULL) +
    plot_scale_x_count() +
    ggplot2::labs(x = "Median duration in years", y = NULL) +
    plot_theme(.grid = "x", .legend = "bottom")
}


#' The distribution of duration, by rung
#' @param .dur Tibble from dte_collapse().
#' @return A ggplot.
dte_plot_spread <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  .dur |>
    dplyr::filter(!is.na(.data$DurationYears), .data$DurationSource != "none") |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$DurationYears,
      y = plot_factor(.data$DurationSource, .key = "DurationSource", .short = TRUE, .rev = TRUE)
    )) +
    ggplot2::geom_boxplot(outlier.size = 0.5, linewidth = 0.4, fill = plot_pal_seq(1L),
                          colour = plot_pal_grey(1L)) +
    plot_scale_x_count() +
    ggplot2::labs(x = "Duration in years", y = NULL) +
    plot_theme(.grid = "x")
}


#' How often each cue fires, against how often it marks a termination
#' @param .tab Tibble from dte_cue_hits().
#' @return A ggplot.
dte_plot_cues <- function(.tab) {
  if (FALSE) .tab <- tab_cues

  plot_bar_ranked(
    .tab      = .tab,
    .cat      = "Cue",
    .val      = "PctOfFuture",
    .pct      = TRUE,
    .accuracy = 1
  ) +
    ggplot2::labs(x = "Share of all future dates the cue sits beside")
}


