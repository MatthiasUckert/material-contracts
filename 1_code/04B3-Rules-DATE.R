# 04B3-Rules-DATE: when a contract starts, ends and how long it runs -----------------------------------------------------
#
# WHAT THIS FILE DOES
# Turns parsed dates and stated terms into a contract duration. Function prefix is `dte_`.
#
# THE NUMBER THAT MADE THIS DOCUMENT NECESSARY
# Table 3 Panel B reports contract duration with a mean of 2.38 years, a median of 0.40 and a
# standard deviation of 40.78, under a stated cap of 30 years. A variable supported on [0, 30] cannot
# have a standard deviation above 15 -- the maximum, reached by a 50/50 split on the endpoints. So
# the cap is not applied to what is tabulated, or something is in the wrong unit. Referee 1 saw the
# symptom and asked whether there were "errors in identifying dates affecting the right tail".
#
# THE PUBLISHED DEFINITION IS THE NAIVE BASELINE, and it is a row in the sweep rather than something
# this document replaces sight unseen: take the farthest future date the contract mentions and call
# the distance to it the duration. Every table below reports it beside the rule, because the
# difference between the two IS what the rule did.
#
# THE RULE IS ONE START AND A FOUR-RUNG END
#   START. The latest date at or before the filing. An amendment citing a 1998 original still signs
#     itself later, and the latest pre-filing date finds that wherever it sits. Where no date
#     precedes the filing, the filing date is used and StartSource records it.
#   END, in order:
#     1. A STATED TERM -- "for a period of five (5) years". A duration with no end date at all, and
#        once the start is known it is arithmetic. This is what the contract SAYS.
#     2. AN OPEN-ENDED TERM -- "shall continue until terminated", "in perpetuity". The duration is
#        left MISSING, because a perpetual agreement has no duration and saying so is a finding.
#     3. A DATED TERMINATION -- the farthest future date with a termination cue beside it.
#     4. THE FARTHEST FUTURE DATE, whatever it is. The naive definition, kept as the last rung and
#        marked, because it is a guess rather than a reading.
# DurationSource distinguishes all four, so a reader can keep only what the contract actually said.
#
# THE LONGEST STATED PERIOD IS THE TERM, AND THAT REPLACED TWO DIALS
# A contract states several periods and only one of them is its duration:
#
#   3.1  Buyer shall cure any breach within a period of thirty (30) days.
#   5.2  Seller warrants the Products for a period of twelve (12) months.
#   9.1  This Agreement shall continue for a period of five (5) years.
#
# All three are real contract terms; only 9.1 is how long the contract lasts. All three match the
# SAME PATTERN, because the drafter wrote "period of N units" three times -- so no filter on pattern
# names can separate them, and taking the first by position releases the CURE PERIOD as the duration.
#
# TAKING THE LONGEST NEEDS NO THRESHOLD AND CANNOT DELETE A CONTRACT'S ONLY TERM. A minimum length
# would work on the example, but it has to be defended -- why one year and not six months -- and it
# deletes the stated term of a genuine three-month agreement, sending it to the guess rung. The
# longest is a SELECTION rather than a FILTER: a contract stating one period keeps it whatever its
# length, and a contract stating several gets the one that outlasts the others.
#
# WHAT IT COSTS, STATED RATHER THAN DISCOVERED. It is wrong exactly where a contract states a longer
# NON-DURATION period than its own duration: a one-year agreement with a ten-year confidentiality
# survival clause, a two-year agreement with a six-year licence carve-out. Survival clauses are the
# real exposure, because they are common and deliberately long. The trade is accepted because a cure
# period is far commoner than a survival clause outlasting the term, and the comparison against
# position-first is reported rather than asserted -- with the disagreements read back in context,
# which is the only thing that can tell a duration from a survival clause.
#
# ONE ENGINE, AND IT SETTLES THE RIGHT-TAIL QUESTION BY CONSTRUCTION
# matcon's patterns require a year IN THE TEXT -- four digits in nine of them, two in SlashShort --
# so there is no partial match to complete and no clock to complete it from. LexNLP's grammar has no
# such constraint, and dateregex.py measured the consequence on this sample: of its spans carrying no
# written year, 76% resolve to the FUTURE at a median of 11.6 years out, and 80.8% of all dates more
# than fifteen years out are year-less. Running matcon alone is therefore not a preference between
# two producers; it is the guard against the exact failure the referee asked about.
#
# AND IT RETIRES A DIAL. RequireYear existed to drop spans whose year the parser invented, which is a
# LexNLP failure mode. Under matcon every emitted date carries a year the contract wrote, so the
# guard has nothing to guard and it is gone rather than left switched off.
#
# THE CUE IS A COLUMN NOW, AND THIS WAS THE LAST TEXT READ IN THE FAMILY
# dte_describe() used to open 04A's canonical text and cut 120 characters before every date to test
# for a termination cue. Every matcon span now carries CueBefore -- 160 raw characters, stored at
# extraction where the text and the offsets are already in one scope -- so the test is a column read.
#
# THAT MATTERS BEYOND THIS FILE. It was the last of three rules that opened a document, and with it
# gone no rule in the 04B family touches contract text at all. 04D's Cues dial existed to switch
# those reads off at corpus scale, and switching them off is what made the governing-law columns
# vanish from the release; there is nothing left for it to gate.
#
# THE CAP DROPS, IT DOES NOT WINSORISE. A duration of exactly thirty years that is not one is worse
# than a missing value, and DurationDropped says which of the two reasons applied.
#
# ONE FILE, ONE ROW PER CONTRACT
# A duration belongs to a contract and to no party in it, so this writes its own file rather than
# widening 04B2's. That is not the two-file problem ORG removed: the counts in that second file were
# DERIVED from the first and could contradict it, while a duration is an independent measurement that
# nothing else computes.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .dir_store <- .lP$Input$Store
}


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


#' Load the TERM spans, and take the longest one per contract
#'
#' THE EXTRACTOR STATES, THE RULE DECIDES. dateregex emits every stated period it can find and
#' records which pattern found it. This file chooses which of them is the contract's duration.
#'
#' THE LONGEST WINS, AND THAT IS THE WHOLE RULE. A contract states a cure period, a warranty period
#' and a term, all three written "period of N units" and all three matching the same pattern; the
#' longest of them is the one the contract lasts for. Precedence by pattern cannot separate them
#' because the drafter used the same words, and precedence by position releases the cure period.
#'
#' NO FLOOR, AND THAT IS DELIBERATE. A minimum length would also work on that example, and it has two
#' costs the longest does not: a number to defend, and the deletion of a genuine three-month
#' agreement's only stated term. A selection cannot delete anything.
#'
#' OPEN-ENDED IS CARRIED SEPARATELY. "shall continue until terminated" has a null TermYears, so it
#' can never be the longest and would vanish; it is a stated term of UNKNOWN length rather than an
#' absent one, and dte_duration() gives it its own rung.
#'
#' NTerms IS REPORTED because the risk lives entirely in contracts stating more than one period. A
#' contract stating one is unambiguous whatever rule picks it.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .lens Tibble: DocID and DocLen.
#' @param .quiet Logical. Suppress the count message.
#' @return Tibble: one row per document stating a term, with the longest and the first by position.
dte_load_terms <- function(.dir_store, .lens, .quiet = FALSE) {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .lens      <- tab_lens
    .quiet     <- FALSE
  }

  raw_ <- ent_load_entity(
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

  bad_ <- setdiff(unique(raw_$TermKind), plot_levels("TermKind"))
  if (length(bad_) > 0L) {
    cli::cli_abort(c(
      "dateregex emits {length(bad_)} TERM pattern{?s} the vocabulary does not name: \\
       {paste(bad_, collapse = ', ')}.",
      "i" = "Register {length(bad_)} name{?s} in TermKind so every table can order them."
    ))
  }

  open_ <- raw_ |>
    dplyr::filter(.data$IsOpen) |>
    dplyr::summarise(OpenKind = dplyr::first(.data$TermKind), .by = DocID)

  closed_ <- dplyr::filter(raw_, !.data$IsOpen, !is.na(.data$TermYears))

  # THE LONGEST, and the FIRST BY POSITION beside it. The second is not used by the rule; it is what
  # the comparison in Selection is measured against, and computing it here means the two answers come
  # from one pass over one table rather than from two that could diverge.
  long_ <- closed_ |>
    dplyr::arrange(.data$DocID, dplyr::desc(.data$TermYears), .data$Start) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::select("DocID", "TermKind", "TermN", "TermUnit", "TermYears",
                  TermStart = "Start", TermSpan = "Span")

  first_ <- closed_ |>
    dplyr::arrange(.data$DocID, .data$Start) |>
    dplyr::slice_head(n = 1L, by = DocID) |>
    dplyr::select("DocID", FirstYears = "TermYears", FirstKind = "TermKind")

  seen_ <- closed_ |>
    dplyr::summarise(
      NTerms   = dplyr::n(),
      MaxYears = max(.data$TermYears),
      MinYears = min(.data$TermYears),
      .by = DocID
    )

  out_ <- long_ |>
    dplyr::left_join(first_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(seen_,  by = dplyr::join_by(DocID)) |>
    dplyr::full_join(open_,  by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      NTerms   = as.integer(dplyr::coalesce(.data$NTerms, 0L)),
      IsOpen   = !is.na(.data$OpenKind),
      Disagree = !is.na(.data$TermYears) & !is.na(.data$FirstYears) &
                 .data$TermYears != .data$FirstYears
    )

  if (!.quiet) {
    cli::cli_alert_success(
      "Stated term in {format(nrow(out_), big.mark = ',')} \\
       {cli::qty(nrow(out_))}document{?s}, from {format(nrow(raw_), big.mark = ',')} \\
       {cli::qty(nrow(raw_))}span{?s}. {format(sum(out_$IsOpen), big.mark = ',')} \\
       {cli::qty(sum(out_$IsOpen))}{?is/are} open-ended, and \\
       {format(sum(out_$Disagree), big.mark = ',')} \\
       {cli::qty(sum(out_$Disagree))}state{?s} a longer period than the first one written."
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
#' @return .dates with GapDays, Side, Before and HasEndCue added.
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

  hit_ <- function(.txt, .terms) {
    Reduce(`|`, lapply(.terms, function(.t) stringi::stri_detect_fixed(.txt, .t)))
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
      HasEndCue = hit_(.data$Before, .dte_end_cue)
    )
}


# 5. The duration --------------------------------------------------------------------------------------------------------

#' One start, one end and a duration per document
#'
#' RUNS FROM THE KEY SIDE, so a document in which matcon found no date at all still carries a row and
#' enters every average as a missing value rather than disappearing from the denominator.
#'
#' THE FOUR RUNGS ARE FOUR KINDS OF EVIDENCE, not four attempts at one thing. A stated term is what
#' the contract SAYS about itself. An open-ended clause says it will not end, which is a finding
#' rather than a gap. A cued date is a reading of the words beside a date. The farthest future date
#' is a maximum over dates the contract mentions for reasons it never states, and it is the naive
#' definition kept as the last rung so that coverage does not fall to nothing.
#'
#' AN OPEN-ENDED TERM OUTRANKS THE FARTHEST FUTURE DATE AND NOT A STATED ONE. A document saying both
#' "five year term" and "shall continue until terminated" has stated a length, and the length is the
#' answer; a document saying only the second has stated that it HAS no length, which is a different
#' thing from having said nothing.
#'
#' THE NAIVE DURATION IS COMPUTED FOR EVERY DOCUMENT whatever the specification asks for, because it
#' is the comparison every table is read against and deriving it later from the released columns is
#' impossible: the farthest future date is not among them.
#'
#' @param .dates Tibble from dte_describe().
#' @param .terms Tibble from dte_load_terms().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .spec List from dte_spec().
#' @return Tibble: one row per document.
dte_duration <- function(.dates, .terms, .keys, .spec) {
  if (FALSE) {
    .dates <- tab_desc
    .terms <- tab_terms
    .keys  <- tab_keys
    .spec  <- .lP$Params$Spec
  }

  src_ <- dplyr::filter(.dates, .data$Parsed)

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
    dplyr::select(DocID, Class, AmendType, DateFiled) |>
    dplyr::left_join(seen_,    by = dplyr::join_by(DocID)) |>
    dplyr::left_join(signed_,  by = dplyr::join_by(DocID)) |>
    dplyr::left_join(end_any_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(end_cue_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(
      dplyr::select(.terms, DocID, TermKind, TermYears, TermN, TermUnit, NTerms, IsOpen,
                    OpenKind, FirstYears, Disagree),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::mutate(
      dplyr::across(c(NDates, NFuture, NCued, NTerms),
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
      # DROPPED, not winsorised, and the reason is recorded. A duration of exactly the cap that is
      # not one is worse than a missing value, and a missing value nobody can explain is worse again.
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


#' The release: one row per contract
#'
#' A DURATION BELONGS TO A CONTRACT AND TO NO PARTY IN IT, so this is its own file rather than four
#' more columns on 04B2's. That is not the two-file problem ORG removed: the counts in that second
#' file were DERIVED from the first and could contradict it, while a duration is an independent
#' measurement that nothing else computes. Joining is one key.
#'
#' NaiveYears IS RELEASED, and it is the definition this document argues against. Without it the
#' comparison every table makes cannot be reproduced from the file -- the farthest future date is not
#' among the released columns, so a reader could not recompute it. Publishing the baseline beside the
#' rule is the same discipline that keeps the excluded parties in 04B1's file.
#'
#' EVERY DOCUMENT GETS A ROW, including the ones matcon found no date in. They carry a missing
#' duration and DurationSource "none", so a mean over the file divides by the sample rather than by
#' the documents that worked.
#'
#' @param .dur Tibble from dte_duration().
#' @return Tibble: one row per contract, sixteen columns.
dte_release <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  .dur |>
    dplyr::transmute(
      .data$DocID, .data$Class, .data$AmendType,
      DateFiled       = .data$DateFiled,
      DateStart       = .data$DateStart,
      StartSource     = .data$StartSource,
      DateEnd         = .data$DateEnd,
      DurationSource  = .data$DurationSource,
      DurationYears   = .data$DurationYears,
      DurationDropped = .data$DurationDropped,
      NaiveYears      = .data$NaiveYears,
      TermYears       = .data$TermYears,
      TermKind        = .data$TermKind,
      NTerms          = .data$NTerms,
      NDates          = .data$NDates,
      NFuture         = .data$NFuture
    ) |>
    dplyr::arrange(.data$DocID)
}


#' Apply one specification end to end
#'
#' @param .dates Tibble from dte_describe().
#' @param .terms Tibble from dte_load_terms().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .spec List from dte_spec().
#' @return A list: Spec, Duration, Release.
dte_apply <- function(.dates, .terms, .keys, .spec) {
  if (FALSE) {
    .dates <- tab_desc
    .terms <- tab_terms
    .keys  <- tab_keys
    .spec  <- .lP$Params$Spec
  }

  dur_ <- dte_duration(.dates = .dates, .terms = .terms, .keys = .keys, .spec = .spec)
  list(Spec = .spec, Duration = dur_, Release = dte_release(.dur = dur_))
}


# 6. Evidence ------------------------------------------------------------------------------------------------------------

#' Reduce one specification to a comparable row
#'
#' SdOverCap IS THE DIAGNOSTIC THIS DOCUMENT WAS BUILT AROUND. On non-negative support under a
#' correctly applied cap the standard deviation cannot exceed half the cap. Table 3 Panel B reports
#' 40.78 against a stated cap of 30, a ratio of 1.36, which no distribution on [0, 30] can produce --
#' so the cap was not applied to what was tabulated, or something was in the wrong unit. Every row
#' below carries the ratio, and it cannot exceed one half.
#'
#' @param .dur Tibble from dte_duration().
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
#' @param .terms Tibble from dte_load_terms().
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
    dte_row(.dur = dte_duration(.dates = .dates, .terms = .terms, .keys = .keys, .spec = .s),
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
#' @param .terms Tibble from dte_load_terms().
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


# 7. Tables --------------------------------------------------------------------------------------------------------------

#' Which rung each contract's end came from, by contract type
#' @param .dur Tibble from dte_duration().
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
    dplyr::mutate(cols_(dplyr::select(.dur, -"Class")), Class = "All", .before = 1L)
  )
}


#' The rule against the naive duration, by contract type
#'
#' THE TABLE THIS DOCUMENT EXISTS TO PRODUCE. NAIVE is the farthest future date the contract mentions,
#' which is the definition Table 3 was built with; RULE is the four-rung cascade. A reader who
#' disagrees with the cascade can read the first column and ignore the second, and both are
#' reproducible from the released file.
#'
#' @param .dur Tibble from dte_duration().
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
    dplyr::mutate(cols_(dplyr::select(.dur, -"Class")), Class = "All", .before = 1L)
  )
}


#' Why a duration is missing
#' @param .dur Tibble from dte_duration().
#' @return Tibble: one row per reason.
dte_table_dropped <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  .dur |>
    dplyr::summarise(Docs = dplyr::n(), .by = DurationDropped) |>
    dplyr::mutate(Share = .data$Docs / sum(.data$Docs)) |>
    dplyr::arrange(plot_factor(.data$DurationDropped, .key = "DurationDropped"))
}


#' What the stated terms look like
#' @param .terms Tibble from dte_load_terms().
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
#' @param .dur Tibble from dte_duration().
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
#' @param .tab Tibble from dte_release().
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
#' @param .terms Tibble from dte_load_terms().
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


#' What every column of the released file means
#'
#' A TABLE RATHER THAN PROSE, AND CHECKED AGAINST THE FILE. A column added or renamed without a
#' matching entry aborts the render rather than leaving the documentation quietly wrong.
#'
#' @param .tab Tibble from dte_release().
#' @return Tibble: Column, Meaning.
dte_dictionary <- function(.tab) {
  if (FALSE) .tab <- tab_release

  dict_ <- tibble::tribble(
    ~Column,           ~Meaning,
    "DocID",           "the contract; joins to every other 04 file",
    "Class",           "contract type, from 03A's label spine",
    "AmendType",       "original or amended, from the same spine",
    "DateFiled",       "when EDGAR recorded the filing",
    "DateStart",       "the signing date: the latest date at or before the filing",
    "StartSource",     "signed, or filed where no date preceded the filing",
    "DateEnd",         "the end, from whichever rung supplied it",
    "DurationSource",  "term, open, cue, maxdate, or none -- WHICH RUNG",
    "DurationYears",   "end minus start; missing where dropped or where nothing was found",
    "DurationDropped", "kept, negative, capped, or no end -- why it is missing",
    "NaiveYears",      "the farthest future date less the start; the published definition",
    "TermYears",       "the longest stated period, where the contract stated one",
    "TermKind",        "which pattern found that period",
    "NTerms",          "how many closed periods the contract states at all",
    "NDates",          "distinct dates found",
    "NFuture",         "of those, dates after the filing"
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


# 8. Report --------------------------------------------------------------------------------------------------------------

#' What the dates and the terms supply
#' @param .dates Tibble from dte_load().
#' @param .terms Tibble from dte_load_terms().
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


#' What the released file holds
#' @param .tab Tibble from dte_release().
#' @return Invisibly the summary.
dte_report_release <- function(.tab) {
  if (FALSE) .tab <- tab_release

  cli::cli_h2("The released file")

  out_ <- .tab |>
    dplyr::summarise(
      Docs      = dplyr::n(),
      MeanYears = .dte_stat_or_na(.x = .data$DurationYears[!is.na(.data$DurationYears)],
                                  .f = mean),
      .by = DurationSource
    ) |>
    dplyr::mutate(
      Share     = tbl_pct(.data$Docs / sum(.data$Docs)),
      MeanYears = tbl_num(.data$MeanYears)
    ) |>
    dplyr::arrange(plot_factor(.data$DurationSource, .key = "DurationSource"))

  tbl_say(.tab = out_, .title = "One row per contract, by the rung that supplied its end")

  cli::cli_alert_info(
    "EVERY CONTRACT GETS A ROW, including the ones matcon found no date in -- those carry NONE and a \\
     missing duration, so a mean over the file divides by the sample rather than by the documents \\
     that worked. NaiveYears is released beside the rule so the comparison every table makes can be \\
     reproduced from the file rather than only from this render."
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
#' @param .tab Tibble from dte_dictionary().
#' @return Invisibly .tab.
dte_report_dictionary <- function(.tab) {
  if (FALSE) .tab <- tab_dict

  cli::cli_h2("What every column means")
  tbl_say(.tab = .tab, .title = "Sixteen columns, in the order the file carries them")

  cli::cli_alert_info(
    "DurationSource IS THE COLUMN THAT MAKES THE REST USABLE. Filtering it to term and open keeps \\
     only what the contract stated about itself; keeping maxdate as well reproduces the published \\
     definition. The dictionary is compared with the file's own names, so a column added or renamed \\
     without an entry aborts this chunk."
  )
  invisible(.tab)
}


#' The rule in one table
#' @param .dur Tibble from dte_duration().
#' @param .release Tibble from dte_release().
#' @return Invisibly the table.
dte_report_headline <- function(.dur, .release) {
  if (FALSE) {
    .dur     <- tab_dur
    .release <- tab_release
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
    "Rows in the released file",                format(nrow(.release), big.mark = ",")
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


# 9. Figures -------------------------------------------------------------------------------------------------------------

#' Which rung supplied the end, by contract type
#' @param .dur Tibble from dte_duration().
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
#' @param .dur Tibble from dte_duration().
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
