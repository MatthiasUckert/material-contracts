# 04B3-Rules-DATE: signing, end and duration -------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# Turns parsed dates into a contract duration. Function prefix is `dte_`, so nothing here collides
# with the `ent_`, `geo_`, `mny_`, `red_` or `plot_` layers.
#
# THE NUMBER THAT MADE THIS DOCUMENT NECESSARY
# Table 3 Panel B reports contract duration with a mean of 2.38 years, a median of 0.40 and a
# standard deviation of 40.78, under a stated cap of 30 years. A variable supported on [0, 30] cannot
# have a standard deviation above 15 -- the maximum, reached by a 50/50 split on the endpoints. So
# the cap is not applied to what is tabulated, or something is in the wrong unit. Referee 1 saw the
# symptom and asked whether there were "errors in identifying dates affecting the right tail".
#
# The published definition is the first row of the sweep rather than a thing this document replaces
# sight unseen, and SdOverCap appears in every row: it cannot exceed one half.
#
# NO CLUSTERING, AND THAT IS A CORRECTION
# An earlier version collapsed each document to one row per calendar date, keeping the earliest
# occurrence. It bought nothing -- the duration is a max() and a min() over VALUES, which do not care
# how many times a value appears -- and it created a bug: the termination cue was then tested at the
# earliest occurrence, so "shall expire on December 31, 2020" was invisible whenever that date had
# already appeared in the preamble, which in a contract is the normal case. Everything below runs at
# SPAN level and deduplicates only where a count is reported.
#
# NO REGION RULE, EITHER
# Dates are not bimodal the way organisations are. Measured over the sample: the head is 72.8%
# pre-filing and the TAIL is 73.0% -- identical -- against a deep U for organisations with 22.6% and
# 25.2% in the two ends. Region separates the head from the middle and separates nothing else, and
# excluding the tail from the end search cost 2.5 points of coverage and left the standard deviation
# unchanged. So the end rule reads the whole document, and the head restriction on the START is a
# swept row rather than a rule.
#
# THE THREE THINGS A CONTRACT CAN SAY ABOUT WHEN IT ENDS
#   1. A STATED TERM -- "for a period of five (5) years from the Effective Date". A duration with no
#      end date at all, and once the start is known it is arithmetic. This is what the contract SAYS,
#      rather than a maximum over dates it happens to mention, so it outranks the other two.
#   2. A DATED TERMINATION -- a future date with a termination cue beside it.
#   3. NOTHING, in which case the farthest future date is taken and recorded as such. That is the
#      published definition, and it picks up a patent expiry and a perpetuity boilerplate along with
#      the contract's own end.
# DurationSource distinguishes all three, because they are not the same evidence.
#
# THE CAP DROPS, IT DOES NOT WINSORISE. A duration of exactly thirty years that is not one is worse
# than a missing value.
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
  .key    = "DateRegion",
  .levels = c("head", "middle", "tail"),
  .short  = c("head", "middle", "tail")
)

plot_register_levels(
  .key    = "DurationSource",
  .levels = c("term", "cue", "maxdate", "none"),
  .short  = c("term", "cue", "maxdate", "none")
)

plot_register_levels(
  .key    = "DateSide",
  .levels = c("before filing", "after filing", "same day"),
  .short  = c("before", "after", "same")
)

plot_register_levels(
  .key    = "NoFuture",
  .levels = c("has future date", "term stated", "all before filing", "no dates"),
  .short  = c("future", "term", "all past", "none")
)


# 2. Cues and terms ----------------------------------------------------------------------------------------------------
# TWO SMALL VOCABULARIES AND A HIT RATE FOR EVERY ENTRY. Neither list is defended by argument: the
# report prints how often each term fires and on what share of future dates, and a term that fires
# everywhere without marking a termination is dropped from the list rather than explained.
#
# THROUGH and UNTIL are the entries under suspicion -- both are common enough to appear beside any
# date at all -- and the per-term table is where that gets settled.

.dte_end_cue <- c("EXPIR", "TERMINAT", "MATURIT", "UNTIL", "THROUGH", "SHALL END", "ENDS ON",
                  "ENDING", "TERM SHALL", "TERM OF THIS")


# Written-out numbers, because a contract writes "five (5) years" as often as "5 years" and about as
# often as "five years". Where the parenthesised digit is present it wins, since it is the drafter's
# own disambiguation.
.dte_number <- c(ONE = 1, TWO = 2, THREE = 3, FOUR = 4, FIVE = 5, SIX = 6, SEVEN = 7, EIGHT = 8,
                 NINE = 9, TEN = 10, ELEVEN = 11, TWELVE = 12, THIRTEEN = 13, FOURTEEN = 14,
                 FIFTEEN = 15, SIXTEEN = 16, SEVENTEEN = 17, EIGHTEEN = 18, NINETEEN = 19,
                 TWENTY = 20, THIRTY = 30, FORTY = 40, FIFTY = 50)

.dte_ordinal <- c(FIRST = 1, SECOND = 2, THIRD = 3, FOURTH = 4, FIFTH = 5, SIXTH = 6, SEVENTH = 7,
                  EIGHTH = 8, NINTH = 9, TENTH = 10, FIFTEENTH = 15, TWENTIETH = 20)

.dte_unit_years <- c(DAY = 1 / 365.25, WEEK = 7 / 365.25, MONTH = 1 / 12, YEAR = 1)


# 3. Input -------------------------------------------------------------------------------------------------------------

#' Build one duration specification
#'
#' @param .start Character. "latest" takes the latest date at or before the filing, wherever it sits;
#'   "head" restricts that to the head of the document; "filed" is the published definition and the
#'   reference row. LATEST rather than HEAD by default because an amendment citing a 1998 original
#'   still signs itself later, and the latest pre-filing date finds that whether or not it sits in a
#'   preamble.
#' @param .end Character. "term" is the full cascade -- stated term, then dated termination, then the
#'   farthest future date. "cue" drops the term arm; "any" is the published definition.
#' @param .head_floor Integer. Head width in characters, used only when .start is "head".
#' @param .head_share Numeric. Head share of document length, taken as max() with the floor.
#' @param .tail_share Numeric. Tail share, reported in the region table and used by no rule.
#' @param .cue_win Integer. Characters read before a date for the termination cue.
#' @param .cap_years Numeric. Durations beyond this are DROPPED, not winsorised: a duration of
#'   exactly the cap that is not one is worse than a missing value. Inf is the uncapped reference.
#' @param .require_year Logical. Use only spans that carry a four-digit year. A span without one had
#'   its year inferred by the parser, and the parser infers the present year -- which is how "Section
#'   3-1" became 2026-03-01 and "Exhibit 10-15" became 2026-10-15. Swept rather than imposed, because
#'   a contract does write "expires December 31" and mean the current year.
#' @param .term_kinds Character vector. Which stated-term families count as a duration. "term of" is
#'   what a contract calls its own duration; "period of" is a notice period, a cure period or a
#'   payment window as often as it is a term, and pooling them put 127 documents at thirty days.
#' @param .label Character or NULL. Overrides the generated label.
#' @return A named list carrying the specification.
dte_spec <- function(.start = "latest", .end = "term", .head_floor = 3000L, .head_share = 0.10,
                     .tail_share = 0.20, .cue_win = 120L, .cap_years = 30, .require_year = FALSE,
                     .term_kinds = c("term of", "anniversary"), .label = NULL) {
  if (FALSE) {
    .start      <- "latest"
    .end        <- "term"
    .head_floor <- 3000L
    .head_share <- 0.10
    .tail_share <- 0.20
    .cue_win      <- 120L
    .cap_years    <- 30
    .require_year <- FALSE
    .term_kinds   <- c("term of", "anniversary")
    .label        <- NULL
  }

  if (!.start %in% c("latest", "head", "filed")) {
    cli::cli_abort("{.arg .start} must be latest, head or filed.")
  }
  if (!.end %in% c("term", "cue", "any")) cli::cli_abort("{.arg .end} must be term, cue or any.")

  lab_ <- if (!is.null(.label)) {
    .label
  } else {
    paste0(.start, " / ", .end, " / ",
           if (is.infinite(.cap_years)) "uncapped" else paste0(.cap_years, "y"),
           if (.require_year) " / year" else "")
  }

  list(Start = .start, End = .end, HeadFloor = as.integer(.head_floor), HeadShare = .head_share,
       TailShare = .tail_share, CueWin = as.integer(.cue_win), CapYears = .cap_years,
       RequireYear = isTRUE(.require_year), TermKinds = .term_kinds, Label = lab_)
}


#' Load the date spans from both engines and keep the ones that parsed
#'
#' Both engines parse nearly everything they emit -- 99.2% and 99.6% -- so the unparsed remainder is
#' a rounding error rather than a population. That is worth saying because the same is not true of
#' spaCy, which the corpus pass does not run.
#'
#' NO CLUSTERING. Every span survives, because the cue has to be tested where the drafter wrote it
#' and a collapsed row can only be tested once.
#'
#' @param .db_path 04A's candidate store.
#' @param .lens Tibble from ent_doc_lens().
#' @param .combos Named character vector of engine tokens.
#' @param .quiet Logical. Suppress the count messages.
#' @return Tibble: one row per span, with Combo and DateValue.
dte_load <- function(.db_path, .lens, .combos, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Input$Store
    .lens    <- tab_lens
    .combos  <- .lP$Params$Combos
    .quiet   <- FALSE
  }

  purrr::imap(.combos, function(.combo, .engine) {
    ent_load_label(
      .db_path = .db_path, .lens = .lens, .label = "date", .combo = .combo,
      .extras = c("DateValue", "DateScore"), .quiet = .quiet
    ) |>
      dplyr::mutate(Combo = .engine, .before = 1L)
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      DateValue = suppressWarnings(anytime::anydate(as.character(.data$DateValue))),
      Parsed    = !is.na(.data$DateValue)
    )
}


#' Region, gap to the filing date, and the termination cue -- per span
#'
#' The cue is read in the characters BEFORE the span, because a clause states the event and then the
#' date: "shall expire on December 31, 2020". At span level rather than after a collapse, so a date
#' written twice is tested twice and the one beside the clause counts.
#'
#' Region is carried for the report and for the swept head restriction on the start. No end rule
#' reads it: the head is 72.8% pre-filing and the tail 73.0%, so region separates the head from the
#' middle and nothing else.
#'
#' @param .dates Tibble from dte_load(), parsed rows only.
#' @param .keys Tibble from ent_anchor_keys(). Supplies DateFiled.
#' @param .path_text 04A's canonical text parquet.
#' @param .spec List from dte_spec().
#' @return .dates with Region, GapDays, Side, Before and HasEndCue added.
dte_describe <- function(.dates, .keys, .path_text, .spec) {
  if (FALSE) {
    .dates     <- tab_parsed
    .keys      <- tab_keys
    .path_text <- .lP$Input$Text
    .spec      <- .lP$Params$Spec
  }

  txt_ <- arrow::read_parquet(.path_text)
  src_ <- txt_$TextRaw[match(.dates$DocID, txt_$DocID)]

  hit_ <- function(.txt, .terms) {
    Reduce(`|`, lapply(.terms, function(.t) stringi::stri_detect_fixed(.txt, .t)))
  }

  .dates |>
    dplyr::left_join(dplyr::select(.keys, DocID, DateFiled), by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      HeadEnd   = pmax(.spec$HeadFloor, .spec$HeadShare * .data$DocLen),
      TailStart = .data$DocLen - .spec$TailShare * .data$DocLen,
      Region    = dplyr::case_when(
        .data$Start <  .data$HeadEnd   ~ "head",
        .data$Start >= .data$TailStart ~ "tail",
        TRUE                           ~ "middle"
      ),
      GapDays = as.integer(.data$DateValue - .data$DateFiled),
      Side    = dplyr::case_when(
        is.na(.data$GapDays) ~ NA_character_,
        .data$GapDays < 0L   ~ "before filing",
        .data$GapDays > 0L   ~ "after filing",
        TRUE                 ~ "same day"
      ),
      Before = stringi::stri_trans_toupper(
        stringi::stri_replace_all_regex(
          stringi::stri_sub(src_, from = pmax(1L, .data$Start + 1L - .spec$CueWin),
                            to = .data$Start),
          "\\s+", " "
        )
      ),
      HasEndCue = hit_(.data$Before, .dte_end_cue),
      # A SPAN WITH NO FOUR-DIGIT YEAR HAD ITS YEAR INFERRED, and the parser infers the present one.
      # That is where the tail comes from: "Section 3-1" parsed to 2026-03-01, "Exhibit 10-15" to
      # 2026-10-15, "10-35 Second" to 2035-10-01. All of them are section and exhibit references, and
      # none is a date the contract wrote. The guard is upstream of these rules in the extractor;
      # this column is what makes the damage countable and optionally removable.
      HasYear = stringi::stri_detect_regex(.data$Span, "\\d{4}")
    )
}


# 4. The stated term ---------------------------------------------------------------------------------------------------

#' Terms a contract states in words rather than as a date
#'
#' "for a period of five (5) years from the Effective Date" is a duration with no end date at all,
#' and once the start is known it is arithmetic. It is also what the contract SAYS, against a maximum
#' over dates it merely mentions, which is why it outranks the other two arms.
#'
#' TWO FAMILIES, DELIBERATELY. A period or term of N units, and an Nth anniversary. A contract has
#' many ways of stating a term and this catches two of them; the rest is reported as absent rather
#' than chased, because the point is to reduce noise and not to eliminate it.
#'
#' The FIRST match in the document is taken, since the term clause precedes the schedules that
#' restate parts of it, and the number of matches is carried so a document stating several is
#' visible rather than silently resolved.
#'
#' @param .path_text 04A's canonical text parquet.
#' @param .quiet Logical. Suppress the count message.
#' @return Tibble: DocID, TermYears, TermText, TermKind, NTermMatch. One row per document that states
#'   a term; documents stating none are absent.
dte_term <- function(.path_text, .quiet = FALSE) {
  if (FALSE) {
    .path_text <- .lP$Input$Text
    .quiet     <- FALSE
  }

  num_ <- paste(names(.dte_number), collapse = "|")
  ord_ <- paste(names(.dte_ordinal), collapse = "|")
  unt_ <- paste(names(.dte_unit_years), collapse = "|")

  # TERM OF AND PERIOD OF ARE SPLIT, and the split is the point. "term of three (3) years" is what a
  # contract calls its own duration; "period of thirty (30) days" is a notice period, a cure period
  # or a payment window, and pooling the two put 127 documents at 30 days and dragged the median of
  # credit agreements to one year. Both families are extracted, both are reported, and the caller
  # chooses which count as a duration.
  rx_of_ <- function(.lead) {
    paste0(.lead, "\\s+OF\\s+(?:APPROXIMATELY\\s+)?(", num_,
           "|\\d{1,3})\\s*(?:\\(\\s*(\\d{1,3})\\s*\\))?\\s+(", unt_, ")S?\\b")
  }
  rx_term_   <- rx_of_("TERM")
  rx_period_ <- rx_of_("PERIOD")
  rx_anniv_  <- paste0("(", ord_, "|\\d{1,2})(?:ST|ND|RD|TH)?\\s+ANNIVERSARY")

  body_ <- arrow::read_parquet(.path_text) |>
    dplyr::transmute(DocID, Body = stringi::stri_trans_toupper(
      stringi::stri_replace_all_regex(.data$TextRaw, "\\s+", " ")
    ))

  # Numbers arrive as a word or as digits; the parenthesised digit wins where the drafter supplied
  # both, because it is their own disambiguation of their own sentence.
  as_num_ <- function(.word, .digit, .map) {
    dplyr::case_when(
      !is.na(.digit) & nzchar(.digit)             ~ suppressWarnings(as.numeric(.digit)),
      stringi::stri_detect_regex(.word, "^\\d+$") ~ suppressWarnings(as.numeric(.word)),
      TRUE                                        ~ unname(.map[.word])
    )
  }

  of_ <- function(.rx, .kind) {
    body_ |>
      dplyr::mutate(M = stringi::stri_match_first_regex(.data$Body, .rx)) |>
      dplyr::mutate(
        NMatch    = stringi::stri_count_regex(.data$Body, .rx),
        TermN     = as_num_(.data$M[, 2], .data$M[, 3], .dte_number),
        TermYears = .data$TermN * unname(.dte_unit_years[.data$M[, 4]]),
        TermText  = .data$M[, 1],
        TermKind  = .kind
      ) |>
      dplyr::filter(!is.na(.data$TermYears), .data$TermYears > 0) |>
      dplyr::select(DocID, TermYears, TermText, TermKind, NTermMatch = NMatch)
  }

  term_   <- of_(.rx = rx_term_,   .kind = "term of")
  period_ <- of_(.rx = rx_period_, .kind = "period of")

  ann_ <- body_ |>
    dplyr::mutate(M = stringi::stri_match_first_regex(.data$Body, rx_anniv_)) |>
    dplyr::mutate(
      NMatch    = stringi::stri_count_regex(.data$Body, rx_anniv_),
      TermYears = as_num_(.data$M[, 2], NA_character_, .dte_ordinal),
      TermText  = .data$M[, 1],
      TermKind  = "anniversary"
    ) |>
    dplyr::filter(!is.na(.data$TermYears), .data$TermYears > 0) |>
    dplyr::select(DocID, TermYears, TermText, TermKind, NTermMatch = NMatch)

  # PRECEDENCE, not union. A document saying "term of three years" and "period of thirty days" has
  # said one thing about its duration and one thing about its notice, and the first is the answer.
  out_ <- dplyr::bind_rows(term_, ann_, period_) |>
    dplyr::mutate(KindRank = match(.data$TermKind, c("term of", "anniversary", "period of"))) |>
    dplyr::slice_min(order_by = .data$KindRank, n = 1L, by = DocID, with_ties = FALSE) |>
    dplyr::select(-KindRank)

  if (!.quiet) {
    cli::cli_alert_success(
      "Stated term found in {format(nrow(out_), big.mark = ',')} \\
       document{cli::qty(nrow(out_))}{?s}."
    )
  }
  out_
}


# 5. The duration ------------------------------------------------------------------------------------------------------

#' One start, one end and a duration per document
#'
#' THE START is the latest date at or before the filing -- optionally restricted to the head, which
#' is a swept row. An amendment citing a 1998 original still signs itself later, and the latest
#' pre-filing date finds that wherever it sits.
#'
#' THE END cascades: a stated term added to the start, then the farthest future date carrying a
#' termination cue, then the farthest future date. DurationSource says which, because they are three
#' different kinds of evidence and pooling them would hide that the third is a maximum over an
#' unfiltered set.
#'
#' @param .dates Tibble from dte_describe().
#' @param .terms Tibble from dte_term().
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

  # The year guard applies to every date this rule reads, start and end alike: a span whose year the
  # parser inferred is no more trustworthy as a signing date than as an expiry.
  src_ <- if (isTRUE(.spec$RequireYear)) dplyr::filter(.dates, .data$HasYear) else .dates

  terms_ <- dplyr::filter(.terms, .data$TermKind %in% .spec$TermKinds)

  past_ <- src_ |>
    dplyr::filter(!is.na(.data$GapDays), .data$GapDays <= 0L)
  if (identical(.spec$Start, "head")) past_ <- dplyr::filter(past_, .data$Region == "head")

  signed_ <- past_ |>
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
      NSpans   = dplyr::n(),
      NDates   = dplyr::n_distinct(.data$DateValue),
      NFuture  = dplyr::n_distinct(.data$DateValue[.data$GapDays > 0L]),
      NCued    = dplyr::n_distinct(.data$DateValue[.data$HasEndCue & .data$GapDays > 0L]),
      .by = DocID
    )

  .keys |>
    dplyr::select(DocID, Class, AmendType, DateFiled) |>
    dplyr::left_join(seen_,    by = dplyr::join_by(DocID)) |>
    dplyr::left_join(signed_,  by = dplyr::join_by(DocID)) |>
    dplyr::left_join(end_any_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(end_cue_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(dplyr::select(terms_, DocID, TermYears, TermKind),
                     by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      dplyr::across(c(NSpans, NDates, NFuture, NCued),
                    \(.x) as.integer(dplyr::coalesce(.x, 0L))),
      DateStart = if (identical(.spec$Start, "filed")) {
        .data$DateFiled
      } else {
        dplyr::coalesce(.data$DateSigned, .data$DateFiled)
      },
      StartSource = dplyr::case_when(
        identical(.spec$Start, "filed") ~ "filed",
        !is.na(.data$DateSigned)        ~ "signed",
        TRUE                            ~ "filed"
      ),
      UseTerm = identical(.spec$End, "term") & !is.na(.data$TermYears),
      UseCue  = !.data$UseTerm & .spec$End %in% c("term", "cue") & !is.na(.data$EndCue),
      EndTerm = .data$DateStart + round(.data$TermYears * 365.25),
      DateEnd = dplyr::case_when(
        .data$UseTerm ~ .data$EndTerm,
        .data$UseCue  ~ .data$EndCue,
        TRUE          ~ .data$EndAny
      ),
      DurationSource = dplyr::case_when(
        .data$UseTerm            ~ "term",
        .data$UseCue             ~ "cue",
        !is.na(.data$DateEnd)    ~ "maxdate",
        TRUE                     ~ "none"
      ),
      # A stated term that REPLACED a future date the document also carried, rather than filling a
      # gap. Defensible -- a term is what the contract says about itself -- but it is a large silent
      # substitution and this is what makes it countable.
      TermOverrode = .data$UseTerm & !is.na(.data$EndAny),
      RawYears = as.numeric(.data$DateEnd - .data$DateStart) / 365.25,
      IsCapped = !is.na(.data$RawYears) & .data$RawYears > .spec$CapYears,
      IsNeg    = !is.na(.data$RawYears) & .data$RawYears < 0,
      # DROPPED, not winsorised. A duration of exactly the cap that is not one is worse than missing.
      DurationYears = dplyr::if_else(
        !is.na(.data$RawYears) & !.data$IsNeg & !.data$IsCapped, .data$RawYears, NA_real_
      ),
      DurationDays = as.integer(round(.data$DurationYears * 365.25))
    )
}


#' Reduce one specification to a comparable row
#'
#' SdOverCap IS THE DIAGNOSTIC. On non-negative support under a correctly applied cap the standard
#' deviation cannot exceed half the cap. Table 3 Panel B reports 40.78 against a stated cap of 30, a
#' ratio of 1.36, which no distribution on [0, 30] can produce.
#'
#' @param .dur Tibble from dte_duration().
#' @param .spec List from dte_spec().
#' @return One-row tibble.
dte_row <- function(.dur, .spec) {
  if (FALSE) {
    .dur  <- tab_dur
    .spec <- .lP$Params$Spec
  }

  d_ <- .dur$DurationYears[!is.na(.dur$DurationYears)]

  tibble::tibble(
    Spec       = .spec$Label,
    Start      = .spec$Start,
    End        = .spec$End,
    Cap        = .spec$CapYears,
    PctWithEnd = mean(!is.na(.dur$DateEnd)),
    PctTerm    = mean(.dur$DurationSource == "term"),
    PctCue     = mean(.dur$DurationSource == "cue"),
    PctMaxDate = mean(.dur$DurationSource == "maxdate"),
    PctOverride = mean(.dur$TermOverrode, na.rm = TRUE),
    PctCapped  = mean(.dur$IsCapped, na.rm = TRUE),
    PctNeg     = mean(.dur$IsNeg, na.rm = TRUE),
    N          = length(d_),
    Mean       = if (length(d_) == 0L) NA_real_ else mean(d_),
    Median     = if (length(d_) == 0L) NA_real_ else stats::median(d_),
    P25        = if (length(d_) == 0L) NA_real_ else unname(stats::quantile(d_, 0.25)),
    P75        = if (length(d_) == 0L) NA_real_ else unname(stats::quantile(d_, 0.75)),
    Sd         = if (length(d_) < 2L) NA_real_ else stats::sd(d_),
    SdOverCap  = if (length(d_) < 2L || is.infinite(.spec$CapYears)) {
      NA_real_
    } else {
      stats::sd(d_) / .spec$CapYears
    }
  )
}


#' Sweep the specifications over ONE description
#'
#' THE DESCRIPTION IS COMPUTED ONCE, and that is the whole of the performance story. An earlier
#' version called it per specification: eight reads of the canonical text and eight passes of the cue
#' search over 102,005 spans, to produce eight IDENTICAL tables, because no specification differs in
#' any field the description reads. Six minutes became the cost of computing the same thing eight
#' times.
#'
#' The fields that would break that are asserted rather than assumed, so a specification varying the
#' cue window or the region bounds fails loudly here instead of silently reusing the wrong
#' description.
#'
#' @param .desc Tibble from dte_describe().
#' @param .terms Tibble from dte_term().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .specs List of lists from dte_spec().
#' @param .quiet Logical. Suppress the progress bar.
#' @return Tibble: one row per specification.
dte_sweep <- function(.desc, .terms, .keys, .specs, .quiet = FALSE) {
  if (FALSE) {
    .desc  <- tab_desc
    .terms <- tab_terms
    .keys  <- tab_keys
    .specs <- .lP$Params$Specs
    .quiet <- FALSE
  }

  fixed_ <- c("HeadFloor", "HeadShare", "TailShare", "CueWin")
  seen_  <- unique(purrr::map_chr(.specs, function(.s) paste(unlist(.s[fixed_]), collapse = "|")))
  if (length(seen_) > 1L) {
    cli::cli_abort(
      "Specifications differ in {.field {fixed_}}, which the shared description already fixed. \
       Recompute dte_describe() per specification, or hold those fields constant."
    )
  }

  purrr::map(.specs, function(.s) {
    dte_row(
      .dur  = dte_duration(.dates = .desc, .terms = .terms, .keys = .keys, .spec = .s),
      .spec = .s
    )
  }, .progress = !.quiet) |>
    purrr::list_rbind()
}


# 6. Description -------------------------------------------------------------------------------------------------------

#' How often each cue term fires, and on what
#'
#' THE LIST IS NOT DEFENDED BY ARGUMENT. A term that appears beside a large share of ALL dates
#' without marking a termination is a word the language uses for other things -- THROUGH and UNTIL
#' are the two under suspicion -- and this table is where that is settled. PctFuture is the
#' discriminating column: a real termination cue sits beside a future date far more often than
#' chance.
#'
#' @param .dates Tibble from dte_describe().
#' @return Tibble: one row per cue term.
dte_cue_hits <- function(.dates) {
  if (FALSE) .dates <- tab_desc

  base_ <- mean(.dates$GapDays > 0L, na.rm = TRUE)

  purrr::map(.dte_end_cue, function(.t) {
    hit_ <- stringi::stri_detect_fixed(.dates$Before, .t)
    tibble::tibble(
      Cue       = .t,
      Spans     = sum(hit_),
      PctOfAll  = mean(hit_),
      PctFuture = mean(.dates$GapDays[hit_] > 0L, na.rm = TRUE),
      Lift      = mean(.dates$GapDays[hit_] > 0L, na.rm = TRUE) / base_
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(dplyr::desc(.data$Lift))
}


#' Why a document has no future date
#'
#' NOT A COVERAGE GAP UNTIL IT IS NAMED. A contract stating "for a period of five years" has said
#' exactly when it ends and named no date to say it with, so counting it as missing would report a
#' drafting convention as an extraction failure.
#'
#' @param .dur Tibble from dte_duration().
#' @return .dur's documents with a NoFuture bucket.
dte_nofuture <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  .dur |>
    dplyr::transmute(
      DocID, Class,
      NoFuture = dplyr::case_when(
        .data$NFuture > 0L       ~ "has future date",
        !is.na(.data$TermYears)  ~ "term stated",
        .data$NDates > 0L        ~ "all before filing",
        TRUE                     ~ "no dates"
      )
    )
}


#' How ambiguous the numeric date formats are
#'
#' Only a numeric date whose two leading components are both twelve or under is ambiguous at all.
#' Measured once so the footnote carries a number: a swapped month and day moves a date by at most
#' thirty days against a duration reported in years.
#'
#' @param .dates Tibble from dte_load().
#' @return Tibble: one row.
dte_daymonth <- function(.dates) {
  if (FALSE) .dates <- tab_dates

  num_ <- .dates |>
    dplyr::filter(
      stringi::stri_detect_regex(.data$Span, "^\\s*\\d{1,2}[/.-]\\d{1,2}[/.-]\\d{2,4}\\s*$")
    ) |>
    dplyr::mutate(
      A = as.integer(stringi::stri_extract_first_regex(.data$Span, "\\d{1,2}")),
      B = as.integer(stringi::stri_match_first_regex(.data$Span, "\\d{1,2}[/.-](\\d{1,2})")[, 2])
    )

  tibble::tibble(
    NumericSpans   = nrow(num_),
    Ambiguous      = sum(num_$A <= 12L & num_$B <= 12L, na.rm = TRUE),
    DayFirstOnly   = sum(num_$A > 12L, na.rm = TRUE),
    MonthFirstOnly = sum(num_$A <= 12L & num_$B > 12L, na.rm = TRUE)
  ) |>
    dplyr::mutate(
      PctAmbiguous  = .data$Ambiguous / pmax(.data$NumericSpans, 1L),
      PctDayFirstUn = .data$DayFirstOnly / pmax(.data$DayFirstOnly + .data$MonthFirstOnly, 1L)
    )
}


# 7. Report ------------------------------------------------------------------------------------------------------------

#' What each engine found and how much of it parsed
#' @param .dates Tibble from dte_load().
#' @return Invisibly the summary.
dte_report_parse <- function(.dates) {
  if (FALSE) .dates <- tab_dates

  cli::cli_h2("Dates found and dates parsed")
  out_ <- .dates |>
    dplyr::summarise(
      Spans     = dplyr::n(),
      Docs      = dplyr::n_distinct(.data$DocID),
      PctParsed = mean(.data$Parsed),
      .by = Combo
    )

  out_ |>
    dplyr::mutate(PctParsed = tbl_pct(.data$PctParsed)) |>
    tbl_say(.title = "Every date span, by engine")

  cli::cli_alert_info(
    "Both engines parse nearly everything they emit, so what follows is bounded by what a contract \\
     WRITES as a date rather than by what an extractor can read. A document with no future date has \\
     not defeated the parser; it has said something else, and the table further down says what."
  )
  invisible(out_)
}


#' How many spans had their year inferred rather than written
#'
#' THE TAIL IS THIS. A span with no four-digit year gets the present year from the parser, and the
#' read block shows what that produces: "Section 3-1" as 2026-03-01, "Exhibit 10-15" as 2026-10-15,
#' "10-35 Second" as 2035-10-01. None is a date the contract wrote. It is an extractor behaviour and
#' cannot be fixed here, but it can be counted and it can be filtered.
#'
#' @param .dates Tibble from dte_describe().
#' @return Invisibly the summary.
dte_report_year <- function(.dates) {
  if (FALSE) .dates <- tab_desc

  cli::cli_h2("Spans carrying their own year")
  out_ <- .dates |>
    dplyr::summarise(
      Spans      = dplyr::n(),
      PctWithYear = mean(.data$HasYear),
      PctFuture   = mean(.data$GapDays > 0L, na.rm = TRUE),
      MedGap      = stats::median(.data$GapDays, na.rm = TRUE),
      .by = c(Combo, HasYear)
    ) |>
    dplyr::arrange(.data$Combo, dplyr::desc(.data$HasYear))

  out_ |>
    dplyr::select(-PctWithYear) |>
    dplyr::mutate(PctFuture = tbl_pct(.data$PctFuture)) |>
    tbl_say(.title = "By engine, split on whether the span wrote a four-digit year")

  far_ <- .dates |>
    dplyr::filter(.data$GapDays > 365L * 15L) |>
    dplyr::summarise(Spans = dplyr::n(), PctNoYear = mean(!.data$HasYear))

  far_ |>
    dplyr::mutate(PctNoYear = tbl_pct(.data$PctNoYear)) |>
    tbl_say(.title = "Among dates more than fifteen years out")

  cli::cli_alert_info(
    "A span with no four-digit year had its year INFERRED, and the parser infers the present one. \\
     The second table is the diagnosis: if the far-future dates are disproportionately year-less, \\
     the right tail is section numbers and exhibit references rather than contract terms. That is an \\
     extractor behaviour, upstream of every rule here, and the swept year guard is what this \\
     document can do about it."
  )
  invisible(out_)
}


#' The cue list, term by term
#' @param .tab Tibble from dte_cue_hits().
#' @return Invisibly .tab.
dte_report_cues <- function(.tab) {
  if (FALSE) .tab <- tab_cues

  cli::cli_h2("The cue list, term by term")
  .tab |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
                  Lift = round(.data$Lift, 2)) |>
    tbl_say(.title = "Ordered by lift over the base rate of future dates", .n = 25L)

  cli::cli_alert_info(
    "LIFT is the share of future dates among the spans a term fires on, over the share among all \\
     dates. A real termination cue sits beside a future date far more often than chance and scores \\
     well above one; a term scoring near one is a word the language uses for other things and is \\
     carrying no information. THROUGH and UNTIL are the two entries this table exists to judge."
  )
  invisible(.tab)
}


#' Terms stated in words
#' @param .terms Tibble from dte_term().
#' @param .n_docs Integer. Documents in the sample.
#' @return Invisibly the summary.
dte_report_terms <- function(.terms, .n_docs) {
  if (FALSE) {
    .terms  <- tab_terms
    .n_docs <- nrow(tab_keys)
  }

  cli::cli_h2("Terms stated in words")
  out_ <- .terms |>
    dplyr::summarise(
      Docs      = dplyr::n(),
      MedYears  = stats::median(.data$TermYears),
      P90Years  = unname(stats::quantile(.data$TermYears, 0.9)),
      PctSubYear = mean(.data$TermYears < 1),
      MedMatch  = stats::median(.data$NTermMatch),
      .by = TermKind
    ) |>
    dplyr::mutate(PctOfSample = .data$Docs / .n_docs)

  out_ |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "By the family that matched")

  .terms |>
    dplyr::summarise(Docs = dplyr::n(), .by = TermYears) |>
    dplyr::arrange(dplyr::desc(.data$Docs)) |>
    tbl_say(.title = "The commonest stated terms, in years", .n = 12L)

  cli::cli_alert_info(
    "PCTSUBYEAR IS WHY THE FAMILIES ARE SPLIT. \"TERM OF three years\" is what a contract calls its \\
     own duration; \"PERIOD OF thirty days\" is a notice period, a cure period or a payment window, \\
     and a family whose terms are mostly under a year is measuring the second thing. Only the \\
     families named in the specification count as a duration; the rest are extracted, reported and \\
     left alone."
  )
  invisible(out_)
}


#' Where dates sit and which side of the filing they fall on
#' @param .dates Tibble from dte_describe().
#' @return Invisibly the summary.
dte_report_region <- function(.dates) {
  if (FALSE) .dates <- tab_desc

  cli::cli_h2("Region against the filing date")
  out_ <- .dates |>
    dplyr::filter(!is.na(.data$Side)) |>
    dplyr::summarise(Dates = dplyr::n(), MedGap = stats::median(.data$GapDays),
                     PctCue = mean(.data$HasEndCue), .by = c(Region, Side)) |>
    dplyr::mutate(Share = .data$Dates / sum(.data$Dates), .by = Region) |>
    dplyr::arrange(plot_factor(.data$Region, .key = "DateRegion"),
                   plot_factor(.data$Side, .key = "DateSide"))

  out_ |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
                  Share = tbl_pct(.data$Share)) |>
    tbl_say(.title = "Reported, and read by no end rule")

  cli::cli_alert_info(
    "REPORTED AND NOT USED. The head is 72.8% pre-filing and the TAIL is 73.0% -- region separates \\
     the head from the middle and separates nothing else, against a deep U for organisations. \\
     Excluding the tail from the end search cost two and a half points of coverage and left the \\
     standard deviation unchanged, so no end rule reads this table and the head restriction on the \\
     START is a swept row rather than a rule."
  )
  invisible(out_)
}


#' Why a document has no future date
#' @param .tab Tibble from dte_nofuture().
#' @return Invisibly the summary.
dte_report_nofuture <- function(.tab) {
  if (FALSE) .tab <- tab_nofut

  cli::cli_h2("Documents with no future date")
  out_ <- .tab |>
    dplyr::summarise(Docs = dplyr::n(), .by = c(Class, NoFuture)) |>
    dplyr::mutate(Share = .data$Docs / sum(.data$Docs), .by = Class) |>
    dplyr::select(-Docs) |>
    tidyr::pivot_wider(names_from = NoFuture, values_from = Share, values_fill = 0) |>
    dplyr::arrange(plot_factor(.data$Class, .key = "ClassDetailed"))

  out_ |>
    dplyr::mutate(dplyr::across(-Class, \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "One row per contract type")

  all_ <- .tab |>
    dplyr::summarise(Docs = dplyr::n(), .by = NoFuture) |>
    dplyr::mutate(Share = tbl_pct(.data$Docs / sum(.data$Docs))) |>
    dplyr::arrange(plot_factor(.data$NoFuture, .key = "NoFuture"))

  tbl_say(.tab = all_, .title = "Over the sample")

  cli::cli_alert_info(
    "TERM STATED is not a gap. A contract writing \"for a period of five years\" has said exactly \\
     when it ends and named no date to say it with, so counting it as missing would report a \\
     drafting convention as an extraction failure. ALL BEFORE FILING is an amendment or a plan that \\
     genuinely states no end, and NO DATES is the only bucket that is an extraction question."
  )
  invisible(out_)
}


#' The numeric-format exposure
#' @param .tab Tibble from dte_daymonth().
#' @return Invisibly .tab.
dte_report_daymonth <- function(.tab) {
  if (FALSE) .tab <- tab_dm

  cli::cli_h2("Numeric date formats")
  .tab |>
    dplyr::mutate(PctAmbiguous = tbl_pct(.data$PctAmbiguous),
                  PctDayFirstUn = tbl_pct(.data$PctDayFirstUn)) |>
    tbl_say(.title = "How much of the sample the convention could move")

  cli::cli_alert_info(
    "Among the UNAMBIGUOUS numeric dates, PctDayFirstUn is the share written day first. Near zero \\
     makes US convention the right assumption, and the residual exposure is the ambiguous share \\
     times that rate times at most thirty days, against a duration reported in years."
  )
  invisible(.tab)
}


#' The specification sweep
#' @param .tab Tibble from dte_sweep().
#' @return Invisibly .tab.
dte_report_sweep <- function(.tab) {
  if (FALSE) .tab <- tab_sweep

  cli::cli_h2("Duration, by specification")
  .tab |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
                  dplyr::across(c(Mean, Median, P25, P75, Sd, SdOverCap), \(.x) round(.x, 2))) |>
    tbl_say(.title = "Each row is one start, one end cascade, one cap and one year guard", .n = 30L)

  cli::cli_alert_info(
    "SdOverCap CANNOT EXCEED ONE HALF on non-negative support under a correctly applied cap. Table 3 \\
     Panel B reports 40.78 against a stated cap of 30, a ratio of 1.36, which no distribution on \\
     [0, 30] can produce. PctTerm, PctCue and PctMaxDate are the three kinds of evidence the end \\
     rests on, and the third is the published definition -- a maximum over an unfiltered set."
  )
  invisible(.tab)
}


#' The released duration by contract type
#' @param .dur Tibble from dte_duration().
#' @return Invisibly the summary.
dte_report_class <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  cli::cli_h2("Duration by contract type")

  f_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs      = dplyr::n(),
      MeanDates = mean(.data$NDates),
      PctEnd    = mean(!is.na(.data$DateEnd)),
      PctTerm   = mean(.data$DurationSource == "term"),
      PctCue    = mean(.data$DurationSource == "cue"),
      N         = sum(!is.na(.data$DurationYears)),
      MedYears  = stats::median(.data$DurationYears, na.rm = TRUE),
      MeanYears = mean(.data$DurationYears, na.rm = TRUE),
      SdYears   = stats::sd(.data$DurationYears, na.rm = TRUE)
    )
  }

  ent_bind_all(.tab = f_(dplyr::group_by(.dur, Class)), .fun = f_, .src = .dur) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
                  dplyr::across(c(MeanDates, MedYears, MeanYears, SdYears), \(.x) round(.x, 2))) |>
    tbl_say(.title = "One row per type, and one for the sample")

  cli::cli_alert_info(
    "PctTerm and PctCue together are the share resting on something the contract SAID; the remainder \\
     rests on the farthest future date, which is a maximum over dates the contract merely mentions. \\
     A class where both are low still has a duration, and it is the weakest kind."
  )
  invisible(.dur)
}


#' Every DATE report in order
#'
#' @param .dates Tibble from dte_load().
#' @param .desc Tibble from dte_describe().
#' @param .cues Tibble from dte_cue_hits().
#' @param .terms Tibble from dte_term().
#' @param .nofut Tibble from dte_nofuture().
#' @param .daymonth Tibble from dte_daymonth().
#' @param .sweep Tibble from dte_sweep().
#' @param .dur Tibble from dte_duration().
#' @param .n_docs Integer. Documents in the sample.
#' @return Invisibly NULL.
dte_report_all <- function(.dates, .desc, .cues, .terms, .nofut, .daymonth, .sweep, .dur, .n_docs) {
  if (FALSE) {
    .dates    <- tab_dates
    .desc     <- tab_desc
    .cues     <- tab_cues
    .terms    <- tab_terms
    .nofut    <- tab_nofut
    .daymonth <- tab_dm
    .sweep    <- tab_sweep
    .dur      <- tab_dur
    .n_docs   <- nrow(tab_keys)
  }

  dte_report_parse(.dates = .dates)
  dte_report_year(.dates = .desc)
  dte_report_daymonth(.tab = .daymonth)
  dte_report_region(.dates = .desc)
  dte_report_cues(.tab = .cues)
  dte_report_terms(.terms = .terms, .n_docs = .n_docs)
  dte_report_nofuture(.tab = .nofut)
  dte_report_sweep(.tab = .sweep)
  dte_report_class(.dur = .dur)
  invisible(NULL)
}


# 8. Figures -----------------------------------------------------------------------------------------------------------

#' Gap between a date and the filing, by region
#' @param .dates Tibble from dte_describe().
#' @return A ggplot object.
dte_plot_gap <- function(.dates) {
  if (FALSE) .dates <- tab_desc

  .dates |>
    dplyr::filter(!is.na(.data$GapDays), abs(.data$GapDays) <= 3650) |>
    dplyr::mutate(Years = .data$GapDays / 365.25) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Years, colour = .data$Region)) +
    ggplot2::stat_ecdf(linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = .plot_ink, linewidth = 0.3) +
    plot_scale_colour_cat(name = NULL) +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}


#' What the end date rests on, by contract type
#' @param .dur Tibble from dte_duration().
#' @return A ggplot object.
dte_plot_source <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  .dur |>
    dplyr::summarise(N = dplyr::n(), .by = c(Class, DurationSource)) |>
    plot_bar_stacked(
      .cat      = "Class",
      .val      = "N",
      .fill     = "DurationSource",
      .key      = "ClassDetailed",
      .key_fill = "DurationSource",
      .short    = FALSE,
      .share    = TRUE
    )
}


#' Median duration by contract type
#' @param .dur Tibble from dte_duration().
#' @return A ggplot object.
dte_plot_class <- function(.dur) {
  if (FALSE) .dur <- tab_dur

  .dur |>
    dplyr::filter(!is.na(.data$DurationYears)) |>
    dplyr::summarise(MedYears = stats::median(.data$DurationYears), .by = Class) |>
    plot_bar_ranked(
      .cat      = "Class",
      .val      = "MedYears",
      .key      = "ClassDetailed",
      .short    = FALSE,
      .label    = TRUE,
      .accuracy = 0.1,
      .pct      = FALSE
    )
}
