# ======================================================================================================================
# Test-MatCon-DATE.R -- the whole DATE and TERM path, end to end, on documents whose answer is known
# ======================================================================================================================
#
# WHAT THIS TESTS
# The seam, not the extractor: fabricated text -> the CLI -> a parquet -> a DuckDB store ->
# ent_load_entity() -> dte_load / dte_load_terms -> dte_describe -> dte_duration. matcon-extract's
# pytest suite proves dateregex.py finds dates. Nothing proves a date found in Python arrives in R as
# a duration in years with the right start, the right end, and the right reason for both.
#
# DATE IS THE HARDEST OF THE FOUR, AND THESE ARE THE THREE REASONS
#
#   TWO LABELS FROM ONE MODULE. dateregex owns DATE and TERM and emits both from one pass over the
#   text, and keep_longest() is called once per label so that a long span of one kind can never
#   delete a short span of the other.
#
#   _io.py's docstring justifies that with a worked example -- "for a period of five (5) years from
#   January 1, 2020" is said to be a term AND a date that OVERLAP. RUNNING IT SHOWS THEY DO NOT. The
#   two spans come back as "period of five (5) years" and "January 1, 2020", separated by " from ":
#   adjacent, not overlapping. No TERM pattern can reach a date, because PeriodOf, ContinueFor,
#   UnitTerm and UnitPeriod all stop at the unit word and Anniversary carries no year at all.
#
#   THE RULE IS RIGHT AND THE EXAMPLE IS NOT. Two independent calls remain correct whatever the
#   patterns do next, and that is precisely their value -- they make a future pattern that DOES
#   reach across labels safe in advance. So document 01 checks what is actually true and testable:
#   both labels arrive from one pass and neither suppresses the other. The absent overlap is
#   recorded in the check's note rather than asserted away.
#
#   EVERY DATE REQUIRES A WRITTEN YEAR. It is the property the package exists for: a grammar-based
#   extractor resolves "May 1 of each year" by supplying a year of its own, and if that year comes
#   from the clock the same document yields different answers on different days.
#
#   BUT HasYear IS NOT THAT PROPERTY. 04B3 computes it as a four-digit match on the span, and
#   SlashShort dates -- 01/15/99, 03/20/20 -- carry a two-digit year the contract wrote and the
#   parser resolved through YEAR_PIVOT. They read as HasYear = FALSE while being exactly the case
#   the guard exists to protect. So 04B3's note that RequireYear is "near-vacuous under matcon,
#   which requires one anyway" IS WRONG: switching it on drops every SlashShort date as though its
#   year had been invented.
#
#   It does not bite today, because 04D sets .require_year = FALSE. The reason given for leaving it
#   off is what is wrong, and a wrong reason is what gets rediscovered expensively. Two checks below
#   separate the property from the column: one asserts no year was invented, the other pins what
#   HasYear actually reports.
#
#   THE CASCADE HAS FOUR RUNGS AND EACH IS A DIFFERENT CLAIM. A stated term, an end-cued date, the
#   farthest future date, nothing. Documents 09 to 13 walk them one at a time, and DurationSource is
#   checked alongside DurationYears every time -- a right number reached by the wrong rung is a
#   coincidence, not an answer.
#
# WHY EVERY CHECK RUNS THROUGH A GUARD
# Test-MatCon-GPE's first version asked for geo_resolve()'s internal column names, which are renamed
# at its final select(). A missing column is NULL, NULL == 1L is logical(0), and all(logical(0)) is
# TRUE -- so four checks passed on columns that do not exist. chk_on() asserts rows and columns
# BEFORE evaluating; chk_none() is the separate function for the cases where empty is the answer.
#
# THE FILING DATE IS FIXED AND EVERY FIXTURE DATE IS POSITIONED AGAINST IT. dte_duration() reads
# DateFiled off the keys and computes every gap from it, so a fixture with dates but no filing date
# would exercise none of the cascade. It is 2020-06-15 throughout, and each document says in its
# comment whether its dates sit before or after that.
#
# IT REPORTS, IT DOES NOT ABORT. Nothing outside tempdir() is written.
#
# Usage:
#   source(here::here("1_code", "_Tests", "Test-MatCon-DATE.R"), encoding = "UTF-8")

here::i_am("1_code/_Commons/_Initialize.R")

options(cli.num_colors = 1, cli.width = 120)

source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),     encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_NER.R"),        encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Entity.R"),     encoding = "UTF-8")
source(here::here("1_code", "04B3-Rules-DATE.R"),         encoding = "UTF-8")


# 1. Configuration ----

.dir_out <- fs::path(tempdir(), "Test-MatCon-DATE")
fs::dir_create(.dir_out)

.lT <- list(
  Stage  = fs::path(.dir_out, "staged_text.parquet"),
  Spans  = fs::path(.dir_out, "spans"),
  Store  = fs::path(.dir_out, "store"),
  # ONE FILING DATE FOR EVERY DOCUMENT, so a fixture date's side is a property of the date rather
  # than of which document it landed in.
  Filed  = as.Date("2020-06-15"),
  # dateregex's own constant, restated so the fixture can be read against it. It lives in
  # dateregex.py and is hashed into dateregex-v3's SPEC; a change there should fail a check here.
  Pivot  = 69L
)
# tempdir() IS PER-SESSION, NOT PER-RUN, and treating the two as the same cost a confusing abort.
# A second run inside one R session finds the first run's DuckDB file, and ner_manifest_write() then
# correctly refuses to ingest rows whose spec hash differs from what that file already holds under
# the same model tag -- "the tag has not moved but the provenance has, so the stored rows mean
# something else". The guard is right and the stale store is the fault.
#
# IT LAY DORMANT UNTIL A SPEC HASH FIRST MOVED. Four runs of this suite passed on a store that was
# never cleared, because the hash was identical every time; the lexnlp rebuild moved it and the
# ingest aborted. Phase F moves all four matcon hashes at once, so the same abort is waiting for
# every one of these files.
purrr::walk(c(.lT$Spans, .lT$Store), \(.d) if (fs::dir_exists(.d)) fs::dir_delete(.d))
# The context width matcon now stores either side of every span. It lives in
# _io.DEFAULT_CUE and in every extractor's SPEC, so a change there moves the hash and
# should fail the re-cut below rather than pass quietly.
.lT$Cue <- 160L

fs::dir_create(c(.lT$Spans, .lT$Store))

# 04B3's released specification, argument for argument as 04D declares it.
.spec <- dte_spec(
  .start        = "latest",   # the latest date at or before the filing
  .end          = "term",     # the stated term where there is one, a cue date otherwise
  .family       = "matcon",
  .head_floor   = 3000L,
  .head_share   = 0.10,
  .tail_share   = 0.20,
  .cue_win      = 120L,       # characters read before a date, for the end cue
  .cap_years    = 30,         # dropped, not winsorised
  .require_year = FALSE,      # near-vacuous under matcon, which requires one anyway
  .term_kinds   = c("UnitTerm", "ContinueFor", "PeriodOf", "Anniversary"),
  .term_floor   = 0,
  .label        = NULL
)

cli::cli_h1("Test-MatCon-DATE")
cli::cli_alert_info("Working under {.path {as.character(.dir_out)}}.")
cli::cli_alert_info("Filing date for every fixture document: {format(.lT$Filed)}.")

.checks <- list()

#' Record one check without interrupting the run
#'
#' @param .name What was checked, in the words a reader would use.
#' @param .ok Logical. NA and length zero both count as failures.
#' @param .note What the failure would mean, or what the value actually was.
#' @return Invisibly TRUE where the check passed.
chk <- function(.name, .ok, .note = "") {
  if (FALSE) {
    .name <- "A stated term wins the cascade"
    .ok   <- TRUE
    .note <- ""
  }

  # LENGTH IS CHECKED, NOT ONLY TRUTH. all(logical(0)) is TRUE, which is how four checks in the
  # first Test-MatCon-GPE passed on columns that did not exist.
  ok_ <- length(.ok) == 1L && isTRUE(.ok)
  .checks[[length(.checks) + 1L]] <<- tibble::tibble(Check = .name, Ok = ok_, Note = .note)
  if (ok_) {
    cli::cli_alert_success("{(.name)}")
  } else {
    cli::cli_alert_danger("{(.name)}{if (nzchar(.note)) paste0(' -- ', .note) else ''}")
  }
  invisible(ok_)
}

#' Check a predicate over a tibble, refusing to evaluate it on nothing
#'
#' An empty subset or an absent column makes all() and any() return a value that reads as a pass, so
#' both are refused BEFORE the predicate runs and each gets its own message -- "no rows" and "column
#' absent" have different causes and different fixes.
#'
#' @param .name What was checked.
#' @param .tab Tibble the predicate reads.
#' @param .cols Character vector of columns the predicate needs.
#' @param .fun Function of one argument, the tibble, returning a length-one logical.
#' @param .note What the failure would mean, or the value actually seen.
#' @return Invisibly TRUE where the check passed.
chk_on <- function(.name, .tab, .cols, .fun, .note = "") {
  if (FALSE) {
    .name <- "A stated term wins the cascade"
    .tab  <- dplyr::filter(tab_dur, .data$DocID == "doc09")
    .cols <- c("DurationSource", "DurationYears")
    .fun  <- \(.t) all(.t$DurationSource == "term")
    .note <- ""
  }

  if (nrow(.tab) == 0L) {
    return(chk(.name, FALSE, "no rows to test -- the subset this check reads is empty"))
  }
  gone_ <- setdiff(.cols, names(.tab))
  if (length(gone_) > 0L) {
    return(chk(.name, FALSE, paste0("column(s) absent: ", paste(gone_, collapse = ", "))))
  }
  chk(.name, .fun(.tab), .note)
}

#' Check that a subset is empty, where empty is the expected answer
#'
#' A SEPARATE FUNCTION RATHER THAN A FLAG, because chk_on()'s guard treats zero rows as a failure
#' and must keep doing so.
#'
#' @param .name What was checked.
#' @param .tab Tibble expected to have no rows.
#' @param .note What a nonzero count would mean.
#' @return Invisibly TRUE where the check passed.
chk_none <- function(.name, .tab, .note = "") {
  if (FALSE) {
    .name <- "A month with no year produces no date"
    .tab  <- dplyr::filter(tab_dates, .data$DocID == "doc04")
    .note <- ""
  }

  chk(.name, nrow(.tab) == 0L,
      if (nrow(.tab) == 0L) .note else paste0(nrow(.tab), " row(s) emitted; ", .note))
}


# 2. The fixture: sixteen documents whose right answer is known ----
#
# Every date is stated relative to a filing date of 2020-06-15. "before" means the cascade can read
# it as a signing date; "after" means it is a candidate end.

tab_docs <- tibble::tribble(
  ~DocID,   ~TextRaw,
  # -- the overlap invariant ------------------------------------------------------------------
  # 01 ONE SENTENCE, TWO LABELS, OVERLAPPING SPANS. A term and a date, both true.
  "doc01",  "This Agreement is for a period of five (5) years from January 1, 2020.",

  # -- the written year -----------------------------------------------------------------------
  # 02 a month and a year: MonthYear fires, resolving to the first of the month
  "doc02",  "This Agreement is dated as of March 2019.",
  # 03 a month and a day but NO year: nothing may be emitted, because the year would be invented
  "doc03",  "Payment shall be made on March 15 of each year without further notice.",
  # 04 a bare month name: likewise nothing
  "doc04",  "The parties shall meet in March to review performance under this Agreement.",

  # -- reading a date correctly ---------------------------------------------------------------
  # 05 the year pivot: 99 is 1999 and 20 is 2020, on the C standard's rule at 69
  "doc05",  "The original agreement was executed on 01/15/99 and amended on 03/20/20.",
  # 06 DayMonthLong must outrank MonthYear: the answer is the 3rd, not the 1st
  "doc06",  "Executed this 3rd day of March, 2011, by the parties hereto.",
  # 07 month by NAME, not by position: both orderings give the same day
  "doc07",  "Dated March 3, 2011, and countersigned 4 April 2011 at the offices of the parties.",

  # -- the term, and what it carries ----------------------------------------------------------
  # 08 the parenthesised digit is a SECOND READING of one number, never a separate one
  "doc08",  "The initial term shall be a period of seven (7) years commencing on the date hereof.",
  # 09 a stated term AND a future date: the term wins and the substitution is countable
  "doc09",  paste("This Agreement is for a period of three (3) years and all schedules expire on",
                  "December 31, 2029."),
  # 10 days and months are within a day of each other in years and are not the same thing
  "doc10",  "The notice period shall be a period of thirty (30) days from receipt of the demand.",
  # 11 the same duration stated in months
  "doc11",  "The notice period shall be a period of one (1) month from receipt of the demand.",
  # 12 open-ended: a stated term of UNKNOWN length, which is not an absent one
  "doc12",  "This Agreement shall continue in full force and effect until terminated by either party.",

  # -- the cascade, one rung per document -----------------------------------------------------
  # 13 no term; a future date carrying an end cue: the cue rung
  "doc13",  paste("This Agreement is dated as of January 10, 2020, and shall terminate on",
                  "December 31, 2023."),
  # 14 no term and no cue; future dates only: the farthest-future rung
  "doc14",  paste("This Agreement is dated as of January 10, 2020. Deliveries are scheduled for",
                  "March 1, 2022 and June 1, 2023."),
  # 15 no date of any kind: the cascade reports none, and the document still gets a row
  "doc15",  "The parties agree that all obligations hereunder shall be performed promptly.",
  # 16 a fifty-year term: past the 30-year cap, so the duration is DROPPED and not winsorised
  "doc16",  "The term of this lease shall be a period of fifty (50) years from the date hereof."
)

arrow::write_parquet(tab_docs, .lT$Stage)

cli::cli_alert_info(
  "{nrow(tab_docs)} {cli::qty(nrow(tab_docs))}fixture document{?s} staged."
)

tab_lens <- tab_docs |>
  dplyr::transmute(.data$DocID, DocLen = stringi::stri_length(.data$TextRaw))

# dte_describe() reads DateFiled and dte_duration() reads Class, AmendType and DateFiled. Nothing
# here reads the values of the first two.
tab_keys <- tab_docs |>
  dplyr::transmute(
    .data$DocID,
    Class     = "Lease",
    AmendType = "Original",
    DateFiled = .lT$Filed
  )


# 3. Extraction: the CLI, the store, the read ----
#
# BOTH LABELS ARE REQUESTED IN ONE CALL, which is how 04A and 04C request them: dateregex owns DATE
# and TERM and runs once over the text for both. Asking separately would read every document twice
# and would not test the invariant this file exists for.

tab_describe <- ner_matcon_describe() |>
  dplyr::mutate(Family = "matcon") |>
  tidyr::unnest_longer(col = "Entities", values_to = "Entity") |>
  dplyr::mutate(Note = dplyr::if_else(.data$Ready, "", "data dependency absent")) |>
  dplyr::select("Family", "Model", "Entity", "SpecHash", "Ready", "Note")

hash_spec <- tab_describe$SpecHash[tab_describe$Entity == "DATE"][[1L]]
cli::cli_alert_info("dateregex spec hash: {(hash_spec)}.")

tab_staged <- ner_extract(
  .family     = "matcon",
  .model      = NULL,
  .entity     = c("DATE", "TERM"),   # ONE module, two labels, one pass
  .path_in    = .lT$Stage,
  .out_dir    = .lT$Spans,
  .describe   = tab_describe,
  .id_col     = "DocID",
  .text_col   = "TextRaw",
  .max_chars  = 0L,
  .workers    = 1L,
  .batch_size = 8L,
  .timeout    = 60L,
  .quiet      = TRUE
)

con_matcon <- ner_db_connect(
  .db_path   = ner_db_path(.dir = .lT$Store, .family = "matcon"),
  .read_only = FALSE
)
ner_db_init(.con = con_matcon)
ner_ingest(.con = con_matcon, .staged = tab_staged, .entity = c("DATE", "TERM"))

tab_ledger <- DBI::dbGetQuery(
  con_matcon, "SELECT DocID, Entity, Status FROM runs ORDER BY DocID, Entity"
) |>
  tibble::as_tibble()

# THE RAW TERM SPANS, before dte_load_terms() collapses them to one per document. Precedence and the
# parenthesised-digit reading are properties of the spans and are invisible after the collapse.
tab_term_raw <- DBI::dbGetQuery(
  con_matcon,
  "SELECT DocID, Start, Stop, Span, LabelRaw, TermN, TermUnit, TermYears
     FROM term WHERE Start IS NOT NULL ORDER BY DocID, Start"
) |>
  tibble::as_tibble()

DBI::dbDisconnect(con_matcon, shutdown = TRUE)


# 4. The chain: load, describe, cascade ----

tab_dates <- dte_load(
  .dir_store = .lT$Store,
  .lens      = tab_lens,
  .families  = "matcon",       # the only family this file tests
  .quiet     = FALSE
)

tab_terms <- dte_load_terms(.dir_store = .lT$Store, .lens = tab_lens, .quiet = FALSE)

tab_desc <- dte_describe(
  .dates     = dplyr::filter(tab_dates, .data$Parsed),
  .keys      = tab_keys,
  .path_text = .lT$Stage,
  .spec      = .spec
)

tab_dur <- dte_duration(
  .dates = tab_desc,
  .terms = tab_terms,
  .keys  = tab_keys,
  .spec  = .spec
)

cli::cli_alert_info(
  "Chain complete: {nrow(tab_dates)} date span{?s}, {nrow(tab_term_raw)} raw term span{?s}, \\
   {nrow(tab_terms)} collapsed term{?s}, {nrow(tab_dur)} document row{?s}."
)


# 5. The checks ----

#' Parsed date spans for one document
#'
#' @param .doc DocID.
#' @return Tibble, possibly empty.
dates_for <- function(.doc) {
  if (FALSE) .doc <- "doc01"
  dplyr::filter(tab_desc, .data$DocID == .doc)
}

#' The duration row for one document
#'
#' @param .doc DocID.
#' @return One-row tibble, or empty where the cascade produced nothing.
dur_for <- function(.doc) {
  if (FALSE) .doc <- "doc09"
  dplyr::filter(tab_dur, .data$DocID == .doc)
}

#' Raw term spans for one document, before the collapse
#'
#' @param .doc DocID.
#' @return Tibble, possibly empty.
terms_for <- function(.doc) {
  if (FALSE) .doc <- "doc08"
  dplyr::filter(tab_term_raw, .data$DocID == .doc)
}

cli::cli_h2("The seam")

chk("The extractor wrote exactly one parquet", nrow(tab_staged) == 1L,
    paste0(nrow(tab_staged), " file(s); one module owns both labels, so one file"))
chk("Both labels reached the ledger",
    all(c("DATE", "TERM") %in% tab_ledger$Entity),
    paste(sort(unique(tab_ledger$Entity)), collapse = ", "))
chk("Every fixture document reached the ledger for both labels",
    dplyr::n_distinct(tab_ledger$DocID) == nrow(tab_docs),
    paste0(dplyr::n_distinct(tab_ledger$DocID), " of ", nrow(tab_docs)))
chk("Date spans were read back out of the store", nrow(tab_dates) > 0L,
    paste0(nrow(tab_dates), " span(s)"))
chk("Term spans were read back out of the store", nrow(tab_term_raw) > 0L,
    paste0(nrow(tab_term_raw), " raw span(s)"))

# text[Start:Stop] == Span over CODE POINTS. stri_sub is 1-based and inclusive; the offsets are
# 0-based and half-open.
cut_ <- stringi::stri_sub(
  tab_docs$TextRaw[match(tab_dates$DocID, tab_docs$DocID)],
  from = tab_dates$Start + 1L, to = tab_dates$Stop
)
chk("Every date span equals the text at its own offsets",
    nrow(tab_dates) > 0L && all(cut_ == tab_dates$Span),
    paste0(sum(cut_ != tab_dates$Span), " mismatch(es) over ", nrow(tab_dates), " span(s)"))

cli::cli_h2("The context columns")

# WHY THIS IS CHECKED IN EVERY TEST AND NOT ONCE. CueBefore and CueAfter exist so that no rule has
# to open a document again: dte_describe()'s end cue, mny_load()'s par filter and geo_law()'s
# governing-law window all read the characters around a span, and all three now read a column
# instead. The columns are cut in Python by _io.Emitter.cues(), which slices code points -- and the
# whole arrangement rests on that agreeing with what R would have cut. So each test re-cuts the
# window from its own fixture and compares.
#
# THE HEAD CASE IS THE ONE THAT WOULD FAIL SILENTLY. text[max(0, start - 160):start] is empty for a
# span at offset 0; text[-160:0] is also empty, but text[-160:] is the LAST 160 characters -- a
# plausible-looking value from entirely the wrong end of the document. Only a span near the head
# distinguishes the two, and a fixture without one would never notice.

#' Both cue columns, checked against the window re-cut from the fixture
#'
#' @param .tab Span table from ent_load_entity().
#' @param .what What the table holds, for the check names.
#' @return Invisibly NULL.
cue_checks <- function(.tab, .what) {
  if (FALSE) {
    .tab  <- tab_raw
    .what <- "GPE"
  }

  chk(paste0(.what, ": the cue columns are present"),
      all(c("CueBefore", "CueAfter") %in% names(.tab)),
      paste(setdiff(c("CueBefore", "CueAfter"), names(.tab)), collapse = ", "))

  if (!all(c("CueBefore", "CueAfter") %in% names(.tab)) || nrow(.tab) == 0L) return(invisible(NULL))

  txt_ <- tab_docs$TextRaw[match(.tab$DocID, tab_docs$DocID)]

  # stri_sub IS 1-BASED AND INCLUSIVE; the offsets are 0-based and half-open. The character at
  # 0-based index Start-1 is 1-based Start, which is why the before-window ends at Start and the
  # after-window begins at Stop + 1 with no further adjustment.
  want_b_ <- stringi::stri_sub(txt_, from = pmax(1L, .tab$Start - .lT$Cue + 1L), to = .tab$Start)
  want_a_ <- stringi::stri_sub(txt_, from = .tab$Stop + 1L, to = .tab$Stop + .lT$Cue)

  chk(paste0(.what, ": CueBefore is the ", .lT$Cue, " characters before the span"),
      all(.tab$CueBefore == want_b_),
      paste0(sum(.tab$CueBefore != want_b_), " mismatch(es) over ", nrow(.tab), " span(s)"))
  chk(paste0(.what, ": CueAfter is the ", .lT$Cue, " characters after it"),
      all(.tab$CueAfter == want_a_),
      paste0(sum(.tab$CueAfter != want_a_), " mismatch(es) over ", nrow(.tab), " span(s)"))

  chk(paste0(.what, ": neither column is null on any span"),
      !any(is.na(.tab$CueBefore)) && !any(is.na(.tab$CueAfter)),
      paste0(sum(is.na(.tab$CueBefore)), " null before, ", sum(is.na(.tab$CueAfter)), " null after"))

  # THE HEAD CASE. A span whose Start is under the window width must carry exactly Start characters
  # of context, not 160 taken from the tail.
  head_ <- dplyr::filter(.tab, .data$Start < .lT$Cue)
  chk(paste0(.what, ": a span near the head does not wrap to the tail"),
      nrow(head_) > 0L && all(stringi::stri_length(head_$CueBefore) == head_$Start),
      if (nrow(head_) == 0L) {
        "no span starts within the window; the fixture cannot test this"
      } else {
        paste0(nrow(head_), " span(s) start within ", .lT$Cue, " characters of the document head")
      })

  invisible(NULL)
}

cue_checks(tab_dates, "DATE")

cli::cli_h2("Two labels from one module, overlapping")

d01_date <- dplyr::filter(tab_dates, .data$DocID == "doc01")
d01_term <- terms_for("doc01")

chk_on("The sentence produced a DATE", d01_date, c("Span", "DateValue"), \(.t) nrow(.t) > 0L,
       paste(d01_date$Span, collapse = " | "))
chk_on("The same sentence produced a TERM", d01_term, c("Span", "TermYears"), \(.t) nrow(.t) > 0L,
       paste(d01_term$Span, collapse = " | "))

# WHAT IS ACTUALLY TESTABLE: both labels arrived from one pass and neither suppressed the other.
# _io.py's worked example claims these two spans OVERLAP; measured, they do not -- see the header.
ov_ <- if (nrow(d01_date) > 0L && nrow(d01_term) > 0L) {
  any(purrr::map_lgl(seq_len(nrow(d01_date)), \(.i) any(
    d01_date$Start[.i] < d01_term$Stop & d01_date$Stop[.i] > d01_term$Start
  )))
} else {
  FALSE
}

chk("One sentence yielded both labels, and neither suppressed the other",
    nrow(d01_date) > 0L && nrow(d01_term) > 0L,
    paste0("DATE and TERM from one pass; the two spans ",
           if (ov_) "overlap" else "are ADJACENT, not overlapping -- _io.py's example is wrong"))

# RECORDED RATHER THAN ASSERTED AWAY. If a future pattern makes the two labels overlap, this check
# starts failing and the note says so -- which is the moment keep_longest()'s two calls stop being
# a precaution and start being load-bearing.
chk("No TERM pattern currently reaches across into a DATE", !ov_,
    "PeriodOf, ContinueFor, UnitTerm and UnitPeriod all stop at the unit word")

cli::cli_h2("Every date needs a written year")

chk_on("A month and a year resolve to the first of that month",
       dates_for("doc02"), "DateValue",
       \(.t) any(.t$DateValue == as.Date("2019-03-01")),
       paste(format(dates_for("doc02")$DateValue), collapse = ", "))
chk_none("A month and a day with no year produce nothing",
         dplyr::filter(tab_dates, .data$DocID == "doc03"),
         "a grammar parser would supply the current year and give a different answer each day")
chk_none("A bare month name produces nothing",
         dplyr::filter(tab_dates, .data$DocID == "doc04"),
         "the package exists so that a value the text does not contain is never supplied")
# THE PROPERTY, tested directly: the year of the parsed date appears IN THE SPAN, as four digits or
# as two. A grammar parser supplying a year from the clock fails this and nothing else catches it.
chk_on("No date carries a year the text did not write", tab_desc, c("Span", "DateValue"),
       \(.t) {
         yr4_ <- format(.t$DateValue, "%Y")
         yr2_ <- stringi::stri_sub(yr4_, from = 3L, to = 4L)
         all(stringi::stri_detect_fixed(.t$Span, yr4_) |
             stringi::stri_detect_fixed(.t$Span, yr2_))
       },
       paste0(nrow(tab_desc), " span(s) checked against their own parsed year"))

# THE COLUMN, pinned separately, because it does NOT measure the property above. 04B3 computes
# HasYear as a four-digit match, so a two-digit year the contract wrote reads as no year at all.
d05_desc <- dates_for("doc05")
chk_on("HasYear is FALSE on a two-digit year, which the contract did write", d05_desc, "HasYear",
       \(.t) !any(.t$HasYear),
       paste0("SlashShort dates: ", paste(d05_desc$Span, collapse = ", "),
              " -- RequireYear = TRUE would drop them"))
chk_on("HasYear is TRUE wherever the year is written in full", tab_desc, c("HasYear", "Span"),
       \(.t) all(.t$HasYear[stringi::stri_detect_regex(.t$Span, "\\d{4}")]),
       paste0(sum(tab_desc$HasYear), " of ", nrow(tab_desc), " span(s) carry four digits"))

cli::cli_h2("Reading a date correctly")

d05 <- dates_for("doc05")
chk_on("The year pivot sends 99 to 1999", d05, "DateValue",
       \(.t) any(.t$DateValue == as.Date("1999-01-15")),
       paste(format(d05$DateValue), collapse = ", "))
chk_on("And 20 to 2020", d05, "DateValue",
       \(.t) any(.t$DateValue == as.Date("2020-03-20")),
       paste0("pivot is ", .lT$Pivot, ": 00-68 to the 2000s, 69-99 to the 1900s"))

chk_on("DayMonthLong outranks MonthYear: the day is not dropped", dates_for("doc06"),
       c("DateValue", "LabelRaw"),
       \(.t) any(.t$DateValue == as.Date("2011-03-03")),
       paste0(paste(format(dates_for("doc06")$DateValue), collapse = ", "), " via ",
              paste(dates_for("doc06")$LabelRaw, collapse = ", ")))

d07 <- dates_for("doc07")
chk_on("Month-first and day-first give the same reading", d07, "DateValue",
       \(.t) any(.t$DateValue == as.Date("2011-03-03")) &&
             any(.t$DateValue == as.Date("2011-04-04")),
       paste(format(sort(d07$DateValue)), collapse = ", "))

cli::cli_h2("The term, and what it carries")

d08 <- terms_for("doc08")
chk_on("The parenthesised digit is one number, not two", d08, c("TermN", "Span"),
       \(.t) nrow(.t) == 1L && all(.t$TermN == 7),
       paste0(nrow(d08), " span(s), TermN = ", paste(d08$TermN, collapse = ", ")))

d10 <- terms_for("doc10")
d11 <- terms_for("doc11")
chk_on("A term in days keeps its unit", d10, c("TermUnit", "TermYears"),
       \(.t) all(.t$TermUnit == "day"), paste(d10$TermUnit, collapse = ", "))
chk_on("A term in months keeps its unit", d11, c("TermUnit", "TermYears"),
       \(.t) all(.t$TermUnit == "month"), paste(d11$TermUnit, collapse = ", "))
chk("Thirty days and one month differ by under a week in years, and are not the same thing",
    nrow(d10) > 0L && nrow(d11) > 0L &&
      abs(d10$TermYears[[1L]] - d11$TermYears[[1L]]) < (7 / 365.25),
    "TermUnit is what separates a notice period from a duration; TermYears cannot")

d12 <- dplyr::filter(tab_terms, .data$DocID == "doc12")
chk_on("An open-ended term is recognised and marked", d12, c("TermKind", "IsOpen", "TermYears"),
       \(.t) all(.t$IsOpen) && all(.t$TermKind == "OpenEnded") && all(is.na(.t$TermYears)),
       paste0(paste(d12$TermKind, collapse = ", "), "; TermYears = ",
              paste(d12$TermYears, collapse = ", ")))

cli::cli_h2("The cascade, one rung at a time")

u09 <- dur_for("doc09")
chk_on("A stated term wins over a future date", u09, c("DurationSource", "TermYears"),
       \(.t) all(.t$DurationSource == "term") && all(.t$TermYears == 3),
       paste0(u09$DurationSource, ", TermYears = ", u09$TermYears))
chk_on("And the substitution is countable", u09, "TermOverrode", \(.t) all(.t$TermOverrode),
       "a term that REPLACED a future date is a large silent substitution unless it is counted")
chk_on("A three-year term gives about three years", u09, "DurationYears",
       \(.t) !is.na(.t$DurationYears) && abs(.t$DurationYears - 3) < 0.01,
       paste0("DurationYears = ", round(dplyr::coalesce(u09$DurationYears, NA_real_), 3)))

u12 <- dur_for("doc12")
chk_on("An open-ended term reports open, and no end date", u12,
       c("DurationSource", "DateEnd", "DurationYears"),
       \(.t) all(.t$DurationSource == "open") && all(is.na(.t$DateEnd)),
       paste0(u12$DurationSource, "; the old code sent these to the farthest future date"))

u13 <- dur_for("doc13")
chk_on("With no term, an end-cued date takes the cue rung", u13,
       c("DurationSource", "DateEnd"),
       \(.t) all(.t$DurationSource == "cue") && all(.t$DateEnd == as.Date("2023-12-31")),
       paste0(u13$DurationSource, ", DateEnd = ", format(u13$DateEnd)))
chk_on("The cue was found in the characters BEFORE the date", dates_for("doc13"),
       c("HasEndCue", "Before"), \(.t) any(.t$HasEndCue),
       paste0(sum(dates_for("doc13")$HasEndCue), " of ", nrow(dates_for("doc13")),
              " span(s) carry a cue"))

u14 <- dur_for("doc14")
chk_on("With no term and no cue, the farthest future date is used", u14,
       c("DurationSource", "DateEnd"),
       \(.t) all(.t$DurationSource == "maxdate") && all(.t$DateEnd == as.Date("2023-06-01")),
       paste0(u14$DurationSource, ", DateEnd = ", format(u14$DateEnd)))

u15 <- dur_for("doc15")
chk_on("A document with no dates reports none, and still gets a row", u15,
       c("DurationSource", "NSpans"),
       \(.t) all(.t$DurationSource == "none") && all(.t$NSpans == 0L),
       paste0(u15$DurationSource, ", NSpans = ", u15$NSpans))

cli::cli_h2("The start date, and the cap")

u01 <- dur_for("doc01")
chk_on("The latest pre-filing date becomes the start", u01, c("DateStart", "StartSource"),
       \(.t) all(.t$StartSource == "signed") && all(.t$DateStart == as.Date("2020-01-01")),
       paste0(u01$StartSource, ", ", format(u01$DateStart)))
chk_on("A document with no pre-filing date falls back to the filing date", u15,
       c("DateStart", "StartSource"),
       \(.t) all(.t$StartSource == "filed") && all(.t$DateStart == .lT$Filed),
       paste0(u15$StartSource, ", ", format(u15$DateStart)))

u16 <- dur_for("doc16")
chk_on("A fifty-year term is flagged as past the cap", u16, c("IsCapped", "RawYears"),
       \(.t) all(.t$IsCapped) && all(.t$RawYears > .spec$CapYears),
       paste0("RawYears = ", round(dplyr::coalesce(u16$RawYears, NA_real_), 1),
              " against a cap of ", .spec$CapYears))
chk_on("And DROPPED rather than winsorised to the cap", u16, "DurationYears",
       \(.t) all(is.na(.t$DurationYears)),
       "a duration of exactly the cap that is not one is worse than a missing one")

cli::cli_h2("The document table")

chk("One row per document", nrow(tab_dur) == nrow(tab_docs),
    paste0(nrow(tab_dur), " rows for ", nrow(tab_docs), " documents"))
# nrow() FIRST on all three below: all() of an empty vector is TRUE and any() is FALSE, so each of
# these would report a pass on a table that never got built.
chk("Every DurationSource is a registered level",
    nrow(tab_dur) > 0L && all(tab_dur$DurationSource %in% plot_levels("DurationSource")),
    paste(setdiff(unique(tab_dur$DurationSource), plot_levels("DurationSource")), collapse = ", "))
chk("Every TermKind emitted is a registered level",
    nrow(tab_term_raw) > 0L && all(tab_term_raw$LabelRaw %in% plot_levels("TermKind")),
    paste(setdiff(unique(tab_term_raw$LabelRaw), plot_levels("TermKind")), collapse = ", "))

n_cols <- dplyr::select(tab_dur, dplyr::starts_with("N"))
chk("Counts are integers, not doubles",
    ncol(n_cols) > 0L && all(purrr::map_lgl(n_cols, is.integer)),
    paste0(ncol(n_cols), " count column(s); rowSums() returns double and renders 723.000"))
chk("No duration is negative",
    nrow(tab_dur) > 0L && !any(dplyr::coalesce(tab_dur$DurationYears, 0) < 0),
    "a negative duration means the end precedes the start and must be dropped, not reported")


# 6. Verdict ----

tab_checks <- purrr::list_rbind(.checks)
n_fail     <- sum(!tab_checks$Ok)

cli::cli_h1("Verdict")
tab_checks |>
  dplyr::mutate(Result = dplyr::if_else(.data$Ok, "pass", "FAIL")) |>
  dplyr::select("Check", "Result", "Note") |>
  print(n = Inf, width = Inf)

if (n_fail == 0L) {
  cli::cli_alert_success(
    "{nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} passed. Both labels survive one pass \\
     over the text, no date is supplied that the text does not contain, and every rung of the \\
     cascade reports which rung it was."
  )
} else {
  cli::cli_alert_danger(
    "{n_fail} of {nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} failed. Read the Note \\
     column, then the artifacts below -- a failure here is a broken variable, not a broken test."
  )
}


# 7. What was written, for inspection ----

arrow::write_parquet(tab_docs,     fs::path(.dir_out, "01_fixture.parquet"))
arrow::write_parquet(tab_dates,    fs::path(.dir_out, "02_dates_raw.parquet"))
arrow::write_parquet(tab_term_raw, fs::path(.dir_out, "03_terms_raw.parquet"))
arrow::write_parquet(tab_terms,    fs::path(.dir_out, "04_terms_collapsed.parquet"))
arrow::write_parquet(tab_desc,     fs::path(.dir_out, "05_described.parquet"))
arrow::write_parquet(tab_dur,      fs::path(.dir_out, "06_duration.parquet"))
readr::write_csv(tab_checks,       fs::path(.dir_out, "07_checks.csv"))

cli::cli_h2("Artifacts")
fs::dir_info(.dir_out, type = "file") |>
  dplyr::transmute(
    File = as.character(fs::path_file(.data$path)),
    KB   = round(as.numeric(.data$size) / 1024, 1)
  ) |>
  dplyr::arrange(.data$File) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info("Open them with: {.code fs::file_show('{as.character(.dir_out)}')}")
cli::cli_alert_info(
  "06_duration.parquet is the released shape; 03_terms_raw.parquet is where the overlap invariant \\
   is visible, and 05_described.parquet carries the characters the end cue read."
)
