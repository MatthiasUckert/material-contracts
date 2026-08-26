# ======================================================================================================================
# Test-MatCon-REDACT.R -- the whole REDACT path, end to end, on documents whose answer is known
# ======================================================================================================================
#
# WHAT THIS TESTS
# The seam: fabricated text -> the CLI -> a parquet -> a DuckDB store -> ent_load_entity() ->
# red_load -> red_counts. matcon-extract's pytest suite proves redaction.py finds markers. Nothing
# proves a marker found in Python arrives in R as a per-thousand-word rate with the right numerator
# and the right denominator.
#
# THE SHORTEST CHAIN OF THE FOUR, AND THE ONE MOST ABOUT WHAT IS *NOT* COUNTED
# GPE resolves through nine stages and DATE through a four-rung cascade. REDACT has two functions.
# Almost everything interesting in it is an EXCLUSION, and an exclusion nobody can see is
# indistinguishable from a pattern that never matched. Four of them, each a documented judgement
# that a referee could poke at:
#
#   PAGE FILLER. "[Remainder of page intentionally left blank]" sits before the execution clause of
#   a great many contracts and conceals nothing whatever -- but it carries INTENTIONALLY and so
#   classified as an omission. A reading session put it at roughly one marker in six, which is the
#   margin by which a redaction count built without this overstates itself.
#
#   THE OPENING LEGEND. A filing explains its own convention by quoting the marker: "...information
#   is indicated by [***]". That occurrence DESCRIBES a marker rather than standing where content
#   was removed. Recognised by the verb to its left, within 80 characters, and not by anything about
#   the bracket.
#
#   A DRAWN RULE. A row of asterisks bordering a notary seal occupies its whole LINE. A redaction
#   never does -- it stands inside a sentence. The test is the line, not the length, because
#   "[*****]" mid-sentence is legitimate and a five-asterisk divider is not.
#
#   A NAMED PLACEHOLDER. "[___]" is a blank where something was removed; "[NAME]" is a field to
#   complete. The first is emitted and the second is not, and that line is drawn deliberately.
#
# Each gets a document below, and each is checked with chk_none() -- because for these, nothing
# found is the right answer and must be a different kind of check from a subset that came back
# empty by accident.
#
# THE DENOMINATOR IS THE REGISTER'S, NOT A RECOUNT
# red_words() used to read every document to count words that 02B already stored. At corpus scale
# that is an hour of reading to reproduce a column, and it is one of the two mistakes 04D's header
# names. NWords therefore arrives on the keys, and the checks below treat it as an input.
#
# WHY EVERY CHECK RUNS THROUGH A GUARD
# all(logical(0)) is TRUE, so a check reading an absent column passes. chk_on() asserts rows and
# columns BEFORE evaluating; chk_none() is the separate function for cases where empty is the
# answer.
#
# IT REPORTS, IT DOES NOT ABORT. Nothing outside tempdir() is written.
#
# Usage:
#   source(here::here("1_code", "_Tests", "Test-MatCon-REDACT.R"), encoding = "UTF-8")

here::i_am("1_code/_Commons/_Initialize.R")

options(cli.num_colors = 1, cli.width = 120)

source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),     encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_NER.R"),        encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Entity.R"),     encoding = "UTF-8")
source(here::here("1_code", "04B5-Rules-REDACT.R"),       encoding = "UTF-8")


# 1. Configuration ----

.dir_out <- fs::path(tempdir(), "Test-MatCon-REDACT")
fs::dir_create(.dir_out)

.lT <- list(
  Stage = fs::path(.dir_out, "staged_text.parquet"),
  Spans = fs::path(.dir_out, "spans"),
  Store = fs::path(.dir_out, "store"),
  # redaction.py's own constants, restated so the fixture can be read against them. They live in
  # that module and are hashed into redaction-v2's SPEC; a change there should fail a check here.
  Classes  = c("RedactExplicit", "RedactSymbol", "RedactBlank", "OmitExplicit", "OmitSymbol",
               "RedactBare"),
  Lookback = 80L    # LEGEND_LOOKBACK: how far left the legend verb may sit
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

cli::cli_h1("Test-MatCon-REDACT")
cli::cli_alert_info("Working under {.path {as.character(.dir_out)}}.")

.checks <- list()

#' Record one check without interrupting the run
#'
#' @param .name What was checked, in the words a reader would use.
#' @param .ok Logical. NA and length zero both count as failures.
#' @param .note What the failure would mean, or what the value actually was.
#' @return Invisibly TRUE where the check passed.
chk <- function(.name, .ok, .note = "") {
  if (FALSE) {
    .name <- "A bracketed marker is counted"
    .ok   <- TRUE
    .note <- ""
  }

  # LENGTH IS CHECKED, NOT ONLY TRUTH. all(logical(0)) is TRUE.
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
#' @param .name What was checked.
#' @param .tab Tibble the predicate reads.
#' @param .cols Character vector of columns the predicate needs.
#' @param .fun Function of one argument, the tibble, returning a length-one logical.
#' @param .note What the failure would mean, or the value actually seen.
#' @return Invisibly TRUE where the check passed.
chk_on <- function(.name, .tab, .cols, .fun, .note = "") {
  if (FALSE) {
    .name <- "A bracketed marker is counted"
    .tab  <- dplyr::filter(tab_marks, .data$DocID == "doc01")
    .cols <- c("Kind", "LabelRaw")
    .fun  <- \(.t) all(.t$Kind == "bracketed")
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
#' THE WORKHORSE OF THIS FILE. Four of the eight substantive rules in redaction.py are exclusions,
#' and for each of them nothing found is the right answer. chk_on() must keep treating zero rows as
#' failure, so the two cases need two functions.
#'
#' @param .name What was checked.
#' @param .tab Tibble expected to have no rows.
#' @param .note What a nonzero count would mean.
#' @return Invisibly TRUE where the check passed.
chk_none <- function(.name, .tab, .note = "") {
  if (FALSE) {
    .name <- "Page filler is not a redaction"
    .tab  <- dplyr::filter(tab_marks, .data$DocID == "doc07")
    .note <- ""
  }

  chk(.name, nrow(.tab) == 0L,
      if (nrow(.tab) == 0L) .note else paste0(nrow(.tab), " row(s) emitted; ", .note))
}


# 2. The fixture: twelve documents whose right answer is known ----
#
# Newlines matter here as they do in no other fixture: the line rule is recognised by occupying its
# own LINE, so a divider written inline would not be one.

tab_docs <- tibble::tribble(
  ~DocID,   ~TextRaw,
  # -- what IS a marker -----------------------------------------------------------------------
  # 01 the explicit form: the bracket names the act
  "doc01",  "The royalty payable shall be [CONFIDENTIAL TREATMENT REQUESTED] per unit sold.",
  # 02 the symbol form: a bracket holding only asterisks, mid-sentence
  "doc02",  "The purchase price shall be [***] payable at closing under this Agreement.",
  # 03 the blank form: an underscore run marks a REMOVAL, and moneyregex already treats it as one
  "doc03",  "The term of this Agreement shall be [___] years from the Effective Date.",
  # 04 an explicit omission
  "doc04",  "Schedule 4.2 [INTENTIONALLY OMITTED] shall be delivered under separate cover.",
  # 05 the bare form: three or more asterisks standing alone, inside a sentence
  "doc05",  "The rate is *** per annum on the outstanding principal balance hereunder.",
  # 06 two markers in one document, so the per-document count is not trivially one
  "doc06",  "The fee is [***] and the minimum is [***] under Section 4 of this Agreement.",

  # -- what is NOT, and each is a documented judgement -----------------------------------------
  # 07 PAGE FILLER. Carries INTENTIONALLY and conceals nothing. Roughly one marker in six.
  "doc07",  "The parties have executed this Agreement.\n[Remainder of page intentionally left blank]\n",
  # 08 THE OPENING LEGEND. The bracket DESCRIBES the convention rather than standing where content
  #    was removed. Recognised by the verb to its left.
  "doc08",  "Portions of this exhibit have been omitted and such information is indicated by [***]",
  # 09 A DRAWN RULE, occupying its whole line, as a divider above a signature block
  "doc09",  "IN WITNESS WHEREOF the parties have signed.\n**********\nName of Authorised Officer",
  # 10 A NAMED PLACEHOLDER: a field to complete, not a removal
  "doc10",  "This Agreement is made by and between [NAME] and the Company as of the date below.",
  # 11 an ordinary cross-reference in brackets, which was never a redaction
  "doc11",  "The obligations set out in [1.4] survive termination of this Agreement.",
  # 12 nothing at all: the document must still get a row, with a zero rather than an absence
  "doc12",  "The parties agree that all obligations hereunder shall be performed promptly."
)

arrow::write_parquet(tab_docs, .lT$Stage)

cli::cli_alert_info("{nrow(tab_docs)} {cli::qty(nrow(tab_docs))}fixture document{?s} staged.")

tab_lens <- tab_docs |>
  dplyr::transmute(.data$DocID, DocLen = stringi::stri_length(.data$TextRaw))

# THE DENOMINATOR IS AN INPUT, NOT A RECOUNT. 02B stores NWords for all 1.77 million register rows
# and red_words() used to read every document to reproduce it. Here it is computed once from the
# fixture and handed in exactly as 04D hands in the register's column.
tab_keys <- tab_docs |>
  dplyr::transmute(
    .data$DocID,
    Class     = "Lease",
    AmendType = "Original",
    NWords    = stringi::stri_count_words(.data$TextRaw)
  )


# 3. Extraction: the CLI, the store, the read ----

tab_describe <- ner_matcon_describe() |>
  dplyr::mutate(Family = "matcon") |>
  tidyr::unnest_longer(col = "Entities", values_to = "Entity") |>
  dplyr::mutate(Note = dplyr::if_else(.data$Ready, "", "data dependency absent")) |>
  dplyr::select("Family", "Model", "Entity", "SpecHash", "Ready", "Note")

hash_spec <- tab_describe$SpecHash[tab_describe$Entity == "REDACT"][[1L]]
cli::cli_alert_info("redaction spec hash: {(hash_spec)}.")

tab_staged <- ner_extract(
  .family     = "matcon",
  .model      = NULL,
  .entity     = "REDACT",
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
ner_ingest(.con = con_matcon, .staged = tab_staged, .entity = "REDACT")

tab_ledger <- DBI::dbGetQuery(
  con_matcon, "SELECT DocID, Entity, Status FROM runs ORDER BY DocID"
) |>
  tibble::as_tibble()
DBI::dbDisconnect(con_matcon, shutdown = TRUE)


# 4. The chain: load and count ----
#
# NO TEXT IS READ. red_load() works on the stored spans and red_counts() on the register's word
# count, which is why this is the one entity 04D can compute in full with cue windows off.

tab_marks <- red_load(
  .dir_store = .lT$Store,
  .lens      = tab_lens,
  .family    = "matcon",
  .quiet     = FALSE
)

tab_counts <- red_counts(
  .marks = tab_marks,
  .words = dplyr::select(tab_keys, "DocID", "NWords"),
  .keys  = tab_keys
)

cli::cli_alert_info(
  "Chain complete: {nrow(tab_marks)} marker{?s} over \\
   {dplyr::n_distinct(tab_marks$DocID)} document{?s}, {nrow(tab_counts)} count row{?s}."
)


# 5. The checks ----

#' Markers for one document
#'
#' @param .doc DocID.
#' @return Tibble, possibly empty.
marks_for <- function(.doc) {
  if (FALSE) .doc <- "doc01"
  dplyr::filter(tab_marks, .data$DocID == .doc)
}

#' The count row for one document
#'
#' @param .doc DocID.
#' @return One-row tibble.
count_for <- function(.doc) {
  if (FALSE) .doc <- "doc06"
  dplyr::filter(tab_counts, .data$DocID == .doc)
}

cli::cli_h2("The seam")

chk("The extractor wrote exactly one parquet", nrow(tab_staged) == 1L,
    paste0(nrow(tab_staged), " file(s) written"))
chk("Every fixture document reached the ledger",
    dplyr::n_distinct(tab_ledger$DocID) == nrow(tab_docs),
    paste0(dplyr::n_distinct(tab_ledger$DocID), " of ", nrow(tab_docs)))
chk("Markers were read back out of the store", nrow(tab_marks) > 0L,
    paste0(nrow(tab_marks), " marker(s)"))

cut_ <- stringi::stri_sub(
  tab_docs$TextRaw[match(tab_marks$DocID, tab_docs$DocID)],
  from = tab_marks$Start + 1L, to = tab_marks$Stop
)
chk("Every marker equals the text at its own offsets",
    nrow(tab_marks) > 0L && all(cut_ == tab_marks$Span),
    paste0(sum(cut_ != tab_marks$Span), " mismatch(es) over ", nrow(tab_marks), " marker(s)"))

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

cue_checks(tab_marks, "REDACT")

cli::cli_h2("What is a marker")

chk_on("An explicit confidentiality bracket is a marker", marks_for("doc01"),
       c("LabelRaw", "Kind"), \(.t) nrow(.t) == 1L,
       paste0(marks_for("doc01")$LabelRaw, ": ", marks_for("doc01")$Span))
chk_on("A bracketed asterisk run is a marker", marks_for("doc02"), c("LabelRaw", "Kind"),
       \(.t) nrow(.t) == 1L,
       paste0(marks_for("doc02")$LabelRaw, ": ", marks_for("doc02")$Span))
chk_on("An underscore blank is a marker, not a placeholder", marks_for("doc03"),
       c("LabelRaw", "Kind"), \(.t) nrow(.t) == 1L,
       paste0(marks_for("doc03")$LabelRaw,
              " -- moneyregex already treats $____ as a redaction; two extractors disagreeing ",
              "would be a gap"))
chk_on("An explicit omission is a marker", marks_for("doc04"), c("LabelRaw", "Kind"),
       \(.t) nrow(.t) == 1L,
       paste0(marks_for("doc04")$LabelRaw, ": ", marks_for("doc04")$Span))
chk_on("A bare asterisk run inside a sentence is a marker", marks_for("doc05"),
       c("LabelRaw", "Kind"), \(.t) nrow(.t) == 1L,
       paste0(marks_for("doc05")$LabelRaw, ": ", marks_for("doc05")$Span))

chk("Every LabelRaw emitted is one of the declared classes",
    nrow(tab_marks) > 0L && all(tab_marks$LabelRaw %in% .lT$Classes),
    paste(setdiff(unique(tab_marks$LabelRaw), .lT$Classes), collapse = ", "))

cli::cli_h2("What is NOT a marker, and why")

chk_none("Page filler is excluded", marks_for("doc07"),
         "carries INTENTIONALLY and conceals nothing; roughly one marker in six")
chk_none("The opening legend is excluded", marks_for("doc08"),
         paste0("the bracket DESCRIBES the convention; the verb sits within ", .lT$Lookback,
                " characters to its left"))
chk_none("A drawn rule on its own line is excluded", marks_for("doc09"),
         "the test is the LINE, not the length: a mid-sentence [*****] is legitimate")
chk_none("A named placeholder is excluded", marks_for("doc10"),
         "[___] marks a removal; [NAME] marks a field to complete")
chk_none("An ordinary cross-reference is excluded", marks_for("doc11"),
         "[1.4] was never a redaction and is not one now")
chk_none("A document with nothing to find yields nothing", marks_for("doc12"),
         "and must still appear in the count table -- checked below")

cli::cli_h2("Bare against bracketed")

chk_on("A bracketed marker is classed bracketed", marks_for("doc02"), "Kind",
       \(.t) all(.t$Kind == "bracketed"), paste(marks_for("doc02")$Kind, collapse = ", "))
chk_on("A bare marker is classed bare", marks_for("doc05"), "Kind",
       \(.t) all(.t$Kind == "bare"), paste(marks_for("doc05")$Kind, collapse = ", "))
chk("Both kinds occur in the fixture, so the split is testable",
    nrow(tab_marks) > 0L && dplyr::n_distinct(tab_marks$Kind) == 2L,
    paste(sort(unique(tab_marks$Kind)), collapse = ", "))

cli::cli_h2("The counts, and the denominator")

c06 <- count_for("doc06")
chk_on("Two markers in one document count as two", c06, c("NBracketed", "NRedact"),
       \(.t) all(.t$NBracketed == 2L) && all(.t$NRedact == 2L),
       paste0("NBracketed = ", c06$NBracketed, ", NRedact = ", c06$NRedact))

c05 <- count_for("doc05")
chk_on("A bare marker counts in NBare and NRedact but NOT in NBracketed", c05,
       c("NBare", "NBracketed", "NRedact"),
       \(.t) all(.t$NBare == 1L) && all(.t$NBracketed == 0L) && all(.t$NRedact == 1L),
       paste0("NBare = ", c05$NBare, ", NBracketed = ", c05$NBracketed))
chk_on("So RedactRatio excludes it and RedactRatioAll does not", c05,
       c("RedactRatio", "RedactRatioAll"),
       \(.t) all(.t$RedactRatio == 0) && all(.t$RedactRatioAll > 0),
       "the two ratios differ by exactly the bare markers, which is the point of having both")

c12 <- count_for("doc12")
chk_on("A document with no markers gets a row of zeros, not an absent row", c12,
       c("NRedact", "HasRedact", "HasBare"),
       \(.t) all(.t$NRedact == 0L) && !any(.t$HasRedact) && !any(.t$HasBare),
       "a contract with no redaction and a file that would not open are different facts")

# THE DENOMINATOR IS THE REGISTER'S. Recomputed here only to prove the ratio uses what it was
# handed, which is what red_words() removal was about.
c01 <- count_for("doc01")
w01_ <- tab_keys$NWords[match("doc01", tab_keys$DocID)]
chk_on("RedactRatio is markers per thousand words of the SUPPLIED count", c01,
       c("RedactRatio", "NWords", "NBracketed"),
       \(.t) abs(.t$RedactRatio - 1000 * .t$NBracketed / w01_) < 1e-9,
       paste0(c01$NBracketed, " marker(s) over ", w01_, " word(s) = ",
              round(c01$RedactRatio, 3), " per thousand"))

cli::cli_h2("The count table")

chk("One row per document", nrow(tab_counts) == nrow(tab_docs),
    paste0(nrow(tab_counts), " rows for ", nrow(tab_docs), " documents"))
chk("Every marker in the store is counted somewhere",
    sum(tab_counts$NRedact) == nrow(tab_marks),
    paste0(sum(tab_counts$NRedact), " counted against ", nrow(tab_marks), " in the store"))

n_cols <- dplyr::select(tab_counts, dplyr::starts_with("N"))
chk("Counts are integers, not doubles",
    ncol(n_cols) > 0L && all(purrr::map_lgl(dplyr::select(n_cols, -"NWords"), is.integer)),
    paste0(ncol(n_cols), " count column(s); rowSums() returns double and renders 723.000"))
chk("No ratio is negative or infinite",
    nrow(tab_counts) > 0L && all(is.finite(tab_counts$RedactRatioAll)) &&
      all(tab_counts$RedactRatioAll >= 0),
    "pmax(NWords, 1L) is what stops a zero-word document dividing by zero")


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
    "{nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} passed. Five forms of marker are \\
     counted, five things that look like markers are not, and every exclusion is visible rather \\
     than indistinguishable from a pattern that never matched."
  )
} else {
  cli::cli_alert_danger(
    "{n_fail} of {nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} failed. Read the Note \\
     column, then the artifacts below -- a failure here is a broken variable, not a broken test."
  )
}


# 7. What was written, for inspection ----

arrow::write_parquet(tab_docs,   fs::path(.dir_out, "01_fixture.parquet"))
arrow::write_parquet(tab_keys,   fs::path(.dir_out, "02_keys_with_nwords.parquet"))
arrow::write_parquet(tab_marks,  fs::path(.dir_out, "03_markers.parquet"))
arrow::write_parquet(tab_counts, fs::path(.dir_out, "04_counts.parquet"))
readr::write_csv(tab_checks,     fs::path(.dir_out, "05_checks.csv"))

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
  "04_counts.parquet is the released shape. 03_markers.parquet holds what survived; the five \\
   documents absent from it are the exclusions, and reading the fixture beside it is how those \\
   judgements are checked rather than trusted."
)
