# ======================================================================================================================
# Test-MatCon-MONEY.R -- the whole MONEY path, end to end, on documents whose answer is known
# ======================================================================================================================
#
# WHAT THIS TESTS
# The seam: fabricated text -> the CLI -> a parquet -> a DuckDB store -> ent_load_entity() ->
# mny_load -> mny_aggregate -> mny_doc_table. matcon-extract's pytest suite proves moneyregex.py
# finds amounts. Nothing proves an amount found in Python arrives in R as a figure in the right
# currency block, with par-value boilerplate removed and a redacted figure distinguished from an
# absent one.
#
# MATCON ONLY, AND THAT IS A CHOICE RATHER THAN A LIMITATION. MONEY is the one entity where 04B4
# keeps BOTH families -- "zero exact agreement, so neither replaces the other" -- but the LexNLP arm
# needs Docker, and a test that cannot run without a container is a test that stops being run. What
# is checked here is the matcon path; the disagreement between the two is 04B4's subject and is
# measured there.
#
# THREE THINGS MAKE MONEY DIFFERENT FROM DATE
#
#   A SECOND OVERLAP RESOLVER. moneyregex uses keep_leftmost(), not keep_longest(), and _io.py is
#   explicit that the two genuinely disagree: "dates compete as alternative readings of the same
#   text, so the longest reading wins outright; money expressions are read in document order, and a
#   figure that has already begun is not superseded by a later, longer expression that happens to
#   swallow it." The consequence that matters is the scale word: "$30 million" must come back as one
#   span worth thirty million, not as "$30" worth thirty. Six orders of magnitude, silently.
#
#   AN AMOUNT CAN BE DELIBERATELY ABSENT. A redacted figure -- $[***], $**, $TBD -- is a denomination
#   whose number was withheld. It carries a currency and no amount, and it must stay
#   distinguishable from a document that names no money at all. Parsed is what separates them.
#
#   THE PAR CUE IS READ BEFORE THE AMOUNT ONLY, AND THE ROXYGEN SAYS OTHERWISE. mny_spec()'s
#   documentation promises "characters either side of an amount"; the code cuts a window that ends
#   at the span. Documents 06 and 07 are the same clause with the par language on either side, and
#   the checks pin what the code does rather than what the comment claims. This is exactly the gap
#   the planned CueBefore / CueAfter columns close, and pinning it now is what will make that change
#   provable rather than hopeful.
#
# WHY EVERY CHECK RUNS THROUGH A GUARD
# all(logical(0)) is TRUE, so a check reading a column that does not exist passes. chk_on() asserts
# rows and columns BEFORE evaluating; chk_none() is the separate function for cases where empty is
# the answer. Four checks in the first Test-MatCon-GPE passed on absent columns before this existed.
#
# IT REPORTS, IT DOES NOT ABORT. Nothing outside tempdir() is written.
#
# Usage:
#   source(here::here("1_code", "_Tests", "Test-MatCon-MONEY.R"), encoding = "UTF-8")

here::i_am("1_code/_Commons/_Initialize.R")

options(cli.num_colors = 1, cli.width = 120)

source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),     encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_NER.R"),        encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Entity.R"),     encoding = "UTF-8")
source(here::here("1_code", "04B4-Rules-MONEY.R"),        encoding = "UTF-8")


# 1. Configuration ----

.dir_out <- fs::path(tempdir(), "Test-MatCon-MONEY")
fs::dir_create(.dir_out)

.lT <- list(
  Stage = fs::path(.dir_out, "staged_text.parquet"),
  Spans = fs::path(.dir_out, "spans"),
  Store = fs::path(.dir_out, "store")
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
fs::dir_create(c(.lT$Spans, .lT$Store))

# 04B4's released specification, argument for argument as 04D declares it with cues on.
.spec <- mny_spec(
  .filter  = "par",   # drop zeros AND par-value boilerplate
  .cue_win = 60L,     # characters read BEFORE an amount, for the par cue
  .label   = NULL
)

# The two other filters, so what each removes is visible rather than asserted.
.spec_none <- mny_spec(.filter = "none", .cue_win = 60L, .label = NULL)
.spec_zero <- mny_spec(.filter = "zero", .cue_win = 60L, .label = NULL)

cli::cli_h1("Test-MatCon-MONEY")
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
    .name <- "A scaled amount keeps its scale"
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
#' An empty subset or an absent column makes all() and any() return a value that reads as a pass, so
#' both are refused BEFORE the predicate runs and each gets its own message.
#'
#' @param .name What was checked.
#' @param .tab Tibble the predicate reads.
#' @param .cols Character vector of columns the predicate needs.
#' @param .fun Function of one argument, the tibble, returning a length-one logical.
#' @param .note What the failure would mean, or the value actually seen.
#' @return Invisibly TRUE where the check passed.
chk_on <- function(.name, .tab, .cols, .fun, .note = "") {
  if (FALSE) {
    .name <- "A scaled amount keeps its scale"
    .tab  <- dplyr::filter(tab_money, .data$DocID == "doc02")
    .cols <- c("Amount", "Currency")
    .fun  <- \(.t) all(.t$Amount == 30e6)
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
#' @param .name What was checked.
#' @param .tab Tibble expected to have no rows.
#' @param .note What a nonzero count would mean.
#' @return Invisibly TRUE where the check passed.
chk_none <- function(.name, .tab, .note = "") {
  if (FALSE) {
    .name <- "A document naming no money produces no span"
    .tab  <- dplyr::filter(tab_money, .data$DocID == "doc09")
    .note <- ""
  }

  chk(.name, nrow(.tab) == 0L,
      if (nrow(.tab) == 0L) .note else paste0(nrow(.tab), " row(s) emitted; ", .note))
}


# 2. The fixture: thirteen documents whose right answer is known ----

tab_docs <- tibble::tribble(
  ~DocID,   ~TextRaw,
  # -- reading an amount ----------------------------------------------------------------------
  # 01 a plain grouped figure: 5,000,000 in USD
  "doc01",  "The Purchaser shall pay $5,000,000 to the Seller on the closing date.",
  # 02 THE SCALE TRAP. The scale word must be inside the span or the figure is out by 1e6.
  "doc02",  "The facility provides for borrowings of up to $30 million in the aggregate.",
  # 03 a non-USD code: the Block column must separate it from the dollar figures
  "doc03",  "The Borrower shall repay EUR 5,500,000 no later than the maturity date.",
  # 04 spelled out: the words reading, not the figure reading
  "doc04",  "The Tenant shall pay FIVE THOUSAND Dollars per month in advance.",

  # -- an amount deliberately absent ----------------------------------------------------------
  # 05 a redacted figure: a denomination with the number withheld. Currency, no Amount.
  "doc05",  "The royalty rate shall be $[***] per unit sold in the Territory.",
  # 06 a bare-asterisk redaction, which is not always bracketed
  "doc06",  "The minimum purchase price is $** per share of common stock.",

  # -- the filters ----------------------------------------------------------------------------
  # 07 par language BEFORE the amount, inside the 60-character window: IsPar must be TRUE
  "doc07",  "The Company shall issue shares having a par value of $0.01 per share.",
  # 08 THE SAME CLAUSE, PAR LANGUAGE AFTER. The window ends at the span, so IsPar is FALSE --
  #    which is what the code does and not what mny_spec()'s roxygen promises.
  "doc08",  "The Company shall issue shares of $0.01 par value per share to the Purchaser.",
  # 09 an amount of exactly zero, which the zero filter removes
  "doc09",  "The outstanding balance under this facility is $0 as of the date hereof.",

  # -- aggregation ----------------------------------------------------------------------------
  # 10 the same figure three times: NSpans 3, NDistinct 1, RepeatRatio one third
  "doc10",  paste("The Seller shall receive $250,000 on closing, $250,000 on the first",
                  "anniversary and $250,000 on the second anniversary."),
  # 11 two currencies in one document: two Block rows, pivoted into one document row
  "doc11",  "The purchase price is $1,000,000 plus EUR 250,000 payable at completion.",
  # 12 no money of any kind: the document still gets a row, with HasMoney FALSE
  "doc12",  "The parties agree that all obligations hereunder shall be performed promptly.",
  # 13 the classifying words FOLLOW the number, which is the case for the planned CueAfter column
  "doc13",  "The Notes are issued in the amount of $40,000,000 aggregate principal amount."
)

arrow::write_parquet(tab_docs, .lT$Stage)

cli::cli_alert_info("{nrow(tab_docs)} {cli::qty(nrow(tab_docs))}fixture document{?s} staged.")

tab_lens <- tab_docs |>
  dplyr::transmute(.data$DocID, DocLen = stringi::stri_length(.data$TextRaw))

# mny_aggregate() selects Class and AmendType off the keys; nothing here reads their values.
tab_keys <- tab_docs |>
  dplyr::transmute(.data$DocID, Class = "Lease", AmendType = "Original")


# 3. Extraction: the CLI, the store, the read ----

tab_describe <- ner_matcon_describe() |>
  dplyr::mutate(Family = "matcon") |>
  tidyr::unnest_longer(col = "Entities", values_to = "Entity") |>
  dplyr::mutate(Note = dplyr::if_else(.data$Ready, "", "data dependency absent")) |>
  dplyr::select("Family", "Model", "Entity", "SpecHash", "Ready", "Note")

hash_spec <- tab_describe$SpecHash[tab_describe$Entity == "MONEY"][[1L]]
cli::cli_alert_info("moneyregex spec hash: {(hash_spec)}.")

tab_staged <- ner_extract(
  .family     = "matcon",
  .model      = NULL,
  .entity     = "MONEY",
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
ner_ingest(.con = con_matcon, .staged = tab_staged, .entity = "MONEY")

tab_ledger <- DBI::dbGetQuery(
  con_matcon, "SELECT DocID, Entity, Status FROM runs ORDER BY DocID"
) |>
  tibble::as_tibble()
DBI::dbDisconnect(con_matcon, shutdown = TRUE)


# 4. The chain: load with the cue window, aggregate, tabulate ----
#
# THE TEXT IS READ HERE, and it is the only reason this chain touches it. mny_load() cuts sixty
# characters before each amount to test for par-value language; everything else runs on the offsets
# and the parsed value the store already holds.

tab_money <- mny_load(
  .dir_store = .lT$Store,
  .lens      = tab_lens,
  .path_text = .lT$Stage,
  .families  = "matcon",
  .win       = .spec$CueWin,
  .quiet     = FALSE
)

tab_agg <- mny_aggregate(.money = tab_money, .keys = tab_keys, .spec = .spec)
tab_doc <- mny_doc_table(.agg = tab_agg, .keys = tab_keys)

# The same aggregation under the other two filters, so what each removes is a measured difference
# rather than a claim.
tab_agg_none <- mny_aggregate(.money = tab_money, .keys = tab_keys, .spec = .spec_none)
tab_agg_zero <- mny_aggregate(.money = tab_money, .keys = tab_keys, .spec = .spec_zero)

cli::cli_alert_info(
  "Chain complete: {nrow(tab_money)} span{?s}, {nrow(tab_agg)} aggregate row{?s}, \\
   {nrow(tab_doc)} document row{?s}."
)


# 5. The checks ----

#' Money spans for one document
#'
#' @param .doc DocID.
#' @return Tibble, possibly empty.
money_for <- function(.doc) {
  if (FALSE) .doc <- "doc01"
  dplyr::filter(tab_money, .data$DocID == .doc)
}

#' The document row for one document
#'
#' @param .doc DocID.
#' @return One-row tibble.
doc_for <- function(.doc) {
  if (FALSE) .doc <- "doc10"
  dplyr::filter(tab_doc, .data$DocID == .doc)
}

cli::cli_h2("The seam")

chk("The extractor wrote exactly one parquet", nrow(tab_staged) == 1L,
    paste0(nrow(tab_staged), " file(s) written"))
chk("Every fixture document reached the ledger",
    dplyr::n_distinct(tab_ledger$DocID) == nrow(tab_docs),
    paste0(dplyr::n_distinct(tab_ledger$DocID), " of ", nrow(tab_docs)))
chk("Spans were read back out of the store", nrow(tab_money) > 0L,
    paste0(nrow(tab_money), " span(s)"))

cut_ <- stringi::stri_sub(
  tab_docs$TextRaw[match(tab_money$DocID, tab_docs$DocID)],
  from = tab_money$Start + 1L, to = tab_money$Stop
)
chk("Every span equals the text at its own offsets",
    nrow(tab_money) > 0L && all(cut_ == tab_money$Span),
    paste0(sum(cut_ != tab_money$Span), " mismatch(es) over ", nrow(tab_money), " span(s)"))

cli::cli_h2("Reading an amount")

d01 <- money_for("doc01")
chk_on("A grouped figure parses to its value", d01, c("Amount", "Currency"),
       \(.t) any(.t$Amount == 5e6 & .t$Currency == "USD"),
       paste0(paste(d01$Span, collapse = ", "), " -> ", paste(d01$Amount, collapse = ", ")))

# THE SCALE TRAP, and it is worth six orders of magnitude. keep_leftmost() sorts by start then by
# DESCENDING length, so symbol_scaled beats symbol_amount at the same offset and the scale word
# stays inside the span. Had the shorter span won, this would be thirty.
d02 <- money_for("doc02")
chk_on("A scale word inside the span is applied", d02, c("Amount", "Span"),
       \(.t) any(.t$Amount == 30e6),
       paste0(paste(d02$Span, collapse = ", "), " -> ", paste(d02$Amount, collapse = ", ")))
chk_on("And the span carries the scale word, not just the figure", d02, "Span",
       \(.t) any(stringi::stri_detect_fixed(stringi::stri_trans_toupper(.t$Span), "MILLION")),
       "a span of \"$30\" alone would parse to thirty and read as perfectly ordinary")

d03 <- money_for("doc03")
chk_on("A non-USD code resolves to its own currency", d03, c("Amount", "Currency"),
       \(.t) any(.t$Currency == "EUR" & .t$Amount == 5.5e6),
       paste0(paste(d03$Currency, collapse = ", "), " ", paste(d03$Amount, collapse = ", ")))
chk_on("And lands in the non-USD block", d03, "Block", \(.t) any(.t$Block == "non-USD"),
       paste(unique(d03$Block), collapse = ", "))

d04 <- money_for("doc04")
chk_on("A spelled-out amount parses to a number", d04, c("Amount", "Currency"),
       \(.t) any(.t$Amount == 5000 & .t$Currency == "USD"),
       paste0(paste(d04$Span, collapse = ", "), " -> ", paste(d04$Amount, collapse = ", ")))

cli::cli_h2("An amount deliberately absent")

d05 <- money_for("doc05")
chk_on("A bracketed redaction produces a span", d05, c("Span", "Amount"), \(.t) nrow(.t) > 0L,
       paste(d05$Span, collapse = ", "))
chk_on("It carries no amount", d05, "Amount", \(.t) all(is.na(.t$Amount)),
       "the figure was withheld; supplying one would invent it")
chk_on("And is therefore not Parsed", d05, "Parsed", \(.t) !any(.t$Parsed),
       "Parsed is what separates a withheld figure from an absent document")

d06 <- money_for("doc06")
chk_on("A bare-asterisk redaction is caught too", d06, c("Span", "Amount"),
       \(.t) nrow(.t) > 0L && all(is.na(.t$Amount)),
       paste0(paste(d06$Span, collapse = ", "), " -- a redaction is not always bracketed"))

chk_none("A document naming no money produces no span", money_for("doc12"),
         "absent and withheld are different facts")

cli::cli_h2("The par cue, read BEFORE the amount only")

d07 <- money_for("doc07")
chk_on("Par language before the amount is caught", d07, c("IsPar", "Before"),
       \(.t) any(.t$IsPar),
       paste0("Before = ", paste(stringi::stri_sub(d07$Before, from = -30L), collapse = " | ")))

# THE ROXYGEN AND THE CODE DISAGREE, and this pins the code. mny_spec() documents "characters either
# side of an amount"; mny_load() cuts a window ENDING at the span. So the identical clause with the
# par language after the figure is not recognised. Recorded as behaviour, not asserted as correct:
# it is the gap the planned CueBefore / CueAfter columns are meant to close, and a change that fixed
# it should flip this check.
d08 <- money_for("doc08")
chk_on("Par language AFTER the amount is missed", d08, c("IsPar", "Before"),
       \(.t) !any(.t$IsPar),
       "mny_spec()'s roxygen promises either side; the code reads before only")

cli::cli_h2("The three filters, and what each removes")

n_par_  <- sum(tab_money$IsPar,  na.rm = TRUE)
n_zero_ <- sum(tab_money$IsZero, na.rm = TRUE)

chk("The fixture contains a par-flagged amount to filter", n_par_ > 0L,
    paste0(n_par_, " span(s) flagged IsPar"))
chk("The fixture contains a zero amount to filter", n_zero_ > 0L,
    paste0(n_zero_, " span(s) flagged IsZero"))

n_none_ <- sum(tab_agg_none$NSpans)
n_zerof_ <- sum(tab_agg_zero$NSpans)
n_parf_  <- sum(tab_agg$NSpans)

chk("The zero filter removes strictly fewer spans than the par filter",
    n_zerof_ > n_parf_ && n_none_ > n_zerof_,
    paste0("none ", n_none_, " -> zero ", n_zerof_, " -> par ", n_parf_))
chk_none("A zero amount does not survive the par filter",
         dplyr::filter(tab_agg, .data$DocID == "doc09"),
         "an outstanding balance of nothing is not a contract value")

cli::cli_h2("Aggregation")

a10 <- dplyr::filter(tab_agg, .data$DocID == "doc10")
chk_on("Three identical figures count as three spans", a10, "NSpans", \(.t) all(.t$NSpans == 3L),
       paste0("NSpans = ", paste(a10$NSpans, collapse = ", ")))
chk_on("But as one distinct amount", a10, "NDistinct", \(.t) all(.t$NDistinct == 1L),
       paste0("NDistinct = ", paste(a10$NDistinct, collapse = ", ")))
chk_on("So the repetition ratio is one third", a10, "RepeatRatio",
       \(.t) all(abs(.t$RepeatRatio - 1 / 3) < 1e-9),
       "NDistinct over NSpans is the caveat a reader needs beside MoneySum")
chk_on("The sum triples the figure and the max does not", a10, c("MoneySum", "MoneyMax"),
       \(.t) all(.t$MoneySum == 750000) && all(.t$MoneyMax == 250000),
       paste0("Sum = ", paste(a10$MoneySum, collapse = ", "), ", Max = ",
              paste(a10$MoneyMax, collapse = ", ")))

a11 <- dplyr::filter(tab_agg, .data$DocID == "doc11")
chk_on("Two currencies give two aggregate rows", a11, "Block", \(.t) nrow(.t) == 2L,
       paste(sort(a11$Block), collapse = ", "))

cli::cli_h2("The document table")

chk("One row per document per engine", nrow(tab_doc) == nrow(tab_docs),
    paste0(nrow(tab_doc), " rows for ", nrow(tab_docs), " documents"))

d11 <- doc_for("doc11")
chk_on("The two blocks pivot into one document row", d11,
       c("NSpansUSD", "NSpansnon-USD"),
       \(.t) all(.t$NSpansUSD == 1L) && all(.t$`NSpansnon-USD` == 1L),
       "the non-USD columns sit beside the USD ones rather than in a table nobody joins")

d12 <- doc_for("doc12")
chk_on("A document naming no money still gets a row, with HasMoney FALSE", d12,
       c("HasMoney", "NSpansUSD"),
       \(.t) !any(.t$HasMoney) && all(.t$NSpansUSD == 0L),
       "a zero is a zero rather than an absence")

n_cols <- dplyr::select(tab_doc, dplyr::starts_with("NSpans"))
chk("Span counts are integers, not doubles",
    ncol(n_cols) > 0L && all(purrr::map_lgl(n_cols, is.integer)),
    paste0(ncol(n_cols), " count column(s); rowSums() returns double and renders 723.000"))
chk("Every Block is USD or non-USD, never NA",
    nrow(tab_agg) > 0L && all(tab_agg$Block %in% c("USD", "non-USD")),
    paste(setdiff(unique(tab_agg$Block), c("USD", "non-USD")), collapse = ", "))


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
    "{nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} passed. A scale word survives into \\
     the span, a withheld figure stays distinguishable from an absent one, and the par cue does \\
     what the code says rather than what the comment says."
  )
} else {
  cli::cli_alert_danger(
    "{n_fail} of {nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} failed. Read the Note \\
     column, then the artifacts below -- a failure here is a broken variable, not a broken test."
  )
}


# 7. What was written, for inspection ----

arrow::write_parquet(tab_docs,      fs::path(.dir_out, "01_fixture.parquet"))
arrow::write_parquet(tab_money,     fs::path(.dir_out, "02_spans_with_cue.parquet"))
arrow::write_parquet(tab_agg_none,  fs::path(.dir_out, "03_agg_filter_none.parquet"))
arrow::write_parquet(tab_agg_zero,  fs::path(.dir_out, "04_agg_filter_zero.parquet"))
arrow::write_parquet(tab_agg,       fs::path(.dir_out, "05_agg_filter_par.parquet"))
arrow::write_parquet(tab_doc,       fs::path(.dir_out, "06_doc_table.parquet"))
readr::write_csv(tab_checks,        fs::path(.dir_out, "07_checks.csv"))

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
  "06_doc_table.parquet is the released shape; 02_spans_with_cue.parquet carries the Before column \\
   the par filter reads, and files 03 to 05 are the same aggregation under each filter in turn."
)
