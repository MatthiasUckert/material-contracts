# ======================================================================================================================
# Test-MatCon-GPE.R -- the whole GPE path, end to end, on documents whose answer is known
# ======================================================================================================================
#
# WHAT THIS TESTS, AND WHAT IT DELIBERATELY DOES NOT
# matcon-extract carries 204 pytest tests and they cover gazetteer.py in isolation. What nothing
# covers is the SEAM: fabricated text -> the CLI -> a parquet -> a DuckDB store -> ent_load_entity()
# -> geo_resolve -> geo_city_state -> geo_country -> geo_context -> geo_law -> geo_attach ->
# geo_org_table -> geo_doc_table. That chain crosses two languages, a command line, a parquet and a
# database, and it is what 04B2 and 04D depend on.
#
# A test of gazetteer.py answers "does the extractor work". This answers "does the variable arrive".
#
# KNOWN ANSWERS, NOT REGRESSION
# Fourteen fabricated documents, each written so that the right answer follows from the gazetteer's
# stated rules rather than from the last run. "SHAKOPEE resolves to MINNESOTA and SCOTT county" can
# be checked by eye; "it produced 4,502 rows like last time" cannot, and a regression test passes
# happily on a result that was wrong from the first run onwards.
#
# THE FIXTURE RESPECTS THE EXTRACTOR'S THREE TIERS, AND TESTS THEM
# gazetteer.py emits a place name on one of three terms, and the first version of this file ignored
# two of them -- so three documents produced no span at all and the test read that as a failure when
# it was the gate working:
#
#   ANCHOR       US State, or Country outside AMBIGUOUS_COUNTRIES. Emitted unconditionally.
#   DISTINCTIVE  IsWord == 0. Emitted where an anchor occurs within STATE_WINDOW = 200 characters.
#   WORD-LIKE    IsWord == 1. Emitted where an anchor follows within WORD_WINDOW = 40, with only
#                separators between.
#
# A bare city name with no state beside it is REFUSED, and refusing to guess is the point of the
# rule. Documents 02 and 13 now test that refusal rather than trip over it.
#
# WHY EVERY CHECK RUNS THROUGH A GUARD
# The first version of this file asked tab_geo for NParentOut, Ambig, StateOut and Iso3Out --
# geo_resolve()'s INTERNAL names, renamed on the way out at its final select(). A missing column is
# NULL, NULL == 1L is logical(0), and all(logical(0)) is TRUE. Four checks therefore PASSED on
# columns that do not exist, and one of them was "no resolved row carries the -99 sentinel" -- the
# check guarding the exact defect the gazetteer rebuild was written to repair.
#
# So no check reads a tibble directly. chk_on() asserts the rows and the columns are there BEFORE
# evaluating anything, and chk_none() is the separate function for the cases where zero rows is the
# expected answer. Vacuous truth is unreachable rather than merely unlikely.
#
# WHY THE ORGANISATIONS ARE FABRICATED
# geo_attach() needs 04B1's roles, which come from LexNLP and therefore from Docker. Supplying them
# directly keeps this a GPE test with no container dependency, and they ARE an input to the chain
# rather than part of what it computes.
#
# IT REPORTS, IT DOES NOT ABORT. Every check runs and the verdict is a table at the end. One failure
# must not hide the next nine.
#
# NOTHING OUTSIDE tempdir() IS WRITTEN. The repository is read for one file, the gazetteer lookup.
# Every artifact lands under a temporary directory whose path is printed at the end, so the output
# can be opened and read rather than only asserted over.
#
# Usage:
#   source(here::here("1_code", "_Tests", "Test-MatCon-GPE.R"), encoding = "UTF-8")

here::i_am("1_code/_Commons/_Initialize.R")

options(cli.num_colors = 1, cli.width = 120)

source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),     encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_NER.R"),        encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Entity.R"),     encoding = "UTF-8")
source(here::here("1_code", "04B2-Rules-GPE.R"),          encoding = "UTF-8")


# 1. Configuration ----

.dir_out <- fs::path(tempdir(), "Test-MatCon-GPE")
fs::dir_create(.dir_out)

.lT <- list(
  Lookup  = here::here("contracts-extract", "src", "matcon_extract", "data", "geo_lookup.parquet"),
  Stage   = fs::path(.dir_out, "staged_text.parquet"),   # the fixture, as the CLI expects it
  Spans   = fs::path(.dir_out, "spans"),                 # what the extractor writes
  Store   = fs::path(.dir_out, "store"),                 # one DuckDB per family, as in 04A and 04C
  Reach   = 200,                                         # 04B2's released attachment reach
  LawWin  = 120L,                                        # 04B2's released governing-law cue window
  # THE EXTRACTOR'S OWN CONSTANTS, restated so the fixture can be read against them. They live in
  # gazetteer.py and are hashed into gazetteer-v2's SPEC; a change there should fail a check here.
  StateWin = 200L,
  WordWin  = 40L
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

cli::cli_h1("Test-MatCon-GPE")
cli::cli_alert_info("Working under {.path {as.character(.dir_out)}}.")

# The verdict accumulator. A check appends; nothing stops the run.
.checks <- list()

#' Record one check without interrupting the run
#'
#' The project's probes have twice buried a diagnosable failure behind nine later ones by aborting
#' or by storing the message in a column printed at the end. This prints where it happens AND keeps
#' the row, so the console is a live account and the verdict is still a table.
#'
#' Called by chk_on() and chk_none() rather than directly, because a bare call can read a column
#' that is not there and pass on logical(0). Use it directly only where the value being tested is
#' already a scalar computed outside a tibble.
#'
#' @param .name What was checked, in the words a reader would use.
#' @param .ok Logical. NA and length zero both count as failures.
#' @param .note What the failure would mean, or what the value actually was.
#' @return Invisibly TRUE where the check passed.
chk <- function(.name, .ok, .note = "") {
  if (FALSE) {
    .name <- "SHAKOPEE resolves unambiguously"
    .ok   <- TRUE
    .note <- ""
  }

  # LENGTH IS CHECKED, NOT ONLY TRUTH. all(logical(0)) is TRUE, which is how four checks in the
  # first version of this file passed on columns that did not exist.
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
#' THE GUARD IS THE POINT. An empty subset or an absent column makes all() and any() return a value
#' that reads as a pass, so both are refused BEFORE the predicate runs and each gets its own message
#' -- "no rows" and "column absent" have different causes and different fixes.
#'
#' .note is lazily evaluated, as every R argument is, so a note referring to a column that turns out
#' to be missing is never forced.
#'
#' @param .name What was checked.
#' @param .tab Tibble the predicate reads.
#' @param .cols Character vector of columns the predicate needs.
#' @param .fun Function of one argument, the tibble, returning a length-one logical.
#' @param .note What the failure would mean, or the value actually seen.
#' @return Invisibly TRUE where the check passed.
chk_on <- function(.name, .tab, .cols, .fun, .note = "") {
  if (FALSE) {
    .name <- "SHAKOPEE is unambiguous"
    .tab  <- dplyr::filter(tab_geo, .data$DocID == "doc01")
    .cols <- c("NParent", "Ambiguous")
    .fun  <- \(.t) all(.t$NParent == 1L)
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
#' and must keep doing so. The gate cases -- a word-like place with no anchor -- are the only ones
#' where nothing found is the right answer, and they should visibly be a different kind of check.
#'
#' @param .name What was checked.
#' @param .tab Tibble expected to have no rows.
#' @param .note What a nonzero count would mean.
#' @return Invisibly TRUE where the check passed.
chk_none <- function(.name, .tab, .note = "") {
  if (FALSE) {
    .name <- "A word-like city with no anchor is refused"
    .tab  <- dplyr::filter(tab_raw, .data$DocID == "doc02")
    .note <- ""
  }

  chk(.name, nrow(.tab) == 0L,
      if (nrow(.tab) == 0L) .note else paste0(nrow(.tab), " row(s) emitted; ", .note))
}


# 2. The fixture: fourteen documents whose right answer is known ----
#
# Each targets one failure. The comment beside it is the expectation and the check in section 5 is
# that expectation written as code, so the two can be compared without running anything.
#
# DOCUMENT 14 IS THE OFFSET CONTRACT. Python slices code points and base R's substr() indexes bytes,
# so a document carrying a multi-byte character BEFORE a span is the one case where the two
# disagree. The character is written as an escape rather than typed, because this file is pure
# ASCII.

.acc <- "\u00e9"                                                 # e-acute: two bytes, one code point
.gap <- strrep("The parties acknowledge the foregoing. ", 12L)   # about 468 characters

tab_docs <- tibble::tribble(
  ~DocID,   ~TextRaw,
  # 01 city with an adjacent state anchor: emitted, unambiguous, state MINNESOTA, county SCOTT
  "doc01",  paste("This Lease is made by Alpha Holdings Inc., with offices at 100 Main Street,",
                  "Shakopee, Minnesota."),
  # 02 city with NO anchor anywhere: REFUSED by the gate, which is the rule and not a gap
  "doc02",  "The Premises are located in Springfield.",
  # 03 ambiguous city WITH the anchor: geo_city_state()'s nearest-named arm resolves it to ILLINOIS
  "doc03",  "The Premises are located in Springfield, Illinois, and are leased as of the date here.",
  # 04 one name at three levels: CLASS_RANK puts US State first
  "doc04",  "Beta Corp maintains its principal office in Washington.",
  # 05 anchor class, no corroboration needed: a bare state is emitted on its own
  "doc05",  "Gamma LLC is organized under the laws of Delaware.",
  # 06 country: Iso3 DEU, and no state or county
  "doc06",  "Delta GmbH is organized under the laws of Germany.",
  # 07 the repaired sentinel: NORWAY carried -99 in the source and must read NOR
  "doc07",  "Epsilon AS has its registered office in Norway.",
  # 08 governing law, named
  "doc08",  paste("This Agreement shall be governed by and construed in accordance with the laws",
                  "of the State of Delaware."),
  # 09 governing law, self-referential: specifies the law and names no place
  "doc09",  paste("This Agreement shall be governed by the laws of the State in which the Premises",
                  "are located."),
  # 10 no geography at all: processed and matched nothing, which is not the same as unprocessed
  "doc10",  "The parties agree that all obligations hereunder shall be performed promptly.",
  # 11 place within reach of an organisation, anchored so it is emitted
  "doc11",  "Zeta Industries Inc., Minneapolis, Minnesota, hereby agrees as follows.",
  # 12 place anchored and emitted, but about 468 characters from the organisation: not attached
  "doc12",  paste0("Eta Systems Inc. ", .gap, "Minneapolis, Minnesota."),
  # 13 city and anchor both present but the anchor is far beyond STATE_WINDOW: REFUSED
  "doc13",  paste0("An office in Minneapolis. ", .gap, "This Agreement concerns Minnesota."),
  # 14 two multi-byte characters before the span, so byte and code-point offsets diverge
  "doc14",  paste0("Soci", .acc, "t", .acc, " Theta S.A. has an office in Minneapolis, Minnesota.")
)

arrow::write_parquet(tab_docs, .lT$Stage)

n_multi <- sum(stringi::stri_numbytes(tab_docs$TextRaw) != stringi::stri_length(tab_docs$TextRaw))
cli::cli_alert_info(
  "{nrow(tab_docs)} {cli::qty(nrow(tab_docs))}fixture document{?s} staged; \\
   {n_multi} {cli::qty(n_multi)}carr{?ies/y} a multi-byte character."
)

# The lengths every loader takes as its document filter. In CODE POINTS, which is what the offsets
# index, and from the fixture rather than from a register.
tab_lens <- tab_docs |>
  dplyr::transmute(.data$DocID, DocLen = stringi::stri_length(.data$TextRaw))

# The anchor keys the document-level tables join on. geo_org_table() relocates Class and
# geo_doc_table() carries AmendType; nothing here reads their values.
tab_keys <- tab_docs |>
  dplyr::transmute(.data$DocID, Class = "Lease", AmendType = "Original")

#' One fabricated organisation, with its offsets computed from the fixture
#'
#' OFFSETS ARE LOCATED, NOT TYPED. A typed offset survives an edit to the document it indexes and
#' silently starts pointing into the middle of a word, which is the failure this whole file exists
#' to catch one layer down.
#'
#' @param .doc DocID the organisation appears in.
#' @param .name The organisation name, exactly as it appears in the text.
#' @param .role One of the roles 04B1 assigns: filer or counterparty.
#' @return One-row tibble: DocID, Key, Name, Role, Start, Stop.
org_at <- function(.doc, .name, .role) {
  if (FALSE) {
    .doc  <- "doc11"
    .name <- "Zeta Industries Inc."
    .role <- "filer"
  }

  txt_   <- tab_docs$TextRaw[match(.doc, tab_docs$DocID)]
  start_ <- stringi::stri_locate_first_fixed(txt_, .name)[, "start"]
  if (is.na(start_)) cli::cli_abort("{(.name)} does not occur in {(.doc)}.")

  tibble::tibble(
    DocID = .doc,
    Key   = ent_norm_key(.x = .name, .min = 1L),
    Name  = .name,
    Role  = .role,
    Start = as.integer(start_ - 1L),                       # 0-based, as the extractors emit
    Stop  = as.integer(start_ - 1L + stringi::stri_length(.name))
  )
}

tab_org <- dplyr::bind_rows(
  org_at("doc01", "Alpha Holdings Inc.",  "filer"),
  org_at("doc04", "Beta Corp",            "filer"),
  org_at("doc05", "Gamma LLC",            "filer"),
  org_at("doc11", "Zeta Industries Inc.", "filer"),
  org_at("doc12", "Eta Systems Inc.",     "filer")
)


# 3. Extraction: the CLI, the store, the read ----
#
# THE SAME ENTRY POINT THE PIPELINE USES. ner_extract() is what 04A and 04C call, so this exercises
# the invocation the corpus was extracted with rather than a second one written for a test.

# A matcon-only description, built exactly as ner_describe()'s matcon branch builds it. The full
# function also probes Docker and spaCy, and neither is needed to extract GPE.
tab_describe <- ner_matcon_describe() |>
  dplyr::mutate(Family = "matcon") |>
  tidyr::unnest_longer(col = "Entities", values_to = "Entity") |>
  dplyr::mutate(Note = dplyr::if_else(.data$Ready, "", "data dependency absent")) |>
  dplyr::select("Family", "Model", "Entity", "SpecHash", "Ready", "Note")

hash_spec <- tab_describe$SpecHash[tab_describe$Entity == "GPE"][[1L]]
cli::cli_alert_info("gazetteer spec hash: {(hash_spec)}.")

tab_staged <- ner_extract(
  .family     = "matcon",          # GPE has one producer
  .model      = NULL,              # matcon versions itself
  .entity     = "GPE",             # one label, so one parquet
  .path_in    = .lT$Stage,         # the fixture
  .out_dir    = .lT$Spans,         # temporary
  .describe   = tab_describe,      # supplies the model tag and the spec hash
  .id_col     = "DocID",
  .text_col   = "TextRaw",
  .max_chars  = 0L,                # no truncation; the fixture is short
  .workers    = 1L,                # in-process, so a failure is debuggable
  .batch_size = 8L,
  .timeout    = 60L,
  .quiet      = TRUE
)

# The store, built the way 04A and 04C build theirs.
con_matcon <- ner_db_connect(
  .db_path   = ner_db_path(.dir = .lT$Store, .family = "matcon"),
  .read_only = FALSE
)
ner_db_init(.con = con_matcon)
ner_ingest(.con = con_matcon, .staged = tab_staged, .entity = "GPE")

tab_ledger <- DBI::dbGetQuery(
  con_matcon, "SELECT DocID, Entity, Status FROM runs ORDER BY DocID"
) |>
  tibble::as_tibble()
DBI::dbDisconnect(con_matcon, shutdown = TRUE)

tab_raw <- ent_load_entity(
  .dir_store = .lT$Store,
  .family    = "matcon",
  .entity    = "GPE",
  .lens      = tab_lens,
  .extras    = ent_extras("matcon", "GPE"),   # declared per family, never typed here
  .model     = NULL,
  .quiet     = FALSE
)


# 4. The chain: resolve, level, country, cue, law, attach, tables ----
#
# Called in 04D's order and with 04D's arguments, so what passes here is what runs on the corpus.

geo_once <- list(
  Lookup = geo_lookup(.path_lookup = .lT$Lookup),
  Cand   = geo_candidates(.path_lookup = .lT$Lookup)
)

tab_resolved <- tab_raw |>
  geo_resolve(.lookup = geo_once$Lookup, .family = "matcon") |>
  dplyr::mutate(Combo = "matcon", .before = 1L)

tab_geo <- tab_resolved |>
  geo_city_state(.cand = geo_once$Cand) |>
  geo_country()

tab_ctx <- geo_context(.geo = tab_geo, .path_text = .lT$Stage, .win = .lT$LawWin)

tab_law <- geo_law(.geo = tab_ctx, .lens = tab_lens, .path_text = .lT$Stage)

tab_roles <- geo_attach(
  .geo  = tab_geo,
  .org  = tab_org,
  .spec = geo_spec(.reach = .lT$Reach, .reach_city = NULL, .party_only = FALSE)
)

tab_orgtab <- geo_org_table(.roles = tab_roles, .org = tab_org, .keys = tab_keys)
tab_doc    <- geo_doc_table(.orgtab = tab_orgtab, .roles = tab_roles, .law = tab_law,
                            .keys = tab_keys)

cli::cli_alert_info(
  "Chain complete: {nrow(tab_raw)} span{?s}, {nrow(tab_geo)} resolved, {nrow(tab_roles)} role{?s}, \\
   {nrow(tab_doc)} document row{?s}."
)


# 5. The checks ----

#' Resolved rows for one document, optionally one place
#'
#' @param .doc DocID.
#' @param .key GeoKey, uppercased, or NULL for every place in the document.
#' @return Tibble, possibly empty.
geo_for <- function(.doc, .key = NULL) {
  if (FALSE) {
    .doc <- "doc01"
    .key <- "SHAKOPEE"
  }

  out_ <- dplyr::filter(tab_geo, .data$DocID == .doc)
  if (is.null(.key)) out_ else dplyr::filter(out_, .data$GeoKey == .key)
}

cli::cli_h2("The seam")

chk("The extractor wrote exactly one parquet", nrow(tab_staged) == 1L,
    paste0(nrow(tab_staged), " file(s) written"))
chk("Every fixture document reached the ledger",
    dplyr::n_distinct(tab_ledger$DocID) == nrow(tab_docs),
    paste0(dplyr::n_distinct(tab_ledger$DocID), " of ", nrow(tab_docs)))
chk("Spans were read back out of the store", nrow(tab_raw) > 0L,
    paste0(nrow(tab_raw), " span(s)"))
chk("geo_resolve() emitted the columns the chain reads",
    all(c("GeoKey", "GeoLevel", "State", "County", "Iso2", "Iso3", "NParent", "Ambiguous")
        %in% names(tab_geo)),
    paste(setdiff(c("GeoKey", "GeoLevel", "State", "County", "Iso2", "Iso3", "NParent",
                    "Ambiguous"), names(tab_geo)), collapse = ", "))

cli::cli_h2("The offset contract")

# text[Start:Stop] == Span, over CODE POINTS. stri_sub is 1-based and inclusive; the offsets are
# 0-based and half-open, hence the +1 on the start alone.
cut_ <- stringi::stri_sub(
  tab_docs$TextRaw[match(tab_raw$DocID, tab_docs$DocID)],
  from = tab_raw$Start + 1L,
  to   = tab_raw$Stop
)
# nrow() FIRST, because all(logical(0)) is TRUE: on an empty span table this would otherwise be a
# pass, and it is the single most important check in the file.
chk("Every span equals the text at its own offsets",
    nrow(tab_raw) > 0L && all(cut_ == tab_raw$Span),
    paste0(sum(cut_ != tab_raw$Span), " mismatch(es) over ", nrow(tab_raw), " span(s)"))

d14 <- dplyr::filter(tab_raw, .data$DocID == "doc14")
chk_on("The multi-byte document produced a span", d14, c("Span", "Start", "Stop"),
       \(.t) nrow(.t) > 0L,
       "doc14 carries two e-acutes before the place name")
chk_on("The multi-byte span survives the offset cut", d14, c("Span", "Start", "Stop"),
       \(.t) all(stringi::stri_sub(tab_docs$TextRaw[match(.t$DocID, tab_docs$DocID)],
                                   from = .t$Start + 1L, to = .t$Stop) == .t$Span),
       "byte indexing fails here and code-point indexing does not")

cli::cli_h2("The three tiers of the gate")

chk_on("An anchor class is emitted with no corroboration", geo_for("doc05", "DELAWARE"),
       "GeoLevel", \(.t) any(.t$GeoLevel == "State"),
       "a bare US State needs nothing beside it")
chk_none("A word-like city with no anchor is refused", geo_for("doc02", "SPRINGFIELD"),
         "refusing to guess is the rule, not a coverage gap")
chk_none("A city whose anchor sits beyond STATE_WINDOW is refused",
         geo_for("doc13", "MINNEAPOLIS"),
         paste0("the anchor is about 468 characters away; the window is ", .lT$StateWin))
chk_on("The far anchor itself is still emitted", geo_for("doc13", "MINNESOTA"), "GeoLevel",
       \(.t) any(.t$GeoLevel == "State"),
       "the gate refuses the city, not the state that failed to license it")

cli::cli_h2("Resolution")

d01 <- geo_for("doc01", "SHAKOPEE")
chk_on("SHAKOPEE resolves at all", d01, "GeoLevel", \(.t) nrow(.t) > 0L)
chk_on("SHAKOPEE is unambiguous", d01, c("NParent", "Ambiguous"),
       \(.t) all(.t$NParent == 1L) && !any(.t$Ambiguous),
       paste0("NParent = ", paste(unique(d01$NParent), collapse = ", ")))
chk_on("SHAKOPEE carries its state", d01, "State", \(.t) all(.t$State == "MINNESOTA"),
       paste(unique(d01$State), collapse = ", "))
chk_on("SHAKOPEE carries its county", d01, "County",
       \(.t) all(toupper(.t$County) == "SCOTT"), paste(unique(d01$County), collapse = ", "))

d03 <- geo_for("doc03", "SPRINGFIELD")
chk_on("SPRINGFIELD is emitted when a state anchors it", d03, "GeoLevel", \(.t) nrow(.t) > 0L)
chk_on("SPRINGFIELD is flagged ambiguous by the lookup", d03, c("NParent", "Ambiguous"),
       \(.t) all(.t$Ambiguous) && all(.t$NParent > 1L),
       paste0("NParent = ", paste(unique(d03$NParent), collapse = ", ")))
chk_on("The nearest-named arm resolves it to the state the contract names", d03,
       c("State", "GeoHow"), \(.t) all(.t$State == "ILLINOIS"),
       paste0(paste(unique(d03$State), collapse = ", "), " via ",
              paste(unique(d03$GeoHow), collapse = ", ")))
chk_on("And records WHICH arm resolved it", d03, "GeoHow",
       \(.t) all(.t$GeoHow == "nearest named"), paste(unique(d03$GeoHow), collapse = ", "))

d06 <- geo_for("doc06", "GERMANY")
chk_on("GERMANY resolves as a country", d06, c("GeoLevel", "Iso3"),
       \(.t) any(.t$GeoLevel == "Country") && any(.t$Iso3 == "DEU"),
       paste(unique(d06$Iso3), collapse = ", "))

d07 <- geo_for("doc07", "NORWAY")
chk_on("NORWAY carries the repaired code, not the sentinel", d07, "Iso3",
       \(.t) any(.t$Iso3 == "NOR") && !any(.t$Iso3 %in% "-99"),
       paste(unique(d07$Iso3), collapse = ", "))
chk_on("No resolved row anywhere carries the -99 sentinel", tab_geo, c("Iso2", "Iso3"),
       \(.t) !any(.t$Iso3 %in% "-99") && !any(.t$Iso2 %in% "-99"),
       "the cross-engine check found this once; it must not travel again")

cli::cli_h2("Governing law")

l08 <- dplyr::filter(tab_law, .data$DocID == "doc08")
chk_on("A named governing law is found", l08, c("LawSpecified", "GeoGoverningLaw"),
       \(.t) all(.t$LawSpecified == "named"), paste(l08$LawSpecified, collapse = ", "))
chk_on("The named jurisdiction is DELAWARE", l08, "GeoGoverningLaw",
       \(.t) all(toupper(dplyr::coalesce(.t$GeoGoverningLaw, "")) == "DELAWARE"),
       paste(dplyr::coalesce(l08$GeoGoverningLaw, "<none>"), collapse = ", "))

l09 <- dplyr::filter(tab_law, .data$DocID == "doc09")
chk_on("A self-referential clause counts as specified", l09, "LawSpecified",
       \(.t) all(.t$LawSpecified == "self-referential"),
       paste(l09$LawSpecified, collapse = ", "))

l10 <- dplyr::filter(tab_law, .data$DocID == "doc10")
chk_on("A document with no clause reports none found", l10, "LawSpecified",
       \(.t) all(.t$LawSpecified == "none found"), paste(l10$LawSpecified, collapse = ", "))
chk("Every document gets a law row, including those with no geography",
    dplyr::n_distinct(tab_law$DocID) == nrow(tab_docs),
    paste0(dplyr::n_distinct(tab_law$DocID), " of ", nrow(tab_docs)))

cli::cli_h2("Attachment")

r11 <- dplyr::filter(tab_roles, .data$DocID == "doc11", .data$GeoKey == "MINNEAPOLIS")
chk_on("A place beside an organisation attaches", r11, c("Attached", "DistToOrg"),
       \(.t) any(.t$Attached), paste0("DistToOrg = ", paste(r11$DistToOrg, collapse = ", ")))

r12 <- dplyr::filter(tab_roles, .data$DocID == "doc12", .data$GeoKey == "MINNEAPOLIS")
chk_on("A place beyond the reach does not attach", r12, c("Attached", "DistToOrg"),
       \(.t) !any(.t$Attached),
       paste0("reach ", .lT$Reach, "; the filler is about 468 characters"))
chk_on("An unattached place carries no organisation name", r12, c("Attached", "OrgName"),
       \(.t) all(is.na(.t$OrgName)),
       "a name here would be a place claimed by an organisation it is not near")

cli::cli_h2("The document table")

chk("One row per document per engine", nrow(tab_doc) == nrow(tab_docs),
    paste0(nrow(tab_doc), " rows for ", nrow(tab_docs), " documents"))
chk("The document with no geography still gets a row", any(tab_doc$DocID == "doc10"),
    "absent and zero are different facts and must stay distinguishable")
n_cols <- dplyr::select(tab_doc, dplyr::starts_with("N"))
chk("Counts are integers, not doubles",
    ncol(n_cols) > 0L && all(purrr::map_lgl(n_cols, is.integer)),
    paste0(ncol(n_cols), " count column(s); rowSums() returns double and renders 723.000"))
chk("The governing-law columns reached the document table",
    all(c("GeoGoverningLaw", "LawSpecified") %in% names(tab_doc)),
    paste(setdiff(c("GeoGoverningLaw", "LawSpecified"), names(tab_doc)), collapse = ", "))


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
    "{nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} passed. The GPE path carries a place \\
     name from contract text to a document-level variable, the gate refuses what it should, and \\
     the offsets survive the seam."
  )
} else {
  cli::cli_alert_danger(
    "{n_fail} of {nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} failed. Read the Note \\
     column, then the artifacts below -- a failure here is a broken variable, not a broken test."
  )
}


# 7. What was written, for inspection ----
#
# The chain's state at every stage, so a failed check can be read rather than guessed at. Parquet
# for the tables and CSV for the verdict, which is the one likely to be opened in a spreadsheet.

arrow::write_parquet(tab_docs,     fs::path(.dir_out, "01_fixture.parquet"))
arrow::write_parquet(tab_raw,      fs::path(.dir_out, "02_spans_raw.parquet"))
arrow::write_parquet(tab_resolved, fs::path(.dir_out, "03_resolved.parquet"))
arrow::write_parquet(tab_geo,      fs::path(.dir_out, "04_levelled.parquet"))
arrow::write_parquet(tab_ctx,      fs::path(.dir_out, "05_context.parquet"))
arrow::write_parquet(tab_law,      fs::path(.dir_out, "06_law.parquet"))
arrow::write_parquet(tab_roles,    fs::path(.dir_out, "07_roles.parquet"))
arrow::write_parquet(tab_orgtab,   fs::path(.dir_out, "08_org_table.parquet"))
arrow::write_parquet(tab_doc,      fs::path(.dir_out, "09_doc_table.parquet"))
readr::write_csv(tab_checks,       fs::path(.dir_out, "10_checks.csv"))

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
  "The document table is 09_doc_table.parquet; 04_levelled.parquet is where a resolution failure \\
   would be visible, and 05_context.parquet carries the characters the governing-law cue read."
)
