# ======================================================================================================================
# Test-MatCon-LAW.R -- governing law as a clause, and the containment rule that replaces proximity
# ======================================================================================================================
#
# WHAT THIS TESTS
# lawregex emits a clause. It does NOT resolve the jurisdiction. So the thing under test is not one
# extractor but a JOIN: a LAW span from lawregex, a GPE span from gazetteer, and the question of
# whether the second sits inside the first. Both labels are extracted here for that reason.
#
# THE CHANGE THIS FILE EXISTS TO JUSTIFY
# 04B2's geo_law() takes every place the gazetteer resolved to a State or Country, reads the 120
# characters BEFORE it, and asks whether that window carries cue language. Two consequences:
#
#   IT IS PROXIMITY. "...governed by New York law. Delaware corporations shall..." puts Delaware
#   within 120 characters of a cue and Delaware is not the jurisdiction. Document 09 is that
#   sentence, and the check on it is the single most important one here: the OLD rule gets it wrong
#   and the new one cannot.
#
#   IT NEEDS THE TEXT, TWICE -- once for the window and once more for a whole-document uppercase in
#   the self-referential arm. That is why 04D runs with Cues = FALSE, and with the cues off the
#   governing-law columns DO NOT APPEAR AT ALL. Not zero: absent.
#
# THE RULE UNDER TEST, WHICH READS NO TEXT
#
#   a LAW span with a GPE span inside it         -> named, jurisdiction is that GPE's GeoUnit
#   a LAW span whose LabelRaw is SelfReferential -> self-referential
#   no LAW span                                  -> none found
#
# Everything above is offsets and stored columns. If these checks pass, geo_law()'s .path_text
# argument and its .lens scan can both go.
#
# WHAT IS NOT SETTLED HERE
# REACH = 120 is a declared constant, not a measured one. This file proves the MECHANISM; what the
# number should be is a question for the 4,398-document sample, where these answers can be set
# beside geo_law()'s. Document 06 is the case that would move it -- a cue sitting early with a long
# jurisdiction phrase after it.
#
# IT REPORTS, IT DOES NOT ABORT. Nothing outside tempdir() is written.
#
# Usage:
#   source(here::here("1_code", "_Tests", "Test-MatCon-LAW.R"), encoding = "UTF-8")

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

.dir_out <- fs::path(tempdir(), "Test-MatCon-LAW")
fs::dir_create(.dir_out)

.lT <- list(
  Lookup = here::here("contracts-extract", "src", "matcon_extract", "data", "geo_lookup.parquet"),
  Stage  = fs::path(.dir_out, "staged_text.parquet"),
  Spans  = fs::path(.dir_out, "spans"),
  Store  = fs::path(.dir_out, "store"),
  # lawregex's own constant, restated so the fixture can be read against it. A change there should
  # fail a check here.
  Reach  = 120L,
  # 04B2's released cue window, for the side-by-side against the old mechanism.
  OldWin = 120L
)

# tempdir() IS PER-SESSION, NOT PER-RUN. A second run inside one R session finds the first run's
# DuckDB file, and ner_manifest_write() then correctly refuses to ingest rows whose spec hash
# differs from what that file holds under the same model tag.
purrr::walk(c(.lT$Spans, .lT$Store), \(.d) if (fs::dir_exists(.d)) fs::dir_delete(.d))
# The context width matcon now stores either side of every span. It lives in
# _io.DEFAULT_CUE and in every extractor's SPEC, so a change there moves the hash and
# should fail the re-cut below rather than pass quietly.
.lT$Cue <- 160L

fs::dir_create(c(.lT$Spans, .lT$Store))

cli::cli_h1("Test-MatCon-LAW")
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
    .name <- "A governing-law clause is found"
    .ok   <- TRUE
    .note <- ""
  }

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
    .name <- "A governing-law clause is found"
    .tab  <- dplyr::filter(tab_law, .data$DocID == "doc01")
    .cols <- "LabelRaw"
    .fun  <- \(.t) nrow(.t) > 0L
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
    .name <- "A document with no clause emits nothing"
    .tab  <- dplyr::filter(tab_law, .data$DocID == "doc12")
    .note <- ""
  }

  chk(.name, nrow(.tab) == 0L,
      if (nrow(.tab) == 0L) .note else paste0(nrow(.tab), " row(s) emitted; ", .note))
}


# 2. The fixture: twelve documents whose right answer is known ----

tab_docs <- tibble::tribble(
  ~DocID,   ~TextRaw,
  # -- the cues, one per document ---------------------------------------------------------------
  # 01 the commonest form: the clause opens on "governed by and construed" and ends at the stop
  "doc01",  paste("This Agreement shall be governed by and construed in accordance with the laws",
                  "of the State of Delaware. The parties agree as follows."),
  # 02 the short cue on its own
  "doc02",  "This Agreement shall be governed by the laws of the State of New York.",
  # 03 a Commonwealth, which is its own cue in 04B2's list
  "doc03",  "The parties agree to the laws of the Commonwealth of Massachusetts as governing.",
  # 04 a jurisdiction submission, which 04B2 treats as a governing-law cue
  "doc04",  "The parties submit to the jurisdiction of the courts of Delaware for all disputes.",
  # 05 a section heading, which is a cue with nothing after it on the same line
  "doc05",  "GOVERNING LAW. This Agreement is made under the laws of the State of Texas.",

  # -- the self-referential form ----------------------------------------------------------------
  # 06 SPECIFIES the law and names no place. Not a coverage gap: 11.9% of leases per 04B2.
  "doc06",  paste("This Agreement shall be governed by the laws of the State in which the Premises",
                  "are located."),
  # 07 the jurisdiction variant of the same idea
  "doc07",  "This Agreement is governed by the laws of the jurisdiction in which the Work is done.",

  # -- what the clause must NOT swallow ---------------------------------------------------------
  # 08 TWO CUES, ONE CLAUSE. "governing law" and "governed by" open within a few characters;
  #    keep_longest() must emit one span rather than two.
  "doc08",  "GOVERNING LAW: this Agreement is governed by the laws of the State of Illinois.",
  # 09 THE CASE THAT JUSTIFIES THE WHOLE CHANGE. The clause names New York; Delaware follows in the
  #    NEXT sentence, within 120 characters of the cue. geo_law()'s window catches Delaware.
  #    Containment cannot, because the full stop ends the clause before Delaware begins.
  "doc09",  paste("This Agreement is governed by the laws of the State of New York.",
                  "Delaware corporations shall deliver a certificate of good standing."),

  # -- boundaries ---------------------------------------------------------------------------------
  # 10 a clause with a cue and NO place in it at all: the span is still emitted, and the rule then
  #    reports neither named nor self-referential
  "doc10",  "GOVERNING LAW. The provisions of this Section survive termination of this Agreement.",
  # 11 a place named with no cue anywhere: a GPE span with no LAW span around it
  "doc11",  "The Supplier maintains its registered office in Delaware and ships from there.",
  # 12 neither: the document must still reach the ledger and report none found
  "doc12",  "The parties agree that all obligations hereunder shall be performed promptly."
)

arrow::write_parquet(tab_docs, .lT$Stage)

cli::cli_alert_info("{nrow(tab_docs)} {cli::qty(nrow(tab_docs))}fixture document{?s} staged.")

tab_lens <- tab_docs |>
  dplyr::transmute(.data$DocID, DocLen = stringi::stri_length(.data$TextRaw))

tab_keys <- tab_docs |>
  dplyr::transmute(.data$DocID, Class = "Lease", AmendType = "Original")


# 3. Extraction: both labels, because the rule is a join ----

tab_describe <- ner_matcon_describe() |>
  dplyr::mutate(Family = "matcon") |>
  tidyr::unnest_longer(col = "Entities", values_to = "Entity") |>
  dplyr::mutate(Note = dplyr::if_else(.data$Ready, "", "data dependency absent")) |>
  dplyr::select("Family", "Model", "Entity", "SpecHash", "Ready", "Note")

chk("lawregex is registered and available",
    "LAW" %in% tab_describe$Entity,
    paste(sort(unique(tab_describe$Entity)), collapse = ", "))

hash_law <- tab_describe$SpecHash[tab_describe$Entity == "LAW"]
hash_gpe <- tab_describe$SpecHash[tab_describe$Entity == "GPE"]
cli::cli_alert_info(
  "lawregex spec hash: {(if (length(hash_law)) hash_law[[1L]] else '<absent>')}; \\
   gazetteer: {(hash_gpe[[1L]])}."
)

tab_staged <- ner_extract(
  .family     = "matcon",
  .model      = NULL,
  .entity     = c("LAW", "GPE"),   # two modules, two files
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
ner_ingest(.con = con_matcon, .staged = tab_staged, .entity = c("LAW", "GPE"))

tab_ledger <- DBI::dbGetQuery(
  con_matcon, "SELECT DocID, Entity, Status FROM runs ORDER BY DocID, Entity"
) |>
  tibble::as_tibble()
DBI::dbDisconnect(con_matcon, shutdown = TRUE)

tab_law <- ent_load_entity(
  .dir_store = .lT$Store, .family = "matcon", .entity = "LAW",
  .lens = tab_lens, .extras = ent_extras("matcon", "LAW"), .model = NULL, .quiet = TRUE
)

tab_gpe_raw <- ent_load_entity(
  .dir_store = .lT$Store, .family = "matcon", .entity = "GPE",
  .lens = tab_lens, .extras = ent_extras("matcon", "GPE"), .model = NULL, .quiet = TRUE
)


# 4. The chain: resolve the places, then contain them ----

geo_once <- list(
  Lookup = geo_lookup(.path_lookup = .lT$Lookup),
  Cand   = geo_candidates(.path_lookup = .lT$Lookup)
)

tab_gpe <- tab_gpe_raw |>
  geo_resolve(.lookup = geo_once$Lookup, .family = "matcon") |>
  dplyr::mutate(Combo = "matcon", .before = 1L) |>
  geo_city_state(.cand = geo_once$Cand) |>
  geo_country()

# THE RULE, AND IT READS NO TEXT. A non-equi join on offsets: which resolved place sits inside which
# clause.
#
# THE PLACE IS THE X SIDE, AND GETTING THAT BACKWARDS COST A RUN. join_by()'s
# within(x_lower, x_upper, y_lower, y_upper) asks whether the X range fits inside the Y range, so
# the CONTAINED thing belongs on the left. The first version put the clause there and therefore
# asked whether a 45-character clause fits inside an 8-character place. It does not, so every
# document came back with no jurisdiction while the offsets plainly nested:
#
#   LAW  [24 .. 69]  "governed by the laws of the State of New York"
#   GPE  [61 .. 69]  "New York"
#
# within() IS NOT NAMESPACED, and cannot be. join_by() evaluates a small DSL of its own --
# between(), within(), overlaps() -- in its own context; dplyr exports no such function, so
# dplyr::within() would resolve to base R's and error. Same standing as .data$ in a verb.
tab_inside <- tab_gpe |>
  dplyr::select("DocID", "Start", "Stop", "Span", "GeoUnit", "GeoLevel") |>
  dplyr::inner_join(
    dplyr::select(tab_law, DocID, LawStart = "Start", LawStop = "Stop", LawSpan = "Span",
                  LawKind = "LabelRaw"),
    by = dplyr::join_by(DocID, within(Start, Stop, LawStart, LawStop))
  )

# THE CLAUSES THAT CONTAINED NOTHING HAVE TO COME BACK. tab_inside is an inner join, so a clause
# with no place inside it is absent from it entirely -- and those are exactly the rows that reach
# "self-referential" and "clause without a place". Starting from tab_law and joining the
# containment back on keeps every clause and adds a jurisdiction where there was one.
tab_rule <- tab_law |>
  dplyr::select(DocID, LawStart = "Start", LawKind = "LabelRaw") |>
  dplyr::left_join(
    dplyr::select(tab_inside, "DocID", "LawStart", "Start", "GeoUnit", "GeoLevel"),
    by = dplyr::join_by(DocID, LawStart)
  ) |>
  dplyr::filter(.data$GeoLevel %in% c("State", "Country") | is.na(.data$GeoLevel)) |>
  # A CLAUSE THAT FOUND SOMETHING OUTRANKS ONE THAT DID NOT, and position only decides between
  # equals. "GOVERNING LAW. This Agreement is made under the laws of the State of Texas." is TWO
  # clauses: the heading, closed by its own full stop at zero useful characters, and the sentence
  # carrying Texas. Ordering by offset alone takes the heading and discards the jurisdiction --
  # a systematic miss, because a heading followed by the substantive sentence is ordinary drafting.
  #
  # is.na() sorts FALSE before TRUE, so this is the whole of it.
  dplyr::arrange(.data$DocID, is.na(.data$GeoUnit), .data$LawStart, .data$Start) |>
  dplyr::slice_head(n = 1L, by = DocID) |>
  dplyr::transmute(
    .data$DocID,
    GeoGoverningLaw = .data$GeoUnit,
    LawLevel        = .data$GeoLevel,
    LawSpecified    = dplyr::case_when(
      !is.na(.data$GeoUnit)              ~ "named",
      .data$LawKind == "SelfReferential" ~ "self-referential",
      .default = "clause without a place"
    )
  )

tab_out <- tab_keys |>
  dplyr::select("DocID") |>
  dplyr::left_join(tab_rule, by = dplyr::join_by(DocID)) |>
  dplyr::mutate(LawSpecified = dplyr::coalesce(.data$LawSpecified, "none found"))

cli::cli_alert_info(
  "Chain complete: {nrow(tab_law)} clause{?s}, {nrow(tab_gpe)} resolved place{?s}, \\
   {nrow(tab_out)} document row{?s}."
)


# 5. The checks ----

#' Clauses for one document
#'
#' @param .doc DocID.
#' @return Tibble, possibly empty.
law_for <- function(.doc) {
  if (FALSE) .doc <- "doc01"
  dplyr::filter(tab_law, .data$DocID == .doc)
}

#' The rule's answer for one document
#'
#' @param .doc DocID.
#' @return One-row tibble.
out_for <- function(.doc) {
  if (FALSE) .doc <- "doc01"
  dplyr::filter(tab_out, .data$DocID == .doc)
}

cli::cli_h2("The seam")

chk("The extractor wrote two parquets, one per module", nrow(tab_staged) == 2L,
    paste0(nrow(tab_staged), " file(s); LAW and GPE have different owners"))
chk("Every fixture document reached the ledger",
    dplyr::n_distinct(tab_ledger$DocID) == nrow(tab_docs),
    paste0(dplyr::n_distinct(tab_ledger$DocID), " of ", nrow(tab_docs)))
chk("Clauses were read back out of the store", nrow(tab_law) > 0L,
    paste0(nrow(tab_law), " clause(s)"))
# "NO EXTRAS" STOPPED BEING EXPRESSIBLE AS A LENGTH, and that is a fact about the design rather than
# a broken assertion. ent_extras() now APPENDS CueBefore and CueAfter to every matcon entity -- the
# same arrangement _io.Emitter uses, so that no module can forget them -- which means this module
# declaring none of its own still returns two. The claim under test is about what lawregex DECLARES,
# not about what arrives, so the universal columns are subtracted before the count is taken.
chk("LAW declares no extras of its own, by design",
    identical(setdiff(ent_extras("matcon", "LAW"), c("CueBefore", "CueAfter")), character(0)),
    paste0("ent_extras() returns ", paste(ent_extras("matcon", "LAW"), collapse = ", "),
           " -- the jurisdiction is the gazetteer's answer, resolved once rather than twice"))

cut_ <- stringi::stri_sub(
  tab_docs$TextRaw[match(tab_law$DocID, tab_docs$DocID)],
  from = tab_law$Start + 1L, to = tab_law$Stop
)
chk("Every clause equals the text at its own offsets",
    nrow(tab_law) > 0L && all(cut_ == tab_law$Span),
    paste0(sum(cut_ != tab_law$Span), " mismatch(es) over ", nrow(tab_law), " clause(s)"))

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

cue_checks(tab_law, "LAW")

cli::cli_h2("The cues")

for (d_ in c("doc01", "doc02", "doc03", "doc04", "doc05")) {
  got_ <- law_for(d_)
  chk_on(paste0("A clause is found in ", d_), got_, c("Span", "LabelRaw"),
         \(.t) nrow(.t) >= 1L,
         paste0(paste(got_$LabelRaw, collapse = ", "), ": ",
                stringi::stri_sub(paste(got_$Span, collapse = " | "), to = 60L)))
}

chk("Every LabelRaw is a declared cue or SelfReferential",
    nrow(tab_law) > 0L &&
      all(tab_law$LabelRaw %in% c("GovernedByConstrued", "GovernedBy", "LawsOfState",
                                  "LawsOfCommonwealth", "ConstruedAccordance", "GoverningLaw",
                                  "SubmitJurisdiction", "SelfReferential")),
    paste(sort(unique(tab_law$LabelRaw)), collapse = ", "))

cli::cli_h2("Two cues, one clause")

l08 <- law_for("doc08")
chk_on("Overlapping cues resolve to a single clause", l08, c("Start", "Stop"),
       \(.t) nrow(.t) == 1L,
       paste0(nrow(l08), " clause(s): ", paste(l08$LabelRaw, collapse = ", "),
              " -- keep_longest() decides, as it does for dates"))

cli::cli_h2("The jurisdiction actually arrives")

# THE CHECK THE FIRST RUN DID NOT HAVE, AND SHOULD HAVE. 27 of 28 checks passed on a rule that
# returned NA for every document, because every one of them tested a SHAPE -- clause found, span
# classified, row emitted -- and the shapes were all correct. Only a check asserting a VALUE could
# fail. At least one per mechanism must.
chk("The containment join found at least one place inside a clause",
    nrow(tab_inside) > 0L,
    paste0(nrow(tab_inside), " place(s) inside a clause, from ", nrow(tab_law), " clause(s) and ",
           nrow(tab_gpe), " place(s)"))

o02 <- out_for("doc02")
chk_on("A plain clause names its jurisdiction", o02, "GeoGoverningLaw",
       \(.t) toupper(dplyr::coalesce(.t$GeoGoverningLaw, "")) == "NEW YORK",
       paste0(dplyr::coalesce(o02$GeoGoverningLaw, "<none>"),
              " -- the place sits at 61-69 inside a clause at 24-69"))

chk("Every clause containing a State yields a named jurisdiction",
    nrow(tab_out) > 0L &&
      sum(tab_out$LawSpecified == "named") == dplyr::n_distinct(
        dplyr::filter(tab_inside, .data$GeoLevel %in% c("State", "Country"))$DocID),
    paste0(sum(tab_out$LawSpecified == "named"), " named of ",
           dplyr::n_distinct(dplyr::filter(tab_inside,
                                           .data$GeoLevel %in% c("State", "Country"))$DocID),
           " document(s) with a place inside a clause"))

# THE HEADING CASE, CHECKED BY NAME. doc05 is why tab_rule orders on is.na(GeoUnit) rather than on
# offset alone, and a rule changed to fix one document should have that document asserted rather
# than left to a count.
o05 <- out_for("doc05")
chk_on("A section heading does not displace the clause carrying the jurisdiction", o05,
       c("GeoGoverningLaw", "LawSpecified"),
       \(.t) toupper(dplyr::coalesce(.t$GeoGoverningLaw, "")) == "TEXAS",
       paste0(dplyr::coalesce(o05$GeoGoverningLaw, "<none>"), " / ", o05$LawSpecified,
              " -- two clauses here: the heading at 0-13 and the sentence at 48-74"))

cli::cli_h2("Containment, not proximity -- the case that justifies the change")

l09 <- law_for("doc09")
g09 <- dplyr::filter(tab_gpe, .data$DocID == "doc09")
o09 <- out_for("doc09")

chk_on("doc09 yields one clause", l09, "Span", \(.t) nrow(.t) == 1L,
       stringi::stri_sub(paste(l09$Span, collapse = " | "), to = 70L))
chk_on("Both places are found by the gazetteer", g09, "GeoUnit",
       \(.t) dplyr::n_distinct(.t$GeoUnit) >= 2L,
       paste(sort(unique(g09$GeoUnit)), collapse = ", "))

# THE CHECK. Delaware sits within 120 characters of the cue, so geo_law()'s window catches it;
# the full stop ends the clause before Delaware begins, so containment cannot.
chk_on("The named jurisdiction is NEW YORK, not the state in the next sentence", o09,
       "GeoGoverningLaw",
       \(.t) toupper(dplyr::coalesce(.t$GeoGoverningLaw, "")) == "NEW YORK",
       paste0(dplyr::coalesce(o09$GeoGoverningLaw, "<none>"),
              " -- proximity would admit Delaware here"))

dist_ <- if (nrow(l09) == 1L && any(toupper(g09$GeoUnit) == "DELAWARE")) {
  min(g09$Start[toupper(g09$GeoUnit) == "DELAWARE"]) - l09$Start[[1L]]
} else {
  NA_integer_
}
chk("And Delaware really was within the old window, so the difference is real",
    !is.na(dist_) && dist_ <= .lT$OldWin,
    paste0("Delaware begins ", dist_, " characters after the cue; the old window is ",
           .lT$OldWin))

cli::cli_h2("The self-referential form")

for (d_ in c("doc06", "doc07")) {
  got_ <- law_for(d_)
  chk_on(paste0("A self-referential clause is classed as such in ", d_), got_, "LabelRaw",
         \(.t) all(.t$LabelRaw == "SelfReferential"),
         paste(got_$LabelRaw, collapse = ", "))
}
chk_on("And the rule reports self-referential, not none found", out_for("doc06"),
       "LawSpecified", \(.t) all(.t$LawSpecified == "self-referential"),
       paste0(out_for("doc06")$LawSpecified,
              " -- recording nothing would count a real term as a coverage gap"))
chk_on("A self-referential clause names no place", out_for("doc06"), "GeoGoverningLaw",
       \(.t) all(is.na(.t$GeoGoverningLaw)),
       "by construction: that is what makes it self-referential")

cli::cli_h2("Boundaries")

chk_on("A clause with no place in it is still emitted", law_for("doc10"), "Span",
       \(.t) nrow(.t) >= 1L,
       "the clause exists; only the jurisdiction is absent")
chk_on("And the rule distinguishes it from none found", out_for("doc10"), "LawSpecified",
       \(.t) all(.t$LawSpecified == "clause without a place"),
       paste0(out_for("doc10")$LawSpecified,
              " -- a cue with nothing in it is not the same as no cue"))

chk_none("A place with no cue around it yields no clause", law_for("doc11"),
         "a registered office is not a governing-law clause")
chk_on("So the rule reports none found for it", out_for("doc11"), "LawSpecified",
       \(.t) all(.t$LawSpecified == "none found"), out_for("doc11")$LawSpecified)

chk_none("A document with neither yields no clause", law_for("doc12"),
         "and must still appear in the output -- checked below")
chk("Every document gets a row, including those with no clause",
    nrow(tab_out) == nrow(tab_docs),
    paste0(nrow(tab_out), " rows for ", nrow(tab_docs), " documents"))

cli::cli_h2("What the rule read")

# THE CLAIM, TESTED RATHER THAN ASSERTED. The first version of this check deparsed sys.function()
# looking for ".path_text" -- at top level there is no enclosing function, so it inspected nothing
# and could not fail. The honest test is to remove the text and recompute: if the rule still gives
# the same answers with the staged parquet deleted, it demonstrably never opened it.
fs::file_delete(.lT$Stage)

# THE SAME INVERSION, and it matters here more than anywhere. Two identical WRONG answers still
# agree, so a recheck built from the same mistaken join would have passed while proving nothing.
inside_ <- tab_gpe |>
  dplyr::select("DocID", "Start", "Stop", "GeoUnit", "GeoLevel") |>
  dplyr::inner_join(
    dplyr::select(tab_law, DocID, LawStart = "Start", LawStop = "Stop"),
    by = dplyr::join_by(DocID, within(Start, Stop, LawStart, LawStop))
  )

tab_recheck <- tab_law |>
  dplyr::select(DocID, LawStart = "Start") |>
  dplyr::left_join(
    dplyr::select(inside_, "DocID", "LawStart", "Start", "GeoUnit", "GeoLevel"),
    by = dplyr::join_by(DocID, LawStart)
  ) |>
  dplyr::filter(.data$GeoLevel %in% c("State", "Country") | is.na(.data$GeoLevel)) |>
  # THE SAME ORDERING, or the comparison stops being like-for-like and the check passes on two
  # answers that differ from the rule above rather than on two that agree with it.
  dplyr::arrange(.data$DocID, is.na(.data$GeoUnit), .data$LawStart, .data$Start) |>
  dplyr::slice_head(n = 1L, by = DocID) |>
  dplyr::transmute(.data$DocID, GeoGoverningLaw = .data$GeoUnit)

joined_ <- tab_out |>
  dplyr::select("DocID", Was = "GeoGoverningLaw") |>
  dplyr::left_join(tab_recheck, by = dplyr::join_by(DocID))

chk("The rule gives the same answers with the document text DELETED",
    nrow(joined_) > 0L &&
      all(dplyr::coalesce(joined_$Was, "") == dplyr::coalesce(joined_$GeoGoverningLaw, "")),
    paste0(sum(dplyr::coalesce(joined_$Was, "") != dplyr::coalesce(joined_$GeoGoverningLaw, "")),
           " difference(s); geo_law()'s .path_text argument and its .lens scan can both go"))

cli::cli_alert_info("The rule's answers, per document:")
tab_out |>
  dplyr::left_join(dplyr::select(tab_law, "DocID", Cue = "LabelRaw"),
                   by = dplyr::join_by(DocID), multiple = "first") |>
  print(n = Inf, width = Inf)


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
    "{nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} passed. Governing law is a clause \\
     with a place inside it, the rule reads no text, and the state in the next sentence is not \\
     mistaken for the jurisdiction."
  )
} else {
  cli::cli_alert_danger(
    "{n_fail} of {nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} failed. Read the Note \\
     column, then the artifacts below."
  )
}


# 7. What was written, for inspection ----

arrow::write_parquet(tab_docs,   fs::path(.dir_out, "01_fixture.parquet"))
arrow::write_parquet(tab_law,    fs::path(.dir_out, "02_clauses.parquet"))
arrow::write_parquet(tab_gpe,    fs::path(.dir_out, "03_places.parquet"))
arrow::write_parquet(tab_inside, fs::path(.dir_out, "04_containment.parquet"))
arrow::write_parquet(tab_out,    fs::path(.dir_out, "05_rule.parquet"))
readr::write_csv(tab_checks,     fs::path(.dir_out, "06_checks.csv"))

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
  "04_containment.parquet is the one to read: one row per clause and place-inside-it, which is the \\
   whole mechanism. 02_clauses.parquet shows how far each clause ran, which is what REACH decides."
)
