# ======================================================================================================================
# Test-LexNLP.R -- the whole LexNLP path, end to end, and the BASELINE the Docker freeze is measured against
# ======================================================================================================================
#
# WHY THIS EXISTS, AND WHY IT IS DIFFERENT FROM THE FOUR MATCON TESTS
# Those four pin a pure-Python extractor whose identity is a SPEC dict: edit a pattern and the hash
# moves, leave it alone and it does not. Freezing contracts-extract could therefore be proved
# harmless by re-running them and reading four unchanged hashes.
#
# LEXNLP'S HASH CANNOT DO THAT JOB. image_spec.SOURCES fingerprints FILE CONTENTS -- the Dockerfile,
# extract_lexnlp.py, company_types.csv and app/geoentities.csv. Pinning the base image by digest and
# adding requirements.lock.txt to that tuple MOVES THE HASH BY DESIGN, whether or not the container
# behaves any differently. A moved hash then proves nothing either way.
#
# So this file compares OUTPUT. It prints a content hash of its own normalised span table, and that
# number is what must survive the rebuild. Run it before the Dockerfile is touched, keep the hash,
# run it after, and compare. The image hash records WHICH image ran; the span hash records WHETHER
# THE ANSWER MOVED, and only the second is a claim about the corpus.
#
# ORG IS THE ENTITY THAT MATTERS MOST HERE. 04D produced 20,837,808 organisation spans from this
# family, ent_apply()'s corporate-family rule reads NameCore off them, and 04B1's entire party chain
# rests on the split between a company's name and its legal form. If a rebuild moved how LexNLP
# resolves a company name, this is where it shows.
#
# TWO SEAM TRAPS ARE PINNED, BOTH DOCUMENTED IN _extra_values() AND BOTH INVISIBLE WHEN THEY FIRE
#
#   AN AMOUNT CROSSES AS A DECIMAL STRING. A Decimal becomes a float once pandas touches it, and
#   "a contract value of 9,752,233.001 does not survive that intact". The fixture carries exactly
#   that figure.
#
#   NO VALUE MAY BE THE LITERAL "nan". LexNLP's geo table is read with pandas, so an absent ISO-3
#   arrives as a float NaN and str() turns it into "nan" -- "not null in parquet, not null in
#   DuckDB, and looks exactly like a country code three characters long. The store then holds nan
#   where it should hold nothing, and every count of resolved codes is wrong in the direction that
#   looks healthy." Checked across every extras column, not only the ISO ones.
#
# THIS TEST NEEDS DOCKER, AND THE FOUR MATCON ONES DELIBERATELY DO NOT. A test that cannot run
# without a container is a test that stops being run, which is why the matcon four avoid the
# dependency. Here it is inherent: the extractor IS the container.
#
# TWO THINGS THE FIRST RUN ESTABLISHED, AND BOTH CHANGED WHAT THIS FILE CHECKS
#
#   LegalForm IS CANONICAL, NOT LITERAL. company_types.csv is (Alias, Abbreviation, Label) and five
#   aliases -- CO, Corp, Corporation, Inc, Incorporated -- all map to the single abbreviation CORP.
#   INC is not an Abbreviation anywhere in the 143-row table. The first version of this file
#   asserted "Inc." would give INC, which was an assumption about the seam rather than a reading of
#   it. _Entity.R's own comment says "LexNLP's company_type_abbr: CORP, INC, LLC -- and NA", which
#   lists a value the table cannot produce.
#
#   LEXNLP EMITTED NO MONEY AT ALL on two ordinary dollar figures. That is not a seam failure -- the
#   container reported 11 candidates and none of them was MONEY. Section 5's money block is
#   therefore a DIAGNOSTIC over five phrasings rather than a pass/fail on one: what 04B4 needs to
#   know is which forms this grammar accepts, and "zero exact agreement with matcon" means something
#   different if the disagreement is absence.
#
# IT REPORTS, IT DOES NOT ABORT. Nothing outside tempdir() is written.
#
# Usage:
#   source(here::here("1_code", "_Tests", "Test-LexNLP.R"), encoding = "UTF-8")

here::i_am("1_code/_Commons/_Initialize.R")

options(cli.num_colors = 1, cli.width = 120)

source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),     encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_NER.R"),        encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Entity.R"),     encoding = "UTF-8")


# 1. Configuration ----

.dir_out <- fs::path(tempdir(), "Test-LexNLP")
fs::dir_create(.dir_out)

.lT <- list(
  Stage    = fs::path(.dir_out, "staged_text.parquet"),
  Spans    = fs::path(.dir_out, "spans"),
  Store    = fs::path(.dir_out, "store"),
  Image    = "contracts-lexnlp",
  Entities = c("ORG", "GPE", "DATE", "MONEY"),   # exactly what 04C asks this family for
  # The exact figure _extra_values() names as the one a float cannot carry.
  Precise  = "9,752,233.001"
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

cli::cli_h1("Test-LexNLP")
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
    .name <- "A legal form is split from the name"
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
    .name <- "A legal form is split from the name"
    .tab  <- dplyr::filter(tab_org, .data$DocID == "doc01")
    .cols <- c("NameCore", "LegalForm")
    .fun  <- \(.t) all(.t$LegalForm == "INC")
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


# 2. The image must be usable, and WHICH image ran is recorded ----
#
# THE BINARY IS NOT THE DAEMON. `docker` sits on PATH with Docker Desktop closed, so a check on the
# executable passes and the failure arrives from the container as "exited with status 1 after 0.1s"
# -- true, and useless. ner_lexnlp_image() distinguishes the four states.

tab_image <- ner_lexnlp_image(.image = .lT$Image)
cli::cli_alert_info(
  "Image state: {(tab_image$State[[1L]])}; spec hash {(tab_image$SpecHash[[1L]])}."
)

if (tab_image$State[[1L]] %in% c("nodocker", "nodaemon", "noimage")) {
  cli::cli_abort(c(
    "The LexNLP family cannot run: {(tab_image$Said[[1L]])}.",
    "i" = "This is the one test in the suite that needs a container. The four matcon tests do not."
  ))
}


# 3. The fixture: twelve documents whose right answer is known ----

tab_docs <- tibble::tribble(
  ~DocID,   ~TextRaw,
  # -- ORG: the split between a name and its legal form -----------------------------------------
  # 01 the commonest form. Name = Acme Holdings, TypeAbbr = INC.
  "doc01",  "This Agreement is entered into by Acme Holdings, Inc. and the Purchaser.",
  # 02 a limited liability company
  "doc02",  "Bravo Ventures, LLC hereby agrees to the terms set out in this Agreement.",
  # 03 a corporation written in full
  "doc03",  "Charlie Manufacturing Corporation shall deliver the goods on the closing date.",
  # 04 a limited partnership, where the abbreviation carries stops
  "doc04",  "Delta Capital Partners, L.P. is the general partner of the Fund.",
  # 05 TWO COMPANIES IN ONE SENTENCE, which is the ordinary shape of a contract preamble
  "doc05",  paste("This Agreement is made between Echo Systems, Inc. and Foxtrot Trading, LLC as",
                  "of the date first written above."),
  # 06 a company with a descriptor after it, which is where Description comes from
  "doc06",  "Golf Industries, Inc., a Delaware corporation, is the Seller under this Agreement.",

  # -- MONEY: five phrasings, because the first run found LexNLP emitting NONE ------------------
  # One form per document so the diagnostic below can attribute a hit to a phrasing rather than to
  # a sentence. 07 carries the exact figure _extra_values() names as the one a float cannot hold.
  "doc07",  "The aggregate purchase price shall be $9,752,233.001 payable at closing.",
  "doc08",  "The Borrower shall repay $2,500,000 under the terms of this facility.",
  "doc13",  "The Purchaser shall pay US$5,000,000 to the Seller on the closing date.",
  "doc14",  "The Purchaser shall pay 5,000,000 dollars to the Seller on the closing date.",
  "doc15",  "The Purchaser shall pay USD 5,000,000 to the Seller on the closing date.",
  "doc16",  "The Purchaser shall pay Five Million Dollars to the Seller on the closing date.",

  # -- DATE: the grammar that CAN supply a year, which is what matcon exists to avoid ------------
  # 09 a fully written date
  "doc09",  "This Agreement is dated as of March 15, 2020, and is effective from that date.",
  # 10 a date with NO year. matcon emits nothing here; LexNLP's grammar may supply one.
  "doc10",  "Payment shall be made on March 15 of each year without further notice.",

  # -- THE COUNTERPARTY KEY: what a unique counterparty is counted BY ---------------------------
  # 04D sets ent_rule(.key = "core"), so Key = CoreKey = ent_norm_key(coalesce(NameCore, Span)).
  # Every one of 04D's 20,837,808 organisation spans is counted through that expression, and these
  # five documents are the cases where it could go wrong.
  #
  # 17 NO LEGAL FORM AT ALL. get_company_annotations() is an NLTK maxent NER; if the suffix is what
  #    triggers detection then every counterparty written bare is absent from the store, and that
  #    is a COVERAGE gap rather than a key one -- invisible to any check on the spans that exist.
  "doc17",  "Acme Holdings shall deliver the goods on the closing date under this Agreement.",
  # 18 THE SAME COMPANY, TWO DRAFTING STYLES. company_types.csv maps Inc and Corporation both onto
  #    CORP, so these should reduce to one key. If they do not, one counterparty counts as two.
  "doc18",  "This Agreement is between Acme Holdings, Inc. and Acme Holdings Corporation.",
  # 19 SUFFIXED AND BARE IN ONE DOCUMENT, which is how a preamble names a party and then refers
  #    back to it. Same key, or the count doubles.
  "doc19",  "This Agreement is between Acme Holdings, Inc. and Acme Holdings as counterparty.",
  # 20 THE .min_key CASE 04B1's roxygen names: "CA, INC. reduces to CA and that is the company's
  #    name rather than a degenerate key".
  "doc20",  "CA, Inc. is the vendor under this Agreement and shall invoice monthly.",
  # 21 FORMS LEXNLP KNOWS AND .ent_suffix DOES NOT. company_types.csv carries 48 abbreviations --
  #    PC, PLLC, GP, LLLP, SARL, SpA, KK among them -- against roughly 21 in .ent_suffix. Where the
  #    two vocabularies disagree, a null NameCore would key differently from a populated one.
  "doc21",  "Zulu Advisors PC and Yankee Group LLLP are the advisors under this Agreement.",

  # -- GPE: where lexnlp's Iso2 is a COUNTRY code, not a subdivision code ------------------------
  # 11 a country, which is the level lexnlp resolves
  "doc11",  "The Supplier maintains its registered office in Germany under local law.",
  # 12 nothing of any kind: the pass must still record the document as processed
  "doc12",  "The parties agree that all obligations hereunder shall be performed promptly."
)

arrow::write_parquet(tab_docs, .lT$Stage)

cli::cli_alert_info("{nrow(tab_docs)} {cli::qty(nrow(tab_docs))}fixture document{?s} staged.")

tab_lens <- tab_docs |>
  dplyr::transmute(.data$DocID, DocLen = stringi::stri_length(.data$TextRaw))


# 4. Extraction: the container, the store, the read ----
#
# ner_extract() is the entry point 04A and 04C use, so this exercises the invocation the corpus was
# extracted with rather than a second one written for a test.

tab_describe <- tidyr::expand_grid(
  Family = "lexnlp",
  Model  = "lexnlp",
  Entity = .lT$Entities
) |>
  dplyr::mutate(
    SpecHash = tab_image$SpecHash[[1L]],
    Ready    = TRUE,
    Note     = ""
  )

tab_staged <- ner_extract(
  .family     = "lexnlp",
  .model      = NULL,             # the family versions itself through the image
  .entity     = .lT$Entities,
  .path_in    = .lT$Stage,
  .out_dir    = .lT$Spans,
  .describe   = tab_describe,
  .id_col     = "DocID",
  .text_col   = "TextRaw",
  .max_chars  = 0L,
  .workers    = 1L,               # in-process inside the container, so a failure is debuggable
  .batch_size = 8L,
  .timeout    = 240L,             # 04C's lexnlp timeout; the maxent company NER is the slow part
  .image      = .lT$Image,
  .quiet      = TRUE
)

con_lex <- ner_db_connect(
  .db_path   = ner_db_path(.dir = .lT$Store, .family = "lexnlp"),
  .read_only = FALSE
)
ner_db_init(.con = con_lex)
ner_ingest(.con = con_lex, .staged = tab_staged, .entity = .lT$Entities)

tab_ledger <- DBI::dbGetQuery(
  con_lex, "SELECT DocID, Entity, Status FROM runs ORDER BY DocID, Entity"
) |>
  tibble::as_tibble()
DBI::dbDisconnect(con_lex, shutdown = TRUE)

#' Load one entity out of the temporary store
#'
#' @param .entity Entity name.
#' @return Tibble of spans, empty where the family produced none.
load_ <- function(.entity) {
  if (FALSE) .entity <- "ORG"

  tryCatch(
    ent_load_entity(
      .dir_store = .lT$Store,
      .family    = "lexnlp",
      .entity    = .entity,
      .lens      = tab_lens,
      .extras    = ent_extras("lexnlp", .entity),
      .model     = NULL,
      .quiet     = TRUE
    ),
    error = function(.e) tibble::tibble()
  )
}

tab_org   <- load_("ORG")
tab_gpe   <- load_("GPE")
tab_date  <- load_("DATE")
tab_money <- load_("MONEY")

cli::cli_alert_info(
  "Chain complete: {nrow(tab_org)} ORG, {nrow(tab_gpe)} GPE, {nrow(tab_date)} DATE, \\
   {nrow(tab_money)} MONEY."
)


# 5. The checks ----

#' Spans of one entity for one document
#'
#' @param .tab One of the four loaded tables.
#' @param .doc DocID.
#' @return Tibble, possibly empty.
for_doc <- function(.tab, .doc) {
  if (FALSE) {
    .tab <- tab_org
    .doc <- "doc01"
  }
  if (nrow(.tab) == 0L) .tab else dplyr::filter(.tab, .data$DocID == .doc)
}

cli::cli_h2("The seam")

chk("The container wrote a parquet", nrow(tab_staged) >= 1L,
    paste0(nrow(tab_staged), " file(s) written"))
chk("Every fixture document reached the ledger",
    dplyr::n_distinct(tab_ledger$DocID) == nrow(tab_docs),
    paste0(dplyr::n_distinct(tab_ledger$DocID), " of ", nrow(tab_docs)))
chk("All four requested entities reached the ledger",
    all(.lT$Entities %in% tab_ledger$Entity),
    paste(sort(unique(tab_ledger$Entity)), collapse = ", "))
chk("Organisation spans were read back out of the store", nrow(tab_org) > 0L,
    paste0(nrow(tab_org), " span(s)"))

# LexNLP's annotation coords are DOCUMENT-ABSOLUTE, which the module docstring asserts and nothing
# has checked. Over code points, because the R side must not use substr().
tab_all <- dplyr::bind_rows(
  dplyr::select(tab_org,   "DocID", "Start", "Stop", "Span"),
  dplyr::select(tab_gpe,   "DocID", "Start", "Stop", "Span"),
  dplyr::select(tab_date,  "DocID", "Start", "Stop", "Span"),
  dplyr::select(tab_money, "DocID", "Start", "Stop", "Span")
)
cut_ <- stringi::stri_sub(
  tab_docs$TextRaw[match(tab_all$DocID, tab_docs$DocID)],
  from = tab_all$Start + 1L, to = tab_all$Stop
)
chk("Every span equals the text at its own offsets",
    nrow(tab_all) > 0L && all(cut_ == tab_all$Span),
    paste0(sum(cut_ != tab_all$Span), " mismatch(es) over ", nrow(tab_all), " span(s)"))

cli::cli_h2("The organisation split, which the party chain rests on")

o01 <- for_doc(tab_org, "doc01")
chk_on("A company is found at all", o01, "Span", \(.t) nrow(.t) > 0L,
       paste(o01$Span, collapse = " | "))
chk_on("NameCore carries the name without its legal form", o01, "NameCore",
       \(.t) any(stringi::stri_detect_fixed(stringi::stri_trans_toupper(
         dplyr::coalesce(.t$NameCore, "")), "ACME")),
       paste(dplyr::coalesce(o01$NameCore, "<null>"), collapse = " | "))
# CANONICAL, NOT LITERAL, and the first version of this check assumed otherwise. company_types.csv
# maps CO, Corp, Corporation, Inc and Incorporated all onto CORP; INC is not an Abbreviation in the
# table at all. So "Inc." giving CORP is the table working, and asserting INC was reading the seam
# through an assumption instead of through its own data.
chk_on("LegalForm carries the CANONICAL form, so Inc. gives CORP", o01, "LegalForm",
       \(.t) any(dplyr::coalesce(.t$LegalForm, "") == "CORP"),
       paste0(paste(dplyr::coalesce(o01$LegalForm, "<null>"), collapse = " | "),
              " -- five aliases collapse onto this one abbreviation"))

chk_on("An LLC is recognised", for_doc(tab_org, "doc02"), c("NameCore", "LegalForm"),
       \(.t) nrow(.t) > 0L,
       paste(dplyr::coalesce(for_doc(tab_org, "doc02")$LegalForm, "<null>"), collapse = " | "))
# THE COLLAPSE MADE VISIBLE: a document writing "Corporation" and one writing "Inc." must land on
# the same abbreviation, because that is what the table says they mean.
chk_on("A corporation written in full gives the same CORP", for_doc(tab_org, "doc03"),
       c("NameCore", "LegalForm"),
       \(.t) any(dplyr::coalesce(.t$LegalForm, "") == "CORP"),
       paste(dplyr::coalesce(for_doc(tab_org, "doc03")$LegalForm, "<null>"), collapse = " | "))
chk_on("A limited partnership is recognised", for_doc(tab_org, "doc04"),
       c("NameCore", "LegalForm"), \(.t) nrow(.t) > 0L,
       paste(dplyr::coalesce(for_doc(tab_org, "doc04")$LegalForm, "<null>"), collapse = " | "))

o05 <- for_doc(tab_org, "doc05")
chk_on("Two companies in one sentence give two spans", o05, c("Span", "Start"),
       \(.t) dplyr::n_distinct(.t$Span) >= 2L,
       paste(o05$Span, collapse = " | "))
chk_on("And they arrive in offset order", o05, "Start",
       \(.t) !is.unsorted(.t$Start),
       "ent_load_entity() arranges on DocID then Start; the party window depends on it")

# THE COLUMN BEING THERE IS NOT EVIDENCE. ANN_FIELDS records that the attribute names are
# company_type_abbr and company_type_label, not type_abbr and type_label -- and that getattr() with
# a default "cannot tell a field that is absent from a field that is empty", so the wrong names
# produced nulls in EVERY row while the columns existed and were typed.
chk("NameCore is populated on every organisation span",
    nrow(tab_org) > 0L && !any(is.na(tab_org$NameCore)),
    paste0(sum(is.na(tab_org$NameCore)), " null(s) of ", nrow(tab_org),
           " -- ent_apply()'s family rule reads this column"))
chk("LegalForm is populated on at least one span",
    nrow(tab_org) > 0L && any(!is.na(tab_org$LegalForm)),
    paste0(sum(!is.na(tab_org$LegalForm)), " of ", nrow(tab_org), " carry a form"))

cli::cli_h2("The counterparty key, which is what a unique counterparty is counted by")

# BUILT THE WAY 04D BUILDS IT, not approximated. apl_load_org() adds exactly these two columns and
# ent_rule(.key = "core") selects CoreKey.
tab_key <- tab_org |>
  dplyr::mutate(
    SpanKey = ent_norm_key(.x = .data$Span, .min = 1L),
    CoreKey = ent_norm_key(.x = dplyr::coalesce(.data$NameCore, .data$Span), .min = 1L)
  )

#' The distinct counterparty keys one document produced
#'
#' @param .doc DocID.
#' @return Character vector of keys, possibly empty.
keys_for <- function(.doc) {
  if (FALSE) .doc <- "doc18"
  sort(unique(dplyr::filter(tab_key, .data$DocID == .doc)$CoreKey))
}

# COVERAGE, NOT KEYING, AND MEASURED RATHER THAN ASSERTED. get_company_annotations() is an NLTK
# maxent NER and the legal-form suffix is what triggers it: every organisation this fixture produces
# carries one. So a counterparty written bare is not mis-keyed -- it is ABSENT, and every check on
# the spans that exist still passes.
#
# WHY THAT IS RECORDED AND NOT FAILED ON. It is a property of a third-party detector, not a defect
# this repository introduced or can fix, and a permanently red line teaches a reader to skip the
# block. What matters is that the property is stated with its consequence attached, so a reader of
# the counterparty count knows what it counts.
#
# THE PRACTICAL REACH IS PROBABLY SMALL AND IS NOT MEASURED HERE. Contracts name their parties
# formally in the preamble, with the suffix, and 04B1's window is +/- 2,000 characters around the
# anchor -- so the loss is later bare references rather than the parties themselves. "Probably" is
# doing work in that sentence and only the corpus can settle it.
o17 <- for_doc(tab_key, "doc17")
chk("The legal-form suffix is what makes a company detectable",
    nrow(tab_key) > 0L && all(!is.na(tab_key$LegalForm)),
    paste0(sum(!is.na(tab_key$LegalForm)), " of ", nrow(tab_key),
           " span(s) carry a form; a bare name yielded ", nrow(o17), " span(s)"))

chk("A bare company name is recorded either way", TRUE,
    if (nrow(o17) == 0L) {
      "NOT FOUND -- the counterparty count omits every name written without a legal form"
    } else {
      paste0("found: ", paste(o17$Span, collapse = " | "))
    })

# SPANS FIRST, THEN KEYS -- the same guard as doc19 below. "One distinct key" is trivially true on
# one span, so a collapse is only demonstrated where there were two things to collapse.
o18 <- for_doc(tab_key, "doc18")
k18 <- keys_for("doc18")
chk("Inc. and Corporation reduce to ONE key",
    nrow(o18) >= 2L && length(k18) == 1L,
    paste0(nrow(o18), " span(s) -> ", length(k18), " key(s): ", paste(k18, collapse = " | "),
           if (nrow(o18) < 2L) " -- only one span, so nothing was collapsed" else ""))

# WHAT THE SUFFIX DEPENDENCY COSTS, AND WHAT IT DOES NOT.
#
# doc19 names one company twice -- once with its form and once bare -- which is how a preamble
# introduces a party and then refers back to it. Only the suffixed mention is found. The first
# version of this check asked for two spans collapsing to one key and FAILED, correctly, because
# there was never a second span: doc17 had already established that a bare name is invisible.
#
# THE CONSEQUENCE IS NARROWER THAN IT LOOKS, and the distinction is worth having in front of anyone
# reading the counterparty count.
#
#   A BARE BACK-REFERENCE IS LOST AND COSTS NOTHING. The document still yields one span and one key
#   for this company, which is the right answer. Had the bare mention been found, ent_norm_key()
#   would have reduced it to the same key and the count would be identical.
#
#   A COMPANY NEVER WRITTEN WITH A FORM IS LOST AND COSTS EVERYTHING. It contributes no span at
#   all, so it is absent rather than mis-keyed, and no check over the spans that exist can see it.
#
# So the exposure is not repeated mentions -- it is counterparties whose name carries no legal form
# anywhere in the contract. That is a coverage question about the corpus and cannot be settled here.
o19 <- for_doc(tab_key, "doc19")
k19 <- keys_for("doc19")
chk("A bare back-reference is lost without changing the document's count",
    nrow(o19) >= 1L && length(k19) == 1L,
    paste0(nrow(o19), " span(s) -> ", length(k19), " key(s): ", paste(k19, collapse = " | "),
           " -- the suffixed mention alone gives the right answer here"))

# THE FALLBACK, TESTED DIRECTLY. CoreKey falls back to the raw Span when NameCore is null, and
# ent_norm_key() strips suffixes of its own -- so the fallback is safe exactly where the two
# vocabularies agree. .ent_suffix carries about 21 forms; company_types.csv carries 48.
tab_key <- tab_key |>
  dplyr::mutate(
    KeyFromCore = ent_norm_key(.x = .data$NameCore, .min = 1L),
    KeyAgrees   = dplyr::coalesce(.data$KeyFromCore == .data$SpanKey, FALSE)
  )

# LATENT, NOT LIVE, AND THE CORPUS HAS SETTLED IT. The fallback only fires where NameCore is null,
# and against 04C's lexnlp store:
#
#   SELECT COUNT(*), SUM(Name IS NULL) FROM org WHERE Start IS NOT NULL;
#   -> 20,837,808 spans, 0 null
#
# Not one of 04D's twenty million organisation spans reaches the fallback. The divergence between
# company_types.csv's 48 abbreviations and .ent_suffix's 30 is therefore documentation rather than
# a defect -- but it is documentation worth keeping, because it becomes live the moment a null
# NameCore appears.
chk("NameCore is populated, so the raw-span fallback never fires",
    nrow(tab_key) > 0L && !any(is.na(tab_key$NameCore)),
    paste0(sum(is.na(tab_key$NameCore)), " null(s) of ", nrow(tab_key),
           " -- the fallback is unreachable while this holds"))

chk("The two reductions agree wherever the fallback COULD fire",
    nrow(tab_key) > 0L && all(tab_key$KeyAgrees | !is.na(tab_key$NameCore)),
    paste0(sum(!tab_key$KeyAgrees), " of ", nrow(tab_key), " reductions differ, all on spans ",
           "whose NameCore is present; see the table below for which forms"))

if (any(!tab_key$KeyAgrees)) {
  cli::cli_alert_warning(
    "Forms LexNLP strips into NameCore that ent_norm_key() does not strip out of Span. \\
     company_types.csv carries 48 abbreviations; .ent_suffix carries {length(.ent_suffix)}."
  )
  tab_key |>
    dplyr::filter(!.data$KeyAgrees) |>
    dplyr::select("DocID", "Span", "NameCore", "LegalForm", "KeyFromCore", "SpanKey") |>
    print(n = Inf, width = Inf)
}

o20 <- for_doc(tab_key, "doc20")
chk_on("A two-letter name survives the reduction", o20, c("CoreKey", "Span"),
       \(.t) any(dplyr::coalesce(.t$CoreKey, "") == "CA"),
       paste0("CoreKey = ", paste(dplyr::coalesce(o20$CoreKey, "<null>"), collapse = " | "),
              " -- 04B1 sets .min_key = 2L for exactly this"))

# THE VOCABULARY GAP, MEASURED RATHER THAN ASSERTED. These two forms are in company_types.csv and
# not in .ent_suffix, so LexNLP strips them into NameCore and the R reduction does not strip them
# out of Span.
o21 <- for_doc(tab_key, "doc21")
chk("Forms LexNLP knows and .ent_suffix does not are reported either way", TRUE,
    if (nrow(o21) == 0L) {
      "PC and LLLP produced no span at all"
    } else {
      paste0(nrow(o21), " span(s); agree on key: ", sum(o21$KeyAgrees), " of ", nrow(o21),
             " -- ", paste(dplyr::coalesce(o21$CoreKey, "<null>"), collapse = " | "))
    })

cli::cli_h2("The two seam traps")

# WHICH PHRASINGS THIS GRAMMAR ACCEPTS, reported as a table rather than asserted as a pass. The
# first run of this file found LexNLP emitting NO money at all on two ordinary dollar figures, and
# the useful question is not whether that is a failure but which forms it does take -- 04B4 keeps
# both money families on the strength of "zero exact agreement", and absence is not disagreement.
tab_money_forms <- tibble::tribble(
  ~DocID,  ~Form,
  "doc07", "$9,752,233.001",
  "doc08", "$2,500,000",
  "doc13", "US$5,000,000",
  "doc14", "5,000,000 dollars",
  "doc15", "USD 5,000,000",
  "doc16", "Five Million Dollars"
) |>
  dplyr::mutate(
    NSpan  = purrr::map_int(.data$DocID, \(.d) nrow(for_doc(tab_money, .d))),
    Amount = purrr::map_chr(.data$DocID, \(.d) {
      got_ <- for_doc(tab_money, .d)
      if (nrow(got_) == 0L) "-" else paste(dplyr::coalesce(as.character(got_$Amount), "<null>"),
                                           collapse = ", ")
    })
  )

cli::cli_alert_info("Which money phrasings LexNLP accepts:")
print(tab_money_forms, n = Inf, width = Inf)

n_money_forms <- sum(tab_money_forms$NSpan > 0L)
chk("LexNLP's money grammar accepts at least one ordinary contract phrasing",
    n_money_forms > 0L,
    paste0(n_money_forms, " of ", nrow(tab_money_forms), " form(s) matched; 04B4 keeps both money ",
           "families on the strength of zero exact agreement, and absence is not disagreement"))

# THE PRECISION CHECK RUNS ONLY WHERE THERE IS SOMETHING TO CHECK. A Decimal becomes a float once
# pandas touches it and "a contract value of 9,752,233.001 does not survive that intact".
m07 <- for_doc(tab_money, "doc07")
if (nrow(m07) > 0L) {
  chk_on("A precise contract value keeps its three decimals across the seam", m07, "Amount",
         \(.t) any(stringi::stri_detect_fixed(dplyr::coalesce(as.character(.t$Amount), ""),
                                              "9752233.001")),
         paste0("Amount = ", paste(dplyr::coalesce(as.character(m07$Amount), "<null>"),
                                   collapse = " | ")))
} else {
  chk("The decimal-precision check had nothing to run on", TRUE,
      "no span for $9,752,233.001; the seam's Decimal handling is untested until one appears")
}

# NO VALUE MAY BE THE LITERAL "nan". Checked across every extras column of every entity, because
# the failure looks healthy: three characters, not null, indistinguishable from a country code.
nan_in_ <- function(.tab) {
  if (FALSE) .tab <- tab_gpe

  if (nrow(.tab) == 0L) return(0L)
  sum(purrr::map_int(.tab, \(.x) sum(as.character(.x) == "nan", na.rm = TRUE)))
}
n_nan_ <- nan_in_(tab_org) + nan_in_(tab_gpe) + nan_in_(tab_date) + nan_in_(tab_money)

chk("No column anywhere holds the literal string nan",
    nrow(tab_all) > 0L && n_nan_ == 0L,
    paste0(n_nan_, " occurrence(s); a pandas NaN stringified is not null in parquet, not null in ",
           "DuckDB, and looks exactly like a three-letter country code"))

cli::cli_h2("Dates, and the grammar matcon exists to avoid")

d09 <- for_doc(tab_date, "doc09")
chk_on("A fully written date is found", d09, c("DateValue", "Span"), \(.t) nrow(.t) > 0L,
       paste(d09$Span, collapse = " | "))
chk_on("DateValue is ten characters of ISO-8601", d09, "DateValue",
       \(.t) all(stringi::stri_detect_regex(dplyr::coalesce(as.character(.t$DateValue), ""),
                                            "^\\d{4}-\\d{2}-\\d{2}$")),
       paste(dplyr::coalesce(as.character(d09$DateValue), "<null>"), collapse = " | "))
chk_on("And it reads 2020-03-15", d09, "DateValue",
       \(.t) any(as.character(.t$DateValue) == "2020-03-15"),
       paste(dplyr::coalesce(as.character(d09$DateValue), "<null>"), collapse = " | "))

# RECORDED, NOT ASSERTED. matcon emits nothing for a date with no written year; LexNLP's grammar may
# supply one, and if it does the year comes from the clock. That is the property 04B3's RequireYear
# guard exists for and the reason matcon is the DATE family in 04D. Whichever way it comes out, the
# note carries the evidence.
d10 <- for_doc(tab_date, "doc10")
chk("A date with no written year is recorded either way",
    TRUE,
    if (nrow(d10) == 0L) {
      "LexNLP emitted nothing, as matcon does"
    } else {
      paste0("LexNLP SUPPLIED a year: ",
             paste(dplyr::coalesce(as.character(d10$DateValue), "<null>"), collapse = ", "),
             " -- this is what RequireYear guards against")
    })

cli::cli_h2("Geography, where Iso2 means something else")

g11 <- for_doc(tab_gpe, "doc11")
chk_on("A country is resolved", g11, c("GeoName", "Iso3"), \(.t) nrow(.t) > 0L,
       paste(dplyr::coalesce(g11$GeoName, g11$Span), collapse = " | "))
chk_on("lexnlp's Iso3 is a country code", g11, "Iso3",
       \(.t) any(dplyr::coalesce(.t$Iso3, "") == "DEU"),
       paste(dplyr::coalesce(g11$Iso3, "<null>"), collapse = " | "))
chk_on("And its Iso2 is a COUNTRY code, where matcon's is a subdivision code", g11, "Iso2",
       \(.t) any(dplyr::coalesce(.t$Iso2, "") == "DE"),
       "04B2 records that reading the two as one quantity gave the right answer by luck")

cli::cli_h2("The output fingerprint, for the freeze")

# THE NUMBER THAT MUST SURVIVE THE REBUILD. Sorted and reduced to the columns that carry meaning, so
# row order and file-level metadata cannot move it. The image's own spec hash WILL move when
# requirements.lock.txt joins SOURCES; this one must not.
tab_print <- tab_all |>
  dplyr::arrange(.data$DocID, .data$Start, .data$Stop, .data$Span) |>
  as.data.frame()

hash_out <- substr(digest::digest(tab_print, algo = "sha256"), 1L, 12L)

chk("The fixture produced spans to fingerprint", nrow(tab_all) > 0L,
    paste0(nrow(tab_all), " span(s) over ", dplyr::n_distinct(tab_all$DocID), " document(s)"))

cli::cli_alert_info(
  "SPAN HASH {(hash_out)} over {nrow(tab_all)} span{?s}. Image spec hash \\
   {(tab_image$SpecHash[[1L]])}."
)
cli::cli_alert_info(
  "Keep both. After the Dockerfile is pinned and requirements.lock.txt joins image_spec.SOURCES, \\
   the IMAGE hash must move and the SPAN hash must not. A moved span hash means the rebuild \\
   changed what the corpus would say."
)


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
    "{nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} passed. Span hash {(hash_out)} is \\
     the baseline the Docker freeze is measured against."
  )
} else {
  cli::cli_alert_danger(
    "{n_fail} of {nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} failed. Do NOT rebuild \\
     the image until this is understood -- a baseline taken from a broken run measures nothing."
  )
}


# 7. What was written, for inspection ----

arrow::write_parquet(tab_docs, fs::path(.dir_out, "01_fixture.parquet"))
if (nrow(tab_org) > 0L)   arrow::write_parquet(tab_org,   fs::path(.dir_out, "02_org.parquet"))
if (nrow(tab_gpe) > 0L)   arrow::write_parquet(tab_gpe,   fs::path(.dir_out, "03_gpe.parquet"))
if (nrow(tab_date) > 0L)  arrow::write_parquet(tab_date,  fs::path(.dir_out, "04_date.parquet"))
if (nrow(tab_money) > 0L) arrow::write_parquet(tab_money, fs::path(.dir_out, "05_money.parquet"))
readr::write_csv(tab_checks, fs::path(.dir_out, "06_checks.csv"))

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
  "02_org.parquet is the one to read first: NameCore and LegalForm are what 04B1's party chain and \\
   ent_apply()'s corporate-family rule are built on."
)
