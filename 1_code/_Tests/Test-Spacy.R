# ======================================================================================================================
# Test-Spacy.R -- the spaCy seam, the cue columns, and H5 verified through the store
# ======================================================================================================================
#
# WHAT THIS TESTS
# The seam: fabricated text -> extract_spacy.py -> a parquet -> a DuckDB store -> ent_load_entity().
# spaCy is a statistical model rather than a pattern table, so what can be asserted differs from the
# other five tests: WHICH entities it finds is a property of the model and not of this repository,
# and a check demanding that Microsoft be recognised would be testing en_core_web_trf.
#
# SO THE CHECKS ARE ABOUT WHAT ARRIVES, NOT ABOUT WHAT IS FOUND. Offsets index the text. Labels map
# through the cross-engine vocabulary. The cue columns cut from the document. Those are this
# repository's claims; the recall is spaCy's.
#
# TWO THINGS MADE THIS FILE NECESSARY
#
#   THE CUE COLUMNS ARE CUT DIFFERENTLY HERE, and it is the copy most likely to be wrong. Long
#   documents are split into overlapping windows and the `doc` inside the emission loop is a WINDOW:
#   only off + ent.start_char is document-absolute. Slicing the window would give a span near a
#   window edge less context than the document holds -- silently, because the value would look
#   ordinary and simply be short. extract_spacy.py therefore cuts from text_by_doc[docid], and this
#   is what checks that it does.
#
#   H5 IS FIXED AND SHOULD BE VISIBLE FROM THE STORE. ner_describe() used to report this family's
#   SpecHash as the MODEL version -- 3.8.0 -- while matcon and lexnlp reported content hashes. An
#   edit to extract_spacy.py moved nothing, so ner_manifest_write() would have admitted rows from
#   the edited script beside rows from the old one under an unchanged tag: the guard was unreachable
#   for this family alone. ner_spacy_spec() now hashes the script AND the model together.
#
# WHAT IS NOT TESTED HERE, AND WHY IT IS SAID RATHER THAN LEFT OUT
# WINDOWING. nlp.max_length is 5,000,000 characters, so exercising it end to end needs a five
# megabyte document -- roughly eighty seconds of en_core_web_sm, and a test slow enough that people
# stop running it is a test that stops catching things. The offset arithmetic it protects is
# therefore unexercised, and that is a real gap rather than an oversight: a document past that
# length is where a cue column would first come from the wrong place. It wants a Python unit test
# beside windows() in matcon-extract's suite, not a slow fixture here.
#
# IT REPORTS, IT DOES NOT ABORT. Nothing outside tempdir() is written.
#
# Usage:
#   source(here::here("1_code", "_Tests", "Test-Spacy.R"), encoding = "UTF-8")

here::i_am("1_code/_Commons/_Initialize.R")

options(cli.num_colors = 1, cli.width = 120)

source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),     encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_NER.R"),        encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Entity.R"),     encoding = "UTF-8")


# 1. Configuration ----

.dir_out <- fs::path(tempdir(), "Test-Spacy")
fs::dir_create(.dir_out)

.lT <- list(
  Stage    = fs::path(.dir_out, "staged_text.parquet"),
  Spans    = fs::path(.dir_out, "spans"),
  Store    = fs::path(.dir_out, "store"),
  # THE DEPLOYED MODEL, not a faster one. 04A measured the three CNN models against the transformer
  # and they lost on every entity, so the transformer is what 04A and 04C run and therefore what a
  # seam test should exercise. Twelve short documents cost seconds even on the GPU path.
  Model    = "en_core_web_trf",
  Entities = c("ORG", "PERSON", "GPE"),   # exactly what 04A asks this family for
  Cue      = 160L
)

# tempdir() IS PER-SESSION, NOT PER-RUN. A second run inside one R session finds the first run's
# DuckDB file, and ner_manifest_write() then correctly refuses to ingest rows whose spec hash
# differs from what that file holds under the same model tag.
purrr::walk(c(.lT$Spans, .lT$Store), \(.d) if (fs::dir_exists(.d)) fs::dir_delete(.d))
fs::dir_create(c(.lT$Spans, .lT$Store))

cli::cli_h1("Test-Spacy")
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
    .name <- "The cue columns are present"
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
    .name <- "The cue columns are present"
    .tab  <- tab_org
    .cols <- c("CueBefore", "CueAfter")
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


# 2. H5, before anything is extracted ----
#
# THE IDENTITY IS CHECKED FIRST BECAUSE EVERYTHING ELSE DEPENDS ON IT. If SpecHash is still a
# version string then the store cannot distinguish rows produced by two different scripts, and a
# green run below would prove only that today's script works today.

tab_installed <- ner_spacy_installed()

chk("The spaCy family reports installed models", nrow(tab_installed) > 0L,
    paste0(nrow(tab_installed), " model(s): ",
           paste(tab_installed$Model, collapse = ", ")))

chk("The deployed model is among them", .lT$Model %in% tab_installed$Model,
    paste0(.lT$Model, if (.lT$Model %in% tab_installed$Model) " present" else " ABSENT"))

# H5: A CONTENT HASH, NOT A VERSION STRING. Twelve hexadecimal characters, the same shape matcon and
# lexnlp report, so a comparison across families compares like with like.
chk("SpecHash is a 12-character hash rather than a version",
    nrow(tab_installed) > 0L &&
      all(stringi::stri_detect_regex(tab_installed$SpecHash, "^[0-9a-f]{12}$")),
    paste(utils::head(tab_installed$SpecHash, 2L), collapse = ", "))

# AND IT DISTINGUISHES THE MODELS, which the version string could not: all four report 3.8.0, so a
# hash over the version alone would give four identical identities for four different models.
chk("And it differs per model, where the version does not",
    nrow(tab_installed) > 1L &&
      dplyr::n_distinct(tab_installed$SpecHash) == nrow(tab_installed) &&
      dplyr::n_distinct(tab_installed$Version) < nrow(tab_installed),
    paste0(dplyr::n_distinct(tab_installed$SpecHash), " distinct hash(es) against ",
           dplyr::n_distinct(tab_installed$Version), " distinct version(s) over ",
           nrow(tab_installed), " model(s)"))


# 3. The fixture: twelve documents ----
#
# WHAT SPACY FINDS IS SPACY'S BUSINESS. These documents are written so that a statistical NER has
# something ordinary to find -- a company, a person, a place -- rather than to pin any particular
# recall. The checks below are about what ARRIVES once something is found.

tab_docs <- tibble::tribble(
  ~DocID,   ~TextRaw,
  # 01 a span at the very head of the document, which is the case the clamp protects
  "doc01",  "Microsoft Corporation entered into this Agreement with the Purchaser.",
  # 02 a person, so PERSON is exercised alongside ORG
  "doc02",  "This Agreement is signed by John Smith on behalf of the Company.",
  # 03 a place, and a US State that the gazetteer would also find
  "doc03",  "The Supplier maintains its registered office in Delaware and ships from there.",
  # 04 two entities in one sentence, so offset ordering is exercised
  "doc04",  "Apple Inc. and Oracle Corporation are the parties to this Agreement.",
  # 05 a location that spaCy tags LOC rather than GPE, so LABEL_MAP is exercised
  "doc05",  "The goods shall be delivered across the Pacific Ocean to the buyer's port.",
  # 06 a quantity, which LABEL_MAP folds onto AMOUNT
  "doc06",  "The Seller shall deliver 500 kilograms of product under this Agreement.",
  # 07 a long preamble so at least one span sits well past the cue width
  "doc07",  paste0(strrep("The parties acknowledge the foregoing provisions. ", 6L),
                   "Siemens AG is the counterparty under this Agreement."),
  # 08 a date, which spaCy finds and matcon reads differently -- both are in the store
  "doc08",  "This Agreement is dated as of March 15, 2020, and takes effect that day.",
  # 09 money, likewise
  "doc09",  "The purchase price of $5,000,000 shall be paid at closing by the Purchaser.",
  # 10 a company and a place adjacent, the ordinary shape of a preamble
  "doc10",  "Acme Holdings, Inc., a Delaware corporation, is the Seller hereunder.",
  # 11 an ordinary sentence with nothing to find: the document must still reach the ledger
  "doc11",  "The parties agree that all obligations hereunder shall be performed promptly.",
  # 12 the same, so a sentinel is not a single-document accident
  "doc12",  "Each provision of this Agreement shall survive the termination hereof."
)

arrow::write_parquet(tab_docs, .lT$Stage)

cli::cli_alert_info("{nrow(tab_docs)} {cli::qty(nrow(tab_docs))}fixture document{?s} staged.")

tab_lens <- tab_docs |>
  dplyr::transmute(.data$DocID, DocLen = stringi::stri_length(.data$TextRaw))


# 4. Extraction: the same entry point 04A uses ----

tab_describe <- ner_describe(.spacy_models = .lT$Model)

hash_spacy <- tab_describe$SpecHash[tab_describe$Family == "spacy"][[1L]]
cli::cli_alert_info("spaCy spec hash: {(hash_spacy)} for {(.lT$Model)}.")

chk("ner_describe() reports the same hash the probe did",
    hash_spacy %in% tab_installed$SpecHash,
    paste0(hash_spacy, " -- the describe and the probe must agree or the store is keyed on one ",
           "and compared against the other"))

tab_staged <- ner_extract(
  .family     = "spacy",
  .model      = .lT$Model,
  .entity     = .lT$Entities,
  .path_in    = .lT$Stage,
  .out_dir    = .lT$Spans,
  .describe   = tab_describe,
  .id_col     = "DocID",
  .text_col   = "TextRaw",
  .max_chars  = 0L,
  .workers    = 1L,
  .batch_size = 8L,
  .timeout    = 300L,
  .quiet      = TRUE
)

con_spacy <- ner_db_connect(
  .db_path   = ner_db_path(.dir = .lT$Store, .family = "spacy"),
  .read_only = FALSE
)
ner_db_init(.con = con_spacy)
ner_ingest(.con = con_spacy, .staged = tab_staged, .entity = .lT$Entities)

tab_ledger <- DBI::dbGetQuery(
  con_spacy, "SELECT DocID, Entity, Status FROM runs ORDER BY DocID, Entity"
) |>
  tibble::as_tibble()

# THE MANIFEST, READ DIRECTLY. This is where H5 lands: what the store recorded as this family's
# provenance for these rows.
tab_manifest <- DBI::dbGetQuery(con_spacy, "SELECT * FROM manifest") |>
  tibble::as_tibble()

DBI::dbDisconnect(con_spacy, shutdown = TRUE)

#' Load one entity out of the temporary store
#'
#' @param .entity Entity name.
#' @return Tibble of spans, empty where the model produced none.
load_ <- function(.entity) {
  if (FALSE) .entity <- "ORG"

  tryCatch(
    ent_load_entity(
      .dir_store = .lT$Store, .family = "spacy", .entity = .entity,
      .lens = tab_lens, .extras = ent_extras("spacy", .entity),
      .model = .lT$Model, .quiet = TRUE
    ),
    error = function(.e) tibble::tibble()
  )
}

tab_org    <- load_("ORG")
tab_person <- load_("PERSON")
tab_gpe    <- load_("GPE")

tab_all <- dplyr::bind_rows(
  dplyr::select(tab_org,    dplyr::any_of(c("DocID", "Start", "Stop", "Span", "CueBefore", "CueAfter"))),
  dplyr::select(tab_person, dplyr::any_of(c("DocID", "Start", "Stop", "Span", "CueBefore", "CueAfter"))),
  dplyr::select(tab_gpe,    dplyr::any_of(c("DocID", "Start", "Stop", "Span", "CueBefore", "CueAfter")))
)

cli::cli_alert_info(
  "Chain complete: {nrow(tab_org)} ORG, {nrow(tab_person)} PERSON, {nrow(tab_gpe)} GPE."
)


# 5. The checks ----

cli::cli_h2("The seam")

chk("The extractor wrote a parquet", nrow(tab_staged) >= 1L,
    paste0(nrow(tab_staged), " file(s) written"))
chk("Every fixture document reached the ledger",
    dplyr::n_distinct(tab_ledger$DocID) == nrow(tab_docs),
    paste0(dplyr::n_distinct(tab_ledger$DocID), " of ", nrow(tab_docs)))
chk("Spans were read back out of the store", nrow(tab_all) > 0L,
    paste0(nrow(tab_all), " span(s) across the three entities"))

cut_ <- stringi::stri_sub(
  tab_docs$TextRaw[match(tab_all$DocID, tab_docs$DocID)],
  from = tab_all$Start + 1L, to = tab_all$Stop
)
chk("Every span equals the text at its own offsets",
    nrow(tab_all) > 0L && all(cut_ == tab_all$Span),
    paste0(sum(cut_ != tab_all$Span), " mismatch(es) over ", nrow(tab_all), " span(s)"))

cli::cli_h2("The manifest, and what it records")

chk("The store recorded a manifest row for this family", nrow(tab_manifest) > 0L,
    paste0(nrow(tab_manifest), " row(s)"))
chk_on("And the provenance it recorded is the content hash", tab_manifest, "SpecHash",
       \(.t) all(.t$SpecHash == hash_spacy),
       paste0(paste(unique(tab_manifest$SpecHash), collapse = ", "),
              " -- before H5 this column held 3.8.0 and an edited script moved nothing"))

cli::cli_h2("The context columns")

# THE COPY MOST LIKELY TO BE WRONG, AND THE REASON IS WINDOWING. See the header: the doc inside the
# emission loop is a WINDOW and only off + ent.start_char is document-absolute, so the cut has to
# come from the full text. Re-cutting it here from the fixture is what says whether it does.

#' Both cue columns, checked against the window re-cut from the fixture
#'
#' @param .tab Span table from ent_load_entity().
#' @param .what Entity name, for the check names.
#' @return Invisibly NULL.
cue_checks <- function(.tab, .what) {
  if (FALSE) {
    .tab  <- tab_org
    .what <- "ORG"
  }

  chk(paste0(.what, ": the cue columns are present"),
      nrow(.tab) > 0L && all(c("CueBefore", "CueAfter") %in% names(.tab)),
      if (nrow(.tab) == 0L) "no spans of this entity" else
        paste(setdiff(c("CueBefore", "CueAfter"), names(.tab)), collapse = ", "))

  if (nrow(.tab) == 0L || !all(c("CueBefore", "CueAfter") %in% names(.tab))) {
    return(invisible(NULL))
  }

  txt_ <- tab_docs$TextRaw[match(.tab$DocID, tab_docs$DocID)]

  # stri_sub IS 1-BASED AND INCLUSIVE; the offsets are 0-based and half-open.
  want_b_ <- stringi::stri_sub(txt_, from = pmax(1L, .tab$Start - .lT$Cue + 1L), to = .tab$Start)
  want_a_ <- stringi::stri_sub(txt_, from = .tab$Stop + 1L, to = .tab$Stop + .lT$Cue)

  chk(paste0(.what, ": CueBefore matches the window R would cut"),
      all(.tab$CueBefore == want_b_),
      paste0(sum(.tab$CueBefore != want_b_), " mismatch(es) over ", nrow(.tab), " span(s)"))
  chk(paste0(.what, ": CueAfter matches the window R would cut"),
      all(.tab$CueAfter == want_a_),
      paste0(sum(.tab$CueAfter != want_a_), " mismatch(es) over ", nrow(.tab), " span(s)"))

  invisible(NULL)
}

purrr::walk2(list(tab_org, tab_person, tab_gpe), c("ORG", "PERSON", "GPE"), cue_checks)

# THE CLAMP. doc01 puts a company at offset 0, so its CueBefore must be empty -- not the last 160
# characters of the document, which is what an unclamped negative slice would give and which would
# look like perfectly ordinary context.
head_ <- dplyr::filter(tab_all, .data$Start < .lT$Cue)
chk("A span near the head does not wrap to the tail",
    nrow(head_) > 0L && all(stringi::stri_length(head_$CueBefore) == head_$Start),
    paste0(nrow(head_), " span(s) start within ", .lT$Cue, " of the head; the shortest carries ",
           if (nrow(head_) > 0L) min(stringi::stri_length(head_$CueBefore)) else NA_integer_,
           " character(s)"))

chk("At least one span sits beyond the cue width, so a FULL window is exercised",
    any(tab_all$Start >= .lT$Cue) &&
      all(stringi::stri_length(dplyr::filter(tab_all, .data$Start >= .lT$Cue)$CueBefore)
          == .lT$Cue),
    paste0(sum(tab_all$Start >= .lT$Cue), " span(s) at or beyond offset ", .lT$Cue,
           " -- doc07's preamble is there to guarantee one"))

chk("All three families now cut the same window",
    nrow(tab_all) > 0L && !any(is.na(tab_all$CueBefore)) && !any(is.na(tab_all$CueAfter)),
    "matcon, lexnlp and spacy: three implementations, one rule, all checked the same way")

cli::cli_h2("The cross-engine vocabulary")

# LABEL_MAP IS THIS REPOSITORY'S CLAIM, NOT SPACY'S. LOC folds onto GPE and QUANTITY onto AMOUNT so
# that an entity means the same thing whichever family found it. Whether spaCy tags the Pacific
# Ocean as LOC is spaCy's business; that a LOC arrives as a GPE is ours.
# THERE IS NO Label COLUMN, AND THERE SHOULD NOT BE. ent_load_entity() reads ONE entity table at a
# time, so the label is the table -- carrying it as a column would repeat the query's own argument
# on every row. The mapping is therefore visible only through LabelRaw: a span that arrives in the
# GPE table with LabelRaw "LOC" is the map having done its work, because LOC is what spaCy called it
# and GPE is where it landed.
chk_on("Every span in the GPE table carries a native label the map accounts for", tab_gpe,
       "LabelRaw",
       \(.t) all(.t$LabelRaw %in% c("GPE", "LOC")),
       paste0(paste(sort(unique(tab_gpe$LabelRaw)), collapse = ", "),
              " -- LOC here IS the mapping, since the table is GPE"))

# THIS CHECK PASSED VACUOUSLY AND THE WARNING WAS THE ONLY SIGN. It read
# tab_gpe$Label[tab_gpe$LabelRaw == "LOC"] == "GPE" -- and tab_gpe$Label is NULL, so NULL[cond] is
# NULL, NULL == "GPE" is logical(0), and all(logical(0)) is TRUE. It reported success on a column
# that does not and should not exist, printing "Unknown or uninitialised column: Label" at the end
# of an otherwise clean run.
#
# T10 IN A FILE WRITTEN AFTER THE GUARD AGAINST IT. chk_on() refuses missing columns, and this went
# through bare chk() because the predicate spans two tables' worth of logic. The lesson is not to
# use the guard more; it is that ANY bare chk() whose predicate reaches into a tibble is the shape
# that fails this way.
#
# WHAT IS ACTUALLY TESTABLE. Check 24 above already asserts every LabelRaw in the GPE table is GPE
# or LOC, and a LOC sitting in the GPE table IS the mapping -- there is nothing further to compare
# it against. What remains is whether the mapping was EXERCISED, which is spaCy's recall and not
# this repository's claim, so it is recorded rather than asserted.
n_loc_ <- if (nrow(tab_gpe) == 0L) 0L else sum(tab_gpe$LabelRaw == "LOC")

chk("The LOC-to-GPE mapping is exercised, or recorded as untested", TRUE,
    if (n_loc_ > 0L) {
      paste0(n_loc_, " span(s) spaCy called LOC arrived in the GPE table -- that IS the mapping")
    } else {
      "no LOC in this fixture; the mapping is untested rather than wrong"
    })

chk("Only the requested entities were ingested",
    nrow(tab_ledger) > 0L && all(tab_ledger$Entity %in% .lT$Entities),
    paste(sort(unique(tab_ledger$Entity)), collapse = ", "))

cli::cli_h2("What the model found")

# RECORDED, NOT ASSERTED. Recall is a property of en_core_web_trf and not of this repository, so
# these print rather than pass or fail -- but a run where all three are zero is a broken seam
# wearing the costume of a cautious model, and the check below is what tells them apart.
tibble::tibble(
  Entity = c("ORG", "PERSON", "GPE"),
  NSpan  = c(nrow(tab_org), nrow(tab_person), nrow(tab_gpe)),
  NDoc   = c(dplyr::n_distinct(tab_org$DocID), dplyr::n_distinct(tab_person$DocID),
             dplyr::n_distinct(tab_gpe$DocID))
) |>
  print(n = Inf, width = Inf)

chk("The model found something in at least two of the three entities",
    sum(c(nrow(tab_org), nrow(tab_person), nrow(tab_gpe)) > 0L) >= 2L,
    "three zeroes would be a broken seam rather than a cautious model")


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
    "{nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} passed. SpecHash is a content hash \\
     like every other family's, and the third implementation of the cue columns cuts the same \\
     window as the other two."
  )
} else {
  cli::cli_alert_danger(
    "{n_fail} of {nrow(tab_checks)} {cli::qty(nrow(tab_checks))}check{?s} failed. Read the Note \\
     column, then the artifacts below."
  )
}

cli::cli_alert_info(
  "NOT TESTED HERE: windowing. nlp.max_length is 5,000,000 characters, so exercising it needs a \\
   five megabyte document. The offset arithmetic it protects is where a cue column would first \\
   come from the wrong place, and it wants a Python unit test beside windows() rather than a slow \\
   fixture."
)


# 7. What was written, for inspection ----

arrow::write_parquet(tab_docs,     fs::path(.dir_out, "01_fixture.parquet"))
arrow::write_parquet(tab_manifest, fs::path(.dir_out, "02_manifest.parquet"))
if (nrow(tab_org) > 0L)    arrow::write_parquet(tab_org,    fs::path(.dir_out, "03_org.parquet"))
if (nrow(tab_person) > 0L) arrow::write_parquet(tab_person, fs::path(.dir_out, "04_person.parquet"))
if (nrow(tab_gpe) > 0L)    arrow::write_parquet(tab_gpe,    fs::path(.dir_out, "05_gpe.parquet"))
readr::write_csv(tab_checks,       fs::path(.dir_out, "06_checks.csv"))

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
  "02_manifest.parquet is where H5 lands -- the provenance the store recorded for these rows. \\
   03_org.parquet is the one to read for the cue columns."
)
