# ======================================================================================================================
# 04D-Preflight.R -- can the 04B rule chain carry 1.19 million documents?
# ======================================================================================================================
#
# WRITES ONLY TO tempdir(), AND REMOVES WHAT IT WROTE. Every store is opened read-only. No artifact
# under 2_output/ is created, moved or touched. The temporary write is not incidental: staging a
# chunk of text to a parquet is the mechanism 04D will use, so a probe that avoided it would not be
# testing the thing being proposed.
#
# WHY THIS EXISTS
# 04D applies the rules 04B settled to the corpus 04C extracted. It cannot load the corpus at once,
# so it must chunk -- and four properties have to hold before chunking is sound. None is visible by
# reading the code, and each invalidates the pair rather than degrades it.
#
#   Q1  DOES THE CHUNK MECHANISM WORK AT ALL?
#       04B reads text from ONE parquet holding DocID and TextRaw. No such file exists for the
#       corpus and none should. The corpus keeps DocID -> Path in each store's `corpus` table and
#       materialises text per chunk, which is what 04C already does for the Python engines. So
#       .path_text stays as it is and points at a per-chunk stage instead.
#
#   Q2  IS THE DOCUMENT LENGTH ALREADY STORED?
#       ent_doc_lens() reads every document's text to compute stri_length(TextRaw). The register
#       carries nChars for all 1.77 million rows. If the two agree, the lens is free and two of the
#       five chains stop needing text at all. 04B5 found the register's WORD count within one per
#       cent for only 47% of documents, so agreement is measured here rather than assumed -- DocLen
#       is the denominator of every relative position and a silent 3% shift moves every window rule.
#
#   Q3  IS EACH CHAIN LINEAR IN DOCUMENTS?
#       A chain that is superlinear at 10x never finishes at 270x.
#
#   Q4  IS ANY RULE PARAMETER DERIVED FROM THE SET BEING PROCESSED?
#       ent_apply() computes ent_token_freq() from the spans it is handed, and ent_rule()'s .fam_df
#       compares against that share. Under chunking the denominator becomes the chunk, so a leading
#       token opening 0.4% of one chunk and 0.6% of another is admitted in one and refused in the
#       other, on identical text. Same shape as cor_pending() taking its completeness target from the
#       ledger it was querying: A TARGET MUST NOT BE DERIVED FROM THE DATA BEING TESTED.
#
# WHAT THE FIRST RUN OF THIS PROBE ESTABLISHED, AND WHY THE SHAPE CHANGED
# ent_anchor_keys() reads the labelled spine from 03A -- DocID, ClassDetailed, AmendType, Fold --
# and filters the register to those 4,398 DocIDs. It is structurally sample-only, so the first run
# timed five chains against keys matching 0.4% of the staged documents: the chains were not slow,
# they were doing almost no matching work, and every number was a fiction. Section 3 now builds
# corpus-scale keys so the timings measure the work 04D will actually do.
#
# Usage:
#   source(here::here("1_code", "_Tests", "04D-Preflight.R"), encoding = "UTF-8")

here::i_am("1_code/_Commons/_Initialize.R")

source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),     encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_NER.R"),        encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Entity.R"),     encoding = "UTF-8")

# 03A CARRIES clf_read_text(), WHICH IS THE ONE TEXT READER. 04C sources it for the same reason and
# sends it into its daemons rather than reimplementing it, which is what keeps one definition of
# "the text of a document" across R, Python and both stores.
source(here::here("1_code", "03A-ClassifyPrepare.R"), encoding = "UTF-8")
source(here::here("1_code", "04C-EntityCorpus.R"),    encoding = "UTF-8")

source(here::here("1_code", "04B1-Rules-ORG.R"),    encoding = "UTF-8")
source(here::here("1_code", "04B2-Rules-GPE.R"),    encoding = "UTF-8")
source(here::here("1_code", "04B3-Rules-DATE.R"),   encoding = "UTF-8")
source(here::here("1_code", "04B4-Rules-MONEY.R"),  encoding = "UTF-8")
source(here::here("1_code", "04B5-Rules-REDACT.R"), encoding = "UTF-8")

options(cli.num_colors = 1, cli.width = 120, mc.table_mode = "console")


# 0. Configuration ---------------------------------------------------------------------------------------------------
#
# THERE IS NO CORPUS TEXT PARQUET AND THERE IS NO ENTRY FOR ONE. The corpus text input is the
# `corpus` table inside each family store, holding DocID, Path and the resolved Extract flag.
#
# THE LADDER IS SMALLER THAN THE FIRST VERSION'S AND THE TEXT IS STAGED ONCE. Every chain takes
# .lens as its document filter, so a smaller lens over the same stage is a smaller run. Staging per
# ladder point read 145,000 documents to time three sizes; staging once reads 32,000 and makes the
# three points nested draws of one text set rather than three independent ones, which is a better
# scaling test as well as a faster one.

.pP <- list(
  Store  = list(
    Corpus = fs::path(
      init_create_script_dir(.dir_here = here::here(), .name_script = "04C-EntityCorpus"), "Output"
    )
  ),
  Text   = list(
    Sample = fs::path(
      init_create_script_dir(.dir_here = here::here(), .name_script = "04A-EntityExtract"),
      "Output", "sample_text.parquet"
    )
  ),
  Input  = list(
    Prepared = fs::path(
      init_create_script_dir(.dir_here = here::here(), .name_script = "03A-ClassifyPrepare"),
      "prepared.parquet"
    ),
    Register = fs::path(
      init_create_script_dir(.dir_here = here::here(), .name_script = "02B-Register"),
      "Output", "Documents.parquet"
    ),
    # THE GAZETTEER SITS INSIDE THE PYTHON PACKAGE, past the CLI seam. 04B2 reads it directly and
    # says why: gazetteer-v2's spec hash covers a content hash of this file, so a read that bypasses
    # the CLI is still covered by the manifest.
    Lookup   = here::here("contracts-extract", "src", "matcon_extract", "data", "geo_lookup.parquet"),
    # 03F CLASSIFIED THE CORPUS, AND THE RULES NEED THAT LABEL. mny_aggregate() and red_counts()
    # both select Class and AmendType off the keys, which in 04B came from 03A's labelled spine. The
    # corpus has no such spine and does not need one: 03F applied the deployed model to all of it.
    # Glob rather than name, because the file carries the length in its stem and _partial where a
    # pass was still running.
    Release  = fs::path(
      init_create_script_dir(.dir_here = here::here(), .name_script = "03F-ClassifyApply"), "release"
    )
  ),
  Sizes   = c(2000L, 8000L, 32000L),  # nested; only the largest is ever read
  Chunks  = c(12L, 60L, 120L),        # partition counts the drift section evaluates
  Workers = 12L,                      # daemons for the chunk read; the corpus pass used 24
  Family  = "matcon",                 # the store whose corpus table supplies DocID and Path
  Seed    = 42L
)

# The stage is a real file because the loaders take a path. It lives in tempdir() and is removed in
# the last section, so an interrupted probe leaves nothing under 2_output/ either.
.pP$Stage <- fs::path(tempdir(), "04D-preflight-stage.parquet")


#' Time one expression and return its elapsed seconds with the result
#'
#' Report functions print and compute functions do not, and a timer has to do both -- so it returns
#' the value, prints nothing, and the caller decides what to say about it.
#'
#' @param .expr Expression to evaluate.
#' @return List: Value (whatever .expr returned), Secs (elapsed, double), Rows (nrow where the value
#'   is a data frame, otherwise NA_integer_).
prb_time <- function(.expr) {
  if (FALSE) .expr <- nrow(tab_index)

  t0_  <- Sys.time()
  val_ <- force(.expr)
  sec_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))

  list(
    Value = val_,
    Secs  = sec_,
    Rows  = if (is.data.frame(val_)) nrow(val_) else NA_integer_
  )
}


#' The corpus document index, read from a family store's corpus table
#'
#' THE POPULATION IS RESOLVED, NOT RE-DERIVED. cor_db_init() writes Extract as the deduplication and
#' population filter already applied, so this is a filter rather than a second statement about what
#' is in the corpus -- which is the whole reason that column is stored.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .family Family whose corpus table is read; all of them carry the same index.
#' @return Tibble: DocID, Path.
prb_corpus_index <- function(.dir_store, .family = "matcon") {
  if (FALSE) {
    .dir_store <- .pP$Store$Corpus
    .family    <- "matcon"
  }

  path_ <- ner_db_path(.dir = .dir_store, .family = .family)
  if (!fs::file_exists(path_)) {
    cli::cli_abort(c(
      "No {(.family)} corpus store at {.path {(path_)}}.",
      "i" = "04C writes it. Without it there is no corpus to probe."
    ))
  }

  con_ <- ner_db_connect(.db_path = path_, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  if (!"corpus" %in% DBI::dbListTables(con_)) {
    cli::cli_abort("The {(.family)} store holds no corpus table; 04C did not complete its load.")
  }

  DBI::dbGetQuery(con_, "SELECT DocID, Path FROM corpus WHERE Extract ORDER BY DocID") |>
    tibble::as_tibble()
}


#' Stage one chunk of corpus text as the parquet the 04B loaders expect
#'
#' THIS IS 04D'S CHUNK MECHANISM, tested rather than described. 04C stages the same shape for the
#' Python engines, and the column is TextRaw there for the same reason it is TextRaw here: one name
#' for one thing, across both languages and both stores.
#'
#' Documents whose file cannot be read are dropped rather than staged empty. That matches 04C, which
#' marks them error in the ledger, and it is why the returned row count can fall short of the draw.
#'
#' @param .docs Tibble carrying DocID and Path.
#' @param .path_stage Where to write the staged parquet.
#' @param .workers Daemons for the read; the caller must have started them.
#' @return Tibble: DocID, TextRaw, for the documents that read.
prb_stage_text <- function(.docs, .path_stage, .workers = 1L) {
  if (FALSE) {
    .docs       <- dplyr::slice_head(tab_index, n = 100L)
    .path_stage <- .pP$Stage
    .workers    <- 1L
  }

  txt_ <- cor_read_text(.docs = .docs, .workers = .workers) |>
    dplyr::filter(!is.na(.data$Text), nzchar(trimws(.data$Text))) |>
    dplyr::transmute(.data$DocID, TextRaw = .data$Text)

  arrow::write_parquet(txt_, .path_stage)
  txt_
}


#' A reproducible document subset of a given size
#'
#' NESTED BY CONSTRUCTION. The draw is a head of one seeded shuffle, so the 2,000 are inside the
#' 8,000 are inside the 32,000. Three independent draws would confound a scaling ratio with
#' whatever differed between three sets of documents.
#'
#' @param .index Tibble from prb_corpus_index().
#' @param .n Integer. Documents wanted; the whole table where it holds fewer.
#' @param .seed Integer. Fixed, so the nesting holds across calls.
#' @return .index restricted to .n rows.
prb_draw <- function(.index, .n, .seed = 42L) {
  if (FALSE) {
    .index <- tab_index
    .n     <- 2000L
    .seed  <- 42L
  }

  if (nrow(.index) <= .n) return(.index)
  withr::with_seed(.seed, dplyr::slice_sample(.index, n = nrow(.index))) |>
    dplyr::slice_head(n = .n)
}


# 1. What is actually on disk ----------------------------------------------------------------------------------------

cli::cli_h1("1. The inputs this probe needs")

have_ <- tibble::tibble(
  What     = c("corpus store", "sample text", "prepared", "register", "geo lookup"),
  Path     = c(.pP$Store$Corpus, .pP$Text$Sample, .pP$Input$Prepared,
               .pP$Input$Register, .pP$Input$Lookup),
  NeededBy = c("everything", "reference only", "reference only", "sections 2-6",
               "the GPE chain only")
) |>
  dplyr::mutate(Exists = fs::file_exists(.data$Path) | fs::dir_exists(.data$Path))

have_ |>
  dplyr::mutate(Path = as.character(fs::path_file(.data$Path))) |>
  tbl_say(.title = "Inputs")

miss_ <- dplyr::filter(have_, !.data$Exists)
if (nrow(miss_) > 0L) {
  cli::cli_alert_warning(
    "Missing: {paste(miss_$What, collapse = ', ')}. Only what the NeededBy column names is skipped."
  )
}

.ok_corpus <- isTRUE(have_$Exists[have_$What == "corpus store"])
.ok_reg    <- isTRUE(have_$Exists[have_$What == "register"])
.ok_geo    <- isTRUE(have_$Exists[have_$What == "geo lookup"])

if (!.ok_corpus) cli::cli_alert_danger("No corpus store. Render 04C first.")


# 2. The index, the keys and the daemons -----------------------------------------------------------------------------
#
# THE DAEMONS ARE STARTED HERE AND NOWHERE ELSE. cor_read_text() assumes the caller started them --
# its roxygen says so -- and passing .workers into a session with none silently falls back to a
# sequential read at about 330 doc/s, which is what made the first run of this probe look like a
# scaling problem when it was a configuration one.

cli::cli_h1("2. Index, keys and daemons")

tab_index <- tibble::tibble(DocID = character(0), Path = character(0))
tab_keys  <- tibble::tibble()
par_ok_   <- FALSE

if (.ok_corpus && .ok_reg) {
  res_index_ <- prb_time(prb_corpus_index(.dir_store = .pP$Store$Corpus, .family = .pP$Family))
  tab_index  <- res_index_$Value

  res_keys_ <- prb_time(ent_corpus_keys(
    .path_register = .pP$Input$Register,
    .path_release  = ent_release_path(.dir_release = .pP$Input$Release),
    .engine        = "Bert",
    .doc_ids       = tab_index$DocID,
    .quiet         = TRUE
  ))
  tab_keys <- res_keys_$Value

  # PREDICTION: Class is present for nearly every document. 03F classified the same population 04C
  # extracted and failed on the same 736. A large hole means the release is partial or the wrong
  # directory, and mny_aggregate() and red_counts() both select Class off these keys.
  cls_ok_ <- "Class" %in% names(tab_keys) && sum(!is.na(tab_keys$Class)) > 0L
  if (!cls_ok_) {
    cli::cli_alert_danger(
      "The keys carry no Class. MONEY and REDACT select it and will abort; check \\
       {.path {as.character(.pP$Input$Release)}}."
    )
  } else {
    cli::cli_alert_info(
      "Class present for {tbl_pct(mean(!is.na(tab_keys$Class)))} of keys, \\
       AmendType for {tbl_pct(mean(!is.na(tab_keys$AmendType)))}."
    )
  }

  par_ok_ <- cor_daemons_start(.workers = .pP$Workers)

  tibble::tibble(
    Step = c("corpus index", "corpus anchor keys", "daemons"),
    Rows = c(nrow(tab_index), nrow(tab_keys), if (par_ok_) .pP$Workers else 1L),
    Secs = round(c(res_index_$Secs, res_keys_$Secs, 0), 2)
  ) |>
    tbl_say(.title = "Setup")

  # PREDICTION: the keys now cover the index almost exactly. A shortfall is a document 04C extracted
  # that 02B does not carry, which would be a register problem rather than a probe one.
  cov_ <- nrow(tab_keys) / max(nrow(tab_index), 1L)
  if (cov_ < 0.99) {
    cli::cli_alert_warning(
      "Keys cover {tbl_pct(cov_)} of the extracted index. The gap is documents 04C extracted and \\
       the register does not carry."
    )
  } else {
    cli::cli_alert_success("Keys cover {tbl_pct(cov_)} of the extracted index.")
  }

  if (!par_ok_) {
    cli::cli_alert_warning(
      "Reading sequentially at roughly 330 doc/s. Everything below still measures the right thing, \\
       only the staged read is slow."
    )
  }
}

.ok_run <- .ok_corpus && .ok_reg && nrow(tab_keys) > 0L


# 3. The one staged read ---------------------------------------------------------------------------------------------
#
# Staged once at the largest size. Every ladder point below cuts .lens down over this same file.

cli::cli_h1("3. Staging the text, once")

tab_lens_full <- tibble::tibble(DocID = character(0), DocLen = integer(0))

if (.ok_run) {
  docs_top_  <- prb_draw(.index = tab_index, .n = max(.pP$Sizes), .seed = .pP$Seed)
  res_stage_ <- prb_time(prb_stage_text(
    .docs = docs_top_, .path_stage = .pP$Stage, .workers = if (par_ok_) .pP$Workers else 1L
  ))
  res_lens_  <- prb_time(ent_doc_lens(.path_text = .pP$Stage))
  tab_lens_full <- res_lens_$Value

  tibble::tibble(
    Step    = c("stage text", "ent_doc_lens() over the stage"),
    Rows    = c(res_stage_$Rows, res_lens_$Rows),
    Secs    = round(c(res_stage_$Secs, res_lens_$Secs), 2),
    DocPerS = round(c(res_stage_$Rows / res_stage_$Secs, NA_real_), 1)
  ) |>
    tbl_say(.title = "One staged read")

  cli::cli_alert_info(
    "At {round(res_stage_$Rows / res_stage_$Secs, 1)} doc/s the whole corpus reads once in \\
     {cor_duration(nrow(tab_index) / (res_stage_$Rows / res_stage_$Secs))}. 04D pays that ONCE, \\
     not once per chain."
  )
}


# 4. Q2 -- is the document length already in the register? -----------------------------------------------------------
#
# ent_doc_lens() reads text to compute stri_length(TextRaw). The register carries nChars for every
# document. If they agree, ORG and REDACT stop needing text at all and the staged read shrinks to
# the three chains that cut windows out of it.
#
# THIS IS NOT A TIDINESS QUESTION. DocLen is the denominator of every relative position and of the
# tail share in ent_window_spec(). A systematic 3% difference moves every window rule in the family
# without changing a single line of rule code.

cli::cli_h1("4. Q2: is DocLen already stored?")

if (.ok_run && nrow(tab_lens_full) > 0L) {
  cmp_ <- tab_lens_full |>
    dplyr::inner_join(
      dplyr::select(tab_keys, "DocID", "nChars"), by = dplyr::join_by(DocID)
    ) |>
    dplyr::filter(!is.na(.data$nChars), .data$nChars > 0L) |>
    dplyr::mutate(RelDiff = abs(.data$nChars - .data$DocLen) / .data$DocLen)

  tibble::tibble(
    Docs      = nrow(cmp_),
    Exact     = tbl_pct(mean(cmp_$RelDiff == 0)),
    Within1   = tbl_pct(mean(cmp_$RelDiff <= 0.01)),
    Within10  = tbl_pct(mean(cmp_$RelDiff <= 0.10)),
    MedRelDif = round(stats::median(cmp_$RelDiff), 4),
    MaxRelDif = round(max(cmp_$RelDiff), 3)
  ) |>
    tbl_say(.title = "Register nChars against stri_length(TextRaw)")

  # PREDICTION: agreement is high but not exact, because 04B5 found the register's WORD count within
  # one per cent for only 47% of documents. Exact agreement means the register counted the same
  # canonical text and the lens is free; broad agreement with a fat tail means it is free for
  # counting and wrong for positioning, which are different uses of the same column.
  if (mean(cmp_$RelDiff == 0) > 0.99) {
    cli::cli_alert_success(
      "nChars IS the character length. ORG and REDACT need no text; read the register instead."
    )
  } else if (mean(cmp_$RelDiff <= 0.01) > 0.95) {
    cli::cli_alert_warning(
      "nChars agrees within one per cent for {tbl_pct(mean(cmp_$RelDiff <= 0.01))} but is not \\
       identical. Usable as a count, not as a position denominator without saying so."
    )
  } else {
    cli::cli_alert_danger(
      "nChars and the text length disagree materially. The lens must come from the text."
    )
  }
}


# 5. Q3 -- is each chain linear in documents? ------------------------------------------------------------------------
#
# ONE CHAIN PER ENTITY AT THREE NESTED SIZES, all over the single stage from section 3. Ratio is
# time growth divided by document growth, so 1.0 is linear and 4.0 is quadratic.
#
# ORG AND REDACT TAKE .stage AND IGNORE IT, and that is the finding rather than an oversight: they
# need spans and lengths, not text.

cli::cli_h1("5. Q3: does the chain scale?")

prb_chain_org <- function(.lens, .keys, .stage) {
  spans_ <- ent_load_entity(
    .dir_store = .pP$Store$Corpus, .family = "lexnlp", .entity = "ORG", .lens = .lens,
    .extras = c("NameCore", "LegalForm", "Description"), .model = NULL, .quiet = TRUE
  ) |>
    dplyr::mutate(
      SpanKey = ent_norm_key(.x = .data$Span, .min = 1L),
      CoreKey = ent_norm_key(.x = dplyr::coalesce(.data$NameCore, .data$Span), .min = 1L)
    )
  ent_apply(
    .spans = spans_, .keys = .keys, .lens = .lens,
    .rule  = ent_rule(),
    .spec  = ent_window_spec(.kind = "fixed", .par = 2000, .tail_share = 0.10)
  )$Counts
}

prb_chain_date <- function(.lens, .keys, .stage) {
  spec_  <- dte_spec(.cap_years = 30)
  dates_ <- dte_load(.dir_store = .pP$Store$Corpus, .lens = .lens, .quiet = TRUE)
  terms_ <- dte_load_terms(.dir_store = .pP$Store$Corpus, .lens = .lens, .quiet = TRUE)
  desc_  <- dte_describe(.dates = dates_, .keys = .keys, .path_text = .stage, .spec = spec_)
  dte_duration(.dates = desc_, .terms = terms_, .keys = .keys, .spec = spec_)
}

prb_chain_money <- function(.lens, .keys, .stage) {
  spec_  <- mny_spec(.filter = "par", .cue_win = 60L, .label = NULL)
  money_ <- mny_load(
    .dir_store = .pP$Store$Corpus, .lens = .lens, .path_text = .stage,
    .families = c("lexnlp", "matcon"), .win = 60L, .quiet = TRUE
  )
  mny_doc_table(
    .agg  = mny_aggregate(.money = money_, .keys = .keys, .spec = spec_),
    .keys = .keys
  )
}

prb_chain_redact <- function(.lens, .keys, .stage) {
  # THE WORD COUNT COMES FROM THE REGISTER, NOT FROM A RECOUNT. red_words() reads every document to
  # produce what 02B already stored, which is one of the two "two implementations of one
  # measurement" cases the 04 pass recorded. Section 4 established that the register's nChars is
  # exact against the text; nWords rides the same read.
  words_ <- dplyr::select(.keys, "DocID", "NWords")
  marks_ <- red_load(
    .dir_store = .pP$Store$Corpus, .lens = .lens, .family = "matcon", .quiet = TRUE
  )
  red_counts(.marks = marks_, .words = words_, .keys = .keys)
}

prb_chain_gpe <- function(.lens, .keys, .stage) {
  # THE GAZETTEER IS LOADED ONCE, OUTSIDE. Reading it per chunk is a fixed cost charged per chunk,
  # and at 2,000 documents it was most of the chain's time -- which would have read as a chain that
  # scales beautifully rather than as a constant that dwarfed the work.
  lookup_ <- .geo_lookup_once
  cand_   <- .geo_cand_once

  raw_ <- ent_load_entity(
    .dir_store = .pP$Store$Corpus, .family = "matcon", .entity = "GPE", .lens = .lens,
    .extras = ent_extras("matcon", "GPE"), .model = NULL, .quiet = TRUE
  ) |>
    geo_resolve(.lookup = lookup_, .family = "matcon") |>
    dplyr::mutate(Combo = "matcon", .before = 1L)

  geo_ <- raw_ |>
    geo_city_state(.cand = cand_) |>
    geo_country()

  geo_context(.geo = geo_, .path_text = .stage, .win = 120L)
}

.geo_lookup_once <- if (.ok_geo) geo_lookup(.path_lookup = .pP$Input$Lookup) else NULL
.geo_cand_once   <- if (.ok_geo) geo_candidates(.path_lookup = .pP$Input$Lookup) else NULL

chains_ <- list(
  ORG    = prb_chain_org,
  DATE   = prb_chain_date,
  MONEY  = prb_chain_money,
  REDACT = prb_chain_redact
)
if (.ok_geo) chains_$GPE <- prb_chain_gpe

if (.ok_run && nrow(tab_lens_full) > 0L) {

  # One entry per chain, flipped by the first failure. An environment rather than a list because it
  # is written from inside a map and read on the next pass.
  .failed <- new.env(parent = emptyenv())

  out_scale <- purrr::map(.pP$Sizes, function(.n) {
    lens_ <- dplyr::slice_head(tab_lens_full, n = min(.n, nrow(tab_lens_full)))

    # THE KEYS ARE CUT WITH THE LENS, AND THIS IS THE FINDING THE FIRST TIMED RUN PRODUCED.
    # ent_apply() runs FROM THE KEY SIDE so that a document in which nothing was found still gets a
    # row -- which is correct, and which means the output is one row per KEY, not one per document
    # in the chunk. Handed the corpus keys with a 2,000-document lens it built 1.19 million rows to
    # describe 2,000 documents, every time, at every ladder point. A chunk is (keys INTERSECT lens)
    # and the two must be cut together or the chunk is not a chunk.
    keys_ <- dplyr::semi_join(tab_keys, lens_, by = dplyr::join_by(DocID))

    cli::cli_alert_info(
      "{format(nrow(lens_), big.mark = ',')} {cli::qty(nrow(lens_))} document{?s}, \\
       {format(nrow(keys_), big.mark = ',')} {cli::qty(nrow(keys_))} key{?s}."
    )

    purrr::imap(chains_, function(.f, .name) {

      # A CHAIN THAT FAILED SMALL FAILS LARGE FOR THE SAME REASON, and running it again is time
      # spent measuring how fast it can fail -- the same mistake as reporting 1,180 doc/s over 736
      # documents that could not be read. Skipped rather than retried, and named as skipped.
      if (isTRUE(mget(.name, envir = .failed, ifnotfound = FALSE)[[1L]])) {
        cli::cli_alert_info("  {(.name)}: skipped, failed at a smaller size")
        return(tibble::tibble(
          Chain = .name, Docs = nrow(lens_), Secs = NA_real_, Rows = NA_integer_, Note = "skipped"
        ))
      }

      t0_  <- Sys.time()
      res_ <- try(prb_time(.f(lens_, keys_, .pP$Stage)), silent = TRUE)
      el_  <- round(as.numeric(difftime(Sys.time(), t0_, units = "secs")), 1)

      # THE MESSAGE IS PRINTED WHERE IT HAPPENS. The previous version stored 52 characters of it in
      # a column nobody saw until fifteen runs had finished, so five failures looked like a slow
      # probe rather than five diagnosable errors.
      if (inherits(res_, "try-error")) {
        assign(.name, TRUE, envir = .failed)
        msg_ <- trimws(gsub("[\r\n]+", " ", as.character(res_)))
        cli::cli_alert_danger("  {(.name)}: failed after {(el_)}s")
        cli::cli_text("    {(stringi::stri_sub(msg_, 1L, 300L))}")
        return(tibble::tibble(
          Chain = .name, Docs = nrow(lens_), Secs = NA_real_, Rows = NA_integer_,
          Note = trimws(stringi::stri_sub(msg_, 1L, 52L))
        ))
      }

      cli::cli_alert_success(
        "  {(.name)}: {(el_)}s, {format(res_$Rows, big.mark = ',')} \\
         {cli::qty(res_$Rows)} row{?s}"
      )
      tibble::tibble(
        Chain = .name, Docs = nrow(lens_), Secs = round(res_$Secs, 1), Rows = res_$Rows, Note = ""
      )
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()

  out_scale <- out_scale |>
    dplyr::arrange(.data$Chain, .data$Docs) |>
    dplyr::mutate(
      Ratio   = round((.data$Secs / dplyr::lag(.data$Secs)) /
                        (.data$Docs / dplyr::lag(.data$Docs)), 2),
      DocPerS = round(.data$Docs / .data$Secs, 1),
      .by     = Chain
    ) |>
    dplyr::select("Chain", "Docs", "Secs", "Rows", "DocPerS", "Ratio", "Note")

  tbl_say(out_scale, .title = "Chain timing across the ladder")

  cli::cli_alert_info(
    "Ratio is time growth over document growth. 1.0 is linear; 2.0 means the corpus costs twice \\
     what extrapolation predicts. The staged read is section 3's number and is not in here."
  )

  # PREDICTION, STATED BEFORE READING THE TABLE: REDACT and MONEY are linear -- a span filter and a
  # per-document sum. DATE is linear but slow, because dte_describe() cuts a cue window per span.
  # ORG is linear in time and superlinear in MEMORY, since ent_match_facts() joins keys to entities.
  # GPE is the candidate for superlinearity: geo_context() reads a window per span.
  worst_ <- out_scale |>
    dplyr::filter(!is.na(.data$Ratio)) |>
    dplyr::slice_max(.data$Ratio, n = 1L, with_ties = FALSE)

  if (nrow(worst_) > 0L && worst_$Ratio > 1.3) {
    cli::cli_alert_warning(
      "{(worst_$Chain)} grows at {(worst_$Ratio)}x the document rate. Chunk it smaller, or find \\
       the join doing it, before 04D is written around it."
    )
  } else {
    cli::cli_alert_success("Every chain is within 1.3x of linear. Chunking is sound on time.")
  }

  full_ <- out_scale |>
    dplyr::filter(.data$Docs == max(out_scale$Docs), !is.na(.data$DocPerS)) |>
    dplyr::mutate(Hours = round(nrow(tab_index) / .data$DocPerS / 3600, 2))

  tbl_say(dplyr::select(full_, "Chain", "DocPerS", "Hours"),
          .title = "Projected full-corpus cost, at the top of the ladder")
}


# 6. Q4 -- does the rule move when the partition moves? --------------------------------------------------------------
#
# ent_apply() calls ent_token_freq() on the spans it was handed, and ent_rule()'s .fam_df admits a
# SINGLE shared leading token when that token opens the name of at most .fam_df of documents. Under
# chunking the denominator is the chunk.
#
# Compute the token frequency once over the staged set, then over partitions of it, and count the
# tokens falling on different sides of the threshold. Each one is a family match that fires in one
# chunk and not another, on identical text.

cli::cli_h1("6. Q4: is the family rule stable under partition?")

if (.ok_run && nrow(tab_lens_full) > 0L) {

  rule_     <- ent_rule()
  spans_q4_ <- ent_load_entity(
    .dir_store = .pP$Store$Corpus, .family = "lexnlp", .entity = "ORG", .lens = tab_lens_full,
    .extras = c("NameCore", "LegalForm", "Description"), .model = NULL, .quiet = TRUE
  ) |>
    dplyr::mutate(
      SpanKey = ent_norm_key(.x = .data$Span, .min = 1L),
      CoreKey = ent_norm_key(.x = dplyr::coalesce(.data$NameCore, .data$Span), .min = 1L)
    )

  ent_q4_  <- ent_mark_fragments(.ent = ent_entities(.spans = spans_q4_, .rule = rule_))
  freq_all <- ent_token_freq(.ent = ent_q4_) |>
    dplyr::transmute(FirstTok = .data$FirstTok, ShareAll = .data$TokShare)

  out_drift <- purrr::map(.pP$Chunks, function(.k) {
    docs_   <- unique(ent_q4_$DocID)
    assign_ <- withr::with_seed(.pP$Seed, sample(rep_len(seq_len(.k), length(docs_))))
    map_    <- tibble::tibble(DocID = docs_, Chunk = assign_)

    per_ <- ent_q4_ |>
      dplyr::inner_join(map_, by = dplyr::join_by(DocID)) |>
      dplyr::group_split(.data$Chunk) |>
      purrr::map(\(.d) ent_token_freq(.ent = .d)) |>
      purrr::list_rbind() |>
      dplyr::summarise(
        ShareMin = min(.data$TokShare), ShareMax = max(.data$TokShare), .by = FirstTok
      )

    joined_ <- freq_all |>
      dplyr::inner_join(per_, by = dplyr::join_by(FirstTok)) |>
      dplyr::mutate(
        # A token FLIPS when the pooled share and some chunk's share fall on opposite sides of the
        # threshold: admitted as a family opener under one partition, refused under another.
        Flips = (.data$ShareAll <= rule_$FamDf & .data$ShareMax >  rule_$FamDf) |
                (.data$ShareAll >  rule_$FamDf & .data$ShareMin <= rule_$FamDf)
      )

    tibble::tibble(
      Chunks   = .k,
      DocsEach = round(length(docs_) / .k),
      Tokens   = nrow(joined_),
      nFlip    = sum(joined_$Flips),
      PctFlip  = tbl_pct(mean(joined_$Flips))
    )
  }) |>
    purrr::list_rbind()

  tbl_say(out_drift, .title = "Leading tokens that change side under partition")

  # PREDICTION: nFlip exceeds zero at every chunk count and rises as chunks shrink, because TokShare
  # is a proportion and its variance grows as the denominator falls. The number answers HOW MUCH,
  # not WHETHER, and the fix is the same either way: compute the frequency once, pass it in.
  if (any(out_drift$nFlip > 0L)) {
    cli::cli_alert_warning(
      "The family rule is partition-dependent. ent_apply() must accept a frozen .freq rather than \\
       computing one, and 04D must build it in a pass of its own before applying anything."
    )
  } else {
    cli::cli_alert_success(
      "No token changes side at any chunk count. ent_apply() can be called per chunk unchanged."
    )
  }
}


# 7. What this probe concluded ---------------------------------------------------------------------------------------

if (fs::file_exists(.pP$Stage)) fs::file_delete(.pP$Stage)
if (par_ok_) mirai::daemons(0L)

cli::cli_h1("7. Read these five things together")

cli::cli_ul(c(
  "Section 3's doc/s: what one pass over the corpus text costs, paid once rather than per chain.",
  "Section 4: whether the register's nChars removes the text read for ORG and REDACT.",
  "Section 5's Ratio: whether any chain must be chunked smaller than the rest.",
  "Section 5's Hours: whether the rule pass is an evening or a week.",
  "Section 6's nFlip: whether ORG needs a frequency pass before the apply pass."
))

cli::cli_alert_info(
  "The staged parquet is removed and the daemons are stopped. Every store was opened read-only \\
   and every draw is seeded, so a second run reproduces these numbers exactly."
)
