# 002g-GetDisclosures-V7: the disclosure store and panel from EDGAR-CORPUS ----------------------
#
# WHAT THIS IS
# V7 builds the paragraph and sentence store from EDGAR-CORPUS (Loukas et al. 2021) instead of
# from a parse of raw EDGAR. Items are GIVEN - one field per 10-K section - so the ladder V6 spent
# three detector generations on does not exist here, and neither do the html and text parse
# routes, fusion, or the item back-fill.
#
# STANDALONE, THE WAY V6 WAS STANDALONE FROM V5. This file contains every function V7 calls: the
# fifty V6 definitions it depends on, extracted by dependency closure and carried VERBATIM under
# their V6 names, followed by the V7 loader and the three functions that differ. Function names
# are unchanged so 058 and everything downstream run untouched - which is also why V6 and V7 must
# not be sourced into the same session: the second source() wins on every shared name.
#
# What is NOT here, and why:
#   v5_build, v5_task, v5_walk, v5_fuse, v5_segment, v5_derive   the raw-EDGAR parser
#   it_*, v6_item_*                                              the item ladder
#   v5_agg_*, v6_agg_items, v6_agg_build                          V5's aggregators and V6's build,
#                                                                 replaced below by v7_agg_*
#   v5_verify                                                     reads _skipped, which V7 lacks
#
# WHY. Measured 2026-09-02 on 119,553 identical filings: V6 and EDGAR-CORPUS agree on every
# document-scope result to the coefficient and differ ONLY in item assignment - theirs finds Item 2
# on 87% of html filings against V6's 7%. This file makes theirs the primary corpus and V6 the
# independent check.
#
# WHAT A V7 MEASURE MEANS. A V7 sentence is v5_sentences()'s sentence: ICU boundaries after
# v5_squish(), stored as offsets into paragraph text. A V7 prose paragraph is v5_derive()'s prose,
# by the same four regexes. Tables are already gone from the source, so is_table is FALSE
# throughout. filed_year carries the corpus's period-of-report year; form_type is "10-K" because
# the variant is not recorded.
#
# THE ONE FIX APPLIED TO CARRIED CODE. v5_sentences() counted words with substr() where
# substring() was meant; every sentence in a paragraph carried the first sentence's count. Found by
# v7_check_sentences() on 2026-09-02. No exported column reads sentences.n_words; the fix is in
# both files.



# Store and connection ------------------------------------------------------------------------------
# Carried verbatim from 002g-GetDisclosures-V6.R.

#' Open a DuckDB connection.
#' `.memory` and `.threads` are NOT optional under parallelism.
#'
#' DuckDB's memory limit is per DATABASE INSTANCE and defaults to roughly 80% of system RAM; its
#' thread count defaults to every core. Each daemon opens its own instance, so twelve workers means
#' twelve processes each believing they may take 80% of the machine and all of the CPU. That is
#' what exhausted memory immediately on 2026-08-24 - the parse stage escaped it only because its
#' workers read a handful of blocks rather than 500 documents of paragraph text.
#'
#' Both SETs existed in the separate connect helpers this function replaced and were lost in the
#' merge, exactly as `enable_progress_bar` was. NULL leaves DuckDB's default, which is correct for
#' the parent and wrong for a worker.
v5_connect <- function(.path_db, .read_only = TRUE, .memory = NULL, .threads = NULL) {

  con_ <- DBI::dbConnect(
    drv       = duckdb::duckdb(),   # DuckDB driver object
    dbdir     = .path_db,           # store file on disk
    read_only = .read_only          # FALSE only for the writer; readers must not lock it
  )

  # DuckDB's OWN progress bar writes carriage returns to the console and overwrites any cli
  # progress bar running above it - which is what hid the term-flagging progress on 2026-08-24.
  # Disabled here rather than at each call site so no connection can be opened without it. It is a
  # session setting, so it works on a read-only connection too.
  DBI::dbExecute(conn = con_, statement = "SET enable_progress_bar = false")

  if (!is.null(x = .memory)) {
    DBI::dbExecute(
      conn      = con_,
      statement = sprintf(fmt = "SET memory_limit = '%s'", .memory)
    )
  }
  if (!is.null(x = .threads)) {
    DBI::dbExecute(
      conn      = con_,
      statement = sprintf(fmt = "SET threads = %d", as.integer(x = .threads))
    )
  }

  con_
}

#' Create the store and its schema. Idempotent.
v5_store_init <- function(.path_db, .overwrite = FALSE) {

  if (.overwrite && file.exists(.path_db)) {
    cli::cli_alert_warning(text = "Overwriting {basename(path = .path_db)}")
    file.remove(.path_db)
  }

  dir.create(
    path          = dirname(path = .path_db),  # Store/ directory
    recursive     = TRUE,                      # create intermediate levels
    showWarnings  = FALSE                      # existing directory is not an error here
  )

  con_ <- v5_connect(
    .path_db   = .path_db,   # store to create
    .read_only = FALSE       # writing the schema
  )
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE TABLE IF NOT EXISTS reports (
        doc_id      VARCHAR PRIMARY KEY,
        cik         INTEGER,
        form_type   VARCHAR,
        date_filed  DATE,
        filed_year  INTEGER,
        n_block     BIGINT,
        route       VARCHAR,     -- html | text : ENDOGENOUS, report it
        parsed      BOOLEAN,
        n_par       INTEGER,
        n_prose     INTEGER,
        n_table     INTEGER,
        n_sentence  INTEGER,
        n_item_kept INTEGER,
        item_cover  DOUBLE,      -- share of prose paragraphs carrying an item
        n_forced    INTEGER      -- fusion force-closes; high means the rule struggled
      )"
  )

  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE TABLE IF NOT EXISTS paragraphs (
        doc_id      VARCHAR,
        par_id      INTEGER,
        node_type   VARCHAR,
        is_table    BOOLEAN,
        table_src   VARCHAR,     -- structural (DOM) | heuristic (text). Do NOT pool these.
        is_prose    BOOLEAN,
        item_num    VARCHAR,     -- '1', '1A', '7' - suffix preserved
        item_header BOOLEAN,
        forced      BOOLEAN,
        n_cell      INTEGER,
        n_row       INTEGER,
        n_words     INTEGER,
        n_chars     INTEGER,
        digit_share DOUBLE,
        text        VARCHAR
      )"
  )

  # Offsets, not text: storing sentence strings would duplicate the whole corpus, and a sentence
  # cannot disagree with its paragraph if it is defined as a span of it.
  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE TABLE IF NOT EXISTS sentences (
        doc_id      VARCHAR,
        par_id      INTEGER,
        sen_id      INTEGER,
        char_start  INTEGER,     -- 1-based inclusive, into paragraphs.text
        char_end    INTEGER,
        n_words     INTEGER
      )"
  )

  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE TABLE IF NOT EXISTS _progress (
        filed_year  INTEGER PRIMARY KEY,
        n_docs      INTEGER,
        n_par       BIGINT,
        n_sentence  BIGINT,
        n_skipped   INTEGER,
        limit_used  INTEGER,   -- NULL = the whole year. A number means TRUNCATED, see below.
        secs        DOUBLE,
        written_at  TIMESTAMP
      )"
  )

  # Deferred, not lost. v5_run_skipped() clears this.
  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE TABLE IF NOT EXISTS _skipped (
        doc_id      VARCHAR PRIMARY KEY,
        filed_year  INTEGER,
        n_block     BIGINT,
        reason      VARCHAR,
        cap_mb      DOUBLE,
        logged_at   TIMESTAMP
      )"
  )

  # Stores written before `limit_used` existed have no such column, and CREATE TABLE IF NOT
  # EXISTS will not add it. NULL backfills as "unknown completeness", which v5_build treats as
  # incomplete - the safe direction.
  DBI::dbExecute(
    conn      = con_,
    statement = "ALTER TABLE _progress ADD COLUMN IF NOT EXISTS limit_used INTEGER"
  )

  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE OR REPLACE VIEW v_sentences AS
      SELECT s.doc_id, s.par_id, s.sen_id, s.n_words,
             p.item_num, p.is_prose, p.is_table, p.node_type,
             SUBSTR(p.text, s.char_start, s.char_end - s.char_start + 1) AS text
        FROM sentences s JOIN paragraphs p
          ON p.doc_id = s.doc_id AND p.par_id = s.par_id"
  )

  cli::cli_alert_success(text = "Store ready: {basename(path = .path_db)}")
  invisible(x = .path_db)
}



# Text rules and sentences --------------------------------------------------------------------------
# Carried verbatim from 002g-GetDisclosures-V6.R.

v5_rx <- list(
  Term     = "[.!?][\"')\\]]?$",
  Leader   = "\\.{4,}",
  Cols     = "( {2,}\\S+){3,}",
  Item     = "(?i)^\\s*ITEM\\s*([0-9]{1,2})\\s*([A-Z])?\\s*[.:)\\-]?\\s",
  ItemKey  = "^([0-9]{1,2})([A-Z]?)$",
  ExhibNum = "(?i)^\\s*\\(?(ex(hibit)?\\s*)?[0-9]{1,2}(\\.[0-9]{1,3})?[a-z]?\\)?[.\\s\\-]+\\S",
  ExhibCue = "(?i)(exhibit|incorporated (herein )?by reference|filed (as|with))",
  Cert     = "(?i)^\\s*(certification|pursuant to (18|section)|i,? .{0,40}certify)",
  Furn     = "(?i)^\\s*(table of contents|index|page [0-9]+)\\s*$",
  PageNum  = "[A-Za-z]\\s+[0-9]{1,3}\\s*$",
  Letters  = "[A-Za-z]{2,}",
  TitleCse = "^([A-Z][a-z]*\\s+){2,}",
  NotBlank = "[^\\p{White_Space}]",
  BlankLn  = "\\n[ \\t\\r]*\\n",
  Digit    = "[0-9]",
  SenRegex = "[^.!?]+[.!?]+[\\s]*|[^.!?]+$",
  Heading  = "^h[1-6]$"
)

#' Does the text end on terminal punctuation? The fusion stop condition.
v5_is_term <- function(.txt) {
  stringi::stri_detect_regex(
    str     = trimws(x = .txt),   # trailing whitespace would hide the punctuation
    pattern = v5_rx$Term          # . ! ? with an optional closing quote or bracket
  )
}

#' Collapse runs of whitespace to a single space.
#'
#' WHY THIS EXISTS, and why it runs AFTER fusion rather than before. ICU sentence boundaries
#' follow UAX #29, and rule SB4 breaks after a paragraph separator - which includes a bare "\n".
#' Filing text is full of newlines: fixed-width filings hard-wrap at ~80 columns, and html_text
#' preserves whatever the markup contained. So every physical LINE was being emitted as a
#' sentence. Measured: 9.27 sentences per prose paragraph at a median of 8 words each, against
#' the 18-28 words V3 measured for real 10-K sentences.
#'
#' V3 collapsed whitespace BEFORE finding sentences, which destroyed paragraph structure. Doing
#' it here keeps the structure - paragraph boundaries are already fixed by this point - and only
#' removes the layout artefact that was fooling the splitter.
#'
#' TABLES ARE EXEMPT: for an ASCII table the column alignment IS the content.
v5_squish <- function(.txt) {
  stringi::stri_trim_both(
    str = stringi::stri_replace_all_regex(
      str         = .txt,      # segment text as extracted
      pattern     = "\\s+",    # any run of whitespace, newlines included
      replacement = " "        # one space
    )
  )
}

#' Blank in the full Unicode sense, and NA-safe.
#'
#' base::trimws strips only [ \t\r\n], so a segment of non-breaking spaces survives it; and
#' nzchar(trimws(NA)) is TRUE, so an NA text passes an nzchar filter. Both put empty paragraphs
#' in the store before this existed.
v5_blank <- function(.txt) {
  is.na(x = .txt) |
    !stringi::stri_detect_regex(
      str     = .txt,             # candidate segment
      pattern = v5_rx$NotBlank    # any non-whitespace character, Unicode-aware
    )
}

#' Split paragraphs into sentences, returned as OFFSETS into the paragraph text.
#'
#' WHY stringi AND NOT ANOTHER R PACKAGE
#'   tokenizers::tokenize_sentences imports stri_split_boundaries - the same ICU engine with a
#'   different wrapper, so it changes nothing. quanteda genuinely differs: its segmenter adds
#'   rules to avoid splitting on "Mr." and similar. But we store CHARACTER OFFSETS, and only
#'   stringi returns them directly; quanteda returns tokens, so offsets would have to be
#'   recovered by matching strings back to positions - fragile on repeated text.
#'
#' THE ABBREVIATION PROBLEM, and stringi's own answer to it. Plain ICU breaks after "Inc.",
#' "Ltd.", "Co.", "U.S." and "No." - all of which are everywhere in a 10-K. ICU >= 56 ships a
#' FILTERED break iterator with a suppression list, reachable through the locale as
#' "en_US@ss=standard". That is what `icu_filtered` uses.
#'
#' DEFAULT IS STILL PLAIN `icu` until the two are compared on our own text. The filtered iterator
#' is very likely better here, but "very likely" is not a measurement, and this stage re-runs from
#' the stored paragraphs without re-parsing, so switching later costs nothing.
#'
#' Data tables are not split: one span covering the whole segment.
v5_sentences <- function(.par, .method = c("icu", "icu_filtered", "regex")) {

  .method <- match.arg(arg = .method)

  brk_ <- if (.method == "icu_filtered") {
    stringi::stri_opts_brkiter(
      type   = "sentence",           # sentence boundaries
      locale = "en_US@ss=standard"   # ICU >= 56: suppresses common abbreviations
    )
  } else {
    stringi::stri_opts_brkiter(type = "sentence")
  }

  purrr::pmap(
    .l = list(.par$DocID, .par$ParID, .par$Text, .par$IsTable),
    .f = \(.d, .p, .t, .tab) {

      if (.tab || is.na(x = .t) || !nzchar(x = .t)) {
        return(
          tibble::tibble(
            DocID     = .d,
            ParID     = .p,
            SenID     = 1L,
            CharStart = 1L,
            CharEnd   = nchar(x = .t),
            NWords    = stringi::stri_count_words(str = .t)
          )
        )
      }

      loc_ <- if (.method == "regex") {
        stringi::stri_locate_all_regex(
          str     = .t,               # paragraph text
          pattern = v5_rx$SenRegex    # crude fallback, for comparison only
        )[[1]]
      } else {
        stringi::stri_locate_all_boundaries(
          str          = .t,     # paragraph text
          opts_brkiter = brk_    # plain or abbreviation-filtered, per .method
        )[[1]]
      }

      if (is.null(x = loc_) || all(is.na(x = loc_))) {
        loc_ <- matrix(
          data = c(1L, nchar(x = .t)),   # whole paragraph as one span
          ncol = 2
        )
      }

      tibble::tibble(
        DocID     = .d,
        ParID     = .p,
        SenID     = seq_len(length.out = nrow(x = loc_)),
        CharStart = as.integer(x = loc_[, 1]),
        CharEnd   = as.integer(x = loc_[, 2]),
        # substring(), NOT substr(). Measured 2026-09-02 by v7_check_sentences(): substr() returns
        # a result the length of `x`, so with one paragraph and a vector of spans it extracted the
        # FIRST sentence only and the tibble recycled that one count down every row - every
        # sentence in a paragraph carried sentence 1's word count. substring() recycles x.
        #
        # NO EXPORTED COLUMN READS sentences.n_words: every nWords sums paragraphs.n_words, which
        # was always right. What this corrupted is v5_compare_splitters() and v5_verify(), whose
        # MedWords / MedSenWords were first-sentence lengths. The V6 store still carries the bad
        # column; nothing downstream needs it recomputed.
        NWords    = stringi::stri_count_words(
          str = substring(text = .t, first = loc_[, 1], last = loc_[, 2])
        )
      )
    }
  ) |>
    purrr::list_rbind()
}



# Model scores --------------------------------------------------------------------------------------
# Carried verbatim from 002g-GetDisclosures-V6.R.

#' The nine task models, with the labels each is EXPECTED to emit.
#'
#' `Tokenizer` overrides where the model repo ships none. climatebert/transition-physical and
#' climatebert/renewable contain only config.json and pytorch_model.bin - BY DESIGN: both model
#' cards show example code loading the DETECTOR's tokenizer while model_name points at the model.
#' Transformers does not error on the missing files; it builds a tokenizer with vocab_size=5 and
#' encodes every paragraph to <s></s>. That is what produced 2.4M identical labels on 2026-08-19.
#' Note the donor matters: distilroberta-base-climate-f gives DIFFERENT ids for the same sentence,
#' so borrowing the wrong sibling silently shifts the vocabulary.
#'
#' `IdLabel` names the classes where the config carries none. Both mappings were established by
#' probe on 2026-08-19, six sentences each, every expected group landing on its own single label at
#' confidence >= 0.997:
#'   transition-physical  LABEL_0 = transition, LABEL_1 = none, LABEL_2 = physical
#'   renewable            LABEL_0 = no,         LABEL_1 = yes
#'
#' `ClimateOnly` marks the models the authors document as operating on CLIMATE-RELATED paragraphs.
#' They have no null class, so applied to arbitrary text they still return a category - ClTCFD's
#' training set has a fifth label, none (not climate-related), that the published model does not
#' expose. Scoring everything is right; INTERPRETING it unconditionally is not, and the cascade is
#' a join on ClDetect = 'yes' at analysis time.
#'
#' `Expect` is a check, not a filter: the Python side compares it against the model's own id2label
#' and warns on a mismatch, and the labels AS RUN are recorded in `_models`. That is how a renamed
#' or re-versioned checkpoint gets caught rather than silently changing a column's meaning.
#'
#' `Unit` records what each model card says it was trained on. All nine say PARAGRAPHS - the
#' detector and specificity cards state outright that the model may not perform well on sentences.
#' That is the reason V5 scores paragraphs, and a reason V3's ~400-token windows were wrong.
v5_models <- function() {

  det_ <- "climatebert/distilroberta-base-climate-detector"

  tibble::tribble(
    ~Model,          ~HfId,                                               ~Tokenizer,    ~IdLabel,                     ~Expect,                            ~Signal,             ~ClimateOnly, ~Unit,       ~Note,
    "ClDetect",      "climatebert/distilroberta-base-climate-detector",   NA_character_, NA_character_,                "no,yes",                           "yes",               FALSE,        "paragraph", "is the paragraph climate-related",
    "NetZero",       "climatebert/netzero-reduction",                     NA_character_, NA_character_,                "none,reduction,net-zero",          "reduction,net-zero",FALSE,        "paragraph", "target type; see the score caveat",
    "ClCommit",      "climatebert/distilroberta-base-climate-commitment", NA_character_, NA_character_,                "no,yes",                           "yes",               TRUE,         "paragraph", "commitments and action",
    "ClSpecificity", "climatebert/distilroberta-base-climate-specificity",NA_character_, NA_character_,                "non,spec",                         "spec",              TRUE,         "paragraph", "specific vs vague; labels are ABBREVIATED - caught by the check on 2026-08-18",
    "ClSentiment",   "climatebert/distilroberta-base-climate-sentiment",  NA_character_, NA_character_,                "opportunity,neutral,risk",         "risk,opportunity",  TRUE,         "paragraph", "climate sentiment; signal is NON-neutral",
    "ClTCFD",        "climatebert/distilroberta-base-climate-tcfd",       NA_character_, NA_character_,                "governance,metrics,risk,strategy", NA_character_,       TRUE,         "paragraph", "TCFD category; training set has a FIFTH label (none) the model does not expose",
    "TrPhysical",    "climatebert/transition-physical",                   det_,          "transition,none,physical",   "transition,none,physical",         "physical,transition",FALSE,       "paragraph", "risk type; mapping established by probe on 2026-08-19",
    "Renewable",     "climatebert/renewable",                             det_,          "no,yes",                     "no,yes",                           "yes",               FALSE,        "paragraph", "renewable energy; mapping established by probe on 2026-08-19",
    "EnvClaims",     "climatebert/environmental-claims",                  NA_character_, NA_character_,                "no,yes",                           "yes",               FALSE,        "paragraph", "environmental claim detection"
  )
}

#' Create the scores store. Idempotent.
v5_score_store_init <- function(.path_scores, .overwrite = FALSE) {

  if (.overwrite && file.exists(.path_scores)) {
    cli::cli_alert_warning(text = "Overwriting {basename(path = .path_scores)}")
    file.remove(.path_scores)
  }

  dir.create(
    path         = dirname(path = .path_scores),
    recursive    = TRUE,
    showWarnings = FALSE
  )

  con_ <- v5_connect(.path_db = .path_scores, .read_only = FALSE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE TABLE IF NOT EXISTS scores (
        doc_id     VARCHAR,
        par_id     INTEGER,
        model      VARCHAR,
        label      VARCHAR,    -- the model's OWN label, never a recoded flag
        score      DOUBLE,     -- confidence in the label CHOSEN. NOT P(positive).
        n_tok      INTEGER,    -- tokens BEFORE truncation
        truncated  BOOLEAN     -- TRUE if the model saw only the head of the paragraph
      )"
  )

  # The ledger. This is what makes 'what is missing' answerable at any moment.
  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE TABLE IF NOT EXISTS _scored (
        doc_id     VARCHAR,
        model      VARCHAR,
        filed_year INTEGER,
        n_par      INTEGER,
        secs       DOUBLE,
        scored_at  TIMESTAMP,
        PRIMARY KEY (doc_id, model)
      )"
  )

  # SCOPE. Which paragraphs the model saw: 'all' prose, or only those ClDetect called climate.
  # Without this the ledger cannot tell them apart, and a document scored on 12% of its paragraphs
  # would read as complete - a later full run would skip it and the coverage would be silently
  # wrong. Exactly the shape of the `.limit` truncation bug fixed on 2026-08-18.
  #
  # Backfilled as 'all' because every row written before this column existed came from a full run.
  DBI::dbExecute(
    conn      = con_,
    statement = "ALTER TABLE _scored ADD COLUMN IF NOT EXISTS scope VARCHAR"
  )
  DBI::dbExecute(
    conn      = con_,
    statement = "UPDATE _scored SET scope = 'all' WHERE scope IS NULL"
  )

  # id2label AS IT WAS AT RUN TIME. A checkpoint can be updated on the Hub; without this there is
  # no way to tell afterwards whether a label set moved under us.
  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE TABLE IF NOT EXISTS _models (
        model      VARCHAR PRIMARY KEY,
        hf_id      VARCHAR,
        labels     VARCHAR,
        expect     VARCHAR,
        revision   VARCHAR,
        device     VARCHAR,
        fp16       BOOLEAN,
        max_length INTEGER,
        run_at     TIMESTAMP
      )"
  )

  # FALSE means the head is genuine but the config carried no label map, so the stored labels read
  # LABEL_0 / LABEL_1 and mean nothing until the mapping is taken from the model card. Added after
  # the first run, and CREATE TABLE IF NOT EXISTS will not add it to an existing store.
  DBI::dbExecute(
    conn      = con_,
    statement = "ALTER TABLE _models ADD COLUMN IF NOT EXISTS labels_mapped BOOLEAN"
  )

  cli::cli_alert_success(text = "Scores store ready: {basename(path = .path_scores)}")
  invisible(x = .path_scores)
}

#' What is still missing: the full (document x model) grid minus what `_scored` holds.
#'
#' The function the document-wise design exists to make possible. Cheap, runnable mid-flight, and
#' indifferent to how earlier work was partitioned.
#' A document scored under "climate" is NOT done for "all" - it saw ~12% of its paragraphs. Asking
#' for "all" must report it outstanding, or the store quietly holds partial coverage that reads as
#' complete.
v5_score_missing <- function(.path_db, .path_scores, .years = NULL, .models = NULL,
                             .limit = NULL, .scope = c("all", "climate")) {

  .scope <- match.arg(arg = .scope)

  mods_ <- v5_models()
  if (!is.null(x = .models)) mods_ <- dplyr::filter(.data = mods_, Model %in% .models)

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  docs_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT doc_id AS DocID, filed_year AS FiledYear, n_prose AS NProse
        FROM reports WHERE parsed AND n_prose > 0"
  ) |>
    tibble::as_tibble()
  DBI::dbDisconnect(conn = con_, shutdown = TRUE)

  if (!is.null(x = .years)) docs_ <- dplyr::filter(.data = docs_, FiledYear %in% .years)
  if (!is.null(x = .limit)) {
    docs_ <- docs_ |>
      dplyr::group_by(FiledYear) |>
      dplyr::slice_head(n = .limit) |>
      dplyr::ungroup()
  }

  done_ <- tibble::tibble(DocID = character(length = 0), Model = character(length = 0))
  if (file.exists(.path_scores)) {
    con2_ <- v5_connect(.path_db = .path_scores, .read_only = TRUE)
    on.exit(
      expr = DBI::dbDisconnect(conn = con2_, shutdown = TRUE),
      add  = TRUE
    )
    if ("_scored" %in% DBI::dbListTables(conn = con2_)) {
      # "all" satisfies a request for "climate" (it is a superset); "climate" does not satisfy
      # "all". The asymmetry is the whole point of the column.
      done_ <- DBI::dbGetQuery(
        conn      = con2_,
        statement = sprintf(
          fmt = "SELECT doc_id AS DocID, model AS Model FROM _scored WHERE %s",
          if (.scope == "all") "scope = 'all'" else "scope IN ('all', 'climate')"
        )
      ) |>
        tibble::as_tibble()
    }
  }

  tidyr::expand_grid(
    dplyr::select(.data = docs_, DocID, FiledYear, NProse),
    dplyr::select(.data = mods_, Model)
  ) |>
    dplyr::anti_join(y = done_, by = c("DocID", "Model"))
}

#' Export one year's paragraphs to parquet for the Python scorer.
#'
#' Text crosses to Python ONCE per year rather than once per model: the Python side loops models
#' over the same frame, so nine models cost one export.
#' `.scope` is the ONLY place the cascade lives.
#'
#'   "all"     every prose paragraph
#'   "climate" only paragraphs ClDetect already labelled 'yes' - roughly 12% of the corpus
#'
#' Everything downstream - Python, batching, writing, resuming - is unaware of the difference. A
#' second function would have duplicated the export, the dispatch, the Python call and the ledger
#' writes, and the two would have drifted; a rename in 061 silently broke 062 in this project on
#' 2026-08-18 for exactly that reason.
v5_score_export <- function(.path_db, .doc_ids, .path_out, .prose_only = TRUE,
                            .scope = c("all", "climate"), .path_scores = NULL,
                            .gate_model = "ClDetect", .gate_label = "yes",
                            .dir_tmp = tempdir()) {

  .scope <- match.arg(arg = .scope)

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  # IDS VIA PARQUET, NOT AN IN LIST. A full year holds thousands of documents; pasting them into
  # `IN (...)` builds a multi-megabyte SQL string that DuckDB must parse before it can plan.
  # read_parquet has no such limit and works on a read-only connection, where a temp table may not.
  path_ids_ <- file.path(.dir_tmp, "v5_score_ids.parquet")
  on.exit(
    expr = unlink(x = path_ids_),
    add  = TRUE
  )
  arrow::write_parquet(
    x    = tibble::tibble(doc_id = .doc_ids),
    sink = path_ids_
  )

  # The scores store is ATTACHED rather than opened separately: the gate join has to happen inside
  # DuckDB, or the paragraph ids would have to come back to R and go out again as a filter.
  if (.scope == "climate") {
    DBI::dbExecute(
      conn      = con_,
      statement = sprintf(fmt = "ATTACH '%s' AS gate (READ_ONLY)", .path_scores)
    )
  }

  gate_sql_ <- if (.scope == "climate") {
    sprintf(
      fmt = "
          JOIN gate.scores g
            ON g.doc_id = p.doc_id AND g.par_id = p.par_id
           AND g.model = '%s' AND g.label = '%s'",
      .gate_model, .gate_label
    )
  } else {
    ""
  }

  par_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "
        SELECT p.doc_id, p.par_id, r.filed_year, p.text
          FROM paragraphs p
          JOIN reports r ON r.doc_id = p.doc_id
          JOIN read_parquet('%s') w ON w.doc_id = p.doc_id%s
         WHERE TRUE%s
         ORDER BY p.doc_id, p.par_id",
      path_ids_,
      gate_sql_,
      if (.prose_only) " AND p.is_prose" else ""
    )
  ) |>
    tibble::as_tibble()

  arrow::write_parquet(
    x    = par_,        # paragraphs to score
    sink = .path_out    # handed to Python; removed by the caller afterwards
  )

  nrow(x = par_)
}

#' Invoke the Python scorer.
v5_score_python <- function(.path_in,
                            .path_scores,
                            .path_registry,
                            .models,
                            .script     = "climatebert_v5.py",
                            .batch_size = 64L,
                            .doc_chunk  = 500L,
                            .device     = "auto",
                            .fp16       = TRUE,
                            .max_length = 512L,
                            .scope      = "all",
                            .dir_python = here::here("_python")) {

  args_ <- c(
    "--input",      .path_in,
    "--out-db",     .path_scores,
    "--registry",   .path_registry,
    "--models",     paste(.models, collapse = ","),
    "--batch-size", format(x = .batch_size, scientific = FALSE),
    "--doc-chunk",  format(x = .doc_chunk,  scientific = FALSE),
    "--device",     .device,
    "--max-length", format(x = .max_length, scientific = FALSE),
    "--scope",      .scope,
    if (.fp16) "--fp16"
  )

  cli::cli_alert_info(text = "uv run {(.script)}")

  st_ <- system2(
    command = "uv",
    args    = c("run", "--project", .dir_python, "python",
                file.path(.dir_python, .script), args_)
  )

  # EXIT CODES. 0 clean; 2 means at least one model could not be loaded but the rest were scored -
  # a warning, NOT an abort, or a single Hub hiccup would kill a thirty-year run. Anything else is
  # a real failure. `_scored` records nothing for a skipped model, so v5_score_missing() reports
  # it and a re-run picks it up.
  if (st_ == 2L) {
    cli::cli_alert_warning(
      text = "{(.script)}: at least one model failed to load - see the log above. \\
              Run v5_score_missing() afterwards; a re-run will retry only what is missing."
    )
  } else if (st_ != 0L) {
    cli::cli_abort(message = "{(.script)} exited {(st_)}")
  }

  invisible(x = st_)
}

#' How many of a year's documents already carry the gate model, under scope "all".
#'
#' A "climate" run joins to ClDetect's labels, so it cannot precede them. Returning an empty export
#' would look like success, which is why this is checked rather than discovered.
v5_score_gate_ready <- function(.path_scores, .doc_ids, .gate_model = "ClDetect") {

  if (!file.exists(.path_scores)) return(0L)

  con_ <- v5_connect(.path_db = .path_scores, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )
  if (!"_scored" %in% DBI::dbListTables(conn = con_)) return(0L)

  DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "SELECT COUNT(*) AS N FROM _scored
              WHERE model = '%s' AND scope = 'all' AND doc_id IN (%s)",
      .gate_model,
      paste(sprintf(fmt = "'%s'", .doc_ids), collapse = ", ")
    )
  )$N
}

#' Score ONE year. The unit of resumability, mirroring v5_run_year.
v5_score_year <- function(.year,
                          .path_db,
                          .path_scores,
                          .models     = v5_models()$Model,
                          .limit      = NULL,
                          .batch_size = 64L,
                          .doc_chunk  = 500L,
                          .device     = "auto",
                          .fp16       = TRUE,
                          .max_length = 512L,
                          .prose_only = TRUE,
                          .scope      = c("all", "climate"),
                          .gate_model = "ClDetect",
                          .gate_label = "yes",
                          .dir_tmp    = tempdir()) {

  .scope <- match.arg(arg = .scope)

  t0_ <- Sys.time()

  # Only export documents still needing at least one model - adding a model must not re-export a
  # whole year of text.
  miss_ <- v5_score_missing(
    .path_db     = .path_db,
    .path_scores = .path_scores,
    .years       = .year,
    .models      = .models,
    .limit       = .limit,
    .scope       = .scope
  )
  if (nrow(x = miss_) == 0) {
    cli::cli_alert_success(text = "{(.year)}: already complete")
    return(invisible(x = NULL))
  }

  todo_ <- unique(x = miss_$DocID)

  if (.scope == "climate") {
    ready_ <- v5_score_gate_ready(
      .path_scores = .path_scores,
      .doc_ids     = todo_,
      .gate_model  = .gate_model
    )
    if (ready_ < length(x = todo_)) {
      cli::cli_abort(
        message = c(
          "{(.year)}: scope 'climate' needs {(.gate_model)} first.",
          "x" = "{ready_} of {length(x = todo_)} document{?s} carry it under scope 'all'.",
          "i" = "Run v5_score_build(.models = '{(.gate_model)}', .scope = 'all') first."
        )
      )
    }
  }
  cli::cli_alert_info(
    text = "{(.year)}: {scales::comma(x = length(x = todo_))} document{?s}, \\
            {scales::comma(x = nrow(x = miss_))} document-model pair{?s} outstanding"
  )

  path_pq_  <- file.path(.dir_tmp, sprintf(fmt = "v5_score_%d.parquet", .year))
  path_reg_ <- file.path(.dir_tmp, "v5_score_registry.json")
  on.exit(
    expr = unlink(x = c(path_pq_, path_reg_)),
    add  = TRUE
  )

  jsonlite::write_json(
    x          = v5_models(),   # ONE source of truth for ids and expected labels
    path       = path_reg_,
    auto_unbox = TRUE
  )

  n_par_ <- v5_score_export(
    .path_db     = .path_db,
    .doc_ids     = todo_,
    .path_out    = path_pq_,
    .prose_only  = .prose_only,
    .scope       = .scope,          # "climate" joins to the gate model's positives
    .path_scores = .path_scores,    # attached read-only for that join
    .gate_model  = .gate_model,
    .gate_label  = .gate_label
  )
  cli::cli_alert_info(
    text = "{(.year)}: {scales::comma(x = n_par_)} paragraphs exported (scope {(.scope)})"
  )
  if (n_par_ == 0) {
    cli::cli_alert_warning(text = "{(.year)}: nothing to score - skipping")
    return(invisible(x = NULL))
  }

  v5_score_python(
    .path_in       = path_pq_,
    .path_scores   = .path_scores,
    .path_registry = path_reg_,
    .models        = .models,
    .batch_size    = .batch_size,
    .doc_chunk     = .doc_chunk,
    .device        = .device,
    .fp16          = .fp16,
    .max_length    = .max_length,
    .scope         = .scope        # written to `_scored` so coverage stays honest
  )

  secs_ <- as.numeric(
    x = difftime(time1 = Sys.time(), time2 = t0_, units = "secs")
  )
  cli::cli_alert_success(
    text = "{(.year)}: done in {round(x = secs_ / 60, digits = 1)} minutes"
  )

  invisible(x = list(NDocs = length(x = todo_), NPar = n_par_, Secs = secs_))
}

#' Score everything, year by year, resumable. Argument shape mirrors v5_build.
v5_score_build <- function(.path_db,
                           .path_scores,
                           .years      = NULL,
                           .models     = v5_models()$Model,
                           .limit      = NULL,
                           .batch_size = 64L,
                           .doc_chunk  = 500L,
                           .device     = "auto",
                           .fp16       = TRUE,
                           .max_length = 512L,
                           .prose_only = TRUE,
                           .scope      = c("all", "climate"),
                           .gate_model = "ClDetect",
                           .gate_label = "yes",
                           .force      = FALSE) {

  .scope <- match.arg(arg = .scope)

  cli::cli_h1(text = "V5 model scores")

  v5_score_store_init(.path_scores = .path_scores, .overwrite = .force)

  if (is.null(x = .years)) {
    con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
    .years <- sort(
      x = DBI::dbGetQuery(
        conn      = con_,
        statement = "SELECT DISTINCT filed_year AS Y FROM reports WHERE parsed ORDER BY Y"
      )$Y
    )
    DBI::dbDisconnect(conn = con_, shutdown = TRUE)
  }

  cli::cli_alert_info(
    text = "{length(x = .models)} model{?s} x {length(x = .years)} year{?s}, \\
            scope {(.scope)}: {paste(.models, collapse = ', ')}"
  )
  if (.scope == "climate") {
    cli::cli_alert_info(
      text = "Only paragraphs {(.gate_model)} labelled '{(.gate_label)}' - roughly 12% of the \\
              corpus, so a model costs hours rather than a day."
    )
  }

  t0_ <- Sys.time()
  for (y_ in .years) {
    v5_score_year(
      .year        = y_,
      .path_db     = .path_db,
      .path_scores = .path_scores,
      .models      = .models,
      .limit       = .limit,
      .batch_size  = .batch_size,
      .doc_chunk   = .doc_chunk,
      .device      = .device,
      .fp16        = .fp16,
      .max_length  = .max_length,
      .prose_only  = .prose_only,
      .scope       = .scope,
      .gate_model  = .gate_model,
      .gate_label  = .gate_label
    )
  }

  cli::cli_alert_success(
    text = "All years done in {round(x = as.numeric(x = difftime(
      time1 = Sys.time(), time2 = t0_, units = 'mins')), digits = 1)} minutes"
  )

  invisible(
    x = v5_score_verify(.path_scores = .path_scores, .path_db = .path_db)
  )
}

#' Coverage by model AND scope. What was actually run on what.
#'
#' A model at scope "climate" has seen ~12% of the paragraphs, so its rates are conditional on the
#' gate by construction and must never be pooled with a model scored on everything.
v5_score_coverage <- function(.path_scores) {

  con_ <- v5_connect(.path_db = .path_scores, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT model AS Model, COALESCE(scope, 'all') AS Scope,
             COUNT(*) AS NDocs, SUM(n_par) AS NPar,
             MIN(filed_year) AS YrMin, MAX(filed_year) AS YrMax
        FROM _scored GROUP BY 1, 2 ORDER BY 1, 2"
  ) |>
    tibble::as_tibble()
}

#' Counts, label distributions, truncation, and what is still missing.
v5_score_verify <- function(.path_scores, .path_db = NULL) {

  cli::cli_h1(text = "Verify scores")

  con_ <- v5_connect(.path_db = .path_scores, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  cli::cli_h2(text = "Coverage by model and scope")
  cli::cli_text(
    text = "Scope 'climate' means the model saw only paragraphs the gate called climate-related -
about 12% of the corpus. Its rates are conditional by construction and must not be pooled with a
model scored on everything."
  )
  DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT s.model AS Model, COALESCE(l.scope, 'all') AS Scope,
             COUNT(DISTINCT s.doc_id) AS NDocs, COUNT(*) AS NPar,
             ROUND(100.0 * AVG(CASE WHEN s.truncated THEN 1 ELSE 0 END), 2) AS TruncPct,
             ROUND(MEDIAN(s.n_tok)) AS MedTokens
        FROM scores s
        LEFT JOIN (SELECT DISTINCT model, scope FROM _scored) l ON l.model = s.model
       GROUP BY 1, 2 ORDER BY 1, 2"
  ) |>
    as.data.frame() |>
    print(row.names = FALSE)

  cli::cli_h2(text = "Label distribution - the raw evidence, no recoding")
  cli::cli_text(
    text = "Read the LABELS, not the scores. `score` is confidence in the label CHOSEN, not
P(positive): on netzero-reduction, thresholding the mean correlated -0.17 with the net-zero count
and +0.77 with confidence, i.e. against the construct."
  )
  DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT model AS Model, label AS Label, COUNT(*) AS N,
             ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (PARTITION BY model), 2) AS Pct,
             ROUND(MEDIAN(score), 3) AS MedScore
        FROM scores GROUP BY 1, 2 ORDER BY 1, 3 DESC"
  ) |>
    as.data.frame() |>
    print(row.names = FALSE)

  cli::cli_h2(text = "Label sets AS RUN - compare against Expect")
  DBI::dbGetQuery(
    conn      = con_,
    statement = "SELECT model AS Model, labels AS Labels, expect AS Expect,
                        labels_mapped AS Mapped, device AS Device, fp16 AS Fp16
                   FROM _models ORDER BY 1"
  ) |>
    as.data.frame() |>
    print(row.names = FALSE)

  unmapped_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "SELECT model FROM _models WHERE labels_mapped IS NOT TRUE"
  )$model
  if (length(x = unmapped_) > 0) {
    cli::cli_alert_warning(
      text = "Labels UNMAPPED for {paste(unmapped_, collapse = ', ')} - the head is real but the
config carried no label map, so the stored labels read LABEL_0 / LABEL_1 and mean nothing until
the mapping is taken from the model card. The scores are usable; the label names are not."
    )
  }

  if (!is.null(x = .path_db)) {
    miss_ <- v5_score_missing(.path_db = .path_db, .path_scores = .path_scores)
    if (nrow(x = miss_) > 0) {
      cli::cli_alert_warning(
        text = "{scales::comma(x = nrow(x = miss_))} document-model pair{?s} NOT yet scored"
      )
      miss_ |>
        dplyr::count(FiledYear, Model) |>
        tidyr::pivot_wider(names_from = Model, values_from = n, values_fill = 0L) |>
        as.data.frame() |>
        print(row.names = FALSE)
    } else {
      cli::cli_alert_success(text = "Nothing missing")
    }
  }

  fp_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "SELECT COUNT(*) AS N, COUNT(DISTINCT doc_id) AS D, COUNT(DISTINCT model) AS M
                   FROM scores"
  )
  fp_txt_ <- paste0(
    "rows=", fp_$N, " docs=", fp_$D, " models=", fp_$M,
    " size=", round(x = file.size(.path_scores) / 1e9, digits = 3), "GB",
    " mtime=", format(x = file.mtime(.path_scores), format = "%Y-%m-%dT%H:%M:%S")
  )
  cli::cli_alert_info(text = "Fingerprint: {fp_txt_}")

  invisible(x = list(Fingerprint = fp_txt_))
}



# Term lists ----------------------------------------------------------------------------------------
# Carried verbatim from 002g-GetDisclosures-V6.R.

#' NULL-coalescing, for arguments that default to "derive it from the others".
#' Carried from V6, where it sits beside the term builder that uses it. An infix operator is
#' defined with backticks, which the dependency-closure extraction that built this file did not
#' look for - found on the first v5_terms_build() call, 2026-09-02.
`%|NA|%` <- function(.x, .y) if (is.null(x = .x)) .y else .x

#' Create the term tables. Idempotent.
v5_terms_store_init <- function(.path_terms, .overwrite = FALSE) {

  if (.overwrite && file.exists(.path_terms)) {
    cli::cli_alert_warning(text = "Overwriting {basename(path = .path_terms)}")
    file.remove(.path_terms)
  }

  dir.create(
    path         = dirname(path = .path_terms),
    recursive    = TRUE,
    showWarnings = FALSE
  )

  con_ <- v5_connect(.path_db = .path_terms, .read_only = FALSE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  # The dictionary as loaded. `term` is what the source printed; `form` is what gets searched.
  # Keeping both is how a bracket-expansion error becomes visible instead of a silent zero.
  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE TABLE IF NOT EXISTS terms (
        list_id   VARCHAR,
        term      VARCHAR,     -- as published
        form      VARCHAR,     -- as searched, after bracket expansion
        notation  VARCHAR,
        category  VARCHAR,
        source    VARCHAR,
        construct VARCHAR,
        status    VARCHAR,
        active    BOOLEAN,
        notes     VARCHAR
      )"
  )

  # SPARSE. One row per (doc, par, sen, list, term) with a count, only where the count is non-zero.
  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE TABLE IF NOT EXISTS term_hits (
        doc_id  VARCHAR,
        par_id  INTEGER,
        sen_id  INTEGER,       -- resolved from the match offset, NOT a second search
        list_id VARCHAR,
        form    VARCHAR,       -- the searched form that matched
        n       INTEGER
      )"
  )

  # Per-list ledger AND cache key. A list is complete for a year only if its hash still matches.
  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE TABLE IF NOT EXISTS _flagged (
        list_id    VARCHAR,
        filed_year INTEGER,
        list_hash  VARCHAR,    -- hash of the expanded forms; changes invalidate this list only
        n_forms    INTEGER,
        n_docs     INTEGER,
        n_par      INTEGER,
        n_hits     BIGINT,
        secs       DOUBLE,
        prose_only BOOLEAN,    -- WHICH PARAGRAPHS WERE SEARCHED; see below
        flagged_at TIMESTAMP,
        PRIMARY KEY (list_id, filed_year)
      )"
  )

  # `prose_only` is provenance, not a cache key. Flagging one list over tables as well as prose -
  # which is the right thing for `states`, since Item 2 Properties TABULATES facilities by state -
  # makes that list incomparable with the other 23, and nothing in the store recorded it. It is
  # deliberately NOT part of the completeness test: recording the mix makes it visible, whereas
  # invalidating on it would silently re-flag 23 lists the first time anyone changed the argument.
  # Stores written before this column existed backfill NULL, which reads as "unknown".
  DBI::dbExecute(
    conn      = con_,
    statement = "ALTER TABLE _flagged ADD COLUMN IF NOT EXISTS prose_only BOOLEAN"
  )

  cli::cli_alert_success(text = "Term store ready: {basename(path = .path_terms)}")
  invisible(x = .path_terms)
}

#' Expand optional-suffix notation into the forms that will actually be searched.
#'
#' BOTH BRACKETS AND PARENTHESES. The notation column says `bracket_plural`, but kww23 is
#' transcribed with PARENTHESES - "CHANGING CLIMATE(S)", "CLIMATE CHANGE LEGISLATION(S)". An
#' earlier version looked only for "[", found none, returned the term unchanged, and the escaper
#' then searched a literal "(S)". That is why 41 of kww23's 64 forms never fired.
#'
#' THREE SHAPES, and anything else is refused rather than half-expanded:
#'   TERM            -> TERM
#'   TERM(S)         -> TERM, TERMS                       one optional group
#'   STEM(Y) (IES)   -> STEMY, STEMIES                    two groups = ALTERNATIVES on a stem,
#'                                                        which is how kww23 writes REGISTR(Y) (IES)
v5_terms_expand <- function(.term) {

  n_grp_ <- stringi::stri_count_regex(str = .term, pattern = "[\\[(][^\\])]+[\\])]")

  if (n_grp_ == 0L) return(list(.term))

  if (n_grp_ == 1L) {
    m_ <- stringi::stri_match_first_regex(
      str     = .term,
      pattern = "^(.*?)[\\[(]([^\\])]+)[\\])](.*)$"   # prefix, optional part, suffix
    )
    if (is.na(x = m_[, 1])) return(list(.term))
    return(list(trimws(x = c(
      paste0(m_[, 2], m_[, 4]),            # without the optional part
      paste0(m_[, 2], m_[, 3], m_[, 4])    # with it
    ))))
  }

  if (n_grp_ == 2L) {
    m_ <- stringi::stri_match_first_regex(
      str     = .term,
      pattern = "^(.*?)[\\[(]([^\\])]+)[\\])]\\s*[\\[(]([^\\])]+)[\\])]\\s*$"
    )
    if (is.na(x = m_[, 1])) return(list(.term))
    return(list(trimws(x = c(
      paste0(m_[, 2], m_[, 3]),    # stem + first alternative
      paste0(m_[, 2], m_[, 4])     # stem + second
    ))))
  }

  cli::cli_abort(
    message = c(
      "Term {(.term)} has {n_grp_} optional groups.",
      "i" = "One or two are supported. Expand it in the CSV or extend v5_terms_expand()."
    )
  )
}

#' Load the term CSVs into the store.
#'
#' Expects the V4 schema: list_id, term, notation, category, source, construct, status, active,
#' notes. Rows with active = FALSE are loaded but never searched - keeping them means a list's
#' history is in the store rather than in a git diff.
v5_terms_load <- function(.path_terms, .files, .replace = TRUE) {

  # NO FILES is its own failure and must say so. Mapping over an empty vector yields an empty list,
  # list_rbind yields a ZERO-COLUMN tibble, and the schema check downstream then reports every
  # column as missing - which points at the CSV contents when the problem is the path.
  if (length(x = .files) == 0) {
    cli::cli_abort(
      message = c(
        "No term CSVs supplied.",
        "i" = "Locate them with: list.files(path = here::here(), \
               pattern = '^pfire_terms.*[.]csv$', recursive = TRUE, full.names = TRUE)"
      )
    )
  }
  missing_ <- .files[!file.exists(.files)]
  if (length(x = missing_) > 0) {
    cli::cli_abort(message = "File{?s} not found: {paste(missing_, collapse = ', ')}")
  }

  raw_ <- purrr::map(
    .x = .files,
    .f = \(.f) {
      d_ <- readr::read_csv(file = .f, show_col_types = FALSE, progress = FALSE)
      cli::cli_alert_info(text = "{basename(path = .f)}: {nrow(x = d_)} row{?s}")
      d_
    }
  ) |>
    purrr::list_rbind()

  need_ <- c("list_id", "term", "notation", "category", "source", "construct",
             "status", "active", "notes")
  miss_ <- setdiff(x = need_, y = names(x = raw_))
  if (length(x = miss_) > 0) {
    cli::cli_abort(
      message = c(
        "Term CSVs are missing column{?s}: {paste(miss_, collapse = ', ')}",
        "i" = "Found: {paste(names(x = raw_), collapse = ', ')}"
      )
    )
  }

  out_ <- raw_ |>
    dplyr::mutate(
      active = as.logical(x = active),
      Forms  = purrr::map(.x = term, .f = v5_terms_expand)
    ) |>
    tidyr::unnest_longer(col = Forms) |>
    tidyr::unnest_longer(col = Forms, values_to = "form") |>
    dplyr::mutate(form = toupper(x = trimws(x = form))) |>
    dplyr::distinct(list_id, term, form, .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(x = need_), form)

  con_ <- v5_connect(.path_db = .path_terms, .read_only = FALSE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )
  if (.replace) DBI::dbExecute(conn = con_, statement = "DELETE FROM terms")
  DBI::dbAppendTable(
    conn  = con_,
    name  = "terms",
    value = dplyr::select(.data = out_, list_id, term, form, notation, category,
                          source, construct, status, active, notes)
  )

  summ_ <- out_ |>
    dplyr::group_by(list_id) |>
    dplyr::summarise(
      NTerms  = dplyr::n_distinct(term),
      NForms  = dplyr::n_distinct(form),
      NActive = dplyr::n_distinct(form[active]),
      .groups = "drop"
    )

  cli::cli_alert_success(
    text = "{nrow(x = out_)} form{?s} from {dplyr::n_distinct(out_$term)} term{?s} \\
            across {nrow(x = summ_)} list{?s}"
  )
  print(x = as.data.frame(x = summ_), row.names = FALSE)

  invisible(x = summ_)
}

#' Add the 50 US states as a term list.
#'
#' GeoDispersion IS a term list - 50 forms, word-bounded, restricted to items. An earlier version
#' gave it a bespoke regex pass inside the panel build, which meant a full scan of 55.6M paragraphs
#' every time the panel was assembled, with no cache and no parallelism. Here it inherits all
#' three, and v5_agg_geo becomes a GROUP BY.
#'
#' Run AFTER v5_terms_load, which replaces the table from the CSVs and would otherwise drop these.
v5_terms_add_states <- function(.path_terms, .states = datasets::state.name,
                                .list_id = "states") {

  con_ <- v5_connect(.path_db = .path_terms, .read_only = FALSE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(fmt = "DELETE FROM terms WHERE list_id = '%s'", .list_id)
  )

  DBI::dbAppendTable(
    conn  = con_,
    name  = "terms",
    value = tibble::tibble(
      list_id   = .list_id,
      term      = .states,
      form      = toupper(x = .states),
      notation  = "plain",
      category  = "state",
      source    = "datasets::state.name",
      construct = "GeoDispersion",
      status    = "active",
      active    = TRUE,
      notes     = "50 US states; GeoDispersion is DISTINCT states over items 1, 2, 6, 7 / 50"
    )
  )

  cli::cli_alert_success(
    text = "{sprintf(fmt = '%d states added as list %s', length(x = .states), .list_id)}"
  )
  invisible(x = .list_id)
}

#' One alternation per list, word-bounded, plus a content hash.
#'
#' \\b...\\b is the whole point. `D&I` matched "BOARD IS" across 20% of the corpus and unanchored
#' `GRI` matched INTEGRITY, AGRICULTURAL, GRILL and GRID; 46% of V1's ESG sentences were substring
#' false positives.
#'
#' Forms are regex-escaped and sorted LONGEST FIRST: alternation is first-match-wins in RE2, so
#' "GREENHOUSE GAS" must precede "GAS" or the longer form never matches.
#'
#' The hash covers the sorted forms, so it changes when a list changes and only then - which is
#' what lets one list be re-flagged without touching the other 22.
v5_terms_patterns <- function(.path_terms, .lists = NULL) {

  con_ <- v5_connect(.path_db = .path_terms, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  tm_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "SELECT list_id, form FROM terms WHERE active"
  ) |>
    tibble::as_tibble()

  if (!is.null(x = .lists)) tm_ <- dplyr::filter(.data = tm_, list_id %in% .lists)
  if (nrow(x = tm_) == 0) cli::cli_abort(message = "No active terms found")

  tm_ |>
    dplyr::distinct(list_id, form) |>
    dplyr::group_by(list_id) |>
    dplyr::summarise(
      NForms  = dplyr::n(),
      Forms   = list(sort(x = form)),
      Pattern = paste0(
        "\\b(",
        paste(
          stringi::stri_replace_all_regex(
            str         = form[order(-nchar(x = form))],   # longest first: RE2 is first-match-wins
            pattern     = "([.\\\\^$|?*+()\\[\\]{}])",
            replacement = "\\\\$1"
          ),
          collapse = "|"
        ),
        ")\\b"
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Hash = purrr::map_chr(
        .x = Forms,
        .f = \(.f) substr(x = digest::digest(object = .f, algo = "xxhash64"), start = 1, stop = 12)
      )
    )
}

#' Flag one batch of paragraphs against one list's pattern.
#'
#' Returns sparse counts at SENTENCE grain. The sentence is resolved from the match OFFSET against
#' the sentence spans already in the store - not by searching the sentence text again.
#'
#' `stri_locate_all_regex` gives positions; `stri_sub` on those positions gives which form matched.
#' Case is folded by searching upper-cased text against upper-cased forms, which is why the loaded
#' forms are upper-cased at load time.
v5_terms_flag_batch <- function(.par, .sen, .list_id, .pattern, .forms = NULL) {

  # stri_trans_toupper, NOT base toupper.
  #
  # Measured 2026-08-25: the stored forms came back SHIFTED RIGHT BY ONE - OPERATIONS as
  # PERATIONS, SALES as ALES, EPA as PA, and variants carrying the following punctuation
  # (PERATIONS. PERATIONS: PERATIONS,). Start+1 and end+1, uniformly.
  #
  # The arithmetic here is right: stri_locate_all_regex and stri_sub both index by code point. So
  # the disagreement is in the STRING. base::toupper is locale-dependent and does not share
  # stringi's UTF-8 index space, so a paragraph containing any multi-byte character - a curly quote
  # or dash v5_ent did not decode, a degree sign, an accented name - can shift every subsequent
  # position by one. That fits: a minority of hits, in a corpus that is mostly ASCII.
  #
  # This is a diagnosis, not a proof. `.forms` below is the check that makes the next run
  # decisive rather than another aggregate to squint at.
  txt_ <- stringi::stri_trans_toupper(str = .par$text)

  loc_ <- stringi::stri_locate_all_regex(
    str          = txt_,
    pattern      = .pattern,
    omit_no_match = TRUE
  )

  n_hit_ <- purrr::map_int(.x = loc_, .f = \(.m) if (is.null(x = .m)) 0L else nrow(x = .m))
  if (sum(n_hit_) == 0L) {
    empty_ <- tibble::tibble(doc_id = character(length = 0), par_id = integer(length = 0),
                             sen_id = integer(length = 0), list_id = character(length = 0),
                             form = character(length = 0), n = integer(length = 0))
    attr(x = empty_, which = "NLost") <- 0L
    attr(x = empty_, which = "NBad")  <- 0L
    return(empty_)
  }

  idx_ <- rep(x = seq_along(n_hit_), times = n_hit_)
  st_  <- unlist(x = purrr::map(.x = loc_, .f = \(.m) if (is.null(x = .m)) integer() else .m[, 1]))
  en_  <- unlist(x = purrr::map(.x = loc_, .f = \(.m) if (is.null(x = .m)) integer() else .m[, 2]))

  hits_ <- tibble::tibble(
    doc_id = .par$doc_id[idx_],
    par_id = .par$par_id[idx_],
    Start  = as.integer(x = st_),
    form   = stringi::stri_sub(str = txt_[idx_], from = st_, to = en_)
  )

  # EVERY EXTRACTED FORM MUST BE ONE OF THE PATTERN'S ALTERNATIVES. It cannot be anything else
  # unless the extraction and the search disagree about position - which is exactly the failure
  # that produced PERATIONS from OPERATIONS. Counting it here turns a silent corruption into a
  # number the caller reports, on the first year rather than after all 23 lists.
  n_bad_ <- 0L
  if (!is.null(x = .forms)) {
    # UNLIST. `Forms` is a list-COLUMN, so as.list(jobs_[i, ])$Forms is a list containing one
    # character vector. `%in%` against that is uniformly FALSE, which reported 100% of hits as bad
    # while the extracted forms were visibly correct - the check was wrong, not the extraction.
    forms_  <- unlist(x = .forms, use.names = FALSE)
    n_bad_  <- sum(!(hits_$form %in% forms_))
  }

  # SENTENCE FROM OFFSET. A non-equi join on the spans already stored: the match at Start belongs
  # to the sentence whose char_start..char_end contains it. This is the step that makes a second
  # search unnecessary and guarantees the paragraph total equals the sum of its sentences.
  #
  # A multi-word term straddling a sentence boundary - "GREENHOUSE GAS" with a period between - is
  # still found, because the SEARCH runs over paragraph text, and is attributed to the sentence
  # containing its start. Searching sentence strings directly would miss it.
  joined_ <- hits_ |>
    dplyr::inner_join(
      y  = dplyr::select(.data = .sen, doc_id, par_id, sen_id, char_start, char_end),
      by = dplyr::join_by(doc_id, par_id, between(Start, char_start, char_end))
    )

  # AN INNER JOIN DROPS SILENTLY. If a match lands outside every stored sentence span the hit
  # vanishes and the count under-reports with no error - the class of failure this project has
  # already paid for twice. Sentence spans should tile a prose paragraph, so a non-zero count is a
  # defect in segmentation or in the offsets, not a curiosity.
  #
  # RETURNED AS AN ATTRIBUTE rather than printed: this runs inside a daemon, where a cli message
  # goes nowhere. The caller accumulates it and reports once.
  out_ <- joined_ |>
    dplyr::count(doc_id, par_id, sen_id, form, name = "n") |>
    dplyr::mutate(list_id = .list_id, n = as.integer(x = n)) |>
    dplyr::select(doc_id, par_id, sen_id, list_id, form, n)

  attr(x = out_, which = "NLost") <- nrow(x = hits_) - nrow(x = joined_)
  attr(x = out_, which = "NBad")  <- n_bad_
  out_
}

#' Which (list, year) pairs still need flagging, hash-aware.
#'
#' A list whose hash has changed is NOT done, however recently it ran. That is the whole reason the
#' hash is stored: editing one list invalidates one list.
v5_terms_missing <- function(.path_db, .path_terms, .lists = NULL, .years = NULL) {

  pat_ <- v5_terms_patterns(.path_terms = .path_terms, .lists = .lists)

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  yrs_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "SELECT DISTINCT filed_year AS FiledYear FROM reports WHERE parsed ORDER BY 1"
  )$FiledYear
  DBI::dbDisconnect(conn = con_, shutdown = TRUE)
  if (!is.null(x = .years)) yrs_ <- intersect(x = yrs_, y = .years)

  con2_ <- v5_connect(.path_db = .path_terms, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con2_, shutdown = TRUE),
    add  = TRUE
  )
  done_ <- DBI::dbGetQuery(
    conn      = con2_,
    statement = "SELECT list_id AS ListId, filed_year AS FiledYear, list_hash AS Hash
                   FROM _flagged"
  ) |>
    tibble::as_tibble()

  tidyr::expand_grid(
    dplyr::select(.data = pat_, ListId = list_id, Hash),
    FiledYear = yrs_
  ) |>
    dplyr::anti_join(y = done_, by = c("ListId", "FiledYear", "Hash"))
}

#' Flag ONE (list, year): read paragraphs, match, resolve sentences, return sparse counts.
#'
#' PURE, and passed BY NAME to mirai_map. It opens its own read-only connection to the paragraph
#' store and returns only counts - a few thousand rows - so no filing text crosses the task queue.
#' An anonymous wrapper would carry its defining frame and ship the paragraphs with it.
#'
#' It does NOT write. DuckDB is single-writer, so twelve daemons writing to the term store would
#' collide; the parent writes what comes back.
v5_terms_task <- function(.job, .path_db, .prose_only = TRUE, .doc_chunk = 250L,
                          .memory = "2GB", .threads = 2L) {

  con_ <- v5_connect(
    .path_db   = .path_db,
    .read_only = TRUE,
    .memory    = .memory,    # per INSTANCE, and every daemon has one
    .threads   = .threads    # otherwise each instance claims every core
  )
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  docs_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "SELECT doc_id FROM reports WHERE parsed AND filed_year = %d ORDER BY doc_id",
      .job$FiledYear
    )
  )$doc_id

  if (length(x = docs_) == 0) {
    return(list(
      Job  = .job,
      Hits = tibble::tibble(doc_id = character(length = 0), par_id = integer(length = 0),
                            sen_id = integer(length = 0), list_id = character(length = 0),
                            form = character(length = 0), n = integer(length = 0)),
      NDocs = 0L, NPar = 0L, NLost = 0L, NBad = 0L, Secs = 0
    ))
  }

  # SMALLER CHUNKS THAN THE SERIAL VERSION. Twelve workers each holding 1.5M paragraphs of text is
  # the 12 GB-per-daemon problem again; 500 documents bounds it to something the machine can hold
  # twelve times over.
  chunks_ <- unname(
    obj = split(x = docs_, f = ceiling(x = seq_along(along.with = docs_) / .doc_chunk))
  )

  t0_    <- Sys.time()
  out_   <- vector(mode = "list", length = length(x = chunks_))
  n_par_ <- 0L
  n_lost_ <- 0L
  n_bad_  <- 0L

  for (i_ in seq_along(along.with = chunks_)) {

    ids_ <- paste(sprintf(fmt = "'%s'", chunks_[[i_]]), collapse = ", ")

    par_ <- DBI::dbGetQuery(
      conn      = con_,
      statement = sprintf(
        fmt = "SELECT doc_id, par_id, text FROM paragraphs
                WHERE doc_id IN (%s)%s ORDER BY doc_id, par_id",
        ids_, if (.prose_only) " AND is_prose" else ""
      )
    ) |>
      tibble::as_tibble()

    if (nrow(x = par_) == 0) next

    sen_ <- DBI::dbGetQuery(
      conn      = con_,
      statement = sprintf(
        fmt = "SELECT doc_id, par_id, sen_id, char_start, char_end FROM sentences
                WHERE doc_id IN (%s)", ids_
      )
    ) |>
      tibble::as_tibble()

    res_ <- v5_terms_flag_batch(
      .par     = par_,
      .sen     = sen_,
      .list_id = .job$ListId,
      .pattern = .job$Pattern,
      .forms   = .job$Forms      # the alternatives; anything else means an offset failure
    )

    out_[[i_]] <- res_
    n_par_  <- n_par_ + nrow(x = par_)
    n_lost_ <- n_lost_ + (attr(x = res_, which = "NLost") %|NA|% 0L)
    n_bad_  <- n_bad_  + (attr(x = res_, which = "NBad")  %|NA|% 0L)

    # v5_terms_flag_batch upper-cases the text, so a chunk is briefly resident twice. Dropping the
    # references and collecting here keeps peak to one chunk rather than growing across the loop.
    rm(par_, sen_, res_)
    gc(verbose = FALSE, full = FALSE)
  }

  list(
    Job   = .job,
    Hits  = purrr::list_rbind(x = purrr::compact(.x = out_)),
    NDocs = length(x = docs_),
    NPar  = n_par_,
    NLost = n_lost_,
    NBad  = n_bad_,
    Secs  = as.numeric(x = difftime(time1 = Sys.time(), time2 = t0_, units = "secs"))
  )
}

#' Write one completed (list, year) into the term store.
#'
#' Replace, not append: a re-flag after a list edit must not add to the old counts.
#'
#' ATTACH IS PER-CONNECTION. An earlier version attached the paragraph store once in
#' v5_terms_build, on a connection that then disconnected - so this function opened its own and
#' found no schema `par`. Attaching here means the lifetime of the attach matches the lifetime of
#' the connection that uses it.
v5_terms_write <- function(.path_terms, .path_db, .result, .prose_only = NA) {

  job_ <- .result$Job

  con_ <- v5_connect(.path_db = .path_terms, .read_only = FALSE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(fmt = "ATTACH IF NOT EXISTS '%s' AS par (READ_ONLY)", .path_db)
  )

  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(
      fmt = "DELETE FROM term_hits WHERE list_id = '%s' AND doc_id IN
               (SELECT doc_id FROM par.reports WHERE filed_year = %d)",
      job_$ListId, job_$FiledYear
    )
  )

  if (nrow(x = .result$Hits) > 0) {
    DBI::dbAppendTable(conn = con_, name = "term_hits", value = .result$Hits)
  }

  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(
      fmt = "DELETE FROM _flagged WHERE list_id = '%s' AND filed_year = %d",
      job_$ListId, job_$FiledYear
    )
  )
  DBI::dbAppendTable(
    conn  = con_,
    name  = "_flagged",
    value = tibble::tibble(
      list_id    = job_$ListId,
      filed_year = job_$FiledYear,
      list_hash  = job_$Hash,
      n_forms    = as.integer(x = job_$NForms),
      n_docs     = as.integer(x = .result$NDocs),
      n_par      = as.integer(x = .result$NPar),
      n_hits     = as.numeric(x = sum(.result$Hits$n)),
      secs       = .result$Secs,
      prose_only = as.logical(x = .prose_only),
      flagged_at = Sys.time()
    )
  )

  invisible(x = NULL)
}

#' Ship the flagging functions into the daemons.
v5_terms_export_to_daemons <- function() {
  mirai::everywhere(
    .expr = {
      for (nm in names(x = objs_)) assign(x = nm, value = objs_[[nm]], envir = globalenv())
    },
    objs_ = mget(
      x     = c("v5_connect", "v5_terms_flag_batch", "v5_terms_task", "%|NA|%"),
      envir = globalenv()
    )
  )
  invisible(x = NULL)
}

#' Flag every outstanding (list, year) in parallel. Resumable, hash-aware.
#'
#' WHY PARALLEL, AND WHY OVER THE GRID
#' Measured 2026-08-24: kww23 (64 forms) ran at ~24,000 paragraphs/second, lin24_climate_change
#' (larger alternation) at ~6,000. Same paragraphs, same reads - the cost is the REGEX, so reading
#' the text once for all lists would buy little. What does buy something is that the 616
#' (list, year) pairs are independent: each reads its own paragraphs and returns a few thousand
#' rows. At one worker this was ~47 hours.
#'
#' THE PARENT WRITES. DuckDB is single-writer, so daemons return counts and the parent commits them
#' one job at a time - which also makes an interrupt cost one (list, year) rather than a batch.
v5_terms_build <- function(.path_db,
                           .path_terms,
                           .lists       = NULL,
                           .years       = NULL,
                           .prose_only  = TRUE,
                           .doc_chunk   = 250L,
                           .workers     = 8L,
                           .mem_worker  = "2GB",
                           .thr_worker  = 2L,
                           .inflight    = NULL,
                           .force       = FALSE) {

  cli::cli_h1(text = "V5 term flags")

  v5_terms_store_init(.path_terms = .path_terms, .overwrite = .force)

  pat_ <- v5_terms_patterns(.path_terms = .path_terms, .lists = .lists)
  cli::cli_alert_info(
    text = "{nrow(x = pat_)} list{?s}, {sum(pat_$NForms)} active form{?s}"
  )

  todo_ <- v5_terms_missing(
    .path_db    = .path_db,
    .path_terms = .path_terms,
    .lists      = .lists,
    .years      = .years
  )
  if (nrow(x = todo_) == 0) {
    cli::cli_alert_success(text = "Nothing to do")
    return(invisible(x = NULL))
  }

  jobs_ <- todo_ |>
    dplyr::left_join(
      y  = dplyr::select(.data = pat_, ListId = list_id, Pattern, NForms, Forms),
      by = "ListId"
    ) |>
    dplyr::arrange(FiledYear, ListId)

  cli::cli_alert_info(
    text = "{nrow(x = jobs_)} list-year pair{?s} across \\
            {dplyr::n_distinct(jobs_$ListId)} list{?s}"
  )

  use_par_ <- .workers > 1L && requireNamespace(package = "mirai", quietly = TRUE)
  if (use_par_) {
    try(expr = mirai::daemons(n = 0), silent = TRUE)
    mirai::daemons(n = .workers)
    on.exit(
      expr = try(expr = mirai::daemons(n = 0), silent = TRUE),
      add  = TRUE
    )
    v5_terms_export_to_daemons()
    cli::cli_alert_info(
      text = "{sprintf(fmt = '%d daemons, %s and %d threads each, %d documents per chunk',
                       .workers, .mem_worker, .thr_worker, .doc_chunk)}"
    )
  }

  # WAVES. mirai_map dispatches every task the moment it is called, so a single map over 616 jobs
  # would queue them all at once. Collecting a wave at a time also means results are WRITTEN as
  # they arrive rather than at the end, so an interrupt keeps everything already committed.
  inflight_ <- if (is.null(x = .inflight)) max(1L, .workers * 2L) else .inflight
  waves_    <- split(
    x = seq_len(length.out = nrow(x = jobs_)),
    f = ceiling(x = seq_len(length.out = nrow(x = jobs_)) / inflight_)
  )

  t0_    <- Sys.time()
  n_done_ <- 0L
  n_lost_ <- 0L
  n_bad_  <- 0L

  for (w_ in seq_along(along.with = waves_)) {

    idx_ <- waves_[[w_]]
    sub_ <- purrr::map(.x = idx_, .f = \(.i) as.list(x = jobs_[.i, ]))

    res_ <- if (use_par_) {
      mirai::mirai_map(
        .x    = sub_,
        .f    = v5_terms_task,
        .args = list(.path_db = .path_db, .prose_only = .prose_only, .doc_chunk = .doc_chunk,
                     .memory = .mem_worker, .threads = .thr_worker)
      )[]
    } else {
      purrr::map(
        .x = sub_,
        .f = \(.j) v5_terms_task(.job = .j, .path_db = .path_db,
                                 .prose_only = .prose_only, .doc_chunk = .doc_chunk,
                                 .memory = .mem_worker, .threads = .thr_worker)
      )
    }

    bad_ <- purrr::map_lgl(
      .x = res_,
      .f = \(.x) inherits(x = .x, what = "miraiError") || inherits(x = .x, what = "errorValue")
    )
    if (any(bad_)) {
      cli::cli_alert_danger(text = "{sum(bad_)} job{?s} failed in wave {w_}. First:")
      print(x = res_[bad_][[1]])
    }

    for (r_ in res_[!bad_]) {
      v5_terms_write(.path_terms = .path_terms, .path_db = .path_db, .result = r_,
                     .prose_only = .prose_only)
      n_done_ <- n_done_ + 1L
      n_lost_ <- n_lost_ + r_$NLost
      n_bad_  <- n_bad_  + r_$NBad
    }

    el_  <- as.numeric(x = difftime(time1 = Sys.time(), time2 = t0_, units = "secs"))
    eta_ <- el_ / max(n_done_, 1L) * (nrow(x = jobs_) - n_done_)
    cli::cli_alert_info(
      text = "{sprintf(fmt = '%d/%d jobs | %s hits | %.1f min elapsed | eta %.1f min',
                       n_done_, nrow(x = jobs_),
                       scales::comma(x = sum(purrr::map_dbl(.x = res_[!bad_],
                                                            .f = \\(.r) sum(.r$Hits$n)))),
                       el_ / 60, eta_ / 60)}"
    )
  }

  if (n_lost_ > 0) {
    cli::cli_alert_warning(
      text = "{sprintf(fmt = '%s matches fell outside every sentence span across the run',
                       scales::comma(x = n_lost_))}"
    )
  }
  if (n_bad_ > 0) {
    cli::cli_alert_danger(
      text = "{sprintf(fmt = '%s extracted forms are NOT in the pattern - the search and the
extraction disagree about position. Hit counts and DeadPct are unreliable until this is zero.',
                       scales::comma(x = n_bad_))}"
    )
  } else {
    cli::cli_alert_success(text = "Every extracted form is a pattern alternative")
  }

  cli::cli_alert_success(
    text = "{sprintf(fmt = 'Done in %.1f minutes',
                     as.numeric(x = difftime(time1 = Sys.time(), time2 = t0_,
                                             units = 'mins')))}"
  )

  invisible(x = v5_terms_verify(.path_terms = .path_terms, .path_db = .path_db))
}

#' Per-list coverage, dead terms, word share, and a sample of what actually matched.
#'
#' THE THREE THINGS WORTH READING
#'   DeadPct   forms that never fired. A high share is usually a transcription or expansion error,
#'             not a corpus fact - kww23's bracket rows match NOTHING unexpanded.
#'   PctWords  hits per 100 words. bbk20 reports 4.0% of words in sustainability reports; V4
#'             measured 0.0242% on 10-Ks. A benchmark to argue with, not to match.
#'   Sample    the matched text itself. A boundary failure looks like a plausible count and an
#'             implausible sample - `GRI` hitting INTEGRITY was invisible in the aggregate.
v5_terms_verify <- function(.path_terms, .path_db = NULL, .n_sample = 12L) {

  cli::cli_h1(text = "Verify term flags")

  con_ <- v5_connect(.path_db = .path_terms, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  cli::cli_h2(text = "Coverage by list")
  cov_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "
      WITH act AS (
        SELECT list_id, COUNT(DISTINCT form) AS n_forms FROM terms WHERE active GROUP BY 1
      ), fired AS (
        SELECT list_id, COUNT(DISTINCT form) AS n_fired, SUM(n) AS n_hits,
               COUNT(DISTINCT doc_id) AS n_docs
          FROM term_hits GROUP BY 1
      ), led AS (
        SELECT list_id, SUM(n_par) AS n_par, COUNT(*) AS n_years, SUM(secs) AS secs,
               CASE WHEN COUNT(DISTINCT prose_only) > 1 THEN 'MIXED'
                    WHEN MAX(prose_only) THEN 'prose'
                    WHEN MAX(prose_only) IS NULL THEN '?'
                    ELSE 'all' END AS searched
          FROM _flagged GROUP BY 1
      )
      SELECT a.list_id AS List, a.n_forms AS NForms,
             COALESCE(f.n_fired, 0) AS NFired,
             ROUND(100.0 * (a.n_forms - COALESCE(f.n_fired, 0)) / a.n_forms, 1) AS DeadPct,
             COALESCE(f.n_hits, 0) AS NHits,
             COALESCE(f.n_docs, 0) AS NDocs,
             l.n_years AS NYears,
             l.searched AS Searched,
             ROUND(l.secs / 60.0, 1) AS Mins
        FROM act a
        LEFT JOIN fired f ON f.list_id = a.list_id
        LEFT JOIN led   l ON l.list_id = a.list_id
       ORDER BY NHits DESC"
  ) |>
    tibble::as_tibble()
  print(x = as.data.frame(x = cov_), row.names = FALSE)

  # Word share needs the paragraph store for the denominator.
  if (!is.null(x = .path_db)) {
    cli::cli_h2(text = "Hits per 100 words")
    DBI::dbExecute(
      conn      = con_,
      statement = sprintf(fmt = "ATTACH '%s' AS par (READ_ONLY)", .path_db)
    )
    DBI::dbGetQuery(
      conn      = con_,
      statement = "
        WITH w AS (SELECT SUM(n_words) AS n_words FROM par.paragraphs WHERE is_prose)
        SELECT h.list_id AS List, SUM(h.n) AS NHits,
               ROUND(100.0 * SUM(h.n) / (SELECT n_words FROM w), 4) AS PctWords
          FROM term_hits h GROUP BY 1 ORDER BY 2 DESC"
    ) |>
      as.data.frame() |>
      print(row.names = FALSE)
    DBI::dbExecute(conn = con_, statement = "DETACH par")
  }

  cli::cli_h2(text = "Top forms per list")
  DBI::dbGetQuery(
    conn      = con_,
    statement = "
      WITH r AS (
        SELECT list_id, form, SUM(n) AS n,
               ROW_NUMBER() OVER (PARTITION BY list_id ORDER BY SUM(n) DESC) AS rn
          FROM term_hits GROUP BY 1, 2
      )
      SELECT list_id AS List, form AS Form, n AS N FROM r WHERE rn <= 5 ORDER BY List, N DESC"
  ) |>
    as.data.frame() |>
    print(row.names = FALSE)

  cli::cli_h2(text = "Dead forms - transcription errors hide here")
  dead_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT t.list_id AS List, t.form AS Form
        FROM (SELECT DISTINCT list_id, form FROM terms WHERE active) t
        LEFT JOIN (SELECT DISTINCT list_id, form FROM term_hits) h
          ON h.list_id = t.list_id AND h.form = t.form
       WHERE h.form IS NULL ORDER BY 1, 2"
  ) |>
    tibble::as_tibble()
  if (nrow(x = dead_) > 0) {
    dead_ |>
      dplyr::group_by(List) |>
      dplyr::summarise(
        NDead   = dplyr::n(),
        Examples = paste(utils::head(x = Form, n = 6L), collapse = ", "),
        .groups = "drop"
      ) |>
      as.data.frame() |>
      print(row.names = FALSE)
  } else {
    cli::cli_alert_success(text = "Every active form fired at least once")
  }

  cli::cli_h2(text = "Reading this")
  cli::cli_ul(
    items = c(
      "DeadPct high means transcription or expansion, not a corpus fact - kww23 has 47 of 64 rows
       in bracket notation and they match NOTHING unexpanded.",
      "PctWords against bbk20's published 4.0% for sustainability reports. V4 measured 0.0242% on
       10-Ks; a benchmark to argue with, not to match.",
      "Top forms is the boundary check. A count can look plausible while the sample is nonsense -
       `GRI` matching INTEGRITY and GRID was invisible in the aggregate and obvious in a sample.",
      "Nothing here is a measure. Paragraph, item and firm-year counts are GROUP BYs over
       term_hits, and n_words already sits on paragraphs and sentences."
    )
  )

  invisible(x = list(Coverage = cov_, Dead = dead_))
}



# Specification -------------------------------------------------------------------------------------
# Carried verbatim from 002g-GetDisclosures-V6.R.

#' Items to report separately. Everything else aggregates only at document level.
#'
#' 1 Business and 1A Risk Factors are where the measured fire effect lives (0.0185*** in 1A against
#' 0.0007 in Item 1); 7 is MD&A and 8 the financial statements.
.V5_ITEMS <- c("1", "1A", "2", "3", "7", "7A", "8")

#' GeoDispersion's item scope, verbatim from V3: Business, Properties, Selected Financial Data,
#' MD&A. The measure is a DISTINCT state count, which is why one document per firm-year is what
#' bounds it at 50.
.V5_GEO_ITEMS <- c("1", "2", "6", "7")



# Aggregation and panel -----------------------------------------------------------------------------
# Carried verbatim from 002g-GetDisclosures-V6.R.

#' NA every item-scoped COUNT on a document where that item was not located.
#'
#' THIS IS THE DEFECT V6 EXISTS TO FIX. v5_fill_block() coalesces a block's columns to 0 whenever
#' the document flag is TRUE, which cannot distinguish "we searched Item 1A and found no climate
#' term" from "we never found Item 1A". The first is a measured zero and belongs in the
#' regression; the second is the GeoDispersion zero-fill artefact wearing a different column name.
#'
#' SHARES AND RATES ARE NOT TOUCHED and must not be. `p...` already guards on `Base > 0`, `r...`
#' on `nWords_I* > 0`, `pNum*` on `nSenESG_I* > 0` - all three are NA already wherever the item is
#' missing, and running the mask over them would be a second definition of the same rule.
#'
#' Suffix matching is EXACT, which is what keeps `_I1` off `nWords_I1A`: "_I1" is not a suffix of
#' "nWords_I1A", whose suffix is "_I1A". Same for `_I7` against `_I7A`. Every column therefore
#' matches at most one item and the loop cannot double-mask.
v6_mask_items <- function(.tab, .cols, .items = .V5_ITEMS) {

  hit_n_ <- 0L

  for (it_ in .items) {

    flag_ <- paste0("HasI", it_)
    if (!flag_ %in% names(x = .tab)) next

    sfx_  <- paste0("_I", it_)
    cols_ <- intersect(
      x = .cols[stringi::stri_endswith_fixed(str = .cols, pattern = sfx_)],
      y = names(x = .tab)
    )
    if (length(x = cols_) == 0) next

    keep_  <- .tab[[flag_]]
    .tab   <- dplyr::mutate(
      .data = .tab,
      dplyr::across(
        .cols = dplyr::all_of(x = cols_),
        .fns  = \(.x) dplyr::if_else(condition = keep_,
                                     true      = as.numeric(x = .x),
                                     false     = NA_real_)
      )
    )
    hit_n_ <- hit_n_ + length(x = cols_)
  }

  cli::cli_alert_info(text = "masked {hit_n_} item-scoped column{?s}")
  .tab
}

#' Did the mask actually land? Semantic, not structural.
#'
#' Checks that every `n*_I<x>` / `u*_I<x>` column is NA on EXACTLY the rows where `HasI<x>` is
#' FALSE and the document exists. A guard that only confirmed the columns were present would pass
#' on a panel where the mask silently did nothing, which is the failure mode this project keeps
#' hitting.
v6_verify_mask <- function(.tab, .items = .V5_ITEMS) {

  cli::cli_h2(text = "Item mask")

  out_ <- purrr::map(
    .x = .items,
    .f = \(.it) {
      flag_ <- paste0("HasI", .it)
      sfx_  <- paste0("_I", .it)
      cols_ <- stringi::stri_subset_regex(str     = names(x = .tab),
                                          pattern = paste0("^[nu].*", sfx_, "$"))
      if (!flag_ %in% names(x = .tab) || length(x = cols_) == 0) return(NULL)

      should_ <- .tab$HasDoc & !.tab[[flag_]]
      wrong_  <- purrr::map_int(
        .x = cols_,
        .f = \(.c) sum(should_ & !is.na(x = .tab[[.c]]), na.rm = TRUE)
      )

      tibble::tibble(
        Item     = .it,
        NCols    = length(x = cols_),
        PctFound = round(x = 100 * mean(x = .tab[[flag_]], na.rm = TRUE), digits = 1),
        NMasked  = sum(should_, na.rm = TRUE),
        NLeaked  = sum(wrong_)
      )
    }
  ) |>
    purrr::list_rbind()

  print(x = as.data.frame(x = out_), row.names = FALSE)

  if (sum(out_$NLeaked) > 0) {
    cli::cli_alert_danger(
      text = "{sum(out_$NLeaked)} cell{?s} not NA where the item was absent - the mask did not land"
    )
  } else {
    cli::cli_alert_success(text = "every item-scoped count is NA exactly where the item is absent")
  }

  invisible(x = out_)
}

#' Zero-fill a block of columns only where the block was actually attempted.
#'
#' THE DISTINCTION V1 COULD NOT MAKE. A firm-year whose document was scored and yielded no climate
#' paragraph has ZERO. A firm-year with no document, or one the scorer never reached, has NA. Both
#' would otherwise be a zero, and the reader cannot tell them apart afterwards.
v5_fill_block <- function(.tab, .flag, .cols) {
  if (length(x = .cols) == 0) return(.tab)
  dplyr::mutate(
    .data = .tab,
    dplyr::across(
      .cols = dplyr::all_of(x = .cols),
      .fns  = \(.x) dplyr::if_else(
        condition = .data[[.flag]],
        true      = dplyr::coalesce(.x, 0),
        false     = NA_real_
      )
    )
  )
}



# Specification -------------------------------------------------------------------------------------
# Carried verbatim from 002g-GetDisclosures-V6.R.

#' "" is the whole document. Everything else is an item, and every item-scoped column is masked to
#' NA where that item was not located - see v6_mask_items().
.V6_SCOPES <- c("", paste0("_I", .V5_ITEMS))

#' list_id -> exported name, with the construct assignment.
#'
#' CONSTRUCT AND PLACEBO ARE DECLARED HERE, BEFORE ANY COEFFICIENT IS SEEN, and match
#' .SW_CONSTRUCTS in 057-AllVariables.R. A list is a placebo if a wildfire near a firm's
#' headquarters has no plausible channel to it. That declaration is what makes the sweep a test
#' rather than a search: if the placebos move as much as the climate lists, the finding is about
#' the design.
#'
#' `lin24` is 8 of the 23 and is HELD AT PROPOSED STATUS on a lemmatisation incompatibility - the
#' authors lemmatise before matching and this pipeline does not. Shipped and documented rather
#' than withheld, so the incompatibility is visible in the output instead of being an absence
#' nobody can see.
.V6_LISTS <- tibble::tribble(
  ~ListId,                    ~Name,           ~Construct,   ~Placebo, ~Note,
  "paper_fire_orig",          "FireOrig",      "fire",       FALSE,    "113 forms, 46 fire",
  "paper_fire_adj",           "FireAdj",       "fire",       FALSE,    "VERIFIED orig minus BLAZE/CONFLAGRATION/INFERNO + plurals",
  "paper_fire_min",           "FireMin",       "fire",       FALSE,    "22 forms; VERIFIED min subset adj subset orig",
  "paper_esg",                "Esg",           "climate",    FALSE,    "the paper's ESG list; nEsgSen is Table 5 c3-4",
  "kww23",                    "Kww23",         "climate",    FALSE,    "Kim, Wang & Wu 2023 RAS",
  "bbk20_env",                "Bbk20Env",      "climate",    FALSE,    "Baier, Berninger & Kiesel 2020, env subset",
  "lsty24_transition",        "Lsty24Trans",   "climate",    FALSE,    "Li, Shan, Tang & Yao 2024 RFS",
  "lin24_climate_change",     "Lin24Clim",     "climate",    FALSE,    "proposed - lemmatisation",
  "lin24_ecosystem",          "Lin24Eco",      "climate",    FALSE,    "proposed - lemmatisation",
  "lin24_general",            "Lin24Gen",      "climate",    FALSE,    "proposed - lemmatisation",
  "lin24_nature_resources",   "Lin24Nat",      "climate",    FALSE,    "proposed - lemmatisation",
  "lin24_pollution_waste",    "Lin24Poll",     "climate",    FALSE,    "proposed - lemmatisation",
  "lsty24_phy_acute",         "Lsty24Acute",   "physical",   FALSE,    "acute physical risk - closest construct to a wildfire",
  "lsty24_phy_chronic",       "Lsty24Chron",   "physical",   FALSE,    "chronic physical risk",
  "paper_damage",             "Damage",        "damage",     FALSE,    "drives the section 3.1 exclusion",
  "paper_loss",               "Loss",          "damage",     FALSE,    "",
  "paper_insurance",          "Insurance",     "damage",     FALSE,    "",
  "paper_potential",          "Potential",     "damage",     FALSE,    "",
  "bbk20",                    "Bbk20",         "governance", TRUE,     "GAAP, COMPENSATION, CONTROL, AUDIT",
  "paper_operation",          "Operation",     "operations", TRUE,     "STOCK, OPERATIONS, ASSETS, SALES",
  "lin24_human_capital",      "Lin24Human",    "social",     TRUE,     "proposed - lemmatisation",
  "lin24_other_stakeholders", "Lin24Stake",    "social",     TRUE,     "proposed - lemmatisation",
  "lin24_products_customers", "Lin24Product",  "social",     TRUE,     "proposed - lemmatisation"
)

#' model x label -> exported name fragment.
#'
#' TWO DIFFERENT FLAGS, from two different kinds of evidence.
#'
#' `Gated` is what the model's AUTHORS document: operates on climate-related paragraphs, no null
#' class. Run unconditionally those four measure the SECTION rather than the content - ClSentiment
#' returned 87.9% non-neutral in Item 1A, which is Risk Factors being written as risk.
#'
#' `ClOnly` is what the STORE says, measured 2026-08-31 from `_scored`. Five models have their
#' documents SPLIT across two scopes:
#'
#'     ClCommit       8,828 all / 133,588 climate     5.6% of documents at scope all
#'     ClSpecificity  8,018 all / 134,261 climate     5.1%
#'     ClSentiment    7,518 all / 134,652 climate     4.8%
#'     ClTCFD         7,518 all / 134,652 climate     4.8%
#'     TrPhysical     3,001 all / 138,673 climate     1.9%
#'
#' `_scored` is keyed (doc_id, model), so a document sits in ONE scope. An unconditional aggregate
#' over ClSentiment would therefore count ALL prose paragraphs for 7,518 documents and only
#' gate-positive paragraphs for 134,652 - one column, two denominators, silently. The share does
#' not rescue it: p divides by that model's own scored paragraphs, so for the 134,652 it already
#' IS the Cl version, which makes the unconditional column a mixture of two measures rather than
#' either one.
#'
#' SO ClOnly MODELS SHIP ONLY THE Cl VARIANT. 14 of the 23 model-label pairs lose their
#' unconditional columns: 1,104 -> 768.
#'
#' Note TrPhysical is ClOnly but NOT Gated - its authors do not require the gate, the store split
#' it anyway. This is also what produced the 114x Detector_no/Detector_yes lift in the model
#' diagnostics: those Detector_no paragraphs come from the 3,001-document `all` run, not from the
#' corpus. Duplicates checked and zero; `scores` reconciles exactly to `_scored`.
.V6_BERT <- tibble::tribble(
  ~Model,          ~Name,          ~Label,        ~LabelName,    ~Gated, ~ClOnly, ~NoCl, ~Note,
  "ClDetect",      "BertDetect",   "no",          "No",          FALSE,  FALSE,   TRUE,  "the gate itself - crossing it with its own gate is degenerate",
  "ClDetect",      "BertDetect",   "yes",         "Yes",         FALSE,  FALSE,   TRUE,  "the gate itself - nBertDetectYesCl would equal nBertDetectYes exactly",
  "NetZero",       "BertNetZero",  "none",        "None",        FALSE,  FALSE,   FALSE, "",
  "NetZero",       "BertNetZero",  "reduction",   "Reduction",   FALSE,  FALSE,   FALSE, "92% of positives are HERE, not net-zero",
  "NetZero",       "BertNetZero",  "net-zero",    "NetZero",     FALSE,  FALSE,   FALSE, "15 of 198 positives on the 1% sample",
  "Renewable",     "BertRenew",    "no",          "No",          FALSE,  FALSE,   FALSE, "no tokenizer shipped; label order by probe",
  "Renewable",     "BertRenew",    "yes",         "Yes",         FALSE,  FALSE,   FALSE, "no tokenizer shipped; label order by probe",
  "EnvClaims",     "BertEnvClaim", "no",          "No",          FALSE,  FALSE,   FALSE, "",
  "EnvClaims",     "BertEnvClaim", "yes",         "Yes",         FALSE,  FALSE,   FALSE, "",
  "ClCommit",      "BertCommit",   "no",          "No",          TRUE,   TRUE,    FALSE, "8,828 all / 133,588 climate",
  "ClCommit",      "BertCommit",   "yes",         "Yes",         TRUE,   TRUE,    FALSE, "8,828 all / 133,588 climate",
  "ClSpecificity", "BertSpec",     "non",         "Non",         TRUE,   TRUE,    FALSE, "8,018 all / 134,261 climate; labels abbreviated in the checkpoint",
  "ClSpecificity", "BertSpec",     "spec",        "Spec",        TRUE,   TRUE,    FALSE, "8,018 all / 134,261 climate",
  "ClSentiment",   "BertSent",     "opportunity", "Opportunity", TRUE,   TRUE,    FALSE, "7,518 all / 134,652 climate",
  "ClSentiment",   "BertSent",     "neutral",     "Neutral",     TRUE,   TRUE,    FALSE, "7,518 all / 134,652 climate",
  "ClSentiment",   "BertSent",     "risk",        "Risk",        TRUE,   TRUE,    FALSE, "7,518 all / 134,652 climate",
  "ClTCFD",        "BertTcfd",     "governance",  "Governance",  TRUE,   TRUE,    FALSE, "no null class - forced into one of four pillars",
  "ClTCFD",        "BertTcfd",     "metrics",     "Metrics",     TRUE,   TRUE,    FALSE, "no null class",
  "ClTCFD",        "BertTcfd",     "risk",        "Risk",        TRUE,   TRUE,    FALSE, "no null class",
  "ClTCFD",        "BertTcfd",     "strategy",    "Strategy",    TRUE,   TRUE,    FALSE, "no null class",
  "TrPhysical",    "BertTrPhys",   "transition",  "Transition",  FALSE,  TRUE,    FALSE, "3,001 all / 138,673 climate - NOT author-gated, but split",
  "TrPhysical",    "BertTrPhys",   "none",        "None",        FALSE,  TRUE,    FALSE, "3,001 all / 138,673 climate",
  "TrPhysical",    "BertTrPhys",   "physical",    "Physical",    FALSE,  TRUE,    FALSE, "3,001 all / 138,673 climate"
)

#' Every fire list crossed with every other list. `X` reads as "cross" and the capital keeps the
#' CamelCase boundary.
#'
#' THE PLACEBO SIDE IS THE POINT. bBbk20XFireOrigSen should be null. Cutting the grid down to the
#' climate lists would remove exactly the columns that make this a test.
#'
#' EXPECT MOST OF THESE TO BE EMPTY. Fire is in 3.3% of documents, and item masking cuts that
#' again: nFireESG_I3 had 30 positives in 101,785 firm-years under V5's naming. The estimability
#' screen will report them; a null on a base this thin is not evidence of anything.
v6_spec_pairs <- function(.lists = .V6_LISTS) {

  fire_ <- dplyr::filter(.data = .lists, Construct == "fire")
  othr_ <- dplyr::filter(.data = .lists, Construct != "fire")

  tidyr::expand_grid(
    A = fire_$Name,
    B = othr_$Name
  ) |>
    dplyr::mutate(Name = paste0(A, "X", B))
}

#' The BASES numerical intensity is computed over: every term list and every Bert model-label.
#'
#' `NoYear` IS THE UNMARKED DEFAULT. V5 shipped pNumNoYear and pNumAny; here the no-year version -
#' the one the paper uses - is plain `Num` and the any-digit version carries `Any`. That is purely
#' a length decision: pNumNoYearBertTrPhysTransition_I1A is 34 characters against Stata's 32.
#'
#' THE BASE SIZE IS ALREADY SHIPPED FOR TERM LISTS. nEsgSen IS the count of Esg sentences, so the
#' denominator needs no new column. Bert bases need one, because n<Bert><Label> counts PARAGRAPHS
#' and the denominator here is SENTENCES in those paragraphs.
v6_spec_bases <- function(.lists = .V6_LISTS, .bert = .V6_BERT) {

  dplyr::bind_rows(
    dplyr::transmute(.data = .lists, Name, Kind = "list"),
    dplyr::transmute(.data = .bert,  Name = paste0(Name, LabelName), Kind = "bert")
  )
}

#' Every column the panel will ship, as a table, computed against NO STORE.
#'
#' Run this before any aggregate. It costs milliseconds, it names every column, and it is the only
#' cheap opportunity to catch a collision or an over-length name - both of which are silent
#' failures once they reach a 6,600-column parquet.
#'
#' THE GRAMMAR FIELDS ARE RETAINED, NOT DISCARDED. Every block below builds Stat, Base, Unit, Cl
#' and Scope in order to paste a name out of them, and the previous version dropped all five in the
#' final select. Anything downstream that then needed to know what a column IS - which list, which
#' unit, which item - had to recover it by parsing the name back apart, and THAT PARSE IS NOT
#' INVERTIBLE under this grammar: `Cl` is a BASE rather than a scope, `Unit` sits inside the name
#' with no separator, and `_IGeo` is not `_I<digit>`. 057-AllVariables.R parses V5 names that way
#' and sends ~400 model columns to "unclassified"; on these names it would send nearly all 6,205.
#' The fields cost nothing to carry and are correct BY CONSTRUCTION rather than by regex.
#'
#' ADDITIVE ONLY. Column, Family, Defn and NChar are unchanged in value AND IN ROW ORDER.
#' v6_agg_assemble() completes its count grid from setdiff(spec$Column, names(out)) and assigns
#' with out_[add_] <- 0, so a reordering here would silently permute the built panel's columns on
#' the next rebuild. Every block that was a literal vector is still a literal vector for that
#' reason, rather than being tidied into an expand_grid.
#'
#' `key` and `geo` are DECLARED OUTSIDE THE GRAMMAR. Keys are not measures, and GeoDispersion is a
#' distinct-state count with no b/n/p prefix - section 6 of export-spec.md says so outright. Both
#' carry a Scope where one is meaningful (HasI1A -> _I1A) and both are exempt from the
#' reconstruction check at the end.
v6_spec_names <- function(.lists  = .V6_LISTS,
                          .bert   = .V6_BERT,
                          .scopes = .V6_SCOPES) {

  cross_ <- function(.base, .family, .stats, .defn) {
    tidyr::expand_grid(Base = .base, Stat = .stats, Scope = .scopes) |>
      dplyr::mutate(
        Unit   = "",
        Cl     = "",
        Column = paste0(Stat, Base, Scope),
        Family = .family,
        Defn   = .defn
      )
  }

  n_item_ <- length(x = .scopes) - 1L

  # ---- denominators and keys --------------------------------------------------------------------
  # Unit carries the noun here and Base is empty, which is what makes nSen_I1 reconstruct as
  # "n" + "" + "Sen" + "" + "_I1" on the same rule as every measure family.
  vol_ <- tibble::tibble(
    Column = c("nSen", "nPar", "nWords", paste0("nSen", .scopes[-1]),
               paste0("nPar", .scopes[-1]), paste0("nWords", .scopes[-1]), "nTable"),
    Stat   = "n",
    Base   = "",
    Unit   = c("Sen", "Par", "Words",
               rep(x = "Sen",   times = n_item_),
               rep(x = "Par",   times = n_item_),
               rep(x = "Words", times = n_item_),
               "Table"),
    Cl     = "",
    Scope  = c("", "", "", .scopes[-1], .scopes[-1], .scopes[-1], ""),
    Family = "volume",
    Defn   = "denominator"
  )

  key_ <- tibble::tibble(
    Column = c("gvkey", "datadate", "doc_id", "form_type", "route", "NDocs",
               "HasDoc", "HasBert", "HasTerms", "item_cover", "n_item_kept",
               paste0("HasI", .V5_ITEMS), "HasGeoItem"),
    Stat   = "",
    Base   = Column,
    Unit   = "",
    Cl     = "",
    # HasI1A is the flag FOR scope _I1A. Carrying that here is what lets a screen pair an
    # item-scoped measure with the flag that says whether its item was found at all, without
    # anything having to parse "HasI" off the front of a name.
    Scope  = c(rep(x = "", times = 11L), paste0("_I", .V5_ITEMS), "_IGeo"),
    Family = "key",
    Defn   = "key or flag"
  )

  # ---- term lists: b, pSen, pPar, nSen, nPar ----------------------------------------------------
  lst_ <- dplyr::bind_rows(
    cross_(.base   = .lists$Name,
           .family = "list",
           .stats  = "b",
           .defn   = "any sentence carrying a term from this list"),
    tidyr::expand_grid(Base = .lists$Name, Stat = c("n", "p"),
                       Unit = c("Sen", "Par"), Scope = .scopes) |>
      dplyr::mutate(Cl = "",
                    Column = paste0(Stat, Base, Unit, Scope), Family = "list",
                    Defn = "count or share of units carrying a term from this list")
  )

  # ---- intersections: b, p, n at BOTH units -----------------------------------------------------
  int_ <- tidyr::expand_grid(
    Base  = v6_spec_pairs(.lists = .lists)$Name,
    Stat  = c("b", "n", "p"),
    Unit  = c("Sen", "Par"),
    Scope = .scopes
  ) |>
    dplyr::mutate(Cl = "",
                  Column = paste0(Stat, Base, Unit, Scope), Family = "pair",
                  Defn = "unit carrying a term from BOTH lists")

  # ---- Bert: b, n, p at two bases, no unit ------------------------------------------------------
  # ClOnly models emit the Cl variant ONLY - see .V6_BERT. Building both and dropping later would
  # leave the unconditional names in circulation long enough for someone to use one.
  nm_ <- paste0(.bert$Name, .bert$LabelName)

  brt_ <- dplyr::bind_rows(
    tidyr::expand_grid(Base = nm_[!.bert$ClOnly & !.bert$NoCl], Stat = c("b", "n", "p"),
                       Cl = c("", "Cl"), Scope = .scopes),
    tidyr::expand_grid(Base = nm_[.bert$ClOnly],                Stat = c("b", "n", "p"),
                       Cl = "Cl",        Scope = .scopes),
    tidyr::expand_grid(Base = nm_[.bert$NoCl],                  Stat = c("b", "n", "p"),
                       Cl = "",          Scope = .scopes)
  ) |>
    dplyr::mutate(Unit = "",
                  Column = paste0(Stat, Base, Cl, Scope), Family = "bert",
                  Defn = "paragraphs carrying this model label")

  # ---- numerical intensity ----------------------------------------------------------------------
  bases_ <- v6_spec_bases(.lists = .lists, .bert = .bert)

  num_ <- dplyr::bind_rows(
    tidyr::expand_grid(Base = bases_$Name, Stat = c("nNum", "nNumAny", "pNum", "pNumAny"),
                       Scope = .scopes) |>
      dplyr::mutate(Unit = "", Cl = "",
                    Column = paste0(Stat, Base, Scope), Family = "numeric",
                    Defn = "digit / non-year-digit sentences over this base"),
    tidyr::expand_grid(Base = bases_$Name[bases_$Kind == "bert"], Scope = .scopes) |>
      dplyr::mutate(Stat = "n", Unit = "Sen", Cl = "",
                    Column = paste0("n", Base, "Sen", Scope), Family = "numeric",
                    Defn = "base size: sentences in paragraphs with this label")
  )

  # ---- geography --------------------------------------------------------------------------------
  # THE FOUR Defn STRINGS NOW DIFFER. They were one identical string, which reads as four encodings
  # of one measure when the entire distinction between them is NA semantics - which is the thing
  # the zero-fill defect was about. The wording is section 6 of export-spec.md.
  geo_ <- tibble::tibble(
    Column = c("GeoDispersion", "GeoDispersion0", "GeoDispersion_IGeo", "GeoDispersion_IGeo0"),
    Stat   = "",
    # THE FILL VARIANT IS PART OF THE BASE. Measured 2026-09-01: with Base identical across all
    # four, anything grouping by (Base, Stat, Unit, Cl) and putting Scope across the columns sees
    # ONE variable with two duplicate scopes, and renders a four-column table headed
    # "(doc) | Geo items | (doc) | Geo items" - the NA-semantics version and the zero-filled one
    # indistinguishable. The distinction between them is the whole reason both ship.
    #
    # Note the naming is genuinely irregular here and this does not repair that: the fill marker
    # sits AFTER the scope (GeoDispersion_IGeo0), so Column is not paste0(Stat, Base, Unit, Cl,
    # Scope) for the filled variants. That is why geo is exempt from the reconstruction check. What
    # this fixes is IDENTITY - two measures at two scopes, rather than one measure at four.
    Base   = c("GeoDispersion", "GeoDispersion0", "GeoDispersion", "GeoDispersion0"),
    Unit   = "",
    Cl     = "",
    Scope  = c("", "", "_IGeo", "_IGeo"),
    Family = "geo",
    Defn   = c("distinct states / 50; NA if no filing or no state found",
               "distinct states / 50; 0 if no state found, NA if no filing",
               "distinct states / 50 over the geo item set; NA if no filing or items not found",
               "distinct states / 50 over the geo item set; 0 if either, NA if no filing")
  )

  out_ <- dplyr::bind_rows(key_, vol_, lst_, int_, brt_, num_, geo_) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(x = c("Stat", "Base", "Unit", "Cl", "Scope")),
        .fns  = \(.x) dplyr::coalesce(.x, "")
      )
    ) |>
    dplyr::select(Column, Family, Defn, Stat, Base, Unit, Cl, Scope) |>
    dplyr::mutate(NChar = nchar(x = Column))

  # ---- the grammar has to be invertible ---------------------------------------------------------
  # THE ONE CHECK HERE THAT CANNOT BE SATISFIED BY ACCIDENT. v6_spec_check() asks whether the names
  # are unique, legal and short - all three of which stay true if a block's fields stop describing
  # the name it emits. This asks whether the parts still rebuild the whole. If they do not, a
  # catalogue built on the fields is describing columns that are not the ones it names, and it says
  # so in a table that looks entirely normal.
  chk_ <- dplyr::filter(.data = out_, !Family %in% c("key", "geo"))
  bad_ <- dplyr::filter(.data = chk_, paste0(Stat, Base, Unit, Cl, Scope) != Column)

  if (nrow(x = bad_) > 0) {
    cli::cli_abort(
      message = c(
        "{nrow(x = bad_)} name{?s} do not reconstruct from their own grammar fields.",
        "x" = "{paste(utils::head(x = bad_$Column, n = 5L), collapse = ', ')}",
        "i" = "A block built a Column by a rule its Stat/Base/Unit/Cl/Scope do not express."
      )
    )
  }

  out_
}

#' Are the names legal, unique and short enough? Semantic, not structural.
#'
#' STATA'S LIMIT IS 32 CHARACTERS and it does not warn - it truncates, which turns two distinct
#' measures into one column that silently overwrites the other. That is the failure this checks
#' for, and it is why the check runs before the data exists rather than after.
v6_spec_check <- function(.spec = v6_spec_names()) {

  cli::cli_h2(text = "Column count by family")
  .spec |>
    dplyr::summarise(.by = Family, NCols = dplyr::n(), MaxChar = max(NChar)) |>
    dplyr::arrange(dplyr::desc(NCols)) |>
    as.data.frame() |>
    print(row.names = FALSE)

  cli::cli_alert_info(text = "{scales::comma(x = nrow(x = .spec))} column{?s} total")

  dup_ <- .spec |>
    dplyr::filter(.by = Column, dplyr::n() > 1) |>
    dplyr::distinct(Column, Family)

  if (nrow(x = dup_) > 0) {
    cli::cli_alert_danger(text = "{nrow(x = dup_)} DUPLICATE name{?s}")
    print(x = as.data.frame(x = utils::head(x = dup_, n = 20L)), row.names = FALSE)
  } else {
    cli::cli_alert_success(text = "every name is unique")
  }

  long_ <- dplyr::filter(.data = .spec, NChar > 32L)

  if (nrow(x = long_) > 0) {
    cli::cli_alert_danger(
      text = "{nrow(x = long_)} name{?s} over 32 characters - Stata TRUNCATES without warning"
    )
    print(x = as.data.frame(x = utils::head(x = dplyr::arrange(long_, dplyr::desc(NChar)),
                                            n = 20L)), row.names = FALSE)
  } else {
    cli::cli_alert_success(text = "every name is within Stata's 32 characters")
  }

  bad_ <- dplyr::filter(
    .data = .spec,
    !stringi::stri_detect_regex(str = Column, pattern = "^[A-Za-z][A-Za-z0-9_]*$")
  )

  if (nrow(x = bad_) > 0) {
    cli::cli_alert_danger(text = "{nrow(x = bad_)} name{?s} are not legal R/Stata identifiers")
    print(x = as.data.frame(x = utils::head(x = bad_, n = 20L)), row.names = FALSE)
  } else {
    cli::cli_alert_success(text = "every name is a legal identifier")
  }

  invisible(x = .spec)
}



# Aggregation and panel -----------------------------------------------------------------------------
# Carried verbatim from 002g-GetDisclosures-V6.R.

#' Denominators. Every share in the panel divides by one of these.
#'
#' WHAT IS NEW IS nSen_I*. V5 computed sentence counts at DOCUMENT scope only, which made every
#' item-scoped sentence share uncomputable - and the paper's ESG unit IS the sentence: Table A1
#' defines ESG Disclosure Length as a count of sentences. `sentences` is keyed
#' (doc_id, par_id, sen_id), so the item scope was always available; it was simply never grouped on.
#'
#' THE SENTENCE-BY-ITEM QUERY IS THE EXPENSIVE ONE. It joins 210M sentences to 74M paragraphs, and
#' after v6_item_install() `paragraphs` is a VIEW, so each row also pays a join to item_v2. The
#' others are single-table scans. Timed separately for that reason - if this stage is slow, this is
#' where it is slow.
#'
#' DENOMINATORS ARE MASKED like everything else. nSen_I1A is NA where Item 1A was not located, so
#' every share built on it inherits the NA rather than dividing by a fabricated zero.
v6_agg_volume <- function(.path_db, .items = .V5_ITEMS) {

  if (FALSE) {
    .path_db <- .lstPaths$Store
    .items   <- .V5_ITEMS
  }

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)

  in_ <- paste(sprintf(fmt = "'%s'", .items), collapse = ", ")

  timed_ <- function(.label, .sql) {
    t0_  <- Sys.time()
    out_ <- tibble::as_tibble(x = DBI::dbGetQuery(conn = con_, statement = .sql))
    cli::cli_alert_info(
      text = "{(.label)}: {scales::comma(x = nrow(x = out_))} row{?s} in \\
              {sprintf(fmt = '%.1f', as.numeric(x = difftime(Sys.time(), t0_, units = 'secs')))}s"
    )
    out_
  }

  doc_ <- timed_(
    .label = "document",
    .sql   = "
      SELECT p.doc_id,
             SUM(CASE WHEN p.is_prose THEN 1 ELSE 0 END)          AS nPar,
             SUM(CASE WHEN p.is_prose THEN p.n_words ELSE 0 END)  AS nWords,
             SUM(CASE WHEN p.is_table THEN 1 ELSE 0 END)          AS nTable
        FROM paragraphs p GROUP BY 1"
  )

  sen_ <- timed_(
    .label = "document sentences",
    .sql   = "
      SELECT s.doc_id, COUNT(*) AS nSen
        FROM sentences s JOIN paragraphs p
          ON p.doc_id = s.doc_id AND p.par_id = s.par_id
       WHERE p.is_prose GROUP BY 1"
  )

  item_par_ <- timed_(
    .label = "item paragraphs",
    .sql   = sprintf(
      fmt = "
        SELECT doc_id, item_num, COUNT(*) AS nPar, SUM(n_words) AS nWords
          FROM paragraphs
         WHERE is_prose AND item_num IN (%s)
         GROUP BY 1, 2", in_
    )
  )

  item_sen_ <- timed_(
    .label = "item sentences",
    .sql   = sprintf(
      fmt = "
        SELECT p.doc_id, p.item_num, COUNT(*) AS nSen
          FROM sentences s JOIN paragraphs p
            ON p.doc_id = s.doc_id AND p.par_id = s.par_id
         WHERE p.is_prose AND p.item_num IN (%s)
         GROUP BY 1, 2", in_
    )
  )

  # LEFT, not full: the sentence query is a strict subset of the paragraph query - both filter on
  # is_prose and the same item set, and a sentence cannot exist without its paragraph. An item with
  # paragraphs but no sentence rows is a MEASURED zero, so coalesce is right here. values_fill in
  # pivot_wider would not catch it: that fills absent COMBINATIONS, not NAs already in the frame.
  item_ <- item_par_ |>
    dplyr::left_join(y = item_sen_, by = c("doc_id", "item_num")) |>
    dplyr::mutate(nSen = dplyr::coalesce(nSen, 0L)) |>
    tidyr::pivot_wider(
      names_from  = item_num,
      values_from = c(nPar, nWords, nSen),
      names_glue  = "{.value}_I{item_num}",
      values_fill = 0L
    )

  out_ <- doc_ |>
    dplyr::left_join(y = sen_,  by = "doc_id") |>
    dplyr::left_join(y = item_, by = "doc_id")

  cli::cli_alert_success(
    text = "volume: {scales::comma(x = nrow(x = out_))} document{?s}, \\
            {ncol(x = out_) - 1L} column{?s}"
  )

  out_
}

#' n<List>Sen<Scope> and n<List>Par<Scope>.
#'
#' V5 also shipped `n` (hits including repeats), `u` (distinct forms) and `r` (per 1,000 words).
#' `n` scales with how often ONE sentence repeats a term, which is a fact about writing style;
#' `u` measures vocabulary breadth, which no hypothesis here is about; `r` is superseded by
#' pSen/pPar. 552 columns not shipped.
v6_agg_lists <- function(.path_terms, .path_db, .lists = .V6_LISTS, .items = .V5_ITEMS) {

  if (FALSE) {
    .path_terms <- .lstPaths$Terms; .path_db <- .lstPaths$Store
    .lists <- .V6_LISTS; .items <- .V5_ITEMS
  }

  con_ <- v5_connect(.path_db = .path_terms, .read_only = TRUE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(conn = con_,
                 statement = sprintf(fmt = "ATTACH IF NOT EXISTS '%s' AS par (READ_ONLY)", .path_db))

  in_lst_ <- paste(sprintf(fmt = "'%s'", .lists$ListId), collapse = ", ")
  in_itm_ <- paste(sprintf(fmt = "'%s'", .items),        collapse = ", ")
  t0_     <- Sys.time()

  doc_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "SELECT h.doc_id, h.list_id, '' AS Scope,
                    COUNT(DISTINCT (h.par_id))           AS nPar,
                    COUNT(DISTINCT (h.par_id, h.sen_id)) AS nSen
               FROM term_hits h WHERE h.list_id IN (%s) GROUP BY 1, 2", in_lst_)
  ) |>
    tibble::as_tibble()

  item_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "SELECT h.doc_id, h.list_id, '_I' || p.item_num AS Scope,
                    COUNT(DISTINCT (h.par_id))           AS nPar,
                    COUNT(DISTINCT (h.par_id, h.sen_id)) AS nSen
               FROM term_hits h
               JOIN par.paragraphs p
                 ON p.doc_id = h.doc_id AND p.par_id = h.par_id
              WHERE h.list_id IN (%s) AND p.item_num IN (%s)
              GROUP BY 1, 2, 3", in_lst_, in_itm_)
  ) |>
    tibble::as_tibble()

  out_ <- dplyr::bind_rows(doc_, item_) |>
    dplyr::inner_join(y = dplyr::select(.data = .lists, list_id = ListId, Base = Name),
                      by = "list_id") |>
    tidyr::pivot_longer(cols = c(nSen, nPar), names_to = "Unit", values_to = "Val") |>
    dplyr::transmute(
      doc_id,
      Column = paste0("n", Base,
                      stringi::stri_replace_first_fixed(str = Unit, pattern = "n",
                                                        replacement = ""),
                      Scope),
      Val
    )

  cli::cli_alert_success(
    text = "lists: {scales::comma(x = nrow(x = out_))} row{?s}, \\
            {dplyr::n_distinct(out_$Column)} column{?s} in \\
            {sprintf(fmt = '%.1f', as.numeric(x = difftime(Sys.time(), t0_, units = 'secs')))}s"
  )

  out_
}

#' Materialise num_any / num_year ONCE, over every prose sentence.
#'
#' WITHOUT THIS, 46 BASES MEANS 46 REGEX SCANS OF 210M SENTENCES, each rebuilding sentence text by
#' SUBSTR from its paragraph. Run once into a persistent table and every base becomes a join
#' against two booleans.
#'
#' THE PATTERNS REPRODUCE 1b_numerical_intensity.py VERBATIM. num_any is any digit; num_year is a
#' four-digit 19xx/20xx bounded by non-digits. Intensity is `num_any AND NOT num_year` - a number
#' that is not a date. Changing either changes Table 5 columns 5-6, so they are not to be tidied.
v6_num_build <- function(.path_db, .table = "sen_num", .force = FALSE) {

  con_ <- v5_connect(.path_db = .path_db, .read_only = FALSE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)

  have_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(fmt = "SELECT COUNT(*) AS N FROM information_schema.tables
                                WHERE table_name = '%s'", .table)
  )$N

  if (have_ > 0 && !.force) {
    n_ <- DBI::dbGetQuery(conn = con_,
                          statement = sprintf(fmt = "SELECT COUNT(*) AS N FROM %s", .table))$N
    cli::cli_alert_info(
      text = "{(.table)} exists: {scales::comma(x = n_)} sentence{?s}. .force = TRUE to rebuild."
    )
    return(invisible(x = n_))
  }

  t0_ <- Sys.time()
  cli::cli_alert_info(text = "scanning every prose sentence - this is the slow one")

  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(
      fmt = "
        CREATE OR REPLACE TABLE %s AS
        SELECT s.doc_id, s.par_id, s.sen_id,
               regexp_matches(SUBSTR(p.text, s.char_start,
                                     s.char_end - s.char_start + 1), '[0-9]') AS num_any,
               regexp_matches(SUBSTR(p.text, s.char_start,
                                     s.char_end - s.char_start + 1),
                              '(^|[^0-9])(19|20)[0-9]{2}([^0-9]|$)')          AS num_year
          FROM sentences s
          JOIN paragraphs p ON p.doc_id = s.doc_id AND p.par_id = s.par_id
         WHERE p.is_prose", .table)
  )

  n_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(fmt = "SELECT COUNT(*) AS N,
                                      SUM(CASE WHEN num_any THEN 1 ELSE 0 END)  AS NAny,
                                      SUM(CASE WHEN num_year THEN 1 ELSE 0 END) AS NYear
                                 FROM %s", .table)
  )

  cli::cli_alert_success(
    text = "{(.table)}: {scales::comma(x = n_$N)} sentence{?s}, \\
            {round(x = 100 * n_$NAny / n_$N, digits = 1)}% any digit, \\
            {round(x = 100 * n_$NYear / n_$N, digits = 1)}% year, \\
            {sprintf(fmt = '%.1f', as.numeric(x = difftime(Sys.time(), t0_, units = 'mins')))} min"
  )

  invisible(x = n_)
}

#' nNum / nNumAny over each of the 46 bases, plus the Bert base sizes.
#'
#' A BASE IS A SENTENCE SET: for a term list, sentences carrying one of its terms; for a Bert
#' model-label, sentences living in paragraphs the model gave that label. Both resolve to
#' (doc_id, par_id, sen_id), so both join sen_num the same way.
#'
#' THE BASE SIZE SHIPS ONLY FOR BERT BASES. For a term list the denominator is n<List>Sen, which
#' v6_agg_lists already produces - two columns that must agree is one column too many.
#'
#' `NoYear` IS UNMARKED: nNumEsg is the non-year count, the one the paper uses; nNumAnyEsg is any
#' digit. A length decision - pNumNoYearBertTrPhysTransition_I1A is 34 characters against 32.
v6_agg_numeric <- function(.path_db, .path_terms, .path_scores,
                           .lists = .V6_LISTS, .bert = .V6_BERT, .items = .V5_ITEMS,
                           .table = "sen_num") {

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(conn = con_, statement = sprintf(
    fmt = "ATTACH IF NOT EXISTS '%s' AS trm (READ_ONLY)", .path_terms))
  DBI::dbExecute(conn = con_, statement = sprintf(
    fmt = "ATTACH IF NOT EXISTS '%s' AS sc (READ_ONLY)", .path_scores))

  in_lst_ <- paste(sprintf(fmt = "'%s'", .lists$ListId), collapse = ", ")
  in_itm_ <- paste(sprintf(fmt = "'%s'", .items),        collapse = ", ")
  t0_     <- Sys.time()

  # DISTINCT first: term_hits holds one row per FORM matched, so a sentence carrying two terms from
  # the same list appears twice and would be double-counted in numerator and denominator alike.
  lst_sql_ <- "
    WITH b AS (SELECT DISTINCT h.doc_id, h.par_id, h.sen_id, h.list_id
                 FROM trm.term_hits h WHERE h.list_id IN (%s))
    SELECT b.doc_id, b.list_id AS Src, %s AS Scope,
           SUM(CASE WHEN n.num_any THEN 1 ELSE 0 END)                         AS nNumAny,
           SUM(CASE WHEN n.num_any AND NOT n.num_year THEN 1 ELSE 0 END)      AS nNum
      FROM b
      JOIN %s n ON n.doc_id = b.doc_id AND n.par_id = b.par_id AND n.sen_id = b.sen_id
      %s
     GROUP BY 1, 2, 3"

  lst_ <- dplyr::bind_rows(
    tibble::as_tibble(x = DBI::dbGetQuery(
      conn = con_, statement = sprintf(fmt = lst_sql_, in_lst_, "''", .table, ""))),
    tibble::as_tibble(x = DBI::dbGetQuery(
      conn = con_, statement = sprintf(
        fmt = lst_sql_, in_lst_, "'_I' || p.item_num", .table,
        sprintf(fmt = "JOIN paragraphs p ON p.doc_id = b.doc_id AND p.par_id = b.par_id
                        AND p.item_num IN (%s)", in_itm_))))
  ) |>
    dplyr::inner_join(y = dplyr::select(.data = .lists, Src = ListId, Base = Name), by = "Src")

  cli::cli_alert_info(text = "numeric, term bases: {scales::comma(x = nrow(x = lst_))} row{?s}")

  # No DISTINCT needed: scores is one row per (doc_id, par_id, model), verified zero duplicates.
  brt_sql_ <- "
    SELECT s.doc_id, s.model || '|' || s.label AS Src, %s AS Scope,
           COUNT(*)                                                          AS nSenBase,
           SUM(CASE WHEN n.num_any THEN 1 ELSE 0 END)                        AS nNumAny,
           SUM(CASE WHEN n.num_any AND NOT n.num_year THEN 1 ELSE 0 END)     AS nNum
      FROM sc.scores s
      JOIN %s n ON n.doc_id = s.doc_id AND n.par_id = s.par_id
      %s
     GROUP BY 1, 2, 3"

  brt_ <- dplyr::bind_rows(
    tibble::as_tibble(x = DBI::dbGetQuery(
      conn = con_, statement = sprintf(fmt = brt_sql_, "''", .table, ""))),
    tibble::as_tibble(x = DBI::dbGetQuery(
      conn = con_, statement = sprintf(
        fmt = brt_sql_, "'_I' || p.item_num", .table,
        sprintf(fmt = "JOIN paragraphs p ON p.doc_id = s.doc_id AND p.par_id = s.par_id
                        AND p.item_num IN (%s)", in_itm_))))
  ) |>
    dplyr::inner_join(
      y  = dplyr::transmute(.data = .bert, Src = paste0(Model, "|", Label),
                            Base = paste0(Name, LabelName)),
      by = "Src"
    )

  out_ <- dplyr::bind_rows(
    dplyr::transmute(.data = lst_, doc_id, Column = paste0("nNum",    Base, Scope), Val = nNum),
    dplyr::transmute(.data = lst_, doc_id, Column = paste0("nNumAny", Base, Scope), Val = nNumAny),
    dplyr::transmute(.data = brt_, doc_id, Column = paste0("nNum",    Base, Scope), Val = nNum),
    dplyr::transmute(.data = brt_, doc_id, Column = paste0("nNumAny", Base, Scope), Val = nNumAny),
    dplyr::transmute(.data = brt_, doc_id, Column = paste0("n", Base, "Sen", Scope), Val = nSenBase)
  )

  cli::cli_alert_success(
    text = "numeric: {scales::comma(x = nrow(x = out_))} row{?s}, \\
            {dplyr::n_distinct(out_$Column)} column{?s} in \\
            {sprintf(fmt = '%.1f', as.numeric(x = difftime(Sys.time(), t0_, units = 'mins')))} min"
  )

  out_
}

#' n<Base><Cl><Scope> - paragraph counts per model-label, at two bases, across 8 scopes.
#'
#' `Cl` IS A BASE, NOT A SCOPE. V5 made _Cl a ninth scope, so it could never be crossed with an
#' item: you could have the climate-restricted count OR the Item 1A count, never both.
#'
#' ClOnly MODELS EMIT ONLY THE Cl VARIANT. Their documents are split across scope 'all' and
#' 'climate' in `_scored`, so an unconditional count would mix all prose paragraphs for a few
#' thousand documents with gate-positive paragraphs for a hundred thousand - one column, two
#' denominators.
#'
#' ClDetect EMITS NO Cl VARIANT AT ALL, because the Cl base IS ClDetect = yes. Crossing the gate
#' with itself is degenerate in both directions: nBertDetectNoCl is empty BY CONSTRUCTION - a
#' paragraph the detector called `no` cannot be in the `yes` set - and nBertDetectYesCl equals
#' nBertDetectYes exactly, which makes pBertDetectYesCl a constant 1.0 in every row. The first was
#' caught because it produced no rows; the SECOND would have shipped silently as a column that
#' looks like a measure and carries no information. That is the more dangerous of the two.
v6_agg_models <- function(.path_scores, .path_db, .bert = .V6_BERT, .items = .V5_ITEMS) {

  con_ <- v5_connect(.path_db = .path_scores, .read_only = TRUE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(conn = con_, statement = sprintf(
    fmt = "ATTACH IF NOT EXISTS '%s' AS par (READ_ONLY)", .path_db))

  in_itm_ <- paste(sprintf(fmt = "'%s'", .items), collapse = ", ")
  t0_     <- Sys.time()

  DBI::dbExecute(
    conn      = con_,
    statement = "
      CREATE OR REPLACE TEMP TABLE cl_yes AS
      SELECT doc_id, par_id FROM scores WHERE model = 'ClDetect' AND label = 'yes'"
  )

  cli::cli_alert_info(
    text = "gate: {scales::comma(x = DBI::dbGetQuery(con_, 'SELECT COUNT(*) AS N FROM cl_yes')$N)} \\
            climate paragraph{?s}"
  )

  sql_ <- "
    SELECT s.doc_id, s.model || '|' || s.label AS Src, %s AS Scope, COUNT(*) AS nPar
      FROM scores s %s %s GROUP BY 1, 2, 3"

  itm_ <- sprintf(fmt = "JOIN par.paragraphs p ON p.doc_id = s.doc_id AND p.par_id = s.par_id
                          AND p.item_num IN (%s)", in_itm_)
  cl_  <- "JOIN cl_yes c ON c.doc_id = s.doc_id AND c.par_id = s.par_id"

  grab_ <- function(.scope, .join, .clj, .tag) {
    tibble::as_tibble(
      x = DBI::dbGetQuery(conn = con_, statement = sprintf(fmt = sql_, .scope, .join, .clj))
    ) |>
      dplyr::mutate(Cl = .tag)
  }

  out_ <- dplyr::bind_rows(
    grab_(.scope = "''",                 .join = "",    .clj = "",  .tag = ""),
    grab_(.scope = "'_I' || p.item_num", .join = itm_,  .clj = "",  .tag = ""),
    grab_(.scope = "''",                 .join = "",    .clj = cl_, .tag = "Cl"),
    grab_(.scope = "'_I' || p.item_num", .join = itm_,  .clj = cl_, .tag = "Cl")
  ) |>
    dplyr::inner_join(
      y  = dplyr::transmute(.data = .bert, Src = paste0(Model, "|", Label),
                            Base = paste0(Name, LabelName), ClOnly, NoCl),
      by = "Src"
    ) |>
    dplyr::filter(!(ClOnly & Cl == ""), !(NoCl & Cl == "Cl")) |>
    dplyr::transmute(doc_id, Column = paste0("n", Base, Cl, Scope), Val = nPar)

  cli::cli_alert_success(
    text = "models: {scales::comma(x = nrow(x = out_))} row{?s}, \\
            {dplyr::n_distinct(out_$Column)} column{?s} in \\
            {sprintf(fmt = '%.1f', as.numeric(x = difftime(Sys.time(), t0_, units = 'mins')))} min"
  )

  out_
}

#' n<Fire>X<Other><Unit><Scope> - every fire list crossed with every other list, at both units.
#'
#' THE FIRE SIDE IS MATERIALISED ONCE. Fire is 19,014 hits across 5,219 documents. Sixty
#' independent self-joins on term_hits would rescan a 300M-row table sixty times for the same
#' answer; joining a 19k-row temp table against it twice does not.
#'
#' SEN AND PAR ARE DIFFERENT QUESTIONS. Sen is one sentence carrying both - Table A1's ESG-Fire
#' Disclosure. Par is a paragraph holding both, possibly in separate sentences, which is strictly
#' weaker. That is why intersections carry a unit on the binary and single lists do not.
#'
#' EXPECT SPARSITY. Fire is in 3.3% of documents and item masking cuts it further. The placebo
#' pairs are here deliberately: bBbk20XFireOrigSen should be null, and a grid without it is not a
#' test.
v6_agg_pairs <- function(.path_terms, .path_db, .lists = .V6_LISTS, .items = .V5_ITEMS) {

  con_ <- v5_connect(.path_db = .path_terms, .read_only = TRUE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(conn = con_, statement = sprintf(
    fmt = "ATTACH IF NOT EXISTS '%s' AS par (READ_ONLY)", .path_db))

  fire_ <- dplyr::filter(.data = .lists, Construct == "fire")
  othr_ <- dplyr::filter(.data = .lists, Construct != "fire")

  in_fire_ <- paste(sprintf(fmt = "'%s'", fire_$ListId), collapse = ", ")
  in_othr_ <- paste(sprintf(fmt = "'%s'", othr_$ListId), collapse = ", ")
  in_itm_  <- paste(sprintf(fmt = "'%s'", .items),       collapse = ", ")
  t0_      <- Sys.time()

  DBI::dbExecute(conn = con_, statement = sprintf(
    fmt = "CREATE OR REPLACE TEMP TABLE fire_sen AS
           SELECT DISTINCT doc_id, par_id, sen_id, list_id FROM term_hits
            WHERE list_id IN (%s)", in_fire_))

  DBI::dbExecute(conn = con_, statement =
    "CREATE OR REPLACE TEMP TABLE fire_par AS
     SELECT DISTINCT doc_id, par_id, list_id FROM fire_sen")

  n_f_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "SELECT COUNT(*) AS NSen, COUNT(DISTINCT doc_id) AS NDoc FROM fire_sen")

  cli::cli_alert_info(
    text = "fire side: {scales::comma(x = n_f_$NSen)} sentence{?s} in \\
            {scales::comma(x = n_f_$NDoc)} document{?s}")

  sen_sql_ <- "
    SELECT f.doc_id, f.list_id AS FireId, h.list_id AS OtherId, %s AS Scope,
           COUNT(DISTINCT (f.par_id, f.sen_id)) AS Val
      FROM fire_sen f
      JOIN term_hits h ON h.doc_id = f.doc_id AND h.par_id = f.par_id
                      AND h.sen_id = f.sen_id AND h.list_id IN (%s)
      %s GROUP BY 1, 2, 3, 4"

  par_sql_ <- "
    SELECT f.doc_id, f.list_id AS FireId, h.list_id AS OtherId, %s AS Scope,
           COUNT(DISTINCT (f.par_id)) AS Val
      FROM fire_par f
      JOIN term_hits h ON h.doc_id = f.doc_id AND h.par_id = f.par_id
                      AND h.list_id IN (%s)
      %s GROUP BY 1, 2, 3, 4"

  itm_ <- sprintf(fmt = "JOIN par.paragraphs p ON p.doc_id = f.doc_id AND p.par_id = f.par_id
                          AND p.item_num IN (%s)", in_itm_)

  get_ <- function(.sql, .scope, .join, .unit) {
    tibble::as_tibble(
      x = DBI::dbGetQuery(conn = con_, statement = sprintf(fmt = .sql, .scope, in_othr_, .join))
    ) |>
      dplyr::mutate(Unit = .unit)
  }

  out_ <- dplyr::bind_rows(
    get_(.sql = sen_sql_, .scope = "''",                 .join = "",   .unit = "Sen"),
    get_(.sql = sen_sql_, .scope = "'_I' || p.item_num", .join = itm_, .unit = "Sen"),
    get_(.sql = par_sql_, .scope = "''",                 .join = "",   .unit = "Par"),
    get_(.sql = par_sql_, .scope = "'_I' || p.item_num", .join = itm_, .unit = "Par")
  ) |>
    dplyr::inner_join(y = dplyr::select(.data = fire_, FireId = ListId, FireName = Name),
                      by = "FireId") |>
    dplyr::inner_join(y = dplyr::select(.data = othr_, OtherId = ListId, OtherName = Name),
                      by = "OtherId") |>
    dplyr::transmute(doc_id,
                     Column = paste0("n", FireName, "X", OtherName, Unit, Scope),
                     Val)

  cli::cli_alert_success(
    text = "pairs: {scales::comma(x = nrow(x = out_))} row{?s}, \\
            {dplyr::n_distinct(out_$Column)} of \\
            {nrow(x = fire_) * nrow(x = othr_) * 2L * (length(x = .items) + 1L)} \\
            count column{?s} non-empty, in \\
            {sprintf(fmt = '%.1f', as.numeric(x = difftime(Sys.time(), t0_, units = 'mins')))} min"
  )

  out_
}

#' GeoDispersion at two scopes, each with an NA and a zero treatment.
#'
#' NOT b/p/n: a DISTINCT-FORM count, how many different states appear rather than how often.
#' Nothing else in the panel works that way, so it keeps its own name.
#'
#' _IGeo IS ITS OWN SCOPE because the geo item set is a UNION over 1, 1A, 1B, 1C, 2, 6, 7, 7A -
#' Item 6 is not in .V5_ITEMS, so it cannot be one of the seven `_I*` suffixes.
#'
#' V5's `Strict` variant is DROPPED. Excluding sub-items was the V5 defect, not a robustness cut:
#' it halved the measure on filings that put their geography in Risk Factors.
v6_agg_geo <- function(.path_terms, .path_db, .list_id = "states", .geo_items = .V5_GEO_ITEMS) {

  con_ <- v5_connect(.path_db = .path_terms, .read_only = TRUE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(conn = con_, statement = sprintf(
    fmt = "ATTACH IF NOT EXISTS '%s' AS par (READ_ONLY)", .path_db))

  out_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "SELECT h.doc_id,
                    COUNT(DISTINCT h.form)                                        AS nAll,
                    COUNT(DISTINCT h.form) FILTER (WHERE regexp_matches(p.item_num, '%s'))
                                                                                  AS nGeo
               FROM term_hits h
               JOIN par.paragraphs p ON p.doc_id = h.doc_id AND p.par_id = h.par_id
              WHERE h.list_id = '%s' GROUP BY 1",
      sprintf(fmt = "^(%s)[A-Z]?$", paste(.geo_items, collapse = "|")), .list_id)
  ) |>
    tibble::as_tibble() |>
    dplyr::transmute(
      doc_id,
      GeoDispersion       = nAll / 50,
      GeoDispersion0      = nAll / 50,
      GeoDispersion_IGeo  = nGeo / 50,
      GeoDispersion_IGeo0 = nGeo / 50
    )

  cli::cli_alert_success(
    text = "geo: {scales::comma(x = nrow(x = out_))} document{?s} with >=1 state hit")

  out_
}

#' Build the V6 panel.
#'
#' ORDER IS NOT ARBITRARY.
#'   1 keys and item flags - every mask below reads the flags
#'   2 volume - every share below divides by one of its columns
#'   3 counts, family by family rather than one 6,272-column pivot
#'   4 mask - NA every item-scoped count where the item was not located
#'   5 shares - AFTER the mask, so a masked numerator gives a masked share
#'   6 binaries - LAST, from masked counts, so NA > 0 is NA and not FALSE
#'
#' STEP 6 AFTER STEP 4 IS THE POINT. Binaries derived before the mask would be FALSE on a document
#' whose item was never found - the zero-fill defect V6 exists to remove, reintroduced one layer up.
#'
#' BINARIES ARE BUILT BY EXPLICIT LOOP over base names from the spec tables, never by regex over the
#' name space. A pattern like "^n(BertDetect|...)" also matches nBertDetectYesSen - the numeric
#' base-size column - and would mint bBertDetectYesSen, a name in no specification that nothing
#' downstream would question.
v6_agg_assemble <- function(.path_sample, .keys, .volume, .lists, .models, .pairs, .numeric, .geo,
                            .items_flag = NULL, .items = .V5_ITEMS,
                            .spec = .V6_LISTS, .bert = .V6_BERT) {

  scopes_ <- c("", paste0("_I", .items))

  # START FROM THE SAMPLE, NOT THE DOCUMENTS. A firm-year with no filing must appear with HasDoc
  # FALSE and NA throughout - 4,558 of them. Starting from the matched documents would silently
  # drop those rows and turn "we have no filing for this firm-year" into "this firm-year does not
  # exist", which no downstream check would notice because the panel would simply be shorter.
  smp_ <- arrow::read_parquet(file = .path_sample, col_select = c("gvkey", "datadate")) |>
    dplyr::distinct(gvkey, datadate) |>
    dplyr::mutate(datadate = as.Date(x = datadate))

  # v5_agg_keys returns list(All =, Primary =). Primary is one document per firm-year, chosen by
  # .V5_FORM_RANK; NDocs keeps the multiplicity visible.
  out_ <- smp_ |>
    dplyr::left_join(
      y  = dplyr::select(.data = .keys$Primary, gvkey, datadate, doc_id, form_type, route,
                         NDocs, item_cover, n_item_kept),
      by = c("gvkey", "datadate")
    ) |>
    dplyr::mutate(HasDoc = !is.na(x = doc_id))

  if (!is.null(x = .items_flag) && nrow(x = .items_flag) > 0) {
    flag_ <- setdiff(x = names(x = .items_flag), y = "doc_id")
    out_  <- out_ |>
      dplyr::left_join(y = .items_flag, by = "doc_id") |>
      dplyr::mutate(dplyr::across(.cols = dplyr::all_of(x = flag_),
                                  .fns  = \(.x) dplyr::coalesce(.x, FALSE)))
  }

  vol_cols_ <- setdiff(x = names(x = .volume), y = "doc_id")
  out_ <- out_ |>
    dplyr::left_join(y = .volume, by = "doc_id") |>
    v5_fill_block(.flag = "HasDoc", .cols = vol_cols_)

  # THE BLOCK FLAGS MUST EXIST BEFORE ANY v5_fill_block() NAMES THEM - it reads .data[[.flag]] and
  # would fail on a missing column. HasBert is derived from the model table rather than assumed:
  # a document with no scored paragraph is a different fact from one scored and found empty.
  bert_docs_ <- if (is.null(x = .models) || nrow(x = .models) == 0) {
    character(length = 0)
  } else {
    unique(x = .models$doc_id)
  }

  out_ <- dplyr::mutate(
    .data    = out_,
    HasTerms = HasDoc,
    HasBert  = HasDoc & doc_id %in% bert_docs_
  )

  # ---- counts -----------------------------------------------------------------------------------
  cnt_cols_ <- character(length = 0)

  for (blk_ in list(list(Tab = .lists,   Flag = "HasTerms"),
                    list(Tab = .pairs,   Flag = "HasTerms"),
                    list(Tab = .numeric, Flag = "HasTerms"),
                    list(Tab = .models,  Flag = "HasBert"))) {

    if (is.null(x = blk_$Tab) || nrow(x = blk_$Tab) == 0) next

    w_ <- tidyr::pivot_wider(data = blk_$Tab, names_from = Column, values_from = Val,
                             values_fill = 0)

    new_      <- setdiff(x = names(x = w_), y = "doc_id")
    out_      <- out_ |>
      dplyr::left_join(y = w_, by = "doc_id") |>
      v5_fill_block(.flag = blk_$Flag, .cols = new_)
    cnt_cols_ <- c(cnt_cols_, new_)

    cli::cli_alert_info(text = "assembled {length(x = new_)} count column{?s}")
  }

  # ---- complete the count grid ------------------------------------------------------------------
  # pivot_wider emits only OBSERVED combinations, so a pair x scope with no document anywhere in
  # the corpus produces no column at all - 28 of 960 pair count columns on this corpus. That makes
  # the panel's SHAPE a function of the detector: rerun at a different window and the column set
  # moves, which is incompatible with a pre-declared sweep grid.
  #
  # A zero is also the CORRECT value here, not a filler. "No document had this intersection in this
  # item" is a measured zero wherever the item was found - and wherever it was not, the mask below
  # turns it to NA. Filling before the mask is what makes both true.
  spec_n_ <- v6_spec_names(.lists = .spec, .bert = .bert, .scopes = scopes_) |>
    dplyr::filter(Family %in% c("list", "pair", "bert", "numeric"),
                  stringi::stri_startswith_fixed(str = Column, pattern = "n"))

  add_ <- setdiff(x = spec_n_$Column, y = names(x = out_))

  if (length(x = add_) > 0) {
    out_[add_] <- 0
    cnt_cols_  <- c(cnt_cols_, add_)
    cli::cli_alert_info(
      text = "completed {length(x = add_)} count column{?s} absent from the corpus"
    )
  }

  # ---- mask -------------------------------------------------------------------------------------
  out_ <- v6_mask_items(.tab = out_, .cols = c(vol_cols_, cnt_cols_), .items = .items)

  # ---- geography --------------------------------------------------------------------------------
  geo_cols_ <- setdiff(x = names(x = .geo), y = "doc_id")
  out_      <- out_ |>
    dplyr::left_join(y = .geo, by = "doc_id") |>
    v5_fill_block(.flag = "HasDoc", .cols = geo_cols_)

  guard_ <- if ("HasGeoItem" %in% names(x = out_)) out_$HasGeoItem else out_$HasDoc

  out_ <- dplyr::mutate(
    .data = out_,
    GeoDispersion      = dplyr::if_else(GeoDispersion > 0, GeoDispersion, NA_real_),
    GeoDispersion_IGeo = dplyr::if_else(guard_ & GeoDispersion_IGeo > 0,
                                        GeoDispersion_IGeo, NA_real_)
  )

  # ---- shares, scope-matched --------------------------------------------------------------------
  # pEsgSen_I1A is nEsgSen_I1A / nSen_I1A - the share of ITEM 1A's sentences. Dividing an item
  # numerator by the document denominator would mix Esg intensity with how long Item 1A happens
  # to be.
  share_ <- function(.tab, .num, .den) {
    if (!.num %in% names(x = .tab) || !.den %in% names(x = .tab)) return(.tab)
    .tab[[stringi::stri_replace_first_fixed(str = .num, pattern = "n", replacement = "p")]] <-
      dplyr::if_else(condition = .tab[[.den]] > 0,
                     true      = .tab[[.num]] / .tab[[.den]],
                     false     = NA_real_)
    .tab
  }

  pairs_ <- tidyr::expand_grid(A = .spec$Name[.spec$Construct == "fire"],
                               B = .spec$Name[.spec$Construct != "fire"]) |>
    dplyr::mutate(Name = paste0(A, "X", B))

  for (sc_ in scopes_) {
    for (u_ in c("Sen", "Par")) {

      den_ <- paste0("n", u_, sc_)

      for (b_ in .spec$Name)  out_ <- share_(.tab = out_,
                                             .num = paste0("n", b_, u_, sc_), .den = den_)
      for (b_ in pairs_$Name) out_ <- share_(.tab = out_,
                                             .num = paste0("n", b_, u_, sc_), .den = den_)
    }

    # Bert: the denominator is the model's own scored paragraphs in that scope, so the share is
    # self-normalising and does not divide by nPar.
    for (i_ in seq_len(length.out = nrow(x = .bert))) {
      base_ <- paste0(.bert$Name[[i_]], .bert$LabelName[[i_]])
      cls_ <- if (.bert$ClOnly[[i_]]) "Cl" else if (.bert$NoCl[[i_]]) "" else c("", "Cl")
      for (cl_ in cls_) {
        sib_ <- paste0("n", .bert$Name[[i_]],
                       .bert$LabelName[.bert$Name == .bert$Name[[i_]]], cl_, sc_)
        sib_ <- intersect(x = sib_, y = names(x = out_))
        if (length(x = sib_) == 0) next
        base_col_ <- paste0("n", base_, cl_, sc_)
        if (!base_col_ %in% names(x = out_)) next
        tot_ <- rowSums(x = as.matrix(x = out_[sib_]), na.rm = TRUE)
        out_[[paste0("p", base_, cl_, sc_)]] <-
          dplyr::if_else(condition = tot_ > 0, true = out_[[base_col_]] / tot_, false = NA_real_)
      }
    }

    # numeric: term-list bases divide by n<List>Sen, Bert bases by their own n<Base>Sen
    for (b_ in c(.spec$Name, paste0(.bert$Name, .bert$LabelName))) {
      den_ <- paste0("n", b_, "Sen", sc_)
      for (st_ in c("nNum", "nNumAny")) {
        out_ <- share_(.tab = out_, .num = paste0(st_, b_, sc_), .den = den_)
      }
    }
  }

  # ---- binaries, LAST ---------------------------------------------------------------------------
  bin_ <- function(.tab, .src, .dst) {
    if (.src %in% names(x = .tab)) .tab[[.dst]] <- .tab[[.src]] > 0
    .tab
  }

  for (sc_ in scopes_) {

    for (b_ in .spec$Name) {
      out_ <- bin_(.tab = out_, .src = paste0("n", b_, "Sen", sc_),
                   .dst = paste0("b", b_, sc_))
    }

    for (b_ in pairs_$Name) {
      for (u_ in c("Sen", "Par")) {
        out_ <- bin_(.tab = out_, .src = paste0("n", b_, u_, sc_),
                     .dst = paste0("b", b_, u_, sc_))
      }
    }

    for (i_ in seq_len(length.out = nrow(x = .bert))) {
      base_ <- paste0(.bert$Name[[i_]], .bert$LabelName[[i_]])
      cls_  <- if (.bert$ClOnly[[i_]]) "Cl" else if (.bert$NoCl[[i_]]) "" else c("", "Cl")
      for (cl_ in cls_) {
        out_ <- bin_(.tab = out_, .src = paste0("n", base_, cl_, sc_),
                     .dst = paste0("b", base_, cl_, sc_))
      }
    }
  }

  dplyr::relocate(
    .data = out_,
    gvkey, datadate, doc_id, form_type, route, NDocs,
    HasDoc, HasBert, HasTerms, item_cover, n_item_kept,
    dplyr::any_of(x = c(paste0("HasI", .items), "HasGeoItem"))
  )
}

#' Does the panel match the specification, exactly?
#'
#' THE ONLY CHECK THAT CANNOT BE SATISFIED BY ACCIDENT. Every other guard in this file asks whether
#' a number is plausible; this one asks whether the thing that was built is the thing that was
#' specified. It is instant, and it is what makes a 6,272-column build reviewable at all.
v6_agg_validate <- function(.tab, .spec = v6_spec_names()) {

  have_ <- names(x = .tab)
  want_ <- .spec$Column

  miss_ <- setdiff(x = want_, y = have_)
  extra_ <- setdiff(x = have_, y = want_)

  cli::cli_h2(text = "Panel against specification")
  cli::cli_alert_info(
    text = "specified {scales::comma(x = length(x = want_))} | \\
            built {scales::comma(x = length(x = have_))}")

  if (length(x = miss_) == 0) {
    cli::cli_alert_success(text = "every specified column is present")
  } else {
    cli::cli_alert_danger(text = "{length(x = miss_)} specified column{?s} MISSING")
    print(x = utils::head(x = miss_, n = 25L))
  }

  if (length(x = extra_) == 0) {
    cli::cli_alert_success(text = "no column outside the specification")
  } else {
    cli::cli_alert_danger(text = "{length(x = extra_)} column{?s} NOT in the specification")
    print(x = utils::head(x = extra_, n = 25L))
  }

  invisible(x = list(Missing = miss_, Extra = extra_))
}

#' Missingness by family, classified by the V6 grammar.
#'
#' v5_agg_verify() classifies on the first character - n, p, r, u, Geo - which was right for V5 and
#' is wrong here in two ways that matter. Every `b` column falls through to "key / flag", so a
#' 1,384-column binary family is reported as though it were identifiers; and `route` is counted as
#' an "r per 1k words" column, a family V6 does not ship at all. Neither is a data defect, but a
#' missingness table that says "key / flag: 1,401 columns, 21.9% NA" is describing binaries under
#' the name of something that should never be NA.
#'
#' Families are read from v6_spec_names() rather than re-derived from the names, so this table and
#' the specification cannot disagree.
v6_agg_verify <- function(.tab, .spec = v6_spec_names()) {

  cli::cli_h2(text = "Panel")
  cli::cli_alert_info(
    text = "{scales::comma(x = nrow(x = .tab))} firm-year{?s}, {ncol(x = .tab)} column{?s}, \\
            {scales::comma(x = dplyr::n_distinct(.tab$gvkey))} firm{?s}"
  )

  stat_ <- function(.col) {
    dplyr::case_when(
      stringi::stri_startswith_fixed(str = .col, pattern = "b") ~ "b binary",
      stringi::stri_startswith_fixed(str = .col, pattern = "n") ~ "n count",
      stringi::stri_startswith_fixed(str = .col, pattern = "p") ~ "p share",
      .default = "other"
    )
  }

  out_ <- tibble::tibble(Col = names(x = .tab)) |>
    dplyr::left_join(y = dplyr::select(.data = .spec, Col = Column, Family), by = "Col") |>
    dplyr::mutate(
      Family = dplyr::coalesce(Family, "unspecified"),
      Stat   = dplyr::if_else(condition = Family %in% c("key", "volume", "geo"),
                              true = Family, false = stat_(.col = Col)),
      PctNA  = purrr::map_dbl(.x = Col,
                              .f = \(.c) 100 * mean(x = is.na(x = .tab[[.c]])))
    ) |>
    dplyr::summarise(
      .by      = c(Family, Stat),
      NCols    = dplyr::n(),
      MedPctNA = round(x = stats::median(x = PctNA), digits = 1),
      MaxPctNA = round(x = max(PctNA), digits = 1)
    ) |>
    dplyr::arrange(Family, Stat)

  print(x = as.data.frame(x = out_), row.names = FALSE)

  # A column that is NA everywhere carries no information and will be silently dropped by any
  # estimator. Better to name them here than to have the sweep report 6,224 specifications and
  # silently estimate fewer.
  dead_ <- tibble::tibble(Col = names(x = .tab)) |>
    dplyr::mutate(PctNA = purrr::map_dbl(.x = Col,
                                         .f = \(.c) 100 * mean(x = is.na(x = .tab[[.c]])))) |>
    dplyr::filter(PctNA == 100)

  if (nrow(x = dead_) > 0) {
    cli::cli_alert_warning(
      text = "{nrow(x = dead_)} column{?s} {?is/are} NA in every row"
    )
    print(x = utils::head(x = dead_$Col, n = 20L))
  } else {
    cli::cli_alert_success(text = "no column is NA in every row")
  }

  invisible(x = out_)
}



# V7 ---------------------------------------------------------------------------------------------
# The loader, and the three functions that differ from V6's build.

.V7_SECTIONS <- c("1", "1A", "1B", "2", "3", "4", "5", "6", "7", "7A", "8", "9", "9A", "9B",
                  "10", "11", "12", "13", "14", "15")

#' A section's last line is very often the NEXT section's header, leaked across the boundary.
#' Measured on 722077_2011: section_1 ends "Item 1A.". Stripped and counted, never silently.
.V7_LEAK <- "(?i)^\\s*ITEM\\s*[0-9]{1,2}\\s*[A-Z]?\\s*[.:)\\-]?\\s*$"


# Reading a year ---------------------------------------------------------------------------------

#' One year of EDGAR-CORPUS as paragraphs: one row per non-blank line of each section.
#'
#' arrow's JSON reader, not DuckDB's: the R duckdb build does not autoload the json extension and
#' a filing exceeds arrow's 1 MB default block, so the block size is raised. Split on "\n" in
#' stringi rather than in SQL so the same code path handles every year.
v7_read_year <- function(.dir, .year, .block_mb = 64L) {

  files_ <- list.files(path = file.path(.dir, .year), pattern = "[.]jsonl$", full.names = TRUE)
  if (length(x = files_) == 0) {
    cli::cli_abort(message = "no jsonl under {file.path(.dir, .year)}")
  }

  raw_ <- purrr::map(
    .x = files_,
    .f = \(.f) tibble::as_tibble(x = arrow::read_json_arrow(
      file         = .f,
      read_options = arrow::JsonReadOptions$create(block_size = .block_mb * 1024L^2)
    ))
  ) |>
    purrr::list_rbind()

  sec_cols_ <- paste0("section_", .V7_SECTIONS)
  miss_ <- setdiff(x = c("filename", "cik", "year", sec_cols_), y = names(x = raw_))
  if (length(x = miss_) > 0) {
    cli::cli_abort(message = "{(.year)}: column{?s} missing from the JSONL: {paste(miss_, collapse = ', ')}")
  }

  # ONE FILING PER CIK-YEAR is their filename's contract. Enforced rather than assumed.
  dup_ <- raw_ |> dplyr::count(cik, year) |> dplyr::filter(n > 1L)
  if (nrow(x = dup_) > 0) {
    cli::cli_alert_warning(text = "{(.year)}: {nrow(x = dup_)} cik-year{?s} duplicated - first kept")
    raw_ <- dplyr::slice_head(.data = raw_, n = 1L, by = c(cik, year))
  }

  long_ <- raw_ |>
    dplyr::transmute(
      DocID = stringi::stri_replace_last_regex(str = filename, pattern = "[.](htm|txt)$",
                                               replacement = ""),
      Cik   = as.integer(x = cik),
      Year  = as.integer(x = year),
      Route = dplyr::if_else(condition = stringi::stri_endswith_fixed(str = filename,
                                                                       pattern = ".htm"),
                             true = "html", false = "text"),
      dplyr::across(.cols = dplyr::all_of(x = sec_cols_), .fns = \(.x) dplyr::coalesce(.x, ""))
    ) |>
    tidyr::pivot_longer(cols = dplyr::all_of(x = sec_cols_), names_to = "ItemNum",
                        values_to = "Section") |>
    dplyr::mutate(ItemNum = stringi::stri_replace_first_fixed(str = ItemNum, pattern = "section_",
                                                              replacement = "")) |>
    dplyr::filter(nzchar(x = Section))

  # Lines. stri_split_fixed returns a list; lengths() gives the per-section count so the ids can
  # be built by rep() rather than by a row-wise loop.
  lines_ <- stringi::stri_split_fixed(str = long_$Section, pattern = "\n")
  n_     <- lengths(x = lines_)

  par_ <- tibble::tibble(
    DocID   = rep(x = long_$DocID,   times = n_),
    Cik     = rep(x = long_$Cik,     times = n_),
    Year    = rep(x = long_$Year,    times = n_),
    Route   = rep(x = long_$Route,   times = n_),
    ItemNum = rep(x = long_$ItemNum, times = n_),
    LineNo  = unlist(x = lapply(X = n_, FUN = seq_len), use.names = FALSE),
    Text    = unlist(x = lines_, use.names = FALSE)
  ) |>
    dplyr::filter(!v5_blank(.txt = Text))

  # The leaked next-section header: last non-blank line of a section that is only "Item N."
  par_ <- par_ |>
    dplyr::mutate(.by = c(DocID, ItemNum),
                  IsLast = LineNo == max(LineNo),
                  Leak   = IsLast & stringi::stri_detect_regex(str = Text, pattern = .V7_LEAK))
  n_leak_ <- sum(par_$Leak)
  par_ <- par_ |>
    dplyr::filter(!Leak) |>
    dplyr::select(-IsLast, -Leak, -LineNo) |>
    dplyr::arrange(DocID, factor(x = ItemNum, levels = .V7_SECTIONS)) |>
    dplyr::mutate(.by = DocID, ParID = dplyr::row_number())

  # A filing with no non-blank line in any section yields no paragraphs and would vanish from
  # reports without a trace. Measured 2011: 37 of 8,405. Counted here, written to _progress.
  n_empty_ <- dplyr::n_distinct(raw_$filename) - dplyr::n_distinct(par_$DocID)

  cli::cli_alert_info(
    text = "{(.year)}: {scales::comma(x = nrow(x = raw_))} filing{?s}, \\
            {scales::comma(x = nrow(x = par_))} paragraph{?s}, \\
            {scales::comma(x = n_leak_)} leaked header{?s} stripped, \\
            {n_empty_} empty filing{?s}"
  )
  attr(x = par_, which = "n_empty") <- n_empty_
  par_
}


# Derived columns --------------------------------------------------------------------------------

#' v5_derive()'s classification, on lines that are already paragraphs.
#'
#' The same four regexes decide the same four flags: exhibit, certification, looks-like-heading,
#' and therefore prose. Item headers are the lines v5_rx$Item matches - the first line of a
#' section in the ordinary case - and are kept, flagged, and excluded from prose. The
#' monotonicity block is not carried: items cannot go backwards when they are given.
v7_derive <- function(.par, .head_words = 12L) {
  .par |>
    dplyr::mutate(
      Text       = v5_squish(.txt = Text),
      NWords     = stringi::stri_count_words(str = Text),
      NChars     = nchar(x = Text),
      DigitShare = stringi::stri_count_regex(str = Text, pattern = v5_rx$Digit) /
        pmax(NChars, 1L),
      IsHeader   = stringi::stri_detect_regex(str = paste0(Text, " "), pattern = v5_rx$Item),
      IsExhibit  = stringi::stri_detect_regex(str = Text, pattern = v5_rx$ExhibNum) &
        stringi::stri_detect_regex(str = Text, pattern = v5_rx$ExhibCue),
      IsCert     = stringi::stri_detect_regex(str = Text, pattern = v5_rx$Cert),
      LooksHead  = NWords <= .head_words &
        !v5_is_term(.txt = Text) &
        (Text == toupper(x = Text) |
           stringi::stri_detect_regex(str = Text, pattern = v5_rx$TitleCse)),
      IsProse    = !IsHeader & !IsExhibit & !IsCert & !LooksHead
    )
}


# Sentences --------------------------------------------------------------------------------------

#' v5_sentences(), vectorised. SAME boundaries, SAME output, one ICU call instead of a pmap.
#'
#' pmap over 1.2M paragraphs a year builds 1.2M tibbles and is the slow path in V6. This calls
#' stri_locate_all_boundaries once on the whole vector and expands the list of matrices with
#' rep() and lengths(). It is a performance rewrite, not a semantic one, and v7_check_sentences()
#' proves that on a sample before anything is written.
v7_sentences <- function(.par, .method = "icu") {

  brk_ <- if (.method == "icu_filtered") {
    stringi::stri_opts_brkiter(type = "sentence", locale = "en_US@ss=standard")
  } else {
    stringi::stri_opts_brkiter(type = "sentence")
  }

  loc_ <- stringi::stri_locate_all_boundaries(str = .par$Text, opts_brkiter = brk_)

  # A paragraph with no boundary found is one span - v5_sentences()'s fallback.
  loc_ <- lapply(X = seq_along(along.with = loc_), FUN = \(.i) {
    m_ <- loc_[[.i]]
    if (is.null(x = m_) || all(is.na(x = m_))) matrix(data = c(1L, nchar(x = .par$Text[.i])),
                                                      ncol = 2) else m_
  })
  n_ <- vapply(X = loc_, FUN = nrow, FUN.VALUE = integer(length = 1L))

  start_ <- as.integer(x = unlist(x = lapply(X = loc_, FUN = \(.m) .m[, 1]), use.names = FALSE))
  end_   <- as.integer(x = unlist(x = lapply(X = loc_, FUN = \(.m) .m[, 2]), use.names = FALSE))
  txt_   <- rep(x = .par$Text, times = n_)

  tibble::tibble(
    DocID     = rep(x = .par$DocID, times = n_),
    ParID     = rep(x = .par$ParID, times = n_),
    SenID     = unlist(x = lapply(X = n_, FUN = seq_len), use.names = FALSE),
    CharStart = start_,
    CharEnd   = end_,
    NWords    = stringi::stri_count_words(str = substr(x = txt_, start = start_, stop = end_))
  )
}


#' REPRODUCE BEFORE DIFFING. The vectorised splitter against v5_sentences() on a sample - every
#' column must be identical, or v7_sentences() is not v5_sentences() and cannot ship.
v7_check_sentences <- function(.par, .n = 3000L, .seed = 42L) {
  set.seed(seed = .seed)
  smp_ <- .par |>
    dplyr::filter(IsProse) |>
    dplyr::slice_sample(n = min(.n, sum(.par$IsProse))) |>
    dplyr::mutate(IsTable = FALSE) |>
    dplyr::arrange(DocID, ParID)

  a_ <- v7_sentences(.par = smp_) |> dplyr::arrange(DocID, ParID, SenID)
  b_ <- v5_sentences(.par = smp_, .method = "icu") |> dplyr::arrange(DocID, ParID, SenID)

  # SAY WHERE, NOT JUST THAT. A count that matches with a column that does not is a specific
  # kind of difference, and the fix depends entirely on which column.
  if (nrow(x = a_) != nrow(x = b_)) {
    cli::cli_abort(message = c(
      "v7_sentences() finds {nrow(x = a_)} sentences, v5_sentences() {nrow(x = b_)}, on the same \\
       {nrow(x = smp_)} paragraphs.",
      "i" = "The boundaries differ. That is a semantic difference and cannot ship."))
  }

  cols_ <- c("DocID", "ParID", "SenID", "CharStart", "CharEnd", "NWords")
  diff_ <- purrr::map_int(.x = cols_, .f = \(.c) sum(a_[[.c]] != b_[[.c]]))
  names(x = diff_) <- cols_
  cls_  <- purrr::map_chr(.x = cols_, .f = \(.c)
    paste0(class(x = a_[[.c]])[1], "/", class(x = b_[[.c]])[1]))
  names(x = cls_) <- cols_

  if (any(diff_ > 0)) {
    cli::cli_alert_danger(text = "{nrow(x = a_)} sentences on both sides, but columns differ:")
    print(x = data.frame(Column = cols_, NDiffer = unname(obj = diff_),
                         Class_v7_v5 = unname(obj = cls_)), row.names = FALSE)
    bad_ <- which(rowSums(x = sapply(X = cols_, FUN = \(.c) a_[[.c]] != b_[[.c]])) > 0)
    cli::cli_h3(text = "first rows that differ - v7 then v5")
    print(x = as.data.frame(x = a_[utils::head(x = bad_, n = 5L), cols_]), row.names = FALSE)
    print(x = as.data.frame(x = b_[utils::head(x = bad_, n = 5L), cols_]), row.names = FALSE)
    # Offsets and ids are the semantics. NWords is derived from them, so a difference there alone
    # is a tokenizer-call difference and is reported rather than fatal - but it should not happen.
    if (any(diff_[c("DocID", "ParID", "SenID", "CharStart", "CharEnd")] > 0)) {
      cli::cli_abort(message = "sentence identity or offsets differ - v7_sentences() cannot ship")
    }
    cli::cli_alert_warning(text = "only NWords differs - offsets identical; investigate before trusting counts")
    return(invisible(x = FALSE))
  }

  cli::cli_alert_success(
    text = "v7_sentences() reproduces v5_sentences() on {scales::comma(x = nrow(x = smp_))} \\
            paragraph{?s}, {scales::comma(x = nrow(x = a_))} sentence{?s}, every column identical"
  )
  invisible(x = TRUE)
}


# Writing a year ---------------------------------------------------------------------------------

#' Append one year to the store: reports, paragraphs, sentences, _progress.
v7_write_year <- function(.path_db, .par, .sen, .year, .secs, .n_skipped = 0L) {

  con_ <- v5_connect(.path_db = .path_db, .read_only = FALSE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)

  # Idempotent: a year that is already in is replaced, never doubled.
  for (t_ in c("reports", "paragraphs", "sentences")) {
    DBI::dbExecute(conn = con_, statement = sprintf(
      fmt = "DELETE FROM %s WHERE doc_id IN (SELECT doc_id FROM reports WHERE filed_year = %d)",
      t_, .year))
  }
  DBI::dbExecute(conn = con_, statement = sprintf(
    fmt = "DELETE FROM _progress WHERE filed_year = %d", .year))

  rep_ <- .par |>
    dplyr::summarise(
      .by         = c(DocID, Cik, Year, Route),
      n_par       = dplyr::n(),
      n_prose     = sum(IsProse),
      n_item_kept = dplyr::n_distinct(ItemNum)
    ) |>
    dplyr::left_join(
      y  = dplyr::summarise(.data = .sen, .by = DocID, n_sentence = dplyr::n()),
      by = "DocID"
    ) |>
    dplyr::transmute(
      doc_id = DocID, cik = Cik, form_type = "10-K", date_filed = as.Date(x = NA),
      filed_year = Year, n_block = NA_integer_, route = Route, parsed = TRUE,
      n_par, n_prose, n_table = 0L,
      n_sentence = dplyr::coalesce(n_sentence, 0L), n_item_kept,
      item_cover = 1, n_forced = 0L
    )

  par_out_ <- .par |>
    dplyr::transmute(
      doc_id = DocID, par_id = ParID, node_type = "line", is_table = FALSE,
      table_src = NA_character_, is_prose = IsProse, item_num = ItemNum,
      item_header = IsHeader, forced = FALSE, n_cell = NA_integer_, n_row = NA_integer_,
      n_words = NWords, n_chars = NChars, digit_share = DigitShare, text = Text
    )

  sen_out_ <- .sen |>
    dplyr::transmute(doc_id = DocID, par_id = ParID, sen_id = SenID,
                     char_start = CharStart, char_end = CharEnd, n_words = NWords)

  DBI::dbAppendTable(conn = con_, name = "reports",    value = rep_)
  DBI::dbAppendTable(conn = con_, name = "paragraphs", value = par_out_)
  DBI::dbAppendTable(conn = con_, name = "sentences",  value = sen_out_)
  DBI::dbAppendTable(conn = con_, name = "_progress", value = tibble::tibble(
    filed_year = .year, n_docs = nrow(x = rep_), n_par = nrow(x = par_out_),
    n_sentence = nrow(x = sen_out_), n_skipped = as.integer(x = .n_skipped),
    limit_used = NA_integer_,
    secs = .secs, written_at = Sys.time()))

  invisible(x = rep_)
}


# One year, end to end ---------------------------------------------------------------------------

#' Load one year. Skips a year already in _progress unless forced.
#'
#' `.check` runs v7_check_sentences() before writing - on by default for the first year of a
#' store, cheap enough to leave on.
v7_load_year <- function(.path_db, .dir, .year, .force = FALSE, .check = TRUE,
                         .sen_method = "icu") {

  con_  <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  done_ <- DBI::dbGetQuery(conn = con_, statement = sprintf(
    fmt = "SELECT n_docs FROM _progress WHERE filed_year = %d", .year))
  DBI::dbDisconnect(conn = con_, shutdown = TRUE)
  if (nrow(x = done_) > 0 && !.force) {
    cli::cli_alert_info(text = "{(.year)}: already loaded ({scales::comma(x = done_$n_docs)} docs) - skipped")
    return(invisible(x = NULL))
  }

  t0_  <- Sys.time()
  par_ <- v7_read_year(.dir = .dir, .year = .year)
  n_empty_ <- attr(x = par_, which = "n_empty")
  par_ <- v7_derive(.par = par_)
  if (.check) v7_check_sentences(.par = par_)

  # Sentences on PROSE paragraphs only. A non-prose paragraph - header, exhibit line, certification -
  # gets no sentence rows, which is what V6 does for tables.
  sen_ <- v7_sentences(.par = dplyr::filter(.data = par_, IsProse), .method = .sen_method)

  secs_ <- as.numeric(x = difftime(time1 = Sys.time(), time2 = t0_, units = "secs"))
  rep_  <- v7_write_year(.path_db = .path_db, .par = par_, .sen = sen_, .year = .year,
                         .secs = secs_, .n_skipped = n_empty_)

  cli::cli_alert_success(
    text = "{sprintf(fmt = '%d: %s docs, %s paragraphs (%.1f%% prose), %s sentences, %.1f words/sentence, %.0f s',
                     .year, scales::comma(x = nrow(x = rep_)), scales::comma(x = nrow(x = par_)),
                     100 * mean(x = par_$IsProse), scales::comma(x = nrow(x = sen_)),
                     stats::median(x = sen_$NWords), secs_)}"
  )
  invisible(x = rep_)
}


# What is in the store ---------------------------------------------------------------------------

#' Per-year shape, and per-item presence. The numbers to hold against V6's.
v7_report <- function(.path_db) {

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)

  cli::cli_h3(text = "by year")
  DBI::dbGetQuery(conn = con_, statement = "
    SELECT r.filed_year AS year, COUNT(*) AS docs,
           round(100.0 * avg(CASE WHEN route = 'html' THEN 1 ELSE 0 END), 1) AS pct_html,
           round(avg(n_par))       AS par_per_doc,
           round(avg(n_prose))     AS prose_per_doc,
           round(avg(n_sentence))  AS sen_per_doc,
           round(1.0 * sum(n_sentence) / nullif(sum(n_prose), 0), 2) AS sen_per_prose_par
      FROM reports r GROUP BY 1 ORDER BY 1") |>
    as.data.frame() |> print(row.names = FALSE)

  cli::cli_h3(text = "sentence length - V6 measured 18-28 words for real 10-K sentences")
  DBI::dbGetQuery(conn = con_, statement = "
    SELECT quantile_cont(n_words, 0.10) AS p10, quantile_cont(n_words, 0.50) AS med,
           quantile_cont(n_words, 0.90) AS p90, COUNT(*) AS n FROM sentences") |>
    as.data.frame() |> print(row.names = FALSE)

  cli::cli_h3(text = "item presence, % of documents with >= 1 prose paragraph in the item")
  DBI::dbGetQuery(conn = con_, statement = "
    WITH d AS (SELECT COUNT(DISTINCT doc_id) AS n FROM reports)
    SELECT item_num,
           round(100.0 * COUNT(DISTINCT doc_id) / (SELECT n FROM d), 1) AS pct_docs
      FROM paragraphs WHERE is_prose GROUP BY 1
     ORDER BY try_cast(regexp_extract(item_num, '^[0-9]+') AS INTEGER), item_num") |>
    as.data.frame() |> print(row.names = FALSE)

  invisible(x = NULL)
}


# All years ------------------------------------------------------------------------------------------

#' Load a range of years, skipping any already in _progress. Interrupt and re-run to resume.
#'
#' Sequential by design: DuckDB takes one writer, and 246 s a year measured on 2011 puts the whole
#' 1993-2020 range at about two hours - short enough that daemons plus a serialised writer would
#' buy back less than they cost to keep correct. The sentence check stays ON for every year; it is
#' 3,000 paragraphs and it is the only guard on the splitter.
v7_load_years <- function(.path_db, .dir, .years, .force = FALSE, .check = TRUE) {
  t0_ <- Sys.time()
  for (y_ in .years) {
    v7_load_year(.path_db = .path_db, .dir = .dir, .year = y_, .force = .force, .check = .check)
  }
  cli::cli_alert_success(
    text = "{sprintf(fmt = '%d year(s) in %.1f min', length(x = .years),
                     as.numeric(x = difftime(Sys.time(), t0_, units = 'mins')))}"
  )
  invisible(x = NULL)
}


# Keys -------------------------------------------------------------------------------------------

#' gvkey and datadate for every V7 document. Replaces v5_agg_keys(), which joins on the accession.
#'
#' V7 has no accession - a document is `cik_year`. 001d's reports store carries cik, gvkey and
#' datadate for every filing in the sample, so the join is cik + year(datadate) against V7's
#' cik + filed_year, where filed_year is EDGAR-CORPUS's period-of-report year. Measured
#' 2026-09-02 as the key on which the two parsers agree about Item 1A presence (85%, best of three
#' candidate offsets); the other two were filing-year offsets and lost.
#'
#' Returns list(All =, Primary =) with the columns v6_agg_assemble() reads, so it drops into the
#' build where v5_agg_keys() was. One document per cik-year on V7's side by construction; more
#' than one document per FIRM-year happens when two CIKs map to one gvkey in a year, and the
#' primary is then the longer one, deterministically.
v7_agg_keys <- function(.path_db, .path_rep, .path_sample) {

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(conn = con_, statement = sprintf(
    fmt = "ATTACH IF NOT EXISTS '%s' AS src (READ_ONLY)", .path_rep))

  keys_ <- DBI::dbGetQuery(conn = con_, statement = "
    WITH s AS (
      SELECT DISTINCT CAST(cik AS INTEGER) AS cik, gvkey,
             CAST(datadate AS DATE) AS datadate,
             CAST(EXTRACT(year FROM CAST(datadate AS DATE)) AS INTEGER) AS yr
        FROM src.reports
       WHERE cik IS NOT NULL AND datadate IS NOT NULL
    )
    SELECT r.doc_id, s.gvkey, s.datadate,
           r.form_type, r.route, r.n_prose, r.n_par, r.item_cover, r.n_item_kept
      FROM reports r
      JOIN s ON s.cik = r.cik AND s.yr = r.filed_year
     WHERE r.parsed") |>
    tibble::as_tibble() |>
    dplyr::mutate(datadate = as.Date(x = datadate))

  n_all_ <- DBI::dbGetQuery(conn = con_,
                            statement = "SELECT COUNT(*) AS N FROM reports WHERE parsed")$N
  if (nrow(x = keys_) == 0) {
    cli::cli_abort(message = c(
      "No V7 document joined to 001d's reports store on cik + year.",
      "i" = "Check that src.reports carries cik and datadate, and that filed_year is populated."))
  }
  cli::cli_alert_info(
    text = "{sprintf(fmt = '%s of %s documents carry gvkey and datadate (%.1f%%)',
                     scales::comma(x = dplyr::n_distinct(keys_$doc_id)), scales::comma(x = n_all_),
                     100 * dplyr::n_distinct(keys_$doc_id) / n_all_)}"
  )

  smp_ <- arrow::read_parquet(file = .path_sample, col_select = c("gvkey", "datadate")) |>
    dplyr::distinct(gvkey, datadate) |>
    dplyr::mutate(datadate = as.Date(x = datadate))
  n_before_ <- nrow(x = keys_)
  keys_ <- dplyr::semi_join(x = keys_, y = smp_, by = c("gvkey", "datadate"))
  cli::cli_alert_info(
    text = "{sprintf(fmt = '%s in sample scope (%s dropped)', scales::comma(x = nrow(x = keys_)),
                     scales::comma(x = n_before_ - nrow(x = keys_)))}"
  )

  out_ <- keys_ |>
    dplyr::mutate(NegPar = -dplyr::coalesce(n_par, 0L)) |>
    dplyr::group_by(gvkey, datadate) |>
    dplyr::arrange(NegPar, doc_id, .by_group = TRUE) |>
    dplyr::mutate(NDocs = dplyr::n(), SizeRank = dplyr::row_number(),
                  IsPrimary = SizeRank == 1L) |>
    dplyr::ungroup() |>
    dplyr::select(-NegPar)

  n_fy_ <- dplyr::n_distinct(out_$gvkey, out_$datadate)
  if (sum(out_$IsPrimary) != n_fy_) {
    cli::cli_abort(message = "{sum(out_$IsPrimary)} primaries for {n_fy_} firm-years")
  }
  cli::cli_alert_success(
    text = "{sprintf(fmt = '%s firm-years, %s with more than one document',
                     scales::comma(x = n_fy_),
                     scales::comma(x = sum(out_$NDocs[out_$IsPrimary] > 1L)))}"
  )
  list(All = out_, Primary = dplyr::filter(.data = out_, IsPrimary))
}


# Item presence ----------------------------------------------------------------------------------

#' HasI<x> and HasGeoItem from the paragraphs table. Replaces v6_agg_items(), which reads item_v2.
#'
#' An item is present when its section has at least `.min_words` prose words. The default is 0,
#' which is "the section is non-empty" - the closest analogue to V6's header flag, where a located
#' item whose span held only "Not applicable" was present with two words. Analysis-time floors
#' belong in the analysis; this is the store's fact.
v7_agg_items <- function(.path_db, .items = .V5_ITEMS, .geo_items = .V5_GEO_ITEMS,
                         .min_words = 0L) {

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)

  flags_ <- paste(sprintf(
    fmt = "MAX(CASE WHEN item_num = '%s' THEN 1 ELSE 0 END)::BOOLEAN AS HasI%s", .items, .items),
    collapse = ",\n           ")
  geo_ <- sprintf(
    fmt = "MAX(CASE WHEN regexp_matches(item_num, '^(%s)[A-Z]?$') THEN 1 ELSE 0 END)::BOOLEAN AS HasGeoItem",
    paste(.geo_items, collapse = "|"))

  out_ <- DBI::dbGetQuery(conn = con_, statement = sprintf(fmt = "
    WITH sec AS (
      SELECT doc_id, item_num, SUM(CASE WHEN is_prose THEN n_words ELSE 0 END) AS w
        FROM paragraphs GROUP BY 1, 2
    )
    SELECT doc_id,
           %s,
           %s
      FROM sec WHERE w >= %d GROUP BY doc_id", flags_, geo_, .min_words)) |>
    tibble::as_tibble()

  cli::cli_alert_success(text = "item flags for {scales::comma(x = nrow(x = out_))} document{?s}")
  out_
}




#' The Bert registry the build should use: full if the scores store has rows, empty if not.
#'
#' Creates the scores store if it does not exist, so §3 of the qmd can run before §5 has. Called
#' by v7_agg_build() and by the qmd's verify chunk, so both see the same answer.
v7_bert_used <- function(.path_scores, .bert = .V6_BERT) {
  if (!file.exists(.path_scores)) {
    cli::cli_alert_warning(
      text = "no scores store - creating an empty one at {basename(path = .path_scores)}")
    v5_score_store_init(.path_scores = .path_scores)
  }
  con_ <- v5_connect(.path_db = .path_scores, .read_only = TRUE)
  on.exit(expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE), add = TRUE)
  n_ <- DBI::dbGetQuery(conn = con_, statement = "SELECT COUNT(*) AS N FROM _scored")$N
  if (n_ == 0) {
    cli::cli_alert_warning(
      text = "scores store is EMPTY - building WITHOUT ClimateBERT. The bert family and the \\
              numeric-over-Bert bases are ABSENT from this panel, not zero."
    )
    return(.bert[0, ])
  }
  .bert
}

# The V7 panel -----------------------------------------------------------------------------------

#' Build the V7 panel. v6_agg_build() with the keys and the item flags swapped, and no ladder.
#'
#' WITHOUT CLIMATEBERT UNTIL THE SCORES EXIST. If the scores store is empty, `.bert` is set to zero
#' rows for the spec, the aggregators AND the assembly. That last one matters: v6_agg_assemble()
#' completes its count grid from the spec, so passing the full .V6_BERT with no scores would mint
#' 720 Bert columns and 1,104 numeric-over-Bert columns as ZERO - the zero-fill defect, on a
#' family that was never measured. A zero-row .bert makes "V7 without ClimateBERT" a smaller,
#' honest specification rather than a larger, false one. Rebuild with the full .V6_BERT once
#' the Python scorer has run against the V7 store.
v7_agg_build <- function(.path_db, .path_terms, .path_scores, .path_rep, .path_sample,
                         .path_out = NULL, .items = .V5_ITEMS, .lists = .V6_LISTS,
                         .bert = .V6_BERT, .geo_list = "states", .validate = TRUE) {

  cli::cli_h1(text = "V7 firm-year panel")
  t0_ <- Sys.time()

  bert_ <- v7_bert_used(.path_scores = .path_scores, .bert = .bert)

  cli::cli_h2(text = "Keys")
  keys_ <- v7_agg_keys(.path_db = .path_db, .path_rep = .path_rep, .path_sample = .path_sample)

  cli::cli_h2(text = "Item presence")
  flag_ <- v7_agg_items(.path_db = .path_db, .items = .items, .geo_items = .V5_GEO_ITEMS)

  cli::cli_h2(text = "Digit flags")
  v6_num_build(.path_db = .path_db)

  cli::cli_h2(text = "Volume")
  vol_ <- v6_agg_volume(.path_db = .path_db, .items = .items)

  cli::cli_h2(text = "Term lists")
  lst_ <- v6_agg_lists(.path_terms = .path_terms, .path_db = .path_db, .lists = .lists,
                       .items = .items)

  cli::cli_h2(text = "Intersections")
  pai_ <- v6_agg_pairs(.path_terms = .path_terms, .path_db = .path_db, .lists = .lists,
                       .items = .items)

  mod_ <- NULL
  if (nrow(x = bert_) > 0) {
    cli::cli_h2(text = "ClimateBERT")
    mod_ <- v6_agg_models(.path_scores = .path_scores, .path_db = .path_db, .bert = bert_,
                          .items = .items)
  }

  cli::cli_h2(text = "Numerical intensity")
  num_ <- v6_agg_numeric(.path_db = .path_db, .path_terms = .path_terms,
                         .path_scores = .path_scores, .lists = .lists, .bert = bert_,
                         .items = .items)

  cli::cli_h2(text = "Geography")
  geo_ <- v6_agg_geo(.path_terms = .path_terms, .path_db = .path_db, .list_id = .geo_list)

  cli::cli_h2(text = "Assemble")
  out_ <- v6_agg_assemble(
    .path_sample = .path_sample, .keys = keys_, .volume = vol_, .lists = lst_,
    .models = mod_, .pairs = pai_, .numeric = num_, .geo = geo_,
    .items_flag = flag_, .items = .items, .spec = .lists, .bert = bert_
  )

  v6_verify_mask(.tab = out_, .items = .items)

  spec_ <- v6_spec_names(.lists = .lists, .bert = bert_)
  if (.validate) v6_agg_validate(.tab = out_, .spec = spec_)

  cli::cli_alert_success(
    text = "{sprintf(fmt = '%s firm-years x %d columns | %s with a document (%.1f%%) | %.1f min',
                     scales::comma(x = nrow(x = out_)), ncol(x = out_),
                     scales::comma(x = sum(out_$HasDoc)), 100 * mean(x = out_$HasDoc),
                     as.numeric(x = difftime(Sys.time(), t0_, units = 'mins')))}"
  )

  if (!is.null(x = .path_out)) {
    dir.create(path = dirname(path = .path_out), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(x = out_, sink = .path_out)
    cli::cli_alert_success(text = "Written: {basename(path = .path_out)}")
  }
  invisible(x = out_)
}


# Against V6 ---------------------------------------------------------------------------------------

#' The acceptance test: V7's document-scope columns against V6's on the same firm-years.
#'
#' Document scope has no ladder on either side, so on identical filings the two panels should
#' agree closely - the 2026-09-02 comparison put per-filing agreement on the fire binaries at
#' 99.8% and on ESG at 95-98%. Anything that agrees worse than that here is a V7 build defect,
#' not a corpus difference, because the corpus difference was already measured and was smaller.
v7_against_v6 <- function(.path_v7, .path_v6,
                          .cols = c("bFireOrig", "bEsg", "bKww23", "nWords", "nSen",
                                    "pEsgSen", "pKww23Sen", "GeoDispersion0")) {

  a_ <- arrow::open_dataset(sources = .path_v7) |>
    dplyr::select(dplyr::all_of(x = c("gvkey", "datadate", "HasDoc", .cols))) |>
    dplyr::collect()
  b_ <- arrow::open_dataset(sources = .path_v6) |>
    dplyr::select(dplyr::all_of(x = c("gvkey", "datadate", "HasDoc", .cols))) |>
    dplyr::collect()
  j_ <- dplyr::inner_join(x = dplyr::filter(.data = a_, HasDoc),
                          y = dplyr::filter(.data = b_, HasDoc),
                          by = c("gvkey", "datadate"), suffix = c("_v7", "_v6"))

  cli::cli_alert_info(
    text = "{scales::comma(x = nrow(x = j_))} firm-year{?s} with a document in both panels"
  )

  purrr::map(.x = .cols, .f = \(.c) {
    x_ <- as.numeric(x = j_[[paste0(.c, "_v7")]]); y_ <- as.numeric(x = j_[[paste0(.c, "_v6")]])
    ok_ <- !is.na(x = x_) & !is.na(x = y_)
    tibble::tibble(
      Column   = .c,
      N        = sum(ok_),
      MeanV7   = signif(x = mean(x = x_[ok_]), digits = 4),
      MeanV6   = signif(x = mean(x = y_[ok_]), digits = 4),
      Spearman = round(x = stats::cor(x = x_[ok_], y = y_[ok_], method = "spearman"), digits = 3),
      PctEqual = round(x = 100 * mean(x = x_[ok_] == y_[ok_]), digits = 1)
    )
  }) |>
    purrr::list_rbind() |>
    as.data.frame() |>
    print(row.names = FALSE)
  invisible(x = j_)
}
