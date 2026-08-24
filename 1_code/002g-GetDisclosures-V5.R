# 002g-GetDisclosures-V5: paragraph and sentence store ----------------------------------------
#
# WHAT THIS IS
# V5 rebuilds the UNIT the text measures are computed on. V3 collapses all whitespace and THEN
# splits sentences, so paragraph structure is destroyed before sentences are found and a sentence
# can silently span what was a paragraph boundary. V5 finds paragraphs FIRST and splits sentences
# WITHIN them, which makes that impossible by construction.
#
# OUTPUT
#   reports     one row per document: metadata, route, per-document quality columns
#   paragraphs  one row per segment: node type, table/prose flags, item, features, text
#   sentences   one row per sentence, as CHARACTER OFFSETS into the paragraph text
#   v_sentences view materialising sentence text from the offsets
#   _progress   years written - the resume mechanism
#   _skipped    documents DEFERRED by the size cap - never silently lost
#
# SIZE CAP AND DEFERRAL
# A handful of very large blocks dominate parse time. `.max_mb` skips them so a build finishes,
# but every skipped document is RECORDED in `_skipped`, reported by v5_verify, and picked up by
# v5_run_skipped(). 654 blocks corpus-wide sit over 12 MB, concentrated at ~2% of 2020-2023, and
# large filings belong to large firms - non-random in both year and size. Deferral is acceptable;
# loss is not.
#
# THE ALGORITHM, with the evidence for each step (064-Evidence, 1,201 documents)
#   0 SOURCE     docs.block from 001d - primary document only
#   1 PREAMBLE   strip SGML markers, verbatim from fp_extract_text
#   2 ENTITIES   named plus numeric; numeric ones end in ";" and corrupted the terminal
#                punctuation test before they were decoded
#   3 ROUTE      <p> or <div>? NOT year. <table> alone is not evidence of HTML - testing for it
#                misrouted every pre-2003 filing in an earlier pass
#   4 TABLES     DATA if n_cell >= 12 AND n_row >= 3, else LAYOUT and UNWRAPPED. Tested on four
#                features it was not tuned on and all four separate: digit share 0.111 vs 0.000,
#                words-per-cell 0.5 vs 4.0, bullets 2.3% vs 38.7%, item headers 0.1% vs 5.4%
#   5 WALK       one XPath, document order, leaf blocks only, spacers dropped
#   6 ITEMS      detected at BLOCK level BEFORE fusion; monotonic filter removes the table of
#                contents and cross-references; suffixes KEPT so "1A" does not collapse into "1"
#   7 FUSION     join prose blocks until the text ends on terminal punctuation. Median words
#                66 -> 74 (html), 27 -> 49 (text); CleanStart, which the rule does NOT use, rose
#   8 PROSE      exclude exhibit entries, certifications, missed headings
#   9 SENTENCES  split within paragraphs; data tables not split
#
# KNOWN IMPERFECT, shipped as flags rather than hidden
#   - route is ENDOGENOUS: a filing-agent property, hence correlated with size and industry
#   - item coverage is weaker on html: 15 of 813 documents reached 95-100%, vs 80 of 388 on text
#   - 21.8% of html documents yield no item: an ABSENCE, not a detector failure
#   - the item fill-forward is UNRESTRICTED; three stops were tested and rejected
#   - the sentence splitter is NOT settled; re-runnable from stored paragraphs, so reversible
#
# CONVENTIONS
#   EVERY argument is named. Any call taking more than one argument is written one argument per
#   line with a short note on what it is for, so a reader never needs the signature. Arguments
#   passed through `...` (tibble columns, mutate expressions) carry data rather than options, so
#   they get one line each but no forced comment.
#   Native pipe; explicit package::function; dot-prefixed arguments; underscore-suffixed locals;
#   cli for logging; no library() calls; parenthesised cli interpolation - {(.x)} not {.x}.


# Store ----------------------------------------------------------------------------------------

#' Open a DuckDB connection.
v5_connect <- function(.path_db, .read_only = TRUE) {

  con_ <- DBI::dbConnect(
    drv       = duckdb::duckdb(),   # DuckDB driver object
    dbdir     = .path_db,           # store file on disk
    read_only = .read_only          # FALSE only for the writer; readers must not lock it
  )

  # DuckDB's OWN progress bar writes carriage returns to the console and overwrites any cli
  # progress bar running above it - which is what hid the term-flagging progress on 2026-08-24.
  # Disabled here rather than at each call site so no connection can be opened without it: the
  # three separate connect helpers this function replaced each had this SET, and it was lost in
  # the merge. It is a session setting, so it works on a read-only connection too.
  DBI::dbExecute(conn = con_, statement = "SET enable_progress_bar = false")

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


#' Years already written, and whether each was written IN FULL.
#'
#' A year run with `.limit` is TRUNCATED, not done. Recording only that it ran meant a later
#' unlimited build skipped it and left a silently incomplete panel that looked finished - the same
#' class of failure as a size cap that drops documents without saying so.
v5_progress <- function(.path_db) {

  empty_ <- tibble::tibble(
    filed_year = integer(length = 0),
    n_docs     = integer(length = 0),
    limit_used = integer(length = 0)
  )

  if (!file.exists(.path_db)) return(empty_)

  con_ <- v5_connect(
    .path_db   = .path_db,   # store to inspect
    .read_only = TRUE        # never write from a query helper
  )
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  if (!"_progress" %in% DBI::dbListTables(conn = con_)) return(empty_)

  DBI::dbGetQuery(
    conn      = con_,
    statement = "SELECT filed_year, n_docs, limit_used FROM _progress ORDER BY filed_year"
  ) |>
    tibble::as_tibble()
}


#' Years written in full. Truncated years are NOT counted as done.
v5_years_done <- function(.path_db, .limit = NULL) {

  prog_ <- v5_progress(.path_db = .path_db)
  if (nrow(x = prog_) == 0) return(integer(length = 0))

  # Complete if it was written without a limit, or with the SAME limit now being requested -
  # re-running the identical smoke build should still resume.
  done_ <- if (is.null(x = .limit)) {
    is.na(x = prog_$limit_used)
  } else {
    is.na(x = prog_$limit_used) | prog_$limit_used == as.integer(x = .limit)
  }

  sort(x = prog_$filed_year[done_])
}


# Text preparation -----------------------------------------------------------------------------

#' Strip the SGML markers. VERBATIM from 001d's fp_extract_text.
v5_pre <- function(.txt) {

  o_ <- gsub(
    pattern     = "<(TYPE|SEQUENCE|FILENAME|DESCRIPTION)>",  # header markers
    replacement = "",                                        # removed entirely
    x           = .txt,                                      # raw block
    perl        = TRUE                                       # PCRE for the alternation
  )
  o_ <- gsub(
    pattern     = "</?TEXT>",   # opening or closing TEXT marker
    replacement = "",
    x           = o_,
    perl        = TRUE
  )
  o_ <- gsub(
    pattern     = "</?DOCUMENT>",   # opening or closing DOCUMENT marker
    replacement = "",
    x           = o_,
    perl        = TRUE
  )
  gsub(
    pattern     = "<PAGE>",   # page break marker
    replacement = "",
    x           = o_,
    fixed       = TRUE        # literal string, no regex engine needed
  )
}


#' Named and numeric entities.
#'
#' The numeric ones matter: `&#146;` ends in ";" and was being read as terminal punctuation by
#' the fusion rule before it was decoded.
v5_ent <- function(.txt) {

  o_ <- gsub(pattern = "&nbsp;", replacement = " ", x = .txt, fixed = TRUE)
  o_ <- gsub(pattern = "&amp;",  replacement = "&", x = o_,   fixed = TRUE)
  o_ <- gsub(pattern = "&lt;",   replacement = "<", x = o_,   fixed = TRUE)
  o_ <- gsub(pattern = "&gt;",   replacement = ">", x = o_,   fixed = TRUE)

  o_ <- gsub(
    pattern     = "&#14[56];",   # curly single quotes
    replacement = "'",
    x           = o_,
    perl        = TRUE
  )
  o_ <- gsub(
    pattern     = "&#14[78];",   # curly double quotes
    replacement = '"',
    x           = o_,
    perl        = TRUE
  )
  o_ <- gsub(
    pattern     = "&#15[01];",   # en and em dashes
    replacement = "-",
    x           = o_,
    perl        = TRUE
  )
  gsub(
    pattern     = "&#160;",   # non-breaking space
    replacement = " ",
    x           = o_,
    fixed       = TRUE
  )
}


#' THE DISPATCH.
#'
#' <table> alone is NOT evidence of HTML: 1990s SGML filings wrap fixed-width ASCII in <TABLE>,
#' <CAPTION>, <S>, <C>, <FN>. An earlier version tested for it and sent every 1990s filing to
#' libxml2, which parsed ASCII financial statements as HTML tables.
v5_has_blocks <- function(.txt) {
  grepl(
    pattern = "<(p|div|P|DIV)[ >/]",   # tag name must be followed by a delimiter, so <pre> misses
    x       = .txt,                    # pre-processed block
    perl    = TRUE                     # PCRE for the alternation
  )
}


#' Fallback text extraction when libxml2 cannot parse the document.
v5_strip_tags <- function(.txt) {

  o_ <- gsub(
    pattern     = "<(script|style)[^>]*>.*?</\\1>",   # non-content elements, with their contents
    replacement = " ",
    x           = .txt,
    perl        = TRUE,          # backreference \\1 and lazy quantifier
    ignore.case = TRUE           # filings mix cases freely
  )
  o_ <- gsub(
    pattern     = "<[^>]*>",   # any remaining tag
    replacement = " ",
    x           = o_,
    perl        = TRUE
  )
  v5_ent(.txt = o_)
}


# Classification patterns ----------------------------------------------------------------------

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


#' Page furniture: dot leaders, bare page numbers, repeating page headers.
#'
#' Without this, fusion glued an entire table of contents into one 275-word segment.
v5_is_furniture <- function(.txt) {

  w_ <- stringi::stri_count_words(str = .txt)

  stringi::stri_detect_regex(str = .txt, pattern = v5_rx$Leader) |
    (w_ <= 12L & stringi::stri_detect_regex(str = .txt, pattern = v5_rx$PageNum)) |
    (w_ <=  6L & stringi::stri_detect_regex(str = .txt, pattern = v5_rx$Furn))
}


#' Item label for ONE block, pre-fusion. Returns "1", "1A", "7" or NA.
#'
#' The word limit belongs HERE and not after fusion: a pre-fusion header block is short by
#' construction, whereas a fused paragraph carrying a header is not. Getting this backwards cut
#' detected headers from ~13 per document to 5.
v5_item_block <- function(.txt, .max_words = 25L, .max_item = 16L) {

  nw_ <- stringi::stri_count_words(str = .txt)

  m_ <- stringi::stri_match_first_regex(
    str     = .txt,        # candidate block
    pattern = v5_rx$Item   # group 1 = number, group 2 = optional suffix letter
  )

  num_ <- suppressWarnings(expr = as.integer(x = m_[, 2]))
  sfx_ <- toupper(x = dplyr::coalesce(m_[, 3], ""))

  ok_ <- !is.na(x = num_) &
    num_ >= 1L &
    num_ <= .max_item &      # 10-K items stop at 16; higher numbers are cross-references
    nw_ <= .max_words        # a long paragraph starting "Item 7." is prose ABOUT item 7

  dplyr::if_else(
    condition = ok_,                    # passed every test above
    true      = paste0(num_, sfx_),     # "1", "1A", "7A"
    false     = NA_character_
  )
}


#' Sort key for the monotonicity filter. 1 -> 1.00, 1A -> 1.01, 2 -> 2.00.
v5_item_key <- function(.item) {

  if (length(x = .item) == 0) return(numeric(length = 0))

  m_ <- stringi::stri_match_first_regex(
    str     = .item,           # "1", "1A", "7A"
    pattern = v5_rx$ItemKey    # group 1 = number, group 2 = suffix
  )

  sfx_rank_ <- match(
    x       = dplyr::coalesce(m_[, 3], ""),   # "" for a bare number
    table   = c("", LETTERS),                 # "" ranks below "A"
    nomatch = 1L                              # unknown suffix sorts as no suffix
  ) - 1L

  as.integer(x = m_[, 2]) + sfx_rank_ / 100
}


# The walk -------------------------------------------------------------------------------------

#' Block XPath for the WALK ONLY.
#'
#' No table term and no ancestor::table exclusion: DATA tables are removed from the tree before
#' this runs, so anything still inside a `table` element belongs to a LAYOUT table and is exactly
#' what we want to reach. An earlier version kept the exclusion and the unwrap silently did
#' nothing - table bullets went 31,565 -> 31,565.
#'
#' td/th are included because layout-table text usually sits directly in a cell; without them it
#' falls to `residual` instead of `prose`.
v5_xpath <- local(expr = {

  blocks_ <- c("p", "div", "li", "blockquote", "td", "th",
               "h1", "h2", "h3", "h4", "h5", "h6")

  nested_ <- paste0(".//", blocks_, collapse = " | ")

  paste0(
    "//", blocks_, "[not(", nested_, ")]",   # leaf blocks only: no block-level descendant
    collapse = " | "
  )
})


#' DOM walk in document order, with the table rule applied.
#'
#' DESTRUCTIVE: removes what it reads, so it must be the last thing to touch the tree.
v5_walk <- function(.doc, .tab_cell = 12L, .tab_row = 3L, .spacer_max = 2L) {

  xml2::xml_remove(
    .x = xml2::xml_find_all(
      x     = .doc,                            # parsed document
      xpath = "//script | //style | //head"    # non-content nodes; their text would pollute
    )
  )

  dat_txt_  <- character(length = 0)
  dat_cell_ <- integer(length = 0)
  dat_row_  <- integer(length = 0)

  tabs_ <- xml2::xml_find_all(
    x     = .doc,
    xpath = "//table[not(ancestor::table)]"   # outermost tables only; nested handled with parent
  )

  if (length(x = tabs_) > 0) {

    ncell_ <- purrr::map_int(
      .x = tabs_,                                                     # each outermost table
      .f = \(.n) length(x = xml2::xml_find_all(x = .n, xpath = ".//td | .//th"))
    )
    nrow_ <- purrr::map_int(
      .x = tabs_,
      .f = \(.n) length(x = xml2::xml_find_all(x = .n, xpath = ".//tr"))
    )

    # THE TABLE RULE. Everything failing it is a layout wrapper and stays in the tree so its
    # contents reach the block walk below.
    is_data_ <- ncell_ >= .tab_cell & nrow_ >= .tab_row

    if (any(is_data_)) {
      dat_txt_  <- xml2::xml_text(x = tabs_[is_data_])
      dat_cell_ <- ncell_[is_data_]
      dat_row_  <- nrow_[is_data_]
      xml2::xml_remove(.x = tabs_[is_data_])
    }
  }

  nd_ <- xml2::xml_find_all(
    x     = .doc,       # tree with data tables already removed
    xpath = v5_xpath    # leaf block nodes, in document order
  )
  tx_ <- if (length(x = nd_) > 0) xml2::xml_text(x = nd_) else character(length = 0)
  ty_ <- if (length(x = nd_) > 0) xml2::xml_name(x = nd_) else character(length = 0)
  if (length(x = nd_) > 0) xml2::xml_remove(.x = nd_)

  tx_ <- v5_ent(.txt = c(dat_txt_, tx_, xml2::xml_text(x = .doc)))
  ty_ <- c(rep(x = "table", times = length(x = dat_txt_)), ty_, "residual")
  nc_ <- c(dat_cell_, rep(x = NA_integer_, times = length(x = nd_)), NA_integer_)
  nr_ <- c(dat_row_,  rep(x = NA_integer_, times = length(x = nd_)), NA_integer_)

  ty_ <- dplyr::if_else(
    condition = ty_ %in% c("td", "th"),   # cell text is not a paragraph element
    true      = "cell",                   # keep the tag honest
    false     = ty_
  )

  # Spacers: `<DIV style="font-size:1pt">&nbsp;</DIV>` and `<p><br></p>`. LETTER-bearing words,
  # because a page-number div holds one "word" that is a digit.
  nl_ <- stringi::stri_count_regex(str = tx_, pattern = v5_rx$Letters)
  k_  <- !v5_blank(.txt = tx_) &
    (nl_ > .spacer_max | ty_ %in% c("table", "residual"))

  tibble::tibble(
    Text     = tx_[k_],
    NodeType = ty_[k_],
    NCell    = nc_[k_],
    NRow     = nr_[k_],
    ItemBlk  = v5_item_block(.txt = tx_[k_])
  )
}


# Fusion ---------------------------------------------------------------------------------------

#' Join consecutive prose blocks until the text ends on terminal punctuation.
#'
#' A block that does not close a sentence is a continuation; one that does is complete. This
#' replaced a 20-word target, which stopped at the first run clearing the threshold and so cut
#' paragraphs in half and glued short distinct ones together.
#'
#' BARRIERS flush the buffer regardless of punctuation, which is why CleanEnd came out at 96%
#' rather than 100% - the residual is real, being paragraphs interrupted by a table or heading.
#' So the measure is NOT fully circular even though the rule uses it.
#'
#' ITEM LABELS propagate: a fused segment inherits the FIRST non-missing label among the blocks
#' it consumed, so a header fused onto its paragraph keeps its item.
v5_fuse <- function(.tab, .max_blocks = 50L, .max_words = 1000L) {

  bar_ <- .tab$NodeType %in% c("table", "residual", "h1", "h2", "h3", "h4", "h5", "h6") |
    v5_is_furniture(.txt = .tab$Text)

  nw_ <- stringi::stri_count_words(str = .tab$Text)
  n_  <- nrow(x = .tab)

  ot_  <- character(length = n_)
  oy_  <- character(length = n_)
  oc_  <- rep(x = NA_integer_,   times = n_)
  orw_ <- rep(x = NA_integer_,   times = n_)
  oi_  <- rep(x = NA_character_, times = n_)
  of_  <- logical(length = n_)
  oh_  <- logical(length = n_)

  k_  <- 0L
  bf_ <- character(length = 0)
  bi_ <- NA_character_
  bn_ <- 0L
  bw_ <- 0L

  flush_ <- function(.forced = FALSE) {
    if (length(x = bf_) == 0) return(invisible(x = NULL))
    k_      <<- k_ + 1L
    ot_[k_] <<- paste(bf_, collapse = " ")
    oy_[k_] <<- "prose"
    oi_[k_] <<- bi_
    of_[k_] <<- .forced
    oh_[k_] <<- !is.na(x = bi_)
    bf_ <<- character(length = 0)
    bi_ <<- NA_character_
    bn_ <<- 0L
    bw_ <<- 0L
    invisible(x = NULL)
  }

  for (i_ in seq_len(length.out = n_)) {

    if (bar_[i_]) {
      flush_(.forced = FALSE)
      k_ <- k_ + 1L
      ot_[k_] <- .tab$Text[i_]
      oy_[k_] <- if (.tab$NodeType[i_] %in% c("table", "residual")) {
        .tab$NodeType[i_]
      } else if (grepl(pattern = v5_rx$Heading, x = .tab$NodeType[i_])) {
        "heading"
      } else {
        "furniture"
      }
      oc_[k_]  <- .tab$NCell[i_]
      orw_[k_] <- .tab$NRow[i_]
      oi_[k_]  <- .tab$ItemBlk[i_]
      oh_[k_]  <- !is.na(x = .tab$ItemBlk[i_])

    } else {
      bf_ <- c(bf_, .tab$Text[i_])
      bn_ <- bn_ + 1L
      bw_ <- bw_ + nw_[i_]
      if (is.na(x = bi_)) bi_ <- .tab$ItemBlk[i_]

      # GUARDS: a document with broken punctuation would otherwise merge into one blob.
      # Measured firing rate 0.04%.
      if (v5_is_term(.txt = .tab$Text[i_])) {
        flush_(.forced = FALSE)
      } else if (bn_ >= .max_blocks || bw_ >= .max_words) {
        flush_(.forced = TRUE)
      }
    }
  }
  flush_(.forced = FALSE)

  tibble::tibble(
    Text     = ot_[seq_len(length.out = k_)],
    NodeType = oy_[seq_len(length.out = k_)],
    NCell    = oc_[seq_len(length.out = k_)],
    NRow     = orw_[seq_len(length.out = k_)],
    ItemBlk  = oi_[seq_len(length.out = k_)],
    IsHdr    = oh_[seq_len(length.out = k_)],
    Forced   = of_[seq_len(length.out = k_)]
  ) |>
    dplyr::filter(!v5_blank(.txt = Text))
}


# Segmentation ---------------------------------------------------------------------------------

#' Segment one document into paragraphs. The routing happens here.
v5_segment <- function(.block,
                       .spacer_max = 2L,
                       .tab_cell   = 12L,
                       .tab_row    = 3L,
                       .max_blocks = 50L,
                       .max_words  = 1000L) {

  if (FALSE) .block <- "..."

  empty_ <- tibble::tibble(
    Route    = character(length = 0),
    Text     = character(length = 0),
    NodeType = character(length = 0),
    NCell    = integer(length = 0),
    NRow     = integer(length = 0),
    ItemBlk  = character(length = 0),
    IsHdr    = logical(length = 0),
    Forced   = logical(length = 0)
  )

  pre_ <- v5_pre(.txt = .block)

  # TEXT ROUTE. html_text passes newlines through untouched - measured identical raw and
  # extracted counts on every inspected document - which is what a blank-line split needs. ASCII
  # tables are tagged here: 19.4% of this route's segments and 30% of its characters were tabular
  # content sitting in `prose`.
  if (!v5_has_blocks(.txt = pre_)) {

    d_ <- try(
      expr   = rvest::read_html(x = pre_, options = "HUGE"),
      silent = TRUE
    )
    tx_ <- if (inherits(x = d_, what = "try-error")) {
      v5_strip_tags(.txt = pre_)
    } else {
      v5_ent(.txt = rvest::html_text(x = d_))
    }

    s_ <- stringi::stri_split_regex(
      str     = tx_,             # extracted plain text
      pattern = v5_rx$BlankLn    # blank line: the only structure a fixed-width filing has
    )[[1]]
    s_ <- s_[!v5_blank(.txt = s_)]
    if (length(x = s_) == 0) return(empty_)

    nl_  <- stringi::stri_count_regex(str = s_, pattern = v5_rx$Letters)
    asc_ <- stringi::stri_detect_regex(str = s_, pattern = v5_rx$Leader) |
      stringi::stri_detect_regex(str = s_, pattern = v5_rx$Cols)

    t_ <- tibble::tibble(
      Text     = s_,
      NodeType = dplyr::case_when(
        nl_ <= .spacer_max         ~ "furniture",
        v5_is_furniture(.txt = s_) ~ "furniture",
        asc_                       ~ "table",
        TRUE                       ~ "prose"
      ),
      NCell   = NA_integer_,
      NRow    = NA_integer_,
      ItemBlk = v5_item_block(.txt = s_)
    )

    return(
      v5_fuse(.tab = t_, .max_blocks = .max_blocks, .max_words = .max_words) |>
        dplyr::mutate(
          Route   = "text",
          .before = 1
        ) |>
        dplyr::mutate(
          Text = dplyr::if_else(
            condition = NodeType == "table",   # ASCII alignment is the content
            true      = Text,
            false     = v5_squish(.txt = Text)
          )
        )
    )
  }

  # HTML ROUTE.
  d_ <- try(
    expr   = rvest::read_html(x = pre_, options = "HUGE"),   # HUGE: filings exceed libxml2 limits
    silent = TRUE
  )
  if (inherits(x = d_, what = "try-error")) return(empty_)

  w_ <- v5_walk(
    .doc        = d_,            # parsed tree
    .tab_cell   = .tab_cell,     # DATA threshold on cells
    .tab_row    = .tab_row,      # DATA threshold on rows
    .spacer_max = .spacer_max    # letter-bearing words below which a segment is layout
  )
  if (nrow(x = w_) == 0) return(empty_)

  v5_fuse(.tab = w_, .max_blocks = .max_blocks, .max_words = .max_words) |>
    dplyr::mutate(
      Route   = "html",
      .before = 1
    ) |>
    dplyr::mutate(
      Text = dplyr::if_else(
        condition = NodeType == "table",   # keep table layout intact
        true      = Text,
        false     = v5_squish(.txt = Text)
      )
    )
}


# Derived columns ------------------------------------------------------------------------------

#' is_table, is_prose, item_num. No re-parse needed to change any of these.
#'
#' ITEM FILL IS UNRESTRICTED, deliberately. Three stopping rules were tested and all rejected: a
#' financial-statement boundary cut Item 8 from 19.4% to 11.0% (Item 8 IS the financial
#' statements) and a distance cap sat at the 97th percentile and never bound.
v5_derive <- function(.seg, .head_words = 12L) {

  .seg |>
    dplyr::mutate(
      NWords     = stringi::stri_count_words(str = Text),
      NChars     = nchar(x = Text),
      DigitShare = stringi::stri_count_regex(str = Text, pattern = v5_rx$Digit) /
        pmax(NChars, 1L),
      IsTable    = NodeType == "table",
      TableSrc   = dplyr::case_when(
        NodeType != "table" ~ NA_character_,
        Route == "html"     ~ "structural",   # n_cell / n_row read off the DOM
        TRUE                ~ "heuristic"     # dot leaders and space-aligned columns
      ),
      IsExhibit  = stringi::stri_detect_regex(str = Text, pattern = v5_rx$ExhibNum) &
        stringi::stri_detect_regex(str = Text, pattern = v5_rx$ExhibCue),
      IsCert     = stringi::stri_detect_regex(str = Text, pattern = v5_rx$Cert),
      LooksHead  = NodeType %in% c("prose", "cell") &
        NWords <= .head_words &
        !v5_is_term(.txt = Text) &
        (Text == toupper(x = Text) |
           stringi::stri_detect_regex(str = Text, pattern = v5_rx$TitleCse)),
      IsProse    = NodeType %in% c("prose", "cell") & !IsExhibit & !IsCert & !LooksHead,
      ItemKey    = v5_item_key(.item = ItemBlk)
    ) |>
    dplyr::group_by(DocID) |>
    dplyr::mutate(
      # MONOTONICITY: an accepted item must not go backwards. This is what removes the table of
      # contents and cross-references without needing a rule for either.
      ItemKeep = !is.na(x = ItemKey) &
        ItemKey >= dplyr::lag(
          x       = cummax(x = dplyr::coalesce(ItemKey, -Inf)),
          default = -Inf
        ),
      ItemNum = dplyr::if_else(
        condition = ItemKeep,
        true      = ItemBlk,
        false     = NA_character_
      )
    ) |>
    tidyr::fill(ItemNum, .direction = "down") |>
    dplyr::ungroup()
}


# Sentences ------------------------------------------------------------------------------------

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
        NWords    = stringi::stri_count_words(
          str = substr(x = .t, start = loc_[, 1], stop = loc_[, 2])
        )
      )
    }
  ) |>
    purrr::list_rbind()
}


#' Compare sentence splitters on paragraphs already in the store.
#'
#' The point of storing offsets rather than text: this re-runs on stored paragraphs and needs no
#' re-parse, so the splitter can be chosen on evidence at any time.
#'
#' READ IT AS: MedWords should land near the 18-28 that V3 measured for real 10-K sentences.
#' AbbrevBreak counts sentences ENDING in a known abbreviation - a direct count of the specific
#' error the filtered iterator exists to prevent, and the number that decides between the two.
v5_compare_splitters <- function(.path_db, .n_par = 20000L, .seed = 42L) {

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  par_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "SELECT doc_id AS DocID, par_id AS ParID, text AS Text
               FROM paragraphs
              WHERE is_prose AND n_words >= 20
              USING SAMPLE %d ROWS (reservoir, %d)",
      .n_par, .seed
    )
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(IsTable = FALSE)

  cli::cli_alert_info(text = "{scales::comma(x = nrow(x = par_))} prose paragraphs sampled")

  abbrev_ <- "(?i)\\b(inc|ltd|co|corp|no|nos|u\\.s|mr|mrs|dr|st|jan|feb|mar|apr|jun|jul|aug|sep|oct|nov|dec)\\.$"

  purrr::map(
    .x = c("icu", "icu_filtered", "regex"),
    .f = \(.m) {
      t0_  <- Sys.time()
      sen_ <- v5_sentences(.par = par_, .method = .m)
      txt_ <- substr(
        x     = par_$Text[match(x = sen_$ParID, table = par_$ParID)],
        start = sen_$CharStart,
        stop  = sen_$CharEnd
      )
      tibble::tibble(
        Method      = .m,
        NSen        = nrow(x = sen_),
        SenPerPar   = round(x = nrow(x = sen_) / nrow(x = par_), digits = 2),
        MedWords    = stats::median(x = sen_$NWords),
        P90Words    = stats::quantile(x = sen_$NWords, probs = 0.9),
        AbbrevBreak = sum(stringi::stri_detect_regex(str = trimws(x = txt_), pattern = abbrev_)),
        Secs        = round(
          x      = as.numeric(x = difftime(time1 = Sys.time(), time2 = t0_, units = "secs")),
          digits = 1
        )
      )
    }
  ) |>
    purrr::list_rbind()
}


# One task -------------------------------------------------------------------------------------

#' Fetch, segment, derive and split one batch of documents.
#'
#' Blocks are fetched INSIDE the worker: accessions go out through the task queue, rows come
#' back. The corpus is 241 GB and must never be resident in the parent.
#'
#' PASSED BY NAME to mirai_map. An anonymous wrapper carries its defining frame - which holds the
#' blocks - and ships it twice.
v5_task <- function(.acc, .path_db, .sen_method = "icu", .gc_every = 1L) {

  if (!file.exists(.path_db)) stop("Worker cannot see the raw store at: ", .path_db)

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )
  if (!"docs" %in% DBI::dbListTables(conn = con_)) stop("No `docs` table in: ", .path_db)

  tab_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "
        SELECT accession AS DocID, cik AS Cik, form_type AS FormType,
               date_filed AS DateFiled, CAST(YEAR(date_filed) AS INTEGER) AS FiledYear,
               n_block AS NBlockBytes, block AS Block
          FROM docs WHERE accession IN (%s)",
      paste(sprintf(fmt = "'%s'", .acc), collapse = ", ")
    )
  ) |>
    tibble::as_tibble()

  res_ <- purrr::pmap(
    .l = list(tab_$DocID, tab_$Cik, tab_$FormType, tab_$DateFiled,
              tab_$FiledYear, tab_$NBlockBytes, tab_$Block,
              seq_len(length.out = nrow(x = tab_))),
    .f = \(.d, .c, .f, .dt, .y, .nb, .b, .i) {

      seg_ <- v5_segment(.block = .b)

      # WHY AN EXPLICIT COLLECT, AND WHY HERE.
      # An xml_document is an EXTERNAL POINTER: R holds a tiny proxy while libxml2 holds the tree,
      # which for a multi-megabyte filing under options = "HUGE" runs to hundreds of megabytes.
      # R sizes its heap from what it can SEE, sees only the proxy, concludes there is no pressure
      # and never triggers. The tree from v5_segment is out of scope by this line but out of scope
      # is not freed - it is collectable, and nothing collects it. Measured consequence: ~12 GB
      # resident PER DAEMON, not falling between years, because daemons outlive the year loop.
      #
      # full = FALSE keeps this cheap; it is the finaliser we need to run, not a full mark-sweep.
      if (.gc_every > 0L && .i %% .gc_every == 0L) {
        gc(verbose = FALSE, full = FALSE)
      }

      if (nrow(x = seg_) == 0) {
        return(
          list(
            Rep = tibble::tibble(
              doc_id      = .d,
              cik         = .c,
              form_type   = .f,
              date_filed  = .dt,
              filed_year  = .y,
              n_block     = .nb,
              route       = NA_character_,
              parsed      = FALSE,
              n_par       = 0L,
              n_prose     = 0L,
              n_table     = 0L,
              n_sentence  = 0L,
              n_item_kept = 0L,
              item_cover  = NA_real_,
              n_forced    = 0L
            ),
            Par = NULL,
            Sen = NULL
          )
        )
      }

      par_ <- seg_ |>
        dplyr::mutate(
          DocID   = .d,
          ParID   = dplyr::row_number(),
          .before = 1
        ) |>
        v5_derive()

      sen_ <- v5_sentences(
        .par    = par_,          # paragraphs of this document
        .method = .sen_method    # icu | regex
      )

      list(
        Rep = tibble::tibble(
          doc_id      = .d,
          cik         = .c,
          form_type   = .f,
          date_filed  = .dt,
          filed_year  = .y,
          n_block     = .nb,
          route       = par_$Route[[1]],
          parsed      = TRUE,
          n_par       = nrow(x = par_),
          n_prose     = sum(par_$IsProse),
          n_table     = sum(par_$IsTable),
          n_sentence  = nrow(x = sen_),
          n_item_kept = sum(par_$ItemKeep),
          item_cover  = if (any(par_$IsProse)) {
            mean(x = !is.na(x = par_$ItemNum[par_$IsProse]))
          } else {
            NA_real_
          },
          n_forced    = sum(par_$Forced)
        ),
        Par = par_ |>
          dplyr::transmute(
            doc_id      = DocID,
            par_id      = ParID,
            node_type   = NodeType,
            is_table    = IsTable,
            table_src   = TableSrc,
            is_prose    = IsProse,
            item_num    = ItemNum,
            item_header = ItemKeep,
            forced      = Forced,
            n_cell      = NCell,
            n_row       = NRow,
            n_words     = NWords,
            n_chars     = NChars,
            digit_share = DigitShare,
            text        = Text
          ),
        Sen = sen_ |>
          dplyr::transmute(
            doc_id     = DocID,
            par_id     = ParID,
            sen_id     = SenID,
            char_start = CharStart,
            char_end   = CharEnd,
            n_words    = NWords
          )
      )
    }
  )

  list(
    Rep = purrr::list_rbind(x = purrr::map(.x = res_, .f = "Rep")),
    Par = purrr::list_rbind(x = purrr::compact(.x = purrr::map(.x = res_, .f = "Par"))),
    Sen = purrr::list_rbind(x = purrr::compact(.x = purrr::map(.x = res_, .f = "Sen")))
  )
}


# Selecting documents --------------------------------------------------------------------------

#' Documents to process for one filing year, plus the ones DEFERRED by the size cap.
#'
#' TWO QUERIES, and this is not optional. A window function materialises its input, so carrying
#' `block` inside a ranking CTE loads all 241 GB to compute a row number. Metadata only here;
#' blocks are fetched inside the workers.
v5_docs_for_year <- function(.path_raw, .year, .max_mb = Inf, .limit = NULL) {

  con_ <- v5_connect(.path_db = .path_raw, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  cap_ <- if (is.finite(x = .max_mb)) .max_mb * 1e6 else Inf

  keep_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "
        SELECT accession AS DocID, n_block AS NBlockBytes
          FROM docs
         WHERE CAST(YEAR(date_filed) AS INTEGER) = %d%s
         ORDER BY accession%s",
      .year,
      if (is.finite(x = cap_)) sprintf(fmt = " AND n_block <= %f", cap_) else "",
      if (is.null(x = .limit)) "" else sprintf(fmt = " LIMIT %d", .limit)
    )
  ) |>
    tibble::as_tibble()

  # The deferred set is returned IN FULL, not counted: it is written to `_skipped` so a later pass
  # can pick it up. A cap that loses documents silently is not acceptable; one that postpones them
  # visibly is.
  defer_ <- if (is.finite(x = cap_)) {
    DBI::dbGetQuery(
      conn      = con_,
      statement = sprintf(
        fmt = "
          SELECT accession AS DocID, n_block AS NBlockBytes
            FROM docs
           WHERE CAST(YEAR(date_filed) AS INTEGER) = %d AND n_block > %f
           ORDER BY n_block",
        .year, cap_
      )
    ) |>
      tibble::as_tibble()
  } else {
    tibble::tibble(
      DocID       = character(length = 0),
      NBlockBytes = numeric(length = 0)
    )
  }

  attr(x = keep_, which = "Deferred") <- defer_
  keep_
}


# Writing --------------------------------------------------------------------------------------

#' Replace one year's rows in the store.
#'
#' ORDER MATTERS. An earlier version deleted `reports` FIRST, so the paragraph and sentence
#' deletes - which identify their rows via `reports` - found nothing and orphaned every old row.
#' That only bites when a year is RE-RUN, which is exactly what happens after a parameter change.
v5_write_year <- function(.path_db, .year, .rep, .par, .sen, .defer, .max_mb, .limit, .secs) {

  con_ <- v5_connect(.path_db = .path_db, .read_only = FALSE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(
      fmt = "DELETE FROM sentences WHERE doc_id IN
               (SELECT doc_id FROM reports WHERE filed_year = %d)",
      .year
    )
  )
  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(
      fmt = "DELETE FROM paragraphs WHERE doc_id IN
               (SELECT doc_id FROM reports WHERE filed_year = %d)",
      .year
    )
  )
  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(fmt = "DELETE FROM reports WHERE filed_year = %d", .year)
  )

  DBI::dbAppendTable(conn = con_, name = "reports",    value = .rep)
  DBI::dbAppendTable(conn = con_, name = "paragraphs", value = .par)
  DBI::dbAppendTable(conn = con_, name = "sentences",  value = .sen)

  # Rewrite the deferred set for this year: a raised cap must be able to CLEAR entries, not only
  # add them.
  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(fmt = "DELETE FROM _skipped WHERE filed_year = %d", .year)
  )
  if (nrow(x = .defer) > 0) {
    DBI::dbAppendTable(
      conn  = con_,
      name  = "_skipped",
      value = tibble::tibble(
        doc_id     = .defer$DocID,
        filed_year = .year,
        n_block    = .defer$NBlockBytes,
        reason     = "size_cap",
        cap_mb     = as.numeric(x = .max_mb),
        logged_at  = Sys.time()
      )
    )
  }

  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(fmt = "DELETE FROM _progress WHERE filed_year = %d", .year)
  )
  DBI::dbAppendTable(
    conn  = con_,
    name  = "_progress",
    value = tibble::tibble(
      filed_year = .year,
      n_docs     = nrow(x = .rep),
      n_par      = nrow(x = .par),
      n_sentence = nrow(x = .sen),
      n_skipped  = nrow(x = .defer),
      limit_used = if (is.null(x = .limit)) NA_integer_ else as.integer(x = .limit),
      secs       = .secs,
      written_at = Sys.time()
    )
  )

  invisible(x = NULL)
}


#' Append a set of documents without touching year boundaries. Used by v5_run_skipped.
v5_write_docs <- function(.path_db, .rep, .par, .sen) {

  con_ <- v5_connect(.path_db = .path_db, .read_only = FALSE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  # Bounded by .chunk in v5_run_skipped (50 by default), so an IN list is safe here.
  ids_ <- paste(sprintf(fmt = "'%s'", .rep$doc_id), collapse = ", ")

  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(fmt = "DELETE FROM sentences WHERE doc_id IN (%s)", ids_)
  )
  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(fmt = "DELETE FROM paragraphs WHERE doc_id IN (%s)", ids_)
  )
  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(fmt = "DELETE FROM reports WHERE doc_id IN (%s)", ids_)
  )

  DBI::dbAppendTable(conn = con_, name = "reports",    value = .rep)
  DBI::dbAppendTable(conn = con_, name = "paragraphs", value = .par)
  DBI::dbAppendTable(conn = con_, name = "sentences",  value = .sen)

  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(fmt = "DELETE FROM _skipped WHERE doc_id IN (%s)", ids_)
  )

  invisible(x = NULL)
}


# Running --------------------------------------------------------------------------------------

#' Ship the parse functions into the daemons.
#'
#' NOT by sourcing this file: that would re-run it inside every worker.
v5_export_to_daemons <- function() {

  mirai::everywhere(
    .expr = {
      for (nm in names(x = objs_)) {
        assign(x = nm, value = objs_[[nm]], envir = globalenv())
      }
    },
    objs_ = mget(
      x     = c("v5_rx", "v5_xpath", "v5_connect", "v5_pre", "v5_ent", "v5_has_blocks",
                "v5_strip_tags", "v5_squish", "v5_blank", "v5_is_term", "v5_is_furniture",
                "v5_item_block", "v5_item_key", "v5_walk", "v5_fuse", "v5_segment",
                "v5_derive", "v5_sentences"),
      envir = globalenv()
    )
  )
  invisible(x = NULL)
}


#' Dispatch a set of accessions across daemons and collect the rows.
v5_dispatch <- function(.acc, .path_raw, .workers, .batch, .sen_method, .label,
                        .gc_every = 1L) {

  # NOT cut(): cut(x, 1) errors with "invalid number of intervals", so any year holding fewer
  # documents than .batch killed the build. 1993 has one document.
  tasks_ <- unname(
    obj = split(
      x = .acc,                                                        # accessions to process
      f = ceiling(x = seq_len(length.out = length(x = .acc)) / .batch) # group index
    )
  )

  res_ <- if (.workers > 1L && requireNamespace(package = "mirai", quietly = TRUE)) {
    mirai::mirai_map(
      .x    = tasks_,      # one element per task
      .f    = v5_task,     # BY NAME, so no defining frame travels with it
      .args = list(        # constant arguments; mirai's `...` does not forward them
        .path_db    = .path_raw,
        .sen_method = .sen_method,
        .gc_every   = .gc_every
      )
    )[]
  } else {
    purrr::map(
      .x = tasks_,
      .f = \(.a) v5_task(.acc = .a, .path_db = .path_raw, .sen_method = .sen_method,
                         .gc_every = .gc_every)
    )
  }

  # mirai returns an error OBJECT per failed task rather than throwing, so an unchecked map(...)
  # silently yields nothing and the failure surfaces much later as a missing column.
  bad_ <- purrr::map_lgl(
    .x = res_,
    .f = \(.x) inherits(x = .x, what = "miraiError") || inherits(x = .x, what = "errorValue")
  )
  if (any(bad_)) {
    cli::cli_alert_danger(
      text = "{(.label)}: {sum(bad_)} of {length(x = res_)} task{?s} failed. First:"
    )
    print(x = res_[bad_][[1]])
    if (all(bad_)) cli::cli_abort(message = "{(.label)}: every task failed - nothing written")
  }

  list(
    Rep     = purrr::list_rbind(x = purrr::map(.x = res_[!bad_], .f = "Rep")),
    Par     = purrr::list_rbind(x = purrr::map(.x = res_[!bad_], .f = "Par")),
    Sen     = purrr::list_rbind(x = purrr::map(.x = res_[!bad_], .f = "Sen")),
    NFailed = sum(bad_)
  )
}


#' Process and write ONE year. The unit of resumability.
v5_run_year <- function(.year,
                        .path_raw,
                        .path_db,
                        .workers    = 24L,
                        .batch      = 8L,
                        .max_mb     = Inf,
                        .limit      = NULL,
                        .sen_method     = "icu",
                        .gc_every       = 1L,
                        .fresh_daemons  = TRUE) {

  t0_ <- Sys.time()

  docs_ <- v5_docs_for_year(
    .path_raw = .path_raw,   # raw store holding the blocks
    .year     = .year,       # filing year to process
    .max_mb   = .max_mb,     # blocks above this are DEFERRED, not dropped
    .limit    = .limit       # NULL for all; a number for smoke tests
  )
  defer_ <- attr(x = docs_, which = "Deferred")

  if (nrow(x = docs_) == 0) {
    cli::cli_alert_warning(text = "{(.year)}: no documents")
    return(invisible(x = NULL))
  }

  cli::cli_alert_info(
    text = "{(.year)}: {scales::comma(x = nrow(x = docs_))} documents, \\
            {round(x = sum(docs_$NBlockBytes) / 1e9, digits = 2)} GB"
  )

  if (nrow(x = defer_) > 0) {
    cli::cli_alert_info(
      text = "{(.year)}: {nrow(x = defer_)} document{?s} DEFERRED over {(.max_mb)}MB \\
              ({round(x = sum(defer_$NBlockBytes) / 1e9, digits = 2)} GB, largest \\
              {round(x = max(defer_$NBlockBytes) / 1e6, digits = 1)}MB) - in `_skipped`, \\
              run v5_run_skipped() to pick them up"
    )
  }

  # FRESH DAEMONS PER YEAR. Process exit returns memory to the OS unconditionally, so this works
  # whatever is accumulating - it does not depend on the diagnosis being right, which an explicit
  # gc() does. Daemons started once in v5_build outlive the whole year loop, which is why resident
  # memory never fell between years.
  #
  # It bounds growth PER YEAR, not within one: a 5,000-document year at 8 documents per task still
  # puts ~200 documents through each daemon before the reset. .gc_every covers that, so the two are
  # complementary rather than alternatives.
  #
  # Cost is daemon startup plus the function export, a few seconds per year against a run measured
  # in hours - and it is visible in the per-year timing below.
  if (.fresh_daemons && .workers > 1L && requireNamespace(package = "mirai", quietly = TRUE)) {
    try(expr = mirai::daemons(n = 0), silent = TRUE)   # tear down anything left over
    mirai::daemons(n = .workers)
    on.exit(
      expr = try(expr = mirai::daemons(n = 0), silent = TRUE),
      add  = TRUE
    )
    v5_export_to_daemons()
  }

  got_ <- v5_dispatch(
    .acc        = docs_$DocID,             # accessions for this year
    .path_raw   = .path_raw,               # workers open this themselves
    .workers    = .workers,                # daemons already running
    .batch      = .batch,                  # documents per task
    .sen_method = .sen_method,             # icu | regex
    .label      = as.character(x = .year), # for error messages
    .gc_every   = .gc_every                # documents between collects INSIDE each daemon
  )

  secs_ <- as.numeric(
    x = difftime(time1 = Sys.time(), time2 = t0_, units = "secs")
  )

  v5_write_year(
    .path_db = .path_db,    # destination store
    .year    = .year,       # partition being replaced
    .rep     = got_$Rep,    # document rows
    .par     = got_$Par,    # paragraph rows
    .sen     = got_$Sen,    # sentence offset rows
    .defer   = defer_,      # deferred documents, for `_skipped`
    .max_mb  = .max_mb,     # recorded alongside them
    .limit   = .limit,      # NULL means the year is COMPLETE; a number means truncated
    .secs    = secs_        # for `_progress`
  )

  cli::cli_alert_success(
    text = "{(.year)}: {scales::comma(x = nrow(x = got_$Rep))} docs, \\
            {scales::comma(x = nrow(x = got_$Par))} paragraphs, \\
            {scales::comma(x = nrow(x = got_$Sen))} sentences in \\
            {round(x = secs_, digits = 1)}s"
  )

  invisible(
    x = list(
      NDocs     = nrow(x = got_$Rep),
      NPar      = nrow(x = got_$Par),
      NSen      = nrow(x = got_$Sen),
      NDeferred = nrow(x = defer_)
    )
  )
}


#' Process the DEFERRED documents.
#'
#' Fewer workers and one document per task by default: these are the large blocks, so the
#' constraint is memory per worker rather than throughput. Smallest first, so a failure on the
#' largest still leaves progress behind.
v5_run_skipped <- function(.path_raw,
                           .path_db,
                           .workers    = 4L,
                           .batch      = 1L,
                           .sen_method = "icu",
                           .chunk      = 50L,
                           .gc_every   = 1L) {

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  todo_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "SELECT doc_id, filed_year, n_block FROM _skipped ORDER BY n_block"
  ) |>
    tibble::as_tibble()
  DBI::dbDisconnect(conn = con_, shutdown = TRUE)

  if (nrow(x = todo_) == 0) {
    cli::cli_alert_success(text = "No deferred documents")
    return(invisible(x = NULL))
  }

  cli::cli_alert_info(
    text = "{nrow(x = todo_)} deferred document{?s}, \\
            {round(x = sum(todo_$n_block) / 1e9, digits = 2)} GB, largest \\
            {round(x = max(todo_$n_block) / 1e6, digits = 1)}MB, {(.workers)} worker{?s}"
  )

  if (.workers > 1L && requireNamespace(package = "mirai", quietly = TRUE)) {
    mirai::daemons(n = .workers)
    on.exit(
      expr = try(expr = mirai::daemons(n = 0), silent = TRUE),
      add  = TRUE
    )
    v5_export_to_daemons()
  }

  chunks_ <- unname(
    obj = split(
      x = todo_$doc_id,
      f = ceiling(x = seq_len(length.out = nrow(x = todo_)) / .chunk)
    )
  )

  for (i_ in seq_len(length.out = length(x = chunks_))) {

    t0_ <- Sys.time()

    got_ <- v5_dispatch(
      .acc        = chunks_[[i_]],
      .path_raw   = .path_raw,
      .workers    = .workers,
      .batch      = .batch,
      .sen_method = .sen_method,
      .label      = sprintf(fmt = "deferred %d/%d", i_, length(x = chunks_)),
      .gc_every   = .gc_every
    )

    v5_write_docs(
      .path_db = .path_db,
      .rep     = got_$Rep,
      .par     = got_$Par,
      .sen     = got_$Sen
    )

    cli::cli_alert_success(
      text = "deferred {i_}/{length(x = chunks_)}: {nrow(x = got_$Rep)} docs, \\
              {scales::comma(x = nrow(x = got_$Par))} paragraphs in \\
              {round(x = as.numeric(x = difftime(time1 = Sys.time(), time2 = t0_,
                                                 units = 'secs')), digits = 1)}s"
    )
  }

  invisible(x = NULL)
}


#' Build the whole store, year by year, resumable.
v5_build <- function(.path_raw,
                     .path_db,
                     .years      = NULL,
                     .workers    = 24L,
                     .batch      = 8L,
                     .max_mb     = Inf,
                     .limit      = NULL,
                     .sen_method    = "icu",
                     .gc_every      = 1L,
                     .fresh_daemons = TRUE,
                     .force         = FALSE) {

  cli::cli_h1(text = "V5 paragraph store")

  v5_store_init(
    .path_db   = .path_db,   # store to create or extend
    .overwrite = .force      # TRUE deletes the file first
  )

  if (is.null(x = .years)) {
    con_ <- v5_connect(.path_db = .path_raw, .read_only = TRUE)
    .years <- sort(
      x = DBI::dbGetQuery(
        conn      = con_,
        statement = "SELECT DISTINCT CAST(YEAR(date_filed) AS INTEGER) AS Y
                       FROM docs ORDER BY Y"
      )$Y
    )
    DBI::dbDisconnect(conn = con_, shutdown = TRUE)
  }

  done_ <- if (.force) {
    integer(length = 0)
  } else {
    v5_years_done(
      .path_db = .path_db,   # store to inspect
      .limit   = .limit      # a year written under a DIFFERENT limit is not done
    )
  }
  todo_ <- setdiff(x = .years, y = done_)

  # Years present but TRUNCATED are about to be rebuilt. Say so, because the alternative is a
  # long silent re-run that looks like a bug.
  prog_  <- v5_progress(.path_db = .path_db)
  trunc_ <- intersect(
    x = todo_,
    y = prog_$filed_year[!is.na(x = prog_$limit_used)]
  )
  if (length(x = trunc_) > 0) {
    cli::cli_alert_warning(
      text = "{length(x = trunc_)} year{?s} were written under a row limit and will be REBUILT: \
              {paste(range(trunc_), collapse = '-')}"
    )
  }

  if (length(x = done_) > 0) {
    cli::cli_alert_info(
      text = "Skipping {length(x = done_)} year{?s} already written: \\
              {paste(range(done_), collapse = '-')}"
    )
  }
  if (length(x = todo_) == 0) {
    cli::cli_alert_success(text = "Nothing to do")
    return(invisible(x = NULL))
  }
  cli::cli_alert_info(
    text = "{length(x = todo_)} year{?s} to process: {paste(range(todo_), collapse = '-')}"
  )

  # With .fresh_daemons the year owns its workers, so nothing is started here. Otherwise one set
  # is started for the whole loop - the old behaviour, kept for a short run where the restart cost
  # would outweigh the memory it reclaims.
  if (!.fresh_daemons && .workers > 1L &&
      requireNamespace(package = "mirai", quietly = TRUE)) {
    mirai::daemons(n = .workers)
    on.exit(
      expr = try(expr = mirai::daemons(n = 0), silent = TRUE),
      add  = TRUE
    )
    v5_export_to_daemons()
    cli::cli_alert_info(text = "{(.workers)} daemons ready (shared across all years)")
  } else if (.fresh_daemons && .workers > 1L) {
    cli::cli_alert_info(text = "{(.workers)} daemons, RESTARTED each year")
  }

  t0_ <- Sys.time()
  for (y_ in todo_) {
    v5_run_year(
      .year       = y_,
      .path_raw   = .path_raw,
      .path_db    = .path_db,
      .workers    = .workers,
      .batch      = .batch,
      .max_mb     = .max_mb,
      .limit      = .limit,
      .sen_method    = .sen_method,
      .gc_every      = .gc_every,
      .fresh_daemons = .fresh_daemons
    )
  }

  cli::cli_alert_success(
    text = "Done in {round(x = as.numeric(x = difftime(time1 = Sys.time(), time2 = t0_,
                                                       units = 'mins')), digits = 1)} minutes"
  )

  invisible(x = v5_verify(.path_db = .path_db))
}


# Verification ---------------------------------------------------------------------------------

#' Store-level checks and a fingerprint.
#'
#' The fingerprint exists because two earlier sessions in this project failed on stale file
#' copies. Report it with any handoff so the receiving side can confirm it holds the same store.
v5_verify <- function(.path_db) {

  cli::cli_h1(text = "Verify")

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  cnt_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT (SELECT COUNT(*) FROM reports)    AS NReports,
             (SELECT COUNT(*) FROM paragraphs) AS NPar,
             (SELECT COUNT(*) FROM sentences)  AS NSen,
             (SELECT COUNT(*) FROM _skipped)   AS NDeferred"
  ) |>
    tibble::as_tibble()
  print(x = as.data.frame(x = cnt_), row.names = FALSE)

  if (cnt_$NDeferred > 0) {
    cli::cli_alert_warning(
      text = "{cnt_$NDeferred} document{?s} DEFERRED and not yet in the store. \\
              Run v5_run_skipped() before treating the panel as complete."
    )
    DBI::dbGetQuery(
      conn      = con_,
      statement = "
        SELECT filed_year, COUNT(*) AS NDeferred,
               ROUND(SUM(n_block) / 1e9, 2) AS GB,
               ROUND(MAX(n_block) / 1e6, 1) AS LargestMB
          FROM _skipped GROUP BY 1 ORDER BY 1"
    ) |>
      as.data.frame() |>
      print(row.names = FALSE)
  }

  trunc_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "SELECT filed_year, n_docs, limit_used FROM _progress
                  WHERE limit_used IS NOT NULL ORDER BY filed_year"
  ) |>
    tibble::as_tibble()

  if (nrow(x = trunc_) > 0) {
    cli::cli_alert_warning(
      text = "{nrow(x = trunc_)} year{?s} were written under a row limit - this store is a \
              SAMPLE, not the corpus. Re-run v5_build() without .limit before using it."
    )
    print(x = as.data.frame(x = trunc_), row.names = FALSE)
  }

  cli::cli_h2(text = "Route and quality by era - both ENDOGENOUS, must be reported")
  DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT CASE WHEN filed_year < 2001 THEN '1993-2000'
                  WHEN filed_year < 2011 THEN '2001-2010' ELSE '2011+' END AS Era,
             route AS Route, COUNT(*) AS NDocs,
             ROUND(MEDIAN(n_par))               AS MedPar,
             ROUND(MEDIAN(n_prose))             AS MedProse,
             ROUND(100 * MEDIAN(item_cover), 1) AS MedItemCoverPct,
             ROUND(100 * AVG(CASE WHEN n_item_kept = 0 THEN 1 ELSE 0 END), 1) AS ZeroItemPct
        FROM reports WHERE parsed GROUP BY 1, 2 ORDER BY 1, 2"
  ) |>
    as.data.frame() |>
    print(row.names = FALSE)

  cli::cli_h2(text = "Sentences per paragraph, split by is_prose")
  cli::cli_text(
    text = "Non-prose segments are split too, and dot leaders plus numeric columns generate
spurious ICU boundaries. If PROSE is at 2-4 sentences with median 18-28 words the splitter is
behaving; if prose itself is at 7 with 4-word sentences, ICU is over-splitting prose and
v5_sentences needs a different method."
  )
  DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT p.is_prose AS IsProse, COUNT(*) AS NSen,
             COUNT(DISTINCT (p.doc_id, p.par_id)) AS NPar,
             ROUND(1.0 * COUNT(*) / COUNT(DISTINCT (p.doc_id, p.par_id)), 2) AS SenPerPar,
             ROUND(MEDIAN(s.n_words), 1) AS MedSenWords
        FROM sentences s JOIN paragraphs p
          ON p.doc_id = s.doc_id AND p.par_id = s.par_id
       GROUP BY 1 ORDER BY 1"
  ) |>
    as.data.frame() |>
    print(row.names = FALSE)

  cli::cli_h2(text = "Item shares over prose paragraphs")
  DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT item_num AS Item, COUNT(*) AS N,
             ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (), 1) AS Pct
        FROM paragraphs WHERE is_prose AND item_num IS NOT NULL
       GROUP BY 1
       ORDER BY TRY_CAST(REGEXP_EXTRACT(item_num, '^[0-9]+') AS INTEGER), item_num"
  ) |>
    as.data.frame() |>
    print(row.names = FALSE)

  cli::cli_h2(text = "Integrity")
  bad_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT
        (SELECT COUNT(*) FROM sentences s LEFT JOIN paragraphs p
           ON p.doc_id = s.doc_id AND p.par_id = s.par_id
          WHERE p.doc_id IS NULL) AS OrphanSen,
        (SELECT COUNT(*) FROM sentences s JOIN paragraphs p
           ON p.doc_id = s.doc_id AND p.par_id = s.par_id
          WHERE s.char_end > LENGTH(p.text)) AS OffsetOverrun,
        (SELECT COUNT(*) FROM paragraphs
          WHERE text IS NULL OR LENGTH(TRIM(text)) = 0) AS EmptyPar"
  ) |>
    tibble::as_tibble()
  print(x = as.data.frame(x = bad_), row.names = FALSE)

  if (sum(unlist(x = bad_)) > 0) {
    cli::cli_alert_danger(text = "Integrity checks FAILED - the offending rows:")
    DBI::dbGetQuery(
      conn      = con_,
      statement = "
        SELECT doc_id, par_id, node_type, n_chars,
               REPLACE(REPLACE(SUBSTR(text, 1, 40), CHR(10), '\\n'), CHR(9), '\\t') AS Escaped
          FROM paragraphs WHERE text IS NULL OR LENGTH(TRIM(text)) = 0 LIMIT 10"
    ) |>
      as.data.frame() |>
      print(row.names = FALSE)
  } else {
    cli::cli_alert_success(text = "Integrity checks passed")
  }

  # ITEM 1 came back at 0.2% against 1A at 12.4% in the 2015 smoke - not small, near-absent. The
  # diagnostic distinguishes the two candidate causes rather than assuming one:
  #   - monotonicity exhausted by a table of contents (headers 1..15 accepted in order), after
  #     which the real body "Item 1" is rejected as out of sequence
  #   - the header is prefixed, e.g. "PART I ITEM 1. BUSINESS", so ^ITEM never matches
  cli::cli_h2(text = "Item-header diagnostics")
  DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT item_num AS Item, COUNT(*) AS NHeaders, ROUND(MEDIAN(par_id)) AS MedParID
        FROM paragraphs WHERE item_header GROUP BY 1
       ORDER BY TRY_CAST(REGEXP_EXTRACT(item_num, '^[0-9]+') AS INTEGER), item_num"
  ) |>
    as.data.frame() |>
    print(row.names = FALSE)

  fp_ <- paste0(
    "n=", cnt_$NReports, "/", cnt_$NPar, "/", cnt_$NSen,
    " deferred=", cnt_$NDeferred,
    if (nrow(x = trunc_) > 0) paste0(" TRUNCATED_YEARS=", nrow(x = trunc_)) else "",
    " size=", round(x = file.size(.path_db) / 1e9, digits = 3), "GB",
    " mtime=", format(x = file.mtime(.path_db), format = "%Y-%m-%dT%H:%M:%S")
  )
  cli::cli_alert_info(text = "Fingerprint: {fp_}")

  invisible(
    x = list(
      Counts      = cnt_,
      Integrity   = bad_,
      Fingerprint = fp_
    )
  )
}


# =============================================================================================
# MODEL SCORING
# =============================================================================================
#
# Brute-force transformer scoring over the paragraphs built above: nine ClimateBERT task models,
# every prose paragraph, raw label and confidence stored per paragraph per model. No aggregation
# and no measure construction - the point is to have all of it on disk and decide later.
#
# WHY THE SCORES LIVE IN A SEPARATE FILE
# DuckDB is single-writer. R holds the paragraph store open, and the Python scorer must write
# while it runs, so writing there would either fail on the lock or force the caller to remember to
# disconnect first. This was a measured decision in the V3 scorer and is kept. The two are joined
# with ATTACH, so they behave as one dataset - see v5_score_attach().
#
# RESUME IS KEYED ON (doc_id, model)
# Not a filename, not a shard index. `_scored` is the ledger; v5_score_missing() answers "what is
# left" at any moment, and adding a tenth model later re-runs only that model.
#
# ---------------------------------------------------------------------------------------------
# THE FOUR MODELS NOT IN THE REGISTRY, AND WHY
#
#   climatebert/distilroberta-base-climate-f     climatebert/distilroberta-base-climate-d
#   climatebert/distilroberta-base-climate-s     climatebert/distilroberta-base-climate-d-s
#
# These are the ClimateBERT LANGUAGE models - DistilRoBERTa further pretrained on climate text and
# published with a MASKED LANGUAGE MODEL head. They differ only in pretraining sample selection
# (FULL / SIM / DIV select) and have no classifier.
#
# They are dangerous rather than merely useless: AutoModelForSequenceClassification does NOT error
# on an MLM checkpoint. It attaches a RANDOMLY INITIALISED classification head, warns, and returns
# predictions that are noise carrying plausible confidence scores. The Python side refuses any
# model whose id2label is the transformers default. Same family as the `== "yes"` bug that made
# pBERT_NetZero exactly zero in the canonical export for a year: a silent wrong answer is worse
# than a crash.
#
# WHAT `score` IS NOT
# The model's confidence in the label it CHOSE, not P(positive). Measured on netzero-reduction:
# thresholding the mean correlated -0.17 with the net-zero count and +0.77 with confidence, i.e.
# AGAINST the construct. That is what made bBERT95_NetZero near-constant at 0.96-0.99. Use label
# COUNTS downstream. The score is stored because it is free, not because it should be thresholded.


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


#' Open the paragraph store with the scores attached read-only.
#'
#' Two files, one logical dataset. Use this for anything that needs paragraphs and scores together.
v5_score_attach <- function(.path_db, .path_scores, .alias = "sc") {

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)

  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(
      fmt = "ATTACH '%s' AS %s (READ_ONLY)",
      .path_scores,   # scores file
      .alias          # queryable as sc.scores, sc._scored, sc._models
    )
  )
  con_
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


#' What the models actually found. Descriptive only - no measures are constructed here.
#'
#' v5_score_verify() answers "did the run complete and are the labels what they claim". This
#' answers "is any of it usable". Six panels, and the order is deliberate: the failures come first,
#' because a degenerate or broken model makes everything downstream of it noise.
#'
#' READ THE LABELS, NOT THE SCORES. `score` is confidence in the label chosen, not P(positive):
#' thresholding it on netzero-reduction correlated -0.17 with the net-zero count and +0.77 with
#' confidence, i.e. against the construct.
#'
#' EVERYTHING HERE IS ON WHATEVER SAMPLE WAS SCORED. With .limit set, year comparisons are across
#' equal-sized samples rather than the corpus, which is the right shape for comparing models and
#' the wrong shape for a level.
v5_score_overview <- function(.path_db, .path_scores, .min_share = 0.995) {

  con_ <- v5_score_attach(
    .path_db     = .path_db,      # paragraphs and reports
    .path_scores = .path_scores,  # attached as `sc`
    .alias       = "sc"
  )
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  mods_ <- v5_models()

  cli::cli_h1(text = "What the models found")

  # ---- A. Which models are broken or uninformative -------------------------------------------
  cli::cli_h2(text = "A. Model health")
  cli::cli_text(
    text = "MedTokens is the diagnostic that matters most. The seven working models sit near 90;
2 means the tokenizer produced only <s> and </s> - the text never reached the model, and every
paragraph then gets one label at confidence ~1. ModalShare near 1 means the model is effectively
constant, which is how bBERT95_NetZero looked before it was abandoned."
  )

  health_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "
      WITH lab AS (
        SELECT model, label, COUNT(*) AS n
          FROM sc.scores GROUP BY 1, 2
      ), tot AS (
        SELECT model, SUM(n) AS n_all, COUNT(*) AS n_lab, MAX(n) AS n_modal FROM lab GROUP BY 1
      ), tok AS (
        SELECT model, ROUND(MEDIAN(n_tok)) AS med_tok,
               ROUND(100.0 * AVG(CASE WHEN truncated THEN 1 ELSE 0 END), 2) AS trunc_pct
          FROM sc.scores GROUP BY 1
      )
      SELECT t.model AS Model, tk.med_tok AS MedTokens, tk.trunc_pct AS TruncPct,
             t.n_lab AS NLabels,
             ROUND(1.0 * t.n_modal / t.n_all, 4) AS ModalShare
        FROM tot t JOIN tok tk ON tk.model = t.model
       ORDER BY ModalShare DESC"
  ) |>
    tibble::as_tibble()

  health_ <- health_ |>
    dplyr::mutate(
      Verdict = dplyr::case_when(
        MedTokens <= 5           ~ "BROKEN - text never tokenised",
        ModalShare >= .min_share ~ "near-constant - little usable variation",
        TRUE                     ~ "ok"
      )
    )
  print(x = as.data.frame(x = health_), row.names = FALSE)

  bad_ <- health_$Model[health_$Verdict != "ok"]
  if (length(x = bad_) > 0) {
    cli::cli_alert_warning(
      text = "Excluded from the panels below: {paste(bad_, collapse = ', ')}"
    )
  }
  ok_ <- setdiff(x = health_$Model, y = bad_)

  # ---- B. The routing break ------------------------------------------------------------------
  cli::cli_h2(text = "B. Signal rate by year and route - THE break check")
  cli::cli_text(
    text = "Route is ENDOGENOUS: a filing-agent property, hence correlated with size and industry.
The transition runs 2002-2007. If a model's rate STEPS at the boundary rather than trending
through it, the routing is in the measure and no amount of model choice fixes that."
  )

  sig_ <- mods_ |>
    dplyr::filter(!is.na(x = Signal), Model %in% ok_) |>
    dplyr::mutate(Labels = strsplit(x = Signal, split = ","))

  by_year_ <- purrr::pmap(
    .l = list(sig_$Model, sig_$Labels),
    .f = \(.m, .lab) {
      DBI::dbGetQuery(
        conn      = con_,
        statement = sprintf(
          fmt = "
            SELECT r.filed_year AS FiledYear, r.route AS Route,
                   COUNT(*) AS NPar,
                   ROUND(100.0 * AVG(CASE WHEN s.label IN (%s) THEN 1 ELSE 0 END), 2) AS SignalPct
              FROM sc.scores s
              JOIN reports r ON r.doc_id = s.doc_id
             WHERE s.model = '%s'
             GROUP BY 1, 2 ORDER BY 1, 2",
          paste(sprintf(fmt = "'%s'", trimws(x = .lab)), collapse = ", "),
          .m
        )
      ) |>
        tibble::as_tibble() |>
        dplyr::mutate(Model = .m, .before = 1)
    }
  ) |>
    purrr::list_rbind()

  by_year_ |>
    dplyr::select(FiledYear, Route, Model, SignalPct) |>
    tidyr::pivot_wider(names_from = Model, values_from = SignalPct) |>
    dplyr::arrange(FiledYear, Route) |>
    as.data.frame() |>
    print(row.names = FALSE)

  # ---- C. Where the signal sits in the filing ------------------------------------------------
  cli::cli_h2(text = "C. Signal rate by item")
  cli::cli_text(
    text = "The prior worth testing: climate content should concentrate in Item 1A (Risk Factors),
which did not exist before fiscal 2005. The measured fire effect lives there - 0.0185*** against
0.0007 in Item 1 Business - so a model that does NOT separate 1A from 1 is not seeing what the
paper needs it to see."
  )

  purrr::pmap(
    .l = list(sig_$Model, sig_$Labels),
    .f = \(.m, .lab) {
      DBI::dbGetQuery(
        conn      = con_,
        statement = sprintf(
          fmt = "
            SELECT p.item_num AS Item, COUNT(*) AS NPar,
                   ROUND(100.0 * AVG(CASE WHEN s.label IN (%s) THEN 1 ELSE 0 END), 2) AS SignalPct
              FROM sc.scores s
              JOIN paragraphs p ON p.doc_id = s.doc_id AND p.par_id = s.par_id
             WHERE s.model = '%s' AND p.item_num IN
                   ('1','1A','2','3','5','7','7A','8','9','10','11','13','14','15')
             GROUP BY 1",
          paste(sprintf(fmt = "'%s'", trimws(x = .lab)), collapse = ", "),
          .m
        )
      ) |>
        tibble::as_tibble() |>
        dplyr::mutate(Model = .m, .before = 1)
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::select(Item, Model, SignalPct) |>
    tidyr::pivot_wider(names_from = Model, values_from = SignalPct) |>
    dplyr::mutate(Key = v5_item_key(.item = Item)) |>
    dplyr::arrange(Key) |>
    dplyr::select(-Key) |>
    as.data.frame() |>
    print(row.names = FALSE)

  # ---- D. Does a model add anything beyond the detector? -------------------------------------
  cli::cli_h2(text = "D. Conditional on ClDetect - what each model adds")
  cli::cli_text(
    text = "Several model cards describe their model as operating on CLIMATE-RELATED paragraphs.
If a model's signal rate is the same inside and outside the detector's positives, it is telling us
what the detector already did. A wide gap means it carries its own information - which is the
whole reason for scoring nine of them."
  )

  if ("ClDetect" %in% ok_) {
    purrr::pmap(
      .l = list(sig_$Model, sig_$Labels),
      .f = \(.m, .lab) {
        if (.m == "ClDetect") return(NULL)
        DBI::dbGetQuery(
          conn      = con_,
          statement = sprintf(
            fmt = "
              SELECT d.label AS Detector, COUNT(*) AS NPar,
                     ROUND(100.0 * AVG(CASE WHEN s.label IN (%s) THEN 1 ELSE 0 END), 2) AS SignalPct
                FROM sc.scores s
                JOIN sc.scores d
                  ON d.doc_id = s.doc_id AND d.par_id = s.par_id AND d.model = 'ClDetect'
               WHERE s.model = '%s'
               GROUP BY 1",
            paste(sprintf(fmt = "'%s'", trimws(x = .lab)), collapse = ", "),
            .m
          )
        ) |>
          tibble::as_tibble() |>
          dplyr::mutate(Model = .m, .before = 1)
      }
    ) |>
      purrr::compact() |>
      purrr::list_rbind() |>
      dplyr::select(Model, Detector, SignalPct) |>
      tidyr::pivot_wider(names_from = Detector, values_from = SignalPct,
                         names_prefix = "Detector_") |>
      dplyr::mutate(Lift = round(x = Detector_yes / pmax(Detector_no, 0.01), digits = 1)) |>
      as.data.frame() |>
      print(row.names = FALSE)
  } else {
    cli::cli_alert_warning(text = "ClDetect is not usable - panel D skipped")
  }

  # ---- E. Multi-class models, which have no single 'signal' ----------------------------------
  cli::cli_h2(text = "E. Multi-class label shares by era")
  multi_ <- mods_$Model[is.na(x = mods_$Signal) & mods_$Model %in% ok_]
  if (length(x = multi_) > 0) {
    DBI::dbGetQuery(
      conn      = con_,
      statement = sprintf(
        fmt = "
          SELECT s.model AS Model,
                 CASE WHEN r.filed_year < 2001 THEN '1993-2000'
                      WHEN r.filed_year < 2011 THEN '2001-2010' ELSE '2011+' END AS Era,
                 s.label AS Label, COUNT(*) AS N,
                 ROUND(100.0 * COUNT(*) / SUM(COUNT(*)) OVER (PARTITION BY s.model,
                   CASE WHEN r.filed_year < 2001 THEN '1993-2000'
                        WHEN r.filed_year < 2011 THEN '2001-2010' ELSE '2011+' END), 2) AS Pct
            FROM sc.scores s JOIN reports r ON r.doc_id = s.doc_id
           WHERE s.model IN (%s)
           GROUP BY 1, 2, 3 ORDER BY 1, 2, 4 DESC",
        paste(sprintf(fmt = "'%s'", multi_), collapse = ", ")
      )
    ) |>
      as.data.frame() |>
      print(row.names = FALSE)
  } else {
    cli::cli_alert_info(text = "No usable multi-class models")
  }

  # ---- F. What to take from it ---------------------------------------------------------------
  cli::cli_h2(text = "F. Reading this")
  cli::cli_ul(
    items = c(
      "Panel A first. A model with MedTokens ~2 saw no text; one with ModalShare ~1 has no
       variation to regress on, whatever its labels say.",
      "Panel B is the one that can invalidate the rest. A STEP at 2002-2007 means the route is in
       the measure. A trend through it is the secular rise in climate disclosure and is fine.",
      "Panel C tests a prior we can state in advance: climate content should concentrate in
       Item 1A. If it does not, either the item assignment or the model is wrong.",
      "Panel D says which models earn their place. Lift near 1 means the model duplicates the
       detector; a large lift means it adds information.",
      "None of this is a measure. Constructing one is a separate step, and one that should not
       start while the state x year identification question is open."
    )
  )

  invisible(x = list(Health = health_, ByYear = by_year_))
}


# =============================================================================================
# SUMMARY
# =============================================================================================

#' One screen of numbers covering everything built so far.
#'
#' v5_verify() and v5_score_verify() are diagnostics - they answer "is anything wrong". This
#' answers "what do we have", in the shape you would put in front of a coauthor. Detail lives in
#' the panels; this is the headline.
#'
#' `.path_scores` may be NULL, in which case only the paragraph store is described.
v5_summary <- function(.path_db, .path_scores = NULL) {

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  cli::cli_h1(text = "V5 summary")

  # ---- Corpus -------------------------------------------------------------------------------
  base_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT (SELECT COUNT(*) FROM reports WHERE parsed)  AS NDocs,
             (SELECT MIN(filed_year) FROM reports)        AS YrMin,
             (SELECT MAX(filed_year) FROM reports)        AS YrMax,
             (SELECT COUNT(*) FROM paragraphs)            AS NPar,
             (SELECT COUNT(*) FROM paragraphs WHERE is_prose) AS NProse,
             (SELECT COUNT(*) FROM sentences)             AS NSen,
             (SELECT COUNT(*) FROM _skipped)              AS NDeferred"
  ) |>
    tibble::as_tibble()

  cli::cli_alert_info(
    text = "{scales::comma(x = base_$NDocs)} documents, {base_$YrMin}-{base_$YrMax} | \\
            {scales::comma(x = base_$NPar)} paragraphs \\
            ({scales::comma(x = base_$NProse)} prose) | \\
            {scales::comma(x = base_$NSen)} sentences | \\
            {round(x = file.size(.path_db) / 1e9, digits = 1)} GB"
  )

  if (base_$NDeferred > 0) {
    cli::cli_alert_warning(
      text = "{scales::comma(x = base_$NDeferred)} document{?s} DEFERRED - run v5_run_skipped()"
    )
  }

  trunc_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = "SELECT COUNT(*) AS N FROM _progress WHERE limit_used IS NOT NULL"
  )$N
  if (trunc_ > 0) {
    cli::cli_alert_warning(
      text = "{trunc_} year{?s} written under a row limit - this store is a SAMPLE"
    )
  }

  # ---- Route, the endogenous one ------------------------------------------------------------
  cli::cli_h2(text = "Route and item coverage")
  DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT CASE WHEN filed_year < 2001 THEN '1993-2000'
                  WHEN filed_year < 2011 THEN '2001-2010' ELSE '2011+' END AS Era,
             route AS Route, COUNT(*) AS NDocs,
             ROUND(MEDIAN(n_prose))             AS MedProse,
             ROUND(100 * MEDIAN(item_cover), 1) AS ItemCoverPct
        FROM reports WHERE parsed GROUP BY 1, 2 ORDER BY 1, 2"
  ) |>
    as.data.frame() |>
    print(row.names = FALSE)

  # ---- Scores -------------------------------------------------------------------------------
  if (!is.null(x = .path_scores) && file.exists(.path_scores)) {

    cli::cli_h2(text = "Model scores")

    con2_ <- v5_connect(.path_db = .path_scores, .read_only = TRUE)
    on.exit(
      expr = DBI::dbDisconnect(conn = con2_, shutdown = TRUE),
      add  = TRUE
    )

    sc_ <- DBI::dbGetQuery(
      conn      = con2_,
      statement = "
        SELECT COUNT(DISTINCT model) AS NModels, COUNT(DISTINCT doc_id) AS NDocs,
               COUNT(*) AS NRows FROM scores"
    ) |>
      tibble::as_tibble()

    cli::cli_alert_info(
      text = "{sc_$NModels} model{?s} over {scales::comma(x = sc_$NDocs)} document{?s} \\
              ({round(x = 100 * sc_$NDocs / base_$NDocs, digits = 1)}% of the corpus), \\
              {scales::comma(x = sc_$NRows)} label{?s}"
    )

    # Aggregate and window cannot be mixed in one projection - MAX(n) is an aggregate while
    # SUM(n) OVER (...) is a window, and DuckDB rejects the combination. Compute the label counts,
    # then aggregate them, then join. Three CTEs rather than one clever expression.
    DBI::dbGetQuery(
      conn      = con2_,
      statement = "
        WITH lab AS (
          SELECT model, label, COUNT(*) AS n FROM scores GROUP BY 1, 2
        ), agg AS (
          SELECT model, COUNT(*) AS n_lab, MAX(n) AS n_modal, SUM(n) AS n_all
            FROM lab GROUP BY 1
        ), tok AS (
          SELECT model, ROUND(MEDIAN(n_tok)) AS med_tok FROM scores GROUP BY 1
        )
        SELECT a.model AS Model, t.med_tok AS MedTok, a.n_lab AS NLab,
               ROUND(100.0 * a.n_modal / a.n_all, 1) AS ModalPct
          FROM agg a JOIN tok t ON t.model = a.model
         ORDER BY 1"
    ) |>
      as.data.frame() |>
      print(row.names = FALSE)

    unmapped_ <- DBI::dbGetQuery(
      conn      = con2_,
      statement = "SELECT model FROM _models WHERE labels_mapped IS NOT TRUE"
    )$model
    if (length(x = unmapped_) > 0) {
      cli::cli_alert_warning(text = "Labels unmapped: {paste(unmapped_, collapse = ', ')}")
    }
  }

  invisible(x = base_)
}


# =============================================================================================
# ROUTE DIAGNOSTIC
# =============================================================================================
#
# THE PROBLEM THIS EXISTS TO SETTLE
# Measured 2026-08-19, 100 documents per year: in 2006 ClDetect returned 20.36% on the html route
# and 3.13% on the text route. Same year, same model, same sample size - a 6.5x gap. In 2008 it was
# 15.90 vs 1.63, nearly 10x. Every model moved the same direction.
#
# Route is a filing-agent property, so it is correlated with firm size and industry, and HTML
# adoption (crossover 2002-03, complete by 2007) overlaps the wildfire treatment cohorts. Neither
# fixed effect absorbs it: YEAR FE removes the year mean, and the gap is WITHIN year; FIRM FE
# removes the firm level, and the gap is a within-firm STEP at that firm's migration date - the
# worst possible shape for a staggered design with firm-specific timing.
#
# TWO CANDIDATE MECHANISMS, DIFFERENT REMEDIES
#
#   MECHANICAL. The unit differs by route: html paragraphs run ~75 median words, text ~49. A 50%
#   longer unit has a mechanically higher chance of containing a climate mention. But 1.5x length
#   against a 6.5x gap cannot be the whole story - a contributor, not the cause. Panel 1 removes it
#   entirely by measuring per 1,000 WORDS instead of per paragraph.
#
#   COMPOSITIONAL. By 2008 only a handful of filers still submitted ASCII, and they are small. The
#   text-route rate FALLING from ~13% in the 1990s to 1.6% in 2008 fits a shrinking, increasingly
#   unrepresentative branch rather than a parser degrading. Panel 2 is the decisive test: the same
#   FIRM observed on both routes in adjacent years. If its rate jumps when its agent switches
#   format, the parser is responsible. If it does not, the cross-sectional gap is composition and
#   firm FE handles the level.
#
# WHAT WOULD MAKE ME WRONG, stated first so the panels are a test and not a search:
#   - if the gap survives word-normalisation AND appears within firms, the parser is at fault and
#     the text route needs rebuilding
#   - if it survives normalisation but NOT within firms, it is composition: firm FE is enough, and
#     the finding is about who filed in ASCII rather than about the measure
#   - if normalisation removes most of it, the paragraph unit is not comparable across routes and
#     every rate should be expressed per word

#' Is the route gap mechanical, compositional, or a parser defect?
v5_route_diagnostic <- function(.path_db, .path_scores, .model = "ClDetect",
                                .signal = "yes", .min_par = 20L) {

  con_ <- v5_score_attach(
    .path_db     = .path_db,
    .path_scores = .path_scores,
    .alias       = "sc"
  )
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  sig_sql_ <- paste(sprintf(fmt = "'%s'", trimws(x = strsplit(x = .signal, split = ",")[[1]])),
                    collapse = ", ")

  cli::cli_h1(text = "Route diagnostic: {(.model)}")

  # ---- 1. Does word-normalisation close the gap? ---------------------------------------------
  cli::cli_h2(text = "1. Per paragraph vs per 1,000 words")
  cli::cli_text(
    text = "html paragraphs are ~50% longer than text-route paragraphs, so a per-PARAGRAPH rate is
not comparable across routes. Per 1,000 WORDS removes that entirely. If SignalPer1kRatio is near 1
while SignalPctRatio is 5-10, the gap was the unit, not the content."
  )

  norm_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "
        SELECT r.filed_year AS FiledYear, r.route AS Route,
               COUNT(*)                                                      AS NPar,
               ROUND(AVG(p.n_words), 1)                                      AS MedWords,
               ROUND(100.0 * AVG(CASE WHEN s.label IN (%s) THEN 1 ELSE 0 END), 2) AS SignalPct,
               ROUND(1000.0 * SUM(CASE WHEN s.label IN (%s) THEN 1 ELSE 0 END)
                     / NULLIF(SUM(p.n_words), 0), 3)                         AS SignalPer1k
          FROM sc.scores s
          JOIN paragraphs p ON p.doc_id = s.doc_id AND p.par_id = s.par_id
          JOIN reports    r ON r.doc_id = s.doc_id
         WHERE s.model = '%s'
         GROUP BY 1, 2 ORDER BY 1, 2",
      sig_sql_, sig_sql_, .model
    )
  ) |>
    tibble::as_tibble()

  norm_ |>
    dplyr::select(FiledYear, Route, NPar, MedWords, SignalPct, SignalPer1k) |>
    tidyr::pivot_wider(names_from = Route,
                       values_from = c(NPar, MedWords, SignalPct, SignalPer1k)) |>
    dplyr::filter(!is.na(x = SignalPct_html), !is.na(x = SignalPct_text)) |>
    dplyr::mutate(
      SignalPctRatio   = round(x = SignalPct_html / pmax(SignalPct_text, 0.01), digits = 1),
      SignalPer1kRatio = round(x = SignalPer1k_html / pmax(SignalPer1k_text, 0.001), digits = 1)
    ) |>
    dplyr::select(FiledYear, MedWords_html, MedWords_text,
                  SignalPct_html, SignalPct_text, SignalPctRatio,
                  SignalPer1k_html, SignalPer1k_text, SignalPer1kRatio) |>
    as.data.frame() |>
    print(row.names = FALSE)

  ov_ <- norm_ |>
    dplyr::group_by(Route) |>
    dplyr::summarise(
      MeanPct  = round(x = mean(x = SignalPct),   digits = 2),
      MeanPer1k = round(x = mean(x = SignalPer1k), digits = 3),
      .groups = "drop"
    )
  print(x = as.data.frame(x = ov_), row.names = FALSE)

  # ---- 2. THE DECISIVE TEST: the same firm, both routes, adjacent years ----------------------
  cli::cli_h2(text = "2. Within-firm route switches")
  cli::cli_text(
    text = "Firms observed on BOTH routes in consecutive filing years. Firm identity is held fixed,
so size, industry and disclosure policy cannot explain a difference. A jump means the PARSER; no
jump means the cross-sectional gap is filer COMPOSITION, and firm fixed effects handle the level."
  )

  sw_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "
        WITH doc AS (
          SELECT r.cik, r.filed_year, r.route,
                 SUM(CASE WHEN s.label IN (%s) THEN 1 ELSE 0 END) AS n_sig,
                 COUNT(*)      AS n_par,
                 SUM(p.n_words) AS n_words
            FROM sc.scores s
            JOIN paragraphs p ON p.doc_id = s.doc_id AND p.par_id = s.par_id
            JOIN reports    r ON r.doc_id = s.doc_id
           WHERE s.model = '%s'
           GROUP BY 1, 2, 3
          HAVING COUNT(*) >= %d
        ), pair AS (
          SELECT a.cik, a.filed_year AS YrA, b.filed_year AS YrB,
                 a.route AS RouteA, b.route AS RouteB,
                 100.0 * a.n_sig / a.n_par                AS PctA,
                 100.0 * b.n_sig / b.n_par                AS PctB,
                 1000.0 * a.n_sig / NULLIF(a.n_words, 0)  AS P1kA,
                 1000.0 * b.n_sig / NULLIF(b.n_words, 0)  AS P1kB
            FROM doc a JOIN doc b
              ON b.cik = a.cik AND b.filed_year = a.filed_year + 1 AND b.route <> a.route
        )
        SELECT RouteA || ' -> ' || RouteB AS Switch, COUNT(*) AS NFirms,
               ROUND(AVG(PctA), 2)  AS PctBefore,  ROUND(AVG(PctB), 2)  AS PctAfter,
               ROUND(AVG(PctB - PctA), 2)          AS PctDelta,
               ROUND(AVG(P1kA), 3)  AS Per1kBefore, ROUND(AVG(P1kB), 3) AS Per1kAfter,
               ROUND(AVG(P1kB - P1kA), 3)          AS Per1kDelta
          FROM pair GROUP BY 1 ORDER BY 1",
      sig_sql_, .model, .min_par
    )
  ) |>
    tibble::as_tibble()

  if (nrow(x = sw_) == 0) {
    cli::cli_alert_warning(
      text = "No firm appears on both routes in consecutive years. With a per-year sample this is
expected - the same firm has to be drawn twice. Re-run scoring without .limit, or raise it, before
treating this as evidence of anything."
    )
  } else {
    print(x = as.data.frame(x = sw_), row.names = FALSE)
    cli::cli_alert_info(
      text = "PctDelta is the WITHIN-FIRM change across a route switch. Compare it against the
cross-sectional gap in panel 1: if the cross-section shows 5-10x and this shows little, the gap is
composition rather than the parser."
    )
  }

  # ---- 3. Is the text route becoming unrepresentative? ---------------------------------------
  cli::cli_h2(text = "3. Who is still filing in ASCII")
  cli::cli_text(
    text = "The compositional story predicts the text branch shrinks AND its firms get smaller and
plainer. Block size is the only size proxy in this store - a crude one, but it does not require
Compustat and it is not derived from the text being measured."
  )

  DBI::dbGetQuery(
    conn      = con_,
    statement = "
      SELECT filed_year AS FiledYear, route AS Route, COUNT(*) AS NDocs,
             ROUND(MEDIAN(n_block) / 1000.0)     AS MedBlockKB,
             ROUND(MEDIAN(n_prose))              AS MedProse,
             ROUND(100 * MEDIAN(item_cover), 1)  AS ItemCoverPct
        FROM reports
       WHERE parsed AND filed_year BETWEEN 1998 AND 2010
       GROUP BY 1, 2 ORDER BY 1, 2"
  ) |>
    as.data.frame() |>
    print(row.names = FALSE)

  # ---- 4. Verdict ---------------------------------------------------------------------------
  cli::cli_h2(text = "4. Reading this")
  cli::cli_ul(
    items = c(
      "Panel 1: if SignalPer1kRatio is near 1 while SignalPctRatio is 5-10, the gap is the UNIT.
       Express every rate per word and the problem largely goes away.",
      "Panel 2 is decisive and panel 1 is not - a cross-sectional gap can be composition, a
       within-firm jump cannot.",
      "Panel 3 tests the compositional story on its own terms: a shrinking text branch whose
       filings get smaller is consistent with selection rather than parser failure.",
      "If the gap survives BOTH normalisation and the within-firm test, the text route needs
       rebuilding and no fixed effect will rescue it."
    )
  )

  invisible(x = list(Normalised = norm_, Switches = sw_))
}


# =============================================================================================
# TERM LISTS
# =============================================================================================
#
# WHAT THIS IS
# Dictionary flagging over the paragraphs built above: five CSV files, 23 lists, ~5,562 terms
# including four published dictionaries. One row per (document, paragraph, sentence, list, term)
# with a count, stored sparsely.
#
# ONE SEARCH, BOTH UNITS
# Sentences are stored as CHARACTER OFFSETS into paragraphs.text, not as separate strings. So the
# search runs over paragraph text once and records WHERE each match landed; the sentence is then a
# lookup - a match at offset 340 belongs to whichever sentence covers 340. Searching the two units
# separately would produce two numbers that ought to agree and occasionally would not. This way a
# paragraph count IS the sum of its sentence counts, arithmetically, with no room for drift.
#
# WORD BOUNDARIES ARE NOT OPTIONAL
# Two measured failures paid for this rule. `D&I` matched "BOARD IS" across 20% of the corpus.
# Unanchored `GRI` matched INTEGRITY, AGRICULTURAL, GRILL and GRID - 46% of V1's ESG sentences were
# substring false positives. Every alternation is wrapped in \b...\b.
#
# BRACKET NOTATION IS EXPANDED FIRST
# kww23 is transcribed AS PRINTED, and 47 of its 64 rows carry bracket notation such as
# "emission[s]". Unexpanded, those terms match nothing at all - a silent zero rather than an error.
# Both forms are stored: `term` as published, `form` as searched.
#
# CONTENT HASHING PER LIST
# Each list's flags are keyed to a hash of its expanded forms. Edit one list and only that list is
# invalidated; the other 22 stay. Carried forward from V4, where it was the difference between a
# one-minute re-flag and a full re-run.
#
# WHAT IS NOT DONE HERE
# No aggregation to firm-year, no measures. Paragraph, item and firm-year counts are GROUP BYs over
# `term_hits`, and n_words already sits on both paragraphs and sentences, so rates per 1,000 words
# need nothing extra.


# Store ----------------------------------------------------------------------------------------

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
        flagged_at TIMESTAMP,
        PRIMARY KEY (list_id, filed_year)
      )"
  )

  cli::cli_alert_success(text = "Term store ready: {basename(path = .path_terms)}")
  invisible(x = .path_terms)
}


# Loading the dictionary -----------------------------------------------------------------------

#' Expand bracket notation into the forms that will actually be searched.
#'
#' "emission[s]" -> c("emission", "emissions"). A term with no bracket is returned unchanged, so
#' this is safe to apply to every row.
#'
#' Only ONE bracket group is handled, which is all the published lists use. A second group would
#' need the cartesian product and is deliberately refused rather than silently half-expanded.
v5_terms_expand <- function(.term) {

  n_open_ <- stringi::stri_count_fixed(str = .term, pattern = "[")

  if (n_open_ == 0L) return(list(.term))
  if (n_open_ > 1L) {
    cli::cli_abort(
      message = c(
        "Term {(.term)} has {n_open_} bracket groups.",
        "i" = "Only one is supported. Expand it in the CSV or extend v5_terms_expand()."
      )
    )
  }

  m_ <- stringi::stri_match_first_regex(
    str     = .term,
    pattern = "^(.*)\\[(.+)\\](.*)$"   # prefix, optional part, suffix
  )
  if (is.na(x = m_[, 1])) return(list(.term))

  list(c(paste0(m_[, 2], m_[, 4]),              # without the bracketed part
         paste0(m_[, 2], m_[, 3], m_[, 4])))    # with it
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


# Patterns -------------------------------------------------------------------------------------

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


# Flagging -------------------------------------------------------------------------------------

#' Flag one batch of paragraphs against one list's pattern.
#'
#' Returns sparse counts at SENTENCE grain. The sentence is resolved from the match OFFSET against
#' the sentence spans already in the store - not by searching the sentence text again.
#'
#' `stri_locate_all_regex` gives positions; `stri_sub` on those positions gives which form matched.
#' Case is folded by searching upper-cased text against upper-cased forms, which is why the loaded
#' forms are upper-cased at load time.
v5_terms_flag_batch <- function(.par, .sen, .list_id, .pattern) {

  txt_ <- toupper(x = .par$text)

  loc_ <- stringi::stri_locate_all_regex(
    str          = txt_,
    pattern      = .pattern,
    omit_no_match = TRUE
  )

  n_hit_ <- purrr::map_int(.x = loc_, .f = \(.m) if (is.null(x = .m)) 0L else nrow(x = .m))
  if (sum(n_hit_) == 0L) {
    return(tibble::tibble(doc_id = character(length = 0), par_id = integer(length = 0),
                          sen_id = integer(length = 0), list_id = character(length = 0),
                          form = character(length = 0), n = integer(length = 0)))
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

  # AN INNER JOIN DROPS SILENTLY. If a match lands outside every stored sentence span, the hit
  # vanishes and n_hits under-reports with no error - the class of failure this project has already
  # paid for twice. Sentence spans should tile a prose paragraph, so a non-zero count here is a
  # defect in segmentation or in the offsets, not a curiosity.
  n_lost_ <- nrow(x = hits_) - nrow(x = joined_)
  if (n_lost_ > 0) {
    cli::cli_alert_warning(
      text = "{sprintf(fmt = '  %s of %s matches fell outside every sentence span (%s%%)',
                       scales::comma(x = n_lost_), scales::comma(x = nrow(x = hits_)),
                       format(x = round(x = 100 * n_lost_ / nrow(x = hits_), digits = 2)))}"
    )
  }

  joined_ |>
    dplyr::count(doc_id, par_id, sen_id, form, name = "n") |>
    dplyr::mutate(list_id = .list_id, n = as.integer(x = n)) |>
    dplyr::select(doc_id, par_id, sen_id, list_id, form, n)
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


#' Flag ONE list for ONE year. The unit of resumability.
v5_terms_flag_year <- function(.year,
                               .list_id,
                               .pattern,
                               .hash,
                               .n_forms,
                               .path_db,
                               .path_terms,
                               .prose_only = TRUE,
                               .doc_chunk  = 2000L) {

  t0_ <- Sys.time()

  con_ <- v5_connect(.path_db = .path_db, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )

  docs_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "SELECT doc_id FROM reports WHERE parsed AND filed_year = %d ORDER BY doc_id",
      .year
    )
  )$doc_id

  if (length(x = docs_) == 0) {
    cli::cli_alert_warning(text = "{(.year)} {(.list_id)}: no documents")
    return(invisible(x = NULL))
  }

  chunks_ <- unname(
    obj = split(
      x = docs_,
      f = ceiling(x = seq_along(along.with = docs_) / .doc_chunk)
    )
  )

  con_out_ <- v5_connect(.path_db = .path_terms, .read_only = FALSE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_out_, shutdown = TRUE),
    add  = TRUE
  )

  # Replace rather than append: a re-flag after a list edit must not add to the old counts.
  DBI::dbExecute(
    conn      = con_out_,
    statement = sprintf(
      fmt = "DELETE FROM term_hits WHERE list_id = '%s' AND doc_id IN (%s)",
      .list_id, paste(sprintf(fmt = "'%s'", docs_), collapse = ", ")
    )
  )

  n_hits_ <- 0L
  n_par_  <- 0L

  # PRINTED LINES, NOT A PROGRESS BAR. Two reasons, and the second is decisive:
  #   - a year is only a handful of document chunks, so a four-step bar says almost nothing
  #   - cli progress bars render NOTHING when the qmd is knitted to HTML, which is how this
  #     pipeline is actually run and reviewed. A printed line appears in both the console and the
  #     rendered document.
  cli::cli_alert_info(
    text = "{sprintf(fmt = '%d %s: %s documents in %d chunks',
                     .year, .list_id, scales::comma(x = length(x = docs_)),
                     length(x = chunks_))}"
  )

  for (i_ in seq_along(along.with = chunks_)) {

    ch_ <- chunks_[[i_]]

    ids_ <- paste(sprintf(fmt = "'%s'", ch_), collapse = ", ")

    par_ <- DBI::dbGetQuery(
      conn      = con_,
      statement = sprintf(
        fmt = "SELECT doc_id, par_id, text FROM paragraphs
                WHERE doc_id IN (%s)%s ORDER BY doc_id, par_id",
        ids_, if (.prose_only) " AND is_prose" else ""
      )
    ) |>
      tibble::as_tibble()

    if (nrow(x = par_) > 0) {
      sen_ <- DBI::dbGetQuery(
        conn      = con_,
        statement = sprintf(
          fmt = "SELECT doc_id, par_id, sen_id, char_start, char_end FROM sentences
                  WHERE doc_id IN (%s)", ids_
        )
      ) |>
        tibble::as_tibble()

      hit_ <- v5_terms_flag_batch(
        .par     = par_,
        .sen     = sen_,
        .list_id = .list_id,
        .pattern = .pattern
      )

      if (nrow(x = hit_) > 0) {
        DBI::dbAppendTable(conn = con_out_, name = "term_hits", value = hit_)
        n_hits_ <- n_hits_ + sum(hit_$n)
      }
      n_par_ <- n_par_ + nrow(x = par_)
    }

    el_  <- as.numeric(x = difftime(time1 = Sys.time(), time2 = t0_, units = "secs"))
    eta_ <- el_ / i_ * (length(x = chunks_) - i_)
    cli::cli_alert_info(
      text = "{sprintf(fmt = '  %d/%d | %s par | %s hits | %s par/s | eta %.1fm',
                       i_, length(x = chunks_), scales::comma(x = n_par_),
                       scales::comma(x = n_hits_),
                       scales::comma(x = round(x = n_par_ / max(el_, 0.001))),
                       eta_ / 60)}"
    )
  }

  secs_ <- as.numeric(x = difftime(time1 = Sys.time(), time2 = t0_, units = "secs"))

  DBI::dbExecute(
    conn      = con_out_,
    statement = sprintf(
      fmt = "DELETE FROM _flagged WHERE list_id = '%s' AND filed_year = %d", .list_id, .year
    )
  )
  DBI::dbAppendTable(
    conn  = con_out_,
    name  = "_flagged",
    value = tibble::tibble(
      list_id    = .list_id,
      filed_year = .year,
      list_hash  = .hash,
      n_forms    = as.integer(x = .n_forms),
      n_docs     = length(x = docs_),
      n_par      = as.integer(x = n_par_),
      n_hits     = as.numeric(x = n_hits_),
      secs       = secs_,
      flagged_at = Sys.time()
    )
  )

  cli::cli_alert_success(
    text = "{sprintf(fmt = '%d %s: %s hits in %s paragraphs, %.1fs',
                     .year, .list_id, scales::comma(x = n_hits_),
                     scales::comma(x = n_par_), secs_)}"
  )

  invisible(x = list(NHits = n_hits_, NPar = n_par_, Secs = secs_))
}


#' Flag every outstanding (list, year). Resumable, hash-aware.
v5_terms_build <- function(.path_db,
                           .path_terms,
                           .lists      = NULL,
                           .years      = NULL,
                           .prose_only = TRUE,
                           .doc_chunk  = 2000L,
                           .force      = FALSE) {

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
  cli::cli_alert_info(
    text = "{nrow(x = todo_)} list-year pair{?s} outstanding across \\
            {dplyr::n_distinct(todo_$ListId)} list{?s}"
  )

  t0_ <- Sys.time()

  for (i_ in seq_len(length.out = nrow(x = todo_))) {
    row_ <- todo_[i_, ]
    p_   <- dplyr::filter(.data = pat_, list_id == row_$ListId)
    v5_terms_flag_year(
      .year       = row_$FiledYear,
      .list_id    = row_$ListId,
      .pattern    = p_$Pattern[[1]],
      .hash       = p_$Hash[[1]],
      .n_forms    = p_$NForms[[1]],
      .path_db    = .path_db,
      .path_terms = .path_terms,
      .prose_only = .prose_only,
      .doc_chunk  = .doc_chunk
    )
  }

  cli::cli_alert_success(
    text = "Done in {round(x = as.numeric(x = difftime(time1 = Sys.time(), time2 = t0_,
                                                       units = 'mins')), digits = 1)} minutes"
  )

  invisible(x = v5_terms_verify(.path_terms = .path_terms, .path_db = .path_db))
}


# Verification ---------------------------------------------------------------------------------

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
        SELECT list_id, SUM(n_par) AS n_par, COUNT(*) AS n_years, SUM(secs) AS secs
          FROM _flagged GROUP BY 1
      )
      SELECT a.list_id AS List, a.n_forms AS NForms,
             COALESCE(f.n_fired, 0) AS NFired,
             ROUND(100.0 * (a.n_forms - COALESCE(f.n_fired, 0)) / a.n_forms, 1) AS DeadPct,
             COALESCE(f.n_hits, 0) AS NHits,
             COALESCE(f.n_docs, 0) AS NDocs,
             l.n_years AS NYears,
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


#' A sample of matched text in context. The boundary check that aggregates cannot do.
v5_terms_sample <- function(.path_db, .path_terms, .list_id, .form = NULL,
                            .n = 15L, .width = 90L, .seed = 42L) {

  con_ <- v5_connect(.path_db = .path_terms, .read_only = TRUE)
  on.exit(
    expr = DBI::dbDisconnect(conn = con_, shutdown = TRUE),
    add  = TRUE
  )
  DBI::dbExecute(
    conn      = con_,
    statement = sprintf(fmt = "ATTACH '%s' AS par (READ_ONLY)", .path_db)
  )

  out_ <- DBI::dbGetQuery(
    conn      = con_,
    statement = sprintf(
      fmt = "
        SELECT h.list_id AS List, h.form AS Form, h.n AS N,
               SUBSTR(p.text, GREATEST(s.char_start, 1),
                      LEAST(s.char_end - s.char_start + 1, %d)) AS Sentence
          FROM term_hits h
          JOIN par.sentences  s ON s.doc_id = h.doc_id AND s.par_id = h.par_id
                               AND s.sen_id = h.sen_id
          JOIN par.paragraphs p ON p.doc_id = h.doc_id AND p.par_id = h.par_id
         WHERE h.list_id = '%s'%s
         USING SAMPLE %d ROWS (reservoir, %d)",
      .width, .list_id,
      if (is.null(x = .form)) "" else sprintf(fmt = " AND h.form = '%s'", toupper(x = .form)),
      .n, .seed
    )
  ) |>
    tibble::as_tibble()

  DBI::dbExecute(conn = con_, statement = "DETACH par")
  out_
}
