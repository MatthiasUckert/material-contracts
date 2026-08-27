# ======================================================================================================================
# 05A-TextSearches -- find stated terms in the corpus, and record where each one sits
# ======================================================================================================================
#
# WHAT THIS IS FOR. Referee 2 asked whether the 2020-21 rise in material-contract filings is the
# pandemic or something else, and called the current reading speculative. That is a question about
# language, not about entities: it asks whether contracts of those quarters SAY something contracts
# of other quarters do not. Nothing in the 04 family answers it, because every rule there is built
# around a producer's spans and no producer emits "pandemic".
#
# THE ENGINE IS GENERAL AND THE QUESTION IS NOT. A term list is a configuration, so the same pass
# answers the next question of this shape without new code. That is why this is a search facility
# rather than a covid script.
#
# ONE ECONOMIC FACT SHAPES THE WHOLE DESIGN. The read is the cost. 04C measured it: one
# arrow::read_parquet() per document, a million times over, is most of the wall clock even at
# matcon's throughput, and a regular expression over text already in memory is nearly free beside it.
# Three consequences follow, and none of them is a preference:
#
#   1. EVERY TERM RIDES ONE PASS. Adding a term to a pass that is running costs approximately
#      nothing. Adding one afterwards costs a second traversal of the corpus. So the term list is
#      deliberately generous, and the placebo and validation families below are not decoration --
#      they are free.
#   2. HITS ARE STORED PER TERM, NEVER PER FAMILY. A family is a grouping of terms decided when the
#      results are read. Regrouping is then a join and costs nothing; had the family been baked into
#      the store, changing one's mind about which terms constitute "pandemic" would mean reading the
#      corpus again.
#   3. THE SCAN IS TWO-STAGE. One combined alternation decides whether a document contains anything
#      at all; only documents that pass pay for per-term location. Most documents match nothing, so
#      the second stage runs on a small minority.
#
# THE READER IS 03A'S, NOT A COPY. Every byte this document searches arrives through
# clf_read_text(), which is the same function that read the labelled sample and the same one 04C
# read the corpus with. A hit and a matcon span are therefore statements about one string rather
# than two that happen to agree. 04C's cor_read_text() is a parallel-map wrapper around that reader
# and is still used for the validation re-read; the pass itself calls the reader directly, for the
# reason below.
#
# READING AND SCANNING HAPPEN IN THE SAME WORKER, and that is the fourth consequence of the read
# being the cost. An earlier design read in parallel and scanned in the main session, which was
# wrong twice over. It left twenty-three cores idle through the expensive half -- and the two-stage
# filter does NOT rescue a serial scan, because the boilerplate family matches nearly every
# agreement ever drafted, so stage two runs on most of the corpus rather than on a minority. It also
# shipped roughly a gigabyte of text per chunk back from the daemons only to discard it after
# matching. Fusing the two sends paths out and brings hits back, and the hits are a small fraction
# of the text they were found in.
#
# OFFSETS ARE CODE POINTS AND THE TEXT IS NEVER REWRITTEN. Matching is case-insensitive through an
# inline flag, and a phrase's internal separators are compiled to a class, so no normalisation pass
# is needed to make matching robust. That is what keeps Start and Stop meaningful: stringi::stri_sub
# on the stored text reproduces any hit exactly, and a hit is comparable by containment against a
# matcon span in the same way 04B2 tests a party address against a governing-law clause.
#
# THE FOUR SOURCES ARE NOT ONE CORPUS. Contracts, 8-K reports and CT orders are read off the mirror
# by DocID. Item 1.01 is not: 01D stores the extracted summary as its own column, and its Start and
# Stop index a whitespace-normalised HTML body rather than TextRaw, so the summary cannot be
# recovered as a slice of the 8-K it came from. It is therefore a fourth source with its own text
# and its own offset origin, which is stated on every row rather than left to be inferred.


# 1. Vocabulary: the terms, and the families they group into -------------------------------------------------------
# EVERY TERM IS A PLAIN PHRASE. The regular expression is compiled from it by txt_pattern(), so the
# list below reads as language rather than as syntax and a referee can check it without parsing
# anything. Five families, and three of them exist to make the first one falsifiable.

.txt_family_levels <- c("Pandemic", "Disruption", "RateReform", "Regulation", "Boilerplate")

.txt_family_short <- c("Pandemic", "Disruption", "Rate reform", "Regulation", "Boilerplate")

plot_register_levels(
  .key     = "TermFamily",
  .levels  = .txt_family_levels,
  .short   = .txt_family_short,
  .colours = NULL
)

.txt_source_levels <- c("Exhibit10", "8-K", "Item101", "CTO")

plot_register_levels(
  .key     = "TextSource",
  .levels  = .txt_source_levels,
  .short   = c("Contracts", "8-K full", "Item 1.01", "CT orders"),
  .colours = NULL
)


#' The published term list
#'
#' FIVE FAMILIES, AND ONLY ONE OF THEM ANSWERS THE QUESTION. The other four are what turn a positive
#' result into evidence rather than into a search that found what it went looking for.
#'
#'   PANDEMIC is the referee's question. It carries the disease names, the public-health vocabulary,
#'   and the United States relief statute -- CARES Act, Paycheck Protection Program, PPP loan --
#'   because a large share of 2020 material agreements were relief loans, and a filing spike made of
#'   PPP loans is a pandemic explanation stated in the only language a credit agreement uses.
#'
#'   DISRUPTION is the contractual mechanism. Force majeure and business interruption are how a
#'   contract responds to a pandemic without naming one, so this family separates "the contract
#'   discusses the pandemic" from "the contract was reopened because of it".
#'
#'   RATEREFORM IS THE INSTRUMENT CHECK, and it is the most important family here after the first.
#'   The LIBOR cessation has a date nobody in this project chose: panels ceased through 2021 and
#'   2023, and SOFR replaced them in credit agreements. If this pass cannot recover a shock whose
#'   timing is externally known, its verdict on a shock whose timing is contested is worth nothing.
#'
#'   REGULATION is a second dated check, further back, to show the instrument works at the start of
#'   the window as well as the end.
#'
#'   BOILERPLATE IS THE PLACEBO. Governing law, counterparts and severability appear in nearly every
#'   agreement in every year. A time series in these terms must be flat. If it moves in 2020, then
#'   what moved is the corpus -- its composition, its length, its parse quality -- and not its
#'   language, and every other series on this page inherits that and means nothing.
#'
#' @param .families Character or NULL. Restrict to these families; NULL takes all five.
#' @param .sep_tolerant Logical. TRUE compiles internal spaces and hyphens to one class, so a phrase
#'   broken across a line break still matches.
#' @return Tibble: Family, Term, Pattern, one row per term.
txt_terms <- function(.families = NULL, .sep_tolerant = TRUE) {
  if (FALSE) {
    .families     <- NULL
    .sep_tolerant <- TRUE
  }

  out_ <- tibble::tribble(
    ~Family,        ~Term,
    "Pandemic",     "covid",
    "Pandemic",     "covid-19",
    "Pandemic",     "coronavirus",
    "Pandemic",     "sars-cov-2",
    "Pandemic",     "pandemic",
    "Pandemic",     "epidemic",
    "Pandemic",     "public health emergency",
    "Pandemic",     "quarantine",
    "Pandemic",     "social distancing",
    "Pandemic",     "shelter in place",
    "Pandemic",     "stay-at-home",
    "Pandemic",     "cares act",
    "Pandemic",     "paycheck protection program",
    "Pandemic",     "ppp loan",
    "Disruption",   "force majeure",
    "Disruption",   "act of god",
    "Disruption",   "business interruption",
    "Disruption",   "supply chain disruption",
    "Disruption",   "material adverse effect",
    "Disruption",   "material adverse change",
    "RateReform",   "libor",
    "RateReform",   "sofr",
    "RateReform",   "benchmark replacement",
    "RateReform",   "benchmark transition",
    "Regulation",   "sarbanes-oxley",
    "Regulation",   "dodd-frank",
    "Boilerplate",  "governing law",
    "Boilerplate",  "counterparts",
    "Boilerplate",  "severability"
  )

  if (!is.null(.families)) {
    bad_ <- setdiff(.families, .txt_family_levels)
    if (length(bad_) > 0L) cli::cli_abort("Unknown {cli::qty(bad_)}famil{?y/ies}: {(bad_)}.")
    out_ <- dplyr::filter(out_, .data$Family %in% .families)
  }

  out_ |>
    dplyr::mutate(
      Family  = factor(.data$Family, levels = .txt_family_levels),
      Pattern = txt_pattern(.phrase = .data$Term, .sep_tolerant = .sep_tolerant)
    ) |>
    dplyr::arrange(.data$Family, .data$Term)
}


#' Compile one plain phrase into a matching pattern
#'
#' THREE THINGS HAPPEN AND EACH ONE GUARDS A FAILURE THAT WOULD NOT ANNOUNCE ITSELF.
#'
#'   1. METACHARACTERS ARE ESCAPED. A term list is written by a researcher, not by a programmer, and
#'      a phrase containing a bracket or a dot must match that bracket or that dot rather than
#'      whatever the regular expression engine makes of it.
#'
#'   2. INTERNAL SEPARATORS BECOME A CLASS. Parsed filing text wraps, so "force majeure" arrives with
#'      a newline in the middle of it often enough to matter, and "covid-19" is written with a
#'      hyphen, a space or nothing at all depending on the drafter. One class covers all of it. The
#'      separator run is required rather than optional -- [-[:space:]]+ and not * -- because making
#'      it optional would let "covid" and "19" match across "covid" followed immediately by a
#'      section number.
#'
#'   3. THE MATCH IS BOUNDED BY WORD BREAKS. Without them "epidemic" matches inside a longer word and
#'      "sofr" matches inside a name. Every term in the published list opens and closes on an
#'      alphanumeric character, which is what makes \\b the right boundary; a term that did not would
#'      need a different one, so the condition is checked rather than assumed.
#'
#' CASE INSENSITIVITY IS AN INLINE FLAG AND NOT A CALL TO tolower(). Folding the text would move
#' every offset in the document off the string the offsets are supposed to index.
#'
#' @param .phrase Character vector of plain phrases.
#' @param .sep_tolerant Logical. TRUE compiles internal spaces and hyphens to one class.
#' @return Character vector of patterns, parallel to .phrase.
txt_pattern <- function(.phrase, .sep_tolerant = TRUE) {
  if (FALSE) {
    .phrase       <- c("force majeure", "covid-19")
    .sep_tolerant <- TRUE
  }

  edge_ <- !stringi::stri_detect_regex(.phrase, "^[[:alnum:]].*[[:alnum:]]$|^[[:alnum:]]$")
  if (any(edge_)) {
    cli::cli_abort(
      "{cli::qty(sum(edge_))}Term{?s} not opening and closing on an alphanumeric character: \\
       {(.phrase[edge_])}. The word-break boundary would not apply."
    )
  }

  esc_ <- stringi::stri_replace_all_regex(.phrase, "([\\\\^$.|?*+()\\[\\]{}])", "\\\\$1")

  body_ <- if (.sep_tolerant) {
    stringi::stri_replace_all_regex(esc_, "([[:space:]]|-)+", "[-[:space:]]+")
  } else {
    stringi::stri_replace_all_regex(esc_, "[[:space:]]+", "[[:space:]]+")
  }

  paste0("(?i)\\b", body_, "\\b")
}


#' One alternation standing for the whole term list
#'
#' THE FIRST STAGE OF THE SCAN, and the reason the pass is affordable. Asking twenty-nine separate
#' questions of every document means twenty-nine traversals of text that mostly answers no to all of
#' them. One alternation answers "any of these?" in a single traversal, and only the documents
#' saying yes go on to be located term by term.
#'
#' THE INLINE FLAGS ARE STRIPPED FROM THE PARTS AND SET ONCE ON THE WHOLE. An inline (?i) inside a
#' branch of an alternation is legal but its scope is not what a reader expects, so the flag is
#' hoisted rather than repeated.
#'
#' @param .terms Output of txt_terms().
#' @return A single pattern string.
txt_pattern_any <- function(.terms) {
  if (FALSE) .terms <- txt_terms()

  parts_ <- stringi::stri_replace_first_fixed(.terms$Pattern, "(?i)", "")
  paste0("(?i)(?:", paste(parts_, collapse = "|"), ")")
}


#' Fingerprint of the term list a pass was run under
#'
#' THE LEDGER IS KEYED ON THIS, so a changed term list produces a new cohort rather than a silent
#' mixture of documents scanned under two vocabularies. The hash covers the patterns rather than the
#' phrases, because the separator dial changes what was searched for without changing any phrase.
#'
#' A CHANGED TERM LIST COSTS A SECOND PASS OVER THE CORPUS, and nothing here can avoid that: the
#' text is not retained, so a term nobody searched for cannot be found later without reading the
#' documents again. That is the argument for a generous list rather than a minimal one.
#'
#' @param .terms Output of txt_terms().
#' @return A sixteen-character string.
txt_terms_hash <- function(.terms) {
  if (FALSE) .terms <- txt_terms()
  stringi::stri_sub(rlang::hash(paste(sort(.terms$Pattern), collapse = "|")), 1L, 16L)
}


#' Refuse to read a file that does not carry the columns being asked for
#'
#' THE SCHEMA COMES FROM ARROW AND IS NEVER INFERRED FROM THE CODE THAT WROTE IT. 02B shapes the
#' register with relocate(any_of(...)), and any_of() silently ignores a name that is not there -- so
#' the writer's column list is a statement of intent rather than of fact, and reading it as a schema
#' is how this document first asked the register for a DocType it does not have.
#'
#' WHAT IT BUYS IS THE MESSAGE. Without it the failure arrives from inside arrow's column_select()
#' naming one missing column and nothing else, so the next guess is as blind as the first. With it,
#' the abort names every missing column AND prints what the file actually carries, which is usually
#' enough to see the right name without opening anything.
#'
#' @param .path Path to a parquet file or dataset directory.
#' @param .cols Character vector of column names the caller is about to select.
#' @return The file's column names, invisibly.
txt_require_cols <- function(.path, .cols) {
  if (FALSE) {
    .path <- .lP$Input$Register
    .cols <- c("DocID", "Group", "DocTypeMod", "YQ")
  }

  if (!fs::file_exists(.path) && !fs::dir_exists(.path)) {
    cli::cli_abort("Nothing to read at {.path {(.path)}}.")
  }

  have_ <- names(arrow::open_dataset(sources = .path))
  miss_ <- setdiff(.cols, have_)

  if (length(miss_) > 0L) {
    cli::cli_abort(c(
      "x" = "{fs::path_file(.path)} is missing {cli::qty(miss_)}column{?s}: {(miss_)}.",
      "i" = "It carries: {(have_)}."
    ))
  }
  invisible(have_)
}


# 2. Input: which documents, and where their text is -----------------------------------------------------------------

#' The documents to scan, from the register
#'
#' THE REGISTER IS THE CORPUS. 02B holds every document with its group and its ladder step attached,
#' so membership and population come from one place; walking the mirror would make a second,
#' independent statement about what exists, one that can disagree without either side reporting it.
#'
#' ATTACHMENTS ARE SCANNED, NOT REGISTRANT COPIES. A filing naming several registrants produces one
#' file per registrant of the same attachment, and searching all of them would count one contract's
#' language once per co-filer. PrimaryFiler marks the copy that was actually read, so restricting to
#' it scans each attachment once; fanning back out to every registrant copy is a join on
#' HashDocument that any downstream question can perform when it wants firm-level counts.
#'
#' THE PATH IS BUILT, NOT STORED. utils_doc_path() is the one place a year-quarter becomes a
#' directory name, and it normalises the register's double form on the way. The type it takes is
#' DocTypeMod, which is the register's own column; DocType is what 04C calls it inside its own
#' corpus table and does not exist upstream of that.
#'
#' @param .path_register 02B's Documents.parquet.
#' @param .dir_mirror Root of the parsed document mirror.
#' @param .sources Character. Register groups to include: any of Exhibit10, 8-K, CTO.
#' @param .population Character. "all" takes every attachment; "descriptive" takes the descriptive
#'   sample only.
#' @return Tibble: DocID, Source, Path, HashDocument.
txt_source_index <- function(.path_register, .dir_mirror, .sources = c("Exhibit10", "8-K", "CTO"),
                             .population = "all") {
  if (FALSE) {
    .path_register <- .lP$Input$Register
    .dir_mirror    <- .lP$Input$Mirror
    .sources       <- c("Exhibit10", "8-K", "CTO")
    .population    <- "all"
  }

  if (!.population %in% c("all", "descriptive")) {
    cli::cli_abort("{.arg .population} must be all or descriptive.")
  }

  cols_ <- c("DocID", "HashDocument", "Group", "DocTypeMod", "YQ", "PrimaryFiler", "DescSample")
  txt_require_cols(.path = .path_register, .cols = cols_)

  # THE STRING FILTER IS PUSHED INTO ARROW AND THE LOGICAL ONES ARE NOT. Group is character, so
  # arrow skips the row groups that cannot match. PrimaryFiler and DescSample arrive as whatever the
  # writer stored -- integer here, not logical -- and a bare logical predicate on an integer column
  # is where an arrow filter quietly matches nothing at all. Both are coerced after the collect,
  # which is exactly what 04C does with the same two columns.
  reg_ <- arrow::open_dataset(sources = .path_register) |>
    dplyr::select(dplyr::all_of(cols_)) |>
    dplyr::filter(.data$Group %in% .sources) |>
    dplyr::collect()

  keep_ <- as.logical(reg_$PrimaryFiler)
  keep_ <- !is.na(keep_) & keep_

  if (identical(.population, "descriptive")) {
    desc_ <- as.logical(reg_$DescSample)
    keep_ <- keep_ & !is.na(desc_) & desc_
  }

  reg_[keep_, ] |>
    dplyr::mutate(
      Source = .data$Group,
      Path   = as.character(utils_doc_path(
        .dir_mirror = .dir_mirror,
        .doc_type   = .data$DocTypeMod,  # the register's type column; 04C builds the same path
        .yq         = .data$YQ,          # utils_doc_path normalises the register's double form
        .doc_id     = .data$DocID
      ))
    ) |>
    dplyr::select("DocID", "Source", "Path", "HashDocument") |>
    dplyr::arrange(.data$Source, .data$DocID)
}


#' The Item 1.01 source, which is a column and not a mirror
#'
#' WHY THIS IS A SOURCE AND NOT A FILTER OVER THE 8-K HITS. 01D locates the item span in a
#' whitespace-normalised rendering of the HTML column, so its Start and Stop do not index TextRaw --
#' which is what an 8-K scan reads and what an 8-K hit's offsets refer to. Testing an 8-K hit for
#' containment in the item span would compare two coordinate systems that look alike and are not, and
#' the failure would be a plausible number rather than an error.
#'
#' So the summary is searched as its own text, ItemText, and every offset from this source indexes
#' that column. OffsetOrigin records it on the row, because a hit whose coordinate system has to be
#' inferred from its source is a hit somebody will slice out of the wrong string.
#'
#' @param .path_item 01D's Item101.parquet.
#' @return Tibble: DocID, Source, Text, one row per document carrying a summary.
txt_item_index <- function(.path_item) {
  if (FALSE) .path_item <- .lP$Input$Item101

  arrow::open_dataset(sources = .path_item) |>
    dplyr::select("DocID", "ItemText") |>
    dplyr::filter(!is.na(.data$ItemText)) |>
    dplyr::collect() |>
    dplyr::transmute(.data$DocID, Source = "Item101", Text = .data$ItemText) |>
    dplyr::arrange(.data$DocID)
}


#' Per-document facts every descriptive needs
#'
#' IDENTITY AND FILING DATE COME FROM THE REGISTER; class and amendment type come from 03F's release
#' and exist for contracts only. A left join is what keeps an 8-K in the table with a missing class
#' rather than dropping it, and the coverage of the join is reported rather than assumed.
#'
#' nWords TRAVELS BECAUSE IT IS A DENOMINATOR. Terms per thousand words is the only comparable
#' intensity across sources whose lengths differ by an order of magnitude, and the register already
#' holds the count for every document, so nothing here opens a file to compute it.
#'
#' @param .path_register 02B's Documents.parquet.
#' @param .path_release Release parquet from 03F, or NA to leave the class columns missing.
#' @param .engine Character. Label prefix in the release.
#' @param .doc_ids Character. Restrict to these documents; NULL takes every row.
#' @return Tibble: DocID, Source, CIK, DateFiled, YQ, Year, nWords, DescSample, Class, ClassBroad,
#'   AmendType.
txt_keys <- function(.path_register, .path_release = NA_character_, .engine = "Bert",
                     .doc_ids = NULL) {
  if (FALSE) {
    .path_register <- .lP$Input$Register
    .path_release  <- path_release
    .engine        <- "Bert"
    .doc_ids       <- NULL
  }

  cols_ <- c("DocID", "Group", "CIK", "DateFiled", "YQ", "nWords", "DescSample", "PrimaryFiler")
  txt_require_cols(.path = .path_register, .cols = cols_)

  arr_ <- arrow::open_dataset(sources = .path_register) |>
    dplyr::select(dplyr::all_of(cols_))

  if (!is.null(.doc_ids)) arr_ <- dplyr::filter(arr_, .data$DocID %in% .doc_ids)

  reg_  <- dplyr::collect(arr_)
  keep_ <- as.logical(reg_$PrimaryFiler)
  keep_ <- !is.na(keep_) & keep_

  keys_ <- reg_[keep_, ] |>
    dplyr::transmute(
      .data$DocID,
      Source     = .data$Group,
      .data$CIK,
      DateFiled  = suppressWarnings(anytime::anydate(as.character(.data$DateFiled))),
      YQ         = as.numeric(.data$YQ),
      Year       = as.integer(floor(as.numeric(.data$YQ))),
      nWords     = as.integer(.data$nWords),
      .data$DescSample
    )

  cls_ <- ent_corpus_class(
    .path_release = .path_release,
    .engine       = .engine,
    .doc_ids      = keys_$DocID
  )

  dplyr::left_join(keys_, cls_, by = dplyr::join_by(DocID))
}


# 3. The store: what has been scanned, and what was found ------------------------------------------------------------
# ONE DUCKDB, THREE TABLES. The corpus table is the queue, the ledger records what has been scanned
# under which term list, and the hits table holds the findings. Chunked parquet would have done for
# the hits alone; the ledger is what makes an interrupted pass resumable, and a ledger wants a join.

#' Open the store and make sure its tables exist
#'
#' THE LEDGER IS KEYED ON DocID AND TermHash, NOT ON DocID AND Term. A per-term ledger would record
#' twenty-nine rows per document to say a document was looked at, and would buy nothing: the text is
#' not retained, so scanning one new term still means reading the document again. The honest key is
#' therefore the term list as a whole, and the consequence is stated where a reader meets it -- a
#' changed list is a new pass.
#'
#' @param .path Path to the database file.
#' @return A DBI connection.
txt_store_open <- function(.path) {
  if (FALSE) .path <- .lP$Output$Store

  fs::dir_create(fs::path_dir(.path))
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = .path)

  DBI::dbExecute(con_, "CREATE TABLE IF NOT EXISTS corpus (
    DocID VARCHAR, Source VARCHAR, Path VARCHAR, HashDocument VARCHAR)")

  DBI::dbExecute(con_, "CREATE TABLE IF NOT EXISTS ledger (
    DocID VARCHAR, Source VARCHAR, TermHash VARCHAR, DocLen INTEGER, nHits INTEGER)")

  DBI::dbExecute(con_, "CREATE TABLE IF NOT EXISTS hits (
    DocID VARCHAR, Source VARCHAR, OffsetOrigin VARCHAR, TermHash VARCHAR, Term VARCHAR,
    Start INTEGER, Stop INTEGER, MatchText VARCHAR, CueBefore VARCHAR, CueAfter VARCHAR,
    RelPos DOUBLE, DocLen INTEGER)")

  con_
}


#' Load the queue into the store
#'
#' WRITTEN ONCE AND REFRESHED, NOT APPENDED. A second render with a wider source list must extend the
#' queue rather than duplicate the rows already in it, so the table is replaced from the index the
#' document just built and the ledger -- which is what carries the work already done -- is untouched.
#'
#' @param .con Connection.
#' @param .tab_index Output of txt_source_index(), optionally with the Item 1.01 rows bound on.
#' @return Number of rows in the queue, invisibly.
txt_store_queue <- function(.con, .tab_index) {
  if (FALSE) {
    .con       <- con
    .tab_index <- tab_index
  }

  need_ <- c("DocID", "Source", "Path", "HashDocument")
  miss_ <- setdiff(need_, names(.tab_index))
  if (length(miss_) > 0L) cli::cli_abort("The index is missing {.val {miss_}}.")

  DBI::dbExecute(.con, "DELETE FROM corpus")
  DBI::dbAppendTable(.con, "corpus", as.data.frame(.tab_index[need_]))

  n_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM corpus")$N[[1L]]
  cli::cli_alert_info("Queue holds {format(n_, big.mark = ',')} {cli::qty(n_)}document{?s}.")
  invisible(n_)
}


#' What is still to scan under this term list
#'
#' THE COHORT IS CHOSEN BEFORE THE LEDGER IS CONSULTED, which is what makes a capped run repeatable.
#' Choosing it afterwards would make "the first fifty thousand" mean "the first fifty thousand still
#' outstanding", and that is a different fifty thousand on every render. Ordering on a hash of the
#' identifier rather than on the identifier itself keeps the cohort spread across sources and years
#' instead of taking the alphabetical head of one of them.
#'
#' @param .con Connection.
#' @param .term_hash Fingerprint of the term list, from txt_terms_hash().
#' @param .sources Character or NULL. Restrict the queue to these sources.
#' @param .limit Integer or NULL. Cap the cohort for a rehearsal; NULL takes everything.
#' @return Tibble: DocID, Source, Path.
txt_pending <- function(.con, .term_hash, .sources = NULL, .limit = NULL) {
  if (FALSE) {
    .con       <- con
    .term_hash <- term_hash
    .sources   <- NULL
    .limit     <- 50000L
  }

  where_ <- if (is.null(.sources)) {
    "TRUE"
  } else {
    paste0("Source IN (", paste0("'", .sources, "'", collapse = ", "), ")")
  }

  from_ <- if (is.null(.limit)) {
    glue::glue("(SELECT * FROM corpus WHERE {where_})")
  } else {
    glue::glue("(SELECT * FROM corpus WHERE {where_} ORDER BY md5(DocID) LIMIT {as.integer(.limit)})")
  }

  DBI::dbGetQuery(.con, glue::glue(
    "SELECT c.DocID, c.Source, c.Path
       FROM {from_} c
       LEFT JOIN (SELECT DocID, Source FROM ledger WHERE TermHash = '{.term_hash}') l
         ON c.DocID = l.DocID AND c.Source = l.Source
      WHERE l.DocID IS NULL
      ORDER BY md5(c.DocID)"
  )) |>
    tibble::as_tibble()
}


#' Append one chunk's findings and mark its documents scanned
#'
#' THE LEDGER ROW IS WRITTEN FOR EVERY DOCUMENT, INCLUDING THE ONES THAT MATCHED NOTHING. A document
#' absent from the ledger is a document still to read, so recording only the ones that hit would
#' re-scan the silent majority on every render and never terminate.
#'
#' BOTH WRITES OR NEITHER. The hits go in first and the ledger second, so an interruption between
#' them costs a repeated chunk rather than a document marked done whose findings were never stored.
#'
#' @param .con Connection.
#' @param .hits Tibble from txt_scan(); may have zero rows.
#' @param .seen Tibble: DocID, Source, DocLen, nHits, one row per document scanned.
#' @param .term_hash Fingerprint of the term list.
#' @return Invisibly NULL.
txt_write_chunk <- function(.con, .hits, .seen, .term_hash) {
  if (FALSE) {
    .con       <- con
    .hits      <- hits_
    .seen      <- seen_
    .term_hash <- term_hash
  }

  if (nrow(.hits) > 0L) {
    cols_ <- c("DocID", "Source", "OffsetOrigin", "TermHash", "Term", "Start", "Stop", "MatchText",
               "CueBefore", "CueAfter", "RelPos", "DocLen")
    out_  <- dplyr::mutate(.hits, TermHash = .term_hash)
    DBI::dbAppendTable(.con, "hits", as.data.frame(out_[cols_]))
  }

  seen_ <- .seen |>
    dplyr::mutate(TermHash = .term_hash) |>
    dplyr::select("DocID", "Source", "TermHash", "DocLen", "nHits")
  DBI::dbAppendTable(.con, "ledger", as.data.frame(seen_))

  invisible(NULL)
}


# 4. Construction: the scan ------------------------------------------------------------------------------------------

#' Locate every term in a vector of texts
#'
#' THE TWO STAGES ARE THE WHOLE PERFORMANCE ARGUMENT. Stage one asks one question of every document
#' through a single alternation. Stage two asks twenty-nine questions of the documents that answered
#' yes, which in this corpus is a small minority. Reversing the order would multiply the cost of the
#' pass by the number of terms and change no answer.
#'
#' EVERY OFFSET IS A CODE POINT. stringi indexes characters, so Start and Stop can be handed straight
#' to stri_sub() on the same text and will reproduce the match. Base substr() would index bytes, and
#' contract text is not pure ASCII often enough for that to be a rounding error.
#'
#' THE CUE WINDOWS ARE STORED, NOT THE DOCUMENT. A hit without its surroundings is a number nobody
#' can adjudicate; a hit carrying eighty characters either side can be read and judged in a console
#' table, which is what makes a false positive discoverable rather than theoretical. Storing the
#' document instead would multiply the corpus.
#'
#' A ZERO-LENGTH OR MISSING TEXT IS A DOCUMENT SCANNED, NOT A DOCUMENT SKIPPED. It contributes a
#' ledger row with no hits, because "this document says nothing" and "this document was never read"
#' must not look the same downstream.
#'
#' @param .doc_id Character vector of identifiers.
#' @param .source Character vector, parallel to .doc_id.
#' @param .text Character vector of text, parallel to .doc_id.
#' @param .terms Output of txt_terms().
#' @param .pattern_any Output of txt_pattern_any(), passed in so it is compiled once per pass.
#' @param .cue Integer. Characters kept either side of every match.
#' @return A list of two tibbles: Hits and Seen.
txt_scan <- function(.doc_id, .source, .text, .terms, .pattern_any, .cue = 80L) {
  if (FALSE) {
    .doc_id      <- chunk_$DocID
    .source      <- chunk_$Source
    .text        <- chunk_$Text
    .terms       <- tab_terms
    .pattern_any <- pattern_any
    .cue         <- 80L
  }

  txt_    <- dplyr::if_else(is.na(.text), "", .text)
  len_    <- stringi::stri_length(txt_)
  origin_ <- dplyr::if_else(.source == "Item101", "ItemText", "TextRaw")

  empty_ <- tibble::tibble(
    DocID = character(0), Source = character(0), OffsetOrigin = character(0), Term = character(0),
    Start = integer(0), Stop = integer(0), MatchText = character(0), CueBefore = character(0),
    CueAfter = character(0), RelPos = numeric(0), DocLen = integer(0)
  )

  seen_ <- tibble::tibble(
    DocID = .doc_id, Source = .source, DocLen = as.integer(len_), nHits = 0L
  )

  # STAGE ONE. One traversal per document, whatever the length of the term list.
  any_ <- stringi::stri_detect_regex(txt_, .pattern_any)
  idx_ <- which(any_ & len_ > 0L)
  if (length(idx_) == 0L) return(list(Hits = empty_, Seen = seen_))

  # STAGE TWO. Only the documents that matched something, and one located term at a time so the
  # match can be attributed. stri_locate_all_regex returns one matrix per element, with a single
  # all-NA row where the element did not match.
  hits_ <- purrr::map(
    .x = seq_len(nrow(.terms)),
    .f = function(.i) {
      loc_ <- stringi::stri_locate_all_regex(txt_[idx_], .terms$Pattern[[.i]])
      n_   <- vapply(loc_, \(.m) sum(!is.na(.m[, 1L])), integer(1))
      if (sum(n_) == 0L) return(NULL)

      pos_  <- do.call(rbind, loc_)
      keep_ <- !is.na(pos_[, 1L])
      tibble::tibble(
        Row   = rep(idx_, times = vapply(loc_, nrow, integer(1)))[keep_],
        Term  = .terms$Term[[.i]],
        Start = as.integer(pos_[keep_, 1L]),
        Stop  = as.integer(pos_[keep_, 2L])
      )
    }
  ) |>
    purrr::list_rbind()

  if (nrow(hits_) == 0L) return(list(Hits = empty_, Seen = seen_))

  out_ <- hits_ |>
    dplyr::mutate(
      DocID        = .doc_id[.data$Row],
      Source       = .source[.data$Row],
      OffsetOrigin = origin_[.data$Row],
      DocLen       = as.integer(len_[.data$Row]),
      MatchText    = stringi::stri_sub(txt_[.data$Row], .data$Start, .data$Stop),
      CueBefore    = stringi::stri_sub(txt_[.data$Row], pmax(1L, .data$Start - .cue),
                                       .data$Start - 1L),
      CueAfter     = stringi::stri_sub(txt_[.data$Row], .data$Stop + 1L,
                                       pmin(.data$DocLen, .data$Stop + .cue)),
      RelPos       = .data$Start / pmax(.data$DocLen, 1L)
    ) |>
    dplyr::mutate(
      dplyr::across(c("MatchText", "CueBefore", "CueAfter"),
                    \(.x) stringi::stri_replace_all_regex(.x, "[[:space:]]+", " "))
    ) |>
    dplyr::select(dplyr::all_of(names(empty_))) |>
    dplyr::arrange(.data$DocID, .data$Start)

  n_by_ <- dplyr::count(out_, .data$DocID, .data$Source, name = "N")
  seen_ <- seen_ |>
    dplyr::left_join(n_by_, by = dplyr::join_by(DocID, Source)) |>
    dplyr::mutate(nHits = as.integer(dplyr::coalesce(.data$N, 0L))) |>
    dplyr::select(-"N")

  list(Hits = out_, Seen = seen_)
}


#' Start daemons and source this document's chain into them
#'
#' A DAEMON IS A FRESH R SESSION AND HOLDS NONE OF THIS ONE'S FUNCTIONS. Sending a function as a
#' serialized closure and assuming its dependencies travel does not work, and the failure is silent
#' in the way that matters: every task comes back a miraiError, which mirai hands over as a value
#' rather than throwing. So the chain is sourced into each daemon instead.
#'
#' THE ORDER IS LOAD-BEARING. _Plots.R rebuilds its level registry empty on every source, and both
#' 03A and this file register a vocabulary against it at load time, so _Plots.R must precede them.
#' 04C is not in the list: the worker calls clf_read_text() directly, so 04C's wrapper is not needed
#' in a daemon.
#'
#' @param .workers Daemons to start. 1 or fewer starts none.
#' @return TRUE where daemons are up and carrying the chain.
txt_daemons_start <- function(.workers) {
  if (FALSE) .workers <- 24L
  if (.workers <= 1L) return(FALSE)

  mirai::daemons(.workers)

  ok_ <- tryCatch({
    mirai::everywhere(
      {
        for (.f in c(file.path("_Commons", "_Initialize.R"), file.path("_Commons", "_Utils.R"),
                     file.path("_Commons", "_Plots.R"),      file.path("_Commons", "_Tables.R"),
                     "03A-ClassifyPrepare.R",                "05A-TextSearches.R")) {
          source(file.path(.code, .f), encoding = "UTF-8")
        }
      },
      .code = here::here("1_code")
    )
    TRUE
  }, error = function(e) {
    cli::cli_alert_warning(
      "Could not prepare the daemons ({conditionMessage(e)}); this pass runs sequentially."
    )
    FALSE
  })

  if (!ok_) mirai::daemons(0L)
  ok_
}


#' Read and scan one batch, in whichever session is running it
#'
#' THE UNIT OF WORK, and it is deliberately whole: a batch arrives as paths and leaves as hits, so
#' the text exists only inside the worker that read it. Running this in the main session and running
#' it in a daemon differ in nothing but where they run, which is what makes the sequential fallback
#' produce an identical answer rather than an approximate one.
#'
#' A ROW EITHER CARRIES ITS TEXT OR CARRIES A PATH TO IT. Item 1.01 rows arrive with Text already
#' populated from 01D's column; every other row arrives with Text missing and a path to read. One
#' column and one code path, so the worker never asks which source it is looking at.
#'
#' @param .batch Tibble: DocID, Source, Path, Text. Text may be NA where Path is not.
#' @param .terms Output of txt_terms().
#' @param .pattern_any Output of txt_pattern_any().
#' @param .cue Integer. Characters kept either side of every match.
#' @return A list of two tibbles: Hits and Seen.
txt_scan_batch <- function(.batch, .terms, .pattern_any, .cue = 80L) {
  if (FALSE) {
    .batch       <- chunk_[1:10, ]
    .terms       <- tab_terms
    .pattern_any <- pattern_any_
    .cue         <- 80L
  }

  text_ <- .batch$Text
  need_ <- is.na(text_) & !is.na(.batch$Path)
  if (any(need_)) {
    text_[need_] <- vapply(.batch$Path[need_], clf_read_text, character(1), USE.NAMES = FALSE)
  }

  txt_scan(
    .doc_id      = .batch$DocID,
    .source      = .batch$Source,
    .text        = text_,
    .terms       = .terms,
    .pattern_any = .pattern_any,
    .cue         = .cue
  )
}


#' Read and scan one chunk across the daemons
#'
#' EVERYTHING THE WORKER NEEDS TRAVELS INSIDE ITS ONE ARGUMENT. mirai_map() maps the elements of .x
#' to the FIRST argument of .f and nothing else, and every way of smuggling further arguments past
#' that is a place this project has been bitten before. Packing the batch and the constants into one
#' list removes the question: .f takes one thing, unpacks it, and calls the worker with named
#' arguments. The term table is twenty-nine rows, so carrying a copy per batch costs nothing worth
#' measuring.
#'
#' AN ERROR IN A DAEMON COMES BACK AS A VALUE, NOT AS A CONDITION. inherits(x, "miraiError") is the
#' test; without it a failed batch is an object that silently is not a result, and the pass would
#' write a short chunk and mark its documents done.
#'
#' @param .chunk Tibble: DocID, Source, Path, Text.
#' @param .terms Output of txt_terms().
#' @param .pattern_any Output of txt_pattern_any().
#' @param .cue Integer. Characters kept either side of every match.
#' @param .workers Integer. Daemons to spread the chunk over; 1 or fewer runs here.
#' @return A list of two tibbles: Hits and Seen.
txt_pass_chunk <- function(.chunk, .terms, .pattern_any, .cue = 80L, .workers = 24L) {
  if (FALSE) {
    .chunk       <- ch_
    .terms       <- tab_terms
    .pattern_any <- pattern_any_
    .cue         <- 80L
    .workers     <- 24L
  }

  here_ <- function(.b) {
    txt_scan_batch(.batch = .b, .terms = .terms, .pattern_any = .pattern_any, .cue = .cue)
  }

  # Below the threshold the dispatch costs more than the split saves, and the last chunk of a pass
  # is routinely short.
  if (.workers <= 1L || nrow(.chunk) < 500L) return(here_(.chunk))

  n_    <- nrow(.chunk)
  grp_  <- ceiling(seq_len(n_) / ceiling(n_ / .workers))
  bats_ <- split(.chunk, grp_) |>
    purrr::map(\(.b) list(Docs = .b, Terms = .terms, PatternAny = .pattern_any, Cue = .cue))

  out_ <- tryCatch(
    mirai::mirai_map(
      .x = bats_,
      .f = function(.job) {
        txt_scan_batch(
          .batch       = .job$Docs,
          .terms       = .job$Terms,
          .pattern_any = .job$PatternAny,
          .cue         = .job$Cue
        )
      }
    )[],
    error = function(e) {
      cli::cli_alert_warning(
        "Parallel scan failed ({conditionMessage(e)}); this chunk runs here instead. The result is \\
         identical, only slower."
      )
      NULL
    }
  )

  if (!is.null(out_)) {
    err_ <- which(vapply(out_, \(.r) inherits(.r, "miraiError"), logical(1)))
    if (length(err_) > 0L) {
      cli::cli_alert_danger(
        "{length(err_)} of {length(out_)} batch{?es} failed in the daemons. First message:"
      )
      cli::cli_text("  {as.character(out_[[err_[[1L]]]])}")
      out_ <- NULL
    }
  }

  if (is.null(out_)) return(here_(.chunk))

  list(
    Hits = purrr::list_rbind(purrr::map(out_, "Hits")),
    Seen = purrr::list_rbind(purrr::map(out_, "Seen"))
  )
}


#' Run the pass, chunk by chunk
#'
#' THE LOOP IS: TAKE A CHUNK OF THE QUEUE, HAND IT TO THE DAEMONS, WRITE WHAT COMES BACK. Reading
#' and matching both happen out in the workers, so this session spends its time on the queue query
#' and the append and nothing else.
#'
#' THE TEXT JOIN KEYS ON SOURCE AS WELL AS DocID, and that is not tidiness. An Item 1.01 row and the
#' 8-K row it was extracted from carry the SAME DocID, because they are the same filing seen two
#' ways. A join on DocID alone would hand the 8-K row the summary as its text, so the full report
#' would be scanned as though it were the paragraph -- silently, and with plausible-looking hits.
#'
#' DAEMONS ARE STARTED ONCE PER PASS AND STOPPED ON EXIT. Startup is about a second each and a pass
#' is many chunks, so starting them inside the loop would spend more on daemons than on documents.
#' on.exit() is safe here because this is a function body.
#'
#' @param .con Connection.
#' @param .terms Output of txt_terms().
#' @param .item_text Tibble from txt_item_index(), or NULL where that source is not in the pass.
#' @param .term_hash Fingerprint of the term list.
#' @param .sources Character or NULL. Restrict the pass to these sources.
#' @param .chunk_size Integer. Documents per chunk.
#' @param .cue Integer. Characters kept either side of every match.
#' @param .workers Integer. Daemons that read and scan. One resource now, because one worker does
#'   both halves of the job.
#' @param .limit Integer or NULL. Cap the cohort for a rehearsal.
#' @param .report_every Integer. Chunks between progress lines.
#' @return Tibble: nDocs, nChunks, nHits, Seconds, DocsPerSecond.
txt_pass <- function(.con, .terms, .item_text = NULL, .term_hash, .sources = NULL,
                     .chunk_size = 20000L, .cue = 80L, .workers = 24L, .limit = NULL,
                     .report_every = 5L) {
  if (FALSE) {
    .con          <- con
    .terms        <- tab_terms
    .item_text    <- tab_item
    .term_hash    <- term_hash
    .sources      <- NULL
    .chunk_size   <- 20000L
    .cue          <- 80L
    .workers      <- 24L
    .limit        <- NULL
    .report_every <- 5L
  }

  queue_ <- txt_pending(.con = .con, .term_hash = .term_hash, .sources = .sources, .limit = .limit)
  if (nrow(queue_) == 0L) {
    cli::cli_alert_success("Nothing outstanding: every queued document is scanned under this term list.")
    return(tibble::tibble(nDocs = 0L, nChunks = 0L, nHits = 0L, Seconds = 0, DocsPerSecond = NA_real_))
  }

  cli::cli_alert_info(
    "{format(nrow(queue_), big.mark = ',')} {cli::qty(nrow(queue_))}document{?s} to scan against \\
     {nrow(.terms)} {cli::qty(nrow(.terms))}term{?s}."
  )

  pattern_any_ <- txt_pattern_any(.terms = .terms)
  par_ok_      <- txt_daemons_start(.workers = .workers)
  on.exit(mirai::daemons(0L), add = TRUE)

  chunks_ <- split(queue_, ceiling(seq_len(nrow(queue_)) / .chunk_size))
  start_  <- Sys.time()
  hits_n_ <- 0L

  for (.i in seq_along(chunks_)) {
    # ONE TEXT COLUMN FOR EVERY SOURCE. Item 1.01 rows get theirs from 01D's published column; every
    # other row keeps NA and a path, and the worker reads it. Keyed on Source as well as DocID -- see
    # the note above, where the two coincide.
    ch_ <- if (is.null(.item_text)) {
      dplyr::mutate(chunks_[[.i]], Text = NA_character_)
    } else {
      dplyr::left_join(
        chunks_[[.i]],
        dplyr::select(.item_text, "DocID", "Source", "Text"),
        by = dplyr::join_by(DocID, Source)
      )
    }

    res_ <- txt_pass_chunk(
      .chunk       = ch_,
      .terms       = .terms,
      .pattern_any = pattern_any_,
      .cue         = .cue,
      .workers     = if (par_ok_) .workers else 1L
    )

    txt_write_chunk(.con = .con, .hits = res_$Hits, .seen = res_$Seen, .term_hash = .term_hash)
    hits_n_ <- hits_n_ + nrow(res_$Hits)

    if (.i %% .report_every == 0L || .i == length(chunks_)) {
      done_ <- sum(vapply(chunks_[seq_len(.i)], nrow, integer(1)))
      secs_ <- as.numeric(difftime(Sys.time(), start_, units = "secs"))
      cli::cli_alert_info(
        "Chunk {(.i)}/{length(chunks_)}: {format(done_, big.mark = ',')} scanned, \\
         {format(hits_n_, big.mark = ',')} hits, {txt_elapsed(.secs = secs_)} elapsed."
      )
    }
  }

  secs_ <- as.numeric(difftime(Sys.time(), start_, units = "secs"))
  tibble::tibble(
    nDocs         = nrow(queue_),
    nChunks       = length(chunks_),
    nHits         = hits_n_,
    Seconds       = secs_,
    DocsPerSecond = nrow(queue_) / max(secs_, 1e-9)
  )
}


#' Seconds, minutes or hours, whichever a reader can hold in their head
#'
#' A pass over this corpus runs for hours, and a progress line reporting 9,431 seconds makes a reader
#' do arithmetic while they are trying to read a number.
#'
#' @param .secs Numeric. Seconds.
#' @return A character string.
txt_elapsed <- function(.secs) {
  if (FALSE) .secs <- 9431
  dplyr::case_when(
    .secs < 90    ~ paste0(round(.secs), "s"),
    .secs < 5400  ~ paste0(round(.secs / 60), "m"),
    .default      = paste0(round(.secs / 3600, 1), "h")
  )
}


# 5. Selection: from hits to one row per document ---------------------------------------------------------------------

#' One row per document per family
#'
#' EVERY SCANNED DOCUMENT GETS A ROW IN EVERY FAMILY, including the families it matched nothing in.
#' That is the same discipline 04B3 applies to a contract with no date: a mean over this file then
#' divides by the documents that were looked at rather than by the documents that happened to hit,
#' and a prevalence is a prevalence rather than a conditional one.
#'
#' THE FAMILY IS APPLIED HERE AND NOT IN THE STORE. Hits are held per term, so regrouping the term
#' list into different families is this join and nothing more -- no second pass over the corpus.
#'
#' PER THOUSAND WORDS, BECAUSE LENGTHS DIFFER BY AN ORDER OF MAGNITUDE ACROSS SOURCES. An Item 1.01
#' summary is a paragraph and a credit agreement is a hundred pages, so a raw count compares the two
#' on length rather than on language.
#'
#' @param .con Connection.
#' @param .terms Output of txt_terms().
#' @param .term_hash Fingerprint of the term list.
#' @param .keys Output of txt_keys(), supplying nWords.
#' @return Tibble: DocID, Source, Family, nHits, nTerms, HasTerm, FirstPos, PerKWords.
txt_doc_table <- function(.con, .terms, .term_hash, .keys) {
  if (FALSE) {
    .con       <- con
    .terms     <- tab_terms
    .term_hash <- term_hash
    .keys      <- tab_keys
  }

  seen_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT DocID, Source FROM ledger WHERE TermHash = '{.term_hash}'"
  )) |>
    tibble::as_tibble()

  hits_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT DocID, Source, Term, MIN(RelPos) AS FirstPos, COUNT(*) AS N
       FROM hits WHERE TermHash = '{.term_hash}'
      GROUP BY DocID, Source, Term"
  )) |>
    tibble::as_tibble() |>
    dplyr::left_join(dplyr::select(.terms, "Term", "Family"), by = dplyr::join_by(Term)) |>
    dplyr::summarise(
      nHits    = as.integer(sum(.data$N)),
      nTerms   = dplyr::n_distinct(.data$Term),
      FirstPos = min(.data$FirstPos),
      .by      = c("DocID", "Source", "Family")
    )

  grid_ <- tidyr::expand_grid(seen_, Family = factor(.txt_family_levels, .txt_family_levels))

  grid_ |>
    dplyr::left_join(hits_, by = dplyr::join_by(DocID, Source, Family)) |>
    dplyr::left_join(dplyr::select(.keys, "DocID", "nWords"), by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      nHits     = as.integer(dplyr::coalesce(.data$nHits, 0L)),
      nTerms    = as.integer(dplyr::coalesce(.data$nTerms, 0L)),
      HasTerm   = as.integer(.data$nHits > 0L),
      PerKWords = dplyr::if_else(
        is.na(.data$nWords) | .data$nWords == 0L, NA_real_, 1000 * .data$nHits / .data$nWords
      )
    ) |>
    dplyr::select("DocID", "Source", "Family", "nHits", "nTerms", "HasTerm", "FirstPos",
                  "PerKWords")
}


# 6. Results -----------------------------------------------------------------------------------------------------------

#' What the pass cost and covered
#'
#' @param .tab Output of txt_pass().
#' @param .n_queue Integer. Rows in the queue before the pass.
#' @return .tab invisibly.
txt_report_pass <- function(.tab, .n_queue) {
  if (FALSE) {
    .tab     <- tab_pass
    .n_queue <- n_queue
  }

  tbl_head("The pass")
  .tab |>
    dplyr::mutate(
      Elapsed   = txt_elapsed(.secs = .data$Seconds),
      Remaining = txt_elapsed(
        .secs = pmax(.n_queue - .data$nDocs, 0L) / pmax(.data$DocsPerSecond, 1e-9)
      ),
      DocsPerSecond = round(.data$DocsPerSecond, 1)
    ) |>
    dplyr::select("nDocs", "nChunks", "nHits", "DocsPerSecond", "Elapsed", "Remaining") |>
    tbl_out(
      .title = "Documents scanned in this render",
      .notes = c(
        DocsPerSecond = "Whole pass including the read, not the matching alone.",
        Remaining     = "What the rest of the queue would cost at this rate. Zero means complete."
      )
    )

  cli::cli_alert_info(
    "Reading and matching share one worker, so this rate covers both. 04C read alone at roughly 150 \\
     documents a second on this machine, which is the ceiling this pass is measured against."
  )
  invisible(.tab)
}


#' Every term, with what it found
#'
#' THE PER-TERM TABLE IS THE ONE THAT SETTLES A TERM LIST. A term firing on nearly every document is
#' not measuring an event, and a term firing on none is either absent from this language or written
#' wrongly. Both are visible here and nowhere else, because the family totals average them away.
#'
#' @param .con Connection.
#' @param .terms Output of txt_terms().
#' @param .term_hash Fingerprint of the term list.
#' @param .n_seen Integer. Documents scanned, the denominator.
#' @return Tibble, invisibly.
txt_report_terms <- function(.con, .terms, .term_hash, .n_seen) {
  if (FALSE) {
    .con       <- con
    .terms     <- tab_terms
    .term_hash <- term_hash
    .n_seen    <- n_seen
  }

  tab_ <- txt_term_counts(.con = .con, .terms = .terms, .term_hash = .term_hash, .n_seen = .n_seen)

  tbl_head("Every term, and what it found")
  tab_ |>
    dplyr::mutate(Family = as.character(.data$Family)) |>
    tbl_out(
      .title = "Hits and document coverage by term",
      .pct   = "ShareDocs",
      .notes = c(
        ShareDocs = "Share of scanned documents carrying the term at least once.",
        nHits     = "Occurrences, so a document mentioning a term twice counts twice."
      )
    )

  zero_ <- tab_$Term[tab_$nDocs == 0L]
  if (length(zero_) > 0L) {
    cli::cli_alert_warning(
      "{length(zero_)} {cli::qty(length(zero_))}term{?s} found nothing: {(zero_)}. A term absent \\
       from a corpus this size is more often a pattern defect than a fact about language."
    )
  } else {
    cli::cli_alert_success("Every term fired at least once, so no pattern is silently broken.")
  }
  invisible(tab_)
}


#' Hits and document coverage per term
#' @param .con Connection.
#' @param .terms Output of txt_terms().
#' @param .term_hash Fingerprint of the term list.
#' @param .n_seen Integer. Documents scanned.
#' @return Tibble: Family, Term, nHits, nDocs, ShareDocs.
txt_term_counts <- function(.con, .terms, .term_hash, .n_seen) {
  if (FALSE) {
    .con       <- con
    .terms     <- tab_terms
    .term_hash <- term_hash
    .n_seen    <- 1500000L
  }

  raw_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT Term, COUNT(*) AS nHits, COUNT(DISTINCT DocID) AS nDocs
       FROM hits WHERE TermHash = '{.term_hash}' GROUP BY Term"
  )) |>
    tibble::as_tibble()

  .terms |>
    dplyr::select("Family", "Term") |>
    dplyr::left_join(raw_, by = dplyr::join_by(Term)) |>
    dplyr::mutate(
      nHits     = as.integer(dplyr::coalesce(.data$nHits, 0L)),
      nDocs     = as.integer(dplyr::coalesce(.data$nDocs, 0L)),
      ShareDocs = .data$nDocs / max(.n_seen, 1L)
    ) |>
    dplyr::arrange(.data$Family, dplyr::desc(.data$nDocs))
}


#' Coverage by source
#' @param .con Connection.
#' @param .term_hash Fingerprint of the term list.
#' @return Tibble, invisibly.
txt_report_sources <- function(.con, .term_hash) {
  if (FALSE) {
    .con       <- con
    .term_hash <- term_hash
  }

  tab_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT l.Source, COUNT(*) AS nScanned, SUM(CASE WHEN l.nHits > 0 THEN 1 ELSE 0 END) AS nWithHit,
            CAST(AVG(l.DocLen) AS INTEGER) AS MeanChars
       FROM ledger l WHERE l.TermHash = '{.term_hash}' GROUP BY l.Source"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(ShareWithHit = .data$nWithHit / pmax(.data$nScanned, 1L)) |>
    dplyr::arrange(dplyr::desc(.data$nScanned))

  tbl_head("What was scanned, by source")
  tbl_out(
    .tab   = tab_,
    .title = "Documents scanned and documents matching any term",
    .pct   = "ShareWithHit",
    .notes = c(
      MeanChars    = "Mean characters scanned. Item 1.01 is a paragraph; a contract is not.",
      ShareWithHit = "Any term in any family, so the boilerplate family dominates it."
    )
  )
  invisible(tab_)
}


#' Read some actual matches
#'
#' THE ONLY DIAGNOSTIC THAT CATCHES A FALSE POSITIVE. A prevalence series cannot show that "cares
#' act" matched a sentence about who cares, or that "epidemic" matched a metaphor. Eighty characters
#' either side can, and a reader adjudicates it in a second.
#'
#' SAMPLED ON A HASH RATHER THAN TAKEN FROM THE HEAD, so the examples are not all from one filer or
#' one quarter, and the same call returns the same rows on a re-render.
#'
#' @param .con Connection.
#' @param .term_hash Fingerprint of the term list.
#' @param .terms Output of txt_terms().
#' @param .family Character. Which family to sample from.
#' @param .n Integer. Rows to show.
#' @param .chars Integer. Characters of context either side.
#' @return Tibble, invisibly.
txt_report_examples <- function(.con, .term_hash, .terms, .family = "Pandemic", .n = 12L,
                                .chars = 60L) {
  if (FALSE) {
    .con       <- con
    .term_hash <- term_hash
    .terms     <- tab_terms
    .family    <- "Pandemic"
    .n         <- 12L
    .chars     <- 60L
  }

  keep_ <- .terms$Term[.terms$Family == .family]
  if (length(keep_) == 0L) cli::cli_abort("No terms in family {(.family)}.")
  in_ <- paste0("'", keep_, "'", collapse = ", ")

  tab_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT Source, Term, RelPos, CueBefore, MatchText, CueAfter
       FROM hits WHERE TermHash = '{.term_hash}' AND Term IN ({in_})
      ORDER BY md5(DocID || Term || CAST(Start AS VARCHAR)) LIMIT {as.integer(.n)}"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      Context = paste0(
        "...", stringi::stri_sub(.data$CueBefore, -.chars, -1L),
        " [", .data$MatchText, "] ",
        stringi::stri_sub(.data$CueAfter, 1L, .chars), "..."
      ),
      RelPos = round(.data$RelPos, 2)
    ) |>
    dplyr::select("Source", "Term", "RelPos", "Context")

  tbl_head("Matches as they appear -- {(.family)}")
  tbl_out(
    .tab   = tab_,
    .title = paste0("A fixed sample of ", .family, " matches in context"),
    .notes = c(
      RelPos  = "Where in the document the match sits, 0 at the start and 1 at the end.",
      Context = "The match in brackets. A match that reads wrong here is a pattern to fix."
    )
  )
  invisible(tab_)
}


# 7. Validation ---------------------------------------------------------------------------------------------------------

#' Do the stored offsets still index the stored text
#'
#' THE CHECK THAT MATTERS, AND IT IS CHEAP. Every hit carries Start, Stop and the substring they were
#' taken from, so re-slicing the text at those offsets must return that substring. Failure means the
#' offsets and the text are in different coordinate systems, which is exactly the mistake avoided by
#' making Item 1.01 its own source rather than a window on the 8-K.
#'
#' RUN ON A SAMPLE AND NOT ON EVERY HIT, because it re-reads documents. A disagreement is structural
#' rather than sporadic: an offset system is either right or wrong, so a few hundred documents settle
#' it as firmly as a million would.
#'
#' @param .con Connection.
#' @param .term_hash Fingerprint of the term list.
#' @param .tab_index The queue, supplying Path for the disk-backed sources.
#' @param .item_text Tibble from txt_item_index(), or NULL.
#' @param .n Integer. Documents to re-read.
#' @return Tibble: Source, nChecked, nAgree, ShareAgree.
txt_check_offsets <- function(.con, .term_hash, .tab_index, .item_text = NULL, .n = 200L) {
  if (FALSE) {
    .con       <- con
    .term_hash <- term_hash
    .tab_index <- tab_index
    .item_text <- tab_item
    .n         <- 200L
  }

  docs_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT DISTINCT DocID, Source FROM hits WHERE TermHash = '{.term_hash}'
      ORDER BY md5(DocID) LIMIT {as.integer(.n)}"
  )) |>
    tibble::as_tibble()

  if (nrow(docs_) == 0L) {
    return(tibble::tibble(Source = character(0), nChecked = integer(0), nAgree = integer(0),
                          ShareAgree = numeric(0)))
  }

  in_   <- paste0("'", unique(docs_$DocID), "'", collapse = ", ")
  hits_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT DocID, Source, Start, Stop, MatchText FROM hits
      WHERE TermHash = '{.term_hash}' AND DocID IN ({in_})"
  )) |>
    tibble::as_tibble()

  disk_ <- docs_ |>
    dplyr::filter(.data$Source != "Item101") |>
    dplyr::left_join(dplyr::select(.tab_index, "DocID", "Path"), by = dplyr::join_by(DocID))
  disk_ <- if (nrow(disk_) > 0L) cor_read_text(.docs = disk_, .workers = 1L) else
    dplyr::mutate(disk_, Text = character(0))

  item_ <- docs_ |>
    dplyr::filter(.data$Source == "Item101")
  item_ <- if (nrow(item_) > 0L && !is.null(.item_text)) {
    dplyr::left_join(item_, dplyr::select(.item_text, "DocID", "Text"), by = dplyr::join_by(DocID))
  } else {
    dplyr::mutate(item_, Text = character(0))
  }

  txt_ <- dplyr::bind_rows(dplyr::select(disk_, "DocID", "Source", "Text"),
                           dplyr::select(item_, "DocID", "Source", "Text"))

  hits_ |>
    dplyr::left_join(txt_, by = dplyr::join_by(DocID, Source)) |>
    dplyr::filter(!is.na(.data$Text)) |>
    dplyr::mutate(
      Slice = stringi::stri_replace_all_regex(
        stringi::stri_sub(.data$Text, .data$Start, .data$Stop), "[[:space:]]+", " "
      ),
      Agree = .data$Slice == .data$MatchText
    ) |>
    dplyr::summarise(
      nChecked = dplyr::n(),
      nAgree   = sum(.data$Agree, na.rm = TRUE),
      .by      = "Source"
    ) |>
    dplyr::mutate(ShareAgree = .data$nAgree / pmax(.data$nChecked, 1L))
}


#' Report the offset check
#' @param .tab Output of txt_check_offsets().
#' @return .tab invisibly.
txt_report_checks <- function(.tab) {
  if (FALSE) .tab <- tab_check

  tbl_head("Do the offsets still index the text")
  tbl_out(
    .tab   = .tab,
    .title = "Re-slicing every sampled hit at its stored offsets",
    .pct   = "ShareAgree",
    .notes = c(ShareAgree = "Anything below one is an offset system mismatch, not a rounding issue.")
  )

  bad_ <- .tab$Source[.tab$ShareAgree < 1]
  if (length(bad_) > 0L) {
    cli::cli_alert_danger("Offsets disagree with the text for {(bad_)}. Nothing downstream is safe.")
  } else {
    cli::cli_alert_success("Every sampled hit re-slices to the stored match.")
  }
  invisible(.tab)
}


# 8. Robustness: the descriptives ---------------------------------------------------------------------------------------

#' Prevalence over time
#'
#' THE SHARE OF DOCUMENTS CARRYING A FAMILY, BY QUARTER. A share and not a count, because the number
#' of filings itself moves -- that movement is the referee's question -- and a count of pandemic
#' mentions would rise with the corpus whether or not the language changed.
#'
#' @param .tab_doc Output of txt_doc_table().
#' @param .keys Output of txt_keys().
#' @param .sources Character or NULL. Restrict to these sources.
#' @param .by Character. Extra grouping column from .keys, or NULL.
#' @return Tibble: YQ, Family, the grouping column where given, nDocs, nWith, Share.
txt_prevalence <- function(.tab_doc, .keys, .sources = NULL, .by = NULL) {
  if (FALSE) {
    .tab_doc <- tab_doc
    .keys    <- tab_keys
    .sources <- "Exhibit10"
    .by      <- "AmendType"
  }

  tab_ <- dplyr::left_join(
    .tab_doc,
    dplyr::select(.keys, "DocID", "YQ", "Year", dplyr::any_of(c("Class", "ClassBroad",
                                                                "AmendType"))),
    by = dplyr::join_by(DocID)
  )

  if (!is.null(.sources)) tab_ <- dplyr::filter(tab_, .data$Source %in% .sources)
  if (!is.null(.by))      tab_ <- dplyr::filter(tab_, !is.na(.data[[.by]]))

  grp_ <- c("YQ", "Family", if (is.null(.sources)) "Source" else NULL, .by)

  tab_ |>
    dplyr::filter(!is.na(.data$YQ)) |>
    dplyr::summarise(
      nDocs = dplyr::n(),
      nWith = sum(.data$HasTerm),
      .by   = dplyr::all_of(grp_)
    ) |>
    dplyr::mutate(Share = .data$nWith / pmax(.data$nDocs, 1L)) |>
    dplyr::arrange(.data$YQ, .data$Family)
}


#' Prevalence by contract category
#' @param .tab_doc Output of txt_doc_table().
#' @param .keys Output of txt_keys().
#' @param .family Character. Which family.
#' @param .years Integer vector or NULL. Restrict to these calendar years.
#' @return Tibble: Class, nDocs, nWith, Share.
txt_prevalence_class <- function(.tab_doc, .keys, .family = "Pandemic", .years = NULL) {
  if (FALSE) {
    .tab_doc <- tab_doc
    .keys    <- tab_keys
    .family  <- "Pandemic"
    .years   <- 2020:2021
  }

  tab_ <- .tab_doc |>
    dplyr::filter(.data$Family == .family, .data$Source == "Exhibit10") |>
    dplyr::left_join(dplyr::select(.keys, "DocID", "Class", "Year"), by = dplyr::join_by(DocID)) |>
    dplyr::filter(!is.na(.data$Class))

  if (!is.null(.years)) tab_ <- dplyr::filter(tab_, .data$Year %in% .years)

  tab_ |>
    dplyr::summarise(nDocs = dplyr::n(), nWith = sum(.data$HasTerm), .by = "Class") |>
    dplyr::mutate(Share = .data$nWith / pmax(.data$nDocs, 1L)) |>
    dplyr::arrange(dplyr::desc(.data$Share))
}


#' Where in a document a family's hits sit
#' @param .con Connection.
#' @param .term_hash Fingerprint of the term list.
#' @param .terms Output of txt_terms().
#' @param .bins Integer. Bins across the relative position.
#' @return Tibble: Family, Bin, N, Share.
txt_position <- function(.con, .term_hash, .terms, .bins = 20L) {
  if (FALSE) {
    .con       <- con
    .term_hash <- term_hash
    .terms     <- tab_terms
    .bins      <- 20L
  }

  DBI::dbGetQuery(.con, glue::glue(
    "SELECT Term, RelPos FROM hits WHERE TermHash = '{.term_hash}' AND Source = 'Exhibit10'"
  )) |>
    tibble::as_tibble() |>
    dplyr::left_join(dplyr::select(.terms, "Term", "Family"), by = dplyr::join_by(Term)) |>
    dplyr::mutate(Bin = pmin(floor(.data$RelPos * .bins) / .bins, (.bins - 1L) / .bins)) |>
    dplyr::summarise(N = dplyr::n(), .by = c("Family", "Bin")) |>
    dplyr::mutate(Share = .data$N / sum(.data$N), .by = "Family") |>
    dplyr::arrange(.data$Family, .data$Bin)
}


# 9. Deployment ----------------------------------------------------------------------------------------------------------

#' Write the two released files
#'
#' TWO FILES AND THEY ANSWER DIFFERENT QUESTIONS. term_hits holds one row per occurrence with its
#' offsets and its context, which is what lets anyone re-read a match or test it against a matcon
#' span. term_docs holds one row per document per family, which is what a regression joins to.
#'
#' THE TERM LIST TRAVELS WITH THEM. A hit table without the vocabulary that produced it cannot be
#' interpreted, let alone reproduced, so the compiled patterns are written beside the results.
#'
#' @param .con Connection.
#' @param .tab_doc Output of txt_doc_table().
#' @param .terms Output of txt_terms().
#' @param .term_hash Fingerprint of the term list.
#' @param .paths Named list: Hits, Docs, Terms.
#' @return Tibble of what was written, invisibly.
txt_write_release <- function(.con, .tab_doc, .terms, .term_hash, .paths) {
  if (FALSE) {
    .con       <- con
    .tab_doc   <- tab_doc
    .terms     <- tab_terms
    .term_hash <- term_hash
    .paths     <- .lP$Output$Release
  }

  DBI::dbExecute(.con, glue::glue(
    "COPY (SELECT * FROM hits WHERE TermHash = '{.term_hash}' ORDER BY DocID, Term, Start)
       TO '{.paths$Hits}' (FORMAT PARQUET)"
  ))

  arrow::write_parquet(.tab_doc, .paths$Docs)
  arrow::write_parquet(dplyr::mutate(.terms, TermHash = .term_hash, Family = as.character(.data$Family)),
                       .paths$Terms)

  out_ <- tibble::tibble(
    File  = c("term_hits", "term_docs", "term_list"),
    Path  = as.character(unlist(.paths[c("Hits", "Docs", "Terms")])),
    Rows  = c(
      DBI::dbGetQuery(.con, glue::glue(
        "SELECT COUNT(*) AS N FROM hits WHERE TermHash = '{.term_hash}'"
      ))$N[[1L]],
      nrow(.tab_doc),
      nrow(.terms)
    )
  ) |>
    dplyr::mutate(MB = round(as.numeric(fs::file_size(.data$Path)) / 1024^2, 1),
                  Path = fs::path_file(.data$Path))

  tbl_head("What was written")
  tbl_out(.tab = out_, .title = "Released files")
  invisible(out_)
}


# 10. Plots ---------------------------------------------------------------------------------------------------------------

#' Prevalence by quarter, one line per family
#'
#' NO TITLE IN THE PLOT. The caption carries it.
#'
#' @param .tab Output of txt_prevalence().
#' @param .families Character or NULL. Restrict to these families.
#' @return A ggplot.
txt_plot_prevalence <- function(.tab, .families = NULL) {
  if (FALSE) {
    .tab      <- tab_prev
    .families <- c("Pandemic", "Disruption")
  }

  dat_ <- if (is.null(.families)) .tab else dplyr::filter(.tab, .data$Family %in% .families)

  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$YQ, y = .data$Share, colour = .data$Family)) +
    ggplot2::geom_line(linewidth = 0.5) +
    plot_scale_colour_key(.key = "TermFamily") +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = "Documents carrying the family", colour = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}


#' Prevalence by quarter, split by amendment type
#' @param .tab Output of txt_prevalence() with .by = "AmendType".
#' @param .family Character. Which family.
#' @return A ggplot.
txt_plot_amend <- function(.tab, .family = "Pandemic") {
  if (FALSE) {
    .tab    <- tab_prev_amend
    .family <- "Pandemic"
  }

  .tab |>
    dplyr::filter(.data$Family == .family) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$YQ, y = .data$Share, colour = .data$AmendType)) +
    ggplot2::geom_line(linewidth = 0.5) +
    plot_scale_colour_key(.key = "AmendType") +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = "Contracts carrying the family", colour = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}


#' Prevalence by contract category
#' @param .tab Output of txt_prevalence_class().
#' @return A ggplot.
txt_plot_class <- function(.tab) {
  if (FALSE) .tab <- tab_prev_class

  plot_bar_ranked(
    .tab   = .tab,
    .cat   = "Class",
    .val   = "Share",
    .key   = "ClassDetailed",
    .short = TRUE,
    .label = TRUE,
    .pct   = TRUE
  ) +
    ggplot2::labs(x = NULL, y = "Contracts carrying the family")
}


#' Where in a contract the hits sit
#' @param .tab Output of txt_position().
#' @param .families Character or NULL. Restrict to these families.
#' @return A ggplot.
txt_plot_position <- function(.tab, .families = NULL) {
  if (FALSE) {
    .tab      <- tab_pos
    .families <- c("Pandemic", "Disruption", "Boilerplate")
  }

  dat_ <- if (is.null(.families)) .tab else dplyr::filter(.tab, .data$Family %in% .families)

  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Bin, y = .data$Share, fill = .data$Family)) +
    ggplot2::geom_col(width = 0.045) +
    ggplot2::facet_wrap(~ .data$Family, ncol = 1L, scales = "free_y") +
    plot_scale_fill_key(.key = "TermFamily") +
    plot_scale_y_pct() +
    ggplot2::labs(x = "Relative position in the document", y = "Share of the family's hits") +
    plot_theme(.grid = "y", .legend = "none")
}


#' Filings and prevalence on one panel
#'
#' THE REFEREE'S QUESTION IN ONE FIGURE. The filing count is what rose in 2020 and 2021; the
#' prevalence is whether the documents behind that rise say anything different. Read separately they
#' are two facts, and read together they are an answer.
#'
#' @param .tab_prev Output of txt_prevalence() for one source.
#' @param .tab_count Tibble: YQ, nDocs.
#' @param .family Character. Which family.
#' @return A ggplot.
txt_plot_spike <- function(.tab_prev, .tab_count, .family = "Pandemic") {
  if (FALSE) {
    .tab_prev  <- tab_prev
    .tab_count <- tab_count
    .family    <- "Pandemic"
  }

  prev_ <- .tab_prev |>
    dplyr::filter(.data$Family == .family) |>
    dplyr::select("YQ", "Share")

  scale_ <- max(.tab_count$nDocs, na.rm = TRUE) / max(prev_$Share, na.rm = TRUE)

  dplyr::left_join(.tab_count, prev_, by = dplyr::join_by(YQ)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$YQ)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$nDocs), fill = plot_pal_grey(1L), width = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = .data$Share * scale_), colour = plot_pal_seq(1L),
                       linewidth = 0.6) +
    ggplot2::scale_y_continuous(
      name     = "Attachments filed",
      labels   = \(.x) format(.x, big.mark = ",", scientific = FALSE),
      sec.axis = ggplot2::sec_axis(~ . / scale_, name = paste(.family, "prevalence"),
                                   labels = scales::percent_format(accuracy = 1))
    ) +
    ggplot2::labs(x = NULL) +
    plot_theme(.grid = "y", .legend = "none")
}
