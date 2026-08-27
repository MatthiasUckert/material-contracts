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

# THE FILING TYPES A CONTRACT CAN ARRIVE UNDER, grouped as the paper's Figure 2 groups them with one
# split added. Figure 2 pools 8-K with S-1, S-4, F-1 and F-4 as "ad-hoc", and those are two different
# things: an 8-K reports a material agreement, while S-1 and S-4 are REGISTRATION statements filed for
# an offering or a merger. Pooling them is what makes the 2021 spike unreadable, because the two
# explanations on the table -- pandemic contracting and the SPAC boom -- land in different halves of
# the pool.
.txt_form_levels <- c("Current report", "Registration", "Annual", "Quarterly", "Other")

# THE SAME LIST WITH THE REGISTRATION POOL OPENED. S-1 registers an offering and S-4 registers a
# merger, and a wave of one is a different event from a wave of the other. A blank-check wave lifts
# BOTH, with S-1 leading and S-4 following as the shells complete their mergers; a conventional
# listing boom lifts S-1 alone; ordinary M&A lifts S-4 alone. Pooled, the three are one number.
.txt_form_detail_levels <- c("Current report", "Offering (S-1, F-1)", "Merger (S-4, F-4)",
                             "Annual", "Quarterly", "Other")

plot_register_levels(
  .key     = "FormDetail",
  .levels  = .txt_form_detail_levels,
  .short   = c("8-K", "S-1/F-1", "S-4/F-4", "10-K/20-F", "10-Q", "Other"),
  .colours = NULL
)

# FOUR INDUSTRY GROUPS, AND ONLY TWO OF THEM ARE HYPOTHESES. 6770 is the SEC's own code for blank
# cheque companies, which is what makes the SPAC question a measurement rather than a reading of the
# calendar. The pharmaceutical group tests the other claim nobody has checked -- that the R&D
# agreements carrying pandemic language are vaccine and therapeutic work. "Other finance" exists so
# that a rise in 6770 cannot be waved away as finance in general.
.txt_sic_levels <- c("Blank check", "Pharma and biotech", "Other finance", "Other")

plot_register_levels(
  .key     = "FormGroup",
  .levels  = .txt_form_levels,
  .short   = c("8-K", "S-1/S-4", "10-K/20-F", "10-Q", "Other"),
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
#'   IT NO LONGER CARRIES epidemic OR quarantine, and that is a measured decision rather than a
#'   stylistic one. On the first corpus pass epidemic was the LARGEST term in this family, ahead of
#'   covid, which cannot be right; the break diagnostic then put its pre-2020 rate at 1.0% of
#'   contracts against 2.7% after, a ratio of 2.7 where covid's is unbounded and pandemic's is 43.
#'   Both terms live in force-majeure enumerations that predate COVID-19 by decades, so they were
#'   carrying this family's baseline without measuring the event. They are in DISRUPTION now.
#'
#'   THE MOVE IS NOT FREE AND THE COST IS WORTH STATING. Ratios of 2.7 and 2.2 are real responses,
#'   so some genuine pandemic signal leaves with them. It is affordable because covid, covid-19,
#'   coronavirus and pandemic already cover the event, and because a baseline that is mostly
#'   boilerplate makes every ratio on the page smaller than the truth.
#'
#'   DISRUPTION is the contractual mechanism. Force majeure and business interruption are how a
#'   contract responds to a pandemic without naming one, so this family separates "the contract
#'   discusses the pandemic" from "the contract was reopened because of it". It is also where
#'   epidemic and quarantine belong: the break diagnostic shows them behaving like the rest of this
#'   family -- present in every year, moving mildly with events -- rather than like an event marker.
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
#' @param .plural_tolerant Logical. TRUE lets a trailing s or es join the match, so a term written
#'   as a singular noun still finds its plural.
#' @return Tibble: Family, Term, Pattern, one row per term.
txt_terms <- function(.families = NULL, .sep_tolerant = TRUE, .plural_tolerant = TRUE) {
  if (FALSE) {
    .families        <- NULL
    .sep_tolerant    <- TRUE
    .plural_tolerant <- TRUE
  }

  out_ <- tibble::tribble(
    ~Family,        ~Term,
    "Pandemic",     "covid",
    "Pandemic",     "covid-19",
    "Pandemic",     "coronavirus",
    "Pandemic",     "sars-cov-2",
    "Pandemic",     "pandemic",
    "Pandemic",     "public health emergency",
    "Pandemic",     "social distancing",
    "Pandemic",     "shelter in place",
    "Pandemic",     "stay-at-home",
    "Pandemic",     "cares act",
    "Pandemic",     "paycheck protection program",
    "Pandemic",     "ppp loan",
    "Disruption",   "force majeure",
    "Disruption",   "act of god",
    # MOVED OUT OF PANDEMIC ON EVIDENCE, not on judgement -- see the break diagnostic in section 8.
    # Both sit inside force-majeure enumerations that predate COVID-19 by decades, and both carried
    # the pandemic family's pre-2020 baseline: epidemic at 1.0% of contracts before the break against
    # 2.7% after, quarantine at 0.4% against 0.9%. Ratios of 2.7 and 2.2 against covid's unbounded
    # one. They belong with the machinery a contract uses to respond to a disruption, not with the
    # names of the disruption itself.
    "Disruption",   "epidemic",
    "Disruption",   "quarantine",
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
      Pattern = txt_pattern(
        .phrase          = .data$Term,
        .sep_tolerant    = .sep_tolerant,
        .plural_tolerant = .plural_tolerant
      )
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
#'   4. A PLURAL IS THE SAME TERM. The closing boundary is what makes point three work, and on the
#'      first corpus pass it also silently refused every plural: "supply chain disruptions" does not
#'      match "supply chain disruption" because the s sits between two word characters where the
#'      boundary has to be. That term found 43 documents in 1.77 million, which is not a fact about
#'      contract language. An optional suffix before the boundary fixes it without loosening
#'      anything else -- "epidemic" still refuses to match inside "epidemiological", because after
#'      the optional group fails on the o there is still no break to be had.
#'
#' CASE INSENSITIVITY IS AN INLINE FLAG AND NOT A CALL TO tolower(). Folding the text would move
#' every offset in the document off the string the offsets are supposed to index.
#'
#' @param .phrase Character vector of plain phrases.
#' @param .sep_tolerant Logical. TRUE compiles internal spaces and hyphens to one class.
#' @param .plural_tolerant Logical. TRUE lets a trailing s or es join the match.
#' @return Character vector of patterns, parallel to .phrase.
txt_pattern <- function(.phrase, .sep_tolerant = TRUE, .plural_tolerant = TRUE) {
  if (FALSE) {
    .phrase          <- c("force majeure", "covid-19")
    .sep_tolerant    <- TRUE
    .plural_tolerant <- TRUE
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

  tail_ <- if (.plural_tolerant) "(?:s|es)?" else ""

  paste0("(?i)\\b", body_, tail_, "\\b")
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


#' Keep one pass in the store and drop every other
#'
#' THE STORE IS A CACHE, NOT AN ARCHIVE. A changed term list produces a new hash, and without this
#' the old pass sits alongside the new one forever: every query already filters on the hash so the
#' answers stay correct, but the file carries a vocabulary nobody will read again and grows by a
#' pass every time the list is revised.
#'
#' THIS IS IRREVERSIBLE AND CHEAPLY REVERSED, which is the only reason it is allowed to be blunt.
#' Nothing here is a source: every row was derived from the mirror by a stated vocabulary, so a
#' dropped pass is recovered by putting that vocabulary back and re-running. What it costs is the
#' corpus read, which is the same thing the pass cost the first time.
#'
#' THE COUNT IS TAKEN BEFORE THE DELETE AND REPORTED AFTER IT. A silent purge and a purge that found
#' nothing to do look identical from the console, and those are different states.
#'
#' @param .con Connection.
#' @param .term_hash Fingerprint of the term list to KEEP.
#' @param .drop_all Logical. TRUE empties the store completely, including the current hash, so the
#'   next pass starts from nothing.
#' @return Rows removed, invisibly.
txt_store_purge <- function(.con, .term_hash, .drop_all = FALSE) {
  if (FALSE) {
    .con       <- con
    .term_hash <- term_hash
    .drop_all  <- FALSE
  }

  where_ <- if (.drop_all) "TRUE" else glue::glue("TermHash <> '{.term_hash}'")

  before_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT TermHash, COUNT(*) AS nDocs FROM ledger WHERE {where_} GROUP BY TermHash"
  )) |>
    tibble::as_tibble()

  n_hits_ <- DBI::dbExecute(.con, glue::glue("DELETE FROM hits   WHERE {where_}"))
  n_led_  <- DBI::dbExecute(.con, glue::glue("DELETE FROM ledger WHERE {where_}"))

  # DuckDB releases the deleted blocks at a checkpoint rather than at the delete, so without this
  # the file keeps its old size and the purge looks like it did nothing.
  DBI::dbExecute(.con, "CHECKPOINT")

  if (nrow(before_) == 0L) {
    cli::cli_alert_info("Store holds one pass only; nothing to purge.")
  } else {
    cli::cli_alert_success(
      "Purged {nrow(before_)} stale {cli::qty(nrow(before_))}pass{?es} \\
       ({txt_n(n_led_)} ledger rows, {txt_n(n_hits_)} hits)."
    )
  }
  invisible(n_hits_ + n_led_)
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
  cli::cli_alert_info("Queue holds {txt_n(n_)} {cli::qty(n_)}document{?s}.")
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
    "{txt_n(nrow(queue_))} {cli::qty(nrow(queue_))}document{?s} to scan against \\
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
        "Chunk {(.i)}/{length(chunks_)}: {txt_n(done_)} scanned, {txt_n(hits_n_)} hits, \\
         {txt_elapsed(.secs = secs_)} elapsed."
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


#' A count, written the way a reader reads one
#'
#' format() SWITCHES TO SCIENTIFIC NOTATION ON ROUND NUMBERS AND NOWHERE ELSE, which is the worst
#' possible place for it to happen. 1,769,504 prints as itself because seven significant digits are
#' longer than the scientific form; 200,000 prints as 2e+05 because they are not. So a message that
#' is correct through an entire corpus pass turns into notation exactly when somebody caps a run at
#' a round number to rehearse it -- which is the moment they are reading the number most carefully.
#'
#' @param .x Numeric. A count.
#' @return A character string.
txt_n <- function(.x) {
  if (FALSE) .x <- 200000
  format(.x, big.mark = ",", scientific = FALSE, trim = TRUE)
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


#' Coverage by source, and whether a silent source was actually read
#'
#' A SOURCE THAT FOUND NOTHING HAS TWO POSSIBLE EXPLANATIONS AND THEY ARE NOT THE SAME. Either its
#' documents genuinely contain none of the terms, or its documents contain no text -- and on the
#' first corpus pass the CT orders returned exactly zero hits across sixteen thousand documents,
#' which is the shape both explanations make.
#'
#' THE LEDGER ALREADY SETTLES IT, so nothing is re-read to find out. DocLen was recorded for every
#' document as it was scanned, so the count of empty documents and the median length are queries
#' rather than a second pass. An empty count of zero against a median in the thousands means the
#' text was there and the terms were not, which is a fact about the source; a median near zero means
#' the opposite, and would make the source's silence an artifact.
#'
#' THE MEDIAN IS REPORTED BESIDE THE MEAN because a handful of long documents move a mean and cannot
#' move a median, and the question here is what a typical document of the source looks like.
#'
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
            SUM(CASE WHEN l.DocLen = 0 THEN 1 ELSE 0 END) AS nEmpty,
            CAST(AVG(l.DocLen) AS INTEGER) AS MeanChars,
            CAST(MEDIAN(l.DocLen) AS INTEGER) AS MedianChars
       FROM ledger l WHERE l.TermHash = '{.term_hash}' GROUP BY l.Source"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ShareWithHit = .data$nWithHit / pmax(.data$nScanned, 1L),
      ShareEmpty   = .data$nEmpty / pmax(.data$nScanned, 1L)
    ) |>
    dplyr::select("Source", "nScanned", "nWithHit", "ShareWithHit", "nEmpty", "ShareEmpty",
                  "MedianChars", "MeanChars") |>
    dplyr::arrange(dplyr::desc(.data$nScanned))

  tbl_head("What was scanned, by source")
  tbl_out(
    .tab   = tab_,
    .title = "Documents scanned, documents matching any term, and documents carrying no text",
    .pct   = c("ShareWithHit", "ShareEmpty"),
    .notes = c(
      ShareWithHit = "Any term in any family, so the boilerplate family dominates it.",
      ShareEmpty   = "Scanned but carrying no text at all. This is what separates a source that \\
                      says nothing from a source that was never really read.",
      MedianChars  = "A typical document of the source. Item 1.01 is a paragraph; a contract is not."
    )
  )

  # A SOURCE WITH NO HITS AT ALL IS REPORTED WITH ITS OWN DIAGNOSIS ATTACHED, because the number
  # that matters for interpreting it is on the same row and a reader should not have to pair them up.
  mute_ <- dplyr::filter(tab_, .data$nWithHit == 0L)
  for (.i in seq_len(nrow(mute_))) {
    src_   <- mute_$Source[[.i]]
    empty_ <- mute_$ShareEmpty[[.i]]
    med_   <- txt_n(mute_$MedianChars[[.i]])

    if (empty_ > 0.5) {
      cli::cli_alert_danger(
        "{(src_)} matched nothing, and {tbl_pct(empty_)} of it carries no text. That is a reading \\
         failure rather than a finding."
      )
    } else {
      cli::cli_alert_info(
        "{(src_)} matched no term in any family, but its documents were read -- median {(med_)} \\
         characters, {tbl_pct(empty_)} empty. The silence is a property of the source."
      )
    }
  }
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
                                .chars = 60L, .doc_ids = NULL, .label = NULL) {
  if (FALSE) {
    .con       <- con
    .term_hash <- term_hash
    .terms     <- tab_terms
    .family    <- "Pandemic"
    .n         <- 12L
    .chars     <- 60L
    .doc_ids   <- NULL
    .label     <- NULL
  }

  keep_ <- .terms$Term[.terms$Family == .family]
  if (length(keep_) == 0L) cli::cli_abort("No terms in family {(.family)}.")
  in_ <- paste0("'", keep_, "'", collapse = ", ")

  # RESTRICTING TO A SET OF DOCUMENTS IS WHAT LETS TWO PERIODS BE READ AGAINST EACH OTHER. A sample
  # drawn across the whole corpus answers "is the matching sound"; a sample drawn from one quarter
  # answers "what were these contracts saying then", and the second question is the one templating
  # turns on. Chunked because a DuckDB IN list of a hundred thousand identifiers is not a query.
  doc_ <- if (is.null(.doc_ids)) "TRUE" else {
    ids_ <- unique(.doc_ids)
    if (length(ids_) == 0L) "FALSE" else
      paste0("DocID IN (", paste0("'", ids_, "'", collapse = ", "), ")")
  }

  tab_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT Source, Term, RelPos, CueBefore, MatchText, CueAfter
       FROM hits WHERE TermHash = '{.term_hash}' AND Term IN ({in_}) AND {doc_}
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

  lab_ <- if (is.null(.label)) .family else paste0(.family, ", ", .label)
  tbl_head("Matches as they appear -- {(lab_)}")
  tbl_out(
    .tab   = tab_,
    .title = paste0("A fixed sample of ", lab_, " matches in context"),
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

#' Group a filing form the way the paper's Figure 2 groups it, with the ad-hoc pool split
#'
#' THE SPLIT IS THE WHOLE POINT. Figure 2's "ad-hoc" category is 8-K, S-1, S-4, F-1 and F-4 together.
#' An 8-K reports a material agreement as it happens; S-1 and S-4 register an offering or a merger and
#' carry whatever exhibits that transaction produced. If the 2021 rise is pandemic contracting it
#' belongs to the first; if it is the SPAC boom -- a shell's S-1 followed by a de-SPAC S-4 -- it
#' belongs to the second. Pooled, the two are indistinguishable, which is why the referee's question
#' cannot be answered from Figure 2 as drawn.
#'
#' THE AMENDMENT SUFFIX IS STRIPPED AND THE PREFIX TEST IS BOUNDED. An amended 8-K is still an 8-K, so
#' "/A" goes first. The registration test then refuses a following digit, because "^S-1" also matches
#' S-11 and "^F-1" also matches F-10, and neither belongs here. The annual and quarterly tests are
#' deliberately loose prefixes, because 10-K405 and 10-KSB are annual reports that ran through the
#' early part of this sample and an exact match would drop them into Other without saying so.
#'
#' @param .form Character vector of form types as the filing index records them.
#' @param .split Logical. TRUE opens the registration pool into offering and merger.
#' @return Character vector of group labels.
txt_form_group <- function(.form, .split = FALSE) {
  if (FALSE) {
    .form  <- c("8-K", "S-1/A", "10-K405", "S-11", "F-10", "10-Q")
    .split <- FALSE
  }

  base_ <- stringi::stri_replace_all_regex(trimws(.form), "/A$", "")

  reg_ <- if (.split) {
    dplyr::case_when(
      stringi::stri_detect_regex(base_, "^(S-1|F-1)([^0-9]|$)") ~ "Offering (S-1, F-1)",
      stringi::stri_detect_regex(base_, "^(S-4|F-4)([^0-9]|$)") ~ "Merger (S-4, F-4)",
      .default                                                  = NA_character_
    )
  } else {
    dplyr::if_else(
      stringi::stri_detect_regex(base_, "^(S-1|S-4|F-1|F-4)([^0-9]|$)"), "Registration",
      NA_character_
    )
  }

  dplyr::case_when(
    stringi::stri_detect_regex(base_, "^8-K")         ~ "Current report",
    !is.na(reg_)                                      ~ reg_,
    stringi::stri_detect_regex(base_, "^(10-K|20-F)") ~ "Annual",
    stringi::stri_detect_regex(base_, "^10-Q")        ~ "Quarterly",
    .default                                          = "Other"
  )
}


#' Group a filer's industry code around the two claims worth testing
#'
#' 6770 IS THE SEC'S OWN CODE FOR BLANK CHEQUE COMPANIES, which is what turns "this looks like the
#' SPAC wave" into something the data can refuse. A SPAC is a shell that lists purely to raise cash
#' into trust and then merges with a private operating company, so it files twice -- an S-1 for the
#' shell and an S-4 for the merger -- and each filing carries an unusual number of material
#' agreements.
#'
#' THE PHARMACEUTICAL GROUP TESTS A CLAIM THIS PROJECT HAS BEEN ASSERTING WITHOUT CHECKING: that the
#' R&D agreements carrying pandemic language are vaccine and therapeutic collaborations. It is a
#' reading of a class label and nothing more until an industry code agrees with it.
#'
#' @param .sic Character or numeric vector of SIC codes.
#' @return Character vector of group labels.
txt_sic_group <- function(.sic) {
  if (FALSE) .sic <- c("6770", "2836", "6022", "3711", NA)

  n_ <- suppressWarnings(as.integer(.sic))

  dplyr::case_when(
    is.na(n_)                                  ~ "Other",
    n_ == 6770L                                ~ "Blank check",
    n_ %in% c(2833L, 2834L, 2835L, 2836L, 8731L) ~ "Pharma and biotech",
    n_ >= 6000L & n_ <= 6799L                  ~ "Other finance",
    .default                                   = "Other"
  )
}


#' The filer industry behind each contract attachment
#'
#' SIC IS NOT IN THE REGISTER and is in 01C's landing table, which 05B's schema dump confirmed. The
#' join is on HashIndex, which _Entity.R reads from the register, so both sides of it are columns a
#' working script already depends on rather than columns this one hopes exist.
#'
#' THE CODE IS THE FILER'S, NOT THE CONTRACT'S. A blank cheque company attaching a subscription
#' agreement is coded 6770 because of what the registrant is, and that is exactly the question here.
#'
#' @param .path_register 02B's Documents.parquet.
#' @param .path_landing 01C's LandingPage.parquet.
#' @return Tibble: DocID, SIC, SICGroup.
txt_filer_sic <- function(.path_register, .path_landing) {
  if (FALSE) {
    .path_register <- .lP$Input$Register
    .path_landing  <- .lP$Input$Landing
  }

  txt_require_cols(.path = .path_landing, .cols = c("HashIndex", "SIC"))

  reg_ <- arrow::open_dataset(sources = .path_register) |>
    dplyr::select("DocID", "HashIndex", "Group", "PrimaryFiler") |>
    dplyr::filter(.data$Group == "Exhibit10") |>
    dplyr::collect() |>
    dplyr::filter(as.logical(.data$PrimaryFiler) %in% TRUE)

  land_ <- arrow::open_dataset(sources = .path_landing) |>
    dplyr::select("HashIndex", "SIC") |>
    dplyr::collect() |>
    dplyr::distinct(.data$HashIndex, .keep_all = TRUE)

  reg_ |>
    dplyr::left_join(land_, by = dplyr::join_by(HashIndex)) |>
    dplyr::mutate(
      SICGroup = factor(txt_sic_group(.sic = .data$SIC), levels = .txt_sic_levels)
    ) |>
    dplyr::select("DocID", "SIC", "SICGroup")
}


#' The filing form each contract arrived under
#'
#' FormType IS NOT CONFIRMED TO BE IN THE REGISTER, so this checks rather than assumes. It appears in
#' 02B's reg_shape() only inside relocate(any_of(...)), which ignores a name that is not there -- the
#' same construction that had this document asking the register for a DocType it does not have. Where
#' the column is present it is read directly; where it is not, the form comes from 01C's landing table
#' on HashIndex, which 05B confirmed carries it. Either way the answer is the same and the path taken
#' is reported.
#'
#' @param .path_register 02B's Documents.parquet.
#' @param .path_landing 01C's LandingPage.parquet, used only if the register lacks the column.
#' @return Tibble: DocID, FormType, FormGroup, one row per contract attachment.
txt_form_types <- function(.path_register, .path_landing, .split = FALSE) {
  if (FALSE) {
    .path_register <- .lP$Input$Register
    .path_landing  <- .lP$Input$Landing
    .split         <- FALSE
  }

  have_ <- names(arrow::open_dataset(sources = .path_register))

  out_ <- if ("FormType" %in% have_) {
    cli::cli_alert_info("Form types read from the register.")
    arrow::open_dataset(sources = .path_register) |>
      dplyr::select("DocID", "Group", "PrimaryFiler", "FormType") |>
      dplyr::filter(.data$Group == "Exhibit10") |>
      dplyr::collect()
  } else {
    cli::cli_alert_info("The register carries no {.field FormType}; joining 01C's landing table.")
    txt_require_cols(.path = .path_landing, .cols = c("HashIndex", "FormType"))

    reg_ <- arrow::open_dataset(sources = .path_register) |>
      dplyr::select("DocID", "HashIndex", "Group", "PrimaryFiler") |>
      dplyr::filter(.data$Group == "Exhibit10") |>
      dplyr::collect()

    land_ <- arrow::open_dataset(sources = .path_landing) |>
      dplyr::select("HashIndex", "FormType") |>
      dplyr::collect() |>
      dplyr::distinct(.data$HashIndex, .keep_all = TRUE)

    dplyr::left_join(reg_, land_, by = dplyr::join_by(HashIndex))
  }

  out_ |>
    dplyr::filter(as.logical(.data$PrimaryFiler) %in% TRUE) |>
    dplyr::mutate(
      FormGroup = factor(
        txt_form_group(.form = .data$FormType, .split = .split),
        levels = if (.split) .txt_form_detail_levels else .txt_form_levels
      )
    ) |>
    dplyr::select("DocID", "FormType", "FormGroup")
}


#' Contract attachments by quarter and filing form
#'
#' THE COMPOSITION, NOT THE COUNT. A rise in contract filings is one fact; a rise concentrated in one
#' kind of filing is a different and much more informative one, because each form implies a different
#' reason for the exhibit to exist.
#'
#' @param .keys Output of txt_keys().
#' @param .forms Output of txt_form_types().
#' @param .year_min Integer or NULL. First calendar year reported.
#' @param .year_max Integer or NULL. Last calendar year reported.
#' @return Tibble: YQ, FormGroup, nDocs, Share.
txt_table_composition <- function(.keys, .forms, .year_min = NULL, .year_max = NULL) {
  if (FALSE) {
    .keys      <- tab_keys
    .forms     <- tab_forms
    .year_min  <- 2001L
    .year_max  <- NULL
  }

  .keys |>
    dplyr::filter(.data$Source == "Exhibit10") |>
    dplyr::inner_join(.forms, by = dplyr::join_by(DocID)) |>
    txt_window(.year_min = .year_min, .year_max = .year_max) |>
    dplyr::filter(!is.na(.data$YQ)) |>
    dplyr::summarise(nDocs = dplyr::n(), .by = c("YQ", "FormGroup")) |>
    dplyr::mutate(Share = .data$nDocs / sum(.data$nDocs), .by = "YQ") |>
    dplyr::arrange(.data$YQ, .data$FormGroup)
}


#' Does the filing form that surged carry the pandemic language
#'
#' THE TEST THAT SEPARATES THE TWO EXPLANATIONS, and it needs both halves to be read together. If the
#' rise is pandemic contracting, the surging form carries pandemic language at least as often as the
#' others. If it is a transaction wave, the surging form carries a great many contracts and no more
#' pandemic language than anything else -- possibly less, since a registration statement's exhibits
#' were drafted for the deal rather than for the year.
#'
#' @param .tab_doc Output of txt_doc_table().
#' @param .keys Output of txt_keys().
#' @param .forms Output of txt_form_types().
#' @param .family Character. Which family.
#' @param .quarters Numeric vector or NULL. Restrict to these year-quarters.
#' @return Tibble: FormGroup, nDocs, nWith, Share.
txt_table_form_family <- function(.tab_doc, .keys, .forms, .family = "Pandemic",
                                  .quarters = NULL) {
  if (FALSE) {
    .tab_doc  <- tab_doc
    .keys     <- tab_keys
    .forms    <- tab_forms
    .family   <- "Pandemic"
    .quarters <- c(2021.1)
  }

  tab_ <- .tab_doc |>
    dplyr::filter(.data$Family == .family, .data$Source == "Exhibit10") |>
    dplyr::left_join(dplyr::select(.keys, "DocID", "YQ"), by = dplyr::join_by(DocID)) |>
    dplyr::inner_join(.forms, by = dplyr::join_by(DocID))

  if (!is.null(.quarters)) tab_ <- dplyr::filter(tab_, .data$YQ %in% .quarters)

  tab_ |>
    dplyr::summarise(nDocs = dplyr::n(), nWith = sum(.data$HasTerm), .by = "FormGroup") |>
    dplyr::mutate(Share = .data$nWith / pmax(.data$nDocs, 1L)) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}


#' Trim a table to the years the paper reports on
#'
#' THIS TRIMS WHAT IS DRAWN AND NOTHING ELSE. The corpus index is built with .population = "all", so
#' the pass reads every attachment EDGAR holds -- and EDGAR holds filings from 1993, while the study
#' window opens in 2001. Those eight years are real documents that were really scanned, and they are
#' also eight years of thin, unrepresentative counts sitting on the left of every time series where
#' they invite a reading the paper does not make.
#'
#' THE STORE AND THE RELEASE ARE UNTOUCHED BY IT, which is the point of doing it here rather than in
#' the queue. A document dropped from the queue is a document nobody can go back to without another
#' corpus read; a document dropped from a figure is a filter. The released files carry every scanned
#' document, and the sample restriction is applied once, at export, where the rest of the sample
#' definition is applied too.
#'
#' @param .tab Tibble carrying a Year column.
#' @param .year_min Integer or NULL. First calendar year to keep.
#' @param .year_max Integer or NULL. Last calendar year to keep. NULL keeps everything after
#'   .year_min, which is right until a partial final quarter starts distorting the end of a line.
#' @return .tab, trimmed.
txt_window <- function(.tab, .year_min = NULL, .year_max = NULL) {
  if (FALSE) {
    .tab      <- tab_
    .year_min <- 2001L
    .year_max <- NULL
  }

  if (!"Year" %in% names(.tab)) cli::cli_abort("The table carries no {.field Year} to trim on.")

  out_ <- .tab
  if (!is.null(.year_min)) out_ <- dplyr::filter(out_, .data$Year >= .year_min)
  if (!is.null(.year_max)) out_ <- dplyr::filter(out_, .data$Year <= .year_max)
  out_
}


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
#' @param .year_min Integer or NULL. First calendar year to report; see txt_window().
#' @param .year_max Integer or NULL. Last calendar year to report.
#' @return Tibble: YQ, Family, the grouping column where given, nDocs, nWith, Share.
txt_prevalence <- function(.tab_doc, .keys, .sources = NULL, .by = NULL, .year_min = NULL,
                           .year_max = NULL) {
  if (FALSE) {
    .tab_doc  <- tab_doc
    .keys     <- tab_keys
    .sources  <- "Exhibit10"
    .by       <- "AmendType"
    .year_min <- 2001L
    .year_max <- NULL
  }

  tab_ <- dplyr::left_join(
    .tab_doc,
    dplyr::select(.keys, "DocID", "YQ", "Year", dplyr::any_of(c("Class", "ClassBroad",
                                                                "AmendType"))),
    by = dplyr::join_by(DocID)
  )

  if (!is.null(.sources)) tab_ <- dplyr::filter(tab_, .data$Source %in% .sources)
  if (!is.null(.by))      tab_ <- dplyr::filter(tab_, !is.na(.data[[.by]]))

  tab_ <- txt_window(.tab = tab_, .year_min = .year_min, .year_max = .year_max)

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


#' Does a term step at a date, or does it sit there all along
#'
#' THE DIAGNOSTIC THAT DECIDES WHICH TERMS BELONG IN THE PANDEMIC FAMILY, and it exists because the
#' first corpus pass produced a result that could not be right: `epidemic` was the largest term in
#' that family, ahead of `covid`. Reading the stored context said why -- "war, terrorism, civil
#' commotion, labor strike or lock-out, epidemic" is a force-majeure enumeration that predates
#' COVID-19 by decades. One example is a suspicion. A rate either side of a date is evidence.
#'
#' AN EVENT TERM STEPS AND A BOILERPLATE TERM DOES NOT. That is the whole test. `covid` cannot appear
#' before 2020 and must appear after, so its ratio is unbounded; `counterparts` is in most agreements
#' of every year, so its ratio is one. A term placed in the pandemic family whose ratio is near one is
#' not measuring the pandemic, whatever it appears to mean.
#'
#' THE PRE-PERIOD RATE MATTERS ON ITS OWN, INDEPENDENTLY OF THE RATIO. A term can step convincingly
#' and still carry a large pre-period rate, and that rate is contamination: it is the part of the
#' family's baseline that has nothing to do with the event. The ratio says whether the term responds
#' to the break; the pre-period rate says how much it costs to include it.
#'
#' THE BREAK IS 2020 BECAUSE THE QUESTION IS ABOUT THE PANDEMIC FAMILY. The other families are
#' reported at the same break for reference, and two of them should be read differently: boilerplate
#' must be flat at ANY break, which is what validates the diagnostic itself, while rate reform steps
#' here for a reason of its own -- the LIBOR cessation happens to fall in the same window.
#'
#' PREVALENCE AND NOT COUNTS. There are more documents after 2020 than in any single year before it,
#' so a raw count would step for every term in the list.
#'
#' @param .con Connection.
#' @param .term_hash Fingerprint of the term list.
#' @param .terms Output of txt_terms().
#' @param .keys Output of txt_keys(), supplying the filing year.
#' @param .source Character. Which source to measure on; contracts are what the claim is about.
#' @param .break Integer. First year of the post period.
#' @param .year_min Integer or NULL. First calendar year considered.
#' @param .year_max Integer or NULL. Last calendar year considered.
#' @param .flat Numeric. At or below this ratio a term is called flat.
#' @param .step Numeric. At or above this ratio a term is called stepped.
#' @return Tibble: Family, Term, nPre, RatePre, nPost, RatePost, Ratio, Verdict.
txt_term_break <- function(.con, .term_hash, .terms, .keys, .source = "Exhibit10", .break = 2020L,
                           .year_min = NULL, .year_max = NULL, .flat = 1.5, .step = 3) {
  if (FALSE) {
    .con       <- con
    .term_hash <- term_hash
    .terms     <- tab_terms
    .keys      <- tab_keys
    .source    <- "Exhibit10"
    .break     <- 2020L
    .year_min  <- 2001L
    .year_max  <- NULL
    .flat      <- 1.5
    .step      <- 3
  }

  # THE DENOMINATOR COMES FROM THE LEDGER AND NOT FROM THE KEYS. The keys table is the register's
  # view of what exists; the ledger is what was actually scanned. They agree after a complete pass
  # and diverge after a capped one, and a rate computed against documents nobody read is wrong in a
  # way no other column would reveal.
  seen_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT DocID FROM ledger WHERE TermHash = '{.term_hash}' AND Source = '{.source}'"
  )) |>
    tibble::as_tibble() |>
    dplyr::left_join(dplyr::select(.keys, "DocID", "Year"), by = dplyr::join_by(DocID)) |>
    txt_window(.year_min = .year_min, .year_max = .year_max) |>
    dplyr::mutate(Era = dplyr::if_else(.data$Year < .break, "Pre", "Post"))

  denom_ <- dplyr::count(seen_, .data$Era, name = "nDocs")

  hits_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT DISTINCT DocID, Term FROM hits
      WHERE TermHash = '{.term_hash}' AND Source = '{.source}'"
  )) |>
    tibble::as_tibble() |>
    dplyr::inner_join(dplyr::select(seen_, "DocID", "Era"), by = dplyr::join_by(DocID)) |>
    dplyr::count(.data$Term, .data$Era, name = "n")

  # EVERY TERM GETS BOTH ERAS, including the ones that appear in neither. A term absent from one side
  # is the most informative case in the table and would otherwise be the one row missing from it.
  grid_ <- tidyr::expand_grid(
    Term = .terms$Term,
    Era  = c("Pre", "Post")
  )

  grid_ |>
    dplyr::left_join(hits_, by = dplyr::join_by(Term, Era)) |>
    dplyr::left_join(denom_, by = dplyr::join_by(Era)) |>
    dplyr::mutate(
      n    = as.integer(dplyr::coalesce(.data$n, 0L)),
      Rate = .data$n / pmax(.data$nDocs, 1L)
    ) |>
    dplyr::select("Term", "Era", "n", "Rate") |>
    tidyr::pivot_wider(names_from = "Era", values_from = c("n", "Rate")) |>
    dplyr::rename(nPre = "n_Pre", nPost = "n_Post", RatePre = "Rate_Pre", RatePost = "Rate_Post") |>
    dplyr::left_join(dplyr::select(.terms, "Term", "Family"), by = dplyr::join_by(Term)) |>
    dplyr::mutate(
      Ratio   = dplyr::if_else(.data$RatePre > 0, .data$RatePost / .data$RatePre, NA_real_),
      Verdict = dplyr::case_when(
        .data$nPost == 0L               ~ "silent",
        .data$RatePre == 0              ~ "new after break",
        .data$Ratio  >= .step           ~ "steps",
        .data$Ratio  <= .flat           ~ "flat",
        .default                        = "mixed"
      )
    ) |>
    dplyr::select("Family", "Term", "nPre", "RatePre", "nPost", "RatePost", "Ratio", "Verdict") |>
    dplyr::arrange(.data$Family, dplyr::desc(.data$Ratio))
}


#' Report the break diagnostic, and name the terms that do not belong where they are
#'
#' THE VERDICT IS PRINTED, NOT LEFT TO BE READ OFF THE RATIO. A pandemic term that is flat across 2020
#' is in the wrong family, and the point of this table is to say so in words rather than to leave a
#' number for somebody to interpret later.
#'
#' @param .tab Output of txt_term_break().
#' @param .family Character. The family whose membership is under examination.
#' @return .tab invisibly.
txt_report_break <- function(.tab, .family = "Pandemic") {
  if (FALSE) {
    .tab    <- tab_break
    .family <- "Pandemic"
  }

  tbl_head("Does each term step at the break")
  .tab |>
    dplyr::mutate(
      Family = as.character(.data$Family),
      Ratio  = round(.data$Ratio, 1)
    ) |>
    tbl_out(
      .title = "Prevalence before and after the break, by term",
      .pct   = c("RatePre", "RatePost"),
      .notes = c(
        RatePre  = "Share of contracts before the break carrying the term. This is the contamination.",
        Ratio    = "RatePost over RatePre. Missing where the term never appeared before the break.",
        Verdict  = "An event term steps; a boilerplate term does not, whatever the term appears to mean."
      )
    )

  bad_ <- .tab |>
    dplyr::filter(.data$Family == .family, .data$Verdict %in% c("flat", "mixed")) |>
    dplyr::arrange(dplyr::desc(.data$RatePre))

  if (nrow(bad_) > 0L) {
    cli::cli_alert_warning(
      "{(.family)}: {cli::qty(nrow(bad_))}{nrow(bad_)} term{?s} do not step at the break: \\
       {(bad_$Term)}. These carry the family's pre-period baseline without measuring the event."
    )
    cli::cli_alert_info(
      "Regrouping is a re-render rather than a corpus pass, because hits are stored per term."
    )
  } else {
    cli::cli_alert_success("Every term in {(.family)} steps at the break.")
  }

  flat_ <- dplyr::filter(.tab, .data$Family == "Boilerplate")
  if (nrow(flat_) > 0L && all(flat_$Verdict %in% c("flat", "mixed"))) {
    cli::cli_alert_success(
      "Boilerplate is flat at this break, which is what makes a step elsewhere readable."
    )
  } else if (nrow(flat_) > 0L) {
    cli::cli_alert_danger(
      "Boilerplate steps at this break. The corpus moved, so no step on this page can be read as \\
       language."
    )
  }
  invisible(.tab)
}


#' Where in a document a family's hits sit
#'
#' THE YEAR TRIM IS APPLIED IN R AND NOT IN SQL, because the hits table carries no date -- a hit
#' knows the document it came from and the document knows when it was filed. Bringing DocID back and
#' joining is what makes the trim possible at all, and it is also what keeps this figure on the same
#' window as every other one rather than quietly spanning a different one.
#'
#' @param .con Connection.
#' @param .term_hash Fingerprint of the term list.
#' @param .terms Output of txt_terms().
#' @param .keys Output of txt_keys(), supplying the filing year.
#' @param .bins Integer. Bins across the relative position.
#' @param .year_min Integer or NULL. First calendar year to report; see txt_window().
#' @param .year_max Integer or NULL. Last calendar year to report.
#' @return Tibble: Family, Bin, N, Share.
txt_position <- function(.con, .term_hash, .terms, .keys, .bins = 20L, .year_min = NULL,
                         .year_max = NULL, .periods = NULL) {
  if (FALSE) {
    .con       <- con
    .term_hash <- term_hash
    .terms     <- tab_terms
    .keys      <- tab_keys
    .bins      <- 20L
    .year_min  <- 2001L
    .year_max  <- NULL
    .periods   <- list(Shock = 2020L, Later = 2022:2024)
  }

  DBI::dbGetQuery(.con, glue::glue(
    "SELECT DocID, Term, RelPos FROM hits
      WHERE TermHash = '{.term_hash}' AND Source = 'Exhibit10'"
  )) |>
    tibble::as_tibble() |>
    dplyr::left_join(dplyr::select(.keys, "DocID", "Year"), by = dplyr::join_by(DocID)) |>
    txt_window(.year_min = .year_min, .year_max = .year_max) |>
    dplyr::left_join(dplyr::select(.terms, "Term", "Family"), by = dplyr::join_by(Term)) |>
    dplyr::mutate(Bin = pmin(floor(.data$RelPos * .bins) / .bins, (.bins - 1L) / .bins)) |>
    txt_period(.periods = .periods) |>
    dplyr::summarise(N = dplyr::n(), .by = c("Family", "Period", "Bin")) |>
    dplyr::mutate(Share = .data$N / sum(.data$N), .by = c("Family", "Period")) |>
    dplyr::arrange(.data$Family, .data$Period, .data$Bin)
}


#' Label rows by a named span of years
#'
#' A SECOND ROUTE TO THE TEMPLATING QUESTION, AND AN INDEPENDENT ONE. If later mentions concentrate at
#' whatever relative position the definitions article occupies while earlier ones are spread through
#' the body, the language moved into a template. Position and intensity are measured from different
#' columns, so agreement between them is evidence rather than one fact reported twice.
#'
#' NULL COLLAPSES TO ONE PERIOD so the caller that does not care about time gets the old behaviour.
#'
#' @param .tab Tibble carrying Year.
#' @param .periods Named list of integer year vectors, or NULL.
#' @return .tab with a Period factor added; rows outside every named span are dropped.
txt_period <- function(.tab, .periods = NULL) {
  if (FALSE) {
    .tab     <- tab_
    .periods <- list(Shock = 2020L, Later = 2022:2024)
  }

  if (is.null(.periods)) return(dplyr::mutate(.tab, Period = factor("All")))

  map_ <- purrr::imap(.periods, \(.y, .n) tibble::tibble(Year = as.integer(.y), Period = .n)) |>
    purrr::list_rbind()

  .tab |>
    dplyr::inner_join(map_, by = dplyr::join_by(Year)) |>
    dplyr::mutate(Period = factor(.data$Period, levels = names(.periods)))
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
#' A CAPPED PASS WRITES FILES NAMED _partial, AND THAT NAME IS THE WHOLE POINT. A rehearsal over a
#' hundred thousand documents produces a term_docs that looks exactly like the real one -- same
#' columns, same types, plausible prevalences -- and joins to anything without complaint. Nothing
#' downstream could tell that its denominator was a twentieth of the corpus. 03F takes the same
#' precaution with its label release for the same reason, and ent_release_path() refuses a partial
#' file rather than letting one be read by accident.
#'
#' COVERAGE IS MEASURED, NOT INFERRED FROM THE LIMIT ARGUMENT. The queue may be short because a pass
#' was capped, or because it was interrupted, or because a source was excluded -- and all three are
#' the same fact about the file being written. Comparing the ledger against the corpus catches every
#' one of them without being told which happened.
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

  n_corpus_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS N FROM corpus")$N[[1L]]
  n_seen_   <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT COUNT(*) AS N FROM ledger WHERE TermHash = '{.term_hash}'"
  ))$N[[1L]]

  partial_ <- n_seen_ < n_corpus_
  if (partial_) {
    .paths <- purrr::map(.paths, function(.p) {
      fs::path(
        fs::path_dir(.p),
        paste0(fs::path_ext_remove(fs::path_file(.p)), "_partial.", fs::path_ext(.p))
      )
    })
    cli::cli_alert_warning(
      "{txt_n(n_seen_)} of {txt_n(n_corpus_)} documents scanned \\
       ({tbl_pct(n_seen_ / n_corpus_)}), so this release is written as _partial. It is a sample and \\
       must not be read as the corpus."
    )
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
  tbl_out(
    .tab   = out_,
    .title = if (partial_) "Released files (PARTIAL -- a sample, not the corpus)" else "Released files",
    .notes = c(Rows = "term_docs carries one row per scanned document per family, so five per document.")
  )
  invisible(out_)
}


# 10. Plots ---------------------------------------------------------------------------------------------------------------

#' Who filed the registration statements
#'
#' THE TEST THAT SEPARATES A BLANK CHEQUE WAVE FROM CAPITAL MARKETS ACTIVITY IN GENERAL. 6770 is the
#' SEC's code for shells that exist to raise cash and merge, so a rise concentrated there is the SPAC
#' reading measured rather than inferred from the calendar. A rise spread across operating industries
#' is a listing boom and should be described as one.
#'
#' READ AGAINST "OTHER FINANCE" AND NOT AGAINST EVERYTHING. Blank cheque companies are financial
#' filers, so a rise in 6770 that came with an equal rise in banks and funds would be finance in
#' general rather than shells in particular.
#'
#' @param .keys Output of txt_keys().
#' @param .forms Output of txt_form_types().
#' @param .sic Output of txt_filer_sic().
#' @param .quarters Numeric vector. Year-quarters to report.
#' @param .form_group Character or NULL. Restrict to one filing form.
#' @return Tibble: YQ, SICGroup, nDocs, Share.
txt_table_sic <- function(.keys, .forms, .sic, .quarters, .form_group = "Registration") {
  if (FALSE) {
    .keys       <- tab_keys
    .forms      <- tab_forms
    .sic        <- tab_sic
    .quarters   <- c(2019.1, 2020.1, 2021.1)
    .form_group <- "Registration"
  }

  tab_ <- .keys |>
    dplyr::filter(.data$Source == "Exhibit10", .data$YQ %in% .quarters) |>
    dplyr::inner_join(.forms, by = dplyr::join_by(DocID)) |>
    dplyr::inner_join(.sic, by = dplyr::join_by(DocID))

  if (!is.null(.form_group)) tab_ <- dplyr::filter(tab_, .data$FormGroup == .form_group)

  tab_ |>
    dplyr::summarise(nDocs = dplyr::n(), .by = c("YQ", "SICGroup")) |>
    dplyr::mutate(Share = .data$nDocs / sum(.data$nDocs), .by = "YQ") |>
    dplyr::arrange(.data$YQ, .data$SICGroup)
}


#' How much a mentioning contract says, not just whether it mentions
#'
#' THE INTENSIVE MARGIN, AND THE ONLY THING THAT CAN EXPLAIN WHY THE LINE NEVER COMES BACK DOWN.
#' Prevalence peaks at 11.8% in 2020Q2 and settles near 6% through 2024, still tens of times the
#' pre-2020 level. Two very different stories produce that shape. Under TEMPLATING, drafters added a
#' pandemic carve-out to the material-adverse-effect definition and never removed it, so every
#' contract now says COVID once, in the same place, forever. Under SUBSTANTIVE CONTRACTING, agreements
#' are still being written around pandemic conditions.
#'
#' THE TEST IS CONDITIONAL AND THAT IS THE POINT. Mentions per mentioning document, not per document:
#' the unconditional mean would fall simply because fewer contracts mention anything, which is the
#' extensive margin again wearing a different coat. Under templating this conditional figure collapses
#' toward one while prevalence stays high; under substantive contracting it does not.
#'
#' @param .tab_doc Output of txt_doc_table().
#' @param .keys Output of txt_keys().
#' @param .family Character. Which family.
#' @param .year_min Integer or NULL.
#' @param .year_max Integer or NULL.
#' @return Tibble: YQ, nDocs, nWith, Share, MeanHits, MedianHits, MeanPerKWords.
txt_table_intensity <- function(.tab_doc, .keys, .family = "Pandemic", .year_min = NULL,
                                .year_max = NULL) {
  if (FALSE) {
    .tab_doc  <- tab_doc
    .keys     <- tab_keys
    .family   <- "Pandemic"
    .year_min <- 2019L
    .year_max <- NULL
  }

  .tab_doc |>
    dplyr::filter(.data$Family == .family, .data$Source == "Exhibit10") |>
    dplyr::left_join(dplyr::select(.keys, "DocID", "YQ", "Year"), by = dplyr::join_by(DocID)) |>
    txt_window(.year_min = .year_min, .year_max = .year_max) |>
    dplyr::filter(!is.na(.data$YQ)) |>
    dplyr::summarise(
      nDocs         = dplyr::n(),
      nWith         = sum(.data$HasTerm),
      MeanHits      = mean(.data$nHits[.data$HasTerm == 1L]),
      MedianHits    = stats::median(.data$nHits[.data$HasTerm == 1L]),
      MeanPerKWords = mean(.data$PerKWords[.data$HasTerm == 1L], na.rm = TRUE),
      .by           = "YQ"
    ) |>
    dplyr::mutate(Share = .data$nWith / pmax(.data$nDocs, 1L)) |>
    dplyr::select("YQ", "nDocs", "nWith", "Share", "MeanHits", "MedianHits", "MeanPerKWords") |>
    dplyr::arrange(.data$YQ)
}


#' Which industries the pandemic contracts came from, by contract category
#'
#' THE CHECK ON A CLAIM THIS PROJECT HAS REPEATED WITHOUT TESTING. R&D leads the category ranking, and
#' everyone including this document has read that as vaccine and therapeutic collaboration. It is an
#' inference from a class label until a filer industry code agrees with it.
#'
#' @param .tab_doc Output of txt_doc_table().
#' @param .keys Output of txt_keys().
#' @param .sic Output of txt_filer_sic().
#' @param .family Character. Which family.
#' @param .years Integer vector. Calendar years to include.
#' @param .classes Character or NULL. Restrict to these contract categories.
#' @return Tibble: Class, SICGroup, nWith, Share.
txt_table_class_sic <- function(.tab_doc, .keys, .sic, .family = "Pandemic", .years = 2020:2021,
                                .classes = NULL) {
  if (FALSE) {
    .tab_doc <- tab_doc
    .keys    <- tab_keys
    .sic     <- tab_sic
    .family  <- "Pandemic"
    .years   <- 2020:2021
    .classes <- NULL
  }

  tab_ <- .tab_doc |>
    dplyr::filter(.data$Family == .family, .data$Source == "Exhibit10", .data$HasTerm == 1L) |>
    dplyr::left_join(dplyr::select(.keys, "DocID", "Class", "Year"), by = dplyr::join_by(DocID)) |>
    dplyr::inner_join(.sic, by = dplyr::join_by(DocID)) |>
    dplyr::filter(.data$Year %in% .years, !is.na(.data$Class))

  if (!is.null(.classes)) tab_ <- dplyr::filter(tab_, .data$Class %in% .classes)

  tab_ |>
    dplyr::summarise(nWith = dplyr::n(), .by = c("Class", "SICGroup")) |>
    dplyr::mutate(Share = .data$nWith / sum(.data$nWith), .by = "Class") |>
    dplyr::arrange(.data$Class, dplyr::desc(.data$nWith))
}


#' Report the composition shift, and name the quarter that moved
#'
#' @param .tab Output of txt_table_composition().
#' @param .focus Numeric. The year-quarter under examination.
#' @param .base Numeric vector. Quarters it is compared against.
#' @return .tab invisibly.
txt_report_composition <- function(.tab, .focus = 2021.1, .base = c(2019.1, 2020.1),
                                  .quarters = NULL) {
  if (FALSE) {
    .tab      <- tab_comp
    .focus    <- 2021.1
    .base     <- c(2019.1, 2020.1)
    .quarters <- NULL
  }

  # A RUN OF QUARTERS RATHER THAN THREE POINTS, where one is asked for. The 2021 decomposition was
  # read off a single quarter against the same quarter in two prior years, which is the right shape
  # for one spike and the wrong shape for asking whether 2020 moved as well.
  keep_ <- if (is.null(.quarters)) c(.base, .focus) else .quarters

  show_ <- .tab |>
    dplyr::filter(.data$YQ %in% keep_) |>
    dplyr::select("YQ", "FormGroup", "nDocs", "Share") |>
    tidyr::pivot_wider(names_from = "FormGroup", values_from = c("nDocs", "Share"),
                       values_fill = 0)

  tbl_head("What kind of filing the contracts arrived under")
  tbl_out(
    .tab   = show_,
    .title = "Contract attachments by filing form, the spike quarter against the same quarter before",
    .pct   = grep("^Share_", names(show_), value = TRUE),
    .notes = c(
      Share_Registration = "S-1, S-4, F-1, F-4. The paper's Figure 2 pools these with 8-K as ad-hoc."
    )
  )

  reg_ <- .tab |>
    dplyr::filter(.data$FormGroup == "Registration", .data$YQ %in% c(.base, .focus))
  if (nrow(reg_) > 0L && .focus %in% reg_$YQ) {
    now_  <- reg_$Share[reg_$YQ == .focus][[1L]]
    then_ <- mean(reg_$Share[reg_$YQ %in% .base], na.rm = TRUE)
    cli::cli_alert_info(
      "Registration statements carry {tbl_pct(now_)} of contract attachments in {(.focus)} against \\
       {tbl_pct(then_)} in the comparison quarters. An 8-K reports an agreement; an S-1 or S-4 \\
       registers a transaction, so this is a change in why the exhibits exist."
    )
  }
  invisible(.tab)
}


#' Report pandemic prevalence by filing form
#'
#' @param .tab Output of txt_table_form_family().
#' @param .label Character. What the quarters under examination are.
#' @return .tab invisibly.
txt_report_form_family <- function(.tab, .label = "2021Q1") {
  if (FALSE) {
    .tab   <- tab_form_fam
    .label <- "2021Q1"
  }

  tbl_head("Does the form that surged carry the language")
  tbl_out(
    .tab   = .tab,
    .title = paste0("Pandemic prevalence by filing form, ", .label),
    .pct   = "Share",
    .notes = c(
      Share = "If the rise were pandemic contracting, the surging form would not carry LESS of it."
    )
  )

  reg_ <- .tab$Share[.tab$FormGroup == "Registration"]
  cur_ <- .tab$Share[.tab$FormGroup == "Current report"]
  if (length(reg_) == 1L && length(cur_) == 1L) {
    if (reg_ < cur_) {
      cli::cli_alert_info(
        "Registration exhibits carry pandemic language less often than 8-K exhibits do \\
         ({tbl_pct(reg_)} against {tbl_pct(cur_)}), which is what a transaction wave looks like \\
         rather than a contracting response."
      )
    } else {
      cli::cli_alert_info(
        "Registration exhibits carry pandemic language at least as often as 8-K exhibits \\
         ({tbl_pct(reg_)} against {tbl_pct(cur_)}), so the composition shift does not by itself \\
         displace the pandemic reading."
      )
    }
  }
  invisible(.tab)
}


#' Report who filed the registration statements
#' @param .tab Output of txt_table_sic().
#' @param .focus Numeric. The quarter under examination.
#' @param .base Numeric vector. Quarters it is read against.
#' @return .tab invisibly.
txt_report_sic <- function(.tab, .focus = 2021.1, .base = c(2019.1, 2020.1)) {
  if (FALSE) {
    .tab   <- tab_sic_reg
    .focus <- 2021.1
    .base  <- c(2019.1, 2020.1)
  }

  show_ <- .tab |>
    dplyr::mutate(YQ = as.character(.data$YQ)) |>
    dplyr::select("YQ", "SICGroup", "nDocs", "Share") |>
    tidyr::pivot_wider(names_from = "SICGroup", values_from = c("nDocs", "Share"), values_fill = 0)

  tbl_head("Who filed the registration statements")
  tbl_out(
    .tab   = show_,
    .title = "Filer industry behind registration exhibits",
    .pct   = grep("^Share_", names(show_), value = TRUE),
    .notes = c(`Share_Blank check` = "SIC 6770, the SEC's own code for shells that list to merge.")
  )

  bc_ <- dplyr::filter(.tab, .data$SICGroup == "Blank check")
  if (nrow(bc_) > 0L && .focus %in% bc_$YQ) {
    now_  <- bc_$Share[bc_$YQ == .focus][[1L]]
    then_ <- mean(bc_$Share[bc_$YQ %in% .base], na.rm = TRUE)
    if (now_ > 2 * max(then_, 0.005)) {
      cli::cli_alert_info(
        "Blank cheque filers are {tbl_pct(now_)} of registration exhibits in {(.focus)} against \\
         {tbl_pct(then_)} before. The wave can be named rather than described."
      )
    } else {
      cli::cli_alert_warning(
        "Blank cheque filers are {tbl_pct(now_)} against {tbl_pct(then_)} before, which does not \\
         carry the SPAC reading. Say concurrent registration activity and stop there."
      )
    }
  }
  invisible(.tab)
}


#' Report the intensive margin
#' @param .tab Output of txt_table_intensity().
#' @param .peak Numeric. The quarter language peaked in.
#' @param .later Numeric vector. Quarters read against it.
#' @return .tab invisibly.
txt_report_intensity <- function(.tab, .peak = 2020.2, .later = c(2023.4, 2024.1, 2024.2)) {
  if (FALSE) {
    .tab   <- tab_int
    .peak  <- 2020.2
    .later <- c(2023.4, 2024.1, 2024.2)
  }

  tbl_head("How much a mentioning contract says")
  .tab |>
    dplyr::mutate(
      YQ            = as.character(.data$YQ),
      MeanHits      = round(.data$MeanHits, 2),
      MeanPerKWords = round(.data$MeanPerKWords, 3)
    ) |>
    tbl_out(
      .title = "Prevalence and, among mentioning contracts, how often they mention",
      .pct   = "Share",
      .notes = c(
        MeanHits      = "Conditional on mentioning. Falling toward one while Share holds is templating.",
        MeanPerKWords = "Mentions per thousand words, so a long agreement is not credited for length."
      )
    )

  p_ <- .tab$MeanHits[.tab$YQ == .peak]
  l_ <- mean(.tab$MeanHits[.tab$YQ %in% .later], na.rm = TRUE)
  if (length(p_) == 1L && is.finite(l_)) {
    cli::cli_alert_info(
      "A mentioning contract carried {round(p_, 2)} mentions at the peak and {round(l_, 2)} later. \\
       A fall toward one alongside a prevalence that holds is a clause that was added once and left."
    )
  }
  invisible(.tab)
}


#' Report industry behind the pandemic contracts, by category
#' @param .tab Output of txt_table_class_sic().
#' @param .class Character. The category whose reading is being checked.
#' @return .tab invisibly.
txt_report_class_sic <- function(.tab, .class = "R&D") {
  if (FALSE) {
    .tab   <- tab_class_sic
    .class <- "R&D"
  }

  show_ <- .tab |>
    dplyr::mutate(Class = as.character(.data$Class), SICGroup = as.character(.data$SICGroup)) |>
    dplyr::select("Class", "SICGroup", "nWith", "Share") |>
    tidyr::pivot_wider(names_from = "SICGroup", values_from = c("nWith", "Share"), values_fill = 0)

  tbl_head("Which industries the pandemic contracts came from")
  tbl_out(
    .tab   = show_,
    .title = "Filer industry by contract category, pandemic-mentioning contracts",
    .pct   = grep("^Share_", names(show_), value = TRUE)
  )

  ph_ <- .tab |>
    dplyr::filter(.data$Class == .class, .data$SICGroup == "Pharma and biotech")
  if (nrow(ph_) == 1L) {
    if (ph_$Share > 0.4) {
      cli::cli_alert_success(
        "{(.class)} pandemic contracts are {tbl_pct(ph_$Share)} pharmaceutical and biotechnology \\
         filers, so the vaccine and therapeutic reading is supported rather than assumed."
      )
    } else {
      cli::cli_alert_warning(
        "{(.class)} pandemic contracts are only {tbl_pct(ph_$Share)} pharmaceutical and \\
         biotechnology filers. The vaccine reading has been asserted and is not carried by this."
      )
    }
  }
  invisible(.tab)
}


#' The intensive margin against the extensive one
#' @param .tab Output of txt_table_intensity().
#' @return A ggplot.
txt_plot_intensity <- function(.tab) {
  if (FALSE) .tab <- tab_int

  scale_ <- max(.tab$Share, na.rm = TRUE) / max(.tab$MeanHits, na.rm = TRUE)

  ggplot2::ggplot(.tab, ggplot2::aes(x = .data$YQ)) +
    ggplot2::geom_col(ggplot2::aes(y = .data$Share), fill = plot_pal_grey(1L), width = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y = .data$MeanHits * scale_), colour = plot_pal_seq(1L),
                       linewidth = 0.6) +
    ggplot2::scale_y_continuous(
      name     = "Contracts mentioning the family",
      labels   = scales::percent_format(accuracy = 1),
      sec.axis = ggplot2::sec_axis(~ . / scale_, name = "Mentions per mentioning contract")
    ) +
    ggplot2::labs(x = NULL) +
    plot_theme(.grid = "y", .legend = "none")
}


#' Composition of contract filings over time
#'
#' THE VOCABULARY IS NAMED RATHER THAN ASSUMED, because there are two of them. The pooled grouping and
#' the one that opens registration into offerings and mergers are separate registered level sets, and
#' a plot drawn against the wrong one does not mislabel quietly -- plot_factor() refuses an
#' unregistered value, which is how this was caught.
#'
#' @param .tab Output of txt_table_composition().
#' @param .key Character. Registered level set: FormGroup for the pooled form, FormDetail for the
#'   split one. It must match the levels the table was built with.
#' @return A ggplot.
txt_plot_composition <- function(.tab, .key = "FormGroup") {
  if (FALSE) {
    .tab <- tab_comp
    .key <- "FormGroup"
  }

  lv_ <- if (identical(.key, "FormDetail")) .txt_form_detail_levels else .txt_form_levels
  bad_ <- setdiff(as.character(unique(.tab$FormGroup)), lv_)
  if (length(bad_) > 0L) {
    cli::cli_abort(c(
      "x" = "The table carries {cli::qty(bad_)}level{?s} {(bad_)}, which {.key} does not register.",
      "i" = "A table built with .split = TRUE needs {.val FormDetail}."
    ))
  }

  .tab |>
    dplyr::mutate(FormGroup = plot_factor(.x = .data$FormGroup, .key = .key, .short = TRUE)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$YQ, y = .data$Share, fill = .data$FormGroup)) +
    ggplot2::geom_col(width = 0.24) +
    plot_scale_fill_key(.key = .key, .short = TRUE) +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = "Share of contract attachments", fill = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}


#' Each term's prevalence before and after the break
#'
#' A DUMBBELL RATHER THAN A SCATTER, because the quantity of interest is the MOVE and a dumbbell draws
#' the move as a line whose length is the finding. Terms whose two points sit on top of each other are
#' the ones in the wrong family.
#'
#' LOG SCALE, BECAUSE THE RATES SPAN FIVE ORDERS OF MAGNITUDE. Boilerplate sits near a third of all
#' contracts and the rarest pandemic terms near a ten-thousandth, and on a linear axis every term
#' except three would be pressed against zero. A term absent from an era has no point on that side.
#'
#' @param .tab Output of txt_term_break().
#' @param .families Character or NULL. Restrict to these families.
#' @return A ggplot.
txt_plot_break <- function(.tab, .families = NULL) {
  if (FALSE) {
    .tab      <- tab_break
    .families <- c("Pandemic", "Disruption", "Boilerplate")
  }

  dat_ <- if (is.null(.families)) .tab else dplyr::filter(.tab, .data$Family %in% .families)

  long_ <- dat_ |>
    dplyr::select("Family", "Term", "RatePre", "RatePost") |>
    tidyr::pivot_longer(c("RatePre", "RatePost"), names_to = "Era", values_to = "Rate") |>
    dplyr::mutate(
      Era  = factor(dplyr::if_else(.data$Era == "RatePre", "Before", "After"),
                    levels = c("Before", "After")),
      Term = stats::reorder(.data$Term, .data$Rate)
    ) |>
    dplyr::filter(.data$Rate > 0)

  ggplot2::ggplot(long_, ggplot2::aes(x = .data$Rate, y = .data$Term)) +
    ggplot2::geom_line(ggplot2::aes(group = .data$Term), colour = plot_pal_grey(1L),
                       linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$Era), size = 1.8) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$Family), scales = "free_y", space = "free_y") +
    ggplot2::scale_x_log10(labels = scales::percent_format(accuracy = 0.001)) +
    plot_scale_colour_cat() +
    ggplot2::labs(x = "Share of contracts carrying the term", y = NULL, colour = NULL) +
    plot_theme(.grid = "x", .legend = "bottom")
}


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
    ggplot2::facet_grid(rows = ggplot2::vars(.data$Family), cols = ggplot2::vars(.data$Period),
                        scales = "free_y") +
    plot_scale_fill_key(.key = "TermFamily") +
    plot_scale_y_pct() +
    ggplot2::labs(x = "Relative position in the document", y = "Share of the family's hits") +
    plot_theme(.grid = "y", .legend = "none")
}


#' One family across the four document types
#'
#' THE FOUR SOURCES ARE FOUR WITNESSES, NOT FOUR SAMPLES OF ONE THING. A contract is what the
#' parties agreed; an 8-K is what the registrant told the market it had agreed; Item 1.01 is the
#' regulated paragraph of that report; a CT order is the Commission's reply about what may stay
#' hidden. Their prevalences are not comparable in level -- the documents differ in length by more
#' than an order of magnitude, so a short summary mentions fewer of anything -- and they are
#' comparable in shape, which is the whole reason to draw them together.
#'
#' COLOURED BY SOURCE AGAINST THE REGISTERED VOCABULARY, so the same document type is the same
#' colour wherever it appears in this document.
#'
#' @param .tab Output of txt_prevalence() with .sources = NULL, so Source survives.
#' @param .family Character. Which family.
#' @param .drop_silent Logical. TRUE removes a source that never matched the family, so a flat line
#'   at zero does not read as a measurement.
#' @return A ggplot.
txt_plot_sources <- function(.tab, .family = "Pandemic", .drop_silent = FALSE) {
  if (FALSE) {
    .tab         <- tab_prev
    .family      <- "Pandemic"
    .drop_silent <- FALSE
  }

  dat_ <- dplyr::filter(.tab, .data$Family == .family)

  if (.drop_silent) {
    keep_ <- dat_ |>
      dplyr::summarise(Any = sum(.data$nWith) > 0L, .by = "Source") |>
      dplyr::filter(.data$Any)
    dat_ <- dplyr::filter(dat_, .data$Source %in% keep_$Source)
  }

  dat_ |>
    dplyr::mutate(Source = plot_factor(.x = .data$Source, .key = "TextSource")) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$YQ, y = .data$Share, colour = .data$Source)) +
    ggplot2::geom_line(linewidth = 0.5) +
    plot_scale_colour_key(.key = "TextSource") +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = "Documents carrying the family", colour = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
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
