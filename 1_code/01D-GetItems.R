# 01D-GetItems: the 8-K item layer -- what each filing reports, and the Item 1.01 summary ------------------------------
#
# WHAT THIS FILE DOES
# Two things, because they are the same fact read at two depths. Every 8-K declares which items it
# reports; that list is parsed here into one row per filing and item, classified, and published. An
# 8-K reporting Item 1.01 also carries the filer's own summary of the agreement it has just entered
# into; that section is located inside the filing's HTML and its text extracted.
#
# The list is what decides which documents the extraction is attempted on, so the two were never
# separable. Before this file owned both, the parse existed twice -- once here as a literal grepl
# over a pipe-joined string, once in 05B with a real code parser and a different rule for filings
# whose registrants disagree. Two implementations of "which filings report which items" is two
# answers, and the fact that they happened to agree on this corpus is luck rather than design.
#
# It is a property of a filing and of a document, derived by reading them, exactly like the word
# counts in 01C. It is in the 01 family and not downstream of sample selection because it does not
# depend on the sample: every 8-K reporting the item is attempted, so the resulting flag means one
# thing corpus-wide rather than "extracted, or never tried".
#
# THE ITEM LIST COMES FROM 01A, NOT 01C
# 01A's landing table is every filing EDGAR indexed; 01C's is only those carrying a retrieved
# Exhibit 10. The superset costs nothing at four million rows and restricting it later is a join,
# whereas widening it later is a rebuild. Everything downstream that wants the narrow frame gets it
# by joining to the register.
#
# NOTHING IS PRE-FILTERED; EVERYTHING IS RECORDED
# Format is not a filter. A document that is not HTML is attempted and recorded as such, so the share
# is measured rather than assumed, and if plain-text filings turn out to be parseable that is a
# finding rather than a permanent exclusion. Every candidate leaves exactly one row carrying an
# outcome, which is what makes the failure modes countable.
#
# An item code that is not in the vocabulary is recorded rather than dropped, for the same reason.
# One filing in 1996 reports "Item 57", which is a filer error and not a taxonomy: it stays in the
# table, carries no era and no kind, and is reported. The check that would stop a render is windowed
# to the sample period, so a genuinely new code -- Item 1.05 arrived in December 2023 and the next
# one will arrive without warning -- fires, and a stray from 1996 does not.
#
# THE TEXT STAYS HERE
# Roughly a quarter of a million summaries is on the order of a gigabyte. The register in 02 carries
# a flag; the text is one join away. Putting it in the register would make the one table nobody can
# load. The item counts are the opposite case: they are small, they are wanted on every row, and 02
# joins them onto the register by HashIndex.
#
# WHAT IS RECOMPUTED, AND WHAT IS NOT
# Three steps take .rerun, defaulting to FALSE, and each fingerprints its inputs before deciding.
# itm_candidates() scans 01C's consolidated table and 01B's index, half a gigabyte of parquet, to
# answer a question fixed by those two files and the item code. itm_write_items() writes the two
# item tables. itm_write_item101() writes the gigabyte of extracted text, which is a pure function
# of the extraction cache and the candidate set.
#
# THE SUMMARY HEURISTICS ARE DERIVED FROM THE CACHE, NOT COMPUTED IN THE WORKER. Counting the dates
# in a summary is a rule that will be tuned; computing it inside itm_process_doc() would mean a
# re-run of the corpus pass, measured in hours, every time the regular expression changes. The text
# is already in the extraction cache, so the derivation is a cheap pass over a table.
#
# FIGURES ARE DEFINED HERE AND WRITTEN NOWHERE. The document displays what the plot functions return;
# the consolidated release script writes the files the manuscript needs.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation.

if (FALSE) {
  .path_landing <- .lP$Input$LandingAll
  .path_meta    <- .lP$Input$MetaData
  .path         <- tab_candidates$Path[1]
  .item         <- "1.01"
  .rerun        <- FALSE
}


# 0. The item vocabulary -----------------------------------------------------------------------------------------------

#: EVERY 8-K ITEM CODE THAT EXISTS, UNDER BOTH TAXONOMIES. 46 rows: 33 dotted codes from the form as
#: reformed on 23 August 2004, and 13 undotted ones from the form before it. That is the complete
#: legal vocabulary of each, and a corpus-wide count returns exactly these plus one 1996 stray --
#: which is the check that the parser is reading codes rather than inventing them.
#:
#: ItemKind IS THE RESEARCH DECISION IN THIS FILE and the reason it is declared rather than derived.
#: Three kinds, not two:
#:
#:   Voluntary   the registrant chooses whether and when. 2.02 and 7.01 are FURNISHED rather than
#:               filed, which is the Section 18 safe harbour; 8.01 is filed but the rule says the
#:               registrant "may, at its option, disclose". Pre-reform these are 12, 9 and 5.
#:   Exhibits    not a disclosure event at all -- it says the filing carries financial statements or
#:               exhibits. Counting it as either of the others makes every contract-bearing 8-K look
#:               one item busier than it is.
#:   Mandatory   triggered by an event outside the registrant's control, four business days.
#:
#: 2.02 IS THE CONTESTED ONE and the dictionary says so. It is required ONCE the registrant announces
#: results, but the announcement is discretionary, which is why the bundling literature counts it as
#: voluntary. ItemFurnished separates the legal cut from the discretion cut, so a robustness column
#: that drops 8.01 or keeps only the furnished pair costs a filter rather than a re-read.
#:
#: ItemSuccessor IS THE PRE-TO-POST CROSSWALK and it is what makes a pre/post count comparable at all.
#: Four matter for the voluntary measure: 5 -> 8.01, 9 -> 7.01, 12 -> 2.02, 7 -> 9.01. It is NA on
#: post-reform rows, and NA on Item 13, which the reform did not carry forward.
#:
#: CODE 9 MEANT SOMETHING ELSE BEFORE REGULATION FD. It first appears in the corpus in 1996, four
#: years before Regulation FD existed, when Item 9 was "Sales of Equity Securities Pursuant to
#: Regulation S" and mandatory. EDGAR has back-labelled every occurrence with the current title, so
#: the label cannot be used to tell them apart. The sample begins in 2001 and Regulation FD was
#: adopted in October 2000, so nothing in frame is affected -- but the row is Voluntary because of
#: what the code meant from 2000, not throughout.
#:
#: ItemLabelShort IS FOR TABLES, NOT FOR DATA. EDGAR's own label is deterministic: across 2.3 million
#: filings not one code carries two spellings, which itm_label_variants() checks. That string is what
#: the published table holds, because a reader should see what EDGAR says. These are the shortened
#: forms, used where a column has to fit.
#:
#: NO EFFECTIVE DATES ARE DECLARED. Several codes postdate their own era: 9 arrives with Regulation
#: FD in 2000, 10, 11, 12 and 13 in 2003, 6.01 and 6.02 with Regulation AB, 5.07 in 2010, 5.08 and
#: 1.04 in 2011, 1.05 in December 2023. Hand-entering 46 dates is a second thing to keep right; the
#: runbook reports first-seen and last-seen year per code from the corpus instead, which validates
#: the era and surfaces the mid-era arrivals without anyone having to remember them.
.itm_items <- tibble::tribble(
  ~ItemCode, ~ItemEra, ~ItemKind,   ~ItemFurnished, ~ItemSuccessor, ~ItemLabelShort,

  # -- Post-reform: Section 1, the registrant's business and operations ---------------------------
  "1.01",    "Post",   "Mandatory", 0L,             NA_character_,  "Entry into a Material Definitive Agreement",
  "1.02",    "Post",   "Mandatory", 0L,             NA_character_,  "Termination of a Material Definitive Agreement",
  "1.03",    "Post",   "Mandatory", 0L,             NA_character_,  "Bankruptcy or Receivership",
  "1.04",    "Post",   "Mandatory", 0L,             NA_character_,  "Mine Safety - Shutdowns and Patterns of Violations",
  "1.05",    "Post",   "Mandatory", 0L,             NA_character_,  "Material Cybersecurity Incidents",

  # -- Post-reform: Section 2, financial information ----------------------------------------------
  "2.01",    "Post",   "Mandatory", 0L,             NA_character_,  "Completion of Acquisition or Disposition",
  "2.02",    "Post",   "Voluntary", 1L,             NA_character_,  "Results of Operations and Financial Condition",
  "2.03",    "Post",   "Mandatory", 0L,             NA_character_,  "Creation of a Direct Financial Obligation",
  "2.04",    "Post",   "Mandatory", 0L,             NA_character_,  "Triggering Events That Accelerate an Obligation",
  "2.05",    "Post",   "Mandatory", 0L,             NA_character_,  "Costs Associated with Exit or Disposal Activities",
  "2.06",    "Post",   "Mandatory", 0L,             NA_character_,  "Material Impairments",

  # -- Post-reform: Section 3, securities and trading markets -------------------------------------
  "3.01",    "Post",   "Mandatory", 0L,             NA_character_,  "Notice of Delisting or Listing Failure",
  "3.02",    "Post",   "Mandatory", 0L,             NA_character_,  "Unregistered Sales of Equity Securities",
  "3.03",    "Post",   "Mandatory", 0L,             NA_character_,  "Material Modifications to Rights of Security Holders",

  # -- Post-reform: Section 4, accountants and financial statements -------------------------------
  "4.01",    "Post",   "Mandatory", 0L,             NA_character_,  "Changes in Registrant's Certifying Accountant",
  "4.02",    "Post",   "Mandatory", 0L,             NA_character_,  "Non-Reliance on Issued Financial Statements",

  # -- Post-reform: Section 5, corporate governance and management --------------------------------
  "5.01",    "Post",   "Mandatory", 0L,             NA_character_,  "Changes in Control of Registrant",
  "5.02",    "Post",   "Mandatory", 0L,             NA_character_,  "Departure or Election of Directors or Officers",
  "5.03",    "Post",   "Mandatory", 0L,             NA_character_,  "Amendments to Articles; Change in Fiscal Year",
  "5.04",    "Post",   "Mandatory", 0L,             NA_character_,  "Temporary Suspension of Trading Under Benefit Plans",
  "5.05",    "Post",   "Mandatory", 0L,             NA_character_,  "Amendments to the Code of Ethics, or Waiver of It",
  "5.06",    "Post",   "Mandatory", 0L,             NA_character_,  "Change in Shell Company Status",
  "5.07",    "Post",   "Mandatory", 0L,             NA_character_,  "Submission of Matters to a Vote of Security Holders",
  "5.08",    "Post",   "Mandatory", 0L,             NA_character_,  "Shareholder Director Nominations",

  # -- Post-reform: Section 6, asset-backed securities ---------------------------------------------
  "6.01",    "Post",   "Mandatory", 0L,             NA_character_,  "ABS Informational and Computational Material",
  "6.02",    "Post",   "Mandatory", 0L,             NA_character_,  "Change of Servicer or Trustee",
  "6.03",    "Post",   "Mandatory", 0L,             NA_character_,  "Change in Credit Enhancement or External Support",
  "6.04",    "Post",   "Mandatory", 0L,             NA_character_,  "Failure to Make a Required Distribution",
  "6.05",    "Post",   "Mandatory", 0L,             NA_character_,  "Securities Act Updating Disclosure",
  "6.06",    "Post",   "Mandatory", 0L,             NA_character_,  "Static Pool",

  # -- Post-reform: Sections 7, 8 and 9, the three that are not event-triggered --------------------
  "7.01",    "Post",   "Voluntary", 1L,             NA_character_,  "Regulation FD Disclosure",
  "8.01",    "Post",   "Voluntary", 0L,             NA_character_,  "Other Events",
  "9.01",    "Post",   "Exhibits",  0L,             NA_character_,  "Financial Statements and Exhibits",

  # -- Pre-reform: the form as it stood before 23 August 2004 --------------------------------------
  "1",       "Pre",    "Mandatory", 0L,             "5.01",         "Changes in Control of Registrant",
  "2",       "Pre",    "Mandatory", 0L,             "2.01",         "Acquisition or Disposition of Assets",
  "3",       "Pre",    "Mandatory", 0L,             "1.03",         "Bankruptcy or Receivership",
  "4",       "Pre",    "Mandatory", 0L,             "4.01",         "Changes in Registrant's Certifying Accountant",
  "5",       "Pre",    "Voluntary", 0L,             "8.01",         "Other Events",
  "6",       "Pre",    "Mandatory", 0L,             "5.02",         "Resignations of Registrant's Directors",
  "7",       "Pre",    "Exhibits",  0L,             "9.01",         "Financial Statements and Exhibits",
  "8",       "Pre",    "Mandatory", 0L,             "5.03",         "Change in Fiscal Year",
  "9",       "Pre",    "Voluntary", 1L,             "7.01",         "Regulation FD Disclosure",
  "10",      "Pre",    "Mandatory", 0L,             "5.05",         "Amendments to the Registrant's Code of Ethics",
  "11",      "Pre",    "Mandatory", 0L,             "5.04",         "Temporary Suspension of Trading Under Benefit Plans",
  "12",      "Pre",    "Voluntary", 1L,             "2.02",         "Results of Operations and Financial Condition",
  "13",      "Pre",    "Mandatory", 0L,             NA_character_,  "Receipt of an Attorney's Written Notice"
)

#: THE THREE KINDS AND THE TWO ERAS, REGISTERED SO A TYPO IN THE TABLE ABOVE CANNOT PASS.
.itm_item_kinds <- c("Voluntary", "Mandatory", "Exhibits")
.itm_item_eras  <- c("Pre", "Post")

#: THE DATE THAT SEPARATES THEM. 2004 belongs to neither side by year: a filing from that year is
#: only interpretable once you know which side of 23 August it fell on, which is why the era check
#: compares the code's era against the filing date rather than against the year.
.itm_reform_date <- as.Date("2004-08-23")

#: THE YEARS THE UNREGISTERED-CODE CHECK IS ALLOWED TO FAIL IN. The corpus runs from 1993; the sample
#: runs from 2001. A stray code outside the sample cannot affect a number in the paper, and stopping
#: a render for one 1996 filing trains everyone to ignore the check that is meant to catch the next
#: real item. The full-frame report is not windowed, so nothing is hidden by this.
.itm_window <- c(2001L, 2024L)

#: THE FIVE CODES THE WIDE TABLE CARRIES AS INDICATORS, and the pre-reform code each corresponds to.
#: 1.01 because it is what this script extracts; 2.02, 7.01 and 8.01 because they are the voluntary
#: set the bundling literature counts; 9.01 because a contract-bearing 8-K that does not report the
#: exhibits item is a red flag and counting them is how you find that out.
.itm_flag_codes <- tibble::tribble(
  ~Flag,            ~CodePost, ~CodePre,
  "ReportsItem101", "1.01",    NA_character_,
  "ReportsItem202", "2.02",    "12",
  "ReportsItem701", "7.01",    "9",
  "ReportsItem801", "8.01",    "5",
  "ReportsItem901", "9.01",    "7"
)

#: HOW A CODE IS READ OUT OF A FILING'S ITEM STRING. Anchored at the head, so the label is whatever
#: follows and no colon is ever split on. Splitting on colons is what turned Item 5.02 -- whose
#: regulated title carries two more of them -- into four items. The separator after the code is
#: optional because filers punctuate it every way there is.
.itm_code_regex  <- "^\\s*Item\\s+([0-9]+(?:\\.[0-9]+)?)"
.itm_strip_regex <- "^\\s*Item\\s+[0-9]+(?:\\.[0-9]+)?\\s*[:.,-]?\\s*"

#: THE HEADING AND THE REGULATED TITLE, WHICH ARE NOT PART OF THE SUMMARY. Together they run to
#: about forty characters that say nothing about the agreement, so a length test that counts them
#: measures the wrong thing and a preview that shows them wastes a third of the line on boilerplate
#: every summary carries. The title is matched loosely -- optional article, optional brackets,
#: optional trailing stop -- because filers punctuate it inconsistently, and a pattern insisting on
#: one spelling would subtract nothing from most of them.
.itm_head_regex  <- "(?i)^\\s*item\\s*\\d+\\s*\\.?\\s*\\d*\\s*[.:)-]*\\s*"
.itm_title_regex <- paste0(
  "(?i)^\\s*[(\\[]?\\s*entry\\s+into\\s+(?:an?\\s+)?material\\s+definitive\\s+agreement",
  "\\s*[)\\]]?\\s*[.;:]*\\s*"
)

#: HOW A CONTRACT DATE IS READ OUT OF AN ITEM 1.01 SUMMARY. The regulated narrative opens "On March
#: 3, 2015, the Company entered into", and the count of DISTINCT such openings is the proxy for how
#: many agreements the summary describes. It is a heuristic and it is stored as a count rather than
#: applied as a filter: the published paper restricted to summaries with exactly one, and that
#: restriction belongs to the analysis that wants it, not to the extraction.
.itm_date_regex <- paste0(
  "\\bOn\\s+(", paste(month.name, collapse = "|"), ")\\s+(\\d{1,2}),\\s*(\\d{4})"
)


# 1. Filing items ------------------------------------------------------------------------------------------------------

#' Read the item lists off a landing table
#'
#' THE ONLY FUNCTION HERE THAT TOUCHES THE LANDING FILE, and it is four lines so that everything
#' downstream can be tested on a tibble built by hand.
#'
#' THE POPULATION IS THE WHOLE POINT. 01A's LandingPageAll is every filing EDGAR indexed; 01C's
#' LandingPage is only those carrying a retrieved Exhibit 10. They differ by an order of magnitude
#' and only one of them is the frame for a given question, so the path is an argument rather than a
#' constant, and the row count is reported rather than assumed.
#'
#' @param .path Path to a landing table carrying HashIndex, Items and a filing date.
#' @param .col_date Character. Date column; dropped if the schema does not carry it.
#' @return A tibble, ONE ROW PER FILING AND REGISTRANT: HashIndex, Items and the date where present.
itm_read_landing <- function(.path, .col_date = "FilingDate") {
  if (FALSE) {
    .path     <- .lP$Input$LandingAll
    .col_date <- "FilingDate"
  }

  have_ <- names(arrow::open_dataset(sources = .path))
  cols_ <- c("HashIndex", "Items", if (!is.null(.col_date) && .col_date %in% have_) .col_date)

  arrow::open_dataset(sources = .path) |>
    dplyr::select(dplyr::all_of(cols_)) |>
    dplyr::filter(!is.na(.data$Items)) |>
    dplyr::collect()
}

#' One row per filing, where its registrants agree about the item list
#'
#' THE LANDING TABLE IS KEYED ON FILING AND REGISTRANT, NOT ON FILING. A filing co-filed by
#' forty-one affiliated entities appears forty-one times carrying the same item list, and unnesting
#' before collapsing multiplies every item count by the number of registrants. On this corpus that
#' step removes 279,627 rows, twelve per cent, and skipping it reports a maximum of 205 items on a
#' filing where the true maximum is 13.
#'
#' WHERE REGISTRANTS DISAGREE THE FILING IS DROPPED rather than one version being chosen, because
#' there is no basis for choosing and the alternative is a silent coin flip on the variable that
#' decides what gets extracted. On this corpus the rule fires zero times -- EDGAR generates the item
#' list once per accession and stamps it on every registrant row, which is the same reason no code
#' carries two label spellings. It is kept because a guard whose firing count is reported costs
#' nothing and a guard that was never written costs a silent error.
#'
#' @param .tab Output of itm_read_landing().
#' @return A tibble with one row per HashIndex, and a Dropped attribute recording both losses.
itm_dedup_landing <- function(.tab) {
  if (FALSE) .tab <- tab_landing

  uniq_ <- dplyr::distinct(.tab, .data$HashIndex, .data$Items, .keep_all = TRUE)
  keep_ <- dplyr::filter(uniq_, dplyr::n() == 1L, .by = "HashIndex")

  attr(keep_, "Dropped") <- c(
    RegistrantRows = nrow(.tab) - nrow(uniq_),
    Disagreeing    = nrow(uniq_) - nrow(keep_)
  )

  keep_
}

#' The run structure of a column whose equal values are contiguous
#'
#' THE TABLES ARE SORTED, AND THAT TURNS EVERY GROUPED AGGREGATE INTO ARITHMETIC. dplyr's summarise
#' over two million groups evaluates each expression once per group: measured on this corpus it is
#' three and a half minutes, and no reformulation of the expressions moves it, because the per-group
#' dispatch is the cost rather than the work. On a column already sorted, a group is a run, and the
#' sum over each run is one difference of a cumulative sum -- eight seconds for the same answer.
#'
#' THE CONTIGUITY IS ASSERTED, NOT ASSUMED. If a caller hands over an unsorted table the runs are
#' wrong and every count is silently wrong with them, so the one condition the arithmetic depends on
#' is checked rather than documented. rle() has already computed what the check needs.
#'
#' @param .x A vector whose equal values are contiguous.
#' @return A list: Values, Lengths, Ends.
itm_runs <- function(.x) {
  if (FALSE) .x <- tab_long$HashIndex

  r_ <- rle(.x)
  if (anyDuplicated(r_$values)) {
    cli::cli_abort("Values are not contiguous: {sum(duplicated(r_$values))} appear in more than one run.")
  }

  ends_ <- cumsum(r_$lengths)
  list(Values = r_$values, Lengths = r_$lengths, Ends = ends_, Starts = ends_ - r_$lengths + 1L)
}

#' Sum a column within each run
#'
#' @param .x Logical or numeric, the same length as the column the runs came from.
#' @param .runs Output of itm_runs().
#' @return Integer, one per run.
itm_run_sum <- function(.x, .runs) {
  if (FALSE) .runs <- itm_runs(tab_long$HashIndex)

  diff(c(0L, cumsum(as.integer(.x))[.runs$Ends]))
}

#' Collapse a character column within each run
#'
#' @param .x Character, the same length as the column the runs came from.
#' @param .runs Output of itm_runs().
#' @param .sep Character. Separator.
#' @return Character, one per run.
itm_run_paste <- function(.x, .runs, .sep = "|") {
  if (FALSE) .runs <- itm_runs(tab_long$HashIndex)

  vapply(
    X         = split(.x, rep.int(seq_along(.runs$Lengths), .runs$Lengths)),
    FUN       = paste,
    FUN.VALUE = character(1L),
    collapse  = .sep,
    USE.NAMES = FALSE
  )
}

#' One row per filing and item, classified
#'
#' THE RESULT IS SORTED, AND THAT IS LOAD-BEARING RATHER THAN TIDY. An Arrow scan makes no promise
#' about the order in which record batches are returned -- within one file as much as across many,
#' because row groups are read in parallel -- and neither distinct() nor a grouped filter reorders
#' what it is given. Two runs over an unchanged file therefore return the same rows in a different
#' order. itm_candidates() takes this table into its own fingerprint, so an unsorted result hashes
#' differently on every render and defeats that cache entirely.
#'
#' ItemOrder PRESERVES DOCUMENT ORDER WITHOUT A STRING. Once the list is one row per item, the order
#' the filer declared them in is only recoverable if it is recorded, and it is the one thing a
#' pipe-joined string carried that a long table otherwise loses.
#'
#' THE LABEL IN THE DATA IS EDGAR'S OWN. Not one code carries two spellings across the corpus, so it
#' is deterministic, and a reader comparing this table against the filing should see what the filing
#' index says rather than a shortened form invented here.
#'
#' AN UNREGISTERED CODE KEEPS ITS ROW and carries no era, kind or successor. Dropping it would make
#' the count of them zero by construction, which is the one number that says whether the vocabulary
#' is still complete.
#'
#' @param .tab Output of itm_dedup_landing().
#' @param .col_date Character. Date column, or NULL.
#' @param .tab_voc The vocabulary.
#' @return A tibble ordered by HashIndex and ItemOrder: HashIndex, FilingDate, Year, ItemCode,
#'   ItemOrder, ItemLabel, ItemEra, ItemKind, ItemFurnished.
itm_items_long <- function(.tab, .col_date = "FilingDate", .tab_voc = .itm_items) {
  if (FALSE) {
    .tab      <- tab_filings
    .col_date <- "FilingDate"
    .tab_voc  <- .itm_items
  }

  if (anyDuplicated(.tab$HashIndex)) {
    cli::cli_abort("Input is not one row per filing; itm_dedup_landing() has not been applied.")
  }

  out_ <- .tab |>
    dplyr::mutate(ItemRaw = stringi::stri_split_regex(.data$Items, "\n")) |>
    tidyr::unnest("ItemRaw") |>
    dplyr::filter(nzchar(trimws(.data$ItemRaw))) |>
    dplyr::mutate(
      ItemCode  = stringi::stri_match_first_regex(.data$ItemRaw, .itm_code_regex)[, 2L],
      ItemLabel = stringi::stri_replace_first_regex(.data$ItemRaw, .itm_strip_regex, "")
    )

  # UNNEST PRESERVES INPUT ORDER and the input is one row per filing, so each filing's items are a
  # contiguous run and the position within the run is the order the filer declared them in. Doing
  # this with a grouped row_number() is the same answer and ten times slower on this corpus.
  out_$ItemOrder <- sequence(itm_runs(.x = out_$HashIndex)$Lengths)

  if (!is.null(.col_date) && .col_date %in% names(out_)) {
    out_ <- dplyr::mutate(out_, FilingDate = as.Date(.data[[.col_date]]))
  } else {
    out_ <- dplyr::mutate(out_, FilingDate = as.Date(NA))
  }

  out_ |>
    dplyr::mutate(Year = as.integer(format(.data$FilingDate, "%Y"))) |>
    dplyr::left_join(
      y  = dplyr::select(.tab_voc, "ItemCode", "ItemEra", "ItemKind", "ItemFurnished"),
      by = dplyr::join_by("ItemCode")
    ) |>
    dplyr::select(
      "HashIndex", "FilingDate", "Year", "ItemCode", "ItemOrder", "ItemLabel",
      "ItemEra", "ItemKind", "ItemFurnished"
    ) |>
    dplyr::arrange(.data$HashIndex, .data$ItemOrder)
}

#' One row per filing, with the counts and indicators the analysis asks for
#'
#' THE ERA RULE LIVES HERE AND NOWHERE ELSE. nItemsVoluntary counts 2.02, 7.01 and 8.01 on a
#' post-reform filing and 12, 9 and 5 on a pre-reform one, because those are the same three items
#' under two numbering schemes. Counting only the dotted codes -- which is what a literal string
#' search for "Item 2.02" does -- returns zero for every filing before 23 August 2004, and a
#' difference-in-differences whose outcome is structurally zero before treatment is measuring the
#' reform's numbering rather than anyone's behaviour.
#'
#' CLASSIFIED BY CODE, NOT BY DATE. Forty filings in this corpus report a pre-reform code after the
#' reform and one reports a post-reform code before it, out of two million. The code is what the
#' filer wrote and what EDGAR labelled, so the code decides; the disagreements are reported by
#' itm_era_cross() rather than reconciled silently.
#'
#' A FILING WITH NO REGISTERED CODES STILL GETS A ROW. nItems counts every item the filing declares,
#' registered or not, because it is the answer to "how many items" and an unregistered code is still
#' an item. The kind counts exclude it, because it has no kind.
#'
#' @param .long Output of itm_items_long().
#' @param .tab_flags The flag vocabulary.
#' @return A tibble, one row per HashIndex.
itm_item_flags <- function(.long, .tab_flags = .itm_flag_codes) {
  if (FALSE) {
    .long      <- tab_long
    .tab_flags <- .itm_flag_codes
  }

  # EVERY AGGREGATE IS A RUN SUM. Measured on this corpus, dplyr's summarise over two million groups
  # is three and a half minutes and no reformulation of the expressions moves it -- precomputing the
  # indicators and summing plain columns came out slower, because the per-group dispatch is the cost
  # rather than the work. The same answer off the run structure is eight seconds. itm_runs() asserts
  # the one condition that makes it valid.
  runs_ <- itm_runs(.x = .long$HashIndex)
  kind_ <- .long$ItemKind
  era_  <- .long$ItemEra

  out_ <- tibble::tibble(
    HashIndex       = runs_$Values,
    FilingDate      = .long$FilingDate[runs_$Starts],
    nItems          = runs_$Lengths,
    nItemsDotted    = itm_run_sum(grepl(".", .long$ItemCode, fixed = TRUE), .runs = runs_),
    nItemsVoluntary = itm_run_sum(!is.na(kind_) & kind_ == "Voluntary", .runs = runs_),
    nItemsMandatory = itm_run_sum(!is.na(kind_) & kind_ == "Mandatory", .runs = runs_),
    nItemsUnknown   = itm_run_sum(is.na(kind_), .runs = runs_),
    nEraPost        = itm_run_sum(!is.na(era_) & era_ == "Post", .runs = runs_),
    nEraPre         = itm_run_sum(!is.na(era_) & era_ == "Pre", .runs = runs_),
    ItemCodes       = itm_run_paste(.x = .long$ItemCode, .runs = runs_)
  )

  # One indicator per row of the flag vocabulary, each matching its code under either taxonomy.
  # Driven off the table rather than written out, so adding a sixth flag is a row rather than an edit
  # in two places.
  for (i_ in seq_len(nrow(.tab_flags))) {
    want_ <- stats::na.omit(c(.tab_flags$CodePost[i_], .tab_flags$CodePre[i_]))
    out_[[.tab_flags$Flag[i_]]] <- as.integer(
      itm_run_sum(.long$ItemCode %in% want_, .runs = runs_) > 0L
    )
  }

  out_ |>
    dplyr::mutate(
      # THE ALL-MISSING CASE COMES FIRST, and it is not decoration. A filing whose every code is
      # unregistered has no evidence for either taxonomy, and an earlier formulation -- all() with
      # na.rm over an empty vector, which is TRUE -- would have called it Post on that basis. An
      # unregistered code ALONGSIDE a classified one does not make the filing mixed: the filing is
      # using whichever taxonomy its readable codes belong to, and nItemsUnknown says one was not
      # read.
      ItemEra = dplyr::case_when(
        .data$nEraPost == 0L & .data$nEraPre == 0L ~ NA_character_,
        .data$nEraPre  == 0L                       ~ "Post",
        .data$nEraPost == 0L                       ~ "Pre",
        TRUE                                       ~ "Mixed"
      )
    ) |>
    dplyr::select(-"nEraPost", -"nEraPre") |>
    dplyr::relocate("ItemEra", .before = "ItemCodes")
}

#' Read the two item tables, or build them and write them
#'
#' THE OUTPUT IS THE CACHE, WHICH IS A DEPARTURE AND A DELIBERATE ONE. Everywhere else in this
#' document the expensive step writes to Cache/ and Deployment writes a cheap projection of it to
#' Output/. Here the expensive step IS the output: building the two tables from the landing file is
#' minutes, reading them back is seconds, and a separate cache would be the same four million rows
#' stored twice. So this function reads them where a fingerprint says they are current, and builds
#' and writes them where it does not.
#'
#' THE VOCABULARY AND THE PARSE RULES ENTER THE FINGERPRINT. Reclassifying a code or widening the
#' code pattern changes both tables without changing any file on disk, and a stamp that did not see
#' that would leave the old classification in place with nothing to indicate it.
#'
#' THE COUNTS ARE STORED BESIDE THE STAMP so the runbook reports the same numbers whether it built
#' the tables or read them. A published document that says "read from cache" where it used to say
#' how many registrant rows were collapsed is a document that got quieter as it got faster.
#'
#' @param .path_landing Path to the landing table, one row per filing and registrant.
#' @param .path_long Destination for the long table, one row per filing and item.
#' @param .path_flags Destination for the per-filing table.
#' @param .path_stamp Parquet under Cache/ holding the fingerprint and the build counts.
#' @param .col_date Character. Date column on the landing table.
#' @param .rerun Logical. TRUE rebuilds regardless of the fingerprint.
#' @return A list: Long, Flags, Stats.
itm_build_items <- function(.path_landing, .path_long, .path_flags, .path_stamp,
                            .col_date = "FilingDate", .rerun = FALSE) {
  if (FALSE) {
    .path_landing <- .lP$Input$LandingAll
    .path_long    <- .lP$Output$FilingItems
    .path_flags   <- .lP$Output$FilingItemFlags
    .path_stamp   <- .lP$Cache$ItemStamp
    .col_date     <- "FilingDate"
    .rerun        <- FALSE
  }

  stamp_ <- utils_dir_stamp(
    .dirs  = .path_landing,
    .extra = list(
      Voc  = rlang::hash(.itm_items),
      Flag = rlang::hash(.itm_flag_codes),
      Rex  = c(.itm_code_regex, .itm_strip_regex),
      Date = .col_date
    )
  )

  fresh_ <- !.rerun &&
    fs::file_exists(.path_long) &&
    fs::file_exists(.path_flags) &&
    identical(utils_stamp_read(.path = .path_stamp), stamp_)

  if (fresh_) {
    out_ <- list(
      Long  = arrow::read_parquet(file = .path_long),
      Flags = arrow::read_parquet(file = .path_flags),
      Stats = arrow::read_parquet(file = .path_stamp)
    )
    cli::cli_alert_info(
      "Item tables unchanged: {format(nrow(out_$Long), big.mark = ',')} item rows, read from cache."
    )
    return(out_)
  }

  if (!.rerun && fs::file_exists(.path_long)) {
    cli::cli_alert_warning("The landing table, the vocabulary or a parse rule has moved; items are being rebuilt.")
  }

  raw_   <- itm_read_landing(.path = .path_landing, .col_date = .col_date)
  one_   <- itm_dedup_landing(.tab = raw_)
  drop_  <- attr(one_, "Dropped")
  long_  <- itm_items_long(.tab = one_, .col_date = .col_date)
  flags_ <- itm_item_flags(.long = long_)

  stats_ <- tibble::tibble(
    Stamp          = stamp_,
    LandingRows    = nrow(raw_),
    RegistrantRows = as.integer(drop_[["RegistrantRows"]]),
    Disagreeing    = as.integer(drop_[["Disagreeing"]]),
    Filings        = nrow(one_),
    ItemRows       = nrow(long_)
  )

  arrow::write_parquet(long_, .path_long)
  arrow::write_parquet(flags_, .path_flags)
  arrow::write_parquet(stats_, .path_stamp)
  cli::cli_alert_success(paste0(
    "Written: {format(nrow(long_), big.mark = ',')} item rows and ",
    "{format(nrow(flags_), big.mark = ',')} filing rows."
  ))

  list(Long = long_, Flags = flags_, Stats = stats_)
}

#' Report what the build did, whether it built or read
#'
#' @param .stats The Stats element returned by itm_build_items().
#' @return .stats, invisibly.
itm_report_build <- function(.stats) {
  if (FALSE) .stats <- lst_items$Stats

  n_ <- function(.x) format(.stats[[.x]][1L], big.mark = ",")

  cli::cli_alert_info(paste0(
    "{n_('LandingRows')} landing rows -> {n_('RegistrantRows')} duplicate registrant rows collapsed, ",
    "{n_('Disagreeing')} dropped for disagreement -> {n_('Filings')} filings, ",
    "{n_('ItemRows')} item rows"
  ))

  invisible(.stats)
}

# 1b. What the item tables say -----------------------------------------------------------------------------------------

#' Every code observed, joined to the vocabulary
#'
#' A FULL JOIN, so the table carries both failure directions: a code in the data that is not
#' registered, and a code registered that the data never shows. Either is a reason to look.
#'
#' @param .long Output of itm_items_long().
#' @param .tab_voc The vocabulary.
#' @return A tibble ordered by filing count, with Status.
itm_code_table <- function(.long, .tab_voc = .itm_items) {
  if (FALSE) .long <- tab_long

  .long |>
    dplyr::summarise(
      nFilings  = dplyr::n_distinct(.data$HashIndex),
      YearFirst = itm_year_edge(.x = .data$Year, .fn = min),
      YearLast  = itm_year_edge(.x = .data$Year, .fn = max),
      ItemLabel = dplyr::first(.data$ItemLabel),
      .by       = "ItemCode"
    ) |>
    dplyr::full_join(
      y  = dplyr::select(.tab_voc, "ItemCode", "ItemEra", "ItemKind", "ItemFurnished"),
      by = dplyr::join_by("ItemCode")
    ) |>
    dplyr::mutate(
      nFilings = dplyr::coalesce(.data$nFilings, 0L),
      pFilings = round(.data$nFilings / dplyr::n_distinct(.long$HashIndex) * 100, 2),
      Status   = dplyr::case_when(
        is.na(.data$ItemEra) ~ "unregistered",
        .data$nFilings == 0L ~ "registered, unseen",
        TRUE                 ~ "ok"
      )
    ) |>
    dplyr::select(
      "ItemCode", "ItemEra", "ItemKind", "ItemFurnished", "nFilings", "pFilings",
      "YearFirst", "YearLast", "Status", "ItemLabel"
    ) |>
    dplyr::arrange(dplyr::desc(.data$nFilings))
}

#' Earliest or latest year present, or NA where there are none
#'
#' min() over an all-missing vector returns Inf with a warning, which then travels into a printed
#' table as a number that looks like data.
#'
#' @param .x Integer. Years, possibly all missing.
#' @param .fn Function. min or max.
#' @return Integer, possibly NA.
itm_year_edge <- function(.x, .fn = min) {
  if (FALSE) .x <- c(2004L, NA_integer_)

  keep_ <- .x[!is.na(.x)]
  if (length(keep_) == 0L) NA_integer_ else as.integer(.fn(keep_))
}

#' Which side of the reform each code's filings actually fell on
#'
#' The era column is assigned from the code. This is the check on that assignment, and it is the one
#' number that says whether a pre/post split by code and a pre/post split by date are the same
#' split. On this corpus they differ on 41 filings out of two million.
#'
#' @param .long Output of itm_items_long().
#' @return A tibble: ItemEra, FiledSide, nRows, nFilings, Codes.
itm_era_cross <- function(.long) {
  if (FALSE) .long <- tab_long

  .long |>
    dplyr::filter(!is.na(.data$ItemEra), !is.na(.data$FilingDate)) |>
    dplyr::mutate(FiledSide = dplyr::if_else(.data$FilingDate < .itm_reform_date, "Pre", "Post")) |>
    dplyr::summarise(
      nRows    = dplyr::n(),
      nFilings = dplyr::n_distinct(.data$HashIndex),
      Codes    = paste(sort(unique(.data$ItemCode)), collapse = " "),
      .by      = c("ItemEra", "FiledSide")
    ) |>
    dplyr::arrange(.data$ItemEra, .data$FiledSide)
}

#' How many items a filing declares
#'
#' The distribution rather than the maximum, because the maximum alone cannot say whether the tail
#' is one pathological filing or a population. It is also the answer to whether a fixed-width reshape
#' downstream is safe: the Stata analysis splits the item string into thirteen columns.
#'
#' @param .flags Output of itm_item_flags().
#' @return A tibble: nItems, nFilings, pFilings.
itm_items_per_filing <- function(.flags) {
  if (FALSE) .flags <- tab_flags

  .flags |>
    dplyr::count(.data$nItems, name = "nFilings") |>
    dplyr::mutate(pFilings = round(.data$nFilings / sum(.data$nFilings) * 100, 3)) |>
    dplyr::arrange(.data$nItems)
}

#' Codes whose label is spelled more than one way
#'
#' Expected to be empty, and the fact that it is says something worth knowing: EDGAR generates the
#' item list from its own table rather than from the filer's text, so the label is a property of the
#' code and the shortened forms in the vocabulary are a display choice rather than a normalisation.
#'
#' @param .long Output of itm_items_long().
#' @return A tibble: ItemCode, ItemLabel, nFilings.
itm_label_variants <- function(.long) {
  if (FALSE) .long <- tab_long

  .long |>
    dplyr::summarise(nFilings = dplyr::n_distinct(.data$HashIndex), .by = c("ItemCode", "ItemLabel")) |>
    dplyr::filter(dplyr::n() > 1L, .by = "ItemCode") |>
    dplyr::arrange(.data$ItemCode, dplyr::desc(.data$nFilings))
}

#' The checks the item tables have to pass
#'
#' THE UNREGISTERED-CODE CHECK IS WINDOWED AND THE REPORT IS NOT. A code seen only outside the sample
#' period cannot affect a number in the paper, and stopping a render for one 1996 filing teaches
#' everyone to ignore the check that exists to catch the next real item.
#'
#' @param .long Output of itm_items_long().
#' @param .flags Output of itm_item_flags().
#' @param .codes Output of itm_code_table().
#' @param .window Integer pair. Years the unregistered check applies to.
#' @return A tibble: Check, Pass, Detail.
itm_check_items <- function(.long, .flags, .codes, .window = .itm_window) {
  if (FALSE) {
    .long   <- tab_long
    .flags  <- tab_flags
    .codes  <- tab_codes
    .window <- .itm_window
  }

  res_ <- function(.name, .pass, .detail = "") {
    tibble::tibble(Check = .name, Pass = isTRUE(.pass), Detail = as.character(.detail))
  }

  in_win_ <- .long |>
    dplyr::filter(dplyr::between(.data$Year, .window[1L], .window[2L])) |>
    dplyr::filter(is.na(.data$ItemEra)) |>
    dplyr::summarise(nFilings = dplyr::n_distinct(.data$HashIndex), .by = "ItemCode")

  unseen_ <- dplyr::filter(.codes, .data$Status == "registered, unseen")
  cross_  <- itm_era_cross(.long = .long) |>
    dplyr::filter(.data$ItemEra != .data$FiledSide)
  nocode_ <- sum(is.na(.long$ItemCode))

  dplyr::bind_rows(
    res_("Every item string yields a code", nocode_ == 0L, sprintf("%d unparsed", nocode_)),
    res_(
      sprintf("No unregistered code in %d-%d", .window[1L], .window[2L]),
      nrow(in_win_) == 0L,
      paste(sprintf("%s (n=%d)", in_win_$ItemCode, in_win_$nFilings), collapse = ", ")
    ),
    res_("Every registered code is seen", nrow(unseen_) == 0L,
         paste(unseen_$ItemCode, collapse = ", ")),
    res_("One row per filing in the wide table", !anyDuplicated(.flags$HashIndex), ""),
    res_("Both tables cover the same filings",
         dplyr::n_distinct(.long$HashIndex) == nrow(.flags),
         sprintf("long %d, wide %d", dplyr::n_distinct(.long$HashIndex), nrow(.flags))),
    res_("nItems agrees with the long table",
         identical(sum(.flags$nItems), nrow(.long)),
         sprintf("wide %d, long %d", sum(.flags$nItems), nrow(.long))),
    res_("Kind counts do not exceed nItems",
         all(.flags$nItemsVoluntary + .flags$nItemsMandatory + .flags$nItemsUnknown <= .flags$nItems),
         ""),
    res_("Voluntary count never exceeds three", max(.flags$nItemsVoluntary) <= 3L,
         sprintf("max %d", max(.flags$nItemsVoluntary))),
    res_("No label is spelled two ways", nrow(itm_label_variants(.long = .long)) == 0L, ""),
    res_("Era matches the filing date", nrow(cross_) == 0L,
         paste(sprintf("%s codes on %s-reform filings: %s", cross_$ItemEra,
                       tolower(cross_$FiledSide), format(cross_$nFilings, big.mark = ",")),
               collapse = "; "))
  )
}

#' Print a check table and say whether anything failed
#'
#' @param .tab Output of itm_check_items().
#' @param .title Character. Heading for the block.
#' @return .tab, invisibly.
itm_report_checks <- function(.tab, .title = "Checks") {
  if (FALSE) .tab <- tab_checks

  cli::cli_h3(.title)
  for (i_ in seq_len(nrow(.tab))) {
    row_ <- .tab[i_, ]
    txt_ <- if (nzchar(row_$Detail)) sprintf("%s -- %s", row_$Check, row_$Detail) else row_$Check
    if (row_$Pass) cli::cli_alert_success(txt_) else cli::cli_alert_danger(txt_)
  }

  n_bad_ <- sum(!.tab$Pass)
  if (n_bad_ == 0L) {
    cli::cli_alert_info("{nrow(.tab)} check{?s} passed")
  } else {
    cli::cli_alert_danger("{n_bad_} of {nrow(.tab)} check{?s} did not pass")
  }

  invisible(.tab)
}

# 2. Candidates --------------------------------------------------------------------------------------------------------

#' Documents to attempt extraction on
#'
#' Every 8-K document whose filing reports the item. Not restricted by sample membership and not by
#' file format: both would turn an absent result into two different things.
#'
#' THE ITEM IS MATCHED AS A CODE, NOT AS A SUBSTRING. An earlier version searched the pipe-joined
#' item string for "1.01" literally. Written as a regular expression that pattern carries a wildcard
#' in the middle and would also match "1101"; written literally, as it was, it would also match a
#' filing reporting Item 11.01 if one existed. Neither case arises, but a rule that is accidentally
#' right is worth replacing with one that is right on purpose -- and the parsed table makes it free.
#'
#' CACHED, BECAUSE IT SCANS HALF A GIGABYTE TO ANSWER A FIXED QUESTION. The consolidated metadata and
#' the document index are read in full, and which documents are candidates cannot change unless one
#' of those files, the item table or the item code changes. All four enter the fingerprint, so an
#' edit to any of them invalidates the cache without anyone having to remember.
#'
#' The Path column is cached along with the rest and is machine-specific. That is correct here and
#' would not be in an Output artifact: this file exists to make a re-render cheap on the machine that
#' wrote it, and it is rebuilt from scratch anywhere else.
#'
#' @param .path_meta Path to 01C's consolidated metadata.
#' @param .tab_items Output of itm_items_long(), one row per filing and item.
#' @param .path_index Path to 01B's DocID to Path index.
#' @param .item Character. Item code, e.g. "1.01".
#' @param .path_cache Parquet holding the candidate set and the fingerprint it was built under.
#' @param .rerun Logical. TRUE rebuilds regardless of the fingerprint.
#' @return A tibble: DocID, HashIndex, DocTypeMod, YQ, Removed, Path, ItemCodes, nItems.
itm_candidates <- function(.path_meta, .tab_items, .path_index, .item = "1.01",
                           .path_cache, .rerun = FALSE) {
  if (FALSE) {
    .path_meta  <- .lP$Input$MetaData
    .tab_items  <- tab_long
    .path_index <- .lP$Input$FilePaths
    .item       <- "1.01"
    .path_cache <- .lP$Cache$Candidates
    .rerun      <- FALSE
  }

  # HASHED AS A PLAIN CHARACTER VECTOR, NOT AS THE TIBBLE. Hashing the object would fold in whatever
  # attributes dplyr happened to leave on it, and the point of the key is the content. The vector is
  # deterministic because itm_items_long() sorts; see the note there for why that matters.
  stamp_ <- utils_dir_stamp(
    .dirs  = c(.path_meta, .path_index),
    .extra = list(
      Item  = .item,
      Items = rlang::hash(paste0(.tab_items$HashIndex, "|", .tab_items$ItemCode))
    )
  )

  if (!.rerun && identical(utils_stamp_read(.path = .path_cache), stamp_)) {
    out_ <- dplyr::select(arrow::read_parquet(file = .path_cache), -"Stamp")
    n_   <- nrow(out_)
    cli::cli_alert_info(
      "Candidates unchanged: {format(n_, big.mark = ',')} {cli::qty(n_)}document{?s}, read from cache."
    )
    return(out_)
  }

  if (!.rerun && fs::file_exists(.path_cache)) {
    cli::cli_alert_warning("The metadata, the index or the item table has moved; candidates are being rebuilt.")
  }

  hit_ <- unique(.tab_items$HashIndex[.tab_items$ItemCode == .item])

  # The item list travels with the candidate so the extraction table can report what else the filing
  # said without a second join. Collapsed to one row per filing first: the long table has one row per
  # item and joining it to documents would multiply them.
  fil_ <- .tab_items |>
    dplyr::filter(.data$HashIndex %in% hit_) |>
    dplyr::summarise(
      ItemCodes = paste(.data$ItemCode, collapse = "|"),
      nItems    = dplyr::n(),
      .by       = "HashIndex"
    )

  out_ <- arrow::open_dataset(sources = .path_meta) |>
    dplyr::filter(grepl("^8-K", .data$DocTypeMod)) |>
    dplyr::filter(.data$HashIndex %in% hit_) |>
    dplyr::select("DocID", "HashIndex", "DocTypeMod", "YQ", "Removed") |>
    dplyr::collect() |>
    dplyr::left_join(
      y  = dplyr::select(arrow::read_parquet(.path_index), "DocID", "Path"),
      by = dplyr::join_by("DocID")
    ) |>
    dplyr::left_join(fil_, by = dplyr::join_by("HashIndex")) |>
    dplyr::arrange(.data$DocID)

  arrow::write_parquet(dplyr::mutate(out_, Stamp = stamp_), .path_cache)
  cli::cli_alert_success("Candidates rebuilt: {format(nrow(out_), big.mark = ',')} documents, cached.")

  out_
}
# 3. Locating the item -------------------------------------------------------------------------------------------------

#' Replace every kind of space with a plain one
#'
#' A NON-BREAKING SPACE IS NOT THE ONLY ONE. Filing agents set headings with typographic spacing --
#' thin, hair, en, em, narrow no-break, zero-width -- written either as named entities, as numeric
#' references, or as the characters themselves once the markup is rendered. To a pattern expecting
#' whitespace between "Item" and its number, every one of them is an ordinary character, and the
#' heading does not parse. Handling only &nbsp; catches the common case and misses the recent one:
#' the thin space accounts for almost every unparsed heading filed after 2022.
#'
#' The numeric ranges are enumerated rather than matched loosely, because a pattern wide enough to
#' catch every space reference in the U+2000 block also catches the curly quotation marks two
#' positions above it, and those are content.
#'
#' @param .x Character vector.
#' @return The same, with space-like entities and characters replaced by U+0020.
itm_unspace <- function(.x) {
  if (FALSE) .x <- "Item&#8201;1.01"

  ent_ <- paste0(
    "&(?:nbsp|ensp|emsp|emsp13|emsp14|numsp|puncsp|thinsp|hairsp|MediumSpace|ZeroWidthSpace",
    "|#160|#819[2-9]|#820[0-3]|#8239|#8287|#12288",
    "|#[Xx](?:[Aa]0|200[0-9AaBb]|202[Ff]|205[Ff]|3000|[Ff][Ee][Ff][Ff]));"
  )

  stringi::stri_replace_all_regex(.x, ent_, " ") |>
    stringi::stri_replace_all_regex("[\\p{Zs}\\u200B-\\u200D\\uFEFF]", " ")
}

#' Read one document, normalised for its format
#'
#' WHITESPACE IS COLLAPSED DIFFERENTLY BY FORMAT. In markup the structure lives in the tags, so
#' every run of whitespace including newlines becomes one space and patterns can assume it. In plain
#' text the structure IS the newlines: a heading is a heading because it starts a line, and
#' collapsing them destroys the only signal there is. Runs of spaces and tabs are still collapsed,
#' because column alignment is padding rather than structure.
#'
#' Space-like entities and characters are replaced first, in every spelling: see itm_unspace().
#'
#' @param .path Path to one parsed document.
#' @return A one-row tibble: DocID, DocExt, Body, IsMarkup.
itm_read_doc <- function(.path) {
  if (FALSE) .path <- tab_candidates$Path[1]

  tab_ <- arrow::read_parquet(
    file       = .path,
    col_select = dplyr::all_of(c("DocID", "DocExt", "HTML"))
  )

  markup_ <- tolower(tab_$DocExt[1L]) %in% c("htm", "html", "xml", "xhtml")

  body_ <- itm_unspace(tab_$HTML[1L])
  body_ <- if (isTRUE(markup_)) {
    stringi::stri_replace_all_regex(body_, "\\s+", " ")
  } else {
    stringi::stri_replace_all_regex(body_, "[ \\t]+", " ") |>
      stringi::stri_replace_all_regex("\\n{2,}", "\n")
  }

  tibble::tibble(
    DocID    = tab_$DocID[1L],
    DocExt   = tab_$DocExt[1L],
    Body     = body_,
    IsMarkup = markup_
  )
}

#' Every item boundary in one document, as spans
#'
#' THE ANCHOR DEPENDS ON THE FORMAT, BECAUSE A HEADING IS MARKED DIFFERENTLY IN EACH. In markup a
#' heading follows a closing tag, so the anchor is ">". Without it the word "item" inside a sentence
#' -- and it appears inside most agreements -- produces a boundary where there is none. In plain text
#' there are almost no tags: the only ">" characters come from SGML wrappers such as <PAGE>, so the
#' same anchor finds one or two boundaries in a document with six, and the first span runs to the end
#' of the filing. There, a heading is a heading because it starts a line.
#'
#' THE HEADER WINDOW IS STRIPPED OF TAGS BEFORE THE CODE IS READ. Modern filing agents wrap headings
#' in nested elements, so the raw window reads "Item</span><span> 1.01" and a pattern expecting
#' digits after the word finds a tag instead. The code is a property of the rendered heading, not of
#' the markup, so the markup is removed before it is read.
#'
#' A signature block is located alongside the items because it terminates the last one; without it
#' the final section would run to the end of the document and swallow the exhibit index.
#'
#' @param .body Character. The normalised body of one document.
#' @param .markup Logical. TRUE for markup formats, FALSE for plain text.
#' @return A tibble of ordered markers: Item, Start, Stop, Header.
itm_locate_items <- function(.body, .markup = TRUE) {
  if (FALSE) {
    doc_    <- itm_read_doc(tab_candidates$Path[1])
    .body   <- doc_$Body
    .markup <- doc_$IsMarkup
  }

  rex_ <- if (isTRUE(.markup)) {
    c("(?i)>\\s*ITEM", "(?i)>\\s*SIGNATURE")
  } else {
    c("(?im)^\\s*ITEM", "(?im)^\\s*SIGNATURE")
  }

  pos_ <- unlist(lapply(rex_, function(.r) stringi::stri_locate_all_regex(.body, .r)[[1L]][, 1L]))
  pos_ <- sort(pos_[!is.na(pos_)])

  if (length(pos_) == 0L) return(tibble::tibble())

  tibble::tibble(Start = pos_) |>
    dplyr::mutate(
      Header = stringi::stri_sub(.body, .data$Start, .data$Start + 150L),
      Plain  = stringi::stri_replace_all_regex(.data$Header, "<[^>]*>", " "),
      Plain  = stringi::stri_replace_all_regex(.data$Plain, "\\s+", " "),
      Item   = stringi::stri_extract_first_regex(
        .data$Plain, "(?i)ITEM\\s*\\d+\\s*_?\\.?\\s*\\d*|SIGNATURE"
      ),
      Item   = gsub(" ", "", toupper(.data$Item)),
      Stop   = dplyr::lead(.data$Start),
      # THE LAST MARKER RUNS TO THE END OF THE DOCUMENT. Dropping it, as taking lead() alone does,
      # discards any filing whose only boundary is the item itself -- which happens whenever the
      # signature block is styled in a way the anchor does not see. Extracting too much is visible
      # in the length distribution; extracting nothing is a document lost without trace.
      Stop   = dplyr::coalesce(.data$Stop, nchar(.body))
    ) |>
    dplyr::select("Start", "Stop", "Item", "Header")
}

#' Render one span as text
#'
#' @param .body Character. The normalised body of one document.
#' @param .start Integer. Span start, in code points.
#' @param .stop Integer. Span end, in code points.
#' @param .markup Logical. TRUE parses the span as HTML; FALSE takes it as it stands.
#' @return Character scalar, or NA if a markup fragment does not parse.
itm_render_span <- function(.body, .start, .stop, .markup = TRUE) {
  if (FALSE) {
    .body   <- itm_read_doc(tab_candidates$Path[1])$Body
    .start  <- 1000L
    .stop   <- 4000L
    .markup <- TRUE
  }

  frag_ <- stringi::stri_sub(.body, .start, .stop)

  if (!isTRUE(.markup)) return(trimws(stringi::stri_replace_all_regex(frag_, "\\s+", " ")))

  out_ <- try(
    expr   = rvest::html_text2(rvest::read_html(frag_, encoding = "UTF-8", options = "RECOVER")),
    silent = TRUE
  )

  if (inherits(out_, "try-error")) NA_character_ else trimws(gsub("<|>", "", out_))
}


# 3b. One document -----------------------------------------------------------------------------------------------------

#' How much of a rendered span is body rather than heading
#'
#' The item code and the regulated title together run to about forty characters and say nothing
#' about the agreement, so a length test that includes them measures the wrong thing. Both are
#' removed and what remains is counted.
#'
#' The title is matched loosely -- optional article, optional brackets, optional trailing stop --
#' because filers punctuate it inconsistently, and a pattern that insisted on one spelling would
#' subtract nothing from most of them.
#'
#' @param .text Character. One rendered span.
#' @return Integer. Characters remaining after the heading and title.
itm_body_chars <- function(.text) {
  if (FALSE) .text <- "Item 1.01 Entry into a Material Definitive Agreement"

  if (is.na(.text)) return(NA_integer_)

  nchar(trimws(itm_strip_heading(.x = .text)))
}

#' Remove the item heading and the regulated title from a rendered span
#'
#' ONE DEFINITION, TWO USES. The length test subtracts them because they are not the summary; the
#' examples table subtracts them because every preview would otherwise open with the same forty
#' characters. Spelling the patterns out in both places is two chances to disagree about what counts
#' as the summary.
#'
#' @param .x Character. Rendered spans.
#' @return The same, with the leading heading and title removed.
itm_strip_heading <- function(.x) {
  if (FALSE) .x <- "Item 1.01 Entry into a Material Definitive Agreement. On March 3, 2015, ..."

  .x |>
    stringi::stri_replace_first_regex(.itm_head_regex, "") |>
    stringi::stri_replace_first_regex(.itm_title_regex, "")
}

#' Locate and extract the item from one document
#'
#' EVERY CANDIDATE LEAVES ONE ROW. A document that could not be read, that carries no markers, or
#' whose fragment would not parse is as much a result as one that extracted, and the outcomes are
#' what make those countable rather than inferred from an absence.
#'
#'   extracted           one item span found and rendered
#'   ambiguous-longest   the item appears more than once; the longest span was taken
#'   heading-only        the span rendered to the item heading and its title, with no body
#'   no-body             the document carries nothing to search
#'   no-markers          no item boundary found at all
#'   markers-unparsed    boundaries found, none of them carrying a readable item code
#'   not-found           codes read, none of them this item
#'   parse-failed        the fragment would not parse as markup
#'
#' A SPAN THAT IS ONLY A HEADING IS NOT A SUMMARY. Some filers set the item list as a table with the
#' numbers in one column and the bodies in another; others put the heading and its title in one cell
#' and the body somewhere the span does not reach. Either way the rendered text is "Item 1.01" or
#' "Item 1.01 Entry into a Material Definitive Agreement" and nothing more, and an earlier version
#' counted tens of thousands of those as successes.
#'
#' The test is on what remains after the heading is removed, not on total length. A threshold on the
#' whole string has to be guessed, and guessing it at fifty characters put two of the three shortest
#' extractions in the corpus exactly on the boundary. The regulated title is a fixed cost of about
#' forty characters that carries no information about the agreement, so subtracting it first makes
#' the threshold mean something: what is left is the summary, and a summary that is a few dozen
#' characters is not one.
#'
#' Recorded and not extracted. Taking the next span instead would return this item's body
#' concatenated with the following item's, and for text analysis contaminated text is worse than a
#' known gap: a clean exclusion is countable, a silent error is not.
#'
#' THE LONGEST SPAN IS TAKEN WHEN THE ITEM REPEATS. A filing with a table of contents lists Item 1.01
#' twice, once as a line in the contents and once as the section. Requiring exactly one occurrence,
#' as an earlier implementation did, discards those documents entirely; length separates them, and
#' the two length distributions are reported so the assumption can be checked rather than trusted.
#'
#' THE TABULAR TEST IS ON THE RENDERED TEXT, NOT THE RAW SPAN. Between two headings in a table sit a
#' few hundred characters of markup and nine characters of text, so a threshold applied to the raw
#' span never fires while one applied to the rendered string fires exactly when it should. The check
#' therefore comes after rendering rather than before it, which costs one render on a document that
#' is then discarded and buys a test that measures what it claims to.
#'
#' @param .path Path to one parsed document.
#' @param .item Character. Item code, e.g. "1.01".
#' @param .min_chars Integer. Characters that must remain after the item heading and its title are
#'   removed for the span to count as a summary.
#' @return A one-row tibble: DocID, DocExt, Outcome, nMarkers, nCodesRead, Start, Stop, nCharsItem,
#'   nBodyChars, ItemText.
itm_process_doc <- function(.path, .item = "1.01", .min_chars = 30L) {
  if (FALSE) {
    .path      <- tab_candidates$Path[1]
    .item      <- "1.01"
    .min_chars <- 30L
  }

  doc_ <- itm_read_doc(.path)

  out_ <- function(.outcome, .n_markers = 0L, .n_items = 0L, .start = NA_integer_,
                   .stop = NA_integer_, .text = NA_character_) {
    tibble::tibble(
      DocID      = doc_$DocID[1L],
      DocExt     = doc_$DocExt[1L],
      Outcome    = .outcome,
      nMarkers   = .n_markers,
      nCodesRead = .n_items,
      Start      = .start,
      Stop       = .stop,
      nCharsItem = if (is.na(.text)) NA_integer_ else nchar(.text),
      nBodyChars = itm_body_chars(.text = .text),
      ItemText   = .text
    )
  }

  if (is.na(doc_$Body[1L]) || !nzchar(doc_$Body[1L])) return(out_("no-body"))

  all_ <- itm_locate_items(.body = doc_$Body[1L], .markup = doc_$IsMarkup[1L])
  if (nrow(all_) == 0L) return(out_("no-markers"))

  mrk_ <- dplyr::filter(all_, !is.na(.data$Item), !is.na(.data$Stop))
  if (nrow(mrk_) == 0L) return(out_("markers-unparsed", nrow(all_)))

  key_ <- gsub(" ", "", toupper(paste0("ITEM", .item)))
  hit_ <- dplyr::filter(mrk_, .data$Item == key_)
  if (nrow(hit_) == 0L) return(out_("not-found", nrow(mrk_)))

  n_it_ <- nrow(hit_)
  hit_  <- hit_ |>
    dplyr::mutate(Len = .data$Stop - .data$Start) |>
    dplyr::slice_max(.data$Len, n = 1L, with_ties = FALSE)

  txt_ <- itm_render_span(
    .body   = doc_$Body[1L],
    .start  = hit_$Start[1L],
    .stop   = hit_$Stop[1L],
    .markup = doc_$IsMarkup[1L]
  )
  if (is.na(txt_)) return(out_("parse-failed", nrow(mrk_), n_it_))

  # Tested after rendering, because the raw span between two headings in a table is hundreds of
  # characters of markup around ten characters of text.
  if (itm_body_chars(.text = txt_) < .min_chars) {
    return(out_("heading-only", nrow(mrk_), n_it_, hit_$Start[1L], hit_$Stop[1L]))
  }

  out_(
    .outcome   = if (n_it_ > 1L) "ambiguous-longest" else "extracted",
    .n_markers = nrow(mrk_),
    .n_items   = n_it_,
    .start     = hit_$Start[1L],
    .stop      = hit_$Stop[1L],
    .text      = txt_
  )
}



# 4. Summary heuristics ------------------------------------------------------------------------------------------------

#' How many distinct agreement dates an Item 1.01 summary names
#'
#' THE PUBLISHED FILTER, RESTORED AS A MEASUREMENT. The paper restricts to "8-K filings containing
#' exactly one contract summary within Item 1.01, excluding cases with multiple concurrent
#' agreements". That restriction was implemented by counting distinct "On <Month> <D>, <YYYY>"
#' openings in the summary text and keeping the ones with exactly one, because the regulated
#' narrative opens that way -- "On March 3, 2015, the Company entered into". It is a proxy for the
#' number of agreements described, not a count of them, and calling it what it is costs nothing.
#'
#' STORED AS A COUNT, NOT APPLIED AS A FILTER. A filter here would make the corpus mean one thing for
#' this measure and another for every other, which is the failure this script exists to avoid: a
#' summary that was excluded and a summary that was never extracted would be indistinguishable
#' downstream. The analysis that wants exactly one keeps SummaryIsSingle; nothing else has to know.
#'
#' DERIVED FROM THE CACHE RATHER THAN IN THE WORKER. The pattern is deliberately narrow -- full month
#' names, a comma, a four-digit year -- and it will be widened. Computing it inside itm_process_doc()
#' would put a tunable rule behind a corpus pass measured in hours; computing it from the extracted
#' text costs one traversal of a table that is already in memory.
#'
#' THE FIRST DATE IS THE ONE KEPT, in the order it appears in the summary. Where a summary names two,
#' the first is the one the opening sentence carries, and the count says not to trust it.
#'
#' THE MONTH IS LOOKED UP, NOT PARSED WITH %B. strptime's %B matches month names in the session's
#' LC_TIME locale, so as.Date("March 3, 2015", format = "%B %d, %Y") returns the date on an English
#' machine and NA on a German one -- silently, for every row. month.name is English whatever the
#' locale, so matching against it and assembling an ISO string gives the same answer everywhere.
#'
#' @param .tab The joined extraction table, carrying ItemText.
#' @param .regex Character. Pattern for the narrative opening.
#' @return .tab with nSummaryDates, SummaryDateFirst and SummaryIsSingle added.
itm_summary_dates <- function(.tab, .regex = .itm_date_regex) {
  if (FALSE) {
    .tab   <- tab_item101
    .regex <- .itm_date_regex
  }

  hit_ <- stringi::stri_extract_all_regex(.tab$ItemText, .regex, omit_no_match = TRUE)
  uni_ <- lapply(hit_, function(.x) unique(stringi::stri_replace_all_regex(.x, "\\s+", " ")))

  first_ <- vapply(uni_, function(.x) if (length(.x) == 0L) NA_character_ else .x[1L], character(1L))
  part_  <- stringi::stri_match_first_regex(first_, .regex)

  .tab |>
    dplyr::mutate(
      nSummaryDates    = ifelse(is.na(.data$ItemText), NA_integer_, lengths(uni_)),
      SummaryDateFirst = as.Date(sprintf(
        "%04d-%02d-%02d",
        as.integer(part_[, 4L]), match(part_[, 2L], month.name), as.integer(part_[, 3L])
      )),
      SummaryIsSingle  = dplyr::if_else(is.na(.data$nSummaryDates), NA_integer_,
                                        as.integer(.data$nSummaryDates == 1L))
    )
}

#' A few summaries at each date count, with what the pattern actually matched
#'
#' THE POINT IS THE ZERO BUCKET. A count says sixteen per cent of recovered summaries name no date
#' the pattern reads; only the text says whether that is the pattern's fault. Printing them beside
#' the ones that did match is the cheapest form of that check, and it is in the published document
#' rather than in a probe because the limitation is permanent and a reader should meet it here.
#'
#' SAMPLED ONCE PER BUCKET, ON A FIXED SEED. Drawing the sample twice -- once to extract the match
#' and once to build the preview -- returns two different sets of documents and prints a match beside
#' the wrong text, which looks entirely plausible and is entirely wrong. A fixed seed also keeps a
#' re-render from silently changing a published table.
#'
#' THE HEADING IS STRIPPED FROM THE PREVIEW. Every summary opens with the same forty characters of
#' item code and regulated title, and showing them would spend a third of the line on the one part
#' of the text that carries no information.
#'
#' @param .tab Output of itm_summary_dates().
#' @param .n Integer. Examples per bucket.
#' @param .chars Integer. Preview length.
#' @param .seed Integer. Sampling seed.
#' @param .regex Character. The pattern whose matches are shown.
#' @return A tibble: Dates, Match, Preview.
itm_summary_examples <- function(.tab, .n = 4L, .chars = 110L, .seed = 42L,
                                 .regex = .itm_date_regex) {
  if (FALSE) {
    .tab <- tab_item101
    .n   <- 4L
  }

  ok_ <- dplyr::filter(.tab, .data$Outcome %in% itm_outcomes_ok(), !is.na(.data$nSummaryDates))

  pick_ <- function(.keep, .lab) {
    idx_ <- which(.keep)
    if (length(idx_) == 0L) return(tibble::tibble())

    set.seed(.seed)
    take_ <- idx_[sample(seq_along(idx_), min(.n, length(idx_)))]
    txt_  <- ok_$ItemText[take_]

    tibble::tibble(
      Dates   = .lab,
      Match   = vapply(
        X         = txt_,
        FUN       = function(.t) {
          hit_ <- unique(stringi::stri_extract_all_regex(.t, .regex, omit_no_match = TRUE)[[1L]])
          if (length(hit_) == 0L) "--" else paste(hit_, collapse = " / ")
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
      ),
      Preview = itm_strip_heading(.x = txt_) |>
        stringi::stri_replace_all_regex("\\s+", " ") |>
        trimws() |>
        stringi::stri_sub(1L, .chars)
    )
  }

  dplyr::bind_rows(
    pick_(.keep = ok_$nSummaryDates == 0L, .lab = "0 -- none read"),
    pick_(.keep = ok_$nSummaryDates == 1L, .lab = "1 -- the published filter keeps these"),
    pick_(.keep = ok_$nSummaryDates >= 2L, .lab = "2+ -- concurrent, or a date reused")
  )
}

#' How the date count is distributed, among summaries that were recovered
#'
#' @param .tab Output of itm_summary_dates().
#' @return A tibble: nSummaryDates, nDocs, pDocs.
itm_summary_date_bands <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  .tab |>
    dplyr::filter(.data$Outcome %in% itm_outcomes_ok()) |>
    dplyr::count(.data$nSummaryDates, name = "nDocs") |>
    dplyr::mutate(pDocs = round(.data$nDocs / sum(.data$nDocs) * 100, 2)) |>
    dplyr::arrange(.data$nSummaryDates)
}
# 5. Deployment --------------------------------------------------------------------------------------------------------

#' Write the extracted summaries, if what produced them has moved
#'
#' A GIGABYTE IS NOT WRITTEN TO PRODUCE A FILE THAT ALREADY EXISTS. Roughly a quarter of a million
#' summaries carry the text itself, and rewriting them on every render is the single most expensive
#' thing this document does after the extraction pass. What the file contains is fixed by the
#' extraction cache and the candidate set, so both enter the fingerprint.
#'
#' THE TABLE IS BUILT EITHER WAY. Only the write is guarded: the join that produces it costs seconds
#' and every report below reads it, so skipping the computation would save nothing and leave the
#' reports describing a file rather than an object.
#'
#' @param .tab The joined table, one row per candidate.
#' @param .path_out Destination parquet path; the published artifact.
#' @param .path_source The extraction cache the table was derived from.
#' @param .path_stamp Parquet under Cache/ holding the fingerprint .path_out was last written under.
#' @param .rerun Logical. TRUE writes regardless of the fingerprint.
#' @return .tab, invisibly.
itm_write_item101 <- function(.tab, .path_out, .path_source, .path_stamp, .rerun = FALSE) {
  if (FALSE) {
    .tab         <- tab_item101
    .path_out    <- .lP$Output$Item101
    .path_source <- .lP$Cache$Extracted
    .path_stamp  <- .lP$Cache$OutputStamp
    .rerun       <- FALSE
  }

  stamp_ <- utils_dir_stamp(.dirs = .path_source, .extra = .tab$DocID)

  fresh_ <- !.rerun &&
    fs::file_exists(.path_out) &&
    identical(utils_stamp_read(.path = .path_stamp), stamp_)

  if (fresh_) {
    cli::cli_alert_info("Extraction unchanged: {fs::path_file(.path_out)} was left as it stands.")
    return(invisible(.tab))
  }

  arrow::write_parquet(.tab, .path_out)
  arrow::write_parquet(tibble::tibble(Stamp = stamp_), .path_stamp)
  cli::cli_alert_success(
    "Written: {format(nrow(.tab), big.mark = ',')} rows to {fs::path_file(.path_out)}."
  )

  invisible(.tab)
}


# 6. Reports -----------------------------------------------------------------------------------------------------------

#' Which outcomes count as a recovered summary
#'
#' Defined once because four reports and, shortly, 02 all ask the same question. A predicate spelled
#' out in five places is five chances to disagree about whether an ambiguous extraction counts, and
#' the disagreement would show up as two coverage figures in one paper.
#'
#' @return Character vector of outcome labels.
itm_outcomes_ok <- function() c("extracted", "ambiguous-longest")

#' Outcomes, by file format
#'
#' Crossed with format rather than reported alone, because the interesting question is not how often
#' extraction failed but whether it failed for a reason that could be fixed. A format whose documents
#' extract as reliably as HTML has no business being excluded; one that never extracts is an honest
#' limitation, and only the cross table distinguishes them.
#'
#' @param .tab Output of the extraction pass.
#' @return A tibble: DocExt, one column per outcome, nDocs, ShareExtracted.
itm_outcome_summary <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  .tab |>
    dplyr::mutate(Ext = dplyr::if_else(.data$DocExt %in% c("htm", "html"), .data$DocExt, "other")) |>
    dplyr::summarise(
      nDocs          = dplyr::n(),
      nExtracted     = sum(.data$Outcome %in% itm_outcomes_ok()),
      nAmbiguous     = sum(.data$Outcome == "ambiguous-longest"),
      nHeadingOnly   = sum(.data$Outcome == "heading-only"),
      nNoMarkers     = sum(.data$Outcome == "no-markers"),
      nUnparsed      = sum(.data$Outcome == "markers-unparsed"),
      nNotFound      = sum(.data$Outcome == "not-found"),
      nNoBody        = sum(.data$Outcome == "no-body"),
      nParseFailed   = sum(.data$Outcome == "parse-failed"),
      .by            = "Ext"
    ) |>
    dplyr::mutate(ShareExtracted = .data$nExtracted / .data$nDocs) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' Length of the extracted summaries
#'
#' The check on the longest-span rule. If a contents entry were being taken for a section the
#' ambiguous group would sit far below the unambiguous one; if the two distributions agree, the rule
#' is picking the section.
#'
#' @param .tab Output of the extraction pass.
#' @return A tibble: Outcome, nDocs and the quartiles of nCharsItem.
itm_length_summary <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  .tab |>
    dplyr::filter(.data$Outcome %in% itm_outcomes_ok()) |>
    dplyr::summarise(
      nDocs  = dplyr::n(),
      Min    = min(.data$nBodyChars),
      Q25    = round(stats::quantile(.data$nBodyChars, 0.25)),
      Median = stats::median(.data$nBodyChars),
      Q75    = round(stats::quantile(.data$nBodyChars, 0.75)),
      Max    = max(.data$nBodyChars),
      .by    = "Outcome"
    )
}

#' Extraction over time
#'
#' @param .tab Output of the extraction pass, carrying YQ.
#' @return A tibble: Year, nDocs, nExtracted, ShareExtracted.
itm_coverage_by_year <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  .tab |>
    dplyr::mutate(Year = as.integer(stringi::stri_sub(.data$YQ, 1L, 4L))) |>
    dplyr::summarise(
      nDocs      = dplyr::n(),
      nExtracted = sum(.data$Outcome %in% itm_outcomes_ok()),
      .by        = "Year"
    ) |>
    dplyr::mutate(ShareExtracted = .data$nExtracted / .data$nDocs) |>
    dplyr::arrange(.data$Year)
}

#' Outcomes by year, in full
#'
#' The share extracted alone says a year went wrong without saying how. A year that falls because
#' documents stopped carrying item markers is a change in filing format; one that falls because the
#' markers are there but Item 1.01 is not among them is a change in what filers report; one that
#' falls because nothing parses is a change in markup. They need different responses, and only the
#' full breakdown distinguishes them.
#'
#' @param .tab Output of the extraction pass.
#' @return A tibble: Year, one column per outcome, nDocs, ShareExtracted.
itm_outcome_by_year <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  .tab |>
    dplyr::mutate(Year = as.integer(stringi::stri_sub(.data$YQ, 1L, 4L))) |>
    dplyr::summarise(
      nDocs        = dplyr::n(),
      Extracted    = sum(.data$Outcome == "extracted"),
      Ambiguous    = sum(.data$Outcome == "ambiguous-longest"),
      HeadingOnly  = sum(.data$Outcome == "heading-only"),
      NoMarkers    = sum(.data$Outcome == "no-markers"),
      Unparsed     = sum(.data$Outcome == "markers-unparsed"),
      NotFound     = sum(.data$Outcome == "not-found"),
      NoBody       = sum(.data$Outcome == "no-body"),
      ParseFailed  = sum(.data$Outcome == "parse-failed"),
      .by          = "Year"
    ) |>
    dplyr::mutate(ShareExtracted = (.data$Extracted + .data$Ambiguous) / .data$nDocs) |>
    dplyr::arrange(.data$Year)
}

#' Outcomes by the actual file extension
#'
#' The summary table folds every non-HTML format into one row, which answers whether the restriction
#' to HTML costs anything but not which format it costs it on. A format that extracts as reliably as
#' HTML has no business being excluded; one that never extracts is an honest limitation. Only the
#' per-format table separates the two.
#'
#' @param .tab Output of the extraction pass.
#' @param .min Integer. Formats with fewer documents than this are pooled as "other".
#' @return A tibble: DocExt, nDocs, nExtracted, ShareExtracted and the failure modes.
itm_format_detail <- function(.tab, .min = 50L) {
  if (FALSE) {
    .tab <- tab_item101
    .min <- 50L
  }

  keep_ <- .tab |>
    dplyr::count(.data$DocExt, name = "n") |>
    dplyr::filter(.data$n >= .min) |>
    dplyr::pull(.data$DocExt)

  .tab |>
    dplyr::mutate(Ext = dplyr::if_else(.data$DocExt %in% keep_, .data$DocExt, "other")) |>
    dplyr::summarise(
      nDocs       = dplyr::n(),
      nExtracted  = sum(.data$Outcome %in% itm_outcomes_ok()),
      nHeadingOnly = sum(.data$Outcome == "heading-only"),
      NoMarkers   = sum(.data$Outcome == "no-markers"),
      Unparsed    = sum(.data$Outcome == "markers-unparsed"),
      NotFound    = sum(.data$Outcome == "not-found"),
      .by         = "Ext"
    ) |>
    dplyr::mutate(ShareExtracted = .data$nExtracted / .data$nDocs) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' Are the extracted summaries a plausible length?
#'
#' EXTRACTED IS NOT THE SAME AS CORRECT. A span that renders is recorded as a success whatever it
#' contains, and two failure modes survive that test. A summary of a few dozen characters is a
#' heading with the section missing, which happens when the next marker sits immediately after this
#' one. A summary of a hundred thousand characters is a span that never terminated, which happens
#' when no later marker was found and the slice ran to the end of the filing.
#'
#' Neither is detectable from the outcome, and both are visible here. The bands are deliberately
#' coarse: they are meant to show whether a problem exists, not to define a threshold.
#'
#' MEASURED ON THE BODY, NOT THE WHOLE SPAN. The heading and its regulated title are a fixed forty
#' characters that say nothing about the agreement, and counting them puts a bare heading into the
#' same band as a one-sentence summary.
#'
#' @param .tab Output of the extraction pass.
#' @return A tibble: Band, nDocs, Share.
itm_length_bands <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  .tab |>
    dplyr::filter(!is.na(.data$nBodyChars)) |>
    dplyr::mutate(
      Band = dplyr::case_when(
        .data$nBodyChars <  100L    ~ "1-under 100: barely more than the heading",
        .data$nBodyChars <  500L    ~ "2-100 to 500: very short",
        .data$nBodyChars < 20000L   ~ "3-500 to 20k: typical",
        .data$nBodyChars < 100000L  ~ "4-20k to 100k: long",
        .default                    = "5-over 100k: the span did not terminate"
      )
    ) |>
    dplyr::summarise(nDocs = dplyr::n(), .by = "Band") |>
    dplyr::mutate(Share = .data$nDocs / sum(.data$nDocs)) |>
    dplyr::arrange(.data$Band)
}

#' The shortest and longest extractions, with a preview
#'
#' The bands say how many are implausible; this says what they look like. A count without an example
#' invites a threshold to be picked from the number rather than from the documents.
#'
#' @param .tab Output of the extraction pass.
#' @param .n Integer. How many from each end.
#' @param .chars Integer. Preview length.
#' @return A tibble: Which, DocID, DocExt, nMarkers, nCharsItem, Preview.
itm_examples <- function(.tab, .n = 3L, .chars = 160L) {
  if (FALSE) {
    .tab   <- tab_item101
    .n     <- 3L
    .chars <- 160L
  }

  hit_ <- dplyr::filter(.tab, !is.na(.data$ItemText))

  dplyr::bind_rows(
    dplyr::mutate(dplyr::slice_min(hit_, .data$nBodyChars, n = .n, with_ties = FALSE), Which = "shortest"),
    dplyr::mutate(dplyr::slice_max(hit_, .data$nBodyChars, n = .n, with_ties = FALSE), Which = "longest")
  ) |>
    dplyr::mutate(
      Preview = gsub("\\s+", " ", stringi::stri_sub(.data$ItemText, 1L, .chars))
    ) |>
    dplyr::select("Which", "DocID", "DocExt", "nMarkers", "nCodesRead", "nCharsItem", "nBodyChars", "Preview")
}

#' Every report in this document, in order
#'
#' TAKES THE FOUR SUMMARIES, DOES NOT COMPUTE THEM. This block is shown twice, in Results and again
#' in the Overview, and two of the four are also shown separately in between. Computing them at each
#' call means the same numbers are derived six times over from a table carrying the text of a quarter
#' of a million summaries.
#'
#' @param .tab_out Output of itm_outcome_summary().
#' @param .tab_fmt Output of itm_format_detail().
#' @param .tab_len Output of itm_length_summary().
#' @param .tab_band Output of itm_length_bands().
#' @return Invisibly NULL.
itm_report_all <- function(.tab_out, .tab_fmt, .tab_len, .tab_band) {
  if (FALSE) {
    .tab_out  <- tab_outcome_summary
    .tab_fmt  <- tab_format_detail
    .tab_len  <- tab_length_summary
    .tab_band <- tab_length_bands
  }

  tbl_head("Outcomes by file format")
  tbl_out(
    .tab    = .tab_out,
    .title  = NULL,
    .pct    = "ShareExtracted",
    .digits = 1L,
    .notes  = c(
      Ext            = "Formats other than HTML are attempted, not excluded; this is the result.",
      nAmbiguous     = "The item appeared more than once and the longest span was taken."
    )
  )

  tbl_head("Outcomes by file extension")
  tbl_out(
    .tab    = .tab_fmt,
    .title  = NULL,
    .pct    = "ShareExtracted",
    .digits = 1L
  )

  tbl_head("Length of the extracted summaries")
  tbl_out(
    .tab   = .tab_len,
    .title = NULL,
    .notes = c(Median = "Ambiguous and unambiguous should agree; a shorter ambiguous group would \\
                         mean a contents entry was taken for a section.")
  )

  tbl_head("Are the lengths plausible?")
  tbl_out(
    .tab    = .tab_band,
    .title  = NULL,
    .pct    = "Share",
    .digits = 1L,
    .notes  = c(Band = "Extracted is not the same as correct: both ends of this table are failures \\
                        that passed the outcome test.")
  )

  invisible(NULL)
}


# 7. Figures -----------------------------------------------------------------------------------------------------------
# DEFINED HERE, WRITTEN NOWHERE. The document displays what these return and the consolidated release
# script writes the files the manuscript needs.

#' How the outcomes compose each year
#'
#' Shares rather than counts, because filing volume varies fourfold across the period and a count
#' chart would show that instead of what is being asked.
#'
#' @param .tab Output of itm_outcome_by_year().
#' @return A ggplot object.
itm_plot_outcome_share <- function(.tab) {
  if (FALSE) .tab <- itm_outcome_by_year(tab_item101)

  dat_ <- .tab |>
    dplyr::select("Year", "Extracted", "Ambiguous", "HeadingOnly", "NoMarkers", "Unparsed", "NotFound") |>
    tidyr::pivot_longer(cols = -"Year", names_to = "Outcome", values_to = "nDocs") |>
    dplyr::filter(.data$nDocs > 0L)

  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$nDocs, fill = .data$Outcome)) +
    ggplot2::geom_col(position = "fill") +
    plot_scale_fill_cat() +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    plot_theme(.grid = "y")
}

#' Distribution of summary length
#'
#' On a log scale, because the range spans four orders of magnitude and a linear axis would render
#' everything below ten thousand characters as one bar.
#'
#' @param .tab Output of the extraction pass.
#' @return A ggplot object.
itm_plot_length <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  dat_ <- dplyr::filter(.tab, !is.na(.data$nBodyChars), .data$nBodyChars > 0L)

  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$nBodyChars, fill = .data$Outcome)) +
    ggplot2::geom_histogram(bins = 60L, position = "identity", alpha = 0.7) +
    ggplot2::scale_x_log10(labels = scales::comma) +
    plot_scale_fill_cat() +
    plot_scale_y_count() +
    ggplot2::labs(x = "Characters in the extracted summary", y = "Documents", fill = NULL) +
    plot_theme(.grid = "y")
}

#' Extraction coverage per year
#'
#' @param .tab Output of itm_coverage_by_year().
#' @return A ggplot object.
itm_plot_coverage <- function(.tab) {
  if (FALSE) .tab <- itm_coverage_by_year(tab_item101)

  dat_ <- .tab |>
    dplyr::select("Year", Extracted = "nExtracted", "nDocs") |>
    dplyr::mutate(Failed = .data$nDocs - .data$Extracted, nDocs = NULL) |>
    tidyr::pivot_longer(cols = c("Extracted", "Failed"), names_to = "Outcome", values_to = "nDocs")

  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$nDocs, fill = .data$Outcome)) +
    ggplot2::geom_col() +
    plot_scale_fill_cat() +
    plot_scale_y_count() +
    ggplot2::labs(x = NULL, y = "Documents", fill = NULL) +
    plot_theme(.grid = "y")
}
