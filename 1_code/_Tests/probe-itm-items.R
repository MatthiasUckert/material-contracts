# ======================================================================================================================
# PROBE -- the 8-K item vocabulary, and whether it survives contact with the corpus
# ======================================================================================================================
#
# WHAT THIS IS. A probe, not a pair. Source it and it runs its own checks and prints them. It writes
# nothing. Section 0 is the only part that ships: it moves into 01D-Item101.R as a declaration, and
# the checks in sections 1 and 2 move into the runbook as chunks.
#
# WHY IT PRINTS ON SOURCE. A file of assignments is silent by construction, which is correct for a
# function library and useless for checking a table by eye. The runner at the foot is what makes this
# testable; it is also the reason this file is not the shipping one.
#
# THE CORPUS SECTION IS OPTIONAL. It needs arrow and the landing table. Where either is missing the
# vocabulary checks still run and say so, because a probe that refuses to start tells you nothing
# about the part that does not need data.

# 0. The item vocabulary -----------------------------------------------------------------------------------------------

#: EVERY 8-K ITEM CODE THAT EXISTS, UNDER BOTH TAXONOMIES. 46 rows: 33 dotted codes from the form as
#: reformed on 23 August 2004, and 13 undotted ones from the form before it. That is the complete
#: legal vocabulary of each, and a corpus-wide count returns exactly these and nothing else -- which
#: is the check that the parser is reading codes rather than inventing them.
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
#: THE LABEL IS A SHORT CANONICAL FORM, NOT THE FILING'S OWN. Filers punctuate the regulated titles
#: inconsistently -- one spells Item 10 with a full stop where an apostrophe belongs -- so the raw
#: label stays in the data as ItemLabelRaw and this one is what tables print.
#:
#: NO EFFECTIVE DATES ARE DECLARED. Several codes postdate their own era: 9 arrives with Regulation FD
#: in 2000, 10, 11, 12 and 13 in 2003, 6.06 in 2006, 5.08 in 2010, 1.04 in 2011 and 1.05 in December
#: 2023. Hand-entering 46 dates is a second thing to keep right; section 2 reports first-seen and
#: last-seen year per code from the corpus instead, which validates the era and surfaces the mid-era
#: additions without anyone having to remember them.
.itm_items <- tibble::tribble(
  ~ItemCode, ~ItemEra, ~ItemKind,   ~ItemFurnished, ~ItemSuccessor, ~ItemLabel,

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

#: THE DATE THAT SEPARATES THEM. 2004 belongs to neither: a filing from that year is only
#: interpretable once you know which side of 23 August it fell on, and reporting it as its own row is
#: also the check that the boundary is in the right place.
.itm_reform_date <- as.Date("2004-08-23")

#: HOW A CODE IS READ OUT OF A FILING'S ITEM STRING. Anchored at the head, so the label is whatever
#: follows and no colon is ever split on. Splitting on colons is what turned Item 5.02 -- whose title
#: carries two more of them -- into four items and produced three phantom codes at 88,974 filings
#: each. The separator after the code is optional because filers punctuate it every way there is.
.itm_code_regex  <- "^\\s*Item\\s+([0-9]+(?:\\.[0-9]+)?)"
.itm_strip_regex <- "^\\s*Item\\s+[0-9]+(?:\\.[0-9]+)?\\s*[:.,-]?\\s*"

# 1. Checks that need no data ------------------------------------------------------------------------------------------

#' One check result
#'
#' @param .name Character. What was checked.
#' @param .pass Logical. Whether it held.
#' @param .detail Character. What was found, whether or not it held.
#' @return A one-row tibble: Check, Pass, Detail.
itm_result <- function(.name, .pass, .detail = "") {
  tibble::tibble(Check = .name, Pass = isTRUE(.pass), Detail = as.character(.detail))
}

#' Every check the vocabulary can answer about itself
#'
#' THE CROSSWALK INVARIANTS ARE THE ONES WORTH HAVING. Counting rows catches a deletion; checking that
#' a pre-reform voluntary item maps to a post-reform voluntary item catches a misclassification, which
#' is the error that would actually change a coefficient.
#'
#' @param .tab The vocabulary.
#' @return A tibble of results, one row per check.
itm_check_vocab <- function(.tab = .itm_items) {
  if (FALSE) .tab <- .itm_items

  post_ <- dplyr::filter(.tab, .data$ItemEra == "Post")
  pre_  <- dplyr::filter(.tab, .data$ItemEra == "Pre")

  # The crosswalk, joined back onto itself so kind and furnished status can be compared across it.
  walk_ <- pre_ |>
    dplyr::filter(!is.na(.data$ItemSuccessor)) |>
    dplyr::left_join(
      y  = dplyr::select(post_, ItemSuccessor = "ItemCode", KindPost = "ItemKind", FurnPost = "ItemFurnished"),
      by = dplyr::join_by("ItemSuccessor")
    )

  # Sections and their highest code, so a gap inside one is caught rather than assumed away.
  sect_ <- c("1" = 5L, "2" = 6L, "3" = 3L, "4" = 2L, "5" = 8L, "6" = 6L, "7" = 1L, "8" = 1L, "9" = 1L)
  want_ <- unlist(lapply(names(sect_), \(s) sprintf("%s.%02d", s, seq_len(sect_[[s]]))), use.names = FALSE)

  dplyr::bind_rows(
    itm_result("46 rows", nrow(.tab) == 46L, sprintf("%d", nrow(.tab))),
    itm_result("33 post, 13 pre", nrow(post_) == 33L && nrow(pre_) == 13L,
               sprintf("post %d, pre %d", nrow(post_), nrow(pre_))),
    itm_result("ItemCode unique", !anyDuplicated(.tab$ItemCode),
               paste(.tab$ItemCode[duplicated(.tab$ItemCode)], collapse = ", ")),
    itm_result("ItemKind registered", all(.tab$ItemKind %in% .itm_item_kinds),
               paste(setdiff(.tab$ItemKind, .itm_item_kinds), collapse = ", ")),
    itm_result("ItemEra registered", all(.tab$ItemEra %in% .itm_item_eras),
               paste(setdiff(.tab$ItemEra, .itm_item_eras), collapse = ", ")),
    itm_result("ItemFurnished is 0 or 1", all(.tab$ItemFurnished %in% c(0L, 1L)), ""),
    itm_result("Post codes well formed", all(grepl("^[1-9]\\.[0-9]{2}$", post_$ItemCode)),
               paste(post_$ItemCode[!grepl("^[1-9]\\.[0-9]{2}$", post_$ItemCode)], collapse = ", ")),
    itm_result("Pre codes are 1 to 13", setequal(pre_$ItemCode, as.character(1:13)),
               paste(sort(as.integer(pre_$ItemCode)), collapse = " ")),
    itm_result("Post sections complete", setequal(post_$ItemCode, want_),
               paste(c(setdiff(want_, post_$ItemCode), setdiff(post_$ItemCode, want_)), collapse = ", ")),
    itm_result("Successor NA on post rows", all(is.na(post_$ItemSuccessor)), ""),
    itm_result("Successors are post codes", all(walk_$ItemSuccessor %in% post_$ItemCode),
               paste(setdiff(walk_$ItemSuccessor, post_$ItemCode), collapse = ", ")),
    itm_result("Successors are one to one", !anyDuplicated(walk_$ItemSuccessor),
               paste(walk_$ItemSuccessor[duplicated(walk_$ItemSuccessor)], collapse = ", ")),
    itm_result("Kind survives the crosswalk", all(walk_$ItemKind == walk_$KindPost),
               paste(walk_$ItemCode[walk_$ItemKind != walk_$KindPost], collapse = ", ")),
    itm_result("Furnished survives the crosswalk", all(walk_$ItemFurnished == walk_$FurnPost),
               paste(walk_$ItemCode[walk_$ItemFurnished != walk_$FurnPost], collapse = ", ")),
    itm_result("Voluntary post is 2.02 7.01 8.01",
               setequal(post_$ItemCode[post_$ItemKind == "Voluntary"], c("2.02", "7.01", "8.01")),
               paste(post_$ItemCode[post_$ItemKind == "Voluntary"], collapse = " ")),
    itm_result("Voluntary pre is 5 9 12",
               setequal(pre_$ItemCode[pre_$ItemKind == "Voluntary"], c("5", "9", "12")),
               paste(pre_$ItemCode[pre_$ItemKind == "Voluntary"], collapse = " ")),
    itm_result("Exhibits is 9.01 and 7",
               setequal(.tab$ItemCode[.tab$ItemKind == "Exhibits"], c("9.01", "7")),
               paste(.tab$ItemCode[.tab$ItemKind == "Exhibits"], collapse = " ")),
    itm_result("Labels unique within era",
               !anyDuplicated(paste(.tab$ItemEra, .tab$ItemLabel)),
               paste(.tab$ItemLabel[duplicated(paste(.tab$ItemEra, .tab$ItemLabel))], collapse = "; ")),
    itm_result("Labels non-empty", all(nzchar(.tab$ItemLabel)), "")
  )
}

#' Print a result table and say whether anything failed
#'
#' @param .tab Output of itm_check_vocab() or itm_check_corpus().
#' @param .title Character. Heading for the block.
#' @return .tab, invisibly.
itm_report <- function(.tab, .title) {
  cli::cli_h2(.title)
  for (i_ in seq_len(nrow(.tab))) {
    row_ <- .tab[i_, ]
    txt_ <- if (nzchar(row_$Detail)) sprintf("%s -- %s", row_$Check, row_$Detail) else row_$Check
    if (row_$Pass) cli::cli_alert_success(txt_) else cli::cli_alert_danger(txt_)
  }
  n_bad_ <- sum(!.tab$Pass)
  if (n_bad_ == 0L) {
    cli::cli_alert_info("{nrow(.tab)} check{?s} passed")
  } else {
    cli::cli_alert_danger("{n_bad_} of {nrow(.tab)} check{?s} FAILED")
  }
  invisible(.tab)
}

# 2. The corpus check, split so only the read needs arrow --------------------------------------------------------------

#' Earliest year present, or NA where there are none
#'
#' min() over an all-NA vector returns Inf with a warning, which then travels into a printed table as
#' a number that looks like data. This returns the missing value instead.
#'
#' @param .x Integer. Years, possibly all missing.
#' @param .fn Function. min or max.
#' @return Integer, possibly NA.
itm_year_edge <- function(.x, .fn = min) {
  keep_ <- .x[!is.na(.x)]
  if (length(keep_) == 0L) NA_integer_ else as.integer(.fn(keep_))
}

#' Read the item lists off a landing table
#'
#' THE ONLY FUNCTION HERE THAT NEEDS ARROW, and it is four lines so that everything downstream can be
#' tested without the corpus. THE POPULATION IS THE WHOLE POINT: 01A's LandingPageAll is every filing
#' EDGAR indexed, 01C's LandingPage only those carrying a retrieved Exhibit 10. The counts differ by
#' an order of magnitude and only one is the frame for a given question, so the path is an argument.
#'
#' @param .path Path to a landing table carrying HashIndex and Items.
#' @param .col_date Character. Date column to read years from; dropped if the schema lacks it.
#' @return A tibble: HashIndex, Items and the date column where present. One row per filing AND
#'   REGISTRANT -- see itm_dedup_landing().
itm_read_landing <- function(.path, .col_date = "FilingDate") {
  if (FALSE) .path <- .PATH_LANDING

  have_ <- names(arrow::open_dataset(sources = .path))
  cols_ <- c("HashIndex", "Items", if (!is.null(.col_date) && .col_date %in% have_) .col_date)

  arrow::open_dataset(sources = .path) |>
    dplyr::select(dplyr::all_of(cols_)) |>
    dplyr::filter(!is.na(.data$Items)) |>
    dplyr::collect()
}

#' One row per filing, where its registrants agree about the item list
#'
#' THE LANDING TABLE IS KEYED ON FILING AND REGISTRANT, NOT ON FILING. A filing co-filed by forty-one
#' affiliated entities appears forty-one times with the same item list, and unnesting before
#' collapsing multiplies every item count by the number of registrants. The first version of this
#' probe skipped the step and reported a maximum of 205 items on a filing; the true figure is that
#' number divided by the registrant count, and the giveaway was that nItems divided by nFilings came
#' out an exact integer almost everywhere in the tail.
#'
#' WHERE REGISTRANTS DISAGREE THE FILING IS DROPPED rather than one version being chosen, because
#' there is no basis for choosing and the alternative is a silent coin flip on the variable that
#' decides everything downstream. This is 01D's rule, reproduced so the two agree; 05B instead keeps
#' an arbitrary registrant's version, which is the second answer the 01D docstring warns about.
#'
#' @param .tab Output of itm_read_landing().
#' @return A tibble with one row per HashIndex, and a Dropped attribute recording the disagreements.
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

#' One row per filing and item
#'
#' @param .tab Output of itm_read_landing().
#' @param .col_date Character. Date column, or NULL.
#' @return A tibble: HashIndex, ItemRaw, ItemCode, ItemLabelRaw, Year.
itm_items_long <- function(.tab, .col_date = "FilingDate") {
  if (FALSE) .tab <- tab_landing

  out_ <- .tab |>
    dplyr::mutate(ItemRaw = stringi::stri_split_regex(.data$Items, "\n")) |>
    tidyr::unnest("ItemRaw") |>
    dplyr::filter(nzchar(trimws(.data$ItemRaw))) |>
    dplyr::mutate(
      ItemCode     = stringi::stri_match_first_regex(.data$ItemRaw, .itm_code_regex)[, 2],
      ItemLabelRaw = stringi::stri_replace_first_regex(.data$ItemRaw, .itm_strip_regex, "")
    )

  if (!is.null(.col_date) && .col_date %in% names(out_)) {
    out_ <- dplyr::mutate(out_, Year = as.integer(format(as.Date(.data[[.col_date]]), "%Y")))
  } else {
    out_ <- dplyr::mutate(out_, Year = NA_integer_)
  }

  if (!is.null(.col_date) && .col_date %in% names(out_)) {
    out_ <- dplyr::mutate(out_, FilingDate = as.Date(.data[[.col_date]]))
  } else {
    out_ <- dplyr::mutate(out_, FilingDate = as.Date(NA))
  }

  dplyr::select(out_, "HashIndex", "ItemRaw", "ItemCode", "ItemLabelRaw", "FilingDate", "Year")
}

#' Every code observed, joined to the vocabulary, with the checks that follow from it
#'
#' A full join, so the table carries both failure directions: a code in the data that is not
#' registered, and a code registered that the data never shows.
#'
#' @param .long Output of itm_items_long().
#' @param .tab The vocabulary.
#' @return A list: Codes, PerFiling, Checks.
itm_check_codes <- function(.long, .tab = .itm_items) {
  if (FALSE) .long <- tab_long

  codes_ <- .long |>
    dplyr::summarise(
      nRows     = dplyr::n(),
      nFilings  = dplyr::n_distinct(.data$HashIndex),
      YearFirst = itm_year_edge(.data$Year, min),
      YearLast  = itm_year_edge(.data$Year, max),
      .by       = "ItemCode"
    ) |>
    dplyr::full_join(y = .tab, by = dplyr::join_by("ItemCode")) |>
    dplyr::mutate(
      nRows    = dplyr::coalesce(.data$nRows, 0L),
      nFilings = dplyr::coalesce(.data$nFilings, 0L),
      Status   = dplyr::case_when(
        is.na(.data$ItemEra) ~ "unregistered",
        .data$nFilings == 0L ~ "registered, unseen",
        TRUE                 ~ "ok"
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$nFilings))

  per_filing_ <- dplyr::count(.long, .data$HashIndex, name = "nItems")

  unreg_  <- dplyr::filter(codes_, .data$Status == "unregistered")
  unseen_ <- dplyr::filter(codes_, .data$Status == "registered, unseen")
  nocode_ <- sum(is.na(.long$ItemCode))
  max_    <- max(per_filing_$nItems)

  # WHICH SIDE OF 23 AUGUST 2004 EACH ROW FELL ON, against the era its code belongs to. A post-reform
  # code before the reform, or a pre-reform code after it, is either a filer numbering by the wrong
  # form or a date that is not what it claims -- and either way the era column is not free.
  era_ <- .long |>
    dplyr::left_join(y = dplyr::select(.tab, "ItemCode", "ItemEra"), by = dplyr::join_by("ItemCode")) |>
    dplyr::filter(!is.na(.data$ItemEra), !is.na(.data$FilingDate)) |>
    dplyr::mutate(FiledSide = dplyr::if_else(.data$FilingDate < .itm_reform_date, "Pre", "Post")) |>
    dplyr::summarise(
      nRows     = dplyr::n(),
      nFilings  = dplyr::n_distinct(.data$HashIndex),
      Codes     = paste(sort(unique(.data$ItemCode)), collapse = " "),
      .by       = c("ItemEra", "FiledSide")
    )

  cross_ <- dplyr::filter(era_, .data$ItemEra != .data$FiledSide)

  checks_ <- dplyr::bind_rows(
    itm_result("Every item string yields a code", nocode_ == 0L, sprintf("%d unparsed", nocode_)),
    itm_result("No unregistered codes", nrow(unreg_) == 0L,
               paste(sprintf("%s (n=%d, %s-%s)", unreg_$ItemCode, unreg_$nFilings,
                             unreg_$YearFirst, unreg_$YearLast), collapse = ", ")),
    itm_result("Every registered code is seen", nrow(unseen_) == 0L,
               paste(unseen_$ItemCode, collapse = ", ")),
    itm_result("Max items per filing is under 14", max_ < 14L, sprintf("max %d", max_)),
    itm_result("No code appears on the wrong side of the reform", nrow(cross_) == 0L,
               paste(sprintf("%s codes on %s-reform filings: %d filing(s)", cross_$ItemEra,
                             tolower(cross_$FiledSide), cross_$nFilings), collapse = "; "))
  )

  list(Codes = codes_, PerFiling = per_filing_, Era = era_, Checks = checks_)
}

# 2b. The parser, tested against the observed strings -------------------------------------------------------------------

#: THE FORTY-EIGHT STRINGS A CORPUS-WIDE COUNT RETURNS, so the parser can be tested where the corpus
#: is not. Two are reconstructed from a truncated console listing -- 5.02 and 2.04 -- and only their
#: tails were cut, which is the part no rule depends on. 5.02 is the one that matters: its regulated
#: title carries two more colons, and splitting on them is what produced three phantom codes at
#: 88,974 filings each. "Item 57:" is in here because it is in the data, once.
.itm_observed <- c(
  "Item 9.01: Financial Statements and Exhibits",
  "Item 8.01: Other Events",
  "Item 2.02: Results of Operations and Financial Condition",
  "Item 7.01: Regulation FD Disclosure",
  "Item 1.01: Entry into a Material Definitive Agreement",
  paste0("Item 5.02: Departure of Directors or Certain Officers; Election of Directors; ",
         "Appointment of Certain Officers: Compensatory Arrangements of Certain Officers"),
  "Item 7: Financial statements and exhibits",
  "Item 5: Other events",
  paste0("Item 2.03: Creation of a Direct Financial Obligation or an Obligation under an ",
         "Off-Balance Sheet Arrangement of a Registrant"),
  "Item 5.07: Submission of Matters to a Vote of Security Holders",
  "Item 3.02: Unregistered Sales of Equity Securities",
  "Item 5.03: Amendments to Articles of Incorporation or Bylaws; Change in Fiscal Year",
  "Item 2.01: Completion of Acquisition or Disposition of Assets",
  "Item 9: Regulation FD Disclosure",
  "Item 2: Acquisition or disposition of assets",
  "Item 1.02: Termination of a Material Definitive Agreement",
  "Item 4.01: Changes in Registrant's Certifying Accountant",
  "Item 12: Results of Operations and Financial Condition",
  paste0("Item 3.01: Notice of Delisting or Failure to Satisfy a Continued Listing Rule or ",
         "Standard; Transfer of Listing"),
  "Item 3.03: Material Modifications to Rights of Security Holders",
  "Item 4: Changes in registrant's certifying accountant",
  "Item 1: Changes in control of registrant",
  "Item 5.01: Changes in Control of Registrant",
  paste0("Item 4.02: Non-Reliance on Previously Issued Financial Statements or a Related Audit ",
         "Report or Completed Interim Review"),
  "Item 2.05: Cost Associated with Exit or Disposal Activities",
  paste0("Item 2.04: Triggering Events That Accelerate or Increase a Direct Financial Obligation ",
         "or an Obligation under an Off-Balance Sheet Arrangement"),
  "Item 2.06: Material Impairments",
  "Item 1.03: Bankruptcy or Receivership",
  "Item 5.06: Change in Shell Company Status",
  "Item 3: Bankruptcy or receivership",
  "Item 6.02: Change of Servicer or Trustee",
  "Item 6: Resignations of registrant's directors",
  paste0("Item 5.05: Amendments to the Registrant's Code of Ethics, or Waiver of a Provision of ",
         "the Code of Ethics"),
  "Item 8: Change in fiscal year",
  "Item 5.04: Temporary Suspension of Trading Under Registrant's Employee Benefit Plans",
  "Item 5.08: Shareholder Nominations Pursuant to Exchange Act Rule 14a-11",
  "Item 6.05: Securities Act Updating Disclosure",
  "Item 6.01: ABS Informational and Computational Material",
  "Item 1.04: Mine Safety - Reporting of Shutdowns and Patterns of Violations",
  "Item 11: Temporary Suspension of Trading Under Registrant's Employee Benefit Plans",
  "Item 6.04: Failure to Make a Required Distribution",
  "Item 10: Amendments to the Registrant.s Code of Ethics",
  "Item 6.03: Change in Credit Enhancement or Other External Support",
  "Item 1.05: Material Cybersecurity Incidents",
  "Item 6.06: Static Pool",
  "Item 13: Receipt of an Attorney's Written Notice Pursuant to 17 CFR 205.3(d)",
  "Item 57:"
)

#' Run the code and label rules over a character vector of item strings
#'
#' @param .x Character. Item strings as they appear in a filing's item list.
#' @param .tab The vocabulary.
#' @return A tibble: ItemRaw, ItemCode, ItemLabelRaw, ItemEra, ItemKind, Status.
itm_parse_strings <- function(.x, .tab = .itm_items) {
  if (FALSE) .x <- .itm_observed

  tibble::tibble(ItemRaw = .x) |>
    dplyr::mutate(
      ItemCode     = stringi::stri_match_first_regex(.data$ItemRaw, .itm_code_regex)[, 2],
      ItemLabelRaw = stringi::stri_replace_first_regex(.data$ItemRaw, .itm_strip_regex, "")
    ) |>
    dplyr::left_join(y = dplyr::select(.tab, "ItemCode", "ItemEra", "ItemKind"),
                     by = dplyr::join_by("ItemCode")) |>
    dplyr::mutate(Status = dplyr::case_when(
      is.na(.data$ItemCode) ~ "no code read",
      is.na(.data$ItemEra)  ~ "unregistered",
      TRUE                  ~ "ok"
    ))
}

#' Checks the parser can answer with no corpus
#'
#' @param .x Character. Item strings.
#' @param .tab The vocabulary.
#' @return A tibble of results.
itm_check_parser <- function(.x = .itm_observed, .tab = .itm_items) {
  if (FALSE) .x <- .itm_observed

  got_    <- itm_parse_strings(.x = .x, .tab = .tab)
  unreg_  <- dplyr::filter(got_, .data$Status == "unregistered")
  nocode_ <- dplyr::filter(got_, .data$Status == "no code read")
  seen_   <- got_$ItemCode[got_$Status == "ok"]
  colon_  <- dplyr::filter(got_, .data$ItemCode == "5.02")

  dplyr::bind_rows(
    itm_result("A code is read from every string", nrow(nocode_) == 0L,
               paste(nocode_$ItemRaw, collapse = " | ")),
    itm_result("Exactly one unregistered code", nrow(unreg_) == 1L,
               paste(sprintf("%s from %s", unreg_$ItemCode, unreg_$ItemRaw), collapse = "; ")),
    itm_result("All 46 registered codes are covered", setequal(seen_, .tab$ItemCode),
               paste(setdiff(.tab$ItemCode, seen_), collapse = ", ")),
    itm_result("Interior colons do not split 5.02", nrow(colon_) == 1L &&
                 grepl("^Departure of Directors", colon_$ItemLabelRaw),
               colon_$ItemLabelRaw),
    itm_result("No label starts with a separator",
               !any(grepl("^[[:space:]:.,-]", got_$ItemLabelRaw[nzchar(got_$ItemLabelRaw)])),
               paste(utils::head(got_$ItemLabelRaw[grepl("^[[:space:]:.,-]", got_$ItemLabelRaw)], 3),
                     collapse = " | "))
  )
}

# 3. Runner ------------------------------------------------------------------------------------------------------------

# THE CONSOLE IS WIDENED FOR THE DURATION. A truncated label is exactly the thing this probe exists
# to let you read, and the default eighty columns cuts every one of them.
.itm_width_old <- getOption("width")
options(width = 165)

cli::cli_h1("Item vocabulary probe")

res_vocab <- itm_check_vocab(.tab = .itm_items)
itm_report(.tab = res_vocab, .title = "Vocabulary, no data needed")

res_parser <- itm_check_parser(.x = .itm_observed, .tab = .itm_items)
itm_report(.tab = res_parser, .title = "Parser against the 48 observed strings")

cli::cli_h2("The vocabulary as declared")
.itm_items |>
  as.data.frame() |>
  print(row.names = FALSE, right = FALSE)

cli::cli_h2("Counts by era and kind")
.itm_items |>
  dplyr::count(.data$ItemEra, .data$ItemKind, name = "nCodes") |>
  tidyr::pivot_wider(names_from = "ItemKind", values_from = "nCodes", values_fill = 0L) |>
  as.data.frame() |>
  print(row.names = FALSE)

cli::cli_h2("The pre-to-post crosswalk")
.itm_items |>
  dplyr::filter(.data$ItemEra == "Pre") |>
  dplyr::select("ItemCode", "ItemKind", "ItemFurnished", "ItemSuccessor", "ItemLabel") |>
  dplyr::left_join(
    y  = dplyr::select(.itm_items, ItemSuccessor = "ItemCode", LabelPost = "ItemLabel"),
    by = dplyr::join_by("ItemSuccessor")
  ) |>
  dplyr::mutate(dplyr::across(
    .cols = c("ItemLabel", "LabelPost"),
    .fns  = ~ stringi::stri_sub(.x, 1L, 50L)
  )) |>
  as.data.frame() |>
  print(row.names = FALSE, right = FALSE)

# THE PURE PATH IS EXERCISED ON A FIXTURE FIRST, so a failure downstream is known to be about the
# corpus rather than about the code that reads it. Three filings built by matching on the observed
# strings rather than by position, because a positional index into that vector silently returned NA
# the first time this was written.
cli::cli_h2("Corpus path, on a synthetic landing table")

itm_pick <- function(.pattern) .itm_observed[grepl(.pattern, .itm_observed)][1]

# Six rows, four filings. ddd appears three times with an identical item list, which is what a
# co-filed submission looks like; eee appears twice with different lists, which is a disagreement and
# must be dropped; bbb carries pre-reform codes on a pre-reform date and ccc a post-reform code on a
# post-reform one, so the era check has something to agree with.
tab_fake <- tibble::tibble(
  HashIndex  = c("aaa", "bbb", "ccc", "ddd", "ddd", "ddd", "eee", "eee"),
  FilingDate = as.Date(c("2019-05-01", "2002-03-14", "2007-11-30",
                         "2015-01-09", "2015-01-09", "2015-01-09", "2016-06-02", "2016-06-02")),
  Items      = c(
    paste(vapply(c("^Item 9\\.01", "^Item 8\\.01", "^Item 5\\.02"), itm_pick, ""), collapse = "\n"),
    paste(vapply(c("^Item 7:", "^Item 5:", "^Item 9:"), itm_pick, ""), collapse = "\n"),
    paste(vapply(c("^Item 1\\.01", "^Item 57"), itm_pick, ""), collapse = "\n"),
    itm_pick("^Item 2\\.02"), itm_pick("^Item 2\\.02"), itm_pick("^Item 2\\.02"),
    itm_pick("^Item 7\\.01"), itm_pick("^Item 8\\.01")
  )
)

tab_fake_one <- itm_dedup_landing(.tab = tab_fake)
fake_drop    <- attr(tab_fake_one, "Dropped")
fake_long    <- itm_items_long(.tab = tab_fake_one, .col_date = "FilingDate")
fake_res  <- itm_check_codes(.long = fake_long, .tab = .itm_items)

cli::cli_alert_info(paste0(
  "{nrow(tab_fake)} landing row{?s} -> {cli::qty(nrow(tab_fake_one))}{nrow(tab_fake_one)} filing{?s} ",
  "-> {cli::qty(nrow(fake_long))}{nrow(fake_long)} item row{?s}"
))
fake_long |>
  dplyr::mutate(dplyr::across(
    .cols = c("ItemRaw", "ItemLabelRaw"),
    .fns  = ~ stringi::stri_sub(.x, 1L, 54L)
  )) |>
  as.data.frame() |>
  print(row.names = FALSE, right = FALSE)

# EXPECTED VALUES, NOT RAW CHECKS. The corpus checks are meant to fail on a fixture that holds six
# codes out of forty-six, so reporting them here would say nothing. These assert what the fixture
# should produce, and every one of them must pass.
fake_codes_ <- sort(fake_long$ItemCode)
dplyr::bind_rows(
  itm_result("Dedup collapses ddd's three registrant rows", fake_drop[["RegistrantRows"]] == 2L,
             sprintf("%d row(s)", fake_drop[["RegistrantRows"]])),
  itm_result("Dedup drops eee, whose registrants disagree", fake_drop[["Disagreeing"]] == 2L,
             sprintf("%d row(s)", fake_drop[["Disagreeing"]])),
  itm_result("Four filings in, four rows survive minus eee", nrow(tab_fake_one) == 4L,
             sprintf("%d", nrow(tab_fake_one))),
  itm_result("ddd contributes one item row, not three",
             sum(fake_long$HashIndex == "ddd") == 1L,
             sprintf("%d", sum(fake_long$HashIndex == "ddd"))),
  itm_result("Fixture yields 9 item rows", nrow(fake_long) == 9L, sprintf("%d", nrow(fake_long))),
  itm_result("No string fails to yield a code", !anyNA(fake_long$ItemCode),
             paste(fake_long$ItemRaw[is.na(fake_long$ItemCode)], collapse = " | ")),
  itm_result("Codes read are 1.01 2.02 5.02 5 57 7 8.01 9.01 9",
             setequal(fake_codes_, c("1.01", "2.02", "5.02", "5", "57", "7", "8.01", "9.01", "9")),
             paste(fake_codes_, collapse = " ")),
  itm_result("57 is the only unregistered code",
             setequal(dplyr::filter(fake_res$Codes, .data$Status == "unregistered")$ItemCode, "57"),
             paste(dplyr::filter(fake_res$Codes, .data$Status == "unregistered")$ItemCode, collapse = " ")),
  itm_result("Pre-reform codes resolve to the Pre era",
             all(dplyr::filter(fake_res$Codes, .data$ItemCode %in% c("5", "7", "9"))$ItemEra == "Pre"), ""),
  itm_result("5.02 keeps its interior colons",
             grepl("Officers: Compensatory", fake_long$ItemLabelRaw[fake_long$ItemCode == "5.02"]),
             stringi::stri_sub(fake_long$ItemLabelRaw[fake_long$ItemCode == "5.02"], 1L, 60L)),
  itm_result("Years are read from the date column",
             setequal(fake_long$Year, c(2002L, 2007L, 2015L, 2019L)),
             paste(sort(unique(fake_long$Year)), collapse = " ")),
  itm_result("9.01 first and last year are 2019",
             identical(dplyr::filter(fake_res$Codes, .data$ItemCode == "9.01")$YearFirst, 2019L), ""),
  itm_result("No code falls on the wrong side of the reform",
             nrow(dplyr::filter(fake_res$Era, .data$ItemEra != .data$FiledSide)) == 0L,
             paste(fake_res$Era$ItemEra, fake_res$Era$FiledSide, collapse = "; "))
) |>
  itm_report(.title = "Fixture, where everything must pass")

# THE CORPUS SECTION RUNS ONLY WHERE IT CAN. Set .PATH_LANDING before sourcing to point it elsewhere;
# the default is 01A's full index, which is the frame for "what are the items" rather than for
# anything joined to Contracts.
if (!exists(".PATH_LANDING")) {
  .PATH_LANDING <- if (requireNamespace("here", quietly = TRUE)) {
    here::here("2_output", "01A-EdgarIndex", "Output", "LandingPageAll.parquet")
  } else {
    "2_output/01A-EdgarIndex/Output/LandingPageAll.parquet"
  }
}

if (!requireNamespace("arrow", quietly = TRUE)) {
  cli::cli_alert_warning("arrow is not installed -- corpus checks skipped")
} else if (!file.exists(.PATH_LANDING)) {
  cli::cli_alert_warning("Not found, corpus checks skipped: {.path {(.PATH_LANDING)}}")
} else {
  cli::cli_alert_info("Reading {.path {(.PATH_LANDING)}}")
  tab_landing <- itm_read_landing(.path = .PATH_LANDING, .col_date = "FilingDate")
  n_raw_      <- nrow(tab_landing)
  cli::cli_alert_info("{format(n_raw_, big.mark = \",\")} landing row{?s}, one per filing and registrant")

  tab_one_ <- itm_dedup_landing(.tab = tab_landing)
  drop_    <- attr(tab_one_, "Dropped")
  cli::cli_alert_info(paste0(
    "{format(drop_[['RegistrantRows']], big.mark = \",\")} duplicate registrant row{?s} collapsed, ",
    "{format(drop_[['Disagreeing']], big.mark = \",\")} dropped where registrants disagree"
  ))
  cli::cli_alert_info("{format(nrow(tab_one_), big.mark = \",\")} {cli::qty(nrow(tab_one_))}filing{?s} remain")

  tab_long   <- itm_items_long(.tab = tab_one_, .col_date = "FilingDate")
  res_corpus <- itm_check_codes(.long = tab_long, .tab = .itm_items)

  itm_report(.tab = res_corpus$Checks, .title = "Vocabulary against the corpus")

  cli::cli_h2("Every code observed, against the vocabulary")
  res_corpus$Codes |>
    dplyr::select("ItemCode", "ItemEra", "ItemKind", "nFilings", "YearFirst", "YearLast",
                  "Status", "ItemLabel") |>
    as.data.frame() |>
    print(row.names = FALSE, right = FALSE)

  cli::cli_h2("Item era against the side of the reform the filing fell on")
  res_corpus$Era |>
    dplyr::mutate(Codes = stringi::stri_sub(.data$Codes, 1L, 60L)) |>
    as.data.frame() |>
    print(row.names = FALSE, right = FALSE)

  cli::cli_h2("Items per filing")
  res_corpus$PerFiling |>
    dplyr::count(.data$nItems, name = "nFilings") |>
    dplyr::arrange(.data$nItems) |>
    as.data.frame() |>
    print(row.names = FALSE)

  cli::cli_h2("Raw label variants per code, where filers disagree")
  tab_long |>
    dplyr::distinct(.data$ItemCode, .data$ItemLabelRaw) |>
    dplyr::filter(dplyr::n() > 1L, .by = "ItemCode") |>
    dplyr::arrange(.data$ItemCode) |>
    print(n = 40)
}

options(width = .itm_width_old)
cli::cli_alert_success("Probe complete")
