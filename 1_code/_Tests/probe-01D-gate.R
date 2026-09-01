source("01D-GetItems.R")

ok <- function(.name, .pass, .detail = "") {
  cat(if (isTRUE(.pass)) "PASS " else "FAIL ", .name,
      if (any(nzchar(.detail))) paste0(" -- ", paste(.detail, collapse = " ")) else "",
      "\n", sep = "")
  isTRUE(.pass)
}

res <- logical(0)

# -- Fixture: five landing rows, three filings. ddd is co-filed twice with an identical list; eee
# -- disagrees between its two registrants and must be dropped.
lab <- c("1.01" = "Entry into a Material Definitive Agreement",
         "9.01" = "Financial Statements and Exhibits",
         "8.01" = "Other Events", "2.02" = "Results of Operations and Financial Condition",
         "5" = "Other events", "7" = "Financial statements and exhibits",
         "9" = "Regulation FD Disclosure", "12" = "Results of Operations and Financial Condition",
         "7.01" = "Regulation FD Disclosure",
         "5.02" = "Departure of Directors or Certain Officers; Election of Directors: Comp Arr",
         "57" = "")
it <- function(...) paste(vapply(c(...), \(k) paste0("Item ", k, ": ", lab[[k]]), ""), collapse = "\n")

tab_landing <- tibble::tibble(
  HashIndex  = c("aaa", "bbb", "ccc", "ddd", "ddd", "eee", "eee", "fff", "ggg"),
  FilingDate = as.Date(c("2019-05-01", "2003-06-10", "1996-02-02",
                         "2015-01-09", "2015-01-09", "2016-06-02", "2016-06-02",
                         "1996-03-04", "2004-09-01")),
  Items      = c(it("1.01", "9.01", "8.01", "5.02"), it("5", "7", "9", "12"), it("1.01", "57"),
                 it("2.02"), it("2.02"), it("7.01"), it("8.01"), it("57"), it("5", "8.01"))
)

tab_one   <- itm_dedup_landing(.tab = tab_landing)
drop_     <- attr(tab_one, "Dropped")
tab_long  <- itm_items_long(.tab = tab_one)
tab_flags <- itm_item_flags(.long = tab_long)
tab_codes <- itm_code_table(.long = tab_long)

res <- c(res, ok("dedup collapses ddd", drop_[["RegistrantRows"]] == 1L, drop_[["RegistrantRows"]]))
res <- c(res, ok("dedup drops eee", drop_[["Disagreeing"]] == 2L, drop_[["Disagreeing"]]))
res <- c(res, ok("six filings survive", nrow(tab_one) == 6L, nrow(tab_one)))
res <- c(res, ok("14 item rows", nrow(tab_long) == 14L, nrow(tab_long)))

res <- c(res, ok("ItemOrder restarts per filing",
                 identical(tab_long$ItemOrder[tab_long$HashIndex == "aaa"], 1:4),
                 paste(tab_long$ItemOrder, collapse = " ")))
res <- c(res, ok("5.02 keeps its interior colon",
                 grepl("Election of Directors: Comp",
                       tab_long$ItemLabel[tab_long$ItemCode == "5.02"]), ""))
res <- c(res, ok("57 carries no era", all(is.na(tab_long$ItemEra[tab_long$ItemCode == "57"])), ""))

# -- The wide table -----------------------------------------------------------------------------
f <- function(.h) tab_flags[tab_flags$HashIndex == .h, ]
res <- c(res, ok("one row per filing", nrow(tab_flags) == 6L, nrow(tab_flags)))
res <- c(res, ok("aaa: 4 items, 1 voluntary",
                 f("aaa")$nItems == 4L && f("aaa")$nItemsVoluntary == 1L,
                 sprintf("%d items, %d vol", f("aaa")$nItems, f("aaa")$nItemsVoluntary)))
res <- c(res, ok("bbb: pre-reform 5/9/12 all count as voluntary",
                 f("bbb")$nItemsVoluntary == 3L, f("bbb")$nItemsVoluntary))
res <- c(res, ok("bbb: era is Pre", f("bbb")$ItemEra == "Pre", f("bbb")$ItemEra))
res <- c(res, ok("aaa: era is Post", f("aaa")$ItemEra == "Post", f("aaa")$ItemEra))
res <- c(res, ok("ccc: an unregistered code alongside 1.01 leaves the era Post",
                 f("ccc")$ItemEra == "Post", f("ccc")$ItemEra))
res <- c(res, ok("fff: a filing of only unregistered codes has no era",
                 is.na(f("fff")$ItemEra), f("fff")$ItemEra))
res <- c(res, ok("ggg: pre and post codes together are Mixed",
                 f("ggg")$ItemEra == "Mixed", f("ggg")$ItemEra))
res <- c(res, ok("ccc: one unknown kind", f("ccc")$nItemsUnknown == 1L, f("ccc")$nItemsUnknown))
res <- c(res, ok("bbb: ReportsItem202 fires on pre-reform code 12",
                 f("bbb")$ReportsItem202 == 1L, f("bbb")$ReportsItem202))
res <- c(res, ok("bbb: ReportsItem801 fires on pre-reform code 5",
                 f("bbb")$ReportsItem801 == 1L, f("bbb")$ReportsItem801))
res <- c(res, ok("bbb: ReportsItem901 fires on pre-reform code 7",
                 f("bbb")$ReportsItem901 == 1L, f("bbb")$ReportsItem901))
res <- c(res, ok("bbb: ReportsItem101 does not fire", f("bbb")$ReportsItem101 == 0L, ""))
res <- c(res, ok("aaa: ItemCodes preserves order",
                 f("aaa")$ItemCodes == "1.01|9.01|8.01|5.02", f("aaa")$ItemCodes))
res <- c(res, ok("nItemsDotted counts only dotted codes",
                 f("aaa")$nItemsDotted == 4L && f("bbb")$nItemsDotted == 0L, ""))

# -- Reports and checks -------------------------------------------------------------------------
res <- c(res, ok("code table is a full join over the vocabulary",
                 nrow(tab_codes) == 46L + 1L, nrow(tab_codes)))
res <- c(res, ok("unseen codes are flagged",
                 sum(tab_codes$Status == "registered, unseen") == 46L - 9L,
                 sum(tab_codes$Status == "registered, unseen")))
res <- c(res, ok("57 is the one unregistered",
                 identical(tab_codes$ItemCode[tab_codes$Status == "unregistered"], "57"), ""))
res <- c(res, ok("year edge handles all-missing",
                 is.na(itm_year_edge(.x = c(NA_integer_, NA_integer_))), ""))
res <- c(res, ok("label variants is empty", nrow(itm_label_variants(.long = tab_long)) == 0L, ""))
res <- c(res, ok("items per filing sums to the filings",
                 sum(itm_items_per_filing(.flags = tab_flags)$nFilings) == 6L, ""))

era <- itm_era_cross(.long = tab_long)
cross <- era[era$ItemEra != era$FiledSide, ]
res <- c(res, ok("era cross finds both crossers, ccc and ggg",
                 nrow(cross) == 2L && all(cross$nFilings == 1L) &&
                   setequal(cross$Codes, c("1.01", "5")),
                 paste(cross$ItemEra, cross$FiledSide, cross$Codes, collapse = "; ")))

chk <- itm_check_items(.long = tab_long, .flags = tab_flags, .codes = tab_codes)
res <- c(res, ok("57 in 1996 does not trip the windowed check",
                 chk$Pass[grepl("^No unregistered code", chk$Check)], ""))
res <- c(res, ok("unseen codes do trip their check",
                 !chk$Pass[chk$Check == "Every registered code is seen"], ""))
res <- c(res, ok("nItems agrees between the two tables",
                 chk$Pass[chk$Check == "nItems agrees with the long table"], ""))

# -- The date heuristic -------------------------------------------------------------------------
tab_txt <- tibble::tibble(
  DocID    = c("d1", "d2", "d3", "d4"),
  Outcome  = c("extracted", "extracted", "extracted", "no-body"),
  ItemText = c(
    "On March 3, 2015, the Company entered into a credit agreement.",
    paste("On March 3, 2015, the Company entered into a lease. On April 9, 2015,",
          "the Company entered into a second lease."),
    "On March 3, 2015, the Company entered into a lease. On March 3, 2015, it was amended.",
    NA_character_
  )
)
tab_dt <- itm_summary_dates(.tab = tab_txt)

res <- c(res, ok("one date counted once", tab_dt$nSummaryDates[1] == 1L, tab_dt$nSummaryDates[1]))
res <- c(res, ok("two distinct dates counted twice", tab_dt$nSummaryDates[2] == 2L, tab_dt$nSummaryDates[2]))
res <- c(res, ok("the same date twice counts once", tab_dt$nSummaryDates[3] == 1L, tab_dt$nSummaryDates[3]))
res <- c(res, ok("no text gives NA, not zero", is.na(tab_dt$nSummaryDates[4]), ""))
res <- c(res, ok("first date parses",
                 identical(tab_dt$SummaryDateFirst[1], as.Date("2015-03-03")),
                 as.character(tab_dt$SummaryDateFirst[1])))
res <- c(res, ok("SummaryIsSingle follows the count",
                 identical(tab_dt$SummaryIsSingle, c(1L, 0L, 1L, NA_integer_)),
                 paste(tab_dt$SummaryIsSingle, collapse = " ")))
res <- c(res, ok("date bands exclude failed outcomes",
                 sum(itm_summary_date_bands(.tab = tab_dt)$nDocs) == 3L, ""))

# -- The run arithmetic must equal the grouped summarise it replaced ----------------------------
set.seed(7)
n_e   <- 20000L
codes_e <- c(.itm_items$ItemCode, "57")
k_e   <- sample(1:5, n_e, TRUE)
long_e <- tibble::tibble(
  HashIndex  = rep(sprintf("h%07d", seq_len(n_e)), k_e),
  FilingDate = rep(as.Date("2000-01-01") + sample(0:8000, n_e, TRUE), k_e),
  ItemCode   = sample(codes_e, sum(k_e), TRUE)
) |>
  dplyr::left_join(dplyr::select(.itm_items, "ItemCode", "ItemEra", "ItemKind", "ItemFurnished"),
                   by = dplyr::join_by("ItemCode")) |>
  dplyr::arrange(.data$HashIndex)
long_e$Year      <- as.integer(format(long_e$FilingDate, "%Y"))
long_e$ItemOrder <- sequence(itm_runs(.x = long_e$HashIndex)$Lengths)

ref_e <- long_e |>
  dplyr::summarise(
    FilingDate      = dplyr::first(.data$FilingDate),
    nItems          = dplyr::n(),
    nItemsDotted    = sum(grepl(".", .data$ItemCode, fixed = TRUE)),
    nItemsVoluntary = sum(.data$ItemKind == "Voluntary", na.rm = TRUE),
    nItemsMandatory = sum(.data$ItemKind == "Mandatory", na.rm = TRUE),
    nItemsUnknown   = sum(is.na(.data$ItemKind)),
    nEraPost        = sum(.data$ItemEra == "Post", na.rm = TRUE),
    nEraPre         = sum(.data$ItemEra == "Pre", na.rm = TRUE),
    ItemCodes       = paste(.data$ItemCode, collapse = "|"),
    ReportsItem101  = as.integer(any(.data$ItemCode %in% "1.01")),
    ReportsItem202  = as.integer(any(.data$ItemCode %in% c("2.02", "12"))),
    ReportsItem701  = as.integer(any(.data$ItemCode %in% c("7.01", "9"))),
    ReportsItem801  = as.integer(any(.data$ItemCode %in% c("8.01", "5"))),
    ReportsItem901  = as.integer(any(.data$ItemCode %in% c("9.01", "7"))),
    .by             = "HashIndex") |>
  dplyr::mutate(ItemEra = dplyr::case_when(
    .data$nEraPost == 0L & .data$nEraPre == 0L ~ NA_character_,
    .data$nEraPre  == 0L                       ~ "Post",
    .data$nEraPost == 0L                       ~ "Pre",
    TRUE                                       ~ "Mixed")) |>
  dplyr::select(-"nEraPost", -"nEraPre") |>
  dplyr::relocate("ItemEra", .before = "ItemCodes")

got_e <- itm_item_flags(.long = long_e)

res <- c(res, ok("run arithmetic equals the grouped summarise",
                 isTRUE(all.equal(as.data.frame(ref_e), as.data.frame(got_e),
                                  check.attributes = FALSE)), ""))
res <- c(res, ok("the equivalence fixture covers all three eras",
                 setequal(stats::na.omit(got_e$ItemEra), c("Pre", "Post", "Mixed")),
                 paste(sort(unique(got_e$ItemEra)), collapse = " ")))
res <- c(res, ok("itm_runs aborts on non-contiguous input",
                 inherits(try(itm_runs(.x = c("a", "b", "a")), silent = TRUE), "try-error"), ""))
res <- c(res, ok("itm_items_long aborts on a duplicated HashIndex",
                 inherits(try(itm_items_long(.tab = tab_landing), silent = TRUE), "try-error"), ""))
res <- c(res, ok("run sums equal grouped sums on the small fixture",
                 identical(itm_run_sum(tab_long$ItemFurnished %in% 1L,
                                       .runs = itm_runs(.x = tab_long$HashIndex)),
                           as.integer(dplyr::summarise(tab_long,
                             n = sum(.data$ItemFurnished, na.rm = TRUE),
                             .by = "HashIndex")$n)), ""))

# -- The date parse must not depend on LC_TIME --------------------------------------------------
old_loc <- Sys.getlocale("LC_TIME")
de_ok   <- suppressWarnings(Sys.setlocale("LC_TIME", "de_DE.UTF-8"))
if (nzchar(de_ok)) {
  tab_de <- itm_summary_dates(.tab = tab_txt)
  res <- c(res, ok("first date parses under a German LC_TIME",
                   identical(tab_de$SummaryDateFirst[1], as.Date("2015-03-03")),
                   as.character(tab_de$SummaryDateFirst[1])))
  res <- c(res, ok("counts are unchanged under a German LC_TIME",
                   identical(tab_de$nSummaryDates, tab_dt$nSummaryDates), ""))
  res <- c(res, ok("the old %B route would have failed here",
                   is.na(as.Date("March 3, 2015", format = "%B %d, %Y")), ""))
  invisible(Sys.setlocale("LC_TIME", old_loc))
} else {
  cat("SKIP German locale unavailable\n")
}

# -- The heading strip and the examples table ---------------------------------------------------
# The refactor lifted two regexes out of itm_body_chars into constants. The test is equivalence with
# the implementation it replaced, not agreement with numbers typed out by hand -- which is how the
# first version of this check came to fail against a correct function.
itm_body_chars_old <- function(.text) {
  if (is.na(.text)) return(NA_integer_)
  r_ <- stringi::stri_replace_first_regex(
    .text, "(?i)^\\s*item\\s*\\d+\\s*\\.?\\s*\\d*\\s*[.:)-]*\\s*", "")
  r_ <- stringi::stri_replace_first_regex(
    r_, paste0("(?i)^\\s*[(\\[]?\\s*entry\\s+into\\s+(?:an?\\s+)?material\\s+definitive",
               "\\s+agreement\\s*[)\\]]?\\s*[.;:]*\\s*"), "")
  nchar(trimws(r_))
}

txt_bc <- c(
  "Item 1.01 Entry into a Material Definitive Agreement. On March 3, 2015, x.",
  "ITEM 1.01. Entry Into a Material Definitive Agreement On June 3, 2016, y.",
  "Item 1.01Entry into a Material Definitive Agreement. Effective July 14, 2009, z.",
  "Item 1.01 (Entry into an Material Definitive Agreement) The information in Item 3.02.",
  "Item 1.01 Entry into a Material Definitive Agreement",
  "Some text with no heading at all.",
  NA_character_
)
res <- c(res, ok("itm_body_chars is unchanged by the refactor",
                 identical(vapply(txt_bc, itm_body_chars, integer(1), USE.NAMES = FALSE),
                           vapply(txt_bc, itm_body_chars_old, integer(1), USE.NAMES = FALSE)),
                 paste(vapply(txt_bc, itm_body_chars, integer(1), USE.NAMES = FALSE),
                       collapse = " ")))

tab_ex <- tibble::tibble(
  DocID    = sprintf("d%02d", 1:8),
  Outcome  = c(rep("extracted", 7L), "no-body"),
  ItemText = c(
    "Item 1.01 Entry into a Material Definitive Agreement. On March 3, 2015, the Company signed.",
    "Item 1.01 Entry into a Material Definitive Agreement. On June 3, 2016, the Seller agreed.",
    "Item 1.01. Entry into a Material Definitive Agreement The information in Item 3.02 is incorporated.",
    "Item 1.01Entry into a Material Definitive Agreement. Effective July 14, 2009, Gabriel entered.",
    "Item 1.01 Entry into a Material Definitive Agreement. On May 22, 2017, a deal. On May 26, 2017, it closed.",
    "Item 1.01 Entry into a Material Definitive Agreement. Reference is made to Items 2.01 and 2.03.",
    "Item 1.01 Entry into a Material Definitive Agreement. On January 3, 2005, x. On April 19, 2005, y.",
    NA_character_)
)
tab_ex <- itm_summary_dates(.tab = tab_ex)
ex     <- itm_summary_examples(.tab = tab_ex, .n = 2L)

res <- c(res, ok("examples cover all three buckets",
                 dplyr::n_distinct(ex$Dates) == 3L, dplyr::n_distinct(ex$Dates)))
res <- c(res, ok("the zero bucket shows two dashes, not NA",
                 all(ex$Match[grepl("^0", ex$Dates)] == "--"), ""))
res <- c(res, ok("the failed outcome is excluded", nrow(ex) == 6L, nrow(ex)))
res <- c(res, ok("the heading is stripped from every preview",
                 !any(grepl("^Item 1.01", ex$Preview, ignore.case = TRUE)),
                 utils::head(ex$Preview, 1L)))
res <- c(res, ok("match and preview come from the same document",
                 all(mapply(function(.m, .p) .m == "--" ||
                              all(vapply(strsplit(.m, " / ")[[1]], grepl, logical(1), x = .p,
                                         fixed = TRUE)),
                            ex$Match, ex$Preview)), ""))
res <- c(res, ok("a fixed seed makes the table stable",
                 identical(ex, itm_summary_examples(.tab = tab_ex, .n = 2L)), ""))

cat("\n", sum(res), "/", length(res), " passed\n", sep = "")
if (!all(res)) quit(status = 1L)
