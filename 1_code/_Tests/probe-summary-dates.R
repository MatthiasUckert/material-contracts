# ======================================================================================================================
# PROBE -- why 45,235 recovered summaries name no agreement date
# ======================================================================================================================
#
# WHAT THIS ANSWERS. The published filter keeps summaries naming exactly one date and drops 25.4% of
# what was recovered, of which two thirds is the zero bucket rather than genuine multiple agreements.
# Zero can mean two very different things: the summary names no date, or it names one in a form the
# pattern does not read. Only the second is ours to fix, and the two are indistinguishable from the
# count alone.
#
# HOW. A cascade of ever-broader patterns run over the zero bucket. Each is reported alone and
# cumulatively, so the table says how much of the gap each relaxation would close. The last two are
# not candidate patterns at all -- "any month-name date" and "any four-digit year" are the diagnostic:
# if the zero bucket is full of dates the base pattern cannot see, the anchor is the problem; if it
# is not, the summaries genuinely open some other way and no widening will help.
#
# IT WRITES NOTHING and changes nothing. Set the two constants below if the paths differ.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; pure ASCII.

if (!exists(".PATH_ITEM101")) {
  .PATH_ITEM101 <- if (requireNamespace("here", quietly = TRUE)) {
    here::here("2_output", "01D-GetItems", "Output", "Item101.parquet")
  } else {
    "2_output/01D-GetItems/Output/Item101.parquet"
  }
}

# SAMPLED BY DEFAULT. The text column is on the order of a gigabyte and this is a diagnostic: twenty
# thousand summaries put every share below within half a point. Set to Inf to read the lot.
if (!exists(".N_SAMPLE")) .N_SAMPLE <- 20000L

# 1. The patterns ------------------------------------------------------------------------------------------------------

.mon_full <- paste(month.name, collapse = "|")
.mon_abbr <- paste(month.abb, collapse = "|")

#: THE CASCADE, BROADEST LAST. Order matters only for the cumulative column; each Alone figure is
#: independent. Base is what 01D ships and what the published filter used.
.sd_patterns <- tibble::tribble(
  ~Kind,        ~Name,             ~Pattern,
  "candidate",  "base: On Month D, YYYY",
  paste0("\\bOn\\s+(", .mon_full, ")\\s+(\\d{1,2}),\\s*(\\d{4})"),

  "candidate",  "lowercase on",
  paste0("(?i)\\bon\\s+(", .mon_full, ")\\s+\\d{1,2},\\s*\\d{4}"),

  "candidate",  "on or about",
  paste0("(?i)\\bon\\s+or\\s+about\\s+(", .mon_full, ")\\s+\\d{1,2},\\s*\\d{4}"),

  "candidate",  "effective / dated / as of",
  paste0("(?i)\\b(?:effective|dated|as\\s+of)\\s+(?:as\\s+of\\s+)?(", .mon_full, ")\\s+\\d{1,2},\\s*\\d{4}"),

  "candidate",  "abbreviated month",
  paste0("(?i)\\b(", .mon_abbr, ")\\.?\\s+\\d{1,2},\\s*\\d{4}"),

  "candidate",  "ordinal day",
  paste0("(?i)\\b(", .mon_full, ")\\s+\\d{1,2}(?:st|nd|rd|th),?\\s*\\d{4}"),

  "candidate",  "no comma before the year",
  paste0("(?i)\\b(", .mon_full, ")\\s+\\d{1,2}\\s+\\d{4}"),

  "candidate",  "day before month",
  paste0("(?i)\\b\\d{1,2}\\s+(", .mon_full, "),?\\s*\\d{4}"),

  "candidate",  "Nth day of Month",
  paste0("(?i)\\b\\d{1,2}(?:st|nd|rd|th)\\s+day\\s+of\\s+(", .mon_full, ")"),

  "candidate",  "numeric, D/M/YYYY",
  "\\b\\d{1,2}/\\d{1,2}/\\d{2,4}\\b",

  "diagnostic", "DIAGNOSTIC: any month-name date",
  paste0("(?i)\\b(", .mon_full, "|", .mon_abbr, ")\\.?\\s+\\d{1,2}(?:st|nd|rd|th)?,?\\s*\\d{4}"),

  "diagnostic", "DIAGNOSTIC: any four-digit year",
  "\\b(?:19|20)\\d{2}\\b"
)

# 2. Pure functions ----------------------------------------------------------------------------------------------------

#' Which texts each pattern matches, alone and cumulatively
#'
#' @param .x Character. Summary texts, already restricted to the bucket of interest.
#' @param .tab The pattern cascade.
#' @return A tibble: Name, nAlone, pAlone, nCumulative, pCumulative, nNew.
sd_coverage <- function(.x, .tab = .sd_patterns) {
  if (FALSE) .x <- txt_zero

  hits_ <- lapply(.tab$Pattern, function(.p) stringi::stri_detect_regex(.x, .p))

  # THE CUMULATIVE RUNS OVER CANDIDATES ONLY. The two diagnostic rows are far broader than anything
  # anyone would ship -- "any four-digit year" matches a summary mentioning a fiscal year and no
  # agreement date at all -- so folding them into a running union would report a coverage no
  # candidate pattern achieves.
  is_cand_         <- .tab$Kind == "candidate"
  cum_             <- Reduce(f = `|`, x = hits_[is_cand_], accumulate = TRUE)
  n_cum_           <- rep(NA_integer_, nrow(.tab))
  n_cum_[is_cand_] <- vapply(cum_, sum, integer(1L))

  tibble::tibble(
    Kind        = .tab$Kind,
    Name        = .tab$Name,
    nAlone      = vapply(hits_, sum, integer(1L)),
    nCumulative = n_cum_
  ) |>
    dplyr::mutate(
      nNew        = .data$nCumulative - dplyr::lag(.data$nCumulative, default = 0L),
      pAlone      = round(.data$nAlone / length(.x) * 100, 1),
      pCumulative = round(.data$nCumulative / length(.x) * 100, 1)
    ) |>
    dplyr::select("Kind", "Name", "nAlone", "pAlone", "nNew", "nCumulative", "pCumulative")
}

#' A readable preview of one summary, with the opening kept
#'
#' @param .x Character.
#' @param .chars Integer. Characters to keep.
#' @return Character, whitespace collapsed.
sd_preview <- function(.x, .chars = 150L) {
  if (FALSE) .x <- "On March 3, 2015, the Company entered into a Credit Agreement."

  stringi::stri_replace_all_regex(.x, "\\s+", " ") |>
    trimws() |>
    stringi::stri_sub(1L, .chars)
}

#' Examples of what a pattern finds, and what it leaves
#'
#' @param .x Character. Summary texts.
#' @param .pattern Character. One regular expression.
#' @param .n Integer. Examples of each kind.
#' @param .chars Integer. Preview length.
#' @param .seed Integer. Sampling seed, so the page is the same on a re-render.
#' @return A tibble: Which, Match, Preview.
sd_examples <- function(.x, .pattern, .n = 6L, .chars = 130L, .seed = 42L) {
  if (FALSE) .x <- txt_zero

  set.seed(.seed)
  hit_ <- stringi::stri_detect_regex(.x, .pattern)

  pick_ <- function(.idx, .lab) {
    if (length(.idx) == 0L) return(tibble::tibble())
    take_ <- sample(.idx, min(.n, length(.idx)))
    tibble::tibble(
      Which   = .lab,
      Match   = stringi::stri_extract_first_regex(.x[take_], .pattern),
      Preview = sd_preview(.x = .x[take_], .chars = .chars)
    )
  }

  dplyr::bind_rows(
    pick_(.idx = which(hit_),  .lab = "would be caught"),
    pick_(.idx = which(!hit_), .lab = "still missed")
  )
}

# 3. Reading -----------------------------------------------------------------------------------------------------------

#' Read the summaries, sampled
#'
#' TWO PASSES BECAUSE THE TEXT IS THE EXPENSIVE COLUMN. The first reads keys only and picks the
#' sample; the second reads text for those alone. Reading everything and sampling afterwards would
#' pull the gigabyte this exists to avoid.
#'
#' @param .path Path to Item101.parquet.
#' @param .n Integer. Sample size per bucket, or Inf for all.
#' @return A tibble: DocID, Outcome, nSummaryDates, ItemText.
sd_read <- function(.path, .n = .N_SAMPLE) {
  if (FALSE) .path <- .PATH_ITEM101

  keys_ <- arrow::open_dataset(sources = .path) |>
    dplyr::select("DocID", "Outcome", "nSummaryDates") |>
    dplyr::collect() |>
    dplyr::filter(!is.na(.data$nSummaryDates))

  set.seed(1L)
  take_ <- keys_ |>
    dplyr::mutate(Bucket = dplyr::case_when(
      .data$nSummaryDates == 0L ~ "zero",
      .data$nSummaryDates == 1L ~ "one",
      TRUE                      ~ "many"
    )) |>
    dplyr::slice_sample(n = min(.n, nrow(keys_)), by = "Bucket")

  arrow::open_dataset(sources = .path) |>
    dplyr::select("DocID", "Outcome", "nSummaryDates", "ItemText") |>
    dplyr::filter(.data$DocID %in% take_$DocID) |>
    dplyr::collect()
}

# 4. Runner ------------------------------------------------------------------------------------------------------------

.sd_width_old <- getOption("width")
options(width = 165)

cli::cli_h1("Item 1.01 summary dates: what the zero bucket contains")

if (!requireNamespace("arrow", quietly = TRUE) || !file.exists(.PATH_ITEM101)) {
  cli::cli_alert_warning("Not found, running on the fixture instead: {.path {(.PATH_ITEM101)}}")
  tab_sum <- tibble::tibble(
    DocID    = sprintf("d%02d", 1:11),
    Outcome  = "extracted",
    ItemText = c(
      "On March 3, 2015, the Company entered into a Credit Agreement with the Lenders named therein.",
      "The Company entered into a lease on April 9, 2015 with a subsidiary of the Landlord.",
      "On or about June 1, 2016, the Registrant completed a private placement of senior notes.",
      "Effective May 12, 2017, the Company and the Executive entered into an amended agreement.",
      "On Mar. 3, 2015, the Company entered into an underwriting agreement with the Underwriters.",
      "On March 3rd, 2015, the Company entered into a settlement with the plaintiff.",
      "On March 3 2015 the Company entered into a purchase agreement for certain assets.",
      "On 3 March 2015, the Issuer entered into a deed of amendment with the Trustee.",
      "This Agreement, made this 15th day of July, 2018, by and between the parties hereto.",
      "On 3/3/2015, the Company entered into a note purchase agreement.",
      "The Company amended its revolving credit facility to increase the borrowing base."
    )
  )
  tab_sum$nSummaryDates <- as.integer(stringi::stri_count_regex(
    tab_sum$ItemText, .sd_patterns$Pattern[1L]
  ))
} else {
  cli::cli_alert_info("Reading {.path {(.PATH_ITEM101)}}")
  tab_sum <- sd_read(.path = .PATH_ITEM101, .n = .N_SAMPLE)
  cli::cli_alert_info("{format(nrow(tab_sum), big.mark = ',')} summaries sampled")
}

tab_sum <- dplyr::filter(tab_sum, !is.na(.data$ItemText))

txt_zero <- tab_sum$ItemText[tab_sum$nSummaryDates == 0L]
txt_one  <- tab_sum$ItemText[tab_sum$nSummaryDates == 1L]
txt_many <- tab_sum$ItemText[tab_sum$nSummaryDates >= 2L]

cli::cli_alert_info(paste0(
  "zero {length(txt_zero)}, one {length(txt_one)}, two or more {length(txt_many)}"
))

cli::cli_h2("What a wider pattern would rescue, within the zero bucket")

if (length(txt_zero) == 0L) {
  cli::cli_alert_warning("The zero bucket is empty in this sample.")
} else {
  sd_coverage(.x = txt_zero) |>
    as.data.frame() |>
    print(row.names = FALSE, right = FALSE)

  cli::cli_text("")
  cli::cli_alert_info(paste0(
    "nAlone is that pattern on its own; nNew is what it adds to everything above it. ",
    "The two DIAGNOSTIC rows are not candidates -- they say whether a date is there at all."
  ))
}

cli::cli_h2("Zero bucket: what the widest month-name pattern catches, and what it does not")

if (length(txt_zero) > 0L) {
  sd_examples(
    .x       = txt_zero,
    .pattern = .sd_patterns$Pattern[.sd_patterns$Name == "DIAGNOSTIC: any month-name date"],
    .n       = 8L
  ) |>
    as.data.frame() |>
    print(row.names = FALSE, right = FALSE)
}

cli::cli_h2("For comparison: summaries the base pattern already reads")

# SAMPLED ONCE PER BLOCK, not once per column. Drawing the sample twice -- once to extract the match
# and once to build the preview -- returns two different sets of documents and prints a match beside
# the wrong text, which looks entirely plausible and is entirely wrong.
sd_show <- function(.x, .lab, .n = 5L, .seed = 7L) {
  if (length(.x) == 0L) return(tibble::tibble())

  set.seed(.seed)
  take_ <- .x[sample(seq_along(.x), min(.n, length(.x)))]

  tibble::tibble(
    Which   = .lab,
    Match   = vapply(
      X         = take_,
      FUN       = function(.t) paste(unique(stringi::stri_extract_all_regex(
        .t, .sd_patterns$Pattern[1L], omit_no_match = TRUE
      )[[1L]]), collapse = " / "),
      FUN.VALUE = character(1L),
      USE.NAMES = FALSE
    ),
    Preview = sd_preview(.x = take_)
  )
}

dplyr::bind_rows(
  sd_show(.x = txt_one,  .lab = "exactly one"),
  sd_show(.x = txt_many, .lab = "two or more")
) |>
  as.data.frame() |>
  print(row.names = FALSE, right = FALSE)

options(width = .sd_width_old)
cli::cli_alert_success("Probe complete")
