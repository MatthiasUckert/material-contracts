# ======================================================================================================================
# PROBE: is the 8-K item list populated before the 2004 taxonomy reform?
# ======================================================================================================================
#
# WHY THIS RUNS BEFORE ANYTHING IS WRITTEN. The response to referee 2's comment 6A asserts that the
# decline in voluntary item counts "is largest for firms whose pre-period 8-Ks were disproportionately
# Item 5 filings -- which is the treated group by construction". That is not a construction, it is an
# empirical claim about the pre-2004 data, and the whole decision to abandon the design rests on it.
#
# THE CLAIM MAY POINT THE WRONG WAY, WHICH IS WHY IT IS WORTH THE TWENTY MINUTES. Before August 2004
# Item 5 was "Other Events", the catch-all, and no Item 1.01 existed. A firm that voluntarily
# disclosed a material contract by 8-K in that period had nowhere else to file it -- and that firm is
# the CONTROL group, because it was already using 8-Ks for contracts. The treated firm, which was not,
# files 8-Ks for earnings, auditor changes and acquisitions: the mandatory items. So the prior runs
# opposite to the memo's sentence.
#
# THIS PROBE DOES NOT TEST THAT. It tests whether the test is possible at all: whether the item list
# exists for filings made before the reform. If it does not, the check cannot be run as specified and
# the memo softens to "we cannot rule out that the taxonomy change drives both signs" -- which is
# still enough to justify abandoning the design, and is an outcome rather than a failure.
#
# READ-ONLY. Opens parquet, writes no data, and touches no cache. The one thing it does create is a
# directory: init_create_script_dir() resolves 01C's output path through fs::dir_create(), which is a
# no-op on a directory that already holds the parquets being read. Saying so is cheaper than leaving
# a reader to check.

source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")

options(cli.num_colors = 1, cli.width = 120)


# 1. What did 01C actually produce -------------------------------------------------------------------------------------
# THE DIRECTORY IS LISTED RATHER THAN A FILE NAME GUESSED. 01C writes more than one landing table --
# one restricted to retrieved filings and one holding every filing seen -- and which of them carries
# the pre-2004 rows is the question, not an assumption. A probe that opens the wrong one answers
# confidently and wrongly.

dir_01c_ <- init_create_script_dir(.dir_here = here::here(), .name_script = "01C-EdgarMetaData")

files_ <- fs::dir_ls(dir_01c_, recurse = TRUE, glob = "*.parquet")

cli::cli_h1("What 01C wrote")
tibble::tibble(
  File = as.character(fs::path_rel(files_, dir_01c_)),
  MB   = round(as.numeric(fs::file_size(files_)) / 1024^2, 1)
) |>
  dplyr::arrange(dplyr::desc(.data$MB)) |>
  print(n = 30)


# 2. The schema, read from Arrow and never inferred ----------------------------------------------------------------------
# 02B shapes the register with relocate(any_of(...)), which silently ignores a name that is not there,
# so a writer's column list is a statement of intent rather than of fact. Every column this probe
# needs is confirmed against the file before it is asked for.

schema_ <- function(.path) {
  ds_ <- arrow::open_dataset(sources = .path)
  tibble::tibble(Column = names(ds_), Type = purrr::map_chr(ds_$schema$fields, \(.f) .f$type$ToString()))
}

land_ <- files_[grepl("Landing", files_, ignore.case = TRUE)]
if (length(land_) == 0L) cli::cli_abort("No landing table under {.path {(dir_01c_)}}.")

for (.p in land_) {
  cli::cli_h2("Schema: {(fs::path_file(.p))}")
  sch_ <- schema_(.path = .p)
  print(sch_, n = 60)
  n_ <- arrow::open_dataset(sources = .p) |> dplyr::summarise(N = dplyr::n()) |> dplyr::collect()
  cli::cli_alert_info("{format(n_$N[[1L]], big.mark = ',', scientific = FALSE)} rows.")
}


# 3. Pick the widest landing table that carries an item list ---------------------------------------------------------
# THE ONE WITH THE MOST ROWS IS THE RIGHT ONE HERE, and that is a statement about this question rather
# than a general rule. The question is about every 8-K a firm filed before 2004, not only the ones
# that turned out to carry an Exhibit 10 -- restricting to retrieved filings would condition the
# pre-period on exactly the behaviour being measured.

has_items_ <- purrr::keep(land_, \(.p) "Items" %in% names(arrow::open_dataset(sources = .p)))
if (length(has_items_) == 0L) cli::cli_abort("No landing table carries {.field Items}.")

rows_     <- purrr::map_dbl(has_items_, \(.p) {
  arrow::open_dataset(sources = .p) |> dplyr::summarise(N = dplyr::n()) |> dplyr::collect() |> _$N[[1L]]
})
path_use_ <- has_items_[[which.max(rows_)]]

cli::cli_h1("Using {(fs::path_file(path_use_))}")

cols_have_ <- names(arrow::open_dataset(sources = path_use_))
col_form_  <- intersect(c("FormType", "Type"), cols_have_)[1L]
col_date_  <- intersect(c("DateFiled", "FilingDate", "Date"), cols_have_)[1L]

if (is.na(col_form_) || is.na(col_date_)) {
  cli::cli_abort(c(
    "x" = "Need a form column and a date column; found form = {(col_form_)}, date = {(col_date_)}.",
    "i" = "The file carries: {(cols_have_)}."
  ))
}
cli::cli_alert_info("Form column {.field {col_form_}}, date column {.field {col_date_}}.")


# 4. Coverage of the item list by year, for 8-K filings only ---------------------------------------------------------
# THE GATE. An item list that only begins in late 2004 makes the pre-period check impossible, and the
# 2004Q3 boundary is where the answer will show: the reform took effect on 23 August 2004.
#
# MISSINGNESS IS COUNTED, NOT DROPPED. itm_filing_items() filters !is.na(Items) before it does
# anything else, which is right for its purpose and would make the only quantity this probe cares
# about invisible.

tab_ <- arrow::open_dataset(sources = path_use_) |>
  dplyr::select(dplyr::all_of(c(col_form_, col_date_, "Items"))) |>
  dplyr::rename(FormType = dplyr::all_of(col_form_), DateFiled = dplyr::all_of(col_date_)) |>
  dplyr::filter(grepl("^8-K", .data$FormType)) |>
  dplyr::collect() |>
  dplyr::mutate(
    Year     = as.integer(substr(as.character(.data$DateFiled), 1L, 4L)),
    HasItems = !is.na(.data$Items) & nzchar(.data$Items)
  ) |>
  dplyr::filter(!is.na(.data$Year), .data$Year >= 1996L, .data$Year <= 2024L)

cli::cli_h1("Does the item list exist before the reform")

tab_ |>
  dplyr::summarise(
    nFilings  = dplyr::n(),
    nWithItem = sum(.data$HasItems),
    .by       = "Year"
  ) |>
  dplyr::mutate(ShareWithItem = round(.data$nWithItem / .data$nFilings, 3)) |>
  dplyr::arrange(.data$Year) |>
  print(n = 40)

pre_  <- dplyr::filter(tab_, .data$Year <= 2003L)
post_ <- dplyr::filter(tab_, .data$Year >= 2005L)

cli::cli_h2("Verdict")
cli::cli_alert_info(
  "Pre-2004: {format(nrow(pre_), big.mark = ',', scientific = FALSE)} 8-K filings, \\
   {round(100 * mean(pre_$HasItems), 1)}% carrying an item list."
)
cli::cli_alert_info(
  "Post-2004: {format(nrow(post_), big.mark = ',', scientific = FALSE)} 8-K filings, \\
   {round(100 * mean(post_$HasItems), 1)}% carrying an item list."
)

if (mean(pre_$HasItems) < 0.5) {
  cli::cli_alert_danger(
    "The pre-period item list is mostly absent. The Item 5 check cannot be run as specified, and \\
     the memo softens rather than asserts."
  )
} else {
  cli::cli_alert_success(
    "The pre-period item list is present. The Item 5 check is runnable; the treated/control split \\
     is the next question."
  )
}


# 5. What the codes actually look like -------------------------------------------------------------------------------
# THE FORMAT DECIDES THE PARSING, AND THE PARSING IS WHERE THIS GOES WRONG QUIETLY. Codes are one
# string per filing, separated; "5" pre-reform and "5.02" post-reform are different items that a
# substring test cannot tell apart, and "1.05" contains a 5 while meaning nothing of the sort. Splitting
# and comparing exactly is the only safe read, and that needs the separator confirmed rather than assumed.

cli::cli_h1("What an item list looks like")

show_ <- function(.tab, .label, .n = 8L) {
  hit_ <- dplyr::filter(.tab, .data$HasItems)
  if (nrow(hit_) == 0L) {
    cli::cli_alert_warning("{(.label)}: nothing with an item list.")
    return(invisible(NULL))
  }
  cli::cli_h2("{(.label)}")
  hit_ |>
    dplyr::slice_sample(n = min(.n, nrow(hit_))) |>
    dplyr::mutate(Items = substr(.data$Items, 1L, 90L)) |>
    dplyr::select("Year", "Items") |>
    print(n = .n)
}

set.seed(42L)
show_(.tab = pre_,  .label = "Pre-2004 filings")
show_(.tab = post_, .label = "Post-2004 filings")

codes_ <- function(.tab, .label) {
  hit_ <- dplyr::filter(.tab, .data$HasItems)
  if (nrow(hit_) == 0L) return(invisible(NULL))
  cli::cli_h2("Item codes, {(.label)}")
  strsplit(hit_$Items, "[|,;\n]") |>
    unlist() |>
    trimws() |>
    (\(.x) .x[nzchar(.x)])() |>
    tibble::tibble(Code = _) |>
    dplyr::count(.data$Code, name = "N", sort = TRUE) |>
    dplyr::mutate(Share = round(.data$N / sum(.data$N), 3)) |>
    utils::head(20L) |>
    print(n = 20)
}

codes_(.tab = pre_,  .label = "pre-2004")
codes_(.tab = post_, .label = "post-2004")

cli::cli_rule()
cli::cli_alert_info("Probe complete. Nothing was written.")
