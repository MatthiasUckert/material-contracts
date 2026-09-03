# check-v6-edgar-corpus.R -------------------------------------------------------------------------
#
# The V6 item ladder against an INDEPENDENT parser, filing by filing.
#
# EDGAR-CORPUS (Loukas et al. 2021) split every 10-K from 1993-2020 into items with its own
# parser, never having seen ours. On 2005 alone, side by side rather than joined, it found Item 2
# in 93.4% of filings against our 18.1%, and Item 1A in 68.6% against our 67.0%. This script does
# the join, so the comparison is on the SAME filings, split by parse route.
#
# WHAT "FOUND" MEANS ON EACH SIDE.
#   ours    HasI<x>, the header flag. Reads the ladder, not nPar > 0.
#   theirs  nw_<x> > .MIN_WORDS. Their card says the corpus "needs further cleaning", and the 2005
#           sample showed a filing with 6 words in Item 1 - so nw > 0 is not "found".
#
# THE JOIN. Their `year` is the EDGAR filing year (EDGAR-CRAWLER walks the full-index by filing
# date), and reports.filed_year is exactly that. cik + filed_year, one filing per cik-year on their
# side by construction of their filename.
#
# WORD COUNTS ARE NOT COMPARABLE ON EQUALITY. Theirs are whitespace tokens; ours are prose words
# from 002g's tokenizer. Where both find the item, agreement is measured by rank correlation and
# by the ratio's median, not by difference.

source("1_code/002g-GetDisclosures-V6.R", encoding = "UTF-8")
source("1_code/058-AllVariables-V6.R",    encoding = "UTF-8")

.PATH_PANEL <- here::here("2_output", "002g-GetDisclosures-V6", "Output",
                          "pfire_panel_v6.parquet")
.PATH_STORE <- here::here("2_output", "002g-GetDisclosures-V6", "Store",
                          "pfire_para_v5.duckdb")
.PATH_EC    <- here::here("2_output", "edgar-corpus", "edgar_corpus_sections.parquet")

.ITEMS     <- c("1", "1A", "2", "3", "7", "7A", "8")
.MIN_WORDS <- 50L     # their side: fewer than this is a parse fragment, not the item


# 1 · Three sources ---------------------------------------------------------------------------------

cli::cli_h1(text = "1 - sources")

.con <- v5_connect(.path_db = .PATH_STORE, .read_only = TRUE)
.rep <- DBI::dbGetQuery(
  conn      = .con,
  statement = "SELECT doc_id, cik, filed_year, route, form_type FROM reports"
) |>
  tibble::as_tibble()
DBI::dbDisconnect(conn = .con, shutdown = TRUE)
cli::cli_alert_info(text = "reports: {scales::comma(x = nrow(x = .rep))} document{?s}")

.spec <- v6_spec_names()
.ours <- arrow::open_dataset(sources = .PATH_PANEL) |>
  dplyr::select(dplyr::all_of(x = c("gvkey", "datadate", "doc_id", "HasDoc",
                                     paste0("HasI", .ITEMS), paste0("nWords_I", .ITEMS)))) |>
  dplyr::collect() |>
  dplyr::filter(HasDoc) |>
  dplyr::inner_join(y = .rep, by = "doc_id")
cli::cli_alert_info(
  text = "panel with a document and a reports row: {scales::comma(x = nrow(x = .ours))}"
)

.ec <- arrow::read_parquet(file = .PATH_EC) |>
  dplyr::mutate(
    cik        = as.integer(x = cik),
    filed_year = as.integer(x = year),
    ec_route   = dplyr::if_else(
      condition = stringi::stri_endswith_fixed(str = filename, pattern = ".htm"),
      true = "html", false = "text"
    )
  )
cli::cli_alert_info(
  text = "EDGAR-CORPUS: {scales::comma(x = nrow(x = .ec))} filing{?s}, \\
          {min(.ec$filed_year)}-{max(.ec$filed_year)}"
)

dup_ <- .ec |> dplyr::count(cik, filed_year) |> dplyr::filter(n > 1L)
if (nrow(x = dup_) > 0) {
  cli::cli_alert_warning(
    text = "{nrow(x = dup_)} cik-year{?s} appear more than once on their side - first kept"
  )
  .ec <- dplyr::slice_head(.data = .ec, n = 1L, by = c(cik, filed_year))
}


# 2 · The join -------------------------------------------------------------------------------------

cli::cli_h1(text = "2 - join on cik + filed_year")

.j <- dplyr::inner_join(
  x  = .ours,
  y  = dplyr::select(.data = .ec, cik, filed_year, filename, ec_route,
                     dplyr::all_of(x = paste0("nw_", .ITEMS))),
  by = c("cik", "filed_year")
)

cli::cli_alert_success(
  text = "{scales::comma(x = nrow(x = .j))} filing{?s} matched - \\
          {round(x = 100 * nrow(x = .j) / sum(.ours$filed_year <= max(.ec$filed_year)), digits = 1)}% \\
          of ours inside their year range"
)

cli::cli_h3(text = "unmatched, by form type - what their corpus does not carry")
.ours |>
  dplyr::filter(filed_year <= max(.ec$filed_year)) |>
  dplyr::anti_join(y = .ec, by = c("cik", "filed_year")) |>
  dplyr::count(form_type, name = "NUnmatched") |>
  dplyr::arrange(dplyr::desc(NUnmatched)) |>
  utils::head(n = 8L) |>
  as.data.frame() |>
  print(row.names = FALSE)

cli::cli_h3(text = "route: do the two sides agree on which parse it was?")
.j |>
  dplyr::count(route, ec_route, name = "N") |>
  as.data.frame() |>
  print(row.names = FALSE)


# 3 · Agreement, item by item, route by route ------------------------------------------------------
# Four cells. `both` and `neither` are agreement. `theirs_only` is where OUR ladder missed an item
# an independent parser found. `ours_only` is the reverse, and is worth reading too.

cli::cli_h1(text = "3 - agreement")

.long <- purrr::map(
  .x = .ITEMS,
  .f = \(.it) {
    tibble::tibble(
      Item   = .it,
      doc_id = .j$doc_id,
      cik    = .j$cik,
      filename = .j$filename,
      route  = .j$route,
      filed_year = .j$filed_year,
      Ours   = dplyr::coalesce(.j[[paste0("HasI", .it)]], FALSE),
      Theirs = dplyr::coalesce(.j[[paste0("nw_", .it)]], 0L) > .MIN_WORDS,
      NwOurs = .j[[paste0("nWords_I", .it)]],
      NwTheirs = .j[[paste0("nw_", .it)]]
    )
  }
) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    Cell = dplyr::case_when(Ours & Theirs ~ "both", !Ours & !Theirs ~ "neither",
                            Theirs ~ "theirs_only", .default = "ours_only"),
    Item = factor(x = Item, levels = .ITEMS)
  )

cli::cli_h3(text = "% of matched filings in each cell - ALL routes")
.long |>
  dplyr::count(Item, Cell) |>
  dplyr::mutate(.by = Item, Pct = round(x = 100 * n / sum(n), digits = 1)) |>
  dplyr::select(-n) |>
  tidyr::pivot_wider(names_from = Cell, values_from = Pct, values_fill = 0) |>
  dplyr::arrange(Item) |>
  as.data.frame() |>
  print(row.names = FALSE)

cli::cli_h3(text = "theirs_only, % - THE LADDER'S MISS RATE against an independent parser")
.long |>
  dplyr::summarise(.by = c(Item, route),
                   N = dplyr::n(),
                   PctOursFound   = round(x = 100 * mean(x = Ours), digits = 1),
                   PctTheirsFound = round(x = 100 * mean(x = Theirs), digits = 1),
                   PctTheirsOnly  = round(x = 100 * mean(x = Cell == "theirs_only"), digits = 1),
                   PctOursOnly    = round(x = 100 * mean(x = Cell == "ours_only"), digits = 1)) |>
  dplyr::arrange(Item, route) |>
  as.data.frame() |>
  print(row.names = FALSE)


# 4 · Where both find it, do they find the SAME thing? ---------------------------------------------

cli::cli_h1(text = "4 - size agreement where both found the item")

.long |>
  dplyr::filter(Cell == "both", !is.na(x = NwOurs), NwOurs > 0, NwTheirs > 0) |>
  dplyr::summarise(
    .by      = c(Item, route),
    N        = dplyr::n(),
    Spearman = round(x = stats::cor(x = NwOurs, y = NwTheirs, method = "spearman"), digits = 3),
    MedRatio = round(x = stats::median(x = NwOurs / NwTheirs), digits = 2),
    P10Ratio = round(x = unname(obj = stats::quantile(x = NwOurs / NwTheirs, probs = 0.10)),
                     digits = 2),
    P90Ratio = round(x = unname(obj = stats::quantile(x = NwOurs / NwTheirs, probs = 0.90)),
                     digits = 2)
  ) |>
  dplyr::arrange(Item, route) |>
  as.data.frame() |>
  print(row.names = FALSE)

cli::cli_alert_info(
  text = "Spearman near 1 and a ratio near 1 with a tight P10-P90 means the two parsers put the \\
          item boundary in the same place. A ratio far from 1 with Spearman still high means one \\
          side's tokenizer, not its boundary."
)


# 5 · The misses, by year - is it the route, or the years? ----------------------------------------

cli::cli_h1(text = "5 - theirs_only by filing year, Item 2 and Item 1")

.long |>
  dplyr::filter(Item %in% c("1", "2", "7")) |>
  dplyr::summarise(.by = c(Item, filed_year),
                   N = dplyr::n(),
                   PctHtml = round(x = 100 * mean(x = route == "html"), digits = 0),
                   TheirsOnly = round(x = 100 * mean(x = Cell == "theirs_only"), digits = 1)) |>
  dplyr::select(-N) |>
  tidyr::pivot_wider(names_from = Item, values_from = TheirsOnly, names_prefix = "I") |>
  dplyr::arrange(filed_year) |>
  as.data.frame() |>
  print(row.names = FALSE)


# 6 · Twenty filings to open ----------------------------------------------------------------------
# Item 2, html, they found a substantial section and we found nothing. Their text is in the JSONL
# on disk under 0_data/edgar-corpus/<year>/, keyed on filename; ours is in the para store keyed on
# doc_id. Open both for the same filing and the reason is usually visible.

cli::cli_h1(text = "6 - Item 2 misses worth opening")

.long |>
  dplyr::filter(Item == "2", Cell == "theirs_only", route == "html") |>
  dplyr::arrange(dplyr::desc(NwTheirs)) |>
  dplyr::select(filed_year, cik, filename, doc_id, NwTheirs) |>
  utils::head(n = 20L) |>
  as.data.frame() |>
  print(row.names = FALSE)

cli::cli_h1(text = "done")
