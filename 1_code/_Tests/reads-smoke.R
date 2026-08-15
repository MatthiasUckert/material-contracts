# reads-smoke: one document through the span viewer ----
#
# WHAT THIS IS
# The single-document test for _Commons/_Reads.R, before it is pointed at four thousand of them.
# It builds its candidate table from the Check-NER-* parquets rather than from the store, which is
# the whole reason the renderer takes a tibble: those files already carry the extras, and the store
# does not yet.
#
# WHAT TO LOOK AT, in the file it opens
#   TWO TIERS OF TABS. The top row is the LABEL and the second row is the engines that emit it, so
#   GPE offers lexnlp, the gazetteer and the four spaCy models and nothing else. One pane is one
#   engine's answer to one label -- a single colour, one question.
#   THE MARKS. Each pane is segmented on ITS OWN spans only, so what LexNLP did to ORG is shown as
#   LexNLP did it, with no boundary introduced by an engine or a label you are not looking at.
#   THE TOOLTIPS. Hover a mark: the label, the engine, and every extra that candidate carried --
#   TypeAbbr on an organisation, DateValue on a date, Amount and Currency on a figure.
#
# WATCH THE COMBO LIST THE CONSOLE PRINTS. It is built from whatever parquets are on disk, so a
# superseded model left behind by an earlier run -- dateregex-v1 beside v2, moneyregex-v5 beside
# v6 -- appears as its own engine and inflates every file. Delete the stale parquets, or pin the
# set with .combos.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII.


# 1. Configuration ----

.dir_04a   <- here::here("2_output", "04A-EntityExtract")
.path_text <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys <- fs::path(.dir_04a, "sample_anchors.parquet")
.dir_probe <- here::here("2_output", "_Probe")
.dir_out   <- fs::dir_create(here::here("2_output", "_Probe", "reads-smoke"))

.n_docs    <- 10L      # rendered in full, to be opened and read
.max_chars <- NULL    # NULL = the whole document; set an integer if a file opens slowly
# NULL takes every engine found on disk. Pin it to exclude a superseded model rather than deleting
# the parquet, e.g. c("lexnlp", "paper:dateregex-v2", "paper:moneyregex-v6", ...).
.combos    <- NULL
.seed      <- 42L

source(here::here("1_code", "_Commons", "_Reads.R"), encoding = "UTF-8")

stopifnot(fs::file_exists(c(.path_text, .path_keys)))


# 2. Candidates, from the five check runs ----
# Every parquet the Check-NER-* scripts left on disk, whatever label and engine, stacked into one
# table. Engine and Model are already stamped in each file, so Combo is derived rather than parsed
# out of the filename -- a filename is a convenience and the columns are the record.

files_ <- fs::dir_ls(.dir_probe, recurse = TRUE, glob = "*.parquet") |>
  (\(.x) .x[stringi::stri_detect_fixed(.x, "Check-NER-")])() |>
  (\(.x) .x[stringi::stri_detect_fixed(.x, "/out/")])()

if (length(files_) == 0L) {
  cli::cli_abort(c(
    "No Check-NER-* parquets found under {.path {(.dir_probe)}}.",
    "i" = "Run the Check-NER-ORG, GPE, DATE, MONEY and REDACT scripts first."
  ))
}

cli::cli_alert_info("{length(files_)} parquet{?s} found.")

tab_all <- purrr::map(files_, function(.f) {
  arrow::read_parquet(.f) |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$Start)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
}) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    Start = as.integer(.data$Start),
    Stop  = as.integer(.data$Stop),
    Combo = dplyr::if_else(.data$Engine == .data$Model, .data$Engine,
                           paste0(.data$Engine, ":", .data$Model))
  )

cli::cli_h2("What the viewer will render")
tab_all |>
  dplyr::count(.data$Combo, .data$Label, name = "N") |>
  tidyr::pivot_wider(id_cols = Combo, names_from = Label, values_from = N, values_fill = 0L) |>
  print(n = Inf, width = Inf)

cli::cli_h2("Extra columns, by label")
extras_ <- setdiff(names(tab_all), .read_core)
purrr::map(sort(unique(tab_all$Label)), function(.l) {
  sub_ <- dplyr::filter(tab_all, .data$Label == .l)
  keep_ <- extras_[purrr::map_lgl(extras_, \(.k) any(!is.na(sub_[[.k]])))]
  tibble::tibble(Label = .l, N = nrow(sub_),
                 Extras = if (length(keep_)) paste(keep_, collapse = ", ") else "-")
}) |>
  purrr::list_rbind() |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "An extra populated for one label and null for every other is the shape the per-label tables \\
   exist for: stacked into one flat table they are mostly holes, and split by label they are dense."
)


# 3. Text and metadata ----

tab_text <- arrow::read_parquet(.path_text) |>
  dplyr::filter(.data$DocID %in% tab_all$DocID) |>
  dplyr::select(DocID, TextRaw)

tab_meta <- arrow::read_parquet(.path_keys) |>
  dplyr::filter(.data$DocID %in% tab_all$DocID) |>
  dplyr::transmute(
    DocID,
    Class     = .data$ClassDetailed,
    Company   = .data$CompanyName,
    CIK       = as.character(.data$CIK),
    Filed     = as.character(.data$DateFiled),
    Amendment = as.character(.data$AmendType)
  )


# 4. Render ----
# Three documents, chosen from those the checks actually covered so every tab has something in it.

docs_ <- withr::with_seed(.seed, sample(unique(tab_all$DocID), size = .n_docs))

res <- read_export(
  .text      = tab_text,
  .cands     = tab_all,
  .dir_out   = .dir_out,
  .meta      = tab_meta,
  .doc_ids   = docs_,
  .combos    = .combos,     # NULL = every engine found, in one fixed order across all files
  .labels    = NULL,        # NULL = ORG, PERSON, GPE, DATE, MONEY, REDACT where present
  .max_chars = .max_chars,
  .overwrite = TRUE,        # a smoke test always re-renders; the cache is for the real pass
  .quiet     = FALSE
)

cli::cli_h2("Written")
res |>
  dplyr::mutate(
    KB    = round(as.numeric(fs::file_size(.data$Path)) / 1024, 1),
    Chars = tab_text$TextRaw[match(.data$DocID, tab_text$DocID)] |> stringi::stri_length(),
    Spans = purrr::map_int(.data$DocID, \(.d) sum(tab_all$DocID == .d)),
    Path  = fs::path_file(.data$Path)
  ) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "Open one and check four things: the label tabs carry the totals from the table above, the \\
   engine row under a label lists only engines that emit it, a tooltip shows the extras, and each \\
   pane is segmented by one engine's spans for one label and nothing else."
)

# THE SIZE QUESTION THE FULL PASS TURNS ON, and the two-tier layout made it larger: the text is now
# repeated once per LABEL-ENGINE PAIR rather than once per engine, which is roughly twenty-eight
# times rather than nine. The file scales with document length, not with candidate count, and the
# sample's longest decile runs past 200,000 characters. If the estimate is uncomfortable, .max_chars
# is the lever and it announces itself on the page.
cli::cli_h2("What the full pass would cost")
tibble::tibble(
  Docs      = nrow(res),
  MedChars  = stats::median(stringi::stri_length(tab_text$TextRaw)),
  MedKB     = stats::median(round(as.numeric(fs::file_size(res$Path)) / 1024, 1)),
  KBPerChar = round(sum(as.numeric(fs::file_size(res$Path))) /
                      sum(stringi::stri_length(
                        tab_text$TextRaw[match(res$DocID, tab_text$DocID)]
                      )), 2)
) |>
  dplyr::mutate(EstimateGB_4398 = round(.data$KBPerChar * .data$MedChars * 4398 / 1024^2, 2)) |>
  print(width = Inf)
