# spotcheck-package.R -- READ-ONLY: does the published package hold together? ------------------------------------------
#
# Four checks on the package 50B built, run against the local copy (identical to the one on Drive):
#
#   A. Checksums   a sample of files is hashed again and compared with MANIFEST.sha256
#   B. Offsets     spans are sliced out of the published text and compared with the text the span file carries.
#                  This is the promise the appendix makes to readers: Start and Stop are 0-based and half-open, so
#                  stringi::stri_sub(Text, Start + 1, Stop) is the span. Extraction ran on TextRaw, which is the
#                  column text/ publishes, so the two must agree character for character.
#   C. One contract  one document end to end: its index row, its text, its spans -- the example a reader would try
#   D. Archives    the entries of one model zip and one replication zip
#
# HOW TO RUN: with the material-contracts project open, open this file and click "Source". It writes one report,
# ~/Downloads/spotcheck-package.txt, and changes nothing. Attach that file in the chat.

YEAR      <- "2015"   # the text file the sampled documents come from
N_DOCS    <- 8L       # documents to check offsets on
N_SPANS   <- 400L     # spans per type and document at most, so the check stays quick
N_HASHES  <- 12L      # files to hash again


# 1. Helpers ----------------------------------------------------------------------------------------------------------

spc_lit <- function(.x) paste0("'", gsub("'", "''", as.character(.x), fixed = TRUE), "'")

spc_query <- function(.con, .sql) tibble::as_tibble(DBI::dbGetQuery(conn = .con, statement = .sql))

spc_in <- function(.ids) paste(spc_lit(.ids), collapse = ", ")

# Every span file, with the columns that carry its offsets and, where it has one, its text.
spc_span_specs <- function() {
  tibble::tribble(
    ~File,                  ~Start,         ~Stop,         ~Text,
    "org_mentions.parquet", "MentionStart", "MentionStop", "SpanText",
    "places_geo.parquet",   "PlaceStart",   "PlaceStop",   "PlaceText",
    "law_clauses.parquet",  "LawStart",     "LawStop",     NA_character_,
    "date_spans.parquet",   "DateStart",    "DateStop",    "DateText",
    "term_spans.parquet",   "TermStart",    "TermStop",    "TermText",
    "money_spans.parquet",  "MoneyStart",   "MoneyStop",   "MoneyText",
    "redact_spans.parquet", "MarkStart",    "MarkStop",    "MarkText"
  )
}


# 2. The report -------------------------------------------------------------------------------------------------------

path_report <- fs::path_expand("~/Downloads/spotcheck-package.txt")
fs::dir_create(fs::path_dir(path_report))
while (sink.number() > 0L) sink()
sink(file = path_report, split = TRUE)

dir_pkg <- fs::path(here::here("2_output", "40B-PublishData"), "Stage", "matcon-data", "v1.0.0")
cat("spotcheck-package.R |", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("package:", as.character(dir_pkg), "| exists:", fs::dir_exists(dir_pkg), "\n")

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, "SET threads = 8")


# A. Checksums
cat("\n######## A. Checksums ########\n")
manifest <- readLines(fs::path(dir_pkg, "MANIFEST.sha256"), warn = FALSE)
man <- tibble::tibble(
  Sha256 = stringi::stri_sub(manifest, from = 1L, to = 64L),
  Path   = stringi::stri_sub(manifest, from = 67L)
)
set.seed(42)
sizes <- fs::file_size(fs::path(dir_pkg, man$Path))
pick <- unique(c(
  sample(which(as.numeric(sizes) < 5e6), size = min(N_HASHES - 3L, sum(as.numeric(sizes) < 5e6))),
  utils::head(order(as.numeric(sizes), decreasing = TRUE), 3L)
))
checked <- man[pick, ] |>
  dplyr::mutate(
    Bytes = as.numeric(sizes[pick]),
    Again = purrr::map_chr(fs::path(dir_pkg, .data$Path), \(.f) digest::digest(.f, algo = "sha256", file = TRUE)),
    Ok    = .data$Again == .data$Sha256
  )
cat("files in the manifest:", nrow(man), "| hashed again:", nrow(checked), "| all match:", all(checked$Ok), "\n")
print(as.data.frame(dplyr::select(checked, "Path", "Bytes", "Ok")), row.names = FALSE, right = FALSE)


# B. Offsets
cat("\n######## B. Offsets: do the spans land on the published text? ########\n")
path_text <- fs::path(dir_pkg, "text", "exhibit10", paste0("exhibit10_", YEAR, ".parquet"))
docs <- spc_query(
  .con = con,
  .sql = sprintf("SELECT DocID FROM read_parquet(%s) ORDER BY DocID LIMIT %d", spc_lit(path_text), N_DOCS)
)
txt <- spc_query(
  .con = con,
  .sql = sprintf(
    "SELECT DocID, TextRaw FROM read_parquet(%s) WHERE DocID IN (%s)",
    spc_lit(path_text), spc_in(docs$DocID)
  )
)
cat("documents sampled from", fs::path_file(path_text), ":", nrow(txt),
    "| characters:", format(sum(nchar(txt$TextRaw)), big.mark = ","), "\n")

out <- purrr::pmap(spc_span_specs(), \(File, Start, Stop, Text) {
  path_ <- fs::path(dir_pkg, "spans", File)
  cols_ <- spc_query(.con = con, .sql = sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", spc_lit(path_)))$column_name
  want_ <- c("DocID", Start, Stop, if (!is.na(Text)) Text)
  if (!all(want_ %in% cols_)) {
    cat("\n--", File, ": expected column(s) missing:", paste(setdiff(want_, cols_), collapse = ", "), "\n")
    return(NULL)
  }
  spans_ <- spc_query(
    .con = con,
    .sql = sprintf(
      "SELECT DocID, %s AS Start, %s AS Stop%s FROM read_parquet(%s) WHERE DocID IN (%s) LIMIT %d",
      Start, Stop, if (is.na(Text)) "" else sprintf(", %s AS SpanText", Text), spc_lit(path_),
      spc_in(txt$DocID), N_SPANS * nrow(txt)
    )
  )
  if (nrow(spans_) == 0L) {
    cat("\n--", File, ": no spans for these documents\n")
    return(NULL)
  }
  joined_ <- spans_ |>
    dplyr::left_join(txt, by = dplyr::join_by("DocID")) |>
    dplyr::mutate(
      Slice = stringi::stri_sub(.data$TextRaw, from = as.integer(.data$Start) + 1L, to = as.integer(.data$Stop))
    )
  if (is.na(Text)) {
    cat("\n--", File, ":", nrow(joined_), "spans, no text column; two slices to read:\n")
    cat(paste0("   [", stringi::stri_sub(utils::head(joined_$Slice, 2), from = 1L, to = 90L), "]"), sep = "\n")
    return(tibble::tibble(File = File, Spans = nrow(joined_), Exact = NA_integer_, Trimmed = NA_integer_))
  }
  exact_ <- joined_$Slice == joined_$SpanText
  trim_ <- trimws(joined_$Slice) == trimws(joined_$SpanText)
  cat("\n--", File, ":", nrow(joined_), "spans | exact:", sum(exact_, na.rm = TRUE),
      "| equal after trimming:", sum(trim_, na.rm = TRUE), "\n")
  bad_ <- joined_[!trim_ | is.na(trim_), ]
  if (nrow(bad_) > 0L) {
    cat("   three that differ (stored | sliced):\n")
    for (i_ in seq_len(min(3L, nrow(bad_)))) {
      cat("   [", stringi::stri_sub(bad_$SpanText[i_], from = 1L, to = 60L), "] | [",
          stringi::stri_sub(bad_$Slice[i_], from = 1L, to = 60L), "]\n", sep = "")
    }
  }
  tibble::tibble(File = File, Spans = nrow(joined_), Exact = sum(exact_, na.rm = TRUE),
                 Trimmed = sum(trim_, na.rm = TRUE))
}) |>
  purrr::list_rbind()
cat("\n")
print(as.data.frame(out), row.names = FALSE, right = FALSE)


# C. One contract end to end
cat("\n######## C. One contract, end to end ########\n")
doc_one <- txt$DocID[1]
idx <- spc_query(
  .con = con,
  .sql = sprintf(
    "SELECT * FROM read_csv_auto(%s) WHERE DocID = %s",
    spc_lit(fs::path(dir_pkg, "core", "ContractIndex.csv.gz")), spc_lit(doc_one)
  )
)
cat("DocID:", doc_one, "\n")
print(as.data.frame(t(as.data.frame(idx))), right = FALSE)
counts <- purrr::map(spc_span_specs()$File, \(.f) {
  n_ <- spc_query(
    .con = con,
    .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s) WHERE DocID = %s",
                   spc_lit(fs::path(dir_pkg, "spans", .f)), spc_lit(doc_one))
  )$n
  tibble::tibble(File = .f, Spans = as.integer(n_))
}) |>
  purrr::list_rbind()
print(as.data.frame(counts), row.names = FALSE, right = FALSE)
cat("first 300 characters of its text:\n[",
    stringi::stri_sub(txt$TextRaw[1], from = 1L, to = 300L), "]\n", sep = "")


# D. Archives
cat("\n######## D. Archives ########\n")
for (zip_ in c(
  fs::path(dir_pkg, "models", "ClassDetailed_L256.zip"),
  fs::path(dir_pkg, "replication", "04B-Rules.zip")
)) {
  if (!fs::file_exists(zip_)) {
    cat("\nmissing:", as.character(zip_), "\n")
    next
  }
  list_ <- zip::zip_list(zip_)
  cat("\n--", fs::path_file(zip_), ":", nrow(list_), "entries,",
      format(fs::as_fs_bytes(sum(list_$uncompressed_size))), "unpacked\n")
  cat(paste0("   ", utils::head(list_$filename, 6)), sep = "\n")
}

DBI::dbDisconnect(conn = con, shutdown = TRUE)
cat("\nDone. Nothing was changed; the report is", as.character(path_report), "\n")
sink()
