# Purpose: Orchestrator — segment every doc-parquet in 0_sample/, batched per quarter ----
# One segment.py call per year-quarter FOLDER (Python imports once and loops over all docs
# in the folder), writing one segments + one anchors parquet per quarter under
# 0_scratch/segment/. Then a few small seam checks. Sample-scale, quick-and-dirty;
# run from the project root with the renv project active.

# Configuration ----
dir_engine   <- here::here("contracts-engine")
python_bin   <- fs::path(dir_engine, ".venv", "bin", "python")   # uv-created venv
path_segment <- fs::path(dir_engine, "segment.py")

dir_in  <- here::here("0_sample")                  # year-quarter subfolders of per-doc parquets
dir_out <- here::here("0_scratch", "segment")      # segments/<quarter>.parquet + anchors/<quarter>.parquet
mode    <- "blank_line"                            # or "single_newline"

id_col   <- "DocID"
text_col <- "TextRaw"

# Helper: sha256 of a string's UTF-8 bytes — matches Python hashlib.sha256(s.encode("utf-8")). ----
sha256_utf8 <- function(.x) {
  if (FALSE) {
    .x <- c("hello", "Caf\u00e9")
  }
  as.character(openssl::sha256(enc2utf8(.x)))
}

# Helper: run segment.py on one input (file OR folder); abort loudly on a non-zero exit. ----
run_segment <- function(.input, .out_segments, .out_anchors, .mode = "blank_line",
                        .python = python_bin, .script = path_segment,
                        .id_col = "DocID", .text_col = "TextRaw") {
  if (FALSE) {
    .input <- quarter_dirs[1]; .out_segments <- out_seg[1]; .out_anchors <- out_anc[1]
    .mode <- "blank_line"; .python <- python_bin; .script <- path_segment
    .id_col <- "DocID"; .text_col <- "TextRaw"
  }
  
  args_ <- c(
    .script, .input,
    "--out-segments", .out_segments,
    "--out-anchors",  .out_anchors,
    "--id-col",       .id_col,
    "--text-col",     .text_col,
    "--mode",         .mode
  )
  
  out_    <- system2(.python, args = args_, stdout = TRUE, stderr = TRUE)
  status_ <- attr(out_, "status")
  if (!is.null(status_) && status_ != 0L) {
    cat(out_, sep = "\n")
    cli::cli_abort("segment.py failed (status {status_}) on {fs::path_file(.input)}")
  }
  invisible(out_)
}

# Pre-flight ----
if (!fs::file_exists(python_bin))
  cli::cli_abort("No engine venv at {.path {python_bin}} — run `uv sync` in contracts-engine/.")
if (!fs::file_exists(path_segment))
  cli::cli_abort("segment.py not found at {.path {path_segment}}.")
if (!fs::dir_exists(dir_in))
  cli::cli_abort("No input dir at {.path {dir_in}}.")

# Job list: one call per year-quarter folder (batched inside Python) ----
quarter_dirs <- fs::dir_ls(dir_in, type = "directory")
if (length(quarter_dirs) == 0L)
  cli::cli_abort("No subfolders under {.path {dir_in}}.")
quarter_names <- fs::path_file(quarter_dirs)               # "2021-1", "2021-2", ...

out_seg <- fs::path(dir_out, "segments", paste0(quarter_names, ".parquet"))
out_anc <- fs::path(dir_out, "anchors",  paste0(quarter_names, ".parquet"))
fs::dir_create(c(fs::path(dir_out, "segments"), fs::path(dir_out, "anchors")))

cli::cli_alert_info("Segmenting {length(quarter_dirs)} folder(s) [mode = {mode}] -> {.path {dir_out}}")

# For the corpus, parallelise this with mirai (each quarter is independent); 4 folders
# run fine sequentially — the win here is batching inside Python, not concurrency.
purrr::walk(seq_along(quarter_dirs), \(.i) {
  run_segment(quarter_dirs[.i], out_seg[.i], out_anc[.i],
              .mode = mode, .id_col = id_col, .text_col = text_col)
}, .progress = "segment.py")

# Read the segments back (offsets-only -> small) ----
segments <- arrow::open_dataset(fs::path(dir_out, "segments")) |> dplyr::collect()

# Small checks ----
# (a) completeness — every input doc shows up in the anchors (one anchor row per doc)
n_in_docs <- arrow::open_dataset(dir_in, unify_schemas = TRUE) |>
  dplyr::distinct(DocID) |> dplyr::collect() |> nrow()
n_out_docs <- arrow::open_dataset(fs::path(dir_out, "anchors")) |>
  dplyr::distinct(DocID) |> dplyr::collect() |> nrow()

# (b) seam — rehydrate a random sample of segments and re-hash. Read ONLY the docs those
#     segments belong to (arrow filters on DocID at scan time), not the whole corpus.
set.seed(1)
seg_sample <- segments |> dplyr::slice_sample(n = min(200L, nrow(segments)))

docs_needed <- arrow::open_dataset(dir_in, unify_schemas = TRUE) |>
  dplyr::filter(DocID %in% seg_sample$DocID) |>
  dplyr::select(DocID, TextRaw) |>
  dplyr::collect()    # mirror Python's non-str -> ""

segment_check <- seg_sample |>
  dplyr::inner_join(docs_needed, by = "DocID") |>
  dplyr::mutate(
    SegText = stringr::str_sub(TextRaw, Start + 1L, Stop),    # == Python TextRaw[Start:Stop]
    HashOk  = sha256_utf8(SegText) == SegHash,
    NCharOk = stringr::str_length(SegText) == NChar
  )

# Report ----
segs_per_doc <- segments |> dplyr::count(DocID, name = "NSeg")
med_seg <- stats::median(segs_per_doc$NSeg)

cli::cli_h2("Segmentation summary")
cli::cli_alert_info("{length(quarter_dirs)} folder(s); {n_in_docs} input doc(s) -> {n_out_docs} segmented")
cli::cli_alert_info("{nrow(segments)} segment(s) (median {med_seg}/doc)")

if (n_in_docs == n_out_docs && all(segment_check$HashOk) && all(segment_check$NCharOk)) {
  cli::cli_alert_success("All docs segmented; sampled segments round-trip across R <-> Python.")
} else {
  cli::cli_alert_danger("Something's off — inspect n_in_docs/n_out_docs and segment_check.")
}

# Eyeball one doc's boundaries to judge --mode (reuses a doc already in memory) ----
preview_id <- seg_sample$DocID[1]
segments |>
  dplyr::filter(DocID == preview_id) |>
  dplyr::inner_join(docs_needed, by = "DocID") |>
  dplyr::arrange(SegmentID) |>
  dplyr::mutate(Preview = stringr::str_trunc(stringr::str_sub(TextRaw, Start + 1L, Stop), 80)) |>
  dplyr::select(SegmentID, Start, Stop, NChar, Preview) |>
  print(n = 30)

