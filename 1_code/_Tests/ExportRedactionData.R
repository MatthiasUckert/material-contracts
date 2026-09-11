# ExportRedactionData.R: the redaction-paper bundle ------------------------------------------------------------------
#
# WHAT THIS IS
# A standalone, one-off export. It is NOT part of the material-contracts pipeline: nothing in 1_code
# sources it, it writes nothing back into 2_output, and it may be deleted once the bundle is on
# Dropbox. It reads finished artifacts and copies them, with the one transformation that cannot be
# skipped (see THE OFFSETS below).
#
# WHAT IT SHIPS
#   data/Contracts.parquet              one row per registrant copy; 10-ExportData's release, verbatim
#   data/Contracts_Codebook.csv         its codebook, verbatim
#   data/RedactMarkers.parquet          one row per redaction marker, plus an EmptyBracket flag
#   data/CtoReferences.parquet          one row per confidential-treatment order reference
#   data/CtoOrders.parquet              one row per order, with its full text
#   data/ContractText/*.parquet         DocID, TextRaw -- one file per year-quarter
#   data/ContractHtml/*.parquet         DocID, HTML    -- one file per year-quarter
#   scripts/                            the redaction chain, with MANIFEST.csv
#   DataDictionary.md, Methodology.md, README.md
#
# THE OFFSETS ARE THE ONE THING THAT CANNOT BE COPIED NAIVELY
# MarkStart and MarkStop index the string clf_read_text() built: the TextRaw column of a parsed
# document, and where that column holds more than one row, its rows joined with "\n". Shipping the
# mirror files would ship the ingredients and not the string. So a document whose parquet holds
# several rows is collapsed here, in the original within-file order, by re-reading that one file --
# a dataset scan over a directory makes no ordering promise, and a document reassembled in the wrong
# order would carry offsets that are wrong and look right. nRowsSource records where this happened,
# and red_verify_offsets() slices markers back out of the shipped text and reports the match rate.
#
# THE EMPTY-BRACKET DEFECT IS SHIPPED, NOT FIXED
# redaction.py's classify() normalises a span and takes inner = norm[1:-1] without stripping, so
# "[ ]" yields inner " ", which ^[*\s]+$ matches, and the marker is stored as RedactSymbol. The
# extractor in scripts/ is the one that produced the data in data/, defect included; fixing it here
# would ship a script that cannot reproduce its own output. EmptyBracket flags every affected row,
# so both the guarded and unguarded counts are recoverable, and Methodology.md states the case.
#
# THE READ IS BY QUARTER, NOT BY DOCUMENT
# 04C reads one parquet per document, 1.19 million times, and calls that the dominant cost of its
# pass. Here the mirror's per-quarter directories are opened as one arrow dataset each, so the whole
# corpus is a few hundred scans rather than a million reads. Quarters already written are skipped,
# so an interrupted run resumes.
#
# House style follows the project rather than the general R skill: native pipe; explicit
# package::function; dot-prefixed args; underscore-suffixed locals; .data$ for existing columns and
# bare CamelCase for new ones; if (FALSE) dev blocks; pure ASCII; stringi never base substr;
# {(.arg)} parens in cli interpolation; 125-column margin; no library() calls.


# 0. Configuration ----------------------------------------------------------------------------------------------------
# Every dial in one place. red_run() takes these as arguments rather than reading the list, so a
# single call can override one of them without editing the file.

.RED_CFG <- list(
  # WHERE THE PIPELINE IS. here::here() resolves from the .here marker at the repository root. Set a
  # literal path instead if this is run from outside the project.
  DirRoot = here::here(),

  # WHERE THE BUNDLE GOES.
  DirOut = "/Users/matthiasuckert/Dropbox/MyPapers/RedactionPaper/Redactions_Data",

  # THE POLICY HASH OF THE REDACT RELEASE, from 10-ExportData.qmd's configuration. Pinned rather than
  # globbed: a rehearsal directory sitting beside the full one must not be picked up by accident.
  HashRedact = "7b84db9925eb",

  # TEXT AT PRIMARY-COPY GRAIN. The mirror holds one parquet per registrant copy and the text is
  # byte-identical across copies of one attachment, so this drops roughly 19% of the bytes and puts
  # the text at exactly the grain RedactMarkers is keyed on. HashDocument in Contracts.parquet maps
  # every copy back to it. FALSE ships every copy.
  PrimaryOnly = TRUE,

  # TWO TREES RATHER THAN ONE FILE. HTML runs three to four times TextRaw, so keeping them apart
  # lets a reader pull the text without the markup. FALSE writes one file per quarter carrying both.
  SplitHtml = TRUE,

  # WHICH QUARTERS. NULL is all of them; a character vector such as c("2019-1", "2019-2") is a
  # rehearsal. The format is the mirror's own, "YYYY-Q".
  Quarters = NULL,

  # zstd over snappy: roughly a third smaller on legal prose, and arrow reads both transparently.
  Compression = "zstd",

  # Markers sampled for the offset round-trip. Drawn once, checked quarter by quarter as the text
  # passes through memory, so verification costs no extra read.
  VerifyN = 20000L,

  # FALSE skips any quarter whose output file already exists, which is what makes the run resumable.
  Rerun = FALSE
)

#: The document types to ship text for, as prefixes of the mirror's own DocType values. 8-K reports
#: are deliberately absent: Item 1.01 lives there and nothing in the redaction chain reads it.
.RED_DOC_TYPES <- c(Exhibit10 = "^Exhibit10", CTO = "^CTO")

#: THE EMPTY BRACKET HAS TWO DEFINITIONS AND THEY DO NOT AGREE, so both are stored rather than one
#: being chosen here. export_REDACT() tests MarkText in RE2, where \\s is exactly tab, newline, form
#: feed, carriage return and space; the next-line and no-break-space characters are named beside it.
#: That is the guard the contract-level counts in Contracts.parquet were built under, so it is spelled
#: out code point by code point rather than left to whichever engine reads this pattern.
.RED_EMPTY_NARROW <- "^\\[[\\u0009\\u000A\\u000C\\u000D\\u0020\\u0085\\u00A0]*\\]$"

#: The Unicode reading. redaction.py normalises with Python's own \\s, which covers every Unicode
#: space separator, so this is arguably what classify() meant to match. It catches a few thousand more
#: markers -- brackets holding en quads, narrow no-break spaces, ideographic spaces -- and is stored
#: as a second flag rather than substituted for the first, because a reader reproducing the published
#: counts needs the narrow one and a reader asking what the extractor intended needs this one.
.RED_EMPTY_WIDE <- "^\\[[\\s\\u0085\\u00A0]*\\]$"

#: Order-level columns of 01E's reference file: those constant within one order document. Declared
#: rather than derived, and intersected with what the file actually carries, so a column renamed
#: upstream is reported rather than silently dropped.
.RED_CTO_ORDER_COLS <- c(
  "DocID", "HashIndex", "CIK", "CompanyName", "DateFiled", "YQ",
  "Status", "Rule", "SourceForm", "SourceFiledOn", "IsExtension", "SourceAmended",
  "nRegistrants", "nChars"
)

#: What travels in scripts/. Source paths are relative to the repository root; a file that is not
#: there is reported and the rest still ship. moneyregex.py is here because the REDACT pass runs
#: MONEY alongside it -- RedactedEntity == "money" is the overlap of the two, and without it the
#: marker file's most informative column cannot be reproduced.
.RED_SCRIPTS <- tibble::tribble(
  ~Src,                                                   ~Dst,
  "contracts-extract/src/matcon_extract/redaction.py",    "python/redaction.py",
  "contracts-extract/src/matcon_extract/moneyregex.py",   "python/moneyregex.py",
  "contracts-extract/src/matcon_extract/_io.py",          "python/_io.py",
  "contracts-extract/src/matcon_extract/__main__.py",     "python/__main__.py",
  "contracts-extract/src/matcon_extract/__init__.py",     "python/__init__.py",
  "contracts-extract/tests/test_redaction.py",            "python/tests/test_redaction.py",
  "contracts-extract/pyproject.toml",                     "python/pyproject.toml",
  "contracts-extract/uv.lock",                            "python/uv.lock",
  "contracts-extract/.python-version",                    "python/.python-version",
  "contracts-extract/README.md",                          "python/README.md",
  "1_code/04B5-Rules-REDACT.R",                           "r/04B5-Rules-REDACT.R",
  "1_code/04D-EntityApply.R",                             "r/04D-EntityApply.R",
  "1_code/01E-CtoExhibits.R",                             "r/01E-CtoExhibits.R",
  "1_code/10-ExportData.R",                               "r/10-ExportData.R"
)


# 1. Paths and small helpers ------------------------------------------------------------------------------------------

#' Every path this script reads or writes
#'
#' ONE FUNCTION HOLDS THE LAYOUT so that a moved artifact is a single edit and a missing one is named
#' rather than discovered by whatever tries to open it first.
#'
#' @param .dir_root Repository root.
#' @param .dir_out Bundle destination.
#' @param .hash_redact Policy hash of 04D's REDACT release.
#' @return A named list with In and Out.
red_paths <- function(.dir_root, .dir_out, .hash_redact) {
  if (FALSE) {
    .dir_root    <- .RED_CFG$DirRoot
    .dir_out     <- .RED_CFG$DirOut
    .hash_redact <- .RED_CFG$HashRedact
  }

  out_ <- fs::path(.dir_out)

  list(
    In = list(
      Contracts = fs::path(.dir_root, "2_output/10-ExportData/Output/Contracts.parquet"),
      Codebook  = fs::path(.dir_root, "2_output/10-ExportData/Output/Contracts_Codebook.csv"),
      CtoRefs   = fs::path(.dir_root, "2_output/01E-CtoExhibits/Output/CtoExhibits.parquet"),
      FilePaths = fs::path(.dir_root, "2_output/01B-EdgarDocuments/Output/FilePaths.parquet"),
      RedactDir = fs::path(.dir_root, "2_output/04D-EntityApply/Store/REDACT", .hash_redact)
    ),
    Out = list(
      Root     = out_,
      Data     = fs::path(out_, "data"),
      Text     = fs::path(out_, "data", "ContractText"),
      Html     = fs::path(out_, "data", "ContractHtml"),
      Scripts  = fs::path(out_, "scripts"),
      Markers  = fs::path(out_, "data", "RedactMarkers.parquet"),
      CtoRefs  = fs::path(out_, "data", "CtoReferences.parquet"),
      CtoOrd   = fs::path(out_, "data", "CtoOrders.parquet"),
      Contract = fs::path(out_, "data", "Contracts.parquet"),
      Codebook = fs::path(out_, "data", "Contracts_Codebook.csv"),
      Dict     = fs::path(out_, "DataDictionary.md"),
      Method   = fs::path(out_, "Methodology.md"),
      Readme   = fs::path(out_, "README.md")
    )
  )
}

#' Abort naming the inputs that are not there
#'
#' @param .paths The In list from red_paths().
#' @return Invisibly TRUE.
red_check_inputs <- function(.paths) {
  if (FALSE) .paths <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)$In

  have_ <- purrr::map_lgl(.paths, \(.p) fs::file_exists(.p) || fs::dir_exists(.p))
  if (!all(have_)) {
    cli::cli_abort(c(
      "{sum(!have_)} input{?s} not found.",
      purrr::set_names(as.character(unlist(.paths[!have_])), rep("x", sum(!have_)))
    ))
  }
  cli::cli_alert_success("All {length(.paths)} inputs resolve.")
  invisible(TRUE)
}

#' Human-readable byte count
#' @param .bytes Numeric vector of bytes.
#' @return Character vector.
red_bytes <- function(.bytes) {
  if (FALSE) .bytes <- c(1024, 1024^3)
  as.character(fs::fs_bytes(as.numeric(.bytes)))
}

#' A content hash for the manifest, with the algorithm recorded
#'
#' digest is not a pipeline dependency, so this falls back to base R's md5 rather than failing. The
#' algorithm travels in the manifest, because a hash whose function is unknown proves nothing.
#'
#' @param .path File to hash.
#' @return A one-row tibble: Algo, Hash.
red_hash_file <- function(.path) {
  if (FALSE) .path <- fs::path(.RED_CFG$DirRoot, "1_code/04B5-Rules-REDACT.R")

  if (requireNamespace("digest", quietly = TRUE)) {
    return(tibble::tibble(Algo = "sha256", Hash = digest::digest(file = .path, algo = "sha256")))
  }
  tibble::tibble(Algo = "md5", Hash = unname(tools::md5sum(as.character(.path))))
}

#' Fill {{TOKEN}} placeholders in a template
#' @param .lines Character vector, the template.
#' @param .values Named character vector; names are tokens without braces.
#' @return Character vector with every token replaced.
red_fill <- function(.lines, .values) {
  if (FALSE) {
    .lines  <- c("Rows: {{N}}")
    .values <- c(N = "12")
  }

  out_ <- .lines
  for (nm_ in names(.values)) {
    out_ <- stringi::stri_replace_all_fixed(out_, paste0("{{", nm_, "}}"), .values[[nm_]])
  }
  left_ <- stringi::stri_extract_all_regex(paste(out_, collapse = "\n"), "\\{\\{[A-Z_]+\\}\\}")[[1L]]
  if (!all(is.na(left_))) {
    cli::cli_abort("Template tokens with no value: {unique(left_)}.")
  }
  out_
}


# 2. The corpus index -------------------------------------------------------------------------------------------------

#' Which directory holds each year-quarter, taken from 01B's index rather than rebuilt
#'
#' THE MIRROR ROOT IS NOT HARD-CODED, deliberately. 01E resolves it under 01A and 04C under 01B, so
#' any constant written here would be right for one of them and wrong for the other. FilePaths.parquet
#' stores the full path 01B actually wrote, and its directory is the quarter directory by
#' construction. The file list comes back with it, which is also what lets the primary-copy filter
#' work on file names before a single parquet is opened.
#'
#' @param .path_filepaths 01B's DocID-to-Path index.
#' @param .pattern Regex matched against DocType, e.g. "^Exhibit10".
#' @return Tibble: DocType, YQ, Dir, nFiles -- one row per quarter, ordered.
red_quarter_index <- function(.path_filepaths, .pattern) {
  if (FALSE) {
    .path_filepaths <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)$In$FilePaths
    .pattern        <- "^Exhibit10"   # or "^CTO"
  }

  arrow::open_dataset(sources = .path_filepaths) |>
    dplyr::select("DocID", "DocType", "YQ", "Path") |>
    dplyr::filter(grepl(.pattern, .data$DocType)) |>
    dplyr::collect() |>
    dplyr::mutate(Dir = as.character(fs::path_dir(.data$Path))) |>
    dplyr::summarise(nFiles = dplyr::n(), .by = c("DocType", "YQ", "Dir")) |>
    dplyr::mutate(YQ = as.character(fs::path_file(.data$Dir))) |>
    dplyr::arrange(.data$DocType, .data$YQ)
}

#' The DocIDs whose text is worth shipping
#'
#' @param .path_contracts 10-ExportData's release.
#' @param .primary_only Logical. TRUE keeps one copy per attachment.
#' @return Tibble: DocID, HashDocument.
red_keep_ids <- function(.path_contracts, .primary_only = TRUE) {
  if (FALSE) {
    .path_contracts <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)$In$Contracts
    .primary_only   <- TRUE
  }

  out_ <- arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select("DocID", "HashDocument", "PrimaryFiler") |>
    dplyr::collect()

  if (.primary_only) out_ <- dplyr::filter(out_, .data$PrimaryFiler == 1L)
  dplyr::select(out_, "DocID", "HashDocument")
}


# 3. Probe ------------------------------------------------------------------------------------------------------------

#' What is there, and how big, before anything is written
#'
#' WRITES NOTHING. Every figure is a column read or a directory listing; no document text is opened,
#' so this returns in seconds and is safe to run repeatedly.
#'
#' @param .paths From red_paths().
#' @param .primary_only Logical. Grain the text estimate is reported at.
#' @return Invisibly a named list of the tables printed.
red_probe <- function(.paths, .primary_only = TRUE) {
  if (FALSE) {
    .paths        <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)
    .primary_only <- TRUE
  }

  cli::cli_h1("Probe: nothing is written")

  cli::cli_h2("Contracts")
  con_ <- arrow::open_dataset(sources = .paths$In$Contracts)
  spine_ <- con_ |>
    dplyr::select("DocID", "PrimaryFiler", "nChars", "nRedactExplicit", "nRedactSymbol",
                  "nRedactBlank", "nOmitExplicit", "nOmitSymbol", "nRedactBare", "HasCto") |>
    dplyr::collect()
  prim_ <- dplyr::filter(spine_, .data$PrimaryFiler == 1L)
  tab_con_ <- tibble::tibble(
    What  = c("rows", "columns", "primary copies", "file on disk"),
    Value = c(format(nrow(spine_), big.mark = ","), format(length(names(con_)), big.mark = ","),
              format(nrow(prim_), big.mark = ","),
              red_bytes(fs::file_size(.paths$In$Contracts)))
  )
  print(tab_con_)

  cli::cli_h2("The two redaction definitions, side by side")
  pub_ <- prim_$nRedactSymbol + prim_$nRedactExplicit
  brk_ <- pub_ + prim_$nRedactBlank + prim_$nOmitExplicit + prim_$nOmitSymbol
  tab_def_ <- tibble::tibble(
    Definition = c("published: Symbol + Explicit", "bracketed: five of six kinds", "HasCto == 1"),
    Contracts  = c(sum(pub_ > 0L, na.rm = TRUE), sum(brk_ > 0L, na.rm = TRUE),
                   sum(prim_$HasCto == 1L, na.rm = TRUE)),
    Markers    = c(sum(pub_, na.rm = TRUE), sum(brk_, na.rm = TRUE), NA_integer_)
  )
  print(tab_def_)
  cli::cli_alert_info(
    "Both are shipped as their component columns; DataDictionary.md names them and picks neither."
  )

  cli::cli_h2("Redaction markers")
  files_ <- fs::path(
    fs::dir_ls(.paths$In$RedactDir, regexp = "/chunk-[^/]+$", type = "directory"),
    "redact_spans.parquet"
  )
  files_ <- files_[fs::file_exists(files_)]
  mrk_ <- arrow::open_dataset(sources = files_)
  n_mrk_ <- mrk_ |> dplyr::summarise(N = dplyr::n()) |> dplyr::collect() |> dplyr::pull(.data$N)
  n_emp_ <- mrk_ |>
    dplyr::filter(grepl("^\\[[\\s\\x85\\xA0]*\\]$", .data$MarkText)) |>
    dplyr::summarise(N = dplyr::n()) |>
    dplyr::collect() |>
    dplyr::pull(.data$N)
  tab_mrk_ <- tibble::tibble(
    What  = c("chunk files", "markers", "empty-bracket markers", "store on disk"),
    Value = c(format(length(files_), big.mark = ","), format(n_mrk_, big.mark = ","),
              format(n_emp_, big.mark = ","), red_bytes(sum(fs::file_size(files_))))
  )
  print(tab_mrk_)

  cli::cli_h2("Confidential-treatment orders")
  ref_ <- arrow::read_parquet(file = .paths$In$CtoRefs)
  tab_cto_ <- ref_ |>
    dplyr::summarise(N = dplyr::n(), .by = "LinkStatus") |>
    dplyr::arrange(.data$LinkStatus) |>
    dplyr::mutate(Share = round(100 * .data$N / sum(.data$N), 1))
  print(tab_cto_)
  cli::cli_alert_info(
    "{format(dplyr::n_distinct(ref_$DocID), big.mark = ',')} order{?s}, \\
     {format(nrow(ref_), big.mark = ',')} reference{?s}."
  )

  cli::cli_h2("Text, by quarter")
  idx_ <- purrr::map(
    .x = .RED_DOC_TYPES,
    .f = \(.p) red_quarter_index(.path_filepaths = .paths$In$FilePaths, .pattern = .p)
  ) |>
    purrr::list_rbind()
  keep_ <- red_keep_ids(.path_contracts = .paths$In$Contracts, .primary_only = .primary_only)
  chars_ <- if (.primary_only) sum(prim_$nChars, na.rm = TRUE) else sum(spine_$nChars, na.rm = TRUE)
  tab_txt_ <- tibble::tibble(
    What  = c("quarters", "files in the mirror", "documents shipped", "TextRaw characters",
              "TextRaw, zstd (est. /4)", "with HTML (est. x4)"),
    Value = c(format(nrow(idx_), big.mark = ","), format(sum(idx_$nFiles), big.mark = ","),
              format(nrow(keep_), big.mark = ","), format(chars_, big.mark = ","),
              red_bytes(chars_ / 4), red_bytes(chars_))
  )
  print(tab_txt_)
  cli::cli_alert_info(
    "The HTML estimate is a rule of thumb, not a measurement: markup runs three to four times the \\
     text it wraps, and compresses better. The first quarter written replaces both guesses."
  )

  cli::cli_h2("Destination")
  cli::cli_alert_info("Bundle root: {.path {as.character(.paths$Out$Root)}}")
  cli::cli_alert_info(
    "Exists: {fs::dir_exists(.paths$Out$Root)}. Free space is not read here; check it before a full run."
  )

  invisible(list(Contracts = tab_con_, Definitions = tab_def_, Markers = tab_mrk_,
                 Cto = tab_cto_, Text = tab_txt_, Index = idx_))
}


# 4. The small tables -------------------------------------------------------------------------------------------------

#' Contracts and its codebook, copied verbatim
#'
#' NOT REBUILT AND NOT TRIMMED. 10-ExportData wrote this file, checked it against its own dictionary
#' and shipped it to the other paper; re-deriving a subset here would create a second definition of
#' the same table with nothing keeping the two in step.
#'
#' @param .paths From red_paths().
#' @param .rerun Logical. FALSE leaves a copy that is already there.
#' @return Invisibly a tibble of what was copied.
red_export_contracts <- function(.paths, .rerun = FALSE) {
  if (FALSE) {
    .paths <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)
    .rerun <- FALSE
  }

  fs::dir_create(.paths$Out$Data)
  src_ <- c(.paths$In$Contracts, .paths$In$Codebook)
  dst_ <- c(.paths$Out$Contract, .paths$Out$Codebook)
  have_ <- fs::file_exists(src_)
  if (!have_[[1L]]) cli::cli_abort("Contracts.parquet is not where it should be.")
  if (!have_[[2L]]) cli::cli_alert_warning("No Contracts_Codebook.csv beside the parquet; skipping it.")

  todo_ <- have_ & (.rerun | !fs::file_exists(dst_))
  if (any(todo_)) fs::file_copy(src_[todo_], dst_[todo_], overwrite = TRUE)

  cli::cli_alert_success(
    "Contracts: {sum(todo_)} file{?s} copied, {sum(have_) - sum(todo_)} already current."
  )
  invisible(tibble::tibble(File = as.character(fs::path_file(dst_)), Copied = todo_))
}

#' Every redaction marker, with the empty brackets flagged rather than removed
#'
#' THE FLAG IS THE WHOLE POINT. export_REDACT() refuses these rows at read time, so the counts in
#' Contracts.parquet already exclude them; shipping the markers without saying which ones they are
#' would leave the two files disagreeing with no way to reconcile them. Filtering EmptyBracket == 0
#' here reproduces the contract-level counts exactly.
#'
#' @param .dir_in 04D's REDACT release directory, including its policy hash.
#' @param .path_out Destination parquet.
#' @param .compression Parquet codec.
#' @param .rerun Logical. FALSE returns the file where it already exists.
#' @return Invisibly the written table.
red_export_markers <- function(.dir_in, .path_out, .compression = "zstd", .rerun = FALSE) {
  if (FALSE) {
    .dir_in      <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)$In$RedactDir
    .path_out    <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)$Out$Markers
    .compression <- "zstd"
    .rerun       <- FALSE
  }

  # CACHED ON THE SCHEMA, NOT ON EXISTENCE. A marker file written before the second flag existed
  # looks entirely current by its name and would silently ship one definition where two are declared.
  if (!.rerun && fs::file_exists(.path_out)) {
    have_ <- names(arrow::open_dataset(sources = .path_out))
    if (all(c("EmptyBracket", "EmptyBracketWide") %in% have_)) {
      cli::cli_alert_info("Markers: already written. Pass .rerun = TRUE to rebuild.")
      return(invisible(arrow::read_parquet(file = .path_out)))
    }
    cli::cli_alert_warning("Markers: the file on disk predates the second empty-bracket flag; rebuilding.")
  }

  files_ <- fs::path(
    fs::dir_ls(.dir_in, regexp = "/chunk-[^/]+$", type = "directory"), "redact_spans.parquet"
  )
  files_ <- files_[fs::file_exists(files_)]
  if (length(files_) == 0L) cli::cli_abort("No redact_spans under {.path {as.character(.dir_in)}}.")

  ds_ <- arrow::open_dataset(sources = files_)
  n_ <- ds_ |> dplyr::summarise(N = dplyr::n()) |> dplyr::collect() |> dplyr::pull(.data$N)
  cli::cli_alert_info("Markers: collecting {format(n_, big.mark = ',')} rows from {length(files_)} chunks.")

  out_ <- dplyr::collect(ds_) |>
    dplyr::mutate(
      EmptyBracket     = as.integer(stringi::stri_detect_regex(.data$MarkText, .RED_EMPTY_NARROW)),
      EmptyBracketWide = as.integer(stringi::stri_detect_regex(.data$MarkText, .RED_EMPTY_WIDE))
    ) |>
    dplyr::arrange(.data$DocID, .data$MarkStart)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out, compression = .compression)

  cli::cli_alert_success(
    "Markers: {format(nrow(out_), big.mark = ',')} rows over \\
     {format(dplyr::n_distinct(out_$DocID), big.mark = ',')} contracts."
  )
  cli::cli_alert_info(
    "Empty brackets: {format(sum(out_$EmptyBracket), big.mark = ',')} under the narrow guard \\
     (which is what Contracts.parquet excludes), \\
     {format(sum(out_$EmptyBracketWide), big.mark = ',')} under the Unicode one."
  )
  invisible(out_)
}

#' 01E's reference table, copied with its link diagnostics intact
#' @param .path_in 01E's CtoExhibits.parquet.
#' @param .path_out Destination parquet.
#' @param .compression Parquet codec.
#' @param .rerun Logical.
#' @return Invisibly the written table.
red_export_cto_refs <- function(.path_in, .path_out, .compression = "zstd", .rerun = FALSE) {
  if (FALSE) {
    .path_in     <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)$In$CtoRefs
    .path_out    <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)$Out$CtoRefs
    .compression <- "zstd"
    .rerun       <- FALSE
  }

  if (!.rerun && fs::file_exists(.path_out)) {
    cli::cli_alert_info("CTO references: already written.")
    return(invisible(arrow::read_parquet(file = .path_out)))
  }

  out_ <- arrow::read_parquet(file = .path_in) |>
    dplyr::arrange(.data$DocID, .data$ExhibitNo)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out, compression = .compression)
  cli::cli_alert_success(
    "CTO references: {format(nrow(out_), big.mark = ',')} rows, \\
     {format(sum(!is.na(out_$DocIDContract)), big.mark = ',')} linked to a contract."
  )
  invisible(out_)
}

#' One row per order, with the full text of the order
#'
#' THE ORDERS ARE SMALL AND THEY ARE THE EVIDENCE. Everything 01E parsed -- the grant or denial, the
#' rule, the source filing, the extension flag -- is a regex over this text, so shipping the parse
#' without the text would leave a reader unable to check any of it.
#'
#' THE TEXT IS RAW AND NOT WHITESPACE-NORMALISED. cto_read_order() collapsed whitespace before
#' matching, so the parse ran on a rendering of this string rather than on the string; that is stated
#' in Methodology.md and the normalisation is one stringi call away for anyone reproducing it.
#'
#' @param .tab_refs The reference table, already read.
#' @param .index Quarter index for the CTO tree, from red_quarter_index().
#' @param .path_out Destination parquet.
#' @param .compression Parquet codec.
#' @param .rerun Logical.
#' @return Invisibly the written table.
red_export_cto_orders <- function(.tab_refs, .index, .path_out, .compression = "zstd", .rerun = FALSE) {
  if (FALSE) {
    .paths       <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)
    .tab_refs    <- arrow::read_parquet(file = .paths$In$CtoRefs)
    .index       <- red_quarter_index(.path_filepaths = .paths$In$FilePaths, .pattern = "^CTO")
    .path_out    <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)$Out$CtoOrd
    .compression <- "zstd"
    .rerun       <- FALSE
  }

  if (!.rerun && fs::file_exists(.path_out)) {
    cli::cli_alert_info("CTO orders: already written.")
    return(invisible(arrow::read_parquet(file = .path_out)))
  }

  cols_ <- intersect(.RED_CTO_ORDER_COLS, names(.tab_refs))
  gone_ <- setdiff(.RED_CTO_ORDER_COLS, names(.tab_refs))
  if (length(gone_) > 0L) {
    cli::cli_alert_warning("Declared order-level columns not in 01E's file: {gone_}.")
  }

  # WHICH COLUMNS ARE ORDER-LEVEL IS MEASURED RATHER THAN DECLARED, because a column can carry an
  # order-level name and a reference-level fact. HashIndex is the case that forced this: by the time
  # cto_link_document() joins on it, it identifies the filing the EXHIBIT was found in, and an order
  # naming exhibits across several filings therefore carries several values. Anything that varies is
  # dropped here and stays in CtoReferences.parquet, at the grain it belongs to. Nothing is lost.
  #
  # ONE GROUPED PASS OVER A NAMED COLUMN SET. across() takes the columns to test as a value, so no
  # column name has to be resolved through the data mask, and the table is read once rather than
  # once per column. n_distinct() counts NA as a level deliberately: a field populated for some
  # references of an order and missing for others is varying, not constant.
  test_ <- setdiff(cols_, "DocID")
  vary_ <- if (length(test_) == 0L) {
    tibble::tibble(Column = character(0), MaxDistinct = integer(0))
  } else {
    wide_ <- .tab_refs |>
      dplyr::summarise(
        dplyr::across(.cols = dplyr::all_of(test_), .fns = \(.x) dplyr::n_distinct(.x)),
        .by = "DocID"
      ) |>
      dplyr::summarise(
        dplyr::across(.cols = dplyr::all_of(test_), .fns = \(.x) max(.x, na.rm = TRUE))
      )
    tibble::tibble(
      Column      = names(wide_),
      MaxDistinct = as.integer(unlist(wide_[1L, ], use.names = FALSE))
    ) |>
      dplyr::filter(.data$MaxDistinct > 1L)
  }

  if (nrow(vary_) > 0L) {
    cli::cli_alert_warning(
      "Not constant within an order, so left to CtoReferences: \\
       {paste0(vary_$Column, ' (up to ', vary_$MaxDistinct, ')')}."
    )
    cols_ <- setdiff(cols_, vary_$Column)
  }

  head_ <- .tab_refs |>
    dplyr::select(dplyr::all_of(cols_)) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$DocID)
  if (anyDuplicated(head_$DocID) > 0L) {
    cli::cli_abort("Order-level columns still vary within DocID after the drop; this cannot happen.")
  }

  counts_ <- .tab_refs |>
    dplyr::summarise(
      nRefs       = dplyr::n(),
      nRefsLinked = sum(!is.na(.data$DocIDContract)),
      .by         = "DocID"
    )

  txt_ <- if (nrow(.index) == 0L) {
    cli::cli_alert_warning("No CTO quarters in the index; orders ship without text.")
    tibble::tibble(DocID = character(0), TextRaw = character(0), HTML = character(0),
                   nRowsSource = integer(0))
  } else {
    purrr::map(.x = .index$Dir, .f = \(.d) red_read_quarter(.dir = .d, .keep_ids = NULL)) |>
      purrr::list_rbind()
  }

  out_ <- head_ |>
    dplyr::left_join(counts_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(txt_, by = dplyr::join_by(DocID))

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out, compression = .compression)
  cli::cli_alert_success(
    "CTO orders: {format(nrow(out_), big.mark = ',')} orders, \\
     {format(sum(!is.na(out_$TextRaw)), big.mark = ',')} with text."
  )
  invisible(out_)
}


# 5. The text -----------------------------------------------------------------------------------------------------------

#' One quarter of the mirror, as one table, with multi-row documents collapsed correctly
#'
#' unify_schemas = TRUE COSTS A FOOTER READ PER FILE AND IS WORTH IT. rGetEDGAR wrote these files
#' independently over years of acquisition, and a quarter containing one file without an HTML column
#' would otherwise either abort the scan or drop the column from all of it.
#'
#' THE MULTI-ROW PATH EXISTS BECAUSE THE OFFSETS DEPEND ON IT. A dataset scan makes no promise about
#' the order rows come back in, so a document whose parquet holds several rows is re-read on its own
#' and joined in file order -- which is what clf_read_text() did and what every offset indexes. The
#' fast path handles the rest untouched, and nRowsSource records which documents took which.
#'
#' @param .dir A quarter directory in the parsed mirror.
#' @param .keep_ids Character vector of DocIDs to keep, or NULL for all. Applied to FILE NAMES, so
#'   documents that are not wanted are never opened.
#' @return Tibble: DocID, TextRaw, HTML, nRowsSource. HTML is NA where the quarter carries none.
red_read_quarter <- function(.dir, .keep_ids = NULL) {
  if (FALSE) {
    .dir      <- "2_output/01B-EdgarDocuments/GetEDGAR/DocumentData/Parsed/Exhibit10/2019-1"
    .keep_ids <- NULL
  }

  files_ <- fs::dir_ls(.dir, glob = "*.parquet", type = "file")
  if (!is.null(.keep_ids)) {
    files_ <- files_[as.character(fs::path_ext_remove(fs::path_file(files_))) %in% .keep_ids]
  }
  if (length(files_) == 0L) {
    return(tibble::tibble(DocID = character(0), TextRaw = character(0), HTML = character(0),
                          nRowsSource = integer(0)))
  }

  ds_   <- arrow::open_dataset(sources = files_, format = "parquet", unify_schemas = TRUE)
  have_ <- names(ds_)
  if (!"TextRaw" %in% have_) cli::cli_abort("No TextRaw column under {.path {as.character(.dir)}}.")
  want_ <- intersect(c("DocID", "TextRaw", "HTML"), have_)

  tab_ <- ds_ |>
    dplyr::select(dplyr::all_of(want_)) |>
    dplyr::collect()
  if (!"HTML" %in% names(tab_)) tab_ <- dplyr::mutate(tab_, HTML = NA_character_)

  cnt_  <- dplyr::summarise(tab_, nRowsSource = dplyr::n(), .by = "DocID")
  many_ <- dplyr::filter(cnt_, .data$nRowsSource > 1L)

  out_ <- tab_ |>
    dplyr::filter(!.data$DocID %in% many_$DocID) |>
    dplyr::left_join(cnt_, by = dplyr::join_by(DocID))

  if (nrow(many_) > 0L) {
    stems_ <- as.character(fs::path_ext_remove(fs::path_file(files_)))
    fix_ <- purrr::map(many_$DocID, function(.id) {
      one_ <- arrow::read_parquet(file = files_[match(.id, stems_)])
      tibble::tibble(
        DocID       = .id,
        TextRaw     = paste(one_[["TextRaw"]], collapse = "\n"),
        HTML        = if ("HTML" %in% names(one_)) paste(one_[["HTML"]], collapse = "\n") else NA_character_,
        nRowsSource = nrow(one_)
      )
    }) |>
      purrr::list_rbind()
    out_ <- dplyr::bind_rows(out_, fix_)
  }

  out_ |>
    dplyr::select("DocID", "TextRaw", "HTML", "nRowsSource") |>
    dplyr::arrange(.data$DocID)
}

#' Every quarter of contract text, one file each, resumable
#'
#' @param .index Quarter index from red_quarter_index().
#' @param .keep Tibble of DocID and HashDocument to ship.
#' @param .dir_text Destination for the text tree.
#' @param .dir_html Destination for the markup tree, or NA to write one file carrying both.
#' @param .quarters Character vector of quarters, or NULL for all.
#' @param .verify Tibble of sampled markers, or NULL to skip verification.
#' @param .compression Parquet codec.
#' @param .rerun Logical. FALSE skips quarters already on disk.
#' @return Invisibly a tibble: one row per quarter, with what it wrote and what it verified.
red_export_text <- function(.index, .keep, .dir_text, .dir_html, .quarters = NULL,
                            .verify = NULL, .compression = "zstd", .rerun = FALSE) {
  if (FALSE) {
    .paths       <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)
    .index       <- red_quarter_index(.path_filepaths = .paths$In$FilePaths, .pattern = "^Exhibit10")
    .keep        <- red_keep_ids(.path_contracts = .paths$In$Contracts)
    .dir_text    <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)$Out$Text
    .dir_html    <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)$Out$Html
    .quarters    <- c("2019-1")
    .verify      <- NULL
    .compression <- "zstd"
    .rerun       <- FALSE
  }

  idx_ <- if (is.null(.quarters)) .index else dplyr::filter(.index, .data$YQ %in% .quarters)
  split_ <- !is.na(.dir_html)
  fs::dir_create(.dir_text)
  if (split_) fs::dir_create(.dir_html)

  keep_ids_ <- .keep$DocID
  cli::cli_alert_info(
    "Text: {nrow(idx_)} quarter{?s}, {format(length(keep_ids_), big.mark = ',')} documents wanted."
  )

  out_ <- purrr::map(seq_len(nrow(idx_)), function(.i) {
    yq_    <- idx_$YQ[[.i]]
    stem_  <- paste0(idx_$DocType[[.i]], "_", yq_, ".parquet")
    p_txt_ <- fs::path(.dir_text, stem_)
    p_htm_ <- if (split_) fs::path(.dir_html, stem_) else p_txt_

    if (!.rerun && fs::file_exists(p_txt_) && (!split_ || fs::file_exists(p_htm_))) {
      cli::cli_alert_info("{yq_}: already written, skipped.")
      return(tibble::tibble(YQ = yq_, Docs = NA_integer_, Collapsed = NA_integer_,
                            Verified = NA_integer_, OkShare = NA_real_, Skipped = TRUE))
    }

    tab_ <- red_read_quarter(.dir = idx_$Dir[[.i]], .keep_ids = keep_ids_) |>
      dplyr::left_join(.keep, by = dplyr::join_by(DocID)) |>
      dplyr::mutate(YQ = yq_)

    txt_ <- tab_ |>
      dplyr::mutate(nCharsText = as.integer(stringi::stri_length(.data$TextRaw))) |>
      dplyr::select("DocID", "HashDocument", "YQ", "TextRaw", "nCharsText", "nRowsSource")
    if (split_) {
      arrow::write_parquet(txt_, p_txt_, compression = .compression)
      htm_ <- tab_ |>
        dplyr::mutate(nCharsHtml = as.integer(stringi::stri_length(.data$HTML))) |>
        dplyr::select("DocID", "HashDocument", "YQ", "HTML", "nCharsHtml")
      arrow::write_parquet(htm_, p_htm_, compression = .compression)
    } else {
      both_ <- tab_ |>
        dplyr::mutate(
          nCharsText = as.integer(stringi::stri_length(.data$TextRaw)),
          nCharsHtml = as.integer(stringi::stri_length(.data$HTML))
        ) |>
        dplyr::select("DocID", "HashDocument", "YQ", "TextRaw", "HTML", "nCharsText",
                      "nCharsHtml", "nRowsSource")
      arrow::write_parquet(both_, p_txt_, compression = .compression)
    }

    ver_ <- red_verify_offsets(.tab_text = tab_, .marks = .verify)
    cli::cli_alert_success(
      "{yq_}: {format(nrow(tab_), big.mark = ',')} documents, \\
       {sum(tab_$nRowsSource > 1L)} collapsed, {ver_$N} marker{?s} checked."
    )

    tibble::tibble(YQ = yq_, Docs = nrow(tab_), Collapsed = sum(tab_$nRowsSource > 1L),
                   Verified = ver_$N, OkShare = ver_$OkShare, Skipped = FALSE)
  }) |>
    purrr::list_rbind()

  invisible(out_)
}


# 6. Verification -------------------------------------------------------------------------------------------------------

#' Slice sampled markers back out of the shipped text
#'
#' THE ONE NUMBER THAT MAKES THE MARKER FILE USABLE. If MarkStart and MarkStop do not index the
#' TextRaw shipped here, every span in RedactMarkers points at the wrong characters and nothing in
#' the bundle can be checked. Offsets are 0-based and half-open, which is why the slice runs from
#' Start + 1 to Stop, exactly as 04A's own round-trip does.
#'
#' @param .tab_text A quarter of text, carrying DocID and TextRaw.
#' @param .marks Sampled markers, or NULL.
#' @return A one-row list: N checked, OkShare.
red_verify_offsets <- function(.tab_text, .marks = NULL) {
  if (FALSE) {
    .tab_text <- red_read_quarter(.dir = ".", .keep_ids = NULL)
    .marks    <- NULL
  }

  if (is.null(.marks) || nrow(.marks) == 0L) return(list(N = 0L, OkShare = NA_real_))

  chk_ <- .marks |>
    dplyr::inner_join(dplyr::select(.tab_text, "DocID", "TextRaw"), by = dplyr::join_by(DocID))
  if (nrow(chk_) == 0L) return(list(N = 0L, OkShare = NA_real_))

  cut_ <- stringi::stri_sub(chk_$TextRaw, from = chk_$MarkStart + 1L, to = chk_$MarkStop)
  ok_  <- !is.na(cut_) & cut_ == chk_$MarkText

  list(N = nrow(chk_), OkShare = mean(ok_, na.rm = TRUE))
}


# 7. Scripts and the manifest -------------------------------------------------------------------------------------------

#' The redaction chain, copied, hashed and pinned to a commit
#'
#' A BUNDLE THAT CANNOT SAY WHICH CODE PRODUCED IT IS NOT A REPLICATION BUNDLE. The manifest carries
#' the source path, the size, a content hash and the repository commit, so a file in scripts/ can be
#' matched against the repository it came from without trusting the copy.
#'
#' @param .dir_root Repository root.
#' @param .dir_out Destination for scripts/.
#' @param .spec Tibble of Src and Dst, relative paths.
#' @return Invisibly the manifest.
red_export_scripts <- function(.dir_root, .dir_out, .spec = .RED_SCRIPTS) {
  if (FALSE) {
    .dir_root <- .RED_CFG$DirRoot
    .dir_out  <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)$Out$Scripts
    .spec     <- .RED_SCRIPTS
  }

  commit_ <- tryCatch(
    system2("git", c("-C", shQuote(as.character(.dir_root)), "rev-parse", "HEAD"),
            stdout = TRUE, stderr = FALSE)[[1L]],
    error = function(e) NA_character_
  )

  src_ <- fs::path(.dir_root, .spec$Src)
  ok_   <- fs::file_exists(src_)
  gone_ <- .spec$Src[!ok_]
  if (length(gone_) > 0L) cli::cli_alert_warning("Not in the repository, so not shipped: {gone_}.")

  spec_ <- dplyr::filter(dplyr::mutate(.spec, Present = ok_), .data$Present)
  dst_  <- fs::path(.dir_out, spec_$Dst)
  fs::dir_create(unique(fs::path_dir(dst_)))
  fs::file_copy(fs::path(.dir_root, spec_$Src), dst_, overwrite = TRUE)

  man_ <- spec_ |>
    dplyr::mutate(
      Bytes  = as.numeric(fs::file_size(dst_)),
      Commit = commit_
    ) |>
    dplyr::bind_cols(purrr::map(dst_, red_hash_file) |> purrr::list_rbind()) |>
    dplyr::select("Dst", "Src", "Bytes", "Algo", "Hash", "Commit")

  utils::write.csv(man_, fs::path(.dir_out, "MANIFEST.csv"), row.names = FALSE)
  cli::cli_alert_success("Scripts: {nrow(man_)} file{?s} copied, manifest written.")
  invisible(man_)
}


# 8. Documentation ------------------------------------------------------------------------------------------------------

#: What every column of the four files this script builds means. Contracts.parquet is deliberately
#: absent: 10-ExportData already wrote its codebook and it is copied beside the data, so documenting
#: it twice would create two descriptions of one file with nothing keeping them in step.
.RED_DICT <- tibble::tribble(
  ~File, ~Column, ~Meaning,

  "RedactMarkers.parquet", "DocID", "The contract, at attachment grain: the primary copy 04C read.",
  "RedactMarkers.parquet", "MarkStart", "Offset of the marker into TextRaw. 0-based, half-open, code points.",
  "RedactMarkers.parquet", "MarkStop", "Offset one past the marker's last character.",
  "RedactMarkers.parquet", "MarkText", "The marker as written; slices from the two offsets exactly.",
  "RedactMarkers.parquet", "Kind", "One of six classes redaction.py assigns. See Methodology.",
  "RedactMarkers.parquet", "Bracketed", "One of the five bracketed classes. The published definition's basis.",
  "RedactMarkers.parquet", "Withheld", "Redaction rather than omission; a placeholder is not a redaction.",
  "RedactMarkers.parquet", "RedactedEntity", "What the marker replaced: 'money' where MONEY knew, else 'unmatched'.",
  "RedactMarkers.parquet", "RedactedRef", "Offset of the span establishing that; null where nothing matched.",
  "RedactMarkers.parquet", "RedactedText", "That span as written, so the match is readable.",
  "RedactMarkers.parquet", "EmptyBracket", "1 where the marker is brackets and ASCII whitespace only. See Methodology.",
  "RedactMarkers.parquet", "EmptyBracketWide", "The same test with Unicode whitespace. Wider; see Methodology.",

  "CtoOrders.parquet", "DocID", "The order document on EDGAR. Unique in this file.",
  "CtoOrders.parquet", "HashIndex", "A filing 01E resolved for this order. Present only where constant within it.",
  "CtoOrders.parquet", "CIK", "The filer the order was addressed to.",
  "CtoOrders.parquet", "CompanyName", "That filer's name as EDGAR recorded it.",
  "CtoOrders.parquet", "DateFiled", "When the order was filed.",
  "CtoOrders.parquet", "YQ", "Year and quarter of filing, as the mirror names them.",
  "CtoOrders.parquet", "Status", "The verb in the title: GRANTING, DENYING, REVOKING.",
  "CtoOrders.parquet", "Rule", "24b-2 (Exchange Act) or 406 (Securities Act); pipe-separated where both.",
  "CtoOrders.parquet", "SourceForm", "The form the order concerns, from its opening paragraph.",
  "CtoOrders.parquet", "SourceFiledOn", "When that filing was made, from the same paragraph.",
  "CtoOrders.parquet", "IsExtension", "1 where the order extends an earlier grant.",
  "CtoOrders.parquet", "SourceAmended", "1 where the source filing is described as amended.",
  "CtoOrders.parquet", "nRegistrants", "Count of 'File No.' occurrences; a proxy for co-registrants.",
  "CtoOrders.parquet", "nChars", "Characters in the whitespace-normalised text the parser read.",
  "CtoOrders.parquet", "nRefs", "Exhibit references this order names.",
  "CtoOrders.parquet", "nRefsLinked", "How many of those resolved to a contract in the corpus.",
  "CtoOrders.parquet", "TextRaw", "The order's full text, raw. NOT whitespace-normalised.",
  "CtoOrders.parquet", "HTML", "The order's markup, where the mirror holds it.",
  "CtoOrders.parquet", "nRowsSource", "Rows the source parquet held; above 1 means the text was joined.",

  "ContractText/*.parquet", "DocID", "The contract. Joins to Contracts.parquet one to one.",
  "ContractText/*.parquet", "HashDocument", "The attachment. Shared by every registrant copy of it.",
  "ContractText/*.parquet", "YQ", "Year and quarter of filing; also the file name.",
  "ContractText/*.parquet", "TextRaw", "The canonical text every offset in RedactMarkers indexes.",
  "ContractText/*.parquet", "nCharsText", "Its length in code points.",
  "ContractText/*.parquet", "nRowsSource", "Rows the source parquet held; above 1 means the text was joined with newlines.",

  "ContractHtml/*.parquet", "DocID", "The contract.",
  "ContractHtml/*.parquet", "HashDocument", "The attachment.",
  "ContractHtml/*.parquet", "YQ", "Year and quarter of filing.",
  "ContractHtml/*.parquet", "HTML", "The parsed markup. Nothing in the redaction chain reads it.",
  "ContractHtml/*.parquet", "nCharsHtml", "Its length in code points."
)

#' DataDictionary.md, derived from the files rather than asserted
#'
#' THE DECLARED MEANINGS ARE CHECKED AGAINST THE WRITTEN COLUMNS and a disagreement aborts, so a
#' column added upstream and documented nowhere stops the export rather than shipping undocumented.
#' CtoReferences is exempt: it is 01E's file verbatim, its columns are 01E's, and its runbook
#' documents them.
#'
#' @param .paths From red_paths().
#' @param .dict The declared meanings.
#' @return Invisibly the table written.
red_write_dictionary <- function(.paths, .dict = .RED_DICT) {
  if (FALSE) {
    .paths <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)
    .dict  <- .RED_DICT
  }

  first_ <- function(.dir) {
    if (!fs::dir_exists(.dir)) return(NA_character_)
    got_ <- fs::dir_ls(.dir, glob = "*.parquet", type = "file")
    if (length(got_) == 0L) NA_character_ else as.character(got_[[1L]])
  }
  files_ <- c(
    "RedactMarkers.parquet"  = as.character(.paths$Out$Markers),
    "CtoOrders.parquet"      = as.character(.paths$Out$CtoOrd),
    "ContractText/*.parquet" = first_(.paths$Out$Text),
    "ContractHtml/*.parquet" = first_(.paths$Out$Html)
  )
  files_ <- files_[!is.na(files_)]
  files_ <- files_[fs::file_exists(files_)]

  seen_ <- purrr::map(names(files_), function(.f) {
    sch_ <- arrow::open_dataset(sources = files_[[.f]])$schema
    tibble::tibble(
      File   = .f,
      Column = purrr::map_chr(sch_$fields, \(.f2) .f2$name),
      Type   = purrr::map_chr(sch_$fields, \(.f2) .f2$type$ToString())
    )
  }) |>
    purrr::list_rbind()

  undoc_ <- dplyr::anti_join(seen_, .dict, by = dplyr::join_by(File, Column))
  unseen_ <- .dict |>
    dplyr::filter(.data$File %in% names(files_)) |>
    dplyr::anti_join(seen_, by = dplyr::join_by(File, Column))
  # THE TWO DIRECTIONS ARE NOT THE SAME RISK. A column in a file that nothing documents ships
  # undocumented, which is what this whole function exists to prevent, so it aborts. A documented
  # column that is not in a file is over-complete documentation, and it is now a normal outcome:
  # red_export_cto_orders() drops columns it measures to be reference-level. That warns.
  if (nrow(undoc_) > 0L) {
    cli::cli_abort(c(
      "Columns are in a file and documented nowhere.",
      "x" = "{paste(undoc_$File, undoc_$Column, sep = '.')}"
    ))
  }
  if (nrow(unseen_) > 0L) {
    cli::cli_alert_warning(
      "Documented and not in a file, so omitted from the dictionary: \\
       {paste(unseen_$File, unseen_$Column, sep = '.')}."
    )
  }

  tab_ <- dplyr::left_join(seen_, .dict, by = dplyr::join_by(File, Column))
  tab_ <- dplyr::filter(tab_, !is.na(.data$Meaning))

  lines_ <- c(
    "# Data dictionary", "",
    "Every file in `data/`, column by column. `Contracts.parquet` is documented in",
    "`Contracts_Codebook.csv` beside it, which the pipeline generated; `CtoReferences.parquet` is",
    "01E's own output and its columns are described in that script's runbook.", "",
    "Grains, and how the files join:", "",
    "| File | Grain | Key |",
    "|:--|:--|:--|",
    "| Contracts.parquet | registrant copy | `DocID` unique; `HashDocument` groups copies |",
    "| RedactMarkers.parquet | marker | `DocID` + `MarkStart` |",
    "| ContractText, ContractHtml | attachment | `DocID` |",
    "| CtoOrders.parquet | order document | `DocID` |",
    "| CtoReferences.parquet | order reference | `DocID` + `ExhibitNo`; `DocIDContract` links |",
    "",
    "Markers and text are at attachment grain, so they join to `Contracts.parquet` on `DocID` for",
    "`PrimaryFiler == 1` rows, or on `HashDocument` to reach every registrant copy.", ""
  )

  for (f_ in unique(tab_$File)) {
    part_ <- dplyr::filter(tab_, .data$File == f_)
    lines_ <- c(
      lines_, paste0("## `", f_, "`"), "",
      "| Column | Type | Meaning |", "|:--|:--|:--|",
      paste0("| `", part_$Column, "` | ", part_$Type, " | ", part_$Meaning, " |"), ""
    )
  }

  writeLines(lines_, con = as.character(.paths$Out$Dict), useBytes = TRUE)
  cli::cli_alert_success("DataDictionary.md: {nrow(tab_)} columns across {length(files_)} files.")
  invisible(tab_)
}

#' Methodology.md, with the run's own numbers in it
#'
#' THE NUMBERS COME FROM THE RUN AND NOT FROM MEMORY. A methodology note quoting a count somebody
#' typed is a note that goes stale the first time the pipeline is re-run; every figure here is
#' interpolated from what this export actually wrote.
#'
#' @param .paths From red_paths().
#' @param .stats Named character vector of run figures.
#' @return Invisibly the lines written.
red_write_methodology <- function(.paths, .stats) {
  if (FALSE) {
    .paths <- red_paths(.RED_CFG$DirRoot, .RED_CFG$DirOut, .RED_CFG$HashRedact)
    .stats <- c(N_MARKERS = "0")
  }

  tpl_ <- c(
    "# Methodology", "",
    "This bundle is an extract from the `material-contracts` pipeline, prepared on {{DATE}} at",
    "commit `{{COMMIT}}`. Everything here was produced by that pipeline; nothing was recomputed for",
    "this export except the collapse described under *The canonical text* below.", "",
    "## The corpus", "",
    "Every Exhibit 10 attachment EDGAR indexed between 2001 and 2024, acquired and parsed from the",
    "SEC's full-text mirror. A filing naming several registrants lists one attachment under each of",
    "them, so `Contracts.parquet` holds {{N_CONTRACTS}} registrant copies of {{N_PRIMARY}} distinct",
    "attachments. `PrimaryFiler == 1` marks the copy that was read; `HashDocument` groups the rest.",
    "Text and markers are shipped at attachment grain.", "",
    "**Nothing is filtered out.** Malformed documents -- too short, not prose, mostly digits -- stay",
    "in the sample with `Removed` flagging them and `RemClass` saying why.", "",
    "## The canonical text", "",
    "The offsets in `RedactMarkers.parquet` index one specific string: the `TextRaw` column of a",
    "parsed document, and where that column held more than one row, its rows joined with a newline.",
    "`ContractText/` ships exactly that string. {{N_COLLAPSED}} documents required the join, and",
    "`nRowsSource` identifies them.", "",
    "The check that matters: {{N_VERIFIED}} markers were sampled and sliced back out of the shipped",
    "text using `MarkStart + 1` to `MarkStop`. {{OK_SHARE}} reproduced their `MarkText` exactly.", "",
    "`HTML` is shipped beside the text for reference. Nothing in the redaction chain reads it.", "",
    "## Redaction markers", "",
    "`redaction.py` marks six classes of place where text was removed:", "",
    "| Kind | What it matches | Bracketed | Withheld |",
    "|:--|:--|:--|:--|",
    "| RedactExplicit | a bracket naming confidential treatment: CONFIDENTIAL, REDACT, CTR | yes | yes |",
    "| RedactSymbol | a bracket of whitespace and asterisks: `[***]` | yes | yes |",
    "| RedactBlank | a bracket of whitespace and underscores: `[___]` | yes | yes |",
    "| OmitExplicit | a bracket recording deletion: INTENTIONALLY, OMITTED, DELETE | yes | no |",
    "| OmitSymbol | a bracket of bullets, ellipses or three dots | yes | no |",
    "| RedactBare | an unbracketed run of three or more asterisks | no | yes |",
    "",
    "`Bracketed` and `Withheld` are stored rather than derived, because they are the published",
    "definitions themselves rather than measurements about a marker.", "",
    "Three exclusions are applied at extraction and are not in this file: page filler (\"[Remainder",
    "of page intentionally left blank]\"), a marker quoted in a filing's opening legend, and a rule of",
    "asterisks drawn as a border.", "",
    "`RedactedEntity` is the one column that is not a property of the marker alone. `moneyregex`",
    "emits a currency with its number removed -- `$[***]` -- and the two extractors' offsets are",
    "overlapped with a small tolerance, so a marker sitting where a price was is labelled `money`.",
    "On the withheld prices, roughly half the markers are `RedactBare`, which is the one class no",
    "bracketed measure sees.", "",
    "## The empty-bracket defect", "",
    "**This bundle ships the defect rather than a fix, and the flag is how it is handled.**", "",
    "`classify()` normalises a matched bracket and takes `inner = norm[1:-1]` without stripping it.",
    "For `\"[ ]\"` that leaves `inner` as a single space, which `^[*\\\\s]+$` matches, so the bracket is",
    "stored as `RedactSymbol`. Every `[ ]` on EDGAR -- checkboxes, form fields, blanks in schedules --",
    "entered the store as a redaction marker on this route: {{N_EMPTY}} of {{N_MARKERS}} markers.", "",
    "`10-ExportData.R` refuses these rows at read time, so the counts in `Contracts.parquet`",
    "(`nRedactSymbol` and the rest) already exclude them. `RedactMarkers.parquet` keeps them and flags",
    "them, so **filtering `EmptyBracket == 0` reproduces the contract-level counts exactly**, and",
    "keeping them reproduces what the extractor emitted. Both numbers are recoverable; neither is",
    "imposed.", "",
    "**What it costs the published measure.** Under `nRedactSymbol + nRedactExplicit`, the guarded",
    "count is {{N_PUB_GUARD}} contracts carrying {{N_PUB_MARKERS}} markers. Ignore the flag and it is",
    "{{N_PUB_RAW}} contracts. Every empty bracket classifies as `RedactSymbol`, so that difference is",
    "the whole of the defect's effect on the headline number, and it is large enough that any figure",
    "quoted from this data has to say which side of the guard it sits on.", "",
    "**The guard itself has two readings, and both are shipped.** `10-ExportData.R` tests the marker",
    "in RE2, where `\\s` is tab, newline, form feed, carriage return and space; the next-line and",
    "no-break-space characters are named beside it. `EmptyBracket` reproduces that test exactly, which",
    "is why it is the one that reconciles with `Contracts.parquet`: {{N_EMPTY}} markers.",
    "`redaction.py` normalises with Python's `\\s`, which covers every Unicode space separator, so a",
    "bracket holding a narrow no-break space or an ideographic space is arguably what `classify()`",
    "meant to catch too. `EmptyBracketWide` is that reading: {{N_EMPTY_WIDE}} markers. The two differ",
    "by a few thousand rows and neither is wrong; the narrow one reproduces the pipeline, the wide one",
    "reproduces the intent.", "",
    "The script in `scripts/python/redaction.py` is the one that produced this data, defect included.",
    "The fix is one line -- requiring at least one asterisk or underscore in the pattern, which also",
    "moves the extractor's spec hash -- and it has deliberately not been applied here, because a",
    "bundle whose script cannot reproduce its own data is worse than one that documents a defect.", "",
    "## The two redaction definitions", "",
    "The published measure is `nRedactSymbol + nRedactExplicit`. The pipeline's regression code also",
    "offers a wider one summing five of the six kinds. **This bundle picks neither**: the component",
    "counts are shipped and both are one addition away. Whichever is used should be stated, because",
    "the two do not agree.", "",
    "## Confidential-treatment orders", "",
    "Before the 2019 FAST Act amendments, redacting an exhibit required an application to the SEC and",
    "an order granting it. `CtoOrders.parquet` holds {{N_ORDERS}} such orders with their full text;",
    "`CtoReferences.parquet` holds {{N_REFS}} exhibit references parsed out of them, of which",
    "{{N_LINKED}} resolved to a contract in the corpus.", "",
    "Order-level fields are regexes over the text, which is shipped so they can be checked. The text",
    "in `TextRaw` is raw; the parser ran on a whitespace-normalised copy of it.", "",
    "**`CtoOrders.parquet` holds only what is constant within an order.** Which columns those are was",
    "measured rather than assumed: an order naming exhibits across several filings carries a different",
    "target filing per reference, so `HashIndex` in 01E's file identifies the filing an exhibit was",
    "found in rather than the filing the order arrived in. Any column found to vary is left in",
    "`CtoReferences.parquet`, at the grain it belongs to. The export names them when it drops them.", "",
    "Two properties of the linkage travel with the data. A reference links only where exactly one",
    "Exhibit 10 in the filing carries its exhibit number. And an attachment filed in several filings",
    "is covered in the one the order names and not in the others, which is what an order granting",
    "relief for a particular filing means. `LinkStatus` gives the reason for every reference that did",
    "not resolve.", "",
    "**The volume of orders is itself a result.** They run at roughly thirteen hundred a year through",
    "2018 and then collapse, because the FAST Act let filers redact immaterial competitively harmful",
    "information without applying at all. Any analysis spanning that change is looking at two",
    "disclosure regimes: before it, redaction is observed through orders; after it, only through",
    "markers in the text.", "",
    "## Known limitations", "",
    "- Where an attachment's registrant copies disagree on length, the text is one of them and the",
    "  markers were computed from that one. 01C found one such attachment in 1.49 million.",
    "- `RedactBare` is noisy by construction and is excluded from every bracketed measure.",
    "- Classification, party, place, date and money columns in `Contracts.parquet` are model and rule",
    "  outputs with their own error rates, documented in `Contracts_Codebook.csv`.", ""
  )

  out_ <- red_fill(.lines = tpl_, .values = .stats)
  writeLines(out_, con = as.character(.paths$Out$Method), useBytes = TRUE)
  cli::cli_alert_success("Methodology.md written.")
  invisible(out_)
}


# 9. Run ------------------------------------------------------------------------------------------------------------------

#' The whole export, in order
#'
#' @param .cfg The configuration list.
#' @param .probe_only Logical. TRUE runs the probe and stops.
#' @return Invisibly a list of everything built.
red_run <- function(.cfg = .RED_CFG, .probe_only = FALSE) {
  if (FALSE) {
    .cfg        <- .RED_CFG
    .probe_only <- TRUE
  }

  paths_ <- red_paths(.dir_root = .cfg$DirRoot, .dir_out = .cfg$DirOut,
                      .hash_redact = .cfg$HashRedact)
  red_check_inputs(.paths = paths_$In)

  if (.probe_only) return(invisible(red_probe(.paths = paths_, .primary_only = .cfg$PrimaryOnly)))

  fs::dir_create(c(paths_$Out$Root, paths_$Out$Data, paths_$Out$Scripts))

  cli::cli_h1("Small tables")
  red_export_contracts(.paths = paths_, .rerun = .cfg$Rerun)
  mrk_ <- red_export_markers(
    .dir_in      = paths_$In$RedactDir,
    .path_out    = paths_$Out$Markers,
    .compression = .cfg$Compression,
    .rerun       = .cfg$Rerun
  )
  ref_ <- red_export_cto_refs(
    .path_in     = paths_$In$CtoRefs,
    .path_out    = paths_$Out$CtoRefs,
    .compression = .cfg$Compression,
    .rerun       = .cfg$Rerun
  )
  idx_cto_ <- red_quarter_index(.path_filepaths = paths_$In$FilePaths, .pattern = .RED_DOC_TYPES[["CTO"]])
  ord_ <- red_export_cto_orders(
    .tab_refs    = ref_,
    .index       = idx_cto_,
    .path_out    = paths_$Out$CtoOrd,
    .compression = .cfg$Compression,
    .rerun       = .cfg$Rerun
  )

  cli::cli_h1("Contract text")
  idx_ex_ <- red_quarter_index(.path_filepaths = paths_$In$FilePaths,
                               .pattern = .RED_DOC_TYPES[["Exhibit10"]])
  keep_ <- red_keep_ids(.path_contracts = paths_$In$Contracts, .primary_only = .cfg$PrimaryOnly)
  ver_ <- mrk_ |>
    dplyr::slice_sample(n = min(.cfg$VerifyN, nrow(mrk_))) |>
    dplyr::select("DocID", "MarkStart", "MarkStop", "MarkText")
  txt_ <- red_export_text(
    .index       = idx_ex_,
    .keep        = keep_,
    .dir_text    = paths_$Out$Text,
    .dir_html    = if (.cfg$SplitHtml) paths_$Out$Html else NA,
    .quarters    = .cfg$Quarters,
    .verify      = ver_,
    .compression = .cfg$Compression,
    .rerun       = .cfg$Rerun
  )

  cli::cli_h1("Scripts and documentation")
  red_export_scripts(.dir_root = .cfg$DirRoot, .dir_out = paths_$Out$Scripts, .spec = .RED_SCRIPTS)

  # WHAT THE DEFECT COSTS THE PUBLISHED MEASURE, computed from the marker file rather than asserted.
  # The published definition is Symbol + Explicit, and the empty brackets all land in Symbol, so the
  # gap between these two counts is the whole of the defect's effect on the headline number.
  pub_src_ <- dplyr::filter(mrk_, .data$Kind %in% c("RedactSymbol", "RedactExplicit"))
  pub_ <- list(
    Guarded   = dplyr::n_distinct(pub_src_$DocID[pub_src_$EmptyBracket == 0L]),
    Unguarded = dplyr::n_distinct(pub_src_$DocID),
    Markers   = sum(pub_src_$EmptyBracket == 0L)
  )
  cli::cli_alert_info(
    "Published measure: {format(pub_[['Guarded']], big.mark = ',')} contracts guarded against \\
     {format(pub_[['Unguarded']], big.mark = ',')} unguarded."
  )

  con_n_ <- arrow::open_dataset(sources = paths_$Out$Contract) |>
    dplyr::summarise(N = dplyr::n()) |>
    dplyr::collect() |>
    dplyr::pull(.data$N)
  ok_ <- if (all(is.na(txt_$OkShare))) NA_real_ else {
    stats::weighted.mean(x = txt_$OkShare, w = txt_$Verified, na.rm = TRUE)
  }
  stats_ <- c(
    DATE         = format(Sys.Date(), "%Y-%m-%d"),
    COMMIT       = tryCatch(
      system2("git", c("-C", shQuote(as.character(.cfg$DirRoot)), "rev-parse", "--short", "HEAD"),
              stdout = TRUE, stderr = FALSE)[[1L]],
      error = function(e) "unknown"
    ),
    N_CONTRACTS  = format(con_n_, big.mark = ","),
    N_PRIMARY    = format(nrow(keep_), big.mark = ","),
    N_COLLAPSED  = format(sum(txt_$Collapsed, na.rm = TRUE), big.mark = ","),
    N_VERIFIED   = format(sum(txt_$Verified, na.rm = TRUE), big.mark = ","),
    OK_SHARE     = paste0(format(round(100 * ok_, 3), nsmall = 3), "%"),
    N_MARKERS      = format(nrow(mrk_), big.mark = ","),
    N_EMPTY        = format(sum(mrk_$EmptyBracket), big.mark = ","),
    N_EMPTY_WIDE   = format(sum(mrk_$EmptyBracketWide), big.mark = ","),
    N_PUB_GUARD    = format(pub_[["Guarded"]], big.mark = ","),
    N_PUB_RAW      = format(pub_[["Unguarded"]], big.mark = ","),
    N_PUB_MARKERS  = format(pub_[["Markers"]], big.mark = ","),
    N_ORDERS     = format(nrow(ord_), big.mark = ","),
    N_REFS       = format(nrow(ref_), big.mark = ","),
    N_LINKED     = format(sum(!is.na(ref_$DocIDContract)), big.mark = ",")
  )
  red_write_dictionary(.paths = paths_, .dict = .RED_DICT)
  red_write_methodology(.paths = paths_, .stats = stats_)

  writeLines(
    c("# Redaction paper data", "",
      paste0("Built ", stats_[["DATE"]], " from material-contracts at commit ", stats_[["COMMIT"]], "."),
      "", "Read `Methodology.md` first, then `DataDictionary.md`. `scripts/MANIFEST.csv` pins every",
      "shipped script to the repository it came from.", "",
      "The one caveat that changes numbers: see *The empty-bracket defect* in `Methodology.md`.", ""),
    con = as.character(paths_$Out$Readme), useBytes = TRUE
  )

  cli::cli_h1("Done")
  cli::cli_alert_success(
    "Bundle at {.path {as.character(paths_$Out$Root)}}: \\
     {red_bytes(sum(fs::dir_info(paths_$Out$Root, recurse = TRUE)$size, na.rm = TRUE))}."
  )
  if (!is.na(ok_) && ok_ < 1) {
    cli::cli_alert_danger(
      "Offset round-trip is {format(round(100 * ok_, 3), nsmall = 3)}%, not 100%. The markers do \\
       not index the shipped text everywhere; do not release until this is one."
    )
  }

  invisible(list(Markers = mrk_, Refs = ref_, Orders = ord_, Text = txt_, Stats = stats_))
}


# 10. Invocation ------------------------------------------------------------------------------------------------------
# Sourcing this file defines functions and runs nothing. Step through these in order.

if (FALSE) {
  source("ExportRedactionData.R", encoding = "UTF-8")

  # 1. Inventory. Writes nothing; a few seconds.
  probe <- red_run(.cfg = .RED_CFG, .probe_only = TRUE)

  # 2. Rehearsal on two quarters, to see real sizes and a real round-trip before the full run.
  cfg_test <- utils::modifyList(.RED_CFG, list(Quarters = c("2019-1", "2019-2")))
  test <- red_run(.cfg = cfg_test)

  # 3. The full run. Resumable: an interrupted run picks up at the first unwritten quarter.
  full <- red_run(.cfg = .RED_CFG)
}
