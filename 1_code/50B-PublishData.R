# 50B-PublishData: the data package ------------------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# It assembles everything published as data into one versioned folder, checks it against its sources, writes a
# checksum manifest and a README, and copies the folder to the shared Drive location the published repository links
# to. Nothing upstream is changed; every output is built from the files the pipeline stages wrote.
#
# THE PACKAGE
#   core/          the release files and their codebooks, the contract index, the labelled sample, the keyword tables
#   spans/         the seven span files of 04D, one file each instead of 476 chunks
#   text/          the retrieved documents -- as filed (HTML) and as text (TextRaw) -- one file per type and year
#   models/        the six fine-tuned classifiers, one zip each, and 03B's record of which are deployed
#   lexnlp/        the Docker image that extracted the entities, with its lockfile and a licence notice
#   replication/   what the paper's exhibits, numbers and appendix read beyond core/, one zip per stage
#
# DUCKDB DOES THE HEAVY WORK, IN SQL. The consolidations read hundreds of thousands of small parquet files and write a
# few large ones; DuckDB streams that without holding it in memory, and one COPY statement per output file keeps each
# step a single, inspectable query. The statements are built by pbd_sql_*() functions and run through pbd_exec() and
# pbd_query(), the only two places that touch the connection.
#
# EVERY EXPENSIVE OUTPUT IS STAMPED. A stamp is a hash of the files an output was built from (count, size, newest
# time) and of the statement that built it, kept under Stamps/ beside the stage. An output whose stamp matches is
# skipped, so a second render takes minutes, and an input that changed is rebuilt rather than served stale.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed locals; .data$ for
# existing columns, bare CamelCase for new columns; if (FALSE) dev blocks; cli/fs/here; pure ASCII; stringi never
# base substr; {(.arg)} parens in cli interpolation.


# 1. Connection and SQL --------------------------------------------------------------------------------------------------

#' Open the DuckDB connection the consolidations run on
#'
#' Insertion order is not preserved: the outputs are sets of rows, and dropping the guarantee is what lets DuckDB
#' write in parallel and spill to disk instead of buffering. The temporary directory sits on the same disk as the
#' stage, which has the room a spill needs.
#'
#' @param .threads Integer. Worker threads.
#' @param .memory Character. DuckDB memory limit, e.g. "96GB".
#' @param .dir_temp Character. Where DuckDB may spill.
#' @return A DBI connection.
pbd_connect <- function(.threads, .memory, .dir_temp) {
  if (FALSE) {
    .threads  <- 16L
    .memory   <- "96GB"
    .dir_temp <- fs::path(.dir_main, "Temp")
  }
  fs::dir_create(.dir_temp)
  con_ <- DBI::dbConnect(duckdb::duckdb())
  pbd_exec(.con = con_, .sql = sprintf("SET threads = %d", as.integer(.threads)))
  pbd_exec(.con = con_, .sql = sprintf("SET memory_limit = '%s'", .memory))
  pbd_exec(.con = con_, .sql = "SET preserve_insertion_order = false")
  pbd_exec(.con = con_, .sql = sprintf("SET temp_directory = %s", pbd_lit(.dir_temp)))
  con_
}

#' Run a statement
#'
#' @param .con A DBI connection.
#' @param .sql Character. One statement.
#' @return Invisibly, what DBI::dbExecute() returns.
pbd_exec <- function(.con, .sql) {
  invisible(DBI::dbExecute(conn = .con, statement = .sql))
}

#' Run a query and return its result
#'
#' @param .con A DBI connection.
#' @param .sql Character. One query.
#' @return A tibble.
pbd_query <- function(.con, .sql) {
  tibble::as_tibble(DBI::dbGetQuery(conn = .con, statement = .sql))
}

#' A string as an SQL literal, quotes doubled
#'
#' @param .x Character.
#' @return Character, quoted.
pbd_lit <- function(.x) {
  paste0("'", gsub("'", "''", as.character(.x), fixed = TRUE), "'")
}

#' The SQL that copies a query into a parquet file
#'
#' @param .query Character. A SELECT statement.
#' @param .path Character. Destination.
#' @param .row_group Integer. Rows per row group.
#' @return Character, one statement.
pbd_sql_copy <- function(.query, .path, .row_group) {
  sprintf(
    "COPY (%s) TO %s (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE %d)",
    .query, pbd_lit(.path), as.integer(.row_group)
  )
}


# 2. Stamps and small helpers --------------------------------------------------------------------------------------------

#' The stamp of an output's inputs, listed without fs
#'
#' A stamp is a hash of three numbers -- how many files the inputs hold, how many bytes, and the newest modification
#' time among them -- together with whatever the caller adds. utils_dir_stamp() gets those numbers from fs, and fs is
#' not safe on this machine for directories of this size: it errors inconsistently and has crashed the session
#' outright (r-lib/fs#281). The listing is therefore done with base R, which is slower and reliable, and the key is
#' assembled exactly as utils_dir_stamp() assembles it.
#'
#' THE VALUE IS THE SAME, which is the point: an output stamped by the old function is not rebuilt by the new one.
#' Where the two differ, it is because fs had miscounted, and one rebuild puts that right for good.
#'
#' Hidden files are skipped and directories are not counted, which is what fs::dir_info(type = "file") does; a tree
#' holding symbolic links would count differently, and none of the inputs here holds any.
#'
#' @param .dirs Character. Directories or files whose state the stamp covers.
#' @param .extra List or NULL. Anything else it must cover, such as the statement that builds the output.
#' @return Character, the stamp.
pbd_stamp <- function(.dirs, .extra = NULL) {
  if (FALSE) {
    .dirs  <- fs::path(.lP$Input$DirRelease, "Contracts.parquet")
    .extra <- list(Query = "SELECT 1")
  }
  paths_ <- as.character(.dirs)
  paths_ <- paths_[file.exists(paths_)]
  isdir_ <- dir.exists(paths_)
  files_ <- c(
    if (any(isdir_)) {
      unlist(
        lapply(
          X   = paths_[isdir_],
          FUN = \(.d) list.files(
            path         = .d,      # one input directory
            all.files    = FALSE,   # hidden entries are not part of the data
            full.names   = TRUE,    # file.info() needs the whole path
            recursive    = TRUE,    # the quarter folders hold the documents
            include.dirs = FALSE,   # directories are not files
            no..         = TRUE
          )
        ),
        use.names = FALSE
      )
    },
    paths_[!isdir_]
  )
  inf_ <- if (length(files_) > 0L) file.info(files_, extra_cols = FALSE) else NULL
  any_ <- !is.null(inf_) && nrow(inf_) > 0L
  key_ <- list(
    nFiles = if (any_) nrow(inf_) else 0L,
    nBytes = if (any_) sum(as.numeric(inf_$size)) else 0,
    Latest = if (any_) round(as.numeric(max(inf_$mtime))) else 0,
    Extra  = .extra
  )
  stringi::stri_sub(str = rlang::hash(key_), from = 1L, to = 16L)
}

#' Where the stamp of an output lives
#'
#' @param .dir_stamps Character. The stamp directory.
#' @param .rel Character. The output, relative to the stage.
#' @return Character.
pbd_stamp_path <- function(.dir_stamps, .rel) {
  fs::path(.dir_stamps, paste0(gsub("[/\\\\]", "__", .rel), ".stamp"))
}

#' Whether an output exists and was built from exactly these inputs
#'
#' @param .path_out Character. The output.
#' @param .path_stamp Character. Its stamp.
#' @param .stamp Character. The stamp its inputs give now.
#' @return Logical.
pbd_is_current <- function(.path_out, .path_stamp, .stamp) {
  fs::file_exists(.path_out) && fs::file_exists(.path_stamp) &&
    identical(readLines(con = .path_stamp, warn = FALSE)[1], .stamp)
}

#' Record the stamp of an output
#'
#' @param .path_stamp Character. Where.
#' @param .stamp Character. What.
#' @return Invisibly, the path.
pbd_stamp_write <- function(.path_stamp, .stamp) {
  fs::dir_create(fs::path_dir(.path_stamp))
  writeLines(text = .stamp, con = .path_stamp)
  invisible(.path_stamp)
}

#' Move a finished temporary file into place
#'
#' Outputs are written under a temporary name and renamed at the end, so an interrupted run never leaves a partial
#' file that a later run would take for a finished one.
#'
#' @param .tmp,.path Character. From, to.
#' @return Invisibly, .path.
pbd_promote <- function(.tmp, .path) {
  fs::dir_create(fs::path_dir(.path))
  if (fs::file_exists(.path)) fs::file_delete(.path)
  fs::file_move(path = .tmp, new_path = .path)
  invisible(.path)
}

#' Write a small text file only when its content changes
#'
#' A file rewritten with identical content would get a new time, and the manifest and the deployment would treat it
#' as new on every render. Comparing first keeps an unchanged file untouched.
#'
#' @param .lines Character. The content.
#' @param .path Character. The file.
#' @return Invisibly, "written" or "current".
pbd_write_lines <- function(.lines, .path) {
  if (FALSE) {
    .lines <- c("a", "b")
    .path  <- fs::path(tempdir(), "x.txt")
  }
  fs::dir_create(fs::path_dir(.path))
  tmp_ <- paste0(.path, ".tmp")
  writeLines(text = .lines, con = tmp_)
  if (fs::file_exists(.path) && identical(unname(tools::md5sum(tmp_)), unname(tools::md5sum(.path)))) {
    fs::file_delete(tmp_)
    return(invisible("current"))
  }
  pbd_promote(.tmp = tmp_, .path = .path)
  invisible("written")
}

#' Write a small csv only when its content changes
#'
#' @param .tab Tibble.
#' @param .path Character. The file.
#' @return Invisibly, "written" or "current".
pbd_write_csv <- function(.tab, .path) {
  if (FALSE) {
    .tab  <- tibble::tibble(a = 1)
    .path <- fs::path(tempdir(), "x.csv")
  }
  pbd_write_lines(.lines = sub("\n$", "", readr::format_csv(x = .tab, na = "")), .path = .path)
}

#' Where Google Drive for desktop keeps "My Drive" on this machine
#'
#' THE ACCOUNT IS FOUND, NOT WRITTEN DOWN. Drive for desktop mounts each account under
#' ~/Library/CloudStorage/GoogleDrive-<account>; the one folder of that shape is used, so no address appears in the
#' code. More than one account stops the document and lists them.
#'
#' @param .dir_cloud Character. The CloudStorage directory.
#' @return Character, the "My Drive" directory, or NA when Drive for desktop is not installed.
pbd_drive_root <- function(.dir_cloud = fs::path_expand("~/Library/CloudStorage")) {
  if (FALSE) {
    .dir_cloud <- fs::path_expand("~/Library/CloudStorage")
  }
  if (!fs::dir_exists(.dir_cloud)) return(NA_character_)
  accounts_ <- fs::dir_ls(path = .dir_cloud, type = "directory", regexp = "/GoogleDrive-[^/]+$")
  if (length(accounts_) == 0L) return(NA_character_)
  if (length(accounts_) > 1L) {
    cli::cli_abort("More than one Google Drive account is mounted: {.path {accounts_}}. Name the one to use.")
  }
  as.character(fs::path(accounts_, "My Drive"))
}


# 3. core/ ----------------------------------------------------------------------------------------------------------------

#' Copy files into the stage, skipping those already there
#'
#' A file counts as already there when the copy has the same size and is not older than the source.
#'
#' @param .paths Character. Source files.
#' @param .dir_out Character. Destination directory.
#' @return Tibble: File, Bytes, Status ("copied", "current").
pbd_copy <- function(.paths, .dir_out) {
  if (FALSE) {
    .paths   <- fs::path(.lP$Input$DirRelease, c("Contracts.parquet", "Contracts_Codebook.csv"))
    .dir_out <- fs::path(.lP$Output$DirPackage, "core")
  }
  missing_ <- .paths[!fs::file_exists(.paths)]
  if (length(missing_) > 0L) cli::cli_abort("Missing input{?s}: {.file {missing_}}")
  fs::dir_create(.dir_out)
  dst_ <- fs::path(.dir_out, fs::path_file(.paths))
  src_info_ <- fs::file_info(.paths)
  dst_info_ <- fs::file_info(dst_)
  current_ <- !is.na(dst_info_$size) & dst_info_$size == src_info_$size &
    dst_info_$modification_time >= src_info_$modification_time
  if (any(!current_)) fs::file_copy(path = .paths[!current_], new_path = dst_[!current_], overwrite = TRUE)
  tibble::tibble(
    File   = as.character(fs::path_file(.paths)),
    Bytes  = as.numeric(src_info_$size),
    Status = ifelse(current_, "current", "copied")
  )
}

#' Copy a whole directory into the stage, skipping files already there
#'
#' @param .dir_in Character. Source directory.
#' @param .dir_out Character. Destination directory.
#' @return Tibble as pbd_copy(), File relative to .dir_in.
pbd_copy_dir <- function(.dir_in, .dir_out) {
  if (FALSE) {
    .dir_in  <- .lP$Input$DirKeywordTables
    .dir_out <- fs::path(.lP$Output$DirPackage, "core", "keyword_tables")
  }
  if (!fs::dir_exists(.dir_in)) cli::cli_abort("Missing input directory {.path {(.dir_in)}}")
  files_ <- fs::dir_ls(path = .dir_in, recurse = TRUE, type = "file")
  rel_ <- fs::path_rel(path = files_, start = .dir_in)
  purrr::map2(files_, rel_, \(.f, .r) {
    pbd_copy(.paths = .f, .dir_out = fs::path(.dir_out, fs::path_dir(.r))) |>
      dplyr::mutate(File = as.character(.r))
  }) |>
    purrr::list_rbind()
}

#' The release's contract file, with its EDGAR links repaired
#'
#' THE RELEASE IS PUBLISHED AS WRITTEN, WITH ONE EXCEPTION. Every `UrlIndexPage` the pipeline writes carries a
#' single slash after the scheme -- `https:/www.sec.gov/...` -- which no browser and no client resolves. The repair
#' collapses the slashes after the scheme back to two, in both link columns, and the counts it reports say how many
#' rows each column needed. Nothing else about the file is touched, and the fault is fixed upstream for the next
#' version.
#'
#' The file is written by DuckDB rather than copied, so its row groups and compression are DuckDB's; the rows, the
#' columns and their types are those of the release.
#'
#' @param .con A DBI connection.
#' @param .path_src Character. The release file.
#' @param .path_out Character. Destination.
#' @param .path_stamp Character. Its stamp; rebuilt only when the release or the statement changed.
#' @return Tibble: Rows, FixedIndexPage, FixedDocument, LeftBroken, Status.
pbd_contracts <- function(.con, .path_src, .path_out, .path_stamp) {
  if (FALSE) {
    .con        <- con
    .path_src   <- fs::path(.lP$Input$DirRelease, "Contracts.parquet")
    .path_out   <- fs::path(.lP$Output$DirPackage, "core", "Contracts.parquet")
    .path_stamp <- pbd_stamp_path(.lP$Output$DirStamps, "core/Contracts.parquet")
  }
  # ^https:/+ collapses one slash or two back to exactly two, so a repaired row and a correct one look the same.
  query_ <- sprintf(
    paste(
      "SELECT * REPLACE (",
      "regexp_replace(UrlIndexPage, '^https:/+', 'https://') AS UrlIndexPage,",
      "regexp_replace(UrlDocument, '^https:/+', 'https://') AS UrlDocument",
      ") FROM read_parquet(%s)"
    ),
    pbd_lit(.path_src)
  )
  before_ <- pbd_query(
    .con = .con,
    .sql = sprintf(
      paste(
        "SELECT count(*) AS Rows,",
        "count(*) FILTER (WHERE UrlIndexPage NOT LIKE 'https://%%') AS FixedIndexPage,",
        "count(*) FILTER (WHERE UrlDocument NOT LIKE 'https://%%') AS FixedDocument",
        "FROM read_parquet(%s)"
      ),
      pbd_lit(.path_src)
    )
  )
  stamp_ <- pbd_stamp(.dirs = .path_src, .extra = list(Query = query_))
  status_ <- "current"
  if (!pbd_is_current(.path_out = .path_out, .path_stamp = .path_stamp, .stamp = stamp_)) {
    tmp_ <- paste0(.path_out, ".tmp")
    fs::dir_create(fs::path_dir(.path_out))
    pbd_exec(.con = .con, .sql = pbd_sql_copy(.query = query_, .path = tmp_, .row_group = 100000L))
    pbd_promote(.tmp = tmp_, .path = .path_out)
    pbd_stamp_write(.path_stamp = .path_stamp, .stamp = stamp_)
    status_ <- "built"
  }
  after_ <- pbd_query(
    .con = .con,
    .sql = sprintf(
      paste(
        "SELECT count(*) AS Rows,",
        "count(*) FILTER (WHERE UrlIndexPage NOT LIKE 'https://%%' OR UrlDocument NOT LIKE 'https://%%')",
        "AS LeftBroken FROM read_parquet(%s)"
      ),
      pbd_lit(.path_out)
    )
  )
  tibble::tibble(
    Rows           = as.integer(after_$Rows),
    FixedIndexPage = as.integer(before_$FixedIndexPage),
    FixedDocument  = as.integer(before_$FixedDocument),
    LeftBroken     = as.integer(after_$LeftBroken),
    Status         = status_
  )
}

#' The contract index: one row per contract copy, for readers who want a list rather than a database
#'
#' The columns are the ones a reader needs to find a contract and to decide whether it concerns them: who filed it,
#' in which filing, what it is, whether parts were withheld, and where it is on EDGAR. The accession number is read
#' off the filing's index address, where EDGAR writes it in its dashed form; an address it cannot be read from leaves
#' it empty and is counted by the validation.
#'
#' @param .con A DBI connection.
#' @param .path_contracts Character. The published contract file, whose links are already repaired.
#' @param .path_out Character. The gzipped csv to write.
#' @param .path_stamp Character. Its stamp; the file is rebuilt only when the release or the query changed.
#' @return Invisibly, a one-row tibble: Rows, NoAccession.
pbd_contract_index <- function(.con, .path_contracts, .path_out, .path_stamp) {
  if (FALSE) {
    .con            <- con
    .path_contracts <- fs::path(.lP$Input$DirRelease, "Contracts.parquet")
    .path_out       <- fs::path(.lP$Output$DirPackage, "core", "ContractIndex.csv.gz")
    .path_stamp     <- pbd_stamp_path(.lP$Output$DirStamps, "core/ContractIndex.csv.gz")
  }
  query_ <- sprintf(
    paste(
      "SELECT DocID,",
      "NULLIF(regexp_extract(UrlIndexPage, '([0-9]{10}-[0-9]{2}-[0-9]{6})', 1), '') AS AccessionNumber,",
      "CIK, CompanyName, FormType, DateFiled, DocName, DocSeq, DocDesc,",
      "Class, ClassBroad, AmendType, HasCto, nRedactExplicit, nRedactSymbol, nRedactBlank,",
      "PrimaryFiler, Removed, RemClass, UrlDocument, UrlIndexPage",
      "FROM read_parquet(%s)"
    ),
    pbd_lit(.path_contracts)
  )
  stamp_ <- pbd_stamp(.dirs = .path_contracts, .extra = list(Query = query_))
  if (!pbd_is_current(.path_out = .path_out, .path_stamp = .path_stamp, .stamp = stamp_)) {
    tmp_ <- paste0(.path_out, ".tmp.csv.gz")
    fs::dir_create(fs::path_dir(.path_out))
    pbd_exec(.con = .con, .sql = sprintf("COPY (%s) TO %s (FORMAT CSV, HEADER, COMPRESSION GZIP)", query_, pbd_lit(tmp_)))
    pbd_promote(.tmp = tmp_, .path = .path_out)
    pbd_stamp_write(.path_stamp = .path_stamp, .stamp = stamp_)
  }
  out_ <- pbd_query(
    .con = .con,
    .sql = sprintf(
      "SELECT count(*) AS Rows, count(*) FILTER (WHERE AccessionNumber IS NULL) AS NoAccession FROM (%s)",
      query_
    )
  )
  invisible(dplyr::mutate(out_, dplyr::across(dplyr::everything(), as.integer)))
}

#' The labelled sample without its texts
#'
#' 03A's prepared file is what every classifier was trained and scored on, with the folds that made the scores out
#' of sample. The texts are dropped because they are in text/ already, keyed on the same DocID.
#'
#' @param .con A DBI connection.
#' @param .path_prepared Character. 03A's prepared.parquet.
#' @param .path_out Character. Destination parquet.
#' @param .path_stamp Character. Its stamp; the file is rebuilt only when 03A's file changed.
#' @return Invisibly, a one-row tibble: Rows, Columns.
pbd_labels <- function(.con, .path_prepared, .path_out, .path_stamp) {
  if (FALSE) {
    .con           <- con
    .path_prepared <- .lP$Input$FilPrepared
    .path_out      <- fs::path(.lP$Output$DirPackage, "core", "ClassificationLabels.parquet")
    .path_stamp    <- pbd_stamp_path(.lP$Output$DirStamps, "core/ClassificationLabels.parquet")
  }
  query_ <- sprintf("SELECT * EXCLUDE (Text) FROM read_parquet(%s)", pbd_lit(.path_prepared))
  stamp_ <- pbd_stamp(.dirs = .path_prepared, .extra = list(Query = query_))
  if (!pbd_is_current(.path_out = .path_out, .path_stamp = .path_stamp, .stamp = stamp_)) {
    tmp_ <- paste0(.path_out, ".tmp")
    fs::dir_create(fs::path_dir(.path_out))
    pbd_exec(.con = .con, .sql = pbd_sql_copy(.query = query_, .path = tmp_, .row_group = 100000L))
    pbd_promote(.tmp = tmp_, .path = .path_out)
    pbd_stamp_write(.path_stamp = .path_stamp, .stamp = stamp_)
  }
  cols_ <- pbd_query(.con = .con, .sql = sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", pbd_lit(.path_out)))
  rows_ <- pbd_query(.con = .con, .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s)", pbd_lit(.path_out)))
  invisible(tibble::tibble(Rows = as.integer(rows_$n), Columns = paste(cols_$column_name, collapse = ", ")))
}


# 4. spans/ ---------------------------------------------------------------------------------------------------------------

#' One file per span type, from 04D's chunks
#'
#' THE CHUNKS ARE READ BY NAME, so 04D's bookkeeping files (`_done`, `_facts`) never enter a span file. Columns are
#' unified by name across the 476 chunks, and a column typed differently in different chunks -- an offset stored as
#' an integer in some and as a double in others -- is written in the wider type, so no value is cut.
#'
#' Rows are ordered by document, so the row groups of the written file follow documents and a reader filtering on
#' DocID skips most of the file.
#'
#' @param .con A DBI connection.
#' @param .specs Tibble: Stem, Pass, DirHash (the 04D directory holding the chunks).
#' @param .dir_out Character. Destination directory.
#' @param .dir_stamps Character. Stamp directory.
#' @param .rel_out Character. .dir_out relative to the stage, for the stamp names.
#' @param .rerun Logical. TRUE rebuilds regardless of the stamps.
#' @return Tibble: Stem, Pass, Chunks, RowsIn, RowsOut, Bytes, Status.
pbd_spans <- function(.con, .specs, .dir_out, .dir_stamps, .rel_out, .rerun = FALSE) {
  if (FALSE) {
    .con        <- con
    .specs      <- specs_spans
    .dir_out    <- fs::path(.lP$Output$DirPackage, "spans")
    .dir_stamps <- .lP$Output$DirStamps
    .rel_out    <- "spans"
    .rerun      <- FALSE
  }
  fs::dir_create(.dir_out)
  purrr::pmap(.specs, \(Stem, Pass, DirHash) {
    chunks_ <- fs::dir_ls(path = DirHash, type = "directory", regexp = "/chunk-[^/]+$")
    files_ <- fs::path(chunks_, paste0(Stem, ".parquet"))
    files_ <- files_[fs::file_exists(files_)]
    if (length(files_) == 0L) cli::cli_abort("No {.file {Stem}.parquet} under {.path {DirHash}}")
    glob_ <- fs::path(DirHash, "chunk-*", paste0(Stem, ".parquet"))
    query_ <- sprintf(
      "SELECT * FROM read_parquet(%s, union_by_name = true) ORDER BY DocID",
      pbd_lit(glob_)
    )
    path_out_ <- fs::path(.dir_out, paste0(Stem, ".parquet"))
    path_stamp_ <- pbd_stamp_path(.dir_stamps = .dir_stamps, .rel = fs::path(.rel_out, fs::path_file(path_out_)))
    stamp_ <- pbd_stamp(.dirs = files_, .extra = list(Query = query_))
    rows_in_ <- pbd_query(
      .con = .con,
      .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s, union_by_name = true)", pbd_lit(glob_))
    )$n
    status_ <- "current"
    if (.rerun || !pbd_is_current(.path_out = path_out_, .path_stamp = path_stamp_, .stamp = stamp_)) {
      tmp_ <- paste0(path_out_, ".tmp")
      pbd_exec(.con = .con, .sql = pbd_sql_copy(.query = query_, .path = tmp_, .row_group = 1000000L))
      pbd_promote(.tmp = tmp_, .path = path_out_)
      pbd_stamp_write(.path_stamp = path_stamp_, .stamp = stamp_)
      status_ <- "built"
    }
    rows_out_ <- pbd_query(.con = .con, .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s)", pbd_lit(path_out_)))$n
    tibble::tibble(
      Stem    = Stem,
      Pass    = Pass,
      Chunks  = length(files_),
      RowsIn  = as.integer(rows_in_),
      RowsOut = as.integer(rows_out_),
      Bytes   = as.numeric(fs::file_size(path_out_)),
      Status  = status_
    )
  }) |>
    purrr::list_rbind()
}

#' The column list of every file in a directory, as a codebook to be completed
#'
#' Names and types are read from the files; the meaning column is left for the documentation to fill, so the
#' codebook can never disagree with the data about what columns exist.
#'
#' @param .con A DBI connection.
#' @param .dir Character. Directory of parquet files.
#' @param .path_out Character. The csv to write.
#' @return Invisibly, the codebook.
pbd_codebook <- function(.con, .dir, .path_out) {
  if (FALSE) {
    .con      <- con
    .dir      <- fs::path(.lP$Output$DirPackage, "spans")
    .path_out <- fs::path(.lP$Output$DirPackage, "spans", "Spans_Codebook.csv")
  }
  files_ <- fs::dir_ls(path = .dir, glob = "*.parquet")
  out_ <- purrr::map(files_, \(.f) {
    pbd_query(.con = .con, .sql = sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", pbd_lit(.f))) |>
      dplyr::transmute(
        File    = as.character(fs::path_file(.f)),
        Column  = .data$column_name,
        Type    = .data$column_type,
        Meaning = NA_character_
      )
  }) |>
    purrr::list_rbind()
  pbd_write_csv(.tab = out_, .path = .path_out)
  invisible(out_)
}


# 5. text/ ----------------------------------------------------------------------------------------------------------------

#' The documents the release refers to, one key table per release file
#'
#' A parsed document is published when the release refers to it: an Exhibit 10 when it is a row of Contracts, a
#' current report or its amendment when it is a row of Summaries, an order when it is a row of CtoOrders. Documents
#' the pipeline retrieved but the release does not use are left out and counted.
#'
#' @param .con A DBI connection.
#' @param .specs Tibble with at least Table (key table to create), File (release parquet), Column (its document key).
#' @return Invisibly, a tibble: Table, Keys.
pbd_text_keys <- function(.con, .specs) {
  if (FALSE) {
    .con   <- con
    .specs <- specs_text
  }
  purrr::pmap(dplyr::distinct(dplyr::select(.specs, "Table", "File", "Column")), \(Table, File, Column) {
    pbd_exec(
      .con = .con,
      .sql = sprintf(
        "CREATE OR REPLACE TABLE %s AS SELECT DISTINCT %s AS DocID FROM read_parquet(%s) WHERE %s IS NOT NULL",
        Table, Column, pbd_lit(File), Column
      )
    )
    n_ <- pbd_query(.con = .con, .sql = sprintf("SELECT count(*) AS n FROM %s", Table))$n
    tibble::tibble(Table = Table, Keys = as.integer(n_))
  }) |>
    purrr::list_rbind() |>
    invisible()
}

#' One file per document type and year, from 01B's one file per document
#'
#' The document as filed (`HTML`) and its text (`TextRaw`) are both kept; the standardised text is not, since
#' rGetEDGAR::standardize_text() derives it from `TextRaw`. The quarter the document was filed in is read off its
#' directory, and the parse diagnostics travel with it.
#'
#' A year is built only when its stamp -- the quarter directories it reads, the release keys, the statement --
#' has changed, and a year with no release document produces no file.
#'
#' @param .con A DBI connection.
#' @param .specs Tibble: Type, Dir (01B's directory for the type), Slug (file prefix), Table (key table).
#' @param .dir_out Character. Destination; one subdirectory per slug.
#' @param .dir_stamps Character. Stamp directory.
#' @param .rel_out Character. .dir_out relative to the stage.
#' @param .extra List. Anything else the stamp must cover (the release stamps).
#' @param .row_group Integer. Rows per row group; documents are large.
#' @param .years Character or NULL. Only these years; NULL builds every year, character(0) none. A test run
#'   builds one; none republishes what is already built without reading the documents again.
#' @param .rerun Logical. TRUE rebuilds regardless of the stamps.
#' @return Tibble: Type, Year, Quarters, RowsIn, RowsOut, Bytes, Seconds, Status.
pbd_text <- function(.con, .specs, .dir_out, .dir_stamps, .rel_out, .extra, .row_group = 1000L, .years = NULL,
                     .rerun = FALSE) {
  if (FALSE) {
    .con        <- con
    .specs      <- specs_text
    .dir_out    <- fs::path(.lP$Output$DirPackage, "text")
    .dir_stamps <- .lP$Output$DirStamps
    .rel_out    <- "text"
    .extra      <- list()
    .row_group  <- 1000L
    .years      <- "2012"
    .rerun      <- FALSE
  }
  purrr::pmap(.specs, \(Type, Dir, Slug, Table, ...) {
    dirs_yq_ <- sort(fs::dir_ls(path = Dir, type = "directory", regexp = "/[0-9]{4}-[1-4]$"))
    years_ <- unique(stringi::stri_sub(fs::path_file(dirs_yq_), from = 1L, to = 4L))
    if (!is.null(.years)) years_ <- intersect(years_, as.character(.years))
    purrr::map(years_, \(.year) {
      t0_ <- Sys.time()
      dirs_ <- dirs_yq_[startsWith(fs::path_file(dirs_yq_), paste0(.year, "-"))]
      glob_ <- fs::path(Dir, paste0(.year, "-*"), "*.parquet")
      query_ <- sprintf(
        paste(
          "SELECT p.DocID, regexp_extract(p.filename, '([0-9]{4}-[1-4])[/\\\\][^/\\\\]*$', 1) AS YQ,",
          "p.DocExt, p.ErrParse, p.MsgParse, p.HTML, p.TextRaw",
          "FROM read_parquet(%s, filename = true, union_by_name = true) AS p",
          "WHERE p.DocID IN (SELECT DocID FROM %s)"
        ),
        pbd_lit(glob_), Table
      )
      rel_ <- fs::path(.rel_out, Slug, paste0(Slug, "_", .year, ".parquet"))
      path_out_ <- fs::path(.dir_out, Slug, paste0(Slug, "_", .year, ".parquet"))
      path_stamp_ <- pbd_stamp_path(.dir_stamps = .dir_stamps, .rel = rel_)
      stamp_ <- pbd_stamp(.dirs = dirs_, .extra = c(.extra, list(Query = query_)))
      rows_in_ <- pbd_query(
        .con = .con,
        .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s, union_by_name = true)", pbd_lit(glob_))
      )$n
      status_ <- "current"
      if (.rerun || !pbd_is_current(.path_out = path_out_, .path_stamp = path_stamp_, .stamp = stamp_)) {
        fs::dir_create(fs::path_dir(path_out_))
        tmp_ <- paste0(path_out_, ".tmp")
        pbd_exec(.con = .con, .sql = pbd_sql_copy(.query = query_, .path = tmp_, .row_group = .row_group))
        n_tmp_ <- pbd_query(.con = .con, .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s)", pbd_lit(tmp_)))$n
        if (n_tmp_ == 0L) {
          fs::file_delete(tmp_)
          if (fs::file_exists(path_out_)) fs::file_delete(path_out_)
          status_ <- "no release documents"
        } else {
          pbd_promote(.tmp = tmp_, .path = path_out_)
          status_ <- "built"
        }
        pbd_stamp_write(.path_stamp = path_stamp_, .stamp = stamp_)
      } else if (!fs::file_exists(path_out_)) {
        status_ <- "no release documents"
      }
      rows_out_ <- if (fs::file_exists(path_out_)) {
        pbd_query(.con = .con, .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s)", pbd_lit(path_out_)))$n
      } else {
        0L
      }
      out_ <- tibble::tibble(
        Type     = Type,
        Year     = .year,
        Quarters = length(dirs_),
        RowsIn   = as.integer(rows_in_),
        RowsOut  = as.integer(rows_out_),
        Bytes    = if (fs::file_exists(path_out_)) as.numeric(fs::file_size(path_out_)) else 0,
        Seconds  = round(as.numeric(difftime(Sys.time(), t0_, units = "secs"))),
        Status   = status_
      )
      if (status_ == "built") {
        cli::cli_alert_success(
          "{Type} {(.year)}: {format(out_$RowsOut, big.mark = ',')} {cli::qty(out_$RowsOut)}document{?s}, \\
           {format(fs::as_fs_bytes(out_$Bytes))}, {out_$Seconds}s"
        )
      }
      out_
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind() |>
    (\(.t) dplyr::bind_rows(
      # The skeleton keeps the columns when no year is built at all (.years = character(0) publishes the text that is
      # already there without touching it), so every report and check below still finds them.
      tibble::tibble(
        Type = character(0), Year = character(0), Quarters = integer(0), RowsIn = integer(0), RowsOut = integer(0),
        Bytes = numeric(0), Seconds = numeric(0), Status = character(0)
      ),
      .t
    ))()
}

#' Release documents that have no text in the package
#'
#' The complement of pbd_text()'s filter, per key table: a key the release holds for which no published text file of
#' the types sharing that table has a row. A nonzero count is a document the pipeline used but did not retrieve, or
#' retrieved under another key.
#'
#' @param .con A DBI connection.
#' @param .specs Tibble as pbd_text().
#' @param .dir_out Character. pbd_text()'s destination.
#' @return Tibble: Table, Types, Keys, WithText, WithoutText, Duplicates.
pbd_text_coverage <- function(.con, .specs, .dir_out) {
  if (FALSE) {
    .con     <- con
    .specs   <- specs_text
    .dir_out <- fs::path(.lP$Output$DirPackage, "text")
  }
  purrr::map(unique(.specs$Table), \(.table) {
    slugs_ <- .specs$Slug[.specs$Table == .table]
    globs_ <- fs::path(.dir_out, slugs_, "*.parquet")
    have_ <- purrr::map_lgl(slugs_, \(.s) length(fs::dir_ls(path = fs::path(.dir_out, .s), glob = "*.parquet",
                                                             fail = FALSE)) > 0L)
    keys_ <- pbd_query(.con = .con, .sql = sprintf("SELECT count(*) AS n FROM %s", .table))$n
    with_ <- 0
    dup_ <- 0
    if (any(have_)) {
      list_ <- paste0("[", paste(pbd_lit(globs_[have_]), collapse = ", "), "]")
      with_ <- pbd_query(
        .con = .con,
        .sql = sprintf(
          "SELECT count(*) AS n FROM %s AS k WHERE k.DocID IN (SELECT DocID FROM read_parquet(%s))",
          .table, list_
        )
      )$n
      # A document is published once: the same DocID in two files, or twice in one, is a duplicate.
      dup_ <- pbd_query(
        .con = .con,
        .sql = sprintf("SELECT count(*) - count(DISTINCT DocID) AS n FROM read_parquet(%s)", list_)
      )$n
    }
    tibble::tibble(
      Table       = .table,
      Types       = paste(.specs$Type[.specs$Table == .table], collapse = ", "),
      Keys        = as.integer(keys_),
      WithText    = as.integer(with_),
      WithoutText = as.integer(keys_) - as.integer(with_),
      Duplicates  = as.integer(dup_)
    )
  }) |>
    purrr::list_rbind()
}


# 6. models/, lexnlp/, replication/ ------------------------------------------------------------------------------------

#' One zip per fine-tuned model, and the record of which are deployed
#'
#' A model directory is named by its whole configuration; the zip is named by the two things a user chooses between
#' -- the task and the context length -- and an index maps each zip back to the directory it came from.
#'
#' @param .dir_models Character. 03B's model_final directory.
#' @param .dir_out Character. Destination.
#' @param .dir_stamps Character. Stamp directory.
#' @param .rel_out Character. .dir_out relative to the stage.
#' @param .rerun Logical. TRUE rebuilds regardless of the stamps.
#' @return Tibble: Zip, Directory, Task, Context, Bytes, Status.
pbd_models <- function(.dir_models, .dir_out, .dir_stamps, .rel_out, .rerun = FALSE) {
  if (FALSE) {
    .dir_models <- .lP$Input$DirModels
    .dir_out    <- fs::path(.lP$Output$DirPackage, "models")
    .dir_stamps <- .lP$Output$DirStamps
    .rel_out    <- "models"
    .rerun      <- FALSE
  }
  fs::dir_create(.dir_out)
  dirs_ <- fs::dir_ls(path = .dir_models, type = "directory", regexp = "__FINAL$")
  if (length(dirs_) == 0L) cli::cli_abort("No model directory ending in __FINAL under {.path {(.dir_models)}}")
  out_ <- purrr::map(dirs_, \(.d) {
    name_ <- as.character(fs::path_file(.d))
    task_ <- sub("__.*$", "", name_)
    ctx_ <- stringi::stri_extract_first_regex(name_, "(?<=_L)[0-9]+(?=_)")
    zip_ <- paste0(task_, "_L", ctx_, ".zip")
    path_zip_ <- fs::path(.dir_out, zip_)
    path_stamp_ <- pbd_stamp_path(.dir_stamps = .dir_stamps, .rel = fs::path(.rel_out, zip_))
    stamp_ <- pbd_stamp(.dirs = .d, .extra = list(Zip = zip_))
    status_ <- "current"
    if (.rerun || !pbd_is_current(.path_out = path_zip_, .path_stamp = path_stamp_, .stamp = stamp_)) {
      tmp_ <- fs::path(.dir_out, paste0(zip_, ".tmp"))
      if (fs::file_exists(tmp_)) fs::file_delete(tmp_)
      # Weights barely compress; a low level keeps the zip fast to write and to open.
      zip::zip(zipfile = tmp_, files = name_, root = .dir_models, mode = "mirror", compression_level = 1)
      pbd_promote(.tmp = tmp_, .path = path_zip_)
      pbd_stamp_write(.path_stamp = path_stamp_, .stamp = stamp_)
      status_ <- "built"
    }
    tibble::tibble(Zip = zip_, Directory = name_, Task = task_, Context = as.integer(ctx_),
                   Bytes = as.numeric(fs::file_size(path_zip_)), Status = status_)
  }) |>
    purrr::list_rbind()
  if (anyDuplicated(out_$Zip) > 0L) {
    cli::cli_abort("Two model directories map to the same zip name: {.val {out_$Zip[duplicated(out_$Zip)]}}")
  }
  pbd_write_csv(
    .tab  = dplyr::select(out_, "Zip", "Directory", "Task", "Context"),
    .path = fs::path(.dir_out, "models_index.csv")
  )
  deployed_ <- fs::path(.dir_models, "deployed.parquet")
  if (fs::file_exists(deployed_)) pbd_copy(.paths = deployed_, .dir_out = .dir_out)
  out_
}

#' The LexNLP image, its lockfile, and the notice its licence requires
#'
#' LexNLP is licensed under the AGPL-3.0. The image is published as the exact artifact that extracted the
#' entities; the notice names the licence and where LexNLP's source for the pinned version is found.
#'
#' @param .path_image Character. The image tarball.
#' @param .path_lock Character. requirements.lock.txt.
#' @param .dir_out Character. Destination.
#' @return Tibble as pbd_copy(), with the notice added.
pbd_lexnlp <- function(.path_image, .path_lock, .dir_out) {
  if (FALSE) {
    .path_image <- .lP$Input$FilLexnlpImage
    .path_lock  <- .lP$Input$FilLexnlpLock
    .dir_out    <- fs::path(.lP$Output$DirPackage, "lexnlp")
  }
  out_ <- pbd_copy(.paths = c(.path_image, .path_lock), .dir_out = .dir_out)
  notice_ <- c(
    "# LexNLP image -- notice",
    "",
    sprintf("`%s` is the Docker image that extracted the organisation, date, place and money", fs::path_file(.path_image)),
    "candidates of the database. Load it with `docker load -i <file>`; the pipeline's 04C stage shows the call it",
    "makes.",
    "",
    "The image contains LexNLP 2.3.0, which is licensed under the GNU Affero General Public License v3.0",
    "(AGPL-3.0). Its source for that version is available from the Python Package Index",
    "(https://pypi.org/project/lexnlp/2.3.0/) and from https://github.com/LexPredict/lexpredict-lexnlp.",
    "The other packages in the image are listed, with their versions, in `requirements.lock.txt`; each is",
    "distributed under its own licence.",
    "",
    "The wrapper code that runs LexNLP is in the published repository under `pipeline/contracts-lexnlp/`,",
    "licensed AGPL-3.0-or-later.",
    ""
  )
  status_ <- pbd_write_lines(.lines = notice_, .path = fs::path(.dir_out, "NOTICE.md"))
  dplyr::bind_rows(out_, tibble::tibble(File = "NOTICE.md", Bytes = NA_real_, Status = status_))
}

#' One zip per pipeline stage, holding what the paper's documents read from it
#'
#' Each zip unpacks into `2_output/` in the layout the pipeline reads, so a reader who unpacks it beside the
#' published code can render the exhibits, numbers and appendix without rebuilding the stage.
#'
#' @param .specs Tibble: Zip, Dir (stage directory under 2_output), Paths (list column: what to include, relative
#'   to 2_output).
#' @param .dir_output_root Character. The 2_output directory.
#' @param .dir_out Character. Destination.
#' @param .dir_stamps Character. Stamp directory.
#' @param .rel_out Character. .dir_out relative to the stage.
#' @param .rerun Logical. TRUE rebuilds regardless of the stamps.
#' @return Tibble: Zip, Files, Bytes, Status.
pbd_replication <- function(.specs, .dir_output_root, .dir_out, .dir_stamps, .rel_out, .rerun = FALSE) {
  if (FALSE) {
    .specs           <- specs_replication
    .dir_output_root <- here::here("2_output")
    .dir_out         <- fs::path(.lP$Output$DirPackage, "replication")
    .dir_stamps      <- .lP$Output$DirStamps
    .rel_out         <- "replication"
    .rerun           <- FALSE
  }
  fs::dir_create(.dir_out)
  purrr::pmap(.specs, \(Zip, Paths, ...) {
    Paths <- unlist(Paths, use.names = FALSE)
    full_ <- fs::path(.dir_output_root, Paths)
    missing_ <- full_[!fs::file_exists(full_)]
    if (length(missing_) > 0L) cli::cli_abort("{Zip}: missing input{?s} {.path {missing_}}")
    files_ <- unlist(purrr::map(full_, \(.p) {
      if (fs::dir_exists(.p)) as.character(fs::dir_ls(path = .p, recurse = TRUE, type = "file")) else as.character(.p)
    }))
    path_zip_ <- fs::path(.dir_out, Zip)
    path_stamp_ <- pbd_stamp_path(.dir_stamps = .dir_stamps, .rel = fs::path(.rel_out, Zip))
    stamp_ <- pbd_stamp(.dirs = full_, .extra = list(Paths = Paths))
    status_ <- "current"
    if (.rerun || !pbd_is_current(.path_out = path_zip_, .path_stamp = path_stamp_, .stamp = stamp_)) {
      tmp_ <- fs::path(.dir_out, paste0(Zip, ".tmp"))
      if (fs::file_exists(tmp_)) fs::file_delete(tmp_)
      zip::zip(zipfile = tmp_, files = Paths, root = .dir_output_root, mode = "mirror", compression_level = 6)
      pbd_promote(.tmp = tmp_, .path = path_zip_)
      pbd_stamp_write(.path_stamp = path_stamp_, .stamp = stamp_)
      status_ <- "built"
    }
    tibble::tibble(Zip = Zip, Files = length(files_), Bytes = as.numeric(fs::file_size(path_zip_)), Status = status_)
  }) |>
    purrr::list_rbind()
}


# 6b. The sample that travels with the site --------------------------------------------------------------------------

#' Which documents the sample is built from
#'
#' THE SAMPLE IS CUT FROM THE LABELLED CONTRACTS, so every example on the site can be scored against a label a human
#' gave. Within each category the documents richest in entities come first -- the ones where an example of an
#' organisation, a place, a date, an amount and a redaction can all be shown -- and ties are broken by DocID, so the
#' pick is the same on every machine and no seed is needed.
#'
#' @param .con A DBI connection.
#' @param .dir_package Character. The published package; the sample is cut from what is published, not from the
#'   pipeline, so it is a subset of exactly the files a reader downloads.
#' @param .n_per_class Integer. Documents per detailed category.
#' @return Tibble: DocID, ClassDetailed, SpanTypes, Chars.
pbd_sample_pick <- function(.con, .dir_package, .n_per_class = 15L) {
  if (FALSE) {
    .con          <- con
    .dir_package  <- .lP$Output$DirPackage
    .n_per_class  <- 15L
  }
  stems_ <- c("org_mentions", "places_geo", "law_clauses", "date_spans", "term_spans", "money_spans", "redact_spans")
  have_ <- purrr::keep(stems_, \(.s) fs::file_exists(fs::path(.dir_package, "spans", paste0(.s, ".parquet"))))
  spans_ <- paste(
    purrr::map_chr(have_, \(.s) sprintf(
      "SELECT DISTINCT DocID, %s AS Kind FROM read_parquet(%s)",
      pbd_lit(.s), pbd_lit(fs::path(.dir_package, "spans", paste0(.s, ".parquet")))
    )),
    collapse = " UNION ALL "
  )
  sql_ <- sprintf(
    paste(
      "WITH lab AS (SELECT DocID, ClassDetailed FROM read_parquet(%s)),",
      "txt AS (SELECT DocID, length(TextRaw) AS Chars FROM read_parquet(%s)),",
      "spn AS (SELECT DocID, count(DISTINCT Kind) AS SpanTypes FROM (%s) GROUP BY DocID)",
      "SELECT lab.DocID, lab.ClassDetailed, coalesce(spn.SpanTypes, 0) AS SpanTypes, txt.Chars",
      "FROM lab JOIN txt ON lab.DocID = txt.DocID LEFT JOIN spn ON lab.DocID = spn.DocID",
      "QUALIFY row_number() OVER (",
      "  PARTITION BY lab.ClassDetailed ORDER BY coalesce(spn.SpanTypes, 0) DESC, txt.Chars, lab.DocID",
      ") <= %d",
      "ORDER BY lab.ClassDetailed, lab.DocID"
    ),
    pbd_lit(fs::path(.dir_package, "core", "ClassificationLabels.parquet")),
    pbd_lit(fs::path(.dir_package, "text", "exhibit10", "*.parquet")),
    spans_,
    as.integer(.n_per_class)
  )
  pbd_query(.con = .con, .sql = sql_)
}

#' Cut every published table down to the sampled documents
#'
#' THE SAMPLE IS A SUBSET, NOT A SUMMARY: same file names, same columns, same types as the package, so a script
#' written against the sample runs against the full data by changing one path. The markup column is the exception --
#' it is left out, because it would make a few megabytes into a few dozen, and the site needs a repository a reader
#' can clone.
#'
#' @param .con A DBI connection.
#' @param .specs Tibble: File (written), Source (package-relative), Key ("DocID", "HashIndex" or "" to copy whole).
#' @param .ids Tibble from pbd_sample_pick().
#' @param .dir_package Character. The published package.
#' @param .dir_out Character. The sample directory.
#' @return Tibble: File, Rows, Bytes.
pbd_sample_write <- function(.con, .specs, .ids, .dir_package, .dir_out) {
  if (FALSE) {
    .con         <- con
    .specs       <- specs_sample
    .ids         <- tab_pick
    .dir_package <- .lP$Output$DirPackage
    .dir_out     <- .lP$Output$DirSample
  }
  fs::dir_create(.dir_out)
  ids_ <- paste(pbd_lit(.ids$DocID), collapse = ", ")
  # The filing key is only looked up when a table is cut on it; Summaries is keyed on the filing, not the document.
  hashes_ <- if (any(.specs$Key == "HashIndex")) {
    pbd_query(
      .con = .con,
      .sql = sprintf(
        "SELECT DISTINCT HashIndex FROM read_parquet(%s) WHERE DocID IN (%s)",
        pbd_lit(fs::path(.dir_package, "core", "Contracts.parquet")), ids_
      )
    )$HashIndex
  } else {
    character(0)
  }
  purrr::pmap(.specs, \(File, Source, Key, ...) {
    src_ <- fs::path(.dir_package, Source)
    out_ <- fs::path(.dir_out, File)
    fs::dir_create(fs::path_dir(out_))
    if (!any(fs::file_exists(fs::path(.dir_package, fs::path_dir(Source))))) {
      cli::cli_abort("Missing source for the sample: {.path {Source}}")
    }
    if (Key == "") {
      pbd_copy(.paths = src_, .dir_out = fs::path_dir(out_))
      return(tibble::tibble(File = File, Rows = NA_integer_, Bytes = as.numeric(fs::file_size(out_))))
    }
    where_ <- if (Key == "HashIndex") {
      sprintf("HashIndex IN (%s)", paste(pbd_lit(hashes_), collapse = ", "))
    } else {
      sprintf("%s IN (%s)", Key, ids_)
    }
    # The markup is dropped here and only here; every other column travels as published.
    cols_ <- pbd_query(.con = .con, .sql = sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", pbd_lit(src_)))$column_name
    select_ <- if ("HTML" %in% cols_) "* EXCLUDE (HTML)" else "*"
    tmp_ <- paste0(out_, ".tmp")
    pbd_exec(
      .con = .con,
      .sql = pbd_sql_copy(
        .query     = sprintf("SELECT %s FROM read_parquet(%s) WHERE %s", select_, pbd_lit(src_), where_),
        .path      = tmp_,
        .row_group = 10000L
      )
    )
    pbd_promote(.tmp = tmp_, .path = out_)
    n_ <- pbd_query(.con = .con, .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s)", pbd_lit(out_)))$n
    tibble::tibble(File = File, Rows = as.integer(n_), Bytes = as.numeric(fs::file_size(out_)))
  }) |>
    purrr::list_rbind()
}

#' The figures the site quotes, as one table it reads instead of repeating them
#'
#' A page that types a size or a row count by hand disagrees with the data the moment either changes. These are
#' written from the manifest and from the published files themselves.
#'
#' @param .con A DBI connection.
#' @param .dir_package Character. The published package.
#' @param .manifest Tibble from pbd_manifest().
#' @param .spans,.text,.coverage,.models Tibbles from the steps above.
#' @param .version Character. The package version.
#' @param .path_out Character. The csv to write.
#' @return Tibble: Key, Value.
pbd_site_numbers <- function(.con, .dir_package, .manifest, .spans, .text, .coverage, .models, .version, .path_out) {
  if (FALSE) {
    .con         <- con
    .dir_package <- .lP$Output$DirPackage
    .manifest    <- tab_manifest
    .spans       <- tab_spans
    .text        <- tab_text
    .coverage    <- tab_coverage
    .models      <- tab_models
    .version     <- .lP$Params$Version
    .path_out    <- fs::path(.lP$Output$DirSample, "package_numbers.csv")
  }
  folders_ <- .manifest |>
    dplyr::mutate(Folder = ifelse(grepl("/", .data$Path), sub("/.*$", "", .data$Path), "root")) |>
    dplyr::summarise(Files = dplyr::n(), Bytes = sum(.data$Bytes), .by = "Folder")
  core_ <- purrr::map(c("Contracts", "Summaries", "Places", "TermDocs", "CtoOrders"), \(.s) {
    n_ <- pbd_query(
      .con = .con,
      .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s)",
                     pbd_lit(fs::path(.dir_package, "core", paste0(.s, ".parquet"))))
    )$n
    tibble::tibble(Key = paste0("rows.", tolower(.s)), Value = as.character(as.integer(n_)))
  }) |>
    purrr::list_rbind()
  dplyr::bind_rows(
    tibble::tibble(Key = "package.version", Value = .version),
    tibble::tibble(Key = "package.files", Value = as.character(nrow(.manifest))),
    tibble::tibble(Key = "package.bytes", Value = as.character(sum(.manifest$Bytes))),
    tibble::tibble(Key = paste0("folder.files.", folders_$Folder), Value = as.character(folders_$Files)),
    tibble::tibble(Key = paste0("folder.bytes.", folders_$Folder), Value = as.character(folders_$Bytes)),
    core_,
    tibble::tibble(Key = paste0("spans.", .spans$Stem), Value = as.character(.spans$RowsOut)),
    tibble::tibble(Key = "spans.total", Value = as.character(sum(.spans$RowsOut))),
    tibble::tibble(
      Key   = paste0("text.documents.", tolower(gsub("[^A-Za-z0-9]", "", unique(.text$Type)))),
      Value = as.character(purrr::map_int(unique(.text$Type), \(.t) sum(.text$RowsOut[.text$Type == .t])))
    ),
    tibble::tibble(Key = "text.years.exhibit10", Value = as.character(sum(.text$Type == "Exhibit10" & .text$RowsOut > 0))),
    tibble::tibble(Key = "models.count", Value = as.character(nrow(.models)))
  ) |>
    (\(.t) {
      pbd_write_csv(.tab = .t, .path = .path_out)
      .t
    })()
}

#' Make one folder of the published repository match a folder here
#'
#' The site repository holds its own pages; this writes only the folder it is given, and inside it adds, replaces and
#' deletes, so a sample that shrinks does not leave stale files behind. Committing stays with the author.
#'
#' @param .dir_from,.dir_to Character. Source and destination folder.
#' @param .apply Logical. FALSE reports the plan only.
#' @return Tibble: Path, Action.
pbd_mirror_repo <- function(.dir_from, .dir_to, .apply = TRUE) {
  if (FALSE) {
    .dir_from <- .lP$Output$DirSample
    .dir_to   <- fs::path(.lP$Output$DirRepo, "sample")
    .apply    <- TRUE
  }
  list_ <- function(.root) {
    if (!fs::dir_exists(.root)) return(character(0))
    as.character(fs::path_rel(fs::dir_ls(path = .root, recurse = TRUE, type = "file", all = TRUE), start = .root))
  }
  src_ <- list_(.dir_from)
  dst_ <- list_(.dir_to)
  both_ <- intersect(src_, dst_)
  same_ <- if (length(both_) == 0L) {
    logical(0)
  } else {
    unname(tools::md5sum(fs::path(.dir_from, both_)) == tools::md5sum(fs::path(.dir_to, both_)))
  }
  plan_ <- dplyr::bind_rows(
    tibble::tibble(Path = setdiff(src_, dst_), Action = "add"),
    tibble::tibble(Path = both_, Action = dplyr::if_else(same_, "same", "update")),
    tibble::tibble(Path = setdiff(dst_, src_), Action = "delete")
  )
  if (.apply && nrow(plan_) > 0L) {
    put_ <- plan_[plan_$Action %in% c("add", "update"), ]
    if (nrow(put_) > 0L) {
      fs::dir_create(unique(fs::path_dir(fs::path(.dir_to, put_$Path))))
      fs::file_copy(path = fs::path(.dir_from, put_$Path), new_path = fs::path(.dir_to, put_$Path), overwrite = TRUE)
    }
    del_ <- plan_[plan_$Action == "delete", ]
    if (nrow(del_) > 0L) fs::file_delete(fs::path(.dir_to, del_$Path))
  }
  dplyr::arrange(plan_, factor(.data$Action, levels = c("add", "update", "delete", "same")), .data$Path)
}


# 7. Manifest, README, deployment ----------------------------------------------------------------------------------------

#' The checksum manifest of the package
#'
#' One SHA-256 per file, in the format `shasum -a 256 -c` reads, so a reader can verify a download with one command.
#' A file whose size and time are unchanged since the last manifest keeps its checksum, so a second run does not
#' re-read tens of gigabytes.
#'
#' @param .dir_package Character. The package directory (the version folder).
#' @param .path_cache Character. Parquet holding the previous run's checksums.
#' @return Tibble: Path, Bytes, Sha256.
pbd_manifest <- function(.dir_package, .path_cache) {
  if (FALSE) {
    .dir_package <- .lP$Output$DirPackage
    .path_cache  <- .lP$Output$FilManifestCache
  }
  info_ <- fs::dir_info(path = .dir_package, recurse = TRUE, type = "file")
  info_ <- info_[!grepl("(\\.tmp$|\\.tmp\\.csv\\.gz$|/MANIFEST\\.sha256$)", info_$path), ]
  now_ <- tibble::tibble(
    Path  = as.character(fs::path_rel(path = info_$path, start = .dir_package)),
    Bytes = as.numeric(info_$size),
    Mtime = round(as.numeric(info_$modification_time))
  )
  cache_ <- if (fs::file_exists(.path_cache)) {
    readr::read_csv(file = .path_cache, show_col_types = FALSE, col_types = "cddc")
  } else {
    tibble::tibble(Path = character(0), Bytes = numeric(0), Mtime = numeric(0), Sha256 = character(0))
  }
  out_ <- now_ |>
    dplyr::left_join(cache_, by = dplyr::join_by("Path", "Bytes", "Mtime"))
  todo_ <- which(is.na(out_$Sha256))
  if (length(todo_) > 0L) {
    cli::cli_alert_info("Computing {length(todo_)} checksum{?s}")
    out_$Sha256[todo_] <- purrr::map_chr(
      fs::path(.dir_package, out_$Path[todo_]),
      \(.f) digest::digest(object = .f, algo = "sha256", file = TRUE)
    )
  }
  out_ <- dplyr::arrange(out_, .data$Path)
  fs::dir_create(fs::path_dir(.path_cache))
  readr::write_csv(x = out_, file = .path_cache)
  pbd_write_lines(.lines = paste0(out_$Sha256, "  ", out_$Path), .path = fs::path(.dir_package, "MANIFEST.sha256"))
  dplyr::select(out_, "Path", "Bytes", "Sha256")
}

#' The README of the package
#'
#' Written before the manifest, so the manifest covers it; the sizes it quotes are read from the folders themselves.
#'
#' @param .dir_package Character. The package directory.
#' @param .version Character. The package version.
#' @param .repo_url Character. Where the code and the documentation are.
#' @return Invisibly, the path written.
pbd_readme <- function(.dir_package, .version, .repo_url) {
  if (FALSE) {
    .dir_package <- .lP$Output$DirPackage
    .version     <- .lP$Params$Version
    .repo_url    <- .lP$Params$RepoUrl
  }
  info_ <- fs::dir_info(path = .dir_package, recurse = TRUE, type = "file")
  rel_ <- as.character(fs::path_rel(path = info_$path, start = .dir_package))
  keep_ <- !grepl("(\\.tmp$|\\.tmp\\.csv\\.gz$)", rel_) & !rel_ %in% c("README.md", "MANIFEST.sha256")
  sizes_ <- tibble::tibble(Path = rel_[keep_], Bytes = as.numeric(info_$size[keep_])) |>
    dplyr::mutate(Folder = ifelse(grepl("/", .data$Path), sub("/.*$", "", .data$Path), ".")) |>
    dplyr::summarise(Files = dplyr::n(), Bytes = sum(.data$Bytes), .by = "Folder") |>
    dplyr::arrange(.data$Folder)
  about_ <- c(
    core        = "Release files with codebooks, contract index, labelled sample, keyword tables",
    spans       = "Every entity span the extractors found, one file per span type",
    text        = "Retrieved documents as filed (HTML) and as text (TextRaw), one file per type and year",
    models      = "Fine-tuned classifiers, one zip per task and context length, with an index",
    lexnlp      = "The Docker image that extracted the entities, its lockfile, and a licence notice",
    replication = "What the paper's exhibits, numbers and appendix read beyond core/, one zip per stage",
    `.`         = "Files at the top level"
  )
  rows_ <- sprintf(
    "| `%s` | %s | %d | %s |",
    sizes_$Folder,
    ifelse(sizes_$Folder %in% names(about_), about_[sizes_$Folder], ""),
    sizes_$Files,
    trimws(format(fs::as_fs_bytes(sizes_$Bytes)))
  )
  lines_ <- c(
    paste("# matcon data --", .version),
    "",
    "Data of \"The analysis of material contracts: Use of SEC contractual data in accounting research\"",
    paste0("(Grosskopf, Sehn and Uckert). Code and documentation: <", .repo_url, ">."),
    "",
    "| Folder | Content | Files | Size |",
    "|:--|:--|--:|--:|",
    rows_,
    "",
    "## Verify a download",
    "",
    "Every file is listed in `MANIFEST.sha256`. From this folder: `shasum -a 256 -c MANIFEST.sha256` (macOS, Linux).",
    "",
    "## Read the files",
    "",
    "- R: `arrow::read_parquet(\"core/Contracts.parquet\")`",
    "- Python: `pandas.read_parquet(\"core/Contracts.parquet\")`",
    "- SQL: `duckdb -c \"SELECT count(*) FROM 'core/Contracts.parquet'\"`",
    "- Stata 19.5 and later: `import parquet using core/Contracts.parquet, clear`; earlier versions: convert with R",
    "  or Python, using the `StataName` column of the codebook.",
    "",
    "## Note on the links",
    "",
    "`Contracts.parquet` and `ContractIndex.csv.gz` carry `UrlDocument` and `UrlIndexPage` with the scheme repaired:",
    "the pipeline writes the index-page link with a single slash after `https:`, which no client resolves. Everything",
    "else in the release files is published as the pipeline wrote it.",
    "",
    "## Licences",
    "",
    "Data: CC BY 4.0. Models: CC BY-SA 4.0, the licence of their base model. The LexNLP image contains software under",
    "the AGPL-3.0 (see `lexnlp/NOTICE.md`). The documents in `text/` are filings made public by the SEC on EDGAR.",
    ""
  )
  path_ <- fs::path(.dir_package, "README.md")
  pbd_write_lines(.lines = lines_, .path = path_)
  invisible(path_)
}

#' Copy the package to the Drive folder, file by file, only where the copy differs
#'
#' NOTHING ON DRIVE IS DELETED. A file there that the package no longer holds is listed, not removed; the Drive
#' folder is shared, and a deletion there is a decision for a person, not for a render.
#'
#' @param .dir_package Character. The package directory.
#' @param .dir_target Character. The destination folder on Drive (the version folder).
#' @param .manifest Tibble from pbd_manifest().
#' @param .apply Logical. FALSE only reports.
#' @param .skip Character. Top-level folders not to copy this time.
#' @return Tibble: Path, Bytes, Action ("copy", "current", "skipped", "only on Drive").
pbd_deploy <- function(.dir_package, .dir_target, .manifest, .apply = TRUE, .skip = character(0)) {
  if (FALSE) {
    .dir_package <- .lP$Output$DirPackage
    .dir_target  <- .lP$Output$DirDrive
    .manifest    <- tab_manifest
    .apply       <- FALSE
    .skip        <- character(0)
  }
  files_ <- c(.manifest$Path, "MANIFEST.sha256")
  src_ <- fs::path(.dir_package, files_)
  dst_ <- fs::path(.dir_target, files_)
  src_info_ <- fs::file_info(src_)
  dst_info_ <- fs::file_info(dst_)
  top_ <- ifelse(grepl("/", files_), sub("/.*$", "", files_), ".")
  current_ <- !is.na(dst_info_$size) & dst_info_$size == src_info_$size &
    dst_info_$modification_time >= src_info_$modification_time
  plan_ <- tibble::tibble(
    Path   = files_,
    Bytes  = as.numeric(src_info_$size),
    Action = dplyr::case_when(
      top_ %in% .skip ~ "skipped",
      current_        ~ "current",
      TRUE            ~ "copy"
    )
  )
  if (fs::dir_exists(.dir_target)) {
    there_ <- as.character(fs::path_rel(fs::dir_ls(path = .dir_target, recurse = TRUE, type = "file"), .dir_target))
    extra_ <- setdiff(there_, files_)
    if (length(extra_) > 0L) {
      plan_ <- dplyr::bind_rows(plan_, tibble::tibble(Path = extra_, Bytes = NA_real_, Action = "only on Drive"))
    }
  }
  if (.apply) {
    todo_ <- which(plan_$Action == "copy")
    if (length(todo_) > 0L) {
      fs::dir_create(unique(fs::path_dir(dst_[todo_])))
      for (i_ in seq_along(todo_)) {
        j_ <- todo_[i_]
        cli::cli_alert_info("Copying {i_}/{length(todo_)}: {plan_$Path[j_]} ({format(fs::as_fs_bytes(plan_$Bytes[j_]))})")
        fs::file_copy(path = src_[j_], new_path = dst_[j_], overwrite = TRUE)
      }
    }
  }
  plan_
}


# 8. Validation and reports ----------------------------------------------------------------------------------------------

#' Compare every output with its source
#'
#' @param .core,.contracts,.index,.labels,.spans,.text,.coverage,.models,.replication Tibbles from the steps above.
#' @param .n_prepared Numeric. Rows of 03A's prepared file.
#' @param .n_contracts Numeric. Rows of the release.
#' @param .n_models Integer. Model directories expected.
#' @return Tibble: Check, Expected, Found, Ok.
pbd_validate <- function(.core, .contracts, .index, .labels, .spans, .text, .coverage, .models, .replication,
                         .n_prepared, .n_contracts, .n_models = 6L) {
  if (FALSE) {
    .core        <- tab_core
    .contracts   <- tab_contracts
    .index       <- tab_index
    .labels      <- tab_labels
    .spans       <- tab_spans
    .text        <- tab_text
    .coverage    <- tab_coverage
    .models      <- tab_models
    .replication <- tab_replication
    .n_prepared  <- n_prepared
    .n_contracts <- n_contracts
    .n_models    <- 6L
  }
  chk_ <- function(.check, .expected, .found) {
    tibble::tibble(Check = .check, Expected = as.character(.expected), Found = as.character(.found),
                   Ok = isTRUE(all.equal(as.character(.expected), as.character(.found))))
  }
  dplyr::bind_rows(
    chk_("core: release files copied", nrow(.core), sum(.core$Status %in% c("copied", "current"))),
    chk_("contracts: one row per release row", .n_contracts, .contracts$Rows),
    chk_("contracts: links left broken", 0, .contracts$LeftBroken),
    chk_("index: one row per release row", .n_contracts, .index$Rows),
    chk_("index: rows without an accession number", 0, .index$NoAccession),
    chk_("labels: one row per labelled document", .n_prepared, .labels$Rows),
    chk_("labels: no text column", "no Text", if (grepl("\\bText\\b", .labels$Columns)) "Text present" else "no Text"),
    chk_("spans: rows out equal rows in", format(sum(.spans$RowsIn), big.mark = ","),
         format(sum(.spans$RowsOut), big.mark = ",")),
    chk_("text: every year built or current", 0, sum(!.text$Status %in% c("built", "current", "no release documents"))),
    chk_("text: release documents without text", 0, sum(.coverage$WithoutText)),
    chk_("text: every document published once", 0, sum(.coverage$Duplicates)),
    chk_("models: one zip per model", .n_models, nrow(.models)),
    chk_("replication: every zip built or current", nrow(.replication),
         sum(.replication$Status %in% c("built", "current")))
  )
}

#' Print a step's table with a title and a one-line reading guide
#'
#' @param .tab Tibble.
#' @param .title Character.
#' @param .note Character or NULL.
#' @param .n Integer or NULL. Rows to show.
#' @return Invisibly, .tab.
pbd_report <- function(.tab, .title, .note = NULL, .n = NULL) {
  if (FALSE) {
    .tab   <- tab_spans
    .title <- "Span files"
    .note  <- NULL
    .n     <- NULL
  }
  show_ <- .tab |>
    dplyr::mutate(dplyr::across(dplyr::any_of("Bytes"), \(.b) format(fs::as_fs_bytes(.b))))
  tbl_say(.tab = show_, .title = .title, .n = .n)
  if (!is.null(.note)) cli::cli_alert_info(.note)
  invisible(.tab)
}

#' The text step, summarised per document type
#'
#' @param .text Tibble from pbd_text().
#' @param .coverage Tibble from pbd_text_coverage().
#' @return Invisibly, the summary.
pbd_report_text <- function(.text, .coverage) {
  if (FALSE) {
    .text     <- tab_text
    .coverage <- tab_coverage
  }
  out_ <- .text |>
    dplyr::summarise(
      Years      = sum(.data$RowsOut > 0),
      Retrieved  = sum(.data$RowsIn),
      Published  = sum(.data$RowsOut),
      NotInRelease = sum(.data$RowsIn) - sum(.data$RowsOut),
      Bytes      = sum(.data$Bytes),
      Built      = sum(.data$Status == "built"),
      .by        = "Type"
    )
  pbd_report(
    .tab   = out_,
    .title = "Text per document type",
    .note  = "NotInRelease counts documents retrieved but not used by the release.",
    .n     = NULL
  )
  pbd_report(
    .tab   = .coverage,
    .title = "Release documents and their text",
    .note  = "WithoutText counts release documents with no published text, Duplicates documents published twice; both
              should be zero.",
    .n     = NULL
  )
  invisible(out_)
}
