# 40B-PublishData: the data package ------------------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# It builds the published data package from what the pipeline stages wrote, checks it against its sources and against
# the public schema, writes a checksum manifest and a README, and copies the package to the shared Drive folder. It
# also cuts the sample the site's examples run on and writes it into the site repository. Nothing upstream is changed.
#
# THE PACKAGE
#   core/     the release tables, the labelled sample, the keyword terms in use, the contract index, their codebooks
#   spans/    the entity spans the rules kept, one file per kind, without MONEY
#   text/     the retrieved documents, as filed (HTML) and as read (TextRaw), one file per type and year
#   models/   the six fine-tuned classifiers, their index, the selection record, a usage guide
#   lexnlp/   the image that extracted the organisation candidates, its lockfile, a notice, a usage guide
#
# ONE PUBLIC SCHEMA DECIDES WHAT IS PUBLISHED. 1_code/_Publish/PublicSchema.csv lists every column of every published
# table: whether it is kept, dropped or added, its type, its meaning and its Stata name. A source column the schema
# does not list stops the build, so nothing reaches the package without a decision; the codebooks are written from
# the schema, so they cannot disagree with the files.
#
# WHAT IS PUBLISHED IS WHAT THE PAPER USED. Two kinds of content come out -- the Compustat fields (licensed) and
# everything about amounts (MONEY, not used) -- the EDGAR links are repaired, and one column is added, the paper's
# broad label ClassRollup. Everything else is published as built, and the text exactly as the models and extractors
# read it.
#
# DUCKDB DOES THE HEAVY WORK, IN SQL: one statement per output file, run through pbd_exec() and pbd_query(), the only
# two places that touch the connection.
#
# EVERY EXPENSIVE OUTPUT IS STAMPED: a hash of its input files and of the statement that builds it. An output whose
# stamp matches is skipped. The text functions are unchanged from the first build, so their stamps still hold and the
# 45 GB of text is neither rebuilt nor copied again.
#
# LISTINGS OF LARGE TREES USE BASE R. fs errs and crashes on directories of this size on this machine (r-lib/fs#281);
# pbd_stamp() lists with list.files() and file.info().
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed locals; .data$ for
# existing columns, bare CamelCase for new columns; if (FALSE) dev blocks; cli/fs/here; pure ASCII; stringi never
# base substr; {(.arg)} parens in cli interpolation.


# 1. Connection and SQL -----------------------------------------------------------------------------------------------

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


# 2. Stamps and small helpers -----------------------------------------------------------------------------------------

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


# 3. The public schema ---------------------------------------------------------------------------------------------------

#' Read the public schema and check it is complete
#'
#' Every published column must carry a type, a meaning, a missing rule and a Stata name of at most 32 characters,
#' unique within its table. A schema that fails any of these stops the build here, before a file is written.
#'
#' @param .path Character. PublicSchema.csv.
#' @return Tibble: Table, File, Order, Column, Action, Type, Block, Meaning, Missing, StataName.
pbd_schema_read <- function(.path) {
  if (FALSE) {
    .path <- .lP$Input$FilSchema
  }
  sch_ <- readr::read_csv(
    file           = .path,
    col_types      = readr::cols(.default = readr::col_character(), Order = readr::col_integer()),
    show_col_types = FALSE
  )
  need_ <- c("Table", "File", "Order", "Column", "Action", "Type", "Block", "Meaning", "Missing", "StataName")
  if (length(setdiff(need_, names(sch_))) > 0L) {
    cli::cli_abort("The schema lacks column{?s} {.val {setdiff(need_, names(sch_))}}.")
  }
  bad_ <- setdiff(unique(sch_$Action), c("keep", "drop", "add"))
  if (length(bad_) > 0L) cli::cli_abort("Unknown action{?s} in the schema: {.val {bad_}}.")
  dup_ <- sch_[duplicated(sch_[, c("Table", "Column")]), ]
  if (nrow(dup_) > 0L) cli::cli_abort("Listed twice: {.val {paste(dup_$Table, dup_$Column, sep = '.')}}.")
  pub_ <- dplyr::filter(sch_, .data$Action != "drop")
  empty_ <- pub_[is.na(pub_$Type) | is.na(pub_$Meaning) | is.na(pub_$Missing) | is.na(pub_$StataName), ]
  if (nrow(empty_) > 0L) {
    cli::cli_abort("Published column{?s} without type, meaning, missing rule or Stata name: \\
                    {.val {paste(empty_$Table, empty_$Column, sep = '.')}}.")
  }
  long_ <- pub_[nchar(pub_$StataName) > 32L, ]
  if (nrow(long_) > 0L) cli::cli_abort("Stata name{?s} over 32 characters: {.val {long_$StataName}}.")
  clash_ <- pub_ |>
    dplyr::count(.data$Table, Lower = tolower(.data$StataName)) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(clash_) > 0L) cli::cli_abort("Stata names that collide: {.val {paste(clash_$Table, clash_$Lower)}}.")
  dplyr::arrange(sch_, .data$Table, .data$Order)
}

#' Hold every source table against the schema
#'
#' A column in a source that the schema does not list, or a kept column the source no longer has, stops the build:
#' nothing reaches the package without a decision, and a column renamed upstream cannot vanish silently.
#'
#' @param .con A DBI connection.
#' @param .schema Tibble from pbd_schema_read().
#' @param .sources Tibble: Table, From (an SQL FROM expression over the source).
#' @return Tibble: Table, Source, Kept, Dropped, Added, Unlisted, Missing.
pbd_schema_check <- function(.con, .schema, .sources) {
  if (FALSE) {
    .con     <- con
    .schema  <- schema
    .sources <- sources_schema
  }
  out_ <- purrr::pmap(.sources, \(Table, From) {
    src_ <- pbd_query(.con = .con, .sql = sprintf("DESCRIBE SELECT * FROM %s", From))$column_name
    sch_ <- .schema[.schema$Table == Table, ]
    listed_ <- sch_$Column[sch_$Action %in% c("keep", "drop")]
    tibble::tibble(
      Table    = Table,
      Source   = length(src_),
      Kept     = sum(sch_$Action == "keep"),
      Dropped  = sum(sch_$Action == "drop"),
      Added    = sum(sch_$Action == "add"),
      Unlisted = paste(setdiff(src_, listed_), collapse = ", "),
      Missing  = paste(setdiff(listed_, src_), collapse = ", ")
    )
  }) |>
    purrr::list_rbind()
  bad_ <- out_[out_$Unlisted != "" | out_$Missing != "", ]
  if (nrow(bad_) > 0L) {
    cli::cli_abort(c(
      "The sources and the schema disagree:",
      purrr::set_names(sprintf("%s: not in the schema [%s]; not in the source [%s]", bad_$Table, bad_$Unlisted,
                               bad_$Missing), rep("x", nrow(bad_))),
      "i" = "Add a row to PublicSchema.csv for every new column, with a decision: keep, drop or add."
    ))
  }
  out_
}

#' The select list of a published table, in the schema's order
#'
#' @param .schema Tibble from pbd_schema_read().
#' @param .table Character. The table.
#' @param .exprs Named character. SQL expressions for columns that are computed rather than copied.
#' @return Character, the comma-separated select list.
pbd_select_list <- function(.schema, .table, .exprs = character(0)) {
  if (FALSE) {
    .schema <- schema
    .table  <- "Contracts"
    .exprs  <- c(ClassRollup = "NULL")
  }
  cols_ <- .schema$Column[.schema$Table == .table & .schema$Action != "drop"]
  if (length(cols_) == 0L) cli::cli_abort("The schema publishes no column of {.val {(.table)}}.")
  added_ <- .schema$Column[.schema$Table == .table & .schema$Action == "add"]
  missing_ <- setdiff(added_, names(.exprs))
  if (length(missing_) > 0L && .table != "KeywordTerms") {
    cli::cli_abort("Added column{?s} of {.val {(.table)}} without an expression: {.val {missing_}}.")
  }
  paste(
    purrr::map_chr(cols_, \(.c) {
      if (.c %in% names(.exprs)) sprintf("%s AS \"%s\"", .exprs[[.c]], .c) else sprintf("\"%s\"", .c)
    }),
    collapse = ", "
  )
}

#' The codebooks, written from the schema
#'
#' One codebook per core table, one for all span files, one for the text files. Written only when their content
#' changes, so an unchanged schema leaves them untouched.
#'
#' @param .schema Tibble from pbd_schema_read().
#' @param .dir_package Character. The package.
#' @param .core Character. The core tables that get a codebook.
#' @param .spans Character. The span tables.
#' @return Tibble: File, Rows, Status.
pbd_codebooks <- function(.schema, .dir_package, .core, .spans) {
  if (FALSE) {
    .schema      <- schema
    .dir_package <- .lP$Output$DirPackage
    .core        <- c("Contracts", "Summaries")
    .spans       <- specs_spans$Stem
  }
  pub_ <- .schema |>
    dplyr::filter(.data$Action != "drop") |>
    dplyr::select("Table", "Order", "Column", "Type", "Block", "Meaning", "Missing", "StataName")
  one_ <- function(.tab, .rel) {
    status_ <- pbd_write_csv(.tab = .tab, .path = fs::path(.dir_package, .rel))
    tibble::tibble(File = .rel, Rows = nrow(.tab), Status = status_)
  }
  dplyr::bind_rows(
    purrr::map(.core, \(.t) one_(
      .tab = pub_ |> dplyr::filter(.data$Table == .t) |> dplyr::arrange(.data$Order) |> dplyr::select(-"Table", -"Order"),
      .rel = fs::path("core", paste0(.t, "_Codebook.csv"))
    )),
    one_(
      .tab = pub_ |>
        dplyr::filter(.data$Table %in% .spans) |>
        dplyr::arrange(match(.data$Table, .spans), .data$Order) |>
        dplyr::transmute(File = paste0(.data$Table, ".parquet"), .data$Column, .data$Type, .data$Block,
                         .data$Meaning, .data$Missing, .data$StataName),
      .rel = fs::path("spans", "Spans_Codebook.csv")
    ),
    one_(
      .tab = pub_ |>
        dplyr::filter(.data$Table == "Text") |>
        dplyr::arrange(.data$Order) |>
        dplyr::select(-"Table", -"Order"),
      .rel = fs::path("text", "Text_Codebook.csv")
    )
  )
}


# 4. core/ ---------------------------------------------------------------------------------------------------------------

#' The broad category of every detailed category, read from the labelled sample
#'
#' The annotators gave every labelled contract a detailed and a broad category, so the labelled sample holds the
#' roll-up the paper uses. It must be a function -- one broad category per detailed one -- or the build stops.
#'
#' @param .con A DBI connection.
#' @param .path_labels Character. 03A's prepared labelled sample.
#' @return Tibble: Detailed, Broad.
pbd_rollup_map <- function(.con, .path_labels) {
  if (FALSE) {
    .con         <- con
    .path_labels <- .lP$Input$FilPrepared
  }
  map_ <- pbd_query(
    .con = .con,
    .sql = sprintf(
      paste("SELECT ClassDetailed AS Detailed, min(ClassBroad) AS Broad, count(DISTINCT ClassBroad) AS n",
            "FROM read_parquet(%s) GROUP BY 1 ORDER BY 1"),
      pbd_lit(.path_labels)
    )
  )
  if (any(map_$n > 1L)) {
    cli::cli_abort("A detailed category rolls up to more than one broad one: {.val {map_$Detailed[map_$n > 1L]}}.")
  }
  dplyr::select(map_, "Detailed", "Broad")
}

#' The SQL CASE that rolls a detailed category up
#'
#' @param .map Tibble from pbd_rollup_map().
#' @param .col Character. The column holding the detailed category.
#' @return Character, an SQL expression.
pbd_rollup_sql <- function(.map, .col = "Class") {
  if (FALSE) {
    .map <- rollup_map
    .col <- "Class"
  }
  paste0(
    "CASE \"", .col, "\" ",
    paste(sprintf("WHEN %s THEN %s", pbd_lit(.map$Detailed), pbd_lit(.map$Broad)), collapse = " "),
    " END"
  )
}

#' Write one published table through the schema
#'
#' The select list comes from the schema, so the file holds exactly the published columns in the published order,
#' and computed columns enter through .exprs. The output is stamped on its sources and its statement.
#'
#' @param .con A DBI connection.
#' @param .schema Tibble from pbd_schema_read().
#' @param .table Character. The table in the schema.
#' @param .from Character. SQL FROM expression over the source.
#' @param .sources Character. Source files the stamp covers.
#' @param .path_out Character. Destination.
#' @param .path_stamp Character. Its stamp.
#' @param .exprs Named character. Computed columns.
#' @param .format Character. "parquet" or "csv" (gzipped).
#' @param .rerun Logical. TRUE rebuilds regardless of the stamp.
#' @return Tibble: Table, RowsIn, RowsOut, Columns, Bytes, Status.
pbd_table <- function(.con, .schema, .table, .from, .sources, .path_out, .path_stamp, .exprs = character(0),
                      .format = "parquet", .rerun = FALSE) {
  if (FALSE) {
    .con        <- con
    .schema     <- schema
    .table      <- "Summaries"
    .from       <- sprintf("read_parquet(%s)", pbd_lit(fs::path(.lP$Input$DirRelease, "Summaries.parquet")))
    .sources    <- fs::path(.lP$Input$DirRelease, "Summaries.parquet")
    .path_out   <- fs::path(.lP$Output$DirPackage, "core", "Summaries.parquet")
    .path_stamp <- pbd_stamp_path(.lP$Output$DirStamps, "core/Summaries.parquet")
    .exprs      <- character(0)
    .format     <- "parquet"
    .rerun      <- FALSE
  }
  query_ <- sprintf("SELECT %s FROM %s", pbd_select_list(.schema = .schema, .table = .table, .exprs = .exprs), .from)
  stamp_ <- pbd_stamp(.dirs = .sources, .extra = list(Query = query_, Format = .format))
  status_ <- "current"
  if (.rerun || !pbd_is_current(.path_out = .path_out, .path_stamp = .path_stamp, .stamp = stamp_)) {
    fs::dir_create(fs::path_dir(.path_out))
    if (.format == "csv") {
      tmp_ <- paste0(.path_out, ".tmp.csv.gz")
      pbd_exec(.con = .con, .sql = sprintf("COPY (%s) TO %s (FORMAT CSV, HEADER, COMPRESSION GZIP)", query_,
                                           pbd_lit(tmp_)))
    } else {
      tmp_ <- paste0(.path_out, ".tmp")
      pbd_exec(.con = .con, .sql = pbd_sql_copy(.query = query_, .path = tmp_, .row_group = 100000L))
    }
    pbd_promote(.tmp = tmp_, .path = .path_out)
    pbd_stamp_write(.path_stamp = .path_stamp, .stamp = stamp_)
    status_ <- "built"
  }
  reader_ <- if (.format == "csv") "read_csv_auto(%s)" else "read_parquet(%s)"
  rows_out_ <- pbd_query(.con = .con, .sql = sprintf(paste("SELECT count(*) AS n FROM", reader_), pbd_lit(.path_out)))$n
  rows_in_ <- pbd_query(.con = .con, .sql = sprintf("SELECT count(*) AS n FROM %s", .from))$n
  tibble::tibble(
    Table   = .table,
    RowsIn  = as.integer(rows_in_),
    RowsOut = as.integer(rows_out_),
    Columns = sum(.schema$Table == .table & .schema$Action != "drop"),
    Bytes   = as.numeric(fs::file_size(.path_out)),
    Status  = status_
  )
}

#' The FROM expression over the keyword terms in use
#'
#' 03C's catalogue marks, per task, the window whose terms the classifier applies. Those three tables are stacked with
#' a Task column; the other windows are not published.
#'
#' @param .con A DBI connection.
#' @param .dir_tables Character. 03C's table folder.
#' @return List: From (SQL), Files (the three tables), Windows (tibble Task, Window, File).
pbd_keywords_from <- function(.con, .dir_tables) {
  if (FALSE) {
    .con        <- con
    .dir_tables <- .lP$Input$DirKeywordTables
  }
  slug_ <- c(ClassDetailed = "detailed", ClassBroad = "broad", AmendType = "amendment")
  cat_ <- pbd_query(
    .con = .con,
    .sql = sprintf("SELECT Task, NWords FROM read_parquet(%s) WHERE \"Default\" ORDER BY Task",
                   pbd_lit(fs::path(.dir_tables, "catalogue.parquet")))
  )
  if (!setequal(cat_$Task, names(slug_)) || anyDuplicated(cat_$Task) > 0L) {
    cli::cli_abort("The keyword catalogue must mark exactly one window per task; it marks {.val {cat_$Task}}.")
  }
  win_ <- cat_ |>
    dplyr::mutate(
      Window = ifelse(.data$NWords == 0, "Wfull", paste0("W", .data$NWords)),
      File   = as.character(fs::path(.dir_tables, sprintf("keyword_table_%s_%s.parquet", slug_[.data$Task], .data$Window)))
    )
  missing_ <- win_$File[!fs::file_exists(win_$File)]
  if (length(missing_) > 0L) cli::cli_abort("Keyword table{?s} missing: {.file {missing_}}")
  from_ <- paste0(
    "(",
    paste(sprintf("SELECT %s AS Task, * FROM read_parquet(%s)", pbd_lit(win_$Task), pbd_lit(win_$File)),
          collapse = " UNION ALL BY NAME "),
    ") AS k"
  )
  list(From = from_, Files = win_$File, Windows = dplyr::select(win_, "Task", "Window", "File"))
}


# 5. spans/ --------------------------------------------------------------------------------------------------------------

#' One file per span type, from 04D's chunks, with the schema's columns
#'
#' The chunks are read by name, so 04D's bookkeeping files never enter a span file, and columns are unified by name
#' across chunks. The select list comes from the schema, which is how the columns that point into money spans leave
#' redact_spans. Rows are ordered by document, so a reader filtering on DocID skips most of a file.
#'
#' @param .con A DBI connection.
#' @param .schema Tibble from pbd_schema_read().
#' @param .specs Tibble: Stem, Pass, DirHash.
#' @param .dir_out Character. Destination directory.
#' @param .dir_stamps Character. Stamp directory.
#' @param .rel_out Character. .dir_out relative to the package.
#' @param .rerun Logical. TRUE rebuilds regardless of the stamps.
#' @return Tibble: Stem, Pass, Chunks, RowsIn, RowsOut, Bytes, Status.
pbd_spans <- function(.con, .schema, .specs, .dir_out, .dir_stamps, .rel_out, .rerun = FALSE) {
  if (FALSE) {
    .con        <- con
    .schema     <- schema
    .specs      <- specs_spans
    .dir_out    <- fs::path(.lP$Output$DirPackage, "spans")
    .dir_stamps <- .lP$Output$DirStamps
    .rel_out    <- "spans"
    .rerun      <- FALSE
  }
  fs::dir_create(.dir_out)
  purrr::pmap(.specs, \(Stem, Pass, DirHash) {
    chunks_ <- list.files(path = DirHash, pattern = "^chunk-", full.names = TRUE)
    files_ <- fs::path(chunks_, paste0(Stem, ".parquet"))
    files_ <- files_[file.exists(files_)]
    if (length(files_) == 0L) cli::cli_abort("No {.file {Stem}.parquet} under {.path {DirHash}}")
    glob_ <- fs::path(DirHash, "chunk-*", paste0(Stem, ".parquet"))
    from_ <- sprintf("read_parquet(%s, union_by_name = true)", pbd_lit(glob_))
    query_ <- sprintf("SELECT %s FROM %s ORDER BY DocID", pbd_select_list(.schema = .schema, .table = Stem), from_)
    path_out_ <- fs::path(.dir_out, paste0(Stem, ".parquet"))
    path_stamp_ <- pbd_stamp_path(.dir_stamps = .dir_stamps, .rel = fs::path(.rel_out, fs::path_file(path_out_)))
    stamp_ <- pbd_stamp(.dirs = files_, .extra = list(Query = query_))
    rows_in_ <- pbd_query(.con = .con, .sql = sprintf("SELECT count(*) AS n FROM %s", from_))$n
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


# 6. text/ -- unchanged since the first build, so its stamps still hold -----------------------------------------------

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


# 7. models/ and lexnlp/ -------------------------------------------------------------------------------------------------

#' Replace the machine's paths in a text file of a model folder
#'
#' Paths into the repository become relative to it, and any other home directory becomes "~/", so a published file
#' names neither a person nor a machine.
#'
#' @param .lines Character. The file's lines.
#' @param .root Character. The repository root.
#' @return Character, the lines rewritten.
pbd_unpath <- function(.lines, .root) {
  if (FALSE) {
    .lines <- c("\"data_path\": \"/Users/x/RProjects/Projects/material-contracts/2_output/a.parquet\"")
    .root  <- "/Users/x/RProjects/Projects/material-contracts"
  }
  out_ <- gsub(paste0(.root, "/"), "", .lines, fixed = TRUE)
  gsub("(/Users/|/home/)[^/\"' ]+/", "~/", out_, perl = TRUE)
}

#' One archive per fine-tuned model, cleaned of machine paths, with an index and the selection record
#'
#' Each model folder is copied to a temporary place, its text files (the run configuration, the log) have their
#' paths rewritten, the empty folders go, and the copy is zipped. The selection record, deployed.parquet, gets the
#' same rewrite in its text columns. The index says which archive labelled the published data and which the
#' cross-validation selected.
#'
#' @param .con A DBI connection.
#' @param .dir_models Character. 03B's model_final directory.
#' @param .dir_out Character. Destination.
#' @param .dir_stamps Character. Stamp directory.
#' @param .rel_out Character. .dir_out relative to the package.
#' @param .root Character. The repository root, for the path rewrite.
#' @param .labelled_ctx Integer. The context length of the models that labelled the published data.
#' @param .path_readme Character. The usage guide to publish beside the archives.
#' @param .rerun Logical. TRUE rebuilds regardless of the stamps.
#' @return Tibble: Zip, Directory, Task, Context, LabelledPublished, Selected, MacroF1, LocalPaths, Bytes, Status.
pbd_models <- function(.con, .dir_models, .dir_out, .dir_stamps, .rel_out, .root, .labelled_ctx, .path_readme,
                       .rerun = FALSE) {
  if (FALSE) {
    .con          <- con
    .dir_models   <- .lP$Input$DirModels
    .dir_out      <- fs::path(.lP$Output$DirPackage, "models")
    .dir_stamps   <- .lP$Output$DirStamps
    .rel_out      <- "models"
    .root         <- here::here()
    .labelled_ctx <- 256L
    .path_readme  <- .lP$Input$FilModelsReadme
    .rerun        <- FALSE
  }
  fs::dir_create(.dir_out)
  dirs_ <- list.files(path = .dir_models, pattern = "__FINAL$", full.names = TRUE)
  dirs_ <- dirs_[dir.exists(dirs_)]
  if (length(dirs_) == 0L) cli::cli_abort("No model directory ending in __FINAL under {.path {(.dir_models)}}")
  src_dep_ <- fs::path(.dir_models, "deployed.parquet")
  dep_ <- pbd_query(.con = .con, .sql = sprintf("SELECT ConfigName, MacroF1 FROM read_parquet(%s)", pbd_lit(src_dep_)))
  root_tmp_ <- fs::path(tempdir(), "pbd-models")

  out_ <- purrr::map(dirs_, \(.d) {
    name_ <- basename(.d)
    task_ <- sub("__.*$", "", name_)
    ctx_ <- as.integer(stringi::stri_extract_first_regex(name_, "(?<=_L)[0-9]+(?=_)"))
    zip_ <- paste0(task_, "_L", ctx_, ".zip")
    path_zip_ <- fs::path(.dir_out, zip_)
    path_stamp_ <- pbd_stamp_path(.dir_stamps = .dir_stamps, .rel = fs::path(.rel_out, zip_))
    stamp_ <- pbd_stamp(.dirs = .d, .extra = list(Zip = zip_, Clean = "paths rewritten, empty folders dropped"))
    status_ <- "current"
    if (.rerun || !pbd_is_current(.path_out = path_zip_, .path_stamp = path_stamp_, .stamp = stamp_)) {
      copy_ <- fs::path(root_tmp_, name_)
      if (dir.exists(copy_)) unlink(copy_, recursive = TRUE)
      fs::dir_create(root_tmp_)
      fs::dir_copy(path = .d, new_path = copy_, overwrite = TRUE)
      texts_ <- list.files(copy_, pattern = "\\.(json|log|txt|md|csv)$", recursive = TRUE, full.names = TRUE)
      for (f_ in texts_) writeLines(pbd_unpath(.lines = readLines(f_, warn = FALSE), .root = .root), f_)
      subdirs_ <- list.dirs(copy_, recursive = TRUE, full.names = TRUE)
      empty_ <- subdirs_[vapply(subdirs_, \(.s) length(list.files(.s, all.files = TRUE, no.. = TRUE)) == 0L, logical(1))]
      if (length(empty_) > 0L) unlink(empty_, recursive = TRUE)
      tmp_ <- fs::path(.dir_out, paste0(zip_, ".tmp"))
      if (file.exists(tmp_)) file.remove(tmp_)
      # Weights barely compress; a low level keeps the archive fast to write and to open.
      zip::zip(zipfile = tmp_, files = name_, root = root_tmp_, mode = "mirror", compression_level = 1)
      unlink(copy_, recursive = TRUE)
      pbd_promote(.tmp = tmp_, .path = path_zip_)
      pbd_stamp_write(.path_stamp = path_stamp_, .stamp = stamp_)
      status_ <- "built"
    }
    entries_ <- zip::zip_list(path_zip_)$filename
    texts_in_ <- entries_[grepl("\\.(json|log|txt|md|csv)$", entries_)]
    hits_ <- sum(purrr::map_int(texts_in_, \(.e) {
      con_ <- unz(path_zip_, .e)
      on.exit(close(con_))
      sum(grepl("/Users/|/home/|Dropbox", readLines(con_, warn = FALSE)))
    }))
    base_ <- sub("__FINAL$", "", name_)
    tibble::tibble(
      Zip               = zip_,
      Directory         = name_,
      Task              = task_,
      Context           = ctx_,
      LabelledPublished = ctx_ == .labelled_ctx,
      Selected          = base_ %in% dep_$ConfigName,
      MacroF1           = if (base_ %in% dep_$ConfigName) dep_$MacroF1[match(base_, dep_$ConfigName)] else NA_real_,
      LocalPaths        = as.integer(hits_),
      Bytes             = as.numeric(fs::file_size(path_zip_)),
      Status            = status_
    )
  }) |>
    purrr::list_rbind()
  if (anyDuplicated(out_$Zip) > 0L) {
    cli::cli_abort("Two model folders map to one archive: {.val {out_$Zip[duplicated(out_$Zip)]}}")
  }

  # The selection record, with the same rewrite in every text column.
  cols_ <- pbd_query(.con = .con, .sql = sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", pbd_lit(src_dep_)))
  txt_ <- cols_$column_name[cols_$column_type == "VARCHAR"]
  repl_ <- if (length(txt_) == 0L) "" else paste0(
    " REPLACE (",
    paste(sprintf("regexp_replace(replace(\"%s\", %s, ''), '(/Users/|/home/)[^/]+/', '~/') AS \"%s\"",
                  txt_, pbd_lit(paste0(.root, "/")), txt_), collapse = ", "),
    ")"
  )
  tmp_dep_ <- fs::path(.dir_out, "deployed.parquet.tmp")
  pbd_exec(.con = .con, .sql = pbd_sql_copy(
    .query     = sprintf("SELECT *%s FROM read_parquet(%s)", repl_, pbd_lit(src_dep_)),
    .path      = tmp_dep_,
    .row_group = 1000L
  ))
  pbd_promote(.tmp = tmp_dep_, .path = fs::path(.dir_out, "deployed.parquet"))

  pbd_write_csv(
    .tab  = dplyr::select(out_, "Zip", "Directory", "Task", "Context", "LabelledPublished", "Selected", "MacroF1"),
    .path = fs::path(.dir_out, "models_index.csv")
  )
  pbd_write_lines(.lines = readLines(.path_readme, warn = FALSE), .path = fs::path(.dir_out, "README.md"))
  out_
}

#' The LexNLP image, its lockfile, the notice its licence requires, and the usage guide
#'
#' @param .path_image Character. The image tarball.
#' @param .path_lock Character. requirements.lock.txt.
#' @param .path_readme Character. The usage guide.
#' @param .commit Character. The commit the image was built from.
#' @param .dir_out Character. Destination.
#' @return Tibble: File, Bytes, Status.
pbd_lexnlp <- function(.path_image, .path_lock, .path_readme, .commit, .dir_out) {
  if (FALSE) {
    .path_image  <- .lP$Input$FilLexnlpImage
    .path_lock   <- .lP$Input$FilLexnlpLock
    .path_readme <- .lP$Input$FilLexnlpReadme
    .commit      <- .lP$Params$LexnlpCommit
    .dir_out     <- fs::path(.lP$Output$DirPackage, "lexnlp")
  }
  out_ <- pbd_copy(.paths = c(.path_image, .path_lock), .dir_out = .dir_out)
  notice_ <- c(
    "# LexNLP image -- notice",
    "",
    sprintf("`%s` is the Docker image that extracted the organisation candidates of the database.",
            fs::path_file(.path_image)),
    "`docker load -i` restores it as `contracts-lexnlp:latest`; `README.md` beside this file explains how to run it.",
    "",
    "The image contains LexNLP 2.3.0, which is licensed under the GNU Affero General Public License v3.0",
    "(AGPL-3.0). Its source for that version is available from the Python Package Index",
    "(https://pypi.org/project/lexnlp/2.3.0/) and from https://github.com/LexPredict/lexpredict-lexnlp.",
    "The other packages in the image are listed with their versions in `requirements.lock.txt`; each is",
    "distributed under its own licence.",
    "",
    "The wrapper that runs LexNLP is inside the image at `/app/extract_lexnlp.py`, and in the pipeline repository",
    sprintf("in `contracts-lexnlp/` at commit %s, the commit the image was built from.", .commit),
    ""
  )
  st_notice_ <- pbd_write_lines(.lines = notice_, .path = fs::path(.dir_out, "NOTICE.md"))
  st_readme_ <- pbd_write_lines(.lines = readLines(.path_readme, warn = FALSE), .path = fs::path(.dir_out, "README.md"))
  dplyr::bind_rows(
    out_,
    tibble::tibble(File = c("NOTICE.md", "README.md"), Bytes = NA_real_, Status = c(st_notice_, st_readme_))
  )
}


# 8. Tidying the stage, and validation ---------------------------------------------------------------------------------

#' Move everything out of the stage that the package does not define
#'
#' The package is exactly the files this build defines. Anything else in the stage -- a part that is no longer
#' published, a file an earlier build wrote under another name -- would enter the manifest, the README and the
#' upload. It is moved aside, into a dated folder beside the stage, not deleted.
#'
#' @param .dir_package Character. The package.
#' @param .expected Character. The package's files, relative to it.
#' @param .dir_aside Character. Where superseded files go.
#' @return Tibble: Path, Bytes, To.
pbd_supersede <- function(.dir_package, .expected, .dir_aside) {
  if (FALSE) {
    .dir_package <- .lP$Output$DirPackage
    .expected    <- expected
    .dir_aside   <- .lP$Output$DirSuperseded
  }
  have_ <- list.files(path = .dir_package, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  extra_ <- setdiff(have_, .expected)
  if (length(extra_) == 0L) {
    return(tibble::tibble(Path = character(0), Bytes = numeric(0), To = character(0)))
  }
  to_root_ <- fs::path(.dir_aside, format(Sys.time(), "%Y%m%d-%H%M%S"))
  to_ <- fs::path(to_root_, extra_)
  bytes_ <- as.numeric(file.size(fs::path(.dir_package, extra_)))
  fs::dir_create(unique(fs::path_dir(to_)))
  ok_ <- file.rename(from = fs::path(.dir_package, extra_), to = to_)
  if (!all(ok_)) cli::cli_abort("Could not move aside: {.file {extra_[!ok_]}}")
  # Folders left empty go too, deepest first.
  dirs_ <- list.dirs(.dir_package, recursive = TRUE, full.names = TRUE)
  dirs_ <- dirs_[order(-nchar(dirs_))]
  for (d_ in dirs_[dirs_ != .dir_package]) {
    if (length(list.files(d_, all.files = TRUE, no.. = TRUE)) == 0L) unlink(d_, recursive = TRUE)
  }
  tibble::tibble(Path = extra_, Bytes = bytes_, To = as.character(to_root_))
}

#' Hold every published file against the schema: its columns, their order and their types
#'
#' @param .con A DBI connection.
#' @param .schema Tibble from pbd_schema_read().
#' @param .targets Tibble: Table, Path (a file or a glob), Format ("parquet" or "csv").
#' @return Tibble: Table, Path, Columns, Order, Types, Ok, Detail.
pbd_conform <- function(.con, .schema, .targets) {
  if (FALSE) {
    .con     <- con
    .schema  <- schema
    .targets <- targets_conform
  }
  purrr::pmap(.targets, \(Table, Path, Format) {
    reader_ <- if (Format == "csv") "read_csv_auto(%s)" else "read_parquet(%s)"
    have_ <- pbd_query(.con = .con, .sql = sprintf(paste("DESCRIBE SELECT * FROM", reader_), pbd_lit(Path)))
    want_ <- .schema[.schema$Table == Table & .schema$Action != "drop", ] |> dplyr::arrange(.data$Order)
    cols_ok_ <- setequal(have_$column_name, want_$Column)
    order_ok_ <- identical(have_$column_name, want_$Column)
    types_ok_ <- if (Format == "csv" || !cols_ok_) NA else identical(have_$column_type, want_$Type)
    detail_ <- c(
      if (!cols_ok_) sprintf("extra [%s] missing [%s]", paste(setdiff(have_$column_name, want_$Column), collapse = ", "),
                             paste(setdiff(want_$Column, have_$column_name), collapse = ", ")),
      if (isFALSE(types_ok_)) {
        bad_ <- have_$column_name[have_$column_type != want_$Type]
        sprintf("types differ: %s", paste(bad_, collapse = ", "))
      }
    )
    tibble::tibble(
      Table   = Table,
      Path    = as.character(fs::path_file(Path)),
      Columns = cols_ok_,
      Order   = order_ok_,
      Types   = types_ok_,
      Ok      = cols_ok_ && order_ok_ && !isFALSE(types_ok_),
      Detail  = paste(detail_, collapse = "; ")
    )
  }) |>
    purrr::list_rbind()
}

#' Search the package for machine paths
#'
#' Every text file (codebooks, guides, the notice, the lockfile, the manifest), the gzipped contract index, the text
#' entries inside the model archives, and the text columns of the small parquet files. The large tables are left out:
#' their text columns come from EDGAR, not from this machine.
#'
#' @param .con A DBI connection.
#' @param .dir_package Character. The package.
#' @return Tibble: File, Hits.
pbd_scan_paths <- function(.con, .dir_package) {
  if (FALSE) {
    .con         <- con
    .dir_package <- .lP$Output$DirPackage
  }
  pat_ <- "/Users/|/home/|Dropbox"
  files_ <- list.files(.dir_package, recursive = TRUE, full.names = TRUE)
  rel_ <- substring(files_, nchar(.dir_package) + 2L)
  out_ <- list()
  for (i_ in seq_along(files_)) {
    f_ <- files_[i_]
    hits_ <- NA_integer_
    if (grepl("\\.(csv|md|txt|json|sha256)$", f_)) {
      hits_ <- sum(grepl(pat_, readLines(f_, warn = FALSE)))
    } else if (grepl("\\.csv\\.gz$", f_)) {
      con_ <- gzfile(f_)
      hits_ <- sum(grepl(pat_, readLines(con_, warn = FALSE)))
      close(con_)
    } else if (grepl("\\.zip$", f_)) {
      ent_ <- zip::zip_list(f_)$filename
      ent_ <- ent_[grepl("\\.(json|log|txt|md|csv)$", ent_)]
      hits_ <- sum(purrr::map_int(ent_, \(.e) {
        c_ <- unz(f_, .e)
        on.exit(close(c_))
        sum(grepl(pat_, readLines(c_, warn = FALSE)))
      }))
    } else if (grepl("\\.parquet$", f_) && file.size(f_) < 50e6) {
      cols_ <- pbd_query(.con = .con, .sql = sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", pbd_lit(f_)))
      txt_ <- cols_$column_name[cols_$column_type == "VARCHAR"]
      hits_ <- if (length(txt_) == 0L) 0L else as.integer(pbd_query(
        .con = .con,
        .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s) WHERE %s", pbd_lit(f_),
                       paste(sprintf("regexp_matches(coalesce(\"%s\", ''), %s)", txt_, pbd_lit(pat_)), collapse = " OR "))
      )$n)
    }
    if (!is.na(hits_)) out_[[length(out_) + 1L]] <- tibble::tibble(File = rel_[i_], Hits = as.integer(hits_))
  }
  purrr::list_rbind(out_)
}

#' Every check the package must pass before it is shared
#'
#' @param .checks List of one-row tibbles made by pbd_check().
#' @return Tibble: Check, Expected, Found, Ok.
pbd_validate <- function(.checks) {
  if (FALSE) {
    .checks <- list(pbd_check("example", 0, 0))
  }
  purrr::list_rbind(.checks)
}

#' One validation row
#'
#' @param .check Character. What is checked.
#' @param .expected,.found The two values; compared as text.
#' @return One-row tibble: Check, Expected, Found, Ok.
pbd_check <- function(.check, .expected, .found) {
  tibble::tibble(
    Check    = .check,
    Expected = format(.expected, big.mark = ",", scientific = FALSE),
    Found    = format(.found, big.mark = ",", scientific = FALSE),
    Ok       = isTRUE(all.equal(as.character(.expected), as.character(.found)))
  )
}


# 9. The sample for the site, and the figures it quotes -----------------------------------------------------------------

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
  stems_ <- c("org_mentions", "places_geo", "law_clauses", "date_spans", "term_spans", "redact_spans")
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
    pbd_lit(fs::path(.dir_package, "core", "Labels.parquet")),
    pbd_lit(fs::path(.dir_package, "text", "exhibit10", "*.parquet")),
    spans_,
    as.integer(.n_per_class)
  )
  pbd_query(.con = .con, .sql = sql_)
}

#' Contracts the site needs to show two columns at work
#'
#' A sample cut from the labelled contracts has neither a rejected attachment nor a second registrant copy, so the
#' site could not show what Removed and PrimaryFiler do. A few of each are added: the first rejected attachments by
#' DocID, and every copy of the first attachments filed by several registrants.
#'
#' @param .con A DBI connection.
#' @param .dir_package Character. The package.
#' @param .n Integer. How many of each.
#' @return Tibble: DocID, Why.
pbd_sample_extras <- function(.con, .dir_package, .n = 3L) {
  if (FALSE) {
    .con         <- con
    .dir_package <- .lP$Output$DirPackage
    .n           <- 3L
  }
  src_ <- pbd_lit(fs::path(.dir_package, "core", "Contracts.parquet"))
  removed_ <- pbd_query(
    .con = .con,
    .sql = sprintf("SELECT DocID FROM read_parquet(%s) WHERE Removed ORDER BY DocID LIMIT %d", src_, as.integer(.n))
  )
  copies_ <- pbd_query(
    .con = .con,
    .sql = sprintf(
      paste("SELECT DocID FROM read_parquet(%1$s) WHERE HashDocument IN (SELECT HashDocument FROM read_parquet(%1$s)",
            "WHERE NOT Removed GROUP BY 1 HAVING count(*) > 1 ORDER BY 1 LIMIT %2$d) ORDER BY DocID"),
      src_, as.integer(.n)
    )
  )
  dplyr::bind_rows(
    tibble::tibble(DocID = removed_$DocID, Why = "rejected attachment"),
    tibble::tibble(DocID = copies_$DocID, Why = "registrant copy")
  )
}

#' Cut every published table down to the sampled documents
#'
#' The sample keeps the package's layout -- core/, spans/, text/ -- its file names and its columns, so a script
#' written against the sample runs against the whole package by changing its root. Only the markup column of the
#' text is left out, which keeps the site repository small. Rows are written in a fixed order, so an unchanged sample
#' is byte-identical from one build to the next and the site repository sees no change.
#'
#' @param .con A DBI connection.
#' @param .specs Tibble: File (in the sample), Source (in the package), Key ("DocID", "HashIndex", "ALL" for every
#'   row, or "" to copy a text file as it is).
#' @param .ids Character. The sampled DocIDs.
#' @param .dir_package Character. The package.
#' @param .dir_out Character. The staged sample; emptied first.
#' @return Tibble: File, Rows, Bytes.
pbd_sample_write <- function(.con, .specs, .ids, .dir_package, .dir_out) {
  if (FALSE) {
    .con         <- con
    .specs       <- specs_sample
    .ids         <- sample_ids
    .dir_package <- .lP$Output$DirPackage
    .dir_out     <- .lP$Output$DirSample
  }
  if (dir.exists(.dir_out)) unlink(.dir_out, recursive = TRUE)
  fs::dir_create(.dir_out)
  ids_ <- paste(pbd_lit(unique(.ids)), collapse = ", ")
  hashes_ <- pbd_query(
    .con = .con,
    .sql = sprintf("SELECT DISTINCT HashIndex FROM read_parquet(%s) WHERE DocID IN (%s)",
                   pbd_lit(fs::path(.dir_package, "core", "Contracts.parquet")), ids_)
  )$HashIndex
  purrr::pmap(.specs, \(File, Source, Key, ...) {
    src_ <- fs::path(.dir_package, Source)
    out_ <- fs::path(.dir_out, File)
    fs::dir_create(fs::path_dir(out_))
    if (Key == "") {
      pbd_write_lines(.lines = readLines(src_, warn = FALSE), .path = out_)
      return(tibble::tibble(File = File, Rows = NA_integer_, Bytes = as.numeric(file.size(out_))))
    }
    where_ <- if (Key == "ALL") {
      "TRUE"
    } else if (Key == "HashIndex") {
      sprintf("HashIndex IN (%s)", if (length(hashes_) > 0L) paste(pbd_lit(hashes_), collapse = ", ") else "NULL")
    } else {
      sprintf("\"%s\" IN (%s)", Key, ids_)
    }
    cols_ <- pbd_query(.con = .con, .sql = sprintf("DESCRIBE SELECT * FROM read_parquet(%s)", pbd_lit(src_)))$column_name
    select_ <- if ("HTML" %in% cols_) "* EXCLUDE (HTML)" else "*"
    tmp_ <- paste0(out_, ".tmp")
    pbd_exec(.con = .con, .sql = pbd_sql_copy(
      .query     = sprintf("SELECT %s FROM read_parquet(%s) WHERE %s ORDER BY ALL", select_, pbd_lit(src_), where_),
      .path      = tmp_,
      .row_group = 10000L
    ))
    pbd_promote(.tmp = tmp_, .path = out_)
    n_ <- pbd_query(.con = .con, .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s)", pbd_lit(out_)))$n
    tibble::tibble(File = File, Rows = as.integer(n_), Bytes = as.numeric(file.size(out_)))
  }) |>
    purrr::list_rbind()
}

#' The figures the site quotes, from the published files themselves
#'
#' @param .con A DBI connection.
#' @param .dir_package Character. The package.
#' @param .manifest Tibble from pbd_manifest().
#' @param .spans,.text,.models Tibbles from the steps above.
#' @param .version Character. The package version.
#' @param .path_out Character. The csv to write.
#' @return Tibble: Key, Value.
pbd_site_numbers <- function(.con, .dir_package, .manifest, .spans, .text, .models, .version, .path_out) {
  if (FALSE) {
    .con         <- con
    .dir_package <- .lP$Output$DirPackage
    .manifest    <- tab_manifest
    .spans       <- tab_spans
    .text        <- tab_text
    .models      <- tab_models
    .version     <- .lP$Params$Version
    .path_out    <- fs::path(.lP$Output$DirSample, "package_numbers.csv")
  }
  folders_ <- .manifest |>
    dplyr::mutate(Folder = ifelse(grepl("/", .data$Path), sub("/.*$", "", .data$Path), "root")) |>
    dplyr::summarise(Files = dplyr::n(), Bytes = sum(.data$Bytes), .by = "Folder")
  core_ <- c("Contracts", "Summaries", "Places", "TermDocs", "CtoOrders", "Labels", "KeywordTerms")
  rows_ <- purrr::map_chr(core_, \(.t) as.character(as.integer(pbd_query(
    .con = .con,
    .sql = sprintf("SELECT count(*) AS n FROM read_parquet(%s)",
                   pbd_lit(fs::path(.dir_package, "core", paste0(.t, ".parquet"))))
  )$n)))
  types_ <- unique(.text$Type)
  out_ <- dplyr::bind_rows(
    tibble::tibble(Key = "package.version", Value = .version),
    tibble::tibble(Key = "package.files", Value = as.character(nrow(.manifest) + 1L)),
    tibble::tibble(Key = "package.bytes", Value = as.character(sum(.manifest$Bytes))),
    tibble::tibble(Key = paste0("folder.files.", folders_$Folder), Value = as.character(folders_$Files)),
    tibble::tibble(Key = paste0("folder.bytes.", folders_$Folder), Value = as.character(folders_$Bytes)),
    tibble::tibble(Key = paste0("rows.", tolower(core_)), Value = rows_),
    tibble::tibble(Key = paste0("spans.", .spans$Stem), Value = as.character(.spans$RowsOut)),
    tibble::tibble(Key = "spans.total", Value = as.character(sum(.spans$RowsOut))),
    tibble::tibble(
      Key   = paste0("text.documents.", tolower(gsub("[^A-Za-z0-9]", "", types_))),
      Value = purrr::map_chr(types_, \(.t) as.character(sum(.text$RowsOut[.text$Type == .t])))
    ),
    tibble::tibble(Key = "text.years.exhibit10", Value = as.character(sum(.text$Type == "Exhibit10" & .text$RowsOut > 0))),
    tibble::tibble(Key = "models.count", Value = as.character(nrow(.models)))
  )
  pbd_write_csv(.tab = out_, .path = .path_out)
  out_
}

#' Make one folder of the site repository match a folder here
#'
#' Deletions run before copies. On a file system that ignores case, a file renamed only in case would otherwise be
#' copied over its old name and then deleted with it.
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
  list_ <- function(.root) if (dir.exists(.root)) list.files(.root, recursive = TRUE, all.files = TRUE) else character(0)
  src_ <- list_(.dir_from)
  dst_ <- list_(.dir_to)
  both_ <- intersect(src_, dst_)
  same_ <- if (length(both_) == 0L) logical(0) else
    unname(tools::md5sum(fs::path(.dir_from, both_)) == tools::md5sum(fs::path(.dir_to, both_)))
  plan_ <- dplyr::bind_rows(
    tibble::tibble(Path = setdiff(dst_, src_), Action = "delete"),
    tibble::tibble(Path = setdiff(src_, dst_), Action = "add"),
    tibble::tibble(Path = both_, Action = dplyr::if_else(same_, "same", "update"))
  )
  if (.apply) {
    del_ <- plan_$Path[plan_$Action == "delete"]
    if (length(del_) > 0L) file.remove(fs::path(.dir_to, del_))
    put_ <- plan_$Path[plan_$Action %in% c("add", "update")]
    if (length(put_) > 0L) {
      fs::dir_create(unique(fs::path_dir(fs::path(.dir_to, put_))))
      file.copy(from = fs::path(.dir_from, put_), to = fs::path(.dir_to, put_), overwrite = TRUE)
    }
    dirs_ <- list.dirs(.dir_to, recursive = TRUE, full.names = TRUE)
    for (d_ in dirs_[order(-nchar(dirs_))]) {
      if (d_ != .dir_to && length(list.files(d_, all.files = TRUE, no.. = TRUE)) == 0L) unlink(d_, recursive = TRUE)
    }
  }
  plan_
}


# 10. README, manifest and deployment ------------------------------------------------------------------------------------

#' A size in decimal units with three significant digits, as the site prints it
#'
#' @param .bytes Numeric.
#' @return Character.
pbd_size <- function(.bytes) {
  units_ <- c("B", "kB", "MB", "GB", "TB")
  i_ <- pmax(0, pmin(4, floor(log10(pmax(.bytes, 1)) / 3)))
  paste(trimws(formatC(signif(.bytes / 1000^i_, 3), format = "fg", digits = 3, big.mark = ",")), units_[i_ + 1])
}

#' The README of the package
#'
#' Written before the manifest, so the manifest covers it; the sizes it quotes are read from the folders themselves.
#'
#' @param .dir_package Character. The package.
#' @param .version Character. The package version.
#' @param .site_url Character. The documentation site.
#' @param .code_url Character. The pipeline repository.
#' @return Invisibly, "written" or "current".
pbd_readme <- function(.dir_package, .version, .site_url, .code_url) {
  if (FALSE) {
    .dir_package <- .lP$Output$DirPackage
    .version     <- .lP$Params$Version
    .site_url    <- .lP$Params$SiteUrl
    .code_url    <- .lP$Params$CodeUrl
  }
  rel_ <- list.files(.dir_package, recursive = TRUE)
  rel_ <- rel_[!rel_ %in% c("README.md", "MANIFEST.sha256") & !grepl("\\.tmp", rel_)]
  sizes_ <- tibble::tibble(Folder = sub("/.*$", "", rel_), Bytes = as.numeric(file.size(fs::path(.dir_package, rel_)))) |>
    dplyr::summarise(Files = dplyr::n(), Bytes = sum(.data$Bytes), .by = "Folder")
  about_ <- c(
    core   = "The database tables, the labelled sample, the keyword terms in use, the contract index, their codebooks",
    spans  = "The entity spans the rules kept: parties, places, law clauses, dates, terms, redaction marks",
    text   = "The documents, as filed (HTML) and as read (TextRaw), one file per type and filing year",
    models = "The six fine-tuned classifiers, their index, the selection record, a usage guide",
    lexnlp = "The image that extracted the organisation candidates, its lockfile, a notice, a usage guide"
  )
  sizes_ <- sizes_[match(names(about_), sizes_$Folder), ]
  rows_ <- sprintf("| `%s/` | %s | %d | %s |", names(about_), about_, sizes_$Files, pbd_size(sizes_$Bytes))
  lines_ <- c(
    paste("# matcon data --", .version),
    "",
    "The data of \"The analysis of material contracts: Use of SEC contractual data in accounting research\"",
    "(Grosskopf, Sehn and Uckert): every Exhibit 10 attachment filed on EDGAR from 1999 to 2024, classified, with its",
    "entities extracted, and the documents themselves.",
    "",
    paste0("Documentation, codebooks and worked examples: <", .site_url, ">. The code that builds this package: <",
           .code_url, ">."),
    "",
    "| Folder | Content | Files | Size |",
    "|:--|:--|--:|--:|",
    rows_,
    "",
    "Each folder can be downloaded on its own. Every table has a codebook beside it (`*_Codebook.csv`) giving each",
    "column's type, meaning, when it is missing, and its name in Stata.",
    "",
    "## Checking a download",
    "",
    "`MANIFEST.sha256` lists every file with its SHA-256 checksum. From the package folder (macOS, Linux):",
    "",
    "```",
    "shasum -a 256 -c MANIFEST.sha256                                  # everything",
    "grep -E '  (core|spans)/' MANIFEST.sha256 | shasum -a 256 -c      # only the parts you downloaded",
    "```",
    "",
    "## Reading the files",
    "",
    "- R: `arrow::read_parquet(\"core/Contracts.parquet\")`",
    "- Python: `pandas.read_parquet(\"core/Contracts.parquet\")`",
    "- SQL: `duckdb -c \"SELECT count(*) FROM 'core/Contracts.parquet'\"`",
    "- Stata: StataNow reads parquet with `import parquet`; other editions convert with R or Python first, using the",
    "  `StataName` column of the codebooks.",
    "",
    "Contracts has one row per registrant copy of an attachment: count contracts on `PrimaryFiler == 1`.",
    "Attachments the text screen rejected stay in the table with `Removed == TRUE`.",
    "",
    "## Span offsets",
    "",
    "Every span file gives positions into the document's `TextRaw` (in `text/`), counted in characters from 0, the",
    "end exclusive: the span is `TextRaw[Start:Stop]` in Python and `stringi::stri_sub(TextRaw, Start + 1, Stop)` in R.",
    "",
    "## The text as read",
    "",
    "`TextRaw` is exactly the text the classifiers and extractors read. In some HTML documents, quotation marks,",
    "apostrophes and dashes appear as control characters (U+0080 to U+009F): they were written in Windows-1252 and",
    "read as Latin-1. To restore them -- one character for one, so every offset stays valid:",
    "",
    "```r",
    "codes <- c(0x80, 0x82:0x8C, 0x8E, 0x91:0x9C, 0x9E, 0x9F)",
    "text  <- chartr(intToUtf8(codes), iconv(rawToChar(as.raw(codes)), \"CP1252\", \"UTF-8\"), text)",
    "```",
    "",
    "```python",
    "table = {c: bytes([c]).decode(\"cp1252\") for c in range(0x80, 0xA0) if c not in (0x81, 0x8D, 0x8F, 0x90, 0x9D)}",
    "text = text.translate(table)",
    "```",
    "",
    "A model or extractor run on repaired text reads slightly different input from the one that produced the data.",
    "",
    "## What is not in the package",
    "",
    "- **Compustat.** It is licensed. `EstiSample` marks the rows of the paper's estimation sample; the match to",
    "  Compustat can be rebuilt from `CIK` and `DateFiled`.",
    "- **Amounts.** Monetary amounts were extracted but are not used by the paper and are not published.",
    "",
    "## Known issues",
    "",
    "- **Columbia.** The gazetteer reads \"Columbia\" as the country Colombia, so Colombia is heavily over-counted in",
    "  `Places`, `places_geo` and the country counts of `Contracts`; in US contracts the word is almost always the",
    "  District of Columbia, British Columbia or a US city.",
    "- **Country spellings.** A country can appear under several names, some in other languages; count on",
    "  `GeoCountryIso`.",
    "- **Durations.** `DurationYears` is left missing above 30 years (`DurationDropped` says so); other date columns",
    "  are not capped.",
    "- **Broad label.** The paper's broad category is `ClassRollup`. `ClassBroad` comes from an additional model",
    "  trained on the broad labels directly; `HierConsistent` marks where the two differ.",
    "- **Span offsets.** `LawStart` and `PlaceStart` are stored as whole-numbered doubles, the other offsets as",
    "  integers.",
    "- **Parties.** A contract without any identified organisation has one row in `org_mentions` without a span.",
    "- **No document-level gold set** exists for the entities.",
    "",
    "## Licences",
    "",
    "Data: CC BY 4.0. Models: CC BY-SA 4.0, the licence of their base model. The LexNLP image contains software",
    "under the AGPL-3.0 (see `lexnlp/NOTICE.md`). The documents in `text/` are filings made public by the SEC on",
    "EDGAR.",
    ""
  )
  invisible(pbd_write_lines(.lines = lines_, .path = fs::path(.dir_package, "README.md")))
}

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

#' Copy the package to the Drive folder, file by file, only where the copy differs
#'
#' NOTHING ON DRIVE IS DELETED, AND NOTHING IS COPIED WHILE DRIVE HOLDS WHAT THE PACKAGE DOES NOT. The folder is
#' shared as a whole, so a file left there from an earlier build would be shared with it. Such files are listed and
#' the copy stops; removing them is a decision for a person.
#'
#' @param .dir_package Character. The package.
#' @param .dir_target Character. The version folder on Drive.
#' @param .manifest Tibble from pbd_manifest().
#' @param .apply Logical. FALSE only reports.
#' @return Tibble: Path, Bytes, Action ("copy", "current", "only on Drive").
pbd_deploy <- function(.dir_package, .dir_target, .manifest, .apply = TRUE) {
  if (FALSE) {
    .dir_package <- .lP$Output$DirPackage
    .dir_target  <- .lP$Output$DirDrive
    .manifest    <- tab_manifest
    .apply       <- FALSE
  }
  files_ <- c(.manifest$Path, "MANIFEST.sha256")
  src_ <- fs::path(.dir_package, files_)
  dst_ <- fs::path(.dir_target, files_)
  s_size_ <- file.size(src_)
  d_size_ <- file.size(dst_)
  current_ <- !is.na(d_size_) & d_size_ == s_size_ & file.mtime(dst_) >= file.mtime(src_)
  plan_ <- tibble::tibble(Path = files_, Bytes = as.numeric(s_size_), Action = ifelse(current_, "current", "copy"))
  if (dir.exists(.dir_target)) {
    there_ <- list.files(.dir_target, recursive = TRUE)
    extra_ <- setdiff(there_, files_)
    if (length(extra_) > 0L) {
      plan_ <- dplyr::bind_rows(plan_, tibble::tibble(Path = extra_, Bytes = NA_real_, Action = "only on Drive"))
    }
  }
  if (.apply) {
    extra_ <- plan_$Path[plan_$Action == "only on Drive"]
    if (length(extra_) > 0L) {
      cli::cli_abort(c(
        "Drive holds {length(extra_)} file{?s} the package does not. Nothing was copied.",
        purrr::set_names(utils::head(extra_, 30), rep("x", min(30L, length(extra_)))),
        "i" = "Delete them in {.path {(.dir_target)}} (Finder), then render again."
      ))
    }
    todo_ <- which(plan_$Action == "copy")
    if (length(todo_) > 0L) fs::dir_create(unique(fs::path_dir(dst_[todo_])))
    for (i_ in seq_along(todo_)) {
      j_ <- todo_[i_]
      cli::cli_alert_info("Copying {i_}/{length(todo_)}: {plan_$Path[j_]} ({pbd_size(plan_$Bytes[j_])})")
      file.copy(from = src_[j_], to = dst_[j_], overwrite = TRUE, copy.date = FALSE)
    }
  }
  plan_
}


# 11. Reports ---------------------------------------------------------------------------------------------------------

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
