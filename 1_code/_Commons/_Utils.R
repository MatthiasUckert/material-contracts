# File Paths ----------------------------------------------------------------------------------
utils_list_files <- function(.dirs, .reg = NULL, .id = "DocID", .rec = FALSE) {
  paths_ <- list.files(.dirs, pattern = .reg, full.names = TRUE, recursive = .rec)
  ids_ <- paths_ |>
    fs::path_file() |>
    fs::path_ext_remove()

  tibble::tibble(
    "{.id}" := ids_,
    Path     = purrr::set_names(paths_, ids_)
  )
}

utils_file_path <- function(...) {
  path_ <- file.path(...)
  ext_ <- tools::file_ext(path_)
  if (ext_ == "") {
    fs::dir_create(path_)
  } else {
    fs::dir_create(dirname(path_))
  }

  fs::path_abs(fs::path_tidy(path_))
}

utils_list_project_files <- function(.dir_data, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .dir_data <- .lP$Input$DirContracts
  }

  if (file.exists(.path_out) & !.rerun) {
    return(arrow::read_parquet(.path_out))
  }


  out_ <- utils_list_files(.dir_data, .rec = TRUE) |>
    dplyr::mutate(
      DocType = basename(dirname(dirname(Path))),
      YQ = basename(dirname(Path)),
      .after = DocID
    ) |>
    dplyr::mutate(Path = unname(Path))

  arrow::write_parquet(out_, .path_out)
  return(out_)
}


#' Rebuild the path to a parsed document from the columns that identify it
#'
#' THE REGISTER CARRIES NO ABSOLUTE PATHS. A path written on one machine is wrong on every other,
#' which is why the migration verification had to exclude path columns and why an archived table
#' would not work for anyone who downloaded it. What identifies a document is its type, its quarter
#' and its identifier, and those are stable everywhere; the path is derived from them at read time.
#'
#' One definition, used by every script that opens a document, because a path rule restated in four
#' places is four rules that will eventually differ.
#'
#' @param .dir_mirror The rGetEDGAR root holding the parsed corpus, e.g. 01B's GetEDGAR directory.
#' @param .doc_type Character vector. Document type as the index records it.
#' @param .yq Character vector. Year and quarter, e.g. "2015-3".
#' @param .doc_id Character vector. Document identifier.
#' @return Character vector of absolute paths.
utils_doc_path <- function(.dir_mirror, .doc_type, .yq, .doc_id) {
  if (FALSE) {
    .dir_mirror <- here::here("2_output", "01B-EdgarDocuments", "GetEDGAR")
    .doc_type   <- "Exhibit10"
    .yq         <- 2015.3            # the register's form
    .doc_id     <- "0000060512-b1ef140a0a2bfd14412f722475dcc7b8"
  }

  # The register stores year-quarter as a DOUBLE (2006.3); the mirror's directories are "2006-3".
  # Normalising here rather than at each call site is deliberate: this is the one function where a
  # year-quarter becomes a directory name, so it is the one place the format question arises, and no
  # caller can forget it. The conversion is arithmetic, not as.character(), because how a double
  # prints is not a property to build a file path on. It is idempotent on the character form --
  # "2006-3" round-trips to itself -- so callers already passing directory strings are unaffected.
  qtr_ <- round(suppressWarnings(as.numeric(.yq)) * 10)
  yq_  <- ifelse(is.na(qtr_), as.character(.yq), paste0(qtr_ %/% 10, "-", qtr_ %% 10))

  fs::path(
    .dir_mirror, "DocumentData", "Parsed",
    .doc_type, yq_, paste0(.doc_id, ".parquet")
  )
}


# Cache fingerprints --------------------------------------------------------------------------

#' A cheap fingerprint of what a set of directories currently holds
#'
#' Expensive steps in this pipeline cache their result and skip the work when the cache is current.
#' A cache keyed only on a file name carries no fingerprint of the input it was built from: change
#' the input, leave the cache in place, and the stale result is served silently. There is no error,
#' no warning, and no visible difference from a correct run. This function is what turns a
#' name-keyed cache into a data-keyed one, and it costs one directory listing.
#'
#' THE FINGERPRINT IS METADATA, NOT CONTENT. It hashes the number of files, their total size and the
#' latest modification time -- never the bytes. The directories it guards hold hundreds of gigabytes,
#' so reading them would cost far more than the computation being cached. The trade is explicit: an
#' edit preserving file count, total size and modification time would go undetected. Nothing in this
#' pipeline rewrites a mirrored file in place, so the case does not arise; a step where it could must
#' pass .rerun = TRUE rather than trust the stamp.
#'
#' MODIFICATION TIMES ENTER AS EPOCH SECONDS, NOT AS FORMATTED TEXT. A formatted timestamp carries
#' the session's time zone, so the same directory would stamp differently under a changed locale and
#' every cache would miss at once.
#'
#' @param .dirs Character vector of paths. Directories are listed recursively; a path naming a single
#'   file contributes that file. One that does not exist contributes nothing rather than raising, so
#'   a stamp taken before the first run is well defined and simply differs from every later one.
#' @param .extra Any R object whose value belongs in the key: the parameters that shaped the
#'   computation, or an upstream result it consumed. NULL where the directories are the whole story.
#' @return A sixteen-character string.
utils_dir_stamp <- function(.dirs, .extra = NULL) {
  if (FALSE) {
    .dirs  <- .lP$Edgar$MasterIndex$DirParquet
    .extra <- list(Forms = edg_sec_forms(), MaxYear = 2024L)
  }

  # fs::file_exists() is TRUE for a directory too, so existence and kind are two separate tests.
  # Single files are accepted because several inputs in this pipeline are one parquet rather than a
  # directory of them, and stamping the file's parent would react to unrelated siblings.
  have_ <- .dirs[fs::file_exists(.dirs)]
  isdir_ <- fs::dir_exists(have_)

  inf_ <- dplyr::bind_rows(
    if (any(isdir_))  fs::dir_info(path = have_[isdir_], recurse = TRUE, type = "file"),
    if (any(!isdir_)) fs::file_info(path = have_[!isdir_])
  )

  any_ <- !is.null(inf_) && nrow(inf_) > 0L

  key_ <- list(
    nFiles = if (any_) nrow(inf_) else 0L,
    nBytes = if (any_) sum(as.numeric(inf_$size)) else 0,
    Latest = if (any_) round(as.numeric(max(inf_$modification_time))) else 0,
    Extra  = .extra
  )

  stringi::stri_sub(str = rlang::hash(key_), from = 1L, to = 16L)
}


#' A fingerprint of a directory tree too large to list
#'
#' utils_dir_stamp() lists every file, which is right for a directory of a few hundred parquets and
#' wrong for the parsed corpus. fs::dir_info(recurse = TRUE) over 1.46 million documents costs about
#' what the walk being avoided costs, so stamping a tree that way buys nothing.
#'
#' This descends a fixed number of levels and hashes the DIRECTORIES -- how many there are and when
#' each last changed -- without ever enumerating the files inside them. On the parsed corpus that is
#' roughly four hundred stat calls in place of one and a half million.
#'
#' WHAT IT SEES AND WHAT IT DOES NOT. Adding or removing a file updates the modification time of the
#' directory holding it, so both are detected. Rewriting a file in place is not: the directory entry
#' is unchanged and only the file's own timestamp moves. Nothing in this pipeline rewrites a
#' retrieved document -- each is written once and never returned to -- but a tree where that could
#' happen must pass .rerun = TRUE rather than trust this.
#'
#' DESCENT IS LEVEL BY LEVEL, NOT RECURSIVE. fs::dir_ls(recurse = TRUE, type = "directory") reads
#' every entry in the tree in order to decide which of them are directories, which is the cost this
#' function exists to avoid.
#'
#' @param .dirs Root of the tree.
#' @param .depth Integer. How many levels of subdirectory to descend. The parsed corpus is
#'   <root>/<DocType>/<YQ>/, so two levels reaches the directories that hold documents.
#' @param .extra Any R object whose value belongs in the key.
#' @return A sixteen-character string.
utils_tree_stamp <- function(.dirs, .depth = 2L, .extra = NULL) {
  if (FALSE) {
    .dirs  <- .lP$Edgar$DocumentData$Parsed
    .depth <- 2L
    .extra <- NULL
  }

  if (!fs::dir_exists(.dirs)) return(utils_dir_stamp(.dirs = character(0), .extra = .extra))

  cur_  <- fs::path(.dirs)
  seen_ <- cur_
  left_ <- .depth
  while (left_ > 0L && length(cur_) > 0L) {
    cur_  <- fs::dir_ls(path = cur_, recurse = FALSE, type = "directory")
    seen_ <- c(seen_, cur_)
    left_ <- left_ - 1L
  }

  inf_ <- fs::file_info(path = seen_)

  key_ <- list(
    nDirs  = length(seen_),
    Mtimes = round(as.numeric(inf_$modification_time)),
    Extra  = .extra
  )

  stringi::stri_sub(str = rlang::hash(key_), from = 1L, to = 16L)
}


#' The stamp a cache file was written under, or NA if there is not one
#'
#' Split out because every cached step needs the same three-line dance -- does the file exist, does
#' it carry a Stamp column, what does that column say -- and reading only that column is what makes
#' the check cheap on a cache holding millions of rows.
#'
#' Returns NA rather than raising on a cache written before this convention existed, or by a
#' different version of the code. A missing stamp is a cache miss, which is the safe reading.
#'
#' @param .path Path to a cache parquet.
#' @return A single string, or NA_character_.
utils_stamp_read <- function(.path) {
  if (FALSE) .path <- .lP$Cache$FrameIndex

  if (!fs::file_exists(.path)) return(NA_character_)

  tryCatch(
    {
      val_ <- arrow::read_parquet(file = .path, col_select = "Stamp")[["Stamp"]]
      if (length(val_) == 0L) NA_character_ else as.character(val_[[1L]])
    },
    error = function(.e) NA_character_
  )
}
