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
    .yq         <- "2015-3"
    .doc_id     <- "0000060512-b1ef140a0a2bfd14412f722475dcc7b8"
  }

  fs::path(
    .dir_mirror, "DocumentData", "Parsed",
    .doc_type, .yq, paste0(.doc_id, ".parquet")
  )
}
