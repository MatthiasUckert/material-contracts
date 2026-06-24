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
