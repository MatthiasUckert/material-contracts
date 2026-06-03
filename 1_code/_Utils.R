# File Paths ----------------------------------------------------------------------------------
utils_list_files <- function(.dirs, .reg = NULL, .id = "DocID", .rec = FALSE) {
  purrr::map(
    .x = .dirs,
    .f = ~ tibble::tibble(Path = list.files(.x, .reg, FALSE, TRUE, .rec))
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      !!dplyr::sym(.id) := fs::path_ext_remove(basename(Path)),
      Path = purrr::set_names(Path, !!dplyr::sym(.id))
    ) |>
    dplyr::select(!!dplyr::sym(.id), Path)
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