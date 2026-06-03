init_create_script_dir <- function(.dir_here, .name_script) {
  dir_script_ <- fs::dir_create(file.path(.dir_here, "2_output", .name_script))
  return(dir_script_)
}

init_create_script_fun <- function(.dir_here, .name_script) {
  dir_fun_ <- fs::dir_create(file.path(.dir_here, "1_code"))
  path_fun_ <- file.path(dir_fun_, paste0(.name_script, ".R"))
  
  if (!file.exists(path_fun_)) {
    write("", path_fun_)
  }
  return(fs::path_abs(path_fun_))
  
}
