# probe-02-history.R -- READ-ONLY: when did 02A-Compustat.R and 02B-Register.R stop being function libraries? -------
#
# In the committed state both files hold the text of a Final Exhibits runbook instead of their functions, so neither
# parses and neither 02A nor 02B can render. This probe lists every commit that changed each file and says, for each
# version, whether it parses and what its first line is. The last version that parses is the one to restore.
#
# HOW TO RUN: with the material-contracts project open, open this file and click "Source". It changes nothing and
# writes one report, ~/Downloads/probe-02-history.txt. Attach that file in the chat.

probe_versions <- function(.dir_repo, .path) {
  if (FALSE) {
    .dir_repo <- here::here()
    .path     <- "1_code/02A-Compustat.R"
  }
  git_ <- function(.args) {
    suppressWarnings(system2(
      command = "git",
      args    = c("-C", shQuote(.dir_repo), shQuote(.args)),
      stdout  = TRUE,
      stderr  = FALSE
    ))
  }
  log_ <- git_(c("log", "--follow", "--date=short", "--format=%H%x09%cd%x09%s", "--", .path))
  if (length(log_) == 0L) {
    cat("\nno history for", .path, "\n")
    return(invisible(NULL))
  }
  parts_ <- strsplit(log_, "\t", fixed = TRUE)
  out_ <- purrr::map(parts_, \(.p) {
    code_ <- git_(c("show", paste0(.p[1], ":", .path)))
    parses_ <- tryCatch(
      expr = {
        parse(text = code_, keep.source = FALSE)
        TRUE
      },
      error = function(.e) FALSE
    )
    tibble::tibble(
      Commit = stringi::stri_sub(.p[1], from = 1L, to = 7L),
      Date   = .p[2],
      Lines  = length(code_),
      Parses = parses_,
      First  = stringi::stri_sub(if (length(code_) > 0L) code_[1] else "", from = 1L, to = 60L),
      Title  = stringi::stri_sub(paste(grep("^title:", code_, value = TRUE), collapse = " "), from = 1L, to = 60L),
      Msg    = stringi::stri_sub(.p[3], from = 1L, to = 30L)
    )
  }) |>
    purrr::list_rbind()
  cat("\n####", .path, "--", nrow(out_), "versions, newest first\n")
  print(as.data.frame(out_), right = FALSE, row.names = FALSE)
  good_ <- out_$Commit[out_$Parses]
  cat("newest version that parses:", if (length(good_) > 0L) good_[1] else "none", "\n")
  invisible(out_)
}

path_report <- fs::path_expand("~/Downloads/probe-02-history.txt")
fs::dir_create(fs::path_dir(path_report))
sink(file = path_report, split = TRUE)
dir_repo <- here::here()
cat("probe-02-history.R |", format(Sys.time(), "%Y-%m-%d %H:%M"), "|", dir_repo, "\n")
for (path in c("1_code/02A-Compustat.R", "1_code/02B-Register.R")) {
  probe_versions(.dir_repo = dir_repo, .path = path)
}
cat("\nDone. Nothing was changed; the report is", path_report, "\n")
sink()
