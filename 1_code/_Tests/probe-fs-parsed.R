# probe-fs-parsed.R -- READ-ONLY: which folder does fs fail to list, and why? --------------------------------------------
#
# 50B stopped in utils_dir_stamp(), which lists the quarter folders of a document type with fs::dir_info(). The error
# ("SET_STRING_ELT() must be a 'CHARSXP' not a 'special'") comes from inside fs and usually means one entry in the
# folder has a name fs cannot turn into a string -- an invalid byte sequence, a control character, a broken symlink.
#
# This walks every quarter folder of every document type three ways: with fs, with base R, and looking at the names
# themselves. The folder where fs fails but base R does not is the one to look at.
#
# HOW TO RUN: with the material-contracts project open, open this file and click "Source". It reads only, and writes
# ~/Downloads/probe-fs-parsed.txt. Attach that file in the chat. It takes a few minutes: it lists 1.5 million files.

path_report <- fs::path_expand("~/Downloads/probe-fs-parsed.txt")
fs::dir_create(fs::path_dir(path_report))
while (sink.number() > 0L) sink()
sink(file = path_report, split = TRUE)

cat("probe-fs-parsed.R |", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat(R.version.string, "| fs", as.character(utils::packageVersion("fs")), "\n")
cat("locale:", Sys.getlocale("LC_CTYPE"), "\n")

dir_parsed <- here::here("2_output", "01B-EdgarDocuments", "GetEDGAR", "DocumentData", "Parsed")
cat("parsed:", dir_parsed, "\n")

for (dir_type in fs::dir_ls(path = dir_parsed, type = "directory")) {
  cat("\n########", fs::path_file(dir_type), "########\n")
  dirs_yq <- sort(fs::dir_ls(path = dir_type, type = "directory", regexp = "/[0-9]{4}-[1-4]$"))
  for (dir_q in dirs_yq) {
    # 1. fs, the way utils_dir_stamp() lists it.
    fs_n <- tryCatch(
      expr  = nrow(fs::dir_info(path = dir_q, recurse = TRUE, type = "file")),
      error = function(.e) paste("FS FAILED:", conditionMessage(.e))
    )
    # 2. base R, which tolerates names fs rejects.
    names_ <- tryCatch(
      expr  = list.files(path = dir_q, all.files = TRUE, no.. = TRUE),
      error = function(.e) character(0)
    )
    # 3. the names themselves: not valid UTF-8, a control character, or not the expected .parquet.
    odd_ <- names_[!validUTF8(names_) | grepl("[[:cntrl:]]", names_) | !grepl("\\.parquet$", names_)]
    flag_ <- is.character(fs_n) || length(odd_) > 0L || (is.numeric(fs_n) && fs_n != length(names_))
    if (flag_ || fs::path_file(dir_q) %in% c("2002-1", "2002-2", "2002-3", "2002-4")) {
      cat(
        fs::path_file(dir_q),
        "| fs:", if (is.character(fs_n)) fs_n else format(fs_n, big.mark = ","),
        "| base:", format(length(names_), big.mark = ","),
        "| odd names:", length(odd_), "\n"
      )
      if (length(odd_) > 0L) cat("   ", utils::head(encodeString(odd_, quote = '"'), 5), sep = "\n   ")
    }
  }
}

cat("\n######## The call that failed, on 2002 as a whole ########\n")
dirs_2002 <- fs::dir_ls(
  path    = fs::path(dir_parsed, "Exhibit10"),
  type    = "directory",
  regexp  = "/2002-[1-4]$"
)
cat("folders:", length(dirs_2002), "\n")
res <- tryCatch(
  expr  = nrow(fs::dir_info(path = dirs_2002, recurse = TRUE, type = "file")),
  error = function(.e) paste("FAILED:", conditionMessage(.e))
)
cat("fs::dir_info on all four at once:", if (is.character(res)) res else format(res, big.mark = ","), "\n")
res2 <- tryCatch(
  expr  = sum(purrr::map_int(dirs_2002, \(.d) nrow(fs::dir_info(path = .d, recurse = TRUE, type = "file")))),
  error = function(.e) paste("FAILED:", conditionMessage(.e))
)
cat("fs::dir_info one folder at a time:", if (is.character(res2)) res2 else format(res2, big.mark = ","), "\n")

cat("\nDone. Nothing was changed; the report is", as.character(path_report), "\n")
sink()
