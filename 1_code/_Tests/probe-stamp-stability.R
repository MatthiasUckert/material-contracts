# probe-stamp-stability.R -- READ-ONLY: is the input stamp of a text year the same twice in a row? ---------------------
#
# Three Exhibit 10 years rebuild on every render of 50B -- the same three, 2010, 2016 and 2022 -- although nothing in
# them changes. The stamp of a year is a hash of what its quarter folders hold: the number of files, their total size
# and the newest modification time. If a listing comes back incomplete without erroring, that triple changes and the
# year rebuilds.
#
# This lists every Exhibit 10 year three times and reports the triple of each pass. A year whose three passes agree is
# stable; a year whose passes differ is the fault, and the differing number says what the listing got wrong.
#
# HOW TO RUN: with the material-contracts project open, open this file and click "Source". It reads only, and writes
# ~/Downloads/probe-stamp-stability.txt. Attach that file in the chat. It takes a few minutes: it lists 1.5 million
# files three times.

path_report <- fs::path_expand("~/Downloads/probe-stamp-stability.txt")
fs::dir_create(fs::path_dir(path_report))
while (sink.number() > 0L) sink()
sink(file = path_report, split = TRUE)

cat("probe-stamp-stability.R |", format(Sys.time(), "%Y-%m-%d %H:%M"), "| fs",
    as.character(utils::packageVersion("fs")), "\n")

dir_type <- here::here("2_output", "01B-EdgarDocuments", "GetEDGAR", "DocumentData", "Parsed", "Exhibit10")
dirs_yq <- sort(fs::dir_ls(path = dir_type, type = "directory", regexp = "/[0-9]{4}-[1-4]$"))
years <- unique(stringi::stri_sub(fs::path_file(dirs_yq), from = 1L, to = 4L))
cat("years:", length(years), "| quarter folders:", length(dirs_yq), "\n\n")

# One pass over a year's folders: what utils_dir_stamp() reduces to a hash.
triple <- function(.dirs) {
  inf_ <- tryCatch(
    expr  = fs::dir_info(path = .dirs, recurse = TRUE, type = "file"),
    error = function(.e) NULL
  )
  if (is.null(inf_)) return(c(Files = NA_real_, Bytes = NA_real_, Latest = NA_real_))
  c(
    Files  = nrow(inf_),
    Bytes  = sum(as.numeric(inf_$size)),
    Latest = round(as.numeric(max(inf_$modification_time)))
  )
}

out <- purrr::map(years, \(.year) {
  dirs_ <- dirs_yq[startsWith(fs::path_file(dirs_yq), paste0(.year, "-"))]
  passes_ <- purrr::map(1:3, \(.i) triple(.dirs = dirs_))
  stamps_ <- purrr::map_chr(1:3, \(.i) utils_dir_stamp(.dirs = dirs_, .extra = NULL))
  tibble::tibble(
    Year    = .year,
    Files   = paste(unique(purrr::map_dbl(passes_, "Files")), collapse = " / "),
    Bytes   = paste(unique(purrr::map_dbl(passes_, "Bytes")), collapse = " / "),
    Latest  = paste(unique(purrr::map_dbl(passes_, "Latest")), collapse = " / "),
    Stamps  = length(unique(stamps_)),
    Stable  = length(unique(stamps_)) == 1L &&
      length(unique(purrr::map_dbl(passes_, "Files"))) == 1L &&
      length(unique(purrr::map_dbl(passes_, "Bytes"))) == 1L
  )
}) |>
  purrr::list_rbind()

cat("######## Every year, three passes ########\n")
print(as.data.frame(out), row.names = FALSE, right = FALSE)

cat("\n######## Unstable years ########\n")
bad <- out[!out$Stable, ]
if (nrow(bad) == 0L) {
  cat("none -- every year gave the same triple and the same stamp three times\n")
} else {
  print(as.data.frame(bad), row.names = FALSE, right = FALSE)
  cat("\nA differing Files or Bytes is a listing that came back incomplete; a differing Latest alone means a file in\n")
  cat("that year was touched between the passes.\n")
}

cat("\n######## What the stamps stored by 50B say ########\n")
dir_stamps <- here::here("2_output", "40B-PublishData", "Stamps")
for (year_ in out$Year) {
  path_ <- fs::path(dir_stamps, paste0("text__exhibit10__exhibit10_", year_, ".parquet.stamp"))
  cat(year_, ":", if (fs::file_exists(path_)) readLines(path_, warn = FALSE)[1] else "no stamp file", "\n")
}

cat("\nDone. Nothing was changed; the report is", as.character(path_report), "\n")
sink()
