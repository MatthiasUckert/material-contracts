# probe-publish.R -- READ-ONLY probe for the two publishing pairs (50A code, 50B data) ----------------------------
#
# HOW TO RUN
#   1. In RStudio, open the material-contracts project (File > Open Project... > material-contracts.Rproj), so that
#      its renv library is the one in use.
#   2. Open this file and click "Source".
#   3. It writes exactly one file, ~/Downloads/probe-publish.txt, and changes nothing else. Attach that file in the
#      chat.
#
# WHAT IT LOOKS AT
#   A. the working repo: branch, uncommitted changes, tracked files per folder, largest tracked files, and tracked
#      text files that mention a local path, Dropbox or API keys
#   B. local clones of rGetEDGAR and rLabelDocs: version, branch, last commit, tags, uncommitted changes
#   C. Google Drive for desktop, the external drive, free disk space
#   D. the inputs of the data package: release, labels, classification outputs, span release, parsed documents
#   E. every tracked file of the working repo

for (pkg in c("arrow", "fs")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package ", pkg, " is not available. Open material-contracts.Rproj first, then source this file again.")
  }
}


# 1. Helpers ----------------------------------------------------------------------------------------------------------

#' Run a command and print what it returned
#'
#' Output and errors are both printed, so a failing call shows up in the report instead of stopping the probe.
#'
#' @param .cmd Character. The program, e.g. "git".
#' @param .args Character vector. Its arguments, already quoted where needed.
#' @return Invisibly, the lines the command printed.
prb_cmd <- function(.cmd, .args) {
  if (FALSE) {
    .cmd  <- "git"
    .args <- c("-C", shQuote(fs::path_expand("~/RProjects/Projects/material-contracts")), "status", "-sb")
  }
  out_ <- tryCatch(
    expr = suppressWarnings(system2(command = .cmd, args = .args, stdout = TRUE, stderr = TRUE)),
    error = function(.e) paste("ERROR:", conditionMessage(.e))
  )
  if (length(out_) > 0L) cat(out_, sep = "\n")
  invisible(out_)
}

#' Print the rows, columns and column types of one or several parquet files read as one dataset
#'
#' @param .paths Character vector of parquet paths; missing ones are dropped.
#' @param .label Character. What to call the dataset in the report.
#' @return Invisibly, NULL.
prb_parquet <- function(.paths, .label) {
  if (FALSE) {
    .paths <- fs::path_expand("~/RProjects/Projects/material-contracts/2_output/10-ExportData/Output/Places.parquet")
    .label <- "Places"
  }
  paths_ <- .paths[file.exists(.paths)]
  if (length(paths_) == 0L) {
    cat("\nMISSING:", .label, "\n")
    return(invisible(NULL))
  }
  ds_ <- arrow::open_dataset(sources = paths_)
  types_ <- vapply(
    X = ds_$schema$fields,
    FUN = function(.f) .f$type$ToString(),
    FUN.VALUE = character(1)
  )
  cat(
    "\n==", .label,
    "| files:", length(paths_),
    "| rows:", format(nrow(ds_), big.mark = ","),
    "| columns:", length(types_), "\n"
  )
  cat(
    paste0(names(ds_), " <", types_, ">"),
    sep = "  ",
    fill = 120
  )
  invisible(NULL)
}

#' Print the size and the column structure of a csv file
#'
#' @param .path Character. The csv.
#' @param .label Character. What to call it in the report.
#' @return Invisibly, NULL.
prb_csv <- function(.path, .label) {
  if (FALSE) {
    .path  <- fs::path_expand("~/RProjects/Projects/material-contracts/0_data/Labels/ClassificationLabels.csv")
    .label <- "Labels"
  }
  if (!file.exists(.path)) {
    cat("\nMISSING:", .label, "\n")
    return(invisible(NULL))
  }
  tab_ <- utils::read.csv(file = .path)
  cat("\n==", .label, "| rows:", nrow(tab_), "| columns:", ncol(tab_), "\n")
  utils::str(
    object = tab_,
    vec.len = 2,
    nchar.max = 60,
    give.attr = FALSE
  )
  invisible(NULL)
}

#' Print the state of one git clone
#'
#' @param .dir Character. The clone.
#' @return Invisibly, NULL.
prb_clone <- function(.dir) {
  if (FALSE) {
    .dir <- fs::path_expand("~/RProjects/Projects/material-contracts")
  }
  cat("\n####", .dir, "\n")
  desc_ <- fs::path(.dir, "DESCRIPTION")
  if (file.exists(desc_)) {
    cat("DESCRIPTION Version:", read.dcf(file = desc_, fields = "Version")[1, 1], "\n")
  }
  if (!fs::dir_exists(fs::path(.dir, ".git"))) {
    cat("not a git clone\n")
    return(invisible(NULL))
  }
  git_ <- c("-C", shQuote(.dir))
  prb_cmd(.cmd = "git", .args = c(git_, "status", "-sb", "--untracked-files=no"))
  prb_cmd(.cmd = "git", .args = c(git_, "log", "-3", "--format=%h_%cd_%s", "--date=short"))
  cat("tags:\n")
  prb_cmd(.cmd = "git", .args = c(git_, "tag", "-l"))
  cat("remote:\n")
  prb_cmd(.cmd = "git", .args = c(git_, "remote", "-v"))
  invisible(NULL)
}


# 2. The probe ----------------------------------------------------------------------------------------------------------

#' Write the whole report
#'
#' Everything printed goes to the console and to the report file at the same time.
#'
#' @param .dir_repo Character. The material-contracts clone.
#' @param .path_report Character. Where the report is written.
#' @return Invisibly, the report path.
prb_run <- function(.dir_repo, .path_report) {
  if (FALSE) {
    .dir_repo    <- fs::path_expand("~/RProjects/Projects/material-contracts")
    .path_report <- fs::path_expand("~/Downloads/probe-publish.txt")
  }
  sink(file = .path_report, split = TRUE)
  on.exit(sink(), add = TRUE)
  out_ <- fs::path(.dir_repo, "2_output")
  git_ <- c("-C", shQuote(.dir_repo))

  cat("probe-publish.R |", format(Sys.time(), "%Y-%m-%d %H:%M"), "| R", as.character(getRversion()), "\n")

  # A. Working repo
  cat("\n######## A. Working repo ########\n")
  prb_cmd(.cmd = "git", .args = c(git_, "status", "-sb", "--untracked-files=no"))
  cat("\nuncommitted changes to tracked files (first 40):\n")
  dirty_ <- prb_cmd(.cmd = "git", .args = c(git_, "status", "--porcelain", "--untracked-files=no"))
  cat("count:", length(dirty_), "\n")
  prb_cmd(.cmd = "git", .args = c(git_, "log", "-3", "--format=%h_%cd_%s", "--date=short"))

  tracked_ <- suppressWarnings(system2(command = "git", args = c(git_, "ls-files"), stdout = TRUE))
  cat("\ntracked files per top-level folder:\n")
  top_ <- ifelse(grepl("/", tracked_, fixed = TRUE), sub("/.*$", "", tracked_), "(root)")
  print(table(top_))
  in_code_ <- tracked_[startsWith(tracked_, "1_code/")]
  sub_ <- ifelse(lengths(strsplit(in_code_, "/", fixed = TRUE)) > 2L, sub("^1_code/([^/]+)/.*$", "\\1", in_code_), "(files)")
  cat("\ntracked files inside 1_code:\n")
  print(table(sub_))

  info_ <- fs::file_info(path = fs::path(.dir_repo, tracked_))
  big_ <- utils::head(info_[order(as.numeric(info_$size), decreasing = TRUE), c("path", "size")], 15)
  cat("\nlargest tracked files:\n")
  cat(sprintf("%10s  %s", format(big_$size), fs::path_rel(path = big_$path, start = .dir_repo)), sep = "\n")

  ext_text_ <- "\\.(R|r|qmd|py|md|sh|toml|yml|yaml|csv|lock|txt|cff|json|tex|bib|css|Rprofile|gitignore|renvignore)$"
  text_ <- tracked_[grepl(ext_text_, tracked_) | fs::path_file(tracked_) %in% c("Dockerfile", ".Rprofile")]
  hits_ <- vapply(
    X = text_,
    FUN = function(.f) {
      lines_ <- tryCatch(
        expr = readLines(con = fs::path(.dir_repo, .f), warn = FALSE, encoding = "UTF-8"),
        error = function(.e) character(0)
      )
      sum(grepl("/Users/|Dropbox|ApiKeys", lines_))
    },
    FUN.VALUE = integer(1)
  )
  cat("\ntracked text files mentioning /Users/, Dropbox or ApiKeys (file: lines):\n")
  if (any(hits_ > 0L)) {
    cat(sprintf("%5d  %s", hits_[hits_ > 0L], names(hits_)[hits_ > 0L]), sep = "\n")
  } else {
    cat("none\n")
  }

  # B. Package clones
  cat("\n######## B. Package clones ########\n")
  cands_ <- fs::dir_ls(
    path = fs::path_expand("~/RProjects"),
    recurse = 3,
    type = "directory",
    regexp = "/(rGetEDGAR|rLabelDocs)[^/]*$",
    fail = FALSE
  )
  cands_ <- cands_[fs::file_exists(fs::path(cands_, "DESCRIPTION"))]
  if (length(cands_) == 0L) cat("no clone found under ~/RProjects (depth 3)\n")
  for (dir_ in cands_) prb_clone(.dir = dir_)
  cat("\nSpotlight, whole machine:\n")
  prb_cmd(.cmd = "mdfind", .args = c("-name", "rGetEDGAR.Rproj"))
  prb_cmd(.cmd = "mdfind", .args = c("-name", "rLabelDocs.Rproj"))

  # C. Storage
  cat("\n######## C. Storage ########\n")
  dir_cloud_ <- fs::path_expand("~/Library/CloudStorage")
  if (fs::dir_exists(dir_cloud_)) {
    for (dir_ in fs::dir_ls(path = dir_cloud_, type = "directory")) {
      cat("\n", dir_, "\n", sep = "")
      print(fs::path_file(fs::dir_ls(path = dir_, fail = FALSE)))
    }
  } else {
    cat("~/Library/CloudStorage does not exist\n")
  }
  path_image_ <- "/Volumes/Ext20TB/ContractsBackUp/contracts-lexnlp-17a366b025fe.tar.gz"
  cat("\nLexNLP image on the external drive present:", file.exists(path_image_), "\n")
  prb_cmd(.cmd = "df", .args = c("-h", shQuote(fs::path_expand("~"))))

  # D. Inputs of the data package
  cat("\n######## D1. Release ########\n")
  dir_rel_ <- fs::path(out_, "10-ExportData", "Output")
  for (stem_ in c("Contracts", "Summaries", "Places", "TermDocs", "CtoOrders")) {
    prb_parquet(.paths = fs::path(dir_rel_, paste0(stem_, ".parquet")), .label = stem_)
  }
  prb_csv(.path = fs::path(dir_rel_, "Contracts_Codebook.csv"), .label = "Contracts_Codebook.csv")

  cat("\n######## D2. Labels ########\n")
  prb_csv(.path = fs::path(.dir_repo, "0_data", "Labels", "ClassificationLabels.csv"), .label = "ClassificationLabels.csv")
  prb_parquet(.paths = fs::path(out_, "03A-ClassifyPrepare", "prepared.parquet"), .label = "03A prepared")

  cat("\n######## D3. Classification outputs ########\n")
  prb_parquet(
    .paths = fs::path(out_, "03F-ClassifyApply", "release", "contract_labels_L256.parquet"),
    .label = "03F contract_labels_L256"
  )
  # The name is matched on the file name only: the directory itself is called ...Keyword, so a match on the full
  # path would return every file under it.
  files_03c_ <- fs::dir_ls(path = fs::path(out_, "03C-ClassifyTrainKeyword"), recurse = TRUE, type = "file")
  kw_ <- files_03c_[grepl("keyword|table|term", fs::path_file(files_03c_), ignore.case = TRUE)]
  cat("\n== 03C:", length(files_03c_), "files, of which named like a keyword table:", length(kw_), "\n")
  cat(utils::head(fs::path_rel(path = kw_, start = fs::path(out_, "03C-ClassifyTrainKeyword")), 30), sep = "\n")

  dir_mf_ <- fs::path(out_, "03B-ClassifyTrainBERT", "model_final")
  mf_ <- fs::dir_info(path = dir_mf_, recurse = 1)
  mf_ <- mf_[order(mf_$path), ]
  cat("\n== 03B model_final (size, type, path)\n")
  cat(sprintf("%10s  %-9s  %s", format(mf_$size), as.character(mf_$type), fs::path_rel(mf_$path, dir_mf_)), sep = "\n")

  cat("\n######## D4. Span release (04D) ########\n")
  for (dir_pass_ in fs::dir_ls(path = fs::path(out_, "04D-EntityApply", "Store"), type = "directory")) {
    for (dir_hash_ in fs::dir_ls(path = dir_pass_, type = "directory")) {
      chunks_ <- fs::dir_ls(path = dir_hash_, type = "directory", regexp = "/chunk-[^/]+$")
      cat("\n####", fs::path_rel(dir_hash_, out_), "| chunk directories:", length(chunks_), "\n")
      if (length(chunks_) == 0L) next
      files_ <- fs::path_file(fs::dir_ls(path = chunks_[1], type = "file"))
      cat("files per chunk:", paste(files_, collapse = ", "), "\n")
      for (file_ in files_[!startsWith(files_, "_")]) {
        prb_parquet(.paths = fs::path(chunks_, file_), .label = fs::path(fs::path_file(dir_pass_), file_))
      }
    }
  }

  cat("\n######## D5. Parsed documents (01B) ########\n")
  dir_parsed_ <- fs::path(out_, "01B-EdgarDocuments", "GetEDGAR", "DocumentData", "Parsed")
  for (dir_type_ in fs::dir_ls(path = dir_parsed_, type = "directory")) {
    dirs_yq_ <- sort(fs::dir_ls(path = dir_type_, type = "directory"))
    cat(
      "\n####", fs::path_file(dir_type_),
      "| quarters:", length(dirs_yq_),
      "| first:", fs::path_file(dirs_yq_[1]),
      "| last:", fs::path_file(dirs_yq_[length(dirs_yq_)]), "\n"
    )
    dir_mid_ <- dirs_yq_[length(dirs_yq_) %/% 2L + 1L]
    files_mid_ <- fs::dir_ls(path = dir_mid_, type = "file")
    cat("documents in", fs::path_file(dir_mid_), ":", length(files_mid_), "\n")
    prb_parquet(.paths = files_mid_[1], .label = fs::path_rel(path = files_mid_[1], start = dir_parsed_))
  }

  # E. Every tracked file
  cat("\n######## E. Every tracked file ########\n")
  cat(tracked_, sep = "\n")

  cat("\nDone. Nothing was changed; the report is", .path_report, "\n")
  invisible(.path_report)
}


# 3. Run ------------------------------------------------------------------------------------------------------------

prb_run(
  .dir_repo    = fs::path_expand("~/RProjects/Projects/material-contracts"),   # the working repo
  .path_report = fs::path_expand("~/Downloads/probe-publish.txt")              # the one file this writes
)
