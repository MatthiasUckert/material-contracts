# 50A-PublishCode: the published copy of the pipeline and its packages ------------------------------------------------
#
# WHAT THIS FILE DOES
# It decides which committed files of this repository and of the two package clones are published, checks them
# for what must never be public, and mirrors them into the published repository -- under `pipeline/` and
# `packages/`, and nowhere else there.
#
# THE SHAPE
#   1. state    where each clone stands: commit, uncommitted edits, ahead of or behind GitHub
#   2. files    every file in each commit, with its committed size and mode
#   3. rules    an ordered rule table decides each file; the first match wins; an undecided file stops the run
#   4. stage    the chosen files are copied out of the commit with `git archive`, never from the working tree
#   5. checks   the staged copy is searched for home paths, credentials, data files, oversized files, code that
#               does not parse, and references to files that are not published
#   6. mirror   the owned subtrees of the target are made to match the stage; nothing else there is touched
#
# COMMIT AND PUSH ARE NOT HERE. The document leaves the change in the target repository and reports it; publishing
# it is a decision taken in that repository.
#
# GIT IS CALLED AS A PROGRAM. Every call goes through pub_git(), which quotes its arguments and stops with git's
# own message on a non-zero exit, so a failing call names itself instead of returning an empty result.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed locals; .data$ for
# existing columns, bare CamelCase for new columns; if (FALSE) dev blocks; cli/fs/here; pure ASCII; stringi never
# base substr; {(.arg)} parens in cli interpolation.


# 1. Git ------------------------------------------------------------------------------------------------------------

#' Run git in a clone and return what it printed
#'
#' Arguments are shell-quoted, so a path with a space or a revision such as `HEAD...@{upstream}` reaches git
#' unchanged. `core.quotepath=off` makes git print file names as they are instead of escaping them.
#'
#' @param .dir Character. The clone.
#' @param .args Character vector. Arguments after `git -C <dir>`.
#' @param .input Character vector or NULL. Lines written to git's standard input.
#' @param .ok_fail Logical. TRUE returns NULL on a non-zero exit instead of stopping.
#' @return Character vector of the lines git printed, or NULL where .ok_fail applied.
pub_git <- function(.dir, .args, .input = NULL, .ok_fail = FALSE) {
  if (FALSE) {
    .dir     <- here::here()
    .args    <- c("rev-parse", "HEAD")
    .input   <- NULL
    .ok_fail <- FALSE
  }
  out_ <- suppressWarnings(system2(
    command = "git",
    args    = c("-C", shQuote(.dir), "-c", "core.quotepath=off", shQuote(.args)),
    stdout  = TRUE,
    stderr  = TRUE,
    input   = .input
  ))
  status_ <- attr(out_, "status")
  if (!is.null(status_) && status_ != 0L) {
    if (.ok_fail) return(NULL)
    cli::cli_abort(c(
      "git failed in {.path {(.dir)}}: git {paste(.args, collapse = ' ')}",
      "x" = "{paste(out_, collapse = ' | ')}"
    ))
  }
  as.character(out_)
}


# 2. Where each clone stands ------------------------------------------------------------------------------------------

#' The state of one clone: its commit, its uncommitted edits, and how it stands against GitHub
#'
#' THE PUBLISHED CODE IS THE COMMIT, NOT THE WORKING TREE. This function reports the difference between the two,
#' so an edit that is still uncommitted is seen here rather than missed later: it will not be in the published copy.
#'
#' FETCHING CHANGES NOTHING BUT THE REMOTE-TRACKING REFERENCES. It is what gives "behind" a meaning; a clone that
#' was never fetched reports itself level with GitHub whatever GitHub holds. A fetch that fails -- offline, or no
#' remote -- is reported in `Fetched` and does not stop the document.
#'
#' @param .name Character. How the clone is named in reports and in the published copy.
#' @param .dir Character. The clone.
#' @param .fetch Logical. TRUE runs `git fetch` first.
#' @return One-row tibble: Repo, Dir, Branch, Head, HeadShort, HeadDate, Subject, Version, nUncommitted,
#'   Uncommitted, Ahead, Behind, Fetched.
pub_repo_state <- function(.name, .dir, .fetch = TRUE) {
  if (FALSE) {
    .name  <- "material-contracts"
    .dir   <- here::here()
    .fetch <- TRUE
  }
  if (!fs::dir_exists(fs::path(.dir, ".git"))) {
    cli::cli_abort("{.path {(.dir)}} is not a git clone.")
  }
  fetched_ <- if (.fetch) {
    !is.null(pub_git(.dir = .dir, .args = c("fetch", "--quiet"), .input = NULL, .ok_fail = TRUE))
  } else {
    NA
  }
  head_ <- pub_git(.dir = .dir, .args = c("rev-parse", "HEAD"), .input = NULL, .ok_fail = FALSE)
  branch_ <- pub_git(.dir = .dir, .args = c("rev-parse", "--abbrev-ref", "HEAD"), .input = NULL, .ok_fail = FALSE)
  log_ <- pub_git(
    .dir     = .dir,
    .args    = c("log", "-1", "--date=short", "--format=%cd%x09%s"),
    .input   = NULL,
    .ok_fail = FALSE
  )
  dirty_ <- pub_git(
    .dir     = .dir,
    .args    = c("status", "--porcelain", "--untracked-files=no"),
    .input   = NULL,
    .ok_fail = FALSE
  )
  counts_ <- pub_git(
    .dir     = .dir,
    .args    = c("rev-list", "--left-right", "--count", "HEAD...@{upstream}"),
    .input   = NULL,
    .ok_fail = TRUE
  )
  counts_ <- if (is.null(counts_)) c(NA_integer_, NA_integer_) else as.integer(strsplit(counts_[1], "\\s+")[[1]])
  desc_ <- fs::path(.dir, "DESCRIPTION")
  version_ <- if (fs::file_exists(desc_)) read.dcf(file = desc_, fields = "Version")[1, 1] else NA_character_
  paths_dirty_ <- stringi::stri_sub(dirty_, from = 4L)

  tibble::tibble(
    Repo         = .name,
    Dir          = as.character(.dir),
    Branch       = branch_[1],
    Head         = head_[1],
    HeadShort    = stringi::stri_sub(head_[1], from = 1L, to = 7L),
    HeadDate     = sub("\t.*$", "", log_[1]),
    Subject      = sub("^[^\t]*\t", "", log_[1]),
    Version      = unname(version_),
    nUncommitted = length(dirty_),
    Uncommitted  = paste(paths_dirty_, collapse = ", "),
    Ahead        = counts_[1],
    Behind       = counts_[2],
    Fetched      = fetched_
  )
}

#' The state of every clone that is published from
#'
#' @param .dirs Named character. Clone directories, named as the repositories are to be called.
#' @param .fetch Logical. Passed to pub_repo_state().
#' @return Tibble, one row per clone, as pub_repo_state().
pub_repo_states <- function(.dirs, .fetch = TRUE) {
  if (FALSE) {
    .dirs  <- c(`material-contracts` = here::here())
    .fetch <- TRUE
  }
  purrr::imap(.dirs, \(.d, .n) pub_repo_state(.name = .n, .dir = .d, .fetch = .fetch)) |>
    purrr::list_rbind()
}

#' Every file in a clone's commit, with its size and mode in that commit
#'
#' `git ls-tree -l` reports the size of the committed blob, so the size check measures what is published rather
#' than a working copy that may differ. The mode carries the executable bit, which the copy restores.
#'
#' @param .name Character. The repository name, carried into the result.
#' @param .dir Character. The clone.
#' @return Tibble: Repo, Path, Size (bytes), Mode.
pub_repo_files <- function(.name, .dir) {
  if (FALSE) {
    .name <- "material-contracts"
    .dir  <- here::here()
  }
  lines_ <- pub_git(
    .dir     = .dir,
    .args    = c("ls-tree", "-r", "-l", "--full-tree", "HEAD"),
    .input   = NULL,
    .ok_fail = FALSE
  )
  # Each line reads "<mode> <type> <object> <size>TAB<path>"; the size is padded with spaces.
  meta_ <- strsplit(trimws(sub("\t.*$", "", lines_)), "\\s+")
  tibble::tibble(
    Repo = .name,
    Path = sub("^[^\t]*\t", "", lines_),
    Type = purrr::map_chr(meta_, \(.m) .m[2]),
    Size = suppressWarnings(as.numeric(purrr::map_chr(meta_, \(.m) .m[4]))),
    Mode = purrr::map_chr(meta_, \(.m) .m[1])
  ) |>
    dplyr::filter(.data$Type == "blob") |>
    dplyr::select("Repo", "Path", "Size", "Mode")
}


# 3. Which files are published ----------------------------------------------------------------------------------------

#' Decide each file by the first rule that matches it
#'
#' THE RULES ARE ORDERED AND THE FIRST MATCH DECIDES, so a narrow exception is written above the broad rule it
#' carves out of. A file that no rule matches keeps `Action = NA`; pub_require_decided() stops on it, because a new
#' file has to be decided about before it is either published or left out.
#'
#' @param .files Tibble with a Path column.
#' @param .rules Tibble: Pattern (Perl regex on the path), Action ("include" or "exclude"), Reason.
#' @return .files with Rule (the deciding row), Action and Reason added.
pub_classify <- function(.files, .rules) {
  if (FALSE) {
    .files <- tibble::tibble(Path = c("1_code/10-ExportData.R", "1_code/_Tests/scratch.R"))
    .rules <- tibble::tibble(Pattern = c("^1_code/_Tests/", "^1_code/"), Action = c("exclude", "include"),
                             Reason = c("scratch", "code"))
  }
  bad_ <- setdiff(unique(.rules$Action), c("include", "exclude"))
  if (length(bad_) > 0L) {
    cli::cli_abort("A rule has an unknown action: {.val {bad_}}. Use include or exclude.")
  }
  rule_ <- rep(NA_integer_, nrow(.files))
  # Assigned from the last rule to the first, so the first matching rule is the one left standing.
  for (i_ in rev(seq_len(nrow(.rules)))) {
    rule_[grepl(pattern = .rules$Pattern[i_], x = .files$Path, perl = TRUE)] <- i_
  }
  .files |>
    dplyr::mutate(
      Rule   = rule_,
      Action = .rules$Action[rule_],
      Reason = .rules$Reason[rule_]
    )
}

#' Stop if any file is undecided
#'
#' @param .tab Tibble from pub_classify().
#' @return Invisibly, .tab unchanged.
pub_require_decided <- function(.tab) {
  if (FALSE) {
    .tab <- tab_sel
  }
  open_ <- .tab |>
    dplyr::filter(is.na(.data$Action))
  if (nrow(open_) > 0L) {
    cli::cli_abort(c(
      "{nrow(open_)} file{?s} match{?es/} no rule and must be decided before anything is published:",
      purrr::set_names(paste(open_$Repo, open_$Path, sep = ": "), rep("x", nrow(open_))),
      "i" = "Add a rule for each to the rule table in the Rules section."
    ))
  }
  invisible(.tab)
}


# 4. The staged copy ----------------------------------------------------------------------------------------------------

#' Write the provenance note into a staged subtree
#'
#' The note carries commit hashes and nothing that changes between runs -- no timestamp -- so an unchanged source
#' produces an unchanged note and the target sees no change.
#'
#' @param .dir Character. The staged subtree.
#' @param .state Tibble from pub_repo_states(), the rows this subtree comes from.
#' @return Invisibly, the path written.
pub_write_provenance <- function(.dir, .state) {
  if (FALSE) {
    .dir   <- fs::path(.lP$Output$DirStage, "packages")
    .state <- tab_state
  }
  rows_ <- sprintf(
    "| %s | %s | `%s` | %s |",
    .state$Repo,
    ifelse(is.na(.state$Version), "-", .state$Version),
    .state$Head,
    .state$HeadDate
  )
  lines_ <- c(
    "# Where this copy comes from",
    "",
    "This directory is a published copy, written by `1_code/50A-PublishCode.qmd` of the pipeline repository.",
    "Every file in it is taken from the commit named below; nothing here is edited by hand.",
    "",
    "| Source | Version | Commit | Date |",
    "|:--|:--|:--|:--|",
    rows_,
    ""
  )
  path_ <- fs::path(.dir, "PUBLISHED_FROM.md")
  fs::dir_create(.dir)
  writeLines(text = lines_, con = path_, useBytes = TRUE)
  invisible(path_)
}

#' Copy every published file out of its commit into the stage
#'
#' THE COPY COMES FROM `git archive HEAD`, never from the working tree, so the published files are exactly the
#' commits reported in the state table: an uncommitted edit cannot slip in.
#'
#' THE STAGE IS REBUILT FROM NOTHING on every run. It holds a copy of commits, not a result; everything in it is
#' restored by the next run, so emptying it loses nothing and guarantees that a file dropped from the published set
#' is dropped from the stage too.
#'
#' The pipeline goes to `<stage>/<.sub_pipeline>/`, each package to `<stage>/<.sub_packages>/<package>/`, and each
#' of the two subtrees receives its provenance note.
#'
#' @param .tab Tibble from pub_classify(), all repositories.
#' @param .state Tibble from pub_repo_states().
#' @param .dir_stage Character. The stage; emptied first.
#' @param .name_pipeline Character. Which repository in .tab is the pipeline.
#' @param .sub_pipeline,.sub_packages Character. Subtree names in the stage and in the target.
#' @return Tibble of the published files: Repo, Path, Size, Mode, Published (path relative to the stage), Staged
#'   (absolute path).
pub_stage <- function(.tab, .state, .dir_stage, .name_pipeline, .sub_pipeline, .sub_packages) {
  if (FALSE) {
    .tab           <- tab_sel
    .state         <- tab_state
    .dir_stage     <- .lP$Output$DirStage
    .name_pipeline <- "material-contracts"
    .sub_pipeline  <- "pipeline"
    .sub_packages  <- "packages"
  }
  if (fs::dir_exists(.dir_stage)) fs::dir_delete(.dir_stage)
  fs::dir_create(.dir_stage)

  pub_ <- .tab |>
    dplyr::filter(.data$Action == "include") |>
    dplyr::mutate(
      Published = ifelse(
        .data$Repo == .name_pipeline,
        fs::path(.sub_pipeline, .data$Path),
        fs::path(.sub_packages, .data$Repo, .data$Path)
      ),
      Staged = as.character(fs::path(.dir_stage, .data$Published))
    )

  for (repo_ in unique(pub_$Repo)) {
    rows_ <- pub_ |>
      dplyr::filter(.data$Repo == repo_)
    dir_repo_ <- .state$Dir[.state$Repo == repo_]
    dir_out_ <- if (repo_ == .name_pipeline) {
      fs::path(.dir_stage, .sub_pipeline)
    } else {
      fs::path(.dir_stage, .sub_packages, repo_)
    }
    zip_ <- fs::file_temp(ext = "zip")
    pub_git(
      .dir     = dir_repo_,
      .args    = c("archive", "--format=zip", paste0("--output=", zip_), "HEAD"),
      .input   = NULL,
      .ok_fail = FALSE
    )
    fs::dir_create(dir_out_)
    utils::unzip(
      zipfile   = zip_,
      files     = rows_$Path,
      exdir     = dir_out_,
      junkpaths = FALSE
    )
    fs::file_delete(zip_)
    exec_ <- rows_$Staged[rows_$Mode == "100755"]
    if (length(exec_) > 0L) fs::file_chmod(path = exec_, mode = "u+x")
  }

  missing_ <- pub_$Staged[!fs::file_exists(pub_$Staged)]
  if (length(missing_) > 0L) {
    cli::cli_abort("{length(missing_)} published file{?s} did not arrive in the stage: {.file {missing_}}")
  }

  pub_write_provenance(
    .dir   = fs::path(.dir_stage, .sub_pipeline),
    .state = dplyr::filter(.state, .data$Repo == .name_pipeline)
  )
  if (any(.state$Repo != .name_pipeline)) {
    pub_write_provenance(
      .dir   = fs::path(.dir_stage, .sub_packages),
      .state = dplyr::filter(.state, .data$Repo != .name_pipeline)
    )
  }
  pub_ |>
    dplyr::select("Repo", "Path", "Size", "Mode", "Published", "Staged")
}


# 5. What must not be published -----------------------------------------------------------------------------------------

#' Whether a file is text: no NUL byte in its first 8 kB
#'
#' A content test rather than an extension list, so a DESCRIPTION, a Dockerfile or a lockfile is scanned without
#' being named, and a parquet file is never read as text.
#'
#' @param .paths Character. Files.
#' @return Logical, one per file.
pub_is_text <- function(.paths) {
  if (FALSE) {
    .paths <- c(fs::path(here::here(), "renv.lock"), fs::path(here::here(), ".here"))
  }
  purrr::map_lgl(.paths, \(.p) {
    raw_ <- readBin(con = .p, what = "raw", n = 8000L)
    !any(raw_ == as.raw(0L))
  })
}

#' The R code of a runbook: every line inside an R chunk
#'
#' THE CHUNKS ARE CUT OUT BY PATTERN, NOT BY KNITR. knitr keeps one registry of chunk labels per session, so calling
#' it from inside a render reports every runbook's `setup` chunk as a duplicate of this document's own. The pattern
#' reads a chunk as the lines between a fence opening with `{r` and the next closing fence, which is all a parse check
#' needs; chunk options are `#|` comments and parse as such.
#'
#' @param .path Character. The runbook.
#' @return Character vector of code lines.
pub_qmd_code <- function(.path) {
  if (FALSE) {
    .path <- fs::path(here::here(), "1_code", "50A-PublishCode.qmd")
  }
  lines_ <- readLines(con = .path, warn = FALSE, encoding = "UTF-8")
  open_ <- grepl("^\\s*```+\\s*\\{r([\\s,}].*)?$", lines_, perl = TRUE)
  fence_ <- grepl("^\\s*```+\\s*$", lines_, perl = TRUE)
  inside_ <- logical(length(lines_))
  state_ <- FALSE
  for (i_ in seq_along(lines_)) {
    if (!state_ && open_[i_]) {
      state_ <- TRUE
    } else if (state_ && fence_[i_]) {
      state_ <- FALSE
    } else if (state_) {
      inside_[i_] <- TRUE
    }
  }
  lines_[inside_]
}

#' Search the staged copy for what must not be published
#'
#' Six checks, each named for the failure it catches, and one note:
#' \describe{
#'   \item{HomePath}{an absolute path into a home directory -- machine-specific, and it names a person}
#'   \item{Secret}{a credential written into the code, or a token in a format that providers use}
#'   \item{DataFile}{a file inside a data directory, or a dataset format; licensed data never goes to GitHub}
#'   \item{Size}{a file GitHub refuses (violation) or warns about (warning)}
#'   \item{Parse}{an R file, or the R chunks of a runbook, that do not parse}
#'   \item{Reference}{a published file naming a file that is not published; a reader cannot follow it}
#'   \item{Dropbox}{lines mentioning Dropbox; mostly prose, listed as a note so each is seen once}
#' }
#' Tabular files that are published (parquet, csv, xlsx) are listed as notes with their size, so every one of
#' them is a conscious decision.
#'
#' @param .tab Tibble from pub_stage().
#' @param .excluded Character. File names left out of the copy, for the Reference check.
#' @param .refs_ok Character. Left-out file names a published file may name, each for a stated reason.
#' @param .warn_mb,.max_mb Numeric. The size thresholds in MB.
#' @return Tibble: Repo, Published, Check, Severity ("violation", "warning", "note"), nLines, Detail.
pub_checks <- function(.tab, .excluded, .refs_ok = character(0), .warn_mb = 50, .max_mb = 100) {
  if (FALSE) {
    .tab      <- tab_staged
    .excluded <- unique(fs::path_file(tab_sel$Path[tab_sel$Action == "exclude"]))
    .refs_ok  <- "geo_lookup_pre_hierarchy.parquet"
    .warn_mb  <- 50
    .max_mb   <- 100
  }
  re_home_ <- "(/Users/|/home/)[A-Za-z0-9._-]+/|[A-Za-z]:\\\\Users\\\\"
  re_secret_ <- paste(
    "(?i)\\b(password|passwd|pwd|secret|api[_-]?key|access[_-]?token)\\b\\s*[:=]\\s*[\"'][^\"'\\s]{6,}[\"']",
    "gh[pousr]_[A-Za-z0-9]{30,}",
    "hf_[A-Za-z0-9]{30,}",
    "AKIA[0-9A-Z]{16}",
    "sk-[A-Za-z0-9_-]{20,}",
    "-----BEGIN [A-Z ]*PRIVATE KEY-----",
    sep = "|"
  )
  re_data_dir_ <- "(^|/)(0_data|2_output|_BackUps?|_Migration)/"
  # Formats that licensed data arrive in. R's own formats are not among them: a package ships its lookup tables as
  # .rda under data/, and those are listed as notes with the other tabular files.
  re_data_ext_ <- "\\.(dta|sas7bdat|sav|sqlite|duckdb|db)$"
  re_table_ext_ <- "\\.(parquet|csv|tsv|xlsx?|feather|arrow|rds|rda|RData)$"
  # A left-out name is traced only where no published file carries the same name -- a package's own utils-pipe.R is
  # not a reference to another package's superseded one -- and where it is specific enough to mean that file.
  excluded_ <- setdiff(.excluded, c(fs::path_file(.tab$Path), .refs_ok))
  excluded_ <- excluded_[!startsWith(excluded_, ".") & nchar(excluded_) >= 6L]

  find_ <- function(.repo, .pub, .check, .sev, .n, .detail) {
    tibble::tibble(Repo = .repo, Published = .pub, Check = .check, Severity = .sev, nLines = .n, Detail = .detail)
  }
  text_ <- pub_is_text(.paths = .tab$Staged)
  out_ <- list()

  for (i_ in seq_len(nrow(.tab))) {
    row_ <- .tab[i_, ]
    pub_ <- row_$Published
    mb_ <- row_$Size / 1024^2

    if (grepl(re_data_dir_, row_$Path, perl = TRUE) || grepl(re_data_ext_, row_$Path, perl = TRUE)) {
      out_[[length(out_) + 1L]] <- find_(row_$Repo, pub_, "DataFile", "violation", NA_integer_,
                                         "a data directory or a dataset format")
    } else if (grepl(re_table_ext_, row_$Path, perl = TRUE, ignore.case = TRUE)) {
      out_[[length(out_) + 1L]] <- find_(row_$Repo, pub_, "DataFile", "note", NA_integer_,
                                         sprintf("tabular file, %.1f MB", mb_))
    }
    if (mb_ > .max_mb) {
      out_[[length(out_) + 1L]] <- find_(row_$Repo, pub_, "Size", "violation", NA_integer_,
                                         sprintf("%.1f MB, above the %s MB GitHub accepts", mb_, .max_mb))
    } else if (mb_ > .warn_mb) {
      out_[[length(out_) + 1L]] <- find_(row_$Repo, pub_, "Size", "warning", NA_integer_,
                                         sprintf("%.1f MB, above the %s MB GitHub warns at", mb_, .warn_mb))
    }
    if (!text_[i_]) next

    lines_ <- readLines(con = row_$Staged, warn = FALSE, encoding = "UTF-8")
    n_home_ <- sum(grepl(re_home_, lines_, perl = TRUE))
    if (n_home_ > 0L) {
      first_ <- lines_[grepl(re_home_, lines_, perl = TRUE)][1]
      out_[[length(out_) + 1L]] <- find_(row_$Repo, pub_, "HomePath", "violation", n_home_,
                                         stringi::stri_sub(trimws(first_), from = 1L, to = 90L))
    }
    n_secret_ <- sum(grepl(re_secret_, lines_, perl = TRUE))
    if (n_secret_ > 0L) {
      out_[[length(out_) + 1L]] <- find_(row_$Repo, pub_, "Secret", "violation", n_secret_,
                                         "credential-like assignment or token; inspect the file")
    }
    n_drop_ <- sum(grepl("dropbox", lines_, ignore.case = TRUE))
    if (n_drop_ > 0L) {
      out_[[length(out_) + 1L]] <- find_(row_$Repo, pub_, "Dropbox", "note", n_drop_, "mentions Dropbox")
    }
    if (length(excluded_) > 0L) {
      named_ <- excluded_[purrr::map_lgl(excluded_, \(.e) any(grepl(.e, lines_, fixed = TRUE)))]
      if (length(named_) > 0L) {
        out_[[length(out_) + 1L]] <- find_(row_$Repo, pub_, "Reference", "warning", length(named_),
                                           paste("names", paste(named_, collapse = ", ")))
      }
    }

    is_r_ <- grepl("\\.R$", row_$Path)
    is_qmd_ <- grepl("\\.qmd$", row_$Path)
    if (is_r_ || is_qmd_) {
      code_ <- if (is_qmd_) pub_qmd_code(.path = row_$Staged) else lines_
      msg_ <- tryCatch(
        expr = {
          parse(text = code_, keep.source = FALSE)
          NA_character_
        },
        error = function(.e) conditionMessage(.e)
      )
      if (!is.na(msg_)) {
        if (is_r_ && any(grepl("^---\\s*$", utils::head(lines_, 1L)))) {
          msg_ <- paste("starts with a YAML header: a runbook saved under the library's name;", msg_)
        }
        out_[[length(out_) + 1L]] <- find_(row_$Repo, pub_, "Parse", "violation", NA_integer_,
                                           stringi::stri_sub(gsub("\\s+", " ", msg_), from = 1L, to = 110L))
      }
    }
  }

  empty_ <- find_(character(0), character(0), character(0), character(0), integer(0), character(0))
  dplyr::bind_rows(empty_, out_) |>
    dplyr::arrange(
      factor(.data$Severity, levels = c("violation", "warning", "note")),
      .data$Check,
      .data$Published
    )
}

#' Stop if a violation was found and the run is strict
#'
#' @param .tab Tibble from pub_checks().
#' @param .strict Logical. TRUE stops on any violation; FALSE lets the copy proceed.
#' @return Invisibly, .tab unchanged.
pub_require_clean <- function(.tab, .strict) {
  if (FALSE) {
    .tab    <- tab_checks
    .strict <- FALSE
  }
  n_ <- sum(.tab$Severity == "violation")
  if (n_ > 0L && .strict) {
    cli::cli_abort(c(
      "{n_} violation{?s} found, and the run is strict: nothing was copied to the target.",
      "i" = "Fix them in the source repositories and commit, then render again."
    ))
  }
  if (n_ > 0L) {
    cli::cli_alert_warning(
      "{n_} violation{?s} found. The run is not strict, so the copy proceeds -- acceptable while the target is
       private, not once it is public."
    )
  }
  invisible(.tab)
}


# 6. Mirroring into the target ----------------------------------------------------------------------------------------

#' Make the owned subtrees of the target match the stage exactly
#'
#' ONLY THE OWNED SUBTREES ARE TOUCHED. The target repository keeps files of its own -- its README, its licences,
#' later the sample and the examples -- and this function neither reads nor writes anything outside the
#' directories it is given. Inside them it adds, replaces and deletes, so a file dropped from the published set
#' also disappears from the target.
#'
#' WHAT GIT IGNORES IN THE TARGET IS LEFT ALONE. A library restored by renv, or a folder a user's system drops in,
#' is local state of that clone and not part of the published copy; deleting it would be destruction without a
#' reason.
#'
#' AN UNCHANGED FILE IS NOT REWRITTEN, so git in the target sees only real changes and a second run reports nothing
#' to do.
#'
#' @param .dir_stage Character. The stage; its top-level directories are the subtrees.
#' @param .dir_target Character. The target repository.
#' @param .subtrees Character. The top-level directories this document owns in the target.
#' @param .apply Logical. FALSE returns the plan without changing the target.
#' @return Tibble: Subtree, Path (relative to the target), Action ("add", "update", "delete", "same").
pub_mirror <- function(.dir_stage, .dir_target, .subtrees, .apply = TRUE) {
  if (FALSE) {
    .dir_stage  <- .lP$Output$DirStage
    .dir_target <- .lP$Target$DirRepo
    .subtrees   <- c("pipeline", "packages")
    .apply      <- FALSE
  }
  if (!fs::dir_exists(fs::path(.dir_target, ".git"))) {
    cli::cli_abort("The target {.path {(.dir_target)}} is not a git clone.")
  }
  list_ <- function(.root) {
    if (!fs::dir_exists(.root)) return(character(0))
    as.character(fs::path_rel(fs::dir_ls(path = .root, recurse = TRUE, type = "file", all = TRUE), start = .root))
  }

  plan_ <- purrr::map(.subtrees, \(.s) {
    src_root_ <- fs::path(.dir_stage, .s)
    dst_root_ <- fs::path(.dir_target, .s)
    src_ <- list_(src_root_)
    dst_ <- list_(dst_root_)
    both_ <- intersect(src_, dst_)
    same_ <- if (length(both_) == 0L) {
      logical(0)
    } else {
      unname(tools::md5sum(fs::path(src_root_, both_)) == tools::md5sum(fs::path(dst_root_, both_)))
    }
    gone_ <- setdiff(dst_, src_)
    if (length(gone_) > 0L) {
      ignored_ <- pub_git(
        .dir     = .dir_target,
        .args    = c("check-ignore", "--stdin"),
        .input   = as.character(fs::path(.s, gone_)),
        .ok_fail = TRUE
      )
      gone_ <- gone_[!as.character(fs::path(.s, gone_)) %in% ignored_]
    }
    dplyr::bind_rows(
      tibble::tibble(Subtree = .s, Rel = setdiff(src_, dst_), Action = "add"),
      tibble::tibble(Subtree = .s, Rel = both_, Action = dplyr::if_else(same_, "same", "update")),
      tibble::tibble(Subtree = .s, Rel = gone_, Action = "delete")
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(Path = as.character(fs::path(.data$Subtree, .data$Rel)))

  if (.apply) {
    put_ <- plan_ |>
      dplyr::filter(.data$Action %in% c("add", "update"))
    if (nrow(put_) > 0L) {
      fs::dir_create(unique(fs::path_dir(fs::path(.dir_target, put_$Path))))
      fs::file_copy(
        path      = fs::path(.dir_stage, put_$Path),
        new_path  = fs::path(.dir_target, put_$Path),
        overwrite = TRUE
      )
    }
    del_ <- plan_ |>
      dplyr::filter(.data$Action == "delete")
    if (nrow(del_) > 0L) fs::file_delete(fs::path(.dir_target, del_$Path))
    # Directories left empty by the deletions go too, deepest first.
    for (s_ in .subtrees) {
      root_ <- fs::path(.dir_target, s_)
      if (!fs::dir_exists(root_)) next
      dirs_ <- fs::dir_ls(path = root_, recurse = TRUE, type = "directory", all = TRUE)
      dirs_ <- dirs_[order(-nchar(dirs_))]
      for (d_ in dirs_) {
        if (length(fs::dir_ls(path = d_, all = TRUE)) == 0L) fs::dir_delete(d_)
      }
    }
  }
  plan_ |>
    dplyr::select("Subtree", "Path", "Action") |>
    dplyr::arrange(factor(.data$Action, levels = c("add", "update", "delete", "same")), .data$Path)
}

#' Write the manifest of the published copy
#'
#' One row per published file: where it came from, the commit, and a checksum of the staged content. It stays in
#' this repository's output and records what a given render published.
#'
#' @param .tab Tibble from pub_stage().
#' @param .state Tibble from pub_repo_states().
#' @param .path Character. The csv to write.
#' @return Invisibly, the manifest.
pub_write_manifest <- function(.tab, .state, .path) {
  if (FALSE) {
    .tab   <- tab_staged
    .state <- tab_state
    .path  <- .lP$Output$FilManifest
  }
  out_ <- .tab |>
    dplyr::left_join(dplyr::select(.state, "Repo", Commit = "Head"), by = dplyr::join_by("Repo")) |>
    dplyr::mutate(Md5 = unname(tools::md5sum(.data$Staged))) |>
    dplyr::select("Published", "Repo", "Path", "Commit", "Size", "Md5") |>
    dplyr::arrange(.data$Published)
  fs::dir_create(fs::path_dir(.path))
  readr::write_csv(x = out_, file = .path, na = "")
  invisible(out_)
}


# 7. Reports ------------------------------------------------------------------------------------------------------------

#' Report where each clone stands
#'
#' @param .tab Tibble from pub_repo_states().
#' @return Invisibly, .tab.
pub_report_state <- function(.tab) {
  if (FALSE) {
    .tab <- tab_state
  }
  .tab |>
    dplyr::transmute(
      Repo         = .data$Repo,
      Branch       = .data$Branch,
      Commit       = .data$HeadShort,
      Date         = .data$HeadDate,
      Version      = .data$Version,
      Uncommitted  = .data$nUncommitted,
      Ahead        = .data$Ahead,
      Behind       = .data$Behind,
      Fetched      = dplyr::case_when(is.na(.data$Fetched) ~ "skipped", .data$Fetched ~ "yes", TRUE ~ "FAILED")
    ) |>
    tbl_say(.title = "Where each source stands", .n = NULL)

  for (i_ in seq_len(nrow(.tab))) {
    r_ <- .tab[i_, ]
    if (r_$nUncommitted > 0L) {
      cli::cli_alert_warning(
        "{r_$Repo}: {r_$nUncommitted} uncommitted change{?s}, NOT in the published copy: {r_$Uncommitted}"
      )
    }
    if (!is.na(r_$Behind) && r_$Behind > 0L) {
      cli::cli_alert_warning("{r_$Repo}: {r_$Behind} commit{?s} behind GitHub. Pull first, or an older state is published.")
    }
    if (!is.na(r_$Ahead) && r_$Ahead > 0L) {
      cli::cli_alert_warning(
        "{r_$Repo}: {r_$Ahead} local commit{?s} not on GitHub. Push, or the commit named in the copy cannot be found."
      )
    }
  }
  cli::cli_alert_info(
    "The copy is taken from the commit in each row. Anything listed above as uncommitted is missing from it."
  )
  invisible(.tab)
}

#' Report what the rules decided
#'
#' @param .tab Tibble from pub_classify(), all repositories.
#' @return Invisibly, .tab.
pub_report_selection <- function(.tab) {
  if (FALSE) {
    .tab <- tab_sel
  }
  .tab |>
    dplyr::summarise(
      Files     = dplyr::n(),
      Published = sum(.data$Action == "include", na.rm = TRUE),
      LeftOut   = sum(.data$Action == "exclude", na.rm = TRUE),
      Undecided = sum(is.na(.data$Action)),
      .by       = "Repo"
    ) |>
    tbl_say(.title = "Files per source", .n = NULL)

  .tab |>
    dplyr::filter(.data$Action == "include") |>
    dplyr::summarise(Files = dplyr::n(), .by = c("Repo", "Rule", "Reason")) |>
    dplyr::arrange(.data$Repo, .data$Rule) |>
    tbl_say(.title = "Published, by the rule that decided", .n = NULL)

  .tab |>
    dplyr::filter(.data$Action == "exclude") |>
    dplyr::select("Repo", "Path", "Reason") |>
    dplyr::arrange(.data$Repo, .data$Path) |>
    tbl_say(.title = "Left out, file by file", .n = NULL)

  cli::cli_alert_info("Every left-out file is listed with its reason. A file that should be public belongs in a rule above.")
  invisible(.tab)
}

#' Report what the checks found
#'
#' @param .tab Tibble from pub_checks().
#' @return Invisibly, .tab.
pub_report_checks <- function(.tab) {
  if (FALSE) {
    .tab <- tab_checks
  }
  .tab |>
    dplyr::summarise(Files = dplyr::n(), .by = c("Severity", "Check")) |>
    tbl_say(.title = "Findings", .n = NULL)

  .tab |>
    dplyr::filter(.data$Severity != "note") |>
    dplyr::select("Severity", "Check", "Published", "nLines", "Detail") |>
    tbl_say(.title = "Violations and warnings", .n = NULL)

  .tab |>
    dplyr::filter(.data$Severity == "note") |>
    dplyr::select("Check", "Published", "nLines", "Detail") |>
    tbl_say(.title = "Notes", .n = NULL)

  cli::cli_alert_info(
    "A HomePath violation is a path to fix in the source, usually by building it from here::here(). A Secret is
     never acceptable. A Reference warning means a published file points a reader at one that is not published."
  )
  invisible(.tab)
}

#' Report what the mirror did, or would do
#'
#' @param .tab Tibble from pub_mirror().
#' @param .applied Logical. Whether the plan was applied.
#' @return Invisibly, .tab.
pub_report_mirror <- function(.tab, .applied) {
  if (FALSE) {
    .tab     <- tab_mirror
    .applied <- TRUE
  }
  .tab |>
    dplyr::count(.data$Subtree, .data$Action, name = "Files") |>
    tbl_say(.title = if (.applied) "Changes made in the target" else "Changes the target WOULD receive", .n = NULL)

  .tab |>
    dplyr::filter(.data$Action != "same") |>
    tbl_say(.title = "Files changed", .n = 60L)

  n_ <- sum(.tab$Action != "same")
  if (n_ == 0L) {
    cli::cli_alert_success("The target already matches the published copy; nothing to commit from this run.")
  } else if (!.applied) {
    cli::cli_alert_info(
      "Apply = FALSE: nothing was changed. Set it to TRUE and render again to write these {n_} change{?s}."
    )
  }
  invisible(.tab)
}

#' Report what git in the target now sees, inside the owned subtrees
#'
#' Read-only. This is the change that a commit in the target repository would publish.
#'
#' @param .dir_target Character. The target repository.
#' @param .subtrees Character. The owned subtrees.
#' @return Invisibly, a tibble: Status, Path.
pub_report_target <- function(.dir_target, .subtrees) {
  if (FALSE) {
    .dir_target <- .lP$Target$DirRepo
    .subtrees   <- c("pipeline", "packages")
  }
  lines_ <- pub_git(
    .dir     = .dir_target,
    .args    = c("status", "--porcelain", "--untracked-files=all", "--", .subtrees),
    .input   = NULL,
    .ok_fail = FALSE
  )
  tab_ <- tibble::tibble(
    Status = trimws(stringi::stri_sub(lines_, from = 1L, to = 2L)),
    Path   = stringi::stri_sub(lines_, from = 4L)
  )
  tab_ |>
    dplyr::mutate(
      Status = dplyr::case_when(
        .data$Status == "??" ~ "new",
        .data$Status == "M"  ~ "modified",
        .data$Status == "D"  ~ "deleted",
        TRUE                 ~ .data$Status
      )
    ) |>
    dplyr::count(.data$Status, name = "Files") |>
    tbl_say(.title = "What git sees in the target", .n = NULL)
  if (nrow(tab_) > 0L) {
    cli::cli_alert_info(
      "To publish: open the target project in RStudio, then Git pane > Commit (tick the files) > Push."
    )
  }
  invisible(tab_)
}

#' The whole run in one table
#'
#' @param .state,.sel,.checks,.mirror Tibbles from the steps above.
#' @return Invisibly, the summary tibble.
pub_report_overview <- function(.state, .sel, .checks, .mirror) {
  if (FALSE) {
    .state  <- tab_state
    .sel    <- tab_sel
    .checks <- tab_checks
    .mirror <- tab_mirror
  }
  out_ <- .state |>
    dplyr::select("Repo", Commit = "HeadShort", "Uncommitted" = "nUncommitted", "Behind") |>
    dplyr::left_join(
      .sel |>
        dplyr::summarise(
          Published = sum(.data$Action == "include", na.rm = TRUE),
          LeftOut   = sum(.data$Action == "exclude", na.rm = TRUE),
          .by       = "Repo"
        ),
      by = dplyr::join_by("Repo")
    ) |>
    dplyr::left_join(
      .checks |>
        dplyr::summarise(
          Violations = sum(.data$Severity == "violation"),
          Warnings   = sum(.data$Severity == "warning"),
          .by        = "Repo"
        ),
      by = dplyr::join_by("Repo")
    ) |>
    dplyr::mutate(
      Violations = dplyr::coalesce(.data$Violations, 0L),
      Warnings   = dplyr::coalesce(.data$Warnings, 0L)
    )
  tbl_say(.tab = out_, .title = "Overview", .n = NULL)
  cli::cli_alert_info(
    "Target: {sum(.mirror$Action == 'add')} added, {sum(.mirror$Action == 'update')} updated,
     {sum(.mirror$Action == 'delete')} deleted, {sum(.mirror$Action == 'same')} unchanged."
  )
  invisible(out_)
}
