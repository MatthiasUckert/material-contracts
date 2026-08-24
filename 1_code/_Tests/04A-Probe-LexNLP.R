# ======================================================================================================================
# 04A-Probe-LexNLP.R -- why does the LexNLP family not run?
# ======================================================================================================================
#
# READ-ONLY. Writes nothing, touches no store, runs at most one container over one throwaway
# document. Safe to source at any point in a session.
#
# WHY A SEPARATE SCRIPT. ner_describe() answers one question -- is this family usable -- and answers
# it with one word. That is right for the document, which should not carry six lines of environment
# archaeology in its output. It is wrong for the ten minutes when the answer is "no" and the reason
# is not obvious, which is what this file is for.
#
# THE POINT IS R'S ENVIRONMENT, NOT THE SHELL'S. Docker working in a terminal says nothing about
# whether it works here: an R session started from the Finder inherits a narrower PATH than a login
# shell, and may resolve a different docker context. Every check below therefore runs through the
# same system2() calls _NER.R uses, not through a terminal.
#
# Usage:
#   source(here::here("1_code", "_Tests", "04A-Probe-LexNLP.R"), encoding = "UTF-8")

here::i_am("1_code/_Commons/_Initialize.R")

source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),     encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_NER.R"),        encoding = "UTF-8")

options(cli.num_colors = 1, cli.width = 120, mc.table_mode = "console")

.image <- "contracts-lexnlp"


# 1. What R can see ------------------------------------------------------------------------------------------------------

cli::cli_h1("1. The environment this R session has")

bin_ <- Sys.which("docker")
cli::cli_alert_info("docker on PATH: {if (bin_ == '') 'NOT FOUND' else as.character(bin_)}")

# PREDICTION: the binary is found. If it is not, nothing below matters and the fix is PATH, not
# Docker -- start R from a login shell, or add the path in ~/.Renviron.
if (bin_ == "") {
  cli::cli_alert_danger("Stop here. R cannot see docker at all.")
  cli::cli_alert_info("PATH as R sees it:")
  cli::cli_verbatim(paste(" ", strsplit(Sys.getenv("PATH"), ":")[[1L]]))
} else {
  cli::cli_alert_info("PATH entries: {length(strsplit(Sys.getenv('PATH'), ':')[[1L]])}")
}

for (v_ in c("DOCKER_HOST", "DOCKER_CONTEXT", "DOCKER_CONFIG")) {
  val_ <- Sys.getenv(v_)
  cli::cli_alert_info("{(v_)}: {if (val_ == '') '(unset)' else val_}")
}


# 2. Is the daemon answering? --------------------------------------------------------------------------------------------

cli::cli_h1("2. The daemon")

# CAPTURED, NOT DISCARDED. The exit status alone cannot distinguish "daemon down" from "wrong
# context" from "permission denied", and those have three different fixes.
info_ <- suppressWarnings(system2("docker", "info", stdout = TRUE, stderr = TRUE))
stat_ <- attr(info_, "status")
stat_ <- if (is.null(stat_)) 0L else as.integer(stat_)

cli::cli_alert_info("docker info exit status: {(stat_)}")
if (stat_ != 0L) {
  cli::cli_alert_danger("The daemon did not answer. What it said:")
  cli::cli_verbatim(paste(" ", utils::head(info_, 10L)))
} else {
  srv_ <- info_[stringi::stri_detect_regex(info_, "Server Version|Operating System|Total Memory")]
  cli::cli_alert_success("The daemon answered.")
  if (length(srv_) > 0L) cli::cli_verbatim(paste(" ", trimws(srv_)))
}

ctx_ <- suppressWarnings(system2("docker", c("context", "ls"), stdout = TRUE, stderr = TRUE))
if (is.null(attr(ctx_, "status"))) {
  cli::cli_alert_info("Contexts R resolves (the starred one is in use):")
  cli::cli_verbatim(paste(" ", ctx_))
}


# 3. Is the image there, and what does it carry? ---------------------------------------------------------------------------

cli::cli_h1("3. The image")

ls_ <- suppressWarnings(system2(
  "docker", c("image", "ls", "-q", .image), stdout = TRUE, stderr = TRUE
))
if (is.null(attr(ls_, "status")) && length(ls_) > 0L && any(nzchar(trimws(ls_)))) {
  cli::cli_alert_success("Image {(.image)} is present: {paste(trimws(ls_), collapse = ', ')}")
} else {
  cli::cli_alert_danger("No image named {(.image)}. Build it: contracts-lexnlp/rebuild_lexnlp.sh")
}

# THE RAW ANSWER, NOT A VERDICT. Docker's inspect output has changed shape across image stores, so
# what a template does or does not resolve is a fact about this installation rather than something
# to assume. Printed verbatim, because a template error and an absent label look identical once
# either has been reduced to NA.
fmt_ <- "{{ with .Config.Labels }}{{ index . \"spec_hash\" }}{{ end }}"
lab_ <- suppressWarnings(system2(
  "docker", c("image", "inspect", .image, "--format", shQuote(fmt_)), stdout = TRUE, stderr = TRUE
))
cli::cli_alert_info("inspect exit status: {if (is.null(attr(lab_, 'status'))) 0L else attr(lab_, 'status')}")
if (length(lab_) > 0L && any(nzchar(trimws(lab_)))) {
  cli::cli_alert_info("inspect said:")
  cli::cli_verbatim(paste(" ", lab_))
}
have_ <- if (is.null(attr(lab_, "status"))) trimws(paste(lab_, collapse = "")) else NA_character_
if (!is.na(have_) && !nzchar(have_)) have_ <- NA_character_

script_ <- fs::path(.ner_family_dir("lexnlp"), "image_spec.py")
want_   <- if (fs::file_exists(script_)) {
  raw_ <- suppressWarnings(system2("python3", script_, stdout = TRUE, stderr = FALSE))
  if (is.null(attr(raw_, "status")) && length(raw_) > 0L) trimws(paste(raw_, collapse = "")) else NA
} else {
  NA_character_
}

tbl_say(
  .tab = tibble::tibble(
    What  = c("spec_hash label inside the image", "hash of the sources on disk"),
    Value = c(if (is.na(have_) || have_ == "") "(none)" else have_,
              if (is.na(want_)) "(could not compute)" else want_)
  ),
  .title = "Provenance"
)

# THE COMPARISON IS THE WHOLE POINT. Equal means the container holds the code in the repository.
# Different means it does not, and every span it produces would be attributed in the store to code
# that has changed since.
if (!is.na(have_) && !is.na(want_) && nzchar(have_)) {
  if (identical(have_, want_)) {
    cli::cli_alert_success("The image matches its sources.")
  } else {
    cli::cli_alert_warning("STALE. Run contracts-lexnlp/rebuild_lexnlp.sh before trusting output.")
  }
}


# 4. What _NER.R concludes -------------------------------------------------------------------------------------------------

cli::cli_h1("4. The verdict the pipeline uses")

img_ <- ner_lexnlp_image(.image = .image)
tbl_say(.tab = img_, .title = "ner_lexnlp_image()")

cli::cli_alert_info(
  "State is what ner_describe() turns into Ready, and Ready is what the benchmark and the \\
   extraction chunks skip on."
)

# THE STALE-DESCRIPTION TRAP, which is the likeliest cause of a family that looks unavailable while
# Docker is plainly running. ner_describe() is called ONCE near the top of the document and every
# later chunk reads that object. Starting Docker afterwards changes the environment and not the
# table, so the render keeps skipping on a verdict that was true when it was taken.
if (exists("tab_describe", envir = globalenv())) {
  was_ <- get("tab_describe", envir = globalenv())
  old_ <- unique(was_$Ready[was_$Family == "lexnlp"])
  new_ <- img_$State[[1L]] %in% c("ok", "stale", "unverified")
  if (!identical(old_, new_)) {
    cli::cli_alert_warning(c(
      "tab_describe in your session says Ready = {(old_)} and the environment now says {(new_)}."
    ))
    cli::cli_alert_info("Re-run the describe-families chunk; the benchmark reads that object.")
  } else {
    cli::cli_alert_success("tab_describe agrees with the environment as it stands now.")
  }
} else {
  cli::cli_alert_info("No tab_describe in this session, so nothing can be stale.")
}


# 5. One container, one document ---------------------------------------------------------------------------------------

cli::cli_h1("5. A real run, on one throwaway document")

# EVERYTHING ABOVE IS INSPECTION. Only this establishes that a container can actually mount, read
# and write -- which is a different question from whether the daemon answers, and is where a bind
# mount refused by macOS file-sharing settings shows up.
if (img_$State[[1L]] %in% c("nodocker", "nodaemon", "noimage")) {
  cli::cli_alert_danger("Skipped: {(img_$Said[[1L]])}")
} else {
  dir_ <- fs::path(tempdir(), "probe-lexnlp")
  fs::dir_create(dir_)
  on.exit(if (fs::dir_exists(dir_)) fs::dir_delete(dir_), add = TRUE)

  in_ <- fs::path(dir_, "one.parquet")
  arrow::write_parquet(tibble::tibble(
    DocID   = "probe-0001",
    TextRaw = paste(
      "This Agreement is made as of January 15, 2019 by and between Acme Holdings, Inc.,",
      "a Delaware corporation, and Beta Systems LLC of Frankfurt, Germany, for USD 4,500,000."
    )
  ), in_)

  got_ <- try(ner_extract(
    .family     = "lexnlp",
    .model      = NULL,
    .entity     = c("ORG", "GPE", "DATE", "MONEY"),
    .path_in    = in_,
    .out_dir    = dir_,
    .describe   = ner_describe(.spacy_models = character(), .lexnlp_image = .image),
    .workers    = 1L,
    .batch_size = 1L,
    .timeout    = 120L,
    .quiet      = TRUE
  ), silent = TRUE)

  if (inherits(got_, "try-error")) {
    cli::cli_alert_danger("The container did not complete:")
    cli::cli_verbatim(paste(" ", as.character(got_)))
  } else {
    out_ <- arrow::read_parquet(got_$Path[[1L]]) |> tibble::as_tibble()
    cli::cli_alert_success(
      "The container ran in {round(got_$Seconds[[1L]], 1)}s and returned {nrow(out_)} \\
       {cli::qty(nrow(out_))}row{?s}."
    )
    tbl_say(
      .tab   = dplyr::select(out_, dplyr::any_of(c("DocID", "Label", "Start", "Stop", "Span"))),
      .title = "What it found",
      .n     = 15L
    )
  }
}

cli::cli_h1("Done")
cli::cli_alert_info(
  "If section 5 succeeded and the benchmark still skips, the cause is section 4: re-run the \\
   describe-families chunk so tab_describe reflects the environment as it is now."
)
