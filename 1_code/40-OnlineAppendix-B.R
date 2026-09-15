# ======================================================================================================================
# 40-OnlineAppendix-B.R -- Appendix B, the software: the exhibits this chapter builds
# ======================================================================================================================
#
# 40-OnlineAppendix-B.qmd holds the chapter's text; _Commons/_OnlineAppendix.R holds everything the chapters share.
# This library holds the two things Appendix B writes itself: the pipeline table, whose content is text rather than
# data and is kept here as a tibble so that it deploys through the same machinery as every other exhibit, and the
# versions of the two packages, read from the installed packages so that the text cannot name a version that is not
# the one the database was built with.
#
# THE PREFIX IS oab_: online appendix, chapter B.


# 1. The pipeline, stage by stage ---------------------------------------------------------------------------------------

#' The pipeline table's content: six stages, what each does, what it produces, where it is described
#'
#' Written as a tibble rather than as prose in the text so that the table is an exhibit like any other -- built,
#' numbered, deployed and read through \\oaown -- and so that its wording is in one place. It names no script: the
#' stages are the ones a reader needs to place the chapters, and the code's own numbering is not part of the paper.
#'
#' @return Tibble: Stage, Does, Produces, Where.
oab_data_stages <- function() {
  tibble::tribble(
    ~Stage, ~Does, ~Produces, ~Where,
    "Acquisition",
    paste("Reads EDGAR's index, visits every selected filing, downloads its documents and converts them",
          "to text"),
    "Every Exhibit 10, Item 1.01 current report and confidential treatment order, as text",
    "A.1; rGetEDGAR (B.2)",
    "Database",
    paste("Identifies unique attachments, flags text that failed conversion, links orders and",
          "announcements to contracts"),
    "One row per contract, with its filing, its copies and its links",
    "A.4",
    "Labelling",
    "Two annotators assign each contract of a stratified sample a category and an amendment flag",
    "The labelled sample",
    "C.1; rLabelDocs (B.3)",
    "Classification",
    paste("Fine-tunes a transformer and derives a keyword table on the labelled sample, out of fold, and",
          "applies both to the corpus"),
    "A category, its probability and a keyword label per contract",
    "C; the classifier (B.4)",
    "Entity extraction",
    paste("Finds partners, places, dates, amounts and redaction markers as spans, and turns spans into",
          "variables by rule"),
    "The content variables per contract",
    "D; the extractors (B.4)",
    "Release",
    "Exports the contract rows, the linked orders and announcements, and the codebook",
    "The published files",
    "F"
  )
}

#' Build the pipeline table
#'
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library, the table's only input.
#' @return Invisibly, the build's status.
oab_build_stages <- function(.dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-B", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-B.R")
  }
  name_ <- "PipelineStages"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = .path_lib, .force = .force)) return(invisible("up to date"))
  tab_ <- oab_data_stages()
  cells_ <- tab_ |>
    dplyr::transmute(
      Stage    = paste0("\\textbf{", oa_tex_escape(.x = .data$Stage), "}"),
      Does     = oa_tex_escape(.x = .data$Does),
      Produces = oa_tex_escape(.x = .data$Produces),
      Where    = oa_tex_escape(.x = .data$Where)
    )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Stage", "What it does", "What it produces", "Described in"),
      .spec   = c(oa_col_text(.share = 0.14), oa_col_text(.share = 0.40), oa_col_text(.share = 0.28),
                  oa_col_text(.share = 0.14))
    ),
    .note    = paste(
      "The six stages of the pipeline, in the order they run. Each stage reads what the previous one produced and",
      "writes its own result, so a stage can be rerun without repeating the ones before it. The two R packages and",
      "the classification and extraction code described in this appendix are the software behind the stages named",
      "beside them; the remaining stages are scripts of the pipeline itself."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# 2. The package versions, read from the packages -----------------------------------------------------------------------

#' The versions of the two packages, as installed
#'
#' The text names the versions the database was built with. Read from the installed packages rather than typed, so
#' the sentence follows an upgrade; a package that is not installed resolves to NA and the number reports it.
#'
#' @return Tibble: Key, Value -- rGetEDGAR and rLabelDocs, as "0.1.0".
oab_data_versions <- function() {
  ver_ <- function(.pkg) {
    if (!requireNamespace(.pkg, quietly = TRUE)) return(NA_character_)
    as.character(utils::packageVersion(.pkg))
  }
  tibble::tibble(
    Key   = c("rGetEDGAR", "rLabelDocs"),
    Value = c(ver_(.pkg = "rGetEDGAR"), ver_(.pkg = "rLabelDocs"))
  )
}

#' Build the versions tibble
#'
#' Rebuilt on every render, since an installed package can change without any file of the pipeline changing.
#'
#' @param .dir_own Character. This chapter's output directory.
#' @return Invisibly, the build's status.
oab_build_versions <- function(.dir_own) {
  if (FALSE) .dir_own <- here::here("2_output", "40-OnlineAppendix-B", "Output")
  fs::dir_create(fs::path(.dir_own, "Data"))
  arrow::write_parquet(
    x    = oab_data_versions(),
    sink = fs::path(.dir_own, "Data", "PackageVersions.parquet")
  )
  invisible("built")
}
