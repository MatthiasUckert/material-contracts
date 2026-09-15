# ======================================================================================================================
# 40-OnlineAppendix-C.R -- Appendix C, the classification approach: the exhibits this chapter builds
# ======================================================================================================================
#
# 40-OnlineAppendix-C.qmd holds the chapter's text; _Commons/_OnlineAppendix.R holds everything the chapters share.
# Every table of the chapter is written here, in the appendix's own cut: most from the data tibbles 30 writes beside
# its own tables (the labelled sample, the sweep, the scores by category, the confusion matrix, the amendment flag,
# the keyword table, the arms and the ceiling), with fewer columns, no fold uncertainty and numbered categories;
# two from other sources -- the descriptions filers give their contracts, from the release, and the second-label
# table, from 03B's out-of-fold classification. 30 stays the manuscript's writer; nothing of 30 is changed or rerun.
#
# THE CATEGORY NUMBERS follow the order of the paper's categories table, which is the order 30 prints, so an error
# that stays inside a parent sits next to the diagonal of the confusion matrix. One vector, .oac_categories, holds
# it; every table reads its numbers from there.
#
# THE PREFIX IS oac_: online appendix, chapter C.


# 1. The sample, as 30 reads it ---------------------------------------------------------------------------------------

#' The unique contracts of the descriptive sample, as 30 selects them
#'
#' The one reader of the release every build here shares, so all of them count exactly the paper's sample: a
#' contract is kept where DescSample is 1 and it is the primary copy of its attachment -- 30's Keep. The flags are
#' compared after the collect, where an integer and a logical flag compare alike; in Arrow they need not.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .cols Character. The columns to keep besides the flags.
#' @return Tibble: .cols, plus Date and Year, for 2001-2024.
oac_read_sample <- function(.path_contracts, .cols) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .cols <- c("FormType", "DateFiled")
  }
  arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::all_of(unique(c(.cols, "DateFiled", "PrimaryFiler", "DescSample")))) |>
    dplyr::collect() |>
    dplyr::filter(
      dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      dplyr::coalesce(as.integer(.data$DescSample), 0L) == 1L
    ) |>
    dplyr::mutate(
      Date = as.Date(.data$DateFiled),
      Year = as.integer(format(.data$Date, "%Y"))
    ) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::select(-"PrimaryFiler", -"DescSample")
}

# 2. What filers call their contracts, per category --------------------------------------------------------------------

#' The most frequent descriptions filers give their contracts, per category
#'
#' The filer's own description is the only human-written label an attachment carries, so it shows what a category
#' holds in the words of the people who file. Free text: filers write what they like, and many write nothing beyond
#' the exhibit number. Three rules make the descriptions comparable. A leading exhibit number is stripped, so that
#' "10.1 Credit Agreement" counts with "Credit Agreement". A description that carries no words beyond an exhibit
#' number or a form name is dropped as uninformative, and the share it accounts for is reported per category.
#' Descriptions are grouped case- and punctuation-insensitively, and the group is printed in its most frequent
#' original spelling.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .n Integer. Descriptions per category.
#' @return Tibble: Class, Kind, Rank, Description, N, Share (of the category's contracts that carry a description),
#'   plus nClass, nNamed and pNamed per category.
oac_data_titles <- function(.path_contracts, .n) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .n <- 10L
  }
  # A LEADING EXHIBIT NUMBER, in the spellings filers use: EX-10.1, EXHIBIT 10.23, 10.1, (10.1), 10.1 -
  # It must carry a decimal point, or the word EX or EXHIBIT: a bare number is part of the title -- a year in
  # "2020 Equity Incentive Plan", a count in "3 Year Supply Agreement" -- and stripping it would corrupt the text.
  .re_number <- paste0(
    "(?i)^[\\s(\\[]*(",
    "ex(hibit)?[\\s.-]*\\d{1,3}([.(][a-z0-9]+\\)?)*",   # EX-10.1, EXHIBIT 10, EX 10.23(a), Ex. 10(a)
    "|\\d{1,3}([.(][a-z0-9]+\\)?)+",                    # 10.1, 10(a), 10.23a
    ")[\\s)\\]:.,-]*"
  )
  # NOTHING BUT A WORD FOR THE EXHIBIT ITSELF, or a form name: no description at all
  .re_unnamed <- paste0(
    "(?i)^(ex|exhibit|exhibits|document|attachment|annex|appendix|material contract[s]?|agreement|contract|",
    "8-k|10-k|10-q|20-f|s-1|s-4|f-1|f-4)$"
  )
  con_ <- oac_read_sample(
    .path_contracts = .path_contracts,
    .cols           = c("Class", "DocDesc")
  ) |>
    dplyr::filter(!is.na(.data$Class))
  if (nrow(con_) == 0L) cli::cli_abort("No contract carries a {.field Class}; the release's labels differ.")
  clean_ <- con_ |>
    dplyr::mutate(
      Desc = trimws(dplyr::coalesce(.data$DocDesc, "")),
      Desc = stringi::stri_replace_first_regex(.data$Desc, .re_number, ""),
      Desc = trimws(stringi::stri_replace_all_regex(.data$Desc, "\\s+", " ")),
      # UNINFORMATIVE: nothing left, or nothing but a form name or a word for the exhibit itself
      Named = nzchar(.data$Desc) & !stringi::stri_detect_regex(.data$Desc, .re_unnamed),
      Key = toupper(stringi::stri_replace_all_regex(.data$Desc, "[^[:alnum:] ]", " ")),
      Key = trimws(stringi::stri_replace_all_regex(.data$Key, "\\s+", " "))
    )
  per_class_ <- clean_ |>
    dplyr::summarise(nClass = dplyr::n(), pNamed = mean(.data$Named), .by = "Class")
  # A READABLE SPELLING. Filers most often write in capitals, and a table of capitals is hard to read and says
  # nothing the lower-case spelling does not. The most frequent spelling that is not all capitals is printed where
  # there is one; otherwise the capitals are set in title case, with the short words that belong inside a title
  # left lower-case.
  pretty_ <- function(.x) {
    small_ <- c("a", "an", "and", "as", "at", "by", "for", "from", "in", "of", "on", "or", "the", "to", "with")
    words_ <- strsplit(tolower(.x), " ", fixed = TRUE)[[1L]]
    up_    <- paste0(toupper(substring(words_, 1L, 1L)), substring(words_, 2L))
    out_   <- ifelse(words_ %in% small_ & seq_along(words_) > 1L, words_, up_)
    paste(out_, collapse = " ")
  }
  n_named_ <- clean_ |>
    dplyr::filter(.data$Named) |>
    dplyr::summarise(nNamed = dplyr::n(), .by = "Class")
  top_ <- clean_ |>
    dplyr::filter(.data$Named) |>
    dplyr::count(.data$Class, .data$Key, .data$Desc, name = "nSpelling") |>
    dplyr::arrange(dplyr::desc(.data$nSpelling)) |>
    dplyr::summarise(
      Description = {
        mixed_ <- .data$Desc[.data$Desc != toupper(.data$Desc)]
        if (length(mixed_) > 0L) mixed_[[1L]] else pretty_(.x = .data$Desc[[1L]])
      },
      N           = sum(.data$nSpelling),
      .by         = c("Class", "Key")
    ) |>
    dplyr::arrange(.data$Class, dplyr::desc(.data$N)) |>
    dplyr::slice_head(n = .n, by = "Class") |>
    dplyr::mutate(Rank = dplyr::row_number(), .by = "Class") |>
    dplyr::left_join(per_class_, by = "Class") |>
    dplyr::left_join(n_named_, by = "Class") |>
    dplyr::mutate(Share = .data$N / .data$nNamed, Kind = "title") |>
    dplyr::select("Class", "Kind", "Rank", "Description", "N", "Share", "nClass", "nNamed", "pNamed")
  dplyr::arrange(top_, .data$Class, .data$Rank)
}

#' Build the table of filer descriptions by category, one row per category
#'
#' The reviewer asked for example titles; a list of ten per category runs to a page and a half, and the same content
#' fits twelve rows: the category, its most frequent descriptions on one line with their counts, and the share of
#' its contracts that carry a description at all.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .n Integer. Descriptions per category.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oac_build_titles <- function(.path_contracts, .n, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .n        <- 5L
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-C", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-C.R")
  }
  name_ <- "CategoryTitles"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(.path_contracts, .path_lib)
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oac_data_titles(
    .path_contracts = .path_contracts,
    .n              = .n
  )
  num_ <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)
  rows_ <- tab_ |>
    dplyr::arrange(.data$Class, .data$Rank) |>
    dplyr::summarise(
      Descriptions = paste0(oa_tex_escape(.x = .data$Description), " (", num_(.x = .data$N), ")", collapse = "; "),
      pNamed       = dplyr::first(.data$pNamed),
      .by          = "Class"
    ) |>
    dplyr::mutate(Order = oac_number(.name = .data$Class)) |>
    dplyr::arrange(.data$Order)
  cells_ <- tibble::tibble(
    Category     = oac_label(.class = rows_$Class),
    Descriptions = rows_$Descriptions,
    Named        = formatC(100 * rows_$pNamed, format = "f", digits = 0L)
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Category", paste("The", .n, "most frequent descriptions (contracts)"), "\\% described"),
      .spec   = c(oa_col_text(.share = 0.22), oa_col_text(.share = 0.62), oa_col_num(.mm = 18))
    ),
    .note    = paste(
      "The", .n, "most frequent descriptions filers give their contracts, by category, over the unique contracts of",
      "the descriptive sample; the category is the classifier's label. A description is the filer's own free text,",
      "the only human-written label an attachment carries. A leading exhibit number is stripped before counting, and",
      "a description that says nothing beyond an exhibit or form name is treated as no description; the last column",
      "is the share of a category's contracts that carry one. Descriptions are grouped without regard to case and",
      "punctuation and printed in the most frequent spelling that is not in capitals."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# 3. The chapter's cut of 30's classification tables -------------------------------------------------------------------
# 30 writes the manuscript's version of each table and, beside it, the data tibble it printed. The appendix prints
# the same numbers with fewer columns, no fold uncertainty and numbered categories. Each builder reads 30's tibble,
# writes the appendix's table under this chapter's directory, and keeps the tibble it read as its own data, so the
# numbers in the text resolve from the same file whichever copy is found first.

# THE CATEGORIES, in the order of the paper's categories table. Class is the label 30's tibbles carry.
.oac_categories <- tibble::tibble(
  Short  = c("Credit", "Equity", "Compensation", "Legal", "Assets", "R&D", "Customer / Supplier", "Licenses",
             "Leases", "Peer Agreements", "M&A", "Other"),
  Number = 1:12
)

#' A category's number, from any of the names it goes by
#'
#' 30's tibbles carry the short name in Row and a qualified name in Class ("Employment: Compensation"); the release
#' carries the qualified name, and M&A's qualified name is "Investment and Merger". The number is looked up on the
#' short name, taken as the part after the parent where there is one, with that alias.
#'
#' @param .name Character. Short or qualified category names.
#' @return Integer, NA where the name is not one of the twelve.
oac_number <- function(.name) {
  if (FALSE) .name <- c("Employment: Compensation", "R&D", "Business Structure: Investment and Merger")
  short_ <- sub("^.*: ", "", as.character(.name))
  short_[short_ %in% c("Investment and Merger", "Investment & Merger", "Mergers and Acquisitions")] <- "M&A"
  match(short_, .oac_categories$Short)
}

#' A category's printed label: its number and its short name, escaped for LaTeX
#'
#' @param .class Character. Category names, short or qualified.
#' @return Character, "(3) Compensation"; NA where the class is not one of the twelve.
oac_label <- function(.class) {
  if (FALSE) .class <- c("Employment: Compensation", "R&D")
  i_ <- oac_number(.name = .class)
  dplyr::if_else(is.na(i_), NA_character_,
                 paste0("(", .oac_categories$Number[i_], ") ", oa_tex_escape(.x = .oac_categories$Short[i_])))
}

#' 30's data tibble for one exhibit, or an abort that names the file
#'
#' @param .dir_data Character. 30's Output/Data.
#' @param .name Character. The exhibit's stem.
#' @return Tibble.
oac_read_30 <- function(.dir_data, .name) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .name     <- "ClassLabelled"
  }
  p_ <- fs::path(.dir_data, paste0(.name, ".parquet"))
  if (!fs::file_exists(p_)) cli::cli_abort("30 has not written {.file {p_}}; render 30 first.")
  arrow::read_parquet(p_)
}

#' The row labels of a category table: numbered leaves, bold parents, bold total
#'
#' 30's category tibbles carry Row (the printed name) and Kind (super, sub, total). A sub row is a leaf under its
#' parent, indented and numbered; a super row whose name is one of the twelve -- Licenses, Leases, Other -- is a leaf
#' of its own and is numbered in bold; any other super row is a parent, in bold without a number.
#'
#' @param .tab Tibble with Row and Kind.
#' @return Character, one label per row, escaped.
oac_row_labels <- function(.tab) {
  if (FALSE) .tab <- tibble::tibble(Row = c("Employment", "Compensation", "Licenses", "Total"),
                                    Kind = c("super", "sub", "super", "total"))
  lab_ <- oac_label(.class = .tab$Row)
  dplyr::case_when(
    .tab$Kind == "total"              ~ paste0("\\textbf{", oa_tex_escape(.x = .tab$Row), "}"),
    !is.na(lab_) & .tab$Kind == "sub" ~ paste0("\\hspace{1em}", lab_),
    !is.na(lab_)                      ~ paste0("\\textbf{", lab_, "}"),
    .default                          = paste0("\\textbf{", oa_tex_escape(.x = .tab$Row), "}")
  )
}

#' Whether a build of one of 30's tables is due
#'
#' @param .dir_own,.name,.path_30,.path_lib,.force As in the builders.
#' @return Logical.
oac_due <- function(.dir_own, .name, .path_30, .path_lib, .force) {
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(.name, c(".tex", ".tex", ".parquet")))
  oa_build_needed(.outputs = outs_, .inputs = c(.path_30, .path_lib), .force = .force)
}

oac_fmt3 <- function(.x) dplyr::if_else(is.na(.x), "--", formatC(.x, format = "f", digits = 3L))
oac_fmtn <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)

#' Build the labelled-sample table: N, share and second labels per category
#'
#' @param .dir_data Character. 30's Output/Data.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oac_build_labelled <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-C", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-C.R")
  }
  name_ <- "ClassLabelled"
  p30_  <- fs::path(.dir_data, paste0(name_, ".parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = name_)
  # THE AMENDMENT BLOCK -- Original / Amended under an "Amendment label" heading -- is a second table's worth of
  # rows; the appendix states the two counts in the text and prints the categories alone.
  cat_ <- tab_ |>
    dplyr::filter(is.na(.data$Level1) | .data$Level1 != "Amendment")
  cells_ <- tibble::tibble(
    Row    = oac_row_labels(.tab = cat_),
    N      = oac_fmtn(.x = cat_$N),
    Share  = formatC(cat_$Share, format = "f", digits = 1L),  # 30 stores the share as a percent
    Second = oac_fmtn(.x = dplyr::coalesce(cat_$Second, 0L))
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", "N", "\\%", "Second label"),
      .spec   = c(oa_col_text(.share = 0.52), oa_col_num(.mm = 18), oa_col_num(.mm = 14), oa_col_num(.mm = 24))
    ),
    .note    = paste(
      "The labelled sample: contracts with readable text and a label, by category. Parent rows sum their",
      "sub-categories. Second label counts the contracts that carry a second valid category, recorded beside the",
      "primary and never used in training. The numbers in parentheses are the categories' numbers throughout this",
      "appendix, in the order of the paper's categories table."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the deployed-models table from the sweep: what was crowned and what was deployed, per task
#'
#' The sweep itself is stated in the text -- the grid, how many configurations, how the winner was chosen. The table
#' shows only the configurations that left the sweep: the one that ships per task and the ones deployed beside it.
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_deployed <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-C", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-C.R")
  }
  name_ <- "ClassDeployed"
  p30_  <- fs::path(.dir_data, "ClassSweep.parquet")
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = "ClassSweep") |>
    dplyr::mutate(Crown = dplyr::coalesce(as.logical(.data$Crown), FALSE),
                  Deployed = dplyr::coalesce(as.logical(.data$Deployed), FALSE)) |>
    dplyr::filter(.data$Crown | .data$Deployed) |>
    dplyr::mutate(
      TaskOrder = match(.data$LabelCol, c("ClassDetailed", "ClassBroad", "AmendType")),
      Status    = dplyr::if_else(.data$Crown, "ships", "deployed")
    ) |>
    dplyr::arrange(.data$TaskOrder, dplyr::desc(.data$Crown), .data$MaxLen)
  cells_ <- tibble::tibble(
    Task     = oa_tex_escape(.x = as.character(tab_$Task)),
    Model    = oa_tex_escape(.x = sub("^.*/", "", as.character(tab_$Model))),
    Context  = as.character(tab_$MaxLen),
    Accuracy = oac_fmt3(.x = tab_$Accuracy),
    MacroF1  = oac_fmt3(.x = tab_$MacroF1),
    Status   = tab_$Status
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Task", "Encoder", "Context", "Accuracy", "Macro-F1", ""),
      .spec   = c(oa_col_text(.share = 0.20), oa_col_text(.share = 0.30), oa_col_num(.mm = 16), oa_col_num(.mm = 20),
                  oa_col_num(.mm = 20), "l")
    ),
    .note    = paste(
      "The configurations that left the sweep: per task, the one crowned by the highest mean out-of-fold macro-F1,",
      "which labels the corpus (ships), and the ones refitted beside it (deployed). Context is the number of tokens",
      "read from the start of a contract. Accuracy and macro-F1 are pooled out of fold over the five folds."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build a scores-by-category table in the appendix's cut, from one of 30's engine tibbles
#'
#' Serves the transformer and the keyword table, which share 30's layout. No fold uncertainty; the columns the
#' text reads: support, and per category precision, recall and F1, with lenient recall for the transformer and
#' coverage for the keyword table.
#'
#' @param .name Character. "ClassTransformer" or "ClassKeyword".
#' @param .columns Character. The score columns to print, from 30's tibble.
#' @param .header Character. Their printed headers.
#' @param .note Character. The table's note.
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_scores <- function(.name, .columns, .header, .note, .dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .name     <- "ClassTransformer"
    .columns  <- c("Precision", "Recall", "F1", "RecallLenient")
    .header   <- c("Precision", "Recall", "F1", "Lenient recall")
    .note     <- "..."
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-C", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-C.R")
  }
  p30_ <- fs::path(.dir_data, paste0(.name, ".parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = .name, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = .name)
  miss_ <- setdiff(.columns, names(tab_))
  if (length(miss_) > 0L) cli::cli_abort("30's {.val {(.name)}} lacks {.field {miss_}}.")
  cells_ <- dplyr::bind_cols(
    tibble::tibble(Row = oac_row_labels(.tab = tab_), N = oac_fmtn(.x = tab_$Support)),
    tab_ |>
      dplyr::select(dplyr::all_of(.columns)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(.x) oac_fmt3(.x = .x)))
  )
  oa_write_exhibit(
    .name    = .name,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", "N", .header),
      .spec   = c(oa_col_text(.share = 0.34), oa_col_num(.mm = 16), rep(oa_col_num(.mm = 18), length(.columns)))
    ),
    .note    = .note,
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the confusion matrix with numbered categories on both axes
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_confusion <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-C", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-C.R")
  }
  name_ <- "ClassConfusion"
  p30_  <- fs::path(.dir_data, paste0(name_, ".parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = name_)
  cols_ <- .oac_categories$Short
  miss_ <- setdiff(cols_, names(tab_))
  if (length(miss_) > 0L) cli::cli_abort("30's confusion matrix lacks the column{?s} {.val {miss_}}.")
  tab_ <- tab_[match(cols_, tab_$True), , drop = FALSE]
  cells_ <- dplyr::bind_cols(
    tibble::tibble(True = oac_label(.class = tab_$True)),
    tab_ |>
      dplyr::select(dplyr::all_of(cols_)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(.x) oac_fmtn(.x = .x)))
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", paste0("(", .oac_categories$Number, ")")),
      .spec   = c(oa_col_text(.share = 0.22), rep(oa_col_num(.mm = 9), 12L))
    ),
    .note    = paste(
      "Confusion matrix of the transformer that ships on the labelled sample, out of fold: rows are the true category,",
      "columns the predicted one, numbered as in the labelled-sample table; cells are numbers of contracts.",
      "Categories are in taxonomic order, so an error that stays inside a parent sits next to the diagonal and one",
      "that crosses a parent sits further from it."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the amendment table: the two labels and the total, without fold uncertainty
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_amendment <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-C", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-C.R")
  }
  name_ <- "ClassAmendment"
  p30_  <- fs::path(.dir_data, paste0(name_, ".parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oac_read_30(.dir_data = .dir_data, .name = name_)
  tot_ <- tab_$Label == "Total"
  cells_ <- tibble::tibble(
    Label     = dplyr::if_else(tot_, paste0("\\textbf{", oa_tex_escape(.x = tab_$Label), "}"),
                               oa_tex_escape(.x = tab_$Label)),
    N         = oac_fmtn(.x = tab_$Support),
    Predicted = oac_fmtn(.x = tab_$Predicted),
    Precision = oac_fmt3(.x = tab_$Precision),
    Recall    = oac_fmt3(.x = tab_$Recall),
    F1        = oac_fmt3(.x = tab_$F1)
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", "N", "Predicted", "Precision", "Recall", "F1"),
      .spec   = c(oa_col_text(.share = 0.30), rep(oa_col_num(.mm = 18), 5L))
    ),
    .note    = paste(
      "Out-of-fold scores of the amendment classifier that ships on the labelled sample. N is the number of",
      "contracts with the label, Predicted the number the classifier assigned it. The Total row reports overall",
      "accuracy in the precision column and macro recall and macro-F1 beside it."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' Build the arms table: every engine on every task, and the ceiling, without fold uncertainty
#'
#' @inheritParams oac_build_labelled
#' @return Invisibly, the build's status.
oac_build_arms <- function(.dir_data, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_data <- here::here("2_output", "30-FinalExhibits", "Output", "Data")
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-C", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-C.R")
  }
  name_ <- "ClassArms"
  p30_  <- fs::path(.dir_data, c("ClassArms.parquet", "ClassArmsCeiling.parquet"))
  if (!oac_due(.dir_own = .dir_own, .name = name_, .path_30 = p30_, .path_lib = .path_lib, .force = .force)) {
    return(invisible("up to date"))
  }
  arms_ <- oac_read_30(.dir_data = .dir_data, .name = "ClassArms")
  ceil_ <- oac_read_30(.dir_data = .dir_data, .name = "ClassArmsCeiling")
  task_ <- c(ClassDetailed = "Detailed (12)", ClassBroad = "Broad (7)", AmendType = "Amendment (2)")
  eng_  <- c(Bert256 = "Transformer, 256 tokens", Bert512 = "Transformer, 512 tokens", Kw = "Keyword table",
             Llm = "Language model")
  arms_ <- arms_ |>
    dplyr::mutate(TaskOrder = match(.data$Task, names(task_)), EngOrder = match(.data$Engine, names(eng_))) |>
    dplyr::arrange(.data$TaskOrder, .data$EngOrder)
  a_ <- tibble::tibble(
    Panel    = unname(dplyr::coalesce(task_[arms_$Task], arms_$Task)),
    Engine   = paste0(oa_tex_escape(.x = unname(dplyr::coalesce(eng_[arms_$Engine], arms_$Engine))),
                      dplyr::if_else(dplyr::coalesce(as.logical(arms_$Ships), FALSE), " (ships)", "")),
    Coverage = oac_fmt3(.x = arms_$Coverage),
    Accuracy = oac_fmt3(.x = arms_$Accuracy),
    AccSel   = oac_fmt3(.x = arms_$AccuracySel),
    MacroF1  = oac_fmt3(.x = arms_$MacroF1)
  )
  c_ <- tibble::tibble(
    Panel    = "Detailed, ceiling of perfect routing",
    Engine   = oa_tex_escape(.x = as.character(ceil_$Engines)),
    Coverage = "",
    Accuracy = oac_fmt3(.x = ceil_$Accuracy),
    AccSel   = "",
    MacroF1  = paste0("+", oac_fmt3(.x = ceil_$Marginal))
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = dplyr::bind_rows(a_, c_),
      .header = c("", "Coverage", "Accuracy", "Accuracy where committed", "Macro-F1"),
      .spec   = c(oa_col_text(.share = 0.36), oa_col_num(.mm = 18), oa_col_num(.mm = 18), oa_col_num(.mm = 30),
                  oa_col_num(.mm = 18))
    ),
    .note    = paste(
      "Every engine on every task, out of fold on the same five folds. Coverage is the share of contracts an engine",
      "labels; the transformer and the language model label every contract, the keyword table abstains where no",
      "term fires. Accuracy is over all contracts, an abstention counting as wrong; accuracy where committed is over",
      "the contracts the engine labelled. The last rows are the ceiling: the accuracy perfect routing would reach on",
      "the detailed task, taking for every contract whichever of the engines named is right, which needs the true",
      "label and cannot be run; the last column is what each added engine gains over the ones before it."
    ),
    .data    = dplyr::bind_rows(dplyr::mutate(arms_, Block = "arms"), dplyr::mutate(ceil_, Block = "ceiling")),
    .dir_own = .dir_own
  )
  invisible("built")
}


# 3. The second label against the runner-up --------------------------------------------------------------------------

#' The second-label data: every dual-labelled document with the model's two choices beside its two labels
#'
#' 03A records a second valid category for the minority of labelled documents that fit two, reviewed by hand so that
#' the primary is the intended one; training never uses it. 03B's classification file carries, for every labelled
#' document, the model's first and second choice with their probabilities, out of fold, and the second label beside
#' them. Two tables are built from it: the matrix of primary against second label, which shows where the annotators
#' saw two answers, and, per primary category, how often the model's first choice is the primary and its runner-up
#' the second label. The margin between the model's two choices, on documents with two labels and with one, is kept
#' in the data for the text.
#'
#' @param .path_class Character. 03B's crowned_classification.parquet for the detailed task.
#' @return Tibble: DocID, Primary, Second, Top1, Top2, Top1Prob, Margin, Dual.
oac_data_second <- function(.path_class) {
  if (FALSE) {
    .path_class <- here::here("2_output", "03B-ClassifyTrainBERT", "classification", "crowned_classification.parquet")
  }
  tab_ <- arrow::read_parquet(.path_class) |>
    dplyr::select(dplyr::any_of(c("DocID", "TrueLabel", "Top1Class", "Top1Prob", "Top2Class", "Margin",
                                  "ClassDetailed2")))
  need_ <- setdiff(c("TrueLabel", "Top1Class", "Top2Class", "Top1Prob", "Margin", "ClassDetailed2"), names(tab_))
  if (length(need_) > 0L) cli::cli_abort("The classification file lacks {.field {need_}}.")
  tab_ |>
    dplyr::transmute(
      DocID    = .data$DocID,
      Primary  = as.character(.data$TrueLabel),
      Second   = as.character(.data$ClassDetailed2),
      Top1     = as.character(.data$Top1Class),
      Top2     = as.character(.data$Top2Class),
      Top1Prob = .data$Top1Prob,
      Margin   = .data$Margin,
      Dual     = !is.na(.data$Second) & nzchar(.data$Second)
    )
}

#' The per-category hits and the margins, from the second-label data
#'
#' @param .tab Tibble from oac_data_second().
#' @return Tibble: Row (a category or Total), Kind, N (documents with two labels), Hit1, Hit2, Both, plus the
#'   two margin rows (Kind "margin": N is the group's size, Value its mean margin).
oac_second_summary <- function(.tab) {
  if (FALSE) .tab <- oac_data_second(.path_class = here::here("2_output", "03B-ClassifyTrainBERT", "classification",
                                                                 "crowned_classification.parquet"))
  dual_ <- dplyr::filter(.tab, .data$Dual) |>
    dplyr::mutate(
      Hit1 = .data$Top1 == .data$Primary,
      Hit2 = .data$Top2 == .data$Second,
      Both = .data$Hit1 & .data$Hit2,
      Order = oac_number(.name = .data$Primary)
    )
  by_ <- dual_ |>
    dplyr::summarise(N = dplyr::n(), Hit1 = sum(.data$Hit1), Hit2 = sum(.data$Hit2), Both = sum(.data$Both),
                     .by = c("Primary", "Order")) |>
    dplyr::arrange(.data$Order) |>
    dplyr::transmute(Row = .data$Primary, Kind = "category", N = .data$N, Hit1 = .data$Hit1, Hit2 = .data$Hit2,
                     Both = .data$Both, Value = NA_real_)
  tot_ <- tibble::tibble(Row = "Total", Kind = "total", N = nrow(dual_), Hit1 = sum(dual_$Hit1),
                         Hit2 = sum(dual_$Hit2), Both = sum(dual_$Both), Value = NA_real_)
  mar_ <- .tab |>
    dplyr::summarise(N = dplyr::n(), Value = mean(.data$Margin, na.rm = TRUE), .by = "Dual") |>
    dplyr::transmute(Row = dplyr::if_else(.data$Dual, "Margin, two labels", "Margin, one label"), Kind = "margin",
                     N = .data$N, Hit1 = NA_integer_, Hit2 = NA_integer_, Both = NA_integer_, Value = .data$Value)
  dplyr::bind_rows(by_, tot_, mar_)
}

#' Build the two second-label tables: the matrix of label pairs, and the model's hits per category
#'
#' @param .path_class Character. 03B's crowned_classification.parquet.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oac_build_second <- function(.path_class, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_class <- here::here("2_output", "03B-ClassifyTrainBERT", "classification", "crowned_classification.parquet")
    .dir_own    <- here::here("2_output", "40-OnlineAppendix-C", "Output")
    .force      <- FALSE
    .path_lib   <- here::here("1_code", "40-OnlineAppendix-C.R")
  }
  names_ <- c("ClassSecondPairs", "ClassSecond")
  outs_  <- unlist(purrr::map(names_, \(.n) fs::path(.dir_own, c("Tables", "Notes", "Data"),
                                                       paste0(.n, c(".tex", ".tex", ".parquet")))))
  if (!fs::file_exists(.path_class)) return(invisible("waiting for 03B's crowned_classification.parquet"))
  if (!oa_build_needed(.outputs = outs_, .inputs = c(.path_class, .path_lib), .force = .force)) {
    return(invisible("up to date"))
  }
  tab_  <- oac_data_second(.path_class = .path_class)
  dual_ <- dplyr::filter(tab_, .data$Dual)
  # THE MATRIX: primary label down, second label across, both numbered; a zero prints as a blank so the pairs stand out
  k_ <- nrow(.oac_categories)
  m_ <- matrix(0L, nrow = k_, ncol = k_)
  i_ <- oac_number(.name = dual_$Primary)
  j_ <- oac_number(.name = dual_$Second)
  if (anyNA(i_) || anyNA(j_)) cli::cli_abort("A second-label category is not one of the twelve.")
  for (r_ in seq_along(i_)) m_[i_[r_], j_[r_]] <- m_[i_[r_], j_[r_]] + 1L
  pairs_ <- tibble::as_tibble(m_, .name_repair = \(.x) .oac_categories$Short) |>
    dplyr::mutate(Primary = .oac_categories$Short, .before = 1L)
  cells_p_ <- dplyr::bind_cols(
    tibble::tibble(Primary = oac_label(.class = pairs_$Primary)),
    pairs_ |>
      dplyr::select(-"Primary") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(.x) dplyr::if_else(.x == 0L, "", oac_fmtn(.x = .x))))
  )
  oa_write_exhibit(
    .name    = names_[1L],
    .lines   = oa_frame_table(
      .tab    = cells_p_,
      .header = c("Primary label", paste0("(", .oac_categories$Number, ")")),
      .spec   = c(oa_col_text(.share = 0.22), rep(oa_col_num(.mm = 9), k_))
    ),
    .note    = paste(
      "The", oac_fmtn(.x = nrow(dual_)), "labelled contracts that carry a second valid category: rows are the",
      "primary label, columns the second, numbered as in the labelled-sample table; cells are numbers of contracts,",
      "blank where zero. Every such contract was reviewed and the intended primary recorded, so the order of the two",
      "labels is informative."
    ),
    .data    = pairs_,
    .dir_own = .dir_own
  )
  # THE HITS: per primary category, the model's first choice against the primary and its runner-up against the second
  sum_ <- oac_second_summary(.tab = tab_)
  rows_ <- dplyr::filter(sum_, .data$Kind %in% c("category", "total"))
  pct_ <- function(.n, .d) paste0(oac_fmtn(.x = .n), " (", formatC(100 * .n / .d, format = "f", digits = 0L), ")")
  cells_h_ <- tibble::tibble(
    Row  = dplyr::if_else(rows_$Kind == "total", paste0("\\textbf{", rows_$Row, "}"),
                          dplyr::coalesce(oac_label(.class = rows_$Row), oa_tex_escape(.x = rows_$Row))),
    N    = oac_fmtn(.x = rows_$N),
    Hit1 = pct_(.n = rows_$Hit1, .d = rows_$N),
    Hit2 = pct_(.n = rows_$Hit2, .d = rows_$N),
    Both = pct_(.n = rows_$Both, .d = rows_$N)
  )
  oa_write_exhibit(
    .name    = names_[2L],
    .lines   = oa_frame_table(
      .tab    = cells_h_,
      .header = c("Primary label", "Two labels", "First choice is the primary (\\%)",
                  "Runner-up is the second label (\\%)", "Both (\\%)"),
      .spec   = c(oa_col_text(.share = 0.26), oa_col_num(.mm = 18), oa_col_num(.mm = 30), oa_col_num(.mm = 32),
                  oa_col_num(.mm = 20))
    ),
    .note    = paste(
      "The contracts with two labels, by primary label, against the detailed transformer's first and second choice",
      "out of fold: how many the model's first choice labels with the primary, how many its runner-up labels with",
      "the second label, and how many both. Shares are of the row's contracts."
    ),
    .data    = sum_,
    .dir_own = .dir_own
  )
  invisible("built")
}
