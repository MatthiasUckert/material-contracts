# 03C-CoWorkCompile.R -- turn reading-session responses into engine term files
#
# WHAT THIS IS
# The return leg of the generated arm. 03C-CoWorkKeywords.R writes one bundle folder per task; a
# reading session fills each bundle's responses/ with JSONL; this script reads those, normalises the
# terms into the form the mining engine can match, and writes one term file per task.
#
# It sources nothing and needs only the bundle root and 03A's prepared sample.
#
# WHY THE TERMS NEED NORMALISING
# The engine tokenises with [a-zA-Z]{3,} and forms n-grams from what survives, so "notice of default"
# is stored as the bigram "notice default" and a term supplied in its written form would never match.
# The session writes natural English and this script folds it. A phrase that survives to nothing, or
# that exceeds the length cap, is reported rather than dropped without notice, because a silent drop
# is indistinguishable from the session simply having proposed fewer terms.
#
# The cap is five words rather than the mining grid's three: in term-file mode the engine takes its
# n-gram range from the supplied vocabulary rather than from the grid, so long phrases match, and the
# long ones are exactly the terminology a frequency miner cannot reach.
#
# House style: native pipe; explicit package::function; dot-prefixed arguments; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) development blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} in cli interpolation.


# 1. Configuration ----------------------------------------------------------------------------------

.DIR_ROOT      <- fs::path("/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts",
                           "MatContractData", "KeyWordsClaude")
.PATH_PREPARED <- here::here("2_output", "03A-ClassifyPrepare", "prepared.parquet")

# One row per bundle folder, matching the task table in 03C-CoWorkKeywords.R.
.TASKS <- tibble::tibble(
  Slug     = c("detailed", "broad", "amendment"),
  LabelCol = c("ClassDetailed", "ClassBroad", "AmendType")
)

.MAX_WORDS <- 5L   # longest phrase kept after normalisation
.MIN_CHARS <- 3L   # the engine's token pattern is [a-zA-Z]{.MIN_CHARS,}


# 2. Helpers ----------------------------------------------------------------------------------------

#' Fold a natural-language phrase into the engine's analyzer form
#'
#' Returns NA where the phrase cannot fire, either because nothing survives tokenisation or because it
#' is longer than the cap, so that the caller can report the loss rather than absorb it.
#'
#' @param .x Character vector of proposed terms.
#' @param .max_words Longest phrase kept.
#' @param .min_chars Shortest token the engine retains.
#' @return Character vector of normalised terms, NA where unusable.
cwc_normalise <- function(.x, .max_words = 5L, .min_chars = 3L) {
  if (FALSE) {
    .x         <- c("notice of default", "in re", "amended from time to time")
    .max_words <- 5L
    .min_chars <- 3L
  }
  .x |>
    stringi::stri_trans_tolower() |>
    stringi::stri_extract_all_regex(pattern = paste0("[a-z]{", .min_chars, ",}")) |>
    purrr::map_chr(function(.t) {
      if (length(.t) == 0L || all(is.na(.t))) return(NA_character_)
      if (length(.t) > .max_words) return(NA_character_)
      paste(.t, collapse = " ")
    })
}

#' Read one JSONL response file
#'
#' One object per line, so a malformed line costs that line and never the file.
#'
#' @param .path Path to a .jsonl file.
#' @return Tibble with Class, TermRaw, Rationale, Evidence, File, Line.
cwc_read_jsonl <- function(.path) {
  if (FALSE) .path <- fs::dir_ls(fs::path(.DIR_ROOT, "detailed", "responses"))[[1]]

  lines_ <- readLines(.path, warn = FALSE)
  keep_  <- trimws(lines_) != ""

  purrr::map2(lines_[keep_], which(keep_), function(.l, .i) {
    obj_  <- tryCatch(jsonlite::fromJSON(.l), error = function(e) NULL)
    one_  <- function(.v) if (is.null(.v)) NA_character_ else as.character(.v)[[1]]
    tibble::tibble(
      Class     = if (is.null(obj_)) NA_character_ else one_(obj_$Class),
      TermRaw   = if (is.null(obj_)) NA_character_ else one_(obj_$Term),
      Rationale = if (is.null(obj_)) NA_character_ else one_(obj_$Rationale),
      Evidence  = if (is.null(obj_)) NA_character_ else one_(obj_$Evidence),
      File      = as.character(fs::path_file(.path)),
      Line      = .i
    )
  }) |>
    purrr::list_rbind()
}

#' Valid class labels for one task
#'
#' Read from the sample rather than hard-coded, so a change to the taxonomy cannot leave this script
#' silently accepting labels that no longer exist.
#'
#' @param .path_prepared Prepared parquet written by 03A.
#' @param .label_col Label column for the task.
#' @return Character vector of class labels.
cwc_valid_classes <- function(.path_prepared, .label_col) {
  tab_ <- arrow::read_parquet(.path_prepared)
  if (!.label_col %in% names(tab_)) cli::cli_abort("No column {(.label_col)} in the prepared sample")
  sort(unique(tab_[[.label_col]][!is.na(tab_[[.label_col]])]))
}


# 3. Compile ----------------------------------------------------------------------------------------

#' Compile one bundle's responses into a term file
#'
#' Responses are located inside the bundle folder, so which task a file belongs to is settled by where
#' it sits rather than inferred from its contents. That matters because several class names occur in
#' more than one taxonomy: a broad response mistaken for a detailed one would be scored as a
#' twelve-category list while holding seven, and nothing in the class field would reveal it.
#'
#' @param .dir_bundle Bundle folder for one task.
#' @param .label_col Label column the bundle addresses.
#' @param .slug Task slug, used in the output filename.
#' @param .path_prepared Prepared parquet, supplying the valid class labels.
#' @param .max_words,.min_chars Normalisation parameters.
#' @return Tibble of kept terms, invisibly; NULL where the bundle holds no responses.
cwc_compile_bundle <- function(.dir_bundle, .label_col, .slug, .path_prepared,
                               .max_words = 5L, .min_chars = 3L) {
  if (FALSE) {
    .dir_bundle    <- fs::path(.DIR_ROOT, "detailed")
    .label_col     <- "ClassDetailed"
    .slug          <- "detailed"
    .path_prepared <- .PATH_PREPARED
    .max_words     <- 5L
    .min_chars     <- 3L
  }
  dir_resp_ <- fs::path(.dir_bundle, "responses")
  fils_ <- if (fs::dir_exists(dir_resp_)) fs::dir_ls(dir_resp_, glob = "*.jsonl") else character()
  if (length(fils_) == 0L && fs::dir_exists(.dir_bundle)) {
    fils_ <- fs::dir_ls(.dir_bundle, glob = "*.jsonl")
  }
  if (length(fils_) == 0L) {
    cli::cli_alert_warning("{(.slug)}: no .jsonl found; skipping")
    return(invisible(NULL))
  }

  raw_    <- purrr::map(fils_, cwc_read_jsonl) |> purrr::list_rbind()
  valid_  <- cwc_valid_classes(.path_prepared = .path_prepared, .label_col = .label_col)

  scored_ <- raw_ |>
    dplyr::mutate(
      Term   = cwc_normalise(.x = .data$TermRaw, .max_words = .max_words, .min_chars = .min_chars),
      Reason = dplyr::case_when(
        is.na(.data$TermRaw)             ~ "unparseable line",
        !.data$Class %in% valid_         ~ "class absent from this taxonomy",
        is.na(.data$Term)                ~ "unusable form: empty or too long",
        TRUE                             ~ NA_character_
      )
    )

  kept_ <- scored_ |>
    dplyr::filter(is.na(.data$Reason)) |>
    dplyr::distinct(Class, Term, .keep_all = TRUE) |>
    dplyr::arrange(.data$Class, .data$Term) |>
    dplyr::select(Class, Term, TermRaw, Rationale, Evidence, File)

  rejected_ <- scored_ |> dplyr::filter(!is.na(.data$Reason))
  nDupe_    <- sum(is.na(scored_$Reason)) - nrow(kept_)

  cli::cli_h3("{(.slug)}")
  cli::cli_alert_info("{length(fils_)} file{?s}, {nrow(raw_)} proposal{?s}")
  if (nrow(rejected_) > 0L) {
    rejected_ |> dplyr::count(.data$Reason, name = "N") |> dplyr::arrange(dplyr::desc(.data$N)) |> print()
  }
  if (nDupe_ > 0L) cli::cli_alert_info("{nDupe_} duplicate (class, term) pair{?s} collapsed")
  cli::cli_alert_success("Kept {nrow(kept_)} unique terms across {dplyr::n_distinct(kept_$Class)} classes")

  path_out_ <- fs::path(.dir_bundle, paste0("cowork_terms_", .slug, ".parquet"))
  arrow::write_parquet(kept_, path_out_)
  readr::write_csv(kept_, fs::path(.dir_bundle, paste0("cowork_terms_", .slug, ".csv")))
  if (nrow(rejected_) > 0L) {
    rejected_ |>
      dplyr::select(Class, TermRaw, Reason, File, Line) |>
      readr::write_csv(fs::path(.dir_bundle, paste0("cowork_rejected_", .slug, ".csv")))
  }

  kept_ |>
    dplyr::count(.data$Class, name = "nTerms") |>
    dplyr::arrange(.data$nTerms) |>
    utils::head(n = 5L) |>
    print()
  cli::cli_alert_info(
    "A class with very few terms here will contribute nothing to the table however good its \\
     proposals were; a class absent entirely means the session skipped its unit."
  )

  invisible(kept_)
}


# 4. Run --------------------------------------------------------------------------------------------

if (!fs::dir_exists(.DIR_ROOT)) cli::cli_abort("No bundle root at {(.DIR_ROOT)}")
if (!fs::file_exists(.PATH_PREPARED)) cli::cli_abort("Run 03A before this script")

lst_terms <- purrr::pmap(.TASKS, function(Slug, LabelCol) {
  cwc_compile_bundle(
    .dir_bundle    = fs::path(.DIR_ROOT, Slug),   # one folder per task
    .label_col     = LabelCol,                    # supplies the valid class labels
    .slug          = Slug,                        # used in the output filename
    .path_prepared = .PATH_PREPARED,              # sample written by 03A
    .max_words     = .MAX_WORDS,                  # engine matches phrases this long in term-file mode
    .min_chars     = .MIN_CHARS                   # shorter tokens are dropped before n-grams form
  )
}) |>
  rlang::set_names(.TASKS$Slug)

cli::cli_h2("Next")
cli::cli_text(
  "03C-ClassifyTrainKeyword.qmd reads these term files and scores each on the held-out fold, \\
   which is the only fold the reading session never saw."
)
