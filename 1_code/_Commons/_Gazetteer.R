# _Gazetteer.R: deterministic HQ-gazetteer keyword arm (gaz_*) ----
# A curated, fully-interpretable counterpart to the MINED keyword classifier (03C):
# instead of learning a per-class lexicon from the training folds, it scores each
# document against a FIXED, human-curated term list (keywords_06_2025.xlsx, HQ == 1)
# and predicts the class whose curated terms fire hardest, abstaining when none do.
#
# Why this is pure R, no Python engine, no CV folds:
#   The gazetteer never trains. It does not look at labels and it does not depend on
#   a fold split -- it predicts every document identically regardless of fold. So
#   there is nothing to cross-validate (no learned parameters to estimate the
#   generalisation of) and no leakage to guard against by holding docs out. It emits
#   the SAME prediction schema as clf_pool_predictions (DocID, TrueLabel, PredLabel,
#   Score), so it drops straight into 03C's kw_calibrate / kw_gate and 03D's
#   orch_route_selective / orch_subset_compare with no special-casing.
#
# Structural coverage caveat (state it, do not hide it):
#   The gazetteer taxonomy (Class2 sheet, 10 categories) maps onto 10 of the 12
#   ClassDetailed classes. R&D and Business Structure: Peer Agreements have NO
#   curated terms -- the gazetteer is constitutionally unable to predict them, and
#   they route to BERT in every selective blend. Customer / Supplier maps only
#   loosely (via "Inventory or Services"). These are exactly the classes where BERT
#   most outperforms keyword, so the gazetteer -- like the mined lexicon -- is
#   confident only where BERT is already near-perfect. That is the point of the
#   experiment, not a flaw to paper over.
#
# House style: native pipe; explicit package::function; dot-prefixed args;
# underscore-suffixed locals; .data$ for existing columns, bare CamelCase for new;
# if (FALSE) dev blocks; cli/fs/here; pure ASCII; stringi over base string ops;
# {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_xlsx <- here::here("0_data", "keywords_06_2025.xlsx")
  .tab_prep  <- arrow::read_parquet(.lP$Input$Prepared)
}


# The taxonomy bridge (THE editorial decision -- review these rows) --------

#' Map the gazetteer's Class2 categories onto the 12-class ClassDetailed taxonomy
#'
#' The gazetteer sheet uses its own coarser 10-class taxonomy with its own spelling
#' (note "Empoyment" is misspelled in the source file -- the keys below match the
#' file verbatim, so do not "fix" them). Two rows are SOFT mappings worth a second
#' look: M&A -> Investment and Merger, and "Inventory or Services" -> Customer /
#' Supplier. Two ClassDetailed classes have no gazetteer source at all (R&D, Peer
#' Agreements) and are intentionally absent here. Edit this single vector to retune
#' the bridge; everything downstream follows.
#'
#' @return Named character vector: gazetteer Category (file spelling) -> ClassDetailed.
gaz_class_map <- function() {
  c(
    "Empoyment - Compensation"                 = "Employment: Compensation",
    "Empoyment - Legal"                        = "Employment: Legal",
    "Fin. Instruments - Debt"                  = "Financial Instruments: Credit",
    "Fin. Instruments - Equity"                = "Financial Instruments: Equity",
    "Leases - Leases Category"                 = "Leases",
    "License - License Category"               = "Licenses",
    "MergerAcquisition - M&A Category"         = "Business Structure: Investment and Merger", # SOFT
    "Other - Other Category"                   = "Other",
    "PurchasesOrSales - Assets"                = "Purchases and Sales: Assets",
    "PurchasesOrSales - Inventory or Services" = "Customer / Supplier"                        # SOFT
  )
}


# Load + map the curated lexicon ------------------------------------------

#' Load the HQ gazetteer and bridge it to ClassDetailed
#'
#' Reads one sheet of keywords_06_2025.xlsx, optionally keeps only the curated
#' high-quality terms (HQ == 1), lower-cases the terms once (matching is
#' case-insensitive), maps each row's Category to a ClassDetailed label via
#' gaz_class_map (dropping any category with no mapping), and reports coverage. The
#' returned table is the term list every doc is scored against.
#'
#' @param .path_xlsx Path to keywords_06_2025.xlsx.
#' @param .sheet Worksheet to read (default "Class2", the 10-class detailed sheet).
#' @param .hq_only Logical. Keep only HQ == 1 terms (default TRUE).
#' @return Tibble: Class (ClassDetailed), Term (lower-cased), CategoryRaw.
gaz_load <- function(.path_xlsx, .sheet = "Class2", .hq_only = TRUE) {
  if (FALSE) {
    .path_xlsx <- here::here("0_data", "keywords_06_2025.xlsx")
    .sheet     <- "Class2"
    .hq_only   <- TRUE
  }
  if (!fs::file_exists(.path_xlsx)) cli::cli_abort("Gazetteer not found at {(.path_xlsx)}")

  raw_ <- readxl::read_excel(.path_xlsx, sheet = .sheet)
  need_ <- c("Category", "Keyword", "HQ")
  miss_ <- setdiff(need_, names(raw_))
  if (length(miss_) > 0L) cli::cli_abort("Gazetteer sheet missing columns: {miss_}")

  if (.hq_only) raw_ <- raw_ |> dplyr::filter(.data$HQ == 1)

  map_ <- gaz_class_map()
  out_ <- raw_ |>
    dplyr::transmute(
      CategoryRaw = .data$Category,
      Class       = unname(map_[.data$Category]),
      Term        = stringi::stri_trans_tolower(trimws(.data$Keyword))
    ) |>
    dplyr::filter(!is.na(.data$Class), .data$Term != "") |>
    dplyr::distinct(Class, Term, .keep_all = TRUE)

  dropped_ <- setdiff(unique(raw_$Category), names(map_))
  if (length(dropped_) > 0L) {
    cli::cli_alert_info("Unmapped gazetteer categories dropped: {paste(dropped_, collapse = '; ')}")
  }
  cli::cli_alert_success(
    "Gazetteer: {nrow(out_)} {if (.hq_only) 'HQ ' else ''}terms over {dplyr::n_distinct(out_$Class)} ClassDetailed classes"
  )
  out_
}

#' Per-ClassDetailed term counts, including the classes the gazetteer cannot reach
#'
#' Makes the structural-coverage point explicit: the count of curated terms behind
#' each ClassDetailed class, with the unreachable classes (R&D, Peer) shown as 0.
#' The classes with 0 terms are the ones a gazetteer selective blend can never label
#' -- they always fall through to BERT.
#'
#' @param .lexicon Output of gaz_load.
#' @param .all_classes Character vector of all ClassDetailed labels (from the prep
#'   sample); classes absent from the lexicon are reported with NTerms = 0.
#' @return Tibble: Class, NTerms (descending), Reachable.
gaz_term_counts <- function(.lexicon, .all_classes) {
  cnt_ <- .lexicon |> dplyr::count(Class, name = "NTerms")
  tibble::tibble(Class = .all_classes) |>
    dplyr::left_join(cnt_, by = dplyr::join_by(Class)) |>
    dplyr::mutate(
      NTerms    = dplyr::coalesce(.data$NTerms, 0L),
      Reachable = .data$NTerms > 0L
    ) |>
    dplyr::arrange(dplyr::desc(.data$NTerms))
}


# Score documents against the gazetteer -----------------------------------

#' Score the prepared sample against the curated gazetteer (deterministic)
#'
#' For each document, counts how many of a class's curated terms FIRE (appear at
#' least once, matched on whole-word boundaries so "RENT" does not hit "AGREEMENT"),
#' and predicts the class with the most distinct terms firing. The confidence Score
#' is that winning count -- the number of curated terms that agreed -- which is the
#' natural per-class confidence signal for kw_calibrate / kw_gate. A document where
#' no term fires abstains ("(none)"). Ties are broken by class order (stable).
#'
#' Whole-word matching uses ICU literal-quoting (\\Q..\\E) inside word boundaries, so
#' regex metacharacters in a term (e.g. "M&A", "R&D") are matched literally. The text
#' is lower-cased once; terms were lower-cased at load.
#'
#' @param .tab_prep Prepared sample (DocID, ClassDetailed, plus the source column).
#' @param .lexicon Output of gaz_load (Class, Term).
#' @param .source "text" (body), "docdesc" (filer title), or "combined" (both).
#' @param .text_col,.desc_col Body and title column names.
#' @return Tibble: DocID, TrueLabel, PredLabel, Score (one row per doc) -- the same
#'   schema as clf_pool_predictions, so it feeds the keyword / routing layers as-is.
gaz_score <- function(.tab_prep, .lexicon,
                      .source = c("text", "docdesc", "combined"),
                      .text_col = "Text", .desc_col = "DocDesc",
                      .none = "(none)") {
  if (FALSE) {
    .tab_prep <- tab_prep
    .lexicon  <- gaz_lex
    .source   <- "text"
    .none     <- "(none)"
  }
  .source <- match.arg(.source)

  src_ <- switch(.source,
    text     = .tab_prep[[.text_col]],
    docdesc  = .tab_prep[[.desc_col]],
    combined = paste(dplyr::coalesce(.tab_prep[[.desc_col]], ""),
                     dplyr::coalesce(.tab_prep[[.text_col]], ""))
  )
  text_ <- stringi::stri_trans_tolower(dplyr::coalesce(src_, ""))

  terms_   <- .lexicon$Term
  classes_ <- sort(unique(.lexicon$Class))
  cli::cli_alert_info("Scoring {nrow(.tab_prep)} docs against {length(terms_)} terms ({(.source)}) ...")

  # docs x terms logical: did this term fire (>=1 whole-word hit) in this doc?
  hit_mat_ <- vapply(
    terms_,
    function(t_) {
      pat_ <- paste0("\\b\\Q", t_, "\\E\\b")
      stringi::stri_count_regex(text_, pat_) > 0L
    },
    logical(length(text_))
  )

  # docs x classes: how many distinct class terms fired
  count_mat_ <- vapply(
    classes_,
    function(c_) {
      cols_ <- which(.lexicon$Class == c_)
      rowSums(hit_mat_[, cols_, drop = FALSE])
    },
    numeric(length(text_))
  )

  maxv_  <- apply(count_mat_, 1L, max)
  wmax_  <- max.col(count_mat_, ties.method = "first")
  pred_  <- ifelse(maxv_ == 0L, .none, classes_[wmax_])

  out_ <- tibble::tibble(
    DocID     = .tab_prep$DocID,
    TrueLabel = .tab_prep$ClassDetailed,
    PredLabel = pred_,
    Score     = as.numeric(maxv_)
  )
  cli::cli_alert_success(
    "Gazetteer scored: predicts {scales::percent(mean(out_$PredLabel != .none), 0.1)} of docs (abstains on the rest)"
  )
  out_
}
