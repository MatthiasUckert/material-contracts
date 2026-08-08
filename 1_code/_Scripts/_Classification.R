# =============================================================================
# _Classification.R
# Functions for reconciling the OLD (Schema 1) and NEW (Schema 2) contract
# classifications into a single final per-document label set.
#
# Sourced by Classification-Overview.qmd. No top-level side effects: all
# configuration (paths, canonical taxonomy, relabel / forward maps) is passed
# in as arguments, so the same functions drive both the report and any script.
#
# Conventions: native pipe, explicit package:: prefixes, dot-prefixed args,
# underscore-suffixed locals. Pure ASCII.
# =============================================================================


# Paths -----------------------------------------------------------------------

#' Pick the first path in a vector that exists on disk
#'
#' Lets several machines keep their own paths in one vector; the first that
#' resolves is used. Empty strings are ignored.
#'
#' @param .paths Character vector of candidate paths.
#' @param .what  Short label used in the error message.
#' @return The first existing path (length-1 character).
#' @export
cls_pick_path <- function(.paths, .what) {
  paths_ <- .paths[nzchar(.paths)]
  hit_   <- paths_[fs::file_exists(paths_)]
  if (length(hit_) == 0L) {
    cli::cli_abort("No existing path found for {.what}. Checked: {paths_}")
  }
  hit_[[1]]
}


# Log access ------------------------------------------------------------------

#' Read and tidy the classification_log table from the SQLite store
#'
#' @param .path_db Path to classification_data.db.
#' @return Tibble: DocID, TimeStamp, Schema, Class, Value (SKIP rows dropped).
#' @export
cls_read_log <- function(.path_db) {
  con_ <- DBI::dbConnect(RSQLite::SQLite(), .path_db)
  on.exit(DBI::dbDisconnect(con_), add = TRUE)
  dplyr::tbl(con_, "classification_log") |>
    dplyr::select(DocID = doc_id, TimeStamp = timestamp, Schema = schema,
                  Class = class, Value = value) |>
    dplyr::collect() |>
    dplyr::filter(Value != "--SKIP DOCUMENT--")
}

#' Keep the latest session per (DocID, Class, Schema), ranking values within it
#'
#' Rank 1 is the primary label; rank 2 (if present) is a dual classification.
#'
#' @param .data Tidied log tibble (see cls_read_log).
#' @return Same rows for the latest session per key, with a Rank column.
#' @export
cls_latest <- function(.data) {
  .data |>
    dplyr::group_by(DocID, Class, Schema) |>
    dplyr::filter(TimeStamp == max(TimeStamp)) |>
    dplyr::arrange(DocID, Class, Schema, Value) |>
    dplyr::mutate(Rank = dplyr::row_number()) |>
    dplyr::ungroup()
}


# Stage builders --------------------------------------------------------------

#' Raw Schema-2 label tally (primary + secondary), latest session per doc
#'
#' Used for label-drift diagnostics and validation. Counts include secondary
#' (dual) labels, so totals exceed the count of documents.
#'
#' @param .log Tidied log tibble.
#' @return Tibble: Value, n_raw.
#' @export
cls_raw_s2_counts <- function(.log) {
  .log |>
    dplyr::filter(Class == "DocClass", Schema == 2) |>
    cls_latest() |>
    dplyr::count(Value, name = "n_raw")
}

#' Build wide Schema-2 labels (S2_v1 primary, S2_v2 secondary) from the log
#'
#' Applies the raw -> canonical relabel map and collapses degenerate duals
#' (where the secondary label equals the primary) back to a single label.
#'
#' @param .log     Tidied log tibble.
#' @param .relabel Tibble: DocClassRaw, DocClass (canonical).
#' @return Tibble: DocID, S2_v1, S2_v2.
#' @export
cls_build_s2 <- function(.log, .relabel) {
  s2_ <- .log |>
    dplyr::filter(Class == "DocClass", Schema == 2) |>
    cls_latest() |>
    dplyr::left_join(.relabel, by = c("Value" = "DocClassRaw")) |>
    dplyr::mutate(DocClass = dplyr::coalesce(DocClass, Value)) |>
    dplyr::select(DocID, Rank, DocClass) |>
    tidyr::pivot_wider(names_from = Rank, names_prefix = "S2_v", values_from = DocClass)
  if (!"S2_v2" %in% names(s2_)) s2_$S2_v2 <- NA_character_
  s2_ |>
    dplyr::mutate(S2_v2 = dplyr::if_else(S2_v2 == S2_v1, NA_character_, S2_v2))
}

#' Build Schema-1 labels + AmendType from the old parquet (latest row per doc)
#'
#' Applies the S1 -> S2 forward map. Labels absent from the map (the split
#' categories) stay NA in DocClassS1Mapped and become UNRESOLVED in cls_combine
#' unless an S2 label exists.
#'
#' @param .path_parq Path to Classifications.parquet.
#' @param .s1_map    Tibble: DocClassS1, DocClass (canonical forward target).
#' @return Tibble: DocID, AmendType, DocClassS1, DocClassS1Mapped.
#' @export
cls_build_s1 <- function(.path_parq, .s1_map) {
  arrow::read_parquet(.path_parq, mmap = FALSE) |>
    dplyr::filter(DocClass != "--SKIP DOCUMENT--") |>
    dplyr::group_by(DocID) |>
    dplyr::slice_max(Timestamp, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(DocID, AmendType, DocClassS1 = DocClass) |>
    dplyr::left_join(.s1_map, by = "DocClassS1") |>
    dplyr::rename(DocClassS1Mapped = DocClass)
}

#' Combine S2 + S1 into the final per-document classification
#'
#' Rule: S2 wins; else mapped S1 (name-change / unchanged only); else
#' UNRESOLVED. Attaches Level 1 / Level 2 from the canonical taxonomy.
#'
#' @param .s2       Output of cls_build_s2.
#' @param .s1       Output of cls_build_s1.
#' @param .taxonomy Tibble: Level1, Level2, DocClass (canonical 12 leaves).
#' @return One row per DocID with the final classification and provenance.
#' @export
cls_combine <- function(.s2, .s1, .taxonomy) {
  .s2 |>
    dplyr::full_join(.s1, by = "DocID") |>
    dplyr::mutate(
      DocClassFinal1 = dplyr::coalesce(S2_v1, DocClassS1Mapped),
      DocClassFinal2 = S2_v2,
      DualClass      = dplyr::if_else(!is.na(S2_v2), 1L, 0L),
      Provenance     = dplyr::case_when(
        !is.na(S2_v1)            ~ "S2",
        !is.na(DocClassS1Mapped) ~ "S1_fallback",
        TRUE                     ~ "UNRESOLVED"
      )
    ) |>
    dplyr::left_join(
      .taxonomy |> dplyr::select(DocClassFinal1 = DocClass, Level1, Level2),
      by = "DocClassFinal1"
    ) |>
    dplyr::select(DocID, AmendType, DocClassFinal1, DocClassFinal2, DualClass,
                  Level1, Level2, Provenance, DocClassS1)
}


# Diagnostics -----------------------------------------------------------------

#' Structural validation of the final table
#'
#' @return Tibble: Check, Status (PASS/FAIL), Detail.
#' @export
cls_validate <- function(.final, .taxonomy, .relabel, .raw_counts) {
  bad_ <- setdiff(
    unique(stats::na.omit(c(.final$DocClassFinal1, .final$DocClassFinal2))),
    .taxonomy$DocClass
  )
  dup_ <- anyDuplicated(.final$DocID) > 0L
  unk_ <- .raw_counts |>
    dplyr::filter(!Value %in% .taxonomy$DocClass, !Value %in% .relabel$DocClassRaw) |>
    dplyr::pull(Value)

  tibble::tribble(
    ~Check,                                    ~Status,                                    ~Detail,
    "All final labels canonical (12 leaves)",  if (length(bad_) == 0L) "PASS" else "FAIL", paste(bad_, collapse = "; "),
    "One row per DocID",                       if (!dup_) "PASS" else "FAIL",              if (dup_) "duplicate DocIDs" else "",
    "All raw S2 labels mapped (no UNKNOWN)",   if (length(unk_) == 0L) "PASS" else "FAIL", paste(unk_, collapse = "; ")
  )
}

#' Reconcile the log's own Schema-1 entries against the parquet Schema-1
#'
#' Confirms whether the parquet is a safe S1 source. Agree == TRUE where they
#' match, FALSE where they differ, NA where only one source has the document.
#'
#' @return Tibble: DocID, S1_parq, S1_log, Agree.
#' @export
cls_reconcile_s1 <- function(.log, .s1) {
  log_s1_ <- .log |>
    dplyr::filter(Class == "DocClass", Schema == 1) |>
    cls_latest() |>
    dplyr::filter(Rank == 1) |>
    dplyr::select(DocID, S1_log = Value)
  .s1 |>
    dplyr::select(DocID, S1_parq = DocClassS1) |>
    dplyr::full_join(log_s1_, by = "DocID") |>
    dplyr::mutate(Agree = S1_parq == S1_log)
}

#' Documents whose two Schema-2 labels are identical (degenerate duals)
#'
#' These are collapsed to a single label in cls_build_s2; this surfaces them.
#'
#' @return Tibble: DocID, Value (the repeated label).
#' @export
cls_self_duals <- function(.log) {
  w_ <- .log |>
    dplyr::filter(Class == "DocClass", Schema == 2) |>
    cls_latest() |>
    dplyr::select(DocID, Rank, Value) |>
    tidyr::pivot_wider(names_from = Rank, names_prefix = "v", values_from = Value)
  if (!"v2" %in% names(w_)) {
    return(tibble::tibble(DocID = character(), Value = character()))
  }
  w_ |>
    dplyr::filter(!is.na(v2), v2 == v1) |>
    dplyr::select(DocID, Value = v1)
}


# Source-coverage helper ------------------------------------------------------

#' One-row coverage summary of the S2 / S1 / final document sets
#' @export
cls_coverage <- function(.s2, .s1, .final) {
  tibble::tibble(
    n_s2      = nrow(.s2),
    n_s1      = nrow(.s1),
    n_both    = length(intersect(.s2$DocID, .s1$DocID)),
    n_s2_only = length(setdiff(.s2$DocID, .s1$DocID)),
    n_s1_only = length(setdiff(.s1$DocID, .s2$DocID)),
    n_final   = nrow(.final)
  )
}


# Plotting --------------------------------------------------------------------
# ggplot2 figures for the overview battery. Colorblind-safe Okabe-Ito palette.

#' Shared minimal theme for the classification figures
#' @export
cls_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor    = ggplot2::element_blank(),
      panel.grid.major.y  = ggplot2::element_blank(),
      plot.title.position = "plot"
    )
}

#' Bar chart: document count per final category, coloured by Level 1
#'
#' @param .final Output of cls_combine.
#' @return A ggplot object.
#' @export
cls_plot_distribution <- function(.final) {
  pal_ <- c(
    "Financial Instruments" = "#0072B2", "Employment"          = "#009E73",
    "Purchases and Sales"   = "#E69F00", "Business Structure"  = "#CC79A7",
    "Leases"                = "#56B4E9", "Licenses"            = "#D55E00",
    "Other"                 = "#999999"
  )
  .final |>
    dplyr::filter(!is.na(DocClassFinal1)) |>
    dplyr::count(Level1, DocClassFinal1, name = "n") |>
    ggplot2::ggplot(ggplot2::aes(n, forcats::fct_reorder(DocClassFinal1, n), fill = Level1)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.15, size = 3) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::scale_fill_manual(values = pal_) +
    ggplot2::labs(x = "Documents", y = NULL, fill = "Level 1") +
    cls_theme()
}

#' Stacked bar: provenance composition (new S2 vs mapped S1) per category
#'
#' Makes the "targeted re-labeling" point visible: how much of each category is
#' genuine Schema-2 work versus a Schema-1 label carried forward.
#'
#' @param .final Output of cls_combine.
#' @return A ggplot object.
#' @export
cls_plot_provenance <- function(.final) {
  pal_ <- c("S2" = "#0072B2", "S1_fallback" = "#999999", "UNRESOLVED" = "#D55E00")
  .final |>
    dplyr::filter(!is.na(DocClassFinal1)) |>
    dplyr::count(DocClassFinal1, Provenance, name = "n") |>
    ggplot2::ggplot(ggplot2::aes(
      n, forcats::fct_reorder(DocClassFinal1, n, .fun = sum), fill = Provenance
    )) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_fill_manual(values = pal_) +
    ggplot2::labs(x = "Documents", y = NULL, fill = "Provenance") +
    cls_theme()
}

#' 100 percent stacked bar: amendment composition per category (ordered)
#'
#' @param .final Output of cls_combine.
#' @return A ggplot object.
#' @export
cls_plot_amend <- function(.final) {
  pal_ <- c("Original" = "#56B4E9", "Amended" = "#E69F00")
  d_ <- .final |>
    dplyr::filter(!is.na(DocClassFinal1), !is.na(AmendType)) |>
    dplyr::count(DocClassFinal1, AmendType, name = "n")
  ord_ <- d_ |>
    dplyr::group_by(DocClassFinal1) |>
    dplyr::summarise(p_am = sum(n[AmendType == "Amended"]) / sum(n), .groups = "drop") |>
    dplyr::arrange(p_am) |>
    dplyr::pull(DocClassFinal1)
  d_ |>
    dplyr::mutate(
      DocClassFinal1 = factor(DocClassFinal1, levels = ord_),
      AmendType      = factor(AmendType, levels = c("Original", "Amended"))
    ) |>
    ggplot2::ggplot(ggplot2::aes(n, DocClassFinal1, fill = AmendType)) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::scale_x_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = pal_) +
    ggplot2::labs(x = "Share amended", y = NULL, fill = NULL) +
    cls_theme()
}

#' Heatmap: primary x secondary co-occurrence among dual-classified documents
#'
#' Rows are the primary category, columns the secondary. Tile shade and label
#' give the document count.
#'
#' @param .final Output of cls_combine.
#' @return A ggplot object.
#' @export
cls_plot_dual <- function(.final) {
  .final |>
    dplyr::filter(DualClass == 1L) |>
    dplyr::count(DocClassFinal1, DocClassFinal2, name = "n") |>
    ggplot2::ggplot(ggplot2::aes(DocClassFinal2, DocClassFinal1, fill = n)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = n), size = 3) +
    ggplot2::scale_fill_gradient(low = "#deebf7", high = "#08519c") +
    ggplot2::labs(x = "Secondary category", y = "Primary category", fill = "Docs") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 40, hjust = 1),
      panel.grid  = ggplot2::element_blank()
    )
}
