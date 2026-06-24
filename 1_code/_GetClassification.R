# =============================================================================
# Final contract classification (Schema 2)
# Combines the OLD Schema-1 ground truth with the NEW Schema-2 re-labeling,
# following category_overview (Jun 2026).
#
# SOURCES
#   Classifications.parquet   OLD / Schema 1: DocID, UserID, Timestamp, AmendType, DocClass
#   classification_data.db    NEW / Schema 2 log (table classification_log):
#                             doc_id, timestamp, schema, class, value
#
# COMBINATION RULE (per overview)
#   1. If a Schema-2 label exists, use it.
#   2. Else, if the old Schema-1 label is a name-change / unchanged category,
#      map it forward to its Schema-2 name (.s1_to_s2).
#   3. Else the old label is a SPLIT category (Inventory or Services -> R&D +
#      Customer / Supplier; M&A -> Business Structure: Peer / Investment).
#      No S1 fallback ("use S2 only"): the doc is marked UNRESOLVED and needs
#      an S2 label. Such docs are never silently kept.
#   AmendType (Original / Amended) comes from Schema 1 only.
#
# OUTPUT  final_  : one row per DocID
#   DocID, AmendType, DocClassFinal1, DocClassFinal2, DualClass,
#   Level1, Level2, Provenance (S2 / S1_fallback / UNRESOLVED), DocClassS1
#
# HOW TO JUDGE
#   Section 7 = hard guards (abort on drift). Section 8 = printed overviews to
#   sanity-check coverage, label dispositions, the hierarchy distribution,
#   dual classifications, fallbacks, and the unresolved docs. Section 9 = the
#   one-time parquet-vs-log S1 source check.
# =============================================================================


# 1. Paths --------------------------------------------------------------------
# Add your own path to each vector. The first one that exists is used, so you
# can both keep your paths here and just run the script.
.paths_parq <- c(
  "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractData/Classification/ClassifyContracts/Classification/Classifications.parquet",
  ""   # <- Ann-Kristin: your Classifications.parquet path here
)
.paths_db <- c(
  "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MaterialContractClassification/DocumentClassification/01-Classification/classification_data.db",
  ""   # <- Ann-Kristin: your classification_data.db path here
)

# Pick the first candidate that exists (empties ignored); abort if none resolve.
pick_path <- function(.paths, .what) {
  paths_ <- .paths[nzchar(.paths)]
  hit_   <- paths_[fs::file_exists(paths_)]
  if (length(hit_) == 0L) {
    cli::cli_abort("No existing path found for {(.what)}. Checked: {paths_}")
  }
  cli::cli_inform("Using {(.what)}: {hit_[[1]]}")
  hit_[[1]]
}

.path_parq <- pick_path(.paths_parq, "Classifications.parquet")
.path_db   <- pick_path(.paths_db, "classification_data.db")


# 2. Canonical taxonomy + relabel maps (single source of truth) ---------------

# 2a. Canonical Schema-2 taxonomy: the 12 leaves, with Level 1 / Level 2.
.s2_taxonomy <- tibble::tribble(
  ~Level1,                  ~Level2,                 ~DocClass,
  "Financial Instruments",  "Credit",                "Financial Instruments: Credit",
  "Financial Instruments",  "Equity",                "Financial Instruments: Equity",
  "Leases",                 NA_character_,           "Leases",
  "Employment",             "Compensation",          "Employment: Compensation",
  "Employment",             "Legal",                 "Employment: Legal",
  "Purchases and Sales",    "Assets",                "Purchases and Sales: Assets",
  "Purchases and Sales",    "R&D",                   "R&D",
  "Purchases and Sales",    "Customer / Supplier",   "Customer / Supplier",
  "Licenses",               NA_character_,           "Licenses",
  "Other",                  NA_character_,           "Other",
  "Business Structure",     "Peer Agreements",       "Business Structure: Peer Agreements",
  "Business Structure",     "Investment and Merger", "Business Structure: Investment and Merger"
)

# 2b. Raw S2 label -> canonical. Accumulator for case / spacing / redundancy
#     drift in what was typed in the labeling app. Guard 7a feeds new rows here.
.s2_relabel <- tibble::tribble(
  ~DocClassRaw,                                        ~DocClass,
  "Purchases and Sales: Purchases and Sales-Assets",   "Purchases and Sales: Assets",
  "Business structure: Investment and merger",         "Business Structure: Investment and Merger",
  "Business structure: Peer Agreements",               "Business Structure: Peer Agreements"
)

# 2c. Old S1 -> S2 forward map: name-change + unchanged categories ONLY.
#     Deliberately NOT mapped (use S2 only -> UNRESOLVED if no S2):
#       "Purchases/Sales: Inventory or Services", "MergerAcquisition: M&A Category"
.s1_to_s2 <- tibble::tribble(
  ~DocClassS1,                  ~DocClass,
  "Purchases/Sales: Assets",    "Purchases and Sales: Assets",
  "Other: Other Category",      "Other",
  "License: License Category",  "Licenses",
  "Leases: Leases Category",    "Leases",
  "Fin. Instruments: Debt",     "Financial Instruments: Credit",
  "Fin. Instruments: Equity",   "Financial Instruments: Equity",
  "Empoyment: Compensation",    "Employment: Compensation",   # source typo "Empoyment"
  "Empoyment: Legal",           "Employment: Legal"
)


# 3. Helper: latest session per (DocID, Class, Schema), rank values within -----
cls_latest <- function(.data) {
  .data |>
    dplyr::group_by(DocID, Class, Schema) |>
    dplyr::filter(TimeStamp == max(TimeStamp)) |>
    dplyr::arrange(DocID, Class, Schema, Value) |>
    dplyr::mutate(Rank = dplyr::row_number()) |>
    dplyr::ungroup()
}


# 4. NEW Schema-2 labels from the log (latest session; dual-class aware) -------
con_ <- DBI::dbConnect(RSQLite::SQLite(), .path_db)
log_raw_ <- dplyr::tbl(con_, "classification_log") |>
  dplyr::select(DocID = doc_id, TimeStamp = timestamp, Schema = schema,
                Class = class, Value = value) |>
  dplyr::collect()
DBI::dbDisconnect(con_)
log_raw_ <- log_raw_ |> dplyr::filter(Value != "--SKIP DOCUMENT--")

s2_ <- log_raw_ |>
  dplyr::filter(Class == "DocClass", Schema == 2) |>
  cls_latest() |>
  dplyr::left_join(.s2_relabel, by = c("Value" = "DocClassRaw")) |>
  dplyr::mutate(DocClass = dplyr::coalesce(DocClass, Value)) |>
  dplyr::select(DocID, Rank, DocClass) |>
  tidyr::pivot_wider(names_from = Rank, names_prefix = "S2_v", values_from = DocClass)
if (!"S2_v2" %in% names(s2_)) s2_$S2_v2 <- NA_character_   # ensure secondary col exists


# 5. OLD Schema-1 labels + AmendType from the parquet (latest row per doc) -----
parq_ <- arrow::read_parquet(.path_parq, mmap = FALSE) |>
  dplyr::filter(DocClass != "--SKIP DOCUMENT--") |>
  dplyr::group_by(DocID) |>
  dplyr::slice_max(Timestamp, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(DocID, AmendType, DocClassS1 = DocClass)

s1_ <- parq_ |>
  dplyr::left_join(.s1_to_s2, by = "DocClassS1") |>
  dplyr::rename(DocClassS1Mapped = DocClass)


# 6. Combine: S2 wins; else mapped S1; else UNRESOLVED ------------------------
final_ <- s2_ |>
  dplyr::full_join(s1_, by = "DocID") |>
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
    .s2_taxonomy |> dplyr::select(DocClassFinal1 = DocClass, Level1, Level2),
    by = "DocClassFinal1"
  ) |>
  dplyr::select(
    DocID, AmendType, DocClassFinal1, DocClassFinal2, DualClass,
    Level1, Level2, Provenance, DocClassS1
  )


# 7. Validation guards (hard: abort on drift) ---------------------------------

# 7a. Final labels must be exactly the 12 canonical S2 leaves.
bad_ <- setdiff(
  unique(stats::na.omit(c(final_$DocClassFinal1, final_$DocClassFinal2))),
  .s2_taxonomy$DocClass
)
if (length(bad_) > 0L) cli::cli_abort("Non-canonical final labels: {bad_}")

# 7b. One row per DocID.
if (anyDuplicated(final_$DocID) > 0L) cli::cli_abort("Duplicate DocID in final_")


# 8. Overviews (judge by eye) -------------------------------------------------
# Raw S2 label counts, reused below.
raw_s2_counts_ <- log_raw_ |>
  dplyr::filter(Class == "DocClass", Schema == 2) |>
  cls_latest() |>
  dplyr::count(Value, name = "n_raw")

ovw_ <- list(
  
  # Source overlap: do the join sizes make sense? Are docs being lost?
  coverage = tibble::tibble(
    n_s2      = nrow(s2_),
    n_s1      = nrow(s1_),
    n_both    = length(intersect(s2_$DocID, s1_$DocID)),
    n_s2_only = length(setdiff(s2_$DocID, s1_$DocID)),
    n_s1_only = length(setdiff(s1_$DocID, s2_$DocID)),
    n_final   = nrow(final_)
  ),
  
  # Every raw S2 label + count + disposition. UNKNOWN = guard 7a would abort.
  raw_labels = raw_s2_counts_ |>
    dplyr::mutate(
      Disposition = dplyr::case_when(
        Value %in% .s2_taxonomy$DocClass   ~ "canonical",
        Value %in% .s2_relabel$DocClassRaw ~ "relabeled",
        TRUE                               ~ "UNKNOWN"
      )
    ) |>
    dplyr::arrange(Disposition, dplyr::desc(n_raw)),
  
  # Where each final label came from.
  provenance = final_ |> dplyr::count(Provenance),
  
  # The headline distribution by hierarchy (Table 1 view). NA row = unresolved.
  by_class = final_ |> dplyr::count(Level1, Level2, DocClassFinal1),
  
  # Dual-classified docs and their primary/secondary pairs.
  dual = final_ |>
    dplyr::filter(DualClass == 1L) |>
    dplyr::count(DocClassFinal1, DocClassFinal2),
  
  # AmendType distribution. NA = S2-only docs absent from the old parquet.
  amend = final_ |> dplyr::count(AmendType),
  
  # Which old S1 labels actually fell back, and to what (forward map firing).
  s1_fallback = final_ |>
    dplyr::filter(Provenance == "S1_fallback") |>
    dplyr::count(DocClassS1, DocClassFinal1),
  
  # Split-category docs with no S2 label -> need a decision (your old n=4 etc).
  unresolved = final_ |>
    dplyr::filter(Provenance == "UNRESOLVED") |>
    dplyr::count(DocClassS1)
)

for (nm_ in names(ovw_)) {
  cli::cli_h2(nm_)
  print(ovw_[[nm_]], n = Inf)
}

# Per-doc list of the unresolved docs, for actioning.
unresolved_docs_ <- final_ |>
  dplyr::filter(Provenance == "UNRESOLVED") |>
  dplyr::select(DocID, DocClassS1, AmendType)


# 9. One-time check: does the log's own Schema-1 agree with the parquet S1? ----
# Confirms the parquet is the right S1 source. Drop this block once settled.
log_s1_ <- log_raw_ |>
  dplyr::filter(Class == "DocClass", Schema == 1) |>
  cls_latest() |>
  dplyr::filter(Rank == 1) |>
  dplyr::select(DocID, S1_log = Value)

recon_ <- s1_ |>
  dplyr::select(DocID, S1_parq = DocClassS1) |>
  dplyr::full_join(log_s1_, by = "DocID") |>
  dplyr::mutate(Agree = S1_parq == S1_log)

recon_ |> dplyr::count(Agree)                          # ideally all TRUE
recon_ |> dplyr::filter(!Agree | is.na(Agree))         # the disagreements / one-sided


# 10. (optional) Persist final classification ---------------------------------
if (FALSE) {
  .path_out <- fs::path(dirname(.path_parq), "Classifications-Final-S2.parquet")
  arrow::write_parquet(final_, .path_out)
  haven::write_dta(final_, fs::path_ext_set(.path_out, "dta"))   # if Stata needs it
}