# 04A-RefineNER -- script-specific helpers ---------------------------------------------------------------------------
# Sourced by 04A-RefineNER.qmd after _NER.R (via .path_fun), so the ner_* library is
# already loaded when this runs. Keep only glue specific to THIS analysis here; the
# reusable machinery (ner_profile_by_class, ner_db_clear, ner_plot_profile, ...) lives
# in _NER.R next to ner_overview / ner_alignment.


# One-call per-class profile for this script. Runs ner_profile_by_class with the
# classification sample as the label source, prints the headline tables (coverage,
# per-type doc counts, the reference-engine fingerprint, and -- when .mentions is
# supplied -- the high-confidence cross-engine rate), and returns the full result list
# invisibly so downstream chunks can drill into $profile / $consensus / $docs.
refine_profile <- function(.db_path, .class_parquet,
                           .class_col = "ClassDetailed",
                           .ref_combo = "spacy:en_core_web_trf",
                           .mentions = NULL,
                           .min_combos = 2L) {
  if (FALSE) {
    .db_path <- .lP$Cache$NerDB
    .class_parquet <- .lP$Input$ClassificationSample
    .class_col <- "ClassDetailed"
    .ref_combo <- "spacy:en_core_web_trf"
    .mentions <- .al$mentions
    .min_combos <- 2L
  }

  prof_ <- ner_profile_by_class(
    .db_path       = .db_path,
    .class_parquet = .class_parquet,
    .class_col     = .class_col,
    .ref_combo     = .ref_combo,
    .mentions      = .mentions,
    .min_combos    = .min_combos
  )

  cli::cli_h2("Class coverage")
  print(prof_$coverage)

  cli::cli_h2("Docs per contract type")
  print(prof_$docs)

  cli::cli_h2("Fingerprint -- candidates per doc ({(.ref_combo)})")
  print(prof_$fingerprint)

  if (!is.null(prof_$consensus)) {
    cli::cli_h2("High-confidence rate -- >= {(.min_combos)} engines agree")
    print(prof_$consensus)
  }

  return(invisible(prof_))
}
