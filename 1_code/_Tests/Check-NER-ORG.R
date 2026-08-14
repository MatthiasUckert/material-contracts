# Check-NER-ORG: one parquet per engine, one contract, every test in one place ----
#
# WHAT THIS IS
# The acceptance check for ORG extraction. Twenty-five documents, every engine:model that emits
# ORG, one parquet each on disk, and a battery of tests over them. It exists to answer two
# questions before anything reaches the store: does each extractor honour the output contract, and
# what does each one actually give us that the others do not.
#
# THE OUTPUT CONTRACT
# Every extractor writes ONE parquet per run with a MANDATORY CORE:
#
#   DocID  Start  Stop  Span  Label  LabelRaw  Engine  Model
#
# Start and Stop are 0-based half-open code-point offsets, so text[Start:Stop] == Span. Label is
# the shared cross-engine vocabulary; LabelRaw is the engine's own tag. Engine and Model are stamped
# by the extractor and are constant within a file.
#
# On top of the core an extractor may write ANY EXTRA COLUMNS IT HAS. Nothing is required to invent
# a field it does not possess, and nothing is required to flatten a field it does. The R
# orchestrator reads the file, validates the core, and decides which extras the target table
# accepts. That is the whole seam: Python emits, R maps.
#
# WHAT THIS PASS EXPECTS TO FIND
#   lexnlp   core + six company fields, because CompanyAnnotation carries them
#   spacy    core and nothing else, for all four models, because doc.ents carries nothing else
#
# THAT ASYMMETRY IS A RESULT, NOT A GAP. It is the reason a researcher reaching for spaCy gets a
# different object from the same label, and it is reported rather than smoothed over.
#
# THE SENTINEL RULE
# Every input document appears in the output at least once. A document with no hits contributes one
# row with null Start/Stop/Span/Label. Without it the ledger cannot tell "found nothing" from "never
# ran", which is the distinction the whole resumability design rests on.
#
# The parquets are left on disk under 2_output/_Probe/Check-NER-ORG/out and are meant to be opened
# and read directly; this script tests them, it does not replace looking at them.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_04a     <- here::here("2_output", "04A-EntityExtract")
.path_text   <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys   <- fs::path(.dir_04a, "sample_anchors.parquet")
.path_store  <- fs::path(.dir_04a, "Store", "EntityCandidates.duckdb")

.dir_check   <- fs::dir_create(here::here("2_output", "_Probe", "Check-NER-ORG"))
.dir_in      <- fs::dir_create(fs::path(.dir_check, "in"))
.dir_out     <- fs::dir_create(fs::path(.dir_check, "out"))
.dir_lexnlp  <- here::here("contracts-lexnlp")
.dir_engine  <- here::here("contracts-engine")
.path_vocab  <- fs::path(.dir_lexnlp, "company_types.csv")

.n_docs      <- 25L
.len_min     <- 4000L
.len_max     <- 40000L
.timeout     <- 240L
# NULL = the whole document, which is what 04A used (.max_chars_extract is NULL there). If 04A is
# ever run with truncation, this MUST match it or block 6 reports drift that is really a difference
# in how much text each pass was given.
.max_chars   <- NULL
.image       <- "contracts-lexnlp"
.rerun       <- TRUE
.seed        <- 42L
.n_read      <- 3L      # documents whose every span is printed side by side

# One row per engine:model that emits ORG. Knobs mirror 04A: the transformer runs single-process
# because a single Metal device cannot be shared across worker processes.
tab_combo <- tibble::tribble(
  ~Engine,  ~Model,            ~Device, ~NProc, ~Batch,
  "lexnlp", "lexnlp",          NA,      20L,    8L,
  "spacy",  "en_core_web_sm",  "cpu",   10L,    64L,
  "spacy",  "en_core_web_md",  "cpu",   10L,    64L,
  "spacy",  "en_core_web_lg",  "cpu",   10L,    32L,
  "spacy",  "en_core_web_trf", "auto",  1L,     8L
) |>
  dplyr::mutate(
    Combo = dplyr::if_else(.data$Engine == .data$Model, .data$Engine,
                           paste0(.data$Engine, ":", .data$Model)),
    File  = paste0("ORG__", .data$Engine, "__", .data$Model, ".parquet"),
    Path  = as.character(fs::path(.dir_out, .data$File))
  )

.core <- c("DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model")

stopifnot(fs::file_exists(c(.path_text, .path_keys, .path_store)))
stopifnot(fs::file_exists(fs::path(.dir_lexnlp, "probe_lexnlp_org.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, "extract_spacy.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, ".venv", "bin", "python")))


# 2. The sample ----
# Stratified by contract type rather than drawn at random, so twenty-five documents cover the
# drafting conventions that differ -- a credit agreement, a stock plan and a lease do not fail the
# same way. Length-bounded so a parquet stays small enough to open and read.

tab_text <- arrow::read_parquet(.path_text) |>
  dplyr::mutate(DocLen = stringi::stri_length(.data$TextRaw))

tab_keys <- arrow::read_parquet(.path_keys) |>
  dplyr::select(DocID, Class = "ClassDetailed", CompanyName, AmendType)

tab_pick <- tab_text |>
  dplyr::select(DocID, DocLen) |>
  dplyr::inner_join(tab_keys, by = dplyr::join_by(DocID)) |>
  dplyr::filter(.data$DocLen >= .len_min, .data$DocLen <= .len_max) |>
  dplyr::arrange(.data$DocID) |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = 3L, by = Class)))() |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n_docs, nrow(.d)))))() |>
  dplyr::arrange(.data$Class, .data$DocID)

cli::cli_h2("The sample")
tab_pick |>
  dplyr::count(.data$Class, name = "Docs") |>
  print(n = Inf, width = Inf)
cli::cli_alert_info(
  "{nrow(tab_pick)} document{?s}, {dplyr::n_distinct(tab_pick$Class)} contract type{?s}, \\
   {format(min(tab_pick$DocLen), big.mark = ',')} to \\
   {format(max(tab_pick$DocLen), big.mark = ',')} characters."
)

tab_text |>
  dplyr::filter(.data$DocID %in% tab_pick$DocID) |>
  dplyr::select(DocID, TextRaw) |>
  arrow::write_parquet(fs::path(.dir_in, "sample.parquet"))


# 3. Run every engine ----
# LexNLP goes through the container with the entrypoint overridden, because the extra fields are
# still a probe. The four spaCy models go through the PRODUCTION extractor unmodified -- there is
# nothing to add to it, which is the finding this pass is partly here to record.

run_lexnlp_ <- function(.path) {
  args_ <- c(
    "run", "--rm",
    "-v", paste0(as.character(fs::path_real(.dir_in)), ":/work:ro"),
    "-v", paste0(as.character(fs::path_real(.dir_out)), ":/out"),
    "-v", paste0(as.character(fs::path_real(.dir_lexnlp)), ":/probe:ro"),
    "--entrypoint", "python",
    .image,
    "/probe/probe_lexnlp_org.py", "/work/sample.parquet",
    "--output", paste0("/out/", fs::path_file(.path)),
    "--n-process", 20L, "--chunk-size", 8L, "--timeout", as.integer(.timeout)
  )
  system2("docker", args_, stdout = "", stderr = "")
}

run_spacy_ <- function(.path, .model, .device, .nproc, .batch) {
  args_ <- c(
    fs::path(.dir_engine, "extract_spacy.py"),
    as.character(fs::path_abs(fs::path(.dir_in, "sample.parquet"))),
    "--output", as.character(.path),
    "--model", .model,
    "--device", .device,
    "--batch-size", as.integer(.batch),
    "--n-process", as.integer(.nproc),
    "--timeout", as.integer(.timeout),
    "--label", "ORG"          # the check is ORG only; other labels are separate passes
  )
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  system2(fs::path(.dir_engine, ".venv", "bin", "python"), args_, stdout = "", stderr = "")
}

purrr::pwalk(tab_combo, function(Engine, Model, Device, NProc, Batch, Combo, File, Path) {
  if (!.rerun && fs::file_exists(Path)) {
    cli::cli_alert_info("Reusing {(File)}.")
    return(invisible(NULL))
  }
  cli::cli_alert_info("Running {(Combo)} ...")
  t0_ <- Sys.time()
  status_ <- if (Engine == "lexnlp") {
    run_lexnlp_(.path = Path)
  } else {
    run_spacy_(.path = Path, .model = Model, .device = Device, .nproc = NProc, .batch = Batch)
  }
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("{(Combo)} failed ({status_}).")
  cli::cli_alert_success(
    "{(Combo)} in {round(as.numeric(difftime(Sys.time(), t0_, units = 'secs')), 1)}s."
  )
})


# 4. Inventory ----
# What is on disk, before any test runs. The column list is the point: it shows at a glance which
# engines carry extras and which do not.

lst_raw <- purrr::set_names(
  purrr::map(tab_combo$Path, \(.p) tibble::as_tibble(arrow::read_parquet(.p))),
  tab_combo$Combo
)

cli::cli_h2("Files on disk")
tab_combo |>
  dplyr::mutate(
    Rows    = purrr::map_int(lst_raw, nrow),
    KB      = round(as.numeric(fs::file_size(.data$Path)) / 1024, 1),
    NExtra  = purrr::map_int(lst_raw, \(.d) length(setdiff(names(.d), .core))),
    Extras  = purrr::map_chr(lst_raw, \(.d) paste(setdiff(names(.d), .core), collapse = ", "))
  ) |>
  dplyr::select(Combo, File, Rows, KB, NExtra, Extras) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "Open these directly at {.path {(.dir_out)}}. Everything below tests them; none of it replaces \\
   reading a few rows."
)


# 5. Tests ----
# Nine checks, run per file, each stated so that a failure names its own consequence. These are the
# contract: an extractor that passes all nine can be inserted into the store without inspection.

tab_len <- dplyr::select(tab_text, DocID, TextRaw, DocLen)

check_one_ <- function(.tab, .combo, .engine, .model) {
  cand_ <- dplyr::filter(.tab, !is.na(.data$Start))
  sent_ <- dplyr::filter(.tab, is.na(.data$Start))

  rt_ <- cand_ |>
    dplyr::left_join(tab_len, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      # 0-based half-open offsets over code points; stri_sub is 1-based inclusive.
      Sliced = stringi::stri_sub(.data$TextRaw, from = .data$Start + 1L, to = .data$Stop),
      OK     = .data$Sliced == .data$Span,
      InDoc  = .data$Start >= 0L & .data$Stop <= .data$DocLen & .data$Start < .data$Stop
    )

  tibble::tibble(
    Combo = .combo,
    Test  = c("T1 core columns present", "T2 every input document present",
              "T3 offsets round-trip to Span", "T4 offsets inside the document",
              "T5 Engine and Model constant and correct", "T6 Label is ORG on candidates",
              "T7 no duplicate DocID/Start/Stop/LabelRaw", "T8 sentinels fully null",
              "T9 extras absent on sentinel rows"),
    N     = c(
      sum(.core %in% names(.tab)),
      dplyr::n_distinct(.tab$DocID),
      sum(rt_$OK),
      sum(rt_$InDoc),
      sum(.tab$Engine == .engine & .tab$Model == .model),
      sum(cand_$Label == "ORG"),
      nrow(dplyr::distinct(cand_, .data$DocID, .data$Start, .data$Stop, .data$LabelRaw)),
      sum(is.na(sent_$Stop) & is.na(sent_$Span) & is.na(sent_$Label)),
      sum(purrr::map_int(seq_len(nrow(sent_)), function(.i) {
        ext_ <- setdiff(names(sent_), .core)
        if (length(ext_) == 0L) return(1L)
        as.integer(all(is.na(unlist(sent_[.i, ext_]))))
      }))
    ),
    Of    = c(length(.core), nrow(tab_pick), nrow(cand_), nrow(cand_), nrow(.tab),
              nrow(cand_), nrow(cand_), nrow(sent_), nrow(sent_))
  )
}

tab_tests <- purrr::pmap(
  .l = list(lst_raw, tab_combo$Combo, tab_combo$Engine, tab_combo$Model),
  .f = function(.d, .c, .e, .m) check_one_(.tab = .d, .combo = .c, .engine = .e, .model = .m)
) |>
  purrr::list_rbind()

cli::cli_h2("Contract tests")
tab_tests |>
  dplyr::mutate(Pass = .data$N == .data$Of) |>
  tidyr::pivot_wider(id_cols = Test, names_from = Combo, values_from = Pass) |>
  print(n = Inf, width = Inf)

if (any(!(tab_tests$N == tab_tests$Of))) {
  cli::cli_alert_danger("Failures below. Each one blocks insertion for that engine.")
  tab_tests |>
    dplyr::filter(.data$N != .data$Of) |>
    print(n = Inf, width = Inf)
} else {
  cli::cli_alert_success("All nine checks pass for all {nrow(tab_combo)} engine{?s}.")
}

cli::cli_alert_info(
  "T2 is the sentinel rule and it is the one worth staring at: a document missing entirely is \\
   indistinguishable in the ledger from one that was never run, which is what makes a resumed pass \\
   silently incomplete."
)


# 6. Does a fresh run reproduce the store? ----
# Reproducibility, not schema. The store was built by 04A weeks ago; these files were built now.
# Identical offsets mean the pipeline is deterministic and the store can be trusted as a cache.

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("ATTACH '", fs::path_abs(.path_store), "' AS s (READ_ONLY)"))

tab_store <- DBI::dbGetQuery(con, paste0(
  "SELECT CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END AS Combo, ",
  "  DocID, Start, Stop, Span FROM s.candidates ",
  "WHERE Label = 'ORG' AND Start IS NOT NULL AND DocID IN ('",
  paste(tab_pick$DocID, collapse = "', '"), "')"
)) |>
  tibble::as_tibble()

DBI::dbDisconnect(con, shutdown = TRUE)

tab_fresh <- purrr::imap(lst_raw, \(.d, .n) {
  dplyr::transmute(dplyr::filter(.d, !is.na(.data$Start)), Combo = .n, DocID, Start, Stop, Span)
}) |>
  purrr::list_rbind()

cli::cli_h2("Fresh run against 04A's store")
dplyr::full_join(
  dplyr::rename(tab_store, SpanStore = "Span"),
  dplyr::rename(tab_fresh, SpanFresh = "Span"),
  by = dplyr::join_by(Combo, DocID, Start, Stop)
) |>
  dplyr::summarise(
    InStore   = sum(!is.na(.data$SpanStore)),
    InFresh   = sum(!is.na(.data$SpanFresh)),
    Matched   = sum(!is.na(.data$SpanStore) & !is.na(.data$SpanFresh)),
    StoreOnly = sum(is.na(.data$SpanFresh)),
    FreshOnly = sum(is.na(.data$SpanStore)),
    TextDiff  = sum(!is.na(.data$SpanStore) & !is.na(.data$SpanFresh) &
                      .data$SpanStore != .data$SpanFresh),
    .by = Combo
  ) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "StoreOnly and FreshOnly should both be zero. Nonzero on a spaCy row would mean model or library \\
   drift since 04A ran; nonzero on lexnlp would mean the probe differs from the production \\
   extractor in more than the extra columns."
)


# 7. Yield ----

cli::cli_h2("Yield per engine")
purrr::imap(lst_raw, \(.d, .n) {
  cand_ <- dplyr::filter(.d, !is.na(.data$Start))
  tibble::tibble(
    Combo       = .n,
    Docs        = dplyr::n_distinct(cand_$DocID),
    NoHitDocs   = nrow(tab_pick) - dplyr::n_distinct(cand_$DocID),
    Spans       = nrow(cand_),
    PerDoc      = round(nrow(cand_) / max(1L, dplyr::n_distinct(cand_$DocID)), 1),
    DistinctSpans = dplyr::n_distinct(stringi::stri_trans_toupper(cand_$Span)),
    MedLen      = stats::median(stringi::stri_length(cand_$Span))
  )
}) |>
  purrr::list_rbind() |>
  print(n = Inf, width = Inf)


# 8. The legal form, applied to every engine ----
# LexNLP's pattern CANNOT FIRE without a legal form or a description, so requiring one is a no-op
# there. spaCy has no such requirement, so the same 142-alias vocabulary applied as a POST-FILTER is
# what a researcher reaching for spaCy would need in order to obtain a comparable object. This block
# measures how far that closes the gap, on identical documents.

tab_vocab <- readr::read_csv(.path_vocab, show_col_types = FALSE) |>
  dplyr::filter(!is.na(.data$Alias), nzchar(.data$Alias)) |>
  dplyr::distinct(.data$Alias, .keep_all = TRUE) |>
  dplyr::arrange(dplyr::desc(nchar(.data$Alias)))

# Every literal dot is made optional, because the vocabulary stores "N.A." and "L.P." while the text
# writes "N.A" and "L.P". That single omission accounted for the whole 11.3% miss in the earlier
# reproduction test.
esc_ <- function(.x) {
  .x |>
    stringi::stri_replace_all_regex("([\\\\^$|()\\[\\]{}*+?])", "\\\\$1") |>
    stringi::stri_replace_all_fixed(".", "\\.?")
}

# TWO VOCABULARIES, NOT ONE. LexNLP's pattern fires on a company TYPE or on a company DESCRIPTION,
# and the descriptions are how it catches banks and trusts. A filter built on the type list alone
# scored lexnlp at 84.8% rather than the ~100% its own precondition implies, and it did so by
# discarding exactly the bank and trust names -- the worst possible bias in a contracts corpus.
.descriptions <- c("Trust Bank", "Trust Company", "Trust", "Bank", "Company", "Partnership",
                   "Agency")
.alt_form <- paste(c(esc_(tab_vocab$Alias), esc_(.descriptions)), collapse = "|")

# NAME BEFORE FORM. A bare "Corporation" or "LLC" ends with a legal form and is not a company; the
# lg model returned "Corporation" fifty-four times. At least one further character is required
# before the form, so the test asks "a name carrying a form" rather than "a string ending in one".
.rgx_form <- paste0("\\S.*[\\s,\\.]\\b(", .alt_form, ")[\\.,\\s]*$")
.rgx_bare <- paste0("^[\\s,\\.]*\\b(", .alt_form, ")[\\.,\\s]*$")

.opt_ci <- stringi::stri_opts_regex(case_insensitive = TRUE)

flat_ <- function(.x) stringi::stri_trim_both(stringi::stri_replace_all_regex(.x, "\\s+", " "))

has_form_ <- function(.x) {
  stringi::stri_detect_regex(flat_(.x), .rgx_form, opts_regex = .opt_ci)
}

is_bare_form_ <- function(.x) {
  stringi::stri_detect_regex(flat_(.x), .rgx_bare, opts_regex = .opt_ci)
}

cli::cli_h2("Share of ORG spans carrying a legal form")
purrr::imap(lst_raw, \(.d, .n) {
  cand_ <- dplyr::filter(.d, !is.na(.data$Start))
  hf_   <- has_form_(.x = cand_$Span)
  tibble::tibble(
    Combo     = .n,
    Spans     = nrow(cand_),
    WithForm  = sum(hf_),
    PctForm   = round(100 * mean(hf_), 1),
    PerDocAll = round(nrow(cand_) / nrow(tab_pick), 1),
    PerDocKept = round(sum(hf_) / nrow(tab_pick), 1),
    BareForm   = sum(is_bare_form_(.x = cand_$Span))
  )
}) |>
  purrr::list_rbind() |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "PctForm near 100 on lexnlp is the pattern's own precondition showing up as a measurement. The \\
   spaCy rows are the interesting ones: PerDocKept against PerDocAll says how much of each model's \\
   output survives a requirement lexnlp imposes by construction."
)

cli::cli_h2("What the filter DROPS from spacy:en_core_web_lg")
lst_raw[["spacy:en_core_web_lg"]] |>
  dplyr::filter(!is.na(.data$Start), !has_form_(.x = .data$Span)) |>
  dplyr::count(.data$Span, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = 30, width = Inf)

cli::cli_h2("What the filter KEEPS from spacy:en_core_web_lg")
lst_raw[["spacy:en_core_web_lg"]] |>
  dplyr::filter(!is.na(.data$Start), has_form_(.x = .data$Span)) |>
  dplyr::count(.data$Span, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = 30, width = Inf)


# 9. The extras, where they exist ----

cli::cli_h2("LexNLP extras: availability")
lst_raw[["lexnlp"]] |>
  dplyr::filter(!is.na(.data$Start)) |>
  dplyr::summarise(dplyr::across(dplyr::any_of(
    c("Name", "NameAbbr", "TypeFull", "TypeAbbr", "TypeLabel", "Description")
  ), \(.x) round(100 * mean(!is.na(.x)), 1))) |>
  print(width = Inf)

cli::cli_h2("LexNLP extras: the legal-form vocabulary as used")
lst_raw[["lexnlp"]] |>
  dplyr::filter(!is.na(.data$Start)) |>
  dplyr::count(.data$TypeAbbr, .data$TypeLabel, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = 25, width = Inf)

cli::cli_h2("LexNLP extras: Description, as found and case-folded")
lst_raw[["lexnlp"]] |>
  dplyr::filter(!is.na(.data$Start), !is.na(.data$Description)) |>
  dplyr::count(
    AsFound = .data$Description,
    Folded  = stringi::stri_trans_toupper(.data$Description),
    name = "N"
  ) |>
  dplyr::arrange(.data$Folded, dplyr::desc(.data$N)) |>
  print(n = Inf, width = Inf)

cli::cli_h2("LexNLP extras: the \"NA\" collision")
tibble::tibble(
  Item = c("TypeAbbr genuinely missing", "TypeAbbr is the STRING \"NA\"",
           "Rows where the two are indistinguishable in print"),
  N = c(
    sum(is.na(lst_raw[["lexnlp"]]$TypeAbbr) & !is.na(lst_raw[["lexnlp"]]$Start)),
    sum(lst_raw[["lexnlp"]]$TypeAbbr %in% "NA"),
    sum(is.na(lst_raw[["lexnlp"]]$TypeAbbr) & !is.na(lst_raw[["lexnlp"]]$Start)) +
      sum(lst_raw[["lexnlp"]]$TypeAbbr %in% "NA")
  )
) |>
  print(n = Inf, width = Inf)

cli::cli_alert_danger(
  "NATIONAL ASSOCIATION ABBREVIATES TO \"NA\", WHICH R PRINTS EXACTLY LIKE A MISSING VALUE. Both \\
   states occur in this column, N.A. is the legal form of every national bank in the sample, and a \\
   CSV round-trip coerces the string to missing by default. The R mapping must rename it on ingest \\
   -- NATASSOC -- or carry TypeLabel instead, which is unambiguous."
)

cli::cli_h2("Extras that are never populated")
lst_raw |>
  purrr::imap(\\(.d, .n) {
    ext_ <- setdiff(names(.d), .core)
    if (length(ext_) == 0L) return(NULL)
    cand_ <- dplyr::filter(.d, !is.na(.data$Start))
    tibble::tibble(
      Combo = .n,
      Extra = ext_,
      PctPopulated = purrr::map_dbl(ext_, \\(.k) round(100 * mean(!is.na(cand_[[.k]])), 1))
    )
  }) |>
  purrr::list_rbind() |>
  dplyr::filter(.data$PctPopulated == 0) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "An extra populated in nothing is a column the store would carry empty forever. Drop it at the \\
   mapping, not at the extractor: the parquet stays a faithful record of what the engine offers."
)

cli::cli_alert_warning(
  "Description is NOT case-normalised by LexNLP: 'Bank' and 'BANK' are separate values. The parquet \\
   keeps it as found, which is right for a faithful record; the R mapping must fold it or every \\
   downstream group-by splits."
)


# 10. Read the same documents through every engine ----
# One table per document: every distinct span any engine proposed, with a presence string showing
# which engines proposed it. L = lexnlp, s/m/l/t = the four spaCy models, a dot where absent.

cli::cli_h2("The same documents, every engine")

tab_long <- purrr::imap(lst_raw, \(.d, .n) {
  dplyr::transmute(dplyr::filter(.d, !is.na(.data$Start)), Combo = .n, DocID, Start, Stop, Span)
}) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    # A named lookup rather than case_match(), which dplyr 1.2.0 deprecated in favour of
    # recode_values(); a vector index needs no version floor at all.
    Mark = dplyr::coalesce(
      unname(c("lexnlp" = "L", "spacy:en_core_web_sm" = "s", "spacy:en_core_web_md" = "m",
               "spacy:en_core_web_lg" = "l", "spacy:en_core_web_trf" = "t")[.data$Combo]),
      "?"
    )
  )

docs_read_ <- withr::with_seed(.seed, sample(tab_pick$DocID, size = .n_read))

purrr::walk(docs_read_, function(.d) {
  meta_ <- dplyr::filter(tab_pick, .data$DocID == .d)
  cli::cli_h3("{meta_$Class} | {meta_$CompanyName} | {format(meta_$DocLen, big.mark = ',')} chars")

  rows_ <- tab_long |>
    dplyr::filter(.data$DocID == .d) |>
    dplyr::summarise(
      Present = paste(
        purrr::map_chr(c("L", "s", "m", "l", "t"),
                       \(.k) if (.k %in% .data$Mark) .k else "."),
        collapse = ""
      ),
      Start = min(.data$Start),
      .by = c(Span)
    ) |>
    dplyr::mutate(Form = dplyr::if_else(has_form_(.x = .data$Span), "F", " ")) |>
    dplyr::arrange(.data$Start)

  cat(sprintf("  %7s %-5s %-4s %s\n", "Start", "Lsmlt", "Form", "Span"))
  purrr::walk(seq_len(nrow(rows_)), function(.i) {
    r_ <- rows_[.i, ]
    cat(sprintf("  %7d %-5s %-4s %s\n", r_$Start, r_$Present, r_$Form,
                stringi::stri_replace_all_regex(r_$Span, "\\s+", " ")))
  })
  cat("\n")
})

cli::cli_alert_info(
  "Read the presence column. A span only lexnlp found is a legal-form match the statistical models \\
   missed; a span only the spaCy models found is either a real company written without a form or a \\
   capitalised phrase that is not a company at all, and the F column is the cheapest way to tell \\
   those two apart."
)


# 11. Verdict ----

cli::cli_h2("Verdict")
tab_combo |>
  dplyr::mutate(
    Rows   = purrr::map_int(lst_raw, nrow),
    Extras = purrr::map_int(lst_raw, \(.d) length(setdiff(names(.d), .core))),
    Passed = purrr::map_lgl(tab_combo$Combo, \(.c) {
      t_ <- dplyr::filter(tab_tests, .data$Combo == .c)
      all(t_$N == t_$Of)
    })
  ) |>
  dplyr::select(Combo, File, Rows, Extras, Passed) |>
  print(n = Inf, width = Inf)

cli::cli_alert_success("Parquets for inspection: {.path {(.dir_out)}}")
