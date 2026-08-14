# Check-NER-MONEY: one parquet per engine, one contract, every test in one place ----
#
# WHAT THIS IS
# The acceptance check for MONEY extraction, built to the contract the ORG, GPE and DATE checks
# settled. Twenty-five documents, every engine:model that emits MONEY, one parquet each on disk,
# and a battery of tests over them.
#
# THE CONTRACT, UNCHANGED
#   Mandatory core   DocID  Start  Stop  Span  Label  LabelRaw  Engine  Model
#   Optional extras  whatever the engine has and nothing it does not
#
# MONEY IS THE LABEL WITH NO ANCHOR AT ALL
# ORG had the filer's name, GPE the registered address, DATE the filing date. EDGAR records no
# contract value, so nothing external can rank an engine here and 04A said so: no engine exceeds a
# positional contrast of 1.85, because money is genuinely distributed through a contract -- payment
# terms, caps, fees, schedules. The instrument that separated ORG and PERSON is silent by
# construction.
#
# SO THE EVIDENCE HAS TO COME FROM SOMEWHERE ELSE, and there are three sources:
#   AGREEMENT     two engines proposing the same amount at the same offsets, one by pattern and one
#                 by grammar, is corroboration that neither can manufacture alone.
#   REDACTION REACH  which engine proposes a span where the figure was WITHHELD. The design names
#                 this as the decisive capability test, and it is decisive because the amounts a
#                 filer withholds are systematically the commercially material ones -- a model with
#                 no number to tag finds nothing there, and a pattern requiring only the currency
#                 marker finds all of them.
#   SPAN QUALITY  what each engine calls money that carries no figure at all.
#
# WHAT CHANGED IN v5, AND WHY THE STORE COMPARISON MATTERS HERE
# moneyregex v4's European number alternative matched "0.000" inside "0.0001", so the trailing
# digit was dropped from the SPAN: "par value $0.0001 per share" -- boilerplate in these filings --
# was stored as $0.000. v5 admits European grouping only where it is unambiguous, and adds Amount
# and Currency. The first is a change to WHAT IS FOUND and forces the model bump; the second is not.
#
# The parquets are left on disk under 2_output/_Probe/Check-NER-MONEY/out and are meant to be opened.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_04a     <- here::here("2_output", "04A-EntityExtract")
.path_text   <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys   <- fs::path(.dir_04a, "sample_anchors.parquet")
.path_store  <- fs::path(.dir_04a, "Store", "EntityCandidates.duckdb")

.dir_check   <- fs::dir_create(here::here("2_output", "_Probe", "Check-NER-MONEY"))
.dir_in      <- fs::dir_create(fs::path(.dir_check, "in"))
.dir_out     <- fs::dir_create(fs::path(.dir_check, "out"))
.dir_lexnlp  <- here::here("contracts-lexnlp")
.dir_engine  <- here::here("contracts-engine")

.n_docs      <- 25L
.len_min     <- 4000L
.len_max     <- 40000L
.timeout     <- 240L
.max_chars   <- NULL   # must match 04A (.max_chars_extract is NULL there) or block 6 reports drift
.image       <- "contracts-lexnlp"
.rerun       <- TRUE
.seed        <- 42L    # the SAME seed as the other three checks: the same twenty-five documents
.n_read      <- 3L
.redact_win  <- 40L    # characters either side of a redaction marker that count as reaching it

tab_combo <- tibble::tribble(
  ~Engine,  ~Model,            ~Device, ~NProc, ~Batch,
  "lexnlp", "lexnlp",          NA,      20L,    8L,
  "paper",  "moneyregex-v6",   NA,      20L,    NA,
  "spacy",  "en_core_web_sm",  "cpu",   10L,    64L,
  "spacy",  "en_core_web_md",  "cpu",   10L,    64L,
  "spacy",  "en_core_web_lg",  "cpu",   10L,    32L,
  "spacy",  "en_core_web_trf", "auto",  1L,     8L
) |>
  dplyr::mutate(
    Combo = dplyr::if_else(.data$Engine == .data$Model, .data$Engine,
                           paste0(.data$Engine, ":", .data$Model)),
    # The store holds v4. Naming the predecessor turns block 6 from a broken comparison into the
    # measurement of what the number fix changed.
    StoreCombo = dplyr::if_else(.data$Model == "moneyregex-v6", "paper:moneyregex-v4",
                                .data$Combo),
    File  = paste0("MONEY__", .data$Engine, "__", .data$Model, ".parquet"),
    Path  = as.character(fs::path(.dir_out, .data$File))
  )

.core <- c("DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model")

stopifnot(fs::file_exists(c(.path_text, .path_keys, .path_store)))
stopifnot(fs::file_exists(fs::path(.dir_lexnlp, "probe_lexnlp_money.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, "extract_spacy.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, "extract_moneyregex.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, ".venv", "bin", "python")))


# 2. The sample ----

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
cli::cli_alert_info(
  "{nrow(tab_pick)} document{?s}, {dplyr::n_distinct(tab_pick$Class)} contract type{?s} -- the \\
   same draw the ORG, GPE and DATE checks used."
)

tab_text |>
  dplyr::filter(.data$DocID %in% tab_pick$DocID) |>
  dplyr::select(DocID, TextRaw) |>
  arrow::write_parquet(fs::path(.dir_in, "sample.parquet"))


# 3. Run every engine ----

run_lexnlp_ <- function(.path) {
  args_ <- c(
    "run", "--rm",
    "-v", paste0(as.character(fs::path_real(.dir_in)), ":/work:ro"),
    "-v", paste0(as.character(fs::path_real(.dir_out)), ":/out"),
    "-v", paste0(as.character(fs::path_real(.dir_lexnlp)), ":/probe:ro"),
    "--entrypoint", "python",
    .image,
    "/probe/probe_lexnlp_money.py", "/work/sample.parquet",
    "--output", paste0("/out/", fs::path_file(.path)),
    "--n-process", 20L, "--chunk-size", 8L, "--timeout", as.integer(.timeout)
  )
  system2("docker", args_, stdout = "", stderr = "")
}

run_regex_ <- function(.path, .nproc) {
  args_ <- c(
    fs::path(.dir_engine, "extract_moneyregex.py"),
    as.character(fs::path_abs(fs::path(.dir_in, "sample.parquet"))),
    "--output", as.character(.path),
    "--label", "MONEY",
    "--n-process", as.integer(.nproc),
    "--timeout", as.integer(.timeout)
  )
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  system2(fs::path(.dir_engine, ".venv", "bin", "python"), args_, stdout = "", stderr = "")
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
    "--label", "MONEY"
  )
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  system2(fs::path(.dir_engine, ".venv", "bin", "python"), args_, stdout = "", stderr = "")
}

# The dots absorb any column tab_combo gains later; pwalk() passes every column by name.
purrr::pwalk(tab_combo, function(Engine, Model, Device, NProc, Batch, Combo, File, Path, ...) {
  if (!.rerun && fs::file_exists(Path)) {
    cli::cli_alert_info("Reusing {(File)}.")
    return(invisible(NULL))
  }
  cli::cli_alert_info("Running {(Combo)} ...")
  t0_ <- Sys.time()
  status_ <- if (Engine == "lexnlp") {
    run_lexnlp_(.path = Path)
  } else if (Engine == "paper") {
    run_regex_(.path = Path, .nproc = NProc)
  } else {
    run_spacy_(.path = Path, .model = Model, .device = Device, .nproc = NProc, .batch = Batch)
  }
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("{(Combo)} failed ({status_}).")
  cli::cli_alert_success(
    "{(Combo)} in {round(as.numeric(difftime(Sys.time(), t0_, units = 'secs')), 1)}s."
  )
})


# 4. Inventory ----

lst_raw <- purrr::set_names(
  purrr::map(tab_combo$Path, \(.p) tibble::as_tibble(arrow::read_parquet(.p))),
  tab_combo$Combo
)

cli::cli_h2("Files on disk")
tab_combo |>
  dplyr::mutate(
    Rows   = purrr::map_int(lst_raw, nrow),
    KB     = round(as.numeric(fs::file_size(.data$Path)) / 1024, 1),
    NExtra = purrr::map_int(lst_raw, \(.d) length(setdiff(names(.d), .core))),
    Extras = purrr::map_chr(lst_raw, \(.d) paste(setdiff(names(.d), .core), collapse = ", "))
  ) |>
  dplyr::select(Combo, File, Rows, KB, NExtra, Extras) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info("Open these directly at {.path {(.dir_out)}}.")


# 5. Tests ----

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
              "T5 Engine and Model constant and correct", "T6 Label is MONEY on candidates",
              "T7 no duplicate DocID/Start/Stop/LabelRaw", "T8 sentinels fully null",
              "T9 extras absent on sentinel rows"),
    N     = c(
      sum(.core %in% names(.tab)),
      dplyr::n_distinct(.tab$DocID),
      sum(rt_$OK),
      sum(rt_$InDoc),
      sum(.tab$Engine == .engine & .tab$Model == .model),
      sum(cand_$Label == "MONEY"),
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


# 6. Does a fresh run reproduce the store? ----

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("ATTACH '", fs::path_abs(.path_store), "' AS s (READ_ONLY)"))

tab_store <- DBI::dbGetQuery(con, paste0(
  "SELECT CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END AS Combo, ",
  "  DocID, Start, Stop, Span FROM s.candidates ",
  "WHERE Label = 'MONEY' AND Start IS NOT NULL AND DocID IN ('",
  paste(tab_pick$DocID, collapse = "', '"), "')"
)) |>
  tibble::as_tibble()

# THE REDACTION SPANS COME FROM THE STORE, NOT FROM A REGEX WRITTEN HERE. The project has an
# extractor for this label -- paper:redaction-v1, five bracket classes, 60,547 spans in the sample
# store -- and re-deriving markers with a pattern of my own was how the first version of block 10
# came to find 66 markers in two documents that contain no money at all, while missing the
# unbracketed forms moneyregex itself matches.
tab_redact <- DBI::dbGetQuery(con, paste0(
  "SELECT DocID, Start AS MStart, Stop AS MStop, Span AS MSpan, LabelRaw AS MClass ",
  "FROM s.candidates WHERE Label = 'REDACT' AND Start IS NOT NULL AND DocID IN ('",
  paste(tab_pick$DocID, collapse = "', '"), "')"
)) |>
  tibble::as_tibble()

DBI::dbDisconnect(con, shutdown = TRUE)

tab_fresh <- purrr::imap(lst_raw, \(.d, .n) {
  dplyr::transmute(dplyr::filter(.d, !is.na(.data$Start)), Combo = .n, DocID, Start, Stop, Span)
}) |>
  purrr::list_rbind() |>
  dplyr::left_join(dplyr::select(tab_combo, Combo, StoreCombo), by = dplyr::join_by(Combo)) |>
  dplyr::mutate(Combo = .data$StoreCombo) |>
  dplyr::select(-StoreCombo)

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
  "EVERY ROW SHOULD BE ZERO EXCEPT moneyregex. The store holds v4 and the fresh run is v6, which \\
   folds in two changes: the sub-cent fix, where $0.000 becomes $0.0001, and the scale words, \\
   where NOK 23 becomes NOK 23 million. Both LENGTHEN an existing span rather than adding or \\
   removing one, so each appears as one StoreOnly and one FreshOnly sharing a Start, never as a \\
   TextDiff. The checkable claim is that StoreOnly equals FreshOnly and every pair shares a Start."
)

cli::cli_h2("What v6 changed against the stored v4")
dplyr::full_join(
  dplyr::rename(dplyr::filter(tab_store, .data$Combo == "paper:moneyregex-v4"),
                SpanV4 = "Span"),
  dplyr::rename(dplyr::filter(tab_fresh, .data$Combo == "paper:moneyregex-v4"),
                SpanV6 = "Span"),
  by = dplyr::join_by(Combo, DocID, Start)
) |>
  dplyr::filter(is.na(.data$SpanV4) | is.na(.data$SpanV6) | .data$SpanV4 != .data$SpanV6) |>
  dplyr::select(Start, SpanV4, SpanV6) |>
  dplyr::count(.data$SpanV4, .data$SpanV6, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = 25, width = Inf)


# 7. Yield, and the form split ----
# moneyregex writes the form into LabelRaw, which is what makes the arm auditable: an amount found
# by its currency symbol and one inferred from words are different kinds of evidence.

cli::cli_h2("Yield per engine")
purrr::imap(lst_raw, \(.d, .n) {
  cand_ <- dplyr::filter(.d, !is.na(.data$Start))
  tibble::tibble(
    Combo         = .n,
    Docs          = dplyr::n_distinct(cand_$DocID),
    NoHitDocs     = nrow(tab_pick) - dplyr::n_distinct(cand_$DocID),
    Spans         = nrow(cand_),
    PerDoc        = round(nrow(cand_) / max(1L, dplyr::n_distinct(cand_$DocID)), 1),
    DistinctSpans = dplyr::n_distinct(stringi::stri_trans_toupper(cand_$Span)),
    MedLen        = stats::median(stringi::stri_length(cand_$Span))
  )
}) |>
  purrr::list_rbind() |>
  print(n = Inf, width = Inf)

cli::cli_h2("LabelRaw, per engine")
purrr::imap(lst_raw, \(.d, .n) {
  dplyr::filter(.d, !is.na(.data$Start)) |>
    dplyr::count(Combo = .n, .data$LabelRaw, name = "N")
}) |>
  purrr::list_rbind() |>
  dplyr::arrange(.data$Combo, dplyr::desc(.data$N)) |>
  print(n = Inf, width = Inf)


# 8. Amount and currency, where they exist ----

tab_amt <- purrr::pmap(
  .l = list(lst_raw, tab_combo$Combo, tab_combo$Engine),
  .f = function(.d, .c, .e) {
    cand_ <- dplyr::filter(.d, !is.na(.data$Start))
    if (!all(c("Amount", "Currency") %in% names(cand_))) {
      cand_ <- dplyr::mutate(cand_, Amount = NA_character_, Currency = NA_character_)
    }
    dplyr::transmute(
      cand_, Combo = .c, DocID, Start, Stop, Span, LabelRaw,
      Amount = suppressWarnings(as.numeric(.data$Amount)),
      Currency
    )
  }
) |>
  purrr::list_rbind()

cli::cli_h2("Resolution rate, per engine")
tab_amt |>
  dplyr::summarise(
    Spans       = dplyr::n(),
    WithAmount  = sum(!is.na(.data$Amount)),
    WithCur     = sum(!is.na(.data$Currency)),
    PctAmount   = round(100 * mean(!is.na(.data$Amount)), 1),
    .by = Combo
  ) |>
  print(n = Inf, width = Inf)

cli::cli_h2("Currency mix")
tab_amt |>
  dplyr::filter(!is.na(.data$Currency)) |>
  dplyr::count(.data$Combo, .data$Currency, name = "N") |>
  dplyr::arrange(.data$Combo, dplyr::desc(.data$N)) |>
  print(n = 20, width = Inf)

# EVERY CURRENCY THAT IS NOT THE DOMESTIC ONE IS READ. Five NOK spans turned up in twenty-five US
# contracts, which is either a real foreign-denominated amount or the ISO alternation firing on
# something that is not a currency code at all -- (?<![A-Za-z])NOK is guarded on the left and not
# on the right, so "NOK" glued to a following token would still match. A currency list is exactly
# the kind of thing that looks settled until someone reads the hits.
cli::cli_h2("Non-USD spans, in context")
tab_amt |>
  dplyr::filter(!is.na(.data$Currency), .data$Currency != "USD") |>
  dplyr::left_join(dplyr::select(tab_text, DocID, TextRaw), by = dplyr::join_by(DocID)) |>
  dplyr::mutate(
    Context = stringi::stri_replace_all_regex(
      paste0(
        "...",
        stringi::stri_sub(.data$TextRaw, from = pmax(1L, .data$Start + 1L - 60L), to = .data$Start),
        " >>>", stringi::stri_sub(.data$TextRaw, from = .data$Start + 1L, to = .data$Stop), "<<< ",
        stringi::stri_sub(.data$TextRaw, from = .data$Stop + 1L, to = .data$Stop + 60L),
        "..."
      ),
      "\\s+", " "
    )
  ) |>
  dplyr::select(Combo, Currency, Amount, LabelRaw, Context) |>
  dplyr::arrange(.data$Currency, .data$Combo) |>
  print(n = 25, width = Inf)

cli::cli_h2("Magnitude of the parsed amounts")
tab_amt |>
  dplyr::filter(!is.na(.data$Amount)) |>
  dplyr::summarise(
    N    = dplyr::n(),
    Min  = min(.data$Amount),
    P25  = stats::quantile(.data$Amount, 0.25),
    Med  = stats::median(.data$Amount),
    P75  = stats::quantile(.data$Amount, 0.75),
    Max  = max(.data$Amount),
    .by = Combo
  ) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "A parse error in money is a factor of a thousand, not a rounding difference, so the quantiles \\
   are the cheapest place to see one. A median of a few thousand and a maximum in the hundreds of \\
   millions is what a contract corpus looks like; a maximum of thirty would be a scale word going \\
   unapplied."
)

cli::cli_h2("The largest and smallest amounts, read")
dplyr::bind_rows(
  dplyr::slice_max(dplyr::filter(tab_amt, !is.na(.data$Amount)), .data$Amount, n = 6L,
                   by = Combo, with_ties = FALSE),
  dplyr::slice_min(dplyr::filter(tab_amt, !is.na(.data$Amount)), .data$Amount, n = 4L,
                   by = Combo, with_ties = FALSE)
) |>
  dplyr::filter(.data$Combo %in% c("lexnlp", "paper:moneyregex-v6")) |>
  dplyr::select(Combo, Span, LabelRaw, Amount, Currency) |>
  dplyr::arrange(.data$Combo, dplyr::desc(.data$Amount)) |>
  print(n = 25, width = Inf)


# 9. Agreement on OVERLAPPING spans ----
# NOT ON IDENTICAL OFFSETS, WHICH IS WHY THE FIRST VERSION REPORTED ZERO. moneyregex includes the
# currency marker and spaCy starts one character later:
#
#     16486  .R....  $50,000
#     16487  ..sm.t   50,000
#
# The same amount, one character apart, and an exact-offset join can never see it. DATE could be
# joined that way because both engines bracketed the same characters; money cannot, because the
# marker is precisely what one engine includes and the other does not.
#
# Overlapping spans are therefore merged into clusters per document -- a new cluster starts where a
# span begins at or after the running maximum Stop -- and agreement is asked within a cluster.

tab_clust <- tab_amt |>
  dplyr::arrange(.data$DocID, .data$Start, .data$Stop) |>
  dplyr::mutate(
    Cluster = cumsum(.data$Start >= dplyr::lag(cummax(.data$Stop), default = -1L)),
    .by = DocID
  )

tab_agree <- tab_clust |>
  dplyr::summarise(
    NEngine = dplyr::n_distinct(.data$Combo),
    NAmount = dplyr::n_distinct(.data$Amount[!is.na(.data$Amount)]),
    NWithAmt = dplyr::n_distinct(.data$Combo[!is.na(.data$Amount)]),
    Spans   = paste(unique(.data$Span), collapse = " | "),
    .by = c(DocID, Cluster)
  )

cli::cli_h2("Clusters of overlapping spans")
tab_agree |>
  dplyr::summarise(
    Clusters      = dplyr::n(),
    MultiEngine   = sum(.data$NEngine > 1L),
    MultiWithAmt  = sum(.data$NWithAmt > 1L),
    Agree         = sum(.data$NWithAmt > 1L & .data$NAmount == 1L),
    Disagree      = sum(.data$NWithAmt > 1L & .data$NAmount > 1L)
  ) |>
  print(width = Inf)

cli::cli_alert_info(
  "MultiEngine counts clusters two or more engines both proposed; MultiWithAmt narrows that to \\
   clusters where at least two of them also produced a number. Only the second can disagree, \\
   because spaCy contributes spans and never amounts."
)

cli::cli_h2("How many engines proposed each cluster")
tab_agree |>
  dplyr::count(.data$NEngine, name = "Clusters") |>
  print(n = Inf, width = Inf)

if (any(tab_agree$NWithAmt > 1L & tab_agree$NAmount > 1L)) {
  cli::cli_alert_danger("Overlapping spans parsed to different amounts:")
  tab_clust |>
    dplyr::semi_join(
      dplyr::filter(tab_agree, .data$NWithAmt > 1L, .data$NAmount > 1L),
      by = dplyr::join_by(DocID, Cluster)
    ) |>
    dplyr::select(Combo, Span, LabelRaw, Amount, Currency) |>
    dplyr::arrange(.data$Span, .data$Combo) |>
    print(n = 30, width = Inf)
} else {
  cli::cli_alert_success("No cluster was parsed to two different amounts.")
}


# 10. Redaction reach ----
# THE CAPABILITY TEST THE DESIGN NAMES AS DECISIVE, and the reason is not subtle: the figures a
# filer withholds are systematically the commercially material ones, so an engine that cannot
# propose a span where the number is missing is blind to exactly the observations that matter.
# A model has nothing to tag; a pattern needs only the currency marker.
#
# The markers come from paper:redaction-v1 in the store, which is the project's own answer to
# "where was something withheld" and carries five bracket classes rather than one guess.

cli::cli_h2("Redaction markers in the sample")
cli::cli_alert_info(
  "{nrow(tab_redact)} marker{?s} across {dplyr::n_distinct(tab_redact$DocID)} document{?s}, from \\
   paper:redaction-v1."
)

if (nrow(tab_redact) > 0L) {
  tab_redact |>
    dplyr::count(.data$MClass, name = "N") |>
    dplyr::arrange(dplyr::desc(.data$N)) |>
    print(n = Inf, width = Inf)

  tab_reach <- tab_amt |>
    dplyr::inner_join(tab_redact, by = dplyr::join_by(DocID), relationship = "many-to-many") |>
    dplyr::filter(.data$Start <= .data$MStop + .redact_win,
                  .data$Stop >= .data$MStart - .redact_win)

  cli::cli_h2("Money spans reaching a redaction marker")
  tab_reach |>
    dplyr::summarise(
      SpansNearMarker = dplyr::n_distinct(paste(.data$DocID, .data$Start, .data$Stop)),
      MarkersReached  = dplyr::n_distinct(paste(.data$DocID, .data$MStart)),
      .by = Combo
    ) |>
    dplyr::mutate(PctMarkers = round(100 * .data$MarkersReached / nrow(tab_redact), 1)) |>
    dplyr::arrange(dplyr::desc(.data$MarkersReached)) |>
    print(n = Inf, width = Inf)

  cli::cli_alert_info(
    "MarkersReached is the number that decides this label. Everything else here ranks engines on \\
     amounts they can all see; this one counts the amounts only some of them can. An engine \\
     absent from this table reached no marker at all."
  )

  cli::cli_h2("What sits beside a marker, read")
  tab_reach |>
    dplyr::slice_head(n = 4L, by = Combo) |>
    dplyr::select(Combo, Span, LabelRaw, MSpan, MClass) |>
    print(n = 25, width = Inf)
}


# 11. What each engine calls MONEY that carries no figure ----
# The transformer tags "U.S. Dollars", "Dollars" and "the Dollar" as money, and none of them is an
# amount: "payable in U.S. Dollars" states a denomination. Requiring a figure separates the two.

has_figure_ <- function(.x) stringi::stri_detect_regex(.x, "\\d")

cli::cli_h2("Share of MONEY spans carrying a digit at all")
purrr::imap(lst_raw, \(.d, .n) {
  cand_ <- dplyr::filter(.d, !is.na(.data$Start))
  hf_   <- has_figure_(.x = cand_$Span)
  tibble::tibble(Combo = .n, Spans = nrow(cand_), WithFigure = sum(hf_),
                 PctFigure = round(100 * mean(hf_), 1))
}) |>
  purrr::list_rbind() |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "A span with no digit is not necessarily an error -- moneyregex emits '$[***]' deliberately -- \\
   so read this against LabelRaw. A redaction form with no digit is the point; a transformer span \\
   reading 'Dollars' is a denomination being counted as an amount."
)

cli::cli_h2("What spacy:en_core_web_trf calls MONEY with no digit")
lst_raw[["spacy:en_core_web_trf"]] |>
  dplyr::filter(!is.na(.data$Start), !has_figure_(.x = .data$Span)) |>
  dplyr::count(.data$Span, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = 20, width = Inf)


# 12. Read the same documents through every engine ----

tab_long <- purrr::imap(lst_raw, \(.d, .n) {
  dplyr::transmute(dplyr::filter(.d, !is.na(.data$Start)),
                   Combo = .n, DocID, Start, Stop, Span, LabelRaw)
}) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    Mark = dplyr::coalesce(
      unname(c("lexnlp" = "L", "paper:moneyregex-v6" = "R", "spacy:en_core_web_sm" = "s",
               "spacy:en_core_web_md" = "m", "spacy:en_core_web_lg" = "l",
               "spacy:en_core_web_trf" = "t")[.data$Combo]),
      "?"
    )
  )

tab_val <- tab_amt |>
  dplyr::filter(!is.na(.data$Amount)) |>
  dplyr::summarise(Val = format(dplyr::first(.data$Amount), big.mark = ",", scientific = FALSE),
                   .by = c(DocID, Span))

docs_read_ <- withr::with_seed(.seed, sample(tab_pick$DocID, size = .n_read))

cli::cli_h2("The same documents, every engine")
purrr::walk(docs_read_, function(.d) {
  meta_ <- dplyr::filter(tab_pick, .data$DocID == .d)
  cli::cli_h3("{meta_$Class} | {meta_$CompanyName}")

  rows_ <- tab_long |>
    dplyr::filter(.data$DocID == .d)

  # Two of the twenty-five documents carry no money at all, and summarise() on an empty group
  # warns from min() and returns Inf. A document with nothing in it is a fact about the sample,
  # not a condition to be handled downstream.
  if (nrow(rows_) == 0L) {
    cat("  no money spans in this document\n\n")
    return(invisible(NULL))
  }

  rows_ <- rows_ |>
    dplyr::summarise(
      Present = paste(
        purrr::map_chr(c("L", "R", "s", "m", "l", "t"),
                       \(.k) if (.k %in% .data$Mark) .k else "."),
        collapse = ""
      ),
      Form  = paste(sort(unique(.data$LabelRaw)), collapse = "/"),
      Start = min(.data$Start),
      .by = c(Span)
    ) |>
    dplyr::left_join(
      dplyr::select(dplyr::filter(tab_val, .data$DocID == .d), Span, Val),
      by = dplyr::join_by(Span)
    ) |>
    dplyr::arrange(.data$Start)

  cat(sprintf("  %7s %-6s %-16s %-16s %s\n", "Start", "LRsmlt", "Parsed", "Form", "Span"))
  purrr::walk(seq_len(nrow(rows_)), function(.i) {
    r_ <- rows_[.i, ]
    cat(sprintf("  %7d %-6s %-16s %-16s %s\n", r_$Start, r_$Present,
                dplyr::coalesce(r_$Val, "-"),
                stringi::stri_sub(r_$Form, from = 1L, to = 16L),
                stringi::stri_replace_all_regex(r_$Span, "\\s+", " ")))
  })
  cat("\n")
})


# 13. Verdict ----

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
