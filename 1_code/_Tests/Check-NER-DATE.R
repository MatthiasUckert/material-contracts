# Check-NER-DATE: one parquet per engine, one contract, every test in one place ----
#
# WHAT THIS IS
# The acceptance check for DATE extraction, built to the contract Check-NER-ORG settled and
# Check-NER-GPE confirmed. Twenty-five documents, every engine:model that emits DATE, one parquet
# each on disk, and a battery of tests over them.
#
# THE CONTRACT, UNCHANGED
#   Mandatory core   DocID  Start  Stop  Span  Label  LabelRaw  Engine  Model
#   Optional extras  whatever the engine has and nothing it does not
#
# DATE DIFFERS FROM THE OTHER TWO IN THREE WAYS
#
# 1. THE EXTRA IS A VALUE, NOT A DESCRIPTION. LexNLP's DateAnnotation carries a PARSED calendar
#    date and a confidence score. ORG's extras described a span and GPE's resolved one; this one
#    replaces the span with a number a regression can subtract. It is also the field the expiry
#    problem needs: 04B reports expiry cues firing thousands of times while 04C yields about twenty
#    documents, and a start date and an end date are the same LABEL under different ROLES, which no
#    string can distinguish and arithmetic can.
#
# 2. THE PAPER'S OWN ENGINE ALREADY SAYS WHICH PATTERN FIRED. dateregex writes the pattern name --
#    ISO, Slash, European, Text, MonthYear -- into LabelRaw. That is enough to parse the span in R
#    DETERMINISTICALLY, so it needs no probe, exactly as the gazetteer's GeoClass did not. What it
#    cannot do is resolve a convention: "1/2/2020" is January 2nd in a US filing and February 1st in
#    a European one, and the pattern name does not say which. That ambiguity is MEASURED here rather
#    than assumed away.
#
# 3. spaCy's DATE IS NOT A DATE. The label covers durations and references -- "thirty (30) days",
#    "the Effective Date", "one year" -- and none of those denotes a calendar date. A block below
#    counts how much of each model's output is a date at all, because that number decides whether
#    spaCy is a candidate source for this label or only corroboration.
#
# The parquets are left on disk under 2_output/_Probe/Check-NER-DATE/out and are meant to be opened.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_04a     <- here::here("2_output", "04A-EntityExtract")
.path_text   <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys   <- fs::path(.dir_04a, "sample_anchors.parquet")
.path_store  <- fs::path(.dir_04a, "Store", "EntityCandidates.duckdb")

.dir_check   <- fs::dir_create(here::here("2_output", "_Probe", "Check-NER-DATE"))
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
.seed        <- 42L    # the SAME seed as the ORG and GPE checks: the same twenty-five documents
.n_read      <- 3L

# A contract signed today is filed within days or weeks; one signed years ago is an amendment to an
# older agreement. Two years back is the window 04A's design uses for the DATE anchor, kept here so
# the two agree, and a year forward because a preamble can date an agreement ahead of its filing.
.win_back    <- 730L
.win_fwd     <- 365L

tab_combo <- tibble::tribble(
  ~Engine,  ~Model,            ~Device, ~NProc, ~Batch,
  "lexnlp", "lexnlp",          NA,      20L,    8L,
  "paper",  "dateregex-v2",    NA,      20L,    NA,
  "spacy",  "en_core_web_sm",  "cpu",   10L,    64L,
  "spacy",  "en_core_web_md",  "cpu",   10L,    64L,
  "spacy",  "en_core_web_lg",  "cpu",   10L,    32L,
  "spacy",  "en_core_web_trf", "auto",  1L,     8L
) |>
  dplyr::mutate(
    Combo = dplyr::if_else(.data$Engine == .data$Model, .data$Engine,
                           paste0(.data$Engine, ":", .data$Model)),
    # THE STORE STILL HOLDS dateregex-v1. Bumping the model is the convention for a pattern change,
    # so the two carry different combo strings and would not join at all -- which would report the
    # whole engine as missing rather than as changed. Naming the predecessor turns block 6 from a
    # broken comparison into the measurement of what v2 adds.
    StoreCombo = dplyr::if_else(.data$Model == "dateregex-v2", "paper:dateregex-v1", .data$Combo),
    File  = paste0("DATE__", .data$Engine, "__", .data$Model, ".parquet"),
    Path  = as.character(fs::path(.dir_out, .data$File))
  )

.core <- c("DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model")

stopifnot(fs::file_exists(c(.path_text, .path_keys, .path_store)))
stopifnot(fs::file_exists(fs::path(.dir_lexnlp, "probe_lexnlp_date.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, "extract_spacy.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, "extract_dateregex.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, ".venv", "bin", "python")))


# 2. The sample ----

tab_text <- arrow::read_parquet(.path_text) |>
  dplyr::mutate(DocLen = stringi::stri_length(.data$TextRaw))

tab_keys <- arrow::read_parquet(.path_keys) |>
  dplyr::select(DocID, Class = "ClassDetailed", CompanyName, AmendType, DateFiled)

tab_pick <- tab_text |>
  dplyr::select(DocID, DocLen) |>
  dplyr::inner_join(tab_keys, by = dplyr::join_by(DocID)) |>
  dplyr::filter(.data$DocLen >= .len_min, .data$DocLen <= .len_max) |>
  dplyr::arrange(.data$DocID) |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = 3L, by = Class)))() |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n_docs, nrow(.d)))))() |>
  dplyr::arrange(.data$Class, .data$DocID) |>
  dplyr::mutate(DateFiled = as.Date(suppressWarnings(anytime::anydate(as.character(.data$DateFiled)))))

cli::cli_h2("The sample")
cli::cli_alert_info(
  "{nrow(tab_pick)} document{?s}, {dplyr::n_distinct(tab_pick$Class)} contract type{?s}, filed \\
   {format(min(tab_pick$DateFiled, na.rm = TRUE))} to \\
   {format(max(tab_pick$DateFiled, na.rm = TRUE))} -- the same draw the ORG and GPE checks used."
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
    "/probe/probe_lexnlp_date.py", "/work/sample.parquet",
    "--output", paste0("/out/", fs::path_file(.path)),
    "--n-process", 20L, "--chunk-size", 8L, "--timeout", as.integer(.timeout)
  )
  system2("docker", args_, stdout = "", stderr = "")
}

run_regex_ <- function(.path, .nproc) {
  args_ <- c(
    fs::path(.dir_engine, "extract_dateregex.py"),
    as.character(fs::path_abs(fs::path(.dir_in, "sample.parquet"))),
    "--output", as.character(.path),
    "--label", "DATE",
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
    "--label", "DATE"
  )
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  system2(fs::path(.dir_engine, ".venv", "bin", "python"), args_, stdout = "", stderr = "")
}

# The dots absorb any column tab_combo gains later. pwalk() passes EVERY column by name, so a
# signature that lists them exhaustively breaks the moment one is added -- which is exactly what
# StoreCombo did.
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
              "T5 Engine and Model constant and correct", "T6 Label is DATE on candidates",
              "T7 no duplicate DocID/Start/Stop/LabelRaw", "T8 sentinels fully null",
              "T9 extras absent on sentinel rows"),
    N     = c(
      sum(.core %in% names(.tab)),
      dplyr::n_distinct(.tab$DocID),
      sum(rt_$OK),
      sum(rt_$InDoc),
      sum(.tab$Engine == .engine & .tab$Model == .model),
      sum(cand_$Label == "DATE"),
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
  "WHERE Label = 'DATE' AND Start IS NOT NULL AND DocID IN ('",
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
  "EVERY ROW SHOULD BE ZERO EXCEPT dateregex. The store holds v1 and the fresh run is v2, so \\
   FreshOnly counts what the new patterns find. StoreOnly is NOT required to be zero: a longer \\
   match wins the overlap resolution, so v1's 'November 2007' is displaced by v2's '26th day of \\
   November 2007' at an earlier offset and the v1 span disappears. The checkable claim is that \\
   StoreOnly equals the fall in MonthYear between the two versions -- a lost span must have been \\
   swallowed by a longer one, never simply dropped."
)

cli::cli_h2("What v2 finds that v1 did not")
tab_v2_ <- dplyr::anti_join(
  dplyr::filter(tab_fresh, .data$Combo == "paper:dateregex-v1"),
  tab_store,
  by = dplyr::join_by(Combo, DocID, Start, Stop)
) |>
  dplyr::left_join(
    dplyr::select(dplyr::filter(lst_raw[["paper:dateregex-v2"]], !is.na(.data$Start)),
                  DocID, Start, Stop, LabelRaw, DateValue),
    by = dplyr::join_by(DocID, Start, Stop)
  )

tab_v2_ |>
  dplyr::count(.data$LabelRaw, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = Inf, width = Inf)

tab_v2_ |>
  dplyr::slice_head(n = 4L, by = LabelRaw) |>
  dplyr::select(LabelRaw, Span, DateValue, Start) |>
  dplyr::arrange(.data$LabelRaw) |>
  print(n = 30, width = Inf)


# 7. Yield, and the pattern split ----
# dateregex writes the pattern name into LabelRaw, which is the only engine here that says HOW it
# matched. That column is what makes the R-side parse deterministic in the next block.

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


# 8. The lexnlp extras ----

if ("DateValue" %in% names(lst_raw[["lexnlp"]])) {
  cand_lex_ <- dplyr::filter(lst_raw[["lexnlp"]], !is.na(.data$Start))

  cli::cli_h2("LexNLP extras: availability")
  tibble::tibble(
    Item = c("Candidates", "With a parsed date", "With a score", "Found but NOT parsed"),
    N    = c(nrow(cand_lex_), sum(!is.na(cand_lex_$DateValue)), sum(!is.na(cand_lex_$Score)),
             sum(is.na(cand_lex_$DateValue)))
  ) |>
    dplyr::mutate(Pct = round(100 * .data$N / nrow(cand_lex_), 1)) |>
    print(n = Inf, width = Inf)

  cli::cli_alert_info(
    "'Found but NOT parsed' is a third state beyond found and not found: the extractor located a \\
     date expression and could not resolve it to a calendar date. Dropping those rows would report \\
     a recall the engine did not achieve."
  )

  cli::cli_h2("LexNLP extras: the score, and whether it separates anything")
  cand_lex_ |>
    dplyr::mutate(Parsed = !is.na(.data$DateValue)) |>
    dplyr::summarise(
      N       = dplyr::n(),
      MinScore = min(.data$Score, na.rm = TRUE),
      MedScore = stats::median(.data$Score, na.rm = TRUE),
      MaxScore = max(.data$Score, na.rm = TRUE),
      .by = Parsed
    ) |>
    print(width = Inf)

  cli::cli_h2("LexNLP extras: spans it could not parse")
  cand_lex_ |>
    dplyr::filter(is.na(.data$DateValue)) |>
    dplyr::count(.data$Span, name = "N") |>
    dplyr::arrange(dplyr::desc(.data$N)) |>
    print(n = 15, width = Inf)
}


# 9. Resolve every engine to a calendar date ----
# BOTH RULE ENGINES NOW SHIP A PARSED VALUE and spaCy still ships none, so this block reads two
# columns and invents nothing. That is the change worth noting: the parse is no longer performed
# here, it is performed where the pattern is defined, and this block only checks it.
#
# THE CONVENTION IS STILL REPORTED. "1/2/2020" is January 2nd under the US reading and February 1st
# under the European one; extract_dateregex.py chooses month-first and says so in its own header.
# Every span where both readings are calendar-valid and differ is flagged here, so the assumption
# reaches a number rather than a footnote.

.months <- c("JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE",
             "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER")

# THE R-SIDE PATTERN PARSER IS GONE. It reproduced, in another language and three hundred lines
# away, a mapping that belongs beside the regexes it derives from: each of the eight patterns
# determines exactly one reading of its own text. extract_dateregex.py now carries the format in
# PATTERNS and emits DateValue, so a ninth pattern added without one is a visible omission in that
# table rather than a silent null appearing here.
#
# Precision is still derived rather than stored, because LabelRaw already says which pattern
# matched: MonthYear resolves to the first of the month and the day is a placeholder.

tab_date <- purrr::pmap(
  .l = list(lst_raw, tab_combo$Combo, tab_combo$Engine),
  .f = function(.d, .c, .e) {
    cand_ <- dplyr::filter(.d, !is.na(.data$Start))
    res_ <- if (.e == "lexnlp") {
      tibble::tibble(
        DateValue  = as.Date(cand_$DateValue),
        Precision  = dplyr::if_else(is.na(cand_$DateValue), NA_character_, "day"),
        Ambiguous  = FALSE,
        DateSource = dplyr::if_else(is.na(cand_$DateValue), "none", "engine")
      )
    } else if (.e == "paper") {
      tibble::tibble(
        DateValue = as.Date(cand_$DateValue),
        Precision = dplyr::if_else(cand_$LabelRaw == "MonthYear", "month", "day"),
        # Both readings calendar-valid and different: only the day-month orders can be ambiguous,
        # and the extractor's own choice is month-first because these are US filings.
        Ambiguous = cand_$LabelRaw %in% c("ISOShort", "Slash", "SlashShort") &
          purrr::map_lgl(cand_$Span, function(.s) {
            n_ <- as.integer(stringi::stri_extract_all_regex(.s, "\\d+")[[1]])
            length(n_) >= 2L && !anyNA(n_[1:2]) && all(n_[1:2] <= 12L) && n_[1] != n_[2]
          }),
        DateSource = dplyr::if_else(is.na(cand_$DateValue), "none", "engine")
      )
    } else {
      tibble::tibble(DateValue = as.Date(NA), Precision = NA_character_, Ambiguous = FALSE,
                     DateSource = "none")
    }
    dplyr::bind_cols(
      dplyr::select(cand_, dplyr::all_of(.core)),
      res_
    ) |>
      dplyr::mutate(Combo = .c)
  }
) |>
  purrr::list_rbind()

cli::cli_h2("Resolution rate, per engine")
tab_date |>
  dplyr::summarise(
    Spans       = dplyr::n(),
    Resolved    = sum(!is.na(.data$DateValue)),
    PctResolved = round(100 * mean(!is.na(.data$DateValue)), 1),
    DayPrec     = sum(.data$Precision == "day", na.rm = TRUE),
    MonthPrec   = sum(.data$Precision == "month", na.rm = TRUE),
    Ambiguous   = sum(.data$Ambiguous),
    .by = Combo
  ) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "'Ambiguous' counts spans where the US and European readings are both calendar-valid and give \\
   different dates. US order is assumed; this is how far that assumption reaches."
)

cli::cli_h2("Ambiguous spans, as read both ways")
tab_date |>
  dplyr::filter(.data$Ambiguous) |>
  dplyr::distinct(.data$Span, .data$LabelRaw, .data$DateValue) |>
  # optional = TRUE, because as.Date() ABORTS rather than returning NA when no format in
  # tryFormats matches, and one unparseable span would take the whole script down.
  dplyr::mutate(EuropeanReading = as.Date(
    stringi::stri_replace_first_regex(.data$Span, "^(\\d{1,2})([/.-])(\\d{1,2})", "$3$2$1"),
    tryFormats = c("%m/%d/%Y", "%m-%d-%Y", "%d.%m.%Y", "%m/%d/%y"), optional = TRUE
  )) |>
  print(n = 15, width = Inf)


# 10. Do two engines agree on the same offsets? ----
# Where lexnlp and dateregex both fire on the same span, they should produce the same date. One
# parsed it with a grammar and the other with a format string, so agreement is genuine
# corroboration and a mismatch is a defect in one of them.

tab_agree <- tab_date |>
  dplyr::filter(!is.na(.data$DateValue)) |>
  dplyr::summarise(
    NEngine = dplyr::n_distinct(.data$Combo),
    NDate   = dplyr::n_distinct(.data$DateValue),
    .by = c(DocID, Start, Stop, Span)
  )

cli::cli_h2("Agreement on the same offsets")
tab_agree |>
  dplyr::summarise(
    SpansBoth = sum(.data$NEngine > 1L),
    Agree     = sum(.data$NEngine > 1L & .data$NDate == 1L),
    Disagree  = sum(.data$NEngine > 1L & .data$NDate > 1L)
  ) |>
  dplyr::mutate(PctAgree = round(100 * .data$Agree / pmax(1L, .data$SpansBoth), 1)) |>
  print(width = Inf)

if (any(tab_agree$NEngine > 1L & tab_agree$NDate > 1L)) {
  cli::cli_alert_danger("Same span, different date:")
  tab_date |>
    dplyr::filter(!is.na(.data$DateValue)) |>
    dplyr::semi_join(
      dplyr::filter(tab_agree, .data$NEngine > 1L, .data$NDate > 1L),
      by = dplyr::join_by(DocID, Start, Stop, Span)
    ) |>
    dplyr::select(Combo, Span, LabelRaw, DateValue, Precision, Ambiguous) |>
    dplyr::arrange(.data$Span, .data$Combo) |>
    print(n = 30, width = Inf)
} else {
  cli::cli_alert_success("No span was parsed to two different dates.")
}


# 11. Against the filing date ----
# THE ONLY EXTERNAL FACT THIS LABEL HAS, and with a parsed value it stops being a yes-or-no test.
# 04A's design notes that the DATE anchor is a RANGE test rather than an identity test and
# therefore carries almost no information: nearly every contract contains SOME date in the two
# years before filing. The distribution of the gap does carry information, and it is what a role
# assignment would be built on: a signing date sits just before the filing, an expiry well after.

tab_gap <- tab_date |>
  dplyr::filter(!is.na(.data$DateValue)) |>
  dplyr::inner_join(dplyr::select(tab_pick, DocID, DateFiled), by = dplyr::join_by(DocID)) |>
  dplyr::mutate(GapDays = as.integer(.data$DateValue - .data$DateFiled))

cli::cli_h2("Days between the extracted date and the filing date")
tab_gap |>
  dplyr::summarise(
    Spans     = dplyr::n(),
    Before    = sum(.data$GapDays < 0L),
    After     = sum(.data$GapDays > 0L),
    InWindow  = sum(.data$GapDays >= -.win_back & .data$GapDays <= .win_fwd),
    MedGap    = stats::median(.data$GapDays),
    P10       = stats::quantile(.data$GapDays, 0.10),
    P90       = stats::quantile(.data$GapDays, 0.90),
    .by = Combo
  ) |>
  dplyr::mutate(PctInWindow = round(100 * .data$InWindow / .data$Spans, 1)) |>
  print(n = Inf, width = Inf)

cli::cli_h2("The gap, by where the date sits in the document")
tab_gap |>
  dplyr::inner_join(dplyr::select(tab_text, DocID, DocLen), by = dplyr::join_by(DocID)) |>
  dplyr::mutate(Region = dplyr::case_when(
    .data$Start < 3000L                     ~ "head (first 3,000 chars)",
    .data$Start >= 0.8 * .data$DocLen       ~ "tail (last fifth)",
    TRUE                                    ~ "middle"
  )) |>
  dplyr::summarise(
    Spans  = dplyr::n(),
    Before = sum(.data$GapDays < 0L),
    After  = sum(.data$GapDays > 0L),
    MedGap = stats::median(.data$GapDays),
    .by = c(Combo, Region)
  ) |>
  dplyr::filter(.data$Combo %in% c("lexnlp", "paper:dateregex-v2")) |>
  dplyr::arrange(.data$Combo, .data$Region) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "This is the shape a role assignment would use. A date in the head that precedes the filing is a \\
   signing date; one that follows it by years is a term or an expiry. Neither the label nor the \\
   span distinguishes them and the arithmetic does, which is the argument for carrying the parsed \\
   value into the store."
)


# 12. What spaCy calls a DATE ----
# The label covers durations and references as well as calendar dates. A span that no format can
# parse is not necessarily an error by spaCy -- "thirty (30) days" IS a temporal expression -- but
# it is not a date, and the share that is not decides whether this engine is a source for the label
# or only corroboration for the others.

# A FOUR-DIGIT RUN IS NOT A YEAR. The first version of this test accepted any "\\d{4}", and the
# read block showed it passing 55379, 55446, 1100 and 3826 -- ZIP codes and street numbers from
# notice blocks -- which overstated every spaCy share below. A year is now bounded to 1900-2099 and
# must not sit inside a longer digit run.
is_datelike_ <- function(.x) {
  stringi::stri_detect_regex(
    .x,
    paste0("(?<!\\d)(19|20)\\d{2}(?!\\d)|\\d{1,2}[/.-]\\d{1,2}[/.-]\\d{2,4}|(?i)\\b(",
           paste(.months, collapse = "|"),
           "|JAN|FEB|MAR|APR|JUN|JUL|AUG|SEP|OCT|NOV|DEC)\\b")
  )
}

cli::cli_h2("Share of spaCy DATE spans that look like a calendar date at all")
purrr::imap(lst_raw, \(.d, .n) {
  cand_ <- dplyr::filter(.d, !is.na(.data$Start))
  dl_   <- is_datelike_(.x = cand_$Span)
  tibble::tibble(Combo = .n, Spans = nrow(cand_), DateLike = sum(dl_),
                 PctDateLike = round(100 * mean(dl_), 1))
}) |>
  purrr::list_rbind() |>
  print(n = Inf, width = Inf)

cli::cli_h2("What spacy:en_core_web_trf calls a DATE that carries no date")
lst_raw[["spacy:en_core_web_trf"]] |>
  dplyr::filter(!is.na(.data$Start), !is_datelike_(.x = .data$Span)) |>
  dplyr::count(.data$Span, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = 25, width = Inf)


# 13. Read the same documents through every engine ----

tab_long <- purrr::imap(lst_raw, \(.d, .n) {
  dplyr::transmute(dplyr::filter(.d, !is.na(.data$Start)),
                   Combo = .n, DocID, Start, Stop, Span, LabelRaw)
}) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    Mark = dplyr::coalesce(
      unname(c("lexnlp" = "L", "paper:dateregex-v2" = "R", "spacy:en_core_web_sm" = "s",
               "spacy:en_core_web_md" = "m", "spacy:en_core_web_lg" = "l",
               "spacy:en_core_web_trf" = "t")[.data$Combo]),
      "?"
    )
  )

# Keyed on (document, span) rather than on offsets: the read block groups engines by span, and two
# engines can bracket the same date with slightly different Stop offsets. One span string in one
# document parses to one date, so that is the right grain for a display join.
tab_val <- tab_date |>
  dplyr::filter(!is.na(.data$DateValue)) |>
  dplyr::summarise(Val = as.character(dplyr::first(.data$DateValue)), .by = c(DocID, Span))

docs_read_ <- withr::with_seed(.seed, sample(tab_pick$DocID, size = .n_read))

cli::cli_h2("The same documents, every engine")
purrr::walk(docs_read_, function(.d) {
  meta_ <- dplyr::filter(tab_pick, .data$DocID == .d)
  cli::cli_h3("{meta_$Class} | {meta_$CompanyName} | filed {format(meta_$DateFiled)}")

  rows_ <- tab_long |>
    dplyr::filter(.data$DocID == .d) |>
    dplyr::summarise(
      Present = paste(
        purrr::map_chr(c("L", "R", "s", "m", "l", "t"),
                       \(.k) if (.k %in% .data$Mark) .k else "."),
        collapse = ""
      ),
      Start = min(.data$Start),
      .by = c(Span)
    ) |>
    dplyr::left_join(
      dplyr::select(dplyr::filter(tab_val, .data$DocID == .d), Span, Val),
      by = dplyr::join_by(Span)
    ) |>
    dplyr::arrange(.data$Start)

  cat(sprintf("  %7s %-6s %-11s %s\n", "Start", "LRsmlt", "Parsed", "Span"))
  purrr::walk(seq_len(nrow(rows_)), function(.i) {
    r_ <- rows_[.i, ]
    cat(sprintf("  %7d %-6s %-11s %s\n", r_$Start, r_$Present,
                dplyr::coalesce(r_$Val, "-"),
                stringi::stri_replace_all_regex(r_$Span, "\\s+", " ")))
  })
  cat("\n")
})


# 14. Verdict ----

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
