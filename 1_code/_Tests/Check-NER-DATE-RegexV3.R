# Check-NER-DATE-RegexV3: does v3 add a label without moving a date ----
#
# WHAT THIS IS
# The acceptance check for dateregex-v3, built to the contract Check-NER-ORG settled and
# Check-NER-DATE confirmed. It is NARROWER than Check-NER-DATE by design: that script asks which
# engine should supply DATE, and this one asks a single question about one engine.
#
# THE QUESTION
# v3 adds a second label to an engine that had one. A contract states when it ends either by naming
# a date -- which v2 already found -- or by stating a DURATION and naming no date at all: "for a
# period of five (5) years from the Effective Date", "the third anniversary", "shall continue until
# terminated". The second is invisible to a date extractor by construction, and on the sample it is
# the only thing said about the end in roughly one document in ten.
#
# Adding it means two things could go wrong, and this script exists to rule out the first and
# measure the second.
#
# 1. THE DATES COULD MOVE. v3 changes how overlaps resolve -- within a label rather than across --
#    because "for a period of five (5) years from January 1, 2020" is a term AND a date and a single
#    greedy pass would let the longer span delete the shorter. That change is supposed to be
#    invisible to the date family. Block 6 compares v2 and v3 SPAN FOR SPAN and a single difference
#    is a defect, not a design choice.
#
# 2. THE TERMS COULD BE SOMETHING ELSE. "Thirty (30) day period" is a stated period and is not a
#    contract's duration; it is a notice, a cure or a payment window. Blocks 7 to 9 measure the
#    families separately, report what each parses to, and put the sub-year share beside it, because
#    the number that decides whether a family is a duration is the distribution of its values and
#    not the plausibility of its name.
#
# WHAT THIS SCRIPT DOES NOT DO. It does not decide which families 04B3 should treat as a duration.
# The extractor's job is to find a stated period; deciding which are durations is a rule, and a rule
# belongs in the document that owns the variable. What this provides is the evidence.
#
# The parquets are left on disk under 2_output/_Probe/Check-NER-DATE-RegexV3/out and are meant to be
# opened.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_04a     <- here::here("2_output", "04A-EntityExtract")
.path_text   <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys   <- fs::path(.dir_04a, "sample_anchors.parquet")

.dir_check   <- fs::dir_create(here::here("2_output", "_Probe", "Check-NER-DATE-RegexV3"))
.dir_in      <- fs::dir_create(fs::path(.dir_check, "in"))
.dir_out     <- fs::dir_create(fs::path(.dir_check, "out"))
.dir_engine  <- here::here("contracts-engine")

# TWO SAMPLES, AND THEY ANSWER DIFFERENT QUESTIONS. The small draw is the same twenty-five documents
# the ORG, GPE and DATE checks used, on the same seed, so a span read here can be read against those
# scripts. The wide draw is every document in the sample, because a family firing on 2% of contracts
# cannot be seen in twenty-five of them and the yield table is the point of blocks 7 to 10.
.n_docs      <- 25L
.len_min     <- 4000L
.len_max     <- 40000L
.timeout     <- 240L
.max_chars   <- NULL   # must match 04A (.max_chars_extract is NULL there) or block 6 reports drift
.rerun       <- TRUE
.seed        <- 42L    # the SAME seed as the ORG, GPE and DATE checks
.n_read      <- 3L
.n_top       <- 10L    # spans listed per pattern in the survey

tab_combo <- tibble::tribble(
  ~Engine, ~Model,          ~NProc,
  "paper", "dateregex-v2",  20L,
  "paper", "dateregex-v3",  20L
) |>
  dplyr::mutate(
    Combo  = paste0(.data$Engine, ":", .data$Model),
    Script = dplyr::if_else(.data$Model == "dateregex-v2", "extract_dateregex.py",
                            "extract_dateregex_v3.py"),
    File   = paste0("DATE__", .data$Engine, "__", .data$Model, ".parquet"),
    Path   = as.character(fs::path(.dir_out, .data$File))
  )

.core <- c("DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model")

stopifnot(fs::file_exists(c(.path_text, .path_keys)))
stopifnot(fs::file_exists(fs::path(.dir_engine, "extract_dateregex.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, "extract_dateregex_v3.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, ".venv", "bin", "python")))


# 2. The sample ----

tab_text <- arrow::read_parquet(.path_text) |>
  dplyr::mutate(DocLen = stringi::stri_length(.data$TextRaw))

tab_keys <- arrow::read_parquet(.path_keys) |>
  dplyr::select(DocID, Class = "ClassDetailed", CompanyName, AmendType, DateFiled) |>
  dplyr::mutate(
    DateFiled = as.Date(suppressWarnings(anytime::anydate(as.character(.data$DateFiled))))
  )

# The wide draw: everything, because a yield table over twenty-five documents measures the draw.
tab_text |>
  dplyr::select(DocID, TextRaw) |>
  arrow::write_parquet(fs::path(.dir_in, "sample.parquet"))

# The narrow draw: the same twenty-five the other checks read, for block 11.
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
  "{format(nrow(tab_text), big.mark = ',')} document{?s} run through both engines; \\
   {nrow(tab_pick)} of them read back in block 11 -- the same draw the ORG, GPE and DATE checks \\
   used, on the same seed."
)


# 3. Run both engines ----

run_regex_ <- function(.path, .script, .nproc, .labels) {
  args_ <- c(
    fs::path(.dir_engine, .script),
    as.character(fs::path_abs(fs::path(.dir_in, "sample.parquet"))),
    "--output", as.character(.path),
    "--label", .labels,
    "--n-process", as.integer(.nproc),
    "--timeout", as.integer(.timeout)
  )
  if (!is.null(.max_chars)) args_ <- c(args_, "--max-chars", as.integer(.max_chars))
  system2(fs::path(.dir_engine, ".venv", "bin", "python"), args_, stdout = "", stderr = "")
}

purrr::pwalk(tab_combo, function(Engine, Model, NProc, Combo, Script, File, Path, ...) {
  if (!.rerun && fs::file_exists(Path)) {
    cli::cli_alert_info("Reusing {(File)}.")
    return(invisible(NULL))
  }
  cli::cli_alert_info("Running {(Combo)} ...")
  t0_ <- Sys.time()
  # v2 has no TERM label and would reject the argument; v3 takes both and is asked for both.
  labels_ <- if (Model == "dateregex-v2") "DATE" else c("DATE", "TERM")
  status_ <- run_regex_(.path = Path, .script = Script, .nproc = NProc, .labels = labels_)
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("{(Combo)} failed ({status_}).")
  secs_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))
  cli::cli_alert_success(
    "{(Combo)} in {round(secs_, 1)}s -- {round(nrow(tab_text) / secs_)} doc/s."
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
    MB     = round(as.numeric(fs::file_size(.data$Path)) / 1024^2, 2),
    NExtra = purrr::map_int(lst_raw, \(.d) length(setdiff(names(.d), .core))),
    Extras = purrr::map_chr(lst_raw, \(.d) paste(setdiff(names(.d), .core), collapse = ", "))
  ) |>
  dplyr::select(Combo, File, Rows, MB, NExtra, Extras) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info("Open these directly at {.path {(.dir_out)}}.")

tab_v2 <- lst_raw[["paper:dateregex-v2"]]
tab_v3 <- lst_raw[["paper:dateregex-v3"]]
tab_len <- dplyr::select(tab_text, DocID, TextRaw, DocLen)


# 5. The contract, on both labels ----
#
# The same nine tests Check-NER-DATE runs, with one change: Label is no longer a constant, so T6
# asks whether every candidate carries one of the TWO permitted labels rather than whether it
# carries DATE. A row with any other label is a schema failure and would reach the store.

check_one_ <- function(.tab, .combo, .engine, .model, .labels) {
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
              "T5 Engine and Model constant and correct", "T6 Label is permitted on candidates",
              "T7 no duplicate DocID/Start/Stop/LabelRaw", "T8 sentinels fully null",
              "T9 extras absent on sentinel rows"),
    N     = c(
      sum(.core %in% names(.tab)),
      dplyr::n_distinct(.tab$DocID),
      sum(rt_$OK),
      sum(rt_$InDoc),
      sum(.tab$Engine == .engine & .tab$Model == .model),
      sum(cand_$Label %in% .labels),
      nrow(dplyr::distinct(cand_, .data$DocID, .data$Start, .data$Stop, .data$LabelRaw)),
      sum(is.na(sent_$Stop) & is.na(sent_$Span) & is.na(sent_$Label)),
      sum(purrr::map_int(seq_len(nrow(sent_)), function(.i) {
        ext_ <- setdiff(names(sent_), .core)
        if (length(ext_) == 0L) return(1L)
        as.integer(all(is.na(unlist(sent_[.i, ext_]))))
      }))
    ),
    Of    = c(length(.core), nrow(tab_text), nrow(cand_), nrow(cand_), nrow(.tab),
              nrow(cand_), nrow(cand_), nrow(sent_), nrow(sent_))
  )
}

tab_tests <- dplyr::bind_rows(
  check_one_(.tab = tab_v2, .combo = "paper:dateregex-v2", .engine = "paper",
             .model = "dateregex-v2", .labels = "DATE"),
  check_one_(.tab = tab_v3, .combo = "paper:dateregex-v3", .engine = "paper",
             .model = "dateregex-v3", .labels = c("DATE", "TERM"))
)

cli::cli_h2("Contract tests")
tab_tests |>
  dplyr::mutate(Pass = .data$N == .data$Of) |>
  tidyr::pivot_wider(id_cols = Test, names_from = Combo, values_from = Pass) |>
  print(n = Inf, width = Inf)

if (any(tab_tests$N != tab_tests$Of)) {
  cli::cli_alert_danger("Failures below. Each one blocks insertion.")
  tab_tests |>
    dplyr::filter(.data$N != .data$Of) |>
    print(n = Inf, width = Inf)
} else {
  cli::cli_alert_success("Both engines hold the contract.")
}


# 6. Does v3 move a single date? ----
#
# THE TEST THIS SCRIPT EXISTS FOR. v3 changes overlap resolution from one greedy pass over every
# pattern to one pass per label, because a term and a date can legitimately overlap. That change is
# supposed to be invisible to the date family: the DATE patterns are unchanged, they compete only
# with each other in both versions, and the same text should therefore give the same spans.
#
# A FULL OUTER JOIN, not a count. Two engines can agree on the number of dates and disagree on which
# ones, and a row present in one and absent in the other is exactly the failure this is looking for.

key_date_ <- function(.tab) {
  .tab |>
    dplyr::filter(!is.na(.data$Start), .data$Label == "DATE") |>
    dplyr::select(DocID, Start, Stop, Span, LabelRaw, DateValue) |>
    dplyr::arrange(.data$DocID, .data$Start)
}

d2_ <- key_date_(tab_v2)
d3_ <- key_date_(tab_v3)

tab_diff <- dplyr::full_join(
  dplyr::mutate(d2_, InV2 = TRUE),
  dplyr::mutate(d3_, InV3 = TRUE),
  by = dplyr::join_by(DocID, Start, Stop)
) |>
  dplyr::mutate(
    InV2 = dplyr::coalesce(.data$InV2, FALSE),
    InV3 = dplyr::coalesce(.data$InV3, FALSE),
    SameRaw = dplyr::coalesce(.data$LabelRaw.x == .data$LabelRaw.y, FALSE),
    SameVal = dplyr::coalesce(.data$DateValue.x == .data$DateValue.y,
                              is.na(.data$DateValue.x) & is.na(.data$DateValue.y))
  )

cli::cli_h2("v3 against v2, span for span")
tibble::tibble(
  Item = c("DATE spans in v2", "DATE spans in v3",
           "...present in both, at the same offsets",
           "...and carrying the same LabelRaw",
           "...and parsing to the same DateValue",
           "In v2 only", "In v3 only"),
  N    = c(nrow(d2_), nrow(d3_),
           sum(tab_diff$InV2 & tab_diff$InV3),
           sum(tab_diff$InV2 & tab_diff$InV3 & tab_diff$SameRaw),
           sum(tab_diff$InV2 & tab_diff$InV3 & tab_diff$SameVal),
           sum(tab_diff$InV2 & !tab_diff$InV3),
           sum(!tab_diff$InV2 & tab_diff$InV3))
) |>
  print(n = Inf, width = Inf)

n_moved_ <- sum(tab_diff$InV2 != tab_diff$InV3) +
  sum(tab_diff$InV2 & tab_diff$InV3 & !(tab_diff$SameRaw & tab_diff$SameVal))

if (n_moved_ == 0L) {
  cli::cli_alert_success(
    "v3 reproduces every one of v2's dates exactly. The label split is invisible to the date family, \\
     which is what it was supposed to be."
  )
} else {
  cli::cli_alert_danger(
    "{n_moved_} date{?s} moved between v2 and v3. The cross-label change leaked into the date \\
     family; the rows are below."
  )
  tab_diff |>
    dplyr::filter(.data$InV2 != .data$InV3 |
                    !(.data$SameRaw & .data$SameVal)) |>
    dplyr::select(DocID, Start, Stop, Span.x, LabelRaw.x, DateValue.x,
                  Span.y, LabelRaw.y, DateValue.y) |>
    print(n = 30L, width = Inf)
}


# 7. The term families ----
#
# One row per pattern: how much it fires, on how many documents, and WHAT IT PARSES TO. The last is
# the column that decides whether a family is a duration, and it is not the plausibility of its name.

tab_term <- tab_v3 |>
  dplyr::filter(!is.na(.data$Start), .data$Label == "TERM") |>
  dplyr::left_join(dplyr::select(tab_keys, DocID, Class), by = dplyr::join_by(DocID))

cli::cli_h2("Term families")
tab_term |>
  dplyr::summarise(
    Spans      = dplyr::n(),
    Docs       = dplyr::n_distinct(.data$DocID),
    PctDocs    = dplyr::n_distinct(.data$DocID) / nrow(tab_text),
    NValued    = sum(!is.na(.data$TermYears)),
    MedYears   = stats::median(.data$TermYears, na.rm = TRUE),
    P90Years   = suppressWarnings(unname(stats::quantile(.data$TermYears, 0.9, na.rm = TRUE))),
    PctSubYear = mean(.data$TermYears < 1, na.rm = TRUE),
    .by = LabelRaw
  ) |>
  dplyr::arrange(dplyr::desc(.data$Spans)) |>
  dplyr::mutate(
    PctDocs    = scales::percent(.data$PctDocs, accuracy = 0.1),
    PctSubYear = scales::percent(.data$PctSubYear, accuracy = 0.1),
    dplyr::across(c(MedYears, P90Years), \(.x) round(.x, 3))
  ) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "PctSubYear IS THE DISCRIMINATING COLUMN. A three-year TERM is what a contract calls its own \\
   duration; a thirty-day PERIOD is a notice, a cure or a payment window, and a family whose values \\
   are mostly under a year is measuring the second thing. UnitTerm and UnitPeriod are one pattern \\
   split on its trailing noun for exactly this reason."
)

cli::cli_h2("The units each family is stated in")
tab_term |>
  dplyr::filter(!is.na(.data$TermUnit)) |>
  dplyr::count(.data$LabelRaw, .data$TermUnit, name = "Spans") |>
  tidyr::pivot_wider(names_from = "TermUnit", values_from = "Spans", values_fill = 0L) |>
  print(n = Inf, width = Inf)


# 8. What each family actually matched ----
#
# THE BLOCK THAT FINDS A FALSE POSITIVE RATHER THAN CONFIRMING ONE. Block 7 tests the distribution
# of what the patterns caught; this shows the phrases the corpus contains. A family whose commonest
# spans are all one boilerplate sentence is a family measuring that sentence.

cli::cli_h2("The commonest span in each family")
purrr::walk(sort(unique(tab_term$LabelRaw)), function(.raw) {
  sub_ <- dplyr::filter(tab_term, .data$LabelRaw == .raw)
  cli::cli_h3(paste0(.raw, " -- ", format(nrow(sub_), big.mark = ","), " spans in ",
                     format(dplyr::n_distinct(sub_$DocID), big.mark = ","), " documents"))
  sub_ |>
    dplyr::mutate(Norm = stringi::stri_trans_tolower(
      stringi::stri_replace_all_regex(.data$Span, "\\s+", " ")
    )) |>
    dplyr::summarise(Spans = dplyr::n(), Docs = dplyr::n_distinct(.data$DocID),
                     Years = dplyr::first(.data$TermYears), .by = Norm) |>
    dplyr::arrange(dplyr::desc(.data$Spans)) |>
    dplyr::mutate(Years = round(.data$Years, 3)) |>
    print(n = .n_top, width = Inf)
})


# 9. Do the term columns say what they should? ----
#
# Arithmetic identities rather than tests of the patterns. They fail only if the parser and the
# columns disagree, which is the failure mode this label is most exposed to and least able to show
# any other way.

unit_years_ <- c(day = 1 / 365.25, week = 7 / 365.25, month = 1 / 12, year = 1)

chk_ <- tab_term |>
  dplyr::mutate(
    Expect = .data$TermN * unname(unit_years_[.data$TermUnit]),
    Agrees = dplyr::coalesce(abs(.data$TermYears - .data$Expect) < 1e-3, FALSE),
    OpenOK = .data$LabelRaw != "OpenEnded" |
      (is.na(.data$TermN) & .data$TermUnit == "open" & is.na(.data$TermYears)),
    ValOK  = is.na(.data$TermYears) | (!is.na(.data$TermN) & !is.na(.data$TermUnit))
  )

# FILTERED, NOT INDEXED. `tab_v3$TermN[tab_v3$Label == "DATE"]` looks like the DATE rows and is not:
# Label is NA on a sentinel, so the logical index carries NAs, `[` returns one NA element for each,
# and is.na() counts them. That reported 51,773 of 51,572 -- a subset larger than its superset, which
# is the shape of an NA-index bug rather than of a failure.
date_ <- dplyr::filter(tab_v3, !is.na(.data$Start), .data$Label == "DATE")

cli::cli_h2("The term columns against each other")
tibble::tibble(
  Check = c("TermYears equals TermN times the unit",
            "OpenEnded rows are exactly (NA, open, NA)",
            "A valued term carries both a number and a unit",
            "Every TERM row carries no DateValue",
            "Every DATE row carries no term column"),
  N     = c(sum(chk_$Agrees[chk_$LabelRaw != "OpenEnded"]),
            sum(chk_$OpenOK),
            sum(chk_$ValOK),
            sum(is.na(tab_term$DateValue)),
            sum(is.na(date_$TermN) & is.na(date_$TermUnit) & is.na(date_$TermYears))),
  Of    = c(sum(chk_$LabelRaw != "OpenEnded"), nrow(chk_), nrow(chk_), nrow(tab_term),
            nrow(date_))
) |>
  dplyr::mutate(Pass = .data$N == .data$Of) |>
  print(n = Inf, width = Inf)


# 10. Does a term overlap a date, and is that allowed? ----
#
# TWO FACTS, AND ONLY THE FIRST IS A REQUIREMENT. No two spans of the SAME label may overlap: that is
# what one greedy pass per label guarantees, and a non-zero count is a defect. Spans of DIFFERENT
# labels MAY overlap, and v3 resolves within a label rather than across so that "for a period of five
# (5) years from January 1, 2020" can be a term and a date at once.
#
# MEASURED, THE SECOND COUNT IS ZERO, and an earlier version of this comment called that surprising.
# It is not. The term patterns stop at their unit or their trailing noun, so a term span ends where
# the date span has not started -- "shall CONTINUE FOR FIVE (5) YEARS until September 30, 2004" holds
# both and they are adjacent, never overlapping. The within-label rule is therefore a SAFETY PROPERTY
# THAT CURRENTLY COSTS NOTHING rather than a fix for an observed collision, which is consistent with
# block 6 finding no date moved: a single greedy pass would have given the same answer on this
# corpus. It is kept because the patterns will change and the guarantee should not depend on their
# having stayed apart by luck.
#
# So a zero on the second row is reported and not judged. What would be a finding is a NON-zero one:
# it would mean a term pattern had grown to swallow a date, and the rows would print.

overlap_within_ <- tab_v3 |>
  dplyr::filter(!is.na(.data$Start)) |>
  dplyr::arrange(.data$DocID, .data$Label, .data$Start) |>
  dplyr::mutate(PrevStop = dplyr::lag(.data$Stop), .by = c(DocID, Label)) |>
  dplyr::filter(!is.na(.data$PrevStop), .data$Start < .data$PrevStop)

dt_ <- dplyr::filter(tab_v3, !is.na(.data$Start), .data$Label == "DATE") |>
  dplyr::select(DocID, DStart = Start, DStop = Stop, DSpan = Span)
tm_ <- dplyr::filter(tab_v3, !is.na(.data$Start), .data$Label == "TERM") |>
  dplyr::select(DocID, TStart = Start, TStop = Stop, TSpan = Span, LabelRaw)

overlap_across_ <- tm_ |>
  dplyr::inner_join(dt_, by = dplyr::join_by(DocID), relationship = "many-to-many") |>
  dplyr::filter(.data$DStart < .data$TStop, .data$TStart < .data$DStop)

# How close the two families come without touching. A term whose nearest date sits a few characters
# away is one pattern edit from overlapping, and that distance is what says whether the guarantee is
# idle or merely unexercised.
near_ <- tm_ |>
  dplyr::inner_join(dt_, by = dplyr::join_by(DocID), relationship = "many-to-many") |>
  dplyr::mutate(
    Gap = dplyr::case_when(
      .data$DStart >= .data$TStop ~ .data$DStart - .data$TStop,
      .data$TStart >= .data$DStop ~ .data$TStart - .data$DStop,
      TRUE                        ~ 0L
    )
  ) |>
  dplyr::slice_min(order_by = .data$Gap, n = 1L, by = c(DocID, TStart), with_ties = FALSE)

cli::cli_h2("Overlaps")
tibble::tibble(
  Item = c("Overlapping spans WITHIN a label (must be zero)",
           "Overlapping spans ACROSS labels (permitted, reported)",
           "Terms whose nearest date is within 20 characters",
           "Median characters from a term to its nearest date"),
  N    = c(nrow(overlap_within_), nrow(overlap_across_),
           sum(near_$Gap <= 20L), as.integer(stats::median(near_$Gap)))
) |>
  print(n = Inf, width = Inf)

if (nrow(overlap_within_) > 0L) {
  cli::cli_alert_danger("Spans of one label overlap. One greedy pass per label did not hold.")
  overlap_within_ |>
    dplyr::select(DocID, Label, LabelRaw, Start, Stop, Span) |>
    print(n = 20L, width = Inf)
} else if (nrow(overlap_across_) > 0L) {
  cli::cli_alert_info("A term and a date share text in {nrow(overlap_across_)} place{?s}:")
  overlap_across_ |>
    dplyr::slice_head(n = 8L) |>
    dplyr::select(DocID, LabelRaw, TSpan, DSpan) |>
    print(n = Inf, width = Inf)
} else {
  cli::cli_alert_info(
    "No term and date share text on this corpus, because the term patterns stop at their unit or \\
     their trailing noun and the date begins after it. The within-label rule is a guarantee that \\
     currently costs nothing rather than a fix for an observed collision -- which is why block 6 \\
     found no date moved. The nearest-date distance above says how much room the patterns have \\
     before that stops being true."
  )
}


# 11. What the terms add that the dates did not have ----
#
# THE NUMBER 04B3 CARES ABOUT. A contract states when it ends by naming a date or by stating a
# duration, and the second is invisible to a date extractor. This crosses the two: how many
# documents carry a future date, how many carry a stated term, and how many carry ONLY a term --
# which is the coverage a date-only rule was reporting as a gap.
#
# A future date is one parsing later than the filing, since that is what an end-date rule reads. The
# term side is counted twice, with and without the families whose values are mostly under a year,
# because that choice belongs to 04B3 and its size belongs here.

# THE CUT IS PER SPAN, NOT PER FAMILY, and an earlier version of this block had it wrong. Excluding
# UnitPeriod and ContinueFor wholesale throws away 1,255 and 138 spans stated in YEARS -- and one of
# them reads "The initial term of this Agreement shall BE FOR 6 MONTHS, commencing on November 26th
# 2007, and ending on May 25th 2008", which is the contract's own term sitting in the family the
# family-level cut discards. Against it, in the same family: "such failure CONTINUES FOR A PERIOD OF
# FIVE DAYS". The discriminator is the value the span carries, not the pattern it came from.
#
# So LongTerm is a term of at least .term_min_years from ANY family, and the family shares stay in
# block 7 as the secondary signal they are. OpenEnded counts: an unbounded term is a statement about
# the end, and a longer one than any of these.
.term_min_years <- 1

tab_cover <- tab_keys |>
  dplyr::select(DocID, Class, DateFiled) |>
  dplyr::left_join(
    tab_v3 |>
      dplyr::filter(.data$Label == "DATE", !is.na(.data$DateValue)) |>
      dplyr::mutate(DateVal = as.Date(.data$DateValue)) |>
      dplyr::summarise(MaxDate = max(.data$DateVal), .by = DocID),
    by = dplyr::join_by(DocID)
  ) |>
  dplyr::left_join(
    tab_term |>
      dplyr::summarise(
        AnyTerm  = TRUE,
        LongTerm = any(.data$LabelRaw == "OpenEnded" |
                         .data$TermYears >= .term_min_years, na.rm = TRUE),
        .by = DocID
      ),
    by = dplyr::join_by(DocID)
  ) |>
  dplyr::mutate(
    HasFuture = !is.na(.data$MaxDate) & !is.na(.data$DateFiled) & .data$MaxDate > .data$DateFiled,
    AnyTerm   = dplyr::coalesce(.data$AnyTerm, FALSE),
    LongTerm  = dplyr::coalesce(.data$LongTerm, FALSE)
  )

cli::cli_h2("Coverage: what a date-only rule was missing")
cli::cli_alert_info(
  "LongTerm is a stated term of at least {(.term_min_years)} year, or an unbounded one, FROM ANY \\
   FAMILY. Cutting by family instead would discard {sum(tab_term$LabelRaw %in% c('UnitPeriod', \\
   'ContinueFor') & tab_term$TermYears >= 1, na.rm = TRUE)} span{?s} stated in years."
)

tibble::tibble(
  Item = c("Documents in the sample",
           "...with a future date",
           "...with any stated term",
           "...with a term of a year or more, or unbounded",
           "NO future date, but a long stated term",
           "NO future date and no term at all"),
  N    = c(nrow(tab_cover),
           sum(tab_cover$HasFuture),
           sum(tab_cover$AnyTerm),
           sum(tab_cover$LongTerm),
           sum(!tab_cover$HasFuture & tab_cover$LongTerm),
           sum(!tab_cover$HasFuture & !tab_cover$AnyTerm))
) |>
  dplyr::mutate(Share = scales::percent(.data$N / nrow(tab_cover), accuracy = 0.1)) |>
  print(n = Inf, width = Inf)

cli::cli_h2("The same, by contract type")
tab_cover |>
  dplyr::summarise(
    Docs      = dplyr::n(),
    PctFuture = mean(.data$HasFuture),
    PctTerm   = mean(.data$LongTerm),
    PctGain   = mean(!.data$HasFuture & .data$LongTerm),
    PctNone   = mean(!.data$HasFuture & !.data$AnyTerm),
    .by = Class
  ) |>
  dplyr::arrange(dplyr::desc(.data$PctGain)) |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("Pct"),
                              \(.x) scales::percent(.x, accuracy = 0.1))) |>
  print(n = Inf, width = Inf)


# 12. Read the terms in context ----
#
# Every table above is equally consistent with a span being a contract's duration and with it being
# a notice period a pattern name flattered. Only the words separate them.

cli::cli_h2("Terms in context")
read_ <- tab_term |>
  dplyr::filter(.data$DocID %in% tab_pick$DocID) |>
  dplyr::left_join(dplyr::select(tab_text, DocID, TextRaw), by = dplyr::join_by(DocID)) |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(20L, nrow(.d)))))() |>
  dplyr::mutate(
    Snippet = stringi::stri_replace_all_regex(
      paste0(
        "...",
        stringi::stri_sub(.data$TextRaw, from = pmax(1L, .data$Start + 1L - 160L),
                          to = .data$Start),
        " >>>", .data$Span, "<<< ",
        stringi::stri_sub(.data$TextRaw, from = .data$Stop + 1L, to = .data$Stop + 160L),
        "..."
      ),
      "\\s+", " "
    )
  )

purrr::walk(seq_len(nrow(read_)), function(.i) {
  row_ <- read_[.i, ]
  cli::cli_h3(paste0(row_$LabelRaw, " | ", row_$TermN, " ", row_$TermUnit,
                     " | ", round(row_$TermYears, 3), "y | ", row_$Class))
  cli::cli_verbatim(strwrap(row_$Snippet, width = 116, prefix = "  "))
})


# 13. Verdict ----

n_fail_ <- sum(tab_tests$N != tab_tests$Of)
n_col_  <- sum(!chk_$Agrees[chk_$LabelRaw != "OpenEnded"]) + sum(!chk_$OpenOK) + sum(!chk_$ValOK)

cli::cli_h2("Verdict")
n_first_ <- sum(stringi::stri_detect_regex(tab_term$Span, "(?i)^first\\s+anniversary$"))

tibble::tibble(
  Item = c("Contract tests failing", "Dates moved between v2 and v3",
           "Term columns disagreeing with each other", "Overlaps within a label",
           "TERM spans", "TERM documents",
           "Documents gaining an end statement from a term alone",
           "...of which 'first anniversary' is the only term"),
  N    = c(n_fail_, n_moved_, n_col_, nrow(overlap_within_),
           nrow(tab_term), dplyr::n_distinct(tab_term$DocID),
           sum(!tab_cover$HasFuture & tab_cover$LongTerm),
           tab_term |>
             dplyr::summarise(OnlyFirst = all(stringi::stri_detect_regex(
               .data$Span, "(?i)^first\\s+anniversary$")), .by = DocID) |>
             dplyr::filter(.data$OnlyFirst) |>
             dplyr::semi_join(dplyr::filter(tab_cover, !.data$HasFuture & .data$LongTerm),
                              by = dplyr::join_by(DocID)) |>
             nrow())
) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "\"First anniversary\" is {n_first_} of the {nrow(dplyr::filter(tab_term, .data$LabelRaw == \
   'Anniversary'))} Anniversary spans, and it is the family whose NUMBER is clean and whose MEANING \
   is not: in a compensation agreement it is usually a vesting date rather than the contract's end. \
   The last row is how many documents rest on it alone."
)

if (n_fail_ == 0L && n_moved_ == 0L && n_col_ == 0L && nrow(overlap_within_) == 0L) {
  cli::cli_alert_success(
    "v3 holds the contract, reproduces every one of v2's dates, and its term columns agree with \\
     each other. What remains is a JUDGEMENT rather than a check: which families 04B3 should treat \\
     as a duration, and block 7's PctSubYear beside block 8's spans is the evidence for it."
  )
} else {
  cli::cli_alert_danger("Failures above. v3 is not ready to replace v2 in the store.")
}
