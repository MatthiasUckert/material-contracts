# Front matter: does it explain the offset tail? ----
#
# THE CLAIM UNDER TEST
# The offset of the contracting party is flat at roughly 210 characters across a 97-fold range of
# document length, but its 90th percentile rises 37-fold over the same range. The proposed
# explanation is that the preamble is a fixed feature at a fixed distance from the start of the
# OPERATIVE text, and that what varies is the front matter in front of it -- the EDGAR wrapper, a
# cover page, and above all a table of contents, which is long precisely when the agreement is long.
#
# If that is right, measuring from the end of the front matter rather than from character zero
# should flatten the tail, and a tight window would then work at every document length with no
# trade-off to accept.
#
# THIS SCRIPT DECIDES NOTHING. It measures marker prevalence, marker position, the implied front
# matter length, and the offset distribution under each candidate origin, then prints text so the
# numbers can be checked against what is actually in the documents.
#
# A CIRCULARITY TO WATCH
# One candidate origin is the bilateral connector, and the party sits beside the connector by
# construction. Measuring the party's distance from it therefore proves nothing about windows. It is
# reported anyway, because if the connector locates the preamble directly then no window is needed
# for this purpose at all -- but it is reported as a DIFFERENT question, not as a rival origin.
#
# Reads only. Run after 04B has written contracting_party.parquet.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.path_text  <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")
.path_party <- here::here("2_output", "04B-EntityMeasure", "contracting_party.parquet")

.horizon    <- 50000L  # characters searched for front matter; a TOC beyond this is not front matter
.n_examples <- 12L
.seed       <- 42L

stopifnot(fs::file_exists(c(.path_text, .path_party)))

tab_party <- arrow::read_parquet(.path_party)
tab_text  <- arrow::read_parquet(.path_text) |>
  dplyr::transmute(
    DocID,
    DocLen = stringi::stri_length(.data$TextRaw),
    Head   = stringi::stri_sub(.data$TextRaw, from = 1L, to = .horizon)
  )

cli::cli_alert_info("{nrow(tab_text)} document{?s}; first {(.horizon)} character{?s} searched.")


# 2. The markers ----
# Each is a separate question. Prevalence says whether the signal exists at all; position says
# whether it sits where front matter would.
#
# WITNESSETH is spaced out in a large share of filings -- "W I T N E S S E T H" -- so the pattern
# has to tolerate whitespace between every letter or it misses most of its own occurrences.

.markers <- c(
  TocMarker    = "(?i)\\bTABLE\\s+OF\\s+CONTENTS\\b",
  DotLeader    = "\\.{5,}\\s*\\d{1,3}\\b",
  DotLoose     = "\\.{4,}",
  EdgarHeader  = "(?i)^EX-\\S+\\s+\\d+\\s+\\S+\\.(htm|txt)",
  ConnectStrict = "(?i)\\bby\\s+and\\s+(between|among)\\b",
  ConnectLoose = "(?i)\\b(between|among)\\b",
  ThisAgreement = "(?i)\\bTHIS\\s+[A-Z][A-Za-z'& ]{2,60}?\\s+AGREEMENT\\b",
  Witnesseth   = "(?i)\\bW\\s*I\\s*T\\s*N\\s*E\\s*S\\s*S\\s*E\\s*T\\s*H\\b",
  Recitals     = "(?i)\\bRECITALS\\b"
)

tab_mark <- purrr::imap(.markers, function(.rgx, .nm) {
  hit_ <- stringi::stri_locate_first_regex(tab_text$Head, .rgx)
  tibble::tibble(DocID = tab_text$DocID, Marker = .nm, At = hit_[, 1])
}) |>
  purrr::list_rbind()

cli::cli_h2("A. Marker prevalence and where it first occurs")
tab_mark |>
  dplyr::summarise(
    Present = sum(!is.na(.data$At)),
    Pct     = round(100 * mean(!is.na(.data$At)), 1),
    P25     = as.integer(stats::quantile(.data$At, 0.25, na.rm = TRUE)),
    P50     = as.integer(stats::quantile(.data$At, 0.50, na.rm = TRUE)),
    P90     = as.integer(stats::quantile(.data$At, 0.90, na.rm = TRUE)),
    .by = Marker
  ) |>
  dplyr::arrange(dplyr::desc(.data$Present)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "DotLoose is the permissive form of DotLeader and is reported so the strict pattern's misses are \\
   visible. A large gap between them means dot leaders survive the html conversion in a form the \\
   strict pattern does not match, and the pattern rather than the corpus is at fault."
)


# 3. How far does the front matter reach ----
# The TOC marker says a table of contents starts; it does not say where it ends. The end is taken as
# the last dot-leader line following the marker, which is what a contents list is made of. Documents
# with a marker but no dot leaders are counted separately: for those the extent is unknown, not zero.

.dot_all <- stringi::stri_locate_all_regex(tab_text$Head, .markers[["DotLeader"]])

tab_front <- tibble::tibble(
  DocID    = tab_text$DocID,
  DocLen   = tab_text$DocLen,
  TocAt    = stringi::stri_locate_first_regex(tab_text$Head, .markers[["TocMarker"]])[, 1],
  EdgarEnd = stringi::stri_locate_first_regex(tab_text$Head, .markers[["EdgarHeader"]])[, 2],
  NDots    = purrr::map_int(.dot_all, function(.m) if (all(is.na(.m))) 0L else nrow(.m)),
  LastDot  = purrr::map_int(.dot_all, function(.m) if (all(is.na(.m))) NA_integer_ else max(.m[, 2]))
) |>
  dplyr::mutate(
    # A dot-leader run before the marker is not a contents list, so the end is only taken where the
    # last run sits after it.
    TocEnd = dplyr::if_else(
      !is.na(.data$TocAt) & !is.na(.data$LastDot) & .data$LastDot > .data$TocAt,
      .data$LastDot,
      NA_integer_
    ),
    Family = dplyr::case_when(
      !is.na(.data$TocEnd)                    ~ "TOC with dot leaders",
      !is.na(.data$TocAt)                     ~ "TOC marker, extent unknown",
      !is.na(.data$LastDot)                   ~ "dot leaders, no marker",
      TRUE                                    ~ "no front matter detected"
    )
  )

cli::cli_h2("B. Front matter family")
tab_front |>
  dplyr::summarise(
    Docs      = dplyr::n(),
    Pct       = round(100 * dplyr::n() / nrow(tab_front), 1),
    MedDocLen = as.integer(stats::median(.data$DocLen)),
    MedTocEnd = as.integer(stats::median(.data$TocEnd, na.rm = TRUE)),
    .by = Family
  ) |>
  dplyr::arrange(dplyr::desc(.data$Docs)) |>
  print(n = Inf, width = Inf)

cli::cli_h2("B. Does front matter length rise with document length?")
tab_front |>
  dplyr::mutate(LenBin = dplyr::ntile(.data$DocLen, 10L)) |>
  dplyr::summarise(
    Docs      = dplyr::n(),
    MedDocLen = as.integer(stats::median(.data$DocLen)),
    PctToc    = round(100 * mean(!is.na(.data$TocAt)), 1),
    MedTocEnd = as.integer(stats::median(.data$TocEnd, na.rm = TRUE)),
    P90TocEnd = as.integer(stats::quantile(.data$TocEnd, 0.90, na.rm = TRUE)),
    .by = LenBin
  ) |>
  dplyr::arrange(.data$LenBin) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "This is the hypothesis in one table. If long documents carry a table of contents far more often \\
   AND a longer one, then front matter is what displaces the preamble, and the offset tail is an \\
   artifact of measuring from character zero."
)


# 4. The decisive test ----
# The same offsets, measured from four candidate origins. Read the Q90 column down the rows: flat
# means the origin explains the tail, rising means it does not.

tab_test <- tab_party |>
  dplyr::filter(.data$Status %in% c("located", "late"), !is.na(.data$Start)) |>
  dplyr::select(DocID, Class, Start, Status) |>
  dplyr::left_join(tab_front, by = dplyr::join_by(DocID)) |>
  dplyr::mutate(
    LenBin  = dplyr::ntile(.data$DocLen, 10L),
    OrigZero = 0L,
    OrigToc  = dplyr::coalesce(.data$TocEnd, 0L),
    OrigEdgar = dplyr::coalesce(.data$EdgarEnd, 0L),
    OrigBoth = pmax(dplyr::coalesce(.data$TocEnd, 0L), dplyr::coalesce(.data$EdgarEnd, 0L))
  )

report_origin_ <- function(.tab, .col, .label) {
  cli::cli_h3(.label)
  .tab |>
    dplyr::mutate(Rel = .data$Start - .data[[.col]]) |>
    dplyr::summarise(
      Docs      = dplyr::n(),
      MedDocLen = as.integer(stats::median(.data$DocLen)),
      Negative  = sum(.data$Rel < 0L),
      Q50       = as.integer(stats::quantile(.data$Rel, 0.50)),
      Q75       = as.integer(stats::quantile(.data$Rel, 0.75)),
      Q90       = as.integer(stats::quantile(.data$Rel, 0.90)),
      Q95       = as.integer(stats::quantile(.data$Rel, 0.95)),
      .by = LenBin
    ) |>
    dplyr::arrange(.data$LenBin) |>
    print(n = Inf, width = Inf)
  invisible(NULL)
}

cli::cli_h2("C. Party offset by document length, under four candidate origins")
report_origin_(tab_test, "OrigZero",  "Origin: character zero (the baseline)")
report_origin_(tab_test, "OrigEdgar", "Origin: end of the EDGAR exhibit header")
report_origin_(tab_test, "OrigToc",   "Origin: end of the table of contents")
report_origin_(tab_test, "OrigBoth",  "Origin: the later of the two")

cli::cli_alert_warning(
  "Negative counts documents whose party sits BEFORE the detected origin. A large count means the \\
   detection overshoots -- a dot-leader run somewhere other than a contents list, most likely -- and \\
   the origin is cutting off the very thing it is meant to locate."
)


# 5. The connector, asked as its own question ----
# Not a rival origin. If the bilateral connector sits within a short distance of the party in most
# documents, then the preamble can be found directly and no search window is needed for this
# purpose at all.

tab_conn <- tab_test |>
  dplyr::left_join(
    tibble::tibble(
      DocID  = tab_text$DocID,
      ConnAt = stringi::stri_locate_first_regex(
        tab_text$Head, .markers[["ConnectStrict"]]
      )[, 1]
    ),
    by = dplyr::join_by(DocID)
  ) |>
  dplyr::mutate(Gap = .data$Start - .data$ConnAt)

cli::cli_h2("D. Distance from the bilateral connector to the party")
tab_conn |>
  dplyr::summarise(
    Docs        = dplyr::n(),
    WithConn    = sum(!is.na(.data$ConnAt)),
    PctWithConn = round(100 * mean(!is.na(.data$ConnAt)), 1),
    MedGap      = as.integer(stats::median(.data$Gap, na.rm = TRUE)),
    PctWithin300 = round(100 * mean(abs(.data$Gap) <= 300L, na.rm = TRUE), 1),
    PctBefore   = round(100 * mean(.data$Gap < 0L, na.rm = TRUE), 1),
    .by = Status
  ) |>
  print(n = Inf, width = Inf)

cli::cli_h2("D. The same, by contract type")
tab_conn |>
  dplyr::filter(.data$Status == "located") |>
  dplyr::summarise(
    Docs         = dplyr::n(),
    PctWithConn  = round(100 * mean(!is.na(.data$ConnAt)), 1),
    MedGap       = as.integer(stats::median(.data$Gap, na.rm = TRUE)),
    PctWithin300 = round(100 * mean(abs(.data$Gap) <= 300L, na.rm = TRUE), 1),
    PctBefore    = round(100 * mean(.data$Gap < 0L, na.rm = TRUE), 1),
    .by = Class
  ) |>
  dplyr::arrange(dplyr::desc(.data$Docs)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "PctWithConn separates the two document families without any further work: an agreement made by \\
   and between parties has a counterparty and a stock incentive plan does not. PctBefore says how \\
   often the party is named ahead of the connector, which decides whether a region built around it \\
   has to look both ways."
)


# 6. Read the front matter ----
# Two samples. The first shows what a long detected front matter actually contains, so the dot-leader
# heuristic can be checked rather than trusted. The second shows the documents where the party was
# found late, which is the group the whole exercise is meant to recover.

show_head_ <- function(.tab, .from, .to, .title) {
  cli::cli_h3(.title)
  purrr::pwalk(.tab, function(DocID, ...) {
    row_ <- list(...)
    txt_ <- tab_text$Head[match(DocID, tab_text$DocID)]
    cat("-- ", DocID, " | DocLen ", row_$DocLen, " | party at ", row_$Start,
        " | TocEnd ", dplyr::coalesce(as.integer(row_$TocEnd), NA_integer_), "\n", sep = "")
    cat("   ", stringi::stri_replace_all_regex(
      stringi::stri_sub(txt_, from = .from, to = .to), "\\s+", " "
    ), "\n\n", sep = "")
  })
  invisible(NULL)
}

cli::cli_h2("E. Documents with the longest detected front matter")
tab_test |>
  dplyr::filter(!is.na(.data$TocEnd)) |>
  dplyr::slice_max(.data$TocEnd, n = .n_examples) |>
  dplyr::select(DocID, DocLen, Start, TocEnd) |>
  show_head_(.from = 1L, .to = 260L, .title = "Opening 260 characters")

cli::cli_h2("F. Documents where the party was found late")
tab_test |>
  dplyr::filter(.data$Status == "late") |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n_examples, nrow(.d)))))() |>
  dplyr::select(DocID, DocLen, Start, TocEnd) |>
  show_head_(.from = 1L, .to = 260L, .title = "Opening 260 characters")

cli::cli_alert_info(
  "Read the second block for one thing: whether the opening looks like front matter that a rule \\
   could skip, or like a document that simply never names its filer near the front. Those are \\
   different problems and only the first is fixed by moving the origin."
)
