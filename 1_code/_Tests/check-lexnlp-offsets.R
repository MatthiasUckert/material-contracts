# Is LexNLP's Start pointing where its Name actually is? ----
#
# WHAT THIS ASKS
# LexNLP builds a company annotation from two things computed separately: `name`, which is the regex
# match after three cleaning passes, and `coords`, which is the raw match offset. The offset is
# (match.start() + sentence_start), and the sentence start comes from get_sentence_span(), which
# tokenises a NORMALISED copy of the text and applies the resulting boundaries to the ORIGINAL. Where
# normalisation changes a character count, every offset after that point shifts.
#
# WHY THE EXISTING TEST CANNOT SEE IT
# T3 asserts Span == text[Start:Stop]. We BUILT Span by slicing at those offsets, so it passes by
# construction whatever the offsets are -- it proves we stored what we sliced, not that we sliced the
# right place. The check that bites is whether the engine's own resolved name is inside the span it
# claims to have found it in.
#
# WHAT TO READ IN THE OUTPUT
#   Block 3  the headline rate. NameInSpan near 100% means the offsets are sound; well below means
#            the store's LexNLP positions are shifted and every offset-keyed join against them is
#            approximate.
#   Block 4  the mismatches, with the text at Start and the text where the name actually is. If the
#            two are the same LENGTH and different POSITION, it is a shift; if the span is longer, it
#            is the cleaning passes and harmless.
#   Block 5  drift against position in the document. A shift that accumulates grows with Start, which
#            is the difference between a nuisance and a reason not to trust deep offsets.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr.


# 1. Configuration ----

.path_store <- here::here("2_output", "04A-EntityExtract", "Store", "EntityCandidates.duckdb")
.path_text  <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")
.n_docs     <- 150L    # documents sampled; every LexNLP ORG row in them is checked
.n_read     <- 12L     # mismatches printed in full
.seed       <- 42L

stopifnot(fs::file_exists(c(.path_store, .path_text)))


# 2. Pull LexNLP's ORG rows and rehydrate them ----

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("ATTACH '", fs::path_abs(.path_store), "' AS s (READ_ONLY)"))

docs_ <- DBI::dbGetQuery(con, paste0(
  "SELECT DISTINCT DocID FROM s.org WHERE Engine = 'lexnlp' AND NameCore IS NOT NULL"
))$DocID
docs_ <- withr::with_seed(.seed, sample(docs_, size = min(.n_docs, length(docs_))))

tab_org <- DBI::dbGetQuery(con, paste0(
  "SELECT DocID, Start, Stop, Span, NameCore, LegalForm, Description ",
  "FROM s.org WHERE Engine = 'lexnlp' AND NameCore IS NOT NULL AND DocID IN ('",
  paste(docs_, collapse = "', '"), "')"
)) |>
  tibble::as_tibble()

DBI::dbDisconnect(con, shutdown = TRUE)

tab_text <- arrow::read_parquet(.path_text) |>
  dplyr::filter(.data$DocID %in% docs_) |>
  dplyr::select(DocID, TextRaw)

cli::cli_alert_info(
  "{format(nrow(tab_org), big.mark = ',')} LexNLP ORG row{?s} across \\
   {length(docs_)} document{?s}."
)


# 3. Is the resolved name inside the span it was found in? ----
# THE FIRST TWO COLUMNS ARE DIFFERENT QUESTIONS. NameInSpan asks whether the offsets point at the
# entity. SpanRoundTrip is T3 -- whether the stored Span equals a fresh slice at those offsets -- and
# it must be 100% because that is how Span was built. It is here to make the point that it can be
# 100% while the first column is not.

tab_chk <- tab_org |>
  dplyr::left_join(tab_text, by = dplyr::join_by(DocID)) |>
  dplyr::mutate(
    Sliced     = stringi::stri_sub(.data$TextRaw, from = .data$Start + 1L, to = .data$Stop),
    RoundTrip  = .data$Sliced == .data$Span,
    NameInSpan = stringi::stri_detect_fixed(.data$Span, .data$NameCore),
    # Where the name really is, taken as the occurrence NEAREST to the claimed Start -- a company is
    # usually named several times and the nearest occurrence is the conservative reading, since it
    # reports the smallest drift consistent with the data.
    #
    # pmap over the three columns rather than an index into the unjoined table: the join is 1:1 and
    # order-preserving today, and a lookup that depends on that is a lookup that breaks the first
    # time a document appears twice in the text file.
    TrueStart  = purrr::pmap_int(
      list(.data$TextRaw, .data$NameCore, .data$Start),
      function(.t, .n, .s) {
        hits_ <- stringi::stri_locate_all_fixed(.t, .n)[[1]]
        if (all(is.na(hits_))) return(NA_integer_)
        cand_ <- as.integer(hits_[, 1]) - 1L
        cand_[which.min(abs(cand_ - .s))]
      }
    ),
    Drift    = .data$TrueStart - .data$Start,
    SpanLen  = stringi::stri_length(.data$Span),
    NameLen  = stringi::stri_length(.data$NameCore)
  )

cli::cli_h2("Do the offsets point at the entity?")
tibble::tibble(
  Check = c("T3  Span == a fresh slice at Start:Stop",
            "    NameCore appears inside Span",
            "    NameCore appears anywhere in the document",
            "    Start is exactly where NameCore begins"),
  N = c(sum(tab_chk$RoundTrip),
        sum(tab_chk$NameInSpan),
        sum(!is.na(tab_chk$TrueStart)),
        sum(tab_chk$Drift == 0L, na.rm = TRUE)),
  Of = nrow(tab_chk)
) |>
  dplyr::mutate(Pct = round(100 * .data$N / .data$Of, 1)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "The first line passes by construction and proves nothing about position. The second is the one \\
   that bites."
)


# 4. The mismatches, read ----
# SpanLen against NameLen is the discriminator. Equal lengths in different places is a SHIFT -- the
# match was the right size and the offset was wrong. A span longer than the name is the cleaning
# passes, which strip punctuation and prefixes from `name` and leave `coords` on the raw match, and
# that is harmless.

bad_ <- dplyr::filter(tab_chk, !.data$NameInSpan)

cli::cli_h2("Where the name is not inside the span")
if (nrow(bad_) == 0L) {
  cli::cli_alert_success("None. The offsets point at the entity in every row checked.")
} else {
  bad_ |>
    dplyr::mutate(
      Kind = dplyr::case_when(
        is.na(.data$TrueStart)             ~ "name not in document at all",
        .data$SpanLen == .data$NameLen     ~ "same length, wrong place -- SHIFT",
        .data$SpanLen >  .data$NameLen     ~ "span longer than name -- cleaning",
        TRUE                               ~ "span shorter than name"
      )
    ) |>
    dplyr::count(.data$Kind, name = "N") |>
    dplyr::arrange(dplyr::desc(.data$N)) |>
    print(n = Inf, width = Inf)

  cli::cli_h3("A dozen, in full")
  show_ <- withr::with_seed(.seed, dplyr::slice_sample(bad_, n = min(.n_read, nrow(bad_))))
  purrr::walk(seq_len(nrow(show_)), function(.i) {
      r_ <- show_[.i, ]
      cat(sprintf("\n  NameCore  %s\n", r_$NameCore))
      cat(sprintf("  Span      %-40s [%d-%d, %d chars]\n",
                  paste0("'", stringi::stri_replace_all_regex(r_$Span, "\\s+", " "), "'"),
                  r_$Start, r_$Stop, r_$SpanLen))
      if (!is.na(r_$TrueStart)) {
        cat(sprintf("  Actually  '%s' at %d  (drift %+d)\n",
                    stringi::stri_replace_all_regex(
                      stringi::stri_sub(r_$TextRaw, from = r_$TrueStart + 1L,
                                        to = r_$TrueStart + r_$SpanLen), "\\s+", " "),
                    r_$TrueStart, r_$Drift))
      } else {
        cat("  Actually  NameCore does not occur in the document -- cleaning invented it\n")
      }
    })
}


# 5. Does the drift accumulate? ----
# A CONSTANT OFFSET IS A NUISANCE; ONE THAT GROWS WITH POSITION IS A REASON NOT TO TRUST DEEP SPANS.
# The party rule reads the head, where accumulation is smallest, so a rising profile would matter
# less there than it does for a notices clause or a signature block.

drift_ <- dplyr::filter(tab_chk, !is.na(.data$Drift))

cli::cli_h2("Drift by position in the document")
if (nrow(drift_) == 0L) {
  cli::cli_alert_warning("No row could be located; nothing to profile.")
} else {
  drift_ |>
    dplyr::mutate(Decile = pmin(10L, 1L + as.integer(
      .data$Start / stringi::stri_length(.data$TextRaw) * 10
    ))) |>
    dplyr::summarise(
      Rows      = dplyr::n(),
      Exact     = sum(.data$Drift == 0L),
      PctExact  = round(100 * mean(.data$Drift == 0L), 1),
      MedDrift  = stats::median(.data$Drift),
      MaxAbs    = max(abs(.data$Drift)),
      .by = Decile
    ) |>
    dplyr::arrange(.data$Decile) |>
    print(n = Inf, width = Inf)

  cli::cli_alert_info(
    "PctExact falling as Decile rises is accumulation. Flat and high means the offsets are sound and \\
     the mismatches above are the cleaning passes, which cost nothing."
  )
}
