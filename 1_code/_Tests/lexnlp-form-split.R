# Legal form: split it in R, or encode it in the string? ----
#
# TWO ROUTES TO THE SAME THING, AND THEY COST VERY DIFFERENT AMOUNTS
#
# ROUTE 1 -- SPLIT IN R. The legal form is a trailing substring of the span in 81% of annotations,
# and the vocabulary LexNLP matches it against is a static 143-row CSV shipped inside the package.
# If R can reproduce LexNLP's own split from the span alone, then nothing is re-extracted, no schema
# changes, and no string is encoded. This is tested FIRST, because if it works the second route is
# unnecessary.
#
# ROUTE 2 -- ENCODE IN THE STRING. Emit "Bank of America,[[[ N.A]]]" so one column carries both.
# Testable, but it carries a constraint that decides the whole design: THE 04 FAMILY'S CONTRACT IS
# text[Start:Stop] == Span, verified at 100% in 04A and again in 04B. A marked Span breaks it
# everywhere. So markers can only go in a column that is not Span -- LabelRaw is the candidate,
# since LexNLP writes the constant "company" there and it carries no information today.
#
# Even in that column the encoding must INSERT and never SUBSTITUTE. "Bank of America [[[N.A]]]"
# has lost a comma and cannot rebuild the span; "Bank of America,[[[ N.A]]]" can, by deleting the
# markers. This script tests the round trip rather than assuming it.
#
# PREDICTIONS, STATED BEFORE LOOKING
#   R1  The R split reproduces LexNLP's Name in over 90% of annotations.
#   R2  Where it disagrees, the cause is LexNLP's extra trims (Borrower, <X> Agent, numeric
#       prefixes) rather than the legal form itself.
#   R3  Square brackets are UNSAFE as markers: EDGAR redactions are written [***] and the 04A read
#       examples show them inside contract text.
#   R4  Roughly 6% of LexNLP organisations carry a legal form no US filing plausibly has -- NV
#       (Naamloze vennootschap) and SC (Sociedad Colectiva) -- because those are state postal codes
#       in an address line.
#
# Reads only. Requires 2_output/_Probe/lexnlp-fields/out/company_fields.parquet from lexnlp-fields.R.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_probe  <- here::here("2_output", "_Probe", "lexnlp-fields")
.path_ann   <- fs::path(.dir_probe, "out", "company_fields.parquet")
.path_store <- here::here("2_output", "04A-EntityExtract", "Store", "EntityCandidates.duckdb")
.path_vocab <- fs::path(.dir_probe, "company_types.csv")   # cached beside the probe output
.dir_script <- here::here("contracts-lexnlp")              # where the shipped copy is looked for

.image      <- "contracts-lexnlp"
.n_read     <- 25L
.seed       <- 42L

stopifnot(fs::file_exists(c(.path_ann, .path_store)))

tab_ann <- arrow::read_parquet(.path_ann) |> tibble::as_tibble()
cli::cli_alert_info("{nrow(tab_ann)} annotation{?s} with LexNLP's own split as ground truth.")


# 2. The legal-form vocabulary ----
# Lifted out of the image rather than retyped, so the R side matches against exactly the list the
# extractor used. 143 rows: Alias is the surface form found in text, Abbreviation the normalised
# short form, Label the legal category.
#
# COMPANY_DESCRIPTIONS is a seven-element Python list rather than a data file, so it is written out
# here. It is the OTHER way the pattern can fire: 20.8% of annotations carry a description and no
# type at all, which is why "the pattern needs a legal form" is true only if a description counts
# as one.

.descriptions <- c("Trust Bank", "Trust Company", "Trust", "Bank", "Company", "Partnership",
                   "Agency")

# Three sources, tried in order. The shipped copy is preferred because it removes a container call
# from a script that otherwise needs none; the two container routes exist so the file can be
# refreshed from the image if the pinned LexNLP version ever moves.
#
# BOTH CONTAINER CALLS QUOTE THEIR ARGUMENT. system2() pastes arguments into one command line
# without quoting them, so an unquoted Python expression is handed to the shell, which reads its
# parentheses and quotes as its own syntax and fails before docker is reached.
if (!fs::file_exists(.path_vocab)) {
  src_ <- fs::path(.dir_script, "company_types.csv")
  if (fs::file_exists(src_)) {
    fs::file_copy(src_, .path_vocab)
    cli::cli_alert_success("Vocabulary taken from {.path {(src_)}}")
  } else {
    # No string literals in the expression, so only the spaces need protecting.
    py_ <- "import lexnlp.config.en as m, os, sys; sys.stdout.write(os.path.dirname(m.__file__))"
    dir_ <- system2("docker",
                    c("run", "--rm", "--entrypoint", "python", .image, "-c", shQuote(py_)),
                    stdout = TRUE, stderr = FALSE)
    dir_ <- utils::tail(dir_[nzchar(dir_)], 1L)
    out_ <- if (length(dir_) == 1L && nzchar(dir_)) {
      system2("docker",
              c("run", "--rm", "--entrypoint", "cat", .image,
                paste0(dir_, "/company_types.csv")),
              stdout = TRUE, stderr = FALSE)
    } else {
      character(0)
    }
    if (length(out_) < 10L) {
      cli::cli_abort(c(
        "Could not read company_types.csv from the image.",
        "i" = "Copy it into {.path {(src_)}} and re-run; the file is static and ships with LexNLP."
      ))
    }
    writeLines(out_, .path_vocab)
    cli::cli_alert_success("Vocabulary cached: {.path {(.path_vocab)}}")
  }
}

tab_vocab <- readr::read_csv(.path_vocab, show_col_types = FALSE) |>
  dplyr::rename(Alias = "Alias", Abbr = "Abbreviation", Label = "Label") |>
  dplyr::filter(!is.na(.data$Alias), nzchar(.data$Alias)) |>
  dplyr::distinct(.data$Alias, .keep_all = TRUE) |>
  dplyr::arrange(dplyr::desc(nchar(.data$Alias)))   # longest first, as LexNLP sorts it

cli::cli_h2("The legal-form vocabulary")
cli::cli_alert_info(
  "{nrow(tab_vocab)} alias{?/es} mapping to {dplyr::n_distinct(tab_vocab$Abbr)} abbreviation{?s} \\
   and {dplyr::n_distinct(tab_vocab$Label)} legal categor{?y/ies}. 04B strips 19 suffixes by hand."
)


# 3. The R-side split ----
# Reproduces what LexNLP does to turn a matched span into a name, in the same order:
# strip the trailing type, remove the Borrower / <X> Agent false positives, strip surrounding
# punctuation, drop a leading or trailing and / & / of, drop a numeric-date prefix.

esc_ <- function(.x) stringi::stri_replace_all_regex(.x, "([\\\\.^$|()\\[\\]{}*+?])", "\\\\$1")

.alt_type <- paste(esc_(tab_vocab$Alias), collapse = "|")
.alt_desc <- paste(esc_(.descriptions), collapse = "|")

# The trailing chunk is separator + alias + trailing punctuation, anchored at the end.
.rgx_type <- paste0("([\\s,\\.]*\\b(", .alt_type, ")[\\.,\\s]*)$")

#' Split a span into its name and its trailing legal form
#'
#' @param .x Character vector of raw spans.
#' @param .repeat Logical. TRUE strips repeated forms ("BANK CO LTD"); LexNLP strips one.
#' @return Tibble: Span, NameR, FormR, AbbrR, LabelR.
split_form_ <- function(.x, .repeat = FALSE) {
  if (FALSE) {
    .x      <- c("Bank of America, N.A", "The Boeing Company", "NORTH LAS VEGAS NV")
    .repeat <- FALSE
  }
  opts_ <- stringi::stri_opts_regex(case_insensitive = TRUE)

  cur_   <- stringi::stri_trim_both(stringi::stri_replace_all_regex(.x, "\\s+", " "))
  form_  <- rep(NA_character_, length(cur_))
  keep_  <- rep(TRUE, length(cur_))

  repeat {
    m_   <- stringi::stri_match_first_regex(cur_, .rgx_type, opts_regex = opts_)
    hit_ <- keep_ & !is.na(m_[, 1]) & nchar(m_[, 1]) < nchar(cur_)
    if (!any(hit_, na.rm = TRUE)) break
    form_[hit_] <- dplyr::coalesce(m_[hit_, 3], form_[hit_])
    cur_[hit_]  <- stringi::stri_sub(cur_[hit_], from = 1L,
                                     to = nchar(cur_[hit_]) - nchar(m_[hit_, 1]))
    if (!.repeat) break
  }

  # LexNLP's remaining trims, in its order.
  cur_ <- cur_ |>
    stringi::stri_replace_all_regex("(?i)(?:the\\s+)?Borrower|\\p{L}+\\s+Agent", "") |>
    stringi::stri_replace_all_regex("^[^A-Za-z0-9&]+|[^A-Za-z0-9&\\)]+$", "") |>
    stringi::stri_replace_all_regex("(?i)^\\s*(?:and|&|of)\\s+|\\s+(?:and|&|of)\\s*$", "") |>
    stringi::stri_replace_first_regex("^\\d\\d[0-9\\.\\s,\\-]*", "") |>
    stringi::stri_trim_both()

  look_ <- tab_vocab[match(stringi::stri_trans_toupper(form_),
                           stringi::stri_trans_toupper(tab_vocab$Alias)), ]

  tibble::tibble(
    Span   = .x,
    NameR  = cur_,
    FormR  = form_,
    AbbrR  = look_$Abbr,
    LabelR = look_$Label
  )
}


# 4. Block A: does the R split reproduce LexNLP's own? ----
# The block that decides whether anything has to change at all. Compared on the flattened forms,
# because LexNLP replaces newlines with spaces before it starts and the store keeps the raw text.

flat_ <- function(.x) stringi::stri_trim_both(stringi::stri_replace_all_regex(.x, "\\s+", " "))

tab_split <- split_form_(.x = tab_ann$SpanText, .repeat = FALSE) |>
  dplyr::mutate(
    DocID    = tab_ann$DocID,
    NameLex  = flat_(tab_ann$Name),
    AbbrLex  = tab_ann$TypeAbbr,
    LabelLex = tab_ann$TypeLabel,
    NameR    = flat_(.data$NameR),
    SameName = .data$NameR == .data$NameLex,
    SameAbbr = dplyr::coalesce(.data$AbbrR, "-") == dplyr::coalesce(.data$AbbrLex, "-")
  )

cli::cli_h2("A. R split against LexNLP's own split")
tibble::tibble(
  Item = c("Annotations", "Name reproduced exactly", "Legal form reproduced exactly",
           "Both reproduced exactly", "R found a form where LexNLP found none",
           "LexNLP found a form where R found none"),
  N    = c(
    nrow(tab_split),
    sum(tab_split$SameName),
    sum(tab_split$SameAbbr),
    sum(tab_split$SameName & tab_split$SameAbbr),
    sum(!is.na(tab_split$AbbrR) & is.na(tab_split$AbbrLex)),
    sum(is.na(tab_split$AbbrR) & !is.na(tab_split$AbbrLex))
  )
) |>
  dplyr::mutate(Pct = round(100 * .data$N / nrow(tab_split), 1)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "R1 predicted over 90% on the name. If it holds, the legal form needs no re-extraction, no schema \\
   change and no encoding: it is a function of the span and a static 143-row lookup."
)

cli::cli_h2("A. The same, with repeated forms stripped")
split_form_(.x = tab_ann$SpanText, .repeat = TRUE) |>
  dplyr::mutate(NameLex = flat_(tab_ann$Name), NameR = flat_(.data$NameR)) |>
  dplyr::summarise(
    N        = dplyr::n(),
    SameName = sum(.data$NameR == .data$NameLex),
    Pct      = round(100 * mean(.data$NameR == .data$NameLex), 1)
  ) |>
  print(width = Inf)

cli::cli_alert_info(
  "LexNLP strips ONE form. 04B's suffix loop strips repeated ones, so this row says which \\
   behaviour to keep: 'BANK CO LTD' is one company written with three tokens of legal form."
)

cli::cli_h2("A. Where the two disagree")
tab_split |>
  dplyr::filter(!.data$SameName) |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n_read, nrow(.d)))))() |>
  dplyr::select(Span, NameR, NameLex, FormR, AbbrLex) |>
  print(n = Inf, width = Inf)


# 5. Block B: is a marker encoding safe? ----
# Two questions and both have to pass. Does the marker collide with characters that occur in real
# contract spans, and does stripping the markers rebuild the span exactly.
#
# EDGAR REDACTIONS ARE WRITTEN [***]. That is visible in 04A's own read examples -- "CERTAIN
# CONFIDENTIAL INFORMATION ... MARKED BY [***], HAS BEEN OMITTED" -- so square brackets are the
# first thing to check rather than the obvious choice.

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("ATTACH '", fs::path_abs(.path_store), "' AS s (READ_ONLY)"))

tab_all <- DBI::dbGetQuery(con, paste0(
  "SELECT Span FROM s.candidates WHERE Engine = 'lexnlp' AND Label = 'ORG' AND Span IS NOT NULL"
)) |>
  tibble::as_tibble()

DBI::dbDisconnect(con, shutdown = TRUE)

.markers <- tibble::tibble(
  Name  = c("triple square", "double square", "single square", "double angle", "double brace",
            "triple pipe", "unit separator"),
  Open  = c("[[[", "[[", "[", "<<", "{{", "|||", "\u001f"),
  Close = c("]]]", "]]", "]", ">>", "}}", "|||", "\u001f")
)

cli::cli_h2("B. Marker collisions in {nrow(tab_all)} stored organisation span{?s}")
.markers |>
  dplyr::mutate(
    Hits = purrr::map2_int(.data$Open, .data$Close, function(.o, .c) {
      sum(stringi::stri_detect_fixed(tab_all$Span, .o) |
            stringi::stri_detect_fixed(tab_all$Span, .c))
    })
  ) |>
  dplyr::mutate(Pct = round(100 * .data$Hits / nrow(tab_all), 3)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_warning(
  "A nonzero count is disqualifying, not merely untidy: a marker that occurs in the text cannot be \\
   removed to rebuild the span, and the rebuild is the only reason the encoding is admissible."
)

# The encoding, built as a pure INSERTION so that deleting the markers restores the span byte for
# byte. Substituting instead -- writing "Bank of America [[[N.A]]]" for "Bank of America, N.A" --
# loses the comma and the span can never be recovered.
encode_ <- function(.span, .form, .open, .close) {
  if (FALSE) {
    .span  <- "Bank of America, N.A"
    .form  <- "N.A"
    .open  <- "[[["
    .close <- "]]]"
  }
  at_ <- stringi::stri_locate_last_fixed(.span, .form)[, 1]
  dplyr::if_else(
    is.na(.form) | is.na(at_),
    .span,
    paste0(stringi::stri_sub(.span, from = 1L, to = at_ - 1L), .open,
           stringi::stri_sub(.span, from = at_), .close)
  )
}

decode_ <- function(.x, .open, .close) {
  .x |>
    stringi::stri_replace_all_fixed(.open, "") |>
    stringi::stri_replace_all_fixed(.close, "")
}

.open  <- "\u001f"   # unit separator, tested above
.close <- "\u001f"

tab_enc <- tab_split |>
  dplyr::mutate(
    SpanFlat = flat_(.data$Span),
    Encoded  = encode_(.span = .data$SpanFlat, .form = .data$FormR, .open = .open, .close = .close),
    Rebuilt  = decode_(.x = .data$Encoded, .open = .open, .close = .close),
    RoundTrip = .data$Rebuilt == .data$SpanFlat
  )

cli::cli_h2("B. Does the encoding round-trip?")
tibble::tibble(
  Item = c("Encoded", "Rebuilt identical to the span", "Carried a form to encode"),
  N    = c(nrow(tab_enc), sum(tab_enc$RoundTrip), sum(!is.na(tab_enc$FormR)))
) |>
  dplyr::mutate(Pct = round(100 * .data$N / nrow(tab_enc), 1)) |>
  print(n = Inf, width = Inf)

cli::cli_h2("B. What the encoded string looks like, markers shown as {{ }}")
tab_enc |>
  dplyr::filter(!is.na(.data$FormR)) |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = 12L)))() |>
  dplyr::transmute(
    SpanFlat,
    Encoded = .data$Encoded |>
      stringi::stri_replace_all_fixed(.open, "{{") |>
      stringi::stri_replace_all_fixed(.close, "}}")
  ) |>
  print(n = Inf, width = Inf)


# 6. Block C: what does knowing the legal form actually buy? ----
# The probe found NV mapping to Naamloze vennootschap and SC to Sociedad Colectiva at 3.7% and 2.3%
# of annotations. Those are state postal codes in an address line, not Dutch and Spanish companies.
# A legal form no US filing plausibly carries is a cheap and precise noise filter -- and unlike a
# stoplist it needs no frequency threshold.

.implausible <- c("Naamloze vennootschap", "Sociedad Colectiva", "Sociedad Anonima",
                  "Sociedad Limitada", "Societa per azioni", "yugen-kaisha", "godo-kaisha")

cli::cli_h2("C. Annotations whose legal form is implausible for a US filing")
tab_ann |>
  dplyr::mutate(
    Implausible = stringi::stri_trans_general(dplyr::coalesce(.data$TypeLabel, ""), "Latin-ASCII")
      %in% .implausible
  ) |>
  dplyr::summarise(
    Annotations = dplyr::n(),
    Flagged     = sum(.data$Implausible),
    Docs        = dplyr::n_distinct(.data$DocID),
    DocsFlagged = dplyr::n_distinct(.data$DocID[.data$Implausible])
  ) |>
  dplyr::mutate(PctFlagged = round(100 * .data$Flagged / .data$Annotations, 1)) |>
  print(width = Inf)

cli::cli_h2("C. What those spans actually are")
tab_ann |>
  dplyr::filter(.data$TypeAbbr %in% c("NV", "SC", "SA", "Spa", "YK", "GK", "SL")) |>
  dplyr::count(.data$TypeAbbr, .data$SpanText, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = 25, width = Inf)

cli::cli_alert_info(
  "R4 predicted about 6%. Read the spans: if they are city-plus-state address fragments then the \\
   filter is a form test, not a place test, and it costs one lookup rather than a gazetteer."
)


# 7. Verdict ----

cli::cli_h2("Verdict")
tibble::tibble(
  Question = c(
    "Can R reproduce the split without re-extraction?",
    "Is a marker encoding round-trip safe?",
    "Does the form flag remove address fragments?"
  ),
  Answer = c(
    paste0(round(100 * mean(tab_split$SameName), 1), "% of names reproduced"),
    paste0(round(100 * mean(tab_enc$RoundTrip), 1), "% round-trip on the tested marker"),
    paste0(sum(tab_ann$TypeAbbr %in% c("NV", "SC"), na.rm = TRUE), " annotations flagged by NV/SC")
  )
) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "If the first row is high, the second and third are the only ones that matter: the split is free \\
   and the question left is whether the legal form is worth carrying as its own column at all."
)
