# ORG smoke test: five documents, every annotation read ----
#
# WHAT THIS IS
# The first of one smoke test per entity extraction. Five documents, small enough that every
# annotation can be printed and looked at, rather than summarised into a statistic that is
# consistent with two different stories. Nothing here is a measurement; it exists to answer
# "does this look right" before anything is committed to the store schema.
#
# THE CHECK THAT MATTERS MOST IS THE FIRST ONE
# The proposed change adds columns to what LexNLP emits. It must not change WHAT it finds. So the
# probe output is diffed against the rows already in 04A's store for the same five documents, on
# offsets and spans. An identical diff means the extra fields are free; a non-identical one means
# something else moved, and finding that out on five documents costs minutes rather than a rebuild.
#
# WHAT ORG WOULD CARRY
# Core, as today:      DocID, Start, Stop, Span, LabelRaw, Engine, Model
# Proposed additions:  LegalForm  -- LexNLP's normalised company type abbreviation (CORP, LLC, NA)
#                      Description -- Bank, Trust, Company, Partnership, Agency, where the pattern
#                                     fired on a description rather than a type
#
# Requires contracts-lexnlp/probe_company_fields.py, which already emits every field.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_04a    <- here::here("2_output", "04A-EntityExtract")
.path_text  <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys  <- fs::path(.dir_04a, "sample_anchors.parquet")
.path_store <- fs::path(.dir_04a, "Store", "EntityCandidates.duckdb")

.dir_probe  <- fs::dir_create(here::here("2_output", "_Probe", "org-smoke"))
.dir_in     <- fs::dir_create(fs::path(.dir_probe, "in"))
.dir_out    <- fs::dir_create(fs::path(.dir_probe, "out"))
.dir_script <- here::here("contracts-lexnlp")

.n_docs     <- 5L      # small enough to read every annotation
.len_min    <- 4000L   # long enough to have a real preamble
.len_max    <- 25000L  # short enough that the annotation list fits on a screen
.ctx        <- 120L    # characters either side of a span in the context block
.open_chars <- 320L    # opening of each document, printed so the preamble is visible
.n_process  <- 5L
.timeout    <- 240L
.image      <- "contracts-lexnlp"
.rerun      <- TRUE    # five documents cost seconds; always re-measure
.seed       <- 42L

stopifnot(fs::file_exists(c(.path_text, .path_keys, .path_store)))
stopifnot(fs::file_exists(fs::path(.dir_script, "probe_company_fields.py")))


# 2. Choose five documents ----
# One per contract type rather than five at random, so the five cover different drafting
# conventions. Length-bounded so every annotation can be printed.

tab_text <- arrow::read_parquet(.path_text) |>
  dplyr::mutate(DocLen = stringi::stri_length(.data$TextRaw))

tab_keys <- arrow::read_parquet(.path_keys) |>
  dplyr::select(DocID, Class = "ClassDetailed", CompanyName, AmendType)

tab_pick <- tab_text |>
  dplyr::select(DocID, DocLen) |>
  dplyr::inner_join(tab_keys, by = dplyr::join_by(DocID)) |>
  dplyr::filter(.data$DocLen >= .len_min, .data$DocLen <= .len_max) |>
  dplyr::arrange(.data$DocID) |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = 1L, by = Class)))() |>
  dplyr::arrange(.data$Class) |>
  head(.n_docs)

cli::cli_h2("The five documents")
tab_pick |>
  dplyr::select(DocID, Class, CompanyName, DocLen) |>
  print(n = Inf, width = Inf)

tab_text |>
  dplyr::filter(.data$DocID %in% tab_pick$DocID) |>
  dplyr::select(DocID, TextRaw) |>
  arrow::write_parquet(fs::path(.dir_in, "sample.parquet"))


# 3. Extract ----

.path_ann <- fs::path(.dir_out, "company_fields.parquet")

if (.rerun || !fs::file_exists(.path_ann)) {
  args_ <- c(
    "run", "--rm",
    "-v", paste0(as.character(fs::path_real(.dir_in)), ":/work:ro"),
    "-v", paste0(as.character(fs::path_real(.dir_out)), ":/out"),
    "-v", paste0(as.character(fs::path_real(.dir_script)), ":/probe:ro"),
    "--entrypoint", "python",
    .image,
    "/probe/probe_company_fields.py", "/work/sample.parquet",
    "--output", "/out/company_fields.parquet",
    "--output-docs", "/out/company_docs.parquet",
    "--n-process", as.integer(.n_process),
    "--chunk-size", 1L,
    "--timeout", as.integer(.timeout)
  )
  status_ <- system2("docker", args_, stdout = "", stderr = "")
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("Probe container failed ({status_}).")
}

tab_ann <- arrow::read_parquet(.path_ann) |>
  tibble::as_tibble() |>
  dplyr::arrange(.data$DocID, .data$Start)

cli::cli_alert_success("{nrow(tab_ann)} annotation{?s} over {dplyr::n_distinct(tab_ann$DocID)} document{?s}.")


# 4. Check 1: does the probe reproduce the store, exactly? ----
# THE DECISIVE CHECK. The proposed change adds fields; it must not change which spans are found or
# where they sit. Compared on the offset pair rather than on the text, because two spans at the same
# offsets with different text would be a far worse defect than a count that moved.

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("ATTACH '", fs::path_abs(.path_store), "' AS s (READ_ONLY)"))

tab_store <- DBI::dbGetQuery(con, paste0(
  "SELECT DocID, Start, Stop, Span FROM s.candidates ",
  "WHERE Engine = 'lexnlp' AND Label = 'ORG' AND DocID IN ('",
  paste(tab_pick$DocID, collapse = "', '"), "')"
)) |>
  tibble::as_tibble() |>
  dplyr::arrange(.data$DocID, .data$Start)

DBI::dbDisconnect(con, shutdown = TRUE)

cmp_ <- dplyr::full_join(
  dplyr::transmute(tab_store, DocID, Start, Stop, SpanStore = .data$Span),
  dplyr::transmute(tab_ann, DocID, Start, Stop, SpanProbe = .data$SpanText),
  by = dplyr::join_by(DocID, Start, Stop)
)

cli::cli_h2("1. Probe against the store")
tibble::tibble(
  Check = c("Rows in the store", "Rows from the probe", "Offset pairs matched",
            "In the store only", "In the probe only", "Matched but different text"),
  N = c(
    nrow(tab_store), nrow(tab_ann),
    sum(!is.na(cmp_$SpanStore) & !is.na(cmp_$SpanProbe)),
    sum(is.na(cmp_$SpanProbe)), sum(is.na(cmp_$SpanStore)),
    sum(!is.na(cmp_$SpanStore) & !is.na(cmp_$SpanProbe) & cmp_$SpanStore != cmp_$SpanProbe)
  )
) |>
  print(n = Inf, width = Inf)

if (any(is.na(cmp_$SpanStore)) || any(is.na(cmp_$SpanProbe))) {
  cli::cli_alert_danger("The probe and the store disagree. Rows below; do NOT proceed until explained.")
  cmp_ |>
    dplyr::filter(is.na(.data$SpanStore) | is.na(.data$SpanProbe)) |>
    print(n = 40, width = Inf)
} else {
  cli::cli_alert_success("Identical: the extra fields cost nothing in what is found.")
}


# 5. Check 2: the offset contract ----
# text[Start:Stop] == Span, on the canonical text every offset in this family indexes. This is the
# one invariant nothing downstream re-derives, so it is checked on every annotation, not a sample.

tab_chk <- tab_ann |>
  dplyr::left_join(dplyr::select(tab_text, DocID, TextRaw), by = dplyr::join_by(DocID)) |>
  dplyr::mutate(
    # 0-based half-open offsets over code points; stri_sub is 1-based inclusive.
    Sliced    = stringi::stri_sub(.data$TextRaw, from = .data$Start + 1L, to = .data$Stop),
    RoundTrip = .data$Sliced == .data$SpanText,
    NameInSpan = stringi::stri_detect_fixed(.data$SpanText, dplyr::coalesce(.data$Name, "")),
    HasForm   = !is.na(.data$TypeAbbr) | !is.na(.data$Description)
  )

cli::cli_h2("2. Invariants")
tibble::tibble(
  Check = c("Span equals text[Start:Stop]", "Name is a substring of Span",
            "Carries a type or a description", "Start < Stop", "Distinct offset pairs"),
  N = c(
    sum(tab_chk$RoundTrip), sum(tab_chk$NameInSpan), sum(tab_chk$HasForm),
    sum(tab_chk$Start < tab_chk$Stop),
    nrow(dplyr::distinct(tab_chk, .data$DocID, .data$Start, .data$Stop))
  ),
  Of = nrow(tab_chk)
) |>
  dplyr::mutate(Pct = round(100 * .data$N / .data$Of, 1)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "Row 1 below 100% is fatal and stops everything. Row 3 below 100% would refute the claim that \\
   the pattern cannot fire without a legal form or a description. Row 5 below the row count means \\
   the same offsets are returned twice, which would double-count in every aggregate."
)


# 6. The proposed org table ----
# Exactly the columns the split would create. Printed rather than described, so the shape can be
# argued about before it is written into ner_db_init().

tab_org <- tab_chk |>
  dplyr::transmute(
    DocID,
    Start,
    Stop,
    Span        = .data$SpanText,
    LabelRaw    = "company",                 # unchanged: the engine's own label
    LegalForm   = .data$TypeAbbr,            # proposed: CORP, LLC, NA, LP
    Description = .data$Description,         # proposed: Bank, Trust, Company, Partnership, Agency
    Engine      = "lexnlp",
    Model       = "lexnlp"
  )

cli::cli_h2("3. What the org table would hold")
tab_org |>
  dplyr::count(.data$LegalForm, .data$Description, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = Inf, width = Inf)


# 7. Read every annotation, document by document ----
# The block the whole script exists for. Five documents, every span, with the EDGAR company name
# beside them so the anchor is visible without a join.

norm_ <- function(.x) {
  .x |>
    stringi::stri_trans_toupper() |>
    stringi::stri_replace_all_regex("[^A-Z0-9 ]", " ") |>
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_trim_both()
}

cli::cli_h2("4. Every annotation, in order")

purrr::walk(tab_pick$DocID, function(.d) {
  meta_ <- dplyr::filter(tab_pick, .data$DocID == .d)
  txt_  <- tab_text$TextRaw[match(.d, tab_text$DocID)]
  rows_ <- dplyr::filter(tab_chk, .data$DocID == .d)
  key_  <- norm_(meta_$CompanyName)

  cli::cli_h3("{meta_$Class} | {meta_$CompanyName} | {format(meta_$DocLen, big.mark = ',')} chars")
  cat("  opening: ",
      stringi::stri_replace_all_regex(
        stringi::stri_sub(txt_, from = 1L, to = .open_chars), "\\s+", " "
      ), "\n\n", sep = "")

  if (nrow(rows_) == 0L) {
    cat("  no annotations\n\n")
    return(invisible(NULL))
  }

  cat(sprintf("  %3s %7s %7s %-6s %-12s %-3s %s\n",
              "#", "Start", "Stop", "Form", "Desc", "Anc", "Span | Name"))
  purrr::walk(seq_len(nrow(rows_)), function(.i) {
    r_ <- rows_[.i, ]
    sn_ <- norm_(r_$SpanText)
    anc_ <- if (nzchar(key_) && (stringi::stri_detect_fixed(sn_, key_) ||
                                 stringi::stri_detect_fixed(key_, sn_))) "[A]" else "   "
    cat(sprintf("  %3d %7d %7d %-6s %-12s %-3s %s\n",
                .i, r_$Start, r_$Stop,
                dplyr::coalesce(r_$TypeAbbr, "-"),
                dplyr::coalesce(r_$Description, "-"),
                anc_,
                paste0(r_$SpanText, "  |  ", dplyr::coalesce(r_$Name, "-"))))
  })
  cat("\n")
})

cli::cli_alert_info(
  "[A] marks a span whose normalised form contains, or is contained in, the EDGAR company name. \\
   Read the Form column against the Span: a form that no US filing plausibly carries is an address \\
   fragment, and NV and SC are state postal codes before they are Dutch and Spanish legal forms."
)


# 8. Every annotation in context ----
# The span alone cannot say whether it is a party, a signatory, a defined term or an address line.
# Five documents is few enough to print the surroundings of every one.

cli::cli_h2("5. The same annotations, in context")

purrr::walk(tab_pick$DocID, function(.d) {
  rows_ <- dplyr::filter(tab_chk, .data$DocID == .d)
  if (nrow(rows_) == 0L) return(invisible(NULL))
  cli::cli_h3("{dplyr::filter(tab_pick, .data$DocID == .d)$CompanyName}")
  purrr::walk(seq_len(nrow(rows_)), function(.i) {
    r_ <- rows_[.i, ]
    snip_ <- stringi::stri_replace_all_regex(
      paste0(
        "...",
        stringi::stri_sub(r_$TextRaw, from = pmax(1L, r_$Start + 1L - .ctx), to = r_$Start),
        " >>>", stringi::stri_sub(r_$TextRaw, from = r_$Start + 1L, to = r_$Stop), "<<< ",
        stringi::stri_sub(r_$TextRaw, from = r_$Stop + 1L, to = r_$Stop + .ctx),
        "..."
      ),
      "\\s+", " "
    )
    cat(sprintf("  %3d  [%s]  %s\n", .i, dplyr::coalesce(r_$TypeAbbr, "-"), snip_))
  })
  cat("\n")
})


# 9. What the legal form would let us filter ----
# Two rules, both expressible over the proposed column alone. The second is the one that needs the
# read above: NV and SC are state postal codes in an address line, but N.V. and S.p.A. written with
# dots are genuine foreign legal forms, and a label blacklist cannot tell them apart.

.foreign <- c("NV", "SC", "SA", "Spa", "SL", "YK", "GK", "SARL", "SAS", "Srl", "AG", "GmbH", "BV")

cli::cli_h2("6. The filter, demonstrated")
tab_chk |>
  dplyr::filter(.data$TypeAbbr %in% .foreign) |>
  dplyr::transmute(
    Span      = .data$SpanText,
    Form      = .data$TypeAbbr,
    Label     = .data$TypeLabel,
    HasDots   = stringi::stri_detect_regex(.data$SpanText, "\\.\\s*[A-Za-z]\\.?\\s*$"),
    Verdict   = dplyr::if_else(.data$HasDots, "keep: punctuated, a real foreign form",
                               "drop: bare, a US state postal code")
  ) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "Empty here is a fine result on five documents; the earlier probe found the pattern concentrated \\
   in 16 documents of 328. What matters is that the rule is expressible over one column."
)

cli::cli_alert_success("Artifacts under {.path {(.dir_probe)}}")
