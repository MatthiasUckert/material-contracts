# LexNLP company fields: what is in the annotation that the store does not keep? ----
#
# THE QUESTION
# extract_lexnlp.py keeps ann.coords and text[start:stop]. The annotation carries six further
# fields, and the R side is currently hand-rolling two of them: a leading-connective strip and a
# corporate-suffix list. LexNLP already computed both, against a 143-entry legal-form vocabulary
# rather than the nineteen suffixes in 04B.
#
# WHAT IS BEING DECIDED
# Whether to change the store schema and re-extract. That is cheap on the sample and expensive on
# the corpus -- 04C projects roughly 51 hours, most of it this extractor -- so it is decided BEFORE
# the corpus pass launches, not after.
#
# THREE THINGS TO FIND OUT
#   1. Is Name cleaner than SpanText, and by how much? Not assumed, measured.
#   2. Does matching the EDGAR anchor against Name beat matching against the span?
#   3. How much do the two silent guards in CompanyDetector cost? It returns immediately when the
#      whole document is uppercase, and skips any sentence that is uppercase, because an all-caps
#      string defeats the proper-noun grammar. Neither leaves a marker.
#
# PREDICTIONS, STATED BEFORE LOOKING
#   Q1  Name differs from SpanText in more than half of annotations; the difference is almost always
#       a leading fragment rather than a trailing one.
#   Q2  Anchor exact-match share rises by at least 10 points when matching on Name.
#   Q3  All-uppercase HEADS reach at least 5% of documents; all-uppercase whole documents under 1%.
#   Q4  Name does NOT fix the state-of-incorporation case: "Ohio corporation" gives Name = "Ohio".
#
# Reads only, except for one sample parquet and the probe output, both under 2_output/_Probe.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_04a    <- here::here("2_output", "04A-EntityExtract")
.path_text  <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys  <- fs::path(.dir_04a, "sample_anchors.parquet")
.path_store <- fs::path(.dir_04a, "Store", "EntityCandidates.duckdb")

.dir_probe  <- fs::dir_create(here::here("2_output", "_Probe", "lexnlp-fields"))
.dir_in     <- fs::dir_create(fs::path(.dir_probe, "in"))
.dir_out    <- fs::dir_create(fs::path(.dir_probe, "out"))
.dir_script <- here::here("contracts-lexnlp")

.n_docs     <- 300L   # random draw; LexNLP company NER runs at roughly 5 doc/s per worker
.n_noise    <- 40L    # documents added deliberately because their current spans carry known damage
.n_read     <- 25L    # side-by-side rows printed
.n_process  <- 20L
.timeout    <- 240L
.image      <- "contracts-lexnlp"
.rerun      <- FALSE  # TRUE re-runs the container even when the output parquet is present
.seed       <- 42L

stopifnot(fs::file_exists(c(.path_text, .path_keys, .path_store)))
stopifnot(fs::file_exists(fs::path(.dir_script, "probe_company_fields.py")))


# 2. The draw ----
# Two parts, and the second is what makes this a test rather than a survey. A random draw shows
# what the fields look like in general; a draw of documents whose CURRENT spans carry the damage
# seen in 04B shows whether the fields fix it. Reporting only the first would answer a question
# nobody asked.

.noise <- paste0(
  "(?i)^(whereas|amendment|value received|for value received|dex[0-9]|ex-[0-9])",
  "|\\.htm|(?i)\\b(alabama|alaska|arizona|arkansas|california|colorado|connecticut|delaware|",
  "florida|georgia|hawaii|idaho|illinois|indiana|iowa|kansas|kentucky|louisiana|maine|maryland|",
  "massachusetts|michigan|minnesota|mississippi|missouri|montana|nebraska|nevada|ohio|oklahoma|",
  "oregon|pennsylvania|tennessee|texas|utah|vermont|virginia|washington|wisconsin|wyoming)\\s+",
  "(corporation|company|corp)\\b"
)

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, paste0("ATTACH '", fs::path_abs(.path_store), "' AS s (READ_ONLY)"))

tab_noise <- DBI::dbGetQuery(con, paste0(
  "SELECT DISTINCT DocID FROM s.candidates ",
  "WHERE Engine = 'lexnlp' AND Label = 'ORG' AND Span IS NOT NULL ",
  "  AND regexp_matches(Span, '", gsub("'", "''", .noise), "') LIMIT 5000"
)) |>
  tibble::as_tibble()

DBI::dbDisconnect(con, shutdown = TRUE)

tab_text <- arrow::read_parquet(.path_text)

docs_noise_ <- withr::with_seed(
  .seed, sample(tab_noise$DocID, size = min(.n_noise, nrow(tab_noise)))
)
docs_rand_ <- withr::with_seed(
  .seed + 1L,
  sample(setdiff(tab_text$DocID, docs_noise_), size = .n_docs)
)
docs_ <- unique(c(docs_rand_, docs_noise_))

tab_text |>
  dplyr::filter(.data$DocID %in% docs_) |>
  arrow::write_parquet(fs::path(.dir_in, "sample.parquet"))

cli::cli_alert_info(
  "{length(docs_)} document{?s} written: {length(docs_rand_)} random, \\
   {length(docs_noise_)} carrying known span damage."
)


# 3. Run the probe in the container ----
# The image entrypoint is the production extractor, so it is overridden. Nothing here can write to
# the store: only the two probe directories are mounted writable.

.path_ann  <- fs::path(.dir_out, "company_fields.parquet")
.path_docs <- fs::path(.dir_out, "company_docs.parquet")

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
    "--chunk-size", 8L,
    "--timeout", as.integer(.timeout)
  )
  t0_ <- Sys.time()
  status_ <- system2("docker", args_, stdout = "", stderr = "")
  if (!identical(as.integer(status_), 0L)) cli::cli_abort("Probe container failed ({status_}).")
  cli::cli_alert_success(
    "Probe done in {round(as.numeric(difftime(Sys.time(), t0_, units = 'secs')), 1)}s."
  )
} else {
  cli::cli_alert_info("Reusing {.path {(.path_ann)}}; set .rerun to TRUE to re-measure.")
}

tab_ann  <- arrow::read_parquet(.path_ann) |> tibble::as_tibble()
tab_docs <- arrow::read_parquet(.path_docs) |> tibble::as_tibble()


# 4. Block A: is the field there at all ----
# Before anything is compared, establish which fields are actually populated. A field present in a
# tenth of annotations is a curiosity; one present in nearly all of them is a column.

cli::cli_h2("A. Field availability")
tibble::tibble(
  Field = c("Name", "NameAbbr", "TypeFull", "TypeAbbr", "TypeLabel", "Description"),
  NonNA = c(
    sum(!is.na(tab_ann$Name)), sum(!is.na(tab_ann$NameAbbr)),
    sum(!is.na(tab_ann$TypeFull)), sum(!is.na(tab_ann$TypeAbbr)),
    sum(!is.na(tab_ann$TypeLabel)), sum(!is.na(tab_ann$Description))
  )
) |>
  dplyr::mutate(Pct = round(100 * .data$NonNA / nrow(tab_ann), 1)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "{nrow(tab_ann)} annotation{?s} over {dplyr::n_distinct(tab_ann$DocID)} document{?s}. TypeFull \\
   near 100% is expected and is the whole reason this engine behaves like a party extractor: the \\
   pattern cannot fire without a legal form or a description."
)


# 5. Block B: Name against SpanText ----
# The comparison the decision rests on. Difference is decomposed into a leading part and a trailing
# part, because they have different causes: the leading part is the article group inside the coords,
# the trailing part is the legal form.

tab_cmp <- tab_ann |>
  dplyr::filter(!is.na(.data$Name)) |>
  dplyr::mutate(
    SpanFlat = stringi::stri_trim_both(
      stringi::stri_replace_all_regex(.data$SpanText, "\\s+", " ")
    ),
    NameFlat = stringi::stri_trim_both(
      stringi::stri_replace_all_regex(.data$Name, "\\s+", " ")
    ),
    Same     = .data$SpanFlat == .data$NameFlat,
    InSpan   = stringi::stri_detect_fixed(.data$SpanFlat, .data$NameFlat),
    LeadCut  = dplyr::if_else(
      .data$InSpan,
      stringi::stri_sub(.data$SpanFlat, from = 1L,
                        to = stringi::stri_locate_first_fixed(.data$SpanFlat, .data$NameFlat)[, 1] - 1L),
      NA_character_
    ),
    TailCut  = dplyr::if_else(
      .data$InSpan,
      stringi::stri_sub(.data$SpanFlat,
                        from = stringi::stri_locate_first_fixed(.data$SpanFlat, .data$NameFlat)[, 2] + 1L),
      NA_character_
    )
  )

cli::cli_h2("B. Does Name differ from the stored span?")
tibble::tibble(
  Item = c("Annotations with a Name", "Name identical to the span",
           "Name is a substring of the span", "Name NOT inside the span",
           "Something cut from the front", "Something cut from the back"),
  N    = c(nrow(tab_cmp), sum(tab_cmp$Same), sum(tab_cmp$InSpan), sum(!tab_cmp$InSpan),
           sum(nzchar(dplyr::coalesce(tab_cmp$LeadCut, ""))),
           sum(nzchar(dplyr::coalesce(tab_cmp$TailCut, ""))))
) |>
  dplyr::mutate(Pct = round(100 * .data$N / nrow(tab_cmp), 1)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_warning(
  "'Name NOT inside the span' is the row that would block adoption. It would mean Name is a \\
   normalised form rather than a slice of the text, and the offsets could not be narrowed to it."
)

cli::cli_h2("B. What gets cut from the FRONT, most common first")
tab_cmp |>
  dplyr::filter(nzchar(dplyr::coalesce(.data$LeadCut, ""))) |>
  dplyr::count(.data$LeadCut, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = 30, width = Inf)

cli::cli_h2("B. What gets cut from the BACK, most common first")
tab_cmp |>
  dplyr::filter(nzchar(dplyr::coalesce(.data$TailCut, ""))) |>
  dplyr::count(.data$TailCut, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = 30, width = Inf)


# 6. Block C: the legal-form vocabulary ----
# LexNLP ships 143 aliases mapping to a normalised abbreviation and a legal category. 04B strips
# nineteen suffixes by hand. This is what the difference looks like in practice, and TypeLabel is a
# publishable descriptive variable in its own right.

cli::cli_h2("C. Legal form, as normalised by LexNLP")
tab_ann |>
  dplyr::count(.data$TypeAbbr, .data$TypeLabel, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  dplyr::mutate(Pct = round(100 * .data$N / nrow(tab_ann), 1)) |>
  print(n = 25, width = Inf)

cli::cli_h2("C. Description, where the pattern fired on one instead of a type")
tab_ann |>
  dplyr::count(.data$Description, name = "N") |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = 15, width = Inf)


# 7. Block D: does Name collapse variants better than the 04B suffix strip? ----
# The dedup question, asked three ways on the same documents: distinct raw spans, distinct 04B keys,
# distinct LexNLP names. Fewer is better only if the collapse is correct, which block G checks.

norm_04b_ <- function(.x) {
  sfx_ <- c("INC", "INCORPORATED", "CORP", "CORPORATION", "LLC", "LLP", "LP", "LTD", "LIMITED",
            "PLC", "NV", "BV", "SA", "AG", "GMBH", "CO", "COMPANY", "TRUST", "NA")
  lead_ <- "^(MADE BY AND BETWEEN|BY AND BETWEEN|BY AND AMONG|AMONGST|BETWEEN|AMONG|AND|WITH|THIS|THE|DATED|AS OF) "
  out_ <- .x |>
    stringi::stri_trans_toupper() |>
    stringi::stri_replace_all_regex("[^A-Z0-9 ]", " ") |>
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_trim_both() |>
    stringi::stri_replace_first_regex(lead_, "")
  purrr::map_chr(out_, function(.s) {
    if (is.na(.s)) return(NA_character_)
    toks_ <- strsplit(.s, " ", fixed = TRUE)[[1]]
    while (length(toks_) > 1L && toks_[length(toks_)] %in% sfx_) toks_ <- toks_[-length(toks_)]
    paste(toks_, collapse = " ")
  })
}

tab_keyed <- tab_ann |>
  dplyr::mutate(
    KeySpan = norm_04b_(.x = .data$SpanText),
    KeyName = norm_04b_(.x = dplyr::coalesce(.data$Name, .data$SpanText))
  )

cli::cli_h2("D. Distinct organisations per document, three ways")
tab_keyed |>
  dplyr::summarise(
    NRaw     = dplyr::n(),
    NDistSpan = dplyr::n_distinct(stringi::stri_trans_toupper(.data$SpanText)),
    NDistKey  = dplyr::n_distinct(.data$KeySpan),
    NDistName = dplyr::n_distinct(.data$KeyName),
    .by = DocID
  ) |>
  dplyr::summarise(
    Docs       = dplyr::n(),
    MedRaw     = stats::median(.data$NRaw),
    MedDistSpan = stats::median(.data$NDistSpan),
    MedDistKey  = stats::median(.data$NDistKey),
    MedDistName = stats::median(.data$NDistName)
  ) |>
  print(width = Inf)

cli::cli_alert_info(
  "MedDistKey is what 04B reports today. MedDistName is the same normalisation applied to LexNLP's \\
   own name field, so the gap between them is what the hand-rolled trim is failing to collapse."
)


# 8. Block E: the anchor match, on the span and on the name ----
# The decisive block. If matching on Name moves annotations from forward to exact and finds the
# filer in documents the span form missed, the change pays for the re-extraction.

tab_anchor <- arrow::read_parquet(.path_keys) |>
  dplyr::transmute(
    DocID,
    Class = .data$ClassDetailed,
    CompanyName,
    AnchorKey = norm_04b_(.x = .data$CompanyName)
  ) |>
  dplyr::mutate(AnchorKey = dplyr::if_else(nchar(.data$AnchorKey) < 5L, NA_character_,
                                           .data$AnchorKey)) |>
  dplyr::filter(.data$DocID %in% tab_ann$DocID)

kind_ <- function(.key, .anchor) {
  dplyr::case_when(
    is.na(.key) | is.na(.anchor) | !nzchar(.key) ~ "none",
    .key == .anchor                              ~ "exact",
    stringi::stri_detect_fixed(.key, .anchor)    ~ "forward",
    stringi::stri_detect_fixed(.anchor, .key)    ~ "reverse",
    TRUE                                         ~ "none"
  )
}

tab_match <- tab_keyed |>
  dplyr::inner_join(tab_anchor, by = dplyr::join_by(DocID)) |>
  dplyr::filter(!is.na(.data$AnchorKey)) |>
  dplyr::mutate(
    KindSpan = kind_(.key = .data$KeySpan, .anchor = .data$AnchorKey),
    KindName = kind_(.key = .data$KeyName, .anchor = .data$AnchorKey)
  )

cli::cli_h2("E. Match kind against the EDGAR anchor, per annotation")
tab_match |>
  dplyr::count(.data$KindSpan, .data$KindName, name = "N") |>
  tidyr::pivot_wider(names_from = KindName, values_from = N, values_fill = 0L,
                     names_prefix = "name_") |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "Read the off-diagonal. Rows moving from name_forward to name_exact are the leading-article \\
   fragment being removed; anything moving INTO name_none is a regression and has to be read."
)

cli::cli_h2("E. Documents in which the filer is matched at all")
tab_match |>
  dplyr::summarise(
    AnySpan = any(.data$KindSpan != "none"),
    AnyName = any(.data$KindName != "none"),
    .by = DocID
  ) |>
  dplyr::summarise(
    Docs      = dplyr::n(),
    BySpan    = sum(.data$AnySpan),
    ByName    = sum(.data$AnyName),
    NameOnly  = sum(.data$AnyName & !.data$AnySpan),
    SpanOnly  = sum(.data$AnySpan & !.data$AnyName)
  ) |>
  print(width = Inf)


# 9. Block F: the two silent guards ----
# CompanyDetector returns without a single annotation when the whole document is uppercase, and
# skips any sentence that is uppercase, because an all-caps string defeats the proper-noun grammar
# the noun-phrase step depends on. Neither leaves a marker in the output, so a document silenced
# this way is indistinguishable from one that names no companies.

cli::cli_h2("F. The uppercase guards")
tab_docs |>
  dplyr::summarise(
    Docs          = dplyr::n(),
    UpperDoc      = sum(.data$UpperDoc),
    UpperHead     = sum(.data$UpperHead),
    ZeroAnn       = sum(.data$NAnn == 0L),
    ZeroAndUpper  = sum(.data$NAnn == 0L & .data$UpperHead)
  ) |>
  dplyr::mutate(
    PctUpperHead = round(100 * .data$UpperHead / .data$Docs, 1),
    PctZero      = round(100 * .data$ZeroAnn / .data$Docs, 1)
  ) |>
  print(width = Inf)

cli::cli_h2("F. Annotations per document, by whether the head is all uppercase")
tab_docs |>
  dplyr::summarise(
    Docs   = dplyr::n(),
    MedAnn = stats::median(.data$NAnn),
    PctZero = round(100 * mean(.data$NAnn == 0L), 1),
    .by = UpperHead
  ) |>
  print(width = Inf)

cli::cli_alert_warning(
  "If PctZero is far higher where the head is uppercase, a share of 04B's 'no match' documents are \\
   not an extraction gap at all -- they are a casing guard, and lowercasing the input would recover \\
   them at no cost beyond a second pass."
)


# 10. Block G: read them side by side ----
# Every table above is consistent with Name being cleaner and with Name being differently wrong.
# Only the strings say which.

cli::cli_h2("G. Span against Name, where they differ")
tab_cmp |>
  dplyr::filter(!.data$Same) |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n_read, nrow(.d)))))() |>
  dplyr::select(SpanFlat, NameFlat, TypeAbbr, TypeLabel, Description) |>
  print(n = Inf, width = Inf)

cli::cli_h2("G. The documents drawn for known span damage")
tab_cmp |>
  dplyr::filter(.data$DocID %in% docs_noise_, !.data$Same) |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n_read, nrow(.d)))))() |>
  dplyr::select(SpanFlat, NameFlat, TypeAbbr, Description) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "Q4 predicted Name will NOT fix the state-of-incorporation case: 'Ohio corporation' should give \\
   Name = 'Ohio' with TypeAbbr = 'Corp'. If that holds, the state guard is a separate rule and this \\
   probe does not remove the need for it."
)

cli::cli_alert_success("Probe artifacts under {.path {(.dir_probe)}}")
