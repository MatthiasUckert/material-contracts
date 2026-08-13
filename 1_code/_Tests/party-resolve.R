# Stage 1b: the contracting party, one row per document ----
#
# WHAT THIS IS
# party-geometry.R measured WHERE the filer is named. This turns that into the answer: for each
# document, which span is the contracting party, where does it sit, and how far can it be trusted.
#
# THE IDENTITY IS NOT IN DISPUTE. EDGAR records the filer, so who the contracting party IS comes
# free. What this resolves is WHERE it is named in the contract text, which is the coordinate
# everything downstream hangs off -- counterparties, party addresses, the signing date.
#
# HOW A CANDIDATE IS CHOSEN
# Best match kind first, then earliest position. Exact beats forward beats a long reverse match, and
# a bare reverse match is refused outright: the unguarded reverse arm matched the single character
# "G" against GEORGIA PACIFIC seventy-three times, and a span like that would put the origin
# anywhere in the document.
#
# Run after party-geometry.R, which writes the probe table this reads.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_04a     <- here::here("2_output", "04A-EntityExtract")
.path_text   <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys   <- fs::path(.dir_04a, "sample_anchors.parquet")
.dir_out     <- fs::dir_create(here::here("2_output", "_Probe"))
.path_probe  <- fs::path(.dir_out, "anchor_org.parquet")

.cap_located <- 3000L   # a match beyond this is reported but not treated as the origin
.min_reverse <- 10L     # a reverse match shorter than this is refused
.ctx         <- 160L    # snippet either side
.n_examples  <- 30L
.seed        <- 42L

stopifnot(fs::file_exists(c(.path_text, .path_keys, .path_probe)))


# 2. Choose one span per document ----
# Quality first, then earliest. Ties on Start are broken by engine name so the choice is
# reproducible rather than dependent on row order in the store.

tab_probe <- arrow::read_parquet(.path_probe) |>
  dplyr::mutate(
    SpanKeyLen = stringi::stri_length(.data$SpanKey),
    Quality    = dplyr::case_when(
      .data$MatchKind == "exact"   ~ 1L,
      .data$MatchKind == "forward" ~ 2L,
      TRUE                         ~ 3L
    ),
    Usable = .data$Quality <= 2L | .data$SpanKeyLen >= .min_reverse
  )

tab_best <- tab_probe |>
  dplyr::filter(.data$Usable) |>
  dplyr::arrange(.data$DocID, .data$Quality, .data$Start, .data$Combo) |>
  dplyr::slice_head(n = 1L, by = DocID) |>
  dplyr::select(DocID, Combo, Start, Stop, Span, MatchKind, SpanKeyLen, Pos, NOcc)


# 3. Trim the span down to a name ----
# The engines leak into neighbouring text: "between Precision BioSciences, Inc.", "amongAMERICAN
# CASINO & ENTERTAINMENT PROPERTIES LLC", "the Common Stock of eHealth, Inc." The location is right
# in all of these and the boundary is not, so the leading connective is stripped rather than the
# span being rejected. Trailing junk is left alone for now -- it needs the counterparty rule to know
# where the name stops.

.lead <- paste0(
  "^\\s*(and|among|amongst|between|by and between|by and among|with|the|this|",
  "made by and between|dated|as of)\\b[\\s,]*"
)

tab_best <- tab_best |>
  dplyr::mutate(
    SpanTrim = .data$Span |>
      stringi::stri_replace_all_regex("\\s+", " ") |>
      stringi::stri_replace_first_regex(.lead, "", opts_regex = stringi::stri_opts_regex(
        case_insensitive = TRUE
      )) |>
      stringi::stri_trim_both(),
    Leaked = .data$SpanTrim != stringi::stri_trim_both(
      stringi::stri_replace_all_regex(.data$Span, "\\s+", " ")
    )
  )


# 4. One row per keyed document, including the ones nothing was found for ----
# Documents with no match at all are absent from the probe table, so the join has to run from the
# key side. Reporting them as a status rather than dropping them is the difference between a
# coverage figure and a flattering one.

tab_party <- arrow::read_parquet(.path_keys) |>
  dplyr::select(DocID, Fold, ClassDetailed, AmendType, CIK, CompanyName, DateFiled) |>
  dplyr::left_join(
    arrow::read_parquet(.path_probe) |>
      dplyr::distinct(.data$DocID, .data$AnchorKey, .data$DocLen),
    by = dplyr::join_by(DocID)
  ) |>
  dplyr::left_join(tab_best, by = dplyr::join_by(DocID)) |>
  dplyr::mutate(
    Status = dplyr::case_when(
      is.na(.data$AnchorKey) & is.na(.data$Start) ~ "no company key",
      !is.na(.data$Start) & .data$Start < .cap_located ~ "located",
      !is.na(.data$Start) ~ "late",
      .data$DocID %in% tab_probe$DocID ~ "fragment matches only",
      TRUE ~ "not found in text"
    )
  ) |>
  dplyr::relocate(Status, .after = DocID)

cli::cli_h2("The contracting party, per document")
tab_party |>
  dplyr::count(.data$Status, name = "N") |>
  dplyr::mutate(Pct = round(100 * .data$N / nrow(tab_party), 1)) |>
  dplyr::arrange(dplyr::desc(.data$N)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "'located' is the working set: a trustworthy match inside the first {(.cap_located)} characters. \\
   Everything else is reported rather than dropped, because a coverage figure computed on the \\
   documents that worked is not a coverage figure."
)


# 5. What the located set looks like ----

cli::cli_h2("Which engine supplied the located span")
tab_party |>
  dplyr::filter(.data$Status == "located") |>
  dplyr::count(.data$Combo, .data$MatchKind, name = "N") |>
  tidyr::pivot_wider(names_from = MatchKind, values_from = N, values_fill = 0L) |>
  print(n = Inf, width = Inf)

cli::cli_alert_warning(
  "Read this as attribution, not as a ranking. Several engines often propose the same span at the \\
   same offset, and the tie goes to whichever sorts first."
)

cli::cli_h2("Located coverage by contract type")
tab_party |>
  dplyr::summarise(
    Docs      = dplyr::n(),
    Located   = sum(.data$Status == "located"),
    MedStart  = stats::median(.data$Start[.data$Status == "located"], na.rm = TRUE),
    PctLeaked = round(100 * mean(.data$Leaked[.data$Status == "located"], na.rm = TRUE), 1),
    .by = ClassDetailed
  ) |>
  dplyr::mutate(PctLocated = round(100 * .data$Located / .data$Docs, 1)) |>
  dplyr::arrange(dplyr::desc(.data$Docs)) |>
  print(n = Inf, width = Inf)


# 6. Read thirty of them ----
# EDGAR's name beside the span found for it, with the surrounding text. This is the only block that
# can tell a correct location from a plausible one.

tab_look <- tab_party |>
  dplyr::filter(.data$Status == "located") |>
  (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n_examples, nrow(.d)))))()

tab_look <- tab_look |>
  dplyr::left_join(
    arrow::read_parquet(.path_text) |> dplyr::filter(.data$DocID %in% tab_look$DocID),
    by = dplyr::join_by(DocID)
  ) |>
  dplyr::mutate(
    Snippet = stringi::stri_replace_all_regex(
      paste0(
        "...",
        stringi::stri_sub(.data$TextRaw, from = pmax(1L, .data$Start + 1L - .ctx), to = .data$Start),
        " >>>", stringi::stri_sub(.data$TextRaw, from = .data$Start + 1L, to = .data$Stop), "<<< ",
        stringi::stri_sub(.data$TextRaw, from = .data$Stop + 1L, to = .data$Stop + .ctx),
        "..."
      ),
      "\\s+", " "
    )
  )

cli::cli_h2("EDGAR's company against the span found for it")
purrr::pwalk(
  dplyr::select(tab_look, CompanyName, SpanTrim, MatchKind, Start, Snippet),
  function(CompanyName, SpanTrim, MatchKind, Start, Snippet) {
    cli::cli_h3("{CompanyName}")
    cat("  found : ", SpanTrim, "  [", MatchKind, " @ ", Start, "]\n", sep = "")
    cat("  ", Snippet, "\n\n", sep = "")
  }
)


# 7. Write it ----

arrow::write_parquet(tab_party, fs::path(.dir_out, "contracting_party.parquet"))
cli::cli_alert_success("Written to 2_output/_Probe/contracting_party.parquet")
