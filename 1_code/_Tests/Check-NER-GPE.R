# Check-NER-GPE: one parquet per engine, one contract, every test in one place ----
#
# WHAT THIS IS
# The acceptance check for GPE extraction, built to the contract Check-NER-ORG settled. Twenty-five
# documents, every engine:model that emits GPE, one parquet each on disk, and a battery of tests
# over them.
#
# THE CONTRACT, UNCHANGED
#   Mandatory core   DocID  Start  Stop  Span  Label  LabelRaw  Engine  Model
#   Optional extras  whatever the engine has and nothing it does not
#
# GPE DIFFERS FROM ORG IN THREE WAYS AND EACH ONE CHANGES A TEST
#
# 1. LabelRaw IS INFORMATIVE FOR spaCy HERE. LABEL_MAP folds spaCy's GPE and LOC into one unified
#    GPE, so LabelRaw is the only place the distinction survives -- and it is a real distinction:
#    GPE is a geopolitical body, LOC a physical feature. For ORG, LabelRaw was constant and carried
#    nothing. This pass reports the split rather than assuming it is noise.
#
# 2. THE EXTRAS DIVIDE INTO DERIVABLE AND NOT, AND ONLY THE SECOND KIND JUSTIFIES A PROBE.
#    - lexnlp resolves an alias to a canonical entity with ISO codes and a category. That mapping
#      lives in the container and NOTHING downstream can reconstruct it from the span. Probe.
#    - the gazetteer already writes its GeoClass into LabelRaw, and its remaining fields -- IsWord,
#      the canonical name, the gate that admitted the span -- are recoverable by joining the
#      uppercased span back to geo_lookup.parquet, which this script demonstrates rather than
#      re-extracting. No probe. The one field that is NOT derivable is the distance to the anchor
#      that gated the match, and whether that is worth emitting is the open question below.
#    - spaCy has nothing beyond the core, as with ORG.
#
# 3. GPE HAS TWO ROLES AND THE POSITION STATISTIC AVERAGES THEM. A place in the preamble is a
#    party's jurisdiction; a place in the final fifth is governing law. 04A left the engine choice
#    open for exactly this reason, so nothing here crowns one -- it reports what each engine gives.
#
# The parquets are left on disk under 2_output/_Probe/Check-NER-GPE/out and are meant to be opened.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration ----

.dir_04a     <- here::here("2_output", "04A-EntityExtract")
.path_text   <- fs::path(.dir_04a, "sample_text.parquet")
.path_keys   <- fs::path(.dir_04a, "sample_anchors.parquet")
.path_store  <- fs::path(.dir_04a, "Store", "EntityCandidates.duckdb")

.dir_check   <- fs::dir_create(here::here("2_output", "_Probe", "Check-NER-GPE"))
.dir_in      <- fs::dir_create(fs::path(.dir_check, "in"))
.dir_out     <- fs::dir_create(fs::path(.dir_check, "out"))
.dir_lexnlp  <- here::here("contracts-lexnlp")
.dir_engine  <- here::here("contracts-engine")
.path_lookup <- fs::path(.dir_engine, "data", "gazetteer", "geo_lookup.parquet")

.n_docs      <- 25L
.len_min     <- 4000L
.len_max     <- 40000L
.timeout     <- 240L
.max_chars   <- NULL   # must match 04A (.max_chars_extract is NULL there) or block 6 reports drift
.image       <- "contracts-lexnlp"
.rerun       <- TRUE
.seed        <- 42L    # the SAME seed as Check-NER-ORG, so the documents are the same twenty-five
.n_read      <- 3L

# The gazetteer's context gates, as extract_gazetteer.py sets them. Carried here only so the
# derived-gate demonstration in block 9 reproduces the extractor's own reasoning.
.win_state   <- 200L
.win_word    <- 40L

tab_combo <- tibble::tribble(
  ~Engine,  ~Model,            ~Device, ~NProc, ~Batch,
  "lexnlp", "lexnlp",          NA,      20L,    8L,
  "paper",  "gazetteer-v1",    NA,      20L,    NA,
  "spacy",  "en_core_web_sm",  "cpu",   10L,    64L,
  "spacy",  "en_core_web_md",  "cpu",   10L,    64L,
  "spacy",  "en_core_web_lg",  "cpu",   10L,    32L,
  "spacy",  "en_core_web_trf", "auto",  1L,     8L
) |>
  dplyr::mutate(
    Combo = dplyr::if_else(.data$Engine == .data$Model, .data$Engine,
                           paste0(.data$Engine, ":", .data$Model)),
    File  = paste0("GPE__", .data$Engine, "__", .data$Model, ".parquet"),
    Path  = as.character(fs::path(.dir_out, .data$File))
  )

.core <- c("DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model")

stopifnot(fs::file_exists(c(.path_text, .path_keys, .path_store, .path_lookup)))
stopifnot(fs::file_exists(fs::path(.dir_lexnlp, "probe_lexnlp_gpe.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, "extract_spacy.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, "extract_gazetteer.py")))
stopifnot(fs::file_exists(fs::path(.dir_engine, ".venv", "bin", "python")))


# 2. The sample ----
# Identical draw to Check-NER-ORG: same seed, same bounds, same stratification. The two passes are
# therefore directly comparable document for document, which is what makes "the gazetteer found a
# state here and lexnlp did not" a statement about the engines rather than about the draw.

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
  "{nrow(tab_pick)} document{?s}, {dplyr::n_distinct(tab_pick$Class)} contract type{?s}, \\
   {format(min(tab_pick$DocLen), big.mark = ',')} to \\
   {format(max(tab_pick$DocLen), big.mark = ',')} characters -- the same draw Check-NER-ORG used."
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
    "/probe/probe_lexnlp_gpe.py", "/work/sample.parquet",
    "--output", paste0("/out/", fs::path_file(.path)),
    "--n-process", 20L, "--chunk-size", 8L, "--timeout", as.integer(.timeout)
  )
  system2("docker", args_, stdout = "", stderr = "")
}

run_gazetteer_ <- function(.path, .nproc) {
  args_ <- c(
    fs::path(.dir_engine, "extract_gazetteer.py"),
    as.character(fs::path_abs(fs::path(.dir_in, "sample.parquet"))),
    "--output", as.character(.path),
    "--lookup", as.character(fs::path_abs(.path_lookup)),
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
    "--label", "GPE"     # LABEL_MAP folds spaCy's GPE and LOC into this one unified label
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
  } else if (Engine == "paper") {
    run_gazetteer_(.path = Path, .nproc = NProc)
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
# The same nine, with one change: T6 asserts Label is GPE. LabelRaw is deliberately NOT constrained,
# because for this label it legitimately varies -- geoentity, four GeoClass values, and spaCy's
# GPE-or-LOC.

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
              "T5 Engine and Model constant and correct", "T6 Label is GPE on candidates",
              "T7 no duplicate DocID/Start/Stop/LabelRaw", "T8 sentinels fully null",
              "T9 extras absent on sentinel rows"),
    N     = c(
      sum(.core %in% names(.tab)),
      dplyr::n_distinct(.tab$DocID),
      sum(rt_$OK),
      sum(rt_$InDoc),
      sum(.tab$Engine == .engine & .tab$Model == .model),
      sum(cand_$Label == "GPE"),
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
  "WHERE Label = 'GPE' AND Start IS NOT NULL AND DocID IN ('",
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
  "A nonzero lexnlp row here would mean the probe's locator differs from the production one -- the \\
   alias columns, the minimum alias length or the conflict resolver -- and that is a difference in \\
   WHAT IS FOUND, not in what is recorded."
)


# 7. Yield, and the LabelRaw split ----
# LabelRaw is the interesting column for this label. It carries the gazetteer's resolved GeoClass,
# lexnlp's constant tag, and spaCy's GPE-versus-LOC distinction, which the unified label discards.

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

cli::cli_alert_info(
  "spaCy's LOC rows are the ones to look at. A geopolitical body and a physical feature are \\
   different objects for a party-jurisdiction rule, and folding them into one label is a decision \\
   LABEL_MAP makes silently. Whether to keep the split is a mapping question, not an extractor one."
)


# 8. The lexnlp extras ----
# Resolution, not description. The span is an ALIAS; Name is the entity it resolved to. Nothing
# downstream can recover that mapping, which is what distinguishes this from the gazetteer's case.

if ("Name" %in% names(lst_raw[["lexnlp"]])) {
  cand_lex_ <- dplyr::filter(lst_raw[["lexnlp"]], !is.na(.data$Start))

  cli::cli_h2("LexNLP extras: availability")
  cand_lex_ |>
    dplyr::summarise(dplyr::across(dplyr::any_of(
      c("Name", "NameEn", "Alias", "EntityCategory", "Iso2", "Iso3",
        "EntityId", "EntityPriority", "Source", "Year")
    ), \(.x) round(100 * mean(!is.na(.x)), 1))) |>
    print(width = Inf)

  cli::cli_h2("LexNLP extras: entity category")
  cand_lex_ |>
    dplyr::count(.data$EntityCategory, name = "N") |>
    dplyr::arrange(dplyr::desc(.data$N)) |>
    print(n = 20, width = Inf)

  cli::cli_h2("LexNLP extras: where the span differs from the resolved name")
  cand_lex_ |>
    dplyr::filter(stringi::stri_trans_toupper(.data$Span) !=
                    stringi::stri_trans_toupper(dplyr::coalesce(.data$Name, ""))) |>
    dplyr::count(Span = .data$Span, Resolved = .data$Name, Iso3 = .data$Iso3, name = "N") |>
    dplyr::arrange(dplyr::desc(.data$N)) |>
    print(n = 25, width = Inf)

  cli::cli_alert_info(
    "These rows are the whole argument for the extras. Where Span and Resolved differ, the engine \\
     performed a normalisation that the span alone does not record, and Iso3 is that normalisation \\
     in a form a regression can group on."
  )
}


# 9. Resolve every engine onto one hierarchy ----
# THE POINT OF THIS BLOCK. Six engines, three vocabularies, one output. A span is matched at
# whatever level its engine works in and is REPORTED at one of three: Country, State, County.
# Populated places are carried and resolved; they are never a reported level, because a city is
# evidence for a county rather than a category of its own.
#
# The resolution is per engine because the engines carry different things:
#   gazetteer  joins the span back to the rebuilt lookup on (GeoKey, GeoClass) and reads the
#              hierarchy straight off it
#   lexnlp     already carries Iso2, Iso3 and EntityCategory, so it needs no lookup at all
#   spacy      resolves nothing and stays NA, which is honest: its spans are corroboration, not
#              identification
#
# AMBIGUITY IS REPORTED, NOT GUESSED. WASHINGTON is a county in 31 states and SPRINGFIELD a place
# in 35, and the extractor cannot say which it matched -- its index collapses the lookup to one
# entry per name. Those rows carry NParent > 1 and a null parent, and the field that would resolve
# them is the anchor the gazetteer gates on and discards.

tab_lookup <- arrow::read_parquet(.path_lookup)

if (!all(c("ParentCounty", "StateName") %in% names(tab_lookup))) {
  cli::cli_abort(c(
    "The lookup is missing {.field {setdiff(c('ParentCounty', 'StateName'), names(tab_lookup))}}.",
    "i" = "Run {.path 1_code/_Scripts/rebuild-geo-lookup.R}; it is re-runnable and rebuilds from
           the preserved original."
  ))
}

#' Map any engine's GPE candidates onto the shared hierarchy
#'
#' @param .tab Candidate rows for one engine, core columns plus whatever extras it carries.
#' @param .engine Character. "paper", "lexnlp" or "spacy".
#' @param .lookup The rebuilt gazetteer lookup.
#' @return .tab with MatchClass, GeoLevel, GeoIso3, GeoIso2, GeoState, GeoCounty, NParent added.
ent_geo_resolve <- function(.tab, .engine, .lookup) {
  if (FALSE) {
    .tab    <- lst_raw[["paper:gazetteer-v1"]]
    .engine <- "paper"
    .lookup <- tab_lookup
  }

  cand_ <- dplyr::filter(.tab, !is.na(.data$Start))

  out_ <- if (.engine == "paper") {
    cand_ |>
      dplyr::mutate(GeoKey = stringi::stri_trans_toupper(.data$Span)) |>
      dplyr::left_join(
        .lookup |>
          dplyr::select(GeoKey, GeoClass, ISO3, Iso2, StateName, ParentCounty, NParent) |>
          dplyr::distinct(.data$GeoKey, .data$GeoClass, .keep_all = TRUE),
        by = dplyr::join_by(GeoKey, LabelRaw == GeoClass)
      ) |>
      dplyr::mutate(
        MatchClass = .data$LabelRaw,
        Amb        = !is.na(.data$NParent) & .data$NParent > 1L,
        GeoIso3    = .data$ISO3,
        GeoIso2    = dplyr::if_else(.data$Amb, NA_character_, .data$Iso2),
        # StateName, NOT ParentState. A state's parent is not itself, so ParentState is NA on
        # every US State row and reading it reported 179 states at country level.
        GeoState   = dplyr::if_else(.data$Amb, NA_character_, .data$StateName),
        GeoCounty  = dplyr::if_else(.data$Amb, NA_character_, .data$ParentCounty)
      )
  } else if (.engine == "lexnlp") {
    # THREE BRANCHES, NOT TWO. The entity table also holds Chinese Provinces and German States,
    # which are subdivisions of a country the US-centric hierarchy has no tier for. Folding them
    # into "Country" would put a subdivision in a country column; they are named and left
    # unresolved instead, which is two rows in twenty-five documents and worth reporting as such.
    cand_ |>
      dplyr::mutate(
        MatchClass = dplyr::case_when(
          .data$EntityCategory == "US States" ~ "US State",
          .data$EntityCategory == "Countries" ~ "Country",
          TRUE                                ~ "Other Subdivision"
        ),
        NParent    = 1L,
        GeoIso3    = dplyr::case_when(
          .data$MatchClass == "US State" ~ "USA",
          .data$MatchClass == "Country"  ~ .data$Iso3,
          TRUE                           ~ NA_character_
        ),
        GeoIso2    = dplyr::if_else(.data$MatchClass == "US State", .data$Iso2, NA_character_),
        GeoState   = dplyr::if_else(.data$MatchClass == "US State",
                                    stringi::stri_trans_toupper(.data$Name), NA_character_),
        GeoCounty  = NA_character_
      )
  } else {
    cand_ |>
      dplyr::mutate(MatchClass = NA_character_, NParent = NA_integer_, GeoIso3 = NA_character_,
                    GeoIso2 = NA_character_, GeoState = NA_character_, GeoCounty = NA_character_)
  }

  out_ |>
    dplyr::mutate(
      # The reported level is the FINEST one that resolved, never the class that was matched.
      GeoLevel = dplyr::case_when(
        !is.na(.data$GeoCounty) ~ "County",
        !is.na(.data$GeoState)  ~ "State",
        !is.na(.data$GeoIso3)   ~ "Country",
        TRUE                    ~ NA_character_
      )
    ) |>
    dplyr::select(dplyr::any_of(c(.core, "MatchClass", "GeoLevel", "GeoIso3", "GeoIso2",
                                  "GeoState", "GeoCounty", "NParent")))
}

tab_geo <- purrr::pmap(
  .l = list(lst_raw, tab_combo$Combo, tab_combo$Engine),
  .f = function(.d, .c, .e) dplyr::mutate(ent_geo_resolve(.tab = .d, .engine = .e,
                                                          .lookup = tab_lookup), Combo = .c)
) |>
  purrr::list_rbind()

cli::cli_h2("Resolution rate, per engine")
tab_geo |>
  dplyr::summarise(
    Spans      = dplyr::n(),
    Resolved   = sum(!is.na(.data$GeoLevel)),
    PctResolved = round(100 * mean(!is.na(.data$GeoLevel)), 1),
    Country    = sum(.data$GeoLevel == "Country", na.rm = TRUE),
    State      = sum(.data$GeoLevel == "State", na.rm = TRUE),
    County     = sum(.data$GeoLevel == "County", na.rm = TRUE),
    Ambiguous  = sum(!is.na(.data$NParent) & .data$NParent > 1L),
    .by = Combo
  ) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "'Ambiguous' is a name the lookup holds under several parents -- WASHINGTON as a county in 31 \\
   states. Those rows keep their Iso3 and lose their state, which is the honest reading: the \\
   country is known and the rest is not, and pretending otherwise would put a wrong state into a \\
   published variable."
)

cli::cli_h2("What the matched class resolves to")
tab_geo |>
  dplyr::filter(!is.na(.data$MatchClass)) |>
  dplyr::count(.data$Combo, .data$MatchClass, .data$GeoLevel, name = "N") |>
  dplyr::arrange(.data$Combo, dplyr::desc(.data$N)) |>
  print(n = Inf, width = Inf)

cli::cli_alert_info(
  "The row that matters is US Populated Place resolving to County. That is a city being used as \\
   EVIDENCE for a county rather than reported as a level of its own -- carried, resolved, and never \\
   published as a city."
)

cli::cli_h2("The aligned output, as it would be stored")
tab_geo |>
  dplyr::filter(!is.na(.data$GeoLevel)) |>
  dplyr::slice_head(n = 2L, by = c(Combo, GeoLevel)) |>
  dplyr::select(Combo, Span, MatchClass, GeoLevel, GeoIso3, GeoIso2, GeoState, GeoCounty) |>
  dplyr::arrange(.data$GeoLevel, .data$Combo) |>
  print(n = 40, width = Inf)

cli::cli_h2("Do two engines agree where both resolved the same span?")
# COMPARED ONLY WHERE BOTH RESOLVED. Counting NA as a value made a span that one engine resolved
# and another did not look like a disagreement, which reported 94.1% when the engines had not
# actually contradicted each other once.
tab_agree <- tab_geo |>
  dplyr::filter(!is.na(.data$GeoIso3)) |>
  dplyr::summarise(
    NEngIso3 = dplyr::n_distinct(.data$Combo),
    NIso3    = dplyr::n_distinct(.data$GeoIso3),
    NEngIso2 = dplyr::n_distinct(.data$Combo[!is.na(.data$GeoIso2)]),
    NIso2    = dplyr::n_distinct(.data$GeoIso2[!is.na(.data$GeoIso2)]),
    .by = c(DocID, Start, Stop, Span)
  )

tab_agree |>
  dplyr::summarise(
    SpansIso3 = sum(.data$NEngIso3 > 1L),
    AgreeIso3 = sum(.data$NEngIso3 > 1L & .data$NIso3 == 1L),
    SpansIso2 = sum(.data$NEngIso2 > 1L),
    AgreeIso2 = sum(.data$NEngIso2 > 1L & .data$NIso2 == 1L)
  ) |>
  dplyr::mutate(
    PctIso3 = round(100 * .data$AgreeIso3 / pmax(1L, .data$SpansIso3), 1),
    PctIso2 = round(100 * .data$AgreeIso2 / pmax(1L, .data$SpansIso2), 1)
  ) |>
  print(width = Inf)

if (any(tab_agree$NEngIso3 > 1L & tab_agree$NIso3 > 1L)) {
  cli::cli_alert_danger("Engines resolved the same offsets to different codes:")
  tab_geo |>
    dplyr::filter(!is.na(.data$GeoIso3)) |>   # engines that resolved nothing are not a conflict
    dplyr::semi_join(
      dplyr::filter(tab_agree, .data$NEngIso3 > 1L, .data$NIso3 > 1L),
      by = dplyr::join_by(DocID, Start, Stop, Span)
    ) |>
    dplyr::distinct(.data$Combo, .data$Span, .data$MatchClass, .data$GeoLevel, .data$GeoIso3,
                    .data$GeoIso2, .data$GeoState, .data$GeoCounty) |>
    dplyr::select(Combo, Span, MatchClass, GeoLevel, GeoIso3, GeoIso2, GeoState, GeoCounty) |>
    dplyr::arrange(.data$Span, .data$Combo) |>
    print(n = 30, width = Inf)
} else {
  cli::cli_alert_success("No span was resolved to two different codes by two engines.")
}


# 9b. Is the registered address available to test against? ----
# 04A already requests BusinessAddress and MailingAddress through .cols_landing, joined DocID ->
# HashIndex -> landing page. Whether they RESOLVED is an environment fact, so it is checked rather
# than assumed -- and it decides whether the keep-or-drop measurement for populated places can run
# at all.

cols_anchor_ <- names(arrow::read_parquet(.path_keys, as_data_frame = FALSE))
has_addr_    <- c("BusinessAddress", "MailingAddress") %in% cols_anchor_

cli::cli_h2("The EDGAR address anchor")
if (all(has_addr_)) {
  tab_addr <- arrow::read_parquet(.path_keys) |>
    dplyr::filter(.data$DocID %in% tab_pick$DocID) |>
    dplyr::transmute(
      DocID,
      Addr = stringi::stri_trans_toupper(
        paste(dplyr::coalesce(.data$BusinessAddress, ""),
              dplyr::coalesce(.data$MailingAddress, ""))
      )
    )

  tab_geo |>
    dplyr::inner_join(tab_addr, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      InAddr = stringi::stri_length(.data$Span) >= 4L &
        stringi::stri_detect_fixed(.data$Addr, stringi::stri_trans_toupper(.data$Span))
    ) |>
    dplyr::summarise(
      Spans     = dplyr::n(),
      InAddress = sum(.data$InAddr),
      Docs      = dplyr::n_distinct(.data$DocID),
      DocsHit   = dplyr::n_distinct(.data$DocID[.data$InAddr]),
      .by = c(Combo, MatchClass)
    ) |>
    dplyr::arrange(dplyr::desc(.data$InAddress)) |>
    print(n = 25, width = Inf)

  cli::cli_alert_info(
    "THIS IS THE KEEP-OR-DROP MEASUREMENT FOR POPULATED PLACES, in miniature. Most filers are \\
     incorporated in Delaware and located elsewhere, so a state-level span matches the registered \\
     address rarely; the city in a notices clause is the token that can. Compare the US Populated \\
     Place row against the US State row before deciding the class earns its place."
  )
} else {
  cli::cli_alert_warning(
    "sample_anchors.parquet carries no address column. 04A requests both through .cols_landing, so \\
     this means the landing-page join did not resolve -- ent_report_sample() in 04A names which \\
     columns were found and which were not."
  )
}


# 10. Read the same documents through every engine ----

tab_long <- purrr::imap(lst_raw, \(.d, .n) {
  dplyr::transmute(dplyr::filter(.d, !is.na(.data$Start)),
                   Combo = .n, DocID, Start, Stop, Span, LabelRaw)
}) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    Mark = dplyr::coalesce(
      unname(c("lexnlp" = "L", "paper:gazetteer-v1" = "G", "spacy:en_core_web_sm" = "s",
               "spacy:en_core_web_md" = "m", "spacy:en_core_web_lg" = "l",
               "spacy:en_core_web_trf" = "t")[.data$Combo]),
      "?"
    )
  )

docs_read_ <- withr::with_seed(.seed, sample(tab_pick$DocID, size = .n_read))

cli::cli_h2("The same documents, every engine")
purrr::walk(docs_read_, function(.d) {
  meta_ <- dplyr::filter(tab_pick, .data$DocID == .d)
  cli::cli_h3("{meta_$Class} | {meta_$CompanyName} | {format(meta_$DocLen, big.mark = ',')} chars")

  rows_ <- tab_long |>
    dplyr::filter(.data$DocID == .d) |>
    dplyr::summarise(
      Present = paste(
        purrr::map_chr(c("L", "G", "s", "m", "l", "t"),
                       \(.k) if (.k %in% .data$Mark) .k else "."),
        collapse = ""
      ),
      Class = paste(sort(unique(.data$LabelRaw)), collapse = "/"),
      Start = min(.data$Start),
      .by = c(Span)
    ) |>
    dplyr::arrange(.data$Start)

  cat(sprintf("  %7s %-6s %-22s %s\n", "Start", "LGsmlt", "LabelRaw", "Span"))
  purrr::walk(seq_len(nrow(rows_)), function(.i) {
    r_ <- rows_[.i, ]
    cat(sprintf("  %7d %-6s %-22s %s\n", r_$Start, r_$Present,
                stringi::stri_sub(r_$Class, from = 1L, to = 22L),
                stringi::stri_replace_all_regex(r_$Span, "\\s+", " ")))
  })
  cat("\n")
})

cli::cli_alert_info(
  "Read the position as much as the span. A place in the opening is a party's jurisdiction; a place \\
   in the final fifth is governing law, and the same string means different things in the two \\
   places. That is why 04A left this engine choice open rather than crowning one on a pooled \\
   statistic."
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
