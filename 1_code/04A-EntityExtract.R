# 04A-EntityExtract: freeze the extraction inputs, run every engine, describe the yield ----
#
# WHAT THIS FILE DOES, IN ONE PARAGRAPH
# 04A takes the 4.4k hand-labelled contracts that 03A prepared, freezes ONE canonical text per
# document, runs every entity extractor over that text, and folds the results into the DuckDB
# candidate store. It then describes what came back: how many candidates per engine, where in the
# document they sit, how far the engines agree, and how the yield varies by contract type. Nothing
# is selected, filtered or ranked here.
#
# WHY IT ONLY DESCRIBES
# The 03 family had 4.4k hand labels and could compute macro-F1. There are no entity labels. Without
# them, the only measurable quantity at this stage is YIELD -- candidates per document, documents
# with a hit. Yield does not order engines: a higher count is either better recall or worse
# precision, and the two are indistinguishable until a gold standard exists. So this document
# answers "what is there and where do the engines differ", and leaves "which engine is right" to
# 04C, which can only run once 04B has built something to be right against.
#
# THE ONE DECISION THAT CANNOT BE UNDONE LATER
# Offsets are integers into a specific string. Every candidate in the store, every context window
# the adjudicator will read, and every rehydrated span in the QA export indexes THAT string. If two
# scripts disagree about what a document's text is, every offset in the project is quietly wrong and
# nothing errors. 04A therefore writes sample_text.parquet and is the only script permitted to
# define it; everything downstream reads that file and never reconstructs the text.
#
# TWO STORES, NOT ONE
# The ledger records that a document was processed by an engine; it does not record
# whether the text was truncated first. A single store therefore cannot hold both a
# full-text and a truncated extraction -- the second would be skipped as already done.
# 04A builds two: the full-text store, which is the substantive one, and a head-window
# store used only to measure what truncation costs in time and buys in throughput. Their
# manifests are separate and neither can be mistaken for the other.
#
# WHAT IS *NOT* HERE
# The gold standard and the anchor checks are 04B. Engine selection is 04C. Field resolution --
# parties, contract dates, party locations, contract value -- is 04D. The corpus pass is 04E. The
# reusable engine seam and the store itself (ner_*) live in _Commons/_NER.R and are shared with all
# of them; only the glue specific to THIS sample carries the ent_ prefix.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns in dplyr verbs, bare CamelCase for new columns; if (FALSE) dev
# blocks; cli/fs/here; pure ASCII; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_prepared <- .lP$Input$Prepared
  .path_meta     <- .lP$Input$MetaData
  .path_text     <- .lP$Sample$Text
  .db_path       <- .lP$Store$NerDB
}


# 1. Sample construction --------------------------------------------------
# Two artifacts come out of this section and both are consumed by every later script in the family.
# sample_text.parquet is the offset basis. sample_anchors.parquet is everything known about a
# document from outside its text: its labels, its fold, and the EDGAR facts that 04B will validate
# the extractions against.

#' Write the canonical text artifact that every offset in the family indexes
#'
#' Takes the text straight from prepared.parquet rather than re-reading the per-document parquets.
#' 03A built its Text column by concatenating a document's rows with newlines; a document whose
#' parquet holds more than one row therefore has a prepared.parquet text that differs from its
#' on-disk TextRaw. Re-reading here would produce a second, subtly different string, and every
#' offset computed against one would be wrong against the other with no error raised. Deriving from
#' prepared.parquet makes the two identical by construction, at the cost of nothing.
#'
#' The column is renamed to TextRaw because that is the name every extractor and every ner_* helper
#' expects; renaming here means no downstream call needs a special case.
#'
#' The file is rewritten on every call rather than reused when present. Skipping the write would
#' defeat the run manifest exactly when it is needed: the manifest fingerprints this file, so a
#' changed prepared.parquet behind a reused text file produces an unchanged fingerprint and reports
#' a match while the store is stale. Copying one column of 4,400 rows costs seconds, which is well
#' below the price of that trap.
#'
#' @param .path_prepared Path to 03A's prepared.parquet.
#' @param .path_out Destination parquet.
#' @return Invisibly the path written.
ent_write_text <- function(.path_prepared, .path_out) {
  if (FALSE) {
    .path_prepared <- .lP$Input$Prepared
    .path_out      <- .lP$Sample$Text
  }

  fs::dir_create(fs::path_dir(.path_out))
  arrow::open_dataset(sources = .path_prepared) |>
    dplyr::select(DocID, Text) |>
    dplyr::collect() |>
    dplyr::rename(TextRaw = Text) |>
    arrow::write_parquet(.path_out)

  cli::cli_alert_success("Canonical text written: {.path {fs::path_file(.path_out)}}")
  invisible(.path_out)
}

#' Build the sample table: labels, fold, and the external facts entities will be checked against
#'
#' The labels and the fold come from 03A so that any per-class or per-fold statement in this family
#' is made on exactly the partition the classifiers were scored on. The EDGAR columns are the reason
#' this function exists at all: the filer's own name, its CIK, its filing date and its addresses are
#' facts about the contract that were never derived from its text, so they can be used to test the
#' extraction without anyone annotating anything. 04B turns them into recall floors; 04A only makes
#' sure they are present and says which are not.
#'
#' Anchor columns are taken with any_of() rather than named directly. Which EDGAR table carries
#' which field is an environment fact, not a code fact, and a hard-coded list would abort on a
#' machine where CompanyName lives in the landing-page export instead. Missing columns are reported,
#' not fatal, so the extraction can proceed while the gap is fixed before 04B needs it.
#'
#' The addresses need a second source. Document-level metadata is keyed on DocID and carries the
#' filer name and filing date, but the registered addresses live on the filing's landing page and
#' are keyed on HashIndex. The join therefore hops DocID -> HashIndex -> address. Both sources are
#' optional in the same way as the columns themselves: a landing page that is absent or unkeyed
#' costs the geographic anchor and nothing else, and is reported rather than fatal.
#'
#' @param .path_prepared Path to 03A's prepared.parquet. Text is deliberately not read.
#' @param .path_meta Path to the EDGAR document metadata parquet.
#' @param .path_landing Path to the EDGAR landing-page parquet, or NULL to skip it.
#' @param .cols_anchor Character vector of anchor columns to take from the document metadata.
#' @param .cols_landing Character vector of anchor columns to take from the landing page.
#' @return Tibble: one row per document, carrying DocID, the three task labels, LabelRound, Fold and
#'   whichever anchors resolved. Carries attr "Anchors", a tibble of found/missing per requested
#'   column, for ent_report_sample().
ent_build_sample <- function(.path_prepared, .path_meta, .path_landing = NULL,
                             .cols_anchor = c("CIK", "CompanyName", "DateFiled",
                                              "nCIK", "MultFiler"),
                             .cols_landing = c("BusinessAddress", "MailingAddress")) {
  if (FALSE) {
    .path_prepared <- .lP$Input$Prepared
    .path_meta     <- .lP$Input$MetaData
    .path_landing  <- .lP$Input$LandingPage
    .cols_anchor   <- c("CIK", "CompanyName", "DateFiled", "nCIK", "MultFiler")
    .cols_landing  <- c("BusinessAddress", "MailingAddress")
  }

  base_ <- arrow::open_dataset(sources = .path_prepared) |>
    dplyr::select(DocID, DocDesc, DocName, ClassBroad, ClassDetailed, ClassDetailed2,
                  AmendType, LabelRound, Fold) |>
    dplyr::collect()

  # HashIndex is requested alongside the anchors: it is not itself an anchor, it is the key the
  # landing page is joined on.
  want_meta_ <- unique(c(.cols_anchor, "HashIndex"))
  avail_ <- arrow::open_dataset(sources = .path_meta)$schema$names
  have_  <- intersect(want_meta_, avail_)
  miss_  <- setdiff(.cols_anchor, avail_)

  meta_ <- arrow::open_dataset(sources = .path_meta) |>
    dplyr::select(dplyr::all_of(c("DocID", have_))) |>
    dplyr::collect() |>
    dplyr::distinct(.data$DocID, .keep_all = TRUE)

  out_ <- base_ |> dplyr::left_join(meta_, by = dplyr::join_by(DocID))

  # Landing page, via HashIndex. Skipped without complaint when either the file or the key is
  # absent; ent_report_sample() surfaces the resulting gap.
  have_land_ <- character(0)
  if (!is.null(.path_landing) && fs::file_exists(.path_landing) && "HashIndex" %in% names(out_)) {
    avail_land_ <- arrow::open_dataset(sources = .path_landing)$schema$names
    have_land_  <- intersect(.cols_landing, avail_land_)
    if (length(have_land_) > 0L && "HashIndex" %in% avail_land_) {
      land_ <- arrow::open_dataset(sources = .path_landing) |>
        dplyr::select(dplyr::all_of(c("HashIndex", have_land_))) |>
        dplyr::collect() |>
        dplyr::distinct(.data$HashIndex, .keep_all = TRUE)
      out_ <- out_ |> dplyr::left_join(land_, by = dplyr::join_by(HashIndex))
    } else {
      have_land_ <- character(0)
    }
  }
  want_ <- c(.cols_anchor, .cols_landing)

  # Status is read off the assembled table rather than off either source list. Tracking which
  # columns each source supplied means keeping two lists in step with the joins, and a list that
  # falls behind reports a column MISSING while its values sit in the table -- which is exactly what
  # happened when the landing-page source was added. The table cannot disagree with itself.
  anchors_ <- tibble::tibble(
    Column = want_,
    NonNA  = purrr::map_int(want_, \(.c) {
      if (!.c %in% names(out_)) return(NA_integer_)
      sum(!is.na(out_[[.c]]))
    })
  ) |>
    dplyr::mutate(
      Status = dplyr::case_when(
        is.na(.data$NonNA)  ~ "MISSING",   # the column never arrived
        .data$NonNA == 0L   ~ "EMPTY",     # it arrived carrying nothing
        TRUE                ~ "found"
      ),
      PctNonNA = .data$NonNA / nrow(out_)
    ) |>
    dplyr::relocate(Column, Status, NonNA, PctNonNA)

  attr(out_, "Anchors") <- anchors_
  gap_ <- anchors_$Column[anchors_$Status != "found"]
  if (length(gap_) > 0L) {
    cli::cli_alert_warning("Anchor column{?s} unusable: {gap_}")
  }
  out_
}

#' Write the sample table downstream scripts join on
#'
#' Kept separate from the text so that the small table can be read repeatedly without dragging a
#' few hundred megabytes of contract text with it. Every later script in the family joins entities
#' to this file, never to prepared.parquet, so the entity work depends on one narrow artifact.
#'
#' @param .tab Tibble from ent_build_sample().
#' @param .path_out Destination parquet.
#' @return Invisibly the path written.
ent_write_anchors <- function(.tab, .path_out) {
  if (FALSE) {
    .tab      <- tab_sample
    .path_out <- .lP$Sample$Anchors
  }
  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(.tab, .path_out)
  cli::cli_alert_success("Sample anchors written: {.path {fs::path_file(.path_out)}} ({nrow(.tab)} docs)")
  invisible(.path_out)
}


# 2. The run manifest -----------------------------------------------------
# _NER.R documents that the ledger is blind to labels and to .max_chars: a document ingested under
# one label set or one truncation counts as done for that engine forever. Re-running the same store
# with a different cap therefore does nothing at all and reports success. The manifest is the
# missing fingerprint -- it records what the store was actually built under, so the mismatch is
# visible rather than silent.

#' Describe the extraction the store is being built under
#'
#' Fingerprints the inputs as well as the settings. A settings-only key would not notice that the
#' text changed underneath a store that was already complete, which is exactly the failure the
#' project's cache rules exist to prevent. Character count over the canonical text is a cheap proxy
#' that changes whenever the sample or the text derivation changes.
#'
#' @param .path_text Canonical text parquet.
#' @param .run Character vector of ner_run() combo tokens.
#' @param .labels Character vector of unified labels requested, or NULL for per-combo policy.
#' @param .max_chars Integer or NULL. Truncation applied before extraction.
#' @return One-row tibble: NDocs, TextChars, Run, Labels, MaxChars, CreatedAt.
ent_manifest <- function(.path_text, .run, .labels, .max_chars) {
  if (FALSE) {
    .path_text <- .lP$Sample$Text
    .run       <- names(ner_tuning)
    .labels    <- NULL
    .max_chars <- NULL
  }

  con_ <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
  fp_ <- DBI::dbGetQuery(con_, paste0(
    "SELECT COUNT(*) AS NDocs, SUM(length(TextRaw)) AS TextChars ",
    "FROM read_parquet('", as.character(fs::path_abs(.path_text)), "')"
  ))

  tibble::tibble(
    NDocs     = as.integer(fp_$NDocs),
    TextChars = as.numeric(fp_$TextChars),
    Run       = paste(sort(.run), collapse = " | "),
    Labels    = if (is.null(.labels)) "per-combo policy" else paste(sort(.labels), collapse = ","),
    MaxChars  = if (is.null(.max_chars)) NA_integer_ else as.integer(.max_chars),
    CreatedAt = Sys.time()
  )
}

#' Compare the current extraction settings against the ones the store was built under
#'
#' Writes the manifest on first use and compares on every later use. A mismatch is reported rather
#' than aborted because during development the settings legitimately move.
#'
#' Not every mismatch means the same thing, and treating them alike is how a good store gets thrown
#' away. Two kinds:
#'
#'   FINGERPRINT -- NDocs, TextChars, MaxChars, Labels. The ledger is blind to all four, so a change
#'     here means the store answers a different question than the one now being asked while
#'     reporting itself complete. The store must be moved aside.
#'   INVENTORY -- Run. Which engines have been asked for is not a property of what is already
#'     stored. Adding a combo is the ordinary incremental case the ledger handles correctly, and
#'     the manifest is updated in place. Only a combo that DISAPPEARS is worth a second look, and
#'     even then only because the store now holds more than the document describes.
#'
#' Promote a fingerprint mismatch to an abort before the corpus pass, where a silent stale store
#' costs days.
#'
#' @param .path_manifest Manifest parquet path.
#' @param .manifest One-row tibble from ent_manifest().
#' @return Tibble: Field, Kind, Stored, Current, Match, Note. Fingerprint rows first.
ent_manifest_sync <- function(.path_manifest, .manifest) {
  if (FALSE) {
    .path_manifest <- .lP$Store$Manifest
    .manifest      <- ent_manifest(.lP$Sample$Text, names(ner_tuning), NULL, NULL)
  }

  keys_ <- c("NDocs", "TextChars", "MaxChars", "Labels", "Run")
  kind_ <- c("fingerprint", "fingerprint", "fingerprint", "fingerprint", "inventory")

  # as.character() on a large double yields scientific notation, so a character count of 250 million
  # would render as "2.5e+08" and compare equal to any other value rounding the same way.
  show_ <- function(.x) {
    if (is.numeric(.x)) format(.x, scientific = FALSE, trim = TRUE) else as.character(.x)
  }
  cur_ <- purrr::map_chr(keys_, \(.k) show_(.manifest[[.k]]))

  if (!fs::file_exists(.path_manifest)) {
    fs::dir_create(fs::path_dir(.path_manifest))
    arrow::write_parquet(.manifest, .path_manifest)
    return(tibble::tibble(
      Field = keys_, Kind = kind_, Stored = "(new)", Current = cur_,
      Match = TRUE, Note = "store created"
    ))
  }

  old_ <- arrow::read_parquet(.path_manifest)
  out_ <- tibble::tibble(
    Field   = keys_,
    Kind    = kind_,
    Stored  = purrr::map_chr(keys_, \(.k) show_(old_[[.k]])),
    Current = cur_
  ) |>
    # NA == NA is NA, not TRUE, so a comparison alone would leave MaxChars unmatched whenever
    # truncation is off on both sides. coalesce() resolves it to the both-missing case.
    dplyr::mutate(Same = dplyr::coalesce(
      .data$Stored == .data$Current,
      is.na(.data$Stored) & is.na(.data$Current)
    ))

  # Engine inventory: name what moved rather than reporting a wall of two run strings.
  split_ <- function(.x) if (is.na(.x)) character(0) else trimws(strsplit(.x, "|", fixed = TRUE)[[1]])
  was_ <- split_(out_$Stored[out_$Field == "Run"])
  now_ <- split_(out_$Current[out_$Field == "Run"])
  added_ <- setdiff(now_, was_)
  gone_  <- setdiff(was_, now_)

  out_ <- out_ |>
    dplyr::mutate(
      Match = dplyr::if_else(.data$Kind == "inventory", length(gone_) == 0L, .data$Same),
      Note  = dplyr::case_when(
        .data$Kind == "inventory" & length(added_) > 0L & length(gone_) == 0L ~
          paste0("added: ", paste(added_, collapse = ", ")),
        .data$Kind == "inventory" & length(gone_) > 0L ~
          paste0("store holds combos this run does not ask for: ", paste(gone_, collapse = ", ")),
        .data$Kind == "inventory" ~ "unchanged",
        .data$Same  ~ "",
        TRUE        ~ "store is stale -- move it aside"
      )
    ) |>
    dplyr::select(Field, Kind, Stored, Current, Match, Note)

  # An inventory-only change is routine, so record it and stop flagging it next time.
  if (all(out_$Match[out_$Kind == "fingerprint"]) && !identical(was_, now_)) {
    arrow::write_parquet(.manifest, .path_manifest)
  }
  out_
}


# 3. Offset integrity -----------------------------------------------------
# The single check that has to pass before anything else in this family means anything.

#' Rehydrate a stratified sample of candidates and confirm the span matches the stored text
#'
#' _NER.R ships ner_check_offsets(), which derives DocID from a file name and therefore assumes one
#' parquet per document. The canonical text here is a single many-row file, so that helper cannot
#' see it. This one runs the round-trip inside DuckDB against the canonical text instead: the text
#' never enters R, and the comparison is made on the same code-point convention the extractors emit
#' (0-based, half-open, so text[Start:Stop] is the span).
#'
#' Sampling is deterministic through a hash of the candidate key rather than a random draw, so the
#' number does not move between renders and a regression is attributable to the data.
#'
#' @param .db_path DuckDB candidate store.
#' @param .path_text Canonical text parquet.
#' @param .n Integer. Candidates sampled per engine x model.
#' @return Tibble: Engine, Model, N, NOk, OkShare.
ent_check_offsets <- function(.db_path, .path_text, .n = 2000L) {
  if (FALSE) {
    .db_path   <- .lP$Store$NerDB
    .path_text <- .lP$Sample$Text
    .n         <- 2000L
  }

  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")

  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  DBI::dbGetQuery(con_, paste0(
    "WITH txt AS (SELECT DocID, TextRaw FROM read_parquet('",
    as.character(fs::path_abs(.path_text)), "')), ",
    "smp AS ( ",
    "  SELECT DocID, Start, Stop, Span, Engine, Model FROM candidates ",
    "  QUALIFY row_number() OVER (PARTITION BY Engine, Model ",
    "    ORDER BY hash(DocID || '#' || CAST(Start AS VARCHAR))) <= ", as.integer(.n),
    ") ",
    "SELECT smp.Engine, smp.Model, COUNT(*) AS N, ",
    "  SUM(CASE WHEN substring(txt.TextRaw, smp.Start + 1, smp.Stop - smp.Start) = smp.Span ",
    "      THEN 1 ELSE 0 END) AS NOk ",
    "FROM smp JOIN txt USING (DocID) GROUP BY smp.Engine, smp.Model ORDER BY smp.Engine, smp.Model"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      N       = as.integer(.data$N),
      NOk     = as.integer(.data$NOk),
      OkShare = .data$NOk / .data$N
    )
}


# 4. Agreement, yield and outliers ----------------------------------------
# Jaccard says how far two engines overlap but not which way. Containment separates a model that
# genuinely adds mentions from one that merely restates a smaller model's output, which is the only
# engine question answerable before a gold standard exists.

#' Directional overlap between engine pairs, per label
#'
#' Derived from ner_alignment()$pairwise rather than recomputed, so the mention clustering behind
#' both is identical by construction. InA is the share of A's mentions that B also found; InB the
#' reverse. A pair with InA near one and InB well below it means A's output is a subset of B's, so
#' A carries no independent information and the choice between them is free. A pair low in both
#' directions is complementary, and is where 04B has to concentrate its sampling.
#'
#' @param .pairwise Tibble from ner_alignment()$pairwise.
#' @return Tibble: Label, ComboA, ComboB, N_A, N_B, Both, InA, InB, Jaccard, sorted by the weaker
#'   containment ascending, so the most complementary pairs come first.
ent_containment <- function(.pairwise) {
  if (FALSE) .pairwise <- .al$pairwise

  .pairwise |>
    dplyr::mutate(
      InA     = .data$Both / .data$N_A,
      InB     = .data$Both / .data$N_B,
      MinIn   = pmin(.data$InA, .data$InB)
    ) |>
    dplyr::select(Label, ComboA, ComboB, N_A, N_B, Both, InA, InB, Jaccard, MinIn) |>
    dplyr::arrange(.data$MinIn)
}


#' Collapse the engine axis to architecture families, and count agreement across those
#'
#' Counting how many engines found a mention overstates agreement whenever the engine set is
#' unbalanced, and this one is: four of the seven combos are spaCy. A mention found by four engines
#' is usually a mention that spaCy agrees with itself about, which is a statement about shared
#' weights rather than corroboration. Grouping into families -- spaCy, LexNLP, the ported paper
#' rules -- gives at most three votes, and each is cast by a method that shares nothing with the
#' others. That is the number worth stratifying a gold standard on.
#'
#' @param .mentions Tibble from ner_alignment()$mentions; its Combos column is the combo set.
#' @return Tibble: Label, NFamilies, NMentions, FamiliesEligible, ShareOfMentions.
ent_family_consensus <- function(.mentions) {
  if (FALSE) .mentions <- .al$mentions

  fam_ <- .mentions |>
    dplyr::mutate(
      HasSpacy  = stringr::str_detect(.data$Combos, stringr::fixed("spacy")),
      HasLexnlp = stringr::str_detect(.data$Combos, stringr::fixed("lexnlp")),
      HasPaper  = stringr::str_detect(.data$Combos, stringr::fixed("paper")),
      NFamilies = as.integer(.data$HasSpacy) + as.integer(.data$HasLexnlp) +
        as.integer(.data$HasPaper)
    )

  elig_ <- fam_ |>
    dplyr::summarise(
      FamiliesEligible = as.integer(any(.data$HasSpacy) + any(.data$HasLexnlp) + any(.data$HasPaper)),
      .by = Label
    )

  fam_ |>
    dplyr::count(.data$Label, .data$NFamilies, name = "NMentions") |>
    dplyr::left_join(elig_, by = "Label") |>
    dplyr::mutate(ShareOfMentions = .data$NMentions / sum(.data$NMentions), .by = Label) |>
    dplyr::arrange(.data$Label, .data$NFamilies)
}


#' Yield per contract type, normalised by the text each type actually contains
#'
#' Candidates per document ranks contract types partly by how long they are. Median length differs
#' by more than a factor of two across this taxonomy, so a type can lead on every entity column for
#' no reason beyond page count. Dividing by characters removes that, and reporting median length
#' beside it makes the size difference visible rather than silently absorbed.
#'
#' @param .db_path DuckDB candidate store.
#' @param .path_text Canonical text parquet, supplying per-document lengths.
#' @param .path_class Parquet carrying DocID and the class column.
#' @param .class_col Class column name.
#' @param .ref_combo Engine the table is built for; one engine, else the columns are not comparable.
#' @return Tibble: Class, NDocs, MedianChars, then one CandPer1k column per label.
ent_yield_by_class <- function(.db_path, .path_text, .path_class,
                               .class_col = "ClassDetailed",
                               .ref_combo = "spacy:en_core_web_trf") {
  if (FALSE) {
    .db_path    <- .lP$Store$NerDB
    .path_text  <- .lP$Sample$Text
    .path_class <- .lP$Sample$Anchors
    .class_col  <- "ClassDetailed"
    .ref_combo  <- .lP$Params$RefCombo
  }

  ref_ <- ner_parse_combo(.ref_combo)
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  tab_ <- DBI::dbGetQuery(con_, paste0(
    "WITH txt AS (SELECT DocID, length(TextRaw) AS DocLen FROM read_parquet('",
    as.character(fs::path_abs(.path_text)), "')), ",
    "cls AS (SELECT DocID, CAST(\"", .class_col, "\" AS VARCHAR) AS Class FROM read_parquet('",
    as.character(fs::path_abs(.path_class)), "') WHERE \"", .class_col, "\" IS NOT NULL), ",
    "docs AS (SELECT cls.Class, COUNT(*) AS NDocs, SUM(txt.DocLen) AS Chars, ",
    "                median(txt.DocLen) AS MedianChars ",
    "         FROM cls JOIN txt USING (DocID) GROUP BY cls.Class), ",
    "cnd AS (SELECT cls.Class, c.Label, COUNT(*) AS N ",
    "        FROM candidates c JOIN cls USING (DocID) ",
    "        WHERE c.Engine = ? AND c.Model = ? GROUP BY cls.Class, c.Label) ",
    "SELECT docs.Class, docs.NDocs, docs.MedianChars, cnd.Label, ",
    "       1000.0 * cnd.N / docs.Chars AS CandPer1k ",
    "FROM docs LEFT JOIN cnd USING (Class)"
  ), params = list(ref_$Engine[1], ref_$Model[1]))

  tab_ |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$Label)) |>
    tidyr::pivot_wider(names_from = Label, values_from = CandPer1k, values_fill = 0) |>
    dplyr::mutate(
      NDocs       = as.integer(.data$NDocs),
      MedianChars = as.integer(.data$MedianChars)
    ) |>
    dplyr::arrange(dplyr::desc(.data$NDocs))
}


#' The longest documents, and how much of the store they account for
#'
#' Every pooled candidate statistic in this document is candidate-weighted, so one document long
#' enough to hold a percent of the corpus text carries a percent of the vote in the positional
#' histogram and in every per-engine total. The quality screens upstream keep tabular filings out
#' of the corpus, but a document that survived them and still runs to several megabytes is worth
#' looking at before its evidence is trusted.
#'
#' @param .db_path DuckDB candidate store.
#' @param .path_text Canonical text parquet.
#' @param .path_class Parquet carrying DocID and ClassDetailed, for context on what they are.
#' @param .n Integer. How many of the longest documents to return.
#' @return Tibble: DocID, ClassDetailed, Chars, PctOfCorpusChars, Candidates, PctOfCandidates.
ent_length_outliers <- function(.db_path, .path_text, .path_class, .n = 10L) {
  if (FALSE) {
    .db_path    <- .lP$Store$NerDB
    .path_text  <- .lP$Sample$Text
    .path_class <- .lP$Sample$Anchors
    .n          <- 10L
  }

  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  DBI::dbGetQuery(con_, paste0(
    "WITH txt AS (SELECT DocID, length(TextRaw) AS Chars FROM read_parquet('",
    as.character(fs::path_abs(.path_text)), "')), ",
    "cls AS (SELECT DocID, ClassDetailed FROM read_parquet('",
    as.character(fs::path_abs(.path_class)), "')), ",
    "cnd AS (SELECT DocID, COUNT(*) AS Candidates FROM candidates GROUP BY DocID), ",
    "tot AS (SELECT SUM(Chars) AS AllChars FROM txt), ",
    "totc AS (SELECT COUNT(*) AS AllCands FROM candidates) ",
    "SELECT txt.DocID, cls.ClassDetailed, txt.Chars, ",
    "       100.0 * txt.Chars / tot.AllChars AS PctOfCorpusChars, ",
    "       COALESCE(cnd.Candidates, 0) AS Candidates, ",
    "       100.0 * COALESCE(cnd.Candidates, 0) / totc.AllCands AS PctOfCandidates ",
    "FROM txt LEFT JOIN cls USING (DocID) LEFT JOIN cnd USING (DocID), tot, totc ",
    "ORDER BY txt.Chars DESC LIMIT ", as.integer(.n)
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      Chars      = as.integer(.data$Chars),
      Candidates = as.integer(.data$Candidates)
    )
}


# 5. The cost of truncation -----------------------------------------------
# Two questions about reading only the head of a document, and they are answered differently.
# What truncation COSTS in recall needs no second extraction at all: every candidate in the
# full-text store carries its offsets, so the share falling inside any window is a query. What
# truncation SAVES in time cannot be answered that way, because a truncated pass is not a filtered
# full pass -- the engines see different context near the cut, and the gazetteer's anchor gate can
# lose the state that licensed a city. That needs its own store, and its own manifest, because the
# ledger cannot tell the two apart.

#' Character cap corresponding to a word budget, derived from the sample rather than assumed
#'
#' A cap has to be expressed in characters because that is what the extractors take, but it is
#' reasoned about in words, and the conversion is a property of the text. Legal prose runs long
#' words and dense citation, so a rate borrowed from general English would be wrong in the
#' direction that matters. Measuring the character offset of the Nth whitespace token per document
#' and taking the median gives a cap under which the typical document contributes about N words;
#' the quantiles alongside show how far from typical the tails are.
#'
#' @param .path_text Canonical text parquet.
#' @param .n_words Integer. The word budget the cap should correspond to.
#' @param .round_to Integer. Round the cap up to a multiple of this, so it is a reportable number.
#' @return One-row tibble: NWords, Cap, P10, P50, P90, PctDocsShorter.
ent_head_cap <- function(.path_text, .n_words = 512L, .round_to = 100L) {
  if (FALSE) {
    .path_text <- .lP$Sample$Text
    .n_words   <- 512L
    .round_to  <- 100L
  }

  con_ <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  q_ <- DBI::dbGetQuery(con_, paste0(
    "WITH h AS (SELECT length(TextRaw) AS DocLen, ",
    "  length(regexp_extract(TextRaw, '^(\\s*\\S+){0,", as.integer(.n_words), "}')) AS HeadChars ",
    "  FROM read_parquet('", as.character(fs::path_abs(.path_text)), "')) ",
    "SELECT quantile_cont(HeadChars, 0.10) AS P10, median(HeadChars) AS P50, ",
    "       quantile_cont(HeadChars, 0.90) AS P90, ",
    "       AVG(CASE WHEN DocLen <= HeadChars THEN 1.0 ELSE 0.0 END) AS PctDocsShorter FROM h"
  ))

  tibble::tibble(
    NWords         = as.integer(.n_words),
    Cap            = as.integer(ceiling(q_$P50 / .round_to) * .round_to),
    P10            = as.integer(q_$P10),
    P50            = as.integer(q_$P50),
    P90            = as.integer(q_$P90),
    PctDocsShorter = as.numeric(q_$PctDocsShorter)
  )
}


#' Run each engine separately and record how long it took
#'
#' ner_run() takes the whole engine set at once, which is the right interface for populating a
#' store and the wrong one for costing it: the summary it returns says how much was extracted, not
#' how long any one engine spent. Looping combo by combo costs nothing -- the ledger still skips
#' what is present -- and turns the same call into a measurement.
#'
#' Only a combo that actually did work yields a timing. On a second render every combo is already
#' complete and reports zero seconds, which is true and useless, so the caller appends measurements
#' to a log and reports from that instead of from the live run.
#'
#' @param .path_text Canonical text parquet.
#' @param .db_path Store to populate.
#' @param .tuning Named per-combo knob list, as passed to ner_run_args().
#' @param .labels Labels to request, or NULL for each engine's own policy.
#' @param .max_chars Truncation applied before extraction, or NULL for none.
#' @param .docs_per_run Slice size.
#' @param .device spaCy device string.
#' @return Tibble: Engine, Model, Combo, Missing, Docs, Candidates, Seconds, MaxChars.
ent_time_engines <- function(.path_text, .db_path, .tuning, .labels, .max_chars,
                             .docs_per_run = 2500L, .device = "auto") {
  if (FALSE) {
    .path_text    <- .lP$Sample$Text
    .db_path      <- .lP$Store$NerDB
    .tuning       <- ner_tuning
    .labels       <- NULL
    .max_chars    <- NULL
    .docs_per_run <- 2500L
    .device       <- "auto"
  }

  args_ <- ner_run_args(.tuning = .tuning)
  out_ <- vector("list", length(args_$.run))

  for (i_ in seq_along(args_$.run)) {
    tok_ <- args_$.run[i_]
    cli::cli_alert_info("Timing {(tok_)} ...")
    t0_ <- Sys.time()
    res_ <- ner_run(
      .inputs        = .path_text,
      .db_path       = .db_path,
      .run           = tok_,
      .labels        = .labels,
      .max_chars     = .max_chars,
      .retry_timeout = FALSE,
      .id_col        = "DocID",
      .text_col      = "TextRaw",
      .docs_per_run  = .docs_per_run,
      .device        = .device,
      .n_process     = args_$.n_process,
      .batch_size    = args_$.batch_size,
      .timeout       = args_$.timeout,
      .keep_staging  = FALSE,
      .quiet         = TRUE
    )
    out_[[i_]] <- res_ |>
      dplyr::mutate(
        Combo    = tok_,
        Seconds  = as.numeric(difftime(Sys.time(), t0_, units = "secs")),
        MaxChars = if (is.null(.max_chars)) NA_integer_ else as.integer(.max_chars)
      )
  }
  dplyr::bind_rows(out_) |> dplyr::relocate(Combo)
}


#' Append the measurements that actually measured something to the timing log
#'
#' Append-only, so a re-measurement never overwrites the record of what was previously observed and
#' the log doubles as a history of how the machine and the settings changed. Rows where no work
#' happened are dropped: a combo the ledger skipped took no time, and recording that as a timing
#' would make the next render report a corpus projection of zero.
#'
#' @param .tab Tibble from ent_time_engines().
#' @param .path_log Timing-log parquet.
#' @return Invisibly the rows appended.
ent_timing_append <- function(.tab, .path_log) {
  if (FALSE) {
    .tab      <- tab_time_full
    .path_log <- .lP$Store$TimingLog
  }

  add_ <- .tab |>
    dplyr::filter(.data$Docs > 0L) |>
    dplyr::transmute(
      Combo    = .data$Combo,
      MaxChars = .data$MaxChars,
      Docs     = as.integer(.data$Docs),
      Candidates = as.integer(.data$Candidates),
      Seconds  = round(.data$Seconds, 1),
      MeasuredAt = Sys.time()
    )
  if (nrow(add_) == 0L) return(invisible(add_))

  fs::dir_create(fs::path_dir(.path_log))
  all_ <- if (fs::file_exists(.path_log)) {
    dplyr::bind_rows(arrow::read_parquet(.path_log), add_)
  } else {
    add_
  }
  arrow::write_parquet(all_, .path_log)
  cli::cli_alert_success("Timing log: appended {nrow(add_)} measurement{?s}")
  invisible(add_)
}


#' The most recent measurement per combo and cap, with a corpus projection
#'
#' The projection scales by DOCUMENT COUNT under truncation and by CHARACTER COUNT without it, and
#' the difference is not cosmetic. A capped pass does the same work on every document regardless of
#' its length, so its cost is linear in how many there are. An uncapped pass does work proportional
#' to the text, so a corpus whose documents average what this sample's do costs what this sample
#' cost, scaled by characters. Applying the wrong one understates an uncapped corpus pass by
#' whatever the length distribution happens to do.
#'
#' @param .path_log Timing-log parquet.
#' @param .corpus_docs Integer. Documents in the full corpus.
#' @return Tibble: Combo, MaxChars, Docs, Seconds, DocsPerSec, CorpusHours, MeasuredAt.
ent_timing_read <- function(.path_log, .corpus_docs) {
  if (FALSE) {
    .path_log    <- .lP$Store$TimingLog
    .corpus_docs <- .lP$Params$CorpusDocs
  }

  if (!fs::file_exists(.path_log)) {
    return(tibble::tibble(
      Combo = character(0), MaxChars = integer(0), Docs = integer(0), Seconds = numeric(0),
      DocsPerSec = numeric(0), CorpusHours = numeric(0), MeasuredAt = as.POSIXct(character(0))
    ))
  }

  arrow::read_parquet(.path_log) |>
    dplyr::slice_max(.data$MeasuredAt, n = 1L, by = c(Combo, MaxChars), with_ties = FALSE) |>
    dplyr::mutate(
      DocsPerSec  = .data$Docs / .data$Seconds,
      # Capped runs scale with document count; uncapped runs scale with text volume, and this
      # sample's documents are the corpus's documents, so the two coincide only by assumption.
      CorpusHours = (.data$Seconds / .data$Docs) * .corpus_docs / 3600
    ) |>
    dplyr::arrange(.data$MaxChars, dplyr::desc(.data$CorpusHours))
}


#' What a head window would have kept, measured on the full-text store
#'
#' Answers the recall side of truncation without extracting anything: every candidate already
#' carries its offsets, so the share of them ending inside the cap is a count. Read it per label,
#' because the answer differs sharply by label and a single number would hide exactly the finding
#' -- organisations are spread through the document while dates and places are not.
#'
#' @param .db_path Full-text candidate store.
#' @param .cap Integer character cap to evaluate.
#' @return Tibble: Combo, Label, N, NHead, PctHead.
ent_head_coverage <- function(.db_path, .cap) {
  if (FALSE) {
    .db_path <- .lP$Store$NerDB
    .cap     <- tab_cap$Cap
  }

  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  DBI::dbGetQuery(con_, paste0(
    "SELECT CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END AS Combo, ",
    "       Label, COUNT(*) AS N, ",
    "       SUM(CASE WHEN Stop <= ", as.integer(.cap), " THEN 1 ELSE 0 END) AS NHead ",
    "FROM candidates GROUP BY 1, 2 ORDER BY 1, 2"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      N       = as.integer(.data$N),
      NHead   = as.integer(.data$NHead),
      PctHead = .data$NHead / .data$N
    )
}


#' Whether a truncated pass equals the full pass filtered to the same window
#'
#' The two are not the same computation and there is no reason to expect the same answer. An engine
#' reading only the head sees a document that ends mid-sentence, so its context near the cut is
#' different; the gazetteer can lose the state that licensed a city two lines further down. A ratio
#' near one means truncation is a pure saving and 04E may take it. A ratio well below one means
#' truncation costs recall inside the window as well as outside it, which is the case a cap has to
#' be chosen against.
#'
#' @param .db_head Truncated store.
#' @param .db_full Full-text store.
#' @param .cap Integer character cap the truncated store was built under.
#' @return Tibble: Combo, Label, NHeadStore, NFullInHead, Ratio.
ent_head_vs_full <- function(.db_head, .db_full, .cap) {
  if (FALSE) {
    .db_head <- .lP$Store$NerDBHead
    .db_full <- .lP$Store$NerDB
    .cap     <- tab_cap$Cap
  }

  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_head), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con_, paste0(
    "ATTACH '", as.character(fs::path_abs(.db_full)), "' AS full_store (READ_ONLY)"
  ))

  DBI::dbGetQuery(con_, paste0(
    "WITH combo AS (SELECT CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END ",
    "                 AS Combo, Label FROM candidates), ",
    "h AS (SELECT Combo, Label, COUNT(*) AS NHeadStore FROM combo GROUP BY 1, 2), ",
    "f AS (SELECT CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END AS Combo, ",
    "             Label, COUNT(*) AS NFullInHead FROM full_store.candidates ",
    "      WHERE Stop <= ", as.integer(.cap), " GROUP BY 1, 2) ",
    "SELECT COALESCE(h.Combo, f.Combo) AS Combo, COALESCE(h.Label, f.Label) AS Label, ",
    "       COALESCE(h.NHeadStore, 0) AS NHeadStore, COALESCE(f.NFullInHead, 0) AS NFullInHead ",
    "FROM h FULL JOIN f ON h.Combo = f.Combo AND h.Label = f.Label ORDER BY 1, 2"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      NHeadStore  = as.integer(.data$NHeadStore),
      NFullInHead = as.integer(.data$NFullInHead),
      Ratio       = dplyr::if_else(.data$NFullInHead > 0L,
                                   .data$NHeadStore / .data$NFullInHead, NA_real_)
    )
}


# 6. Report ---------------------------------------------------------------
# Every block prints through 03A's fixed-width formatter and returns its tibble invisibly, so the
# console output of this document is one consistent, copy-pasteable stream and every number stays
# available afterwards.

#' Intake and anchor coverage for the extraction sample
#' @param .tab Tibble from ent_build_sample().
#' @return Invisibly the anchor-coverage tibble.
ent_report_sample <- function(.tab) {
  if (FALSE) .tab <- tab_sample

  cli::cli_h2("Extraction sample")
  clf_say_table(
    .tab = tibble::tibble(
      Item = c("Documents", "Detailed categories", "Broad categories", "Folds",
               "Amendments", "Originals"),
      N    = c(nrow(.tab),
               dplyr::n_distinct(.tab$ClassDetailed),
               dplyr::n_distinct(.tab$ClassBroad),
               dplyr::n_distinct(.tab$Fold),
               sum(.tab$AmendType == "Amended", na.rm = TRUE),
               sum(.tab$AmendType == "Original", na.rm = TRUE))
    )
  )

  anchors_ <- attr(.tab, "Anchors")
  cli::cli_h3("External anchors")
  clf_say_table(
    .tab = anchors_ |> dplyr::mutate(PctNonNA = clf_pct(.data$PctNonNA))
  )
  cli::cli_alert_info(
    "MISSING means the column never arrived and the check it supports cannot run in 04B. EMPTY \\
     means it arrived carrying nothing, which is a join failure rather than a source gap. A found \\
     column with low coverage still works, on the documents that have it."
  )
  invisible(anchors_)
}

#' What the store holds: documents run, candidates found, label mix, document length
#' @param .ov List from ner_overview().
#' @return Invisibly the ledger tibble.
ent_report_store <- function(.ov) {
  if (FALSE) .ov <- .ov

  cli::cli_h2("Store ledger")
  clf_say_table(
    .tab = .ov$ledger |>
      dplyr::select(Combo, Docs, Success, NoHit, Timeout, Candidates, DocsWithHit) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer))
  )
  cli::cli_alert_info(
    "Docs must be identical across combos; a shortfall is an unfinished run, not a quiet engine."
  )

  cli::cli_h2("Candidates per document, by label")
  clf_say_table(
    .tab = .ov$labels |>
      dplyr::select(Combo, Label, Candidates, Docs) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer))
  )

  if (!is.null(.ov$lengths)) {
    cli::cli_h2("Document length (characters)")
    clf_say_table(.tab = dplyr::mutate(.ov$lengths, dplyr::across(dplyr::everything(), as.integer)))
  }
  invisible(.ov$ledger)
}

#' Offset round-trip, per engine
#' @param .tab Tibble from ent_check_offsets().
#' @return Invisibly .tab.
ent_report_offsets <- function(.tab) {
  if (FALSE) .tab <- ent_check_offsets(.lP$Store$NerDB, .lP$Sample$Text, .n = 2000L)

  cli::cli_h2("Offset round-trip")
  clf_say_table(
    .tab = .tab |> dplyr::mutate(OkShare = clf_pct(.data$OkShare, .digits = 2L))
  )
  bad_ <- .tab |> dplyr::filter(.data$OkShare < 1)
  if (nrow(bad_) == 0L) {
    cli::cli_alert_success("Every sampled span rehydrates from the canonical text.")
  } else {
    cli::cli_alert_danger(
      "Offsets do not index the canonical text for {nrow(bad_)} combo{?s}. Nothing downstream is \\
       meaningful until this is one: the adjudicator would be reading the wrong characters."
    )
  }
  invisible(.tab)
}

#' Cross-engine agreement and its direction
#' @param .al List from ner_alignment().
#' @param .n Integer. Most-complementary pairs to print.
#' @return Invisibly the containment tibble.
ent_report_agreement <- function(.al, .n = 12L) {
  if (FALSE) {
    .al <- .al
    .n  <- 12L
  }

  cli::cli_h2("Consensus across architecture families")
  clf_say_table(
    .tab = ent_family_consensus(.mentions = .al$mentions) |>
      dplyr::mutate(ShareOfMentions = clf_pct(.data$ShareOfMentions))
  )
  cli::cli_alert_info(
    "This is the agreement number to use. Families are spaCy, LexNLP and the ported paper rules, \\
     so each vote is cast by a method sharing nothing with the others."
  )

  cli::cli_h2("Consensus by engine count -- for contrast only")
  clf_say_table(
    .tab = .al$consensus |>
      dplyr::select(Label, NCombos, NMentions, CombosEligible, ShareOfMentions) |>
      dplyr::mutate(
        dplyr::across(c(NCombos, NMentions, CombosEligible), as.integer),
        ShareOfMentions = clf_pct(.data$ShareOfMentions)
      )
  )
  cli::cli_alert_info(
    "Four of the combos are spaCy, so a high engine count here largely records spaCy agreeing \\
     with itself. Where this table looks more reassuring than the one above, that is the reason."
  )

  cont_ <- ent_containment(.pairwise = .al$pairwise)
  cli::cli_h2("Containment: the {(.n)} most complementary engine pairs")
  clf_say_table(
    .tab = cont_ |>
      head(.n) |>
      dplyr::mutate(dplyr::across(c(InA, InB, Jaccard), \(.x) clf_pct(.x))) |>
      dplyr::select(Label, ComboA, ComboB, Both, InA, InB, Jaccard)
  )
  cli::cli_alert_info(
    "InA near 100% with InB well below it means A is a subset of B and adds nothing. Pairs low in \\
     both directions are where 04B must sample."
  )
  invisible(cont_)
}

#' Yield per contract type for one reference engine
#' @param .prof List from ner_profile_by_class().
#' @param .ref_combo Character. The engine the fingerprint is printed for.
#' @return Invisibly the fingerprint tibble.
ent_report_yield <- function(.prof, .ref_combo) {
  if (FALSE) {
    .prof      <- .prof_detailed
    .ref_combo <- .lP$Params$RefCombo
  }

  cli::cli_h2("Label coverage of the classified sample")
  clf_say_table(
    .tab = .prof$coverage |>
      dplyr::mutate(
        dplyr::across(c(NRunDocs, NClassed), as.integer),
        PctClassed = clf_pct(.data$PctClassed)
      )
  )

  cli::cli_h2("Candidates per document by class ({(.ref_combo)})")
  clf_say_table(
    .tab = .prof$fingerprint |>
      dplyr::mutate(
        NDocs = as.integer(.data$NDocs),
        dplyr::across(dplyr::where(is.double), \(.x) round(.x, 2))
      )
  )
  cli::cli_alert_info(
    "Read this as yield, not accuracy: a high count is either better recall or looser matching, \\
     and the two are not separable until 04B."
  )
  invisible(.prof$fingerprint)
}

#' Length-normalised yield per contract type
#' @param .tab Tibble from ent_yield_by_class().
#' @return Invisibly .tab.
ent_report_yield_norm <- function(.tab) {
  if (FALSE) .tab <- tab_yield_norm

  cli::cli_h2("Candidates per 1,000 characters, by class")
  clf_say_table(
    .tab = .tab |> dplyr::mutate(dplyr::across(dplyr::where(is.double), \(.x) round(.x, 2)))
  )
  cli::cli_alert_info(
    "Compare against the per-document table: a class that leads there and not here was leading on \\
     length. MedianChars is the column that explains the difference."
  )
  invisible(.tab)
}

#' The longest documents and their share of the evidence
#' @param .tab Tibble from ent_length_outliers().
#' @return Invisibly .tab.
ent_report_outliers <- function(.tab) {
  if (FALSE) .tab <- tab_outliers

  cli::cli_h2("Longest documents in the sample")
  clf_say_table(
    .tab = .tab |>
      dplyr::mutate(
        PctOfCorpusChars = clf_pct(.data$PctOfCorpusChars / 100, .digits = 2L),
        PctOfCandidates  = clf_pct(.data$PctOfCandidates / 100, .digits = 2L)
      )
  )
  cli::cli_alert_info(
    "A single document holding a percent or more of either column carries that much weight in \\
     every pooled statistic here, the positional histogram included."
  )
  invisible(.tab)
}

#' Measured extraction time and what it implies for the corpus
#' @param .tab Tibble from ent_timing_read().
#' @param .corpus_docs Integer. Documents in the full corpus, for the note.
#' @return Invisibly .tab.
ent_report_timing <- function(.tab, .corpus_docs) {
  if (FALSE) {
    .tab         <- tab_timing
    .corpus_docs <- .lP$Params$CorpusDocs
  }

  cli::cli_h2("Measured extraction time")
  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("No measurements yet -- nothing has been extracted in this store.")
    return(invisible(.tab))
  }
  clf_say_table(
    .tab = .tab |>
      dplyr::mutate(
        Seconds     = round(.data$Seconds, 1),
        DocsPerSec  = round(.data$DocsPerSec, 1),
        CorpusHours = round(.data$CorpusHours, 1)
      ) |>
      dplyr::select(Combo, MaxChars, Docs, Seconds, DocsPerSec, CorpusHours)
  )
  cli::cli_alert_info(
    "CorpusHours projects this rate onto {(.corpus_docs)} documents. It is a fair projection for \\
     a capped pass, where every document costs the same, and an optimistic one for an uncapped \\
     pass, where cost follows length and the corpus tail is longer than this sample's."
  )
  invisible(.tab)
}

#' What a head window keeps, and whether truncating equals filtering
#' @param .tab_cap One-row tibble from ent_head_cap().
#' @param .tab_cov Tibble from ent_head_coverage().
#' @param .tab_cmp Tibble from ent_head_vs_full(), or NULL before the head store exists.
#' @return Invisibly the coverage tibble.
ent_report_head <- function(.tab_cap, .tab_cov, .tab_cmp = NULL) {
  if (FALSE) {
    .tab_cap <- tab_cap
    .tab_cov <- tab_head_cov
    .tab_cmp <- tab_head_cmp
  }

  cli::cli_h2("The head window")
  clf_say_table(
    .tab = .tab_cap |> dplyr::mutate(PctDocsShorter = clf_pct(.data$PctDocsShorter))
  )
  cli::cli_alert_info(
    "Cap is the character budget under which the median document contributes {(.tab_cap$NWords)} \\
     words. P10 and P90 show how far the conversion moves across the sample."
  )

  cli::cli_h2("Share of full-text candidates inside the window")
  clf_say_table(
    .tab = .tab_cov |> dplyr::mutate(PctHead = clf_pct(.data$PctHead))
  )
  cli::cli_alert_info(
    "This is what truncation costs in recall, measured without extracting anything. A label whose \\
     share is far below the others cannot be built from a head window at any speed."
  )

  if (!is.null(.tab_cmp)) {
    cli::cli_h2("Truncated pass against the full pass filtered to the same window")
    clf_say_table(
      .tab = .tab_cmp |> dplyr::mutate(Ratio = round(.data$Ratio, 3))
    )
    cli::cli_alert_info(
      "A ratio near 1 means truncation is a pure saving. Below 1 means the engines find less \\
       inside the window when the rest of the document is absent, which is a cost no offset \\
       filter would have revealed."
    )
  }
  invisible(.tab_cov)
}

#' Every 04A report block, in order
#'
#' The single block to copy out when the extraction needs checking.
#'
#' @param .tab_sample Tibble from ent_build_sample().
#' @param .ov List from ner_overview().
#' @param .al List from ner_alignment().
#' @param .tab_offsets Tibble from ent_check_offsets().
#' @param .prof List from ner_profile_by_class() on ClassDetailed.
#' @param .ref_combo Character. Reference engine for the fingerprint.
#' @param .tab_norm Tibble from ent_yield_by_class().
#' @param .tab_outliers Tibble from ent_length_outliers().
#' @param .tab_timing Tibble from ent_timing_read().
#' @param .corpus_docs Integer. Documents in the full corpus.
#' @return Invisibly NULL.
ent_report_all <- function(.tab_sample, .ov, .al, .tab_offsets, .prof, .ref_combo,
                           .tab_norm, .tab_outliers, .tab_timing, .corpus_docs) {
  if (FALSE) {
    .tab_sample   <- tab_sample
    .ov           <- .ov
    .al           <- .al
    .tab_offsets  <- tab_offsets
    .prof         <- .prof_detailed
    .ref_combo    <- .lP$Params$RefCombo
    .tab_norm     <- tab_yield_norm
    .tab_outliers <- tab_outliers
    .tab_timing   <- tab_timing
    .corpus_docs  <- .lP$Params$CorpusDocs
  }

  ent_report_sample(.tab = .tab_sample)
  ent_report_store(.ov = .ov)
  ent_report_offsets(.tab = .tab_offsets)
  ent_report_agreement(.al = .al, .n = 12L)
  ent_report_yield(.prof = .prof, .ref_combo = .ref_combo)
  ent_report_yield_norm(.tab = .tab_norm)
  ent_report_outliers(.tab = .tab_outliers)
  ent_report_timing(.tab = .tab_timing, .corpus_docs = .corpus_docs)
  invisible(NULL)
}
