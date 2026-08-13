# 04A-EntityExtract: freeze the extraction inputs, run every engine, describe the yield ---------------------------------
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
# precision, and the two are indistinguishable until something external exists to score against. So
# this document answers "what is there and where do the engines differ", and leaves "which engine is
# right" to 04B, which scores against facts EDGAR already recorded.
#
# THE ONE DECISION THAT CANNOT BE UNDONE LATER
# Offsets are integers into a specific string. Every candidate in the store and every rehydrated
# span indexes THAT string. If two scripts disagree about what a document's text is, every offset in
# the project is quietly wrong and nothing errors. 04A therefore writes sample_text.parquet and is
# the only script permitted to define it; everything downstream reads that file and never
# reconstructs the text.
#
# TWO STORES, NOT ONE
# The ledger records that a document was processed by an engine; it does not record whether the text
# was truncated first. A single store therefore cannot hold both a full-text and a truncated
# extraction -- the second would be skipped as already done. 04A builds two: the full-text store,
# which is the substantive one, and a head-window store used only to measure what truncation costs
# in time and buys in throughput. Their manifests are separate and neither can be mistaken for the
# other.
#
# WHAT IS NOT HERE
# Engine and rule scoring against the EDGAR anchors is 04B. Field resolution -- parties, contract
# dates, party locations, contract value -- is 04C. The corpus pass is 04D. The reusable engine seam
# and the store itself (ner_*) live in _Commons/_NER.R and are shared with 04D; everything specific
# to THIS sample, including the descriptive layer in section 7, carries the ent_ prefix.
#
# WHY SECTION 7 IS HERE AND NOT IN _Commons/_NER.R
# The overview, agreement and per-class profiling functions used to sit in the shared file. Nothing
# else ever called them. A shared file holding one script's analysis makes that script's concerns
# look like everyone's, and it gets sourced into every entity document for no reason. The engine
# seam is genuinely shared; describing what the seam produced is 04A's own job, so it lives with
# 04A under 04A's prefix.
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


# 0. Vocabulary ------------------------------------------------------------------------------------------------------
# Registered at source time, exactly as 03A registers the three classification taxonomies, so that
# every figure in this document orders and labels entity types and engines identically without any
# chunk restating the order. plot_factor() aborts on an unregistered value, which is the behaviour
# wanted: a dropped category produces a figure that looks entirely normal while omitting a row.
#
# Two keys, and they behave differently.
#
# Label is a closed set of five and orders by how much of the paper rests on it: parties first,
# then geography, then dates, then value, then the redaction indicators that are not an entity type
# at all but travel in the same coordinate system.
#
# Combo is nine engine:model tokens and needs explicit colours, because the categorical palette caps
# at eight. The short labels matter more here than anywhere else in the project: en_core_web_trf is
# twenty-one characters and a legend built from the full tokens consumes most of a 7.5-inch canvas.
# Colours are assigned by FAMILY rather than by position -- the three spaCy CNN models share a blue
# ramp, the transformer takes the darkest blue, LexNLP takes the warm accent, and the four ported
# paper extractors take neutral tones -- so a reader can see at a glance which marks belong to one
# implementation family and which are genuinely independent evidence.

.ent_labels <- c("ORG", "GPE", "DATE", "MONEY", "REDACT")

.ent_combos <- c(
  "spacy:en_core_web_sm",
  "spacy:en_core_web_md",
  "spacy:en_core_web_lg",
  "spacy:en_core_web_trf",
  "lexnlp",
  "paper:dateregex-v1",
  "paper:gazetteer-v1",
  "paper:redaction-v1",
  "paper:moneyregex-v4"
)

.ent_combos_short <- c(
  "spacy sm", "spacy md", "spacy lg", "spacy trf",
  "lexnlp",
  "dateregex", "gazetteer", "redaction", "moneyregex"
)

# WHICH ENGINES ACTUALLY HAVE TO BE TOLD APART is a narrower question than it looks, and the answer
# is what this palette is built on. The four ported paper extractors are label SPECIALISTS -- each
# emits exactly one label -- so no two of them ever appear in the same panel of any figure here.
# They therefore need to be separable from the blues and from LexNLP, and not from each other.
# Only spaCy and LexNLP are multi-label, and those are the two that genuinely compete.
#
# So: one blue ramp for spaCy, ordered by capacity; the house ochre for LexNLP, which is then the
# only warm mark anywhere and cannot be confused with anything; a grey ramp for the four paper
# rules. Two earlier versions of this got it wrong in opposite directions -- the first put redaction
# and dateregex on blues, so they read as further spaCy models; the second put three paper rules on
# warm browns, which collided with LexNLP in the DATE panel, the one place dateregex has to be read
# against it.
.ent_combo_colours <- c(
  "#bfd7ed", # spacy sm    -- lightest of the CNN ramp
  "#60a3d9", # spacy md
  "#0074b7", # spacy lg
  "#002147", # spacy trf   -- darkest, and the only transformer
  "#B7791F", # lexnlp      -- the one warm mark in the figure
  "#4A4A4A", # dateregex   -- the paper rules, in greys they never need separating within
  "#6E6E6E", # gazetteer
  "#3A3A3A", # redaction    (alone in its panel, so it takes the darkest for legibility)
  "#8E8E8E"  # moneyregex
)

plot_register_levels(
  .key     = "Label",
  .levels  = .ent_labels,
  .short   = NULL,                        # already short; the full name IS the label
  .colours = plot_pal_cat(length(.ent_labels))
)

plot_register_levels(
  .key     = "Combo",
  .levels  = .ent_combos,
  .short   = .ent_combos_short,
  .colours = .ent_combo_colours           # explicit: nine levels, and the palette caps at eight
)


# 1. Sample construction ---------------------------------------------------------------------------------------------
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
    cli::cli_alert_warning("{length(gap_)} anchor column{?s} unusable: {gap_}")
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


# 2. The run manifest ------------------------------------------------------------------------------------------------
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

  con_ <- ner_db_connect()
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

#' Classify combination tokens against the set a run declares
#'
#' An extractor's model tag ends in a version -- gazetteer-v1, moneyregex-v4 -- and bumping it is
#' how a revised rule set is kept from being confused with the one it replaced. ner_run() honours
#' that correctly: a new version is a new combination, so it is extracted rather than skipped. What
#' it does not do is remove the old one, because the ledger has no way to know a version was
#' retired rather than merely not asked for on this pass.
#'
#' The consequence showed up in the head-window store, which accumulated four money-regex versions
#' across the run in which the pattern set was being developed. Nothing downstream read them, so no
#' published number was wrong, but the truncation comparison reported three engines that do not
#' exist and the timing table ranked one of them on throughput.
#'
#' SUPERSEDED IS A NARROWER TEST THAN UNDECLARED, deliberately. A combination is superseded when the
#' declared set holds another version of the SAME engine and model stem. An engine simply left out
#' of this run is "undeclared" and gets no verdict beyond that, because temporarily running a subset
#' is ordinary and treating it as retirement would be a trap far worse than the one this fixes.
#'
#' Kept separate from its two callers so that the store and the timing log apply one rule rather
#' than two implementations of it.
#'
#' @param .combos Character. Combination tokens to classify.
#' @param .run Character. The combination tokens this run declares.
#' @return Tibble: Combo, Stem, Declared, Verdict -- one row per element of .combos.
ent_combo_verdict <- function(.combos, .run) {
  if (FALSE) {
    .combos <- c("paper:moneyregex-v1", "paper:moneyregex-v4", "lexnlp")
    .run    <- lst_run_args$.run
  }

  # The stem is the token with any trailing -v<N> removed, which is what makes two versions of one
  # extractor comparable and keeps two genuinely different extractors apart.
  stem_ <- function(.x) sub("-v[0-9]+$", "", .x)

  tibble::tibble(Combo = sort(unique(.combos))) |>
    dplyr::mutate(
      Stem     = stem_(.data$Combo),
      Declared = .data$Combo %in% .run,
      StemHeld = .data$Stem %in% stem_(.run),
      Verdict  = dplyr::case_when(
        .data$Declared                   ~ "declared",
        !.data$Declared & .data$StemHeld ~ "superseded",
        TRUE                             ~ "undeclared"
      )
    ) |>
    dplyr::select(Combo, Stem, Declared, Verdict)
}

#' Which combinations a store holds, and how each stands against the declared run
#'
#' @param .db_path Path to the store to inspect.
#' @param .run Character. The combination tokens this run declares.
#' @return Tibble from ent_combo_verdict(), one row per combination present in the store.
ent_superseded <- function(.db_path, .run) {
  if (FALSE) {
    .db_path <- .lP$Store$NerDBHead
    .run     <- lst_run_args$.run
  }
  if (!fs::file_exists(.db_path)) {
    return(tibble::tibble(Combo = character(0), Stem = character(0),
                          Declared = logical(0), Verdict = character(0)))
  }

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
  have_ <- DBI::dbGetQuery(con_, paste0(
    "SELECT DISTINCT CASE WHEN Engine = Model THEN Engine ",
    "ELSE Engine || ':' || Model END AS Combo FROM runs"
  ))$Combo

  ent_combo_verdict(.combos = have_, .run = .run)
}

#' Report what a store holds against what this run declares, and clear what is superseded
#'
#' Idempotent, and that is the point: on the first render it removes the superseded versions and
#' says so; on every later one it finds nothing and says that instead. Leaving the cleanup as a
#' command someone remembers to run is how the store came to hold four money regexes in the first
#' place.
#'
#' Only "superseded" is cleared. "undeclared" is reported and kept -- see ent_superseded().
#'
#' @param .db_path Path to the store.
#' @param .run Character. The combination tokens this run declares.
#' @param .clear Logical. FALSE reports without touching the store, which is what a first look at an
#'   unfamiliar store wants.
#' @return Invisibly, the tibble from ent_superseded() as it stood BEFORE any clearing.
ent_report_superseded <- function(.db_path, .run, .clear = TRUE) {
  if (FALSE) {
    .db_path <- .lP$Store$NerDBHead
    .run     <- lst_run_args$.run
    .clear   <- TRUE
  }

  tab_ <- ent_superseded(.db_path = .db_path, .run = .run)
  cli::cli_h2("Store contents against the declared run")
  if (nrow(tab_) == 0L) {
    cli::cli_alert_info("Store does not exist yet; nothing to reconcile.")
    return(invisible(tab_))
  }

  tbl_say(.tab = dplyr::count(tab_, .data$Verdict, name = "Combos"))

  old_ <- tab_$Combo[tab_$Verdict == "superseded"]
  und_ <- tab_$Combo[tab_$Verdict == "undeclared"]

  if (length(und_) > 0L) {
    cli::cli_alert_info(paste0("Held but not declared, and left alone: ",
                               "{paste(und_, collapse = ', ')}."))
  }
  if (length(old_) == 0L) {
    cli::cli_alert_success("No superseded engine versions in this store.")
    return(invisible(tab_))
  }

  cli::cli_alert_warning("Superseded: {paste(old_, collapse = ', ')}.")
  if (!.clear) {
    cli::cli_alert_info("Reporting only; pass {.arg .clear} to remove them.")
    return(invisible(tab_))
  }
  ner_db_clear(.db_path = .db_path, .run = old_, .doc_ids = NULL, .status = NULL, .quiet = FALSE)
  cli::cli_alert_success("Cleared {length(old_)} superseded combination{?s}.")
  invisible(tab_)
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
          paste0("held but not asked for: ", paste(gone_, collapse = ", ")),
        .data$Kind == "inventory" ~ "unchanged",
        .data$Same  ~ "",
        TRUE        ~ "store is stale -- move it aside"
      ),
      # The inventory value is nine pipe-joined tokens running past two hundred characters, and the
      # console formatter pads every column to its widest cell -- so one row was rendering the whole
      # table four hundred columns wide, well past anything that survives being pasted into a
      # discussion. The count goes in the cell and the names go in the Note, where only the tokens
      # that MOVED are named. Nothing is hidden: an unchanged inventory has nothing to report, and a
      # changed one reports the difference rather than both full lists.
      dplyr::across(c(Stored, Current), \(.x) dplyr::if_else(
        .data$Kind == "inventory",
        paste0(lengths(lapply(.x, split_)), " combos"),
        .x
      ))
    ) |>
    dplyr::select(Field, Kind, Stored, Current, Match, Note)

  # An inventory-only change is routine, so record it and stop flagging it next time.
  if (all(out_$Match[out_$Kind == "fingerprint"]) && !identical(was_, now_)) {
    arrow::write_parquet(.manifest, .path_manifest)
  }
  out_
}


# 3. Offset integrity ------------------------------------------------------------------------------------------------
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

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
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


# 4. Agreement, yield and outliers -----------------------------------------------------------------------------------
# Jaccard says how far two engines overlap but not which way. Containment separates a model that
# genuinely adds mentions from one that merely restates a smaller model's output, which is the only
# engine question answerable before a gold standard exists.

#' Directional overlap between engine pairs, per label
#'
#' Derived from ent_alignment()$pairwise rather than recomputed, so the mention clustering behind
#' both is identical by construction. InA is the share of A's mentions that B also found; InB the
#' reverse. A pair with InA near one and InB well below it means A's output is a subset of B's, so
#' A carries no independent information and the choice between them is free. A pair low in both
#' directions is complementary, and is where 04B has to concentrate its sampling.
#'
#' @param .pairwise Tibble from ent_alignment()$pairwise.
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
#' @param .mentions Tibble from ent_alignment()$mentions; its Combos column is the combo set.
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
  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
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

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
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


# 5. The cost of truncation ------------------------------------------------------------------------------------------
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

  con_ <- ner_db_connect()
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
#' The log is append-only, so it accumulates every combination ever measured -- including versions
#' of an extractor since superseded. Those are filtered on READ rather than deleted, because the log
#' is a record of what was measured and when, and a measurement that happened did happen. What must
#' not survive is a retired engine appearing in a throughput ranking as though it were a candidate
#' for the corpus pass.
#'
#' The filter is derived from .run through the same test the store reconciliation uses, NOT from a
#' list of tokens computed once. A one-off exclusion would clear the store on its first render and
#' then let the log rows reappear on the second, because by then nothing would be left in the store
#' to notice them by.
#'
#' @param .path_log Timing-log parquet.
#' @param .corpus_docs Integer. Documents in the full corpus.
#' @param .run Character or NULL. The combination tokens this run declares. Rows for superseded
#'   versions of a declared extractor are dropped from the report; the log keeps them. NULL reports
#'   everything the log holds.
#' @return Tibble: Combo, MaxChars, Docs, Seconds, DocsPerSec, CorpusHours, MeasuredAt.
ent_timing_read <- function(.path_log, .corpus_docs, .run = NULL) {
  if (FALSE) {
    .path_log    <- .lP$Store$TimingLog
    .corpus_docs <- .lP$Params$CorpusDocs
    .run         <- lst_run_args$.run
  }

  if (!fs::file_exists(.path_log)) {
    return(tibble::tibble(
      Combo = character(0), MaxChars = integer(0), Docs = integer(0), Seconds = numeric(0),
      DocsPerSec = numeric(0), CorpusHours = numeric(0), MeasuredAt = as.POSIXct(character(0))
    ))
  }

  log_ <- arrow::read_parquet(.path_log)
  if (!is.null(.run)) {
    stale_ <- ent_combo_verdict(.combos = log_$Combo, .run = .run)
    log_   <- dplyr::filter(
      log_, !.data$Combo %in% stale_$Combo[stale_$Verdict == "superseded"]
    )
  }

  log_ |>
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

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
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
#' near one means truncation is a pure saving and 04D may take it. A ratio well below one means
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

  con_ <- ner_db_connect(.db_path = .db_head, .read_only = TRUE)
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


# 6. Report ----------------------------------------------------------------------------------------------------------
# Every block prints through 03A's fixed-width formatter and returns its tibble invisibly, so the
# console output of this document is one consistent, copy-pasteable stream and every number stays
# available afterwards.

#' Intake and anchor coverage for the extraction sample
#' @param .tab Tibble from ent_build_sample().
#' @return Invisibly the anchor-coverage tibble.
ent_report_sample <- function(.tab) {
  if (FALSE) .tab <- tab_sample

  cli::cli_h2("Extraction sample")
  tbl_say(
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
  tbl_say(
    .tab = anchors_ |> dplyr::mutate(PctNonNA = tbl_pct(.data$PctNonNA))
  )
  cli::cli_alert_info(
    "MISSING means the column never arrived and the check it supports cannot run in 04B. EMPTY \\
     means it arrived carrying nothing, which is a join failure rather than a source gap. A found \\
     column with low coverage still works, on the documents that have it."
  )
  invisible(anchors_)
}

#' What the store holds: documents run, candidates found, label mix, document length
#' @param .ov List from ent_overview().
#' @return Invisibly the ledger tibble.
ent_report_store <- function(.ov) {
  if (FALSE) .ov <- .ov

  cli::cli_h2("Store ledger")
  tbl_say(
    .tab = .ov$ledger |>
      dplyr::select(Combo, Docs, Success, NoHit, Timeout, Candidates, DocsWithHit) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer))
  )
  cli::cli_alert_info(
    "Docs must be identical across combos; a shortfall is an unfinished run, not a quiet engine."
  )

  cli::cli_h2("Candidates per document, by label")
  tbl_say(
    .tab = .ov$labels |>
      dplyr::select(Combo, Label, Candidates, Docs) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), as.integer))
  )

  if (!is.null(.ov$lengths)) {
    cli::cli_h2("Document length (characters)")
    tbl_say(.tab = dplyr::mutate(.ov$lengths, dplyr::across(dplyr::everything(), as.integer)))
  }
  invisible(.ov$ledger)
}

#' Offset round-trip, per engine
#' @param .tab Tibble from ent_check_offsets().
#' @return Invisibly .tab.
ent_report_offsets <- function(.tab) {
  if (FALSE) .tab <- ent_check_offsets(.lP$Store$NerDB, .lP$Sample$Text, .n = 2000L)

  cli::cli_h2("Offset round-trip")
  tbl_say(
    .tab = .tab |> dplyr::mutate(OkShare = tbl_pct(.data$OkShare, .digits = 2L))
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
#' @param .al List from ent_alignment().
#' @param .n Integer. Most-complementary pairs to print.
#' @return Invisibly the containment tibble.
ent_report_agreement <- function(.al, .n = 12L) {
  if (FALSE) {
    .al <- .al
    .n  <- 12L
  }

  cli::cli_h2("Consensus across architecture families")
  tbl_say(
    .tab = ent_family_consensus(.mentions = .al$mentions) |>
      dplyr::mutate(ShareOfMentions = tbl_pct(.data$ShareOfMentions))
  )
  cli::cli_alert_info(
    "This is the agreement number to use. Families are spaCy, LexNLP and the ported paper rules, \\
     so each vote is cast by a method sharing nothing with the others."
  )

  cli::cli_h2("Consensus by engine count -- for contrast only")
  tbl_say(
    .tab = .al$consensus |>
      dplyr::select(Label, NCombos, NMentions, CombosEligible, ShareOfMentions) |>
      dplyr::mutate(
        dplyr::across(c(NCombos, NMentions, CombosEligible), as.integer),
        ShareOfMentions = tbl_pct(.data$ShareOfMentions)
      )
  )
  cli::cli_alert_info(
    "Four of the combos are spaCy, so a high engine count here largely records spaCy agreeing \\
     with itself. Where this table looks more reassuring than the one above, that is the reason."
  )

  cont_ <- ent_containment(.pairwise = .al$pairwise)
  cli::cli_h2("Containment: the {(.n)} most complementary engine pairs")
  tbl_say(
    .tab = cont_ |>
      head(.n) |>
      dplyr::mutate(dplyr::across(c(InA, InB, Jaccard), \(.x) tbl_pct(.x))) |>
      dplyr::select(Label, ComboA, ComboB, Both, InA, InB, Jaccard)
  )
  cli::cli_alert_info(
    "InA near 100% with InB well below it means A is a subset of B and adds nothing. Pairs low in \\
     both directions are where 04B must sample."
  )
  invisible(cont_)
}

#' Yield per contract type for one reference engine
#' @param .prof List from ent_profile_by_class().
#' @param .ref_combo Character. The engine the fingerprint is printed for.
#' @return Invisibly the fingerprint tibble.
ent_report_yield <- function(.prof, .ref_combo) {
  if (FALSE) {
    .prof      <- .prof_detailed
    .ref_combo <- .lP$Params$RefCombo
  }

  cli::cli_h2("Label coverage of the classified sample")
  tbl_say(
    .tab = .prof$coverage |>
      dplyr::mutate(
        dplyr::across(c(NRunDocs, NClassed), as.integer),
        PctClassed = tbl_pct(.data$PctClassed)
      )
  )

  cli::cli_h2("Candidates per document by class ({(.ref_combo)})")
  tbl_say(
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
  tbl_say(
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
  tbl_say(
    .tab = .tab |>
      dplyr::mutate(
        PctOfCorpusChars = tbl_pct(.data$PctOfCorpusChars / 100, .digits = 2L),
        PctOfCandidates  = tbl_pct(.data$PctOfCandidates / 100, .digits = 2L)
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
  tbl_say(
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
  tbl_say(
    .tab = .tab_cap |> dplyr::mutate(PctDocsShorter = tbl_pct(.data$PctDocsShorter))
  )
  cli::cli_alert_info(
    "Cap is the character budget under which the median document contributes {(.tab_cap$NWords)} \\
     words. P10 and P90 show how far the conversion moves across the sample."
  )

  cli::cli_h2("Share of full-text candidates inside the window")
  tbl_say(
    .tab = .tab_cov |> dplyr::mutate(PctHead = tbl_pct(.data$PctHead))
  )
  cli::cli_alert_info(
    "This is what truncation costs in recall, measured without extracting anything. A label whose \\
     share is far below the others cannot be built from a head window at any speed."
  )

  if (!is.null(.tab_cmp)) {
    cli::cli_h2("Truncated pass against the full pass filtered to the same window")
    tbl_say(
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
#' @param .ov List from ent_overview().
#' @param .al List from ent_alignment().
#' @param .tab_offsets Tibble from ent_check_offsets().
#' @param .prof List from ent_profile_by_class() on ClassDetailed.
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


# 7. Describing the store --------------------------------------------------------------------------------------------
# Everything that reads the finished candidate store to say what is in it. These functions used to
# live in _Commons/_NER.R; nothing but this document ever called them, and the dev blocks below
# still carry the paths they were written against.
#
# All three are read-only DuckDB scans that aggregate on the database side and return small tibbles.
# The store holds tens of millions of candidate rows and none of them crosses into R.

#' Read-only summaries of the candidate store
#'
#' The first look at a completed extraction, and deliberately four separate tibbles rather than one
#' joined table: they have different grains and joining them would force a choice about which grain
#' wins before anything is known.
#'
#' $ledger is per combination from the runs table: documents seen, split by status, and the share
#' with a hit. $labels is per combination and label: candidate and distinct-document counts.
#' $lengths and $positions need .inputs, because both are computed against document length, and the
#' store does not hold the text.
#'
#' $positions is the substantive one. It is the midpoint of each candidate over the document length
#' in code points, so a histogram of it answers where in a contract each entity type is found. That
#' is the evidence for or against describing extracted places as party locations, and it is
#' available without any labels at all.
#'
#' CAVEAT UNDER TRUNCATION. With .max_chars set, documents longer than the cap carry candidates only
#' in their first cap characters while the denominator is still the full length, so their right-hand
#' bins under-fill. This document runs the full-text store, where the caveat does not bite.
#'
#' Run after ner_run() has finished. The store is opened read-only, and DuckDB is single-writer, so
#' a live writing connection elsewhere blocks it.
#'
#' @param .db_path Path to the DuckDB candidate store.
#' @param .inputs Parquet path(s) holding the canonical text, or NULL to skip the length-based
#'   pieces. Pass the same path given to ner_run().
#' @param .id_col Document identifier column in the input.
#' @param .text_col Text column the offsets index.
#' @param .quiet Suppress progress messages.
#' @return A list of four tibbles: ledger, labels, lengths, positions.
ent_overview <- function(.db_path,
                         .inputs = NULL,
                         .id_col = "DocID",
                         .text_col = "TextRaw",
                         .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Cache$NerDB
    .inputs <- .lP$Input$SampleContracts
    .id_col <- "DocID"
    .text_col <- "TextRaw"
    .quiet <- FALSE
  }

  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  add_combo_ <- function(.df) {
    dplyr::mutate(.df, Combo = dplyr::if_else(Engine == Model, Engine, paste0(Engine, ":", Model)))
  }

  runs_ <- dplyr::tbl(con_, "runs")
  cand_ <- dplyr::tbl(con_, "candidates")

  # Ledger: docs run per combo, split by Status, with the hit rate.
  ledger_ <- runs_ |>
    dplyr::group_by(Engine, Model) |>
    dplyr::summarise(
      Docs    = dplyr::n(),
      Success = sum(Status == "success", na.rm = TRUE),
      NoHit   = sum(Status == "nohit",   na.rm = TRUE),
      Timeout = sum(Status == "timeout", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect()

  cand_combo_ <- cand_ |>
    dplyr::group_by(Engine, Model) |>
    dplyr::summarise(
      Candidates  = dplyr::n(),
      DocsWithHit = dplyr::n_distinct(DocID),
      .groups = "drop"
    ) |>
    dplyr::collect()

  ledger_out_ <- ledger_ |>
    dplyr::left_join(cand_combo_, by = c("Engine", "Model")) |>
    dplyr::mutate(
      Candidates    = dplyr::coalesce(Candidates, 0L),
      DocsWithHit   = dplyr::coalesce(DocsWithHit, 0L),
      HitRate       = dplyr::if_else(Docs > 0L, Success / Docs, NA_real_),
      CandPerHitDoc = dplyr::if_else(DocsWithHit > 0L, Candidates / DocsWithHit, NA_real_)
    ) |>
    add_combo_() |>
    dplyr::relocate(Combo) |>
    dplyr::arrange(Engine, Model)

  # Label mix per combo.
  labels_out_ <- cand_ |>
    dplyr::group_by(Engine, Model, Label) |>
    dplyr::summarise(Candidates = dplyr::n(), Docs = dplyr::n_distinct(DocID), .groups = "drop") |>
    dplyr::collect() |>
    add_combo_() |>
    dplyr::relocate(Combo) |>
    dplyr::arrange(Engine, Model, dplyr::desc(Candidates))

  lengths_out_ <- NULL
  positions_out_ <- NULL

  if (!is.null(.inputs)) {
    files_ <- ner_input_files(.inputs)
    files_sql_ <- paste0("'", files_, "'", collapse = ", ")
    DBI::dbExecute(con_, paste0(
      "CREATE OR REPLACE TEMP VIEW ov_lengths AS ",
      "SELECT \"", .id_col, "\" AS DocID, length(\"", .text_col, "\") AS DocLen ",
      "FROM read_parquet([", files_sql_, "])"
    ))
    lens_ <- dplyr::tbl(con_, "ov_lengths")

    lengths_out_ <- lens_ |>
      dplyr::collect() |>
      dplyr::summarise(
        NDocs     = dplyr::n(),
        MinLen    = min(DocLen),
        MedianLen = stats::median(DocLen),
        MeanLen   = mean(DocLen),
        MaxLen    = max(DocLen)
      )

    positions_out_ <- cand_ |>
      dplyr::inner_join(lens_, by = "DocID") |>
      dplyr::filter(DocLen > 0) |>
      dplyr::transmute(
        Engine, Model, Label,
        Rel = ((Start + Stop) / 2.0) / DocLen
      ) |>
      dplyr::collect() |>
      add_combo_() |>
      dplyr::relocate(Combo)
  }

  if (!.quiet) {
    skip_msg_ <- if (is.null(.inputs)) " (no .inputs -> length/position skipped)" else ""
    cli::cli_alert_success(
      paste0("Overview: {sum(ledger_out_$Candidates)} candidate(s) over ",
             "{nrow(ledger_out_)} combo(s){skip_msg_}.")
    )
  }

  list(
    ledger    = ledger_out_,
    labels    = labels_out_,
    lengths   = lengths_out_,
    positions = positions_out_
  )
}

# Positional histogram: where candidates sit relative to full document length.
# .positions is ent_overview()$positions. Faceted by Label; filled by engine:model
# so you can see whether the engines agree on where a label-type lives (e.g. dates
# clustering at the signature block, parties/ORGs near the top).

#' Cross-engine agreement on merged mentions
#'
#' Answers the one question the absence of labels still permits: where do the engines differ? Two
#' engines whose outputs are nested carry one engine's worth of information and the choice between
#' them is free. Two that disagree are where a measurement has something to work on.
#'
#' Within each document and label, overlapping spans are merged into a mention and the contributing
#' combinations recorded. Agreement is ANY overlap rather than exact match, which is the right
#' notion for a recall scaffold: two engines that both found the same organisation but disagreed
#' about whether the comma belongs have agreed about the organisation.
#'
#' Returns $mentions (one row per merged mention, most-agreed first, with the widest contributing
#' candidate as its representative span), $consensus (per label, how many mentions were found by
#' one, two, ... combinations against how many were eligible) and $pairwise (per label and
#' combination pair, the Jaccard overlap, with every eligible pair listed so a pair that never
#' overlaps appears as a zero rather than as a gap).
#'
#' READ $pairwise BY FAMILY, NOT BY PAIR. Three spaCy models agreeing with each other is one
#' implementation agreeing with itself, and it inflates the apparent corroboration for ORG to a
#' figure the independent families do not support.
#'
#' KNOWN LIMIT. The merge is transitive, so a dense run of adjacent spans can chain into one long
#' mention. It has not mattered at the densities seen here; tighten to exact match or an IoU
#' threshold if it ever does.
#'
#' @param .db_path Path to the DuckDB candidate store.
#' @param .quiet Suppress progress messages.
#' @return A list of three tibbles: mentions, consensus, pairwise.
ent_alignment <- function(.db_path, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Cache$NerDB
    .quiet <- FALSE
  }
  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  if (DBI::dbGetQuery(con_, "SELECT COUNT(*) AS n FROM candidates")$n == 0L) {
    cli::cli_abort("No candidates in the store yet.")
  }

  # 1) Per-candidate, tagged with a MentionID = merged overlap cluster within
  #    (DocID, Label). Gaps-and-islands: a new cluster starts when Start is at/after
  #    the running max end of all PRIOR spans in the ordered group (half-open spans).
  DBI::dbExecute(con_, "
    CREATE TEMP TABLE ner_align AS
    WITH base AS (
      SELECT DocID, Label,
             CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END AS Combo,
             Start, Stop, Span, (Stop - Start) AS Width
      FROM candidates
    ),
    lagged AS (
      SELECT *, MAX(Stop) OVER (PARTITION BY DocID, Label ORDER BY Start, Stop
                  ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS PrevMaxEnd
      FROM base
    ),
    flagged AS (
      SELECT *, CASE WHEN PrevMaxEnd IS NULL OR Start >= PrevMaxEnd THEN 1 ELSE 0 END AS IsNew
      FROM lagged
    ),
    clustered AS (
      SELECT *, SUM(IsNew) OVER (PARTITION BY DocID, Label ORDER BY Start, Stop
                  ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS Cluster
      FROM flagged
    )
    SELECT DocID, Label, Combo, Start, Stop, Span, Width,
           DocID || '::' || Label || '::' || CAST(Cluster AS VARCHAR) AS MentionID
    FROM clustered
  ")

  # 2) One row per mention: bounds, counts, ordered distinct combo set, widest Span.
  DBI::dbExecute(con_, "
    CREATE TEMP TABLE ner_mentions AS
    WITH mc AS (SELECT DISTINCT MentionID, Combo FROM ner_align),
    mc_agg AS (
      SELECT MentionID, COUNT(*) AS NCombos,
             string_agg(Combo, ' | ' ORDER BY Combo) AS Combos
      FROM mc GROUP BY MentionID
    ),
    sp AS (
      SELECT MentionID, any_value(DocID) AS DocID, any_value(Label) AS Label,
             MIN(Start) AS Start, MAX(Stop) AS Stop, COUNT(*) AS NCands,
             arg_max(Span, Width) AS Span
      FROM ner_align GROUP BY MentionID
    )
    SELECT sp.MentionID, sp.DocID, sp.Label, sp.Start, sp.Stop,
           sp.NCands, mc_agg.NCombos, mc_agg.Combos, sp.Span
    FROM sp JOIN mc_agg USING (MentionID)
  ")

  mentions_ <- DBI::dbGetQuery(con_, "
    SELECT DocID, Label, Start, Stop, NCands, NCombos, Combos, Span
    FROM ner_mentions ORDER BY NCombos DESC, DocID, Start
  ") |> tibble::as_tibble()

  # 3) Consensus: mentions by NCombos per label, vs combos eligible for that label.
  consensus_ <- DBI::dbGetQuery(con_, "
    SELECT Label, NCombos, COUNT(*) AS NMentions FROM ner_mentions GROUP BY Label, NCombos
  ") |>
    dplyr::left_join(
      DBI::dbGetQuery(con_, "
        SELECT Label, COUNT(*) AS CombosEligible
        FROM (SELECT DISTINCT Label, Combo FROM ner_align) GROUP BY Label
      "),
      by = "Label"
    ) |>
    dplyr::group_by(Label) |>
    dplyr::mutate(ShareOfMentions = NMentions / sum(NMentions)) |>
    dplyr::ungroup() |>
    dplyr::arrange(Label, NCombos) |>
    tibble::as_tibble()

  # 4) Pairwise: co-occurrence (DB self-join) + per-combo counts; all eligible pairs
  #    assembled in R (tiny) so non-overlapping pairs show Jaccard 0.
  combo_counts_ <- DBI::dbGetQuery(con_, "
    SELECT Label, Combo, COUNT(DISTINCT MentionID) AS Mentions FROM ner_align GROUP BY Label, Combo
  ") |> tibble::as_tibble()
  cooc_ <- DBI::dbGetQuery(con_, "
    WITH m AS (SELECT DISTINCT MentionID, Label, Combo FROM ner_align)
    SELECT a.Label, a.Combo AS ComboA, b.Combo AS ComboB, COUNT(*) AS Both
    FROM m a JOIN m b ON a.MentionID = b.MentionID AND a.Combo < b.Combo
    GROUP BY a.Label, a.Combo, b.Combo
  ") |> tibble::as_tibble()

  pairwise_ <- combo_counts_ |>
    dplyr::select(Label, ComboA = Combo) |>
    dplyr::inner_join(dplyr::select(combo_counts_, Label, ComboB = Combo),
                      by = "Label", relationship = "many-to-many") |>
    dplyr::filter(ComboA < ComboB) |>
    dplyr::left_join(cooc_, by = c("Label", "ComboA", "ComboB")) |>
    dplyr::mutate(Both = dplyr::coalesce(Both, 0L)) |>
    dplyr::left_join(dplyr::rename(combo_counts_, N_A = Mentions), by = c("Label", "ComboA" = "Combo")) |>
    dplyr::left_join(dplyr::rename(combo_counts_, N_B = Mentions), by = c("Label", "ComboB" = "Combo")) |>
    dplyr::mutate(Jaccard = Both / (N_A + N_B - Both)) |>
    dplyr::arrange(Label, dplyr::desc(Jaccard))

  if (!.quiet) {
    multi_ <- mean(mentions_$NCombos >= 2L)
    cli::cli_alert_success(
      "Alignment: {nrow(mentions_)} mention(s); {scales::percent(multi_, accuracy = 0.1)} found by >= 2 combos."
    )
  }

  list(mentions = mentions_, consensus = consensus_, pairwise = pairwise_)
}

# Pairwise agreement heatmap: Jaccard between combos, faceted by Label.

#' What the extraction finds per contract type
#'
#' Joins the candidate store to the hand-assigned classification labels and reports yield per
#' contract type. This is counts, not accuracy, and the hit-rate view carries the conclusion the
#' count view does not: a near-zero cell means the entity type is absent from that contract type in
#' the text itself, so no later adjudication can rescue a variable built on it.
#'
#' NEITHER MEASURE IS A PARTY COUNT, and the pair brackets the truth rather than establishing it.
#' Per document is inflated by length -- seven of the ten longest documents in this sample are
#' credit agreements, which is most of why credit looked entity-rich. Per thousand characters is
#' deflated for a long contract that names three parties a hundred times each. A count of distinct
#' parties needs deduplication, and that is 04C's job.
#'
#' $consensus is the defensible one for the paper where it can be had. It is computed on merged
#' spans and so is engine-agnostic, which avoids counting the same organisation once per engine that
#' found it. It is only produced when .mentions is supplied.
#'
#' @param .db_path Path to the DuckDB candidate store.
#' @param .class_parquet Parquet path(s) carrying at least .id_col and .class_col.
#' @param .class_col Label column to profile by. ClassDetailed by default; ClassBroad and AmendType
#'   are the other registered vocabularies.
#' @param .id_col Document identifier column.
#' @param .run Combination tokens to restrict to, or NULL for all.
#' @param .ref_combo Combination the readable wide fingerprint is printed for. One engine, because a
#'   wide table over nine of them is unreadable.
#' @param .mentions Merged mentions from ent_alignment(), or NULL to skip the consensus view.
#' @param .min_combos Combinations that must agree for a mention to count as high-confidence.
#' @param .quiet Suppress progress messages.
#' @return A list of tibbles: docs, coverage, profile, fingerprint and, when .mentions is given,
#'   consensus.
ent_profile_by_class <- function(.db_path,
                                 .class_parquet,
                                 .class_col = "ClassDetailed",
                                 .id_col = "DocID",
                                 .run = NULL,
                                 .ref_combo = "spacy:en_core_web_trf",
                                 .mentions = NULL,
                                 .min_combos = 2L,
                                 .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Cache$NerDB
    .class_parquet <- .lP$Input$ClassificationSample
    .class_col <- "ClassDetailed"
    .id_col <- "DocID"
    .run <- NULL
    .ref_combo <- "spacy:en_core_web_trf"
    .mentions <- NULL # or .al$mentions from ent_alignment()
    .min_combos <- 2L
    .quiet <- FALSE
  }

  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  add_combo_ <- function(.df) {
    dplyr::mutate(.df, Combo = dplyr::if_else(Engine == Model, Engine, paste0(Engine, ":", Model)))
  }

  # Class map: DocID -> Class. Validate the column exists, fail loudly with the
  # available names if .class_col is wrong (cheap LIMIT 0 schema probe).
  files_ <- ner_input_files(.class_parquet)
  files_sql_ <- paste0("'", files_, "'", collapse = ", ")
  avail_ <- names(DBI::dbGetQuery(con_, paste0(
    "SELECT * FROM read_parquet([", files_sql_, "]) LIMIT 0"
  )))
  if (!.class_col %in% avail_) {
    cli::cli_abort(c(
      "Class column {.val {(.class_col)}} not found in {.arg .class_parquet}.",
      "i" = "Available columns: {paste(avail_, collapse = ', ')}"
    ))
  }
  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TEMP VIEW ner_class AS ",
    "SELECT \"", .id_col, "\" AS DocID, CAST(\"", .class_col, "\" AS VARCHAR) AS Class ",
    "FROM read_parquet([", files_sql_, "]) ",
    "WHERE \"", .class_col, "\" IS NOT NULL"
  ))
  class_ <- dplyr::tbl(con_, "ner_class")
  class_tbl_ <- dplyr::collect(class_) # small (~n docs); reused for the .mentions join

  runs_ <- dplyr::tbl(con_, "runs")
  cand_ <- dplyr::tbl(con_, "candidates")

  # Optional combo filter (restricts both ledger and candidates).
  if (!is.null(.run)) {
    combos_ <- purrr::map(.run, ner_parse_combo) |>
      dplyr::bind_rows() |>
      dplyr::distinct()
    duckdb::duckdb_register(con_, "ner_prof_combos", as.data.frame(combos_))
    on.exit(duckdb::duckdb_unregister(con_, "ner_prof_combos"), add = TRUE, after = FALSE)
    keep_ <- dplyr::tbl(con_, "ner_prof_combos")
    runs_ <- dplyr::semi_join(runs_, keep_, by = c("Engine", "Model"))
    cand_ <- dplyr::semi_join(cand_, keep_, by = c("Engine", "Model"))
  }

  # Per-class processed-doc count (the denominator). One row per (DocID, Class);
  # combos all cover the same doc set, so this is combo-independent.
  docs_ <- runs_ |>
    dplyr::distinct(DocID) |>
    dplyr::inner_join(class_, by = "DocID") |>
    dplyr::group_by(Class) |>
    dplyr::summarise(NDocs = dplyr::n_distinct(DocID), .groups = "drop") |>
    dplyr::collect() |>
    dplyr::arrange(dplyr::desc(NDocs))

  n_run_docs_ <- runs_ |>
    dplyr::summarise(n = dplyr::n_distinct(DocID)) |>
    dplyr::pull(n)
  n_classed_ <- sum(docs_$NDocs)
  coverage_ <- tibble::tibble(
    NRunDocs   = as.integer(n_run_docs_),
    NClassed   = as.integer(n_classed_),
    PctClassed = if (n_run_docs_ > 0L) n_classed_ / n_run_docs_ else NA_real_
  )

  # Per Class x Combo x Label.
  prof_ <- cand_ |>
    dplyr::inner_join(class_, by = "DocID") |>
    dplyr::group_by(Class, Engine, Model, Label) |>
    dplyr::summarise(
      Candidates  = dplyr::n(),
      DocsWithHit = dplyr::n_distinct(DocID),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::left_join(docs_, by = "Class") |>
    dplyr::mutate(
      CandPerDoc     = Candidates / NDocs,
      PctDocsWithHit = DocsWithHit / NDocs
    ) |>
    add_combo_() |>
    dplyr::relocate(Class, Combo, Engine, Model, Label) |>
    dplyr::arrange(Class, Combo, dplyr::desc(Candidates))

  # Readable fingerprint: one reference combo, Class x Label, CandPerDoc.
  ref_ <- ner_parse_combo(.ref_combo)
  fingerprint_ <- prof_ |>
    dplyr::filter(Engine == ref_$Engine[1], Model == ref_$Model[1]) |>
    dplyr::select(Class, Label, CandPerDoc) |>
    tidyr::pivot_wider(names_from = Label, values_from = CandPerDoc, values_fill = 0) |>
    dplyr::left_join(docs_, by = "Class") |>
    dplyr::relocate(Class, NDocs) |>
    dplyr::arrange(dplyr::desc(NDocs))
  if (nrow(fingerprint_) == 0L && !.quiet) {
    cli::cli_alert_warning("Reference combo {.val {(.ref_combo)}} not present -> empty fingerprint.")
  }

  # High-confidence (>= .min_combos engines agree) per-type rate, from merged
  # mentions. Engine-agnostic, so no double counting across engines.
  consensus_ <- NULL
  if (!is.null(.mentions)) {
    consensus_ <- .mentions |>
      dplyr::filter(NCombos >= .min_combos) |>
      dplyr::inner_join(class_tbl_, by = "DocID") |>
      dplyr::group_by(Class, Label) |>
      dplyr::summarise(
        HiConfMentions = dplyr::n(),
        DocsWithHit    = dplyr::n_distinct(DocID),
        .groups = "drop"
      ) |>
      dplyr::left_join(docs_, by = "Class") |>
      dplyr::mutate(
        HiConfPerDoc   = HiConfMentions / NDocs,
        PctDocsWithHit = DocsWithHit / NDocs
      ) |>
      dplyr::arrange(Class, dplyr::desc(HiConfMentions))
  }

  if (!.quiet) {
    pct_classed_ <- scales::label_percent(0.1)(coverage_$PctClassed)
    cons_msg_    <- if (is.null(.mentions)) " (pass .mentions for the consensus view)" else ""
    cli::cli_alert_success(
      paste0("Profiled {nrow(docs_)} class(es) over {coverage_$NClassed} classified ",
             "doc(s) ({pct_classed_} of the ledger){cons_msg_}.")
    )
    if (!is.na(coverage_$PctClassed) && coverage_$PctClassed < 0.9) {
      pct_unclassed_ <- scales::label_percent(0.1)(1 - coverage_$PctClassed)
      cli::cli_alert_warning(paste0("{pct_unclassed_} of ledger docs have no class -- check ",
                                    "{.arg .class_parquet} / {.arg .class_col} coverage."))
    }
  }

  list(
    docs        = docs_,
    coverage    = coverage_,
    profile     = prof_,
    fingerprint = fingerprint_,
    consensus   = consensus_
  )
}


# Heatmap of the per-class fingerprint: Class (rows) x Label (cols), filled by the
# chosen metric. .profile is ent_profile_by_class()$profile. Pass .combo to pick one
# engine:model (else it facets across combos). .metric: CandPerDoc | PctDocsWithHit.


# 8. Figures ---------------------------------------------------------------------------------------------------------
# Three shapes, all drawn through the shared design layer so that an entity figure and a
# classification figure are the same object rendered from different data. Two of the three are the
# heatmap primitive with different keys; only the positional histogram needs its own ggplot.
#
# None of them sets a title. The caption in the runbook carries that, which is what makes the figure
# reusable in the paper without editing the function that drew it.

#' Figure height for a wrapped facet grid
#'
#' plot_height() maps a count of rows on a discrete axis onto the project's four-rung ladder. A
#' faceted figure has no such axis: its vertical extent is driven by how many FACET rows the wrap
#' produces, and each facet is worth several chart rows of space. Passing the panel count straight
#' to plot_height() therefore understates a two-row grid by about a third, which is how the two
#' figures in this document came to carry hand-typed heights off the ladder entirely.
#'
#' This converts one to the other and then defers to the ladder, so the height is still derived
#' rather than chosen. Nothing here is added to the shared design layer: the conversion depends on
#' how a particular figure is wrapped, which is the document's business and not the layer's.
#'
#' @param .n_panels Integer. Facets in the wrap.
#' @param .n_cols Integer. Columns ggplot2 will lay them out in. Three is what facet_wrap() chooses
#'   for five panels at the project's fixed width.
#' @param .rows_per_panel Integer. Chart rows one facet is worth vertically. Six reproduces the
#'   height these figures were rendered and read at before the ladder was applied.
#' @param .square Logical. Passed through for matrix facets, where height tracks width.
#' @return Numeric height in inches, taken from the ladder.
ent_facet_height <- function(.n_panels, .n_cols = 3L, .rows_per_panel = 6L, .square = FALSE) {
  if (FALSE) {
    .n_panels       <- 5L
    .n_cols         <- 3L
    .rows_per_panel <- 6L
    .square         <- FALSE
  }
  plot_height(.rows_per_panel * ceiling(.n_panels / .n_cols), .square = .square)
}

#' Where candidates sit relative to document length
#'
#' The figure the geography claim rests on. Mass at the opening and again around the governing-law
#' clause supports describing extracted places as party locations; a flat distribution through the
#' body does not.
#'
#' DENSITY, NOT COUNTS, and the reason is that counts cannot answer the question this figure is
#' asked. The engines differ in volume by more than an order of magnitude -- spaCy proposes 1.5
#' million organisations where LexNLP proposes 78 thousand -- so a stacked count is a picture of
#' which engine is loudest, and the smaller arms are slivers along the axis. Normalising each
#' engine to its own total makes the SHAPES comparable, which is what "do the engines agree about
#' where this entity type lives" actually means. Volume is already reported, per engine and per
#' label, in the store ledger.
#'
#' Lines rather than bars for the same reason: nine overlaid histograms occlude each other whatever
#' the transparency, and the comparison here is between profiles rather than between totals.
#'
#' @param .positions The positions tibble from ent_overview(). Requires that .inputs was supplied
#'   there, since position is computed against document length.
#' @param .bins Integer. Bins across the unit interval.
#' @param .by_combo Logical. One profile per combination. FALSE pools every engine into a single
#'   distribution, which is the right view only when the engines are already known to agree.
#' @param .free_y Logical. Independent vertical scales per facet. Even as densities the labels
#'   differ in concentration, so a shared scale flattens the flatter panels.
#' @return A ggplot.
ent_plot_positions <- function(.positions, .bins = 30L, .by_combo = TRUE, .free_y = TRUE) {
  if (FALSE) {
    .positions <- .ov$positions
    .bins      <- 30L
    .by_combo  <- TRUE
    .free_y    <- TRUE
  }
  if (is.null(.positions) || nrow(.positions) == 0L) {
    cli::cli_abort("No positions to plot: pass {.arg .inputs} to ent_overview().")
  }

  # Binned in R rather than by geom_histogram(), because the share has to be taken WITHIN each
  # engine and label. Left to the geom, ggplot would normalise across the whole panel and the
  # figure would silently become the count plot again.
  brk_  <- seq(0, 1, length.out = .bins + 1L)
  grp_  <- if (.by_combo) c("Label", "Combo") else "Label"

  dat_ <- .positions |>
    dplyr::mutate(Bin = cut(.data$Rel, breaks = brk_, include.lowest = TRUE, labels = FALSE)) |>
    dplyr::summarise(N = dplyr::n(), .by = dplyr::all_of(c(grp_, "Bin"))) |>
    dplyr::mutate(
      Share = .data$N / sum(.data$N),
      .by   = dplyr::all_of(grp_)
    ) |>
    dplyr::mutate(
      Mid       = brk_[.data$Bin] + diff(brk_)[1] / 2,
      PlotLabel = plot_factor(.data$Label, .key = "Label")
    )

  p_ <- if (.by_combo) {
    dat_ |>
      dplyr::mutate(PlotCombo = plot_factor(.data$Combo, .key = "Combo", .short = TRUE)) |>
      ggplot2::ggplot(ggplot2::aes(x = .data$Mid, y = .data$Share, colour = .data$PlotCombo)) +
      ggplot2::geom_line(linewidth = 0.5) +
      plot_scale_colour_key(.key = "Combo", .short = TRUE)
  } else {
    dat_ |>
      ggplot2::ggplot(ggplot2::aes(x = .data$Mid, y = .data$Share)) +
      ggplot2::geom_line(linewidth = 0.5, colour = "#002147")
  }

  p_ +
    ggplot2::facet_wrap(~PlotLabel, scales = if (.free_y) "free_y" else "fixed") +
    plot_scale_x_pct(
      .accuracy = 1,
      .expand   = c(0, 0),
      .breaks   = scales::breaks_pretty(n = 4)   # 0/25/50/75/100, not ggplot's 2/5/8/10
    ) +
    plot_scale_y_pct(
      .accuracy = 1,
      .expand   = c(0, 0.02),
      .breaks   = scales::breaks_pretty(n = 4)
    ) +
    ggplot2::labs(x = "Position in document", y = "Share of the engine's candidates", colour = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}

#' Pairwise agreement between engines, by label
#'
#' Read this by family rather than by cell. High values mean the two engines are substitutes and the
#' choice between them costs nothing; low values mark the spans on which they disagree. The trap is
#' that four of the nine combinations are the same spaCy pipeline at four capacities, so their
#' mutual agreement is one implementation agreeing with itself and says nothing about whether any of
#' them is right.
#'
#' ONLY ELIGIBLE ENGINES APPEAR IN A PANEL. An engine that cannot emit a label has not disagreed
#' about it, and drawing it as an empty row invites exactly that misreading -- which is what the
#' first version of this figure did, giving every panel a nine-by-nine grid of which five rows and
#' five columns were structurally blank. The axes therefore drop per panel, which needs both the
#' free facet scales below and .drop on the primitive.
#'
#' The fill scale is fixed to the unit interval. Jaccard has a meaningful absolute scale, and left
#' to auto-scale the ramp would stretch across whatever range the data happened to occupy, rendering
#' rounding differences as strong visual structure.
#'
#' @param .pairwise The pairwise tibble from ent_alignment().
#' @param .accuracy Numeric. Rounding for the printed cell values.
#' @return A ggplot.
ent_plot_agreement <- function(.pairwise, .accuracy = 0.01) {
  if (FALSE) {
    .pairwise <- .al$pairwise
    .accuracy <- 0.01
  }
  if (is.null(.pairwise) || nrow(.pairwise) == 0L) {
    cli::cli_abort("No pairwise agreement to plot: a label needs at least two combinations.")
  }

  .pairwise |>
    plot_heatmap(
      .tab      = _,
      .x        = "ComboA",       # both axes carry the same vocabulary, so both are keyed on Combo
      .y        = "ComboB",
      .fill     = "Jaccard",
      .key_x    = "Combo",
      .key_y    = "Combo",
      .short    = TRUE,           # the full tokens run to twenty-one characters
      .label    = TRUE,
      .pct      = FALSE,          # Jaccard reads as a ratio, not as a percentage
      .accuracy = .accuracy,
      .angle    = 40,
      .limits   = c(0, 1),        # fixed: the quantity has an absolute scale
      .drop     = "both"          # per panel, with the free scales below
    ) +
    ggplot2::facet_wrap(~Label, scales = "free")
}

#' Yield per contract type and entity label
#'
#' One combination at a time. A wide matrix faceted over nine engines is unreadable at the project's
#' fixed width, and the comparison this figure is for is across contract types rather than across
#' engines -- that comparison is what ent_plot_agreement() is for.
#'
#' THE SHADING AND THE NUMBERS ARE DIFFERENT QUANTITIES, deliberately. Organisations run from 80 to
#' 509 per document while money runs from 3 to 19, so one ramp across the whole matrix renders three
#' of the four columns uniformly white and the figure carries one column of information. Each cell
#' is therefore shaded by its share of the largest value IN ITS OWN LABEL, and prints the raw value.
#' Every column becomes legible and no number changes; what is lost is comparability of shade
#' across columns, which was never readable anyway.
#'
#' Two guards on that. The rescaling is switched off below .min_rows, because a share of the column
#' maximum needs a distribution to rescale and a two-row matrix has none. And the ramp is fixed to
#' the observed range rather than to the unit interval, because the smallest class over the largest
#' is around a sixth, so a unit-interval ramp spends its lower third on values that never occur and
#' pushes the whole matrix into the dark half.
#'
#' The contract-type axis is keyed on the classification vocabulary 03A registered, so the rows
#' appear in the same order and under the same short names as every figure in the 03 family. That is
#' the point of a shared registry: a reader does not have to re-learn an ordering between documents.
#'
#' A label the chosen engine cannot emit is dropped rather than drawn as an empty column, on the
#' same argument as the agreement figure: the transformer emits no redaction markers, and a blank
#' REDACT column reads as "no contract type contains any" rather than "this engine does not look".
#'
#' @param .profile The profile tibble from ent_profile_by_class().
#' @param .combo Combination token to draw, or NULL to facet over all of them.
#' @param .metric Which quantity to fill by. CandPerDoc is inflated by document length;
#'   PctDocsWithHit is not, and is the one carrying the conclusion about absence.
#' @param .key_class Registration key for the contract-type axis, matching the .class_col the
#'   profile was built with.
#' @param .accuracy Numeric. Rounding for the printed cell values.
#' @param .min_rows Integer. Rows below which the within-column rescaling is switched off. Share of
#'   the column maximum sets the largest cell in every column to one, so on a two-row matrix the top
#'   row is uniformly darkest by construction. The threshold is a floor on having a distribution to
#'   rescale, not a tuned value.
#' @return A ggplot.
ent_plot_profile <- function(.profile, .combo = NULL,
                             .metric = c("CandPerDoc", "PctDocsWithHit"),
                             .key_class = "ClassDetailed", .accuracy = 0.1,
                             .min_rows = 4L) {
  if (FALSE) {
    .profile   <- .prof_detailed$profile
    .combo     <- "spacy:en_core_web_trf"
    .metric    <- "CandPerDoc"
    .key_class <- "ClassDetailed"
    .accuracy  <- 0.1
    .min_rows  <- 4L
  }
  .metric <- match.arg(.metric)
  if (is.null(.profile) || nrow(.profile) == 0L) cli::cli_abort("Empty {.arg .profile}.")

  dat_ <- .profile
  if (!is.null(.combo)) dat_ <- dplyr::filter(dat_, .data$Combo == .combo)
  if (nrow(dat_) == 0L) cli::cli_abort("No rows for combination {.val {(.combo)}}.")

  is_pct_ <- identical(.metric, "PctDocsWithHit")

  # RESCALING NEEDS ENOUGH ROWS TO HAVE A DISTRIBUTION. Share of the column maximum sets the largest
  # cell in every column to one, so on a two-row matrix the top row is uniformly darkest by
  # construction and carries no information at all -- which is exactly what it did to the amendment
  # figure. Below the floor the raw value is shaded directly, which is readable at that size because
  # a two-row column has nothing to bury.
  n_rows_  <- dplyr::n_distinct(dat_$Class)
  rescale_ <- !is_pct_ && n_rows_ >= .min_rows

  dat_ <- dat_ |>
    dplyr::mutate(Value = .data[[.metric]]) |>
    dplyr::mutate(
      Shade = if (rescale_) .data$Value / max(.data$Value, na.rm = TRUE) else .data$Value,
      .by   = Label
    )

  # A hit rate has an absolute scale and is fixed to it. A rescaled share does not use its full
  # range -- the floor here is the smallest class over the largest, around a sixth -- so fixing the
  # ramp to the unit interval spends a third of it on values that never occur and renders the whole
  # matrix in the dark half. Fixing it to the observed range instead spreads the contrast over the
  # values actually present.
  lim_ <- if (is_pct_) {
    c(0, 1)
  } else if (rescale_) {
    range(dat_$Shade, na.rm = TRUE)
  } else {
    NULL
  }

  p_ <- dat_ |>
    plot_heatmap(
      .tab        = _,
      .x          = "Label",
      .y          = "Class",
      .fill       = "Shade",        # within-label share where there are rows enough to warrant it
      .cell_label = if (rescale_) "Value" else NULL,  # the raw count is what gets printed
      .key_x      = "Label",
      .key_y      = .key_class,     # the vocabulary 03A registered, so rows match the 03 family
      .short      = TRUE,
      .label      = TRUE,
      .pct        = is_pct_,
      .accuracy   = if (is_pct_) 0.01 else .accuracy,
      .angle      = 0,              # five short labels; rotation would cost legibility for nothing
      .limits     = lim_,
      # x only: a label this engine cannot emit is absent, not empty. The contract types stay
      # whatever happens, because a category holding no documents is a finding and not a gap.
      .drop       = "x"
    )

  if (is.null(.combo) && dplyr::n_distinct(dat_$Combo) > 1L) {
    p_ <- p_ + ggplot2::facet_wrap(~Combo)
  }
  p_
}
