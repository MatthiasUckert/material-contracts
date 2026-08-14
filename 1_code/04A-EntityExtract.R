# 04A-EntityExtract: freeze the extraction inputs, run every engine, describe what came back ---------------------------
#
# WHAT THIS FILE DOES, IN ONE PARAGRAPH
# 04A takes the 4.4k hand-labelled contracts that 03A prepared, freezes ONE canonical text per
# document, runs every entity extractor over the WHOLE of that text, and folds the results into one
# DuckDB candidate store. It then answers three questions and no others: what came back, where in
# the document it sits, and how far the engines agree with each other. Nothing is selected, filtered
# or ranked here.
#
# WHY IT ONLY DESCRIBES
# The 03 family had 4.4k hand labels and could compute macro-F1. There are no entity labels. Without
# them, the only measurable quantity at this stage is YIELD -- candidates per document, documents
# with a hit. Yield does not order engines: a higher count is either better recall or worse
# precision, and the two are indistinguishable until something external exists to score against. So
# this document answers "what is there and where do the engines differ", and leaves "which engine is
# right" to 04B, which scores against facts EDGAR already recorded.
#
# THE LEDGER IS BLIND TO LABELS, WHICH IS WHY THE MANIFEST RECORDS THEM
# The ledger knows that engine E has seen document D. It does not know which labels were asked for.
# Add a label to the policy and re-render, and every combination is already marked done: the store
# reports itself complete while holding output built under the previous label set. That is not
# hypothetical -- PERSON was absent from the spaCy policy for the whole first pass, and nothing in
# the document could see it. The manifest closes the hole by recording the RESOLVED label set per
# engine, so a policy edit changes the fingerprint, and a changed fingerprint clears the affected
# engines so they extract again.
#
# THE ONE DECISION THAT CANNOT BE UNDONE LATER
# Offsets are integers into a specific string. Every candidate in the store and every rehydrated
# span indexes THAT string. If two scripts disagree about what a document's text is, every offset in
# the project is quietly wrong and nothing errors. 04A therefore writes sample_text.parquet and is
# the only script permitted to define it; everything downstream reads that file and never
# reconstructs the text.
#
# ONE STORE, AND THE GRID IN IT IS RAGGED
# There is one store and every engine reads the whole document. An earlier design carried a second
# store holding the same engines run over a truncated head, to price a truncation that was then not
# adopted; it is gone, and with it the reason the store used to accumulate retired engine versions.
#
# The store is Engine x (the labels that engine can emit), not a full crossing. A gazetteer cannot
# propose a person and a date regex cannot propose an organisation, so empty cells in every table
# below mean "this engine does not do that" rather than "this failed". The ragged shape is declared
# in _NER.R's label policy and reproduced in the manifest, so a reader can check it rather than
# infer it.
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
# Label is a closed set of six and orders by how much of the paper rests on it: the two party types
# first, then geography, then dates, then value, then the redaction indicators that are not an
# entity type at all but travel in the same coordinate system.
#
# PERSON sits beside ORG because on this corpus it IS a party type. Roughly half of Exhibit 10
# material contracts are employment, indemnity or award documents whose counterparty is an
# individual, and a family that reads only organisations reports the largest part of the sample as
# having one party. Only the spaCy models emit it: LexNLP excludes it by policy and the four rule
# engines are single-label specialists.
#
# Combo is nine engine:model tokens and needs explicit colours, because the categorical palette caps
# at eight. The short labels matter more here than anywhere else in the project: en_core_web_trf is
# twenty-one characters and a legend built from the full tokens consumes most of a 7.5-inch canvas.
# Colours are assigned by FAMILY rather than by position -- the three spaCy CNN models share a blue
# ramp, the transformer takes the darkest blue, LexNLP takes the warm accent, and the four ported
# paper extractors take neutral tones -- so a reader can see at a glance which marks belong to one
# implementation family and which are genuinely independent evidence.

.ent_labels <- c("ORG", "PERSON", "GPE", "DATE", "MONEY", "REDACT")

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
#' RECORDS THE RESOLVED LABEL SET, NOT A POINTER TO THE POLICY. An earlier version wrote the literal
#' string "per-combo policy" whenever .labels was NULL, which is what 04A always passes. Editing the
#' policy in _NER.R therefore left the fingerprint identical and the comparison reported a match
#' while the store held output built under the previous label set -- the exact failure the manifest
#' exists to catch. Resolving through ner_label_policy() means the fingerprint moves whenever the
#' policy does.
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
    Labels    = ent_labels_resolved(.run = .run, .labels = .labels) |>
      (\(.d) paste0(.d$Combo, "=", .d$Labels, collapse = " | "))(),
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
    .db_path <- .lP$Store$NerDB
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
    .db_path <- .lP$Store$NerDB
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


#' The label set each declared combination will actually be asked for
#'
#' ner_run() holds the per-engine policy and applies it silently. Reading it back out gives a reader
#' the table that explains why the store's grid is ragged: a gazetteer emits places and nothing
#' else, so its empty columns are a property of the engine and not a failure.
#'
#' NO LONGER A GUARD. It used to fingerprint the labels so a policy change could be detected and the
#' affected engines cleared. The ledger is keyed on the label now, so a change is visible as missing
#' work rather than as a mismatch, and nothing has to be cleared to act on it. What remains is
#' documentation.
#'
#' @param .run Character vector of combo tokens.
#' @param .labels Character, named list or NULL, exactly as passed to ner_run().
#' @return Tibble: Combo, Labels -- the resolved set, comma-joined and sorted so it compares as a
#'   string.
ent_labels_resolved <- function(.run, .labels = NULL) {
  if (FALSE) {
    .run    <- lst_run_args$.run
    .labels <- NULL
  }

  tibble::tibble(Combo = sort(.run)) |>
    dplyr::mutate(
      Labels = purrr::map_chr(.data$Combo, function(.tok) {
        parts_  <- strsplit(.tok, ":", fixed = TRUE)[[1]]
        engine_ <- parts_[1]
        model_  <- if (length(parts_) > 1L) paste(parts_[-1], collapse = ":") else engine_
        paste(sort(ner_label_policy(.engine = engine_, .model = model_, .labels = .labels)),
              collapse = ",")
      })
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


# 4. Agreement --------------------------------------------------------------------------------------------------------
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


# 5. Tuning ------------------------------------------------------------------------------------------------------------
# Measures the SETTINGS rather than the engines. Everything here runs against a scratch store that
# is deleted afterwards, because timing against the real store would be circular: the ledger would
# report the documents already done and the second setting would be timed on no work at all.

#' Time one extractor over a subsample at a given batch size
#'
#' Startup is subtracted. Container start, library import and dictionary construction are paid once
#' per invocation whatever the batch size, so leaving them in would compress the very differences
#' being measured -- and compress them most at the settings that finish fastest.
#'
#' @param .combo Character. Combination token, e.g. "lexnlp".
#' @param .path_text Canonical text parquet.
#' @param .n Integer. Documents drawn.
#' @param .batch Integer. Batch size under test.
#' @param .n_process Integer. Held fixed; this measures the batch knob.
#' @param .timeout Integer. Per-document cap in seconds.
#' @param .start_seconds Numeric. Fixed cost to subtract, from ent_tune_startup().
#' @param .seed Integer. Draw seed.
#' @return Tibble: Combo, Batch, NetSeconds, DocsPerSec, Candidates.
ent_tune_run <- function(.combo, .path_text, .n = 200L, .batch = 8L, .n_process = 20L,
                         .timeout = 240L, .start_seconds = 0, .seed = 42L) {
  if (FALSE) {
    .combo         <- "lexnlp"
    .path_text     <- .lP$Sample$Text
    .n             <- 200L
    .batch         <- 8L
    .n_process     <- 20L
    .timeout       <- 240L
    .start_seconds <- 3
    .seed          <- 42L
  }

  dir_ <- fs::dir_create(fs::path(tempdir(), paste0("tune_", gsub("[:/]", "_", .combo))))
  on.exit(if (fs::dir_exists(dir_)) fs::dir_delete(dir_), add = TRUE)

  arrow::read_parquet(.path_text) |>
    (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n, nrow(.d)))))() |>
    dplyr::select(DocID, TextRaw) |>
    arrow::write_parquet(fs::path(dir_, "in.parquet"))

  db_ <- fs::path(dir_, "scratch.duckdb")
  t0_ <- Sys.time()
  ok_ <- tryCatch({
    ner_run(
      .inputs = fs::path(dir_, "in.parquet"), .db_path = db_,
      .run = .combo, .labels = NULL, .max_chars = NULL,
      .n_process = .n_process, .batch_size = .batch, .timeout = .timeout,
      .device = "auto", .no_progress = TRUE, .quiet = TRUE
    )
    TRUE
  }, error = function(.e) FALSE)
  secs_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))
  net_  <- pmax(secs_ - .start_seconds, 0.1)

  n_cand_ <- if (ok_ && fs::file_exists(db_)) {
    con_ <- ner_db_connect(.db_path = db_, .read_only = TRUE)
    on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE, after = FALSE)
    DBI::dbGetQuery(con_, "SELECT COUNT(*) AS N FROM candidates")$N
  } else {
    NA_integer_
  }

  tibble::tibble(
    Combo      = .combo,
    Batch      = as.integer(.batch),
    NetSeconds = round(net_, 1),
    DocsPerSec = round(.n / net_, 2),
    Candidates = as.integer(n_cand_)
  )
}


#' The fixed cost of one invocation, so it can be taken out of the comparison
#' @param .combo Combination token.
#' @param .path_text Canonical text parquet.
#' @param .n_process Integer.
#' @return Numeric seconds.
ent_tune_startup <- function(.combo, .path_text, .n_process = 20L) {
  if (FALSE) {
    .combo     <- "lexnlp"
    .path_text <- .lP$Sample$Text
    .n_process <- 20L
  }
  ent_tune_run(.combo = .combo, .path_text = .path_text, .n = 1L, .batch = 8L,
               .n_process = .n_process, .start_seconds = 0)$NetSeconds
}


#' Sweep the batch size for one engine and report it
#'
#' BATCH SIZE IS THE KNOB THAT MOVES. Measured on this corpus, LexNLP runs 4.6 times slower at a
#' batch of 128 than at 8: the pool hands out one task per batch, so a 200-document subsample at 128
#' is two tasks for twenty workers. Document length makes it worse -- one contract in this sample
#' runs to 876,000 characters, and a large batch strands whichever worker draws it while the rest
#' finish early and idle.
#'
#' Worker count is not swept. It rises 1.60x from eight to sixteen and 1.15x from sixteen to
#' twenty-four, which is a 28-core M3 Ultra reaching the end of its twenty performance cores. That
#' is a property of the machine rather than of the extraction, and measuring it once is enough.
#'
#' CACHED, AND THE ALTERNATIVE IS THREE MINUTES ON EVERY RENDER. A sweep costs roughly 165 seconds
#' for LexNLP alone, most of it in the slowest cell -- which is to say most of the bill is spent
#' re-measuring the setting already rejected. The result changes only when the grid, the sample
#' size, the worker count or the extractor itself changes, none of which happens between renders of
#' a document being read for its tables.
#'
#' The cache is keyed on the settings, so editing the grid re-measures automatically. It is NOT
#' keyed on the extractor's own code: change a Python file and the stale timing survives, which is
#' what .rerun is for. Same trade the corpus index makes, and the same caveat.
#'
#' @param .combo Combination token.
#' @param .path_text Canonical text parquet.
#' @param .path_cache Parquet holding previous sweeps. NULL disables caching entirely.
#' @param .rerun Logical. TRUE re-measures and overwrites the cached row for this key.
#' @param .batches Integer vector of batch sizes.
#' @param .n Documents per cell.
#' @param .n_process Held fixed.
#' @param .timeout Per-document cap.
#' @return Invisibly the sweep tibble.
ent_report_tuning <- function(.combo, .path_text, .path_cache = NULL, .rerun = FALSE,
                              .batches = c(8L, 32L, 128L),
                              .n = 200L, .n_process = 20L, .timeout = 240L) {
  if (FALSE) {
    .combo      <- "lexnlp"
    .path_text  <- .lP$Sample$Text
    .path_cache <- .lP$Sample$Tuning
    .rerun      <- FALSE
    .batches    <- c(8L, 32L, 128L)
    .n          <- 200L
    .n_process  <- 20L
    .timeout    <- 240L
  }

  cli::cli_h3("Batch size: {(.combo)}")

  key_ <- paste(.combo, paste(sort(.batches), collapse = "-"), .n, .n_process, .timeout, sep = "|")
  cached_ <- if (!is.null(.path_cache) && fs::file_exists(.path_cache) && !isTRUE(.rerun)) {
    arrow::read_parquet(.path_cache) |> dplyr::filter(.data$Key == key_)
  } else {
    tibble::tibble()
  }

  out_ <- if (nrow(cached_) > 0L) {
    cli::cli_alert_info("Cached; set {.arg .rerun} to re-measure.")
    dplyr::select(cached_, -Key)
  } else {
    start_ <- ent_tune_startup(.combo = .combo, .path_text = .path_text, .n_process = .n_process)
    res_ <- purrr::map(.batches, function(.b) {
      r_ <- ent_tune_run(.combo = .combo, .path_text = .path_text, .n = .n, .batch = .b,
                         .n_process = .n_process, .timeout = .timeout, .start_seconds = start_)
      cat(sprintf("   batch %-5d %7.1fs net  %7.2f doc/s  %8d candidates\n",
                  .b, r_$NetSeconds, r_$DocsPerSec, r_$Candidates))
      utils::flush.console()
      r_
    }) |>
      purrr::list_rbind() |>
      dplyr::mutate(Relative = round(max(.data$NetSeconds) / .data$NetSeconds, 2))

    if (!is.null(.path_cache)) {
      prior_ <- if (fs::file_exists(.path_cache)) {
        arrow::read_parquet(.path_cache) |> dplyr::filter(.data$Key != key_)
      } else {
        tibble::tibble()
      }
      fs::dir_create(fs::path_dir(.path_cache))
      dplyr::bind_rows(prior_, dplyr::mutate(res_, Key = key_)) |>
        arrow::write_parquet(.path_cache)
    }
    res_
  }

  tbl_say(.tab = out_, .title = paste0("Batch size at n_process = ", .n_process))
  invisible(out_)
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

#' What each engine found, and where it sits in the document
#'
#' One block, replacing the three the document used to carry. The question worth asking of an
#' unlabelled extraction is narrow: is the label usable at all, and do the engines put it in the
#' same places.
#'
#' DISTINCT PER DOCUMENT IS THE COLUMN THAT ANSWERS THE FIRST QUESTION, not the raw count. Total
#' spans measures how often an engine repeats itself; distinct spans measure how many things it
#' found. A contract names a handful of people and a handful of counterparties, so a distinct median
#' in the hundreds says the engine is collecting signatories, notice contacts and capitalised words
#' rather than parties -- and the raw count cannot tell that apart from good recall.
#'
#' Pooled rather than broken down by contract type. Extraction yield varies with document length and
#' little else at this stage; the by-type breakdown earns its place in 04B, where coverage genuinely
#' differs by type and the difference carries a finding.
#'
#' Aggregated database-side. The store holds tens of millions of rows and none of them enters R.
#'
#' POSITION IS BINNED IN THE DATABASE, not collected and binned in R. The earlier overview returned
#' one row per candidate so the figure could histogram it, which meant pulling every span in the
#' store across the boundary -- millions of rows, for a picture with thirty bars in it. Thirty is
#' divisible by ten, so the same table serves the figure at full resolution and the console table
#' rolled up to deciles.
#'
#' @param .db_path DuckDB candidate store.
#' @param .path_text Canonical text parquet, supplying document lengths.
#' @param .bins Integer. Position bins; a multiple of ten, so deciles roll up exactly.
#' @return A list of two tibbles. $yield is Combo, Label, Spans, Docs, PerDoc, DistinctPerDoc,
#'   MaxPerDoc. $position is Combo, Label, Bin, Mid, Spans, Share.
ent_describe <- function(.db_path, .path_text, .bins = 30L) {
  if (FALSE) {
    .db_path   <- .lP$Store$NerDB
    .path_text <- .lP$Sample$Text
    .bins      <- 30L
  }
  if (.bins %% 10L != 0L) cli::cli_abort("{.arg .bins} must be a multiple of ten.")

  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TEMP VIEW lens AS SELECT DocID, length(TextRaw) AS DocLen ",
    "FROM read_parquet('", as.character(fs::path_abs(.path_text)), "') WHERE length(TextRaw) > 0"
  ))

  combo_ <- "CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END"

  yield_ <- DBI::dbGetQuery(con_, paste0(
    "WITH per AS ( ",
    "  SELECT ", combo_, " AS Combo, Label, DocID, ",
    "    COUNT(*) AS N, COUNT(DISTINCT upper(Span)) AS NDistinct ",
    "  FROM candidates WHERE Label IS NOT NULL GROUP BY Combo, Label, DocID) ",
    "SELECT Combo, Label, SUM(N) AS Spans, COUNT(*) AS Docs, ",
    "  median(N) AS PerDoc, median(NDistinct) AS DistinctPerDoc, max(N) AS MaxPerDoc ",
    "FROM per GROUP BY Combo, Label"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(c(Spans, Docs, PerDoc, DistinctPerDoc, MaxPerDoc), as.integer))

  n_ <- as.integer(.bins)
  pos_ <- DBI::dbGetQuery(con_, paste0(
    "SELECT ", combo_, " AS Combo, c.Label, ",
    "  least(", n_ - 1L, ", CAST(floor((((c.Start + c.Stop) / 2.0) / l.DocLen) * ", n_,
    ") AS INTEGER)) AS Bin, COUNT(*) AS Spans ",
    "FROM candidates c JOIN lens l USING (DocID) ",
    "WHERE c.Label IS NOT NULL AND c.Start IS NOT NULL ",
    "GROUP BY Combo, c.Label, Bin"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      Spans = as.integer(.data$Spans),
      Bin   = as.integer(.data$Bin),
      Mid   = (.data$Bin + 0.5) / n_
    ) |>
    # Share WITHIN engine and label. Taken across the whole table instead, a figure would compare
    # engines on how much they emit rather than on where they emit it.
    dplyr::mutate(Share = .data$Spans / sum(.data$Spans), .by = c(Combo, Label))

  list(yield = yield_, position = pos_)
}

#' Collapse a positional distribution to one comparable number
#'
#' Ten decile shares per engine and label are readable pooled and unreadable once a twelve-class
#' breakdown multiplies them. The contrast is the mean share at the two ends of the document over
#' the mean share through the middle, which is one number that answers the question the shares were
#' being consulted for: does this engine concentrate the label where parties are named, or spread it
#' evenly through the text.
#'
#' A uniform engine scores 1.0 by construction. LexNLP's organisations score about 4.5; every spaCy
#' model scores about 1.0.
#'
#' D1 AND D8 ARE EXCLUDED FROM THE MIDDLE deliberately. They are shoulder: a preamble spills into the
#' second decile of a short document and a signature block into the ninth, so counting them as
#' middle would blunt the very contrast being measured.
#'
#' @param .tab Binned table with Mid and Share, already shared within .by.
#' @param .by Character vector of grouping columns.
#' @return Tibble: the grouping columns, plus EndShare, MidShare and Contrast.
ent_contrast <- function(.tab, .by = c("Combo", "Label")) {
  if (FALSE) {
    .tab <- .desc$position
    .by  <- c("Combo", "Label")
  }

  .tab |>
    dplyr::mutate(Zone = dplyr::case_when(
      .data$Mid < 0.1 | .data$Mid >= 0.9  ~ "End",
      .data$Mid >= 0.2 & .data$Mid < 0.8  ~ "Mid",
      TRUE                                ~ "Shoulder"
    )) |>
    dplyr::summarise(Share = sum(.data$Share), .by = dplyr::all_of(c(.by, "Zone"))) |>
    tidyr::pivot_wider(names_from = Zone, values_from = Share, values_fill = 0) |>
    dplyr::mutate(
      # Two deciles at the ends against six through the middle, so both sides are per-decile means
      # and the ratio is scale-free.
      EndShare = .data$End / 2,
      MidShare = .data$Mid / 6,
      Contrast = dplyr::if_else(.data$MidShare > 0, .data$EndShare / .data$MidShare, NA_real_)
    ) |>
    dplyr::select(dplyr::all_of(.by), EndShare, MidShare, Contrast)
}


#' The same description, cut by contract type
#'
#' A pooled median hides an engine that behaves well on employment agreements and badly on credit
#' agreements, and those are exactly the two the party rules will lean on hardest. Two quantities
#' carry the engine verdict -- distinct spans per document, and positional contrast -- so those are
#' the two cut here, rather than the whole yield table cut twelve ways.
#'
#' READ `Docs` BEFORE READING `Contrast`. A contrast computed over a handful of documents is a
#' number about those documents. The column is reported rather than the cell suppressed, because a
#' threshold chosen here would be a decision hidden in a helper.
#'
#' @param .db_path DuckDB candidate store.
#' @param .path_text Canonical text parquet, supplying document lengths.
#' @param .path_anchors Sample anchors parquet, supplying the contract type.
#' @param .bins Integer. Position bins; a multiple of ten.
#' @return Tibble: Class, Combo, Label, Docs, Spans, DistinctPerDoc, Contrast.
ent_describe_class <- function(.db_path, .path_text, .path_anchors, .bins = 30L) {
  if (FALSE) {
    .db_path      <- .lP$Store$NerDB
    .path_text    <- .lP$Sample$Text
    .path_anchors <- .lP$Sample$Anchors
    .bins         <- 30L
  }
  if (.bins %% 10L != 0L) cli::cli_abort("{.arg .bins} must be a multiple of ten.")

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TEMP VIEW lens AS SELECT DocID, length(TextRaw) AS DocLen ",
    "FROM read_parquet('", as.character(fs::path_abs(.path_text)), "') WHERE length(TextRaw) > 0"
  ))
  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TEMP VIEW cls AS SELECT DocID, ClassDetailed AS Class ",
    "FROM read_parquet('", as.character(fs::path_abs(.path_anchors)), "')"
  ))

  combo_ <- "CASE WHEN c.Engine = c.Model THEN c.Engine ELSE c.Engine || ':' || c.Model END"
  n_     <- as.integer(.bins)

  yield_ <- DBI::dbGetQuery(con_, paste0(
    "WITH per AS ( ",
    "  SELECT k.Class, ", combo_, " AS Combo, c.Label, c.DocID, ",
    "    COUNT(*) AS N, COUNT(DISTINCT upper(c.Span)) AS NDistinct ",
    "  FROM candidates c JOIN cls k USING (DocID) ",
    "  WHERE c.Label IS NOT NULL GROUP BY k.Class, Combo, c.Label, c.DocID) ",
    "SELECT Class, Combo, Label, COUNT(*) AS Docs, SUM(N) AS Spans, ",
    "  median(NDistinct) AS DistinctPerDoc ",
    "FROM per GROUP BY Class, Combo, Label"
  )) |>
    tibble::as_tibble()

  pos_ <- DBI::dbGetQuery(con_, paste0(
    "SELECT k.Class, ", combo_, " AS Combo, c.Label, ",
    "  least(", n_ - 1L, ", CAST(floor((((c.Start + c.Stop) / 2.0) / l.DocLen) * ", n_,
    ") AS INTEGER)) AS Bin, COUNT(*) AS Spans ",
    "FROM candidates c JOIN lens l USING (DocID) JOIN cls k USING (DocID) ",
    "WHERE c.Label IS NOT NULL AND c.Start IS NOT NULL ",
    "GROUP BY k.Class, Combo, c.Label, Bin"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(Mid = (as.integer(.data$Bin) + 0.5) / n_) |>
    dplyr::mutate(Share = .data$Spans / sum(.data$Spans), .by = c(Class, Combo, Label)) |>
    ent_contrast(.by = c("Class", "Combo", "Label"))

  yield_ |>
    dplyr::left_join(pos_, by = dplyr::join_by(Class, Combo, Label)) |>
    dplyr::mutate(
      Docs           = as.integer(.data$Docs),
      Spans          = as.integer(.data$Spans),
      DistinctPerDoc = as.integer(.data$DistinctPerDoc)
    ) |>
    dplyr::select(Class, Combo, Label, Docs, Spans, DistinctPerDoc, Contrast)
}


#' What each engine found
#' @param .desc List from ent_describe().
#' @return Invisibly the yield tibble.
ent_report_yield <- function(.desc) {
  if (FALSE) .desc <- .desc

  cli::cli_h2("What each engine found")
  .desc$yield |>
    dplyr::arrange(plot_factor(.data$Label, .key = "Label"),
                   plot_factor(.data$Combo, .key = "Combo")) |>
    tbl_say(.title = "Spans and documents, per engine and label")
  cli::cli_alert_info(
    "Read DistinctPerDoc, not PerDoc. A contract names a few parties and a few places, so a \\
     distinct median in the tens or hundreds is an engine collecting mentions rather than entities. \\
     Absent rows are engines that do not emit that label, which is a property of the engine."
  )
  invisible(.desc$yield)
}

#' Where in the document each label sits
#' @param .desc List from ent_describe().
#' @return Invisibly the position tibble, widened to one row per engine and label.
ent_report_position <- function(.desc) {
  if (FALSE) .desc <- .desc

  wide_ <- .desc$position |>
    # Thirty bins are the right resolution for a curve and the wrong one for a console table, so
    # they roll up here rather than being computed twice.
    dplyr::mutate(Decile = pmin(9L, as.integer(floor(.data$Mid * 10)))) |>
    dplyr::summarise(Share = sum(.data$Share), .by = c(Combo, Label, Decile)) |>
    tidyr::pivot_wider(names_from = Decile, values_from = Share,
                       names_prefix = "D", values_fill = 0) |>
    dplyr::select(Combo, Label, dplyr::num_range("D", 0:9)) |>
    dplyr::arrange(plot_factor(.data$Label, .key = "Label"),
                   plot_factor(.data$Combo, .key = "Combo"))

  cli::cli_h2("Where in the document each label sits")
  wide_ |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("D"), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "Share of spans by decile of document length")
  cli::cli_alert_info(
    "Row percentages, so each engine and label is read against itself. Ten percent everywhere is a \\
     label scattered through the document; mass at D0 and D9 is one named in the preamble and again \\
     at the signature block, which is what a party looks like."
  )
  invisible(wide_)
}


#' Positional contrast by contract type, one label at a time
#'
#' Twelve classes down, engines across. One number per cell, so the grid can be read for two things
#' at once: which engine concentrates the label at the ends of the document, and whether it does so
#' consistently enough for a pooled decision to hold.
#'
#' @param .tab Tibble from ent_describe_class().
#' @param .label Character. Which label to show.
#' @return Invisibly the widened tibble.
ent_report_class <- function(.tab, .label) {
  if (FALSE) {
    .tab   <- tab_class
    .label <- "ORG"
  }

  wide_ <- .tab |>
    dplyr::filter(.data$Label == .label) |>
    dplyr::select(Class, Combo, Contrast) |>
    tidyr::pivot_wider(names_from = Combo, values_from = Contrast) |>
    dplyr::arrange(plot_factor(.data$Class, .key = "ClassDetailed"))

  cli::cli_h3("{(.label)}: positional contrast by contract type")
  wide_ |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(.x) round(.x, 2))) |>
    tbl_say(.title = paste0("Ends over middle -- ", .label))
  invisible(wide_)
}


#' Does the pooled engine verdict hold in every contract type
#'
#' The stability check, in two columns. An engine whose contrast runs 4.1 to 4.9 across classes can
#' be crowned pooled; one running 1.2 to 8.0 cannot, and needs either a caveat or a per-class rule.
#' Reported for every engine rather than for a shortlist, because an engine that looks poor pooled
#' and excellent on one class is exactly what a shortlist would have discarded unseen.
#'
#' @param .tab Tibble from ent_describe_class().
#' @param .min_docs Integer. Classes with fewer documents than this are excluded from the range,
#'   since a contrast over a handful of documents is a number about those documents.
#' @return Invisibly the stability tibble.
ent_report_stability <- function(.tab, .min_docs = 30L) {
  if (FALSE) {
    .tab      <- tab_class
    .min_docs <- 30L
  }

  out_ <- .tab |>
    dplyr::filter(.data$Docs >= .min_docs, !is.na(.data$Contrast)) |>
    dplyr::summarise(
      Classes     = dplyr::n(),
      MinContrast = min(.data$Contrast),
      MedContrast = stats::median(.data$Contrast),
      MaxContrast = max(.data$Contrast),
      MedDistinct = stats::median(.data$DistinctPerDoc),
      .by = c(Label, Combo)
    ) |>
    dplyr::mutate(Spread = .data$MaxContrast / pmax(.data$MinContrast, 0.01)) |>
    dplyr::arrange(plot_factor(.data$Label, .key = "Label"), dplyr::desc(.data$MedContrast))

  cli::cli_h2("Is the pooled verdict stable across contract types?")
  out_ |>
    dplyr::mutate(dplyr::across(c(MinContrast, MedContrast, MaxContrast, Spread),
                                \(.x) round(.x, 2))) |>
    tbl_say(.title = paste0("Contrast range over classes with at least ", .min_docs, " documents"))
  cli::cli_alert_info(
    "Spread is Max over Min. Near one means the engine behaves the same everywhere and the pooled \\
     number can be trusted. Large means the label is doing different jobs in different contract \\
     types, which is a reason to defer the choice rather than to pick the higher median."
  )
  invisible(out_)
}


#' The same document read by every engine that emits a label
#'
#' The block no summary replaces. A distinct-count of five against thirty-eight is a number; the two
#' LISTS side by side, from one contract, show which thirty-three the larger engine added, and
#' whether they are parties, referenced companies, or defined terms it mistook for names.
#'
#' Documents are drawn from the middle of the distribution rather than at random. The extremes are
#' unreadable and unrepresentative -- one contract in this sample yields 68,482 organisation spans --
#' and a block nobody reads is worse than no block.
#'
#' Spans are DISTINCT and in document order, so the head of each list is what the engine found at the
#' top of the contract, which is where parties are named. That makes the lists comparable line by
#' line rather than only by length.
#'
#' @param .db_path DuckDB candidate store.
#' @param .path_text Canonical text parquet.
#' @param .path_anchors Sample anchors parquet, supplying the contract type.
#' @param .label Character. Which label to show.
#' @param .n Integer. Documents drawn.
#' @param .max_spans Integer. Distinct spans listed per engine before truncation.
#' @param .opening Integer. Characters of the document opening shown for orientation.
#' @param .seed Integer. Draw seed, so the same documents appear on every render.
#' @return Tibble: DocID, Class, DocLen, Opening, Combo, NDistinct, Spans.
ent_engine_examples <- function(.db_path, .path_text, .path_anchors, .label,
                                .n = 5L, .max_spans = 8L, .opening = 190L, .seed = 42L) {
  if (FALSE) {
    .db_path      <- .lP$Store$NerDB
    .path_text    <- .lP$Sample$Text
    .path_anchors <- .lP$Sample$Anchors
    .label        <- "ORG"
    .n            <- 5L
    .max_spans    <- 8L
    .opening      <- 190L
    .seed         <- 42L
  }

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  combo_ <- "CASE WHEN c.Engine = c.Model THEN c.Engine ELSE c.Engine || ':' || c.Model END"

  # One row per engine per document, carrying the distinct spans in document order. The window
  # function ranks within engine and document so the list can be truncated at the head rather than
  # sampled, and everything stays database-side until the draw has narrowed it to a handful.
  per_ <- DBI::dbGetQuery(con_, paste0(
    "WITH d AS ( ",
    "  SELECT ", combo_, " AS Combo, c.DocID, upper(c.Span) AS Key, ",
    "    any_value(c.Span) AS Span, min(c.Start) AS Start ",
    "  FROM candidates c WHERE c.Label = '", .label, "' AND c.Start IS NOT NULL ",
    "  GROUP BY Combo, c.DocID, Key), ",
    "r AS (SELECT *, row_number() OVER (PARTITION BY Combo, DocID ORDER BY Start) AS Rank FROM d) ",
    "SELECT Combo, DocID, COUNT(*) AS NDistinct, ",
    "  string_agg(CASE WHEN Rank <= ", as.integer(.max_spans), " THEN Span END, ' | ' ",
    "             ORDER BY Start) AS Spans ",
    "FROM r GROUP BY Combo, DocID"
  )) |>
    tibble::as_tibble()

  if (nrow(per_) == 0L) {
    cli::cli_alert_warning("No {(.label)} spans in the store.")
    return(tibble::tibble())
  }

  # Documents every emitting engine saw, so the lists are comparable and a blank line means the
  # engine found nothing rather than that it was never asked.
  n_combo_ <- dplyr::n_distinct(per_$Combo)
  tot_ <- per_ |>
    dplyr::summarise(Combos = dplyr::n_distinct(.data$Combo), Tot = sum(.data$NDistinct),
                     .by = DocID) |>
    dplyr::filter(.data$Combos == n_combo_)

  # The middle two quartiles by total distinct spans: enough entities to tell the engines apart,
  # few enough to print.
  q_ <- stats::quantile(tot_$Tot, c(0.25, 0.75), na.rm = TRUE)
  pick_ <- tot_ |>
    dplyr::filter(.data$Tot >= q_[1], .data$Tot <= q_[2]) |>
    (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n, nrow(.d)))))()

  meta_ <- arrow::read_parquet(.path_anchors) |>
    dplyr::filter(.data$DocID %in% pick_$DocID) |>
    dplyr::select(DocID, Class = ClassDetailed)

  text_ <- arrow::read_parquet(.path_text) |>
    dplyr::filter(.data$DocID %in% pick_$DocID) |>
    dplyr::transmute(
      DocID,
      DocLen  = stringi::stri_length(.data$TextRaw),
      Opening = stringi::stri_replace_all_regex(
        stringi::stri_sub(.data$TextRaw, from = 1L, to = .opening), "\\s+", " "
      )
    )

  per_ |>
    dplyr::filter(.data$DocID %in% pick_$DocID) |>
    dplyr::left_join(meta_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(text_, by = dplyr::join_by(DocID)) |>
    dplyr::arrange(.data$DocID, dplyr::desc(.data$NDistinct)) |>
    dplyr::select(DocID, Class, DocLen, Opening, Combo, NDistinct, Spans)
}


#' Print the paired reads, one block per document
#'
#' Engines are ordered by how many distinct spans they returned, largest first, so the comparison
#' reads as a subtraction: the shortest list is the selective engine and everything above it is what
#' the others added.
#'
#' @param .tab Tibble from ent_engine_examples().
#' @param .label Character. Shown in the block header.
#' @param .width Integer. Characters of the span list printed before truncation.
#' @return Invisibly .tab.
ent_report_examples <- function(.tab, .label, .width = 96L) {
  if (FALSE) {
    .tab   <- tab_ex
    .label <- "ORG"
    .width <- 96L
  }
  if (nrow(.tab) == 0L) return(invisible(.tab))

  cli::cli_h3("{(.label)}: the same contract read by every engine")
  pad_ <- max(nchar(.tab$Combo))

  purrr::walk(unique(.tab$DocID), function(.d) {
    rows_ <- dplyr::filter(.tab, .data$DocID == .d)
    cat("-- ", .d, " | ", rows_$Class[1], " | ", format(rows_$DocLen[1], big.mark = ","),
        " chars\n", sep = "")
    cat("   opening : ", rows_$Opening[1], "\n", sep = "")
    purrr::pwalk(dplyr::select(rows_, Combo, NDistinct, Spans),
                 function(Combo, NDistinct, Spans) {
                   txt_ <- dplyr::coalesce(Spans, "")
                   if (nchar(txt_) > .width) txt_ <- paste0(substr(txt_, 1L, .width), " ...")
                   cat("   ", formatC(Combo, width = -pad_), " [", formatC(NDistinct, width = 4),
                       "] ", txt_, "\n", sep = "")
                 })
    cat("\n")
  })

  cli::cli_alert_info(
    "Read down each block as a subtraction. The shortest list is the selective engine; what the \\
     longer lists add is either a party the selective engine missed or a mention it was right to \\
     leave out, and only the words distinguish those."
  )
  invisible(.tab)
}


#' Every report block in this document, in order
#'
#' @param .tab_sample Tibble from ent_build_sample().
#' @param .ov List from ent_overview().
#' @param .desc List from ent_describe().
#' @param .class Tibble from ent_describe_class().
#' @param .al List from ent_alignment().
#' @param .tab_offsets Tibble from ent_check_offsets().
#' @return Invisibly NULL.
ent_report_all <- function(.tab_sample, .ov, .desc, .class, .al, .tab_offsets) {
  if (FALSE) {
    .tab_sample  <- tab_sample
    .ov          <- .ov
    .desc        <- .desc
    .class       <- tab_class
    .al          <- .al
    .tab_offsets <- tab_offsets
  }

  ent_report_sample(.tab = .tab_sample)
  ent_report_store(.ov = .ov)
  ent_report_yield(.desc = .desc)
  ent_report_position(.desc = .desc)
  ent_report_stability(.tab = .class)
  ent_report_agreement(.al = .al, .n = 12L)
  ent_report_offsets(.tab = .tab_offsets)
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

  # Ledger: documents run per combination, split by Status.
  #
  # DOCS IS DISTINCT AND THE STATUS COUNTS ARE NOT, because the ledger is keyed on document AND
  # label: a document run for four labels contributes four rows. Counting rows as documents would
  # report four times the sample size and would do so differently per engine, since the ragged grid
  # gives LexNLP four labels and the date regex one.
  ledger_ <- runs_ |>
    dplyr::group_by(Engine, Model) |>
    dplyr::summarise(
      Docs    = dplyr::n_distinct(DocID),
      Labels  = dplyr::n_distinct(Label),
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
ent_plot_positions <- function(.positions, .by_combo = TRUE, .free_y = TRUE) {
  if (FALSE) {
    .positions <- .desc$position
    .by_combo  <- TRUE
    .free_y    <- TRUE
  }
  if (is.null(.positions) || nrow(.positions) == 0L) {
    cli::cli_abort("No positions to plot; {.arg .positions} is ent_describe()$position.")
  }

  # Already binned, and already shared within engine and label, by ent_describe(). Re-deriving the
  # share here would be a second definition of the same quantity, and the two would drift.
  dat_ <- if (.by_combo) {
    dplyr::mutate(.positions, PlotLabel = plot_factor(.data$Label, .key = "Label"))
  } else {
    .positions |>
      dplyr::summarise(Spans = sum(.data$Spans), .by = c(Label, Bin, Mid)) |>
      dplyr::mutate(Share = .data$Spans / sum(.data$Spans), .by = Label) |>
      dplyr::mutate(PlotLabel = plot_factor(.data$Label, .key = "Label"))
  }

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
