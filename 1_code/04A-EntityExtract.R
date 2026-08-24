# ======================================================================================================================
# 04A-EntityExtract.R -- library for 04A-EntityExtract.qmd
# ======================================================================================================================
#
# Sample construction, the pass over each family, and the reports 04A prints. Everything that talks
# to an extractor or to a candidate store lives in _Commons/_NER.R and is sourced before this file.
#
# WHAT THIS SCRIPT OWNS. The canonical text and three candidate databases. Both are read by 04B and
# by 04C, so their paths are a contract even though the reports in the document are not.
#
# THIS FILE IS NOT INERT. It registers two vocabularies against _Plots.R's level registry at LOAD
# time, so it must be sourced after _Plots.R -- which rebuilds that registry empty every time it is
# sourced. Any probe or script that sources this library outside a render must follow the document's
# order or it is not seeing the render's environment. A file's top-level statements are part of its
# interface.


# 0. The plot vocabularies -----------------------------------------------------------------------------------------------
#
# Two keys, registered once so that an entity figure and a classification figure are the same object
# drawn from different data: same fonts, same palette discipline, same ordering everywhere.
#
# PRODUCER IS FAMILY-GRAINED EXCEPT FOR SPACY, and that asymmetry is the store's, not a choice made
# here. Within one panel matcon can only appear once -- DATE comes from dateregex and GPE from the
# gazetteer, never both -- so naming the module would add a distinction no panel can show. spaCy is
# the one family where several models can compete for the same entity, so its models stay separate.

.ent_entities <- c("ORG", "PERSON", "GPE", "DATE", "MONEY", "REDACT", "TERM")

.ent_producers <- c(
  "spacy:sm", "spacy:md", "spacy:lg", "spacy:trf",
  "lexnlp",
  "matcon"
)

# WHICH PRODUCERS ACTUALLY HAVE TO BE TOLD APART is a narrower question than it looks, and the answer
# is what this palette is built on. Only spaCy and LexNLP are multi-entity, and those are the two
# that genuinely compete. matcon's rules are entity specialists, so matcon never needs separating
# from itself within a panel.
#
# THREE HUES FOR THREE FAMILIES, and the first version got this wrong by reasoning about the wrong
# comparison. It gave matcon a dark grey on the grounds that matcon never needs separating from
# itself -- true, and beside the point. What matcon always needs separating from is spaCy, and
# #4A4A4A against the transformer's #002147 rendered two overlaid lines in the GPE panel that could
# not be told apart.
#
# So: a blue ramp for spaCy ordered by capacity, the house ochre for LexNLP, and a teal for matcon.
# Three distinct hues, each legible against the other two at line weight. An earlier version put the
# rule engines on blues, where they read as further spaCy models; a grey is the same mistake with
# less saturation.
.ent_producer_colours <- c(
  "#bfd7ed", # spacy:sm   -- lightest of the CNN ramp
  "#60a3d9", # spacy:md
  "#0074b7", # spacy:lg
  "#002147", # spacy:trf  -- darkest, and the only transformer
  "#B7791F", # lexnlp     -- the one warm mark in any figure
  "#2E7D6F"  # matcon     -- the rules, distinct from both ramps at line weight
)

plot_register_levels(
  .key     = "Entity",
  .levels  = .ent_entities,
  .short   = NULL,                                  # already short; the full name IS the entity
  .colours = plot_pal_cat(length(.ent_entities))
)

plot_register_levels(
  .key     = "Producer",
  .levels  = .ent_producers,
  .short   = .ent_producers,                        # short and full coincide; kept for the API
  .colours = .ent_producer_colours                  # explicit: assigned by family, not by position
)


# 1. Sample --------------------------------------------------------------------------------------------------------------

#' Build the labelled sample this family works on
#'
#' Reads 03A's prepared sample, which is the single file every downstream method in the monorepo
#' consumes. Drawing from it rather than from the raw corpus means the entity work is scored on
#' exactly the partition the classifiers were scored on, joins to the same folds, and orders contract
#' types the same way in every table.
#'
#' THE TEXT COLUMN IS RENAMED, DELIBERATELY. 03A calls it `Text`; everything in the 04 family calls
#' it `TextRaw`, which is also what every extractor defaults to. Renaming here rather than passing a
#' column name through five layers means one place decides what the canonical text is called, and the
#' extractors need no configuration.
#'
#' TEXT IS COPIED, NOT REFERENCED. Every offset in every store indexes THIS file. If the text a span
#' points into can change -- because 03A re-read the corpus, or normalised it differently -- then an
#' offset is a promise nobody can keep.
#'
#' @param .path_prepared 03A's prepared.parquet.
#' @param .text_col Text column in that file.
#' @return Tibble carrying every column 03A wrote, with the text column renamed to TextRaw.
ent_build_sample <- function(.path_prepared, .text_col = "Text") {
  if (FALSE) {
    .path_prepared <- .lP$Input$Prepared
    .text_col      <- "Text"
  }

  tab_ <- arrow::read_parquet(.path_prepared)

  if (!.text_col %in% names(tab_)) {
    cli::cli_abort(c(
      "{fs::path_file(.path_prepared)} has no column {(.text_col)}.",
      "i" = "Columns present: {paste(names(tab_), collapse = ', ')}"
    ))
  }

  out_ <- tab_ |>
    dplyr::rename(TextRaw = dplyr::all_of(.text_col)) |>
    dplyr::filter(!is.na(.data$TextRaw), nchar(.data$TextRaw) > 0L)

  drop_ <- nrow(tab_) - nrow(out_)
  if (drop_ > 0L) {
    cli::cli_warn("{drop_} document{?s} carried no text and {?was/were} dropped from the sample.")
  }
  if (nrow(out_) == 0L) cli::cli_abort("The sample is empty; check {(.path_prepared)}.")

  out_
}

#' Write the canonical text, only where its content has changed
#'
#' GUARDED, because every offset in three databases indexes this file. A rewrite that changes nothing
#' leaves the mtime untouched, so nothing downstream keyed on it can be tripped by a no-op render;
#' a rewrite that changes something invalidates every stored offset and must be loud about it.
#'
#' The comparison is on OBJECTS, not bytes: arrow does not guarantee byte-identical parquet for
#' identical input, so a file hash reports changes that are not changes.
#'
#' @param .tab The sample, carrying DocID and TextRaw.
#' @param .path Destination.
#' @return .path, invisibly.
ent_write_sample <- function(.tab, .path) {
  if (FALSE) {
    .tab  <- tab_sample
    .path <- .lP$Output$Sample
  }

  new_ <- dplyr::select(.tab, "DocID", "TextRaw")

  if (fs::file_exists(.path)) {
    old_ <- arrow::read_parquet(.path)
    if (identical(as.data.frame(old_), as.data.frame(new_))) {
      cli::cli_alert_success(
        "Unchanged, not rewritten: {fs::path_rel(.path, start = here::here())} \\
         ({format(nrow(new_), big.mark = ',')} {cli::qty(nrow(new_))}document{?s}, mtime preserved)."
      )
      return(invisible(.path))
    }
    cli::cli_alert_warning(
      "The canonical text has CHANGED. Every offset in every store indexes it, so the stores are \\
       stale until they are cleared and re-extracted."
    )
  }

  arrow::write_parquet(new_, .path)
  cli::cli_alert_success(
    "Wrote {fs::path_rel(.path, start = here::here())} \\
     ({format(nrow(new_), big.mark = ',')} {cli::qty(nrow(new_))}document{?s})."
  )
  invisible(.path)
}

#' Report the sample
#'
#' @param .tab Output of ent_build_sample().
#' @param .class_col Contract-type column to break down by, or NULL.
#' @return .tab, invisibly.
ent_report_sample <- function(.tab, .class_col = "ClassDetailed") {
  if (FALSE) {
    .tab       <- tab_sample
    .class_col <- "ClassDetailed"
  }

  n_ <- nrow(.tab)
  cli::cli_alert_info(
    "{format(n_, big.mark = ',')} {cli::qty(n_)}document{?s}, \\
     {format(sum(nchar(.tab$TextRaw)), big.mark = ',')} characters."
  )

  len_ <- tibble::tibble(
    Metric = c("Min", "Median", "Mean", "P90", "Max"),
    Chars  = as.integer(c(
      min(nchar(.tab$TextRaw)),
      stats::median(nchar(.tab$TextRaw)),
      round(mean(nchar(.tab$TextRaw))),
      stats::quantile(nchar(.tab$TextRaw), 0.9),
      max(nchar(.tab$TextRaw))
    ))
  )
  tbl_say(.tab = len_, .title = "Document length")

  if (!is.null(.class_col) && .class_col %in% names(.tab)) {
    cls_ <- .tab |>
      dplyr::count(Class = .data[[.class_col]], name = "NDoc") |>
      dplyr::mutate(Share = round(.data$NDoc / sum(.data$NDoc), 3)) |>
      dplyr::arrange(dplyr::desc(.data$NDoc))
    tbl_say(.tab = cls_, .title = "Contract types")
  }

  invisible(.tab)
}


# 2. The plan ------------------------------------------------------------------------------------------------------------

#' What each family is asked for, and which models actually run
#'
#' THE PLAN IS DECLARED HERE AND CHECKED AGAINST THE DESCRIPTION, rather than derived from it.
#' Everything a family can produce is not the same question as what this pass asks for: LexNLP's GPE
#' is its geoentity pass and costs more than its other three extractors combined, so asking for it is
#' a decision rather than a default.
#'
#' `Run` records the models excluded and why, instead of deleting them. A table that says three spaCy
#' models were declared and not run answers a referee's question; a table that never mentions them
#' invites it.
#'
#' @param .describe Output of ner_describe().
#' @param .plan Tibble: Family, Entities (list) -- what each family is asked for.
#' @param .skip_models Model names to declare but not run.
#' @return Tibble: Family, Model, Entities (list), Ready, Run, Note.
ent_plan <- function(.describe, .plan, .skip_models = character()) {
  if (FALSE) {
    .describe     <- tab_describe
    .plan         <- .lP$Plan
    .skip_models  <- .lP$Params$SkipModels
  }

  ask_ <- .plan |>
    tidyr::unnest_longer(col = "Entities", values_to = "Entity")

  # THE REASON TRAVELS WITH THE ROW. An earlier version wrote "not installed" for every unready
  # model, which was wrong for the case that actually occurred: the Docker daemon was not running,
  # and nothing about that is fixed by installing anything.
  .describe |>
    dplyr::inner_join(ask_, by = dplyr::join_by(Family, Entity)) |>
    dplyr::summarise(
      Entities = list(sort(unique(.data$Entity))),
      Ready    = all(.data$Ready),
      Why      = dplyr::first(.data$Note),
      .by      = c("Family", "Model")
    ) |>
    dplyr::mutate(
      Run  = .data$Ready & !.data$Model %in% .skip_models,
      Note = dplyr::case_when(
        !.data$Ready                  ~ .data$Why,
        .data$Model %in% .skip_models ~ "declared, not run",
        .default                      = ""
      )
    ) |>
    dplyr::select("Family", "Model", "Entities", "Ready", "Run", "Note") |>
    dplyr::arrange(.data$Family, .data$Model)
}

#' Report the plan
#'
#' @param .tab Output of ent_plan().
#' @return .tab, invisibly.
ent_report_plan <- function(.tab) {
  if (FALSE) {
    .tab <- tab_plan
  }

  show_ <- .tab |>
    dplyr::transmute(
      .data$Family, .data$Model,
      Entities = purrr::map_chr(.data$Entities, \(.x) paste(.x, collapse = ", ")),
      .data$Run, .data$Note
    )
  tbl_say(.tab = show_, .title = "What runs, and what is declared but does not")

  n_ <- sum(.tab$Run)
  cli::cli_alert_info(
    "{(n_)} model{?s} will run. A model listed with Run = FALSE is recorded rather than deleted, \\
     so the comparison this document reports says which models it covered and which it did not."
  )

  # DELIBERATELY EXCLUDED AND UNAVAILABLE ARE DIFFERENT FINDINGS. The first is a decision this
  # document made and argues for; the second is a gap in the evidence, and pooling them would let a
  # missing family read as a considered choice.
  gone_ <- dplyr::filter(.tab, !.data$Ready)
  if (nrow(gone_) > 0L) {
    cli::cli_alert_warning(
      "{nrow(gone_)} model{?s} {?is/are} unavailable rather than excluded, so every table below \\
       is missing {?it/them}: {paste(unique(gone_$Family), collapse = ', ')}."
    )
  }

  invisible(.tab)
}


#' The benchmark's key for every model that can run right now
#'
#' TWO KEY SPACES HAVE TO BE RECONCILED, and passing the plan's Model column straight through would
#' not do it. The benchmark keys a family that versions itself by the FAMILY -- matcon's timings are
#' stored under "matcon" -- while the plan keys the same rows by module, "dateregex-v3" and the rest.
#' Comparing the two directly would report matcon as unavailable on every render.
#'
#' @param .plan Output of ent_plan().
#' @return Character vector of benchmark model keys.
ent_live_keys <- function(.plan) {
  if (FALSE) {
    .plan <- tab_plan
  }
  .plan |>
    dplyr::filter(.data$Run) |>
    dplyr::mutate(Key = dplyr::if_else(.data$Family == "spacy", .data$Model, .data$Family)) |>
    dplyr::pull(.data$Key) |>
    unique()
}


# 3. Extraction ------------------------------------------------------------------------------------------------------------

#' Run one family over the sample and ingest what it produced
#'
#' EXTRACTION AND INGEST ARE SEPARATE CALLS, joined here rather than fused. The benchmark runs
#' extraction alone and discards the output; fusing them would make timing impossible without
#' polluting the store.
#'
#' THE LEDGER MAKES RE-RENDERING CHEAP. A (model, entity) whose documents are all recorded is skipped
#' entirely, so a family that takes an hour on its first render costs seconds on the next. That is
#' also why no chunk in this document needs a switch: expense is handled by knowing what is done, not
#' by turning work off.
#'
#' @param .con Connection to this family's database.
#' @param .family Family name.
#' @param .plan Output of ent_plan(), filtered or not -- rows for other families are ignored.
#' @param .path_in The canonical text.
#' @param .doc_ids Documents the ledger is asked about.
#' @param .stage_dir Where the family writes its parquets.
#' @param .describe Output of ner_describe().
#' @param .workers Worker processes.
#' @param .batch_size Documents per unit of work.
#' @param .timeout Per-document cap in seconds.
#' @param .force Re-run even where the ledger records a model as complete.
#' @return Tibble: Family, Model, Entity, NTodo, Ran, NSpan, Seconds.
ent_extract_family <- function(.con, .family, .plan, .path_in, .doc_ids, .stage_dir, .describe,
                               .workers = .ner_workers, .batch_size = .ner_batch_size,
                               .timeout = .ner_timeout, .force = FALSE) {
  if (FALSE) {
    .con        <- con_matcon
    .family     <- "matcon"
    .plan       <- tab_plan
    .path_in    <- .lP$Output$Sample
    .doc_ids    <- tab_sample$DocID
    .stage_dir  <- .lP$Output$Stage
    .describe   <- tab_describe
    .workers    <- 5L
    .batch_size <- 64L
    .timeout    <- 120L
    .force      <- FALSE
  }

  rows_ <- dplyr::filter(.plan, .data$Family == .family, .data$Run)
  if (nrow(rows_) == 0L) {
    why_ <- dplyr::filter(.plan, .data$Family == .family, !.data$Run)
    cli::cli_alert_warning(
      "Nothing to run for {(.family)}: \\
       {paste(unique(why_$Note[nchar(why_$Note) > 0L]), collapse = '; ')}"
    )
    cli::cli_alert_info(
      "The plan is built from ner_describe(), which ran earlier. If the environment has changed \\
       since, re-run the describe and plan chunks before this one."
    )
    return(tibble::tibble())
  }

  fs::dir_create(.stage_dir)
  out_ <- tibble::tibble()

  # spaCy is the only family with several models, and each is its own subprocess. For the other two
  # the loop runs once and the model tag is whatever the extractor stamps.
  models_ <- if (.family == "spacy") rows_$Model else NA_character_
  ask_    <- sort(unique(unlist(rows_$Entities)))

  for (m_ in models_) {
    tag_ <- if (is.na(m_)) .family else m_

    # SKIP ON THE LEDGER, NOT ON THE FILE. A staged parquet on disk proves a run happened, not that
    # it covered these documents; the ledger is the only thing that answers per document.
    #
    # The model tags a run WILL stamp are the plan's, so the ledger is asked about those rather than
    # about the family. For matcon that is several tags for one call, and a single outstanding tag
    # is enough to make the call worth making.
    tags_ <- if (.family == "spacy") m_ else dplyr::filter(rows_, .data$Family == .family)$Model
    todo_ <- if (.force) {
      .doc_ids
    } else {
      tidyr::expand_grid(Model = tags_, Entity = ask_) |>
        dplyr::filter(purrr::map2_lgl(
          .data$Model, .data$Entity,
          \(.t, .e) .e %in% .describe$Entity[.describe$Model == .t]
        )) |>
        purrr::pmap(\(Model, Entity) ner_db_missing(
          .con = .con, .doc_ids = .doc_ids, .model = Model, .entity = Entity
        )) |>
        purrr::reduce(union, .init = character(0))
    }

    if (length(todo_) == 0L) {
      cli::cli_alert_success("{(tag_)} -- complete, skipped.")
      out_ <- dplyr::bind_rows(out_, tibble::tibble(
        Family = .family, Model = tag_, Entity = ask_,
        NTodo = 0L, Ran = FALSE, NSpan = NA_integer_, Seconds = NA_real_
      ))
      next
    }

    cli::cli_alert_info(
      "{(tag_)} -- {format(length(todo_), big.mark = ',')} \\
       {cli::qty(length(todo_))}document{?s} to do."
    )

    staged_ <- ner_extract(
      .family     = .family,
      .model      = if (.family == "spacy") m_ else NULL,
      .entity     = ask_,
      .path_in    = .path_in,
      .out_dir    = .stage_dir,
      .describe   = .describe,
      .workers    = .workers,
      .batch_size = .batch_size,
      .timeout    = .timeout,
      .quiet      = FALSE
    )

    got_ <- ner_ingest(.con = .con, .staged = staged_, .entity = ask_)

    out_ <- dplyr::bind_rows(out_, got_ |>
      dplyr::transmute(
        Family = .family, .data$Model, .data$Entity,
        NTodo   = length(todo_),
        Ran     = TRUE,
        .data$NSpan,
        Seconds = staged_$Seconds[[1L]]
      ))
  }

  out_
}

#' Report what one family's pass did
#'
#' @param .tab Output of ent_extract_family().
#' @return .tab, invisibly.
ent_report_extract <- function(.tab) {
  if (FALSE) {
    .tab <- tab_run_matcon
  }

  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("Nothing ran and nothing was skipped; the plan was empty.")
    return(invisible(.tab))
  }

  tbl_say(.tab = .tab, .title = "What ran, and what was skipped")

  ran_ <- dplyr::filter(.tab, .data$Ran)
  if (nrow(ran_) == 0L) {
    cli::cli_alert_success(
      "Nothing to do: every document is already recorded for every model and entity."
    )
  } else {
    secs_ <- sum(unique(dplyr::select(ran_, "Model", "Seconds"))$Seconds, na.rm = TRUE)
    cli::cli_alert_success("Extraction took {round(secs_ / 60, 1)} minutes.")
  }

  invisible(.tab)
}


# 4. Validation ------------------------------------------------------------------------------------------------------------

#' The offset contract, on every span in one database
#'
#' THE INVARIANT EVERYTHING RESTS ON. text[Start:Stop] == Span, over code points.
#' stringi::stri_sub because base substr indexes BYTES and misaligns roughly ninety-nine per cent of
#' spans on this corpus, so getting this wrong is not a subtle error.
#'
#' Checked per entity table rather than through a union view, because the tables no longer share a
#' schema and there is no view to check through.
#'
#' @param .con Connection, read-only.
#' @param .tab_text The sample, carrying DocID and TextRaw.
#' @param .n Documents to sample, or NULL for all of them.
#' @return Tibble of failures; empty is a pass.
ent_check_offsets <- function(.con, .tab_text, .n = NULL) {
  if (FALSE) {
    .con      <- con_matcon
    .tab_text <- tab_sample
    .n        <- NULL
  }

  txt_ <- if (is.null(.n)) .tab_text else dplyr::slice_sample(.tab_text, n = .n)
  txt_ <- dplyr::select(txt_, "DocID", "TextRaw")

  tabs_ <- ner_db_tables(.con = .con)
  if (length(tabs_) == 0L) return(tibble::tibble())

  purrr::map(tabs_, \(.t) {
    DBI::dbGetQuery(.con, glue::glue(
      "SELECT DocID, Start, Stop, Span FROM {.t} WHERE Start IS NOT NULL"
    )) |>
      tibble::as_tibble() |>
      dplyr::mutate(Entity = toupper(.t))
  }) |>
    purrr::list_rbind() |>
    dplyr::inner_join(txt_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(Cut = stringi::stri_sub(.data$TextRaw, .data$Start + 1L, .data$Stop)) |>
    dplyr::filter(.data$Cut != .data$Span) |>
    dplyr::select("DocID", "Entity", "Start", "Stop", "Span", "Cut")
}

#' Every sample document has an outcome for every model and entity that was asked for
#'
#' A document missing from the ledger for one entity is invisible in any span count, because a
#' document with no spans and a document never processed produce the same empty result.
#'
#' @param .con Connection, read-only.
#' @param .doc_ids Sample document identifiers.
#' @param .plan Output of ent_plan(), for this family.
#' @param .describe Output of ner_describe().
#' @return Tibble: Model, Entity, NMissing.
ent_check_ledger <- function(.con, .doc_ids, .plan, .describe) {
  if (FALSE) {
    .con      <- con_matcon
    .doc_ids  <- tab_sample$DocID
    .plan     <- dplyr::filter(tab_plan, .data$Family == "matcon")
    .describe <- tab_describe
  }

  want_ <- .plan |>
    dplyr::filter(.data$Run) |>
    tidyr::unnest_longer(col = "Entities", values_to = "Entity") |>
    dplyr::select("Family", "Model", "Entity")

  # For matcon the plan's Model column already names the module that owns each entity, because
  # ent_plan() built it by joining the description. So no second lookup is needed here.
  want_ |>
    dplyr::mutate(NMissing = purrr::map2_int(
      .data$Model, .data$Entity,
      \(.m, .e) length(ner_db_missing(.con = .con, .doc_ids = .doc_ids, .model = .m, .entity = .e))
    )) |>
    dplyr::select("Model", "Entity", "NMissing")
}

#' Run both validations for one family and report them as one block
#'
#' The single block to copy out when something needs checking. Both abort rather than warn: an
#' offset that does not round-trip makes every rule downstream meaningless, and a ledger gap makes
#' every count downstream wrong in an invisible direction.
#'
#' AN EMPTY STORE IS NOT A PASS, and the first version of this reported one as two green ticks.
#' ent_check_offsets() on a store with no entity tables returns an empty tibble, and
#' ent_check_ledger() on a plan with nothing to run returns an empty one, so `nrow() == 0` and
#' `all(... == 0L)` were both satisfied by having nothing to check. That is the "a check that cannot
#' fail is not a check" pattern with a tick attached, in the section a reader trusts most.
#'
#' So both halves now report the SIZE of what they checked, and a store with nothing in it says so
#' instead of passing.
#'
#' @param .con Connection, read-only.
#' @param .family Family name, for the messages.
#' @param .tab_text The sample.
#' @param .plan Output of ent_plan().
#' @param .describe Output of ner_describe().
#' @return List of the individual results, invisibly.
ent_report_validation <- function(.con, .family, .tab_text, .plan, .describe) {
  if (FALSE) {
    .con      <- con_matcon
    .family   <- "matcon"
    .tab_text <- tab_sample
    .plan     <- tab_plan
    .describe <- tab_describe
  }

  cli::cli_h3(paste0("Validation -- ", .family))

  tabs_  <- ner_db_tables(.con = .con)
  nspan_ <- if (length(tabs_) == 0L) {
    0L
  } else {
    sum(purrr::map_int(tabs_, \(.t) as.integer(
      DBI::dbGetQuery(.con, glue::glue("SELECT COUNT(*) AS n FROM {.t}"))$n[[1L]]
    )))
  }
  nled_ <- as.integer(DBI::dbGetQuery(.con, "SELECT COUNT(*) AS n FROM runs")$n[[1L]])

  if (nspan_ == 0L && nled_ == 0L) {
    cli::cli_alert_warning(
      "The {(.family)} store is empty, so NOTHING WAS CHECKED. This is not a pass."
    )
    why_ <- dplyr::filter(.plan, .data$Family == .family, !.data$Run)
    if (nrow(why_) > 0L) {
      cli::cli_alert_info(
        "{(.family)} did not run: \\
         {paste(unique(why_$Note[nchar(why_$Note) > 0L]), collapse = '; ')}"
      )
    }
    return(invisible(list(Offsets = tibble::tibble(), Ledger = tibble::tibble())))
  }

  off_ <- ent_check_offsets(.con = .con, .tab_text = .tab_text)
  if (nrow(off_) == 0L) {
    cli::cli_alert_success(
      "Offset contract holds on all {format(nspan_, big.mark = ',')} \\
       {cli::qty(nspan_)}span{?s} in the {(.family)} store."
    )
  } else {
    tbl_say(.tab = utils::head(off_, 20L), .title = "OFFSET FAILURES")
    cli::cli_abort(
      "{nrow(off_)} span{?s} in {(.family)} do not round-trip. Nothing downstream can be trusted."
    )
  }

  led_ <- ent_check_ledger(
    .con      = .con,
    .doc_ids  = .tab_text$DocID,
    .plan     = dplyr::filter(.plan, .data$Family == .family),
    .describe = .describe
  )
  if (nrow(led_) == 0L) {
    cli::cli_alert_warning(
      "The {(.family)} store holds {format(nled_, big.mark = ',')} ledger \\
       {cli::qty(nled_)}row{?s} but the plan asks for nothing, so completeness was NOT checked."
    )
  } else if (all(led_$NMissing == 0L)) {
    cli::cli_alert_success(
      "All {format(nrow(.tab_text), big.mark = ',')} documents reached each of \\
       {nrow(led_)} model-and-entity combination{?s} {(.family)} was asked for."
    )
  } else {
    tbl_say(.tab = dplyr::filter(led_, .data$NMissing > 0L), .title = "INCOMPLETE COMBINATIONS")
    cli::cli_abort("{sum(led_$NMissing > 0L)} combination{?s} did not cover every document.")
  }

  invisible(list(Offsets = off_, Ledger = led_))
}


# 5. Report: what the three stores hold --------------------------------------------------------------------------------

#' Coverage: the share of sample documents in which a model found at least one span of an entity
#'
#' NOT A QUALITY MEASURE, and it should not be read as one: an engine tagging every capitalised word
#' as an organisation reaches complete coverage and is useless. What it establishes is which
#' comparisons are possible at all, since two engines can only be compared where both fire.
#'
#' TIMEOUTS AND ERRORS ARE REPORTED APART FROM CLEAN MISSES. A document processed and matched is a
#' coverage fact; a document whose extractor crashed is a defect, and pooling the two hides it.
#'
#' @param .summary Output of ner_db_summary().
#' @param .family Family name, added as a column.
#' @param .n_doc Denominator: documents in the sample.
#' @return Tibble: Family, Model, Entity, NSpan, NDoc, Coverage, nTimeout, nError.
ent_coverage <- function(.summary, .family, .n_doc) {
  if (FALSE) {
    .summary <- ner_db_summary(.con = con_matcon)
    .family  <- "matcon"
    .n_doc   <- nrow(tab_sample)
  }

  if (nrow(.summary) == 0L) return(tibble::tibble())

  .summary |>
    dplyr::transmute(
      Family   = .family,
      .data$Model, .data$Entity, .data$NSpan,
      NDoc     = .data$nHit,
      Coverage = round(.data$nHit / .n_doc, 3),
      .data$nTimeout, .data$nError
    ) |>
    dplyr::arrange(.data$Entity, .data$Model)
}

#' Report coverage across every family, in one table
#'
#' NAMES WHAT IS ABSENT, because a family that never ran contributes no rows and a table cannot show
#' the reader something that is not in it. A missing family looks exactly like a family that found
#' nothing, and the two are opposite findings.
#'
#' @param .tab Bound output of ent_coverage() for all three families.
#' @param .plan Output of ent_plan(), so the table can be compared against what was intended.
#' @return .tab, invisibly.
ent_report_coverage <- function(.tab, .plan) {
  if (FALSE) {
    .tab  <- tab_coverage
    .plan <- tab_plan
  }

  tbl_say(.tab = .tab, .title = "Coverage, spans and failures by family, model and entity")

  want_ <- dplyr::filter(.plan, .data$Run)
  gap_  <- dplyr::anti_join(
    dplyr::distinct(want_, .data$Family, .data$Model),
    dplyr::distinct(.tab, .data$Family, .data$Model),
    by = dplyr::join_by(Family, Model)
  )
  if (nrow(gap_) > 0L) {
    tbl_say(.tab = gap_, .title = "PLANNED BUT ABSENT FROM THIS TABLE")
    cli::cli_alert_warning(
      "{nrow(gap_)} model{?s} {?was/were} planned and produced no rows at all. That is not a \\
       coverage of zero; it is an absence, and every comparison below is missing {?it/them}."
    )
  }

  bad_ <- dplyr::filter(.tab, .data$nTimeout > 0L | .data$nError > 0L)
  if (nrow(bad_) == 0L) {
    cli::cli_alert_success("No timeouts and no extractor errors anywhere.")
  } else {
    cli::cli_alert_warning(
      "{nrow(bad_)} combination{?s} recorded a timeout or an error. A crash and a clean miss \\
       produce the same empty result, which is why they are counted separately."
    )
  }

  invisible(.tab)
}

# 5b. Which producer reaches which entity, and where two of them meet ----------------------------------------------------

#' Coverage as a producer-by-entity grid
#'
#' THE SHAPE THE QUESTION IS ACTUALLY ASKED IN. The long table says what each combination did; this
#' says which combinations exist, which is what a reader scans for before anything else.
#'
#' A DASH IS NOT A ZERO, and the distinction is the whole reason this needs a note. An empty cell
#' means that producer does not attempt that entity; a cell reading 0.000 would mean it attempted and
#' never fired. Pivoting long to wide manufactures missing values that look identical to real ones,
#' which is precisely the defect 03E found in its own panel.
#'
#' @param .coverage Bound output of ent_coverage().
#' @return Tibble: Entity, then one column per producer.
ent_producer_grid <- function(.coverage) {
  if (FALSE) {
    .coverage <- tab_coverage
  }

  if (nrow(.coverage) == 0L) return(tibble::tibble())

  .coverage |>
    dplyr::mutate(Producer = dplyr::if_else(
      .data$Family == "spacy",
      paste0("spacy:", stringi::stri_replace_first_fixed(.data$Model, "en_core_web_", "")),
      .data$Family
    )) |>
    dplyr::select("Entity", "Producer", "Coverage") |>
    tidyr::pivot_wider(names_from = "Producer", values_from = "Coverage") |>
    dplyr::arrange(.data$Entity)
}

#' Report the grid and say how many producers each entity has
#'
#' THE DASH IS RENDERED, NOT LEFT AS NA. The prose beneath this table promises an empty cell and the
#' first version printed `NA`, which is the same word R uses for a missing measurement -- exactly the
#' ambiguity the note exists to remove. Formatting happens here rather than in ent_producer_grid(),
#' whose output stays numeric for anything downstream that wants to compute with it.
#'
#' @param .grid Output of ent_producer_grid().
#' @return .grid, invisibly, unformatted.
ent_report_grid <- function(.grid) {
  if (FALSE) {
    .grid <- tab_grid
  }

  if (nrow(.grid) == 0L) {
    cli::cli_alert_warning("No coverage rows, so there is no grid to show.")
    return(invisible(.grid))
  }

  show_ <- .grid |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      \(.x) dplyr::if_else(is.na(.x), "-", formatC(.x, format = "f", digits = 3))
    ))
  tbl_say(.tab = show_, .title = "Coverage by entity and producer")

  n_ <- .grid |>
    dplyr::rowwise() |>
    dplyr::mutate(NProducer = sum(!is.na(dplyr::c_across(-"Entity")))) |>
    dplyr::ungroup() |>
    dplyr::select("Entity", "NProducer") |>
    dplyr::arrange(dplyr::desc(.data$NProducer), .data$Entity)

  tbl_say(.tab = n_, .title = "How many producers reach each entity")
  cli::cli_alert_info(
    "A dash means that producer does not attempt that entity. It is not a coverage of zero, and \\
     only an entity reached by two or more producers can be compared at all."
  )

  invisible(.grid)
}

#' Which entities have more than one producer
#'
#' @param .tables Output of ner_attached_tables().
#' @return Tibble: Entity, NFamily, Families.
ent_producers <- function(.tables) {
  if (FALSE) {
    .tables <- tab_tables
  }
  if (nrow(.tables) == 0L) return(tibble::tibble())

  .tables |>
    dplyr::filter(.data$NSpan > 0L) |>
    dplyr::summarise(
      NFamily  = dplyr::n(),
      Families = paste(sort(.data$Family), collapse = ", "),
      .by      = "Entity"
    ) |>
    dplyr::arrange(dplyr::desc(.data$NFamily), .data$Entity)
}

#' Every producer's spans in one table, and overlapping spans merged into mentions
#'
#' THE STEP THAT MAKES AGREEMENT MEASURABLE AT ALL. Comparing engines on exact offsets is
#' misleading, and there is direct evidence: a single-sentence probe returned the LexNLP DATE span
#' "of January 15, 2019 by" where the regex engine returns "January 15, 2019". Both found the same
#' date. On identical offsets they agree on nothing, and a figure built from that would report two
#' engines that disagree completely.
#'
#' So overlapping spans within a document and entity are merged into a MENTION -- a place in the
#' text where something was found -- and agreement is asked at that level: how many producers found
#' this mention. Boundary disagreement stops being noise and becomes a separate, reportable
#' quantity.
#'
#' Gaps-and-islands over half-open spans: a new mention starts where a span begins at or after the
#' running maximum end of every prior span in the ordered group. Done in SQL because the window
#' functions are the whole algorithm and R would need the spans in memory to do the same thing.
#'
#' @param .con Connection from ner_attach().
#' @param .tables Output of ner_attached_tables().
#' @param .spacy_models spaCy models to include; each becomes its own producer.
#' @return .con, invisibly. Creates temp tables ent_spans and ent_mentions.
ent_build_mentions <- function(.con, .tables, .spacy_models = "en_core_web_trf") {
  if (FALSE) {
    .con          <- con_all
    .tables       <- tab_tables
    .spacy_models <- "en_core_web_trf"
  }

  have_ <- dplyr::filter(.tables, .data$NSpan > 0L)
  if (nrow(have_) == 0L) cli::cli_abort("No spans in any attached database.")

  # One SELECT per (family, entity), and per spaCy MODEL where that family is involved, because the
  # spaCy store keeps Model on the row and pooling several models would count one implementation
  # several times and inflate every overlap.
  parts_ <- purrr::pmap(
    list(have_$Family, have_$Entity, have_$Table),
    function(.f, .e, .t) {
      if (.f != "spacy") {
        return(glue::glue(
          "SELECT DocID, '{.e}' AS Entity, '{.f}' AS Producer, Start, Stop, Span FROM {.t}"
        ))
      }
      purrr::map_chr(.spacy_models, function(.m) {
        short_ <- paste0("spacy:", stringi::stri_replace_first_fixed(.m, "en_core_web_", ""))
        as.character(glue::glue(
          "SELECT DocID, '{.e}' AS Entity, '{short_}' AS Producer, Start, Stop, Span
             FROM {.t} WHERE Model = '{.m}'"
        ))
      })
    }
  ) |>
    unlist()

  DBI::dbExecute(.con, "DROP TABLE IF EXISTS ent_spans")
  DBI::dbExecute(.con, paste0(
    "CREATE TEMP TABLE ent_spans AS ", paste(parts_, collapse = "\n    UNION ALL\n    ")
  ))

  DBI::dbExecute(.con, "DROP TABLE IF EXISTS ent_mentions")
  DBI::dbExecute(.con, "
    CREATE TEMP TABLE ent_mentions AS
    WITH lagged AS (
      SELECT *, MAX(Stop) OVER (PARTITION BY DocID, Entity ORDER BY Start, Stop
                  ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS PrevMaxEnd
        FROM ent_spans
    ),
    flagged AS (
      SELECT *, CASE WHEN PrevMaxEnd IS NULL OR Start >= PrevMaxEnd THEN 1 ELSE 0 END AS IsNew
        FROM lagged
    ),
    clustered AS (
      SELECT *, SUM(IsNew) OVER (PARTITION BY DocID, Entity ORDER BY Start, Stop
                  ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS Cluster
        FROM flagged
    )
    SELECT DocID, Entity, Producer, Start, Stop, Span,
           DocID || '::' || Entity || '::' || CAST(Cluster AS VARCHAR) AS MentionID
      FROM clustered")

  invisible(.con)
}

#' How many producers found each mention
#'
#' The headline agreement number, and the one that is not defeated by boundary differences. A
#' mention found by every eligible producer is one nobody disputes; a mention found by one is either
#' a discovery or a false positive, and this cannot tell them apart -- 04B's rules can.
#'
#' @param .con Connection carrying ent_mentions.
#' @return Tibble: Entity, NProducer, NMentions, Share, Eligible.
ent_consensus <- function(.con) {
  if (FALSE) {
    .con <- con_all
  }

  elig_ <- DBI::dbGetQuery(.con, "
    SELECT Entity, COUNT(*) AS Eligible
      FROM (SELECT DISTINCT Entity, Producer FROM ent_spans) GROUP BY Entity") |>
    tibble::as_tibble()

  DBI::dbGetQuery(.con, "
    WITH m AS (SELECT MentionID, any_value(Entity) AS Entity,
                      COUNT(DISTINCT Producer) AS NProducer
                 FROM ent_mentions GROUP BY MentionID)
    SELECT Entity, NProducer, COUNT(*) AS NMentions FROM m GROUP BY Entity, NProducer") |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(c("NProducer", "NMentions"), as.integer)) |>
    dplyr::mutate(Share = round(.data$NMentions / sum(.data$NMentions), 3), .by = "Entity") |>
    dplyr::left_join(elig_, by = dplyr::join_by(Entity)) |>
    dplyr::mutate(Eligible = as.integer(.data$Eligible)) |>
    dplyr::arrange(.data$Entity, .data$NProducer)
}

#' Pairwise agreement over mentions, with the boundary question kept separate
#'
#' `Jaccard` is agreement about WHAT IS THERE: the share of mentions either producer found that both
#' found. `ExactShare` is agreement about WHERE IT ENDS: among the mentions both found, the share on
#' which the two put a span at identical offsets.
#'
#' THE GAP BETWEEN THEM IS THE FINDING. A high Jaccard with a low ExactShare means the engines agree
#' about the world and disagree about boundaries, which is a normalisation problem and exactly what
#' 04B's rules exist to solve. A low Jaccard means something else entirely -- the producers mostly
#' propose different things and are complements rather than substitutes.
#'
#' EXACTNESS IS PER PAIR, NOT PER MENTION, and the first version got that wrong. It asked whether
#' every span in a mention shared one offset pair, so on a mention found by three producers where
#' two agreed and the third did not, the agreeing pair was recorded as inexact along with the other
#' two. With two producers the two definitions coincide, which is why it survived a render -- the
#' third producer is what makes them diverge.
#'
#' PAIRS ARE ORDERED BY THE PLOT REGISTRY, not alphabetically. The order has no meaning for the
#' numbers, which are symmetric, but it decides which half of a matrix they land in -- and a
#' triangle assembled under one ordering and drawn under another comes out ragged.
#'
#' ALL ELIGIBLE PAIRS ARE ASSEMBLED, so two producers that never co-occur show a Jaccard of zero
#' rather than vanishing from the table. A missing row and a zero are different findings.
#'
#' @param .con Connection carrying ent_mentions.
#' @return Tibble: Entity, ProducerA, ProducerB, Both, Exact, MentionsA, MentionsB, Jaccard,
#'   ExactShare.
ent_pairwise <- function(.con) {
  if (FALSE) {
    .con <- con_all
  }

  counts_ <- DBI::dbGetQuery(.con, "
    SELECT Entity, Producer, COUNT(DISTINCT MentionID) AS Mentions
      FROM ent_mentions GROUP BY Entity, Producer") |>
    tibble::as_tibble()

  if (nrow(counts_) == 0L) return(tibble::tibble())

  # BOTH DIRECTIONS, so the join below works whichever way round R orients a pair. The aggregate is
  # a few dozen rows, so the duplication costs nothing and removes an ordering assumption that would
  # otherwise have to hold in two languages at once.
  cooc_ <- DBI::dbGetQuery(.con, "
    WITH m AS (SELECT DISTINCT MentionID, Entity, Producer, Start, Stop FROM ent_mentions)
    SELECT a.Entity, a.Producer AS ProducerA, b.Producer AS ProducerB,
           COUNT(DISTINCT a.MentionID) AS Both,
           COUNT(DISTINCT CASE WHEN a.Start = b.Start AND a.Stop = b.Stop
                               THEN a.MentionID END) AS Exact
      FROM m a
      JOIN m b ON a.MentionID = b.MentionID AND a.Producer <> b.Producer
     GROUP BY a.Entity, a.Producer, b.Producer") |>
    tibble::as_tibble()

  ord_ <- \(.x) match(.x, .ent_producers)

  counts_ |>
    dplyr::select("Entity", ProducerA = "Producer") |>
    dplyr::inner_join(dplyr::select(counts_, "Entity", ProducerB = "Producer"),
                      by = dplyr::join_by(Entity), relationship = "many-to-many") |>
    dplyr::filter(ord_(.data$ProducerA) < ord_(.data$ProducerB)) |>
    dplyr::left_join(cooc_, by = dplyr::join_by(Entity, ProducerA, ProducerB)) |>
    dplyr::mutate(
      Both  = as.integer(dplyr::coalesce(.data$Both, 0L)),
      Exact = as.integer(dplyr::coalesce(.data$Exact, 0L))
    ) |>
    dplyr::left_join(dplyr::rename(counts_, ProducerA = "Producer", MentionsA = "Mentions"),
                     by = dplyr::join_by(Entity, ProducerA)) |>
    dplyr::left_join(dplyr::rename(counts_, ProducerB = "Producer", MentionsB = "Mentions"),
                     by = dplyr::join_by(Entity, ProducerB)) |>
    dplyr::mutate(
      MentionsA  = as.integer(.data$MentionsA),
      MentionsB  = as.integer(.data$MentionsB),
      Jaccard    = round(
        .data$Both / (.data$MentionsA + .data$MentionsB - .data$Both), 3
      ),
      ExactShare = round(dplyr::if_else(.data$Both > 0L, .data$Exact / .data$Both, NA_real_), 3)
    ) |>
    dplyr::arrange(.data$Entity, ord_(.data$ProducerA), ord_(.data$ProducerB))
}

#' Report consensus and pairwise agreement together
#'
#' @param .consensus Output of ent_consensus().
#' @param .pairwise Output of ent_pairwise().
#' @return .pairwise, invisibly.
ent_report_alignment <- function(.consensus, .pairwise) {
  if (FALSE) {
    .consensus <- tab_consensus
    .pairwise  <- tab_pairwise
  }

  tbl_say(.tab = .consensus, .title = "Mentions by how many producers found them")

  # NAMED, NOT SILENTLY OMITTED. An entity with one eligible producer cannot have a consensus above
  # one, so its whole distribution sits in a single row and means nothing -- which is different from
  # a low consensus, and a reader has no way to tell them apart from the table alone.
  solo_ <- .consensus |>
    dplyr::filter(.data$Eligible < 2L) |>
    dplyr::distinct(.data$Entity)
  if (nrow(solo_) > 0L) {
    cli::cli_alert_info(
      "One producer each, so consensus cannot exceed one and no agreement exists to report: \\
       {paste(solo_$Entity, collapse = ', ')}. Coverage and the offset contract still apply."
    )
  }

  if (nrow(.pairwise) == 0L) {
    cli::cli_alert_warning("No entity has two producers, so no pair can be compared.")
    return(invisible(.pairwise))
  }

  tbl_say(.tab = .pairwise, .title = "Pairwise agreement over mentions")
  cli::cli_alert_info(
    "Jaccard is agreement about what is there; ExactShare is agreement, among mentions both found, \\
     about where it ends."
  )

  # TWO PATTERNS, TWO DIAGNOSES, and only naming one of them was a defect in the first version of
  # this report. The prose predicted wide agreement with narrow boundary matching; GPE came back the
  # other way round, and the check stayed silent on the finding that mattered.
  bound_ <- dplyr::filter(.pairwise, .data$Jaccard > 0.5, !is.na(.data$ExactShare),
                          .data$ExactShare < 0.5)
  if (nrow(bound_) > 0L) {
    cli::cli_alert_warning(
      "{nrow(bound_)} pair{?s} agree about what is there and disagree about where it ends: \\
       {paste(paste0(bound_$Entity, ' (', bound_$ProducerA, '/', bound_$ProducerB, ')'),
              collapse = ', ')}. That is a normalisation problem, and it is 04B's job."
    )
  }

  subst_ <- dplyr::filter(.pairwise, .data$Jaccard < 0.5, !is.na(.data$ExactShare),
                          .data$ExactShare >= 0.5)
  if (nrow(subst_) > 0L) {
    cli::cli_alert_warning(
      "{nrow(subst_)} pair{?s} disagree about WHAT IS THERE while agreeing about boundaries \\
       where they meet: \\
       {paste(paste0(subst_$Entity, ' (', subst_$ProducerA, '/', subst_$ProducerB, ')'),
              collapse = ', ')}. These producers are not two opinions about one task -- most of \\
       what each finds, the other never proposes -- so they are complements rather than substitutes \\
       and 04B cannot pick one and discard the other without losing coverage."
    )
  }

  invisible(.pairwise)
}


# 5c. Where in a document each producer finds things ---------------------------------------------------------------------

#' Candidate positions, binned across the unit interval
#'
#' Position is the midpoint of a span over the length of its document, so it is comparable across
#' documents that differ by three orders of magnitude in length.
#'
#' SHARE IS TAKEN WITHIN PRODUCER AND ENTITY. Across the whole table instead, a figure would compare
#' producers on how much they emit rather than on where they emit it -- and they differ in volume by
#' more than an order of magnitude, so the smaller arms would be slivers along the axis.
#'
#' @param .con Connection carrying ent_spans.
#' @param .path_text The canonical text, for document lengths.
#' @param .bins Bins across the unit interval; a multiple of ten so decile zones are exact.
#' @return Tibble: Producer, Entity, Bin, Mid, Spans, Share.
ent_positions <- function(.con, .path_text, .bins = 30L) {
  if (FALSE) {
    .con       <- con_all
    .path_text <- .lP$Output$Sample
    .bins      <- 30L
  }

  if (.bins %% 10L != 0L) cli::cli_abort("{.arg .bins} must be a multiple of ten.")
  n_ <- as.integer(.bins)

  DBI::dbExecute(.con, glue::glue(
    "CREATE OR REPLACE TEMP VIEW ent_lens AS
       SELECT DocID, length(TextRaw) AS DocLen
         FROM read_parquet('{as.character(fs::path_real(.path_text))}')
        WHERE length(TextRaw) > 0"
  ))

  DBI::dbGetQuery(.con, glue::glue(
    "SELECT s.Producer, s.Entity,
            least({n_ - 1L}, CAST(floor((((s.Start + s.Stop) / 2.0) / l.DocLen) * {n_})
              AS INTEGER)) AS Bin,
            COUNT(*) AS Spans
       FROM ent_spans s JOIN ent_lens l USING (DocID)
      WHERE s.Start IS NOT NULL
      GROUP BY s.Producer, s.Entity, Bin"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      Spans = as.integer(.data$Spans),
      Bin   = as.integer(.data$Bin),
      Mid   = (.data$Bin + 0.5) / n_
    ) |>
    dplyr::mutate(Share = .data$Spans / sum(.data$Spans), .by = c("Producer", "Entity"))
}

#' Collapse a positional distribution to one comparable number
#'
#' Thirty bin shares per producer and entity are readable pooled and unreadable once anything is
#' crossed with them. The contrast is the mean share at the two ends of the document over the mean
#' share through the middle: one number answering the question the shares are consulted for -- does
#' this producer concentrate the entity where parties are named, or spread it through the text.
#'
#' A uniform producer scores 1.0 by construction.
#'
#' THE SECOND AND NINTH DECILES ARE EXCLUDED FROM THE MIDDLE deliberately. They are shoulder: a
#' preamble spills into the second decile of a short document and a signature block into the ninth,
#' so counting them as middle would blunt the very contrast being measured.
#'
#' @param .tab Output of ent_positions().
#' @param .by Grouping columns.
#' @return Tibble: the grouping columns, EndShare, MidShare, Contrast.
ent_contrast <- function(.tab, .by = c("Producer", "Entity")) {
  if (FALSE) {
    .tab <- tab_positions
    .by  <- c("Producer", "Entity")
  }

  .tab |>
    dplyr::mutate(Zone = dplyr::case_when(
      .data$Mid < 0.1 | .data$Mid >= 0.9 ~ "End",
      .data$Mid >= 0.2 & .data$Mid < 0.8 ~ "Mid",
      .default = "Shoulder"
    )) |>
    dplyr::filter(.data$Zone != "Shoulder") |>
    dplyr::summarise(Share = mean(.data$Share), .by = c(dplyr::all_of(.by), "Zone")) |>
    tidyr::pivot_wider(names_from = "Zone", values_from = "Share",
                       names_glue = "{Zone}Share") |>
    dplyr::mutate(Contrast = round(.data$EndShare / .data$MidShare, 2)) |>
    dplyr::mutate(dplyr::across(c("EndShare", "MidShare"), \(.x) round(.x, 4))) |>
    dplyr::arrange(.data$Entity, dplyr::desc(.data$Contrast))
}


# 6. Figures -------------------------------------------------------------------------------------------------------------
#
# Two shapes, both drawn through the shared design layer so that an entity figure and a
# classification figure are the same object rendered from different data. Neither sets a title: the
# caption in the runbook carries that, which is what makes a figure reusable in the paper without
# editing the function that drew it.

#' Figure height for a wrapped facet grid
#'
#' plot_height() maps a count of rows on a discrete axis onto the project's four-rung ladder. A
#' faceted figure has no such axis: its vertical extent is driven by how many FACET ROWS the wrap
#' produces, and each facet is worth several chart rows of space. Passing the panel count straight
#' to plot_height() understates a two-row grid by about a third, which is how these figures came to
#' carry hand-typed heights off the ladder entirely.
#'
#' This converts one to the other and then defers to the ladder, so the height is still derived
#' rather than chosen. It stays here rather than in the shared layer because the conversion depends
#' on how a particular figure is wrapped, which is the document's business.
#'
#' @param .n_panels Facets in the wrap.
#' @param .n_cols Columns ggplot2 will lay them out in.
#' @param .rows_per_panel Chart rows one facet is worth vertically.
#' @param .square Passed through for matrix facets, where height tracks width.
#' @return Numeric height in inches, from the ladder.
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
#' THE FIGURE THE GEOGRAPHY CLAIM RESTS ON. Mass at the opening and again around the governing-law
#' clause supports describing extracted places as party locations; a flat distribution through the
#' body does not.
#'
#' DENSITY, NOT COUNTS, because counts cannot answer the question this figure is asked. The producers
#' differ in volume by more than an order of magnitude, so a stacked count is a picture of which one
#' is loudest and the smaller arms are slivers along the axis. Normalising each producer to its own
#' total makes the SHAPES comparable, which is what "do they agree about where this entity lives"
#' actually means. Volume is already reported, per producer and per entity, in the coverage table.
#'
#' Lines rather than bars for the same reason: overlaid histograms occlude each other whatever the
#' transparency, and the comparison here is between profiles rather than between totals.
#'
#' THE VERTICAL AXIS INCLUDES ZERO, and leaving it out was a defect rather than a preference. With
#' free scales and no zero, ggplot fits each panel to its own data range: ORG spans 2.9 to 4.1 per
#' cent and rendered as violent oscillation, while the contrast table two lines above reported 0.97 --
#' a distribution that is uniform to within rounding. The figure and the table told opposite stories
#' and the figure was the one that was wrong.
#'
#' Zero costs the flat panels their drama and that is the point: a shape that survives a zero
#' baseline is a shape.
#'
#' @param .positions Output of ent_positions().
#' @param .free_y Independent vertical scales per facet. Even as densities the entities differ in
#'   concentration by several fold, so one scale across all of them flattens the flatter panels past
#'   readability. Free scales plus a zero baseline is the combination that is both readable and
#'   honest.
#' @return A ggplot.
ent_plot_positions <- function(.positions, .free_y = TRUE) {
  if (FALSE) {
    .positions <- tab_positions
    .free_y    <- TRUE
  }

  if (is.null(.positions) || nrow(.positions) == 0L) {
    cli::cli_abort("No positions to plot; {.arg .positions} is ent_positions().")
  }

  .positions |>
    dplyr::mutate(
      PlotEntity   = plot_factor(.data$Entity,   .key = "Entity"),
      PlotProducer = plot_factor(.data$Producer, .key = "Producer")
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Mid, y = .data$Share, colour = .data$PlotProducer)) +
    ggplot2::geom_line(linewidth = 0.6) +
    plot_scale_colour_key(.key = "Producer") +
    ggplot2::facet_wrap(~PlotEntity, scales = if (.free_y) "free_y" else "fixed") +
    ggplot2::expand_limits(y = 0) +
    plot_scale_x_pct(
      .accuracy = 1,
      .expand   = c(0, 0),
      .breaks   = scales::breaks_pretty(n = 4)   # 0/25/50/75/100, not ggplot's 2/5/8/10
    ) +
    plot_scale_y_pct(
      # ONE DECIMAL, BECAUSE WHOLE PER CENT REPEATED ITSELF. On a panel spanning 2.9 to 4.1, four
      # pretty breaks round to 3%, 3%, 4%, 4% -- an axis that labels two different values with the
      # same string is worse than an unlabelled one.
      .accuracy = 0.1,
      .expand   = c(0, 0.02),
      .breaks   = scales::breaks_pretty(n = 4)
    ) +
    ggplot2::labs(x = "Position in document", y = "Share of candidates", colour = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}

#' How many producers found each mention, as a share per entity
#'
#' THE AGREEMENT FIGURE THAT WORKS AT ANY PRODUCER COUNT, which the heatmap does not. One bar per
#' entity, split by how many producers found each mention: the left segment is what only one
#' producer proposed, the right segments what several agreed on. An entity with one producer is a
#' single full-width segment, which is honest rather than broken.
#'
#' This is also the more useful view of the number. A Jaccard of 0.44 is a ratio a reader has to
#' reason about; "fifty-six per cent of GPE mentions were found by one engine and not the other" is
#' the same fact stated as the thing it implies.
#'
#' @param .consensus Output of ent_consensus().
#' @return A ggplot.
ent_plot_consensus <- function(.consensus) {
  if (FALSE) {
    .consensus <- tab_consensus
  }

  if (is.null(.consensus) || nrow(.consensus) == 0L) {
    cli::cli_abort("Nothing to plot; {.arg .consensus} is ent_consensus().")
  }

  .consensus |>
    dplyr::mutate(Found = factor(
      .data$NProducer,
      levels = sort(unique(.data$NProducer)),
      labels = paste0(sort(unique(.data$NProducer)), " producer",
                      ifelse(sort(unique(.data$NProducer)) == 1L, "", "s"))
    )) |>
    plot_bar_stacked(
      .tab   = _,
      .cat   = "Entity",
      .val   = "NMentions",
      .fill  = "Found",
      .key   = "Entity",       # the registered order, so entities sit where they do everywhere else
      .share = TRUE            # shares, since the entities differ in volume by two orders of size
    )
}

#' Pairwise agreement between producers, by entity
#'
#' AN UPPER TRIANGLE WITH THE DIAGONAL INTACT, which is how a correlation matrix is read and why the
#' shape is worth the small amount of assembly. The diagonal anchors the eye at the top of the scale:
#' a reader sees what perfect agreement looks like in this palette and reads every off-diagonal cell
#' against it, rather than against the legend alone.
#'
#' THE DIAGONAL IS ADDED HERE AND NOT IN ent_pairwise(), deliberately. That a producer agrees with
#' itself is an identity rather than a measurement, and putting seven rows of 1.000 into a findings
#' table would be padding. In a figure it is a reading aid, which is a different job.
#'
#' Both axes carry the same vocabulary in the same order, rows are the earlier producer and columns
#' the later one, and plot_heatmap() reverses the vertical axis -- so the registry's first producer
#' sits at the top left and every cell lands on or above the diagonal.
#'
#' Read it by family rather than by cell. High values mean two producers are substitutes and the
#' choice between them costs little; low values mark where they disagree. The trap, once the CNN
#' models are switched on, is that four producers are the same spaCy pipeline at four capacities, so
#' their mutual agreement is one implementation agreeing with itself and says nothing about whether
#' any of them is right.
#'
#' ONLY PRODUCERS ELIGIBLE FOR AN ENTITY APPEAR IN ITS PANEL. A producer that does not attempt an
#' entity has not disagreed about it, and drawing it as an empty row invites exactly that
#' misreading. The axes drop per panel, which needs both the free facet scales below and .drop on
#' the primitive.
#'
#' The fill scale is fixed to the unit interval. Jaccard has a meaningful absolute scale, and left to
#' auto-scale the ramp would stretch across whatever range the data happened to occupy, rendering
#' rounding differences as strong visual structure.
#'
#' @param .pairwise Output of ent_pairwise().
#' @param .accuracy Rounding for the printed cell values.
#' @param .min_producers Producers an entity must have before a matrix is worth drawing. Two is the
#'   floor: with the diagonal that is a readable three-cell triangle, where one producer would be a
#'   single cell reading 1.00 and saying nothing.
#' @return A ggplot, or NULL where no entity clears the floor.
ent_plot_agreement <- function(.pairwise, .accuracy = 0.01, .min_producers = 2L) {
  if (FALSE) {
    .pairwise      <- tab_pairwise
    .accuracy      <- 0.01
    .min_producers <- 2L
  }

  if (is.null(.pairwise) || nrow(.pairwise) == 0L) {
    cli::cli_alert_info("No entity has two producers, so there is no agreement matrix to draw.")
    return(invisible(NULL))
  }

  wide_ <- .pairwise |>
    dplyr::summarise(
      NProducer = dplyr::n_distinct(c(.data$ProducerA, .data$ProducerB)),
      .by       = "Entity"
    ) |>
    dplyr::filter(.data$NProducer >= .min_producers)

  if (nrow(wide_) == 0L) {
    cli::cli_alert_info(
      "No entity has {(.min_producers)} producers, so every panel would be a single cell. The \\
       pairwise table above carries the same {nrow(.pairwise)} number{?s} in less space."
    )
    return(invisible(NULL))
  }

  keep_ <- dplyr::semi_join(.pairwise, wide_, by = dplyr::join_by(Entity))

  # THE DIAGONAL, BUILT FROM THE PRODUCERS EACH PANEL ACTUALLY HAS rather than from the registry.
  # A producer that never appears for an entity must not gain a diagonal cell there: it would put a
  # 1.00 on an axis the panel has otherwise dropped and imply an agreement that was never measured.
  diag_ <- keep_ |>
    dplyr::reframe(Producer = unique(c(.data$ProducerA, .data$ProducerB)), .by = "Entity") |>
    dplyr::transmute(
      .data$Entity,
      ProducerA  = .data$Producer,
      ProducerB  = .data$Producer,
      Jaccard    = 1,
      ExactShare = 1
    )

  dplyr::bind_rows(dplyr::select(keep_, "Entity", "ProducerA", "ProducerB", "Jaccard"), diag_) |>
    dplyr::mutate(Entity = plot_factor(.data$Entity, .key = "Entity")) |>
    plot_heatmap(
      .tab      = _,
      .x        = "ProducerB",    # the later producer is the column
      .y        = "ProducerA",    # the earlier is the row, and the y axis is reversed
      .fill     = "Jaccard",
      .key_x    = "Producer",     # both axes carry the same vocabulary, so both are keyed the same
      .key_y    = "Producer",
      .label    = TRUE,
      .pct      = FALSE,          # Jaccard reads as a ratio, not as a percentage
      .accuracy = .accuracy,
      .angle    = 40,
      .limits   = c(0, 1),        # fixed: the quantity has an absolute scale
      .drop     = "both"          # per panel, with the free scales below
    ) +
    ggplot2::facet_wrap(~Entity, scales = "free")
}


#' What each family wrote, as a deployment statement
#'
#' @param .dir The output directory holding the three databases.
#' @param .families Family names.
#' @param .path_sample The canonical text.
#' @return Tibble: Artifact, Exists, MB.
ent_report_artifacts <- function(.dir, .families, .path_sample) {
  if (FALSE) {
    .dir         <- .lP$Output$Store
    .families    <- .ner_families
    .path_sample <- .lP$Output$Sample
  }

  paths_ <- c(.path_sample, purrr::map_chr(.families, \(.f) as.character(ner_db_path(.dir, .f))))

  out_ <- tibble::tibble(Path = paths_) |>
    dplyr::mutate(
      Artifact = fs::path_file(.data$Path),
      Exists   = fs::file_exists(.data$Path),
      MB       = round(dplyr::if_else(
        .data$Exists, as.numeric(fs::file_size(.data$Path)) / 1024^2, NA_real_
      ), 1)
    ) |>
    dplyr::select("Artifact", "Exists", "MB")

  tbl_say(.tab = out_, .title = "Written by 04A")

  miss_ <- dplyr::filter(out_, !.data$Exists)
  if (nrow(miss_) > 0L) {
    cli::cli_alert_warning(
      "{nrow(miss_)} artifact{?s} {?is/are} absent: \\
       {paste(miss_$Artifact, collapse = ', ')}."
    )
  }

  invisible(out_)
}
