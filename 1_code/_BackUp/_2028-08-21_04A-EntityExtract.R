# ======================================================================================================================
# 04A-EntityExtract.R -- library for 04A-EntityExtract.qmd
# ======================================================================================================================
#
# Sample construction, extraction orchestration, and the reports 04A prints. Everything that talks to
# a suite or to the store lives in _Commons/_NER.R and is sourced before this file.
#
# WHAT THIS SCRIPT OWNS. The labelled sample and the candidate store. Both are read by 04B and by the
# check scripts, so their paths are a contract even though the reports in the document are not.


# 1. Sample ------------------------------------------------------------------------------------------------------------

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
#' TEXT IS COPIED, NOT REFERENCED. Every offset in the store indexes THIS file. If the text a span
#' points into can change -- because 03A re-read the corpus, or normalised it differently -- then an
#' offset is a promise nobody can keep. So the sample carries its own copy and nothing rebuilds it.
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
    cli::cli_warn("{drop_} document{?s} carried no text and were dropped from the sample.")
  }
  if (nrow(out_) == 0L) cli::cli_abort("The sample is empty; check {(.path_prepared)}.")

  out_
}

#' Report the sample
#'
#' @param .tab Output of ent_build_sample().
#' @param .class_col Contract-type column to break down by, or NULL.
#' @return .tab, invisibly.
ent_report_sample <- function(.tab, .class_col = "ClassDetailed") {
  if (FALSE) {
    .tab       <- arrow::read_parquet(.lP$Output$Sample)
    .class_col <- "ClassDetailed"
  }

  cli::cli_alert_info(
    "{format(nrow(.tab), big.mark = ',')} document{?s}, \\
     {format(sum(nchar(.tab$TextRaw)), big.mark = ',')} characters."
  )

  len_ <- tibble::tibble(
    Metric = c("Min", "Median", "Mean", "P90", "Max"),
    Chars  = c(min(nchar(.tab$TextRaw)), stats::median(nchar(.tab$TextRaw)),
               round(mean(nchar(.tab$TextRaw))), stats::quantile(nchar(.tab$TextRaw), 0.9),
               max(nchar(.tab$TextRaw)))
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


# 2. Extraction ----------------------------------------------------------------------------------------------------------

#' Run every declared combination and ingest what it produced
#'
#' EXTRACTION AND INGEST ARE SEPARATE CALLS, joined here rather than fused. The benchmark runs
#' extraction alone and discards the output; fusing them would make timing impossible without
#' polluting the store.
#'
#' THE LEDGER MAKES RE-RENDERING CHEAP. A combination whose documents are all recorded is skipped
#' entirely, so a document that takes hours on its first render costs seconds on the next. That is
#' also why no chunk in this document needs a switch: expense is handled by knowing what is done, not
#' by turning work off.
#'
#' @param .con Store connection.
#' @param .path_in Staged parquet carrying the sample text.
#' @param .doc_ids Document identifiers in that parquet.
#' @param .spec Tibble: Suite, Model, Labels (list) -- what to run.
#' @param .stage_dir Directory for staged parquets.
#' @param .n_process Workers.
#' @param .batch_size Documents per unit of work.
#' @param .timeout Per-document cap, seconds.
#' @param .force Re-run even where the ledger says a combination is complete.
#' @return Tibble: Suite, Model, Label, NDoc, Ran, Seconds.
ent_extract_all <- function(.con, .path_in, .doc_ids, .spec, .stage_dir,
                            .n_process = .ner_n_process, .batch_size = .ner_batch_size,
                            .timeout = .ner_timeout, .force = FALSE) {
  if (FALSE) {
    .con        <- ner_db_connect(.db_path = .lP$Output$Store)
    .path_in    <- .lP$Output$Sample
    .doc_ids    <- arrow::read_parquet(.lP$Output$Sample, col_select = "DocID")$DocID
    .spec       <- ner_describe() |> dplyr::select("Suite", "Engine", "Model", "Labels")
    .stage_dir  <- .lP$Output$Stage
    .n_process  <- 20L
    .batch_size <- 32L
    .timeout    <- 600L
    .force      <- FALSE
  }

  fs::dir_create(.stage_dir)
  out_ <- tibble::tibble()

  for (i_ in seq_len(nrow(.spec))) {
    suite_  <- .spec$Suite[[i_]]
    model_  <- .spec$Model[[i_]]
    engine_ <- .spec$Engine[[i_]]
    labels_ <- .spec$Labels[[i_]]

    # SKIP ON THE LEDGER, NOT ON THE FILE. A staged parquet on disk proves a run happened, not that
    # it covered these documents; the ledger is the only thing that answers per document.
    todo_ <- if (.force) {
      .doc_ids
    } else {
      purrr::map(labels_, \(.l) ner_db_missing(
        .con = .con, .doc_ids = .doc_ids, .engine = engine_, .model = model_, .label = .l
      )) |>
        purrr::reduce(union)
    }

    if (length(todo_) == 0L) {
      cli::cli_alert_success("{(suite_)}:{(model_)} -- complete, skipped.")
      out_ <- dplyr::bind_rows(out_, tibble::tibble(
        Suite = suite_, Model = model_, Label = labels_,
        NDoc = 0L, Ran = FALSE, Seconds = NA_real_
      ))
      next
    }

    cli::cli_alert_info("{(suite_)}:{(model_)} -- {length(todo_)} document{?s} to do.")

    fun_   <- switch(suite_, spacy = ner_spacy, lexnlp = ner_lexnlp, matcon = ner_matcon)
    paths_ <- fun_(
      .path_in    = .path_in,
      .out_dir    = .stage_dir,
      .model      = if (suite_ == "spacy") model_ else NULL,
      .labels     = labels_,
      .n_process  = .n_process,
      .batch_size = .batch_size,
      .timeout    = .timeout,
      .quiet      = FALSE
    )
    secs_ <- attr(paths_, "elapsed")

    for (p_ in paths_) ner_db_append(.con = .con, .path = p_, .labels = labels_)

    out_ <- dplyr::bind_rows(out_, tibble::tibble(
      Suite = suite_, Model = model_, Label = labels_,
      NDoc = length(todo_), Ran = TRUE, Seconds = secs_
    ))
  }

  out_
}


#' Run every combination over a handful of documents and check the plumbing
#'
#' THE CHEAPEST QUESTION ASKED FIRST. A full pass over this sample takes hours, most of it in one
#' suite, and every structural failure it can have -- a missing interpreter, a stale container, a
#' schema that moved, an offset that does not round-trip -- is visible on fifty documents. Asking
#' after the hours have been spent is asking too late.
#'
#' Writes to a temporary directory and NEVER to the store. A smoke test that ingests is no longer a
#' test; it is a partial run that has to be undone.
#'
#' @param .tab_sample The sample.
#' @param .spec Tibble: Suite, Engine, Model, Labels (list).
#' @param .dir Directory for the staged parquets. Caller owns cleanup.
#' @param .n Documents to draw.
#' @param .seed Fixes the draw, so a failure is reproducible.
#' @param .n_process,.batch_size,.timeout Passed through unchanged.
#' @return Tibble, one row per combination, with a column per check.
ent_smoke_test <- function(.tab_sample, .spec, .dir, .n = 50L, .seed = 42L,
                           .n_process = .ner_n_process, .batch_size = .ner_batch_size,
                           .timeout = .ner_timeout) {
  if (FALSE) {
    .tab_sample <- arrow::read_parquet(.lP$Output$Sample)
    .spec       <- dplyr::select(ner_describe(), "Suite", "Engine", "Model", "Labels")
    .dir        <- fs::path(tempdir(), "smoke")
    .n          <- 50L
    .seed       <- 42L
    .n_process  <- 20L
    .batch_size <- 32L
    .timeout    <- 600L
  }

  fs::dir_create(.dir)
  withr::with_seed(.seed, {
    tab_ <- dplyr::slice_sample(.tab_sample, n = min(.n, nrow(.tab_sample)))
  })

  path_in_ <- fs::path(.dir, "smoke_text.parquet")
  arrow::write_parquet(dplyr::select(tab_, "DocID", "TextRaw"), path_in_)
  txt_ <- rlang::set_names(tab_$TextRaw, tab_$DocID)

  out_ <- tibble::tibble()

  for (i_ in seq_len(nrow(.spec))) {
    suite_  <- .spec$Suite[[i_]]
    model_  <- .spec$Model[[i_]]
    labels_ <- .spec$Labels[[i_]]

    fun_ <- switch(suite_, spacy = ner_spacy, lexnlp = ner_lexnlp, matcon = ner_matcon)
    paths_ <- fun_(
      .path_in    = path_in_,
      .out_dir    = .dir,
      .model      = if (suite_ == "spacy") model_ else NULL,
      .labels     = labels_,
      .n_process  = .n_process,
      .batch_size = .batch_size,
      .timeout    = .timeout,
      .quiet      = TRUE
    )
    secs_ <- attr(paths_, "elapsed")

    for (p_ in paths_) {
      got_ <- arrow::read_parquet(p_)
      hit_ <- dplyr::filter(got_, !is.na(.data$Start))

      out_ <- dplyr::bind_rows(out_, tibble::tibble(
        Suite   = suite_,
        Model   = paste(unique(got_$Model), collapse = "/"),
        Spans   = nrow(hit_),
        Seconds = round(secs_, 1),
        # ONE (Engine, Model) PER FILE: the assertion ingest makes, checked before ingest exists.
        OneStamp = dplyr::n_distinct(got_$Engine) == 1L && dplyr::n_distinct(got_$Model) == 1L,
        CoreOK   = identical(names(got_)[seq_along(.store_core)], .store_core),
        # Processed-and-empty must stay distinguishable from never-processed.
        AllDocs  = dplyr::n_distinct(got_$DocID) == nrow(tab_),
        # The invariant everything rests on. stri_sub, never substr: offsets are code points.
        Offsets  = nrow(hit_) == 0L || all(
          hit_$Span == stringi::stri_sub(txt_[hit_$DocID], hit_$Start + 1L, hit_$Stop)
        ),
        # A null Start must mean a null everything, or a consumer filtering on Start keeps junk.
        Sentinel = got_ |>
          dplyr::filter(is.na(.data$Start)) |>
          dplyr::summarise(OK = all(is.na(.data$Stop) & is.na(.data$Span))) |>
          dplyr::pull(.data$OK) |> (\(.x) length(.x) == 0L || isTRUE(.x))()
      ))
    }
  }

  out_
}

#' Report the smoke test, and stop if anything failed
#'
#' Aborts rather than warns. Every check here is structural, and a structural failure means the hours
#' that follow would produce a store nothing downstream can trust.
#'
#' @param .tab Output of ent_smoke_test().
#' @return .tab, invisibly.
ent_report_smoke <- function(.tab) {
  if (FALSE) {
    .tab <- ent_smoke_test(.tab_sample = tab_sample, .spec = tab_describe,
                           .dir = fs::path(tempdir(), "smoke"))
  }

  tbl_say(.tab = .tab, .title = "Smoke test: every combination over a small slice")

  cols_ <- c("OneStamp", "CoreOK", "AllDocs", "Offsets", "Sentinel")
  bad_  <- dplyr::filter(.tab, !dplyr::if_all(dplyr::all_of(cols_)))

  if (nrow(bad_) > 0L) {
    cli::cli_abort(c(
      "{nrow(bad_)} combination{?s} failed a structural check.",
      "x" = "{paste(bad_$Suite, bad_$Model, collapse = ' | ')}",
      "i" = "Nothing that follows would be trustworthy, so the document stops here."
    ))
  }

  cli::cli_alert_success(
    "All {nrow(.tab)} combination{?s} produced a well-formed parquet: one stamp, core columns,
     every document present, offsets exact, sentinels clean."
  )
  invisible(.tab)
}


# 3. Report: coverage --------------------------------------------------------------------------------------------------

#' What each combination found
#'
#' @param .con Store connection.
#' @param .n_doc Documents in the sample, for the coverage share.
#' @return Tibble: Engine, Model, Label, Docs, Spans, SpansPerDoc, Coverage, plus the ledger states.
ent_coverage <- function(.con, .n_doc) {
  if (FALSE) {
    .con   <- ner_db_connect(.db_path = .lP$Output$Store, .read_only = TRUE)
    .n_doc <- 4398L
  }

  ner_db_summary(.con = .con) |>
    dplyr::mutate(
      Docs        = .data$SpanDocs,
      SpansPerDoc = round(dplyr::if_else(.data$SpanDocs > 0L, .data$Spans / .data$SpanDocs, 0), 1),
      Coverage    = round(.data$SpanDocs / .n_doc, 3)
    ) |>
    dplyr::select(
      "Engine", "Model", "Label", "Docs", "Spans", "SpansPerDoc", "Coverage",
      dplyr::any_of(c("hit", "nohit", "timeout", "error"))
    )
}

#' Report coverage, and flag anything that failed
#'
#' A timeout or an error is not a coverage number, it is a defect, and pooling the two hides it. The
#' error state exists precisely because a crash used to be indistinguishable from a clean miss.
#'
#' @param .con Store connection.
#' @param .n_doc Documents in the sample.
#' @return The coverage table, invisibly.
ent_report_coverage <- function(.con, .n_doc) {
  if (FALSE) {
    .con   <- ner_db_connect(.db_path = .lP$Output$Store, .read_only = TRUE)
    .n_doc <- 4398L
  }

  tab_ <- ent_coverage(.con = .con, .n_doc = .n_doc)
  tbl_say(.tab = tab_, .title = "Coverage by combination and label")

  bad_ <- tab_ |>
    dplyr::filter(dplyr::coalesce(.data$timeout, 0L) + dplyr::coalesce(.data$error, 0L) > 0L)

  if (nrow(bad_) > 0L) {
    tbl_say(.tab = dplyr::select(bad_, "Engine", "Model", "Label",
                                 dplyr::any_of(c("timeout", "error"))),
            .title = "Documents that did not complete")
    cli::cli_alert_warning(
      "A timeout or an error is a defect rather than a coverage number, and is reported apart from
       nohit for that reason."
    )
  } else {
    cli::cli_alert_success("No timeouts and no errors.")
  }

  invisible(tab_)
}


# 4. Report: validation ------------------------------------------------------------------------------------------------

#' Verify the offset contract against the sample text
#'
#' THE INVARIANT EVERYTHING RESTS ON. text[Start:Stop] == Span, over code points. stringi::stri_sub
#' because base substr indexes bytes and misaligns about ninety-nine per cent of spans on this
#' corpus.
#'
#' @param .con Store connection.
#' @param .tab_text The sample, carrying DocID and TextRaw.
#' @param .n Documents to sample, or NULL for all of them.
#' @return Tibble of failures; empty is a pass.
ent_check_offsets <- function(.con, .tab_text, .n = NULL) {
  if (FALSE) {
    .con      <- ner_db_connect(.db_path = .lP$Output$Store, .read_only = TRUE)
    .tab_text <- arrow::read_parquet(.lP$Output$Sample)
    .n        <- NULL
  }

  txt_ <- if (is.null(.n)) .tab_text else dplyr::slice_sample(.tab_text, n = .n)

  DBI::dbGetQuery(.con, "SELECT * FROM candidates WHERE Start IS NOT NULL") |>
    tibble::as_tibble() |>
    dplyr::inner_join(dplyr::select(txt_, "DocID", "TextRaw"), by = dplyr::join_by(DocID)) |>
    dplyr::mutate(Cut = stringi::stri_sub(.data$TextRaw, .data$Start + 1L, .data$Stop)) |>
    dplyr::filter(.data$Cut != .data$Span) |>
    dplyr::select("DocID", "Engine", "Model", "Label", "Start", "Stop", "Span", "Cut")
}

#' Verify that every sample document reached every requested combination
#'
#' A document missing from the ledger for one combination is not visible in any span count, because
#' a document with no spans and a document never processed produce the same empty result.
#'
#' @param .con Store connection.
#' @param .doc_ids Sample document identifiers.
#' @param .grid Output of ner_grid().
#' @return Tibble: Engine, Model, Label, NMissing.
ent_check_ledger <- function(.con, .doc_ids, .grid) {
  if (FALSE) {
    .con     <- ner_db_connect(.db_path = .lP$Output$Store, .read_only = TRUE)
    .doc_ids <- arrow::read_parquet(.lP$Output$Sample, col_select = "DocID")$DocID
    .grid    <- ner_grid(.describe = ner_describe())
  }

  .grid |>
    dplyr::mutate(NMissing = purrr::pmap_int(
      list(.data$Engine, .data$Model, .data$Label),
      \(.e, .m, .l) length(ner_db_missing(
        .con = .con, .doc_ids = .doc_ids, .engine = .e, .model = .m, .label = .l
      ))
    )) |>
    dplyr::select("Engine", "Model", "Label", "NMissing")
}

#' Run every validation and report it as one block
#'
#' The single block to copy out when something needs checking.
#'
#' @param .con Store connection.
#' @param .tab_text The sample.
#' @param .grid Output of ner_grid().
#' @return List of the individual results, invisibly.
ent_report_validation <- function(.con, .tab_text, .grid) {
  if (FALSE) {
    .con      <- ner_db_connect(.db_path = .lP$Output$Store, .read_only = TRUE)
    .tab_text <- arrow::read_parquet(.lP$Output$Sample)
    .grid     <- ner_grid(.describe = ner_describe())
  }

  off_ <- ent_check_offsets(.con = .con, .tab_text = .tab_text)
  if (nrow(off_) == 0L) {
    cli::cli_alert_success("Offset contract holds on every span in the store.")
  } else {
    tbl_say(.tab = utils::head(off_, 20L), .title = "OFFSET FAILURES")
    cli::cli_abort("{nrow(off_)} span{?s} do not round-trip. Nothing downstream can be trusted.")
  }

  led_ <- ent_check_ledger(.con = .con, .doc_ids = .tab_text$DocID, .grid = .grid)
  if (all(led_$NMissing == 0L)) {
    cli::cli_alert_success("Every document reached every requested combination.")
  } else {
    tbl_say(.tab = dplyr::filter(led_, .data$NMissing > 0L), .title = "INCOMPLETE COMBINATIONS")
  }

  invisible(list(Offsets = off_, Ledger = led_))
}


# 5. Report: agreement -------------------------------------------------------------------------------------------------

#' Cross-engine agreement on identical spans, for labels with more than one producer
#'
#' EXACT OFFSETS ONLY. Two engines agreeing that a document mentions an organisation is not agreement
#' about which characters that organisation occupies, and only the second is checkable without
#' judgement.
#'
#' @param .con Store connection.
#' @param .label Label to compare.
#' @return Tibble: EngineA, ModelA, EngineB, ModelB, Both, OnlyA, OnlyB, Jaccard.
ent_agreement <- function(.con, .label) {
  if (FALSE) {
    .con   <- ner_db_connect(.db_path = .lP$Output$Store, .read_only = TRUE)
    .label <- "GPE"
  }

  tab_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT DocID, Start, Stop, Engine, Model FROM candidates
      WHERE Label = '{.label}' AND Start IS NOT NULL"
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(Combo = paste0(.data$Engine, ":", .data$Model))

  combos_ <- sort(unique(tab_$Combo))
  if (length(combos_) < 2L) return(tibble::tibble())

  pairs_ <- utils::combn(combos_, 2L, simplify = FALSE)

  purrr::map(pairs_, \(.p) {
    a_ <- dplyr::filter(tab_, .data$Combo == .p[[1L]]) |> dplyr::select("DocID", "Start", "Stop")
    b_ <- dplyr::filter(tab_, .data$Combo == .p[[2L]]) |> dplyr::select("DocID", "Start", "Stop")
    both_ <- nrow(dplyr::inner_join(a_, b_, by = dplyr::join_by(DocID, Start, Stop)))
    tibble::tibble(
      ComboA  = .p[[1L]],
      ComboB  = .p[[2L]],
      Both    = both_,
      OnlyA   = nrow(a_) - both_,
      OnlyB   = nrow(b_) - both_,
      Jaccard = round(both_ / (nrow(a_) + nrow(b_) - both_), 3)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(dplyr::desc(.data$Jaccard))
}

#' Report agreement, skipping labels that cannot have any
#'
#' REDACT and TERM have exactly one producer each, so there is nothing to compare. Saying so once
#' beats emitting two sets of empty panels, and the count comes from the grid rather than from an
#' empty result.
#'
#' @param .con Store connection.
#' @param .producers Output of ner_producers().
#' @return Named list of agreement tables, invisibly.
ent_report_agreement <- function(.con, .producers) {
  if (FALSE) {
    .con       <- ner_db_connect(.db_path = .lP$Output$Store, .read_only = TRUE)
    .producers <- ner_producers(.grid = ner_grid(.describe = ner_describe()))
  }

  single_ <- dplyr::filter(.producers, !.data$Comparable)$Label
  if (length(single_) > 0L) {
    cli::cli_alert_info(
      "Skipped, one producer each: {paste(single_, collapse = ', ')}. Coverage and offsets still
       apply to them; agreement does not exist to be reported."
    )
  }

  out_ <- dplyr::filter(.producers, .data$Comparable)$Label |>
    rlang::set_names() |>
    purrr::map(\(.l) {
      tab_ <- ent_agreement(.con = .con, .label = .l)
      if (nrow(tab_) > 0L) tbl_say(.tab = tab_, .title = paste0("Agreement on exact spans: ", .l))
      tab_
    })

  invisible(out_)
}

#' Where the gazetteer and LexNLP both fire on a place, do they resolve the same one?
#'
#' A COMPARISON THAT DID NOT EXIST BEFORE gazetteer-v2. Both engines emit Iso2 in the US-MN form --
#' the geo lookup was built that way because LexNLP emits it -- so for the first time two independent
#' methods produce the same identifier for the same characters, and disagreement is checkable rather
#' than a matter of reading both outputs.
#'
#' @param .con Store connection.
#' @return Tibble: Both, SameIso2, DiffIso2, OneNull, plus the disagreements.
ent_geo_concordance <- function(.con) {
  if (FALSE) {
    .con <- ner_db_connect(.db_path = .lP$Output$Store, .read_only = TRUE)
  }

  tab_ <- DBI::dbGetQuery(.con, "
    SELECT m.DocID, m.Start, m.Stop, m.Span,
           m.Iso2 AS IsoMatcon, m.NParent, m.MatchKind,
           l.Iso2 AS IsoLexnlp
    FROM gpe_matcon m
    INNER JOIN gpe_lexnlp l
      ON m.DocID = l.DocID AND m.Start = l.Start AND m.Stop = l.Stop") |>
    tibble::as_tibble()

  if (nrow(tab_) == 0L) return(tibble::tibble())

  tab_ |>
    dplyr::mutate(Verdict = dplyr::case_when(
      is.na(.data$IsoMatcon) | is.na(.data$IsoLexnlp) ~ "one null",
      .data$IsoMatcon == .data$IsoLexnlp              ~ "same",
      .default                                        ~ "different"
    ))
}
