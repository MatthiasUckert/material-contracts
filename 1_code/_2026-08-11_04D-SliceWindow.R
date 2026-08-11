# _2026-08-11_04D-SliceWindow.R: extracting a head-and-tail window, parked ---------------------------------------------
#
# PARKED, NOT RETIRED. This is the design that let a corpus pass read the FRONT and the BACK of each
# document without splicing them into a string that exists nowhere. A label carrying a tail budget
# was extracted twice, from a head slice and a tail slice, and the tail's offsets were shifted back
# onto the full document before anything read them.
#
# It is out of 04D because every label now reads full text, so the plan emits one slice per engine
# and that slice is the whole document -- the machinery is a no-op. ner_run() covers what remains:
# it takes .max_chars and truncates a prefix, which is the head-only case.
#
# WHAT IT WOULD TAKE TO REVIVE IT. A head-only cap needs nothing from this file. A head-PLUS-TAIL
# cap needs all four functions: ent_slice_plan() reads the budgets off the policy, ent_write_slice()
# cuts the slice and returns the offset shift, ent_run_engine() dispatches to the extractor, and
# ent_extract_chunk() runs the plan and adds the shift back. They were working at the point they
# were parked; the rehearsal in 04D ran through them over two thousand documents.
#
# WHY IT MIGHT COME BACK. The corpus pass projects to roughly eighty hours reading full text. If
# that proves unaffordable, a window is the lever -- and because every candidate carries offsets, a
# window imposed at EXTRACTION is the only kind that cannot be revised afterwards. That asymmetry is
# the argument for reading everything, and it is also the reason this file is kept rather than
# deleted: the decision to reimpose a cap should cost an edit to .WINDOW in 04B, not a rebuild of an
# offset-mapping scheme under time pressure.
#
# ent_run_engine() here also carries the extractor dispatch. If it is revived, check it against
# ent_engine_supported() in 04D, which is the list the plan is validated against.


#' The distinct extractor runs a policy implies
#'
#' Several labels can share an engine under different windows -- LexNLP serves organisations with no
#' tail and dates with one -- so running once per label would extract the same head twice. Grouping
#' by engine and slice runs each combination once and keeps only the labels that asked for it.
#'
#' @param .policy Tibble from ent_read_policy().
#' @return Tibble: Combo, Slice, Chars, Labels (a list column).
ent_slice_plan <- function(.policy) {
  if (FALSE) .policy <- tab_policy

  head_ <- .policy |>
    dplyr::transmute(Combo, Slice = "head", Chars = as.integer(.data$CapChars), Label)
  tail_ <- .policy |>
    dplyr::filter(.data$TailChars > 0L) |>
    dplyr::transmute(Combo, Slice = "tail", Chars = as.integer(.data$TailChars), Label)

  dplyr::bind_rows(head_, tail_) |>
    dplyr::summarise(Labels = list(sort(unique(.data$Label))), .by = c(Combo, Slice, Chars)) |>
    dplyr::arrange(.data$Combo, .data$Slice)
}

##' Which combination tokens this document can actually run
#'
#' The tail slice records the offset it starts at. Every span the extractor returns for it indexes
#' the slice, and adding the shift back is what keeps a corpus offset meaning the same thing as a
#' sample offset. A document shorter than the budget is its own slice and the shift is zero.
#'
#' @param .docs Chunk with DocID and Text.
#' @param .slice "head" or "tail".
#' @param .chars Character budget.
#' @param .path Destination parquet.
#' @return Tibble: DocID, OffsetShift.
ent_write_slice <- function(.docs, .slice, .chars, .path) {
  if (FALSE) {
    .docs  <- docs_
    .slice <- "tail"
    .chars <- 5000L
    .path  <- fs::file_temp(ext = "parquet")
  }

  len_ <- stringi::stri_length(.docs$Text)
  if (identical(.slice, "head")) {
    txt_   <- stringi::stri_sub(.docs$Text, 1L, pmin(len_, .chars))
    shift_ <- rep(0L, nrow(.docs))
  } else {
    shift_ <- pmax(0L, len_ - .chars)
    txt_   <- stringi::stri_sub(.docs$Text, shift_ + 1L, len_)
  }
  fs::dir_create(fs::path_dir(.path))
  arrow::write_parquet(tibble::tibble(DocID = .docs$DocID, TextRaw = txt_), .path)
  tibble::tibble(DocID = .docs$DocID, OffsetShift = as.integer(shift_))
}

#' Dispatch one combo to its extractor
#'
#' The same wrappers 04A ran, so the corpus is extracted by the code the measurement was made on.
#' A combo with no branch aborts rather than returning nothing, because an engine silently absent
#' from a corpus pass is a column of missing variables nobody can explain afterwards.
#'
#' @param .combo Engine token, as in the policy.
#' @param .input,.output Parquet paths.
#' @param .labels Labels to request.
#' @param .max_chars Truncation, or NULL; the slice is already cut, so this is normally NULL.
#' @param .n_process,.batch_size,.timeout Per-combo throughput knobs.
#' @param .device spaCy device string.
#' @param .quiet Passed through.
#' @return Invisibly .output.
ent_run_engine <- function(.combo, .input, .output, .labels, .max_chars = NULL,
                           .n_process = 16L, .batch_size = 64L, .timeout = 0L,
                           .device = "auto", .quiet = TRUE) {
  if (FALSE) {
    .combo  <- "lexnlp"
    .input  <- fs::file_temp(ext = "parquet")
    .output <- fs::file_temp(ext = "parquet")
    .labels <- c("ORG", "DATE")
  }

  parts_  <- strsplit(.combo, ":", fixed = TRUE)[[1]]
  engine_ <- parts_[1]
  model_  <- if (length(parts_) > 1L) paste(parts_[-1], collapse = ":") else engine_

  # DISPATCH ON THE MODEL STEM, NOT THE FULL TAG. A paper extractor's model tag ends in a version --
  # moneyregex-v4, gazetteer-v1 -- and the version exists so that a revised rule set cannot be
  # confused with the one it replaced. Matching the whole tag here would mean a version bump aborts
  # the corpus pass with "no extractor for" a combination that plainly has one, which is the
  # opposite of what the suffix is for.
  stem_ <- sub("-v[0-9]+$", "", model_)

  if (engine_ == "spacy") {
    ner_spacy(.inputs = .input, .output = .output, .labels = .labels, .max_chars = .max_chars,
              .model = model_, .device = if (grepl("trf", model_, fixed = TRUE)) .device else "cpu",
              .batch_size = .batch_size, .n_process = if (grepl("trf", model_, fixed = TRUE)) 1L
              else .n_process, .timeout = .timeout, .quiet = .quiet)
  } else if (engine_ == "lexnlp") {
    ner_lexnlp(.inputs = .input, .output = .output, .labels = .labels, .max_chars = .max_chars,
               .chunk_size = .batch_size, .n_process = .n_process, .timeout = .timeout,
               .quiet = .quiet)
  } else if (engine_ == "paper" && stem_ == "dateregex") {
    ner_dateregex(.inputs = .input, .output = .output, .labels = .labels,
                  .max_chars = .max_chars, .quiet = .quiet)
  } else if (engine_ == "paper" && stem_ == "gazetteer") {
    ner_gazetteer(.inputs = .input, .output = .output, .labels = .labels, .max_chars = .max_chars,
                  .n_process = .n_process, .chunk_size = .batch_size, .timeout = .timeout,
                  .quiet = .quiet)
  } else if (engine_ == "paper" && stem_ == "redaction") {
    ner_redaction(.inputs = .input, .output = .output, .labels = .labels, .max_chars = .max_chars,
                  .n_process = .n_process, .chunk_size = .batch_size, .timeout = .timeout,
                  .quiet = .quiet)
  } else if (engine_ == "paper" && stem_ == "moneyregex") {
    # Absent until the corpus rehearsal found it. This dispatch was written while money's engine was
    # the transformer, so the spaCy branch covered it and no money branch was needed; when 04B moved
    # money to the regex arm on the redaction-reach evidence, the policy and the throughput knobs
    # were updated and this was not. The rehearsal exists to find exactly this, at fifty documents
    # rather than thirty hours in.
    ner_moneyregex(.inputs = .input, .output = .output, .labels = .labels,
                   .max_chars = .max_chars, .n_process = .n_process, .chunk_size = .batch_size,
                   .timeout = .timeout, .quiet = .quiet)
  } else {
    cli::cli_abort(c(
      "No extractor for combo {.val {(.combo)}}.",
      "i" = "Engine {.val {(engine_)}}, model stem {.val {(stem_)}}.",
      "i" = "Dispatch covers: spacy, lexnlp, and paper with stem dateregex, gazetteer, redaction \\
             or moneyregex.",
      "x" = "A policy naming an engine this cannot run would abort a corpus pass part-way through."
    ))
  }
  invisible(.output)
}

#' Extract one chunk under the whole policy and return candidates on full-document offsets
#'
#' @param .docs Chunk with DocID and Text.
#' @param .plan Tibble from ent_slice_plan().
#' @param .dir_work Scratch directory for the slices and the extractor output.
#' @param .knobs Named list of per-combo throughput settings.
#' @param .device spaCy device string.
#' @return Tibble: DocID, Label, Start, Stop, Span, LabelRaw, Engine, Model.
ent_extract_chunk <- function(.docs, .plan, .dir_work, .knobs = list(), .device = "auto") {
  if (FALSE) {
    .docs     <- docs_
    .plan     <- tab_plan
    .dir_work <- fs::path(.dir_main, "Work")
    .knobs    <- .KNOBS
    .device   <- "auto"
  }

  fs::dir_create(.dir_work)
  out_ <- purrr::pmap(.plan, function(Combo, Slice, Chars, Labels) {
    tag_  <- paste0(gsub("[^A-Za-z0-9]", "-", Combo), "_", Slice)
    in_   <- fs::path(.dir_work, paste0("in_", tag_, ".parquet"))
    outp_ <- fs::path(.dir_work, paste0("out_", tag_, ".parquet"))
    if (fs::file_exists(outp_)) fs::file_delete(outp_)

    shift_ <- ent_write_slice(.docs = .docs, .slice = Slice, .chars = Chars, .path = in_)
    knob_  <- .knobs[[Combo]] %||% list()
    ent_run_engine(
      .combo      = Combo,
      .input      = in_,
      .output     = outp_,
      .labels     = Labels,
      .max_chars  = NULL,                       # the slice is already cut
      .n_process  = knob_$n_process  %||% 16L,
      .batch_size = knob_$batch_size %||% 64L,
      .timeout    = knob_$timeout    %||% 0L,
      .device     = .device,
      .quiet      = TRUE
    )
    cand_ <- arrow::read_parquet(outp_) |>
      dplyr::filter(!is.na(.data$Start), .data$Label %in% Labels) |>
      dplyr::left_join(shift_, by = dplyr::join_by(DocID)) |>
      dplyr::mutate(
        Start = as.integer(.data$Start) + .data$OffsetShift,
        Stop  = as.integer(.data$Stop)  + .data$OffsetShift
      ) |>
      dplyr::select(-OffsetShift)
    fs::file_delete(c(in_, outp_)[fs::file_exists(c(in_, outp_))])
    cand_
  }) |>
    purrr::list_rbind()

  # A short document is its whole head slice and its whole tail slice, so the same span arrives
  # twice. Deduplicating on the span itself is exact; deduplicating on the document is not.
  dplyr::distinct(out_, DocID, Label, Start, Stop, Span, .keep_all = TRUE)
}


# 3. Resolution over a chunk -----------------------------------------------------------------------------------------
