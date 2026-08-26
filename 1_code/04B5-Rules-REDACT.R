# 04B5-Rules-REDACT: what was withheld -----------------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# Counts redaction markers by kind and asks what each one replaced. Function prefix is `red_`.
#
# SIX KINDS, ONE COLUMN EACH, AND THAT IS THE WHOLE RULE
# redaction.py emits a class on every span and this releases all six of them separately:
#
#   RedactExplicit  a bracket naming confidential treatment -- CONFIDENTIAL, REDACT, CTR
#   RedactSymbol    a bracket holding only whitespace and asterisks -- [***], [ * * * ], [*]
#   RedactBlank     a bracket holding only whitespace and underscores -- [___], [__]
#   OmitExplicit    a bracket recording deletion -- INTENTIONALLY, OMITTED, DELETE
#   OmitSymbol      a bracket holding only bullets or ellipses, or three dots
#   RedactBare      an unbracketed run of three or more asterisks
#
# THE VERSION THIS REPLACES THREW FIVE OF THEM AWAY. It tested whether LabelRaw contained "BARE" and
# called everything else "bracketed", so the four published classes and the one addition arrived as a
# single bucket. Releasing the classes separately costs four columns and makes every question below a
# filter rather than a decision taken here.
#
# THE PUBLISHED METHOD IS BRACKETS ONLY, and that is not an inference. 06-Redactions.R extracted
# \\[.+?\\] from uppercased, whitespace-collapsed text and joined a curated indicator list: no
# unbracketed pattern could reach it. So NBracketed -- the five bracketed classes summed -- is the
# quantity Table 3 was built on, and it is released as its own column so the published figure is
# reproducible from this file rather than only from the old code.
#
# REDACT IS NOT OMIT, and pooling them counts absent text as withheld text. "[INTENTIONALLY OMITTED]"
# is a placeholder where a schedule was left out; "[***]" is confidential treatment. Both are gaps in
# a contract and only one is a redaction, so they get separate columns and a reader chooses.
#
# ONE FILING CARRIES 14,689 BARE MARKERS
# 24% of every redaction span in the sample, from a single document. Nothing here removes it: it is a
# real count of a real document and the by-kind columns make it visible without a rule -- its markers
# are entirely RedactBare, so any measure built on the bracketed classes never sees it.
#
# WHAT IT COST WHEN THE KINDS WERE POOLED is the reason the class contrast is computed FOUR WAYS,
# span-weighted and document-weighted, with bare markers and without. Employment: Compensation once
# showed a positional contrast of 13.29 against 1 to 5 everywhere else, and that was one filing. A
# finding surviving all four cells is a finding; one appearing only in the span-weighted pooled cell
# is a flood document.
#
# THE RATIO IS THE COMPARABLE QUANTITY
# Contracts run from roughly 4,200 to 17,400 words by class, so a count of markers is partly a count
# of words. Markers per thousand words is what makes a class or industry comparison mean anything,
# and it is what referee 2 asked for when they wanted redaction intensity by firm-quarter.
#
# THE DENOMINATOR COMES FROM THE REGISTER, AND THIS DOCUMENT OPENS NOTHING
# 02B carries nWords for all 1,771,923 documents. Counting \\S+ over the canonical text works on 4,398
# and cannot work on 1.19 million, because 04C writes no corpus text file. The recount is available as
# an agreement check on the sample and is skipped entirely when no text path is given, which is what
# the corpus run does.
#
# WITH 04B2, 04B3 AND 04B4 ALREADY MOVED TO STORED CUE COLUMNS, no rule in the 04B family needs a
# document. 04D's Cues dial has nothing left to gate.
#
# WHAT A MARKER REPLACED
# A marker beside a currency symbol hid an amount; beside an organisation, a party name. That is a
# proximity question of exactly the kind 04B2 answers, and it is the only thing here that goes beyond
# counting. It is reported and NOT released, because 82% of markers had no labelled entity within
# forty characters and a variable missing four times in five is a diagnostic rather than a measure.
#
# ONE FILE, ONE ROW PER CONTRACT
# What a contract withheld belongs to the contract and to no party in it.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_text <- .lP$Input$Text
  .dir_store <- .lP$Input$Store
}


# 1. Vocabulary ----------------------------------------------------------------------------------------------------------
# THE FIVE BRACKETED CLASSES FIRST, then the one that is not. Order is the order every table reports
# in, and putting RedactBare last is what makes a cumulative reading of the columns run from the
# published definition outwards rather than through it.

plot_register_levels(
  .key    = "RedactKind",
  .levels = c("RedactExplicit", "RedactSymbol", "RedactBlank", "OmitExplicit", "OmitSymbol",
              "RedactBare"),
  .short  = c("explicit", "symbol", "blank", "omit-exp", "omit-sym", "bare")
)

plot_register_levels(
  .key    = "RedactWeight",
  .levels = c("spans, with bare", "spans, no bare", "documents, with bare", "documents, no bare"),
  .short  = c("span+bare", "span", "doc+bare", "doc")
)

plot_register_levels(
  .key    = "RedactNear",
  .levels = c("money", "org", "date", "gpe", "nothing"),
  .short  = c("money", "org", "date", "gpe", "none")
)

#: The five classes a bracket can carry. RedactBare is the sixth and is not one of them.
.red_bracketed <- c("RedactExplicit", "RedactSymbol", "RedactBlank", "OmitExplicit", "OmitSymbol")

#: The classes that mean text was WITHHELD rather than a section left out.
.red_withheld <- c("RedactExplicit", "RedactSymbol", "RedactBlank", "RedactBare")


# 2. Input ---------------------------------------------------------------------------------------------------------------

#' Load the redaction markers, with the class the extractor assigned
#'
#' THE CLASS IS KEPT AS THE EXTRACTOR WROTE IT. The version this replaces collapsed six classes into
#' two by testing whether LabelRaw contained "BARE", which discarded the distinction between a bracket
#' naming confidential treatment and one recording an omitted schedule -- and with it the ability to
#' reproduce the published figure, which was built on brackets alone.
#'
#' AN UNKNOWN CLASS ABORTS. redaction.py's CLASSES tuple and this file's vocabulary are two statements
#' of one list, and a class emitted here and unregistered there would order arbitrarily in every table
#' and appear in no column of the release. Better to stop than to drop it quietly.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .lens Tibble: DocID and DocLen.
#' @param .family Character. The family supplying markers.
#' @param .quiet Logical. Suppress the count message.
#' @return Tibble: one row per marker, with Kind.
red_load <- function(.dir_store, .lens, .family = "matcon", .quiet = FALSE) {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .lens      <- tab_lens
    .family    <- "matcon"
    .quiet     <- FALSE
  }

  out_ <- ent_load_entity(
    .dir_store = .dir_store,
    .family    = .family,
    .entity    = "REDACT",
    .lens      = .lens,
    .extras    = ent_extras(.family, "REDACT"),   # none; the class is in the core LabelRaw
    .quiet     = .quiet
  ) |>
    dplyr::mutate(Kind = .data$LabelRaw)

  known_ <- plot_levels("RedactKind")
  bad_   <- setdiff(unique(out_$Kind), known_)
  if (length(bad_) > 0L) {
    cli::cli_abort(c(
      "redaction.py emits {length(bad_)} class{?es} the vocabulary does not name: \\
       {paste(bad_, collapse = ', ')}.",
      "i" = "Register {?it/them} in RedactKind, and add {?a column/columns} to red_release()."
    ))
  }
  out_
}


#' The denominator, from the register
#'
#' THE REGISTER ALREADY HOLDS IT, and reading it there rather than recounting is what lets this
#' variable reach the corpus. 02B carries nWords for all 1,771,923 documents; counting \\S+ over the
#' canonical text works on 4,398 and cannot work on 1.19 million, because 04C writes no corpus text
#' file.
#'
#' THE RECOUNT IS A CHECK AND NOT A SOURCE. The two are not identical -- one counts whitespace-
#' separated runs in R and the other was computed upstream -- so this reports how far apart they are
#' rather than assuming they agree, and it is the register's figure that is used either way.
#'
#' .path_text = NULL SKIPS IT ENTIRELY, which is what the corpus run passes. Making that legal rather
#' than incidental is what lets this document say it opens nothing: the recount is a sample-scale
#' diagnostic, and a corpus pass that silently fell back to a text file would be reading 1.19 million
#' parquets to check a column it already has.
#'
#' @param .keys Tibble from ent_anchor_keys(), carrying nWords from the register.
#' @param .path_text 04A's canonical text parquet, for the agreement check. NULL skips it.
#' @param .quiet Logical. Suppress the agreement report.
#' @return Tibble: DocID, NWords.
red_words <- function(.keys, .path_text = NULL, .quiet = FALSE) {
  if (FALSE) {
    .keys      <- tab_keys
    .path_text <- .lP$Input$Text
    .quiet     <- FALSE
  }

  reg_ <- dplyr::transmute(.keys, DocID, NWords = as.numeric(.data$nWords))

  if (is.null(.path_text)) {
    if (!.quiet) {
      cli::cli_alert_info(
        "Word counts from the register alone, on {format(sum(!is.na(reg_$NWords)), big.mark = ',')} \\
         {cli::qty(sum(!is.na(reg_$NWords)))}document{?s}. No text was opened, which is the corpus \\
         path: 04C writes no corpus text file, so a recount could not run there at all."
      )
    }
    return(reg_)
  }

  txt_ <- arrow::read_parquet(.path_text) |>
    dplyr::transmute(DocID, TxtWords = stringi::stri_count_regex(.data$TextRaw, "\\S+"))

  out_ <- dplyr::left_join(reg_, txt_, by = dplyr::join_by(DocID))

  if (!.quiet) {
    both_ <- dplyr::filter(out_, !is.na(.data$NWords), !is.na(.data$TxtWords))
    tibble::tibble(
      Item = c("Documents in the sample",
               "Word count from the register",
               "Recounted from the canonical text",
               "Both, and within one per cent",
               "Both, and within ten per cent"),
      N    = c(nrow(out_),
               sum(!is.na(out_$NWords)),
               sum(!is.na(out_$TxtWords)),
               sum(abs(both_$NWords - both_$TxtWords) <= 0.01 * both_$TxtWords),
               sum(abs(both_$NWords - both_$TxtWords) <= 0.10 * both_$TxtWords))
    ) |>
      dplyr::mutate(Share = tbl_pct(.data$N / nrow(out_))) |>
      tbl_say(.title = "The denominator, from two independent sources")

    cli::cli_alert_info(
      "TWO COUNTS OF ONE QUANTITY, and the register's is the one used. The recount exists to say \\
       whether they agree, because where they disagree materially every per-thousand-words figure \\
       below is sensitive to which was taken -- and only the register's can reach the corpus."
    )
  }

  dplyr::select(out_, "DocID", "NWords")
}


# 3. The variables -------------------------------------------------------------------------------------------------------

#' The release: one row per contract, one column per marker class
#'
#' SIX COUNTS AND THREE TOTALS, so every candidate definition is a column rather than a decision taken
#' here. NBracketed is the published one -- 06-Redactions.R matched bracketed text and joined a
#' curated indicator list, so no unbracketed pattern could reach it. NWithheld separates redaction
#' from omission. NRedact is everything.
#'
#' EVERY CONTRACT GETS A ROW, including the ones carrying no marker at all. They carry zeros rather
#' than missing values, because a contract that withheld nothing withheld nothing -- that is a
#' measurement and not a gap, and a mean over the file should divide by the sample.
#'
#' TWO RATIOS AND NOT SIX, because a ratio is a count over a denominator and both are in the file.
#' The two released are the two candidate headline definitions; anything else is one division.
#'
#' @param .marks Tibble from red_load().
#' @param .words Tibble from red_words().
#' @param .keys Tibble from ent_anchor_keys().
#' @return Tibble: one row per contract, sixteen columns.
red_release <- function(.marks, .words, .keys) {
  if (FALSE) {
    .marks <- tab_marks
    .words <- tab_words
    .keys  <- tab_keys
  }

  wide_ <- .marks |>
    dplyr::count(.data$DocID, .data$Kind) |>
    tidyr::pivot_wider(names_from = Kind, values_from = n, values_fill = 0L)

  # NAMED EXPLICITLY RATHER THAN BY PREFIX. pivot_wider() emits a column only for a class that
  # appears somewhere in the data, so a sample holding no OmitSymbol would produce a file with a
  # different schema from one that does, and nothing would say so. Declaring the six here means an
  # absent class is a column of zeros rather than an absent column.
  for (nm_ in setdiff(plot_levels("RedactKind"), names(wide_))) wide_[[nm_]] <- 0L

  .keys |>
    dplyr::select("DocID", "Class", "AmendType") |>
    dplyr::left_join(.words, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(wide_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(plot_levels("RedactKind")),
                    \(.x) as.integer(dplyr::coalesce(.x, 0L)))
    ) |>
    dplyr::transmute(
      .data$DocID, .data$Class, .data$AmendType,
      NWords          = as.integer(.data$NWords),
      NRedactExplicit = .data$RedactExplicit,
      NRedactSymbol   = .data$RedactSymbol,
      NRedactBlank    = .data$RedactBlank,
      NOmitExplicit   = .data$OmitExplicit,
      NOmitSymbol     = .data$OmitSymbol,
      NRedactBare     = .data$RedactBare,
      NBracketed      = .data$RedactExplicit + .data$RedactSymbol + .data$RedactBlank +
                        .data$OmitExplicit + .data$OmitSymbol,
      NWithheld       = .data$RedactExplicit + .data$RedactSymbol + .data$RedactBlank +
                        .data$RedactBare,
      NRedact         = .data$RedactExplicit + .data$RedactSymbol + .data$RedactBlank +
                        .data$OmitExplicit + .data$OmitSymbol + .data$RedactBare,
      RedactRatio     = 1000 * .data$NBracketed / pmax(.data$NWords, 1L),
      RedactRatioAll  = 1000 * .data$NRedact / pmax(.data$NWords, 1L),
      HasRedact       = .data$NBracketed > 0L,
      HasBare         = .data$NRedactBare > 0L
    ) |>
    dplyr::arrange(.data$DocID)
}


# 4. What a marker replaced ------------------------------------------------------------------------------------------

#' The nearest labelled entity to each marker
#'
#' A marker beside a currency symbol hid an amount; beside an organisation, a party name. The nearest
#' span of any other label within reach is the cheapest statement of that, and it is the same
#' instrument 04B2 uses -- a non-equi comparison of offsets, reading no text.
#'
#' REPORTED AND NOT RELEASED. On the earlier check 82% of markers had no labelled entity within forty
#' characters, and a variable missing four times in five is a diagnostic rather than a measure.
#'
#' @param .marks Tibble from red_load().
#' @param .dir_store Directory holding the family databases.
#' @param .lens Tibble: DocID and DocLen.
#' @param .labels Character. Entities to look for.
#' @param .reach Integer. Characters either side of a marker.
#' @param .quiet Logical. Suppress the loader messages.
#' @return .marks with Near added.
red_neighbour <- function(.marks, .dir_store, .lens, .labels = c("MONEY", "GPE", "DATE"),
                          .reach = 40L, .quiet = TRUE) {
  if (FALSE) {
    .marks     <- tab_marks
    .dir_store <- .lP$Input$Store
    .lens      <- tab_lens
    .labels    <- c("MONEY", "GPE", "DATE")
    .reach     <- 40L
    .quiet     <- TRUE
  }

  other_ <- purrr::map(.labels, function(.e) {
    ent_load_entity(
      .dir_store = .dir_store, .family = "matcon", .entity = .e,
      .lens = .lens, .extras = character(0), .quiet = .quiet
    ) |>
      dplyr::transmute(DocID, OtherStart = .data$Start, OtherStop = .data$Stop,
                       NearKind = tolower(.e))
  }) |>
    purrr::list_rbind()

  near_ <- .marks |>
    dplyr::mutate(RowId = dplyr::row_number()) |>
    dplyr::select("RowId", "DocID", "Start", "Stop") |>
    dplyr::inner_join(other_, by = dplyr::join_by(DocID), relationship = "many-to-many") |>
    dplyr::mutate(
      Gap = dplyr::case_when(
        .data$OtherStop  < .data$Start ~ .data$Start - .data$OtherStop,
        .data$OtherStart > .data$Stop  ~ .data$OtherStart - .data$Stop,
        .default                       = 0L
      )
    ) |>
    dplyr::filter(.data$Gap <= .reach) |>
    dplyr::arrange(.data$RowId, .data$Gap) |>
    dplyr::slice_head(n = 1L, by = RowId) |>
    dplyr::select("RowId", "NearKind")

  .marks |>
    dplyr::mutate(RowId = dplyr::row_number()) |>
    dplyr::left_join(near_, by = dplyr::join_by(RowId)) |>
    dplyr::mutate(Near = dplyr::coalesce(.data$NearKind, "nothing")) |>
    dplyr::select(-"RowId", -"NearKind")
}


# 5. Evidence ------------------------------------------------------------------------------------------------------------

#' What each class contributes, and to how many documents
#' @param .marks Tibble from red_load().
#' @param .release Tibble from red_release().
#' @return Tibble: one row per class, and one for every marker.
red_table_kind <- function(.marks, .release) {
  if (FALSE) {
    .marks   <- tab_marks
    .release <- tab_release
  }

  cnt_ <- .marks |>
    dplyr::summarise(Spans = dplyr::n(), Docs = dplyr::n_distinct(.data$DocID), .by = Kind) |>
    dplyr::mutate(
      Share    = .data$Spans / sum(.data$Spans),
      PctDocs  = .data$Docs / nrow(.release),
      MedPerDoc = NA_real_
    ) |>
    dplyr::arrange(plot_factor(.data$Kind, .key = "RedactKind"))

  med_ <- .marks |>
    dplyr::count(.data$DocID, .data$Kind) |>
    dplyr::summarise(MedPerDoc = stats::median(.data$n), MaxPerDoc = max(.data$n), .by = Kind)

  cnt_ |>
    dplyr::select(-"MedPerDoc") |>
    dplyr::left_join(med_, by = dplyr::join_by(Kind))
}


#' The class contrast, computed four ways
#'
#' SPAN-WEIGHTED AND DOCUMENT-WEIGHTED, WITH BARE MARKERS AND WITHOUT. A finding surviving all four
#' cells is a finding; one appearing only in the span-weighted pooled cell is a flood document, and
#' this sample holds one carrying 14,689 bare markers.
#'
#' @param .marks Tibble from red_load().
#' @param .release Tibble from red_release().
#' @return Tibble: one row per class and weighting.
red_contrast <- function(.marks, .release) {
  if (FALSE) {
    .marks   <- tab_marks
    .release <- tab_release
  }

  cls_ <- dplyr::select(.release, "DocID", "Class")

  cell_ <- function(.keep_bare, .weight) {
    src_ <- if (.keep_bare) .marks else dplyr::filter(.marks, .data$Kind != "RedactBare")
    lab_ <- paste0(.weight, if (.keep_bare) ", with bare" else ", no bare")

    src_ |>
      dplyr::left_join(cls_, by = dplyr::join_by(DocID)) |>
      dplyr::summarise(
        Value = if (identical(.weight, "spans")) {
          dplyr::n()
        } else {
          dplyr::n_distinct(.data$DocID)
        },
        .by = Class
      ) |>
      dplyr::mutate(Weight = lab_, Share = .data$Value / sum(.data$Value))
  }

  purrr::map2(rep(c(TRUE, FALSE), each = 2L), rep(c("spans", "documents"), times = 2L),
              \(.b, .w) cell_(.keep_bare = .b, .weight = .w)) |>
    purrr::list_rbind() |>
    dplyr::filter(!is.na(.data$Class))
}


#' Redaction intensity by contract type
#' @param .release Tibble from red_release().
#' @return Tibble: one row per type, and one for the sample.
red_table_class <- function(.release) {
  if (FALSE) .release <- tab_release

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs      = dplyr::n(),
      PctAny    = mean(.data$NBracketed > 0L),
      PctBare   = mean(.data$NRedactBare > 0L),
      MedWords  = stats::median(.data$NWords, na.rm = TRUE),
      MedRatio  = stats::median(.data$RedactRatio[.data$NBracketed > 0L], na.rm = TRUE),
      MeanCount = mean(.data$NBracketed),
      .by = dplyr::any_of("Class")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(.release), dplyr::desc(.data$Docs)),
    dplyr::mutate(cols_(dplyr::select(.release, -"Class")), Class = "All", .before = 1L)
  )
}


# 6. Reading ---------------------------------------------------------------------------------------------------------

#' Real markers of every class, as the extractor found them
#'
#' THE BLOCK THAT DECIDES WHICH CLASSES A HEADLINE VARIABLE SHOULD USE. Six classes are six claims
#' about what a bracket means, and no count can say whether "[INTENTIONALLY OMITTED]" belongs in a
#' redaction measure -- only the span does. Drawn per class so the rare ones appear at all: a random
#' draw across all markers would show RedactSymbol six times.
#'
#' THE SPAN IS IN THE STORE, so this opens no document. It is the marker's own text, cut at extraction
#' from the offsets that produced it.
#'
#' @param .marks Tibble from red_load(), after red_neighbour().
#' @param .n Integer. Distinct spans drawn per class.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: Kind, Span, Near, and how often that exact span occurs.
red_read_kinds <- function(.marks, .n = 4L, .seed = 42L) {
  if (FALSE) {
    .marks <- tab_near
    .n     <- 4L
    .seed  <- 42L
  }

  # COLLAPSED WHITESPACE FOR DISPLAY ONLY. A marker written "[ * * * ]" is the same drafting act as
  # "[***]" and printing both wastes a row; the STORE keeps the raw span, and the offsets index it.
  src_ <- .marks |>
    dplyr::mutate(
      Shown = stringi::stri_replace_all_regex(dplyr::coalesce(.data$Span, ""), "\\s+", " ")
    ) |>
    dplyr::filter(nzchar(.data$Shown))

  freq_ <- src_ |>
    dplyr::summarise(
      Times = dplyr::n(),
      Docs  = dplyr::n_distinct(.data$DocID),
      Near  = names(sort(table(.data$Near), decreasing = TRUE))[[1L]],
      .by = c(Kind, Shown)
    )

  purrr::map(plot_levels("RedactKind"), function(.k) {
    pool_ <- dplyr::filter(freq_, .data$Kind == .k)
    if (nrow(pool_) == 0L) return(tibble::tibble())
    # THE COMMONEST FIRST, THEN A RANDOM ONE. The commonest says what the class mostly is; the random
    # draw is what catches a class whose head looks clean and whose tail does not.
    top_ <- dplyr::slice_max(pool_, order_by = .data$Times, n = max(.n - 1L, 1L), with_ties = FALSE)
    rest_ <- dplyr::anti_join(pool_, top_, by = dplyr::join_by(Kind, Shown))
    rnd_ <- if (nrow(rest_) == 0L) {
      tibble::tibble()
    } else {
      withr::with_seed(.seed, dplyr::slice_sample(rest_, n = 1L))
    }
    dplyr::bind_rows(top_, rnd_)
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      Shown = stringi::stri_sub(.data$Shown, to = 44L),
      Share = .data$Times / sum(.data$Times)
    ) |>
    dplyr::select("Kind", Span = "Shown", "Times", "Docs", "Near") |>
    dplyr::arrange(plot_factor(.data$Kind, .key = "RedactKind"), dplyr::desc(.data$Times))
}


#' One marker of each class, with the text it sits in
#'
#' A COUNT SAYS WHAT A CLASS MATCHES AND A SPAN SAYS WHAT IT MEANS, but neither says what the drafter
#' was doing. "[***]" is unambiguous once you see "$ [***] per share" around it, and "[.]" is not a
#' redaction at all until the sentence says whether something was withheld or a bullet was left in.
#'
#' THE CONTEXT IS A COLUMN, NOT A READ. Every matcon extractor emits CueBefore and CueAfter -- 160
#' characters either side, cut at extraction from the offsets that produced the span -- so this block
#' opens no document, exactly like the three rules that used to.
#'
#' THE COMMONEST SPAN OF EACH CLASS, so this block and the table above are about the same markers:
#' the row printed here is the first row of its class there. Among that span's occurrences, the one
#' with the most context on both sides wins, because a marker at the very start of a document has
#' nothing before it and shows half a picture.
#'
#' WIDTH IS BUDGETED RATHER THAN TRUNCATED AT THE END. The line must fit the console without
#' wrapping, so the span is measured first and what remains is split evenly either side. A fixed
#' context width would wrap on a long span and leave the marker itself off the visible line.
#'
#' @param .marks Tibble from red_load(), carrying CueBefore and CueAfter.
#' @param .width Integer. Characters the printed line may use, excluding the indent.
#' @return Tibble: Kind, Line -- one row per class.
red_read_context <- function(.marks, .width = 108L) {
  if (FALSE) {
    .marks <- tab_marks
    .width <- 108L
  }

  if (!all(c("CueBefore", "CueAfter") %in% names(.marks))) {
    cli::cli_abort(c(
      "The REDACT spans carry no cue columns, so the context block cannot run.",
      "i" = "matcon appends them to every entity at extraction; a store written before that change
             has to be rebuilt."
    ))
  }

  flat_ <- function(.x) {
    stringi::stri_trim_both(
      stringi::stri_replace_all_regex(dplyr::coalesce(.x, ""), "\\s+", " ")
    )
  }

  src_ <- .marks |>
    dplyr::mutate(
      Shown  = flat_(.data$Span),
      Before = flat_(.data$CueBefore),
      After  = flat_(.data$CueAfter)
    ) |>
    dplyr::filter(nzchar(.data$Shown))

  top_ <- src_ |>
    dplyr::count(.data$Kind, .data$Shown) |>
    dplyr::slice_max(order_by = .data$n, n = 1L, by = Kind, with_ties = FALSE) |>
    dplyr::select("Kind", "Shown")

  src_ |>
    dplyr::inner_join(top_, by = dplyr::join_by(Kind, Shown)) |>
    dplyr::mutate(Room = pmin(stringi::stri_length(.data$Before),
                              stringi::stri_length(.data$After))) |>
    dplyr::slice_max(order_by = .data$Room, n = 1L, by = Kind, with_ties = FALSE) |>
    dplyr::mutate(
      # The marker is wrapped in >>> and <<< because the console has no colour here -- cli.num_colors
      # is 1 -- and the span itself is usually brackets, so brackets could not mark it.
      Mark = paste0(">>>", .data$Shown, "<<<"),
      Side = pmax((.width - stringi::stri_length(.data$Mark) - 6L) %/% 2L, 8L),
      Cut1 = stringi::stri_sub(.data$Before, from = -.data$Side),
      Cut2 = stringi::stri_sub(.data$After, to = .data$Side),
      Line = paste0("...", .data$Cut1, .data$Mark, .data$Cut2, "...")
    ) |>
    dplyr::arrange(plot_factor(.data$Kind, .key = "RedactKind")) |>
    dplyr::select("Kind", "Line")
}


#' The documents carrying the most markers
#'
#' READ RATHER THAN AVERAGED. A mean over a distribution with one observation at 14,689 is a fiction,
#' and this is what that observation looks like beside the ones around it.
#'
#' @param .release Tibble from red_release().
#' @param .n Integer. Documents listed.
#' @return Tibble: the heaviest documents.
red_read_heavy <- function(.release, .n = 10L) {
  if (FALSE) {
    .release <- tab_release
    .n       <- 10L
  }

  .release |>
    dplyr::arrange(dplyr::desc(.data$NRedact)) |>
    dplyr::slice_head(n = .n) |>
    dplyr::select("DocID", "Class", "NWords", "NBracketed", "NRedactBare", "NRedact",
                  "RedactRatio", "RedactRatioAll")
}


#' What every column of the released file means
#' @param .tab Tibble from red_release().
#' @return Tibble: Column, Meaning.
red_dictionary <- function(.tab) {
  if (FALSE) .tab <- tab_release

  dict_ <- tibble::tribble(
    ~Column,           ~Meaning,
    "DocID",           "the contract; joins to every other 04 file",
    "Class",           "contract type, from 03A's label spine",
    "AmendType",       "original or amended, from the same spine",
    "NWords",          "the denominator, from 02B's register",
    "NRedactExplicit", "brackets naming confidential treatment: CONFIDENTIAL, REDACT, CTR",
    "NRedactSymbol",   "brackets of whitespace and asterisks: [***], [ * * * ], [*]",
    "NRedactBlank",    "brackets of whitespace and underscores: [___], [__]",
    "NOmitExplicit",   "brackets recording deletion: INTENTIONALLY, OMITTED, DELETE",
    "NOmitSymbol",     "brackets of bullets or ellipses, or three dots",
    "NRedactBare",     "unbracketed runs of three or more asterisks; NOT in the published method",
    "NBracketed",      "the five bracketed classes; THE PUBLISHED DEFINITION",
    "NWithheld",       "the Redact classes only, bare included; omission excluded",
    "NRedact",         "every marker of every class",
    "RedactRatio",     "NBracketed per thousand words -- the comparable quantity",
    "RedactRatioAll",  "NRedact per thousand words",
    "HasRedact",       "did this contract carry a bracketed marker",
    "HasBare",         "did it carry an unbracketed run of asterisks"
  )

  undoc_  <- setdiff(names(.tab), dict_$Column)
  unseen_ <- setdiff(dict_$Column, names(.tab))
  say_    <- function(.x) if (length(.x) == 0L) "none" else paste(.x, collapse = ", ")

  if (length(undoc_) > 0L || length(unseen_) > 0L) {
    cli::cli_abort(c(
      "The dictionary and the released file disagree.",
      "x" = "In the file and undocumented: {say_(undoc_)}.",
      "x" = "Documented and not in the file: {say_(unseen_)}.",
      "i" = "A dictionary that can drift from its file documents nothing."
    ))
  }

  dict_[match(names(.tab), dict_$Column), ]
}


# 7. Report --------------------------------------------------------------------------------------------------------------

#' What each class contributes
#' @param .tab Tibble from red_table_kind().
#' @return Invisibly .tab.
red_report_kind <- function(.tab) {
  if (FALSE) .tab <- tab_kind

  cli::cli_h2("The six marker classes")
  .tab |>
    dplyr::mutate(dplyr::across(c(Share, PctDocs), \(.x) tbl_pct(.x))) |>
    tbl_say(.title = "One row per class, in the order the release carries them")

  cli::cli_alert_info(
    "THE FIRST FIVE ARE BRACKETS AND THE SIXTH IS NOT. 06-Redactions.R matched bracketed text and \\
     joined a curated indicator list, so no unbracketed pattern could reach the published figure -- \\
     which makes the five summed the quantity Table 3 was built on. MaxPerDoc on the RedactBare row \\
     is the flood: one filing, and the by-class columns are what stop it reaching a measure built on \\
     the other five."
  )
  invisible(.tab)
}


#' Real markers of every class
#' @param .tab Tibble from red_read_kinds().
#' @return Invisibly .tab.
red_report_read_kinds <- function(.tab) {
  if (FALSE) .tab <- tab_read

  cli::cli_h2("What each class actually catches")
  .tab |>
    dplyr::mutate(Times = format(.data$Times, big.mark = ",")) |>
    tbl_say(.title = "The commonest spans of each class, and one drawn at random")

  cli::cli_alert_info(
    "SIX CLASSES ARE SIX CLAIMS ABOUT WHAT A BRACKET MEANS, and no count decides whether \\
     \"[INTENTIONALLY OMITTED]\" belongs in a redaction measure -- only the span does. Read the Omit \\
     rows against the Redact ones: a placeholder where a schedule was left out is a gap in a contract \\
     and is not confidential treatment, which is why they are separate columns rather than one."
  )
  cli::cli_alert_info(
    "NEAR IS THE COMMONEST THING WITHIN FORTY CHARACTERS of that exact span. It is reported and not \\
     released, because most markers have nothing labelled beside them at all."
  )
  invisible(.tab)
}


#' The class contrast, four ways
#' @param .tab Tibble from red_contrast().
#' @return Invisibly .tab.
red_report_contrast <- function(.tab) {
  if (FALSE) .tab <- tab_contrast

  cli::cli_h2("The class contrast, computed four ways")

  .tab |>
    dplyr::select("Class", "Weight", "Share") |>
    dplyr::mutate(Share = tbl_pct(.data$Share)) |>
    tidyr::pivot_wider(names_from = Weight, values_from = Share) |>
    tbl_say(.title = "Share of markers by contract type, under each weighting")

  cli::cli_alert_info(
    "A FINDING SURVIVING ALL FOUR CELLS IS A FINDING; one appearing only under span weighting with \\
     bare markers included is a flood document. This sample holds one filing carrying 14,689 bare \\
     markers -- 24% of every span -- and pooling the classes once made Employment: Compensation show \\
     a positional contrast of 13.29 against 1 to 5 everywhere else."
  )
  invisible(.tab)
}


#' Redaction intensity by contract type
#' @param .tab Tibble from red_table_class().
#' @return Invisibly .tab.
red_report_class <- function(.tab) {
  if (FALSE) .tab <- tab_class

  cli::cli_h2("Redaction intensity by contract type")
  .tab |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct(.x)),
      dplyr::across(c(MedRatio, MeanCount), \(.x) tbl_num(.x)),
      MedWords = format(round(.data$MedWords), big.mark = ",")
    ) |>
    tbl_say(.title = "One row per type, and one for the sample")

  cli::cli_alert_info(
    "MedRatio IS THE COMPARABLE QUANTITY and MeanCount is not: contracts run from roughly 4,200 to \\
     17,400 words by class, so a count of markers is partly a count of words. The ratio is taken over \\
     the contracts that carried a bracketed marker at all, because a median over a column that is \\
     mostly zero is zero and says nothing."
  )
  invisible(.tab)
}


#' What the released file holds
#' @param .tab Tibble from red_release().
#' @return Invisibly the summary.
red_report_release <- function(.tab) {
  if (FALSE) .tab <- tab_release

  cli::cli_h2("The released file")

  out_ <- tibble::tibble(
    Item = c("Contracts",
             "Carrying a bracketed marker",
             "Carrying an unbracketed run of asterisks",
             "Carrying neither",
             "Carrying an omission but no redaction"),
    N    = c(nrow(.tab),
             sum(.tab$NBracketed > 0L),
             sum(.tab$NRedactBare > 0L),
             sum(.tab$NRedact == 0L),
             sum((.tab$NOmitExplicit + .tab$NOmitSymbol) > 0L & .tab$NWithheld == 0L))
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$N / nrow(.tab)))

  tbl_say(.tab = out_, .title = "One row per contract, by what it carried")

  cli::cli_alert_info(
    "THE LAST ROW IS WHY OMISSION AND REDACTION ARE SEPARATE COLUMNS. Those contracts left a schedule \\
     out and withheld nothing; a measure pooling the classes counts them as redacting. Every contract \\
     gets a row and a contract carrying no marker carries ZEROS rather than missing values, because \\
     withholding nothing is a measurement and not a gap."
  )
  invisible(out_)
}


#' The documents carrying the most markers
#' @param .tab Tibble from red_read_heavy().
#' @return Invisibly .tab.
red_report_heavy <- function(.tab) {
  if (FALSE) .tab <- tab_heavy

  cli::cli_h2("The documents carrying the most markers")
  .tab |>
    dplyr::mutate(
      dplyr::across(c(NWords, NBracketed, NRedactBare, NRedact), \(.x) format(.x, big.mark = ",")),
      dplyr::across(c(RedactRatio, RedactRatioAll), \(.x) tbl_num(.x))
    ) |>
    tbl_say(.title = "The ten heaviest, read rather than averaged")

  cli::cli_alert_info(
    "READ NBracketed AGAINST NRedactBare ROW BY ROW. The heaviest document in this sample carries \\
     14,689 bare markers and its bracketed count is what a measure built on the published definition \\
     sees instead. Nothing here removes it -- it is a real count of a real document, and the \\
     by-class columns are what make it visible without a rule."
  )
  invisible(.tab)
}


#' One marker of each class, with the text it sits in
#' @param .tab Tibble from red_read_context().
#' @return Invisibly .tab.
red_report_context <- function(.tab) {
  if (FALSE) .tab <- tab_context

  cli::cli_h2("What one marker of each class sits in")

  # PRINTED AS LINES AND NOT A TABLE. A table pads every cell to its column's widest, so one long
  # context would push every other row past the console width and the block would wrap -- which is
  # the one thing a passage meant to be read cannot do.
  purrr::pwalk(.tab, function(Kind, Line) {
    cli::cli_text("{.strong {Kind}}")
    cli::cli_verbatim(paste0("  ", Line))
  })

  cli::cli_alert_info(
    "THE MARKER IS BETWEEN >>> AND <<<, and the text either side is CueBefore and CueAfter -- 160 \\
     characters cut at extraction from the offsets that produced the span, so this block opens no \\
     document. Each is the COMMONEST span of its class, which makes it the first row of that class \\
     in the table above, shown at the occurrence carrying the most context on both sides."
  )
  invisible(.tab)
}


#' The column dictionary
#' @param .tab Tibble from red_dictionary().
#' @return Invisibly .tab.
red_report_dictionary <- function(.tab) {
  if (FALSE) .tab <- tab_dict

  cli::cli_h2("What every column means")
  tbl_say(.tab = .tab, .title = "Seventeen columns, in the order the file carries them")

  cli::cli_alert_info(
    "SIX COUNTS AND THREE TOTALS, so every candidate definition is a column rather than a decision \\
     taken here: NBracketed reproduces the published figure, NWithheld separates redaction from \\
     omission, NRedact is everything. The dictionary is compared with the file's own names, so a \\
     class added to the extractor without a column here aborts this chunk."
  )
  invisible(.tab)
}


#' The rule in one table
#' @param .release Tibble from red_release().
#' @param .marks Tibble from red_load().
#' @return Invisibly the table.
red_report_headline <- function(.release, .marks) {
  if (FALSE) {
    .release <- tab_release
    .marks   <- tab_marks
  }

  cli::cli_h2("The rule in one table")

  any_ <- .release$RedactRatio[.release$NBracketed > 0L]

  out_ <- tibble::tribble(
    ~Item,                                      ~Value,
    "Contracts",                                format(nrow(.release), big.mark = ","),
    "Markers found",                            format(nrow(.marks), big.mark = ","),
    "Of those, bracketed",                      tbl_pct(mean(.marks$Kind != "RedactBare")),
    "Carrying a bracketed marker",              tbl_pct(mean(.release$NBracketed > 0L)),
    "Carrying an unbracketed run",              tbl_pct(mean(.release$NRedactBare > 0L)),
    "Median markers per thousand words",        tbl_num(stats::median(any_)),
    "Markers in the heaviest single document",  format(max(.release$NRedact), big.mark = ","),
    "That document's share of every marker",    tbl_pct(max(.release$NRedact) / nrow(.marks)),
    "Rows in the released file",                format(nrow(.release), big.mark = ",")
  )

  tbl_say(.tab = out_, .title = "Everything this document decided")

  cli::cli_alert_info(
    "THE LAST TWO ROWS ARE WHY THE CLASSES ARE NEVER POOLED. One filing carries a quarter of every \\
     marker in the sample, and it carries them as unbracketed asterisks -- so a measure built on the \\
     published definition never sees it, and one built on every class is decided by it."
  )
  invisible(out_)
}


# 8. Figures -------------------------------------------------------------------------------------------------------------

#' What each class contributes
#' @param .tab Tibble from red_table_kind().
#' @return A ggplot.
red_plot_kind <- function(.tab) {
  if (FALSE) .tab <- tab_kind

  .tab |>
    dplyr::select("Kind", Spans = "Share", Documents = "PctDocs") |>
    tidyr::pivot_longer(cols = c("Spans", "Documents"), names_to = "Measure",
                        values_to = "Share") |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$Share,
      y = plot_factor(.data$Kind, .key = "RedactKind", .short = TRUE, .rev = TRUE),
      fill = .data$Measure
    )) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
    plot_scale_fill_cat(name = NULL) +
    plot_scale_x_pct(.accuracy = 1) +
    ggplot2::labs(x = "Share of markers, and of contracts carrying one", y = NULL) +
    plot_theme(.grid = "x", .legend = "bottom")
}


#' Redaction intensity by contract type
#' @param .release Tibble from red_release().
#' @return A ggplot.
red_plot_ratio <- function(.release) {
  if (FALSE) .release <- tab_release

  .release |>
    dplyr::filter(.data$NBracketed > 0L, .data$RedactRatio > 0) |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$RedactRatio,
      y = stats::reorder(.data$Class, .data$RedactRatio, FUN = stats::median)
    )) +
    ggplot2::geom_boxplot(outlier.size = 0.5, linewidth = 0.4, fill = plot_pal_seq(1L),
                          colour = plot_pal_grey(1L)) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "Bracketed markers per thousand words, log scale", y = NULL) +
    plot_theme(.grid = "x")
}


#' Where the markers sit in a document
#' @param .marks Tibble from red_load().
#' @param .bins Integer. Histogram bins.
#' @return A ggplot.
red_plot_density <- function(.marks, .bins = 50L) {
  if (FALSE) {
    .marks <- tab_marks
    .bins  <- 50L
  }

  .marks |>
    dplyr::filter(!is.na(.data$DocLen), .data$DocLen > 0L) |>
    dplyr::mutate(Pos = .data$Start / .data$DocLen) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Pos)) +
    ggplot2::geom_histogram(bins = .bins, fill = plot_pal_seq(1L), colour = NA) +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(plot_factor(.data$Kind, .key = "RedactKind", .short = TRUE)),
      scales = "free_y"
    ) +
    plot_scale_x_pct(.accuracy = 1) +
    plot_scale_y_count() +
    ggplot2::labs(x = "Position through the document", y = "Markers") +
    plot_theme(.grid = "y")
}


#' What sat beside a marker
#' @param .near Tibble from red_neighbour().
#' @return A ggplot.
red_plot_near <- function(.near) {
  if (FALSE) .near <- tab_near

  .near |>
    dplyr::summarise(Spans = dplyr::n(), .by = Near) |>
    plot_bar_ranked(
      .tab      = _,
      .cat      = "Near",
      .val      = "Spans",
      .key      = "RedactNear",
      .short    = TRUE,
      .accuracy = 1
    ) +
    ggplot2::labs(x = "Markers with that label nearest")
}
