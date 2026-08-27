# 04B5-Rules-REDACT: what a contract withheld ----------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# redaction.py marks six kinds of place where text was removed. This file counts them, and it applies
# no filter at all -- there is nothing to choose, only a taxonomy and arithmetic.
#
# ONE FILE COMES OUT AND IT IS LONG
# redact_spans.parquet is one row per marker. Every count the previous release stored -- six per
# class, three totals, two ratios -- is a group-by over it, so a reader who wants a different
# definition writes a predicate instead of taking one.
#
# THE THREE CANDIDATE DEFINITIONS ARE THREE PREDICATES
#   NBracketed  Bracketed          the PUBLISHED definition: 06-Redactions.R matched bracketed text
#   NWithheld   Withheld           redaction rather than omission
#   NRedact     every row          everything the extractor marked
# Bracketed and Withheld are stored rather than derived, and that is deliberate. They are not
# measurements, they are the definitions themselves -- membership in a five-of-six and a four-of-six
# list -- and a reader should not have to reconstruct either from prose to reproduce a published
# figure.
#
# THE MARKER IS MATCHED TO WHAT IT REPLACED, AND MONEY IS THE FIRST CASE
# moneyregex emits a currency with the number removed -- $[***], $** -- and redaction.py marks the
# bracket inside it. The same redaction is therefore in two files, described from two sides: one
# knows a PRICE was withheld, the other knows a MARKER is there. Overlapping their offsets gives each
# marker a RedactedEntity, and NRedactMoney is the count of prices a contract withheld.
#
# MEASURED BEFORE IT WAS BUILT. The offsets overlap rather than abut: $[***] puts the marker five
# characters inside the money span, and the observed gaps run from -9 to +6 with nothing beyond. So
# the test is interval overlap with a small tolerance, and the tolerance is a specification dial
# rather than a constant.
#
# AND THE MATCH IS A VALIDATION RESULT. On the withheld prices -- where a second extractor
# independently establishes that something WAS removed -- roughly half the markers are RedactBare,
# which is the one class no bracketed measure sees. That is a sharper statement of the bare-marker
# problem than a single pathological filing, because the population is one where the redaction is not
# in doubt.
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
      "i" = "Register {?it/them} in RedactKind; red_collapse() then gives {?it/them} a column."
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


# 3. The specification ---------------------------------------------------------------------------------------------------

#' Build one redaction specification
#'
#' ONE DIAL, AND IT IS A TOLERANCE RATHER THAN A REACH. A marker and the withheld money span that
#' contains it are the SAME redaction seen by two extractors, so the test is interval overlap. The
#' tolerance exists only for the adjacent form, where moneyregex stopped at the currency symbol and
#' the marker starts one or two characters later.
#'
#' TEN CHARACTERS, AND THE NUMBER IS MEASURED. The observed gaps between a withheld money span and
#' its nearest marker run from -9 to +6 with nothing beyond, so ten covers the whole cluster and
#' reaches no further. Swept in Selection rather than asserted here.
#'
#' @param .tol Integer. Characters of slack allowed between a marker and a withheld span that do not
#'   overlap outright.
#' @param .label Character or NULL. Overrides the generated label.
#' @return A named list carrying the specification.
red_spec <- function(.tol = 10L, .label = NULL) {
  if (FALSE) {
    .tol   <- 10L
    .label <- NULL
  }

  list(
    Tol   = as.integer(.tol),
    Label = .label %||% paste0("overlap +/- ", .tol, " chars")
  )
}


# 4. The release ---------------------------------------------------------------------------------------------------------

#' Match each marker to the entity it replaced
#'
#' TWO EXTRACTORS ON ONE REDACTION. moneyregex matches a currency whose number was removed and keeps
#' the currency; redaction.py matches the bracket and keeps the class. Neither alone says what was
#' withheld -- the first knows a price is missing, the second knows a marker is there -- and the join
#' of their offsets says both.
#'
#' OVERLAP, NOT PROXIMITY, WITH A TOLERANCE FOR THE ADJACENT FORM. "$[***]" puts the marker inside the
#' money span; "$ [***]" puts it just after. The first is an interval containment and the second is a
#' two-character gap, so the test is overlap OR a gap within the specification's tolerance.
#'
#' NEAREST WINS WHERE SEVERAL COULD MATCH, by absolute gap then by position, so a marker is claimed by
#' at most one withheld span and a span by at most one marker.
#'
#' EXTENSIBLE BY CONSTRUCTION. RedactedEntity is a column rather than a boolean, so DATE and GPE join
#' the same way later without a second rule.
#'
#' @param .marks Tibble from red_load().
#' @param .money Tibble from mny_load(), filtered to Withheld.
#' @param .spec List from red_spec().
#' @return Tibble: DocID, Start, RedactedEntity, RedactedRef, RedactedText.
red_match_entity <- function(.marks, .money, .spec) {
  if (FALSE) {
    .marks <- tab_marks
    .money <- dplyr::filter(tab_money, .data$Withheld)
    .spec  <- .lP$Params$Spec
  }

  if (nrow(.money) == 0L) {
    return(tibble::tibble(DocID = character(0), Start = integer(0),
                          RedactedEntity = character(0), RedactedRef = integer(0),
                          RedactedText = character(0)))
  }

  dplyr::inner_join(
    dplyr::select(.marks, "DocID", "Start", "Stop"),
    dplyr::transmute(.money, .data$DocID, MStart = .data$Start, MStop = .data$Stop,
                     MSpan = .data$Span),
    by = dplyr::join_by(DocID), relationship = "many-to-many"
  ) |>
    dplyr::mutate(
      # OVERLAP IS A TEST ON TWO INTERVALS and not on two points. Half-open spans overlap when each
      # starts before the other ends, which is what catches "$[***]" where the marker sits wholly
      # inside the money span.
      Overlap = .data$Start < .data$MStop & .data$MStart < .data$Stop,
      Gap     = dplyr::if_else(.data$Start >= .data$MStop, .data$Start - .data$MStop,
                               .data$MStart - .data$Stop)
    ) |>
    dplyr::filter(.data$Overlap | .data$Gap <= .spec$Tol) |>
    dplyr::arrange(.data$DocID, .data$Start, dplyr::desc(.data$Overlap), abs(.data$Gap),
                   .data$MStart) |>
    dplyr::slice_head(n = 1L, by = c(DocID, Start)) |>
    dplyr::transmute(
      .data$DocID, .data$Start,
      RedactedEntity = "money",
      RedactedRef    = as.integer(.data$MStart),
      RedactedText   = .data$MSpan
    )
}


#' The release: one row per marker
#'
#' NO FILTER ANYWHERE. Every marker the extractor found is here, with the class it assigned. This is
#' the one entity in the family with nothing to choose: the rule is a taxonomy, and the three
#' candidate definitions are three predicates over one column.
#'
#' Bracketed AND Withheld ARE STORED RATHER THAN DERIVED, and that is the one deliberate exception to
#' this family's habit. They are not measurements about a marker; they ARE the published definitions,
#' membership in a five-of-six and a four-of-six list. A reader reproducing 06-Redactions.R's figure
#' should filter a column rather than reconstruct a class list from prose.
#'
#' @param .marks Tibble from red_load().
#' @param .entity Tibble from red_match_entity().
#' @return Tibble: one row per marker.
red_release_spans <- function(.marks, .entity) {
  if (FALSE) {
    .marks  <- tab_marks
    .entity <- tab_entity
  }

  .marks |>
    dplyr::left_join(.entity, by = dplyr::join_by(DocID, Start)) |>
    dplyr::transmute(
      .data$DocID,
      MarkStart  = as.integer(.data$Start),
      MarkStop   = as.integer(.data$Stop),
      MarkText   = .data$Span,
      .data$Kind,
      Bracketed  = .data$Kind %in% .red_bracketed,
      Withheld   = .data$Kind %in% .red_withheld,
      RedactedEntity = dplyr::coalesce(.data$RedactedEntity, "unmatched"),
      RedactedRef    = .data$RedactedRef,
      RedactedText   = .data$RedactedText
    ) |>
    dplyr::arrange(.data$DocID, .data$MarkStart)
}


#' What every column of the marker file means
#' @param .tab Tibble from red_release_spans().
#' @return Tibble: Column, Grain, Meaning.
red_dictionary_spans <- function(.tab) {
  if (FALSE) .tab <- tab_release

  dict_ <- tibble::tribble(
    ~Column,          ~Grain,     ~Meaning,
    "DocID",          "document", "the contract",
    "MarkStart",      "marker",   "offset of the marker, into 04A's canonical text",
    "MarkStop",       "marker",   "offset one past its last character",
    "MarkText",       "marker",   "the surface form, raw: it slices from the two offsets exactly",
    "Kind",           "marker",   "one of the six classes redaction.py assigns",
    "Bracketed",      "marker",   "one of the five bracketed classes -- THE PUBLISHED DEFINITION",
    "Withheld",       "marker",   "redaction rather than omission; a placeholder is not a redaction",
    "RedactedEntity", "marker",   "what this marker replaced, where another extractor knows",
    "RedactedRef",    "marker",   "offset of the span that says so; null where nothing matched",
    "RedactedText",   "marker",   "that span as written, so the match is readable"
  )

  undoc_  <- setdiff(names(.tab), dict_$Column)
  unseen_ <- setdiff(dict_$Column, names(.tab))
  say_    <- function(.x) if (length(.x) == 0L) "none" else paste(.x, collapse = ", ")

  if (length(undoc_) > 0L || length(unseen_) > 0L) {
    cli::cli_abort(c(
      "The marker dictionary and the released file disagree.",
      "x" = "In the file and undocumented: {say_(undoc_)}.",
      "x" = "Documented and not in the file: {say_(unseen_)}."
    ))
  }

  dict_[match(names(.tab), dict_$Column), ]
}


# 5. The collapse --------------------------------------------------------------------------------------------------------

#' One row per contract, from the marker file
#'
#' DEFINED HERE AND WRITTEN NOWHERE. Nine counts and two ratios, all group-bys over the released file.
#' The export calls this; a count stored as data is a count that can disagree with the rows it came
#' from.
#'
#' THE WORD COUNT IS THE DOCUMENT'S AND NOT THE MARKER'S. It comes from the register, so every ratio
#' below has a denominator that no extractor produced and that a reader can check independently.
#'
#' EVERY CONTRACT GETS A ROW, including those carrying no marker at all. They carry zeros rather than
#' missing values, because a contract that withheld nothing withheld nothing -- a measurement, not a
#' gap -- and a mean over the file should divide by the sample.
#'
#' @param .release Tibble or dataset from red_release_spans().
#' @param .words Tibble from red_words(). Supplies NWords.
#' @param .keys Tibble from ent_anchor_keys(). Supplies the population.
#' @return Tibble: one row per document in .keys.
red_collapse <- function(.release, .words, .keys) {
  if (FALSE) {
    .release <- tab_release
    .words   <- tab_words
    .keys    <- tab_keys
  }

  kinds_ <- plot_levels("RedactKind")

  by_kind_ <- .release |>
    dplyr::summarise(N = dplyr::n(), .by = c(DocID, Kind)) |>
    tidyr::pivot_wider(names_from = "Kind", values_from = "N", names_prefix = "N")

  totals_ <- .release |>
    dplyr::summarise(
      NBracketed   = sum(.data$Bracketed),
      NWithheld    = sum(.data$Withheld),
      NRedact      = dplyr::n(),
      NRedactMoney = sum(.data$RedactedEntity == "money"),
      .by = DocID
    )

  out_ <- .keys |>
    dplyr::select("DocID") |>
    dplyr::left_join(dplyr::select(.words, DocID, NWords), by = dplyr::join_by(DocID)) |>
    dplyr::left_join(by_kind_, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(totals_,  by = dplyr::join_by(DocID))

  # A CLASS THE SAMPLE NEVER SAW STILL GETS A COLUMN OF ZEROS. pivot_wider makes columns from the
  # values present, so an absent class would leave the release one column short and the dictionary
  # would abort -- correctly, and unhelpfully. Naming the columns from the vocabulary makes the shape
  # a property of the taxonomy rather than of this sample.
  miss_ <- setdiff(paste0("N", kinds_), names(out_))
  for (nm_ in miss_) out_[[nm_]] <- 0L

  out_ |>
    dplyr::mutate(
      # NAMED EXPLICITLY AND NOT BY PREFIX. starts_with("N") also matches NWords, which is a
      # denominator from the register rather than a count of markers, and coalescing it to zero would
      # turn a document with no register row into one of infinite redaction density.
      dplyr::across(
        dplyr::any_of(c(paste0("N", kinds_), "NBracketed", "NWithheld", "NRedact",
                        "NRedactMoney")),
        \(.x) as.integer(dplyr::coalesce(.x, 0L))
      ),
      NWords         = as.integer(.data$NWords),
      RedactRatio    = 1000 * .data$NBracketed / pmax(.data$NWords, 1L),
      RedactRatioAll = 1000 * .data$NRedact / pmax(.data$NWords, 1L),
      HasRedact      = .data$NBracketed > 0L,
      HasBare        = .data$NRedactBare > 0L
    ) |>
    dplyr::select("DocID", "NWords", dplyr::all_of(paste0("N", kinds_)),
                  "NBracketed", "NWithheld", "NRedact", "NRedactMoney",
                  "RedactRatio", "RedactRatioAll", "HasRedact", "HasBare") |>
    dplyr::arrange(.data$DocID)
}


#' Apply the rule end to end
#'
#' ONE ENTRY POINT, AND 04D CALLS EXACTLY THIS. The release comes out; the collapse is returned beside
#' it because this document reports on it, and the export computes it again from the file.
#'
#' @param .marks Tibble from red_load().
#' @param .money Tibble from mny_load(). Filtered to Withheld inside.
#' @param .words Tibble from red_words().
#' @param .keys Tibble from ent_anchor_keys().
#' @param .spec List from red_spec().
#' @return A list: Spec, Release, Counts.
red_apply <- function(.marks, .money, .words, .keys, .spec) {
  if (FALSE) {
    .marks <- tab_marks
    .money <- tab_money
    .words <- tab_words
    .keys  <- tab_keys
    .spec  <- .lP$Params$Spec
  }

  entity_  <- red_match_entity(
    .marks = .marks,
    .money = dplyr::filter(.money, .data$Withheld),
    .spec  = .spec
  )
  release_ <- red_release_spans(.marks = .marks, .entity = entity_)

  list(
    Spec    = .spec,
    Release = release_,
    Counts  = red_collapse(.release = release_, .words = .words, .keys = .keys)
  )
}


# 6. What the markers replaced -------------------------------------------------------------------------------------------

#' Withheld prices matched to a marker, and the class of the marker that matched
#'
#' THE VALIDATION RESULT THIS DOCUMENT EXISTS TO PRODUCE. A withheld money span is independent
#' evidence that something WAS redacted -- moneyregex found a currency with the number removed -- so
#' the markers sitting on those spans are a population where the redaction is not in doubt. What
#' share of them a bracketed measure would see is then a statement about the published definition
#' rather than about one unusual filing.
#'
#' @param .release Tibble from red_release_spans().
#' @param .money Tibble from mny_load(), unfiltered.
#' @return Tibble: one row per marker kind, plus the unmatched prices.
red_table_money <- function(.release, .money) {
  if (FALSE) {
    .release <- tab_release
    .money   <- tab_money
  }

  held_ <- sum(.money$Withheld)
  hit_  <- dplyr::filter(.release, .data$RedactedEntity == "money")

  body_ <- hit_ |>
    dplyr::summarise(Markers = dplyr::n(), .by = c(Kind, Bracketed)) |>
    dplyr::mutate(PctMatched = .data$Markers / pmax(nrow(hit_), 1L)) |>
    dplyr::arrange(plot_factor(.data$Kind, .key = "RedactKind"))

  dplyr::bind_rows(
    body_,
    tibble::tibble(
      Kind       = "no marker found",
      Bracketed  = NA,
      Markers    = held_ - nrow(hit_),
      PctMatched = NA_real_
    )
  )
}


# 7. Tables --------------------------------------------------------------------------------------------------------------

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


#' What each class contributes, and to how many documents
#' @param .marks Tibble from red_load().
#' @param .release Tibble from red_collapse().
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
#' @param .release Tibble from red_collapse().
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
#' @param .release Tibble from red_collapse().
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
#' @return Tibble: Kind, Span, PctMoney, and how often that exact span occurs.
red_read_kinds <- function(.release, .n = 4L, .seed = 42L) {
  if (FALSE) {
    .release <- tab_release
    .n       <- 4L
    .seed    <- 42L
  }

  # COLLAPSED WHITESPACE FOR DISPLAY ONLY. A marker written "[ * * * ]" is the same drafting act as
  # "[***]" and printing both wastes a row; the FILE keeps the raw span, and the offsets index it.
  src_ <- .release |>
    dplyr::mutate(
      Shown = stringi::stri_replace_all_regex(dplyr::coalesce(.data$MarkText, ""), "\\s+", " ")
    ) |>
    dplyr::filter(nzchar(.data$Shown))

  # REPLACED RATHER THAN NEAR, and the difference is a guess against a fact. The previous version
  # took the commonest labelled entity within forty characters, which is proximity; RedactedEntity is
  # an OVERLAP -- a second extractor found a currency with its number removed at these very offsets.
  # It answers for money only, so a class dominated by "unmatched" is a class nothing else can speak
  # to rather than one sitting alone in the text.
  freq_ <- src_ |>
    dplyr::summarise(
      Times    = dplyr::n(),
      Docs     = dplyr::n_distinct(.data$DocID),
      # A SHARE AND NOT A MODE. Three thousand markers of sixty-one thousand overlap a withheld
      # price, so the commonest RedactedEntity is "unmatched" on every row of every class and the
      # column says nothing. The share says how often THIS exact span sits on a price -- which is
      # the question, and which varies from nothing to most of them across the spans below.
      PctMoney = mean(.data$RedactedEntity == "money"),
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
    dplyr::select("Kind", Span = "Shown", "Times", "Docs", "PctMoney") |>
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
#' @param .release Tibble from red_collapse().
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


#' Withheld prices by contract type
#'
#' THE RESEARCH VARIABLE THIS DOCUMENT PRODUCES, and it is not a count of markers. NRedactMoney is
#' how many PRICES a contract withheld -- established by two extractors agreeing on the same offsets
#' -- which is a different quantity from how many brackets it carries and much closer to what a study
#' of disclosure would use.
#'
#' PctBracketed IS THE CAVEAT THAT TRAVELS WITH IT. Where a type's withheld prices are mostly marked
#' by unbracketed runs, any measure built on the published definition understates that type
#' specifically rather than uniformly, and a comparison across types inherits the bias.
#'
#' @param .release Tibble from red_release_spans().
#' @param .counts Tibble from red_collapse(), optionally carrying Class.
#' @return Tibble: one row per contract type, and one for the sample.
red_table_money_class <- function(.release, .counts) {
  if (FALSE) {
    .release <- tab_release
    .counts  <- tab_counts
  }

  brack_ <- .release |>
    dplyr::filter(.data$RedactedEntity == "money") |>
    dplyr::summarise(NMoneyBracketed = sum(.data$Bracketed), .by = DocID)

  src_ <- .counts |>
    dplyr::left_join(brack_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(NMoneyBracketed = as.integer(dplyr::coalesce(.data$NMoneyBracketed, 0L)))

  cols_ <- function(.d) {
    dplyr::summarise(
      .d,
      Docs         = dplyr::n(),
      PctAnyPrice  = mean(.data$NRedactMoney > 0L),
      MeanPrices   = mean(.data$NRedactMoney),
      MaxPrices    = max(.data$NRedactMoney),
      PctBracketed = sum(.data$NMoneyBracketed) / pmax(sum(.data$NRedactMoney), 1L),
      .by = dplyr::any_of("Class")
    )
  }

  dplyr::bind_rows(
    dplyr::arrange(cols_(src_), dplyr::desc(.data$Docs)),
    dplyr::mutate(cols_(dplyr::select(src_, -dplyr::any_of("Class"))), Class = "All", .before = 1L)
  )
}


# 8. Report --------------------------------------------------------------------------------------------------------------

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
    dplyr::mutate(
      Times    = format(.data$Times, big.mark = ","),
      PctMoney = tbl_pct_safe(.data$PctMoney)
    ) |>
    tbl_say(.title = "The commonest spans of each class, and one drawn at random")

  cli::cli_alert_info(
    "SIX CLASSES ARE SIX CLAIMS ABOUT WHAT A BRACKET MEANS, and no count decides whether \\
     \"[INTENTIONALLY OMITTED]\" belongs in a redaction measure -- only the span does. Read the Omit \\
     rows against the Redact ones: a placeholder where a schedule was left out is a gap in a contract \\
     and is not confidential treatment, which is why they are separate columns rather than one."
  )
  cli::cli_alert_info(
    "PctMoney IS AN OVERLAP AND NOT A NEIGHBOUR. It is the share of THIS exact span's occurrences \\
     that a withheld money figure covers -- a second extractor finding a currency with its number \\
     removed at those very offsets, so the marker and the price are one redaction seen twice. A zero \\
     means no other extractor speaks to that span, not that it sits alone in the text: only money \\
     currently answers."
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


#' The withheld prices, matched
#' @param .tab Tibble from red_table_money().
#' @return Invisibly .tab.
red_report_money <- function(.tab) {
  if (FALSE) .tab <- tab_money_tab

  cli::cli_h2("What a marker replaced, where a second extractor knows")

  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("No withheld price matched a marker.")
    return(invisible(.tab))
  }

  .tab |>
    dplyr::mutate(
      Markers    = format(.data$Markers, big.mark = ","),
      PctMatched = tbl_pct_safe(.data$PctMatched)
    ) |>
    tbl_say(.title = "Markers sitting on a withheld price, by the class the extractor gave them")

  hit_   <- dplyr::filter(.tab, .data$Kind != "no marker found")
  brack_ <- sum(hit_$Markers[dplyr::coalesce(hit_$Bracketed, FALSE)])
  all_   <- sum(hit_$Markers)

  cli::cli_alert_info(
    "THIS IS A POPULATION WHERE THE REDACTION IS NOT IN DOUBT. moneyregex found a currency with the \\
     number removed, independently of redaction.py, so every marker in this table sits on a place \\
     something WAS withheld from. That is what makes the next line a statement about the published \\
     definition rather than about one unusual filing."
  )
  cli::cli_alert_warning(
    "A BRACKETED MEASURE SEES {tbl_pct_safe(brack_ / max(all_, 1L))} OF THEM. The rest are \\
     RedactBare -- a marker carrying no bracket -- which NBracketed excludes by construction, and \\
     which is therefore invisible to the figure 06-Redactions.R produced."
  )
  cli::cli_alert_info(
    "THE LAST ROW IS WITHHELD PRICES NO MARKER MATCHED. A large count there means the two extractors \\
     disagree about what a redaction is, which is a finding about the extractors; a small one means \\
     they agree and the classes above are the whole story."
  )
  invisible(.tab)
}


#' Withheld prices by contract type, reported
#' @param .tab Tibble from red_table_money_class().
#' @return Invisibly .tab.
red_report_money_class <- function(.tab) {
  if (FALSE) .tab <- tab_money_class

  cli::cli_h2("Which contracts withhold a price")
  .tab |>
    dplyr::mutate(
      Docs       = format(.data$Docs, big.mark = ","),
      MeanPrices = tbl_num(.data$MeanPrices),
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct_safe(.x))
    ) |>
    tbl_say(.title = "Prices a contract withheld, and how many of them a bracket marked")

  cli::cli_alert_info(
    "NRedactMoney IS A COUNT OF PRICES AND NOT OF MARKERS. It counts the places two extractors agree \\
     on -- a currency whose number was removed, with a redaction marker at the same offsets -- so it \\
     measures what a contract withheld rather than how much bracket syntax it contains."
  )
  # THE WORST CLASS IS NAMED FROM THE TABLE AND NOT TYPED HERE. A sentence about a specific contract
  # type is a claim about this sample, so it has to be computed from the sample or it goes stale the
  # first time the corpus changes. Restricted to types carrying a material number of prices, because
  # a class with four of them can score 0% on noise.
  body_ <- .tab |>
    dplyr::filter(.data$Class != "All", .data$MeanPrices * .data$Docs >= 50)

  if (nrow(body_) > 0L) {
    worst_ <- dplyr::slice_min(body_, .data$PctBracketed, n = 1L, with_ties = FALSE)
    all_   <- dplyr::filter(.tab, .data$Class == "All")
    cli::cli_alert_danger(
      "THE PUBLISHED DEFINITION DOES NOT UNDERSTATE UNIFORMLY -- IT REORDERS THE TYPES. Across the \\
       sample it sees {tbl_pct_safe(all_$PctBracketed[[1L]])} of withheld prices. On \\
       {(worst_$Class[[1L]])} it sees {tbl_pct_safe(worst_$PctBracketed[[1L]])}, so a measure built \\
       on brackets reports {tbl_num(worst_$MeanPrices[[1L]] * worst_$PctBracketed[[1L]])} prices \\
       per contract there against {tbl_num(worst_$MeanPrices[[1L]])} actually withheld."
    )
    cli::cli_alert_warning(
      "A COMPARISON ACROSS CONTRACT TYPES THEREFORE INHERITS A BIAS THAT DIFFERS BY CELL, which is a \\
       harder problem than a constant undercount: the ranking of types by redaction is not preserved. \\
       PctBracketed is what says by how much, type by type."
    )
  }
  invisible(.tab)
}


#' What the released file holds
#' @param .release Tibble from red_release_spans().
#' @param .counts Tibble from red_collapse().
#' @return Invisibly the summary.
red_report_release <- function(.release, .counts) {
  if (FALSE) {
    .release <- tab_release
    .counts  <- tab_counts
  }

  cli::cli_h2("The released file")

  out_ <- tibble::tibble(
    Item = c("Markers released",
             "Contracts carrying any marker",
             "Contracts in the collapse",
             "Contracts carrying a bracketed marker",
             "Contracts carrying a bare marker"),
    N    = c(nrow(.release),
             dplyr::n_distinct(.release$DocID),
             nrow(.counts),
             sum(.counts$HasRedact),
             sum(.counts$HasBare))
  ) |>
    dplyr::mutate(N = format(.data$N, big.mark = ","))

  tbl_say(.tab = out_, .title = "redact_spans.parquet, and the collapse it supports")

  cli::cli_alert_info(
    "NO FILTER ANYWHERE. This is the one entity in the family with nothing to choose: the rule is a \\
     taxonomy, and the three candidate definitions are three predicates over one column. Bracketed \\
     and Withheld are stored because they ARE those definitions, not because they are measurements."
  )
  cli::cli_alert_info(
    "THE COLLAPSE IS NOT WRITTEN. red_collapse() produces one row per contract and the export calls \\
     it; this document checks its shape against a dictionary so the export cannot produce a \\
     different one."
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
#' @param .tab Any of this document's dictionary tables.
#' @param .title Character. Heading, since the reporter is shared.
#' @return Invisibly .tab.
red_report_dictionary <- function(.tab, .title = "The columns, in the order the file carries them") {
  if (FALSE) {
    .tab   <- tab_dict
    .title <- "redact_spans.parquet"
  }

  cli::cli_h2(.title)
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
#' @param .release Tibble from red_collapse().
#' @param .marks Tibble from red_load().
#' @param .n_spans Integer. Rows in redact_spans.parquet, which is the released file --
#'   .release is the collapse and has one row per contract rather than per marker.
#' @return Invisibly the table.
red_report_headline <- function(.release, .marks, .n_spans) {
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
    "Prices a marker was found for",          format(sum(.release$NRedactMoney),
                                                     big.mark = ","),
    "Contracts withholding a price",          tbl_pct(mean(.release$NRedactMoney > 0L)),
    "Markers in the released file",           format(.n_spans, big.mark = ",")
  )

  tbl_say(.tab = out_, .title = "Everything this document decided")

  cli::cli_alert_info(
    "THE LAST TWO ROWS ARE WHY THE CLASSES ARE NEVER POOLED. One filing carries a quarter of every \\
     marker in the sample, and it carries them as unbracketed asterisks -- so a measure built on the \\
     published definition never sees it, and one built on every class is decided by it."
  )
  invisible(out_)
}


# 9. Figures -------------------------------------------------------------------------------------------------------------

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
#' @param .release Tibble from red_collapse().
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


#' Markers on a withheld price, bracketed against bare
#'
#' THE VALIDATION RESULT AS A FIGURE. Every bar is a marker class sitting on a place where a second
#' extractor established that a price was removed, so the split between bracketed and bare is the
#' share of an undisputed redaction population that the published definition can and cannot see.
#'
#' @param .tab Tibble from red_table_money().
#' @return A ggplot.
red_plot_money <- function(.tab) {
  if (FALSE) .tab <- tab_money_tab

  .tab |>
    dplyr::filter(.data$Kind != "no marker found") |>
    dplyr::mutate(
      Sees = dplyr::if_else(dplyr::coalesce(.data$Bracketed, FALSE),
                            "a bracketed measure sees it", "it does not")
    ) |>
    plot_bar_stacked(
      .cat   = "Kind",
      .val   = "Markers",
      .fill  = "Sees",
      .key   = "RedactKind",
      .short = TRUE
    ) +
    ggplot2::labs(x = "Markers sitting on a withheld price")
}


#' Where a contract withholds a price, by contract type
#'
#' THE INCIDENCE AND NOT THE VOLUME. A contract withholding one price and a contract withholding forty
#' are one row each here, because the question this answers is which kinds of agreement redact a price
#' at all -- volume is MeanPrices in the table, and it is decided by a handful of heavy filings.
#'
#' READ IT AGAINST PctBracketed IN THE TABLE. The types highest here are not necessarily the types a
#' published measure sees, and where those two orders differ a cross-type comparison built on
#' NBracketed is biased by type rather than uniformly.
#'
#' @param .tab Tibble from red_table_money_class().
#' @return A ggplot.
red_plot_money_class <- function(.tab) {
  if (FALSE) .tab <- tab_money_class

  .tab |>
    dplyr::filter(.data$Class != "All") |>
    plot_bar_ranked(
      .cat      = "Class",
      .val      = "PctAnyPrice",
      .short    = TRUE,
      .pct      = TRUE,   # the helper owns the value scale; .pct makes it a percent axis
      .accuracy = 1
    ) +
    ggplot2::labs(x = "Share of contracts withholding at least one price")
}
