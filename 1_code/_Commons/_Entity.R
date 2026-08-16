# _Entity.R: tooling every entity-rule document shares -----------------------------------------------------------------
#
# WHAT THIS FILE IS
# 04B is one document per entity -- 04B1-Rules-ORG, 04B2-Rules-GPE, and so on -- because a single
# document covering five entities would run to several thousand lines, would re-run every other
# entity's sweeps to change one date rule, and would make "everything renders on every pass" the
# reason nothing renders. What those documents have in common lives here.
#
# WHAT BELONGS HERE
# Anything that is about ENTITIES IN GENERAL rather than about one label: reading the canonical text,
# reducing a company name to a comparable key, loading one label out of 04A's store, rehydrating a
# span for reading, checking that an offset pair is real, and the two table shapes every entity
# document reports through.
#
# WHAT DOES NOT BELONG HERE
# Rules. Every threshold, window, cue and role vocabulary is the property of one entity's document
# and stays there. A function that would need an argument saying which entity called it is a rule
# wearing a shared function's clothes.
#
# SOURCE ORDER. This file uses cli, stringi, arrow and the tbl_* helpers, so it is sourced after
# _Utils.R, _NER.R, _Store.R, _Plots.R and _Tables.R and before the entity's own function file.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_anchors <- .lP$Input$Anchors
  .path_text    <- .lP$Input$Text
  .db_path      <- .lP$Input$Store
}


# 0. Vocabulary of the shared artifacts --------------------------------------------------------------------------------
# A level set belongs HERE and not in an entity's own file when it describes an artifact one document
# writes and another reads. 04B1 writes roles_org.parquet with a Role column; 04B2 reads it and never
# sources 04B1, which is the whole point of splitting the family -- so registering OrgRole in 04B1
# leaves 04B2 with a column it cannot order, and it fails at the first plot_factor() rather than at
# load. Anything private to one entity stays in that entity's file.
#
# Sourced after _Plots.R, which rebuilds the registry empty each time it loads.

plot_register_levels(
  .key    = "OrgRole",
  .levels = c("filer", "counterparty", "fragment", "signatory", "other"),
  .short  = c("filer", "counter", "frag", "signer", "other")
)


# 1. Reducing a name -------------------------------------------------------------------------------------------------
# EDGAR writes a name in registration form and a contract writes it in prose. Both are reduced to a
# common key before they are compared. The steps, in order: strip EDGAR's conformed-name artifacts,
# uppercase, an ampersand between spaces to AND, punctuation to space, whitespace collapsed, a
# leading connective dropped, trailing corporate suffixes stripped.
#
# THE DOTTED FORMS ARE IN THE LIST AS SEPARATE TOKENS, and they have to be: punctuation-to-space
# turns "WILLIAMS PARTNERS L.P." into "WILLIAMS PARTNERS L P", and a list holding only "LP" strips
# nothing -- which demoted an exact pair to a family match. Partnerships and funds are common enough
# in this corpus that this is not an edge case.

.ent_suffix <- c("INC", "INCORPORATED", "CORP", "CORPORATION", "LLC", "LLP", "LP", "LTD", "LIMITED",
                 "PLC", "NV", "BV", "SA", "AG", "GMBH", "CO", "COMPANY", "COMPANIES", "TRUST",
                 "NATASSOC", "AND",
                 "L P", "L L C", "L L P", "N V", "B V", "S A", "A G", "P L C", "S A R L")

.ent_lead <- c("MADE BY AND BETWEEN", "BY AND BETWEEN", "BY AND AMONG", "AMONGST", "BETWEEN",
               "AMONG", "AND", "WITH", "THIS", "THE", "DATED", "AS OF")


#' Remove EDGAR's conformed-name artifacts before the name is reduced
#'
#' EDGAR appends a state-of-incorporation marker to the conformed name -- "UGI CORP /PA/" and the
#' backslash variant. The marker is not part of the name, but the reduction would turn it into a
#' trailing token that matches nothing in the contract text. A former-name parenthetical goes for the
#' same reason: it is metadata about the registrant, not a name any contract will write.
#'
#' @param .x Character vector of EDGAR conformed company names.
#' @return Character vector with the artifacts removed.
ent_strip_conformed <- function(.x) {
  if (FALSE) .x <- c("UGI CORP /PA/", "SMITH BARNEY \\DE\\", "ACME INC (FORMERLY: OLD ACME)")

  .x |>
    stringi::stri_replace_all_regex("\\s*\\((FORMERLY|FKA)[^)]*\\)", "") |>
    stringi::stri_replace_all_regex("[/\\\\][A-Za-z]{2,4}[/\\\\]?\\s*$", "") |>
    stringi::stri_trim_both()
}


#' Reduce a company name or a candidate span to a comparable key
#'
#' The corporate suffix is the problem this solves: EDGAR records "BOEING CO" where the contract
#' writes "The Boeing Company". Suffixes are removed with a repeated group rather than a token loop,
#' because "BANK CO LTD" carries three, and the alternation is sorted by DESCENDING LENGTH so a
#' longer form is tried before a prefix of itself.
#'
#' The ampersand maps to AND only BETWEEN SPACES, so "PETROL OIL & GAS" agrees with its written-out
#' form while "AT&T" is left to the punctuation pass.
#'
#' @param .x Character vector of names or spans as recorded.
#' @param .min Integer. Keys shorter than this become NA. One by default, so nothing is discarded
#'   here: the floors that matter are rule parameters applied at match time, where they can be swept.
#' @return Character vector of keys, NA below .min characters.
ent_norm_key <- function(.x, .min = 1L) {
  if (FALSE) {
    .x   <- c("ACME HOLDINGS, INC.", "WILLIAMS PARTNERS L.P.", "PETROL OIL & GAS INC")
    .min <- 1L
  }

  sfx_sorted_ <- .ent_suffix[order(nchar(.ent_suffix), decreasing = TRUE)]
  lead_ <- paste0("^(", paste(.ent_lead, collapse = "|"), ") ")
  sfx_  <- paste0("( (", paste(sfx_sorted_, collapse = "|"), "))+$")

  out_ <- .x |>
    stringi::stri_trans_toupper() |>
    stringi::stri_replace_all_fixed(" & ", " AND ") |>
    stringi::stri_replace_all_regex("[^A-Z0-9 ]", " ") |>
    stringi::stri_replace_all_regex("\\s+", " ") |>
    stringi::stri_trim_both() |>
    stringi::stri_replace_first_regex(lead_, "") |>
    stringi::stri_replace_first_regex(sfx_, "") |>
    stringi::stri_trim_both()

  dplyr::if_else(is.na(out_) | nchar(out_) < .min, NA_character_, out_)
}


#' How many leading tokens two keys share
#'
#' The instrument for the corporate family: "Elephant & Castle Group" against "Elephant & Castle
#' International" share a real corporate prefix and no suffix rule reaches that. LEADING tokens
#' rather than any tokens, because a shared trailing word -- HOLDINGS, PARTNERS, GROUP -- is a
#' drafting convention rather than a relationship.
#'
#' @param .a Character vector of keys.
#' @param .b Character vector of keys, aligned with .a.
#' @return Integer vector: matching tokens from the start, zero where the first differs or either
#'   side is missing.
ent_shared_tokens <- function(.a, .b) {
  if (FALSE) {
    .a <- c("ELEPHANT CASTLE GROUP", "FRANKLIN RESOURCES")
    .b <- c("ELEPHANT CASTLE INTERNATIONAL", "BOEING")
  }

  ta_ <- stringi::stri_split_fixed(.a, " ")
  tb_ <- stringi::stri_split_fixed(.b, " ")

  purrr::map2_int(ta_, tb_, function(.x, .y) {
    n_ <- min(length(.x), length(.y))
    if (n_ == 0L) return(0L)
    eq_ <- .x[seq_len(n_)] == .y[seq_len(n_)]
    if (!isTRUE(eq_[[1L]])) return(0L)
    bad_ <- which(!eq_ | is.na(eq_))
    if (length(bad_) == 0L) as.integer(n_) else as.integer(bad_[[1L]] - 1L)
  })
}


# 2. Input -----------------------------------------------------------------------------------------------------------

#' Document lengths, in code points
#'
#' stringi::stri_length rather than nchar, for the same reason every offset operation in this family
#' uses stri_sub: Python emits code-point offsets, and a byte-based length would disagree with them
#' on any document carrying a multibyte character.
#'
#' @param .path_text 04A's canonical text parquet.
#' @return Tibble: DocID, DocLen. Empty documents are dropped.
ent_doc_lens <- function(.path_text) {
  if (FALSE) .path_text <- .lP$Input$Text

  arrow::read_parquet(.path_text) |>
    dplyr::transmute(DocID, DocLen = stringi::stri_length(.data$TextRaw)) |>
    dplyr::filter(.data$DocLen > 0L)
}


#' Per-document anchor facts, one row per contract
#'
#' Reads 04A's sample table rather than the EDGAR metadata, so every entity document depends on one
#' narrow artifact and none of them can drift from the sample 04A extracted. The registered addresses
#' are carried because the geography rule validates against them, and the filing date because the
#' date rule measures from it -- both are free here and expensive to reach for later.
#'
#' No length floor is applied to the key. AnchorLen is carried and each entity's rule decides what is
#' long enough for equality and what for containment, which are different questions.
#'
#' @param .path_anchors 04A's sample_anchors.parquet.
#' @return Tibble: one row per document with the labels, the company name and key, the addresses and
#'   the filing date.
ent_anchor_keys <- function(.path_anchors) {
  if (FALSE) .path_anchors <- .lP$Input$Anchors

  raw_ <- arrow::read_parquet(.path_anchors)

  # The address columns are used only by the geography document and may be absent from an older
  # artifact. Missing them should not stop the ORG document from rendering.
  addr_ <- intersect(c("BusinessAddress", "MailingAddress"), names(raw_))

  raw_ |>
    dplyr::select(
      DocID, Fold, ClassDetailed, AmendType, CIK, CompanyName, DateFiled, dplyr::all_of(addr_)
    ) |>
    dplyr::rename(Class = ClassDetailed) |>
    dplyr::mutate(
      CompanyClean = ent_strip_conformed(.x = .data$CompanyName),
      DateFiled    = suppressWarnings(anytime::anydate(as.character(.data$DateFiled))),
      AnchorKey    = ent_norm_key(.x = .data$CompanyClean, .min = 1L),
      AnchorTight  = stringi::stri_replace_all_fixed(.data$AnchorKey, " ", ""),
      AnchorLen    = nchar(.data$AnchorKey),
      AnchorTok    = stringi::stri_count_fixed(.data$AnchorKey, " ") + 1L
    )
}


#' Load one label from 04A's store, for one engine combination
#'
#' The store is opened READ-ONLY, so no path through any entity document can write to an artifact it
#' does not own. Each label sits in its own table carrying its own resolved columns, so the extras
#' are named by the caller rather than guessed here.
#'
#' LabelRaw IS A CORE COLUMN AND IS ALWAYS SELECTED. It carries what the engine itself called the
#' span before the store normalised it -- the gazetteer's GeoClass, the redaction extractor's marker
#' kind -- and a rule that needs it has no other way to reach it.
#'
#' @param .db_path 04A's candidate store.
#' @param .lens Tibble from ent_doc_lens().
#' @param .label Character. Store table: "org", "gpe", "date", "money" or "redact".
#' @param .combo Character. Engine combination token, e.g. "lexnlp" or "paper:gazetteer-v1".
#' @param .extras Character vector of label-specific columns to select beside the core ones.
#' @param .quiet Logical. Suppress the count message.
#' @return Tibble: one row per span, with DocLen joined on.
ent_load_label <- function(.db_path, .lens, .label, .combo, .extras = character(), .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Input$Store
    .lens    <- tab_lens
    .label   <- "org"
    .combo   <- "lexnlp"
    .extras  <- c("NameCore", "LegalForm", "Description")
    .quiet   <- FALSE
  }

  con_ <- ner_db_connect(.db_path = .db_path, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  cols_ <- paste(c("DocID", "Start", "Stop", "Span", "LabelRaw", .extras), collapse = ", ")

  raw_ <- DBI::dbGetQuery(con_, paste0(
    "SELECT ", cols_, " ",
    "FROM ", .label, " ",
    "WHERE Start IS NOT NULL ",
    "  AND (CASE WHEN Engine = Model THEN Engine ELSE Engine || ':' || Model END) = '", .combo, "'"
  )) |>
    tibble::as_tibble()

  out_ <- raw_ |>
    dplyr::inner_join(.lens, by = dplyr::join_by(DocID)) |>
    dplyr::arrange(.data$DocID, .data$Start)

  if (!.quiet) {
    cli::cli_alert_success(
      "{(.combo)} / {(.label)}: {format(nrow(out_), big.mark = ',')} \\
       span{cli::qty(nrow(out_))}{?s} over \\
       {format(dplyr::n_distinct(out_$DocID), big.mark = ',')} \\
       document{cli::qty(dplyr::n_distinct(out_$DocID))}{?s}."
    )
  }
  out_
}


# 3. Describing an engine --------------------------------------------------------------------------------------------

#' Share of spans by relative position, weighted two ways
#'
#' Describes the engine and enters no rule. Span-weighted mass can be produced by a handful of
#' table-heavy filings -- 04A measured one document carrying 3,053 organisation spans -- and
#' document-weighted mass cannot. Where the two agree, a shape is a property of contracts; where they
#' diverge, it is a property of a few documents.
#'
#' @param .spans Tibble carrying DocID, Start and DocLen.
#' @param .bins Integer. Bins across relative position.
#' @return Tibble: Weight, Pos, Share.
ent_density <- function(.spans, .bins = 50L) {
  if (FALSE) {
    .spans <- tab_spans
    .bins  <- 50L
  }

  base_ <- dplyr::mutate(
    .spans, Bin = pmin(.bins, floor(.bins * .data$Start / .data$DocLen) + 1L)
  )

  spans_ <- base_ |>
    dplyr::summarise(N = dplyr::n(), .by = Bin) |>
    dplyr::mutate(Weight = "spans", Share = .data$N / sum(.data$N))

  docs_ <- base_ |>
    dplyr::mutate(W = 1 / dplyr::n(), .by = DocID) |>
    dplyr::summarise(N = sum(.data$W), .by = Bin) |>
    dplyr::mutate(Weight = "documents", Share = .data$N / sum(.data$N))

  dplyr::bind_rows(spans_, docs_) |>
    dplyr::mutate(Pos = (.data$Bin - 0.5) / .bins) |>
    dplyr::select(Weight, Pos, Share)
}


#' How far a span runs past the name or value it resolves to
#'
#' Every engine in this family emits a span and, separately, something resolved from it. The two
#' disagree at the edges, and how much they disagree decides which rules can use the offsets. A rule
#' comparing a position against a cutoff thousands of characters away is insensitive to it; a rule
#' assigning one span to its NEAREST neighbour is not, because the governing scale there is the gap
#' between neighbours.
#'
#' HoldsName is reported beside the overshoot because the overshoot understates the displacement
#' wherever it is FALSE: a span like ", a signer of the foregoing i" contains none of the name it
#' resolves to, so its offsets sit past the name rather than around it.
#'
#' @param .tab Tibble carrying SpanWidth, NameLen, Overshoot and SpanHoldsName.
#' @return Tibble: one row of quantiles and the share of spans holding their own resolved value.
ent_boundary <- function(.tab) {
  if (FALSE) .tab <- tab_ent

  .tab |>
    dplyr::summarise(
      Entities     = dplyr::n(),
      MedNameLen   = stats::median(.data$NameLen),
      MedSpanWidth = stats::median(.data$SpanWidth),
      MedOvershoot = stats::median(.data$Overshoot),
      P90Overshoot = unname(stats::quantile(.data$Overshoot, 0.9)),
      PctOver20    = mean(.data$Overshoot > 20L),
      PctHoldsName = mean(.data$SpanHoldsName)
    )
}


#' Print the boundary block
#' @param .tab Tibble from ent_boundary().
#' @param .note Character. One sentence saying what this entity's rules do with the answer.
#' @return Invisibly .tab.
ent_report_boundary <- function(.tab, .note = "") {
  if (FALSE) {
    .tab  <- tab_boundary
    .note <- "The party rule reads positions and is insensitive to a ragged boundary."
  }

  cli::cli_h2("Span boundaries against resolved values")
  .tab |>
    dplyr::mutate(
      PctOver20    = tbl_pct(.data$PctOver20),
      PctHoldsName = tbl_pct(.data$PctHoldsName)
    ) |>
    tbl_say(.title = "How far a span runs past what it resolves to")

  if (nzchar(.note)) cli::cli_alert_info(.note)
  invisible(.tab)
}


# 4. Reading spans back ----------------------------------------------------------------------------------------------
# Every table an entity document prints is equally consistent with a span being the thing the rule
# thinks it is and with it being a letterhead, a page footer or a defined term. Only the text
# distinguishes them, so a fixed sample is read on every render.

#' Rehydrate a sample of spans with the text either side
#'
#' Sampled deterministically, so the same documents appear on every render and a change in the
#' examples means a change in the extraction rather than a change in the draw.
#'
#' @param .tab Tibble carrying DocID and the two offset columns.
#' @param .path_text 04A's canonical text parquet.
#' @param .col_start Character. Name of the start-offset column.
#' @param .col_stop Character. Name of the stop-offset column.
#' @param .n Integer. Rows drawn.
#' @param .ctx Integer. Characters either side of the span.
#' @param .seed Integer. Sampling seed.
#' @return .tab's columns for the drawn rows, plus Snippet. Empty tibble where nothing was drawable.
ent_read_spans <- function(.tab, .path_text, .col_start, .col_stop, .n = 10L, .ctx = 200L,
                           .seed = 42L) {
  if (FALSE) {
    .tab       <- dplyr::filter(tab_party, .data$Status == "matched")
    .path_text <- .lP$Input$Text
    .col_start <- "PartyStart"
    .col_stop  <- "PartyStop"
    .n         <- 10L
    .ctx       <- 200L
    .seed      <- 42L
  }

  pick_ <- .tab |>
    dplyr::filter(!is.na(.data[[.col_start]])) |>
    (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n, nrow(.d)))))()

  if (nrow(pick_) == 0L) return(tibble::tibble())

  pick_ |>
    dplyr::left_join(
      arrow::read_parquet(.path_text) |> dplyr::filter(.data$DocID %in% pick_$DocID),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::mutate(
      # 0-based half-open offsets over code points; stri_sub is 1-based and inclusive, hence the +1.
      # The left edge is clamped, because a negative "from" counts from the END and would silently
      # return a slice from the wrong part of the document.
      Snippet = stringi::stri_replace_all_regex(
        paste0(
          "...",
          stringi::stri_sub(.data$TextRaw,
                            from = pmax(1L, .data[[.col_start]] + 1L - .ctx),
                            to   = .data[[.col_start]]),
          " >>>",
          stringi::stri_sub(.data$TextRaw,
                            from = .data[[.col_start]] + 1L,
                            to   = .data[[.col_stop]]),
          "<<< ",
          stringi::stri_sub(.data$TextRaw,
                            from = .data[[.col_stop]] + 1L,
                            to   = .data[[.col_stop]] + .ctx),
          "..."
        ),
        "\\s+", " "
      )
    ) |>
    dplyr::select(-TextRaw)
}


#' Cut the text at the stored offsets and compare it to the stored span
#'
#' AN EQUALITY, NOT A CONTAINMENT. Asking whether the span appears somewhere inside the slice is
#' answered YES more easily the wider the slice runs, so a pair of offsets bracketing four pages of
#' contract would pass it every time. The slice must BE the span, which is the only form that can
#' fail -- and it is what catches an entity whose offsets were assembled from two different mentions.
#'
#' NAMED ent_verify_span RATHER THAN ent_verify_span, because 04A already owns that name for a
#' different job -- checking the whole store against the text, from a database path. This file is
#' sourced BEFORE 03A and 04A, so any name they also define is silently overwritten and fails only at
#' the call site. Every function here has been checked against theirs.
#'
#' @param .tab Tibble carrying DocID, the two offset columns and the span column.
#' @param .path_text 04A's canonical text parquet.
#' @param .col_start Character. Name of the start-offset column.
#' @param .col_stop Character. Name of the stop-offset column.
#' @param .col_span Character. Name of the column holding the span as stored.
#' @param .n Integer. Rows drawn.
#' @param .seed Integer. Sampling seed.
#' @return Tibble: DocID, the offsets, Stored, Sliced and Exact.
ent_verify_span <- function(.tab, .path_text, .col_start, .col_stop, .col_span, .n = 500L,
                            .seed = 42L) {
  if (FALSE) {
    .tab       <- tab_party
    .path_text <- .lP$Input$Text
    .col_start <- "PartyStart"
    .col_stop  <- "PartyStop"
    .col_span  <- "PartySpan"
    .n         <- 500L
    .seed      <- 42L
  }

  pick_ <- .tab |>
    dplyr::filter(!is.na(.data[[.col_start]]), !is.na(.data[[.col_span]])) |>
    (\(.d) withr::with_seed(.seed, dplyr::slice_sample(.d, n = min(.n, nrow(.d)))))()

  if (nrow(pick_) == 0L) return(tibble::tibble())

  pick_ |>
    dplyr::left_join(
      arrow::read_parquet(.path_text) |> dplyr::filter(.data$DocID %in% pick_$DocID),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::transmute(
      DocID,
      Start  = .data[[.col_start]],
      Stop   = .data[[.col_stop]],
      Stored = .data[[.col_span]],
      Sliced = stringi::stri_sub(.data$TextRaw,
                                 from = .data[[.col_start]] + 1L,
                                 to   = .data[[.col_stop]]),
      Exact  = .data$Sliced == .data$Stored
    )
}


#' Print a read block
#'
#' @param .tab Tibble carrying Snippet, from ent_read_spans().
#' @param .cols Character vector of columns shown beside the snippet.
#' @param .title Character. Block title.
#' @return Invisibly .tab.
ent_report_read <- function(.tab, .cols, .title = "Spans in context") {
  if (FALSE) {
    .tab   <- tab_read_matched
    .cols  <- c("CompanyName", "Party", "MatchKind", "PartyStart")
    .title <- "Matched parties in context"
  }

  cli::cli_h2(.title)
  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("Nothing to read: the filter selected no rows carrying an offset.")
    return(invisible(.tab))
  }

  purrr::walk(seq_len(nrow(.tab)), function(.i) {
    row_ <- .tab[.i, ]
    hdr_ <- purrr::map_chr(intersect(.cols, names(row_)), function(.c) {
      paste0(.c, ": ", as.character(row_[[.c]]))
    })
    cli::cli_h3(paste(hdr_, collapse = " | "))
    cli::cli_verbatim(strwrap(row_$Snippet, width = 116, prefix = "  "))
  })
  invisible(.tab)
}


# 5. Table shapes ----------------------------------------------------------------------------------------------------

#' Append an "All documents" row to a per-class summary
#'
#' Every entity reports by contract type with a sample row beneath, so a class figure can be read
#' against the whole without arithmetic. Class is handled as ORDERED CHARACTER rather than as the
#' registered factor, because "All documents" is not a contract type and must not enter the taxonomy
#' that every figure in the monorepo orders its axes by.
#'
#' @param .tab Tibble carrying a Class column, already summarised.
#' @param .fun Function taking the ungrouped source and returning a one-row summary.
#' @param .src Tibble the summary is computed from.
#' @return .tab with the ALL row appended, ordered by the registered class levels.
ent_bind_all <- function(.tab, .fun, .src) {
  if (FALSE) {
    .tab <- tab_where
    .fun <- function(.d) dplyr::summarise(.d, Docs = dplyr::n())
    .src <- tab_counts
  }

  lev_ <- plot_levels("ClassDetailed")

  dplyr::bind_rows(
    dplyr::mutate(.tab, Class = as.character(.data$Class)) |>
      dplyr::arrange(match(.data$Class, lev_)),
    dplyr::mutate(.fun(.src), Class = "All documents", .before = 1L)
  )
}


#' Documents whose count changes between two specifications
#'
#' The instrument for reading an ANOMALY rather than a sample. Where one cell of a sweep table moves
#' differently from every other cell, a random draw from that cell mostly returns documents that did
#' not move; this returns the ones that did.
#'
#' @param .sweep Tibble carrying DocID, Spec and the count column.
#' @param .from Character. Label of the narrower specification.
#' @param .to Character. Label of the wider one.
#' @param .col Character. Name of the count column.
#' @param .min Integer. Smallest increase that counts as a jump.
#' @return Character vector of DocIDs, ordered by the size of the jump.
ent_jump_docs <- function(.sweep, .from, .to, .col = "NCounter", .min = 2L) {
  if (FALSE) {
    .sweep <- tab_sweep_window
    .from  <- "+/- 1,000"
    .to    <- "+/- 2,000"
    .col   <- "NCounter"
    .min   <- 2L
  }

  .sweep |>
    dplyr::filter(.data$Spec %in% c(.from, .to)) |>
    dplyr::select(DocID, Spec, Val = dplyr::all_of(.col)) |>
    tidyr::pivot_wider(names_from = Spec, values_from = Val) |>
    dplyr::mutate(Jump = .data[[.to]] - .data[[.from]]) |>
    dplyr::filter(.data$Jump >= .min) |>
    dplyr::arrange(dplyr::desc(.data$Jump)) |>
    dplyr::pull(.data$DocID)
}
