# _Reads.R: the span viewer -- one standalone HTML per document ------------------------------------------------------
#
# WHAT THIS IS
# Every table in the 04 family is consistent with two different stories until the text is read.
# This file writes the text: one self-contained HTML per contract, one tab per engine, every span
# that engine proposed marked in place, and every extra it carried in the tooltip. No Quarto, no
# render step, no server -- a file that opens in a browser and can be sent to a coauthor.
#
# IT TAKES CANDIDATES, NOT A STORE PATH. The parked version read the DuckDB store directly, which
# tied the viewer to a schema that is about to change and made it untestable until the change
# lands. Here the caller supplies a tibble, so the same function renders from the store, from the
# Check-NER-* parquets, or from anything else with the right columns. The renderer knows about
# offsets and nothing about storage.
#
# ONE SEGMENTATION PER TAB, NOT ONE SHARED SEGMENTATION FILTERED BY DROPDOWN. The parked version
# cut the text once at every candidate boundary across ALL engines and toggled marks with
# JavaScript. That is cleverer and it is wrong for this purpose: a boundary introduced by spaCy
# then splits a LexNLP span into two marks even when spaCy is not being viewed, so what a single
# engine did is never shown as that engine did it. Each tab is segmented independently on its own
# spans only. It costs the text N times over and it is the only way the tabs are honest.
#
# THE COST IS REAL AND WORTH STATING. A median contract is 33,000 characters; nine engines make
# that roughly 600 KB of HTML, and the longest decile reaches 4 MB. Across the sample it is a few
# gigabytes on disk. That is affordable and it is not free, which is why the driver caches.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII; stringi::stri_sub
# never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .cands <- arrow::read_parquet("2_output/_Probe/Check-NER-ORG/out/ORG__lexnlp__lexnlp.parquet")
  .text  <- arrow::read_parquet("2_output/04A-EntityExtract/sample_text.parquet")
  .meta  <- arrow::read_parquet("2_output/04A-EntityExtract/sample_anchors.parquet")
}


# 1. Vocabulary ------------------------------------------------------------------------------------------------------
# The core columns every candidate carries. Anything else in the table is an EXTRA and is rendered
# into the tooltip and the candidate list without this file needing to know what it means -- which
# is what lets a label gain a field later without touching the viewer.

.read_core <- c("DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model", "Combo")

# The order labels appear in, intersected with what a document actually has. Fixed for the same
# reason the engine order is fixed: a tab that moves between files cannot be compared by eye.
.read_label_order <- c("ORG", "PERSON", "GPE", "DATE", "MONEY", "REDACT")

# One colour per label, background and text. Chosen for contrast against black body text rather
# than for prettiness: a mark has to be visible in a wall of monospace and still readable.
.read_colour <- c(
  ORG    = "#d1fae5|#065f46",
  PERSON = "#fce7f3|#9d174d",
  GPE    = "#ede9fe|#5b21b6",
  DATE   = "#e0e7ff|#3730a3",
  MONEY  = "#fef3c7|#92400e",
  REDACT = "#ffe4e6|#9f1239"
)


# 2. Escaping and slugs ----------------------------------------------------------------------------------------------

#' Make a string safe to place inside HTML text or an attribute
#'
#' Ampersand FIRST. Escaping it after the others would re-escape the ampersands they introduce and
#' turn "&lt;" into "&amp;lt;", which renders as literal "&lt;" on the page.
#'
#' @param .x Character vector.
#' @return Character vector with the five HTML-significant characters escaped.
read_escape <- function(.x) {
  .x |>
    stringi::stri_replace_all_fixed("&", "&amp;") |>
    stringi::stri_replace_all_fixed("<", "&lt;") |>
    stringi::stri_replace_all_fixed(">", "&gt;") |>
    stringi::stri_replace_all_fixed("\"", "&quot;") |>
    stringi::stri_replace_all_fixed("'", "&#39;")
}


#' A combination token as an HTML-safe identifier
#'
#' "spacy:en_core_web_trf" carries a colon, which is legal in an id attribute and awkward in a CSS
#' selector and in querySelector. Replaced rather than quoted, because a selector that needs
#' escaping is a selector someone will get wrong later.
#'
#' @param .x Character vector of combination tokens.
#' @return Character vector safe as an id.
read_slug <- function(.x) {
  stringi::stri_replace_all_regex(.x, "[^A-Za-z0-9]+", "-")
}


# 3. Styling ---------------------------------------------------------------------------------------------------------

#' The stylesheet, inlined so the file is self-contained
#'
#' Built rather than stored as a constant because the label colours come from .read_colour, and a
#' colour defined in one place and written out in another is a pair that drifts.
#'
#' @return A single character string of CSS.
read_css <- function() {
  marks_ <- purrr::imap_chr(.read_colour, function(.v, .k) {
    parts_ <- strsplit(.v, "|", fixed = TRUE)[[1]]
    paste0(".ner[data-label=\"", .k, "\"]{background:", parts_[1], ";color:", parts_[2], ";}")
  })

  paste0(
    "body{font-family:ui-sans-serif,system-ui,sans-serif;max-width:1100px;margin:24px auto;",
    "padding:0 16px;color:#111827;}\n",
    "h1{font-size:19px;margin:0 0 4px;} .sub{color:#6b7280;font-size:13px;margin:0 0 14px;}\n",
    "table.meta{border-collapse:collapse;font-size:13px;margin:0 0 16px;}\n",
    "table.meta td{border:1px solid #e5e7eb;padding:3px 8px;}\n",
    "table.meta td:first-child{background:#f9fafb;color:#6b7280;white-space:nowrap;}\n",
    ".bar{position:sticky;top:0;background:#fff;padding:8px 0;border-bottom:1px solid #e5e7eb;",
    "z-index:5;}\n",
    ".tabs{display:flex;flex-wrap:wrap;gap:4px;margin-bottom:6px;}\n",
    ".tabs button{font-size:13px;padding:4px 10px;border:1px solid #e5e7eb;background:#f9fafb;",
    "border-radius:4px;cursor:pointer;color:#374151;}\n",
    ".tabs button.on{background:#111827;color:#fff;border-color:#111827;}\n",
    ".tabs button .n{opacity:.6;margin-left:5px;}\n",
    ".tabs.lab button{font-weight:600;}\n",
    ".tabs.eng{margin-left:2px;padding-left:10px;border-left:2px solid #e5e7eb;}\n",
    ".tabs.eng button{font-size:12px;padding:3px 8px;}\n",
    ".tabs.eng[hidden]{display:none;}\n",
    ".pane{display:none;} .pane.on{display:block;}\n",
    ".doc{white-space:pre-wrap;font-family:ui-monospace,Menlo,monospace;font-size:13px;",
    "line-height:1.9;background:#fff;border:1px solid #e5e7eb;border-radius:6px;padding:16px;",
    "margin-top:12px;}\n",
    ".ner{border-radius:3px;padding:0 1px;cursor:help;}\n",
    ".ner.off{background:transparent!important;color:inherit!important;}\n",
    paste(marks_, collapse = "\n"), "\n",
    "details{margin-top:14px;} summary{cursor:pointer;color:#374151;font-size:13px;}\n",
    "table.cand{border-collapse:collapse;width:100%;font-size:12px;margin-top:8px;}\n",
    "table.cand th,table.cand td{border:1px solid #e5e7eb;padding:3px 6px;text-align:left;",
    "vertical-align:top;}\n",
    "table.cand th{background:#f9fafb;position:sticky;top:0;}\n",
    "tr.off{display:none;}\n",
    ".empty{color:#9ca3af;font-style:italic;padding:16px;}\n"
  )
}


#' The behaviour: switch tabs, and filter marks by label
#'
#' Two controls and nothing else. The label filter applies to whichever pane is showing AND to its
#' candidate list, so the page never says one thing in the text and another in the table.
#'
#' @return A single character string of JavaScript.
read_js <- function() {
  paste0(
    "(function(){\n",
    "var labs=document.querySelectorAll('.tabs.lab button');\n",
    "var rows=document.querySelectorAll('.tabs.eng');\n",
    "var panes=document.querySelectorAll('.pane');\n",
    "function showPane(lab, combo){\n",
    "  document.querySelectorAll('.tabs.eng button').forEach(function(b){\n",
    "    b.classList.toggle('on', b.dataset.lab===lab && b.dataset.combo===combo);\n",
    "  });\n",
    "  panes.forEach(function(p){ p.classList.remove('on'); });\n",
    "  var sel='.pane[data-lab=\"'+lab+'\"][data-combo=\"'+combo+'\"]';\n",
    "  var target=document.querySelector(sel);\n",
    "  if(target){ target.classList.add('on'); return; }\n",
    "  var e=document.getElementById('pane-empty');\n",
    "  e.querySelector('.empty').textContent =\n",
    "    'No '+lab+' candidates from '+(combo||'any engine')+' in this document.';\n",
    "  e.classList.add('on');\n",
    "}\n",
    "function showLab(lab){\n",
    "  labs.forEach(function(b){b.classList.toggle('on', b.dataset.lab===lab);});\n",
    "  rows.forEach(function(r){r.hidden = (r.dataset.lab!==lab);});\n",
    "  var first=document.querySelector('.tabs.eng[data-lab=\"'+lab+'\"] button');\n",
    "  showPane(lab, first ? first.dataset.combo : '');\n",
    "}\n",
    "labs.forEach(function(b){\n",
    "  b.addEventListener('click', function(){ showLab(b.dataset.lab); });\n",
    "});\n",
    "document.querySelectorAll('.tabs.eng button').forEach(function(b){\n",
    "  b.addEventListener('click', function(){ showPane(b.dataset.lab, b.dataset.combo); });\n",
    "});\n",
    "if(labs.length) showLab(labs[0].dataset.lab);\n",
    "})();\n"
  )
}
# 4. The tooltip -----------------------------------------------------------------------------------------------------

#' Every extra a candidate carries, as one readable line, for a whole table at once
#'
#' VECTORISED OVER ROWS BECAUSE THE SCALAR VERSION WAS THE COST. Called once per marked segment and
#' once per row of the candidate list, across twenty-eight panes, a per-row builder ran tens of
#' thousands of times per document and each call did its own map over the extra columns. This runs
#' once per pane and loops over COLUMNS -- at most a dozen -- with every step vectorised over rows.
#'
#' LABEL-AGNOSTIC BY CONSTRUCTION. It pastes whatever is not a core column, so ORG's LegalForm,
#' GPE's Iso2, DATE's DateValue and MONEY's Amount all render without this function knowing any of
#' them exist, and a label that gains a field later needs no change here.
#'
#' Null extras are dropped rather than shown empty: a tooltip reading "DateScore= Iso3=" is worse
#' than one that omits them, because absent and empty look identical once they are on the page.
#'
#' @param .cands Candidate rows.
#' @param .extras Character vector of extra column names present in the table.
#' @return Character vector, one tooltip per row, already HTML-escaped.
read_tooltip <- function(.cands, .extras) {
  if (FALSE) {
    .cands  <- dplyr::filter(tab_all, .data$Label == "DATE")
    .extras <- c("DateValue", "DateScore")
  }
  n_ <- nrow(.cands)
  if (n_ == 0L) return(character(0))

  head_ <- paste0(.cands$Label, " | ", .cands$Combo)
  raw_  <- .cands$LabelRaw
  show_ <- !is.na(raw_) & nzchar(raw_) & raw_ != .cands$Label
  head_[show_] <- paste0(head_[show_], " | ", raw_[show_])

  if (length(.extras) == 0L) return(read_escape(head_))

  # One pass per COLUMN, each vectorised over rows. The alternative -- one pass per row -- is the
  # same work reordered into the arrangement R is slowest at.
  tail_ <- rep("", n_)
  for (.k in .extras) {
    v_ <- as.character(.cands[[.k]])
    ok_ <- !is.na(v_) & nzchar(v_)
    if (!any(ok_)) next
    add_ <- paste0(.k, "=", v_[ok_])
    tail_[ok_] <- dplyr::if_else(nzchar(tail_[ok_]), paste0(tail_[ok_], " | ", add_), add_)
  }
  read_escape(dplyr::if_else(nzchar(tail_), paste0(head_, "\n", tail_), head_))
}


# 5. Segmentation ----------------------------------------------------------------------------------------------------

#' Cut one document's text at the span boundaries of ONE engine, and mark the covered pieces
#'
#' Cutting at boundaries rather than wrapping spans directly is what makes overlapping candidates
#' renderable at all: HTML tags cannot interleave, so two spans that partially overlap have no valid
#' nesting. Segments have no such problem, because every segment is either wholly inside a span or
#' wholly outside it.
#'
#' THE LOOP IS OVER CANDIDATES, NOT SEGMENTS, and that is the whole performance story. Boundaries
#' come from the candidates, so segments outnumber them roughly two to one; iterating over segments
#' meant a scalar stri_sub and a scalar paste for each, tens of thousands of times per document once
#' twenty-eight panes are counted. Iterating over candidates is half as many passes, and every pass
#' is vectorised over the segments it claims.
#'
#' Where several candidates cover the same segment -- LexNLP's geoentity resolver emits both readings
#' of an ambiguous place at identical offsets -- the FIRST claims it, which with the sort order below
#' is the earliest-starting and, among nested spans, the outermost.
#'
#' @param .text The document's canonical text, one string.
#' @param .cands Candidates for one document and one combination.
#' @param .extras Character vector of extra column names.
#' @return A single character string of HTML.
read_segments <- function(.text, .cands, .extras) {
  if (FALSE) {
    .text   <- tab_text$TextRaw[1]
    .cands  <- dplyr::filter(tab_all, .data$Combo == "lexnlp", .data$Label == "ORG")
    .extras <- c("LegalForm")
  }
  n_ <- stringi::stri_length(.text)
  if (nrow(.cands) == 0L) return(read_escape(.text))

  cand_ <- dplyr::arrange(.cands, .data$Start, .data$Stop)
  cuts_ <- sort(unique(c(0L, n_, cand_$Start, cand_$Stop)))
  cuts_ <- cuts_[cuts_ >= 0L & cuts_ <= n_]
  if (length(cuts_) < 2L) return(read_escape(.text))

  a_ <- cuts_[-length(cuts_)]
  b_ <- cuts_[-1L]
  keep_ <- b_ > a_
  a_ <- a_[keep_]
  b_ <- b_[keep_]

  # 0-based half-open offsets over code points; stri_sub is 1-based inclusive. One call, not one
  # call per segment.
  segs_ <- stringi::stri_sub(.text, from = a_ + 1L, to = b_)
  esc_  <- read_escape(segs_)

  owner_ <- rep(NA_integer_, length(a_))
  n_hit_ <- integer(length(a_))
  for (.j in seq_len(nrow(cand_))) {
    in_ <- a_ >= cand_$Start[.j] & b_ <= cand_$Stop[.j]
    if (!any(in_)) next
    n_hit_[in_] <- n_hit_[in_] + 1L
    free_ <- in_ & is.na(owner_)
    owner_[free_] <- .j
  }

  marked_ <- !is.na(owner_)
  if (!any(marked_)) return(paste(esc_, collapse = ""))

  tips_ <- read_tooltip(.cands = cand_, .extras = .extras)[owner_[marked_]]
  many_ <- n_hit_[marked_] > 1L
  if (any(many_)) {
    tips_[many_] <- paste0(
      tips_[many_],
      read_escape(paste0("\n(", n_hit_[marked_][many_], " candidates at this position)"))
    )
  }

  out_ <- esc_
  out_[marked_] <- paste0(
    "<span class=\"ner\" data-label=\"", read_escape(cand_$Label[owner_[marked_]]), "\"",
    " title=\"", tips_, "\">", esc_[marked_], "</span>"
  )
  paste(out_, collapse = "")
}


# 6. The candidate list ----------------------------------------------------------------------------------------------

#' Every candidate in one tab as a table, extras included
#'
#' The marked text answers "where"; this answers "what exactly, and with what attached". Both are
#' filtered by the same control, so they cannot disagree.
#'
#' BUILT COLUMN BY COLUMN. The row-wise version nested a map over columns inside a map over rows, so
#' a document with three thousand candidates ran tens of thousands of small allocations to produce a
#' table nobody opens unless they are already suspicious.
#'
#' @param .cands Candidates for one document and one combination.
#' @param .extras Character vector of extra column names.
#' @return A single character string of HTML.
read_cand_table <- function(.cands, .extras) {
  if (FALSE) {
    .cands  <- dplyr::filter(tab_all, .data$Combo == "lexnlp")
    .extras <- c("LegalForm")
  }
  if (nrow(.cands) == 0L) return("")
  cols_ <- c("Start", "Stop", "Label", "LabelRaw", "Span", .extras)

  head_ <- paste0("<tr>", paste0("<th>", read_escape(cols_), "</th>", collapse = ""), "</tr>")

  cells_ <- purrr::map(cols_, function(.k) {
    v_ <- as.character(.cands[[.k]])
    paste0("<td>", dplyr::if_else(is.na(v_), "", read_escape(v_)), "</td>")
  })
  body_ <- Reduce(paste0, cells_)

  rows_ <- paste0("<tr data-label=\"", read_escape(.cands$Label), "\">", body_, "</tr>")

  paste0(
    "<details><summary>", nrow(.cands), " candidate(s)</summary>",
    "<table class=\"cand\"><thead>", head_, "</thead><tbody>",
    paste(rows_, collapse = ""), "</tbody></table></details>"
  )
}
# 7. One document ----------------------------------------------------------------------------------------------------

#' Render one document to a self-contained HTML string
#'
#' ONE PANE PER (LABEL, ENGINE), NOT PER ENGINE. An engine tab holding every label at once shows
#' four colours interleaved and answers no question: LexNLP's twenty-eight candidates in a credit
#' agreement are organisations, places, dates and amounts together, and what is being asked is
#' always "what did THIS engine do to THIS label". The label tier chooses the question and the
#' engine tier chooses who answered it.
#'
#' EVERY PAIR APPEARS IN EVERY FILE, whether or not it found anything here. A tab present in one
#' document and absent from the next cannot be compared by eye, and "this engine found nothing in
#' this contract" is a finding rather than a gap -- it is how the Intrawest lease shows that LexNLP
#' returned no organisations at all. The pair grid therefore comes from the WHOLE candidate set and
#' is passed in, not derived per document.
#'
#' ONE SHARED EMPTY PANE, NOT ONE PER EMPTY PAIR. Every pane for a pair with no candidates would
#' hold an identical copy of the text, and with nine engines over six labels that is up to
#' twenty-eight copies of a document that may only carry hits for nineteen. A single pane holds the
#' plain text once and the caption is rewritten on click.
#'
#' @param .docid The document identifier.
#' @param .text The document's canonical text, one string.
#' @param .cands Candidates for THIS document, carrying Combo.
#' @param .meta Named character vector of metadata rendered in the header, or NULL.
#' @param .pairs Tibble of Label and Combo fixing the full tab grid, or NULL to derive it from
#'   .cands -- which yields a per-document grid and is only right for a single-document call.
#' @param .max_chars Integer or NULL. Truncate the DISPLAYED text, with a visible marker.
#' @param .css,.js The stylesheet and the behaviour, or NULL to build them here. Both are CONSTANT
#'   across documents, so the driver builds them once and passes them in; rebuilt per document they
#'   are four thousand identical string concatenations for nothing.
#' @return A single character string: a complete HTML document.
read_doc_html <- function(.docid, .text, .cands, .meta = NULL, .pairs = NULL,
                          .max_chars = NULL, .css = NULL, .js = NULL) {
  if (FALSE) {
    .docid     <- tab_pick$DocID[1]
    .text      <- tab_text$TextRaw[1]
    .cands     <- dplyr::filter(tab_all, .data$DocID == .docid)
    .meta      <- c(Class = "Leases", Company = "Intrawest")
    .pairs     <- NULL
    .max_chars <- NULL
    .css       <- NULL
    .js        <- NULL
  }

  css_ <- if (is.null(.css)) read_css() else .css
  js_  <- if (is.null(.js)) read_js() else .js

  full_n_ <- stringi::stri_length(.text)
  trunc_  <- !is.null(.max_chars) && full_n_ > .max_chars
  text_   <- if (trunc_) stringi::stri_sub(.text, from = 1L, to = .max_chars) else .text
  n_      <- stringi::stri_length(text_)

  cand_   <- dplyr::filter(.cands, .data$Stop <= n_)
  extras_ <- setdiff(names(.cands), .read_core)
  pairs_  <- if (is.null(.pairs)) read_pair_grid(.cands = .cands) else .pairs
  labs_   <- unique(pairs_$Label)

  lab_tabs_ <- purrr::map_chr(labs_, function(.l) {
    paste0("<button data-lab=\"", read_escape(.l), "\">", read_escape(.l),
           "<span class=\"n\">", sum(cand_$Label == .l), "</span></button>")
  })

  eng_rows_ <- purrr::map_chr(labs_, function(.l) {
    sub_ <- dplyr::filter(pairs_, .data$Label == .l)
    btns_ <- purrr::map_chr(sub_$Combo, function(.c) {
      paste0("<button data-lab=\"", read_escape(.l), "\" data-combo=\"", read_escape(.c), "\">",
             read_escape(.c), "<span class=\"n\">",
             sum(cand_$Label == .l & cand_$Combo == .c), "</span></button>")
    })
    paste0("<div class=\"tabs eng\" data-lab=\"", read_escape(.l), "\" hidden>",
           paste(btns_, collapse = ""), "</div>")
  })

  # Only pairs that actually have candidates get their own pane; the rest fall back to the shared
  # empty one, which is why a file does not grow with the size of the grid.
  hit_ <- pairs_ |>
    dplyr::mutate(N = purrr::map2_int(.data$Label, .data$Combo,
                                      \(.l, .c) sum(cand_$Label == .l & cand_$Combo == .c))) |>
    dplyr::filter(.data$N > 0L)

  panes_ <- purrr::map_chr(seq_len(nrow(hit_)), function(.i) {
    l_ <- hit_$Label[.i]
    c_ <- hit_$Combo[.i]
    sub_ <- dplyr::filter(cand_, .data$Label == l_, .data$Combo == c_)
    paste0(
      "<div class=\"pane\" data-lab=\"", read_escape(l_), "\" data-combo=\"", read_escape(c_),
      "\"><div class=\"doc\">",
      read_segments(.text = text_, .cands = sub_, .extras = extras_), "</div>",
      read_cand_table(.cands = sub_, .extras = extras_), "</div>"
    )
  })

  empty_pane_ <- paste0(
    "<div class=\"pane\" id=\"pane-empty\"><div class=\"empty\"></div>",
    "<div class=\"doc\">", read_escape(text_), "</div></div>"
  )

  meta_rows_ <- if (is.null(.meta)) "" else paste0(
    purrr::imap_chr(.meta, function(.v, .k) {
      paste0("<tr><td>", read_escape(.k), "</td><td>", read_escape(as.character(.v)), "</td></tr>")
    }),
    collapse = ""
  )

  paste0(
    "<!DOCTYPE html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<title>", read_escape(.docid), "</title><style>\n", css_, "</style></head><body>\n",
    "<h1>", read_escape(.docid), "</h1>\n",
    "<p class=\"sub\">", format(n_, big.mark = ","), " characters",
    if (trunc_) paste0(" shown of ", format(full_n_, big.mark = ","), " -- TRUNCATED FOR VIEWING")
    else "",
    " | ", nrow(pairs_), " label-engine pair(s), ", nrow(hit_), " with candidates | ",
    nrow(cand_), " candidate(s)</p>\n",
    if (nzchar(meta_rows_)) paste0("<table class=\"meta\">", meta_rows_, "</table>\n") else "",
    "<div class=\"bar\"><div class=\"tabs lab\">", paste(lab_tabs_, collapse = ""), "</div>",
    paste(eng_rows_, collapse = ""), "</div>\n",
    paste(panes_, collapse = "\n"), "\n", empty_pane_, "\n",
    "<script>\n", js_, "</script>\n</body></html>\n"
  )
}


#' The full label-by-engine grid, in a fixed order
#'
#' Derived once from every candidate rather than per document, so the tab bar is identical in every
#' file. Only pairs that occur SOMEWHERE are included: paper:redaction-v1 emits nothing but REDACT
#' and LexNLP emits no persons, and a Cartesian product would fill the bar with combinations that
#' can never exist.
#'
#' @param .cands The full candidate table, carrying Label and Combo.
#' @param .combos Character vector fixing the engine order, or NULL for alphabetical.
#' @param .labels Character vector fixing the label order, or NULL for .read_label_order.
#' @return Tibble of Label and Combo, ordered.
read_pair_grid <- function(.cands, .combos = NULL, .labels = NULL) {
  if (FALSE) {
    .cands  <- tab_all
    .combos <- NULL
    .labels <- NULL
  }
  combos_ <- if (is.null(.combos)) sort(unique(.cands$Combo)) else .combos
  labs_   <- if (is.null(.labels)) {
    c(intersect(.read_label_order, unique(.cands$Label)),
      setdiff(sort(unique(.cands$Label)), .read_label_order))
  } else {
    .labels
  }

  .cands |>
    dplyr::distinct(.data$Label, .data$Combo) |>
    dplyr::filter(.data$Label %in% labs_, .data$Combo %in% combos_) |>
    dplyr::mutate(
      Label = factor(.data$Label, levels = labs_),
      Combo = factor(.data$Combo, levels = combos_)
    ) |>
    dplyr::arrange(.data$Label, .data$Combo) |>
    dplyr::mutate(dplyr::across(c(Label, Combo), as.character))
}
# 8. The driver ------------------------------------------------------------------------------------------------------

#' Write one HTML file per document, in parallel, skipping those already written
#'
#' CACHED BY DEFAULT, because this runs inside a render. Four thousand documents at several hundred
#' kilobytes each is a few gigabytes and several minutes; a document whose file already exists is
#' left alone. Set .overwrite when the renderer itself changes -- the cache cannot know that the code
#' that produced a file has been edited, which is the one thing it will get wrong.
#'
#' PARALLEL BECAUSE THE WORK IS PURE AND PER DOCUMENT. Building the HTML is string manipulation over
#' one contract's text and its own candidates: no shared state, no ordering, one file written per
#' task under a name no other task uses. The only serial parts are splitting the inputs and, at the
#' end, adding up the bytes.
#'
#' EACH DAEMON SOURCES THIS FILE ONCE, not once per task. A mirai daemon keeps its global
#' environment between tasks, so the guard in the worker is false after the first document it
#' handles -- which makes the setup cost four sources rather than four thousand.
#'
#' @param .text Tibble with DocID and TextRaw.
#' @param .cands Tibble of candidates with the core columns plus Combo and any extras.
#' @param .dir_out Directory for the files; created if absent.
#' @param .meta Tibble with DocID and any columns to show in the header, or NULL.
#' @param .doc_ids Character vector of documents to render, or NULL for every document in .text.
#' @param .combos Character vector fixing the engine order, or NULL for alphabetical.
#' @param .labels Character vector fixing the label order, or NULL for .read_label_order.
#' @param .max_chars Integer or NULL; see read_doc_html().
#' @param .overwrite Logical. TRUE re-renders files that already exist.
#' @param .workers Integer. 1 runs in this session; above that starts that many mirai daemons and
#'   stops them again on exit. NULL takes half the cores, because the parent holds every document's
#'   text and the payloads are copied into the daemons.
#' @param .source Path to this file, sourced inside each daemon. The default is where the project
#'   keeps it; a worker cannot find it any other way, because a daemon is a bare R session with no
#'   knowledge of what sourced its parent.
#' @param .quiet Logical. Suppress the progress bar and the summary.
#' @return Invisibly, a tibble of DocID, Path and Written.
read_export <- function(.text, .cands, .dir_out, .meta = NULL, .doc_ids = NULL,
                        .combos = NULL, .labels = NULL, .max_chars = NULL, .overwrite = FALSE,
                        .workers = NULL,
                        .source = here::here("1_code", "_Commons", "_Reads.R"),
                        .quiet = FALSE) {
  if (FALSE) {
    .text      <- tab_text
    .cands     <- tab_all
    .dir_out   <- fs::path(.dir_main, "Reads")
    .meta      <- tab_pick
    .doc_ids   <- NULL
    .combos    <- NULL
    .labels    <- NULL
    .max_chars <- NULL
    .overwrite <- FALSE
    .workers   <- NULL
    .source    <- here::here("1_code", "_Commons", "_Reads.R")
    .quiet     <- FALSE
  }

  stopifnot(all(c("DocID", "TextRaw") %in% names(.text)))
  stopifnot(all(c("DocID", "Start", "Stop", "Span", "Label", "Combo") %in% names(.cands)))
  fs::dir_create(.dir_out)

  ids_ <- if (is.null(.doc_ids)) unique(.text$DocID) else intersect(.doc_ids, .text$DocID)

  # THE GRID IS BUILT ONCE, FROM EVERY CANDIDATE, and handed to every file. Derived per document it
  # would list only the pairs that document happens to carry, so a tab would appear in one file and
  # vanish from the next -- and the absence, which is the finding, would be invisible.
  pairs_ <- read_pair_grid(.cands = .cands, .combos = .combos, .labels = .labels)
  if (!.quiet) {
    cli::cli_alert_info(
      "{nrow(pairs_)} label-engine pair{?s} in the tab bar of every file: \\
       {dplyr::n_distinct(pairs_$Label)} label{?s} across \\
       {dplyr::n_distinct(pairs_$Combo)} engine{?s}."
    )
  }

  paths_ <- purrr::set_names(fs::path(.dir_out, paste0(ids_, ".html")), ids_)
  todo_  <- if (.overwrite) ids_ else ids_[!fs::file_exists(paths_)]

  if (length(todo_) == 0L) {
    if (!.quiet) cli::cli_alert_info("All {length(ids_)} file{?s} cached; nothing to render.")
    return(invisible(tibble::tibble(DocID = ids_, Path = as.character(paths_), Written = FALSE)))
  }

  # Split ONCE, in the parent. Filtering the full candidate table inside each task would send every
  # document's spans to every worker and then discard almost all of them.
  lst_cand_ <- split(dplyr::filter(.cands, .data$DocID %in% todo_),
                     ~ factor(DocID, levels = todo_))
  lst_meta_ <- if (is.null(.meta)) NULL else {
    m_ <- dplyr::filter(.meta, .data$DocID %in% todo_)
    split(m_, ~ factor(DocID, levels = todo_))
  }
  txt_ <- purrr::set_names(.text$TextRaw[match(todo_, .text$DocID)], todo_)

  css_ <- read_css()
  js_  <- read_js()

  payloads_ <- purrr::map(todo_, function(.d) {
    meta_ <- NULL
    if (!is.null(lst_meta_) && nrow(lst_meta_[[.d]]) == 1L) {
      m_ <- dplyr::select(lst_meta_[[.d]], -dplyr::any_of("DocID"))
      meta_ <- purrr::set_names(purrr::map_chr(m_, \(.v) as.character(.v[1])), names(m_))
    }
    list(DocID = .d, Text = unname(txt_[[.d]]), Cands = lst_cand_[[.d]], Meta = meta_,
         Path = as.character(paths_[[.d]]))
  })

  render_one_ <- function(.p, .pairs, .max_chars, .css, .js, .source) {
    # A daemon is a bare R session. It keeps its global environment between tasks, so this sources
    # once per worker rather than once per document.
    if (!exists("read_doc_html", mode = "function")) source(.source, encoding = "UTF-8")
    readr::write_file(
      read_doc_html(.docid = .p$DocID, .text = .p$Text, .cands = .p$Cands, .meta = .p$Meta,
                    .pairs = .pairs, .max_chars = .max_chars, .css = .css, .js = .js),
      .p$Path
    )
    .p$Path
  }

  n_work_ <- if (is.null(.workers)) max(1L, floor(parallel::detectCores() / 2)) else
    as.integer(.workers)
  n_work_ <- min(n_work_, length(todo_))

  if (!.quiet) {
    cli::cli_alert_info(
      "Rendering {length(todo_)} of {length(ids_)} document{?s} on {n_work_} worker{?s}; \\
       {length(ids_) - length(todo_)} cached."
    )
  }

  t0_ <- Sys.time()
  if (n_work_ > 1L && requireNamespace("mirai", quietly = TRUE)) {
    if (!fs::file_exists(.source)) {
      cli::cli_abort(c(
        "Cannot source {.path {(.source)}} in the workers.",
        "i" = "Pass {.arg .source} pointing at this file, or set {.arg .workers} to 1."
      ))
    }
    mirai::daemons(n_work_)
    on.exit(mirai::daemons(0), add = TRUE)

    m_ <- mirai::mirai_map(
      .x = payloads_,
      .f = render_one_,
      .args = list(.pairs = pairs_, .max_chars = .max_chars, .css = css_, .js = js_,
                   .source = as.character(.source))
    )

    # PROGRESS IS POLLED FROM THE OUTPUT DIRECTORY, NOT ASKED OF mirai. Its `.progress` marker has
    # changed form between versions, and when it is absent the collection either errors or returns
    # silently -- which is how a render that was working looked like a render doing nothing. A file
    # on disk is the same fact in a form no package version can take away, and it is the fact the
    # caller actually cares about.
    want_ <- unname(paths_[todo_])
    bar_  <- if (.quiet) NULL else cli::cli_progress_bar("Rendering", total = length(todo_))
    repeat {
      n_done_ <- sum(fs::file_exists(want_))
      if (!is.null(bar_)) cli::cli_progress_update(id = bar_, set = n_done_)
      if (n_done_ >= length(todo_)) break
      # A task that errors never writes its file, so the file count alone would spin forever.
      settled_ <- tryCatch(!any(unlist(lapply(m_, mirai::unresolved))),
                           error = function(.e) FALSE)
      if (settled_) break
      Sys.sleep(0.25)
    }
    if (!is.null(bar_)) cli::cli_progress_done(id = bar_)

    res_ <- m_[]
    failed_ <- purrr::keep(res_, \(.r) inherits(.r, "miraiError") || inherits(.r, "errorValue"))
    if (length(failed_) > 0L) {
      cli::cli_abort(c(
        "{length(failed_)} document{?s} failed in the workers.",
        "x" = "First: {as.character(failed_[[1]])}"
      ))
    }
  } else {
    # THE BAR IS HELD BY ID, NOT BY ENVIRONMENT. cli ties a progress bar to the frame that created
    # it, and inside purrr::walk() the callback's parent frame is purrr's, not this one -- so an
    # .envir guess works until purrr changes how it calls, and then updates go nowhere silently.
    bar_ <- if (.quiet) NULL else cli::cli_progress_bar("Rendering", total = length(todo_))
    purrr::walk(payloads_, function(.p) {
      render_one_(.p = .p, .pairs = pairs_, .max_chars = .max_chars, .css = css_, .js = js_,
                  .source = .source)
      if (!is.null(bar_)) cli::cli_progress_update(id = bar_)
    })
    if (!is.null(bar_)) cli::cli_progress_done(id = bar_)
  }
  secs_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))

  out_ <- tibble::tibble(
    DocID   = ids_,
    Path    = as.character(paths_),
    Written = ids_ %in% todo_
  )

  if (!.quiet) {
    mb_ <- round(sum(as.numeric(fs::file_size(out_$Path[out_$Written]))) / 1024^2, 1)
    cli::cli_alert_success(
      "{sum(out_$Written)} file{?s} in {round(secs_, 1)}s \\
       ({round(sum(out_$Written) / max(secs_, 0.001), 1)}/s, {mb_} MB); \\
       {sum(!out_$Written)} cached. {.path {(as.character(.dir_out))}}"
    )
  }
  invisible(out_)
}
