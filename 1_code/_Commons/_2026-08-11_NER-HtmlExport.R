# _2026-08-11_NER-HtmlExport.R: the interactive span viewer, parked ------------------------------------------------------
#
# PARKED, NOT RETIRED. This is the tool for reading what an extractor actually did to a document:
# one self-contained HTML file per contract, every engine in it, with a Model and a Label dropdown
# that live-filter both the highlighted body and the candidate list, and an Engine x Label count
# matrix at the top. Spans found by a single engine are dimmed when viewing all engines, so
# agreement is visible at a glance rather than inferred from a Jaccard table.
#
# It was moved out of _Commons/_NER.R because nothing in the pipeline calls it, and a shared file
# is not the place to keep code with no callers -- it makes the seam look larger than it is and it
# gets sourced into every entity document for no reason.
#
# TO REVIVE IT. Source this file after _Commons/_NER.R and call ner_export_html() with the store
# path, the canonical text parquet, and the document identifiers wanted. Two things need doing
# first, and both are small:
#
#   1. html_escape() carries no prefix, so it sits in the global namespace where any other file's
#      helper of that name would silently win. Rename it ner_html_escape() on revival.
#   2. Nothing here has roxygen. The functions are documented by the block comments they were
#      written with, which is enough to read them and not enough to maintain them.
#
# The store schema it reads has not changed, so it should run as-is against a current
# 04A-EntityExtract store.


# HTML export -- one interactive file per doc, every model in it ------------------------------------------------------
# Reads the candidate store directly (all engines), breaks each doc's text at every
# candidate boundary into atomic segments, and tags each segment with the labels and
# engine:model combos covering it. The page has a Model and a Label dropdown that
# live-filter the highlighted body AND the candidate list; a Model x Label count
# matrix sits up top. Spans found by a single model are dimmed when viewing "all
# models" so agreement stands out. Self-contained (inline CSS/JS), read-only store.

html_escape <- function(.x) {
  .x |>
    stringi::stri_replace_all_fixed("&", "&amp;") |>
    stringi::stri_replace_all_fixed("<", "&lt;") |>
    stringi::stri_replace_all_fixed(">", "&gt;") |>
    stringi::stri_replace_all_fixed("\"", "&quot;")
}

ner_html_css <- function() {
  r"---(
:root{--c:#374151;--bg:#e5e7eb;}
body{font-family:ui-sans-serif,system-ui,sans-serif;max-width:1000px;margin:24px auto;padding:0 16px;color:#111827;}
h2,h3{margin:.6em 0 .3em;} .meta{color:#6b7280;font-size:13px;}
code{background:#f3f4f6;padding:1px 4px;border-radius:3px;}
.controls{position:sticky;top:0;background:#fff;padding:10px 0;border-bottom:1px solid #e5e7eb;display:flex;gap:18px;align-items:center;z-index:5;}
.controls select{font-size:14px;padding:3px 6px;}
table.sum{border-collapse:collapse;font-size:13px;margin:8px 0;}
table.sum th,table.sum td{border:1px solid #e5e7eb;padding:3px 8px;text-align:right;}
table.sum th:first-child,table.sum td:first-child{text-align:left;}
table.sum th{background:#f9fafb;}
.doc{white-space:pre-wrap;font-family:ui-monospace,Menlo,monospace;font-size:13px;line-height:1.8;background:#fff;border:1px solid #e5e7eb;border-radius:6px;padding:16px;}
.ner{border-radius:3px;padding:0 1px;cursor:default;}
.ner.off{background:transparent!important;color:inherit!important;box-shadow:none!important;opacity:1!important;}
.ner[data-show="ORG"]{background:#d1fae5;color:#065f46;}
.ner[data-show="DATE"]{background:#e0e7ff;color:#3730a3;}
.ner[data-show="MONEY"]{background:#fef3c7;color:#92400e;}
.ner[data-show="GPE"]{background:#ede9fe;color:#5b21b6;}
.ner.solo{opacity:.45;}
.ner[data-multi]:not(.off){box-shadow:inset 0 -2px 0 rgba(0,0,0,.28);}
details{margin-top:14px;} summary{cursor:pointer;color:#374151;font-size:14px;}
table.cand{border-collapse:collapse;width:100%;font-size:12px;margin-top:8px;}
table.cand th,table.cand td{border:1px solid #e5e7eb;padding:3px 6px;text-align:left;vertical-align:top;}
table.cand th{background:#f9fafb;}
)---"
}

ner_html_js <- function() {
  r"---(
(function(){
  var fM=document.getElementById('fModel'), fL=document.getElementById('fLabel');
  var spans=document.querySelectorAll('.doc .ner');
  var rows=document.querySelectorAll('#cand tbody tr');
  function apply(){
    var m=fM.value, l=fL.value, allM=(m==='');
    spans.forEach(function(el){
      var models=el.dataset.models.split(' ');
      var labels=el.dataset.labels.split(',');
      var on=(m===''||models.indexOf(m)>=0)&&(l===''||labels.indexOf(l)>=0);
      el.classList.toggle('off',!on);
      var lab=(l!==''&&labels.indexOf(l)>=0)?l:labels[0];
      el.setAttribute('data-show',lab);
      el.classList.toggle('solo', on&&allM&&el.dataset.n==='1');
    });
    rows.forEach(function(tr){
      var on=(m===''||tr.dataset.model===m)&&(l===''||tr.dataset.label===l);
      tr.style.display=on?'':'none';
    });
  }
  fM.addEventListener('change',apply); fL.addEventListener('change',apply); apply();
})();
)---"
}

# Weave the text into atomic segments; covered segments become tagged spans.
ner_html_body <- function(.text, .cands) {
  n_ <- stringi::stri_length(.text)
  if (nrow(.cands) == 0L) return(html_escape(.text))
  prio_ <- c("ORG", "GPE", "DATE", "MONEY")
  bounds_ <- sort(unique(c(0L, n_, .cands$Start, .cands$Stop)))
  bounds_ <- bounds_[bounds_ >= 0L & bounds_ <= n_]
  pieces_ <- character(length(bounds_) - 1L)
  for (i_ in seq_len(length(bounds_) - 1L)) {
    a_ <- bounds_[i_]
    b_ <- bounds_[i_ + 1L]
    if (b_ <= a_) next
    seg_ <- stringi::stri_sub(.text, a_ + 1L, b_)
    cov_ <- which(.cands$Start <= a_ & .cands$Stop >= b_)
    if (length(cov_) == 0L) {
      pieces_[i_] <- html_escape(seg_)
      next
    }
    labs_ <- unique(.cands$Label[cov_])
    labs_ <- c(intersect(prio_, labs_), sort(setdiff(labs_, prio_)))
    combos_ <- sort(unique(.cands$Combo[cov_]))
    title_ <- paste0(
      paste(labs_, collapse = "/"), " \u00b7 ", length(combos_),
      if (length(combos_) > 1L) " models: " else " model: ", paste(combos_, collapse = ", ")
    )
    multi_ <- if (length(labs_) > 1L) " data-multi=\"1\"" else ""
    pieces_[i_] <- paste0(
      "<span class=\"ner\"",
      " data-labels=\"", paste(labs_, collapse = ","), "\"",
      " data-models=\"", paste(combos_, collapse = " "), "\"",
      " data-n=\"", length(combos_), "\"",
      " data-show=\"", labs_[1], "\"", multi_,
      " title=\"", html_escape(title_), "\">",
      html_escape(seg_), "</span>"
    )
  }
  paste(pieces_, collapse = "")
}

# Model x Label count matrix (with row/column totals).
ner_html_summary <- function(.cands, .labs, .combos) {
  cnt_ <- dplyr::count(.cands, Combo, Label)
  cell_ <- function(.c, .l) {
    v_ <- cnt_$n[cnt_$Combo == .c & cnt_$Label == .l]
    if (length(v_) == 0L) 0L else v_
  }
  head_ <- paste0("<tr><th>Model</th>", paste0("<th>", .labs, "</th>", collapse = ""), "<th>Total</th></tr>")
  body_ <- vapply(.combos, function(.c) {
    vals_ <- vapply(.labs, function(.l) cell_(.c, .l), integer(1))
    paste0("<tr><td>", html_escape(.c), "</td>",
           paste0("<td>", vals_, "</td>", collapse = ""),
           "<td>", sum(vals_), "</td></tr>")
  }, character(1))
  tot_ <- vapply(.labs, function(.l) sum(cnt_$n[cnt_$Label == .l]), integer(1))
  foot_ <- paste0("<tr><th>Total</th>", paste0("<th>", tot_, "</th>", collapse = ""),
                  "<th>", sum(tot_), "</th></tr>")
  paste0("<table class=\"sum\"><thead>", head_, "</thead><tbody>",
         paste(body_, collapse = ""), "</tbody><tfoot>", foot_, "</tfoot></table>")
}

# Full self-contained HTML for one document.
ner_html_doc <- function(.docid, .text, .cands) {
  prio_ <- c("ORG", "GPE", "DATE", "MONEY")
  labs_ <- unique(.cands$Label)
  labs_ <- c(intersect(prio_, labs_), sort(setdiff(labs_, prio_)))
  combos_ <- sort(unique(.cands$Combo))

  body_ <- ner_html_body(.text, .cands)
  summary_ <- ner_html_summary(.cands, labs_, combos_)

  model_opts_ <- paste0("<option value=\"", html_escape(combos_), "\">",
                        html_escape(combos_), "</option>", collapse = "")
  label_opts_ <- paste0("<option value=\"", labs_, "\">", labs_, "</option>", collapse = "")

  rows_ <- .cands |> dplyr::arrange(Start, Stop) |> dplyr::mutate(Index = dplyr::row_number())
  cand_rows_ <- paste0(
    "<tr data-model=\"", html_escape(rows_$Combo), "\" data-label=\"", rows_$Label, "\">",
    "<td>", rows_$Index, "</td>",
    "<td>", rows_$Start, "-", rows_$Stop, "</td>",
    "<td>", rows_$Label, "</td>",
    "<td>", html_escape(rows_$Combo), "</td>",
    "<td>", html_escape(rows_$Span), "</td></tr>",
    collapse = ""
  )

  paste0(
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<title>NER \u2014 ", html_escape(.docid), "</title><style>", ner_html_css(), "</style></head><body>",
    "<h2>NER candidates \u2014 <code>", html_escape(.docid), "</code></h2>",
    "<p class=\"meta\">", nrow(.cands), " candidate(s) \u00b7 ", length(combos_),
    " model(s) \u00b7 spans break at every candidate boundary; a span lists all models that flagged it.</p>",
    summary_,
    "<div class=\"controls\">",
    "<label>Model <select id=\"fModel\"><option value=\"\">All models</option>", model_opts_, "</select></label>",
    "<label>Label <select id=\"fLabel\"><option value=\"\">All labels</option>", label_opts_, "</select></label>",
    "</div>",
    "<div class=\"doc\" id=\"doc\">", body_, "</div>",
    "<details><summary>Candidate list (", nrow(.cands), ")</summary>",
    "<table class=\"cand\" id=\"cand\"><thead><tr><th>#</th><th>Span</th><th>Label</th><th>Model</th><th>Text</th></tr></thead><tbody>",
    cand_rows_, "</tbody></table></details>",
    "<script>", ner_html_js(), "</script></body></html>"
  )
}

# Orchestrator: read candidates + TextRaw, write one HTML per doc (+ optional index).
ner_export_html <- function(.db_path, .inputs, .dir_out,
                            .doc_ids = NULL, .labels = NULL, .run = NULL,
                            .index = TRUE, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Cache$NerDB
    .inputs <- .lP$Input$SampleContracts
    .dir_out <- file.path(.lP$Cache$NerDB |> fs::path_dir(), "Highlight")
    .doc_ids <- NULL
    .labels <- NULL
    .run <- NULL
    .index <- TRUE
    .quiet <- FALSE
  }
  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")
  fs::dir_create(.dir_out)

  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  q_ <- dplyr::tbl(con_, "candidates")
  if (!is.null(.doc_ids)) q_ <- dplyr::filter(q_, DocID %in% .doc_ids)
  if (!is.null(.labels))  q_ <- dplyr::filter(q_, Label %in% .labels)
  cand_ <- q_ |>
    dplyr::select(DocID, Start, Stop, Span, Label, LabelRaw, Engine, Model) |>
    dplyr::collect() |>
    dplyr::mutate(Combo = dplyr::if_else(Engine == Model, Engine, paste0(Engine, ":", Model)))
  if (!is.null(.run)) cand_ <- dplyr::filter(cand_, Combo %in% .run)
  if (nrow(cand_) == 0L) {
    cli::cli_alert_warning("No candidates for that filter \u2014 nothing to export.")
    return(tibble::tibble())
  }

  ids_ <- unique(cand_$DocID)
  texts_ <- arrow::open_dataset(.inputs) |>
    dplyr::filter(DocID %in% ids_) |>
    dplyr::select(DocID, TextRaw) |>
    dplyr::collect()

  paths_ <- purrr::map(ids_, function(.d) {
    text_ <- texts_$TextRaw[texts_$DocID == .d]
    if (length(text_) == 0L) {
      cli::cli_alert_warning("No TextRaw for {(.d)}; skipped.")
      return(NULL)
    }
    text_ <- text_[[1]]
    cands_d_ <- dplyr::filter(cand_, DocID == .d)

    bad_ <- sum(stringi::stri_sub(text_, cands_d_$Start + 1L, cands_d_$Stop) != cands_d_$Span, na.rm = TRUE)
    if (bad_ > 0L) cli::cli_alert_danger("{(.d)}: {bad_} span(s) fail round-trip \u2014 offsets/encoding suspect.")

    safe_id_ <- stringi::stri_replace_all_regex(.d, "[^A-Za-z0-9._-]", "_")
    p_ <- fs::path(.dir_out, paste0(safe_id_, ".html"))
    readr::write_file(ner_html_doc(.d, text_, cands_d_), p_)
    tibble::tibble(DocID = .d, Candidates = nrow(cands_d_), Path = as.character(p_))
  }) |>
    purrr::compact() |>
    purrr::list_rbind()

  if (isTRUE(.index) && nrow(paths_) > 0L) {
    links_ <- paste0(
      "<li><a href=\"", fs::path_file(paths_$Path), "\">", html_escape(paths_$DocID),
      "</a> <span class=\"meta\">(", paths_$Candidates, " candidates)</span></li>",
      collapse = ""
    )
    idx_ <- paste0(
      "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>NER export</title>",
      "<style>", ner_html_css(), "</style></head><body><h2>NER export</h2>",
      "<p class=\"meta\">", nrow(paths_), " document(s).</p><ul>", links_, "</ul></body></html>"
    )
    readr::write_file(idx_, fs::path(.dir_out, "index.html"))
  }

  if (!.quiet) cli::cli_alert_success("Wrote {nrow(paths_)} HTML file(s) to {.path {(.dir_out)}}.")
  return(paths_)
