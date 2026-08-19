# 01D-Item101: extract the Item 1.01 section from 8-K filings ----------------------------------------------------------
#
# WHAT THIS FILE DOES
# An 8-K reporting Item 1.01 carries the filer's own summary of a material agreement it has just
# entered into. This locates that section inside the filing's HTML and extracts its text.
#
# It is a property of a document, derived by opening it, exactly like the word counts in 01C. It is
# in the 01 family and not downstream of sample selection because it does not depend on the sample:
# every 8-K reporting the item is attempted, so the resulting flag means one thing corpus-wide
# rather than "extracted, or never tried".
#
# NOTHING IS PRE-FILTERED; EVERYTHING IS RECORDED
# Format is not a filter. A document that is not HTML is attempted and recorded as such, so the share
# is measured rather than assumed, and if plain-text filings turn out to be parseable that is a
# finding rather than a permanent exclusion. Every candidate leaves exactly one row carrying an
# outcome, which is what makes the failure modes countable.
#
# THE TEXT STAYS HERE
# Roughly a quarter of a million summaries is on the order of a gigabyte. The register in 02 carries
# a flag; the text is one join away. Putting it in the register would make the one table nobody can
# load.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) dev blocks; pure
# ASCII; parenthesised cli interpolation.

if (FALSE) {
  .path_landing <- .lP$Input$LandingPage
  .path_meta    <- .lP$Input$MetaData
  .path         <- tab_candidates$Path[1]
  .item         <- "1.01"
}


# 1. Filing items ------------------------------------------------------------------------------------------------------

#' The item list for each filing, where its registrants agree
#'
#' The landing table holds one row per filing and registrant, so a filing's item list appears once
#' per registrant. Where every registrant agrees the row is kept; where they do not the filing is
#' dropped rather than one version being chosen, because there is no basis for choosing and the
#' alternative is a silent coin flip on the variable that decides what gets extracted.
#'
#' THIS LIVES HERE BECAUSE THIS IS THE FIRST SCRIPT THAT NEEDS IT. 02 needs the same rule and sources
#' it from here. Two implementations of "which filings report which items" would be two answers.
#'
#' @param .path_landing Path to the landing table restricted to retrieved filings.
#' @return A tibble: HashIndex, Items.
itm_filing_items <- function(.path_landing) {
  if (FALSE) .path_landing <- .lP$Input$LandingPage

  arrow::open_dataset(sources = .path_landing) |>
    dplyr::select("HashIndex", "Items") |>
    dplyr::filter(!is.na(.data$Items)) |>
    dplyr::collect() |>
    dplyr::mutate(Items = gsub("\n", "|", .data$Items)) |>
    dplyr::distinct() |>
    dplyr::filter(dplyr::n() == 1L, .by = "HashIndex")
}

#' Documents to attempt extraction on
#'
#' Every 8-K document whose filing reports the item. Not restricted by sample membership and not by
#' file format: both would turn an absent result into two different things.
#'
#' The item code is matched literally. As a regular expression "1.01" carries a wildcard in the
#' middle and would also match "1101"; no item code has that shape, so the two agree, but a rule that
#' is accidentally loose is worth not carrying forward.
#'
#' @param .path_meta Path to 01C's consolidated metadata.
#' @param .tab_items Output of itm_filing_items().
#' @param .path_index Path to 01B's DocID to Path index.
#' @param .item Character. Item code, e.g. "1.01".
#' @return A tibble: DocID, Path, DocTypeMod, YQ, Items.
itm_candidates <- function(.path_meta, .tab_items, .path_index, .item = "1.01") {
  if (FALSE) {
    .path_meta  <- .lP$Input$MetaData
    .tab_items  <- tab_items
    .path_index <- .lP$Input$FilePaths
    .item       <- "1.01"
  }

  hit_ <- .tab_items$HashIndex[grepl(.item, .tab_items$Items, fixed = TRUE)]

  arrow::open_dataset(sources = .path_meta) |>
    dplyr::filter(grepl("^8-K", .data$DocTypeMod)) |>
    dplyr::filter(.data$HashIndex %in% hit_) |>
    dplyr::select("DocID", "HashIndex", "DocTypeMod", "YQ", "Removed") |>
    dplyr::collect() |>
    dplyr::left_join(
      y  = dplyr::select(arrow::read_parquet(.path_index), "DocID", "Path"),
      by = dplyr::join_by("DocID")
    ) |>
    dplyr::left_join(.tab_items, by = dplyr::join_by("HashIndex"))
}


# 2. Locating the item -------------------------------------------------------------------------------------------------

#' Replace every kind of space with a plain one
#'
#' A NON-BREAKING SPACE IS NOT THE ONLY ONE. Filing agents set headings with typographic spacing --
#' thin, hair, en, em, narrow no-break, zero-width -- written either as named entities, as numeric
#' references, or as the characters themselves once the markup is rendered. To a pattern expecting
#' whitespace between "Item" and its number, every one of them is an ordinary character, and the
#' heading does not parse. Handling only &nbsp; catches the common case and misses the recent one:
#' the thin space accounts for almost every unparsed heading filed after 2022.
#'
#' The numeric ranges are enumerated rather than matched loosely, because a pattern wide enough to
#' catch every space reference in the U+2000 block also catches the curly quotation marks two
#' positions above it, and those are content.
#'
#' @param .x Character vector.
#' @return The same, with space-like entities and characters replaced by U+0020.
itm_unspace <- function(.x) {
  if (FALSE) .x <- "Item&#8201;1.01"

  ent_ <- paste0(
    "&(?:nbsp|ensp|emsp|emsp13|emsp14|numsp|puncsp|thinsp|hairsp|MediumSpace|ZeroWidthSpace",
    "|#160|#819[2-9]|#820[0-3]|#8239|#8287|#12288",
    "|#[Xx](?:[Aa]0|200[0-9AaBb]|202[Ff]|205[Ff]|3000|[Ff][Ee][Ff][Ff]));"
  )

  stringi::stri_replace_all_regex(.x, ent_, " ") |>
    stringi::stri_replace_all_regex("[\\p{Zs}\\u200B-\\u200D\\uFEFF]", " ")
}

#' Read one document, normalised for its format
#'
#' WHITESPACE IS COLLAPSED DIFFERENTLY BY FORMAT. In markup the structure lives in the tags, so
#' every run of whitespace including newlines becomes one space and patterns can assume it. In plain
#' text the structure IS the newlines: a heading is a heading because it starts a line, and
#' collapsing them destroys the only signal there is. Runs of spaces and tabs are still collapsed,
#' because column alignment is padding rather than structure.
#'
#' Space-like entities and characters are replaced first, in every spelling: see itm_unspace().
#'
#' @param .path Path to one parsed document.
#' @return A one-row tibble: DocID, DocExt, Body, IsMarkup.
itm_read_doc <- function(.path) {
  if (FALSE) .path <- tab_candidates$Path[1]

  tab_ <- arrow::read_parquet(
    file       = .path,
    col_select = dplyr::all_of(c("DocID", "DocExt", "HTML"))
  )

  markup_ <- tolower(tab_$DocExt[1L]) %in% c("htm", "html", "xml", "xhtml")

  body_ <- itm_unspace(tab_$HTML[1L])
  body_ <- if (isTRUE(markup_)) {
    stringi::stri_replace_all_regex(body_, "\\s+", " ")
  } else {
    stringi::stri_replace_all_regex(body_, "[ \\t]+", " ") |>
      stringi::stri_replace_all_regex("\\n{2,}", "\n")
  }

  tibble::tibble(
    DocID    = tab_$DocID[1L],
    DocExt   = tab_$DocExt[1L],
    Body     = body_,
    IsMarkup = markup_
  )
}

#' Every item boundary in one document, as spans
#'
#' THE ANCHOR DEPENDS ON THE FORMAT, BECAUSE A HEADING IS MARKED DIFFERENTLY IN EACH. In markup a
#' heading follows a closing tag, so the anchor is ">". Without it the word "item" inside a sentence
#' -- and it appears inside most agreements -- produces a boundary where there is none. In plain text
#' there are almost no tags: the only ">" characters come from SGML wrappers such as <PAGE>, so the
#' same anchor finds one or two boundaries in a document with six, and the first span runs to the end
#' of the filing. There, a heading is a heading because it starts a line.
#'
#' THE HEADER WINDOW IS STRIPPED OF TAGS BEFORE THE CODE IS READ. Modern filing agents wrap headings
#' in nested elements, so the raw window reads "Item</span><span> 1.01" and a pattern expecting
#' digits after the word finds a tag instead. The code is a property of the rendered heading, not of
#' the markup, so the markup is removed before it is read.
#'
#' A signature block is located alongside the items because it terminates the last one; without it
#' the final section would run to the end of the document and swallow the exhibit index.
#'
#' @param .body Character. The normalised body of one document.
#' @param .markup Logical. TRUE for markup formats, FALSE for plain text.
#' @return A tibble of ordered markers: Item, Start, Stop, Header.
itm_locate_items <- function(.body, .markup = TRUE) {
  if (FALSE) {
    doc_    <- itm_read_doc(tab_candidates$Path[1])
    .body   <- doc_$Body
    .markup <- doc_$IsMarkup
  }

  rex_ <- if (isTRUE(.markup)) {
    c("(?i)>\\s*ITEM", "(?i)>\\s*SIGNATURE")
  } else {
    c("(?im)^\\s*ITEM", "(?im)^\\s*SIGNATURE")
  }

  pos_ <- unlist(lapply(rex_, function(.r) stringi::stri_locate_all_regex(.body, .r)[[1L]][, 1L]))
  pos_ <- sort(pos_[!is.na(pos_)])

  if (length(pos_) == 0L) return(tibble::tibble())

  tibble::tibble(Start = pos_) |>
    dplyr::mutate(
      Header = stringi::stri_sub(.body, .data$Start, .data$Start + 150L),
      Plain  = stringi::stri_replace_all_regex(.data$Header, "<[^>]*>", " "),
      Plain  = stringi::stri_replace_all_regex(.data$Plain, "\\s+", " "),
      Item   = stringi::stri_extract_first_regex(
        .data$Plain, "(?i)ITEM\\s*\\d+\\s*_?\\.?\\s*\\d*|SIGNATURE"
      ),
      Item   = gsub(" ", "", toupper(.data$Item)),
      Stop   = dplyr::lead(.data$Start),
      # THE LAST MARKER RUNS TO THE END OF THE DOCUMENT. Dropping it, as taking lead() alone does,
      # discards any filing whose only boundary is the item itself -- which happens whenever the
      # signature block is styled in a way the anchor does not see. Extracting too much is visible
      # in the length distribution; extracting nothing is a document lost without trace.
      Stop   = dplyr::coalesce(.data$Stop, nchar(.body))
    ) |>
    dplyr::select("Start", "Stop", "Item", "Header")
}

#' Render one span as text
#'
#' @param .body Character. The normalised body of one document.
#' @param .start Integer. Span start, in code points.
#' @param .stop Integer. Span end, in code points.
#' @param .markup Logical. TRUE parses the span as HTML; FALSE takes it as it stands.
#' @return Character scalar, or NA if a markup fragment does not parse.
itm_render_span <- function(.body, .start, .stop, .markup = TRUE) {
  if (FALSE) {
    .body   <- itm_read_doc(tab_candidates$Path[1])$Body
    .start  <- 1000L
    .stop   <- 4000L
    .markup <- TRUE
  }

  frag_ <- stringi::stri_sub(.body, .start, .stop)

  if (!isTRUE(.markup)) return(trimws(stringi::stri_replace_all_regex(frag_, "\\s+", " ")))

  out_ <- try(
    expr   = rvest::html_text2(rvest::read_html(frag_, encoding = "UTF-8", options = "RECOVER")),
    silent = TRUE
  )

  if (inherits(out_, "try-error")) NA_character_ else trimws(gsub("<|>", "", out_))
}


# 3. One document ------------------------------------------------------------------------------------------------------

#' How much of a rendered span is body rather than heading
#'
#' The item code and the regulated title together run to about forty characters and say nothing
#' about the agreement, so a length test that includes them measures the wrong thing. Both are
#' removed and what remains is counted.
#'
#' The title is matched loosely -- optional article, optional brackets, optional trailing stop --
#' because filers punctuate it inconsistently, and a pattern that insisted on one spelling would
#' subtract nothing from most of them.
#'
#' @param .text Character. One rendered span.
#' @return Integer. Characters remaining after the heading and title.
itm_body_chars <- function(.text) {
  if (FALSE) .text <- "Item 1.01 Entry into a Material Definitive Agreement"

  if (is.na(.text)) return(NA_integer_)

  rest_ <- .text |>
    stringi::stri_replace_first_regex("(?i)^\\s*item\\s*\\d+\\s*\\.?\\s*\\d*\\s*[.:)-]*\\s*", "") |>
    stringi::stri_replace_first_regex(
      "(?i)^\\s*[(\\[]?\\s*entry\\s+into\\s+(?:an?\\s+)?material\\s+definitive\\s+agreement\\s*[)\\]]?\\s*[.;:]*\\s*",
      ""
    )

  nchar(trimws(rest_))
}

#' Locate and extract the item from one document
#'
#' EVERY CANDIDATE LEAVES ONE ROW. A document that could not be read, that carries no markers, or
#' whose fragment would not parse is as much a result as one that extracted, and the outcomes are
#' what make those countable rather than inferred from an absence.
#'
#'   extracted           one item span found and rendered
#'   ambiguous-longest   the item appears more than once; the longest span was taken
#'   heading-only        the span rendered to the item heading and its title, with no body
#'   no-body             the document carries nothing to search
#'   no-markers          no item boundary found at all
#'   markers-unparsed    boundaries found, none of them carrying a readable item code
#'   not-found           codes read, none of them this item
#'   parse-failed        the fragment would not parse as markup
#'
#' A SPAN THAT IS ONLY A HEADING IS NOT A SUMMARY. Some filers set the item list as a table with the
#' numbers in one column and the bodies in another; others put the heading and its title in one cell
#' and the body somewhere the span does not reach. Either way the rendered text is "Item 1.01" or
#' "Item 1.01 Entry into a Material Definitive Agreement" and nothing more, and an earlier version
#' counted tens of thousands of those as successes.
#'
#' The test is on what remains after the heading is removed, not on total length. A threshold on the
#' whole string has to be guessed, and guessing it at fifty characters put two of the three shortest
#' extractions in the corpus exactly on the boundary. The regulated title is a fixed cost of about
#' forty characters that carries no information about the agreement, so subtracting it first makes
#' the threshold mean something: what is left is the summary, and a summary that is a few dozen
#' characters is not one.
#'
#' Recorded and not extracted. Taking the next span instead would return this item's body
#' concatenated with the following item's, and for text analysis contaminated text is worse than a
#' known gap: a clean exclusion is countable, a silent error is not.
#'
#' THE LONGEST SPAN IS TAKEN WHEN THE ITEM REPEATS. A filing with a table of contents lists Item 1.01
#' twice, once as a line in the contents and once as the section. Requiring exactly one occurrence,
#' as an earlier implementation did, discards those documents entirely; length separates them, and
#' the two length distributions are reported so the assumption can be checked rather than trusted.
#'
#' THE TABULAR TEST IS ON THE RENDERED TEXT, NOT THE RAW SPAN. Between two headings in a table sit a
#' few hundred characters of markup and nine characters of text, so a threshold applied to the raw
#' span never fires while one applied to the rendered string fires exactly when it should. The check
#' therefore comes after rendering rather than before it, which costs one render on a document that
#' is then discarded and buys a test that measures what it claims to.
#'
#' @param .path Path to one parsed document.
#' @param .item Character. Item code, e.g. "1.01".
#' @param .min_chars Integer. Characters that must remain after the item heading and its title are
#'   removed for the span to count as a summary.
#' @return A one-row tibble: DocID, DocExt, Outcome, nMarkers, nItems, Start, Stop, nCharsItem,
#'   nBodyChars, ItemText.
itm_process_doc <- function(.path, .item = "1.01", .min_chars = 30L) {
  if (FALSE) {
    .path      <- tab_candidates$Path[1]
    .item      <- "1.01"
    .min_chars <- 30L
  }

  doc_ <- itm_read_doc(.path)

  out_ <- function(.outcome, .n_markers = 0L, .n_items = 0L, .start = NA_integer_,
                   .stop = NA_integer_, .text = NA_character_) {
    tibble::tibble(
      DocID      = doc_$DocID[1L],
      DocExt     = doc_$DocExt[1L],
      Outcome    = .outcome,
      nMarkers   = .n_markers,
      nItems     = .n_items,
      Start      = .start,
      Stop       = .stop,
      nCharsItem = if (is.na(.text)) NA_integer_ else nchar(.text),
      nBodyChars = itm_body_chars(.text = .text),
      ItemText   = .text
    )
  }

  if (is.na(doc_$Body[1L]) || !nzchar(doc_$Body[1L])) return(out_("no-body"))

  all_ <- itm_locate_items(.body = doc_$Body[1L], .markup = doc_$IsMarkup[1L])
  if (nrow(all_) == 0L) return(out_("no-markers"))

  mrk_ <- dplyr::filter(all_, !is.na(.data$Item), !is.na(.data$Stop))
  if (nrow(mrk_) == 0L) return(out_("markers-unparsed", nrow(all_)))

  key_ <- gsub(" ", "", toupper(paste0("ITEM", .item)))
  hit_ <- dplyr::filter(mrk_, .data$Item == key_)
  if (nrow(hit_) == 0L) return(out_("not-found", nrow(mrk_)))

  n_it_ <- nrow(hit_)
  hit_  <- hit_ |>
    dplyr::mutate(Len = .data$Stop - .data$Start) |>
    dplyr::slice_max(.data$Len, n = 1L, with_ties = FALSE)

  txt_ <- itm_render_span(
    .body   = doc_$Body[1L],
    .start  = hit_$Start[1L],
    .stop   = hit_$Stop[1L],
    .markup = doc_$IsMarkup[1L]
  )
  if (is.na(txt_)) return(out_("parse-failed", nrow(mrk_), n_it_))

  # Tested after rendering, because the raw span between two headings in a table is hundreds of
  # characters of markup around ten characters of text.
  if (itm_body_chars(.text = txt_) < .min_chars) {
    return(out_("heading-only", nrow(mrk_), n_it_, hit_$Start[1L], hit_$Stop[1L]))
  }

  out_(
    .outcome   = if (n_it_ > 1L) "ambiguous-longest" else "extracted",
    .n_markers = nrow(mrk_),
    .n_items   = n_it_,
    .start     = hit_$Start[1L],
    .stop      = hit_$Stop[1L],
    .text      = txt_
  )
}


# 4. Reports -----------------------------------------------------------------------------------------------------------

#' Which outcomes count as a recovered summary
#'
#' Defined once because four reports and, shortly, 02 all ask the same question. A predicate spelled
#' out in five places is five chances to disagree about whether an ambiguous extraction counts, and
#' the disagreement would show up as two coverage figures in one paper.
#'
#' @return Character vector of outcome labels.
itm_outcomes_ok <- function() c("extracted", "ambiguous-longest")

#' Outcomes, by file format
#'
#' Crossed with format rather than reported alone, because the interesting question is not how often
#' extraction failed but whether it failed for a reason that could be fixed. A format whose documents
#' extract as reliably as HTML has no business being excluded; one that never extracts is an honest
#' limitation, and only the cross table distinguishes them.
#'
#' @param .tab Output of the extraction pass.
#' @return A tibble: DocExt, one column per outcome, nDocs, ShareExtracted.
itm_outcome_summary <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  .tab |>
    dplyr::mutate(Ext = dplyr::if_else(.data$DocExt %in% c("htm", "html"), .data$DocExt, "other")) |>
    dplyr::summarise(
      nDocs          = dplyr::n(),
      nExtracted     = sum(.data$Outcome %in% itm_outcomes_ok()),
      nAmbiguous     = sum(.data$Outcome == "ambiguous-longest"),
      nHeadingOnly   = sum(.data$Outcome == "heading-only"),
      nNoMarkers     = sum(.data$Outcome == "no-markers"),
      nUnparsed      = sum(.data$Outcome == "markers-unparsed"),
      nNotFound      = sum(.data$Outcome == "not-found"),
      nNoBody        = sum(.data$Outcome == "no-body"),
      nParseFailed   = sum(.data$Outcome == "parse-failed"),
      .by            = "Ext"
    ) |>
    dplyr::mutate(ShareExtracted = .data$nExtracted / .data$nDocs) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' Length of the extracted summaries
#'
#' The check on the longest-span rule. If a contents entry were being taken for a section the
#' ambiguous group would sit far below the unambiguous one; if the two distributions agree, the rule
#' is picking the section.
#'
#' @param .tab Output of the extraction pass.
#' @return A tibble: Outcome, nDocs and the quartiles of nCharsItem.
itm_length_summary <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  .tab |>
    dplyr::filter(.data$Outcome %in% itm_outcomes_ok()) |>
    dplyr::summarise(
      nDocs  = dplyr::n(),
      Min    = min(.data$nBodyChars),
      Q25    = round(stats::quantile(.data$nBodyChars, 0.25)),
      Median = stats::median(.data$nBodyChars),
      Q75    = round(stats::quantile(.data$nBodyChars, 0.75)),
      Max    = max(.data$nBodyChars),
      .by    = "Outcome"
    )
}

#' Extraction over time
#'
#' @param .tab Output of the extraction pass, carrying YQ.
#' @return A tibble: Year, nDocs, nExtracted, ShareExtracted.
itm_coverage_by_year <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  .tab |>
    dplyr::mutate(Year = as.integer(stringi::stri_sub(.data$YQ, 1L, 4L))) |>
    dplyr::summarise(
      nDocs      = dplyr::n(),
      nExtracted = sum(.data$Outcome %in% itm_outcomes_ok()),
      .by        = "Year"
    ) |>
    dplyr::mutate(ShareExtracted = .data$nExtracted / .data$nDocs) |>
    dplyr::arrange(.data$Year)
}

#' Outcomes by year, in full
#'
#' The share extracted alone says a year went wrong without saying how. A year that falls because
#' documents stopped carrying item markers is a change in filing format; one that falls because the
#' markers are there but Item 1.01 is not among them is a change in what filers report; one that
#' falls because nothing parses is a change in markup. They need different responses, and only the
#' full breakdown distinguishes them.
#'
#' @param .tab Output of the extraction pass.
#' @return A tibble: Year, one column per outcome, nDocs, ShareExtracted.
itm_outcome_by_year <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  .tab |>
    dplyr::mutate(Year = as.integer(stringi::stri_sub(.data$YQ, 1L, 4L))) |>
    dplyr::summarise(
      nDocs        = dplyr::n(),
      Extracted    = sum(.data$Outcome == "extracted"),
      Ambiguous    = sum(.data$Outcome == "ambiguous-longest"),
      HeadingOnly  = sum(.data$Outcome == "heading-only"),
      NoMarkers    = sum(.data$Outcome == "no-markers"),
      Unparsed     = sum(.data$Outcome == "markers-unparsed"),
      NotFound     = sum(.data$Outcome == "not-found"),
      NoBody       = sum(.data$Outcome == "no-body"),
      ParseFailed  = sum(.data$Outcome == "parse-failed"),
      .by          = "Year"
    ) |>
    dplyr::mutate(ShareExtracted = (.data$Extracted + .data$Ambiguous) / .data$nDocs) |>
    dplyr::arrange(.data$Year)
}

#' Outcomes by the actual file extension
#'
#' The summary table folds every non-HTML format into one row, which answers whether the restriction
#' to HTML costs anything but not which format it costs it on. A format that extracts as reliably as
#' HTML has no business being excluded; one that never extracts is an honest limitation. Only the
#' per-format table separates the two.
#'
#' @param .tab Output of the extraction pass.
#' @param .min Integer. Formats with fewer documents than this are pooled as "other".
#' @return A tibble: DocExt, nDocs, nExtracted, ShareExtracted and the failure modes.
itm_format_detail <- function(.tab, .min = 50L) {
  if (FALSE) {
    .tab <- tab_item101
    .min <- 50L
  }

  keep_ <- .tab |>
    dplyr::count(.data$DocExt, name = "n") |>
    dplyr::filter(.data$n >= .min) |>
    dplyr::pull(.data$DocExt)

  .tab |>
    dplyr::mutate(Ext = dplyr::if_else(.data$DocExt %in% keep_, .data$DocExt, "other")) |>
    dplyr::summarise(
      nDocs       = dplyr::n(),
      nExtracted  = sum(.data$Outcome %in% itm_outcomes_ok()),
      nHeadingOnly = sum(.data$Outcome == "heading-only"),
      NoMarkers   = sum(.data$Outcome == "no-markers"),
      Unparsed    = sum(.data$Outcome == "markers-unparsed"),
      NotFound    = sum(.data$Outcome == "not-found"),
      .by         = "Ext"
    ) |>
    dplyr::mutate(ShareExtracted = .data$nExtracted / .data$nDocs) |>
    dplyr::arrange(dplyr::desc(.data$nDocs))
}

#' Are the extracted summaries a plausible length?
#'
#' EXTRACTED IS NOT THE SAME AS CORRECT. A span that renders is recorded as a success whatever it
#' contains, and two failure modes survive that test. A summary of a few dozen characters is a
#' heading with the section missing, which happens when the next marker sits immediately after this
#' one. A summary of a hundred thousand characters is a span that never terminated, which happens
#' when no later marker was found and the slice ran to the end of the filing.
#'
#' Neither is detectable from the outcome, and both are visible here. The bands are deliberately
#' coarse: they are meant to show whether a problem exists, not to define a threshold.
#'
#' MEASURED ON THE BODY, NOT THE WHOLE SPAN. The heading and its regulated title are a fixed forty
#' characters that say nothing about the agreement, and counting them puts a bare heading into the
#' same band as a one-sentence summary.
#'
#' @param .tab Output of the extraction pass.
#' @return A tibble: Band, nDocs, Share.
itm_length_bands <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  .tab |>
    dplyr::filter(!is.na(.data$nBodyChars)) |>
    dplyr::mutate(
      Band = dplyr::case_when(
        .data$nBodyChars <  100L    ~ "1-under 100: barely more than the heading",
        .data$nBodyChars <  500L    ~ "2-100 to 500: very short",
        .data$nBodyChars < 20000L   ~ "3-500 to 20k: typical",
        .data$nBodyChars < 100000L  ~ "4-20k to 100k: long",
        .default                    = "5-over 100k: the span did not terminate"
      )
    ) |>
    dplyr::summarise(nDocs = dplyr::n(), .by = "Band") |>
    dplyr::mutate(Share = .data$nDocs / sum(.data$nDocs)) |>
    dplyr::arrange(.data$Band)
}

#' The shortest and longest extractions, with a preview
#'
#' The bands say how many are implausible; this says what they look like. A count without an example
#' invites a threshold to be picked from the number rather than from the documents.
#'
#' @param .tab Output of the extraction pass.
#' @param .n Integer. How many from each end.
#' @param .chars Integer. Preview length.
#' @return A tibble: Which, DocID, DocExt, nMarkers, nCharsItem, Preview.
itm_examples <- function(.tab, .n = 3L, .chars = 160L) {
  if (FALSE) {
    .tab   <- tab_item101
    .n     <- 3L
    .chars <- 160L
  }

  hit_ <- dplyr::filter(.tab, !is.na(.data$ItemText))

  dplyr::bind_rows(
    dplyr::mutate(dplyr::slice_min(hit_, .data$nBodyChars, n = .n, with_ties = FALSE), Which = "shortest"),
    dplyr::mutate(dplyr::slice_max(hit_, .data$nBodyChars, n = .n, with_ties = FALSE), Which = "longest")
  ) |>
    dplyr::mutate(
      Preview = gsub("\\s+", " ", stringi::stri_sub(.data$ItemText, 1L, .chars))
    ) |>
    dplyr::select("Which", "DocID", "DocExt", "nMarkers", "nItems", "nCharsItem", "nBodyChars", "Preview")
}

#' Every report in this document, in order
#'
#' @param .tab Output of the extraction pass.
#' @return Invisibly NULL.
itm_report_all <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  tbl_head("Outcomes by file format")
  tbl_out(
    .tab    = itm_outcome_summary(.tab = .tab),
    .title  = NULL,
    .pct    = "ShareExtracted",
    .digits = 1L,
    .notes  = c(
      Ext            = "Formats other than HTML are attempted, not excluded; this is the result.",
      nAmbiguous     = "The item appeared more than once and the longest span was taken."
    )
  )

  tbl_head("Outcomes by file extension")
  tbl_out(
    .tab    = itm_format_detail(.tab = .tab),
    .title  = NULL,
    .pct    = "ShareExtracted",
    .digits = 1L
  )

  tbl_head("Length of the extracted summaries")
  tbl_out(
    .tab   = itm_length_summary(.tab = .tab),
    .title = NULL,
    .notes = c(Median = "Ambiguous and unambiguous should agree; a shorter ambiguous group would \\
                         mean a contents entry was taken for a section.")
  )

  tbl_head("Are the lengths plausible?")
  tbl_out(
    .tab    = itm_length_bands(.tab = .tab),
    .title  = NULL,
    .pct    = "Share",
    .digits = 1L,
    .notes  = c(Band = "Extracted is not the same as correct: both ends of this table are failures \\
                        that passed the outcome test.")
  )

  invisible(NULL)
}


# 5. Figures -----------------------------------------------------------------------------------------------------------

#' How the outcomes compose each year
#'
#' Shares rather than counts, because filing volume varies fourfold across the period and a count
#' chart would show that instead of what is being asked.
#'
#' @param .tab Output of itm_outcome_by_year().
#' @return A ggplot object.
itm_plot_outcome_share <- function(.tab) {
  if (FALSE) .tab <- itm_outcome_by_year(tab_item101)

  dat_ <- .tab |>
    dplyr::select("Year", "Extracted", "Ambiguous", "HeadingOnly", "NoMarkers", "Unparsed", "NotFound") |>
    tidyr::pivot_longer(cols = -"Year", names_to = "Outcome", values_to = "nDocs") |>
    dplyr::filter(.data$nDocs > 0L)

  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$nDocs, fill = .data$Outcome)) +
    ggplot2::geom_col(position = "fill") +
    plot_scale_fill_cat() +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
    plot_theme(.grid = "y")
}

#' Distribution of summary length
#'
#' On a log scale, because the range spans four orders of magnitude and a linear axis would render
#' everything below ten thousand characters as one bar.
#'
#' @param .tab Output of the extraction pass.
#' @return A ggplot object.
itm_plot_length <- function(.tab) {
  if (FALSE) .tab <- tab_item101

  dat_ <- dplyr::filter(.tab, !is.na(.data$nBodyChars), .data$nBodyChars > 0L)

  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$nBodyChars, fill = .data$Outcome)) +
    ggplot2::geom_histogram(bins = 60L, position = "identity", alpha = 0.7) +
    ggplot2::scale_x_log10(labels = scales::comma) +
    plot_scale_fill_cat() +
    plot_scale_y_count() +
    ggplot2::labs(x = "Characters in the extracted summary", y = "Documents", fill = NULL) +
    plot_theme(.grid = "y")
}

#' Extraction coverage per year
#'
#' @param .tab Output of itm_coverage_by_year().
#' @return A ggplot object.
itm_plot_coverage <- function(.tab) {
  if (FALSE) .tab <- itm_coverage_by_year(tab_item101)

  dat_ <- .tab |>
    dplyr::select("Year", Extracted = "nExtracted", "nDocs") |>
    dplyr::mutate(Failed = .data$nDocs - .data$Extracted, nDocs = NULL) |>
    tidyr::pivot_longer(cols = c("Extracted", "Failed"), names_to = "Outcome", values_to = "nDocs")

  ggplot2::ggplot(dat_, ggplot2::aes(x = .data$Year, y = .data$nDocs, fill = .data$Outcome)) +
    ggplot2::geom_col() +
    plot_scale_fill_cat() +
    plot_scale_y_count() +
    ggplot2::labs(x = NULL, y = "Documents", fill = NULL) +
    plot_theme(.grid = "y")
}
