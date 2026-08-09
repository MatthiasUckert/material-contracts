# 04B-CoWorkEntities: prepare a CoWork rule-generation bundle for the entity layer ----
#
# WHAT THIS IS
# The reading arm of 04B, built the same way as the keyword arm in 03C: a standalone script that
# turns the candidate store into self-contained folders a reading session can work from. Reading the
# answers back is a separate concern and lives in 04B-CoWorkCompile.R.
#
# WHY RULES RATHER THAN JUDGEMENTS
# The classification bundle asks for terms because terms can be applied to 4,400 documents in
# seconds. The same constraint binds harder here. The full-text store holds 7.4 million candidates
# for 4,398 documents; the corpus is 258 times larger, so a per-candidate model pass is a billion
# calls and is not a budget question but an impossibility. The session therefore never judges a
# candidate. It reads a sample and proposes DETERMINISTIC RULES -- context cues, stop terms, section
# patterns -- which the pipeline then applies at whatever scale it likes.
#
# WHAT MAKES IT FALSIFIABLE, WHICH IS THE HARD PART
# 03C is honest because every proposed term is scored against 4,400 hand labels on a fold the
# session never saw. There are no entity labels, so that judge does not exist here. What exists
# instead are EDGAR facts: the filer's own name, its filing date, its registered addresses. Each is
# recorded outside the contract and each identifies at least one KNOWN-TRUE entity per document. A
# proposed rule can therefore be scored without annotation -- of the spans it keeps, what share are
# the filer's own name; of the filer's mentions, what share does it keep -- and scored on fold 5,
# which this bundle never shows.
#
# That is also what supplies the contrast. Where 03C pairs TARGET documents with OTHER documents,
# this pairs anchor-matched candidates with unmatched ones. Same one-vs-rest shape, same reason.
#
# THE BIAS THIS INTRODUCES, STATED RATHER THAN HIDDEN
# The anchor is the FILER's own name, so a session that keys on entity identity would learn to
# recognise one particular party rather than parties in general. The instructions therefore ask for
# CONTEXT patterns, which are symmetric: "by and between X and Y" licenses both sides of an
# agreement, and the joint filings give two anchors in the same document. It is a real limitation
# and the compile step reports rules that look identity-bound.
#
# THE ANCHOR LOGIC IS NOT DUPLICATED HERE ANY MORE
# The first version of this script carried its own copy of the company normaliser so that it could
# stay standalone. That cost exactly what duplication always costs: when the scoring side learned
# that EDGAR stores states as two-letter codes and contracts write them out, this side did not, and
# the geographic contrast a reading session saw was built from cities alone while the measurement
# behind it was not. The bundle and the scoring must agree about what an anchor is or they are about
# different sets. So this script now sources 04B-EntityMeasure.R for that logic. It is a pure
# function library with no side effects, and one source() removes a whole class of error.
#
# THE FIFTH UNIT
# 04A now emits bracketed redaction indicators as spans, which opens a question nothing else in the
# family can ask: not how much was withheld, which the published analysis already counts, but WHAT.
# A marker sits exactly where a commercially material figure used to be, so the text around it is a
# labelled slot -- and a cue that precedes "[***]" in one contract precedes an amount in another.
# That makes redaction sites a training set for slot-finding that should transfer to unredacted
# documents, which is the nearest thing to a handle on money this family has.
#
# MONEY HAS NO ANCHOR
# Nothing in EDGAR records a contract's value, so the money unit cannot be built as a contrast and
# is not pretended to be one. It is exploratory: a stratified sample of amounts in context, labelled
# with contract type, asking which cue marks a contract-level figure in each type. The likely
# finding is that no single "contract value" exists across the taxonomy, and the per-type vocabulary
# is what would establish that.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new; if (FALSE) dev blocks; cli/fs/here;
# pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.


# 1. Configuration --------------------------------------------------------
# The bundle lives outside the repository. The reading session has file access, so an instruction
# not to open 2_output is a request; a folder that does not contain it is a guarantee.

# The anchor logic lives in 04B-EntityMeasure.R and is sourced rather than copied: the contrast a
# session reads and the measurement applied to its answers have to be about the same anchor.
source(here::here("1_code", "04B-EntityMeasure.R"), encoding = "UTF-8")

.PATH_TEXT    <- here::here("2_output", "04A-EntityExtract", "sample_text.parquet")
.PATH_ANCHORS <- here::here("2_output", "04A-EntityExtract", "sample_anchors.parquet")
.PATH_STORE   <- here::here("2_output", "04A-EntityExtract", "Store", "EntityCandidates.duckdb")

.DIR_BUNDLE <- fs::path("/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts",
                        "MatContractData", "EntitiesClaude")

# One unit per entity type. Roles are a CLOSED vocabulary: a free-text role cannot be dispatched on
# downstream, and 04D needs to turn these into columns. The session is given the list and told to use
# it verbatim.
#
# NEx is the examples shown per set. Redaction gets twice the rest because it is the only unit
# asking a question no previous session has seen, while the other four are a second pass over
# vocabulary that is largely taken: 51 organisation cues came out of 80 examples last time and
# fourteen survived measurement, so example 120 in that unit is worth much less than example 40 was.
.UNITS <- tibble::tibble(
  Label = c("ORG", "DATE", "GPE", "MONEY", "REDACT"),
  Slug  = c("01-org", "02-date", "03-gpe", "04-money", "05-redaction"),
  NEx   = c(60L, 60L, 60L, 60L, 120L),
  Roles = list(
    c("party", "agent", "guarantor", "affiliate", "third_party", "regulator"),
    c("signing", "effective", "term_start", "expiry", "statutory", "other"),
    c("party_address", "incorporation", "governing_law", "performance", "incidental"),
    c("contract_value", "periodic_payment", "fee", "cap", "per_unit", "redacted", "other"),
    # What the marker HIDES, not what the marker is. section_body is the whole-clause case that
    # "[INTENTIONALLY OMITTED]" marks, which is a different thing from a withheld number.
    c("royalty_rate", "milestone_payment", "unit_price", "aggregate_amount", "party_name",
      "term_length", "section_body", "other")
  )
)

.FOLDS_GENERATE <- 1:4        # folds the session may read
.FOLD_HOLDOUT   <- 5L         # reserved for scoring the rules; never bundled

.N_CONTEXT  <- 240L           # characters of context each side of a span
# Two per document rather than four. The example count is what costs reading time; the number of
# DISTINCT documents behind it is what buys variation, and halving the cap doubles the second at no
# cost in the first.
.N_PER_DOC  <- 2L
.N_DOCS_FULL <- 8L            # complete contracts included for the section-map question
.N_HEADINGS <- 150L           # rows of the measured heading inventory
.N_RULES    <- 30L            # rules requested per unit
.SEED       <- 43L            # a different draw from run 1, so the two sessions read different text

.DATE_WINDOW_DAYS <- 730L     # a contract date is expected within two years before the filing


# 2. Helpers --------------------------------------------------------------

#' Filesystem-safe slug
#' @param .x Character vector.
#' @return Lowercase hyphenated ASCII slug.
cwe_slug <- function(.x) {
  if (FALSE) .x <- "Employment: Compensation"
  .x |>
    stringi::stri_trans_tolower() |>
    stringi::stri_replace_all_regex("[^a-z0-9]+", "-") |>
    stringi::stri_replace_all_regex("^-+|-+$", "")
}

#' Slice a context window around a span and mark the span inside it
#'
#' Offsets are 0-based half-open code points, as every extractor emits them; stri_sub is 1-based and
#' inclusive, hence the +1. The left edge is clamped, because a negative "from" is read as counting
#' from the end and would silently return the wrong slice.
#'
#' @param .text Character vector of document text.
#' @param .start,.stop Integer offsets.
#' @param .window Integer characters each side.
#' @return Character vector with the span wrapped in << >>.
cwe_mark <- function(.text, .start, .stop, .window = 240L) {
  if (FALSE) {
    .text   <- "THIS AGREEMENT between Acme Holdings, Inc. and Beta Bank."
    .start  <- 23L
    .stop   <- 42L
    .window <- 20L
  }
  left_  <- stringi::stri_sub(.text, pmax(1L, .start + 1L - .window), .start)
  mid_   <- stringi::stri_sub(.text, .start + 1L, .stop)
  right_ <- stringi::stri_sub(.text, .stop + 1L, .stop + .window)
  paste0(
    stringi::stri_replace_all_regex(left_,  "\\s+", " "), "<<", mid_, ">>",
    stringi::stri_replace_all_regex(right_, "\\s+", " ")
  )
}


# 3. Pulling candidates out of the store ----------------------------------
# All bounding happens in DuckDB. The store holds millions of rows and only a few hundred are ever
# shown, so nothing large crosses into R.

#' Open the store read-only with the sample table registered
#'
#' @param .db_path Candidate store.
#' @param .tab_anchor Tibble with DocID and whatever anchor columns the label needs.
#' @return A live DBI connection; the caller disconnects.
cwe_connect <- function(.db_path, .tab_anchor) {
  if (FALSE) {
    .db_path    <- .PATH_STORE
    .tab_anchor <- tab_anchor
  }
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(.db_path), read_only = TRUE)
  duckdb::duckdb_register(con_, "cwe_anchor", as.data.frame(.tab_anchor))
  con_
}

#' Candidates of one label, bounded per document, optionally restricted to anchor matches
#'
#' The anchor predicate is pushed into SQL for ORG and GPE, where it is string containment on a
#' normalised form. DATE needs a parser and MONEY has no anchor at all, so both return unfiltered
#' rows and are classified in R.
#'
#' @param .con Connection from cwe_connect().
#' @param .label Entity label.
#' @param .anchored Logical or NULL. TRUE returns only anchor matches, FALSE only non-matches,
#'   NULL everything (used where the anchor cannot be expressed in SQL).
#' @param .n_per_doc Integer cap per document, so a long contract cannot fill the sample.
#' @return Tibble: DocID, Start, Stop, Span, Combos, Class, Fold.
cwe_pull <- function(.con, .label, .anchored = NULL, .n_per_doc = 4L) {
  if (FALSE) {
    .con       <- con
    .label     <- "ORG"
    .anchored  <- TRUE
    .n_per_doc <- 4L
  }

  # Normalised span, built identically to ent_norm_company's first pass so the two are comparable.
  norm_ <- "trim(regexp_replace(regexp_replace(upper(c.Span), '[^A-Z0-9 ]', ' ', 'g'), '\\s+', ' ', 'g'))"

  pred_ <- if (is.null(.anchored)) {
    "TRUE"
  } else if (identical(.label, "ORG")) {
    paste0("a.AnchorKey IS NOT NULL AND (contains(", norm_, ", a.AnchorKey) ",
           "OR contains(a.AnchorKey, ", norm_, "))")
  } else if (identical(.label, "GPE")) {
    "a.AnchorText IS NOT NULL AND length(c.Span) >= 4 AND contains(a.AnchorText, upper(c.Span))"
  } else {
    "TRUE"
  }
  if (isFALSE(.anchored) && !identical(pred_, "TRUE")) pred_ <- paste0("NOT (", pred_, ")")

  DBI::dbGetQuery(.con, paste0(
    "SELECT DocID, Start, Stop, Span, LabelRaw, Combos, Class, Fold FROM ( ",
    "  SELECT c.DocID, c.Start, c.Stop, c.Span, ",
    "         any_value(c.LabelRaw) AS LabelRaw, a.Class, a.Fold, ",
    "         string_agg(DISTINCT CASE WHEN c.Engine = c.Model THEN c.Engine ",
    "                    ELSE c.Engine || ':' || c.Model END, ',') AS Combos ",
    "  FROM candidates c JOIN cwe_anchor a USING (DocID) ",
    "  WHERE c.Label = ? AND (", pred_, ") ",
    "  GROUP BY c.DocID, c.Start, c.Stop, c.Span, a.Class, a.Fold ",
    "  QUALIFY row_number() OVER (PARTITION BY c.DocID ",
    "            ORDER BY hash(c.DocID || c.Span || CAST(c.Start AS VARCHAR))) <= ",
    as.integer(.n_per_doc), " )"
  ), params = list(.label)) |>
    tibble::as_tibble()
}

#' Classify DATE candidates against the filing date
#'
#' A contract's own date must precede its filing and, in practice, not by many years. Everything
#' outside that window is the interesting negative: statutory years carried by "Act of 1933",
#' maturities decades out, and bare four-digit years that parse to nothing meaningful. Those are the
#' spans that inflate the published duration measure, so showing them is the point of the unit.
#'
#' @param .tab Tibble from cwe_pull() with a Span column.
#' @param .filed Named character vector, DocID -> filing date.
#' @param .window_days Integer. How far before the filing a contract date may sit.
#' @return .tab with Parsed and IsAnchor columns.
cwe_flag_date <- function(.tab, .filed, .window_days = 730L) {
  if (FALSE) {
    .tab         <- tab_date
    .filed       <- filed_map
    .window_days <- 730L
  }
  .tab |>
    dplyr::mutate(
      Parsed = suppressWarnings(anytime::anydate(.data$Span)),
      Filed  = suppressWarnings(anytime::anydate(unname(.filed[.data$DocID]))),
      IsAnchor = !is.na(.data$Parsed) & !is.na(.data$Filed) &
        .data$Parsed <= .data$Filed &
        .data$Parsed >= (.data$Filed - .window_days)
    )
}

#' Draw the display sample, spread across contract types and document positions
#'
#' Proportional sampling would fill a unit with the three largest contract types and with whichever
#' region of the document the engines happen to favour. Both matter: a cue learned only from
#' preambles will not find a governing-law clause, and one learned only from credit agreements will
#' not find a lease. Dealing round-robin across type and position band buys that spread cheaply.
#'
#' @param .tab Candidate tibble carrying Class and Pos.
#' @param .n Integer rows wanted.
#' @param .by_labelraw Logical. Deal across LabelRaw as well, so a unit whose engine emits several
#'   classes shows all of them. Redaction needs it: an omitted SECTION and a withheld NUMBER are
#'   different questions, and a proportional draw would bury whichever class is rarer.
#' @return Tibble of at most .n rows.
cwe_spread <- function(.tab, .n, .by_labelraw = FALSE) {
  if (FALSE) {
    .tab         <- tab_target
    .n           <- 60L
    .by_labelraw <- FALSE
  }
  if (nrow(.tab) == 0L) return(.tab)
  keys_ <- if (isTRUE(.by_labelraw)) c("Class", "Band", "LabelRaw") else c("Class", "Band")
  .tab |>
    dplyr::mutate(
      Band = cut(.data$Pos, breaks = c(-0.01, 0.1, 0.5, 0.9, 1.01),
                 labels = c("head", "early", "late", "tail"))
    ) |>
    dplyr::slice_sample(prop = 1) |>
    dplyr::mutate(Slot = dplyr::row_number(), .by = dplyr::all_of(keys_)) |>
    dplyr::arrange(.data$Slot) |>
    utils::head(.n) |>
    dplyr::select(-Slot)
}


# 4. The measured section inventory ---------------------------------------

#' Count heading-like lines across the corpus
#'
#' The positional histogram in 04A shows geography arriving in two bursts, at the opening and again
#' at the end, which is the shape of two SECTIONS rather than of a prefix. If headings can be located
#' reliably, extraction can target regions instead of a first-N-characters window, which is both
#' cheaper than full text and better than a head window on exactly the labels a head window handles
#' worst. Whether they can is an empirical question about a corpus spanning two decades of
#' HTML-to-text conversion, so it is measured rather than assumed, and the measurement is written
#' into the bundle for the session to work from.
#'
#' Two shapes are counted. A standalone heading occupies its own short line. A run-in heading
#' carries an ARTICLE or SECTION number and its title before the paragraph continues on the same
#' line; ignoring those would undercount every document that uses the convention.
#'
#' @param .path_text Canonical text parquet.
#' @param .n Integer rows returned.
#' @return Tibble: Heading, DocFreq, PctDocs, sorted by frequency.
cwe_scan_headings <- function(.path_text, .n = 150L) {
  if (FALSE) {
    .path_text <- .PATH_TEXT
    .n         <- 150L
  }
  con_ <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  # Numbering is stripped only where it is genuinely numbering. An unanchored [IVXLCDM]+ would eat
  # the leading letters of ordinary words -- CREDIT becomes REDIT, DEFINITIONS becomes EFINITIONS --
  # so the Roman-numeral branch requires a trailing period and the bare-number branch a digit.
  strip_ <- paste0("'^((ARTICLE|SECTION)\\s+[IVXLCDM0-9]+(\\.[0-9]+)*|[0-9]+(\\.[0-9]+)*",
                   "|[IVXLCDM]+\\.)[.:]?\\s*'")
  runin_ <- paste0("'^(?:ARTICLE|SECTION)\\s+[IVXLCDM0-9]+(?:\\.[0-9]+)*\\.?\\s+",
                   "([A-Za-z][A-Za-z &''-]{2,58}?)\\s*[.:]'")

  out_ <- DBI::dbGetQuery(con_, paste0(
    "WITH lines AS ( ",
    "  SELECT DocID, trim(unnest(string_split(TextRaw, chr(10)))) AS Line ",
    "  FROM read_parquet('", as.character(fs::path_abs(.path_text)), "')), ",
    "standalone AS ( ",
    "  SELECT DocID, upper(trim(regexp_replace(Line, ", strip_, ", '', 'i'))) AS Heading ",
    "  FROM lines WHERE length(Line) BETWEEN 3 AND 80), ",
    "runin AS ( ",
    "  SELECT DocID, upper(trim(regexp_extract(Line, ", runin_, ", 1, 'i'))) AS Heading ",
    "  FROM lines WHERE length(Line) > 80), ",
    "cand AS (SELECT * FROM standalone UNION ALL SELECT * FROM runin), ",
    "tot AS (SELECT COUNT(DISTINCT DocID) AS NDocs FROM lines) ",
    "SELECT Heading, COUNT(DISTINCT DocID) AS DocFreq, ",
    "       COUNT(DISTINCT DocID) / any_value(tot.NDocs) AS PctDocs ",
    "FROM cand, tot WHERE Heading <> '' AND length(Heading) BETWEEN 3 AND 60 ",
    "  AND regexp_matches(Heading, '^[A-Z][A-Z &''-]*$') ",
    "  AND array_length(string_split(Heading, ' ')) <= 8 ",
    "GROUP BY Heading ORDER BY DocFreq DESC LIMIT ", as.integer(.n)
  ))

  out_ |>
    tibble::as_tibble() |>
    dplyr::mutate(DocFreq = as.integer(.data$DocFreq))
}


# 5. The agent-facing documents -------------------------------------------

#' Write one set of marked context windows as a single readable markdown file
#'
#' One file rather than one file per example: sixty short windows are read in sequence and sixty
#' files are not. The numbering is stable so a rule can cite the example it came from.
#'
#' @param .tab Sampled candidates carrying Marked, Class, Combos, Pos.
#' @param .path Destination file.
#' @param .title Heading for the file.
#' @param .note One sentence saying what this set is.
#' @return Invisible row count.
cwe_write_contexts <- function(.tab, .path, .title, .note) {
  if (FALSE) {
    .tab   <- tab_target
    .path  <- fs::path(dir_u_, "TARGET.md")
    .title <- "TARGET"
    .note  <- "Spans matching the filer's own name."
  }
  fs::dir_create(fs::path_dir(.path))
  body_ <- if (nrow(.tab) == 0L) {
    "_No examples available._"
  } else {
    purrr::map_chr(seq_len(nrow(.tab)), function(.i) {
      lab_ <- if (!is.null(.tab$LabelRaw) && !is.na(.tab$LabelRaw[[.i]])) {
        paste0("  |  ", .tab$LabelRaw[[.i]])
      } else {
        ""
      }
      paste0(
        sprintf("### %03d  |  %s%s  |  %s  |  %.0f%% through document",
                .i, .tab$Class[[.i]], lab_, .tab$Combos[[.i]], 100 * .tab$Pos[[.i]]),
        "\n\n",
        .tab$Marked[[.i]], "\n"
      )
    })
  }
  writeLines(c(paste0("# ", .title), "", .note, "",
               "The candidate span is wrapped in `<<` `>>`.", "", body_),
             .path, useBytes = TRUE)
  invisible(nrow(.tab))
}

#' Per-unit README
#' @param .label Entity label.
#' @param .roles Character vector of permitted roles.
#' @param .n_target,.n_other Examples actually written.
#' @param .n_rules Rules requested.
#' @param .slug Unit slug, which drives the response filename.
#' @param .anchored Logical. Whether this unit has an anchor-based contrast.
#' @return Character vector of markdown lines.
cwe_unit_readme <- function(.label, .roles, .n_target, .n_other, .n_rules, .slug, .anchored) {
  if (FALSE) {
    .label    <- "ORG"
    .roles    <- .UNITS$Roles[[1]]
    .n_target <- 60L
    .n_other  <- 60L
    .n_rules  <- 30L
    .slug     <- "01-org"
    .anchored <- TRUE
  }

  sets_ <- if (identical(.label, "REDACT")) {
    c(paste0("- `SAMPLE.md` -- ", .n_other, " redaction markers in context"),
      "",
      "Each marker stands where something was removed before filing. The class of marker is",
      "in the header line: a symbol form such as `[***]` normally replaces a value, while",
      "`[INTENTIONALLY OMITTED]` normally replaces a whole clause. Read the surrounding",
      "text and say WHAT IS MISSING, and which phrase tells you.",
      "",
      "This is the question the rest of the project cannot ask. The number of redactions per",
      "contract is already counted; what they conceal is not known.")
  } else if (.anchored) {
    c(paste0("- `TARGET.md` -- ", .n_target, " spans that ARE the known entity"),
      paste0("- `OTHER.md`  -- ", .n_other, " spans that are not"),
      "",
      "TARGET is not a definition of the role. It is one entity per document that external",
      "records prove is present. Everything in OTHER is unlabelled, and some of it is",
      "certainly correct too. Your job is to find what the TARGET spans have in common that",
      "you can also see working in OTHER.")
  } else {
    c(paste0("- `SAMPLE.md` -- ", .n_other, " spans in context, labelled with contract type"),
      "",
      "There is no external record of this quantity, so there is no TARGET set. Read the",
      "sample and say which cues mark which role, and where the roles differ by contract type.")
  }

  extra_ <- if (identical(.label, "REDACT")) {
    c("",
      "## Two things per rule",
      "",
      "`Role` is what the marker hides. `Pattern` is the phrase in the surrounding text that",
      "tells you so. A rule therefore reads: when this phrase sits near a marker, the thing",
      "removed was of this kind.",
      "",
      "Some markers will be uninformative -- the surrounding text says nothing about what was",
      "there. Say so in a `note` rather than guessing; how often that happens is itself the",
      "answer to whether this question can be asked at scale.")
  } else {
    character()
  }

  c(paste0("# Unit: ", .label), "",
    sets_, extra_, "",
    "## Roles for this unit",
    "",
    "Use these strings exactly in the `Role` field. Do not invent others:",
    "",
    paste0("- `", .roles, "`"),
    "",
    paste0("Propose about ", .n_rules, " rules."),
    paste0("Write your answer to `responses/", .slug, ".jsonl`."),
    "",
    "See `INSTRUCTIONS.md` in the bundle root for the rule format.")
}

#' The pinned prompt written into the bundle root
#'
#' Says nothing about which engines produced the spans, how many candidates each returns, or where
#' the pipeline currently believes the good ones are. The session's judgement has to be independent
#' of the extraction's or the two arms stop being independent.
#'
#' @param .units Tibble of units in the bundle.
#' @param .n_rules Rules requested per unit.
#' @param .n_context Context window in characters.
#' @return Character vector of markdown lines.
cwe_instructions <- function(.units, .n_rules, .n_context) {
  if (FALSE) {
    .units     <- .UNITS
    .n_rules   <- 30L
    .n_context <- 240L
  }

  ex_cue_ <- paste0(
    '{"Kind":"cue","Label":"ORG","Role":"party","Pattern":"by and between","Side":"left",',
    '"Window":80,"Rationale":"Introduces the contracting parties in the preamble.",',
    '"Evidence":"units/01-org/TARGET.md#004"}'
  )
  ex_stop_ <- paste0(
    '{"Kind":"stop","Label":"ORG","Role":"regulator","Pattern":"securities and exchange commission",',
    '"Side":"span","Window":0,"Rationale":"A regulator named in boilerplate, never a party.",',
    '"Evidence":"units/01-org/OTHER.md#017"}'
  )
  ex_sec_ <- paste0(
    '{"Kind":"section","Label":"GPE","Role":"governing_law","Pattern":"governing law",',
    '"Side":"heading","Window":0,"Rationale":"The jurisdiction sits under this heading.",',
    '"Evidence":"documents/003_lease.txt"}'
  )
  ex_note_ <- paste0(
    '{"Kind":"note","Label":"ORG","Role":"party","Pattern":"",',
    '"Side":"","Window":0,"Rationale":"A party is usually followed by a parenthesised defined ',
    'term in quotes, which a literal phrase cannot express.","Evidence":"units/01-org/TARGET.md#009"}'
  )

  c(
    "# Entity rules from extracted contracts",
    "",
    "You are working inside this folder. Everything you need is here.",
    "",
    "## What this is for",
    "",
    "Automatic extractors have proposed millions of entity spans across these contracts. Most",
    "are not what we want. A contract names two or three parties and the extractors return",
    "hundreds of organisations per document; a contract has one signing date and they return",
    "dozens of dates.",
    "",
    "We cannot have a model read every candidate -- there are far too many. So we need RULES",
    "that a machine can apply: phrases that appear near a span and tell you what it is.",
    "",
    "## The task",
    "",
    "Each folder under `units/` is one entity type. Each holds spans in context, with the",
    sprintf("candidate wrapped in `<<` `>>` and about %d characters either side.", .n_context),
    "",
    "Read them, then propose rules of four kinds:",
    "",
    "- **cue** -- a phrase that appears near a span and marks its role",
    "- **stop** -- a span that is never the thing, however often it appears",
    "- **section** -- a document heading under which a role reliably sits",
    "- **note** -- something you noticed that no literal phrase can express",
    "",
    "## Cues are literal phrases, not patterns",
    "",
    "A cue is matched case-insensitively as plain words within its window. Write words only:",
    "",
    "1. One to eight words.",
    "2. Letters and spaces only -- no digits, punctuation, brackets or wildcards.",
    "3. Lowercase.",
    "4. `Side` is `left`, `right` or `either`, relative to the span.",
    "5. `Window` is how many characters away it may sit. Be tight where the phrase abuts the",
    "   span and generous where it does not.",
    "",
    "If the thing that identifies a span is STRUCTURAL rather than lexical -- a parenthesis, a",
    "quotation, a capitalisation habit -- do not try to encode it. Write it as a `note` in",
    "plain English and it will be implemented by hand.",
    "",
    "## Be generous, not precise",
    "",
    sprintf("Propose about %d rules per unit. Do not self-censor.", .n_rules),
    "",
    "Every rule you propose is afterwards measured against thousands of contracts nobody",
    "showed you: how often it fires, and whether what it keeps is right. Rules that fail are",
    "dropped mechanically and cost nothing. Rules you never proposed cannot be recovered.",
    "",
    "## The redaction unit is a different question",
    "",
    "One unit holds redaction markers rather than entities. Each marker stands where something",
    "was removed from the contract before it was filed, so the text around it describes a gap.",
    "For those, `Role` is WHAT WAS REMOVED and `Pattern` is the phrase that tells you.",
    "",
    "The class of marker is shown in each example's header line. A symbol form normally",
    "replaces a value; a wording such as an intentional omission normally replaces a whole",
    "clause. Treat those as different cases.",
    "",
    "Where the surrounding text gives no clue what was removed, say so in a `note` instead of",
    "guessing. How often that happens is itself a result.",
    "",
    "## The document structure question",
    "",
    "`SECTIONS.md` lists the headings that actually occur in this corpus, with the share of",
    "documents carrying each. `documents/` holds a few complete contracts so you can see how",
    "they are laid out.",
    "",
    "Entities are not spread evenly through a contract. Parties are named in the preamble;",
    "the governing jurisdiction sits near the end; amounts sit in schedules. If a role lives",
    "reliably under a heading, say so as a `section` rule -- that lets extraction target",
    "regions of a document rather than reading all of it.",
    "",
    "If the headings in `SECTIONS.md` look too inconsistent to rely on, say that in a `note`",
    "and move on. A negative answer there is a useful result, not a failure.",
    "",
    "## What to write",
    "",
    "One file per unit in `responses/`, named after the unit folder, extension `.jsonl`.",
    "One JSON object per line, no wrapping array, no markdown fences:",
    "",
    "```",
    ex_cue_,
    ex_stop_,
    ex_sec_,
    ex_note_,
    "```",
    "",
    "Fields:",
    "",
    "- `Kind`      -- `cue`, `stop`, `section` or `note`",
    "- `Label`     -- the unit's entity type, exactly as in its `UNIT.md`",
    "- `Role`      -- one of the roles listed in that `UNIT.md`, exactly",
    "- `Pattern`   -- the literal phrase; empty for a `note`",
    "- `Side`      -- `left`, `right`, `either`, `span` or `heading`",
    "- `Window`    -- characters; 0 where it does not apply",
    "- `Rationale` -- one sentence, why this discriminates",
    "- `Evidence`  -- the file and example number where you saw it",
    "",
    "`Evidence` is not bookkeeping. Naming the example forces the rule to come from the text",
    "in front of you rather than from general intuition about what a contract probably says.",
    "",
    "## Order of work",
    "",
    "Do one unit at a time and finish its response file before starting the next. The units",
    "are independent, so the session can be stopped and resumed.",
    "",
    "## Units in this bundle",
    "",
    paste0("- `", .units$Slug, "` -- ", .units$Label)
  )
}

#' The measured heading inventory, written for the session to read
#' @param .tab Tibble from cwe_scan_headings().
#' @param .n_docs Integer documents scanned.
#' @return Character vector of markdown lines.
cwe_sections_md <- function(.tab, .n_docs) {
  if (FALSE) {
    .tab    <- tab_head
    .n_docs <- 4398L
  }
  top_ <- .tab |>
    dplyr::mutate(Pct = paste0(formatC(100 * .data$PctDocs, format = "f", digits = 1), "%")) |>
    dplyr::mutate(Row = paste0("| ", .data$Heading, " | ", .data$DocFreq, " | ", .data$Pct, " |"))

  c("# Headings that occur in this corpus", "",
    paste0("Measured over ", .n_docs, " contracts. A line was counted as a heading if it stood",
           " alone and was short, or if it followed an ARTICLE or SECTION number on a longer line."),
    "",
    "This is evidence, not a target. If the top of this table is dominated by headings that",
    "appear in almost every contract, sections are reliable and worth building rules around. If",
    "nothing clears a modest share of documents, they are not, and cues are the only route.",
    "",
    "| Heading | Documents | Share |",
    "|:--|--:|--:|",
    top_$Row)
}


# 6. Build the bundle -----------------------------------------------------

#' Write the complete CoWork entity-rule bundle
#'
#' @param .path_text Canonical text parquet from 04A.
#' @param .path_anchors Sample anchors parquet from 04A.
#' @param .path_store Candidate store from 04A.
#' @param .dir_bundle Destination folder; must be empty or absent.
#' @param .units Tibble of Label, Slug, Roles.
#' @param .folds_generate Folds the session may read.
#' @param .fold_holdout Fold reserved for scoring; never bundled.
#' @param .n_context Context characters each side. Per-unit example counts come from .units$NEx.
#' @param .n_per_doc Cap per document per label.
#' @param .n_docs_full Complete contracts included for the structure question.
#' @param .n_headings Rows of the heading inventory.
#' @param .n_rules Rules requested per unit.
#' @param .window_days Date-anchor window.
#' @param .seed Sampling seed, recorded in the manifest.
#' @return Invisible tibble, one row per unit with the counts actually written.
cwe_write_bundle <- function(.path_text, .path_anchors, .path_store, .dir_bundle,
                             .units = .UNITS,
                             .folds_generate = 1:4, .fold_holdout = 5L,
                             .n_context = 240L,
                             .n_per_doc = 2L, .n_docs_full = 8L, .n_headings = 150L,
                             .n_rules = 30L, .window_days = 730L, .seed = 43L) {
  if (FALSE) {
    .path_text      <- .PATH_TEXT
    .path_anchors   <- .PATH_ANCHORS
    .path_store     <- .PATH_STORE
    .dir_bundle     <- .DIR_BUNDLE
    .units          <- .UNITS
    .folds_generate <- 1:4
    .fold_holdout   <- 5L
    .n_context      <- 240L
    .n_per_doc      <- 2L
    .n_docs_full    <- 8L
    .n_headings     <- 150L
    .n_rules        <- 30L
    .window_days    <- 730L
    .seed           <- 43L
  }

  for (p_ in c(.path_text, .path_anchors, .path_store)) {
    if (!fs::file_exists(p_)) cli::cli_abort("Missing {(p_)} -- run 04A first")
  }
  if (fs::dir_exists(.dir_bundle) && length(fs::dir_ls(.dir_bundle)) > 0L) {
    cli::cli_abort("{(.dir_bundle)} is not empty -- delete it or choose another path")
  }
  set.seed(.seed)

  # Built by the measurement library, so the geographic anchor here carries the expanded state
  # names it carries there. Copying this logic is what let the two drift the first time.
  anch_ <- ent_anchor_keys(.path_anchors = .path_anchors) |>
    dplyr::filter(.data$Fold %in% .folds_generate) |>
    dplyr::mutate(DateFiled = as.character(.data$DateFiled))

  if (any(anch_$Fold == .fold_holdout)) {
    cli::cli_abort("Holdout fold {(.fold_holdout)} leaked into the pool -- aborting")
  }
  cli::cli_alert_info(
    "Pool: {nrow(anch_)} documents from folds {paste(.folds_generate, collapse = ', ')} \\
     (fold {(.fold_holdout)} held out). Company anchor on {sum(!is.na(anch_$AnchorKey))}, \\
     address anchor on {sum(!is.na(anch_$AnchorText))}."
  )

  lens_ <- arrow::open_dataset(.path_text) |>
    dplyr::select(DocID, TextRaw) |>
    dplyr::collect() |>
    dplyr::mutate(DocLen = stringi::stri_length(.data$TextRaw))

  con_ <- cwe_connect(.path_store, anch_)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  fs::dir_create(fs::path(.dir_bundle, "units"))
  fs::dir_create(fs::path(.dir_bundle, "responses"))

  filed_ <- rlang::set_names(anch_$DateFiled, anch_$DocID)

  finish_ <- function(.tab) {
    .tab |>
      dplyr::inner_join(lens_, by = dplyr::join_by(DocID)) |>
      dplyr::filter(.data$DocLen > 0) |>
      dplyr::mutate(
        Pos    = ((.data$Start + .data$Stop) / 2) / .data$DocLen,
        Marked = cwe_mark(.data$TextRaw, as.integer(.data$Start), as.integer(.data$Stop), .n_context)
      ) |>
      dplyr::select(-TextRaw)
  }

  out_ <- purrr::pmap(.units, function(Label, Slug, NEx, Roles) {
    cli::cli_h3("{Label}")
    dir_u_ <- fs::path(.dir_bundle, "units", Slug)
    anchored_ <- Label %in% c("ORG", "GPE", "DATE")
    by_raw_   <- identical(Label, "REDACT")

    if (identical(Label, "DATE")) {
      all_ <- cwe_pull(con_, Label, .anchored = NULL, .n_per_doc = .n_per_doc * 2L) |>
        cwe_flag_date(filed_, .window_days)
      tgt_ <- dplyr::filter(all_, .data$IsAnchor)
      oth_ <- dplyr::filter(all_, !.data$IsAnchor)
    } else if (anchored_) {
      tgt_ <- cwe_pull(con_, Label, .anchored = TRUE,  .n_per_doc = .n_per_doc)
      oth_ <- cwe_pull(con_, Label, .anchored = FALSE, .n_per_doc = .n_per_doc)
    } else {
      oth_ <- cwe_pull(con_, Label, .anchored = NULL, .n_per_doc = .n_per_doc)
      tgt_ <- oth_[0, ]   # no anchor exists for this label; an empty TARGET, not a wasted query
    }

    tgt_ <- if (nrow(tgt_) > 0L) cwe_spread(finish_(tgt_), NEx) else tgt_
    oth_ <- cwe_spread(finish_(oth_), NEx, .by_labelraw = by_raw_)

    if (anchored_) {
      cwe_write_contexts(tgt_, fs::path(dir_u_, "TARGET.md"), paste0(Label, " -- TARGET"),
                         "Spans that external records confirm are present in the contract.")
      cwe_write_contexts(oth_, fs::path(dir_u_, "OTHER.md"), paste0(Label, " -- OTHER"),
                         "Spans with no such confirmation. Some are correct; most are not.")
      if (nrow(tgt_) < NEx) {
        cli::cli_alert_warning("{Label}: only {nrow(tgt_)} anchored example{?s} (asked {NEx})")
      }
    } else {
      note_ <- if (by_raw_) {
        "Redaction markers across contract types and marker classes. What was removed is the question."
      } else {
        "A spread of spans across contract types. No external anchor exists here."
      }
      cwe_write_contexts(oth_, fs::path(dir_u_, "SAMPLE.md"), paste0(Label, " -- SAMPLE"), note_)
    }

    writeLines(
      cwe_unit_readme(Label, Roles, nrow(tgt_), nrow(oth_), .n_rules, Slug, anchored_),
      fs::path(dir_u_, "UNIT.md")
    )
    if (by_raw_ && nrow(oth_) > 0L) {
      mix_ <- oth_ |> dplyr::count(.data$LabelRaw, name = "N")
      cli::cli_alert_info("Marker classes shown: {paste0(mix_$LabelRaw, '=', mix_$N, collapse = ', ')}")
    }
    tibble::tibble(Unit = Slug, Label = Label, NTarget = nrow(tgt_), NOther = nrow(oth_))
  }) |>
    purrr::list_rbind()

  # Complete contracts, for the structure question only.
  docs_ <- lens_ |>
    dplyr::semi_join(anch_, by = dplyr::join_by(DocID)) |>
    dplyr::filter(dplyr::between(.data$DocLen, 8000, 120000)) |>
    dplyr::slice_sample(n = .n_docs_full) |>
    dplyr::left_join(dplyr::select(anch_, DocID, Class), by = dplyr::join_by(DocID))
  fs::dir_create(fs::path(.dir_bundle, "documents"))
  purrr::walk(seq_len(nrow(docs_)), function(.i) {
    writeLines(docs_$TextRaw[[.i]],
               fs::path(.dir_bundle, "documents",
                        sprintf("%03d_%s.txt", .i, cwe_slug(docs_$Class[[.i]]))),
               useBytes = TRUE)
  })

  cli::cli_alert_info("Scanning headings ...")
  head_ <- cwe_scan_headings(.path_text, .n_headings)
  writeLines(cwe_sections_md(head_, nrow(lens_)), fs::path(.dir_bundle, "SECTIONS.md"))
  cli::cli_alert_info(
    "Top heading: {head_$Heading[1]} in {formatC(100 * head_$PctDocs[1], format = 'f', digits = 1)}% \\
     of documents. Below roughly 40% the section route is not worth building on."
  )

  writeLines(cwe_instructions(.units, .n_rules, .n_context),
             fs::path(.dir_bundle, "INSTRUCTIONS.md"))

  jsonlite::write_json(
    list(
      bundle_version  = "entity_rules_v1",
      created_at      = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      text_path       = as.character(.path_text),
      anchors_path    = as.character(.path_anchors),
      store_path      = as.character(.path_store),
      folds_generate  = .folds_generate,
      fold_holdout    = .fold_holdout,
      n_context       = .n_context,
      n_per_doc       = .n_per_doc,
      n_rules_asked   = .n_rules,
      date_window_days = .window_days,
      seed            = .seed,
      n_pool_docs     = nrow(anch_),
      roles           = rlang::set_names(.units$Roles, .units$Label),
      units           = out_,
      headings_top    = utils::head(head_, 25L),
      r_version       = paste(R.version$major, R.version$minor, sep = ".")
    ),
    fs::path(.dir_bundle, "MANIFEST.json"), auto_unbox = TRUE, pretty = TRUE
  )

  cli::cli_alert_success(
    "Bundle written: {nrow(out_)} units, {sum(out_$NTarget) + sum(out_$NOther)} examples, \\
     {nrow(docs_)} complete contracts"
  )
  cli::cli_alert_info("Point CoWork at {(.dir_bundle)} and start with INSTRUCTIONS.md")
  invisible(out_)
}


# 7. Run --------------------------------------------------------------------------------------------
# Idempotent: a bundle already written is left alone, so re-sourcing after a partial session is safe.
# Reading the responses back is a separate concern and lives in 04B-CoWorkCompile.R.

if (fs::dir_exists(.DIR_BUNDLE) && length(fs::dir_ls(.DIR_BUNDLE)) > 0L) {
  cli::cli_alert_info("Bundle already written at {(.DIR_BUNDLE)}; leaving it alone")
} else {
  cwe_write_bundle(
    .path_text      = .PATH_TEXT,          # canonical text; the offset basis for every context slice
    .path_anchors   = .PATH_ANCHORS,       # labels, folds and the EDGAR facts the contrast rests on
    .path_store     = .PATH_STORE,         # candidate store written by 04A
    .dir_bundle     = .DIR_BUNDLE,         # one self-contained folder, outside the repository
    .units          = .UNITS,              # one unit per entity type, with its closed role vocabulary
    .folds_generate = .FOLDS_GENERATE,     # folds the session may read
    .fold_holdout   = .FOLD_HOLDOUT,       # never bundled; the honest estimate comes from here
    .n_context      = .N_CONTEXT,          # characters each side of a span
    .n_per_doc      = .N_PER_DOC,          # cap per document, so no contract dominates a unit
    .n_docs_full    = .N_DOCS_FULL,        # complete contracts for the structure question
    .n_headings     = .N_HEADINGS,         # rows of the measured heading inventory
    .n_rules        = .N_RULES,            # rules requested per unit
    .window_days    = .DATE_WINDOW_DAYS,   # how far before filing a contract date may sit
    .seed           = .SEED                # recorded in the manifest
  )
}

cli::cli_h2("Next")
cli::cli_text(
  "Point the reading session at {(.DIR_BUNDLE)} and start from INSTRUCTIONS.md. When the responses \\
   are written, 04B-CoWorkCompile.R turns them into rule files, and 04B scores those on fold \\
   {(.FOLD_HOLDOUT)} against the EDGAR anchors."
)
