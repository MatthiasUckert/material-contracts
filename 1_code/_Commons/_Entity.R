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
# _Utils.R, _NER.R, _Plots.R and _Tables.R and before the entity's own function file. _Store.R is NOT
# in that list any more: it was deleted, its three live functions now live in _NER.R, and the per-
# label schema it declared is gone with the flat store it described.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .path_text     <- .lP$Input$Text
  .path_prepared <- .lP$Input$Prepared
  .path_register <- .lP$Input$Register
  .dir_store     <- .lP$Input$Store
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


#' The anchor columns, from a table already carrying CompanyName and DateFiled
#'
#' THE ANCHOR IS DEFINED ONCE, HERE. Two callers need it and they differ only in where their rows
#' come from: ent_anchor_keys() takes the labelled spine, ent_corpus_keys() takes the register whole.
#' When the scoring side of 04B learned that EDGAR stores states as two-letter codes and contracts
#' write them out, a second copy of the anchor logic did not learn it, and the geographic contrast a
#' reading session saw was built from cities alone while the measurement behind it was not. A
#' definition with two implementations has one that is wrong or about to be.
#'
#' @param .tab Tibble carrying CompanyName and DateFiled.
#' @return .tab with CompanyClean, DateFiled parsed, AnchorKey, AnchorTight, AnchorLen, AnchorTok.
ent_anchor_cols <- function(.tab) {
  if (FALSE) .tab <- reg_

  .tab |>
    dplyr::mutate(
      CompanyClean = ent_strip_conformed(.x = .data$CompanyName),
      DateFiled    = suppressWarnings(anytime::anydate(as.character(.data$DateFiled))),
      AnchorKey    = ent_norm_key(.x = .data$CompanyClean, .min = 1L),
      AnchorTight  = stringi::stri_replace_all_fixed(.data$AnchorKey, " ", ""),
      AnchorLen    = nchar(.data$AnchorKey),
      AnchorTok    = stringi::stri_count_fixed(.data$AnchorKey, " ") + 1L
    )
}


#' Anchor keys at corpus scale, from the register and a classification release
#'
#' WHAT DIFFERS FROM ent_anchor_keys(), AND WHAT DOES NOT. The anchor -- the filer's own name and the
#' filing date -- is identical and comes from ent_anchor_cols(). What differs is the row source and
#' the class: 04B takes both from 03A's labelled spine of 4,398 documents, and at corpus scale there
#' is no spine. The register carries every document, and 03F's release carries the class for every
#' document it could label.
#'
#' THE ENGINE IS DECLARED, NOT RESOLVED HERE. 03F's release carries every engine's label side by side
#' and deliberately no single Class column: which engine is authoritative is a decision, and a
#' decision belongs in a configuration a reader can see rather than inside a function they would have
#' to open.
#'
#' THE JOIN IS LEFT AND THE HOLE IS COUNTED. A document 03F could not label is a real category -- 736
#' hold no text at all -- and an inner join would drop exactly the rows the coverage line reports.
#'
#' @param .path_register 02B's Documents.parquet.
#' @param .path_release Release parquet from 03F. NA leaves Class and AmendType missing.
#' @param .engine Character. Label prefix in the release, "Bert" or "Kw".
#' @param .doc_ids Character. Restrict to these documents; NULL takes every register row.
#' @param .quiet Logical. Suppress the coverage line.
#' @return Tibble: the anchor columns, Class, ClassBroad, AmendType, NWords, nChars.
ent_corpus_keys <- function(.path_register, .path_release = NA_character_, .engine = "Bert",
                            .doc_ids = NULL, .quiet = FALSE) {
  if (FALSE) {
    .path_register <- .lP$Input$Register
    .path_release  <- path_release
    .engine        <- "Bert"
    .doc_ids       <- tab_index$DocID
    .quiet         <- FALSE
  }

  reg_ <- arrow::open_dataset(sources = .path_register) |>
    dplyr::select("DocID", "HashDocument", "HashIndex", "CIK", "CompanyName", "DateFiled",
                  "nWords", "nChars")
  if (!is.null(.doc_ids)) reg_ <- dplyr::filter(reg_, .data$DocID %in% .doc_ids)

  out_ <- reg_ |>
    dplyr::collect() |>
    ent_anchor_cols() |>
    dplyr::mutate(NWords = as.integer(.data$nWords))

  cls_ <- ent_corpus_class(
    .path_release = .path_release, .engine = .engine, .doc_ids = out_$DocID
  )
  out_ <- dplyr::left_join(out_, cls_, by = dplyr::join_by(DocID))

  if (!.quiet) {
    cli::cli_alert_success(
      "{format(nrow(out_), big.mark = ',')} {cli::qty(nrow(out_))}anchor{?s} built from the \\
       register."
    )
    miss_ <- sum(is.na(out_$CompanyName))
    if (miss_ > 0L) {
      cli::cli_alert_danger(
        "{format(miss_, big.mark = ',')} {cli::qty(miss_)}document{?s} carry no company name and \\
         can only take a fallback party."
      )
    }
    if ("Class" %in% names(out_)) {
      cli::cli_alert_info(
        "Class present for {tbl_pct(mean(!is.na(out_$Class)))} of documents."
      )
    }
  }
  out_
}


#' The corpus classification, read under one declared engine
#'
#' THE SCHEMA IS READ, NOT ASSUMED. Which label columns a release carries depends on which tasks 03F
#' ran, and asking for one that is absent should name what is there -- the alternative is a silently
#' missing column arriving downstream as NA and reading as an unlabelled corpus.
#'
#' @param .path_release Release parquet from 03F; NA returns an empty frame.
#' @param .engine Character. Label prefix, "Bert" or "Kw".
#' @param .doc_ids Character. Restrict to these documents.
#' @return Tibble: DocID, Class, ClassBroad, AmendType, and HierConsistent where the release has it.
ent_corpus_class <- function(.path_release, .engine = "Bert", .doc_ids) {
  if (FALSE) {
    .path_release <- path_release
    .engine       <- "Bert"
    .doc_ids      <- tab_index$DocID
  }

  empty_ <- tibble::tibble(
    DocID = character(0), Class = character(0), ClassBroad = character(0),
    AmendType = character(0)
  )
  if (is.na(.path_release)) return(empty_)

  have_ <- names(arrow::open_dataset(sources = .path_release))
  want_ <- c(
    Class      = paste0(.engine, "ClassDetailed"),
    ClassBroad = paste0(.engine, "ClassBroad"),
    AmendType  = paste0(.engine, "AmendType")
  )
  got_ <- want_[want_ %in% have_]

  if (length(got_) == 0L) {
    cli::cli_abort(c(
      "The release carries no {(.engine)} label columns.",
      "i" = "It has: {paste(have_, collapse = ', ')}.",
      "x" = "Name a prefix this file actually uses."
    ))
  }
  if (length(got_) < length(want_)) {
    cli::cli_alert_warning(
      "The release has no {paste(setdiff(want_, got_), collapse = ', ')}; \\
       {cli::qty(length(setdiff(want_, got_)))}{?that column is/those columns are} left missing."
    )
  }

  arrow::open_dataset(sources = .path_release) |>
    dplyr::select(dplyr::all_of(c("DocID", unname(got_), intersect("HierConsistent", have_)))) |>
    dplyr::filter(.data$DocID %in% .doc_ids) |>
    dplyr::collect() |>
    dplyr::rename(dplyr::all_of(got_)) |>
    dplyr::distinct(.data$DocID, .keep_all = TRUE)
}


#' The newest usable classification release
#'
#' THE FILE IS FOUND, NOT NAMED. 03F stamps the transformer length into the stem and appends _partial
#' where a pass was still running, so a hard-coded name is wrong on the day either changes. A complete
#' release wins; a partial one is used only where nothing else exists, and says so, because a silently
#' partial class column makes every class contrast a statement about an unnamed subset.
#'
#' @param .dir_release 03F's release directory.
#' @return Character path, or NA where none exists.
ent_release_path <- function(.dir_release) {
  if (FALSE) .dir_release <- .lP$Input$Release

  if (!fs::dir_exists(.dir_release)) return(NA_character_)

  files_ <- fs::dir_ls(.dir_release, glob = "*contract_labels*.parquet")
  files_ <- files_[!stringi::stri_detect_fixed(fs::path_file(files_), "_manifest")]
  if (length(files_) == 0L) return(NA_character_)

  full_ <- files_[!stringi::stri_detect_fixed(fs::path_file(files_), "_partial")]
  pick_ <- if (length(full_) > 0L) full_ else files_
  pick_ <- pick_[order(fs::file_info(pick_)$modification_time, decreasing = TRUE)][[1L]]

  if (length(full_) == 0L) {
    cli::cli_alert_warning(
      "Only a partial release exists ({.path {as.character(fs::path_file(pick_))}}); the class \\
       column covers part of the corpus and every contrast on it inherits that."
    )
  }
  as.character(pick_)
}


#' Per-document anchor facts, one row per contract
#'
#' THE ANCHORS ARE BUILT HERE AND NOT READ FROM AN ARTIFACT, and that is a change of design rather
#' than a change of path. An earlier version read a sample_anchors.parquet that 04A was supposed to
#' write; five documents named it as a hardcoded string and nothing wrote it, so five documents could
#' not render. The columns are not extraction results -- they are the registrant's own facts -- so
#' nothing about them belongs to a document that runs extractors. Two reads and a join produce them,
#' and the join is reported rather than assumed.
#'
#' Labels and folds come from 03A's prepared sample, which is the partition every method in the
#' monorepo scores against. Identity and filing date come from 02B's register. The registered
#' addresses are the exception: they exist only in 01C's landing page, they are wanted only by the
#' geography rule, and they are therefore optional -- a missing landing page must not stop the ORG
#' document from rendering.
#'
#' No length floor is applied to the key. AnchorLen is carried and each entity's rule decides what is
#' long enough for equality and what for containment, which are different questions.
#'
#' @param .path_prepared 03A's prepared.parquet: DocID, folds, ClassDetailed, AmendType.
#' @param .path_register 02B's Documents.parquet: CIK, CompanyName, DateFiled, HashDocument.
#' @param .path_landing 01C's LandingPage.parquet, for the registered addresses. NULL omits them.
#' @param .quiet Logical. Suppress the coverage report.
#' @return Tibble: one row per document with the labels, the company name and key, the filing date
#'   and, where the landing page was supplied, the two addresses.
ent_anchor_keys <- function(.path_prepared, .path_register, .path_landing = NULL, .quiet = FALSE) {
  if (FALSE) {
    .path_prepared <- .lP$Input$Prepared
    .path_register <- .lP$Input$Register
    .path_landing  <- .lP$Input$Landing
    .quiet         <- FALSE
  }

  if (!fs::file_exists(.path_prepared)) {
    cli::cli_abort("No prepared sample at {.path {(.path_prepared)}}.")
  }
  if (!fs::file_exists(.path_register)) {
    cli::cli_abort("No register at {.path {(.path_register)}}.")
  }

  spine_ <- arrow::open_dataset(sources = .path_prepared) |>
    dplyr::select("DocID", "ClassDetailed", "AmendType", "Fold") |>
    dplyr::collect()

  # THE REGISTER IS READ WITH open_dataset AND FILTERED BEFORE COLLECTING, because it holds 1.77
  # million rows and this needs 4,398 of them. Selecting by name rather than by a relocate list, for
  # the reason the 03 pass established: any_of() drops silently, so a list of intended columns is
  # neither a superset nor a subset of what is actually there.
  reg_ <- arrow::open_dataset(sources = .path_register) |>
    dplyr::select("DocID", "HashDocument", "HashIndex", "CIK", "CompanyName", "DateFiled",
                  "nWords", "nChars") |>
    dplyr::filter(.data$DocID %in% spine_$DocID) |>
    dplyr::collect()

  # LEFT JOIN AND REPORT, NEVER INNER. A check that cannot fail is not a check: an inner join would
  # silently drop any labelled document the register does not carry, and the count that matters is
  # exactly the count it would have hidden.
  out_ <- dplyr::left_join(spine_, reg_, by = dplyr::join_by(DocID))

  if (!is.null(.path_landing) && fs::file_exists(.path_landing)) {
    land_ <- arrow::open_dataset(sources = .path_landing)
    addr_ <- intersect(c("BusinessAddress", "MailingAddress"), names(land_))
    if (length(addr_) > 0L) {
      out_ <- out_ |>
        dplyr::left_join(
          land_ |>
            dplyr::select(dplyr::all_of(c("HashIndex", addr_))) |>
            dplyr::filter(.data$HashIndex %in% out_$HashIndex) |>
            dplyr::collect() |>
            dplyr::distinct(.data$HashIndex, .keep_all = TRUE),
          by = dplyr::join_by(HashIndex)
        )
    }
  }

  out_ <- out_ |>
    dplyr::rename(Class = "ClassDetailed") |>
    ent_anchor_cols()

  if (!.quiet) {
    miss_ <- sum(is.na(out_$CompanyName))
    cli::cli_alert_success(
      "{format(nrow(out_), big.mark = ',')} {cli::qty(nrow(out_))}anchor{?s} built from the \\
       prepared sample and the register."
    )
    if (miss_ > 0L) {
      cli::cli_alert_danger(
        "{format(miss_, big.mark = ',')} {cli::qty(miss_)}document{?s} reached no register row, so \\
         {?it has/they have} no company name to match against and can only take a fallback party."
      )
    } else {
      cli::cli_alert_success("Every labelled document carries a registered company name.")
    }
    addr_have_ <- intersect(c("BusinessAddress", "MailingAddress"), names(out_))
    if (length(addr_have_) > 0L) {
      cli::cli_alert_info(
        "Addresses present on \\
         {tbl_pct(mean(!is.na(out_[[addr_have_[[1L]]]])))} of documents."
      )
    }
  }

  out_
}


#' The columns a family calls something else
#'
#' RENAMED ON READ, AND ONLY ON READ. The stores keep each family's own column names, which is what
#' makes a column traceable to the documentation of the engine that produced it. But two of LexNLP's
#' names cannot survive contact with R:
#'
#'   TypeAbbr holds the string "NA" for a National Association -- every national bank in the corpus
#'   -- and R prints that identically to a missing value. A column whose most common value is
#'   indistinguishable from absence in every table it appears in is a defect waiting for a reader.
#'
#'   Name means the resolved company for ORG and the resolved place for GPE. One column called Name
#'   meaning two different things is the kind of thing that is obvious while writing and invisible
#'   six weeks later.
#'
#' So the rename happens here, in one function, next to the schema it serves -- rather than in the
#' ingest, where it would have made the store disagree with LexNLP's own documentation.
.ent_rename <- list(
  lexnlp = c(
    NameCore       = "Name",
    LegalForm      = "TypeAbbr",     # LexNLP's company_type_abbr: CORP, INC, LLC -- and NA
    LegalFormFull  = "TypeFull",
    LegalFormLabel = "TypeLabel",
    GeoName        = "NameEn",
    GeoAlias       = "Alias",
    GeoCategory    = "EntityCategory",
    DateScore      = "Score"
  ),
  matcon = c(
    # Iso2 is a SUBDIVISION code here and never a country; Iso3 is a country code and carries USA on
    # every US entity, so neither name means what the identical name means in the LexNLP store.
    SubIso      = "Iso2",
    CountryIso3 = "Iso3"
  )
)

#' What each family emits for one entity, under this project's names
#'
#' DECLARED IN ONE PLACE BECAUSE THE FAMILIES NO LONGER SHARE A SCHEMA. Under the flat store every
#' GPE row carried the union of both producers' columns and a caller named one extras list for both.
#' One database per family ended that: matcon's gpe table has GeoKey, IsWord, NParent and MatchKind
#' and no GeoName; LexNLP's has GeoName, GeoAlias and GeoCategory and none of the others. A single
#' list would have asked each family for the other's columns.
#'
#' 04A's numbers say why the two are worth keeping apart rather than reconciling here: they agree on
#' only 44.5% of GPE mentions while agreeing on boundaries 97.6% of the time where they meet, which
#' is complements at different tiers rather than rivals at one.
.ent_extras <- list(
  lexnlp = list(
    ORG   = c("NameCore", "LegalForm", "Description"),
    GPE   = c("GeoName", "GeoAlias", "GeoCategory", "Iso2", "Iso3"),
    DATE  = c("DateValue", "DateScore"),
    MONEY = c("Amount", "Currency")
  ),
  matcon = list(
    # THE TWO ISO COLUMNS ARE RENAMED because matcon and LexNLP use the same two names for four
    # different quantities. matcon's own docstring is explicit: Iso2 resolves "all 50 states, 81% of
    # populated places, 95% of counties, and NEVER a country", while Iso3 is "every country, and USA
    # for every US entity". LexNLP's Iso2 is a country code OR a subdivision code and its Iso3 is a
    # country code on countries alone.
    #
    # Mapping both families onto Iso2/Iso3 was a defect that rendered clean: geo_country() reads Iso3
    # first, matcon's Iso3 happens to be a country code, and the answer came out right by luck while
    # the ISO-3166-2 prefix recovery sat dead at 0.0% of matcon's rows. A shared column NAME is not a
    # shared QUANTITY, and the only place that can be settled is the read.
    GPE    = c("GeoKey", "IsWord", "NParent", "SubIso", "CountryIso3", "MatchKind"),
    DATE   = c("DateValue"),
    TERM   = c("TermN", "TermUnit", "TermYears"),
    MONEY  = c("Amount", "Currency"),
    REDACT = character()
  ),
  spacy = list(
    ORG    = character(),
    PERSON = character(),
    GPE    = character()
  )
)

#' The extras one family emits for one entity
#'
#' @param .family Family name.
#' @param .entity Entity name.
#' @return Character vector, possibly empty.
ent_extras <- function(.family, .entity) {
  if (FALSE) {
    .family <- "matcon"
    .entity <- "GPE"
  }
  fam_ <- .ent_extras[[.family]]
  if (is.null(fam_)) cli::cli_abort("No extras declared for family {(.family)}.")
  out_ <- fam_[[toupper(.entity)]]
  if (is.null(out_)) {
    cli::cli_abort(c(
      "{(.family)} declares no {(.entity)} extras.",
      "i" = "It declares: {paste(names(fam_), collapse = ', ')}."
    ))
  }
  out_
}

#' Load one entity table out of one family's store
#'
#' ONE DATABASE PER FAMILY, so the family is the FILENAME and there is no engine token to type. The
#' previous design put all families in one file and asked callers for a combination string --
#' "paper:gazetteer-v1" -- which every 04B document duly typed and which every one of them got wrong
#' the moment a model version moved. The query then matched nothing, returned an empty tibble, and
#' the document rendered reporting zero spans as a success. Here a wrong family is a missing file,
#' and paths fail loudly.
#'
#' SENTINELS ARE NO LONGER IN THE ENTITY TABLES. A document processed with nothing found is recorded
#' in the ledger, not as a null-offset row, so every row in an entity table is a real span and the
#' `WHERE Start IS NOT NULL` that used to be necessary is now a formality kept for safety.
#'
#' LabelRaw IS ALWAYS SELECTED. It carries what the engine itself called the span before anything
#' normalised it -- the gazetteer's match kind, the redaction extractor's marker class -- and a rule
#' that needs it has no other way to reach it.
#'
#' @param .dir_store Directory holding the family databases, e.g. 04A's Output/.
#' @param .family Character: "lexnlp", "matcon" or "spacy".
#' @param .entity Character: "ORG", "GPE", "DATE", "TERM", "MONEY", "REDACT", "PERSON".
#' @param .lens Tibble from ent_doc_lens(). Supplies DocLen, so an offset becomes a position.
#' @param .extras Character vector of family-specific columns, named as this file renames them.
#' @param .model Character. Restrict to one model, for the spaCy store where models compete. NULL
#'   takes every row, which is correct for the families that version themselves.
#' @param .quiet Logical. Suppress the count message.
#' @return Tibble: one row per span, with DocLen joined on.
ent_load_entity <- function(.dir_store, .family, .entity, .lens, .extras = character(),
                           .model = NULL, .quiet = FALSE) {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .family    <- "lexnlp"
    .entity    <- "ORG"
    .lens      <- tab_lens
    .extras    <- c("NameCore", "LegalForm", "Description")
    .model     <- NULL
    .quiet     <- FALSE
  }

  path_ <- ner_db_path(.dir = .dir_store, .family = .family)
  if (!fs::file_exists(path_)) {
    cli::cli_abort(c(
      "No {(.family)} store at {.path {(path_)}}.",
      "i" = "04A writes the sample stores and 04C the corpus stores, one file per family."
    ))
  }

  con_ <- ner_db_connect(.db_path = path_, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  tbl_ <- tolower(.entity)
  if (!tbl_ %in% ner_db_tables(.con = con_)) {
    cli::cli_abort(c(
      "The {(.family)} store holds no {(.entity)} table.",
      "i" = "It holds: {paste(toupper(ner_db_tables(.con = con_)), collapse = ', ')}."
    ))
  }

  # THE SCHEMA IS READ, NOT ASSUMED. Which extras a table carries is a property of the parquet the
  # ingest created it from, and asking for a column that is not there should say so rather than
  # arrive as a silently absent name -- which is what any_of() would do, and what turned a documented
  # rule into a fallback nobody noticed.
  have_ <- DBI::dbListFields(con_, tbl_)
  ren_  <- .ent_rename[[.family]]
  want_ <- if (is.null(ren_)) .extras else dplyr::coalesce(unname(ren_[.extras]), .extras)
  bad_  <- .extras[!want_ %in% have_]
  if (length(bad_) > 0L) {
    cli::cli_abort(c(
      "{(.family)}/{(.entity)} carries no {paste(bad_, collapse = ', ')}.",
      "i" = "Columns present: {paste(have_, collapse = ', ')}."
    ))
  }

  sel_   <- c("DocID", "Start", "Stop", "Span", "LabelRaw", want_)
  scope_ <- if (is.null(.model) || !"Model" %in% have_) {
    ""
  } else {
    glue::glue(" AND Model = '{.model}'")
  }

  raw_ <- DBI::dbGetQuery(con_, glue::glue(
    "SELECT {paste(sel_, collapse = ', ')} FROM {tbl_}
      WHERE Start IS NOT NULL{scope_}"
  )) |>
    tibble::as_tibble()

  # Back to this project's vocabulary, and only for the columns the caller asked for by that name.
  if (length(want_) > 0L) {
    names(raw_)[match(want_, names(raw_))] <- .extras
  }

  out_ <- raw_ |>
    dplyr::inner_join(.lens, by = dplyr::join_by(DocID)) |>
    dplyr::arrange(.data$DocID, .data$Start)

  if (!.quiet) {
    n_    <- nrow(out_)
    ndoc_ <- dplyr::n_distinct(out_$DocID)
    cli::cli_alert_success(
      "{(.family)} / {(.entity)}: {format(n_, big.mark = ',')} \\
       {cli::qty(n_)}span{?s} over {format(ndoc_, big.mark = ',')} \\
       {cli::qty(ndoc_)}document{?s}."
    )
    lost_ <- nrow(raw_) - n_
    if (lost_ > 0L) {
      cli::cli_alert_warning(
        "{format(lost_, big.mark = ',')} {cli::qty(lost_)}span{?s} dropped: the store holds \\
         {?a document/documents} the canonical text does not."
      )
    }
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
