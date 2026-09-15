# 05A1-Dictionary.R: the T1 dictionary, as agreed in 05-Dictionary-Glossary-and-Algorithm.md ------------------------
#
# Steps (each cached to parquet through step_run):
#   1 extract      every face line as printed                              -> FaceItems
#   2 classify     custom / resolved / retired / newer                      -> TagClass
#   3 universe     one row per concept filers printed                       -> Concepts
#   4 ladder       every caption at every rung, L0..L5                      -> Ladder
#   5 observations one term per (filing, line item) at the working rung     -> Observations
#   6 dictionary   (line item, term) with counts and shares                 -> Dict, DictRole
#   7 reverse      (statement, term) -> concepts, star, pStar               -> TermIndex, TermIndexRole
#   8 groups       proposed line-item groups from term overlap              -> GroupProposals
#
# TWO PURPOSES. A: describe the tagged tier -- everything kept. B: look up untagged reports -- pruned
# and disambiguated in 05B. Steps 1-7 serve A (and feed B); step 8 serves B only.
#
# TWO DICTIONARIES. Gaap is a key of every table from Concepts on. Nothing is pooled across US and IFRS.
#
# THE UNIT IS THE LINE ITEM: (Gaap, CID, Statement). An observation is one filing printing one line
# item, once; if it printed it under several captions, step 5 keeps one.
#
# Prefix dct_. DuckDB through dplyr verbs and the Commons helpers; raw SQL only inside dplyr::sql()
# for two regexps. Style: native pipe, explicit namespace, dot-prefixed arguments, underscore-suffixed
# locals, ASCII.


# 1. Reference -------------------------------------------------------------------------------------------------------

.dct_rungs <- c("Raw", "L0", "L1", "L2", "L3", "L4", "L5")
.dct_rung_rules <- c(
  Raw = "as printed",
  L0  = "whitespace squished, lower-cased",
  L1  = "+ note references and trailing punctuation removed",
  L2  = "+ parentheticals and all punctuation removed",
  L3  = "+ digits removed",
  L4  = "+ lemmatised (lexicon lookup)",
  L5  = "+ qualifiers removed (period phrases, leading aggregation words, roll-forward phrases)"
)

.dct_statements <- c("BS", "IS", "CF", "CI", "EQ")

.dct_note_rx <- paste0(
  "\\(\\s*(see\\s+)?notes?\\s+[0-9a-z]+(\\s*(,|and|&)\\s*[0-9a-z]+)*\\s*\\)",
  "|\\bsee\\s+notes?\\s+[0-9a-z]+\\b",
  "|\\(\\s*notes?\\s*\\)"
)

# L5: what a label role adds to a caption without changing what the line is. Applied to L4 terms, in
# order. Conservative on purpose: "net" and "gross" are never touched -- "net sales" versus "sales" is
# a difference the paper is about. Extend by reading dct_report_l5().
.dct_qualifier_rx <- c(
  # roll-forward captions: "balance at beginning of year", "balances at end of period", "beginning balance"
  paste0("^balances? (at|as of|as at) (the )?(beginning|begin|end|ending|start|close|opening|open|closing)",
         "( of)?( the)?( fiscal)? ?(year|period|quarter)?\\b"),
  "^(beginning|begin|ending|end|opening|open|closing|close) balances?\\b",
  # period phrases anywhere: "at beginning of year", "end of period", "as of end of the fiscal year".
  # Lemma forms (begin, open) are accepted as well as the protected originals.
  paste0("\\b(at|as of|as at)?\\s*(the )?(beginning|begin|end|ending|start|close|closing|opening|open) of ",
         "(the )?(fiscal )?(year|period|quarter)\\b"),
  "\\b(beginning|begin|ending|opening|closing)$",
  # leading aggregation words a totalLabel / negatedLabel adds
  "^(total|subtotal|less|plus|add|deduct|minus)\\b",
  # trailing period markers
  "\\b(for the (fiscal )?(year|period)( then)? ended)$",
  "\\b(during the (fiscal )?(year|period))$"
)

# Words the lemmatiser must leave alone. textstem's lexicon maps "less" to "little" and "beginning" to
# "begin"; the first is wrong, the second breaks the L5 period rules. Function words and the two
# words the paper is about are protected too.
.dct_protect <- c("less", "net", "gross", "non", "per", "plus", "other", "beginning", "ending", "total",
                  "current", "long", "short", "term")

# Role order for the tiebreak in step 5, after "shortest term". Anything not listed ranks last.
.dct_role_rank <- c(label = 1L, terseLabel = 2L, verboseLabel = 3L, totalLabel = 4L)

#' Order a Rung column by the ladder
#' @param .tab A tibble with a Rung column.
#' @param ... Further arrange() terms after Rung.
#' @return .tab, arranged, Rung as character.
dct_arrange_rung <- function(.tab, ...) {
  .tab |>
    dplyr::mutate(Rung = factor(.data$Rung, levels = .dct_rungs)) |>
    dplyr::arrange(.data$Rung, ...) |>
    dplyr::mutate(Rung = as.character(.data$Rung))
}

#' Let a CamelCase name wrap in a rendered table
#'
#' Inserts a zero-width space at every lower-to-upper boundary, so the browser can break
#' "OtherComprehensiveIncomeLossNetOfTax" across lines. Only in kable mode: console output stays as it
#' is, and the character is produced by an escape so the source file stays ASCII.
#'
#' @param .x Character vector.
#' @return Character vector.
dct_wrap <- function(.x) {
  if (FALSE) .x <- "OtherComprehensiveIncomeLossNetOfTax"
  if (!identical(tbl_mode(), "kable")) return(.x)
  stringi::stri_replace_all_regex(.x, "(?<=[a-z0-9])(?=[A-Z])", "\u200b")
}

#' Read a parquet this stage wrote, lazily
#' @param .lP The configuration list.
#' @param .name Character. Output name.
#' @return An Arrow dataset.
dct_read <- function(.lP, .name) {
  if (FALSE) { .lP <- init_config(.script = "05A1-Dictionary"); .name <- "Dict" }
  arrow::open_dataset(sources = .lP$Output[[.name]], format = "parquet")
}

#' The characterisation floor per dictionary: observations a line item needs to be described
#'
#' A share of the dictionary's filings, so that the same intent -- "not too thin to characterise" --
#' gives about 100 filings for US GAAP and a handful for IFRS.
#'
#' @param .lP The configuration list.
#' @return A tibble: Gaap, Filings, MinObs.
dct_floor <- function(.lP) {
  if (FALSE) .lP <- init_config(.script = "05A1-Dictionary")
  dct_read(.lP = .lP, .name = "Observations") |>
    dplyr::distinct(.data$Gaap, .data$Adsh) |>
    dplyr::count(.data$Gaap, name = "Filings") |>
    dplyr::collect() |>
    dplyr::mutate(Filings = as.integer(.data$Filings),
                  MinObs = pmax(1L, as.integer(ceiling(.data$Filings * .lP$Params$MinShareFilings))))
}


# 2. Step 1: extract -------------------------------------------------------------------------------------------------

#' Every face line item as printed
#'
#' In-scope filings, the five face statements, primary report, no parentheticals, item rows valued or
#' not. Custom tags are in at this step so that step 2 can count them. Vintage is the first four-digit
#' run after the namespace; Gaap is read off the concept prefix, NA for custom tags.
#'
#' @param .lP The configuration list.
#' @param .force Logical.
#' @return The parquet path.
dct_extract <- function(.lP, .force = FALSE) {
  if (FALSE) { .lP <- init_config(.script = "05A1-Dictionary"); .force <- FALSE }
  step_run(
    .name   = "dct_extract",
    .writes = c(FaceItems = .lP$Output$FaceItems),
    .reads  = c(Captions = .lP$Input$Captions, Filings = .lP$Input$Filings),
    .params = list(Statements = .dct_statements),
    .lP     = .lP,
    .force  = .force,
    .expr   = {
      con_ <- duck_connect(.lP = .lP)
      on.exit(duck_disconnect(.con = con_), add = TRUE)
      filings_ <- duck_parquet(.con = con_, .path = .lP$Input$Filings) |>
        dplyr::filter(.data$InScope) |>
        dplyr::select("Adsh", "CIK", "FY")
      items_ <- duck_parquet(.con = con_, .path = fs::path(.lP$Input$Captions, "*.parquet")) |>
        dplyr::filter(.data$IsFace == 1L, .data$IsPrimaryReport == 1L, .data$Inpth == 0L,
                      .data$RowType %in% c("Item", "Unvalued"), .data$Statement %in% .dct_statements) |>
        dplyr::inner_join(filings_, by = "Adsh") |>
        dplyr::transmute(
          .data$Adsh, .data$CIK, .data$FY, .data$Statement, .data$Report, .data$Line,
          .data$Tag, .data$Version, .data$CID,
          IsCustom    = .data$IsExtension == 1L,
          Gaap        = dplyr::case_when(
            .data$IsExtension == 1L                         ~ NA_character_,
            dplyr::sql("regexp_matches(CID, '^ifrs')")      ~ "IFRS",
            TRUE                                            ~ "US"
          ),
          VintageYear = dplyr::if_else(.data$IsExtension == 1L, NA_integer_,
                                       as.integer(dplyr::sql("regexp_extract(Version, '/([0-9]{4})', 1)"))),
          .data$PRole, .data$Caption, IsValued = .data$RowType == "Item"
        )
      n_ <- duck_write_parquet(.con = con_, .query = items_, .path = .lP$Output$FaceItems)
      cli::cli_alert_success("Extracted {format(n_, big.mark = ',')} face line items.")
    }
  )
}


# 3. Step 2: classify ------------------------------------------------------------------------------------------------

#' Custom, resolved, retired, newer: every (tag, version) with its bucket and its weight; cached
#' @param .lP The configuration list.
#' @param .force Logical.
#' @return The parquet path.
dct_classify <- function(.lP, .force = FALSE) {
  if (FALSE) { .lP <- init_config(.script = "05A1-Dictionary"); .force <- FALSE }
  step_run(
    .name   = "dct_classify",
    .writes = c(TagClass = .lP$Output$TagClass),
    .reads  = c(FaceItems = .lP$Output$FaceItems, Concepts = .lP$Input$Concepts),
    .params = list(TaxonomyVintage = .lP$Params$TaxonomyVintage),
    .lP     = .lP,
    .force  = .force,
    .expr   = {
      con_ <- duck_connect(.lP = .lP)
      on.exit(duck_disconnect(.con = con_), add = TRUE)
      tax_ <- duck_parquet(.con = con_, .path = .lP$Input$Concepts) |>
        dplyr::distinct(.data$CID) |>
        dplyr::mutate(InTaxonomy = TRUE)
      cls_ <- duck_parquet(.con = con_, .path = .lP$Output$FaceItems) |>
        dplyr::left_join(tax_, by = "CID") |>
        dplyr::group_by(.data$Gaap, .data$Tag, .data$Version, .data$VintageYear, .data$CID, .data$IsCustom,
                        .data$InTaxonomy) |>
        dplyr::summarise(nLines = dplyr::n(), nFilings = dplyr::n_distinct(.data$Adsh), .groups = "drop") |>
        dplyr::collect() |>
        dplyr::mutate(
          dplyr::across(c("nLines", "nFilings"), as.integer),
          Bucket = dplyr::case_when(
            .data$IsCustom                                  ~ "custom",
            dplyr::coalesce(.data$InTaxonomy, FALSE)        ~ "standard, resolved",
            is.na(.data$VintageYear)                        ~ "standard, unknown vintage",
            .data$VintageYear > .lP$Params$TaxonomyVintage  ~ "standard, newer",
            TRUE                                            ~ "standard, retired"
          ),
          IsResolved = .data$Bucket == "standard, resolved"
        ) |>
        dplyr::select("Gaap", "Tag", "Version", "VintageYear", "CID", "Bucket", "IsResolved", "nLines", "nFilings") |>
        dplyr::arrange(dplyr::desc(.data$nFilings))
      utils_write_parquet(.tab = cls_, .path = .lP$Output$TagClass)
      cli::cli_alert_success("Classified {format(nrow(cls_), big.mark = ',')} (tag, version) pairs.")
    }
  )
}


# 4. Step 3: the universe --------------------------------------------------------------------------------------------

#' One row per standard concept filers printed on a face statement; cached
#' @param .lP The configuration list.
#' @param .force Logical.
#' @return The parquet path.
dct_concepts <- function(.lP, .force = FALSE) {
  if (FALSE) { .lP <- init_config(.script = "05A1-Dictionary"); .force <- FALSE }
  step_run(
    .name   = "dct_concepts",
    .writes = c(Concepts = .lP$Output$Concepts),
    .reads  = c(FaceItems = .lP$Output$FaceItems, TagClass = .lP$Output$TagClass,
                Catalog = .lP$Input$Catalog, Taxonomy = .lP$Input$Concepts),
    .lP     = .lP,
    .force  = .force,
    .expr   = {
      con_ <- duck_connect(.lP = .lP)
      on.exit(duck_disconnect(.con = con_), add = TRUE)
      items_ <- duck_parquet(.con = con_, .path = .lP$Output$FaceItems) |> dplyr::filter(!.data$IsCustom)

      use_ <- items_ |>
        dplyr::group_by(.data$Gaap, .data$CID) |>
        dplyr::summarise(
          nLines = dplyr::n(), nObs = dplyr::n_distinct(.data$Adsh), nFirms = dplyr::n_distinct(.data$CIK),
          FirstFY = min(.data$FY, na.rm = TRUE), LastFY = max(.data$FY, na.rm = TRUE),
          FirstVintage = min(.data$VintageYear, na.rm = TRUE), LastVintage = max(.data$VintageYear, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(dplyr::across(c("nLines", "nObs", "nFirms", "FirstFY", "LastFY", "FirstVintage", "LastVintage"),
                                    as.integer))

      stm_ <- items_ |>
        dplyr::distinct(.data$Gaap, .data$CID, .data$Statement, .data$Adsh) |>
        dplyr::count(.data$Gaap, .data$CID, .data$Statement, name = "n") |>
        dplyr::collect() |>
        dplyr::mutate(n = as.integer(.data$n)) |>
        dplyr::mutate(Share = .data$n / sum(.data$n), .by = c("Gaap", "CID"))
      home_ <- stm_ |>
        dplyr::slice_max(.data$Share, n = 1L, by = c("Gaap", "CID"), with_ties = FALSE) |>
        dplyr::transmute(.data$Gaap, .data$CID, HomeStatement = .data$Statement, HomeShare = round(.data$Share, 4))
      spread_ <- stm_ |>
        dplyr::arrange(.data$Gaap, .data$CID, dplyr::desc(.data$Share)) |>
        dplyr::summarise(nStatements = dplyr::n(),
                         Statements = paste0(.data$Statement, " ", round(100 * .data$Share), "%", collapse = " | "),
                         .by = c("Gaap", "CID"))

      cids_ <- items_ |> dplyr::distinct(.data$CID)
      lab_ <- duck_parquet(.con = con_, .path = .lP$Input$Catalog) |>
        dplyr::filter(!.data$IsCustom, !is.na(.data$TLabel), .data$TLabel != "") |>
        dplyr::semi_join(cids_, by = "CID") |>
        dplyr::select("CID", "Version", "TLabel") |>
        dplyr::collect() |>
        dplyr::mutate(VYear = as.integer(stringi::stri_extract_first_regex(.data$Version, "(?<=/)[0-9]{4}"))) |>
        dplyr::arrange(.data$CID, dplyr::desc(.data$VYear)) |>
        dplyr::distinct(.data$CID, .keep_all = TRUE) |>
        dplyr::transmute(.data$CID, FsdsLabel = .data$TLabel, FsdsLabelVintage = .data$VYear)

      resolved_ <- arrow::read_parquet(.lP$Output$TagClass) |>
        dplyr::filter(.data$Bucket != "custom") |>
        dplyr::summarise(
          IsResolved = any(.data$IsResolved),
          ResolveStatus = dplyr::case_when(
            any(.data$IsResolved)                      ~ "resolved",
            any(.data$Bucket == "standard, newer")     ~ "newer",
            any(.data$Bucket == "standard, retired")   ~ "retired",
            TRUE                                       ~ "unknown vintage"
          ),
          .by = c("Gaap", "CID")
        )
      tax_ <- arrow::read_parquet(.lP$Input$Concepts) |>
        dplyr::select("CID", "StandardLabel", "ConceptKind", "IsDeprecated", "nStatementRoles")

      cpt_ <- use_ |>
        dplyr::left_join(resolved_, by = c("Gaap", "CID")) |>
        dplyr::left_join(home_, by = c("Gaap", "CID")) |>
        dplyr::left_join(spread_, by = c("Gaap", "CID")) |>
        dplyr::left_join(lab_, by = "CID") |>
        dplyr::left_join(tax_, by = "CID") |>
        dplyr::relocate("IsResolved", "ResolveStatus", .after = "CID") |>
        dplyr::arrange(.data$Gaap, dplyr::desc(.data$nObs))
      utils_write_parquet(.tab = cpt_, .path = .lP$Output$Concepts)
      cli::cli_alert_success("{format(nrow(cpt_), big.mark = ',')} concepts, \\
                              {format(sum(!cpt_$IsResolved), big.mark = ',')} not in the reference taxonomy.")
    }
  )
}


# 5. Step 4: the ladder ----------------------------------------------------------------------------------------------

#' Every distinct caption at every rung, L0 to L5
#' @param .captions Character vector of distinct printed captions.
#' @param .qualifier_rx Character vector of regexes applied in order at L5.
#' @return A tibble: Caption, Rung, Term.
dct_ladder_build <- function(.captions, .qualifier_rx = .dct_qualifier_rx) {
  if (FALSE) { .captions <- c("Cash and cash equivalents, end of period (Note 3)"); .qualifier_rx <- .dct_qualifier_rx }
  squish_ <- function(.x) .x |> stringi::stri_replace_all_regex("\\s+", " ") |> stringi::stri_trim_both()
  l0_ <- .captions |> stringi::stri_trans_tolower() |> squish_()
  l1_ <- l0_ |>
    stringi::stri_replace_all_regex(.dct_note_rx, " ") |>
    stringi::stri_replace_all_regex("[\\s:;,.\\-]+$", "") |>
    squish_()
  l2_ <- l1_ |>
    stringi::stri_replace_all_regex("\\([^)]*\\)", " ") |>
    stringi::stri_replace_all_regex("[^a-z0-9 ]+", " ") |>
    squish_()
  l3_ <- l2_ |> stringi::stri_replace_all_regex("[0-9]+", " ") |> squish_()
  u3_ <- unique(l3_)
  l4_ <- dct_lemmatize(u3_)[match(l3_, u3_)] |> squish_()
  u4_ <- unique(l4_)
  l5u_ <- u4_
  for (rx_ in .qualifier_rx) l5u_ <- stringi::stri_replace_all_regex(l5u_, rx_, " ")
  l5u_ <- squish_(l5u_)
  # A qualifier-only caption ("Total", "Balance at end of year") would become empty; keep L4 for those.
  l5u_ <- dplyr::if_else(nzchar(l5u_), l5u_, u4_)
  l5_ <- l5u_[match(l4_, u4_)]

  tibble::tibble(Caption = .captions, Raw = .captions, L0 = l0_, L1 = l1_, L2 = l2_, L3 = l3_, L4 = l4_, L5 = l5_) |>
    tidyr::pivot_longer(cols = dplyr::all_of(.dct_rungs), names_to = "Rung", values_to = "Term") |>
    dplyr::filter(nzchar(.data$Term))
}

#' Lemmatise word by word, leaving protected words alone
#'
#' Splits the unique strings into words, lemmatises the unique words once (fast), maps back. Words in
#' .dct_protect are returned as they are.
#'
#' @param .x Character vector of L3 terms.
#' @return Character vector of the same length.
dct_lemmatize <- function(.x) {
  if (FALSE) .x <- c("less accumulated depreciation", "cash at beginning of year")
  words_ <- stringi::stri_split_fixed(.x, " ")
  flat_  <- unlist(words_, use.names = FALSE)
  uw_    <- unique(flat_)
  lem_   <- textstem::lemmatize_words(uw_)
  keep_  <- uw_ %in% .dct_protect
  lem_[keep_] <- uw_[keep_]
  # Map every word once, then re-join by string without an R-level loop.
  mapped_ <- lem_[match(flat_, uw_)]
  stringi::stri_join_list(utils::relist(mapped_, words_), sep = " ")
}

#' The ladder over every distinct standard-tag caption; cached
#' @param .lP The configuration list.
#' @param .force Logical.
#' @return The parquet path.
dct_ladder <- function(.lP, .force = FALSE) {
  if (FALSE) { .lP <- init_config(.script = "05A1-Dictionary"); .force <- FALSE }
  step_run(
    .name   = "dct_ladder",
    .writes = c(Ladder = .lP$Output$Ladder),
    .reads  = c(FaceItems = .lP$Output$FaceItems),
    .params = list(Rungs = .dct_rungs, NoteRx = .dct_note_rx, QualifierRx = .dct_qualifier_rx,
                   Protect = .dct_protect),
    .fns    = list(dct_ladder_build = dct_ladder_build, dct_lemmatize = dct_lemmatize),
    .lP     = .lP,
    .force  = .force,
    .expr   = {
      captions_ <- dct_read(.lP = .lP, .name = "FaceItems") |>
        dplyr::filter(!.data$IsCustom) |>
        dplyr::distinct(.data$Caption) |>
        dplyr::collect() |>
        dplyr::pull("Caption")
      lad_ <- dct_ladder_build(.captions = captions_, .qualifier_rx = .dct_qualifier_rx)
      utils_write_parquet(.tab = lad_, .path = .lP$Output$Ladder)
      cli::cli_alert_success("Ladder over {format(length(captions_), big.mark = ',')} distinct captions.")
    }
  )
}


# 6. Step 5: one term per observation --------------------------------------------------------------------------------

#' One term per (filing, line item) at the working rung; cached
#'
#' A filing that printed a line item under several captions contributes one observation. The term kept
#' is the shortest at the working rung; on a tie, the role in .dct_role_rank order; then the term
#' alphabetically, so the choice is deterministic. IsTie records that more than one distinct term was
#' available; nCaptions how many face lines the line item had in the filing.
#'
#' @param .lP The configuration list.
#' @param .force Logical.
#' @return The parquet path.
dct_observations <- function(.lP, .force = FALSE) {
  if (FALSE) { .lP <- init_config(.script = "05A1-Dictionary"); .force <- FALSE }
  step_run(
    .name   = "dct_observations",
    .writes = c(Observations = .lP$Output$Observations),
    .reads  = c(FaceItems = .lP$Output$FaceItems, Ladder = .lP$Output$Ladder),
    .params = list(Rung = .lP$Params$Rung, RoleRank = .dct_role_rank),
    .lP     = .lP,
    .force  = .force,
    .expr   = {
      con_ <- duck_connect(.lP = .lP)
      on.exit(duck_disconnect(.con = con_), add = TRUE)
      rank_ <- tibble::tibble(PRole = names(.dct_role_rank), RoleRank = unname(.dct_role_rank))
      duck_register(.con = con_, .name = "rolerank", .tab = rank_)
      lad_ <- duck_parquet(.con = con_, .path = .lP$Output$Ladder) |>
        dplyr::filter(.data$Rung == !!.lP$Params$Rung) |>
        dplyr::select("Caption", "Term")
      lines_ <- duck_parquet(.con = con_, .path = .lP$Output$FaceItems) |>
        dplyr::filter(!.data$IsCustom) |>
        dplyr::inner_join(lad_, by = "Caption") |>
        dplyr::left_join(dplyr::tbl(con_, "rolerank"), by = "PRole") |>
        dplyr::mutate(RoleRank = dplyr::coalesce(.data$RoleRank, 99L), TermLen = nchar(.data$Term))
      key_ <- c("Adsh", "Gaap", "CID", "Statement")
      agg_ <- lines_ |>
        dplyr::group_by(dplyr::across(dplyr::all_of(key_))) |>
        dplyr::summarise(nCaptions = dplyr::n(), nTerms = dplyr::n_distinct(.data$Term), .groups = "drop")
      pick_ <- lines_ |>
        dplyr::group_by(dplyr::across(dplyr::all_of(key_))) |>
        dbplyr::window_order(.data$TermLen, .data$RoleRank, .data$Term) |>
        dplyr::mutate(Rn = dplyr::row_number()) |>
        dplyr::ungroup() |>
        dplyr::filter(.data$Rn == 1L) |>
        dplyr::select(dplyr::all_of(key_), "CIK", "FY", "Term", "PRole", "Caption")
      obs_ <- pick_ |>
        dplyr::inner_join(agg_, by = key_) |>
        dplyr::mutate(IsTie = .data$nTerms > 1L) |>
        dplyr::select("Adsh", "CIK", "FY", "Gaap", "CID", "Statement", "Term", "PRole", "Caption", "nCaptions", "nTerms",
                      "IsTie")
      n_ <- duck_write_parquet(.con = con_, .query = obs_, .path = .lP$Output$Observations)
      cli::cli_alert_success("{format(n_, big.mark = ',')} observations at {.val {(.lP)$Params$Rung}}.")
    }
  )
}


# 7. Step 6: the dictionary ------------------------------------------------------------------------------------------

#' The dictionary at the working rung: (line item, term) with counts and shares; cached
#' @param .lP The configuration list.
#' @param .force Logical.
#' @return The two parquet paths.
dct_dictionary <- function(.lP, .force = FALSE) {
  if (FALSE) { .lP <- init_config(.script = "05A1-Dictionary"); .force <- FALSE }
  step_run(
    .name   = "dct_dictionary",
    .writes = c(Dict = .lP$Output$Dict, DictRole = .lP$Output$DictRole),
    .reads  = c(Observations = .lP$Output$Observations, TagClass = .lP$Output$TagClass),
    .lP     = .lP,
    .force  = .force,
    .expr   = {
      con_ <- duck_connect(.lP = .lP)
      on.exit(duck_disconnect(.con = con_), add = TRUE)
      obs_ <- duck_parquet(.con = con_, .path = .lP$Output$Observations)
      resolved_ <- duck_parquet(.con = con_, .path = .lP$Output$TagClass) |>
        dplyr::filter(.data$Bucket != "custom") |>
        dplyr::group_by(.data$Gaap, .data$CID) |>
        dplyr::summarise(IsResolved = max(as.integer(.data$IsResolved)) == 1L, .groups = "drop")
      item_ <- c("Gaap", "CID", "Statement")

      build_ <- function(.keys) {
        obs_ |>
          dplyr::group_by(dplyr::across(dplyr::all_of(.keys))) |>
          dplyr::summarise(nObs = dplyr::n(), nFirms = dplyr::n_distinct(.data$CIK),
                           nLines = sum(.data$nCaptions, na.rm = TRUE), .groups = "drop") |>
          dplyr::group_by(dplyr::across(dplyr::all_of(item_))) |>
          dplyr::mutate(Share = .data$nObs / sum(.data$nObs, na.rm = TRUE)) |>
          dplyr::ungroup() |>
          dplyr::left_join(resolved_, by = c("Gaap", "CID"))
      }
      role_ <- build_(c(item_, "Term", "PRole")) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(item_, "PRole")))) |>
        dplyr::mutate(ShareInRole = .data$nObs / sum(.data$nObs, na.rm = TRUE)) |>
        dplyr::ungroup()
      n1_ <- duck_write_parquet(.con = con_, .query = role_, .path = .lP$Output$DictRole)
      n2_ <- duck_write_parquet(.con = con_, .query = build_(c(item_, "Term")), .path = .lP$Output$Dict)
      cli::cli_alert_success("Dict {format(n2_, big.mark = ',')} rows; DictRole {format(n1_, big.mark = ',')} rows.")
    }
  )
}


# 8. Step 7: the reverse index -----------------------------------------------------------------------------------------

#' For each term on a statement, which concepts it names; cached, in DuckDB
#' @param .lP The configuration list.
#' @param .force Logical.
#' @return The two parquet paths.
dct_reverse <- function(.lP, .force = FALSE) {
  if (FALSE) { .lP <- init_config(.script = "05A1-Dictionary"); .force <- FALSE }
  step_run(
    .name   = "dct_reverse",
    .writes = c(TermIndex = .lP$Output$TermIndex, TermIndexRole = .lP$Output$TermIndexRole),
    .reads  = c(Dict = .lP$Output$Dict, DictRole = .lP$Output$DictRole),
    .lP     = .lP,
    .force  = .force,
    .expr   = {
      con_ <- duck_connect(.lP = .lP)
      on.exit(duck_disconnect(.con = con_), add = TRUE)
      one_ <- function(.path, .keys) {
        d_ <- duck_parquet(.con = con_, .path = .path)
        agg_ <- d_ |>
          dplyr::group_by(dplyr::across(dplyr::all_of(.keys))) |>
          dplyr::summarise(nConcepts = dplyr::n(), nUses = sum(.data$nObs, na.rm = TRUE),
                           nUsesStar = max(.data$nObs, na.rm = TRUE), .groups = "drop")
        star_ <- d_ |>
          dplyr::select(dplyr::all_of(c(.keys, "CID", "nObs"))) |>
          dplyr::inner_join(agg_ |> dplyr::select(dplyr::all_of(.keys), "nUsesStar"),
                            by = c(.keys, "nObs" = "nUsesStar")) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(.keys))) |>
          dplyr::summarise(CIDStar = min(.data$CID, na.rm = TRUE), .groups = "drop")
        agg_ |>
          dplyr::inner_join(star_, by = .keys) |>
          dplyr::mutate(pStar = .data$nUsesStar / .data$nUses)
      }
      n1_ <- duck_write_parquet(.con = con_, .query = one_(.lP$Output$Dict, c("Gaap", "Statement", "Term")),
                                .path = .lP$Output$TermIndex)
      n2_ <- duck_write_parquet(.con = con_, .query = one_(.lP$Output$DictRole, c("Gaap", "Statement", "Term", "PRole")),
                                .path = .lP$Output$TermIndexRole)
      cli::cli_alert_success("TermIndex {format(n1_, big.mark = ',')} rows; TermIndexRole {format(n2_, big.mark = ',')}.")
    }
  )
}


# 9. Step 8: line-item groups ------------------------------------------------------------------------------------------

#' Propose line-item groups from term overlap; cached
#'
#' Within one dictionary and one statement: every pair of line items above the characterisation floor,
#' the cosine of their share vectors over terms (terms with at least MinTermShare of either item, so
#' the tail does not vote), sharing at least MinSharedTerms head terms. Pairs at or above GroupCosine
#' are proposals; connected components at that threshold are proposed groups. Every proposal carries
#' the evidence: the shared terms and their share on each side. Nothing is decided here; the accepted
#' list is a parameter of a later render.
#'
#' @param .lP The configuration list.
#' @param .force Logical.
#' @return The parquet path.
dct_groups <- function(.lP, .force = FALSE) {
  if (FALSE) { .lP <- init_config(.script = "05A1-Dictionary"); .force <- FALSE }
  step_run(
    .name   = "dct_groups",
    .writes = c(GroupProposals = .lP$Output$GroupProposals),
    .reads  = c(Dict = .lP$Output$Dict, Observations = .lP$Output$Observations),
    .params = list(GroupCosine = .lP$Params$GroupCosine, MinTermShare = .lP$Params$MinTermShare,
                   MinSharedTerms = .lP$Params$MinSharedTerms,
                   MinShareFilings = .lP$Params$MinShareFilings),
    .fns    = list(dct_groups_one = dct_groups_one),
    .lP     = .lP,
    .force  = .force,
    .expr   = {
      floor_ <- dct_floor(.lP = .lP)
      dict_ <- dct_read(.lP = .lP, .name = "Dict") |>
        dplyr::select("Gaap", "CID", "Statement", "Term", "nObs", "Share") |>
        dplyr::collect() |>
        dplyr::mutate(ItemObs = sum(.data$nObs), .by = c("Gaap", "CID", "Statement")) |>
        dplyr::inner_join(floor_ |> dplyr::select("Gaap", "MinObs"), by = "Gaap") |>
        dplyr::filter(.data$ItemObs >= .data$MinObs, .data$Share >= .lP$Params$MinTermShare)
      out_ <- dict_ |>
        dplyr::group_split(.data$Gaap, .data$Statement) |>
        purrr::map(\(.d) dct_groups_one(.d, .cos = .lP$Params$GroupCosine,
                                           .min_shared = .lP$Params$MinSharedTerms)) |>
        purrr::list_rbind()
      utils_write_parquet(.tab = out_, .path = .lP$Output$GroupProposals)
      cli::cli_alert_success("{format(nrow(out_), big.mark = ',')} proposed pairs in \\
                              {dplyr::n_distinct(out_$Gaap, out_$Statement, out_$ProposedGroup)} proposed groups.")
    }
  )
}

#' Cosine overlap between every pair of line items in one (dictionary, statement)
#' @param .d Tibble: rows of Dict for one Gaap and Statement, tail removed.
#' @param .cos Numeric. Threshold.
#' @return Tibble of proposed pairs with evidence, or an empty tibble.
dct_groups_one <- function(.d, .cos = 0.5, .min_shared = 2L) {
  if (FALSE) {
    .d <- tibble::tibble(Gaap = "US", Statement = "IS", CID = "a", Term = "x", Share = 1, nObs = 1L); .cos <- 0.5
  }
  if (nrow(.d) == 0L) return(tibble::tibble())
  items_ <- sort(unique(.d$CID)); terms_ <- sort(unique(.d$Term))
  if (length(items_) < 2L) return(tibble::tibble())
  m_ <- Matrix::sparseMatrix(i = match(.d$Term, terms_), j = match(.d$CID, items_), x = .d$Share,
                             dims = c(length(terms_), length(items_)))
  norm_ <- sqrt(Matrix::colSums(m_^2)); norm_[norm_ == 0] <- 1
  mn_ <- m_ %*% Matrix::Diagonal(x = 1 / norm_)
  cs_ <- as.matrix(Matrix::crossprod(mn_))
  idx_ <- which(upper.tri(cs_) & cs_ >= .cos, arr.ind = TRUE)
  if (nrow(idx_) == 0L) return(tibble::tibble())
  pairs_ <- tibble::tibble(CIDa = items_[idx_[, 1L]], CIDb = items_[idx_[, 2L]], Cosine = round(cs_[idx_], 4))
  # A pair must share at least .min_shared head terms: one shared term gives a cosine of 1 for nothing.
  shared_ <- .d |>
    dplyr::select(CIDa = "CID", "Term") |>
    dplyr::inner_join(.d |> dplyr::select(CIDb = "CID", "Term"), by = "Term", relationship = "many-to-many") |>
    dplyr::count(.data$CIDa, .data$CIDb, name = "nShared")
  pairs_ <- pairs_ |>
    dplyr::inner_join(shared_, by = c("CIDa", "CIDb")) |>
    dplyr::filter(.data$nShared >= .min_shared) |>
    dplyr::select(-"nShared")
  if (nrow(pairs_) == 0L) return(tibble::tibble())
  # Connected components at the threshold.
  g_ <- igraph::graph_from_data_frame(pairs_[, c("CIDa", "CIDb")], directed = FALSE, vertices = items_)
  comp_ <- igraph::components(g_)$membership
  # Evidence: the shared terms, with the share on each side.
  shares_ <- .d |> dplyr::select("CID", "Term", "Share")
  ev_ <- pairs_ |>
    dplyr::inner_join(shares_ |> dplyr::rename(CIDa = "CID", ShareA = "Share"), by = "CIDa",
                      relationship = "many-to-many") |>
    dplyr::inner_join(shares_ |> dplyr::rename(CIDb = "CID", ShareB = "Share"), by = c("CIDb", "Term")) |>
    dplyr::arrange(.data$CIDa, .data$CIDb, dplyr::desc(pmin(.data$ShareA, .data$ShareB))) |>
    dplyr::summarise(SharedTerms = dplyr::n(),
                     Evidence = paste0(utils::head(.data$Term, 3L), " (", round(100 * utils::head(.data$ShareA, 3L)), "% | ",
                                       round(100 * utils::head(.data$ShareB, 3L)), "%)", collapse = "; "),
                     .by = c("CIDa", "CIDb"))
  pairs_ |>
    dplyr::left_join(ev_, by = c("CIDa", "CIDb")) |>
    dplyr::mutate(Gaap = .d$Gaap[[1L]], Statement = .d$Statement[[1L]],
                  ProposedGroup = as.integer(comp_[.data$CIDa]), Decision = NA_character_, .before = 1L) |>
    dplyr::arrange(.data$ProposedGroup, dplyr::desc(.data$Cosine))
}


# 10. The statistics sheet ---------------------------------------------------------------------------------------------

#' One sheet per render: the numbers a reader takes away, US GAAP and IFRS side by side
#'
#' Everything the Overview tables show, reduced to one line each. Ambiguity is reported at a common
#' absolute floor across the two dictionaries, because the per-dictionary floor (97 versus 4
#' observations) makes the Overview's ambiguity rows incomparable: a term with four uses is trivially
#' unambiguous.
#'
#' @param .lP The configuration list.
#' @param .tab_class TagClass.
#' @param .tab_cpt Concepts.
#' @param .tab_dict Dict.
#' @param .tab_groups GroupProposals.
#' @param .tab_floor Output of dct_floor().
#' @param .common_floor Integer. Uses a term needs, in both dictionaries alike, to enter the ambiguity rows.
#' @return Invisibly, the sheet.
dct_stats <- function(.lP, .tab_class, .tab_cpt, .tab_dict, .tab_groups, .tab_floor, .common_floor = 20L) {
  if (FALSE) {
    .lP <- init_config(.script = "05A1-Dictionary"); .tab_class <- tibble::tibble(); .tab_cpt <- tibble::tibble()
    .tab_dict <- tibble::tibble(); .tab_groups <- tibble::tibble(); .tab_floor <- tibble::tibble(); .common_floor <- 20L
  }
  gaaps_ <- c("US", "IFRS")
  item_  <- c("Gaap", "CID", "Statement")
  n_  <- function(.x) format(round(.x), big.mark = ",")
  p_  <- function(.x) paste0(formatC(100 * .x, format = "f", digits = 1L), "%")
  r_  <- function(.x, .d = 2L) formatC(.x, format = "f", digits = .d)
  row_ <- function(.stat, .us, .ifrs, .section) {
    tibble::tibble(Section = .section, Statistic = .stat, US = .us, IFRS = .ifrs)
  }
  pick_ <- function(.t, .col, .g) { v_ <- .t[[.col]][.t$Gaap == .g]; if (length(v_) == 0L) NA else v_[[1L]] }

  # Coverage: standard lines only carry a Gaap; custom is one number for the whole face.
  cov_ <- .tab_class |>
    dplyr::filter(!is.na(.data$Gaap)) |>
    dplyr::summarise(Lines = sum(.data$nLines), Retired = sum(.data$nLines[.data$Bucket == "standard, retired"]),
                     .by = "Gaap") |>
    dplyr::mutate(ShareRetired = .data$Retired / .data$Lines)
  custom_ <- sum(.tab_class$nLines[.tab_class$Bucket == "custom"]) / sum(.tab_class$nLines)

  # Size.
  size_ <- .tab_dict |>
    dplyr::summarise(LineItems = dplyr::n_distinct(paste(.data$CID, .data$Statement)),
                     Terms = dplyr::n_distinct(.data$Term), Rows = dplyr::n(), Obs = sum(.data$nObs), .by = "Gaap") |>
    dplyr::left_join(.tab_floor, by = "Gaap")
  cpt_n_ <- .tab_cpt |> dplyr::count(.data$Gaap, name = "Concepts")

  # Many-to-one.
  cs_ <- .tab_dict |>
    dplyr::summarise(ObsItem = sum(.data$nObs), ModalObs = max(.data$nObs), nTerms5 = sum(.data$Share >= 0.05),
                     .by = dplyr::all_of(item_)) |>
    dplyr::inner_join(.tab_floor |> dplyr::select("Gaap", "MinObs"), by = "Gaap")
  modal_all_ <- cs_ |> dplyr::summarise(v = sum(.data$ModalObs) / sum(.data$ObsItem), .by = "Gaap")
  modal_stm_ <- cs_ |> dplyr::summarise(v = sum(.data$ModalObs) / sum(.data$ObsItem), .by = c("Gaap", "Statement"))
  syn_ <- cs_ |>
    dplyr::filter(.data$ObsItem >= .data$MinObs) |>
    dplyr::summarise(Median5 = stats::median(.data$nTerms5), Share4Plus = mean(.data$nTerms5 >= 4L), .by = "Gaap")

  # Key.
  plc_ <- .tab_cpt |>
    dplyr::inner_join(.tab_floor |> dplyr::select("Gaap", "MinObs"), by = "Gaap") |>
    dplyr::filter(.data$nObs >= .data$MinObs) |>
    dplyr::summarise(OneStatement = mean(.data$HomeShare >= 0.99), Multi = sum(.data$HomeShare < 0.8), .by = "Gaap")

  # One-to-many at the common floor.
  amb_ <- dct_read(.lP = .lP, .name = "TermIndex") |>
    dplyr::filter(.data$nUses >= .common_floor) |>
    dplyr::select("Gaap", "nConcepts", "nUses", "pStar") |>
    dplyr::collect() |>
    dplyr::summarise(Terms = dplyr::n(), ShareAmbiguous = mean(.data$nConcepts >= 2L),
                     pStar = sum(.data$pStar * .data$nUses) / sum(.data$nUses), .by = "Gaap")

  # Head and tail at 10+.
  prune_ <- .tab_dict |>
    dplyr::summarise(RowsShare = mean(.data$nObs >= 10L),
                     Coverage = sum(.data$nObs[.data$nObs >= 10L]) / sum(.data$nObs), .by = "Gaap")

  # Roles and ties.
  role_ <- dct_read(.lP = .lP, .name = "DictRole") |>
    dplyr::select("Gaap", "CID", "Statement", "Term", "PRole", "nObs") |>
    dplyr::collect()
  terse_ <- role_ |>
    dplyr::summarise(Terse = sum(.data$nObs[.data$PRole == "terseLabel"], na.rm = TRUE) / sum(.data$nObs), .by = "Gaap")
  multi_ <- role_ |>
    dplyr::summarise(Roles = dplyr::n_distinct(.data$PRole), Obs = sum(.data$nObs),
                     .by = c("Gaap", "CID", "Statement", "Term")) |>
    dplyr::summarise(ShareObs = sum(.data$Obs[.data$Roles >= 2L]) / sum(.data$Obs), .by = "Gaap")
  tie_ <- dct_read(.lP = .lP, .name = "Observations") |>
    dplyr::select("Gaap", "IsTie") |>
    dplyr::collect() |>
    dplyr::summarise(ShareTie = mean(.data$IsTie), .by = "Gaap")

  # Over time: first and last full year, modal share; generality mean over full years.
  yr_ <- dct_read(.lP = .lP, .name = "Observations") |>
    dplyr::count(.data$Gaap, .data$FY, .data$CID, .data$Statement, .data$Term, name = "n") |>
    dplyr::collect() |>
    dplyr::mutate(n = as.integer(.data$n))
  tot_ <- yr_ |> dplyr::summarise(Total = sum(.data$n), .by = c("Gaap", "CID", "Statement", "Term"))
  md_ <- tot_ |>
    dplyr::slice_max(.data$Total, n = 1L, by = c("Gaap", "CID", "Statement"), with_ties = FALSE) |>
    dplyr::mutate(IsModal = TRUE) |>
    dplyr::select("Gaap", "CID", "Statement", "Term", "IsModal")
  time_ <- yr_ |>
    dplyr::inner_join(tot_, by = c("Gaap", "CID", "Statement", "Term")) |>
    dplyr::left_join(md_, by = c("Gaap", "CID", "Statement", "Term")) |>
    dplyr::mutate(IsModal = dplyr::coalesce(.data$IsModal, FALSE), Other = .data$Total - .data$n) |>
    dplyr::summarise(Obs = sum(.data$n), Modal = sum(.data$n[.data$IsModal]) / sum(.data$n),
                     General = sum(.data$n[.data$Other >= .lP$Params$MinOther]) / sum(.data$n),
                     .by = c("Gaap", "FY")) |>
    dplyr::filter(!is.na(.data$FY), .data$Obs >= 10000L) |>
    dplyr::arrange(.data$Gaap, .data$FY) |>
    dplyr::summarise(FirstFY = dplyr::first(.data$FY), LastFY = dplyr::last(.data$FY),
                     ModalFirst = dplyr::first(.data$Modal), ModalLast = dplyr::last(.data$Modal),
                     General = mean(.data$General), .by = "Gaap")

  grp_ <- .tab_groups |>
    dplyr::summarise(Groups = dplyr::n_distinct(paste(.data$Statement, .data$ProposedGroup)), Pairs = dplyr::n(),
                     .by = "Gaap")

  g_ <- function(.t, .col, .f) purrr::map_chr(gaaps_, \(.g) { v_ <- pick_(.t, .col, .g); if (is.na(v_)) "--" else .f(v_) })
  two_ <- function(.stat, .t, .col, .f, .section) { v_ <- g_(.t, .col, .f); row_(.stat, v_[[1L]], v_[[2L]], .section) }

  sheet_ <- dplyr::bind_rows(
    two_("Filings", size_, "Filings", n_, "Size"),
    two_("Concepts", cpt_n_, "Concepts", n_, "Size"),
    two_("Line items (concept on a statement)", size_, "LineItems", n_, "Size"),
    two_("Distinct terms at the working rung", size_, "Terms", n_, "Size"),
    two_("Dictionary rows (line item, term)", size_, "Rows", n_, "Size"),
    two_("Observations (filing x line item)", size_, "Obs", n_, "Size"),
    two_("Characterisation floor (observations)", size_, "MinObs", n_, "Size"),
    row_("Custom tags, share of all face lines", p_(custom_), p_(custom_), "Coverage"),
    two_("Retired concepts, share of standard lines", cov_, "ShareRetired", p_, "Coverage"),
    two_("Modal term, share of observations", modal_all_, "v", p_, "Many-to-one"),
    purrr::map(c("BS", "IS", "CF", "CI", "EQ"), \(.s) {
      two_(paste0("  on ", .s), modal_stm_ |> dplyr::filter(.data$Statement == .s), "v", p_, "Many-to-one")
    }) |> purrr::list_rbind(),
    two_("Synonyms above 5%, median line item", syn_, "Median5", \(.x) r_(.x, 0L), "Many-to-one"),
    two_("Line items with 4+ synonyms above 5%", syn_, "Share4Plus", p_, "Many-to-one"),
    two_("Concepts on one statement 99%+ of the time", plc_, "OneStatement", p_, "Key"),
    two_("Concepts below 80% on their modal statement", plc_, "Multi", n_, "Key"),
    two_(paste0("Terms with ", .common_floor, "+ uses (common floor)"), amb_, "Terms", n_, "One-to-many"),
    two_("  naming more than one concept on their statement", amb_, "ShareAmbiguous", p_, "One-to-many"),
    two_("  star concept correct, use-weighted (pStar)", amb_, "pStar", p_, "One-to-many"),
    two_("Rows kept at 10+ observations", prune_, "RowsShare", p_, "Head and tail"),
    two_("  observations they cover", prune_, "Coverage", p_, "Head and tail"),
    two_("Kept caption was the terse label", terse_, "Terse", p_, "Label role"),
    two_("Observations under strings used in several roles", multi_, "ShareObs", p_, "Label role"),
    two_("Observations where the tiebreak chose", tie_, "ShareTie", p_, "Label role"),
    two_("First full year", time_, "FirstFY", \(.x) as.character(.x), "Over time"),
    two_("Last full year", time_, "LastFY", \(.x) as.character(.x), "Over time"),
    two_("Modal share, first full year", time_, "ModalFirst", p_, "Over time"),
    two_("Modal share, last full year", time_, "ModalLast", p_, "Over time"),
    two_("Generality, mean over full years", time_, "General", p_, "Over time"),
    two_("Proposed groups", grp_, "Groups", n_, "Groups"),
    two_("Proposed pairs", grp_, "Pairs", n_, "Groups")
  )
  tbl_out(.tab = sheet_, .title = "Statistics: the two dictionaries side by side, at the working rung")
  tbl_note(paste0("Ambiguity rows use a common floor of ", .common_floor, " uses in both dictionaries; the Overview's \
            ambiguity table uses each dictionary's own floor and is not comparable across the two. Full years are \
            those with at least 10,000 observations. Terms are lemmatised keys, not display strings."))
  invisible(sheet_)
}


# 11. Overview ---------------------------------------------------------------------------------------------------------
# Every table per dictionary, at the working rung. The ladder table is the one place all rungs appear.

#' 1. Coverage: every face line by bucket, per dictionary
#' @param .tab_class TagClass.
#' @return Invisibly, the table.
dct_ov_coverage <- function(.tab_class) {
  if (FALSE) .tab_class <- tibble::tibble()
  out_ <- .tab_class |>
    dplyr::mutate(Gaap = dplyr::coalesce(.data$Gaap, "(custom)")) |>
    dplyr::summarise(Tags = dplyr::n(), Concepts = dplyr::n_distinct(.data$CID), Lines = sum(.data$nLines),
                     .by = c("Gaap", "Bucket")) |>
    dplyr::mutate(ShareLines = round(.data$Lines / sum(.data$Lines), 3)) |>
    dplyr::arrange(.data$Gaap, dplyr::desc(.data$Lines))
  tbl_out(.tab = out_, .title = "1. Coverage: every face line, by what the dictionary does with it")
  tbl_note("Custom tags leave. Every standard bucket is in. Retired: the reference taxonomy no longer declares \\
            the tag; newer: the filer's vintage postdates the reference. Shares are of all lines.")
  odd_ <- .tab_class |>
    dplyr::filter(.data$Bucket == "standard, unknown vintage") |>
    dplyr::count(.data$Version, wt = .data$nLines, name = "Lines") |>
    dplyr::arrange(dplyr::desc(.data$Lines)) |>
    utils::head(12L)
  if (nrow(odd_) > 0L) tbl_out(.tab = odd_, .title = "1. Coverage: version strings the vintage regex could not read")
  invisible(out_)
}

#' 2. Universe: concepts by resolve status, by home statement, retired by last vintage; per dictionary
#' @param .tab_cpt Concepts.
#' @return Invisibly, the first table.
dct_ov_universe <- function(.tab_cpt) {
  if (FALSE) .tab_cpt <- tibble::tibble()
  lv_ <- c("resolved", "retired", "newer", "unknown vintage")
  out_ <- .tab_cpt |>
    dplyr::summarise(Concepts = dplyr::n(), Obs = sum(.data$nObs), WithFsdsLabel = sum(!is.na(.data$FsdsLabel)),
                     WithTaxLabel = sum(!is.na(.data$StandardLabel)), .by = c("Gaap", "ResolveStatus")) |>
    dplyr::mutate(ResolveStatus = factor(.data$ResolveStatus, levels = lv_)) |>
    dplyr::arrange(.data$Gaap, .data$ResolveStatus) |>
    dplyr::mutate(ResolveStatus = as.character(.data$ResolveStatus))
  tbl_out(.tab = out_, .title = "2. Universe: concepts filers printed, by how the reference taxonomy relates to them")
  tbl_note("Obs is concept-level: filings that printed the concept anywhere. WithFsdsLabel is the tag.txt label, \\
            available for every concept; WithTaxLabel the taxonomy's, for resolved ones only.")
  home_ <- .tab_cpt |>
    dplyr::count(.data$Gaap, .data$HomeStatement, name = "Concepts") |>
    tidyr::pivot_wider(names_from = "HomeStatement", values_from = "Concepts", values_fill = 0L)
  tbl_out(.tab = home_, .title = "2. Universe: concepts by their modal statement")
  span_ <- .tab_cpt |>
    dplyr::filter(.data$ResolveStatus == "retired") |>
    dplyr::count(.data$Gaap, .data$LastVintage, name = "Concepts") |>
    tidyr::pivot_wider(names_from = "Gaap", values_from = "Concepts", values_fill = 0L) |>
    dplyr::arrange(.data$LastVintage)
  tbl_out(.tab = span_, .title = "2. Universe: retired concepts by the last vintage a filer used them under")
  invisible(out_)
}

#' 3. Ladder: distinct terms per rung, and five captions walked through it
#' @param .tab_ladder Ladder.
#' @param .lP The configuration list, to weight the walkthrough by use.
#' @return Invisibly, the rung table.
dct_ov_ladder <- function(.tab_ladder, .lP) {
  if (FALSE) { .tab_ladder <- dct_ladder_build(c("a")); .lP <- init_config(.script = "05A1-Dictionary") }
  out_ <- .tab_ladder |>
    dplyr::summarise(Distinct = dplyr::n_distinct(.data$Term), .by = "Rung") |>
    dct_arrange_rung() |>
    dplyr::mutate(Rule = unname(.dct_rung_rules[.data$Rung]), Collapsed = dplyr::lag(.data$Distinct) - .data$Distinct)
  tbl_out(.tab = out_, .title = "3. Ladder: distinct terms at each rung")
  tbl_note("Every string a rung collapses is a difference the dictionary at that rung no longer records. The working \\
            rung is a parameter; this table is what it was picked from.")

  # Five captions, chosen by pattern among the most used, so each shows one rung doing something.
  use_ <- dct_read(.lP = .lP, .name = "FaceItems") |>
    dplyr::filter(!.data$IsCustom) |>
    dplyr::count(.data$Caption, name = "Lines") |>
    dplyr::collect()
  pick_ <- function(.rx) use_ |>
    dplyr::filter(stringi::stri_detect_regex(.data$Caption, .rx)) |>
    dplyr::slice_max(.data$Lines, n = 1L, with_ties = FALSE) |>
    dplyr::pull("Caption")
  caps_ <- unique(c(
    pick_("\\(Note"), pick_("[Bb]eginning of"), pick_("^Total .*\\("), pick_("\\$[0-9.]+ par value"),
    pick_("^Balance at"), pick_("^Less:")
  ))
  walk_ <- .tab_ladder |>
    dplyr::filter(.data$Caption %in% caps_) |>
    dplyr::mutate(Rung = factor(.data$Rung, levels = .dct_rungs)) |>
    tidyr::pivot_wider(id_cols = "Caption", names_from = "Rung", values_from = "Term") |>
    dplyr::select(-"Raw", -"L0")
  tbl_out(.tab = walk_, .title = "3. Ladder: captions walked up the rungs (L0 differs from Raw only in case)")
  invisible(out_)
}

#' 3b. What L5 removed most: the L4 -> L5 collapses with the most uses
#' @param .lP The configuration list.
#' @return Invisibly, the table.
dct_ov_l5 <- function(.lP) {
  if (FALSE) .lP <- init_config(.script = "05A1-Dictionary")
  lad_ <- dct_read(.lP = .lP, .name = "Ladder") |>
    dplyr::filter(.data$Rung %in% c("L4", "L5")) |>
    dplyr::collect() |>
    tidyr::pivot_wider(id_cols = "Caption", names_from = "Rung", values_from = "Term") |>
    dplyr::filter(.data$L4 != .data$L5)
  use_ <- dct_read(.lP = .lP, .name = "FaceItems") |>
    dplyr::filter(!.data$IsCustom) |>
    dplyr::count(.data$Caption, name = "Lines") |>
    dplyr::collect()
  out_ <- lad_ |>
    dplyr::inner_join(use_, by = "Caption") |>
    dplyr::summarise(Lines = sum(.data$Lines), Captions = dplyr::n(), .by = c("L4", "L5")) |>
    dplyr::arrange(dplyr::desc(.data$Lines)) |>
    utils::head(25L)
  tbl_out(.tab = out_, .title = "3. Ladder: the 25 L4 -> L5 collapses with the most face lines")
  tbl_note("Read L4 against L5: every row is a phrase the qualifier rules removed. A row that removed meaning \\
            rather than form is a rule to narrow; a phrase that should have gone and did not is a rule to add.")
  invisible(out_)
}

#' 4. Size at the working rung, per dictionary and statement
#' @param .tab_dict Dict.
#' @param .tab_obs_n Tibble from dct_floor().
#' @return Invisibly, the table.
dct_ov_size <- function(.tab_dict, .tab_obs_n) {
  if (FALSE) { .tab_dict <- tibble::tibble(); .tab_obs_n <- tibble::tibble() }
  by_stm_ <- .tab_dict |>
    dplyr::summarise(LineItems = dplyr::n_distinct(.data$CID), Terms = dplyr::n_distinct(.data$Term),
                     Rows = dplyr::n(), Obs = sum(.data$nObs), .by = c("Gaap", "Statement"))
  tot_ <- .tab_dict |>
    dplyr::summarise(Statement = "all", Concepts = dplyr::n_distinct(.data$CID),
                     LineItems = dplyr::n_distinct(paste(.data$CID, .data$Statement)),
                     Terms = dplyr::n_distinct(.data$Term), Rows = dplyr::n(), Obs = sum(.data$nObs), .by = "Gaap") |>
    dplyr::left_join(.tab_obs_n |> dplyr::select("Gaap", "Filings", "MinObs"), by = "Gaap")
  out_ <- dplyr::bind_rows(tot_, by_stm_) |>
    dplyr::mutate(TermsPerItem = round(.data$Rows / .data$LineItems, 1)) |>
    dplyr::arrange(.data$Gaap, .data$Statement != "all", .data$Statement)
  tbl_out(.tab = out_, .title = "4. Size at the working rung: line items, terms, rows, observations")
  tbl_note("An observation is one filing printing one line item, once. Filings and MinObs are per dictionary: a \\
            line item is characterised below only if it has at least MinObs observations.")
  invisible(out_)
}

#' 5. Key: placement stability, per dictionary
#' @param .tab_cpt Concepts.
#' @param .tab_obs_n Tibble from dct_floor().
#' @return Invisibly, the bucket table.
dct_ov_placement <- function(.tab_cpt, .tab_obs_n) {
  if (FALSE) { .tab_cpt <- tibble::tibble(); .tab_obs_n <- tibble::tibble() }
  big_ <- .tab_cpt |>
    dplyr::inner_join(.tab_obs_n |> dplyr::select("Gaap", "MinObs"), by = "Gaap") |>
    dplyr::filter(.data$nObs >= .data$MinObs)
  buck_ <- big_ |>
    dplyr::mutate(Bucket = cut(.data$HomeShare, c(0, 0.5, 0.8, 0.9, 0.99, 1), include.lowest = TRUE)) |>
    dplyr::count(.data$Gaap, .data$Bucket, name = "Concepts") |>
    dplyr::mutate(Share = round(.data$Concepts / sum(.data$Concepts), 3), .by = "Gaap")
  tbl_out(.tab = buck_, .title = "5. Key: share of a concept's use on its modal statement, concepts above the floor")
  tbl_note("Concepts near 1 are unaffected by keying on the statement. The rest are what the key separates.")
  multi_ <- big_ |>
    dplyr::filter(.data$HomeShare < 0.8) |>
    dplyr::arrange(.data$Gaap, dplyr::desc(.data$nObs)) |>
    dplyr::slice_head(n = 12L, by = "Gaap") |>
    dplyr::transmute(.data$Gaap,
                     CID = dct_wrap(stringi::stri_replace_first_regex(.data$CID, "^(us-gaap|ifrs-full):", "")),
                     .data$nObs, .data$HomeStatement, .data$HomeShare, .data$Statements)
  tbl_out(.tab = multi_, .title = "5. Key: the most used concepts printed on several statements, per dictionary")
  invisible(buck_)
}

#' 6. Many-to-one: terms per line item, modal share, synonym buckets; per dictionary
#' @param .tab_dict Dict.
#' @param .tab_obs_n Tibble from dct_floor().
#' @return Invisibly, the distribution table.
dct_ov_terms <- function(.tab_dict, .tab_obs_n) {
  if (FALSE) { .tab_dict <- tibble::tibble(); .tab_obs_n <- tibble::tibble() }
  item_ <- c("Gaap", "CID", "Statement")
  # ObsItem, never nObs: summarise() evaluates sequentially, and a result named like the source column
  # replaces it for the expressions after it.
  cs_ <- .tab_dict |>
    dplyr::summarise(
      nTerms = dplyr::n(), ObsItem = sum(.data$nObs),
      nTerms5 = sum(.data$Share >= 0.05), nTerms1 = sum(.data$Share >= 0.01),
      ModalObs = max(.data$nObs), ModalShare = max(.data$Share), HHI = sum(.data$Share^2),
      .by = dplyr::all_of(item_)
    )
  big_ <- cs_ |>
    dplyr::inner_join(.tab_obs_n |> dplyr::select("Gaap", "MinObs"), by = "Gaap") |>
    dplyr::filter(.data$ObsItem >= .data$MinObs)
  q_ <- function(.x, .p) round(stats::quantile(.x, .p, names = FALSE), 2)
  out_ <- big_ |>
    dplyr::summarise(
      LineItems = dplyr::n(), OneTerm = round(mean(.data$nTerms == 1L), 3),
      TermsMedian = q_(.data$nTerms, 0.5), TermsP90 = q_(.data$nTerms, 0.9),
      Terms1Median = q_(.data$nTerms1, 0.5), Terms5Median = q_(.data$nTerms5, 0.5), Terms5P90 = q_(.data$nTerms5, 0.9),
      ModalMedian = q_(.data$ModalShare, 0.5), HHIMedian = q_(.data$HHI, 0.5),
      .by = "Gaap"
    )
  tbl_out(.tab = out_, .title = "6. Many-to-one: terms per line item, line items above the floor")
  tbl_note("Terms1 and Terms5: strings with at least 1% or 5% of the line item's observations -- the synonyms in \\
            real use. ModalShare is the most common term's share; HHI the sum of squared shares.")

  modal_ <- cs_ |>
    dplyr::summarise(v = round(sum(.data$ModalObs) / sum(.data$ObsItem), 3), .by = c("Gaap", "Statement")) |>
    tidyr::pivot_wider(names_from = "Statement", values_from = "v") |>
    dplyr::inner_join(cs_ |> dplyr::summarise(All = round(sum(.data$ModalObs) / sum(.data$ObsItem), 3), .by = "Gaap"),
                      by = "Gaap") |>
    dplyr::relocate("All", .after = "Gaap")
  tbl_out(.tab = modal_, .title = "6. Many-to-one: share of observations printed as the line item's modal term")
  tbl_note("Observation-weighted, all line items. 'How standardised is the face' at the unit the paper uses.")

  buck_ <- big_ |>
    dplyr::mutate(Synonyms = dplyr::case_when(
      .data$nTerms5 == 0L ~ "0: none above 5%", .data$nTerms5 == 1L ~ "1", .data$nTerms5 == 2L ~ "2",
      .data$nTerms5 == 3L ~ "3", .data$nTerms5 <= 5L ~ "4-5", TRUE ~ "6+")) |>
    dplyr::count(.data$Gaap, .data$Synonyms, name = "N") |>
    dplyr::mutate(Share = round(.data$N / sum(.data$N), 3), .by = "Gaap") |>
    dplyr::select("Gaap", "Synonyms", "Share") |>
    tidyr::pivot_wider(names_from = "Gaap", values_from = "Share", values_fill = 0)
  tbl_out(.tab = buck_, .title = "6. Many-to-one: line items by number of synonyms in real use (>= 5%)")
  invisible(out_)
}

#' 7. One-to-many: ambiguity per dictionary and statement
#' @param .lP The configuration list.
#' @param .tab_obs_n Tibble from dct_floor().
#' @return Invisibly, the per-dictionary table.
dct_ov_ambiguity <- function(.lP, .tab_obs_n) {
  if (FALSE) { .lP <- init_config(.script = "05A1-Dictionary"); .tab_obs_n <- tibble::tibble() }
  one_ <- function(.name, .nm) dct_read(.lP = .lP, .name = .name) |>
    dplyr::select("Gaap", "Statement", "nConcepts", "nUses", "pStar") |>
    dplyr::collect() |>
    dplyr::inner_join(.tab_obs_n |> dplyr::select("Gaap", "MinObs"), by = "Gaap") |>
    dplyr::filter(.data$nUses >= .data$MinObs) |>
    dplyr::summarise(
      Terms = dplyr::n(), Ambiguous = sum(.data$nConcepts >= 2L),
      ShareAmbiguous = round(mean(.data$nConcepts >= 2L), 3),
      pStarMedian = round(stats::median(.data$pStar), 3),
      pStarWeighted = round(sum(.data$pStar * .data$nUses) / sum(.data$nUses), 3),
      .by = "Gaap"
    ) |>
    dplyr::mutate(Index = .nm, .before = 1L)
  out_ <- dplyr::bind_rows(one_("TermIndex", "statement + term"), one_("TermIndexRole", "statement + term + role")) |>
    dplyr::arrange(.data$Gaap, .data$Index)
  tbl_out(.tab = out_, .title = "7. One-to-many: terms naming more than one concept on the same statement, above the floor")
  tbl_note("pStar is how often the string on its statement -- or with its role -- picks the concept the filer \\
            meant. Weighted by uses. The ceiling for looking a caption up before groups.")
  by_stm_ <- dct_read(.lP = .lP, .name = "TermIndex") |>
    dplyr::select("Gaap", "Statement", "nConcepts", "nUses", "pStar") |>
    dplyr::collect() |>
    dplyr::inner_join(.tab_obs_n |> dplyr::select("Gaap", "MinObs"), by = "Gaap") |>
    dplyr::filter(.data$nUses >= .data$MinObs) |>
    dplyr::summarise(Terms = dplyr::n(), ShareAmbiguous = round(mean(.data$nConcepts >= 2L), 3),
                     pStarWeighted = round(sum(.data$pStar * .data$nUses) / sum(.data$nUses), 3),
                     .by = c("Gaap", "Statement")) |>
    dplyr::arrange(.data$Gaap, .data$Statement)
  tbl_out(.tab = by_stm_, .title = "7. One-to-many: by dictionary and statement")
  invisible(out_)
}

#' 8. Head and tail: what a pruned dictionary keeps, per dictionary
#' @param .tab_dict Dict.
#' @return Invisibly, the table.
dct_ov_pruning <- function(.tab_dict) {
  if (FALSE) .tab_dict <- tibble::tibble()
  item_ <- c("Gaap", "CID", "Statement")
  ranked_ <- .tab_dict |>
    dplyr::arrange(.data$Gaap, .data$CID, .data$Statement, dplyr::desc(.data$nObs)) |>
    dplyr::mutate(Rank = dplyr::row_number(), .by = dplyr::all_of(item_))
  rule_ <- function(.keep, .nm) ranked_ |>
    dplyr::summarise(Kept = sum(.data$nObs[.keep(.data$Rank, .data$Share, .data$nObs)]), Total = sum(.data$nObs),
                     RowsKept = sum(.keep(.data$Rank, .data$Share, .data$nObs)), RowsTotal = dplyr::n(), .by = "Gaap") |>
    dplyr::transmute(.data$Gaap, Rule = .nm, RowsShare = round(.data$RowsKept / .data$RowsTotal, 3),
                     Coverage = round(.data$Kept / .data$Total, 4))
  out_ <- dplyr::bind_rows(
    rule_(\(.r, .s, .n) .r <= 1L,   "modal term only"),
    rule_(\(.r, .s, .n) .s >= 0.01, "terms with >= 1% of the item"),
    rule_(\(.r, .s, .n) .n >= 10L,  "terms used by >= 10 filings")
  ) |>
    dplyr::arrange(.data$Gaap, .data$Rule)
  tbl_out(.tab = out_, .title = "8. Head and tail: observations covered if only the head of each line item is kept")
  tbl_note("RowsShare: share of dictionary rows a rule keeps; Coverage: share of observations they account for. \\
            What 05B chooses between.")
  invisible(out_)
}

#' 9. Label role: distribution, strings under several roles, and what the tiebreak touched; per dictionary
#' @param .lP The configuration list.
#' @return Invisibly, the role distribution.
dct_ov_roles <- function(.lP) {
  if (FALSE) .lP <- init_config(.script = "05A1-Dictionary")
  r_ <- dct_read(.lP = .lP, .name = "DictRole") |>
    dplyr::select("Gaap", "CID", "Statement", "Term", "PRole", "nObs") |>
    dplyr::collect()
  dist_ <- r_ |>
    dplyr::mutate(PRole = dplyr::coalesce(.data$PRole, "(none)")) |>
    dplyr::summarise(Obs = sum(.data$nObs), .by = c("Gaap", "PRole")) |>
    dplyr::mutate(Share = round(.data$Obs / sum(.data$Obs), 3), .by = "Gaap") |>
    dplyr::arrange(.data$Gaap, dplyr::desc(.data$Obs)) |>
    dplyr::slice_head(n = 8L, by = "Gaap")
  tbl_out(.tab = dist_, .title = "9. Label role: which role the kept caption carried, by observations")
  multi_ <- r_ |>
    dplyr::summarise(Roles = dplyr::n_distinct(.data$PRole), Obs = sum(.data$nObs),
                     .by = c("Gaap", "CID", "Statement", "Term")) |>
    dplyr::summarise(Terms = dplyr::n(), UnderTwoPlusRoles = sum(.data$Roles >= 2L),
                     ShareTerms = round(mean(.data$Roles >= 2L), 3),
                     ShareObs = round(sum(.data$Obs[.data$Roles >= 2L]) / sum(.data$Obs), 3), .by = "Gaap")
  tbl_out(.tab = multi_, .title = "9. Label role: (line item, term) pairs printed under more than one role")
  tbl_note("If most observations sit under strings that appear under several roles, the role does not decide the \\
            caption, and putting it in the key would split synonyms that are the same string.")
  tie_ <- dct_read(.lP = .lP, .name = "Observations") |>
    dplyr::select("Gaap", "Statement", "nCaptions", "nTerms", "IsTie") |>
    dplyr::collect() |>
    dplyr::summarise(Obs = dplyr::n(), MultiCaption = sum(.data$nCaptions > 1L), Tie = sum(.data$IsTie),
                     ShareMultiCaption = round(mean(.data$nCaptions > 1L), 3), ShareTie = round(mean(.data$IsTie), 3),
                     .by = c("Gaap", "Statement")) |>
    dplyr::arrange(.data$Gaap, .data$Statement)
  tbl_out(.tab = tie_, .title = "9. One term per observation: line items printed more than once in a filing, and ties")
  tbl_note("MultiCaption: the filing printed the line item on more than one face line. Tie: those lines carried \\
            more than one distinct term at the working rung, so the shortest-first rule chose. The gap between \\
            the two is what L5 already collapsed.")
  invisible(dist_)
}

#' 10. Over time: generality and modal share per fiscal year, per dictionary
#' @param .lP The configuration list.
#' @param .min_other Integer.
#' @return Invisibly, the table.
dct_ov_years <- function(.lP, .min_other = 10L) {
  if (FALSE) { .lP <- init_config(.script = "05A1-Dictionary"); .min_other <- 10L }
  yr_ <- dct_read(.lP = .lP, .name = "Observations") |>
    dplyr::count(.data$Gaap, .data$FY, .data$CID, .data$Statement, .data$Term, name = "n") |>
    dplyr::collect() |>
    dplyr::mutate(n = as.integer(.data$n))
  key_ <- c("Gaap", "CID", "Statement", "Term")
  tot_ <- yr_ |> dplyr::summarise(Total = sum(.data$n), .by = dplyr::all_of(key_))
  modal_ <- tot_ |>
    dplyr::slice_max(.data$Total, n = 1L, by = c("Gaap", "CID", "Statement"), with_ties = FALSE) |>
    dplyr::mutate(IsModal = TRUE) |>
    dplyr::select(dplyr::all_of(key_), "IsModal")
  out_ <- yr_ |>
    dplyr::inner_join(tot_, by = key_) |>
    dplyr::left_join(modal_, by = key_) |>
    dplyr::mutate(Other = .data$Total - .data$n, IsModal = dplyr::coalesce(.data$IsModal, FALSE)) |>
    dplyr::summarise(
      Obs = sum(.data$n),
      ShareGeneral = round(sum(.data$n[.data$Other >= .min_other]) / sum(.data$n), 3),
      ShareModal   = round(sum(.data$n[.data$IsModal]) / sum(.data$n), 3),
      .by = c("Gaap", "FY")
    ) |>
    dplyr::filter(!is.na(.data$FY)) |>
    tidyr::pivot_wider(names_from = "Gaap", values_from = c("Obs", "ShareGeneral", "ShareModal")) |>
    dplyr::arrange(.data$FY)
  tbl_out(.tab = out_, .title = paste0("10. Over time: observations per fiscal year covered by terms used in other \\
                                        years (>= ", .min_other, ") and by the all-years modal term"))
  tbl_note("ShareGeneral is the number to watch: a year where it drops is a year whose captions the rest of the \\
            dictionary does not know. ShareModal is the paper's trend, shown to see whether it moves.")
  invisible(out_)
}

#' 11. Groups: what step 8 proposed, per dictionary and statement
#' @param .tab_groups GroupProposals.
#' @return Invisibly, the summary.
dct_ov_groups <- function(.tab_groups) {
  if (FALSE) .tab_groups <- tibble::tibble()
  if (nrow(.tab_groups) == 0L) {
    tbl_note("No pairs above the group threshold.", .type = "warn")
    return(invisible(.tab_groups))
  }
  sum_ <- .tab_groups |>
    dplyr::summarise(Pairs = dplyr::n(), Groups = dplyr::n_distinct(.data$ProposedGroup),
                     Items = dplyr::n_distinct(c(.data$CIDa, .data$CIDb)),
                     CosineMedian = round(stats::median(.data$Cosine), 3), .by = c("Gaap", "Statement")) |>
    dplyr::arrange(.data$Gaap, .data$Statement)
  tbl_out(.tab = sum_, .title = "11. Groups: proposed pairs and groups by dictionary and statement")
  top_ <- .tab_groups |>
    dplyr::arrange(dplyr::desc(.data$Cosine)) |>
    dplyr::slice_head(n = 10L, by = "Gaap") |>
    dplyr::transmute(.data$Gaap, .data$Statement, .data$ProposedGroup,
                     CIDa = dct_wrap(stringi::stri_replace_first_regex(.data$CIDa, "^(us-gaap|ifrs-full):", "")),
                     CIDb = dct_wrap(stringi::stri_replace_first_regex(.data$CIDb, "^(us-gaap|ifrs-full):", "")),
                     .data$Cosine, .data$SharedTerms, .data$Evidence)
  tbl_out(.tab = top_, .title = "11. Groups: the ten strongest proposals per dictionary, with evidence")
  tbl_note("Cosine of the two line items' share vectors over their head terms. Evidence: up to three shared terms \\
            with the share on each side. These are proposals; the accepted list is a parameter of a later render.")
  invisible(sum_)
}


# 12. Examples ---------------------------------------------------------------------------------------------------------

#' One concept, every statement it appears on, at the working rung
#' @param .tab_dict Dict.
#' @param .cid Character.
#' @param .n Integer. Terms per statement.
#' @return Invisibly NULL.
dct_report_concept <- function(.tab_dict, .cid, .n = 6L) {
  if (FALSE) { .tab_dict <- tibble::tibble(); .cid <- "us-gaap:NetIncomeLoss"; .n <- 6L }
  d_ <- .tab_dict |> dplyr::filter(.data$CID == .cid)
  if (nrow(d_) == 0L) { cli::cli_alert_warning("{(.cid)}: no rows."); return(invisible(NULL)) }
  tot_ <- d_ |>
    dplyr::summarise(Obs = sum(.data$nObs), Terms = dplyr::n(), .by = "Statement") |>
    dplyr::arrange(dplyr::desc(.data$Obs))
  tbl_out(.tab = tot_, .title = paste0(.cid, ": statements it is printed on"))
  tbl_out(
    .tab = d_ |>
      dplyr::slice_max(.data$nObs, n = .n, by = "Statement", with_ties = FALSE) |>
      dplyr::arrange(.data$Statement, dplyr::desc(.data$nObs)) |>
      dplyr::transmute(.data$Statement, Term = stringi::stri_sub(.data$Term, 1L, 52L), .data$nObs, .data$nFirms,
                       Share = round(.data$Share, 3)),
    .title = paste0(.cid, ": top ", .n, " terms per statement")
  )
  invisible(NULL)
}
