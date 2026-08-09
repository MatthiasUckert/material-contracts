# 04B-CoWorkCompile.R -- turn reading-session responses into machine-applicable rule files
#
# WHAT THIS IS
# The return leg of the entity reading arm. 04B-CoWorkEntities.R writes one bundle; a reading
# session fills its responses/ with JSONL; this script reads those, validates them against the
# closed vocabularies the bundle declared, normalises the patterns into the form the matcher can
# apply, and writes one rule file per kind.
#
# It sources nothing and needs only the bundle folder.
#
# WHY THE VOCABULARIES ARE CLOSED AND CHECKED HERE
# A cue carries a Label and a Role, and 04D turns roles into columns. A role the session invented --
# "party_or_agent", "signing_date" where the vocabulary says "signing" -- would pass silently through
# a permissive reader and surface much later as a column nobody expected, or as a rule that never
# fires because nothing dispatches on it. The vocabularies are read back out of the bundle's own
# MANIFEST.json rather than restated here, so the two cannot drift.
#
# WHY THE PATTERNS NEED NORMALISING
# Cues are applied as case-insensitive literal phrases over a context window, not as regular
# expressions. A pattern carrying punctuation, digits or regex metacharacters would either fail to
# match or, worse, match as a pattern and quietly mean something else. Anything that cannot survive
# normalisation is reported rather than dropped in silence, because a silent drop is
# indistinguishable from the session simply having proposed fewer rules.
#
# NOTES ARE KEPT, NOT SCORED
# A note is an observation that no literal phrase can express -- a parenthesised defined term, a
# capitalisation habit. It has no Pattern and cannot be applied by machine, so it is written to its
# own file for a human to read and implement. Discarding them would throw away exactly the
# observations a frequency table cannot reach, which is the reason for having a reading arm at all.
#
# House style: native pipe; explicit package::function; dot-prefixed arguments; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new ones; if (FALSE) development blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} in cli interpolation.


# 1. Configuration ----------------------------------------------------------------------------------

.DIR_BUNDLE <- fs::path("/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts",
                        "MatContractData", "EntitiesClaude")

# A previous bundle to compare against, or NULL. Recurrence between two independent sessions is the
# only reliability evidence the reading arm can produce: a rule proposed twice, from different
# documents, is a property of the corpus rather than of one draw.
.DIR_PRIOR  <- NULL

.KINDS      <- c("cue", "stop", "section", "note")
.SIDES      <- c("left", "right", "either", "span", "heading", "")
.MAX_WORDS  <- 8L     # longest cue phrase kept; money cues are long and formulaic
.MIN_CHARS  <- 2L     # shortest token retained; cues are read as words, not as engine n-grams
.MAX_WINDOW <- 400L   # a cue claiming a wider reach than this is not a cue, it is the document


# 2. Helpers ----------------------------------------------------------------------------------------

#' Fold a proposed phrase into the matcher's form
#'
#' The matcher looks for a lowercase word sequence inside a context window, so the phrase has to
#' reduce to words. Returns NA where nothing usable survives or the phrase is longer than the cap, so
#' the caller can report the loss.
#'
#' @param .x Character vector of proposed patterns.
#' @param .max_words Longest phrase kept.
#' @param .min_chars Shortest token retained.
#' @return Character vector of normalised patterns, NA where unusable.
cwec_normalise <- function(.x, .max_words = 8L, .min_chars = 2L) {
  if (FALSE) {
    .x         <- c("by and between", "(the \"Seller\")", "dated as of", "")
    .max_words <- 8L
    .min_chars <- 2L
  }
  .x |>
    stringi::stri_trans_tolower() |>
    stringi::stri_extract_all_regex(pattern = paste0("[a-z]{", .min_chars, ",}")) |>
    purrr::map_chr(function(.t) {
      if (length(.t) == 0L || all(is.na(.t))) return(NA_character_)
      if (length(.t) > .max_words) return(NA_character_)
      paste(.t, collapse = " ")
    })
}

#' Read one JSONL response file
#'
#' One object per line, so a malformed line costs that line and never the file.
#'
#' @param .path Path to a .jsonl file.
#' @return Tibble with the raw fields plus File and Line.
cwec_read_jsonl <- function(.path) {
  if (FALSE) .path <- fs::dir_ls(fs::path(.DIR_BUNDLE, "responses"))[[1]]

  lines_ <- readLines(.path, warn = FALSE)
  keep_  <- trimws(lines_) != ""

  purrr::map2(lines_[keep_], which(keep_), function(.l, .i) {
    obj_ <- tryCatch(jsonlite::fromJSON(.l), error = function(e) NULL)
    one_ <- function(.v, .default = NA_character_) {
      if (is.null(.v) || length(.v) == 0L) .default else as.character(.v)[[1]]
    }
    tibble::tibble(
      Kind        = one_(obj_$Kind),
      Label       = one_(obj_$Label),
      Role        = one_(obj_$Role, ""),
      PatternRaw  = one_(obj_$Pattern, ""),
      Side        = one_(obj_$Side, ""),
      WindowRaw   = one_(obj_$Window, "0"),
      Rationale   = one_(obj_$Rationale),
      Evidence    = one_(obj_$Evidence),
      File        = as.character(fs::path_file(.path)),
      Line        = .i
    )
  }) |>
    purrr::list_rbind()
}

#' The vocabularies the bundle declared
#'
#' Read from MANIFEST.json rather than restated in this script. The bundle is the authority on which
#' roles it asked for; a second copy here would be a second thing to keep in step, and the failure
#' mode -- a role silently rejected because this file was not updated -- looks exactly like a session
#' that ignored the instructions.
#'
#' @param .dir_bundle Bundle folder.
#' @return Named list: entity label -> character vector of permitted roles.
cwec_roles <- function(.dir_bundle) {
  if (FALSE) .dir_bundle <- .DIR_BUNDLE

  path_ <- fs::path(.dir_bundle, "MANIFEST.json")
  if (!fs::file_exists(path_)) cli::cli_abort("No MANIFEST.json in {(.dir_bundle)}")
  man_ <- jsonlite::fromJSON(path_, simplifyVector = FALSE)
  if (is.null(man_$roles)) cli::cli_abort("MANIFEST.json carries no role vocabulary")
  purrr::map(man_$roles, \(.r) unlist(.r, use.names = FALSE))
}


# 3. Compile ----------------------------------------------------------------------------------------

#' Validate, normalise and split the responses into rule files
#'
#' Every rejection carries a reason and every reason is counted, so a thin rule file is always
#' attributable: a session that skipped a unit and a session whose proposals were all malformed look
#' identical in the output and completely different in the reject table.
#'
#' @param .dir_bundle Bundle folder holding responses/.
#' @param .kinds Permitted values of Kind.
#' @param .sides Permitted values of Side.
#' @param .max_words,.min_chars Normalisation parameters.
#' @param .max_window Widest context window a cue may claim.
#' @return Invisible list with the kept rules and the rejects.
cwec_compile <- function(.dir_bundle, .kinds = .KINDS, .sides = .SIDES,
                         .max_words = 8L, .min_chars = 2L, .max_window = 400L) {
  if (FALSE) {
    .dir_bundle <- .DIR_BUNDLE
    .kinds      <- .KINDS
    .sides      <- .SIDES
    .max_words  <- 8L
    .min_chars  <- 2L
    .max_window <- 400L
  }

  dir_resp_ <- fs::path(.dir_bundle, "responses")
  fils_ <- if (fs::dir_exists(dir_resp_)) fs::dir_ls(dir_resp_, glob = "*.jsonl") else character()
  if (length(fils_) == 0L) {
    cli::cli_alert_warning("No .jsonl found under {(dir_resp_)}; nothing to compile")
    return(invisible(NULL))
  }

  roles_ <- cwec_roles(.dir_bundle)
  raw_   <- purrr::map(fils_, cwec_read_jsonl) |> purrr::list_rbind()

  scored_ <- raw_ |>
    dplyr::mutate(
      Window  = suppressWarnings(as.integer(.data$WindowRaw)),
      Pattern = cwec_normalise(.x = .data$PatternRaw, .max_words = .max_words,
                               .min_chars = .min_chars),
      RoleOk  = purrr::map2_lgl(.data$Label, .data$Role, function(.l, .r) {
        if (is.na(.l) || !.l %in% names(roles_)) return(FALSE)
        identical(.r, "") || .r %in% roles_[[.l]]
      }),
      Reason = dplyr::case_when(
        is.na(.data$Kind) | !.data$Kind %in% .kinds  ~ "unparseable line or unknown Kind",
        !.data$Label %in% names(roles_)              ~ "label absent from this bundle",
        !.data$RoleOk                                ~ "role outside the unit's vocabulary",
        !.data$Side %in% .sides                      ~ "unknown Side",
        .data$Kind == "note"                         ~ NA_character_,
        is.na(.data$Pattern)                         ~ "unusable pattern: empty or too long",
        .data$Kind == "cue" & is.na(.data$Window)    ~ "window is not a number",
        .data$Kind == "cue" & .data$Window > .max_window ~ "window wider than the cap",
        TRUE                                         ~ NA_character_
      )
    )

  ok_ <- scored_ |> dplyr::filter(is.na(.data$Reason))

  notes_ <- ok_ |>
    dplyr::filter(.data$Kind == "note") |>
    dplyr::select(Label, Role, Rationale, Evidence, File, Line)

  rules_ <- ok_ |>
    dplyr::filter(.data$Kind != "note") |>
    dplyr::distinct(Kind, Label, Role, Pattern, Side, .keep_all = TRUE) |>
    dplyr::mutate(
      # A cue that abuts its span needs no window; one that claims none is given a default rather
      # than silently matching at distance zero, which would make it fire almost never.
      Window = dplyr::case_when(
        .data$Kind != "cue"                      ~ 0L,
        is.na(.data$Window) | .data$Window <= 0L ~ 80L,
        TRUE                                     ~ .data$Window
      ),
      Side = dplyr::if_else(.data$Kind == "cue" & .data$Side == "", "either", .data$Side)
    ) |>
    dplyr::arrange(.data$Label, .data$Kind, .data$Role, .data$Pattern) |>
    dplyr::select(Kind, Label, Role, Pattern, Side, Window, PatternRaw, Rationale, Evidence, File)

  rejected_ <- scored_ |> dplyr::filter(!is.na(.data$Reason))
  nDupe_    <- nrow(ok_) - nrow(rules_) - nrow(notes_)

  # Identity-bound cues: a rule keyed on a company's own name learns to recognise the filer rather
  # than a party, which is the bias the anchor contrast introduces. Flag rather than drop -- some
  # are legitimate corporate-form words -- but make them visible before anything is scored on them.
  suspect_ <- rules_ |>
    dplyr::filter(.data$Kind == "cue", .data$Label == "ORG",
                  stringr::str_detect(.data$Pattern,
                                      "\\b(inc|llc|corp|corporation|company|ltd|holdings)\\b"))

  cli::cli_h3("Responses")
  cli::cli_alert_info("{length(fils_)} file{?s}, {nrow(raw_)} proposal{?s}")
  if (nrow(rejected_) > 0L) {
    rejected_ |>
      dplyr::count(.data$Reason, name = "N") |>
      dplyr::arrange(dplyr::desc(.data$N)) |>
      print()
  }
  if (nDupe_ > 0L) cli::cli_alert_info("{nDupe_} duplicate rule{?s} collapsed")
  cli::cli_alert_success(
    "Kept {nrow(rules_)} rule{?s} and {nrow(notes_)} note{?s} across \\
     {dplyr::n_distinct(rules_$Label)} label{?s}"
  )

  cli::cli_h3("Rules by label and kind")
  rules_ |> dplyr::count(.data$Label, .data$Kind, name = "N") |> print(n = Inf)

  thin_ <- rules_ |> dplyr::count(.data$Label, .data$Role, name = "N") |> dplyr::filter(.data$N < 3L)
  if (nrow(thin_) > 0L) {
    cli::cli_alert_info(
      "{nrow(thin_)} role{?s} with fewer than three rules; {?it/they} will not carry a column in 04D:"
    )
    print(thin_, n = Inf)
  }
  if (nrow(suspect_) > 0L) {
    cli::cli_alert_warning(
      "{nrow(suspect_)} ORG cue{?s} mention a corporate form. Check these are context patterns \\
       rather than the filer's own name learned back from the anchor."
    )
  }

  fs::dir_create(fs::path(.dir_bundle, "compiled"))
  arrow::write_parquet(rules_, fs::path(.dir_bundle, "compiled", "entity_rules.parquet"))
  readr::write_csv(rules_,     fs::path(.dir_bundle, "compiled", "entity_rules.csv"))
  readr::write_csv(notes_,     fs::path(.dir_bundle, "compiled", "entity_notes.csv"))
  if (nrow(rejected_) > 0L) {
    rejected_ |>
      dplyr::select(Kind, Label, Role, PatternRaw, Side, WindowRaw, Reason, File, Line) |>
      readr::write_csv(fs::path(.dir_bundle, "compiled", "entity_rejected.csv"))
  }

  cli::cli_alert_info(
    "Rules written to {(fs::path(.dir_bundle, 'compiled'))}. Notes are not machine-applicable and \\
     need reading; the rejects say whether a thin file is a quiet session or a malformed one."
  )
  invisible(list(rules = rules_, notes = notes_, rejected = rejected_))
}


# 4. Run --------------------------------------------------------------------------------------------

if (!fs::dir_exists(.DIR_BUNDLE)) cli::cli_abort("No bundle at {(.DIR_BUNDLE)}")

lst_rules <- cwec_compile(
  .dir_bundle = .DIR_BUNDLE,    # written by 04B-CoWorkEntities.R
  .kinds      = .KINDS,         # cue, stop, section, note
  .sides      = .SIDES,         # where a cue may sit relative to its span
  .max_words  = .MAX_WORDS,     # longest phrase the matcher will look for
  .min_chars  = .MIN_CHARS,     # shortest token retained inside a phrase
  .max_window = .MAX_WINDOW     # widest reach a cue may claim
)

# Recurrence against the prior run, where one was named.
if (!is.null(.DIR_PRIOR) && !is.null(lst_rules$rules)) {
  path_prior_ <- fs::path(.DIR_PRIOR, "compiled", "entity_rules.parquet")
  if (fs::file_exists(path_prior_)) {
    prior_ <- arrow::read_parquet(path_prior_)
    now_   <- lst_rules$rules

    cli::cli_h3("Recurrence against the previous session")
    dplyr::full_join(
      dplyr::distinct(prior_, Label, Kind, Pattern) |> dplyr::mutate(InPrior = TRUE),
      dplyr::distinct(now_,   Label, Kind, Pattern) |> dplyr::mutate(InNow = TRUE),
      by = dplyr::join_by(Label, Kind, Pattern)
    ) |>
      dplyr::mutate(dplyr::across(c(InPrior, InNow), \(.x) tidyr::replace_na(.x, FALSE))) |>
      dplyr::summarise(
        Both       = sum(.data$InPrior & .data$InNow),
        PriorOnly  = sum(.data$InPrior & !.data$InNow),
        NowOnly    = sum(!.data$InPrior & .data$InNow),
        .by = c(Label, Kind)
      ) |>
      dplyr::arrange(.data$Label, .data$Kind) |>
      print(n = Inf)
    cli::cli_alert_info(
      "Both is the recurrence. It is the number to read, not the totals: the two bundles differ in \\
       size, so a rule appearing in one and not the other is as often a sampling difference as a \\
       disagreement. Compare recurrence among the rules the previous run VALIDATED, not among all \\
       of them."
    )
  } else {
    cli::cli_alert_warning("No prior rules at {.path {path_prior_}}; skipping the comparison")
  }
}

cli::cli_h2("Next")
cli::cli_text(
  "04B scores these rules on the held-out fold against the EDGAR anchors: of the spans a rule \\
   keeps, what share are the known entity, and of the known entity's mentions, what share does it \\
   keep. Rules that beat the base rate survive; the rest are dropped mechanically."
)
