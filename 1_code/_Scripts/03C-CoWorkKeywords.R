# 03C-CoWorkKeywords: prepare a CoWork keyword-generation bundle ----
#
# WHAT THIS IS
# A standalone script that turns the labelled sample into self-contained folders a reading session can
# work from. Reading the answers back is a separate concern and lives in 03C-CoWorkCompile.R.
#
# WHY GENERATION RATHER THAN PRUNING
# The miner in 03C can only surface terms that clear min_df and win their greedy
# slot. An agent reading actual contracts is an INDEPENDENT arm: it can propose
# terminology that is rare, or that lost to a longer n-gram, or that a human
# lawyer would name but a frequency table would never isolate. Where the two arms
# agree, that agreement is evidence neither produces alone.
#
# THE FOLD DISCIPLINE THAT MAKES IT HONEST
# The agent reads documents WITH their labels, which is direct supervision. So the
# bundle is drawn from .FOLDS_GENERATE only and .FOLD_HOLDOUT never enters it.
# Evaluate the resulting list on the held-out fold alone and the estimate is
# clean. Scoring it on all five folds would report numbers the agent was taught.
# The script asserts the holdout is absent rather than trusting the filter.
#
# THE CONTRAST IS THE WHOLE DESIGN
# Handing an agent one class of contracts and asking "what characterises these"
# returns "agreement", "party", "shall" -- language common to every contract ever
# filed. A keyword is only useful if it FAILS to fire elsewhere, and that is
# invisible from one class alone. So every unit pairs TARGET documents with OTHER
# documents drawn from the remaining classes and labelled in the filename. Same
# one-vs-rest structure the statistical miner uses, for the same reason.
#
# TERM FORM (the non-obvious constraint)
# The engine tokenises with [a-zA-Z]{3,}, so words under three letters are DROPPED
# before n-grams are formed: "notice of default" becomes the bigram
# "notice default". A term supplied as "notice of default" can therefore never
# match. Rather than burden the agent with that, it writes natural English and
# 03C-CoWorkCompile.R folds each phrase into the analyzer's form on the way back in.
#
# House style: native pipe; explicit package::function; dot-prefixed args;
# underscore-suffixed locals; .data$ for existing columns, bare CamelCase for new;
# if (FALSE) dev blocks; cli/fs/here; pure ASCII; stringi::stri_sub never base
# substr; {(.arg)} parens in cli interpolation.


# 1. Configuration --------------------------------------------------------
# Point .DIR_BUNDLE somewhere OUTSIDE the repo (Desktop, Dropbox). The blindness
# constraint is not enforceable by instruction -- CoWork has file access, so
# "please do not read runs/" is a request. A folder that does not contain runs/
# is a guarantee.

.PATH_PREPARED  <- here::here("2_output", "03A-ClassifyPrepare", "prepared.parquet")

# The bundle lives in the shared project folder rather than the repository. The reading session has
# file access, so an instruction not to open 2_output is a request; a folder that does not contain it
# is a guarantee. 03C-CoWorkCompile.R and 03C-ClassifyTrainKeyword.qmd read from this same root.
.DIR_ROOT       <- fs::path("/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts",
                            "MatContractData", "KeyWordsClaude")

# One bundle per task. ClassBroad is NOT redundant with ClassDetailed: mining is
# one-vs-rest at whatever level it runs, so a term shared evenly by three detailed
# children of one broad parent scores precision ~1/3 against each child and dies,
# despite being a clean marker of the parent. Those terms are only reachable by
# working at the broad level directly.
#
# AmendType gets ONE unit. "Original" is defined by the ABSENCE of amendment
# language, so asking what characterises originals is the wrong question -- the
# same asymmetry that made the engine mine only the positive class.
.TASKS <- tibble::tibble(
  LabelCol = c("ClassDetailed", "ClassBroad", "AmendType"),
  Classes  = list(NULL, NULL, "Amended"),
  Slug     = c("detailed", "broad", "amendment")
)

.FOLDS_GENERATE <- 1:4               # folds the agent may read
.FOLD_HOLDOUT   <- 5L                # fold reserved for evaluation; never bundled

.N_TARGET       <- 16L               # in-class documents per unit
.N_OTHER        <- 16L               # out-of-class documents per unit
.N_WORDS        <- 512L              # truncation; match the mining window
.N_TERMS_ASK    <- 40L               # terms requested per unit
.SEED           <- 42L


# 2. Helpers --------------------------------------------------------------

# Null-coalescing: a response line may omit an optional field entirely.
`%||%` <- function(.x, .y) if (is.null(.x)) .y else .x

#' Filesystem-safe slug from a class label
#'
#' Class names carry spaces, ampersands and slashes ("R&D", "Employment -
#' Compensation"), none of which belong in a path.
#'
#' @param .x Character vector.
#' @return Lowercase hyphenated ASCII slug.
cowork_slug <- function(.x) {
  .x |>
    stringi::stri_trans_tolower() |>
    stringi::stri_replace_all_regex("[^a-z0-9]+", "-") |>
    stringi::stri_replace_all_regex("^-+|-+$", "")
}

#' Truncate text to the first N whitespace words
#'
#' Mirrors the engine's --nwords exactly, so the agent reads what the classifier
#' reads: a term it proposes is one that could actually fire.
#'
#' @param .x Character vector of document text.
#' @param .n_words Integer word budget.
#' @return Character vector.
cowork_truncate <- function(.x, .n_words = 512L) {
  toks_ <- stringi::stri_split_regex(.x, "\\s+", omit_empty = TRUE)
  purrr::map_chr(toks_, function(.t) {
    paste(utils::head(.t, .n_words), collapse = " ")
  })
}

#' Draw out-of-class documents spread evenly over the remaining classes
#'
#' Proportional sampling would fill OTHER with the largest classes and never show
#' the agent a thin one, so the contrast would miss exactly the confusions that
#' matter. This deals round-robin across the other classes instead.
#'
#' @param .tab Candidate pool (already restricted to the generation folds).
#' @param .class Target class to exclude.
#' @param .n Documents wanted.
#' @param .label_col Label column name.
#' @return Tibble of sampled rows.
cowork_sample_other <- function(.tab, .class, .n, .label_col) {
  if (FALSE) {
    .tab       <- pool
    .class     <- "Lease"
    .n         <- 16L
    .label_col <- "ClassDetailed"
  }
  others_ <- .tab |> dplyr::filter(.data[[.label_col]] != .class)
  if (nrow(others_) == 0L) return(others_)

  shuffled_ <- others_ |>
    dplyr::slice_sample(prop = 1) |>
    dplyr::mutate(Slot = dplyr::row_number(), .by = dplyr::all_of(.label_col)) |>
    dplyr::arrange(.data$Slot)

  shuffled_ |> utils::head(.n) |> dplyr::select(-Slot)
}

#' Write one document set to disk as plain text
#'
#' @param .tab Rows to write.
#' @param .dir Destination directory.
#' @param .label_col Label column; NULL omits the label from the filename.
#' @return Invisible count written.
cowork_write_docs <- function(.tab, .dir, .label_col = NULL) {
  fs::dir_create(.dir)
  purrr::walk(seq_len(nrow(.tab)), function(.i) {
    stem_ <- if (is.null(.label_col)) {
      sprintf("%03d_%s.txt", .i, .tab$DocID[[.i]])
    } else {
      sprintf("%03d_%s_%s.txt", .i,
              cowork_slug(.tab[[.label_col]][[.i]]), .tab$DocID[[.i]])
    }
    writeLines(.tab$TextCut[[.i]], fs::path(.dir, stem_), useBytes = TRUE)
  })
  invisible(nrow(.tab))
}


# 3. The agent-facing documents -------------------------------------------

#' The pinned prompt written into the bundle root
#'
#' Deliberately says nothing about model performance, precision targets or which
#' terms the statistical miner found. The agent's judgement has to be independent
#' of the miner's or the two arms stop being independent.
#'
#' @param .classes Character vector of class names in the bundle.
#' @param .n_terms Terms requested per unit.
#' @param .n_words Truncation used.
#' @param .label_col Task, which swaps the framing paragraph.
#' @return Character vector of markdown lines.
cowork_instructions <- function(.classes, .n_terms, .n_words,
                                .label_col = "ClassDetailed") {
  # Single quotes keep the JSON example free of backslash escaping, which otherwise renders the
  # source line unreadable and pushes it past the margin.
  example_ <- paste0(
    '{"Class": "Lease", "Term": "quiet enjoyment", ',
    '"Rationale": "Standard covenant in leases; absent from every OTHER document.", ',
    '"Evidence": "TARGET/004_0001104659-14-012345-1.txt"}'
  )

  amend_ <- identical(.label_col, "AmendType")

  # The amendment task is not "what type of contract is this" -- it is "does this
  # document modify an earlier one". The trap is worth naming explicitly: the
  # phrase "amended and restated" is a TITLE CONVENTION that appears on plenty of
  # genuine originals, which is precisely why a keyword approach lost this task to
  # the transformer. An agent told this up front can look for the structural
  # markers instead (references to a prior dated agreement, section-level edits).
  framing_ <- if (amend_) {
    c(
      "## What separates an amendment from an original",
      "",
      "This unit is not about contract TYPE. It is about whether a document",
      "MODIFIES an earlier contract.",
      "",
      "One warning, because it is the trap that defeats naive keyword lists:",
      "**\"amended and restated\" is a title convention** and appears on many",
      "genuine originals. Do not propose it, or its close variants, on the",
      "strength of the phrase alone -- check the OTHER set and you will find it",
      "there too.",
      "",
      "Look instead for structural markers of modification: references to a",
      "previously dated agreement between the same parties, language that strikes",
      "or replaces numbered sections, statements that remaining terms continue",
      "unchanged, recitals describing what the parties now wish to change.",
      ""
    )
  } else {
    character()
  }

  c(
    "# Keyword generation from labelled contracts",
    "",
    "You are working inside this folder. Everything you need is here.",
    "",
    "## The task",
    "",
    "Each folder under `units/` is one contract type. Each contains two sets of",
    "real SEC Exhibit-10 contracts:",
    "",
    "- `TARGET/` -- contracts OF that type",
    "- `OTHER/`  -- contracts of OTHER types (the type is in the filename)",
    "",
    sprintf("Documents are truncated to their first %d words, which is what the", .n_words),
    "downstream classifier reads.",
    "",
    "For each unit: read both sets, then propose terms that appear in TARGET and",
    "NOT in OTHER.",
    "",
    framing_,
    "## The OTHER set is the point",
    "",
    "A term that appears in both sets is useless however central it seems.",
    "\"agreement\", \"party\", \"shall\", \"hereby\", \"witnesseth\" appear in every",
    "contract ever filed. They are not keywords. The OTHER set exists precisely",
    "so you can rule them out -- check every candidate against it before writing",
    "it down.",
    "",
    "## Be generous, not precise",
    "",
    sprintf("Propose about %d terms per unit. Do not self-censor.", .n_terms),
    "",
    "Every term you propose is afterwards measured against thousands of labelled",
    "contracts: how often it fires, how often it fires in the right class, and",
    "whether that survives on documents nobody showed you. Terms that fail are",
    "dropped mechanically and cost nothing. Terms you never proposed cannot be",
    "recovered. If a phrase looks characteristic, propose it -- uncertainty is",
    "handled downstream, not by you.",
    "",
    "## Form of a term",
    "",
    "1. One to three words.",
    "2. Letters only -- no digits, no punctuation, no hyphens, no apostrophes.",
    "3. Lowercase.",
    "4. Prefer phrases to single words: \"revolving credit facility\" beats \"credit\".",
    "5. Write natural English. Short connecting words (\"of\", \"to\", \"be\") are",
    "   removed automatically, so \"notice of default\" is fine to write.",
    "",
    "Do NOT propose:",
    "",
    "- company, person or place names, however reliably they appear",
    "- dates, dollar amounts, section or exhibit numbers",
    "- terms you did not actually see in the TARGET documents",
    "",
    "## What to write",
    "",
    "For each unit, write ONE file into `responses/` named after the unit folder",
    "with the extension `.jsonl` (for example `responses/01-lease.jsonl`).",
    "",
    "One JSON object per line, no wrapping array, no markdown fences:",
    "",
    "```",
    example_,
    "```",
    "",
    "Fields:",
    "",
    "- `Class`     -- exactly the class name given in the unit's `UNIT.md`",
    "- `Term`      -- the proposed term",
    "- `Rationale` -- one sentence, why this discriminates",
    "- `Evidence`  -- the filename of one TARGET document where you saw it",
    "",
    "`Evidence` is not bookkeeping. Naming the document forces the term to come",
    "from the contract text in front of you rather than from general intuition",
    "about what a contract of this type probably says.",
    "",
    "## Order of work",
    "",
    "Do one unit at a time and finish its response file before starting the next.",
    "The units are independent, so the session can be stopped and resumed.",
    "",
    "## Units in this bundle",
    "",
    paste0("- ", .classes)
  )
}

#' Per-unit README written beside the two document folders
#'
#' @param .class Class name.
#' @param .n_target,.n_other Document counts actually written.
#' @param .n_terms Terms requested.
#' @param .slug Unit slug (drives the response filename).
#' @return Character vector of markdown lines.
cowork_unit_readme <- function(.class, .n_target, .n_other, .n_terms, .slug) {
  c(
    paste0("# Unit: ", .class),
    "",
    paste0("- `TARGET/` -- ", .n_target, " contracts of type **", .class, "**"),
    paste0("- `OTHER/`  -- ", .n_other, " contracts of other types (type in filename)"),
    "",
    paste0("Propose about ", .n_terms, " terms that appear in TARGET and not in OTHER."),
    "",
    paste0("Write your answer to `responses/", .slug, ".jsonl`."),
    paste0("Use exactly this string in the `Class` field: `", .class, "`"),
    "",
    "See `INSTRUCTIONS.md` in the bundle root for the term rules and format."
  )
}


# 4. Build the bundle -----------------------------------------------------

#' Write the complete CoWork generation bundle
#'
#' @param .path_prepared 03A prepared parquet.
#' @param .dir_bundle Destination folder (created; must be empty or absent).
#' @param .label_col Label column driving the units.
#' @param .classes Character vector restricting which classes get a unit
#'   (NULL = all). Used to skip the negative class on asymmetric tasks.
#' @param .folds_generate Folds the agent may read.
#' @param .fold_holdout Fold reserved for evaluation.
#' @param .n_target,.n_other Documents per unit.
#' @param .n_words Truncation.
#' @param .n_terms Terms requested per unit.
#' @param .seed Sampling seed (recorded in the manifest).
#' @return Invisible tibble: one row per unit with the counts actually written.
cowork_write_bundle <- function(.path_prepared, .dir_bundle,
                                .label_col = "ClassDetailed",
                                .classes = NULL,
                                .folds_generate = 1:4,
                                .fold_holdout = 5L,
                                .n_target = 16L, .n_other = 16L,
                                .n_words = 512L, .n_terms = 40L,
                                .seed = 42L) {
  if (FALSE) {
    .path_prepared  <- .PATH_PREPARED
    .dir_bundle     <- .DIR_BUNDLE
    .label_col      <- "ClassDetailed"
    .folds_generate <- 1:4
    .fold_holdout   <- 5L
    .n_target       <- 16L
    .n_other        <- 16L
    .n_words        <- 512L
    .n_terms        <- 40L
    .seed           <- 42L
  }
  if (!fs::file_exists(.path_prepared)) {
    cli::cli_abort("No prepared sample at {(.path_prepared)} -- run 03A first")
  }
  if (fs::dir_exists(.dir_bundle) && length(fs::dir_ls(.dir_bundle)) > 0L) {
    cli::cli_abort("{(.dir_bundle)} is not empty -- delete it or choose another path")
  }
  set.seed(.seed)

  tab_ <- arrow::read_parquet(.path_prepared)
  need_ <- c("DocID", "Text", "Fold", .label_col)
  miss_ <- setdiff(need_, names(tab_))
  if (length(miss_) > 0L) cli::cli_abort("Prepared sample missing: {miss_}")

  pool_ <- tab_ |>
    dplyr::filter(!is.na(.data[[.label_col]]), .data$Fold %in% .folds_generate) |>
    dplyr::filter(!is.na(.data$Text), trimws(.data$Text) != "") |>
    dplyr::mutate(TextCut = cowork_truncate(.data$Text, .n_words))

  # trust nothing: assert the held-out fold is genuinely absent
  if (any(pool_$Fold == .fold_holdout)) {
    cli::cli_abort("Holdout fold {(.fold_holdout)} leaked into the pool -- aborting")
  }
  cli::cli_alert_info(
    "Pool: {nrow(pool_)} documents from folds {paste(.folds_generate, collapse = ', ')} \\
     (fold {(.fold_holdout)} held out)"
  )

  classes_ <- sort(unique(pool_[[.label_col]]))
  if (!is.null(.classes)) {
    unknown_ <- setdiff(.classes, classes_)
    if (length(unknown_) > 0L) cli::cli_abort("Unknown class{?es}: {unknown_}")
    classes_ <- .classes
    cli::cli_alert_info("Restricted to {length(classes_)} class{?es}: {classes_}")
  }
  fs::dir_create(fs::path(.dir_bundle, "units"))
  fs::dir_create(fs::path(.dir_bundle, "responses"))

  out_ <- purrr::map(seq_along(classes_), function(.i) {
    class_ <- classes_[[.i]]
    slug_  <- sprintf("%02d-%s", .i, cowork_slug(class_))
    dir_u_ <- fs::path(.dir_bundle, "units", slug_)

    target_ <- pool_ |>
      dplyr::filter(.data[[.label_col]] == class_) |>
      dplyr::slice_sample(n = min(.n_target, sum(pool_[[.label_col]] == class_)))
    other_ <- cowork_sample_other(pool_, class_, .n_other, .label_col)

    cowork_write_docs(target_, fs::path(dir_u_, "TARGET"))
    cowork_write_docs(other_, fs::path(dir_u_, "OTHER"), .label_col = .label_col)

    writeLines(
      cowork_unit_readme(class_, nrow(target_), nrow(other_), .n_terms, slug_),
      fs::path(dir_u_, "UNIT.md")
    )

    if (nrow(target_) < .n_target) {
      cli::cli_alert_warning(
        "{class_}: only {nrow(target_)} documents available (asked {(.n_target)})"
      )
    }
    tibble::tibble(Unit = slug_, Class = class_,
                   NTarget = nrow(target_), NOther = nrow(other_))
  }) |>
    purrr::list_rbind()

  writeLines(
    cowork_instructions(classes_, .n_terms, .n_words, .label_col),
    fs::path(.dir_bundle, "INSTRUCTIONS.md")
  )

  jsonlite::write_json(
    list(
      bundle_version = "generation_v1",
      created_at     = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      prepared_path  = as.character(.path_prepared),
      label_col      = .label_col,
      classes        = classes_,
      folds_generate = .folds_generate,
      fold_holdout   = .fold_holdout,
      n_target       = .n_target,
      n_other        = .n_other,
      n_words        = .n_words,
      n_terms_asked  = .n_terms,
      seed           = .seed,
      n_pool         = nrow(pool_),
      units          = out_,
      r_version      = paste(R.version$major, R.version$minor, sep = ".")
    ),
    fs::path(.dir_bundle, "MANIFEST.json"), auto_unbox = TRUE, pretty = TRUE
  )

  cli::cli_alert_success(
    "Bundle written: {nrow(out_)} units, {sum(out_$NTarget) + sum(out_$NOther)} documents"
  )
  cli::cli_alert_info("Point CoWork at {(.dir_bundle)} and start with INSTRUCTIONS.md")
  invisible(out_)
}


# 5. Run --------------------------------------------------------------------------------------------
# Idempotent: a bundle already written is left alone, so re-sourcing after a partial session is safe.
# Reading the responses back is a separate concern and lives in 03C-CoWorkCompile.R.

purrr::pwalk(.TASKS, function(LabelCol, Classes, Slug) {
  dir_ <- fs::path(.DIR_ROOT, Slug)
  cli::cli_h2("{LabelCol}")

  if (fs::dir_exists(dir_) && length(fs::dir_ls(dir_)) > 0L) {
    cli::cli_alert_info("Bundle already written at {dir_}; leaving it alone")
    return(invisible(NULL))
  }
  cowork_write_bundle(
    .path_prepared  = .PATH_PREPARED,    # sample written by 03A
    .dir_bundle     = dir_,              # one self-contained folder per task
    .label_col      = LabelCol,          # task driving the units
    .classes        = Classes,           # NULL keeps every class
    .folds_generate = .FOLDS_GENERATE,   # folds the session may read
    .fold_holdout   = .FOLD_HOLDOUT,     # never bundled; the honest estimate comes from here
    .n_target       = .N_TARGET,         # in-class documents per unit
    .n_other        = .N_OTHER,          # contrast documents per unit
    .n_words        = .N_WORDS,          # truncation, matching the mined arm
    .n_terms        = .N_TERMS_ASK,      # terms requested per unit
    .seed           = .SEED              # recorded in the manifest
  )
})

cli::cli_h2("Next")
cli::cli_text(
  "Point the reading session at {(.DIR_ROOT)} and start from each bundle's INSTRUCTIONS.md. \\
   When the responses are written, 03C-CoWorkCompile.R turns them into term files."
)
