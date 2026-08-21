# 03C-ClassifyTrainKeyword: the keyword table (kw_*) -------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# The deliverable of this stage is an artifact rather than a model: a short, ranked, human-readable
# table of terms per contract type that can be applied to EDGAR without a GPU, carrying a stated
# precision and a stated coverage. Every design decision below follows from that goal, and where a
# choice would have gone the other way had the goal been accuracy, the roxygen says so.
#
# THE SEAM
# Mining is expensive, selection is cheap. The Python engine tokenises the sample and writes
# per-term-per-category statistics plus incidence; every floor, the greedy rule, the decision rule and
# the thresholds live here in R. That split keeps the mining grid at a few hundred cells while leaving
# the selection sweep interactive, and it keeps the engine task-agnostic: the amendment decision rule
# is a decision, so it belongs on the R side.
#
# POWER
# The Wilson 95 percent lower bound on a term's training precision. Bounded in [0, 1] and monotone in
# both precision and evidence: a term seen twice in two documents scores 0.342, one seen 190 times in
# 200 scores 0.910, where raw precision would rank the first ahead of the second. Power is the sort
# key of the published table and the evidence gate at scoring time. It is NOT the precision gate.
#
# ABSTENTION
# Unlike the transformer, this classifier declines to label a document no term reaches, emitting the
# sentinel KW_NONE. That is the mechanism by which a stated precision is possible at all, and it is
# why the shared scoring layer is abstention-aware: an abstained document is a false negative for its
# true category and a false positive for nothing, so abstaining costs recall and protects precision.
# The sentinel is not a category, so figures pass it as an extra level rather than registering it.
#
# WHAT IS NOT HERE
# Sample construction, the scoring layer and the category vocabulary: those are 03A, which the runbook
# sources first, so this arm and the transformer arm are scored by identical code on identical folds.
# The look of any figure or table: that is _Commons/_Plots.R and _Commons/_Tables.R. Nothing below
# sets a colour, a font or a height.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; if (FALSE) dev blocks; pure ASCII; parenthesised cli interpolation.

KW_NONE <- "(none)"

`%||%` <- function(.x, .y) if (is.null(.x)) .y else .x


# 1. Engine seam: mining commands, the sweep, and the index ------------------------------------------------------------

#' Build the argument vector for one mining cell
#'
#' Pure: validates and returns the command, with no side effects, so the sequential and parallel
#' paths construct byte-identical calls and a failed cell can be reproduced by printing it.
#'
#' Test fold 0 mines every labelled row and holds nothing out. Its lexicon becomes the published
#' table, mirroring the convention used for the transformer: the folds pay for the honest estimate,
#' an all-data fit produces the deployed artifact.
#'
#' @param .path_data Prepared parquet written by 03A.
#' @param .label_col Label column to mine.
#' @param .source Field to mine: document body or filer title.
#' @param .test_fold Held-out fold; 0 mines all labelled rows.
#' @param .nwords Truncate the body to the first N whitespace words; 0 keeps the whole document.
#' @param .ngram_max Longest n-gram mined.
#' @param .stopwords Stopword regime. Under an accuracy objective "none" wins, because function-word
#'   collocations are discriminative. Under a readability objective it loses, because "borrower any
#'   its" cannot be published. Both are swept and judged on the terms produced.
#' @param .min_df,.max_df Vocabulary pruning bounds; max_df removes corpus-wide boilerplate.
#' @param .min_token_len Shortest alphabetic token kept.
#' @param .power_floor Permissive engine-side pre-filter; the binding floors are applied in R.
#' @param .max_candidates Candidate cap per class. Bounds artifact size only.
#' @param .terms_file Optional (Class, Term) file; supplying it scores that list instead of mining.
#' @param .seed Stamped for parity across methods; mining itself is deterministic.
#' @param .mines_root Directory under which mine folders are written.
#' @param .python,.script Interpreter and miner paths.
#' @param .overwrite Re-mine cells already present on disk.
#' @param .smoke Tiny subsample, for checking the seam rather than producing results.
#' @return List with elements `python` and `args`.
kw_mine_command <- function(.path_data,
                            .label_col      = c("ClassDetailed", "ClassBroad", "AmendType"),
                            .source         = c("text", "docdesc"),
                            .test_fold      = 1L,
                            .nwords         = 0L,
                            .ngram_max      = 3L,
                            .stopwords      = c("none", "english_domain", "english", "domain"),
                            .min_df         = 3L,
                            .max_df         = 0.5,
                            .min_token_len  = 3L,
                            .power_floor    = 0.30,
                            .max_candidates = 300L,
                            .terms_file     = NULL,
                            .seed           = 42L,
                            .mines_root     = NULL,
                            .python         = NULL,
                            .script         = NULL,
                            .overwrite      = FALSE,
                            .smoke          = FALSE) {
  if (FALSE) {
    .path_data  <- .lP$Input$Prepared
    .label_col  <- "ClassDetailed"
    .source     <- "text"
    .test_fold  <- 1L
    .nwords     <- 256L
    .mines_root <- .lP$Output$Mines
    .python     <- .lP$Engine$Python
    .script     <- .lP$Engine$Script
  }
  .label_col <- match.arg(.label_col)
  .source    <- match.arg(.source)
  .stopwords <- match.arg(.stopwords)

  args_ <- c(
    .script,
    "--data",            .path_data,
    "--label-col",       .label_col,
    "--source",          .source,
    "--test-fold",       as.character(.test_fold),
    "--nwords",          as.character(.nwords),
    "--ngram-max",       as.character(.ngram_max),
    "--stopwords",       .stopwords,
    "--min-df",          as.character(.min_df),
    "--max-df",          as.character(.max_df),
    "--min-token-len",   as.character(.min_token_len),
    "--power-floor",     as.character(.power_floor),
    "--max-candidates",  as.character(.max_candidates),
    "--seed",            as.character(.seed),
    "--runs-root",       .mines_root
  )
  if (!is.null(.terms_file)) args_ <- c(args_, "--terms-file", .terms_file)
  if (.overwrite)            args_ <- c(args_, "--overwrite")
  if (.smoke)                args_ <- c(args_, "--smoke")

  list(python = .python, args = args_)
}

#' Run one mining cell in the foreground
#'
#' For smoke checks and for scoring a supplied term list, where the parallel path adds nothing.
#'
#' @param .path_data Prepared parquet.
#' @param ... Passed to kw_mine_command.
#' @param .python,.script Interpreter and miner paths.
#' @return Exit status, invisibly.
kw_mine <- function(.path_data, ..., .python = NULL, .script = NULL) {
  if (FALSE) {
    .path_data <- .lP$Input$Prepared
    .python    <- .lP$Engine$Python
    .script    <- .lP$Engine$Script
  }
  if (!fs::file_exists(.python)) cli::cli_abort("Python not found at {(.python)}")
  if (!fs::file_exists(.script)) cli::cli_abort("Miner not found at {(.script)}")

  cmd_    <- kw_mine_command(.path_data, ..., .python = .python, .script = .script)
  status_ <- system2(cmd_$python, args = cmd_$args, stdout = "", stderr = "")
  if (!identical(status_, 0L)) cli::cli_abort("Keyword miner failed (exit {status_})")
  invisible(status_)
}

#' Build the mining grid
#'
#' The filer title is a single short field, so the truncation axis does not apply to it and the
#' docdesc arm gets one row per (task, n-gram, fold) rather than one per window.
#'
#' @param .label_cols Tasks to mine.
#' @param .nwords_text Word windows for the body arm; 0 is the whole document.
#' @param .ngram_max Longest n-gram(s) to try.
#' @param .stopwords Stopword regimes to cross.
#' @param .folds Held-out folds.
#' @param .with_alldata Append the fold-0 all-data mine that produces the published table.
#' @return Tibble with columns LabelCol, Source, NWords, NgramMax, Stopwords, Fold.
kw_mine_grid <- function(.label_cols   = c("ClassDetailed", "ClassBroad", "AmendType"),
                         .nwords_text  = c(256L, 512L, 1024L, 2048L, 0L),
                         .ngram_max    = c(2L, 3L),
                         .stopwords    = c("none", "english_domain"),
                         .folds        = 1:5,
                         .with_alldata = TRUE) {
  if (FALSE) {
    .label_cols   <- "ClassDetailed"
    .nwords_text  <- c(256L, 512L)
    .ngram_max    <- 3L
    .stopwords    <- "none"
    .folds        <- 1:5
    .with_alldata <- TRUE
  }
  folds_ <- if (.with_alldata) c(.folds, 0L) else .folds

  dplyr::bind_rows(
    tidyr::expand_grid(
      LabelCol = .label_cols, Source = "text", NWords = .nwords_text,
      NgramMax = .ngram_max, Stopwords = .stopwords, Fold = folds_
    ),
    tidyr::expand_grid(
      LabelCol = .label_cols, Source = "docdesc", NWords = 0L,
      NgramMax = .ngram_max, Stopwords = .stopwords, Fold = folds_
    )
  )
}

#' Run the mining grid in parallel
#'
#' Idempotent: the engine skips any cell whose termstats file already exists, so re-running the
#' document costs seconds and reproduces identical output. That is what makes it affordable to run
#' this chunk on every render rather than guarding it.
#'
#' Because the mine name encodes hyperparameters and not a hash of the data, changing the sample
#' without clearing the mines directory would serve stale results silently. The document states this
#' in its Configuration section.
#'
#' @param .grid Grid from kw_mine_grid.
#' @param .path_data Prepared parquet.
#' @param .mines_root Mines directory.
#' @param .python,.script Interpreter and miner paths.
#' @param .workers Daemons to run; NULL uses all cores but two.
#' @param .overwrite Re-mine cells already on disk.
#' @param ... Passed to kw_mine_command.
#' @return Mine index for the whole directory, invisibly.
kw_mine_sweep <- function(.grid, .path_data, .mines_root, .python, .script,
                          .workers = NULL, .overwrite = FALSE, ...) {
  if (FALSE) {
    .grid       <- kw_mine_grid(.label_cols = "ClassDetailed", .nwords_text = 512L)
    .path_data  <- .lP$Input$Prepared
    .mines_root <- .lP$Output$Mines
    .python     <- .lP$Engine$Python
    .script     <- .lP$Engine$Script
    .workers    <- 24L
    .overwrite  <- FALSE
  }
  if (!fs::file_exists(.python)) cli::cli_abort("Python not found at {(.python)}")
  if (!fs::file_exists(.script)) cli::cli_abort("Miner not found at {(.script)}")

  need_ <- c("LabelCol", "Source", "NWords", "NgramMax", "Stopwords", "Fold")
  miss_ <- setdiff(need_, names(.grid))
  if (length(miss_) > 0L) cli::cli_abort("Grid missing columns: {miss_}")

  if (is.null(.workers)) .workers <- as.integer(max(1L, parallel::detectCores() - 2L))

  cmds_ <- purrr::map(seq_len(nrow(.grid)), function(.i) {
    kw_mine_command(
      .path_data  = .path_data,
      .label_col  = .grid$LabelCol[[.i]],
      .source     = .grid$Source[[.i]],
      .test_fold  = .grid$Fold[[.i]],
      .nwords     = .grid$NWords[[.i]],
      .ngram_max  = .grid$NgramMax[[.i]],
      .stopwords  = .grid$Stopwords[[.i]],
      .mines_root = .mines_root,
      .python     = .python,
      .script     = .script,
      .overwrite  = .overwrite,
      ...
    )
  })

  cli::cli_alert_info("Dispatching {length(cmds_)} mine cells across {(.workers)} daemons")
  t0_ <- Sys.time()

  mirai::daemons(.workers)
  on.exit(mirai::daemons(0L), add = TRUE)

  res_  <- mirai::mirai_map(cmds_, \(cmd) system2(cmd$python, cmd$args, stdout = FALSE, stderr = FALSE))[.progress]
  ok_   <- purrr::map_lgl(res_, \(s_) !inherits(s_, "miraiError") && identical(as.integer(s_), 0L))
  mins_ <- round(as.numeric(difftime(Sys.time(), t0_, units = "mins")), 1)

  if (any(!ok_)) {
    cli::cli_alert_warning("{sum(!ok_)} of {length(cmds_)} cells failed; each cell's run.log holds the reason")
  }
  cli::cli_alert_success("Mining complete: {sum(ok_)}/{length(cmds_)} cells in {mins_} min")

  invisible(kw_mine_index(.mines_root = .mines_root))
}

#' Index every mine on disk
#'
#' Reads manifests rather than parsing directory names, so a change to the naming scheme cannot
#' silently mislabel a run.
#'
#' MINED CELLS BY DEFAULT. The manual mines written for the generated arm sit under the same root and
#' carry the same axes -- task, source, window, n-gram order, stopword regime -- so any filter naming
#' those axes and not the mode collects both. That costs nothing on a fresh tree, because the manual
#' mines do not exist at the moment the index is built, and it silently changes the published tables
#' on every render after the first, when they do. A bug that appears only on the second run is one
#' that appears only in someone else's hands. The default is therefore the safe set, and a caller
#' wanting the written-list mines has to name them.
#'
#' @param .mines_root Mines directory.
#' @param .mode Mine mode to return: "mine", "manual", or NULL for both.
#' @return Tibble, one row per mine, ordered by task then configuration.
kw_mine_index <- function(.mines_root, .mode = "mine") {
  if (FALSE) {
    .mines_root <- .lP$Output$Mines
    .mode       <- "mine"
  }
  paths_ <- fs::dir_ls(.mines_root, recurse = TRUE, glob = "*mine.json")
  paths_ <- paths_[!grepl("_smoke", paths_, fixed = TRUE)]
  if (length(paths_) == 0L) cli::cli_abort("No mine.json found under {(.mines_root)}")

  idx_ <- purrr::map(paths_, function(.p) {
    m_ <- jsonlite::read_json(.p, simplifyVector = TRUE)
    tibble::tibble(
      MineDir   = as.character(fs::path_dir(.p)),
      MineName  = m_$mine_name,
      Mode      = m_$mode,
      LabelCol  = m_$label_col,
      Source    = m_$source,
      NWords    = as.integer(m_$nwords),
      NgramMax  = as.integer(m_$ngram_range[[2]]),
      Stopwords = m_$stopwords %||% "none",
      # Read rather than assumed. It is never swept, which is exactly why assuming it is dangerous:
      # a value nobody varies is a value nobody notices changing, and the applier has to reproduce
      # the tokenisation the mine was built under or the lexicon fires on different documents.
      MinTokenLen = as.integer(m_$min_token_len %||% 3L),
      TermsFile = m_$terms_file %||% NA_character_,
      TermsHash = m_$terms_hash %||% NA_character_,
      Fold      = as.integer(m_$test_fold),
      nTrain    = as.integer(m_$n_train),
      nTest     = as.integer(m_$n_test),
      nVocab    = as.integer(m_$n_vocab),
      nCand     = as.integer(m_$n_candidates),
      MineSec   = as.numeric(m_$duration_sec)
    )
  }) |>
    purrr::list_rbind()

  # Explicit rather than a recycled condition inside filter(). This function is the one that got the
  # mode wrong; it is not the place to be clever about how the correction is expressed.
  if (!is.null(.mode)) idx_ <- idx_ |> dplyr::filter(.data$Mode %in% .mode)

  idx_ |>
    dplyr::arrange(.data$LabelCol, .data$Source, .data$Stopwords, .data$NWords, .data$NgramMax,
                   .data$MinTokenLen, .data$Fold)
}

#' Load one mine's artifacts
#'
#' @param .mine_dir Directory of a single mine.
#' @return List with TermStats, IncTrain, IncTest, DocsTrain, DocsTest, Manifest. The test elements
#'   are NULL for an all-data mine, which has no held-out fold.
kw_load_mine <- function(.mine_dir) {
  if (FALSE) .mine_dir <- idx_mine$MineDir[[1]]

  opt_ <- function(.f) {
    p_ <- fs::path(.mine_dir, .f)
    if (fs::file_exists(p_)) arrow::read_parquet(p_) else NULL
  }
  list(
    TermStats = arrow::read_parquet(fs::path(.mine_dir, "termstats.parquet")),
    IncTrain  = arrow::read_parquet(fs::path(.mine_dir, "incidence_train.parquet")),
    IncTest   = opt_("incidence_test.parquet"),
    DocsTrain = arrow::read_parquet(fs::path(.mine_dir, "docs_train.parquet")),
    DocsTest  = opt_("docs_test.parquet"),
    Manifest  = jsonlite::read_json(fs::path(.mine_dir, "mine.json"), simplifyVector = TRUE)
  )
}


# 2. Selection: from candidate statistics to a lexicon -----------------------------------------------------------------

#' Select a minimal non-redundant lexicon from one mine
#'
#' Four gates and one greedy pass, each answering a different question.
#'
#' PRECISION is the promise the table makes, and it is deliberately separate from the Power floor.
#' Setting precision to 1 in the Wilson formula leaves Power_max(n) = n / (n + 3.84), so Power carries
#' a support-dependent ceiling: a perfect term needs 44 hits to reach 0.92 at all. A category holding
#' 71 documents therefore has no term that can clear a high global Power threshold, however clean.
#' Thresholding on Power alone selects for class size rather than quality and empties every thin
#' category by arithmetic. Precision has no such ceiling; Power stays on as the evidence gate.
#'
#' FILER DIVERSITY catches a term concentrated in one registrant's own template language. It does not
#' catch a counterparty name, which is spread across the many suppliers that contract with that
#' counterparty; nothing available here does, because the text is lowercased before mining and the
#' one signal identifying a proper noun is gone before any statistic sees the term.
#'
#' REPEATED TOKENS arise when stopword removal collides across the gap: "employment agreement (this
#' Agreement)" mines as "employment agreement agreement". Accurate, and unreadable.
#'
#' GREEDY MARGINAL REACH accepts a term only where it catches class documents no accepted term
#' caught. One rule subsumes nested n-grams, near-synonyms and boilerplate variants, and because it
#' works on document overlap rather than surface form it also removes redundancy no string rule sees.
#'
#' @param .mine Loaded mine from kw_load_mine.
#' @param .min_precision Minimum training precision; the promise the table makes.
#' @param .min_hits Minimum positive-class training hits.
#' @param .min_tot Minimum total training hits.
#' @param .min_reach Minimum marginal reach for acceptance, as a share of the class.
#' @param .max_terms Cap on accepted terms per class. Readability, not accuracy.
#' @param .min_filers Minimum distinct registrants a term must span; 0 disables.
#' @param .min_filer_ratio Minimum NFilers / HitsPos. Scale-free, and the sharper of the two.
#' @param .drop_repeats Drop n-grams containing a repeated token.
#' @param .filer_pattern Regex extracting the registrant identifier from DocID.
#' @return Tibble of accepted terms, one row per (class, term), ordered by class then Power.
kw_select <- function(.mine,
                      .min_precision   = 0.95,
                      .min_hits        = 5L,
                      .min_tot         = 5L,
                      .min_reach       = 0.01,
                      .max_terms       = 25L,
                      .min_filers      = 5L,
                      .min_filer_ratio = 0.5,
                      .drop_repeats    = TRUE,
                      .filer_pattern   = "^[0-9]{10}") {
  if (FALSE) {
    .mine            <- kw_load_mine(.mine_dir = idx_mine$MineDir[[1]])
    .min_precision   <- 0.95
    .min_hits        <- 5L
    .min_tot         <- 5L
    .min_reach       <- 0.01
    .max_terms       <- 25L
    .min_filers      <- 5L
    .min_filer_ratio <- 0.5
    .drop_repeats    <- TRUE
    .filer_pattern   <- "^[0-9]{10}"
  }
  empty_ <- tibble::tibble(
    Class = character(), Term = character(), Rank = integer(), Power = numeric(),
    Precision = numeric(), HitsPos = integer(), HitsTot = integer(), NFilers = integer(),
    FilerRatio = numeric(), Reach = numeric(), MarginalReach = numeric(), CumReach = numeric(),
    NClass = integer()
  )

  stats_ <- .mine$TermStats |>
    dplyr::filter(
      .data$Precision >= .min_precision,
      .data$HitsPos   >= .min_hits,
      .data$HitsTot   >= .min_tot
    )
  if (nrow(stats_) == 0L) return(empty_)

  if (.drop_repeats) {
    distinct_ <- stats_$Term |>
      stringi::stri_split_fixed(pattern = " ") |>
      purrr::map_lgl(\(.t) length(unique(.t)) == length(.t))
    stats_ <- stats_[distinct_, ]
    if (nrow(stats_) == 0L) return(empty_)
  }

  stats_ <- stats_ |>
    dplyr::arrange(.data$Class, dplyr::desc(.data$Power), dplyr::desc(.data$HitsPos), .data$Term)

  n_train_ <- nrow(.mine$DocsTrain)
  inc_     <- .mine$IncTrain |> dplyr::semi_join(stats_, by = dplyr::join_by(Class, Term))

  filers_ <- inc_ |>
    dplyr::left_join(.mine$DocsTrain |> dplyr::select(DocIdx, DocID), by = dplyr::join_by(DocIdx)) |>
    dplyr::mutate(Filer = stringi::stri_extract_first_regex(.data$DocID, pattern = .filer_pattern)) |>
    dplyr::summarise(NFilers = dplyr::n_distinct(.data$Filer), .by = c(Class, Term))

  stats_ <- stats_ |>
    dplyr::left_join(filers_, by = dplyr::join_by(Class, Term)) |>
    dplyr::mutate(FilerRatio = .data$NFilers / pmax(.data$HitsPos, 1L)) |>
    dplyr::filter(
      !is.na(.data$NFilers),
      .data$NFilers    >= .min_filers,
      .data$FilerRatio >= .min_filer_ratio
    )
  if (nrow(stats_) == 0L) return(empty_)
  inc_ <- inc_ |> dplyr::semi_join(stats_, by = dplyr::join_by(Class, Term))

  purrr::map(unique(stats_$Class), function(.c) {
    cand_    <- stats_ |> dplyr::filter(.data$Class == .c)
    n_class_ <- cand_$NClass[[1]]
    if (n_class_ == 0L) return(empty_)

    hits_ <- inc_ |>
      dplyr::filter(.data$Class == .c) |>
      (\(.d) split(.d$DocIdx, .d$Term))()

    covered_ <- logical(n_train_)
    keep_    <- logical(nrow(cand_))
    marg_    <- numeric(nrow(cand_))
    cum_     <- numeric(nrow(cand_))
    n_kept_  <- 0L

    for (.i in seq_len(nrow(cand_))) {
      docs_ <- hits_[[cand_$Term[[.i]]]]
      if (is.null(docs_)) next
      idx_  <- docs_ + 1L                                   # DocIdx is written 0-based by the engine
      gain_ <- sum(!covered_[idx_]) / n_class_
      if (gain_ >= .min_reach) {
        covered_[idx_] <- TRUE
        keep_[.i]      <- TRUE
        marg_[.i]      <- gain_
        cum_[.i]       <- sum(covered_) / n_class_
        n_kept_        <- n_kept_ + 1L
        if (n_kept_ >= .max_terms) break
      }
    }

    cand_[keep_, ] |>
      dplyr::mutate(
        Rank          = dplyr::row_number(),
        MarginalReach = marg_[keep_],
        CumReach      = cum_[keep_]
      ) |>
      dplyr::select(Class, Term, Rank, Power, Precision, HitsPos, HitsTot, NFilers, FilerRatio,
                    Reach, MarginalReach, CumReach, NClass)
  }) |>
    purrr::list_rbind()
}

#' Build the selection sweep grid
#'
#' The evidence floors are single-valued rather than swept. Power already encodes evidence, so by the
#' time the greedy pass accepts a term its support runs to dozens or hundreds and a floor of twenty
#' never binds; crossing those axes returns identical results at four times the cost. Marginal reach
#' and the per-class cap are the axes that change the answer.
#'
#' @param .min_precision Precision gate, held fixed across the sweep.
#' @param .min_hits,.min_tot Evidence floors, held fixed.
#' @param .min_reach Marginal-reach levels to cross.
#' @param .max_terms Per-class caps to cross.
#' @return Tibble grid with a SelName key per row.
kw_select_grid <- function(.min_precision = 0.95,
                           .min_hits      = 5L,
                           .min_tot       = 5L,
                           .min_reach     = c(0.005, 0.01, 0.02, 0.05),
                           .max_terms     = c(15L, 25L, 40L)) {
  if (FALSE) {
    .min_precision <- 0.95
    .min_hits      <- 5L
    .min_tot       <- 5L
    .min_reach     <- c(0.005, 0.01, 0.02, 0.05)
    .max_terms     <- c(15L, 25L, 40L)
  }
  tidyr::expand_grid(
    MinPrecision = .min_precision,
    MinHits      = .min_hits,
    MinTot       = .min_tot,
    MinReach     = .min_reach,
    MaxTerms     = .max_terms
  ) |>
    dplyr::mutate(
      SelName = sprintf("P%02d_H%d_C%d_R%03d_M%d", round(.data$MinPrecision * 100), .data$MinHits,
                        .data$MinTot, round(.data$MinReach * 1000), .data$MaxTerms)
    )
}


# 3. Decision: turning a lexicon into labels ---------------------------------------------------------------------------
# Two functions produce hits and one function consumes them. kw_hits_mine reads the incidence table
# the miner already wrote, which is free and is what the sweeps use; kw_hits_engine computes incidence
# for documents no mine has ever seen, which is what a corpus pass needs. They return the same four
# columns, and kw_decide cannot tell which produced its input. That is the point: the rule that
# decides a label is written once, so a table's measured precision and its applied precision are the
# same quantity rather than two similar ones.

#' The decision rule a task's keyword arm runs under
#'
#' Amendment is structurally unlike the taxonomies. An original contract is defined by the ABSENCE of
#' amendment language, so there is no argument-maximum across classes to take and nothing to abstain
#' into: a document firing no term is evidence for Original, not evidence for nothing. The rule is
#' therefore binary, and a multiclass rule applied to it would label most of the corpus as undecided
#' and report a coverage figure that describes the rule rather than the contracts.
#'
#' Defined here because two stages need it. Measurement calls it directly; deployment reads it out of
#' the published catalogue, which is written from it. Stated twice, the two would eventually disagree
#' about what a no-hit document means, and the disagreement would surface as an applied accuracy that
#' does not reproduce the published one -- with nothing anywhere reporting a fault.
#'
#' @param .label_col Character. Task.
#' @return List with Mode and PositiveClass; PositiveClass is NA outside binary mode.
kw_mode <- function(.label_col) {
  if (FALSE) .label_col <- "AmendType"
  if (identical(.label_col, "AmendType")) {
    list(Mode = "binary", PositiveClass = "Amended")
  } else {
    list(Mode = "multiclass", PositiveClass = NA_character_)
  }
}

#' Term hits on a mine's held-out fold, independent of the threshold
#'
#' Computed once per lexicon so that an entire threshold curve costs one join. A term can belong to
#' more than one class lexicon, hence the many-to-many relationship.
#'
#' THE INDEX CONVERSION HAPPENS HERE AND NOWHERE ELSE. DocIdx is the mine's own row index, written
#' zero-based by the miner and converted on load; DocID is the document's real identifier. They are
#' not interchangeable and are never both in flight downstream, because a rename that collapsed one
#' into the other would destroy exactly the mapping this join depends on while leaving code that
#' still runs.
#'
#' @param .lexicon Output of kw_select.
#' @param .mine Loaded mine carrying a held-out fold.
#' @return Tibble with DocID, Class, Term, Power.
kw_hits_mine <- function(.lexicon, .mine) {
  if (FALSE) {
    .mine    <- kw_load_mine(.mine_dir = idx_mine$MineDir[[1]])
    .lexicon <- kw_select(.mine = .mine)
  }
  if (is.null(.mine$IncTest)) cli::cli_abort("This mine holds nothing out; there is no fold to score")

  out_ <- .mine$IncTest |>
    dplyr::inner_join(
      .lexicon |> dplyr::select(Class, Term, Power),
      by           = dplyr::join_by(Term),
      relationship = "many-to-many"
    ) |>
    # Left rather than inner, so an index the document table does not hold is an abort rather than a
    # silent shortfall. An inner join here would quietly drop those hits, and the only symptom would
    # be a coverage figure slightly lower than it should be -- which is indistinguishable from a
    # lexicon that simply fires less often.
    dplyr::left_join(
      .mine$DocsTest |> dplyr::select(DocIdx, DocID),
      by = dplyr::join_by(DocIdx)
    )

  n_lost_ <- sum(is.na(out_$DocID))
  if (n_lost_ > 0L) {
    cli::cli_abort(c(
      "{n_lost_} hit{?s} name{?s/} a row index this mine's held-out document table does not hold.",
      "i" = "The incidence table and the document table travelled separately, so the join is no \\
             longer the mapping it appears to be."
    ))
  }
  out_ |> dplyr::select(DocID, Class, Term, Power)
}

#' Term hits from the engine, for documents no mine has seen
#'
#' Shells out to keyword_apply.py, which matches under the analyzer the miner used. The tokenisation
#' cannot be reproduced on this side: terms are n-grams formed AFTER stopword removal, so a term mined
#' as "corporation borrower" came from "the corporation and the borrower" and cannot be found by
#' looking for it literally. A hand-written substring search over raw text under-matches a published
#' table badly and reports nothing while doing it, which is why matching lives in one place and that
#' place is Python.
#'
#' What crosses the seam is (DocID, Term) and nothing else. The engine knows nothing of classes,
#' thresholds or labels; those are the decision, and the decision is kw_decide. Returning finished
#' predictions instead would put a second copy of the rule behind the seam and recreate the problem
#' one layer up.
#'
#' @param .lexicon Published table or selected lexicon, carrying Class, Term and Power.
#' @param .docs Documents to score, carrying .doc_col and the text column implied by .source.
#' @param .source Field the table was mined against: text or docdesc.
#' @param .n_words Truncation window in whitespace words; 0 reads the whole document.
#' @param .stopwords Stopword regime the table was mined under.
#' @param .min_token_len Shortest alphabetic token kept.
#' @param .python,.script Interpreter and keyword_apply.py.
#' @param .doc_col,.text_col,.desc_col Column names in .docs.
#' @param .out_dir Scratch directory for the parquet seam.
#' @return Tibble with DocID, Class, Term, Power.
kw_hits_engine <- function(.lexicon, .docs, .source, .n_words, .stopwords, .min_token_len,
                           .python, .script,
                           .doc_col  = "DocID",
                           .text_col = "Text",
                           .desc_col = "DocDesc",
                           .out_dir  = fs::path(tempdir(), "kw-apply")) {
  if (FALSE) {
    .lexicon       <- pubs[[1]]$Lexicon
    .docs          <- dplyr::slice_head(tab_prepared, n = 100L)
    .source        <- "text"
    .n_words       <- 512L
    .stopwords     <- "none"
    .min_token_len <- 3L
    .python        <- .lP$Engine$Python
    .script        <- .lP$Engine$ScriptApply
    .doc_col       <- "DocID"
    .text_col      <- "Text"
    .desc_col      <- "DocDesc"
    .out_dir       <- fs::path(tempdir(), "kw-apply")
  }
  if (!fs::file_exists(.python)) {
    cli::cli_abort(c(
      "No Python interpreter at {(.python)}.",
      "i" = "The mining sweep runs through {.path contracts-engine/.venv/bin/python}; the applier \\
             must run through the same one, or it is a different tokenisation."
    ))
  }
  if (!fs::file_exists(.script)) {
    cli::cli_abort(c(
      "No applier script at {(.script)}.",
      "i" = "Expected {.path contracts-engine/keyword_apply.py}, the companion to keyword_train.py."
    ))
  }
  text_col_ <- if (identical(.source, "docdesc")) .desc_col else .text_col
  need_     <- c(.doc_col, text_col_)
  miss_     <- setdiff(need_, names(.docs))
  if (length(miss_) > 0L) cli::cli_abort("The documents carry no {miss_} column.")

  fs::dir_create(.out_dir)
  lex_in_  <- fs::path(.out_dir, "lexicon.parquet")
  doc_in_  <- fs::path(.out_dir, "docs.parquet")
  hits_out_ <- fs::path(.out_dir, "hits.parquet")

  # Distinct terms, because the engine matches a vocabulary and a term belonging to two classes is one
  # vocabulary entry. The class and its Power are joined back afterwards, which is also where the
  # many-to-many relationship belongs.
  .lexicon |>
    dplyr::distinct(Term) |>
    arrow::write_parquet(lex_in_)
  .docs |>
    dplyr::select(dplyr::all_of(c(.doc_col, text_col_))) |>
    arrow::write_parquet(doc_in_)

  args_ <- c(
    .script,
    "--lexicon",       as.character(lex_in_),
    "--data",          as.character(doc_in_),
    "--out",           as.character(hits_out_),
    "--doc-col",       .doc_col,
    "--text-col",      .text_col,
    "--desc-col",      .desc_col,
    "--source",        .source,
    "--nwords",        as.character(as.integer(.n_words)),
    "--stopwords",     .stopwords,
    "--min-token-len", as.character(as.integer(.min_token_len))
  )

  # Captured rather than echoed. The child writes progress and warnings on both streams, and letting
  # them through redraws over any progress bar this is called under; capturing also means the output
  # is available to quote when the call fails, where otherwise it has already scrolled past.
  out_lines_ <- suppressWarnings(
    system2(.python, args = args_, stdout = TRUE, stderr = TRUE)
  )
  status_ <- attr(out_lines_, "status")
  if (!is.null(status_) && !identical(as.integer(status_), 0L)) {
    cli::cli_abort(c(
      "The keyword applier failed (exit {status_}).",
      "i" = "Last lines from the engine:",
      utils::tail(out_lines_, 10L)
    ))
  }

  hits_ <- arrow::read_parquet(hits_out_)
  if (nrow(hits_) == 0L) {
    return(tibble::tibble(DocID = character(), Class = character(), Term = character(),
                          Power = numeric()))
  }
  hits_ |>
    dplyr::transmute(DocID = as.character(.data$DocID), Term = as.character(.data$Term)) |>
    dplyr::inner_join(
      .lexicon |> dplyr::select(Class, Term, Power),
      by           = dplyr::join_by(Term),
      relationship = "many-to-many"
    ) |>
    dplyr::select(DocID, Class, Term, Power)
}

#' Assign labels from term hits at one threshold
#'
#' A document's score for a class is the single highest-Power term of that class firing at or above
#' the threshold, not a sum over firing terms. The maximum keeps every prediction traceable to one
#' printable term, which is the point of shipping a table rather than a model; a weighted sum is not
#' inspectable, and at the operating point the two rules agree anyway because most documents fire
#' either no term or one.
#'
#' Amendment is asymmetric: an original is defined by the absence of amendment language, so the
#' argument-maximum rule does not apply. The positive class is predicted where any of its terms
#' clears the threshold and the other label otherwise, with no abstention.
#'
#' THE DOCUMENT UNIVERSE IS SUPPLIED, NOT DISCOVERED. Measurement passes a fold's held-out rows and
#' the classes the training folds carried; deployment passes a corpus chunk and the classes the
#' manifest pins. Reading either off a mine would tie the rule to an object a corpus pass does not
#' have, and the rule has to be the same one in both places or the published precision describes a
#' system nobody runs.
#'
#' Truth is optional. A corpus document has no label, and NA is what unknown looks like; branching the
#' output schema on whether labels exist would give the two stages different columns to reconcile.
#'
#' @param .hits Output of kw_hits_mine or kw_hits_engine.
#' @param .docs Documents to decide over, carrying DocID and optionally Label.
#' @param .classes Character vector of the classes in play.
#' @param .tau Power threshold, acting as the evidence gate.
#' @param .mode Decision rule to apply.
#' @param .positive_class Class predicted on any hit, required in binary mode.
#' @param .probs Logical. TRUE returns the per-class score table, which is one row per document per
#'   class and therefore the expensive part of this function at corpus scale.
#' @param .none Abstention sentinel.
#' @return List with Pred (one row per document, carrying the runner-up) and Prob (one row per
#'   document and class, empty when .probs is FALSE).
kw_decide <- function(.hits, .docs, .classes,
                      .tau            = 0.70,
                      .mode           = c("multiclass", "binary"),
                      .positive_class = NULL,
                      .probs          = TRUE,
                      .none           = KW_NONE) {
  if (FALSE) {
    .mine           <- kw_load_mine(.mine_dir = idx_mine$MineDir[[1]])
    .hits           <- kw_hits_mine(.lexicon = kw_select(.mine = .mine), .mine = .mine)
    .docs           <- .mine$DocsTest
    .classes        <- sort(unique(.mine$DocsTrain$Label))
    .tau            <- 0.70
    .mode           <- "multiclass"
    .positive_class <- NULL
    .probs          <- TRUE
    .none           <- KW_NONE
  }
  .mode <- match.arg(.mode)
  docs_ <- .docs
  if (!"Label" %in% names(docs_)) docs_$Label <- NA_character_

  best_ <- .hits |>
    dplyr::filter(.data$Power >= .tau) |>
    dplyr::arrange(.data$DocID, .data$Class, dplyr::desc(.data$Power), .data$Term) |>
    dplyr::distinct(DocID, Class, .keep_all = TRUE)

  prob_ <- if (.probs) {
    tidyr::expand_grid(DocID = docs_$DocID, Class = .classes) |>
      dplyr::left_join(best_ |> dplyr::select(DocID, Class, Power),
                       by = dplyr::join_by(DocID, Class)) |>
      dplyr::mutate(Prob = dplyr::coalesce(.data$Power, 0)) |>
      dplyr::select(DocID, Class, Prob)
  } else {
    tibble::tibble(DocID = character(), Class = character(), Prob = numeric())
  }

  if (.mode == "binary") {
    if (is.null(.positive_class) || is.na(.positive_class)) {
      cli::cli_abort("Binary mode requires .positive_class")
    }
    other_ <- setdiff(.classes, .positive_class)
    if (length(other_) != 1L) cli::cli_abort("Binary mode expects exactly two classes")

    pred_ <- docs_ |>
      dplyr::left_join(
        best_ |> dplyr::filter(.data$Class == .positive_class) |>
          dplyr::select(DocID, PosPower = Power, PosTerm = Term),
        by = dplyr::join_by(DocID)
      ) |>
      dplyr::mutate(
        PredLabel = dplyr::if_else(!is.na(.data$PosPower), .positive_class, other_),
        Score     = dplyr::coalesce(.data$PosPower, 0),
        TopTerm   = .data$PosTerm,
        # NO RUNNER-UP EXISTS HERE. The rule is not an argument-maximum across classes: the negative
        # label is assigned by absence of evidence, so naming it as a second choice with a score would
        # report a comparison that was never made. NA is what "this rule does not produce one" looks
        # like, and it is distinguishable from a class that genuinely scored zero.
        Pred2     = NA_character_,
        Score2    = NA_real_
      ) |>
      dplyr::select(DocID, TrueLabel = Label, PredLabel, Score, TopTerm, Pred2, Score2)

    return(list(Pred = pred_, Prob = prob_))
  }

  # Ranked by CLASS, not by term. A document matching four terms of one category and one of another
  # has two candidate classes, not five candidate terms, and the runner-up worth recording is the
  # second class.
  ranked_ <- best_ |>
    dplyr::arrange(.data$DocID, dplyr::desc(.data$Power), .data$Class) |>
    dplyr::mutate(
      MaxPower = max(.data$Power),
      nTied    = sum(.data$Power == max(.data$Power)),
      Rank     = dplyr::row_number(),
      .by = DocID
    ) |>
    dplyr::filter(.data$Rank <= 2L)

  pred_ <- docs_ |>
    dplyr::left_join(
      ranked_ |> dplyr::filter(.data$Rank == 1L) |>
        dplyr::select(DocID, WinClass = Class, WinTerm = Term, MaxPower, nTied),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::left_join(
      ranked_ |> dplyr::filter(.data$Rank == 2L) |>
        dplyr::select(DocID, NextClass = Class, NextPower = Power),
      by = dplyr::join_by(DocID)
    ) |>
    dplyr::mutate(
      Committed = !is.na(.data$MaxPower) & .data$nTied == 1L,
      PredLabel = dplyr::if_else(.data$Committed, .data$WinClass, .none),
      Score     = dplyr::if_else(.data$Committed, .data$MaxPower, 0),
      TopTerm   = dplyr::if_else(.data$Committed, .data$WinTerm, NA_character_),
      # Carried only where the document committed. An abstention has no first choice, so reporting a
      # second one would invite a reader to break the tie the rule declined to break.
      Pred2     = dplyr::if_else(.data$Committed, .data$NextClass, NA_character_),
      Score2    = dplyr::if_else(.data$Committed, .data$NextPower, NA_real_)
    ) |>
    dplyr::select(DocID, TrueLabel = Label, PredLabel, Score, TopTerm, Pred2, Score2)

  list(Pred = pred_, Prob = prob_)
}


# 4. Evaluation: one cell, and the sweeps over cells -------------------------------------------------------------------

#' Canonical configuration name
#'
#' Encodes every axis that varied, so the shared leaderboard -- which keys on this string -- ranks
#' keyword and transformer configurations side by side without knowing their axes differ.
#'
#' EVERY SELECTION AXIS APPEARS. An axis omitted from this name is an axis two configurations can
#' differ on while sharing an identifier, and the run folder is named from it -- so the second write
#' lands in the first one's directory and replaces results nobody asked to replace. The evidence
#' floors were previously absent, which was harmless only while nothing varied them; the published
#' tables vary them, because their floors are chosen per task rather than swept.
#'
#' @param .label_col,.source,.nwords,.ngram_max,.stopwords Mining axes.
#' @param .min_precision,.min_hits,.min_tot,.min_reach,.max_terms Selection axes.
#' @param .tau Power threshold.
#' @param .seed Stamped for parity.
#' @param .terms_tag Optional short tag identifying a supplied term list. Mined runs pass NULL and
#'   keep their existing names; only runs scored from a written list are distinguished by it.
#' @return Character scalar.
kw_config_name <- function(.label_col, .source, .nwords, .ngram_max, .stopwords,
                           .min_precision, .min_hits, .min_tot, .min_reach, .max_terms, .tau,
                           .seed = 42L, .terms_tag = NULL) {
  if (FALSE) {
    .label_col     <- "ClassDetailed"
    .source        <- "text"
    .nwords        <- 256L
    .ngram_max     <- 3L
    .stopwords     <- "none"
    .min_precision <- 0.95
    .min_hits      <- 5L
    .min_tot       <- 5L
    .min_reach     <- 0.01
    .max_terms     <- 25L
    .tau           <- 0.70
    .seed          <- 42L
    .terms_tag     <- NULL
  }
  window_ <- if (.nwords == 0L) "full" else as.character(.nwords)
  sw_     <- c(none = "none", english = "en", domain = "dom", english_domain = "endom")[[.stopwords]]
  paste0(
    .label_col, "__keyword-", .source, "__",
    "W", window_, "_N", .ngram_max, "_SW", sw_,
    "_P", sprintf("%02d", round(.min_precision * 100)),
    "_H", as.integer(.min_hits),
    "_C", as.integer(.min_tot),
    "_R", sprintf("%03d", round(.min_reach * 1000)),
    "_M", .max_terms,
    "_T", sprintf("%02d", round(.tau * 100)),
    if (is.null(.terms_tag)) "" else paste0("_X", .terms_tag),
    "_S", .seed
  )
}

#' Evaluate one mine under one selection at one threshold
#'
#' Writes a run folder in the schema the transformer trainer uses, so the shared loading and
#' leaderboard functions consume these runs unchanged even though R rather than Python authored them.
#'
#' @param .mine Loaded mine carrying a held-out fold.
#' @param .min_precision,.min_hits,.min_tot,.min_reach,.max_terms Selection parameters.
#' @param .min_filers,.min_filer_ratio,.drop_repeats Selection parameters.
#' @param .tau Power threshold.
#' @param .runs_root Runs directory, or NULL to evaluate without writing.
#' @param .seed Stamped for parity.
#' @return One-row tibble of metrics, carrying the predictions and lexicon in list columns.
kw_evaluate <- function(.mine,
                        .min_precision   = 0.95,
                        .min_hits        = 5L,
                        .min_tot         = 5L,
                        .min_reach       = 0.01,
                        .max_terms       = 25L,
                        .min_filers      = 5L,
                        .min_filer_ratio = 0.5,
                        .drop_repeats    = TRUE,
                        .tau             = 0.70,
                        .runs_root       = NULL,
                        .seed            = 42L) {
  if (FALSE) {
    .mine          <- kw_load_mine(.mine_dir = idx_mine$MineDir[[1]])
    .min_precision <- 0.95
    .min_reach     <- 0.01
    .max_terms     <- 25L
    .tau           <- 0.70
    .runs_root     <- NULL
    .seed          <- 42L
  }
  man_ <- .mine$Manifest
  if (is.null(.mine$IncTest)) cli::cli_abort("An all-data mine holds nothing out and cannot be scored")

  # The rule comes from kw_mode rather than from a test on the task name here, so measurement and
  # deployment cannot end up running different rules on the same task.
  mode_    <- kw_mode(.label_col = man_$label_col)
  binary_  <- identical(mode_$Mode, "binary")
  pos_     <- if (binary_) mode_$PositiveClass else NULL
  classes_ <- sort(unique(.mine$DocsTrain$Label))
  t0_      <- Sys.time()

  lex_ <- kw_select(
    .mine            = .mine,
    .min_precision   = .min_precision,
    .min_hits        = .min_hits,
    .min_tot         = .min_tot,
    .min_reach       = .min_reach,
    .max_terms       = .max_terms,
    .min_filers      = .min_filers,
    .min_filer_ratio = .min_filer_ratio,
    .drop_repeats    = .drop_repeats
  )
  lex_tau_ <- lex_ |> dplyr::filter(.data$Power >= .tau)

  if (nrow(lex_tau_) == 0L) {
    # The negative label is whichever class is not the positive one, read off the training folds. An
    # empty lexicon under the binary rule predicts it everywhere, which is a real prediction and
    # scores as one; under the multiclass rule the same table decides nothing and abstains.
    none_ <- if (binary_) setdiff(classes_, pos_) else KW_NONE
    pred_ <- .mine$DocsTest |>
      dplyr::transmute(
        DocID, TrueLabel = Label,
        PredLabel = none_,
        Score     = 0,
        TopTerm   = NA_character_,
        Pred2     = NA_character_,
        Score2    = NA_real_
      )
    prob_ <- tibble::tibble(DocID = character(), Class = character(), Prob = numeric())
  } else {
    dec_  <- kw_decide(
      .hits           = kw_hits_mine(.lexicon = lex_, .mine = .mine),
      .docs           = .mine$DocsTest |> dplyr::select(DocID, Label),
      .classes        = classes_,
      .tau            = .tau,
      .mode           = mode_$Mode,
      .positive_class = pos_,
      .probs          = TRUE,
      .none           = KW_NONE
    )
    pred_ <- dec_$Pred
    prob_ <- dec_$Prob
  }

  cfg_ <- kw_config_name(
    .label_col     = man_$label_col,
    .source        = man_$source,
    .nwords        = man_$nwords,
    .ngram_max     = man_$ngram_range[[2]],
    .stopwords     = man_$stopwords %||% "none",
    .min_precision = .min_precision,
    .min_hits      = .min_hits,
    .min_tot       = .min_tot,
    .min_reach     = .min_reach,
    .max_terms     = .max_terms,
    .tau           = .tau,
    .seed          = .seed,
    # A supplied list is identified by its content, not by its shape. Two lists scored for the same
    # task, source, window and floors differ in nothing kw_config_name otherwise sees, so without
    # this the generated and union arms collide and whichever is written second wins the folder.
    .terms_tag     = if (identical(man_$mode, "manual")) {
      stringi::stri_sub(man_$terms_hash %||% "nohash", 1L, 6L)
    } else {
      NULL
    }
  )
  run_  <- paste0(cfg_, "_F", man_$test_fold)
  sc_   <- clf_scores(pred_, .none = KW_NONE)
  pc_   <- clf_perclass(pred_, .none = KW_NONE)
  hit_  <- pred_$PredLabel != KW_NONE

  overall_ <- tibble::tibble(
    ConfigName   = cfg_,
    Run          = run_,
    RunName      = run_,
    Model        = paste0("keyword-", man_$source),
    LabelCol     = man_$label_col,
    TextCol      = man_$source,
    ClassWeights = FALSE,
    TestFold     = as.integer(man_$test_fold),
    MaxLen       = NA_real_,
    Epochs       = NA_real_,
    BatchSize    = NA_real_,
    LR           = NA_real_,
    Seed         = as.integer(.seed),
    Device       = "cpu",
    nTrain       = as.integer(man_$n_train),
    nTest        = as.integer(man_$n_test),
    nClasses     = as.integer(man_$n_classes),
    Accuracy     = sc_$Accuracy,
    F1_macro     = sc_$F1_macro,
    F1_weighted  = sc_$F1_weighted,
    DurationSec  = round(as.numeric(difftime(Sys.time(), t0_, units = "secs")), 2),
    Smoke        = FALSE,
    Source       = man_$source,
    NWords       = as.integer(man_$nwords),
    NgramMax     = as.integer(man_$ngram_range[[2]]),
    Stopwords    = man_$stopwords %||% "none",
    MinPrecision = .min_precision,
    MinHits      = as.integer(.min_hits),
    MinTot       = as.integer(.min_tot),
    MinReach     = .min_reach,
    MaxTerms     = as.integer(.max_terms),
    Tau          = .tau,
    Origin       = if (identical(man_$mode, "manual")) "supplied" else "mined",
    TermsFile    = as.character(man_$terms_file %||% NA_character_),
    TermsHash    = as.character(man_$terms_hash %||% NA_character_),
    nTerms       = nrow(lex_tau_),
    nClassesHit  = dplyr::n_distinct(pred_$PredLabel[hit_]),
    Coverage     = sc_$Coverage,
    SelPrecision = if (any(hit_)) mean(pred_$PredLabel[hit_] == pred_$TrueLabel[hit_]) else NA_real_
  )

  if (!is.null(.runs_root)) {
    kw_write_run(
      .runs_root   = .runs_root,
      .run_name    = run_,
      .config_name = cfg_,
      .pred        = pred_,
      .prob        = prob_,
      .overall     = overall_,
      .perclass    = pc_,
      .lexicon     = lex_tau_,
      .manifest    = man_
    )
  }

  overall_ |> dplyr::mutate(Pred = list(pred_), Lexicon = list(lex_tau_))
}

#' Write one keyword run in the shared run-folder schema
#'
#' @param .runs_root Runs directory.
#' @param .run_name,.config_name Identifiers.
#' @param .pred,.prob,.overall,.perclass,.lexicon Artifacts to write.
#' @param .manifest Mine manifest, carried into config.json as provenance.
#' @return Run directory path, invisibly.
kw_write_run <- function(.runs_root, .run_name, .config_name, .pred, .prob, .overall, .perclass,
                         .lexicon, .manifest) {
  if (FALSE) {
    .runs_root   <- .lP$Output$Runs
    .run_name    <- "run"
    .config_name <- "cfg"
    .pred        <- perf_final$Pred[[1]]
    .prob        <- tibble::tibble()
    .overall     <- perf_final[1, ]
    .perclass    <- clf_perclass(.pred, .none = KW_NONE)
    .lexicon     <- perf_final$Lexicon[[1]]
    .manifest    <- kw_load_mine(.mine_dir = idx_mine$MineDir[[1]])$Manifest
  }
  dir_ <- fs::path(.runs_root, .run_name)
  fs::dir_create(dir_)

  .pred |>
    dplyr::mutate(ConfigName = .config_name, Run = .run_name, Fold = as.integer(.manifest$test_fold)) |>
    dplyr::select(ConfigName, Run, DocID, TrueLabel, PredLabel, Score, TopTerm, Fold) |>
    arrow::write_parquet(fs::path(dir_, "predictions.parquet"))

  .prob |>
    dplyr::mutate(ConfigName = .config_name) |>
    dplyr::select(ConfigName, DocID, Class, Prob) |>
    arrow::write_parquet(fs::path(dir_, "probabilities.parquet"))

  arrow::write_parquet(.overall, fs::path(dir_, "metrics_overall.parquet"))

  .perclass |>
    dplyr::mutate(ConfigName = .config_name, Run = .run_name) |>
    arrow::write_parquet(fs::path(dir_, "metrics_perclass.parquet"))

  .lexicon |>
    dplyr::mutate(ConfigName = .config_name, Run = .run_name) |>
    arrow::write_parquet(fs::path(dir_, "lexicon.parquet"))

  jsonlite::write_json(
    list(config_name = .config_name, run_name = .run_name, authored_by = "03C kw_evaluate",
         mine = .manifest, overall = as.list(.overall)),
    fs::path(dir_, "config.json"), auto_unbox = TRUE, pretty = TRUE
  )
  invisible(dir_)
}

#' Score every mine under one selection
#'
#' Isolates the mining axes from the selection axes. Crossing both at once searches a far larger space
#' than the transformer sweep does on the same documents, and the winner of a large search on a small
#' sample is partly a winner by luck.
#'
#' @param .mine_index Mine index; all-data and manual mines are skipped.
#' @param .runs_root Runs directory, or NULL.
#' @param ... Selection parameters passed to kw_evaluate.
#' @return Tibble of per-(mine, fold) metrics.
kw_sweep_mines <- function(.mine_index, .runs_root = NULL, ...) {
  if (FALSE) {
    .mine_index <- idx_mine
    .runs_root  <- NULL
  }
  idx_ <- .mine_index |> dplyr::filter(.data$Fold > 0L, .data$Mode == "mine")
  cli::cli_alert_info("Scoring {nrow(idx_)} mine-folds")

  purrr::map(idx_$MineDir, function(.d) {
    kw_evaluate(.mine = kw_load_mine(.mine_dir = .d), .runs_root = .runs_root, ...)
  }, .progress = "mine-folds") |>
    purrr::list_rbind()
}

#' Sweep the selection grid over chosen mines
#'
#' Mines are loaded once and reused across every selection configuration, which is what makes this
#' sweep interactive rather than a second mining run.
#'
#' @param .mine_index Mine index restricted to the configurations to explore.
#' @param .grid_sel Grid from kw_select_grid.
#' @param .tau Power threshold.
#' @param .runs_root Runs directory, or NULL.
#' @param ... Further selection parameters passed to kw_evaluate.
#' @return Tibble of per-(mine, fold, selection) metrics.
kw_sweep_selection <- function(.mine_index, .grid_sel, .tau = 0.70, .runs_root = NULL, ...) {
  if (FALSE) {
    .mine_index <- idx_sel
    .grid_sel   <- kw_select_grid()
    .tau        <- 0.70
    .runs_root  <- NULL
  }
  idx_ <- .mine_index |> dplyr::filter(.data$Fold > 0L, .data$Mode == "mine")
  cli::cli_alert_info("Scoring {nrow(idx_)} mine-folds under {nrow(.grid_sel)} selection configs")

  purrr::map(idx_$MineDir, function(.d) {
    mine_ <- kw_load_mine(.mine_dir = .d)
    purrr::map(seq_len(nrow(.grid_sel)), function(.j) {
      kw_evaluate(
        .mine          = mine_,
        .min_precision = .grid_sel$MinPrecision[[.j]],
        .min_hits      = .grid_sel$MinHits[[.j]],
        .min_tot       = .grid_sel$MinTot[[.j]],
        .min_reach     = .grid_sel$MinReach[[.j]],
        .max_terms     = .grid_sel$MaxTerms[[.j]],
        .tau           = .tau,
        .runs_root     = .runs_root,
        ...
      )
    }) |>
      purrr::list_rbind()
  }, .progress = "selection sweep") |>
    purrr::list_rbind()
}


# 5. Operating point ---------------------------------------------------------------------------------------------------

#' Precision and coverage across the threshold
#'
#' The threshold filters a fixed lexicon at scoring time, so the whole curve costs one hit join per
#' fold. nClassesHit is reported alongside precision because a global Power floor removes thin
#' categories by arithmetic: a curve can look excellent on precision while describing a table that
#' labels five categories out of twelve.
#'
#' @param .mine_dirs Fold mine directories for one task and source.
#' @param .taus Thresholds to evaluate.
#' @param ... Selection parameters passed to kw_select.
#' @return Tibble with one row per threshold.
kw_tau_curve <- function(.mine_dirs, .taus = seq(0.40, 0.96, by = 0.02), ...) {
  if (FALSE) {
    .mine_dirs <- idx_sel$MineDir
    .taus      <- seq(0.40, 0.96, by = 0.02)
  }
  prepped_ <- purrr::map(.mine_dirs, function(.d) {
    mine_ <- kw_load_mine(.mine_dir = .d)
    lex_  <- kw_select(.mine = mine_, ...)
    mode_ <- kw_mode(.label_col = mine_$Manifest$label_col)
    list(
      Lex     = lex_,
      Hits    = kw_hits_mine(.lexicon = lex_, .mine = mine_),
      Docs    = mine_$DocsTest |> dplyr::select(DocID, Label),
      Classes = sort(unique(mine_$DocsTrain$Label)),
      Mode    = mode_
    )
  })

  purrr::map(.taus, function(.t) {
    pred_ <- purrr::map(prepped_, function(.p) {
      kw_decide(
        .hits           = .p$Hits,
        .docs           = .p$Docs,
        .classes        = .p$Classes,
        .tau            = .t,
        .mode           = .p$Mode$Mode,
        .positive_class = if (identical(.p$Mode$Mode, "binary")) .p$Mode$PositiveClass else NULL,
        # The curve reads precision, coverage and breadth off the predictions. The per-class score
        # table is one row per document per class per threshold and is never consulted here, so
        # building it would multiply the cost of the sweep by the number of categories for nothing.
        .probs          = FALSE,
        .none           = KW_NONE
      )$Pred
    }) |>
      purrr::list_rbind()

    sc_  <- clf_scores(pred_, .none = KW_NONE)
    hit_ <- pred_$PredLabel != KW_NONE
    tibble::tibble(
      Tau          = .t,
      nTerms       = sum(purrr::map_int(prepped_, \(.p) sum(.p$Lex$Power >= .t))),
      nClassesHit  = dplyr::n_distinct(pred_$PredLabel[hit_]),
      Coverage     = sc_$Coverage,
      SelPrecision = if (any(hit_)) mean(pred_$PredLabel[hit_] == pred_$TrueLabel[hit_]) else NA_real_,
      Accuracy     = sc_$Accuracy,
      F1_macro     = sc_$F1_macro,
      N            = nrow(pred_)
    )
  }) |>
    purrr::list_rbind()
}

#' Choose the operating point from the curve
#'
#' Among thresholds whose realised precision reaches the target within tolerance, prefer the one
#' labelling most categories, then the widest coverage, then the lowest threshold.
#'
#' The tolerance is not slack, it is arithmetic. With roughly two thousand documents classified, the
#' standard error of a proportion near 0.95 is about half a percentage point, so a hard comparison
#' against the target treats differences it cannot measure as decisive. Applied to a curve sitting
#' just under the target, a hard rule raises the threshold, and because Power carries a
#' support-dependent ceiling the categories it drops first are the small ones. Paying two categories
#' for half a percentage point of unmeasurable precision is the wrong trade for an artifact whose
#' value is breadth at a stated precision.
#'
#' @param .curve Output of kw_tau_curve.
#' @param .target_precision Precision the published table must clear.
#' @param .tolerance Precision band treated as indistinguishable from the target.
#' @param .min_classes Categories the table must label.
#' @return One-row tibble.
kw_operating_point <- function(.curve, .target_precision = 0.95, .tolerance = 0.01,
                               .min_classes = 1L) {
  if (FALSE) {
    .curve            <- curve_detailed
    .target_precision <- 0.95
    .tolerance        <- 0.01
    .min_classes      <- 9L
  }
  feasible_ <- .curve |>
    dplyr::filter(!is.na(.data$SelPrecision), .data$Coverage > 0,
                  .data$SelPrecision >= .target_precision - .tolerance)

  ok_ <- feasible_ |> dplyr::filter(.data$nClassesHit >= .min_classes)
  if (nrow(ok_) > 0L) {
    return(ok_ |>
             dplyr::arrange(dplyr::desc(.data$nClassesHit), dplyr::desc(.data$Coverage), .data$Tau) |>
             dplyr::slice_head(n = 1L))
  }

  if (nrow(feasible_) > 0L) {
    cli::cli_alert_warning(
      "Precision {(.target_precision)} is reachable but never with {(.min_classes)} categories \\
       labelled; the widest is {max(feasible_$nClassesHit)}. Returning that point."
    )
    return(feasible_ |>
             dplyr::arrange(dplyr::desc(.data$nClassesHit), dplyr::desc(.data$Coverage), .data$Tau) |>
             dplyr::slice_head(n = 1L))
  }

  cli::cli_alert_warning("Precision {(.target_precision)} is unreachable; returning the most precise point")
  .curve |>
    dplyr::filter(!is.na(.data$SelPrecision), .data$Coverage > 0) |>
    dplyr::slice_max(.data$SelPrecision, n = 1L, with_ties = FALSE)
}

#' The mining cells behind one published table
#'
#' Resolves task and window to the fold mines that estimate the table and the all-data mine that
#' produces it, holding the n-gram order, stopword regime and source at the values the mining sweep
#' crowned.
#'
#' WRITTEN ONCE BECAUSE IT WAS WRITTEN TWICE. The publication step and the threshold probe both need
#' this filter, and a filter stated in two places is two places that have to agree about which axes
#' identify a cell. The axes not named here are the ones that matter: mode, because the manual mines
#' of the generated arm share every axis that is named, and token length, because nothing varies it
#' and so nobody watches it.
#'
#' @param .label_col,.n_words,.source Table to resolve.
#' @param .idx_mine Mining index.
#' @param .cfg_mine Crowned mining configuration per task and source.
#' @return List: Cells, Dirs, DirAll, NgramMax, Stopwords, MinTokenLen.
kw_publish_cells <- function(.label_col, .n_words, .idx_mine, .cfg_mine, .source = "text") {
  if (FALSE) {
    .label_col <- "ClassBroad"
    .n_words   <- 512L
    .idx_mine  <- idx_mine
    .cfg_mine  <- cfg_mine
    .source    <- "text"
  }
  cfg_ <- .cfg_mine |> dplyr::filter(.data$LabelCol == .label_col, .data$Source == .source)
  if (nrow(cfg_) == 0L) {
    cli::cli_abort("No crowned mining configuration for {(.label_col)} on {(.source)}.")
  }

  cells_ <- .idx_mine |>
    dplyr::filter(
      .data$LabelCol  == .label_col,
      .data$Source    == .source,
      .data$NWords    == .n_words,
      .data$NgramMax  == cfg_$NgramMax[[1]],
      .data$Stopwords == cfg_$Stopwords[[1]]
    )
  dirs_    <- cells_ |> dplyr::filter(.data$Fold > 0L) |> dplyr::pull(MineDir)
  dir_all_ <- cells_ |> dplyr::filter(.data$Fold == 0L) |> dplyr::pull(MineDir)
  if (length(dirs_) == 0L || length(dir_all_) == 0L) {
    cli::cli_abort("No mines for {(.label_col)} at window {(.n_words)} on {(.source)}.")
  }
  if (length(dir_all_) > 1L) {
    cli::cli_abort("{length(dir_all_)} all-data mines match {(.label_col)} at window {(.n_words)}.")
  }

  # ONE MINE PER FOLD, ASSERTED STRUCTURALLY. A fold appearing twice means the axes above do not
  # identify a cell, whatever the reason -- and the consequences do not announce themselves: the
  # threshold curve pools two populations, the fold-agreement filter counts one fold twice, and the
  # published table changes without anything reporting that it did.
  dup_ <- cells_ |>
    dplyr::filter(.data$Fold > 0L) |>
    dplyr::summarise(n = dplyr::n(), .by = Fold) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(dup_) > 0L) {
    cli::cli_abort(c(
      "More than one mine matches {(.label_col)} at window {(.n_words)} for fold{?s} {dup_$Fold}.",
      "i" = "Mines matching: {cells_$MineName[cells_$Fold %in% dup_$Fold]}"
    ))
  }

  # The token length is not one of the axes this filter selects on, because it has never been varied.
  # That is precisely why it is checked: an unvaried parameter is one nobody watches, and two mines
  # differing only on it would be pooled here into a table whose terms were formed under two
  # tokenisations while the catalogue pinned one of them.
  tl_ <- unique(cells_$MinTokenLen)
  if (length(tl_) != 1L) {
    cli::cli_abort(c(
      "The mines for {(.label_col)} at window {(.n_words)} disagree on the minimum token length: {tl_}.",
      "i" = "Publishing across them would pin one tokenisation for terms formed under several."
    ))
  }

  list(
    Cells = cells_, Dirs = dirs_, DirAll = dir_all_,
    NgramMax = cfg_$NgramMax[[1]], Stopwords = cfg_$Stopwords[[1]],
    MinTokenLen = as.integer(tl_)
  )
}

#' Does the threshold grid's lower edge decide the operating point
#'
#' A grid is a choice about where to look, and an optimum sitting on its edge was not found by the
#' rule -- it was imposed by the edge. That is easy to miss where the rule's tiebreak reaches the
#' threshold last: under the binary rule coverage is one by construction, so most categories and
#' widest coverage are ties for every threshold and the lowest one always wins. The reported optimum
#' is then the smallest number offered, and it would move if a smaller one were offered.
#'
#' Traces the curve below the published floor and selects twice from the one curve: once restricted to
#' the thresholds actually published, once over the whole extension. Where the two agree the floor is
#' documentation; where they differ it is a parameter, and one nobody set deliberately.
#'
#' This REPORTS rather than decides. Extending the grid would change a published artifact, which is
#' not a change to make as a side effect of checking whether it should be made.
#'
#' @param .label_col,.n_words,.source Table to probe.
#' @param .idx_mine,.cfg_mine Mining index and the crowned cell naming the held axes.
#' @param .floors One-row tibble from kw_task_floors().
#' @param .target_precision,.min_classes,.tolerance Acceptance rule, as published.
#' @param .taus_published Thresholds the published table was chosen from.
#' @param .taus_extended Thresholds to trace, which must contain .taus_published.
#' @return Two-row tibble: Grid, Tau, Terms, Classes, Coverage, SelPrecision, F1_macro, plus Binds.
kw_tau_floor_probe <- function(.label_col, .n_words, .idx_mine, .cfg_mine, .floors,
                               .target_precision, .min_classes,
                               .taus_published, .taus_extended,
                               .tolerance = 0.01, .source = "text") {
  if (FALSE) {
    .label_col        <- "AmendType"
    .n_words          <- 256L
    .idx_mine         <- idx_mine
    .cfg_mine         <- cfg_mine
    .floors           <- floors_task[["AmendType"]]
    .target_precision <- 0.85
    .min_classes      <- 2L
    .taus_published   <- seq(0.40, 0.96, by = 0.02)
    .taus_extended    <- seq(0.10, 0.96, by = 0.02)
    .tolerance        <- 0.01
    .source           <- "text"
  }
  # THRESHOLDS ARE COMPARED ON A FIXED INTEGER SCALE, NOT AS DOUBLES. Two seq() calls reaching the
  # same nominal threshold from different starting points disagree in the last place -- 11 of the 29
  # published values here do -- so comparing by value either refuses a legal pair of grids or, far
  # worse, quietly keeps two thirds of the rows and selects an operating point from a curve with holes
  # in it. The scale is fixed rather than tolerance-based because the grid is decimal by construction.
  key_      <- function(.x) as.integer(round(.x * 1e4))
  pub_key_  <- key_(.taus_published)
  ext_key_  <- key_(.taus_extended)
  n_absent_ <- sum(!pub_key_ %in% ext_key_)
  if (n_absent_ > 0L) {
    cli::cli_abort(c(
      "The extended grid must contain the published one, or the two rows are not comparable.",
      "i" = "{n_absent_} of {length(pub_key_)} published thresholds are absent from the extension."
    ))
  }
  cells_ <- kw_publish_cells(
    .label_col = .label_col,
    .n_words   = .n_words,
    .idx_mine  = .idx_mine,
    .cfg_mine  = .cfg_mine,
    .source    = .source
  )

  curve_ <- kw_tau_curve(
    .mine_dirs     = cells_$Dirs,
    .taus          = .taus_extended,
    .min_precision = .target_precision,
    .min_hits      = .floors$MinHits,
    .min_tot       = .floors$MinTot,
    .min_reach     = .floors$MinReach,
    .max_terms     = .floors$MaxTerms
  )

  pick_ <- function(.c) {
    kw_operating_point(
      .curve            = .c,
      .target_precision = .target_precision,
      .tolerance        = .tolerance,
      .min_classes      = .min_classes
    )
  }
  # Asserted rather than trusted. A restriction that silently loses rows produces a curve the rule can
  # still select from, so the failure arrives as a plausible operating point rather than as an error.
  curve_pub_ <- curve_ |> dplyr::filter(key_(.data$Tau) %in% pub_key_)
  if (nrow(curve_pub_) != length(pub_key_)) {
    cli::cli_abort(c(
      "Restricting the traced curve to the published grid kept {nrow(curve_pub_)} of \\
       {length(pub_key_)} thresholds.",
      "i" = "The curve and the published grid disagree about which thresholds exist."
    ))
  }
  op_pub_ <- pick_(curve_pub_)
  op_ext_ <- pick_(curve_)

  # A FLOOR BINDS WHEN IT CHANGES THE TABLE, NOT WHEN IT CHANGES THE NUMBER IN THE TAU COLUMN. The
  # acceptance rule prefers the lowest acceptable threshold, so where a range of thresholds selects
  # the same lexicon it reports the bottom of that range -- and moves to the new bottom the moment a
  # lower one is offered, having changed nothing. That happens by construction here: the evidence and
  # precision gates bound Power from below, since the weakest term that can pass MinHits of 5 at the
  # promised precision still carries a Wilson bound near 0.5, so every threshold beneath that selects
  # an identical lexicon. Reported as two facts because they are two: whether the reported threshold
  # moved, and whether anything followed from it.
  moves_ <- op_ext_$Tau < op_pub_$Tau
  binds_ <- moves_ &&
    !(identical(op_ext_$nTerms, op_pub_$nTerms) &&
      identical(op_ext_$nClassesHit, op_pub_$nClassesHit) &&
      isTRUE(all.equal(op_ext_$Coverage, op_pub_$Coverage)) &&
      isTRUE(all.equal(op_ext_$SelPrecision, op_pub_$SelPrecision)))

  dplyr::bind_rows(
    op_pub_ |> dplyr::mutate(Grid = "published"),
    op_ext_ |> dplyr::mutate(Grid = "extended")
  ) |>
    dplyr::transmute(
      Task = .label_col, NWords = as.integer(.n_words), Grid,
      Tau, Terms = .data$nTerms, Classes = .data$nClassesHit,
      Coverage, SelPrecision, F1_macro,
      Moves = moves_, Binds = binds_
    )
}

#' Print the threshold-floor probe
#'
#' @param .tab Output of kw_tau_floor_probe(), bound across tasks.
#' @return Invisibly .tab.
kw_report_tau_floor <- function(.tab) {
  if (FALSE) .tab <- tab_tau_floor
  .tab |>
    dplyr::mutate(
      Window       = dplyr::if_else(.data$NWords == 0L, "full", as.character(.data$NWords)),
      Coverage     = tbl_pct(.data$Coverage),
      SelPrecision = tbl_pct(.data$SelPrecision)
    ) |>
    dplyr::select(Task, Window, Grid, Tau, Terms, Classes, Coverage, SelPrecision, F1_macro) |>
    tbl_say(.title = "Operating point under the published grid and under a wider one")
  cli::cli_text("")

  ext_    <- .tab |> dplyr::filter(.data$Grid == "extended")
  n_bind_ <- sum(ext_$Binds)
  n_move_ <- sum(ext_$Moves)

  if (n_bind_ > 0L) {
    cli::cli_alert_warning(
      "{n_bind_} of {nrow(ext_)} tables select a DIFFERENT table when the grid is widened, so the \\
       published floor is deciding what ships. Read the extended row against the published one: more \\
       terms at held precision means the floor is costing coverage, and more terms at lower precision \\
       means it is protecting the promise and should be argued as a floor rather than left looking \\
       like an optimum."
    )
    return(invisible(.tab))
  }

  if (n_move_ > 0L) {
    cli::cli_alert_success(
      "Widening the grid changes no published table. {n_move_} of {nrow(ext_)} report a lower \\
       threshold and an otherwise identical row, which is the threshold being unidentified rather \\
       than the floor binding: the evidence and precision gates already bound Power well above the \\
       floor, so every threshold beneath that selects the same terms and the rule reports the lowest \\
       of them."
    )
    cli::cli_alert_info(
      "That is the reading of an operating point at 0.40 on a task where coverage is one by \\
       construction. The threshold is not a corner solution; it is the bottom of a range over which \\
       nothing varies, and quoting the realised precision rather than the threshold remains the \\
       right way to describe the table."
    )
    return(invisible(.tab))
  }

  cli::cli_alert_success(
    "No table reaches the lower edge of either grid, so the floor never entered the decision."
  )
  invisible(.tab)
}


# 6. The published table -----------------------------------------------------------------------------------------------

#' Fold stability of a selection
#'
#' Counts how many of the independent fold mines chose each term. A term chosen by every fold is
#' stable; one chosen by a single fold is an artifact of that split.
#'
#' @param .mine_dirs Fold mine directories for one configuration.
#' @param ... Selection parameters passed to kw_select.
#' @return Tibble with Class, Term, Folds, PowerMean, PowerMin.
kw_lexicon_stability <- function(.mine_dirs, ...) {
  if (FALSE) {
    .mine_dirs <- dirs_detailed
  }
  purrr::map(.mine_dirs, function(.d) {
    kw_select(.mine = kw_load_mine(.mine_dir = .d), ...) |> dplyr::select(Class, Term, Power)
  }) |>
    purrr::list_rbind() |>
    dplyr::summarise(
      Folds     = dplyr::n(),
      PowerMean = mean(.data$Power),
      PowerMin  = min(.data$Power),
      .by = c(Class, Term)
    )
}

#' Assemble the publishable table
#'
#' The folds pay for the honest estimate; the all-data mine produces the artifact. The stability
#' filter is applied here and nowhere else: mine k trains on every fold but k, so filtering a fold-k
#' lexicon on agreement across all mines would let the held-out fold influence selection. An all-data
#' mine holds nothing out, so the filter is legitimate. The consequence is that the reported precision
#' and coverage describe the unfiltered procedure while the published table carries one further filter
#' that only removes terms.
#'
#' @param .mine_dir_all Directory of the all-data mine.
#' @param .mine_dirs_folds Fold mine directories for the same configuration.
#' @param .tau Power floor for inclusion.
#' @param .min_folds Fold agreement required.
#' @param ... Selection parameters passed to kw_select.
#' @return Tibble ordered by class then Power, ranked within class.
kw_lexicon_final <- function(.mine_dir_all, .mine_dirs_folds, .tau = 0.70, .min_folds = 3L, ...) {
  if (FALSE) {
    .mine_dir_all    <- idx_mine$MineDir[[1]]
    .mine_dirs_folds <- dirs_detailed
    .tau             <- 0.70
    .min_folds       <- 3L
  }
  lex_ <- kw_select(.mine = kw_load_mine(.mine_dir = .mine_dir_all), ...) |>
    dplyr::filter(.data$Power >= .tau)

  stab_ <- kw_lexicon_stability(.mine_dirs = .mine_dirs_folds, ...) |>
    dplyr::select(Class, Term, Folds)

  joined_ <- lex_ |>
    dplyr::left_join(stab_, by = dplyr::join_by(Class, Term)) |>
    dplyr::mutate(Folds = tidyr::replace_na(.data$Folds, 0L))

  n_before_ <- nrow(joined_)
  out_      <- joined_ |> dplyr::filter(.data$Folds >= .min_folds)
  cli::cli_alert_info("Fold agreement >= {(.min_folds)}/5 keeps {nrow(out_)} of {n_before_} terms")

  out_ |>
    dplyr::arrange(.data$Class, dplyr::desc(.data$Power)) |>
    dplyr::mutate(Rank = dplyr::row_number(), .by = Class) |>
    dplyr::select(Class, Rank, Term, Power, Precision, HitsPos, HitsTot, NFilers, FilerRatio,
                  MarginalReach, CumReach, Folds)
}

#' Write the published table
#'
#' Parquet for downstream use and CSV because the artifact is meant to be opened by readers who will
#' not have an R session.
#'
#' @param .tab Output of kw_lexicon_final.
#' @param .dir Output directory.
#' @param .stem File stem.
#' @return Written paths, invisibly.
kw_save_lexicon <- function(.tab, .dir, .stem = "keyword_table") {
  if (FALSE) {
    .tab  <- tab_keywords
    .dir  <- .lP$Output$Table
    .stem <- "keyword_table_detailed"
  }
  fs::dir_create(.dir)
  paths_ <- c(fs::path(.dir, paste0(.stem, ".parquet")), fs::path(.dir, paste0(.stem, ".csv")))
  arrow::write_parquet(.tab, paths_[[1]])
  readr::write_csv(.tab, paths_[[2]])
  cli::cli_alert_success("Wrote {nrow(.tab)} terms across {dplyr::n_distinct(.tab$Class)} categories")
  invisible(paths_)
}


# 7. The generated arm -------------------------------------------------------------------------------------------------

#' Score a supplied term list on named folds
#'
#' The engine's term-file mode computes statistics and incidence for exactly the pairs supplied and
#' mines nothing, so a supplied list is measured on the same folds by the same rule as a mined one.
#' Terms absent from the corpus, or unable to survive tokenisation, are reported by the engine as
#' unmatchable rather than dropped without notice.
#'
#' The evidence and reach floors are opened here so that a supplied list is measured rather than
#' re-pruned, but the precision gate is not: it is the promise the table makes, and both arms must
#' clear the same one or the comparison is meaningless.
#'
#' @param .path_data Prepared parquet.
#' @param .terms_file Parquet of (Class, Term) pairs.
#' @param .label_col Task the list addresses.
#' @param .source Field to score against.
#' @param .nwords Truncation, matching the mined arm.
#' @param .stopwords Stopword regime, matching the mined arm. This is not cosmetic. Under a regime
#'   the miner forms n-grams after removal, so a mined term such as "corporation borrower" describes
#'   two words that are not adjacent in the raw text and cannot fire unless the same regime is applied
#'   here. Scoring a mined list under a different regime silently returns zero hits for every term.
#' @param .folds Folds to score. Where the list was written after reading documents, this must name
#'   only folds those documents did not come from.
#' @param .tau Power threshold.
#' @param .min_precision Precision gate, matching the mined arm.
#' @param .mines_root,.runs_root Output directories.
#' @param .python,.script Interpreter and miner paths.
#' @return Tibble of per-fold metrics.
kw_terms_evaluate <- function(.path_data, .terms_file, .label_col, .source, .nwords, .stopwords,
                              .folds, .tau, .min_precision, .mines_root, .runs_root, .python,
                              .script) {
  if (FALSE) {
    .path_data     <- .lP$Input$Prepared
    .terms_file    <- .lP$Input$TermsDetailed
    .label_col     <- "ClassDetailed"
    .source        <- "text"
    .nwords        <- 256L
    .stopwords     <- "english_domain"
    .folds         <- 5L
    .tau           <- 0.70
    .min_precision <- 0.95
    .mines_root    <- .lP$Output$Mines
    .runs_root     <- .lP$Output$Runs
  }
  if (!fs::file_exists(.terms_file)) cli::cli_abort("No term list at {(.terms_file)}")

  purrr::walk(.folds, function(.f) {
    kw_mine(
      .path_data  = .path_data,
      .label_col  = .label_col,
      .source     = .source,
      .test_fold  = .f,
      .nwords     = .nwords,
      .stopwords  = .stopwords,
      .terms_file = .terms_file,
      .mines_root = .mines_root,
      .python     = .python,
      .script     = .script
    )
  })

  # Selecting on task, source and fold alone would also match every OTHER list scored for the same
  # task, and the results would be silently averaged across them. The manifest records the list each
  # mine consumed, so the mine is identified by its input rather than by its shape.
  want_ <- as.character(fs::path_abs(.terms_file))
  idx_  <- kw_mine_index(.mines_root = .mines_root, .mode = "manual") |>
    dplyr::filter(
      .data$LabelCol  == .label_col,
      .data$Source    == .source,
      .data$NWords    == .nwords,
      .data$Stopwords == .stopwords,
      .data$TermsFile == want_,
      .data$Fold %in% .folds
    )
  if (nrow(idx_) != length(.folds)) {
    cli::cli_abort(c(
      "Expected {length(.folds)} mine{?s} for {(want_)}, found {nrow(idx_)}.",
      "i" = "More than expected usually means mines written under an earlier naming scheme are still \
             present; remove the kwmanual folders under the mines directory and re-run."
    ))
  }

  purrr::map(idx_$MineDir, function(.d) {
    kw_evaluate(
      .mine            = kw_load_mine(.mine_dir = .d),
      .min_precision   = .min_precision,
      .min_hits        = 1L,
      .min_tot         = 1L,
      .min_reach       = 0,
      .max_terms       = 1000L,
      .min_filers      = 0L,
      .min_filer_ratio = 0,
      .drop_repeats    = FALSE,
      .tau             = .tau,
      .runs_root       = .runs_root
    )
  }) |>
    purrr::list_rbind()
}

#' Combine a mined lexicon and a supplied list into one term file
#'
#' The mined half must come from a fold mine trained on exactly the folds the supplied list was
#' written from. Taking it from the all-data mine instead would give the union a half that has seen
#' the evaluation fold while the other half has not.
#'
#' @param .lexicon Mined lexicon from the appropriate fold mine.
#' @param .terms_file Supplied term list.
#' @param .path_out Destination parquet.
#' @return Combined tibble, invisibly.
kw_terms_union <- function(.lexicon, .terms_file, .path_out) {
  if (FALSE) {
    .lexicon    <- lex_mined_holdout
    .terms_file <- .lP$Input$TermsDetailed
    .path_out   <- .lP$Output$Union
  }
  out_ <- dplyr::bind_rows(
    .lexicon |> dplyr::select(Class, Term) |> dplyr::mutate(Origin = "mined"),
    arrow::read_parquet(.terms_file) |> dplyr::select(Class, Term) |> dplyr::mutate(Origin = "generated")
  ) |>
    dplyr::distinct(Class, Term, .keep_all = TRUE) |>
    dplyr::arrange(.data$Class, .data$Term)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)
  invisible(out_)
}


# 8. Compute: summaries for reporting ----------------------------------------------------------------------------------

#' Summarise performance across the mining axes
#'
#' @param .tab Output of kw_sweep_mines.
#' @param .label_col Task to summarise.
#' @return Tibble, one row per mining configuration.
kw_summarise_mines <- function(.tab, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab       <- perf_mines
    .label_col <- "ClassDetailed"
  }
  .tab |>
    dplyr::filter(.data$LabelCol == .label_col) |>
    dplyr::summarise(
      nFolds     = dplyr::n(),
      mTerms     = round(mean(.data$nTerms)),
      mClasses   = round(mean(.data$nClassesHit)),
      mCoverage  = mean(.data$Coverage),
      mPrecision = mean(.data$SelPrecision, na.rm = TRUE),
      mMacroF1   = mean(.data$F1_macro),
      sMacroF1   = stats::sd(.data$F1_macro),
      .by = c(Source, Stopwords, NWords, NgramMax)
    ) |>
    dplyr::arrange(.data$Source, .data$Stopwords, .data$NWords, .data$NgramMax)
}

#' Summarise performance across the selection axes
#'
#' @param .tab Output of kw_sweep_selection.
#' @param .label_col Task to summarise.
#' @return Tibble, one row per selection configuration.
kw_summarise_selection <- function(.tab, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab       <- perf_sel
    .label_col <- "ClassDetailed"
  }
  .tab |>
    dplyr::filter(.data$LabelCol == .label_col) |>
    dplyr::summarise(
      nFolds     = dplyr::n(),
      mTerms     = round(mean(.data$nTerms)),
      mClasses   = round(mean(.data$nClassesHit)),
      mCoverage  = mean(.data$Coverage),
      mPrecision = mean(.data$SelPrecision, na.rm = TRUE),
      mMacroF1   = mean(.data$F1_macro),
      .by = c(MinPrecision, MinHits, MinTot, MinReach, MaxTerms)
    ) |>
    dplyr::arrange(dplyr::desc(.data$mPrecision), dplyr::desc(.data$mCoverage))
}

#' Choose a configuration by precision, breaking ties on coverage
#'
#' Precision is the promise, so it decides; coverage breaks ties inside a band the sample cannot
#' measure. `.prefer` names axes chosen on grounds no column reports -- readability of the terms --
#' and is applied only among rows already tied on the promise.
#'
#' The coverage floor is a PUBLICATION criterion: a table labelling less than a quarter of the corpus
#' is not a usable artifact. It is not a criterion for a routing arm, where a source that commits
#' rarely and is nearly always right is exactly what a cascade wants and the transformer backfills
#' the rest. Applying one threshold to both purposes silently deletes the second: a source clearing
#' no configuration at the publication bar used to return zero rows into a bind, and every downstream
#' step -- mine selection, run writing, the router's arm inventory -- inherited the absence without
#' an error anywhere.
#'
#' So the floor is now advisory. Where nothing clears it, the best configuration is returned anyway,
#' flagged `Publishable = FALSE` and announced. The published table filters on that flag; 03D does
#' not, and judges an arm by the coverage the inventory reports.
#'
#' @param .tab Summarised sweep (kw_summarise_mines or kw_summarise_selection).
#' @param .tolerance Precision band treated as a tie.
#' @param .min_coverage Coverage a configuration needs to be publishable.
#' @param .prefer Named list of preferred axis levels, applied only among tied rows.
#' @param .fallback Logical. Return the best configuration even where none clears .min_coverage.
#' @return One row, carrying a Publishable flag. Zero rows only where .tab was empty.
kw_choose_config <- function(.tab, .tolerance = 0.01, .min_coverage = 0.25, .prefer = NULL,
                             .fallback = TRUE) {
  if (FALSE) {
    .tab          <- kw_summarise_mines(.tab = perf_mines)
    .tolerance    <- 0.01
    .min_coverage <- 0.25
    .prefer       <- list(Stopwords = "english_domain")
    .fallback     <- TRUE
  }
  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning("kw_choose_config received no rows; nothing to choose from.")
    return(.tab |> dplyr::mutate(Publishable = logical()))
  }

  ok_    <- .tab |> dplyr::filter(.data$mCoverage >= .min_coverage)
  pub_   <- nrow(ok_) > 0L
  cand_  <- if (pub_) ok_ else .tab

  if (!pub_) {
    if (!.fallback) {
      cli::cli_alert_warning(
        "No configuration reaches {tbl_pct(.min_coverage)} coverage and .fallback is FALSE; \\
         returning nothing."
      )
      return(.tab[0, ] |> dplyr::mutate(Publishable = logical()))
    }
    cli::cli_alert_warning(
      "No configuration reaches {tbl_pct(.min_coverage)} coverage (best is \\
       {tbl_pct(max(.tab$mCoverage))}). Returning the best anyway, flagged not publishable -- it is \\
       still a usable routing arm."
    )
  }

  tied_ <- cand_ |>
    dplyr::filter(.data$mPrecision >= max(.data$mPrecision, na.rm = TRUE) - .tolerance)

  if (!is.null(.prefer)) {
    wanted_ <- purrr::reduce(names(.prefer), function(.acc, .col) {
      if (!.col %in% names(tied_)) return(.acc)
      .acc & tied_[[.col]] %in% .prefer[[.col]]
    }, .init = rep(TRUE, nrow(tied_)))
    if (any(wanted_)) tied_ <- tied_[wanted_, ]
  }

  tied_ |>
    dplyr::arrange(dplyr::desc(.data$mCoverage), dplyr::desc(.data$mPrecision)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::mutate(Publishable = pub_)
}

#' Survival of proposed terms through the precision and evidence gates
#'
#' @param .proposed Tibble of proposed (Class, Term) pairs.
#' @param .lexicon Surviving lexicon from kw_evaluate.
#' @return Tibble, one row per class.
kw_survival <- function(.proposed, .lexicon) {
  if (FALSE) {
    .proposed <- arrow::read_parquet(.lP$Input$TermsDetailed)
    .lexicon  <- perf_generated$Lexicon[[1]]
  }
  .proposed |>
    dplyr::count(.data$Class, name = "nProposed") |>
    dplyr::left_join(.lexicon |> dplyr::count(.data$Class, name = "nSurvived"),
                     by = dplyr::join_by(Class)) |>
    dplyr::mutate(
      nSurvived    = tidyr::replace_na(.data$nSurvived, 0L),
      ShareSurvive = .data$nSurvived / .data$nProposed
    ) |>
    dplyr::arrange(.data$nSurvived)
}

#' Compare term lists on identical folds
#'
#' @param .tabs Named list of kw_evaluate outputs, names becoming the List column.
#' @return Tibble, one row per list.
kw_summarise_arms <- function(.tabs) {
  if (FALSE) {
    .tabs <- list(mined = perf_final, generated = perf_generated)
  }
  purrr::imap(.tabs, \(.t, .n) .t |> dplyr::mutate(List = .n)) |>
    purrr::list_rbind() |>
    dplyr::summarise(
      nTerms       = round(mean(.data$nTerms)),
      nClassesHit  = round(mean(.data$nClassesHit)),
      Coverage     = mean(.data$Coverage),
      SelPrecision = mean(.data$SelPrecision, na.rm = TRUE),
      F1_macro     = mean(.data$F1_macro),
      .by = List
    )
}


# 9. Report ------------------------------------------------------------------------------------------------------------

#' Report the mining axes
#'
#' @param .tab Output of kw_sweep_mines.
#' @param .label_col Task to report.
#' @return The compact tibble, invisibly.
kw_report_mines <- function(.tab, .label_col = "ClassDetailed") {
  if (FALSE) {
    .tab       <- perf_mines
    .label_col <- "ClassDetailed"
  }
  out_ <- kw_summarise_mines(.tab = .tab, .label_col = .label_col)

  cli::cli_h2("Mining axes: {(.label_col)}")
  out_ |>
    dplyr::mutate(
      Window    = dplyr::if_else(.data$NWords == 0L, "full", as.character(.data$NWords)),
      Coverage  = tbl_pct(.data$mCoverage),
      Precision = tbl_pct(.data$mPrecision),
      MacroF1   = sprintf("%.3f +/- %.3f", .data$mMacroF1, .data$sMacroF1)
    ) |>
    dplyr::select(Source, Stopwords, Window, NgramMax, nFolds, Terms = mTerms,
                  Classes = mClasses, Coverage, Precision, MacroF1) |>
    tbl_say()
  cli::cli_alert_info(
    "Precision is measured on the classified subset and is the number the table promises. \\
     A Classes count well below the number of categories means the threshold is removing thin \\
     categories by arithmetic, not judging their terms."
  )
  invisible(out_)
}

#' Report the selection axes
#'
#' @param .tab Output of kw_sweep_selection.
#' @param .label_col Task to report.
#' @param .n Rows to show.
#' @return The compact tibble, invisibly.
kw_report_selection <- function(.tab, .label_col = "ClassDetailed", .n = 12L) {
  if (FALSE) {
    .tab       <- perf_sel
    .label_col <- "ClassDetailed"
    .n         <- 12L
  }
  out_ <- kw_summarise_selection(.tab = .tab, .label_col = .label_col)

  cli::cli_h2("Selection floors: {(.label_col)}")
  out_ |>
    utils::head(n = .n) |>
    dplyr::mutate(
      Coverage  = tbl_pct(.data$mCoverage),
      Precision = tbl_pct(.data$mPrecision),
      MacroF1   = sprintf("%.3f", .data$mMacroF1)
    ) |>
    dplyr::select(MinReach, MaxTerms, nFolds, Terms = mTerms, Classes = mClasses, Coverage,
                  Precision, MacroF1) |>
    tbl_say()
  cli::cli_alert_info(
    "Marginal reach is the greedy acceptance bar: raising it shortens the list. Rows differing \\
     only in MaxTerms and returning identical numbers mean the cap is not binding."
  )
  invisible(out_)
}

#' Report the threshold curve and the chosen operating point
#'
#' @param .curve Output of kw_tau_curve.
#' @param .target_precision Precision the table must clear.
#' @param .min_classes Categories the table must label.
#' @param .every Print every Nth row; the curve is dense.
#' @return The operating point, invisibly.
kw_report_tau <- function(.curve, .target_precision = 0.95, .tolerance = 0.01, .min_classes = 1L,
                          .every = 2L) {
  if (FALSE) {
    .curve            <- curve_detailed
    .target_precision <- 0.95
    .tolerance        <- 0.01
    .min_classes      <- 9L
    .every            <- 2L
  }
  cli::cli_h2("Precision and coverage across the threshold")
  .curve |>
    dplyr::filter(dplyr::row_number() %% .every == 1L) |>
    dplyr::mutate(
      Tau          = sprintf("%.2f", .data$Tau),
      Coverage     = tbl_pct(.data$Coverage),
      SelPrecision = tbl_pct(.data$SelPrecision),
      Accuracy     = sprintf("%.3f", .data$Accuracy),
      F1_macro     = sprintf("%.3f", .data$F1_macro)
    ) |>
    dplyr::select(Tau, Terms = nTerms, Classes = nClassesHit, Coverage, SelPrecision, Accuracy,
                  F1_macro) |>
    tbl_say()

  op_ <- kw_operating_point(.curve = .curve, .target_precision = .target_precision,
                            .tolerance = .tolerance, .min_classes = .min_classes)
  cli::cli_alert_success(
    "Operating point tau = {sprintf('%.2f', op_$Tau)}: {op_$nTerms} terms across \\
     {op_$nClassesHit} categories label {tbl_pct(op_$Coverage)} of documents at \\
     {tbl_pct(op_$SelPrecision)} precision"
  )
  cli::cli_alert_info(
    "The threshold is a floor on Power, not on precision, so quote the realised precision above \\
     and never the threshold. Precision rising while Classes falls means coverage is being bought \\
     by dropping categories."
  )
  invisible(op_)
}

#' Report the published table, category by category
#'
#' @param .tab Output of kw_lexicon_final.
#' @param .top Terms shown per category.
#' @return .tab, invisibly.
kw_report_lexicon <- function(.tab, .top = 8L) {
  if (FALSE) {
    .tab <- tab_keywords
    .top <- 8L
  }
  cli::cli_h2("Keyword table, sorted by Power within category")
  purrr::walk(sort(unique(.tab$Class)), function(.c) {
    .tab |>
      dplyr::filter(.data$Class == .c) |>
      utils::head(n = .top) |>
      dplyr::mutate(
        Power     = sprintf("%.3f", .data$Power),
        Precision = sprintf("%.3f", .data$Precision),
        Filers    = sprintf("%d (%.2f)", .data$NFilers, .data$FilerRatio),
        MargReach = tbl_pct(.data$MarginalReach),
        CumReach  = tbl_pct(.data$CumReach),
        Stability = paste0(.data$Folds, "/5")
      ) |>
      dplyr::select(Rank, Term, Power, Precision, HitsPos, Filers, MargReach, CumReach, Stability) |>
      tbl_say(.title = .c)
  })
  cli::cli_alert_info(
    "Marginal reach is the share of the category a term adds beyond its predecessors; a filer ratio \\
     far below one marks a term concentrated in a single registrant's own template."
  )
  invisible(.tab)
}

#' Report why documents received the labels they did
#'
#' Sampled per predicted category rather than by score, because every document a term fires in
#' carries that term's Power identically and a global sort returns one term repeatedly.
#'
#' @param .tab_pred Pooled predictions carrying TopTerm.
#' @param .per_class Rows per predicted category.
#' @param .wrong Show misclassifications rather than correct labels.
#' @return The shown tibble, invisibly.
kw_report_audit <- function(.tab_pred, .per_class = 2L, .wrong = FALSE) {
  if (FALSE) {
    .tab_pred  <- pred_final
    .per_class <- 2L
    .wrong     <- FALSE
  }
  out_ <- .tab_pred |>
    dplyr::filter(.data$PredLabel != KW_NONE) |>
    dplyr::mutate(Correct = .data$PredLabel == .data$TrueLabel) |>
    dplyr::filter(.data$Correct != .wrong) |>
    dplyr::slice_max(.data$Score, n = .per_class, by = PredLabel, with_ties = FALSE) |>
    dplyr::arrange(.data$PredLabel, dplyr::desc(.data$Score))

  cli::cli_h2("Term responsible for each label: {if (.wrong) 'errors' else 'correct'}")
  out_ |>
    dplyr::mutate(Power = sprintf("%.3f", .data$Score)) |>
    dplyr::select(DocID, TrueLabel, PredLabel, Power, TopTerm) |>
    tbl_say()
  cli::cli_alert_info(
    "A term appearing repeatedly in the error block is a candidate for removal; a term appearing \\
     in both blocks is ambiguous rather than wrong."
  )
  invisible(out_)
}

#' Report the term lists side by side
#'
#' @param .tabs Named list of kw_evaluate outputs.
#' @return The comparison tibble, invisibly.
kw_report_arms <- function(.tabs) {
  if (FALSE) {
    .tabs <- list(mined = perf_final, generated = perf_generated, union = perf_union)
  }
  out_ <- kw_summarise_arms(.tabs = .tabs)

  cli::cli_h2("Where the terms came from")
  out_ |>
    dplyr::mutate(
      Coverage     = tbl_pct(.data$Coverage),
      SelPrecision = tbl_pct(.data$SelPrecision),
      F1_macro     = sprintf("%.3f", .data$F1_macro)
    ) |>
    dplyr::select(List, Terms = nTerms, Classes = nClassesHit, Coverage, SelPrecision, F1_macro) |>
    tbl_say()
  cli::cli_alert_info(
    "Read Classes first. A union matching the arms on precision while labelling more categories \\
     means the two arms recover different vocabulary; matching on all columns means one arm is \\
     redundant."
  )
  invisible(out_)
}

#' Every report in order
#'
#' The block to copy out when something needs checking.
#'
#' @param .perf_mines,.perf_sel Sweep outputs.
#' @param .curve Threshold curve.
#' @param .tab_pred Pooled predictions at the operating point.
#' @param .tab_lexicon Published table.
#' @param .label_col Task to report.
#' @param .target_precision,.min_classes Operating-point constraints.
#' @return NULL, invisibly.
kw_report_all <- function(.perf_mines, .perf_sel, .curve, .tab_pred, .tab_lexicon,
                          .label_col = "ClassDetailed", .target_precision = 0.95,
                          .min_classes = 1L) {
  if (FALSE) {
    .perf_mines       <- perf_mines
    .perf_sel         <- perf_sel
    .curve            <- curve_detailed
    .tab_pred         <- pred_final
    .tab_lexicon      <- tab_keywords
    .label_col        <- "ClassDetailed"
    .target_precision <- 0.95
    .min_classes      <- 9L
  }
  kw_report_mines(.tab = .perf_mines, .label_col = .label_col)
  kw_report_selection(.tab = .perf_sel, .label_col = .label_col)
  kw_report_tau(.curve = .curve, .target_precision = .target_precision, .min_classes = .min_classes)
  clf_report_scores(.tab_pred, .title = "At the operating point")
  clf_report_perclass(.tab_pred, .title = "Per category")
  kw_report_audit(.tab_pred = .tab_pred, .per_class = 2L, .wrong = TRUE)
  kw_report_lexicon(.tab = .tab_lexicon, .top = 8L)
  invisible(NULL)
}


#' Selection floors for one task, chosen once and reused across its windows
#'
#' The floors govern how long a list may grow and how much marginal reach a term must add. Neither is
#' a property of how much text the miner read, so they are settled once per task and held while the
#' window varies. Sweeping them again per window would treat a question about list length as though
#' it depended on truncation, and would multiply the selection sweep by the size of the window axis
#' for an answer that does not move.
#'
#' @param .perf_sel Selection sweep across every task.
#' @param .label_col Character. Task.
#' @param .tolerance Numeric. Precision band treated as indistinguishable.
#' @param .min_coverage Numeric. Coverage below which a configuration is not publishable.
#' @return One-row tibble of floors.
kw_task_floors <- function(.perf_sel, .label_col, .tolerance = 0.01, .min_coverage = 0.25) {
  if (FALSE) {
    .perf_sel     <- perf_sel
    .label_col    <- "ClassDetailed"
    .tolerance    <- 0.01
    .min_coverage <- 0.25
  }
  kw_summarise_selection(.tab = .perf_sel, .label_col = .label_col) |>
    kw_choose_config(
      .tolerance    = .tolerance,
      .min_coverage = .min_coverage,
      .prefer       = NULL,
      .fallback     = TRUE
    )
}

#' Publish one table: one task, one truncation window
#'
#' Every window is published, not only the crowned one. The window is the keyword arm's cost knob and
#' its coverage knob at once -- a shorter window is cheaper to apply and more precise, a longer one
#' labels more -- so which window to ship is a decision worth leaving open until someone has the
#' throughput figures. Publishing them all costs a threshold curve each and a few kilobytes of
#' parquet, which is nothing against the alternative of re-mining to change one's mind.
#'
#' The n-gram order, stopword regime and text source are held at the values this task's mining sweep
#' crowned. Those are not deployment knobs: nobody applying a published table chooses to read the
#' filer's title instead of the contract, or to match two-word phrases instead of three. They were
#' settled on the evidence and stay settled.
#'
#' @param .label_col Character. Task to publish.
#' @param .n_words Integer. Truncation window in words; 0 reads the whole document.
#' @param .idx_mine Full mining index, including the all-data mine at fold zero.
#' @param .cfg_mine Crowned mining configuration per task and source.
#' @param .floors One-row tibble from kw_task_floors().
#' @param .target_precision Numeric. The promise this task makes.
#' @param .min_classes Integer. Categories the table must reach before a threshold is acceptable.
#' @param .taus Numeric vector of evidence thresholds to trace.
#' @param .tolerance Numeric. Precision band treated as indistinguishable.
#' @param .min_folds Integer. Fold agreement required of a published term.
#' @param .source Character. Text field the arm reads.
#' @return List: LabelCol, NWords, Source, NgramMax, Stopwords, MinTokenLen, Mode, PositiveClass,
#'   ConfigName, Dirs, DirAll, Promised, Floors, Curve, Operating, Lexicon.
kw_publish_lexicon <- function(.label_col, .n_words, .idx_mine, .cfg_mine, .floors,
                               .target_precision, .min_classes,
                               .taus = seq(0.40, 0.96, by = 0.02), .tolerance = 0.01,
                               .min_folds = 3L, .source = "text") {
  if (FALSE) {
    .label_col        <- "ClassDetailed"
    .n_words          <- 512L
    .idx_mine         <- idx_mine
    .cfg_mine         <- cfg_mine
    .floors           <- kw_task_floors(perf_sel, "ClassDetailed")
    .target_precision <- 0.95
    .min_classes      <- 9L
    .taus             <- seq(0.40, 0.96, by = 0.02)
    .tolerance        <- 0.01
    .min_folds        <- 3L
    .source           <- "text"
  }
  cells_ <- kw_publish_cells(
    .label_col = .label_col,
    .n_words   = .n_words,
    .idx_mine  = .idx_mine,
    .cfg_mine  = .cfg_mine,
    .source    = .source
  )

  curve_ <- kw_tau_curve(
    .mine_dirs     = cells_$Dirs,
    .taus          = .taus,
    .min_precision = .target_precision,
    .min_hits      = .floors$MinHits,
    .min_tot       = .floors$MinTot,
    .min_reach     = .floors$MinReach,
    .max_terms     = .floors$MaxTerms
  )

  op_ <- kw_operating_point(
    .curve            = curve_,
    .target_precision = .target_precision,
    .tolerance        = .tolerance,
    .min_classes      = .min_classes
  )

  # The all-data mine holds nothing out, which is what makes the fold-agreement filter legitimate
  # against it and illegitimate against any single fold mine.
  lex_ <- kw_lexicon_final(
    .mine_dir_all    = cells_$DirAll,
    .mine_dirs_folds = cells_$Dirs,
    .tau             = op_$Tau,
    .min_folds       = .min_folds,
    .min_precision   = .target_precision,
    .min_hits        = .floors$MinHits,
    .min_tot         = .floors$MinTot,
    .min_reach       = .floors$MinReach,
    .max_terms       = .floors$MaxTerms
  )

  mode_ <- kw_mode(.label_col = .label_col)

  # THE CONFIGURATION NAME IS COMPUTED HERE, once, and everything else reads it. It is the join key
  # between the artifact and the runs that estimate it, exactly as the transformer's is, so a second
  # construction site would be a second answer to "which runs measured this table" -- and the two
  # would diverge on the first axis anyone added.
  cfg_name_ <- kw_config_name(
    .label_col     = .label_col,
    .source        = .source,
    .nwords        = .n_words,
    .ngram_max     = cells_$NgramMax,
    .stopwords     = cells_$Stopwords,
    .min_precision = .target_precision,
    .min_hits      = .floors$MinHits,
    .min_tot       = .floors$MinTot,
    .min_reach     = .floors$MinReach,
    .max_terms     = .floors$MaxTerms,
    .tau           = op_$Tau,
    .seed          = 42L,
    .terms_tag     = NULL
  )

  list(
    LabelCol = .label_col, NWords = as.integer(.n_words), Source = .source,
    NgramMax = cells_$NgramMax, Stopwords = cells_$Stopwords,
    MinTokenLen = cells_$MinTokenLen,
    Mode = mode_$Mode, PositiveClass = mode_$PositiveClass,
    ConfigName = cfg_name_,
    # Carried rather than recomputed downstream. Which cells belong to this published table is a
    # filter over three crowned axes, and a caller rebuilding it would be a second place that has to
    # agree about what the crowned axes were.
    Dirs = cells_$Dirs, DirAll = cells_$DirAll,
    Promised = .target_precision, Floors = .floors,
    Curve = curve_, Operating = op_, Lexicon = lex_
  )
}

#' Out-of-fold runs for one published table
#'
#' WHAT THIS ESTIMATES, AND WHAT IT DELIBERATELY DOES NOT. The published table is mined on all the
#' labelled data and then filtered on fold agreement, so scoring it against any fold's held-out
#' documents would score it on documents it was built from. The number would be optimistic and would
#' sit in the orchestration stage's leaderboard beside transformer arms whose fold scores are
#' genuinely held out.
#'
#' So this estimates the PROCEDURE, the way the transformer arm does: mine at these axes, select at
#' these floors, decide at this threshold, measured on documents the mine never read. The artifact is
#' the all-data table, exactly as the transformer's artifact is an all-data refit whose weights are
#' never scored out of fold either. The two are joined by configuration name, which is why that name
#' is computed once upstream and asserted here rather than rebuilt.
#'
#' The estimate describes the procedure BEFORE the fold-agreement filter, which can only remove terms.
#' That gap is stated where the filter is applied and is the price of a filter that would otherwise
#' let a held-out fold influence its own selection.
#'
#' @param .pub One kw_publish_lexicon() result.
#' @param .runs_root Runs directory, the same one the sweep writes to.
#' @param .seed Integer. Stamped for parity with the other engines.
#' @return Tibble of the per-fold metric rows, one per fold.
kw_publish_runs <- function(.pub, .runs_root, .seed = 42L) {
  if (FALSE) {
    .pub       <- pubs[[1]]
    .runs_root <- .lP$Output$Runs
    .seed      <- 42L
  }
  out_ <- purrr::map(.pub$Dirs, function(.d) {
    kw_evaluate(
      .mine          = kw_load_mine(.mine_dir = .d),
      .min_precision = .pub$Promised,
      .min_hits      = .pub$Floors$MinHits,
      .min_tot       = .pub$Floors$MinTot,
      .min_reach     = .pub$Floors$MinReach,
      .max_terms     = .pub$Floors$MaxTerms,
      .tau           = .pub$Operating$Tau,
      .runs_root     = .runs_root,
      .seed          = .seed
    )
  }) |>
    purrr::list_rbind()

  # The assertion is the point of the function. Two independent constructions of one identifier is
  # exactly the shape that lets a manifest pin an artifact nobody measured, and it fails silently:
  # the runs land under one name, the catalogue advertises another, and the orchestration stage's
  # join simply returns nothing rather than reporting a mismatch.
  seen_ <- unique(out_$ConfigName)
  if (length(seen_) != 1L || !identical(seen_, .pub$ConfigName)) {
    cli::cli_abort(c(
      "The runs written for {(.pub$LabelCol)} at window {(.pub$NWords)} do not carry the published \\
       configuration name.",
      "i" = "Published: {(.pub$ConfigName)}",
      "i" = "Written: {seen_}"
    ))
  }
  out_ |> dplyr::select(ConfigName, Run, TestFold, Coverage, SelPrecision, Accuracy, F1_macro,
                        nTerms, nClassesHit)
}

#' Do the two hit producers agree
#'
#' The mine's incidence table and the engine implement one tokenisation in one place, but they reach
#' it by different routes: the miner transformed the held-out fold against its full candidate
#' vocabulary while mining, and the engine transforms the same documents against the lexicon alone.
#' If those ever diverge, every published precision becomes a number about a system nobody runs, and
#' the divergence is invisible -- the terms still fire, just on different documents.
#'
#' PREDICTION: the two sets of (DocID, Term) pairs are identical, so MineOnly and EngineOnly are both
#' zero. A nonzero count on either side is a tokenisation disagreement, not a selection one: the
#' lexicon is held fixed across both. MineOnly alone would mean the engine is under-matching, which is
#' the failure that motivated the seam; EngineOnly alone would mean the mine's candidate filters
#' removed a term the lexicon still names.
#'
#' @param .pub One kw_publish_lexicon() result.
#' @param .mine_dir Directory of the fold mine to check against.
#' @param .docs Prepared sample carrying DocID and the text columns.
#' @param .python,.script Interpreter and keyword_apply.py.
#' @return One-row tibble: Task, NWords, Fold, nTerms, nDocs, nMine, nEngine, MineOnly, EngineOnly,
#'   Agree.
kw_parity_check <- function(.pub, .mine_dir, .docs, .python, .script) {
  if (FALSE) {
    .pub      <- pubs[[1]]
    .mine_dir <- pubs[[1]]$Dirs[[1]]
    .docs     <- tab_prepared
    .python   <- .lP$Engine$Python
    .script   <- .lP$Engine$ScriptApply
  }
  mine_ <- kw_load_mine(.mine_dir = .mine_dir)
  lex_  <- kw_select(
    .mine          = mine_,
    .min_precision = .pub$Promised,
    .min_hits      = .pub$Floors$MinHits,
    .min_tot       = .pub$Floors$MinTot,
    .min_reach     = .pub$Floors$MinReach,
    .max_terms     = .pub$Floors$MaxTerms
  ) |>
    dplyr::filter(.data$Power >= .pub$Operating$Tau)

  # The fold's held-out documents, with their text, which the mine does not carry.
  docs_ <- mine_$DocsTest |>
    dplyr::select(DocID) |>
    dplyr::inner_join(.docs, by = dplyr::join_by(DocID))

  from_mine_ <- kw_hits_mine(.lexicon = lex_, .mine = mine_) |>
    dplyr::distinct(DocID, Term)
  from_eng_  <- kw_hits_engine(
    .lexicon       = lex_,
    .docs          = docs_,
    .source        = .pub$Source,
    .n_words       = .pub$NWords,
    .stopwords     = .pub$Stopwords,
    .min_token_len = .pub$MinTokenLen,
    .python        = .python,
    .script        = .script
  ) |>
    dplyr::distinct(DocID, Term)

  tibble::tibble(
    Task       = .pub$LabelCol,
    NWords     = .pub$NWords,
    Fold       = as.integer(mine_$Manifest$test_fold),
    nTerms     = dplyr::n_distinct(lex_$Term),
    nDocs      = nrow(docs_),
    nMine      = nrow(from_mine_),
    nEngine    = nrow(from_eng_),
    MineOnly   = nrow(dplyr::anti_join(from_mine_, from_eng_, by = dplyr::join_by(DocID, Term))),
    EngineOnly = nrow(dplyr::anti_join(from_eng_, from_mine_, by = dplyr::join_by(DocID, Term)))
  ) |>
    dplyr::mutate(Agree = .data$MineOnly == 0L & .data$EngineOnly == 0L)
}

#' Print the parity check
#'
#' @param .tab Output of kw_parity_check(), bound across published tables.
#' @return Invisibly .tab.
kw_report_parity <- function(.tab) {
  if (FALSE) .tab <- tab_parity
  cli::cli_h2("Mine and engine, matching the same lexicon")
  .tab |>
    dplyr::mutate(
      Window = dplyr::if_else(.data$NWords == 0L, "full", as.character(.data$NWords)),
      Agree  = dplyr::if_else(.data$Agree, "yes", "NO")
    ) |>
    dplyr::select(Task, Window, Fold, nTerms, nDocs, nMine, nEngine, MineOnly, EngineOnly, Agree) |>
    tbl_say()
  cli::cli_text("")
  n_bad_ <- sum(!.tab$Agree)
  if (n_bad_ == 0L) {
    cli::cli_alert_success(
      "Every published table matches identically whichever producer is asked, so the precision each \\
       one reports is the precision it will realise when applied."
    )
  } else {
    cli::cli_alert_danger(
      "{n_bad_} table{?s} match{?es/} differently through the engine than through the mine. The \\
       lexicon is held fixed across both, so this is a tokenisation disagreement and every published \\
       precision below is measured under a rule deployment will not reproduce."
    )
  }
  invisible(.tab)
}

#' Choose the window a task ships by default
#'
#' The mining sweep crowns a cell by how it scored at a threshold held common across the sweep, which
#' is the right way to compare windows on equal terms and the wrong way to choose an artifact. Each
#' published table stands at its own operating point -- a short window holds precision at a lower
#' evidence bar than a long one, so judging it at the long window's threshold understates it -- and on
#' this sample the two questions give different answers for every task.
#'
#' So the default is chosen from the catalogue rather than inherited from the sweep, in three steps:
#'
#'   1. KEEP WHAT HOLDS THE PROMISE. Realised precision within tolerance of the target, the same test
#'      that chose the threshold. This rarely excludes anything; it is a guard against publishing a
#'      table that does not keep its word rather than the criterion that decides.
#'   2. MOST CATEGORIES. A table covering ten of twelve categories is a different artifact from one
#'      covering five, and no amount of coverage compensates: the categories it omits cannot be
#'      labelled by it at all.
#'   3. MOST COVERAGE, THEN SHORTEST WINDOW. Among equally broad tables, prefer the one that declines
#'      least; among equally broad and equally covering tables, prefer the one that reads least text,
#'      because applying it over a corpus is what the window costs.
#'
#' Breadth is counted on the PUBLISHED file rather than on the threshold curve. The curve's count is
#' estimated before the fold-agreement filter, and that filter can remove the last surviving term for
#' a category -- so the two differ, and only one of them describes what a reader would receive.
#'
#' @param .tab Catalogue rows for every window of every task.
#' @param .tolerance Numeric. Precision band treated as indistinguishable.
#' @return .tab with a logical Default column.
kw_default_window <- function(.tab, .tolerance = 0.01) {
  if (FALSE) {
    .tab       <- catalogue
    .tolerance <- 0.01
  }
  picked_ <- .tab |>
    dplyr::mutate(Holds = .data$Realised >= .data$Promised - .tolerance) |>
    # A task where no window holds its promise still needs a default, or the deployment stage has
    # nothing to apply. Falling back to every window keeps the ranking meaningful and the shortfall
    # visible in the Realised column rather than hidden behind a missing row.
    dplyr::mutate(Holds = if (any(.data$Holds)) .data$Holds else TRUE, .by = Task) |>
    dplyr::filter(.data$Holds) |>
    # A window of zero words means the miner read the whole document, so it is the LONGEST window
    # wearing the smallest number. Sorting on the raw column would make the tiebreak that exists to
    # prefer cheap tables pick the most expensive one, and it would do so only where every other
    # column ties -- which is exactly where nobody would look.
    dplyr::mutate(Cost = dplyr::if_else(.data$NWords == 0L, Inf, as.numeric(.data$NWords))) |>
    dplyr::arrange(.data$Task, dplyr::desc(.data$Categories), dplyr::desc(.data$Coverage),
                   .data$Cost) |>
    dplyr::slice_head(n = 1L, by = Task) |>
    dplyr::transmute(Task, ChosenWords = .data$NWords)

  .tab |>
    dplyr::left_join(picked_, by = dplyr::join_by(Task)) |>
    dplyr::mutate(Default = .data$NWords == .data$ChosenWords, ChosenWords = NULL)
}

#' The published catalogue, one row per table, with the default marked
#'
#' What a reader needs in order to override the default with their eyes open: every window's
#' threshold, what it promised and realised, how much of the corpus it labels, how many categories it
#' reaches and how many terms it carries. The default is marked rather than being the only row,
#' because a window winning by a margin smaller than the sample can measure is a reason to prefer the
#' cheaper one, and only a table showing both makes that judgement possible.
#'
#' @param .pubs List of kw_publish_lexicon() results.
#' @param .tolerance Numeric. Precision band treated as indistinguishable when choosing the default.
#' @return Invisibly the catalogue tibble.
kw_catalogue <- function(.pubs, .tolerance = 0.01) {
  if (FALSE) {
    .pubs      <- pubs
    .tolerance <- 0.01
  }
  purrr::map(.pubs, function(.p) {
    tibble::tibble(
      Task       = .p$LabelCol,
      NWords     = .p$NWords,
      Tau        = .p$Operating$Tau,
      Promised   = .p$Promised,
      Realised   = .p$Operating$SelPrecision,
      Coverage   = .p$Operating$Coverage,
      Reached    = .p$Operating$nClassesHit,
      Terms      = nrow(.p$Lexicon),
      Categories = dplyr::n_distinct(.p$Lexicon$Class),
      # THE APPLIER CONFIGURATION. Not deployment knobs -- nobody applying a table chooses its
      # stopword regime -- but pinned all the same, because they decide which n-grams can form and
      # therefore which documents the table fires on. Pinned is not the same as offered. Without them
      # a downstream stage has to guess, and a guess that happens to be wrong produces ordinary
      # labels on the wrong documents and reports nothing.
      ConfigName    = .p$ConfigName,
      Source        = .p$Source,
      Stopwords     = .p$Stopwords,
      NgramMax      = as.integer(.p$NgramMax),
      MinTokenLen   = as.integer(.p$MinTokenLen),
      Mode          = .p$Mode,
      PositiveClass = .p$PositiveClass
    )
  }) |>
    purrr::list_rbind() |>
    kw_default_window(.tolerance = .tolerance) |>
    dplyr::arrange(.data$Task, .data$NWords)
}

#' Print the catalogue
#'
#' @param .tab Output of kw_catalogue().
#' @return Invisibly .tab.
kw_report_catalogue <- function(.tab) {
  if (FALSE) .tab <- catalogue
  cli::cli_h2("Published tables")
  .tab |>
    dplyr::mutate(
      Window   = dplyr::if_else(.data$NWords == 0L, "full", as.character(.data$NWords)),
      Tau      = sprintf("%.2f", .data$Tau),
      Promised = tbl_pct(.data$Promised),
      Realised = tbl_pct(.data$Realised),
      Coverage = tbl_pct(.data$Coverage),
      Default  = dplyr::if_else(.data$Default, "<-", "")
    ) |>
    dplyr::select(Task, Window, Default, Tau, Promised, Realised, Coverage, Reached, Terms,
                  Categories) |>
    tbl_say()
  cli::cli_text("")
  cli::cli_alert_info(
    "Coverage is the share of documents the table labels at all; Reached is how many categories it \\
     ever assigns. A table holds its promise by declining, so the two are read together."
  )
  cli::cli_alert_info(
    "The arrow marks the widest table that holds its promise, breaking ties on coverage and then on \\
     the shorter window. It is chosen from this table rather than inherited from the mining sweep, \\
     which ranks windows at a common threshold and so understates the short ones."
  )
  cli::cli_alert_info(
    "Where Categories is below Reached, the fold-agreement filter removed the last surviving term \\
     for a category. Reached describes the procedure; Categories describes the file."
  )

  # Long identifiers destroy a console table, and these are constant within a task. Reported once
  # underneath instead, which is also where a reader can see that they were pinned at all.
  cli::cli_text("")
  .tab |>
    dplyr::distinct(Task, Source, Stopwords, NgramMax, MinTokenLen, Mode, PositiveClass) |>
    tbl_say(.title = "Applier configuration, pinned per task")
  cli::cli_text("")
  cli::cli_alert_info(
    "These are not knobs a caller chooses. They decide which n-grams can form and therefore which \\
     documents a table fires on, so they travel with it: a table matched under a different \\
     tokenisation from the one it was mined under produces ordinary labels on the wrong documents."
  )
  invisible(.tab)
}

#' The published table a task ships by default
#'
#' Reads the choice from the catalogue rather than remaking it, so the marked row and the table the
#' rest of this document reports on cannot disagree. Looked up by task and window carried inside each
#' entry, never by position: the catalogue is sorted for reading and the entries are built in plan
#' order, so an index taken from one and applied to the other silently returns a different task's
#' table -- which is not an error anywhere, because a lexicon is a lexicon whatever categories it
#' holds.
#'
#' @param .pubs List of kw_publish_lexicon() results.
#' @param .catalogue Output of kw_catalogue(), which carries the Default column.
#' @param .label_col Character. Task.
#' @return One element of .pubs.
kw_default_pub <- function(.pubs, .catalogue, .label_col) {
  if (FALSE) {
    .pubs      <- pubs
    .catalogue <- catalogue
    .label_col <- "ClassDetailed"
  }
  win_ <- .catalogue |>
    dplyr::filter(.data$Task == .label_col, .data$Default) |>
    dplyr::pull(NWords)
  if (length(win_) != 1L) {
    cli::cli_abort("Expected one default window for {.val {(.label_col)}}, found {length(win_)}.")
  }

  hit_ <- purrr::detect(
    .x = .pubs,
    .f = function(.p) identical(.p$LabelCol, .label_col) && identical(.p$NWords, as.integer(win_))
  )
  if (is.null(hit_)) {
    cli::cli_abort("No published table for {.val {(.label_col)}} at window {win_}.")
  }
  hit_
}

#' File stem for one published table
#'
#' Named by task and window rather than by an index, because the deployment stage reads these files
#' by name and a positional convention shared between two stages is one that will eventually be
#' renumbered in one place and not the other.
#'
#' @param .label_col Character. Task.
#' @param .n_words Integer. Truncation window; 0 reads the whole document.
#' @return Character stem.
kw_table_stem <- function(.label_col, .n_words) {
  if (FALSE) {
    .label_col <- "ClassDetailed"
    .n_words   <- 512L
  }
  task_ <- switch(.label_col,
    ClassDetailed = "detailed",
    ClassBroad    = "broad",
    AmendType     = "amendment",
    tolower(.label_col)
  )
  win_ <- if (as.integer(.n_words) == 0L) "full" else as.character(as.integer(.n_words))
  paste0("keyword_table_", task_, "_W", win_)
}


# 10. Figures ----------------------------------------------------------------------------------------------------------
# The look comes entirely from _Commons/_Plots.R. What these functions own is the mapping from a
# swept tibble to a figure shape, which is the part that has to know what the numbers mean.

#' Performance against the truncation window
#'
#' The reference line marks the window the transformer reads. Anything to its right is signal no
#' transformer in this study can see, so a curve still rising there would locate information the whole
#' pipeline currently discards -- which is a claim about the corpus, not about this classifier.
#'
#' Three panels rather than three figures, because the three metrics move against each other: a window
#' that raises precision by narrowing what the miner sees will lower coverage at the same time, and
#' reading that trade-off requires them side by side on a shared horizontal axis.
#'
#' The vertical scale is free per panel, and deliberately so. What is shared between the metrics is
#' the window, not the level: precision sits near ninety percent while coverage sits near sixty, so a
#' common vertical axis would compress precision's whole range into a line thinner than the marker and
#' the panel would show nothing. The axis label on each panel states its own range, and no comparison
#' this figure invites is a comparison of levels across panels.
#'
#' @param .tab Output of kw_sweep_mines().
#' @param .label_col Character. Task to plot.
#' @param .window_ref Integer. Reference window in words, drawn recessive behind the curves.
#' @return A ggplot.
kw_plot_window <- function(.tab, .label_col = "ClassDetailed", .window_ref = 512L) {
  if (FALSE) {
    .tab        <- perf_mines
    .label_col  <- "ClassDetailed"
    .window_ref <- 512L
  }
  dat_ <- .tab |>
    dplyr::filter(.data$LabelCol == .label_col, .data$Source == "text") |>
    dplyr::summarise(
      MacroF1   = mean(.data$F1_macro),
      Precision = mean(.data$SelPrecision, na.rm = TRUE),
      Coverage  = mean(.data$Coverage),
      .by = c(NWords, NgramMax, Stopwords)
    ) |>
    # A window of zero means the miner read the whole document. Plotted on a log axis it needs a
    # finite position, so it is placed one doubling beyond the largest real window and labelled for
    # what it is rather than for the number standing in for it.
    dplyr::mutate(Window = dplyr::if_else(.data$NWords == 0L, 4096L, .data$NWords)) |>
    tidyr::pivot_longer(
      cols      = c(MacroF1, Precision, Coverage),
      names_to  = "Metric",
      values_to = "Value"
    )

  dat_ |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$Window, y = .data$Value,
      colour = .data$Stopwords, linetype = factor(.data$NgramMax)
    )) +
    ggplot2::geom_vline(xintercept = .window_ref, linewidth = 0.3, linetype = 2, colour = .plot_ref) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::facet_wrap(ggplot2::vars(Metric), nrow = 1L, scales = "free_y") +
    plot_scale_colour_cat(name = "Stopwords") +
    ggplot2::scale_x_continuous(
      transform = "log2",
      breaks    = c(256, 512, 1024, 2048, 4096),
      labels    = c("256", "512", "1024", "2048", "full")
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(x = "Word window", y = NULL, linetype = "n-gram max") +
    plot_theme(.grid = "y", .legend = "bottom")
}

#' Precision bought by declining to label
#'
#' Each point is one evidence threshold. Moving up the curve raises the threshold, which discards the
#' documents whose only supporting term was weak and so raises precision on what remains. Point area
#' is the number of categories the table still reaches, which is the constraint that stops the curve
#' being read as free: a very precise table that has dropped half the taxonomy is not a better table.
#'
#' @param .curve Output of kw_tau_curve().
#' @param .target_precision Numeric. The promise, drawn as a reference line.
#' @return A ggplot.
kw_plot_tau <- function(.curve, .target_precision = 0.95) {
  if (FALSE) {
    .curve            <- curve_detailed
    .target_precision <- 0.95
  }
  .curve |>
    dplyr::filter(!is.na(.data$SelPrecision), .data$Coverage > 0) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Coverage, y = .data$SelPrecision)) +
    ggplot2::geom_hline(
      yintercept = .target_precision, linewidth = 0.3, linetype = 2, colour = .plot_ref
    ) +
    ggplot2::geom_line(linewidth = 0.4, colour = .plot_ref) +
    ggplot2::geom_point(ggplot2::aes(size = .data$nClassesHit), colour = .plot_ink, alpha = 0.8) +
    ggplot2::scale_size_area(max_size = 4) +
    ggplot2::scale_x_continuous(labels = scales::label_percent()) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(
      x = "Coverage", y = "Precision on the classified subset", size = "Categories reached"
    ) +
    plot_theme(.grid = "both", .legend = "right")
}

#' Reach accumulated by successive terms within each category
#'
#' How quickly a category is covered, and where a list stops earning its length. A curve that flattens
#' after four terms says the fifth is decoration; one still climbing at the last term says the list was
#' truncated before the category was covered.
#'
#' Panels follow the registered taxonomic order rather than the alphabet, so this figure can be read
#' against the confusion matrix and the per-category scores without re-establishing which panel is
#' which. Every registered category gets a panel even when the published table holds no term for it:
#' an empty panel is the statement that the mining found nothing publishable for that category, and
#' dropping it would remove from the figure precisely the categories a reader most needs warning
#' about.
#'
#' @param .tab Output of kw_lexicon_final().
#' @param .key Character. Registered vocabulary ordering and labelling the panels.
#' @return A ggplot.
kw_plot_reach <- function(.tab, .key = "ClassDetailed") {
  if (FALSE) {
    .tab <- tab_keywords
    .key <- "ClassDetailed"
  }
  .tab |>
    dplyr::mutate(Panel = plot_factor(.data$Class, .key = .key, .short = TRUE)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Rank, y = .data$CumReach, group = .data$Panel)) +
    ggplot2::geom_step(linewidth = 0.4, colour = .plot_ink) +
    ggplot2::geom_point(size = 1.0, colour = .plot_ink) +
    ggplot2::facet_wrap(ggplot2::vars(Panel), ncol = 4L, drop = FALSE) +
    # Rank counts terms, so the axis takes whole numbers. The default continuous breaks land on
    # halves, which reads as though a term could be the two-and-a-halfth in its list.
    ggplot2::scale_x_continuous(breaks = \(.x) unique(round(scales::breaks_pretty(4)(.x)))) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(x = "Term rank within category", y = "Cumulative share of category reached") +
    plot_theme(.grid = "y", .legend = "none")
}
