# 03C-ClassifyTrainKeyword.R -- library for the keyword table
#
# The deliverable of this stage is an artifact rather than a model: a short, ranked, human-readable
# table of terms per contract type that can be applied to EDGAR without a GPU, with a stated
# precision and a stated coverage. Every design decision below follows from that goal, and where a
# choice would differ had the goal been accuracy, the roxygen says so.
#
# THE SEAM
# Mining is expensive, selection is cheap. contracts-engine/keyword_train.py tokenises the sample and
# writes per-(term, class) statistics plus incidence; every floor, the greedy rule, the decision rule
# and the thresholds live here. That split keeps the mining grid at a few hundred cells while leaving
# the selection sweep interactive, and it makes the engine task-agnostic: the amendment decision rule
# is a decision, so it lives in R.
#
# POWER
# Wilson 95% lower bound on training precision. Bounded in [0, 1], monotone in both precision and
# evidence: a term seen 2 times in 2 documents scores 0.342, one seen 190 times in 200 scores 0.910,
# where raw precision ranks the first ahead of the second. Power is the sort key of the published
# table and the evidence gate at scoring time. It is NOT the precision gate -- see kw_select.
#
# Sources 03A for the shared layer (clf_scores, clf_perclass, clf_confusion, clf_leaderboard,
# clf_say_table, clf_pct, clf_apply_theme). Run folders are written in the same schema the BERT
# trainer uses, so those functions consume keyword runs without special-casing.

KW_NONE <- "(none)"

`%||%` <- function(.x, .y) if (is.null(.x)) .y else .x


# 1. Engine seam: mining commands, the sweep, and the index --------------------------------------

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
#' @param .mines_root Mines directory.
#' @return Tibble, one row per mine, ordered by task then configuration.
kw_mine_index <- function(.mines_root) {
  if (FALSE) .mines_root <- .lP$Output$Mines

  paths_ <- fs::dir_ls(.mines_root, recurse = TRUE, glob = "*mine.json")
  paths_ <- paths_[!grepl("_smoke", paths_, fixed = TRUE)]
  if (length(paths_) == 0L) cli::cli_abort("No mine.json found under {(.mines_root)}")

  purrr::map(paths_, function(.p) {
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
    purrr::list_rbind() |>
    dplyr::arrange(.data$LabelCol, .data$Source, .data$Stopwords, .data$NWords, .data$NgramMax,
                   .data$Fold)
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


# 2. Selection: from candidate statistics to a lexicon -------------------------------------------

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


# 3. Decision: turning a lexicon into labels -----------------------------------------------------

#' Term hits on the held-out fold, independent of the threshold
#'
#' Computed once per lexicon so that an entire threshold curve costs one join. A term can belong to
#' more than one class lexicon, hence the many-to-many relationship.
#'
#' @param .lexicon Output of kw_select.
#' @param .mine Loaded mine carrying a held-out fold.
#' @return Tibble with DocIdx, Class, Term, Power.
kw_hits <- function(.lexicon, .mine) {
  if (FALSE) {
    .mine    <- kw_load_mine(.mine_dir = idx_mine$MineDir[[1]])
    .lexicon <- kw_select(.mine = .mine)
  }
  if (is.null(.mine$IncTest)) cli::cli_abort("This mine holds nothing out; there is no fold to score")

  .mine$IncTest |>
    dplyr::inner_join(
      .lexicon |> dplyr::select(Class, Term, Power),
      by           = dplyr::join_by(Term),
      relationship = "many-to-many"
    )
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
#' @param .hits Output of kw_hits.
#' @param .mine Loaded mine; its held-out documents supply the universe and the truth.
#' @param .tau Power threshold, acting as the evidence gate.
#' @param .mode Decision rule to apply.
#' @param .positive_class Class predicted on any hit, required in binary mode.
#' @param .none Abstention sentinel.
#' @return List with Pred (one row per document) and Prob (one row per document and class).
kw_decide <- function(.hits, .mine,
                      .tau            = 0.70,
                      .mode           = c("multiclass", "binary"),
                      .positive_class = NULL,
                      .none           = KW_NONE) {
  if (FALSE) {
    .mine           <- kw_load_mine(.mine_dir = idx_mine$MineDir[[1]])
    .hits           <- kw_hits(.lexicon = kw_select(.mine = .mine), .mine = .mine)
    .tau            <- 0.70
    .mode           <- "multiclass"
    .positive_class <- NULL
    .none           <- KW_NONE
  }
  .mode    <- match.arg(.mode)
  docs_    <- .mine$DocsTest
  classes_ <- sort(unique(.mine$DocsTrain$Label))

  best_ <- .hits |>
    dplyr::filter(.data$Power >= .tau) |>
    dplyr::arrange(.data$DocIdx, .data$Class, dplyr::desc(.data$Power), .data$Term) |>
    dplyr::distinct(DocIdx, Class, .keep_all = TRUE)

  prob_ <- tidyr::expand_grid(DocIdx = docs_$DocIdx, Class = classes_) |>
    dplyr::left_join(best_ |> dplyr::select(DocIdx, Class, Power), by = dplyr::join_by(DocIdx, Class)) |>
    dplyr::mutate(Prob = dplyr::coalesce(.data$Power, 0)) |>
    dplyr::left_join(docs_ |> dplyr::select(DocIdx, DocID), by = dplyr::join_by(DocIdx)) |>
    dplyr::select(DocID, Class, Prob)

  if (.mode == "binary") {
    if (is.null(.positive_class)) cli::cli_abort("Binary mode requires .positive_class")
    other_ <- setdiff(classes_, .positive_class)
    if (length(other_) != 1L) cli::cli_abort("Binary mode expects exactly two classes")

    pred_ <- docs_ |>
      dplyr::left_join(
        best_ |> dplyr::filter(.data$Class == .positive_class) |>
          dplyr::select(DocIdx, PosPower = Power, PosTerm = Term),
        by = dplyr::join_by(DocIdx)
      ) |>
      dplyr::mutate(
        PredLabel = dplyr::if_else(!is.na(.data$PosPower), .positive_class, other_),
        Score     = dplyr::coalesce(.data$PosPower, 0),
        TopTerm   = .data$PosTerm
      ) |>
      dplyr::select(DocID, TrueLabel = Label, PredLabel, Score, TopTerm)

    return(list(Pred = pred_, Prob = prob_))
  }

  top_ <- best_ |>
    dplyr::mutate(
      MaxPower = max(.data$Power),
      nTied    = sum(.data$Power == max(.data$Power)),
      .by = DocIdx
    ) |>
    dplyr::arrange(.data$DocIdx, dplyr::desc(.data$Power), .data$Class) |>
    dplyr::distinct(DocIdx, .keep_all = TRUE)

  pred_ <- docs_ |>
    dplyr::left_join(
      top_ |> dplyr::select(DocIdx, WinClass = Class, WinTerm = Term, MaxPower, nTied),
      by = dplyr::join_by(DocIdx)
    ) |>
    dplyr::mutate(
      Committed = !is.na(.data$MaxPower) & .data$nTied == 1L,
      PredLabel = dplyr::if_else(.data$Committed, .data$WinClass, .none),
      Score     = dplyr::if_else(.data$Committed, .data$MaxPower, 0),
      TopTerm   = dplyr::if_else(.data$Committed, .data$WinTerm, NA_character_)
    ) |>
    dplyr::select(DocID, TrueLabel = Label, PredLabel, Score, TopTerm)

  list(Pred = pred_, Prob = prob_)
}


# 4. Evaluation: one cell, and the sweeps over cells ---------------------------------------------

#' Canonical configuration name
#'
#' Encodes every axis that varied, so the shared leaderboard -- which keys on this string -- ranks
#' keyword and transformer configurations side by side without knowing their axes differ.
#'
#' @param .label_col,.source,.nwords,.ngram_max,.stopwords Mining axes.
#' @param .min_precision,.min_reach,.max_terms Selection axes.
#' @param .tau Power threshold.
#' @param .seed Stamped for parity.
#' @return Character scalar.
kw_config_name <- function(.label_col, .source, .nwords, .ngram_max, .stopwords,
                           .min_precision, .min_reach, .max_terms, .tau, .seed = 42L) {
  if (FALSE) {
    .label_col     <- "ClassDetailed"
    .source        <- "text"
    .nwords        <- 256L
    .ngram_max     <- 3L
    .stopwords     <- "none"
    .min_precision <- 0.95
    .min_reach     <- 0.01
    .max_terms     <- 25L
    .tau           <- 0.70
    .seed          <- 42L
  }
  window_ <- if (.nwords == 0L) "full" else as.character(.nwords)
  sw_     <- c(none = "none", english = "en", domain = "dom", english_domain = "endom")[[.stopwords]]
  paste0(
    .label_col, "__keyword-", .source, "__",
    "W", window_, "_N", .ngram_max, "_SW", sw_,
    "_P", sprintf("%02d", round(.min_precision * 100)),
    "_R", sprintf("%03d", round(.min_reach * 1000)),
    "_M", .max_terms,
    "_T", sprintf("%02d", round(.tau * 100)),
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

  binary_ <- identical(man_$label_col, "AmendType")
  pos_    <- if (binary_) "Amended" else NULL
  t0_     <- Sys.time()

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
    pred_ <- .mine$DocsTest |>
      dplyr::transmute(
        DocID, TrueLabel = Label,
        PredLabel = if (binary_) "Original" else KW_NONE,
        Score     = 0,
        TopTerm   = NA_character_
      )
    prob_ <- tibble::tibble(DocID = character(), Class = character(), Prob = numeric())
  } else {
    dec_  <- kw_decide(
      .hits           = kw_hits(.lexicon = lex_, .mine = .mine),
      .mine           = .mine,
      .tau            = .tau,
      .mode           = if (binary_) "binary" else "multiclass",
      .positive_class = pos_
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
    .min_reach     = .min_reach,
    .max_terms     = .max_terms,
    .tau           = .tau,
    .seed          = .seed
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


# 5. Operating point -----------------------------------------------------------------------------

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
    list(
      Mine   = mine_,
      Lex    = lex_,
      Hits   = kw_hits(.lexicon = lex_, .mine = mine_),
      Binary = identical(mine_$Manifest$label_col, "AmendType")
    )
  })

  purrr::map(.taus, function(.t) {
    pred_ <- purrr::map(prepped_, function(.p) {
      kw_decide(
        .hits           = .p$Hits,
        .mine           = .p$Mine,
        .tau            = .t,
        .mode           = if (.p$Binary) "binary" else "multiclass",
        .positive_class = if (.p$Binary) "Amended" else NULL
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


# 6. The published table -------------------------------------------------------------------------

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


# 7. The generated arm ---------------------------------------------------------------------------

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
  idx_  <- kw_mine_index(.mines_root = .mines_root) |>
    dplyr::filter(
      .data$Mode      == "manual",
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


# 8. Compute: summaries for reporting ------------------------------------------------------------

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
#' Precision differences inside the tolerance are indistinguishable from fold-to-fold noise, so
#' selecting the maximum among them buys spurious precision at real cost in coverage and compute.
#' Among configurations that are equivalent on the promise, the widest one is preferred.
#'
#' Where an axis is chosen for a reason a metric does not carry, .prefer states it. The stopword
#' regime is the case in point: both regimes are judged on the readability of the terms they produce,
#' which no column reports, so leaving the choice to a coverage tiebreak would silently decide it on
#' a criterion the document explicitly rejects.
#'
#' @param .tab Summary from kw_summarise_mines or kw_summarise_selection.
#' @param .tolerance Precision band treated as a tie.
#' @param .min_coverage Configurations below this coverage are not eligible.
#' @param .prefer Named list of column-value pairs preferred among tied rows, or NULL.
#' @return One-row tibble.
kw_choose_config <- function(.tab, .tolerance = 0.01, .min_coverage = 0.25, .prefer = NULL) {
  if (FALSE) {
    .tab          <- kw_summarise_mines(.tab = perf_mines)
    .tolerance    <- 0.01
    .min_coverage <- 0.25
    .prefer       <- list(Stopwords = "english_domain")
  }
  tied_ <- .tab |>
    dplyr::filter(.data$mCoverage >= .min_coverage) |>
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
    dplyr::slice_head(n = 1L)
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


# 9. Report --------------------------------------------------------------------------------------

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
      Coverage  = clf_pct(.data$mCoverage),
      Precision = clf_pct(.data$mPrecision),
      MacroF1   = sprintf("%.3f +/- %.3f", .data$mMacroF1, .data$sMacroF1)
    ) |>
    dplyr::select(Source, Stopwords, Window, NgramMax, nFolds, Terms = mTerms,
                  Classes = mClasses, Coverage, Precision, MacroF1) |>
    clf_say_table()
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
      Coverage  = clf_pct(.data$mCoverage),
      Precision = clf_pct(.data$mPrecision),
      MacroF1   = sprintf("%.3f", .data$mMacroF1)
    ) |>
    dplyr::select(MinReach, MaxTerms, nFolds, Terms = mTerms, Classes = mClasses, Coverage,
                  Precision, MacroF1) |>
    clf_say_table()
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
      Coverage     = clf_pct(.data$Coverage),
      SelPrecision = clf_pct(.data$SelPrecision),
      Accuracy     = sprintf("%.3f", .data$Accuracy),
      F1_macro     = sprintf("%.3f", .data$F1_macro)
    ) |>
    dplyr::select(Tau, Terms = nTerms, Classes = nClassesHit, Coverage, SelPrecision, Accuracy,
                  F1_macro) |>
    clf_say_table()

  op_ <- kw_operating_point(.curve = .curve, .target_precision = .target_precision,
                            .tolerance = .tolerance, .min_classes = .min_classes)
  cli::cli_alert_success(
    "Operating point tau = {sprintf('%.2f', op_$Tau)}: {op_$nTerms} terms across \\
     {op_$nClassesHit} categories label {clf_pct(op_$Coverage)} of documents at \\
     {clf_pct(op_$SelPrecision)} precision"
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
        MargReach = clf_pct(.data$MarginalReach),
        CumReach  = clf_pct(.data$CumReach),
        Stability = paste0(.data$Folds, "/5")
      ) |>
      dplyr::select(Rank, Term, Power, Precision, HitsPos, Filers, MargReach, CumReach, Stability) |>
      clf_say_table(.title = .c)
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
    clf_say_table()
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
      Coverage     = clf_pct(.data$Coverage),
      SelPrecision = clf_pct(.data$SelPrecision),
      F1_macro     = sprintf("%.3f", .data$F1_macro)
    ) |>
    dplyr::select(List, Terms = nTerms, Classes = nClassesHit, Coverage, SelPrecision, F1_macro) |>
    clf_say_table()
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


# 10. Figures ------------------------------------------------------------------------------------

#' Performance against the truncation window
#'
#' The reference line marks the window the transformer reads. Anything to its right is signal no
#' transformer in this study can see, so a curve still rising there would locate information the
#' whole pipeline currently discards.
#'
#' @param .tab Output of kw_sweep_mines.
#' @param .label_col Task to plot.
#' @param .window_ref Reference window in words.
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
    dplyr::mutate(Window = dplyr::if_else(.data$NWords == 0L, 4096L, .data$NWords)) |>
    tidyr::pivot_longer(cols = c(MacroF1, Precision, Coverage), names_to = "Metric",
                        values_to = "Value")

  p_ <- dat_ |>
    ggplot2::ggplot(ggplot2::aes(x = Window, y = Value, colour = Stopwords,
                                 linetype = factor(NgramMax))) +
    ggplot2::geom_vline(xintercept = .window_ref, linewidth = 0.3, colour = "grey60") +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::facet_wrap(ggplot2::vars(Metric), nrow = 1L) +
    ggplot2::scale_x_continuous(transform = "log2", breaks = c(256, 512, 1024, 2048, 4096),
                                labels = c("256", "512", "1024", "2048", "full")) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = "Word window", y = NULL, colour = "Stopwords", linetype = "n-gram max")
  clf_apply_theme(p_)
}

#' Precision against coverage across the threshold
#'
#' @param .curve Output of kw_tau_curve.
#' @param .target_precision Reference line.
#' @return A ggplot.
kw_plot_tau <- function(.curve, .target_precision = 0.95) {
  if (FALSE) {
    .curve            <- curve_detailed
    .target_precision <- 0.95
  }
  p_ <- .curve |>
    dplyr::filter(!is.na(.data$SelPrecision), .data$Coverage > 0) |>
    ggplot2::ggplot(ggplot2::aes(x = Coverage, y = SelPrecision)) +
    ggplot2::geom_hline(yintercept = .target_precision, linewidth = 0.3, linetype = "dashed",
                        colour = "grey50") +
    ggplot2::geom_line(linewidth = 0.4, colour = "grey30") +
    ggplot2::geom_point(ggplot2::aes(size = nClassesHit), colour = "grey20", alpha = 0.7) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = "Coverage", y = "Precision on the classified subset", size = "Categories")
  clf_apply_theme(p_)
}

#' Reach accumulated by successive terms within each category
#'
#' Shows how quickly a category is covered and where a list stops earning its length.
#'
#' @param .tab Output of kw_lexicon_final.
#' @return A ggplot.
kw_plot_reach <- function(.tab) {
  if (FALSE) {
    .tab <- tab_keywords
  }
  p_ <- .tab |>
    ggplot2::ggplot(ggplot2::aes(x = Rank, y = CumReach, group = Class)) +
    ggplot2::geom_step(linewidth = 0.4, colour = "grey30") +
    ggplot2::geom_point(size = 1.2, colour = "grey20") +
    ggplot2::facet_wrap(ggplot2::vars(Class), ncol = 4L, labeller = ggplot2::label_wrap_gen(24)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = "Term rank within category", y = "Cumulative share of category reached")
  clf_apply_theme(p_)
}
