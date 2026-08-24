# ======================================================================================================================
# 04C-EntityCorpus.R -- library for 04C-EntityCorpus.qmd
# ======================================================================================================================
#
# The corpus pass. Same extractors, same stores, same offset contract as 04A -- over 1.19 million
# attachments instead of 4,398.
#
# THE POINT OF SOURCING 04A'S EXTRACTION PATH RATHER THAN COPYING IT. The corpus is extracted by the
# code the sample was extracted with, which is what makes the two sets of stores comparable rather
# than merely similar. ner_extract() and ner_ingest() are called here unmodified; everything in this
# file is about WHICH documents reach them and how their text gets off disk.
#
# THREE THINGS DIFFER FROM 04A, and each is why this file exists:
#
#   1. The text is not in one parquet. 04A wrote sample_text.parquet and every offset indexes it.
#      Here the text is 1.19 million individual parquets in 01B's mirror, and reading them is the
#      dominant cost of the pass -- not the extractors. So the read is parallel and the engines are
#      not.
#   2. The queue is resumable. A pass over a corpus is interrupted; the ledger makes resumption a
#      property rather than a feature, because a document with a ledger row is never sent again.
#   3. spaCy is absent, on evidence 04A produced. See the configuration.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ in dplyr verbs; if (FALSE) dev blocks; pure ASCII; stringi::stri_sub never substr.


# 1. The store -----------------------------------------------------------------------------------------------------------
#
# ONE DATABASE PER FAMILY, as in 04A, and each carries its own copy of the corpus index. Duplicating
# an index of 1.19 million rows across two files costs a few hundred megabytes and buys the property
# that every family database answers its own questions: what is outstanding is an anti-join inside
# one file rather than a join across two, and a family can be cleared, moved or shipped alone.

#' Add the corpus index and the failure log to a family database
#'
#' ner_db_init() creates the tables every store has -- the ledger, the manifest, the benchmark. These
#' two exist only for a corpus pass, so they are created here rather than there.
#'
#' @param .con Connection from ner_db_connect().
#' @return .con, invisibly.
cor_db_init <- function(.con) {
  if (FALSE) {
    .con <- con_matcon
  }

  # Path is STORED rather than rebuilt per query. utils_doc_path() is cheap for one document and not
  # cheap 1.19 million times inside a loop, and the path is a pure function of three register columns
  # that never change once written.
  #
  # Extract is the deduplication and the population filter resolved once, at load. Every later query
  # is then a filter rather than a re-derivation of the same rule, and a rule applied in one place
  # cannot disagree with itself.
  DBI::dbExecute(.con, "
    CREATE TABLE IF NOT EXISTS corpus (
      DocID            VARCHAR PRIMARY KEY,
      HashDocument     VARCHAR,
      Path             VARCHAR,
      DocType          VARCHAR,
      YQ               VARCHAR,
      PrimaryFiler     BOOLEAN,
      FilerCopiesAgree BOOLEAN,
      DescSample       BOOLEAN,
      EstiSample       BOOLEAN,
      InPopulation     BOOLEAN,
      Extract          BOOLEAN
    )")

  # RETRYABLE BY CONSTRUCTION. A failure is recorded with its reason and excluded from the current
  # pass; the next pass tries again. A file missing because a mount hiccuped must not be blacklisted
  # forever, and a document that is genuinely empty simply reappears in the report, which is honest
  # rather than tidy.
  DBI::dbExecute(.con, "
    CREATE TABLE IF NOT EXISTS failures (
      DocID  VARCHAR,
      Family VARCHAR,
      Reason VARCHAR,
      SeenAt TIMESTAMP
    )")

  invisible(.con)
}

#' Load the corpus index from 02B's register
#'
#' THE REGISTER IS THE GATE, NOT A DIRECTORY WALK. A walk makes a second, independent statement about
#' what is in the corpus, and the two can disagree without either reporting it -- 03A's walk cache was
#' found holding paths into a previous repository and returning them on file existence alone. The
#' register holds every document with its ladder step attached, so membership, population and
#' deduplication all come from one place. It stores no path: DocTypeMod, YQ and DocID determine one
#' and utils_doc_path() rebuilds it, verified across all four document types.
#'
#' ATTACHMENTS, NOT COPIES, and this is the decision that halves nothing and saves seventeen hours.
#' 01C left it open -- "classification wants one, entity extraction may want each, because the
#' registrant-side metadata differs even where the text does not" -- and the resolution is that the
#' metadata differing is an ANCHORING input, applied downstream of extraction. The text of two copies
#' of one attachment is identical, extraction reads text, so extracting both produces identical spans
#' at 23% more cost. One row per attachment here; fan out at release, as 03F does.
#'
#' WHICH ATTACHMENT ANSWERS FOR A POPULATION. PrimaryFiler is chosen globally while EstiSample
#' depends on a per-CIK Compustat match, so an attachment can have its primary outside a population
#' and another copy inside it -- 4,432 of them for the estimation sample, none for the descriptive
#' one. So: extract the primary of every attachment with at least one copy in the population.
#' Restricting to primaries that are themselves in it would leave those unextracted, silently.
#'
#' @param .con Connection to one family database.
#' @param .path_register 02B's Documents.parquet.
#' @param .dir_mirror 01B's parsed mirror root.
#' @param .population One of "all", "descriptive", "estimation".
#' @param .doc_type Register document type.
#' @param .reload Rebuild the index rather than reusing what the store holds.
#' @return Attachments this population extracts, invisibly.
cor_corpus_load <- function(.con, .path_register, .dir_mirror, .population = "all",
                            .doc_type = "Exhibit10", .reload = FALSE) {
  if (FALSE) {
    .con           <- con_matcon
    .path_register <- .lP$Input$Register
    .dir_mirror    <- .lP$Params$DirMirror
    .population    <- "all"
    .doc_type      <- "Exhibit10"
    .reload        <- FALSE
  }
  .population <- match.arg(.population, c("all", "descriptive", "estimation"))

  # THE EARLY RETURN HANDS BACK THE SAME QUANTITY THE FULL PATH DOES. In 03F an earlier version
  # returned COUNT(*) here and the extract count below, so a render reusing an existing index
  # reported the copy count as the work to do and projected the run 23% long. One function, one
  # meaning, on every branch -- and this branch only runs on a re-render, which is exactly why it
  # went unexercised there.
  n_all_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS n FROM corpus")$n[[1L]]
  if (n_all_ > 0L && !.reload) {
    n_ext_ <- DBI::dbGetQuery(.con, "SELECT COUNT(*) AS n FROM corpus WHERE Extract")$n[[1L]]
    cli::cli_alert_info(
      "Corpus index already loaded: {format(n_all_, big.mark = ',')} \\
       {cli::qty(n_all_)}cop{?y/ies}, {format(n_ext_, big.mark = ',')} to extract."
    )
    return(invisible(as.integer(n_ext_)))
  }
  if (!fs::file_exists(.path_register)) cli::cli_abort("No register at {.path {(.path_register)}}.")

  reg_ <- arrow::open_dataset(sources = .path_register) |>
    dplyr::select(
      "DocID", "HashDocument", "DocTypeMod", "YQ", "CIK",
      "Removed", "DescSample", "EstiSample",
      "MultFiler", "nCIK", "PrimaryFiler", "FilerCopiesAgree"
    ) |>
    dplyr::filter(.data$DocTypeMod == .doc_type) |>
    dplyr::collect()

  in_pop_ <- switch(
    .population,
    all         = rep(TRUE, nrow(reg_)),
    descriptive = as.logical(reg_$DescSample),
    estimation  = as.logical(reg_$EstiSample)
  )
  in_pop_    <- !is.na(in_pop_) & in_pop_
  hash_want_ <- unique(reg_$HashDocument[in_pop_])

  reg_ <- reg_ |>
    dplyr::mutate(
      InPopulation = in_pop_,
      Extract      = as.logical(.data$PrimaryFiler) & .data$HashDocument %in% hash_want_
    ) |>
    dplyr::arrange(.data$DocID)

  DBI::dbExecute(.con, "DELETE FROM corpus")
  DBI::dbAppendTable(
    .con, "corpus",
    reg_ |>
      dplyr::transmute(
        .data$DocID, .data$HashDocument,
        DocType = .data$DocTypeMod,
        YQ      = as.character(.data$YQ),
        Path    = as.character(utils_doc_path(
          .dir_mirror = .dir_mirror,
          .doc_type   = .data$DocTypeMod,
          .yq         = .data$YQ,          # utils_doc_path normalises the register's double form
          .doc_id     = .data$DocID
        )),
        PrimaryFiler     = as.logical(.data$PrimaryFiler),
        FilerCopiesAgree = as.logical(.data$FilerCopiesAgree),
        DescSample       = as.logical(.data$DescSample),
        EstiSample       = as.logical(.data$EstiSample),
        InPopulation     = .data$InPopulation,
        Extract          = .data$Extract
      ) |>
      as.data.frame()
  )

  n_copy_    <- nrow(reg_)
  n_attach_  <- dplyr::n_distinct(reg_$HashDocument)
  n_extract_ <- sum(reg_$Extract)

  cli::cli_alert_success(
    "Corpus index loaded from the register: {format(n_copy_, big.mark = ',')} \\
     {cli::qty(n_copy_)}cop{?y/ies} of {format(n_attach_, big.mark = ',')} \\
     {cli::qty(n_attach_)}attachment{?s}."
  )
  # ATTACHMENTS AGAINST ATTACHMENTS. Reporting this as a share of copies reads as a selection when it
  # is not: population "all" wants every attachment, and 81% of copies is the deduplication rate
  # wearing the language of a filter.
  cli::cli_alert_info(
    "Population {.val {(.population)}} wants {format(n_extract_, big.mark = ',')} of them \\
     ({round(100 * n_extract_ / n_attach_)}%). That count -- attachments, not copies -- is the \\
     denominator every progress and cost figure below is read against."
  )
  invisible(as.integer(n_extract_))
}


# 2. The queue -----------------------------------------------------------------------------------------------------------

#' The (model, entity) pairs a family will stamp for the entities requested
#'
#' WHAT A FINISHED DOCUMENT LOOKS LIKE, and it has to come from the description rather than from the
#' ledger. An earlier version of cor_pending() took the target from the ledger it was querying --
#' the largest number of rows any document had -- which is circular twice over: on an empty store
#' there is nothing to take it from, and on a partial one the target is whatever the luckiest
#' document happens to carry. The empty case returned zero outstanding documents and reported a
#' finished corpus over a store holding nothing.
#'
#' It is fewer than models times entities, because the mapping is ragged: dateregex owns DATE and
#' TERM, gazetteer owns GPE alone. Counting the pairs is the only way to get it right without
#' restating the package's internals in R.
#'
#' @param .describe Output of ner_describe().
#' @param .family Family name.
#' @param .entity Entities requested.
#' @return Tibble: Model, Entity.
cor_pairs <- function(.describe, .family, .entity) {
  if (FALSE) {
    .describe <- tab_describe
    .family   <- "matcon"
    .entity   <- c("GPE", "DATE", "TERM", "MONEY", "REDACT")
  }

  out_ <- .describe |>
    dplyr::filter(.data$Family == .family, .data$Entity %in% .entity, .data$Ready) |>
    dplyr::select("Model", "Entity") |>
    dplyr::distinct() |>
    dplyr::arrange(.data$Model, .data$Entity)

  if (nrow(out_) == 0L) {
    cli::cli_abort(c(
      "{(.family)} produces none of the requested entities.",
      "i" = "Requested: {paste(.entity, collapse = ', ')}."
    ))
  }
  out_
}

#' Documents this family has not finished
#'
#' THE ANTI-JOIN THE LEDGER EXISTS FOR, and it runs in SQL rather than in R because 1.19 million
#' identifiers crossed with five entities is six million rows, and pulling them into R to use
#' setdiff() would cost more than the query it replaces.
#'
#' A DOCUMENT IS OUTSTANDING IF ANY REQUESTED PAIR IS MISSING, not all of them. One call to a family
#' produces every entity it was asked for, so a document needing one entity is sent for all of them
#' -- and re-ingesting the entities it already has is a delete-then-insert of identical rows, which
#' costs nothing and keeps the alternative (asking each entity separately, reading each document once
#' per entity) off the table.
#'
#' A ROW IN `failures` DOES NOT EXCLUDE A DOCUMENT. Failures are retried on the next pass by design;
#' the ledger is what excludes.
#'
#' @param .con Connection.
#' @param .describe Output of ner_describe().
#' @param .family Family name.
#' @param .entity Entities requested.
#' @param .limit Cap for a rehearsal; NULL takes everything outstanding.
#' @return Tibble: DocID, Path.
cor_pending <- function(.con, .describe, .family, .entity, .limit = NULL) {
  if (FALSE) {
    .con      <- con_matcon
    .describe <- tab_describe
    .family   <- "matcon"
    .entity   <- c("GPE", "DATE", "TERM", "MONEY", "REDACT")
    .limit    <- NULL
  }

  pairs_ <- cor_pairs(.describe = .describe, .family = .family, .entity = .entity)
  want_  <- nrow(pairs_)

  mods_ <- paste0("'", unique(pairs_$Model), "'", collapse = ", ")
  ents_ <- paste0("'", unique(pairs_$Entity), "'", collapse = ", ")
  lim_  <- if (is.null(.limit)) "" else glue::glue(" LIMIT {as.integer(.limit)}")

  DBI::dbGetQuery(.con, glue::glue(
    "SELECT c.DocID, c.Path
       FROM corpus c
       LEFT JOIN (
         SELECT DocID, COUNT(*) AS n FROM runs
          WHERE Model IN ({mods_}) AND Entity IN ({ents_}) GROUP BY DocID
       ) r ON r.DocID = c.DocID
      WHERE c.Extract AND COALESCE(r.n, 0) < {want_}
      ORDER BY c.DocID{lim_}"
  )) |>
    tibble::as_tibble()
}

#' Record an outcome for a document that never reached an extractor
#'
#' A DOCUMENT WITH NO TEXT CANNOT SUCCEED, and the first version retried it on every render forever.
#' The design said failures are retried rather than blacklisted, which is right for a mount that
#' hiccuped and wrong for a file that is empty: the queue never emptied, the failure log grew by 736
#' rows a render, and the status report advised re-rendering to continue -- advice that could not
#' work.
#'
#' The evidence that these are permanent rather than transient is independent: 03F's classification
#' pass over the same population failed on 736 documents for the same reason. Two passes, different
#' engines, the same count.
#'
#' So the ledger records `error` for every pair the family would have produced. The document is then
#' excluded, counted honestly in nError, and recoverable by hand -- DELETE FROM runs WHERE Status =
#' 'error' puts them all back in the queue, which is the same escape hatch a timeout has.
#'
#' @param .con Connection.
#' @param .doc_ids Documents that produced no text.
#' @param .pairs Output of cor_pairs().
#' @param .status Ledger status to record.
#' @return Rows written, invisibly.
cor_ledger_mark <- function(.con, .doc_ids, .pairs, .status = "error") {
  if (FALSE) {
    .con     <- con_matcon
    .doc_ids <- c("0000001-abc", "0000002-def")
    .pairs   <- cor_pairs(tab_describe, "matcon", c("GPE", "DATE"))
    .status  <- "error"
  }
  if (length(.doc_ids) == 0L) return(invisible(0L))

  rows_ <- tidyr::expand_grid(DocID = .doc_ids, .pairs) |>
    dplyr::transmute(.data$DocID, .data$Model, .data$Entity, Status = .status, RunAt = Sys.time())

  mods_ <- paste0("'", unique(.pairs$Model), "'", collapse = ", ")
  ents_ <- paste0("'", unique(.pairs$Entity), "'", collapse = ", ")

  DBI::dbWriteTable(.con, "tmp_mark", data.frame(DocID = .doc_ids),
                    temporary = TRUE, overwrite = TRUE)
  DBI::dbExecute(.con, glue::glue(
    "DELETE FROM runs
      WHERE Model IN ({mods_}) AND Entity IN ({ents_})
        AND DocID IN (SELECT DocID FROM tmp_mark)"
  ))
  DBI::dbAppendTable(.con, "runs", as.data.frame(rows_))
  DBI::dbRemoveTable(.con, "tmp_mark")

  invisible(nrow(rows_))
}

#' Split a queue into chunks
#'
#' @param .docs Tibble.
#' @param .size Documents per chunk.
#' @return List of tibbles.
cor_chunks <- function(.docs, .size) {
  if (FALSE) {
    .docs <- queue
    .size <- 5000L
  }
  if (nrow(.docs) == 0L) return(list())
  split(.docs, ceiling(seq_len(nrow(.docs)) / .size)) |> unname()
}

#' Read a chunk's text off disk, in parallel
#'
#' THE DOMINANT COST OF THE PASS, and not the extractors. One arrow::read_parquet() per document,
#' 1.19 million times, is most of the wall clock at matcon's throughput and a large part of it at
#' LexNLP's. So the parallelism goes into the read, where it also cannot change a result -- a read is
#' pure -- rather than into the engines, which are subprocesses already running their own workers.
#'
#' Lifted from 03F with the same three safeguards, each of which cost a diagnosis there:
#'
#'   - mirai RETURNS errors as values rather than throwing, so a failed batch arrives looking like one
#'     text and N failed batches unlist to a vector of N. Detected and the first message printed.
#'   - A SHORT RESULT IS A FAILURE, not a partial success. Pairing a short vector onto .docs would
#'     recycle it and give documents other documents' text, which is worse than being slow and would
#'     not announce itself.
#'   - Order must survive the round trip, because text is paired on positionally. Batches are
#'     contiguous, split() orders numeric groups numerically, and mirai_map() returns in input order.
#'
#' @param .docs Tibble carrying DocID and Path.
#' @param .workers Daemons, assumed already started by the caller.
#' @return .docs with a Text column.
cor_read_text <- function(.docs, .workers = 1L) {
  if (FALSE) {
    .docs    <- queue[1:10, ]
    .workers <- 24L
  }
  if (nrow(.docs) == 0L) return(dplyr::mutate(.docs, Text = character(0)))

  seq_read_ <- function(.d) dplyr::mutate(.d, Text = purrr::map_chr(.data$Path, clf_read_text))

  # Below the threshold the dispatch costs more than the read saves, and the last chunk of a pass is
  # routinely short.
  if (.workers <= 1L || nrow(.docs) < 500L) return(seq_read_(.docs))

  n_    <- nrow(.docs)
  grp_  <- ceiling(seq_len(n_) / ceiling(n_ / .workers))
  bats_ <- split(.docs$Path, grp_)

  txt_ <- tryCatch(
    {
      out_ <- mirai::mirai_map(
        .x = bats_,
        .f = function(paths) vapply(paths, clf_read_text, character(1), USE.NAMES = FALSE)
      )[]

      err_ <- which(vapply(out_, \(.r) inherits(.r, "miraiError"), logical(1)))
      if (length(err_) > 0L) {
        cli::cli_alert_danger(
          "{length(err_)} of {length(out_)} batch{?es} failed in the daemons. First message:"
        )
        cli::cli_text("  {as.character(out_[[err_[[1L]]]])}")
        NULL
      } else {
        unlist(out_, use.names = FALSE)
      }
    },
    error = function(e) {
      cli::cli_alert_warning(
        "Parallel read failed ({conditionMessage(e)}); falling back to a sequential read for this \\
         chunk. The result is identical, only slower."
      )
      NULL
    }
  )

  if (is.null(txt_) || length(txt_) != n_) {
    if (!is.null(txt_)) {
      cli::cli_alert_warning(
        "Parallel read returned {length(txt_)} text{?s} for {n_} document{?s}; reading \\
         sequentially. A count matching the BATCH count rather than the document count means the \\
         tasks failed and returned their error messages."
      )
    }
    return(seq_read_(.docs))
  }
  dplyr::mutate(.docs, Text = txt_)
}

#' Start daemons and source the reader into them
#'
#' A DAEMON IS A FRESH R SESSION AND HOLDS NONE OF THIS ONE'S FUNCTIONS. Sending clf_read_text() as a
#' serialized closure and assuming it carries does not work, and the failure is silent in the sense
#' that matters: every task returns a miraiError, which mirai hands back as a value. So the chain is
#' sourced into each daemon instead -- which also keeps ONE implementation of the reader, so the
#' corpus is read by the same function that read the labelled sample rather than by a copy that
#' happens to agree today.
#'
#' The whole chain travels because 03A's library is not inert: it registers a vocabulary against
#' _Plots.R at load time, so _Plots.R must be in scope before it loads.
#'
#' @param .workers Daemons to start. 1 or fewer starts none.
#' @return TRUE where daemons are up and carrying the reader.
cor_daemons_start <- function(.workers) {
  if (FALSE) {
    .workers <- 24L
  }
  if (.workers <= 1L) return(FALSE)

  mirai::daemons(.workers)

  ok_ <- tryCatch({
    mirai::everywhere(
      {
        for (.f in c(file.path("_Commons", "_Initialize.R"), file.path("_Commons", "_Utils.R"),
                     file.path("_Commons", "_Plots.R"),      file.path("_Commons", "_Tables.R"),
                     "03A-ClassifyPrepare.R")) {
          source(file.path(.code, .f), encoding = "UTF-8")
        }
      },
      .code = here::here("1_code")
    )
    TRUE
  }, error = function(e) {
    cli::cli_alert_warning(
      "Could not prepare the daemons ({conditionMessage(e)}); this pass reads sequentially."
    )
    FALSE
  })

  if (ok_) cli::cli_alert_success("{(.workers)} daemon{?s} up and carrying the reader.")
  ok_
}

#' Seconds as something a person reads at three in the morning
#'
#' @param .secs Numeric.
#' @return Character.
cor_duration <- function(.secs) {
  if (FALSE) {
    .secs <- 4210
  }
  if (is.na(.secs) || !is.finite(.secs)) return("--")
  if (.secs < 90) return(paste0(round(.secs), "s"))
  if (.secs < 5400) return(paste0(round(.secs / 60), "m"))
  paste0(round(.secs / 3600, 1), "h")
}


# 3. The pass ------------------------------------------------------------------------------------------------------------

#' Run one family over everything it has not finished
#'
#' The loop is: take a chunk of the queue, read its text in parallel, stage it as a parquet, hand
#' that to ner_extract(), ingest what comes back. The staging file is what the engines read -- a
#' container cannot be handed an R object -- and it is overwritten per chunk rather than accumulated.
#'
#' RESUMPTION IS A PROPERTY, NOT A FEATURE. ner_ingest() writes the ledger per chunk, so an
#' interruption costs at most the chunk in flight. Nothing has to be remembered between renders and
#' no state exists that could be wrong about itself.
#'
#' @param .con Connection to this family's database.
#' @param .family Family name.
#' @param .entity Entities to request.
#' @param .describe Output of ner_describe(), which supplies both the model tags and the count of
#'   (model, entity) pairs a finished document carries.
#' @param .stage_path Where each chunk's text is staged. Overwritten per chunk.
#' @param .stage_dir Where the family writes its output parquets. Cleared per chunk.
#' @param .chunk_size Documents per engine invocation.
#' @param .read_workers Daemons for the R-side read.
#' @param .engine_workers Worker processes INSIDE the extractor. A separate argument from the read
#'   workers because they are separate resources doing separate work: the daemons read parquet files
#'   and finish before the container starts, so the two never contend, but they need not be equal and
#'   one number standing for both would hide that.
#' @param .batch_size Documents per unit of work inside the engine.
#' @param .timeout Per-document cap in seconds.
#' @param .limit Cap the queue for a rehearsal; NULL takes everything.
#' @param .report_every Chunks between progress lines.
#' @return One-row tibble: Family, nDocs, nRan, nChunks, nFailed, Seconds, DocsPerSecond. The rate
#'   is over nRan, so a resumption that finds only unreadable documents reports no rate at all.
cor_pass <- function(.con, .family, .entity, .describe, .stage_path, .stage_dir,
                     .chunk_size = 5000L, .read_workers = 24L, .engine_workers = 24L,
                     .batch_size = 8L, .timeout = 240L, .limit = NULL, .report_every = 5L) {
  if (FALSE) {
    .con            <- con_matcon
    .family         <- "matcon"
    .entity         <- c("GPE", "DATE", "TERM", "MONEY", "REDACT")
    .describe       <- tab_describe
    .stage_path     <- fs::path(.lP$Output$Stage, "chunk.parquet")
    .stage_dir      <- fs::path(.lP$Output$Stage, "matcon")
    .chunk_size     <- 5000L
    .read_workers   <- 24L
    .engine_workers <- 24L
    .batch_size     <- 16L
    .timeout        <- 120L
    .limit          <- NULL
    .report_every   <- 5L
  }

  fs::dir_create(c(fs::path_dir(.stage_path), .stage_dir))

  # ONE CONSTRUCTOR FOR BOTH EXITS. A function with two returns owes them the same shape, and the
  # first version of this owed and did not pay: nRan was added to the working return and not to the
  # early one, so a render where nothing was outstanding produced a tibble missing a column the
  # report then asked for. The early branch only runs on a completed corpus, which is exactly why it
  # went unexercised until the pass finished -- the same reason 03F's app_corpus_load() carried a
  # wrong early return through four renders.
  #
  # Building the row in one place makes the divergence impossible rather than catchable.
  row_ <- function(.n_docs, .n_ran, .n_chunks, .n_failed, .secs) {
    tibble::tibble(
      Family        = .family,
      nDocs         = as.integer(.n_docs),
      nRan          = as.integer(.n_ran),
      nChunks       = as.integer(.n_chunks),
      nFailed       = as.integer(.n_failed),
      Seconds       = as.numeric(.secs),
      DocsPerSecond = if (.n_ran > 0L) .n_ran / max(as.numeric(.secs), 1e-9) else NA_real_
    )
  }

  pairs_ <- cor_pairs(.describe = .describe, .family = .family, .entity = .entity)
  queue_ <- cor_pending(
    .con = .con, .describe = .describe, .family = .family, .entity = .entity, .limit = .limit
  )
  if (nrow(queue_) == 0L) {
    cli::cli_alert_success("{(.family)}: nothing outstanding.")
    return(row_(.n_docs = 0L, .n_ran = 0L, .n_chunks = 0L, .n_failed = 0L, .secs = 0))
  }

  # DAEMONS ARE STARTED ONCE PER PASS, not once per chunk. Startup is about a second each and a full
  # pass is a couple of hundred chunks, so starting them inside the loop would spend more on daemons
  # than the parallel read saves.
  par_ok_ <- cor_daemons_start(.workers = .read_workers)
  on.exit(mirai::daemons(0L), add = TRUE)

  chunks_ <- cor_chunks(.docs = queue_, .size = .chunk_size)
  # THE TARGET IS STATED, so a queue that comes back surprising can be read against the number that
  # produced it rather than guessed at.
  cli::cli_alert_info(
    "{(.family)}: {format(nrow(queue_), big.mark = ',')} {cli::qty(nrow(queue_))}document{?s} \\
     in {length(chunks_)} chunk{?s} of {format(.chunk_size, big.mark = ',')}, \\
     {nrow(pairs_)} model-entity pair{?s} each."
  )

  t0_     <- Sys.time()
  done_   <- 0L
  failed_ <- 0L

  for (.i in seq_along(chunks_)) {
    ch_   <- cor_read_text(.docs = chunks_[[.i]], .workers = if (par_ok_) .read_workers else 1L)
    done_ <- done_ + nrow(chunks_[[.i]])

    bad_ <- dplyr::filter(ch_, is.na(.data$Text) | !nzchar(trimws(.data$Text)))
    ok_  <- dplyr::filter(ch_, !is.na(.data$Text), nzchar(trimws(.data$Text)))

    if (nrow(bad_) > 0L) {
      failed_ <- failed_ + nrow(bad_)
      # PERMANENT, SO IT IS RECORDED IN THE LEDGER, not only in the failure log. Otherwise the queue
      # never empties and every render repeats the same futile read.
      cor_ledger_mark(
        .con = .con, .doc_ids = bad_$DocID, .pairs = pairs_, .status = "error"
      )
      DBI::dbExecute(.con, glue::glue(
        "DELETE FROM failures WHERE Family = '{.family}' AND Reason = 'no text'
           AND DocID IN ({paste0(\"'\", bad_$DocID, \"'\", collapse = ', ')})"
      ))
      DBI::dbAppendTable(.con, "failures", as.data.frame(tibble::tibble(
        DocID = bad_$DocID, Family = .family, Reason = "no text", SeenAt = Sys.time()
      )))
    }

    if (nrow(ok_) > 0L) {
      # THE ENGINES READ A FILE, so the chunk is staged. TextRaw rather than Text because that is
      # what every extractor defaults to and what 04A's canonical text is called -- one name for one
      # thing, across the R side, the Python side and both stores.
      arrow::write_parquet(
        dplyr::transmute(ok_, .data$DocID, TextRaw = .data$Text), .stage_path
      )

      res_ <- tryCatch({
        staged_ <- ner_extract(
          .family     = .family,
          .model      = NULL,
          .entity     = .entity,
          .path_in    = .stage_path,
          .out_dir    = .stage_dir,
          .describe   = .describe,
          .workers    = .engine_workers,   # inside the extractor, not the daemons above
          .batch_size = .batch_size,
          .timeout    = .timeout,
          .quiet      = FALSE
        )
        ner_ingest(.con = .con, .staged = staged_, .entity = .entity)
      }, error = function(e) {
        cli::cli_alert_danger("Chunk {(.i)} failed: {conditionMessage(e)}")
        NULL
      })

      if (is.null(res_)) {
        failed_ <- failed_ + nrow(ok_)
        DBI::dbAppendTable(.con, "failures", as.data.frame(tibble::tibble(
          DocID = ok_$DocID, Family = .family, Reason = "engine error", SeenAt = Sys.time()
        )))
      }
    }

    # PROGRESS IS REPORTED WHATEVER HAPPENED TO THE CHUNK, including one that failed entirely: a run
    # that goes quiet is indistinguishable from a run that has hung, and the difference matters at
    # three in the morning. The rate is cumulative rather than per-chunk, so it self-corrects -- the
    # first chunk pays for container startup and never pays again.
    if (.i %% .report_every == 0L || .i == 1L || .i == length(chunks_)) {
      now_     <- Sys.time()
      elapsed_ <- as.numeric(difftime(now_, t0_, units = "secs"))
      rate_    <- done_ / max(elapsed_, 1e-9)
      left_    <- nrow(queue_) - done_
      eta_     <- left_ / max(rate_, 1e-9)
      cli::cli_alert_info(
        "  chunk {(.i)}/{length(chunks_)} | {format(done_, big.mark = ',')}/\\
         {format(nrow(queue_), big.mark = ',')} | {cor_duration(elapsed_)} elapsed | \\
         {round(rate_, 1)}/s | \\
         {if (left_ == 0L) 'done' else paste0('ETA ', cor_duration(eta_), ' (~',
          format(now_ + eta_, '%a %H:%M'), ')')}"
      )
    }
  }

  # THE RATE IS OVER DOCUMENTS THAT REACHED AN EXTRACTOR, which row_() enforces. A resumption that
  # finds only unreadable documents left processed 736 of them in a second and reported 1,180 a
  # second, projecting the corpus at seventeen minutes -- a number measuring how fast this pass can
  # fail to read an empty file. Where nothing ran, the rate is missing rather than impressive.
  secs_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))
  row_(
    .n_docs   = nrow(queue_),
    .n_ran    = nrow(queue_) - failed_,
    .n_chunks = length(chunks_),
    .n_failed = failed_,
    .secs     = secs_
  )
}

#' Report what a pass did, and what it implies
#'
#' @param .tab Bound output of cor_pass().
#' @param .n_corpus Attachments the population extracts, from cor_corpus_load().
#' @return .tab, invisibly.
cor_report_pass <- function(.tab, .n_corpus) {
  if (FALSE) {
    .tab      <- tab_pass
    .n_corpus <- n_corpus
  }

  if (nrow(.tab) == 0L) {
    cli::cli_alert_info("No pass ran.")
    return(invisible(.tab))
  }

  show_ <- .tab |>
    dplyr::transmute(
      .data$Family, .data$nDocs, .data$nRan, .data$nChunks, .data$nFailed,
      Elapsed = purrr::map_chr(.data$Seconds, cor_duration),
      DocPerS = round(.data$DocsPerSecond, 2),
      FullRun = purrr::map_chr(
        .data$DocsPerSecond, \(.r) if (is.na(.r) || .r <= 0) "--" else cor_duration(.n_corpus / .r)
      )
    )
  tbl_say(.tab = show_, .title = "What this render's pass did")

  # THIS RENDER, NOT THE WHOLE PASS. A resumption reports only what it did, so a corpus extracted
  # over three renders has no single elapsed time here -- the cumulative picture is the coverage
  # table in the Overview, and saying so beats letting a reader read one for the other.
  cli::cli_alert_info(
    "These are this render's numbers. Where a pass resumed, earlier renders did the rest and the \\
     cumulative position is the coverage table below."
  )

  # THE PROJECTION IS AGAINST ATTACHMENTS, never against the index. The index holds one row per
  # registrant copy and is 23% larger; using it here is the mistake that made 03F project every run
  # long, in four separate places, each surviving until a branch reached it.
  cli::cli_alert_info(
    "FullRun projects the measured rate over all {format(.n_corpus, big.mark = ',')} \\
     {cli::qty(.n_corpus)}attachment{?s} the population wants -- not over the index, which is \\
     larger because it holds one row per registrant copy."
  )

  # THE DESCRIPTION FOLLOWED THE BEHAVIOUR HERE ON THE SECOND ATTEMPT. This said failures were
  # "logged for retry" and "not blacklisted", which was true when a failure produced only a log row
  # and false the moment it produced a ledger row -- and the code changed while the sentence did not.
  # Nothing mechanical catches that; only reading the rendered claim against what the code does.
  #
  # Also: the count is per family, not per document. Summing 736 across two families and calling the
  # result 1,472 documents is the attachments-versus-copies mistake in another costume.
  bad_ <- dplyr::filter(.tab, .data$nFailed > 0L)
  if (nrow(bad_) > 0L) {
    cli::cli_alert_warning(
      "Failed in this render, per family: \\
       {paste(paste0(bad_$Family, ' ', format(bad_$nFailed, big.mark = ',')), collapse = ', ')}. \\
       Recorded in the ledger as errors and EXCLUDED from later passes, so re-rendering will not \\
       retry them."
    )
    cli::cli_alert_info(
      "To send them again: {.code DELETE FROM runs WHERE Status = 'error'}, then re-render."
    )
  }

  invisible(.tab)
}


# 4. Status and validation -------------------------------------------------------------------------------------------

#' How far each family has got
#'
#' @param .con Connection.
#' @param .describe Output of ner_describe().
#' @param .entity Entities.
#' @param .family Family name, added as a column.
#' @return One-row tibble: Family, nExtract, nDone, nUnreadable, nPending, Share. Share counts only
#'   documents actually extracted, so an unreadable one is neither pending nor complete.
cor_status <- function(.con, .describe, .entity, .family) {
  if (FALSE) {
    .con      <- con_matcon
    .describe <- tab_describe
    .entity   <- c("GPE", "DATE")
    .family   <- "matcon"
  }

  n_ext_ <- as.integer(
    DBI::dbGetQuery(.con, "SELECT COUNT(*) AS n FROM corpus WHERE Extract")$n[[1L]]
  )
  n_pend_ <- nrow(cor_pending(
    .con = .con, .describe = .describe, .family = .family, .entity = .entity
  ))

  # RECORDED AS AN ERROR IS NOT THE SAME AS EXTRACTED, and the share above counts both as done
  # because both have an outcome. Reporting the unreadable count beside it is what stops "0.999
  # complete" being read as "0.999 extracted".
  n_err_ <- as.integer(DBI::dbGetQuery(.con, "
    SELECT COUNT(*) AS n FROM (SELECT DISTINCT DocID FROM runs WHERE Status = 'error')")$n[[1L]])

  # nDone SUBTRACTS THE ERRORS, and the first version did not -- so it read 1,189,805 beside a Share
  # of 0.999 in the same row, two columns disagreeing about the same quantity. A document recorded as
  # unreadable has an outcome, which is why nPending is zero, and it is not extracted, which is why
  # it belongs in neither nDone nor nPending.
  tibble::tibble(
    Family      = .family,
    nExtract    = n_ext_,
    nDone       = n_ext_ - n_pend_ - n_err_,
    nUnreadable = n_err_,
    nPending    = as.integer(n_pend_),
    Share       = round((n_ext_ - n_pend_ - n_err_) / max(n_ext_, 1L), 4)
  )
}

#' Report status across families
#'
#' @param .tab Bound output of cor_status().
#' @return .tab, invisibly.
cor_report_status <- function(.tab) {
  if (FALSE) {
    .tab <- tab_status
  }
  tbl_say(.tab = .tab, .title = "Corpus coverage by family")

  # OUTSTANDING AND UNREADABLE ARE DIFFERENT FINDINGS, and collapsing them produced advice that
  # could not work: the first version said "re-render to continue" over 736 documents that hold no
  # text, which no number of renders will change. Now the two are counted apart and only the first
  # is something a reader can act on.
  if (all(.tab$nPending == 0L)) {
    cli::cli_alert_success(
      "Every family has an outcome recorded for every attachment the population wants."
    )
    if (any(.tab$nUnreadable > 0L)) {
      cli::cli_alert_info(
        "{max(.tab$nUnreadable)} of them hold no text and are recorded as errors rather than \\
         extracted. 03F's classification pass failed on the same count, so they are a property of \\
         the corpus rather than of this run. Clear them with \\
         {.code DELETE FROM runs WHERE Status = 'error'} to try again."
      )
    }
  } else {
    cli::cli_alert_info(
      "{sum(.tab$nPending)} document-family combination{?s} still to do. Re-render to continue; \\
       the ledger resumes where this stopped."
    )
  }
  invisible(.tab)
}

#' The offset contract, on a sample of corpus documents
#'
#' THE CHECK 04A RUNS OVER EVERY SPAN CANNOT RUN HERE, and the reason is structural rather than a
#' matter of cost: 04A's spans all index one file, so the text is one read. The corpus text is 1.19
#' million separate parquets, so verifying every span means reading the corpus again.
#'
#' So it is a sample, and the sample is honest about being one. A systematic offset failure -- a byte
#' rather than code-point slice, a truncation the extractor did not report -- shows in any sample at
#' all; a failure confined to a handful of documents would not, and nothing here claims otherwise.
#'
#' @param .con Connection.
#' @param .n Documents to draw.
#' @param .seed Fixed, so a re-render checks the same documents and a new failure is a new finding.
#' @return Tibble of failures; empty is a pass.
cor_check_offsets <- function(.con, .n = 500L, .seed = 42L) {
  if (FALSE) {
    .con  <- con_matcon
    .n    <- 500L
    .seed <- 42L
  }

  tabs_ <- ner_db_tables(.con = .con)
  if (length(tabs_) == 0L) return(tibble::tibble())

  pool_ <- DBI::dbGetQuery(.con, glue::glue(
    "SELECT DocID, Path FROM corpus WHERE Extract ORDER BY DocID"
  )) |>
    tibble::as_tibble()
  if (nrow(pool_) == 0L) return(tibble::tibble())

  # nrow() rather than dplyr::n(): slice_sample() evaluates `n` outside the data mask, so dplyr::n()
  # raises there. The cap matters because a rehearsal with .limit set has fewer extracted documents
  # than the draw asks for.
  set.seed(.seed)
  docs_ <- dplyr::slice_sample(pool_, n = min(as.integer(.n), nrow(pool_)))

  txt_ <- dplyr::mutate(docs_, TextRaw = purrr::map_chr(.data$Path, clf_read_text)) |>
    dplyr::filter(!is.na(.data$TextRaw)) |>
    dplyr::select("DocID", "TextRaw")

  ids_ <- paste0("'", txt_$DocID, "'", collapse = ", ")

  purrr::map(tabs_, \(.t) {
    DBI::dbGetQuery(.con, glue::glue(
      "SELECT DocID, Start, Stop, Span FROM {.t}
        WHERE Start IS NOT NULL AND DocID IN ({ids_})"
    )) |>
      tibble::as_tibble() |>
      dplyr::mutate(Entity = toupper(.t))
  }) |>
    purrr::list_rbind() |>
    dplyr::inner_join(txt_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(Cut = stringi::stri_sub(.data$TextRaw, .data$Start + 1L, .data$Stop)) |>
    dplyr::filter(.data$Cut != .data$Span) |>
    dplyr::select("DocID", "Entity", "Start", "Stop", "Span", "Cut")
}

#' Run the corpus validations for one family and report them as one block
#'
#' @param .con Connection.
#' @param .family Family name.
#' @param .describe Output of ner_describe().
#' @param .entity Entities.
#' @param .n_offsets Documents to draw for the offset check.
#' @return List of results, invisibly.
cor_report_validation <- function(.con, .family, .describe, .entity, .n_offsets = 500L) {
  if (FALSE) {
    .con       <- con_matcon
    .family    <- "matcon"
    .describe  <- tab_describe
    .entity    <- c("GPE", "DATE")
    .n_offsets <- 500L
  }

  cli::cli_h3(paste0("Validation -- ", .family))

  tabs_  <- ner_db_tables(.con = .con)
  nspan_ <- if (length(tabs_) == 0L) {
    0L
  } else {
    sum(purrr::map_dbl(tabs_, \(.t) as.numeric(
      DBI::dbGetQuery(.con, glue::glue("SELECT COUNT(*) AS n FROM {.t}"))$n[[1L]]
    )))
  }

  # AN EMPTY STORE IS NOT A PASS. Both checks below are satisfied by having nothing to check, which
  # is the shape 04A's first version reported as two green ticks on an empty LexNLP store.
  if (nspan_ == 0L) {
    cli::cli_alert_warning(
      "The {(.family)} store holds no spans, so NOTHING WAS CHECKED. This is not a pass."
    )
    return(invisible(list(Offsets = tibble::tibble(), Pending = NA_integer_)))
  }

  n_pool_ <- as.integer(
    DBI::dbGetQuery(.con, "SELECT COUNT(*) AS n FROM corpus WHERE Extract")$n[[1L]]
  )
  n_draw_ <- min(as.integer(.n_offsets), n_pool_)

  off_ <- cor_check_offsets(.con = .con, .n = .n_offsets)
  if (nrow(off_) == 0L) {
    cli::cli_alert_success(
      "Offset contract holds on every span in {(n_draw_)} sampled document{?s} \\
       ({format(nspan_, big.mark = ',')} {cli::qty(nspan_)}span{?s} in the store)."
    )
  } else {
    tbl_say(.tab = utils::head(off_, 20L), .title = "OFFSET FAILURES")
    cli::cli_abort(
      "{nrow(off_)} span{?s} in {(.family)} do not round-trip. Nothing downstream can be trusted."
    )
  }

  pend_ <- nrow(cor_pending(
    .con = .con, .describe = .describe, .family = .family, .entity = .entity
  ))
  if (pend_ == 0L) {
    cli::cli_alert_success("Ledger complete: every attachment the population wants has an outcome.")
  } else {
    cli::cli_alert_warning(
      "{format(pend_, big.mark = ',')} {cli::qty(pend_)}attachment{?s} still outstanding, so the \\
       counts below describe a partial pass."
    )
  }

  invisible(list(Offsets = off_, Pending = as.integer(pend_)))
}


# 5. Report --------------------------------------------------------------------------------------------------------------

#' What one family's corpus store holds
#'
#' @param .con Connection.
#' @param .family Family name.
#' @param .n_corpus Attachments the population wants, as the coverage denominator.
#' @return Tibble: Family, Model, Entity, NSpan, NDoc, Coverage, nTimeout, nError, SpecHash.
cor_summary <- function(.con, .family, .n_corpus) {
  if (FALSE) {
    .con      <- con_matcon
    .family   <- "matcon"
    .n_corpus <- n_corpus
  }

  sum_ <- ner_db_summary(.con = .con)
  if (nrow(sum_) == 0L) return(tibble::tibble())

  sum_ |>
    dplyr::transmute(
      Family   = .family,
      .data$Model, .data$Entity, .data$NSpan,
      NDoc     = .data$nHit,
      Coverage = round(.data$nHit / .n_corpus, 3),
      SpansPerDoc = round(.data$NSpan / pmax(.data$nHit, 1L), 1),
      .data$nTimeout, .data$nError, .data$SpecHash
    ) |>
    dplyr::arrange(.data$Entity, .data$Model)
}

#' Report the corpus stores, and compare them with the labelled sample
#'
#' THE COMPARISON IS THE POINT. 04A measured every one of these engines on 4,398 labelled documents;
#' if the corpus coverage differs sharply from the sample's, either the sample is not representative
#' of the corpus or something in the pass is wrong -- and both are findings worth having before any
#' rule is written against these spans.
#'
#' @param .tab Bound output of cor_summary().
#' @param .sample Coverage from 04A, carrying Family, Entity and Coverage. NULL skips the comparison.
#' @return .tab, invisibly.
cor_report_summary <- function(.tab, .sample = NULL) {
  if (FALSE) {
    .tab    <- tab_summary
    .sample <- tab_sample_cov
  }

  tbl_say(.tab = dplyr::select(.tab, -"SpecHash"), .title = "The corpus stores")

  bad_ <- dplyr::filter(.tab, .data$nTimeout > 0L | .data$nError > 0L)
  if (nrow(bad_) == 0L) {
    cli::cli_alert_success("No timeouts and no extractor errors anywhere in the corpus pass.")
  } else {
    tbl_say(.tab = dplyr::select(bad_, "Family", "Model", "Entity", "nTimeout", "nError"),
            .title = "TIMEOUTS AND ERRORS")
    cli::cli_alert_warning(
      "A crash and a clean miss produce the same empty result, which is why they are counted \\
       separately from coverage. BOTH ARE EXCLUDED from later passes: a ledger row is a ledger row \\
       whatever its status, so neither is retried by re-rendering."
    )
    # THE TWO HAVE DIFFERENT REMEDIES AND DIFFERENT PROSPECTS, so they are named apart. A timeout
    # might complete under a longer cap; a document with no text will not, whatever the cap.
    cli::cli_alert_info(
      "A timeout may complete under a longer cap -- clear it with \\
       {.code DELETE FROM runs WHERE Status = 'timeout'} and raise the cap. An error is a document \\
       that held no text, so raising anything will not help; the same {.code DELETE} with \\
       {.code Status = 'error'} only repeats the read."
    )
  }

  if (!is.null(.sample) && nrow(.sample) > 0L) {
    cmp_ <- .tab |>
      dplyr::select("Family", "Entity", Corpus = "Coverage") |>
      dplyr::inner_join(
        dplyr::select(.sample, "Family", "Entity", Sample = "Coverage"),
        by = dplyr::join_by(Family, Entity)
      ) |>
      dplyr::mutate(Diff = round(.data$Corpus - .data$Sample, 3)) |>
      dplyr::arrange(dplyr::desc(abs(.data$Diff)))

    tbl_say(.tab = cmp_, .title = "Corpus coverage against the labelled sample")

    far_ <- dplyr::filter(cmp_, abs(.data$Diff) > 0.1)
    if (nrow(far_) > 0L) {
      cli::cli_alert_warning(
        "{nrow(far_)} combination{?s} differ from the sample by more than ten points. Either the \\
         sample is not representative of the corpus for {?it/them}, or something in the pass is \\
         wrong -- and the two are told apart by reading the spans, not by reading this table."
      )
    } else {
      cli::cli_alert_success(
        "Every combination is within ten points of its rate on the labelled sample."
      )
    }
  }

  # THE MANIFEST IS THE PROVENANCE STATEMENT, and printing it here is what lets a reader of the
  # rendered page say which version of which extractor produced the corpus without opening a store.
  tbl_say(
    .tab   = dplyr::distinct(.tab, .data$Family, .data$Model, .data$SpecHash),
    .title = "What produced these spans"
  )

  invisible(.tab)
}

#' What the pass wrote
#'
#' @param .dir Directory holding the family databases.
#' @param .families Family names.
#' @return Tibble: Artifact, Exists, MB.
cor_report_artifacts <- function(.dir, .families) {
  if (FALSE) {
    .dir      <- .lP$Output$Store
    .families <- c("matcon", "lexnlp")
  }

  out_ <- tibble::tibble(
    Path = purrr::map_chr(.families, \(.f) as.character(ner_db_path(.dir = .dir, .family = .f)))
  ) |>
    dplyr::mutate(
      Artifact = fs::path_file(.data$Path),
      Exists   = fs::file_exists(.data$Path),
      MB       = round(dplyr::if_else(
        .data$Exists, as.numeric(fs::file_size(.data$Path)) / 1024^2, NA_real_
      ), 1)
    ) |>
    dplyr::select("Artifact", "Exists", "MB")

  tbl_say(.tab = out_, .title = "Written by 04C")
  invisible(out_)
}
