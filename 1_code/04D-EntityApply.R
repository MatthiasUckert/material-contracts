# 04D-EntityApply: the five rules at corpus scale --------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# 04A measured which family to trust, 04B built and scored a rule per entity, 04C extracted the
# corpus. This decides nothing. It reads the rules as parameters, applies them one ENTITY AT A TIME,
# and writes the same eight long files 04B released with the corpus in them.
#
# THE UNIT OF WORK IS ONE DOCUMENT
# A worker is handed one document's spans and returns that document's rows. Nothing else can be
# affected by it: a contract with an impossible offset, a name the grouping cannot reduce, a date
# outside every range costs exactly that document and no other. There is no batch to lose and no
# partial state to reason about.
#
# It is also the only unit that is provably safe. Every 04B rule reads one document -- grouping
# compares names within a document, the window measures from a party in the same document, the
# ambiguous city takes a state the same contract names -- so a rule applied per document cannot be
# reading a quantity computed over the group it happened to be handed. An earlier version needed a
# whole Validation section to establish that; here it is true by construction.
#
# THE READ IS PER WINDOW AND THE WORK IS PER DOCUMENT
# One DuckDB query loads WindowSize documents' spans, the parent splits them by DocID, and the
# workers take one each. The database is asked a few hundred times rather than a million, and the
# rules still see one contract at a time.
#
# RESUMPTION IS BY DOCUMENT, NOT BY POSITION
# A pass asks which documents the ledger has and which are already written; the difference is the
# work. Nothing is named for a position, so a window size changed between renders costs nothing and a
# population that grew -- a family still finishing extraction -- simply adds to the queue.
#
# A WINDOW COMMITS AS A DIRECTORY. Outputs and a _done.parquet are written into a hidden name and the
# whole directory is renamed into place. A rename within one filesystem is atomic, so a killed
# process leaves a dotfile no glob sees and no reader counts.
#
# _done.parquet NAMES EVERY DOCUMENT PROCESSED, not every document that produced a row. Seven of the
# eight releases carry a row only where the extractor found something -- redact_spans reaches 14% of
# documents -- so resuming on the outputs would reprocess the other 86% on every render, forever.
#
# FIVE HASHES. ORG folds in lexnlp's spec hashes; GPE folds in matcon's AND ORG's, because it reads
# ORG's mention file; DATE, MONEY and REDACT fold in matcon's. REDACT reads matcon's money spans to
# find the ones the filer emptied, but "emptied" is a moneyregex PATTERN NAME rather than a filtered
# result, so it depends on the extractor and not on 04B4. The window size and the worker count are in
# no hash: nothing depends on how the remaining work is divided.
#
# NO COLLAPSE IS WRITTEN. The long files are the release; the EXPORT calls ent_party_facts(),
# geo_collapse(), dte_collapse(), mny_collapse() and red_collapse().
#
# NOTHING OPENS A DOCUMENT. Every cue window was cut at extraction and stored.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .dir_store <- .lP$Input$Store
}


# 1. What each pass is ---------------------------------------------------------------------------------------------------

#: Family is whose ledger defines the population and whose spans the chain reads. Needs is what a
#: document must have finished. Writes is the file stem each pass produces, and Depends is the pass
#: whose output it reads. Declared rather than inferred, because a pass reading an entity its family
#: never ran comes back empty and reports it as a corpus holding none of that entity.
.apl_passes <- tibble::tribble(
  ~Pass,     ~Family,  ~Needs,                  ~Writes,                        ~Depends,
  "ORG",     "lexnlp", c("ORG"),                c("org_mentions"),              NA_character_,
  "GPE",     "matcon", c("GPE", "LAW"),         c("places_geo", "law_clauses"), "ORG",
  "DATE",    "matcon", c("DATE", "TERM"),       c("date_spans", "term_spans"),  NA_character_,
  "MONEY",   "matcon", c("MONEY"),              c("money_spans"),               NA_character_,
  "REDACT",  "matcon", c("REDACT", "MONEY"),    c("redact_spans"),              NA_character_
)



# 2. Policy --------------------------------------------------------------------------------------------------------------

#' What one pass is, as a list
#' @param .pass Character. ORG, GPE, DATE, MONEY or REDACT.
#' @return A one-row list: Pass, Family, Needs, Writes, Depends.
apl_pass_spec <- function(.pass) {
  if (FALSE) .pass <- "GPE"

  row_ <- dplyr::filter(.apl_passes, .data$Pass == .pass)
  if (nrow(row_) != 1L) {
    cli::cli_abort(c(
      "{(.pass)} is not one of the five passes.",
      "i" = "They are: {paste(.apl_passes$Pass, collapse = ', ')}."
    ))
  }
  list(
    Pass    = row_$Pass[[1L]],
    Family  = row_$Family[[1L]],
    Needs   = row_$Needs[[1L]],
    Writes  = row_$Writes[[1L]],
    Depends = row_$Depends[[1L]]
  )
}


#' One hash per pass, standing for every choice that pass makes
#'
#' THE OUTPUT DIRECTORY IS NAMED FOR IT, so a changed rule cannot be served output produced by a
#' rule that no longer exists. Everything already written is worth days on a corpus this size and
#' worth nothing at all if it can answer for the wrong policy.
#'
#' THE EXTRACTOR IS PART OF THE POLICY. Every span this pass reads was produced by a model whose spec
#' hash 04C recorded; re-extracting with a changed gazetteer or a changed money grammar leaves the
#' parameters byte-identical while the input underneath them is different.
#'
#' AND SO IS THE PASS IT DEPENDS ON. GPE reads ORG's mention file, so geography built on parties
#' that have since been regrouped is exactly what this hash exists to prevent being served.
#'
#' THE SCHEDULE IS NOT IN IT. Window size and worker count decide how long a pass takes, not what it
#' answers -- and nothing here depends on how the remaining work is divided.
#'
#' @param .pass Character. Which pass.
#' @param .params The runbook's .lP$Params.
#' @param .describe Output of ner_describe(), carrying one spec hash per model and entity.
#' @param .depends Named character of hashes this pass depends on; NULL where it depends on none.
#' @return Character. A twelve-character hash.
apl_policy_hash <- function(.pass, .params, .describe, .depends = NULL) {
  if (FALSE) {
    .pass     <- "GPE"
    .params   <- .lP$Params
    .describe <- tab_describe
    .depends  <- c(ORG = "abc123")
  }

  spec_ <- apl_pass_spec(.pass = .pass)

  # ONLY THIS PASS'S FAMILY AND ONLY THIS PASS'S ENTITIES. Folding in every model's hash would make a
  # spaCy re-run invalidate the date cache, which is the coarseness this file exists to remove.
  fam_ <- .describe |>
    dplyr::filter(.data$Ready, .data$Family == spec_$Family, .data$Entity %in% spec_$Needs) |>
    dplyr::arrange(.data$Model, .data$Entity) |>
    dplyr::transmute(Row = paste(.data$Model, .data$Entity, .data$SpecHash))

  if (nrow(fam_) == 0L) {
    cli::cli_abort(c(
      "No READY extractor answers for {(.pass)}.",
      "x" = "{(spec_$Family)} / {paste(spec_$Needs, collapse = ', ')}.",
      "i" = "ner_describe() marks a family unready when its dependency is absent -- Docker not
             running, a gazetteer missing. Hashing without it would let two different extractions
             share a cache directory."
    ))
  }

  # THE RULE PARAMETERS THIS PASS ACTUALLY READS, and nothing else. Naming them per pass is what
  # keeps a money filter out of the party hash.
  rule_ <- switch(
    .pass,
    ORG    = list(.params$Rule$Org, .params$Spec$Org, .params$Extras$ORG),
    GPE    = list(.params$Spec$Geo),
    DATE   = list(.params$Spec$Date, .params$CueWin$Date),
    MONEY  = list(.params$Spec$Money),
    REDACT = list(.params$Spec$Redact)
  )

  stringi::stri_sub(
    digest::digest(list(.pass, rule_, fam_$Row, .depends), algo = "xxhash64"), to = 12L
  )
}


#' Every choice this document makes, as a table
#'
#' PRINTED RATHER THAN DESCRIBED. A parameter a reader cannot see is a parameter nobody reviewed, and
#' the render is where this pass is defended.
#'
#' @param .params The runbook's .lP$Params.
#' @return Tibble: Pass, Setting, Value, From.
apl_policy_table <- function(.params) {
  if (FALSE) .params <- .lP$Params

  tibble::tribble(
    ~Pass,    ~Setting,      ~Value,                                          ~From,
    "ORG",    "family",      "lexnlp",                                        "04A",
    "ORG",    "key",         .params$Rule$Org$Key,                            "04B1",
    "ORG",    "merge",       as.character(.params$Rule$Org$MergeFragments),    "04B1",
    "ORG",    "window",      format(.params$Spec$Org$Par, big.mark = ","),    "04B1",
    "ORG",    "tail share",  format(.params$Spec$Org$TailShare),              "04B1",
    "GPE",    "family",      "matcon",                                        "04B2",
    "GPE",    "reach",       format(.params$Spec$Geo$Reach),                  "04B2",
    "DATE",   "start",       .params$Spec$Date$Start,                         "04B3",
    "DATE",   "end",         .params$Spec$Date$End,                           "04B3",
    "DATE",   "cap, years",  format(.params$Spec$Date$CapYears),              "04B3",
    "DATE",   "cue window",  format(.params$CueWin$Date),                     "04B3",
    "MONEY",  "filter",      .params$Spec$Money$Filter,                       "04B4",
    "MONEY",  "cue window",  format(.params$Spec$Money$CueWin),               "04B4",
    "REDACT", "tolerance",   format(.params$Spec$Redact$Tol),                 "04B5",
    "PASS",   "chunk size",  format(.params$ChunkSize, big.mark = ","),       "04D",
    "PASS",   "workers",     format(.params$Workers),                         "04D",
    "PASS",   "limit",       if (is.null(.params$Limit)) "none" else
                               format(.params$Limit, big.mark = ","),         "04D"
  )
}


#' The policy and the five hashes
#' @param .tab Tibble from apl_policy_table().
#' @param .hashes Named character from apl_policy_hash(), one per pass.
#' @param .limit Integer or NULL.
#' @return Invisibly .tab.
apl_report_policy <- function(.tab, .hashes, .limit = NULL) {
  if (FALSE) {
    .tab    <- tab_policy
    .hashes <- .lP$Params$Hash
    .limit  <- .lP$Params$Limit
  }

  cli::cli_h2("The policy")
  tbl_say(.tab = .tab, .title = "Every choice this document makes, and which document made it")

  tibble::tibble(
    Pass    = names(.hashes),
    Hash    = unname(.hashes),
    Depends = purrr::map_chr(names(.hashes),
                             \(.p) dplyr::coalesce(apl_pass_spec(.p)$Depends, "-"))
  ) |>
    tbl_say(.title = "One hash per pass, and what each folds in")

  cli::cli_alert_info(
    "FIVE HASHES AND NOT ONE. Each covers its own rule parameters, its own family's extractor spec \\
     hashes, and the hash of any pass it reads -- so a changed date cap re-runs DATE alone, and a \\
     changed party rule re-runs ORG and GPE because GPE reads ORG's file."
  )
  cli::cli_alert_info(
    "THE CHUNK SIZE AND THE WORKER COUNT ARE NOT IN ANY HASH. They decide how long a pass takes and \\
     not what it answers, and Validation proves that rather than asserting it. Hashing them would \\
     mean re-running a corpus to change a worker count."
  )

  if (!is.null(.limit)) {
    cli::cli_alert_warning(
      "THIS IS A LIMITED RUN OF {format(.limit, big.mark = ',')} \\
       {cli::qty(.limit)}document{?s} PER PASS, and it writes into its own directory. It neither \\
       reads nor invalidates the full pass, and the two can sit on disk together -- but nothing \\
       below describes the corpus."
    )
  }
  invisible(.tab)
}


# 3. Input ---------------------------------------------------------------------------------------------------------------

#' Every document one family finished, from its ledger
#'
#' THE LEDGER AND NOT THE ENTITY TABLES. A document processed and found to hold no date has no row in
#' the date table and is not missing -- it is a contract with no date in it, which is a measurement.
#' Taking the population from the tables would silently drop exactly those documents.
#'
#' FINISHED MEANS A ROW FOR EVERY ENTITY THE PASS NEEDS. A document part-way through 04C's plan would
#' be applied on the entities it has and carry nothing for the rest, which is indistinguishable in
#' the output from a contract that named none of them.
#'
#' THE POPULATION IS PER FAMILY AND NOT PER ENTITY. The preflight measured it: every matcon entity
#' finished the same 1,189,069 documents and every lexnlp entity the same 1,099,419. So this asks the
#' family's ledger for the entities the pass names, and the count is a property of the family.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .pass Character. Which pass.
#' @param .limit Integer or NULL. Take the first N documents, for a test run.
#' @return Tibble: DocID.
apl_corpus_index <- function(.dir_store, .pass, .limit = NULL) {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .pass      <- "GPE"
    .limit     <- 2000L
  }

  spec_ <- apl_pass_spec(.pass = .pass)

  con_ <- ner_db_connect(
    .db_path = ner_db_path(.dir = .dir_store, .family = spec_$Family), .read_only = TRUE
  )
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  in_  <- paste0("'", spec_$Needs, "'", collapse = ", ")
  lim_ <- if (is.null(.limit)) "" else paste0(" LIMIT ", as.integer(.limit))

  # ORDERED BY DocID AND THEN LIMITED, so a limited run is the SAME first N documents for every pass
  # and the five test releases describe one overlapping set rather than five arbitrary draws.
  DBI::dbGetQuery(con_, glue::glue(
    "SELECT DocID FROM runs
      WHERE Entity IN ({in_}) AND Status <> 'error'
      GROUP BY DocID
     HAVING COUNT(DISTINCT Entity) = {length(spec_$Needs)}
      ORDER BY DocID{lim_}"
  )) |>
    tibble::as_tibble()
}


#' The anchor for every document a pass will reach
#'
#' THE REGISTER AND NOTHING ELSE. No release function reads Class: it appears only in the collapses
#' and in reports, both of which join it at their own time. So this document needs no label release,
#' no partial-release check and no dependency on the classification stage at all -- which is three
#' failure modes removed rather than handled.
#'
#' nWords IS CARRIED BECAUSE REDACT DIVIDES BY IT. It is the register's count, which is the only one
#' that reaches the corpus: there is no corpus text file to recount from.
#'
#' @param .path_register 02B's register.
#' @param .index Tibble from apl_corpus_index().
#' @return Tibble: one row per document in .index that has a register row.
apl_keys <- function(.path_register, .index) {
  if (FALSE) {
    .path_register <- .lP$Input$Register
    .index         <- tab_index$ORG
  }

  out_ <- ent_corpus_keys(
    .path_register = .path_register,
    .path_release  = NA_character_,   # no label release; Class reaches no release function
    .doc_ids       = .index$DocID,
    .quiet         = TRUE
  ) |>
    dplyr::semi_join(.index, by = dplyr::join_by(DocID))

  miss_ <- nrow(.index) - nrow(out_)
  if (miss_ > 0L && !.quiet) {
    cli::cli_alert_warning(
      "{format(miss_, big.mark = ',')} {cli::qty(miss_)}document{?s} 04C finished {?has/have} no \\
       register row, so {?it is/they are} not applied. An anchor is the filing date and the filer's \\
       name; a rule cannot measure a start or match a registrant without one."
    )
  }
  out_
}


# 4. What is already written ---------------------------------------------------------------------------------------------

#' The windows a pass has committed
#'
#' A WINDOW IS A DIRECTORY AND ITS PRESENCE IS ITS COMPLETENESS. Everything a window produces goes
#' into a hidden working directory which is then renamed into place, so a killed process leaves a
#' name beginning with a dot that this does not match. There is no state to read and nothing that
#' can be half true.
#'
#' @param .dir Directory named for a pass's policy hash.
#' @return Character vector of directory paths, possibly empty.
apl_batches <- function(.dir) {
  if (FALSE) .dir <- .lP$Output$Store[["ORG"]]

  if (!fs::dir_exists(.dir)) return(character(0))
  fs::dir_ls(.dir, regexp = "/window-[^/]+$", type = "directory", recurse = FALSE)
}


#' Which documents a pass has already processed
#'
#' THE DONE FILE AND NOT THE OUTPUT'S DocID COLUMN. Seven of the eight releases carry a row only
#' where the extractor found something -- redact_spans reaches 14% of documents -- so resuming on
#' what the outputs contain would reprocess the other 86% on every render, forever. This names every
#' document a batch PROCESSED, whatever that document produced.
#'
#' ONE COLUMN OVER A FEW HUNDRED SMALL FILES, so it is seconds even at corpus scale.
#'
#' @param .dir Directory named for a pass's policy hash.
#' @return Character vector of DocIDs.
apl_done_ids <- function(.dir) {
  if (FALSE) .dir <- .lP$Output$Store[["ORG"]]

  files_ <- fs::path(apl_batches(.dir = .dir), "_done.parquet")
  files_ <- files_[fs::file_exists(files_)]
  if (length(files_) == 0L) return(character(0))

  arrow::open_dataset(sources = files_) |>
    dplyr::select("DocID") |>
    dplyr::collect() |>
    dplyr::pull("DocID") |>
    unique()
}


#' One released file, as an arrow dataset
#'
#' THE DIRECTORY IS THE RELEASE. A directory of parquets already is a dataset, so there is no final
#' bind and no moment where a partial pass could be assembled into something that looks finished.
#'
#' @param .dir Directory named for a pass's policy hash.
#' @param .stem Character. The file stem.
#' @return An arrow Dataset, or NULL where no window has been committed.
apl_dataset <- function(.dir, .stem) {
  if (FALSE) {
    .dir  <- .lP$Output$Store[["ORG"]]
    .stem <- "org_mentions"
  }

  files_ <- fs::path(apl_batches(.dir = .dir), paste0(.stem, ".parquet"))
  files_ <- files_[fs::file_exists(files_)]
  if (length(files_) == 0L) return(NULL)
  arrow::open_dataset(sources = files_)
}


#' The DocID range of every file the dependency wrote
#'
#' BUILT ONCE IN THE PARENT AND HANDED TO EVERY CHUNK, and the alternative was catastrophic. Without
#' it, apl_read_dep() opened the WHOLE dependency -- at corpus scale roughly 220 files and 22 million
#' mention rows, one of whose columns is raw span text -- and trusted arrow to push a range filter
#' down far enough to skip most of it. It does not, or not reliably: every worker materialised a
#' large share of the entire ORG release, and eight of them at once took 250 GB.
#'
#' A CHUNK OVERLAPS ONE OR TWO FILES, NEVER TWO HUNDRED. Both passes cut sorted DocID lists, so a
#' chunk's [lo, hi] meets a contiguous run of the dependency's files. Knowing each file's range turns
#' the read from a scan of everything into opening exactly what is needed.
#'
#' ONLY THE DocID COLUMN IS READ. min() and max() over one column of each file is seconds for the
#' whole release, and it happens once per pass rather than once per chunk.
#'
#' @param .dir Directory holding the dependency's chunk files.
#' @param .stem Character. The file stem.
#' @return Tibble: File, Lo, Hi. Empty where the directory holds nothing.
apl_dep_index <- function(.dir, .stem) {
  if (FALSE) {
    .dir  <- .lP$Output$Store[["ORG"]]
    .stem <- "org_mentions"
  }

  if (is.null(.dir)) return(tibble::tibble())
  files_ <- fs::path(apl_batches(.dir = .dir), paste0(.stem, ".parquet"))
  files_ <- files_[fs::file_exists(files_)]
  if (length(files_) == 0L) return(tibble::tibble())

  purrr::map(files_, function(.f) {
    r_ <- arrow::open_dataset(sources = .f) |>
      dplyr::summarise(Lo = min(.data$DocID), Hi = max(.data$DocID)) |>
      dplyr::collect()
    tibble::tibble(File = as.character(.f), Lo = r_$Lo[[1L]], Hi = r_$Hi[[1L]])
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$Lo)
}


#' One window's worth of another pass's output
#'
#' THE CHUNK DIRECTORY IS AN ARROW DATASET, so this is a filtered read rather than a bind. GPE reads
#' ORG's mentions this way, and it has to: the two passes run on different populations -- lexnlp's
#' 1,099,419 documents against matcon's 1,189,069 -- so chunk seven of one is not chunk seven of the
#' other and matching by index would silently pair the wrong documents.
#'
#' THE RANGE FILTER IS WHAT MAKES IT CHEAP. A chunk is a contiguous run of sorted DocIDs, so bounding
#' on the range pushes into the parquet reader and skips whole files; the exact membership test then
#' runs on what survives. A bare %in% over thirty million rows would scan every file for every chunk.
#'
#' @param .dir Directory holding the other pass's chunk files.
#' @param .stem Character. The file stem to read.
#' @param .doc_ids Character. This chunk's documents.
#' @return Tibble, empty where the directory holds nothing yet.
apl_read_dep <- function(.stem, .doc_ids, .index) {
  if (FALSE) {
    .stem    <- "org_mentions"
    .doc_ids <- todo_[1:5000]
    .index   <- tab_dep
  }

  if (is.null(.index) || nrow(.index) == 0L) {
    cli::cli_abort(c(
      "The pass that writes {(.stem)} has committed nothing.",
      "i" = "Running this one anyway would attach every span to nothing and record that as a
             finding about the corpus."
    ))
  }

  files_ <- {
    lo0_ <- min(.doc_ids)
    hi0_ <- max(.doc_ids)
    # OVERLAP AND NOT CONTAINMENT. Two intervals meet when each starts before the other ends, and a
    # chunk of one population straddles a file boundary of the other whenever the two populations
    # differ -- which they do, by about ninety thousand documents.
    .index$File[.index$Lo <= hi0_ & lo0_ <= .index$Hi]
  }

  # AN EMPTY DIRECTORY AND AN EMPTY RESULT ARE OPPOSITE FINDINGS, and returning a bare tibble for
  # both was a defect. A directory with no files means the pass this one depends on has not run: the
  # chain would then attach every place to nothing and write a corpus in which no place sits near a
  # party, which is indistinguishable in the output from a corpus where none does. A filter that
  # returns no rows is ordinary -- ORG runs on lexnlp's population and GPE on matcon's, so a chunk
  # can legitimately hold documents ORG has not reached.
  # AN INDEX THAT MATCHES NOTHING IS NOT AN EMPTY DIRECTORY. Where the index has rows but none of
  # them overlap, the dependency simply never reached these documents -- which is ordinary, because
  # ORG runs on lexnlp's population and GPE on matcon's.
  if (length(files_) == 0L) {
    return(dplyr::filter(arrow::read_parquet(.index$File[[1L]]), FALSE))
  }

  lo_ <- min(.doc_ids)
  hi_ <- max(.doc_ids)

  # THE RANGE FILTER PUSHES INTO THE READER AND THE MEMBERSHIP TEST DOES NOT, so the order matters:
  # bounding first skips whole files, and the exact test then runs on what survives. A zero-row
  # result still carries the file's own columns, which is what the chains downstream need.
  arrow::open_dataset(sources = files_) |>
    dplyr::filter(.data$DocID >= lo_, .data$DocID <= hi_) |>
    dplyr::collect() |>
    dplyr::filter(.data$DocID %in% .doc_ids)
}


# 5. One window of documents ---------------------------------------------------------------------------------------------

#' Every span one pass needs, for one window of documents
#'
#' ONE QUERY PER ENTITY PER WINDOW, and that is the whole reason a window exists. The rules run per
#' document; the DATABASE does not want to be asked per document. Loading twenty thousand documents'
#' spans in one query and then splitting them in memory asks DuckDB a few hundred times over a corpus
#' rather than a million.
#'
#' THE LOADERS ARE 04B'S OWN. ent_load_org() adds the two candidate keys the reduction reads;
#' dte_describe() cuts the cue window and computes the gap to the filing date. Nothing is
#' reimplemented here.
#'
#' @param .pass Character. Which pass.
#' @param .keys Tibble. The window's anchor rows.
#' @param .lens Tibble. The window's document lengths.
#' @param .params The runbook's .lP$Params.
#' @param .dir_store Directory holding the family databases.
#' @param .dep_index Tibble from apl_dep_index(). GPE only.
#' @return A named list of tibbles, one per input the chain reads.
apl_load_window <- function(.pass, .keys, .lens, .params, .dir_store, .dep_index = NULL) {
  if (FALSE) {
    .pass      <- "GPE"
    .keys      <- keys_
    .lens      <- lens_
    .params    <- .lP$Params
    .dir_store <- .lP$Input$Store
    .dep_index <- tab_dep
  }

  switch(
    .pass,
    ORG = list(
      Spans  = ent_load_org(.dir_store = .dir_store, .lens = .lens, .family = "lexnlp",
                            .extras = .params$Extras$ORG, .quiet = TRUE),
      Filers = ent_filer_keys(.path_register = .params$PathRegister,
                              .doc_ids = .keys$DocID, .quiet = TRUE)
    ),
    GPE = list(
      Spans    = ent_load_entity(.dir_store = .dir_store, .family = "matcon", .entity = "GPE",
                                 .lens = .lens, .extras = ent_extras("matcon", "GPE"),
                                 .quiet = TRUE),
      Law      = ent_load_entity(.dir_store = .dir_store, .family = "matcon", .entity = "LAW",
                                 .lens = .lens, .extras = ent_extras("matcon", "LAW"),
                                 .quiet = TRUE),
      Mentions = apl_read_dep(.stem = "org_mentions", .doc_ids = .keys$DocID, .index = .dep_index)
    ),
    DATE = list(
      Spans = dte_describe(
        .dates = dplyr::filter(
          dte_load(.dir_store = .dir_store, .lens = .lens, .family = "matcon", .quiet = TRUE),
          .data$Parsed
        ),
        .keys = .keys, .win = .params$CueWin$Date
      ),
      Terms = dte_load_terms(.dir_store = .dir_store, .lens = .lens, .quiet = TRUE)
    ),
    MONEY = list(
      Spans = mny_load(.dir_store = .dir_store, .lens = .lens, .family = "matcon", .quiet = TRUE)
    ),
    REDACT = list(
      Spans = red_load(.dir_store = .dir_store, .lens = .lens, .family = "matcon", .quiet = TRUE),
      Money = mny_load(.dir_store = .dir_store, .lens = .lens, .family = "matcon", .quiet = TRUE),
      Words = red_words(.keys = .keys, .path_text = NULL, .quiet = TRUE)
    )
  )
}


#' Split a table into one element per document, empties included
#'
#' A DOCUMENT WITH NO SPANS STILL GETS A JOB, and split() alone will not give one. Every 04B apply
#' runs FROM THE KEY SIDE so that a contract in which nothing was found still produces a row -- the
#' sentinel in org_mentions is exactly that -- so a document absent from the spans must arrive at its
#' worker as an EMPTY FRAME WITH THE RIGHT COLUMNS rather than not arrive at all.
#'
#' @param .tab Tibble carrying DocID.
#' @param .doc_ids Character. Every document the window covers.
#' @return A list the length of .doc_ids, aligned with it by POSITION and deliberately unnamed.
apl_split_docs <- function(.tab, .doc_ids) {
  if (FALSE) {
    .tab     <- win_$Spans
    .doc_ids <- keys_$DocID
  }

  empty_ <- .tab[0L, , drop = FALSE]
  if (nrow(.tab) == 0L) return(rep(list(empty_), length(.doc_ids)))

  # ONE VECTORISED match() AND THEN POSITIONAL INDEXING. by_[[.d]] with a CHARACTER key looks like a
  # lookup and is a linear scan: R does not hash list names. Doing that once per document over a
  # window of twenty thousand is twenty thousand scans of a twenty-thousand-element list per table --
  # hundreds of millions of string comparisons, single-threaded, in the parent, while every worker
  # waits. match() is one pass; everything after it is by position.
  #
  # RETURNED UNNAMED, deliberately. A name on this list invites the same lookup back.
  by_  <- split(.tab, .tab$DocID)
  idx_ <- match(.doc_ids, names(by_))
  out_ <- vector("list", length(.doc_ids))
  hit_ <- !is.na(idx_)
  out_[hit_]  <- by_[idx_[hit_]]
  out_[!hit_] <- list(empty_)
  out_
}


#' Apply one entity's rule to ONE document
#'
#' THE UNIT OF WORK, AND THE REASON NOTHING CAN BREAK. Every call here is 04B's own, given one
#' document's spans and one row of keys. A contract that fails costs exactly that contract: its
#' neighbours are in other calls, in other processes, and cannot be touched by it.
#'
#' IT IS ALSO WHAT MAKES THE PARTITION IRRELEVANT. Every 04B rule reads one document, so a rule
#' applied per document cannot be computing a quantity over the group it was handed. An earlier
#' version needed a whole Validation section to establish that; here there is no group to compute
#' over.
#'
#' REDACT USES THE MONEY SPANS IT WAS GIVEN, not MONEY's released file. It needs the ones the filer
#' emptied, and that is a moneyregex pattern name rather than a filtered result.
#'
#' @param .pass Character. Which pass.
#' @param .job A list carrying this document's slice of every input.
#' @param .params The runbook's .lP$Params.
#' @param .geo List: the gazetteer. GPE only.
#' @return A named list of tibbles, one per file this pass writes.
apl_doc_apply <- function(.pass, .job, .params, .geo = NULL) {
  if (FALSE) {
    .pass   <- "ORG"
    .job    <- jobs_[[1L]]
    .params <- .lP$Params
    .geo    <- geo_once
  }

  keys_ <- .job$Keys
  lens_ <- .job$Lens

  if (.pass == "ORG") {
    res_ <- ent_apply(
      .spans  = .job$Spans, .keys = keys_, .lens = lens_,
      .rule   = .params$Rule$Org, .spec = .params$Spec$Org, .filers = .job$Filers
    )
    return(list(org_mentions = res_$Release))
  }

  if (.pass == "GPE") {
    res_ <- geo_apply(
      .spans = .job$Spans, .law = .job$Law, .mentions = .job$Mentions,
      .geo   = .geo, .spec = .params$Spec$Geo
    )
    return(list(places_geo = res_$Release, law_clauses = res_$Law))
  }

  if (.pass == "DATE") {
    res_ <- dte_apply(
      .dates = .job$Spans, .terms = .job$Terms, .keys = keys_, .spec = .params$Spec$Date
    )
    return(list(date_spans = res_$Dates, term_spans = res_$Terms))
  }

  if (.pass == "MONEY") {
    res_ <- mny_apply(.money = .job$Spans, .keys = keys_, .spec = .params$Spec$Money)
    return(list(money_spans = res_$Release))
  }

  if (.pass == "REDACT") {
    res_ <- red_apply(
      .marks = .job$Spans, .money = .job$Money, .words = .job$Words,
      .keys  = keys_, .spec = .params$Spec$Redact
    )
    return(list(redact_spans = res_$Release))
  }

  cli::cli_abort("{(.pass)} is not one of the five passes.")
}


#' Cut a window's inputs into one job per document
#' @param .pass Character. Which pass.
#' @param .win List from apl_load_window().
#' @param .keys Tibble. The window's anchor rows.
#' @param .lens Tibble. The window's document lengths.
#' @return A list the length of the window, named by DocID.
apl_window_jobs <- function(.pass, .win, .keys, .lens) {
  if (FALSE) {
    .pass <- "GPE"
    .win  <- win_
    .keys <- keys_
    .lens <- lens_
  }

  ids_   <- .keys$DocID
  nms_   <- names(.win)
  parts_ <- purrr::map(.win, \(.t) apl_split_docs(.tab = .t, .doc_ids = ids_))
  keys1_ <- apl_split_docs(.tab = .keys, .doc_ids = ids_)
  lens1_ <- apl_split_docs(.tab = .lens, .doc_ids = ids_)

  # EVERY INDEX HERE IS AN INTEGER. The lists came back aligned with ids_ by position, so assembling
  # the jobs is one pass rather than one linear scan per document per table.
  purrr::set_names(
    purrr::map(seq_along(ids_), function(.i) {
      c(list(DocID = ids_[[.i]], Keys = keys1_[[.i]], Lens = lens1_[[.i]]),
        purrr::set_names(purrr::map(parts_, \(.p) .p[[.i]]), nms_))
    }),
    ids_
  )
}


# 6. One entity, end to end ----------------------------------------------------------------------------------------------

#' Run one pass over whatever is not already written
#'
#' READ PER WINDOW, WORK PER DOCUMENT, COMMIT PER WINDOW. The database is asked once per window, the
#' rules see one contract at a time, and the outputs land as one atomic directory.
#'
#' EVERY DAEMON SOURCES THE LIBRARIES BY PATH, and receives the parameters and the gazetteer once
#' rather than once per document. A closure does not cross a process boundary with its environment,
#' so a worker handed a function that calls ent_apply() finds no ent_apply(); and sending 181,810
#' gazetteer rows with every document would cost more than the rule.
#'
#' A DOCUMENT THAT FAILS IS RECORDED AND EXCLUDED FROM _done, so the window still commits and that one
#' contract is retried on the next render. Nothing else is affected by it, which is the point of
#' making the document the unit.
#'
#' @param .pass Character. Which pass.
#' @param .keys Tibble from apl_keys().
#' @param .todo Character. Documents not yet written.
#' @param .dir_out Directory named for this pass's policy hash.
#' @param .params The runbook's .lP$Params.
#' @param .dir_store Directory holding the family databases.
#' @param .geo List: the gazetteer, for the serial path. A daemon builds its own.
#' @param .dep_index Tibble from apl_dep_index(). GPE only.
#' @param .path_lookup The gazetteer parquet, for the daemons to read.
#' @param .path_fun Character. Library files every daemon sources.
#' @return Tibble: one row per window.
apl_entity_run <- function(.pass, .keys, .todo, .dir_out, .params, .dir_store,
                           .geo = NULL, .dep_index = NULL, .path_lookup = NULL,
                           .path_fun = character()) {
  if (FALSE) {
    .pass      <- "ORG"
    .keys      <- tab_keys$ORG
    .todo      <- todo_
    .dir_out   <- .lP$Output$Store[["ORG"]]
    .params    <- .lP$Params
    .dir_store <- .lP$Input$Store
  }

  fs::dir_create(.dir_out)
  cli::cli_h3("{(.pass)}")

  if (length(.todo) == 0L) {
    cli::cli_alert_success("Nothing outstanding.")
    return(tibble::tibble())
  }

  stems_   <- apl_pass_spec(.pass = .pass)$Writes
  windows_ <- split(.todo, ceiling(seq_along(.todo) / .params$WindowSize))
  run_     <- format(Sys.time(), "%Y%m%dT%H%M%S")
  n_work_  <- max(as.integer(.params$Workers), 1L)

  cli::cli_alert_info(
    "{format(length(.todo), big.mark = ',')} outstanding, in {length(windows_)} \\
     {cli::qty(length(windows_))}window{?s} of {format(.params$WindowSize, big.mark = ',')}, one \\
     document per worker."
  )

  if (n_work_ > 1L) {
    mirai::daemons(n_work_)
    on.exit(mirai::daemons(0L), add = TRUE)   # SAFE ONLY INSIDE A FUNCTION BODY, which this is

    # SENT ONCE PER DAEMON AND NOT ONCE PER DOCUMENT. everywhere() with .args evaluates its
    # expression in an environment carrying those arguments, whose parent is the daemon's global --
    # so a plain assignment binds THERE and is gone when the call returns. assign() into globalenv()
    # is what makes these survive to the next task.
    mirai::everywhere(
      {
        purrr::walk(.files, \(.f) source(.f, encoding = "UTF-8"))
        assign("apl_params", .par, envir = globalenv())
        assign(
          x     = "geo_once",
          value = if (is.null(.lookup)) NULL else list(
            Lookup = geo_lookup(.path_lookup = .lookup),
            Cand   = geo_candidates(.path_lookup = .lookup)
          ),
          envir = globalenv()
        )
      },
      .args = list(.files = .path_fun, .par = .params,
                   .lookup = if (.pass == "GPE") .path_lookup else NULL)
    )
  }

  t0_   <- Sys.time()
  seen_ <- 0L

  out_ <- purrr::imap(windows_, function(.ids, .w) {
    keys_ <- dplyr::filter(.keys, .data$DocID %in% .ids)
    lens_ <- keys_ |>
      dplyr::transmute(DocID, DocLen = as.integer(.data$nChars)) |>
      dplyr::filter(!is.na(.data$DocLen), .data$DocLen > 0L)

    tic_ <- Sys.time()
    win_ <- apl_load_window(.pass = .pass, .keys = keys_, .lens = lens_, .params = .params,
                            .dir_store = .dir_store, .dep_index = .dep_index)
    load_ <- as.numeric(difftime(Sys.time(), tic_, units = "secs"))

    jobs_ <- apl_window_jobs(.pass = .pass, .win = win_, .keys = keys_, .lens = lens_)
    rm(win_)

    tic_ <- Sys.time()
    res_ <- if (n_work_ <= 1L) {
      purrr::map(jobs_, \(.j) try(
        apl_doc_apply(.pass = .pass, .job = .j, .params = .params, .geo = .geo), silent = TRUE
      ))
    } else {
      mirai::mirai_map(
        .x = jobs_,
        .f = function(.job, .pass) {
          try(apl_doc_apply(.pass = .pass, .job = .job, .params = apl_params, .geo = geo_once),
              silent = TRUE)
        },
        .args = list(.pass = .pass)
      )[]
    }
    work_ <- as.numeric(difftime(Sys.time(), tic_, units = "secs"))

    # A FAILURE IS A VALUE HERE, TWICE OVER: try() returns its condition and mirai returns a
    # miraiError, and neither throws. Both are checked by class before anything is read as a result.
    bad_ <- purrr::map_lgl(res_, \(.r) inherits(.r, c("try-error", "miraiError")))
    ok_  <- res_[!bad_]

    tabs_ <- purrr::set_names(
      purrr::map(stems_, \(.s) purrr::list_rbind(purrr::map(ok_, \(.r) .r[[.s]]))), stems_
    )

    id_   <- paste0(run_, "-", formatC(as.integer(.w), width = 4L, flag = "0"))
    work_dir_ <- fs::path(.dir_out, paste0(".window-", id_, ".part"))
    if (fs::dir_exists(work_dir_)) fs::dir_delete(work_dir_)
    fs::dir_create(work_dir_)

    purrr::iwalk(tabs_, \(.t, .n) arrow::write_parquet(.t, fs::path(work_dir_,
                                                                    paste0(.n, ".parquet"))))
    # ONLY THE DOCUMENTS THAT SUCCEEDED, so a failure is retried rather than recorded as done. And
    # every document that succeeded, whether or not it produced a row -- which is what makes
    # resumption stable for a release most documents contribute nothing to.
    arrow::write_parquet(
      tibble::tibble(DocID = names(ok_)), fs::path(work_dir_, "_done.parquet")
    )
    fs::file_move(work_dir_, fs::path(.dir_out, paste0("window-", id_)))

    seen_ <<- seen_ + length(.ids)
    el_   <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))
    cli::cli_alert_info(
      "  {(.w)}/{length(windows_)} | {format(seen_, big.mark = ',')} docs | \\
       {round(seen_ / max(el_, 1e-9), 1)}/s | read {round(load_)}s, rules {round(work_)}s"
    )

    tibble::tibble(
      Pass = .pass, Window = id_, Docs = length(.ids), Failed = sum(bad_),
      Rows = as.integer(sum(purrr::map_int(tabs_, nrow))),
      LoadSecs = load_, WorkSecs = work_
    )
  }) |>
    purrr::list_rbind()

  fail_ <- sum(out_$Failed)
  if (fail_ > 0L) {
    cli::cli_alert_danger(
      "{format(fail_, big.mark = ',')} {cli::qty(fail_)}document{?s} failed and {?was/were} left \\
       out of _done, so {?it is/they are} retried on the next render. Nothing else was affected: \\
       the unit of work is one document."
    )
  }
  out_
}


#' What every pass did, and what is left
#' @param .tab Tibble from the entity runs, bound together.
#' @param .index Named list of tibbles from apl_corpus_index().
#' @param .dirs Named character of output directories.
#' @return Invisibly the summary.
apl_report_pass <- function(.tab, .index, .dirs) {
  if (FALSE) {
    .tab   <- tab_pass
    .index <- tab_index
    .dirs  <- .lP$Output$Store
  }

  cli::cli_h2("What this render's passes did")

  out_ <- purrr::map(names(.dirs), function(.p) {
    p_    <- dplyr::filter(.tab, .data$Pass == .p)
    done_ <- length(apl_done_ids(.dir = .dirs[[.p]]))
    pop_  <- nrow(.index[[.p]])
    tibble::tibble(
      Pass = .p, Windows = nrow(p_), DocsRun = sum(p_$Docs), Failed = sum(p_$Failed),
      LoadSecs = sum(p_$LoadSecs), WorkSecs = sum(p_$WorkSecs),
      Done = done_, Population = pop_, Left = max(pop_ - done_, 0L)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      PerSecond = .data$DocsRun / pmax(.data$LoadSecs + .data$WorkSecs, 1e-9),
      LoadShare = .data$LoadSecs / pmax(.data$LoadSecs + .data$WorkSecs, 1e-9)
    )

  out_ |>
    dplyr::mutate(
      dplyr::across(c(DocsRun, Done, Population, Left), \(.x) format(.x, big.mark = ",")),
      dplyr::across(c(LoadSecs, WorkSecs, PerSecond), \(.x) tbl_num(.x)),
      LoadShare = tbl_pct(.data$LoadShare)
    ) |>
    tbl_say(.title = "One row per pass")

  cli::cli_alert_info(
    "LoadShare IS THE READ AGAINST THE RULES. Loading is DuckDB and already uses every core; the \\
     rules are single-threaded R, one document at a time. A high share means the window is too small \\
     -- the database is being asked too often -- and a low one means workers are what help."
  )

  left_ <- sum(out_$Left)
  if (left_ > 0L) {
    cli::cli_alert_info(
      "{format(left_, big.mark = ',')} {cli::qty(left_)}document{?s} outstanding. Re-render to \\
       continue: each pass asks the ledger what it has and the directory what is written."
    )
  } else {
    cli::cli_alert_success("Every document in every population has been written.")
  }
  invisible(out_)
}


# 7. The released files --------------------------------------------------------------------------------------------------

#' What every released file holds
#' @param .dirs Named character of output directories.
#' @param .index Named list of tibbles from apl_corpus_index().
#' @return Tibble: one row per file.
apl_table_outputs <- function(.dirs, .index) {
  if (FALSE) {
    .dirs  <- .lP$Output$Store
    .index <- tab_index
  }

  purrr::map(names(.dirs), function(.p) {
    stems_ <- apl_pass_spec(.pass = .p)$Writes
    purrr::map(stems_, function(.s) {
      ds_ <- apl_dataset(.dir = .dirs[[.p]], .stem = .s)
      if (is.null(ds_)) {
        return(tibble::tibble(Pass = .p, File = .s, Rows = 0L, Docs = 0L, Cols = 0L, PctPop = 0))
      }
      n_ <- ds_ |>
        dplyr::summarise(N = dplyr::n(), D = dplyr::n_distinct(.data$DocID)) |>
        dplyr::collect()
      tibble::tibble(
        Pass = .p, File = .s,
        Rows = as.integer(n_$N[[1L]]), Docs = as.integer(n_$D[[1L]]),
        Cols = length(names(ds_)),
        PctPop = n_$D[[1L]] / pmax(nrow(.index[[.p]]), 1L)
      )
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
}


#' What every released file holds, reported
#' @param .tab Tibble from apl_table_outputs().
#' @return Invisibly .tab.
apl_report_outputs <- function(.tab) {
  if (FALSE) {
    .tab <- tab_out
  }

  cli::cli_h2("The eight released files")
  .tab |>
    dplyr::mutate(
      dplyr::across(c(Rows, Docs), \(.x) format(.x, big.mark = ",")),
      PctPop = tbl_pct(.data$PctPop)
    ) |>
    tbl_say(.title = "One row per file, over every chunk written so far")

  cli::cli_alert_info(
    "ONLY org_mentions IS BUILT FROM THE KEY SIDE, so it alone must read 100%: 04B1 writes a sentinel \\
     row for a contract in which no organisation was found, which is what makes n_distinct(DocID) \\
     over that file the whole population. The other seven are SPAN files -- a row exists where the \\
     extractor found something -- so PctPop below 100% there is a measurement about the corpus and \\
     not a gap in the pass."
  )
  cli::cli_alert_info(
    "THE DENOMINATOR FOR A RATE IS THEREFORE THE PASS'S POPULATION AND NEVER THE FILE'S DocID COUNT. \\
     A contract with no redaction marker has no row in redact_spans and still redacted nothing, \\
     which is a finding; counting only the documents present would report every contract as having \\
     redacted something."
  )
  cli::cli_alert_info(
    "THE DIRECTORY IS THE RELEASE. There is no bind and no single file, so nothing here can be \\
     assembled from a partial pass into an artifact that looks finished."
  )

  invisible(.tab)
}


#' Every released file against the dictionary its 04B document declared
#'
#' THE SAME CHECK 04B RUNS, ON THE CORPUS RATHER THAN THE SAMPLE. Each 04B document builds a
#' dictionary from a declared list and aborts where it disagrees with the file it wrote; running the
#' same functions here is what says the corpus release has the same shape as the sample release, which
#' is the whole claim this document makes.
#'
#' SCHEMA FROM ARROW, NEVER INFERRED. names(open_dataset()) reads the parquet's own schema, so a
#' column added or renamed by a chain is seen as the file has it rather than as this document expects.
#'
#' @param .dirs Named character of output directories.
#' @return Tibble: one row per file.
apl_check_schema <- function(.dirs) {
  if (FALSE) .dirs <- .lP$Output$Store

  spec_ <- list(
    org_mentions = list(Fun = ent_dictionary_mentions, Doc = "04B1"),
    places_geo   = list(Fun = geo_dictionary_places,   Doc = "04B2"),
    law_clauses  = list(Fun = geo_dictionary_law,      Doc = "04B2"),
    date_spans   = list(Fun = dte_dictionary_dates,    Doc = "04B3"),
    term_spans   = list(Fun = dte_dictionary_terms,    Doc = "04B3"),
    money_spans  = list(Fun = mny_dictionary_spans,    Doc = "04B4"),
    redact_spans = list(Fun = red_dictionary_spans,    Doc = "04B5")
  )

  purrr::map(names(.dirs), function(.p) {
    purrr::map(apl_pass_spec(.pass = .p)$Writes, function(.s) {
      ds_ <- apl_dataset(.dir = .dirs[[.p]], .stem = .s)
      if (is.null(ds_)) {
        return(tibble::tibble(File = .s, From = spec_[[.s]]$Doc, Cols = 0L, Agrees = NA))
      }
      # BUILT FROM THE SCHEMA AND NOT QUERIED. The dictionary functions compare names() and nothing
      # else, so no row has to be read -- and arrow refuses filter(FALSE) anyway, because it wants a
      # column expression rather than a literal. Constructing the frame from names() is both correct
      # and free.
      empty_ <- purrr::set_names(
        purrr::map(names(ds_), \(.x) logical(0)), names(ds_)
      ) |>
        tibble::as_tibble()
      ok_    <- try(spec_[[.s]]$Fun(empty_), silent = TRUE)
      tibble::tibble(
        File = .s, From = spec_[[.s]]$Doc, Cols = length(names(ds_)),
        Agrees = !inherits(ok_, "try-error")
      )
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
}


#' The schema check, reported
#' @param .tab Tibble from apl_check_schema().
#' @return Invisibly .tab.
apl_report_schema <- function(.tab) {
  if (FALSE) .tab <- tab_schema

  cli::cli_h2("The corpus files have the shape 04B released")
  tbl_say(.tab = .tab, .title = "Each file, against the dictionary its 04B document declared")

  bad_ <- dplyr::filter(.tab, !is.na(.data$Agrees), !.data$Agrees)
  if (nrow(bad_) > 0L) {
    cli::cli_abort(c(
      "{nrow(bad_)} corpus file{?s} {?does/do} not match the dictionary {?its/their} 04B document
       declared.",
      "x" = "{paste(bad_$File, collapse = ', ')}.",
      "i" = "The rule that wrote it changed without the dictionary following, or this pass is
             calling a version of it the runbook does not name."
    ))
  }
  cli::cli_alert_success(
    "Every corpus file carries exactly the columns its 04B document documents, so a reader who knows \\
     the sample release knows this one."
  )
  invisible(.tab)
}


# 8. The rule against the naive ladder -----------------------------------------------------------------------------------

#' Every entity's naive ladder, computed batch by batch over the corpus
#'
#' THE COLLAPSE FUNCTIONS AND NOT A SECOND IMPLEMENTATION. Each rung below comes from the same
#' ent_doc_facts(), geo_doc_facts(), dte_collapse(), mny_collapse() or red_collapse() that 04B
#' reports from, so a corpus number and a sample number are the same query on different data. Writing
#' the arithmetic again in arrow would be faster and would let the two disagree at the third decimal.
#'
#' CHUNK BY CHUNK, AND ONLY THE DOCUMENT-LEVEL RESULT IS KEPT. Every collapse is per document, so a
#' chunk's collapse is exact; binding the document rows gives a table of about a million rows and a
#' dozen columns, which is small. Collecting the long files themselves would not be.
#'
#' NOTHING IS WRITTEN. These are report artifacts. The export materialises the collapses properly.
#'
#' @param .pass Character. Which pass.
#' @param .dir Directory named for that pass's policy hash.
#' @param .dep_index Tibble from apl_dep_index(). GPE only.
#' @param .keys Tibble from apl_keys().
#' @param .params The runbook's .lP$Params.
#' @return Tibble: one row per document, with the entity's ladder columns.
apl_facts <- function(.pass, .dir, .keys, .params, .dep_index = NULL) {
  if (FALSE) {
    .pass    <- "DATE"
    .dir     <- .lP$Output$Store[["DATE"]]
    .keys    <- tab_keys$DATE
    .params  <- .lP$Params
  }

  stems_ <- apl_pass_spec(.pass = .pass)$Writes
  dirs_  <- apl_batches(.dir = .dir)
  if (length(dirs_) == 0L) return(tibble::tibble())

  purrr::map(dirs_, function(.b) {
    read_ <- function(.s) {
      p_ <- fs::path(.b, paste0(.s, ".parquet"))
      if (fs::file_exists(p_)) arrow::read_parquet(p_) else tibble::tibble()
    }
    done_  <- arrow::read_parquet(fs::path(.b, "_done.parquet"))$DocID
    first_ <- read_(stems_[[1L]])

    # THE DENOMINATOR IS _done.parquet AND NOT THE OUTPUT. Seven of the eight releases carry a row
    # only where the extractor found something, so restricting the keys to the documents PRESENT
    # would compute every rate over the documents that HAD the thing -- "contracts carrying a
    # marker" came out at 100% because the denominator was contracts carrying a marker.
    keys_ <- dplyr::filter(.keys, .data$DocID %in% done_)
    if (nrow(keys_) == 0L) return(tibble::tibble())

    switch(
      .pass,
      ORG    = ent_doc_facts(
        .release = first_,
        .lens    = dplyr::transmute(keys_, DocID, DocLen = as.integer(.data$nChars)),
        .spec    = .params$Spec$Org
      ),
      GPE    = geo_doc_facts(
        .release = first_,
        .party   = ent_party_facts(
          .release = apl_read_dep(.stem = "org_mentions", .doc_ids = done_,
                                  .index = .dep_index)
        ),
        .keys    = keys_
      ),
      DATE   = dte_collapse(.dates = first_, .terms = read_("term_spans"),
                            .keys = keys_, .spec = .params$Spec$Date),
      MONEY  = mny_collapse(.release = first_, .keys = keys_, .spec = .params$Spec$Money),
      REDACT = red_collapse(.release = first_,
                            .words = red_words(.keys = keys_, .path_text = NULL, .quiet = TRUE),
                            .keys = keys_)
    )
  }, .progress = TRUE) |>
    purrr::list_rbind()
}


#' The rule against the naive definition, one row per entity
#'
#' EVERY ROW IS A LADDER RUNG AND THE PAIR IS THE RESULT. What a reader gets without the rule, and
#' what the rule gives. Each pair is the corpus version of a number 04B already reported on 4,398
#' documents, and the comparison is the point of having run 04A at all: a corpus figure far from the
#' sample figure is either a finding about representativeness or a defect in this pass.
#'
#' @param .facts Named list of tibbles from apl_facts().
#' @return Tibble: Entity, Measure, Naive, Rule, Change.
apl_table_naive <- function(.facts) {
  if (FALSE) .facts <- tab_facts

  say_ <- function(.e, .m, .n, .r) {
    tibble::tibble(Entity = .e, Measure = .m, Naive = .n, Rule = .r)
  }
  out_ <- list()

  f_ <- .facts$ORG
  if (!is.null(f_) && nrow(f_) > 0L) {
    out_ <- c(out_, list(
      say_("ORG", "Distinct spellings vs parties",
           mean(f_$NaiveSpans), mean(f_$NaiveParties)),
      say_("ORG", "Counterparties per contract",
           mean(f_$NaiveCounter), mean(f_$NCounter))
    ))
  }

  f_ <- .facts$GPE
  if (!is.null(f_) && nrow(f_) > 0L) {
    out_ <- c(out_, list(
      say_("GPE", "Distinct states per contract",
           mean(f_$NaiveStates), mean(f_$RuleStates)),
      say_("GPE", "Distinct countries per contract",
           mean(f_$NaiveCountries), mean(f_$RuleCountries))
    ))
  }

  f_ <- .facts$DATE
  if (!is.null(f_) && nrow(f_) > 0L) {
    out_ <- c(out_, list(
      say_("DATE", "Mean duration, years",
           mean(f_$NaiveYears, na.rm = TRUE), mean(f_$DurationYears, na.rm = TRUE)),
      say_("DATE", "Standard deviation, years",
           stats::sd(f_$NaiveYears, na.rm = TRUE), stats::sd(f_$DurationYears, na.rm = TRUE))
    ))
  }

  f_ <- .facts$MONEY
  if (!is.null(f_) && nrow(f_) > 0L) {
    out_ <- c(out_, list(
      say_("MONEY", "Amounts per contract",
           mean(f_$NAmountsRaw), mean(f_$NAmounts)),
      # BOTH OVER THE SAME CONTRACTS, and this is the error 04B4's own prose warns about. Computing
      # each median over its own non-missing rows compares a set INCLUDING the contracts whose every
      # figure was boilerplate against one excluding them, and then reports the filters as having
      # RAISED contract value. Restricting to where the rule kept an amount is what makes it a
      # comparison at all.
      say_("MONEY", "Median largest USD amount",
           stats::median(f_$NaiveMaxUSD[!is.na(f_$MoneyMaxUSD)], na.rm = TRUE),
           stats::median(f_$MoneyMaxUSD, na.rm = TRUE))
    ))
  }

  f_ <- .facts$REDACT
  if (!is.null(f_) && nrow(f_) > 0L) {
    out_ <- c(out_, list(
      say_("REDACT", "Markers per contract",
           mean(f_$NRedact), mean(f_$NBracketed)),
      say_("REDACT", "Contracts carrying one",
           mean(f_$NRedact > 0L), mean(f_$NBracketed > 0L))
    ))
  }

  purrr::list_rbind(out_) |>
    dplyr::mutate(Change = .data$Rule / dplyr::if_else(.data$Naive == 0, NA_real_, .data$Naive) - 1)
}


#' The rule against the naive definition, reported
#' @param .tab Tibble from apl_table_naive().
#' @param .limit Integer or NULL.
#' @return Invisibly .tab.
apl_report_naive <- function(.tab, .limit = NULL) {
  if (FALSE) {
    .tab   <- tab_naive
    .limit <- .lP$Params$Limit
  }

  cli::cli_h2("The rule against the naive definition, at corpus scale")

  if (nrow(.tab) == 0L) {
    cli::cli_alert_info("No pass has written a file, so there is nothing to compare.")
    return(invisible(.tab))
  }

  .tab |>
    dplyr::mutate(
      dplyr::across(c(Naive, Rule), \(.x) tbl_num(.x)),
      Change = tbl_pct_safe(.data$Change)
    ) |>
    tbl_say(.title = "One row per measure, over every document written so far")

  cli::cli_alert_info(
    "NAIVE IS WHAT A READER GETS WITHOUT THE RULE and RULE is what this pipeline gives. Every pair \\
     comes from the SAME collapse function 04B reports from, so a corpus figure and a sample figure \\
     are one query on different data rather than two implementations that could disagree."
  )
  cli::cli_alert_info(
    "READ THESE AGAINST 04B'S OWN HEADLINE TABLES. A corpus rate far from the sample rate is either \\
     a finding about representativeness or a defect in this pass, and the two are told apart by \\
     reading spans rather than by reading this table."
  )
  cli::cli_alert_info(
    "DATE'S SECOND ROW IS THE ONE TO SHOW A REFEREE. A duration capped at thirty years cannot have a \\
     standard deviation above fifteen, and the naive column is where a published figure of 40.78 \\
     comes from."
  )

  if (!is.null(.limit)) {
    cli::cli_alert_warning(
      "THIS IS A LIMITED RUN. Nothing above describes the corpus; it describes the first \\
       {format(.limit, big.mark = ',')} documents of each population."
    )
  }
  invisible(.tab)
}


#' Everything this document did, in one table
#' @param .out Tibble from apl_table_outputs().
#' @param .hashes Named character of policy hashes.
#' @param .index Named list of tibbles from apl_corpus_index().
#' @return Invisibly the table.
apl_report_headline <- function(.out, .hashes, .index) {
  if (FALSE) {
    .out    <- tab_out
    .hashes <- .lP$Params$Hash
    .index  <- tab_index
  }

  cli::cli_h2("The five passes in one table")

  tibble::tibble(
    Pass       = names(.hashes),
    Hash       = unname(.hashes),
    Population = purrr::map_chr(names(.hashes),
                                \(.p) format(nrow(.index[[.p]]), big.mark = ",")),
    Files      = purrr::map_chr(names(.hashes),
                                \(.p) paste(apl_pass_spec(.p)$Writes, collapse = ", ")),
    Rows       = purrr::map_chr(names(.hashes), function(.p) {
      format(sum(.out$Rows[.out$Pass == .p]), big.mark = ",")
    })
  ) |>
    tbl_say(.title = "One row per pass")

  cli::cli_alert_info(
    "THE HASHES ARE THE ONE THING TO RECORD. Each names its pass's chunk directory and stands for \\
     every rule parameter, every extractor spec hash and every dependency that pass used -- so two \\
     releases carrying the same hash were produced by the same pipeline and two carrying different \\
     ones were not."
  )
  invisible(.out)
}


