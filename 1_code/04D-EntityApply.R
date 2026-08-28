# 04D-EntityApply: the five rules at corpus scale ------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# 04A measured which family to trust, 04B built and scored a rule per entity, 04C extracted the
# corpus. This decides nothing. It reads the rules as parameters, applies them one ENTITY AT A TIME,
# and writes the same eight long files 04B released with the corpus in them.
#
# THE UNIT OF WORK IS A CHUNK OF DOCUMENTS, AND THE WORKER READS ITS OWN
# Every 04B chain is a vectorised dplyr pipeline over a whole table, and every grouping and join key
# in all of them carries DocID. So a chunk is not an approximation of per-document work: it is the
# same arithmetic, done once instead of twenty-five thousand times. Validation proves that rather
# than asserting it.
#
# A worker is handed a list of DocIDs and reads its own anchors and its own spans. Nothing
# span-grain crosses a process boundary in either direction; the parent splits a character vector
# and collects a row of numbers. That inversion is the whole design. The alternative -- the parent
# reads and ships -- makes the parent the bottleneck and the serialiser the cost, whatever the unit
# of work happens to be.
#
# RESUMPTION IS BY DOCUMENT, NOT BY POSITION
# A pass asks which documents the ledger has and which are already written; the difference is the
# work. Nothing is named for a position, so a chunk size changed between renders costs nothing and a
# population that grew simply adds to the queue.
#
# A CHUNK COMMITS AS A DIRECTORY. Everything it produces goes into a hidden name which is then
# renamed into place. A rename within one filesystem is atomic, so a killed process leaves a dotfile
# no glob sees and no reader counts.
#
# _done.parquet NAMES EVERY DOCUMENT PROCESSED, not every document that produced a row. Seven of the
# eight releases carry a row only where the extractor found something -- redact_spans reaches 19% of
# documents -- so resuming on the outputs would reprocess the other 81% on every render, forever.
#
# A FAILING DOCUMENT COSTS ITS NEIGHBOURS NOTHING. A chunk that throws is retried in groups of
# RetryFloor documents, and a group that throws again one document at a time. The isolation a
# per-document unit would buy is kept as an exception path and paid for only when something breaks.
#
# FIVE HASHES. ORG folds in lexnlp's spec hashes; GPE folds in matcon's AND ORG's, because it reads
# ORG's mention file; DATE, MONEY and REDACT fold in matcon's. REDACT reads matcon's money spans to
# find the ones the filer emptied, but "emptied" is a moneyregex PATTERN NAME rather than a filtered
# result, so it depends on the extractor and not on 04B4. The chunk size and the worker count are in
# no hash: nothing depends on how the remaining work is divided.
#
# THE EIGHT LONG FILES ARE THE RELEASE AND THE COLLAPSE IS NOT ONE. _done.parquet and
# _facts.parquet are underscore-prefixed because they are bookkeeping: the first says what was
# processed, the second carries the document-grain collapse this document reports from. The EXPORT
# calls ent_party_facts(), geo_collapse(), dte_collapse(), mny_collapse() and red_collapse() over
# the long files itself and reads neither.
#
# NOTHING OPENS A DOCUMENT. Every cue window was cut at extraction and stored beside its span.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .paths  <- .lP$Input
  .params <- .lP$Params
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


#' What one pass is, as a list
#'
#' EVERY OTHER FUNCTION ASKS THIS RATHER THAN CARRYING ITS OWN COPY. A pass's family, its entities
#' and its file stems are stated once, so adding a sixth pass is a row in one table.
#'
#' @param .pass Character. ORG, GPE, DATE, MONEY or REDACT.
#' @return A list: Pass, Family, Needs, Writes, Depends.
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


# 2. Policy --------------------------------------------------------------------------------------------------------------

#' One hash per pass, standing for every choice that pass makes
#'
#' THE OUTPUT DIRECTORY IS NAMED FOR IT, so a changed rule cannot be served output produced by a rule
#' that no longer exists. Everything already written is worth an hour on a corpus this size, and
#' worth nothing at all if it can answer for the wrong policy.
#'
#' THE EXTRACTOR IS PART OF THE POLICY. Every span this pass reads was produced by a model whose spec
#' hash 04C recorded; re-extracting with a changed gazetteer or a changed money grammar leaves the
#' parameters byte-identical while the input underneath them is different.
#'
#' AND SO IS THE PASS IT DEPENDS ON. GPE reads ORG's mention file, so geography built on parties that
#' have since been regrouped is exactly what this hash exists to prevent being served.
#'
#' THE SCHEDULE IS NOT IN IT. Chunk size and worker count decide how long a pass takes, not what it
#' answers, and Validation establishes that rather than assuming it.
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
    ~Pass,    ~Setting,         ~Value,                                        ~From,
    "ORG",    "family",         "lexnlp",                                      "04A",
    "ORG",    "key",            .params$Rule$Org$Key,                          "04B1",
    "ORG",    "merge",          as.character(.params$Rule$Org$MergeFragments), "04B1",
    "ORG",    "window",         format(.params$Spec$Org$Par, big.mark = ","),  "04B1",
    "ORG",    "tail share",     format(.params$Spec$Org$TailShare),            "04B1",
    "GPE",    "family",         "matcon",                                      "04B2",
    "GPE",    "reach",          format(.params$Spec$Geo$Reach),                "04B2",
    "DATE",   "start",          .params$Spec$Date$Start,                       "04B3",
    "DATE",   "end",            .params$Spec$Date$End,                         "04B3",
    "DATE",   "cap, years",     format(.params$Spec$Date$CapYears),            "04B3",
    "DATE",   "cue window",     format(.params$CueWin$Date),                   "04B3",
    "MONEY",  "filter",         .params$Spec$Money$Filter,                     "04B4",
    "MONEY",  "cue window",     format(.params$Spec$Money$CueWin),             "04B4",
    "REDACT", "tolerance",      format(.params$Spec$Redact$Tol),               "04B5",
    "SCHED",  "chunk size",     format(.params$ChunkSize, big.mark = ","),     "04D",
    "SCHED",  "workers",        format(.params$Workers),                       "04D",
    "SCHED",  "duckdb threads", format(.params$DuckThreads),                   "04D",
    "SCHED",  "retry floor",    format(.params$RetryFloor, big.mark = ","),    "04D",
    "SCHED",  "limit",          if (is.null(.params$Limit)) "none" else
                                  format(.params$Limit, big.mark = ","),       "04D"
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
                             \(.p) dplyr::coalesce(apl_pass_spec(.pass = .p)$Depends, "-"))
  ) |>
    tbl_say(.title = "One hash per pass, and what each folds in")

  cli::cli_alert_info(
    "FIVE HASHES AND NOT ONE. Each covers its own rule parameters, its own family's extractor spec \\
     hashes, and the hash of any pass it reads -- so a changed date cap re-runs DATE alone, and a \\
     changed party rule re-runs ORG and GPE because GPE reads ORG's file."
  )
  cli::cli_alert_info(
    "THE SCHEDULE SETTINGS ARE IN NO HASH. They decide how long a pass takes and not what it \\
     answers; Validation establishes that by running a rule over the same documents cut two ways. \\
     Hashing them would mean re-running a corpus to change a worker count."
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


# 3. The population and the work -----------------------------------------------------------------------------------------

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
#' ORDERED BY DocID, AND EVERY LATER STEP DEPENDS ON IT. Chunks are contiguous runs of this order, so
#' a chunk's spans are a contiguous band of the store and a chunk's dependency is one or two files
#' rather than a whole release.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .pass Character. Which pass.
#' @param .limit Integer or NULL. Take the first N documents, for a test run.
#' @return Tibble: DocID.
apl_corpus_index <- function(.dir_store, .pass, .limit = NULL) {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .pass      <- "GPE"
    .limit     <- 25000L
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


#' The chunks a pass has committed
#'
#' A CHUNK IS A DIRECTORY AND ITS PRESENCE IS ITS COMPLETENESS. Everything a chunk produces goes into
#' a hidden working directory which is then renamed into place, so a killed process leaves a name
#' beginning with a dot that this does not match. There is no state to read and nothing that can be
#' half true.
#'
#' @param .dir Directory named for a pass's policy hash.
#' @return Character vector of directory paths, possibly empty.
apl_chunk_dirs <- function(.dir) {
  if (FALSE) .dir <- .lP$Output$Store[["ORG"]]

  if (!fs::dir_exists(.dir)) return(character(0))
  fs::dir_ls(.dir, regexp = "/chunk-[^/]+$", type = "directory", recurse = FALSE)
}


#' Which documents a pass has already processed
#'
#' THE DONE FILE AND NOT THE OUTPUT'S DocID COLUMN. Seven of the eight releases carry a row only
#' where the extractor found something -- redact_spans reaches 19% of documents -- so resuming on
#' what the outputs contain would reprocess the other 81% on every render, forever. This names every
#' document a chunk PROCESSED, whatever that document produced.
#'
#' @param .dir Directory named for a pass's policy hash.
#' @return Character vector of DocIDs, possibly empty.
apl_done_ids <- function(.dir) {
  if (FALSE) .dir <- .lP$Output$Store[["ORG"]]

  files_ <- fs::path(apl_chunk_dirs(.dir = .dir), "_done.parquet")
  files_ <- files_[fs::file_exists(files_)]
  if (length(files_) == 0L) return(character(0))

  arrow::open_dataset(sources = files_) |>
    dplyr::select("DocID") |>
    dplyr::collect() |>
    dplyr::pull("DocID") |>
    unique()
}


#' Cut a pass's outstanding work into contiguous chunks
#'
#' CONTIGUOUS IN DocID ORDER, WHICH IS WHAT MAKES THE READ CHEAP. ent_load_entity() bounds its SQL on
#' the range its documents span, so a chunk of a sorted list touches one band of the store; a random
#' partition of the same documents would bound on nearly the whole table and read all of it every
#' time.
#'
#' THE INDEX TRAVELS INSIDE THE ELEMENT. mirai_map() maps elements to the FIRST argument only, so a
#' chunk that needs to know which chunk it is has to carry that itself. It is used for a filename and
#' for nothing else: identity lives in _done.parquet.
#'
#' @param .todo Character. Documents not yet written, in DocID order.
#' @param .size Integer. Documents per chunk.
#' @return An unnamed list of lists, each with Index and DocIDs.
apl_chunks <- function(.todo, .size) {
  if (FALSE) {
    .todo <- todo_
    .size <- 25000L
  }

  if (length(.todo) == 0L) return(list())
  parts_ <- unname(split(.todo, ceiling(seq_along(.todo) / as.integer(.size))))
  purrr::imap(parts_, \(.ids, .i) list(Index = as.integer(.i), DocIDs = .ids))
}


# 4. Reading another pass's release --------------------------------------------------------------------------------------

#' The DocID range of every file the dependency wrote
#'
#' BUILT ONCE IN THE PARENT AND HANDED TO EVERY CHUNK, and the alternative was catastrophic. Without
#' it a chunk opened the WHOLE dependency -- at corpus scale about fifty files and twenty million
#' mention rows -- and trusted arrow to push a range filter down far enough to skip most of it. It
#' does not do that reliably on string keys, and every worker then materialised a large share of the
#' entire ORG release.
#'
#' A CHUNK OVERLAPS ONE OR TWO FILES, NEVER FIFTY. Both passes cut sorted DocID lists, so a chunk's
#' [lo, hi] meets a contiguous run of the dependency's files. Knowing each file's range turns the read
#' from a scan of everything into opening exactly what is needed.
#'
#' ONLY THE DocID COLUMN IS READ, so min() and max() over each file is seconds for the whole release,
#' once per pass rather than once per chunk.
#'
#' @param .dir Directory holding the dependency's chunk directories.
#' @param .stem Character. The file stem.
#' @return Tibble: File, Lo, Hi. Empty where the directory holds nothing.
apl_dep_index <- function(.dir, .stem) {
  if (FALSE) {
    .dir  <- .lP$Output$Store[["ORG"]]
    .stem <- "org_mentions"
  }

  if (is.null(.dir)) return(tibble::tibble())
  files_ <- fs::path(apl_chunk_dirs(.dir = .dir), paste0(.stem, ".parquet"))
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


#' One chunk's worth of another pass's output
#'
#' AN EMPTY DIRECTORY AND AN EMPTY RESULT ARE OPPOSITE FINDINGS. A dependency that has committed
#' nothing means the pass it depends on has not run: the chain would then attach every place to
#' nothing and write a corpus in which no place sits near a party, which is indistinguishable in the
#' output from a corpus where none does. A filter that returns no rows is ordinary.
#'
#' THE RANGE FILTER PUSHES INTO THE READER AND THE MEMBERSHIP TEST DOES NOT, so the order matters:
#' bounding first skips whole files, and the exact test then runs on what survives.
#'
#' @param .stem Character. The file stem to read.
#' @param .doc_ids Character. This chunk's documents.
#' @param .index Tibble from apl_dep_index().
#' @return Tibble, with the dependency's own columns, possibly with no rows.
apl_read_dep <- function(.stem, .doc_ids, .index) {
  if (FALSE) {
    .stem    <- "org_mentions"
    .doc_ids <- todo_[1:25000]
    .index   <- tab_dep_org
  }

  if (is.null(.index) || nrow(.index) == 0L) {
    cli::cli_abort(c(
      "The pass that writes {(.stem)} has committed nothing.",
      "i" = "Running this one anyway would attach every span to nothing and record that as a
             finding about the corpus."
    ))
  }

  lo_ <- min(.doc_ids)
  hi_ <- max(.doc_ids)

  # OVERLAP AND NOT CONTAINMENT. Two intervals meet when each starts before the other ends, and a
  # chunk of one population straddles a file boundary of the other whenever the two were cut on
  # different todo lists -- which they are, because each pass resumes on its own done set.
  files_ <- .index$File[.index$Lo <= hi_ & lo_ <= .index$Hi]

  # AN INDEX THAT MATCHES NOTHING IS NOT AN EMPTY DIRECTORY. The dependency simply never reached
  # these documents, which is ordinary, and the chain still needs a frame carrying its COLUMNS AND
  # THEIR TYPES -- geo_attach() binds mention offsets to place offsets and an untyped frame would
  # fail there. arrow refuses filter(FALSE) because it wants a column expression rather than a
  # literal, so the predicate is one no DocID satisfies; zone maps skip every row group.
  if (length(files_) == 0L) {
    return(
      arrow::open_dataset(sources = .index$File[[1L]]) |>
        dplyr::filter(.data$DocID == "") |>
        dplyr::collect()
    )
  }

  arrow::open_dataset(sources = files_) |>
    dplyr::filter(.data$DocID >= lo_, .data$DocID <= hi_) |>
    dplyr::collect() |>
    dplyr::filter(.data$DocID %in% .doc_ids)
}


# 5. One chunk, end to end -----------------------------------------------------------------------------------------------

#' Every input one pass needs, for one list of documents
#'
#' THE WORKER READS ITS OWN, AND THAT IS THE POINT. Anchors are a fifth of a second for twenty-five
#' thousand documents and spans are one bounded query; shipping either from the parent would put the
#' whole corpus through a serialiser to save a read that costs less than the serialising.
#'
#' THE LOADERS ARE 04B'S OWN. ent_load_org() adds the two candidate keys the reduction reads;
#' dte_describe() cuts the cue window and computes the gap to the filing date. Nothing is
#' reimplemented here.
#'
#' @param .pass Character. Which pass.
#' @param .doc_ids Character. This chunk's documents.
#' @param .paths The runbook's .lP$Input.
#' @param .params The runbook's .lP$Params.
#' @param .dep_index Tibble from apl_dep_index(). GPE only.
#' @return A named list: Keys, Lens, and one element per input the chain reads.
apl_chunk_load <- function(.pass, .doc_ids, .paths, .params, .dep_index = NULL) {
  if (FALSE) {
    .pass      <- "GPE"
    .doc_ids   <- todo_[1:25000]
    .paths     <- .lP$Input
    .params    <- .lP$Params
    .dep_index <- tab_dep_org
  }

  # THE REGISTER AND NOTHING ELSE. No release function reads Class: it appears only in the collapses
  # and in reports, both of which join it at their own time. So this pass needs no label release, no
  # partial-release check and no dependency on 03F at all, which is three failure modes removed.
  keys_ <- ent_corpus_keys(
    .path_register = .paths$Register,
    .path_release  = NA_character_,
    .doc_ids       = .doc_ids,
    .quiet         = TRUE
  )
  lens_ <- keys_ |>
    dplyr::transmute(DocID, DocLen = as.integer(.data$nChars)) |>
    dplyr::filter(!is.na(.data$DocLen), .data$DocLen > 0L)

  rest_ <- switch(
    .pass,
    ORG = list(
      Spans  = ent_load_org(.dir_store = .paths$Store, .lens = lens_, .family = "lexnlp",
                            .extras = .params$Extras$ORG, .quiet = TRUE),
      Filers = ent_filer_keys(.path_register = .paths$Register, .doc_ids = keys_$DocID,
                              .quiet = TRUE)
    ),
    GPE = list(
      Spans    = ent_load_entity(.dir_store = .paths$Store, .family = "matcon", .entity = "GPE",
                                 .lens = lens_, .extras = ent_extras("matcon", "GPE"),
                                 .quiet = TRUE),
      Law      = ent_load_entity(.dir_store = .paths$Store, .family = "matcon", .entity = "LAW",
                                 .lens = lens_, .extras = ent_extras("matcon", "LAW"),
                                 .quiet = TRUE),
      Mentions = apl_read_dep(.stem = "org_mentions", .doc_ids = keys_$DocID, .index = .dep_index)
    ),
    DATE = list(
      Spans = dte_describe(
        .dates = dplyr::filter(
          dte_load(.dir_store = .paths$Store, .lens = lens_, .family = "matcon", .quiet = TRUE),
          .data$Parsed
        ),
        .keys = keys_, .win = .params$CueWin$Date
      ),
      Terms = dte_load_terms(.dir_store = .paths$Store, .lens = lens_, .quiet = TRUE)
    ),
    MONEY = list(
      Spans = mny_load(.dir_store = .paths$Store, .lens = lens_, .family = "matcon", .quiet = TRUE)
    ),
    REDACT = list(
      Spans = red_load(.dir_store = .paths$Store, .lens = lens_, .family = "matcon", .quiet = TRUE),
      Money = mny_load(.dir_store = .paths$Store, .lens = lens_, .family = "matcon", .quiet = TRUE),
      Words = red_words(.keys = keys_, .path_text = NULL, .quiet = TRUE)
    )
  )

  c(list(Keys = keys_, Lens = lens_), rest_)
}


#' Apply one entity's rule to one chunk's inputs
#'
#' EVERY CALL HERE IS 04B'S OWN, and this function chooses nothing. It hands a chunk's tables to the
#' entry point 04B defined, takes the release out, and takes the document-grain collapse.
#'
#' THE COLLAPSE IS TAKEN, NOT RECOMPUTED. dte_apply(), mny_apply() and red_apply() each return their
#' collapse beside the release because 04B reports from it; discarding it here and rebuilding it in
#' the Results section would be the same arithmetic twice, with two chances to disagree. ORG and GPE
#' have no collapse inside their entry point, so theirs is called explicitly -- from the same
#' functions 04B1 and 04B2 report from.
#'
#' REDACT USES THE MONEY SPANS IT WAS GIVEN, not MONEY's released file. It needs the ones the filer
#' emptied, and that is a moneyregex pattern name rather than a filtered result.
#'
#' @param .pass Character. Which pass.
#' @param .win List from apl_chunk_load().
#' @param .params The runbook's .lP$Params.
#' @param .geo List: Lookup and Cand. GPE only.
#' @return A list: Files, a named list of release tibbles; Facts, one row per document.
apl_chunk_apply <- function(.pass, .win, .params, .geo = NULL) {
  if (FALSE) {
    .pass   <- "ORG"
    .win    <- win_
    .params <- .lP$Params
    .geo    <- geo_once
  }

  keys_ <- .win$Keys
  lens_ <- .win$Lens

  if (.pass == "ORG") {
    res_ <- ent_apply(
      .spans = .win$Spans, .keys = keys_, .lens = lens_,
      .rule  = .params$Rule$Org, .spec = .params$Spec$Org, .filers = .win$Filers
    )
    return(list(
      Files = list(org_mentions = res_$Release),
      Facts = ent_doc_facts(.release = res_$Release, .lens = lens_, .spec = .params$Spec$Org)
    ))
  }

  if (.pass == "GPE") {
    res_ <- geo_apply(
      .spans = .win$Spans, .law = .win$Law, .mentions = .win$Mentions,
      .geo   = .geo, .spec = .params$Spec$Geo
    )
    return(list(
      Files = list(places_geo = res_$Release, law_clauses = res_$Law),
      Facts = geo_doc_facts(
        .release = res_$Release,
        .party   = ent_party_facts(.release = .win$Mentions),
        .keys    = keys_
      )
    ))
  }

  if (.pass == "DATE") {
    res_ <- dte_apply(
      .dates = .win$Spans, .terms = .win$Terms, .keys = keys_, .spec = .params$Spec$Date
    )
    return(list(
      Files = list(date_spans = res_$Dates, term_spans = res_$Terms),
      Facts = res_$Duration
    ))
  }

  if (.pass == "MONEY") {
    res_ <- mny_apply(.money = .win$Spans, .keys = keys_, .spec = .params$Spec$Money)
    return(list(Files = list(money_spans = res_$Release), Facts = res_$Agg))
  }

  if (.pass == "REDACT") {
    res_ <- red_apply(
      .marks = .win$Spans, .money = .win$Money, .words = .win$Words,
      .keys  = keys_, .spec = .params$Spec$Redact
    )
    return(list(Files = list(redact_spans = res_$Release), Facts = res_$Counts))
  }

  cli::cli_abort("{(.pass)} is not one of the five passes.")
}


#' Load and apply one list of documents
#'
#' THE SMALLEST THING THAT CAN FAIL, and the unit the retry splits. Everything around it is
#' bookkeeping; this is the pass.
#'
#' @param .pass Character. Which pass.
#' @param .doc_ids Character. The documents to apply.
#' @param .paths The runbook's .lP$Input.
#' @param .params The runbook's .lP$Params.
#' @param .geo List: the gazetteer. GPE only.
#' @param .dep_index Tibble from apl_dep_index(). GPE only.
#' @return A list: Files, Facts.
apl_apply_ids <- function(.pass, .doc_ids, .paths, .params, .geo = NULL, .dep_index = NULL) {
  if (FALSE) {
    .pass    <- "MONEY"
    .doc_ids <- todo_[1:500]
    .paths   <- .lP$Input
    .params  <- .lP$Params
  }

  win_ <- apl_chunk_load(
    .pass = .pass, .doc_ids = .doc_ids, .paths = .paths, .params = .params,
    .dep_index = .dep_index
  )
  apl_chunk_apply(.pass = .pass, .win = win_, .params = .params, .geo = .geo)
}


#' Apply a list of documents, splitting it where it fails
#'
#' TWO RETRY LEVELS AND A FLOOR OF ONE. A chunk that throws is retried in groups of RetryFloor, and a
#' group that throws again one document at a time -- so a single unparseable contract costs itself
#' and nothing else, and a healthy corpus never pays for the machinery.
#'
#' THE FAILURE IS A VALUE. try() returns its condition rather than raising, so a bad document ends up
#' in Bad and out of _done, and is retried on the next render instead of aborting this one.
#'
#' @param .pass Character. Which pass.
#' @param .doc_ids Character. The documents to apply.
#' @param .paths The runbook's .lP$Input.
#' @param .params The runbook's .lP$Params.
#' @param .geo List: the gazetteer. GPE only.
#' @param .dep_index Tibble from apl_dep_index(). GPE only.
#' @return A list: Files, Facts, Ok, Bad.
apl_apply_safe <- function(.pass, .doc_ids, .paths, .params, .geo = NULL, .dep_index = NULL) {
  if (FALSE) {
    .pass    <- "MONEY"
    .doc_ids <- todo_[1:25000]
    .paths   <- .lP$Input
    .params  <- .lP$Params
  }

  stems_ <- apl_pass_spec(.pass = .pass)$Writes
  none_  <- purrr::set_names(purrr::map(stems_, \(.s) tibble::tibble()), stems_)

  if (length(.doc_ids) == 0L) {
    return(list(Files = none_, Facts = tibble::tibble(),
                Ok = character(0), Bad = character(0)))
  }

  res_ <- try(
    apl_apply_ids(.pass = .pass, .doc_ids = .doc_ids, .paths = .paths, .params = .params,
                  .geo = .geo, .dep_index = .dep_index),
    silent = TRUE
  )
  if (!inherits(res_, "try-error")) {
    return(list(Files = res_$Files, Facts = res_$Facts, Ok = .doc_ids, Bad = character(0)))
  }

  # ONE DOCUMENT THAT STILL THROWS IS THE FLOOR. Splitting further is impossible and retrying it is
  # not useful, so it is named and left out of _done.
  if (length(.doc_ids) == 1L) {
    return(list(Files = none_, Facts = tibble::tibble(),
                Ok = character(0), Bad = .doc_ids))
  }

  size_  <- if (length(.doc_ids) > .params$RetryFloor) as.integer(.params$RetryFloor) else 1L
  parts_ <- unname(split(.doc_ids, ceiling(seq_along(.doc_ids) / size_)))

  sub_ <- purrr::map(parts_, \(.p) apl_apply_safe(
    .pass = .pass, .doc_ids = .p, .paths = .paths, .params = .params,
    .geo = .geo, .dep_index = .dep_index
  ))

  list(
    Files = purrr::set_names(
      purrr::map(stems_, \(.s) purrr::list_rbind(purrr::map(sub_, \(.r) .r$Files[[.s]]))), stems_
    ),
    Facts = purrr::list_rbind(purrr::map(sub_, \(.r) .r$Facts)),
    Ok    = unlist(purrr::map(sub_, \(.r) .r$Ok), use.names = FALSE),
    Bad   = unlist(purrr::map(sub_, \(.r) .r$Bad), use.names = FALSE)
  )
}


#' One chunk: read it, apply it, commit it
#'
#' THIS IS WHAT A WORKER RUNS, and it takes a list of DocIDs and a few paths. Nothing span-grain
#' crosses the process boundary in either direction: the worker opens its own database and returns
#' one row of numbers.
#'
#' THE DIRECTORY IS RENAMED INTO PLACE. Outputs, _done and _facts are written into a hidden name and
#' the whole directory is moved, so a process killed mid-write leaves a dotfile that apl_chunk_dirs()
#' does not match and no reader counts.
#'
#' _done CARRIES ONLY THE DOCUMENTS THAT SUCCEEDED, so a failure is retried on the next render rather
#' than recorded as finished -- and every document that succeeded, whether or not it produced a row.
#'
#' @param .chunk A list with Index and DocIDs, from apl_chunks().
#' @param .pass Character. Which pass.
#' @param .dir_out Directory named for this pass's policy hash.
#' @param .run Character. This render's run identifier, for the directory name.
#' @param .paths The runbook's .lP$Input.
#' @param .params The runbook's .lP$Params.
#' @param .geo List: the gazetteer. GPE only.
#' @param .dep_index Tibble from apl_dep_index(). GPE only.
#' @return Tibble: one row describing what this chunk did.
apl_chunk_run <- function(.chunk, .pass, .dir_out, .run, .paths, .params,
                          .geo = NULL, .dep_index = NULL) {
  if (FALSE) {
    .chunk   <- chunks_[[1L]]
    .pass    <- "MONEY"
    .dir_out <- .lP$Output$Store[["MONEY"]]
    .run     <- "20260827T210000"
    .paths   <- .lP$Input
    .params  <- .lP$Params
  }

  t0_  <- Sys.time()
  res_ <- apl_apply_safe(
    .pass = .pass, .doc_ids = .chunk$DocIDs, .paths = .paths, .params = .params,
    .geo = .geo, .dep_index = .dep_index
  )
  secs_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))

  id_   <- paste0(.run, "-", formatC(.chunk$Index, width = 4L, flag = "0"))
  work_ <- fs::path(.dir_out, paste0(".chunk-", id_, ".part"))
  if (fs::dir_exists(work_)) fs::dir_delete(work_)
  fs::dir_create(work_)

  # A ZERO-COLUMN FRAME IS NOT WRITTEN, and the distinction matters. A chain that ran and found
  # nothing returns a zero-ROW frame carrying its full schema, which is written and unions cleanly
  # with every other chunk. A chunk whose every document failed has no schema at all, and writing
  # that would put a file in the release that open_dataset() cannot reconcile with its siblings.
  purrr::iwalk(res_$Files, function(.t, .n) {
    if (ncol(.t) > 0L) arrow::write_parquet(.t, fs::path(work_, paste0(.n, ".parquet")))
  })
  arrow::write_parquet(tibble::tibble(DocID = res_$Ok), fs::path(work_, "_done.parquet"))
  if (ncol(res_$Facts) > 0L) {
    arrow::write_parquet(res_$Facts, fs::path(work_, "_facts.parquet"))
  }
  fs::file_move(work_, fs::path(.dir_out, paste0("chunk-", id_)))

  tibble::tibble(
    Pass   = .pass,
    Chunk  = id_,
    Docs   = length(.chunk$DocIDs),
    Failed = length(res_$Bad),
    Rows   = as.integer(sum(purrr::map_int(res_$Files, nrow))),
    Secs   = secs_
  )
}


# 6. One pass, end to end ------------------------------------------------------------------------------------------------

#' Run one pass over whatever is not already written
#'
#' THE PARENT SPLITS A CHARACTER VECTOR AND COLLECTS A ROW OF NUMBERS. Everything else happens in a
#' worker, which is why a dozen of them do not queue behind one R process holding the corpus.
#'
#' EVERY DAEMON SOURCES THE LIBRARIES BY PATH. A closure does not cross a process boundary with its
#' environment, so a worker handed a function that calls ent_apply() finds no ent_apply(). The
#' gazetteer is built once per daemon rather than once per chunk, because 181,810 rows sent fifty
#' times would cost more than the rule.
#'
#' THE THREAD SETTING IS AN OPTION IN THE DAEMON. A dozen workers each opening a DuckDB connection
#' that helps itself to every core is 288 threads on a 24-core machine, and the contention costs more
#' than the read.
#'
#' @param .pass Character. Which pass.
#' @param .todo Character. Documents not yet written, in DocID order.
#' @param .dir_out Directory named for this pass's policy hash.
#' @param .paths The runbook's .lP$Input.
#' @param .params The runbook's .lP$Params.
#' @param .geo List: the gazetteer, for the serial path. A daemon builds its own.
#' @param .dep_index Tibble from apl_dep_index(). GPE only.
#' @param .path_fun Character. Library files every daemon sources, in order.
#' @return Tibble: one row per chunk.
apl_pass_run <- function(.pass, .todo, .dir_out, .paths, .params,
                         .geo = NULL, .dep_index = NULL, .path_fun = character()) {
  if (FALSE) {
    .pass    <- "MONEY"
    .todo    <- todo_
    .dir_out <- .lP$Output$Store[["MONEY"]]
    .paths   <- .lP$Input
    .params  <- .lP$Params
  }

  fs::dir_create(.dir_out)
  cli::cli_h3("{(.pass)}")

  if (length(.todo) == 0L) {
    cli::cli_alert_success("Nothing outstanding.")
    return(tibble::tibble())
  }

  chunks_ <- apl_chunks(.todo = .todo, .size = .params$ChunkSize)
  run_    <- format(Sys.time(), "%Y%m%dT%H%M%S")
  n_work_ <- max(as.integer(.params$Workers), 1L)

  cli::cli_alert_info(
    "{format(length(.todo), big.mark = ',')} outstanding, in {length(chunks_)} \\
     {cli::qty(length(chunks_))}chunk{?s} of {format(.params$ChunkSize, big.mark = ',')}, on \\
     {n_work_} {cli::qty(n_work_)}worker{?s}."
  )

  if (n_work_ > 1L) {
    mirai::daemons(n_work_)
    on.exit(mirai::daemons(0L), add = TRUE)   # SAFE ONLY INSIDE A FUNCTION BODY, which this is

    # SENT ONCE PER DAEMON AND NOT ONCE PER CHUNK. everywhere() evaluates its expression in an
    # environment carrying .args, whose parent is the daemon's global -- so a plain assignment binds
    # THERE and is gone when the call returns. assign() into globalenv() is what makes it survive.
    mirai::everywhere(
      {
        purrr::walk(.files, \(.f) source(.f, encoding = "UTF-8"))
        options(mc.duckdb_threads = .threads)
        assign(
          x     = "apl_geo_once",
          value = if (is.null(.lookup)) NULL else list(
            Lookup = geo_lookup(.path_lookup = .lookup),
            Cand   = geo_candidates(.path_lookup = .lookup)
          ),
          envir = globalenv()
        )
      },
      .args = list(
        .files   = .path_fun,
        .threads = .params$DuckThreads,
        .lookup  = if (.pass == "GPE") .paths$Lookup else NULL
      )
    )
  }

  t0_ <- Sys.time()

  out_ <- if (n_work_ <= 1L) {
    purrr::map(chunks_, \(.c) apl_chunk_run(
      .chunk = .c, .pass = .pass, .dir_out = .dir_out, .run = run_, .paths = .paths,
      .params = .params, .geo = .geo, .dep_index = .dep_index
    ), .progress = TRUE)
  } else {
    # mirai_map() MAPS ELEMENTS TO THE FIRST ARGUMENT ONLY, so everything constant travels in .args
    # and the chunk index travels inside the element.
    mirai::mirai_map(
      .x = chunks_,
      .f = function(.chunk, .pass, .dir_out, .run, .paths, .params, .dep_index) {
        apl_chunk_run(
          .chunk = .chunk, .pass = .pass, .dir_out = .dir_out, .run = .run, .paths = .paths,
          .params = .params, .geo = apl_geo_once, .dep_index = .dep_index
        )
      },
      .args = list(.pass = .pass, .dir_out = .dir_out, .run = run_, .paths = .paths,
                   .params = .params, .dep_index = .dep_index)
    )[.progress]
  }

  # A MIRAI ERROR IS A VALUE, NOT A CONDITION. A chunk whose worker died outright -- rather than one
  # whose documents threw, which apl_apply_safe() handles -- comes back as an object and has to be
  # found by class before anything reads a column off it.
  bad_ <- purrr::map_lgl(out_, \(.r) inherits(.r, "miraiError"))
  if (any(bad_)) {
    cli::cli_alert_danger(
      "{sum(bad_)} {cli::qty(sum(bad_))}chunk{?s} died in {?its/their} worker and wrote nothing. \\
       {cli::qty(sum(bad_))}{?It is/They are} retried on the next render; those documents are \\
       simply in no _done."
    )
  }

  out_ <- purrr::list_rbind(out_[!bad_])
  el_  <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))

  if (nrow(out_) > 0L) {
    n_doc_ <- sum(out_$Docs)
    cli::cli_alert_success(
      "{format(n_doc_, big.mark = ',')} {cli::qty(n_doc_)}document{?s} in {round(el_)}s, \\
       {round(n_doc_ / max(el_, 1e-9))}/s."
    )
    fail_ <- sum(out_$Failed)
    if (fail_ > 0L) {
      cli::cli_alert_danger(
        "{format(fail_, big.mark = ',')} {cli::qty(fail_)}document{?s} failed and {?was/were} left \\
         out of _done, so {?it is/they are} retried on the next render. Nothing else was affected: \\
         a chunk that throws is split and retried down to the single document."
      )
    }
  }
  out_
}


#' What every pass did, and what is left
#' @param .tab Tibble from the pass runs, bound together.
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
    p_    <- if (nrow(.tab) == 0L) .tab else dplyr::filter(.tab, .data$Pass == .p)
    done_ <- length(apl_done_ids(.dir = .dirs[[.p]]))
    pop_  <- nrow(.index[[.p]])
    tibble::tibble(
      Pass = .p, Chunks = nrow(p_), DocsRun = sum(p_$Docs), Failed = sum(p_$Failed),
      Secs = sum(p_$Secs), Done = done_, Population = pop_, Left = max(pop_ - done_, 0L)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(PerSecond = .data$DocsRun / pmax(.data$Secs, 1e-9))

  out_ |>
    dplyr::mutate(
      dplyr::across(c(DocsRun, Done, Population, Left), \(.x) format(.x, big.mark = ",")),
      dplyr::across(c(Secs, PerSecond), \(.x) tbl_num(.x))
    ) |>
    tbl_say(.title = "One row per pass")

  cli::cli_alert_info(
    "Secs IS WORKER TIME SUMMED AND NOT WALL CLOCK. Divide by the worker count for the elapsed \\
     figure; the gap between the two is what the parallelism bought."
  )

  left_ <- sum(out_$Left)
  if (left_ > 0L) {
    cli::cli_alert_info(
      "{format(left_, big.mark = ',')} {cli::qty(left_)}document{?s} outstanding across the five \\
       passes. Re-render to continue: each asks the ledger what it has and the directory what is \\
       written."
    )
  } else {
    cli::cli_alert_success("Every document in every population has been written.")
  }
  invisible(out_)
}


# 7. The released files --------------------------------------------------------------------------------------------------

#' One released file, as an arrow dataset
#'
#' THE DIRECTORY IS THE RELEASE. A directory of parquets already is a dataset, so there is no final
#' bind and no moment where a partial pass could be assembled into something that looks finished.
#'
#' @param .dir Directory named for a pass's policy hash.
#' @param .stem Character. The file stem.
#' @return An arrow Dataset, or NULL where no chunk has been committed.
apl_dataset <- function(.dir, .stem) {
  if (FALSE) {
    .dir  <- .lP$Output$Store[["ORG"]]
    .stem <- "org_mentions"
  }

  files_ <- fs::path(apl_chunk_dirs(.dir = .dir), paste0(.stem, ".parquet"))
  files_ <- files_[fs::file_exists(files_)]
  if (length(files_) == 0L) return(NULL)
  arrow::open_dataset(sources = files_)
}


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
    purrr::map(apl_pass_spec(.pass = .p)$Writes, function(.s) {
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
  if (FALSE) .tab <- tab_out

  cli::cli_h2("The eight released files")
  .tab |>
    dplyr::mutate(
      dplyr::across(c(Rows, Docs), \(.x) format(.x, big.mark = ",")),
      PctPop = tbl_pct(.data$PctPop)
    ) |>
    tbl_say(.title = "One row per file, over every chunk written so far")

  cli::cli_alert_info(
    "ONLY org_mentions IS BUILT FROM THE KEY SIDE, so it alone must read 100%: 04B1 writes a \\
     sentinel row for a contract in which no organisation was found, which is what makes \\
     n_distinct(DocID) over that file the whole population. The other seven are SPAN files, so \\
     PctPop below 100% there is a measurement about the corpus and not a gap in the pass."
  )
  cli::cli_alert_info(
    "THE DENOMINATOR FOR A RATE IS THEREFORE THE PASS'S POPULATION AND NEVER THE FILE'S DocID \\
     COUNT. A contract with no redaction marker has no row in redact_spans and still redacted \\
     nothing, which is a finding; counting only the documents present would report every contract \\
     as having redacted something."
  )
  invisible(.tab)
}


#' Every released file against the dictionary its 04B document declared
#'
#' THE SAME CHECK 04B RUNS, ON THE CORPUS RATHER THAN THE SAMPLE. Each 04B document builds a
#' dictionary from a declared list and aborts where it disagrees with the file it wrote; running the
#' same functions here is what says the corpus release has the same shape as the sample release,
#' which is the whole claim this document makes.
#'
#' SCHEMA FROM ARROW, NEVER INFERRED. names(open_dataset()) reads the parquet's own schema, so a
#' column added or renamed by a chain is seen as the file has it rather than as this document
#' expects.
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
      # column expression rather than a literal.
      empty_ <- purrr::set_names(purrr::map(names(ds_), \(.x) logical(0)), names(ds_)) |>
        tibble::as_tibble()
      ok_ <- try(spec_[[.s]]$Fun(empty_), silent = TRUE)
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
    "Every corpus file carries exactly the columns its 04B document documents, so a reader who \\
     knows the sample release knows this one."
  )
  invisible(.tab)
}


# 8. The rule against the naive ladder -----------------------------------------------------------------------------------

#' Every document's collapse, as the chunks wrote it
#'
#' READ RATHER THAN RECOMPUTED. Each chunk wrote the document-grain collapse that the same call which
#' produced its release had already computed, so this is a read of about a million small rows rather
#' than fifty re-runs of five collapse functions on every render. That is what keeps a second render
#' to seconds, which is what buys a document with no eval: false in it.
#'
#' _facts.parquet IS NOT PART OF THE RELEASE, and the underscore says so. It is what this document
#' reports from; the export calls the collapse functions over the long files itself.
#'
#' @param .dir Directory named for a pass's policy hash.
#' @return Tibble: one row per document, with that entity's ladder columns.
apl_facts <- function(.dir) {
  if (FALSE) .dir <- .lP$Output$Store[["DATE"]]

  files_ <- fs::path(apl_chunk_dirs(.dir = .dir), "_facts.parquet")
  files_ <- files_[fs::file_exists(files_)]
  if (length(files_) == 0L) return(tibble::tibble())

  arrow::open_dataset(sources = files_) |>
    dplyr::collect()
}


#' The rule against the naive definition, one row per measure
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

  say_ <- function(.e, .m, .n, .r) tibble::tibble(Entity = .e, Measure = .m, Naive = .n, Rule = .r)
  out_ <- list()

  f_ <- .facts$ORG
  if (!is.null(f_) && nrow(f_) > 0L) {
    out_ <- c(out_, list(
      say_("ORG", "Distinct spellings vs parties", mean(f_$NaiveSpans), mean(f_$NaiveParties)),
      say_("ORG", "Counterparties per contract",   mean(f_$NaiveCounter), mean(f_$NCounter))
    ))
  }

  f_ <- .facts$GPE
  if (!is.null(f_) && nrow(f_) > 0L) {
    out_ <- c(out_, list(
      say_("GPE", "Distinct states per contract",    mean(f_$NaiveStates), mean(f_$RuleStates)),
      say_("GPE", "Distinct countries per contract", mean(f_$NaiveCountries),
           mean(f_$RuleCountries))
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
      say_("MONEY", "Amounts per contract", mean(f_$NAmountsRaw), mean(f_$NAmounts)),
      # BOTH OVER THE SAME CONTRACTS, and this is the error 04B4's own prose warns about. Computing
      # each median over its own non-missing rows compares a set INCLUDING the contracts whose every
      # figure was boilerplate against one excluding them, and then reports the filters as having
      # RAISED contract value.
      say_("MONEY", "Median largest USD amount",
           stats::median(f_$NaiveMaxUSD[!is.na(f_$MoneyMaxUSD)], na.rm = TRUE),
           stats::median(f_$MoneyMaxUSD, na.rm = TRUE))
    ))
  }

  f_ <- .facts$REDACT
  if (!is.null(f_) && nrow(f_) > 0L) {
    out_ <- c(out_, list(
      say_("REDACT", "Markers per contract",   mean(f_$NRedact), mean(f_$NBracketed)),
      say_("REDACT", "Contracts carrying one", mean(f_$NRedact > 0L), mean(f_$NBracketed > 0L))
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
     are one query on different data rather than two implementations free to disagree."
  )
  cli::cli_alert_info(
    "READ THESE AGAINST 04B'S OWN HEADLINE TABLES. A corpus rate far from the sample rate is either \\
     a finding about representativeness or a defect in this pass, and the two are told apart by \\
     reading spans rather than by reading this table."
  )
  cli::cli_alert_info(
    "DATE'S SECOND ROW IS THE ONE TO SHOW A REFEREE. A duration capped at thirty years cannot have \\
     a standard deviation above fifteen, and the naive column is where a published figure of 40.78 \\
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


# 9. Validation ----------------------------------------------------------------------------------------------------------

#' Whether cutting the same documents two ways changes the answer
#'
#' THE ONE CLAIM THIS DOCUMENT CANNOT SIMPLY ASSERT. Everything above rests on a chunk being the same
#' arithmetic as a document: every grouping and every join key in all five chains carries DocID, so a
#' rule applied to a chunk cannot be reading a quantity computed over the group it happened to be
#' handed. That is checkable, and a check beats an argument.
#'
#' ONE SET OF DOCUMENTS, RUN WHOLE AND RUN AS TWO HALVES, and the released rows compared after
#' sorting. The halves are disjoint sets of DOCUMENTS rather than a split of rows, because a
#' partition of rows would test something this pass never does.
#'
#' IT WOULD FAIL LOUDLY IF A RULE EVER GREW A CORPUS-WIDE DENOMINATOR -- a frequency gate, a
#' quantile, a share over the input. 04B1's family-frequency gate was removed for exactly that
#' reason, and this is what would catch its return.
#'
#' @param .pass Character. Which pass.
#' @param .doc_ids Character. A small set of documents, in DocID order.
#' @param .paths The runbook's .lP$Input.
#' @param .params The runbook's .lP$Params.
#' @param .geo List: the gazetteer. GPE only.
#' @param .dep_index Tibble from apl_dep_index(). GPE only.
#' @return Tibble: Pass, File, Rows, Identical.
apl_check_split <- function(.pass, .doc_ids, .paths, .params, .geo = NULL, .dep_index = NULL) {
  if (FALSE) {
    .pass    <- "MONEY"
    .doc_ids <- utils::head(tab_index$MONEY$DocID, 400L)
    .paths   <- .lP$Input
    .params  <- .lP$Params
  }

  cut_   <- ceiling(length(.doc_ids) / 2L)
  stems_ <- apl_pass_spec(.pass = .pass)$Writes

  run_ <- function(.ids) {
    apl_apply_ids(.pass = .pass, .doc_ids = .ids, .paths = .paths, .params = .params,
                  .geo = .geo, .dep_index = .dep_index)$Files
  }

  one_ <- run_(.ids = .doc_ids)
  a_   <- run_(.ids = .doc_ids[seq_len(cut_)])
  b_   <- run_(.ids = .doc_ids[-seq_len(cut_)])

  # SORTED ON EVERY COLUMN BEFORE COMPARING, because the halves are bound in an order the whole run
  # never produces and row order is not part of the claim.
  # pick() AND NOT across(). dplyr deprecated across() inside arrange() at 1.1, and a deprecation
  # warning raised once per file per pass is noise a reader learns to skip past.
  norm_ <- function(.t) {
    if (nrow(.t) == 0L) return(.t)
    dplyr::arrange(.t, dplyr::pick(dplyr::everything()))
  }

  purrr::map(stems_, function(.s) {
    whole_ <- norm_(.t = one_[[.s]])
    split_ <- norm_(.t = dplyr::bind_rows(a_[[.s]], b_[[.s]]))
    tibble::tibble(
      Pass = .pass, File = .s, Rows = nrow(whole_),
      Identical = isTRUE(all.equal(whole_, split_, check.attributes = FALSE))
    )
  }) |>
    purrr::list_rbind()
}


#' The split check, reported
#' @param .tab Tibble from apl_check_split(), bound across passes.
#' @param .n Integer. How many documents each pass was checked on.
#' @return Invisibly .tab.
apl_report_split <- function(.tab, .n) {
  if (FALSE) {
    .tab <- tab_split
    .n   <- 400L
  }

  cli::cli_h2("The partition cannot change an answer")
  tbl_say(
    .tab   = .tab,
    .title = paste0("Each file, run whole and run as two halves, on ",
                    format(.n, big.mark = ","), " documents")
  )

  bad_ <- dplyr::filter(.tab, !.data$Identical)
  if (nrow(bad_) > 0L) {
    cli::cli_abort(c(
      "{nrow(bad_)} file{?s} {?differs/differ} when the same documents are cut two ways.",
      "x" = "{paste(paste0(bad_$Pass, '/', bad_$File), collapse = ', ')}.",
      "i" = "A rule is reading a quantity computed over the group it was handed. The chunk size is
             then part of the result and the release is not reproducible; do not publish it."
    ))
  }
  cli::cli_alert_success(
    "Every file is identical under both partitions, so the chunk size and the worker count are \\
     scheduling decisions and belong in no hash."
  )
  invisible(.tab)
}


#' Whether the release and the done set agree
#'
#' TWO WAYS TO BE WRONG, AND BOTH ARE SILENT. A document with rows in a release but no entry in any
#' _done was written by a chunk that was not asked about it, which is a partition leak. A document
#' named in two _done files was processed twice, and its rows are in the release twice.
#'
#' @param .dirs Named character of output directories.
#' @param .index Named list of tibbles from apl_corpus_index().
#' @return Tibble: one row per pass.
apl_check_done <- function(.dirs, .index) {
  if (FALSE) {
    .dirs  <- .lP$Output$Store
    .index <- tab_index
  }

  purrr::map(names(.dirs), function(.p) {
    files_ <- fs::path(apl_chunk_dirs(.dir = .dirs[[.p]]), "_done.parquet")
    files_ <- files_[fs::file_exists(files_)]
    if (length(files_) == 0L) {
      return(tibble::tibble(Pass = .p, Done = 0L, Duplicated = 0L, Outside = 0L, Unasked = 0L))
    }

    all_  <- unlist(purrr::map(files_, \(.f) arrow::read_parquet(.f)$DocID), use.names = FALSE)
    uniq_ <- unique(all_)
    ds_   <- apl_dataset(.dir = .dirs[[.p]], .stem = apl_pass_spec(.pass = .p)$Writes[[1L]])

    seen_ <- if (is.null(ds_)) {
      character(0)
    } else {
      ds_ |>
        dplyr::distinct(.data$DocID) |>
        dplyr::collect() |>
        dplyr::pull("DocID")
    }

    tibble::tibble(
      Pass       = .p,
      Done       = length(uniq_),
      Duplicated = length(all_) - length(uniq_),
      Outside    = sum(!uniq_ %in% .index[[.p]]$DocID),
      Unasked    = sum(!seen_ %in% uniq_)
    )
  }) |>
    purrr::list_rbind()
}


#' The done-set check, reported
#' @param .tab Tibble from apl_check_done().
#' @return Invisibly .tab.
apl_report_done <- function(.tab) {
  if (FALSE) .tab <- tab_done

  cli::cli_h2("Every chunk was asked about a disjoint set, and wrote only about it")
  .tab |>
    dplyr::mutate(dplyr::across(c(Done, Duplicated, Outside, Unasked),
                                \(.x) format(.x, big.mark = ","))) |>
    tbl_say(.title = "One row per pass")

  bad_ <- dplyr::filter(.tab, .data$Duplicated > 0L | .data$Outside > 0L | .data$Unasked > 0L)
  if (nrow(bad_) > 0L) {
    cli::cli_abort(c(
      "{nrow(bad_)} pass{?es} {?has/have} a done set that disagrees with what was released.",
      "x" = "{paste(bad_$Pass, collapse = ', ')}.",
      "i" = "Duplicated means a document was processed twice and its rows are in the release twice.
             Outside means a document was processed that the ledger does not name. Unasked means a
             file holds rows for a document no chunk recorded processing."
    ))
  }
  cli::cli_alert_success(
    "No document was processed twice, none was processed that the ledger does not name, and no file \\
     holds a row for a document no chunk was asked about."
  )
  invisible(.tab)
}


#' Directories written by a design this document no longer uses
#'
#' MOVED ASIDE RATHER THAN DELETED, AND REPORTED RATHER THAN IGNORED. An earlier 04D named its
#' committed directories window-*; this one names them chunk-*, so an interrupted run under the old
#' design is invisible to every glob here and would sit in the release directory being neither read
#' nor noticed.
#'
#' @param .dirs Named character of output directories.
#' @return Invisibly a tibble: Pass, Legacy.
apl_check_legacy <- function(.dirs) {
  if (FALSE) .dirs <- .lP$Output$Store

  out_ <- purrr::map(names(.dirs), function(.p) {
    d_ <- .dirs[[.p]]
    n_ <- if (!fs::dir_exists(d_)) {
      0L
    } else {
      length(fs::dir_ls(d_, regexp = "/window-[^/]+$", type = "directory", recurse = FALSE))
    }
    tibble::tibble(Pass = .p, Legacy = n_)
  }) |>
    purrr::list_rbind()

  n_ <- sum(out_$Legacy)
  if (n_ > 0L) {
    cli::cli_alert_warning(
      "{n_} {cli::qty(n_)}director{?y/ies} named window-* {?was/were} found. {cli::qty(n_)}\\
       {?It/They} came from an earlier design, {?is/are} read by nothing here, and should be renamed \\
       out of the release directory rather than deleted."
    )
  } else {
    cli::cli_alert_success("No output from an earlier design is present.")
  }
  invisible(out_)
}


# 10. Overview -----------------------------------------------------------------------------------------------------------

#' The corpus label spine, for the figures alone
#'
#' NOTHING THAT WRITES A FILE READS THIS. The five passes take their anchors from the register and
#' from nothing else, so the release does not depend on the classification stage at all. The
#' OVERVIEW does: every headline figure 04B produced is split by contract type, and a type is a
#' label rather than an extraction result.
#'
#' THE ENGINE IS DECLARED RATHER THAN RESOLVED HERE. 03F's release carries every engine's label side
#' by side and deliberately no single Class column, because which one is authoritative is a decision
#' and a decision belongs in a configuration a reader can see.
#'
#' @param .path_release Release parquet from 03F, as ent_release_path() resolved it.
#' @param .engine Character. Label prefix in the release, "Bert" or "Kw".
#' @param .doc_ids Character. The documents the figures will cover.
#' @return Tibble: DocID, Class.
apl_class <- function(.path_release, .engine, .doc_ids) {
  if (FALSE) {
    .path_release <- path_labels
    .engine       <- .lP$Params$Engine
    .doc_ids      <- tab_index$GPE$DocID
  }

  if (is.na(.path_release)) {
    cli::cli_abort(c(
      "03F has released no corpus labels, so the Overview has no contract types to split by.",
      "i" = "Every table above is unaffected: the five passes read the register alone and the eight
             released files carry no class column. Render 03F-ClassifyApply, or drop the by-type
             figures from this document."
    ))
  }

  out_ <- ent_corpus_class(
    .path_release = .path_release, .engine = .engine, .doc_ids = .doc_ids
  ) |>
    dplyr::select("DocID", "Class") |>
    dplyr::filter(!is.na(.data$Class))

  cli::cli_alert_info(
    "{format(nrow(out_), big.mark = ',')} of {format(length(.doc_ids), big.mark = ',')} \\
     {cli::qty(length(.doc_ids))}document{?s} carry a {(.engine)} label \\
     ({tbl_pct(nrow(out_) / max(length(.doc_ids), 1L))})."
  )
  out_
}


#' The document-grain facts, restricted to the documents that carry a type
#'
#' AN INNER JOIN, AND THE DENOMINATOR CHANGES BECAUSE OF IT. Every figure below is a statement about
#' contract types, so a document 03F could not label belongs in none of them -- but that makes the
#' Overview's population smaller than the Results tables above, and a reader comparing the two
#' without knowing why would read the difference as a defect.
#'
#' @param .facts Named list of tibbles from apl_facts().
#' @param .class Tibble from apl_class().
#' @return A named list of tibbles, each with Class added.
apl_label_facts <- function(.facts, .class) {
  if (FALSE) {
    .facts <- tab_facts
    .class <- tab_class
  }

  purrr::map(.facts, function(.t) {
    if (nrow(.t) == 0L) return(.t)
    dplyr::inner_join(.t, .class, by = dplyr::join_by(DocID))
  })
}


#' The marker file, at the grain the kind figure needs
#'
#' TWO COLUMNS AND NOT SEVENTEEN. red_table_kind() counts markers and the documents carrying them, so
#' the marker text, its offsets and what it replaced are all read for nothing -- and redact_spans is
#' the longest of the eight releases.
#'
#' @param .dir Directory named for REDACT's policy hash.
#' @return Tibble: DocID, Kind. Empty where nothing is released.
apl_marks <- function(.dir) {
  if (FALSE) .dir <- .lP$Output$Store[["REDACT"]]

  ds_ <- apl_dataset(.dir = .dir, .stem = "redact_spans")
  if (is.null(ds_)) return(tibble::tibble(DocID = character(0), Kind = character(0)))

  ds_ |>
    dplyr::select("DocID", "Kind") |>
    dplyr::collect()
}


#' The governing-law file, at the grain the jurisdiction figure needs
#' @param .dir Directory named for GPE's policy hash.
#' @return Tibble: Jurisdiction, JurisdictionLevel. Empty where nothing is released.
apl_law <- function(.dir) {
  if (FALSE) .dir <- .lP$Output$Store[["GPE"]]

  ds_ <- apl_dataset(.dir = .dir, .stem = "law_clauses")
  if (is.null(ds_)) {
    return(tibble::tibble(Jurisdiction = character(0), JurisdictionLevel = character(0)))
  }

  ds_ |>
    dplyr::select("Jurisdiction", "JurisdictionLevel") |>
    dplyr::collect()
}


#' Everything this document did, in one table
#' @param .out Tibble from apl_table_outputs().
#' @param .hashes Named character of policy hashes.
#' @param .index Named list of tibbles from apl_corpus_index().
#' @return Invisibly .out.
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
                                \(.p) paste(apl_pass_spec(.pass = .p)$Writes, collapse = ", ")),
    Rows       = purrr::map_chr(names(.hashes),
                                \(.p) format(sum(.out$Rows[.out$Pass == .p]), big.mark = ","))
  ) |>
    tbl_say(.title = "One row per pass")

  cli::cli_alert_info(
    "THE HASHES ARE THE ONE THING TO RECORD. Each names its pass's directory and stands for every \\
     rule parameter, every extractor spec hash and every dependency that pass used -- so two \\
     releases carrying the same hash were produced by the same pipeline and two carrying different \\
     ones were not."
  )
  invisible(.out)
}
