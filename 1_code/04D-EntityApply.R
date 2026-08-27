# 04D-EntityApply: apply the 04B rules to the corpus 04C extracted -------------------------------------------------------
#
# WHAT THIS FILE DOES, IN ONE PARAGRAPH
# Every rule has been settled. 04A extracted a sample and measured which family to trust, 04B built
# and scored a rule per entity against facts EDGAR recorded, 04C extracted the whole corpus. This
# stage decides nothing. It reads the rules as parameters, applies them to 1.19 million attachments
# in chunks, and writes the same four files 04B released -- identical columns, identical order,
# identical meaning -- with the corpus in them instead of 4,398 documents.
#
# WHY AN APPLY STAGE HAS ALMOST NO DECISIONS OF ITS OWN
# An apply stage free to choose its own family or its own window is one whose output depends on which
# version of it last ran, and a released dataset cannot be defended on those terms. Everything that
# could vary is declared in the runbook's Configuration, printed in the render, and hashed into the
# chunk directory so a changed rule cannot be served from a stale cache.
#
# THE RULE FUNCTIONS ARE 04B'S, CALLED RATHER THAN COPIED
# Not one rule is reimplemented here. The five 04B libraries are sourced and their apply functions
# called on chunks of the corpus instead of on the sample. That is the project's own instruction --
# shared tooling lives upstream and later scripts source it -- and it is the mistake this family
# already made twice: dte_term() re-implemented dateregex-v3's TERM in R and red_words() recounted
# what the register held. Both worked. Both could only ever work on the sample.
#
# THREE THINGS THAT WERE TRUE OF THE PREVIOUS VERSION AND ARE NOT TRUE NOW
#
#   NO CHAIN OPENS A DOCUMENT. dte_describe() cut a cue window out of the canonical text, geo_law()
#   scanned whole documents for governing-law language, and mny_load() read sixty characters before
#   every amount. All three now read CueBefore and CueAfter, stored at extraction. The text staging
#   this file used to do -- read a chunk's parquets, write one staged file, hand it to three chains
#   -- is gone entirely, and with it the Cues dial that existed to switch those reads off and whose
#   only effect was to delete the governing-law columns from the corpus release.
#
#   THE PARTY CHAIN IS CHUNKABLE. ent_rule()'s family-frequency gate admitted a shared leading token
#   as evidence of a corporate family when that token opened at most a given share of DOCUMENTS, and
#   ent_apply() computed the share from the spans it was handed -- so under chunking the denominator
#   became the chunk. Measured, 176 leading tokens fell on opposite sides of the threshold at 2,546
#   documents per chunk. The gate is gone: grouping is now word-prefix comparison WITHIN a document,
#   which needs no denominator at all. So ORG chunks with everything else and the whole-corpus pass
#   this file used to make has no reason to exist.
#
#   EVERY RULE IS PER DOCUMENT, WHICH IS WHAT MAKES CHUNKING SAFE. Grouping, party identification,
#   the window, place attachment, the ambiguous-city resolution, the date cascade, the money filters
#   and the marker counts all read one document's spans and nothing else. The gazetteer is global and
#   read-only. That is an argument rather than a proof, so Validation runs the same documents as one
#   chunk and as several and compares the four releases row for row.
#
# WHAT COMES OUT: THE SAME FOUR FILES 04B WROTE
#   parties_geo.parquet     one row per contract per party  -- 04B1's release, widened by 04B2
#   contracts_date.parquet  one row per contract            -- 04B3
#   contracts_money.parquet one row per contract            -- 04B4
#   contracts_redact.parquet one row per contract           -- 04B5
# Each is written by the same 04B function that wrote the sample's, so a column that changed there
# changes here and a dictionary check that passes there passes here.
#
# THE CHUNK CACHE IS KEYED ON A POLICY HASH, AND THE HASH INCLUDES THE EXTRACTOR
# A rule changed without a new chunk directory would be served from the old one. The hash therefore
# folds in every parameter AND the per-family spec hashes 04C recorded, because a re-extraction that
# moved a model's rules leaves the parameters untouched while every span underneath them changes.
# That was a live defect: apl_policy_hash() hashed .lP$Params alone.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi::stri_sub never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .dir_store <- .lP$Input$Store
}


# 1. Policy --------------------------------------------------------------------------------------------------------------

#' One hash standing for every choice this pass makes
#'
#' THE CHUNK DIRECTORY IS NAMED FOR IT, so a changed rule cannot be served from a stale cache. The
#' cache is worth days on a corpus this size and worth nothing at all if it can return the output of
#' a rule that no longer exists.
#'
#' THE EXTRACTOR IS PART OF THE POLICY, and leaving it out was a defect. Every span this pass reads
#' was produced by a model whose spec hash 04C recorded; re-extracting with a changed gazetteer or a
#' changed money grammar leaves .lP$Params byte-identical while the input underneath it is different.
#' Hashing the parameters alone would then serve chunks computed from spans that no longer exist.
#'
#' @param .params The runbook's .lP$Params.
#' @param .describe Output of ner_describe(), carrying one spec hash per model and entity.
#' @return Character. A twelve-character hash.
apl_policy_hash <- function(.params, .describe) {
  if (FALSE) {
    .params   <- .lP$Params
    .describe <- tab_describe
  }

  spec_ <- .describe |>
    dplyr::filter(.data$Ready) |>
    dplyr::arrange(.data$Family, .data$Model, .data$Entity) |>
    dplyr::transmute(Row = paste(.data$Family, .data$Model, .data$Entity, .data$SpecHash))

  # stri_sub AND NOT substr, which is the house rule and is load-bearing here for a different reason
  # than usual: a hash is ASCII so the two agree, but a file that reaches for substr once teaches the
  # next reader that substr is allowed, and everywhere else in this project it silently indexes bytes
  # where the offsets are code points.
  stringi::stri_sub(digest::digest(list(.params, spec_$Row), algo = "xxhash64"), to = 12L)
}


#' Every choice this pass makes, as a table
#'
#' PRINTED RATHER THAN DESCRIBED. A parameter a reader cannot see is a parameter nobody reviewed, and
#' the render is where this pass is defended.
#'
#' @param .params The runbook's .lP$Params.
#' @return Tibble: Stage, Setting, Value, From.
apl_policy_table <- function(.params) {
  if (FALSE) .params <- .lP$Params

  tibble::tribble(
    ~Stage,   ~Setting,        ~Value,                                          ~From,
    "ORG",    "family",        .params$Family$ORG,                              "04B1",
    "ORG",    "key",           .params$Rule$Org$Key,                            "04B1",
    "ORG",    "merge",         as.character(.params$Rule$Org$MergeFragments),   "04B1",
    "ORG",    "window",        format(.params$Spec$Org$Par, big.mark = ","),    "04B1",
    "ORG",    "tail share",    format(.params$Spec$Org$TailShare),              "04B1",
    "GPE",    "family",        .params$Family$GPE,                              "04B2",
    "GPE",    "reach",         format(.params$Spec$Geo$Reach, big.mark = ","),  "04B2",
    "DATE",   "family",        .params$Family$DATE,                             "04B3",
    "DATE",   "start",         .params$Spec$Date$Start,                         "04B3",
    "DATE",   "end",           .params$Spec$Date$End,                           "04B3",
    "DATE",   "cap, years",    format(.params$Spec$Date$CapYears),              "04B3",
    "DATE",   "cue window",    format(.params$CueWin$Date),                     "04B3",
    "MONEY",  "family",        .params$Family$MONEY,                            "04B4",
    "MONEY",  "filter",        .params$Spec$Money$Filter,                       "04B4",
    "MONEY",  "cue window",    format(.params$Spec$Money$CueWin),               "04B4",
    "REDACT", "family",        .params$Family$REDACT,                           "04B5",
    "REDACT", "words from",    "register",                                      "04B5",
    "PASS",   "chunk size",    format(.params$ChunkSize, big.mark = ","),       "04D"
  )
}


#' What this pass will do, before it does any of it
#' @param .tab Tibble from apl_policy_table().
#' @param .hash Character from apl_policy_hash().
#' @return Invisibly .tab.
apl_report_policy <- function(.tab, .hash) {
  if (FALSE) {
    .tab  <- tab_policy
    .hash <- .lP$Params$Hash
  }

  cli::cli_h2("The policy")
  tbl_say(.tab = .tab, .title = "Every choice this pass makes, and which document made it")

  cli::cli_alert_info(
    "POLICY HASH {(.hash)}. The chunk directory is named for it, so a changed rule cannot be served \\
     from a stale cache. It folds in the per-family SPEC HASHES 04C recorded as well as these \\
     settings, because a re-extraction that moved a model's rules leaves every value above \\
     unchanged while every span underneath them is different."
  )
  invisible(.tab)
}


# 2. Input ---------------------------------------------------------------------------------------------------------------

#' Every document 04C finished, from the ledger
#'
#' THE LEDGER AND NOT THE ENTITY TABLES. A document processed and found to hold no date has no row in
#' the date table and is not missing -- it is a contract with no date in it, which is a measurement.
#' Taking the population from the tables would silently drop exactly those documents and inflate
#' every rate this pass reports.
#'
#' FINISHED MEANS A ROW FOR EVERY PAIR. A document part-way through 04C's plan would be applied on the
#' entities it has and would carry zeros for the rest, which is indistinguishable in the output from a
#' contract that named none of them.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .family Character. Family whose ledger defines the population.
#' @param .entity Character. Entities a finished document must carry.
#' @return Tibble: DocID.
apl_corpus_index <- function(.dir_store, .family, .entity) {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .family    <- "matcon"
    .entity    <- c("GPE", "DATE", "TERM", "MONEY", "REDACT", "LAW")
  }

  con_ <- ner_db_connect(
    .db_path = ner_db_path(.dir = .dir_store, .family = .family), .read_only = TRUE
  )
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  in_ <- paste0("'", .entity, "'", collapse = ", ")

  DBI::dbGetQuery(con_, glue::glue(
    "SELECT DocID FROM runs
      WHERE Entity IN ({in_}) AND Status <> 'error'
      GROUP BY DocID
     HAVING COUNT(DISTINCT Entity) = {length(.entity)}
      ORDER BY DocID"
  )) |>
    tibble::as_tibble()
}


#' Resolve 03F's corpus label release to one file
#'
#' A DIRECTORY, NOT A FILE, AND THE NAME CARRIES STATE. app_write_release() writes
#' contract_labels_L<max_len>[_partial].parquet into a release directory, with a manifest beside each
#' one -- so the name depends on the token limit that produced it and on whether every classification
#' run had finished. Naming a fixed file here would break the moment the limit changed, and it did:
#' this document first pointed at labels.parquet, which no version of 03F has ever written.
#'
#' A PARTIAL RELEASE IS REFUSED RATHER THAN READ. 03F stamps _partial where any run is incomplete,
#' and a label file missing a slice of the corpus would silently shrink the population this pass
#' applies to -- reported as a smaller corpus rather than as a missing input, which is the shape of
#' error nobody catches.
#'
#' THE NEWEST COMPLETE ONE WINS where several exist. A re-run at a different token limit leaves both
#' on disk, and the later file is the one 03F meant.
#'
#' @param .dir 03F's release directory.
#' @param .stem Character. The stem app_write_release() stamps.
#' @return Character. One path.
apl_labels_path <- function(.dir, .stem = "contract_labels") {
  if (FALSE) {
    .dir   <- .lP$Input$Labels
    .stem  <- "contract_labels"
  }

  if (!fs::dir_exists(.dir)) {
    cli::cli_abort(c(
      "No label release directory at {.path {(.dir)}}.",
      "i" = "03F-ClassifyApply writes it. Its Output$Release is a DIRECTORY, not a file."
    ))
  }

  all_ <- fs::dir_ls(.dir, regexp = paste0(.stem, ".*\\.parquet$"), recurse = FALSE)
  # The manifest sits beside every release and matches the same stem, so it is excluded by name
  # rather than by hoping the ordering below never reaches it.
  all_ <- all_[!stringi::stri_detect_fixed(fs::path_file(all_), "_manifest")]
  ok_  <- all_[!stringi::stri_detect_fixed(fs::path_file(all_), "_partial")]

  if (length(ok_) == 0L) {
    cli::cli_abort(c(
      "No complete label release in {.path {(.dir)}}.",
      "x" = "{length(all_)} file{?s} found, {?all/all} marked partial.",
      "i" = "A partial release would shrink the population this pass applies to and report it as a
             smaller corpus rather than as a missing input. Finish 03F first."
    ))
  }

  out_ <- ok_[[which.max(fs::file_info(ok_)$modification_time)]]
  cli::cli_alert_info("Labels from {.path {fs::path_file(out_)}}.")
  out_
}


#' The anchor for every document in the corpus
#'
#' THE SAME FUNCTION 04B CALLS, on the register rather than on the sample. ent_anchor_keys() reads
#' 03A's labels, 02B's register and optionally 01C's addresses; the corpus has the second and the
#' third, and the first only where 03F classified. A document with no label carries NA in Class,
#' which every rule tolerates because Class is carried and never conditioned on.
#'
#' THE LABEL COLUMNS ARE NAMED, because 03F's release does not use 03A's names. The prepared sample
#' carries ClassDetailed and AmendType; the corpus release carries BertClassDetailed and
#' BertAmendType, one pair per engine, and no Fold at all -- a fold is a cross-validation artifact of
#' the sample. Naming them here is what lets one anchor function serve both files.
#'
#' THE TRANSFORMER'S LABEL AND NOT THE KEYWORD ARM'S. 03E measured the routing question and returned a
#' strong null: nested cross-validated policy selection did not improve on the transformer, and the
#' oracle ceiling equalled BERT-everywhere at 0.882 macro-F1. So the released label is BERT's, and the
#' keyword columns beside it are a confirmation flag rather than a second opinion to reconcile.
#'
#' THE SEMI-JOIN IS DOING MORE THAN IT LOOKS. 03F's release is fanned out to one row per REGISTRANT
#' COPY, while 04C extracted one row per ATTACHMENT -- the primary copy. Restricting to the index
#' therefore keeps exactly the primaries, which is the population this pass applies to, and drops the
#' repeat copies rather than multiplying every document by its number of filers.
#'
#' @param .path_labels 03F's corpus label release, resolved by apl_labels_path().
#' @param .path_register 02B's register.
#' @param .path_landing 01C's landing pages, for 04B2's precision flag.
#' @param .index Tibble from apl_corpus_index().
#' @param .labels Named character. Which columns of the label file hold Class and AmendType.
#' @param .quiet Logical.
#' @return Tibble: one row per document in .index.
apl_keys <- function(.path_labels, .path_register, .path_landing, .index,
                     .labels = c(Class = "BertClassDetailed", AmendType = "BertAmendType"),
                     .quiet = FALSE) {
  if (FALSE) {
    .path_labels   <- apl_labels_path(.dir = .lP$Input$Labels)
    .path_register <- .lP$Input$Register
    .path_landing  <- .lP$Input$Landing
    .index         <- tab_index
    .labels        <- c(Class = "BertClassDetailed", AmendType = "BertAmendType")
    .quiet         <- FALSE
  }

  out_ <- ent_anchor_keys(
    .path_prepared = .path_labels,
    .path_register = .path_register,
    .path_landing  = .path_landing,
    .labels        = .labels,
    .quiet         = .quiet
  ) |>
    dplyr::semi_join(.index, by = dplyr::join_by(DocID))

  miss_ <- nrow(.index) - nrow(out_)
  if (miss_ > 0L && !.quiet) {
    cli::cli_alert_warning(
      "{format(miss_, big.mark = ',')} {cli::qty(miss_)}document{?s} 04C finished {?has/have} no \\
       anchor row, so {?it is/they are} not applied. An anchor is the filing date and the filer's \\
       name; a rule cannot measure a start or match a registrant without one."
    )
  }
  out_
}


# 3. Chunking ------------------------------------------------------------------------------------------------------------

#' Cut the corpus into chunks of documents
#'
#' @param .index Tibble from apl_corpus_index().
#' @param .size Integer. Documents per chunk.
#' @return List of character vectors.
apl_chunks <- function(.index, .size) {
  if (FALSE) {
    .index <- tab_index
    .size  <- .lP$Params$ChunkSize
  }

  ids_ <- .index$DocID
  if (length(ids_) == 0L) return(list())
  split(ids_, ceiling(seq_along(ids_) / .size))
}


#' The keys and lengths for one chunk, cut together
#'
#' ONE PLACE DEFINES A CHUNK, and that is the point of the function. Every 04B apply runs FROM THE KEY
#' SIDE so that a document in which nothing was found still gets a row -- correct, and it means the
#' output is one row per KEY. Handed the corpus keys with a two-thousand-document lens, a chain builds
#' 1.19 million rows to describe two thousand documents. Cutting both here means the two cannot drift.
#'
#' @param .keys Tibble from apl_keys().
#' @param .doc_ids Character. The chunk's documents.
#' @return A list: Keys and Lens, both restricted to .doc_ids.
apl_chunk_frame <- function(.keys, .doc_ids) {
  if (FALSE) {
    .keys    <- tab_keys
    .doc_ids <- chunks_[[1L]]
  }

  keys_ <- dplyr::filter(.keys, .data$DocID %in% .doc_ids)

  lens_ <- keys_ |>
    dplyr::transmute(DocID, DocLen = as.integer(.data$nChars)) |>
    dplyr::filter(!is.na(.data$DocLen), .data$DocLen > 0L)

  list(Keys = keys_, Lens = lens_)
}


# 4. One chunk, five chains, four files -----------------------------------------------------------------------------------

#' Apply all five rules to one chunk
#'
#' EVERY CALL HERE IS 04B'S OWN, with the arguments the runbook declared. Nothing is reimplemented and
#' nothing is adapted: if a chain works on 4,398 documents it works on a chunk of the corpus, because
#' a chunk IS a small sample as far as the rule is concerned.
#'
#' THE ORDER IS A DEPENDENCY, NOT A PREFERENCE. 04B2 attaches places to the parties 04B1 found and to
#' the MENTIONS index it writes, so ORG runs first and hands both on in memory rather than through a
#' file. The other three are independent of both and of each other.
#'
#' NOTHING OPENS A DOCUMENT. Three of these chains used to; all three now read CueBefore and CueAfter,
#' which the store carries. That is why this function takes no staging path and why the pass loop has
#' no read step.
#'
#' @param .frame List from apl_chunk_frame().
#' @param .params The runbook's .lP$Params.
#' @param .dir_store Directory holding the family databases.
#' @param .geo List: the gazetteer lookup and candidate list, read once.
#' @return A list of four tibbles, each the shape its 04B document released.
apl_chunk_apply <- function(.frame, .params, .dir_store, .geo) {
  if (FALSE) {
    .frame     <- frame_
    .params    <- .lP$Params
    .dir_store <- .lP$Input$Store
    .geo       <- geo_once
  }

  keys_ <- .frame$Keys
  lens_ <- .frame$Lens

  # ORG -- 04B1. Grouping, party identification, the window and the tail, then the release and the
  # mention index 04B2 needs.
  # ent_load_org() AND NOT ent_load_entity(). ent_apply() reads SpanKey and CoreKey, which are the
  # reduction and not the store's columns; 04B1 builds them in the loader so that this document calls
  # one function rather than copying a mutate out of a runbook.
  res_org_ <- ent_apply(
    .spans = ent_load_org(
      .dir_store = .dir_store, .lens = lens_, .family = .params$Family$ORG,
      .extras = .params$Extras$ORG, .quiet = TRUE
    ),
    .keys = keys_, .lens = lens_,
    .rule = .params$Rule$Org, .spec = .params$Spec$Org
  )
  parties_ <- ent_release_parties(.roles = res_org_$Roles, .party = res_org_$Party)

  # GPE -- 04B2. The governing-law exclusion, the attachment, and the roll-up onto 04B1's rows.
  res_geo_ <- geo_apply(
    .spans = ent_load_entity(
      .dir_store = .dir_store, .family = .params$Family$GPE, .entity = "GPE",
      .lens = lens_, .extras = ent_extras(.params$Family$GPE, "GPE"), .quiet = TRUE
    ),
    .law = ent_load_entity(
      .dir_store = .dir_store, .family = .params$Family$GPE, .entity = "LAW",
      .lens = lens_, .extras = ent_extras(.params$Family$GPE, "LAW"), .quiet = TRUE
    ),
    .party    = parties_,
    .mentions = res_org_$Mentions,
    .keys     = keys_,
    .geo      = .geo,
    .spec     = .params$Spec$Geo
  )

  # DATE -- 04B3. The cue is a column read; .win narrows the stored window and opens nothing.
  res_dte_ <- dte_apply(
    .dates = dte_describe(
      .dates = dplyr::filter(
        dte_load(.dir_store = .dir_store, .lens = lens_, .family = .params$Family$DATE,
                 .quiet = TRUE),
        .data$Parsed
      ),
      .keys = keys_, .win = .params$CueWin$Date
    ),
    .terms = dte_load_terms(.dir_store = .dir_store, .lens = lens_, .quiet = TRUE),
    .keys  = keys_,
    .spec  = .params$Spec$Date
  )

  # MONEY -- 04B4. The par cue is read from both sides, from the stored columns.
  res_mny_ <- mny_apply(
    .money = mny_load(.dir_store = .dir_store, .lens = lens_, .family = .params$Family$MONEY,
                      .quiet = TRUE),
    .keys  = keys_,
    .spec  = .params$Spec$Money
  )

  # REDACT -- 04B5. NULL for the text path: the word count is the register's, and there is no corpus
  # text file to recount from.
  out_red_ <- red_release(
    .marks = red_load(.dir_store = .dir_store, .lens = lens_, .family = .params$Family$REDACT,
                      .quiet = TRUE),
    .words = red_words(.keys = keys_, .path_text = NULL, .quiet = TRUE),
    .keys  = keys_
  )

  list(
    Parties = res_geo_$Release,
    Date    = res_dte_$Release,
    Money   = res_mny_$Release,
    Redact  = out_red_
  )
}


#' Apply one chunk, write its four files, return a summary and not the data
#'
#' THE UNIT BOTH PATHS SHARE. The serial loop and the parallel map call this and nothing else, so a
#' change to what a chunk does cannot apply to one and not the other -- which is the failure mode of
#' having written the work twice.
#'
#' IT RETURNS A SUMMARY, NEVER THE TABLES. A chunk of 20,000 documents produces roughly 160,000 party
#' rows; handing those back from a daemon would serialise them across a process boundary and rebuild
#' them in the parent, which is more work than computing them. The worker writes its own parquets --
#' the names differ by chunk index so two workers never contend -- and returns nine numbers.
#'
#' FOUR FILES OR NONE. A chunk directory holding three of four reads as complete on the next render
#' and leaves one entity short for that chunk, silently.
#'
#' @param .frame List from apl_chunk_frame().
#' @param .index Integer or character. The chunk's index, which names its files.
#' @param .dir_chunks Directory named for the policy hash.
#' @param .params The runbook's .lP$Params.
#' @param .dir_store Directory holding the family databases.
#' @param .geo List: the gazetteer lookup and candidate list.
#' @return One-row tibble.
apl_chunk_run <- function(.frame, .index, .dir_chunks, .params, .dir_store, .geo) {
  if (FALSE) {
    .frame      <- frame_
    .index      <- 1L
    .dir_chunks <- .lP$Output$Chunks
    .params     <- .lP$Params
    .dir_store  <- .lP$Input$Store
    .geo        <- geo_once
  }

  names_ <- c("Parties", "Date", "Money", "Redact")
  paths_ <- purrr::set_names(
    fs::path(.dir_chunks, paste0(names_, "-", .index, ".parquet")), names_
  )
  n_doc_ <- nrow(.frame$Keys)

  if (all(fs::file_exists(paths_))) {
    return(tibble::tibble(Chunk = as.integer(.index), Docs = n_doc_, Rows = NA_integer_,
                          Seconds = 0, Status = "cached"))
  }

  tic_ <- Sys.time()
  res_ <- try(
    apl_chunk_apply(.frame = .frame, .params = .params, .dir_store = .dir_store, .geo = .geo),
    silent = TRUE
  )

  if (inherits(res_, "try-error")) {
    return(tibble::tibble(
      Chunk = as.integer(.index), Docs = n_doc_, Rows = NA_integer_,
      Seconds = as.numeric(difftime(Sys.time(), tic_, units = "secs")),
      Status = paste0("error: ", conditionMessage(attr(res_, "condition")))
    ))
  }

  purrr::iwalk(res_, \(.t, .n) arrow::write_parquet(.t, paths_[[.n]]))

  tibble::tibble(
    Chunk = as.integer(.index), Docs = n_doc_, Rows = as.integer(nrow(res_$Parties)),
    Seconds = as.numeric(difftime(Sys.time(), tic_, units = "secs")), Status = "ran"
  )
}


#' Run the pass, chunk by chunk, resuming where it stopped
#'
#' RESUMPTION IS A PROPERTY, NOT A FEATURE. Each chunk writes four parquets into a directory named
#' for the policy hash; a chunk whose four files exist is skipped. There is no state to remember
#' between renders and nothing that could be wrong about itself.
#'
#' A CHUNK THAT FAILS IS RECORDED AND THE PASS CONTINUES. One document with an impossible offset
#' should not cost the other million, and a chunk that errored wrote no file so the next render
#' retries it.
#'
#' ONE WORKER IS THE SERIAL PATH, and it is the same function either way: apl_chunk_run() does the
#' work and the only difference is what calls it. Writing the loop twice would let the two drift.
#'
#' WHY PARALLELISM HELPS HERE AND OFTEN DOES NOT. The rules are dplyr over data already in memory,
#' which is single-threaded R; the LOAD is DuckDB, which already uses every core. So workers help in
#' proportion to how much of a chunk is rule rather than read, and the split is reported below rather
#' than assumed. Oversubscribing costs more than it buys.
#'
#' BATCHED RATHER THAN SUBMITTED ALL AT ONCE, for memory. A chunk holds roughly 1.5 million spans
#' each carrying 320 characters of stored context, which is around half a gigabyte before anything
#' else; running every chunk at once would put all of them in flight together. A batch is bounded by
#' the worker count, and it is also what makes progress reportable at all.
#'
#' @param .chunks List from apl_chunks().
#' @param .keys Tibble from apl_keys().
#' @param .dir_chunks Directory named for the policy hash.
#' @param .params The runbook's .lP$Params.
#' @param .dir_store Directory holding the family databases.
#' @param .geo List: the gazetteer lookup and candidate list. Used by the serial path only; a daemon
#'   builds its own from .path_lookup, because shipping 181,810 rows to every worker per chunk is
#'   slower than reading the file once per worker.
#' @param .workers Integer. 1 runs serially in this process.
#' @param .path_lookup The gazetteer parquet, for the daemons to read.
#' @param .path_fun Character. Library files every daemon must source.
#' @return Tibble: one row per chunk.
apl_pass <- function(.chunks, .keys, .dir_chunks, .params, .dir_store, .geo,
                     .workers = 1L, .path_lookup = NULL, .path_fun = character()) {
  if (FALSE) {
    .chunks      <- chunks_
    .keys        <- tab_keys
    .dir_chunks  <- .lP$Output$Chunks
    .params      <- .lP$Params
    .dir_store   <- .lP$Input$Store
    .geo         <- geo_once
    .workers     <- 6L
    .path_lookup <- .lP$Input$Lookup
    .path_fun    <- .lP$Params$Sources
  }

  fs::dir_create(.dir_chunks)
  if (length(.chunks) == 0L) {
    cli::cli_alert_info("Nothing outstanding.")
    return(tibble::tibble())
  }

  # THE FRAMES ARE CUT IN THE PARENT, and that is what keeps the payload small. .keys is 1.19 million
  # rows; a daemon needs the twenty thousand belonging to its chunk. Sending the whole table to every
  # worker would serialise the corpus once per chunk.
  frames_ <- purrr::imap(.chunks, \(.ids, .i) apl_chunk_frame(.keys = .keys, .doc_ids = .ids))
  idx_    <- names(.chunks)
  t0_     <- Sys.time()
  done_   <- 0L

  say_ <- function(.k) {
    el_ <- as.numeric(difftime(Sys.time(), t0_, units = "secs"))
    cli::cli_alert_info(
      "  {(.k)}/{length(.chunks)} chunks | {format(done_, big.mark = ',')} docs | \\
       {round(done_ / max(el_, 1e-9), 1)}/s"
    )
  }

  if (.workers <= 1L) {
    out_ <- purrr::imap(frames_, function(.f, .i) {
      r_ <- apl_chunk_run(.frame = .f, .index = .i, .dir_chunks = .dir_chunks,
                          .params = .params, .dir_store = .dir_store, .geo = .geo)
      done_ <<- done_ + nrow(.f$Keys)
      say_(.k = which(idx_ == .i))
      r_
    }) |>
      purrr::list_rbind()
    return(apl_pass_check(.tab = out_))
  }

  # EVERY DAEMON SOURCES THE LIBRARIES BY PATH. A closure does not cross a process boundary with its
  # environment, so a worker handed a function that calls ent_apply() finds no ent_apply(); the files
  # have to be named and sourced there. This project has been caught by that before.
  mirai::daemons(.workers)
  on.exit(mirai::daemons(0L), add = TRUE)   # SAFE ONLY INSIDE A FUNCTION BODY, which this is

  # ASSIGNED INTO THE DAEMON'S GLOBAL ENVIRONMENT EXPLICITLY, and the difference is not cosmetic.
  # everywhere() with .args evaluates its expression in an environment carrying those arguments,
  # whose parent is the daemon's global -- so `geo_once <- ...` binds THERE and is gone the moment
  # the call returns. source() has local = FALSE and writes to the global regardless, which is why
  # the probe found apl_chunk_run and not the gazetteer: the two were landing in different places.
  mirai::everywhere(
    {
      purrr::walk(.files, \(.f) source(.f, encoding = "UTF-8"))
      assign(
        x     = "geo_once",
        value = list(
          Lookup = geo_lookup(.path_lookup = .lookup),
          Cand   = geo_candidates(.path_lookup = .lookup)
        ),
        envir = globalenv()
      )
    },
    .args = list(.files = .path_fun, .lookup = .path_lookup)
  )

  # A DAEMON IS CHECKED BEFORE A HUNDRED MINUTES ARE SPENT ON IT. everywhere() reports nothing about
  # whether the sourcing worked, so a path that resolved in this process and not in a worker would
  # fail on every chunk and be discovered at the end. One tiny job settles it.
  probe_ <- mirai::mirai_map(
    .x = list(1L),
    .f = function(.x) c(Fun = exists("apl_chunk_run"), Geo = exists("geo_once"))
  )[][[1L]]

  # THE PROBE CAN ITSELF COME BACK AS AN ERROR, and a miraiError is a VALUE rather than a condition
  # -- so all() on it does not do what it looks like. Checked by class before it is read as a result,
  # which is the same discipline every chunk's return needs.
  if (inherits(probe_, "miraiError")) {
    cli::cli_abort(c(
      "A daemon could not be started or could not source the libraries.",
      "x" = "{as.character(probe_)}",
      "i" = "everywhere() sources .path_fun by path, so a path that resolves in this process and
             not in a worker fails here rather than on every chunk."
    ))
  }

  has_fun_ <- isTRUE(probe_[["Fun"]])
  has_geo_ <- isTRUE(probe_[["Geo"]])

  if (!has_fun_ || !has_geo_) {
    cli::cli_abort(c(
      "A daemon is missing what it needs to run a chunk.",
      "x" = "apl_chunk_run found: {(has_fun_)}. Gazetteer built: {(has_geo_)}.",
      "i" = "A missing FUNCTION means a file absent from .path_fun. A missing GAZETTEER means the
             assignment did not reach the daemon's global environment, which is what everywhere()
             does to a bare `<-` when .args are supplied."
    ))
  }
  cli::cli_alert_success(
    "{(.workers)} {cli::qty(.workers)}daemon{?s} started, {?has/have} sourced \\
     {length(.path_fun)} librar{?y/ies} and built the gazetteer."
  )

  # THE INDEX TRAVELS INSIDE THE ELEMENT, and that is the whole of the fix. mirai_map() maps each
  # element of .x onto the FIRST argument of .f and nothing else -- the names of .x name the results,
  # they are not passed as a second argument. A two-argument .f therefore ran with .i missing on
  # every chunk, ten times, and the error handling reported it ten times exactly as designed.
  jobs_ <- purrr::imap(frames_, \(.f, .i) list(Frame = .f, Index = .i))

  batches_ <- split(seq_along(frames_), ceiling(seq_along(frames_) / .workers))

  out_ <- purrr::map(batches_, function(.b) {
    m_ <- mirai::mirai_map(
      .x = purrr::set_names(jobs_[.b], idx_[.b]),
      .f = function(.job, .dir, .par, .store) {
        apl_chunk_run(.frame = .job$Frame, .index = .job$Index, .dir_chunks = .dir,
                      .params = .par, .dir_store = .store, .geo = geo_once)
      },
      .args = list(.dir = .dir_chunks, .par = .params, .store = .dir_store)
    )
    res_ <- m_[]

    # ERRORS COME BACK AS VALUES, NOT AS CONDITIONS. An unchecked miraiError is an element of the
    # list that looks like a result, and list_rbind() on it fails somewhere far from the cause.
    bad_ <- purrr::map_lgl(res_, \(.r) inherits(.r, "miraiError"))
    res_[bad_] <- purrr::map2(idx_[.b][bad_], res_[bad_], \(.i, .e) tibble::tibble(
      Chunk = as.integer(.i), Docs = NA_integer_, Rows = NA_integer_, Seconds = NA_real_,
      Status = paste0("error: ", as.character(.e))
    ))

    done_ <<- done_ + sum(purrr::map_int(frames_[.b], \(.f) nrow(.f$Keys)))
    say_(.k = max(.b))
    purrr::list_rbind(res_)
  }) |>
    purrr::list_rbind()

  apl_pass_check(.tab = out_)
}


#' Say plainly whether any chunk failed
#'
#' A FAILURE REPORTED AS A ROW IN A TABLE IS A FAILURE NOBODY READS. The status column carries the
#' condition message, and a pass with one bad chunk in fifty otherwise looks like a pass.
#'
#' @param .tab Tibble from the pass.
#' @return .tab, invisibly warned about.
apl_pass_check <- function(.tab) {
  if (FALSE) .tab <- tab_pass

  bad_ <- dplyr::filter(.tab, stringi::stri_startswith_fixed(.data$Status, "error"))
  if (nrow(bad_) > 0L) {
    cli::cli_alert_danger(
      "{nrow(bad_)} {cli::qty(nrow(bad_))}chunk{?s} failed and wrote no file, so {?it is/they are} \\
       retried on the next render."
    )
    purrr::walk2(bad_$Chunk, bad_$Status, \(.c, .s) cli::cli_bullets(c("x" = "chunk {(.c)}: {(.s)}")))
  }
  .tab
}


#' Where one chunk's time goes: reading spans against applying rules
#'
#' THE MEASUREMENT THAT DECIDES THE WORKER COUNT. Loading is DuckDB, which already uses every core;
#' the rules are dplyr over data in memory, which is single-threaded R. Workers therefore help in
#' proportion to how much of a chunk is APPLY, and oversubscribing a load that is already parallel
#' costs more than it buys.
#'
#' RUN ON ONE CHUNK AND REPORTED, not assumed. The split differs by corpus and by machine, and this
#' project has enough evidence-free parameters without adding one to the top of a hundred-minute run.
#'
#' @param .frame List from apl_chunk_frame().
#' @param .params The runbook's .lP$Params.
#' @param .dir_store Directory holding the family databases.
#' @return Tibble: one row per phase.
apl_time_split <- function(.frame, .params, .dir_store) {
  if (FALSE) {
    .frame     <- frame_
    .params    <- .lP$Params
    .dir_store <- .lP$Input$Store
  }

  lens_ <- .frame$Lens
  keys_ <- .frame$Keys

  # TIMED BY BRACKETING AND NOT BY A HELPER, and the helper is why. A one-line timer taking the work
  # as an argument has to get the result back out, and the obvious way -- t_(x <<- expr) -- does not
  # work: the promise evaluates in the frame that CREATED it, and <<- then starts its search in that
  # frame's PARENT. So the assignment lands in the global environment and the local stays NULL, which
  # surfaces three lines later as mutate() applied to NULL rather than as a scoping error.
  tic_   <- Sys.time()
  org_   <- ent_load_org(.dir_store = .dir_store, .lens = lens_, .family = .params$Family$ORG,
                         .extras = .params$Extras$ORG, .quiet = TRUE)
  t_org_ <- as.numeric(difftime(Sys.time(), tic_, units = "secs"))

  tic_   <- Sys.time()
  res_   <- ent_apply(.spans = org_, .keys = keys_, .lens = lens_,
                      .rule = .params$Rule$Org, .spec = .params$Spec$Org)
  t_app_ <- as.numeric(difftime(Sys.time(), tic_, units = "secs"))

  tibble::tibble(
    Phase   = c("Load spans (DuckDB, already parallel)", "Apply rule (dplyr, single-threaded)"),
    Seconds = c(t_org_, t_app_),
    N       = c(nrow(org_), nrow(res_$Roles))
  ) |>
    dplyr::mutate(Share = .data$Seconds / sum(.data$Seconds))
}


#' The timing split, and what it implies for the worker count
#' @param .tab Tibble from apl_time_split().
#' @param .workers Integer. The worker count the runbook set.
#' @return Invisibly .tab.
apl_report_time_split <- function(.tab, .workers) {
  if (FALSE) {
    .tab     <- tab_split
    .workers <- 6L
  }

  cli::cli_h2("Where a chunk's time goes")
  .tab |>
    dplyr::mutate(
      Seconds = tbl_num(.data$Seconds),
      N       = format(.data$N, big.mark = ","),
      Share   = tbl_pct(.data$Share)
    ) |>
    tbl_say(.title = "The ORG chain on one chunk, split into its read and its rule")

  app_ <- .tab$Share[[2L]]
  cli::cli_alert_info(
    "ORG IS THE PROBE AND NOT THE WHOLE CHUNK -- it is 362,000 spans of roughly 1.5 million, and the \\
     heaviest single rule of the five. Read the split rather than the seconds."
  )
  cli::cli_alert_info(
    "APPLY IS {tbl_pct(app_)} OF THIS CHAIN, and that is roughly the share workers can speed up. \\
     Amdahl bounds the rest: with {(.workers)} workers the ceiling is about \\
     {tbl_num(1 / ((1 - app_) + app_ / .workers))}x, before any cost of moving data to them. A low \\
     share here means the pass is waiting on DuckDB, which is already using every core, and adding R \\
     workers would oversubscribe rather than help."
  )
  invisible(.tab)
}


#' Read every chunk of one output back as a dataset
#'
#' ARROW RATHER THAN A BIND. The parties file is one row per contract per party over 1.19 million
#' contracts, which is tens of millions of rows; open_dataset() reads the schema and defers the rest,
#' so a summary is a query and never a copy in memory.
#'
#' @param .dir_chunks Directory named for the policy hash.
#' @param .name Character. Parties, Date, Money or Redact.
#' @return An arrow Dataset, or NULL where no chunk was written.
apl_dataset <- function(.dir_chunks, .name) {
  if (FALSE) {
    .dir_chunks <- .lP$Output$Chunks
    .name       <- "Parties"
  }

  # REGEXP RATHER THAN GLOB. fs::dir_ls() matches a glob against the whole path, so "*/Parties-*"
  # would require a directory level that is not there and return nothing at all -- silently, as an
  # empty dataset rather than an error.
  files_ <- fs::dir_ls(.dir_chunks, regexp = paste0(.name, "-\\d+\\.parquet$"), recurse = FALSE)
  if (length(files_) == 0L) return(NULL)
  arrow::open_dataset(sources = files_)
}


# 5. Chunk invariance ------------------------------------------------------------------------------------------------

#' The same documents, applied as one chunk and as several
#'
#' THE ONE THING THIS FILE ASSUMES, CHECKED RATHER THAN ARGUED. Every rule reads one document's spans:
#' grouping compares names within a document, the window measures from a party in the same document,
#' the ambiguous city takes a state the same contract names, the date cascade reads that contract's
#' dates. So a partition cannot change an answer.
#'
#' THE FAMILY HAS BROKEN THAT ASSUMPTION BEFORE. ent_rule()'s family-frequency gate computed a share
#' of documents from the spans it was handed, so the denominator became the chunk and 176 leading
#' tokens fell on opposite sides of the threshold at 2,546 documents per chunk. The gate is gone, and
#' the way to know it is gone is to partition the same documents two ways and compare.
#'
#' COMPARED AS SORTED FRAMES, not as objects. Chunking changes the ORDER rows arrive in and nothing
#' else, so the comparison sorts both sides on their own key columns first; a difference that survives
#' that is a difference in a value.
#'
#' @param .keys Tibble from apl_keys().
#' @param .doc_ids Character. Documents to test on.
#' @param .params The runbook's .lP$Params.
#' @param .dir_store Directory holding the family databases.
#' @param .geo List: the gazetteer lookup and candidate list.
#' @param .parts Integer. How many chunks the split arm uses.
#' @return Tibble: one row per output.
apl_check_invariance <- function(.keys, .doc_ids, .params, .dir_store, .geo, .parts = 4L) {
  if (FALSE) {
    .keys      <- tab_keys
    .doc_ids   <- tab_index$DocID[1:2000]
    .params    <- .lP$Params
    .dir_store <- .lP$Input$Store
    .geo       <- geo_once
    .parts     <- 4L
  }

  one_ <- apl_chunk_apply(
    .frame = apl_chunk_frame(.keys = .keys, .doc_ids = .doc_ids),
    .params = .params, .dir_store = .dir_store, .geo = .geo
  )

  split_ <- split(.doc_ids, ceiling(seq_along(.doc_ids) / ceiling(length(.doc_ids) / .parts)))

  many_ <- purrr::map(split_, function(.ids) {
    apl_chunk_apply(
      .frame = apl_chunk_frame(.keys = .keys, .doc_ids = .ids),
      .params = .params, .dir_store = .dir_store, .geo = .geo
    )
  })

  bind_ <- purrr::map(names(one_), function(.n) {
    purrr::map(many_, \(.m) .m[[.n]]) |> purrr::list_rbind()
  }) |>
    purrr::set_names(names(one_))

  key_ <- list(Parties = c("DocID", "PartyKey"), Date = "DocID", Money = "DocID",
               Redact = "DocID")

  purrr::map(names(one_), function(.n) {
    a_ <- dplyr::arrange(one_[[.n]], dplyr::across(dplyr::all_of(key_[[.n]])))
    b_ <- dplyr::arrange(bind_[[.n]], dplyr::across(dplyr::all_of(key_[[.n]])))
    tibble::tibble(
      Output   = .n,
      RowsOne  = nrow(a_),
      RowsMany = nrow(b_),
      Same     = isTRUE(all.equal(as.data.frame(a_), as.data.frame(b_),
                                  check.attributes = FALSE))
    )
  }) |>
    purrr::list_rbind()
}


#' The invariance check, reported
#' @param .tab Tibble from apl_check_invariance().
#' @param .n_doc Integer. Documents the check ran on.
#' @param .parts Integer. Chunks the split arm used.
#' @return Invisibly .tab.
apl_report_invariance <- function(.tab, .n_doc, .parts) {
  if (FALSE) {
    .tab   <- tab_invar
    .n_doc <- 2000L
    .parts <- 4L
  }

  cli::cli_h2("Chunking changes nothing")
  tbl_say(
    .tab   = .tab,
    .title = paste0(format(.n_doc, big.mark = ","), " documents, as one chunk and as ", .parts)
  )

  if (all(.tab$Same)) {
    cli::cli_alert_success(
      "Every output is identical under both partitions, so the chunk size is a memory and time \\
       decision and not a decision about the answer."
    )
  } else {
    cli::cli_abort(c(
      "{sum(!.tab$Same)} output{?s} {?differs/differ} under chunking.",
      "x" = "{paste(.tab$Output[!.tab$Same], collapse = ', ')}.",
      "i" = "A rule is reading a quantity computed over the spans it was handed rather than over one
             document, which is the defect ent_rule()'s family-frequency gate had. The corpus pass
             cannot be trusted until it is found."
    ))
  }
  invisible(.tab)
}


# 6. Report --------------------------------------------------------------------------------------------------------------

#' What the pass did
#' @param .tab Tibble from apl_pass().
#' @param .n_index Integer. Documents in the corpus index.
#' @return Invisibly the summary.
apl_report_pass <- function(.tab, .n_index) {
  if (FALSE) {
    .tab     <- tab_pass
    .n_index <- nrow(tab_index)
  }

  cli::cli_h2("What this render's pass did")

  if (nrow(.tab) == 0L) {
    cli::cli_alert_info("No chunk ran.")
    return(invisible(tibble::tibble()))
  }

  # THE STATUS CARRIES THE CONDITION MESSAGE, so a failure is "error: <what went wrong>" and never
  # the bare word. Testing equality against "error" would count none of them and report a clean pass
  # over a run that failed -- which is the one outcome this table exists to prevent.
  ran_ <- dplyr::filter(.tab, .data$Status == "ran")
  bad_ <- stringi::stri_startswith_fixed(.tab$Status, "error")

  out_ <- tibble::tibble(
    Item = c("Chunks in the pass",
             "Ran this render",
             "Served from the cache",
             "Failed",
             "Documents applied this render"),
    N    = c(nrow(.tab),
             nrow(ran_),
             sum(.tab$Status == "cached"),
             sum(bad_),
             sum(ran_$Docs, na.rm = TRUE))
  )

  tbl_say(.tab = out_, .title = "Chunks, and what became of them")

  if (nrow(ran_) > 0L) {
    rate_ <- sum(ran_$Docs) / max(sum(ran_$Seconds), 1e-9)
    cli::cli_alert_info(
      "{round(rate_, 1)} documents a second over the chunks that ran, which projects \\
       {round(.n_index / rate_ / 3600, 1)} hours for all \\
       {format(.n_index, big.mark = ',')} in the index. A cached chunk costs nothing and is \\
       excluded from the rate."
    )
  }

  if (any(bad_)) {
    cli::cli_alert_warning(
      "{sum(bad_)} {cli::qty(sum(bad_))}chunk{?s} failed and wrote no file, so {?it is/they are} \\
       retried on the next render. The pass continued: one bad chunk should not cost the other \\
       million documents."
    )
  }
  invisible(out_)
}


#' What each output holds, at corpus scale
#'
#' COUNTED THROUGH ARROW rather than in memory. The parties file is tens of millions of rows, and a
#' summary that had to materialise it would need more memory than the pass that produced it.
#'
#' @param .dir_chunks Directory named for the policy hash.
#' @param .n_index Integer. Documents in the corpus index.
#' @return Tibble: one row per output.
apl_table_outputs <- function(.dir_chunks, .n_index) {
  if (FALSE) {
    .dir_chunks <- .lP$Output$Chunks
    .n_index    <- nrow(tab_index)
  }

  purrr::map(c("Parties", "Date", "Money", "Redact"), function(.n) {
    ds_ <- apl_dataset(.dir_chunks = .dir_chunks, .name = .n)
    if (is.null(ds_)) {
      return(tibble::tibble(Output = .n, Rows = 0L, Docs = 0L, Cols = 0L, PctIndex = 0))
    }
    n_ <- ds_ |> dplyr::summarise(N = dplyr::n(), D = dplyr::n_distinct(.data$DocID)) |>
      dplyr::collect()
    tibble::tibble(
      Output   = .n,
      Rows     = as.integer(n_$N[[1L]]),
      Docs     = as.integer(n_$D[[1L]]),
      Cols     = length(names(ds_)),
      PctIndex = n_$D[[1L]] / pmax(.n_index, 1L)
    )
  }) |>
    purrr::list_rbind()
}


#' What each output holds
#' @param .tab Tibble from apl_table_outputs().
#' @return Invisibly .tab.
apl_report_outputs <- function(.tab) {
  if (FALSE) .tab <- tab_out

  cli::cli_h2("What the corpus pass produced")
  .tab |>
    dplyr::mutate(
      dplyr::across(c(Rows, Docs), \(.x) format(.x, big.mark = ",")),
      PctIndex = tbl_pct(.data$PctIndex)
    ) |>
    tbl_say(.title = "One row per output, over every chunk written so far")

  cli::cli_alert_info(
    "PctIndex MUST BE THE SAME ON ALL FOUR ROWS. Every chain ran on one chunk frame and every 04B \\
     apply runs from the key side, so a document reaches all four outputs or none -- a row that \\
     differs is a chain dropping documents rather than a population difference. Parties carries more \\
     ROWS than the other three because it is one row per party and they are one row per contract."
  )
  invisible(.tab)
}


#' The four column dictionaries, checked against the corpus files
#'
#' THE SAME CHECK 04B RUNS, ON THE CORPUS RATHER THAN THE SAMPLE. Each 04B document builds a
#' dictionary from a declared list and aborts where it disagrees with the file it just wrote; running
#' the same functions here is what says the corpus release has the same shape as the sample release,
#' which is the whole claim this document makes.
#'
#' SCHEMA FROM ARROW, NEVER INFERRED. names(open_dataset()) reads the parquet's own schema, so a
#' column added or renamed by a chain is seen as the file has it rather than as this document expects.
#'
#' @param .dir_chunks Directory named for the policy hash.
#' @return Tibble: one row per output.
apl_check_schema <- function(.dir_chunks) {
  if (FALSE) .dir_chunks <- .lP$Output$Chunks

  spec_ <- list(
    Parties = list(Fun = geo_dictionary_parties, Doc = "04B2"),
    Date    = list(Fun = dte_dictionary,         Doc = "04B3"),
    Money   = list(Fun = mny_dictionary,         Doc = "04B4"),
    Redact  = list(Fun = red_dictionary,         Doc = "04B5")
  )

  purrr::map(names(spec_), function(.n) {
    ds_ <- apl_dataset(.dir_chunks = .dir_chunks, .name = .n)
    if (is.null(ds_)) {
      return(tibble::tibble(Output = .n, From = spec_[[.n]]$Doc, Cols = 0L, Agrees = NA))
    }
    # An empty frame with the file's own column names is enough: the dictionary functions compare
    # names() and nothing else, so no row has to be read to run the check.
    empty_ <- ds_ |> dplyr::filter(FALSE) |> dplyr::collect()
    ok_    <- try(spec_[[.n]]$Fun(empty_), silent = TRUE)
    tibble::tibble(
      Output = .n, From = spec_[[.n]]$Doc, Cols = length(names(ds_)),
      Agrees = !inherits(ok_, "try-error")
    )
  }) |>
    purrr::list_rbind()
}


#' The schema check, reported
#' @param .tab Tibble from apl_check_schema().
#' @return Invisibly .tab.
apl_report_schema <- function(.tab) {
  if (FALSE) .tab <- tab_schema

  cli::cli_h2("The corpus files have the shape 04B released")
  tbl_say(.tab = .tab, .title = "Each output, against the dictionary its 04B document declared")

  bad_ <- dplyr::filter(.tab, !is.na(.data$Agrees), !.data$Agrees)
  if (nrow(bad_) > 0L) {
    cli::cli_abort(c(
      "{nrow(bad_)} corpus output{?s} {?does/do} not match the dictionary {?its/their} 04B document
       declared.",
      "x" = "{paste(bad_$Output, collapse = ', ')}.",
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


# 7. The corpus statistics -------------------------------------------------------------------------------------------

#' The headline numbers of every entity, at corpus scale
#'
#' THE SAME QUANTITIES 04B PUT IN ITS OWN HEADLINE TABLE, so a reader can set the two side by side and
#' see whether the corpus behaves like the sample. That comparison is the point of having run 04A at
#' all: a corpus rate far from the sample rate is either a finding about representativeness or a
#' defect in this pass, and both are worth having before anyone regresses on these columns.
#'
#' COMPUTED THROUGH ARROW. Every figure below is a group-by pushed into the parquet reader, so nothing
#' larger than the result is ever in memory.
#'
#' @param .dir_chunks Directory named for the policy hash.
#' @return Tibble: Entity, Item, Value.
apl_table_stats <- function(.dir_chunks) {
  if (FALSE) .dir_chunks <- .lP$Output$Chunks

  say_ <- function(.x) format(round(.x), big.mark = ",")
  pct_ <- function(.x) tbl_pct(.x)
  row_ <- function(.e, .i, .v) tibble::tibble(Entity = .e, Item = .i, Value = .v)

  out_ <- list()

  ds_ <- apl_dataset(.dir_chunks = .dir_chunks, .name = "Parties")
  if (!is.null(ds_)) {
    p_ <- ds_ |>
      dplyr::summarise(
        Docs   = dplyr::n_distinct(.data$DocID),
        Rows   = dplyr::n(),
        NReg   = sum(.data$PartyRole == "registrant", na.rm = TRUE),
        NMatch = sum(.data$PartyRole == "registrant" & .data$Matched, na.rm = TRUE),
        NCount = sum(.data$PartyRole == "counterparty", na.rm = TRUE),
        NState = sum(!is.na(.data$GeoState), na.rm = TRUE),
        NLaw   = sum(.data$LawSpecified == "named", na.rm = TRUE)
      ) |>
      dplyr::collect()
    out_ <- c(out_, list(
      row_("ORG", "Contracts",                     say_(p_$Docs)),
      row_("ORG", "Parties",                       say_(p_$Rows)),
      row_("ORG", "Registrant matched to EDGAR",   pct_(p_$NMatch / pmax(p_$NReg, 1))),
      row_("ORG", "Counterparties per contract",   tbl_num(p_$NCount / pmax(p_$Docs, 1))),
      row_("GPE", "Parties given a state",         pct_(p_$NState / pmax(p_$Rows, 1))),
      row_("GPE", "Rows naming a governing law",   pct_(p_$NLaw / pmax(p_$Rows, 1)))
    ))
  }

  ds_ <- apl_dataset(.dir_chunks = .dir_chunks, .name = "Date")
  if (!is.null(ds_)) {
    d_ <- ds_ |>
      dplyr::summarise(
        Docs  = dplyr::n(),
        NTerm = sum(.data$DurationSource == "term", na.rm = TRUE),
        NMax  = sum(.data$DurationSource == "maxdate", na.rm = TRUE),
        NKept = sum(!is.na(.data$DurationYears), na.rm = TRUE),
        SumY  = sum(.data$DurationYears, na.rm = TRUE)
      ) |>
      dplyr::collect()
    out_ <- c(out_, list(
      row_("DATE", "End from a stated term",       pct_(d_$NTerm / pmax(d_$Docs, 1))),
      row_("DATE", "End from the farthest date",   pct_(d_$NMax / pmax(d_$Docs, 1))),
      row_("DATE", "Duration reported",            pct_(d_$NKept / pmax(d_$Docs, 1))),
      row_("DATE", "Mean duration, years",         tbl_num(d_$SumY / pmax(d_$NKept, 1)))
    ))
  }

  ds_ <- apl_dataset(.dir_chunks = .dir_chunks, .name = "Money")
  if (!is.null(ds_)) {
    m_ <- ds_ |>
      dplyr::summarise(
        Docs  = dplyr::n(),
        NUSD  = sum(!is.na(.data$MoneyMaxUSD), na.rm = TRUE),
        NRaw  = sum(.data$NAmountsRaw, na.rm = TRUE),
        NKept = sum(.data$NAmounts, na.rm = TRUE),
        NHeld = sum(.data$NWithheldUSD + .data$NWithheldOther, na.rm = TRUE),
        NAnyH = sum((.data$NWithheldUSD + .data$NWithheldOther) > 0L, na.rm = TRUE)
      ) |>
      dplyr::collect()
    out_ <- c(out_, list(
      row_("MONEY", "Naming a USD amount",         pct_(m_$NUSD / pmax(m_$Docs, 1))),
      row_("MONEY", "Amounts kept of amounts read", pct_(m_$NKept / pmax(m_$NRaw, 1))),
      row_("MONEY", "Prices the contract withheld", say_(m_$NHeld)),
      row_("MONEY", "Contracts withholding one",   pct_(m_$NAnyH / pmax(m_$Docs, 1)))
    ))
  }

  ds_ <- apl_dataset(.dir_chunks = .dir_chunks, .name = "Redact")
  if (!is.null(ds_)) {
    r_ <- ds_ |>
      dplyr::summarise(
        Docs   = dplyr::n(),
        NAny   = sum(.data$NBracketed > 0L, na.rm = TRUE),
        NBare  = sum(.data$NRedactBare > 0L, na.rm = TRUE),
        NBrack = sum(.data$NBracketed, na.rm = TRUE),
        NAll   = sum(.data$NRedact, na.rm = TRUE)
      ) |>
      dplyr::collect()
    out_ <- c(out_, list(
      row_("REDACT", "Carrying a bracketed marker", pct_(r_$NAny / pmax(r_$Docs, 1))),
      row_("REDACT", "Carrying a bare run",         pct_(r_$NBare / pmax(r_$Docs, 1))),
      row_("REDACT", "Bracketed markers",           say_(r_$NBrack)),
      row_("REDACT", "Markers of every class",      say_(r_$NAll))
    ))
  }

  purrr::list_rbind(out_)
}


#' The corpus statistics, reported
#' @param .tab Tibble from apl_table_stats().
#' @return Invisibly .tab.
apl_report_stats <- function(.tab) {
  if (FALSE) .tab <- tab_stats

  cli::cli_h2("The corpus, entity by entity")

  if (nrow(.tab) == 0L) {
    cli::cli_alert_info("No chunk has been written, so there is nothing to summarise.")
    return(invisible(.tab))
  }

  tbl_say(.tab = .tab, .title = "The same quantities each 04B document put in its headline table")

  cli::cli_alert_info(
    "READ THESE AGAINST 04B'S OWN HEADLINE TABLES. Each row is the corpus version of a number the \\
     sample already reported, and the comparison is the point of having run 04A: a corpus rate far \\
     from the sample rate is either a finding about representativeness or a defect in this pass, and \\
     the two are told apart by reading spans rather than by reading this table."
  )
  cli::cli_alert_info(
    "EVERY FIGURE IS A GROUP-BY PUSHED INTO THE PARQUET READER, so nothing larger than the result was \\
     ever in memory -- which is what lets a table of tens of millions of party rows be summarised by \\
     the same render that wrote it."
  )
  invisible(.tab)
}


#' Coverage by contract type, for one output
#'
#' THE CLASS SPINE REACHES ONLY WHERE 03F CLASSIFIED, and that is a smaller population than this pass
#' applies to. Reported as its own row rather than dropped, because a contract with no label is a gap
#' in the classification and not a gap in the entity.
#'
#' @param .dir_chunks Directory named for the policy hash.
#' @return Tibble: one row per contract type.
apl_table_class <- function(.dir_chunks) {
  if (FALSE) .dir_chunks <- .lP$Output$Chunks

  ds_ <- apl_dataset(.dir_chunks = .dir_chunks, .name = "Date")
  if (is.null(ds_)) return(tibble::tibble())

  dte_ <- ds_ |>
    dplyr::summarise(
      Docs    = dplyr::n(),
      PctTerm = sum(.data$DurationSource == "term", na.rm = TRUE) / dplyr::n(),
      .by = Class
    ) |>
    dplyr::collect()

  mny_ <- apl_dataset(.dir_chunks = .dir_chunks, .name = "Money")
  mny_ <- if (is.null(mny_)) tibble::tibble(Class = character(0), PctUSD = numeric(0)) else {
    mny_ |>
      dplyr::summarise(
        PctUSD = sum(!is.na(.data$MoneyMaxUSD), na.rm = TRUE) / dplyr::n(), .by = Class
      ) |>
      dplyr::collect()
  }

  red_ <- apl_dataset(.dir_chunks = .dir_chunks, .name = "Redact")
  red_ <- if (is.null(red_)) tibble::tibble(Class = character(0), PctRedact = numeric(0)) else {
    red_ |>
      dplyr::summarise(
        PctRedact = sum(.data$NBracketed > 0L, na.rm = TRUE) / dplyr::n(), .by = Class
      ) |>
      dplyr::collect()
  }

  dte_ |>
    dplyr::left_join(mny_, by = dplyr::join_by(Class)) |>
    dplyr::left_join(red_, by = dplyr::join_by(Class)) |>
    dplyr::mutate(Class = dplyr::coalesce(.data$Class, "unclassified")) |>
    dplyr::arrange(dplyr::desc(.data$Docs))
}


#' Coverage by contract type, reported
#' @param .tab Tibble from apl_table_class().
#' @return Invisibly .tab.
apl_report_class <- function(.tab) {
  if (FALSE) .tab <- tab_class

  cli::cli_h2("The corpus by contract type")

  if (nrow(.tab) == 0L) {
    cli::cli_alert_info("No chunk has been written.")
    return(invisible(.tab))
  }

  .tab |>
    dplyr::mutate(
      Docs = format(.data$Docs, big.mark = ","),
      dplyr::across(dplyr::starts_with("Pct"), \(.x) tbl_pct_safe(.x))
    ) |>
    tbl_say(.title = "One row per contract type, over every chunk written so far")

  cli::cli_alert_info(
    "UNCLASSIFIED IS A ROW AND NOT A GAP. 03F's spine reaches the documents it classified and this \\
     pass reaches every document 04C finished, which is the larger set; a contract with no label is \\
     a gap in the CLASSIFICATION and its entity variables are as good as any other's."
  )
  invisible(.tab)
}


# 8. Deployment ----------------------------------------------------------------------------------------------------------

#' Bind every chunk into the four released files
#'
#' WRITTEN ONCE, FROM THE CHUNKS, AND ONLY WHEN THE PASS IS COMPLETE. A release assembled from a
#' partial pass is a file that looks finished and is not, and nothing in it says so.
#'
#' THE PARTIES FILE IS WRITTEN THROUGH ARROW rather than collected first. It is one row per contract
#' per party over 1.19 million contracts; write_dataset() streams it.
#'
#' @param .dir_chunks Directory named for the policy hash.
#' @param .paths Named list of output paths.
#' @param .complete Logical. Whether every chunk in the index has been written.
#' @return Tibble: one row per file.
apl_write <- function(.dir_chunks, .paths, .complete) {
  if (FALSE) {
    .dir_chunks <- .lP$Output$Chunks
    .paths      <- .lP$Output$Release
    .complete   <- TRUE
  }

  if (!isTRUE(.complete)) {
    cli::cli_alert_warning(
      "The pass is not complete, so no release is written. A file assembled from a partial pass \\
       looks finished and is not, and nothing in it would say so. Re-render to continue; the chunk \\
       cache resumes where this stopped."
    )
    return(tibble::tibble())
  }

  purrr::map(names(.paths), function(.n) {
    ds_ <- apl_dataset(.dir_chunks = .dir_chunks, .name = .n)
    if (is.null(ds_)) return(tibble::tibble(Output = .n, Rows = 0L, MB = 0))
    fs::dir_create(fs::path_dir(.paths[[.n]]))
    arrow::write_parquet(dplyr::collect(ds_), .paths[[.n]])
    tibble::tibble(
      Output = .n,
      Rows   = as.integer(dplyr::collect(dplyr::summarise(ds_, N = dplyr::n()))$N[[1L]]),
      MB     = round(as.numeric(fs::file_size(.paths[[.n]])) / 1024^2, 1)
    )
  }) |>
    purrr::list_rbind()
}


#' The release, reported
#' @param .tab Tibble from apl_write().
#' @return Invisibly .tab.
apl_report_write <- function(.tab) {
  if (FALSE) .tab <- tab_write

  cli::cli_h2("The release")

  if (nrow(.tab) == 0L) {
    cli::cli_alert_info("Nothing written.")
    return(invisible(.tab))
  }

  .tab |>
    dplyr::mutate(Rows = format(.data$Rows, big.mark = ",")) |>
    tbl_say(.title = "Four files, the same four 04B released")

  cli::cli_alert_info(
    "THE SAME COLUMNS IN THE SAME ORDER AS THE SAMPLE'S, written by the same 04B functions. A reader \\
     who knows one knows the other, and the dictionary check above is what says so rather than this \\
     sentence."
  )
  invisible(.tab)
}
