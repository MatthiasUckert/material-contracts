# 04D-EntityApply: apply the 04B rules to the corpus 04C extracted -----------------------------------------------------
#
# WHAT THIS FILE DOES, IN ONE PARAGRAPH
# Every rule has been settled. 04A extracted a sample and measured which family to trust, 04B built
# and scored a rule per entity against facts EDGAR recorded, 04C extracted the whole corpus. This
# stage decides nothing. It reads the rules as parameters, applies them to 1.19 million attachments
# in chunks, and writes one document-level table per entity.
#
# WHY AN APPLY STAGE HAS ALMOST NO DECISIONS OF ITS OWN
# An apply stage free to choose its own family or its own window is one whose output depends on which
# version of it last ran, and a released dataset cannot be defended on those terms. Everything that
# could vary is declared in the runbook's Configuration, printed in the render, and hashed into the
# chunk directory so a changed rule cannot be served from a stale cache.
#
# THE RULE FUNCTIONS ARE 04B'S, CALLED RATHER THAN COPIED
# Not one rule is reimplemented here. The five 04B libraries are sourced and their functions called
# on chunks of the corpus instead of on the sample. That is the project's own instruction -- shared
# tooling lives upstream and later scripts source it -- and it is the mistake this family already
# made twice: dte_term() re-implemented dateregex-v3's TERM in R and red_words() recounted what the
# register held. Both worked. Both could only ever work on the sample.
#
# THE THREE THINGS THE PREFLIGHT PROBE ESTABLISHED, AND WHICH THIS FILE IS BUILT AROUND
#
#   A CHUNK IS (KEYS INTERSECT LENS), CUT TOGETHER.
#   ent_apply() runs from the key side so a document in which nothing was found still gets a row.
#   Correct -- and it means the output is one row per KEY. Handed the corpus keys with a 2,000
#   document lens it builds 1.19 million rows to describe 2,000 documents. apl_chunk_frame() is the
#   only place a chunk is defined, so the two cannot drift.
#
#   THE TEXT WINDOW CHAINS GO QUADRATIC ABOVE ROUGHLY EIGHT THOUSAND DOCUMENTS.
#   dte_describe() and geo_context() both build one vector element per SPAN, each holding a whole
#   document, then cut a code-point window out of each. Measured: time growth over document growth
#   was 0.94 from 2,000 to 8,000 and 4.72 from 8,000 to 32,000. The chunk size is evidence, not
#   taste, and the sweep that produced it is in _Tests/04D-Preflight.R.
#
#   THE PARTY CHAIN CANNOT BE CHUNKED AT ALL, SO IT IS NOT.
#   ent_rule()'s .fam_df admits a single shared leading token as evidence of a corporate family when
#   that token opens the name of at most that share of documents, and ent_apply() computes the share
#   from the spans it is handed. Under chunking the denominator becomes the chunk: measured, 176
#   leading tokens fell on opposite sides of the threshold at 2,546 documents per chunk, 1,077 at
#   509 and 2,512 at 255. Each is a family match admitted in one chunk and refused in another on
#   identical text.
#
# WHY THE ANSWER IS ONE CALL RATHER THAN A FROZEN ARGUMENT
# The obvious fix is to compute the frequency over the corpus and pass it in, which means an
# argument ent_apply() does not have. The better fix is to notice that the party chain is the one
# chain that needs no text at all, and that the preflight measured it getting FASTER as chunks grow
# -- 110, then 289, then 518 documents per second, sublinear at both steps. So it runs once, over
# everything, through 04B1's function exactly as 04B1 calls it. The denominator is then the corpus
# because the input is the corpus, and nothing in 04B changes.
#
# A TARGET MUST NOT BE DERIVED FROM THE DATA BEING TESTED. Here that is satisfied by making the data
# and the corpus the same thing rather than by threading a parameter through a function that would
# then have two ways of being called.


# 1. Configuration and policy ------------------------------------------------------------------------------------------

#' Fingerprint the policy that produced a set of chunks
#'
#' THE CACHE KEY MUST COVER THE RULE, NOT ONLY THE CHUNK NUMBER. A chunk directory keyed on position
#' alone serves chunk 7 from the last render whatever changed in between, and the failure is silent:
#' the file is well formed, the row count is right, and half the corpus was scored under one rule and
#' half under another. Hashing the declared parameters makes a changed rule a different directory.
#'
#' @param .params The runbook's .lP$Params, or any list of declared policy.
#' @return Character. Twelve hex characters.
apl_policy_hash <- function(.params) {
  if (FALSE) .params <- .lP$Params

  substr(digest::digest(.params, algo = "sha256"), 1L, 12L)
}


#' Every rule this stage could apply, with the selected one marked
#'
#' STATED RATHER THAN CHOSEN SILENTLY. The alternatives are the argument: a reader who disagrees with
#' the selection can see what the other choice would have been and what it was measured at, without
#' opening 04B. This is the same shape as the benchmark table in 04A -- the table ranks, the
#' configuration decides, and the decision is visible beside the evidence for it.
#'
#' @param .params The runbook's .lP$Params.
#' @return Tibble: Dial, Selected, Alternatives, Why.
apl_policy_table <- function(.params) {
  if (FALSE) .params <- .lP$Params

  tibble::tibble(
    Dial = c("ORG family", "GPE family", "DATE family", "MONEY families", "REDACT family",
             "Class engine", "Cue windows", "Party window", "Duration cap", "Money filter",
             "Chunk size"),
    Selected = c(
      .params$Family$ORG, .params$Family$GPE, .params$Spec$Date$Family,
      paste(.params$Family$MONEY, collapse = " + "), .params$Family$REDACT,
      .params$ClassEngine,
      if (isTRUE(.params$Cues)) "on" else "off",
      paste0(.params$Spec$Org$Kind, " ", .params$Spec$Org$Par),
      paste0(.params$Spec$Date$CapYears, " years"),
      .params$Spec$Money$Filter,
      format(.params$ChunkSize, big.mark = ",")
    ),
    Alternatives = c(
      "matcon emits no ORG", "lexnlp: no city, no county", "lexnlp", "either alone",
      "the only family emitting a marker", "KwClassDetailed, BertClassDetailed2",
      "on: reads text, about 8 hours",
      "gap 250/500/1000, fixed 500/1000/4000", "uncapped, or 15", "none, or zeros dropped",
      "2,000 linear, 32,000 not"
    ),
    Why = c(
      "04A: lexnlp ORG has the strongest positional contrast at 4.36",
      "04B2: 4,525 distinct cities against none, address match 44.7% City",
      "04B3: matcon requires a year in the text, which the guard needs",
      "04B4: the two disagree on every co-occurrence, so both are kept",
      "04A: only matcon emits REDACT",
      "03B: the deployed all-data refit, macro-F1 0.882, no routing headroom",
      "off: money -1.7% matcon, duration 464 of 2,580 fall a rung, NO governing law",
      "04B1: P90 neighbour gap 1,343 characters",
      "04B3: SdOverCap cannot exceed one half under a cap",
      "04B4: the par filter moves lexnlp not at all and matcon by 1.7%",
      "04D preflight: growth ratio 0.94 at 8,000 and 4.72 at 32,000"
    )
  )
}


#' Print the policy, and say what a reader should check in it
#'
#' @param .params The runbook's .lP$Params.
#' @param .hash Character from apl_policy_hash().
#' @return Invisibly the policy tibble.
apl_report_policy <- function(.params, .hash) {
  if (FALSE) {
    .params <- .lP$Params
    .hash   <- hash_policy
  }

  tab_ <- apl_policy_table(.params = .params)
  tbl_say(tab_, .title = "Every dial, and what it is set to")

  cli::cli_alert_info(
    "Policy hash {(.hash)}. Chunks are cached under it, so changing any dial above starts a new \\
     directory rather than mixing two rules in one output."
  )
  invisible(tab_)
}


# 2. Input: the corpus, the keys, the anchors ---------------------------------------------------------------------------

#' The corpus document index, from a family store's corpus table
#'
#' THE POPULATION IS RESOLVED, NOT RE-DERIVED. cor_db_init() wrote Extract as the deduplication and
#' population filter already applied, so this is a filter rather than a second statement about what
#' is in the corpus -- which is why that column is stored at all.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .family Family whose corpus table is read; every family carries the same index.
#' @return Tibble: DocID, Path, HashDocument, PrimaryFiler, InPopulation.
apl_corpus_index <- function(.dir_store, .family = "matcon") {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .family    <- "matcon"
  }

  path_ <- ner_db_path(.dir = .dir_store, .family = .family)
  if (!fs::file_exists(path_)) {
    cli::cli_abort(c(
      "No {(.family)} corpus store at {.path {(path_)}}.",
      "i" = "04C writes it. Without it there is nothing to apply a rule to."
    ))
  }

  con_ <- ner_db_connect(.db_path = path_, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  if (!"corpus" %in% DBI::dbListTables(con_)) {
    cli::cli_abort("The {(.family)} store holds no corpus table; 04C did not complete its load.")
  }

  DBI::dbGetQuery(
    con_,
    "SELECT DocID, Path, HashDocument, PrimaryFiler, InPopulation
       FROM corpus WHERE Extract ORDER BY DocID"
  ) |>
    tibble::as_tibble()
}


# 3. Chunking ------------------------------------------------------------------------------------------------------------

#' Split the corpus index into chunks of a fixed size
#'
#' ORDERED BY DocID, NOT SHUFFLED. A chunk boundary that moves between renders makes a resumed pass
#' incomparable with the pass it resumed, and the whole point of caching chunks is that chunk 7 means
#' the same thing tomorrow. Sorting by an identifier no rule reads keeps the partition stable without
#' making it informative.
#'
#' @param .index Tibble from apl_corpus_index().
#' @param .size Integer. Documents per chunk.
#' @return List of tibbles, each a slice of .index, named Chunk0001 and so on.
apl_chunks <- function(.index, .size) {
  if (FALSE) {
    .index <- tab_index
    .size  <- 8000L
  }

  idx_ <- dplyr::arrange(.index, .data$DocID)
  grp_ <- ceiling(seq_len(nrow(idx_)) / as.integer(.size))
  out_ <- split(idx_, grp_)
  names(out_) <- sprintf("Chunk%04d", as.integer(names(out_)))
  out_
}


#' Stage one chunk of corpus text as the parquet the 04B loaders expect
#'
#' 04C stages the same shape for the Python engines and the column is TextRaw in both places: one
#' name for one thing, across R, Python and both stores.
#'
#' Documents whose file cannot be read are dropped rather than staged empty, which matches 04C's
#' treatment and is why a chunk's staged count can fall short of its index count. The shortfall is
#' reported per chunk rather than summed at the end, because a mount that fails halfway through a
#' nine hour pass should be visible while it is still running.
#'
#' @param .docs Tibble carrying DocID and Path.
#' @param .path_stage Where to write the staged parquet.
#' @param .workers Daemons for the read; the caller must have started them.
#' @return Tibble: DocID, TextRaw, for the documents that read.
apl_stage_text <- function(.docs, .path_stage, .workers = 1L) {
  if (FALSE) {
    .docs       <- chunks_[[1L]]
    .path_stage <- .lP$Output$Stage
    .workers    <- 12L
  }

  txt_ <- cor_read_text(.docs = .docs, .workers = .workers) |>
    dplyr::filter(!is.na(.data$Text), nzchar(trimws(.data$Text))) |>
    dplyr::transmute(.data$DocID, TextRaw = .data$Text)

  arrow::write_parquet(txt_, .path_stage)
  txt_
}


#' The stage a chunk gets when cue windows are off
#'
#' EMPTY TEXT RATHER THAN NO ARGUMENT, and the distinction is deliberate. The 04B loaders take a
#' path and cut a window out of whatever is behind it; handing them a document whose text is empty
#' produces an empty window, which is the honest representation of a cue that was not looked for.
#' The alternative -- branching inside four 04B functions on whether text was wanted -- would give
#' each of them two ways of being called and put the decision in five places instead of one.
#'
#' WHAT THIS DOES NOT DO IS FABRICATE AN ANSWER. An empty window means no cue fires, and each rule
#' then falls to the rung below it: the duration cascade to the stated term, the money filter to
#' zeros. Governing law has no rung below it, so it is dropped as a variable rather than reported
#' as absent everywhere -- see apl_chunk_apply().
#'
#' @param .docs Tibble carrying DocID.
#' @param .path_stage Where to write the staged parquet.
#' @return Tibble: DocID, TextRaw, the latter empty.
apl_stage_empty <- function(.docs, .path_stage) {
  if (FALSE) {
    .docs       <- chunks_[[1L]]
    .path_stage <- .lP$Output$Stage
  }

  txt_ <- tibble::tibble(DocID = .docs$DocID, TextRaw = "")
  arrow::write_parquet(txt_, .path_stage)
  txt_
}


#' The keys and the lengths for one chunk, cut together
#'
#' THIS FUNCTION IS THE DEFINITION OF A CHUNK, and it exists so there is only one.
#'
#' ent_apply() runs from the key side, so a document in which the family found nothing still gets a
#' row. That is correct and it is why the two sides must be cut together: handed the corpus keys with
#' an eight thousand document lens, the ORG chain builds 1.19 million rows to describe eight thousand
#' documents, and does it again for every chunk. The preflight probe found this by timing it.
#'
#' THE LENGTH COMES FROM THE REGISTER, NOT FROM THE TEXT. ent_doc_lens() reads every document to
#' compute stri_length(TextRaw); the register's nChars was measured against it on 31,981 documents
#' and agreed exactly on every one. Recomputing it is the red_words() mistake in another costume, and
#' at corpus scale it is an hour of reading to reproduce a stored column.
#'
#' @param .keys Tibble from ent_corpus_keys().
#' @param .doc_ids Character. The documents this chunk staged.
#' @return List: Keys and Lens, restricted to .doc_ids and agreeing on their document set.
apl_chunk_frame <- function(.keys, .doc_ids) {
  if (FALSE) {
    .keys    <- tab_keys
    .doc_ids <- staged_$DocID
  }

  keys_ <- dplyr::filter(.keys, .data$DocID %in% .doc_ids)
  lens_ <- keys_ |>
    dplyr::transmute(.data$DocID, DocLen = as.integer(.data$nChars)) |>
    dplyr::filter(!is.na(.data$DocLen), .data$DocLen > 0L)

  # A KEY WITHOUT A LENGTH CANNOT BE SCORED, so it leaves both sides rather than one. Otherwise the
  # party chain sees a document the span loader never joined and reports it as an empty contract.
  list(Keys = dplyr::semi_join(keys_, lens_, by = dplyr::join_by(DocID)), Lens = lens_)
}


# 4. The party stage: one call, whole corpus -----------------------------------------------------------------------------

#' Run the party chain over every document at once
#'
#' THE ONE CHAIN THAT IS NOT CHUNKED, AND THE REASON IS THE RULE RATHER THAN THE COST. ent_apply()
#' derives the leading-token document frequency from the spans it receives, and .fam_df compares
#' against it. Give it a chunk and the rule becomes a property of the partition; give it the corpus
#' and the denominator is the corpus. It is called here exactly as 04B1 calls it, with no extra
#' argument and no reimplementation of the composition inside it.
#'
#' AFFORDABLE BECAUSE IT READS NO TEXT. The preflight measured the party chain at 110, 289 and 518
#' documents per second at 2,000, 8,000 and 31,981 -- faster as the set grows, because its fixed
#' costs amortise. The spans are read per chunk and bound so the DuckDB result never has to be
#' materialised in one query, but the rule sees one table.
#'
#' @param .dir_store Corpus store directory.
#' @param .family ORG family.
#' @param .keys Tibble from ent_corpus_keys().
#' @param .chunks List from apl_chunks(); used for the READ only, never for the rule.
#' @param .rule List from ent_rule().
#' @param .spec List from ent_window_spec().
#' @param .extras Character. Extra columns the ORG table carries.
#' @param .quiet Logical. Suppress the per-chunk read line.
#' @return List: Counts (one row per document) and Roles (one row per document per name).
apl_org_whole <- function(.dir_store, .family, .keys, .chunks, .rule, .spec, .extras,
                          .quiet = FALSE) {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .family    <- .lP$Params$Family$ORG
    .keys      <- tab_keys
    .chunks    <- chunks_
    .rule      <- .lP$Params$Rule
    .spec      <- .lP$Params$Spec$Org
    .extras    <- .lP$Params$Extras$ORG
    .quiet     <- FALSE
  }

  frame_ <- apl_chunk_frame(.keys = .keys, .doc_ids = unlist(purrr::map(.chunks, "DocID")))

  spans_ <- purrr::imap(.chunks, function(.ch, .name) {
    lens_ <- dplyr::filter(frame_$Lens, .data$DocID %in% .ch$DocID)
    if (nrow(lens_) == 0L) return(NULL)

    out_ <- apl_load_org(
      .dir_store = .dir_store, .family = .family, .lens = lens_, .extras = .extras
    )
    if (!.quiet) {
      cli::cli_alert_info(
        "  {(.name)}: {format(nrow(out_), big.mark = ',')} {cli::qty(nrow(out_))} span{?s}."
      )
    }
    out_
  }) |>
    purrr::list_rbind()

  cli::cli_alert_info(
    "{format(nrow(spans_), big.mark = ',')} {cli::qty(nrow(spans_))} organisation span{?s} over \\
     {format(nrow(frame_$Lens), big.mark = ',')} {cli::qty(nrow(frame_$Lens))} document{?s}, in \\
     one call."
  )

  res_ <- ent_apply(
    .spans = spans_,
    .keys  = frame_$Keys,
    .lens  = frame_$Lens,
    .rule  = .rule,
    .spec  = .spec
  )
  list(Counts = res_$Counts, Roles = res_$Roles)
}


#' What the party stage produced, and what a reader should check in it
#'
#' @param .org List from apl_org_whole().
#' @param .n_doc Integer. Documents the keys covered.
#' @return Invisibly the counts tibble.
apl_report_org <- function(.org, .n_doc) {
  if (FALSE) {
    .org   <- res_org
    .n_doc <- nrow(tab_keys)
  }

  cli::cli_h2("The party stage")

  cli::cli_alert_info(
    "{format(nrow(.org$Roles), big.mark = ',')} {cli::qty(nrow(.org$Roles))} organisation{?s} over \\
     {format(dplyr::n_distinct(.org$Roles$DocID), big.mark = ',')} \\
     {cli::qty(dplyr::n_distinct(.org$Roles$DocID))} document{?s}; \\
     {format(nrow(.org$Counts), big.mark = ',')} {cli::qty(nrow(.org$Counts))} count row{?s}."
  )

  cli::cli_alert_info(
    "The count rows should equal the keys, because the chain runs from the key side so a contract \\
     naming no organisation still gets a row. A shortfall is a join, not a rule."
  )
  invisible(.org$Counts)
}


# 5. The four chunked chains, each calling 04B unchanged --------------------------------------------------------------

#' Load ORG spans for one chunk, with both candidate keys
#'
#' The two keys are built here rather than inside the rule because neither depends on a rule
#' parameter: rebuilding them per chunk would run the same strings through the same regex to produce
#' the identical answer, and rebuilding them per SWEEP CELL was what 04B1 removed.
#'
#' @param .dir_store Corpus store directory.
#' @param .family ORG family.
#' @param .lens Tibble: DocID, DocLen.
#' @param .extras Character. Extra columns to demand.
#' @return Tibble of ORG spans with SpanKey and CoreKey.
apl_load_org <- function(.dir_store, .family, .lens, .extras) {
  if (FALSE) {
    .dir_store <- .lP$Input$Store
    .family    <- "lexnlp"
    .lens      <- frame_$Lens
    .extras    <- .lP$Params$Extras$ORG
  }

  ent_load_entity(
    .dir_store = .dir_store,
    .family    = .family,
    .entity    = "ORG",
    .lens      = .lens,
    .extras    = .extras,
    .model     = NULL,
    .quiet     = TRUE
  ) |>
    dplyr::mutate(
      SpanKey = ent_norm_key(.x = .data$Span, .min = 1L),
      CoreKey = ent_norm_key(.x = dplyr::coalesce(.data$NameCore, .data$Span), .min = 1L)
    )
}


#' Run the four chunked rule chains over one chunk
#'
#' THE PARTY CHAIN IS NOT HERE. It ran once over the corpus, for the reason set out at the top of
#' this file, and its organisations arrive as .roles -- which is also why geography can be chunked
#' at all: geo_attach() needs the organisations, and they already exist.
#'
#' REDACT NEVER TOUCHES .stage. It needs spans and a word count, and the word count is the
#' register's rather than a recount of text this document has already read once. It sits in this
#' loop rather than in a pass of its own because at 5,000 documents per second it is free, and one
#' chunk identity is worth more than the seconds a separate pass would save.
#'
#' @param .frame List from apl_chunk_frame().
#' @param .roles Tibble. This chunk's organisations, from the party stage.
#' @param .stage Path to this chunk's staged text.
#' @param .params The runbook's .lP$Params.
#' @param .dir_store Corpus store directory.
#' @param .geo List: Lookup and Cand, loaded once by the caller.
#' @return List of four document-level tibbles: Geo, Date, Money, Redact.
apl_chunk_apply <- function(.frame, .roles, .stage, .params, .dir_store, .geo) {
  if (FALSE) {
    .frame     <- frame_
    .roles     <- roles_
    .stage     <- .lP$Output$Stage
    .params    <- .lP$Params
    .dir_store <- .lP$Input$Store
    .geo       <- geo_once
  }

  keys_ <- .frame$Keys
  lens_ <- .frame$Lens

  # REDACT -- no text; the word count is the register's, not a recount.
  marks_ <- red_load(
    .dir_store = .dir_store, .lens = lens_, .family = .params$Family$REDACT, .quiet = TRUE
  )
  out_red_ <- red_counts(
    .marks = marks_, .words = dplyr::select(keys_, "DocID", "NWords"), .keys = keys_
  )

  # MONEY -- a window either side of each amount.
  money_ <- mny_load(
    .dir_store = .dir_store, .lens = lens_, .path_text = .stage,
    .families = .params$Family$MONEY, .win = .params$Spec$Money$CueWin, .quiet = TRUE
  )
  out_mny_ <- mny_doc_table(
    .agg  = mny_aggregate(.money = money_, .keys = keys_, .spec = .params$Spec$Money),
    .keys = keys_
  )

  # DATE -- a cue window before each date, then the cascade.
  dates_ <- dte_load(
    .dir_store = .dir_store, .lens = lens_, .families = .params$Family$DATE, .quiet = TRUE
  )
  terms_ <- dte_load_terms(.dir_store = .dir_store, .lens = lens_, .quiet = TRUE)
  desc_  <- dte_describe(
    .dates = dates_, .keys = keys_, .path_text = .stage, .spec = .params$Spec$Date
  )
  out_dte_ <- dte_duration(
    .dates = desc_, .terms = terms_, .keys = keys_, .spec = .params$Spec$Date
  )

  # GPE -- resolve, attach to the organisations the party stage found, then the governing-law scan.
  raw_geo_ <- ent_load_entity(
    .dir_store = .dir_store, .family = .params$Family$GPE, .entity = "GPE", .lens = lens_,
    .extras = .params$Extras$GPE, .model = NULL, .quiet = TRUE
  ) |>
    geo_resolve(.lookup = .geo$Lookup, .family = .params$Family$GPE) |>
    dplyr::mutate(Combo = .params$Family$GPE, .before = 1L)

  geo_ <- raw_geo_ |>
    geo_city_state(.cand = .geo$Cand) |>
    geo_country()

  roles_geo_ <- geo_attach(.geo = geo_, .org = .roles, .spec = .params$Spec$Geo)
  orgtab_    <- geo_org_table(.roles = roles_geo_, .org = .roles, .keys = keys_)

  # GOVERNING LAW IS DROPPED, NOT REPORTED ABSENT. A governing-law clause sits nowhere near a party
  # -- 04B2 measured 95% of the spans its cue catches attaching to no organisation at all -- so
  # proximity cannot reach it and the cue is the only thing that identifies it. With cues off there
  # is no evidence either way, and a column saying every contract names no governing law would be a
  # false statement rather than a missing one. An empty key-only table joins as no columns.
  law_ <- if (isTRUE(.params$Cues)) {
    geo_law(
      .geo   = geo_context(.geo = geo_, .path_text = .stage, .win = .params$LawWin),
      .lens  = lens_,
      .path_text = .stage
    )
  } else {
    tibble::tibble(DocID = character(0))
  }

  out_geo_ <- geo_doc_table(
    .orgtab = orgtab_, .roles = roles_geo_, .law = law_, .keys = keys_
  )

  list(Geo = out_geo_, Date = out_dte_, Money = out_mny_, Redact = out_red_)
}


# 6. The pass ---------------------------------------------------------------------------------------------------------

#' Apply the policy to every chunk, resuming where a previous render stopped
#'
#' IDEMPOTENT BY CONSTRUCTION, WHICH IS WHAT BUYS AN HONEST DOCUMENT. Nine hours is exactly the cost
#' that tempts an eval: false, and a document that skips its own work reports numbers computed from
#' inputs that no longer exist while looking entirely normal. Instead each chunk writes five parquet
#' files into a directory named for the policy hash, and a chunk already present is skipped. The
#' first render is long; every later one takes seconds and reproduces the same output.
#'
#' THE DIRECTORY IS KEYED ON THE POLICY, NOT ON THE CHUNK NUMBER. A cache keyed on position serves
#' chunk 7 from the last render whatever changed in between, and half a corpus scored under one rule
#' and half under another is a file no check would catch.
#'
#' @param .chunks List from apl_chunks().
#' @param .keys Tibble from ent_corpus_keys().
#' @param .dir_chunks Directory for per-chunk output, already named for the policy.
#' @param .stage Path to write each chunk's staged text.
#' @param .params The runbook's .lP$Params.
#' @param .dir_store Corpus store directory.
#' @param .path_roles Parquet the party stage wrote; read lazily and filtered per chunk.
#' @param .geo List: Lookup and Cand.
#' @param .workers Daemons for the text read.
#' @return Tibble, one row per chunk: Chunk, nIndex, nStaged, Secs, Status.
apl_pass <- function(.chunks, .keys, .dir_chunks, .stage, .params, .dir_store, .path_roles, .geo,
                     .workers = 1L) {
  if (FALSE) {
    .chunks     <- chunks_
    .keys       <- tab_keys
    .dir_chunks <- .lP$Output$Chunks
    .stage      <- .lP$Output$Stage
    .params     <- .lP$Params
    .dir_store  <- .lP$Input$Store
    .path_roles <- .lP$Output$Roles
    .geo        <- geo_once
    .workers    <- 12L
  }

  fs::dir_create(.dir_chunks)
  t0_ <- Sys.time()

  purrr::imap(.chunks, function(.ch, .name) {
    path_ <- fs::path(.dir_chunks, paste0(.name, ".rds"))

    if (fs::file_exists(path_)) {
      return(tibble::tibble(
        Chunk = .name, nIndex = nrow(.ch), nStaged = NA_integer_, Secs = NA_real_,
        Status = "cached"
      ))
    }

    tc_     <- Sys.time()
    staged_ <- if (isTRUE(.params$Cues)) {
      apl_stage_text(.docs = .ch, .path_stage = .stage, .workers = .workers)
    } else {
      # NO FILE IS OPENED. With cues off nothing reads contract text, so the 736 documents 04C could
      # not read are no longer a reason to drop a document here: their spans and their register row
      # are intact and every rule that survives runs on those.
      apl_stage_empty(.docs = .ch, .path_stage = .stage)
    }

    if (nrow(staged_) == 0L) {
      # NOT AN ERROR AND NOT A SUCCESS. A chunk in which nothing read is recorded and skipped, so
      # the pass neither stops nor silently writes an empty result that later binds as real zeros.
      cli::cli_alert_warning("{(.name)}: nothing read; recorded and skipped.")
      return(tibble::tibble(
        Chunk = .name, nIndex = nrow(.ch), nStaged = 0L,
        Secs = as.numeric(difftime(Sys.time(), tc_, units = "secs")), Status = "unreadable"
      ))
    }

    frame_ <- apl_chunk_frame(.keys = .keys, .doc_ids = staged_$DocID)

    # READ LAZILY AND FILTER, so the whole organisation table never sits in memory beside the chunk
    # it is being used for. arrow pushes the predicate into the file rather than collecting first.
    roles_ <- arrow::open_dataset(sources = .path_roles) |>
      dplyr::filter(.data$DocID %in% frame_$Lens$DocID) |>
      dplyr::collect()

    out_ <- apl_chunk_apply(
      .frame = frame_, .roles = roles_, .stage = .stage, .params = .params,
      .dir_store = .dir_store, .geo = .geo
    )
    saveRDS(out_, path_)

    sec_  <- as.numeric(difftime(Sys.time(), tc_, units = "secs"))
    done_ <- which(names(.chunks) == .name)
    eta_  <- as.numeric(difftime(Sys.time(), t0_, units = "secs")) / done_ *
      (length(.chunks) - done_)

    cli::cli_alert_success(
      "{(.name)} ({done_}/{length(.chunks)}): {format(nrow(staged_), big.mark = ',')} \\
       {cli::qty(nrow(staged_))} document{?s} in {cor_duration(sec_)}; \\
       about {cor_duration(eta_)} left."
    )

    tibble::tibble(
      Chunk = .name, nIndex = nrow(.ch), nStaged = nrow(staged_), Secs = round(sec_, 1),
      Status = "done"
    )
  }) |>
    purrr::list_rbind()
}


#' Bind every written chunk into one table per chunked entity
#'
#' READ FROM DISK RATHER THAN FROM WHAT THE PASS RETURNED, so a resumed render produces exactly what
#' an uninterrupted one would. A pass that returned only the chunks it computed this time would give
#' a second render a file covering a tenth of the corpus, and nothing in the file would say so.
#'
#' @param .dir_chunks Directory holding the per-chunk files.
#' @return Named list of four tibbles; the party table comes from the party stage, not from here.
apl_bind <- function(.dir_chunks) {
  if (FALSE) .dir_chunks <- .lP$Output$Chunks

  files_ <- sort(fs::dir_ls(.dir_chunks, glob = "*.rds"))
  if (length(files_) == 0L) {
    cli::cli_abort("No chunks under {.path {as.character(.dir_chunks)}}; the pass wrote nothing.")
  }

  parts_ <- purrr::map(files_, readRDS)
  purrr::map(
    c(Geo = "Geo", Date = "Date", Money = "Money", Redact = "Redact"),
    \(.k) purrr::list_rbind(purrr::map(parts_, \(.p) .p[[.k]]))
  )
}


# 7. Report: what the pass produced ---------------------------------------------------------------------------------------

#' What the pass did, chunk by chunk
#'
#' @param .tab Tibble from apl_pass().
#' @param .n_index Integer. Attachments the index carried.
#' @return Invisibly a one-row summary.
apl_report_pass <- function(.tab, .n_index) {
  if (FALSE) {
    .tab     <- tab_pass
    .n_index <- nrow(tab_index)
  }

  cli::cli_h2("What this render's pass did")

  ran_ <- dplyr::filter(.tab, .data$Status == "done")
  out_ <- tibble::tibble(
    Chunks     = nrow(.tab),
    nDone      = sum(.tab$Status == "done"),
    nCached    = sum(.tab$Status == "cached"),
    nUnread    = sum(.tab$Status == "unreadable"),
    nStaged    = sum(.tab$nStaged, na.rm = TRUE),
    Elapsed    = cor_duration(sum(.tab$Secs, na.rm = TRUE)),
    # A RATE OVER WHAT ACTUALLY RAN. Where nothing ran this render the rate is missing rather than
    # impressive: 736 documents failing to read in a second once reported 1,180 doc/s and projected
    # the corpus at seventeen minutes.
    DocPerS    = if (nrow(ran_) == 0L) NA_real_ else {
      round(sum(ran_$nStaged, na.rm = TRUE) / max(sum(ran_$Secs, na.rm = TRUE), 1e-9), 1)
    }
  )
  tbl_say(out_, .title = "This render")

  cli::cli_alert_info(
    "Cached chunks were written by an earlier render under the same policy hash and were not \\
     recomputed. A nonzero nUnread is a mount or a path problem, not a rule one."
  )
  invisible(out_)
}


#' Every report in order
#'
#' @param .pass Tibble from apl_pass().
#' @param .tabs Named list from apl_bind().
#' @param .n_index Integer. Attachments the index carried.
#' @return Invisibly NULL.
apl_report_all <- function(.pass, .tabs, .n_index) {
  if (FALSE) {
    .pass    <- tab_pass
    .tabs    <- tab_all
    .n_index <- nrow(tab_index)
  }

  apl_report_pass(.tab = .pass, .n_index = .n_index)
  apl_report_rows(.tabs = .tabs, .n_index = .n_index)
  invisible(NULL)
}


#' Row counts against the corpus, one line per entity
#'
#' @param .tabs Named list from apl_bind().
#' @param .n_index Integer. Attachments the index carried.
#' @return Invisibly a tibble.
apl_report_rows <- function(.tabs, .n_index) {
  if (FALSE) {
    .tabs    <- tab_all
    .n_index <- nrow(tab_index)
  }

  cli::cli_h2("What was written, against what was asked for")

  out_ <- tibble::tibble(
    Entity = names(.tabs),
    Rows   = purrr::map_int(.tabs, nrow),
    Docs   = purrr::map_int(.tabs, \(.t) dplyr::n_distinct(.t$DocID)),
    Index  = .n_index
  ) |>
    dplyr::mutate(Share = tbl_pct(.data$Docs / .data$Index))

  tbl_say(out_, .title = "Rows and documents per entity")

  cli::cli_alert_info(
    "MONEY carries one row per document AND FAMILY, so its Rows is twice its Docs by design. Any \\
     other entity whose Rows exceeds its Docs is a duplicate the chunk bind introduced."
  )
  invisible(out_)
}


# 8. Deployment ----------------------------------------------------------------------------------------------------------

#' Write one document-level table per entity
#'
#' FIVE FILES RATHER THAN ONE JOINED TABLE. Each is the corpus-scale counterpart of the file 04B
#' wrote for the sample and carries the same name, so a reader who has read 04B3 knows what
#' duration_doc.parquet holds without opening it. The join, and the fan-out from 1,189,805
#' attachments to 1,462,939 registrant copies on HashDocument, belong to the release: fanning out
#' here would multiply five files by 23% each and force every later reader to undo it.
#'
#' @param .tabs Named list from apl_bind().
#' @param .paths Named list of destinations, names matching .tabs.
#' @return Invisibly a tibble naming what was written.
apl_write <- function(.tabs, .paths) {
  if (FALSE) {
    .tabs  <- tab_all
    .paths <- .lP$Output$Doc
  }

  fs::dir_create(unique(fs::path_dir(unlist(.paths))))

  purrr::iwalk(.tabs, \(.t, .k) arrow::write_parquet(.t, .paths[[.k]]))

  out_ <- tibble::tibble(
    Entity = names(.tabs),
    File   = as.character(fs::path_file(unlist(.paths[names(.tabs)]))),
    Rows   = purrr::map_int(.tabs, nrow)
  ) |>
    dplyr::mutate(
      MB = round(
        as.numeric(fs::file_size(unlist(.paths[names(.tabs)]))) / 1024^2, 1
      )
    )

  tbl_say(out_, .title = "Written by 04D")
  invisible(out_)
}
