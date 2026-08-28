# ======================================================================================================================
# 04D-Probe.R -- the numbers 04D's chunk size and schedule are set from
# ======================================================================================================================
#
# READS THE CORPUS STORES, THE REGISTER AND THE GAZETTEER, AND WRITES NOTHING AT ALL. Every
# connection is opened read-only and closed on exit. Nothing under 2_output/ is created or touched.
#
# INSTALL THE TWO COMMONS FILES DELIVERED BESIDE THIS ONE FIRST. _Entity.R bounds
# ent_load_entity()'s SQL on the DocID range .lens covers; _NER.R lets ner_db_connect() take a thread
# count from an option. Without the first, Q3 pulls the WHOLE entity table into this session -- for
# matcon GPE, tens of millions of rows carrying two 160-character cue columns -- which is exactly what
# 04D was doing once per window, per input table, and is why three designs were rewritten to fix a
# parent that was never the problem. Section 0 checks for it and aborts rather than measuring it.
#
# WHY THESE FOUR, AND WHAT IS PREDICTED
#
#   Q1  HOW MANY SPANS DOES EACH ENTITY TABLE ACTUALLY HOLD?
#       Every size in the 04D design so far is the 4,398-document sample scaled by 270. That assumes
#       span density is flat across the corpus, and it has never been checked.
#       PREDICTED: matcon GPE between 25 and 40 million rows; lexnlp ORG between 15 and 25 million.
#
#   Q2  DOES A DocID RANGE PREDICATE SKIP, OR SCAN?
#       This decides whether 04D can run overnight. 04C queues its corpus ORDER BY DocID and ingests
#       batch by batch, so row groups should be near-perfectly DocID-clustered and DuckDB's zone maps
#       should refuse to open almost all of them.
#       PREDICTED: the bounded count returns in under a tenth of the unbounded one, on every table.
#       IF IT DOES NOT, the remedy is one CREATE TABLE ... ORDER BY DocID per table before the run.
#
#   Q3  WHAT DOES ONE REAL CHUNK COST, READ AND CHAIN?
#       The per-document costs in the handoff -- ORG 4.9ms, GPE 16.9ms -- are dplyr's fixed dispatch
#       cost rather than the rule's: every 04B chain is vectorised over a whole table and was measured
#       by calling it once per document. This measures the chain the way 04D will call it.
#       PREDICTED: the chain is single-digit seconds per 25,000-document chunk, and the READ is the
#       larger half.
#
#   Q4  DOES THE PATCHED LOADER STILL RETURN EXACTLY THE DOCUMENTS ASKED FOR?
#       The bound is a range and a chunk is contiguous, but a SCATTERED .lens must still come back
#       exact: the inner_join is the membership test and the bound only declines to fetch rows that
#       join could not have kept. Checked against an explicit IN list rather than argued.
#
# GPE IS MEASURED WITHOUT ITS DEPENDENCY, and that is the one number here that is a floor rather than
# a measurement. geo_apply() attaches places to ORG's released mentions and ORG has not run at corpus
# scale yet, so the attachment step is handed an empty frame. Everything before it -- the gazetteer
# resolution, the ambiguous-city fix, the law-clause containment -- is the bulk, and is measured.
#
# Usage:
#   source(here::here("1_code", "_Tests", "04D-Probe.R"), encoding = "UTF-8")

here::i_am("1_code/_Commons/_Initialize.R")

purrr::walk(
  .x = c("_Initialize", "_Utils", "_NER", "_Plots", "_Tables", "_Entity"),
  .f = \(.s) source(here::here("1_code", "_Commons", paste0(.s, ".R")), encoding = "UTF-8")
)

# THE FIVE RULE LIBRARIES, AFTER _Plots.R. It rebuilds its level registry empty on every source, so
# anything registering a vocabulary has to follow it -- which all five of these do.
purrr::walk(
  .x = c("04B1-Rules-ORG", "04B2-Rules-GPE", "04B3-Rules-DATE", "04B4-Rules-MONEY",
         "04B5-Rules-REDACT"),
  .f = \(.s) source(init_create_script_fun(.dir_here = here::here(), .name_script = .s),
                    encoding = "UTF-8")
)

options(cli.num_colors = 1, cli.width = 120, mc.table_mode = "console")


# 0. Configuration -------------------------------------------------------------------------------------------------------
#
# THE CORPUS STORE AND NOT THE SAMPLE'S. Every 04B document points .lP$Input$Store at 04A's Output/,
# which holds 4,398 documents, and answering these questions about that store would answer them about
# the wrong data.

.pP <- list(
  Store = fs::path(
    init_create_script_dir(.dir_here = here::here(), .name_script = "04C-EntityCorpus"), "Output"
  ),
  Register = fs::path(
    init_create_script_dir(.dir_here = here::here(), .name_script = "02B-Register"),
    "Output", "Documents.parquet"
  ),
  Lookup = here::here("contracts-extract", "src", "matcon_extract", "data", "geo_lookup.parquet"),

  # THE CHUNK 04D IS BEING SIZED FOR. Change it and re-run to read the cost curve directly; that is
  # the only reason it is a parameter rather than a constant.
  Chunk = 25000L,

  # WHERE IN THE SORTED POPULATION THE CHUNK IS TAKEN FROM. A DocID begins with an accession number
  # which begins with the filer's CIK, so the head of the corpus is the oldest registrants and is not
  # a fair draw. The middle is.
  At = 0.5,

  # Documents for the exactness check. Small: the question is whether the bound changed the answer,
  # not what the answer is.
  NCheck = 200L,

  # Threads per connection, so the probe measures a read under the setting the daemons will use.
  Threads = 2L
)

.pP$Passes <- tibble::tribble(
  ~Pass,    ~Family,  ~Needs,
  "ORG",    "lexnlp", c("ORG"),
  "GPE",    "matcon", c("GPE", "LAW"),
  "DATE",   "matcon", c("DATE", "TERM"),
  "MONEY",  "matcon", c("MONEY"),
  "REDACT", "matcon", c("REDACT", "MONEY")
)

options(mc.duckdb_threads = .pP$Threads)

cli::cli_h1("04D probe")

tab_paths <- tibble::tibble(
  Item   = c("Corpus stores", "Register", "Gazetteer"),
  Path   = purrr::map_chr(list(.pP$Store, .pP$Register, .pP$Lookup),
                          \(.p) fs::path_rel(.p, start = here::here())),
  Exists = c(fs::dir_exists(.pP$Store), fs::file_exists(.pP$Register),
             fs::file_exists(.pP$Lookup))
)

tbl_say(.tab = tab_paths, .title = "Every input resolves before anything is read")

if (any(!tab_paths$Exists)) {
  cli::cli_abort(c(
    "{sum(!tab_paths$Exists)} input{?s} {?does/do} not resolve.",
    "x" = "{paste(tab_paths$Item[!tab_paths$Exists], collapse = ', ')}."
  ))
}

# THE PATCH IS CHECKED RATHER THAN ASSUMED. Q3 without it reads the whole corpus once per pass, which
# on this machine is minutes of paging and a probe nobody would trust afterwards.
if (!any(grepl("bound_", deparse(ent_load_entity), fixed = TRUE))) {
  cli::cli_abort(c(
    "ent_load_entity() still reads unbounded: it has no bound on .lens.",
    "i" = "The _Commons/_Entity.R delivered beside this probe is the one it needs. Install it,
           restart R, and source this again."
  ))
}


# 1. Helpers -------------------------------------------------------------------------------------------------------------

#' Seconds a call takes, with the call's value returned beside it
#'
#' THE EXPRESSION IS A PROMISE AND force() IS WHERE IT RUNS, so the clock starts before the work
#' rather than after the argument was already evaluated at the call site.
#'
#' @param .expr An expression, evaluated exactly once.
#' @return List: Secs, Value.
probe_time <- function(.expr) {
  t0_  <- Sys.time()
  val_ <- force(.expr)
  list(Secs = as.numeric(difftime(Sys.time(), t0_, units = "secs")), Value = val_)
}


#' Every document a pass can reach, in DocID order
#'
#' THE SAME QUERY 04D's INDEX WILL RUN. A pass takes its population from its own family's ledger and
#' requires every entity it needs to have finished, which is what HAVING counts.
#'
#' @param .pass Character. One of the five.
#' @return Character vector of DocIDs, sorted.
probe_index <- function(.pass) {
  if (FALSE) .pass <- "GPE"

  row_  <- dplyr::filter(.pP$Passes, .data$Pass == .pass)
  need_ <- row_$Needs[[1L]]
  in_   <- paste0("'", need_, "'", collapse = ", ")

  con_ <- ner_db_connect(
    .db_path = ner_db_path(.dir = .pP$Store, .family = row_$Family[[1L]]), .read_only = TRUE
  )
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  DBI::dbGetQuery(con_, glue::glue(
    "SELECT DocID FROM runs
      WHERE Entity IN ({in_}) AND Status <> 'error'
      GROUP BY DocID
     HAVING COUNT(DISTINCT Entity) = {length(need_)}
      ORDER BY DocID"
  ))$DocID
}


#' The contiguous chunk this probe measures, taken from the middle
#' @param .ids Character vector of DocIDs, sorted.
#' @return Character vector, at most .pP$Chunk long.
probe_chunk <- function(.ids) {
  if (FALSE) .ids <- idx_all[["GPE"]]

  from_ <- max(1L, floor(length(.ids) * .pP$At))
  .ids[seq.int(from_, min(length(.ids), from_ + .pP$Chunk - 1L))]
}


# THE LEDGER IS ASKED ONCE PER PASS AND NOT ONCE PER QUESTION. A GROUP BY over 1.19 million documents
# is seconds, and every section below wants the same five answers.
cli::cli_alert_info("Reading the five populations from the ledgers.")
idx_all <- purrr::set_names(
  purrr::map(.pP$Passes$Pass, \(.p) probe_index(.pass = .p)), .pP$Passes$Pass
)

# ONE TABLE PER FAMILY AND ENTITY, because two passes share matcon's MONEY table and neither owns it.
tab_tables <- .pP$Passes |>
  tidyr::unnest_longer(col = Needs, values_to = "Entity") |>
  dplyr::distinct(.data$Family, .data$Entity) |>
  dplyr::mutate(
    Pass = purrr::map_chr(.data$Family, \(.f) .pP$Passes$Pass[.pP$Passes$Family == .f][[1L]])
  )


# 2. Q1  How many spans each entity table actually holds ------------------------------------------------------------------

cli::cli_h2("Q1  The size of every table 04D reads")

tab_size <- purrr::pmap(
  dplyr::select(tab_tables, "Family", "Entity"),
  function(Family, Entity) {
    con_ <- ner_db_connect(
      .db_path = ner_db_path(.dir = .pP$Store, .family = Family), .read_only = TRUE
    )
    on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

    if (!tolower(Entity) %in% ner_db_tables(.con = con_)) {
      return(tibble::tibble(Family = Family, Entity = Entity, Spans = NA_real_, Docs = NA_real_))
    }
    q_ <- DBI::dbGetQuery(con_, glue::glue(
      "SELECT COUNT(*) AS Spans, COUNT(DISTINCT DocID) AS Docs FROM {tolower(Entity)}"
    ))
    tibble::tibble(
      Family = Family, Entity = Entity,
      Spans = as.numeric(q_$Spans[[1L]]), Docs = as.numeric(q_$Docs[[1L]])
    )
  }
) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    PerDoc   = .data$Spans / .data$Docs,
    InChunk  = .data$PerDoc * .pP$Chunk
  )

tab_size |>
  dplyr::transmute(
    .data$Family, .data$Entity,
    Spans            = format(round(.data$Spans), big.mark = ","),
    Documents        = format(round(.data$Docs), big.mark = ","),
    `Rows per doc`   = round(.data$PerDoc, 1),
    `Rows per chunk` = format(round(.data$InChunk), big.mark = ",")
  ) |>
  tbl_say(.title = paste0(
    "Every entity table, and what a ", format(.pP$Chunk, big.mark = ","),
    "-document chunk of it weighs"
  ))


# 3. Q2  Whether a DocID range predicate skips ----------------------------------------------------------------------------

cli::cli_h2("Q2  Does the range predicate skip, or scan?")

tab_skip <- purrr::pmap(
  tab_tables,
  function(Family, Entity, Pass) {
    con_ <- ner_db_connect(
      .db_path = ner_db_path(.dir = .pP$Store, .family = Family), .read_only = TRUE
    )
    on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

    tbl_ <- tolower(Entity)
    if (!tbl_ %in% ner_db_tables(.con = con_)) return(NULL)

    band_ids_ <- probe_chunk(.ids = idx_all[[Pass]])
    lo_ <- DBI::dbQuoteString(con_, min(band_ids_))
    hi_ <- DBI::dbQuoteString(con_, max(band_ids_))

    full_ <- probe_time(DBI::dbGetQuery(con_, glue::glue("SELECT COUNT(*) AS N FROM {tbl_}")))
    bnd_  <- probe_time(DBI::dbGetQuery(con_, glue::glue(
      "SELECT COUNT(*) AS N FROM {tbl_} WHERE DocID BETWEEN {lo_} AND {hi_}"
    )))

    tibble::tibble(
      Family = Family, Entity = Entity,
      FullSecs = full_$Secs, BandSecs = bnd_$Secs,
      FullRows = as.numeric(full_$Value$N[[1L]]), BandRows = as.numeric(bnd_$Value$N[[1L]])
    )
  }
) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    RowShare  = .data$BandRows / .data$FullRows,
    TimeShare = .data$BandSecs / pmax(.data$FullSecs, 1e-9),
    # A PREDICATE THAT SKIPS COSTS FAR LESS THAN ITS SHARE OF ROWS WOULD COST TO SCAN. Reading a
    # fiftieth of the rows in a fiftieth of the time is a scan that happened to stop early; doing it
    # in a five-hundredth is zone maps declining to open row groups at all.
    Skips     = .data$TimeShare < 0.25
  )

tab_skip |>
  dplyr::transmute(
    .data$Family, .data$Entity,
    `Full scan, s`   = round(.data$FullSecs, 2),
    `Bounded, s`     = round(.data$BandSecs, 2),
    `Rows in band`   = tbl_pct(.data$RowShare),
    `Time, of full`  = tbl_pct(.data$TimeShare),
    Skips            = .data$Skips
  ) |>
  tbl_say(.title = "One chunk-wide band against the whole table")

if (all(tab_skip$Skips)) {
  cli::cli_alert_success(
    "Every table skips. A chunk read opens the row groups its band touches and no others, so 04D can
     read per chunk and the schedule is the chain's cost rather than the store's."
  )
} else {
  cli::cli_alert_danger(
    "{sum(!tab_skip$Skips)} of {nrow(tab_skip)} tables SCAN: \\
     {paste(paste0(tab_skip$Family, '/', tab_skip$Entity)[!tab_skip$Skips], collapse = ', ')}."
  )
  cli::cli_alert_info(
    "Every chunk of those would re-scan the whole table, and a dozen workers would do it at once. The
     remedy is one CREATE TABLE ... AS SELECT * FROM <t> ORDER BY DocID per table before the run:
     minutes once, and every read afterwards is a band."
  )
}


# 4. Q3  What one real chunk costs, read and chain -------------------------------------------------------------------------

cli::cli_h2("Q3  One chunk, read and chain")

geo_once <- list(
  Lookup = geo_lookup(.path_lookup = .pP$Lookup),
  Cand   = geo_candidates(.path_lookup = .pP$Lookup)
)

# THE RULES AT THE VALUES 04B RELEASED. A probe run under different dials measures a different
# document, and the point of this one is to predict tonight.
.probe_spec <- list(
  Org    = ent_window_spec(.par = 2000, .tail_share = 0.10),
  Geo    = geo_spec(.reach = 200),
  Date   = dte_spec(.start = "latest", .end = "term", .cap_years = 30),
  Money  = mny_spec(.filter = "par", .cue_win = 60L),
  Redact = red_spec(.tol = 10L)
)
.probe_rule_org <- ent_rule(.key = "core", .merge_fragments = TRUE)
.probe_cue_date <- 120L

# ORG HAS NOT RUN AT CORPUS SCALE, so GPE's attachment is handed a frame with the right columns and
# no rows. Built once, named here, so the shape it stands in for is visible rather than improvised.
.probe_mentions <- tibble::tibble(
  DocID = character(0), PartyKey = character(0), MentionStart = integer(0),
  MentionStop = integer(0), IsFirst = logical(0)
)

tab_cost <- purrr::map(.pP$Passes$Pass, function(.pass) {
  cli::cli_h3("{(.pass)}")

  ids_ <- probe_chunk(.ids = idx_all[[.pass]])

  keys_t_ <- probe_time(ent_corpus_keys(
    .path_register = .pP$Register, .path_release = NA_character_, .doc_ids = ids_, .quiet = TRUE
  ))
  keys_ <- keys_t_$Value
  lens_ <- keys_ |>
    dplyr::transmute(DocID, DocLen = as.integer(.data$nChars)) |>
    dplyr::filter(!is.na(.data$DocLen), .data$DocLen > 0L)

  # ent_filer_keys() SCANS THE REGISTER TWICE AND ORG IS THE ONLY PASS THAT NEEDS IT. Timed on its
  # own because 04D will build it ONCE PER PASS rather than once per chunk, and this is the number
  # that says what that is worth.
  fil_t_ <- if (.pass == "ORG") {
    probe_time(ent_filer_keys(.path_register = .pP$Register, .doc_ids = ids_, .quiet = TRUE))
  } else {
    list(Secs = NA_real_, Value = NULL)
  }

  read_t_ <- probe_time(switch(
    .pass,
    ORG = list(
      Spans = ent_load_org(.dir_store = .pP$Store, .lens = lens_, .family = "lexnlp",
                           .extras = c("NameCore", "LegalForm", "Description"), .quiet = TRUE)
    ),
    GPE = list(
      Spans = ent_load_entity(.dir_store = .pP$Store, .family = "matcon", .entity = "GPE",
                              .lens = lens_, .extras = ent_extras("matcon", "GPE"), .quiet = TRUE),
      Law   = ent_load_entity(.dir_store = .pP$Store, .family = "matcon", .entity = "LAW",
                              .lens = lens_, .extras = ent_extras("matcon", "LAW"), .quiet = TRUE)
    ),
    DATE = list(
      Spans = dte_describe(
        .dates = dplyr::filter(
          dte_load(.dir_store = .pP$Store, .lens = lens_, .family = "matcon", .quiet = TRUE),
          .data$Parsed
        ),
        .keys = keys_, .win = .probe_cue_date
      ),
      Terms = dte_load_terms(.dir_store = .pP$Store, .lens = lens_, .quiet = TRUE)
    ),
    MONEY = list(
      Spans = mny_load(.dir_store = .pP$Store, .lens = lens_, .family = "matcon", .quiet = TRUE)
    ),
    REDACT = list(
      Spans = red_load(.dir_store = .pP$Store, .lens = lens_, .family = "matcon", .quiet = TRUE),
      Money = mny_load(.dir_store = .pP$Store, .lens = lens_, .family = "matcon", .quiet = TRUE),
      Words = red_words(.keys = keys_, .path_text = NULL, .quiet = TRUE)
    )
  ))
  win_ <- read_t_$Value

  work_t_ <- probe_time(switch(
    .pass,
    ORG    = ent_apply(.spans = win_$Spans, .keys = keys_, .lens = lens_,
                       .rule = .probe_rule_org, .spec = .probe_spec$Org, .filers = fil_t_$Value),
    GPE    = geo_apply(.spans = win_$Spans, .law = win_$Law, .mentions = .probe_mentions,
                       .geo = geo_once, .spec = .probe_spec$Geo),
    DATE   = dte_apply(.dates = win_$Spans, .terms = win_$Terms, .keys = keys_,
                       .spec = .probe_spec$Date),
    MONEY  = mny_apply(.money = win_$Spans, .keys = keys_, .spec = .probe_spec$Money),
    REDACT = red_apply(.marks = win_$Spans, .money = win_$Money, .words = win_$Words,
                       .keys = keys_, .spec = .probe_spec$Redact)
  ))

  # EVERY NUMBER IS TAKEN BEFORE ANYTHING IS FREED. A row read off an object after rm() is the shape
  # of defect that renders clean on a sample and aborts four hours into a corpus pass.
  out_ <- tibble::tibble(
    Pass       = .pass,
    Docs       = length(ids_),
    Population = length(idx_all[[.pass]]),
    KeySecs    = keys_t_$Secs,
    FilerSecs  = fil_t_$Secs,
    ReadSecs   = read_t_$Secs,
    WorkSecs   = work_t_$Secs,
    RowsIn     = sum(purrr::map_int(win_, nrow)),
    RowsOut    = switch(
      .pass,
      ORG    = nrow(work_t_$Value$Release),
      GPE    = nrow(work_t_$Value$Release) + nrow(work_t_$Value$Law),
      DATE   = nrow(work_t_$Value$Dates) + nrow(work_t_$Value$Terms),
      MONEY  = nrow(work_t_$Value$Release),
      REDACT = nrow(work_t_$Value$Release)
    )
  )

  rm(win_, read_t_, work_t_, keys_t_, fil_t_)
  invisible(gc(verbose = FALSE))
  out_
}) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    Chunks = ceiling(.data$Population / .data$Docs),
    # KEYS AND FILERS ARE PER PASS IN 04D AND ARE DELIBERATELY NOT IN THIS. What a chunk costs is the
    # read plus the chain; the two register scans are a constant reported beside them.
    PassSecs = .data$Chunks * (.data$ReadSecs + .data$WorkSecs)
  )

tab_cost |>
  dplyr::transmute(
    .data$Pass,
    Population  = format(.data$Population, big.mark = ","),
    .data$Chunks,
    `Rows in`   = format(.data$RowsIn, big.mark = ","),
    `Rows out`  = format(.data$RowsOut, big.mark = ","),
    `Keys, s`   = round(.data$KeySecs, 1),
    `Filers, s` = round(.data$FilerSecs, 1),
    `Read, s`   = round(.data$ReadSecs, 1),
    `Chain, s`  = round(.data$WorkSecs, 1),
    `Pass, min` = round(.data$PassSecs / 60, 1)
  ) |>
  tbl_say(.title = paste0(
    "One ", format(.pP$Chunk, big.mark = ","),
    "-document chunk, taken from the middle of each population"
  ))

.probe_serial <- sum(tab_cost$PassSecs)

cli::cli_alert_info(
  "SERIAL, EVERY PASS, WHOLE CORPUS: {round(.probe_serial / 60)} minutes. At twelve workers and
   perfect division that is {round(.probe_serial / 60 / 12)}, and the truth sits between them --
   the reads contend and the last chunk of a pass finishes on its own."
)
cli::cli_alert_info(
  "GPE's chain is a FLOOR. geo_attach() was handed no mentions, because ORG has not run at corpus
   scale. On the sample it is a small share of geo_apply(), but it is not zero."
)


# 5. Q4  Whether the bound changed the answer -----------------------------------------------------------------------------

cli::cli_h2("Q4  The bounded read still returns exactly the documents asked for")

set.seed(42L)
# SCATTERED AND NOT CONTIGUOUS, DELIBERATELY. A contiguous chunk is the easy case, because there the
# band IS the chunk. The bound has to stay exact where the band spans documents the caller did not
# ask for, which is every gap in a draw like this one.
ids_chk_ <- sort(sample(idx_all[["MONEY"]], size = min(.pP$NCheck, length(idx_all[["MONEY"]]))))

lens_chk_ <- ent_corpus_keys(
  .path_register = .pP$Register, .path_release = NA_character_, .doc_ids = ids_chk_, .quiet = TRUE
) |>
  dplyr::transmute(DocID, DocLen = as.integer(.data$nChars)) |>
  dplyr::filter(!is.na(.data$DocLen), .data$DocLen > 0L)

got_ <- mny_load(.dir_store = .pP$Store, .lens = lens_chk_, .family = "matcon", .quiet = TRUE)

con_chk_ <- ner_db_connect(
  .db_path = ner_db_path(.dir = .pP$Store, .family = "matcon"), .read_only = TRUE
)
in_chk_ <- paste(DBI::dbQuoteString(con_chk_, lens_chk_$DocID), collapse = ", ")
want_n_ <- DBI::dbGetQuery(con_chk_, glue::glue(
  "SELECT COUNT(*) AS N FROM money WHERE Start IS NOT NULL AND DocID IN ({in_chk_})"
))$N[[1L]]
DBI::dbDisconnect(con_chk_, shutdown = TRUE)

tibble::tibble(
  Item = c("Documents asked for", "Documents the loader returned", "Spans the loader returned",
           "Spans an explicit IN list counts", "Documents outside the request"),
  N    = c(nrow(lens_chk_), dplyr::n_distinct(got_$DocID), nrow(got_), as.numeric(want_n_),
           sum(!got_$DocID %in% lens_chk_$DocID))
) |>
  tbl_say(.title = "A scattered draw, read through the bound and counted independently")

if (nrow(got_) == want_n_ && all(got_$DocID %in% lens_chk_$DocID)) {
  cli::cli_alert_success(
    "The bound changed nothing. It is a read optimisation with an identity behind it: the inner_join
     on .lens is the membership test, and it always was."
  )
} else {
  cli::cli_abort(c(
    "The bounded read does not agree with an explicit IN list.",
    "x" = "The loader returned {nrow(got_)} spans against {want_n_} counted.",
    "i" = "Do not run 04D. Put the previous _Entity.R back and report this."
  ))
}


cli::cli_h2("Probe complete")
cli::cli_alert_info(
  "Q2 decides whether the tables need re-ordering before tonight; Q3 sets the chunk size and the
   worker count; Q1 and Q4 say whether either number can be trusted."
)
