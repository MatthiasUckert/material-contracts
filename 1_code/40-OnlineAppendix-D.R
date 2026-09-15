# ======================================================================================================================
# 40-OnlineAppendix-D.R -- Appendix D, entity extraction and the rules: the exhibits this chapter builds
# ======================================================================================================================
#
# 40-OnlineAppendix-D.qmd holds the chapter's text; _Commons/_OnlineAppendix.R holds everything the chapters share.
# This library builds the chapter's own exhibits: what each extractor found on the labelled sample, counted from the
# three span stores 04A wrote; where in a contract each extractor's candidates fall, and how far the extractors agree,
# computed from the same stores with 04A's definitions; where each contract's end date comes from, and what the content
# variables are worth under the naive and the rule-based reading, both from the release. The naive-against-rule
# coverage by category is 30's.
#
# THE PREFIX IS oad_: online appendix, chapter D.


# 1. The sample, as 30 reads it ---------------------------------------------------------------------------------------

#' The unique contracts of the descriptive sample, as 30 selects them
#'
#' The one reader of the release every build here shares, so all of them count exactly the paper's sample: a
#' contract is kept where DescSample is 1 and it is the primary copy of its attachment -- 30's Keep. The flags are
#' compared after the collect, where an integer and a logical flag compare alike; in Arrow they need not.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .cols Character. The columns to keep besides the flags.
#' @return Tibble: .cols, plus Date and Year, for 2001-2024.
oad_read_sample <- function(.path_contracts, .cols) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .cols <- c("FormType", "DateFiled")
  }
  arrow::open_dataset(sources = .path_contracts) |>
    dplyr::select(dplyr::all_of(unique(c(.cols, "DateFiled", "PrimaryFiler", "DescSample")))) |>
    dplyr::collect() |>
    dplyr::filter(
      dplyr::coalesce(as.integer(.data$PrimaryFiler), 0L) == 1L,
      dplyr::coalesce(as.integer(.data$DescSample), 0L) == 1L
    ) |>
    dplyr::mutate(
      Date = as.Date(.data$DateFiled),
      Year = as.integer(format(.data$Date, "%Y"))
    ) |>
    dplyr::filter(dplyr::between(.data$Year, 2001L, 2024L)) |>
    dplyr::select(-"PrimaryFiler", -"DescSample")
}

# 2. What each extractor found on the labelled sample ------------------------------------------------------------------

#' Spans and coverage per family, model and entity, from 04A's stores
#'
#' 04A writes one DuckDB file per family under its Output/Store, holding one table per entity, every row a span with
#' its document and its offsets; spaCy's tables also carry the model. The stores are opened read-only and counted:
#' spans, and the documents in which the family found at least one, over the documents of the sample. Coverage is
#' not a quality measure -- an extractor that tags every capitalised word reaches complete coverage -- and the text
#' says so; what it establishes is what each extractor attempts and how much it proposes.
#'
#' @param .dir_store Character. 04A's Output, holding matcon.duckdb, spacy.duckdb and lexnlp.duckdb.
#' @param .path_sample Character. 04A's sample_text.parquet, the documents every store indexes.
#' @return Tibble: Family, Model, Entity, Spans, Docs, Coverage, over nDocs documents (as an attribute-free column).
oad_data_coverage <- function(.dir_store, .path_sample) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
  }
  n_docs_ <- nrow(arrow::read_parquet(.path_sample, col_select = "DocID"))
  meta_   <- c("ledger", "manifest", "bench", "failures", "corpus_index", "sample")
  read_family_ <- function(.family) {
    p_ <- fs::path(.dir_store, paste0(.family, ".duckdb"))
    if (!fs::file_exists(p_)) cli::cli_abort("04A's store {.file {p_}} does not exist.")
    con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = as.character(p_), read_only = TRUE)
    on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
    tabs_ <- setdiff(DBI::dbListTables(con_), meta_)
    purrr::map_dfr(tabs_, \(.t) {
      cols_ <- DBI::dbListFields(con_, .t)
      if (!"DocID" %in% cols_) return(NULL)
      by_model_ <- "Model" %in% cols_
      sql_ <- if (by_model_) {
        sprintf("SELECT Model, COUNT(*) AS Spans, COUNT(DISTINCT DocID) AS Docs FROM \"%s\" GROUP BY Model", .t)
      } else {
        sprintf("SELECT '%s' AS Model, COUNT(*) AS Spans, COUNT(DISTINCT DocID) AS Docs FROM \"%s\"", .family, .t)
      }
      DBI::dbGetQuery(con_, sql_) |>
        tibble::as_tibble() |>
        dplyr::mutate(Family = .family, Entity = toupper(.t), .before = 1L)
    })
  }
  purrr::map_dfr(c("lexnlp", "spacy", "matcon"), read_family_) |>
    dplyr::mutate(
      Spans    = as.integer(.data$Spans),
      Docs     = as.integer(.data$Docs),
      Coverage = .data$Docs / n_docs_,
      nDocs    = n_docs_
    ) |>
    dplyr::arrange(.data$Family, .data$Model, .data$Entity)
}

#' Build the extractor-coverage table
#'
#' Rows are the entities the chapter discusses, in the order it discusses them; columns are the three families, spaCy
#' represented by the transformer model 04A ran. A dash marks an entity a family does not attempt.
#'
#' @param .dir_store Character. 04A's Output.
#' @param .path_sample Character. 04A's sample_text.parquet.
#' @param .spacy_model Character. The spaCy model to print, "en_core_web_trf".
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_coverage <- function(.dir_store, .path_sample, .spacy_model, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
    .spacy_model <- "en_core_web_trf"
    .dir_own     <- here::here("2_output", "40-OnlineAppendix-D", "Output")
    .force       <- FALSE
    .path_lib    <- here::here("1_code", "40-OnlineAppendix-D.R")
  }
  name_ <- "EntityCoverage"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  ins_  <- c(fs::path(.dir_store, paste0(c("lexnlp", "spacy", "matcon"), ".duckdb")), .path_lib)
  if (!all(fs::file_exists(ins_[1:3]))) return(invisible("waiting for 04A's stores"))
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  tab_ <- oad_data_coverage(
    .dir_store   = .dir_store,
    .path_sample = .path_sample
  )
  ent_ <- tibble::tribble(
    ~Entity,  ~Label,
    "ORG",    "Organisations",
    "PERSON", "Persons",
    "GPE",    "Places",
    "DATE",   "Dates",
    "TERM",   "Stated periods",
    "MONEY",  "Monetary amounts",
    "REDACT", "Redaction markers",
    "LAW",    "Governing-law clauses"
  )
  fam_ <- c("lexnlp", "spacy", "matcon")
  pick_ <- tab_ |>
    dplyr::filter(.data$Family != "spacy" | .data$Model == .spacy_model) |>
    dplyr::summarise(Spans = sum(.data$Spans), Docs = max(.data$Docs), Coverage = max(.data$Coverage),
                     .by = c("Family", "Entity"))
  cell_ <- function(.f, .e) {
    r_ <- pick_[pick_$Family == .f & pick_$Entity == .e, , drop = FALSE]
    if (nrow(r_) == 0L) return(c("--", "--"))
    c(format(r_$Spans, big.mark = ",", trim = TRUE), formatC(100 * r_$Coverage, format = "f", digits = 0L))
  }
  cells_ <- purrr::map_dfr(seq_len(nrow(ent_)), \(.i) {
    v_ <- unlist(purrr::map(fam_, \(.f) cell_(.f = .f, .e = ent_$Entity[.i])))
    tibble::tibble(Entity = ent_$Label[.i], L1 = v_[1], L2 = v_[2], S1 = v_[3], S2 = v_[4], M1 = v_[5], M2 = v_[6])
  })
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", "LexNLP", "\\%", "spaCy", "\\%", "Patterns", "\\%"),
      .spec   = c(oa_col_text(.share = 0.28), rep(oa_col_num(.mm = 17), 6L))
    ),
    .note    = paste(
      "What each extractor proposed on the", format(tab_$nDocs[1], big.mark = ","), "contracts of the labelled",
      "sample: LexNLP, spaCy (its transformer model) and the pattern and gazetteer extractors written for the",
      "database (Patterns). Under each, the number of text spans proposed and the share of contracts",
      "in which the extractor found at least one. A dash marks an entity the extractor does not attempt. Coverage is",
      "not a quality measure: an extractor that tags every capitalised word reaches complete coverage."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}


# 3. Where each contract's end date comes from --------------------------------------------------------------------------

#' Where each contract's end date comes from, and what the two durations look like
#'
#' The cascade answers from the first of four sources present, so the rungs partition the sample: a stated term, an
#' open-ended clause, which establishes that there is no end date, a future date beside a termination cue, and the
#' farthest future date. The naive measure uses the last of these alone. Both durations run from the same start, so
#' they differ only in the end they take, and the quartiles below are over the contracts each measure is defined on.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Rung, Label, N, Share, plus the quartiles of the rule-based and the naive duration.
oad_data_duration <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  con_ <- oad_read_sample(
    .path_contracts = .path_contracts,
    .cols           = c("DurationSource", "DurationDropped", "DurationYears", "NaiveYears")
  )
  rungs_ <- tibble::tribble(
    ~Rung,     ~Label,
    "term",    "A stated term",
    "open",    "An open-ended clause",
    "cue",     "A future date beside a termination cue",
    "maxdate", "The farthest future date",
    "none",    "No end established"
  )
  q_ <- function(.x, .p) {
    x_ <- .x[!is.na(.x)]
    if (length(x_) == 0L) return(NA_real_)
    unname(stats::quantile(x_, probs = .p, type = 7L))
  }
  n_all_ <- nrow(con_)
  by_rung_ <- con_ |>
    dplyr::mutate(Rung = dplyr::coalesce(.data$DurationSource, "none")) |>
    dplyr::summarise(
      N       = dplyr::n(),
      Defined = sum(!is.na(.data$DurationYears)),
      Q1      = q_(.x = .data$DurationYears, .p = 0.25),
      Med     = q_(.x = .data$DurationYears, .p = 0.50),
      Q3      = q_(.x = .data$DurationYears, .p = 0.75),
      .by     = "Rung"
    )
  out_ <- rungs_ |>
    dplyr::left_join(by_rung_, by = "Rung") |>
    dplyr::mutate(dplyr::across(c("N", "Defined"), \(.x) dplyr::coalesce(.x, 0L))) |>
    dplyr::mutate(Kind = "rung")
  # THE TWO MEASURES, each over the contracts it is defined on, so the comparison is of what each one yields
  totals_ <- tibble::tibble(
    Rung    = c("all-rule", "all-naive"),
    Label   = c("Rule-based duration", "Naive duration"),
    N       = c(n_all_, n_all_),
    Defined = c(sum(!is.na(con_$DurationYears)), sum(!is.na(con_$NaiveYears))),
    Q1      = c(q_(.x = con_$DurationYears, .p = 0.25), q_(.x = con_$NaiveYears, .p = 0.25)),
    Med     = c(q_(.x = con_$DurationYears, .p = 0.50), q_(.x = con_$NaiveYears, .p = 0.50)),
    Q3      = c(q_(.x = con_$DurationYears, .p = 0.75), q_(.x = con_$NaiveYears, .p = 0.75)),
    Kind    = "measure"
  )
  # A COMPLETE ACCOUNT OF WHAT IS MISSING. The panel is built over the contracts that have no duration, so its rows
  # sum to exactly that number; a contract whose reason the release does not record is a row of its own rather than
  # a silent remainder.
  dropped_ <- con_ |>
    dplyr::filter(is.na(.data$DurationYears)) |>
    dplyr::mutate(Reason = dplyr::coalesce(.data$DurationDropped, "unrecorded")) |>
    dplyr::mutate(Reason = dplyr::if_else(.data$Reason == "kept", "unrecorded", .data$Reason)) |>
    dplyr::count(.data$Reason, name = "N") |>
    dplyr::transmute(
      Rung    = paste0("dropped-", .data$Reason),
      Label   = dplyr::case_match(
        .data$Reason,
        "capped"     ~ "Longer than thirty years, dropped",
        "negative"   ~ "The end precedes the start, dropped",
        "no end"     ~ "No end date established",
        "unrecorded" ~ "No reason recorded",
        .default     = .data$Reason
      ),
      N       = .data$N,
      Defined = 0L,
      Q1      = NA_real_,
      Med     = NA_real_,
      Q3      = NA_real_,
      Kind    = "dropped"
    ) |>
    dplyr::arrange(dplyr::desc(.data$N))
  dplyr::bind_rows(out_, dropped_, totals_) |>
    dplyr::mutate(Share = .data$N / n_all_, Contracts = n_all_) |>
    dplyr::select("Rung", "Label", "Kind", "N", "Share", "Defined", "Q1", "Med", "Q3", "Contracts")
}

#' Build the table of duration sources
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This document's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_duration <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-D", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-D.R")
  }
  name_ <- "DurationRungs"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = c(.path_contracts, .path_lib), .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oad_data_duration(.path_contracts = .path_contracts)
  num_ <- function(.x) format(.x, big.mark = ",", trim = TRUE, scientific = FALSE)
  yrs_ <- function(.x) dplyr::if_else(is.na(.x), "--", formatC(.x, format = "f", digits = 1L))
  panel_ <- c(rung = "Which rung answered", dropped = "Why a duration is missing",
              measure = "The two measures, over the contracts each is defined on")
  cells_ <- tab_ |>
    dplyr::transmute(
      Panel   = unname(panel_[.data$Kind]),
      Label   = oa_tex_escape(.x = .data$Label),
      N       = num_(.x = .data$N),
      Share   = paste0(formatC(100 * .data$Share, format = "f", digits = 1L), "\\%"),
      Defined = dplyr::if_else(.data$Kind == "dropped", "--", num_(.x = .data$Defined)),
      Q1      = yrs_(.x = .data$Q1),
      Med     = yrs_(.x = .data$Med),
      Q3      = yrs_(.x = .data$Q3)
    )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("Source", "Contracts", "Share", "Defined", "25th", "Median", "75th"),
      .spec   = c(oa_col_text(.share = 0.25), oa_col_num(.mm = 16), oa_col_num(.mm = 12),
                  oa_col_num(.mm = 15), oa_col_num(.mm = 11), oa_col_num(.mm = 13), oa_col_num(.mm = 11))
    ),
    .note    = paste(
      "The unique contracts of the descriptive sample. The cascade takes the first source present, so the rungs",
      "partition the sample: a stated term; an open-ended clause, which establishes that the contract has no end",
      "date and therefore no duration; a future date within a termination cue's reach; and the farthest future date.",
      "Defined counts the contracts of the row for which a duration could be computed, and the quartiles are in",
      "years, over those contracts. Shares are of all contracts throughout, so the second panel accounts for every",
      "contract that lacks a duration and the first for every contract.",
      "The naive duration takes the farthest future date for every contract, and both",
      "measures run from the same start date, so they differ only in the end they take."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}

#' The counts the text of Appendix A cites, which no table prints
#'
#' Three passages name numbers that belong to no exhibit: what each quality rule flags, what the confidential
#' treatment orders cover, and how many Item 1.01 announcements the sample keeps. They are computed here, written as
#' a tibble like any other exhibit's data, and read by the numbers spec; nothing is printed.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .path_orders Character. The release's CtoOrders.parquet, one row per reference an order makes.
#' @return Tibble: Key, Value.


# 4. Where candidates fall, and how far the extractors agree: computed from 04A's stores ---------------------------------
# 04A reports positions, contrast, consensus and pairwise agreement in its runbook and saves none of them. They are
# recomputed here from the same stores, with the same definitions: every span binned by its position in its document;
# contrast as the share in the first and last decile over the share through the middle deciles; overlapping spans of
# one entity in one document merged into a mention, whatever produced them; consensus as the number of producers that
# found each mention; agreement as the Jaccard index over mentions, and exact agreement as the share of mentions both
# found whose boundaries coincide.

#' Every span of the entities compared, from the three stores, as one table in DuckDB
#'
#' Opens the three stores read-only in one connection, checks that each entity table carries a document key and
#' half-open offsets, and unions the spans of the entities asked for into a temporary table on the connection:
#' Producer, Entity, DocID, Start, Stop. spaCy is represented by one model. The connection is returned so that the
#' callers can run their SQL on the table; they close it.
#'
#' @param .dir_store Character. 04A's Output, holding matcon.duckdb, spacy.duckdb and lexnlp.duckdb.
#' @param .spacy_model Character. The spaCy model to include.
#' @param .entities Character. The entity tables to read, upper case.
#' @return A DBI connection holding the temporary table "spans".
oad_spans_open <- function(.dir_store, .spacy_model, .entities) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .spacy_model <- "en_core_web_trf"
    .entities    <- c("ORG", "PERSON", "GPE", "LAW", "DATE", "TERM", "MONEY", "REDACT")
  }
  con_ <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  fam_ <- c("lexnlp", "spacy", "matcon")
  for (f_ in fam_) {
    p_ <- fs::path(.dir_store, paste0(f_, ".duckdb"))
    if (!fs::file_exists(p_)) {
      DBI::dbDisconnect(con_, shutdown = TRUE)
      cli::cli_abort("04A's store {.file {p_}} does not exist.")
    }
    DBI::dbExecute(con_, sprintf("ATTACH '%s' AS %s (READ_ONLY)", as.character(p_), f_))
  }
  parts_ <- character(0)
  for (f_ in fam_) {
    tabs_ <- DBI::dbGetQuery(con_, sprintf(
      "SELECT table_name FROM information_schema.tables WHERE table_catalog = '%s' AND table_schema = 'main'", f_
    ))$table_name
    for (e_ in .entities) {
      t_ <- tolower(e_)
      if (!t_ %in% tabs_) next
      cols_ <- DBI::dbListFields(con_, DBI::Id(catalog = f_, schema = "main", table = t_))
      need_ <- setdiff(c("DocID", "Start", "Stop"), cols_)
      if (length(need_) > 0L) {
        DBI::dbDisconnect(con_, shutdown = TRUE)
        cli::cli_abort("{f_}.{t_} lacks {.field {need_}}; it has {.field {cols_}}.")
      }
      prod_  <- if (f_ == "spacy") paste0("spacy:", sub("^en_core_web_", "", .spacy_model)) else f_
      where_ <- if (f_ == "spacy" && "Model" %in% cols_) sprintf(" WHERE Model = '%s'", .spacy_model) else ""
      parts_ <- c(parts_, sprintf(
        "SELECT '%s' AS Producer, '%s' AS Entity, DocID, CAST(Start AS BIGINT) AS Start, CAST(Stop AS BIGINT) AS Stop
         FROM %s.main.%s%s", prod_, e_, f_, t_, where_
      ))
    }
  }
  if (length(parts_) == 0L) {
    DBI::dbDisconnect(con_, shutdown = TRUE)
    cli::cli_abort("None of the entity tables asked for exists in the three stores.")
  }
  DBI::dbExecute(con_, paste("CREATE TEMP TABLE spans AS", paste(parts_, collapse = " UNION ALL ")))
  con_
}

#' Positions and contrast: where in its document each candidate falls, per producer and entity
#'
#' @param .con A connection from oad_spans_open().
#' @param .path_sample Character. 04A's sample_text.parquet, for the length of every document.
#' @param .bins Integer. Bins per document, a multiple of ten.
#' @return List of two tibbles: positions (Producer, Entity, Bin, N, Share) and contrast (Producer, Entity,
#'   MidShare, EndShare, Contrast).
oad_data_positions <- function(.con, .path_sample, .bins = 30L) {
  if (FALSE) {
    .con         <- oad_spans_open(here::here("2_output", "04A-EntityExtract", "Output"), "en_core_web_trf", "ORG")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
    .bins        <- 30L
  }
  # THE TEXT EVERY OFFSET INDEXES: DocID and TextRaw. Its length in code points is what the offsets are relative to.
  len_ <- arrow::open_dataset(.path_sample) |>
    dplyr::select("DocID", "TextRaw") |>
    dplyr::collect() |>
    dplyr::transmute(DocID = .data$DocID, Length = nchar(.data$TextRaw, type = "chars"))
  DBI::dbWriteTable(.con, "doclen", as.data.frame(len_), temporary = TRUE, overwrite = TRUE)
  pos_ <- DBI::dbGetQuery(.con, sprintf(
    "SELECT s.Producer, s.Entity,
            LEAST(%d, 1 + CAST(FLOOR(%d * s.Start / GREATEST(d.Length, 1)) AS INTEGER)) AS Bin,
            COUNT(*) AS N
     FROM spans s JOIN doclen d USING (DocID)
     GROUP BY 1, 2, 3", .bins, .bins
  )) |>
    tibble::as_tibble() |>
    dplyr::mutate(N = as.integer(.data$N)) |>
    tidyr::complete(tidyr::nesting(Producer, Entity), Bin = seq_len(.bins), fill = list(N = 0L)) |>
    dplyr::mutate(Share = .data$N / sum(.data$N), .by = c("Producer", "Entity")) |>
    dplyr::arrange(.data$Producer, .data$Entity, .data$Bin)
  # CONTRAST: the first and last decile against the middle, the second and ninth deciles left out as shoulders
  dec_ <- .bins / 10L
  con_ <- pos_ |>
    dplyr::mutate(Zone = dplyr::case_when(
      .data$Bin <= dec_ | .data$Bin > .bins - dec_ ~ "end",
      .data$Bin <= 2L * dec_ | .data$Bin > .bins - 2L * dec_ ~ "shoulder",
      .default = "mid"
    )) |>
    dplyr::filter(.data$Zone != "shoulder") |>
    dplyr::summarise(Share = mean(.data$Share), .by = c("Producer", "Entity", "Zone")) |>
    tidyr::pivot_wider(names_from = "Zone", values_from = "Share") |>
    dplyr::transmute(Producer = .data$Producer, Entity = .data$Entity, MidShare = .data$mid, EndShare = .data$end,
                     Contrast = .data$end / .data$mid)
  list(positions = pos_, contrast = con_)
}

#' Consensus and pairwise agreement over mentions
#'
#' Overlapping spans of one entity in one document, from any producer, are merged into a mention by gaps and
#' islands; a mention's producers are whoever contributed a span to it. Consensus counts mentions by how many
#' producers found them. Pairwise agreement, for every pair of producers that attempt an entity, is the Jaccard
#' index over mentions -- both over either -- and exact agreement is the share of mentions both found on which
#' their outermost boundaries coincide.
#'
#' @param .con A connection from oad_spans_open().
#' @return List of two tibbles: consensus (Entity, NProducer, NMentions, Share, Eligible) and pairwise (Entity,
#'   ProducerA, ProducerB, Both, Exact, MentionsA, MentionsB, Jaccard, ExactShare).
oad_data_agreement <- function(.con) {
  if (FALSE) .con <- oad_spans_open(here::here("2_output", "04A-EntityExtract", "Output"), "en_core_web_trf", "GPE")
  DBI::dbExecute(.con, "
    CREATE OR REPLACE TEMP TABLE mentions AS
    WITH o AS (
      SELECT *, MAX(Stop) OVER (PARTITION BY DocID, Entity ORDER BY Start, Stop
                                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING) AS PrevMax
      FROM spans
    ),
    g AS (
      SELECT *, CASE WHEN PrevMax IS NULL OR Start >= PrevMax THEN 1 ELSE 0 END AS NewGroup FROM o
    ),
    m AS (
      SELECT *, SUM(NewGroup) OVER (PARTITION BY DocID, Entity ORDER BY Start, Stop
                                    ROWS UNBOUNDED PRECEDING) AS MentionID
      FROM g
    )
    SELECT DocID, Entity, MentionID, Producer, MIN(Start) AS Start, MAX(Stop) AS Stop
    FROM m GROUP BY 1, 2, 3, 4
  ")
  cons_ <- DBI::dbGetQuery(.con, "
    WITH per AS (SELECT Entity, DocID, MentionID, COUNT(DISTINCT Producer) AS NProducer
                 FROM mentions GROUP BY 1, 2, 3),
         elig AS (SELECT Entity, COUNT(DISTINCT Producer) AS Eligible FROM spans GROUP BY 1)
    SELECT p.Entity, p.NProducer, COUNT(*) AS NMentions, e.Eligible
    FROM per p JOIN elig e USING (Entity) GROUP BY 1, 2, 4 ORDER BY 1, 2
  ") |>
    tibble::as_tibble() |>
    dplyr::mutate(NMentions = as.integer(.data$NMentions), Eligible = as.integer(.data$Eligible)) |>
    dplyr::mutate(Share = .data$NMentions / sum(.data$NMentions), .by = "Entity")
  pair_ <- DBI::dbGetQuery(.con, "
    WITH prods AS (SELECT DISTINCT Entity, Producer FROM spans),
         pairs AS (SELECT a.Entity, a.Producer AS A, b.Producer AS B
                   FROM prods a JOIN prods b ON a.Entity = b.Entity AND a.Producer < b.Producer),
         cnt AS (SELECT Entity, Producer, COUNT(*) AS N FROM mentions GROUP BY 1, 2),
         overlap AS (SELECT x.Entity, x.Producer AS A, y.Producer AS B,
                         COUNT(*) AS Both,
                         SUM(CASE WHEN x.Start = y.Start AND x.Stop = y.Stop THEN 1 ELSE 0 END) AS Exact
                  FROM mentions x JOIN mentions y
                    ON x.Entity = y.Entity AND x.DocID = y.DocID AND x.MentionID = y.MentionID
                   AND x.Producer < y.Producer
                  GROUP BY 1, 2, 3)
    SELECT p.Entity, p.A AS ProducerA, p.B AS ProducerB,
           COALESCE(b.Both, 0) AS Both, COALESCE(b.Exact, 0) AS Exact,
           ca.N AS MentionsA, cb.N AS MentionsB
    FROM pairs p
    LEFT JOIN overlap b ON b.Entity = p.Entity AND b.A = p.A AND b.B = p.B
    JOIN cnt ca ON ca.Entity = p.Entity AND ca.Producer = p.A
    JOIN cnt cb ON cb.Entity = p.Entity AND cb.Producer = p.B
    ORDER BY 1, 2, 3
  ") |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(c("Both", "Exact", "MentionsA", "MentionsB"), as.integer),
      Jaccard    = .data$Both / (.data$MentionsA + .data$MentionsB - .data$Both),
      ExactShare = dplyr::if_else(.data$Both > 0L, .data$Exact / .data$Both, NA_real_)
    )
  list(consensus = cons_, pairwise = pair_)
}

#' The positions figure: share of candidates by position in the document, one panel per entity, one line per producer
#'
#' @param .tab Tibble: Producer, Entity, Bin, Share.
#' @return A ggplot.
oad_plot_positions <- function(.tab) {
  if (FALSE) {
    .tab <- tibble::tibble(Producer = "lexnlp", Entity = "ORG", Bin = 1:30, Share = rep(1 / 30, 30))
  }
  ent_ <- c("ORG", "PERSON", "GPE", "LAW", "DATE", "TERM", "MONEY", "REDACT")
  lab_ <- c(ORG = "Organisations", PERSON = "Persons", GPE = "Places", LAW = "Governing law", DATE = "Dates",
            TERM = "Stated periods", MONEY = "Amounts", REDACT = "Redaction markers")
  prod_ <- c("lexnlp", "spacy:trf", "matcon")
  plab_ <- c(lexnlp = "LexNLP", `spacy:trf` = "spaCy", matcon = "Patterns")
  tab_ <- .tab |>
    dplyr::filter(.data$Entity %in% ent_, .data$Producer %in% prod_) |>
    dplyr::mutate(
      Entity   = factor(unname(lab_[.data$Entity]), levels = unname(lab_)),
      Producer = factor(unname(plab_[.data$Producer]), levels = unname(plab_)),
      Position = (.data$Bin - 0.5) / max(.data$Bin)
    )
  ggplot2::ggplot(tab_, ggplot2::aes(x = .data$Position, y = .data$Share, colour = .data$Producer)) +
    ggplot2::geom_line(linewidth = 0.45) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Entity), ncol = 4L, scales = "free_y") +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks = c(0.25, 0.5, 0.75)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_colour_manual(values = plot_pal_cat(.n = 3L), drop = FALSE) +
    ggplot2::labs(x = "Position in the contract", y = "Share of the producer's candidates", colour = NULL) +
    plot_theme(.grid = "y", .legend = "bottom") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0))
}

#' The agreement figure: pairwise agreement over mentions, one panel per entity two producers reach
#'
#' @param .tab Tibble: Entity, ProducerA, ProducerB, Jaccard.
#' @return A ggplot.
oad_plot_agreement <- function(.tab) {
  if (FALSE) .tab <- tibble::tibble(Entity = "GPE", ProducerA = "lexnlp", ProducerB = "matcon", Jaccard = 0.66)
  plab_ <- c(lexnlp = "LexNLP", `spacy:trf` = "spaCy", matcon = "Patterns")
  lab_  <- c(ORG = "Organisations", GPE = "Places", DATE = "Dates", MONEY = "Amounts")
  lvl_  <- unname(plab_)
  pairs_ <- .tab |>
    dplyr::filter(.data$Entity %in% names(lab_)) |>
    dplyr::transmute(Entity = .data$Entity, A = unname(plab_[.data$ProducerA]), B = unname(plab_[.data$ProducerB]),
                     Jaccard = .data$Jaccard)
  diag_ <- pairs_ |>
    dplyr::select("Entity", "A", "B") |>
    tidyr::pivot_longer(cols = c("A", "B"), values_to = "P") |>
    dplyr::distinct(.data$Entity, .data$P) |>
    dplyr::transmute(Entity = .data$Entity, A = .data$P, B = .data$P, Jaccard = 1)
  cells_ <- dplyr::bind_rows(pairs_, diag_) |>
    dplyr::mutate(
      A      = factor(.data$A, levels = lvl_),
      B      = factor(.data$B, levels = rev(lvl_)),
      Entity = factor(unname(lab_[.data$Entity]), levels = unname(lab_)),
      Label  = formatC(.data$Jaccard, format = "f", digits = 2L),
      Dark   = .data$Jaccard > 0.5
    )
  ggplot2::ggplot(cells_, ggplot2::aes(x = .data$A, y = .data$B, fill = .data$Jaccard)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Label, colour = .data$Dark), family = .plot_font, size = 3.2,
                       show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "grey20")) +
    ggplot2::scale_fill_gradient(low = "#EEF0F4", high = plot_pal_cat(.n = 1L), limits = c(0, 1)) +
    ggplot2::facet_wrap(ggplot2::vars(.data$Entity), ncol = 2L, scales = "free") +
    ggplot2::labs(x = NULL, y = NULL, fill = "Agreement") +
    plot_theme(.grid = "none", .legend = "right") +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Build the positions and the agreement figures together, from one pass over the stores
#'
#' One connection, one spans table, both computations; written as two exhibits. The build is keyed on the three
#' stores and this library.
#'
#' @param .dir_store Character. 04A's Output, holding the three stores.
#' @param .path_sample Character. 04A's sample_text.parquet.
#' @param .spacy_model Character. The spaCy model compared.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_alignment <- function(.dir_store, .path_sample, .spacy_model, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .dir_store   <- here::here("2_output", "04A-EntityExtract", "Output")
    .path_sample <- here::here("2_output", "04A-EntityExtract", "Output", "sample_text.parquet")
    .spacy_model <- "en_core_web_trf"
    .dir_own     <- here::here("2_output", "40-OnlineAppendix-D", "Output")
    .force       <- FALSE
    .path_lib    <- here::here("1_code", "40-OnlineAppendix-D.R")
  }
  names_ <- c("EntityPositions", "EntityAgreement")
  outs_  <- unlist(purrr::map(names_, \(.n) c(
    fs::path(.dir_own, "Figures", paste0(.n, c(".pdf", ".png"))),
    fs::path(.dir_own, c("Notes", "Data"), paste0(.n, c(".tex", ".parquet")))
  )))
  ins_   <- c(fs::path(.dir_store, paste0(c("lexnlp", "spacy", "matcon"), ".duckdb")), .path_sample, .path_lib)
  if (!all(fs::file_exists(ins_[1:4]))) return(invisible("waiting for 04A's stores"))
  if (!oa_build_needed(.outputs = outs_, .inputs = ins_, .force = .force)) return(invisible("up to date"))
  con_ <- oad_spans_open(
    .dir_store   = .dir_store,
    .spacy_model = .spacy_model,
    .entities    = c("ORG", "PERSON", "GPE", "LAW", "DATE", "TERM", "MONEY", "REDACT")
  )
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
  pos_ <- oad_data_positions(.con = con_, .path_sample = .path_sample)
  agr_ <- oad_data_agreement(.con = con_)
  oa_write_exhibit(
    .name    = names_[1L],
    .lines   = NULL,
    .note    = paste(
      "Every candidate span each extractor proposed on the labelled sample, by its position in the contract: the",
      "contract is divided into thirty bins of equal length, and each line is the share of the producer's candidates",
      "for that entity that fall in each bin. LexNLP, spaCy (its transformer model) and the pattern and gazetteer",
      "extractors written for the database (Patterns); an extractor absent from a panel does not attempt that",
      "entity. A producer that spreads an entity evenly through the text draws a flat line."
    ),
    .data    = dplyr::left_join(pos_$positions, pos_$contrast, by = c("Producer", "Entity")),
    .dir_own = .dir_own
  )
  plot_save(
    .plot   = oad_plot_positions(.tab = pos_$positions),
    .name   = names_[1L],
    .dir    = fs::path(.dir_own, "Figures"),
    .height = 4.6
  )
  oa_write_exhibit(
    .name    = names_[2L],
    .lines   = NULL,
    .note    = paste(
      "Agreement between extractors on the labelled sample, for the entities two of them attempt. Overlapping spans",
      "of one entity within a contract are merged into a mention -- a place in the text where something was found",
      "-- and agreement is the Jaccard index over mentions: the share of mentions both producers found among those",
      "either found. It measures agreement about what is there; agreement about where a mention ends is a separate",
      "quantity and is not shown. The diagonal is a producer against itself."
    ),
    .data    = dplyr::bind_rows(dplyr::mutate(agr_$pairwise, Block = "pairwise"),
                                dplyr::mutate(agr_$consensus, Block = "consensus")),
    .dir_own = .dir_own
  )
  plot_save(
    .plot   = oad_plot_agreement(.tab = agr_$pairwise),
    .name   = names_[2L],
    .dir    = fs::path(.dir_own, "Figures"),
    .height = 4.4
  )
  invisible("built")
}


# 5. What the variables are worth under the naive and the rule-based reading --------------------------------------------

#' The values companion's data: by category, the typical value of each measure under both readings
#'
#' 30's contrast table counts on how many contracts each measure is defined; this one reports what it is worth on
#' them. Duration: the median years under the naive end (the farthest future date) and under the cascade. Parties:
#' the mean number of distinct organisation spellings (naive) and of registrants, co-registrants and counterparties
#' (rule). Countries and states: the mean number under the naive count (any mention outside a governing-law clause)
#' and attached to the registrant and to the counterparties by the 200-character rule, the two reported apart
#' because a country attached to both is one country. Amounts: the mean number of distinct figures read (naive)
#' and kept after the zero and par-value filters, in U.S. dollars.
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @return Tibble: Row, Kind, Level1, Class, N, and one column per measure and reading.
oad_data_values <- function(.path_contracts) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
  }
  cols_ <- c("Class", "DurationYears", "NaiveYears", "nUniSpellingsNaive", "nUniRegistrant",
             "nUniCofiler", "nUniCounterparty", "nUniCountryNaive", "nUniCountryRegistrant",
             "nUniCountryCounterparty", "nUniStateNaive", "nUniStateRegistrant", "nUniStateCounterparty",
             "nUniAmountNaive", "nUniAmountUSD")
  con_ <- oad_read_sample(
    .path_contracts = .path_contracts,
    .cols           = cols_
  ) |>
    dplyr::mutate(
      # THE TWO LEVELS, from the release's Class: "Employment: Compensation" is parent and sub; "Licenses" is both;
      # two Purchases-and-Sales sub-categories are released without their parent and are put back under it.
      Level2 = dplyr::if_else(grepl(": ", .data$Class, fixed = TRUE), sub("^.*: ", "", .data$Class), .data$Class),
      Level2 = dplyr::if_else(.data$Level2 == "Investment and Merger", "M&A", .data$Level2),
      Level1 = dplyr::case_when(
        grepl(": ", .data$Class, fixed = TRUE)            ~ sub(": .*$", "", .data$Class),
        .data$Class %in% c("R&D", "Customer / Supplier") ~ "Purchases and Sales",
        .default                                          = .data$Class
      ),
      PartiesRule = dplyr::coalesce(.data$nUniRegistrant, 0L) + dplyr::coalesce(.data$nUniCofiler, 0L) +
        dplyr::coalesce(.data$nUniCounterparty, 0L)
    )
  stat_ <- function(.d) {
    tibble::tibble(
      N            = nrow(.d),
      DurNaive     = stats::median(.d$NaiveYears, na.rm = TRUE),
      DurRule      = stats::median(.d$DurationYears, na.rm = TRUE),
      PartNaive    = mean(.d$nUniSpellingsNaive, na.rm = TRUE),
      PartRule     = mean(.d$PartiesRule, na.rm = TRUE),
      CtryNaive    = mean(.d$nUniCountryNaive, na.rm = TRUE),
      CtryReg      = mean(.d$nUniCountryRegistrant, na.rm = TRUE),
      CtryCpty     = mean(.d$nUniCountryCounterparty, na.rm = TRUE),
      StateNaive   = mean(.d$nUniStateNaive, na.rm = TRUE),
      StateReg     = mean(.d$nUniStateRegistrant, na.rm = TRUE),
      StateCpty    = mean(.d$nUniStateCounterparty, na.rm = TRUE),
      AmtNaive     = mean(.d$nUniAmountNaive, na.rm = TRUE),
      AmtRule      = mean(.d$nUniAmountUSD, na.rm = TRUE)
    )
  }
  lab_ <- dplyr::filter(con_, !is.na(.data$Class))
  # THE ROWS: parents with their sub-categories, leaf parents alone, then the total, in taxonomic order
  l1_ <- c("Financial Instruments", "Employment", "Purchases and Sales", "Licenses", "Leases", "Business Structure",
           "Other")
  rows_ <- purrr::map_dfr(l1_, \(.p) {
    d1_ <- dplyr::filter(lab_, .data$Level1 == .p)
    if (nrow(d1_) == 0L) return(NULL)
    order_ <- c("Credit", "Equity", "Compensation", "Legal", "Assets", "R&D", "Customer / Supplier",
                "Peer Agreements", "M&A")
    subs_ <- unique(d1_$Level2[!is.na(d1_$Level2) & d1_$Level2 != .p])
    subs_ <- subs_[order(match(subs_, order_))]
    top_ <- dplyr::bind_cols(tibble::tibble(Row = .p, Kind = "super", Level1 = .p, Class = NA_character_),
                             stat_(.d = d1_))
    if (length(subs_) == 0L) return(top_)
    sub_ <- purrr::map_dfr(subs_, \(.s) {
      d2_ <- dplyr::filter(d1_, .data$Level2 == .s)
      dplyr::bind_cols(tibble::tibble(Row = .s, Kind = "sub", Level1 = .p, Class = d2_$Class[1]), stat_(.d = d2_))
    })
    dplyr::bind_rows(top_, sub_)
  })
  dplyr::bind_rows(
    rows_,
    dplyr::bind_cols(tibble::tibble(Row = "Total", Kind = "total", Level1 = NA_character_, Class = NA_character_),
                     stat_(.d = con_))
  )
}

#' Build the values companion
#'
#' @param .path_contracts Character. The release's Contracts.parquet.
#' @param .dir_own Character. This chapter's output directory.
#' @param .force Logical. TRUE rebuilds regardless of the inputs.
#' @param .path_lib Character. This library.
#' @return Invisibly, the build's status.
oad_build_values <- function(.path_contracts, .dir_own, .force, .path_lib) {
  if (FALSE) {
    .path_contracts <- fs::path(
      "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractPipeline",
      "100_Data_Export",
      "Contracts.parquet"
    )
    .dir_own  <- here::here("2_output", "40-OnlineAppendix-D", "Output")
    .force    <- FALSE
    .path_lib <- here::here("1_code", "40-OnlineAppendix-D.R")
  }
  name_ <- "ContentValues"
  outs_ <- fs::path(.dir_own, c("Tables", "Notes", "Data"), paste0(name_, c(".tex", ".tex", ".parquet")))
  if (!oa_build_needed(.outputs = outs_, .inputs = c(.path_contracts, .path_lib), .force = .force)) {
    return(invisible("up to date"))
  }
  tab_ <- oad_data_values(.path_contracts = .path_contracts)
  f1_ <- function(.x) formatC(.x, format = "f", digits = 1L)
  f2_ <- function(.x) formatC(.x, format = "f", digits = 2L)
  lab_ <- dplyr::case_when(
    tab_$Kind == "total" ~ paste0("\\textbf{", oa_tex_escape(.x = tab_$Row), "}"),
    tab_$Kind == "sub"   ~ paste0("\\hspace{1em}", oa_tex_escape(.x = tab_$Row)),
    .default             = paste0("\\textbf{", oa_tex_escape(.x = tab_$Row), "}")
  )
  cells_ <- tibble::tibble(
    Row        = lab_,
    DurNaive   = f1_(.x = tab_$DurNaive),
    DurRule    = f1_(.x = tab_$DurRule),
    PartNaive  = f2_(.x = tab_$PartNaive),
    PartRule   = f2_(.x = tab_$PartRule),
    CtryNaive  = f2_(.x = tab_$CtryNaive),
    CtryReg    = f2_(.x = tab_$CtryReg),
    CtryCpty   = f2_(.x = tab_$CtryCpty),
    StateNaive = f2_(.x = tab_$StateNaive),
    StateReg   = f2_(.x = tab_$StateReg),
    StateCpty  = f2_(.x = tab_$StateCpty),
    AmtNaive   = f2_(.x = tab_$AmtNaive),
    AmtRule    = f2_(.x = tab_$AmtRule)
  )
  oa_write_exhibit(
    .name    = name_,
    .lines   = oa_frame_table(
      .tab    = cells_,
      .header = c("", "Naive", "Rule", "Naive", "Rule", "Naive", "Reg.", "Cpty.", "Naive", "Reg.", "Cpty.",
                  "Naive", "Rule"),
      .spec   = c(oa_col_text(.share = 0.20), rep(oa_col_num(.mm = 10), 12L))
    ),
    .note    = paste(
      "The companion to the coverage contrast: what each measure is worth, by category, on the unique contracts of",
      "the descriptive sample, under the naive reading of the spans and under the rule. Duration (years) is the",
      "median over the contracts on which each reading defines it: the naive end is the farthest future date, the",
      "rule's end the first source present of a stated term, an open-ended clause, a cued date and the farthest",
      "date, dropped above thirty years. Parties is the mean number per contract of distinct organisation spellings",
      "(naive) and of registrants, co-registrants and counterparties (rule). Countries and states are the mean",
      "number per contract of distinct mentions outside a governing-law clause (naive) and of those attached to",
      "the registrant (Reg.) and to the counterparties (Cpty.) by the 200-character rule, reported apart because a",
      "place attached to both is one place. Amounts is the mean number of distinct figures read with a currency",
      "marker (naive) and kept in U.S. dollars after the zero and par-value filters (rule). The column groups are,",
      "from left to right, duration, parties, countries, states and amounts."
    ),
    .data    = tab_,
    .dir_own = .dir_own
  )
  invisible("built")
}
