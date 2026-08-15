# _Store.R: the candidate store, one table per label -----------------------------------------------------------------
#
# WHAT THIS IS
# The store layer, rewritten so each label has its own table and can carry its own typed columns.
# It supersedes three functions in _NER.R -- ner_db_init(), ner_db_append() and ner_db_clear() --
# and adds two: ner_db_split(), which converts an existing flat store, and ner_db_ensure_label(),
# which creates a table for a label nobody declared.
#
# SOURCE THIS AFTER _Commons/_NER.R. R redefines the three functions and the rest of _NER.R is
# untouched. It is a separate file so the change can be run, read and reverted before anything is
# cut from a 1,833-line shared file; once it is trusted, move these functions into _NER.R section 2
# and delete this one. It is not meant to live here permanently.
#
# WHY PER-LABEL TABLES
# A single `candidates` table forces every engine to the same eight columns, which is why LexNLP's
# company type, parsed date, amount and currency had nowhere to go and were discarded at the seam
# for the whole life of the project. The extension axis is the LABEL, not the engine: LegalForm
# belongs to ORG whoever found it, Amount to MONEY, DateValue to DATE. Six tables, each with the
# core plus the extras that label admits.
#
#   org     core + LegalForm, LegalFormLabel, Description, NameCore
#   gpe     core + GeoName, GeoAlias, GeoCategory, Iso2, Iso3, GeoId, GeoPriority
#   date    core + DateValue, DateScore
#   money   core + Amount, Currency
#   person  core
#   redact  core
#
# THE CORE IS UNCHANGED and `candidates` survives as a VIEW over the union of it. Every reader in
# the family -- ner_db_missing(), 04A's overview, alignment, contrast and family-consensus layers --
# queries `candidates` and needs no edit at all. That is the whole reason for the view: the schema
# change is invisible to everything that only reads the core.
#
# EXTRAS ARE SELECTED, NOT REQUIRED. An extractor's parquet may carry a column no table declares, or
# omit one that is declared; ingest takes the intersection and nulls the rest. So spaCy's core-only
# output and LexNLP's wide output go through the same path, and a label that gains a field later
# needs a DDL line and nothing else.
#
# THE LEDGER IS UNTOUCHED. runs is already keyed on (DocID, Engine, Model, Label), which is exactly
# what per-label routing needs, and the resumability logic built on it in August works as written.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; pure ASCII.


# 1. The schema ------------------------------------------------------------------------------------------------------

.store_core <- c("DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model")

# Label -> (table, extras). The DDL type comes with the name because a column's type is part of its
# meaning: an Amount stored as text is a string that looks like a number, and DuckDB is where that
# should be settled rather than in whichever consumer casts it first.
#
# TWO NAMES ARE DELIBERATELY NOT LexNLP'S OWN. TypeAbbr becomes LegalForm because "NA" is LexNLP's
# abbreviation for National Association and R prints that identically to a missing value -- the
# rename removes a class of silent error that no downstream check could catch. And Name becomes
# NameCore for ORG and GeoName for GPE, because one column called Name meaning two different things
# in two tables is the kind of thing a union view makes indistinguishable.
.store_schema <- list(
  ORG = list(
    table  = "org",
    extras = c(NameCore       = "VARCHAR",
               LegalForm      = "VARCHAR",
               LegalFormFull  = "VARCHAR",
               LegalFormLabel = "VARCHAR",
               Description    = "VARCHAR")
  ),
  PERSON = list(table = "person", extras = character(0)),
  GPE = list(
    table  = "gpe",
    extras = c(GeoName     = "VARCHAR",
               GeoAlias    = "VARCHAR",
               GeoCategory = "VARCHAR",
               Iso2        = "VARCHAR",
               Iso3        = "VARCHAR",
               GeoId       = "BIGINT",
               GeoPriority = "BIGINT")
  ),
  DATE = list(
    table  = "date",
    extras = c(DateValue = "DATE",
               DateScore = "DOUBLE")
  ),
  MONEY = list(
    table  = "money",
    extras = c(Amount   = "DOUBLE",
               Currency = "VARCHAR")
  ),
  REDACT = list(table = "redact", extras = character(0))
)

# STORE COLUMN -> EXTRACTOR COLUMN, PER LABEL. Keyed this way round and split by label because the
# mapping is not a function of the column name alone: LexNLP emits `Name` for both ORG and GPE and
# it means a company in one and a place in the other, so one flat lookup cannot serve both. An
# earlier version tried, indexed a named vector by names that mostly do not exist, and leaked an
# empty source column into the generated SQL -- which failed as a parser error a long way from the
# lookup that caused it.
#
# A label absent here declares no extras and takes the core columns only, which is spaCy's case for
# every label it emits.
.store_rename <- list(
  ORG = c(
    NameCore       = "Name",
    LegalForm      = "TypeAbbr",       # LexNLP's company_type_abbr, e.g. CORP, INC, LLC
    LegalFormFull  = "TypeFull",       # the spelled-out form the abbreviation stands for
    LegalFormLabel = "TypeLabel",
    Description    = "Description"
  ),
  GPE = c(
    GeoName     = "Name",            # the RESOLVED entity, not the matched alias
    GeoAlias    = "Alias",
    GeoCategory = "EntityCategory",
    Iso2        = "Iso2",
    Iso3        = "Iso3",
    GeoId       = "EntityId",
    GeoPriority = "EntityPriority"
  ),
  DATE = c(
    DateValue = "DateValue",
    DateScore = "Score"
  ),
  MONEY = c(
    Amount   = "Amount",
    Currency = "Currency"
  )
)


#' The table a label lives in
#'
#' @param .label Character vector of unified labels.
#' @return Character vector of table names, lower-cased label where undeclared.
store_table <- function(.label) {
  purrr::map_chr(.label, function(.l) {
    if (!is.null(.store_schema[[.l]])) .store_schema[[.l]]$table else tolower(.l)
  })
}


#' Which declared extras this parquet can actually fill
#'
#' THE INTERSECTION, NOT THE DECLARED SET. A parquet that omits a declared extra still ingests and
#' the column reads NULL, which is what it is; a parquet carrying a column no table declares has it
#' dropped rather than causing an abort, because the store's schema is the store's decision and an
#' extractor is free to emit more than the store wants.
#'
#' @param .label One unified label.
#' @param .src_cols Character vector of columns present in the staging parquet.
#' @return Named character vector: store column name to source column name. Empty where none.
store_source_map <- function(.label, .src_cols) {
  if (FALSE) {
    .label    <- "DATE"
    .src_cols <- c("DocID", "Start", "Stop", "Span", "Label", "LabelRaw", "Engine", "Model")
  }
  map_ <- .store_rename[[.label]]
  if (is.null(map_) || length(map_) == 0L) return(character(0))
  map_ <- map_[names(map_) %in% names(store_extras(.label))]
  map_[map_ %in% .src_cols]
}


#' The store's base tables, never its views
#'
#' DBI::dbListTables() RETURNS VIEWS TOO, and that is the whole reason this exists. The view built
#' below is called `candidates`; asked for "tables", DBI hands it back, the catch-all for
#' undeclared labels treats it as one, and the next rebuild writes
#' `CREATE VIEW candidates AS ... UNION ALL SELECT ... FROM candidates`. DuckDB then refuses every
#' query against it with an infinite-recursion binder error -- and because the broken definition is
#' persisted, it survives into the next session.
#'
#' @param .con Live connection.
#' @return Character vector of base table names.
store_tables <- function(.con) {
  DBI::dbGetQuery(.con, paste0(
    "SELECT table_name FROM information_schema.tables WHERE table_type = 'BASE TABLE'"
  ))$table_name
}


#' The extras a label's table declares
#'
#' @param .label One unified label.
#' @return Named character vector of column name to DDL type; empty where none.
store_extras <- function(.label) {
  if (is.null(.store_schema[[.label]])) return(character(0))
  .store_schema[[.label]]$extras
}


# 2. Creating and migrating ------------------------------------------------------------------------------------------

#' Create one label's table if it is absent
#'
#' CALLED ON INGEST, NOT ONLY AT INIT. spaCy's label map can emit PERCENT or AMOUNT depending on the
#' policy, and it has before. A label with no declared table gets a core-only one rather than
#' failing the append: refusing would lose a real extraction over a naming gap, and a core-only
#' table loses nothing, because a label nobody declared has no extras to lose.
#'
#' @param .con Live connection.
#' @param .label One unified label.
#' @param .quiet Logical. Suppress the creation message.
#' @return Invisibly, the table name.
ner_db_ensure_label <- function(.con, .label, .quiet = FALSE) {
  if (FALSE) {
    .con   <- con
    .label <- "ORG"
    .quiet <- FALSE
  }

  tbl_    <- store_table(.label)
  extras_ <- store_extras(.label)
  known_  <- store_tables(.con = .con)

  if (!tbl_ %in% known_) {
    cols_ <- c(
      "DocID    VARCHAR NOT NULL",
      "Start    BIGINT  NOT NULL",
      "Stop     BIGINT  NOT NULL",
      "Span     VARCHAR NOT NULL",
      "LabelRaw VARCHAR",
      "Engine   VARCHAR NOT NULL",
      "Model    VARCHAR NOT NULL",
      if (length(extras_)) paste(names(extras_), unname(extras_)) else NULL
    )
    DBI::dbExecute(.con, paste0("CREATE TABLE ", tbl_, " (", paste(cols_, collapse = ", "), ")"))
    if (!.quiet && is.null(.store_schema[[.label]])) {
      cli::cli_alert_warning(
        "Label {.val {(.label)}} is not declared in .store_schema; created {.field {(tbl_)}} \\
         with the core columns only."
      )
    }
    return(invisible(tbl_))
  }

  # A declared extra added after the table was built. Adding it is safe -- existing rows read NULL,
  # which is exactly what they are -- and refusing would mean deleting the store to gain a column.
  have_ <- names(DBI::dbGetQuery(.con, paste0("SELECT * FROM ", tbl_, " LIMIT 0")))
  add_  <- setdiff(names(extras_), have_)
  for (.k in add_) {
    DBI::dbExecute(.con, paste0("ALTER TABLE ", tbl_, " ADD COLUMN ", .k, " ", extras_[[.k]]))
    if (!.quiet) cli::cli_alert_info("Added column {.field {(.k)}} to {.field {(tbl_)}}.")
  }
  invisible(tbl_)
}


#' Rebuild the `candidates` view over whichever label tables exist
#'
#' Label is a CONSTANT in each branch rather than a stored column: the table already says which
#' label its rows carry, and a stored copy is a second source of truth that can disagree with the
#' first.
#'
#' @param .con Live connection.
#' @return Invisibly, the number of tables in the view.
ner_db_view <- function(.con) {
  if (FALSE) .con <- con

  # BASE TABLES ONLY. Reading dbListTables() here put the view into its own definition; see
  # store_tables() for what that costs.
  tabs_ <- setdiff(store_tables(.con = .con), "runs")
  if (length(tabs_) == 0L) return(invisible(0L))

  # Table -> label. A declared label uses its declared name; anything else is a label nobody
  # declared, whose table was created lower-cased, so upper-casing recovers it.
  declared_ <- names(.store_schema)
  lab_of_   <- purrr::set_names(declared_, store_table(declared_))
  labs_     <- purrr::map_chr(tabs_, function(.t) {
    if (.t %in% names(lab_of_)) unname(lab_of_[[.t]]) else toupper(.t)
  })

  parts_ <- purrr::map2_chr(labs_, tabs_, function(.l, .t) {
    paste0("SELECT DocID, Start, Stop, Span, '", .l, "' AS Label, LabelRaw, Engine, Model FROM ",
           .t)
  })
  DBI::dbExecute(.con, paste0("CREATE OR REPLACE VIEW candidates AS ",
                              paste(parts_, collapse = " UNION ALL ")))
  invisible(length(parts_))
}


#' Open the candidate store, creating its schema if absent
#'
#' Supersedes the flat-table version in _NER.R. Idempotent, so a caller never has to know whether
#' the store exists; the view is rebuilt every time because a table added since the last call would
#' otherwise be invisible to every reader.
#'
#' @param .db_path Character. Path to the DuckDB file.
#' @param .quiet Logical. Suppress the creation message.
#' @return A live DBI connection; the caller disconnects with dbDisconnect(con, shutdown = TRUE).
ner_db_init <- function(.db_path, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Store$NerDB
    .quiet   <- FALSE
  }

  exists_ <- fs::file_exists(.db_path)
  fs::dir_create(fs::path_dir(.db_path))
  con_ <- ner_db_connect(.db_path = .db_path, .read_only = FALSE)

  # The ledger, unchanged. LABEL IS IN THE KEY and its absence was the most expensive defect in this
  # project: a ledger keyed only on the combination cannot tell "spaCy has seen this document" from
  # "spaCy has seen this document AND WAS ASKED FOR PERSON", so adding a label to the policy marks
  # every combination already done and the store reports itself complete under a set it was never
  # built with.
  DBI::dbExecute(con_, "
    CREATE TABLE IF NOT EXISTS runs (
      DocID     VARCHAR    NOT NULL,
      Engine    VARCHAR    NOT NULL,
      Model     VARCHAR    NOT NULL,
      Label     VARCHAR    NOT NULL,
      Status    VARCHAR    NOT NULL CHECK (Status IN ('success', 'nohit', 'timeout')),
      CreatedAt TIMESTAMP  NOT NULL,
      UNIQUE (DocID, Engine, Model, Label)
    );
  ")

  cols_ <- names(DBI::dbGetQuery(con_, "SELECT * FROM runs LIMIT 0"))
  if (!"Label" %in% cols_) {
    DBI::dbDisconnect(con_, shutdown = TRUE)
    cli::cli_abort(c(
      "Store at {.path {(.db_path)}} predates the label-level ledger.",
      "x" = "Its {.field runs} table has no {.field Label} column.",
      "i" = "Delete the store and its manifest, then re-extract."
    ))
  }

  # A FLAT candidates TABLE FROM THE OLD SCHEMA IS REFUSED RATHER THAN READ AROUND. The view this
  # file creates has the same name, so a leftover table would be shadowed or would collide, and
  # either way half the store would be silently unreachable. ner_db_split() converts it in place.
  if ("candidates" %in% DBI::dbListTables(con_)) {   # views included here ON PURPOSE
    kind_ <- DBI::dbGetQuery(con_, paste0(
      "SELECT table_type FROM information_schema.tables WHERE table_name = 'candidates'"
    ))
    if (nrow(kind_) == 1L && !identical(kind_$table_type[1], "VIEW")) {
      DBI::dbDisconnect(con_, shutdown = TRUE)
      cli::cli_abort(c(
        "Store at {.path {(.db_path)}} holds a flat {.field candidates} TABLE.",
        "i" = "Run {.fun ner_db_split} on it once; it moves the rows into per-label tables and \\
               replaces {.field candidates} with a view over them."
      ))
    }
  }

  purrr::walk(names(.store_schema), \(.l) ner_db_ensure_label(.con = con_, .label = .l,
                                                              .quiet = TRUE))
  n_view_ <- ner_db_view(.con = con_)

  if (!.quiet) {
    cli::cli_alert_success(
      "NER store {ifelse(exists_, 'OPENED', 'CREATED')} at {.path {(.db_path)}} \\
       ({n_view_} label table{?s})."
    )
  }
  con_
}


#' Convert a flat store to per-label tables, once
#'
#' A migration and not part of the running pipeline, which is why it is separate and loud. The rows
#' are moved rather than copied and the flat table is dropped only after every label has been
#' written, inside one transaction: a half-migrated store that still answers queries is worse than
#' one that failed.
#'
#' Extras are NOT populated. The old table never held them, so every extra column comes out NULL and
#' is filled the next time that extractor runs. Nothing is lost, because nothing was there.
#'
#' @param .db_path Character. Path to the DuckDB file.
#' @param .quiet Logical. Suppress the per-label messages.
#' @return Invisibly, a tibble of Label, Table and Rows.
ner_db_split <- function(.db_path, .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Store$NerDB
    .quiet   <- FALSE
  }

  if (!fs::file_exists(.db_path)) cli::cli_abort("No store at {.path {(.db_path)}}.")
  con_ <- ner_db_connect(.db_path = .db_path, .read_only = FALSE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  kind_ <- DBI::dbGetQuery(con_, paste0(
    "SELECT table_type FROM information_schema.tables WHERE table_name = 'candidates'"
  ))
  if (nrow(kind_) == 0L) cli::cli_abort("No {.field candidates} object in the store.")
  if (identical(kind_$table_type[1], "VIEW")) {
    cli::cli_alert_info("Already split: {.field candidates} is a view.")
    return(invisible(tibble::tibble()))
  }

  labs_ <- DBI::dbGetQuery(con_, "SELECT DISTINCT Label FROM candidates ORDER BY Label")$Label
  cli::cli_alert_info("Splitting {length(labs_)} label{?s}: {paste(labs_, collapse = ', ')}.")

  DBI::dbBegin(con_)
  ok_ <- FALSE
  on.exit(if (!ok_) DBI::dbRollback(con_), add = TRUE, after = FALSE)

  out_ <- purrr::map(labs_, function(.l) {
    ner_db_ensure_label(.con = con_, .label = .l, .quiet = TRUE)
    tbl_ <- store_table(.l)
    n_ <- DBI::dbExecute(con_, paste0(
      "INSERT INTO ", tbl_, " (DocID, Start, Stop, Span, LabelRaw, Engine, Model) ",
      "SELECT DocID, Start, Stop, Span, LabelRaw, Engine, Model FROM candidates WHERE Label = ?"
    ), params = list(.l))
    if (!.quiet) cli::cli_alert_success("{format(n_, big.mark = ',')} row{?s} -> {.field {(tbl_)}}")
    tibble::tibble(Label = .l, Table = tbl_, Rows = n_)
  }) |>
    purrr::list_rbind()

  DBI::dbExecute(con_, "DROP TABLE candidates")
  ner_db_view(.con = con_)

  DBI::dbCommit(con_)
  ok_ <- TRUE

  cli::cli_alert_success(
    "Split complete: {format(sum(out_$Rows), big.mark = ',')} row{?s} across \\
     {nrow(out_)} table{?s}; {.field candidates} is now a view."
  )
  invisible(out_)
}


# 3. Ingest ----------------------------------------------------------------------------------------------------------

#' Append one extractor's staging parquet to the store
#'
#' Supersedes the flat-table version. The identity guard, the ledger derivation and the timeout
#' retry path are unchanged; what changes is that candidates are written to one table per label,
#' selecting whichever declared extras the parquet happens to carry.
#'
#' THE REQUESTED LABELS ARE AN ARGUMENT AND CANNOT BE INFERRED FROM THE PARQUET. The staging file
#' records what was FOUND and is silent about what was ASKED FOR: a document requested for ORG and
#' GPE that yielded only organisations needs two ledger rows, ORG success and GPE nohit, and without
#' the requested set the second is underivable, so GPE would look missing on every subsequent render
#' and re-extract forever.
#'
#' @param .db_path Character. Path to the DuckDB file.
#' @param .parquet Character. Staging parquet written by an extractor.
#' @param .labels Character. The labels this extraction REQUESTED, not the ones it found.
#' @param .expect_engine Character or NULL. Assert the Engine tag in the staging file.
#' @param .expect_model Character or NULL. Assert the Model tag.
#' @param .retry_timeout Logical. TRUE deletes prior timeout rows for these documents first.
#' @param .quiet Logical. Suppress the ingest message.
#' @return Invisibly, a list of the row counts written.
ner_db_append <- function(.db_path, .parquet, .labels,
                          .expect_engine = NULL, .expect_model = NULL,
                          .retry_timeout = FALSE, .quiet = FALSE) {
  if (FALSE) {
    .db_path       <- .lP$Store$NerDB
    .parquet       <- "2_output/_Probe/Check-NER-ORG/out/ORG__lexnlp__lexnlp.parquet"
    .labels        <- "ORG"
    .expect_engine <- "lexnlp"
    .expect_model  <- "lexnlp"
    .retry_timeout <- FALSE
    .quiet         <- FALSE
  }

  if (!fs::file_exists(.parquet)) cli::cli_abort("Staging parquet not found: {.path {(.parquet)}}.")

  con_ <- ner_db_init(.db_path, .quiet = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  parquet_ <- as.character(fs::path_abs(.parquet))
  DBI::dbExecute(con_, paste0(
    "CREATE OR REPLACE TEMP VIEW ner_src AS SELECT * FROM read_parquet('", parquet_, "')"
  ))
  src_    <- dplyr::tbl(con_, "ner_src")
  src_cols_ <- names(DBI::dbGetQuery(con_, "SELECT * FROM ner_src LIMIT 0"))

  # Identity guard: the parquet must carry exactly one (Engine, Model), and where an expectation was
  # passed it must equal what ner_run dispatched. An extractor writing the wrong tag produces a
  # store that looks complete and attributes spans to a method that never saw the document.
  stamped_ <- src_ |>
    dplyr::distinct(Engine, Model) |>
    dplyr::collect()
  if (nrow(stamped_) != 1L) {
    cli::cli_abort(c(
      "Staging parquet carries {nrow(stamped_)} distinct (Engine, Model) combo(s); expected 1.",
      "i" = "File: {.path {fs::path_file(.parquet)}}"
    ))
  }
  if (!is.null(.expect_engine) && !is.null(.expect_model)) {
    if (!identical(stamped_$Engine[1], .expect_engine) ||
        !identical(stamped_$Model[1], .expect_model)) {
      cli::cli_abort(c(
        "Stamped identity does not match what was dispatched -- the extractor's ENGINE/MODEL \\
         constants have drifted from the {.arg .run} token.",
        "x" = "parquet stamps {.val {stamped_$Engine[1]}} / {.val {stamped_$Model[1]}}",
        "v" = "ner_run expected {.val {(.expect_engine)}} / {.val {(.expect_model)}}",
        "i" = "Fix the extractor's constants (or the token), then rerun. Ingesting as-is would \\
               misfile the rows and rerun this combo forever."
      ))
    }
  }

  labels_req_ <- sort(unique(as.character(.labels)))
  if (length(labels_req_) == 0L) {
    cli::cli_abort("{.arg .labels} must name the labels this extraction requested.")
  }

  # Per document and label: timeout > success > nohit. A timeout is a property of the document under
  # the engine rather than of one label -- the extractor was cut off, so nothing it was asked for
  # can be called clean.
  hits_ <- src_ |>
    dplyr::filter(!is.na(Start)) |>
    dplyr::distinct(DocID, Engine, Model, Label) |>
    dplyr::collect() |>
    dplyr::mutate(HasHit = TRUE)

  tmo_ <- src_ |>
    dplyr::group_by(DocID, Engine, Model) |>
    dplyr::summarise(
      HasTimeout = max(dplyr::if_else(!is.na(LabelRaw) & LabelRaw %like% "timeout:%", 1L, 0L),
                       na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect()

  status_ <- tidyr::expand_grid(
    dplyr::select(tmo_, DocID, Engine, Model, HasTimeout),
    Label = labels_req_
  ) |>
    dplyr::left_join(hits_, by = dplyr::join_by(DocID, Engine, Model, Label)) |>
    dplyr::mutate(Status = dplyr::case_when(
      .data$HasTimeout == 1L ~ "timeout",
      !is.na(.data$HasHit)   ~ "success",
      TRUE                   ~ "nohit"
    )) |>
    dplyr::select(DocID, Engine, Model, Label, Status)

  combos_ <- status_ |>
    dplyr::left_join(
      dplyr::tbl(con_, "runs") |>
        dplyr::select(DocID, Engine, Model, Label, OldStatus = Status) |>
        dplyr::collect(),
      by = dplyr::join_by(DocID, Engine, Model, Label)
    )

  n_all_ <- nrow(combos_)
  new_   <- dplyr::filter(combos_, is.na(.data$OldStatus))
  retry_ <- dplyr::filter(combos_, !is.na(.data$OldStatus), .data$OldStatus == "timeout")
  to_ingest_ <- if (isTRUE(.retry_timeout)) dplyr::bind_rows(new_, retry_) else new_

  if (nrow(to_ingest_) == 0L) {
    if (!.quiet) {
      cli::cli_alert_info("All {n_all_} document-label pair(s) already in store -- nothing to do.")
    }
    return(invisible(list(docs = 0L, candidates = 0L, retried = 0L)))
  }

  duckdb::duckdb_register(con_, "ner_ingest_status",
                          to_ingest_[c("DocID", "Engine", "Model", "Label", "Status")])
  on.exit(duckdb::duckdb_unregister(con_, "ner_ingest_status"), add = TRUE, after = FALSE)

  engine_  <- to_ingest_$Engine[1]
  model_   <- to_ingest_$Model[1]
  n_retry_ <- if (isTRUE(.retry_timeout)) nrow(retry_) else 0L
  labs_in_ <- sort(unique(to_ingest_$Label))

  DBI::dbBegin(con_)
  ok_ <- FALSE
  on.exit(if (!ok_) DBI::dbRollback(con_), add = TRUE, after = FALSE)

  n_cand_ <- 0L
  for (.l in labs_in_) {
    tbl_ <- ner_db_ensure_label(.con = con_, .label = .l, .quiet = TRUE)

    map_    <- store_source_map(.label = .l, .src_cols = src_cols_)
    extras_ <- names(map_)
    src_of_ <- unname(map_)
    if (anyNA(src_of_) || any(!nzchar(src_of_))) {
      cli::cli_abort("Unresolved source column for {.val {(.l)}}; refusing to build the insert.")
    }

    # paste0("s.", character(0)) IS "s.", NOT character(0). R recycles to the longest argument and
    # treats a zero-length one as "", so an engine with no extras -- every spaCy model, the
    # gazetteer, redaction -- contributed a bare "s." to the column list and DuckDB rejected the
    # statement with a parser error pointing at the alias rather than at the empty name. recycle0
    # exists for this and has since R 4.0.1; the explicit branch says so where it matters.
    sel_extra_ <- if (length(src_of_) > 0L) paste0("s.", src_of_) else character(0)

    cols_sql_ <- paste(c("DocID", "Start", "Stop", "Span", "LabelRaw", "Engine", "Model", extras_),
                       collapse = ", ")
    sel_sql_  <- paste(c("s.DocID", "s.Start", "s.Stop", "s.Span", "s.LabelRaw", "s.Engine",
                         "s.Model", sel_extra_), collapse = ", ")

    # The two lists must agree in length or the INSERT silently maps the wrong source to the wrong
    # column, which is worse than the parser error it replaces.
    if (length(extras_) != length(sel_extra_)) {
      cli::cli_abort("Column list and select list disagree for {.val {(.l)}}.")
    }

    if (n_retry_ > 0L) {
      DBI::dbExecute(con_, paste0(
        "DELETE FROM ", tbl_, " WHERE Engine = ? AND Model = ? ",
        "AND EXISTS (SELECT 1 FROM ner_ingest_status i WHERE i.DocID = ", tbl_, ".DocID ",
        "            AND i.Label = ?) ",
        "AND EXISTS (SELECT 1 FROM runs r WHERE r.DocID = ", tbl_, ".DocID ",
        "            AND r.Engine = ", tbl_, ".Engine AND r.Model = ", tbl_, ".Model ",
        "            AND r.Label = ? AND r.Status = 'timeout')"
      ), params = list(engine_, model_, .l, .l))
    }

    n_cand_ <- n_cand_ + DBI::dbExecute(con_, paste0(
      "INSERT INTO ", tbl_, " (", cols_sql_, ") SELECT ", sel_sql_, " FROM ner_src s ",
      "WHERE s.Start IS NOT NULL AND s.Label = ? ",
      "  AND EXISTS (SELECT 1 FROM ner_ingest_status i ",
      "              WHERE i.DocID = s.DocID AND i.Label = s.Label)"
    ), params = list(.l))
  }

  DBI::dbExecute(con_, "
    INSERT INTO runs (DocID, Engine, Model, Label, Status, CreatedAt)
    SELECT DocID, Engine, Model, Label, Status, now()::TIMESTAMP FROM ner_ingest_status
  ")

  ner_db_view(.con = con_)
  DBI::dbCommit(con_)
  ok_ <- TRUE

  if (!.quiet) {
    msg_ <- paste0("Appended {n_cand_} candidate(s) over {nrow(to_ingest_)} of {n_all_} ",
                   "document-label pair(s) from {.path {fs::path_file(.parquet)}}")
    if (n_retry_ > 0L) msg_ <- paste0(msg_, " (incl. {n_retry_} timeout retr{?y/ies})")
    cli::cli_alert_success(msg_)
  }
  invisible(list(docs = nrow(to_ingest_), candidates = n_cand_, retried = n_retry_))
}


# 4. Clearing --------------------------------------------------------------------------------------------------------

#' Clear a combination's rows from the store, scoped
#'
#' Supersedes the flat-table version: the delete runs once per label table instead of once against
#' `candidates`, which is now a view and cannot be deleted from.
#'
#' @param .db_path Character. Path to the DuckDB file.
#' @param .run Character vector of combination tokens.
#' @param .doc_ids Character vector of documents, or NULL for all.
#' @param .status Character vector of ledger statuses to restrict to, or NULL for all.
#' @param .labels Character vector of labels to restrict to, or NULL for all.
#' @param .quiet Logical. Suppress the message.
#' @return Invisibly, a list of the row counts deleted.
ner_db_clear <- function(.db_path, .run, .doc_ids = NULL, .status = NULL, .labels = NULL,
                         .quiet = FALSE) {
  if (FALSE) {
    .db_path <- .lP$Store$NerDB
    .run     <- "lexnlp"
    .doc_ids <- NULL
    .status  <- "timeout"
    .labels  <- NULL
    .quiet   <- FALSE
  }

  if (!fs::file_exists(.db_path)) cli::cli_abort("No NER store at {.path {(.db_path)}}.")
  if (!is.null(.status)) {
    .status <- match.arg(.status, c("success", "nohit", "timeout"), several.ok = TRUE)
  }

  combos_ <- purrr::map(.run, ner_parse_combo) |>
    dplyr::bind_rows() |>
    dplyr::distinct()

  con_ <- ner_db_init(.db_path, .quiet = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  duckdb::duckdb_register(con_, "ner_clear_combos", as.data.frame(combos_))
  on.exit(duckdb::duckdb_unregister(con_, "ner_clear_combos"), add = TRUE, after = FALSE)

  has_docs_ <- !is.null(.doc_ids)
  if (has_docs_) {
    duckdb::duckdb_register(con_, "ner_clear_docs", data.frame(DocID = unique(.doc_ids)))
    on.exit(duckdb::duckdb_unregister(con_, "ner_clear_docs"), add = TRUE, after = FALSE)
  }
  has_status_ <- !is.null(.status)
  if (has_status_) {
    duckdb::duckdb_register(con_, "ner_clear_status", data.frame(Status = .status))
    on.exit(duckdb::duckdb_unregister(con_, "ner_clear_status"), add = TRUE, after = FALSE)
  }

  labs_ <- if (is.null(.labels)) {
    DBI::dbGetQuery(con_, "SELECT DISTINCT Label FROM runs ORDER BY Label")$Label
  } else {
    sort(unique(.labels))
  }
  labs_ <- labs_[store_table(labs_) %in% store_tables(.con = con_)]

  combo_pred_ <- "(Engine, Model) IN (SELECT Engine, Model FROM ner_clear_combos)"
  doc_pred_   <- if (has_docs_) " AND DocID IN (SELECT DocID FROM ner_clear_docs)" else ""
  runs_status_pred_ <- if (has_status_) {
    " AND Status IN (SELECT Status FROM ner_clear_status)"
  } else ""

  DBI::dbBegin(con_)
  ok_ <- FALSE
  on.exit(if (!ok_) DBI::dbRollback(con_), add = TRUE, after = FALSE)

  # The label tables go first: their status gate reads runs, so deleting runs first would leave
  # candidates that no longer have a ledger row to be found by.
  n_cand_ <- 0L
  for (.l in labs_) {
    tbl_ <- store_table(.l)
    stat_pred_ <- if (has_status_) {
      paste0(" AND DocID IN (SELECT DocID FROM runs r WHERE r.Engine = ", tbl_, ".Engine ",
             "AND r.Model = ", tbl_, ".Model AND r.Label = '", .l, "' ",
             "AND r.Status IN (SELECT Status FROM ner_clear_status))")
    } else ""
    n_cand_ <- n_cand_ + DBI::dbExecute(con_, paste0(
      "DELETE FROM ", tbl_, " WHERE ", combo_pred_, doc_pred_, stat_pred_
    ))
  }

  lab_pred_ <- if (is.null(.labels)) "" else {
    paste0(" AND Label IN ('", paste(sort(unique(.labels)), collapse = "', '"), "')")
  }
  n_runs_ <- DBI::dbExecute(con_, paste0(
    "DELETE FROM runs WHERE ", combo_pred_, doc_pred_, runs_status_pred_, lab_pred_
  ))

  DBI::dbCommit(con_)
  ok_ <- TRUE

  status_msg_ <- if (has_status_) paste0(" [status: ", paste(.status, collapse = "/"), "]") else ""
  docs_msg_   <- if (has_docs_) paste0(" [", length(unique(.doc_ids)), " doc(s)]") else ""
  label_msg_  <- if (is.null(.labels)) "" else {
    paste0(" [", paste(sort(unique(.labels)), collapse = "/"), "]")
  }
  if (!.quiet) {
    cli::cli_alert_success(paste0(
      "Cleared {n_runs_} ledger row(s) and {n_cand_} candidate(s) across {nrow(combos_)} ",
      "combo(s){status_msg_}{docs_msg_}{label_msg_}. Re-run ner_run() to repopulate."
    ))
  }
  invisible(list(runs = n_runs_, candidates = n_cand_, combos = combos_))
}
