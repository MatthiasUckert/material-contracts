# ======================================================================================================================
# 04D-Preflight.R -- three questions to answer before 04D is written
# ======================================================================================================================
#
# READS NOTHING BUT THE CORPUS STORES AND THE REGISTER, AND WRITES NOTHING AT ALL. Every connection is
# opened read-only and closed on exit. Nothing under 2_output/ is created, moved or touched.
#
# WHY THESE THREE
#
#   Q1  DOES THE POPULATION DIFFER BY ENTITY, AND BY HOW MUCH?
#       04D is five passes now, and each takes its population from its OWN family's ledger rather
#       than from matcon's alone. That is only an improvement if the ledgers actually differ -- and
#       if they differ a lot, the five releases cover five different document sets and every
#       cross-entity join downstream has to say so.
#
#   Q2  DO THE CORPUS SPANS CARRY THE CUE COLUMNS?
#       This is the one that would break silently at scale. dte_describe() reads CueBefore and aborts
#       without it; mny_mark_par() reads both sides; geo_law_inside() needs the offsets only. The
#       SAMPLE stores carry 160 characters either side -- verified. The CORPUS stores were written by
#       a different run of 04C and have never been checked. A null column there means DATE and MONEY
#       cannot run on the corpus at all, and it is better to know now than at hour two.
#
#   Q3  DO THE KEYS BUILD WITHOUT 03F?
#       No release function reads Class any more: it appears only in collapses and reports. So 04D
#       should need the register and nothing else -- no label release, no partial-release check, no
#       dependency on the classification stage. This confirms the register alone carries the filer
#       name, the filing date and the filer count every rule needs.
#
# Usage:
#   source(here::here("1_code", "_Tests", "04D-Preflight.R"), encoding = "UTF-8")

here::i_am("1_code/_Commons/_Initialize.R")

purrr::walk(
  .x = c("_Initialize", "_Utils", "_NER", "_Plots", "_Tables", "_Entity"),
  .f = \(.s) source(here::here("1_code", "_Commons", paste0(.s, ".R")), encoding = "UTF-8")
)

options(cli.num_colors = 1, cli.width = 120, mc.table_mode = "console")


# 0. Configuration -----------------------------------------------------------------------------------------------------
#
# THE CORPUS STORE AND NOT THE SAMPLE'S. Every 04B document points .lP$Input$Store at 04A's Output/,
# which holds 4,398 documents. 04C writes its own stores under its own directory, and confusing the
# two is the single easiest way to answer these questions about the wrong data.

.pP <- list(
  Store = fs::path(
    init_create_script_dir(.dir_here = here::here(), .name_script = "04C-EntityCorpus"), "Output"
  ),
  Register = fs::path(
    init_create_script_dir(.dir_here = here::here(), .name_script = "02B-Register"),
    "Output", "Documents.parquet"
  ),
  # The families 04D runs, and what each answers for.
  Families = c(matcon = "matcon", lexnlp = "lexnlp"),
  # Documents drawn for the cue check. Small: the question is whether the column is populated at all,
  # not what its distribution is.
  NCue     = 200L,
  Seed     = 42L
)

cli::cli_h1("04D preflight")

tab_paths <- tibble::tibble(
  Item   = c("Corpus stores", "Register"),
  Path   = purrr::map_chr(list(.pP$Store, .pP$Register),
                          \(.p) fs::path_rel(.p, start = here::here())),
  Exists = c(fs::dir_exists(.pP$Store), fs::file_exists(.pP$Register))
)

tbl_say(.tab = tab_paths, .title = "Both inputs resolve before anything is read")

if (any(!tab_paths$Exists)) {
  cli::cli_abort(c(
    "{sum(!tab_paths$Exists)} input{?s} {?does/do} not resolve.",
    "x" = "{paste(tab_paths$Item[!tab_paths$Exists], collapse = ', ')}.",
    "i" = "04C writes the stores and 02B writes the register."
  ))
}


# 1. Q1 -- the population, entity by entity -----------------------------------------------------------------------------

#' Every entity a family's ledger recorded, and how many documents finished it
#'
#' THE LEDGER AND NOT THE ENTITY TABLES. A document processed and found to hold no date has no row in
#' the date table and is not missing -- it is a contract with no date, which is a measurement. Reading
#' the population from the tables would drop exactly those and overstate every rate.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .family Character. Which store to open.
#' @return Tibble: Family, Entity, Docs, Errors.
prb_population <- function(.dir_store, .family) {
  if (FALSE) {
    .dir_store <- .pP$Store
    .family    <- "matcon"
  }

  path_ <- ner_db_path(.dir = .dir_store, .family = .family)
  if (!fs::file_exists(path_)) {
    cli::cli_alert_danger("No {(.family)} store at {.path {fs::path_file(path_)}}.")
    return(tibble::tibble())
  }

  con_ <- ner_db_connect(.db_path = path_, .read_only = TRUE)
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  if (!"runs" %in% DBI::dbListTables(con_)) {
    cli::cli_alert_danger("The {(.family)} store holds no ledger.")
    return(tibble::tibble())
  }

  DBI::dbGetQuery(con_, "
    SELECT Entity,
           COUNT(DISTINCT CASE WHEN Status <> 'error' THEN DocID END) AS Docs,
           COUNT(DISTINCT CASE WHEN Status  = 'error' THEN DocID END) AS Errors
      FROM runs GROUP BY Entity ORDER BY Entity") |>
    tibble::as_tibble() |>
    dplyr::mutate(Family = .family, .before = 1L)
}

cli::cli_h2("Q1  The population, entity by entity")

tab_pop <- purrr::map(.pP$Families, \(.f) prb_population(.dir_store = .pP$Store, .family = .f)) |>
  purrr::list_rbind()

if (nrow(tab_pop) == 0L) {
  cli::cli_abort("No ledger in any store, so there is no population to describe.")
}

tab_pop |>
  dplyr::mutate(dplyr::across(c(Docs, Errors), \(.x) format(.x, big.mark = ","))) |>
  tbl_say(.title = "Documents each family finished, per entity")

# THE FIVE PASSES AND WHAT EACH NEEDS. Named here rather than inferred, because a pass reading an
# entity its family never ran would come back empty and report it as a corpus with none of that
# entity in it.
tab_need <- tibble::tribble(
  ~Pass,     ~Family,   ~Entity,
  "ORG",     "lexnlp",  "ORG",
  "GPE",     "matcon",  "GPE",
  "GPE",     "matcon",  "LAW",
  "DATE",    "matcon",  "DATE",
  "DATE",    "matcon",  "TERM",
  "MONEY",   "matcon",  "MONEY",
  "REDACT",  "matcon",  "REDACT",
  "REDACT",  "matcon",  "MONEY"      # the overlap join reads the withheld money spans
) |>
  dplyr::left_join(tab_pop, by = dplyr::join_by(Family, Entity)) |>
  dplyr::mutate(Docs = dplyr::coalesce(.data$Docs, 0L))

tab_need |>
  dplyr::mutate(Docs = format(.data$Docs, big.mark = ",")) |>
  dplyr::select("Pass", "Family", "Entity", "Docs") |>
  tbl_say(.title = "What each pass needs, and whether the ledger has it")

miss_ <- dplyr::filter(tab_need, .data$Docs == 0L)
if (nrow(miss_) > 0L) {
  cli::cli_alert_danger(
    "{nrow(miss_)} pass-entity pair{?s} {?has/have} no finished document: \\
     {paste(paste0(miss_$Pass, '/', miss_$Entity), collapse = ', ')}. 04C has not run {?it/them}."
  )
} else {
  cli::cli_alert_success("Every entity the five passes need has finished documents in its ledger.")
}

# THE SPREAD IS THE POINT OF THIS TABLE. Five passes taking five populations is an improvement only
# if the populations are close; where they are not, the five releases cover different document sets
# and every cross-entity join downstream inherits that.
span_ <- range(tab_need$Docs[tab_need$Docs > 0L])
cli::cli_alert_info(
  "THE POPULATIONS RUN FROM {format(span_[[1L]], big.mark = ',')} TO \\
   {format(span_[[2L]], big.mark = ',')} DOCUMENTS, a spread of \\
   {tbl_pct((span_[[2L]] - span_[[1L]]) / max(span_[[2L]], 1L))}. Each pass takes its own family's \\
   ledger, so a small spread means the five releases describe nearly the same corpus and a large one \\
   means they do not -- and the export has to say which."
)


# 2. Q2 -- the cue columns at corpus scale --------------------------------------------------------------------------

#' Are the stored cue windows populated on the corpus spans
#'
#' THE CHECK THAT WOULD OTHERWISE FAIL AT HOUR TWO. dte_describe() aborts without CueBefore and
#' mny_mark_par() aborts without both. The sample stores carry 160 characters either side; the corpus
#' stores were written by a different run of 04C and have never been asked.
#'
#' DOCUMENTS DRAWN FROM THE LEDGER AND NOT FROM THE REGISTER. ent_load_entity() inner-joins on the
#' lens, so a draw of register rows 04C never extracted returns nothing and reports a populated
#' column as absent.
#'
#' @param .dir_store Directory holding the family databases.
#' @param .family Character.
#' @param .entity Character.
#' @param .lens Tibble: DocID and DocLen, for the whole corpus.
#' @param .n Integer. Documents drawn.
#' @return One-row tibble.
prb_cues <- function(.dir_store, .family, .entity, .lens, .n = 200L) {
  if (FALSE) {
    .dir_store <- .pP$Store
    .family    <- "matcon"
    .entity    <- "DATE"
    .lens      <- tab_lens
    .n         <- 200L
  }

  con_ <- ner_db_connect(
    .db_path = ner_db_path(.dir = .dir_store, .family = .family), .read_only = TRUE
  )
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  docs_ <- DBI::dbGetQuery(con_, glue::glue(
    "SELECT DISTINCT DocID FROM runs
      WHERE Entity = '{.entity}' AND Status <> 'error' LIMIT {as.integer(.n)}"
  ))$DocID
  DBI::dbDisconnect(con_, shutdown = TRUE)
  on.exit()

  if (length(docs_) == 0L) {
    return(tibble::tibble(Family = .family, Entity = .entity, Spans = 0L,
                          HasBefore = NA_real_, HasAfter = NA_real_, MedChars = NA_real_))
  }

  spans_ <- ent_load_entity(
    .dir_store = .dir_store,
    .family    = .family,
    .entity    = .entity,
    .lens      = dplyr::filter(.lens, .data$DocID %in% docs_),
    .extras    = ent_extras(.family, .entity),
    .quiet     = TRUE
  )

  if (nrow(spans_) == 0L) {
    return(tibble::tibble(Family = .family, Entity = .entity, Spans = 0L,
                          HasBefore = NA_real_, HasAfter = NA_real_, MedChars = NA_real_))
  }

  tibble::tibble(
    Family    = .family,
    Entity    = .entity,
    Spans     = nrow(spans_),
    HasBefore = mean(!is.na(spans_$CueBefore)),
    HasAfter  = mean(!is.na(spans_$CueAfter)),
    MedChars  = stats::median(nchar(dplyr::coalesce(spans_$CueBefore, "")))
  )
}

cli::cli_h2("Q2  The cue columns, on the corpus stores")

# THE LENS IS THE REGISTER'S nChars. There is no corpus text file and there should not be: the cue
# columns exist precisely so that no rule has to open a document at corpus scale.
tab_lens <- arrow::open_dataset(sources = .pP$Register) |>
  dplyr::select("DocID", "nChars") |>
  dplyr::collect() |>
  dplyr::transmute(DocID, DocLen = as.integer(.data$nChars)) |>
  dplyr::filter(!is.na(.data$DocLen), .data$DocLen > 0L)

cli::cli_alert_info(
  "{format(nrow(tab_lens), big.mark = ',')} {cli::qty(nrow(tab_lens))}document length{?s} from the \\
   register. No text file is read, which is the whole reason the cue windows are stored."
)

tab_cue <- purrr::pmap(
  list(c("matcon", "matcon", "matcon", "matcon", "lexnlp"),
       c("DATE", "MONEY", "GPE", "TERM", "ORG")),
  \(.f, .e) prb_cues(.dir_store = .pP$Store, .family = .f, .entity = .e,
                     .lens = tab_lens, .n = .pP$NCue)
) |>
  purrr::list_rbind()

tab_cue |>
  dplyr::mutate(
    Spans = format(.data$Spans, big.mark = ","),
    dplyr::across(c(HasBefore, HasAfter), \(.x) tbl_pct_safe(.x))
  ) |>
  tbl_say(.title = "Stored cue windows, on documents drawn from each ledger")

need_ <- dplyr::filter(tab_cue, .data$Entity %in% c("DATE", "MONEY"))
bad_  <- dplyr::filter(need_, is.na(.data$HasBefore) | .data$HasBefore < 0.99)

if (nrow(bad_) > 0L) {
  cli::cli_alert_danger(
    "{nrow(bad_)} entit{?y/ies} the rules read cues from {?has/have} no populated window: \\
     {paste(bad_$Entity, collapse = ', ')}. dte_describe() and mny_mark_par() BOTH ABORT without \\
     one, so those passes cannot run on the corpus until 04C re-extracts with a cue width set."
  )
} else {
  cli::cli_alert_success(
    "DATE and MONEY both carry a populated cue window at a median of \\
     {stats::median(need_$MedChars)} characters, so the rules run on the corpus unchanged."
  )
}

cli::cli_alert_info(
  "GPE AND TERM ARE REPORTED AND NOT REQUIRED. The governing-law exclusion is a comparison of \\
   offsets and reads no window at all; the period classification reads one but only to fill \\
   PeriodKind, which is a diagnostic column no rule conditions on."
)


# 3. Q3 -- the keys, without a label release ---------------------------------------------------------------------------

cli::cli_h2("Q3  The corpus keys, from the register alone")

tab_keys <- ent_corpus_keys(
  .path_register = .pP$Register,
  .path_release  = NA_character_,   # NO LABEL RELEASE. Class reaches no release function any more.
  .doc_ids       = NULL,
  .quiet         = TRUE
)

tibble::tibble(
  Item = c("Rows in the register",
           "Carrying a company name",
           "Carrying a filing date",
           "Carrying a filer count",
           "Filed by more than one registrant"),
  N    = c(nrow(tab_keys),
           sum(!is.na(tab_keys$CompanyName)),
           sum(!is.na(tab_keys$DateFiled)),
           sum(!is.na(tab_keys$NFilers)),
           sum(dplyr::coalesce(tab_keys$NFilers, 1L) > 1L))
) |>
  dplyr::mutate(Share = tbl_pct(.data$N / max(nrow(tab_keys), 1L))) |>
  tbl_say(.title = "What the register alone supplies")

cli::cli_alert_info(
  "NO RELEASE FUNCTION READS Class. It appears only in collapses and in reports, both of which join \\
   it at their own time -- so 04D needs the register and nothing else, and the label release, the \\
   partial-release check and the dependency on 03F all come out."
)

reach_ <- dplyr::semi_join(tab_keys, dplyr::distinct(tab_lens, DocID), by = dplyr::join_by(DocID))
cli::cli_alert_info(
  "{format(nrow(reach_), big.mark = ',')} of {format(nrow(tab_keys), big.mark = ',')} register rows \\
   carry a usable length, which is the population any pass can reach. A rule needs an anchor and a \\
   document length; it does not need a label."
)

cli::cli_h2("Preflight complete")
cli::cli_alert_info(
  "Three answers: whether the five populations agree, whether the corpus carries the cue windows the \\
   DATE and MONEY rules read, and whether the keys build without 03F. Paste all three tables."
)
