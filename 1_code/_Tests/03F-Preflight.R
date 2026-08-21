# 03F-Preflight: measuring the four things the redesign rests on ---------------------------------------------------------
#
# WHAT THIS IS
# 03F is about to be rewritten around the register: a population toggle, primary-filer deduplication,
# and a fan-out at release. Each of those rests on a claim about the register that has not been
# measured. This measures them. IT WRITES NOTHING.
#
# THE FOUR CLAIMS
#
#   1. THE POPULATIONS ARE WHAT WE THINK. `all`, `descriptive` and `estimation` for Exhibit 10, taken
#      from the register's ladder columns rather than from the numbers quoted in 02B's prose.
#
#   2. PrimaryFiler DESIGNATES EXACTLY ONE COPY PER ATTACHMENT. 01C says it does and checks it there.
#      03F is about to depend on it, so it checks it here too: zero or two primaries for one
#      HashDocument would silently drop or double-classify an attachment.
#
#   3. THE PRIMARY IS NOT ALWAYS IN THE POPULATION. PrimaryFiler is chosen globally -- lowest DocID
#      among copies passing the quality rules -- while EstiSample depends on a per-CIK Compustat
#      match. So an attachment can have its primary outside the estimation sample and another copy
#      inside it. If that count is above zero, classifying only primaries inside the population would
#      leave those attachments unlabelled, and the design must classify the primary whenever ANY copy
#      is in the population.
#
#   4. DocTypeMod REPRODUCES THE MIRROR'S DIRECTORY NAME ACROSS DOCUMENT TYPES. The 03A probe showed
#      this at 100%, but on a sample that was entirely Exhibit 10 -- so any column constant at
#      "Exhibit10" would have agreed trivially. That result was explicitly logged as NOT transferable
#      to a corpus pass. This is the corpus pass, so it is measured properly here.
#
# PREDICTIONS, STATED BEFORE RUNNING
#   2. Exhibit 10 descriptive ~1,405,378; estimation ~967,249
#   3. Exactly one primary per attachment, zero exceptions
#   4. Primaries outside the population: SMALL BUT NOT ZERO -- Compustat coverage varies by registrant
#   5. DocTypeMod matches the mirror directory on every document type, not only Exhibit 10
#   6. Deduplication saves roughly one document in six
#
# Run from the project root:  source("1_code/_Tests/03F-Preflight.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

pct_ <- function(.n, .d) if (.d == 0L) "-" else paste0(format(round(100 * .n / .d, 1), nsmall = 1), "%")

ok_ <- function(.flag, .yes = "PASS", .no = "FAIL") if (isTRUE(.flag)) .yes else .no


# 1. Resolve ------------------------------------------------------------------------------------------------------------

here::i_am("1_code/_Commons/_Initialize.R")
source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")

path_reg_   <- here::here("2_output", "02B-Register", "Output", "Documents.parquet")
dir_mirror_ <- here::here("2_output", "01B-EdgarDocuments", "GetEDGAR")
path_store_ <- here::here("2_output", "03F-ClassifyApply", "store", "labels.duckdb")

say_("== 03F-Preflight ==   WRITES NOTHING.")
say_("")
say_("register : ", if (fs::file_exists(path_reg_)) "ok" else "MISSING")
say_("mirror   : ", if (fs::dir_exists(dir_mirror_)) "ok" else "MISSING")
say_("store    : ", if (fs::file_exists(path_store_)) "ok" else "absent (nothing classified yet)")
say_("")
stopifnot("register missing" = fs::file_exists(path_reg_))

cols_ <- names(arrow::open_dataset(sources = path_reg_))
need_ <- c("DocID", "HashDocument", "DocTypeMod", "YQ", "Group",
           "Removed", "DescSample", "EstiSample", "MultFiler", "nCIK",
           "PrimaryFiler", "FilerCopiesAgree")
miss_ <- setdiff(need_, cols_)
say_("register columns needed : ", length(need_), "   missing: ",
     if (length(miss_) == 0L) "none" else paste(miss_, collapse = ", "))
stopifnot("register is missing columns the design needs" = length(miss_) == 0L)
say_("")

reg_ <- arrow::open_dataset(sources = path_reg_) |>
  dplyr::select(dplyr::all_of(need_)) |>
  dplyr::collect()


# 2. The three populations ----------------------------------------------------------------------------------------------
# Read off the register rather than quoted from 02B's prose. Exhibit 10 only, which is the corpus 03F
# labels.

ex10_ <- reg_ |> dplyr::filter(.data$DocTypeMod == "Exhibit10")

say_("== 2. Populations (Exhibit 10) ==")
say_("register rows, all types : ", fmt_(nrow(reg_)))
say_("Exhibit 10               : ", fmt_(nrow(ex10_)))
say_("  descriptive            : ", fmt_(sum(as.logical(ex10_$DescSample), na.rm = TRUE)))
say_("  estimation             : ", fmt_(sum(as.logical(ex10_$EstiSample), na.rm = TRUE)))
say_("  flagged Removed        : ", fmt_(sum(as.logical(ex10_$Removed), na.rm = TRUE)))
say_("")


# 3. One primary per attachment -----------------------------------------------------------------------------------------
# PREDICTION: exactly one, always. 01C checks this where the flag is made; 03F is about to depend on
# it, so it does not inherit the check on trust.

prim_ <- ex10_ |>
  dplyr::summarise(nPrimary = sum(as.logical(.data$PrimaryFiler), na.rm = TRUE), .by = "HashDocument")

say_("== 3. PrimaryFiler ==")
say_("attachments (HashDocument) : ", fmt_(nrow(prim_)))
prim_ |>
  dplyr::count(.data$nPrimary, name = "Attachments") |>
  dplyr::arrange(.data$nPrimary) |>
  (\(.t) say_(paste0("  nPrimary=", format(.t$nPrimary, width = 4), fmt_(.t$Attachments),
                     collapse = "\n")))()
one_ok_ <- all(prim_$nPrimary == 1L)
say_("exactly one, everywhere    : ", ok_(one_ok_))
say_("")


# 4. Is the primary always in the population? ---------------------------------------------------------------------------
# THE INTERACTION THE DESIGN TURNS ON. PrimaryFiler is chosen globally; EstiSample depends on a
# per-CIK Compustat match. Where the two disagree, classifying only primaries that are themselves in
# the population would leave an attachment unlabelled even though a copy of it is in the sample.
#
# PREDICTION: small but not zero.

gap_ <- function(.flag) {
  in_pop_ <- ex10_ |>
    dplyr::filter(as.logical(.data[[.flag]])) |>
    dplyr::distinct(.data$HashDocument)
  prim_in_ <- ex10_ |>
    dplyr::filter(as.logical(.data$PrimaryFiler), as.logical(.data[[.flag]])) |>
    dplyr::distinct(.data$HashDocument)
  list(
    Attachments = nrow(in_pop_),
    PrimaryIn   = nrow(prim_in_),
    Orphaned    = nrow(dplyr::anti_join(in_pop_, prim_in_, by = dplyr::join_by(HashDocument)))
  )
}

say_("== 4. Primary versus population ==")
for (f_ in c("DescSample", "EstiSample")) {
  g_ <- gap_(f_)
  say_("  ", format(f_, width = 12),
       "attachments ", format(fmt_(g_$Attachments), width = 12),
       "primary also in ", format(fmt_(g_$PrimaryIn), width = 12),
       "ORPHANED ", fmt_(g_$Orphaned))
}
say_("")
say_("  An orphaned attachment has a copy in the population but its primary outside it. Above zero")
say_("  means the design must classify the primary whenever ANY copy is in the population, rather")
say_("  than only primaries that are themselves in it.")
say_("")


# 5. Does DocTypeMod name the mirror's directory? -----------------------------------------------------------------------
# THE 03A RESULT DOES NOT TRANSFER AND WAS LOGGED AS NOT TRANSFERRING. There, every labelled document
# was Exhibit 10, so any column constant at "Exhibit10" agreed at 100% for free. Here it is measured
# against the directories that actually exist under the mirror.

say_("== 5. DocTypeMod against the mirror ==")
dir_parsed_ <- fs::path(dir_mirror_, "DocumentData", "Parsed")
if (!fs::dir_exists(dir_parsed_)) {
  say_("  no Parsed tree at ", fs::path_rel(dir_parsed_, here::here()))
  type_ok_ <- NA
} else {
  on_disk_ <- fs::path_file(fs::dir_ls(dir_parsed_, type = "directory"))
  in_reg_  <- sort(unique(reg_$DocTypeMod))
  say_("directories under Parsed/ : ", length(on_disk_))
  say_("  ", paste(sort(on_disk_), collapse = ", "))
  say_("DocTypeMod values         : ", length(in_reg_))
  say_("  ", paste(in_reg_, collapse = ", "))
  say_("")
  say_("in register, no directory : ", paste(setdiff(in_reg_, on_disk_), collapse = ", "))
  say_("directory, not in register: ", paste(setdiff(on_disk_, in_reg_), collapse = ", "))
  type_ok_ <- length(setdiff(in_reg_, on_disk_)) == 0L
  say_("every DocTypeMod has a directory : ", ok_(type_ok_))
}
say_("")

# A sample of rebuilt paths, across types, actually resolving.
set.seed(42)
smp_ <- reg_ |> dplyr::slice_sample(n = 2000L)
paths_ <- utils_doc_path(
  .dir_mirror = dir_mirror_,
  .doc_type   = smp_$DocTypeMod,
  .yq         = smp_$YQ,          # utils_doc_path normalises the double form itself
  .doc_id     = smp_$DocID
)
ex_ <- fs::file_exists(paths_)
say_("path sample (2,000 rows, every type):")
say_("  resolve on disk : ", fmt_(sum(ex_)), " of ", fmt_(length(ex_)), "  (", pct_(sum(ex_), length(ex_)), ")")
if (any(!ex_)) {
  say_("  first 5 that do not:")
  say_(paste0("    ", fs::path_rel(utils::head(paths_[!ex_], 5), here::here()), collapse = "\n"))
  say_("  by DocTypeMod:")
  smp_[!ex_, ] |>
    dplyr::count(.data$DocTypeMod, name = "Missing") |>
    (\(.t) say_(paste0("    ", format(.t$DocTypeMod, width = 20), fmt_(.t$Missing), collapse = "\n")))()
}
say_("")


# 6. What deduplication actually saves ----------------------------------------------------------------------------------
# Under the design: classify the primary of every attachment with at least one copy in the population.

say_("== 6. What the pass would cost ==")
say_("  ", format("population", width = 14), format("copies", width = 14),
     format("attachments", width = 14), format("to classify", width = 14), "saving")
for (f_ in c("all", "DescSample", "EstiSample")) {
  pop_ <- if (f_ == "all") ex10_ else ex10_ |> dplyr::filter(as.logical(.data[[f_]]))
  hash_ <- unique(pop_$HashDocument)
  todo_ <- ex10_ |>
    dplyr::filter(as.logical(.data$PrimaryFiler), .data$HashDocument %in% hash_) |>
    nrow()
  say_("  ", format(f_, width = 14), format(fmt_(nrow(pop_)), width = 14),
       format(fmt_(length(hash_)), width = 14), format(fmt_(todo_), width = 14),
       pct_(nrow(pop_) - todo_, nrow(pop_)))
}
say_("")
say_("  'copies' is one row per registrant copy -- what a fan-out release holds. 'to classify' is one")
say_("  row per attachment -- what the store holds and what the engine actually reads.")
say_("")


# 7. What the existing store already holds ------------------------------------------------------------------------------

say_("== 7. The existing store ==")
if (!fs::file_exists(path_store_)) {
  say_("  no store yet -- the first pass classifies everything in the chosen population.")
} else {
  con_ <- tryCatch(DBI::dbConnect(duckdb::duckdb(), dbdir = path_store_, read_only = TRUE),
                   error = function(e) NULL)
  if (is.null(con_)) {
    say_("  store present but could not be opened read-only (is a render holding it?)")
  } else {
    on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)
    tabs_ <- DBI::dbListTables(con_)
    say_("tables : ", paste(tabs_, collapse = ", "))
    for (tb_ in intersect(c("corpus", "bert_labels", "kw_labels", "failures", "runs"), tabs_)) {
      n_ <- DBI::dbGetQuery(con_, paste0("SELECT COUNT(*) AS n FROM ", tb_))$n[[1]]
      say_("  ", format(tb_, width = 14), fmt_(n_), " row(s)")
    }
    say_("")
    say_("  This is the baseline the re-run is compared against. Back it up before rendering: the")
    say_("  comparison of new labels to old is the determinism test, and it needs the old ones.")
  }
}
say_("")


# 8. The answer ---------------------------------------------------------------------------------------------------------

say_("== 8. THE ANSWER ==")
say_("")
say_("  one primary per attachment  : ", ok_(one_ok_))
say_("  DocTypeMod covers the tree  : ", if (is.na(type_ok_)) "UNTESTED" else ok_(type_ok_))
say_("  paths resolve               : ", ok_(all(ex_)))
say_("")
say_("  Section 4 decides the design. If ORPHANED is above zero, the pass must classify the primary")
say_("  of every attachment with ANY copy in the population -- not only primaries that are themselves")
say_("  in it -- or those attachments get no label at all.")
say_("")
say_("Reminder: this script wrote nothing.")
