# 03C-Preflight: can 03C render, and would it re-mine anything? ----------------------------------------------------------
#
# 03C IS NOT 03B, AND THE DIFFERENCE MATTERS BOTH WAYS.
#
# CHEAPER. A full re-mine of the grid is minutes, not days -- the document's own note says so and the
# sweep runs 24 mirai daemons. Nothing here is unrecoverable, so this is not a script about
# protecting an expensive artifact.
#
# MORE FRAGILE. Three things make 03C harder to render than 03B:
#
#   1. THE SMOKE CELL RUNS UNCONDITIONALLY. `mine-smoke-one-cell` is the first execution chunk and it
#      always invokes Python. 03C therefore fails at the TOP of the document if the engine path is
#      wrong, not at the end like 03B did.
#
#   2. EVERY CELL IS DISPATCHED. Unlike bert_sweep(), which filters in R and never reaches Python for
#      a run that exists, kw_mine_sweep() builds a command for EVERY grid row and dispatches all of
#      them; the skip is decided inside Python, per cell. So a render starts ~720 processes whatever
#      is on disk.
#
#   3. THE TERM LISTS ARE OUT OF REPO. Three parquets under a Dropbox path feed everything from
#      `generated-evaluate` onward -- nine call sites. If they are missing the document dies halfway.
#      This is D2b, the twin of the label-spine decision already settled for 03A.
#
# A REPORTING WEAKNESS WORTH KNOWING BEFORE READING THE RENDER
# kw_mine_sweep() maps with `stdout = FALSE, stderr = FALSE`, so the engine's per-cell output is
# discarded, and it reports "Mining complete: N/720 cells" whether it mined 720 or skipped 720. The
# count is of successful exits, not of work done. THE ONLY SIGNAL IS ELAPSED MINUTES. Section 4 below
# tells you in advance what that number should be.
#
# IT WRITES NOTHING. It invokes Python once per script, with --help.
#
# PREDICTIONS, STATED BEFORE RUNNING
#   2. prepared.parquet resolves; the three term lists are the open question
#   3. F2 not yet landed, so contracts-engine is still named in the config
#   4. Every grid cell already mined -> the sweep should report in seconds, not minutes
#
# Run from the project root:  source("1_code/_Tests/03C-Preflight.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

ok_ <- function(.flag, .yes = "PASS", .no = "FAIL") if (isTRUE(.flag)) .yes else .no


# 1. Source, in the document's order ------------------------------------------------------------------------------------

here::i_am("1_code/_Commons/_Initialize.R")
source(here::here("1_code", "_Commons", "_Initialize.R"),     encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),          encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),          encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),         encoding = "UTF-8")
source(here::here("1_code", "03A-ClassifyPrepare.R"),         encoding = "UTF-8")
source(here::here("1_code", "03C-ClassifyTrainKeyword.R"),    encoding = "UTF-8")

dir_c_      <- here::here("2_output", "03C-ClassifyTrainKeyword")
mines_root_ <- fs::path(dir_c_, "mines")
path_prep_  <- here::here("2_output", "03A-ClassifyPrepare", "prepared.parquet")

# The out-of-repo term lists, exactly as 03C.qmd:132 declares them.
dir_cowork_ <- fs::path(
  "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MatContractData",
  "KeyWordsClaude"
)
terms_ <- c(
  Detailed = fs::path(dir_cowork_, "detailed",  "cowork_terms_detailed.parquet"),
  Broad    = fs::path(dir_cowork_, "broad",     "cowork_terms_broad.parquet"),
  Amend    = fs::path(dir_cowork_, "amendment", "cowork_terms_amendment.parquet")
)

say_("== 03C-Preflight ==   WRITES NOTHING.")
say_("")


# 2. Does everything it reads exist? ------------------------------------------------------------------------------------
# The term lists are the blocker. Nine call sites from `generated-evaluate` onward read them, so their
# absence does not fail early and cleanly -- it fails two thirds of the way down.

say_("== 2. Inputs ==")
say_("prepared.parquet  : ", if (fs::file_exists(path_prep_)) "ok" else "MISSING")
say_("mines/            : ", if (fs::dir_exists(mines_root_)) "ok" else "MISSING")
say_("")
say_("term lists (D2b, still out of repo):")
for (nm_ in names(terms_)) {
  say_("  ", format(nm_, width = 10), if (fs::file_exists(terms_[[nm_]])) "ok " else "MISSING",
       "  ", fs::path_file(terms_[[nm_]]))
}
terms_ok_ <- all(fs::file_exists(terms_))
say_("")
say_("all three present  : ", ok_(terms_ok_))
say_(if (terms_ok_)
       "  -> reachable today, but a reviewer cannot obtain them. Same class as the label spine." else
       "  -> 03C CANNOT RENDER past `generated-evaluate`. Move them in-repo before anything else.")
say_("")


# 3. F2 -----------------------------------------------------------------------------------------------------------------
# 03C needs THREE scripts, not one: keyword_train.py for mining, keyword_apply.py for the applier, and
# the config names keyword_text.py's regime too. Five sites carry the old folder name, two of them
# inside cli error messages that would send a reader to a directory that no longer exists.

say_("== 3. The Python folder ==")
for (nm_ in c("contracts-engine", "contracts-classify")) {
  d_ <- here::here(nm_)
  say_("  ", format(nm_, width = 20), "dir ", if (fs::dir_exists(d_)) "yes" else "no ",
       "  venv ", if (fs::file_exists(fs::path(d_, ".venv", "bin", "python"))) "yes" else "no ",
       "  keyword_train ", if (fs::file_exists(fs::path(d_, "keyword_train.py"))) "yes" else "no ",
       "  keyword_apply ", if (fs::file_exists(fs::path(d_, "keyword_apply.py"))) "yes" else "no")
}
say_("")

py_ <- here::here("contracts-classify", ".venv", "bin", "python")
engine_ok_ <- NA
if (fs::file_exists(py_)) {
  engine_ok_ <- TRUE
  for (sc_ in c("keyword_train.py", "keyword_apply.py")) {
    p_ <- here::here("contracts-classify", sc_)
    if (!fs::file_exists(p_)) { say_("  ", sc_, " : MISSING"); engine_ok_ <- FALSE; next }
    out_ <- suppressWarnings(system2(py_, args = c(p_, "--help"), stdout = TRUE, stderr = TRUE))
    st_  <- attr(out_, "status")
    good_ <- is.null(st_) || st_ == 0L
    say_("  ", format(sc_, width = 18), "answers --help : ", ok_(good_))
    if (!good_) say_(paste0("      ", utils::head(out_, 8), collapse = "\n"))
    engine_ok_ <- engine_ok_ && good_
  }
} else {
  say_("  contracts-classify venv not found")
  engine_ok_ <- FALSE
}
say_("")
say_("  F2 sites to change: 03C.qmd:159, 160, 163 (config) and 03C.R:660, 667 (cli messages).")
say_("  The two in .R are inside error text that would name a folder that no longer exists.")
say_("")


# 4. Would the sweep actually mine anything? ----------------------------------------------------------------------------
# PREDICTION: nothing. Every cell is on disk, so all ~720 dispatched processes hit the engine's skip
# path and the sweep reports in seconds.
#
# Note what this section can and cannot promise. It says how many cells are MISSING. It does NOT say
# the existing ones match the current sample -- a mine is named after its hyperparameters and carries
# no fingerprint of the data, which the document's own prose warns about. 03A's prepared.parquet was
# verified byte-identical on 2026-08-20, so the mines are consistent with it; that is an argument from
# the 03A verification, not something this section establishes.

say_("== 4. The mining grid ==")

mine_ok_ <- NA
if (!fs::dir_exists(mines_root_)) {
  say_("  no mines/ directory -- a render would mine the whole grid (minutes)")
} else {
  grid_ <- kw_mine_grid(
    .label_cols   = c("ClassDetailed", "ClassBroad", "AmendType"),
    .nwords_text  = c(256L, 512L, 1024L, 2048L, 0L),
    .ngram_max    = c(2L, 3L),
    .stopwords    = c("none", "english_domain"),
    .folds        = 1:5,
    .with_alldata = TRUE
  )

  idx_ <- tryCatch(kw_mine_index(.mines_root = mines_root_), error = function(e) NULL)

  if (is.null(idx_)) {
    say_("  no mine.json manifests found -- a render would mine the whole grid")
  } else {
    key_ <- c("LabelCol", "Source", "NWords", "NgramMax", "Stopwords", "Fold")
    have_ <- idx_ |> dplyr::select(dplyr::all_of(key_)) |> dplyr::distinct()
    todo_ <- dplyr::anti_join(grid_, have_, by = key_)

    say_("grid cells        : ", fmt_(nrow(grid_)))
    say_("mines on disk     : ", fmt_(nrow(have_)))
    say_("WOULD MINE        : ", fmt_(nrow(todo_)), "   ", ok_(nrow(todo_) == 0L))
    say_(if (nrow(todo_) == 0L)
           "  -> expect 'Mining complete: N/N cells in 0-something min'. Seconds, not minutes." else
           "  -> the sweep will do real work. Cheap, but it should be expected rather than a surprise.")

    if (nrow(todo_) > 0L) {
      say_("")
      todo_ |>
        dplyr::count(.data$LabelCol, .data$Source, name = "Cells") |>
        (\(.t) say_(paste0("    ", format(.t$LabelCol, width = 16),
                           format(.t$Source, width = 10), fmt_(.t$Cells), collapse = "\n")))()
    }

    # Orphans: mines with no row in the grid. Not a fault -- superseded cells are kept as record --
    # but worth seeing, because the index is what several later sections read.
    orph_ <- dplyr::anti_join(have_, grid_, by = key_)
    say_("")
    say_("mines outside the grid : ", fmt_(nrow(orph_)),
         if (nrow(orph_) > 0L) "   (kept as record; not a fault)" else "")

    mine_ok_ <- nrow(todo_) == 0L
  }
}
say_("")


# 5. The answer ---------------------------------------------------------------------------------------------------------

say_("== 5. THE ANSWER ==")
say_("")

blockers_ <- c(
  if (!isTRUE(terms_ok_))  "the three term lists (D2b)",
  if (!isTRUE(engine_ok_)) "a working contracts-classify engine"
)

if (length(blockers_) == 0L) {
  say_("  READY, once F2 lands.")
  say_("")
  say_("  Everything 03C reads is reachable and both engine scripts answer. The smoke cell at the")
  say_("  top of the document is what will fail first if F2 is not applied, so apply it before")
  say_("  rendering rather than finding out on line 232.")
  if (isTRUE(mine_ok_)) {
    say_("")
    say_("  The sweep will dispatch every cell and mine none of them. If it reports minutes rather")
    say_("  than seconds, something re-mined and the cause is worth finding before reading further.")
  }
} else {
  say_("  NOT READY. Missing: ", paste(blockers_, collapse = "; "))
  say_("")
  say_("  The term lists are the harder one. They are read at nine sites from `generated-evaluate`")
  say_("  onward, so their absence does not stop the document early -- it stops it two thirds of the")
  say_("  way down, after the mining sweep has already run.")
}
say_("")
say_("Reminder: this script wrote nothing.")
