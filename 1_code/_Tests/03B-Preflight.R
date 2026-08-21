# 03B-Preflight: would rendering 03B train anything, or delete anything? -------------------------------------------------
#
# WHY THIS EXISTS
# 03A could not destroy anything: its one write was guarded and the render was provably a no-op on
# disk. 03B is different. The BERT sweep took four days and CANNOT BE BACKED UP at a sensible cost,
# so the protection is not a copy to restore from -- it is not triggering the work in the first
# place. This script establishes that, and it does so WITHOUT rendering 03B.
#
# THREE SWITCHES SPEND SOMETHING, NOT TWO
# The document's own prose names two. There is a third, and it is the dangerous one:
#
#   RunSweep   = TRUE   trains every CV configuration not already on disk
#   RunDeploy  = TRUE   fits every crowned final model not already on disk
#   PruneFinal = TRUE   *** fs::dir_delete()s any *__FINAL model no longer crowned ***
#
# PruneFinal does not spend GPU time. It destroys the product of it. A shift in which configuration
# is crowned would delete a fitted model, and refitting it is not free.
#
# WHAT MAKES THIS SAFE TO RUN
# bert_sweep() decides ENTIRELY IN R. It builds a RowKey per grid row, reads bert_done_keys() off the
# runs directory, and returns early -- printing "Nothing to do" -- when nothing is outstanding. The
# Python interpreter is reached only inside bert_run(), which is called only for outstanding rows.
#
# THIS HAS A CONSEQUENCE FOR F2: the contracts-engine -> contracts-classify rename CANNOT trigger
# training, because the engine path is never evaluated for a run that already exists. The rename is
# safe to make; what it cannot be allowed to change is .runs_root, which is where the history lives.
#
# This script calls the document's own bert_run_key(), bert_done_keys(), clf_load_overall() and
# bert_crowned_config(). It reproduces bert_prune_final()'s decision rule WITHOUT calling it, because
# that function's Removed column is gated on .prune and so reads FALSE on a dry run whatever the
# intent. It never calls bert_run(), bert_sweep(), bert_fit_final() or bert_fit_final_all().
#
# IT WRITES NOTHING AND DELETES NOTHING.
#
# PREDICTIONS, STATED BEFORE RUNNING
#   3. Outstanding CV runs        0 of 480
#   4. Crowned configs 6; models the prune would delete 0
#   5. Outstanding final fits     0, and config.json present on all 6
#   6. The new Python folder answers --help
#
# Run from the project root:  source("1_code/_Tests/03B-Preflight.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

ok_ <- function(.flag, .yes = "PASS", .no = "FAIL") if (isTRUE(.flag)) .yes else .no


# 1. Source, in the document's order ------------------------------------------------------------------------------------
# 03B's library registers nothing at load time, but 03A's does -- it calls plot_register_levels()
# against _Plots.R's registry -- and 03B sources 03A. Follow the document's order or this is not the
# environment the render sees.

here::i_am("1_code/_Commons/_Initialize.R")
source(here::here("1_code", "_Commons", "_Initialize.R"),   encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),        encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),        encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),       encoding = "UTF-8")
source(here::here("1_code", "03A-ClassifyPrepare.R"),       encoding = "UTF-8")
source(here::here("1_code", "03B-ClassifyTrainBERT.R"),     encoding = "UTF-8")

dir_b_     <- here::here("2_output", "03B-ClassifyTrainBERT")
runs_root_ <- fs::path(dir_b_, "runs")
model_dir_ <- fs::path(dir_b_, "model_final")
path_prep_ <- here::here("2_output", "03A-ClassifyPrepare", "prepared.parquet")

# Both spellings, because F2 has not landed yet and either may be on disk.
dir_engine_old_ <- here::here("contracts-engine")
dir_engine_new_ <- here::here("contracts-classify")

say_("== 03B-Preflight ==   WRITES NOTHING, DELETES NOTHING.")
say_("")
say_("runs root   : ", fs::path_rel(runs_root_, here::here()),
     "  [", if (fs::dir_exists(runs_root_)) "ok" else "MISSING", "]")
say_("model_final : ", fs::path_rel(model_dir_, here::here()),
     "  [", if (fs::dir_exists(model_dir_)) "ok" else "MISSING", "]")
say_("prepared    : [", if (fs::file_exists(path_prep_)) "ok" else "MISSING", "]")
say_("")

stopifnot("runs root missing" = fs::dir_exists(runs_root_))


# 2. Rebuild the grid exactly as the document does ----------------------------------------------------------------------
# Copied from 03B.qmd's build-grid-full and build-grid chunks. If those chunks change, change this.

grid_full_ <- tidyr::expand_grid(
  model         = c("nlpaueb/legal-bert-base-uncased", "roberta-base"),
  label_col     = c("ClassDetailed", "ClassBroad", "AmendType"),
  lr            = c(1e-5, 2e-5, 3e-5),
  max_len       = c(256L, 512L),
  epochs        = c(3, 6, 10),
  class_weights = c(FALSE, TRUE),
  fold          = 1:5
)

grid_all_ <- grid_full_ |> dplyr::filter(.data$epochs != 10, .data$lr != 1e-5)

say_("== 2. The grid ==")
say_("full factorial    : ", fmt_(nrow(grid_full_)))
say_("after exclusions  : ", fmt_(nrow(grid_all_)), "   (epochs != 10, lr != 1e-5)")
say_("")


# 3. How many CV runs would the sweep actually start? -------------------------------------------------------------------
# PREDICTION: zero. The runs tree has not been touched since 2026-08-13 and the grid has not changed.
# Uses the document's own key functions rather than a reimplementation, so a match here is a match on
# the real comparison and not on my copy of it.

keys_want_ <- bert_run_key(
  grid_all_$label_col, grid_all_$model, "Text", grid_all_$max_len,
  grid_all_$epochs, 32L, grid_all_$lr, grid_all_$class_weights, 42L, grid_all_$fold
)

keys_done_ <- bert_done_keys(.runs_roots = runs_root_)

todo_ <- setdiff(keys_want_, keys_done_)

say_("== 3. Outstanding CV runs (RunSweep) ==")
say_("requested by grid : ", fmt_(length(keys_want_)))
say_("found on disk     : ", fmt_(length(keys_done_)))
say_("WOULD TRAIN       : ", fmt_(length(todo_)), "   ", ok_(length(todo_) == 0L))
say_(if (length(todo_) == 0L)
       "  -> bert_sweep() returns 'Nothing to do' and never reaches Python." else
       "  -> a render WOULD start training. Do not render 03B until this is zero.")

if (length(todo_) > 0L) {
  say_("")
  say_("  first 10 that would run:")
  say_(paste0("    ", utils::head(todo_, 10), collapse = "\n"))
}
say_("")


# 4. The crown, and what the prune would delete (PruneFinal) --------------------------------------------------------------
# bert_crowned_config() returns a NAMED LIST (config_name, label_col, max_len, ...), not a tibble --
# the first draft of this probe assumed a tibble and died in list_rbind(). The crown TABLE is built
# inside bert_fit_final_all(), one row per task-and-length, and that construction is reproduced here.
#
# PREDICTION: 6 crowned configs (3 tasks x 2 lengths), and nothing to delete. The crown is computed
# from metrics already on disk and those have not moved since 2026-08-13.

say_("== 4. The crown ==")

crown_       <- NULL
prune_ok_    <- NA
tab_overall_ <- tryCatch(clf_load_overall(.runs_roots = runs_root_), error = function(e) NULL)

if (is.null(tab_overall_)) {
  say_("  cannot evaluate -- no metrics under runs/")
} else {
  # Same plan as bert_fit_final_all(): drop any task-length combination with no completed run.
  plan_ <- tidyr::expand_grid(
    Task   = c("ClassDetailed", "ClassBroad", "AmendType"),
    MaxLen = c(256L, 512L)
  ) |>
    dplyr::mutate(
      Runs = purrr::map2_int(.data$Task, .data$MaxLen, \(.t, .m) {
        sum(tab_overall_$LabelCol == .t & as.integer(tab_overall_$MaxLen) == .m)
      })
    ) |>
    dplyr::filter(.data$Runs > 0L)

  crown_ <- purrr::pmap(
    dplyr::select(plan_, "Task", "MaxLen"),
    \(Task, MaxLen) {
      cfg_ <- bert_crowned_config(.tab_overall = tab_overall_, .label_col = Task, .max_len = MaxLen)
      tibble::tibble(
        LabelCol   = cfg_$label_col,
        MaxLen     = cfg_$max_len,
        ConfigName = cfg_$config_name
      )
    }
  ) |>
    purrr::list_rbind()

  say_("task-length combinations with runs : ", nrow(plan_), " of 6")
  say_("crowned configurations             : ", nrow(crown_))
  say_(paste0("    ", format(crown_$LabelCol, width = 16), "L", crown_$MaxLen, "  ",
              crown_$ConfigName, collapse = "\n"))
  say_("")
}


say_("== 4b. What the prune would delete ==")

if (is.null(crown_) || !fs::dir_exists(model_dir_)) {
  say_("  cannot evaluate -- no crown table or no model_final/")
} else {
  # bert_prune_final()'s own logic, minus the deletion. Removed = !Keep & Ready & .prune, so with
  # .prune = FALSE the Removed column is FALSE everywhere by construction and the INTENT has to be
  # recomputed. Ready matters and the first draft ignored it: a directory is only ever removed once
  # every crowned config for its task is fitted, so an unfitted crown protects its own task's
  # leftovers.
  found_ <- fs::dir_ls(model_dir_, type = "directory", glob = "*__FINAL")
  if (length(found_) == 0L) {
    say_("  no *__FINAL directories under model_final/ -- nothing to prune")
    prune_ok_ <- TRUE
  } else {
    found_ <- tibble::tibble(
      Path       = as.character(found_),
      ConfigName = sub("__FINAL$", "", fs::path_file(found_))
    ) |>
      dplyr::mutate(LabelCol = sub("__.*$", "", .data$ConfigName))

    ready_ <- crown_ |>
      dplyr::mutate(
        Fitted = unname(fs::dir_exists(
          fs::path(model_dir_, paste0(.data$ConfigName, "__FINAL"), "model")
        ))
      ) |>
      dplyr::summarise(Ready = all(.data$Fitted), .by = "LabelCol")

    intent_ <- found_ |>
      dplyr::mutate(Keep = .data$ConfigName %in% crown_$ConfigName) |>
      dplyr::left_join(y = ready_, by = dplyr::join_by(LabelCol)) |>
      dplyr::mutate(
        Ready       = dplyr::coalesce(.data$Ready, FALSE),
        WouldRemove = !.data$Keep & .data$Ready
      )

    say_("models under model_final/ : ", nrow(intent_))
    say_("still crowned             : ", sum(intent_$Keep))
    say_("WOULD DELETE              : ", sum(intent_$WouldRemove), "   ",
         ok_(sum(intent_$WouldRemove) == 0L))

    if (any(intent_$WouldRemove)) {
      say_("")
      say_(paste0("    ", fs::path_rel(intent_$Path[intent_$WouldRemove], here::here()),
                  collapse = "\n"))
      say_("  -> set PruneFinal = FALSE before rendering, and work out why the crown moved.")
    } else {
      say_("  -> nothing would be removed.")
    }

    # Uncrowned but protected: worth seeing, because it becomes deletable the moment its task's
    # crowns are all fitted.
    held_ <- intent_ |> dplyr::filter(!.data$Keep, !.data$Ready)
    if (nrow(held_) > 0L) {
      say_("")
      say_("  NOTE: ", nrow(held_), " uncrowned model(s) survive only because their task has an")
      say_("  unfitted crown. They become deletable as soon as it is fitted:")
      say_(paste0("    ", held_$ConfigName, collapse = "\n"))
    }

    prune_ok_ <- sum(intent_$WouldRemove) == 0L
  }
}
say_("")


# 5. Outstanding final fits (RunDeploy) ------------------------------------------------------------------------------------
# THE MARKER THAT DECIDES IS config.json, NOT THE model/ DIRECTORY, and the first draft checked the
# wrong one.
#
# bert_fit_final_all() has NO R-side done check: with .run_remaining = TRUE it walks every crowned
# config and calls bert_fit_final() for each, unconditionally. The skip therefore happens inside
# Python, and classify_train.py in --fit-final mode uses
#
#     done_marker = run_dir / "config.json"          (CV mode uses metrics_overall.parquet)
#
# with run_dir = <model_dir>/<config_name>__FINAL. The model/ subdirectory is what R reports to the
# reader; config.json is what the engine actually tests. A config with model/ but no config.json
# would be reported as fitted and refitted anyway.
#
# PREDICTION: all 6 carry config.json, and model/ agrees with it on every row.

say_("== 5. Outstanding final fits (RunDeploy) ==")

deploy_ok_ <- NA
if (!is.null(crown_) && nrow(crown_) > 0L && fs::dir_exists(model_dir_)) {
  dir_run_ <- fs::path(model_dir_, paste0(crown_$ConfigName, "__FINAL"))

  has_marker_ <- unname(fs::file_exists(fs::path(dir_run_, "config.json")))   # what Python tests
  has_model_  <- unname(fs::dir_exists(fs::path(dir_run_, "model")))          # what R reports

  say_("crowned                       : ", nrow(crown_))
  say_("with config.json (the marker) : ", sum(has_marker_))
  say_("with model/ (what R reports)  : ", sum(has_model_))
  say_("WOULD FIT                     : ", sum(!has_marker_), "   ", ok_(all(has_marker_)))

  if (any(!has_marker_)) {
    say_("")
    say_("  no config.json -- the engine would refit these:")
    say_(paste0("    ", crown_$ConfigName[!has_marker_], collapse = "\n"))
  }

  # The dangerous disagreement: R says fitted, Python says not.
  lying_ <- has_model_ & !has_marker_
  if (any(lying_)) {
    say_("")
    say_("  WARNING: ", sum(lying_), " config(s) have model/ but no config.json. The document would")
    say_("  report them as deployed while the engine refits them. Investigate before rendering:")
    say_(paste0("    ", crown_$ConfigName[lying_], collapse = "\n"))
  }

  deploy_ok_ <- all(has_marker_)
} else {
  say_("  cannot evaluate -- no crown table or no model_final/")
}
say_("")


# 6. Does the renamed Python folder work? -------------------------------------------------------------------------------
# --help parses arguments and exits. It never reads data, never builds a run directory, never touches
# the GPU. This is the only place the interpreter is invoked, and it is invoked with the one flag
# that cannot do anything.

say_("== 6. The Python folder ==")

for (nm_ in c("contracts-engine", "contracts-classify")) {
  d_ <- here::here(nm_)
  py_ <- fs::path(d_, ".venv", "bin", "python")
  sc_ <- fs::path(d_, "classify_train.py")
  say_("  ", format(nm_, width = 20),
       "dir ", if (fs::dir_exists(d_)) "yes" else "no ",
       "  venv ", if (fs::file_exists(py_)) "yes" else "no ",
       "  script ", if (fs::file_exists(sc_)) "yes" else "no")
}
say_("")

py_new_ <- fs::path(dir_engine_new_, ".venv", "bin", "python")
sc_new_ <- fs::path(dir_engine_new_, "classify_train.py")
engine_ok_ <- NA

if (fs::file_exists(py_new_) && fs::file_exists(sc_new_)) {
  out_h_ <- suppressWarnings(
    system2(py_new_, args = c(sc_new_, "--help"), stdout = TRUE, stderr = TRUE)
  )
  st_ <- attr(out_h_, "status")
  engine_ok_ <- (is.null(st_) || st_ == 0L) && any(grepl("--runs-root", out_h_))
  say_("  contracts-classify answers --help : ", ok_(engine_ok_))
  if (!isTRUE(engine_ok_)) say_(paste0("    ", utils::head(out_h_, 12), collapse = "\n"))
} else {
  say_("  contracts-classify not usable yet -- F2 has not landed, or the venv is not built.")
}
say_("")
say_("  NOTE: --runs-root defaults to '2_output/03-Classification/runs' inside the engine, which is")
say_("  NOT where the history lives. The R side always passes it explicitly. When F2 lands, change")
say_("  ONLY .python and .script. Touching .runs_root would hide four days of runs from the engine.")
say_("")


# 7. The answer ---------------------------------------------------------------------------------------------------------

safe_ <- length(todo_) == 0L && isTRUE(prune_ok_) && isTRUE(deploy_ok_)

say_("== 7. THE ANSWER ==")
say_("")
if (safe_) {
  say_("  SAFE TO RENDER.")
  say_("")
  say_("  Nothing outstanding: the sweep returns 'Nothing to do' without reaching Python, every")
  say_("  crowned model is already fitted, and the prune has nothing to remove. All three switches")
  say_("  can stay TRUE and the render will spend no GPU time and delete nothing.")
  say_("")
  say_("  F2 IS REQUIRED, NOT OPTIONAL. contracts-engine no longer exists on disk, and")
  say_("  bert_fit_final_all() has no R-side done check: it walks every crowned config and calls")
  say_("  bert_fit_final(), which aborts on its file_exists(.python) test. 03B stops at the deploy")
  say_("  step, not the sweep. Change .python and .script only; leave .runs_root exactly as it is.")
} else {
  say_("  NOT SAFE. Set RunSweep, RunDeploy and PruneFinal to FALSE before rendering, then:")
  say_("")
  if (length(todo_) > 0L)      say_("    - section 3: a render would TRAIN ", fmt_(length(todo_)), " configuration(s).")
  if (isFALSE(prune_ok_))      say_("    - section 4: a render would DELETE fitted models.")
  if (isFALSE(deploy_ok_))     say_("    - section 5: a render would FIT final models.")
  say_("")
  say_("  None of these is a reason to panic -- with the switches off, 03B reports its plan and")
  say_("  spends nothing. But find out WHY before turning them back on.")
}
say_("")
say_("Reminder: this script wrote nothing and deleted nothing.")
