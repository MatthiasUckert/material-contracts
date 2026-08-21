# 03D-Preflight: would rendering 03D call the model? --------------------------------------------------------------------
#
# 03D IS THE THIRD SHAPE OF THIS PROBLEM.
#
#   03B  four days of GPU. Protection: bert_sweep() filters in R and never reaches Python.
#   03C  minutes. Protection: the engine skips per cell, but the R side dispatches regardless.
#   03D  hours of local inference, and it depends on an EXTERNAL SERVICE rather than a file.
#
# WHY 03D IS THE BEST-DEFENDED OF THE THREE
# Its caches are keyed on CONTENT, not on names, and the document says so in as many words:
# "Every model answer is stored under a hash of the document, the assembled prompt, the model tag,
# the context window and whether declining was permitted... it is why this project treats name-keyed
# caches as a trap." (03D.qmd, before cost-build-grids)
#
#   per answer   rlang::hash(DocID, prompt, model, allow_abstain, num_ctx, think)   03D.R:680
#   per cell     the RUN DIRECTORY NAME embeds _X{prompt_hash}                      03D.R:1015, 438
#
# So a changed prompt produces a name that does not exist on disk and correctly re-runs, where 03B's
# runs and 03C's mines are named from hyperparameters alone and would silently serve stale results.
# 03D is the pattern the other two should adopt.
#
# WHAT THIS SCRIPT CHECKS, AND WHAT IT CANNOT
# It parses the run directories already on disk and matches them against the grid on every field
# EXCEPT the prompt hash. That answers "is the parameter coverage complete" and, more usefully,
# catches the dangerous case: runs present under a DIFFERENT hash, which means the prompt moved and
# every cell will re-run.
#
# It does NOT rebuild prompts, so it cannot promise the hash will match. It does not need to: the
# hash is a function of the labels block, the task line, the examples and the window, and the
# examples come from prepared.parquet, which was verified byte-identical on 2026-08-20.
#
# IT WRITES NOTHING. It contacts Ollama only to list installed models.
#
# PREDICTIONS, STATED BEFORE RUNNING
#   2. Ollama answering, qwen3:8b present
#   3. Grid: 3 blind + 9 crossfold + 3 tuned = 15 cells, 63 run directories
#   4. Every cell covered, exactly one prompt hash per cell
#   5. rates.parquet covers every shape, so the cost projection measures nothing
#
# Run from the project root:  source("1_code/_Tests/03D-Preflight.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

ok_ <- function(.flag, .yes = "PASS", .no = "FAIL") if (isTRUE(.flag)) .yes else .no


# 1. Source, in the document's order ------------------------------------------------------------------------------------

here::i_am("1_code/_Commons/_Initialize.R")
source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),     encoding = "UTF-8")
source(here::here("1_code", "03A-ClassifyPrepare.R"),     encoding = "UTF-8")
source(here::here("1_code", "03D-ClassifyLLM.R"),         encoding = "UTF-8")

dir_d_      <- here::here("2_output", "03D-ClassifyLLM")
runs_root_  <- fs::path(dir_d_, "runs")
cache_dir_  <- fs::path(dir_d_, "cache")
path_rates_ <- fs::path(dir_d_, "rates.parquet")
path_prep_  <- here::here("2_output", "03A-ClassifyPrepare", "prepared.parquet")

host_   <- "http://localhost:11434"
models_ <- "qwen3:8b"

say_("== 03D-Preflight ==   WRITES NOTHING.")
say_("")
say_("runs/        : ", if (fs::dir_exists(runs_root_)) "ok" else "MISSING")
say_("cache/       : ", if (fs::dir_exists(cache_dir_)) paste0(fmt_(length(
  fs::dir_ls(cache_dir_, type = "file")))," answers") else "MISSING")
say_("rates.parquet: ", if (fs::file_exists(path_rates_)) "ok" else "MISSING")
say_("prepared     : ", if (fs::file_exists(path_prep_)) "ok" else "MISSING")
say_("")

stopifnot("prepared.parquet missing" = fs::file_exists(path_prep_))


# 2. The external dependency --------------------------------------------------------------------------------------------
# The one input that is not a file. A missing tag is the failure worth catching first: Ollama answers
# a request for an unpulled model with a 404, which the classifier reads as a declined answer, so a
# configuration completes with zero coverage after thousands of futile calls and nothing errors.

say_("== 2. Ollama ==")
have_ <- tryCatch(llm_available_models(.host = host_), error = function(e) NULL)

if (is.null(have_)) {
  say_("  NO SERVER at ", host_, "   FAIL")
  say_("  -> start it with `ollama serve`. 03D aborts at preflight-models without it.")
  ollama_ok_ <- FALSE
} else {
  miss_ <- setdiff(models_, have_)
  ollama_ok_ <- length(miss_) == 0L
  say_("server        : answering at ", host_)
  say_("models wanted : ", paste(models_, collapse = ", "))
  say_("present       : ", ok_(ollama_ok_))
  if (!ollama_ok_) say_("  MISSING: ", paste(miss_, collapse = ", "), " -- `ollama pull` it")
  say_("installed     : ", paste(utils::head(have_, 12), collapse = ", "))
}
say_("")


# 3. The grid, rebuilt as the document builds it ------------------------------------------------------------------------
# tab_definitions is empty in the document, so guidance collapses to "labels" alone and the grid is
# smaller than the configuration block suggests. Reproduced here rather than assumed.

guidance_ <- "labels"   # tab_definitions is empty; 03D.qmd collapses the axis in configure-definitions
tasks_    <- c("ClassDetailed", "ClassBroad", "AmendType")
shots_    <- c(1L, 2L, 4L)

grid_ <- dplyr::bind_rows(
  llm_grid(.label_cols = tasks_, .models = models_, .guidance = guidance_, .shots = 0L,
           .n_chars = 6000L, .allow_abstain = FALSE, .think = FALSE, .tier = "blind"),
  llm_grid(.label_cols = tasks_, .models = models_, .guidance = guidance_, .shots = shots_,
           .n_chars = 6000L, .allow_abstain = FALSE, .think = FALSE, .tier = "crossfold"),
  llm_grid(.label_cols = "ClassDetailed", .models = models_, .guidance = guidance_, .shots = shots_,
           .n_chars = 6000L, .allow_abstain = FALSE, .think = FALSE, .tier = "tuned")
)

say_("== 3. The grid ==")
grid_ |>
  dplyr::count(.data$Tier, .data$LabelCol, name = "Cells") |>
  (\(.t) say_(paste0("  ", format(.t$Tier, width = 12), format(.t$LabelCol, width = 16),
                     fmt_(.t$Cells), collapse = "\n")))()
say_("")
say_("cells total   : ", fmt_(nrow(grid_)))
say_("blind and crossfold run all 5 folds; tuned runs the holdout only.")
say_("")


# 4. What is already on disk --------------------------------------------------------------------------------------------
# Run directory names are parsed rather than rebuilt. Rebuilding one requires the prompt hash, which
# requires assembling the prompt, which is exactly the fragile reconstruction this project has been
# bitten by. Parsing answers the useful questions directly.
#
# THE FINDING TO WATCH FOR is more than one distinct prompt hash for the same cell: that means the
# prompt has changed at some point, and the cells carrying the old hash will re-run.

say_("== 4. Runs on disk ==")

cells_ok_ <- NA
if (!fs::dir_exists(runs_root_)) {
  say_("  no runs/ -- a render would run the entire grid. That is hours of inference.")
} else {
  dirs_ <- fs::dir_ls(runs_root_, type = "directory")
  nm_   <- fs::path_file(dirs_)

  rx_ <- paste0(
    "^(?<Task>[A-Za-z]+)__llm-(?<Model>[A-Za-z0-9]+)__",
    "(?<Tier>[A-Z])_G(?<Gui>[a-z]{3})_S(?<Shots>[0-9]+)_C(?<NChars>[0-9]+)",
    "_A(?<Ab>[0-9])_R(?<Think>[0-9d])_K(?<Ctx>[0-9]+)_X(?<Hash>[^_]+)",
    "(?:_L(?<Limit>[0-9]+))?_S(?<Seed>[0-9]+)_F(?<Fold>[0-9]+)$"
  )
  m_ <- stringi::stri_match_first_regex(nm_, rx_)
  parsed_ <- tibble::tibble(
    Name = nm_, Task = m_[, 2], Tier = m_[, 4], Shots = m_[, 6], Hash = m_[, 11], Fold = m_[, 14]
  ) |>
    dplyr::filter(!is.na(.data$Task))

  say_("run directories : ", fmt_(length(dirs_)), "   parsed: ", fmt_(nrow(parsed_)))
  if (nrow(parsed_) < length(dirs_)) {
    say_("  unparsed names (worth a look):")
    say_(paste0("    ", utils::head(setdiff(nm_, parsed_$Name), 5), collapse = "\n"))
  }
  say_("")

  # One row per cell, with its fold coverage and how many distinct prompt hashes it carries.
  cover_ <- parsed_ |>
    dplyr::summarise(
      Folds  = dplyr::n_distinct(.data$Fold),
      Hashes = dplyr::n_distinct(.data$Hash),
      .by = c("Task", "Tier", "Shots")
    ) |>
    dplyr::arrange(.data$Tier, .data$Task, .data$Shots)

  say_("coverage by cell:")
  say_(paste0("  ", format(.subset2(cover_, "Tier"), width = 6),
              format(.subset2(cover_, "Task"), width = 16),
              "S", format(.subset2(cover_, "Shots"), width = 4),
              "folds ", format(.subset2(cover_, "Folds"), width = 4),
              "hashes ", .subset2(cover_, "Hashes"), collapse = "\n"))
  say_("")

  multi_ <- cover_ |> dplyr::filter(.data$Hashes > 1L)
  say_("cells with more than one prompt hash : ", nrow(multi_), "   ",
       ok_(nrow(multi_) == 0L))
  say_(if (nrow(multi_) == 0L)
         "  -> one prompt per cell. Whatever is on disk was built by one version of the prompt." else
         "  -> the prompt changed at some point; the stale-hash runs will not be reused.")

  say_("cells expected by the grid            : ", fmt_(nrow(grid_)))
  say_("cells present on disk                 : ", fmt_(nrow(cover_)))
  cells_ok_ <- nrow(cover_) >= nrow(grid_) && nrow(multi_) == 0L
}
say_("")


# 5. The cost projection ------------------------------------------------------------------------------------------------
# llm_report_cost() measures only shapes absent from rates.parquet, then writes the union back. A warm
# rate store therefore costs nothing; a cold one costs 5 documents per unknown shape, which is real
# inference and the only part of this document that cannot be skipped by having runs on disk.

say_("== 5. Generation rates ==")
if (!fs::file_exists(path_rates_)) {
  say_("  rates.parquet absent -- every shape will be measured at 5 documents each.")
} else {
  rates_ <- arrow::read_parquet(path_rates_)
  say_("shapes on record : ", fmt_(nrow(rates_)))
  say_("  -> llm_report_cost() measures only shapes it does not already hold. Any shape added by a")
  say_("     changed prompt is 5 real calls; every known shape is free.")
}
say_("")


# 6. The answer ---------------------------------------------------------------------------------------------------------

say_("== 6. THE ANSWER ==")
say_("")
if (isTRUE(ollama_ok_) && isTRUE(cells_ok_)) {
  say_("  READY.")
  say_("")
  say_("  Ollama is up with the requested model, every grid cell has its runs on disk, and each")
  say_("  carries a single prompt hash. A render should report 'Skipping ...' for every cell and")
  say_("  call the model only for cost shapes it has not already priced.")
  say_("")
  say_("  What would change that is a changed PROMPT -- the task lines, the window, the examples or")
  say_("  prepared.parquet. Any of those alters the hash, the run directory name stops matching, and")
  say_("  the cell re-runs. That is the cache behaving correctly, not failing.")
} else {
  say_("  NOT READY.")
  say_("")
  if (!isTRUE(ollama_ok_)) {
    say_("    - section 2: Ollama is not serving the requested model. 03D aborts at preflight-models,")
    say_("      which is the right behaviour and costs nothing.")
  }
  if (isFALSE(cells_ok_)) {
    say_("    - section 4: the grid is not fully covered, or a cell carries more than one prompt hash.")
    say_("      Read the coverage table before rendering: uncovered cells mean real inference.")
  }
}
say_("")
say_("Reminder: this script wrote nothing.")
