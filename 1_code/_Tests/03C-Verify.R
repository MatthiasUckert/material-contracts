# 03C-Verify: did the render change anything of substance? ---------------------------------------------------------------
#
# READ THIS FIRST IF YOU ARE COMING BACK TO IT COLD. The answer is section 5, in plain English.
#
# WHAT "UNCHANGED" MEANS HERE, AND WHY IT IS NOT "IDENTICAL"
# 03C guards none of its writes -- every write is unconditional -- so runs/ and table/ are rewritten
# on every render and their timestamps always move. That is expected and is not a fault. What must
# not move is the CONTENT.
#
# ONE COLUMN IS EXPECTED TO DIFFER. kw_evaluate() records DurationSec, a wall-clock measurement, into
# the tibble that becomes runs/*/metrics_overall.parquet and the `overall` block of runs/*/config.json
# (03C.R:1060). It is therefore different on every render by construction, whatever the metrics do.
# THIS SCRIPT IGNORES IT DELIBERATELY. A comparison that flagged it would report a difference on every
# single render and teach you to ignore the whole check.
#
# THREE CATEGORIES, AND ONLY THE FIRST IS BYTE-EQUAL
#   mines/ excluding _smoke    never touched -- the engine skips each cell, so these must be identical
#   runs/, table/, _smoke      rewritten every render -- compare content, minus DurationSec
#   the rendered page          differs on date and on the sweep's elapsed-minutes line
#
# It writes nothing.
#
# Run from the project root:  source("1_code/_Tests/03C-Verify.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

ok_ <- function(.flag, .yes = "PASS", .no = "FAIL") if (isTRUE(.flag)) .yes else .no


# 1. Paths, and the state recorded before the render --------------------------------------------------------------------
# Recorded 2026-08-20 from 03C-Preflight. A future run that disagrees is telling you something changed;
# do not "fix" a mismatch by editing these.

here::i_am("1_code/_Commons/_Initialize.R")

dir_live_   <- here::here("2_output", "03C-ClassifyTrainKeyword")
dir_backup_ <- here::here("2_output", "_BackUps", "_2026-08-20_03C-ClassifyTrainKeyword")

exp_grid_  <- 432L   # 360 text cells + 72 docdesc cells
exp_mines_ <- 432L   # every grid cell mined, nothing outside the grid

# WALL CLOCKS LIVE IN THREE PLACES, NOT ONE. The first version of this script knew about the first
# and missed the other two, which is why 105 config.json files reported as differing while every one
# of their sibling parquets was identical.
#
#   runs/*/metrics_overall.parquet   DurationSec          written by kw_evaluate()
#   mines/*/mine.json                duration_sec         written by the miner
#   runs/*/config.json               overall.DurationSec  AND mine.duration_sec, because config.json
#                                                         embeds the whole mine manifest under `mine`
#
# None of them is content. All are stripped before comparison, at any depth.
drop_cols_ <- c("DurationSec")

# PROVENANCE, NOT CONTENT. keyword_train.py:540-541 writes duration_sec, started_at and ended_at into
# every mine.json, and runs/*/config.json embeds the whole manifest under `mine`. None of the three
# describes what was computed; all three describe when. They are excluded from the EQUALITY TEST and
# reported separately, because their difference is itself informative: a mine whose timestamps moved
# was re-run, and a mine that was skipped is not written at all (keyword_train.py:358).
# THE FORGIVEN SET IS A CATEGORY, NOT A LIST OF INSTANCES. Everything here describes WHEN and WHERE
# a computation ran, never WHAT it computed:
#
#   duration_sec / DurationSec   how long it took
#   started_at / ended_at        when, in UTC        (keyword_train.py:541)
#   versions                     python, numpy, pandas, sklearn as resolved in the venv
#   git_commit                   the repository state it ran from
#
# `versions` matters here specifically: contracts-classify declares open bounds (scikit-learn>=1.4,
# numpy>=1.26, pandas>=2.0) and carries no uv.lock, so its venv resolved differently from the one
# contracts-engine used. Mines written before and after F2 therefore record different libraries.
#
# Forgiven for the EQUALITY TEST and REPORTED regardless, because "the sklearn version changed and
# the output did not" is a fact worth seeing rather than one worth hiding. This list was extended
# three times by guessing which field would differ next; naming the category is the fix, and the key
# report below is the backstop for whatever the category still misses.
drop_keys_ <- c("DurationSec", "duration_sec", "started_at", "ended_at",
                "versions", "git_commit")

# run.log is a log, not an artifact: it records what happened, so of course it differs between two
# runs of the same thing. Compared, it would fail forever and teach you to ignore the check.
skip_files_ <- c("run.log")

say_("== 03C-Verify ==   THIS SCRIPT WRITES NOTHING.")
say_("Expected state recorded 2026-08-20. The plain-English answer is section 5.")
say_("")
say_("live   : ", fs::path_rel(dir_live_, here::here()),
     "  [", if (fs::dir_exists(dir_live_)) "ok" else "MISSING", "]")
say_("backup : ", fs::path_rel(dir_backup_, here::here()),
     "  [", if (fs::dir_exists(dir_backup_)) "ok" else "MISSING", "]")
say_("")

stopifnot("live directory missing"   = fs::dir_exists(dir_live_),
          "backup directory missing" = fs::dir_exists(dir_backup_))

# Paths are returned RELATIVE TO THE OUTPUT ROOT, not to the subdirectory. The first version of this
# script returned root-relative paths here and then prepended the subdirectory again in compare_(),
# so every comparison ran against <root>/mines/mines/... -- a path that does not exist. read_parquet()
# fell into its tryCatch and md5sum() returned NA, so 100% of files reported as differing.
#
# THAT IS THE SIGNATURE TO REMEMBER: when everything differs, suspect the comparison, not the data.
# Section 5 now says so out loud rather than leaving it to be rediscovered.
rel_ <- function(.dir, .sub = NULL) {
  d_ <- if (is.null(.sub)) .dir else fs::path(.dir, .sub)
  if (!fs::dir_exists(d_)) return(character(0))
  as.character(fs::path_rel(fs::dir_ls(d_, recurse = TRUE, type = "file"), .dir))
}

# Which keys differ between two parsed JSON objects, flattened to dotted paths.
#
# THIS IS REPORTED WHETHER OR NOT THE STRIP RESCUES THE FILE, and the first version of it was not --
# it recorded keys only when the content matched, which is precisely the case where you do not need
# them. When a JSON file fails the comparison, the key names ARE the diagnosis.
json_diff_keys_ <- function(.a, .b, .prefix = "") {
  ks_ <- union(names(.a), names(.b))
  if (is.null(ks_)) return(character(0))
  out_ <- character(0)
  for (k_ in ks_) {
    va_ <- .a[[k_]]; vb_ <- .b[[k_]]
    path_ <- if (nzchar(.prefix)) paste0(.prefix, ".", k_) else k_
    if (is.list(va_) && is.list(vb_) && !is.null(names(va_))) {
      out_ <- c(out_, json_diff_keys_(va_, vb_, path_))
    } else if (!identical(va_, vb_)) {
      out_ <- c(out_, path_)
    }
  }
  out_
}

# Collected across one compare_() pass. `prov` holds keys whose difference the strip forgave;
# `real` holds keys that survived it and therefore explain a FAIL.
jkeys_ <- new.env(parent = emptyenv())
jkeys_$prov <- character(0)
jkeys_$real <- character(0)

# Content comparison. Parquet by identical() on the object, minus the duration column: arrow does not
# guarantee byte-identical output for identical input, so md5 on a parquet reports false differences.
#
# Returns TRUE, FALSE, or NA where a side is unreadable -- NA is a THIRD state and is reported as
# such, rather than being collapsed into "differs".
same_file_ <- function(.a, .b) {
  if (!fs::file_exists(.a) || !fs::file_exists(.b)) return(NA)
  ext_ <- tolower(fs::path_ext(.a))
  if (ext_ == "parquet") {
    ta_ <- tryCatch(arrow::read_parquet(.a), error = function(e) NULL)
    tb_ <- tryCatch(arrow::read_parquet(.b), error = function(e) NULL)
    if (is.null(ta_) || is.null(tb_)) return(NA)
    ta_ <- ta_ |> dplyr::select(-dplyr::any_of(drop_cols_))
    tb_ <- tb_ |> dplyr::select(-dplyr::any_of(drop_cols_))
    return(identical(ta_, tb_))
  }
  if (ext_ == "json") {
    ja_ <- tryCatch(jsonlite::read_json(.a), error = function(e) NULL)
    jb_ <- tryCatch(jsonlite::read_json(.b), error = function(e) NULL)
    if (is.null(ja_) || is.null(jb_)) return(NA)
    # Recursive: config.json nests the mine manifest under `mine`, so the duration sits one level
    # down and a top-level strip misses it.
    strip_ <- function(.x) {
      if (!is.list(.x)) return(.x)
      nm_ <- names(.x)
      if (!is.null(nm_)) .x <- .x[!nm_ %in% drop_keys_]
      lapply(.x, strip_)
    }
    if (identical(ja_, jb_)) return(TRUE)

    keys_ <- json_diff_keys_(ja_, jb_)
    same_ <- identical(strip_(ja_), strip_(jb_))
    if (same_) {
      jkeys_$prov <- unique(c(jkeys_$prov, keys_))
    } else {
      jkeys_$real <- unique(c(jkeys_$real, setdiff(keys_, drop_keys_)))
    }
    return(same_)
  }
  a_ <- unname(tools::md5sum(.a)); b_ <- unname(tools::md5sum(.b))
  if (is.na(a_) || is.na(b_)) return(NA)
  a_ == b_
}

compare_ <- function(.sub, .exclude_smoke = FALSE) {
  jkeys_$prov <- character(0)
  jkeys_$real <- character(0)
  fl_ <- rel_(dir_live_, .sub)
  fb_ <- rel_(dir_backup_, .sub)
  if (.exclude_smoke) {
    fl_ <- fl_[!grepl("_smoke", fl_, fixed = TRUE)]
    fb_ <- fb_[!grepl("_smoke", fb_, fixed = TRUE)]
  }
  both_ <- intersect(fl_, fb_)
  both_ <- both_[!fs::path_file(both_) %in% skip_files_]

  # rel_() already carries the subdirectory. Joining it again is the bug this comment exists for.
  res_ <- purrr::map_lgl(both_, \(.f) {
    v_ <- same_file_(fs::path(dir_live_, .f), fs::path(dir_backup_, .f))
    if (is.na(v_)) NA else v_
  })

  list(
    live = fl_, backup = fb_, both = both_,
    lost = setdiff(fb_, fl_), gained = setdiff(fl_, fb_),
    diff = both_[!is.na(res_) & !res_],
    unreadable = both_[is.na(res_)],
    prov_keys = jkeys_$prov, real_keys = jkeys_$real
  )
}

# Always report the JSON keys, in both directions. Which fields moved is the diagnosis; the count is
# not.
say_keys_ <- function(.res, .indent = "  ") {
  if (length(.res$prov_keys) > 0L) {
    say_(.indent, "forgiven (timing/provenance) : ", paste(.res$prov_keys, collapse = ", "))
    say_(.indent, "  -> a skipped mine is not written at all, so a moved timestamp means it ran.")
  }
  if (length(.res$real_keys) > 0L) {
    say_(.indent, "KEYS THAT EXPLAIN THE FAIL   : ", paste(.res$real_keys, collapse = ", "))
  }
}

# Group differing files by extension. WHICH files differ is more informative than how many: a
# difference confined to .json and .log is a wall-clock artefact, while one that reaches .parquet is
# a change in the data. Printing the breakdown means the reader does not have to infer that.
by_ext_ <- function(.files) {
  if (length(.files) == 0L) return(invisible(NULL))
  tibble::tibble(Ext = fs::path_ext(.files), Name = fs::path_file(.files)) |>
    dplyr::count(.data$Ext, .data$Name, name = "Files") |>
    dplyr::arrange(dplyr::desc(.data$Files)) |>
    (\(.t) say_(paste0("      ", format(.t$Name, width = 26), fmt_(.t$Files), collapse = "\n")))()
}

# A whole-population difference is far more likely to be a broken comparison than a changed pipeline.
suspect_ <- function(.res) {
  n_ <- length(.res$both)
  n_ > 20L && (length(.res$diff) + length(.res$unreadable)) == n_
}


# 2. mines/ -- the one thing that must be byte-equal --------------------------------------------------------------------
# The engine skips every cell that already exists, so these files should not have been opened. A
# difference here means something re-mined, and that is the finding.

say_("== 2. mines/ (excluding _smoke) ==")
m_ <- compare_("mines", .exclude_smoke = TRUE)
say_("files live / backup : ", fmt_(length(m_$live)), " / ", fmt_(length(m_$backup)))
say_("lost                : ", fmt_(length(m_$lost)), "   ", ok_(length(m_$lost) == 0L))
say_("gained              : ", fmt_(length(m_$gained)))
say_("unreadable          : ", fmt_(length(m_$unreadable)))
say_("content differing   : ", fmt_(length(m_$diff)), "   ", ok_(length(m_$diff) == 0L))
if (length(m_$diff) > 0L) { say_("  differing files, by name:"); by_ext_(m_$diff) }
say_keys_(m_)
if (length(m_$lost) > 0L) say_(paste0("    lost: ", utils::head(m_$lost, 10), collapse = "\n"))
if (suspect_(m_)) {
  say_("")
  say_("  EVERY comparable file differs or is unreadable. That is almost never a pipeline change --")
  say_("  a real one touches some files, not all of them. Suspect this script before the data:")
  say_("  check that the paths it builds actually exist.")
} else if (length(m_$diff) > 0L || length(m_$lost) > 0L) {
  say_("  -> CONTENT changed. Find out what before reading the results.")
} else if (any(c("started_at", "ended_at", "duration_sec") %in% m_$prov_keys)) {
  # THREE-WAY, NOT TWO. An earlier version concluded "nothing was re-mined" from a zero diff count
  # alone, while the line directly above it reported moved timestamps -- the two contradicted each
  # other and the conclusion was the wrong one. Identical content does not mean nothing ran.
  say_("  -> EVERY MINE RAN, and produced identical content. The skip path did NOT hold: a skipped")
  say_("     mine is never written (keyword_train.py:357), so a moved timestamp is proof it executed.")
  say_("     Nothing is wrong with the results; the mining is deterministic. What is unexplained is")
  say_("     why the done marker did not stop it. See C3 -- kw_mine_sweep() discards the engine's")
  say_("     stdout, so the render cannot be asked.")
} else {
  say_("  -> nothing ran and nothing changed. The skip path held.")
}
say_("")


# 3. runs/ and table/ -- rewritten, so content is the test --------------------------------------------------------------
# These ARE rewritten every render because nothing in 03C guards a write. Timestamps moving is
# expected. DurationSec differing is expected and excluded. Anything else differing is not.

say_("== 3. runs/ and table/ (rewritten; content compared, DurationSec ignored) ==")
run_ok_ <- TRUE
for (sub_ in c("runs", "table")) {
  c_ <- compare_(sub_)
  say_("  ", format(sub_, width = 8),
       "live/backup ", format(paste0(length(c_$live), "/", length(c_$backup)), width = 12),
       "lost ", format(length(c_$lost), width = 5),
       "unreadable ", format(length(c_$unreadable), width = 5),
       "differing ", format(length(c_$diff), width = 5),
       ok_(length(c_$diff) == 0L && length(c_$lost) == 0L))
  if (length(c_$diff) > 0L) by_ext_(c_$diff)
  say_keys_(c_, .indent = "      ")
  if (length(c_$lost) > 0L) say_(paste0("      lost: ", utils::head(c_$lost, 8), collapse = "\n"))
  if (suspect_(c_)) say_("      EVERY file differs -- suspect the comparison, not the data.")
  run_ok_ <- run_ok_ && length(c_$diff) == 0L && length(c_$lost) == 0L
}
say_("")


# 4. The grid, against what was recorded --------------------------------------------------------------------------------

say_("== 4. Against the recorded shape ==")
idx_ok_ <- NA
n_mine_ <- tryCatch({
  src_ <- here::here("1_code", "03C-ClassifyTrainKeyword.R")
  if (!exists("kw_mine_index")) source(src_, encoding = "UTF-8")
  nrow(kw_mine_index(.mines_root = fs::path(dir_live_, "mines")))
}, error = function(e) NA_integer_)

say_("mines indexed : ", if (is.na(n_mine_)) "could not read" else fmt_(n_mine_),
     "   expected ", fmt_(exp_mines_), "   ", ok_(isTRUE(n_mine_ == exp_mines_)))
idx_ok_ <- isTRUE(n_mine_ == exp_mines_)
say_("")


# 5. THE ANSWER ---------------------------------------------------------------------------------------------------------

clean_ <- length(m_$diff) == 0L && length(m_$lost) == 0L && isTRUE(run_ok_) && isTRUE(idx_ok_)

say_("== 5. THE ANSWER ==")
say_("")
if (clean_) {
  say_("  CLEAN.")
  say_("")
  say_("  Every artifact carries the same content it carried before -- same metrics, same tables,")
  say_("  same lexicons. 03E and 03F read what they read before.")
  if (any(c("started_at", "ended_at", "duration_sec") %in% m_$prov_keys)) {
    say_("")
    say_("  Note that CLEAN does not mean nothing ran. The mines' timestamps moved, so they were")
    say_("  re-mined and produced byte-identical output. That is a statement about determinism, not")
    say_("  about idempotency -- the skip path did not hold, and why is still open (C3).")
  }
  say_("")
  say_("  Timestamps under runs/ and table/ WILL have moved. 03C guards none of its writes, so every")
  say_("  render rewrites them. That is expected and is not a fault. DurationSec will also differ; it")
  say_("  is a wall-clock measurement written into the artifact and is ignored here on purpose.")
} else {
  say_("  NOT CLEAN.")
  say_("")
  if (length(m_$diff) > 0L || length(m_$lost) > 0L)
    say_("    - section 2: mines/ changed. Something re-mined, which the skip path should prevent.")
  if (!isTRUE(run_ok_))
    say_("    - section 3: a rewritten artifact changed in more than DurationSec.")
  if (!isTRUE(idx_ok_))
    say_("    - section 4: the mine count no longer matches what was recorded.")
  say_("")
  if (suspect_(m_)) {
    say_("    - NOTE: every comparable file differs. Before investigating the pipeline, check that")
    say_("      the paths this script builds resolve. A whole-population difference is a bug here")
    say_("      far more often than a change there.")
    say_("")
  }
  say_("  Re-mining is cheap -- minutes -- so this is not an emergency. But a mine carries no")
  say_("  fingerprint of the data it consumed, so a change here means either the sample moved or a")
  say_("  hyperparameter did, and both are worth naming before trusting the results above them.")
}
say_("")
say_("Reminder: this script wrote nothing.")
