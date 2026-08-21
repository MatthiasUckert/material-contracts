# 03A-Verify: is 03A's output still the one the models were trained on? --------------------------------------------------
#
# READ THIS FIRST IF YOU ARE COMING BACK TO IT COLD.
#
# This script answers ONE question: does prepared.parquet still contain what it contained when the
# BERT sweep was fitted against it? Everything else it prints is context for that answer.
#
# THE ANSWER YOU WANT IS SECTION 8. It is written in plain English. If it says CLEAN, stop reading.
#
# WHAT COUNTS AS A PROBLEM, AND WHAT DOES NOT
#
#   A CHANGE IN CONTENT IS A PROBLEM. Six documents read this file and the BERT engine skips a run
#   whose metrics already exist, so a changed sample would NOT trigger a retrain -- it would silently
#   serve models fitted on the previous one. Section 2 is therefore the real test.
#
#   A CHANGE IN TIMESTAMP, ON ITS OWN, IS NOT A PROBLEM. Nothing downstream keys on this file's
#   modification time. The timestamp moved once, on 2026-08-20, when the guarded write replaced the
#   unguarded one; from then on an unchanged render leaves it alone. Section 4 reports it as
#   information, not as a verdict. THE FIRST VERSION OF THIS SCRIPT CALLED THAT A FAILURE AND IT WAS
#   WRONG TO -- that is why this note exists.
#
#   A MISSING Cache/ DIRECTORY IS NOT A PROBLEM. It was deleted deliberately. Section 5 explains.
#
# It writes nothing.
#
# Run from the project root:  source("1_code/_Tests/03A-Verify.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

ok_ <- function(.flag, .yes = "PASS", .no = "FAIL") if (isTRUE(.flag)) .yes else .no


# 1. Paths, and the state this was verified in --------------------------------------------------------------------------
#
# THE EXPECTED VALUES BELOW ARE RECORDED, NOT DERIVED. They are what the pipeline looked like on
# 2026-08-20, the day 03A was rewritten and the rewrite was shown to be content-neutral. A future run
# that disagrees with them is telling you something changed since; the disagreement IS the finding.
# Do not "fix" a mismatch by editing these numbers.

here::i_am("1_code/_Commons/_Initialize.R")

dir_live_   <- here::here("2_output", "03A-ClassifyPrepare")
dir_backup_ <- here::here("2_output", "_BackUps", "2026_08_09_03A-ClassifyPrepare")

exp_rows_  <- 4398L                                  # documents surviving intake
exp_folds_ <- c(885L, 882L, 881L, 876L, 874L)        # fold sizes, in fold order
exp_cols_  <- c("DocID", "Text", "DocDesc", "DocName", "ClassBroad", "ClassDetailed",
                "ClassDetailed2", "AmendType", "LabelRound", "Fold")

# Deliberately deleted on 2026-08-20. It was the tree-walk's DocID-to-Path index, it is read by
# nothing since 03A became register-gated, and it held paths into the PREVIOUS repository
# (../pMatDisc). Its ABSENCE is correct. Its RETURN would be the thing worth investigating.
exp_removed_ <- "Cache/ContractFiles.parquet"

# Newest file in each downstream tree as of 2026-08-20, all predating the 03A rewrite. Nothing 03A
# does can touch these; the check exists so that claim is measured rather than trusted.
exp_downstream_ <- c(
  "03B-ClassifyTrainBERT"    = "2026-08-13",
  "03C-ClassifyTrainKeyword" = "2026-08-11",
  "03D-ClassifyLLM"          = "2026-08-11",
  "03E-ClassifyOrchestrate"  = NA_character_    # empty on 2026-08-20
)

say_("== 03A-Verify ==   THIS SCRIPT WRITES NOTHING.")
say_("Expected state recorded 2026-08-20. The plain-English answer is section 8.")
say_("")
say_("live   : ", fs::path_rel(dir_live_, here::here()),
     "  [", if (fs::dir_exists(dir_live_)) "ok" else "MISSING", "]")
say_("backup : ", fs::path_rel(dir_backup_, here::here()),
     "  [", if (fs::dir_exists(dir_backup_)) "ok" else "MISSING", "]")
say_("")

stopifnot("live directory missing"   = fs::dir_exists(dir_live_),
          "backup directory missing" = fs::dir_exists(dir_backup_))

path_live_   <- fs::path(dir_live_, "prepared.parquet")
path_backup_ <- fs::path(dir_backup_, "prepared.parquet")

stopifnot("prepared.parquet missing (live)"   = fs::file_exists(path_live_),
          "prepared.parquet missing (backup)" = fs::file_exists(path_backup_))


# 2. THE REAL TEST: is the content the same? ----------------------------------------------------------------------------
# Compared as OBJECTS, not as bytes. Arrow does not guarantee byte-identical Parquet output across
# versions for identical input, so an md5 of the file would report changes that are not changes.
# identical() compares values, types and attributes, and is the strictest test R has.

tab_live_   <- arrow::read_parquet(path_live_)
tab_backup_ <- arrow::read_parquet(path_backup_)

content_same_ <- identical(tab_live_, tab_backup_)

say_("== 2. Content (THIS IS THE TEST THAT MATTERS) ==")
say_("identical(live, backup) : ", ok_(content_same_))

if (!content_same_) {
  say_("")
  say_("  rows live/backup : ", fmt_(nrow(tab_live_)), " / ", fmt_(nrow(tab_backup_)))
  say_("  cols live/backup : ", ncol(tab_live_), " / ", ncol(tab_backup_))
  common_ <- intersect(names(tab_live_), names(tab_backup_))
  if (nrow(tab_live_) == nrow(tab_backup_)) {
    for (c_ in common_) {
      a_ <- tab_live_[[c_]]; b_ <- tab_backup_[[c_]]
      d_ <- sum(!((is.na(a_) & is.na(b_)) | (!is.na(a_) & !is.na(b_) & a_ == b_)))
      say_("    ", format(c_, width = 16), "differing: ", fmt_(d_), if (d_ == 0L) "" else "   <-- HERE")
    }
  }
}
say_("")


# 3. Does it still match the recorded shape? ----------------------------------------------------------------------------
# Section 2 compares live against the backup. This compares live against the numbers written down on
# 2026-08-20, so the check survives the backup being moved, pruned or lost.

rows_ok_  <- nrow(tab_live_) == exp_rows_
cols_ok_  <- identical(names(tab_live_), exp_cols_)
folds_    <- as.integer(table(tab_live_$Fold))
folds_ok_ <- identical(folds_, exp_folds_)

say_("== 3. Against the recorded shape ==")
say_("rows        : ", fmt_(nrow(tab_live_)), " (expected ", fmt_(exp_rows_), ")   ",
     ok_(rows_ok_))
say_("columns     : ", ncol(tab_live_), " (expected ", length(exp_cols_), ")   ",
     ok_(cols_ok_))
say_("fold sizes  : ", paste(folds_, collapse = " / "))
say_("   expected : ", paste(exp_folds_, collapse = " / "), "   ", ok_(folds_ok_))
if (!cols_ok_) {
  say_("  unexpected : ", paste(setdiff(names(tab_live_), exp_cols_), collapse = ", "))
  say_("  missing    : ", paste(setdiff(exp_cols_, names(tab_live_)), collapse = ", "))
}
say_("")


# 4. Timestamps -- INFORMATION, NOT A VERDICT ---------------------------------------------------------------------------
# Read this only if section 2 failed. Nothing downstream keys on this file's modification time: the
# BERT engine's idempotency is its own, keyed on whether a run's metrics already exist.
#
# The live timestamp is EXPECTED to be later than the backup's. The backup predates the rewrite, and
# the transition from the unguarded write to the guarded one necessarily wrote the file once.

m_live_   <- fs::file_info(path_live_)$modification_time
m_backup_ <- fs::file_info(path_backup_)$modification_time

say_("== 4. Timestamps (information only) ==")
say_("live   : ", format(m_live_), "   size ", fmt_(fs::file_size(path_live_)))
say_("backup : ", format(m_backup_), "   size ", fmt_(fs::file_size(path_backup_)))
say_("")
say_("  A later live timestamp is expected and is not a fault. What WOULD be worth a look is the")
say_("  timestamp moving again on a render where nothing changed -- that would mean the guard in")
say_("  clf_write_prepared() has stopped working. Section 7 checks what the last render actually did.")
say_("")


# 5. Directory inventory ------------------------------------------------------------------------------------------------
# Removals are classified against exp_removed_, so a deliberate deletion reads as expected rather than
# as loss.

rel_ <- function(.dir) {
  if (!fs::dir_exists(.dir)) return(character(0))
  as.character(fs::path_rel(fs::dir_ls(.dir, recurse = TRUE, type = "file"), .dir))
}

f_live_   <- rel_(dir_live_)
f_backup_ <- rel_(dir_backup_)

gone_       <- setdiff(f_backup_, f_live_)
gone_exp_   <- intersect(gone_, exp_removed_)
gone_unexp_ <- setdiff(gone_, exp_removed_)
gained_     <- setdiff(f_live_, f_backup_)
returned_   <- intersect(f_live_, exp_removed_)

say_("== 5. Directory inventory ==")
say_("files live / backup    : ", fmt_(length(f_live_)), " / ", fmt_(length(f_backup_)))
say_("removed, as expected   : ", fmt_(length(gone_exp_)),
     if (length(gone_exp_) > 0L) paste0("   (", paste(gone_exp_, collapse = ", "), ")") else "")
say_("removed, NOT expected  : ", fmt_(length(gone_unexp_)), "   ", ok_(length(gone_unexp_) == 0L))
if (length(gone_unexp_) > 0L) say_(paste0("    ", gone_unexp_, collapse = "\n"))
say_("gained                 : ", fmt_(length(gained_)))
if (length(gained_) > 0L) say_(paste0("    ", gained_, collapse = "\n"))
if (length(returned_) > 0L) {
  say_("")
  say_("  NOTE: a file that was deliberately deleted has come back: ",
       paste(returned_, collapse = ", "))
  say_("  Cache/ContractFiles.parquet holds paths into ../pMatDisc and is read by nothing. If it is")
  say_("  here again, something regenerated it -- find out what before trusting a render.")
}
say_("")


# 6. Downstream trees ---------------------------------------------------------------------------------------------------
# 03A's live code reads 0_data/Labels, 02B's Output and 01B's mirror, and writes only its own
# directory. This measures that rather than trusting it, against the dates recorded on 2026-08-20.

say_("== 6. Downstream trees ==")

down_ok_ <- TRUE
for (nm_ in names(exp_downstream_)) {
  d_ <- here::here("2_output", nm_)
  if (!fs::dir_exists(d_)) { say_("  ", format(nm_, width = 26), "(absent)"); next }
  inf_ <- fs::dir_info(d_, recurse = TRUE, type = "file")
  if (nrow(inf_) == 0L) { say_("  ", format(nm_, width = 26), "(empty)"); next }

  i_       <- which.max(inf_$modification_time)
  newest_  <- inf_$modification_time[[i_]]
  exp_     <- exp_downstream_[[nm_]]
  drifted_ <- !is.na(exp_) && as.Date(newest_) > as.Date(exp_)
  if (drifted_) down_ok_ <- FALSE

  say_("  ", format(nm_, width = 26), format(newest_),
       "  expected <= ", format(dplyr::coalesce(exp_, "(empty)"), width = 12),
       ok_(!drifted_, "ok", "CHANGED SINCE"))
}
say_("")
say_("  A later date here is not automatically wrong -- you may have re-run 03B or 03C on purpose.")
say_("  It is wrong only if you did not.")
say_("")


# 7. What the last render actually did ----------------------------------------------------------------------------------
# THE CONCLUSIVE EVIDENCE. clf_write_prepared() says out loud whether it wrote. Reading it out of the
# rendered page is better than inferring it from a timestamp, because it is the document's own
# testimony rather than a reconstruction.

say_("== 7. The last render's own testimony ==")

html_ <- fs::dir_ls(here::here(), recurse = TRUE, type = "file",
                    glob = "*03A-ClassifyPrepare.html", fail = FALSE)
html_ <- html_[!grepl("_BackUp|_BackUps|_superseded", html_)]

guard_ok_ <- NA
if (length(html_) == 0L) {
  say_("  No rendered 03A-ClassifyPrepare.html found -- skipped.")
} else {
  h_   <- html_[[which.max(fs::file_info(html_)$modification_time)]]
  txt_ <- paste(readLines(h_, warn = FALSE), collapse = "\n")
  n_skip_  <- lengths(regmatches(txt_, gregexpr("Unchanged, not rewritten", txt_)))
  n_wrote_ <- lengths(regmatches(txt_, gregexpr("Wrote &#39;2_output|Wrote '2_output", txt_)))

  say_("  page    : ", fs::path_rel(h_, here::here()))
  say_("  rendered: ", format(fs::file_info(h_)$modification_time))
  say_("  says 'Unchanged, not rewritten' : ", n_skip_)
  say_("  says 'Wrote'                    : ", n_wrote_)

  guard_ok_ <- n_skip_ > 0L && n_wrote_ == 0L
  say_("  -> ", if (isTRUE(guard_ok_))
         "the guard fired; that render did not touch the file" else
         "that render WROTE the file -- expected only on the first render after a real change")
}
say_("")


# 8. THE ANSWER ---------------------------------------------------------------------------------------------------------

shape_ok_ <- rows_ok_ && cols_ok_ && folds_ok_
clean_    <- content_same_ && shape_ok_ && length(gone_unexp_) == 0L

say_("== 8. THE ANSWER ==")
say_("")
if (clean_) {
  say_("  CLEAN.")
  say_("")
  say_("  prepared.parquet holds exactly what it held when the BERT sweep was fitted: same ",
       fmt_(exp_rows_), " rows,")
  say_("  same ten columns, same fold sizes, byte-equivalent content. 03B through 03F and 04A read")
  say_("  the same sample they have always read, and no model needs refitting.")
  say_("")
  if (isFALSE(identical(as.numeric(m_live_), as.numeric(m_backup_)))) {
    say_("  The file's TIMESTAMP differs from the backup's. That is expected and is not a fault: the")
    say_("  backup predates the rewrite, and switching from the unguarded write to the guarded one")
    say_("  wrote the file once. Nothing downstream reads the timestamp.")
    say_("")
  }
  if (!isTRUE(down_ok_)) {
    say_("  One or more downstream trees are newer than recorded. If you re-ran them on purpose,")
    say_("  update the dates in section 1. If you did not, find out what did.")
    say_("")
  }
} else {
  say_("  NOT CLEAN. Something to look at:")
  say_("")
  if (!content_same_)              say_("    - the CONTENT differs from the backup. Section 2 names the columns.")
  if (!shape_ok_)                  say_("    - the shape differs from what was recorded on 2026-08-20. Section 3.")
  if (length(gone_unexp_) > 0L)    say_("    - files are missing that were not meant to be. Section 5.")
  say_("")
  say_("  Before re-running anything downstream: the BERT engine skips a run whose metrics already")
  say_("  exist, so a changed sample will NOT cause a retrain. It will quietly serve models fitted on")
  say_("  the old one. Settle this first.")
  say_("")
}
say_("Reminder: this script wrote nothing.")
