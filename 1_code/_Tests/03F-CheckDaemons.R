# 03F-CheckDaemons: does the parallel read actually work? ------------------------------------------------------------------
#
# WHAT THIS IS
# A thirty-second check of the daemon setup, runnable in a SECOND R session while a pass is running in
# the first. It reads ten documents in parallel and compares them against the same ten read
# sequentially. It writes nothing and does not touch the store.
#
# WHY IT EXISTS
# The first attempt at a parallel read sent clf_read_text() to the daemons as a serialized closure and
# assumed it would carry. It did not. mirai does not throw when a task fails -- it RETURNS the error
# as a miraiError, a character scalar with a class -- so 24 failed batches unlisted to a character
# vector of 24 and the symptom read as "returned 24 texts for 5000 documents". The length check caught
# it and the pass fell back correctly, but finding out WHY meant reading the messages that had been
# thrown away.
#
# This asks the question directly, in seconds, rather than at the top of a pass.
#
# Run from the project root:  source("1_code/_Tests/03F-CheckDaemons.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

here::i_am("1_code/_Commons/_Initialize.R")
source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),     encoding = "UTF-8")
source(here::here("1_code", "03A-ClassifyPrepare.R"),     encoding = "UTF-8")

n_workers_ <- 4L    # four is enough to prove the mechanism; the pass uses more

say_("== 03F-CheckDaemons ==   WRITES NOTHING.")
say_("")


# 1. Ten real paths, from the register ----------------------------------------------------------------------------------

reg_ <- arrow::open_dataset(
  sources = here::here("2_output", "02B-Register", "Output", "Documents.parquet")
) |>
  dplyr::select("DocID", "DocTypeMod", "YQ") |>
  dplyr::filter(.data$DocTypeMod == "Exhibit10") |>
  head(10L) |>
  dplyr::collect()

paths_ <- as.character(utils_doc_path(
  .dir_mirror = fs::path(here::here("2_output", "01B-EdgarDocuments"), "GetEDGAR"),
  .doc_type   = reg_$DocTypeMod,
  .yq         = reg_$YQ,
  .doc_id     = reg_$DocID
))

say_("paths built  : ", length(paths_), "   all exist: ", all(fs::file_exists(paths_)))
say_("")


# 2. Sequential, as the fallback does it --------------------------------------------------------------------------------

seq_ <- vapply(paths_, clf_read_text, character(1), USE.NAMES = FALSE)
say_("== 2. Sequential ==")
say_("texts read   : ", length(seq_))
say_("non-empty    : ", sum(!is.na(seq_) & nzchar(seq_)))
say_("total chars  : ", format(sum(nchar(seq_), na.rm = TRUE), big.mark = ","))
say_("")


# 3. Parallel, exactly as app_pass sets it up ---------------------------------------------------------------------------
# The daemons are fresh R sessions. The reader is SOURCED into them rather than sent as a closure, and
# the whole chain travels because 03A's library calls plot_register_levels() at load time.

say_("== 3. Parallel ==")

# NO on.exit() HERE, DELIBERATELY. Inside a function it fires when the function returns; at TOP LEVEL
# in a sourced file the frame it attaches to belongs to source() itself, and the teardown can fire
# before the next expression runs -- which is how an earlier version of this script tore its own
# daemons down and then reported "No daemons set". app_pass() is unaffected: its on.exit is inside a
# function body, which is the case the mechanism is for. Here the teardown is explicit, at the end.
mirai::daemons(n_workers_)

# ASKED, NOT ASSUMED. daemons() returning without error does not mean daemons connected, and the
# difference is the whole question this script exists to settle.
st_ <- tryCatch(mirai::status(), error = function(e) NULL)
say_("daemons requested : ", n_workers_)
say_("daemons connected : ",
     if (is.null(st_)) "could not read status()" else paste(st_$connections, collapse = ", "))
if (!is.null(st_$daemons)) {
  say_("daemon table      : ", NROW(st_$daemons), " row(s)")
}
say_("")

setup_ok_ <- tryCatch({
  mirai::everywhere(
    {
      for (.f in c(file.path("_Commons", "_Initialize.R"), file.path("_Commons", "_Utils.R"),
                   file.path("_Commons", "_Plots.R"),      file.path("_Commons", "_Tables.R"),
                   "03A-ClassifyPrepare.R")) {
        source(file.path(.code, .f), encoding = "UTF-8")
      }
    },
    .code = here::here("1_code")
  )
  TRUE
}, error = function(e) {
  say_("  everywhere() failed: ", conditionMessage(e))
  FALSE
})
say_("daemon setup : ", if (setup_ok_) "ok" else "FAILED")

if (setup_ok_) {
  bats_ <- split(paths_, ceiling(seq_along(paths_) / ceiling(length(paths_) / n_workers_)))
  out_  <- mirai::mirai_map(
    .x = bats_,
    .f = function(paths) vapply(paths, clf_read_text, character(1), USE.NAMES = FALSE)
  )[]

  err_ <- which(vapply(out_, \(.r) inherits(.r, "miraiError"), logical(1)))
  say_("batches      : ", length(out_), "   failed: ", length(err_))

  if (length(err_) > 0L) {
    say_("")
    say_("  THE ACTUAL ERROR, which is what the pass was throwing away:")
    say_("    ", as.character(out_[[err_[[1]]]]))
    say_("")
    say_("  Common causes: the daemon cannot see the renv library (arrow fails to load), or a")
    say_("  sourced file is not where here::here() puts it from inside a daemon.")
  } else {
    par_ <- unlist(out_, use.names = FALSE)
    say_("texts read   : ", length(par_))
    say_("identical to sequential : ", identical(par_, seq_))
    if (!identical(par_, seq_)) {
      say_("  MISMATCH. Lengths ", length(par_), " vs ", length(seq_),
           "; differing elements: ", sum(par_ != seq_, na.rm = TRUE))
    }
  }
}
say_("")


# 4. The answer ---------------------------------------------------------------------------------------------------------

mirai::daemons(0L)   # explicit teardown, since on.exit is not safe at top level here

say_("== 4. THE ANSWER ==")
say_("")
say_("  Parallel and sequential must return IDENTICAL text in IDENTICAL order. Text is paired onto")
say_("  the document table positionally, so an order that survives is not a nicety -- a reordered")
say_("  read would label documents with other documents' text and say nothing about it.")
say_("")
say_("Reminder: this script wrote nothing.")
