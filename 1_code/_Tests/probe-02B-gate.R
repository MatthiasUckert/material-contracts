# ======================================================================================================================
# PROBE -- 02B's register functions, on a fixture
# ======================================================================================================================
#
# WHAT THIS IS. A gate, not a pair. Source it and it runs its own checks and prints them. It needs
# the library and nothing else -- no corpus, no register, no Compustat -- so a signature or a
# column-name mistake costs seconds rather than a register rebuild. Run it before rendering 02B.
#
# It writes one small parquet to a tempfile(). R removes the session temp directory on exit, so there
# is no cleanup code -- which matters, because the cleanup code is what broke the first version of
# this file: on.exit() at top level registers on the frame of the top-level expression it sits in,
# and that frame unwinds on the next line. The directory was deleted one statement after it was
# created. on.exit() belongs inside a function body and nowhere else.

.p_lib <- if (requireNamespace("here", quietly = TRUE)) {
  here::here("1_code", "02B-Register.R")
} else {
  "1_code/02B-Register.R"
}
if (!file.exists(.p_lib)) .p_lib <- "02B-Register.R"   # if the working directory is already 1_code
if (!file.exists(.p_lib)) stop("Cannot find 02B-Register.R; set .p_lib by hand.")

suppressWarnings(source(.p_lib, encoding = "UTF-8"))
cat("library:", .p_lib, "\n\n")
ok <- function(.n, .p, .d = "") { cat(if (isTRUE(.p)) "PASS " else "FAIL ", .n,
  if (any(nzchar(.d))) paste0(" -- ", paste(.d, collapse = " ")) else "", "\n", sep = ""); isTRUE(.p) }
res <- logical(0)

# THE FIXTURE PARQUET IS A TEMPFILE. Writing it beside the code would leave a stray artifact in a
# repository whose whole point is that outputs rebuild from inputs, and managing a directory by hand
# is what went wrong here once already.
.path_fix <- tempfile(pattern = "probe-02b-", fileext = ".parquet")

arrow_ok <- requireNamespace("arrow", quietly = TRUE)

# -- reg_shape ----------------------------------------------------------------------------------
tab <- tibble::tibble(
  DocID = c("a", "b"), HashDocument = c("h1", "h2"), HashIndex = c("i1", "i2"),
  CIK = 1:2, Group = c("Exhibit10", "8-K"), DocTypeMod = c("Exhibit10", "8-K"),
  DocPath = c("/x", "/y"), Path = c("/x", "/y"), nQuarters = c(1L, 1L),
  Removed = c(0L, 0L), DescSample = c(1L, 0L), EstiSample = c(1L, 0L),
  SampleStepCode = c("06", "06"), SampleStepDesc = c("f", "f"),
  Item101Outcome = c(NA_character_, "extracted"), HasSummary = c(0L, 1L)
)
shaped <- reg_shape(.tab = tab)

res <- c(res, ok("reg_shape drops the path columns",
                 !any(c("DocPath", "Path", "nQuarters") %in% names(shaped)), ""))
res <- c(res, ok("reg_shape no longer carries Items or any Cto column",
                 !any(c("Items", "HasCto", "nCtoOrders", "CtoReleaseFirst",
                        "CtoReleaseLast", "CtoIsExtension") %in% names(shaped)), ""))
res <- c(res, ok("reg_shape keeps the extraction block",
                 all(c("HasSummary", "Item101Outcome") %in% names(shaped)), ""))
res <- c(res, ok("HasItem101 is gone from the register",
                 !"HasItem101" %in% names(shaped), ""))

# -- reg_group_summary --------------------------------------------------------------------------
grp <- reg_group_summary(.tab = shaped)
res <- c(res, ok("group summary reports nSummary, not nItem101 or nCto",
                 "nSummary" %in% names(grp) && !any(c("nItem101", "nCto") %in% names(grp)),
                 paste(names(grp), collapse = " ")))
res <- c(res, ok("nSummary counts the recovered summaries", sum(grp$nSummary) == 1L, sum(grp$nSummary)))

# -- reg_attach_item101, against a real parquet -------------------------------------------------
if (arrow_ok) {
  arrow::write_parquet(tibble::tibble(
    DocID    = c("b", "c", "d"),
    Outcome  = c("extracted", "heading-only", "ambiguous-longest"),
    ItemCodes = c("1.01|9.01", "1.01", "1.01|8.01")
  ), .path_fix)

  # The fixture is checked before it is read. When the first version of this probe deleted its own
  # working directory, the symptom was an arrow IOError two calls later, which says nothing about
  # the cause. A one-line assertion turns that into a sentence.
  res <- c(res, ok("the fixture parquet survives to be read", file.exists(.path_fix), .path_fix))

  base <- tibble::tibble(DocID = c("a", "b", "c", "d"))
  att  <- reg_attach_item101(.tab = base, .path_item = .path_fix)

  res <- c(res, ok("ItemCodes is not pulled into the register",
                   !any(c("Items", "ItemCodes") %in% names(att)), paste(names(att), collapse = " ")))
  res <- c(res, ok("a non-candidate gets zero, not missing",
                   att$HasSummary[att$DocID == "a"] == 0L && is.na(att$Item101Outcome[att$DocID == "a"]), ""))
  res <- c(res, ok("extracted and ambiguous-longest both count",
                   identical(att$HasSummary, c(0L, 1L, 0L, 1L)), paste(att$HasSummary, collapse = " ")))
  res <- c(res, ok("heading-only is a candidate but not a recovery",
                   !is.na(att$Item101Outcome[att$DocID == "c"]) && att$HasSummary[att$DocID == "c"] == 0L, ""))
} else {
  cat("SKIP arrow unavailable\n")
}

cat("\n", sum(res), "/", length(res), " passed\n", sep = "")
if (!all(res)) quit(status = 1L)
