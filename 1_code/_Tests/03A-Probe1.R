# 03A-Probe: can the register replace the tree walk, and what does it say about the labelled sample? -------------------
#
# WHAT THIS IS
# A read-only check run BEFORE 03A is rewritten. It writes nothing and depends on nothing left in
# the session. Self-contained: it resolves its own paths.
#
# WHY IT EXISTS
# The register-as-gate design for 03A rests on claims that have never been tested together:
#
#   EVERY LABELLED DocID IS IN THE REGISTER. If it is not, the training sample under the new design
#   is smaller than under the old one, and the difference is a set of documents the pipeline does not
#   know about. A finding either way, but it has to be measured before it is designed around.
#
#   THE REGISTER CARRIES WHAT A PATH NEEDS. 02B stores no path: its prose says DocType, YQ and DocID
#   determine one. The FIRST VERSION OF THIS PROBE ASSUMED THAT AND DIED -- there is no DocType
#   column. The schema had been read off reg_shape()'s relocate(any_of(...)), and any_of() silently
#   drops names that are not there, so that list is a wish list rather than a declaration. Section 3
#   now reads the real schema and everything downstream adapts to it.
#
#   THE REBUILT PATH IS THE PATH THE TREE WALK FOUND. Two ways of locating the same document must
#   agree on the string, not merely both resolve.
#
# WHAT IT ADDS BEYOND THAT
# The register keeps its sample-ladder columns on every row, so the join also answers something the
# tree walk cannot: WHERE ON THE LADDER THE LABELLED DOCUMENTS SIT. A directory listing knows nothing
# about ladder steps, so 03A currently cannot say whether the classifier is trained on documents its
# own quality rules flag as removed. Section 5 says.
#
# PREDICTIONS, STATED BEFORE RUNNING
#   4. Register coverage of the spine    100%
#   5. Removed among labelled documents  small but NOT zero -- the labelling predates the quality rules
#   6. Doc-type column resolvable        one register column agrees with the walk's DocType at 100%
#   7. Rebuilt paths that exist          100% of matched rows
#   8. Rebuilt path == tree-walk path    identical strings on every row present in both
#   9. Metadata coverage (DocDesc)       matches what 03A's raw-input table reports today
#
# Run from the project root:  source("1_code/_Tests/03A-Probe.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

pct_ <- function(.n, .d) if (.d == 0L) "-" else paste0(format(round(100 * .n / .d, 2), nsmall = 2), "%")

# Print a count tibble as aligned lines: every column but the last is a key, the last is the count.
# Built rather than left to default tibble printing, which truncates exactly the columns that matter.
lay_ <- function(.tab, .w = 30) {
  if (nrow(.tab) == 0L) return(say_("  (none)"))
  keys_ <- do.call(
    paste0,
    lapply(.tab[-ncol(.tab)], \(.c) format(dplyr::coalesce(as.character(.c), "-"), width = .w))
  )
  say_(paste0("  ", keys_, fmt_(.tab[[ncol(.tab)]]), collapse = "\n"))
}


# 1. Resolve -----------------------------------------------------------------------------------------------------------
# fs::path() rather than utils_file_path(), which creates directories as a side effect. A probe that
# leaves directories behind has changed the thing it was measuring.

here::i_am("1_code/_Commons/_Initialize.R")
source(here::here("1_code", "_Commons", "_Utils.R"), encoding = "UTF-8")

# The label spine, still at its out-of-repo location while D2 is open. Repoint here when it moves.
path_spine_ <- fs::path(
  "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MaterialContractClassification",
  "DocumentClassification/02-AK_classification_comparison/final_classification.csv"
)

path_reg_   <- here::here("2_output", "02B-Register", "Output", "Documents.parquet")
path_meta_  <- here::here("2_output", "01C-EdgarMetaData", "Output", "FullMetaData.parquet")
dir_mirror_ <- here::here("2_output", "01B-EdgarDocuments", "GetEDGAR")
path_walk_  <- here::here("2_output", "03A-ClassifyPrepare", "Cache", "ContractFiles.parquet")

say_("== Probe 03A ==")
say_("spine    : ", path_spine_, "  [", if (fs::file_exists(path_spine_)) "ok" else "MISSING", "]")
say_("register : ", fs::path_rel(path_reg_, here::here()), "  [",
     if (fs::file_exists(path_reg_)) "ok" else "MISSING", "]")
say_("metadata : ", fs::path_rel(path_meta_, here::here()), "  [",
     if (fs::file_exists(path_meta_)) "ok" else "MISSING", "]")
say_("mirror   : ", fs::path_rel(dir_mirror_, here::here()), "  [",
     if (fs::dir_exists(dir_mirror_)) "ok" else "MISSING", "]")
say_("old walk : ", fs::path_rel(path_walk_, here::here()), "  [",
     if (fs::file_exists(path_walk_)) "ok" else "absent -- sections 6 and 8 skipped", "]")
say_("")

stopifnot(
  "label spine is missing"  = fs::file_exists(path_spine_),
  "register is missing"     = fs::file_exists(path_reg_),
  "FullMetaData is missing" = fs::file_exists(path_meta_),
  "01B mirror is missing"   = fs::dir_exists(dir_mirror_)
)


# 2. The spine ---------------------------------------------------------------------------------------------------------
# readr rather than arrow: arrow's strings_can_be_null defaults to FALSE and turns a bare NA into the
# two-letter string, which is 03A's own load-bearing note. Read it the same way here or the counts
# below describe a different table from the one 03A builds.

spine_ <- readr::read_csv(
  file      = path_spine_,
  col_types = readr::cols(.default = readr::col_character(), DualClass = readr::col_integer()),
  progress  = FALSE
)

say_("== 2. The spine ==")
say_("rows              : ", fmt_(nrow(spine_)))
say_("unique DocID      : ", fmt_(dplyr::n_distinct(spine_$DocID)))
say_("duplicated DocID  : ", fmt_(sum(duplicated(spine_$DocID))))
say_("with class label  : ", fmt_(sum(!is.na(spine_$DocClassFinal1))))
say_("")


# 3. What the register actually holds -----------------------------------------------------------------------------------
# NO PREDICTION. This section exists because the first version of the probe had one and it was wrong.
# Read the schema, do not infer it from code that uses any_of().

cols_reg_ <- names(arrow::open_dataset(sources = path_reg_))

have_ <- function(.x) .x %in% cols_reg_

say_("== 3. Register schema ==")
say_("columns           : ", length(cols_reg_))
say_(strwrap(paste(cols_reg_, collapse = ", "), width = 118, prefix = "  ") |> paste(collapse = "\n"))
say_("")

# The columns a path needs, and the candidates for each.
cand_type_ <- c("DocType", "DocTypeMod", "DocTypeRaw", "Group")
cand_yq_   <- c("YQ", "YearQuarter", "YQuarter", "Quarter")
cand_ladd_ <- c("Removed", "RemClass", "SampleStepCode", "SampleStepDesc", "DescSample", "EstiSample")

got_type_ <- cand_type_[have_(cand_type_)]
got_yq_   <- cand_yq_[have_(cand_yq_)]
got_ladd_ <- cand_ladd_[have_(cand_ladd_)]

say_("wanted for a path:")
say_("  DocID           : ", if (have_("DocID")) "present" else "ABSENT")
say_("  doc-type cand.  : ", if (length(got_type_) == 0L) "NONE PRESENT" else paste(got_type_, collapse = ", "))
say_("  year-qtr cand.  : ", if (length(got_yq_) == 0L) "NONE PRESENT" else paste(got_yq_, collapse = ", "))
say_("ladder columns    : ", if (length(got_ladd_) == 0L) "NONE" else paste(got_ladd_, collapse = ", "))
say_("")

stopifnot("register has no DocID column" = have_("DocID"))


# 4. Register coverage --------------------------------------------------------------------------------------------------
# PREDICTION: every spine DocID is in the register. 02B holds all 1,771,923 documents with a ladder
# step attached, and the Compustat restriction is a separate downstream filter rather than a gate on
# membership. A shortfall here is a finding about the register, not a reason to walk the tree.
#
# This check needs DocID and nothing else -- the first version coupled it to DocType for no reason.
# Columns are selected in arrow before collect(): the register is 1.77M rows and need not arrive whole.

cols_pull_ <- unique(c("DocID", got_type_, got_yq_, got_ladd_))

reg_ <- arrow::open_dataset(sources = path_reg_) |>
  dplyr::select(dplyr::all_of(cols_pull_)) |>
  dplyr::collect() |>
  dplyr::mutate(InRegister = TRUE)

matched_ <- spine_ |>
  dplyr::select("DocID") |>
  dplyr::left_join(y = reg_, by = dplyr::join_by(DocID))

n_hit_  <- sum(!is.na(matched_$InRegister))
n_miss_ <- nrow(matched_) - n_hit_

say_("== 4. Register coverage ==")
say_("register rows     : ", fmt_(nrow(reg_)))
say_("spine rows        : ", fmt_(nrow(spine_)))
say_("found in register : ", fmt_(n_hit_), "  (", pct_(n_hit_, nrow(spine_)), ")")
say_("NOT found         : ", fmt_(n_miss_))
say_(if (n_miss_ == 0L) "  -> prediction holds; the register can gate 03A" else
     "  -> prediction FAILS; inspect the misses before designing anything")

if (n_miss_ > 0L) {
  say_("")
  say_("  first 10 unmatched DocIDs:")
  say_(paste0("    ", utils::head(matched_$DocID[is.na(matched_$InRegister)], 10), collapse = "\n"))
}
say_("")

hit_ <- matched_ |> dplyr::filter(!is.na(.data$InRegister))


# 5. Where the labelled documents sit on the ladder ---------------------------------------------------------------------
# The question the tree walk cannot answer. No prediction on the shape of these tables -- they are
# descriptive -- but Removed above zero is expected, since the labelling predates the quality rules.
# Whether such a document should train the classifier is a decision for 03A's prose, not for a probe.

say_("== 5. The labelled sample, on the register's terms ==")

for (col_ in got_type_) {
  say_("by ", col_, ":")
  hit_ |> dplyr::count(.data[[col_]], name = "Docs", sort = TRUE) |> lay_(.w = 26)
  say_("")
}

if (all(c("Removed", "RemClass") %in% got_ladd_)) {
  say_("by quality flag:")
  hit_ |> dplyr::count(.data$Removed, .data$RemClass, name = "Docs", sort = TRUE) |> lay_(.w = 14)
  say_("")
}

if ("SampleStepCode" %in% got_ladd_) {
  say_("by sample-ladder step:")
  hit_ |>
    dplyr::count(dplyr::across(dplyr::any_of(c("SampleStepCode", "SampleStepDesc"))), name = "Docs") |>
    dplyr::arrange(.data$SampleStepCode) |>
    lay_(.w = 10)
  say_("")
}

for (col_ in c("DescSample", "EstiSample")[c("DescSample", "EstiSample") %in% got_ladd_]) {
  n_ <- sum(as.logical(hit_[[col_]]), na.rm = TRUE)
  say_("in ", col_, " : ", fmt_(n_), "  (", pct_(n_, nrow(hit_)), " of matched)")
}
say_("")


# 6. Which register column names the mirror directory --------------------------------------------------------------------
# PREDICTION: exactly one candidate agrees with the walk's DocType at 100%. The walk derived DocType
# from the directory name itself -- basename(dirname(dirname(Path))) -- so it is ground truth for what
# utils_doc_path() must be handed. If NO candidate agrees, the register cannot rebuild a path as it
# stands and D1's answer changes: either 02B gains the column, or 03A gets its paths from elsewhere.

say_("== 6. Resolving the path columns ==")

col_type_ <- NA_character_
col_yq_   <- NA_character_
walk_     <- NULL

if (!fs::file_exists(path_walk_)) {
  say_("  old ContractFiles.parquet absent -- cannot resolve against ground truth")
} else {
  walk_ <- arrow::read_parquet(path_walk_) |>
    dplyr::select(dplyr::any_of(c("DocID", "DocType", "YQ", "Path")))
  names(walk_) <- sub("^DocType$", "WalkType", names(walk_))
  names(walk_) <- sub("^YQ$", "WalkYQ", names(walk_))
  names(walk_) <- sub("^Path$", "WalkPath", names(walk_))

  cmp_type_ <- hit_ |> dplyr::inner_join(y = walk_, by = dplyr::join_by(DocID))
  say_("comparable rows   : ", fmt_(nrow(cmp_type_)))

  for (col_ in got_type_) {
    agree_ <- sum(as.character(cmp_type_[[col_]]) == cmp_type_$WalkType, na.rm = TRUE)
    exact_ <- agree_ == nrow(cmp_type_)
    say_("  ", format(col_, width = 14), "vs walk DocType : ",
         format(pct_(agree_, nrow(cmp_type_)), width = 9), if (exact_) "  <- exact" else "")
    if (exact_ && is.na(col_type_)) col_type_ <- col_
  }

  for (col_ in got_yq_) {
    agree_ <- sum(as.character(cmp_type_[[col_]]) == cmp_type_$WalkYQ, na.rm = TRUE)
    exact_ <- agree_ == nrow(cmp_type_)
    say_("  ", format(col_, width = 14), "vs walk YQ      : ",
         format(pct_(agree_, nrow(cmp_type_)), width = 9), if (exact_) "  <- exact" else "")
    if (exact_ && is.na(col_yq_)) col_yq_ <- col_
  }

  say_(if (!is.na(col_type_) && !is.na(col_yq_))
         paste0("  -> use ", col_type_, " and ", col_yq_) else
         "  -> prediction FAILS; the register does not reproduce the mirror's directory names")
}
say_("")


# 7. Path reconstruction -------------------------------------------------------------------------------------------------
# PREDICTION: every matched row rebuilds to a file that exists. utils_doc_path() is pure string
# construction, so a miss means the mirror layout and the register's columns disagree.

say_("== 7. Path reconstruction ==")

paths_    <- NULL
all_exist_ <- NA

if (is.na(col_type_) || is.na(col_yq_)) {
  say_("  skipped -- no resolved doc-type and year-quarter pair (see sections 3 and 6)")
} else {
  paths_ <- utils_doc_path(
    .dir_mirror = dir_mirror_,          # 01B's parsed mirror
    .doc_type   = hit_[[col_type_]],    # resolved in section 6, not assumed
    .yq         = hit_[[col_yq_]],      # resolved in section 6
    .doc_id     = hit_$DocID
  )
  exists_    <- fs::file_exists(paths_)
  all_exist_ <- all(exists_)

  say_("paths rebuilt     : ", fmt_(length(paths_)))
  say_("exist on disk     : ", fmt_(sum(exists_)), "  (", pct_(sum(exists_), length(exists_)), ")")
  say_("MISSING           : ", fmt_(sum(!exists_)))
  say_(if (all_exist_) "  -> prediction holds; utils_doc_path() can replace the tree walk" else
       "  -> prediction FAILS; the mirror and the register disagree about where documents live")

  if (any(!exists_)) {
    say_("")
    say_("  first 5 paths that do not exist:")
    say_(paste0("    ", fs::path_rel(utils::head(paths_[!exists_], 5), here::here()), collapse = "\n"))
  }
}
say_("")


# 8. Reconciliation against the tree walk --------------------------------------------------------------------------------
# PREDICTION: identical strings on every DocID present in both. Two routes to the same file that
# resolve differently is worse than one that fails, because both look correct.

say_("== 8. Rebuilt path vs the tree walk ==")

agree_all_ <- NA
if (is.null(walk_) || is.null(paths_)) {
  say_("  skipped -- needs both the old cache and a rebuilt path")
} else {
  cmp_ <- tibble::tibble(DocID = hit_$DocID, PathReg = as.character(paths_)) |>
    dplyr::inner_join(y = dplyr::select(walk_, "DocID", "WalkPath"), by = dplyr::join_by(DocID)) |>
    dplyr::mutate(Agree = fs::path_abs(.data$PathReg) == fs::path_abs(.data$WalkPath))

  agree_all_ <- all(cmp_$Agree)
  say_("comparable rows   : ", fmt_(nrow(cmp_)))
  say_("agree             : ", fmt_(sum(cmp_$Agree)), "  (", pct_(sum(cmp_$Agree), nrow(cmp_)), ")")
  say_("DISAGREE          : ", fmt_(sum(!cmp_$Agree)))
  say_(if (agree_all_) "  -> prediction holds; the two routes are one route" else
       "  -> prediction FAILS; inspect before replacing the walk")

  if (!agree_all_) {
    say_("")
    ex_ <- cmp_ |> dplyr::filter(!.data$Agree) |> dplyr::slice_head(n = 3)
    for (i_ in seq_len(nrow(ex_))) {
      say_("    reg : ", fs::path_rel(ex_$PathReg[[i_]], here::here()))
      say_("    walk: ", fs::path_rel(ex_$WalkPath[[i_]], here::here()))
      say_("")
    }
  }
}
say_("")


# 9. Metadata coverage ---------------------------------------------------------------------------------------------------
# DocDesc and DocName are not on the register, so 03A keeps a second in-repo read of 01C.
# PREDICTION: coverage matches what 03A's raw-input table reports today.

cov_ <- spine_ |>
  dplyr::select("DocID") |>
  dplyr::left_join(
    y  = arrow::open_dataset(sources = path_meta_) |>
      dplyr::select("DocID", "DocDesc", "DocName") |>
      dplyr::collect(),
    by = dplyr::join_by(DocID)
  )

say_("== 9. Metadata coverage ==")
say_("spine rows        : ", fmt_(nrow(cov_)))
say_("with DocDesc      : ", fmt_(sum(!is.na(cov_$DocDesc))), "  (",
     pct_(sum(!is.na(cov_$DocDesc)), nrow(cov_)), ")")
say_("with DocName      : ", fmt_(sum(!is.na(cov_$DocName))), "  (",
     pct_(sum(!is.na(cov_$DocName)), nrow(cov_)), ")")
say_("")


# 10. Verdict -------------------------------------------------------------------------------------------------------------

say_("== 10. Verdict ==")
say_("register can gate 03A     : ", if (n_miss_ == 0L) "YES" else "NO -- see section 4")
say_("path columns resolvable   : ", if (!is.na(col_type_) && !is.na(col_yq_)) "YES" else "NO -- see sections 3 and 6")
say_("paths reconstruct         : ", if (is.na(all_exist_)) "UNTESTED" else if (all_exist_) "YES" else "NO -- see section 7")
say_("tree walk is redundant    : ", if (is.na(agree_all_)) "UNTESTED" else if (agree_all_) "YES" else "NO -- see section 8")
say_("")
say_("If all four are YES, 03A drops utils_list_project_files() and Cache/ContractFiles.parquet,")
say_("and gains a membership claim it cannot currently make.")
