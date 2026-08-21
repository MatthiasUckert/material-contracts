# 03A-Probe3: would the register route produce the SAME prepared.parquet? ----------------------------------------------
#
# WHAT THIS IS
# The safety check before 03A is rewritten. It builds the training sample by the proposed
# register-as-gate route, in memory, and diffs it against the prepared.parquet that the BERT models
# were actually trained on. IT WRITES NOTHING. Nothing on disk is touched, moved or recreated.
#
# WHY IT MUST RUN BEFORE THE REWRITE, NOT AFTER
# 03B's idempotency belongs to the Python engine: a run whose metrics already exist is skipped. It is
# NOT keyed on prepared.parquet. So rewriting prepared.parquet will not trigger a retrain -- which
# protects days of training -- but it also means that IF the content changed, 03B would skip and
# serve models trained on the previous sample without raising anything. That is the failure 03B's own
# prose warns about, and content-identity is the only thing standing between us and it.
#
# Once 03A is rewritten and run, prepared.parquet is overwritten and there is nothing left to compare
# against. The comparison has to happen now.
#
# HOW IT AVOIDS TOUCHING THE OLD MIRROR
# prepared.parquet CARRIES ITS OWN Text COLUMN. It is therefore the record of what was read out of
# ../pMatDisc, and comparing against it proves mirror-equivalence without reading ../pMatDisc at all.
#
# WHAT MAKES THE TEST FAIR
# It calls 03A's OWN clf_prepare_sample(), unmodified, sourced from the live 03A-ClassifyPrepare.R.
# Only the assembly of its input changes -- register instead of FullMetaData plus tree walk. If the
# input table agrees on the nine columns that function consumes, the output must agree, and any
# disagreement is a real difference rather than a reimplementation artefact.
#
# WHY Path CANNOT MATTER, AND WHERE IT STILL COULD
# clf_prepare_sample() ends in transmute(DocID, Text, DocDesc, DocName, ClassBroad, ClassDetailed,
# ClassDetailed2, AmendType, LabelRound, Fold). Path is NOT among them, so the mirror root is invisible
# in the output. It can still reach the output by three routes, and all three are tested here:
#   gate 2  a row with no Path is dropped
#   gate 3  a row whose Path does not resolve is dropped
#   gate 4  the text read from that Path becomes the Text column
#
# PREDICTIONS, STATED BEFORE RUNNING
#   4. Identical schema, in the same order, with the same types
#   5. Identical row count and identical DocID sequence
#   6. Every column identical element-for-element, INCLUDING Text and Fold
#   7. identical() on the whole tibble returns TRUE
#
# Run from the project root:  source("1_code/_Tests/03A-Probe3.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

# Element-wise difference count that treats NA as a value, so NA vs NA counts as equal and NA vs "x"
# counts as different. Plain == returns NA for both and sum(na.rm = TRUE) would hide the second case.
n_diff_ <- function(.a, .b) {
  if (length(.a) != length(.b)) return(NA_integer_)
  sum(!((is.na(.a) & is.na(.b)) | (!is.na(.a) & !is.na(.b) & .a == .b)))
}


# 1. Resolve and source ---------------------------------------------------------------------------------------------------
# Sourcing 03A-ClassifyPrepare.R is NOT inert -- see the note below. It registers taxonomy levels
# against _Plots.R's registry, in memory. Nothing is written to disk and no connection is opened.

here::i_am("1_code/_Commons/_Initialize.R")

# The same four _Commons files 03A's setup-sources chunk loads, in the same order, and for the reason
# that chunk states: 03A's own library is NOT inert. It calls plot_register_levels() three times at
# top level (03A.R:117, 124, 131) to register the taxonomy against _Plots.R's level registry, so
# _Plots.R must be in scope before it loads. Sourcing them in the document's order is also the only
# way to be sure this probe sees the same environment the render does.
source(here::here("1_code", "_Commons", "_Initialize.R"), encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Utils.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Plots.R"),      encoding = "UTF-8")
source(here::here("1_code", "_Commons", "_Tables.R"),     encoding = "UTF-8")
source(here::here("1_code", "03A-ClassifyPrepare.R"),     encoding = "UTF-8")

path_spine_ <- fs::path(
  "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MaterialContractClassification",
  "DocumentClassification/02-AK_classification_comparison/final_classification.csv"
)

path_reg_   <- here::here("2_output", "02B-Register", "Output", "Documents.parquet")
dir_mirror_ <- here::here("2_output", "01B-EdgarDocuments", "GetEDGAR")
path_prep_  <- here::here("2_output", "03A-ClassifyPrepare", "prepared.parquet")

# From 03A's config, lines 138-139. If those change, change these.
k_folds_ <- 5L
seed_    <- 42L

say_("== Probe 03A, part 3: would the rewrite change prepared.parquet? ==")
say_("THIS SCRIPT WRITES NOTHING.")
say_("")
say_("existing prepared : ", fs::path_rel(path_prep_, here::here()), "  [",
     if (fs::file_exists(path_prep_)) "ok" else "MISSING", "]")

if (!fs::file_exists(path_prep_)) {
  say_("")
  say_("  prepared.parquet is not at that path. Look for it and repoint path_prep_ before going on --")
  say_("  without it there is no ground truth and the rewrite cannot be shown to be safe.")
  say_("  candidates found under 03A:")
  say_(paste0("    ", fs::path_rel(
    fs::dir_ls(here::here("2_output", "03A-ClassifyPrepare"), recurse = TRUE, glob = "*.parquet"),
    here::here()), collapse = "\n"))
}
stopifnot("prepared.parquet not found"  = fs::file_exists(path_prep_),
          "label spine is missing"      = fs::file_exists(path_spine_),
          "register is missing"         = fs::file_exists(path_reg_),
          "01B mirror is missing"       = fs::dir_exists(dir_mirror_))

say_("mtime of prepared : ", format(fs::file_info(path_prep_)$modification_time))
say_("")


# 2. The ground truth ------------------------------------------------------------------------------------------------------

old_ <- arrow::read_parquet(path_prep_)

say_("== 2. What the models were trained on ==")
say_("rows              : ", fmt_(nrow(old_)))
say_("columns           : ", ncol(old_))
say_(paste0("  ", paste(names(old_), collapse = ", ")))
say_("fold sizes        : ", paste(as.integer(table(old_$Fold)), collapse = " / "))
say_("")


# 3. Build the input the register way ---------------------------------------------------------------------------------------
# Mirrors 03A's load-inputs chunk exactly, with one substitution: the FullMetaData join and the
# tree-walk join collapse into one register join, and Path is reconstructed rather than looked up.
#
# The YQ normalisation is the arithmetic form, not as.character(). The register stores year-quarter as
# a double (2006.3) and the mirror's directories are "2006-3"; deriving the parts by rounding avoids
# depending on how a double happens to print.

reg_ <- arrow::open_dataset(sources = path_reg_) |>
  dplyr::select("DocID", "DocDesc", "DocName", "DocTypeMod", "YQ") |>
  dplyr::collect()

yq_ <- round(reg_$YQ * 10)

reg_ <- reg_ |>
  dplyr::mutate(Path = as.character(utils_doc_path(
    .dir_mirror = dir_mirror_,
    .doc_type   = .data$DocTypeMod,
    .yq         = paste0(yq_ %/% 10, "-", yq_ %% 10),
    .doc_id     = .data$DocID
  ))) |>
  dplyr::select("DocID", "DocDesc", "DocName", "Path")

new_input_ <- readr::read_csv(
  file      = path_spine_,
  col_types = readr::cols(.default = readr::col_character(), DualClass = readr::col_integer()),
  progress  = FALSE
) |>
  dplyr::select(
    DocID, AmendType, DocClassFinal1, DocClassFinal2, DualClass,
    Level1, Level2, Provenance
  ) |>
  dplyr::left_join(y = reg_, by = dplyr::join_by(DocID))

say_("== 3. Register-route input assembled ==")
say_("rows              : ", fmt_(nrow(new_input_)))
say_("with a Path       : ", fmt_(sum(!is.na(new_input_$Path))))
say_("Path resolves     : ", fmt_(sum(fs::file_exists(new_input_$Path[!is.na(new_input_$Path)]))))
say_("")


# 4. Run 03A's own function ------------------------------------------------------------------------------------------------
# Unmodified, from the live .R. Same .round, .k and .seed as the config carries. This reads 4,402
# documents and takes a minute.

say_("== 4. Building the sample through clf_prepare_sample() ==")
new_ <- clf_prepare_sample(.tab_input = new_input_, .round = NULL, .k = k_folds_, .seed = seed_)
say_("")


# 5. Schema ------------------------------------------------------------------------------------------------------------------
# PREDICTION: identical names in identical order, identical types.

say_("== 5. Schema ==")
say_("names identical   : ", identical(names(old_), names(new_)))
if (!identical(names(old_), names(new_))) {
  say_("  only in old : ", paste(setdiff(names(old_), names(new_)), collapse = ", "))
  say_("  only in new : ", paste(setdiff(names(new_), names(old_)), collapse = ", "))
  say_("  old order   : ", paste(names(old_), collapse = ", "))
  say_("  new order   : ", paste(names(new_), collapse = ", "))
}
types_ok_ <- identical(
  vapply(old_[intersect(names(old_), names(new_))], \(.c) class(.c)[[1]], character(1)),
  vapply(new_[intersect(names(old_), names(new_))], \(.c) class(.c)[[1]], character(1))
)
say_("types identical   : ", types_ok_)
say_("")


# 6. Rows ---------------------------------------------------------------------------------------------------------------------
# PREDICTION: identical count and identical DocID sequence. Row ORDER matters and is not a formality:
# the fold deal assigns sample(n()) positionally within each ClassDetailed group, so two tables with
# the same members in a different order carry different folds.

say_("== 6. Rows ==")
say_("old rows          : ", fmt_(nrow(old_)))
say_("new rows          : ", fmt_(nrow(new_)))
say_("membership same   : ", setequal(old_$DocID, new_$DocID))
say_("  in old not new  : ", fmt_(length(setdiff(old_$DocID, new_$DocID))))
say_("  in new not old  : ", fmt_(length(setdiff(new_$DocID, old_$DocID))))
say_("DocID order same  : ", identical(old_$DocID, new_$DocID))

if (length(setdiff(old_$DocID, new_$DocID)) > 0L) {
  say_("  first 5 lost    : ", paste(utils::head(setdiff(old_$DocID, new_$DocID), 5), collapse = ", "))
}
if (length(setdiff(new_$DocID, old_$DocID)) > 0L) {
  say_("  first 5 gained  : ", paste(utils::head(setdiff(new_$DocID, old_$DocID), 5), collapse = ", "))
}
say_("")


# 7. Column by column -----------------------------------------------------------------------------------------------------------
# PREDICTION: zero differences everywhere. Text proves the two mirrors hold the same bytes; Fold
# proves the training splits are the ones the models were fitted on.

say_("== 7. Column by column ==")

if (!identical(old_$DocID, new_$DocID)) {
  say_("  DocID order differs -- comparing on a DocID join instead of positionally")
  cmp_ <- dplyr::inner_join(
    old_ |> dplyr::rename_with(\(.n) paste0(.n, ".old"), -"DocID"),
    new_ |> dplyr::rename_with(\(.n) paste0(.n, ".new"), -"DocID"),
    by = dplyr::join_by(DocID)
  )
  for (col_ in setdiff(names(old_), "DocID")) {
    d_ <- n_diff_(cmp_[[paste0(col_, ".old")]], cmp_[[paste0(col_, ".new")]])
    say_("  ", format(col_, width = 16), "differing: ", fmt_(d_), if (d_ == 0L) "   ok" else "   <-- DIFFERS")
  }
} else {
  for (col_ in names(old_)) {
    d_ <- n_diff_(old_[[col_]], new_[[col_]])
    say_("  ", format(col_, width = 16), "differing: ", fmt_(d_), if (d_ == 0L) "   ok" else "   <-- DIFFERS")
    if (d_ > 0L && col_ == "Text") {
      i_ <- utils::head(which(!((is.na(old_$Text) & is.na(new_$Text)) |
                                  (!is.na(old_$Text) & !is.na(new_$Text) & old_$Text == new_$Text))), 1)
      say_("      first differing DocID : ", old_$DocID[[i_]])
      say_("      old nchar / new nchar : ", nchar(old_$Text[[i_]]), " / ", nchar(new_$Text[[i_]]))
    }
  }
}
say_("")


# 8. Verdict ---------------------------------------------------------------------------------------------------------------------

ident_ <- identical(old_, new_)

say_("== 8. Verdict ==")
say_("identical(old, new) : ", ident_)
say_("")
if (ident_) {
  say_("  The register route reproduces prepared.parquet exactly. The rewrite is content-neutral:")
  say_("  every downstream 03 pair and 04A reads the same bytes, and no model needs refitting.")
} else {
  say_("  NOT identical. Do not rewrite 03A yet -- read sections 5 to 7 and find out which of the")
  say_("  three routes Path takes into the output is responsible. Retraining is NOT triggered")
  say_("  automatically, so a difference here would propagate silently.")
}
say_("")
say_("Reminder: this script wrote nothing. prepared.parquet is untouched.")
