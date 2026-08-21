# 03A-Probe2: settle the YQ format, and find out how much of 03A's input the register replaces --------------------------
#
# WHAT THIS IS
# The follow-up to 03A-Probe.R, which established that the register covers the labelled spine exactly
# (4,402 of 4,402) but could not rebuild a path, because its YQ column agreed with the mirror's
# directory names on ZERO of 4,402 rows. Zero rather than "most" is a format mismatch, not a data
# error, and it is the only thing standing between 03A and register-as-gate.
#
# It also settles a claim that turned out to be wrong. The 03 handoff recorded that reg_shape() drops
# DocDesc and DocName, inferred from its relocate(any_of(...)) list. Both columns are on the register.
# relocate() reorders the names it is given and leaves everything else at the end, so that list is
# neither a superset nor a subset of the schema -- it is an ordering hint and nothing more. If the
# register's copies match 01C's, 03A reads TWO inputs rather than three.
#
# WHAT IT ANSWERS
#   1. What do the two YQ formats actually look like, and does one transformation reconcile them?
#   2. Given that, do the rebuilt paths exist, and are they the strings the tree walk found?
#   3. Does the register's DocDesc / DocName / CompanyName match 01C's FullMetaData on the spine?
#   4. What does Removed say on its own? (RemClass is not on the register, so the first probe's
#      quality cross-tab required a column that does not exist and silently reported nothing.)
#
# PREDICTIONS, STATED BEFORE RUNNING
#   1. A year-and-quarter extraction reconciles them at 100%. Both encode the same fact.
#   2. Paths exist and match the walk at 100%, on all 4,402.
#   3. DocName matches 01C exactly. DocDesc matches on the 4,051 rows where 01C has one; whether the
#      register has MORE of them is the interesting case, since 92.03% coverage is a real gap.
#   4. Removed is TRUE on 31 rows, matching ladder step 02.
#
# Run from the project root:  source("1_code/_Tests/03A-Probe2.R")

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

pct_ <- function(.n, .d) if (.d == 0L) "-" else paste0(format(round(100 * .n / .d, 2), nsmall = 2), "%")


# 1. Resolve -----------------------------------------------------------------------------------------------------------

here::i_am("1_code/_Commons/_Initialize.R")
source(here::here("1_code", "_Commons", "_Utils.R"), encoding = "UTF-8")

path_spine_ <- fs::path(
  "/Users/matthiasuckert/Dropbox/MyPapers/MaterialContracts/MaterialContractClassification",
  "DocumentClassification/02-AK_classification_comparison/final_classification.csv"
)

path_reg_   <- here::here("2_output", "02B-Register", "Output", "Documents.parquet")
path_meta_  <- here::here("2_output", "01C-EdgarMetaData", "Output", "FullMetaData.parquet")
dir_mirror_ <- here::here("2_output", "01B-EdgarDocuments", "GetEDGAR")
path_walk_  <- here::here("2_output", "03A-ClassifyPrepare", "Cache", "ContractFiles.parquet")

stopifnot(
  "label spine is missing"  = fs::file_exists(path_spine_),
  "register is missing"     = fs::file_exists(path_reg_),
  "FullMetaData is missing" = fs::file_exists(path_meta_),
  "01B mirror is missing"   = fs::dir_exists(dir_mirror_),
  "old walk cache is gone"  = fs::file_exists(path_walk_)
)

spine_ <- readr::read_csv(
  file      = path_spine_,
  col_types = readr::cols(.default = readr::col_character(), DualClass = readr::col_integer()),
  progress  = FALSE
)

reg_ <- arrow::open_dataset(sources = path_reg_) |>
  dplyr::select(
    "DocID", "Group", "DocTypeMod", "YQ", "Removed", "SampleStepCode",
    "DocDesc", "DocName", "CompanyName"
  ) |>
  dplyr::collect()

walk_ <- arrow::read_parquet(path_walk_) |>
  dplyr::select(dplyr::any_of(c("DocID", "DocType", "YQ", "Path")))
names(walk_) <- c("DocID", "WalkType", "WalkYQ", "WalkPath")[match(names(walk_),
                                                                   c("DocID", "DocType", "YQ", "Path"))]

hit_ <- spine_ |>
  dplyr::select("DocID") |>
  dplyr::inner_join(y = reg_, by = dplyr::join_by(DocID)) |>
  dplyr::inner_join(y = walk_, by = dplyr::join_by(DocID))

say_("== Probe 03A, part 2 ==")
say_("spine rows joined to register and walk : ", fmt_(nrow(hit_)), " of ", fmt_(nrow(spine_)))
say_("")


# 2. The two YQ formats ---------------------------------------------------------------------------------------------------
# PREDICTION: both encode a year and a quarter, and one extraction reconciles them. Zero agreement on
# 4,402 rows cannot be a data problem -- it means the strings are shaped differently everywhere.

say_("== 2. What the two YQ columns look like ==")
say_("register YQ class : ", paste(class(hit_$YQ), collapse = "/"))
say_("walk YQ class     : ", paste(class(hit_$WalkYQ), collapse = "/"))
say_("")
say_("first 10 pairs, side by side:")
say_(paste0(
  "  reg=", format(as.character(utils::head(hit_$YQ, 10)), width = 16),
  "walk=", as.character(utils::head(hit_$WalkYQ, 10)),
  collapse = "\n"
))
say_("")
say_("distinct register YQ values : ", fmt_(dplyr::n_distinct(hit_$YQ)),
     "   distinct walk YQ values : ", fmt_(dplyr::n_distinct(hit_$WalkYQ)))
say_("")


# 3. Reconciling them -----------------------------------------------------------------------------------------------------
# A generic extraction rather than a guessed substitution: pull the four-digit year and the first
# single digit 1-4 that follows it, and rebuild in the mirror's "YYYY-Q" shape. This survives Q3,
# -Q3, .3, 3 and a bare concatenation without needing to know which it met.

yq_norm_ <- function(.x) {
  m_ <- stringi::stri_match_first_regex(as.character(.x), "(\\d{4})\\D*([1-4])")
  ifelse(is.na(m_[, 2]), NA_character_, paste0(m_[, 2], "-", m_[, 3]))
}

hit_ <- hit_ |> dplyr::mutate(YQNorm = yq_norm_(.data$YQ))

n_ok_ <- sum(hit_$YQNorm == hit_$WalkYQ, na.rm = TRUE)

say_("== 3. Reconciling the formats ==")
say_("normalised register YQ vs walk YQ : ", fmt_(n_ok_), " of ", fmt_(nrow(hit_)),
     "  (", pct_(n_ok_, nrow(hit_)), ")")
say_(if (n_ok_ == nrow(hit_)) "  -> prediction holds; one extraction reconciles them" else
     "  -> prediction FAILS; the two columns do not encode the same fact")

if (n_ok_ < nrow(hit_)) {
  say_("")
  say_("  first 10 rows that still disagree:")
  bad_ <- hit_ |> dplyr::filter(is.na(.data$YQNorm) | .data$YQNorm != .data$WalkYQ) |> dplyr::slice_head(n = 10)
  say_(paste0("    reg=", format(as.character(bad_$YQ), width = 16),
              "norm=", format(dplyr::coalesce(bad_$YQNorm, "-"), width = 10),
              "walk=", bad_$WalkYQ, collapse = "\n"))
}
say_("")


# 4. Path reconstruction and reconciliation --------------------------------------------------------------------------------
# PREDICTION: 100% exist, and 100% are the identical string the tree walk holds. Two routes to the
# same file that resolve differently is worse than one that fails, because both look correct.
#
# DocTypeMod rather than Group, arbitrarily -- both were constant "Exhibit10" on this sample and so
# both agreed trivially. That agreement says nothing about the corpus at large and must NOT be
# borrowed by 03F or 04C, whose samples are not one document type.

say_("== 4. Path reconstruction ==")

if (n_ok_ < nrow(hit_)) {
  say_("  skipped -- YQ is not reconciled")
} else {
  paths_  <- utils_doc_path(
    .dir_mirror = dir_mirror_,        # 01B's parsed mirror
    .doc_type   = hit_$DocTypeMod,    # constant "Exhibit10" here; see the note above
    .yq         = hit_$YQNorm,        # normalised in section 3
    .doc_id     = hit_$DocID
  )
  exists_ <- fs::file_exists(paths_)
  agree_  <- fs::path_abs(paths_) == fs::path_abs(hit_$WalkPath)

  say_("paths rebuilt     : ", fmt_(length(paths_)))
  say_("exist on disk     : ", fmt_(sum(exists_)), "  (", pct_(sum(exists_), length(exists_)), ")")
  say_("match the walk    : ", fmt_(sum(agree_)), "  (", pct_(sum(agree_), length(agree_)), ")")
  say_(if (all(exists_) && all(agree_)) "  -> prediction holds; the tree walk is redundant" else
       "  -> prediction FAILS; inspect below before replacing the walk")

  if (!all(exists_ & agree_)) {
    say_("")
    idx_ <- utils::head(which(!(exists_ & agree_)), 3)
    for (i_ in idx_) {
      say_("    reg : ", fs::path_rel(paths_[[i_]], here::here()), "   exists=", exists_[[i_]])
      say_("    walk: ", fs::path_rel(hit_$WalkPath[[i_]], here::here()))
      say_("")
    }
  }
}
say_("")


# 5. Does the register replace FullMetaData? -------------------------------------------------------------------------------
# PREDICTION: DocName matches exactly. DocDesc matches wherever 01C has one; the interesting case is
# whether the register has MORE, since 01C covered only 92.03% of the spine.

meta_ <- arrow::open_dataset(sources = path_meta_) |>
  dplyr::select("DocID", MetaDesc = "DocDesc", MetaName = "DocName") |>
  dplyr::collect()

cmp_ <- hit_ |>
  dplyr::select("DocID", "DocDesc", "DocName", "CompanyName") |>
  dplyr::left_join(y = meta_, by = dplyr::join_by(DocID))

say_("== 5. Register metadata vs 01C ==")
say_("rows compared     : ", fmt_(nrow(cmp_)))
say_("")
say_("DocDesc  on register : ", fmt_(sum(!is.na(cmp_$DocDesc))), "  (",
     pct_(sum(!is.na(cmp_$DocDesc)), nrow(cmp_)), ")")
say_("DocDesc  on 01C      : ", fmt_(sum(!is.na(cmp_$MetaDesc))), "  (",
     pct_(sum(!is.na(cmp_$MetaDesc)), nrow(cmp_)), ")")
say_("  identical where BOTH present : ",
     fmt_(sum(cmp_$DocDesc == cmp_$MetaDesc, na.rm = TRUE)), " of ",
     fmt_(sum(!is.na(cmp_$DocDesc) & !is.na(cmp_$MetaDesc))))
say_("  on register but NOT on 01C   : ", fmt_(sum(!is.na(cmp_$DocDesc) & is.na(cmp_$MetaDesc))))
say_("  on 01C but NOT on register   : ", fmt_(sum(is.na(cmp_$DocDesc) & !is.na(cmp_$MetaDesc))))
say_("")
say_("DocName  on register : ", fmt_(sum(!is.na(cmp_$DocName))))
say_("  identical to 01C   : ", fmt_(sum(cmp_$DocName == cmp_$MetaName, na.rm = TRUE)), " of ",
     fmt_(sum(!is.na(cmp_$DocName) & !is.na(cmp_$MetaName))))
say_("")
say_("CompanyName present  : ", fmt_(sum(!is.na(cmp_$CompanyName))), "  (",
     pct_(sum(!is.na(cmp_$CompanyName)), nrow(cmp_)), ")   [not read by 03A today]")
say_("")


# 6. Removed, on its own ---------------------------------------------------------------------------------------------------
# RemClass is NOT on the register, so the first probe's quality cross-tab required a column that does
# not exist and reported nothing at all. PREDICTION: Removed is TRUE on 31 rows, matching ladder
# step 02 ("Less: Malformatted Documents").

say_("== 6. The quality flag ==")
hit_ |>
  dplyr::count(.data$Removed, name = "Docs") |>
  (\(.t) say_(paste0("  Removed=", format(as.character(.t$Removed), width = 8),
                     fmt_(.t$Docs), collapse = "\n")))()
say_("")
say_("cross-check against ladder step 02 : ",
     fmt_(sum(hit_$SampleStepCode == 2L, na.rm = TRUE)), " documents")
say_("")


# 7. Verdict ---------------------------------------------------------------------------------------------------------------

say_("== 7. Verdict ==")
say_("If sections 3 and 4 both hold, 03A's inputs go from three to two:")
say_("  label spine + register       replaces      label spine + FullMetaData + tree walk")
say_("and utils_list_project_files(), Cache/ContractFiles.parquet and the 01C read all disappear.")
