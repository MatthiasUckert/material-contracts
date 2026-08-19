# 01E-Probe: what is actually in the confidential-treatment orders? ----------------------------------------------------
#
# WHAT THIS IS
# A read-only look at the CTO corpus, run before 01E is designed rather than after. Nothing is
# written and nothing is changed.
#
# WHY IT EXISTS
# The letters are a generated form, so a parser for them is cheap to write and easy to get subtly
# wrong: a pattern that fits four sampled documents can miss a variant that accounts for a tenth of
# the corpus, and the miss is invisible because the field simply comes out empty. Every question
# below is one that changes the design of 01E rather than merely describing the data.
#
#   1. FORMAT      the previous implementation downloaded PDFs and fought the text extractor. If the
#                  text already in the corpus is clean, that whole layer disappears.
#   2. TEMPLATE    how much of the corpus carries each anchor phrase the parser will rely on.
#   3. STATUS      grants, denials, extensions and amendments. A denial means the filer had to
#                  disclose; an extension points at a prior order rather than at a contract. Both
#                  need their own handling, and how much depends on how common they are.
#   4. STRUCTURE   how many registrants and how many exhibits one order carries. The multi-registrant
#                  case is where the paper's "ambiguous exhibit names" comes from.
#   5. EXHIBITS    whether the exhibit numbers parse, and what the ones that do not look like.
#
# Run from the project root:  source("1_code/_Migration/01E-Probe.R")

.N_SAMPLE <- 1000L   # documents read for the text-based questions
.SEED     <- 42L     # fixed, so two runs of this probe report the same numbers


# 1. Tools -------------------------------------------------------------------------------------------------------------

say_ <- function(...) {
  cat(..., "\n", sep = "")
  utils::flush.console()
}

rule_ <- function(...) {
  say_("\n", strrep("-", 100))
  say_("  ", ...)
  say_(strrep("-", 100))
}

fmt_ <- function(.x) format(.x, big.mark = ",", scientific = FALSE)

pct_ <- function(.x) paste0(format(round(100 * .x, 1), nsmall = 1), "%")


# 2. The corpus --------------------------------------------------------------------------------------------------------

source(here::here("1_code", "_Commons", "_Utils.R"), encoding = "UTF-8")

path_meta_ <- here::here("2_output", "01C-EdgarMetaData", "FullMetaData.parquet")
path_idx_  <- here::here("2_output", "01B-EdgarDocuments", "FilePaths.parquet")

stopifnot(
  "01C metadata missing" = fs::file_exists(path_meta_),
  "01B index missing"    = fs::file_exists(path_idx_)
)

cto_ <- arrow::open_dataset(sources = path_meta_) |>
  dplyr::filter(grepl("^CTO", .data$DocTypeMod)) |>
  dplyr::select(dplyr::any_of(c(
    "DocID", "HashIndex", "CIK", "CompanyName", "DateFiled", "YQ",
    "DocTypeRaw", "DocDesc", "DocName", "DocSize", "Removed"
  ))) |>
  dplyr::collect() |>
  dplyr::left_join(
    y  = dplyr::select(arrow::read_parquet(path_idx_), "DocID", "Path"),
    by = dplyr::join_by("DocID")
  )

rule_("1. The corpus")
say_("  orders: ", fmt_(nrow(cto_)), "   filings: ", fmt_(dplyr::n_distinct(cto_$HashIndex)),
     "   filers: ", fmt_(dplyr::n_distinct(cto_$CIK)))

say_("\n-- by year --")
cto_ |>
  dplyr::mutate(Year = as.integer(format(as.Date(.data$DateFiled), "%Y"))) |>
  dplyr::count(.data$Year, name = "nOrders") |>
  as.data.frame() |>
  print(row.names = FALSE)

say_("\n-- by file format --")
cto_ |>
  dplyr::count(.data$DocTypeRaw, name = "nOrders", sort = TRUE) |>
  utils::head(10L) |>
  as.data.frame() |>
  print(row.names = FALSE)


# 3. Read a sample -----------------------------------------------------------------------------------------------------
#
# Read once and reused by every question below. The orders are about a kilobyte each, so a thousand
# of them is a megabyte and the cost is in opening the files rather than in holding them.

rule_("2. Reading ", .N_SAMPLE, " orders")

set.seed(.SEED)
smp_ <- dplyr::slice_sample(cto_, n = min(.N_SAMPLE, nrow(cto_)))

t0_  <- Sys.time()
txt_ <- purrr::map_chr(
  .x = smp_$Path,
  .f = function(.p) {
    out_ <- try(arrow::read_parquet(.p, col_select = "TextRaw")$TextRaw[1L], silent = TRUE)
    if (inherits(out_, "try-error") || is.na(out_)) return(NA_character_)
    stringi::stri_replace_all_regex(out_, "\\s+", " ")
  }
)

say_("  read in ", round(as.numeric(difftime(Sys.time(), t0_, units = "secs")), 1), "s")
say_("  empty or unreadable: ", sum(is.na(txt_) | !nzchar(txt_)))
say_("  characters: median ", stats::median(nchar(txt_), na.rm = TRUE),
     ", range ", min(nchar(txt_), na.rm = TRUE), " to ", max(nchar(txt_), na.rm = TRUE))

ok_ <- txt_[!is.na(txt_) & nzchar(txt_)]


# 4. Is the text clean? ------------------------------------------------------------------------------------------------
#
# The previous implementation matched "F\\s*i\\s*l\\s*e" rather than "File", because its PDF text
# extractor inserted spaces inside words. If the corpus does not have that problem the parser can be
# written plainly, and a plain pattern is one that can be read and checked.

rule_("3. Is the text clean, or spaced out like PDF extraction?")

tibble::tibble(
  Plain   = pct_(mean(grepl("File No", ok_, fixed = TRUE))),
  Spaced  = pct_(mean(grepl("F\\s+i\\s+l\\s+e", ok_))),
  PlainEx = pct_(mean(grepl("Exhibit", ok_, fixed = TRUE))),
  SpacedEx = pct_(mean(grepl("E\\s+x\\s+h\\s+i\\s+b\\s+i\\s+t", ok_)))
) |>
  as.data.frame() |>
  print(row.names = FALSE)
say_("  Plain high and Spaced near zero means the defensive patterns can be dropped.")


# 5. How stable is the template? ---------------------------------------------------------------------------------------

rule_("4. Template anchors")

tibble::tibble(
  Anchor = c(
    "ORDER GRANTING CONFIDENTIAL TREATMENT",
    "submitted an application under Rule 24b-2",
    "to a Form <type>",
    "filed on <date>",
    "Exhibit <number> through <date>",
    "File No."
  ),
  Share = c(
    pct_(mean(grepl("ORDER GRANTING CONFIDENTIAL TREATMENT", ok_, ignore.case = TRUE))),
    pct_(mean(grepl("Rule 24b-2", ok_, fixed = TRUE))),
    pct_(mean(grepl("(?i)to an?\\s+Form\\s+[A-Z0-9-]+", ok_, perl = TRUE))),
    pct_(mean(grepl("(?i)filed on\\s+[A-Z][a-z]+\\s+\\d", ok_, perl = TRUE))),
    pct_(mean(grepl("(?i)Exhibit\\s+[\\d.]+\\s+through", ok_, perl = TRUE))),
    pct_(mean(grepl("(?i)File\\s+No", ok_, perl = TRUE)))
  )
) |>
  as.data.frame() |>
  print(row.names = FALSE)


# 6. Status ------------------------------------------------------------------------------------------------------------
#
# A grant links an exhibit to a contract. A denial means the filer had to disclose, which is a
# different question and arguably a more interesting one. An extension points at a prior order
# rather than at a contract, so it needs a second join. How much machinery each deserves depends
# entirely on how many there are.

rule_("5. Status: grants, denials, extensions, amendments")

sts_ <- tibble::tibble(
  Text      = ok_,
  Granted   = grepl("GRANTING CONFIDENTIAL TREATMENT", .data$Text, ignore.case = TRUE),
  Denied    = grepl("\\bdenie[ds]\\b|DENYING", .data$Text, ignore.case = TRUE),
  Extension = grepl("\\bextension\\b|\\bextend", .data$Text, ignore.case = TRUE),
  Amended   = grepl("\\bamend", .data$Text, ignore.case = TRUE)
)

tibble::tibble(
  Status = c("granted", "denied", "extension", "amended", "none of these"),
  nDocs  = c(sum(sts_$Granted), sum(sts_$Denied), sum(sts_$Extension), sum(sts_$Amended),
             sum(!sts_$Granted & !sts_$Denied & !sts_$Extension & !sts_$Amended)),
  Share  = pct_(c(mean(sts_$Granted), mean(sts_$Denied), mean(sts_$Extension), mean(sts_$Amended),
                  mean(!sts_$Granted & !sts_$Denied & !sts_$Extension & !sts_$Amended)))
) |>
  as.data.frame() |>
  print(row.names = FALSE)

say_("\n-- an order that is not a plain grant, in full --")
odd_ <- ok_[sts_$Denied | sts_$Extension | sts_$Amended]
if (length(odd_) > 0L) {
  purrr::walk(utils::head(odd_, 2L), function(.t) say_("\n", stringi::stri_sub(.t, 1L, 1600L)))
} else {
  say_("  none in this sample.")
}


# 7. Structure ---------------------------------------------------------------------------------------------------------
#
# One order can name several registrants and several exhibits. Where it names more than one of each,
# the listing block attributes each exhibit to a registrant by name -- which is the case the paper
# drops as ambiguous, and the case worth parsing properly.

rule_("6. Registrants and exhibits per order")

str_ <- tibble::tibble(
  nFileNo  = stringi::stri_count_regex(ok_, "(?i)File\\s+No"),
  nExhibit = stringi::stri_count_regex(ok_, "(?i)Exhibit\\s+[\\d(]"),
  nThrough = stringi::stri_count_regex(ok_, "(?i)\\bthrough\\b")
)

tibble::tibble(
  Quantity = c("File No. mentions", "Exhibit mentions", "'through' mentions"),
  Min      = c(min(str_$nFileNo), min(str_$nExhibit), min(str_$nThrough)),
  Median   = c(stats::median(str_$nFileNo), stats::median(str_$nExhibit), stats::median(str_$nThrough)),
  Mean     = round(c(mean(str_$nFileNo), mean(str_$nExhibit), mean(str_$nThrough)), 2),
  Max      = c(max(str_$nFileNo), max(str_$nExhibit), max(str_$nThrough))
) |>
  as.data.frame() |>
  print(row.names = FALSE)

say_("\n  orders naming more than one registrant: ", sum(str_$nFileNo > 1L),
     " (", pct_(mean(str_$nFileNo > 1L)), ")")

if (any(str_$nFileNo > 1L)) {
  say_("\n-- a multi-registrant order, in full --")
  say_("\n", stringi::stri_sub(ok_[which(str_$nFileNo > 1L)[1L]], 1L, 1800L))
}


# 8. Do the exhibit references parse? ----------------------------------------------------------------------------------
#
# The listing block is the payload: everything else in the letter is boilerplate. This applies the
# pattern 01E would use and reports what it recovers, plus what it recovers nothing from -- the
# second being the more useful of the two.

rule_("7. Exhibit references")

rex_ <- paste0(
  "(?i)Exhibit\\s+",                                       # the literal word
  "([0-9]+(?:\\.[0-9]+)*[A-Za-z]?(?:\\([a-z0-9]+\\))?)",   # 10.1, 10.18.1, 10.4A, 10(a).2
  "\\s*(?:through|until)\\s+",                             # the release clause
  "([A-Za-z]+\\s+[0-9]{1,2},\\s*[0-9]{4})"                # December 31, 2016
)

ref_ <- purrr::map(
  .x = seq_along(ok_),
  .f = function(.i) {
    m_ <- stringi::stri_match_all_regex(ok_[.i], rex_)[[1L]]
    if (all(is.na(m_))) return(tibble::tibble(i = .i, Exhibit = NA_character_, Through = NA_character_))
    tibble::tibble(i = .i, Exhibit = m_[, 2L], Through = m_[, 3L])
  }
) |>
  dplyr::bind_rows()

hit_ <- dplyr::filter(ref_, !is.na(.data$Exhibit))

say_("  orders with at least one parsed reference: ", fmt_(dplyr::n_distinct(hit_$i)),
     " of ", fmt_(length(ok_)), " (", pct_(dplyr::n_distinct(hit_$i) / length(ok_)), ")")
say_("  references parsed: ", fmt_(nrow(hit_)),
     "   per order: ", round(nrow(hit_) / max(dplyr::n_distinct(hit_$i), 1L), 2))

say_("\n-- exhibit numbers, most frequent --")
hit_ |>
  dplyr::count(.data$Exhibit, name = "n", sort = TRUE) |>
  utils::head(12L) |>
  as.data.frame() |>
  print(row.names = FALSE)

say_("\n-- exhibit numbers that are not the plain 10.n shape --")
hit_ |>
  dplyr::filter(!grepl("^[0-9]+\\.[0-9]+$", .data$Exhibit)) |>
  dplyr::count(.data$Exhibit, name = "n", sort = TRUE) |>
  utils::head(15L) |>
  as.data.frame() |>
  print(row.names = FALSE)

say_("\n-- how many references are Exhibit 10 rather than 99, 4, and so on --")
hit_ |>
  dplyr::mutate(Series = stringi::stri_extract_first_regex(.data$Exhibit, "^[0-9]+")) |>
  dplyr::count(.data$Series, name = "n", sort = TRUE) |>
  utils::head(8L) |>
  as.data.frame() |>
  print(row.names = FALSE)

miss_ <- setdiff(seq_along(ok_), unique(hit_$i))
if (length(miss_) > 0L) {
  say_("\n-- orders where nothing parsed: ", length(miss_), ". Two of them, in full --")
  purrr::walk(utils::head(miss_, 2L), function(.i) say_("\n", stringi::stri_sub(ok_[.i], 1L, 1400L)))
}


# 9. Can the source filing be found? -----------------------------------------------------------------------------------
#
# The letter names the filing whose exhibits were redacted, by form type and filing date. If that
# filing is in the master index under the same CIK, the join to the contract is two exact hops and
# needs no fuzzy matching at all.

rule_("8. Does the named source filing exist in the master index?")

path_mst_ <- here::here("2_output", "01C-EdgarMetaData", "MasterIndex.parquet")

if (fs::file_exists(path_mst_)) {
  mst_ <- arrow::read_parquet(path_mst_) |>
    dplyr::select("CIK", "FormType", "DateFiled", "HashIndex") |>
    dplyr::distinct()

  src_ <- tibble::tibble(
    CIK      = smp_$CIK[!is.na(txt_) & nzchar(txt_)],
    Form     = stringi::stri_match_first_regex(ok_, "(?i)to an?\\s+Form\\s+([A-Z0-9/-]+)")[, 2L],
    FiledRaw = stringi::stri_match_first_regex(ok_, "(?i)filed on\\s+([A-Za-z]+\\s+[0-9]{1,2},\\s*[0-9]{4})")[, 2L]
  ) |>
    dplyr::mutate(
      Form  = toupper(trimws(.data$Form)),
      Filed = suppressWarnings(as.Date(.data$FiledRaw, format = "%B %d, %Y"))
    )

  say_("  form type parsed: ", pct_(mean(!is.na(src_$Form))),
       "   filing date parsed: ", pct_(mean(!is.na(src_$Filed))))

  say_("\n-- source form types named --")
  src_ |>
    dplyr::count(.data$Form, name = "n", sort = TRUE) |>
    utils::head(10L) |>
    as.data.frame() |>
    print(row.names = FALSE)

  join_ <- src_ |>
    dplyr::filter(!is.na(.data$Form), !is.na(.data$Filed)) |>
    dplyr::left_join(
      y  = dplyr::rename(mst_, Form = "FormType", Filed = "DateFiled"),
      by = dplyr::join_by("CIK", "Form", "Filed")
    )

  say_("\n  exact match on CIK + form + filing date: ",
       pct_(mean(!is.na(join_$HashIndex))), " of ", fmt_(nrow(join_)), " parsed orders")
  say_("  A high share means the linkage is arithmetic. A low one means the master index does not")
  say_("  carry the named form -- 6-K and 20-F are outside the seventeen types 01A selects.")
} else {
  say_("  MasterIndex.parquet not found; skipped.")
}


# 10. Verdict ----------------------------------------------------------------------------------------------------------

rule_("9. What this decides")

say_("  Section 3  clean text means the parser needs no defensive spacing patterns.")
say_("  Section 5  denials and extensions each need handling only in proportion to their share.")
say_("  Section 6  multi-registrant orders are the case the paper drops as ambiguous.")
say_("  Section 7  the exhibit series tells us how many references are contracts at all.")
say_("  Section 8  a high exact-match share means 01E is a join rather than a matching problem.")
