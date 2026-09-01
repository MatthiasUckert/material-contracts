# ======================================================================================================================
# PROBE -- does joining the CT orders on DocID reach every copy of a covered contract?
# ======================================================================================================================
#
# WHAT THIS SETTLES. 02B used to collapse 01E's order references to one row per contract and join
# them onto the register by DocID. That join is moving to 10, unchanged. Before it moves, three
# things are worth knowing and none of them is guessable from the code.
#
# 1. 01E links a reference only where EXACTLY ONE Exhibit 10 document matches (HashIndex, ExhibitNo):
#
#      one_ <- .tab_docs |> dplyr::filter(dplyr::n() == 1L, .by = c("HashIndex", "ExhibitNo"))
#
#    and .tab_docs is every Exhibit 10 in the metadata, registrant copies included. If a co-filed
#    exhibit puts two rows on that key, every reference to it is labelled "7-exhibit ambiguous" and
#    never links -- which would mean multi-filer contracts are systematically uncovered.
#
# 2. If linking does reach co-filed exhibits, the DocID join reaches ONE copy. The other copies of the
#    same attachment would read HasCto = 0 beside a copy reading 1, which is a contradiction inside
#    one file rather than a missing value.
#
# 3. Whatever the answer, the move must be NEUTRAL. Contracts.parquet still holds the HasCto the
#    register produced, because 10 has not been re-run. Comparing the two settles it outright.
#
# IT WRITES NOTHING. Set the paths below if they differ.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; pure ASCII.

.p_here <- function(...) {
  if (requireNamespace("here", quietly = TRUE)) here::here(...) else file.path(...)
}

if (!exists(".PATH_01E_LIB"))  .PATH_01E_LIB  <- .p_here("1_code", "01E-CtoExhibits.R")
if (!exists(".PATH_CTO"))      .PATH_CTO      <- .p_here("2_output", "01E-CtoExhibits", "Output", "CtoExhibits.parquet")
if (!exists(".PATH_REGISTER")) .PATH_REGISTER <- .p_here("2_output", "02B-Register", "Output", "Documents.parquet")
if (!exists(".PATH_EXPORT"))   .PATH_EXPORT   <- .p_here("2_output", "10-ExportData", "Output", "Contracts.parquet")

# 1. The collapse, exactly as 02B did it -------------------------------------------------------------------------------

#' Collapse order references to one row per contract
#'
#' LIFTED VERBATIM FROM reg_attach_cto(), including the guard on the dates. min() over a set of
#' all-missing release dates returns Inf, which lands in a date column as a number that looks like
#' data rather than as an absence.
#'
#' @param .tab 01E's CtoExhibits table, one row per order reference.
#' @return A tibble, one row per covered contract DocID.
cto_collapse <- function(.tab) {
  if (FALSE) .tab <- tab_cto

  .tab |>
    dplyr::filter(!is.na(.data$DocIDContract)) |>
    dplyr::select("DocIDContract", "ReleaseDate", "IsExtension") |>
    dplyr::summarise(
      nCtoOrders      = dplyr::n(),
      CtoReleaseFirst = suppressWarnings(min(.data$ReleaseDate, na.rm = TRUE)),
      CtoReleaseLast  = suppressWarnings(max(.data$ReleaseDate, na.rm = TRUE)),
      CtoIsExtension  = as.integer(any(.data$IsExtension == 1L)),
      .by             = "DocIDContract"
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(c("CtoReleaseFirst", "CtoReleaseLast")),
        .fns  = \(.x) dplyr::if_else(is.finite(.x), .x, as.Date(NA))
      )
    )
}

# 2. Runner ------------------------------------------------------------------------------------------------------------

.cto_width_old <- getOption("width")
options(width = 150)

say_ <- function(.name, .pass, .detail = "") {
  cat(if (isTRUE(.pass)) "PASS " else "FAIL ", .name,
      if (any(nzchar(.detail))) paste0(" -- ", paste(.detail, collapse = " ")) else "", "\n", sep = "")
  isTRUE(.pass)
}

cli::cli_h1("CT order linkage: is a DocID join complete?")

need_ <- c(Lib = .PATH_01E_LIB, Cto = .PATH_CTO, Register = .PATH_REGISTER)
miss_ <- need_[!file.exists(need_)]
if (length(miss_) > 0L) {
  cli::cli_abort("Missing: {paste(names(miss_), unname(miss_), sep = ' -> ', collapse = '; ')}")
}

source(.PATH_01E_LIB, encoding = "UTF-8")   # for cto_exhibit_number()
cli::cli_alert_info("Sourced {.path {(.PATH_01E_LIB)}}")

tab_cto <- arrow::read_parquet(file = .PATH_CTO)
cli::cli_alert_info("{format(nrow(tab_cto), big.mark = ',')} order reference{?s}")

# -- Question 1: where does the linkage break? --------------------------------------------------
cli::cli_h2("Where each order reference ends up")

tab_cto |>
  dplyr::count(.data$LinkStatus, name = "nRefs") |>
  dplyr::mutate(pRefs = round(.data$nRefs / sum(.data$nRefs) * 100, 1)) |>
  dplyr::arrange(.data$LinkStatus) |>
  as.data.frame() |>
  print(row.names = FALSE, right = FALSE)

n_amb_ <- sum(tab_cto$LinkStatus == "7-exhibit ambiguous", na.rm = TRUE)
cli::cli_alert_info(
  "{format(n_amb_, big.mark = ',')} reference{?s} lost to an ambiguous exhibit -- the number this probe exists for."
)

# -- Question 2: does a co-filed exhibit collide on (HashIndex, ExhibitNo)? ----------------------
cli::cli_h2("Exhibit 10 documents sharing one filing and one exhibit number")

tab_reg <- arrow::open_dataset(sources = .PATH_REGISTER) |>
  dplyr::filter(grepl("^Exhibit10", .data$DocTypeMod)) |>
  dplyr::select("DocID", "HashDocument", "HashIndex", "DocTypeRaw", "nCIK", "MultFiler", "PrimaryFiler") |>
  dplyr::collect() |>
  dplyr::mutate(ExhibitNo = cto_exhibit_number(.x = .data$DocTypeRaw))

key_ <- tab_reg |>
  dplyr::summarise(
    nDocs      = dplyr::n(),
    nMultFiler = sum(.data$MultFiler == 1L),
    .by        = c("HashIndex", "ExhibitNo")
  )

key_ |>
  dplyr::mutate(Bucket = dplyr::if_else(.data$nDocs > 4L, "5+", as.character(.data$nDocs))) |>
  dplyr::summarise(
    nKeys        = dplyr::n(),
    nAllMulti    = sum(.data$nMultFiler == .data$nDocs),
    .by          = "Bucket"
  ) |>
  dplyr::arrange(.data$Bucket) |>
  as.data.frame() |>
  print(row.names = FALSE, right = FALSE)

cli::cli_alert_info(paste0(
  "nDocs is how many Exhibit 10 documents share one filing and one exhibit number. ",
  "Anything above one is invisible to 01E's linker."
))

res <- logical(0)
res <- c(res, say_("Co-filed exhibits do not collide on the linker's key",
                   all(key_$nDocs == 1L),
                   sprintf("%s key(s) carry more than one document",
                           format(sum(key_$nDocs > 1L), big.mark = ","))))

# -- Question 3: the fan-out gap ----------------------------------------------------------------
cli::cli_h2("Does a covered contract have copies the join would miss?")

tab_cov <- cto_collapse(.tab = tab_cto)
cli::cli_alert_info("{format(nrow(tab_cov), big.mark = ',')} covered contract{?s} after the collapse")

joined_ <- tab_reg |>
  dplyr::left_join(tab_cov, by = dplyr::join_by("DocID" == "DocIDContract")) |>
  dplyr::mutate(HasCto = as.integer(!is.na(.data$nCtoOrders)))

mixed_ <- joined_ |>
  dplyr::summarise(nCopies = dplyr::n(), nCovered = sum(.data$HasCto), .by = "HashDocument") |>
  dplyr::filter(.data$nCovered > 0L, .data$nCovered < .data$nCopies)

res <- c(res, say_("No attachment has a covered copy beside an uncovered one",
                   nrow(mixed_) == 0L,
                   sprintf("%s attachment(s) split", format(nrow(mixed_), big.mark = ","))))

if (nrow(mixed_) > 0L) {
  cli::cli_alert_warning("The split attachments, by how many copies they have:")
  mixed_ |>
    dplyr::count(.data$nCopies, .data$nCovered, name = "nAttachments") |>
    dplyr::arrange(.data$nCopies, .data$nCovered) |>
    as.data.frame() |>
    print(row.names = FALSE, right = FALSE)
}

cov_ <- dplyr::filter(joined_, .data$HasCto == 1L)
cli::cli_alert_info(paste0(
  "Of the covered rows, {sum(cov_$PrimaryFiler == 1L)} are the primary copy and ",
  "{sum(cov_$PrimaryFiler == 0L)} are not; {sum(cov_$MultFiler == 1L)} sit on a multi-filer filing."
))

# -- Question 4: is the move neutral? -----------------------------------------------------------
cli::cli_h2("Against the HasCto the register produced")

if (!file.exists(.PATH_EXPORT)) {
  cli::cli_alert_warning("No Contracts.parquet yet, so the neutrality check is skipped.")
} else {
  old_ <- arrow::open_dataset(sources = .PATH_EXPORT) |>
    dplyr::select("DocID", OldHasCto = "HasCto", OldOrders = "nCtoOrders",
                  OldFirst = "CtoReleaseFirst", OldExt = "CtoIsExtension") |>
    dplyr::collect()

  cmp_ <- old_ |>
    dplyr::left_join(dplyr::select(joined_, "DocID", "HasCto", "nCtoOrders", "CtoReleaseFirst",
                                   "CtoIsExtension"),
                     by = dplyr::join_by("DocID")) |>
    dplyr::mutate(
      nCtoOrders     = dplyr::coalesce(.data$nCtoOrders, 0L),
      CtoIsExtension = dplyr::coalesce(.data$CtoIsExtension, 0L)
    )

  res <- c(res, say_("HasCto matches the register's, row for row",
                     identical(cmp_$OldHasCto, cmp_$HasCto),
                     sprintf("%s row(s) differ", format(sum(cmp_$OldHasCto != cmp_$HasCto), big.mark = ","))))
  res <- c(res, say_("nCtoOrders matches", identical(cmp_$OldOrders, cmp_$nCtoOrders),
                     sprintf("%s differ", format(sum(cmp_$OldOrders != cmp_$nCtoOrders), big.mark = ","))))
  res <- c(res, say_("CtoIsExtension matches", identical(cmp_$OldExt, cmp_$CtoIsExtension), ""))
  res <- c(res, say_("CtoReleaseFirst matches",
                     identical(cmp_$OldFirst, cmp_$CtoReleaseFirst),
                     sprintf("%s differ", format(sum(!is.na(cmp_$OldFirst) != !is.na(cmp_$CtoReleaseFirst)),
                                                 big.mark = ","))))
  res <- c(res, say_("No date came through as an infinity",
                     !any(is.infinite(as.numeric(cmp_$CtoReleaseFirst)), na.rm = TRUE), ""))
}

cat("\n", sum(res), "/", length(res), " checks passed\n", sep = "")
options(width = .cto_width_old)
cli::cli_alert_success("Probe complete")
