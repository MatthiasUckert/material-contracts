# ======================================================================================================================
# 05B-ItemTaxonomy -- what the 2004 item reform did, and which population each number comes from
# ======================================================================================================================
#
# THE CLAIM THIS DOCUMENT EXISTS TO SETTLE. The response to referee 2's comment 6A says the decline in
# voluntary item counts "is largest for firms whose pre-period 8-Ks were disproportionately Item 5
# filings -- which is the treated group by construction, since these firms were not using 8-Ks for
# contracts". The conclusion drawn from it is large: the design is abandoned and both specifications
# are demoted to an online appendix. "By construction" is not a construction. It is an empirical claim
# about pre-2004 data, and a referee can check it.
#
# THE PRIOR RUNS THE OTHER WAY. Before 23 August 2004 there was no Item 1.01, and Item 5 was "Other
# events", the catch-all. A firm disclosing a material contract by 8-K in that period had nowhere else
# to file it. But that firm is the CONTROL group -- it was already using 8-Ks for contracts. The
# treated firm, which was not, filed 8-Ks for earnings, auditor changes and acquisitions: the
# mandatory items. So the memo's sentence attributes the mechanism to the wrong group.
#
# THE MECHANISM AND THE ATTRIBUTION ARE TWO CLAIMS AND ONLY ONE OF THEM IS IN DOUBT. That Item 5
# content was redistributed into new mandatory items after the reform is a fact about the taxonomy,
# and this document measures it. Which group was Item-5-heavy beforehand is the contested part, and
# this document is explicit about how far the available data can settle it.
#
# THREE POPULATIONS, AND CONFUSING THEM IS THE WHOLE RISK -----------------------------------------------------------
#
#   LANDING PAGES (01C)      Filings that were RETRIEVED, which means filings carrying an Exhibit 10.
#                            Carries Items. This is the only source of item lists anywhere in the
#                            pipeline, and it is not a sample of all 8-Ks -- it is the contract-bearing
#                            ones.
#
#   MASTER INDEX, RAW (01A)  Every filing EDGAR indexed, contract-bearing or not. Carries no Items,
#                            because the full index does not publish them. This is the only way to ask
#                            whether a firm filed 8-Ks at all.
#
#   MASTER INDEX, 01C OUTPUT Restricted to the retrieved set, exactly like the landing table. Reading
#                            it as the universe would compare the retrieved population against itself
#                            and find, unsurprisingly, that they agree.
#
# WHY THE POPULATION MATTERS MORE THAN THE STATISTIC HERE. Pre-2004 the landing table holds a few
# thousand 8-Ks a year; post-2004 it holds tens of thousands. That jump is not a fact about EDGAR, it
# is the treatment: before the reform almost nobody filed contracts by 8-K and afterwards everyone had
# to. It follows that the pre-2004 8-Ks in that table are, by construction, control-group filings --
# treated firms' pre-period 8-Ks were never retrieved, because they carried no contract. An Item 5
# share computed there is a statement about control firms and about nothing else, and reporting it as
# though it covered both groups would repeat the memo's error in a new place.
#
# WHAT CANNOT BE ANSWERED WITHOUT A NETWORK PASS, stated here so nobody looks for it below: the item
# lists of treated firms' NON-contract 8-Ks. Those filings exist and EDGAR indexes them, but their
# landing pages were never fetched because nothing in them was wanted. Getting them is a scrape of
# tens of thousands of pages against the SEC rate limit, which is hours, and 01C already holds the
# machinery for it if the answer is ever worth that.


# 1. Guards ------------------------------------------------------------------------------------------------------------

#' Refuse to read a file that does not carry the columns being asked for
#'
#' THE SCHEMA COMES FROM ARROW AND IS NEVER INFERRED FROM THE CODE THAT WROTE IT. 02B shapes the
#' register with relocate(any_of(...)), and any_of() ignores a name that is not there, so a writer's
#' column list states intent rather than fact. The abort names every missing column and prints what
#' the file actually carries, which is usually enough to see the right name without opening anything.
#'
#' @param .path Path to a parquet file or dataset directory.
#' @param .cols Character vector of column names the caller is about to select.
#' @return The file's column names, invisibly.
itx_require_cols <- function(.path, .cols) {
  if (FALSE) {
    .path <- .lP$Input$Landing
    .cols <- c("HashIndex", "CIK", "FormType", "FilingDate", "Items")
  }

  if (!fs::file_exists(.path) && !fs::dir_exists(.path)) {
    cli::cli_abort("Nothing to read at {.path {(.path)}}.")
  }

  have_ <- names(arrow::open_dataset(sources = .path))
  miss_ <- setdiff(.cols, have_)

  if (length(miss_) > 0L) {
    cli::cli_abort(c(
      "x" = "{fs::path_file(.path)} is missing {cli::qty(miss_)}column{?s}: {(miss_)}.",
      "i" = "It carries: {(have_)}."
    ))
  }
  invisible(have_)
}


#' Where 01A mirrored the unrestricted master index
#'
#' RECONSTRUCTED THROUGH rGetEDGAR RATHER THAN TYPED, for the reason 01A gives when it builds the same
#' path: calling get_directories() means the layout cannot drift from what the package expects. The
#' root is 01A's own output directory, which is a different mirror from 01B's document tree.
#'
#' A MISSING INDEX IS RETURNED AS NA RATHER THAN RAISED. Every table that needs it says so and is
#' skipped; the taxonomy results do not depend on it, and a document that refuses to render because
#' one supporting table is unavailable is worse than one that reports the gap.
#'
#' @param .dir_here Project root.
#' @return Path to the parquet directory, or NA_character_ where it cannot be resolved.
itx_master_dir <- function(.dir_here) {
  if (FALSE) .dir_here <- here::here()

  root_ <- fs::path(
    init_create_script_dir(.dir_here = .dir_here, .name_script = "01A-EdgarIndex"), "GetEDGAR"
  )
  if (!fs::dir_exists(root_)) {
    cli::cli_alert_warning("No index mirror at {.path {(root_)}}; universe tables are skipped.")
    return(NA_character_)
  }

  out_ <- tryCatch(
    as.character(rGetEDGAR::get_directories(root_)$MasterIndex$DirParquet),
    error = function(e) {
      cli::cli_alert_warning("rGetEDGAR could not resolve the mirror ({conditionMessage(e)}).")
      NA_character_
    }
  )

  if (!is.na(out_) && !fs::dir_exists(out_)) {
    cli::cli_alert_warning("The index mirror resolves to {.path {(out_)}}, which does not exist.")
    return(NA_character_)
  }
  out_
}


# 2. Input -------------------------------------------------------------------------------------------------------------

#' Filing-level rows from the landing table
#'
#' ONE ROW PER FILING AND NOT PER DOCUMENT. The landing table is keyed on HashIndex, which identifies
#' the filing; a filing carrying nine exhibits appears once here and nine times in the register. Every
#' count in this document is a count of filings, so the distinction is load-bearing rather than tidy.
#'
#' @param .path Landing table.
#' @param .forms Character. Regular expression the form type must match. NULL keeps every form.
#' @return Tibble: HashIndex, CIK, CompanyName, FormType, FilingDate, Items.
itx_read_landing <- function(.path, .forms = "^8-K") {
  if (FALSE) {
    .path  <- .lP$Input$Landing
    .forms <- "^8-K"
  }

  cols_ <- c("HashIndex", "CIK", "CompanyName", "FormType", "FilingDate", "Items")
  itx_require_cols(.path = .path, .cols = cols_)

  arr_ <- arrow::open_dataset(sources = .path) |>
    dplyr::select(dplyr::all_of(cols_))

  if (!is.null(.forms)) arr_ <- dplyr::filter(arr_, grepl(.forms, .data$FormType))

  arr_ |>
    dplyr::collect() |>
    dplyr::distinct(.data$HashIndex, .keep_all = TRUE) |>
    dplyr::mutate(
      FilingDate = as.Date(.data$FilingDate),
      Year       = as.integer(format(.data$FilingDate, "%Y"))
    ) |>
    dplyr::filter(!is.na(.data$Year))
}


#' Filing counts from the unrestricted index
#'
#' THE ONLY TABLE IN THIS DOCUMENT DRAWN FROM THE WHOLE OF EDGAR, and the only one that can say
#' whether a firm filed 8-Ks at all. It carries no item list, so it answers questions about VOLUME and
#' never about content.
#'
#' COUNTS ARE AGGREGATED IN ARROW, NOT COLLECTED. The mirrored index is tens of millions of rows
#' across every form type; collecting it to count would spend gigabytes to produce a table of a few
#' hundred.
#'
#' @param .dir Parquet directory of the mirrored index, or NA.
#' @param .forms Character. Regular expression the form type must match.
#' @return Tibble: CIK, Year, nFilings. Zero rows where the index is unavailable.
itx_read_master <- function(.dir, .forms = "^8-K") {
  if (FALSE) {
    .dir   <- dir_master
    .forms <- "^8-K"
  }

  empty_ <- tibble::tibble(CIK = character(0), Year = integer(0), nFilings = integer(0))
  if (is.na(.dir)) return(empty_)

  have_ <- names(arrow::open_dataset(sources = .dir))
  col_f_ <- intersect(c("FormType", "Type"), have_)[[1L]]
  col_c_ <- intersect(c("CIK", "Cik"), have_)[[1L]]
  col_d_ <- intersect(c("DateFiled", "FilingDate", "Date"), have_)[[1L]]

  if (any(is.na(c(col_f_, col_c_, col_d_)))) {
    cli::cli_alert_warning("The index carries {(have_)}; no form, CIK and date triple. Skipped.")
    return(empty_)
  }

  arrow::open_dataset(sources = .dir) |>
    dplyr::select(FormType = dplyr::all_of(col_f_), CIK = dplyr::all_of(col_c_),
                  DateFiled = dplyr::all_of(col_d_)) |>
    dplyr::filter(grepl(.forms, .data$FormType)) |>
    # THE YEAR IS TAKEN WITH lubridate AND NOT BY SLICING THE STRING. This mutate runs inside Arrow,
    # before the collect, because the mirrored index is tens of millions of rows and aggregating it
    # in R would spend gigabytes to produce a few hundred. stringi::stri_sub() has no Arrow binding
    # and would abort the query; lubridate::year() has one, and it also stops depending on the date
    # having been stored as a string that happens to start with the year.
    dplyr::mutate(Year = as.integer(lubridate::year(as.Date(.data$DateFiled)))) |>
    dplyr::summarise(nFilings = dplyr::n(), .by = c("CIK", "Year")) |>
    dplyr::collect() |>
    dplyr::mutate(CIK = as.character(.data$CIK)) |>
    dplyr::filter(!is.na(.data$Year))
}


#' Which filings carried a material contract, and under which form
#'
#' THE REGISTER IS WHAT MAKES A FILING "CONTRACT-BEARING", and it is a document-level table, so the
#' Exhibit 10 rows are reduced to their distinct filings here. A firm attaching six exhibits to one
#' 8-K disclosed contracts once, not six times.
#'
#' @param .path_register 02B's Documents.parquet.
#' @return Tibble: HashIndex, nExhibits.
itx_contract_filings <- function(.path_register) {
  if (FALSE) .path_register <- .lP$Input$Register

  cols_ <- c("HashIndex", "Group", "PrimaryFiler")
  itx_require_cols(.path = .path_register, .cols = cols_)

  arrow::open_dataset(sources = .path_register) |>
    dplyr::select(dplyr::all_of(cols_)) |>
    dplyr::filter(.data$Group == "Exhibit10") |>
    dplyr::collect() |>
    dplyr::filter(as.logical(.data$PrimaryFiler) %in% TRUE) |>
    dplyr::summarise(nExhibits = dplyr::n(), .by = "HashIndex")
}


# 3. Construction: parsing the item list --------------------------------------------------------------------------------

#' Split one filing's item list into codes and labels
#'
#' THE SEPARATOR IS A NEWLINE AND ONLY A NEWLINE. 01D rewrites it to a pipe for its own cache key, and
#' copying that convention here would be a bug: the labels themselves contain colons and commas.
#' "Item 5.02: Departure of Directors or Certain Officers: Election of Directors: Appointment of
#' Certain Officers" is ONE item, and splitting on colons turns it into four -- which is exactly what
#' a first pass at this did, producing three phantom codes each appearing 88,974 times.
#'
#' THE CODE IS ANCHORED AND THE LABEL IS EVERYTHING AFTER THE FIRST COLON. Anchoring matters because
#' a label can contain the word "Item"; taking the first colon rather than the last matters because
#' most of them contain several.
#'
#' PRE-REFORM AND POST-REFORM CODES ARE DIFFERENT OBJECTS THAT LOOK ALIKE. "5" is the old Other Events
#' catch-all and "5.02" is departures of directors; "1.05" contains a 5 and means neither. Comparing
#' the parsed code exactly is the only safe test, and every count below does.
#'
#' @param .tab Tibble with HashIndex and Items.
#' @return Tibble: HashIndex, ItemCode, ItemLabel, IsDotted, one row per item per filing.
itx_parse_items <- function(.tab) {
  if (FALSE) .tab <- tab_land

  hit_ <- dplyr::filter(.tab, !is.na(.data$Items) & nzchar(.data$Items))

  parts_ <- stringi::stri_split_regex(hit_$Items, "\n")
  lens_  <- vapply(parts_, length, integer(1))

  raw_ <- tibble::tibble(
    HashIndex = rep(hit_$HashIndex, times = lens_),
    Raw       = trimws(unlist(parts_, use.names = FALSE))
  ) |>
    dplyr::filter(nzchar(.data$Raw))

  raw_ |>
    dplyr::mutate(
      ItemCode  = stringi::stri_match_first_regex(
        .data$Raw, "^Item[[:space:]]+([0-9]+(?:\\.[0-9]+)?)[[:space:]]*:"
      )[, 2L],
      ItemLabel = trimws(stringi::stri_replace_first_regex(
        .data$Raw, "^Item[[:space:]]+[0-9]+(?:\\.[0-9]+)?[[:space:]]*:[[:space:]]*", ""
      )),
      IsDotted  = grepl(".", .data$ItemCode, fixed = TRUE)
    ) |>
    dplyr::select("HashIndex", "ItemCode", "ItemLabel", "IsDotted")
}


#' Which side of the reform a filing falls on
#'
#' THE BOUNDARY IS A DATE AND NOT A YEAR. The release took effect on 23 August 2004, so a calendar
#' split puts seven months of old-taxonomy filings on the new side. 2004 is reported as its own era
#' rather than folded into either, because a filing from that year is only interpretable once you know
#' which half it came from -- and the counts either side of the boundary are themselves a check that
#' the boundary is in the right place.
#'
#' @param .date Date vector.
#' @param .reform Date. When the new taxonomy took effect.
#' @return Factor with levels Pre, Transition, Post.
itx_era <- function(.date, .reform = as.Date("2004-08-23")) {
  if (FALSE) {
    .date   <- tab_land$FilingDate
    .reform <- as.Date("2004-08-23")
  }

  out_ <- dplyr::case_when(
    .date <  as.Date("2004-01-01") ~ "Pre",
    .date <  .reform               ~ "Transition",
    .default                       = "Post"
  )
  factor(out_, levels = c("Pre", "Transition", "Post"))
}


#' Name the era of a group of filings, honestly, when they may span more than one
#'
#' A YEAR IS NOT AN ERA, AND 2004 IS THE YEAR THAT PROVES IT. The reform took effect on 23 August, so
#' calendar 2004 holds filings from both sides of the boundary. An earlier version labelled the row
#' with whichever era happened to come first, which put "Transition" on a row reporting 26% Item 5 AND
#' 53% Item 1.01 -- numbers that are correct together and impossible from one side of the boundary.
#' The label implied a claim the row does not make.
#'
#' @param .era Factor or character vector of era labels.
#' @return A single string: the era where they agree, "Mixed" where they do not.
itx_era_label <- function(.era) {
  if (FALSE) .era <- factor(c("Transition", "Post"))
  u_ <- unique(as.character(.era))
  if (length(u_) == 1L) u_ else "Mixed"
}


#' Treated or control, from what a firm did with its contracts before the reform
#'
#' THE DEFINITION IS THE MEMO'S OWN: treated firms "were not using 8-Ks for contracts". So a firm is
#' control where at least one of its pre-reform material contracts arrived attached to an 8-K, and
#' treated where its pre-reform contracts arrived only through periodic filings.
#'
#' THIS IS A RECONSTRUCTION AND IT IS FLAGGED AS ONE WHEREVER IT IS USED. The estimation sample is
#' defined in Stata, not here, and a split that does not match the one the table was built on answers
#' a question nobody asked. Robustness varies the threshold so a reader can see whether the answer
#' turns on the definition or survives it.
#'
#' @param .tab_land Landing rows for ALL forms, not just 8-K.
#' @param .contract Output of itx_contract_filings().
#' @param .reform Date the new taxonomy took effect.
#' @param .min_contracts Integer. Pre-reform contract filings a firm needs before it is classified at
#'   all; below it the firm is Unclassified rather than forced into a group.
#' @return Tibble: CIK, nPre, nPre8K, Status.
itx_firm_status <- function(.tab_land, .contract, .reform = as.Date("2004-08-23"),
                            .min_contracts = 1L) {
  if (FALSE) {
    .tab_land      <- tab_land_all
    .contract      <- tab_contract
    .reform        <- as.Date("2004-08-23")
    .min_contracts <- 1L
  }

  .tab_land |>
    dplyr::inner_join(.contract, by = dplyr::join_by(HashIndex)) |>
    dplyr::filter(.data$FilingDate < .reform) |>
    dplyr::summarise(
      nPre   = dplyr::n(),
      nPre8K = sum(grepl("^8-K", .data$FormType)),
      .by    = "CIK"
    ) |>
    dplyr::mutate(
      Status = dplyr::case_when(
        .data$nPre < .min_contracts ~ "Unclassified",
        .data$nPre8K > 0L           ~ "Control",
        .default                    = "Treated"
      ),
      Status = factor(.data$Status, levels = c("Treated", "Control", "Unclassified"))
    )
}


# 4. Selection: the tables ----------------------------------------------------------------------------------------------

#' Contract-bearing 8-Ks against every 8-K EDGAR indexed
#'
#' THE POPULATION TABLE, AND THE MOST IMPORTANT ONE IN THIS DOCUMENT. Every other number here is
#' conditional on a filing having been retrieved, and this is what shows how strong that condition is.
#' A retrieved share that is small before the reform and large after it means the pre-period holds
#' contract-disclosing firms and essentially nobody else.
#'
#' THE TABLE STOPS WHERE THE CORPUS STOPS. 01A closes the acquisition frame at a fixed year so that
#' renders stay comparable, and EDGAR keeps indexing filings past it. A year outside the frame
#' therefore has indexed 8-Ks and, by construction, no retrieved ones -- which prints as a retrieved
#' share of zero and reads as a collapse in contract disclosure. It is not a measurement, so it is not
#' shown.
#'
#' @param .tab_land 8-K landing rows.
#' @param .contract Output of itx_contract_filings().
#' @param .master Output of itx_read_master().
#' @param .year_min Integer or NULL. First year reported.
#' @param .year_max Integer or NULL. Last year reported; the frame closure, not a display preference.
#' @return Tibble: Year, nContract8K, nAll8K, ShareRetrieved.
itx_table_population <- function(.tab_land, .contract, .master, .year_min = NULL,
                                 .year_max = NULL) {
  if (FALSE) {
    .tab_land <- tab_land
    .contract <- tab_contract
    .master   <- tab_master
    .year_min <- 2001L
    .year_max <- 2024L
  }

  con_ <- .tab_land |>
    dplyr::inner_join(.contract, by = dplyr::join_by(HashIndex)) |>
    dplyr::summarise(nContract8K = dplyr::n(), .by = "Year")

  all_ <- if (nrow(.master) == 0L) {
    tibble::tibble(Year = integer(0), nAll8K = integer(0))
  } else {
    dplyr::summarise(.master, nAll8K = sum(.data$nFilings), .by = "Year")
  }

  out_ <- dplyr::full_join(con_, all_, by = dplyr::join_by(Year)) |>
    dplyr::mutate(
      nContract8K    = as.integer(dplyr::coalesce(.data$nContract8K, 0L)),
      ShareRetrieved = dplyr::if_else(
        is.na(.data$nAll8K) | .data$nAll8K == 0L, NA_real_, .data$nContract8K / .data$nAll8K
      )
    )

  if (!is.null(.year_min)) out_ <- dplyr::filter(out_, .data$Year >= .year_min)
  if (!is.null(.year_max)) out_ <- dplyr::filter(out_, .data$Year <= .year_max)

  dplyr::arrange(out_, .data$Year)
}


#' Item codes by era
#'
#' @param .items Output of itx_parse_items().
#' @param .tab_land 8-K landing rows carrying Era.
#' @param .n Integer. Codes to keep per era.
#' @return Tibble: Era, ItemCode, ItemLabel, nFilings, Share.
itx_table_codes <- function(.items, .tab_land, .n = 12L) {
  if (FALSE) {
    .items    <- tab_items
    .tab_land <- tab_land
    .n        <- 12L
  }

  .items |>
    dplyr::inner_join(dplyr::select(.tab_land, "HashIndex", "Era"),
                      by = dplyr::join_by(HashIndex)) |>
    dplyr::summarise(
      nFilings  = dplyr::n_distinct(.data$HashIndex),
      ItemLabel = dplyr::first(.data$ItemLabel),
      .by       = c("Era", "ItemCode")
    ) |>
    dplyr::mutate(
      Total = sum(.data$nFilings), Share = .data$nFilings / .data$Total, .by = "Era"
    ) |>
    dplyr::slice_max(.data$nFilings, n = .n, by = "Era", with_ties = FALSE) |>
    dplyr::select("Era", "ItemCode", "ItemLabel", "nFilings", "Share") |>
    dplyr::arrange(.data$Era, dplyr::desc(.data$nFilings))
}


#' The share of contract-bearing 8-Ks carrying a given item, by year
#'
#' THE NUMBER THE MEMO NEEDS, and the one whose population has to be stated in the same breath. It
#' covers contract-bearing 8-Ks, which before the reform means control-group filings, so it is a
#' statement about how firms that WERE using 8-Ks for contracts filed them -- not a comparison between
#' groups.
#'
#' @param .items Output of itx_parse_items().
#' @param .tab_land 8-K landing rows.
#' @param .contract Output of itx_contract_filings().
#' @param .codes Character. Item codes to report.
#' @return Tibble: Year, Era, nFilings, one share column per requested code.
itx_table_item_share <- function(.items, .tab_land, .contract, .codes = c("5", "7", "1.01", "8.01")) {
  if (FALSE) {
    .items    <- tab_items
    .tab_land <- tab_land
    .contract <- tab_contract
    .codes    <- c("5", "7", "1.01", "8.01")
  }

  base_ <- dplyr::inner_join(.tab_land, .contract, by = dplyr::join_by(HashIndex))

  flags_ <- .items |>
    dplyr::filter(.data$ItemCode %in% .codes) |>
    dplyr::distinct(.data$HashIndex, .data$ItemCode) |>
    dplyr::mutate(Has = 1L) |>
    tidyr::pivot_wider(names_from = "ItemCode", values_from = "Has", names_prefix = "Item_",
                       values_fill = 0L)

  base_ |>
    dplyr::left_join(flags_, by = dplyr::join_by(HashIndex)) |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("Item_"), \(.x) dplyr::coalesce(.x, 0L))) |>
    dplyr::summarise(
      nFilings = dplyr::n(),
      Era      = itx_era_label(.era = .data$Era),
      dplyr::across(dplyr::starts_with("Item_"), mean),
      .by      = "Year"
    ) |>
    dplyr::arrange(.data$Year)
}


#' What else a pre-reform Item 5 filing carried, and what replaced it after
#'
#' THE REDISTRIBUTION CLAIM MADE CHECKABLE. If the reform moved contract disclosure out of the catch-all
#' and into a mandatory item, then pre-reform contract 8-Ks concentrate on Item 5 and post-reform ones
#' concentrate on Item 1.01, with the catch-all surviving as 8.01 at a fraction of its old share.
#'
#' @param .items Output of itx_parse_items().
#' @param .tab_land 8-K landing rows.
#' @param .contract Output of itx_contract_filings().
#' @return Tibble: Era, Role, ItemCode, ItemLabel, Share.
itx_table_shift <- function(.items, .tab_land, .contract) {
  if (FALSE) {
    .items    <- tab_items
    .tab_land <- tab_land
    .contract <- tab_contract
  }

  base_ <- .tab_land |>
    dplyr::inner_join(.contract, by = dplyr::join_by(HashIndex)) |>
    dplyr::filter(.data$Era != "Transition") |>
    dplyr::select("HashIndex", "Era")

  n_by_ <- dplyr::summarise(base_, nFilings = dplyr::n(), .by = "Era")

  .items |>
    dplyr::inner_join(base_, by = dplyr::join_by(HashIndex)) |>
    dplyr::summarise(
      n         = dplyr::n_distinct(.data$HashIndex),
      ItemLabel = dplyr::first(.data$ItemLabel),
      .by       = c("Era", "ItemCode")
    ) |>
    dplyr::left_join(n_by_, by = dplyr::join_by(Era)) |>
    dplyr::mutate(Share = .data$n / .data$nFilings) |>
    dplyr::slice_max(.data$Share, n = 8L, by = "Era", with_ties = FALSE) |>
    dplyr::select("Era", "ItemCode", "ItemLabel", "n", "Share") |>
    dplyr::arrange(.data$Era, dplyr::desc(.data$Share))
}


#' Treated and control firms, and what each did with 8-Ks before the reform
#'
#' THE NON-TAUTOLOGICAL PART OF THE COMPARISON. That treated firms filed no contract-bearing 8-Ks
#' before the reform is true by definition and worth nothing. Whether they filed 8-Ks AT ALL is not,
#' and only the unrestricted index can answer it: a treated firm that filed 8-Ks regularly and simply
#' never put a contract in one is a different object from a firm that had no 8-K practice, and the
#' memo's mechanism needs the first.
#'
#' @param .status Output of itx_firm_status().
#' @param .master Output of itx_read_master().
#' @param .pre_years Integer vector. Years counted as the pre-period.
#' @return Tibble: Status, nFirms, nWithAny8K, ShareWithAny8K, Median8K, MeanContract8K.
itx_table_firms <- function(.status, .master, .pre_years = 2001:2003) {
  if (FALSE) {
    .status    <- tab_status
    .master    <- tab_master
    .pre_years <- 2001:2003
  }

  if (nrow(.master) == 0L) {
    return(tibble::tibble(
      Status = factor(character(0), levels = levels(.status$Status)),
      nFirms = integer(0), nWithAny8K = integer(0), ShareWithAny8K = numeric(0),
      Median8K = numeric(0), MeanContract8K = numeric(0)
    ))
  }

  any8k_ <- .master |>
    dplyr::filter(.data$Year %in% .pre_years) |>
    dplyr::summarise(n8K = sum(.data$nFilings), .by = "CIK")

  .status |>
    dplyr::filter(.data$Status != "Unclassified") |>
    dplyr::left_join(any8k_, by = dplyr::join_by(CIK)) |>
    dplyr::mutate(n8K = dplyr::coalesce(.data$n8K, 0L)) |>
    dplyr::summarise(
      nFirms         = dplyr::n(),
      nWithAny8K     = sum(.data$n8K > 0L),
      Median8K       = stats::median(.data$n8K),
      MeanContract8K = mean(.data$nPre8K),
      .by            = "Status"
    ) |>
    dplyr::mutate(ShareWithAny8K = .data$nWithAny8K / pmax(.data$nFirms, 1L)) |>
    dplyr::select("Status", "nFirms", "nWithAny8K", "ShareWithAny8K", "Median8K",
                  "MeanContract8K") |>
    dplyr::arrange(.data$Status)
}


# 5. Results -------------------------------------------------------------------------------------------------------------

#' Report the population table
#' @param .tab Output of itx_table_population().
#' @return .tab invisibly.
itx_report_population <- function(.tab) {
  if (FALSE) .tab <- tab_pop

  # THE YEAR IS HANDED OVER AS TEXT. tbl_out() formats a numeric column with a thousands separator,
  # which is right for a count and turns 2001 into "2,001". A year is a label rather than a quantity,
  # so it travels as one.
  tbl_head("Contract-bearing 8-Ks against every 8-K")
  .tab |>
    dplyr::mutate(Year = as.character(.data$Year)) |>
    tbl_out(
      .title = "8-K filings by year, retrieved against indexed",
      .pct   = "ShareRetrieved",
      .notes = c(
        nContract8K    = "8-K filings carrying at least one Exhibit 10. This is what has an item list.",
        nAll8K         = "Every 8-K EDGAR indexed. No item list exists for these.",
        ShareRetrieved = "How strong the condition is that every other table here is conditional on."
      )
    )

  pre_  <- dplyr::filter(.tab, .data$Year %in% 2001:2003)
  post_ <- dplyr::filter(.tab, .data$Year %in% 2005:2007)
  # Both windows sit inside the acquisition frame, so neither can pick up an out-of-frame zero.
  if (nrow(pre_) > 0L && nrow(post_) > 0L && !all(is.na(pre_$ShareRetrieved))) {
    cli::cli_alert_info(
      "Retrieved share {tbl_pct(mean(pre_$ShareRetrieved, na.rm = TRUE))} in 2001-2003 against \\
       {tbl_pct(mean(post_$ShareRetrieved, na.rm = TRUE))} in 2005-2007. The rise IS the treatment, \\
       so the pre-period rows are contract-disclosing firms and almost nobody else."
    )
  } else {
    cli::cli_alert_warning(
      "The unrestricted index was unavailable, so the retrieved share cannot be computed and every \\
       table below is conditional on retrieval without that condition being measured."
    )
  }
  invisible(.tab)
}


#' Report the item code distribution by era
#' @param .tab Output of itx_table_codes().
#' @return .tab invisibly.
itx_report_codes <- function(.tab) {
  if (FALSE) .tab <- tab_codes

  for (.e in levels(.tab$Era)) {
    part_ <- dplyr::filter(.tab, .data$Era == .e)
    if (nrow(part_) == 0L) next
    tbl_head("Item codes, {(.e)}-reform")
    part_ |>
      dplyr::mutate(ItemLabel = stringi::stri_sub(.data$ItemLabel, 1L, 52L)) |>
      dplyr::select("ItemCode", "ItemLabel", "nFilings", "Share") |>
      tbl_out(
        .title = paste0("Most common items on contract-bearing 8-Ks, ", .e, "-reform"),
        .pct   = "Share",
        .notes = c(Share = "Share of filings in the era carrying the item, so a column sums above one.")
      )
  }
  invisible(.tab)
}


#' Report the item-share series
#' @param .tab Output of itx_table_item_share().
#' @return .tab invisibly.
itx_report_item_share <- function(.tab) {
  if (FALSE) .tab <- tab_share

  pct_ <- grep("^Item_", names(.tab), value = TRUE)

  tbl_head("Which item carried contract disclosure")
  .tab |>
    dplyr::filter(.data$Year >= 2001L, .data$Year <= 2008L) |>
    dplyr::mutate(Year = as.character(.data$Year)) |>
    tbl_out(
      .title = "Share of contract-bearing 8-Ks carrying each item, by year",
      .pct   = pct_,
      .notes = c(
        Item_5 = "The pre-reform Other Events catch-all. Exact code match, so 5.02 is not counted here."
      )
    )
  invisible(.tab)
}


#' Report the redistribution
#' @param .tab Output of itx_table_shift().
#' @return .tab invisibly.
itx_report_shift <- function(.tab) {
  if (FALSE) .tab <- tab_shift

  tbl_head("What the reform moved")
  .tab |>
    dplyr::mutate(ItemLabel = stringi::stri_sub(.data$ItemLabel, 1L, 46L)) |>
    tbl_out(
      .title = "Items on contract-bearing 8-Ks, before and after the reform",
      .pct   = "Share",
      .groups = NULL,
      .notes  = c(
        Share = "Within era. Item 5 giving way to Item 1.01 is the reclassification the memo asserts."
      )
    )
  invisible(.tab)
}


#' Report the firm comparison
#' @param .tab Output of itx_table_firms().
#' @return .tab invisibly.
itx_report_firms <- function(.tab) {
  if (FALSE) .tab <- tab_firms

  if (nrow(.tab) == 0L) {
    cli::cli_alert_warning(
      "The unrestricted index was unavailable, so whether treated firms filed 8-Ks at all cannot be \\
       answered. Without it the comparison is definitional and says nothing."
    )
    return(invisible(.tab))
  }

  tbl_head("Treated and control firms before the reform")
  tbl_out(
    .tab   = .tab,
    .title = "Pre-reform 8-K practice by group",
    .pct   = "ShareWithAny8K",
    .notes = c(
      ShareWithAny8K = "Filed any 8-K at all, from the unrestricted index. Not a contract 8-K.",
      MeanContract8K = "Contract-bearing 8-Ks. Zero for treated firms BY DEFINITION, so it is not evidence.",
      Median8K       = "Total 8-Ks. This one is not definitional and is what the mechanism needs."
    )
  )

  cli::cli_alert_info(
    "The share filing ANY 8-K is the informative cell: a treated firm with an active 8-K practice \\
     that never carried a contract is what the reclassification story requires. A treated group that \\
     barely filed 8-Ks at all would mean something else entirely."
  )
  invisible(.tab)
}


# 6. Validation -----------------------------------------------------------------------------------------------------------

#' Did the parser lose or invent anything
#'
#' THREE IDENTITIES. Every filing with a non-empty item string yields at least one row; every row
#' yields a code that matches the taxonomy's shape; and the count of distinct filings coming out
#' equals the count going in. The second is the one that catches a separator mistake, because
#' splitting on the wrong character produces fragments that carry no "Item N:" prefix at all.
#'
#' @param .tab_land Landing rows fed to the parser.
#' @param .items Output of itx_parse_items().
#' @return Tibble of checks.
itx_check_parse <- function(.tab_land, .items) {
  if (FALSE) {
    .tab_land <- tab_land
    .items    <- tab_items
  }

  in_ <- dplyr::filter(.tab_land, !is.na(.data$Items) & nzchar(.data$Items))

  tibble::tibble(
    Check = c(
      "Filings in, filings out",
      "Rows with an unparsed code",
      "Codes matching the taxonomy shape",
      "Filings yielding no item at all"
    ),
    Value = c(
      dplyr::n_distinct(.items$HashIndex) - nrow(in_),
      sum(is.na(.items$ItemCode)),
      sum(grepl("^[0-9]+(\\.[0-9]+)?$", .items$ItemCode), na.rm = TRUE),
      length(setdiff(in_$HashIndex, .items$HashIndex))
    ),
    Want = c("0", "0", "= rows", "0")
  )
}


#' Report the parser checks
#' @param .tab Output of itx_check_parse().
#' @param .items Output of itx_parse_items().
#' @return .tab invisibly.
itx_report_checks <- function(.tab, .items) {
  if (FALSE) {
    .tab   <- tab_check
    .items <- tab_items
  }

  tbl_head("Did the parser lose or invent anything")
  tbl_out(.tab = .tab, .title = "Parser identities")

  bad_ <- .tab$Value[[2L]] > 0L || .tab$Value[[1L]] != 0L || .tab$Value[[4L]] > 0L
  if (bad_) {
    cli::cli_alert_danger("The parser is dropping or mangling items; nothing below is safe.")
    .items |>
      dplyr::filter(is.na(.data$ItemCode)) |>
      utils::head(10L) |>
      print()
  } else {
    cli::cli_alert_success(
      "{txt_n(nrow(.items))} items parsed from {txt_n(dplyr::n_distinct(.items$HashIndex))} filings, \\
       none unparsed."
    )
  }
  invisible(.tab)
}


# 7. Deployment ------------------------------------------------------------------------------------------------------------

#' Write the parsed item table and the firm classification
#'
#' THE ITEM TABLE IS THE REUSABLE ARTIFACT. Nothing else in the pipeline holds 8-K items in a parsed,
#' joinable form, and the next question about 8-K composition -- bundling, voluntary counts, anything
#' conditioned on what else the filing reported -- wants exactly this rather than the raw string.
#'
#' @param .items Output of itx_parse_items().
#' @param .land 8-K landing rows.
#' @param .status Output of itx_firm_status().
#' @param .paths Named list: Items, Firms.
#' @return Tibble of what was written, invisibly.
itx_write_release <- function(.items, .land, .status, .paths) {
  if (FALSE) {
    .items  <- tab_items
    .land   <- tab_land
    .status <- tab_status
    .paths  <- .lP$Output$Release
  }

  out_items_ <- .items |>
    dplyr::inner_join(
      dplyr::select(.land, "HashIndex", "CIK", "FilingDate", "Year", "Era", "FormType"),
      by = dplyr::join_by(HashIndex)
    ) |>
    dplyr::select("HashIndex", "CIK", "FilingDate", "Year", "Era", "FormType", "ItemCode",
                  "ItemLabel", "IsDotted")

  arrow::write_parquet(out_items_, .paths$Items)
  arrow::write_parquet(dplyr::mutate(.status, Status = as.character(.data$Status)), .paths$Firms)

  out_ <- tibble::tibble(
    File = c("filing_items", "firm_status"),
    Path = as.character(unlist(.paths[c("Items", "Firms")])),
    Rows = c(nrow(out_items_), nrow(.status))
  ) |>
    dplyr::mutate(
      MB   = round(as.numeric(fs::file_size(.data$Path)) / 1024^2, 1),
      Path = fs::path_file(.data$Path)
    )

  tbl_head("What was written")
  tbl_out(
    .tab   = out_,
    .title = "Released files",
    .notes = c(Rows = "filing_items is long: one row per item per filing.")
  )
  invisible(out_)
}


# 8. Plots ------------------------------------------------------------------------------------------------------------------

#' Contract-bearing 8-Ks against every 8-K
#' @param .tab Output of itx_table_population().
#' @param .year_min Integer. First year drawn.
#' @param .year_max Integer or NULL. Last year drawn; the acquisition frame closes here.
#' @return A ggplot.
itx_plot_population <- function(.tab, .year_min = 2001L, .year_max = NULL) {
  if (FALSE) {
    .tab      <- tab_pop
    .year_min <- 2001L
    .year_max <- 2024L
  }

  dat_ <- dplyr::filter(.tab, .data$Year >= .year_min, !is.na(.data$ShareRetrieved))
  if (!is.null(.year_max)) dat_ <- dplyr::filter(dat_, .data$Year <= .year_max)

  dat_ |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Year, y = .data$ShareRetrieved)) +
    ggplot2::geom_col(fill = plot_pal_seq(1L), width = 0.7) +
    ggplot2::geom_vline(xintercept = 2004.6, linetype = "dashed", linewidth = 0.4,
                        colour = plot_pal_grey(1L)) +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = "8-Ks carrying a material contract") +
    plot_theme(.grid = "y", .legend = "none")
}


#' Which item carried contract disclosure, over time
#' @param .tab Output of itx_table_item_share().
#' @param .year_min Integer. First year drawn.
#' @return A ggplot.
itx_plot_item_share <- function(.tab, .year_min = 2001L) {
  if (FALSE) {
    .tab      <- tab_share
    .year_min <- 2001L
  }

  .tab |>
    dplyr::filter(.data$Year >= .year_min) |>
    dplyr::select("Year", dplyr::starts_with("Item_")) |>
    tidyr::pivot_longer(-"Year", names_to = "Item", values_to = "Share") |>
    dplyr::mutate(Item = sub("^Item_", "Item ", .data$Item)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Year, y = .data$Share, colour = .data$Item)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = 2004.6, linetype = "dashed", linewidth = 0.4,
                        colour = plot_pal_grey(1L)) +
    plot_scale_colour_cat() +
    plot_scale_y_pct() +
    ggplot2::labs(x = NULL, y = "Contract-bearing 8-Ks carrying the item", colour = NULL) +
    plot_theme(.grid = "y", .legend = "bottom")
}


#' The redistribution, before against after
#' @param .tab Output of itx_table_shift().
#' @return A ggplot.
itx_plot_shift <- function(.tab) {
  if (FALSE) .tab <- tab_shift

  .tab |>
    dplyr::mutate(
      Item = paste0(.data$ItemCode, ": ", stringi::stri_sub(.data$ItemLabel, 1L, 28L)),
      Item = stats::reorder(.data$Item, .data$Share)
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Share, y = .data$Item)) +
    ggplot2::geom_col(fill = plot_pal_seq(1L)) +
    ggplot2::facet_wrap(~ .data$Era, ncol = 1L, scales = "free_y") +
    plot_scale_x_pct() +
    ggplot2::labs(x = "Share of contract-bearing 8-Ks in the era", y = NULL) +
    plot_theme(.grid = "x", .legend = "none")
}
