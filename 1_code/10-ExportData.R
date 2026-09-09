# 10-ExportData: one function per exported file ---------------------------------------------------------------------------
#
# WHAT THIS FILE DOES
# 04D released eight long files at span grain, 02B holds the register and 03F the labels. Eight
# functions turn them into eight CONTRACT-LEVEL CACHES, and a ninth joins those into the one file
# the Stata analysis reads. Three more release what that file cannot hold -- the places behind the
# geography counts, the term hits behind the pandemic panel, the orders behind HasCto -- as long files
# beside it, so 30-Descriptives runs off the release folder alone.
#
# ONE FUNCTION PER FILE, AND IT TAKES PATHS. Nothing is built in the runbook and handed in. Each
# export opens its own input, names the columns it reads at the call site, does its own work and
# writes one parquet, so running one chunk on its own does exactly what a full render does and what
# goes into a file is written in the function that writes it.
#
# THE SHAPE, THE SAME EIGHT TIMES
#   1. cached unless .rerun -- read the file back and return it
#   2. open the release as an arrow dataset, select only the columns this file reads
#   3. hand that to DuckDB and do EVERY aggregation there, lazily
#   4. collect once
#   5. reshape in R, because a pivot has no lazy form
#   6. write, and return the table so it can be inspected
#
# WHY DUCKDB AND NOT ARROW ALONE. org_mentions is 20.9 million rows and the work is a DISTINCT
# followed by two group-bys and a join. arrow can express those, but it materialises what it cannot
# push down; DuckDB streams from the arrow scan and spills to disk when a hash table outgrows memory.
# The projection still happens in arrow, which is why select() comes before to_duckdb().
#
# NO SQL. Every calculation is a dplyr verb against the registered table, so the same code reads the
# same way whether it runs in DuckDB, in arrow or on a tibble.
#
# THE CACHES ARE AT ATTACHMENT GRAIN AND THE FINAL FILE IS NOT. A collapse is keyed on the primary
# copy 04C extracted; a filing naming several registrants lists that one attachment under each of
# them. export_final() fans every entity cache out on HashDocument, so all 1.46 million registrant
# copies carry the values of their attachment and PrimaryFiler marks the one that was read. That is
# 03F's own decision, applied to the entity blocks as well.
#
# NOTHING OPENS A DOCUMENT. red_words() is never called with a text path; every word count comes from
# the register. The text seam 04A established holds through the export.
#
# House style: native pipe; explicit package::function; dot-prefixed args; underscore-suffixed
# locals; .data$ for existing columns, bare CamelCase for new columns; if (FALSE) dev blocks;
# cli/fs/here; pure ASCII; stringi never base substr; {(.arg)} parens in cli interpolation.

if (FALSE) {
  .dir_in   <- .lP$Input$DirEntityORG
  .path_out <- .lP$Cache$CacheEntityORG
}


# 0. Vocabulary and shared helpers ------------------------------------------------------------------------------------------

#: WHAT THE SAMPLE FILE CARRIES, and the one place to change it. These are the register's own facts
#: about a contract: identity, where it sits on EDGAR, what it is, how big, whether it is usable, who
#: filed it, which Compustat quarter it matched, where it sits on the sample ladder, and what 01D and
#: 01E found. Nothing here is an extraction result; those are the other seven files.
#:
#: Group, DocType and YQ are deliberately absent. Group is constant once the register is cut to one
#: document type, and the other two are components of a machine path that utils_doc_path() rebuilds.
#: export_sample() reports every register column this list leaves behind, so a column that belongs in
#: the release surfaces in that line rather than being missed for a year.
.exp_sample_cols <- c(
  # identity, and the two links back to EDGAR. Referee 2 asked for an overview file carrying "an
  # identifiable cik-accession-filename combination (rather than a random hash ID)" and "most
  # importantly, a link to the actual file location at EDGAR". DocID is the accession and the
  # accession opens with the CIK, so DocName is the word that was missing. UrlIndexPage is the
  # filing; UrlDocument is the one attachment, and a reader wants both.
  "DocID", "HashDocument", "HashIndex", "CIK", "CompanyName",
  "DocName", "DocSeq", "UrlDocument", "UrlIndexPage",
  # what the document is. DocDesc is the filer's own description of the exhibit, and the only
  # human-written label an attachment carries.
  "DocTypeRaw", "DocTypeMod", "DocDesc", "FormType", "DocExt", "DateFiled",
  # how big, and whether it is usable
  "nWords", "nWordsAdj", "nChars", "nNums", "pStopShort", "Removed", "RemClass",
  # whether it is one of several copies of one attachment
  "nCIK", "MultFiler", "FilerCopiesAgree", "PrimaryFiler",
  # who filed it, and which fiscal quarter it fell in
  "gvkey", "datadate", "cyear", "fyear", "fqtr",
  # which sample it is in
  "SampleStepCode", "SampleStepDesc", "DescSample", "EstiSample"
  # THE ITEM AND ORDER COLUMNS ARE NOT HERE, and they used to be. 02B carried both, which put a
  # filing-level fact and a firm-level one on a document row for no reason either the ladder or 03
  # and 04 needed: those read twelve named columns from the register and none of these is among
  # them. They are joined here instead, from 01D on HashIndex and from 01E on DocID, which is one
  # line each and puts them where the only consumer is. HasSummary and Item101Outcome stay in the
  # register because they are properties of an 8-K report document, and this file holds none.
)

#: What 03F's release carries beside its labels. These are the register's own columns and the sample
#: supplies them, so the classification cache drops them: a Stata merge of two files both defining a
#: variable fails rather than choosing.
.exp_label_drop <- c("HashDocument", "PrimaryFiler", "FilerCopiesAgree", "DescSample", "EstiSample")

#: The two roles the geography counts are split by. Every other role -- cofiler, signatory, other --
#: has an attached place often enough (a notary address, a guarantor's state) but no reading that an
#: analysis wants, so they are excluded by this filter rather than by a missing column. Declared here
#: because the pivot takes its shape from it.
.exp_geo_roles <- c("registrant", "counterparty")

#: The currency blocks 04B4 releases, against the suffix each becomes. "non-USD" is not a legal
#: column ending, so the mapping is declared rather than derived; the function checks it against the
#: registered vocabulary and aborts where a block appears that nothing here names, because a third
#: currency block silently dropped would leave a contract's largest figure out of the file.
.exp_money_blocks <- c("USD" = "USD", "non-USD" = "Other")

#: The clause classes lawregex.py emits: seven opening cues and the one label that outranks them.
#: Declared so a cue added upstream aborts here rather than arriving as a value nothing documents.
#: SelfReferential is not a cue -- classify() assigns it to a clause that names no place by
#: construction, "governed by the laws of the State in which the Premises are located".
.exp_law_kinds <- c(
  "GovernedByConstrued", "GovernedBy", "LawsOfState", "LawsOfCommonwealth",
  "ConstruedAccordance", "GoverningLaw", "SubmitJurisdiction", "SelfReferential"
)


#' Which release column becomes Class, ClassBroad and AmendType
#'
#' 03F CROWNS NOTHING, DELIBERATELY: its release carries every engine side by side because which one
#' is authoritative is a decision. 03B made that decision and this is where it is applied, once, so
#' that no downstream script has to know the winner's name.
#'
#' @param .engine Character. Label prefix in 03F's release, "Bert" or "Kw".
#' @return Named character: the crowned name against the release column it copies.
.exp_crowned <- function(.engine = "Bert") {
  if (FALSE) .engine <- "Bert"

  c(
    Class      = paste0(.engine, "ClassDetailed"),
    ClassBroad = paste0(.engine, "ClassBroad"),
    AmendType  = paste0(.engine, "AmendType")
  )
}


#' Is the cache still good, or has an input moved under it
#'
#' EXISTENCE IS NOT VALIDITY, and this document has already proved it. Rebuilding the sample with two
#' new columns left Contracts.parquet holding the old one: the join is cached on its own existence,
#' the row count did not change, and every identity check passed against a file that no longer
#' matched its own spine. Nothing in the render said so.
#'
#' MODIFICATION TIME IS THE CHEAP CORRECT TEST. An input newer than the output means the output was
#' built from something else. It costs one stat call per file and it catches the case a path cannot:
#' the 04D stores carry a policy hash, so a changed rule is a changed path and the cache simply is not
#' there -- but the register, 01C's removed list and 03F's release are all paths that stay the same
#' while their contents move.
#'
#' A DIRECTORY IS ITS NEWEST FILE. 04D's stores are trees of chunk directories, and a directory's own
#' timestamp says nothing about what is inside it.
#'
#' IT PRINTS ONLY WHEN IT SAYS NO, and it says which input moved. A cache hit is reported by the
#' caller, which is where the row count is.
#'
#' @param .task Character. Which export, for the message.
#' @param .path_out The cache this export writes.
#' @param .paths_in Character. Every file or directory it reads.
#' @param .rerun Logical. TRUE always rebuilds.
#' @return Logical. TRUE where the cache may be used as it stands.
exp_cache_hit <- function(.task, .path_out, .paths_in, .rerun = FALSE) {
  if (FALSE) {
    .task     <- "ORG"
    .path_out <- .lP$Cache$CacheEntityORG
    .paths_in <- .lP$Input$DirEntityORG
    .rerun    <- FALSE
  }

  if (.rerun) return(FALSE)
  if (!fs::file_exists(.path_out)) return(FALSE)

  out_ <- as.numeric(fs::file_info(.path_out)$modification_time)

  when_ <- purrr::map_dbl(as.character(.paths_in), function(.p) {
    if (fs::dir_exists(.p)) {
      info_ <- fs::dir_info(path = .p, recurse = TRUE, type = "file")
      return(if (nrow(info_) == 0L) -Inf else as.numeric(max(info_$modification_time)))
    }
    if (fs::file_exists(.p)) as.numeric(fs::file_info(.p)$modification_time) else -Inf
  })

  moved_ <- as.character(.paths_in)[when_ > out_]
  if (length(moved_) > 0L) {
    cli::cli_alert_warning(
      "{(.task)}: rebuilding. {length(moved_)} {cli::qty(length(moved_))}input{?s} {?is/are} newer \\
       than the cache: {paste(fs::path_file(fs::path_tidy(moved_)), collapse = ', ')}."
    )
    return(FALSE)
  }
  TRUE
}


#' Would two tables about to be joined collide on a name
#'
#' A COLLISION IS SILENT AND SURVIVES EVERY LATER CHECK. dplyr suffixes Column.x and Column.y without
#' a warning, the released file then carries two columns nobody named, and whoever finds them a year
#' later cannot tell which side each came from. Asking before the join names both.
#'
#' @param .a Character. Column names of the left side.
#' @param .b Character. Column names of the right side.
#' @param .by Character. Join keys, which are allowed to appear in both.
#' @param .what Character. What the two are, for the message.
#' @return Invisibly the colliding names, character(0) where there are none.
exp_check_collide <- function(.a, .b, .by, .what = "the join") {
  if (FALSE) {
    .a    <- names(tab_sample)
    .b    <- names(tab_EntityORG)
    .by   <- "DocID"
    .what <- "the ORG join"
  }

  hit_ <- setdiff(intersect(.a, .b), .by)
  if (length(hit_) > 0L) {
    cli::cli_abort(c(
      "{length(hit_)} {cli::qty(length(hit_))}column{?s} would collide in {(.what)}.",
      "x" = "{paste(hit_, collapse = ', ')}.",
      "i" = "dplyr suffixes these silently. Decide which side owns each and drop it from the other."
    ))
  }
  invisible(hit_)
}


#' Make a tibble something haven can write
#'
#' FOUR THINGS STATA CANNOT TAKE, and every one is silent or fatal at write time rather than visible
#' now. haven refuses a logical column outright. A name over 32 characters, or carrying anything but
#' letters, digits and underscores, is not a legal Stata identifier. A factor arrives as its integer
#' codes unless it is made character first. And a string over 2,045 characters exceeds str2045, the
#' widest fixed string a dta holds, so haven errors and takes the render with it.
#'
#' THE TRUNCATION IS THE DTA'S ALONE. The parquet is written before this runs and keeps the full
#' value, so nothing is lost by the export -- only by the format that cannot hold it. It is reported
#' rather than done quietly, because a truncated URL that still looks like a URL goes unnoticed.
#'
#' @param .tab Any tibble bound for Stata.
#' @param .max_chars Integer. Widest string a dta column may hold.
#' @return A list: Tab, the Stata-ready tibble; Names, the mapping; Changed, how many were renamed;
#'   Cut, the columns that had to be truncated.
exp_stata_ready <- function(.tab, .max_chars = 2045L) {
  if (FALSE) {
    .tab       <- tab_Contracts
    .max_chars <- 2045L
  }

  safe_ <- function(.x) {
    out_ <- stringi::stri_replace_all_regex(.x, "[^A-Za-z0-9_]", "_")
    out_ <- stringi::stri_replace_all_regex(out_, "^([0-9])", "v$1")
    stringi::stri_sub(str = out_, from = 1L, to = 32L)
  }

  new_ <- safe_(names(.tab))
  # A truncation can collide where two names agree on their first 32 characters. Suffixing the
  # duplicates is ugly and traceable; leaving them would make haven overwrite one with the other.
  dup_ <- duplicated(new_)
  if (any(dup_)) new_[dup_] <- paste0(stringi::stri_sub(new_[dup_], 1L, 29L), "_", which(dup_))

  chr_ <- names(.tab)[purrr::map_lgl(.tab, is.character)]
  cut_ <- chr_[purrr::map_lgl(chr_, \(.c) {
    max(stringi::stri_length(.tab[[.c]]), na.rm = TRUE) > .max_chars
  })]

  out_ <- .tab |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.logical), \(.x) as.integer(.x)),
      dplyr::across(dplyr::where(is.factor),  \(.x) as.character(.x)),
      dplyr::across(
        dplyr::all_of(cut_), \(.x) stringi::stri_sub(str = .x, from = 1L, to = .max_chars)
      )
    ) |>
    stats::setNames(new_)

  list(Tab = out_, Names = tibble::tibble(Parquet = names(.tab), Stata = new_),
       Changed = sum(names(.tab) != new_), Cut = cut_)
}


# 1. The sample -------------------------------------------------------------------------------------------------------------

#' The sample: the spine every other file matches on DocID
#'
#' THE REGISTER, RESTRICTED AND SELECTED. Three things happen and each is visible: the register is
#' cut to one document type, the columns worth exporting are named, and DateFiled is made a Date.
#'
#' NOTHING IS FILTERED OUT. 02B removes no document ever, and a released sample that quietly excluded
#' the malformed ones would make every later study unable to see what was lost. Removed says which
#' they are, RemClass says WHY -- too short, not prose, mostly digits -- SampleStepCode says where
#' each sits on the ladder, and UrlDocument is the link back to EDGAR that referee 2 asked for.
#'
#' RemClass IS JOINED FROM 01C, because the register does not carry it: reg_shape() relocates the
#' name without ever having joined the column. Referee 1 asked whether the malformed contracts are
#' noted anywhere in the data, and without this they are flagged but unexplained.
#'
#' ONE ROW PER REGISTRANT COPY, which is what the register already is. PrimaryFiler marks the copy
#' 04C extracted, so a reader wanting one row per attachment filters on it.
#'
#' DateFiled IS COERCED HERE AND NOWHERE ELSE. dte_collapse() does date arithmetic on it, and one
#' coercion in one place is what stops two exports disagreeing about a type.
#'
#' .exp_sample_cols IS THE ONE PLACE TO CHANGE WHAT IS RELEASED. What it takes and what it leaves is
#' audited by exp_table_columns() against the register, on every render rather than only on the one
#' where this file was rebuilt.
#'
#' @param .path_in 02B's Documents.parquet.
#' @param .path_removed 01C's RemovedDocs.parquet; supplies RemClass.
#' @param .path_out Destination parquet.
#' @param .doc_type Character. Which Group of the register to export.
#' @param .rerun Logical. FALSE returns the file where it already exists.
#' @return Invisibly the written table: one row per registrant copy.
export_sample <- function(.path_in, .path_removed, .path_out, .doc_type = "Exhibit10",
                          .rerun = FALSE) {
  if (FALSE) {
    .path_in      <- .lP$Input$FilSample
    .path_removed <- .lP$Input$FilRemoved
    .path_out     <- .lP$Cache$CacheSample
    .doc_type     <- "Exhibit10"
    .rerun        <- FALSE
  }

  if (exp_cache_hit(.task = "Sample", .path_out = .path_out, .paths_in = c(.path_in, .path_removed),
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "Sample: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  reg_ <- arrow::open_dataset(sources = .path_in) |>
    dplyr::filter(.data$Group == .doc_type) |>
    dplyr::collect()

  # WHICH COLUMNS WERE TAKEN AND WHICH LEFT IS exp_table_columns()'S TO SAY, not this function's. A
  # message printed at write time appears on a cold render and vanishes on a warm one; the audit
  # belongs in the rendered document either way.
  keep_ <- .exp_sample_cols[.exp_sample_cols %in% names(reg_)]

  # RemClass COMES FROM 01C AND NOT FROM THE REGISTER. 02B's reg_shape() relocates a column it never
  # joined, so Removed says a document was flagged and nothing in the register says why. Referee 1
  # asked exactly that -- "are these malformed contracts noted anywhere in the data" -- and the answer
  # is one join away, in the file 01C wrote beside the metadata.
  rem_ <- arrow::open_dataset(sources = .path_removed) |>
    dplyr::select("DocID", "RemClass") |>
    dplyr::collect() |>
    dplyr::distinct(.data$DocID, .keep_all = TRUE)

  out_ <- reg_ |>
    dplyr::select(dplyr::all_of(keep_)) |>
    dplyr::left_join(rem_, by = dplyr::join_by(DocID), relationship = "many-to-one") |>
    dplyr::relocate("RemClass", .after = "Removed") |>
    dplyr::mutate(DateFiled = anytime::anydate(as.character(.data$DateFiled))) |>
    dplyr::arrange(.data$DocID)

  n_rem_ <- sum(as.logical(out_$Removed), na.rm = TRUE)
  n_why_ <- sum(!is.na(out_$RemClass))
  if (n_rem_ != n_why_) {
    cli::cli_alert_warning(
      "{format(n_rem_, big.mark = ',')} {cli::qty(n_rem_)}row{?s} {?is/are} flagged Removed but \
       {format(n_why_, big.mark = ',')} carr{?ies/y} a reason. The two come from different files."
    )
  }

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)

  cli::cli_alert_success(
    "Sample -> {.path {as.character(fs::path_file(.path_out))}}: \\
     {format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns, \\
     {format(sum(as.logical(out_$PrimaryFiler)), big.mark = ',')} of them primary."
  )

  invisible(out_)
}


# 2. The classification -----------------------------------------------------------------------------------------------------

#' The classification: 03F's release, as it wrote it
#'
#' BOTH ENGINES, ALL THREE TASKS, every probability and every agreement flag. Five columns are dropped
#' -- HashDocument, PrimaryFiler, FilerCopiesAgree, DescSample, EstiSample -- because they are the
#' register's own and the sample carries them; a Stata merge of two files both defining a variable
#' fails rather than choosing.
#'
#' THREE COLUMNS ARE ADDED AND NOT RENAMED. Class, ClassBroad and AmendType are copies of the crowned
#' engine's, so no downstream script has to know which engine won; the originals stay because the
#' agreement flags refer to them by name.
#'
#' NO FAN-OUT AND NO SAMPLE. 03F's release is ALREADY one row per registrant copy -- it fans out on
#' HashDocument before it writes -- so a co-filer copy carries the label computed from its
#' attachment's primary and nothing here has to reach for the register to find that out. That is why
#' this export depends on no other file.
#'
#' @param .path_in 03F's release parquet.
#' @param .path_out Destination parquet.
#' @param .engine Character. Which engine's labels become Class, ClassBroad and AmendType.
#' @param .rerun Logical. FALSE returns the file where it already exists.
#' @return Invisibly the written table: one row per registrant copy.
export_classification <- function(.path_in, .path_out, .engine = "Bert", .rerun = FALSE) {
  if (FALSE) {
    .path_in  <- .lP$Input$FilClassification
    .path_out <- .lP$Cache$CacheClassification
    .engine   <- "Bert"
    .rerun    <- FALSE
  }

  if (exp_cache_hit(.task = "Classification", .path_out = .path_out, .paths_in = .path_in,
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "Classification: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  lab_ <- arrow::open_dataset(sources = .path_in) |>
    dplyr::collect() |>
    dplyr::select(-dplyr::any_of(.exp_label_drop)) |>
    dplyr::distinct(.data$DocID, .keep_all = TRUE)

  got_ <- .exp_crowned(.engine = .engine)
  got_ <- got_[got_ %in% names(lab_)]

  if (length(got_) == 0L) {
    cli::cli_abort(c(
      "The release carries no {(.engine)} label columns.",
      "i" = "It has: {paste(names(lab_), collapse = ', ')}."
    ))
  }

  # A LOOP AND NOT A mutate() OVER A MAPPED LIST, because mutate() does not take an unnamed list of
  # vectors and would fail at the first column.
  for (nm_ in names(got_)) lab_[[nm_]] <- lab_[[unname(got_[[nm_]])]]

  out_ <- dplyr::arrange(lab_, .data$DocID)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)

  cli::cli_alert_success(
    "Classification -> {.path {as.character(fs::path_file(.path_out))}}: \\
     {format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns; \\
     {paste(names(got_), collapse = ', ')} copied from {(.engine)}."
  )

  invisible(out_)
}


# 3b. Items and orders -------------------------------------------------------------------------------------------------

#' The parent filing's 8-K items, one row per filing
#'
#' A RESTRICTION AND A SELECT, NOT A COMPUTATION. 01D already collapsed its long item table to one
#' row per filing, so nothing here derives anything: the era rule, the voluntary count and the
#' indicators are all its. What this adds is the cut to the filings this sample actually touches --
#' 01D covers every filing EDGAR indexed, which is four times what the contracts reach, and a cache
#' larger than anything reading it is a cache nobody trusts.
#'
#' FilingDate IS DROPPED. 01D reads it off the landing page; the register carries DateFiled from the
#' index. They agree, and two dates for one filing in one file is a question waiting to be asked.
#'
#' THE JOIN KEY IS HashIndex, WHICH IS NEW HERE. Every other cache is keyed on a document, either
#' directly or through its attachment. This one is keyed on the filing a document arrived in, so it
#' is many-to-one against the spine and a contract shares its counts with every other exhibit of the
#' same 8-K. That is correct and worth stating: the items are a property of the filing, not of the
#' contract, and two contracts filed together necessarily carry the same ones.
#'
#' @param .path_in 01D's FilingItemFlags.parquet, one row per filing.
#' @param .path_sample The sample cache, read for the HashIndex values to keep.
#' @param .path_out Destination parquet.
#' @param .rerun Logical. TRUE rebuilds regardless of the cache check.
#' @return The written table, invisibly.
export_items <- function(.path_in, .path_sample, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .path_in     <- .lP$Input$FilItemFlags
    .path_sample <- .lP$Cache$CacheSample
    .path_out    <- .lP$Cache$CacheItems
    .rerun       <- FALSE
  }

  if (exp_cache_hit(.task = "Items", .path_out = .path_out,
                    .paths_in = c(.path_in, .path_sample), .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "Items: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  keep_ <- arrow::open_dataset(sources = .path_sample) |>
    dplyr::select("HashIndex") |>
    dplyr::collect() |>
    dplyr::distinct(.data$HashIndex) |>
    dplyr::pull(.data$HashIndex)

  out_ <- arrow::open_dataset(sources = .path_in) |>
    dplyr::filter(.data$HashIndex %in% keep_) |>
    dplyr::select(-dplyr::any_of("FilingDate")) |>
    dplyr::collect() |>
    dplyr::arrange(.data$HashIndex)

  if (anyDuplicated(out_$HashIndex)) {
    cli::cli_abort("01D's per-filing table is not one row per filing; the join would multiply the sample.")
  }

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)
  cli::cli_alert_success(
    "Items: {format(nrow(out_), big.mark = ',')} of {format(length(keep_), big.mark = ',')} \\
     {cli::qty(length(keep_))}filing{?s} the sample reaches."
  )

  invisible(out_)
}

#' The Item 1.01 summary measures, one row per filing
#'
#' READS 01D'S PUBLISHED TABLE AND KEEPS THE NUMBERS, NOT THE TEXT. Item101.parquet carries a
#' gigabyte of narrative; the measures Table 7 needs are ten numbers beside it. Only those travel,
#' keyed on HashIndex like the items block, and only for summaries 01D recovered.
#'
#' ONE SUMMARY PER FILING. 01D attempts every 8-K document in a filing, and a filing can carry more
#' than one -- an HTML body beside its plain-text rendering, or a body beside an amendment -- so
#' its table is one row per DOCUMENT. Where two documents in one filing both yielded a summary, the
#' cleaner outcome wins, then the longer text, then the lower DocID for determinism. The count of
#' filings this collapsed is reported, because a large number would mean 01D's candidates are wider
#' than one summary per filing in a way worth knowing about.
#'
#' THE ATTACHED FLAG IS ONE ON EVERY ROW HERE AND IS KEPT ANYWAY. Every row of the release is an
#' Exhibit 10, so a summary that reaches it was attached by construction; the delayed 8-Ks have no
#' contract row to land on. It travels so the block can be joined back to 01D's full population,
#' where it is the split.
#'
#' @param .path_in Character. 01D's Item101.parquet.
#' @param .path_sample Character. The sample cache, read for its HashIndex values.
#' @param .path_out Character. Where to write.
#' @param .rerun Logical.
#' @return Tibble, one row per filing with a recovered summary that the sample reaches.
export_summary <- function(.path_in, .path_sample, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .path_in     <- .lP$Input$FilItem101
    .path_sample <- .lP$Cache$CacheSample
    .path_out    <- .lP$Cache$CacheSummary
    .rerun       <- FALSE
  }
  if (exp_cache_hit(.task = "Summary", .path_out = .path_out,
                    .paths_in = c(.path_in, .path_sample), .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "Summary: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }
  keep_ <- arrow::open_dataset(sources = .path_sample) |>
    dplyr::select("HashIndex") |>
    dplyr::collect() |>
    dplyr::distinct(.data$HashIndex) |>
    dplyr::pull(.data$HashIndex)
  cols_ <- c("HashIndex", "DocID", "Outcome", "nSummaryDates", "SummaryIsSingle", "nWords", "nSentences",
             "nComplex", "FogIndex", "nUncertain", "nNumbers", "nDollars", "nPercents", "HasExhibit10",
             "BoilerplateShare", "AnnounceLagDays")
  raw_ <- arrow::open_dataset(sources = .path_in) |>
    dplyr::filter(.data$HashIndex %in% keep_, .data$Outcome %in% c("extracted", "ambiguous-longest")) |>
    dplyr::select(dplyr::all_of(cols_)) |>
    dplyr::collect()
  n_multi_ <- sum(duplicated(raw_$HashIndex))
  out_ <- raw_ |>
    dplyr::arrange(.data$HashIndex, .data$Outcome != "extracted", dplyr::desc(.data$nWords), .data$DocID) |>
    dplyr::distinct(.data$HashIndex, .keep_all = TRUE) |>
    dplyr::select(-"DocID", -"Outcome") |>
    dplyr::rename(
      SumDates = "nSummaryDates", SumIsSingle = "SummaryIsSingle", SumWords = "nWords",
      SumSentences = "nSentences", SumComplex = "nComplex", SumFog = "FogIndex",
      SumUncertain = "nUncertain", SumNumbers = "nNumbers", SumDollars = "nDollars",
      SumPercents = "nPercents", SumAttached = "HasExhibit10", SumBoiler = "BoilerplateShare",
      SumLagDays = "AnnounceLagDays"
    ) |>
    dplyr::arrange(.data$HashIndex)
  if (anyDuplicated(out_$HashIndex)) {
    cli::cli_abort("01D's summary table is not one row per filing; the join would multiply the sample.")
  }
  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)
  cli::cli_alert_success(
    "Summary: {format(nrow(out_), big.mark = ',')} of {format(length(keep_), big.mark = ',')} \\
     {cli::qty(length(keep_))}filing{?s} the sample reaches carry a recovered summary."
  )
  if (n_multi_ > 0L) {
    cli::cli_alert_info(
      "{format(n_multi_, big.mark = ',')} {cli::qty(n_multi_)}filing{?s} held more than one summarised \\
       document; one was kept per filing."
    )
  }
  invisible(out_)
}

# THE SECOND RELEASE FILE'S DICTIONARY. Summaries.dta is one row per Item 1.01 8-K, attached or
# not, and is what Tables 3 and 5 of the paper are estimated on. Same layout as .exp_dictionary so
# the two codebooks read alike.
.exp_dictionary_summaries <- tibble::tribble(
  ~Column, ~Meaning, ~Missing,
  "HashIndex", "The filing. Joins to Contracts on HashIndex where a contract was attached.", "never",
  "DocID", "The 8-K document the summary was read from.", "never",
  "CIK", "The filer. The key to Compustat, with DateFiled, as her 102 always did it.", "never",
  "DateFiled", "The 8-K's filing date.", "never",
  "FormType", "8-K or 8-K/A.", "never",
  "SumAttached",
  "Whether an Exhibit 10 rode on this 8-K. Zero is the delayed-or-omitted half of every announcement,
   which no contract-level file can see.", "never",
  "SumDates", "Distinct agreement dates the narrative opens with.", "structural",
  "SumDateFirst", "The first of them: the agreement date.", "where the narrative names none",
  "SumIsSingle", "SumDates == 1. The paper's Table 7 sample.", "structural",
  "SumLagDays",
  "DateFiled less SumDateFirst, calendar days, signed. Four business days is at most six calendar.",
  "where SumDateFirst is",
  "SumWords", "Words after the heading is removed.", "never",
  "SumSentences", "ICU sentence boundaries.", "never",
  "SumComplex", "Words of three or more syllables.", "never",
  "SumFog", "Gunning Fog.", "never",
  "SumUncertain", "Loughran-McDonald Uncertainty words.", "never",
  "SumNumbers", "Numeric tokens.", "never",
  "SumDollars", "Dollar figures.", "never",
  "SumPercents", "Percentages.", "never",
  "SumBoiler", "Share of four-word phrases in at least one in a hundred distinct filers.", "never"
)

#' The summaries as their own release: one row per Item 1.01 8-K, attached or not
#'
#' WHY A SECOND FILE. Contracts.dta is one row per Exhibit 10. An 8-K that announced an agreement
#' and attached nothing has no row there, and that is the half of every summary comparison the paper
#' makes: delayed against attached, late against on time. Those tables need every announcement as a
#' row, and this file is that. It is 01D's Item101.parquet with the narrative left behind, one row
#' per filing, and the names the Contracts block uses so a column means the same thing in both.
#'
#' NO SAMPLE LADDER. The ladder is a property of exhibits; an announcement is in this file if 01D
#' recovered its narrative, whether or not any exhibit followed. Restricting to single-agreement
#' announcements (SumIsSingle) is the paper's choice and is left to the paper.
#'
#' @param .path_in Character. 01D's Item101.parquet.
#' @param .path_out Character. Where to write the parquet; the dta and codebook sit beside it.
#' @param .rerun Logical.
#' @param .dta Logical. Also write Summaries.dta and Summaries_Codebook.csv.
#' @return Tibble, one row per filing with a recovered summary.
export_summaries <- function(.path_in, .path_out, .rerun = FALSE, .dta = TRUE) {
  if (FALSE) {
    .path_in  <- .lP$Input$FilItem101
    .path_out <- .lP$Output$FilSummaries
    .rerun    <- FALSE
    .dta      <- TRUE
  }
  if (exp_cache_hit(.task = "Summaries", .path_out = .path_out, .paths_in = .path_in, .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "Summaries: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }
  cols_ <- c("HashIndex", "DocID", "CIK", "DateFiled", "FormType", "Outcome", "HasExhibit10",
             "nSummaryDates", "SummaryDateFirst", "SummaryIsSingle", "AnnounceLagDays", "nWords",
             "nSentences", "nComplex", "FogIndex", "nUncertain", "nNumbers", "nDollars", "nPercents",
             "BoilerplateShare")
  raw_ <- arrow::open_dataset(sources = .path_in) |>
    dplyr::filter(.data$Outcome %in% c("extracted", "ambiguous-longest")) |>
    dplyr::select(dplyr::all_of(cols_)) |>
    dplyr::collect()
  n_multi_ <- sum(duplicated(raw_$HashIndex))
  out_ <- raw_ |>
    dplyr::arrange(.data$HashIndex, .data$Outcome != "extracted", dplyr::desc(.data$nWords), .data$DocID) |>
    dplyr::distinct(.data$HashIndex, .keep_all = TRUE) |>
    dplyr::select(-"Outcome") |>
    dplyr::rename(
      SumAttached = "HasExhibit10", SumDates = "nSummaryDates", SumDateFirst = "SummaryDateFirst",
      SumIsSingle = "SummaryIsSingle", SumLagDays = "AnnounceLagDays", SumWords = "nWords",
      SumSentences = "nSentences", SumComplex = "nComplex", SumFog = "FogIndex",
      SumUncertain = "nUncertain", SumNumbers = "nNumbers", SumDollars = "nDollars",
      SumPercents = "nPercents", SumBoiler = "BoilerplateShare"
    ) |>
    dplyr::arrange(.data$HashIndex)
  if (anyDuplicated(out_$HashIndex)) {
    cli::cli_abort("The summaries file is not one row per filing after the collapse.")
  }
  missing_ <- setdiff(names(out_), .exp_dictionary_summaries$Column)
  if (length(missing_) > 0L) {
    cli::cli_abort("Summaries.parquet would carry undocumented {cli::qty(length(missing_))}column{?s}: {.val {missing_}}")
  }

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)
  if (.dta) {
    sta_ <- exp_stata_ready(.tab = out_)
    haven::write_dta(sta_$Tab, fs::path_ext_set(.path_out, "dta"))
    tibble::tibble(Column = names(out_), StataName = sta_$Names$Stata) |>
      dplyr::left_join(.exp_dictionary_summaries, by = dplyr::join_by(Column)) |>
      readr::write_csv(fs::path_ext_set(paste0(fs::path_ext_remove(.path_out), "_Codebook"), "csv"))
  }
  n_att_ <- sum(out_$SumAttached)
  cli::cli_alert_success(
    "Summaries: {format(nrow(out_), big.mark = ',')} announcements, {format(n_att_, big.mark = ',')} with a contract \\
     attached, {format(nrow(out_) - n_att_, big.mark = ',')} without."
  )
  if (n_multi_ > 0L) {
    cli::cli_alert_info(
      "{format(n_multi_, big.mark = ',')} {cli::qty(n_multi_)}filing{?s} held more than one summarised \\
       document; one was kept per filing."
    )
  }
  invisible(out_)
}

#' Put the release where Ann-Kristin reads it, and keep what was there
#'
#' THE PREVIOUS RELEASE IS ARCHIVED BEFORE IT IS OVERWRITTEN, under a timestamp, so a table she ran
#' last month can be traced to the file it ran on. Nothing is archived when nothing changed: a file
#' in Dropbox that is the same size as the one here and no older than it is left alone, and if every
#' file is, the function reports that and stops. 7-Zip is used when it is on the PATH, because a
#' 3 GB .dta shrinks by a factor of ten under it; zip is the fallback.
#'
#' ONLY THE RELEASE FILES ARE TOUCHED. Sub-directories in the Dropbox folder -- _archive, anything
#' she keeps beside the release -- are hers and are not read or written.
#'
#' @param .dir_out Character. This document's Output directory.
#' @param .dir_dropbox Character. The folder she reads from.
#' @param .files Character. File names to deploy, all expected in .dir_out.
#' @return Invisibly, a tibble of what was done per file.
export_deploy <- function(.dir_out, .dir_dropbox, .files) {
  if (FALSE) {
    .dir_out     <- fs::path_dir(.lP$Output$FilContracts)
    .dir_dropbox <- .lP$Output$DirDropbox
    .files       <- c("Contracts.parquet", "Contracts.dta", "Contracts_Codebook.csv",
                      "Summaries.parquet", "Summaries.dta", "Summaries_Codebook.csv")
  }
  if (!fs::dir_exists(.dir_dropbox)) {
    cli::cli_alert_warning("Dropbox folder not found at {.path {(.dir_dropbox)}}; nothing deployed.")
    return(invisible(NULL))
  }
  src_ <- fs::path(.dir_out, .files)
  dst_ <- fs::path(.dir_dropbox, .files)
  if (!all(fs::file_exists(src_))) {
    cli::cli_abort("Not every release file is in Output: {.file {(.files[!fs::file_exists(src_)])}}")
  }
  info_src_ <- fs::file_info(src_)
  info_dst_ <- fs::file_info(dst_)
  same_ <- !is.na(info_dst_$size) & info_dst_$size == info_src_$size &
    info_dst_$modification_time >= info_src_$modification_time
  if (all(same_)) {
    cli::cli_alert_info("Dropbox already holds this release; nothing archived, nothing copied.")
    return(invisible(tibble::tibble(File = .files, Action = "unchanged")))
  }

  # Archive whatever release files are there now, before any of them is overwritten.
  present_ <- dst_[fs::file_exists(dst_)]
  if (length(present_) > 0L) {
    dir_arch_ <- fs::path(.dir_dropbox, "_archive")
    fs::dir_create(dir_arch_)
    stamp_ <- format(Sys.time(), "%Y-%m-%d_%H%M")
    seven_ <- Sys.which(c("7zz", "7z", "7za"))
    seven_ <- seven_[nzchar(seven_)][1]
    if (!is.na(seven_)) {
      arch_ <- fs::path(dir_arch_, paste0(stamp_, ".7z"))
      system2(seven_, c("a", "-mx=5", shQuote(arch_), shQuote(present_)), stdout = FALSE, stderr = FALSE)
    } else {
      arch_ <- fs::path(dir_arch_, paste0(stamp_, ".zip"))
      utils::zip(zipfile = arch_, files = present_, flags = "-j -q")
    }
    cli::cli_alert_success(
      "Archived {length(present_)} previous {cli::qty(length(present_))}file{?s} to {.file {fs::path_file(arch_)}}"
    )
  }

  fs::file_copy(src_[!same_], dst_[!same_], overwrite = TRUE)
  cli::cli_alert_success(
    "Copied {sum(!same_)} {cli::qty(sum(!same_))}file{?s} to Dropbox: {.file {(.files[!same_])}}"
  )
  invisible(tibble::tibble(File = .files, Action = ifelse(same_, "unchanged", "copied")))
}

#' The confidential-treatment orders naming each contract, one row per contract
#'
#' THE ONLY CACHE HERE THAT COLLAPSES ANYTHING. 01E publishes one row per order REFERENCE: an order
#' can name several exhibits and a contract can be named by several orders, most often because a
#' grant was later extended. This is the collapse 02B used to do, moved rather than rewritten, and a
#' probe confirmed the two agree on every row of the previous release before it moved.
#'
#' BOTH RELEASE DATES ARE KEPT. Where two orders cover one contract they rarely lapse together, and
#' which one matters depends on the question: the earliest is when any part becomes releasable, the
#' latest when all of it does.
#'
#' ONLY GRANTS COUNT, AND THE DEFINITION IS APPLIED HERE. 01E links every order whatever its outcome,
#' so that the ten denials and one revocation in the corpus can be counted; the paper's variable is
#' the permission to redact, and a denial is its opposite. A contract is covered where an order
#' GRANTED treatment. Extensions are grants of more time and stay; the four linked references from
#' denied orders -- three contracts -- are set aside, and the count of what was set aside is reported.
#'
#' THE GUARD ON THE DATES IS LOAD-BEARING. min() over a set of all-missing release dates returns Inf,
#' which lands in a date column as a number that looks like data rather than as an absence. 01E
#' parses no date for some orders, and sets aside a release date thirty years past its order as a
#' typo in the letter, so the case is real rather than defensive.
#'
#' TWO PROPERTIES OF 01E'S LINKAGE TRAVEL WITH THIS TABLE, and neither is introduced here. A
#' reference links only where exactly one Exhibit 10 in the filing carries its exhibit number, which
#' costs 44 references out of 32,410. And an attachment filed in several filings is covered in the
#' one the order names and not in the others -- 148 attachments split that way at the last count,
#' every one of them across different filings, which is what an order granting relief for a
#' particular filing means.
#'
#' @param .path_in 01E's CtoExhibits.parquet, one row per order reference.
#' @param .path_out Destination parquet.
#' @param .rerun Logical. TRUE rebuilds regardless of the cache check.
#' @return The written table, invisibly.
export_cto <- function(.path_in, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .path_in  <- .lP$Input$FilCtoExhibits
    .path_out <- .lP$Cache$CacheCto
    .rerun    <- FALSE
  }

  if (exp_cache_hit(.task = "Orders", .path_out = .path_out, .paths_in = .path_in,
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "Orders: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  lnk_ <- arrow::open_dataset(sources = .path_in) |>
    dplyr::filter(!is.na(.data$DocIDContract)) |>
    dplyr::select("DocIDContract", "Status", "ReleaseDate", "IsExtension") |>
    dplyr::collect()

  # The paper's definition: covered means granted. Status is read from the order's title line in
  # 01E, so anything other than GRANTING is a denial, a revocation, or an order whose text was empty.
  ref_   <- dplyr::filter(lnk_, .data$Status %in% "GRANTING")
  n_out_ <- nrow(lnk_) - nrow(ref_)

  out_ <- ref_ |>
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
    ) |>
    dplyr::rename(DocID = "DocIDContract") |>
    # THE DATES COME FIRST BECAUSE THE MARKER IS THE FIRST NON-KEY COLUMN. nCtoOrders is coalesced to
    # zero at the join, so in the released file it is never missing and would report every contract
    # as carrying this block. CtoReleaseFirst is where the absence actually shows, which is what
    # .exp_blocks declares, and ordering the cache to agree keeps one rule rather than two.
    dplyr::relocate("DocID", "CtoReleaseFirst", "CtoReleaseLast", "nCtoOrders", "CtoIsExtension") |>
    dplyr::arrange(.data$DocID)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)
  cli::cli_alert_success(
    "Orders: {format(nrow(out_), big.mark = ',')} contract{?s} named by \\
     {format(nrow(ref_), big.mark = ',')} granting reference{?s}; {n_out_} linked \\
     {cli::qty(n_out_)}reference{?s} from orders that did not grant set aside."
  )

  invisible(out_)
}

# 3. Parties ----------------------------------------------------------------------------------------------------------------

#' Parties: how many of each kind a contract names
#'
#' ONE ROW PER CONTRACT, and the population is whatever DocIDs org_mentions holds. That is every
#' contract 04D processed, because 04B1 writes a sentinel row for a contract in which no organisation
#' was found -- so a document reaching this file with every count at zero is a measurement rather than
#' a gap, and HasNoParty is what says so.
#'
#' THE GRAIN IS THE ATTACHMENT, which is the primary copy 04C extracted. The primary is itself one of
#' the registrant copies, so these DocIDs are a subset of the sample's and the two merge one to one;
#' a co-filer copy the extraction never reached comes back unmatched, which is correct.
#'
#' nUniSpellingsNaive IS THE RUNG ABOVE THE COUNTS. It is the distinct names the extractor proposed
#' BEFORE grouping merged them into parties -- 8.18 per contract at corpus scale against 7.28 parties
#' -- so a reader who rejects the grouping takes this and recomputes. It cannot be derived from the
#' five counts, which is why it is stored; the other two rungs can be, and are not. NULL keys are
#' excluded by COUNT(DISTINCT), so the sentinel contributes zero.
#'
#' THE SIX ROLE COLUMNS COME FROM THE VOCABULARY AND NOT FROM THE DATA. names_expand = TRUE over a
#' registered factor is what makes the shape a property of the taxonomy: a role no contract in a
#' subset happens to carry still gets its column, so a test run and a corpus run produce the same
#' file. A pivot named from data does not, which is the defect 04B4 and 04B5 both hit.
#'
#' DISTINCT-THEN-COUNT, NOT n_distinct(PartyKey). The sentinel row carries a null PartyKey, and
#' COUNT(DISTINCT) would score it zero where DISTINCT-then-count scores it one. The second is what
#' makes "this contract has no party" visible rather than indistinguishable from a missing row.
#'
#' @param .dir_in 04D's release directory for the ORG pass, including its policy hash.
#' @param .path_out Destination parquet.
#' @param .rerun Logical. FALSE returns the file where it already exists.
#' @return Invisibly the written table: one row per contract, eight columns.
export_ORG <- function(.dir_in, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .dir_in   <- .lP$Input$DirEntityORG
    .path_out <- .lP$Output$FilEntityORG
    .rerun    <- FALSE
  }

  # CACHED ON THE FILE'S EXISTENCE AND NOTHING ELSE, so a changed rule needs .rerun = TRUE or the
  # file deleted. That is a weaker key than a fingerprint of the input, and it is the cost of not
  # carrying one: the release this reads is named by a policy hash in .dir_in, so a changed policy is
  # a changed path rather than the same path holding different bytes.
  if (exp_cache_hit(.task = "ORG", .path_out = .path_out, .paths_in = .dir_in,
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "ORG: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  # THE CHUNK GLOB IS 04D'S OWN. A chunk directory holds the release files beside _done.parquet and
  # _facts.parquet, which have different schemas, so opening the directory itself would ask arrow to
  # unify three unrelated tables.
  ds_ <- apl_dataset(.dir = .dir_in, .stem = "org_mentions")
  if (is.null(ds_)) {
    cli::cli_abort(c(
      "No org_mentions under {.path {as.character(.dir_in)}}.",
      "i" = "The path must be 04D's ORG directory including its policy hash."
    ))
  }

  con_ <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  # FOUR OF TWELVE COLUMNS. The select() runs in arrow, so the projection is pushed into the parquet
  # read and SpanText -- the raw surface form of every mention, and the widest column in the file --
  # is never materialised.
  src_ <- ds_ |>
    dplyr::select("DocID", "PartyKey", "PartyRole", "SpanKey") |>
    arrow::to_duckdb(con = con_, auto_disconnect = FALSE)

  # ONE ROW PER DOCUMENT: the naive rung.
  spell_ <- src_ |>
    dplyr::group_by(.data$DocID) |>
    dplyr::summarise(nUniSpellingsNaive = dplyr::n_distinct(.data$SpanKey), .groups = "drop")

  # ONE ROW PER DOCUMENT PER ROLE, with the naive rung carried alongside so that one collect serves
  # both. It is constant within a document, so the pivot below keeps it as an identifier column.
  tab_long <- src_ |>
    dplyr::distinct(.data$DocID, .data$PartyKey, .data$PartyRole) |>
    dplyr::count(.data$DocID, .data$PartyRole, name = "N") |>
    dplyr::left_join(spell_, by = dplyr::join_by(DocID)) |>
    dplyr::collect()

  out_ <- tab_long |>
    dplyr::mutate(
      dplyr::across(c("N", "nUniSpellingsNaive"), as.integer),
      # plot_factor() ABORTS ON A ROLE NOBODY REGISTERED, which is the gate; fct_relabel() then puts
      # the levels in title case so the columns read nUniRegistrant rather than nUniregistrant, and
      # leaves the registered order alone.
      PartyRole = forcats::fct_relabel(
        plot_factor(.x = .data$PartyRole, .key = "PartyRole"), stringi::stri_trans_totitle
      )
    ) |>
    tidyr::pivot_wider(
      id_cols      = c("DocID", "nUniSpellingsNaive"),   # one row per contract
      names_from   = "PartyRole",                        # a registered factor, so the shape is fixed
      values_from  = "N",                                # distinct parties in that role
      names_prefix = "nUni",
      names_expand = TRUE,                               # every registered level gets a column
      values_fill  = 0L                                  # a role the contract does not name is zero
    ) |>
    dplyr::mutate(
      # AN INTEGER FLAG RATHER THAN THE SENTINEL COUNT. nUniNone is always zero or one and says
      # nothing a reader could guess; the flag says what it means, and Stata wants a 0/1 dummy.
      HasNoParty = as.integer(.data$nUniNone > 0L)
    ) |>
    dplyr::select(
      "DocID", "nUniSpellingsNaive",
      "nUniRegistrant", "nUniCofiler", "nUniCounterparty", "nUniSignatory", "nUniOther",
      "HasNoParty"
    ) |>
    dplyr::arrange(.data$DocID)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)

  cli::cli_alert_success(
    "ORG -> {.path {as.character(fs::path_file(.path_out))}}: \\
     {format(nrow(out_), big.mark = ',')} contracts, {ncol(out_)} columns."
  )

  invisible(out_)
}


# 4. Geography --------------------------------------------------------------------------------------------------------------

#' Geography: how many distinct states and countries a contract names
#'
#' ONE ROW PER CONTRACT THAT NAMES A PLACE, and no sentinel. places_geo carries a row only where the
#' extractor found something, so a contract that was read and named nowhere is absent from this file
#' and arrives as NA when the sample merges to it -- indistinguishable from a contract 04D never
#' reached. That is accepted rather than fixed: filling the population would need org_mentions to
#' supply it, and the two states matter to nobody downstream.
#'
#' LAW-CLAUSE PLACES ARE EXCLUDED EVERYWHERE, INCLUDING THE NAIVE COUNT. "The laws of the State of
#' Delaware" is a jurisdiction rather than a location, and it belongs to entities_law. THIS MOVES THE
#' NAIVE RUNG: the published naive figures -- 2.56 distinct states, 1.81 countries -- count law-clause
#' places in, and these will come out below them. What this file calls naive is every place in this
#' file, which is internally consistent and is not the manuscript's number.
#'
#' THE ROLE SPLIT NEEDS org_mentions, because a role is a property of the party and places_geo carries
#' only the key. That is 04D's own dependency -- its GPE pass folds ORG's policy hash into its own --
#' and it is why this export takes two directories where the others take one.
#'
#' THE JOIN IS THE ATTACHMENT FILTER. geo_attach() nulls PartyKey on every place it did not offer to
#' a party, and a SQL join never matches a null, so an unattached place drops out of the role counts
#' without a filter being written for it. It stays in the naive count, which is what makes the two
#' rungs a comparison.
#'
#' COUNT(DISTINCT) SKIPS NULLS, which is what makes GeoState right without a filter. A country-level
#' mention -- "United States" with no state under it -- has a null GeoState, so it counts towards the
#' country and not towards the state. A contract whose only attached place is a country therefore
#' scores zero states and one country, which is the truth about that contract.
#'
#' ZERO AND NA MEAN DIFFERENT THINGS IN THE ROLE COLUMNS. A contract in this file with no place
#' attached to its registrant scores zero: places were found and none of them was the registrant's.
#' NA appears only after the merge to the sample, for a contract that is not in this file at all.
#'
#' @param .dir_in 04D's release directory for the GPE pass, including its policy hash.
#' @param .dir_org 04D's release directory for the ORG pass; supplies the party roles.
#' @param .path_out Destination parquet.
#' @param .rerun Logical. FALSE returns the file where it already exists.
#' @return Invisibly the written table: one row per contract, seven columns.
export_GPE <- function(.dir_in, .dir_org, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .dir_in   <- .lP$Input$DirEntityGPE
    .dir_org  <- .lP$Input$DirEntityORG
    .path_out <- .lP$Output$FilEntityGPE
    .rerun    <- FALSE
  }

  if (exp_cache_hit(.task = "GPE", .path_out = .path_out, .paths_in = c(.dir_in, .dir_org),
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "GPE: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  ds_plc <- apl_dataset(.dir = .dir_in,  .stem = "places_geo")
  ds_org <- apl_dataset(.dir = .dir_org, .stem = "org_mentions")

  if (is.null(ds_plc) || is.null(ds_org)) {
    cli::cli_abort(c(
      "GPE needs both a places_geo and an org_mentions release.",
      "x" = "places_geo under {.path {as.character(.dir_in)}}: {!is.null(ds_plc)}.",
      "x" = "org_mentions under {.path {as.character(.dir_org)}}: {!is.null(ds_org)}."
    ))
  }

  con_ <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  # FIVE OF places_geo's EIGHTEEN COLUMNS, and the filter runs in arrow beside the projection, so a
  # law-clause place is never read rather than read and discarded. This is the heaviest input in the
  # export at 27.6 million rows.
  src_plc <- ds_plc |>
    dplyr::select("DocID", "PartyKey", "GeoState", "GeoCountryIso", "InLawClause") |>
    dplyr::filter(.data$InLawClause == FALSE) |>
    arrow::to_duckdb(con = con_, auto_disconnect = FALSE)

  # THREE OF org_mentions' TWELVE, and only the two roles the counts split by.
  src_org <- ds_org |>
    dplyr::select("DocID", "PartyKey", "PartyRole") |>
    dplyr::filter(.data$PartyRole %in% .exp_geo_roles) |>
    arrow::to_duckdb(con = con_, auto_disconnect = FALSE)

  role_ <- dplyr::distinct(src_org, .data$DocID, .data$PartyKey, .data$PartyRole)

  # NULL IS NOT A STATE, AND THE COUNT HAS TO SAY SO ITSELF. A country-level mention carries a null
  # GeoState and an unresolved city carries both nulls, and the released counts came out one too high
  # on every contract with such a row: the Places release, which lists the rows, found 144,952
  # registrant counts of 682,024 higher than the rows support. Whatever COUNT(DISTINCT) does with a
  # null on the way from arrow through DuckDB, the rows with nothing to count are removed before it
  # runs, so the count is the same on every backend. The population is kept separately, so a contract
  # whose only places carry no state still has its row with a zero.
  cnt_ <- function(.src, .col, .name, .by) {
    .src |>
      dplyr::filter(!is.na(.data[[.col]])) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(.by))) |>
      dplyr::summarise(N = dplyr::n_distinct(.data[[.col]]), .groups = "drop") |>
      dplyr::rename_with(.fn = \(.x) .name, .cols = "N")
  }

  # ONE ROW PER DOCUMENT: every place the extractor found outside a law clause, attached or not.
  naive_ <- src_plc |>
    dplyr::distinct(.data$DocID) |>
    dplyr::left_join(cnt_(src_plc, "GeoState", "nUniStateNaive", "DocID"), by = dplyr::join_by(DocID)) |>
    dplyr::left_join(cnt_(src_plc, "GeoCountryIso", "nUniCountryNaive", "DocID"), by = dplyr::join_by(DocID))

  # ONE ROW PER DOCUMENT PER ROLE. The inner join is what restricts this to attached places.
  att_ <- src_plc |>
    dplyr::inner_join(role_, by = dplyr::join_by(DocID, PartyKey))
  byrole_ <- att_ |>
    dplyr::distinct(.data$DocID, .data$PartyRole) |>
    dplyr::left_join(cnt_(att_, "GeoState", "State", c("DocID", "PartyRole")),
                     by = dplyr::join_by(DocID, PartyRole)) |>
    dplyr::left_join(cnt_(att_, "GeoCountryIso", "Country", c("DocID", "PartyRole")),
                     by = dplyr::join_by(DocID, PartyRole))

  tab_naive <- dplyr::collect(naive_) |>
    dplyr::mutate(dplyr::across(c("nUniStateNaive", "nUniCountryNaive"), \(.x) dplyr::coalesce(as.integer(.x), 0L)))
  tab_role  <- dplyr::collect(byrole_) |>
    dplyr::mutate(dplyr::across(c("State", "Country"), \(.x) dplyr::coalesce(as.integer(.x), 0L)))

  # THE SHAPE COMES FROM .exp_geo_roles AND NOT FROM THE DATA. names_expand over a factor built on
  # the declared roles is what makes a subset in which no contract names a counterparty produce the
  # same four columns as the corpus does.
  labs_ <- stringi::stri_trans_totitle(.exp_geo_roles)

  wide_ <- tab_role |>
    dplyr::mutate(
      dplyr::across(c("State", "Country"), as.integer),
      PartyRole = factor(stringi::stri_trans_totitle(.data$PartyRole), levels = labs_)
    ) |>
    tidyr::pivot_wider(
      id_cols      = "DocID",                        # one row per contract
      names_from   = "PartyRole",                    # a factor, so the shape is fixed
      values_from  = c("State", "Country"),          # two counts per role
      names_glue   = "nUni{.value}{PartyRole}",      # nUniStateRegistrant, and so on
      names_expand = TRUE,                           # every declared role gets its columns
      values_fill  = 0L                              # a role this contract does not place is zero
    )

  out_ <- tab_naive |>
    dplyr::mutate(dplyr::across(c("nUniStateNaive", "nUniCountryNaive"), as.integer)) |>
    dplyr::left_join(wide_, by = dplyr::join_by(DocID)) |>
    # ZERO AND NOT NA. A contract in this file with no place attached to either role had places found
    # and none of them attached, which is a measurement. The join can only leave NA here, so the
    # coalesce turns the one into the other; NA survives only through absence from the file itself.
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("nUni"), \(.x) as.integer(dplyr::coalesce(.x, 0L)))
    ) |>
    dplyr::select(
      "DocID", "nUniStateNaive", "nUniCountryNaive",
      "nUniStateRegistrant", "nUniCountryRegistrant",
      "nUniStateCounterparty", "nUniCountryCounterparty"
    ) |>
    dplyr::arrange(.data$DocID)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)

  cli::cli_alert_success(
    "GPE -> {.path {as.character(fs::path_file(.path_out))}}: \\
     {format(nrow(out_), big.mark = ',')} contracts, {ncol(out_)} columns."
  )

  invisible(out_)
}


# 5. Governing law ----------------------------------------------------------------------------------------------------------

#' Governing law: every jurisdiction a contract's law clauses named
#'
#' NOTHING IS PICKED. Where a contract names one jurisdiction the column holds it; where it names
#' several they are all there, pipe-separated, in the order the clauses appear in the document. That
#' is a deliberate refusal: no ordering of the candidates is defensible on evidence yet, and a pick
#' would bury the ambiguity in a column that looks certain.
#'
#' THE THREE STRINGS ARE POSITIONALLY ALIGNED. Element i of LawJurisdiction was named by element i of
#' LawKind at level i of LawJurisdictionLevel. That alignment is the whole point, because the kind is
#' what tells a reader which candidate to believe.
#'
#' WHY THAT MATTERS, IN ONE CASE. The cue regexes are unanchored substring matches, so
#'
#'   "duly organized and existing under the laws of the State of Delaware"
#'
#' produces a LawsOfState clause naming Delaware. AN INCORPORATION STATEMENT IN THE PREAMBLE IS
#' INDISTINGUISHABLE FROM A GOVERNING-LAW CLAUSE in the released table, and it is always earlier in
#' the document. A contract can therefore read
#'
#'   LawJurisdiction  "New Jersey|Pennsylvania"
#'   LawKind          "LawsOfState|GovernedBy"
#'
#' where New Jersey is where the company was incorporated and Pennsylvania is the law that governs.
#' Any rule that took the first named jurisdiction would return New Jersey for every contract of this
#' shape, systematically rather than at random.
#'
#' THE CUES DIVIDE INTO THREE READINGS, and this file leaves the division to the reader:
#'   GovernedBy, GovernedByConstrued, ConstruedAccordance   the phrase occurs nowhere else
#'   GoverningLaw                                           a heading; it carries a place only where
#'                                                          its 120 characters ran into the clause
#'   LawsOfState, LawsOfCommonwealth                         also matches incorporation language
#'   SubmitJurisdiction                                     FORUM, not governing law: where you sue
#'                                                          rather than which law applies
#'
#' A REPEATED PAIR APPEARS ONCE. The distinct is on jurisdiction AND kind, so a contract naming
#' Delaware in three GovernedBy clauses lists it once, while one naming Delaware as both LawsOfState
#' and GovernedBy lists both -- which is the reader's evidence that the two agree.
#'
#' A CLAUSE THAT NAMED NOTHING IS COUNTED AND NOT LISTED. nLawClause counts every clause found,
#' nLawSelfRef counts those that name no place by construction, and the difference from the pipe
#' string is clauses whose 120 characters held no place the gazetteer knew. A contract with a
#' GOVERNING LAW heading and nothing parseable under it carries nLawClause = 1 and a missing
#' jurisdiction, which says something a zero could not.
#'
#' nUniJurisdiction COUNTS PLACES AND THE STRING COUNTS PAIRS, so the two differ exactly where one
#' jurisdiction was named by two kinds. Where nUniJurisdiction is 1 the string is a single place and
#' can be used as one without splitting.
#'
#' ONE CLAUSE CAN HOLD TWO PLACES -- "the laws of the State of Delaware, with offices in New York" --
#' and law_clauses carries the clause's offset but not the place's, so two places from one clause
#' order by the file's own row order. dplyr::arrange() is a stable sort, so that is the extractor's
#' order and is deterministic; it is not, however, a property anything documents.
#'
#' THE POPULATION IS CONTRACTS CARRYING AT LEAST ONE CLAUSE, about three quarters of the corpus. The
#' rest state no governing law, are absent here, and merge to NA.
#'
#' @param .dir_in 04D's release directory for the GPE pass, including its policy hash. law_clauses is
#'   released beside places_geo because one 04D pass wrote both.
#' @param .path_out Destination parquet.
#' @param .rerun Logical. FALSE returns the file where it already exists.
#' @return Invisibly the written table: one row per contract, seven columns.
export_LAW <- function(.dir_in, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .dir_in   <- .lP$Input$DirEntityGPE
    .path_out <- .lP$Output$FilEntityLAW
    .rerun    <- FALSE
  }

  if (exp_cache_hit(.task = "LAW", .path_out = .path_out, .paths_in = .dir_in,
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "LAW: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  ds_ <- apl_dataset(.dir = .dir_in, .stem = "law_clauses")
  if (is.null(ds_)) {
    cli::cli_abort(c(
      "No law_clauses under {.path {as.character(.dir_in)}}.",
      "i" = "It sits beside places_geo, in 04D's GPE directory including its policy hash."
    ))
  }

  con_ <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  # FIVE OF law_clauses' SIX COLUMNS. LawStop bounds the clause and nothing here reads it; LawStart
  # is kept because it is what orders the pipe strings.
  src_ <- ds_ |>
    dplyr::select("DocID", "LawStart", "LawKind", "Jurisdiction", "JurisdictionLevel") |>
    arrow::to_duckdb(con = con_, auto_disconnect = FALSE)

  # THE COUNTS COVER EVERY CLAUSE, named or not, so they are the denominator the pipe string is a
  # subset of. n_distinct() skips nulls, so nUniJurisdiction counts places rather than clauses.
  count_ <- src_ |>
    dplyr::group_by(.data$DocID) |>
    dplyr::summarise(
      nLawClause       = dplyr::n(),
      nLawSelfRef      = sum(dplyr::if_else(.data$LawKind == "SelfReferential", 1L, 0L),
                             na.rm = TRUE),
      nUniJurisdiction = dplyr::n_distinct(.data$Jurisdiction),
      .groups          = "drop"
    )

  # ONLY THE CLAUSES THAT NAMED A PLACE reach the strings. A clause naming nothing is in the counts
  # above; putting it in the string as an empty element would make position i mean nothing.
  named_ <- src_ |>
    dplyr::filter(!is.na(.data$Jurisdiction))

  tab_count <- dplyr::collect(count_)
  tab_named <- dplyr::collect(named_)

  # A CUE ADDED UPSTREAM ABORTS HERE. lawregex.py's CUES tuple and this list are two statements of
  # one vocabulary, and a class this file does not know would travel into the release unremarked.
  seen_ <- setdiff(unique(tab_named$LawKind), .exp_law_kinds)
  if (length(seen_) > 0L) {
    cli::cli_abort(c(
      "law_clauses holds {cli::qty(length(seen_))}clause class{?es} this export does not know.",
      "x" = "{paste(seen_, collapse = ', ')}.",
      "i" = "Add them to .exp_law_kinds once their reading is settled."
    ))
  }

  # THE PASTE HAPPENS IN R, because a string aggregation whose ORDER matters is not something to
  # trust to a translation. arrange() then distinct() keeps first appearance, so the three strings
  # are built from one ordering and cannot fall out of step.
  pipe_ <- tab_named |>
    dplyr::arrange(.data$DocID, .data$LawStart) |>
    dplyr::distinct(.data$DocID, .data$Jurisdiction, .data$LawKind, .keep_all = TRUE) |>
    dplyr::summarise(
      LawJurisdiction      = paste(.data$Jurisdiction, collapse = "|"),
      LawJurisdictionLevel = paste(.data$JurisdictionLevel, collapse = "|"),
      LawKind              = paste(.data$LawKind, collapse = "|"),
      .by                  = DocID
    )

  out_ <- tab_count |>
    dplyr::left_join(pipe_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      dplyr::across(c("nLawClause", "nLawSelfRef", "nUniJurisdiction"), as.integer)
    ) |>
    dplyr::select(
      "DocID",
      # aligned: element i of each was read from the same clause
      "LawJurisdiction", "LawJurisdictionLevel", "LawKind",
      # every clause, those that name no place by construction, and distinct places
      "nLawClause", "nLawSelfRef", "nUniJurisdiction"
    ) |>
    dplyr::arrange(.data$DocID)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)

  # HOW MANY CONTRACTS NAME MORE THAN ONE JURISDICTION IS REPORTED, not printed here: it is the mean
  # and the maximum of nUniJurisdiction, which exp_table_numeric() shows on every render.

  cli::cli_alert_success(
    "LAW -> {.path {as.character(fs::path_file(.path_out))}}: \\
     {format(nrow(out_), big.mark = ',')} contracts, {ncol(out_)} columns."
  )

  invisible(out_)
}


# 6. Dates ------------------------------------------------------------------------------------------------------------------

#' Dates: when a contract starts, when it ends, and how that was decided
#'
#' THE ONE EXPORT THAT DOES NOT COUNT ANYTHING. Duration is a cascade rather than an aggregate, and
#' dte_collapse() is where the cascade is written, argued and scored. Re-deriving it here would mean
#' a second implementation of a four-rung rule that has an ordering, a cap and a term parser in it.
#'
#' THE FOUR RUNGS, IN ORDER, AND DurationSource SAYS WHICH ONE ANSWERED.
#'   term    -- the contract states its own length ("a term of five years"). Outranks everything.
#'   open    -- it states that it has no length ("until terminated"). A finding, not a gap.
#'   cue     -- a future date with an end cue beside it: MATURIT, THROUGH, EXPIR.
#'   maxdate -- the farthest future date, for no stated reason. The naive definition, kept last so
#'              coverage does not fall to nothing.
#'
#' TWO FILES AND A REGISTER. Dates and terms are separate releases of the same 04D pass, and the
#' filing date comes from 02B: DateStart is the latest date at or before filing, and a contract whose
#' every date is in the future falls back to the filing date itself. StartSource says which happened.
#'
#' THE POPULATION IS EITHER FILE, NOT BOTH. A contract with a stated term and no parsed date still has
#' a duration -- the term runs from the filing date -- so taking the population from date_spans alone
#' would drop exactly those. A contract in neither file is absent here and merges to NA.
#'
#' TermYears IS THE LONGEST STATED TERM, not the first. dte_terms_collapse() takes the longest and
#' records whether the first disagreed with it; a contract stating a five-year term and a thirty-day
#' cure period should not be described by the cure period.
#'
#' THE CAP DROPS RATHER THAN WINSORISES. A duration above .cap_years leaves DurationYears missing and
#' DurationDropped says "capped". A duration of exactly thirty years that is not one is worse than a
#' missing value.
#'
#' THE NAIVE RUNG IS THE END DATE AND NOT THE START. DateEndNaive is the farthest date after the
#' filing date, whatever the contract mentioned it for, and NaiveYears is that measured from the SAME
#' DateStart the rule uses -- so the two durations differ in one decision rather than two and the
#' comparison isolates it.
#'
#' IT IS MISSING WHENEVER THE CONTRACT NAMES NO FUTURE DATE, and that is the coverage argument for
#' the cascade rather than a fault. A contract stating "a term of two years" and mentioning only
#' dates in its own past has a duration under the term rung and none at all under the naive
#' definition, which is precisely what the term rung exists to reach.
#'
#' NO DateStartNaive, DELIBERATELY. The start rule is a separate dial -- .start = "filed" would make
#' every start the filing date -- and it was never part of the ladder. Publishing the filing date
#' under a naive name would invite a reader to difference the two naive dates and get a number that
#' is not NaiveYears.
#'
#' @param .dir_in 04D's release directory for the DATE pass, including its policy hash.
#' @param .path_register 02B's Documents.parquet; supplies the filing date.
#' @param .path_out Destination parquet.
#' @param .spec List from dte_spec(). The default is what 04B3 released and 04D applied.
#' @param .rerun Logical. FALSE returns the file where it already exists.
#' @return Invisibly the written table: one row per contract, eleven columns.
export_DATE <- function(.dir_in, .path_register, .path_out,
                        .spec = dte_spec(.start = "latest", .end = "term", .cap_years = 30),
                        .rerun = FALSE) {
  if (FALSE) {
    .dir_in        <- .lP$Input$DirEntityDATE
    .path_register <- .lP$Input$FilSample
    .path_out      <- .lP$Output$FilEntityDATE
    .spec          <- dte_spec(.start = "latest", .end = "term", .cap_years = 30)
    .rerun         <- FALSE
  }

  if (exp_cache_hit(.task = "DATE", .path_out = .path_out, .paths_in = c(.dir_in, .path_register),
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "DATE: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  ds_dte <- apl_dataset(.dir = .dir_in, .stem = "date_spans")
  ds_trm <- apl_dataset(.dir = .dir_in, .stem = "term_spans")

  if (is.null(ds_dte) || is.null(ds_trm)) {
    cli::cli_abort(c(
      "DATE needs both released files.",
      "x" = "date_spans: {!is.null(ds_dte)}. term_spans: {!is.null(ds_trm)}.",
      "i" = "Both are under {.path {as.character(.dir_in)}}."
    ))
  }

  con_ <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  # FIVE OF date_spans' TEN COLUMNS. DateText and the two offsets describe where the span sat in the
  # document, which the cascade does not read; Side and CueHit are what HasEndCue was derived from.
  src_dte <- ds_dte |>
    dplyr::select("DocID", "DateValue", "Parsed", "GapDays", "HasEndCue") |>
    arrow::to_duckdb(con = con_, auto_disconnect = FALSE)

  # NINE OF term_spans' ELEVEN. TermStop is the only column dte_terms_collapse() never reads.
  src_trm <- ds_trm |>
    dplyr::select("DocID", "TermStart", "TermText", "TermKind", "TermN", "TermUnit", "TermYears",
                  "IsOpen", "PeriodKind") |>
    arrow::to_duckdb(con = con_, auto_disconnect = FALSE)

  src_reg <- arrow::open_dataset(sources = .path_register) |>
    dplyr::select("DocID", "DateFiled") |>
    arrow::to_duckdb(con = con_, auto_disconnect = FALSE)

  # THE POPULATION IS THE UNION AND THE JOIN IS INNER. A DocID either file names gets a row; one the
  # register has no filing date for cannot have a start, so it drops rather than arriving with a
  # duration measured from nothing.
  tab_keys <- dplyr::union(
    dplyr::distinct(src_dte, .data$DocID),
    dplyr::distinct(src_trm, .data$DocID)
  ) |>
    dplyr::inner_join(src_reg, by = dplyr::join_by(DocID)) |>
    dplyr::collect() |>
    # THE CASCADE DOES DATE ARITHMETIC, so the register's filing date has to be a Date rather than
    # whatever it was stored as. anytime has no SQL form, which is why this sits after the collect.
    dplyr::mutate(DateFiled = anytime::anydate(as.character(.data$DateFiled)))

  # COLLECTED WHOLE, AND DELIBERATELY. dte_collapse() takes a maximum over the dates of a document
  # and a longest-term pick over its terms; neither has a form DuckDB could hold onto, so the two
  # span tables come back to R and the cascade runs there on 04B3's own function.
  tab_dates <- dplyr::collect(src_dte)
  tab_terms <- dplyr::collect(src_trm)

  out_ <- dte_collapse(
    .dates = tab_dates,   # one row per parsed date
    .terms = tab_terms,   # one row per stated period
    .keys  = tab_keys,    # the population, with the filing date
    .spec  = .spec        # which rungs the cascade may use, and the cap
  ) |>
    dplyr::select(
      "DocID",
      # what the contract runs from, and whether that was read or assumed
      "DateStart", "StartSource",
      # what it runs to, which rung said so, and why a duration is missing where it is
      "DateEnd", "DurationYears", "DurationSource", "DurationDropped",
      # what the contract says about its own length, independent of the arithmetic above
      "TermYears", "TermKind",
      # THE NAIVE RUNG, AND IT VARIES THE END ONLY. Both durations run from the same DateStart, so
      # NaiveYears is exactly (DateEndNaive - DateStart) / 365.25 and the two cannot disagree.
      DateEndNaive = "EndAny", "NaiveYears"
    ) |>
    dplyr::arrange(.data$DocID)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)

  cli::cli_alert_success(
    "DATE -> {.path {as.character(fs::path_file(.path_out))}}: \\
     {format(nrow(out_), big.mark = ',')} contracts, {ncol(out_)} columns."
  )

  invisible(out_)
}


# 7. Money ------------------------------------------------------------------------------------------------------------------

#' Money: how many amounts a contract names, and how large they are
#'
#' KNOWN DEFECT, SHIPPED DELIBERATELY, AND THIS IS WHERE IT IS WRITTEN DOWN.
#'
#' MoneyMaxUSD AND MoneyMaxOther ARE WRONG FOR ROUGHLY 1,200 CONTRACTS. 1,684 spans of 12.4 million
#' carry an Amount above 1e12, up to 5.7e294. They are not parser errors: the raw strings are
#' concatenated table cells. "$ 201620172018201..." is four fiscal years glued together and
#' "EUR00000000300661..." is a fixed-width numeric field. HTML flattening removed the cell boundaries
#' and the money regex read the run as one number. It hits both the symbol and the ISO patterns, so
#' it is digit-run greediness rather than one bad expression.
#'
#' THE TAIL IS NOT THE PROBLEM. A 295-digit figure is obvious and can be filtered by anyone. Two
#' glued four-digit cells give 20162017, which is twenty million dollars: plausible, undetectable by
#' magnitude, and invisible in every diagnostic run so far. Any use of a maximum should expect a
#' small share of contracts to carry a figure that is two cells rather than one.
#'
#' THE MEDIANS ARE FAR LESS AFFECTED, WHICH IS NOT THE SAME AS IMMUNE. A single glued run cannot move
#' a median, which is why MoneyMedUSD sits beside the maximum rather than instead of it -- but a
#' contract whose figures are MOSTLY glued runs has a glued median, and 118 of the 761,144 contracts
#' carrying a USD figure have a median above 1e11. That is 0.015% against 0.2% for the maximum.
#'
#' THE FIX IS AGREED AND NOT YET APPLIED: a declared ceiling at 1e11 in mny_spec(), a sixth MoneyDrop
#' level beside withheld / unparsed / zero / par / kept, and one more clause in the filter. It lands
#' in the MONEY policy hash, so MONEY alone re-runs. The real repair for the body of the problem is
#' grouping REGULARITY -- a genuine number has commas every three digits from the right, a glued pair
#' has one group of the wrong length -- because commas return at the very top of the distribution and
#' cannot separate junk from real above twelve digits.
#'
#' Every render reports how many contracts in the file exceed 1e11, so the number is measured rather
#' than carried forward from this comment.
#'
#' THE LADDER. The naive rung is every amount the extractor parsed and the issuer did not withhold.
#' The rule additionally drops a zero, and a figure with par-value language beside it -- "par value
#' $0.001 per share" is a share denomination rather than a contract amount. That is mny_spec()'s
#' .filter = "par", written out here rather than passed in, because the filter is three predicates
#' and reading them is the point.
#'
#' THE CURRENCY SPLIT IS LOAD-BEARING. There is no FX conversion anywhere in this pipeline, so a
#' single largest-amount column would mix dollars with euros and yen silently. Counts are
#' currency-blind and may be pooled, which is why the naive count is not split; values are not.
#'
#' NO SUM. A total is whichever table the contract happened to include, and it cannot be added across
#' currencies. The maximum and the median answer the questions a sum is usually reached for.
#'
#' THE POPULATION IS EVERY CONTRACT THE EXTRACTOR FOUND MONEY IN, including one whose every figure
#' was withheld. Those carry nUniAmountNaive = 0 with the values missing, which distinguishes "the
#' prices in this contract are redacted" from "this contract names no money" -- and the first is the
#' state this whole project exists to measure.
#'
#' @param .dir_in 04D's release directory for the MONEY pass, including its policy hash.
#' @param .path_out Destination parquet.
#' @param .rerun Logical. FALSE returns the file where it already exists.
#' @return Invisibly the written table: one row per contract, eight columns.
export_MONEY <- function(.dir_in, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .dir_in   <- .lP$Input$DirEntityMONEY
    .path_out <- .lP$Output$FilEntityMONEY
    .rerun    <- FALSE
  }

  if (exp_cache_hit(.task = "MONEY", .path_out = .path_out, .paths_in = .dir_in,
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "MONEY: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  # A BLOCK NOBODY NAMES WOULD BE DROPPED BY THE PIVOT WITHOUT A WORD, so the mapping is checked
  # against 04B4's registered vocabulary before anything is read.
  blocks_ <- plot_levels(.key = "MoneyBlock")
  if (!setequal(blocks_, names(.exp_money_blocks))) {
    cli::cli_abort(c(
      "The currency blocks 04B4 registers are not the ones this export names.",
      "x" = "Registered: {paste(blocks_, collapse = ', ')}.",
      "i" = "Named here: {paste(names(.exp_money_blocks), collapse = ', ')}."
    ))
  }

  ds_ <- apl_dataset(.dir = .dir_in, .stem = "money_spans")
  if (is.null(ds_)) {
    cli::cli_abort(c(
      "No money_spans under {.path {as.character(.dir_in)}}.",
      "i" = "The path must be 04D's MONEY directory including its policy hash."
    ))
  }

  con_ <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  # SEVEN OF money_spans' SIXTEEN COLUMNS. MoneyText and the offsets say where the figure sat,
  # Pattern and AmountRaw say how it was read, ParSide and ParCue say why IsPar is what it is, and
  # MoneyDrop is the label this filter would reproduce. None of them is read here.
  src_ <- ds_ |>
    dplyr::select("DocID", "Amount", "Block", "Parsed", "Withheld", "IsZero", "IsPar") |>
    arrow::to_duckdb(con = con_, auto_disconnect = FALSE)

  # THE POPULATION: every contract the extractor found any money figure in, withheld or not.
  spine_ <- dplyr::distinct(src_, .data$DocID)

  # THE NAIVE RUNG: parsed, and not a figure the issuer withheld. A withheld amount has no value to
  # count, so it is excluded from both rungs rather than being the difference between them.
  seen_ <- dplyr::filter(src_, .data$Parsed, !.data$Withheld)

  naive_ <- seen_ |>
    dplyr::group_by(.data$DocID) |>
    dplyr::summarise(nUniAmountNaive = dplyr::n_distinct(.data$Amount), .groups = "drop")

  # THE RULE: mny_spec(.filter = "par") written out. Both clauses drop a figure that is not a
  # contract amount -- a zero, and a share denomination sitting beside par-value language.
  kept_ <- seen_ |>
    dplyr::filter(!.data$IsZero, !.data$IsPar)

  # median() BECOMES A PERCENTILE AGGREGATE IN DUCKDB, which is why this can stay lazy; every figure
  # a contract names is aggregated in the database and only one row per contract per block comes
  # back.
  byblock_ <- kept_ |>
    dplyr::group_by(.data$DocID, .data$Block) |>
    dplyr::summarise(
      nUniAmount = dplyr::n_distinct(.data$Amount),
      MoneyMax   = max(.data$Amount, na.rm = TRUE),
      MoneyMed   = stats::median(.data$Amount, na.rm = TRUE),
      .groups    = "drop"
    )

  tab_spine <- dplyr::collect(spine_)
  tab_naive <- dplyr::collect(naive_)
  tab_block <- dplyr::collect(byblock_)

  # THE SHAPE COMES FROM .exp_money_blocks. names_expand over a factor built on the declared blocks
  # gives a contract naming only dollars its four columns anyway, so a subset and the corpus produce
  # the same file.
  wide_ <- tab_block |>
    dplyr::mutate(
      nUniAmount = as.integer(.data$nUniAmount),
      Block      = factor(
        .data$Block, levels = names(.exp_money_blocks), labels = unname(.exp_money_blocks)
      )
    ) |>
    tidyr::pivot_wider(
      id_cols      = "DocID",                             # one row per contract
      names_from   = "Block",                             # a factor, so the shape is fixed
      values_from  = c("nUniAmount", "MoneyMax", "MoneyMed"),
      names_glue   = "{.value}{Block}",                   # nUniAmountUSD, MoneyMaxOther, and so on
      names_expand = TRUE,                                # every declared block gets its columns
      # A COUNT AND A VALUE FILL DIFFERENTLY. A contract naming no euro figure names zero of them;
      # its largest euro figure is not zero, it does not exist.
      values_fill  = list(nUniAmount = 0L)
    )

  out_ <- tab_spine |>
    dplyr::left_join(tab_naive, by = dplyr::join_by(DocID)) |>
    dplyr::left_join(wide_,     by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      # ZERO AND NOT NA ON THE COUNTS. A contract in this file whose every figure was withheld or
      # unparsed named no usable amount, which is a measurement; NA survives only through absence
      # from the file itself.
      dplyr::across(dplyr::starts_with("nUni"), \(.x) as.integer(dplyr::coalesce(.x, 0L)))
    ) |>
    dplyr::select(
      "DocID", "nUniAmountNaive", "nUniAmountUSD", "nUniAmountOther",
      "MoneyMaxUSD", "MoneyMedUSD", "MoneyMaxOther", "MoneyMedOther"
    ) |>
    dplyr::arrange(.data$DocID)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)

  # THE DEFECT IS MEASURED IN THE REPORT AND NOT HERE. exp_table_numeric(.ceiling = 1e11) counts what
  # exceeds the ceiling 04B4 would declare, on every render rather than only on a rebuild, and the
  # Max column beside it says how far the tail runs -- which a count alone would not.

  cli::cli_alert_success(
    "MONEY -> {.path {as.character(fs::path_file(.path_out))}}: \\
     {format(nrow(out_), big.mark = ',')} contracts, {ncol(out_)} columns."
  )

  invisible(out_)
}


# 8. Redactions -------------------------------------------------------------------------------------------------------------

#' Redactions: how many markers of each kind a contract carries
#'
#' SIX KINDS AND NOTHING ELSE, because every aggregate anyone wants is a sum of them. Unlike the
#' party file, where the naive rung could not be recovered from the role counts, here it can:
#'
#'   every marker           = all six
#'   bracketed only         = the first five; RedactBare is the only unbracketed kind
#'   text actually withheld = the four Redact kinds; the two Omit kinds are not redactions
#'
#' Storing those three beside the six would be storing the same information twice and choosing a
#' denominator on the reader's behalf. Whoever uses this file adds up the columns their definition
#' calls for, in their own code, where the choice is visible.
#'
#' THE TWO SPLITS CUT ACROSS EACH OTHER, WHICH IS WHY NO SINGLE TOTAL IS "REDACTIONS".
#'
#'   Kind             Bracketed  Withheld   What it is
#'   RedactExplicit   yes        yes        a bracket naming confidential treatment
#'   RedactSymbol     yes        yes        a bracket holding a symbol, "[ ]" or "[*]"
#'   RedactBlank      yes        yes        an empty bracket
#'   OmitExplicit     yes        NO         "[Intentionally Omitted]" -- a section left out
#'   OmitSymbol       yes        NO         "[.]" in the same role
#'   RedactBare       NO         yes        "*******" with no bracket around it
#'
#' The published measure counts BRACKETS, which is the first five and therefore includes the two Omit
#' kinds -- a page left blank on purpose is not text withheld from a reader. The withheld measure is
#' the four Redact kinds and therefore includes RedactBare, which is the unreliable one: a row of
#' asterisks may be a horizontal rule. CONFIRM WHICH SUM THE PAPER'S FIGURES USED before either is
#' quoted against them.
#'
#' Bracketed AND Withheld ARE NOT READ, because both are functions of Kind and 04B5 declares them as
#' such. Reading them would let a future release disagree with itself in this file without anything
#' noticing.
#'
#' nRedactMoney IS THE ONE COLUMN THAT IS NOT A SUM OF THE OTHERS. It comes from matching a marker to
#' a withheld price beside it, so it says how many of this contract's PRICES were withheld rather
#' than how many marks it carries -- which is the measurement this project exists for.
#'
#' n AND NOT nUni. Every row is a marker at its own offset in the document, so there is nothing to
#' de-duplicate and these are occurrences rather than distinct things.
#'
#' THE POPULATION IS CONTRACTS CARRYING AT LEAST ONE MARKER, which is about a fifth of the corpus.
#' The other four fifths redact nothing, are absent from this file, and merge to NA -- the same state
#' as a contract 04D never reached. A zero here means markers were found and none was of that kind.
#'
#' @param .dir_in 04D's release directory for the REDACT pass, including its policy hash.
#' @param .path_out Destination parquet.
#' @param .rerun Logical. FALSE returns the file where it already exists.
#' @return Invisibly the written table: one row per contract, eight columns.
export_REDACT <- function(.dir_in, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .dir_in   <- .lP$Input$DirEntityREDACT
    .path_out <- .lP$Output$FilEntityREDACT
    .rerun    <- FALSE
  }

  if (exp_cache_hit(.task = "REDACT", .path_out = .path_out, .paths_in = .dir_in,
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "REDACT: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  ds_ <- apl_dataset(.dir = .dir_in, .stem = "redact_spans")
  if (is.null(ds_)) {
    cli::cli_abort(c(
      "No redact_spans under {.path {as.character(.dir_in)}}.",
      "i" = "The path must be 04D's REDACT directory including its policy hash."
    ))
  }

  con_ <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  # FOUR OF redact_spans' TEN COLUMNS. MarkText is read to refuse a marker; the two offsets locate
  # it, RedactedRef and RedactedText carry the price it was matched to, and Bracketed and Withheld
  # are restatements of Kind.
  #
  # AN EMPTY BRACKET IS NOT A MARKER, AND THE EXTRACTOR EMITTED 478,771 OF THEM. redaction.py's
  # RedactSymbol pattern read ^[*\\s]+$, which a lone space satisfies, so every "[ ]" on EDGAR --
  # checkboxes, form fields, blanks in schedules -- was stored as a symbol marker: 63,270 contracts
  # with nothing but these, at a pre-FAST CTO precision of 0.011. The same defect sits in the blank
  # and bullet patterns. The pattern is fixed at the source; until the REDACT pass is re-run, this
  # is the guard: a marker whose text is nothing but brackets and whitespace is refused here, on
  # the same rule the fixed classify() applies, so the counts are the ones the fixed extractor
  # would have written. The refused count is reported, and a release built on the re-run store
  # should report zero.
  n_all_ <- ds_ |>
    dplyr::summarise(N = dplyr::n()) |>
    dplyr::collect() |>
    dplyr::pull(.data$N)
  src_ <- ds_ |>
    dplyr::select("DocID", "Kind", "RedactedEntity", "MarkText") |>
    # RE2, not Python: \\s here is ASCII whitespace, so the next-line and no-break-space characters
    # that Python's \\s covers -- both present in the store -- are named. This is the same set the
    # fixed classify() strips.
    dplyr::filter(!grepl("^\\[[\\s\\x85\\xA0]*\\]$", .data$MarkText)) |>
    dplyr::select("DocID", "Kind", "RedactedEntity") |>
    arrow::to_duckdb(con = con_, auto_disconnect = FALSE)
  n_kept_ <- src_ |>
    dplyr::summarise(N = dplyr::n()) |>
    dplyr::collect() |>
    dplyr::pull(.data$N)
  n_refused_ <- as.integer(n_all_ - n_kept_)
  if (n_refused_ > 0L) {
    cli::cli_alert_warning(
      "REDACT: {format(n_refused_, big.mark = ',')} empty-bracket {cli::qty(n_refused_)}marker{?s} refused \\
       (the [ ] defect in redaction.py). Zero here means the store was built with the fixed extractor."
    )
  }

  bykind_ <- src_ |>
    dplyr::group_by(.data$DocID, .data$Kind) |>
    dplyr::summarise(N = dplyr::n(), .groups = "drop")

  # if_else RATHER THAN sum(RedactedEntity == "money"). A boolean has no sum in SQL, and the CASE
  # WHEN if_else() becomes does have one. RedactedEntity is never null -- 04B5 coalesces an unmatched
  # marker to "unmatched" -- so na.rm here guards against a future release rather than this one.
  money_ <- src_ |>
    dplyr::group_by(.data$DocID) |>
    dplyr::summarise(
      nRedactMoney = sum(dplyr::if_else(.data$RedactedEntity == "money", 1L, 0L), na.rm = TRUE),
      .groups      = "drop"
    )

  tab_kind  <- dplyr::collect(bykind_)
  tab_money <- dplyr::collect(money_)

  out_ <- tab_kind |>
    dplyr::mutate(
      N = as.integer(.data$N),
      # plot_factor() ABORTS ON A KIND NOBODY REGISTERED, which is the gate redaction.py's CLASSES
      # tuple and 04B5's vocabulary need between them. The registered order is the column order.
      Kind = plot_factor(.x = .data$Kind, .key = "RedactKind")
    ) |>
    tidyr::pivot_wider(
      id_cols      = "DocID",        # one row per contract
      names_from   = "Kind",         # a registered factor, so the shape is fixed
      values_from  = "N",            # markers of that kind
      names_prefix = "n",            # the kinds already read Redact* and Omit*
      names_expand = TRUE,           # every registered kind gets a column
      values_fill  = 0L              # a kind this contract does not carry is zero, not missing
    ) |>
    dplyr::left_join(tab_money, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(nRedactMoney = as.integer(dplyr::coalesce(.data$nRedactMoney, 0L))) |>
    dplyr::select(
      "DocID",
      # the three that withhold text, inside brackets
      "nRedactExplicit", "nRedactSymbol", "nRedactBlank",
      # the two that record a section left out, inside brackets
      "nOmitExplicit", "nOmitSymbol",
      # the one that withholds text with no bracket around it, and is the noisy one
      "nRedactBare",
      # not a sum of the above: markers matched to a withheld price beside them
      "nRedactMoney"
    ) |>
    dplyr::arrange(.data$DocID)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)

  cli::cli_alert_success(
    "REDACT -> {.path {as.character(fs::path_file(.path_out))}}: \\
     {format(nrow(out_), big.mark = ',')} contracts, {ncol(out_)} columns."
  )

  invisible(out_)
}


# 9. The joined file --------------------------------------------------------------------------------------------------------

#' Everything, joined onto the sample
#'
#' THE ONE FILE THE ANALYSIS READS. The eight caches above are intermediate: each is cheap to rebuild
#' on its own, and a changed counterparty rule rewrites one of them and nothing else. This joins them
#' and is the only place a .dta is written.
#'
#' THE SAMPLE IS THE SPINE AND IT IS A LEFT JOIN THROUGHOUT. Every registrant copy is in the output
#' whatever the extraction reached, so the row count is the register's and no contract disappears
#' because one pass could not read it.
#'
#' THE CLASSIFICATION JOINS ON DocID, ONE TO ONE, because 03F already fanned out to copies.
#'
#' THE ENTITY CACHES JOIN ON HashDocument, MANY TO ONE, and that is the substance of this function. A
#' collapse is keyed on the primary copy 04C extracted; a filing naming several registrants lists that
#' one attachment under each of them, and the text is the same attachment in every copy. So each cache
#' is re-keyed from its primary DocID to the HashDocument that primary belongs to, and every copy of
#' that attachment picks up the same values. Roughly one row in six is a copy of this kind, and
#' leaving them empty would have discarded them for no reason.
#'
#' TWO THINGS FOLLOW FROM THAT. The entity values REPEAT across copies, so a corpus mean taken over
#' the raw file double-counts and wants PrimaryFiler == 1 -- the same condition 04D's own tables use.
#' And a row that is still empty after the join means 04D genuinely did not reach that attachment,
#' rather than that the row is a co-filer.
#'
#' FilerCopiesAgree IS THE ONE CASE THIS CANNOT SPEAK FOR. Where an attachment's copies disagree on
#' length the text under one registrant is not the text under another, and every entity value here was
#' computed from one of them. 01C found one attachment of 1.49 million in that state.
#'
#' EVERY JOIN IS CHECKED FOR NAME COLLISIONS FIRST. dplyr suffixes Column.x and Column.y in silence,
#' and a file of a hundred columns is exactly where that would go unnoticed.
#'
#' THE CODEBOOK CARRIES BOTH HALVES. Which cache a column came from, its type and its Stata name are
#' derived from the file; what it means and what a missing value in it says are declared in section
#' 11. exp_table_dictionary() aborts where the two disagree about which columns exist, so a column
#' added upstream and documented nowhere stops the render rather than shipping undocumented.
#'
#' @param .path_sample The sample cache; the spine.
#' @param .path_class The classification cache, or NA to leave it out.
#' @param .paths_entity Named character. The entity caches, keyed on the primary DocID.
#' @param .path_out Destination parquet. The dta and the codebook sit beside it.
#' @param .rerun Logical. FALSE returns the file where it already exists.
#' @param .dta Logical. Also write the Stata copy and the codebook.
#' @return Invisibly the written table: one row per registrant copy.
export_final <- function(.path_sample, .path_items, .path_summary, .path_cto, .path_class,
                         .paths_entity, .path_out, .rerun = FALSE, .dta = TRUE) {
  if (FALSE) {
    .path_sample  <- .lP$Cache$CacheSample
    .path_items   <- .lP$Cache$CacheItems
    .path_summary <- .lP$Cache$CacheSummary
    .path_cto     <- .lP$Cache$CacheCto
    .path_class   <- .lP$Cache$CacheClassification
    .paths_entity <- .lP$Cache[c("CacheEntityORG", "CacheEntityGPE")]
    .path_out     <- .lP$Output$FilContracts
    .rerun        <- FALSE
    .dta          <- TRUE
  }

  in_ <- c(.path_sample, .path_items, .path_summary, .path_cto, .path_class, unlist(.paths_entity))

  if (exp_cache_hit(.task = "Contracts", .path_out = .path_out, .paths_in = in_,
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "Contracts: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  out_ <- arrow::read_parquet(file = .path_sample)

  need_ <- c("DocID", "HashDocument", "HashIndex", "PrimaryFiler")
  miss_ <- setdiff(need_, names(out_))
  if (length(miss_) > 0L) {
    cli::cli_abort(c(
      "The sample cannot act as the spine.",
      "x" = "It is missing {paste(miss_, collapse = ', ')}.",
      "i" = "DocID keys the classification and the orders, HashDocument keys the entity blocks,
             HashIndex keys the items, and PrimaryFiler says which copy was read."
    ))
  }

  book_ <- tibble::tibble(Column = names(out_), Source = "Sample")

  # THE ITEMS ARE A PROPERTY OF THE FILING, so the join is many-to-one on HashIndex and two exhibits
  # of one 8-K necessarily carry the same counts. NA after this join means the parent filing has no
  # item list at all -- a 10-K, an S-1 -- which is not the same as an 8-K reporting none, and that
  # second case cannot happen. Coalescing to zero here would erase the difference; an analysis that
  # wants it erased can say so in one line.
  if (!is.na(.path_items)) {
    itm_ <- arrow::read_parquet(file = .path_items)
    exp_check_collide(
      .a = names(out_), .b = names(itm_), .by = "HashIndex", .what = "the items join"
    )
    out_ <- dplyr::left_join(
      out_, itm_, by = dplyr::join_by(HashIndex), relationship = "many-to-one"
    )
    book_ <- dplyr::bind_rows(
      book_, tibble::tibble(Column = setdiff(names(itm_), "HashIndex"), Source = "Items")
    )
  }

  # THE SUMMARY BLOCK REACHES ONLY 8-K CONTRACTS WITH A RECOVERED NARRATIVE, on the same key as the
  # items. Missing everywhere else: a 10-K, or an 8-K whose Item 1.01 01D could not parse.
  if (!is.na(.path_summary)) {
    sum_ <- arrow::read_parquet(file = .path_summary)
    exp_check_collide(
      .a = names(out_), .b = names(sum_), .by = "HashIndex", .what = "the summary join"
    )
    out_ <- dplyr::left_join(
      out_, sum_, by = dplyr::join_by(HashIndex), relationship = "many-to-one"
    )
    book_ <- dplyr::bind_rows(
      book_, tibble::tibble(Column = setdiff(names(sum_), "HashIndex"), Source = "Summary")
    )
  }

  # THE ORDERS ARE THE ONE BLOCK WITH NO MISSING MARKER. nCtoOrders and CtoIsExtension become zero
  # where no order names the contract, because "no order was found" is a measurement over every
  # contract rather than a block that failed to arrive -- and it is the definition the published
  # redaction variable is built on. The dates stay missing, which is where the absence shows.
  if (!is.na(.path_cto)) {
    cto_ <- arrow::read_parquet(file = .path_cto)
    exp_check_collide(
      .a = names(out_), .b = names(cto_), .by = "DocID", .what = "the orders join"
    )
    out_ <- out_ |>
      dplyr::left_join(cto_, by = dplyr::join_by(DocID), relationship = "one-to-one") |>
      dplyr::mutate(
        nCtoOrders     = dplyr::coalesce(.data$nCtoOrders, 0L),
        CtoIsExtension = dplyr::coalesce(.data$CtoIsExtension, 0L),
        HasCto         = as.integer(.data$nCtoOrders > 0L)
      ) |>
      dplyr::relocate("HasCto", "nCtoOrders", "CtoReleaseFirst", "CtoReleaseLast",
                      "CtoIsExtension", .after = "EstiSample")
    book_ <- dplyr::bind_rows(
      book_, tibble::tibble(Column = c("HasCto", setdiff(names(cto_), "DocID")), Source = "Orders")
    )
  }

  # THE CLASSIFICATION IS ALREADY AT COPY GRAIN. one-to-one is a check as much as a hint: a duplicate
  # DocID on either side stops the render rather than silently multiplying the sample.
  if (!is.na(.path_class)) {
    cls_ <- arrow::read_parquet(file = .path_class)
    exp_check_collide(
      .a = names(out_), .b = names(cls_), .by = "DocID", .what = "the classification join"
    )
    out_ <- dplyr::left_join(
      out_, cls_, by = dplyr::join_by(DocID), relationship = "one-to-one"
    )
    book_ <- dplyr::bind_rows(
      book_, tibble::tibble(Column = setdiff(names(cls_), "DocID"), Source = "Classification")
    )
  }

  # THE MAP FROM ATTACHMENT TO COPY, built once from the spine so every entity cache fans out on the
  # same key.
  map_ <- out_ |>
    dplyr::filter(as.logical(.data$PrimaryFiler)) |>
    dplyr::select("HashDocument", Primary = "DocID")

  for (nm_ in names(.paths_entity)) {
    ent_ <- arrow::read_parquet(file = .paths_entity[[nm_]])
    exp_check_collide(
      .a = names(out_), .b = setdiff(names(ent_), "DocID"), .by = character(0),
      .what = paste("the", nm_, "join")
    )

    # RE-KEYED FROM THE PRIMARY DocID TO ITS HashDocument. one-to-one here is the 1:1 check on the
    # cache: a primary named twice, or a DocID the sample does not have as a primary, stops here.
    ent_ <- ent_ |>
      dplyr::rename(Primary = "DocID") |>
      dplyr::inner_join(map_, by = dplyr::join_by(Primary), relationship = "one-to-one") |>
      dplyr::select(-"Primary")

    out_ <- dplyr::left_join(
      out_, ent_, by = dplyr::join_by(HashDocument), relationship = "many-to-one"
    )
    book_ <- dplyr::bind_rows(
      book_, tibble::tibble(Column = setdiff(names(ent_), "HashDocument"), Source = nm_)
    )
  }

  out_ <- dplyr::arrange(out_, .data$DocID)

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(out_, .path_out)

  if (.dta) {
    sta_ <- exp_stata_ready(.tab = out_)

    haven::write_dta(sta_$Tab, fs::path_ext_set(.path_out, "dta"))

    # THE CODEBOOK IS THE DICTIONARY JOINED TO THE FILE, not a list of types. Source says which cache
    # a column came from and is derived; Meaning and Missing are declared in section 11 and are the
    # part nobody can reconstruct from the data.
    exp_table_dictionary(.tab = out_) |>
      dplyr::left_join(book_, by = dplyr::join_by(Column)) |>
      dplyr::select("Column", "StataName", "Block", "Source", "Type", "Meaning", "Missing") |>
      readr::write_csv(fs::path_ext_set(paste0(fs::path_ext_remove(.path_out), "_Codebook"), "csv"))

    if (sta_$Changed > 0L) {
      cli::cli_alert_info(
        "{sta_$Changed} {cli::qty(sta_$Changed)}column{?s} {?was/were} renamed for Stata; the \\
         codebook carries both names."
      )
    }
    if (length(sta_$Cut) > 0L) {
      cli::cli_alert_warning(
        "{length(sta_$Cut)} {cli::qty(length(sta_$Cut))}column{?s} exceeded Stata's \\
         2,045-character string and {?was/were} truncated IN THE DTA ONLY: \\
         {paste(sta_$Cut, collapse = ', ')}. The parquet holds the full value."
      )
    }
  }

  # WHAT THE JOIN REACHED IS exp_check_final()'S TO SAY, on every render rather than on a rebuild.

  cli::cli_alert_success(
    "Contracts -> {.path {as.character(fs::path_file(.path_out))}}: \\
     {format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns."
  )

  invisible(out_)
}


# 9b. The long releases ---------------------------------------------------------------------------------------------------
#
# THREE FILES AT A GRAIN Contracts CANNOT HOLD. Contracts is one row per contract and carries counts:
# how many states, how many countries, whether an order covers it. The descriptives in 30 want the
# rows behind those counts -- which places, so a map can be drawn; which term families, so the
# pandemic panel can be built; which orders, so the order side of section 3.5 can be counted rather
# than only the contract side. Until here those three lived only in the repository, under 04D, 05A
# and 01E, and 30 could not run off her Dropbox folder alone.
#
# EACH IS ONE FUNCTION IN THE SHAPE OF export_summaries(): cached on the inputs' modification time,
# every column declared in a dictionary the function checks itself against, written as parquet with a
# codebook beside it, and a dta only where Stata could hold it. None of them joins into Contracts, and
# export_final() never sees them; they carry DocID so 30 joins them itself, and HashDocument so a
# reader who wants every registrant copy can fan them out the way export_final() fans out the entity
# blocks.

#: What each place row may attach to. One role per party: export_ORG() counts a party under every
#: role it holds, but a place on a map wants one owner, so where a party holds two the earlier one
#: here wins. The order is the ORG rules' own: the registrant is found before its co-filers, those
#: before the counterparties. "none" is a place no party mention reached.
.exp_place_roles <- c("registrant", "cofiler", "counterparty", "signatory", "other", "none")

#: The Places release, column by column. Checked by export_places() against what it writes.
.exp_dictionary_places <- tibble::tribble(
  ~Column, ~Meaning, ~Missing,
  "DocID", "The contract: the primary copy 04D read. Joins to Contracts on DocID.", "never",
  "HashDocument", "The attachment. Joins to every registrant copy of it in Contracts.", "never",
  "GeoLevel", "Country, State, County, City or Other: what the name resolved to.", "never",
  "GeoState", "The US state, where the tier or the resolution gives one.", "where the place is not in the US",
  "GeoCountryIso", "ISO-3 code; USA for any US tier.", "where the gazetteer gave none",
  "GeoCountry", "The country's name; United States for any US tier.", "where GeoCountryIso is",
  "PartyRole", "registrant, cofiler, counterparty, signatory, other, or none where no party reached it.", "never",
  "InLawClause", "1 where the mention sat inside a governing-law clause, so a jurisdiction, not a location.", "never",
  "nMentions", "How often this contract mentions this place in this role.", "never"
)

#: The TermDocs release: 05A's term_docs, cut to the contracts, with its own names kept.
.exp_dictionary_termdocs <- tibble::tribble(
  ~Column, ~Meaning, ~Missing,
  "DocID", "The contract 05A scanned. Joins to Contracts on DocID.", "never",
  "HashDocument", "The attachment. Joins to every registrant copy of it in Contracts.", "never",
  "Family", "Pandemic, Disruption, RateReform, Regulation or Boilerplate: the term family.", "never",
  "nHits", "Occurrences of any term of the family; a term found twice counts twice.", "never",
  "nTerms", "Distinct terms of the family found.", "never",
  "HasTerm", "1 where nHits is positive.", "never",
  "FirstPos", "Relative position of the first hit, 0 at the start of the text and 1 at its end.", "where nHits is zero",
  "PerKWords", "nHits per thousand words of the contract.", "where the register holds no word count"
)

#: The CtoOrders release: 01E's references, one row each, the order side of the redaction story.
.exp_dictionary_cto_orders <- tibble::tribble(
  ~Column, ~Meaning, ~Missing,
  "OrderDocID", "The order's own document on EDGAR.", "never",
  "OrderCIK", "The filer the order was addressed to.", "never",
  "OrderDate", "The day the order was filed.", "never",
  "Status", "GRANTING, DENYING, REVOKING or what else the title line said.", "where the order's text was empty",
  "IsExtension", "1 where the order grants more time on an earlier grant.", "never",
  "ExhibitNo", "The exhibit the reference names, bare: 10.1, not EX-10.1.", "where no reference parsed",
  "SourceForm", "The form the reference says the exhibit was filed under.", "where the reference named none",
  "SourceFiledOn", "The day that filing was made, as the reference gives it.", "where the reference named none",
  "ReleaseDate", "When the granted treatment lapses.", "where the letter gave none or gave a typo",
  "HashIndex", "The filing 01E resolved the reference to.", "where the filing was not found",
  "DocID", "The contract the reference resolved to. Joins to Contracts on DocID.", "where the chain broke",
  "LinkStatus", "A-linked, or the numbered step at which the chain broke.", "never"
)

#' Places: every place a contract names, with the party it belongs to
#'
#' THE ROWS BEHIND export_GPE()'S COUNTS. That cache says how many distinct states and countries a
#' contract names, per role; this file says which, so 30 can put them on a map. Same two stores, same
#' filter on the attachment, one difference: law-clause places are kept and flagged rather than
#' dropped, because the governing-law map is a map too and the flag is what separates the two.
#'
#' ONE ROW PER CONTRACT, PLACE, ROLE AND CLAUSE FLAG, with the mention count beside it. The distinct
#' is on the geography and not on the name -- "New York" and "New York, N.Y." are one row -- and the
#' count says how often the row's place was mentioned, so a map of contracts and a map of mentions
#' come from the same file.
#'
#' ONE ROLE PER PARTY. export_ORG() counts a party under every role it holds; a place wants one owner,
#' and .exp_place_roles says which wins.
#'
#' THE COUNTRY NAME IS FILLED FOR THE US TIERS. places_geo leaves GeoCountry null on a recovered
#' subdivision -- a state, a county, a city -- and a map wants a name on every row; USA is the only
#' code the store writes without one.
#'
#' CUT TO THE SAMPLE. The store covers every contract 04D processed; this file keeps the ones in the
#' release, and that join is also where HashDocument comes from.
#'
#' @param .dir_in 04D's release directory for the GPE pass, including its policy hash.
#' @param .dir_org 04D's release directory for the ORG pass; supplies the party roles.
#' @param .path_sample The sample cache; supplies HashDocument and the cut.
#' @param .path_out Destination parquet; the codebook is written beside it.
#' @param .rerun Logical. TRUE rebuilds regardless of the cache check.
#' @return Invisibly the written table.
export_places <- function(.dir_in, .dir_org, .path_sample, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .dir_in      <- .lP$Input$DirEntityGPE
    .dir_org     <- .lP$Input$DirEntityORG
    .path_sample <- .lP$Cache$CacheSample
    .path_out    <- .lP$Output$FilPlaces
    .rerun       <- FALSE
  }

  if (exp_cache_hit(.task = "Places", .path_out = .path_out, .paths_in = c(.dir_in, .dir_org, .path_sample),
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "Places: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  ds_plc <- apl_dataset(.dir = .dir_in,  .stem = "places_geo")
  ds_org <- apl_dataset(.dir = .dir_org, .stem = "org_mentions")
  if (is.null(ds_plc) || is.null(ds_org)) {
    cli::cli_abort(c(
      "Places needs both a places_geo and an org_mentions release.",
      "x" = "places_geo under {.path {as.character(.dir_in)}}: {!is.null(ds_plc)}.",
      "x" = "org_mentions under {.path {as.character(.dir_org)}}: {!is.null(ds_org)}."
    ))
  }

  spine_ <- arrow::read_parquet(file = .path_sample) |>
    dplyr::filter(as.logical(.data$PrimaryFiler)) |>
    dplyr::select("DocID", "HashDocument")

  con_ <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con_, shutdown = TRUE), add = TRUE)

  # SEVEN OF places_geo's EIGHTEEN COLUMNS, projected in arrow; the surface form and the offsets never
  # leave the parquet.
  src_plc <- ds_plc |>
    dplyr::select("DocID", "PartyKey", "GeoLevel", "GeoState", "GeoCountryIso", "GeoCountry", "InLawClause") |>
    arrow::to_duckdb(con = con_, auto_disconnect = FALSE)

  # ONE ROLE PER PARTY, the earliest in .exp_place_roles. A role the vocabulary does not know ranks
  # after every one it does and is released as "other" rather than dropped; every step is a group-by
  # or a join, so it runs in DuckDB without a window function.
  rank_ <- tibble::tibble(PartyRole = .exp_place_roles, RoleRank = seq_along(.exp_place_roles))
  role_ <- ds_org |>
    dplyr::select("DocID", "PartyKey", "PartyRole") |>
    dplyr::filter(!is.na(.data$PartyKey)) |>
    arrow::to_duckdb(con = con_, auto_disconnect = FALSE) |>
    dplyr::distinct(.data$DocID, .data$PartyKey, .data$PartyRole) |>
    dplyr::left_join(rank_, by = dplyr::join_by(PartyRole), copy = TRUE) |>
    dplyr::mutate(RoleRank = dplyr::coalesce(.data$RoleRank, 99L)) |>
    dplyr::group_by(.data$DocID, .data$PartyKey) |>
    dplyr::summarise(RoleRank = min(.data$RoleRank, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(rank_, by = dplyr::join_by(RoleRank), copy = TRUE) |>
    dplyr::mutate(PartyRole = dplyr::coalesce(.data$PartyRole, "other")) |>
    dplyr::select("DocID", "PartyKey", "PartyRole")

  # THE LEFT JOIN KEEPS THE UNATTACHED PLACE, which arrives with a null role and becomes "none".
  long_ <- src_plc |>
    dplyr::left_join(role_, by = dplyr::join_by(DocID, PartyKey)) |>
    dplyr::mutate(PartyRole = dplyr::coalesce(.data$PartyRole, "none")) |>
    dplyr::group_by(
      .data$DocID, .data$GeoLevel, .data$GeoState, .data$GeoCountryIso, .data$GeoCountry, .data$PartyRole,
      .data$InLawClause
    ) |>
    dplyr::summarise(nMentions = dplyr::n(), .groups = "drop") |>
    dplyr::collect()

  out_ <- long_ |>
    dplyr::inner_join(spine_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      GeoCountry  = dplyr::if_else(
        is.na(.data$GeoCountry) & !is.na(.data$GeoCountryIso) & .data$GeoCountryIso == "USA",
        "United States", .data$GeoCountry
      ),
      InLawClause = as.integer(dplyr::coalesce(.data$InLawClause, FALSE)),
      nMentions   = as.integer(.data$nMentions)
    ) |>
    dplyr::select(dplyr::all_of(.exp_dictionary_places$Column)) |>
    dplyr::arrange(.data$DocID, .data$PartyRole, .data$GeoCountryIso, .data$GeoState, .data$GeoLevel)

  exp_write_long(.tab = out_, .path_out = .path_out, .dict = .exp_dictionary_places, .dta = FALSE)

  n_law_ <- sum(out_$InLawClause == 1L)
  cli::cli_alert_success(
    "Places: {format(nrow(out_), big.mark = ',')} place rows on \\
     {format(dplyr::n_distinct(out_$DocID), big.mark = ',')} contracts; \\
     {format(n_law_, big.mark = ',')} of them inside a law clause; \\
     {format(nrow(long_) - nrow(out_), big.mark = ',')} {cli::qty(nrow(long_) - nrow(out_))}row{?s} on contracts \\
     outside the release dropped."
  )
  invisible(out_)
}

#' TermDocs: 05A's term hits per contract and family, cut to the release
#'
#' NOTHING IS RECOMPUTED. 05A holds the hits per term and folds them into families in
#' txt_doc_table(); this file takes that table for the Exhibit 10 source, keeps the contracts in the
#' release, and adds HashDocument. The names are 05A's, so its documentation applies unchanged.
#'
#' EVERY SCANNED CONTRACT HAS FIVE ROWS, one per family, with zeros where nothing hit. That is what
#' makes a prevalence over this file a prevalence: the denominator is the contracts 05A read, not the
#' ones that happened to hit.
#'
#' @param .path_in 05A's term_docs.parquet.
#' @param .path_sample The sample cache; supplies HashDocument and the cut.
#' @param .path_out Destination parquet; the codebook is written beside it.
#' @param .rerun Logical.
#' @return Invisibly the written table.
export_term_docs <- function(.path_in, .path_sample, .path_out, .rerun = FALSE) {
  if (FALSE) {
    .path_in     <- .lP$Input$FilTermDocs
    .path_sample <- .lP$Cache$CacheSample
    .path_out    <- .lP$Output$FilTermDocs
    .rerun       <- FALSE
  }

  if (exp_cache_hit(.task = "TermDocs", .path_out = .path_out, .paths_in = c(.path_in, .path_sample),
                    .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "TermDocs: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  spine_ <- arrow::read_parquet(file = .path_sample) |>
    dplyr::filter(as.logical(.data$PrimaryFiler)) |>
    dplyr::select("DocID", "HashDocument")

  cols_ <- c("DocID", "Source", "Family", "nHits", "nTerms", "HasTerm", "FirstPos", "PerKWords")
  ds_ <- arrow::open_dataset(sources = .path_in)
  miss_ <- setdiff(cols_, names(ds_))
  if (length(miss_) > 0L) {
    cli::cli_abort("05A's term_docs is missing {.val {miss_}}; the export reads exactly {.val {cols_}}.")
  }

  raw_ <- ds_ |>
    dplyr::filter(.data$Source == "Exhibit10") |>
    dplyr::select(dplyr::all_of(cols_)) |>
    dplyr::collect() |>
    dplyr::mutate(Family = as.character(.data$Family))

  out_ <- raw_ |>
    dplyr::inner_join(spine_, by = dplyr::join_by(DocID)) |>
    dplyr::mutate(
      dplyr::across(c("nHits", "nTerms", "HasTerm"), as.integer),
      dplyr::across(c("FirstPos", "PerKWords"), as.numeric)
    ) |>
    dplyr::select(dplyr::all_of(.exp_dictionary_termdocs$Column)) |>
    dplyr::arrange(.data$DocID, .data$Family)

  if (anyDuplicated(out_[, c("DocID", "Family")]) > 0L) {
    cli::cli_abort("TermDocs is not one row per contract and family.")
  }

  exp_write_long(.tab = out_, .path_out = .path_out, .dict = .exp_dictionary_termdocs, .dta = FALSE)

  n_drop_ <- dplyr::n_distinct(raw_$DocID) - dplyr::n_distinct(out_$DocID)
  cli::cli_alert_success(
    "TermDocs: {format(dplyr::n_distinct(out_$DocID), big.mark = ',')} contracts, \\
     {dplyr::n_distinct(out_$Family)} families, {format(nrow(out_), big.mark = ',')} rows; \\
     {format(n_drop_, big.mark = ',')} scanned {cli::qty(n_drop_)}contract{?s} outside the release dropped."
  )
  invisible(out_)
}

#' CtoOrders: every order reference 01E parsed, linked or not
#'
#' THE ORDER SIDE OF THE REDACTION STORY. Contracts says whether a contract is covered; this file
#' says what the SEC issued: how many orders, how many exhibits each named, how many of those
#' resolved to a contract in the release and where the chain broke for the rest. export_cto() keeps
#' the grants and collapses to the contract; this keeps everything and collapses nothing, so the
#' denials and the unlinked references are countable.
#'
#' 01E's COLUMNS, RENAMED WHERE TWO FILES WOULD OTHERWISE DISAGREE. DocID means the contract in every
#' release, so the order's own document is OrderDocID and its filing date OrderDate; the reference's
#' DocIDContract becomes DocID, which is the join key 30 uses everywhere. SourceForm and SourceFiledOn
#' are 01E's UseForm and UseFiledOn: the filing the reference itself named, falling back to the
#' opening paragraph's.
#'
#' @param .path_in 01E's CtoExhibits.parquet, one row per reference.
#' @param .path_out Destination parquet; the dta and codebook are written beside it.
#' @param .rerun Logical.
#' @param .dta Logical. Also write CtoOrders.dta.
#' @return Invisibly the written table.
export_cto_orders <- function(.path_in, .path_out, .rerun = FALSE, .dta = TRUE) {
  if (FALSE) {
    .path_in  <- .lP$Input$FilCtoExhibits
    .path_out <- .lP$Output$FilCtoOrders
    .rerun    <- FALSE
    .dta      <- TRUE
  }

  if (exp_cache_hit(.task = "CtoOrders", .path_out = .path_out, .paths_in = .path_in, .rerun = .rerun)) {
    out_ <- arrow::read_parquet(file = .path_out)
    cli::cli_alert_info(
      "CtoOrders: {.path {as.character(fs::path_file(.path_out))}} already written \\
       ({format(nrow(out_), big.mark = ',')} rows, {ncol(out_)} columns). Pass .rerun = TRUE to rebuild."
    )
    return(invisible(out_))
  }

  take_ <- c(
    OrderDocID = "DocID", OrderCIK = "CIK", OrderDate = "DateFiled", Status = "Status",
    IsExtension = "IsExtension", ExhibitNo = "ExhibitNo", SourceForm = "UseForm",
    SourceFiledOn = "UseFiledOn", ReleaseDate = "ReleaseDate", HashIndex = "HashIndex",
    DocID = "DocIDContract", LinkStatus = "LinkStatus"
  )
  ds_ <- arrow::open_dataset(sources = .path_in)
  miss_ <- setdiff(unname(take_), names(ds_))
  if (length(miss_) > 0L) {
    cli::cli_abort("01E's CtoExhibits is missing {.val {miss_}}; the export reads exactly {.val {unname(take_)}}.")
  }

  out_ <- ds_ |>
    dplyr::select(dplyr::all_of(unname(take_))) |>
    dplyr::collect() |>
    dplyr::rename(dplyr::all_of(take_)) |>
    dplyr::mutate(
      IsExtension = as.integer(.data$IsExtension),
      OrderDate   = as.Date(.data$OrderDate),
      ReleaseDate = as.Date(.data$ReleaseDate)
    ) |>
    dplyr::select(dplyr::all_of(.exp_dictionary_cto_orders$Column)) |>
    dplyr::arrange(.data$OrderDate, .data$OrderDocID, .data$ExhibitNo)

  exp_write_long(.tab = out_, .path_out = .path_out, .dict = .exp_dictionary_cto_orders, .dta = .dta)

  n_ref_ <- sum(!is.na(out_$ExhibitNo))
  cli::cli_alert_success(
    "CtoOrders: {format(dplyr::n_distinct(out_$OrderDocID), big.mark = ',')} orders, \\
     {format(n_ref_, big.mark = ',')} references, {format(sum(!is.na(out_$DocID)), big.mark = ',')} linked \\
     to a contract; {format(sum(out_$Status != 'GRANTING', na.rm = TRUE), big.mark = ',')} rows from orders \\
     that did not grant."
  )
  invisible(out_)
}

#' Write a long release: parquet, codebook, and a dta where asked
#'
#' THE DICTIONARY IS THE GATE. A column in the table and not in the dictionary is undocumented, one
#' in the dictionary and not in the table describes nothing; either aborts, so the codebook written
#' beside the file can never describe a different one.
#'
#' @param .tab The table to write.
#' @param .path_out Destination parquet.
#' @param .dict Tibble: Column, Meaning, Missing.
#' @param .dta Logical. Also write the dta.
#' @return Invisibly NULL.
exp_write_long <- function(.tab, .path_out, .dict, .dta = FALSE) {
  if (FALSE) {
    .tab      <- out_
    .path_out <- .lP$Output$FilPlaces
    .dict     <- .exp_dictionary_places
    .dta      <- FALSE
  }

  undoc_  <- setdiff(names(.tab), .dict$Column)
  unseen_ <- setdiff(.dict$Column, names(.tab))
  if (length(undoc_) > 0L || length(unseen_) > 0L) {
    cli::cli_abort(c(
      "{.file {fs::path_file(.path_out)}} and its dictionary disagree.",
      "x" = "In the file, not the dictionary: {.val {undoc_}}.",
      "x" = "In the dictionary, not the file: {.val {unseen_}}."
    ))
  }

  fs::dir_create(fs::path_dir(.path_out))
  arrow::write_parquet(.tab, .path_out)
  sta_ <- exp_stata_ready(.tab = .tab)
  if (.dta) haven::write_dta(sta_$Tab, fs::path_ext_set(.path_out, "dta"))
  tibble::tibble(Column = names(.tab), StataName = sta_$Names$Stata) |>
    dplyr::left_join(.dict, by = dplyr::join_by(Column)) |>
    readr::write_csv(fs::path_ext_set(paste0(fs::path_ext_remove(.path_out), "_Codebook"), "csv"))
  invisible(NULL)
}


# 10. Reporting -----------------------------------------------------------------------------------------------------------
#
# EVERY REPORT READS A RETURNED TABLE AND NOTHING ELSE. The exports above cache on the file's
# existence, so anything they printed themselves appeared on a cold render and vanished on a warm
# one -- and the rendered document, which is the artifact a reader opens, said nine times that a file
# was already written and nothing about what was in it.
#
# So the diagnostics live here, driven off tab_EntityORG and its siblings. A cached render and a cold
# render now say exactly the same thing, because both have the same tables in hand.
#
# THREE OF THESE ARE GENERIC AND ONE IS NOT. A numeric summary and a level breakdown serve every
# task; the column audit needs the register, because "what did the sample leave behind" is a question
# only the register can answer.

#' Every numeric and date column of one exported table, summarised
#'
#' NonMiss AND Pct ARE THE POINT, not the mean. The question a reader has about an exported column is
#' first whether it reaches enough contracts to use, and only then what it looks like. A mean over the
#' non-missing tells you nothing about a variable that reaches a fifth of the corpus.
#'
#' DATES ARE INCLUDED AND FORMATTED AS DATES. A start date whose minimum is 1900 or whose maximum is
#' next century is the fastest thing to spot and the easiest to miss, and excluding them from the one
#' table anyone reads would hide it.
#'
#' EVERYTHING IS RETURNED AS CHARACTER except the counts, because one column has to hold a formatted
#' date beside a formatted amount. These tables are for reading, which is also why anything at or
#' above 1e15 -- the point past which a double stops holding consecutive integers -- prints in
#' scientific notation rather than spelled out.
#'
#' .ceiling COUNTS WHAT EXCEEDS IT, which is how the money defect becomes visible without a function
#' of its own: NAbove on MoneyMaxUSD is the tail of 04B4's digit-run concatenation.
#'
#' @param .tab An exported table.
#' @param .ceiling Numeric or NULL. Adds NAbove, the count above this value, to every numeric column.
#' @return Tibble: one row per numeric or date column.
exp_table_numeric <- function(.tab, .ceiling = NULL) {
  if (FALSE) {
    .tab     <- tab_EntityORG
    .ceiling <- NULL
  }

  keep_ <- names(.tab)[purrr::map_lgl(.tab, \(.x) is.numeric(.x) || inherits(.x, "Date"))]
  if (length(keep_) == 0L) {
    cli::cli_abort("This table carries no numeric or date column to summarise.")
  }

  # ABOVE 2^53 A DOUBLE NO LONGER HOLDS CONSECUTIVE INTEGERS, so every digit past the fifteenth is an
  # artefact of the representation rather than a measurement. Spelling them out is how one money
  # column carrying a 295-digit mean made every other column of this table unreadable.
  say_ <- function(.x, .v) {
    if (length(.x) == 0L || is.na(.x) || !is.finite(.x)) return(NA_character_)
    if (inherits(.v, "Date")) return(format(as.Date(.x, origin = "1970-01-01"), "%Y-%m-%d"))
    if (abs(.x) >= 1e15) return(format(.x, scientific = TRUE, digits = 3L))
    tbl_num(.x = .x, .digits = if (max(abs(.v), na.rm = TRUE) > 1000) 0L else 2L)
  }

  purrr::map(keep_, function(.c) {
    v_ <- .tab[[.c]]
    n_ <- sum(!is.na(v_))
    num_ <- if (inherits(v_, "Date")) as.numeric(v_) else v_

    tibble::tibble(
      Column  = .c,
      NonMiss = n_,
      Pct     = n_ / max(nrow(.tab), 1L),
      Median  = if (n_ == 0L) NA_character_ else say_(stats::median(num_, na.rm = TRUE), v_),
      Mean    = if (n_ == 0L || inherits(v_, "Date")) NA_character_ else
        say_(mean(num_, na.rm = TRUE), v_),
      Min     = if (n_ == 0L) NA_character_ else say_(min(num_, na.rm = TRUE), v_),
      Max     = if (n_ == 0L) NA_character_ else say_(max(num_, na.rm = TRUE), v_),
      NAbove  = if (is.null(.ceiling) || inherits(v_, "Date")) NA_integer_ else
        sum(num_ > .ceiling, na.rm = TRUE)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::select(dplyr::where(\(.x) !all(is.na(.x))))
}


#' What one exported table holds, reported
#' @param .tab Tibble from exp_table_numeric().
#' @param .title Character. The heading.
#' @param .notes Named character passed to tbl_out().
#' @return Invisibly .tab.
exp_report_numeric <- function(.tab, .title, .notes = NULL) {
  if (FALSE) {
    .tab   <- tab_num
    .title <- "Parties"
    .notes <- NULL
  }

  tbl_head(.text = .title)
  .tab |>
    dplyr::mutate(
      NonMiss = tbl_num(.x = .data$NonMiss, .digits = 0L),
      dplyr::across(dplyr::any_of("NAbove"), \(.x) tbl_num(.x = .x, .digits = 0L))
    ) |>
    tbl_out(
      .title = NULL,
      .pct   = "Pct",
      .notes = c(
        Pct = "Share of the rows in THIS file, not of the corpus. What each file's population is
               belongs to the prose above it.",
        .notes
      )
    )
  invisible(.tab)
}


#' How one categorical column distributes
#'
#' A MISSING VALUE IS A LEVEL. "(missing)" appears in the table rather than being dropped, because on
#' several of these columns the missing share is the finding: a duration with no source, a contract
#' whose law clause named no place.
#'
#' @param .tab An exported table.
#' @param .col Character. The column to break down.
#' @param .top Integer. Levels shown, commonest first.
#' @return Tibble: Level, N, Pct.
exp_table_levels <- function(.tab, .col, .top = 15L) {
  if (FALSE) {
    .tab <- tab_EntityDATE
    .col <- "DurationSource"
    .top <- 15L
  }

  if (!.col %in% names(.tab)) cli::cli_abort("{(.col)} is not a column of this table.")

  tibble::tibble(Level = dplyr::coalesce(as.character(.tab[[.col]]), "(missing)")) |>
    dplyr::count(.data$Level, name = "N") |>
    dplyr::mutate(Pct = .data$N / max(nrow(.tab), 1L)) |>
    dplyr::arrange(dplyr::desc(.data$N)) |>
    utils::head(n = .top)
}


#' How one categorical column distributes, reported
#' @param .tab Tibble from exp_table_levels().
#' @param .title Character. The heading.
#' @param .notes Named character passed to tbl_out().
#' @return Invisibly .tab.
exp_report_levels <- function(.tab, .title, .notes = NULL) {
  if (FALSE) {
    .tab   <- tab_lev
    .title <- "Which rung decided the duration"
    .notes <- NULL
  }

  tbl_head(.text = .title)
  .tab |>
    dplyr::mutate(N = tbl_num(.x = .data$N, .digits = 0L)) |>
    tbl_out(.title = NULL, .pct = "Pct", .notes = .notes)
  invisible(.tab)
}


#' Which register columns the sample carries, and which it leaves behind
#'
#' THE ONE AUDIT THE RETURNED TABLE CANNOT DO ALONE. Everything else here reads an exported file; this
#' asks the register what it holds and differences the two, so a column that ought to be released
#' surfaces on every render rather than only on the one where the sample was rebuilt.
#'
#' THE SCHEMA IS READ FROM ARROW AND NOTHING IS COLLECTED. names() on an open dataset is metadata.
#'
#' @param .tab The sample table.
#' @param .path_register 02B's Documents.parquet.
#' @return Tibble: Column, Status.
exp_table_columns <- function(.tab, .path_register) {
  if (FALSE) {
    .tab           <- tab_Sample
    .path_register <- .lP$Input$FilSample
  }

  have_ <- names(arrow::open_dataset(sources = .path_register))

  # A COLUMN THE EXPORT ADDED IS NOT ABSENT. RemClass is named in .exp_sample_cols, is not in the
  # register, and is joined in from 01C -- so the third branch has to exclude what the table actually
  # carries, or it would report one column under two statuses.
  dplyr::bind_rows(
    tibble::tibble(Column = names(.tab), Status = "exported"),
    tibble::tibble(Column = setdiff(have_, names(.tab)), Status = "left behind"),
    tibble::tibble(
      Column = setdiff(.exp_sample_cols, c(have_, names(.tab))), Status = "named but absent"
    )
  ) |>
    dplyr::arrange(.data$Status, .data$Column)
}


#' What the sample took and what it left, reported
#'
#' "left behind" IS A REPORT AND NOT A FAULT, and the report is the point: the list has changed three
#' times and a prose count of it was wrong within a month of being written. Group is constant once
#' the register is cut to one document type, DocType and YQ are components of a machine path, and
#' HasSummary and Item101Outcome describe an 8-K report rather than a contract. Anything else in that
#' list is a decision to revisit. "named but absent" IS a fault: .exp_sample_cols names a column the
#' register does not have.
#'
#' @param .tab Tibble from exp_table_columns().
#' @return Invisibly .tab.
exp_report_columns <- function(.tab) {
  if (FALSE) .tab <- tab_cols

  tbl_head(.text = "The register's columns, exported or not")
  .tab |>
    dplyr::summarise(
      Columns = dplyr::n(),
      Which   = paste(.data$Column, collapse = ", "),
      .by     = Status
    ) |>
    tbl_out(
      .title = NULL,
      .notes = c(
        Which = "The count is not asserted here, because it moved once already while a note still
                 said six. Everything left behind is left for one of three reasons. Group, DocType
                 and YQ are constant or are components of a machine path. HasSummary and
                 Item101Outcome are properties of an 8-K report document and this file holds none.
                 DocSize, UrlFullText, nImgs, nWordsFile, pNums and pStopLong are measurements the
                 released set already covers in another form -- nChars, nWords, pStopShort -- or,
                 for UrlFullText, a link to a submission blob that runs to tens of megabytes.
                 Anything beyond those is a decision to revisit; anything under 'named but absent'
                 is a fault in .exp_sample_cols."
      )
    )

  bad_ <- dplyr::filter(.tab, .data$Status == "named but absent")
  if (nrow(bad_) > 0L) {
    cli::cli_alert_warning(
      "{nrow(bad_)} named {cli::qty(nrow(bad_))}column{?s} {?is/are} not in the register: \\
       {paste(bad_$Column, collapse = ', ')}."
    )
  }
  invisible(.tab)
}


#' Every cache against the joined file
#'
#' THE RECONCILIATION. Each cache's row count is the population of the pass that fed it, and the
#' joined file's coverage is what survived the fan-out. Two numbers that differ for a reason are more
#' informative than two that agree, and this is where the reason has to be legible.
#'
#' THE MARKER IS DERIVED, NOT DECLARED: the first column of each cache that is not a key. A block is
#' present in a row of the joined file where its marker is not missing. That way a column added to a
#' cache cannot make this table stale.
#'
#' @param .tabs Named list of the cache tables, Sample first.
#' @param .final The joined table.
#' @return Tibble: one row per cache.
exp_table_files <- function(.tabs, .final) {
  if (FALSE) {
    .tabs  <- list(Sample = tab_Sample, ORG = tab_EntityORG)
    .final <- tab_Contracts
  }

  # HashIndex IS A KEY TOO, and leaving it out of this list is not cosmetic: the marker is derived
  # as the first non-key column, so the items cache reported HashIndex as its marker and 100% of the
  # file as carrying the block. Every row carries a HashIndex; only some carry items.
  keys_ <- c("DocID", "HashDocument", "HashIndex")

  purrr::map(names(.tabs), function(.n) {
    tab_ <- .tabs[[.n]]

    # THE MARKER IS DECLARED, AND ONLY GUESSED WHERE NOTHING DECLARES IT. Deriving it as the first
    # non-key column made this table disagree with the grain table two rows apart, which is the
    # worst kind of wrong: both are printed in the same document and neither says it is a guess.
    said_   <- .exp_blocks$Marker[match(.n, .exp_blocks$Cache)]
    marker_ <- if (!is.na(said_) && said_ %in% names(tab_)) said_ else setdiff(names(tab_), keys_)[[1L]]
    in_     <- if (marker_ %in% names(.final)) sum(!is.na(.final[[marker_]])) else NA_integer_

    tibble::tibble(
      File    = .n,
      Rows    = nrow(tab_),
      Cols    = ncol(tab_),
      Marker  = marker_,
      InFinal = in_,
      PctFinal = in_ / max(nrow(.final), 1L)
    )
  }) |>
    purrr::list_rbind()
}


#' The reconciliation, reported
#' @param .tab Tibble from exp_table_files().
#' @return Invisibly .tab.
exp_report_files <- function(.tab) {
  if (FALSE) .tab <- tab_files

  tbl_head(.text = "Every cache against the joined file")
  .tab |>
    dplyr::mutate(dplyr::across(c("Rows", "InFinal"), \(.x) tbl_num(.x = .x, .digits = 0L))) |>
    tbl_out(
      .title = NULL,
      .pct   = "PctFinal",
      .notes = c(
        Rows     = "Attachment grain for the six entity caches, registrant-copy grain for the sample
                    and the classification.",
        InFinal  = "Rows of the joined file carrying this block. Larger than Rows for an entity
                    cache, because the fan-out gives every copy of an attachment its values.",
        Marker   = "The first non-key column of the cache; a block counts as present where it is not
                    missing."
      )
    )
  invisible(.tab)
}


#' Identities the joined file cannot violate
#'
#' FOUR THAT MUST HOLD AND THREE THAT ARE MEASUREMENTS. The row count must be the sample's, no DocID
#' may repeat, and every row must carry a HashDocument -- those are faults if they fail. How many
#' rows are primary copies, and how many carry no entity block at all, are facts about the corpus.
#'
#' A ROW WITH NO ENTITY BLOCK IS A CONTRACT 04D DID NOT REACH, not a co-filer copy: the fan-out gave
#' every copy of an extracted attachment its values, so an empty row means the attachment itself was
#' never read.
#'
#' THE COLUMN CHECK IS THE ONE THAT CATCHES A STALE JOIN. Row counts cannot: the spine is the sample
#' either way, so a join built from an older sample has exactly the right number of rows and passes
#' every count above. This document has already produced that state once -- the sample gained a column
#' and lost three, the joined file kept the old set, and nothing in the render said so. Differencing
#' the caches' column names against the joined file's is what would have.
#'
#' @param .tab The joined table.
#' @param .tabs Named list of the cache tables; supplies the columns the join must carry.
#' @param .n_sample Integer. Rows in the sample cache.
#' @param .marker Character. A column that is present wherever an entity block is.
#' @return Tibble: one row per check.
exp_check_final <- function(.tab, .tabs, .n_sample, .marker = "nUniRegistrant") {
  if (FALSE) {
    .tab      <- tab_Contracts
    .tabs     <- list(Sample = tab_Sample, ORG = tab_EntityORG)
    .n_sample <- nrow(tab_Sample)
    .marker   <- "nUniRegistrant"
  }

  ent_  <- if (.marker %in% names(.tab)) sum(!is.na(.tab[[.marker]])) else NA_integer_
  prim_ <- sum(as.logical(.tab$PrimaryFiler), na.rm = TRUE)
  want_ <- unique(unlist(purrr::map(.tabs, names)))
  miss_ <- setdiff(want_, names(.tab))

  tibble::tibble(
    Check = c("Rows in the joined file", "Rows in the sample", "Duplicate DocID",
              "Rows with no HashDocument", "Columns the caches supply",
              "Columns in the joined file", "Cache columns missing from the join",
              "Primary copies", "Co-filer copies", "Rows carrying the entity blocks"),
    N     = c(nrow(.tab), .n_sample, sum(duplicated(.tab$DocID)), sum(is.na(.tab$HashDocument)),
              length(want_), ncol(.tab), length(miss_),
              prim_, nrow(.tab) - prim_, ent_),
    Want  = c("the sample's", "the sample's", "0", "0", "a measurement", "a measurement", "0",
              "a measurement", "a measurement", "a measurement"),
    Which = c(rep(NA_character_, 6L), if (length(miss_) == 0L) NA_character_ else
                paste(miss_, collapse = ", "), rep(NA_character_, 3L))
  )
}


#' The identities, reported
#' @param .tab Tibble from exp_check_final().
#' @return Invisibly .tab.
exp_report_final <- function(.tab) {
  if (FALSE) .tab <- tab_ident

  tbl_head(.text = "Identities the joined file cannot violate")
  .tab |>
    dplyr::mutate(N = tbl_num(.x = .data$N, .digits = 0L)) |>
    tbl_out(
      .title = NULL,
      .notes = c(
        Want  = "A nonzero count on any of the three zeros is a fault in the join, not a fact about
                 the corpus. The entity values REPEAT across copies of one attachment, so a corpus
                 mean over this file double-counts and wants PrimaryFiler == 1.",
        Which = "Which cache columns the joined file does not carry. Not empty means the join was
                 built from an older version of a cache, which the row counts cannot see."
      )
    )

  n_join_ <- .tab$N[.tab$Check == "Rows in the joined file"]
  n_samp_ <- .tab$N[.tab$Check == "Rows in the sample"]
  bad_    <- .tab |> dplyr::filter(.data$Want == "0", .data$N != 0)
  gone_   <- .tab$Which[.tab$Check == "Cache columns missing from the join"]

  if (!is.na(gone_)) {
    cli::cli_abort(c(
      "The joined file does not carry every column its caches hold.",
      "x" = "Missing: {(gone_)}.",
      "i" = "A cache was rebuilt and the join was not. Delete the joined file, or pass
             .rerun = TRUE to export_final()."
    ))
  }

  if (!identical(n_join_, n_samp_)) {
    cli::cli_abort(c(
      "The join changed the row count.",
      "x" = "{format(n_join_, big.mark = ',')} rows against the sample's \\
             {format(n_samp_, big.mark = ',')}.",
      "i" = "Every join is a left join from the sample, so this cannot happen without a duplicate
             key on the right of one of them."
    ))
  }
  if (nrow(bad_) > 0L) {
    cli::cli_abort(c(
      "{nrow(bad_)} {cli::qty(nrow(bad_))}identit{?y/ies} {?is/are} violated.",
      "x" = "{paste(bad_$Check, collapse = '; ')}."
    ))
  }
  invisible(.tab)
}


# 11. The column dictionary -----------------------------------------------------------------------------------------------
#
# WHAT EACH COLUMN OF Contracts MEANS, AND WHAT A MISSING VALUE MEANS IN IT. The second is the part
# nobody can reconstruct from the data: a missing DurationYears and a missing MoneyMaxUSD are
# different states arrived at for different reasons, and neither is visible in the file.
#
# THE JOIN IS THE UNION OF THE EIGHT CACHES, so documenting it documents all of them. A column keeps
# its meaning whichever file it is read from.
#
# TYPE AND STATA NAME ARE NOT DECLARED HERE. They are read off the file and joined, so the dictionary
# cannot claim a type the data does not have. exp_table_dictionary() aborts where the declaration and
# the file disagree about which COLUMNS exist, which is the drift that actually happens: a column
# added upstream and documented nowhere.

#: THE EIGHT BLOCKS, and the three facts that are true of a whole block rather than of one column.
#:
#: Cache NAMES THE FILE EACH BLOCK ARRIVES IN, so exp_table_files() can take the marker from here
#: rather than guessing it. It guessed it as the first non-key column, which disagreed with this
#: table in two places at once: the orders cache led with nCtoOrders, which the join coalesces to
#: zero and which therefore reported every contract as carrying the block, and the sample led with
#: CIK once HashIndex was recognised as a key. One declaration, read by both.
#:
#: Marker is the column that says whether the block reached a row at all. In the joined file NA means
#: two different things and no column distinguishes them: the contract is not in that cache, or it is
#: and this particular value does not exist. Marker separates them -- where it is missing the whole
#: block is absent, and every other NA in the block is the second kind.
.exp_blocks <- tibble::tribble(
  ~Block, ~Cache, ~Grain, ~Population, ~Marker,
  "Sample", "Sample", "registrant copy",
  "Every Exhibit 10 attachment on EDGAR, once per registrant that filed it. Nothing is excluded.",
  "HashIndex",

  "Items", "Items", "parent filing, repeated across its exhibits",
  "Contracts whose 8-K declares an item list. Filings with no item structure -- a 10-K, an S-1 --
   reach no row here, which is why the marker is missing rather than zero.",
  "nItems",

  "Summary", "Summary", "parent filing, repeated across its exhibits",
  "Contracts on an 8-K whose Item 1.01 narrative 01D recovered. Missing on a 10-K, and on an 8-K
   whose summary could not be parsed; SumWords is the marker.",
  "SumWords",

  "Orders", "Orders", "registrant copy",
  "Every contract: nCtoOrders is a count over all of them, zero where the SEC granted no
   confidential treatment. The release dates carry the absence instead.",
  "CtoReleaseFirst",

  "Classification", "Classification", "registrant copy",
  "Attachments 03F could label, fanned out to their copies. 741 copies short of the sample.",
  "BertClassDetailed",

  "Parties", "ORG", "attachment, repeated across copies",
  "Every contract 04D read. 04B1 writes a sentinel where no organisation was found, so the block
   reaches all of them.",
  "nUniSpellingsNaive",

  "Geography", "GPE", "attachment, repeated across copies",
  "Contracts naming a place outside a governing-law clause. About seven in eight.",
  "nUniStateNaive",

  "Law", "LAW", "attachment, repeated across copies",
  "Contracts carrying at least one governing-law clause. About three quarters.",
  "nLawClause",

  "Dates", "DATE", "attachment, repeated across copies",
  "Contracts naming a parsed date OR a stated term. Either is enough to place them here.",
  "DateStart",

  "Money", "MONEY", "attachment, repeated across copies",
  "Contracts in which the extractor found any money figure, withheld or not. About seven in ten.",
  "nUniAmountNaive",

  "Redactions", "REDACT", "attachment, repeated across copies",
  "Contracts carrying at least one redaction marker. About one in five; the rest redact nothing.",
  "nRedactExplicit"
)

#: ONE ROW PER COLUMN OF Contracts. Meaning is what it holds; Missing is what an NA in it says.
#:
#: "structural" IN Missing MEANS THE COLUMN IS NEVER MISSING WHERE ITS BLOCK REACHED THE ROW. A count
#: of zero is a measurement -- the extractor looked and found none -- and is not the same as absence.
.exp_dictionary <- tibble::tribble(
  ~Column, ~Block, ~Meaning, ~Missing,

  # -- Sample ---------------------------------------------------------------------------------------
  "DocID", "Sample",
  "The filing's accession number and a hash of the attachment. Unique in this file.",
  "never",

  "HashDocument", "Sample",
  "Identifies the attachment itself. Shared by every registrant copy of one document, and the key the
   entity blocks were fanned out on.",
  "never",

  "HashIndex", "Sample",
  "Identifies the filing the attachment arrived in. This is how a contract reaches its own 8-K.",
  "never",

  "CIK", "Sample",
  "The registrant's EDGAR central index key.",
  "never",

  "CompanyName", "Sample",
  "The registrant's name as EDGAR recorded it for this filing.",
  "never",

  "UrlDocument", "Sample",
  "Where the attachment sits on EDGAR. The link referee 2 asked for.",
  "never",

  "DocTypeRaw", "Sample",
  "The exhibit label as filed: EX-10.1 and its many variants.",
  "never",

  "DocTypeMod", "Sample",
  "That label reduced to its exhibit family.",
  "never",

  "FormType", "Sample",
  "The parent form the attachment came in: 8-K, 10-K, 10-Q and so on.",
  "never",

  "DocExt", "Sample",
  "The attachment's file extension, htm or txt.",
  "never",

  "DateFiled", "Sample",
  "When EDGAR accepted the filing. Coerced to a Date once, here, and read as one everywhere after.",
  "never",

  "nWords", "Sample",
  "Words in the flattened text.",
  "the document holds no readable text",

  "nWordsAdj", "Sample",
  "nWords less the words the file header contributed.",
  "the document holds no readable text",

  "nChars", "Sample",
  "Characters in the flattened text. Every offset in 04D's span files indexes into this.",
  "the document holds no readable text",

  "nNums", "Sample",
  "Numeric tokens in the text.",
  "the document holds no readable text",

  "pStopShort", "Sample",
  "Share of tokens that are short stopwords. A low value marks a document that is not prose.",
  "the document holds no readable text",

  "Removed", "Sample",
  "1 where 01C flagged the document as unusable. It is NOT dropped: a released sample that excluded
   the malformed ones would leave a later study unable to see what was lost.",
  "never",

  "RemClass", "Sample",
  "Which rule flagged it -- too short, not prose, mostly digits. Joined from 01C, because the register
   carries the flag without the reason.",
  "the document was not flagged",

  "nCIK", "Sample",
  "How many registrants the filing names.",
  "never",

  "MultFiler", "Sample",
  "1 where the filing names more than one registrant.",
  "never",

  "FilerCopiesAgree", "Sample",
  "1 where every copy of this attachment has the same length. 01C found one attachment of 1.49
   million where they do not, and there the entity values speak for one copy only.",
  "never",

  "PrimaryFiler", "Sample",
  "1 on the one copy 04C extracted. Every entity value on the other copies was computed from this
   one, so a corpus mean over this file wants PrimaryFiler == 1.",
  "never",

  "gvkey", "Sample",
  "Compustat firm identifier, matched on CIK and filing date.",
  "no Compustat quarter matched this filing",

  "datadate", "Sample",
  "The Compustat quarter end the filing was matched to.",
  "no Compustat quarter matched this filing",

  "cyear", "Sample",
  "Calendar year of datadate.",
  "no Compustat quarter matched this filing",

  "fyear", "Sample",
  "Fiscal year of datadate.",
  "no Compustat quarter matched this filing",

  "fqtr", "Sample",
  "Fiscal quarter of datadate.",
  "no Compustat quarter matched this filing",

  "SampleStepCode", "Sample",
  "Where the document sits on the sample ladder, as a sortable code.",
  "never",

  "SampleStepDesc", "Sample",
  "The same rung of the ladder in words.",
  "never",

  "DescSample", "Sample",
  "1 where the document is in the descriptive sample.",
  "never",

  "EstiSample", "Sample",
  "1 where the document is in the estimation sample.",
  "never",

  "DocName", "Sample",
  "EDGAR's own filename for the attachment. The third part of the cik-accession-filename
   combination a referee asked the release to carry.",
  "never",

  "DocSeq", "Sample",
  "Where the attachment sits in its filing's exhibit list. Orders exhibits within one 8-K.",
  "never",

  "UrlIndexPage", "Sample",
  "Link to the filing's index page on EDGAR, which shows every document it carried. UrlDocument
   opens this attachment alone.",
  "never",

  "DocDesc", "Sample",
  "The filer's own description of the exhibit, and the only human-written label an attachment
   carries. Free text: filers write what they like.",
  "where the filer supplied none",

  "HasCto", "Orders",
  "1 where a confidential treatment order granted treatment for this contract. Orders that denied or
   revoked treatment do not count.",
  "never",

  "nCtoOrders", "Orders",
  "How many granting CT orders name it. Extensions of an earlier grant count.",
  "never",

  "CtoReleaseFirst", "Orders",
  "Earliest release date across those orders. Missing where no order states one -- grants after 2019
   often carry no expiry -- and where 01E set a date aside as a typo in the letter (one case).",
  "no granting CT order names this contract, or none states a release date",

  "CtoReleaseLast", "Orders",
  "Latest release date across those orders. Missing on the same rows as CtoReleaseFirst.",
  "no granting CT order names this contract, or none states a release date",

  "CtoIsExtension", "Orders",
  "1 where any of those orders extends an earlier one.",
  "never",

  # -- Classification -------------------------------------------------------------------------------
  "Class", "Classification",
  "Contract type, twelve classes. A copy of BertClassDetailed, so nothing downstream needs to know
   which engine was crowned.",
  "03F could not label this attachment",

  "ClassBroad", "Classification",
  "Contract type, seven classes. A copy of BertClassBroad.",
  "03F could not label this attachment",

  "AmendType", "Classification",
  "Original or Amended. A copy of BertAmendType.",
  "03F could not label this attachment",

  # -- Items ----------------------------------------------------------------------------------------
  "nItems", "Items",
  "How many items the parent 8-K declares. The block marker: missing means the filing has no item
   structure at all, which is not the same as an 8-K reporting none.",
  "where the parent filing declares no items",

  "nItemsDotted", "Items",
  "How many of them are post-2004 dotted codes. Separates the two taxonomies without a date rule.",
  "structural",

  "SumWords", "Summary",
  "Words in the parent 8-K's Item 1.01 narrative, after the item heading and title are removed. The
   block marker: missing means no summary was recovered for the filing, or the filing is not an 8-K.",
  "where the parent filing is not an 8-K or its Item 1.01 could not be parsed",

  "SumSentences", "Summary",
  "Sentences in the narrative, by ICU sentence boundaries.",
  "structural",

  "SumComplex", "Summary",
  "Words of three or more syllables, by vowel-group count with a silent trailing e discounted.",
  "structural",

  "SumFog", "Summary",
  "Gunning Fog: 0.4 x (words per sentence + 100 x the share of complex words). Years of schooling
   to read it on first pass.",
  "structural",

  "SumUncertain", "Summary",
  "Words in the Loughran-McDonald Uncertainty list. A rate is SumUncertain / SumWords x 1000.",
  "structural",

  "SumNumbers", "Summary",
  "Numeric tokens of any kind in the narrative.",
  "structural",

  "SumDollars", "Summary",
  "Dollar figures: a currency mark, a number, an optional scale word.",
  "structural",

  "SumPercents", "Summary",
  "Percentages: a number and a percent sign.",
  "structural",

  "SumDates", "Summary",
  "Distinct agreement dates the narrative opens with. The paper's proxy for how many agreements it
   describes.",
  "structural",

  "SumIsSingle", "Summary",
  "SumDates == 1: the narrative describes one agreement. The paper's Table 7 sample.",
  "structural",

  "SumBoiler", "Summary",
  "Share of the narrative's four-word phrases that appear in at least one in a hundred distinct
   filers, after Lang and Stice-Lawrence (2015). House style is not boilerplate; the form is.",
  "structural",

  "SumLagDays", "Summary",
  "Filing date less the agreement date the narrative opens with, in calendar days, signed. Item 1.01
   allows four business days, which is at most six calendar days; a negative lag is a firm announcing
   before it signed.",
  "where the narrative names no agreement date",

  "SumAttached", "Summary",
  "Whether an Exhibit 10 rode on the same filing. One on every row of this release by construction,
   because every row is such an exhibit; carried so the block joins back to 01D's full population,
   where it splits attached from delayed.",
  "structural",

  "nItemsVoluntary", "Items",
  "Items the registrant chose to report: 2.02, 7.01 and 8.01 after the 2004 reform, and 12, 9 and 5
   before it, which are the same three under two numbering schemes. Counting only the dotted codes
   returns zero for every filing before 23 August 2004.",
  "structural",

  "nItemsMandatory", "Items",
  "Items triggered by an event outside the registrant's control, four business days.",
  "structural",

  "nItemsUnknown", "Items",
  "Items whose code is not in 01D's registered vocabulary. One 1996 filing, outside this sample.",
  "structural",

  "ItemEra", "Items",
  "Which taxonomy the filing numbers by: Post, Pre, or Mixed where it uses both. Assigned from the
   codes rather than the date; 41 filings in the corpus disagree with their own filing date.",
  "where no code could be classified",

  "ItemCodes", "Items",
  "Every item the filing declares, pipe-separated, in the order declared. Nothing is picked, so any
   other indicator is recomputable from it.",
  "structural",

  "ReportsItem101", "Items",
  "1 where the parent filing reports Item 1.01, entry into a material definitive agreement. NOT the
   same as the register's HasSummary, which says 01D recovered the narrative: a filing can report
   the item while the extraction fails.",
  "structural",

  "ReportsItem202", "Items",
  "1 where the filing reports results of operations: Item 2.02, or Item 12 before the reform.",
  "structural",

  "ReportsItem701", "Items",
  "1 where the filing reports a Regulation FD disclosure: Item 7.01, or Item 9 before the reform.",
  "structural",

  "ReportsItem801", "Items",
  "1 where the filing reports other events: Item 8.01, or Item 5 before the reform.",
  "structural",

  "ReportsItem901", "Items",
  "1 where the filing reports financial statements and exhibits: Item 9.01, or Item 7 before the
   reform. A contract-bearing 8-K that does not report it is worth a look.",
  "structural",

  "BertClassDetailed", "Classification",
  "The twelve-class label from legal-bert, which 03B crowned at 0.882 macro-F1.",
  "03F could not label this attachment",

  "BertClassDetailedProb", "Classification",
  "Softmax probability of that class.",
  "03F could not label this attachment",

  "BertClassDetailed2", "Classification",
  "The runner-up class.",
  "03F could not label this attachment",

  "BertClassDetailed2Prob", "Classification",
  "Its probability. The gap to the top probability is the model's confidence in the call.",
  "03F could not label this attachment",

  "KwClassDetailed", "Classification",
  "The same twelve classes from the keyword lexicon, which is the independent arm rather than a
   validation: it has no claim to be right where the two differ.",
  "the lexicon abstained: no term fired, or the top two tied",

  "KwClassDetailedPower", "Classification",
  "Score of the winning class in the lexicon.",
  "the lexicon abstained",

  "KwClassDetailedTerm", "Classification",
  "The term that carried it.",
  "the lexicon abstained",

  "ClassDetailedFlag", "Classification",
  "confirmed, contradicted or unchecked: whether the lexicon agreed with Bert, disagreed, or
   abstained.",
  "never",

  "BertClassBroad", "Classification",
  "The seven-class label from legal-bert.",
  "03F could not label this attachment",

  "BertClassBroadProb", "Classification",
  "Softmax probability of that class.",
  "03F could not label this attachment",

  "BertClassBroad2", "Classification",
  "The runner-up broad class.",
  "03F could not label this attachment",

  "BertClassBroad2Prob", "Classification",
  "Its probability.",
  "03F could not label this attachment",

  "KwClassBroad", "Classification",
  "The seven broad classes from the keyword lexicon.",
  "the lexicon abstained",

  "KwClassBroadPower", "Classification",
  "Score of the winning broad class in the lexicon.",
  "the lexicon abstained",

  "KwClassBroadTerm", "Classification",
  "The term that carried it.",
  "the lexicon abstained",

  "ClassBroadFlag", "Classification",
  "confirmed, contradicted or unchecked, for the broad task.",
  "never",

  "BertAmendType", "Classification",
  "Original or Amended, from legal-bert.",
  "03F could not label this attachment",

  "BertAmendTypeProb", "Classification",
  "Softmax probability of that call.",
  "03F could not label this attachment",

  "BertAmendType2", "Classification",
  "The other of the two, which is the complement rather than a second opinion.",
  "03F could not label this attachment",

  "BertAmendType2Prob", "Classification",
  "Its probability, which is one minus the first.",
  "03F could not label this attachment",

  "KwAmendType", "Classification",
  "Original or Amended from the keyword lexicon.",
  "the lexicon abstained",

  "KwAmendTypePower", "Classification",
  "Score of the winning call in the lexicon.",
  "the lexicon abstained",

  "KwAmendTypeTerm", "Classification",
  "The term that carried it.",
  "the lexicon abstained",

  "AmendTypeFlag", "Classification",
  "confirmed, contradicted or unchecked, for the amendment task.",
  "never",

  "HierConsistent", "Classification",
  "1 where the detailed class rolls up to the broad class the model chose independently. The two
   passes never see each other, so this is a free consistency check rather than a constraint.",
  "03F could not label this attachment",

  # -- Parties --------------------------------------------------------------------------------------
  "nUniSpellingsNaive", "Parties",
  "Distinct organisation names the extractor proposed, BEFORE grouping merged them into parties.
   8.18 per contract at corpus scale against 7.28 parties after grouping. The rung a reader who
   rejects the grouping recomputes from.",
  "structural",

  "nUniRegistrant", "Parties",
  "Distinct parties matched to the filing registrant. One where the match succeeded, zero where no
   organisation was found at all.",
  "structural",

  "nUniCofiler", "Parties",
  "Distinct parties matched to another registrant of the same filing.",
  "structural",

  "nUniCounterparty", "Parties",
  "Distinct parties in the window around the registrant that are not the registrant. 1.26 per
   contract at corpus scale.",
  "structural",

  "nUniSignatory", "Parties",
  "Distinct parties appearing only in the final tenth of the document.",
  "structural",

  "nUniOther", "Parties",
  "Distinct parties named outside the window and outside the signature block.",
  "structural",

  "HasNoParty", "Parties",
  "1 where the extractor found no organisation in the contract at all. Every count above is zero
   there, and nothing else in the file would say why.",
  "structural",

  # -- Geography ------------------------------------------------------------------------------------
  "nUniStateNaive", "Geography",
  "Distinct states named anywhere outside a governing-law clause, attached to a party or not.",
  "structural",

  "nUniCountryNaive", "Geography",
  "Distinct countries, on the same basis.",
  "structural",

  "nUniStateRegistrant", "Geography",
  "Distinct states attached to the registrant party. Above one means the extractor found several
   candidate places near one name, NOT that the contract has several registrants.",
  "structural",

  "nUniCountryRegistrant", "Geography",
  "Distinct countries attached to the registrant party.",
  "structural",

  "nUniStateCounterparty", "Geography",
  "Distinct states attached to any counterparty.",
  "structural",

  "nUniCountryCounterparty", "Geography",
  "Distinct countries attached to any counterparty.",
  "structural",

  # -- Law ------------------------------------------------------------------------------------------
  "LawJurisdiction", "Law",
  "Every jurisdiction the contract's law clauses named, pipe-separated in document order. NOTHING IS
   PICKED: the median contract with a clause names two, and no ordering of the candidates is
   defensible on evidence yet.",
  "the clauses named no place the gazetteer knew",

  "LawJurisdictionLevel", "Law",
  "State or Country for each, aligned element for element with LawJurisdiction.",
  "the clauses named no place",

  "LawKind", "Law",
  "Which cue named each, aligned element for element. GovernedBy and ConstruedAccordance occur only
   in a governing-law clause; LawsOfState also matches 'organized under the laws of the State of X',
   which is incorporation and is always earlier in the document. SubmitJurisdiction is forum, not
   governing law.",
  "the clauses named no place",

  "nLawClause", "Law",
  "How many law clauses were found, whether or not they named a place.",
  "structural",

  "nLawSelfRef", "Law",
  "How many name no place by construction: governed by the law of the state where the premises are.",
  "structural",

  "nUniJurisdiction", "Law",
  "Distinct places named. Above one means the contract named several and the reader has to choose;
   it differs from the number of pipes where one place was named by two different cues.",
  "structural",

  # -- Dates ----------------------------------------------------------------------------------------
  "DateStart", "Dates",
  "When the contract runs from: the latest date at or before the filing, and the filing date itself
   where there is none. Not capped -- extremes are spans the extractor really found.",
  "structural",

  "StartSource", "Dates",
  "signed where a date was found before the filing, filed where the start fell back to it.",
  "structural",

  "DateEnd", "Dates",
  "When it runs to, from whichever rung of the cascade answered. Not capped.",
  "no rung established an end",

  "DurationYears", "Dates",
  "DateEnd less DateStart in years, CAPPED AT 30. A longer duration is dropped rather than
   winsorised, because thirty years that is not thirty years is worse than a missing value.",
  "no end was established, or the duration was dropped -- DurationDropped says which",

  "DurationSource", "Dates",
  "Which rung answered: term (the contract states its own length, 56% of them), open (it states it
   has none), cue (a future date with MATURIT, THROUGH or EXPIR beside it), maxdate (the farthest
   future date, for no stated reason), none.",
  "structural",

  "DurationDropped", "Dates",
  "kept, negative, capped, or no end. The reason DurationYears is missing where it is.",
  "structural",

  "TermYears", "Dates",
  "The LONGEST stated term, in years. A contract stating a five-year term and a thirty-day cure
   period should not be described by the cure period. Not capped.",
  "the contract states no term",

  "TermKind", "Dates",
  "What that term is: Anniversary, UnitPeriod, PeriodOf.",
  "the contract states no term",

  "DateEndNaive", "Dates",
  "The farthest date after the filing, whatever the contract mentioned it for. The naive end.",
  "the contract names no future date",

  "NaiveYears", "Dates",
  "DateEndNaive less DateStart in years, UNCAPPED. Measured from the same DateStart as
   DurationYears, so the two differ in one decision rather than two. Missing far more often, which
   is the coverage argument for the cascade rather than a defect.",
  "the contract names no future date",

  # -- Money ----------------------------------------------------------------------------------------
  "nUniAmountNaive", "Money",
  "Distinct parsed, non-withheld amounts in any currency, before the zero and par-value filter.
   Zero where every figure in the contract was withheld, which is a measurement this project exists
   for.",
  "structural",

  "nUniAmountUSD", "Money",
  "Distinct USD amounts after the filter drops zeros and par-value figures.",
  "structural",

  "nUniAmountOther", "Money",
  "Distinct non-USD amounts after the same filter. Not converted -- there is no FX anywhere in this
   pipeline -- so this pools currencies and only the COUNT is meaningful across them.",
  "structural",

  "MoneyMaxUSD", "Money",
  "Largest USD amount. CARRIES A KNOWN DEFECT: HTML flattening removed table cell boundaries and the
   money regex read the run as one number, so roughly 1,500 contracts carry a figure that is two
   glued cells. The visible tail is above 1e11; two glued four-digit cells give 20162017, twenty
   million dollars, which no magnitude test finds.",
  "the contract names no usable USD figure",

  "MoneyMedUSD", "Money",
  "Median USD amount. Far less affected by the defect above but not immune: 118 contracts carry a
   median over 1e11, because a contract whose figures are mostly glued runs has a glued median.",
  "the contract names no usable USD figure",

  "MoneyMaxOther", "Money",
  "Largest non-USD amount, mixing currencies. Same defect as MoneyMaxUSD at a higher rate.",
  "the contract names no usable non-USD figure",

  "MoneyMedOther", "Money",
  "Median non-USD amount, mixing currencies.",
  "the contract names no usable non-USD figure",

  # -- Redactions -----------------------------------------------------------------------------------
  "nRedactExplicit", "Redactions",
  "Bracketed markers naming confidential treatment. Bracketed, and withholds text.",
  "structural",

  "nRedactSymbol", "Redactions",
  "Bracketed markers holding a symbol, '[ ]' or '[*]'. Bracketed, and withholds text.",
  "structural",

  "nRedactBlank", "Redactions",
  "Empty brackets. Bracketed, and withholds text.",
  "structural",

  "nOmitExplicit", "Redactions",
  "'[Intentionally Omitted]' and its variants. Bracketed, and withholds NOTHING: a section left out
   on purpose is not text taken from a reader.",
  "structural",

  "nOmitSymbol", "Redactions",
  "The same role in symbol form, '[.]'. Bracketed, withholds nothing.",
  "structural",

  "nRedactBare", "Redactions",
  "Unbracketed runs of asterisks. Withholds text, and is the least reliable kind: a row of asterisks
   may be a horizontal rule.",
  "structural",

  "nRedactMoney", "Redactions",
  "Markers matched to a withheld price beside them. The only column here that is not a sum of the
   others, and the direct measure of how many of a contract's prices were withheld.",
  "structural"
)


#' The dictionary against the file it describes
#'
#' THE DECLARATION IS DIFFERENCED AGAINST THE DATA, both ways. A column in the file and not the
#' dictionary is a column nobody documented; a column in the dictionary and not the file is a
#' dictionary describing something that no longer exists. Both are drift and both abort.
#'
#' TYPE AND STATA NAME COME FROM THE FILE, not from the declaration, so the dictionary cannot claim a
#' type the data does not have. It also means adding a column upstream breaks this loudly rather than
#' producing a codebook that quietly omits it.
#'
#' @param .tab The joined table.
#' @param .dict Tibble. The declared dictionary.
#' @return Tibble: one row per column, in the file's own order.
exp_table_dictionary <- function(.tab, .dict = .exp_dictionary) {
  if (FALSE) {
    .tab  <- tab_Contracts
    .dict <- .exp_dictionary
  }

  undoc_ <- setdiff(names(.tab), .dict$Column)
  stale_ <- setdiff(.dict$Column, names(.tab))
  dup_   <- .dict$Column[duplicated(.dict$Column)]

  if (length(undoc_) > 0L || length(stale_) > 0L || length(dup_) > 0L) {
    say_ <- function(.x) if (length(.x) == 0L) "none" else paste(.x, collapse = ", ")
    cli::cli_abort(c(
      "The dictionary and the joined file disagree about which columns exist.",
      "x" = "In the file, documented nowhere: {say_(undoc_)}.",
      "x" = "In the dictionary, not in the file: {say_(stale_)}.",
      "x" = "Documented twice: {say_(unique(dup_))}.",
      "i" = "The dictionary is .exp_dictionary in this file's section 11."
    ))
  }

  sta_ <- exp_stata_ready(.tab = .tab)

  .dict |>
    dplyr::mutate(
      Block = factor(.data$Block, levels = .exp_blocks$Block),
      Type  = purrr::map_chr(.data$Column, \(.c) class(.tab[[.c]])[[1L]])
    ) |>
    dplyr::left_join(sta_$Names, by = dplyr::join_by(Column == Parquet)) |>
    dplyr::rename(StataName = "Stata") |>
    dplyr::arrange(match(.data$Column, names(.tab))) |>
    dplyr::select("Column", "StataName", "Block", "Type", "Meaning", "Missing")
}


#' What is true of a whole block rather than of one column, reported
#'
#' READ THIS BEFORE THE COLUMN TABLES. Grain says what a row is and therefore what an average over
#' this file means; Marker says how to tell a block that never reached a contract from a value that
#' does not exist within a block that did.
#'
#' @param .tab Tibble. The declared block table.
#' @return Invisibly .tab.
exp_report_grain <- function(.tab = .exp_blocks) {
  if (FALSE) .tab <- .exp_blocks

  tbl_head(.text = paste(nrow(.exp_blocks), "blocks: what a row is, and who is in it"))
  tbl_out(
    # Cache is machinery -- which file on disk the block arrives in -- and belongs to the coverage
    # table above rather than to a reader asking what a row means.
    .tab   = dplyr::select(.tab, -dplyr::any_of("Cache")),
    .title = NULL,
    .notes = c(
      Grain  = "An entity value is one attachment's, repeated on every registrant copy of it. A
                corpus mean over this file therefore double-counts and wants PrimaryFiler == 1.",
      Marker = "Missing here means the block never reached this contract. Any other missing value in
                the block is the second kind: the block reached it and that particular value does
                not exist."
    )
  )
  invisible(.tab)
}


#' One block of the dictionary, reported
#' @param .tab Tibble from exp_table_dictionary().
#' @param .block Character. Which block to print.
#' @return Invisibly the rows printed.
exp_report_dictionary <- function(.tab, .block) {
  if (FALSE) {
    .tab   <- tab_Dictionary
    .block <- "Parties"
  }

  out_ <- dplyr::filter(.tab, .data$Block == .block)
  if (nrow(out_) == 0L) cli::cli_abort("The dictionary holds no block called {(.block)}.")

  tbl_head(.text = paste0(.block, ": ", nrow(out_), " columns"))
  out_ |>
    dplyr::select("Column", "Type", "Meaning", "Missing") |>
    tbl_out(
      .title = NULL,
      .notes = c(
        Missing = "'structural' means the column is never missing where its block reached the row. A
                   count of zero is a measurement -- the extractor looked and found none -- and is
                   not the same as an absence."
      )
    )
  invisible(out_)
}
