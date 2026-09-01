# Gate for 10: the two new caches and the joins they feed, on fixtures.
.p <- if (requireNamespace("here", quietly = TRUE)) here::here("1_code", "10-ExportData.R") else "10-ExportData.R"
if (!file.exists(.p)) .p <- "10-ExportData.R"
suppressWarnings(source(.p, encoding = "UTF-8"))
cat("library:", .p, "\n\n")

ok <- function(.n, .p, .d = "") { cat(if (isTRUE(.p)) "PASS " else "FAIL ", .n,
  if (any(nzchar(.d))) paste0(" -- ", paste(.d, collapse = " ")) else "", "\n", sep = ""); isTRUE(.p) }
res <- logical(0)

# -- The vocabulary changed; assert what it now says --------------------------------------------
res <- c(res, ok("the four referee columns are in the sample list",
                 all(c("DocName", "DocSeq", "UrlIndexPage", "DocDesc") %in% .exp_sample_cols), ""))
res <- c(res, ok("no CTO column is taken from the register any more",
                 !any(grepl("^(Has|n)?Cto", .exp_sample_cols)),
                 paste(grep("Cto", .exp_sample_cols, value = TRUE), collapse = " ")))
res <- c(res, ok("Items and Orders are registered blocks",
                 all(c("Items", "Orders") %in% .exp_blocks$Block), ""))
res <- c(res, ok("every block marker is a documented column",
                 all(.exp_blocks$Marker %in% .exp_dictionary$Column),
                 paste(setdiff(.exp_blocks$Marker, .exp_dictionary$Column), collapse = " ")))
res <- c(res, ok("the item columns are all documented",
                 all(c("nItems", "nItemsVoluntary", "ItemEra", "ItemCodes", "ReportsItem101",
                       "ReportsItem901") %in% .exp_dictionary$Column), ""))
res <- c(res, ok("the CTO columns moved block from Sample to Orders",
                 all(.exp_dictionary$Block[.exp_dictionary$Column %in%
                       c("HasCto", "nCtoOrders", "CtoReleaseFirst", "CtoReleaseLast",
                         "CtoIsExtension")] == "Orders"), ""))
res <- c(res, ok("no column is documented twice",
                 !anyDuplicated(.exp_dictionary$Column),
                 paste(.exp_dictionary$Column[duplicated(.exp_dictionary$Column)], collapse = " ")))

# -- export_final's argument list ---------------------------------------------------------------
fml <- names(formals(export_final))
res <- c(res, ok("export_final takes the two new paths",
                 all(c(".path_items", ".path_cto") %in% fml), paste(fml, collapse = " ")))

# -- The CTO collapse, on a fixture -------------------------------------------------------------
if (requireNamespace("arrow", quietly = TRUE)) {
  pth <- tempfile(fileext = ".parquet")
  arrow::write_parquet(tibble::tibble(
    DocIDContract = c("a", "a", "b", "c", "c", NA, "d"),
    ReleaseDate   = as.Date(c("2010-01-01", "2012-06-01", NA, "2015-03-03", NA, "2011-01-01", NA)),
    IsExtension   = c(0L, 1L, 0L, 0L, 0L, 1L, 0L)
  ), pth)
  out <- export_cto(.path_in = pth, .path_out = tempfile(fileext = ".parquet"), .rerun = TRUE)

  res <- c(res, ok("the unlinked reference is dropped", nrow(out) == 4L, nrow(out)))
  res <- c(res, ok("the key is renamed to DocID", "DocID" %in% names(out), paste(names(out), collapse = " ")))
  res <- c(res, ok("two orders collapse to one row with an extension flag",
                   out$nCtoOrders[out$DocID == "a"] == 2L && out$CtoIsExtension[out$DocID == "a"] == 1L, ""))
  res <- c(res, ok("first and last bracket the orders",
                   out$CtoReleaseFirst[out$DocID == "a"] == as.Date("2010-01-01") &&
                     out$CtoReleaseLast[out$DocID == "a"] == as.Date("2012-06-01"), ""))
  res <- c(res, ok("all-missing dates give NA, not an infinity",
                   is.na(out$CtoReleaseFirst[out$DocID == "b"]) &&
                     !any(is.infinite(as.numeric(out$CtoReleaseFirst))), ""))

  # -- export_items, on a fixture ---------------------------------------------------------------
  p_it <- tempfile(fileext = ".parquet"); p_sm <- tempfile(fileext = ".parquet")
  arrow::write_parquet(tibble::tibble(
    HashIndex = c("i1", "i2", "i9"), FilingDate = as.Date("2015-01-01"),
    nItems = c(2L, 3L, 1L), nItemsVoluntary = c(1L, 0L, 0L), ItemEra = "Post",
    ItemCodes = c("1.01|9.01", "1.01|8.01|9.01", "5"), ReportsItem101 = c(1L, 1L, 0L)
  ), p_it)
  arrow::write_parquet(tibble::tibble(
    DocID = c("d1", "d2", "d3"), HashIndex = c("i1", "i1", "i2")
  ), p_sm)
  its <- export_items(.path_in = p_it, .path_sample = p_sm,
                      .path_out = tempfile(fileext = ".parquet"), .rerun = TRUE)

  res <- c(res, ok("filings the sample never reaches are dropped",
                   nrow(its) == 2L && !"i9" %in% its$HashIndex, nrow(its)))
  res <- c(res, ok("FilingDate is not carried", !"FilingDate" %in% names(its),
                   paste(names(its), collapse = " ")))
  res <- c(res, ok("one row per filing survives", !anyDuplicated(its$HashIndex), ""))
} else {
  cat("SKIP arrow unavailable\n")
}

# -- The marker rule must agree between the two places that state it ----------------------------
res <- c(res, ok("HashIndex counts as a key in the coverage table",
                 grepl('keys_ <- c\\("DocID", "HashDocument", "HashIndex"\\)',
                       paste(readLines(.p, warn = FALSE), collapse = "\n")), ""))

if (requireNamespace("arrow", quietly = TRUE)) {
  # exp_table_files derives the marker as the first non-key column; .exp_blocks declares it. They
  # have to agree, or the coverage table reports a block as complete that is not.
  keys <- c("DocID", "HashDocument", "HashIndex")
  res <- c(res, ok("the orders cache leads with its declared marker",
                   setdiff(names(out), keys)[[1L]] ==
                     .exp_blocks$Marker[.exp_blocks$Block == "Orders"],
                   paste(setdiff(names(out), keys)[[1L]],
                         .exp_blocks$Marker[.exp_blocks$Block == "Orders"])))
  res <- c(res, ok("the items cache leads with its declared marker",
                   setdiff(names(its), keys)[[1L]] ==
                     .exp_blocks$Marker[.exp_blocks$Block == "Items"],
                   paste(setdiff(names(its), keys)[[1L]],
                         .exp_blocks$Marker[.exp_blocks$Block == "Items"])))
}

# -- Every documented block must actually be rendered -------------------------------------------
# THE CHECK THE FILE DID NOT HAVE. exp_table_dictionary() differences the declaration against the
# data both ways and aborts on either, so a column missing from .exp_dictionary stops the render.
# Nothing checked that a documented column is ever SHOWN: the sections are one hand-written chunk
# per block, and two new blocks arrived with their dictionary rows and no chunks. Seventeen columns
# were documented and invisible, and every existing check passed.
.p_qmd <- sub("\\.R$", ".qmd", .p)
if (file.exists(.p_qmd)) {
  qmd_  <- paste(readLines(.p_qmd, warn = FALSE), collapse = "\n")
  shown <- unique(regmatches(qmd_, gregexpr('(?<=\\.block = ")[^"]+', qmd_, perl = TRUE))[[1L]])
  doc   <- unique(.exp_dictionary$Block)

  res <- c(res, ok("every documented block has a dictionary section",
                   length(setdiff(doc, shown)) == 0L,
                   paste(setdiff(doc, shown), collapse = ", ")))
  res <- c(res, ok("no section renders a block that does not exist",
                   length(setdiff(shown, doc)) == 0L,
                   paste(setdiff(shown, doc), collapse = ", ")))
  res <- c(res, ok("the sections account for every column of the file",
                   sum(.exp_dictionary$Block %in% shown) == nrow(.exp_dictionary),
                   sprintf("%d shown of %d documented",
                           sum(.exp_dictionary$Block %in% shown), nrow(.exp_dictionary))))
}

res <- c(res, ok("the declared blocks and the documented blocks are the same set",
                 setequal(.exp_blocks$Block, unique(.exp_dictionary$Block)),
                 paste(c(setdiff(.exp_blocks$Block, .exp_dictionary$Block),
                         setdiff(.exp_dictionary$Block, .exp_blocks$Block)), collapse = ", ")))

# -- The declared marker must be the one the coverage table uses --------------------------------
res <- c(res, ok("every block names the cache it arrives in",
                 "Cache" %in% names(.exp_blocks) && !anyNA(.exp_blocks$Cache), ""))
res <- c(res, ok("no two blocks claim the same cache",
                 !anyDuplicated(.exp_blocks$Cache),
                 paste(.exp_blocks$Cache[duplicated(.exp_blocks$Cache)], collapse = ", ")))
res <- c(res, ok("every declared marker is a documented column",
                 all(.exp_blocks$Marker %in% .exp_dictionary$Column),
                 paste(setdiff(.exp_blocks$Marker, .exp_dictionary$Column), collapse = ", ")))

# The coverage table used to guess the marker as the first non-key column. Where that guess and the
# declaration disagree the two tables in one render say different things, which happened for three
# blocks at once: Sample, Orders and Law.
if (exists("its") && exists("out")) {
  keys <- c("DocID", "HashDocument", "HashIndex")
  res <- c(res, ok("the declared marker is used, not the first non-key column",
                   identical(
                     exp_table_files(.tabs = list(Items = its, Orders = out),
                                     .final = dplyr::bind_cols(its[1, ], out[1, ]))$Marker,
                     c("nItems", "CtoReleaseFirst")
                   ), ""))
}

cat("\n", sum(res), "/", length(res), " passed\n", sep = "")
if (!all(res)) quit(status = 1L)
