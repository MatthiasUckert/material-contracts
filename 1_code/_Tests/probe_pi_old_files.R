# probe_pi_old_files.R -- what are the two old PerfectInformation files, and do they bridge to anything? ------------------
#
# 01C-International needs a bridge from a PerfectInfo document id to (Worldscope id, year, document type).
# The text corpus on the external drive carries numeric document ids only. Two files survive from the old
# pipeline -- PiDocumentFile.parquet and PiMatchingFile.parquet -- and nobody has written down what is in
# them. This probe assumes NO column name. Everything is discovered from the values:
#
#   A. What the files are: size, rows, schema.
#   B. Every column profiled: type, share missing, distinct values, unique or not, widest value, examples.
#   C. Every low-cardinality column tabulated in full (this is where document type, language, country and
#      year show up without anyone having to guess what they are called), and every date column by year.
#   D. Keys: which columns are unique within a file, and which column pairs link the two files.
#   E. The Worldscope bridge: which PI column carries values found in which 00C:FirmIdentifiers column.
#      Run twice, as stored and with leading zeros stripped, because item6035 is a padded string in some
#      sources and an integer in others.
#   F. The corpus bridge: which PI column carries the document ids found in the archives' .list sidecars,
#      overall and per subfolder. This view also re-counts the handoff's per-subfolder file table.
#
# Views E and F are skipped, and say so, when 00C's output or the external drive is not there.
#
# Reads the two parquet files, 00C-FirmIdentifiers/FirmIdentifiers.parquet and the .list sidecars.
# Nothing is written, nothing is extracted.

here::i_am("terminology-standardization.Rproj")

dir_pi   <- fs::path_expand("~/Dropbox/MyPapers/TerminologyPaper-R2B/Terminology-Data/PerfectInformation")
dir_arch <- "/Volumes/Ext20TB/PI-Transfer/archives"
path_fid <- here::here("2_output", "00C-FirmIdentifiers", "FirmIdentifiers.parquet")

files_pi <- c(
  PiDocumentFile = fs::path(dir_pi, "PiDocumentFile.parquet"),
  PiMatchingFile = fs::path(dir_pi, "PiMatchingFile.parquet")
)

# A column wider than this is text, not an identifier or a category: it is sized and otherwise left alone.
MaxKeyBytes <- 200L
# A column with at most this many distinct values is tabulated in full.
MaxTabulate <- 60L
# A column needs at least this many distinct values to be tried as an identifier.
MinIdValues <- 1000L

stopifnot(all(fs::file_exists(files_pi)))

# Helpers ----

hdr <- function(.txt) {
  if (FALSE) .txt <- "A. Files"

  # -- -- -- --

  cat("\n\n== ", .txt, " ", strrep("=", max(0L, 100L - nchar(.txt))), "\n\n", sep = "")

  return(invisible(NULL))
}

fmt_n <- function(.n) {
  if (FALSE) .n <- 1234567

  # -- -- -- --

  out_ <- format(.n, big.mark = ",", scientific = FALSE, trim = TRUE)

  return(out_)
}

col_read <- function(.path, .col) {
  if (FALSE) {
    .path <- files_pi[["PiDocumentFile"]]
    .col  <- "DocId"
  }

  # -- -- -- --

  out_ <- arrow::read_parquet(.path, col_select = dplyr::all_of(.col))[[1]]

  return(out_)
}

#' Any column as a comparable character key
#'
#' Whole doubles are printed without an exponent (as.character(100000) is "1e+05", which matches nothing),
#' integer64 is handled before the double branch because it IS a double underneath, everything is trimmed
#' and upper-cased, and the empty string is missing.
to_key <- function(.v) {
  if (FALSE) .v <- c(100000, NA, 12)

  # -- -- -- --

  if (inherits(.v, "integer64")) {
    out_ <- as.character(.v)
  } else if (is.double(.v) && !inherits(.v, c("Date", "POSIXt", "difftime"))) {
    whole_ <- all(.v == floor(.v), na.rm = TRUE)
    out_   <- if (whole_) sprintf("%.0f", .v) else as.character(.v)
    out_[is.na(.v)] <- NA_character_
  } else {
    out_ <- as.character(.v)
  }

  out_ <- stringi::stri_trans_toupper(stringi::stri_trim_both(out_))
  out_[!is.na(out_) & out_ == ""] <- NA_character_

  return(out_)
}

strip_zeros <- function(.v) {
  if (FALSE) .v <- c("000123", "0", "A01")

  # -- -- -- --

  out_ <- unique(stringi::stri_replace_first_regex(.v, "^0+(?=.)", ""))

  return(out_)
}

#' Profile one column
#'
#' Returns the profile row, the distinct key set (NULL for list columns and text), and a tabulation when the
#' column is low-cardinality or a date.
col_profile <- function(.v, .name) {
  if (FALSE) {
    .v    <- c("a", "b", NA, "a")
    .name <- "x"
  }

  # -- -- -- --

  type_ <- paste(class(.v), collapse = "/")
  n_    <- length(.v)

  if (is.list(.v)) {
    row_ <- data.frame(Column = .name, Type = type_, NaShare = NA_real_, nDistinct = NA_integer_,
                       Unique = NA, MaxBytes = NA_integer_, Examples = "(list or binary column, skipped)")
    return(list(Row = row_, Set = NULL, Tab = NULL))
  }

  # Width first, in bytes and on the untouched value, so a text column is never trimmed or upper-cased.
  bytes_     <- nchar(as.character(.v), type = "bytes", allowNA = TRUE, keepNA = TRUE)
  max_bytes_ <- if (all(is.na(bytes_))) NA_integer_ else max(bytes_, na.rm = TRUE)

  if (!is.na(max_bytes_) && max_bytes_ > MaxKeyBytes) {
    row_ <- data.frame(
      Column = .name, Type = type_, NaShare = round(mean(is.na(.v)), 4), nDistinct = NA_integer_,
      Unique = NA, MaxBytes = max_bytes_,
      Examples = sprintf("(text: median %s bytes, total %s MB)",
                         fmt_n(stats::median(bytes_, na.rm = TRUE)),
                         fmt_n(round(sum(bytes_, na.rm = TRUE) / 1e6)))
    )
    return(list(Row = row_, Set = NULL, Tab = NULL))
  }

  key_ <- to_key(.v)
  set_ <- unique(key_[!is.na(key_)])

  tab_ <- NULL
  if (inherits(.v, c("Date", "POSIXt"))) {
    tab_ <- table(format(.v, "%Y"), useNA = "ifany")
  } else if (length(set_) <= MaxTabulate) {
    tab_ <- sort(table(key_, useNA = "ifany"), decreasing = TRUE)
  }

  ex_ <- stringi::stri_sub(stringi::stri_enc_toutf8(utils::head(set_, 3L), validate = TRUE), 1L, 28L)

  row_ <- data.frame(
    Column    = .name,
    Type      = type_,
    NaShare   = round(mean(is.na(key_)), 4),
    nDistinct = length(set_),
    Unique    = length(set_) == n_,
    MaxBytes  = max_bytes_,
    Examples  = paste(ex_, collapse = " | ")
  )

  return(list(Row = row_, Set = set_, Tab = tab_))
}

#' Share of one side's distinct values found on the other, for every column pair
overlap_pairs <- function(.sets_a, .sets_b, .min_share = 0.5) {
  if (FALSE) {
    .sets_a    <- list(x = c("1", "2"))
    .sets_b    <- list(y = c("2", "3"))
    .min_share <- 0.5
  }

  # -- -- -- --

  rows_ <- list()
  for (a_ in names(.sets_a)) {
    for (b_ in names(.sets_b)) {
      both_ <- sum(.sets_a[[a_]] %in% .sets_b[[b_]])
      rows_[[length(rows_) + 1L]] <- data.frame(
        ColA = a_, ColB = b_,
        nA = length(.sets_a[[a_]]), nB = length(.sets_b[[b_]]), nBoth = both_,
        ShareOfA = round(both_ / length(.sets_a[[a_]]), 4),
        ShareOfB = round(both_ / length(.sets_b[[b_]]), 4)
      )
    }
  }

  if (length(rows_) == 0L) return(NULL)

  out_ <- do.call(rbind, rows_)
  out_ <- out_[out_$ShareOfA >= .min_share | out_$ShareOfB >= .min_share, , drop = FALSE]
  out_ <- out_[order(-out_$ShareOfA, -out_$ShareOfB), , drop = FALSE]
  rownames(out_) <- NULL

  return(out_)
}

show_tab <- function(.tab, .empty = "  (nothing above the threshold)") {
  if (FALSE) .tab <- NULL

  # -- -- -- --

  if (is.null(.tab) || nrow(.tab) == 0L) cat(.empty, "\n") else print(.tab, right = FALSE, row.names = FALSE)

  return(invisible(NULL))
}

id_sets <- function(.sets) {
  if (FALSE) .sets <- list(x = as.character(1:2000), y = c("a", "b"))

  # -- -- -- --

  out_ <- .sets[vapply(.sets, \(.s) !is.null(.s) && length(.s) >= MinIdValues, logical(1L))]

  return(out_)
}

# A. Files ----

hdr("A. Files")

for (nm_ in names(files_pi)) {
  ds_ <- arrow::open_dataset(files_pi[[nm_]])
  cat(sprintf("  %-16s %10s rows | %3d columns | %8s MB on disk\n", nm_, fmt_n(nrow(ds_)), length(names(ds_)),
              fmt_n(round(as.numeric(fs::file_size(files_pi[[nm_]])) / 1e6, 1))))
  cat("\n", ds_$schema$ToString(), "\n\n", sep = "")
}

# B. Columns ----
#
# One column is read at a time, so a text column in either file costs its own size and no more.

hdr("B. Every column profiled")

profiles <- list()
sets     <- list()
tabs     <- list()

for (nm_ in names(files_pi)) {
  cols_ <- names(arrow::open_dataset(files_pi[[nm_]]))
  prof_ <- list()
  sets[[nm_]] <- list()
  tabs[[nm_]] <- list()

  for (col_ in cols_) {
    res_ <- col_profile(col_read(files_pi[[nm_]], col_), col_)
    prof_[[col_]]      <- res_$Row
    sets[[nm_]][[col_]] <- res_$Set
    tabs[[nm_]][[col_]] <- res_$Tab
    gc(verbose = FALSE)
  }

  profiles[[nm_]] <- do.call(rbind, prof_)
  cat("-- ", nm_, "\n\n", sep = "")
  show_tab(profiles[[nm_]])
  cat("\n")
}

# C. Categories and years ----

hdr(sprintf("C. Columns with at most %d distinct values, and dates by year", MaxTabulate))

for (nm_ in names(files_pi)) {
  for (col_ in names(tabs[[nm_]])) {
    tab_ <- tabs[[nm_]][[col_]]
    if (is.null(tab_)) next

    out_ <- data.frame(Value = names(tab_), n = as.integer(tab_))
    out_$Value[is.na(out_$Value)] <- "<NA>"
    out_$Share <- round(out_$n / sum(out_$n), 4)

    cat(sprintf("-- %s : %s\n\n", nm_, col_))
    show_tab(out_)
    cat("\n")
  }
}

# D. Keys ----

hdr("D. Keys within each file, links between the two")

for (nm_ in names(files_pi)) {
  uniq_ <- profiles[[nm_]]$Column[which(profiles[[nm_]]$Unique & profiles[[nm_]]$NaShare == 0)]
  cat(sprintf("  %-16s unique, never missing: %s\n", nm_,
              if (length(uniq_) == 0L) "(no single column)" else paste(uniq_, collapse = ", ")))
}

cat("\n  Column names in both files: ",
    paste(intersect(profiles[[1]]$Column, profiles[[2]]$Column), collapse = ", "), "\n\n", sep = "")

cat(sprintf("  Column pairs sharing values (ColA from %s, ColB from %s; columns with %s+ distinct values):\n\n",
            names(files_pi)[1], names(files_pi)[2], fmt_n(MinIdValues)))
show_tab(overlap_pairs(id_sets(sets[[1]]), id_sets(sets[[2]])))

# E. Worldscope bridge ----

hdr("E. Which PI column carries a Worldscope identifier (00C:FirmIdentifiers)")

if (!fs::file_exists(path_fid)) {
  cat("  SKIPPED --", path_fid, "is not on disk.\n")
} else {
  fid_      <- arrow::read_parquet(path_fid)
  sets_fid_ <- id_sets(lapply(fid_, \(.v) if (is.list(.v)) NULL else { k_ <- to_key(.v); unique(k_[!is.na(k_)]) }))
  cat("  FirmIdentifiers:", fmt_n(nrow(fid_)), "rows; identifier columns tried:",
      paste(names(sets_fid_), collapse = ", "), "\n")

  for (nm_ in names(files_pi)) {
    pi_ <- id_sets(sets[[nm_]])

    cat(sprintf("\n-- %s, values as stored (ColA = PI column, ColB = FirmIdentifiers column)\n\n", nm_))
    asis_ <- overlap_pairs(pi_, sets_fid_, .min_share = 0.2)
    show_tab(asis_)

    cat(sprintf("\n-- %s, leading zeros stripped on both sides\n\n", nm_))
    show_tab(overlap_pairs(lapply(pi_, strip_zeros), lapply(sets_fid_, strip_zeros), .min_share = 0.2))

    # Distinct values say whether the column IS the identifier; rows say how much of the file it reaches.
    if (!is.null(asis_) && nrow(asis_) > 0L) {
      cat(sprintf("\n-- %s, the same in ROWS for the three strongest pairs (values as stored)\n\n", nm_))
      for (i_ in seq_len(min(3L, nrow(asis_)))) {
        key_ <- to_key(col_read(files_pi[[nm_]], asis_$ColA[i_]))
        cat(sprintf("  %-24s -> %-16s rows with a value %10s | of those matched %6.1f%%\n",
                    asis_$ColA[i_], asis_$ColB[i_], fmt_n(sum(!is.na(key_))),
                    100 * mean(key_[!is.na(key_)] %in% sets_fid_[[asis_$ColB[i_]]])))
      }
    }
  }
}

# F. Corpus bridge ----

hdr("F. Which PI column carries the document ids of the text archives")

if (!fs::dir_exists(dir_arch)) {
  cat("  SKIPPED --", dir_arch, "is not mounted.\n")
} else {
  lists_ <- fs::dir_ls(dir_arch, glob = "*__documents_text__*.list")
  cat("  .list sidecars read:", length(lists_), "\n\n")

  lines_ <- unlist(lapply(lists_, readLines, warn = FALSE), use.names = FALSE)
  m_     <- stringi::stri_match_first_regex(lines_, "(?i)([^/\\\\]+)[/\\\\]([^/\\\\]+)\\.txt$")
  arch_  <- data.frame(Sub = m_[, 2], DocId = m_[, 3])[!is.na(m_[, 1]), ]
  arch_$DocId <- stringi::stri_replace_first_regex(arch_$DocId, "^0+(?=.)", "")

  cat("  Files per subfolder, from the sidecars (compare with the handoff's table):\n\n")
  by_sub_ <- as.data.frame(table(Sub = arch_$Sub), responseName = "nFiles")
  show_tab(by_sub_[order(-by_sub_$nFiles), ])
  cat(sprintf("\n  %s .txt entries | %s distinct document ids\n", fmt_n(nrow(arch_)),
              fmt_n(length(unique(arch_$DocId)))))

  set_arch_ <- unique(arch_$DocId)
  subs_     <- toupper(unique(arch_$Sub))

  for (nm_ in names(files_pi)) {
    # A column naming the subfolder would make (Sub, DocId) joinable directly.
    sub_cols_ <- names(sets[[nm_]])[vapply(sets[[nm_]], \(.s) !is.null(.s) && any(.s %in% subs_), logical(1L))]
    cat(sprintf("\n-- %s\n\n  Columns holding subfolder names: %s\n", nm_,
                if (length(sub_cols_) == 0L) "(none)" else paste(sub_cols_, collapse = ", ")))

    digit_ <- id_sets(sets[[nm_]])
    digit_ <- digit_[vapply(digit_, \(.s) isTRUE(mean(stringi::stri_detect_regex(.s, "^[0-9]+$")) >= 0.99), logical(1L))]
    digit_ <- lapply(digit_, strip_zeros)

    cat("\n  All-digit PI columns against the archive ids (ColA = PI column):\n\n")
    hit_ <- overlap_pairs(digit_, list(ArchiveDocId = set_arch_), .min_share = 0.05)
    show_tab(hit_)

    if (is.null(hit_) || nrow(hit_) == 0L) next

    best_ <- hit_$ColA[1]
    cat(sprintf("\n  Per subfolder, for the strongest column (%s):\n\n", best_))
    per_ <- lapply(split(arch_$DocId, arch_$Sub), \(.ids) {
      ids_ <- unique(.ids)
      data.frame(nArchiveIds = length(ids_), nInPi = sum(ids_ %in% digit_[[best_]]))
    })
    per_ <- cbind(Sub = names(per_), do.call(rbind, per_))
    per_$Share <- round(per_$nInPi / per_$nArchiveIds, 4)
    show_tab(per_[order(-per_$nArchiveIds), ])
  }
}

cat("\n\nDone. Nothing was written.\n")
